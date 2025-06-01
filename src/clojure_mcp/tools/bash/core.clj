(ns clojure-mcp.tools.bash.core
  "Core bash command execution functionality"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [clojure-mcp.nrepl :as nrepl]
   [clojure-mcp.tools.eval.core :as eval-core])
  (:import
   (java.util.concurrent TimeUnit TimeoutException)
   (java.io InputStreamReader BufferedReader)))

;; 3 minutes? some test suites take much longer.
;; TODO this should go into the config.
(def ^:private default-timeout-ms 180000)

;; this is pretty useless
(def ^:private disallowed-commands
  #{;; System modification commands
    "rm -rf" "rm -r" "rmdir" "dd" "mkfs" "format"
    ;; Network attack tools
    "nmap" "netcat" "nc"
    ;; User/permission commands
    "sudo" "su" "passwd" "chmod" "chown"
    ;; Dangerous system commands
    "shutdown" "reboot" "halt" "init"})

(defn- command-allowed?
  [command]
  (let [trimmed-command (str/trim command)]
    (not-any? #(str/starts-with? trimmed-command %) disallowed-commands)))

(defn- read-stream
  [stream]
  (with-open [reader (BufferedReader. (InputStreamReader. stream))]
    (str/join "\n" (line-seq reader))))

(defn execute-bash-command
  [_ {:keys [command working-directory timeout-ms] :as args}]
  (let [timeout-ms (or timeout-ms default-timeout-ms)]
    (when-not (command-allowed? command)
      (throw (ex-info "Command not allowed due to security restrictions"
                      {:command command
                       :error-details "The command contains restricted operations"})))
    (log/debug "Executing bash command" command args)
    ;; Set up the ProcessBuilder
    (let [pb (ProcessBuilder. (into-array ["bash" "-c" command]))
          _ (when working-directory
              (.directory pb (io/file working-directory)))
          process (.start pb)]
      (if (.waitFor process timeout-ms TimeUnit/MILLISECONDS)
        {:exit-code (.exitValue process)
         :stdout (read-stream (.getInputStream process))
         :stderr (read-stream (.getErrorStream process))
         :timed-out false}
        (do
          (.destroyForcibly process)
          {:stdout (try (read-stream (.getInputStream process))
                        (catch Exception _ ""))
           :stderr (try (read-stream (.getErrorStream process))
                        (catch Exception _ ""))
           :exit-code -1
           :timed-out true})))))

(defn generate-shell-eval-code
  "Generate Clojure code to execute shell command with explicit timeout handling"
  [command working-directory timeout-ms]
  (format "(try
             (require '[clojure.java.io :as io])
             (require '[clojure.string :as str])
             (let [pb (ProcessBuilder. (into-array [\"bash\" \"-c\" %s]))
                   _ %s
                   process (.start pb)
                   completed (.waitFor process %s java.util.concurrent.TimeUnit/MILLISECONDS)
                   nrepl-limit %s
                   max-stderr-length (quot nrepl-limit 2)
                   truncate-with-limit
                   (fn [s limit]
                     (if (> (count s) limit)
                       (str (subs s 0 (- limit 16))
                            \"\\n... (truncated)\")
                       s))
                   read-it (fn [limit input-stream]
                              (-> (try
                                    (with-open [reader (io/reader input-stream)]
                                      (str/join \"\\n\" (line-seq reader)))
                                    (catch Exception _ \"\"))
                                  (truncate-with-limit limit)))
                   stderr (read-it max-stderr-length (.getErrorStream process))
                   remaining-space (max 500 (- nrepl-limit 500 (count stderr)))
                   stdout (read-it remaining-space (.getInputStream process))]
               (if completed
                 {:exit-code (.exitValue process)
                  :stdout stdout
                  :stderr stderr
                  :timed-out false}
                 (do
                   (.destroyForcibly process)
                   {:exit-code -1
                    :stdout stdout
                    :stderr stderr
                    :timed-out true})))
             (catch Exception e
               {:exit-code -1
                :stdout \"\"
                :stderr (.getMessage e)
                :timed-out false
                :error (.getMessage e)}))"
          (pr-str command)
          ;; Fix working directory handling - only set if not nil
          (if working-directory
            (format "(when %s (.directory pb (java.io.File. %s)))"
                    (pr-str working-directory) (pr-str working-directory))
            "nil  ;; no working directory specified")
          (or timeout-ms default-timeout-ms)
          ;; should be less than overall nrepl limit
          (int (* nrepl/truncation-length 0.85))))

(defn execute-bash-command-nrepl
  [nrepl-client-atom {:keys [command working-directory timeout-ms] :as args}]
  (let [timeout-ms (or timeout-ms default-timeout-ms)]

    (when-not (command-allowed? command)
      (throw (ex-info "Command not allowed due to security restrictions"
                      {:command command
                       :error-details "The command contains restricted operations"})))
    (let [clj-shell-code (str (edn/read-string
                               (generate-shell-eval-code command
                                                         working-directory
                                                         timeout-ms)))
          ;; Increase timeout buffer from 200ms to 5000ms for more reliability
          eval-timeout-ms (+ 5000 timeout-ms)
          result (eval-core/evaluate-code
                  @nrepl-client-atom
                  {:code clj-shell-code
                   :timeout-ms eval-timeout-ms})
          inner-value (:value (into {} (:outputs result)))]

      ;; Add logging for debugging intermittent failures
      (when (:error result)
        (log/warn "REPL evaluation failed for bash command"
                  {:command command
                   :working-directory working-directory
                   :timeout-ms timeout-ms
                   :eval-timeout-ms eval-timeout-ms
                   :error result}))

      (if (not (:error result))
        (try
          (edn/read-string inner-value)
          (catch Exception e
            (log/error e "Failed to parse bash command result as EDN"
                       {:command command
                        :inner-value inner-value
                        :result result})
            ;; Return more informative error response
            {:stdout ""
             :stderr (format "EDN parsing failed: %s\nRaw result: %s"
                             (.getMessage e) inner-value)
             :timed-out false
             :exit-code -1
             :error "edn-parse-failure"}))
        (if-let [val (try (edn/read-string inner-value)
                          (catch Exception e
                            (log/warn e "Secondary EDN parse attempt failed"
                                      {:command command
                                       :inner-value inner-value})
                            nil))]
          val
          (do
            (log/error "Bash command evaluation failed completely"
                       {:command command
                        :result result})
            {:stdout ""
             :stderr (format "Command evaluation failed: %s" (pr-str result))
             :timed-out false
             :exit-code -1
             :error "eval-failure"}))))))

(comment
  (require '[clojure-mcp.config :as config])

  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  #_(config/set-config! client-atom :nrepl-user-dir (System/getProperty "user.dir"))
  #_(config/set-config! client-atom :allowed-directories [(System/getProperty "user.dir")])
  (clojure-mcp.nrepl/start-polling @client-atom)

  (execute-bash-command-nrepl client-atom {:command "curl -s https://wttr.in/Phoenix?format=j1"
                                           :working-directory (System/getProperty "user.dir")})

  (->> (eval-core/evaluate-code
        @client-atom
        {:code (generate-shell-eval-code
                "clojure -X:test"
                (System/getProperty "user.dir")
                100000)
         :timeout-ms 100000})
       :outputs
       (into {})
       :value
       edn/read-string)

  (clojure-mcp.nrepl/stop-polling @client-atom))

#_(execute-bash-command
   {:command "ls -al"
    :working-directory (System/getProperty "user.dir") :timeout-ms nil})

