(ns clojure-mcp.tools.bash.core
  "Core bash command execution functionality"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [clojure-mcp.tools.eval.core :as eval-core])
  (:import
   (java.util.concurrent TimeUnit TimeoutException)
   (java.io InputStreamReader BufferedReader)))

(def ^:private default-timeout-ms 30000)

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

#_(defn execute-bash-command-old
  [{:keys [command working-directory timeout-ms] :as args}]
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
             (let [pb (ProcessBuilder. (into-array [\"bash\" \"-c\" %s]))
                   _ (when %s
                       (.directory pb (java.io.File. %s)))
                   process (.start pb)
                   timeout-ms %s
                   completed (.waitFor process timeout-ms java.util.concurrent.TimeUnit/MILLISECONDS)]
               (if completed
                 (let [exit-code (.exitValue process)
                       stdout (slurp (.getInputStream process))
                       stderr (slurp (.getErrorStream process))]
                   {:exit-code exit-code
                    :stdout stdout
                    :stderr stderr
                    :timed-out false})
                 (do
                   (.destroyForcibly process)
                   (let [stdout (try (slurp (.getInputStream process)) (catch Exception _ \"\"))
                         stderr (try (slurp (.getErrorStream process)) (catch Exception _ \"\"))]
                     {:exit-code -1
                      :stdout stdout
                      :stderr stderr
                      :timed-out true}))))
             (catch Exception e
               {:exit-code -1
                :stdout \"\"
                :stderr (.getMessage e)
                :timed-out false
                :error (.getMessage e)}))"
          (pr-str command)
          (if working-directory (pr-str working-directory) (pr-str nil))
          (if working-directory (pr-str working-directory) (pr-str ""))
          (or timeout-ms default-timeout-ms)))

(defn execute-bash-command
  [nrepl-client-atom {:keys [command working-directory timeout-ms] :as args}]
  (let [timeout-ms (or timeout-ms default-timeout-ms)]
    (when-not (command-allowed? command)
      (throw (ex-info "Command not allowed due to security restrictions"
                      {:command command
                       :error-details "The command contains restricted operations"})))
    (log/debug "Executing bash command" command args)
    (let [clj-shell-code (str (edn/read-string
                               (generate-shell-eval-code command
                                                         working-directory
                                                         timeout-ms)))
          result (eval-core/evaluate-code
                  @nrepl-client-atom
                  {:code clj-shell-code
                   ;; make this timeout a little longer than the inner timeout
                   :timeout-ms (+ 200 timeout-ms)})
          inner-value (:value (into {} (:outputs result)))]
      (if (not (:error result))
        (edn/read-string inner-value)
        (if-let [val (try (edn/read-string inner-value) (catch Exception e nil))]
          val
          {:stdout "ERROR: reading results of Bash call."
           :stderr ""
           :timed-out false
           :exit-code -1})))))

(comment
  (require '[clojure-mcp.config :as config])
  
  (def client-atom (atom (clojure-mcp.nrepl/create {:port 7888})))
  #_(config/set-config! client-atom :nrepl-user-dir (System/getProperty "user.dir"))
  #_(config/set-config! client-atom :allowed-directories [(System/getProperty "user.dir")])
  (clojure-mcp.nrepl/start-polling @client-atom)

  (execute-bash-command client-atom {:command "ls -al"
                                     :working-directory (System/getProperty "user.dir")})
  
  (eval-core/evaluate-code @client-atom {:code (generate-shell-eval-code "ls -al" (System/getProperty "user.dir")
                                                                         5000)
                                         } )


  
  (clojure-mcp.nrepl/stop-polling @client-atom)
  )


#_(execute-bash-command
   {:command "ls -al"
    :working-directory (System/getProperty "user.dir") :timeout-ms nil})
