(ns clojure-mcp.tools.bash.core
  "Core bash command execution functionality"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.logging :as log])
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

(defn execute-bash-command
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

#_(execute-bash-command
   {:command "ls -al"
    :working-directory (System/getProperty "user.dir") :timeout-ms nil})
