(ns clojure-mcp.tools.bash.core
  "Core bash command execution functionality"
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   (java.util.concurrent TimeUnit TimeoutException)
   (java.io InputStreamReader BufferedReader)))

(def ^:private default-timeout-seconds 30)

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
  (not-any? #(str/includes? command %) disallowed-commands))

(defn- read-stream
  [stream]
  (with-open [reader (BufferedReader. (InputStreamReader. stream))]
    (str/join "\n" (line-seq reader))))

(defn execute-bash-command
  [command {:keys [working-directory timeout-seconds]
            :or {timeout-seconds default-timeout-seconds}}]
  (when-not (command-allowed? command)
    (throw (ex-info "Command not allowed due to security restrictions"
                    {:command command
                     :error-details "The command contains restricted operations"})))

  ;; Set up the ProcessBuilder
  (let [pb (ProcessBuilder. (into-array ["bash" "-c" command]))
        _ (when working-directory
            (.directory pb (io/file working-directory)))
        process (.start pb)]
    (if (.waitFor process timeout-seconds TimeUnit/SECONDS)
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
         :timed-out true}))))
