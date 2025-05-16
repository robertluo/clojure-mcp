(ns clojure-mcp.tools.bash.tool
  "Implementation of the bash tool using the tool-system multimethod approach."
  (:require
   [clojure-mcp.tool-system :as tool-system]
   [clojure-mcp.config :as config]
   [clojure-mcp.repl-tools.utils :as utils]
   [clojure-mcp.tools.bash.core :as core]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; Factory function to create the tool configuration
(defn create-bash-tool
  "Creates the bash tool configuration.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  {:tool-type :bash
   :nrepl-client-atom nrepl-client-atom
   :working-dir (config/get-nrepl-user-dir @nrepl-client-atom)})

;; Implement the required multimethods for the bash tool
(defmethod tool-system/tool-name :bash [_]
  "bash")

(defmethod tool-system/tool-description :bash [_]
  "Execute bash shell commands on the host system.

This tool allows running shell commands and capturing their output.
Commands are executed in a bash shell with the -c flag.

Parameters:
- command: The shell command to execute (required)
- working_directory: Directory to run the command in (optional)
- timeout_seconds: Maximum execution time in seconds (optional, default: 30)

Returns:
- stdout: Standard output from the command
- stderr: Standard error output
- exit_code: The command's exit status (0 typically means success)
- timed_out: Whether the command exceeded the timeout limit

Security considerations:
- Certain dangerous commands are blocked for safety
- Commands with system-modifying potential may be restricted
- All commands execute with the permissions of the MCP server process

Examples:
1. List files: bash(command: \"ls -la\")
2. Find text in files: bash(command: \"grep -r 'pattern' /path/to/search\")
3. With working directory: bash(command: \"ls -la\", working_directory: \"/tmp\")
4. With timeout: bash(command: \"sleep 10\", timeout_seconds: 5)

Note: Non-zero exit codes are NOT treated as tool errors - check exit_code
in the response to determine command success.")

(defmethod tool-system/tool-schema :bash [_]
  {:type :object
   :properties {:command {:type :string
                          :description "The shell command to execute"}
                :working_directory {:type :string
                                    :description "Directory to run the command in (optional)"}
                :timeout_seconds {:type :integer
                                  :description "Maximum execution time in seconds (optional, default: 30)"}}
   :required [:command]})

(defmethod tool-system/validate-inputs :bash [{:keys [nrepl-client-atom working-dir]} inputs]
  (let [{:keys [command working_directory timeout_seconds]} inputs
        nrepl-client @nrepl-client-atom
        working_directory (or working_directory working-dir)]
    ;; Validate required parameters
    (when-not command
      (throw (ex-info "Missing required parameter: command"
                      {:inputs inputs})))
    (assert working_directory)
    (let [validated-dir (utils/validate-path-with-client working_directory nrepl-client)
          dir (io/file validated-dir)]
      (when-not (.exists dir)
        (throw (ex-info (str "Working directory does not exist: " validated-dir)
                        {:inputs inputs
                         :error-details (str "Directory not found: " validated-dir)})))
      (when-not (.isDirectory dir)
        (throw (ex-info (str "Path is not a directory: " validated-dir)
                        {:inputs inputs
                         :error-details (str "Not a directory: " validated-dir)})))


      ;; Validate timeout if provided
      (when (and timeout_seconds (or (not (number? timeout_seconds))
                                     (< timeout_seconds 1)))
        (throw (ex-info "Invalid timeout value"
                        {:inputs inputs
                         :error-details "Timeout must be a positive number"})))

      ;; Return validated and normalized inputs
      (cond-> {:command command}
        working_directory (assoc :working-directory validated-dir)
        timeout_seconds (assoc :timeout-seconds timeout_seconds)))))

(defmethod tool-system/execute-tool :bash [_ inputs]
  (let [{:keys [command working-directory timeout-seconds]} inputs
        result (core/execute-bash-command
                command
                {:working-directory working-directory
                 :timeout-seconds timeout-seconds})]
    result))

(defmethod tool-system/format-results :bash [_ result]
  (let [{:keys [stdout stderr exit-code timed-out error]} result
        formatted-output (cond-> []
                           ;; If there's an error message from the tool itself (not command exit code)
                           error (conj (str "Error: " error))

                           ;; Always include command output details
                           :always (conj (str "Exit code: " exit-code
                                              (when timed-out " (timed out)")))

                           ;; Include stdout if present
                           (not (str/blank? stdout)) (conj (str "Standard output:\n" stdout))

                           ;; Include stderr if present
                           (not (str/blank? stderr)) (conj (str "Standard error:\n" stderr)))]

    ;; We treat command execution as successful even if the command itself returns non-zero
    ;; Only report error for problems with the tool itself
    {:result formatted-output
     :error (boolean error)}))

;; Backward compatibility function that returns the registration map
(defn bash-tool
  "Returns the registration map for the bash tool.
   
   Parameters:
   - nrepl-client-atom: Atom containing the nREPL client"
  [nrepl-client-atom]
  (tool-system/registration-map (create-bash-tool nrepl-client-atom)))
