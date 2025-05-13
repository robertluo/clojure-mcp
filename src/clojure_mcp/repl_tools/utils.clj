(ns clojure-mcp.repl-tools.utils
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure-mcp.config :as config])) ; Added :refer [nrepl-client-atom]

(defn validate-path
  "Validates that a path is within allowed directories.
   
   Parameters:
   - path: The path to validate (can be relative or absolute)
   - current-working-directory: The absolute path of the current working directory
   - allowed-directories: A sequence of absolute paths representing allowed directories
   
   Returns:
   - The normalized absolute path if valid
   - Throws an exception if the path is not within any allowed directory"
  [path current-working-directory allowed-directories]
  (let [cwd-file (io/file current-working-directory)
        path-file (io/file path)]

    ;; Validate current working directory is absolute
    (when-not (.isAbsolute cwd-file)
      (throw (ex-info (str "Current working directory must be absolute \nCWD:" current-working-directory)
                      {:cwd current-working-directory})))

    ;; Validate all allowed directories are absolute
    (let [non-absolute (filter #(not (.isAbsolute (io/file %))) allowed-directories)]
      (when (seq non-absolute)
        (throw (ex-info "All allowed directories must be absolute paths"
                        {:non-absolute-dirs non-absolute}))))

    ;; Normalize the allowed directories upfront
    (let [canonical-allowed-dirs (map #(.getCanonicalPath (io/file %))
                                      (filter identity allowed-directories))

          ;; Normalize the path (resolve relative paths against CWD)
          abs-path-file (if (.isAbsolute path-file)
                          path-file
                          (io/file cwd-file path))
          normalized-path (.getCanonicalPath abs-path-file)]

      ;; Check if path is within any allowed directory
      (if (or (empty? canonical-allowed-dirs) ;; If no allowed directories, accept all paths
              (some (fn [allowed-canonical]
                      ;; Check if normalized path starts with allowed dir plus separator
                      ;; or if it equals the allowed dir exactly
                      (or (= normalized-path allowed-canonical)
                          (str/starts-with? normalized-path
                                            (str allowed-canonical
                                                 (System/getProperty "file.separator")))))
                    canonical-allowed-dirs))
        normalized-path
        (throw (ex-info (str "Your path:\n" normalized-path
                             "is outside the allowed directories:\n" (str/join "\n" allowed-directories))
                        {:path normalized-path
                         :allowed-dirs allowed-directories}))))))

(defn validate-path-with-client
  "Validates a path using settings from the nrepl-client.
   
   Parameters:
   - path: The path to validate (can be relative or absolute)
   - nrepl-client-map: The nREPL client map (dereferenced atom)
   
   Returns:
   - The normalized absolute path if valid
   - Throws an exception if the path is invalid or if required settings are missing"
  [path nrepl-client]
  (let [current-dir (config/get-nrepl-user-dir nrepl-client)
        allowed-dirs (config/get-allowed-directories nrepl-client)]

    (when-not current-dir
      (throw (ex-info "Missing nrepl-user-dir in config"
                      {:client-keys (keys nrepl-client)})))

    (when-not allowed-dirs
      (throw (ex-info "Missing allowed-directories in config"
                      {:client-keys (keys nrepl-client)})))

    (validate-path path current-dir allowed-dirs)))

(defn generate-diff-via-shell
  "Generates a unified diff between two strings using the external diff command.
   Requires two strings (file1-content, file2-content) and context-lines count."
  [file1-content file2-content context-lines]
  (let [temp-prefix "clj-diff-"
        temp-suffix ".tmp"
        file1 (java.io.File/createTempFile temp-prefix temp-suffix)
        file2 (java.io.File/createTempFile temp-prefix temp-suffix)]
    (try
      (spit file1 file1-content)
      (spit file2 file2-content)

      (let [file1-path (.getAbsolutePath file1)
            file2-path (.getAbsolutePath file2)
            ;; Use -U for unified diff format with specified context
            diff-result (shell/sh "diff" "-U" (str context-lines) file1-path file2-path)]
        (if (= 0 (:exit diff-result))
          ;; Files are identical or diff succeeded
          (:out diff-result)
          ;; Diff command indicated differences (exit code 1) or an error (other non-zero)
          ;; Exit code 1 is normal for differences, so we still return the output.
          ;; Handle other errors specifically if needed.
          (if (= 1 (:exit diff-result))
            (:out diff-result)
            (throw (ex-info "Diff command failed" {:result diff-result})))))
      (finally
        ;; Ensure temporary files are deleted
        (.delete file1)
        (.delete file2)))))
