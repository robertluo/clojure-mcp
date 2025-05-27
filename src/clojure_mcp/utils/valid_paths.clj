(ns clojure-mcp.utils.valid-paths
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure-mcp.config :as config]))

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

(defn clojure-file?
  "Checks if a file path has a Clojure-related extension.
   
   Supported extensions:
   - .clj (Clojure)
   - .cljs (ClojureScript)
   - .cljc (Clojure/ClojureScript shared)
   - .bb (Babashka)
   - .edn (Extensible Data Notation)"
  [file-path]
  (when file-path
    (let [lower-path (str/lower-case file-path)]
      (or (str/ends-with? lower-path ".clj")
          (str/ends-with? lower-path ".cljs")
          (str/ends-with? lower-path ".cljc")
          (str/ends-with? lower-path ".bb")
          (str/ends-with? lower-path ".edn")))))
