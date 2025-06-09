(ns clojure-mcp.config
  (:require
   [clojure.java.io :as io]
   [clojure-mcp.nrepl :as nrepl]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]))

(defn- relative-to [dir path]
  (try
    (let [f (io/file path)]
      (if (.isAbsolute f)
        (.getCanonicalPath f)
        (.getCanonicalPath (io/file dir path))))
    (catch Exception e
      (log/warn "Bad file paths " (pr-str [dir path]))
      nil)))

(defn process-remote-config [{:keys [allowed-directories emacs-notify write-file-guard] :as config} user-dir]
  (let [ud (io/file user-dir)]
    (assert (and (.isAbsolute ud)
                 (.isDirectory ud)))
    (cond-> config
      user-dir (assoc :nrepl-user-dir (.getCanonicalPath ud))
      true
      (assoc :allowed-directories
             (->> (cons user-dir allowed-directories)
                  (keep #(relative-to user-dir %))
                  distinct
                  vec))
      (some? (:emacs-notify config))
      (assoc :emacs-notify (boolean (:emacs-notify config)))
      ;; Validate write-file-guard if present
      (some? write-file-guard)
      (do (when-not (contains? #{:full-read :partial-read false} write-file-guard)
            (log/warn "Invalid write-file-guard value:" write-file-guard
                      "- using default :full-read")
            (throw (ex-info (str "Invalid Config: write-file-guard value:  " write-file-guard
                                 "- must be one of (:full-read, :partial-read, false)")
                            {:write-file-guard write-file-guard})))
          config))))

(defn load-remote-config [nrepl-client user-dir]
  (let [remote-cfg-str
        (nrepl/tool-eval-code
         nrepl-client
         (pr-str
          '(do
             (require '[clojure.java.io :as io])
             (if-let [f (clojure.java.io/file "." ".clojure-mcp" "config.edn")]
               (when (.exists f) (clojure.edn/read-string (slurp f)))))))
        remote-config (try (edn/read-string remote-cfg-str)
                           (catch Exception _ {}))
        processed-config (process-remote-config remote-config user-dir)]
    (log/info "Loaded remote-config:" remote-config)
    (log/info "Processed config:" processed-config)
    processed-config))

(defn get-config [nrepl-client-map k]
  (get-in nrepl-client-map [::config k]))

(defn get-allowed-directories [nrepl-client-map]
  (get-config nrepl-client-map :allowed-directories))

(defn get-emacs-notify [nrepl-client-map]
  (get-config nrepl-client-map :emacs-notify))

(defn get-nrepl-user-dir [nrepl-client-map]
  (get-config nrepl-client-map :nrepl-user-dir))

(defn get-write-file-guard [nrepl-client-map]
  (or (get-config nrepl-client-map :write-file-guard) :full-read))

(defn set-config! [nrepl-client-atom k v]
  (swap! nrepl-client-atom assoc-in [::config k] v))

