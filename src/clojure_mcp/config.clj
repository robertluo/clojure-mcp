(ns clojure-mcp.config
  (:require
   [clojure.java.io :as io]
   [clojure-mcp.nrepl :as nrepl]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]))

(defn process-remote-config [{:keys [allowed-directories emacs-notify] :as config} user-dir]
  (cond-> config
    (seq allowed-directories)
    (assoc :allowed-directories
           (->> allowed-directories
                (keep #(try (.getCanonicalPath (io/file user-dir %))
                            (catch Exception e nil)))
                distinct
                vec))
    (some? (:emacs-notify config))
    (assoc :emacs-notify (boolean (:emacs-notify config)))))

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

(defn set-config! [nrepl-client-atom k v]
  (swap! nrepl-client-atom assoc-in [::config k] v))

