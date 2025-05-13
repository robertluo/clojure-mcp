(ns clojure-mcp.config
  (:require
   [clojure.java.io :as io]
   [clojure-mcp.nrepl :as nrepl]
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]))

(defn process-remote-config [{:keys [allowed-dirs emacs-notify] :as config} user-dir]
  (cond-> config
    (seq allowed-dirs)
    (assoc :clojure-mcp.core/allowed-directories
           (vec (keep #(try (.getCanonicalPath (io/file user-dir %))
                            (catch Exception e nil))
                      allowed-dirs)))
    (some? (:emacs-notify config))
    (assoc :clojure-mcp.core/emacs-notify (boolean (:emacs-notify config)))))

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
                           (catch Exception _ {}))]
    (log/info "Loaded remote-config:" remote-config)
    (process-remote-config remote-config user-dir)))

