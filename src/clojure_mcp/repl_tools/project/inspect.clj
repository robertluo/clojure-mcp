(ns clojure-mcp.repl-tools.project.inspect
  "Tools for project inspection and analysis.
   These tools help understand project structure, dependencies, and source files."
  (:require
   [clojure.string :as str]
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure-mcp.nrepl :as mcp-nrepl]))

(defn inspect-project-code
  "REPL expression to gather project information including paths, dependencies, and source files.
   Returns a map with project details that can be evaluated in the project's context."
  []
  '(let [deps (when (.exists (clojure.java.io/file "deps.edn"))
                (-> "deps.edn" slurp clojure.edn/read-string))
         project-clj (when (.exists (clojure.java.io/file "project.clj"))
                      (-> "project.clj" slurp read-string))
         lein-project? (boolean project-clj)
         deps-project? (boolean deps)
         source-files (fn [dir]
                        (when (.exists (clojure.java.io/file dir))
                          (->> (clojure.java.io/file dir)
                               file-seq
                               (filter #(.isFile %))
                               (filter #(.endsWith (.getName %) ".clj"))
                               (map #(.getPath %))
                               sort)))
         ;; Extract paths from deps.edn or project.clj
         source-paths (cond
                        deps (or (:paths deps) ["src"])
                        lein-project? (->> project-clj
                                          (drop-while #(not= :source-paths %))
                                          second)
                        :else ["src"])
         test-paths (cond
                      deps (get-in deps [:aliases :test :extra-paths] ["test"])
                      lein-project? (->> project-clj
                                        (drop-while #(not= :test-paths %))
                                        second)
                      :else ["test"])
         all-paths (concat source-paths test-paths)
         sources (mapcat source-files all-paths)
         namespaces (mapv #(-> % 
                              (.replace "/" ".")
                              (.replace "_" "-")
                              (.replace ".clj" "")) 
                         (filter #(.endsWith % ".clj") sources))
         java-version (System/getProperty "java.version")
         clojure-version-info (clojure-version)]
     {:working-dir (System/getProperty "user.dir")
      :project-type (cond 
                      (and deps-project? lein-project?) "deps.edn + Leiningen"
                      deps-project? "deps.edn"
                      lein-project? "Leiningen"
                      :else "Unknown")
      :java-version java-version
      :clj-version clojure-version-info
      :deps deps
      :project-clj (when lein-project? 
                     (let [version (->> project-clj (drop-while #(not= :version %)) second)
                           name (->> project-clj (drop-while #(not= :name %)) second)]
                       {:name name
                        :version version}))
      :source-paths source-paths
      :test-paths test-paths
      :namespaces namespaces
      :sources sources}))

(defn format-project-info
  "Formats the project information into a readable string.
   
   Arguments:
   - insp-data: The project inspection data as an EDN string
   
   Returns a formatted string with project details"
  [insp-data]
  (when insp-data
    (when-let [{:keys [working-dir
                       project-type
                       clj-version
                       java-version
                       deps
                       project-clj
                       source-paths
                       test-paths
                       namespaces
                       sources]}
               (try
                 (edn/read-string insp-data)
                 (catch Throwable e 
                   (println "Error parsing data:" (ex-message e))
                   nil))]
      (with-out-str
        (println "\n📦 Clojure Project Information:")
        (println "==============================")
        
        (println "\n🔧 Environment:")
        (println "• Working Directory:" working-dir)
        (println "• Project Type:" project-type)
        (println "• Clojure Version:" clj-version)
        (println "• Java Version:" java-version)
        
        (println "\n📂 Source Paths:")
        (doseq [path source-paths]
          (println "•" path))
        
        (println "\n🧪 Test Paths:")
        (doseq [path test-paths]
          (println "•" path))
        
        (when deps
          (println "\n🔗 Dependencies:")
          (doseq [[dep coord] (sort-by key (:deps deps))]
            (println "•" dep "=>" coord)))
        
        (when-let [aliases (:aliases deps)]
          (println "\n🏷️ Aliases:")
          (doseq [[alias config] (sort-by key aliases)]
            (println "•" alias ":" (pr-str config))))
        
        (when project-clj
          (println "\n📋 Leiningen Project:")
          (println "• Name:" (:name project-clj))
          (println "• Version:" (:version project-clj)))
        
        (println "\n🧩 Namespaces (" (count namespaces) "):")
        (doseq [ns-name (take 15 namespaces)]
          (println "•" ns-name))
        (when (> (count namespaces) 15)
          (println "• ... and" (- (count namespaces) 15) "more"))
        
        (println "\n📄 Project Structure (" (count sources) " files):")
        (doseq [source-file (take 15 sources)]
          (println "•" source-file))
        (when (> (count sources) 15)
          (println "• ... and" (- (count sources) 15) "more"))))))

(defn inspect-project-tool
  "Creates an MCP tool for inspecting Clojure project structure and dependencies.
   
   Arguments:
   - service-atom: Atom containing the nREPL service connection
   
   Returns a tool map with name, description, schema and implementation"
  [service-atom]
  {:name "clojure_inspect_project"
   :description 
   (str "Analyzes and provides detailed information about a Clojure project's structure, "
        "including dependencies, source files, namespaces, and environment details.\n\n"
        "This tool helps you understand project organization without having to manually "
        "explore multiple configuration files. It works with both deps.edn and Leiningen projects.\n\n"
        "The tool provides information about:\n"
        "- Project environment (working directory, Clojure version, Java version)\n"
        "- Source and test paths\n"
        "- Dependencies and their versions\n"
        "- Aliases and their configurations\n"
        "- Available namespaces\n"
        "- Source file structure\n\n"
        "Use this tool to quickly get oriented in an unfamiliar Clojure codebase or to "
        "get a high-level overview of your current project.\n\n"
        "# Example:\n"
        "# clojure_inspect_project()\n")
   :schema (json/write-str {:type :object
                            :properties {}
                            :required []})
   :tool-fn (fn [_ _ clj-result-k]
              (let [nrepl-client @service-atom
                    insp-code (str (inspect-project-code))
                    eval-fn (fn [result]
                              (if (:error result)
                                (clj-result-k [(or (:message result)
                                                   (str "Error during project inspection: " (:error result)))]
                                              true)
                                (let [formatted-info (format-project-info (:value result))]
                                  (clj-result-k [formatted-info] false))))]
                (mcp-nrepl/eval-code nrepl-client insp-code eval-fn)))})
