# Creating Your Own Custom MCP Server

**🎉 Welcome to the fun part!** Creating your own custom MCP server is not just easy—it's empowering and delightful. You get to craft a personalized AI assistant that understands YOUR workflow, YOUR project structure, and YOUR development style. During the alpha phase of clojure-mcp, creating your own main entry point is the primary way to configure the server, giving you complete control over your development experience.

Think of it as building your own personalized AI development companion. Want only read-only tools for safer exploration? Done. Need specialized ClojureScript tools? Add them in. Have custom prompts for your team's coding standards? Perfect. This is YOUR server, tailored to YOUR needs.

> **💡 Pro Tip**: Always refer to `src/clojure_mcp/main.clj` to see the current optimized set of tools, resources, and prompts. This file represents the carefully curated default configuration and serves as the best reference for understanding which tools work well together and how they're organized.

## Why Create a Custom Server?

- **Personalization**: Include only the tools you actually use
- **Custom Workflows**: Add prompts specific to your project or team
- **Specialized Resources**: Expose your project's unique documentation
- **Tool Integration**: Add tools for your specific tech stack (Shadow-cljs, Figwheel, etc.)
- **Safety Controls**: Choose between read-only exploration or full editing capabilities
- **Performance**: Smaller tool sets mean faster startup and less cognitive overhead

## The Basic Pattern

The beauty of clojure-mcp is that you can leverage all the existing infrastructure. Here's the pattern:

1. **Require** `clojure-mcp.main` to access pre-built functions
2. **Customize** by adding, removing, or modifying components
3. **Start** your server with your custom configuration

## Minimal Custom Server Example

Let's start with the simplest possible custom server that reuses everything from `main`:

```clojure
(ns my-company.mcp-server
  (:require [clojure-mcp.core :as core]
            [clojure-mcp.config :as config]
            [clojure-mcp.main :as main]))

(defn start-mcp-server [nrepl-args]
  (let [nrepl-client-map (core/create-and-start-nrepl-connection nrepl-args)
        working-dir (config/get-nrepl-user-dir nrepl-client-map)
        resources (main/my-resources nrepl-client-map working-dir)
        tools (main/my-tools (atom nrepl-client-map))
        prompts (main/my-prompts working-dir)
        mcp (core/mcp-server)]
    
    ;; Register everything with the MCP server
    (doseq [resource resources] (core/add-resource mcp resource))
    (doseq [tool tools] (core/add-tool mcp tool))
    (doseq [prompt prompts] (core/add-prompt mcp prompt))
    
    nil))
```

## Customizing Resources

Want to add your own documentation? Override the `my-resources` function:

```clojure
(ns my-company.mcp-server
  (:require [clojure-mcp.core :as core]
            [clojure-mcp.config :as config]
            [clojure-mcp.main :as main]
            [clojure-mcp.resources :as resources]
            [clojure.java.io :as io]))

(defn my-resources [nrepl-client-map working-dir]
  ;; Start with the default resources
  (concat
   (main/my-resources nrepl-client-map working-dir)
   ;; Add your custom resources
   [(resources/create-file-resource
     "custom://architecture"
     "ARCHITECTURE.md"
     "Our system architecture documentation"
     "text/markdown"
     (.getCanonicalPath (io/file working-dir "docs/ARCHITECTURE.md")))
    
    (resources/create-string-resource
     "custom://team-standards"
     "Team Coding Standards"
     "Our team's Clojure coding standards"
     "text/markdown"
     "# Team Coding Standards\n\n- Always use kebab-case\n- Prefer threading macros\n- Write tests first")]))
```

## Customizing Tools

### Selective Tool Loading

Maybe you want a read-only server for safer exploration:

```clojure
(defn my-read-only-tools [nrepl-client-atom]
  ;; Only include read-only and evaluation tools
  [(directory-tree-tool/directory-tree-tool nrepl-client-atom)
   (unified-read-file-tool/unified-read-file-tool nrepl-client-atom)
   (new-grep-tool/grep-tool nrepl-client-atom)
   (glob-files-tool/glob-files-tool nrepl-client-atom)
   (think-tool/think-tool nrepl-client-atom)
   (eval-tool/eval-code nrepl-client-atom)
   (project-tool/inspect-project-tool nrepl-client-atom)])
```

### Adding Custom Tools

Have a custom tool? Add it to the mix:

```clojure
(ns my-company.mcp-server
  (:require ;; ... other requires ...
            [clojure-mcp.main :as main]
            [my-company.database-tool :as db-tool]))

(defn my-tools [nrepl-client-atom]
  ;; Start with main tools and add your own
  (conj (main/my-tools nrepl-client-atom)
        (db-tool/database-query-tool nrepl-client-atom)))
```

### Complete Tool Customization

Or build your tool list from scratch:

```clojure
(ns my-company.mcp-server
  (:require ;; Core tools
            [clojure-mcp.tools.directory-tree.tool :as directory-tree-tool]
            [clojure-mcp.tools.eval.tool :as eval-tool]
            [clojure-mcp.tools.unified-read-file.tool :as unified-read-file-tool]
            [clojure-mcp.tools.grep.tool :as new-grep-tool]
            [clojure-mcp.tools.glob-files.tool :as glob-files-tool]
            [clojure-mcp.tools.think.tool :as think-tool]
            [clojure-mcp.tools.bash.tool :as bash-tool]
            ;; Editing tools
            [clojure-mcp.tools.form-edit.combined-edit-tool :as combined-edit-tool]
            [clojure-mcp.tools.file-write.tool :as file-write-tool]
            ;; Project tools
            [clojure-mcp.tools.project.tool :as project-tool]))

(defn my-tools [nrepl-client-atom]
  [;; Exploration
   (directory-tree-tool/directory-tree-tool nrepl-client-atom)
   (unified-read-file-tool/unified-read-file-tool nrepl-client-atom)
   (new-grep-tool/grep-tool nrepl-client-atom)
   (glob-files-tool/glob-files-tool nrepl-client-atom)
   
   ;; Thinking and evaluation
   (think-tool/think-tool nrepl-client-atom)
   (eval-tool/eval-code nrepl-client-atom)
   (bash-tool/bash-tool nrepl-client-atom)
   
   ;; Editing (only the safe ones!)
   (combined-edit-tool/unified-form-edit-tool nrepl-client-atom)
   (file-write-tool/file-write-tool nrepl-client-atom)
   
   ;; Project understanding
   (project-tool/inspect-project-tool nrepl-client-atom)])
```

## Custom Prompts

Add project-specific prompts to guide your AI assistant:

```clojure
(defn my-prompts [working-dir]
  ;; Start with default prompts
  (concat
   (main/my-prompts working-dir)
   ;; Add custom prompts
   [{:name "database-migration"
     :description "Generate database migration code"
     :arguments [{:name "table-name"
                  :description "Name of the table to migrate"
                  :required? true}
                 {:name "operation"
                  :description "add-column, remove-column, create-table, etc."
                  :required? true}]
     :prompt-fn (fn [_ args callback]
                  (callback
                   {:description "Database migration assistant"
                    :messages [{:role :user
                                :content (str "Generate a database migration for: "
                                            (get args "table-name")
                                            " operation: " (get args "operation")
                                            "\nUse our standard migration format.")}]}))}
    
    {:name "test-generator"
     :description "Generate test cases for a namespace"
     :arguments [{:name "namespace"
                  :description "The namespace to test"
                  :required? true}]
     :prompt-fn (fn [_ args callback]
                  (let [ns-name (get args "namespace")]
                    (callback
                     {:description "Test generation"
                      :messages [{:role :user
                                  :content (str "Please generate comprehensive tests for: " ns-name
                                              "\n\nInclude:"
                                              "\n- Unit tests for each public function"
                                              "\n- Property-based tests where appropriate"
                                              "\n- Edge cases and error conditions"
                                              "\n- Use our team's test naming conventions")}]})))}]))
```

## Real-World Example: Shadow-cljs Server

Here's how the Shadow-cljs example extends the main server:

```clojure
(ns my-company.shadow-mcp
  (:require [clojure-mcp.core :as core]
            [clojure-mcp.config :as config]
            [clojure-mcp.main :as main]
            [clojure-mcp.tools.eval.tool :as eval-tool]))

(defn shadow-eval-tool [nrepl-client-atom config]
  ;; Create a customized eval tool for ClojureScript
  (-> (eval-tool/eval-code nrepl-client-atom)
      (assoc :name "clojurescript_eval")
      (assoc :description "Evaluates ClojureScript code in Shadow-cljs REPL")))

(defn my-tools [nrepl-client-atom config]
  ;; Add the shadow tool to the standard tools
  (conj (main/my-tools nrepl-client-atom)
        (shadow-eval-tool nrepl-client-atom config)))

(defn start-mcp-server [nrepl-args]
  (let [nrepl-client-map (core/create-and-start-nrepl-connection nrepl-args)
        working-dir (config/get-nrepl-user-dir nrepl-client-map)
        atom-client (atom nrepl-client-map)
        mcp (core/mcp-server)]
    
    ;; Use customized tools, but standard resources and prompts
    (doseq [resource (main/my-resources nrepl-client-map working-dir)]
      (core/add-resource mcp resource))
    (doseq [tool (my-tools atom-client nrepl-args)]
      (core/add-tool mcp tool))
    (doseq [prompt (main/my-prompts working-dir)]
      (core/add-prompt mcp prompt))
    
    nil))
```

## Complete Custom Server Template

Here's a full template you can use as a starting point:

```clojure
(ns my-company.custom-mcp-server
  "Custom MCP server tailored for our team's Clojure development"
  (:require [clojure-mcp.core :as core]
            [clojure-mcp.config :as config]
            [clojure-mcp.main :as main]
            [clojure-mcp.resources :as resources]
            [clojure-mcp.prompts :as prompts]
            ;; Add specific tool requires as needed
            [clojure-mcp.tools.eval.tool :as eval-tool]
            [clojure-mcp.tools.unified-read-file.tool :as read-tool]
            [clojure.java.io :as io]))

(defn my-resources
  "Custom resources including our team documentation"
  [nrepl-client-map working-dir]
  (concat
   ;; Include some defaults
   [(first (main/my-resources nrepl-client-map working-dir))] ; PROJECT_SUMMARY
   ;; Add our custom resources
   [(resources/create-file-resource
     "custom://style-guide"
     "STYLE_GUIDE.md"
     "Our comprehensive Clojure style guide"
     "text/markdown"
     (.getCanonicalPath (io/file working-dir "docs/STYLE_GUIDE.md")))]))

(defn my-prompts
  "Custom prompts for our workflow"
  [working-dir]
  [{:name "pr-review"
    :description "Review code changes for a pull request"
    :arguments []
    :prompt-fn (prompts/simple-content-prompt-fn
                "PR Review Guide"
                "Please review the recent changes focusing on:
                 1. Our team's style guide compliance
                 2. Test coverage
                 3. Performance implications
                 4. API compatibility")}])

(defn my-tools
  "Curated tool selection for our team"
  [nrepl-client-atom]
  ;; Mix and match from main tools or add your own
  [(eval-tool/eval-code nrepl-client-atom)
   (read-tool/unified-read-file-tool nrepl-client-atom)
   ;; ... add more tools as needed
   ])

(def nrepl-client-atom (atom nil))

(defn start-mcp-server
  "Start our custom MCP server"
  [nrepl-args]
  (let [nrepl-client-map (core/create-and-start-nrepl-connection nrepl-args)
        working-dir (config/get-nrepl-user-dir nrepl-client-map)
        _ (reset! nrepl-client-atom nrepl-client-map)
        mcp (core/mcp-server)]
    
    ;; Register our custom components
    (doseq [resource (my-resources nrepl-client-map working-dir)]
      (core/add-resource mcp resource))
    (doseq [tool (my-tools nrepl-client-atom)]
      (core/add-tool mcp tool))
    (doseq [prompt (my-prompts working-dir)]
      (core/add-prompt mcp prompt))
    
    ;; Store server reference
    (swap! nrepl-client-atom assoc :mcp-server mcp)
    nil))
```

## Configuring deps.edn

Point your deps.edn to your custom server:

```clojure
{:aliases 
  {:my-mcp 
    {:deps {org.slf4j/slf4j-nop {:mvn/version "2.0.16"}
            com.bhauman/clojure-mcp {:local/root "~/workspace/clojure-mcp"}}
     :extra-paths ["src"] ; Where your custom server lives
     :exec-fn my-company.custom-mcp-server/start-mcp-server
     :exec-args {:port 7888}}}}
```

## Tips for Success

1. **Start Simple**: Begin by reusing main's functions, then gradually customize
2. **Test Incrementally**: Add one customization at a time and test
3. **Document Your Choices**: Comment why you included/excluded specific tools
4. **Version Control**: Keep your custom server in version control
5. **Team Sharing**: Share your server configuration with your team

## Common Patterns

### Development vs Production Servers

```clojure
(defn dev-tools [nrepl-client-atom]
  ;; All tools including editing
  (main/my-tools nrepl-client-atom))

(defn prod-tools [nrepl-client-atom]
  ;; Read-only tools for production debugging
  [(read-tool/unified-read-file-tool nrepl-client-atom)
   (eval-tool/eval-code nrepl-client-atom)])

(defn start-mcp-server [{:keys [env] :as nrepl-args}]
  (let [tools-fn (if (= env "production") prod-tools dev-tools)
        ;; ... rest of setup
        ])
  ;; ...)
```

### Project-Type Specific Servers

```clojure
(defn web-app-tools [nrepl-client-atom]
  ;; Tools for web development
  ;; ... include HTTP testing tools, etc.
  )

(defn library-tools [nrepl-client-atom]
  ;; Tools for library development
  ;; ... focus on documentation, API design tools
  )
```

## Conclusion

Creating your own custom MCP server is where the real magic happens. It's not just configuration—it's crafting your perfect AI-powered development environment. Whether you need a minimal read-only explorer, a full-featured development powerhouse, or something specialized for your unique workflow, the power is in your hands.

Remember: during the alpha phase, this IS the way to configure clojure-mcp. Embrace it, experiment with it, and make it yours. Your custom server is your AI assistant's personality—make it reflect how YOU want to work!

Happy customizing! 🚀
