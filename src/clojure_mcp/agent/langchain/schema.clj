(ns clojure-mcp.agent.langchain.schema
  (:require
   [clojure.string :as string])
  (:import
   [dev.langchain4j.model.chat.request.json
    JsonArraySchema
    JsonBooleanSchema
    JsonEnumSchema
    JsonIntegerSchema
    JsonNumberSchema
    JsonObjectSchema
    JsonStringSchema
    JsonSchemaElement]))

(defmulti edn->sch
  (fn [{:keys [type enum] :as json-edn}]
    (or
     (and type (keyword type))
     (and enum :enum)
     (throw (ex-info "By JSON data" {:json-edn json-edn})))))

(defmethod edn->sch :string [{:keys [description]}]
  (cond-> (JsonStringSchema/builder)
      description (.description description)
      :always (.build)))

(defmethod edn->sch :number [{:keys [description]}]
  (cond-> (JsonNumberSchema/builder)
      description (.description description)
      :always (.build)))

(defmethod edn->sch :integer [{:keys [description]}]
  (cond-> (JsonIntegerSchema/builder)
    description (.description description)
    :always (.build)))

(defmethod edn->sch :boolean [{:keys [description]}]
  (cond-> (JsonBooleanSchema/builder)
    description (.description description)
    :always (.build)))

(defmethod edn->sch :enum [{:keys [enum]}]
  (assert (every? string? enum))
  (assert (not-empty enum))
  (-> (JsonEnumSchema/builder)
      (.enumValues (map name enum))
      (.build)))

(defmethod edn->sch :array [{:keys [items]}]
  (assert items)
  (-> (JsonArraySchema/builder)
      (.items (edn->sch items))
      (.build)))

(defmethod edn->sch :object [{:keys [properties description required]}]
  (assert properties)
  (let [obj-build
        (cond-> (JsonObjectSchema/builder)
          (not (string/blank? description)) (.description description)
          (not-empty required) (.required (map name required)))]
    (doseq [[nm edn-schema] properties]
      (.addProperty obj-build (name nm) (edn->sch edn-schema)))
    (.build obj-build)))


(comment
  (edn->sch {:type :string
             :description "Hello"})
  (edn->sch {:type :integer
             :description "Hello"})
  (edn->sch {:type :number
             :description "Hello"})
  (edn->sch {:type :boolean
             :description "Hello"})

  (edn->sch {:enum ["a"]})

  (edn->sch {:type :array
             :items {:type :integer
                     :description "Hello"}})

  (edn->sch {:type :object
             :description "HOWDy"
             :properties {:name {:type :string
                                 :description "The name"}
                          :edits {:type :array
                                  :items {:type :object
                                          :properties {:old {:type :string
                                                             :description "The name"}
                                                       :new {:type :string
                                                             :description "The name"}
                                                       }
                                          :required [:old :new]}}}
             })
  )
