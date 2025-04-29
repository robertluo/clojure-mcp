(ns clojure-mcp.agent.langchain.schema-test
  (:require [clojure.test :refer [deftest testing is are]]
            [clojure-mcp.agent.langchain.schema :as schema]
            [clojure.data.json :as json])
  (:import [dev.langchain4j.model.chat.request.json
            JsonArraySchema
            JsonBooleanSchema
            JsonEnumSchema
            JsonIntegerSchema
            JsonNumberSchema
            JsonObjectSchema
            JsonStringSchema]))

(deftest test-simple-schema-types
  (testing "Converting primitive type schemas"
    (let [string-schema {:type :string, :description "A simple string"}
          number-schema {:type :number, :description "A simple number"}
          integer-schema {:type :integer, :description "A simple integer"}
          boolean-schema {:type :boolean, :description "A simple boolean"}

          converted-string (schema/edn->sch string-schema)
          converted-number (schema/edn->sch number-schema)
          converted-integer (schema/edn->sch integer-schema)
          converted-boolean (schema/edn->sch boolean-schema)]

      (is (instance? JsonStringSchema converted-string))
      (is (= "A simple string" (.description converted-string)))

      (is (instance? JsonNumberSchema converted-number))
      (is (= "A simple number" (.description converted-number)))

      (is (instance? JsonIntegerSchema converted-integer))
      (is (= "A simple integer" (.description converted-integer)))

      (is (instance? JsonBooleanSchema converted-boolean))
      (is (= "A simple boolean" (.description converted-boolean))))))

(deftest test-enum-schemas
  (testing "Converting enum schema"
    (let [enum-schema {:enum ["read" "write" "append"]}
          converted-enum (schema/edn->sch enum-schema)]

      (is (instance? JsonEnumSchema converted-enum))
      (is (= ["read" "write" "append"] (.enumValues converted-enum))))))

(deftest test-array-schemas
  (testing "Converting array schemas with different item types"
    (let [string-array-schema {:type :array, :items {:type :string}}
          number-array-schema {:type :array, :items {:type :number}}
          integer-array-schema {:type :array, :items {:type :integer}}
          boolean-array-schema {:type :array, :items {:type :boolean}}

          converted-string-array (schema/edn->sch string-array-schema)
          converted-number-array (schema/edn->sch number-array-schema)
          converted-integer-array (schema/edn->sch integer-array-schema)
          converted-boolean-array (schema/edn->sch boolean-array-schema)]

      (is (instance? JsonArraySchema converted-string-array))
      (is (instance? JsonStringSchema (.items converted-string-array)))

      (is (instance? JsonArraySchema converted-number-array))
      (is (instance? JsonNumberSchema (.items converted-number-array)))

      (is (instance? JsonArraySchema converted-integer-array))
      (is (instance? JsonIntegerSchema (.items converted-integer-array)))

      (is (instance? JsonArraySchema converted-boolean-array))
      (is (instance? JsonBooleanSchema (.items converted-boolean-array))))))

(deftest test-object-schemas
  (testing "Converting object schemas with required fields"
    (let [object-schema {:type :object
                         :description "A test object"
                         :properties {:name {:type :string, :description "The name"}
                                      :age {:type :integer, :description "The age"}}
                         :required [:name]}
          converted-object (schema/edn->sch object-schema)
          properties (.properties converted-object)]

      (is (instance? JsonObjectSchema converted-object))
      (is (= "A test object" (.description converted-object)))
      (is (= ["name"] (seq (.required converted-object))))
      (is (instance? JsonStringSchema (get properties "name")))
      (is (instance? JsonIntegerSchema (get properties "age")))
      (is (= "The name" (.description (get properties "name"))))
      (is (= "The age" (.description (get properties "age")))))))

(deftest test-nested-object-schemas
  (testing "Converting nested object schemas"
    (let [nested-schema {:type :object
                         :description "A nested object schema"
                         :properties {:user {:type :object
                                             :properties {:name {:type :string}
                                                          :details {:type :object
                                                                    :properties {:age {:type :integer}
                                                                                 :active {:type :boolean}}
                                                                    :required [:active]}}
                                             :required [:name]}
                                      :metadata {:type :object
                                                 :properties {:tags {:type :array
                                                                     :items {:type :string}}}}}
                         :required [:user]}
          converted-nested (schema/edn->sch nested-schema)
          properties (.properties converted-nested)
          user-schema (get properties "user")
          user-properties (.properties user-schema)
          details-schema (get user-properties "details")
          details-properties (.properties details-schema)
          metadata-schema (get properties "metadata")
          metadata-properties (.properties metadata-schema)
          tags-schema (get metadata-properties "tags")]

      (is (instance? JsonObjectSchema converted-nested))
      (is (= "A nested object schema" (.description converted-nested)))
      (is (= ["user"] (seq (.required converted-nested))))

      (is (instance? JsonObjectSchema user-schema))
      (is (= ["name"] (seq (.required user-schema))))
      (is (instance? JsonStringSchema (get user-properties "name")))

      (is (instance? JsonObjectSchema details-schema))
      (is (= ["active"] (seq (.required details-schema))))
      (is (instance? JsonIntegerSchema (get details-properties "age")))
      (is (instance? JsonBooleanSchema (get details-properties "active")))

      (is (instance? JsonObjectSchema metadata-schema))
      (is (instance? JsonArraySchema tags-schema))
      (is (instance? JsonStringSchema (.items tags-schema))))))

(deftest test-complex-combined-schemas
  (testing "Converting complex schemas combining multiple types"
    (let [complex-schema {:type :object
                          :description "A complex schema"
                          :properties {:id {:type :string}
                                       :tags {:type :array
                                              :items {:type :string}}
                                       :config {:type :object
                                                :properties {:mode {:enum ["simple" "advanced" "expert"]}
                                                             :limits {:type :object
                                                                      :properties {:max {:type :integer}
                                                                                   :min {:type :integer}}
                                                                      :required [:max :min]}
                                                             :flags {:type :array
                                                                     :items {:type :boolean}}}
                                                :required [:mode]}}
                          :required [:id :config]}
          converted-complex (schema/edn->sch complex-schema)
          properties (.properties converted-complex)
          tags-schema (get properties "tags")
          config-schema (get properties "config")
          config-properties (.properties config-schema)
          mode-schema (get config-properties "mode")
          limits-schema (get config-properties "limits")
          limits-properties (.properties limits-schema)
          flags-schema (get config-properties "flags")]

      (is (instance? JsonObjectSchema converted-complex))
      (is (= "A complex schema" (.description converted-complex)))
      (is (= #{"id" "config"} (set (.required converted-complex))))

      (is (instance? JsonStringSchema (get properties "id")))
      (is (instance? JsonArraySchema tags-schema))
      (is (instance? JsonStringSchema (.items tags-schema)))

      (is (instance? JsonObjectSchema config-schema))
      (is (= ["mode"] (seq (.required config-schema))))

      (is (instance? JsonEnumSchema mode-schema))
      (is (= ["simple" "advanced" "expert"] (.enumValues mode-schema)))

      (is (instance? JsonObjectSchema limits-schema))
      (is (= #{"max" "min"} (set (.required limits-schema))))
      (is (instance? JsonIntegerSchema (get limits-properties "max")))
      (is (instance? JsonIntegerSchema (get limits-properties "min")))

      (is (instance? JsonArraySchema flags-schema))
      (is (instance? JsonBooleanSchema (.items flags-schema))))))

(deftest test-string-json-conversion
  (testing "Converting JSON string schemas"
    (let [json-schema-str (json/write-str {:type "object"
                                           :properties {:name {:type "string"}
                                                        :age {:type "integer"}}
                                           :required ["name"]})
          schema-map (json/read-str json-schema-str :key-fn keyword)
          converted-schema (schema/edn->sch schema-map)
          properties (.properties converted-schema)]

      (is (instance? JsonObjectSchema converted-schema))
      (is (= ["name"] (seq (.required converted-schema))))
      (is (instance? JsonStringSchema (get properties "name")))
      (is (instance? JsonIntegerSchema (get properties "age"))))))

(deftest test-tool-specification-integration
  (testing "Using schema conversion with ToolSpecification"
    (let [schema {:type :object
                  :properties {:prompt {:type :string
                                        :description "The prompt to send"}
                               :options {:type :array
                                         :items {:type :string}
                                         :description "List of options"}}
                  :required [:prompt]}
          json-schema-str (json/write-str schema)]

      ;; Test with direct EDN schema
      (let [edn-converted (schema/edn->sch schema)
            properties (.properties edn-converted)]
        (is (instance? JsonObjectSchema edn-converted))
        (is (= ["prompt"] (seq (.required edn-converted))))
        (is (instance? JsonStringSchema (get properties "prompt")))
        (is (instance? JsonArraySchema (get properties "options")))
        (is (instance? JsonStringSchema (.items (get properties "options")))))

      ;; Test with JSON string after parsing
      (let [json-converted (schema/edn->sch (json/read-str json-schema-str :key-fn keyword))
            properties (.properties json-converted)]
        (is (instance? JsonObjectSchema json-converted))
        (is (= ["prompt"] (seq (.required json-converted))))
        (is (instance? JsonStringSchema (get properties "prompt")))
        (is (instance? JsonArraySchema (get properties "options")))
        (is (instance? JsonStringSchema (.items (get properties "options"))))))))








