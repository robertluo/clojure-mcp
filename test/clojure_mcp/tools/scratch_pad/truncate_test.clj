(ns clojure-mcp.tools.scratch-pad.truncate-test
  (:require [clojure.test :refer :all]
            [clojure-mcp.tools.scratch-pad.truncate :as truncate]))

(deftest truncate-depth-test
  (testing "Basic truncation at different depths"
    (let [data {:a {:b {:c {:d "deep"}}}}]
      (is (= {:a '...}
             (truncate/truncate-depth data 0)))
      (is (= {:a {:b '...}}
             (truncate/truncate-depth data 1)))
      (is (= {:a {:b {:c '...}}}
             (truncate/truncate-depth data 2)))))

  (testing "Empty collections remain empty"
    (let [data {:empty-vec []
                :empty-map {}
                :empty-set #{}}]
      (is (= {:empty-vec [] :empty-map {} :empty-set #{}}
             (truncate/truncate-depth data 1)))))

  (testing "Size indicators for sequences"
    (is (= ['... '... '... '...10_elements]
           (truncate/truncate-depth (vec (range 10)) 0)))
    (is (= ['... '... '...5_elements]
           (truncate/truncate-depth (vec (range 5)) 0 {:show-first 2}))))

  (testing "Size indicators for maps"
    (let [large-map (zipmap (range 10) (repeat "val"))]
      (is (= {0 '... 7 '... 1 '... '... '...10_entries}
             (truncate/truncate-depth large-map 0)))))

  (testing "Size indicators for sets"
    (is (= #{'...8_items '...}
           (truncate/truncate-depth #{:a :b :c :d :e :f :g :h} 0))))

  (testing "Preserves collection types"
    (let [data {:sorted (sorted-map :z 1 :a 2)
                :list '(1 2 3)
                :vec [1 2 3]}]
      (let [result (truncate/truncate-depth data 1)]
        (is (sorted? (:sorted result)))
        (is (list? (:list result)))
        (is (vector? (:vec result))))))

  (testing "Custom ellipsis"
    (is (= {:a {:b '<...>}}
           (truncate/truncate-depth {:a {:b {:c 1}}} 1 {:ellipsis '<...>}))))

  (testing "Disable size indicators"
    (is (= ['... '... '... '... '...]
           (truncate/truncate-depth (vec (range 5)) 0 {:show-size false})))))

(deftest pprint-truncated-test
  (testing "Pretty prints truncated data"
    (let [result (truncate/pprint-truncated {:a {:b {:c 1}}} 1)]
      (is (string? result))
      (is (re-find #"\.\.\." result)))))
