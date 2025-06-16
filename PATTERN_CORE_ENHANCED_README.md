# Pattern Core Enhanced - Reader Conditional Support

## Overview

The `pattern_core_enhanced.clj` module extends the original `pattern_core.clj` to support Clojure's reader conditionals in `.cljc` files. This enhancement allows the pattern-based file exploration to discover and analyze functions that are defined inside reader conditionals like `#?(:clj ...)` and `#?@(:clj [...])`.

## Problem Solved

The original `pattern_core.clj` would miss functions defined inside reader conditionals because:
- Reader conditionals are parsed as `:reader-macro` nodes by rewrite-clj
- The forms inside these macros need special handling to extract
- Platform-specific code would be invisible to pattern searches

## Key Features

### 1. Reader Conditional Support

- **Regular conditionals**: `#?(:clj (defn foo [] ...) :cljs (defn foo [] ...))`
- **Splicing conditionals**: `#?@(:clj [(def x 1) (def y 2)])`
- **Platform labeling**: Functions are labeled with their platform, e.g., `"platform-specific-function [:clj]"`

### 2. Unified Validation Logic

- `valid-node-to-include?`: Core validation for nodes (filters whitespace, newlines, etc.)
- `valid-form-to-include?`: Wrapper for zipper locations
- Reused throughout to ensure consistent filtering

### 3. Enhanced Form Collection

The `collect-top-level-forms` function now:
1. Traverses the file using rewrite-clj zippers
2. Detects `:reader-macro` nodes
3. Extracts forms from inside conditionals
4. Labels each form with its platform when applicable

## Implementation Details

### Key Functions

```clojure
(defn extract-forms-from-reader-conditional
  "Extract forms from inside reader conditionals like #?(:clj ...) and #?@(:clj [...])"
  [reader-node]
  ;; Handles both regular and splicing reader conditionals
  ;; Returns [{:node form-node :platform platform-keyword}...]
  )

(defn process-form-node
  "Process a form node and extract metadata"
  [node platform]
  ;; Extracts name, type, content from a node
  ;; Adds platform suffix to name if platform is specified
  )
```

### How It Works

1. **Detection**: When traversing a file, check for `:reader-macro` tags
2. **Extraction**: Parse the reader conditional structure:
   - First child is the token (`?` or `?@`)
   - Second child contains the platform/form pairs
3. **Processing**: 
   - For regular conditionals: Extract the form directly
   - For splicing conditionals: Extract forms from the vector
4. **Labeling**: Append platform info to function names

## Usage Examples

```clojure
(require '[clojure-mcp.tools.unified-read-file.pattern-core-enhanced :as enhanced])

;; Collect all forms including those in reader conditionals
(def forms (enhanced/collect-top-level-forms "path/to/file.cljc" false))

;; Filter by pattern - finds platform-specific functions
(def result (enhanced/generate-pattern-based-file-view 
              "path/to/file.cljc" 
              "platform-specific"  ; name pattern
              nil))                ; content pattern

;; Results include platform information
;; => {:matches [{:name "platform-specific-function [:clj]", :platform :clj}
;;               {:name "platform-specific-function [:cljs]", :platform :cljs}]}
```

## Writing Tests

### Test File Structure

Create a test file at `test/clojure_mcp/tools/unified_read_file/pattern_core_enhanced_test.clj`:

```clojure
(ns clojure-mcp.tools.unified-read-file.pattern-core-enhanced-test
  (:require [clojure.test :refer [deftest testing is]]
            [clojure-mcp.tools.unified-read-file.pattern-core-enhanced :as enhanced]
            [clojure.java.io :as io]))

(deftest test-reader-conditional-extraction
  (testing "Extract forms from regular reader conditionals"
    (let [test-content "(ns test)
                        
                        #?(:clj 
                           (defn clj-only []
                             :clj-version))
                        
                        #?(:cljs
                           (defn cljs-only []
                             :cljs-version))"
          temp-file (doto (io/file "test-reader-conditional.cljc")
                      (spit test-content))]
      (try
        (let [forms (enhanced/collect-top-level-forms (.getPath temp-file) false)]
          (is (= 3 (count forms))) ; ns + 2 platform-specific defns
          (is (some #(= "clj-only [:clj]" (:name %)) forms))
          (is (some #(= "cljs-only [:cljs]" (:name %)) forms)))
        (finally
          (.delete temp-file))))))

(deftest test-splicing-reader-conditional
  (testing "Extract forms from splicing reader conditionals"
    (let [test-content "(ns test)
                        
                        #?@(:clj [(def x 1)
                                  (def y 2)]
                            :cljs [(def a 3)
                                   (def b 4)])"
          temp-file (doto (io/file "test-splicing.cljc")
                      (spit test-content))]
      (try
        (let [forms (enhanced/collect-top-level-forms (.getPath temp-file) false)]
          (is (= 5 (count forms))) ; ns + 4 defs
          (is (some #(= "x [:clj]" (:name %)) forms))
          (is (some #(= "y [:clj]" (:name %)) forms))
          (is (some #(= "a [:cljs]" (:name %)) forms))
          (is (some #(= "b [:cljs]" (:name %)) forms)))
        (finally
          (.delete temp-file))))))

(deftest test-pattern-matching-with-reader-conditionals
  (testing "Pattern matching finds platform-specific functions"
    (let [test-content "(ns test)
                        
                        (defn common-fn []
                          :common)
                        
                        #?(:clj
                           (defn platform-fn []
                             :clj-impl))
                        
                        #?(:cljs
                           (defn platform-fn []
                             :cljs-impl))"
          temp-file (doto (io/file "test-pattern-match.cljc")
                      (spit test-content))]
      (try
        (let [result (enhanced/generate-pattern-based-file-view
                       (.getPath temp-file)
                       "platform"
                       nil)]
          (is (= 2 (count (:matches result))))
          (is (every? #(= :platform-fn (keyword (first (clojure.string/split (:name %) #" ")))) 
                      (:matches result))))
        (finally
          (.delete temp-file))))))

(deftest test-node-validation
  (testing "Node validation filters correctly"
    ;; Test that whitespace, newlines, etc. are filtered
    (is (not (enhanced/valid-node-to-include? 
               {:tag :whitespace} 
               false)))
    (is (not (enhanced/valid-node-to-include? 
               {:tag :newline} 
               false)))
    (is (enhanced/valid-node-to-include? 
             {:tag :list} 
             false))
    ;; Test comment handling
    (is (not (enhanced/valid-node-to-include? 
               {:tag :comment} 
               false)))
    (is (enhanced/valid-node-to-include? 
             {:tag :comment} 
             true))))
```

### Running Tests

```bash
# Run all tests
clojure -X:test

# Run specific test namespace
clojure -X:test :nses '[clojure-mcp.tools.unified-read-file.pattern-core-enhanced-test]'

# Run with test selector
clojure -X:test :focus :reader-conditional
```

### Test Best Practices

1. **Use temporary files**: Create test `.cljc` files dynamically to avoid dependencies
2. **Test edge cases**: Empty conditionals, nested forms, malformed syntax
3. **Verify platform labels**: Ensure platform information is correctly preserved
4. **Test pattern matching**: Verify that patterns work across conditional boundaries
5. **Clean up**: Always delete temporary files in `finally` blocks

## Integration with Existing Tools

To integrate this enhanced version:

1. Update the tool implementation to use `pattern-core-enhanced` instead of `pattern-core`
2. Ensure the UI displays platform information appropriately
3. Consider adding platform-specific filtering options

## Future Enhancements

1. Support for reader conditionals in other contexts (e.g., inside function bodies)
2. Platform-specific pattern matching (e.g., find only `:clj` implementations)
3. Support for feature conditionals beyond `:clj` and `:cljs`
4. Handling of nested reader conditionals