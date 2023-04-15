(ns fhir.core
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.data :refer [diff]]
            [lambdaisland.deep-diff2 :as ddiff]))

(def Mismatch lambdaisland.deep_diff2.diff_impl.Mismatch)
(def Insertion lambdaisland.deep_diff2.diff_impl.Insertion)
(def Deletion lambdaisland.deep_diff2.diff_impl.Deletion)

(defn strip-url [url]
  (last (str/split url #"/")))

(defn diff-name
  "Gets the name of the Mismatch, Insertion or Deletion. Fails with an exception."
  [value]
  (let [val-type (type value)]
    (cond
      (= val-type Insertion) (str "+" (name (:+ value)))
      (= val-type Deletion) (str "-" (name (:- value)))
      (= val-type Mismatch) (str "+" (name (:+ value)) " -" (name (:- value))))))

(defn get-name
  "Tries to get the name of a symbol. On fail, tries to get the name of a
  mismatch. Fails with an exception."
  [value]
  (try
    (name value)
    (catch ClassCastException e
      (diff-name value))))

(defn traverse
  ([root] (traverse root []))
  ([root path]
   (if (map? root)
     ; traverse deeper
     (doseq [leaf (seq root)]
       (traverse (second leaf) (conj path (first leaf))))
     ; got to the leaf
     (println (str (str/join "." (map get-name path)) ": " root)))))

(comment
  (def types-r5 (json/parse-string (slurp "spec/r5/profiles-types.json")))
  (def entry-r5 (get types-r5 "entry"))
  (def types-r4 (json/parse-string (slurp "spec/r4/profiles-types.json")))
  (def entry-r4 (get types-r4 "entry"))

  ; ("resourceType" "id" "meta" "type" "entry")
  (prn (keys types-r5))

  ; ("fullUrl" "resource")
  (prn (keys (first entry-r5)))

  ; ("Element" "BackboneElement" "base64Binary" "boolean" "canonical" "code" "date" "dateTime" "decimal" "id" "instant" "integer" "integer64" "markdown" "oid" "positiveInt" "string" "time" "unsignedInt" "uri" "url" "uuid" "xhtml" "Address" "Age" "Annotation" "Attachment" "Availability" "BackboneType" "Base" "CodeableConcept" "CodeableReference" "Coding" "ContactDetail" "ContactPoint" "Contributor" "Count" "DataRequirement" "DataType" "Distance" "Dosage" "Duration" "ElementDefinition" "Expression" "ExtendedContactDetail" "Extension" "HumanName" "Identifier" "MarketingStatus" "Meta" "MonetaryComponent" "Money" "Narrative" "ParameterDefinition" "Period" "PrimitiveType" "ProductShelfLife" "Quantity" "Range" "Ratio" "RatioRange" "Reference" "RelatedArtifact" "SampledData" "Signature" "Timing" "TriggerDefinition" "UsageContext" "VirtualServiceDetail" "MoneyQuantity" "SimpleQuantity")
  (prn (map #(strip-url (get % "fullUrl")) entry-r5))

  ; ("abstract" "url" "experimental" "id" "snapshot" "name" "extension" "status"
  ; "text" "kind" "jurisdiction" "fhirVersion" "type" "baseDefinition"
  ; "publisher" "mapping" "version" "meta" "date" "differential" "resourceType"
  ; "contact" "derivation" "description")
  (prn (keys (get (first entry-r5) "resource")))

  (prn (diff {:a 1 :b 2} {:a 1 :c 3}))
  (prn (diff entry-r5 entry-r4))

  (def ver-diff (get (ddiff/minimize (ddiff/diff types-r5 types-r4)) "entry"))
  (prn (keys (get (second ver-diff) "resource")))

  (def test-tree {:a 1 :b 2 :d {:e {:f 4} :g 5 :h 6} :e [1 2 3 4]})
  (prn (ddiff/diff test-tree {:a 2 :c 3 :e [1 2 5 4]}))

  (def test-diff
    (ddiff/minimize
      (ddiff/diff {:a 1 :b 2 :d {:e 6} :g [1 2 4]}
                  {:a 1 :d {:e {:f 6}} :g [1 2 3 4]})))
  (prn test-diff)
  (traverse test-diff)

  (traverse (get (second ver-diff) "resource"))
  )
