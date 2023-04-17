(ns fhir.core
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [clojure.data :refer [diff]]
            [lambdaisland.deep-diff2 :as ddiff]))

(def Mismatch lambdaisland.deep_diff2.diff_impl.Mismatch)
(def Insertion lambdaisland.deep_diff2.diff_impl.Insertion)
(def Deletion lambdaisland.deep_diff2.diff_impl.Deletion)

(defn diff-record? [value]
  (let [t (type value)]
    (or (= t Mismatch) (= t Insertion) (= t Deletion))))

(defn strip-url [url]
  (last (str/split url #"/")))

(defn strip-diff-record [value]
  (let [t (type value)]
    (cond
      (= t Insertion) (:+ value)
      (= t Deletion) (:- value)
      :else value)))

(defn get-name
  "Tries to get the name of a symbol. On fail, tries to get the name of a
  mismatch. Fails with an exception."
  [value]
  (let [v (strip-diff-record value)]
    (try
      (name v)
      (catch ClassCastException _e v))))

(defn tag [k v]
  (let [key-type (type k)
        value-type (type v)]
    (cond
      (= value-type Mismatch) :mismatch
      (or (= key-type Deletion)
          (= value-type Deletion)) :deletion
      (or (= key-type Insertion)
          (= value-type Insertion)) :insertion
      (map? v) :map
      (vector? v) :vector
      :else :atom)))

(defn join-path [path]
  (str/join "." (map get-name path)))

(defn val->str [value]
  (let [s (str value)
        lim 32]
    (if (> (count s) lim)
      (str (subs s 0 lim) "...")
      s)))

(defn traverse
  ([root] (traverse root []))
  ([root path]
   (let [p (join-path path)]
     (case (tag (last path) root)
       :mismatch
       (println p "=" (val->str (:- root)) "->" (val->str (:+ root)))

       :deletion
       (println "-" p "=" (-> root strip-diff-record val->str))

       :insertion
       (println "+" p "=" (-> root strip-diff-record val->str))

       :atom
       (println p "=" (val->str root))

       :map
       ; traverse deeper
       (doseq [leaf (seq root)]
         (traverse (second leaf) (conj path (first leaf))))

       :vector
       (traverse (apply assoc {} (interleave (range) root)) path)))))

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
    ;(ddiff/minimize
      (ddiff/diff {:same 1 :rm 2 :rename 4 :d {:e 6} :reorder [1 2 4 3]}
                  {:same 1 :add 3 :renamed 4 :d {:e {:f 6}} :reorder [2 2 3 4]})
     ;)
    )
  (prn test-diff)
  (traverse test-diff)

  (traverse (get (second ver-diff) "resource"))
  (traverse ver-diff)
  )
