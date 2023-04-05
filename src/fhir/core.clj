(ns fhir.core
  (:require [cheshire.core :as json]
            [clojure.string :as str]))

(defn strip-url [url]
  (last (str/split url #"/")))

(comment
  (def types (json/parse-string (slurp "spec/r5/profiles-types.json")))
  (def entry (get types "entry"))
  ; ("resourceType" "id" "meta" "type" "entry")
  (prn (keys types))
  ; ("fullUrl" "resource") 
  (prn (keys (first entry)))
  (prn (map #(strip-url (get % "fullUrl")) entry))
  ; ("abstract" "url" "experimental" "id" "snapshot" "name" "extension" "status"
  ; "text" "kind" "jurisdiction" "fhirVersion" "type" "baseDefinition"
  ; "publisher" "mapping" "version" "meta" "date" "differential" "resourceType"
  ; "contact" "derivation" "description")
  (prn (keys (get (first entry) "resource")))
  )
