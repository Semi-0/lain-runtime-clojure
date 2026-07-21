(ns propagators.compiler-2.runtime.ids
  "Stable identifiers and boundary slot keys for the compiler-2 runtime."
  (:require [propagators.ids :as ids]))

(def default-xr-client-id "xr")

(def external-source-client-id ::external-source)

(defn stable-node-id
  [& parts]
  (ids/->NodeId
   (java.util.UUID/nameUUIDFromBytes
    (.getBytes (pr-str (into [:compiler-2-runtime] parts)) "UTF-8"))))

(defn runtime-graph-id []
  (stable-node-id :runtime :semantic-graph))

(defn xr-outbox-id []
  (stable-node-id :runtime :xr :outbox))

(defn boundary-outbox-id []
  (xr-outbox-id))

(defn receipt-slot-key
  [effect-id]
  (str "boundary/receipt:" (hash effect-id)))

(defn effect-slot-key
  [effect-id]
  (str "boundary/effect:" (hash effect-id)))
