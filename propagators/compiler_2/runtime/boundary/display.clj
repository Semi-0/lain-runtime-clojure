(ns propagators.compiler-2.runtime.boundary.display
  "Generic runtime display-update normalization."
  (:require [propagators.datastructures.compound-information :as information]
            [propagators.datastructures.event :as event]
            [propagators.semantic-trace :as semantic-trace]))

(defn update-value
  "Preserve semantic partial information; give raw display values event identity."
  [display-id source-id tick payload]
  (if (or (information/semantic-kind payload)
          (semantic-trace/semantic-trace-graph? payload))
    payload
    (event/active-event display-id source-id tick payload)))
