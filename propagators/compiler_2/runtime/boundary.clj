(ns propagators.compiler-2.runtime.boundary
  "Boundary effect request and receipt data for compiler-2 runtime ports.")

(defn tui-write-effect-request
  [effect-id text-id payload epoch]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :tui
   :boundary/kind :tui/write-block
   :boundary/target {:text-id text-id}
   :boundary/payload payload
   :boundary/epoch epoch})

(defn tui-display-effect-request
  [effect-id display-id payload tick]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :tui
   :boundary/kind :tui/write-display
   :boundary/target {:display-id display-id}
   :boundary/payload payload
   :boundary/tick tick
   :boundary/epoch tick})

(defn xr-effect-request
  [effect-id trace-graph receipt-id epoch]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :xr
   :boundary/kind :xr/launch-trace
   :boundary/payload {:graph trace-graph}
   :boundary/receipt-id receipt-id
   :boundary/epoch epoch})

(defn xr-trace-subscribe-request
  [effect-id request target-id epoch]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :xr
   :boundary/kind :xr/trace-subscribe
   :boundary/payload {:request request
                      :target-id target-id}
   :boundary/epoch epoch})

(defn xr-receipt
  [request status]
  {:boundary/receipt true
   :boundary/id (:boundary/id request)
   :boundary/port (:boundary/port request)
   :boundary/kind (:boundary/kind request)
   :boundary/status status
   :boundary/epoch (:boundary/epoch request)})

(defn environment-effect-request
  "Declare a compiler-2 environment/file boundary request.

  The compiling propagator only constructs this value.  Filesystem access,
  Clojure loading, compilation, and receipt delivery belong to the runtime
  boundary driver."
  [effect-id kind payload receipt-id]
  {:boundary/effect true
   :boundary/id effect-id
   :boundary/port :environment
   :boundary/kind kind
   :boundary/payload payload
   :boundary/receipt-id receipt-id})

(defn environment-receipt
  [request status details]
  (merge {:boundary/receipt true
          :boundary/id (:boundary/id request)
          :boundary/port :environment
          :boundary/kind (:boundary/kind request)
          :boundary/status status}
         details))
