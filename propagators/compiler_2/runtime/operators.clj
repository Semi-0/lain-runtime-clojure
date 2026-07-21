(ns propagators.compiler-2.runtime.operators
  "Compiler-2 runtime operator registry facade."
  (:require [propagators.compiler-2.runtime.operators.list-text :as list-text]
            [propagators.compiler-2.runtime.operators.environment :as environment]
            [propagators.compiler-2.runtime.operators.trace :as trace]
            [propagators.compiler-2.runtime.operators.tui :as tui]
            [propagators.compiler-2.runtime.operators.web-bridge :as web-bridge]
            [propagators.compiler-2.runtime.operators.xr :as xr]))

(def block-target-operator tui/block-target-operator)
(def p:block tui/p:block)
(def block-cell-operator tui/block-cell-operator)
(def block-at-operator tui/block-at-operator)
(def be-block-at-operator tui/be-block-at-operator)
(def be-block-target-operator tui/be-block-target-operator)
(def instance-operator tui/instance-operator)
(def translate-operator tui/translate-operator)

(def trace-target-operator tui/trace-target-operator)
(def trace-operator trace/trace-operator)

(def xr-io-operator xr/xr-io-operator)
(def io-xr-operator xr/io-xr-operator)

(def runtime-clients-operator web-bridge/runtime-clients-operator)
(def runtime-client-pipe-operator web-bridge/runtime-client-pipe-operator)

(def list-text-events-operator list-text/list-text-events-operator)

(def load-primitive-environment-operator
  environment/load-primitive-environment-operator)
(def load-lain-operator environment/load-lain-operator)
(def save-environment-operator environment/save-environment-operator)
(def load-blocks-operator environment/load-blocks-operator)
(def save-blocks-operator environment/save-blocks-operator)
