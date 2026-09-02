(ns propagators.runtime.operators
  "Compiler-2 runtime operator registry facade."
  (:require [propagators.runtime.operators.list-text :as list-text]
            [propagators.runtime.operators.environment :as environment]
            [propagators.runtime.operators.trace :as trace]
            [propagators.runtime.operators.session :as tui]))

(def block-target-operator tui/block-target-operator)
(def p:block tui/p:block)
(def block-cell-operator tui/block-cell-operator)
(def block-at-operator tui/block-at-operator)
(def be-block-at-operator tui/be-block-at-operator)
(def be-event-block-at-operator tui/be-event-block-at-operator)
(def be-block-target-operator tui/be-block-target-operator)
(def instance-operator tui/instance-operator)
(def translate-operator tui/translate-operator)

(def trace-target-operator tui/trace-target-operator)
(def trace-operator trace/trace-operator)

(def list-text-events-operator list-text/list-text-events-operator)

(def load-primitive-environment-operator
  environment/load-primitive-environment-operator)
(def load-lain-operator environment/load-lain-operator)
(def save-environment-operator environment/save-environment-operator)
(def load-blocks-operator environment/load-blocks-operator)
(def save-blocks-operator environment/save-blocks-operator)
