(ns propagators.compiler-2.runtime.operators.tui
  "TUI/block compiler-2 runtime operators."
  (:require [propagators.compiler-2.runtime.operators.tui.effects :as effects]
            [propagators.compiler-2.runtime.operators.tui.targets :as targets]
            [propagators.compiler-2.runtime.operators.tui.translate :as translate]))

(def block-target-operator targets/block-target-operator)
(def p:block targets/p:block)
(def block-cell-operator targets/block-cell-operator)
(def trace-target-operator targets/trace-target-operator)
(def instance-operator targets/instance-operator)
(def block-at-operator effects/block-at-operator)
(def be-block-at-operator effects/be-block-at-operator)
(def be-block-target-operator effects/be-block-target-operator)
(def translate-operator translate/translate-operator)
