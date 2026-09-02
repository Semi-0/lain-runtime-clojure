(ns propagators.runtime.session.program.topology
  "Program-net topology seeding for compiler-2 runtime TUI blocks."
  (:require [propagators.runtime.session.program :as program]
            [propagators.infra.datastructures.compound-object :as obj]
            [propagators.infra.network-builder :as nb]))

(defn- seed-instance
  [runtime-net [program-net props] {:keys [instance-id blocks-id]}]
  (let [program-net (-> program-net
                        (program/seed-program-block-cell runtime-net instance-id)
                        (program/seed-program-block-cell runtime-net blocks-id))
        [_cell-id prop-ids program-net]
        (obj/install-slot-access program-net
                                 :instance/blocks
                                 instance-id
                                 blocks-id)]
    [program-net (into props prop-ids)]))

(defn- seed-block
  [runtime-net [program-net props] block]
  (let [program-net (reduce #(program/seed-program-block-cell %1
                                                              runtime-net
                                                              %2)
                            program-net
                            [(:block-id block) (:index-id block)
                             (:next-id block) (:text-id block)
                             (:display-id block)])
        [_index-cell index-props program-net]
        (obj/install-slot-access program-net
                                 :block/index
                                 (:block-id block)
                                 (:index-id block))
        program-net (if (:text-current-source? block)
                      (nb/install-cell program-net
                                       (:text-id block)
                                       (:text-current block)
                                       (:text-current block))
                      program-net)
        [_text-cell text-props program-net]
        (obj/install-slot-access program-net
                                 :block/text
                                 (:block-id block)
                                 (:text-id block))
        [_display-cell display-props program-net]
        (obj/install-slot-access program-net
                                 :block/display
                                 (:block-id block)
                                 (:display-id block))
        [_next-cell next-props program-net]
        (obj/install-slot-access program-net
                                 :cdr
                                 (:block-id block)
                                 (:next-id block))]
    [program-net
     (into props cat [index-props text-props display-props next-props])]))

(defn seed-program-topology
  [state program-net]
  (let [runtime-net (:network state)
        [program-net props] (reduce (partial seed-instance runtime-net)
                                    [program-net []]
                                    (vals (:tuis state)))
        [program-net props] (reduce (partial seed-block runtime-net)
                                    [program-net props]
                                    (program/all-blocks state))]
    (nb/run-propagators program-net props)))

(defn seed-appended-block-topology-state
  [state client-id block previous-tail]
  (let [runtime-net (:network state)
        tui (get-in state [:tuis client-id])
        [program-net props] (if tui
                              (seed-instance runtime-net
                                             [(:program/net state) []]
                                             tui)
                              [(:program/net state) []])
        [program-net props] (if previous-tail
                              (seed-block runtime-net
                                          [program-net props]
                                          previous-tail)
                              [program-net props])
        [program-net props] (seed-block runtime-net
                                        [program-net props]
                                        block)]
    (assoc state :program/net (nb/run-propagators program-net props))))
