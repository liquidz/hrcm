(ns hrcm.core)

(def ^:private inboxes  (atom []))
(def ^:private outboxes (atom []))
;(def ^:private register (atom {}))
;(def ^:private holding  (atom {}))

(defn inbox
  []
  (let [hold (first @inboxes)]
    (swap! inboxes rest)
    hold))

(defn outbox
  [holding]
  (if-not holding
    (throw (ex-info "not holding" {})))
  (swap! outboxes conj holding))

(defn copyto
  [hold target]
  )

(defn run*
  [programs]
  (let [pos' (atom 0)]
    (try
      (loop [pos 0
             holding nil]
        (reset! pos' pos)
        (let [[op & args] (nth programs pos nil)]
          (case op
            :inbox
            (if-let [hold (inbox)]
              (recur (inc pos) hold))

            :outbox
            (do (outbox holding)
                (recur (inc pos) nil))

            ;:copyto
            ;(do
            ;  (apply copyto holding args)
            ;  )

            "finish")))
        (catch Exception e
          )
        )
    )
      (catch Exception e
        (throw (ex-info
                 (.getMessage e)
                 (merge (ex-data e) {:position pos :holding holding})))
        nil))
    )
  )

(defn run
  [& args]
  (try
    (apply run* args)
    (catch Exception e
      (println e))))


(comment
  (do
    (reset! inboxes [1 2])
    (reset! outboxes [])
    (run '[
           ;(:inbox)
           ;(:copyto 1)
           ;(:copyto [1])
           (:outbox)
           ])
    (println @inboxes @outboxes)
    )
  )
