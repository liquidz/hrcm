(ns hrcm.core)

(defn- inc-pos
  [c]
  (update c :pos inc))

(defn inbox
  [{[x & rst] :inbox :as c}]
  (if x
    (assoc c :inbox rst :holding x)
    (assoc c :finish true)))

(defn outbox
  [{:keys [holding outbox] :or {outbox []} :as c}]
  (if holding
    (assoc c :holding nil :outbox (conj outbox holding))
    (assoc c :failed "FIXME")))

(defn- address
  [register target]
  (if (sequential? target)
    (get register (first target) nil)
    target))

(defn copyto
  [{:keys [register holding] :or {register {}} :as c} target]
  (if-not holding
    (assoc c :failed "FIXME")
    (update c :register
            assoc (address register target) holding)))

(defn copyfrom
  [{:keys [register] :or {register {}} :as c} target]
  (if-let [x (get register (address register target))]
    (assoc c :holding x)
    (assoc c :failed "no data")))

(defn label
  [c id]
  c)

(defn- find-label-index
  [codes id]
  (reduce
    (fn [index code]
      (if (= code [:label id])
        (reduced index)
        (inc index)))
    0 codes))

(defn jump
  [{:keys [codes] :as c} id]
  (assoc c :pos (find-label-index codes id)))

(defn jump-if-zero
  [{:keys [holding codes] :as c} id]
  (cond
    (not holding)
    (assoc c :failed "not holding")

    (not (number? holding))
    (assoc c :failed "must be number")

    (zero? holding)
    (assoc c :pos (find-label-index codes id))

    :else c))

(defn jump-if-neg
  [{:keys [holding codes] :as c} id]
  (cond
    (not holding)
    (assoc c :failed "not holding")

    (not (number? holding))
    (assoc c :failed "must be number")

    (< holding 0)
    (assoc c :pos (find-label-index codes id))

    :else c))

;(defn run*
;  [programs]
;  (let [pos' (atom 0)]
;    (try
;      (loop [pos 0
;             holding nil]
;        (reset! pos' pos)
;        (let [[op & args] (nth programs pos nil)]
;          (case op
;            :inbox
;            (if-let [hold (inbox)]
;              (recur (inc pos) hold))
;
;            :outbox
;            (do (outbox holding)
;                (recur (inc pos) nil))
;
;            ;:copyto
;            ;(do
;            ;  (apply copyto holding args)
;            ;  )
;
;            "finish")))
;        (catch Exception e
;          )
;        )
;    )
;      (catch Exception e
;        (throw (ex-info
;                 (.getMessage e)
;                 (merge (ex-data e) {:position pos :holding holding})))
;        nil))
;    )
;  )
;
;(defn run
;  [& args]
;  (try
;    (apply run* args)
;    (catch Exception e
;      (println e))))
;
;
;(comment
;  (do
;    (reset! inboxes [1 2])
;    (reset! outboxes [])
;    (run '[
;           ;(:inbox)
;           ;(:copyto 1)
;           ;(:copyto [1])
;           (:outbox)
;           ])
;    (println @inboxes @outboxes)
;    )
;  )
