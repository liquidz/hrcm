(ns hrcm.core)

(defn- inc-pos
  [{:keys [pos] :or {pos 0} :as c}]
  (assoc c :pos (inc pos)))
(defn- inc-step
  [{:keys [step] :or {step 0} :as c}]
  (assoc c :step (inc step)))

(defn inbox
  [{[x & rst] :inbox :as c}]
  (if x
    (assoc c :inbox rst :holding x)
    (assoc c :end true)))

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
  (println "kiteru")
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

(def ^:private operations
  {
   :inbox  (comp inc-step inc-pos inbox)
   :outbox (comp inc-step inc-pos outbox)
   :jump   (comp inc-step inc-pos jump)
   :label  (comp inc-pos label)
   }
  
  )

(defn run
  [{:keys [pos codes] :or {pos 0} :as c}]
  (let [[op & args] (nth codes pos nil)
        opf (get operations op)]
    (when-not opf (throw (ex-info "unknown operation" {:op op})))

    (let [{:keys [step end failed] :as res} (apply opf c args)]
      (cond
        failed
        (println "FAILED: " (update res :step dec))

        end
        (println "FINISHED: " (update res :step dec))

        :else
        (run res)))))

(comment

  (let [sample
        {:inbox (range 12)
         :codes [
                 [:label "top"]
                 [:inbox]
                 [:outbox]
                 [:inbox]
                 [:outbox]
                 [:jump "top"]]
         }
        ]
    ; size 3
    ; speed 30
    (run sample)
    )
  
  )
