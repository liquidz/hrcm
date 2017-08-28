(ns hrcm.core
  (:require
    [clojure.spec.alpha :as s]))

(s/def ::value     (s/or :char char? :number number?))
(s/def ::number    number?)
(s/def ::holding   ::value)
;(s/def ::same-class )
(s/def ::must-hold (s/keys :req-un [::holding]))

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
  {:pre [(s/valid? ::must-hold c)]}
  (assoc c :holding nil :outbox (conj outbox holding)))

(defn- address
  [register target]
  (if (sequential? target)
    (get register (first target) nil)
    target))

(defn- get*
  [register target]
  (get register (address register target)))

(defn copyto
  [{:keys [register holding] :or {register {}} :as c} target]
  {:pre [(s/valid? ::must-hold c)]}
  (update c :register
          assoc (address register target) holding))

(defn copyfrom
  [{:keys [register] :or {register {}} :as c} target]
  (let [x (get* register target)]
    (assert (s/valid? ::holding x))
    (assoc c :holding x)))

(defn add
  [{:keys [holding register] :or {register {}} :as c} target]
  {:pre [(s/valid? ::must-hold c)]}
  (let [x (get* register target)]
    (assert (s/valid? ::number x))
    (assert (s/valid? ::number holding))
    (assoc c :holding (+ holding x))))

(defn sub
  [{:keys [holding register] :or {register {}} :as c} target]
  {:pre [(s/valid? ::must-hold c)]}
  (let [x (get* register target)]
    (cond
      (not holding)
      (assoc c :failed "not holding")

      (not x)
      (assoc c :failed "no data")

      (not= (class holding) (class x))
      (assoc c :failed "must be same type")

      :else
      (assoc c :holding (if (char? x)
                          (- (int holding) (int x))
                          (- holding x))))))

(defn bump-inc
  [{:keys [register] :or {register {}} :as c} target]
  (let [addr (address register target)
        x    (some-> (get register addr) inc)]
    (if (and x (number? x))
      (assoc c :holding x :register (assoc register addr x))
      (assoc c :failed "no data"))))

(defn bump-dec
  [{:keys [register] :or {register {}} :as c} target]
  (let [addr (address register target)
        x    (some-> (get register addr) dec)]
    (if (and x (number? x))
      (assoc c :holding x :register (assoc register addr x))
      (assoc c :failed "no data"))))

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

(def ^:private operations
  {:inbox        (comp inc-step inc-pos inbox)
   :outbox       (comp inc-step inc-pos outbox)
   :copyfrom     (comp inc-step inc-pos copyfrom)
   :copyto       (comp inc-step inc-pos copyto)
   :add          (comp inc-step inc-pos add)
   :sub          (comp inc-step inc-pos sub)
   :bump+        (comp inc-step inc-pos bump-inc)
   :bump-        (comp inc-step inc-pos bump-dec)
   :jump         (comp inc-step inc-pos jump)
   :jump-if-zero (comp inc-step inc-pos jump-if-zero)
   :jump-if-neg  (comp inc-step inc-pos jump-if-neg)
   :label        (comp inc-pos  label)})

(defn run*
  [{:keys [pos codes] :or {pos 0} :as c}]
  (let [[op & args] (nth codes pos nil)
        opf (get operations op)]
    (when-not opf (throw (ex-info "unknown operation" {:op op})))
    ;(println (assoc c :codes (cons op args)))
    (let [{:keys [end failed] :as res} (apply opf c args)]
      (if (or failed end)
        (update res :step dec)
        (run* res)))))


(comment

  (defn foo [x]
    {:pre [(s/valid? ::must-hold x)]}
    (println x))

  (foo {:hoolding 1})
  )
