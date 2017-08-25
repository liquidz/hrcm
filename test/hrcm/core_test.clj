(ns hrcm.core-test
  (:require
    [clojure.test :refer :all]
    [hrcm.core :refer :all]
    [clojure.java.io :as io]
    [clojure.edn :as edn]
    ))

(deftest inbox-test
  (are [before after] (= (inbox before) after)
    {:inbox [1 2] :holding nil}
    {:inbox [2] :holding 1}
    
    {:inbox [] :holding nil}
    {:inbox [] :holding nil :end true}))

(deftest outbox-test
  (are [before after] (= (outbox before) after)
    {:holding 1 :outbox [0]}
    {:holding nil :outbox [0 1]}
    
    {:holding 1}
    {:holding nil :outbox [1]})
  (is (thrown? Throwable (outbox {})))
  ;(is (contains? (outbox {}) :failed))
  )

(deftest copyto-test
  (are [before target after] (= (copyto before target) after)
    {:holding 1}
    :target
    {:holding 1 :register {:target 1}}

    {:holding 1 :register {:addr 2}}
    [:addr]
    {:holding 1 :register {:addr 2, 2 1}})
  (is (contains? (copyto {} nil) :failed)))

(deftest copyfrom-test
  (are [before target after] (= (copyfrom before target) after)
    {:register {:target 1}}
    :target
    {:holding 1 :register {:target 1}}

    {:register {:addr 2, 2 1}}
    [:addr]
    {:holding 1 :register {:addr 2, 2 1}})
  (is (contains? (copyfrom {} :x) :failed)))

(deftest jump-test
  (are [before id after] (= (jump before id) after)
    {:codes [0 [:label "foo"] 2] :pos 99}
    "foo"
    {:codes [0 [:label "foo"] 2] :pos 1}))

(deftest jump-if-zero-test
  (are [before id after] (= (jump-if-zero before id) after)
    {:holding 0 :codes [0 [:label "foo"] 2] :pos 99}
    "foo"
    {:holding 0 :codes [0 [:label "foo"] 2] :pos 1}
    
    {:holding 1 :codes [0 [:label "foo"] 2] :pos 99}
    "foo"
    {:holding 1 :codes [0 [:label "foo"] 2] :pos 99})

  (doseq [c [{} {:holding "A"}]]
    (is (contains? (jump-if-zero c nil) :failed))))

(deftest jump-if-neg-test
  (are [before id after] (= (jump-if-neg before id) after)
    {:holding -1 :codes [0 [:label "foo"] 2] :pos 99}
    "foo"
    {:holding -1 :codes [0 [:label "foo"] 2] :pos 1}
    
    {:holding 0 :codes [0 [:label "foo"] 2] :pos 99}
    "foo"
    {:holding 0 :codes [0 [:label "foo"] 2] :pos 99})
  
  (doseq [c [{} {:holding "A"}]]
    (is (contains? (jump-if-neg c nil) :failed))))

(deftest add-test
  (are [before target after] (= (add before target) after)
    {:holding 1 :register {:target 2}}
    :target
    {:holding 3 :register {:target 2}}

    {:holding 1 :register {:addr 0, 0 2}}
    [:addr]
    {:holding 3 :register {:addr 0, 0 2}})

  (is (contains? (add {:holding \A} nil) :failed))
  (is (contains? (add {:holding 1 :register {:target \A}} :target) :failed)))

(deftest sub-test
  (are [before target after] (= (sub before target) after)
    {:holding 9 :register {:target 2}}
    :target
    {:holding 7 :register {:target 2}}

    {:holding 9 :register {:addr 0, 0 2}}
    [:addr]
    {:holding 7 :register {:addr 0, 0 2}}

    {:holding \B :register {:target \A}}
    :target
    {:holding 1 :register {:target \A}})

  (is (contains? (add {:holding \A :register {:target 1}} :target) :failed))
  (is (contains? (add {:holding 1 :register {:target \A}} :target) :failed)))

(deftest bump-inc-test
  (are [before target after] (= (bump-inc before target) after)
    {:holding 0 :register {:target 1}}
    :target
    {:holding 2 :register {:target 2}}

    {:holding 0 :register {:addr 0, 0 1}}
    [:addr]
    {:holding 2 :register {:addr 0, 0 2}}))

(deftest bump-dec-test
  (are [before target after] (= (bump-dec before target) after)
    {:holding 9 :register {:target 1}}
    :target
    {:holding 0 :register {:target 0}}

    {:holding 9 :register {:addr 0, 0 1}}
    [:addr]
    {:holding 0 :register {:addr 0, 0 0}}))

(deftest run*-test
  (testing "samples: count down"
    (let [c   (edn/read-string (slurp "samples/countdown.edn"))
          res (run* c)]
      (is (:end res))
      (is (= (:answer c) (:outbox res)))))

  (testing "samples: fixme"
    (let [c   (edn/read-string (slurp "samples/hello.edn"))
          res (run* c)]
      (is (:end res))
      (is (= (:answer c) (:outbox res)))))
  )
