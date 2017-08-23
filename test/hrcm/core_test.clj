(ns hrcm.core-test
  (:require
    [clojure.test :refer :all]
    [hrcm.core :refer :all]))

(deftest inbox-test
  (are [before after] (= (inbox before) after)
    {:inbox [1 2] :holding nil}
    {:inbox [2] :holding 1}
    
    {:inbox [] :holding nil}
    {:inbox [] :holding nil :finish true}))

(deftest outbox-test
  (are [before after] (= (outbox before) after)
    {:holding 1 :outbox [0]}
    {:holding nil :outbox [0 1]}
    
    {:holding 1}
    {:holding nil :outbox [1]})
  (is (contains? (outbox {}) :failed)))

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

;(deftest a-test
;  (testing "FIXME, I fail."
;    (is (= 0 1))))
