(ns psil-interpreter.core-test
  (:require [clojure.test :refer :all]
            [psil-interpreter.core :refer :all]))

(deftest rrest-test
  (testing "rrest with a regular list and an empty list"
    (is (= (rrest '(0 1 2 3 4)) '(2 3 4)))
    (is (= (rrest '(0)) '()))))

(deftest remove-edges-test
  (testing "remove-edges with a regular string and an empty string"
    (is (= (remove-edges "abcdefg") "bcdef"))
    (is (= (remove-edges "") ""))))

(deftest third-test
  (testing "third with a regular list and an empty list"
    (is (= (third '(0 1 2 3 4)) 2))
    (is (= (third '(0)) nil))))

(deftest balanced?-test
  (testing "balanced? with balanced, unbalanced, and empty strings"
    (is (= (balanced? "(abc(d)efg)") true))
    (is (= (balanced? "(ab)c(def)ghi(jk))") false))
    (is (= (balanced? "") true))))

(deftest return-dict-test
  (testing "return-dict"
    (is (= (return-dict "(+ 1 1)" nil {}) {:expressions "(+ 1 1)", :result nil, :environment {}}))))

(deftest ret-eval-test
  (testing "ret-eval. This will also test most of evaluate as evaluate is the main part of this function"
    (is (= (ret-eval "(+ 1 1)" nil {}) {:expressions "", :result 2, :environment {}}))
    (is (= (ret-eval "(* 3 1)" nil {}) {:expressions "", :result 3, :environment {}}))
    (is (= (ret-eval "(* 5 4 3 2 1)" nil {}) {:expressions "", :result 120, :environment {}}))
    (is (= (ret-eval "123" nil {}) {:expressions "", :result 123, :environment {}}))
    (is (= (ret-eval "(+ 1 (* 2 3) (* 4 2))" nil {}) {:expressions "", :result 15, :environment {}}))
    (is (= (ret-eval "(bind x 5)" nil {}) {:expressions "", :result 5, :environment {"x" 5}}))))

(deftest operation-test
  (testing "operation with + and *"
    (is (= (operation 0 + ["1" "2" "5" "7"] {}) 15))
    (is (= (operation 0 + ["x" "2" "5" "7"] {"x" 1}) 15))
    (is (= (operation 1 * ["1" "3" "5"] {}) 15))
    (is (= (operation 1 * ["x" "3" "5"] {"x" 1}) 15))))

(deftest find-open-close-parentheses-test
  (testing "find-open-close-parentheses"
    (is (= (find-open-close-parentheses "(abc(d)efg)") [4 6]))
    (is (= (find-open-close-parentheses "(ab)c(def)ghi(jk)") [0 3]))
    (is (nil? (find-open-close-parentheses "(ab")))))

(deftest get-value-test
  (testing "get-value"
    (is (= (get-value "hello" {"hello" 5}) 5))
    (is (thrown? Exception (get-value "hello" {"hellno" 5})))))

(deftest get-inside-expression-test
  (testing "get-value"
    (is (= (get-inside-expression "(abc(d)efg)") "(d)"))
    (is (= (get-inside-expression "(ab)c(def)ghi(jk)") "(ab)"))
    (is (thrown? Exception (get-inside-expression "(ab")))))

(deftest bind-test
  (testing "bind"
    (is (= (bind ["bind" "x" "2"] {}) {"x" 2}))
    (is (= (bind ["bind" "x" "2"] {"y" 3}) {"y" 3 "x" 2}))
    (is (= (bind ["bind" "x" "2"] {"x" 4}) {"x" 2}))))

(deftest evaluate--test
  (testing "Tests already done by ret-eval"
    (is true)))

(deftest evaluate-parentheses-test
  (testing "evaluate-parentheses. Most tests already done by evaluate except Exception"
    (is (= (evaluate-parentheses "(+ 1 1)" nil {}) {:expressions "", :result 2, :environment {}}))
    (is (= (evaluate-parentheses "(+ 1 (* 2 3) (* 4 2))" nil {}) {:expressions "", :result 15, :environment {}}))
    (is (thrown? Exception (evaluate-parentheses "(bind x 5" nil {})))))


(deftest make-balanced-test
  (testing "make-balanced."
    (is (= (make-balanced ["(+ 1 1)"])  ["(+ 1 1)"]))
    (is (= (make-balanced ["(+ 1 1" "1 2)"])  ["(+ 1 1 1 2)"]))))

(deftest separate-line-test
  (testing "separate-line."
    (is (= (separate-line "(+ 1 1) (+ 1 2)") '("(+ 1 1)" " (+ 1 2)")))
    (is (= (separate-line "(+ 1") '("(+ 1")))))

(deftest evaluate-all-test
  (testing "evaluate-all"
    (is (= (evaluate-all {} nil ["(bind x 11)" "(bind y 12)" "(* x y)"]) 132))))

(defn create-file-path
  [file-name]
  (str "/Users/shray/GitHub/psil-interpreter/test/psil_interpreter/" file-name))

(deftest read-psil-test
  (testing "read-psil. Testing all given examples from problem"
    (is (= (read-psil (create-file-path "Psil-test-file1.txt")) 123))
    (is (= (read-psil (create-file-path "Psil-test-file2.txt")) 3))
    (is (= (read-psil (create-file-path "Psil-test-file3.txt")) 12))
    (is (= (read-psil (create-file-path "Psil-test-file4.txt")) 120))
    (is (= (read-psil (create-file-path "Psil-test-file5.txt")) 15))
    (is (= (read-psil (create-file-path "Psil-test-file6.txt")) 12))
    (is (= (read-psil (create-file-path "Psil-test-file7.txt")) 100))
    (is (= (read-psil (create-file-path "Psil-test-file8.txt")) 100))
    (is (= (read-psil (create-file-path "Psil-test-file9.txt")) 121))
    (is (= (read-psil (create-file-path "Psil-test-file10.txt")) 21))
    (is (thrown? Exception (read-psil (create-file-path "Psil-test-file11.txt"))))
    (is (thrown? Exception (read-psil (create-file-path "Psil-test-file12.txt"))))
    (is (thrown? Exception (read-psil (create-file-path "Psil-test-file13.txt"))))
    (is (thrown? Exception (read-psil (create-file-path "Psil-test-file14.txt"))))
    (is (thrown? Exception (read-psil (create-file-path "Psil-test-file15.txt"))))))

(run-all-tests #"clojure.test.example")
