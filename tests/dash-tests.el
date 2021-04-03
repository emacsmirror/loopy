;; Run these tests from project dir using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit


(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'dash "./dependecy-links/dash.el")
(require 'loopy "./loopy.el")
(require 'loopy-dash "./loopy-dash.el")

(load-file "tests/tests.el")

(setq loopy-default-flags '(dash))

(ert-deftest dash-flag-default ()
  (should (equal '(5 6)
                 (let ((loopy-default-flags '(dash)))
                   (eval (quote (loopy (list (&plist :a a  :b b)
                                             '((:a 3  :b 4) (:a 5 :b 6)))
                                       (finally-return a b))))))))

(ert-deftest dash-flag-default-disable ()
  :expected-result :failed
  (should (equal '(5 6)
                 (let ((loopy-default-flags '(dash)))
                   (eval (quote (loopy (flag -dash)
                                       (list (&plist :a a  :b b)
                                             '((:a 3  :b 4) (:a 5 :b 6)))
                                       (finally-return a b))))))))

(ert-deftest dash-flag-enable-disable ()
  :expected-result :failed
  (should (equal '(5 6)
                 (eval (quote (loopy (flag dash -dash)
                                     (list (&plist :a a  :b b)
                                           '((:a 3  :b 4) (:a 5 :b 6)))
                                     (finally-return a b)))))))


(ert-deftest dash-with-destructuring ()
  (should (= 7 (eval (quote (loopy (flag dash)
                                   (with ((&plist :a a  :b b) '(:a 3 :b 4)))
                                   (repeat 1)
                                   (return (+ a b))))))))
