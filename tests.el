(require 'ert)

(load-file "./loopy.el")
(require 'loopy)


(loopy ((with a 3)
        (with b 7))
       ((from-list i '(1 2 3 4 5 6 7))
        (from-expr j (+ 1 1)))
       ((collect (+ i b))
        (collect (+ j a))))

(loopy--parse-with-forms '((with a 3) (with b 4)))

(loopy--parse-with-forms nil)

(let )
(dolist (i nil))

(loopy ((with a 3)
        (with b 6))
       ((list i '(1 2 3))
        (expr j (1+ i))
        (collect c (list a b j)))
       (return c))


(loopy--parse-body-forms '((list x '(1 2 3))))

(loopy--parse-body-forms '((do (message "cat")
                              (+ 1 2))))

(loopy-parse-when-forms '(= i 1)
                        '((list i '(1 2 3))))

(loopy--parse-body-forms nil '((when (= i 1)
                                 (list i '(1 2 3))
                                 (list b (number sequence 1 10)))))


(loopy--parse-body-forms '((return-with 3)))

(loopy--parse-body-forms '((seq i '(1 2 3))))

((pre-conditions . g937) (loop-body setq i (seq-elt g937 g938)
                                    g937 (seq-drop g937 1) g938 (1+ g938)) (value-holders g938
                                    0) (value-holders g937 '(1 2 3)))

(loopy--parse-body-forms '((when (= i 1)
                             (do (1+ 2))
                             (return-with 3)))
                         'cat)

(loopy ((seq i '(1 2 3))
        (do (message "I %s" i)))
       (finally-return i))

(loopy my-loop
       ((with something1 1)
        (with something2 2))
       ((list el (number-sequence 1 10)))
       (finally-return el))

(dolist (i '(1 2 3 4))
  (cl-tagbody
   (when (cl-evenp i) (go bye-msg))
   hi-msg
   (message "hi")
   bye-msg
   ;; (message "by")
   ))

(let ((prec ;; '(a b c)
       ))
  `(and ,@(or prec t)))


(loopy nil
       ((with something 3))
       ((list i (number-sequence 1 10))
        (when (cl-evep i)
          (return i)))
       ;; (finally-return 23)
       )

(loopy ((list i '(1 2 3))
        (list j '(4 5 6))
        (expr b i))
       (final-return b))

(loopy ((with j 0))
       ((do (cl-incf j))
        (when (> j 5)
          (return-with j))))


;;; Leaving, Returning, Skipping
(ert-deftest mod-when-test ()
  "Check WHEN processing."
  (should (equal (loopy ((seq i (number-sequence 1 20))
                         (when (zerop (mod i 10))
                           (skip))
                         (when (cl-evenp i)
                           (prepend my-collection i)))
                        (finally-return (nreverse my-collection)))
                 '(2 4 6 8 12 14 16 18))))

(ert-deftest leave-named ()
  (should (= 6
             (loopy outer
                    ((list i (number-sequence 1 10))
                     (when (> i 5)
                       (leave-named-loop outer i)))))))
(ert-deftest leave-outer-named ()
  (should (eq 6
              (loopy
               outer
               ((list outer-i (number-sequence 1 10))
                (expr ret-loop
                      (loopy inner
                             ((expr inner-sum (+ outer-i 10))
                              (when (> inner-sum 15)
                                (leave-named-loop outer outer-i))
                              ;; Note: Without explicit return, inner loop is
                              ;; infinite.
                              (return)))))))))

(loopy ((seq i (number-sequence 1 20))
        (when (zerop (mod i 10))
          (skip))
        (when (cl-evenp i)
          (prepend my-collection i)))
       (finally-return (nreverse my-collection)))

;;; Conditonals
(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (loopy (with (first-var 2)
                 (second-var 3))
           ((seq el [1 2 3 4 5 6 7])
            ;; Could also use (do (cond ...)).
            (when (zerop (mod el first-var))
              (prepend msg-coll (message "Multiple of 2: %d" el)))
            (when (zerop (mod el second-var))
              (prepend msg-coll (message "Multiple of 3: %d" el))))
           (finally-return (string-join (nreverse msg-coll) "\n")))
    "Multiple of 2: 2
Multiple of 3: 3
Multiple of 2: 4
Multiple of 2: 6
Multiple of 3: 6")))

(ert-deftest recursive-when-test ()
  (should (equal
           (loopy ((list i (number-sequence 1 10))
                   (list j '(1 2 3 6 7 8))
                   (when (cl-evenp i)
                     (when (> j i)
                       (do (message "J > I"))
                       (return (cons j i))))))
           '(6 . 4))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (loopy (with (first-var 2)
                 (second-var 3))
           ((seq el [1 2 3 4 5 6 7])
            ;; Could also use (do (cond ...)).
            ()
            (when (zerop (mod el first-var))
              (prepend msg-coll (message "Multiple of 2: %d" el)))
            (when (zerop (mod el second-var))
              (prepend msg-coll (message "Multiple of 3: %d" el))))
           (finally-return (string-join (nreverse msg-coll) "\n")))
    "Multiple of 2: 2
Multiple of 3: 3
Multiple of 2: 4
Multiple of 2: 6
Multiple of 3: 6")))

;;;; Unless
(ert-deftest multi-unless-prepend-test ()
  (should
   (string=
    (loopy (with (first-var 2)
                 (second-var 3))
           ((seq el [1 2 3 4 5 6 7])
            ;; Could also use (do (cond ...)).
            (unless (zerop (mod el first-var))
              (prepend msg-coll (message "Not multiple of 2: %d" el)))
            (unless (zerop (mod el second-var))
              (prepend msg-coll (message "Not multiple of 3: %d" el))))
           (finally-return (string-join (nreverse msg-coll) "\n")))
    "Not multiple of 2: 1
Not multiple of 3: 1
Not multiple of 3: 2
Not multiple of 2: 3
Not multiple of 3: 4
Not multiple of 2: 5
Not multiple of 3: 5
Not multiple of 2: 7
Not multiple of 3: 7")))

;;;; Cond FORMS
(ert-deftest parse-cond-form ()
  (should (equal (loopy--parse-cond-forms '(((= a 1)
                                             (do (message "hi")))
                                            ((= b 2)
                                             (return 5))))
                 '((loop-body cond
                              ((= a 1) (progn (message "hi")))
                              ((= b 2) (cl-return-from nil 5)))))))

(ert-deftest parse-cond-loop ()
  (should (equal (loopy ((list i (number-sequence 1 10))
                         (cond
                          ((cl-evenp i)
                           (prepend evens i))
                          (t (prepend odds i))))
                        (finally-return (list evens odds)))
                 '((10 8 6 4 2) (9 7 5 3 1)))))
