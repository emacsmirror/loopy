;;; loopy --- A looping library.
;; -*- lexical-binding: t -*-

;;; Commentary:
;; Loop has general construct:
;;
;; (loopy (NO UPDATES)
;;         (UPDATES)
;;         (RETURNS))
;;
;; - NO-UPDATES is things declared in parallel with WITH.
;; - UPDATES is things updated in the loop with FOR or an accumulation.
;;   UPDATES determines exist clauses.  When there's nothing left, eval the
;;   returns.
;; - RETURNS is what is returned.
;;


;;; Code:

(require 'cl-lib)

;; TODO: Prefer ((with a 1) (with b 2)) or (with (a 1) (b 2))?
(defun loopy--parse-with-forms (with-forms)
  "Remove the \"with\" and return list of forms in WITH-FORMS.
This list is substituted into a LET* binding."
  (cdr with-forms)
  ;; (let ((parsed))
  ;;   (dolist (form with-forms (nreverse parsed))
  ;;     (push (list (cl-second form) (cl-third form))
  ;;           parsed)))
  )

(defun loopy--parse-conditional-forms (wrapper condition forms &optional loop-name)
  "Parse FORMS, wrapping loop-body instructions in a WRAPPER with CONDITION.
Optionally needs LOOP-NAME for block returns.
Wrapped forms are things that would occur in the loop body, including returns."
  (let ((full-instructions)
        (sub-instructions (loopy--parse-body-forms forms loop-name))
        (loop-body))
    (dolist (instruction sub-instructions)
      (cl-case (car instruction)
        ('loop-body
         (push (cdr instruction) loop-body))
        (t (push instruction full-instructions))))
    (push `(loop-body . (,wrapper ,condition ,@loop-body)) full-instructions)
    full-instructions))

(defun loopy--parse-body-forms (forms &optional loop-name)
  "Get update and continue conditions from FORMS.
Optionally needs LOOP-NAME for block returns."
  (let ((instructions))
    (dolist (form forms)
      ;; Make a quick function, since we keeping missing the second argument of
      ;; `push'.
      (cl-flet ((add-instruction (instr) (push instr instructions)))
        (pcase form
          ;; A DO form for a generic lisp body. Not searched for special forms.
          (`(do . ,body)
           (add-instruction `(loop-body . (progn ,@body))))
          (`(expr ,var ,val)
           (add-instruction `(loop-body . (setq ,var ,val))))
          (`(list ,var ,val)
           (let ((val-holder (gensym)))
             (add-instruction `(value-holders . (,val-holder ,val)) )
             (add-instruction `(loop-body . (setq ,var (pop ,val-holder))))
             (add-instruction `(pre-conditions . ,val-holder))))
          (`(seq ,var ,val)
           ;; TODO: Destructively modify sequence or no?
           ;; Destructive version:
           (let ((val-holder (gensym)))
             (add-instruction `(value-holders . (,val-holder ,val)))
             (add-instruction
              `(loop-body . (setq ,var (seq-elt ,val-holder 0)
                                  ,val-holder (seq-drop ,val-holder 1))))
             (add-instruction `(pre-conditions . (not (seq-empty-p ,val-holder)))))
           ;; ;; Non-destructive version:
           ;; (let ((val-holder (gensym))
           ;;       (index-holder (gensym)))
           ;;   (add-instruction `(value-holders . (,val-holder ,val)))
           ;;   (add-instruction `(value-holders . (,index-holder 0)) )
           ;;   (add-instruction
           ;;    `(loop-body . (setq ,var (seq-elt ,val-holder ,index-holder)
           ;;                        ,index-holder (1+ ,index-holder))))
           ;;   (add-instruction `(pre-conditions . (= ,index-holder
           ;;                                          (length ,val-holder)))))
           )

          ;; A conditional WHEN form. Searched for special forms.
          (`(when ,cond . ,body)
           (mapc #'add-instruction
                 (loopy-parse-when-forms cond body loop-name)))
                 (loopy--parse-conditional-forms 'when cond body loop-name)))

          ;; Control Flow
          ((or '(skip) '(continue))
           (add-instruction '(loop-body . (go continue-tag))))

          ((or `(return) `(leave) `(break))
           (add-instruction `(loop-body . (cl-return-from ,loop-name))))

          ((or `(return-with ,val) `(leave-with ,val) `(break-with ,val))
           (add-instruction `(loop-body . (cl-return-from ,loop-name ,val))))

          (`(leave-named-loop ,named . ,val)
           (add-instruction `(loop-body . (cl-return-from ,named ,val))))

          ;; Accumulation
          (`(prepend ,var ,val)
           (add-instruction `(updates-initial . (,var nil)))
           (add-instruction `(loop-body . (push ,val ,var))))

          (_
           (error "Loopy: This form unkown: %s" form)))))
    instructions))


;; (cl-defmacro loopy (name-arg &key with for return)
(cl-defmacro loopy (&rest body)
  "A loop is something like the following form.  BODY is up to 4 arguments.

\(loopy loop-name
        ((with a 1)
         (with b 2))
        ((list i '(1 2 3 4 5 6 7))
         (do (message \"I is %d\" i)
             (message \"i+2 is %d\" (+ i 2)))
         (collect coll i)
         (when (= 6 i)
           (early-return)))
        (final-return coll))

Things to note:
- Return values are explicit.  If you want one, give one.
- Body clauses are of the form (CMD VAR VAL)."
  (let* (;; Vars to handle macro arguments.
         (name-arg)
         (with-args)
         (loop-body-args)
         (return-args)

         ;; -- Vars for processing loop clauses --
         (with-forms) ; WITH values and initial values
         ;; Holds lists, increment counters, and other values not given a name.
         (value-holders)
         ;; Explicitly named inits that will be updated in order.
         (updates-initial)
         ;; The loop is a while loop. Pre-conditions are things like whether a
         ;; temporary list is null.
         (pre-conditions)
         ;; The loop body is whether DO expressions and variable updating happen.
         ;; The order of evaluation is the same as the order that things were
         ;; fed to the macro.
         (loop-body)
         ;; Post-conditions are things that could cause the loop to exit after
         ;; an iteration, somewhat like a do-while loop.
         (post-conditions)
         ;; Returns should be explicit. There is only one return value, but it
         ;; can be a list of things, if a list of things is given in the
         ;; FINAL-RETURN clause.
         (final-return))

    ;; Check what was passed to the macro.
    (dolist (arg body)
      (cond
       ((symbolp arg)
        (setq name-arg arg))
       ((memq (car arg) '(finally-return final-return))
        (setq return-args (if (= (length (cdr arg))
                                 1)
                              (cadr arg)
                            (cdr arg))))
       ((memq (car arg) '(with let*))
        (setq with-args arg))
       (t ; Body forms have the most variety.
        (setq loop-body-args arg))))

    ;; Process clauses.
    (setq with-forms (loopy--parse-with-forms with-args))
    (setq final-return return-args)
    ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
    ;; Things added are expanded in place.
    (dolist (instruction (loopy--parse-body-forms loop-body-args name-arg))
      ;; Do it this way instead of with set, cause was getting errors about void
      ;; variables.
      (cl-case (car instruction)
        ('with-forms
         (push (cdr instruction) with-forms))
        ('value-holders
         (push (cdr instruction) value-holders))
        ('updates-initial
         (push (cdr instruction) updates-initial))
        ('pre-conditions
         (push (cdr instruction) pre-conditions))
        ('loop-body
         (push (cdr instruction) loop-body))
        ('post-conditions
         (push (cdr instruction) post-conditions))
        ('final-return
         (push (cdr instruction) final-return))
        (t
         (error "loopy: Unknown body instruction: %s" instruction))))

    ;; Add post condition checks if needed.
    (when post-conditions
      (push `(unless (and ,@post-conditions)
               (cl-return-from ,name-arg))
            loop-body))

    ;; (message "Pre-cond: %s" pre-conditions)
    `(let (,@(or (append value-holders updates-initial)
                 '((_))))
       (let* (,@(or with-forms '((_))))
         (cl-block ,name-arg
           (while ,(if pre-conditions
                       (cons 'and pre-conditions)
                     t)
             (cl-tagbody
              ;; Note: `push'-ing things into the instruction list in
              ;;       `loopy--parse-body-forms' and then reading them back and
              ;;       then pushing into `loop-body' counters out the flipped
              ;;       order normally caused by `push'.
              (progn ,@loop-body)
              continue-tag))
           (cl-return-from ,name-arg ,final-return))))))

(provide 'loopy)
;;; loopy.el ends here
