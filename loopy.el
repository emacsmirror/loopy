;;; loopy --- A looping library.
;; -*- lexical-binding: t -*-

;;; Commentary:
;; Loop has general construct:
;;


;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)

;;;; Included parsing functions.

(defun loopy--parse-with-forms (with-forms)
  "Remove the \"with\" and return list of forms in WITH-FORMS.
This list is substituted into a LET* binding."
  (cdr with-forms))

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

(defun loopy--parse-cond-forms (forms &optional loop-name)
  "Parse FORMS where the `car' is a condition.  Use COND forms for IF-ELSE.
Optionally needs LOOP-NAME for block returns.
Wrapped forms are things that would occur in the loop body, including returns.

This takes the `cdr' of the COND form (i.e., doesn't start with \"cond\")."
  (let ((full-instructions)
        (cond-body))
    (dolist (cond-and-body forms)
      (let ((condition (car cond-and-body))
            (sub-instructions (loopy--parse-body-forms (cdr cond-and-body)
                                                       loop-name))
            (instructions-for-wrapping))
        (dolist (instruction sub-instructions)
          (cl-case (car instruction)
            ('loop-body
             (push (cdr instruction) instructions-for-wrapping))
            (t (push instruction full-instructions))))
        (push (cons condition instructions-for-wrapping)
              cond-body)))
    (push `(loop-body . ,(cons 'cond (nreverse cond-body))) full-instructions)
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
;;;;; Generic body clauses
          ;; A DO form for a generic lisp body. Not searched for special forms.
          (`(do . ,body)
           (add-instruction `(loop-body . (progn ,@body))))
          (`(expr ,var ,val)
           (add-instruction `(loop-body . (setq ,var ,val))))

;;;;; Iteration Clauses
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

;;;;; Conditional Body Forms
          ;; Since these can contain other commands/clauses, it's easier if they
          ;; have their own parsing functions, which call back into this one to
          ;; parse sub-clauses.
          (`(when ,cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-conditional-forms 'when cond body loop-name)))

          (`(unless ,cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-conditional-forms 'unless cond body loop-name)))

          (`(if ,cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-conditional-forms 'if cond body loop-name)))

          (`(cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-cond-forms body loop-name)))

;;;;; Exit and Return Clauses
          ((or '(skip) '(continue))
           (add-instruction '(loop-body . (go continue-tag))))

          ((or `(return . ,rest) `(leave . ,rest) `(break . ,rest))
           (add-instruction `(loop-body . (cl-return-from ,loop-name ,
                                            (car rest)))))

          ((or `(return-with ,val) `(leave-with ,val) `(break-with ,val))
           (add-instruction `(loop-body . (cl-return-from ,loop-name ,val))))

          (`(leave-named-loop ,named . ,val)
           (add-instruction `(loop-body . (cl-return-from ,named ,(car val)))))

;;;;; Accumulation Clauses
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
  (let* ((name-arg) ; Name of loop.  Used for early return.

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
         ;; One might wish to perform some final processing after the loop is
         ;; completed, but before a value is returned.
         (final-do)
         ;; Returns should be explicit. There is only one return value, but it
         ;; can be a list of things, if a list of things is given in the
         ;; FINAL-RETURN clause.
         (final-return))

    ;; Check what was passed to the macro.
    (dolist (arg body)
      (cond
       ((symbolp arg)
        (setq name-arg arg))
       ((memq (car arg) '(finally-return final-return return))
        (setq final-return
              (if (= 1 (length (cdr arg)))
                  (cadr arg)
                (cons 'list (cdr arg)))))
       ((memq (car arg) '( finally-do final-do do
                           finally-progn final-progn progn))
        (setq final-do (cdr arg)))
       ((memq (car arg) '(with let*))
        (setq with-forms arg))
       (t
        ;; Body forms have the most variety.
        ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
        ;; Things added are expanded in place.
        (dolist (instruction (loopy--parse-body-forms arg name-arg))
          ;; Do it this way instead of with `set', cause was getting errors
          ;; about void variables.
          (cl-case (car instruction)
            ;; ('with-forms
            ;;  (push (cdr instruction) with-forms))
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
            ;; This shouldn't be affected by the body clauses.
            ;; ('final-return
            ;;  (push (cdr instruction) final-return))
            (t
             (error "loopy: Unknown body instruction: %s" instruction)))))))

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
           ,(when final-do
              (cons 'progn final-do))
           ,(when final-return
              (list 'cl-return-from name-arg final-return)))))))

(provide 'loopy)
;;; loopy.el ends here
