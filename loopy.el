;;; loopy --- A looping library.
;; -*- lexical-binding: t -*-

;;; Commentary:
;; Loop has general construct:
;;


;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)

;;;; Important Variables
;; These are only ever set locally.

(defvar loopy--with-forms nil
  "With Forms are variables explicitly created using the `with' keyword.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let*' binding.
They are created by passing (with (VAR1 VAL1) (VAR2 VAL2) ...) to `loopy'.")

(defvar loopy--value-holders nil
  "Value Holders are implicit variables created automatically by loop commands.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let' binding.

For example, using (list i '(1 2 3)) will create a value holder
containing '(1 2 3).  This makes iteration easier.")

(defvar loopy--updates-initial nil
  "Explicitly named variables and their first values, updated in order.

This is useful for lexically scoping variables, and for declaring
an initial value before a different value assigned in the loop.")

(defvar loopy--before-do nil
  "A list of expressions to evaluate before the loop starts.
This is done using a `progn'.")

(defvar loopy--pre-conditions nil
  "The list of expressions that determine whether the `while' loop starts/loops.
These are fed to an `and', so all conditions must be true for the
  `while' to start/loop.")

(defvar loopy--loop-body nil
  "A list of expressions to run (in order) inside the loop.
These expressions are created by parsing the loop commands passed to `loopy'.

For example, the command (list i my-list) effectively puts
\(setq i (pop my-list)) into the loop body.  Most commands require some setup,
and so don't affect only the loop body.")

(defvar loopy--post-conditions nil
  "Post-conditions that could cause the loop to exit evaluating the loop body.

All expressions in the list must be true for the program to continue.
This is similar to `do-while' in other languages.")

(defvar loopy--after-do nil
  "Expressions to run (in order) after the loop successfully completes.
These run in a `progn'.")

(defvar loopy--final-do nil
  "A list of expressions always run (in order) after the loop finishes/exits.")

(defvar loopy--final-return nil
  "What the macro finally returns.  This overrides any early return value.")

;;;; Miscellaneous Functions
(defun loopy--bound-p (var-name)
  "Check if VAR-NAME (a symbol) is already bound for the macro.

This can happen when multiple loop commands refer to the same
variable, or when a variable is introduced via `with'.

The variable can exist in `loopy--with-forms' or `loopy--updates-initial'."
  (or (memq var-name (mapcar #'car loopy--with-forms))
      (memq var-name (mapcar #'car loopy--updates-initial))
      ;; (memq var-name (mapcar #'cadr loopy--value-holders))
      ))

;;;; Included parsing functions.

(defun loopy--parse-with-forms (with-forms)
  "Remove the \"with\" and return list of forms in WITH-FORMS.
This list is substituted into a LET* binding."
  (cdr with-forms))

(defun loopy--parse-conditional-forms (wrapper condition forms &optional loop-name)
  "Parse FORMS, wrapping `loopy--loop-body' expressions in a conditional form.
The instructions (e.g., return expressions) are wrapped with a
WRAPPER with CONDITION.  Optionally needs LOOP-NAME for block
returns."
  (let ((full-instructions)
        (sub-instructions (loopy--parse-body-forms forms loop-name))
        (loop-body))
    (dolist (instruction sub-instructions)
      (cl-case (car instruction)
        ('loopy--loop-body
         (push (cdr instruction) loop-body))
        (t (push instruction full-instructions))))
    (push `(loopy--loop-body . (,wrapper ,condition ,@loop-body)) full-instructions)
    full-instructions))

(defun loopy--parse-cond-form (forms &optional loop-name)
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
            ('loopy--loop-body
             (push (cdr instruction) instructions-for-wrapping))
            (t (push instruction full-instructions))))
        (push (cons condition instructions-for-wrapping)
              cond-body)))
    (push `(loopy--loop-body . ,(cons 'cond (nreverse cond-body))) full-instructions)
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
           (add-instruction `(loopy--loop-body . (progn ,@body))))
          (`(expr ,var ,val)
           (add-instruction `(loopy--loop-body . (setq ,var ,val)))
           ;; Want to make sure VAR is lexically bound.
           (add-instruction `(loopy--updates-initial . (,var nil))))

;;;;; Iteration Clauses
          (`(list ,var ,val)
           (let ((val-holder (gensym)))
             (add-instruction `(loopy--value-holders . (,val-holder ,val)) )
             (add-instruction `(loopy--updates-initial . (,var nil)))
             (add-instruction `(loopy--loop-body . (setq ,var (pop ,val-holder))))
             (add-instruction `(loopy--pre-conditions . ,val-holder))))
          (`(seq ,var ,val)
           ;; TODO: Destructively modify sequence or no?
           ;; Destructive version:
           (let ((val-holder (gensym)))
             (add-instruction `(loopy--value-holders . (,val-holder ,val)))
             (add-instruction `(loopy--updates-initial . (,var nil)))
             (add-instruction
              `(loopy--loop-body . (setq ,var (seq-elt ,val-holder 0)
                                         ,val-holder (seq-drop ,val-holder 1))))
             (add-instruction `(loopy--pre-conditions . (not (seq-empty-p ,val-holder)))))
           ;; ;; Non-destructive version:
           ;; (let ((val-holder (gensym))
           ;;       (index-holder (gensym)))
           ;;   (add-instruction `(loopy--value-holders . (,val-holder ,val)))
           ;;   (add-instruction `(loopy--value-holders . (,index-holder 0)) )
           ;;   (add-instruction
           ;;    `(loopy--loop-body . (setq ,var (seq-elt ,val-holder ,index-holder)
           ;;                        ,index-holder (1+ ,index-holder))))
           ;;   (add-instruction `(loopy--pre-conditions . (= ,index-holder
           ;;                                          (length ,val-holder)))))
           )
          (`(repeat ,count)
           (let ((val-holder (gensym)))
             (add-instruction `(loopy--value-holders . (,val-holder 0)))
             (add-instruction `(loopy--loop-body . (cl-incf ,val-holder)))
             (add-instruction `(loopy--pre-conditions . (< ,val-holder ,count)))))

          (`(repeat ,var ,count)
           (add-instruction `(loopy--value-holders . (,var 0)))
           (add-instruction `(loopy--loop-body . (cl-incf ,var)))
           (add-instruction `(loopy--pre-conditions . (< ,var ,count))))

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
                 (loopy--parse-cond-form body loop-name)))

;;;;; Exit and Return Clauses
          ((or '(skip) '(continue))
           (add-instruction '(loopy--loop-body . (go loopy--continue-tag))))

          (`(return ,val)
           (add-instruction `(loopy--loop-body . (cl-return-from ,loop-name ,val))))

          (`(return-from ,name ,val)
           (add-instruction `(loopy--loop-body . (cl-return-from ,name ,val))))

          ((or '(leave) '(break))
           (add-instruction `(loopy--loop-body . (cl-return-from ,loop-name nil))))

          ((or `(leave-from ,name) `(break-from ,name))
           (add-instruction `(loopy--loop-body . (cl-return-from ,name nil))))


;;;;; Accumulation Clauses
          ((or `(prepend ,var ,val) `(push ,var ,val) `(push-into ,var ,val))
           (add-instruction `(loopy--updates-initial . (,var nil)))
           (add-instruction `(loopy--loop-body . (push ,val ,var))))
          (`(collect ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var nil)))
           (add-instruction `(loopy--loop-body . (setq ,var (append ,var
                                                                    (list ,val))))))
          (`(append ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var nil)))
           (add-instruction `(loopy--loop-body . (setq ,var (append ,var ,val)))))
          (`(concat ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var nil)))
           (add-instruction `(loopy--loop-body . (setq ,var (concat ,var ,val)))))
          (`(vconcat ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var nil)))
           (add-instruction `(loopy--loop-body . (setq ,var (vconcat ,var ,val)))))
          (`(count ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var 0)))
           (add-instruction `(loopy--loop-body . (when ,val (cl-incf ,var)))))
          (`(sum ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var 0)))
           (add-instruction `(loopy--loop-body . (setq ,var (+ ,var ,val)))))
          ((or `(max ,var ,val) `(maximize ,var ,val))
           (add-instruction `(loopy--updates-initial . (,var -1.0e+INF)))
           (add-instruction `(loopy--loop-body . (setq ,var (max ,var ,val)))))
          ((or `(min ,var ,val) `(minimize ,var ,val))
           (add-instruction `(loopy--updates-initial . (,var 1.0e+INF)))
           (add-instruction `(loopy--loop-body . (setq ,var (min ,var ,val)))))
          (`(nconc ,var ,val)
           (add-instruction `(loopy--updates-initial . (,var nil)))
           (add-instruction `(loopy--loop-body . (setq ,var (nconc ,var ,val)))))
          (_
           (error "Loopy: This form unkown: %s" form)))))
    instructions))

;;;; The Macro Itself
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
  (declare (debug (&optional ;; TODO: Is this correct?
                   ([&or "with" "let*"] &rest (symbolp &optional form))
                   ([&or "before-do" "before-progn" "before"] body)
                   ([&optional "loop"]
                    &rest [&or (symbolp &optional symbolp form) ; Covers most.
                               ([&or "when" "if" "unless"] form body)
                               ("cond" &rest (body))])
                   ([&or "after-do" "after-progn" "after"] body)
                   ([&or "finally-do" "finally-progn"] body)
                   ([&or "finally-return" "return"] form &optional form))))
  (let (;; -- Top-level expressions other than loop body --
        (loopy--name-arg)
        (loopy--with-forms)
        (loopy--before-do)
        (loopy--after-do)
        (loopy--final-do)
        (loopy--final-return)

        ;; -- Vars for processing loop clauses --
        (loopy--value-holders)
        (loopy--updates-initial)
        (loopy--pre-conditions)
        (loopy--loop-body)
        (loopy--post-conditions))

;;;;; Interpreting the macro arguments.
    ;; Check what was passed to the macro.
    (dolist (arg body)
      (cond
       ((symbolp arg)
        (setq loopy--name-arg arg))
       ((memq (car-safe arg) '(with let*))
        (setq loopy--with-forms (loopy--parse-with-forms arg)))
       ((memq (car-safe arg) '(before-do before-progn before))
        (setq loopy--before-do (cdr arg)))
       ((memq (car-safe arg) '(finally-return return))
        (setq loopy--final-return
              (if (= 1 (length (cdr arg)))
                  (cadr arg)
                (cons 'list (cdr arg)))))
       ((memq (car-safe arg) '(after-do after-progn after))
        (setq loopy--after-do (cdr arg)))
       ((memq (car-safe arg) '(finally-do finally-progn))
        (setq loopy--final-do (cdr arg)))
       (t
        ;; Body forms have the most variety.
        ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
        ;; Things added are expanded in place.
        (dolist (instruction (loopy--parse-body-forms (if (eq (car-safe arg) 'loop)
                                                          (cdr arg)
                                                        arg)
                                                      loopy--name-arg))
          ;; Do it this way instead of with `set', cause was getting errors
          ;; about void variables.
          (cl-case (car instruction)
            ('loopy--value-holders
             ;; Don't wont to accidentally rebind variables to `nil'.
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) loopy--value-holders)))
            ('loopy--updates-initial
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) loopy--updates-initial)))
            ('loopy--pre-conditions
             (push (cdr instruction) loopy--pre-conditions))
            ('loopy--loop-body
             (push (cdr instruction) loopy--loop-body))
            ('loopy--post-conditions
             (push (cdr instruction) loopy--post-conditions))
            (t
             (error "Loopy: Unknown body instruction: %s" instruction)))))))

    ;; Add post condition checks if needed.
    (when loopy--post-conditions
      (push `(unless (and ,@loopy--post-conditions)
               (cl-return-from ,loopy--name-arg))
            loopy--loop-body))

;;;;; Creating the returned code.
    ;; Note: `let'/`let*' will signal an error if we accidentally substitute
    ;;       `nil' as the variable declaration, since it will assume we are
    ;;       trying to redefine a constant.  To avoid that, we just bind `_' to
    ;;       `nil', which is used (at least in `pcase') as a throw-away symbol.
    `(let* (,@(or loopy--with-forms '((_))))
       (let (,@(or (append loopy--value-holders loopy--updates-initial)
                   '((_))))
         ;; If we need to, capture early return, those that has less
         ;; priority than a final return.
         (let ((loopy--early-return-capture
                (cl-block ,loopy--name-arg
                  ,@loopy--before-do
                  (while ,(if loopy--pre-conditions
                              (cons 'and loopy--pre-conditions)
                            t)
                    (cl-tagbody
                     ;; Note: `push'-ing things into the instruction list in
                     ;;       `loopy--parse-body-forms' and then reading them
                     ;;       back and then pushing into `loopy--loop-body'
                     ;;       counters out the flipped order normally caused by
                     ;;       `push'.
                     ,@loopy--loop-body
                     loopy--continue-tag))
                  ,@loopy--after-do
                  ;; We don't want anything in `loopy--after-do' accidentally
                  ;; giving us a return value, so we explicitly return nil.
                  ;;
                  ;; TODO: Is this actually needed?
                  nil)))
           ,@loopy--final-do
           ,(if loopy--final-return
                loopy--final-return
              'loopy--early-return-capture))))))

(provide 'loopy)
;;; loopy.el ends here
