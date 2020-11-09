# Loopy: An Emacs Looping Library

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Loopy: An Emacs Looping Library](#loopy-an-emacs-looping-library)
    - [How does it compare to other approaches?](#how-does-it-compare-to-other-approaches)
    - [How to use](#how-to-use)
        - [Loop Body Commands](#loop-body-commands)
            - [Commands for Generic Evaluation](#commands-for-generic-evaluation)
            - [Iteration and Looping Commands](#iteration-and-looping-commands)
            - [Accumulation Commands](#accumulation-commands)
            - [Control Flow](#control-flow)
                - [Conditionals](#conditionals)
                - [Skipping an Iteration](#skipping-an-iteration)
                - [Exiting the Loop Early](#exiting-the-loop-early)
    - [Extending with Personal Loop Commands](#extending-with-personal-loop-commands)
        - [Background Explanation](#background-explanation)
        - [A Small Example](#a-small-example)
    - [Translating from `cl-loop`](#translating-from-cl-loop)
        - [For Clauses](#for-clauses)
        - [Iteration Clauses](#iteration-clauses)
        - [Accumulation Clauses](#accumulation-clauses)

<!-- markdown-toc end -->

`loopy` is a macro meant for iterating and looping.  It is similar in usage to
[`cl-loop`][cl-loop] but uses symbolic expressions rather than keywords.

It should be comparable with `cl-loop` for basic things, keeping in mind the
following points:
- It is probably less efficient than `cl-loop`, though I am so far trying to
  keep the same logic that `cl-loop` uses.
- It has more flexible control flow commands, under which you can easily group
  several commands, including assignments. This is my main motivator for having
  this macro.
- It has a `skip` command to skip to skip the rest of the loop body and
  immediately start the next iteration (which I don't think `cl-loop` can
  do).  Of course, grouping the rest under a control-flow command, e.g., `when`,
  can achieve the same affect.
- Using an accumulation command does not imply a return value.  Return values
  must be explicitly stated.  The default return value is `nil`.

`loopy` is not a replacement for `cl-loop` in all cases, especially when there
is already a convenient way of doing things between the `cl-lib` and `seq`
libraries and Emacs's regular functions.  Loopy is, however, easy to use and
extend, and performs well in the cases that it already handles.

The `loopy` macro has several possible arguments, each beginning with a keyword.
- `with` declares variables that are bound in order before and around the loop,
  like in a `let*` binding.
- `before-do` is a list of normal Lisp expressions to run before the loop executes.
- `loop` is a list of special commands that create the loop body.  These commands
  are described in detail in the section [How to Use](#how-to-use).
- `after-do` is a list of normal Lisp expressions to run after the successful
  completion of the loop.
- `finally-do` is a list of normal Lisp expressions that always run, regardless
  of whether an early return was triggered in the loop body.
- `finally-return` is an expression whose value is always returned, regardless
  of whether an early return was triggered in the loop body.

The loop commands generally follow the form `(COMMAND VARIABLE-NAME &rest ARGS)`.
For example,
- To iterate through a [sequence][sequence-docs], use `(seq elem [1 2 3])`
  (for efficiency, there are also more specific commands, like `list`).
- To collect values into a list, use `(collect my-collection collected-elem)`.
- To just bind a variable to the result of a Lisp expression,
  use `(expr my-var (my-func))`

Below is a full example of the arguments of the `loopy` macro.  The top-level
forms have a flexible-order, but meaning is clearest if they have the following
order.  All of the arguments are technically optional, but having a loop without
a body wouldn't be very useful.

``` elisp
;; (loopy NAME
;;        (with WITH-ARGS)
;;        (before-do EXPR [EXPR …])
;;        (loop BODY-COMMAND [BODY-COMMAND …])
;;        (after-do EXPR [EXPR …])
;;        (finally-do EXPR [EXPR …])
;;        (finally-return VAL [VAL …]))

;; Returns: '((2 4 6 8 10) (1 3 5 7 9))
(loopy my-loop
       (with (success-p nil))
       (before-do (message "Beginning loop ..."))
       (loop (list i (number-sequence 1 10))
             (do (message "Checking number: %d" i))
             (if (cl-evenp i)
                 (collect found-evens i)
               (collect found-odds i)))
       (after-do (message "Loop completed successfully.")
                 (setq success-p t))
       (finally-do (if success-p
                       (message "Confirmed success reported.")
                     (message "W: Success not confirmed!"))
                   (message "Found evens: %s" found-evens)
                   (message "Found odds: %s" found-odds))
       (finally-return (list found-evens found-odds)))
```

Loopy is not yet feature complete.  Here are some things that would be nice:
- Iteration clauses that are supported by `cl-loop`.  `seq` can iterate
  through sequences, but `cl-loop` does more.
  - A few are unneeded, such as the `for` statements that can instead be done
    using `number-sequence`.
- De-structuring can be useful, but this can be done using repeated `expr`
  commands.

## How does it compare to other approaches?

Loopy is similar in use to other looping libraries, except for its current lack
of convenience features and extensibility.

Below is a simple example of `loopy` vs `cl-loop`.

``` elisp
(require 'cl-lib)
(cl-loop with some-thing = 5
         for i from 1 to 100
         do (message "I is %s" i)
         when (> (+ i 5) 20)
         return (format "Done: %d" i))

(require 'loopy)
(loopy (with (some-thing 5))
       ((list i (number-sequence 1 100))
        (do (message "I is %s" i))
        (when (> (+ i 5) 20)
          (return (format "Done: %d" i)))))
```

The main benefit (I believe) of Loopy is clearer grouping of constructs under
conditionals while still using a clean syntax, such as in the below example.

``` elisp
(loopy ((list i (number-sequence 1 20))
        (when (cl-evenp i)
          (expr once i)
          (expr twice (* 2 i))
          (prepend together (cons once twice))))
       (finally-return (nreverse together)))
```

In my experience, `cl-loop` does not allow the easy grouping of assignment
statements under a `when` condition.  For example, below is something I would
like to try to do with `cl-loop`.

I am aware that in this example the `for` statements aren't necessary and that
the `collect` statements would be sufficient, but (when I come across things
like this in my work) I would like to use them to declare variables for
readability purposes.

``` elisp
(require 'cl-lib)
(save-match-data
  (cl-loop with pattern = "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)"
           for line in (split-string "Line1-Data1\nBad\nLine2-Data2")
           when (string-match pattern line)
           for line-num = (concat "L" (match-string 1 line))
           and for data-nums = (concat "D" (match-string 2 line))
           and collect (match-string 1 line) into line-nums
           and collect (match-string 2 line) into data-nums
           finally return (list line-nums data-nums)))

;; Normal Elisp:
(save-match-data
  (let ((pattern "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)")
        (line-nums)
        (data-nums))
    (dolist (line (split-string "Line1-Data1\nBad\nLine2-Data2"))
      (when (string-match pattern line)
        (let ((line-num (concat "L" (match-string 1 line)))
              (datum-num (concat "D" (match-string 2 line))))
          (push line-num line-nums)
          (push datum-num data-nums))))
    (list (nreverse line-nums) (nreverse data-nums))))
```

Here is how one could (currently) do it with `loopy`.

``` elisp
(require 'loopy)
(save-match-data
  (loopy (with (pattern "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)"))
         ((list line (split-string "Line1-Data1\nBad\nLine2-Data2"))
          (when (string-match pattern line)
            (expr line-num (concat "L" (match-string 1 line)))
            (expr datum-num (concat "D" (match-string 2 line)))

            ;; … Further processing now that data is named …

            (collect line-nums line-num)
            (collect data-nums datum-num)))
         (finally-return line-nums data-nums)))

;; The `loopy' expression currently expands to the below.
(let ((g760 (split-string "Line1-Data1\nBad\nLine2-Data2"))
      (line nil)
      (line-num nil)
      (datum-num nil)
      (line-nums nil)
      (data-nums nil))
  (let* ((pattern "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)")
         (loopy--early-return-capture
          (cl-block nil
            (while (and g760)
              (cl-tagbody
               (setq line (pop g760))
               (when (string-match pattern line)
                 (setq line-num  (concat "L" (match-string 1 line)))
                 (setq datum-num (concat "D" (match-string 2 line)))
                 (setq line-nums (append line-nums (list line-num)))
                 (setq data-nums (append data-nums (list datum-num))))
               loopy--continue-tag))
            nil)))
    (list line-nums data-nums)))
```

This expansion is probably less efficient than what `cl-loop` does.

The expansion shows that there is room for improvement, but it's a nice start,
is easy to read, and does what I want.  I believe that the value of the macro
increases for longer loop bodies with several conditional commands.

Another nice ability, one that I'm not sure `cl-loop` has, is
skipping/continuing a loop iteration.

``` elisp
;; Returns even numbers that aren't multiples of 10.
(loopy ((list i (number-sequence 1 20))
        (when (zerop (mod i 10))
          (skip))
        (when (cl-evenp i)
          (prepend my-collection i)))
       (finally-return (nreverse my-collection)))

;; Expands to:
(let ((g1069 (number-sequence 1 20))
      (i nil)
      (my-collection nil))
  (let* ((_))
    (cl-block nil
      (while (and g1069)
        (cl-tagbody
         (progn
           (setq i (pop g1069))
           (when (zerop (mod i 10))
             (go continue-tag))
           (when (cl-evenp i)
             (push i my-collection)))
         continue-tag))
      nil
      (cl-return-from nil (nreverse my-collection)))))
```

A real-world example is a version the `selectrum-outline` command from the [Selectrum
wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands#jumping-to-outline-headings).
The command checks each line of text in the buffer against a chosen regular
expression, a builds a list of completion candidates matching that
expression.  It needs to find and format a candidate (including keeping track of
the preceding higher-level headings) and pick a default candidate in one pass.

Here is a version using the normal features of Elisp:

``` elisp
(defun selectrum-outline ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  ;; Signal a `user-error' if we don't have a regexp for this major mode.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (let ((selectrum-should-sort-p nil) ; Headings should stay in order of appearance.
            ;; Get the basic information of each heading in the accessible
            ;; portion of the buffer.
            (buffer-lines (split-string (buffer-string) "\n"))
            (line-number 0)
            (line-number-format)

            ;; Finding the default heading
            (default-heading)
            (current-line-number (line-number-at-pos (point)))

            ;; Keeping track of the tree.
            (backwards-prefix-list)
            (prev-heading-text)
            (prev-heading-level)

            ;; Backwards result of the `dolist'.  Will `nreverse'.
            (formatted-headings))

        (setq line-number-format
              (concat "L%0"
                      (number-to-string
                       (length (number-to-string (length buffer-lines))))
                      "d: "))

        (save-match-data
          (dolist (text-line buffer-lines)
            ;; Increment line number when moving to next.
            (cl-incf line-number)
            (when (string-match heading-regexp text-line)
              (let ((heading-text (match-string-no-properties 2 text-line))
                    (heading-level
                     (length (match-string-no-properties 1 text-line)))
                    (formatted-heading))

                ;; Want to make sure this has a correct value.
                (when (null prev-heading-level)
                  (setq prev-heading-level heading-level))

                ;; Decide whether to update the prefix list and the previous
                ;; heading level.
                (cond
                 ;; If we've moved to a greater level (further down the tree),
                 ;; add the previous heading to the heading prefix list so
                 ;; that we can prepend it to the current heading when
                 ;; formatting.
                 ((> heading-level prev-heading-level)
                  (setq backwards-prefix-list (cons prev-heading-text
                                                    backwards-prefix-list)
                        prev-heading-level heading-level))
                 ;; Otherwise, if we've moved to a lower level (higher up the
                 ;; tree), and need to remove the most recently added prefix
                 ;; from the list (i.e., go from '(c b a) back to '(b a)).
                 ((< heading-level prev-heading-level)
                  (setq backwards-prefix-list (last backwards-prefix-list
                                                    heading-level)
                        prev-heading-level heading-level))
                 ;; Otherwise, do nothing.
                 (t nil))

                ;; Regardless of what happens, update the previous heading text.
                (setq prev-heading-text heading-text)

                ;; Decide whether the previous formatted heading was the
                ;; default.
                (when (and (null default-heading)
                           (> (- line-number current-line-number) 0))
                  (setq default-heading (car formatted-headings)))

                ;; Finally, add to list of formatted headings.
                ;; Create heading of form "L#: a/b/c" as:
                ;; - having a text property holding the line number
                ;; - prepended with a formatted line number,
                ;;   with the face `completions-annotations'.
                (push (propertize
                       (concat (string-join (reverse backwards-prefix-list) "/")
                               (and backwards-prefix-list "/")
                               heading-text)
                       'line-number line-number
                       'selectrum-candidate-display-prefix
                       (propertize
                        (format line-number-format line-number)
                        'face 'completions-annotations))
                      formatted-headings)))))

        ;; Now that candidates formatted, select from candidates.
        (let ((chosen-heading
               (selectrum-read "Jump to heading: "
                               (nreverse formatted-headings)
                               :default-candidate default-heading
                               :history 'selectrum-outline-history
                               :require-match t
                               :no-move-default-candidate t)))
          ;; Push mark, in case we want to return to current location.  This
          ;; needs to happen /after/ the user has made it clear that they want
          ;; to go somewhere.
          (push-mark (point) t)
          ;; Move to beginning of chosen line.
          (forward-line (- (get-text-property 0 'line-number chosen-heading)
                           current-line-number))
          (beginning-of-line-text 1)))
    (user-error "selectrum-outline: No headings defined for %s." major-mode)))

```

Here is a version using `loopy`:

``` elisp
(defun selectrum-outline-loopy ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  ;; Signal a `user-error' if we don't have a regexp for this major mode.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (let ((selectrum-should-sort-p))
        )

    (let ((selectrum-should-sort-p nil) ; Headings should stay in order of appearance.
          ;; Get the basic information of each heading in the accessible
          ;; portion of the buffer.
          (buffer-lines (split-string (buffer-string) "\n"))
          (line-number 0)
          (line-number-format)

          ;; Finding the default heading
          (default-heading)
          (current-line-number (line-number-at-pos (point)))

          ;; Keeping track of the tree.
          (backwards-prefix-list)
          (prev-heading-text)
          (prev-heading-level)

          ;; Backwards result of the `dolist'.  Will `nreverse'.
          (formatted-headings))

      (setq line-number-format
            (concat "L%0"
                    (number-to-string
                     (length (number-to-string (length buffer-lines))))
                    "d: "))

      (save-match-data
        (dolist (text-line buffer-lines)
          ;; Increment line number when moving to next.
          (cl-incf line-number)
          (when (string-match heading-regexp text-line)
            (let ((heading-text (match-string-no-properties 2 text-line))
                  (heading-level
                   (length (match-string-no-properties 1 text-line)))
                  (formatted-heading))

              ;; Want to make sure this has a correct value.
              (when (null prev-heading-level)
                (setq prev-heading-level heading-level))

              ;; Decide whether to update the prefix list and the previous
              ;; heading level.
              (cond
               ;; If we've moved to a greater level (further down the tree),
               ;; add the previous heading to the heading prefix list so
               ;; that we can prepend it to the current heading when
               ;; formatting.
               ((> heading-level prev-heading-level)
                (setq backwards-prefix-list (cons prev-heading-text
                                                  backwards-prefix-list)
                      prev-heading-level heading-level))
               ;; Otherwise, if we've moved to a lower level (higher up the
               ;; tree), and need to remove the most recently added prefix
               ;; from the list (i.e., go from '(c b a) back to '(b a)).
               ((< heading-level prev-heading-level)
                (setq backwards-prefix-list (last backwards-prefix-list
                                                  heading-level)
                      prev-heading-level heading-level))
               ;; Otherwise, do nothing.
               (t nil))

              ;; Regardless of what happens, update the previous heading text.
              (setq prev-heading-text heading-text)

              ;; Decide whether the previous formatted heading was the
              ;; default.
              (when (and (null default-heading)
                         (> (- line-number current-line-number) 0))
                (setq default-heading (car formatted-headings)))

              ;; Finally, add to list of formatted headings.
              ;; Create heading of form "L#: a/b/c" as:
              ;; - having a text property holding the line number
              ;; - prepended with a formatted line number,
              ;;   with the face `completions-annotations'.
              (push (propertize
                     (concat (string-join (reverse backwards-prefix-list) "/")
                             (and backwards-prefix-list "/")
                             heading-text)
                     'line-number line-number
                     'selectrum-candidate-display-prefix
                     (propertize
                      (format line-number-format line-number)
                      'face 'completions-annotations))
                    formatted-headings)))))

      ;; Now that candidates formatted, select from candidates.
      (let ((chosen-heading
             (selectrum-read "Jump to heading: "
                             (nreverse formatted-headings)
                             :default-candidate default-heading
                             :history 'selectrum-outline-history
                             :require-match t
                             :no-move-default-candidate t)))
        ;; Push mark, in case we want to return to current location.  This
        ;; needs to happen /after/ the user has made it clear that they want
        ;; to go somewhere.
        (push-mark (point) t)
        ;; Move to beginning of chosen line.
        (forward-line (- (get-text-property 0 'line-number chosen-heading)
                         current-line-number))
        (beginning-of-line-text 1)))
    (user-error "selectrum-outline: No headings defined for %s." major-mode)))
```

For a "translation table" of sorts from `cl-loop` to `loopy`, see the end of
this document.

## How to use

`loopy` takes at most 7 arguments.  They are all technically optional, but a
loop that does nothing isn't very useful.  Except for a name for the loop, all of
the arguments are list that begin with a keyword (allowing for conveniences).

| Keyword          | Other Names      | Usage                                                   |
|------------------|------------------|---------------------------------------------------------|
| `with`           | `let*`           | Declare variables before the loop.                      |
| `before-do`      | `before-progn`   | Run Lisp expressions before loop starts.                |
| `loop`           | Can be excluded. | Add expressions to loop body, performing further setup. |
| `after-do`       | `after-progn`    | Run Lisp expressions after loop successfully completes. |
| `finally-do`     | `finally-progn`  | Always run Lisp expressions after loop exits.           |
| `finally-return` | `return`         | Return a value, regardless of success.                  |


Loop body commands are the meat of the `loopy` macro, and are described in the
following sections.   A loop command inserts expressions into the loop body, but
can also perform additional setup, such as initializing specified variables or
creating extra ones.  Many set up a condition for ending the loop.

A loop ends when any condition required by a loop command evaluates to false.  If
no conditions are needed, the loop runs infinitely unless a return command is
reached.

Returns must be stated explicitly, either as an early return for in the loop
body via the `return` command, or as a `finally-return` to the macro.  `nil` is
returned by default.

### Loop Body Commands

A command can underneath be made of several instructions (which are described in
detail in [Extending with Personal Loop Commands](#extending_with_personal_loop_commands)).
Some examples are
- Declare a given variable in a let form to make sure it's lexically scoped.
- Declare a generated variable in a let form to contain a given value.
- Add a condition for continuing/exiting the loop.
- Add code to be run during the main loop body.
- Add code to be run after the main loop body.

The implementation details of commands generally shouldn't matter, except that
code from commands is evaluated in the order it was found.  This means that
attempting to do something like

``` elisp
(loopy (loop (collect coll (+ i 2))
             (list i '(1 2 3)))
       (return coll))
```

won't work, as `i` is assigned to after collecting `(+ i 2)` into `coll`.

For convenience, the same command can have multiple names (such as `do` and
`progn`), and some commands can take optional arguments (such as `list`).  In
this section, alternative names are separated by a vertical bar (`do|progn`),
and optional arguments are surround by brackets (`[EXPR]`).

For consistency, `EXPR` below means a single Lisp expression.  `EXPRS` means
multiple Lisp expressions, with `[EXPRS]` being equivalent to
`[EXPR [EXPR [...]]]`.  `CMD` means a loop body command, as opposed to normal
Lisp code.  `VAR` is an unquoted variable name.

#### Commands for Generic Evaluation
- `(do|progn EXPRS)`: Evaluate multiple Lisp expressions, like a `progn`.  You
  cannot include arbitrary code in the loop body, except for the conditions of
  the conditional commands (`when`, `unless`, `if`, and `cond`) and in a `do`
  command.  Doing otherwise will result in errors, as the macro will attempt to
  interpret such code as a command.

  ```elisp
  (loopy ((list i '(1 2 3))
          (do (message "%d" i))))
  ```

- `(expr VAR EXPR)`: Bind `VAR` to the value of `EXPR` in each iteration.

  **NOTE**: Loops are lexically scoped, so this is not always the same as
            `(do (setq VAR EXPR))`.

  ```elisp
  (loopy ((list i '(1 2 3))
          (expr j (* i 2))
          (do (message "%d" j))))
  ```

#### Iteration and Looping Commands

The iteration commands bind local variables and determine when the loop ends.
If no command sets that condition, then the loop runs forever.

Some commands take an optional function argument, which changes how iteration
progresses.  This is represented by `[FUNC]`.

- `(array VAR EXPR)`: Iterate through the elements of the array from `EXPR`.

  ```elisp
  (loopy ((array i [1 2 3])
          (do (message "%d" i))))
  ```

- `(array-ref|arrayf VAR EXPR)`: Iterate through the elements of the array from
  `EXPR`, binding `VAR` to a `setf`-able place.

  ```elisp
  (loopy (with (my-str "cat"))
         (loop (array-ref i my-str)
               (do (setf i ?a)))
         (return my-str)) ; => "aaa"
  ```

- `(cons|conses VAR EXPR [FUNC])`: Iterate through the cons cells in the value
  of `EXPR`.  Optionally, find the cons cells via `FUNC` instead of `cdr`.

- `(list VAR EXPR [FUNC])`: Iterate through the list `EXPR`, binding `VAR` to
  each element in the list.  Optionally, update the list by `FUNC` instead of
  `cdr`.

  ```elisp
  (loopy ((list i (number-sequence 1 10 3)) ; Inclusive, so '(1 4 7 10).
          (do (message "%d" i))))
  ```

- `(list-ref|listf VAR EXPR [FUNC])`: Iterate through the list `EXPR`, binding
  `VAR` to each element in the list as a `setf`-able location.  Optionally,
  update the list by `FUNC` instead of `cdr`.

  ```elisp
  (loopy (with (my-list '(1 2 3)))
         (loop (list-ref i my-list)
               (do (setf i 7)))
         (finally-return my-list)) ; Returns '(7 7 7).
  ```

- `(repeat EXPR)`: Add a condition that the loop should stop after `EXPR`
  iterations.

  ```elisp
    (loopy ((repeat 3)
          (do (message "Messaged three times."))))
  ```

- `(repeat VAR EXPR)`: Add a condition that the loop should stop after
  `NUMBER` iterations.  `VAR` starts at 0, and is incremented by 1 at the end of
  the loop.

  ```elisp
  (loopy ((repeat i 3)
          (do (message "%d" i))))

  ```

- `(seq VAR EXPR [FUNC])`: Iterate through the sequence `val`, binding `var` to the
  elements of the sequence.

  ```elisp
  (loopy ((seq i [1 2 3])
          (do (message "%d" i))))
  ```

- `(seq-ref|seqf VAR EXPR [FUNC])`: Iterate through the sequence `val`, binding `var`
  to the elements of the sequence as a `setf`-able place.

  ```elisp
  (loopy (with (my-seq '(1 2 3 4)))
                   (loop (seq-ref i my-seq)
                         (do (setf i 7)))
                   (return my-seq)) ; => '(7 7 7 7)
  ```

#### Accumulation Commands

Unlike in `cl-loop`, the presence of an accumulation does not imply a return
value.  You must provide a variable in which to store the accumulated value.  If
you wish, you can then return the value of that variable (either early, or after
the loop).

- `(append VAR EXPR)`: Repeatedly `append` the value of `EXPR` to `VAR`. `VAR`
  starts as `nil`.

- `(collect VAR EXPR)`: Repeatedly `append` a list containing value of `EXPR` to
  `VAR`.  `VAR` starts as `nil`.

  ```elisp
  (loopy ((seq i [1 2 3])
          (collect coll i))
         (finally-return coll)) ; => '(1 2 3)
  ```

  In `cl-loop`, `collect EXPR` means to repeatedly `push` the value of `EXPR`
  into the accumulated list, and then `nreverse` that list for a return value.
  If you specifically want this behavior, then you should use the `push-into`
  command like in its respective example below.

- `(concat VAR EXPR)`: Repeatedly `concat` the value of `EXPR` onto the end of
  `VAR`.  `VAR` starts as `nil`.  See the `vconcat` command for vectors.

  ```elisp
  (loopy ((list i '("a" "b" "c"))
          (concat str i))
         (return str)) ; => "abc"
  ```

- `(count VAR EXPR)`: Count the number of times that `EXPR` evaluates to a
  non-nil value, adding 1 to `VAR` each time.  `VAR` starts at 0.

  ```elisp
  (loopy ((list i '(1 nil 3 nil 5))
          (count non-nil-count i))
         (return non-nil-count)) ; => 3
  ```

- `(max|maximize VAR EXPR)`: Repeatedly set `VAR` to the greater of `VAR` and
  the value of `EXPR`.  `VAR` starts at `-1.0e+INF`, so that any other value
  should be greater that it.

  ```elisp
  (loopy ((list i '(1 11 2 10 3 9 4 8 5 7 6))
          (max my-max i))
         (return my-max)) ; => 11
  ```

- `(min|minimize VAR EXPR)`: Repeatedly set `VAR` to the lesser of `VAR` and
  the value of `EXPR`.  `VAR` starts at `1.0e+INF`, so that any other value
  should be less than it.

  ```elisp
  (loopy ((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
          (min my-min i))
         (return my-min)) ; => 0
  ```

- `(nconc VAR EXPR)`: Repeatedly concatenate the value of `EXPR` onto `VAR` with
  `nconc`.  Unlike `append`, `nconc` does not concatenate copies of the lists,
  but modifies `VAR` directly.

  ```elisp
  (loopy (loop (list i '((1 2 3 4) (5 6 7 8)))
               (nconc my-new-list i))
         (return my-new-list)) ; => '(1 2 3 4 5 6 7 8)
  ```

- `(push|push-into|prepend VAR EXPR)`: Repeatedly `push` `EXPR` into
  `VAR`. `VAR` stars as `nil`.

  ```elisp
  (loopy ((seq i [1 2 3])
          (push reversed i))
         (finally-return (nreverse reversed))) ; => '(1 2 3)
  ```

- `(sum VAR EXPR)`: Repeatedly set `VAR` to the sum of the value of `EXPR` and
  `VAR`.  `VAR` starts at 0.

  ```elisp
  (loopy ((list i '(1 2 3 4))
          (sum my-sum i))
         (return my-sum)) ; => 10
  ```

- `(vconcat VAR EXPR)`: Repeatedly `vconcat` the value of `EXPR` onto `VAR`.
  `VAR` starts as `nil`.

  ```elisp
  (loopy ((list i '([1 2 3] [4 5 6]))
          (vconcat vector i))
         (return vector)) ; => [1 2 3 4 5 6]
  ```

#### Control Flow
##### Conditionals

- `(when EXPR CMDS)`: Like a normal `when`, run the commands `CMDS` only if
  `EXPR` is non-nil.

  ```elisp
  ;; Get only the inner lists with all even numbers.
  (loopy ((list i '((2 4 6) (8 10 12) (13 14 15) (16 18 20)))
          (when (loopy ((list j i)
                        (when (cl-oddp j) (return nil)))
                       (finally-return t))
            (collect only-evens i)))
         (finally-return only-evens))
  ```

- `(if EXPR CMDS)`: Like a normal `if`, run the first command if `EXPR` is
  non-nil. Otherwise, run the remaining commands.

  ```elisp
  (loopy ((seq i [1 2 3 4 5 6 7 8 9 10])
          (if (cl-oddp i)
              (prepend reversed-odds i)
            (prepend reversed-evens i)
            (prepend some-threes 3)))
            (finally-return (list reversed-odds
                                  reversed-evens
                                  some-threes)))
   ```

- `(cond [(EXPR CMDS) [...]])`: Like a normal `cond`, for the first `EXPR` to
  evaluate to non-nil, run the following commands `CMDS`.

  ```elisp
        (loopy ((list i (number-sequence 1 10))
                (cond
                 ((cl-evenp i)
                  (prepend evens i))
                 (t (prepend odds i))))
               (finally-return (list evens odds)))
        ```

##### Skipping an Iteration

- `(skip|continue)`: Go to next loop iteration.

  ```elisp
  (loopy ((seq i (number-sequence 1 20))
          (when (zerop (mod i 10))
            (skip))
          (when (cl-evenp i)
            (prepend my-collection i)))
         (finally-return (nreverse my-collection)))
  ```

##### Exiting the Loop Early

The loop is contained in a `cl-block`, and these forms are all `cl-return-from`
underneath.  In fact, you could use `(do (cl-return-from NAME VAL))` to achieve
the same effect.  These forms are provided for convenience.

- `(return EXPR)`:   Leave the current loop, returning value.

  ```elisp
  (loopy (with  (j 0))
         ((do (cl-incf j))
          (when (> j 5)
            (return j))))
  ```

- `(return-from NAME EXPR)`: Leave the loop `NAME`, returning `VAL`.

  ```elisp
  (loopy outer-loop
      ((list inner-list '((1 2 3) (1 bad-val? 1) (4 5 6)))
          (do (loopy ((list i inner-list)
                      (when (eq i 'bad-val?)
                      (return-from outer-loop 'bad-val?)))))))
  ```

- `(leave|break)`: Leave the loop.  Return `nil`.

  ``` elisp
  (loopy ((list i '(1 2 3 "cat" 4 5 6))
          (if (numberp i)
              (do (message "Number: %d" i))
            (leave))))
  ```

- `(leave-from|break-from NAME)`: Leave the loop `NAME`.  Return `nil`.

  ``` elisp
  (loopy outer
      (with (failure-condition 'fail)
              (failed-p nil))
      ((list i '((1 2 3) (4 5 6) (7 fail 8)))
          (do (loopy ((list j i)
                      (when (eq j failure-condition)
                      ;; Note: Can't do (expr failed-p t), since
                      ;;       `expr' is local to its own loop.
                      (do (setq failed-p t))
                      (break-from outer))))))
      (finally-do (if failed-p
                      (message "Failed!")
                      (message "Success!"))))
  ```

## Extending with Personal Loop Commands
### Background Explanation
The core working of `loopy` is taking a command and generating code that is
substituted into a loop body.

For example, the parsing the command `(list i '(1 2 3))` produces the following
instructions, in which the `car` of the instruction is a place to put code and
the `cdr` of the instruction is said code to put.  Some commands require the
creation of unique temporary variables, such as `g3019` in the below output.

``` elisp
(loopy--implicit-vars g3019 '(1 2 3))
(loopy--explicit-vars i nil)
(loopy--pre-conditions consp g3019)
(loopy--loop-body setq i (car g3019))
(loopy--latter-body setq g3019 (cdr g3019))
```

Commands are parsed by the function `loopy--parse-body-forms` (which calls other
parsing functions if needed).  For example, to parse `when` commands, which can
have a body of other `loop` commands, it calls
`loopy--parse-conditional-forms`.  We can see that
`loopy--parse-conditional-forms` calls back into `loopy--parse-body-forms` to
create instructions from the `when` commands body, and then wraps any
code going to the main loop body with the Emacs Lisp `when` macro.

``` elisp
(defun loopy--parse-conditional-forms
  (wrapper condition forms &optional loop-name)
  "Parse FORMS, wrapping `loopy--main-body' expressions in a conditional form.
The instructions (e.g., return expressions) are wrapped with a
WRAPPER with CONDITION.  Optionally needs LOOP-NAME for block
returns."
  (let ((full-instructions)
        (sub-instructions (loopy--parse-body-forms forms loop-name))
        (loop-body))
    (dolist (instruction sub-instructions)
      (cl-case (car instruction)
        (loopy--main-body
         (push (cdr instruction) loop-body))
        (t (push instruction full-instructions))))
    (push `(loopy--main-body . (,wrapper ,condition ,@loop-body))
          full-instructions)
    full-instructions))
```
A loop body command has 7 places to put code.  Here is a quick description of
each and an example taken mainly from parsing the `list` command.

- `loopy--explicit-generalized-vars`: Lists of a symbol and a macro expansion
  that will be given to `cl-symbol-macrolet`.  This is used for `setf`-able
  variables.

  ~~~
  `(loopy--explicit-generalized-vars . (,var (car ,val-holder)))
  ~~~

- `loopy--implicit-vars`: Lists of a symbol and an expression that will be given
  to `let`.  This is used for creating variables that are not named by must
  exists, such as for holding `'(1 2 3)` in `(list i '(1 2 3))`.

  ~~~
  `(loopy--implicit-vars . (,val-holder ,list))
  ~~~

- `loopy--explicit-vars`: Lists of a symbol and an expression that will be given
  to `let`.  This is needed to ensure that named variables in commands are
  lexically scoped, such as the `i` in `(list i '(1 2 3))`.

  ~~~
  `(loopy--explicit-vars . (,var nil))
  ~~~

- `loopy--pre-conditions`: Expressions that determine if the `while` loop
  runs/continues, such as whether a list still has elements in it.  If there is
  more than one expression, than all expressions are used in an `and` special
  form.

  ~~~
  `(loopy--pre-conditions . (consp ,val-holder))
  ~~~

- `loopy--main-body`: Expressions that make up the main body of the loop.

  ~~~
  `(loopy--main-body . (setq ,var (car ,val-holder)))
  ~~~

- `loopy--latter-body`: Expressions that need to be run after the main body,
  such as updating implicit variables.

  ~~~
  `(loopy--latter-body . (setq ,val-holder (,actual-func ,val-holder)))
  ~~~

<!-- - `loopy--post-conditions`: *Currently unused.*  Expressions that determine -->
<!--   whether the `while` loop continues, but checked after the loop body has run. -->

These `cdr`s of these instructions will be substituted into the following
quoted code, which is the return value of the `loopy` macro.

``` elisp
`(cl-symbol-macrolet (,@(or loopy--explicit-generalized-vars
                            (list (list (gensym) nil))))
   (let* (,@(or loopy--with-forms '((_))))
     (let (,@(or (append loopy--implicit-vars loopy--explicit-vars)
                 '((_))))
       (let ((loopy--early-return-capture
              (cl-block ,loopy--name-arg
                ,@loopy--before-do
                (while ,(cl-case (length loopy--pre-conditions)
                          (0 t)
                          (1 (car loopy--pre-conditions))
                          (t (cons 'and loopy--pre-conditions)))
                  (cl-tagbody
                   ,@loopy--main-body
                   loopy--continue-tag
                   ,@loopy--latter-body))
                ,@loopy--after-do
                nil)))
         ,@loopy--final-do
         ,(if loopy--final-return
              loopy--final-return
            'loopy--early-return-capture)))))
```

### A Small Example

To implement a custom loop body command, `loopy` needs two pieces of
information:
1. The keyword that names your command
2. The parsing function that can turn uses of your command into instructions.

Importantly, your custom commands cannot share a name.

For example, say that you're tired of typing out `(do (message "Hello, %s" first
last))` and would prefer to instead use `(greet FIRST [LAST])`.  This only
requires pushing code into the main loopy body, so the definition of the parsing
function is quite simple.

``` elisp
(cl-defun my-loopy-greet-command-parser ((_ first &optional last))
  "Greet one with first name FIRST and optional last name LAST."
  `((loopy--main-body . (if ,last
                            (message "Hello, %s %s" ,first ,last)
                          (message "Hello, %s" ,first)))))
```

`loopy` will pass the entire command expression to the parsing function, and
expects back a list of instructions.

To tell `loopy` about this function, add it and the command name `greet` to
`loopy-custom-command-parsers`.

``` elisp
(add-to-list 'loopy-custom-command-parsers
             '(greet . my-loopy-greet-command-parser))
```

After that, you can use your custom command in the loop body.

``` elisp
(loopy ((list name '(("John" "Deer") ("Jane" "Doe") ("Jimmy")))
        (greet (car name) (cadr name))))
```

By running `M-x pp-macroexpand-last-sexp` on the above expression, you can see
that it expands to do what we want, as expected.

``` elisp
(cl-symbol-macrolet ((g3314 nil))
  (let* ((_))
    (let ((g3313 '(("John" "Deer") ("Jane" "Doe") ("Jimmy")))
          (name nil))
      (let ((loopy--early-return-capture
             (cl-block nil
               (while (consp g3313)
                 (cl-tagbody
                  (setq name (car g3313))
                  (if (cadr name)
                      (message "Hello, %s %s" (car name) (cadr name))
                    (message "Hello, %s" (car name)))
                  loopy--continue-tag
                  (setq g3313 (cdr g3313))))
               nil)))
        loopy--early-return-capture))))
```

## Translating from `cl-loop`

### For Clauses

As Emacs has many functions that return lists, I decided to not implement an
exact equivalent for every for-clause that `cl-loop` has.  Instead, one can just
iterate through the return value of the appropriate function using the `list`
command.

| `cl-loop`                                     | `loopy`                                           |
|:----------------------------------------------|:-------------------------------------------------|
| `for VAR from EXPR1 to EXPR2 by EXPR3`        | `(list VAR (number-sequence EXPR1 EXPR2 EXPR3))` |
| `for VAR in LIST [by FUNCTION]`               | `(list VAR LIST [FUNC])`                         |
| `for VAR on LIST [by FUNCTION]`               | `(cons VAR VAL [FUNC])`                          |
| `for VAR in-ref LIST by FUNCTION`             | `(list-ref VAR LIST [FUNC])`                     |
| `for VAR across ARRAY`                        | `(array VAR ARRAY)`                              |
| `for VAR across-ref ARRAY`                    | `(array-ref VAR ARRAY)`                          |
| `for VAR being the elements of SEQUENCE`      | `(seq VAR SEQUENCE)`                             |
| `for VAR being the elements of-ref SEQUENCE`  | `(seq-ref VAR SEQUENCE)`                         |
| `for VAR being the symbols [of OBARRAY]`      | None so far.                                     |
| `for VAR being the hash-keys of HASH-TABLE`   | `(list VAR (hash-table-keys HASH-TABLE))`        |
| `for VAR being the hash-values of HASH-TABLE` | `(list VAR (hash-table-values HASH-TABLE))`      |
| `for VAR being the key-codes of KEYMAP`       | None so far.                                     |
| `for VAR being the key-bindings of KEYMAP`    | None so far.                                     |
| `for VAR being the key-seqs of KEYMAP`        | None so far.                                     |
| `for VAR being the overlays [of BUFFER]`      | None so far.                                     |
| `for VAR being the intervals [of BUFFER]`     | None so far.                                     |
| `for VAR being the frames`                    | `(list VAR (frame-list))`                        |
| `for VAR being the windows [of FRAME]`        | `(list VAR (window-list FRAME))`                 |
| `for VAR being the buffers`                   | `(list VAR (buffer-list))`                       |
| `for VAR = EXPR1 then EXPR2`                  | None so far.                                     |

### Iteration Clauses

| `cl-loop`         | `loopy`               |
|:------------------|:----------------------|
| repeat INT do ... | (repeat INT)          |
| while COND do ... | (unless COND (leave)) |
| until COND do ... | (when COND (leave))   |
| iter-by iterator  | None so far.          |

For the clauses `always`, `never`, `thereis`, can be replaced with a combination
of `(leave)` in the loop body and `(finally-return)` as a macro argument.  Below
is an example from the CL Lib manual.

``` elisp
;; With `cl-loop':
(if (cl-loop for size in size-list always (> size 10))
    (only-big-sizes)
  (some-small-sizes))

;; With `loopy':
;; - A less convenient but more literal translation:
(if (loopy (with (success? t))
           ((list size size-list)
            (unless (> size 10)
              (expr success? nil)
              (leave)))
           (finally-return success?))
    (only-big-sizes)
  (some-small-sizes))

;; - A more convenient/straightforward translation,
;;   depending on whether the functions have a return value.
(loopy ((list size size-list)
        (when (< size 10) (do (some-small-sizes)) (leave)))
       ;; Only runs if loop doesn't exit early.
       (after-do (only-big-sizes)))
```

A seen in the above example, `loopy` does not always have a one-to-one
translation to `cl-loop`.  It is not an explicit goal to be able to replace all
uses of `cl-loop` with `loopy`, and in the above example, you could be better
served by `cl-every`, `seq-every-p`, or even continuing to use `cl-loop`.

### Accumulation Clauses


[sequence-docs]: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html>
[cl-loop]: <https://www.gnu.org/software/emacs/manual/html_node/cl/Loop-Facility.html>


<!-- Would normally would grab H1 level, but we're using that for the title. -->
<!-- Local Variables: -->
<!-- flycheck-disabled-checkers: (proselint) -->
<!-- End: -->
