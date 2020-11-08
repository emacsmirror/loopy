# Loopy: An Emacs Looping Library

Loopy is a macro meant for iterating and looping.  It is similar in usage to
`cl-loop` but uses symbolic expressions rather than keywords.

It should be comparable with `cl-loop`, keeping in mind the following points:
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

In addition to a name for the loop, the `loopy` macro has several possible
arguments, each beginning with a keyword.
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

Loopy is not feature complete.  Here are some things that would be nice:
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

#### Generic Evaluation
- `(do|progn EXPRS)`: Evaluate multiple Lisp expressions, like a `progn`.  You
  cannot include arbitrary code in the loop body, except for the conditions of
  the conditional commands (`when`, `unless`, `if`, and `cond`) and in a `do`
  command.  Doing otherwise will result in errors, as the macro will attempt to
  interpret such code as a command.

  ```elisp
  (loopy ((list i '(1 2 3))
          (do (message "%d" i))))
  ```

#### Assignment and Iteration

The iteration commands determine when the loop ends.  If no command sets that
condition, then the loop runs forever.

- `(expr VAR EXPR)`: Bind `VAR` to the value of `EXPR` in each iteration.

  **NOTE**: Be aware that loops are lexically scoped, so this is not always the
  same as `(do (setq VAR EXPR))`.

  ```elisp
  (loopy ((list i '(1 2 3))
          (expr j (* i 2))
          (do (message "%d" j))))
  ```

- `(seq VAR EXPR)`: Iterate through the sequence `val`, binding `var` to the
  first element in the sequence.  Afterwards, that element is dropped from the
  sequence.  The loop ends when the sequence is empty.

  ```elisp
  (loopy ((seq i [1 2 3])
          (do (message "%d" i))))
  ```

- `(list VAR EXPR [FUNC])`: Iterate through the list `EXPR`, binding `VAR` to
  each element in the list.  Optionally, update the list by `FUNC` instead of
  `cdr`.

  ```elisp
  (loopy ((list i (number-sequence 1 10 3)) ; Inclusive, so '(1 4 7 10).
          (do (message "%d" i))))
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

#### Accumulation

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


#### Conditionals

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

#### Skipping an Iteration

- `(skip|continue)`: Go to next loop iteration.

  ```elisp
  (loopy ((seq i (number-sequence 1 20))
          (when (zerop (mod i 10))
            (skip))
          (when (cl-evenp i)
            (prepend my-collection i)))
         (finally-return (nreverse my-collection)))
  ```

#### Leaving or Exiting the Loop Early

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

To be implemented, but should be straight forward.

## Translating from `cl-loop`

### For Clauses

As Emacs has many functions that return lists, I decided to not implement an
exact equivalent for every for-clause that `cl-loop` has.  Instead, one can just
iterate through the return value of the appropriate function using the `list`
command.

| `cl-loop`                                     | `loop`                                         |
|:----------------------------------------------|:-----------------------------------------------|
| `for VAR from EXPR1 to EXPR2 by EXPR3`        | (list VAR (number-sequence EXPR1 EXPR2 EXPR3)) |
| `for VAR in LIST`                             |                                                |
| `for VAR in LIST by FUNCTION`                 |                                                |
| `for VAR on LIST`                             | (cdrs VAR VAL)                                 |
| `for VAR on LIST by FUNCTION`                 |                                                |
| `for VAR in-ref LIST by FUNCTION`             |                                                |
| `for VAR across ARRAY`                        | (array VAR ARRAY)                              |
| `for VAR across-ref ARRAY`                    |                                                |
| `for VAR being the elements of SEQUENCE`      | (sequence VAR SEQUENCE)                        |
| `for VAR being the elements of-ref SEQUENCE`  |                                                |
| `for VAR being the symbols [of OBARRAY]`      |                                                |
| `for VAR being the hash-keys of HASH-TABLE`   | (list VAR (hash-table-keys HASH-TABLE))        |
| `for VAR being the hash-values of HASH-TABLE` | (list VAR (hash-table-values HASH-TABLE))      |
| `for VAR being the key-codes of KEYMAP`       |                                                |
| `for VAR being the key-bindings of KEYMAP`    |                                                |
| `for VAR being the key-seqs of KEYMAP`        |                                                |
| `for VAR being the overlays [of BUFFER]`      |                                                |
| `for VAR being the intervals [of BUFFER]`     |                                                |
| `for VAR being the frames`                    | (list VAR (frame-list))                        |
| `for VAR being the windows [of FRAME]`        | (list VAR (window-list FRAME))                 |
| `for VAR being the buffers`                   | (list VAR (buffer-list))                       |
| `for VAR = EXPR1 then EXPR2`                  |                                                |
|                                               |                                                |



[sequence-docs]: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html>


<!-- Would normally would grab H1 level, but we're using that for the title. -->
<!-- Local Variables: -->
<!-- flycheck-disabled-checkers: (proselint) -->
<!-- End: -->
