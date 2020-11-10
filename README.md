# Loopy: A Looping and Iteration Macro

`loopy` is a macro meant for iterating and looping.  It is similar in usage to
[`cl-loop`][cl-loop] but uses symbolic expressions rather than keywords.

`loopy` should be complementary to the features provided the [Seq][seq.el] and
[CL][cl-lib.el] libraries (including `cl-loop`) and Emacs's regular looping and
mapping features.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Loopy: A Looping and Iteration Macro](#loopy-a-looping-and-iteration-macro)
    - [How does it compare to other approaches?](#how-does-it-compare-to-other-approaches)
    - [How to use](#how-to-use)
        - [Macro Arguments](#macro-arguments)
        - [Loop Commands](#loop-commands)
            - [Commands for Generic Evaluation](#commands-for-generic-evaluation)
            - [Iteration and Looping Commands](#iteration-and-looping-commands)
            - [Accumulation Commands](#accumulation-commands)
            - [Control Flow](#control-flow)
                - [Conditionals](#conditionals)
                - [Skipping an Iteration](#skipping-an-iteration)
                - [Exiting the Loop Early](#exiting-the-loop-early)
    - [Adding Custom Commands](#adding-custom-commands)
        - [Background Information](#background-information)
        - [A Small Example](#a-small-example)
    - [Translating from `cl-loop`](#translating-from-cl-loop)
        - [For Clauses](#for-clauses)
        - [Iteration Clauses](#iteration-clauses)
        - [Accumulation Clauses](#accumulation-clauses)
        - [Other Clauses](#other-clauses)

<!-- markdown-toc end -->


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
- To collect values into a list, use `(collect my-collection collected-value)`.
- To just bind a variable to the result of a Lisp expression,
  use `(expr my-var (my-func))`

Below is a full example of the arguments of the `loopy` macro.  The top-level
forms have a flexible-order, but meaning is clearest if they have the following
order.  All of the arguments are technically optional, but having a loop without
a body wouldn't be very useful.

``` elisp
;; Returns: '((2 4 6 8 10) (1 3 5 7 9)) and prints messages.
(loopy
 ;; Name the loop `my-loop'.
 my-loop
 ;; Create the lexically scoped variable `success-p', initialized to `nil'.
 (with (success-p nil))
 ;; Before starting the loop, print a message.
 (before-do (message "Beginning loop ..."))
 ;; Create the loop body.
 (loop (list i (number-sequence 1 10))        ; Assign `i' from 1 through 10.
       (do (message "Checking number: %d" i)) ; Report the value of `i'.
       (if (cl-evenp i)                       ; If `i' is even, add to the list
           (collect found-evens i)            ; of even numbers, otherwise add
         (collect found-odds i)))             ; to the list of odd numbers.
 ;; If the loop completes successfully, print a message and update `success-p'.
 (after-do (message "Loop completed successfully.")
           (setq success-p t))
 ;; Always report based on the value of `success-p', and message the value of
 ;; the lists of even and odd numbers.
 (finally-do (if success-p
                 (message "Confirmed success reported.")
               (message "W: Success not confirmed!"))
             (message "Found evens: %s" found-evens)
             (message "Found odds: %s" found-odds))
 ;; Always return a list containing the list of even numbers and the list of odd
 ;; numbers.
 (finally-return (list found-evens found-odds)))
```

Loopy is not yet feature complete.  Here are some things that would be nice to
add/have:
- Add equivalent features for all that `cl-loop` can do.  Many things are
  already covered, but not everything.  See [Translating from
  `cl-loop`](#translating_from_cl_loop) for more.
- De-structuring can be useful, but this can already be done using repeated
  `expr` commands.

## How does it compare to other approaches?

`loopy` should be comparable with `cl-loop` for most things, keeping in mind
the following:
- It is probably less efficient than `cl-loop`, though I am so far trying to
  keep the same logic that `cl-loop` uses.
- It has more flexible control-flow commands, under which you can easily group
  sub-commands, including assignments.
- Using an accumulation command does not imply a return value.
- It has a `skip` command to skip to skip the rest of the loop body and
  immediately start the next iteration.  Of course, a similar effect could be
  achieved using the `when` or `unless` commands.

`loopy` is not always one-to-one replacement for `cl-loop`, but it is easy to
use and extend, and performs well in the cases that it already handles.

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
          (push-into together (cons once twice))))
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

           ;; … Further processing now that data is named …

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

          ;; … Further processing now that data is named …

          (push line-num line-nums)
          (push datum-num data-nums))))
    (list (nreverse line-nums) (nreverse data-nums))))
```

Here is how one could currently do it with `loopy`:

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
```


I believe that the value of the macro increases for longer loop bodies with
several conditional commands.

Another nice ability, one that I'm not sure `cl-loop` has, is
skipping/continuing a loop iteration.

``` elisp
;; Returns even numbers that aren't multiples of 10.
(loopy ((list i (number-sequence 1 20))
        (when (zerop (mod i 10))
          (skip))
        (when (cl-evenp i)
          (push-into my-collection i)))
       (finally-return (nreverse my-collection))) ; => (2 4 6 8 12 14 16 18)
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

Macro arguments set up the lexical environment the loop runs in, Lisp code that
runs before or after the loop, and the ultimate return value of the macro.  See
the section [Macro Arguments](#macro_arguments).

Loop commands are the main feature of the `loopy` macro.  By "command", I mean
the expressions that make up the `loop` macro argument, such as `list` in `(list
i '(1 2 3))`.  A command inserts code into the loop body, but can also perform
additional setup, such as initializing specified variables or creating extra
ones.  Many commands set up a condition for ending the loop.  See the section
[Loop Commands](#loop_commands).

The loop ends when any condition required by a loop command evaluates to false.
If no conditions are needed, the loop runs infinitely until a `return` or
`leave` command is reached.  See the section
[Exiting the Loop Early](#exiting_the_loop_early).

Returns must be stated explicitly, either as an early return for in the loop
body via the `return` command, or as a `finally-return` to the macro.  `nil` is
returned by default.

### Macro Arguments

`loopy` takes at most 7 arguments.  They are all technically optional, but a
loop that does nothing isn't very useful.

A loop can be named by passing in an unquoted symbol as an argument.  All other
arguments are expressions that begin with a keyword from the table below.

| Keyword          | Other Names                | Usage                                                   |
|------------------|----------------------------|---------------------------------------------------------|
| `with`           | `let*`                     | Declare variables before the loop.                      |
| `before-do`      | `before`                   | Run Lisp expressions before loop starts.                |
| `loop`           | Can be excluded.           | Add expressions to loop body, performing further setup. |
| `after-do`       | `after`, `else`, `else-do` | Run Lisp expressions after loop successfully completes. |
| `finally-do`     | `finally`                  | Always run Lisp expressions after loop exits.           |
| `finally-return` | `return`                   | Return a value, regardless of how the loop completes.   |

Additionally, `(finally-return 1 2 3)` is the same as
`(finally-return (list 1 2 3))`.


### Loop Commands

Loop commands are only valid when inside the `loop` macro argument.

These are valid:

``` elisp
(loopy (loop (list i '(1 2 3))
             (collect coll i))
       (finally-return coll))

(loopy ((list i '(1 2 3))
        (collect coll i))
       (return coll))
```

This is not:

``` elisp
(loopy (with (list i '(1 2 3)))
       (return (collect coll i)))
```

Trying to use loop commands where they don't belong will result in errors when
the code is evaluated.

Underneath, interpreting a command results in "instructions" that describe how
to substitute code into the loop body and other locations.  This process is
described in detail in [Background Information](#background_information).

Some examples of instructions are:
- Declaring a given variable in a let form to make sure it's lexically scoped.
- Declaring a generated variable in a let form to contain a given value.
- Adding a condition for continuing/exiting the loop.
- Adding code to be run during the main loop body.
- Adding code to be run after the main loop body.

The implementation details of commands generally shouldn't matter, except that
code from commands is evaluated in the order it was found.  This means that
attempting to do something like

``` elisp
(loopy (loop (collect coll (+ i 2))
             (list i '(1 2 3)))
       (return coll))
```

won't work, as `i` is assigned a value after collecting `(+ i 2)` into `coll`.

For convenience and understanding, the same command can have multiple names
(such as `do` having the alias `progn`), and some commands can take optional
arguments (such as `list`).

For simplicity, the commands are described using the following notation:

- If a command has multiple names, the names are separated by a vertical bar,
  such as in `do|progn`.
- `VAR` is an unquoted symbol that will be used as a variable name, such as the
  `i` in `(list i my-list)`.
- `FUNC` is a Lisp function name, such as `my-func`, `#'my-func` or `'my-func`.
- `NAME` is an unquoted name of a loop (or, more accurately, of a `cl-block`).
- `EXPR` is a single Lisp expression, such as `(+ 1 2)`, `'(1 2 3)`, `my-var`,
  or `(some-function my-var)`.  `EXPRS` means multiple expressions.
- `CMD` is a loop command, as opposed to a normal Lisp expression.
  `(list i '(1 2 3))`, `(repeat 5)`, and `(return-from outer-loop 7)` are
  examples of loop commands.  `CMDS` means multiple commands.
- Optional arguments are surround by brackets.  `[EXPR]` is an optional
  expression, and `[CMD]` is an optional command.  By extension, `[EXPRS]` is
  equivalent to `[EXPR [EXPR [...]]]`, and `[CMDS]` to `[CMD [CMD [...]]]`.

#### Commands for Generic Evaluation

- `(do|progn EXPRS)`: Evaluate multiple Lisp expressions, like a `progn`.

  You cannot include arbitrary code in the loop body.  Trying to do so will
  result in errors, as the macro will attempt to interpret such code as a
  command.

  ```elisp
  (loopy ((list i '(1 2 3))
          (do (message "%d" i))))
  ```

- `(expr|exprs|set VAR [EXPRS])`: Bind `VAR` to each `EXPR` in order.  Once the
  last `EXPR` is reached, it is used repeatedly for the rest of the loop.  With
  no `EXPR`, `VAR` is repeatedly bound to `nil`.

  **NOTE**: Loops are lexically scoped, so this is not always the same as
            `(do (setq VAR EXPR))`.

  ```elisp
  (loopy ((repeat 5) (expr i 1 2 3) (collect coll i))
         (return coll)) ; => '(1 2 3 3 3)

  (loopy ((repeat 5) (expr i 0 (1+ i)) (collect coll i))
         (return coll)) ; => '(0 1 2 3 4)
  ```

#### Iteration and Looping Commands

Iteration commands bind local variables and determine when the loop ends.
If no command sets that condition, then the loop runs forever.

- `(array VAR EXPR)`: Iterate through the elements of the array `EXPR`.

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

  ``` elisp

  ```

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

- `(seq VAR EXPR)`: Iterate through the sequence `val`, binding `var` to the
  elements of the sequence.

  ```elisp
  (loopy ((seq i [1 2 3])
          (do (message "%d" i))))
  ```

- `(seq-ref|seqf VAR EXPR)`: Iterate through the sequence `val`, binding `var`
  to the elements of the sequence as a `setf`-able place.

  ```elisp
  (loopy (with (my-seq '(1 2 3 4)))
                   (loop (seq-ref i my-seq)
                         (do (setf i 7)))
                   (return my-seq)) ; => '(7 7 7 7)
  ```

#### Accumulation Commands

Unlike in `cl-loop`, the presence of an accumulation command does not imply a
return value.  You must provide a variable in which to store the accumulated
value.  If you wish, you can then return the value of that variable (either
early, or after the loop).

- `(append VAR EXPR)`: Repeatedly `append` the value of `EXPR` to `VAR`.  `VAR`
  starts as `nil`.

  ``` elisp
  (loopy ((list i '((1 2 3) (4 5 6)))
          (append coll i))
         (return coll)) ; => '(1 2 3 4 5 6)
  ```

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
  command like in its example below.

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

- `(push|push-into VAR EXPR)`: Repeatedly `push` `EXPR` into `VAR`. `VAR` stars
  as `nil`.

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

Conditional commands in `loopy` can take multiple sub-commands, and work more
like their Lisp counterparts.  There is therefore no need for an `and` command
as used in `cl-loop`.

- `(when EXPR CMDS)`: Like the Lisp `when`, run `CMDS` only if `EXPR` is
  non-nil.

  ```elisp
  ;; Get only the inner lists with all even numbers.
  ;; => '((2 4 6) (8 10 12) (16 18 20))
  (loopy ((list i '((2 4 6) (8 10 12) (13 14 15) (16 18 20)))
          (when (loopy ((list j i)
                        (when (cl-oddp j)
                          (return nil)))
                        (else-do (cl-return t)))
            (collect only-evens i)))
         (finally-return only-evens))
  ```

- `(if EXPR CMDS)`: Like the Lisp `if`, run the first command if `EXPR` is
  non-nil. Otherwise, run the remaining commands.

  ```elisp
  ;; => '((7 5 3 1) (6 4 2) (3 3 3))
  (loopy ((seq i [1 2 3 4 5 6 7])
          (if (cl-oddp i)
              (push-into reversed-odds i)
            (push-into reversed-evens i)
            (push-into some-threes 3)))
            (finally-return (list reversed-odds
                                  reversed-evens
                                  some-threes)))
   ```

- `(cond [(EXPR CMDS) [...]])`: Like the Lisp `cond`, for the first `EXPR` to
  evaluate to non-nil, run the following commands `CMDS`.

  ```elisp
  (loopy ((list i '(1 2 3 "cat" 4 5 6 "dog"))
          (cond
           ((not (numberp i)) (collect not-numbers i))
           ((cl-evenp i)      (collect evens i))
           (t                 (collect odds i))))
         (return evens odds not-numbers)) ; => '((2 4 6) (1 3 5) ("cat" "dog"))
  ```

##### Skipping an Iteration

- `(skip|continue)`: Go to next loop iteration.

  ```elisp
  (loopy ((seq i (number-sequence 1 20))
          (when (zerop (mod i 10)) (skip))
          (when (cl-evenp i)       (push-into my-collection i)))
         (finally-return (nreverse my-collection))) ; => (2 4 6 8 12 14 16 18)
  ```

##### Exiting the Loop Early

The loop is contained in a `cl-block`, and these forms are all variations of
`cl-return-from` underneath.  In fact, you could use `(do (cl-return-from NAME
VAL))` to achieve the same effect.  These forms are provided for convenience.

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

## Adding Custom Commands

### Background Information

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

| `cl-loop`                                     | `loopy`                                          |
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
| `for VAR = EXPR1 then EXPR2`                  | `(expr VAR EXPR1 EXPR2)`                         |

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
        ;; `return` is just a wrapper for `cl-return`.
        (when (< size 10) (return (some-small-sizes))))
       ;; Only runs if loop doesn't exit early.
       (after-do (cl-return (only-big-sizes))))
```

A seen in the above example, `loopy` does not always have a one-to-one
translation to `cl-loop` (though you might want to try creating your own
command).  It is not an explicit goal to be able to replace all uses of
`cl-loop` with `loopy`.  In the above example, you could be better served by
`cl-every`, `seq-every-p`, or even continuing to use `cl-loop`.

### Accumulation Clauses

**NOTE**: In `loopy`, accumulation commands do not imply a return value.  You
cannot simply do `(collect FORM)`; you must always give a variable into which to
accumulate the form.

| `cl-loop`                 | `loopy`              |
|:--------------------------|:---------------------|
| `append FORM into VAR `   | `(append VAR FORM)`  |
| `collect FORM into VAR `  | `(collect VAR FORM)` |
| `concat FORM into VAR `   | `(concat VAR FORM)`  |
| `count FORM into VAR `    | `(count VAR FORM)`   |
| `maximize FORM into VAR ` | `(max VAR FORM)`     |
| `minimize FORM into VAR ` | `(min VAR FORM)`     |
| `nconc FORM into VAR `    | `(nconc VAR FORM)`   |
| `sum FORM into VAR `      | `(sum VAR FORM)`     |
| `vconcat FORM into VAR `  | `(vconcat VAR FORM)` |

### Other Clauses

In `loopy`, `if`, `when`, and `unless` can take multiple loop commands as
arguments, and operate more like their Lisp counterparts.

This means that `if` is not a synonym for `when`.  Just like the normal Lisp
special form `if`, `(if COND cmd1 cmd2 cmd3)` only runs `cmd1` if `COND`
evaluates to non-nil, and only runs commands `cmd2` and `cmd3` if `COND`
evaluates to `nil`.

`loopy` also provides the command `cond`, which works like the normal Lisp
special form `cond`.

| `cl-loop`              | `loopy`                                     |
|:-----------------------|:--------------------------------------------|
| `with var = value`     | `(with (VAR VALUE))` as a macro argument    |
| `if COND clause`       | `(if COND CMDS)` as a loop command          |
| `when COND clause`     | `(when COND CMDS)` as a loop command        |
| `unless COND clause`   | `(unless COND CMDS)` as a loop command      |
| `named NAME`           | `NAME` as a macro argument                  |
| `initially [do] EXPRS` | `(before-do EXPRS)` as a macro argument     |
| `finally [do] EXPRS`   | `(finally-do EXPRS)` as a macro argument    |
| `finally return EXPR`  | `(finally-return EXPR)` as a macro argument |
| `do EXPR`              | `(do EXPRS)` as a loop command              |
| `return EXPR`          | `(return EXPR)` as a loop command           |

[cl-lib.el]: <https://www.gnu.org/software/emacs/manual/html_node/cl/index.html>
[cl-loop]: <https://www.gnu.org/software/emacs/manual/html_node/cl/Loop-Facility.html>
[seq.el]: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html>
[sequence-docs]: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html>

<!-- Would normally would grab H1 level, but we're using that for the title. -->
<!-- Local Variables: -->
<!-- flycheck-disabled-checkers: (proselint) -->
<!-- End: -->
