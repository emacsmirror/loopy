# Loopy: An Emacs Looping Library

Loopy is a macro meant for iterating and looping. It is similar in usage to
`cl-loop` but uses symbolic expressions rather than keywords.

The expressions generally follow the form `(COMMAND VARIABLE-NAME &rest ARGS)`.

- To iterate through a [sequence][sequence-docs], use `(seq elem source-seq)`
  (for efficiency, there are also more specific commands, like `list`).
- To declare values before the loop use `(with (VAR VAL) [(VAR VAL) ...])`. This
  binds in the order given, like a `let*` binding (which it is underneath).
- To return values after the loop, use `(finally-return VAL [VAL ...])`.
  Returning multiple values is the same as returning a list of those values
  (`(finally-return (list VAL [VAL ...]))`).
- To do final processing after the loop, but before the final return, use
  `(finally-do EXPR [EXPR ...])`.

The loop body is wrapped in a `cl-block`, and can be exited, among other ways,
with the specific command `(leave-named-loop NAME [VAL])`. Underneath, the loop
is just a `while` loop whose continue condition is generated from the Loopy
commands you use.

Below is a full example of the arguments of the `loopy` macro. The top-level
forms have a flexible-order, but meaning is clearest if they have the following
order. All of the arguments are technically optional, but having a loop without
a body wouldn't be very useful.

``` elisp
(loopy (with WITH-ARGS)
       (BODY-COMMAND [BODY-COMMAND ...])
       (finally-do EXPR [EXPR ...])
       (finally-return VAL [VAL ...]))
```

The general idea has been developed, but the current lack of convenience
features means that this library is still in the early stages. Here are some
things that would be nice:

- Accumulation clauses that are supported by `cl-loop`. `with` and
  `expr` covers this, but it could be more convenient.
- Iteration clauses that are supported by `cl-loop`. `seq` can iterate
  through sequences, but `cl-loop` does more.
- Have other conditionals like `unless`, `etc`.

## How does it compare to other approaches?

Loopy is similar in use to other looping libraries, except for its lack of
convenience features and lack of extensibility.

Below is an example of `loopy` vs `cl-loop`.

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
statements under a `when` condition. For example, below is something I would
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

            ;; ... Further processing now that data is named ...

            (prepend line-nums line-num)
            (prepend data-nums datum-num)))
         (finally-return (nreverse line-nums) (nreverse data-nums))))

;; The `loopy' expression currently expands to the below.
(let ((g1068 (split-string "Line1-Data1\nBad\nLine2-Data2"))
      (line nil)
      (line-num nil)
      (datum-num nil)
      (line-nums nil)
      (data-nums nil))
  (let* ((pattern "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)"))
    (cl-block nil
      (while (and g1068)
        (cl-tagbody
         (progn
           (setq line (pop g1068))
           (when (string-match pattern line)
             (setq line-num (concat "L" (match-string 1 line)))
             (setq datum-num (concat "D" (match-string 2 line)))
             (push line-num line-nums)
             (push datum-num data-nums)))
         continue-tag))
      nil
      (cl-return-from nil (list (nreverse line-nums) (nreverse data-nums))))))
```

The expansion shows that there is room for improvement, but it's a nice start,
is easy to read, and does what I want. I believe that the value of the macro
increases for longer loop bodies with several conditional commands.

Another nice ability is skipping/continuing a loop iteration.

``` elisp
;; Returns even numbers that aren't multiples of 10.
(loopy ((list i (number-sequence 1 20))
        (when (zerop (mod i 10))
          (skip))
        (when (cl-evenp i)
          (prepend my-collection i)))
       (finally-return (nreverse my-collection)))

;; Expands to:
(let ((g970 (number-sequence 1 20))
      (my-collection nil))
  (let* ((_))
    (cl-block nil
      (while (and g970)
        (cl-tagbody
         (progn
           (setq i (pop g970))
           (when (zerop (mod i 10))
             (go continue-tag))
           (when (cl-evenp i)
             (push i my-collection)))
         continue-tag))
      nil
      (cl-return-from nil (nreverse my-collection)))))
```

This expansion is probably less efficient than what `cl-loop` does.


## How to use

There are 4 possible arguments to the `loopy` macro:

1. A name for the loop.
2. A list of declarations using `with` or `let*`. These are evaluated in order
   as in `let*`, and are set before running the loop body.
3. A list of iterations and expressions for the loop body. A loop is
   infinite unless a clause makes it not so.
4. A final return statement, like `finally return` in `cl-loop`. The
    loop always returns `nil` unless declared otherwise.

Parts 1, 2, and 4 are optional. Part 3 is recommended.

An expression starts with a command, followed by arguments if needed.

A generic example is

``` elisp
(loopy (with (first-var 2)
             (second-var 3))
       ((seq el [1 2 3 4 5 6 7])
        ;; Could also use (do (cond ...)).
        (when (zerop (mod el first-var))
          (do (message "Multiple of 2: %d" el)))
        (when (zerop (mod el second-var))
          (do (message "Multiple of 3: %d" el)))
        (prepend reversed el))
       (finally-return reversed))
```

### Generic
- `(do SEXPS)`:   Evaluate multiple sexps, like a `progn`.

  ```elisp
  (loopy ((list i '(1 2 3))
          (do (message "%d" i))))
  ```

### Assignment Before Loop

- `(with|let* (SEXPS))`:   Bind `SEXPS` as if in a `let*` binding.

   ```elisp
   (loopy (with (a 5) (b 6))
          ((list i '(1 2 3))
           (do (message "%d" (+ a b i)))))
   ```

### Assignment and iteration

- `(expr var val)`: Bind `var` to expression `val` in each iteration.

  ```elisp
  (loopy ((list i '(1 2 3))
          (expr j (* i 2))
          (do (message "%d" j))))
  ```

- `(seq var val)`:  Iterate through the sequence `val`, binding each element to
  `var`. The loop ends when the sequence is empty.

  ```elisp
  (loopy ((seq i [1 2 3])
          (do (message "%d" i))))
  ```

Accumulation (for Convenience)

- `(prepend var val)`: Repeatedly `push` `val` into `var`.

  ```elisp
  (loopy ((seq i [1 2 3])
          (prepend reversed i))
         (finally-return reversed))
  ```

Conditionals:

- `(when COND SEXPS)`: Conditionally run binding `SEXPS`. A sexp can be one of
  the body forms in this list.

  ```elisp
  (loopy ((seq i [1 2 3])
          (when (cl-oddp i)
            (prepend reversed-odds i)))
         (finally-return reversed-odds))
  ```

- `(if COND SEXPS)`: Like an `if` body.

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

- `(cond )`: Like a `cond`. Use for IF-ELIF-ELSE things.

  ```elisp
        (loopy ((list i (number-sequence 1 10))
                (cond
                 ((cl-evenp i)
                  (prepend evens i))
                 (t (prepend odds i))))
               (finally-return (list evens odds)))
        ```

Skipping or leaving the loop:

- `(skip|continue)`: Go to next loop iteration. Can be `(skip)` or `(continue)`.

  ```elisp
  (loopy ((seq i (number-sequence 1 20))
          (when (zerop (mod i 10))
            (skip))
          (when (cl-evenp i)
            (prepend my-collection i)))
         (finally-return (nreverse my-collection)))
  ```

- `(return|leave|break)`:   Leave the current loop with an optional return value.

  ```elisp
  (loopy ((with j 0))
         ((do (cl-incf j))
          (when (> j 5)
            (return j))))
  ```

- `(return-with|leave-with|break-with)`: Leave current loop and return a
  value. Unlike the above, the return value is required.

  ```elisp
  (loopy ((with j 0))
         ((do (cl-incf j))
          (when (> j 5)
            (return-with j))))
  ```

- `(leave-named-loop name val)`: Leave the loop named `name` (as with
  `cl-return-from`)

  ```elisp
  (loopy
   outer ; Don't quote name.
   ((list outer-i (number-sequence 1 10))
    (expr ret-loop
          (loopy inner
                 ((expr inner-sum (+ outer-i 10))
                  (when (> inner-sum 15)
                    ;; Don't quote name.
                    (leave-named-loop outer outer-i))
                  ;; Note: Without explicit return, inner loop is
                  ;; infinite.
                  (return))))))
  ```

The last category could be cleaned up a bit.




[sequence-docs]: <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html>
