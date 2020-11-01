# About

Loopy is a macro meant for iterating and looping. It is similar in usage
to `cl-loop` but uses sexps rather than keywords.

It's in the early stages.

## How does it compare to other approaches?

Currently, it is more limited in many respects. It does not have as many
convenience features (though these can be added) and it's structure is
more limited.

Below is an example. You can see that they are similar.

``` elisp
(require 'cl-lib)
(cl-loop with some-thing = 5
         for i in (number-sequence 1 100)
         do (message "I is %s" i)
         when (> (+ i 5) 20)
         return "Done")

(require 'loopy)
(loopy (with (some-thing 5))
       ((list i (number-sequence 1 100))
        (do (message "I is %s" i))
        (when (> (+ i 5) 20)
          (return "Done"))))
```

The main benefit of Loopy is clearer grouping of constructs under
conditionals while still using a clean syntax, such as in the below
example.

``` elisp
(loopy ((list i (number-sequence 1 20))
        (when (cl-evenp i)
          (expr once i)
          (expr twice (* 2 i))
          (prepend together (cons once twice))))
       (finally-return (nreverse together)))
```

`cl-loop` does not allow the easy grouping of statements under a `when`
condition. Another nice ability is skipping/continuing a loop iteration.

``` elisp
(loopy ((list i (number-sequence 1 20))
        (when (zerop (mod i 10))
          (skip))
        (when (cl-evenp i)
          (prepend my-collection i)))
       (finally-return (nreverse my-collection)))
```

This currently expands readably (more or less) to the below, which is
simple enough to make debugging and relatively easy. Generated variables
are used where needed.

This expansion is probably less efficient than what `cl-loop` does.

``` elisp
(let ((g1270 (number-sequence 1 20))
      (my-collection nil))
  (let* ((_))
    (cl-block nil
      (while (and g1270)
        (cl-tagbody
         (progn
           (setq i (pop g1270))
           (when (zerop (mod i 10))
             (go continue-tag))
           (when (cl-evenp i)
             (push i my-collection)))
         continue-tag))
      (nreverse my-collection))))
```

## How to use

There are 4 possible arguments to the `loopy` macro:

1.  A name for the loop.
2.  A list of declarations using `with` or `let*`. These are evaluated in order
    as in `let*`, and are set before running the loop body.
3.  A list of iterations and expressions for the loop body. A loop is
    infinite unless a clause makes it not so.
4.  A final return statement, like `finally return` in `cl-loop`. The
    loop always returns `nil` unless declared otherwise.

Parts 1, 2, and 4 are optional. Part 3 is recommended.

An expression starts with a command, followed by arguments if needed.
Here is the current list of valid iterations and expressions:

Generic
- `(do SEXPS)`:   Evaluate multiple sexps, like a `progn`.

  ```elisp
  (loopy ((list i '(1 2 3))
          (do (message "%d" i))))
  ```

Assignment Before Loop:

- `(with|let* (SEXPS))`:   Bind `SEXPS` as if in a `let*` binding.

   ```elisp
   (loopy (with (a 5) (b 6))
          ((list i '(1 2 3))
           (do (message "%d" (+ a b i)))))
   ```

Assignment and iteration:

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

- `(cond )`:   Like a `cond`. Use for IF-ELIF-ELSE things.

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

- `(return|leave|break)` :   Leave the current loop with an optional return value.

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

A generic example is

``` elisp
(loopy (with (first-var 2)
             (second-var 3)
             )
       ((seq el [1 2 3 4 5 6 7])
        ;; Could also use (do (cond ...)).
        (when (zerop (mod el first-var))
          (do (message "Multiple of 2: %d" el)))
        (when (zerop (mod el second-var))
          (do (message "Multiple of 3: %d" el)))
        (prepend reversed el))
       (finally-return reversed))
```

## Things to Do

Here are some things that would be nice to have, though `loopy` should already
be able to generally do most things (if not conveniently, syntax-wise).

- Accumulation clauses that are supported by `cl-loop`. `with` and
  `expr` covers this, but it could be more convenient.
- Iteration clauses that are supported by `cl-loop`. `seq` can iterate
  through sequences, but `cl-loop` does more.
- Have other conditionals like `unless`, `etc`.
