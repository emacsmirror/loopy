;;; loopy-dash.el --- Dash destructuring for `loopy' -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (loopy "0.4") (dash "2"))
;; Keywords: extensions
;; LocalWords:  Loopy's emacs

;;; Disclaimer:
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; `loopy' is a macro that is used similarly to `cl-loop'.  It provides "loop
;; commands" that define a loop body and it's surrounding environment, as well
;; as exit conditions.
;;
;; For more information, see Loopy’s README.org or the Info files derived from
;; that README with the command `info'.
;;
;; This package provides extra functions to use Dash's destructuring abilities
;; in Emacs.

;;; Code:
(require 'loopy)
(require 'dash)
(require 'cl-lib)

(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--destructuring-for-with-vars-function)
(defvar loopy--flag-settings nil)

;;;###autoload
(defun loopy-dash--enable-flag-dash ()
  "Make this `loopy' loop use Dash destructuring."
  (setq
   loopy--destructuring-for-iteration-function
   #'loopy-dash--destructure-for-iteration
   loopy--destructuring-for-with-vars-function
   #'loopy-dash--destructure-for-with-vars
   loopy--destructuring-accumulation-parser
   #'loopy-dash--parse-destructuring-accumulation-command))

(defun loopy-dash--disable-flag-dash ()
  "Make this `loopy' loop use Dash destructuring."
  (if (eq loopy--destructuring-for-iteration-function
          #'loopy-dash--destructure-for-iteration)
      (setq loopy--destructuring-for-iteration-function
            #'loopy--destructure-for-iteration-default))
  (if (eq loopy--destructuring-for-with-vars-function
          #'loopy-dash--destructure-for-with-vars)
      (setq loopy--destructuring-for-with-vars-function
            #'loopy--destructure-for-with-vars-default))
  (if (eq loopy--destructuring-accumulation-parser
          #'loopy-dash--parse-destructuring-accumulation-command)
      (setq loopy--destructuring-accumulation-parser
            #'loopy--parse-destructuring-accumulation-command)))

(add-to-list 'loopy--flag-settings (cons 'dash #'loopy-dash--enable-flag-dash))
(add-to-list 'loopy--flag-settings (cons '+dash #'loopy-dash--enable-flag-dash))
(add-to-list 'loopy--flag-settings (cons '-dash #'loopy-dash--disable-flag-dash))

;;;; The actual functions:
(defun loopy-dash--destructure-for-with-vars (bindings)
  "Return a way to destructure BINDINGS as if by `-let*'.

Returns a list of two elements:
1. The symbol `-let*'.
2. A new list of bindings."
  (list '-let* bindings))

(defun loopy-dash--destructure-for-iteration (var val)
  "Destructure VAL according to VAR as if by `-let'.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (let ((bindings (dash--match var val)))
    (list (cons 'setq (apply #'append bindings))
          ;; Note: This includes the named variables and the needed generated
          ;;       variables.
          (mapcar #'car bindings))))

(defvar loopy-dash--accumulation-destructured-symbols nil
  "The names of copies of variable names that Dash will destructure.

Each element is `(old-name . new-name)', where new-name is based
on old name.  For example, `(i . loopy--copy-i-378)', where `i'
is the name explicitly given by the user and the copy is what
Dash assigns to when destructuring.

See `loopy-dash--transform-var-list'.")

(defun loopy-dash--transform-var-list (var-list)
  "Get a new VAR-LIST same structure but different variable names.

This function will push transformations to
`loopy-dash--accumulation-destructured-symbols'.

For accumulation, we don't want Dash to assign to the named
  variables, so we pass it this instead."
  (cl-typecase var-list
    (symbol
     ;; Don't create copies for symbols like "&as", ":foo", or "_".
     (if (string-match-p (rx (or "&" "|" (seq string-start "_" string-end)))
                         (symbol-name var-list))
         var-list
       (let ((dash-copy (gensym (format "loopy--copy-%s-" var-list))))
         (push (cons var-list dash-copy)
               loopy-dash--accumulation-destructured-symbols)
         dash-copy)))
    (list
     (let ((copied-var-list))
       (while (car-safe var-list)
         (push (loopy-dash--transform-var-list (pop var-list))
               copied-var-list))
       (let ((correct-order (reverse copied-var-list)))
         ;; If it was a dotted list, then `var-list' is still non-nil.
         (when var-list
           (setcdr (last correct-order)
                   (loopy-dash--transform-var-list var-list)))
         correct-order)))
    (array
     (cl-map 'vector #'loopy-dash--transform-var-list var-list))))

(cl-defun loopy-dash--parse-destructuring-accumulation-command ((name var val))
  "Parse the accumulation loop commands, like `collect', `append', etc.

NAME is the name of the command.  VAR-OR-VAL is a variable name
or, if using implicit variables, a value .  VAL is a value, and
should only be used if VAR-OR-VAL is a variable."
  (let* (;; An alist of (given-name . dash-copy).  We let Dash produce the
         ;; bindings it needs, then copy those values into the explicitly
         ;; given variables.
         (loopy-dash--accumulation-destructured-symbols nil)
         ;; The new variable list that Dash will destructure:
         (copied-var-list (loopy-dash--transform-var-list var))
         ;; The bindings produced by Dash's destructuring on those
         ;; new variable names:
         (destructurings (dash--match copied-var-list val)))

    ;; Make sure variables are in order of appearance for the loop body.
    (setq loopy-dash--accumulation-destructured-symbols
          (reverse loopy-dash--accumulation-destructured-symbols))

    `(;; Bind the variables that Dash uses for destructuring to nil.
      ,@(--map `(loopy--accumulation-vars . (,(car it) nil))
               destructurings)
      ;; Let Dash perform the destructuring on the copied variable names.
      (loopy--main-body . (setq ,@(-flatten-n 1 destructurings)))
      ;; Accumulate the values of those copied variable names into the
      ;; explicitly given variables.
      ;;
      ;; This gives instructions for the main body, the implicit result,
      ;; and the explicitly named accumulation vars.
      ,@(mapcan (-lambda ((given-var . dash-copy))
                  (loopy--parse-accumulation-commands
                   (list name given-var dash-copy)))
                loopy-dash--accumulation-destructured-symbols))))

(provide 'loopy-dash)
;;; loopy-dash.el ends here
