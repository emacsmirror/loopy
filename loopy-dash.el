;;; loopy-dash.el --- Dash support for `loopy'. -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (loopy "0.1") (dash "2"))
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
(require 'dash)

(defvar loopy--destructuring-function)
(defvar loopy--flags-setup)

(defun loopy--flag-setup-dash ()
  "Make this `loopy' loop use Dash destructuring."
  (setq
   loopy--destructuring-function #'loopy--create-destructured-assignment-dash
   loopy--accumulation-parser #'loopy--parse-accumulation-commands-dash))

(with-eval-after-load 'loopy
  (add-to-list 'loopy--flags-setup
               (cons 'dash #'loopy--flag-setup-dash)))

;;;; The actual functions:
(defun loopy--create-destructured-assignment-dash
    (var value-expression)
  "Create a list of instructions for using Dash's destructuring in `loopy'."
  (let ((destructurings (dash--match var value-expression)))
    `(,@(--map `(loopy--explicit-vars . (,(car it) nil))
               destructurings)
      (loopy--main-body . (setq ,@(-flatten-n 1 destructurings))))))

(defvar loopy--dash-accumulation-new-names nil
  "The names of copies of variable names that Dash will destructure.

Each element is `(old-name . new-name)', where new-name is based
on old name.  For example, `(i . loopy--copy-i-378)', where `i'
is the name explicitly given by the user and the copy is what
Dash assigns to when destructuring.

See `loopy--dash-copy-var-list'.")

(defun loopy--dash-copy-var-list (var-list)
  "Get a new VAR-LIST same structure and a correspondence alist.

New list has similarly named variables.

For accumulation, we don't want Dash to assign to the named
  variables, so we pass it this instead."
  (cl-typecase var-list
    (symbol
     (if (string-match-p "\\&\\|:" (symbol-name var-list))
         var-list ; Don't rename symbols like "&as", "&plist", or ":foo".
       (let ((dash-copy (gensym (format "loopy--copy-%s-" var-list))))
         (push (cons var-list dash-copy)
               loopy--dash-accumulation-new-names)
         dash-copy)))
    (list
     (let ((copied-var-list))
       (while (car-safe var-list)
         (push (loopy--dash-copy-var-list (pop var-list))
               copied-var-list))
       (let ((correct-order (nreverse copied-var-list)))
         ;; If it was a dotted list, then `var-list' is still non-nil.
         (when var-list
           (setcdr (last correct-order)
                   (loopy--dash-copy-var-list var-list)))
         correct-order)))
    (array
     (cl-map 'vector #'loopy--dash-copy-var-list var-list))))

(cl-defun loopy--parse-accumulation-commands-dash ((name var val))
  "Parse the accumulation loop commands, like `collect', `append', etc.

NAME is the name of the command.  VAR-OR-VAL is a variable name
or, if using implicit variables, a value .  VAL is a value, and
should only be used if VAR-OR-VAL is a variable."
  (let* ((loopy--dash-accumulation-new-names nil)
         (copied-var-list (loopy--dash-copy-var-list var))
         (destructurings (dash--match copied-var-list val)))

    ;; Make sure variables are in order of appearance for the loop body.
    (setq loopy--dash-accumulation-new-names
          (nreverse loopy--dash-accumulation-new-names))

    `(;; Declare what Dash will assign to as implicit.
      ,@(--map `(loopy--implicit-vars . (,(car it) nil))
               destructurings)
      ;; Declare as explicit what the user actually named.
      ,@(--map `(loopy--explicit-vars . (,(car it) ,(cl-case name
                                                      ((sum count)    0)
                                                      ((max maximize) -1.0e+INF)
                                                      ((min minimize) +1.0e+INF)
                                                      (t nil))))
               ;; Alist of (old-name . new-name)
               loopy--dash-accumulation-new-names)
      ;; Declare the explicitly given variables as implicit returns.
      ,@(--map `(loopy--implicit-return . ,(car it))
               loopy--dash-accumulation-new-names)
      ;; Let Dash perform the destructuring on the copied variable names.
      (loopy--main-body . (setq ,@(-flatten-n 1 destructurings)))
      ;; Accumulate the values of those copied variable names into the
      ;; explicitly given variables.
      ,@(-map (-lambda ((given-var . dash-copy))
                `(loopy--main-body
                  . ,(cl-ecase name
                       (append `(setq ,given-var (append ,dash-copy ,given-var)))
                       (collect `(setq ,given-var (append ,dash-copy (list ,given-var))))
                       (concat `(setq ,given-var (concat ,dash-copy ,given-var)))
                       (vconcat `(setq ,given-var (vconcat ,dash-copy ,given-var)))
                       (count `(if ,dash-copy (setq ,given-var (1+ ,given-var))))
                       ((max maximize) `(setq ,given-var (max ,dash-copy ,given-var)))
                       ((min minimize) `(setq ,given-var (min ,dash-copy ,given-var)))
                       (nconc `(setq ,given-var (nconc ,dash-copy ,given-var)))
                       ((push-into push) `(setq ,given-var (push ,given-var ,dash-copy)))
                       (sum `(setq ,given-var (+ ,dash-copy ,val))))))
              loopy--dash-accumulation-new-names))))

(provide 'loopy-dash)
;;; loopy-dash.el ends here
