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

(defun loopy--dash-vars-to-list (sequence)
  "Return a list (maybe of sublists) of elements in sequence.

Needed since `-flatten' doesn't work on vectors."
  (cond
   ((symbolp sequence) sequence)
   ((consp sequence) sequence)
   (t
    (seq-map #'loopy--dash-get-vars sequence))))

(cl-defun loopy--parse-accumulation-commands-dash ((name var val))
  "Parse the accumulation loop commands, like `collect', `append', etc.

NAME is the name of the command.  VAR-OR-VAL is a variable name
or, if using implicit variables, a value .  VAL is a value, and
should only be used if VAR-OR-VAL is a variable."
  (let ((var-list (-flatten (loopy--dash-vars-to-list var))))
    `(,@(-mapcat
         (-lambda ((sub-var sub-val))
           ;; Only apply accumulation to named sub-variables.  If symbol is
           ;; a Dash source, don't accumulate, just let through.
           (if (memq sub-var var-list)
               `((loopy--explicit-vars
                  . (,sub-var ,(cl-case name
                                 ((sum count)    0)
                                 ((max maximize) -1.0e+INF)
                                 ((min minimize) +1.0e+INF))))
                 (loopy--main-body
                  . ,(cl-ecase name
                       (append `(setq ,sub-var (append ,sub-var ,sub-val)))
                       (collect `(setq ,sub-var (append ,sub-var (list ,sub-val))))
                       (concat `(setq ,sub-var (concat ,sub-var ,sub-val)))
                       (vconcat `(setq ,sub-var (vconcat ,sub-var ,sub-val)))
                       (count `(if ,sub-val (setq ,sub-var (1+ ,sub-var))))
                       ((max maximize) `(setq ,sub-var (max ,sub-var ,sub-val)))
                       ((min minimize) `(setq ,sub-var (min ,sub-var ,sub-val)))
                       (nconc `(setq ,sub-var (nconc ,sub-var ,sub-val)))
                       ((push-into push) `(push ,sub-val ,sub-var))
                       (sum `(setq ,sub-var (+ ,sub-var ,sub-val)))))
                 (loopy--implicit-return . ,sub-var))
             `((loopy--implicit-vars . (,sub-var nil))
               (loopy--main-body . (setq ,sub-var ,sub-val)))))
         ;; Get Dash's destructuring, and feed to the lambda.
         (dash--match var val)))))

(provide 'loopy-dash)
;;; loopy-dash.el ends here
