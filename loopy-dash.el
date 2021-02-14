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
  (let* ((var-list (-flatten (loopy--dash-vars-to-list var)))
         (copied-var-list (--map (cons it (gensym (format "copy-%s-" it)))
                                 var-list))
         (destructurings (dash--match var val)))
    (remq
     nil
     `(,@(-mapcat (-lambda ((var _))
                    (when (memq var var-list)
                      (let ((copy-name (cdr (assq var copied-var-list))))
                        `((loopy--implicit-vars . (,copy-name nil))
                          (loopy--explicit-vars . (,var
                                                   ,(cl-case name
                                                      ((sum count)    0)
                                                      ((max maximize) -1.0e+INF)
                                                      ((min minimize) +1.0e+INF))))
                          (loopy--implicit-return . ,var)
                          ;; Copy of the value of var.
                          (loopy--main-body . (setq ,copy-name ,var))))))
                  destructurings)

       ;; Let Dash overwrite the values it wishes.
       (loopy--main-body . (setq ,@(-flatten-n 1 destructurings)))

       ;; Restore those values when this command completes.
       ,@(-map (-lambda ((var _))
                 (when (memq var var-list)
                   `(loopy--main-body
                     . ,(cl-ecase name
                          (append `(setq ,var (append ,(cdr (assq var copied-var-list)) ,var)))
                          (collect `(setq ,var (append ,(cdr (assq var copied-var-list)) (list ,var))))
                          (concat `(setq ,var (concat ,(cdr (assq var copied-var-list)) ,var)))
                          (vconcat `(setq ,var (vconcat ,(cdr (assq var copied-var-list)) ,var)))
                          (count (setq ,var (if var
                                                (1+ ,(cdr (assq var copied-var-list)))
                                              ,(cdr (assq var copied-var-list)))))
                          ((max maximize) `(setq ,var (max ,(cdr (assq var copied-var-list)) ,var)))
                          ((min minimize) `(setq ,var (min ,(cdr (assq var copied-var-list)) ,var)))
                          (nconc `(setq ,var (nconc ,(cdr (assq var copied-var-list)) ,var)))
                          ((push-into push) `(setq ,var (push ,var ,(cdr (assq var copied-var-list)))))
                          (sum `(setq ,var (+ ,(cdr (assq var copied-var-list)) ,val)))))))
               destructurings)))))

(provide 'loopy-dash)
;;; loopy-dash.el ends here
