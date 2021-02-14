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

(defun loopy--create-destructured-assignment-dash
    (var value-expression)
  "Create a list of instructions for using Dash's destructuring in `loopy'."
  (let ((destructurings (dash--match var value-expression)))
    `(,@(--map `(loopy--explicit-vars . (,(car it) nil))
               destructurings)
      (loopy--main-body . (setq ,@(-flatten-n 1 destructurings))))))


(defun loopy--flag-setup-dash ()
  "Make this `loopy' loop use Dash destructuring."
  (setq loopy--destructuring-function
        #'loopy--create-destructured-assignment-dash))

(with-eval-after-load 'loopy
  (add-to-list 'loopy--flags-setup
               (cons 'dash #'loopy--flag-setup-dash)))

(provide 'loopy-dash)
;;; loopy-dash.el ends here
