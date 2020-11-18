(require 'selectrum)
(defvar selectrum-should-sort-p)
(defvar selectrum-mode)
(declare-function selectrum-read "ext:selectrum")
(autoload 'selectrum-read "ext:selectrum")


(defun selectrum-swiper-loopy ()
  "Search for a matching line and jump to the beginning of its text.  Obeys narrowing."
  (interactive)
  (let ( selectrum-should-sort-p default-candidate formatted-candidates
         (current-line-number (line-number-at-pos (point) t)))
    (loopy (with (buffer-text-lines (split-string (buffer-string) "\n"))
                 (number-format
                  (concat "L%0"
                          (number-to-string (length (number-to-string (length buffer-text-lines))))
                          "d: ")))
           (loop (list line-text buffer-text-lines)
                 (expr line-num (line-number-at-pos (point-min) t) (1+ line-num))
                 (unless (string-empty-p line-text)
                   (push-into formatted-lines
                              (propertize line-text
                                          'selectrum-candidate-display-prefix
                                          (propertize (format number-format line-num)
                                                      'face 'completions-annotations)
                                          'line-num line-num))
                   ;; There are a few different ways that you could express this.
                   (when (null default-cand)
                     (expr prev-dist +1.0e+INF dist-to-default-cand)
                     (expr dist-to-default-cand (abs (- current-line-number
                                                        line-num)))
                     (when (> dist-to-default-cand prev-dist)
                       (expr default-cand (cadr formatted-lines))))))
           ;; Could also use `cl-multiple-value-bind' and `finally-return',
           ;; which has the benefit of not being captured by the loop's
           ;; `let'-forms.
           (finally-do (setq default-candidate default-cand
                             formatted-candidates (nreverse formatted-lines))))
    (let ((chosen-line-number
           (get-text-property 0 'line-num
                              (selectrum-read "Jump to matching line: "
                                              formatted-candidates
                                              :default-candidate default-candidate
                                              :history 'selectrum-swiper-history
                                              :require-match t
                                              :no-move-default-candidate t))))
      (push-mark (point) t)
      (forward-line (- chosen-line-number current-line-number))
      (beginning-of-line-text 1))))


(defun selectrum-outline-loopy ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  ;; Signal a `user-error' if we don't have a regexp for this major mode.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (let (selectrum-should-sort-p
            (current-line-number (line-number-at-pos (point) t)))
        (save-match-data
          (cl-multiple-value-bind (default-candidate formatted-candidates)
              (loopy
               (with (buffer-lines (split-string (buffer-string) "\n"))
                     (line-number-format
                      (concat "L%0"
                              (number-to-string
                               (length (number-to-string (length buffer-lines))))
                              "d: ")))
               (loop (expr line-number 1 (1+ line-number))
                     (list text-line buffer-lines)
                     (when (string-match heading-regexp text-line)
                       (expr heading-text
                             (match-string-no-properties 2 text-line))
                       (expr heading-level
                             (length (match-string-no-properties 1 text-line)))
                       (cond ((> heading-level (or prev-heading-level
                                                   heading-level))
                              (push-into backwards-prefix-list
                                         prev-heading-text))
                             ((< heading-level (or prev-heading-level
                                                   heading-level))
                              (expr backwards-prefix-list
                                    (last backwards-prefix-list
                                          heading-level))))

                       (expr prev-heading-text heading-text)
                       (expr prev-heading-level heading-level)

                       (when (and (null default-heading)
                                  (> (- line-number current-line-number) 0))
                         (expr default-heading (car formatted-headings)))

                       (push-into
                        formatted-headings
                        (propertize
                         (concat (string-join (reverse backwards-prefix-list) "/")
                                 (and backwards-prefix-list "/")
                                 heading-text)
                         'line-number line-number
                         'selectrum-candidate-display-prefix
                         (propertize (format line-number-format line-number)
                                     'face 'completions-annotations)))))
               (finally-return default-heading (nreverse formatted-headings)))
            (let ((chosen-heading
                   (selectrum-read "Jump to heading: "
                                   formatted-candidates
                                   :default-candidate default-candidate
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
              (beginning-of-line-text 1)))))
    (user-error "selectrum-outline: No headings defined for %s." major-mode)))


;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
