;;; sideline-load-cost.el --- Display load/require module size with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline-load-cost
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (sideline "0.1.0"))
;; Keywords: help

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Display load/require module size with sideline.
;;
;; 1) Add sideline-load-cost to sideline backends list,
;;
;;   (setq sideline-backends-right '(sideline-load-cost))
;;
;; 2) Then enable sideline-mode in the target buffer,
;;
;;   M-x sideline-mode
;;

;;; Code:

(require 'find-func)

(require 'sideline)

(defgroup sideline-load-cost nil
  "Display load/require module size with sideline."
  :prefix "sideline-load-cost-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/emacs-sideline/sideline-load-cost"))

(defface sideline-load-cost
  '((t :foreground "#D14D3B"))
  "Face for load cost in sideline."
  :group 'sideline-load-cost)

;;
;;; Util

(defun sideline-load-cost--replace (old new s)
  "Replace OLD with NEW in S."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun sideline-load-cost--inside-comment-or-string-p ()
  "Return non-nil if it's inside comment or string."
  (or (nth 4 (syntax-ppss))
      (nth 8 (syntax-ppss))))

(defun sideline-load-cost--file-size (filename)
  "Return the FILENAME size."
  (when-let (((file-readable-p filename))
             (attributes (file-attributes filename)))
    (file-attribute-size attributes)))

(defun sideline-load-cost--find-size-el (filename)
  "Return FILENAME's el size."
  (let ((filename (sideline-load-cost--replace ".elc" ".el" filename)))
    (sideline-load-cost--file-size filename)))

(defun sideline-load-cost--find-size-elc (filename)
  "Return FILENAME's elc size."
  (let ((filename (sideline-load-cost--replace ".el" ".elc" filename)))
    (sideline-load-cost--file-size filename)))

(defun sideline-load-cost--find-size-elc-gz (filename)
  "Return FILENAME's elc.gz size."
  (let ((filename (sideline-load-cost--replace ".el" ".elc.gz" filename)))
    (sideline-load-cost--file-size filename)))

;;
;;; Prefix

(defconst sideline-load-cost--load-names '("require" "load" "load-file")
  "List of load operations.")

(defun sideline-load-cost--statement-p (line)
  "Return non-nil if LINE has the load operations."
  (cl-some (lambda (op)
             (and (string-prefix-p (concat "(" op) line)
                  op))
           sideline-load-cost--load-names))

;;
;;; Core

(defun sideline-load-cost--find-cost ()
  "Return the cost text."
  (when-let (((and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                   (not (sideline-load-cost--inside-comment-or-string-p))))
             (line (thing-at-point 'line))
             (op (sideline-load-cost--statement-p line))
             (bol (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (when (search-forward op bol t)
        (forward-word 1)
        (when-let* (((<= (point) bol))
                    (thing (or (thing-at-point 'string)
                               (thing-at-point 'symbol)))
                    (filename (ignore-errors (find-library-name (format "%s" thing)))))
          (let* ((size-el (sideline-load-cost--find-size-el filename))
                 (size-elc (sideline-load-cost--find-size-elc filename))
                 (size-elc-gz (sideline-load-cost--find-size-elc-gz filename))
                 (text-el (and size-el
                               (format "el: %s" (file-size-human-readable size-el))))
                 (text-elc (and size-elc
                                (format "elc: %s" (file-size-human-readable size-elc))))
                 (text-elc-gz (and size-elc-gz
                                   (format "elc.gz: %s" (file-size-human-readable size-elc-gz))))
                 (display-list (cl-remove-if #'null (list text-el text-elc text-elc-gz)))
                 (text (mapconcat #'identity display-list " / ")))
            text))))))

;;;###autoload
(defun sideline-load-cost (command)
  "Backend for sideline.

Argument COMMAND is required in sideline backend."
  (cl-case command
    (`candidates (cons :async #'sideline-load-cost--show))))

(defun sideline-load-cost--show (callback &rest _)
  "Execute CALLBACK to display with sideline."
  (when-let* ((text (sideline-load-cost--find-cost)))
    (add-face-text-property 0 (length text) 'sideline-load-cost nil text)
    (funcall callback (list text))))

(provide 'sideline-load-cost)
;;; sideline-load-cost.el ends here
