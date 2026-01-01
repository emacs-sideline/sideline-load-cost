;;; sideline-load-cost.el --- Display load/require module size with sideline  -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-sideline/sideline-load-cost
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (sideline "0.1.0"))
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

(require 'benchmark)
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
;;; Externals

(defvar features)

;;
;;; Util

(defun sideline-load-cost--comment-p ()
  "Return non-nil if it's inside comment or string."
  (nth 4 (syntax-ppss)))

(defun sideline-load-cost--file-size (filename)
  "Return the FILENAME size."
  (when-let* (((file-readable-p filename))
              (attributes (file-attributes filename)))
    (file-attribute-size attributes)))

(defun sideline-load-cost--find-size (filename &optional ext)
  "Return FILENAME's (extension . size).

Optional argument EXT is use to drop-in replace the current extension."
  (when-let* ((filename (if ext (file-name-sans-extension filename)
                          filename))
              (filename (concat filename ext))
              (size (sideline-load-cost--file-size filename)))
    (cons (or ext (file-name-extension filename))
          size)))

;;
;;; Standard

(defvar sideline-load-cost--standard-time
  (benchmark-run 1 (let ((features)) (require 'sideline)))
  "Standard time use to measure average load time.")

(defvar sideline-load-cost--standard-file-size
  (let ((lib (find-library-name "sideline")))
    (sideline-load-cost--file-size lib))
  "Standard time use to measure average load time.")

(defun sideline-load-cost--measure-load-time (size)
  "Measure load time cost by file's SIZE."
  (* (/ (float size) sideline-load-cost--standard-file-size)
     (car sideline-load-cost--standard-time)))

;;
;;; Prefix

(defconst sideline-load-cost--load-names '("require" "load" "load-file")
  "List of load operations.")

(defun sideline-load-cost--statement-p (line)
  "Return non-nil if LINE has the load operations."
  (cl-some (lambda (op)
             (and (string-prefix-p (concat "(" op " ") line)
                  op))
           sideline-load-cost--load-names))

;;
;;; Core

(defun sideline-load-cost--find-cost ()
  "Return the cost text."
  (when-let* (((and (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                    (not (sideline-load-cost--comment-p))))
              (line (thing-at-point 'line))
              (line (string-trim line))
              (op (sideline-load-cost--statement-p line))
              (bol (line-end-position)))
    (save-excursion
      (beginning-of-line)
      (when (search-forward op bol t)
        (forward-word 1)
        (when-let* (((<= (point) bol))
                    (thing (or (ignore-errors (thing-at-point 'string))
                               (ignore-errors (thing-at-point 'symbol))))
                    (thing (if (stringp thing)
                               (sideline--s-replace "\"" "" thing)
                             thing))
                    (filename
                     (cond
                      ((and (stringp thing) (file-exists-p thing))
                       (expand-file-name thing))
                      (t (ignore-errors (find-library-name (format "%s" thing)))))))
          (let* ((size-data (or (sideline-load-cost--find-size filename ".el.gz")
                                (sideline-load-cost--find-size filename ".elc")
                                (sideline-load-cost--find-size filename ".el")
                                (sideline-load-cost--find-size filename)))
                 (size (cdr size-data))
                 (ext (car size-data))
                 (text-size (and size
                                 (format "%s: %s" ext (file-size-human-readable size))))
                 (time (sideline-load-cost--measure-load-time size))
                 (text-time (format "(%.2fs)" time)))
            (concat text-size " " text-time)))))))

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
