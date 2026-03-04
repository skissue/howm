;;; howm-eldoc.el --- Eldoc support for howm ref links -*- lexical-binding: t -*-

;; Author: Ad <me@skissue.xyz>
;; Maintainer: Ad <me@skissue.xyz>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (howm "1.5.6"))
;; Homepage: https://github.com/skissue/howm


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Show a preview of the target note in the eldoc area when point is
;; on a howm ref link (">>> path/to/file").  Only concrete file-path
;; links are supported (expand-file-name + file-exists-p gate).
;;
;; Requires Emacs 28+ (eldoc-documentation-functions).
;;
;; Usage:
;;   (require 'howm-eldoc)
;;   (add-hook 'howm-mode-hook #'howm-eldoc-mode)

;;; Code:

(require 'howm)
(require 'eldoc)

(defgroup howm-eldoc nil
  "Eldoc preview for howm ref links."
  :group 'howm)

(defcustom howm-eldoc-preview-max-lines 10
  "Maximum number of lines to show in the eldoc preview."
  :type 'integer
  :group 'howm-eldoc)

(defcustom howm-eldoc-fontify-preview nil
  "When non-nil, fontify the preview using the target file's major mode."
  :type 'boolean
  :group 'howm-eldoc)

(defvar howm-eldoc--cache (make-hash-table :test #'equal)
  "Cache hash-table mapping FILEPATH to (MODTIME . PREVIEW-STRING).")

(defun howm-eldoc--cache-get (filepath)
  "Return cached preview for FILEPATH if still valid, else nil."
  (when-let* ((entry (gethash filepath howm-eldoc--cache))
              (modtime (car entry))
              ((equal modtime (file-attribute-modification-time
                               (file-attributes filepath)))))
    (cdr entry)))

(defun howm-eldoc--cache-put (filepath preview)
  "Store PREVIEW for FILEPATH in the cache."
  (puthash filepath
           (cons (file-attribute-modification-time
                  (file-attributes filepath))
                 preview)
           howm-eldoc--cache))

(defun howm-eldoc-match-keyword (regexp pos)
  "If point is on a match of REGEXP on the current line, return group POS.
Respects `action-lock-case-fold-search'.  Leaves match data set."
  (let ((c (point))
        (case-fold-search (if action-lock-use-case-fold-search
                              action-lock-case-fold-search
                            case-fold-search))
        (result nil))
    (save-excursion
      (let ((eol (line-end-position)))
        (beginning-of-line)
        (while (and (<= (point) c)
                    (re-search-forward regexp eol t)
                    (not result))
          (let ((beg (match-beginning pos))
                (end (match-end pos)))
            (when (and beg (<= beg c) (< c end))
              (setq result (match-string-no-properties pos)))))))
    result))

(defun howm-eldoc-keyword-at-point ()
  "Return the keyword string if point is on a howm ref link, else nil."
  (howm-eldoc-match-keyword howm-ref-regexp howm-ref-regexp-pos))

(defun howm-eldoc-resolve-file (keyword)
  "If KEYWORD names an existing file, return its expanded path.  Else nil."
  (let ((f (expand-file-name keyword)))
    (when (and (file-exists-p f)
               (not (file-directory-p f)))
      f)))

(defun howm-eldoc-preview (filepath)
  "Return a preview string of the first section of FILEPATH, or nil."
  (or (howm-eldoc--cache-get filepath)
      (let ((preview (howm-eldoc--make-preview filepath)))
        (when preview
          (howm-eldoc--cache-put filepath preview))
        preview)))

(defun howm-eldoc--make-preview (filepath)
  "Build a preview string for FILEPATH."
  (with-temp-buffer
    (howm-page-insert:file filepath)
    (when (> (buffer-size) 0)
      (howm-set-configuration-for-file-name filepath)
      (goto-char (point-min))
      (let* ((region (howm-view-paragraph-region))
             (beg (car region))
             (end (cadr region)))
        (when howm-eldoc-fontify-preview
          (delay-mode-hooks
            (when-let* ((mode (assoc filepath auto-mode-alist
                                     #'string-match-p)))
              (funcall (cdr mode)))
            (font-lock-ensure beg end)))
        (let ((text (string-trim-right
                     (buffer-substring beg end))))
          (when (> (length text) 0)
            (let ((lines (split-string text "\n")))
              (when (> (length lines) howm-eldoc-preview-max-lines)
                (setq lines (append (seq-take lines howm-eldoc-preview-max-lines)
                                    '("...")))
                (setq text (string-join lines "\n"))))
            text))))))

(defun howm-eldoc-function (callback &rest _plist)
  "Eldoc documentation function for howm ref links.
Shows a preview of the target file when point is on a ref link
whose keyword resolves to an existing file."
  (when-let* ((keyword (howm-eldoc-keyword-at-point))
              (filepath (howm-eldoc-resolve-file keyword))
              (preview (howm-eldoc-preview filepath)))
    (funcall callback preview)
    t))

;;;###autoload
(define-minor-mode howm-eldoc-mode
  "Show eldoc previews for howm ref links."
  :lighter nil
  (if howm-eldoc-mode
      (progn
        (eldoc-mode 1)
        (add-hook 'eldoc-documentation-functions #'howm-eldoc-function nil t))
    (remove-hook 'eldoc-documentation-functions #'howm-eldoc-function t)))

(provide 'howm-eldoc)

;;; howm-eldoc.el ends here
