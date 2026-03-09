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

;; Show contextual eldoc information for howm links:
;;
;;   1. >>> filepath  (file exists)  → preview of the target note
;;   2. >>> keyword   (not a file)   → number of search matches
;;   3. Implicit come-from keywords  → preview of the <<< source note
;;
;; Usage:
;;   (add-hook 'howm-mode-hook #'howm-eldoc-mode)

;;; Code:

(require 'howm)
(require 'eldoc)

(defvar howm-eldoc--cache (make-hash-table :test #'equal)
  "Cache hash-table mapping FILEPATH to (MODTIME . PREVIEW-STRING).")

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
  :group 'howm-eldoc
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (clrhash howm-eldoc--cache)))

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
          (when (and (<= (match-beginning 0) c) (< c (match-end 0)))
            (setq result (match-string-no-properties pos))))))
    result))

(defun howm-eldoc-keyword-at-point ()
  "Return the keyword string if point is on a howm ref link, else nil."
  (howm-eldoc-match-keyword howm-ref-regexp howm-ref-regexp-pos))

(defun howm-eldoc-implicit-keyword-at-point ()
  "Return the implicit come-from keyword at point, or nil.
Reconstructs the keyword regexp the same way `howm-action-lock-setup' does."
  (let ((ks (howm-keyword-for-goto)))
    (when ks
      (let ((r (mapconcat (if howm-check-word-break
                              #'howm-action-lock-quote-keyword
                            #'regexp-quote)
                          ks "\\|")))
        (howm-eldoc-match-keyword r 0)))))

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

(defun howm-eldoc--async-search (keyword callback formatter)
  "Search for KEYWORD asynchronously and call CALLBACK with formatted result.
FORMATTER is called with (items priv-item) and should return a string or nil."
  (let ((buf (current-buffer)))
    (run-with-timer
     0 nil
     (lambda ()
       (when (buffer-live-p buf)
         (let* ((result (howm-search-execute keyword
                                             (howm-search-path-folder)
                                             nil t))
                (items (nth 0 result))
                (text (funcall formatter items)))
           (when text
             (funcall callback text)))))))
  t)

(defun howm-eldoc--format-search-count (items)
  "Format search result count for ITEMS."
  (let ((n (length items)))
    (when (> n 0)
      (format "%d match%s" n (if (= n 1) "" "es")))))

(defun howm-eldoc--format-comefrom-preview (items)
  "Format come-from preview by finding the <<< source in ITEMS."
  (when-let* ((source (cl-find-if
                       (lambda (item)
                         (string-match howm-keyword-regexp
                                       (howm-item-summary item)))
                       items))
              (filepath (howm-item-name source)))
    (howm-eldoc-preview filepath)))

(defun howm-eldoc-function (callback &rest _plist)
  "Eldoc documentation function for howm links.
Dispatches to one of three handlers:
  1. >>> filepath (file exists) — synchronous content preview
  2. >>> keyword (not a file)   — async search match count
  3. Implicit come-from keyword — async preview of <<< source note

CALLBACK is as in `eldoc-documentation-functions'."
  (let ((keyword (howm-eldoc-keyword-at-point)))
    (cond
     ;; Case 1 & 2: >>> ref link
     (keyword
      (let ((filepath (howm-eldoc-resolve-file keyword)))
        (if filepath
            ;; Case 1: file exists — synchronous preview
            (when-let* ((preview (howm-eldoc-preview filepath)))
              (funcall callback preview)
              t)
          ;; Case 2: not a file — async search count
          (howm-eldoc--async-search keyword callback
                                    #'howm-eldoc--format-search-count))))
     ;; Case 3: implicit come-from keyword
     ((when-let* ((implicit (howm-eldoc-implicit-keyword-at-point)))
        (howm-eldoc--async-search implicit callback
                                  #'howm-eldoc--format-comefrom-preview))))))

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
