;;; -*- lexical-binding: t; -*-
;;; howm-map.el --- Context map for howm notes
;;; Copyright (C) 2026
;;;   HIRAOKA Kazuyuki <kakkokakko@gmail.com>
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 1, or (at your option)
;;; any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; The GNU General Public License is available by anonymouse ftp from
;;; prep.ai.mit.edu in pub/gnu/COPYING.  Alternately, you can write to
;;; the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;;; USA.
;;--------------------------------------------------------------------

(provide 'howm-map)
(require 'cl-lib)
(require 'howm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; customization

(defgroup howm-map nil
  "Context map visualization for howm notes."
  :group 'howm)

(defcustom howm-map-buffer-name "*howm-map*"
  "Buffer name for the context map display."
  :type 'string
  :group 'howm-map)

(defcustom howm-map-title-width 40
  "Maximum display columns for a node title.
If a title is shorter, the box shrinks to fit; if longer, truncate."
  :type 'integer
  :group 'howm-map)

(defcustom howm-map-unicode nil
  "When non-nil, use Unicode box-drawing characters for the context map.
Uses characters like │ ─ ┬ ┼ ▼ ◀─▶ instead of | - + V <->."
  :type 'boolean
  :group 'howm-map)

(defcustom howm-map-context-line-format 'auto
  "How to recognize a context (backlink) line in a howm note.
Only grep hits whose matched line satisfies this pattern are
counted as children in the context map.

Possible values:

  `auto'     Build the regexp automatically at runtime from
             `howm-dtime-format' and `howm-ref-header'.
             This is the recommended default and works with
             howm-org, howm-markdown, and the standard format.

  STRING     A regexp format string.  Three `%s' placeholders are
             substituted in order: (1) a regexp matching any
             timestamp in `howm-dtime-format', (2) the
             `regexp-quote'd `howm-ref-header', (3) the
             `regexp-quote'd abbreviated file path.
             The default `auto' value is equivalent to:
               \"^%s %s %s$\"

  FUNCTION   Called with one argument, the abbreviated file path
             (not regexp-quoted).  Must return a regexp that
             matches a context line referencing that file."
  :type '(choice (const :tag "Auto (from howm-dtime-format)" auto)
                 (string :tag "Regexp format (three %s: timestamp-re, ref-header-re, file-re)")
                 (function :tag "Function (file-path → regexp)"))
  :group 'howm-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; context-line matching

(defconst howm-map--dtime-spec-alist
  '(("%Y" . "[0-9]\\{4\\}")
    ("%m" . "[0-9]\\{2\\}")
    ("%d" . "[0-9]\\{2\\}")
    ("%H" . "[0-9]\\{2\\}")
    ("%M" . "[0-9]\\{2\\}")
    ("%S" . "[0-9]\\{2\\}")
    ("%a" . "[A-Za-z]+")
    ("%A" . "[A-Za-z]+")
    ("%b" . "[A-Za-z]+")
    ("%B" . "[A-Za-z]+")
    ("%p" . "[A-Za-z]+")
    ("%Z" . "[A-Za-z/]+"))
  "Alist mapping `format-time-string' directives to Emacs regexps.")

(defun howm-map--dtime-format-to-regexp (fmt)
  "Convert a `format-time-string' format FMT to a regexp.
Known %-directives are replaced with character-class patterns;
all literal text is `regexp-quote'd."
  (let ((pos 0)
        parts)
    (while (string-match "%[A-Za-z]" fmt pos)
      (let* ((ms (match-beginning 0))
             (me (match-end 0))
             (literal (substring fmt pos ms))
             (spec (match-string 0 fmt))
             (replacement (cdr (assoc spec
                                      howm-map--dtime-spec-alist))))
        (push (regexp-quote literal) parts)
        (push (or replacement (regexp-quote spec)) parts)
        (setq pos me)))
    (push (regexp-quote (substring fmt pos)) parts)
    (apply #'concat (nreverse parts))))

(defun howm-map--context-line-regexp (abbrev-path)
  "Return a regexp matching a context line that references ABBREV-PATH.
Consults `howm-map-context-line-format' to determine the
pattern.  ABBREV-PATH is the `abbreviate-file-name' of the target."
  (let ((fmt howm-map-context-line-format))
    (cond
     ((eq fmt 'auto)
      (concat "^"
              (howm-map--dtime-format-to-regexp howm-dtime-format)
              " "
              (regexp-quote howm-ref-header)
              " "
              (regexp-quote abbrev-path)
              "$"))
     ((stringp fmt)
      (format fmt
              (howm-map--dtime-format-to-regexp howm-dtime-format)
              (regexp-quote howm-ref-header)
              (regexp-quote abbrev-path)))
     ((functionp fmt)
      (funcall fmt abbrev-path))
     (t (error "Invalid `howm-map-context-line-format': %S" fmt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data gathering

(defun howm-map-get-title (file)
  "Extract the title from FILE by looking for `howm-view-title-header'.
Return \"?\" if FILE is nil, missing, or has a blank title."
  (if (not (and file (file-exists-p file)))
      "?"
    (with-temp-buffer
      (insert-file-contents file nil 0 1024)
      (goto-char (point-min))
      (if-let* ((re (rx bol
                        (literal howm-view-title-header)
                        (+ blank) ; "space" includes newline which can wrap
                        (group (+ nonl))))
                (title (and (re-search-forward re nil t)
                            (string-trim (match-string-no-properties 1))))
                ((not (string-empty-p title))))
          title
        (or (file-name-sans-extension (file-name-nondirectory file))
            "?")))))

(defun howm-map-parent (file)
  "Find the parent file of FILE by parsing context links.
The context link may appear anywhere on a line (e.g. after a date stamp).
Return the expanded file path of the parent, or nil."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let ((re (rx (literal howm-ref-header)
                    (+ space)
                    (group (+ (not (in " \t\n"))))))
            expanded)
        (when (and (re-search-forward re nil t)
                   (file-exists-p
                    (setq expanded (expand-file-name
                                    (match-string-no-properties 1)))))
          expanded)))))

(defun howm-map-ancestors (file)
  "Return list of ancestor files for FILE, from root to immediate parent.
Each element is an expanded file path."
  (cl-loop with current = file
           with seen = (list (expand-file-name file))
           for parent = (howm-map-parent current)
           while (and parent (not (member parent seen)))
           collect parent into ancestors
           do (push parent seen) (setq current parent)
           finally return ancestors))

(defun howm-map-children (file)
  "Find children of FILE: notes containing a context link to FILE.
Returns list of expanded file paths.
Grep hits are filtered by `howm-map-context-line-format'
so that only lines matching the context-link pattern are counted."
  (let* ((target (expand-file-name file))
         (abbrev-target (abbreviate-file-name target))
         (search-str (concat howm-ref-header " " abbrev-target))
         (items (howm-folder-grep (howm-folder) search-str t))
         (context-re (howm-map--context-line-regexp abbrev-target)))
    (delete-dups
     (cl-loop for item in items
              for name = (expand-file-name (howm-item-name item))
              unless (or (string= name target)
                         (not (string-match-p context-re
                                              (howm-item-summary item))))
              collect name))))

(defun howm-map-siblings (file)
  "Find chronological prev/next siblings of FILE.
FILE must be an expanded path.
Returns (prev . next) where each is a file path or nil."
  (let ((all-files (sort (howm-files-in-directory howm-directory)
                         #'string<)))
    (cl-loop for (a b c) on all-files
             when (string= b file) return (cons a c)
             finally return (cons nil nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; formatting helpers

(defconst howm-map--node-h 3
  "Height in rows of a single node box.")

(defconst howm-map--node-mid 1
  "Row offset of the middle (title) line within a node box.")

(defun howm-map--format-node (title current-p)
  "Format a node as a 3-row box with variable width.
Return a plist (:lines (TOP MID BOT) :w WIDTH).
Box width shrinks to fit TITLE, capped at `howm-map-title-width'."
  (let* ((tl  (howm-map--char 'box-tl))
         (tr  (howm-map--char 'box-tr))
         (bl  (howm-map--char 'box-bl))
         (br  (howm-map--char 'box-br))
         (h   (howm-map--char 'hline))
         (v   (howm-map--char 'vline))
         (marker (if current-p
                     (if howm-map-unicode "★ " "* ")
                   ""))
         (marker-w (string-width marker))
         (max-text-w (max 1 (- howm-map-title-width marker-w)))
         (truncated (truncate-string-to-width title max-text-w nil nil t))
         (text-w (string-width truncated))
         (inner-w (+ marker-w text-w))
         (tw (+ inner-w 2))
         (hfill (make-string inner-w h))
         (top (concat (string tl) hfill (string tr)))
         (bot (concat (string bl) hfill (string br)))
         (mid (concat (string v) marker truncated (string v))))
    (list :lines (list top mid bot) :w tw)))

(defun howm-map--draw-node (x y node)
  "Draw NODE (a plist from `--format-node') at position (X, Y).
Draws 3 lines at y, y+1, y+2."
  (cl-loop for line in (plist-get node :lines)
           for row from y
           do (howm-map--draw-text x row line)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; character sets (ASCII vs Unicode)

(defun howm-map--char (name)
  "Return the drawing character for NAME.
When `howm-map-unicode' is non-nil, return a Unicode
box-drawing character; otherwise return the ASCII equivalent."
  (if howm-map-unicode
      (pcase name
        ('vline      ?│)
        ('hline      ?─)
        ('junction   ?┼)
        ('top-junc   ?┬)
        ('arrow-down ?▼)
        ('friend     " ◀──▶ ")
        ('box-tl     ?┌)
        ('box-tr     ?┐)
        ('box-bl     ?└)
        ('box-br     ?┘)
        (_ (error "Unknown drawing char: %s" name)))
    (pcase name
      ('vline      ?|)
      ('hline      ?-)
      ('junction   ?+)
      ('top-junc   ?+)
      ('arrow-down ?V)
      ('friend     " <-> ")
      ('box-tl     ?+)
      ('box-tr     ?+)
      ('box-bl     ?+)
      ('box-br     ?+)
      (_ (error "Unknown drawing char: %s" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; canvas drawing primitives (buffer-as-2D-grid)

(defun howm-map--goto-xy (x y)
  "Move point to column X on line Y, expanding buffer as needed."
  (goto-char (point-max))
  (let ((current-lines (count-lines (point-min) (point-max))))
    (when (< current-lines (1+ y))
      (insert (make-string (- (1+ y) current-lines) ?\n))))
  (goto-char (point-min))
  (forward-line y)
  (move-to-column x t))

(defun howm-map--put-char (x y ch)
  "Place character CH at display column X, line Y.
CH may be a character or a symbol resolved via `howm-map--char'.
Handles multi-column characters correctly."
  (when (symbolp ch)
    (setq ch (howm-map--char ch)))
  (howm-map--goto-xy x y)
  (let* ((new-w (max 1 (char-width ch)))
         (deleted 0))
    ;; delete enough existing columns to make room for new-w columns
    (while (and (< (point) (line-end-position))
                (< deleted new-w))
      (cl-incf deleted (max 1 (char-width (char-after))))
      (delete-char 1))
    (insert-char ch 1)
    ;; if we deleted more columns than needed, pad with spaces
    (when (> deleted new-w)
      (insert-char ?\s (- deleted new-w)))))

(defun howm-map--draw-text (x y str)
  "Draw STR starting at display column X, line Y.
Overwrites existing content character by character,
accounting for multi-column characters."
  (let ((col x))
    (cl-loop for i from 0 below (length str)
             for ch = (aref str i)
             do (howm-map--put-char col y ch)
                (cl-incf col (max 1 (char-width ch))))))

(defun howm-map--hline (x1 x2 y ch)
  "Draw horizontal line of CH from column X1 to X2 (inclusive) on line Y.
CH may be a character or a symbol resolved via `howm-map--char'."
  (when (symbolp ch)
    (setq ch (howm-map--char ch)))
  (cl-loop for x from (min x1 x2) to (max x1 x2)
           do (howm-map--put-char x y ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; layout and rendering (org-brain inspired)

(defun howm-map-render (file)
  "Render the context map for FILE into the current buffer."
  (let* ((target (expand-file-name file))
         (nh howm-map--node-h)
         (nm howm-map--node-mid)
         ;; gather data
         (ancestors (howm-map-ancestors target))
         (children (howm-map-children target))
         (sibling-pair (howm-map-siblings target))
         (prev-file (car sibling-pair))
         (next-file (cdr sibling-pair))
         ;; format current node
         (cur-title (howm-map-get-title target))
         (cur-node (howm-map--format-node cur-title t))
         (cur-w (plist-get cur-node :w))
         (arrow (howm-map--char 'friend))
         (arrow-w (string-width arrow))
         ;; compute cur-x: ensure prev sibling fits to the left
         (prev-space (if prev-file
                         (let* ((pw (plist-get
                                     (howm-map--format-node
                                      (howm-map-get-title prev-file) nil)
                                     :w)))
                           (+ pw arrow-w))
                       0))
         (margin 2)
         (cur-x (+ margin prev-space))
         (cx (+ cur-x (/ cur-w 2)))
         (y 0))

    (erase-buffer)

    ;; === ANCESTORS (vertical chain) ===
    (dolist (afile ancestors)
      (let* ((title (howm-map-get-title afile))
             (node (howm-map--format-node title nil))
             (nw (plist-get node :w))
             (lx (- cx (/ nw 2))))
        (howm-map--draw-node lx y node)
        (setq y (+ y nh))
        (howm-map--put-char cx y 'vline)
        (setq y (1+ y))))

    ;; Replace last connector with down-arrow if ancestors were drawn
    (when ancestors
      (howm-map--put-char cx (1- y) 'arrow-down))

    ;; === CURRENT ROW with FRIENDS (prev/next) ===
    (let* ((current-row y)
           (cur-end (+ cur-x cur-w)))

      ;; draw prev (left friend)
      (when prev-file
        (let* ((prev-title (howm-map-get-title prev-file))
               (prev-node (howm-map--format-node prev-title nil))
               (prev-w (plist-get prev-node :w))
               (prev-x (- cur-x arrow-w prev-w)))
          (howm-map--draw-node prev-x y prev-node)
          (howm-map--draw-text (+ prev-x prev-w) (+ y nm) arrow)))

      ;; draw current node box
      (howm-map--draw-node cur-x y cur-node)

      ;; draw next (right friend)
      (when next-file
        (let* ((next-title (howm-map-get-title next-file))
               (next-node (howm-map--format-node next-title nil))
               (next-x (+ cur-end arrow-w)))
          (howm-map--draw-text cur-end (+ y nm) arrow)
          (howm-map--draw-node next-x y next-node)))

      ;; === CHILDREN (inverted wire diagram below current) ===
      (when children
        (let* ((child-nodes
                (mapcar (lambda (cf)
                          (howm-map--format-node (howm-map-get-title cf) nil))
                        children))
               (child-widths (mapcar (lambda (n) (plist-get n :w))
                                     child-nodes))
               (gap 3)
               (start-x margin)
               (child-positions
                (cl-loop with x = start-x
                         for cw in child-widths
                         collect (cons x (+ x (/ cw 2)))
                         do (cl-incf x (+ cw gap))))
               (child-centers (mapcar #'cdr child-positions))
               (bus-left (apply #'min child-centers))
               (bus-right (apply #'max child-centers))
               (y-pipe (+ current-row nh))
               (y-rail (+ y-pipe 1))
               (y-drop (+ y-rail 1))
               (y-child-top (+ y-drop 1)))

          ;; vertical pipe from current down to rail
          (howm-map--put-char cx y-pipe 'vline)

          ;; horizontal rail
          (howm-map--hline (min bus-left cx) (max bus-right cx)
                           y-rail 'hline)

          ;; junction at center where pipe meets rail
          (howm-map--put-char cx y-rail 'top-junc)

          ;; junctions and drops at each child center
          (dolist (cc child-centers)
            (howm-map--put-char cc y-rail 'junction)
            (howm-map--put-char cc y-drop 'vline))

          ;; child node boxes
          (cl-loop for pos in child-positions
                   for node in child-nodes
                   do (howm-map--draw-node (car pos) y-child-top node)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; major mode

(defvar-local howm-map--source-file nil
  "The file whose context map is displayed in this buffer.")

(defvar howm-map-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'howm-map-refresh)
    map)
  "Keymap for `howm-map-mode'.")

(define-derived-mode howm-map-mode special-mode "howm-map"
  "Major mode for the howm context-map buffer.
Disables visual-line-mode and line truncation."
  :group 'howm-map
  (visual-line-mode -1)
  (setq-local truncate-lines t
              word-wrap nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; entry point

(defun howm-map ()
  "Display a context map for the current howm note."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (error "Current buffer is not visiting a file"))
    (let ((buf (get-buffer-create howm-map-buffer-name)))
      (display-buffer buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (howm-map-mode)
          (setq howm-map--source-file file)
          (howm-map-render file)
          (goto-char (point-min)))
        (set-buffer-modified-p nil)))))

(defun howm-map-refresh ()
  "Redraw the context map with updated data."
  (interactive)
  (unless howm-map--source-file
    (error "No source file recorded; open the map with `howm-map' first"))
  (let ((inhibit-read-only t))
    (howm-map-render howm-map--source-file)
    (goto-char (point-min)))
  (set-buffer-modified-p nil))

;;; howm-map.el ends here
