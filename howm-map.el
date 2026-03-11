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

(defgroup howm-context-map nil
  "Context map visualization for howm notes."
  :group 'howm)

(defcustom howm-context-map-buffer-name "*howm-context-map*"
  "Buffer name for the context map display."
  :type 'string
  :group 'howm-context-map)

(defcustom howm-context-map-max-ancestors 6
  "Maximum number of ancestor levels to display before eliding."
  :type 'integer
  :group 'howm-context-map)

(defcustom howm-context-map-max-children 7
  "Maximum number of child notes to display."
  :type 'integer
  :group 'howm-context-map)

(defcustom howm-context-map-min-title-width 18
  "Minimum display columns for a node title."
  :type 'integer
  :group 'howm-context-map)

(defcustom howm-context-map-max-title-width 40
  "Maximum display columns for a node title."
  :type 'integer
  :group 'howm-context-map)

(defcustom howm-context-map-unicode nil
  "When non-nil, use Unicode box-drawing characters for the context map.
Uses characters like │ ─ ┬ ┼ ▼ ◀─▶ instead of | - + V <->."
  :type 'boolean
  :group 'howm-context-map)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; data gathering

(defun howm-context-map-get-title (file)
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

(defun howm-context-map-parent (file)
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

(defun howm-context-map-ancestors (file)
  "Return list of ancestor files for FILE, from root to immediate parent.
Each element is an expanded file path."
  (cl-loop with current = file
           with seen = (list (expand-file-name file))
           for parent = (howm-context-map-parent current)
           while (and parent (not (member parent seen)))
           repeat (1+ howm-context-map-max-ancestors)
           collect parent into ancestors
           do (push parent seen) (setq current parent)
           finally return ancestors))

(defun howm-context-map-children (file)
  "Find children of FILE: notes containing a context link to FILE.
Returns list of expanded file paths."
  (let* ((target (expand-file-name file))
         (search-str (concat howm-ref-header " " (abbreviate-file-name target)))
         (items (howm-folder-grep (howm-folder) search-str t)))
    (delete-dups
     (cl-loop for item in items
              for name = (expand-file-name (howm-item-name item))
              unless (string= name target)
              collect name))))

(defun howm-context-map-siblings (file)
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

(defun howm-context-map--title-width (canvas-width)
  "Compute title display width from CANVAS-WIDTH.
Must allow 3 labels + 2 arrows (\" <-> \") to fit on one row."
  (let ((w (min (/ (- canvas-width 10) 3)   ; 3 labels + 2×5-char arrows
                howm-context-map-max-title-width)))
    (max howm-context-map-min-title-width w)))

(defun howm-context-map--format-node (title current-p tw)
  "Format a node string. TITLE is truncated to TW display columns."
  (let* ((prefix (if howm-context-map-unicode
                     (if current-p "┃★ " "┃")
                   (if current-p "[* " "[")))
         (suffix (if howm-context-map-unicode "┃" "]"))
         (inner-w (- tw (string-width prefix) (string-width suffix)))
         (truncated (truncate-string-to-width title (max 1 inner-w) nil nil t)))
    (concat prefix truncated suffix)))

(defun howm-context-map--format-overflow (count tw)
  "Format an overflow indicator."
  (let ((fmt (if howm-context-map-unicode
                 (format "┃… (%d more)┃" count)
               (format "[... (%d more)]" count))))
    (truncate-string-to-width fmt tw nil nil "...")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; character sets (ASCII vs Unicode)

(defun howm-context-map--char (name)
  "Return the drawing character for NAME.
When `howm-context-map-unicode' is non-nil, return a Unicode
box-drawing character; otherwise return the ASCII equivalent."
  (if howm-context-map-unicode
      (pcase name
        ('vline      ?│)
        ('hline      ?─)
        ('junction   ?┼)
        ('top-junc   ?┬)
        ('arrow-down ?▼)
        ('friend     " ◀──▶ ")
        (_ (error "Unknown drawing char: %s" name)))
    (pcase name
      ('vline      ?|)
      ('hline      ?-)
      ('junction   ?+)
      ('top-junc   ?+)
      ('arrow-down ?V)
      ('friend     " <-> ")
      (_ (error "Unknown drawing char: %s" name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; canvas drawing primitives (buffer-as-2D-grid)

(defun howm-context-map--canvas-init (width height)
  "Fill current buffer with HEIGHT lines of WIDTH spaces."
  (erase-buffer)
  (dotimes (_ height)
    (insert (make-string width ?\s) "\n")))

(defun howm-context-map--goto-xy (x y)
  "Move point to display column X on line Y (0-indexed)."
  (goto-char (point-min))
  (forward-line y)
  (move-to-column x t))

(defun howm-context-map--put-char (x y ch)
  "Place character CH at display column X, line Y.
CH may be a character or a symbol resolved via `howm-context-map--char'."
  (when (symbolp ch)
    (setq ch (howm-context-map--char ch)))
  (howm-context-map--goto-xy x y)
  (unless (eobp)
    (delete-char (min 1 (- (line-end-position) (point))))
    (insert-char ch 1)))

(defun howm-context-map--draw-text (x y str)
  "Draw STR starting at display column X, line Y.
Overwrites existing content character by character."
  (howm-context-map--goto-xy x y)
  (cl-loop for ch across str
           unless (eobp) do
           (when (> (- (line-end-position) (point)) 0)
             (delete-char 1))
           (insert-char ch 1)))

(defun howm-context-map--hline (x1 x2 y ch)
  "Draw horizontal line of CH from column X1 to X2 (inclusive) on line Y.
CH may be a character or a symbol resolved via `howm-context-map--char'."
  (when (symbolp ch)
    (setq ch (howm-context-map--char ch)))
  (cl-loop for x from (min x1 x2) to (max x1 x2)
           do (howm-context-map--put-char x y ch)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; layout and rendering (org-brain inspired)

(defun howm-context-map-render (file)
  "Render the context map for FILE into the current buffer."
  (let* ((target (expand-file-name file))
         (w (max (window-body-width (get-buffer-window (current-buffer) t)) 60))
         (tw (howm-context-map--title-width w))
         (cx (/ w 2))
         ;; gather data
         (ancestors (howm-context-map-ancestors target))
         (children (howm-context-map-children target))
         (sibling-pair (howm-context-map-siblings target))
         (prev-file (car sibling-pair))
         (next-file (cdr sibling-pair))
         ;; limit counts
         (n-ancestors (length ancestors))
         (n-children (length children))
         (show-ancestors (min n-ancestors howm-context-map-max-ancestors))
         (show-children (min n-children howm-context-map-max-children))
         (ancestor-overflow (- n-ancestors show-ancestors))
         (children-overflow (- n-children show-children))
         (vis-ancestors (last ancestors show-ancestors))
         (vis-children (when (> show-children 0)
                         (cl-subseq children 0 show-children)))
         ;; compute height
         (y 0)
         (ancestor-rows (+ (if (> ancestor-overflow 0) 2 0)
                           (* show-ancestors 2)))
         (current-row ancestor-rows)
         (children-rows (if (> show-children 0)
                            (+ 3 (if (> children-overflow 0) 1 0))
                          0))
         (total-height (+ current-row 1 children-rows 1))
         ;; format current node
         (cur-title (howm-context-map-get-title target))
         (cur-label (howm-context-map--format-node cur-title t tw))
         (cur-w (string-width cur-label)))

    ;; init canvas
    (howm-context-map--canvas-init w total-height)

    ;; === ANCESTORS (vertical chain) ===
    (setq y 0)
    (when (> ancestor-overflow 0)
      (let* ((overflow-label (howm-context-map--format-overflow
                              ancestor-overflow tw))
             (ox (max 0 (- cx (/ (string-width overflow-label) 2)))))
        (howm-context-map--draw-text ox y overflow-label)
        (setq y (1+ y))
        (howm-context-map--put-char cx y 'vline)
        (setq y (1+ y))))

    (dolist (afile vis-ancestors)
      (let* ((title (howm-context-map-get-title afile))
             (label (howm-context-map--format-node title nil tw))
             (lw (string-width label))
             (lx (max 0 (- cx (/ lw 2)))))
        (howm-context-map--draw-text lx y label)
        (setq y (1+ y))
        (howm-context-map--put-char cx y 'vline)
        (setq y (1+ y))))

    ;; Replace last connector with down-arrow if ancestors were drawn
    (when (> show-ancestors 0)
      (howm-context-map--put-char cx (1- y) 'arrow-down))

    ;; === CURRENT ROW with FRIENDS (prev/next) ===
    ;; Draw left-to-right to avoid position drift from delete/insert.
    (setq y current-row)
    (let* ((cur-x (max 0 (- cx (/ cur-w 2))))
           (cur-end (+ cur-x cur-w))
           (arrow (howm-context-map--char 'friend))
           (arrow-w (string-width arrow)))

      ;; draw prev (left friend) first
      (when prev-file
        (let* ((prev-title (howm-context-map-get-title prev-file))
               (prev-label (howm-context-map--format-node prev-title nil tw))
               (prev-w (string-width prev-label))
               (prev-end (- cur-x arrow-w))
               (prev-x (max 0 (- prev-end prev-w))))
          (when (>= prev-end 0)
            (howm-context-map--draw-text prev-x y prev-label)
            (howm-context-map--draw-text (+ prev-x prev-w) y arrow))))

      ;; draw current label
      (howm-context-map--draw-text cur-x y cur-label)

      ;; draw next (right friend)
      (when next-file
        (let* ((next-title (howm-context-map-get-title next-file))
               (next-label (howm-context-map--format-node next-title nil tw))
               (next-w (string-width next-label))
               (next-x (+ cur-end arrow-w)))
          (when (< (+ next-x next-w) w)
            (howm-context-map--draw-text cur-end y arrow)
            (howm-context-map--draw-text next-x y next-label)))))

    ;; === CHILDREN (inverted wire diagram below current) ===
    (when (> show-children 0)
      (let* ((child-labels
              (mapcar (lambda (cf)
                        (howm-context-map--format-node
                         (howm-context-map-get-title cf) nil tw))
                      vis-children))
             (child-widths (mapcar #'string-width child-labels))
             (n show-children)
             (total-label-w (apply #'+ child-widths))
             (gap (if (> n 1)
                      (max 2 (/ (max 0 (- w total-label-w)) (1- n)))
                    0))
             (block-w (+ total-label-w (* gap (max 0 (1- n)))))
             (start-x (max 0 (/ (- w block-w) 2)))
             (child-positions
              (cl-loop with x = start-x
                       for cw in child-widths
                       collect (cons x (+ x (/ cw 2)))
                       do (cl-incf x (+ cw gap)))))

        (let* ((child-centers (mapcar #'cdr child-positions))
               (bus-left (apply #'min child-centers))
               (bus-right (apply #'max child-centers))
               (y-pipe (+ current-row 1))
               (y-rail (+ current-row 2)))

          ;; vertical pipe from current down to rail
          (howm-context-map--put-char cx y-pipe 'vline)

          ;; horizontal rail
          (howm-context-map--hline (min bus-left cx) (max bus-right cx)
                                   y-rail 'hline)

          ;; junction at center where pipe meets rail
          (howm-context-map--put-char cx y-rail 'top-junc)

          ;; junctions and drops at each child center
          (dolist (cc child-centers)
            (howm-context-map--put-char cc y-rail 'junction)
            (howm-context-map--put-char cc (+ y-rail 1) 'vline))

          ;; child labels (row below drops)
          (cl-loop with y-labels = (+ y-rail 2)
                   for pos in child-positions
                   for label in child-labels
                   do (howm-context-map--draw-text (car pos) y-labels label))

          ;; children overflow
          (when (> children-overflow 0)
            (let* ((y-overflow (+ y-rail 3))
                   (overflow-label (howm-context-map--format-overflow
                                    children-overflow tw))
                   (ox (max 0 (- cx (/ (string-width overflow-label) 2)))))
              (howm-context-map--draw-text ox y-overflow overflow-label))))))

    ;; trim trailing whitespace from each line
    (goto-char (point-min))
    (while (not (eobp))
      (end-of-line)
      (delete-horizontal-space)
      (forward-line 1))
    ;; remove trailing blank lines
    (goto-char (point-max))
    (skip-chars-backward " \t\n")
    (delete-region (1+ (point)) (point-max))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; entry point

(defun howm-context-map ()
  "Display a context map for the current howm note."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (error "Current buffer is not visiting a file"))
    (let ((buf (get-buffer-create howm-context-map-buffer-name)))
      (display-buffer buf)
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (howm-context-map-render file)
          (goto-char (point-min)))
        (setq buffer-read-only t)
        (set-buffer-modified-p nil)))))

;;; howm-map.el ends here
