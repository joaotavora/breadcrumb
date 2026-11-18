;;; breadcrumb.el --- project and imenu-based breadcrumb paths   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Free Software Foundation, Inc.

;; Author: João Távora <joaotavora@gmail.com>
;; Package-Requires: ((emacs "28.1") (project "0.9.8"))
;; Version: 1.1.0
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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
;;
;;;; Usage:
;;
;; Breadcrumbs are sequences of short strings indicating where you
;; are in some big tree-like maze.
;;
;; To craft these strings, this library uses the maps provided by
;; project.el and Imenu, respectively.  Project breadcrumbs shows you
;; the current buffer's path in a large project.  Imenu breadcrumbs
;; show the current position of point in the buffer's nested
;; structure of programming constructs (for example, a specific
;; functions within multiple C++ nested namespaces).
;;
;; To use this library:
;;
;; * `M-x breadcrumb-mode` is a global mode.  Will try to turn itself
;;   on conservatively and only if there's a project.
;;
;; * `M-x breadcrumb-local-mode` is a buffer-local minor mode, if you
;;    don't want the default heuristics for turning it on everywhere.
;;
;; * Manually put the mode-line constructs
;;
;;     (:eval (breadcrumb-imenu-crumbs))
;;
;;   and
;;
;;     (:eval (breadcrumb-project-crumbs))
;;
;;  in your settings of the `mode-line-format' or
;;  `header-line-format' variables.
;;
;; The shape and size of each breadcrumb groups may be tweaked via
;; `breadcrumb-imenu-max-length', `breadcrumb-project-max-length',
;; `breadcrumb-imenu-crumb-separator', and
;; `breadcrumb-project-crumb-separator'.
;;
;; The structure each the breadcrumbs varies depending on whether
;; either project.el and imenu.el (or both) can do useful things for
;; your buffer.
;;
;; For Project breadcrumbs, this depends on whether project.el's
;; `project-current' can guess what project the current buffer
;; belongs to.
;;
;; For Imenu breadcrumbs, this varies.  Depending on the major-mode
;; author's taste, the Imenu tree (in variable `imenu--index-alist')
;; may have different structure.  Sometimes, minor mode also tweak
;; the Imenu tree in useful ways.  For example, with recent Eglot (I
;; think Eglot 1.14+), managed buffers get extra region info added to
;; it, which makes Breadcrumb show "richer" paths.
;;
;;;; Implementation notes:
;;
;; This _should_ be faster than which-func.el due some caching
;; strategies.  One of these strategies occurs in `bc--ipath-alist',
;; which takes care not to over-call `imenu--make-index-alist', which
;; could be slow (in fact very slow if an external process needs to
;; be contacted).  The variable `breadcrumb-idle-delay' controls
;; that.  Another cache occurs in `bc--ipath-plain-cache' second is
;; just a simple "space-for-speed" cache.
;;
;; Breadcrumb uses the double-dashed Imenu symbols
;; `imenu--index-alist' and `imenu--make-index-alist'.  There's
;; really no official API here.  It's arguable that, despite the
;; name, these aren't really internal symbols (the much older
;; which-func.el library makes liberal use of them, for example).
;;
;;;; Todo:
;;
;; Make more clicky buttons in the headerline to do whatever
;;

;;; Code:
(require 'cl-lib)
(require 'imenu)
(require 'project)


;;;; Customization options
;;

(defgroup breadcrumb nil
  "One-liner indication of where you are in the maze."
  :prefix "breadcrumb-"
  :group 'convenience)

(defcustom bc-project-max-length 0.3
  "Soft cutoff for `breadcrumb-project-crumbs'.
If a fixnum, it's a absolute number of characters.  If a float, a
percentage of `window-width'."
  :type '(choice (natnum :tag "Number of characters")
                 (float  :tag "Percent of window's width")))

(defcustom bc-project-crumb-separator "/"
  "Separator for `breadcrumb-project-crumbs'." :type 'string)

(defcustom bc-imenu-max-length 0.3
  "Soft cutoff for `breadcrumb-imenu-crumbs'.
If a fixnum, it's a absolute number of characters.  If a float, a
percentage of `window-width'."
  :type '(choice (natnum :tag "Number of characters")
                 (float  :tag "Percent of window's width")))

(defcustom bc-imenu-crumb-separator " > "
  "Separator for `breadcrumb-project-crumbs'." :type 'string)

(defcustom bc-decorate-function nil
  "Function to decorate each crumb, or nil.
Called with two arguments, CRUMB and INFO.  Should return a string to
display in place of CRUMB, for example CRUMB with an icon prepended.

The returned string can be propertized with `keymap', `mouse-face' or
`help-echo' to override the values normally in CRUMB.  To make clicks on
a decoration do something else, give it its own `keymap', binding events
like [mode-line mouse-3] and [header-line mouse-3].  To keep CRUMB's
bindings as well, use `make-composed-keymap' with CRUMB's `keymap'
property as fallback.

INFO is a plist.  These keys are currently defined:

`:type'      One of `project-base', `project-dir', `project-file',
             `buffer', `imenu-crumb' or `imenu-leaf'.
`:file'      For project crumbs, the absolute file or directory name.
`:kind'      For Imenu crumbs, the kind of the construct, a string
             like \"Function\" or \"Class\".  Only if the Imenu
             backend provides it, as Eglot does.

Decorations that make crumbs wider are the first thing to go when
space is short.  They're shown only if no crumb had to be shortened
to fit `breadcrumb-imenu-max-length' or `breadcrumb-project-max-length',
and then only while they fit, rightmost crumbs first.  Decorations
that don't widen crumbs, like ones adding only a `keymap', are always
kept.

See `breadcrumb-decorate-with-nerd-font' for an example.  If this
is nil, `breadcrumb-opinionated-mode' uses that function."
  :type '(choice (const :tag "No decoration" nil)
                 (const :tag "Nerd Font icons" bc-decorate-with-nerd-font)
                 function))

(defface bc-face '((t (:inherit shadow)))
  "Base face for all breadcrumb things.")

(defface bc-imenu-crumbs-face '((t (:inherit bc-face)))
  "Face for imenu crumbs in the breadcrumb imenu path.")

(defface bc-imenu-leaf-face '((t (:inherit (font-lock-function-name-face
                                            bc-imenu-crumbs-face))))
  "Face for imenu leaf crumbs in the breadcrumb imenu path.")

(defface bc-project-crumbs-face '((t (:inherit bc-face)))
  "Face for project crumbs in the breadcrumb project path.")

(defface bc-project-base-face '((t (:inherit bc-project-crumbs-face)))
  "Face for project base in the breadcrumb project path.")

(defface bc-project-leaf-face '((t (:inherit (mode-line-buffer-id))))
  "Face for the project leaf crumb in breadcrumb project path.")


;;;; "ipath" management logic and imenu interoperation
;;
(cl-defun bc--bisect (a x &key (from 0) (to (length a)) key from-end)
  "Compute index to insert X in sequence A, keeping it sorted.
If X already in A, the resulting index is the leftmost such
index, unless FROM-END is t.  KEY is as usual in other CL land."
  (cl-macrolet ((search (from-end key)
                  `(cl-loop while (< from to)
                            for mid = (/ (+ from to) 2)
                            for p1 = (elt a mid)
                            for p2 = ,(if key `(funcall key p1) `p1)
                            if (,(if from-end '< '<=) x p2)
                            do (setq to mid) else do (setq from (1+ mid))
                            finally return from)))
    (if from-end (if key (search t key) (search t nil))
      (if key (search nil key) (search nil nil)))))

(defun bc--region (name)
  "Get (BEG . END) region of Imenu node NAME, or nil.
Prefer the `imenu-region' property, fall back to the older
`breadcrumb-region' (github#48)."
  (or (get-text-property 0 'imenu-region name)
      (get-text-property 0 'breadcrumb-region name)))

(defun bc--ipath-rich (index-alist pos)
  "Compute ipath for rich `imenu--index-alist' structures.
These structures have an `imenu-region' or `breadcrumb-region'
property on every node."
  (cl-labels
      ((search (nodes &optional ipath)
         (cl-loop
          for n in nodes
          for reg = (bc--region (car n))
          when (<= (car reg) pos (cdr reg))
          return (search (cdr n) (cons
                                  (propertize (car n)
                                              'breadcrumb-siblings nodes
                                              'breadcrumb-parent (car ipath))
                                  ipath))
          finally (cl-return ipath))))
    (nreverse (search index-alist))))

(defvar-local bc--ipath-plain-cache nil
  "A cache for `breadcrumb--ipath-plain'.")

(defun bc--mark-start (name entries)
  "Record on NAME the smallest position in ENTRIES, and return ENTRIES.
ENTRIES are (POS . IPATH) entries of `breadcrumb--ipath-plain-cache'
for Imenu node NAME, or for its descendants.  See
`breadcrumb--node-start'."
  (when (and entries (not (string-empty-p name)))
    (put-text-property 0 (length name) 'bc--start
                       (cl-loop for (pos . _) in entries minimize pos)
                       name))
  entries)

(defun bc--ipath-plain (index-alist pos)
  "Compute ipath for plain `imenu--index-alist' structures.
These structures don't have region properties on."
  (cl-labels ((dfs (n &optional ipath siblings)
                ;; Copy names, as `bc--mark-start' and the siblings
                ;; property below modify them.
                (setq ipath (cons (copy-sequence (car n)) ipath))
                (cond
                 ((imenu--subalist-p n)
                  (bc--mark-start
                   (car ipath)
                   (mapcan (lambda (n2) (dfs n2 ipath (cdr n))) (cdr n))))
                 ;; Empty submenus or "special elements" (NAME POS
                 ;; FUNCTION ARGS...), whose POS isn't necessarily a
                 ;; buffer position (github#51).  See docstring of
                 ;; `imenu--index-alist'.
                 ((listp (cdr n)) nil)
                 (t
                  ;; FIXME: we convert markers to points via the `+'
                  ;; down there.  But for siblings, no such conversion
                  ;; happens, they might still point to invalid
                  ;; markers.  Not worth doing another tree traversal
                  ;; for that IMO, and siblings seems to be unused
                  ;; anyway (github#22)
                  (unless (string-empty-p (car ipath)) ; github#36
                    (put-text-property 0 1 'breadcrumb-siblings (cdr siblings)
                                       (car ipath)))
                  (bc--mark-start
                   (car ipath)
                   (list (cons
                          ;; See github#17 and docstring of
                          ;; `imenu--index-alist' for the
                          ;; "overlay" edge case.
                          (cl-etypecase (cdr n)
                            (number (cdr n))
                            (marker (+ (cdr n) 0))
                            (overlay (overlay-start (cdr n))))
                          ipath)))))))
    (unless bc--ipath-plain-cache
      (setq bc--ipath-plain-cache
            (cl-sort (vconcat (mapcan (lambda (i) (dfs i nil index-alist))
                                      index-alist))
                     #'< :key #'car)))
    (when (and (length> bc--ipath-plain-cache 0)
               (>= pos (car (aref bc--ipath-plain-cache 0))))
      (let ((res (bc--bisect bc--ipath-plain-cache pos :key #'car :from-end t)))
        (unless (zerop res) (reverse (cdr (elt bc--ipath-plain-cache (1- res)))))))))

(defun bc-ipath (index-alist pos)
  "Get breadcrumb for position POS given INDEX-ALIST."
  (if (bc--region (caar index-alist))
      (bc--ipath-rich index-alist pos)
    (bc--ipath-plain index-alist pos)))

(defvar bc-idle-time 1
  "Control idle time before requesting new breadcrumbs.")

(defvar-local bc--idle-timer nil
  "Timer used by `breadcrumb--ipath-alist'.")

(defvar-local bc--last-update-tick 0
  "Last time `breadcrumb--ipath-alist' asked for an update.")

(defun bc--ipath-alist ()
  "Return `imenu--index-alist', maybe arrange for its update."
  ;; Be mindful of indirect buffers (github#31)
  (let ((buf (or (buffer-base-buffer) (current-buffer))))
    (with-current-buffer buf
      (unless (= (buffer-chars-modified-tick) bc--last-update-tick)
        (setq bc--last-update-tick (buffer-chars-modified-tick))
        (when bc--idle-timer (cancel-timer bc--idle-timer))
        (setq bc--idle-timer
              (run-with-idle-timer
               bc-idle-time nil
               (lambda ()
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (setq bc--last-update-tick (buffer-chars-modified-tick))
                     (let ((non-essential t)
                           (imenu-auto-rescan t))
                       (ignore-errors
                         (imenu--make-index-alist t))
                       (setq bc--ipath-plain-cache nil)
                       ;; no point is taxing the mode-line machinery now
                       ;; if the buffer isn't showing anywhere.
                       (when (get-buffer-window buf t)
                         (force-mode-line-update t)))))))))
      imenu--index-alist)))


;;;; Higher-level functions
;;

;; FIXME: Why do I need to put these key definitiosn in special
;; variables?
(defvar bc--header-line-key [header-line mouse-1])
(defvar bc--mode-line-key [mode-line mouse-1])

(defun bc--length (len)
  "Interpret LEN using `window-width' and return a number."
  (cond ((floatp len) (* (window-width) len))
        (t len)))

(defun bc--click-props (what fn)
  "Text properties making a crumb call FN when clicked.
FN is called with no arguments, in the window whose mode line or header
line was clicked.  WHAT describes the click's destination."
  (let ((m (make-sparse-keymap))
        (cmd (lambda (event)
               (interactive "e")
               (select-window (posn-window (event-start event)))
               (funcall fn))))
    (define-key m bc--header-line-key cmd)
    (define-key m bc--mode-line-key cmd)
    (list 'mouse-face 'header-line-highlight
          'help-echo (format "mouse-1: Go to %s" what)
          'keymap m)))

(defun bc--node-start (name)
  "Get start position of Imenu node NAME, or nil."
  (or (car (bc--region name))
      (get-text-property 0 'bc--start name)))

(defun bc--format-ipath-node (p more path)
  "Format Imenu crumb P, given MORE crumbs after it.
PATH is the list of crumbs from the outermost one down to P."
  (let ((start (bc--node-start p)))
    (apply
     #'propertize
     p
     'face (if more 'bc-imenu-crumbs-face 'bc-imenu-leaf-face)
     'bc-dont-shorten (null more)
     'bc-crumb-info (list :type (if more 'imenu-crumb 'imenu-leaf)
                          :kind (or (get-text-property 0 'imenu-kind p)
                                    (get-text-property 0 'breadcrumb-kind p)))
     (and start
          (bc--click-props (string-join path bc-imenu-crumb-separator)
                           (lambda () (bc--goto (selected-window) start)))))))

;;;###autoload
(defun breadcrumb-imenu-crumbs ()
  "Describe point inside the Imenu tree of current file."
  (when-let* ((alist (bc--ipath-alist)))
    (when (cl-some #'identity alist)
      (bc--summarize
       (cl-loop
        for (p . more) on (mapcar #'string-trim (bc-ipath alist (point)))
        collect (substring-no-properties p) into path
        collect (bc--format-ipath-node p more path))
       (bc--length bc-imenu-max-length)
       (propertize bc-imenu-crumb-separator
                   'face 'bc-face)))))

(defun bc--summarize (crumbs cutoff separator)
  "Return a string that summarizes CRUMBS, a list of strings.
\"Summarization\" consists of truncating some CRUMBS to 1 character.
Rightmost members of CRUMBS are summarized last.  Members with a non-nil
`breadcrumb-dont-shorten' property are never truncated.  Aim for a
return string that is at most CUTOFF characters long.  Join the crumbs
with SEPARATOR.

Then decorate the crumbs with `breadcrumb--decorate'.  Decorations
that widen crumbs are the first thing to go when space is short:
they're only added if no crumb was truncated, and only while they fit
in CUTOFF.  See `breadcrumb--decorate-crumbs'."
  (cl-loop
   for available = (- cutoff used)
   for (c . more) on (reverse crumbs)
   for seplen = (if more (length separator) 0)
   for shorten-p = (unless (or (string-empty-p c)
                               (get-text-property 0 'bc-dont-shorten c))
                     (> (+ (length c) seplen) available))
   for toadd = (if shorten-p (substring c 0 1) c)
   sum (+ (length toadd) seplen) into used
   count shorten-p into nshortened
   collect toadd into rcrumbs
   finally return
   (string-join (reverse (bc--decorate-crumbs
                          rcrumbs
                          (if (zerop nshortened) (max 0 (- cutoff used)) 0)))
                separator)))

(defun bc--decorate-crumbs (rcrumbs budget)
  "Decorate RCRUMBS, a list of crumbs in right-to-left order.
BUDGET is how many characters decorations may add in total.  Spend it
on the rightmost crumbs first.  At the first decoration that doesn't
fit, stop adding characters, so that only a rightmost run of crumbs
gets wider.  Decorations that add no characters are always kept.
Return the decorated list, in the same order."
  (cl-loop for c in rcrumbs
           for d = (bc--decorate c)
           for extra = (- (length d) (length c))
           if (> extra budget) do (setq d c budget 0)
           else do (cl-decf budget (max extra 0))
           collect d))

(defvar bc-opinionated-mode)

(defun bc--decorate (crumb)
  "Maybe decorate CRUMB with `breadcrumb-decorate-function'.
If that is nil and `breadcrumb-opinionated-mode' is on, use
`breadcrumb-decorate-with-nerd-font'."
  (let ((info (get-text-property 0 'bc-crumb-info crumb))
        (fn (or bc-decorate-function
                (and bc-opinionated-mode #'bc-decorate-with-nerd-font))))
    (if (not (and fn info))
        crumb
      (let ((d (copy-sequence (funcall fn crumb info))))
        ;; Make the decorations as clickable as the crumb itself,
        ;; unless FN made them clickable some other way.
        (dolist (p '(keymap mouse-face help-echo) d)
          (when-let* ((v (get-text-property 0 p crumb)))
            (bc--put-where-absent p v d)))))))

(defun bc--put-where-absent (prop value string)
  "Set PROP to VALUE in the parts of STRING where PROP is nil."
  (let ((beg 0) (len (length string)) end)
    (while (< beg len)
      (setq end (next-single-property-change beg prop string len))
      (unless (get-text-property beg prop string)
        (put-text-property beg end prop value string))
      (setq beg end))))

(defun bc--format-project-node (p more root path)
  "Helper for `bc--project-crumbs-1'.
Formats path crumb P given optional MORE nodes.  ROOT is the
default directory of P's project.  PATH is the path of P relative
to ROOT."
  (let ((file (expand-file-name path root)))
    (apply #'propertize p
           'face (if more 'bc-project-crumbs-face 'bc-project-leaf-face)
           'bc-dont-shorten (null more)
           'bc-crumb-info (list :type (if more 'project-dir 'project-file)
                                :file file)
           (bc--click-props (abbreviate-file-name file)
                            (lambda () (find-file file))))))

(defvar-local bc--cached-project nil
  ;; This is a fairly "dumb" cache, but hopefully good enough for most
  ;; cases.  A better smarter cache that could realize when certain
  ;; project things happened would live in project.el which has much
  ;; more project-smarts, but that's apparently very hard, so do it
  ;; here (github#18, github#55)
  "Cache the expensive `project-current' call.")

(defun bc--project-crumbs-1 (bfn)
  "Helper for `breadcrumb-project-crumbs'.
Given BFN, the `buffer-file-name', produce a list of
propertized crumbs."
  (cl-loop
   with project = (or bc--cached-project (setq bc--cached-project
                                               (project-current)))
   with root = (if project (project-root project) default-directory)
   with pname = (if project (project-name project)
                  (file-name-nondirectory (directory-file-name root)))
   with relname = (file-relative-name (or bfn default-directory)
                                      root)
   for (s . more) on (split-string relname "/")
   concat s into upto
   when more concat "/" into upto
   collect (bc--format-project-node s more root upto) into retval
   finally
   (cl-return
    (if root
        (cons (apply #'propertize pname
                     'bc-dont-shorten t
                     'bc-crumb-info (list :type 'project-base :file root)
                     'face 'bc-project-base-face
                     (bc--click-props (abbreviate-file-name root)
                                      (lambda () (find-file root))))
              retval)
      retval))))

;;;###autoload
(cl-defun breadcrumb-project-crumbs ()
  "Describing the current file inside project."
  (bc--summarize
   (if buffer-file-name (bc--project-crumbs-1 buffer-file-name)
     (list (propertize (buffer-name)
                       'face 'bc-project-leaf-face
                       'bc-dont-shorten t
                       'bc-crumb-info '(:type buffer))))
   (bc--length bc-project-max-length)
   (propertize bc-project-crumb-separator
               'face 'bc-project-crumbs-face)))

(defun bc--header-line ()
  "Helper for `breadcrumb-headerline-mode'."
  (let ((x (cl-remove-if
            #'seq-empty-p (mapcar #'funcall
                                  '(bc-project-crumbs bc-imenu-crumbs)))))
    (mapconcat #'identity x (propertize " : " 'face 'bc-face))))

;;;###autoload
(define-minor-mode breadcrumb-local-mode
  "Header lines with breadcrumbs."
  :init-value nil
  (if bc-local-mode (add-to-list 'header-line-format '(:eval (bc--header-line)))
    (setq header-line-format (delete '(:eval (bc--header-line)) header-line-format))))

(defun bc--turn-on-local-mode-on-behalf-of-global-mode ()
  (unless (or (minibufferp)
              (not (buffer-file-name))
              (null (bc-project-crumbs)))
    (bc-local-mode 1)))

;;;###autoload
(define-globalized-minor-mode breadcrumb-mode bc-local-mode
  bc--turn-on-local-mode-on-behalf-of-global-mode)

(require 'pulse)
(defun bc--goto (window pos)
  "Helper for `breadcrumb-jump'."
  (with-selected-window window
    (with-current-buffer (window-buffer)
      (push-mark)
      (goto-char pos)
      (let ((pulse-delay 0.05) (pulse-flag t))
        (pulse-momentary-highlight-region (line-beginning-position) (line-end-position))))))

;;;###autoload
(defun breadcrumb-jump ()
  "Like \\[execute-extended-command] `imenu', but breadcrumb-powered."
  (interactive)
  (let (cands choice)
    (cl-labels
        ((fmt (strs)
           (mapconcat #'identity strs " > "))
         (dfs (nodes &optional ipath)
           (cl-loop
            for n in nodes
            for newpath = (cons (car n) ipath)
            for pos = (or (car (bc--region (car n)))
                          (and (number-or-marker-p (cdr n)) (cdr n)))
            when pos do (push (cons (fmt (reverse newpath)) pos)
                              cands)
            do (dfs (cdr n) newpath))))
      (imenu--make-index-alist)
      (dfs imenu--index-alist)
      (unless cands (user-error "Sorry, no breadcrumb items to jump to"))
      (setq choice (cdr (assoc (completing-read "Index item? " cands nil t)
                               cands #'string=)))
      (push-mark)
      (bc--goto (selected-window) choice))))


;;;; Opinionated mode line
;;

;;;###autoload
(defcustom breadcrumb-opinionated-mlf
  '((:eval (bc--fancy-stub t))
    "%e"
    mode-line-modified
    " "
    (:eval (bc-project-crumbs))
    (:eval (bc--fancy-stub nil))
    (:propertize " " face default)
    (:eval (bc--fancy-imenu-crumbs))
    (:propertize " " face default)
    mode-line-format-right-align
    (:eval (bc--fancy-stub t))
    (vc-mode vc-mode)
    " "
    mode-line-modes
    mode-line-misc-info
    " %p%  "
    (:propertize "\uf05b " face font-lock-keyword-face) ; crosshairs
    "%l:%c "
    (:propertize "\uf017 " face font-lock-type-face)    ; clock
    (:eval (format-time-string "%H:%M"))
    (:eval (bc--fancy-stub nil))
    (:propertize " " face default))
  "A `mode-line-format' with breadcrumb project and imenu crumbs.
Replaces `mode-line-buffer-identification' with the output of
`breadcrumb-project-crumbs' and `breadcrumb-imenu-crumbs'."
  :type '(repeat sexp))

(defcustom bc-opinionated-diminished-modes
  '(yas-minor-mode eldoc-mode company-mode whitespace-mode
    which-key-mode)
  "Minor modes whose lighters are hidden by `breadcrumb-opinionated-mode'."
  :type '(repeat symbol))


(defvar bc--saved-mlf)
(defvar bc--saved-lighters nil)

(defun bc--diminish (mode)
  (when-let* ((cell (assq mode minor-mode-alist)))
    (push (cons mode (cadr cell)) bc--saved-lighters)
    (setcar (cdr cell) "")))

(defcustom bc-fancy-stub-chars (cons ?\ue0b6 ?\ue0b4)
  "Cons (OPENING . CLOSING) of characters bracketing fancy mode-line stubs."
  :type '(choice (cons :tag "Rounded" (const ?\ue0b6) (const ?\ue0b4))
                 (cons :tag "Angled" (const ?\ue0b2) (const ?\ue0b0))
                 (cons :tag "Slant" (const ?\ue0ba) (const ?\ue0b8))
                 (cons :tag "Backslant" (const ?\ue0be) (const ?\ue0bc))
                 (cons :tag "Custom" character character)))

(defvar bc--fancy-stub-cache (make-hash-table :test 'equal)
  "Cache of `bc--fancy-stub' results, keyed by (CHAR . SELECTED).
Each value is (BG . STRING), recomputed when BG no longer matches.")

(defvar bc--displayable-cache (make-hash-table :test 'equal)
  "Cache of `bc--displayable-p' results, keyed by (CHAR . FONT).")

(defun bc--displayable-p (char)
  "Like `char-displayable-p' for CHAR, but cached per frame font.
On text terminals Emacs can't know if the terminal's font has CHAR,
so this only checks the terminal's coding system and is often
optimistic."
  (let* ((key (cons char (frame-parameter nil 'font)))
         (v (gethash key bc--displayable-cache 'unknown)))
    (if (eq v 'unknown)
        (puthash key (and (char-displayable-p char) t) bc--displayable-cache)
      v)))

(defun bc--fancy-stub (opening)
  (let ((char (if opening (car bc-fancy-stub-chars) (cdr bc-fancy-stub-chars))))
    (if (not (bc--displayable-p char))
        ""
      (let* ((selected (mode-line-window-selected-p))
             (key (cons char selected))
             (bg (face-attribute (if selected 'mode-line 'mode-line-inactive) :background))
             (cached (gethash key bc--fancy-stub-cache)))
        (if (and cached (equal (car cached) bg))
            (cdr cached)
          (let ((str (propertize (string char) 'face
                                  `(:inherit default :foreground ,bg))))
            (puthash key (cons bg str) bc--fancy-stub-cache)
            str))))))

(cl-defun bc--fancy-imenu-crumbs ()
  (let ((retval (breadcrumb-imenu-crumbs)))
    (when (cl-plusp (length retval))
      (concat (bc--fancy-stub t) retval (bc--fancy-stub nil)))))

(defcustom bc-nerd-font-icons
  '(;; Project crumbs, by `:type'.  No `project-dir' entry: a folder
    ;; icon on every directory adds only noise.  `buffer' (file-less
    ;; buffers) has no CHAR: a file icon would mislead, but the
    ;; `nerd-icons' icon for the major mode doesn't.
    (project-base ?\uea62 nil)                          ; repo
    (project-file ?\uea7b nil)                          ; file
    (buffer nil nil)                                    ; only with nerd-icons
    ;; Imenu crumbs, by `:kind' (LSP SymbolKind names).  Faces
    ;; follow the `nerd-icons-corfu' package.  Comments name the
    ;; nf-cod- glyphs.
    ("File" ?\ueb60 font-lock-string-face)              ; symbol_file
    ("Module" ?\uea8b font-lock-preprocessor-face)      ; symbol_namespace
    ("Namespace" ?\uea8b font-lock-preprocessor-face)   ; symbol_namespace
    ("Package" ?\uea8b font-lock-preprocessor-face)     ; symbol_namespace
    ("Object" ?\uea8b font-lock-preprocessor-face)      ; symbol_namespace
    ("Class" ?\ueb5b font-lock-type-face)               ; symbol_class
    ("Interface" ?\ueb61 font-lock-type-face)           ; symbol_interface
    ("Struct" ?\uea91 font-lock-type-face)              ; symbol_structure
    ("TypeParameter" ?\uea92 font-lock-type-face)       ; symbol_parameter
    ("Array" ?\uea8a font-lock-type-face)               ; symbol_array
    ("Method" ?\uea8c font-lock-function-name-face)     ; symbol_method
    ("Function" ?\uea8c font-lock-function-name-face)   ; symbol_method
    ("Constructor" ?\uea8c font-lock-function-name-face) ; symbol_method
    ("Property" ?\ueb65 font-lock-variable-name-face)   ; symbol_property
    ("Field" ?\ueb5f font-lock-variable-name-face)      ; symbol_field
    ("Variable" ?\uea88 font-lock-variable-name-face)   ; symbol_variable
    ("Constant" ?\ueb5d font-lock-constant-face)        ; symbol_constant
    ("Enum" ?\uea95 font-lock-builtin-face)             ; symbol_enum
    ("EnumMember" ?\ueb5e font-lock-builtin-face)       ; symbol_enum_member
    ("Number" ?\uea90 font-lock-builtin-face)           ; symbol_numeric
    ("Boolean" ?\uea8f font-lock-builtin-face)          ; symbol_boolean
    ("String" ?\ueb8d font-lock-string-face)            ; symbol_string
    ("Key" ?\uea93 font-lock-keyword-face)              ; symbol_key
    ("Event" ?\uea86 font-lock-warning-face)            ; symbol_event
    ("Operator" ?\ueb64 font-lock-comment-delimiter-face)) ; symbol_operator
  "Alist of (KEY CHAR FACE) for `breadcrumb-decorate-with-nerd-font'.
KEY is a crumb `:type' symbol for project crumbs, or a `:kind'
string for Imenu crumbs (see `breadcrumb-decorate-function').
CHAR is a Nerd Font glyph, by default one of the Codicons.  FACE
colors it, or nil to use the crumb's face.  Crumbs with no entry get
no icon.  Set to nil to disable icons in `breadcrumb-opinionated-mode'.

If the `nerd-icons' package is installed, file, directory and `buffer'
crumbs with an entry get its colored icons for the file type or major
mode instead.  A nil CHAR means an icon only in that case."
  :type '(alist
          :key-type (choice symbol string)
          :value-type (list (choice (const :tag "Only with nerd-icons" nil)
                                    character)
                            (choice (const :tag "Crumb's face" nil) face))))

(declare-function nerd-icons-icon-for-file "ext:nerd-icons")
(declare-function nerd-icons-icon-for-dir "ext:nerd-icons")
(declare-function nerd-icons-icon-for-mode "ext:nerd-icons")

(defun bc-decorate-with-nerd-font (crumb info)
  "Prepend a Nerd Font icon to CRUMB according to INFO.
Pick the icon from `breadcrumb-nerd-font-icons' by INFO's `:kind' for
Imenu crumbs and by its `:type' for the others.  If the `nerd-icons'
package is installed, use its colored icons for the file type of file
and directory crumbs, and for the major mode of file-less buffers.
Return CRUMB unchanged if there's no icon or if the frame can't
display it.

This is a value for `breadcrumb-decorate-function', and an example
for writing others."
  (let* ((type (plist-get info :type))
         (entry (cdr (assoc (if (memq type '(imenu-crumb imenu-leaf))
                                (plist-get info :kind)
                              type)
                            bc-nerd-font-icons)))
         (icon (and entry
                    (bc--nerd-font-icon type (plist-get info :file) entry
                                        (get-text-property 0 'face crumb)))))
    (if (not (and (stringp icon) (not (string-empty-p icon))
                  (bc--displayable-p (aref icon 0))))
        crumb
      ;; Follow ICON with a space in ICON's face.  Some terminals (st,
      ;; for one) clip a wide glyph to its cell unless the next cell
      ;; is blank and has the same attributes.
      (concat icon (propertize " " 'face (get-text-property 0 'face icon))
              crumb))))

(defun bc--nerd-font-icon (type file entry crumb-face)
  "Icon for a crumb of TYPE and FILE, given its ENTRY.
ENTRY is (CHAR FACE), from `breadcrumb-nerd-font-icons'.  CRUMB-FACE
is the crumb's face."
  (pcase-let ((`(,char ,face) entry))
    (cond ((and (eq type 'project-file) (fboundp 'nerd-icons-icon-for-file))
           (nerd-icons-icon-for-file file))
          ((and (eq type 'project-dir) (fboundp 'nerd-icons-icon-for-dir))
           (nerd-icons-icon-for-dir file))
          ((and (eq type 'buffer) (fboundp 'nerd-icons-icon-for-mode))
           (nerd-icons-icon-for-mode major-mode))
          (char (propertize (string char)
                            'face (delq nil (list face crumb-face)))))))

;;;###autoload
(define-minor-mode breadcrumb-opinionated-mode
  "Global minor mode installing an opinionated breadcrumb mode line.
On activation, saves the current `mode-line-format' default and replaces
it with `breadcrumb-opinionated-mlf'.  Unless
`breadcrumb-decorate-function' is set, also decorates crumbs with
Nerd Font icons, via `breadcrumb-decorate-with-nerd-font'."
  :global t
  (cond
   (bc-opinionated-mode
    (unless (boundp 'bc--saved-mlf) (setq bc--saved-mlf (default-value 'mode-line-format)))
    (setq-default mode-line-format bc-opinionated-mlf)
    (mapc #'bc--diminish bc-opinionated-diminished-modes)
    (advice-add 'add-minor-mode :after
                (lambda (toggle &rest _)
                  (when (memq toggle bc-opinionated-diminished-modes)
                    (bc--diminish toggle)))
                '((name . bc--do-diminish-advice))))
   (t
    (setq-default mode-line-format bc--saved-mlf)
    (advice-remove 'add-minor-mode 'bc--do-diminish-advice)
    (dolist (saved bc--saved-lighters)
      (when-let* ((cell (assq (car saved) minor-mode-alist)))
        (setcar (cdr cell) (cdr saved))))
    (setq bc--saved-lighters nil)
    (makunbound 'bc--saved-mlf))))

(advice-add 'mode--line-format-right-align :filter-return
            (lambda (r)
              (if bc-opinionated-mode (propertize r 'face 'default) r))
            '((name . bc-opinionated-mlf)))

(provide 'breadcrumb)
;;;###autoload (register-definition-prefixes "breadcrumb" '("breadcrumb-"))

;; Local Variables:
;; read-symbol-shorthands: (("bc-" . "breadcrumb-"))
;; autoload-compute-prefixes: nil
;; End:

;;; breadcrumb.el ends here
