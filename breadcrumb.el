;;; breadcrumb.el --- project and imenu-based breadcrumb paths   -*- lexical-binding: t; -*-

;; Copyright (C) 2023  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Version: 0.0.3beta
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
;;;
;;;; Usage:
;;;
;;; Breadcrumbs are sequences of short strings indicating where you
;;; are in some big tree-like maze.
;;;
;;; To craft these strings, this library uses the maps provided by
;;; project.el and Imenu, respectively.  Project breadcrumbs shows you
;;; the current buffer's path in a large project.  Imenu breadcrumbs
;;; show the current position of point in the buffer's nested
;;; structure of programming constructs (for example, a specific
;;; functions within multiple C++ nested namespaces).
;;;
;;; To use this library:
;;;
;;; * `M-x breadcrumb-mode` is a global mode.  Will try to turn itself
;;;   on conservatively and only if there's a project.

;;; * `M-x breadcrumb-local-mode` is a buffer-local minor mode, if you
;;;    don't want the default heuristics for turning it on everywhere.
;;;
;;; * Manually put the mode-line constructs
;;;
;;;     (:eval (breadcrumb-imenu-crumbs))
;;;
;;;   and
;;;
;;;     (:eval (breadcrumb-project-crumbs))
;;;
;;;  in your settings of the `mode-line-format' or
;;;  `header-line-format' variables.
;;;
;;; The shape and size of each breadcrumb groups may be tweaked via
;;; `breadcrumb-imenu-max-length', `breadcrumb-project-max-length',
;;; `breadcrumb-imenu-crumb-separator', and
;;; `breadcrumb-project-crumb-separator'.
;;;
;;; The structure each the breadcrumbs varies depending on whether
;;; either project.el and imenu.el (or both) can do useful things for
;;; your buffer.
;;;
;;; For Project breadcrumbs, this depends on whether project.el's
;;; `project-current' can guess what project the current buffer
;;; belongs to.
;;;
;;; For Imenu breadcrumbs, this varies.  Depending on the major-mode
;;; author's taste, the Imenu tree (in variable `imenu--index-alist')
;;; may have different structure.  Sometimes, minor mode also tweak
;;; the Imenu tree in useful ways.  For example, with recent Eglot (I
;;; think Eglot 1.14+), managed buffers get extra region info added to
;;; it, which makes Breadcrumb show "richer" paths.
;;;
;;;; Implementation notes:
;;;
;;; This _should_ be faster than which-func.el due some caching
;;; strategies.  One of these strategies occurs in `bc--ipath-alist',
;;; which takes care not to over-call `imenu--make-index-alist', which
;;; could be slow (in fact very slow if an external process needs to
;;; be contacted).  The variable `breadcrumb-idle-delay' controls
;;; that.  Another cache occurs in `bc--ipath-plain-cache' second is
;;; just a simple "space-for-speed" cache.
;;;
;;; Breadcrumb uses the double-dashed Imenu symbols
;;; `imenu--index-alist' and `imenu--make-index-alist'.  There's
;;; really no official API here.  It's arguable that, despite the
;;; name, these aren't really internal symbols (the much older
;;; which-func.el library makes liberal use of them, for example).
;;;
;;;; Todo:
;;;
;;; Make more clicky buttons in the headerline to do whatever
;;;

;;; Code:
(require 'cl-lib)
(require 'imenu)
(require 'project)

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

(defun bc--ipath-rich (index-alist pos)
  "Compute ipath for rich `imenu--index-alist' structures.
These structures have a `breadcrumb-region' property on every
node."
  (cl-labels
      ((search (nodes &optional ipath)
         (cl-loop
          for n in nodes
          for reg = (get-text-property 0 'breadcrumb-region (car n))
          when (<= (car reg) pos (cdr reg))
          return (search (cdr n) (cons (car n) ipath))
          finally (cl-return ipath))))
    (nreverse (search index-alist))))

(defvar-local bc--ipath-plain-cache nil
  "A cache for `breadcrumb--ipath-plain'.")

(defun bc--ipath-plain (index-alist pos)
  "Compute ipath for plain `imenu--index-alist' structures.
These structures don't have a `breadcrumb-region' property on."
  (cl-labels ((dfs (n &optional ipath)
                (setq ipath (cons (car n) ipath))
                (if (consp (cdr n))
                    (mapc (lambda (n) (dfs n ipath)) (cdr n))
                  (setq bc--ipath-plain-cache
                        (vconcat bc--ipath-plain-cache
                                 `[,(cons (cdr n) ipath)])))))
    (unless bc--ipath-plain-cache
      (mapc #'dfs index-alist)
      (setq bc--ipath-plain-cache (cl-sort bc--ipath-plain-cache #'< :key #'car)))
    (unless (< pos (car (aref bc--ipath-plain-cache 0)))
      (let ((res (bc--bisect bc--ipath-plain-cache pos :key #'car :from-end t)))
        (unless (zerop res) (reverse (cdr (elt bc--ipath-plain-cache (1- res)))))))))

(defun bc-ipath (index-alist pos)
  "Get breadcrumb for position POS given INDEX-ALIST."
  (if (get-text-property 0 'breadcrumb-region (caar index-alist))
      (bc--ipath-rich index-alist pos)
    (bc--ipath-plain index-alist pos)))

(defvar bc--header-line-key [header-line mouse-1])

(defun bc--format-node (p)
  (let ((reg (get-text-property 0 'breadcrumb-region p)))
    (if reg
        (propertize p
                    'mouse-face 'header-line-highlight
                    'help-echo "Go here"
                    'keymap (let ((m (make-sparse-keymap)))
                              (define-key
                               m bc--header-line-key
                               (lambda (&rest _e)
                                 (interactive "@")
                                 (with-current-buffer (window-buffer)
                                   (push-mark)
                                   (goto-char (car reg)))))
                              m))
      p)))

(defvar bc-idle-time 1
  "Control idle time before requesting new breadcrumbs.")

(defvar-local bc--idle-timer nil
  "Timer used by `breadcrumb--ipath-alist'.")

(defvar-local bc--last-update-tick 0
  "Last time `breadcrumb--ipath-alist' asked for an update.")

(defun bc--ipath-alist ()
  "Return `imenu--index-alist', maybe arrange for its update."
  (let ((nochangep (= (buffer-chars-modified-tick) bc--last-update-tick))
        (buf (current-buffer)))
    (unless nochangep
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
    imenu--index-alist))

(defgroup breadcrumb nil
  "One-liner indication of where you are in the maze."
  :prefix "breadcrumb-"
  :group 'convenience)

(defcustom bc-project-max-length 40
  "Soft cutoff for `breadcrumb-project-crumbs'." :type 'natnum)

(defcustom bc-project-crumb-separator "/"
  "Separator for `breadcrumb-project-crumbs'." :type 'string)

(defcustom bc-imenu-max-length 40
  "Soft cutoff for `breadcrumb-imenu-crumbs'." :type 'natnum)

(defcustom bc-imenu-crumb-separator " > "
  "Separator for `breadcrumb-project-crumbs'." :type 'string)

;;;###autoload
(defun breadcrumb-imenu-crumbs ()
  "Describe point inside the Imenu tree of current file."
  (when-let ((alist (bc--ipath-alist)))
    (bc--summarize
     (cl-loop for p in (bc-ipath alist (point))
              collect (bc--format-node p))
     bc-imenu-max-length
     bc-imenu-crumb-separator)))

(defun bc--summarize (crumbs cutoff separator)
  (let ((rcrumbs
         (cl-loop
             for available = (- cutoff used)
             for (c . more) on (reverse crumbs)
             for seplen = (if more (length separator) 0)
             for shorten-p = (unless (get-text-property 0 'bc-dont-shorten c)
                               (> (+ (length c) seplen) available))
             for toadd = (if shorten-p (substring c 0 1) c)
             sum (+ (length toadd) seplen) into used
             collect toadd)))
    (string-join (reverse rcrumbs) separator)))

(defvar-local bc--cached-project-crumbs nil)

;;;###autoload
(cl-defun breadcrumb-project-crumbs ()
  "Describing the current file inside project."
  (or bc--cached-project-crumbs
      (setq bc--cached-project-crumbs
            (when-let ((p (project-current)))
              (bc--summarize
               (cons (propertize (project-name p) 'bc-dont-shorten t)
                     (split-string
                      (file-relative-name (or (buffer-file-name)
                                              default-directory)
                                          (project-root p))
                      "/"))
               bc-project-max-length
               bc-project-crumb-separator)))))

(defun bc--header-line ()
  "Helper for `breadcrumb-headerline-mode'."
  (let ((x (cl-remove-if
            #'seq-empty-p (mapcar #'funcall
                                  '(bc-project-crumbs bc-imenu-crumbs)))))
    (mapconcat #'identity x " : ")))

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
            for pos = (or (car (get-text-property 0 'breadcrumb-region (car n)))
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
      (goto-char choice))))

(provide 'breadcrumb)
;;; breadcrumb.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("bc-" . "breadcrumb-"))
;; End:
