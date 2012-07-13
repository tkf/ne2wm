;;; ne2wm-utils.el --- utility functions

;; Copyright (C) 2012  Takafumi Arakaki

;; Author: Takafumi Arakaki
;; Keywords: tools, window manager

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl))
(require 'e2wm)
(require 'e2wm-vcs)

(require 'ne2wm-pst-magit+)
(require 'ne2wm-pst-monky+)

;;; Commands

(defun ne2wm:toggle-sub (&optional move-buffer)
  "Toggle on/off of e2wm sub window.

If the universal prefix argument is given, the current buffer
will be shown in the next window."
  (interactive "P")
  (let* ((prev-buf (current-buffer))
         (wm (e2wm:pst-get-wm))
         ;; When *not* in sub, do not specify NEXT-WINDOW so that
         ;; `e2wm:pst-window-toggle' let me stay in the current
         ;; window.
         (next-window
          (when (eq (wlf:get-window-name wm (selected-window)) 'sub)
            (e2wm:$pst-main (e2wm:pst-get-instance)))))
    (e2wm:aif (e2wm:pst-window-toggle 'sub t next-window)
        (when move-buffer
          (wlf:set-buffer wm it prev-buf)))))

(defun ne2wm:hide-sub ()
  "Hide e2wm sub window."
  (interactive)
  (let ((wm (e2wm:pst-get-wm)))
    (when (eq (wlf:get-window-name wm (selected-window)) 'sub)
      (wlf:select wm (e2wm:$pst-main (e2wm:pst-get-instance))))
    (wlf:hide wm 'sub)))

(defun ne2wm:select-sub ()
  "Focus e2wm sub window."
  (interactive)
  (let ((wm (e2wm:pst-get-wm)))
    (wlf:select wm 'sub)))

(defun ne2wm:pst-update-windows-command ()
  "Reset window configurations.

This is an extend version of `e2wm:pst-update-windows-command'.

Additional feature:
* Move buffer shown in the main window to the top of the history list.
"
  (interactive)
  (let* ((wm (e2wm:pst-get-wm))
         (wname (e2wm:$pst-main (e2wm:pst-get-instance)))
         (buf (wlf:get-buffer wm wname))
         (curwname (wlf:get-window-name wm (selected-window))))
    (e2wm:history-add buf)
    (e2wm:pst-update-windows-command)
    (e2wm:pst-window-select curwname))) ; avoid changing window focus


(defun ne2wm:open-vcs ()
  "Open VCS perspective depending on the VCS used in the project.

Currently, only Magit (Git) and Monky (Mercurial) are supported."
  (interactive)
  (let ((dir default-directory))
    (cond
     ;; magit
     ((and dir (magit-get-top-dir dir))
      (ne2wm:dp-magit+))
     ;; monky
     ((condition-case err
          (monky-get-root-dir)
        (error nil))
      (ne2wm:dp-monky+))
     ;; FIXME: support SVN
     ;; otherwise
     (t
      (message "Not in VCS")))))


(defun ne2wm:win-ring-get (&optional frame):
  (e2wm:frame-param-get 'ne2wm:win-ring frame))

(defun ne2wm:win-ring-set (val &optional frame):
  (e2wm:frame-param-set 'ne2wm:win-ring val frame))

(defun ne2wm:rorate-list-right (seq offset)
  (if (<= offset 0)
      seq
    (ne2wm:rorate-list-right
     (append (last seq) (nbutlast seq)) (1- offset))))

(defun ne2wm:rorate-list-left (seq offset)
  (if (<= offset 0)
      seq
    (ne2wm:rorate-list-left (append (cdr seq) (list (car seq))) (1- offset))))

(defun ne2wm:rorate-list (seq &optional offset)
  "[internal]"
  (setq offset (or offset 1))
  (if (<= offset 0)
      (ne2wm:rorate-list-left seq (- offset))
    (ne2wm:rorate-list-right seq offset)))

(defun ne2wm:win-ring-rotate ()
  (interactive)                         ; FIXME: use prefix arg
  (loop with ring = (ne2wm:win-ring-get)
        for wname in ring
        for buf in (ne2wm:rorate-list (mapcar #'e2wm:pst-buffer-get ring))
        do (e2wm:pst-buffer-set wname buf))
  (e2wm:pst-update-windows))


(defun ne2wm:find-next-in-seq (seq item &optional offset)
  "[internal]  Return OFFSET-next position from ITEM in SEQ.

If OFFSET is omitted or nil, it is assumed to be 1.

Examples:
  (ne2wm:find-next-in-seq '(0 1 2 3) 1)     ; => 2
  (ne2wm:find-next-in-seq '(0 1 2 3) 3)     ; => 0
  (ne2wm:find-next-in-seq '(0 1 2 3) 1 2)   ; => 3
  (ne2wm:find-next-in-seq '(0 1 2 3) 1 -1)  ; => 0
"
  (nth (loop for current in seq
             for i from 0
             when (eql current item)
             return (mod (+ i (or offset 1)) (length seq)))
       seq))


(defun ne2wm:win-ring-push (arg)
  (interactive "p")
  (let* ((ring (ne2wm:win-ring-get))
         (cur-wname (ne2wm:current-wname-in-list ring))
         (next-wname
          (when cur-wname
            (ne2wm:find-next-in-seq ring cur-wname arg))))
    (if (not next-wname)
        (message "Not in win-ring windows %S." ring)
      (let ((cur-buf  (e2wm:pst-buffer-get cur-wname))
            (next-buf (e2wm:pst-buffer-get next-wname)))
        (e2wm:pst-buffer-set cur-wname next-buf)
        (e2wm:pst-buffer-set next-wname cur-buf nil t))
      (e2wm:pst-update-windows))))



;;; Utility functions for e2wm

(defun ne2wm:current-wname-in-list (wname-list)
  "Return current window name if it is found in WNAME-LIST."
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (loop for wname in wname-list
          when (eql curwin (wlf:get-window wm wname))
          return wname)))

(defun ne2wm:current-wname ()
  "Return current window name."
  (let ((wm (e2wm:pst-get-wm))
        (curwin (selected-window)))
    (loop for winfo in (wlf:wset-winfo-list wm)
          for wname = (wlf:window-name winfo)
          when (eql curwin (wlf:get-window wm wname))
          return wname)))

(defun ne2wm:pst-buffer-set-current-window (buffer)
  "Set the BUFFER in the current window."
  (e2wm:aif (ne2wm:current-wname)
      (e2wm:pst-buffer-set it buffer)
    (message "Failed to set the buffer %S in the current window" it)))


;;; Misc

(defun ne2wm:load-files (&optional regex dir)
  (let* ((dir (or dir (file-name-directory load-file-name)))
         (regex (or regex ".+"))
         (files (and
                 (file-accessible-directory-p dir)
                 (directory-files dir 'full regex))))
    (mapc #'load files)))


(provide 'ne2wm-utils)
;;; ne2wm-utils.el ends here
