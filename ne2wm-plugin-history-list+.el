;;; ne2wm-plugin-history-list+.el --- history-list plugin for many windows

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

(eval-when-compile (require 'cl)
                   (require 'howm nil t))
(require 'e2wm)

(defvar ne2wm:c-plugin-history-list+-mode-line-format
  '("-" mode-line-mule-info " " mode-line-position "-%-")
  "Mode line for history-list+.  See `mode-line-format'.")

(defvar ne2wm:def-plugin-history-list+-wname-list nil
  "List of the window names to show in the history-list+ plugin.
Use `ne2wm:def-plugin-history-list+-setup' to set this value.")


(defvar ne2wm:def-plugin-history-list+-pointer-list nil
  "List of the window markers to use in the history-list+ plugin.
Use `ne2wm:def-plugin-history-list+-setup' to set this value.")


(make-variable-frame-local 'ne2wm:def-plugin-history-list+-wname-list)
(make-variable-frame-local 'ne2wm:def-plugin-history-list+-pointer-list)


(defun ne2wm:def-plugin-history-list+-setup (wnames pointers)
  "Setup history-list+ for current perspective.

Call this function from the `:start' function.

WNAMES is the list of window names (symbol).
Example: '(left right).

POINTERS ist the list of pointers (string).
Example: '(\"<--\" \"-->\")."
  (setq ne2wm:def-plugin-history-list+-wname-list wnames)
  (setq ne2wm:def-plugin-history-list+-pointer-list pointers))


(defun ne2wm:def-plugin-history-list+-howm-title (buffer-or-name)
  "[internal] helper for `ne2wm:def-plugin-history-list+-pretty-buffer-name'."
  (let ((buffer (get-buffer buffer-or-name)))
    (when (and (featurep 'howm) buffer)
      (save-excursion
        (with-current-buffer buffer
          (when howm-mode
            (goto-char (point-min))
            (e2wm:aif (howm-title-at-current-point)
                (concat "= " it " - " (buffer-name buffer)))))))))


(defun ne2wm:def-plugin-history-list+-pretty-buffer-name (buffer-or-name)
  "Prettify buffer-name of the BUFFER-OR-NAME."
  (let* ((buffer (get-buffer buffer-or-name))
         (name (buffer-name buffer))
         (howm-title))
    (cond
     ((null buffer)
      buffer-or-name)                   ; should I?
     ((setq howm-title
            (ne2wm:def-plugin-history-list+-howm-title buffer))
      howm-title)
     ((eql (buffer-local-value 'major-mode buffer) 'dired-mode)
      (concat name "/"))
     (t name))))


(defun ne2wm:def-plugin-history-list+ (frame wm winfo)
  "History-list+ plugin definition."
  (let ((wname (wlf:window-name winfo))
        (win (wlf:window-live-window winfo))
        (buf (get-buffer " *WM:History+*"))
        (line-format
         (format "%%%ds %%2s %%s %%s\n"
                 (loop for s in ne2wm:def-plugin-history-list+-pointer-list
                       sum (length s))))
        (space-list
         (loop for pointer in ne2wm:def-plugin-history-list+-pointer-list
               for len = (length pointer)
               collect (apply #'concat (loop for -no-use- from 1 to len
                                             collect " "))))
        current-pos)
    (unless (and buf (buffer-live-p buf))
      (setq buf (get-buffer-create " *WM:History+*"))
      (with-current-buffer buf
        ;; (e2wm:def-plugin-history-list2-mode)
        (setq buffer-read-only t)
        (setq truncate-lines t)
        (buffer-disable-undo buf)
        (hl-line-mode 1)))
    (with-current-buffer buf
      (unwind-protect
          (progn
            (setq buffer-read-only nil)
            (setq current-pos (point))
            (erase-buffer)
            (goto-char (point-min))
            (let* ((history (e2wm:history-get))
                   (history-backup (reverse (e2wm:history-get-backup)))
                   (buf-list (mapcar
                              (lambda (n) (wlf:get-buffer wm n))
                              ne2wm:def-plugin-history-list+-wname-list))
                   (main-buf   (nth 0 buf-list))
                   (second-buf (nth 1 buf-list))
                   (cnt 1))
              (loop for h in (append history-backup history)
                    for name =
                    (ne2wm:def-plugin-history-list+-pretty-buffer-name h)
                    do (insert
                        (e2wm:tp
                         (e2wm:rt
                          (format
                           line-format
                           (apply
                            #'concat
                            (loop for buf in buf-list
                                  for pointer in
                                  ne2wm:def-plugin-history-list+-pointer-list
                                  for space in space-list
                                  collect (if (eql h buf) pointer space)))
                           cnt name
                           (if (buffer-modified-p h) "*" ""))
                          (cond
                           ((eql h main-buf)
                            'e2wm:face-history-list-select1)
                           ((eql h second-buf)
                            'e2wm:face-history-list-select2)
                           ((memql h buf-list)
                            'e2wm:face-history-list-normal)
                           (t
                            'e2wm:face-history-list-normal)))
                         'e2wm:buffer h))
                    (incf cnt))
              (goto-char current-pos)
              (setq mode-line-format
                    ne2wm:c-plugin-history-list+-mode-line-format)
              (setq header-line-format
                    (format "Buffer History [%i]" (1- cnt)))))
        (setq buffer-read-only t)))
    (wlf:set-buffer wm wname buf)
    (when win (set-window-point win current-pos))))


(e2wm:plugin-register 'history-list+
                     "History List+"
                     'ne2wm:def-plugin-history-list+)


(defun ne2wm:def-plugin-history-list+-current-wname ()
  "Return current window name if it is found in the list
`ne2wm:def-plugin-history-list+-wname-list'."
  (ne2wm:current-wname-in-list
   ne2wm:def-plugin-history-list+-wname-list))


(defun ne2wm:def-plugin-history-list+-current-or-nth-wname (wnum)
  "Return current or WNUM-th window name if it is found in the list."
  (if wnum
      (nth (1- wnum) ne2wm:def-plugin-history-list+-wname-list)
    (ne2wm:def-plugin-history-list+-current-wname)))


(defun ne2wm:def-plugin-history-list+-other-wname (&optional offset)
  "Return window name of the \"other\" window.

\"Other\" window means the next OFFSET-th window of the current
one defined in the list `ne2wm:def-plugin-history-list+-wname-list'."
  (let* ((wname (ne2wm:def-plugin-history-list+-current-wname))
         (wname-list ne2wm:def-plugin-history-list+-wname-list)
         (len (length wname-list))
         (index (- len             ; what is the best way to do this??
                   (length (memql wname wname-list)))))
    (nth (% (+ (or offset 1) index) len) wname-list)))


(defun ne2wm:def-plugin-history-list+-update (wname direcfunc)
  "Change the buffer of the WNAME window to the previous/next one.

The direction (previous or next) is determined by the DIRECFUNC
function.  This argument is either `e2wm:history-get-next' or
`e2wm:history-get-next'."
  (e2wm:pst-buffer-set
   wname (funcall direcfunc (e2wm:pst-buffer-get wname)))
  (e2wm:plugin-exec-update-by-plugin-name
   (selected-frame) (e2wm:pst-get-wm) 'history-list+))


(defun ne2wm:def-plugin-history-list+-forward-current-command (&optional wnum)
  "Change the buffer of the current window to the next item in the history.

When the numerical prefix argument WNUM is given, change the WNUM-th
window in the `ne2wm:def-plugin-history-list+-wname-list'. Note that
WNUM is 1-origin.

Use `ne2wm:def-plugin-history-list+-forward-current-command' to
change the buffer to the opposite direction in the history list."
  (interactive "P")
  (ne2wm:def-plugin-history-list+-update
   (ne2wm:def-plugin-history-list+-current-or-nth-wname wnum)
   #'e2wm:history-get-next))


(defun ne2wm:def-plugin-history-list+-back-current-command (&optional wnum)
  "Change the buffer of the current window to the previous item in the history.

See also: `ne2wm:def-plugin-history-list+-forward-current-command'."
  (interactive "P")
  (ne2wm:def-plugin-history-list+-update
   (ne2wm:def-plugin-history-list+-current-or-nth-wname wnum)
   #'e2wm:history-get-prev))


(defun ne2wm:def-plugin-history-list+-forward-other-command (&optional offset)
  "Change the buffer of the other window to the next item in the history.

Other window is the next window of the current window in the
window list `ne2wm:def-plugin-history-list+-wname-list'.

When the numeric prefix argument OFFSET is given, other window is
OFFSET-th next window in the window list
`ne2wm:def-plugin-history-list+-wname-list'.

Use `ne2wm:def-plugin-history-list+-back-other-command' to
change the buffer to the opposite direction in the history list."
  (interactive "P")
  (ne2wm:def-plugin-history-list+-update
   (ne2wm:def-plugin-history-list+-other-wname offset)
   #'e2wm:history-get-next))


(defun ne2wm:def-plugin-history-list+-back-other-command (&optional offset)
  "Change the buffer of the other window to the previous item in the history.

See also: `ne2wm:def-plugin-history-list+-forward-other-command'."
  (interactive "P")
  (ne2wm:def-plugin-history-list+-update
   (ne2wm:def-plugin-history-list+-other-wname offset)
   #'e2wm:history-get-prev))


(provide 'ne2wm-plugin-history-list+)
;;; ne2wm-plugin-history-list+.el ends here
