;;; ne2wm-plugin-vcs.el --- Plugins for VCS (git, hg, ...)

;; Copyright (C) 2013 Takafumi Arakaki

;; Author: Takafumi Arakaki <aka.tkf at gmail.com>

;; This file is NOT part of GNU Emacs.

;; ne2wm-plugin-vcs.el is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; ne2wm-plugin-vcs.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with ne2wm-plugin-vcs.el.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(eval-when-compile (require 'cl))
(require 'e2wm-vcs)

(defvar ne2wm:def-plugin-vcs-backends
  '((magit
     :get-top-dir magit-get-top-dir
     :run-status (lambda (dir _) (magit-status (file-name-as-directory dir))))
    (monky
     :get-top-dir (lambda (default-directory)
                    (ignore-errors (monky-get-root-dir)))
     :run-status (lambda (_ __) (monky-status))))
  "Alist of supported VCS backends.  Each key is a name of
backend (e.g., `magit') and each value is a plist of the
following keys:

`:get-top-dir'
    Function that returns root directory of the VCS repository.
    It should take one argument: CWD.
`:run-status'
    Function that creates VCS status buffer and set it to the
    current window.
    It should take two arguments: CWD TOP-DIR.
")

(defvar ne2wm:def-plugin-vcs-get-buffer 'current-buffer
  "You may set this to `e2wm:history-get-main-buffer'.")

(defun ne2wm:def-plugin-vcs-top-dir-all (cwd)
  (loop for (name . props) in ne2wm:def-plugin-vcs-backends
        for func = (plist-get props :get-top-dir)
        when (and (functionp func)
                  (funcall func cwd))
        collect `(:name ,name :top-dir ,it ,@props)))

(defun ne2wm:def-plugin-vcs-get-backend (cwd)
  (let ((depth (lambda (elem) (length (plist-get elem :top-dir)))))
    (car
     (sort (ne2wm:def-plugin-vcs-top-dir-all (or cwd default-directory))
           (lambda (x y) (> (funcall depth x) (funcall depth y)))))))

(defun ne2wm:def-plugin-vcs-run (frame wm winfo key)
  (let* ((buf (funcall ne2wm:def-plugin-vcs-get-buffer))
         (dir (or (e2wm:aand buf
                             (buffer-file-name it)
                             (file-name-directory it))
                  default-directory))
         (backend (ne2wm:def-plugin-vcs-get-backend dir))
         (topdir (plist-get backend :top-dir))
         (body-func (plist-get backend key)))
    (e2wm:with-advice
     (cond
      (topdir
       (with-selected-window (wlf:get-window wm (wlf:window-name winfo))
         (with-current-buffer buf
           (funcall body-func dir topdir))
         (wlf:set-buffer wm (wlf:window-name winfo)
                         (window-buffer (selected-window)))))
      (t
       (wlf:set-buffer wm (wlf:window-name winfo)
                       (e2wm:def-plugin-vcs-na-buffer "N/A")))))))

(defun ne2wm:def-plugin-vcs-status (frame wm winfo)
  (ne2wm:def-plugin-vcs-run frame wm winfo :run-status))

(e2wm:plugin-register 'vcs-status
                      "VCS Status"
                      'ne2wm:def-plugin-vcs-status)

(provide 'ne2wm-plugin-vcs)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; ne2wm-plugin-vcs.el ends here
