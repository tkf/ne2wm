;;; ne2wm-buffer-p.el --- ne2wm:*-buffer-p functions

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

(require 'e2wm)
(require 'howm nil t)

(defvar ne2wm:require-wide-window-regexps
  `(,(regexp-quote "*Calendar*")
    ,(regexp-quote " *Org tags*")))

(defun ne2wm:require-wide-window-p (buf)
  (let ((regexp
         (mapconcat 'identity ne2wm:require-wide-window-regexps "\\|")))
    (string-match-p regexp (buffer-name buf))))

;;; monky/magit
(defun ne2wm:vcs-status-buffer-p (buf)
  "Return non-nil when the buffer BUF is VCS status buffer."
  (let ((buf-name (buffer-name buf)))
    (when (or (string-match "\\*\\(magit\\|monky\\): .*\\*" buf-name)
              (string-match "\\*monky-queue\\*" buf-name))
      (e2wm:message ">>> '%s' is magit or monky status buffer" buf-name)
      t)))

(defun ne2wm:vcs-log-buffer-p (buf)
  "Return non-nil when the buffer BUF is VCS log buffer."
  (let ((buf-name (buffer-name buf)))
    (when (string-match "\\*\\(magit\\|monky\\|vc-change\\)-log\\*"
                        buf-name)
      (e2wm:message ">>> '%s' is magit or monky log buffer" buf-name)
      t)))

(defun ne2wm:vcs-commit-buffer-p (buf)
  "Return non-nil when the buffer BUF is VCS commit buffer."
  (let ((buf-name (buffer-name buf)))
    (when (string-match "\\*\\(magit\\|monky\\)-commit\\*" buf-name)
      (e2wm:message ">>> '%s' is magit- or monky-commit buffer" buf-name)
      t)))


;;; howm
(defun ne2wm:howm-summary-buffer-p (buf)
  "Return non-nil when the buffer BUF is howm summary (*howmS*) buffer."
  (let ((buf-name (buffer-name buf)))
    (when (and (boundp 'howm-view-summary-name)
               (string-equal howm-view-summary-name buf-name))
      (e2wm:message ">>> '%s' is howm summary buffer" buf-name)
      t)))

(defun ne2wm:howm-contents-buffer-p (buf)
  "Return non-nil when the buffer BUF is howm contents (*howmC*) buffer."
  (let ((buf-name (buffer-name buf)))
    (when (and (boundp 'howm-view-contents-name)
               (string-equal howm-view-contents-name buf-name))
      (e2wm:message ">>> '%s' is howm contents buffer" buf-name)
      t)))


(provide 'ne2wm-buffer-p)
;;; ne2wm-buffer-p.el ends here
