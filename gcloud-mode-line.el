;;; gcloud-mode-line.el --- Display current gcloud project in Emacs mode line

;; Copyright (C) 2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Created: 2 Oct 2019
;; Keywords: mode-line gcloud
;; Homepage: https://github.com/terjesannum/emacs-gcloud-mode-line

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

;; See https://github.com/terjesannum/emacs-gcloud-mode-line/blob/master/README.md

;;; Code:
(defvar gcloud-command "gcloud" "gcloud binary")
(defvar gcloud-timer nil)
(defvar gcloud-string "")
(defvar gcloud-update-interval 10 "Number of seconds between background mode-line updates")
(defvar gcloud-string-format " %P" "String to display in mode-line (%P = project")

(defun gcloud-run (&rest args)
  "Run gcloud command"
  (with-temp-buffer
    (let ((default-directory "~"))
      (if (and (executable-find gcloud-command)
               (= (apply 'call-process gcloud-command nil t nil args) 0))
          (replace-regexp-in-string "\n\\'" "" (buffer-string))
        "n/a"))))

(defun make-gcloud-string (project)
  "Create gcloud string to display"
  (replace-regexp-in-string "%P" project gcloud-string-format t))

(defun gcloud-update ()
  "Update gcloud mode-line string with current project"
  (interactive)
  (let ((project (gcloud-run "config" "get-value" "project")))
    (setq gcloud-string (make-gcloud-string project))
    (force-mode-line-update t)))

(define-minor-mode gcloud-mode
  "Add gcloud project info to the mode line"
  :global t
  (when (not global-mode-string) (setq global-mode-string '("")))
  (when gcloud-timer (cancel-timer gcloud-timer))
  (if (not gcloud-mode)
      (setq global-mode-string
            (delq 'gcloud-string global-mode-string))
    (add-to-list 'global-mode-string 'gcloud-string t)
    (when (> gcloud-update-interval 0)
      (setq gcloud-timer
            (run-at-time nil gcloud-update-interval
                         'gcloud-update)))
    (gcloud-update)))

(provide 'gcloud-mode-line)

;;; gcloud-mode-line.el ends here
