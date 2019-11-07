;;; gcloud-mode.el --- Global minor-mode to change gcloud project and display current project in Emacs mode line

;; Copyright (C) 2019 Terje Sannum

;; Author: Terje Sannum <terje@offpiste.org>
;; Created: 2 Oct 2019
;; Keywords: mode-line gcloud
;; Homepage: https://github.com/terjesannum/emacs-gcloud-mode

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

;; See https://github.com/terjesannum/emacs-gcloud-mode/blob/master/README.md

;;; Code:
(defvar gcloud-mode-command "gcloud" "gcloud executable")
(defvar gcloud-mode-timer nil)
(defvar gcloud-mode-mode-line-string "")
(defvar gcloud-mode-mode-line-update-interval 10 "Number of seconds between background mode-line updates")
(defvar gcloud-mode-mode-line-string-format " [gcp:%P]" "String to display in mode-line (%P = project")
(defvar gcloud-mode-submap)
(define-prefix-command 'gcloud-mode-submap)
(define-key gcloud-mode-submap "p" 'gcloud-mode-set-project)
(defvar gcloud-mode-keybind (kbd "C-c C-g") "Keybind where gcloud-mode-submap is assigned")

(defun gcloud-mode-run-gcloud-command (&rest args)
  "Run gcloud command"
  (with-temp-buffer
    (let ((default-directory "~"))
      (if (and (executable-find gcloud-mode-command)
               (= (apply 'call-process gcloud-mode-command nil t nil args) 0))
          (replace-regexp-in-string "\n\\'" "" (buffer-string))
        "n/a"))))

(defun gcloud-mode-projects ()
  "Get list of projects"
  (split-string (gcloud-mode-run-gcloud-command "projects" "list" "--format=value(project_id)")))

(defun gcloud-mode-gcloud-string (project)
  "Create gcloud string to display in mode-line"
  (replace-regexp-in-string "%P" project gcloud-mode-mode-line-string-format t))

(defun gcloud-mode-set-project (project)
  "Set current gcloud project"
  (interactive
   (list
    (completing-read "Project: " (gcloud-mode-projects) nil t)))
  (gcloud-mode-run-gcloud-command "config" "set" "project" project)
  (gcloud-mode-mode-line-update))

(defun gcloud-mode-mode-line-update ()
  "Update gcloud mode-line string with current project"
  (interactive)
  (let ((project (gcloud-mode-run-gcloud-command "config" "get-value" "project")))
    (setq gcloud-mode-mode-line-string (gcloud-mode-gcloud-string project))
    (force-mode-line-update t)))

(define-minor-mode gcloud-mode
  "Change gcloud project and display current project in the mode line"
  :global t
  :keymap `((,gcloud-mode-keybind . ,gcloud-mode-submap))
  (when (not global-mode-string) (setq global-mode-string '("")))
  (when gcloud-mode-timer (cancel-timer gcloud-mode-timer))
  (if (not gcloud-mode)
      (setq global-mode-mode-line-string
            (delq 'gcloud-mode-mode-line-string global-mode-string))
    (add-to-list 'global-mode-string 'gcloud-mode-mode-line-string t)
    (when (> gcloud-mode-mode-line-update-interval 0)
      (setq gcloud-mode-timer
            (run-at-time nil gcloud-mode-mode-line-update-interval
                         'gcloud-mode-mode-line-update)))
    (gcloud-mode-mode-line-update)))

(provide 'gcloud-mode)

;;; gcloud-mode.el ends here
