;;; consult-tramp.el --- Tramp ivy interface for ssh, docker, -*- lexical-binding: t; -*-

;; Copyright (C) 2022 by Aya Igarashi

;; Author: Aya Igarashi
;; URL: https://github.com/Ladicle/consult-tramp
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (consult "0.16") (tramp "2.5.2"))

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

;; consult-tramp provides interfaces of Tramp
;; You can also use tramp with consult interface as root
;; If you use it with docker-tramp, you can also use docker with consult interface

;;; Code:

(require 'consult)
(require 'tramp)

(defgroup consult-tramp nil
  :group 'consult)

(defcustom consult-tramp-method "scpx"
  "Default method"
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-ssh-config "~/.ssh/config"
  "Path to the ssh configuration"
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-enable-shosts t
  "Use known_hosts for completion"
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-known-hosts "~/.ssh/known_hosts"
  "Path to the ssh configuration"
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-enable-docker t
  "Use docker containers for completion"
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-path "~"
  "Initial directory when connecting."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-docker-path "/"
  "Initial directory when connecting with /docker:<container>:."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-sudo-path "/"
  "Initial directory when connecting with /sudo:root@localhost:."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-extra-targets '()
  "A list to manually add extra targets."
  :group 'consult-tramp
  :type 'string)

(defun consult-tramp--candidates ()
  "Generate tramp candidates from ssh configuration, docker and extra targets."
  (let ((hosts consult-tramp-extra-targets))
    ;; SSH config
    (dolist (cand (tramp-parse-sconfig consult-tramp-ssh-config))
      (let ((user (if (car cand) (concat (car cand) "@")))
            (host (car (cdr cand))))
        (if host
            (push (concat "/" consult-tramp-method
                          ":" user host
                          ":" consult-tramp-path) hosts))))
    ;; Known hosts
    (when consult-tramp-enable-shosts
      (dolist (cand (tramp-parse-shosts consult-tramp-known-hosts))
        (let ((user (if (car cand) (concat (car cand) "@")))
              (host (car (cdr cand))))
          (if host
              (push (concat "/" consult-tramp-method
                            ":" user host
                            ":" consult-tramp-path) hosts)))))
    ;; Docker
    (when (and (require 'docker-tramp nil t) consult-tramp-enable-docker)
      (setq docker-tramp-use-names t)
      (dolist (cand (docker-tramp--parse-running-containers ""))
        (let ((user (if (not (string-empty-p (car cand)))
                        (concat (car cand) "@")))
              (host (car (cdr cand))))
          (push (concat "/docker:" user host ":/") hosts))))
    ;; Root
    (push (concat "/sudo:root@localhost:" consult-tramp-sudo-path) hosts)
    (reverse hosts)))

;;;###autoload
(defun consult-tramp ()
  "Connects to the selected target via tramp method."
  (interactive)
  (let ((dst (consult--read (consult-tramp--candidates) :prompt "Tramp: ")))
    (funcall #'find-file dst)))

(provide 'consult-tramp)
;;; consult-tramp.el ends here
