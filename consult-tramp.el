;;; consult-tramp.el --- Tramp consult interface for ssh, docker, -*- lexical-binding: t; -*-

;; Copyright (C) 2022 by Aya Igarashi

;; Author: Aya Igarashi
;; URL: https://github.com/Ladicle/consult-tramp
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (consult "0.16"))

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

;; This package provides an interface be able to interact with remote hosts via
;; consult and tramp. Candidates are selected from a number of pre-defined sources which currently are:
;;
;; - User's SSH config file (defined via `consult-tramp-ssh-config')
;; - User's known hosts file (defined via `consult-tramp-known-hosts')
;; - Currently running docker containers
;;
;; It's also possible to include/exclude these sources at the user's will.
;;
;; For creating other sources, reference the current examples to create a new
;; source and add it to the `consult-tramp-sources' list.
;;
;; After selecting a host, the default action defined by
;; `consult-tramp-default-action' will be called using the
;; defined host as the `default-directory'. If prefix arg is specified, then the
;; function will be called interactively.

;;; Code:

(require 'consult)
(require 'tramp)

(declare-function docker-tramp--parse-running-containers "docker-tramp")

(defgroup consult-tramp nil
  "Tramp interface using consult."
  :prefix "consult-tramp-"
  :group 'consult)

(defvar consult-tramp-history nil
  "The history list for `consult-tramp'.")

(defcustom consult-tramp-method "scpx"
  "Default connection protocol to use to connect to remote hosts."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-ssh-config "~/.ssh/config"
  "Path to the ssh configuration."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-known-hosts "~/.ssh/known_hosts"
  "Path to the known_hosts file."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-path "~"
  "Default path to connect to for remote hosts."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-docker-path "/"
  "Path to connect to the docker container."
  :group 'consult-tramp
  :type 'string)

(defcustom consult-tramp-local-hosts '("/sudo:root@localhost:/")
  "A list of local hosts to include."
  :group 'consult-tramp
  :type '(repeat string))

(defcustom consult-tramp-default-action #'find-file
  "Default action to run after selecting a host using `consult-tramp'."
  :type 'function)

(defface consult-tramp-host
  '((t :inherit font-lock-constant-face))
  "Face used to highlight hosts in `consult-tramp'.")

(defun consult-tramp--parse-config (config)
  "Given a CONFIG, parse the hosts from it and return the results as a list."
  (let ((hosts))
    (dolist (cand (tramp-parse-sconfig config))
      (let ((user (if (car cand) (concat (car cand) "@")))
            (host (cadr cand)))
        (when host
          (push (concat "/" consult-tramp-method
                        ":" user host
                        ":" consult-tramp-path)
                hosts))))
    hosts))

(defun consult-tramp--known-hosts ()
  "Get a list of hosts from `consult-tramp-known-hosts'."
  (consult-tramp--parse-config consult-tramp-known-hosts))

(defun consult-tramp--ssh-hosts ()
  "Get a list of hosts from `consult-tramp-ssh-config'."
  (consult-tramp--parse-config consult-tramp-ssh-config))

(defun consult-tramp--docker-hosts ()
  "Get a list of hosts from docker."
  (when (require 'docker-tramp nil t)
    (let ((hosts)
          (docker-tramp-use-names t))
      (dolist (cand (docker-tramp--parse-running-containers))
        (let ((user (if (not (string-empty-p (car cand)))
                        (concat (car cand) "@")))
              (host (car (cdr cand))))
          (push (concat "/docker:" user host ":" consult-tramp-docker-path) hosts)))
      hosts)))

(defvar consult-tramp--source-local
  `(:name     "Local"
    :narrow   ?l
    :category tramp
    :face     consult-tramp-host
    :history  consult-tramp-history
    :items    ,consult-tramp-local-hosts)
  "Local host candidate source for `consult-tramp'.")

(defvar consult-tramp--source-known-host
  `(:name     "Known Hosts"
    :narrow   ?k
    :category tramp
    :face     consult-tramp-host
    :history  consult-tramp-history
    :items    ,#'consult-tramp--known-hosts)
  "Known Host candiadate source for `consult-tramp'.")

(defvar consult-tramp--source-ssh
  `(:name     "SSH Config"
    :narrow   ?s
    :category tramp
    :face     consult-tramp-host
    :history  consult-tramp-history
    :items    ,#'consult-tramp--ssh-hosts)
  "SSH Config candiadate source for `consult-tramp'.")

(defvar consult-tramp--source-docker
  `(:name     "Docker"
    :narrow   ?d
    :category tramp
    :face     consult-tramp-host
    :history  consult-tramp-history
    :items    ,#'consult-tramp--docker-hosts)
  "Docker candiadate source for `consult-tramp'.")

(defcustom consult-tramp-sources
  '(consult-tramp--source-local
    consult-tramp--source-known-host
    consult-tramp--source-ssh
    consult-tramp--source-docker)
  "Sources used by `consult-tramp'."
  :type '(repeat symbol))

;;;###autoload
(defun consult-tramp (&optional arg)
  "Choose a host and act on it.

If ARG is specified, call the `consult-tramp-default-action' interactively."
  (interactive)
  (let* ((host (car (consult--multi consult-tramp-sources
                                    :prompt (format "Select host to connect to via %s: " consult-tramp-method)
                                    :sort nil)))
         (default-directory host))
    (if arg
        (call-interactively consult-tramp-default-action))
    (funcall consult-tramp-default-action host)))

(provide 'consult-tramp)
;;; consult-tramp.el ends here
