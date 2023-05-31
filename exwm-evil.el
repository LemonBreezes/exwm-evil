;;; packages/exwm-evil/exwm-evil.el -*- lexical-binding: t; -*-

;; Author: ***REMOVED*** <***REMOVED***>
;; URL: https://github.com/LemonBreezes/exwm-evil
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (exwm "0.16") (evil "1.0.0"))
;; Keywords: extensions

;; exwm-evil.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; exwm-evil.el is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements Evil states for EXWM applications.

(require 'evil)
(require 'evil-core)
(require 'exwm)
(require 'exwm-input)

;;; Code:
(defun exwm-evil-normal ()
  "Pass every key directly to Emacs."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t)
  (evil-normal-state))

(defun exwm-evil-insert ()
  "Pass every key directly to the application."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough nil)
  (evil-insert-state))

(defvar exwm-evil-mode-map (make-sparse-keymap))

;;;###autoload
(define-minor-mode exwm-evil-mode
  "Toggle the EXWM Evil mode.

The EXWM Evil mode should only be enabled in EXWM buffers. When
enabled, Evil's normal state will automatically be entered."
  :keymap exwm-evil-mode-map
  (when exwm-evil-mode
    (exwm-evil-normal)))

(define-key exwm-evil-mode-map [remap evil-normal-state] 'exwm-evil-normal)
(define-key exwm-evil-mode-map [remap evil-force-normal-state] 'exwm-evil-normal)

(evil-define-key 'normal exwm-evil-mode-map (kbd "i") #'exwm-evil-insert)
(evil-define-key 'insert exwm-evil-mode-map (kbd "<escape>") #'exwm-evil-normal)

(provide 'exwm-evil)

;;; exwm-evil.el ends here