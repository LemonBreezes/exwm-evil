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

;;; Code:
(require 'evil)
(require 'evil-core)
(require 'exwm)
(require 'exwm-input)

(defvar exwm-evil-mode-map (make-sparse-keymap))
(defvar exwm-evil-disable-mouse-workaround nil)

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

;; HACK See https://github.com/walseb/exwm-firefox-evil/issues/1#issuecomment-672390501
(defun exwm-evil--on-ButtonPress-line-mode (buffer button-event)
  "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on. BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
  (with-current-buffer buffer
    (let ((read-event (exwm-input--mimic-read-event button-event)))
      (exwm--log "%s" read-event)
      (if (and read-event
               (exwm-input--event-passthrough-p read-event))
          ;; The event should be forwarded to emacs
          (progn
            (exwm-input--cache-event read-event)
            (exwm-input--unread-event button-event)

            xcb:Allow:ReplayPointer)
        ;; The event should be replayed
        xcb:Allow:ReplayPointer))))

;;;###autoload
(define-minor-mode exwm-evil-mode
  "Toggle the EXWM Evil mode.

The EXWM Evil mode should only be enabled in EXWM buffers. When
enabled, Evil's normal state will automatically be entered."
  :keymap exwm-evil-mode-map
  (if exwm-evil-mode
      (progn (exwm-evil-normal)
             (unless exwm-evil-disable-mouse-workaround
               (advice-add #'exwm-input--on-ButtonPress-line-mode
                           :override
                           #'exwm-evil--on-ButtonPress-line-mode)))
    (advice-remove #'exwm-input--on-ButtonPress-line-mode
                   #'exwm-evil--on-ButtonPress-line-mode)))

(define-key exwm-evil-mode-map [remap evil-normal-state] 'exwm-evil-normal)
(define-key exwm-evil-mode-map [remap evil-force-normal-state] 'exwm-evil-normal)

(evil-define-key 'normal exwm-evil-mode-map (kbd "i") #'exwm-evil-insert)
(evil-define-key 'insert exwm-evil-mode-map (kbd "<escape>") #'exwm-evil-normal)

(provide 'exwm-evil)

;;; exwm-evil.el ends here
