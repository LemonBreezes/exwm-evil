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
(require 'exwm-evil-core)

(defvar exwm-evil-mode-map (make-sparse-keymap))
(defvar exwm-evil-disable-mouse-workaround nil)
(defvar exwm-evil-input-delay 0.06)

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

(defun exwm-evil-send-key (count key)
  (when (and (integerp count) (> count 50))
    (message "Truncating COUNT to 50. Do not use a large COUNT for EXWM Evil commands.")
    (setq count 50))
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 `(lambda (&rest _)
                    (exwm-input--fake-key ',key)))))

(defmacro exwm-evil-command (key)
  `(evil-define-motion ,(intern (concat "exwm-evil-core-" (symbol-name key))) (count)
     (exwm-evil-send-key count ',key)))

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

(defun exwm-evil-enable-mouse-workaround ()
  "Enables a workaround which allows for mouse events to be sent to an
application during the normal state."
  (advice-add #'exwm-input--on-ButtonPress-line-mode
              :override
              #'exwm-evil--on-ButtonPress-line-mode))

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

(evil-define-key 'normal exwm-evil-mode-map (kbd "j") #' exwm-evil-core-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "k") #' exwm-evil-core-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "h") #' exwm-evil-core-left)
(evil-define-key 'normal exwm-evil-mode-map (kbd "l") #' exwm-evil-core-right)
(evil-define-key 'normal exwm-evil-mode-map (kbd "gg") #' exwm-evil-core-top)
(evil-define-key 'normal exwm-evil-mode-map (kbd "G") #' exwm-evil-core-bottom)
;; Now bind all modified versions of these keys
(evil-define-key 'normal exwm-evil-mode-map (kbd "J") (exwm-evil-command S-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "K") (exwm-evil-command S-up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-j") (exwm-evil-command C-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "M-j") (exwm-evil-command M-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-k") (exwm-evil-command C-up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "M-k") (exwm-evil-command M-up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-M-j") (exwm-evil-command C-M-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-M-k") (exwm-evil-command C-M-up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-j") (exwm-evil-command S-C-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-M-j") (exwm-evil-command S-M-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-k") (exwm-evil-command S-C-up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-M-k") (exwm-evil-command S-M-up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-M-j") (exwm-evil-command S-C-M-down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-M-k") (exwm-evil-command S-C-M-up))

(evil-define-key 'normal exwm-evil-mode-map (kbd "p") #'exwm-evil-paste)
(evil-define-key 'normal exwm-evil-mode-map (kbd "y") #'exwm-evil-copy)
(evil-define-key 'normal exwm-evil-mode-map (kbd "x") #'exwm-evil-cut)
(evil-define-key 'normal exwm-evil-mode-map (kbd "+") #'exwm-evil-core-zoom-in)
(evil-define-key 'normal exwm-evil-mode-map (kbd "-") #'exwm-evil-core-zoom-out)
(evil-define-key 'normal exwm-evil-mode-map (kbd "=") #'exwm-evil-core-reset-zoom)

(evil-define-key 'normal exwm-evil-mode-map (kbd "<next>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<prior>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<return>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<home>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<end>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<left>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<right>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<down>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<up>") #'exwm-evil-core-send-this-key)

(provide 'exwm-evil)

;;; exwm-evil.el ends here
