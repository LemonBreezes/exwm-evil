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

(defvar exwm-evil-mode-map (make-sparse-keymap)
  "Keymap for `exwm-evil-mode'.")
(defvar exwm-evil-input-delay 0.06
  "The delay between bundled keypresses. If you set it too low, not every key
press will register.")
(defvar exwm-evil-visual-state-enabled nil
  "This variable determines whether we are currently entering keys with shift held.")

(evil-define-motion exwm-evil-core-send-this-key (count)
  "Send this key to the application COUNT times."
  (exwm-evil-send-key count (aref (this-command-keys-vector) 0)))

(defun exwm-evil-normal-state ()
  "Pass every key directly to Emacs."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t
              exwm-evil-visual-state-enabled nil)
  (kill-local-variable 'exwm-input-prefix-keys)
  (evil-normal-state))

(defun exwm-evil-insert ()
  "Pass every key directly to the application."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough nil
              exwm-input-prefix-keys '(escape)
              exwm-evil-visual-state-enabled nil)
  (evil-insert-state))

(defun exwm-evil-visual-char ()
  (interactive)
  (setq-local exwm-evil-visual-state-enabled
              (not exwm-evil-visual-state-enabled)))

(defun exwm-evil-visual-line ()
  (interactive)
  (when (not exwm-evil-visual-state-enabled)
      (exwm-input--fake-key 'home)
    (exwm-input--fake-key 'S-end))
  (setq-local exwm-evil-visual-state-enabled
              (not exwm-evil-visual-state-enabled)))

(defun exwm-evil-send-key (count key)
  "Sends KEY to the application COUNT times."
  (when (and (integerp count) (> count 50))
    (message "Truncating COUNT to 50. Do not use a large COUNT for EXWM Evil commands.")
    (setq count 50))
  (when exwm-evil-visual-state-enabled
    (setq key (if (symbolp key)
                  (intern (concat "S-" (symbol-name key)))
                (aref (kbd (concat "S-" (char-to-string key))) 0))))
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (key)
                   (exwm-input--fake-key key))
                 key)))
;; (setq exwm-evil-visual-state-enabled nil)
;; (aref (kbd (concat "S-" (char-to-string ?\C-t))) 0)

(defmacro exwm-evil-command (key)
  "Defines an EXWM Evil command for KEY."
  `(evil-define-motion ,(intern (concat "exwm-evil-core-"
                                        (if (ignore-errors (integerp key))
                                            (char-to-string key)
                                          (symbol-name key))))
     (count)
     (exwm-evil-send-key
      count
      (if (ignore-errors (integerp ,key)) ,key ',key))))

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
    (exwm-evil-normal-state)))

(define-key exwm-evil-mode-map [remap evil-normal-state] #'exwm-evil-normal-state)
(define-key exwm-evil-mode-map [remap evil-force-normal-state] #'exwm-evil-normal-state)

(evil-define-key 'normal exwm-evil-mode-map (kbd "i") #'exwm-evil-insert)
(evil-define-key 'insert exwm-evil-mode-map (kbd "<escape>") #'exwm-evil-normal-state)

(evil-define-key 'normal exwm-evil-mode-map (kbd "j") (exwm-evil-command down))
(evil-define-key 'normal exwm-evil-mode-map (kbd "k") (exwm-evil-command up))
(evil-define-key 'normal exwm-evil-mode-map (kbd "h") (exwm-evil-command left))
(evil-define-key 'normal exwm-evil-mode-map (kbd "l") (exwm-evil-command right))
(evil-define-key 'normal exwm-evil-mode-map (kbd "e") (exwm-evil-command C-right))
(evil-define-key 'normal exwm-evil-mode-map (kbd "b") (exwm-evil-command C-left))
(evil-define-key 'normal exwm-evil-mode-map (kbd "gg") (exwm-evil-command C-home))
(evil-define-key 'normal exwm-evil-mode-map (kbd "G") (exwm-evil-command C-end))
(evil-define-key 'normal exwm-evil-mode-map (kbd "a") #'exwm-evil-core-append)
(evil-define-key 'normal exwm-evil-mode-map (kbd "A") #'exwm-evil-core-append-line)
(evil-define-key 'normal exwm-evil-mode-map (kbd "c") #'exwm-evil-core-change)
(evil-define-key 'normal exwm-evil-mode-map (kbd "0") (exwm-evil-command home))
(evil-define-key 'normal exwm-evil-mode-map (kbd "$") (exwm-evil-command end))
(evil-define-key 'normal exwm-evil-mode-map (kbd "u") (exwm-evil-command ?\C-u))
(evil-define-key 'normal exwm-evil-mode-map (kbd "d") (exwm-evil-command ?\C-x))
(evil-define-key 'normal exwm-evil-mode-map (kbd "D") (exwm-evil-command ?\C-x))
(evil-define-key 'normal exwm-evil-mode-map (kbd "<deletechar>") (exwm-evil-command delete))
(evil-define-key 'normal exwm-evil-mode-map (kbd "<backspace>") (exwm-evil-command backspace))
(evil-define-key 'normal exwm-evil-mode-map (kbd "<tab>") (exwm-evil-command tab))
(evil-define-key 'normal exwm-evil-mode-map (kbd "<backtab>") (exwm-evil-command backtab))
(evil-define-key 'normal exwm-evil-mode-map (kbd "v") #'exwm-evil-visual-char)
(evil-define-key 'normal exwm-evil-mode-map (kbd "V") #'exwm-evil-visual-line)
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
(evil-define-key 'normal exwm-evil-mode-map (kbd "<C-deletechar>") (exwm-evil-command C-delete))
(evil-define-key 'normal exwm-evil-mode-map (kbd "<C-backspace>") (exwm-evil-command C-backspace))

(evil-define-key 'normal exwm-evil-mode-map (kbd "p") (exwm-evil-command C-p))
(evil-define-key 'normal exwm-evil-mode-map (kbd "y") (exwm-evil-command C-c))
(evil-define-key 'normal exwm-evil-mode-map (kbd "x") (exwm-evil-command backspace))
(evil-define-key 'normal exwm-evil-mode-map (kbd "+") (exwm-evil-command C-+))
(evil-define-key 'normal exwm-evil-mode-map (kbd "-") (exwm-evil-command C--))
(evil-define-key 'normal exwm-evil-mode-map (kbd "=") (exwm-evil-command C-=))
(evil-define-key 'normal exwm-evil-mode-map (kbd "M-<f4>") (exwm-evil-command M-f4))

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
