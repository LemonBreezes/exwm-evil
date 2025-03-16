;;; packages/exwm-evil/exwm-evil-core.el -*- lexical-binding: t; -*-

;; Author: LemonBreezes
;; URL: https://github.com/LemonBreezes/exwm-evil

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

(require 'evil)
(require 'exwm-input)

(defvar exwm-evil-input-delay 0.06
  "The delay between bundled keypresses. If you set it too low, not every key
press will register.")

(defmacro exwm-evil-core-define-movement (name key &optional doc)
  "Define a movement command NAME that sends KEY.
Optional DOC is the docstring for the command."
  `(evil-define-motion ,(intern (format "exwm-evil-core-%s" name)) (count)
     ,(or doc (format "Move %s COUNT times." name))
     (exwm-evil-send-key count ',key)))

(exwm-evil-core-define-movement "up" up "Move up COUNT times.")
(exwm-evil-core-define-movement "down" down "Move down COUNT times.")
(exwm-evil-core-define-movement "left" left "Move left COUNT times.")
(exwm-evil-core-define-movement "right" right "Move right COUNT times.")

(defmacro exwm-evil-core-define-simple-motion (name key &optional doc)
  "Define a simple motion command NAME that sends KEY.
Optional DOC is the docstring for the command."
  `(evil-define-motion ,(intern (format "exwm-evil-core-%s" name)) ()
     ,(or doc (format "Move to the %s." name))
     (exwm-input--fake-key ',key)))

(exwm-evil-core-define-simple-motion "top" home "Move to the top.")
(exwm-evil-core-define-simple-motion "bottom" end "Move to the bottom.")

;; Zoom commands
(exwm-evil-core-define-movement "zoom-in" ?\C-= "Zoom in COUNT times.")
(exwm-evil-core-define-movement "zoom-out" ?\C-- "Zoom out COUNT times.")
(exwm-evil-core-define-simple-motion "reset-zoom" ?\C-0
  "Reset the level of zoom in the current application.")

(evil-define-motion exwm-evil-core-send-this-key (count)
  "Send this key to the application COUNT times."
  (exwm-evil-send-key count (aref (this-command-keys-vector) 0)))

;; Clipboard commands
(exwm-evil-core-define-simple-motion "paste" ?\C-v "Paste text from the clipboard.")
(exwm-evil-core-define-simple-motion "copy" ?\C-c "Copy selected text to the clipboard.")
(exwm-evil-core-define-simple-motion "cut" ?\C-x "Cut selected text to the clipboard.")

(exwm-evil-core-define-movement "forward-word" C-right "Move forward COUNT words.")
(exwm-evil-core-define-movement "backward-word" C-left "Move backward COUNT words.")

(evil-define-motion exwm-evil-core-quit ()
  "Close the current application."
  (exwm-input--fake-key 'M-f4))

(defun exwm-evil-core-normal ()
  "Enter normal state and pass keys to Emacs.
This sets up the appropriate EXWM input variables."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t)
  (kill-local-variable 'exwm-input-prefix-keys)
  (evil-normal-state))

(defun exwm-evil-core-insert ()
  "Enter insert state and pass keys to the application.
This sets up the appropriate EXWM input variables."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough nil
              exwm-input-prefix-keys '(escape))
  (evil-insert-state))

(evil-define-motion exwm-evil-core-append ()
  "Move forward once and enter insert state."
  (exwm-input--fake-key 'right)
  (exwm-evil-core-insert))

(evil-define-motion exwm-evil-core-append-line ()
  "Move forward to the end of the line and enter insert state."
  (exwm-input--fake-key 'end)
  (exwm-evil-core-insert))

(exwm-evil-core-define-simple-motion "beginning-of-line" home
  "Move backwards to the beginning of the current line.")
(exwm-evil-core-define-simple-motion "end-of-line" end
  "Move forwards to the end of the current line.")

(evil-define-motion exwm-evil-core-undo (count)
  "Undo COUNT times."
  (exwm-evil-send-key count ?\C-z))

;; This function is already defined above with the clipboard commands

(evil-define-motion exwm-evil-core-change ()
  "Delete the selection and switch to insert state."
  (exwm-input--fake-key 'delete)
  (exwm-evil-core-insert))

(evil-define-motion exwm-evil-core-change-line ()
  "Delete the current line and switch to insert state."
  (exwm-input--fake-key 'home)
  (exwm-input--fake-key 'S-end)
  (exwm-input--fake-key 'delete)
  (exwm-evil-core-insert))

(defun exwm-evil-core-insert-line ()
  "Move to the beginning of the current line and switch to insert
state."
  (interactive)
  (exwm-input--fake-key 'home)
  (exwm-evil-core-insert))

(defun exwm-evil-core-copy-all ()
  "Copy everything."
  (interactive)
  (exwm-input--fake-key ?\C-a)
  (run-at-time exwm-evil-input-delay nil #'exwm-input--fake-key ?\C-c))

(defun exwm-evil--on-ButtonPress-line-mode-a (_)
  "Handle button events in line mode.
BUFFER is the `exwm-mode' buffer the event was generated
on. BUTTON-EVENT is the X event converted into an Emacs event.

The return value is used as event_mode to release the original
button event."
  (bound-and-true-p xcb:Allow:ReplayPointer))

(provide 'exwm-evil-core)
