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

(evil-define-motion exwm-evil-core-up (count)
  "Move up COUNT times."
  (exwm-evil-send-key count 'up))

(evil-define-motion exwm-evil-core-down (count)
  "Move up COUNT times."
  (exwm-evil-send-key count 'down))

(evil-define-motion exwm-evil-core-left (count)
  "Move left COUNT times."
  (exwm-evil-send-key count 'left))

(evil-define-motion exwm-evil-core-right (count)
  "Move right COUNT times."
  (exwm-evil-send-key count 'right))

(evil-define-motion exwm-evil-core-top ()
  "Move to the top."
  (exwm-input--fake-key 'home))

(evil-define-motion exwm-evil-core-bottom ()
  "Move to the bottom."
  (exwm-input--fake-key 'end))

(evil-define-motion exwm-evil-core-zoom-in (count)
  "Zoom in COUNT times."
  (exwm-evil-send-key count ?\C-=))

(evil-define-motion exwm-evil-core-zoom-out (count)
  "Zoom out COUNT times."
  (exwm-evil-send-key count ?\C--))

(evil-define-motion exwm-evil-core-reset-zoom ()
  "Reset the level of zoom in the current application."
  (exwm-input--fake-key ?\C-0))

(evil-define-motion exwm-evil-core-send-this-key (count)
  "Send this key to the application COUNT times."
  (exwm-evil-send-key count (aref (this-command-keys-vector) 0)))

(evil-define-motion exwm-evil-core-paste ()
  "Pastes text from the clipboard."
  (exwm-input--fake-key ?\C-v))

(evil-define-motion exwm-evil-core-copy ()
  "Pastes text from the clipboard."
  (exwm-input--fake-key ?\C-c))

(evil-define-motion exwm-evil-core-cut ()
  "Pastes text from the clipboard."
  (exwm-input--fake-key ?\C-x))

(evil-define-motion exwm-evil-core-forward-word (count)
  "Moves forward COUNT words."
  (exwm-evil-send-key count 'C-right))

(evil-define-motion exwm-evil-core-backward-word (count)
  "Moves backward COUNT words."
  (exwm-evil-send-key count 'C-left))

(evil-define-motion exwm-evil-core-quit ()
  "Close the current application."
  (exwm-input--fake-key 'M-f4))

(defun exwm-evil-core-normal ()
  "Pass every key directly to Emacs."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t)
  (kill-local-variable 'exwm-input-prefix-keys)
  (evil-normal-state))

(defun exwm-evil-core-insert ()
  "Pass every key directly to the application."
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

(evil-define-motion exwm-evil-core-beginning-of-line ()
  "Move backwards to the beginning of the current line."
  (exwm-input--fake-key 'home))

(evil-define-motion exwm-evil-core-end-of-line ()
  "Move forwards to the end of the current line."
  (exwm-input--fake-key 'end))

(evil-define-motion exwm-evil-core-undo (count)
  "Undo COUNT times."
  (exwm-evil-send-key count ?\C-z))

(defun exwm-evil-core-cut ()
  "Cut text."
  (interactive)
  (exwm-input--fake-key ?\C-x))

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

(provide 'exwm-evil-core)

(defun exwm-evil-core-do-mouse-click-x-y (x y &optional button-num window-id)
  "Perform a mouse click at (window relative) position X and Y

By default BUTTON-NUM is ``1'' (i.e. main click) and the WINDOW-ID is the currently selected window."
  (let* ((button-index (intern (format "xcb:ButtonIndex:%d" (or button-num 1))))
         (button-mask (intern (format "xcb:ButtonMask:%d" (or button-num 1))))
         (window-id (or window-id (exwm--buffer->id
                                   (window-buffer (selected-window)))
                        (user-error "No window selected")))
         (button-actions `((xcb:ButtonPress . ,button-mask)
                           (xcb:ButtonRelease . 0))))
    (dolist (b-action button-actions)
      (xcb:+request exwm--connection
          (make-instance 'xcb:SendEvent
                         :propagate 0
                         :destination window-id
                         :event-mask xcb:EventMask:NoEvent
                         :event (xcb:marshal
                                 (make-instance (car b-action)
                                                :detail button-index
                                                :time xcb:Time:CurrentTime
                                                :root exwm--root
                                                :event window-id
                                                :child 0
                                                :root-x 0
                                                :root-y 0
                                                :event-x x
                                                :event-y y
                                                :state (cdr b-action)
                                                :same-screen 0)
                                 exwm--connection))))
    (xcb:flush exwm--connection)))

(defun exwm-evil-core-do-mouse-click (button-num)
  "Perform a left mouse click at the current cursor position."
  (interactive)
  (cl-destructuring-bind (mouse-x . mouse-y)
      (mouse-absolute-pixel-position)
    (if (provided-mode-derived-p
         (buffer-local-value 'major-mode
                             (window-buffer (window-at mouse-x mouse-y)))
         'exwm-mode)
        (progn
          (exwm-evil-insert)
          (exwm-evil-core-do-mouse-click-x-y mouse-x mouse-y button-num))
      (call-interactively #'evil-mouse-drag-region))))
