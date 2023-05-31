;;; packages/exwm-evil/exwm-evil-core.el -*- lexical-binding: t; -*-

;; Author: ***REMOVED*** <***REMOVED***>
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
(require 'exwm)
(require 'exwm-input)

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


(provide 'exwm-evil-core)
