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
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (&rest _)
                   (exwm-input--fake-key 'up)))))

(evil-define-motion exwm-evil-core-down (count)
  "Move up COUNT times."
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (&rest _)
                   (exwm-input--fake-key 'down)))))

(evil-define-motion exwm-evil-core-left (count)
  "Move left COUNT times."
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (&rest _)
                   (exwm-input--fake-key 'left)))))

(evil-define-motion exwm-evil-core-right (count)
  "Move right COUNT times."
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (&rest _)
                   (exwm-input--fake-key 'right)))))

(evil-define-motion exwm-evil-core-top ()
  "Move to the top."
  (exwm-input--fake-key 'home))

(evil-define-motion exwm-evil-core-bottom ()
  "Move to the bottom."
  (exwm-input--fake-key 'end))

(evil-define-motion exwm-evil-core-zoom-in (count)
  "Zoom in COUNT times."
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (&rest _)
                   (exwm-input--fake-key ?\C-=)))))

(evil-define-motion exwm-evil-core-zoom-out (count)
  "Zoom out COUNT times."
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (&rest _)
                   (exwm-input--fake-key ?\C--)))))

(evil-define-motion exwm-evil-core-reset-zoom ()
  "Reset the level of zoom in the current application."
  (exwm-input--fake-key ?\C-0))

(evil-define-motion exwm-evil-core-send-this-key (count)
  "Send this key to the application COUNT times."
  (cl-dotimes (i (or count 1))
    (run-at-time
     (* i exwm-evil-input-delay) nil
     `(lambda (&rest _)
       (cl-loop for key in (listify-key-sequence ,(this-command-keys))
                do (exwm-input--fake-key key))))))

(provide 'exwm-evil-core)
