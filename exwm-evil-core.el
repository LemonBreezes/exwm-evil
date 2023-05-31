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

(evil-define-motion exwm-evil-core-append ()
  "Move forward once and enter insert state."
  (exwm-input--fake-key 'right)
  (exwm-evil-core-insert))

(evil-define-motion exwm-evil-core-append-line ()
  "Move forward to the end of the line and enter insert state."
  (exwm-input--fake-key 'end)
  (exwm-evil-core-insert))

(evil-define-motion exwm-evil-core-change ()
  "Delete the selection and switch to insert state."
  (exwm-input--fake-key 'delete)
  (exwm-evil-core-insert))

(provide 'exwm-evil-core)
