;;; exwm-evil.el --- Evil states for EXWM applications -*- lexical-binding: t; -*-

;; Author: LemonBreezes
;; URL: https://github.com/LemonBreezes/exwm-evil
;; Version: 1.0
;; Package-Requires: ((emacs "25.1") (exwm "0.16") (evil "1.0.0"))
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
;; It allows you to use Evil keybindings with X applications.

;;; Code:
(require 'evil)
(require 'exwm-evil-core)

(defvar exwm-evil-mode-map (make-sparse-keymap)
  "Keymap for `exwm-evil-mode'.")

(defvar exwm-evil-input-delay 0.01
  "Delay between sending keys to the application.")
(defvar exwm-evil-visual-state-enabled nil
  "This variable determines whether we are currently entering keys with shift held.
When non-nil, keys will be sent with shift modifier.")

(defcustom exwm-evil-initial-state-alist nil
  "A mapping of EXWM class names to Evil initial state.
Only `normal' and `insert' are currently supported."
  :type '(alist :key-type string :value-type symbol)
  :group 'exwm-evil)

(defcustom exwm-evil-default-initial-state 'normal
  "The default initial state for EXWM buffers."
  :type 'symbol
  :group 'exwm-evil)

(evil-define-motion exwm-evil-core-send-this-key (count)
  "Send this key to the application COUNT times."
  (exwm-evil-send-key count (aref (this-command-keys-vector) 0)))

(defun exwm-evil-normal-state ()
  "Pass every key directly to Emacs."
  (interactive)
  (setq-local exwm-input-line-mode-passthrough t
              exwm-evil-visual-state-enabled nil
              exwm--input-mode 'line-mode)
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
  "Emulate `evil-visual-state' using keybindings."
  (interactive)
  (setq-local exwm-evil-visual-state-enabled
              (not exwm-evil-visual-state-enabled)))

(defun exwm-evil-visual-line ()
  "Emulate the function `evil-visual-line' using keybindings."
  (interactive)
  (when (not exwm-evil-visual-state-enabled)
    (exwm-input--fake-key 'home)
    (exwm-input--fake-key 'S-end))
  (setq-local exwm-evil-visual-state-enabled
              (not exwm-evil-visual-state-enabled)))

(defun exwm-evil--get-key-symbol (key)
  "Convert KEY to a symbol name for use in function names.
For example, converts the character \\[control-a] to the string \"exwm-evil-core-C-a\"."
  (concat "exwm-evil-core-"
          (if (ignore-errors (integerp key))
              (cond ((<= ?\C-a key ?\C-z)
                     (concat "C-" (char-to-string
                                   (+ (- key ?\C-a) ?a))))
                    ((<= ?\M-a key ?\M-z)
                     (concat "M-" (char-to-string (+ (- key ?\M-a) ?a))))
                    ((<= ?\C-\M-a key ?\C-\M-z)
                     (concat "C-M-" (char-to-string (+ (- key ?\C-\M-a) ?a))))
                    (t
                     (char-to-string key)))
            (symbol-name key))))

;; (exwm-evil--get-key-symbol ?\M-z) => "exwm-evil-core-M-z"

(defun exwm-evil-send-key (count key)
  "Send KEY to the application COUNT times.
If `exwm-evil-visual-state-enabled' is non-nil, add shift modifier to KEY."
  (when (and (integerp count) (> count 50))
    (message "Truncating COUNT to 50. Do not use a large COUNT for EXWM Evil commands.")
    (setq count 50))
  (when exwm-evil-visual-state-enabled
    (setq key (exwm-evil--add-shift-modifier key)))
  (cl-dotimes (i (or count 1))
    (run-at-time (* i exwm-evil-input-delay) nil
                 (lambda (key)
                   (exwm-input--fake-key key))
                 key)))

(defun exwm-evil--add-shift-modifier (key)
  "Add shift modifier to KEY.
Works with both symbol keys and character keys."
  (if (symbolp key)
      (intern (concat "S-" (symbol-name key)))
    (aref (kbd (concat "S-" (char-to-string key))) 0)))

;; (setq exwm-evil-visual-state-enabled nil)
;; (aref (kbd (concat "S-" (char-to-string ?\C-t))) 0) => 33554452

(defun exwm-evil-define-motion-command (key)
  "Define an EXWM Evil motion command for KEY.
Creates a motion command that sends KEY to the application."
  (let* ((key-name (exwm-evil--get-key-symbol key))
         (key-symbol (intern key-name))
         (key-doc (format "Send %s key to the application COUNT times." key))
         (key-val (if (ignore-errors (integerp key)) key key)))

    ;; Define the motion command function
    (defalias key-symbol
      (lambda (count)
        (interactive "p")
        (exwm-evil-send-key count key-val)))

    ;; Add evil motion property
    (put key-symbol 'function-documentation key-doc)
    (evil-set-command-property key-symbol :type 'motion)
    (evil-set-command-property key-symbol :repeat 'motion)

    ;; Return the symbol for binding
    key-symbol))

(defun exwm-evil-command (key)
  "Define an EXWM Evil command for KEY.
Creates a motion command that sends KEY to the application.
Returns the symbol of the created command."
  (exwm-evil-define-motion-command key))

;;;###autoload
(define-minor-mode exwm-evil-mode
  "Toggle the EXWM Evil mode.

The EXWM Evil mode should only be enabled in EXWM buffers. When
enabled, Evil's normal state will automatically be entered."
  :keymap exwm-evil-mode-map
  (if exwm-evil-mode
      (exwm-evil--setup)
    (exwm-evil--teardown)))

(defun exwm-evil--setup ()
  "Set up EXWM Evil mode in the current buffer."
  (pcase (or (alist-get exwm-class-name exwm-evil-initial-state-alist
                        nil nil #'string=)
             exwm-evil-default-initial-state)
    ('normal (exwm-evil-normal-state))
    ('insert (exwm-evil-insert)))
  (advice-add #'exwm-input--on-ButtonPress-line-mode
              :filter-return
              #'exwm-evil--on-ButtonPress-line-mode-a))

(defun exwm-evil--teardown ()
  "Clean up EXWM Evil mode in the current buffer."
  (kill-local-variable 'exwm-input-line-mode-passthrough)
  (kill-local-variable 'exwm-input-prefix-keys)
  (kill-local-variable 'exwm-evil-visual-state-enabled)
  (advice-remove #'exwm-input--on-ButtonPress-line-mode
                 #'exwm-evil--on-ButtonPress-line-mode-a))

;;;###autoload
(defun enable-exwm-evil-mode (&rest _)
  "Turns on Evil mode for the current EXWM buffer."
  (interactive)
  (exwm-evil-mode +1))

(define-key exwm-evil-mode-map [remap evil-normal-state] #'exwm-evil-normal-state)
(define-key exwm-evil-mode-map [remap evil-force-normal-state] #'exwm-evil-normal-state)

(evil-define-key 'normal exwm-evil-mode-map (kbd "i") #'exwm-evil-insert)
(evil-define-key 'insert exwm-evil-mode-map (kbd "<escape>") #'exwm-evil-normal-state)
;; Should we send escape to application with ESC ESC?
;; (evil-define-key 'normal exwm-evil-mode-map (kbd "<escape>") (exwm-evil-command escape))

;; Define all the motion commands
(dolist (key '(down up left right
               C-right C-left C-home C-end
               home end C-u C-x delete
               tab S-tab C-a C-p C-o
               S-down S-up C-down M-down
               C-up M-up C-M-down C-M-up
               S-C-down S-M-down S-C-up S-M-up
               S-C-M-down S-C-M-up C-delete
               C-backspace C-v C-c backspace
               C-+ C-- C-= M-f4))
  (exwm-evil-command key))

;; Bind the keys to the motion commands
(evil-define-key 'normal exwm-evil-mode-map (kbd "j") #'exwm-evil-core-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "k") #'exwm-evil-core-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "h") #'exwm-evil-core-left)
(evil-define-key 'normal exwm-evil-mode-map (kbd "l") #'exwm-evil-core-right)
(evil-define-key 'normal exwm-evil-mode-map (kbd "e") #'exwm-evil-core-C-right)
(evil-define-key 'normal exwm-evil-mode-map (kbd "b") #'exwm-evil-core-C-left)
(evil-define-key 'normal exwm-evil-mode-map (kbd "gg") #'exwm-evil-core-C-home)
(evil-define-key 'normal exwm-evil-mode-map (kbd "G") #'exwm-evil-core-C-end)
(evil-define-key 'normal exwm-evil-mode-map (kbd "a") #'exwm-evil-core-append)
(evil-define-key 'normal exwm-evil-mode-map (kbd "A") #'exwm-evil-core-append-line)
(evil-define-key 'normal exwm-evil-mode-map (kbd "c") #'exwm-evil-core-change)
(evil-define-key 'normal exwm-evil-mode-map (kbd "C") #'exwm-evil-core-change-line)
(evil-define-key 'normal exwm-evil-mode-map (kbd "I") #'exwm-evil-core-insert-line)
(evil-define-key 'normal exwm-evil-mode-map (kbd "0") #'exwm-evil-core-home)
(evil-define-key 'normal exwm-evil-mode-map (kbd "$") #'exwm-evil-core-end)
(evil-define-key 'normal exwm-evil-mode-map (kbd "u") #'exwm-evil-core-C-u)
(evil-define-key 'normal exwm-evil-mode-map (kbd "d") #'exwm-evil-core-C-x)
(evil-define-key 'normal exwm-evil-mode-map (kbd "D") #'exwm-evil-core-C-x)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<deletechar>") #'exwm-evil-core-delete)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<tab>") #'exwm-evil-core-tab)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<backtab>") #'exwm-evil-core-S-tab)
(evil-define-key 'normal exwm-evil-mode-map (kbd "v") #'exwm-evil-visual-char)
(evil-define-key 'normal exwm-evil-mode-map (kbd "V") #'exwm-evil-visual-line)
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-a") #'exwm-evil-core-C-a)
;; Now bind all modified versions of these keys
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-p") #'exwm-evil-core-C-p)
(evil-define-key 'normal exwm-evil-mode-map (kbd "o") #'exwm-evil-core-C-o)
(evil-define-key 'normal exwm-evil-mode-map (kbd "J") #'exwm-evil-core-S-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "K") #'exwm-evil-core-S-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-j") #'exwm-evil-core-C-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "M-j") #'exwm-evil-core-M-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-k") #'exwm-evil-core-C-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "M-k") #'exwm-evil-core-M-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-M-j") #'exwm-evil-core-C-M-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "C-M-k") #'exwm-evil-core-C-M-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-j") #'exwm-evil-core-S-C-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-M-j") #'exwm-evil-core-S-M-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-k") #'exwm-evil-core-S-C-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-M-k") #'exwm-evil-core-S-M-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-M-j") #'exwm-evil-core-S-C-M-down)
(evil-define-key 'normal exwm-evil-mode-map (kbd "S-C-M-k") #'exwm-evil-core-S-C-M-up)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<C-deletechar>") #'exwm-evil-core-C-delete)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<C-backspace>") #'exwm-evil-core-C-backspace)

(evil-define-key 'normal exwm-evil-mode-map (kbd "p") #'exwm-evil-core-C-v)
(evil-define-key 'normal exwm-evil-mode-map (kbd "y") #'exwm-evil-core-C-c)
(evil-define-key 'normal exwm-evil-mode-map (kbd "Y") #'exwm-evil-core-copy-all)
(evil-define-key 'normal exwm-evil-mode-map (kbd "x") #'exwm-evil-core-backspace)
(evil-define-key 'normal exwm-evil-mode-map (kbd "+") #'exwm-evil-core-C-+)
(evil-define-key 'normal exwm-evil-mode-map (kbd "-") #'exwm-evil-core-C--)
(evil-define-key 'normal exwm-evil-mode-map (kbd "=") #'exwm-evil-core-C-=)
(evil-define-key 'normal exwm-evil-mode-map (kbd "M-<f4>") #'exwm-evil-core-M-f4)

(evil-define-key 'normal exwm-evil-mode-map (kbd "<next>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<prior>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<return>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<home>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<end>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<left>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<right>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<down>") #'exwm-evil-core-send-this-key)
(evil-define-key 'normal exwm-evil-mode-map (kbd "<up>") #'exwm-evil-core-send-this-key)

(evil-define-key 'motion exwm-evil-mode-map (kbd "<down-mouse-1>") #'ignore)
(evil-define-key 'motion exwm-evil-mode-map (kbd "<down-mouse-2>") #'ignore)
(evil-define-key 'motion exwm-evil-mode-map (kbd "<down-mouse-3>") #'ignore)

(evil-define-key 'insert exwm-evil-mode-map (kbd "C-o") #'exwm-evil-core-send-this-key)

(provide 'exwm-evil)

;;; exwm-evil.el ends here
