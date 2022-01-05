;;; packages/exwm-evil/exwm-evil-core.el -*- lexical-binding: t; -*-

(require 'evil)
(require 'exwm)
(require 'exwm-input)

(evil-define-motion exwm-evil-core-up (count)
  "Move up COUNT times."
  (cl-dotimes (_ (or count 1))
    (exwm-input--fake-key 'up)))

(evil-define-motion exwm-evil-core-down (count)
  "Move up COUNT times."
  (cl-dotimes (_ (or count 1))
    (exwm-input--fake-key 'down)))

(evil-define-motion exwm-evil-core-left (count)
  "Move left COUNT times."
  (cl-dotimes (_ (or count 1))
    (exwm-input--fake-key 'left)))

(evil-define-motion exwm-evil-core-right (count)
  "Move right COUNT times."
  (cl-dotimes (_ (or count 1))
    (exwm-input--fake-key 'right)))

(evil-define-motion exwm-evil-core-top ()
  "Move to the top."
  (exwm-input--fake-key 'home))

(evil-define-motion exwm-evil-core-bottom ()
  "Move to the bottom."
  (exwm-input--fake-key 'end))

(evil-define-motion exwm-evil-core-send-this-key (count)
  "Send this key to the application COUNT times."
  (cl-dotimes (_ (or count 1))
    (cl-loop for key in (listify-key-sequence (this-command-keys))
             do (exwm-input--fake-key key))))
