;;;; lem-setup-faces.el --- Setup for faces          -*- lexical-binding: t; -*-
;; Copyright (C) 2022
;; SPDX-License-Identifier: MIT
;; Author: Colin McLear
;;; Commentary:
;;
;;; Code:

;;;; Underline
(setq x-underline-at-descent-line t)

;;;; Dim inactive windows
(use-package dimmer
  :straight (:host github :repo "gonewest818/dimmer.el")
  :hook (after-init . dimmer-mode)
  :config
  ;; (setq mini-frame-create-lazy nil)
  (setq dimmer-prevent-dimming-predicates '(window-minibuffer-p))
  (setq dimmer-fraction 0.5)
  (setq dimmer-adjustment-mode :foreground)
  (setq dimmer-use-colorspace :rgb)
  (setq dimmer-watch-frame-focus-events nil)
  (dimmer-configure-which-key)
  (dimmer-configure-hydra)
  (dimmer-configure-magit)
  (dimmer-configure-posframe)
  (dimmer-configure-vertico))

(defun dimmer-configure-vertico ()
  "Convenience settings for vertico-buffer users."
  (with-no-warnings
    (add-to-list
     'dimmer-buffer-exclusion-regexps "^ \\*Vertico\\*$")))
;; (add-to-list
;; 'dimmer-prevent-dimming-predicates #'vertico-buffer-mode)))

;;;; Cursor
(use-package emacs
  :straight (:type built-in)
  :custom
  ;; don't show cursor in inactive windows
  (cursor-in-non-selected-windows nil))

;;;; Reveal Mode
;; Toggle uncloaking of invisible text near point, including folded org headlines (Reveal mode).
(use-package reveal
  :straight (:type built-in)
  :defer 1
  :config
  (setq reveal-auto-hide nil)
  (global-reveal-mode))

;;;; SVG Library (For Tags/Labels/etc.)
  ;;; SVG Tag Mode
(use-package svg-tag-mode
  :straight (:type git :host github :repo "rougier/svg-tag-mode")
  :hook (prog-mode . svg-tag-mode)
  :config
  (setq svg-tag-tags
        '(;; Replaces any occurence of :XXX: with a dynamic SVG tag displaying XXX
          ("\\(:[A-Z]+:\\)" . ((lambda (tag)
                                 (svg-tag-make tag :face 'success :inverse t :beg 1 :end -1))))
          ;; other tags
          ("DONE:"  . ((lambda (tag) (svg-tag-make "DONE:"  :face 'shadow  :inverse t ))))
          ("FIXME:" . ((lambda (tag) (svg-tag-make "FIXME:" :face 'error :inverse t))))
          ("HACK:"  . ((lambda (tag) (svg-tag-make "HACK:"  :face 'bespoke-red :inverse t))))
          ("NOTE:"  . ((lambda (tag) (svg-tag-make "NOTE:"  :face 'bespoke-yellow :inverse t))))
          ("TODO:"  . ((lambda (tag) (svg-tag-make "TODO:"  :face 'warning :inverse t)))))))


;;;; Highlight
;;;;; Highlight Lines
;; Highlight lines. You can toggle this off
(use-package hl-line+
  :straight t
  :custom-face
  ;; subtle highlighting
  (hl-line ((t (:inherit highlight))))
  :custom
  (global-hl-line-mode nil)
  (hl-line-flash-show-period 1.0)
  (hl-line-inhibit-highlighting-for-modes '(dired-mode))
  (hl-line-when-idle-interval 2)
  :config
  (toggle-hl-line-when-idle 1 t)
  ;; don't highlight when idle on splash page
  (add-hook 'lem-splash-mode-hook (lambda () (when lem-splash-mode (toggle-hl-line-when-idle))))

  )

;;;;; Highlight Numbers & TODOS
(use-package highlight-numbers
  :defer t
  :commands highlight-numbers-mode
  :init
  (add-hook 'prog-mode-hook #'highlight-numbers-mode))

(use-package hl-todo
  :defer t
  :commands hl-todo-mode
  :init
  ;; (add-hook 'org-mode-hook #'hl-todo-mode)
  (add-hook 'prog-mode-hook #'hl-todo-mode)
  (add-hook 'markdown-mode-hook #'hl-todo-mode))

;; hydra for TODOs
(with-eval-after-load 'hydra
  (defhydra lem/hydra-todo
    (:pre
     (hl-todo-mode 1)
     :post
     (hl-todo-mode -1))
    "Todo"
    ("n" hl-todo-next "Next")
    ("p" hl-todo-previous "Previous")
    ("o" hl-todo-occur "Occur")
    ("q" nil "Quit" :color blue :exit t)))

;; ;;https://github.com/erickgnavar/dotfiles/tree/master/.emacs.d#highlight-todo-fixme-etc
;; (defun lem/highlight-todo-like-words ()
;;   (font-lock-add-keywords
;;    nil `(("\\<\\(FIXME\\|TODO\\|NOTE\\)"
;;           1 font-lock-warning-face t))))


;; (add-hook 'prog-mode-hook 'my/highlight-todo-like-words)


;;;;; Highlight Cursor Line with Pulse
;; From https://karthinks.com/software/batteries-included-with-emacs/
;; Replace external package with internal command

(use-package pulse
  :straight (:type built-in)
  :defer 1
  :commands (pulse-line pulse-momentary-highlight-one-line)
  :config
  (defun pulse-line (&rest _)
    "Pulse the current line."
    (interactive)
    (pulse-momentary-highlight-one-line (point)))
  ;; pulse for commands
  (dolist (command '(scroll-up-command scroll-down-command
                                       recenter-top-bottom other-window))
    (advice-add command :after #'pulse-line))
  ;; pulse on window change
  (push 'pulse-line window-selection-change-functions))

;; Pulse changes in buffer region
(use-package goggles
  :straight (:type git :host github :repo "minad/goggles")
  :defer 1
  :config
  (goggles-mode 1))

;;;;; Crosshair Highlighting
(use-package crosshairs
  :straight t
  :commands (crosshairs-highlight
             crosshairs-mode
             flash-crosshairs)
  :bind (:map lem+toggle-keys
         ("c" . crosshairs-mode))
  :custom-face
  (col-highlight ((t (:inherit hl-line))))
  :config
  ;; same colors for both hlines
  (setq col-highlight-vline-face-flag t))

;;;; Empty Lines
;; Don't show empty lines.
;; .. Allows you to tell if there are blank lines at the end of the file.
(setq-default indicate-empty-lines nil)


(provide 'lem-setup-faces)
;;; lem-setup-faces.el ends here
