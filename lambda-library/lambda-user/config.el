;;; config.el --- summary -*- lexical-binding: t -*-

;; Maintainer: Colin Mclear
;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Personal config file
;; This file contains all user-specific settings

;;; Code:
;;;; Personal Information

;; Give Emacs some personal info
(setq user-full-name ""
      user-mail-address "")

;;;;; Private File
;; where to store private or "secret" info
(let ((private (expand-file-name "private.el" lem-user-dir)))
  (if (file-exists-p private)
	  (load-file private)))

;;;; User Vars
;; Denote settings
(customize-set-variable 'lem-notes-dir (concat (getenv "HOME") "/Documents/03-resources/notes/"))
(customize-set-variable 'denote-directory lem-notes-dir)
(customize-set-variable 'denote-known-keywords '("emacs" "teaching" "cbs" "neurds" "workbook"))
(customize-set-variable 'denote-prompts '(title keywords subdirectory))

(setq consult-notes-sources
      `(("Zettel"          ?z ,(concat lem-notes-dir "zettel/"))
        ("Lecture Notes"   ?l ,(concat lem-notes-dir "lecture-notes/"))
        ("Reference Notes" ?r ,(concat lem-notes-dir "ref-notes/"))
        ("Org"             ?o "~/Documents/03-resources/org/")
        ("Workbook"        ?w ,(concat lem-notes-dir "workbook/"))
        ("Refile"          ?R ,(concat lem-notes-dir "refile-notes/"))))


;;;;; Set Fonts
;; Set fonts
(custom-set-variables
 '(lem-ui-default-font
   '(:font "SF Mono 13")))

(custom-set-variables
 '(lem-ui-variable-width-font
   '(:font "SF Pro" :weight normal)))



;;;;; Org Directories
;; Set these if you're using org
(setq org-directory "~/Documents/03-resources/org/"
      org-default-notes-file (concat org-directory "inbox.org")
      org-agenda-files (list org-directory))

;;;;; Straight Package Manager
;; Don't walk straight repos
(push "straight" vc-directory-exclusion-list)
;; Delete .DS_Store before prune
(advice-add 'straight-prune-build :before #'(lambda () (move-file-to-trash (concat lem-var-dir "straight/build/.DS_Store"))))

;;;;; Set Splash Footer
;; Set a footer for the splash screen
(setq lem-splash-footer  "")

;;;; Load Modules
;; Load modules in stages for a shorter init time. We load core modules first,
;; then more expensive modules after init, with the rest loaded after startup
;; has fully completed.

;;;;; Load base modules
(message "
;; ======================================================
;; *Loading 𝛌-Emacs Base Modules*
;; ======================================================")
(measure-time
 (cl-dolist (mod (list
                  ;; Base modules
                  'lem-setup-libraries
                  'lem-setup-settings
                  'lem-setup-functions
                  'lem-setup-macros
                  'lem-setup-scratch
                  'lem-setup-theme

                  ;; Basic UI modules
                  'lem-setup-windows
                  'lem-setup-buffers
                  'lem-setup-frames
                  'lem-setup-fonts
                  'lem-setup-faces))
   (require mod nil t)))

;;;;; Load After-Init Modules
(defun lem-user-config-after-init ()
  "Modules loaded after init."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-init Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list
                                 ;; Splash/Dashboard
                                 'lem-setup-splash
                                 ;;'lem-setup-dashboard

                                 ;; Completion & Keybinds
                                 'lem-setup-completion
                                 'lem-setup-keybindings

                                 ;; Navigation & Search modules
                                 'lem-setup-navigation
                                 'lem-setup-dired
                                 'lem-setup-search

                                 ;; Project & Tab/Workspace modules
                                 'lem-setup-vc
                                 'lem-setup-projects
                                 'lem-setup-tabs
                                 'lem-setup-workspaces))
                  (require mod nil t))))
(add-hook 'after-init-hook #'lem-user-config-after-init)

;;;;; Load After-Startup Modules
(defun lem-user-config-after-startup ()
  "Modules loaded after Emacs startup."
  (message "
  ;; ======================================================
  ;; *Loading 𝛌-Emacs after-startup Modules*
  ;; ======================================================")
  (measure-time (cl-dolist (mod (list

                                 ;; Writing modules
                                 'lem-setup-writing
                                 'lem-setup-notes
                                 'lem-setup-citation

                                 ;; Programming modules
                                 'lem-setup-programming
                                 'lem-setup-debug
                                 'lem-setup-skeleton

                                 ;; Shell & Terminal
                                 'lem-setup-shell
                                 'lem-setup-eshell

                                 ;; Org modules
                                 'lem-setup-org-base
                                 'lem-setup-org-settings
                                 'lem-setup-org-extensions

                                 ;; Productivity
                                 'lem-setup-pdf
                                 'lem-setup-elfeed

                                 ;; OS settings
                                 ;; load only if on macos
                                 (when sys-mac
                                   'lem-setup-macos)

                                 ;; Other UI/UX
                                 'lem-setup-help
                                 'lem-setup-colors
                                 'lem-setup-modeline

                                 ;; Server
                                 'lem-setup-server))
                  (require mod nil t))))
(add-hook 'emacs-startup-hook #'lem-user-config-after-startup)

;;;;; Scratch Directory
(customize-set-variable 'lem-scratch-default-dir lem-scratch-save-dir)

;;;; User Keybindings
;; Set this to whatever you wish. All Lambda-Emacs keybinds will be available through this prefix.
(customize-set-variable 'lem-prefix "C-c C-SPC")

;;;; User Functions
;; Put any custom user functions here.

(setq lem-bibliography "~/Documents/03-resources/bibliography.bib")
(setq lem-citar-note "Notes on ${author editor}, ${title}")
(setq lem-bib-notes "~/Documents/03-resources/notes/ref-notes/")

(straight-use-package 'somafm)
(require 'somafm)

(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(use-package speed-type
  :straight t
  :commands (speed-type-text)
  :ensure t
  )

(use-package jupyter
  :straight t
  :init
  (setq jupyter-eval-use-overlays t))

;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((emacs-lisp . t) ;; Other languages
;;    (shell . t)
;;    ;; Python & Jupyter
;;    (python . t)
;;    (jupyter . t)))

;; (org-babel-jupyter-override-src-block "python")

;;;; org setup
(setq org-capture-templates
      ;; Note the ` and , to get concat to evaluate properly
      `(("c" "Capture" entry (file ,(concat org-directory "inbox.org"))
         "* TODO %?\n %i" :empty-lines 1)

        ("j" "Journal" entry (file+olp+datetree ,(concat org-directory "journal.org"))
         "**** %<%H:%M>\n%?" :empty-lines 1)

        ("l" "A link, for reading later" entry (file ,(concat org-directory "inbox.org"))
         "* %? :link: \n%(grab-mac-link 'safari 'org)"  :empty-lines 1)

        ("r" "Reference" entry (file ,(concat org-directory "reference.org"))
         "* %?"  :empty-lines 1)

        ("w" "Review: Weekly Review" entry (file+datetree ,(concat org-directory "reviews.org"))
         (file ,(concat org-directory "templates/weekly_review_template.org")))

        ("R" "Referee report" entry (file+datetree ,(concat org-directory "referee-reports.org"))
         (file ,(concat org-directory "templates/referee-report-template.org")))))

;; Add date to captured items
(defun add-property-with-date-captured ()
  "Add DATE_CAPTURED property to the current item."
  (interactive)
  (org-set-property "DATE_CAPTURED" (format-time-string "%F %A")))

(add-hook 'org-capture-before-finalize-hook 'add-property-with-date-captured)

;; Add newline to captured items
(defun lem-org-capture-newlines-at-end ()
  (goto-char (point-max))
  (insert "\n\n"))
(add-hook 'org-capture-prepare-finalize 'lem-org-capture-newlines-at-end)

;;;; Org Journal Capture
;; Tell emacs what you're doing a few times a day. Depends on a
;; [[/Users/roambot/bin/scripts/emacs_journal.sh][shell script]] run in the
;; background. I got the idea from
;; [[http://www.diegoberrocal.com/blog/2015/08/19/org-protocol/][Diego Berrocal]].
;; Hat tip to
;; [[http://stackoverflow.com/questions/23517372/hook-or-advice-when-aborting-org-capture-before-template-selection][stack
;; overflow]] for help on hooks for the created frame.

(defun lem-org-journal ()
  (interactive) (org-capture nil "j"))

;;; Org Indirect Buffer
(setq org-indirect-buffer-display 'current-window)
;; Some advice to automatically switch to a new indirect buffer upon creation
(defadvice org-tree-to-indirect-buffer (after org-tree-to-indirect-buffer-after activate) (other-window 1))


;;; Org Entities
(setq org-entities-user
      '(("nec" "\Box" nil "◻" "" "" "◻")
        ("pos" "\Diamond" nil "◇" "" "" "◇")
        ("space" "~" nil "&nbsp;" " " " " " ")))

;;; Org Tags
(setq org-tag-alist '((:startgrouptag)
                      ("@computer" . ?c)
                      (:grouptags)
                      ("emacs" . ?m)
                      (:endgrouptag)
                      ("@errand" . ?e)
                      ("@phone" . ?p)
                      ("@cbs" . ?s)
                      ("email")
                      ("postal-mail")
                      ("@home" . ?h)))
(setq org-fast-tag-selection-single-key t)

;;; Provide
(provide 'config)
;;; config.el ends here
