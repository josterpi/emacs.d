
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar linux-p (string-match "linux" (symbol-name system-type)))

;; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(load "help-init") ;; helper functions for some org-mode stuff

;; https://www.emacswiki.org/emacs/BackToIndentationOrBeginning
(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (back-to-indentation) (point)))
	   (beginning-of-line)))
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; make PC keyboard's Win key or other to type Super or Hyper, for emacs running on Windows.
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super) ; Left Windows key

(setq w32-pass-rwindow-to-system nil)
(setq w32-rwindow-modifier 'super) ; Right Windows key

;; Don't litter my hard drive with backup files
(setq
 backup-by-copying t			; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/saves/"))		; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)			; use versioned backups

;; https://www.emacswiki.org/emacs/ShowParenMode
(show-paren-mode 1)

;; show cursor position within line
(column-number-mode 1)

;; C-u C-space C-space to repeart popping mark instead of C-u C-space C-u C-space
(setq set-mark-command-repeat-pop 1)

(setq find-program "C:/tools/msys64/usr/bin/find.exe")

(require 'server)
(unless (server-running-p) (server-start))

;; Suppress the welcome screen
(setq inhibit-startup-message t)

(defmacro set-initial-dimensions (height-lines)
  `(setq initial-frame-alist
        '(
          (tool-bar-lines . 0)
          (width . 106) ; chars
          (height . ,height-lines) ; lines
          (left . 50)
          (top . 0)))
  `(setq default-frame-alist
        '(
          (tool-bar-lines . 0)
          (width . 106)
          (height . ,height-lines)
          (left . 50)
          (top . 0))))

(if (display-graphic-p)
    (if (string= (system-name) "JOSTER") ; Hack to set frame height to 53 on my laptop
	(set-initial-dimensions 53)
      (set-initial-dimensions 65)))	; Set frame height to 65 on a 1080p monitor

;; Set up initial frame size
;; (if (display-graphic-p)
;;     (progn
;;       (setq initial-frame-alist
;;             '(
;;               (tool-bar-lines . 0)
;;               (width . 106) ; chars
;;               (height . 65) ; lines
;;               (left . 50)
;;               (top . 0)))
;;       (setq default-frame-alist
;;             '(
;;               (tool-bar-lines . 0)
;;               (width . 106)
;;               (height . 65)
;;               (left . 50)
;;               (top . 0)))))

(require 'package)
;; (setq package-enable-at-startup nil) ;; I don't know why this was here
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...

;; Don't show the tool bar
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Use my latest version of python-mode
;; (setq py-install-directory "~/.emacs.d/python-mode.el-6.1.2")
;; (add-to-list 'load-path py-install-directory)
;; (require 'python-mode)

(defun jo/python-shell-send-line-and-next ()
  "Send a line of code to the shell and advance to the next line, ignoring indentation"
  (interactive)
  (let (start end)
    (save-excursion
      (back-to-indentation)
      (setq start (point))
      (move-end-of-line nil)
      (setq end (point))
      (kill-ring-save start end)
      (with-current-buffer "*Python*"
	(end-of-buffer)
        (yank)
	(comint-send-input)))
    (move-end-of-line nil)
    (next-line)))

(add-hook 'python-mode-hook
	  #'(lambda ()
	      (define-key python-mode-map (kbd "<C-return>") 'jo/python-shell-send-line-and-next)))

;; (defun jo/test () ;; use Elisp to run something in an R command and get the results
;;   (interactive) ;; Ultimately, I want completion in piped expressions
;;   (let ((proc (ess-get-next-available-process)))
;;     (let ((buf (get-buffer-create " *ess-command-output*")))
;;       (with-current-buffer (process-buffer proc)
;; 	(ess-command "names(intran)\n" buf))
;;       (with-current-buffer buf
;; 	(kill-ring-save (point-min) (point-max))))))

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

(setq
;; python-shell-interpreter-args "-colors NoColor"
;; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
python-shell-completion-setup-code
  "from IPython.core.completerlib import module_completion"
python-shell-completion-module-string-code
  "';'.join(module_completion('''%s'''))\n"
python-shell-completion-string-code
  "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
  )
;; (setq-default py-shell-name "ipython")
;; (setq-default py-which-bufname "IPython")

(when (executable-find "ipython")
  (setq python-shell-interpreter "ipython"))

; Set up some stuff for org-mode
(setq org-export-backends (quote (ascii html latex icalendar md)))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(global-set-key "\C-cb" 'org-iswitchb)
;;(define-key global-map "\C-ca" 'org-agenda)
(global-set-key (kbd "<f12>") 'org-agenda)

(setq org-log-done t)
;; Save clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

(when mswindows-p
  ;; ~/docs is a symlink convention I use for command line use which
  ;; goes to wherever my Windows Documents folder is. Currently,
  ;; that's in OneDrive, so it's really obnoxious to navigate to on
  ;; the CL
  (setq org-directory "~/docs/org"))

;; Use org-habit, only because norang depends on it for now
(add-to-list 'org-modules 'org-habit)

;; Org-mode refiling across files
;; http://permalink.gmane.org/gmane.emacs.orgmode/34029
(setq org-refile-targets '((nil :maxlevel . 2)
                                ; all top-level headlines in the
                                ; current buffer are used (first) as a
                                ; refile target
                           (org-agenda-files :maxlevel . 2)))

;; provide refile targets as paths, including the file name
;; (without directory) as level 1 of the path
(setq org-refile-use-outline-path 'file)

(setq org-outline-path-complete-in-steps nil)

;; allow to create new nodes (must be confirmed by the user) as
;; refile targets
(setq org-refile-allow-creating-parent-nodes 'confirm)

;; refile only within the current buffer
(defun my/org-refile-within-current-buffer ()
  "Move the entry at point to another heading in the current buffer."
  (interactive)
  (let ((org-refile-targets '((nil :maxlevel . 5))))
    (org-refile)))

;; Set up Org-mode Capture
(setq org-default-notes-file (concat org-directory "/inbox.org"))
(define-key global-map "\C-cc" 'org-capture)
;; org-mode capture templates
(setq org-capture-templates
      `(("i" "Inbox" entry (file ,(concat org-directory "/inbox.org"))
             "* TODO %?\n  %U")
	("s" "Snippet" entry (file ,(concat org-directory "/inbox.org"))
             "* TODO %?\n  %x\n  %U")
        ("j" "Journal" entry (file+datetree ,(concat org-directory "/journal.org"))
	 "* %U\n%?")))

;; Custom Agenda: http://doc.norang.ca/org-mode.html#CustomAgendaViewSetup
(setq org-agenda-custom-commands
      (quote (("N" "Notes" tags "NOTE"
               ((org-agenda-overriding-header "Notes")
                (org-tags-match-list-sublevels t)))
              ("h" "Habits" tags-todo "STYLE=\"habit\""
               ((org-agenda-overriding-header "Habits")
                (org-agenda-sorting-strategy
                 '(todo-state-down effort-up category-keep))))
	      ("w" "Test"
               ((tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))))
              (" " "Agenda"
               ((agenda "" nil)
                (tags "REFILE"
                      ((org-agenda-overriding-header "Tasks to Refile")
                       (org-tags-match-list-sublevels nil)))
                (tags-todo "-CANCELLED/!"
                           ((org-agenda-overriding-header "Stuck Projects")
                            (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-HOLD-CANCELLED/!"
                           ((org-agenda-overriding-header "Projects")
                            (org-agenda-skip-function 'bh/skip-non-projects)
                            (org-tags-match-list-sublevels 'indented)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED/!NEXT"
                           ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                            (org-tags-match-list-sublevels t)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(todo-state-down effort-up category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Project Subtasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-REFILE-CANCELLED-WAITING-HOLD/!"
                           ((org-agenda-overriding-header (concat "Standalone Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-project-tasks)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-sorting-strategy
                             '(category-keep))))
                (tags-todo "-CANCELLED+WAITING|HOLD/!"
                           ((org-agenda-overriding-header (concat "Waiting and Postponed Tasks"
                                                                  (if bh/hide-scheduled-and-waiting-next-tasks
                                                                      ""
                                                                    " (including WAITING and SCHEDULED tasks)")))
                            (org-agenda-skip-function 'bh/skip-non-tasks)
                            (org-tags-match-list-sublevels nil)
                            (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                            (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)))
                (tags "-REFILE/"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       (org-tags-match-list-sublevels nil))))
               nil))))

;; http://nflath.com/2010/03/org-mode-2/
;; (setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-insert-heading-respect-content t)
(setq org-return-follows-link t)

;; for Org-Mobile
(setq org-mobile-directory "~/Dropbox/org")
;; Don't add id properties to things
(setq org-mobile-force-id-on-agenda-items nil)


;; virtual indentation according to outline level. by default
(setq-default org-startup-indented t)

; Use the default web browser
(when linux-p
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "gnome-www-browser"))

; On windows, set up paths to get git to work
(when mswindows-p
  ;;(setq explicit-shell-file-name
  ;;    "C:/Program Files (x86)/Git/bin/bash.exe")
  ;;(setq shell-file-name explicit-shell-file-name)
  ;(setq default-directory (concat (file-name-as-directory (getenv "UserProfile")) "AppData/Roaming" ))
  (setq default-directory (getenv "UserProfile"))
  (setenv "GIT_ASKPASS" "git-gui--askpass") ;; Use GUI to ask for credentials
  (setenv "SSH_ASKPASS" "git-gui--askpass") ;; Use GUI to ask for SSH key credentials
  ;;(add-to-list 'exec-path "C:/Program Files (x86)/Git/bin")
)

(defun jo/git-sync ()
  "For use with org to synchronize to git repo"
  (interactive)
  (let ((system (downcase (system-name))))
    ;; If git status is not blank, then commit all changed files
    (if (magit-git-string "status" "-s" "-uno") ; -s(hort) -uno (no unstaged files)
	(magit-run-git "commit" "-a" (format "--message='%s'" system)))
    (magit-pull-from-upstream nil)
    (magit-push-current-to-upstream nil)))

;; Lilypond
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; ESS
(require 'ess)  ;; I think this is already enables through MELPA
(setq ess-view--spreadsheet-program "C:/Program Files/LibreOffice 5/program/scalc.exe")
;; Actually, let's save some keypresses. Improved with ess-smart-underscore
;; (add-hook 'ess-mode-hook ;; Disable _ being replaced with <-
;;           (lambda ()
;;             (ess-toggle-underscore nil)))

;; H/T https://emacs.stackexchange.com/a/8055
(defun pipe_R_operator ()
  "R - %>% operator or 'then' pipe operator"
  (interactive)
  (just-one-space 1)
  (insert "%>%")
  ;; (reindent-then-newline-and-indent)
  )
(define-key ess-mode-map (kbd "C-|") 'pipe_R_operator)
(define-key inferior-ess-mode-map (kbd "C-|") 'pipe_R_operator)

;; hunspell
(add-to-list 'exec-path "C:/hunspell/bin/")
(setq ispell-program-name "hunspell")

;; IDO
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-create-new-buffer 'always) ; Don't ask for confirmation

;; Done with IDO for now. Let's try Ivy
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq counsel-find-file-ignore-regexp "\\.R~\\'")

;; Projectile for projects
(projectile-mode 1)
;; I don't currently have Super set up, but it could be the windows key
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(setq projectile-completion-system 'ivy)

;; emmet-mode for amazing html and css
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Globally Enable hi-lock mode. M-s h . to highlight symbol found near point. M-s h u to unhighlight something
(global-hi-lock-mode 1)

;; Enable company-mode everywhere
(add-hook 'after-init-hook 'global-company-mode)

;; jump-char: like f and F in vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; hide extraneous info from dired. Show/hide with '(' and ')'
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")

;; Enable narrow-to-region, but I don't know why I'd use it :)
(put 'narrow-to-region 'disabled nil)

;; ACE jump and window settings
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "C-x o") 'ace-window)
(global-set-key (kbd "M-o") 'other-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; Use the home row for ace-window
(setq aw-ignore-current t) ;; Don't have me jump to the window I'm in

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)
