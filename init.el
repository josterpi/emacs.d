
;;;;
;;;; Setup
;;;;

;;; Some variables to allow different config between Windows & Linux
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar linux-p (string-match "linux" (symbol-name system-type)))

;; Set up packages
(package-initialize)
;; Small speedup:
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Package-Installation.html
(setq package-enable-at-startup nil)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )

;; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Use Package ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(eval-when-compile
  (require 'use-package))
;; (use-package auto-compile
  ;; :config (auto-compile-on-load-mode))

;; To hide minor mode lighters
(use-package diminish)

;;;;
;;;; Minor tweaks to improve emacs
;;;;

;; Suppress the welcome screen
(setq inhibit-startup-message t)

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

;; Enable narrow-to-region, but I don't know why I'd use it :)
(put 'narrow-to-region 'disabled nil)

;; C-u C-space C-space to repeart popping mark instead of C-u C-space C-u C-space
(setq set-mark-command-repeat-pop 1)

;; Globally Enable hi-lock mode. M-s h . to highlight symbol found
;; near point. M-s h u to unhighlight something
(global-hi-lock-mode 1)

;; show buffer file name in title bar
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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

(setq find-program "C:/tools/msys64/usr/bin/find.exe")

(require 'server)
(unless (server-running-p) (server-start))

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

;; Now that I'm running an emacs daemon, it's running init without
;; (display-graphic-p) being true, so my frame size is wrong.
;; We can use this hook, but it doesn't work for the first frame:
;; https://www.reddit.com/r/emacs/comments/6lxf9b/question_emacsclient_and_connection_hooks/
;; We'll just live with this for now:
(add-hook 'before-make-frame-hook
          #'(lambda ()
              (message "BEFORE FRAME HOOK")
              (if (display-graphic-p)
                  ;; Hack to set frame height to 53 on my laptop
                  (if (string= (system-name) "JOSTER")
	              (set-initial-dimensions 53)
                    ;; Set frame height to 65 on a 1080p monitor
                    (set-initial-dimensions 65)))))

(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...

;; Don't show the tool bar
(when (display-graphic-p)
  (tool-bar-mode -1))

;; to supress ElDoc in mode line
(eval-after-load "eldoc"
  '(diminish 'eldoc-mode))

;; H/T https://news.ycombinator.com/item?id=22883750
;; Handling of spaces
(setq-default show-trailing-whitespace 't)
;; H/T https://emacs.stackexchange.com/q/37069/28438
(defun jo/hide-trailing-whitespace-maybe ()
  "Disable 'show-trailing-whitespace' in selected modes."
  (when (derived-mode-p 'shell-mode
                        'comint-mode
                        'magit-popup-mode
                        'tabulated-list-mode)
    (setq show-trailing-whitespace nil)))
(add-hook 'after-change-major-mode-hook
          'jo/hide-trailing-whitespace-maybe)
(setq-default indicate-empty-lines 't)
;; try not to use tab characters ever when formatting code
(setq-default indent-tabs-mode nil)
(setq-default require-final-newline 'ask)
(setq-default mode-require-final-newline 'ask)

;;;;
;;;; Major modes
;;;;

;;; PYTHON *******************************************************************

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

;;; ORG ***********************************************************************

;; Set up some stuff for org-mode
(load "help-init") ;; helper functions for some org-mode stuff
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

;; Set the agenda files, concating org-directory since it's different
;; between linux and windows
(setq org-agenda-files
   (mapcar #'(lambda (file) (concat org-directory file))
    '("/personal.org" "/work.org" "/house.org" "/inbox.org")))

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
              ("\\" "Agenda"
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
                (todo "DONE"
                      ((org-agenda-overriding-header "Tasks to Archive")
                       ;; (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                       ;; (org-tags-match-list-sublevels nil)
                       )))
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

;;; ESS ***********************************************************************


(use-package ess
  :init (require 'ess-site)
  ;; :mode "\\.R\\'"
  :defer 5
  :bind (:map ess-r-mode-map
              ("C-|" . pipe_R_operator)
         :map inferior-ess-r-mode-map
         ("C-|" . pipe_R_operator))
  :preface
  ;; H/T https://emacs.stackexchange.com/a/8055
  (defun pipe_R_operator ()
    "R - %>% operator or 'then' pipe operator"
    (interactive)
    (just-one-space 1)
    (insert "%>%")
    ;; (reindent-then-newline-and-indent)
    )
  )
;; (setq ess-view--spreadsheet-program "C:/Program Files/LibreOffice 5/program/scalc.exe")
;; Actually, let's save some keypresses. Improved with ess-smart-underscore
;; (add-hook 'ess-mode-hook ;; Disable _ being replaced with <-
;;           (lambda ()
;;             (ess-toggle-underscore nil)))
;; (define-key ess-r-mode-map "_" #'ess-insert-assign)
;; (define-key inferior-ess-r-mode-map "_" #'ess-insert-assign)

;; Rmarkdown
(use-package poly-markdown
  :defer t)
(use-package poly-R
  :defer t
  :after poly-markdown
  :config
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  :preface
  ;; H/T https://emacs.stackexchange.com/a/27419/28438
  (defun jo/insert-r-chunk ()
    "Insert an R chunk in Rmarkdown"
    (interactive)
    (insert "```{r}\n\n```")
    (forward-line -1))

  (defun jo/insert-rchunk-header (option)
    "Add an R chunk header option"
    (save-excursion
      (search-backward "`{r")
      (move-end-of-line 1)
      (backward-char)
      (insert (concat ", " option))))

  (defun jo/select-rchunk-header ()
    "Using ivy, select an R chunk header option, which is then inserted"
    (interactive)
    (ivy-read "Chunk options: "
	      '(("include -- prevent code & results from appearing" . "include = F")
	        ("echo -- prevent code, but show results" . "echo = F")
	        ("eval -- don't run the code" . "eval = F")
	        ("cache -- cache results for future knits" . "cache = T")
	        ("message -- prevent messages from appearing" . "message = F")
	        ("warning -- prevent warnings from appearing" . "warning = F"))
	      :action (lambda (x) (jo/insert-rchunk-header (cdr x)))))
  ;; In Rmarkdown files, insert new R chunks and set their options
  :bind (:map poly-markdown+r-mode-map
              ("C-c r" . 'jo/insert-r-chunk)
              ("C-c h" . 'jo/select-rchunk-header)))

;; When I'm working on R in multiple frames, reuse the R process buffer
;; if it's anywhere. There appears to be a bug in ESS where it doesn't
;; go back to the original frame.
(setq display-buffer-alist
      `(("*R"
       (display-buffer-reuse-window)
       (reusable-frames . visible))))

;; http://ess.r-project.org/Manual/ess.html#Command-History
;; For R and any other comint buffers: type a prefix and search for matches
;; in the command history
(eval-after-load "comint"
  '(progn
     (define-key comint-mode-map [up]
       'comint-previous-matching-input-from-input)
     (define-key comint-mode-map [down]
       'comint-next-matching-input-from-input)
     ))

;;;;
;;;; Less big packages and configuration
;;;;

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
  (setq directory-abbrev-alist
        '(("c:/Users/josterhouse/OneDrive - LaSalle Company, Inc/Documents" .
           "c:/Users/josterhouse/docs")))
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

;; CSV-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; Lilypond
(autoload 'LilyPond-mode "lilypond-mode")
(setq auto-mode-alist
      (cons '("\\.ly$" . LilyPond-mode) auto-mode-alist))

(add-hook 'LilyPond-mode-hook (lambda () (turn-on-font-lock)))

;; hunspell
(add-to-list 'exec-path "C:/hunspell/bin/")
(setq ispell-program-name "hunspell")

;; ivy is already enabling recentf for virtual buffers, but I want to
;; increase the number of items saved and exclude org-mode archives
(require 'recentf)
(setq recentf-max-saved-items 100)
(add-to-list 'recentf-exclude "\.org_archive$")
(recentf-mode)

;; Done with IDO for now. Let's try Ivy
(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d) ")
  (counsel-find-file-ignore-regexp "\\.R~\\'")
  :diminish ivy-mode
  :config
  (ivy-mode))

(use-package swiper
  :after ivy
  :bind ("C-s" . swiper))

(use-package counsel
  :after ivy
  :diminish counsel-mode
  :config (counsel-mode))

;; H/T https://emacs.stackexchange.com/a/17671/28438
(defun jo/describe-char-at-mouse (event)
  (interactive "e")
  (let* ((mouse-pos  (event-start event))
         (pos-pt     (posn-point mouse-pos)))
    (describe-char pos-pt)))

;; C-mouse-1
(global-set-key [(control down-mouse-1)] 'ignore)
(global-set-key [(control mouse-1)] 'jo/describe-char-at-mouse)

;; Projectile for projects
(use-package projectile
  :after ivy
  :defer 5
  :bind-keymap*
              ;; AutoHotKey may be set up with the Windows key as super
              (("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :custom
  (projectile-completion-system 'ivy)
  :config
  (projectile-mode 1))

;; emmet-mode for amazing html and css
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;; jump-char: like f and F in vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; hide extraneous info from dired. Show/hide with '(' and ')'
(require 'dired-details)
(dired-details-install)
(setq dired-details-hidden-string "")

;; ACE jump and window settings
(use-package ace-jump-mode
  :bind ("C-c SPC" . 'ace-jump-mode))
;; (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
(use-package ace-window
  :bind (("C-x o" . 'ace-window)
         ("M-o" . 'other-window))
  :custom
  ;; Use the home row for ace-window
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; Don't have me jump to the window I'm in
  (aw-ignore-current t))

;; Magit
(use-package magit
  :defer t
  :bind ("C-x g" . 'magit-status)
  :diminish 'auto-revert-mode
  )
;; (global-set-key (kbd "C-x g") 'magit-status)

;; https://www.emacswiki.org/emacs/EmacsAsDaemon#toc10
;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server"
  (interactive)
  (if (y-or-n-p "Kill all emacs and server? ")
      (progn
        (save-some-buffers)
        (kill-emacs)))
  )
(global-set-key (kbd "C-x C-c") 'server-shutdown)

;; H/T https://www.reddit.com/r/emacs/comments/6zvw4d/dmyhd9y/
;; Scroll grep buffer past super-long find command
(defun jo/rgrep-skip-gibberish-hook (&rest _)
  (when isearch-mode (isearch-exit))
  (select-window (get-buffer-window "*grep*"))
  (goto-char (point-min))
  (next-logical-line 4)
  (recenter 0)
  (select-window (next-window)))
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Advising-Functions.html
(advice-add 'rgrep :after #'jo/rgrep-skip-gibberish-hook)
