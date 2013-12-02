
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))
(defvar linux-p (string-match "linux" (symbol-name system-type)))

;; Suppress the welcome screen
(setq inhibit-startup-message t)

(setq abbrev-file-name             ;; tell emacs where to read abbrev
      "~/.emacs.d/abbrev_defs")    ;; definitions from...

;; Don't show the tool bar
(when (display-graphic-p)
  (tool-bar-mode -1))

;; Use my latest version of python-mode
(setq py-install-directory "~/.emacs.d/python-mode.el-6.1.2")
(add-to-list 'load-path py-install-directory)
(require 'python-mode)

;(setq
; python-shell-interpreter-args "-colors NoColor"
; python-shell-prompt-regexp "In \\[[0-9]+\\]: "
; python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
; python-shell-completion-setup-code
;   "from IPython.core.completerlib import module_completion"
; python-shell-completion-module-string-code
;   "';'.join(module_completion('''%s'''))\n"
; python-shell-completion-string-code
;   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n"
;   )
;(setq-default py-shell-name "ipython")
;(setq-default py-which-bufname "IPython")

;; Use my local org-mode
(add-to-list 'load-path "~/.emacs.d/org-8.2.3c/lisp")
(add-to-list 'load-path "~/.emacs.d/org-8.2.3c/contrib/lisp")
; Set up some stuff for org-mode
(setq org-export-backends (quote (ascii html latex icalendar md)))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; Save clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

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
(setq org-default-notes-file (concat org-directory "/gtd.org"))
(define-key global-map "\C-cc" 'org-capture)
;; org-mode capture templates
(setq org-capture-templates
      '(("i" "Inbox" entry (file+headline "~/org/gtd.org" "Inbox")
             "* TODO %?\n  %U")
	("s" "Snippet" entry (file+headline "~/org/gtd.org" "Inbox")
             "* TODO %?\n  %x\n  %U")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
             "* %U\n%?")))

;; http://nflath.com/2010/03/org-mode-2/
(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
(setq org-insert-heading-respect-content t)
(setq org-return-follows-link t)

;; virtual indentation according to outline level. by default
(setq-default org-startup-indented t)

; Use the default web browser
(when linux-p
  (setq browse-url-browser-function 'browse-url-generic
	browse-url-generic-program "gnome-www-browser"))

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
;; 
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/org/house.org" "~/org/gtd.org"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
