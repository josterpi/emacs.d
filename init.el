;; Suppress the welcome screen
(setq inhibit-startup-message t)

;; Don't show the tool bar
(tool-bar-mode -1)

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

; Set up some stuff for org-mode
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;; Save clock history across Emacs sessions
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)

;; virtual indentation according to outline level. by default
(setq-default org-startup-indented t)

; Use the default web browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "gnome-www-browser")

;; Set up Org-mode Capture
(setq org-default-notes-file (concat org-directory "~/gtd.org"))
(define-key global-map "\C-cc" 'org-capture)

;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono")))))
;; 
