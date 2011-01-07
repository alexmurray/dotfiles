;;;; Standard Emacs options and inbuilt packages ;;;;

;; set visual properties early to reduce frame flickering
;; set default font properties
(set-face-attribute 'default nil :font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 90)
;; disable tool-bar and scroll-bar, show matching parenthesis and time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(display-time)

;; default to utf-8
(set-language-environment "UTF-8")

;; some nice keybindings
(global-set-key "\C-x\C-h" 'hexl-mode)
(global-set-key "\C-x\C-m" 'compile)
(global-set-key "\C-xe" 'shell)

;; Show line column numbers in mode line
(line-number-mode t)
(column-number-mode t)

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; always end a file with a newline
(setq require-final-newline 'query)

;; use proper english
(setq ispell-dictionary "british")

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)

;; default to unified diff
(setq diff-switches "-u")

;; since we store .emacs in a symlinked git repo, always follow symlinks for vc
(setq vc-follow-symlinks t)

;; remember recently used files
(require 'recentf)
(recentf-mode 1)

;; ido mode for opening files and switching buffers
(ido-mode 1)

;; for org-mode
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; semantic in emacs 23.2 - useful for auto-complete mode
(require 'semantic)
(semantic-mode 1)

;;;; External packages ;;;;

;; set load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; color theme - requires emacs-goodies-el to be installed on ubuntu
(autoload 'color-theme-blackboard "color-theme-blackboard" "blackboard color theme." t)
(color-theme-blackboard)

;; auto-complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

;; rainbow mode - for colouring strings that represent colors
(autoload 'rainbow-mode "rainbow-mode" "rainbow-mode." t)
(rainbow-mode 1)

;; code snippet support
(add-to-list 'load-path (expand-file-name "~/.emacs.d/yasnippet-0.6.1c"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippet-0.6.1c/snippets")

;; undo-tree.el - from http://www.dr-qubit.org/undo-tree/undo-tree.el
(require 'undo-tree)
(global-undo-tree-mode 1)

;; scratch.el - from http://github.com/ieure/scratch-el
(autoload 'scratch "scratch" nil t)

;;;; Support for specific languages ;;;;

;; for auctex and reftex integration
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    ;; Enable source-specials for Control-click forward/reverse search.
	    (TeX-source-specials-mode 1)
	    (setq TeX-source-specials-view-start-server t)
	    ;; Enable reftex
	    (turn-on-reftex)
	    (setq reftex-plug-into-AUCTeX t)
	    ;; Enable flyspell
	    (flyspell-mode 1)
	    ;; workaround bug in autocomplete and flyspell
	    (ac-flyspell-workaround)
	    )
	  )

;; C coding etc
(add-hook 'c-mode-common-hook
          (lambda ()
	    ;; use linux kernel style
	    (c-set-style "linux")
	    ;; use tabs not spaces to indent
	    (setq indent-tabs-mode t)
	    ;; options for c-eldoc mode - could also add other libs too like OpenGL if
	    ;; needed
	    (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
	    (load "c-eldoc")
	    ;; turn on auto-newline and hungry-delete
	    (c-toggle-auto-hungry-state t)
	    ;; show trailing whitespace
	    (setq show-trailing-whitespace t)
	    ;; set auto newline
	    (setq c-auto-newline 1)
	    (setq fill-column 80)
	    (setq comment-column 60)
	    ;; turn on spell checking for strings and comments
	    (flyspell-prog-mode)
	    ;; workaround bug in autocomplete and flyspell
	    (ac-flyspell-workaround)
	    ;; make CamelCase words separate subwords (ie. Camel and Case can
	    ;; be operated on separately as separate words
	    (subword-mode 1)
	    ;; turn on c-eldoc
	    (c-turn-on-eldoc-mode)
	    ;; enable gtk-doc helpers from gtk-doc-tools to easily
	    ;; insert gtk-doc style comment declarations using C-x 4 h
	    ;; (gtk-doc-insert) or C-x 4 s (gtk-doc-insert-section) to
	    ;; comment current function or section respectively
	    (load "gtk-doc")
	    ;; use semantic as a source for auto complete
	    (setq ac-sources (append ac-sources '(ac-source-semantic)))
	    (auto-fill-mode 1)
	    ;; devhelp
	    (require 'devhelp
		     ;; Bind F6 to enable the automatic assistant.
		     (global-set-key [f6] 'devhelp-toggle-automatic-assistant)
		     ;; Bind F7 to search with the assistant window.
		     (global-set-key [f7] 'devhelp-assistant-word-at-point)
		     )

	    )
	  )

;; for C code highlight TODO and fixme so it looks scary
(font-lock-add-keywords
 'c-mode
 '(("\\<\\(TODO\\|todo\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t)))

;; android-mode
(require 'android-mode)
(setq android-mode-sdk-dir "~/android-sdk-linux_x86/")
(add-hook 'gud-mode-hook
	  (lambda ()
            (add-to-list 'gud-jdb-classpath "/home/alex/android-sdk-linux_86/platforms/android-8/android.jar")
            ))

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.32")
 '(user-full-name "Alex Murray")
 '(user-mail-address "murray.alex@gmail.com")
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
