;;;; Standard Emacs options and inbuilt packages ;;;;

;; set visual properties early to reduce frame flickering
;; set default font properties
(set-frame-font "Monospace-10")

;; disable tool-bar and scroll-bar, show matching parenthesis and time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(display-time)

;; default to utf-8
(set-language-environment "UTF-8")

;; automatically reload buffer when file on disk changes
(global-auto-revert-mode t)

;; some nice keybindings
(global-set-key "\C-x\C-h" 'hexl-mode)
(global-set-key "\C-x\C-m" 'compile)
(global-set-key "\C-xe" 'shell)

;; make f11 full-screen - from http://www.emacswiki.org/emacs/FullScreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key [f11] 'toggle-fullscreen)

;; Show line column numbers in mode line
(line-number-mode t)
(column-number-mode t)

;; show colours correctly in shell
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; use proper english
(setq ispell-dictionary "british")

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)

;; default to unified diff
(setq diff-switches "-u")

;; since we store .emacs in a symlinked git repo, always follow symlinks for vc
(setq vc-follow-symlinks t)

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

;; zeitgeist integration
(require 'zeitgeist)

;; color theme - requires emacs-goodies-el to be installed on ubuntu
(require 'color-theme)
(color-theme-initialize)
;;(autoload 'color-theme-blackboard "color-theme-blackboard" "blackboard color theme." t)
;;(color-theme-blackboard)
(autoload 'color-theme-zen-and-art "color-theme-zen-and-art" "zen-and-art color theme." t)
(color-theme-zen-and-art)

;; auto-complete mode
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;; make autostart after entering a single character
(setq ac-auto-start 1)

;; rainbow mode - for colouring strings that represent colors
(require 'rainbow-mode)

;; undo-tree.el - from http://www.dr-qubit.org/undo-tree/undo-tree.el
(require 'undo-tree)
(global-undo-tree-mode 1)

;; scratch.el - from http://github.com/ieure/scratch-el
(autoload 'scratch "scratch" nil t)

;; magit - installed as a system package
(require 'magit)
(global-set-key "\C-x\C-z" 'magit-status)

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;;;; Support for specific languages ;;;;

;; for auctex and reftex integration
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    ;; Use PDF by default
	    (TeX-PDF-mode 1)
	    (setq TeX-view-program-selection
		  '((output-pdf "Evince")
		    (output-dvi "Evince")))
	    ;; Enable source-specials for Control-click forward/reverse search.
	    (TeX-source-specials-mode 1)
	    (setq TeX-source-specials-view-start-server t)
	    ;; Enable reftex
	    (turn-on-reftex)
	    (setq reftex-plug-into-AUCTeX t)
	    ;; Enable flyspell
	    (flyspell-mode 1)
	    ;; workaround bug in autocomplete and flyspell
	    (ac-flyspell-workaround)))

;; c-mode and other derived modes (c++, java etc) etc
(add-hook 'c-mode-common-hook
          (lambda ()
	    ;; use tabs not spaces to indent
	    (setq indent-tabs-mode t)
	    ;; set a reasonable fill and comment column
	    (setq fill-column 80)
	    (setq comment-column 70)
	    ;; turn on spell checking for strings and comments
	    (flyspell-prog-mode)
	    ;; workaround bug in autocomplete and flyspell
	    (ac-flyspell-workaround)
	    ;; make CamelCase words separate subwords (ie. Camel and Case can
	    ;; be operated on separately as separate words
	    (subword-mode 1)
	    (auto-fill-mode 1)
	    ;; show trailing whitespace
	    (setq show-trailing-whitespace t)
	    ;; turn on auto-newline and hungry-delete
	    (c-toggle-auto-hungry-state t)
	    ;; set auto newline
	    (setq c-auto-newline 1)
	    ;; use semantic as source for auto complete - this
	    ;; provides the most relevant options plus we still retain
	    ;; yasnippet and gtags sources as well
	    (setq ac-sources '(ac-source-semantic))
	    ;; highlight TODO and fixme so it looks scary
	    (font-lock-add-keywords nil
				    '(("\\<\\(TODO\\|todo\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t)))))

;; c-only modes
(add-hook 'c-mode-hook
	  (lambda ()
	    ;; use linux kernel style
	    (c-set-style "linux")
	    ;; options for c-eldoc mode - could also add other libs
	    ;; too like OpenGL if needed
	    (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ ")
	    (load "c-eldoc")
	    ;; turn on c-eldoc
	    (c-turn-on-eldoc-mode)
	    ;; enable gtk-doc helpers from gtk-doc-tools to easily
	    ;; insert gtk-doc style comment declarations using C-x 4 h
	    ;; (gtk-doc-insert) or C-x 4 s (gtk-doc-insert-section) to
	    ;; comment current function or section respectively
	    ;;(load "gtk-doc")
	    ;; devhelp
	    (require 'devhelp
	    	     ;; Bind F6 to enable the automatic assistant.
	    	     (global-set-key [f6] 'devhelp-toggle-automatic-assistant)
	    	     ;; Bind F7 to search with the assistant window.
	    	     (global-set-key [f7] 'devhelp-assistant-word-at-point))
	    ;; enable gobject helper
	    (require 'gobject-class)))

;; ajc-java-complete
(add-hook 'java-mode-hook
	  (lambda ()
	    (add-to-list 'load-path (expand-file-name "~/.emacs.d/ajc-java-complete/"))
	    (require 'ajc-java-complete-config)
	    ;; manually call hook since we are already in java hook
	    (ajc-java-complete-hook)
	    ;; android-mode
	    (require 'android-mode)
	    (setq android-mode-sdk-dir "~/android-sdk-linux_x86/")
	    ;; change prefix so doesn't conflict with comment-region
	    (setq android-mode-key-prefix "\C-c \C-m")
	    (add-hook 'gud-mode-hook
		      (lambda ()
			(add-to-list 'gud-jdb-classpath "/home/alex/android-sdk-linux_x86/platforms/android-8/android.jar")))))

;; python ropemacs autocomplete
(ac-ropemacs-initialize)
(add-hook 'python-mode-hook
	  (lambda ()
	    (add-to-list 'ac-sources 'ac-source-ropemacs)))

;; ;; nxhtml
;; (load "~/.emacs.d/nxhtml/autostart.el")

; autoload slime when you open a .lisp file
(require 'slime)
(add-hook 'slime-mode-hook
 (lambda ()
   (unless (slime-connected-p)
     (save-excursion (slime)))))
; autoclose emacs even if lisp processes are running
(setq slime-kill-without-query-p t)

;; slime autocomplete
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
;; set ac-modes to include slime
(add-to-list 'ac-modes 'lisp-mode)
(add-to-list 'ac-modes 'slime-repl-mode)

;; disable easy navigation keys to learn emacs shortcuts properly -
;; from http://danamlund.dk/emacs/no-easy-keys.html
(defvar no-easy-keys-minor-mode-map (make-keymap)
  "no-easy-keys-minor-mode keymap.")
(let ((f (lambda (m)
           `(lambda () (interactive)
              (message (concat "No! use " ,m " instead."))))))
  (dolist (l '(("<left>" . "C-b") ("<right>" . "C-f") ("<up>" . "C-p")
               ("<down>" . "C-n")
               ("<C-left>" . "M-f") ("<C-right>" . "M-b") ("<C-up>" . "M-{")
               ("<C-down>" . "M-}")
               ("<M-left>" . "M-f") ("<M-right>" . "M-b") ("<M-up>" . "M-{")
               ("<M-down>" . "M-}")
               ("<delete>" . "C-d") ("<C-delete>" . "M-d")
               ("<M-delete>" . "M-d") ("<next>" . "C-v") ("<C-next>" . "M-x <")
               ("<prior>" . "M-v") ("<C-prior>" . "M-x >")
               ("<home>" . "C-a") ("<C-home>" . "M->")
               ("<C-home>" . "M-<") ("<end>" . "C-e") ("<C-end>" . "M->")))
    (define-key no-easy-keys-minor-mode-map
      (read-kbd-macro (car l)) (funcall f (cdr l)))))
(define-minor-mode no-easy-keys-minor-mode
  "A minor mode that disables the arrow-keys, pg-up/down, delete
  and backspace."  t " !E"
  'no-easy-keys-minor-mode-map :global t)
(no-easy-keys-minor-mode 1)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(user-full-name "Alex Murray")
 '(user-mail-address "murray.alex@gmail.com")
 '(x-select-enable-clipboard t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
