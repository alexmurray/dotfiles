;;;; Standard Emacs options and inbuilt packages ;;;;

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)

;; disable tool-bar and scroll-bar, show matching parenthesis and time
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(display-time)
;; Show line column numbers in mode line
(line-number-mode t)
(column-number-mode t)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(add-hook 'before-make-frame-hook 'turn-off-tool-bar)

;; default to utf-8
(prefer-coding-system 'utf-8)

;; automatically reload buffer when file on disk changes
(global-auto-revert-mode t)

;; built-in tab completion
(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

;; use CUA mode for rectangle selections etc but not copy/paste etc
(cua-selection-mode t)

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; show empty lines in left fringe
(setq default-indicate-empty-lines t)

;; some nice keybindings
(global-set-key (kbd "C-x C-h") 'hexl-mode)
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-x e") 'eshell)

(global-set-key (kbd "C-M-h") 'backward-kill-word) ;; like readline
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Help should search more than just commands
(global-set-key (kbd "C-h a") 'apropos)

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "\C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; a couple nice definitions taken from emacs-starter-kit
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
          "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
          "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
          "aliquip ex ea commodo consequat. Duis aute irure dolor in "
          "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
          "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
          "culpa qui officia deserunt mollit anim id est laborum."))

;; make f11 full-screen - from http://www.emacswiki.org/emacs/FullScreen
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     (if (boundp 'old-fullscreen) old-fullscreen nil)
			   (progn (setq old-fullscreen current-value)
				  'fullboth)))))
(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; use chrome as default broswer
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; delete trailing whitespace on save
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; use proper english
(setq ispell-dictionary "british")

;; make emacs use the clipboard
(setq x-select-enable-clipboard t)

;; default to unified diff
(setq diff-switches "-u")

;; since we store .emacs in a symlinked git repo, always follow symlinks for vc
(setq vc-follow-symlinks t)

;; ido mode for opening files and switching buffers
(ido-mode 1)
(setq ido-enable-flex-matching t)

;; for org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; semantic in emacs 23.2 - useful for auto-complete mode
(require 'semantic)
(semantic-mode 1)

;;;; External packages ;;;;

;; set load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

;; color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-zen-and-art)
;;(color-theme-blackboard)

;; smooth scrolling
(require 'smooth-scrolling)

;; zeitgeist integration
(require 'zeitgeist)

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

(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers

;; use dbus to notify and append messages automatically
(require 'dbus)
(defun send-desktop-notification (title body &optional icon)
  "Call the notification daemon over dbus to display a
  notification with the title body and icon"
  (if (equal icon nil) (setq icon "emacs-snapshot"))
  (dbus-call-method
    :session                        ; use the session (not system) bus
    "org.freedesktop.Notifications" ; service name
    "/org/freedesktop/Notifications"   ; path name
    "org.freedesktop.Notifications" "Notify" ; Method
    "emacs"
    0
    icon
    title
    body
    '(:array)
    '(:array (:dict-entry "x-canonical-append" (:variant "")))
    ':int32 2000))

;; notify on compilation finished
(defun compilation-finished (buffer result)
  ;; ignore non-compilation buffers (such as grep etc)
  (when (string-match "compilation" (buffer-name buffer))
    (let ((title "Emacs compilation finished")
	  (body "Success")
	  (icon nil))
      ;; remove any newlines in result message
      (while (string-match "\n" result)
	(setq result (replace-match "" t nil result)))
      ;; if looks like an error message show it with error icon
      (unless (string-match "finished" result)
	(setq body result
	      icon "gtk-dialog-error"))
      (send-desktop-notification title body icon))))
(setq compilation-finish-functions 'compilation-finished)

;; also notify when reverting a buffer if we auto-revert
(defun notify-buffer-reverted ()
  (send-desktop-notification "Emacs buffer reverted"
			     (buffer-name (current-buffer))))
(add-hook 'after-revert-hook 'notify-buffer-reverted)

;; from textmate.el and emacs-starter-kit
(require 'imenu)
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to.
Symbols matching the text at point are put first in the completion list."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position
                                     (get-text-property 1 'org-imenu-marker
                                                        symbol))))
                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    ;; If there are matching symbols at point, put them at the beginning
    ;; of `symbol-names'.
    (let ((symbol-at-point (thing-at-point 'symbol)))
      (when symbol-at-point
        (let* ((regexp (concat (regexp-quote symbol-at-point) "$"))
               (matching-symbols (delq nil
                                       (mapcar
                                        (lambda (symbol)
                                          (if (string-match regexp symbol)
                                              symbol))
                                        symbol-names))))
          (when matching-symbols
            (sort matching-symbols (lambda (a b) (> (length a) (length b))))
            (mapc (lambda (symbol)
                    (setq symbol-names (cons symbol
                                             (delete symbol symbol-names))))
                  matching-symbols)))))
    (let* ((selected-symbol (ido-completing-read "Symbol? " (reverse symbol-names)))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char (if (overlayp position) (overlay-start position) position)))))
;; Jump to a definition in the current file. (This is awesome.)
(global-set-key (kbd "C-x C-i") 'ido-imenu)

;; magit - installed as a system package
(when (locate-library "magit")
  (require 'magit)
  (global-set-key (kbd "C-x C-z") 'magit-status))

;;;; Support for specific languages ;;;;

;; for auctex and reftex integration
(add-hook 'LaTeX-mode-hook
	  (lambda ()
	    ;; use visual line mode to do soft word wrapping
	    (visual-line-mode 1)
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
	    ;; enable gobject helper
	    (require 'gobject-class)
	    ;; enable gtk-doc helpers from gtk-doc-tools to easily
	    ;; insert gtk-doc style comment declarations using C-x 4 h
	    ;; (gtk-doc-insert) or C-x 4 s (gtk-doc-insert-section) to
	    ;; comment current function or section respectively
	    ;;(load "gtk-doc")
	    ;; devhelp
	    (require 'devhelp)
	    ;; Bind F6 to enable the automatic assistant.
	    (global-set-key (kbd "<f6>") 'devhelp-toggle-automatic-assistant)
	    ;; Bind F7 to search with the assistant window.
	    (global-set-key (kbd "<f7>") 'devhelp-assistant-word-at-point)))

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

;; nxhtml
(load "~/.emacs.d/nxhtml/autostart.el")
(setq mumamo-chunk-coloring 2)

; autoload slime when you open a .lisp file
(when (locate-library "slime")
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
  (add-to-list 'ac-modes 'slime-repl-mode))

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

(when (locate-library "prolog")
  ;; set our prolog system
  (setq prolog-system 'swi)
  ;; associate .pl files with prolog mode rather than perl mode
  (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)) auto-mode-alist)))

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
