(setq user-full-name "Alex Murray")
(setq user-mail-address "murray.alex@gmail.com")

;;;; Standard Emacs options and inbuilt packages ;;;;

;; inhibit startup message and splash screen
(setq inhibit-startup-message t)
;; remove message from initial scratch buffer
(setq initial-scratch-message nil)

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
  (blink-cursor-mode -1)
  (set-face-attribute 'default nil :font "Ubuntu Mono 12"))

;; default to utf-8
(prefer-coding-system 'utf-8)

;; automatically reload buffer when file on disk changes
(global-auto-revert-mode t)

;; use CUA mode for rectangle selections etc but not copy/paste etc
(cua-selection-mode t)

;; enable delete-selection mode to allow replacing selected region
;; with new text automatically
(delete-selection-mode 1)

;; electric indent and layout modes to make more IDE like
(electric-indent-mode t)
(electric-layout-mode t)
(electric-pair-mode t)

;; show empty lines in left fringe
(setq indicate-empty-lines t)

;; just use y or n not yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

;; some nice keybindings
(global-set-key (kbd "C-x C-h") 'hexl-mode)
(global-set-key (kbd "C-x C-m") 'compile)
(global-set-key (kbd "C-x m") 'eshell)

;; C-h is more useful as a delete substitute like readline
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "C-c C-h") help-map)
(global-set-key (kbd "C-M-h") 'backward-kill-word) ;; also like readline
(global-set-key (kbd "C-c r") 'revert-buffer)

;; Help should search more than just commands
(global-set-key (kbd "C-c C-h a") 'apropos)

;; Use regex searches and replace by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-%") 'query-replace)

;; http://www.emacswiki.org/emacs-en/CopyAboveWhileSame
(autoload 'copy-from-above-command "misc"
    "Copy characters from previous nonblank line, starting just above point.

  \(fn &optional arg)"
    'interactive)

(defun copy-above-while-same ()
  "Copy from the previous two lines until the first difference."
  (interactive)
  (let* ((col (current-column))
	 (n (compare-buffer-substrings
	     (current-buffer) ;; This buffer
	     (save-excursion
	       (forward-line -2)
	       (move-to-column col)
	       (point)) ;; Start 1
	     (line-end-position -1) ;; End 1
	     (current-buffer) ;; Still this buffer
	     (save-excursion
	       (forward-line -1)
	       (move-to-column col)
	       (point)) ;; Start 2
	     (line-end-position 0)))) ;; End 2
    (if (and (integerp n)
	     (> (abs n) 1))
	(copy-from-above-command (1- (abs n) ))
      (copy-from-above-command 1))))
(global-set-key (kbd "S-<right>") 'copy-above-while-same)

;; pretty lambda (see also slime) ->  "λ"
;;  'greek small letter lambda' / utf8 cebb / unicode 03bb -> \u03BB / mule?!
;; in greek-iso8859-7 -> 107  >  86 ec
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(\\lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    'font-lock-keyword-face))))))

;; if no mark is active then change copy / cut to do current line
;; rather than nothing to easily allow copying / cutting of lines
;; without selecting them - from
;; http://emacs-fu.blogspot.com/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active
	(list (region-beginning) (region-end))
      (list (line-beginning-position) (line-beginning-position 2)))))

;; a couple nice definitions taken from emacs-starter-kit
(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo::" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo::" buffer-file-name))))

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
(defvar *old-fullscreen* nil)
(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
    (set-frame-parameter nil 'fullscreen
			 (if (equal 'fullboth current-value)
			     *old-fullscreen*
			   (progn (setq *old-fullscreen* current-value)
				  'fullboth)))))
(global-set-key (kbd "<f11>") 'toggle-fullscreen)

;; use chrome as default broswer
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

;; show colours correctly in shell
(ansi-color-for-comint-mode-on)

;; delete trailing whitespace on save - make sure we can toggle it
(defun enable-delete-trailing-whitespace ()
  (interactive)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(defun disable-delete-trailing-whitespace ()
  (interactive)
  (remove-hook 'before-save-hook 'delete-trailing-whitespace))

;; by default delete-trailing whitespace in all buffers
(enable-delete-trailing-whitespace)

;; sentences have single spaces, not double spaces in between them -
;; http://www.slate.com/articles/technology/technology/2011/01/space_invaders.html
(setq sentence-end-double-space nil)

;; use proper english
(setq ispell-dictionary "british")

;; default to unified diff
(setq diff-switches "-u")

;; since we store .emacs in a symlinked git repo, always follow symlinks for vc
(setq vc-follow-symlinks t)

;; ido mode for opening files and switching buffers
(ido-mode 1)
(setq ido-enable-flex-matching t
      ;; use ido virtual buffers to remember previously opened files
      ido-use-virtual-buffers t)

;; when using ido, the confirmation is rather annoying...
(setq confirm-nonexistent-file-or-buffer nil)

;; Jump to a definition in the current file. (This is awesome.)
(require 'imenu)
(global-set-key (kbd "C-x C-i") 'imenu)

;; for org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; semantic and semanticdb - stores semantic information in a db so is
;; faster to compute next time a file is loaded
(require 'semantic)
(require 'semantic/db)
(semantic-mode 1)
(global-semanticdb-minor-mode 1)
;; show summary of tag at point when idle
(global-semantic-idle-summary-mode 1)

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; use super + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)

;; some notifications stuff - only do if can load with no error so
;; when via ssh etc and no dbus we are still okay
(when (require 'notifications "notifications" t)
  ;; notify on compilation finished
  (defun notify-compilation-finished (buffer result)
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
	;; close notification automatically after 5 seconds
	(let ((n (notifications-notify :title title :body body :icon icon
				       :timeout 5000 :transient t)))
	  (run-with-timer 5 nil #'notifications-close-notification n)))))
  (setq compilation-finish-functions 'notify-compilation-finished)

  ;; also notify when reverting a buffer if we auto-revert
  (defun notify-buffer-reverted ()
    ;; close notification automatically after 5 seconds
    (let ((n (notifications-notify :title "Emacs buffer reverted"
				   :body (buffer-name (current-buffer))
				   :timeout 5000 :transient t)))
      (run-with-timer 5 nil #'notifications-close-notification n)))
  (add-hook 'after-revert-hook 'notify-buffer-reverted))

;; which-function-mode to display current defun in modeline
(require 'which-func)
(which-function-mode t)

;;;; External packages ;;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

;; smooth scrolling
(require 'smooth-scrolling)

;; zeitgeist integration
(require 'zeitgeist)

;; ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/vendor/ace-jump-mode")
(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; expand region
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/expand-region"))
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; rainbow mode - for colouring strings that represent colors
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/rainbow-mode"))
(require 'rainbow-mode)
;; enable rainbow mode automatically for css and html modes
(dolist (hook '(css-mode-hook html-mode-hook))
  (add-hook hook 'rainbow-mode))

;; undo-tree.el - from http://www.dr-qubit.org/undo-tree/undo-tree.el
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/undo-tree"))
(require 'undo-tree)
(global-undo-tree-mode 1)

;; scratch.el
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/scratch-el"))
(autoload 'scratch "scratch" nil t)

;; goto-last-change
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/goto-last-change"))
(autoload 'goto-last-change "goto-last-change"
   "Set point to the position of the last change." t)
(global-set-key (kbd "C-x C-\\") 'goto-last-change)

;; paredit
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/paredit"))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(autoload 'enable-paredit-mode "paredit" "Turn on paredit mode" t)

(dolist (hook '(emacs-lisp-mode-hook lisp-mode-hook))
  (add-hook hook 'pretty-lambdas)
  (add-hook hook 'enable-paredit-mode))

(defun conditionally-enable-paredit-mode ()
  "Enable paredit-mode during eval-expression"
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load "cua-rect"
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name -1)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))
(suspend-mode-during-cua-rect-selection 'paredit-mode)

;; markdown-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/markdown-mode"))
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (append '(("\\.md" . markdown-mode)) auto-mode-alist))

;; solarized theme
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized"))
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/vendor/emacs-color-theme-solarized"))
(load-theme 'solarized-light t)

;; smex (http://www.emacswiki.org/emacs/Smex)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/smex"))
(require 'smex)
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; ubiquitous ido
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ido-ubiquitous"))
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

;; auto-complete mode - requires fuzzy and popup packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/fuzzy-el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/popup-el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
(ac-config-default)
;; don't show automatically - instead use tab to show completions
(setq ac-delay 0)
(setq ac-auto-show-menu nil)
(setq ac-expand-on-auto-complete nil)
(define-key ac-completing-map "\t" 'ac-complete)

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet)
(setq yas/snippet-dirs "~/.emacs.d/vendor/yasnippet/snippets")
;; enable yasnippet globally
(yas/global-mode 1)

;; mark-multiple.el
(add-to-list 'load-path "~/.emacs.d/vendor/mark-multiple")
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)

;; slime
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/slime-2012-05-09"))
(setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
(require 'slime)

;; autoload slime when you open a .lisp file
(defun slime-mode-setup ()
  (unless (slime-connected-p)
    (save-excursion (slime))))
(add-hook 'slime-mode-hook 'slime-mode-setup)
;; autoclose emacs even if lisp processes are running
(setq slime-kill-without-query-p t)
;; bind C-z to slime-selector
(global-set-key (kbd "C-z") 'slime-selector)
;; enable paredit for slime modes
(dolist (hook '(slime-mode-hook slime-repl-mode-hook))
  (add-hook hook 'enable-paredit-mode))
;; use fancy for REPL etc by default
(slime-setup '(slime-fancy))

;; slime autocomplete
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/ac-slime"))
(require 'ac-slime)
;; set load slime-ac on slime modes and set ac-modes to include slime
(dolist (mode '(slime-mode slime-repl-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'set-up-slime-ac)
  (add-to-list 'ac-modes mode))

;; php-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/php-mode"))
(require 'php-mode)

;; js2-mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/js2-mode"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; magit - installed as a system package
(when (locate-library "magit")
  (require 'magit)
  (global-set-key (kbd "C-x C-z") 'magit-status))

;;;; Support for specific languages ;;;;

;; common stuff for all programming languages
(defun common-programming-setup ()
  ;; turn on spell checking for strings and comments
  (flyspell-prog-mode)
  ;; workaround bug in autocomplete and flyspell
  (ac-flyspell-workaround)
  ;; highlight TODO and fixme so it looks scary
  (font-lock-add-keywords nil
   '(("\\<\\(TODO\\|todo\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t))))

(dolist (hook '(c-mode-common-hook
		lisp-mode-hook
		emacs-lisp-mode-hook
		python-mode-hook
		shell-mode-hook
		php-mode-hook
		css-mode-hook
		nxml-mode-hook
		javascript-mode-hook))
  (add-hook hook 'common-programming-setup))

;; use eldoc-mode for emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

(defun latex-mode-setup ()
  ;; use visual line mode to do soft word wrapping
  (visual-line-mode 1)
  ;; Enable flyspell
  (flyspell-mode 1)
  ;; for auctex and reftex integration
  (when (locate-library "auctex")
    (require 'auctex)
    ;; standard auctex setup
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)
    ;; enable math mode in latex
    (LaTeX-math-mode 1)
    ;; Enable reftex
    (turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    ;; Enable source-specials for Control-click forward/reverse search.
    (TeX-source-specials-mode 1)
    (setq TeX-source-specials-view-start-server t)))
(add-hook 'LaTeX-mode-hook 'latex-mode-setup)

;; show #if 0 / #endif etc regions in comment face - taken from
;; http://stackoverflow.com/questions/4549015/in-c-c-mode-in-emacs-change-face-of-code-in-if-0-endif-block-to-comment-fa
(defun c-mode-font-lock-if0 (limit)
  (save-restriction
    (widen)
    (save-excursion
      (goto-char (point-min))
      (let ((depth 0) str start start-depth)
        (while (re-search-forward "^\\s-*#\\s-*\\(if\\|else\\|endif\\)" limit 'move)
          (setq str (match-string 1))
          (if (string= str "if")
              (progn
                (setq depth (1+ depth))
                (when (and (null start) (looking-at "\\s-+0"))
                  (setq start (match-end 0)
                        start-depth depth)))
            (when (and start (= depth start-depth))
              (c-put-font-lock-face start (match-beginning 0) 'font-lock-comment-face)
              (setq start nil))
            (when (string= str "endif")
              (setq depth (1- depth)))))
        (when (and start (> depth 0))
          (c-put-font-lock-face start (point) 'font-lock-comment-face)))))
  nil)

;; c-mode and other derived modes (c++, java etc) etc
(defun c-mode-common-setup ()
  ;; use spaces not tabs to indent
  (setq indent-tabs-mode nil)
  ;; set a reasonable fill and comment column
  (setq fill-column 80)
  (setq comment-column 70)
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
  ;; show #if 0 / #endif etc regions in comment face
  (font-lock-add-keywords
   nil
   '((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end))

(add-hook 'c-mode-common-hook 'c-mode-common-setup)

;; c-only modes
(defun c-mode-setup ()
  ;; use semantic as source for auto complete
  (add-to-list 'ac-sources 'ac-source-semantic)
  ;; use linux kernel and hence GNOME coding style for C
  (c-set-style "linux")
  (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/c-eldoc"))
  (require 'c-eldoc)
  ;; turn on c-eldoc
  (c-turn-on-eldoc-mode)
  ;; enable gobject helper
  (require 'gobject-class)
  ;; enable gtk-doc helpers from gtk-doc-tools to easily
  ;; insert gtk-doc style comment declarations using C-x 4 h
  ;; (gtk-doc-insert) or C-x 4 s (gtk-doc-insert-section) to
  ;; comment current function or section respectively
  (load "gtk-doc" t) ; ignore error if can't be found
  ;; devhelp - ignore error if couldn't be loaded
  (when (require 'devhelp nil t)
    ;; Bind F6 to enable the automatic assistant.
    (global-set-key (kbd "<f6>") 'devhelp-toggle-automatic-assistant)
    ;; Bind F7 to search with the assistant window.
    (global-set-key (kbd "<f7>") 'devhelp-assistant-word-at-point)))
(add-hook 'c-mode-hook 'c-mode-setup)

;; android mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/android-mode"))
(require 'android-mode)
(setq android-mode-sdk-dir "~/android-sdk-linux/")
;; change prefix so doesn't conflict with comment-region
(setq android-mode-key-prefix (kbd "C-c C-m"))
;; setup gud for debugging - http://gregorygrubbs.com/development/tips-on-android-development-using-emacs
(defun gud-mode-setup ()
  (add-to-list 'gud-jdb-classpath (expand-file-name "~/android-sdk-linux/platforms/android-10/android.jar")))
(add-hook 'gud-mode-hook 'gud-mode-setup)

;; setup python mode for eldoc and auto-complete with semantic
(defun python-mode-setup ()
  (eldoc-mode)
  (add-to-list 'ac-sources 'ac-source-semantic))
(add-hook 'python-mode-hook 'python-mode-setup)

;; enable pymacs / ropemacs support
(when (locate-library "pymacs")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-")
  (setq ropemacs-guess-project t)
  (setq ropemacs-separate-doc-buffer t)
  (setq ropemacs-enable-autoimport nil))

;; prolog
(when (locate-library "prolog")
  (require 'prolog)
  ;; set our prolog system
  (setq prolog-system 'swi)
  ;; associate .pl files with prolog mode rather than perl mode
  (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)) auto-mode-alist)))
