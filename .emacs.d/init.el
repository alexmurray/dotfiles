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

;; electric indent and layout modes to make more IDE like (see
;; autopair below instead of electric-pair-mode)
(electric-indent-mode t)
(electric-layout-mode t)

;; show empty lines in left fringe
(setq default-indicate-empty-lines t)

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

;; Use regex searches by default.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

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
   nil `(("(\\(lambda\\>\\)"
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

;; semantic in emacs 23.2 - useful for auto-complete mode
(require 'semantic)
(semantic-mode 1)

;; uniquify: unique buffer names
(require 'uniquify) ;; make buffer names more unique
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":"
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

;; use super + arrow keys to switch between visible buffers
(require 'windmove)
(windmove-default-keybindings 'super)

;; some notifications stuff
(require 'notifications)
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
      (notifications-notify :title title :body body :icon icon))))
(setq compilation-finish-functions 'compilation-finished)

;; also notify when reverting a buffer if we auto-revert
(defun notify-buffer-reverted ()
  (notifications-notify :title "Emacs buffer reverted"
			:body (buffer-name (current-buffer))))
(add-hook 'after-revert-hook 'notify-buffer-reverted)

;; which-function-mode to display current defun in modeline
(require 'which-func)
(which-func-mode t)

;;;; External packages ;;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor"))

;; smooth scrolling
(require 'smooth-scrolling)

;; zeitgeist integration
(require 'zeitgeist)

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

;; use autopair by default
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/autopair"))
(require 'autopair)
(autopair-global-mode) ;; enable autopair in all buffers
;; make sure autopair doesn't screw up some slime stuff
(set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))

;; paredit
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/paredit"))
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(autoload 'enable-paredit-mode "paredit" "Turn on paredit mode" t)

;; disable autopair during paredit
(defadvice enable-paredit-mode (before disable-autopair activate)
  (setq autopair-dont-activate t)
  (autopair-mode -1))

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
(ido-ubiquitous t)

;; auto-complete mode
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/auto-complete"))
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete/dict")
(ac-config-default)
(setq ac-auto-start 1) ; make autostart after entering a single character
(setq ac-auto-show-menu (+ ac-delay 0.1)) ; show menu after 100ms
;; quick help has to be after menu so again set to 100ms more
(setq ac-quick-help-delay (+ ac-auto-show-menu 0.1))

;; yasnippet
(add-to-list 'load-path "~/.emacs.d/vendor/yasnippet")
(require 'yasnippet)
;; use C-tab so easy to trigger with auto-complete
(setq yas/trigger-key "<C-tab>")
(yas/initialize)
(setq yas/root-directory "~/.emacs.d/vendor/yasnippet/snippets")
(yas/load-directory yas/root-directory)

;; mark-multiple.el
(add-to-list 'load-path "~/.emacs.d/vendor/mark-multiple")
(require 'inline-string-rectangle)
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)

(require 'mark-more-like-this)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)

;; slime
(add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/slime-2011-10-09"))
(setq slime-lisp-implementations '((sbcl ("/usr/bin/sbcl"))))
(require 'slime)

;; autoload slime when you open a .lisp file
(add-hook 'slime-mode-hook
	  (lambda ()
	    (unless (slime-connected-p)
	      (save-excursion (slime)))))
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

;; nxhtml
(load "~/.emacs.d/vendor/nxhtml/autostart.el")
(setq mumamo-chunk-coloring 2)

;; magit - installed as a system package
(when (locate-library "magit")
  (require 'magit)
  (global-set-key (kbd "C-x C-z") 'magit-status))

;;;; Support for specific languages ;;;;

;; use eldoc-mode for emacs-lisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

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

(defun common-programming-setup ()
  ;; turn on spell checking for strings and comments
  (flyspell-prog-mode)
  ;; workaround bug in autocomplete and flyspell
  (ac-flyspell-workaround)
  ;; highlight TODO and fixme so it looks scary
  (font-lock-add-keywords nil
   '(("\\<\\(TODO\\|todo\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t))))

;; common stuff for all programming languages
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
(add-hook 'c-mode-common-hook
          (lambda ()
	    ;; use tabs not spaces to indent
	    (setq indent-tabs-mode t)
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
	     '((c-mode-font-lock-if0 (0 font-lock-comment-face prepend))) 'add-to-end)))

;; c-only modes
(add-hook 'c-mode-hook
	  (lambda ()
	    ;; use semantic as source for auto complete
	    (add-to-list 'ac-sources 'ac-source-semantic)
	    ;; use linux kernel style
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
	      (global-set-key (kbd "<f7>") 'devhelp-assistant-word-at-point))))

;; android mode
(add-hook 'java-mode-hook
	  (lambda ()
	    ;; android-mode
	    (add-to-list 'load-path (expand-file-name "~/.emacs.d/vendor/android-mode"))
	    (require 'android-mode)
	    (setq android-mode-sdk-dir "~/android-sdk-linux_x86/")
	    ;; change prefix so doesn't conflict with comment-region
	    (setq android-mode-key-prefix "\C-c \C-m")
	    (add-hook 'gud-mode-hook
		      (lambda ()
			(add-to-list 'gud-jdb-classpath "/home/alex/android-sdk-linux_x86/platforms/android-8/android.jar")))))

;; setup python mode for eldoc and auto-complete with semantic
(add-hook 'python-mode-hook
	  (lambda ()
	    (eldoc-mode)
	    (add-to-list 'ac-sources 'ac-source-semantic)))

;; enable pymacs / ropemacs support
(when (locate-library "pymacs")
  (require 'pymacs)
  (pymacs-load "ropemacs" "rope-"))

;; prolog
(when (locate-library "prolog")
  ;; set our prolog system
  (setq prolog-system 'swi)
  ;; associate .pl files with prolog mode rather than perl mode
  (setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)) auto-mode-alist)))
