;;; -*- coding: utf-8 -*-
;;; zenburn-theme.el --- zenburn, custom-theme version
;; Copyright (C) 2003, 2004, 2005, 2006  Daniel Brockman
;; Copyright (C) 2009  Adrian C., Bastien Guerry
;; Copyright (C) 2010 Kao Félix

;; Author: Kao Félix <kcfelix@gmail.com>
;; URL: http://bitbucket.org/kcfelix/zenburn-theme.el
;;

;; This file is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty
;; of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with GNU Emacs; if not, write to the Free
;; Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Based heavily on code from zenburn.el found here:
;; http://www.brockman.se/software/zenburn/zenburn.el
;;
;; The original zenburn theme was created by Jani Nurminen for Vim.
;; More info about it can be found on
;; http://slinky.imukuppi.org/zenburnpage/

(deftheme zenburn
  "Created 2010-03-18.")

;; Define variables for colors
(defvar zenburn-fg "#dcdccc")
(defvar zenburn-bg-1 "#2b2b2b")
(defvar zenburn-bg "#3f3f3f")
(defvar zenburn-bg+1 "#4f4f4f")
(defvar zenburn-bg+2 "#5f5f5f")
(defvar zenburn-yellow+1 "#dfdfbf")
(defvar zenburn-yellow "#f0dfaf")
(defvar zenburn-yellow-1 "#e0cf9f")
(defvar zenburn-yellow-2 "#d0bf8f")
(defvar zenburn-orange "#dfaf8f")
(defvar zenburn-red+1 "#dca3a3")
(defvar zenburn-red "#cc9393")
(defvar zenburn-red-1 "#bc8383")
(defvar zenburn-red-2 "#ac7373")
(defvar zenburn-red-3 "#9c6363")
(defvar zenburn-red-4 "#8c5353")
(defvar zenburn-green-1 "#5f7f5f")
(defvar zenburn-green "#7f9f7f")
(defvar zenburn-green+1 "#8fb28f")
(defvar zenburn-green+2 "#9fc59f")
(defvar zenburn-green+3 "#afd8af")
(defvar zenburn-green+4 "#bfebbf")
(defvar zenburn-blue+1 "#94bff3")
(defvar zenburn-blue "#8cd0d3")
(defvar zenburn-blue-1 "#7cb8bb")
(defvar zenburn-blue-2 "#6ca0a3")
(defvar zenburn-blue-3 "#5c888b")
(defvar zenburn-blue-4 "#4c7073")
(defvar zenburn-cyan "#93e0e3")
(defvar zenburn-magenta "#dc8cc3")

;; Faces for inheriting
(defface zenburn-primary-1 `((t :foreground ,zenburn-yellow :weight bold))
  "Zenburn primary face 1")
(defface zenburn-primary-2 `((t :foreground ,zenburn-orange :weight bold))
  "Zenburn primary face 2")
(defface zenburn-primary-3 `((t :foreground "#dfdfbf" :weight bold))
  "Zenburn primary face 3")
(defface zenburn-primary-4 `((t :foreground "#dca3a3" :weight bold))
  "Zenburn primary face 4")
(defface zenburn-primary-5 `((t :foreground "#94bff3" :weight bold))
  "Zenburn primary face 5")
(defface zenburn-highlight-damp `((t :foreground "#88b090" :background "#2e3330"))
  "Zenburn highlight damp")
(defface zenburn-highlight-alerting `((t :background "#2e3330"))
  "Zenburn highlight alerting")
(defface zenburn-highlight-subtle `((t :background "#464646"))
  "Zenburn highlight subtle")
(defface zenburn-lowlight-1 `((t :foreground "#606060"))
  "Zenburn lowlight face 1")
(defface zenburn-lowlight-2 `((t :foreground "#708070"))
  "Zenburn lowlight face 2")

;;; TODO: Put something on those dummy docstrings
(defface plain-widget-button `((t (:weight bold)))
  "")
(defface plain-widget-button-pressed `((t (:inverse-video t)))
  "")
(defface plain-widget-documentation `((t (:inherit font-lock-doc)))
  "")
(defface plain-widget-field `((t (:background ,zenburn-bg+2)))
  "")
(defface plain-widget-inactive `((t (:strike-through t)))
  "")
(defface plain-widget-single-line-field `((t (:background ,zenburn-bg+2)))
  "")
(defface fancy-widget-button `((t (:background ,zenburn-bg+1
                                               :box (:line-width 2 :style released-button))))
  "")
(defface fancy-widget-button-pressed `((t (:background ,zenburn-bg+1
                                                       :box (:line-width 2 :style pressed-button))))
  "")
(defface fancy-widget-button-highlight `((t (:background ,zenburn-bg+1
                                                         :box (:line-width 2 :style released-button))))
  "")
(defface fancy-widget-button-pressed-highlight `((t (:background ,zenburn-bg+1
                                                                 :box (:line-width 2 :style pressed-button))))
  "")
(defface fancy-widget-documentation `((t (:inherit font-lock-doc)))
  "")
(defface fancy-widget-field `((t (:background ,zenburn-bg+2)))
  "")
(defface fancy-widget-inactive `((t (:strike-through t)))
  "")
(defface fancy-widget-single-line-field `((t (:background ,zenburn-bg+2)))
  "")

;;; Convenience function for setting property values in the default frame alist.
(defun zen-set-default-frame-alist-prop (prop value)
  "Sets a property in the default-frame-alist AList."
  (progn
    (setq default-frame-alist (assq-delete-all prop default-frame-alist))
    (add-to-list 'default-frame-alist (cons prop value))))

(zen-set-default-frame-alist-prop 'background-color zenburn-bg)
(zen-set-default-frame-alist-prop 'foreground-color zenburn-fg)
(zen-set-default-frame-alist-prop 'cursor-color zenburn-fg)

(custom-theme-set-variables
 'zenburn
 '(frame-background-mode (quote dark))
 )

(custom-theme-set-faces
 'zenburn
 `(default ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))
 `(link ((t (:foreground ,zenburn-blue :underline t))))
 `(link-visited ((t (:foreground ,zenburn-magenta :underline t))))
 `(escape-glyph ((t (:foreground ,zenburn-blue))))
 `(nobreak-space ((t (:foreground ,zenburn-blue))))

 `(font-lock-builtin-face
   ((t (:foreground ,zenburn-blue))))
 `(font-lock-comment-face
   ((t (:foreground ,zenburn-green))))
 `(font-lock-comment-delimiter-face
   ((t (:inherit zenburn-lowlight-2))))
 `(font-lock-constant-face
   ((t (:inherit zenburn-primary-4))))
 `(font-lock-doc-face
   ((t (:foreground ,zenburn-green+1))))
 `(font-lock-function-name-face
   ((t (:foreground ,zenburn-yellow))))
 `(font-lock-keyword-face
   ((t (:inherit zenburn-primary-1))))
 `(font-lock-negation-char-face
   ((t (:inherit zenburn-primary-1))))
 `(font-lock-preprocessor-face
   ((t (:foreground ,zenburn-blue))))
 `(font-lock-string-face
   ((t (:foreground ,zenburn-red))))
 `(font-lock-type-face
   ((t (:inherit zenburn-primary-3))))
 `(font-lock-variable-name-face
   ((t (:foreground ,zenburn-yellow))))
 `(font-lock-warning-face
   ((t (:inherit zenburn-highlight-alerting))))

 `(fringe
   ((t (:inherit zenburn-highlight-subtle))))
 '(mode-line
   ((t (:foreground "#acbc90" :background "#1e2320"
                    :box (:color "#1e2320" :line-width 2)))))
 '(mode-line-inactive
   ((t (:background "#2e3330" :foreground "#88b090"
                    :box (:color "#2e3330" :line-width 2)))))
 '(region ((t (:foreground "#71d3b4" :background "#233323"))))
 `(minibuffer-prompt ((t (:foreground ,zenburn-yellow))))
 `(Buffer-menu-buffer ((t (:inherit zenburn-primary-1))))

 `(secondary-selection ((t (:foreground ,zenburn-fg :background "#506070"))))

 '(trailing-whitespace ((t (:inherit font-lock-warning-face))))
 '(highlight ((t (:inherit font-lock-warning-face))))
 '(paren ((t (:inherit zenburn-lowlight-1))))
 '(show-paren-mismatch ((t (:inherit font-lock-warning-face))))
 '(show-paren-match ((t (:inherit font-lock-keyword-face))))
 '(match ((t (:weight bold))))

 `(cursor ((t (:background ,zenburn-fg :foreground ,zenburn-bg))))
 '(hover-highlight ((t (:underline t :foreground "#f8f893"))))
 '(menu ((t nil)))
 '(mouse ((t (:inherit zenburn-foreground))))
 `(scroll-bar ((t (:background ,zenburn-bg+2))))
 `(tool-bar ((t (:background ,zenburn-bg+2))))

 '(ido-first-match ((t (:inherit zenburn-primary-1))))
 '(ido-only-match ((t (:inherit zenburn-primary-2))))
 `(ido-subdir ((t (:foreground ,zenburn-yellow))))

 `(isearch ((t (:foreground ,zenburn-fg :background "#506070"))))
 `(isearch-lazy-highlight
   ((t (:foreground ,zenburn-fg :background "#1e2320" :weight normal))))

 '(comint-highlight-input ((t (:inherit zenburn-primary-1))))
 '(comint-highlight-prompt ((t (:inherit zenburn-primary-2))))

 '(compilation-info ((t (:inherit zenburn-primary-1))))
 '(compilation-warning ((t (:inherit font-lock-warning-face))))

 '(custom-button
   ((t (:inherit fancy-widget-button))))
 '(custom-button-pressed
   ((t (:inherit fancy-widget-button-pressed))))
 `(custom-changed
   ((t (:foreground ,zenburn-blue))))
 '(custom-comment
   ((t (:inherit font-lock-doc))))
 '(custom-comment-tag
   ((t (:inherit font-lock-doc))))
 '(custom-documentation
   ((t (:inherit font-lock-doc))))
 `(custom-link
   ((t (:foreground ,zenburn-yellow :underline t))))
 '(custom-tag
   ((t (:inherit zenburn-primary-2))))
 '(custom-group-tag
   ((t (:inherit zenburn-primary-1))))
 '(custom-group-tag-1
   ((t (:inherit zenburn-primary-4))))
 '(custom-invalid
   ((t (:inherit font-lock-warning))))
 '(custom-modified
   ((t (:inherit zenburn-primary-3))))
 '(custom-rogue
   ((t (:inhrit font-lock-warning))))
 '(custom-saved
   ((t (:underline t))))
 `(custom-set
   ((t (:inverse-video t :foreground ,zenburn-blue))))
 '(custom-state
   ((t (:inherit font-lock-comment))))
 '(custom-variable-button
   ((t (:weight bold :underline t))))
 '(custom-variable-tag
   ((t (:inherit zenburn-primary-2))))

 `(info-xref ((t (:foreground ,zenburn-yellow :weight bold))))
 '(info-xref-visited ((t (:inherit info-xref ))))
 '(info-header-xref ((t (:inherit info-xref))))
 `(info-menu-star ((t (:foreground ,zenburn-orange :weight bold))))
 `(info-menu-5 ((t (:inherit info-menu-star))))
 '(info-node ((t (:weight bold))))

 `(org-agenda-date-today
   ((t (:foreground ,zenburn-fg :slant italic :weight bold))) t)
 `(org-agenda-structure ((t (:foreground ,zenburn-green))))
 `(org-archived ((t (:foreground "#8f8f8f"))))
 `(org-checkbox ((t (:background ,zenburn-bg :foreground ,zenburn-fg
                                 :box (:line-width 1 :style released-button)))))
 `(org-date ((t (:foreground ,zenburn-blue :underline t))))
 `(org-deadline-announce ((t (:foreground ,zenburn-red-1))))
 `(org-done ((t (:bold t :weight bold :foreground ,zenburn-green+3))))
 `(org-formula ((t (:foreground ,zenburn-yellow-2))))
 `(org-headline-done ((t (:foreground ,zenburn-green+3))))
 `(org-hide ((t (:foreground "#282828"))))
 `(org-level-1 ((t (:foreground ,zenburn-orange))))
 `(org-level-2 ((t (:foreground ,zenburn-yellow))))
 `(org-level-3 ((t (:foreground ,zenburn-blue))))
 `(org-level-4 ((t (:foreground ,zenburn-orange))))
 `(org-level-5 ((t (:foreground ,zenburn-yellow))))
 `(org-level-6 ((t (:foreground ,zenburn-blue))))
 `(org-level-7 ((t (:foreground ,zenburn-orange))))
 `(org-level-8 ((t (:foreground ,zenburn-yellow))))
 `(org-link ((t (:foreground ,zenburn-yellow-2 :underline t))))

 `(org-scheduled ((t (:foreground ,zenburn-green+4))))
 `(org-scheduled-previously ((t (:foreground ,zenburn-red-4))))
 `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
 `(org-special-keyword ((t (:foreground ,zenburn-yellow-1))))
 `(org-table ((t (:foreground ,zenburn-green+2))))
 `(org-tag ((t (:bold t :weight bold))))
 `(org-time-grid ((t (:foreground ,zenburn-orange))))
 `(org-todo ((t (:bold t :foreground ,zenburn-red :weight bold))))
 `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))
 `(org-warning ((t (:bold t :foreground ,zenburn-red :weight bold))))

 `(mumamo-background-chunk-major ((t (:background ,zenburn-bg))))
 `(mumamo-background-chunk-submode1 ((t (:background ,zenburn-bg+1))))
 `(mumamo-background-chunk-submode2
   ((t (:inherit mumamo-background-chunk-submode1))))
 `(mumamo-background-chunk-submode3
   ((t (:inherit mumamo-background-chunk-submode2))))
 `(mumamo-background-chunk-submode4
   ((t (:inherit mumamo-background-chunk-submode3))))

 `(flyspell-duplicate ((t (:underline ,zenburn-yellow))))
 `(flyspell-incorrect ((t (:underline ,zenburn-red))))

 `(py-pseudo-keyword-face ((t (:foreground ,zenburn-orange :weight bold))))
 `(html-helper-tag-face ((t (:foreground ,zenburn-yellow :weight bold))))

 `(hl-line ((t (:background ,zenburn-bg-1))))

 `(magit-section-title ((t (:inherit zenburn-primary-1))))
 `(magit-branch ((t (:inherit zenburn-primary-2))))

 `(elscreen-tab-current-screen ((t (:inherit zenburn-primary-1))))
 `(elscreen-tab-other-screen ((t (:foreground ,zenburn-yellow
                                              :background ,zenburn-green))))

 `(wl-highlight-message-headers ((t (:foreground ,zenburn-red+1))))
 `(wl-highlight-message-header-contents ((t (:foreground ,zenburn-green))))
 `(wl-highlight-message-important-header-contents
   ((t (:foreground ,zenburn-yellow))))
 `(wl-highlight-message-important-header-contents2
   ((t (:foreground ,zenburn-blue))))
 `(wl-highlight-message-unimportant-header-contents
   ((t (:inherit zenburn-term-dark-gray))))   ;; reuse term
 `(wl-highlight-message-citation-header ((t (:foreground ,zenburn-red))))

 `(wl-highlight-message-cited-text-1 ((t (:foreground ,zenburn-green))))
 `(wl-highlight-message-cited-text-2 ((t (:foreground ,zenburn-blue))))
 '(wl-highlight-message-cited-text-3 ((t (:foreground "#8f8f8f"))))
 `(wl-highlight-message-cited-text-4 ((t (:foreground ,zenburn-green))))

 `(wl-highlight-message-signature ((t (:foreground ,zenburn-yellow))))

 `(wl-highlight-summary-answered ((t (:foreground ,zenburn-fg))))
 '(wl-highlight-summary-new ((t (:foreground "#e89393"))))

 `(wl-highlight-summary-displaying ((t (:underline t
                                                   :foreground ,zenburn-yellow-2))))

 '(wl-highlight-thread-indent ((t (:foreground "#ecbcec"))))
 '(wl-highlight-summary-thread-top ((t (:foreground "#efdcbc"))))

 `(wl-highlight-summary-normal ((t (:foreground ,zenburn-fg))))

 `(wl-highlight-folder-zero ((t (:foreground ,zenburn-fg))))
 `(wl-highlight-folder-few ((t (:foreground ,zenburn-red+1))))
 `(wl-highlight-folder-many ((t (:foreground ,zenburn-red+1))))
 '(wl-highlight-folder-unread ((t (:foreground "#e89393"))))

 `(wl-highlight-folder-path ((t (:foreground ,zenburn-orange))))

 `(rpm-spec-dir ((t (:foreground ,zenburn-green))))
 `(rpm-spec-doc ((t (:foreground ,zenburn-green))))
 `(rpm-spec-ghost ((t (:foreground ,zenburn-red))))
 `(rpm-spec-macro ((t (:foreground ,zenburn-yellow))))
 `(rpm-spec-obsolete-tag ((t (:foreground ,zenburn-red))))
 `(rpm-spec-package ((t (:foreground ,zenburn-red))))
 `(rpm-spec-section ((t (:foreground ,zenburn-yellow))))
 `(rpm-spec-tag ((t (:foreground ,zenburn-blue))))
 `(rpm-spec-var ((t (:foreground ,zenburn-red))))

 `(mew-face-header-subject ((t (:foreground ,zenburn-orange))))
 `(mew-face-header-from ((t (:foreground ,zenburn-yellow))))
 `(mew-face-header-date ((t (:foreground ,zenburn-green))))
 `(mew-face-header-to ((t (:foreground ,zenburn-red))))
 `(mew-face-header-key ((t (:foreground ,zenburn-green))))
 `(mew-face-header-private ((t (:foreground ,zenburn-green))))
 `(mew-face-header-important ((t (:foreground ,zenburn-blue))))
 `(mew-face-header-marginal ((t (:inherit zenburn-term-dark-gray))))
 `(mew-face-header-warning ((t (:foreground ,zenburn-red))))
 `(mew-face-header-xmew ((t (:foreground ,zenburn-green))))
 `(mew-face-header-xmew-bad ((t (:foreground ,zenburn-red))))
 `(mew-face-body-url ((t (:foreground ,zenburn-orange))))
 `(mew-face-body-comment ((t (:inherit zenburn-term-dark-gray))))
 `(mew-face-body-cite1 ((t (:foreground ,zenburn-green))))
 `(mew-face-body-cite2 ((t (:foreground ,zenburn-blue))))
 `(mew-face-body-cite3 ((t (:foreground ,zenburn-orange))))
 `(mew-face-body-cite4 ((t (:foreground ,zenburn-yellow))))
 `(mew-face-body-cite5 ((t (:foreground ,zenburn-red))))
 `(mew-face-mark-review ((t (:foreground ,zenburn-blue))))
 `(mew-face-mark-escape ((t (:foreground ,zenburn-green))))
 `(mew-face-mark-delete ((t (:foreground ,zenburn-red))))
 `(mew-face-mark-unlink ((t (:foreground ,zenburn-yellow))))
 `(mew-face-mark-refile ((t (:foreground ,zenburn-green))))
 `(mew-face-mark-unread ((t (:foreground ,zenburn-red-2))))
 `(mew-face-eof-message ((t (:foreground ,zenburn-green))))
 `(mew-face-eof-part ((t (:foreground ,zenburn-yellow))))

 `(diff-added ((t (:foreground ,zenburn-green+1))))
 `(diff-indicator-added ((t (:inherit diff-added))))
 `(diff-removed ((t (:foreground ,zenburn-red+1))))
 `(diff-indicator-removed ((t (:inherit diff-removed))))
 `(diff-changed ((t (:foreground ,zenburn-yellow))))
 `(diff-refine-change ((t (:background "grey35" :bold t))))
 `(diff-context ((t (:inherit default))))

 `(flymake-errline ((t (:underline ,zenburn-red-2))))
 `(flymake-warnline ((t (:underline ,zenburn-blue))))


 `(js2-warning-face ((t (:underline ,zenburn-orange))))
 `(js2-error-face ((t (:underline ,zenburn-red))))
 `(js2-jsdoc-tag-face ((t (:foreground ,zenburn-blue-4))))
 `(js2-jsdoc-type-face ((t (:foreground ,zenburn-blue+1))))
 `(js2-jsdoc-value-face ((t (:foreground ,zenburn-yellow-2))))
 `(js2-function-param-face ((t (:foreground ,zenburn-green+2))))
 `(js2-instance-member-face ((t (:foreground ,zenburn-magenta))))
 `(js2-private-member-face ((t (:foreground ,zenburn-yellow-2))))
 `(js2-private-function-call-face ((t (:foreground ,zenburn-orange))))
 `(js2-jsdoc-html-tag-name-face ((t (t (:foreground ,zenburn-yellow+1)))))
 `(js2-jsdoc-html-tag-delimiter-face ((t (:foreground ,zenburn-green+4))))
 `(js2-magic-paren-face ((t (:underline t))))
 `(js2-external-variable-face ((t (:foreground ,zenburn-orange))))
)

(provide-theme 'zenburn)
