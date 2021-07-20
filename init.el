;;; init.el --- Emacs main configuration file -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Emacs Config
;;;
;;; Code:

;; Loading eraly-init.el if Emacs version < 27
(unless (featurep 'early-init)
  (load (expand-file-name "early-init" user-emacs-directory)))

;; At this point use-package should be installed
(require 'use-package)

;; General Settings
;; ------------------------------------

;; Allow async compilation of packages
(use-package async
  :init
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  :custom (async-bytecomp-allowed-packages '(all)))

;; Disable bells
(setq ring-bell-function 'ignore)

;; Set transparent background
(defvar my/frame-transparency '(95 . 95))
(set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;; Decrease echo keystrokes
(add-hook 'after-init-hook (lambda () (setq echo-keystrokes 5)))

;; Save history between sessions
(use-package savehist
  :straight nil
  :config (savehist-mode 1))

;; Scratch is clean and for normal text
(use-package startup
  :no-require t
  :straight nil
  :custom
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message "")
  (inhibit-splash-screen t))

;; No line wrap be default
(setq-default truncate-lines t)

;; Dont use a bar - use Ctrl + mouse-r instead
(use-package emacs
  :straight nil
  :config
  (menu-bar-mode -1)
  (blink-cursor-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  ;; UTF-8 if possible
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  ;; Don't center curser at off screen
  (setq scroll-conservatively 101)
  ;; Create a margin for off screen
  (setq scroll-margin 5)
  ;; Auto refresh changed buffers
  (global-auto-revert-mode t))

(use-package tramp
  :straight nil
  :config
  ;; Set default connection mode to SSH
  (setq tramp-default-method "ssh"))

;; ------------------------------------


;; EVIL - Load it as fast as possible
;; ------------------------------------
(progn
  ;; Better undo for evil
  (use-package undo-fu)
  (use-package undo-fu-session
    :after undo-fu
    :init
    (global-undo-fu-session-mode))

  ;; Basic Evil mode
  (use-package evil
    :bind (:map evil-window-map
	   ("<left>" . evil-window-left)
	   ("<right>" . evil-window-right)
	   ("<up>" . evil-window-up)
	   ("<down>" . evil-window-down))
    :after undo-fu-session
    :init
    (setq evil-want-integration t
	  evil-want-keybinding nil
	  evil-want-Y-yank-to-eol t
	  evil-vsplit-window-right t
	  evil-split-window-below t
	  evil-want-C-i-jump nil
	  evil-undo-system 'undo-fu)
    :config
    (evil-mode 1)
    (evil-set-leader 'normal " "))

  ;; Collection of bindings Evil does not cover
  (use-package evil-collection
    :after evil
    :config
    (evil-collection-init))

  ;; Move quickly in the document
  (use-package evil-easymotion
    :after evil
    :commands (evilem-motion-next-line evilem-motion-previous-line))

  ;; Comment code efficiently
  (use-package evil-nerd-commenter
    :after evil
    :commands (evilnc-comment-or-uncomment-lines))

  ;; Terminal cursor mode support
  ;; more readable :)
  (unless (display-graphic-p)
    (use-package evil-terminal-cursor-changer
      :config
      (evil-terminal-cursor-changer-activate)))

  ;; Vim like surround package
  (use-package evil-surround
    :after evil
    :config
    (global-evil-surround-mode))

  ;; View Registers and marks
  (use-package evil-owl
    :after evil
    :custom
    (evil-owl-display-method 'posframe)
    (evil-owl-extra-posfram-args '(:width 50 :height 20))
    (evil-owl-idle-delay 0)
    :config
    (evil-owl-mode))

  ;; Increment / Decrement binary, octal, decimal and hex literals
  (use-package evil-numbers
    :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt))

  ;; Jump between opening/closing tags using %
  (use-package evil-matchit
    :after evil))

;; ------------------------------------

;; Theme
;; ------------------------------------
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-palenight t)
  (doom-themes-visual-bell-config))

(use-package doom-modeline
  :config
  (doom-modeline-mode)
  (when (display-graphic-p)
    (setq doom-modeline-icon t)))

;; Set color backgrounds to color names
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; Rainbow colors for brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; ------------------------------------

;; Functions
;; ------------------------------------
(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun my/indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (indent-region (point-min) (point-max)))))

(defun sudo-save ()
  "save this file as super user"
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; describe this point lisp only
(defun describe-thing-at-point ()
  "Show the documentation of the Elisp function and variable near point.
This checks in turn:
-- for a function name where point is
-- for a variable name where point is
-- for a surrounding function call
"
  (interactive)
  (let (sym)
    ;; sigh, function-at-point is too clever.  we want only the first half.
    (cond ((setq sym (ignore-errors
		       (with-syntax-table emacs-lisp-mode-syntax-table
			 (save-excursion
			   (or (not (zerop (skip-syntax-backward "_w")))
			       (eq (char-syntax (char-after (point))) ?w)
			       (eq (char-syntax (char-after (point))) ?_)
			       (forward-sexp -1))
			   (skip-chars-forward "`'")
			   (let ((obj (read (current-buffer))))
			     (and (symbolp obj) (fboundp obj) obj))))))
	   (describe-function sym))
	  ((setq sym (variable-at-point)) (describe-variable sym))
	  ;; now let it operate fully -- i.e. also check the
	  ;; surrounding sexp for a function call.
	  ((setq sym (function-at-point)) (describe-function sym)))))

(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.
   The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
	(end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
	(save-restriction
	  (narrow-to-region (point-min) (1- begin))
	  (delete-trailing-whitespace)
	  (widen)))
      (when (> (point-max) (+ end 2))
	(save-restriction
	  (narrow-to-region (+ end 2) (point-max))
	  (delete-trailing-whitespace)
	  (widen))))))

(defun smart-delete-trailing-whitespace ()
  "Invoke `delete-trailing-whitespace-except-current-line' on selected major modes only."
  (unless (member major-mode '(diff-mode))
    (delete-trailing-whitespace-except-current-line)))

;; Auto-indent after paste yanked
(defadvice insert-for-yank-1 (after indent-region activate)
  "Indent yanked region in certain modes, C-u prefix to disable"
  (if (and (not current-prefix-arg)
	   (member major-mode '(sh-mode
				emacs-lisp-mode lisp-mode
				c-mode c++-mode objc-mode d-mode java-mode cuda-mode js-mode
				LaTeX-mode TeX-mode
				xml-mode html-mode css-mode)))
      (indent-region (region-beginning) (region-end) nil)))

(add-hook 'before-save-hook #'smart-delete-trailing-whitespace)
;; ------------------------------------

;; Fonts
;; ------------------------------------
;; Set fonts if possible
(cond ((my/font-installed-p "Cascadia Code")
       (set-face-attribute 'default nil :font "Cascadia Code 10"))
      ((my/font-installed-p "JetBrainsMono")
       (set-face-attribute 'default nil :font "JetBrainsMono 10"))
      ((my/font-installed-p "Hack")
       (set-face-attribute 'default nil :font "Hack 10")))

(use-package all-the-icons
  :config
  (when (and (not (my/font-installed-p "all-the-icons"))
	     (window-system))
    (all-the-icons-install-fonts t)))

;; Icons for dired
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
;; ------------------------------------

;; Line numbers
(use-package display-line-numbers
  :straight nil
  ;; :when aorst-enable-line-numbers
  :hook (prog-mode . display-line-numbers-mode)
  :custom
  (display-line-numbers-width 4)
  (display-line-numbers-width-start t)
  (add-hook 'display-line-numbers-mode-hook
	    (lambda () (setq display-line-numbers-type 'relative))))

;; Undo/Redo with C-c left/right
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; Don't Lock Files
(setq-default create-lockfiles nil)

;; Normal mouse scrolling
(setq mouse-wheel-progressive-speed nil)

;; User profile
(setq user-full-name "David Delarosa"
      user-mail-address "xdavidel@gmail.com")

;; Move backups to emacs folder
(setq backup-directory-alist `(("." . ,(expand-file-name "tmp/backups/" user-emacs-directory))))

;; Auto Save Files
;; create directory for auto-save-mode
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name "tmp/auto-saves/sessions/" user-emacs-directory)
      auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

;; No Lock Files
(setq create-lockfiles nil)

;; Benchmark
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Show matching parens
(use-package paren
  :straight nil
  :config (show-paren-mode 1))

;; Recent files
(use-package recentf
  :straight nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
		     ".cache"
		     ".cask"
		     ".elfeed"
		     "bookmarks"
		     "cache"
		     "ido.*"
		     "persp-confs"
		     "recentf"
		     "undo-tree-hist"
		     "url"
		     "COMMIT_EDITMSG\\'")))

;; Auto focus help window
(use-package help
  :straight nil
  :commands (help)
  :custom (help-window-select t))

;; Lightweight syntax highlighting improvement for numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Lightweight syntax highlighting improvement for escape sequences (e.g. \n, \t).
(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; Lightweight completion framwork
(use-package vertico
  :init
  (vertico-mode)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a")))))

;; Fuzzy search
(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles . (partial-completion))))))

;; Rich completions
(use-package marginalia
  :after vertico
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

;; Menu completion
(use-package consult
  :after vertico
  :hook (completion-setup . hl-line-mode)
  :config
  (setq consult-preview-key (kbd "M-."))
  :custom
  (completion-in-region-function #'consult-completion-in-region))

;; Completion framwork for anything
(use-package company
  :bind
  (:map company-active-map
	 ("<down>"   . company-select-next)
	 ("<up>"   . company-select-previous)
	 ("TAB" . company-complete-common-or-cycle)
	 ("<tab>" . company-complete-common-or-cycle)
	 ("<S-Tab>" . company-select-previous)
	 ("<backtab>" . company-select-previous)
	 ("RET"   . company-complete-selection)
	 ("<ret>" . company-complete-selection))
  :hook (after-init . global-company-mode)
  :custom
  (company-require-match 'never)
  (company-minimum-prefix-length 2)
  (company-tooltip-align-annotations t)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
		       company-preview-frontend
		       company-echo-metadata-frontend))
  (company-backends '(company-capf company-files))
  (company-tooltip-minimum-width 30)
  (company-tooltip-maximum-width 60))

;; Allow submenu for company
(use-package company-posframe
  :after company
  :custom
  (company-posframe-quickhelp-show-header nil)
  (company-posframe-show-indicator nil)
  (company-posframe-show-metadata nil)
  (company-posframe-quickhelp-delay nil)
  (company-posframe-quickhelp-show-params
   (list :poshandler #'company-posframe-quickhelp-right-poshandler
	 :internal-border-width 1
	 :timeout 60
	 :internal-border-color (face-attribute 'mode-line :background)
	 :no-properties nil))
  (company-posframe-show-params
   (list :poshandler #'company-posframe-quickhelp-right-poshandler
	 :internal-border-width 1
	 :timeout 60
	 :internal-border-color (face-attribute 'mode-line :background)
	 :no-properties nil))
  :config
  (company-posframe-mode))


;; Buitin file manager
(use-package dired
  :straight nil
  :bind (:map dired-mode-map
	 ("-" . dired-up-directory)
	 ("<backspace>" . dired-up-directory))
  :custom ((dired-listing-switches "-aghoA --group-directories-first"))
  :config
  (setq dired-omit-files
	(rx (or (seq bol (? ".") "#")
		(seq bol "." eol)
		(seq bol ".." eol)))))


(use-package hydra)
:config
(defhydra hydra-expand-region ()
  "region: "
  ("k" er/expand-region "expand")
  ("j" er/contract-region "contract"))

(defhydra hydra-fold (:pre (hs-minor-mode 1))
  "fold"
  ("t" fold-dwim-toggle "toggle")
  ("h" fold-dwim-hide-all "hide-all")
  ("s" fold-dwim-show-all "show-all")
  ("q" nil "quit"))

(defun my/resize-window-down ()
  "Resize a window downwards."
  (interactive)
  (if (window-in-direction 'below)
      (enlarge-window 1)
    (shrink-window 1)))
(defun my/resize-window-up ()
  "Resize a window upwards."
  (interactive)
  (if (window-in-direction 'above)
      (enlarge-window 1)
    (shrink-window 1)))
(defun my/resize-window-left ()
  "Resize a window leftwards."
  (interactive)
  (if (window-in-direction 'left)
      (enlarge-window-horizontally 1)
    (shrink-window-horizontally 1)))
(defun my/resize-window-right ()
  "Resize a window rightwards."
  (interactive)
  (if (window-in-direction 'right)
      (enlarge-window-horizontally 1)
    (shrink-window-horizontally 1)))

(defhydra hydra-window-resize (global-map "C-c w")
  "Window resizing"
  ("j" my/resize-window-down "down")
  ("k" my/resize-window-up "up")
  ("l" my/resize-window-right "right")
  ("h" my/resize-window-left "left"))

(defhydra hydra-outline (:color pink :hint nil)

  "
 ^Hide^             ^Show^           ^Move
 ^^^^^^------------------------------------------------------
 _q_: sublevels     _a_: all         _u_: up
 _t_: body          _e_: entry       _n_: next visible
 _o_: other         _i_: children    _p_: previous visible
 _c_: entry         _k_: branches    _f_: forward same level
 _l_: leaves        _s_: subtree     _b_: backward same level
 _d_: subtree   _<tab>_: cycle
 "
  ;; Hide
  ("q" hide-sublevels)  ; Hide everything but the top-level headings
  ("t" hide-body)    ; Hide everything but headings (all body lines)
  ("o" hide-other)   ; Hide other branches
  ("c" hide-entry)   ; Hide this entry's body
  ("l" hide-leaves)  ; Hide body lines in this entry and sub-entries
  ("d" hide-subtree) ; Hide everything in this entry and sub-entries
  ;; Show
  ("a" show-all)                      ; Show (expand) everything
  ("e" show-entry)                    ; Show this heading's body
  ("i" show-children) ; Show this heading's immediate child sub-headings
  ("k" show-branches) ; Show all sub-headings under this heading
  ("s" show-subtree) ; Show (expand) everything in this heading & below
  ("<tab>" org-cycle)
  ;; Move
  ("u" outline-up-heading)               ; Up
  ("n" outline-next-visible-heading)     ; Next
  ("p" outline-previous-visible-heading) ; Previous
  ("f" outline-forward-same-level)       ; Forward - same level
  ("b" outline-backward-same-level)      ; Backward - same level
  ("z" nil "leave"))

(defhydra multiple-cursors-hydra (:hint nil)
  "
      ^Up^            ^Down^        ^Other^
 ----------------------------------------------
 [_p_]   Next    [_n_]   Next    [_l_] Edit lines
 [_P_]   Skip    [_N_]   Skip    [_a_] Mark all
 [_M-p_] Unmark  [_M-n_] Unmark  [_r_] Mark by regexp
 ^ ^             ^ ^             [_q_] Quit
 "
  ("l" mc/edit-lines :exit t)
  ("a" mc/mark-all-like-this :exit t)
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)
  ("r" mc/mark-all-in-region-regexp :exit t)
  ("q" nil))
(defhydra hydra-origami (:color red)
  "
  _o_pen node    _n_ext fold       toggle _f_orward    _t_oggle recursively
  _c_lose node   _p_revious fold   toggle _a_ll
  "
  ("o" origami-open-node)
  ("t" origami-recursively-toggle-node)
  ("c" origami-close-node)
  ("n" origami-next-fold)
  ("p" origami-previous-fold)
  ("f" origami-forward-toggle-node)
  ("a" origami-toggle-all-nodes))

(defhydra hydra-move-previous
  (:body-pre (previous-line))
  "move"
  ("n" next-line)
  ("p" previous-line)
  ("<tab>" org-cycle)
  ("q" nil))

(defhydra hydra-move-next
  (:body-pre (next-line))
  "move"
  ("n" next-line)
  ("p" previous-line)
  ("<tab>" org-cycle)
  ("q" nil))

;; Templates of code
(use-package yasnippet
  :config
  (add-to-list 'yas-key-syntaxes 'yas-shortest-key-until-whitespace))

;; Interface to Git
(use-package magit
  :hook ((git-commit-mode . flyspell-mode))
  :custom
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-ignore-whitespace nil)
  (magit-diff-refine-hunk 'all)
  (magit-blame-styles
   '((headings
      (heading-format . "%-20a %C %s\n"))
     (margin
      (margin-format " %s%f" " %C %a" " %H")
      (margin-width . 42)
      (margin-face . magit-blame-margin)
      (margin-body-face magit-blame-dimmed))
     (highlight
      (highlight-face . magit-blame-highlight))
     (lines
      (show-lines . nil)
      (show-message . t))))
  :config
  (advice-add 'magit-set-header-line-format :override #'ignore))

;; Show TODOs in magit
(use-package magit-todos
  :after magit
  :config
  (let ((inhibit-message t))
    (magit-todos-mode 1))
  (transient-append-suffix 'magit-status-jump '(0 0 -1)
    '("T " "Todos" magit-todos-jump-to-todos)))

;; Use magit with treemacs
(use-package treemacs-magit
  :defer t
  :after (treemacs magit))

;; Be smart when using parens, and highlight content
(use-package smartparens
  :hook ((java-mode python-mode go-mode
		    js-mode js2-mode typescript-mode web-mode
		    c-mode c++-mode objc-mode) . smartparens-mode)
  :config
  ;; highligh matching brackets
  (show-smartparens-global-mode 0)
  ;; so that paren highlights do not override region marking (aka selecting)
  (setq show-paren-priority -1)
  (setq show-paren-when-point-inside-paren t)
  (setq sp-show-pair-from-inside t)
  (setq show-paren-style 'mixed))

;; LSP client
(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-auto-guess-root nil)
  (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
  (lsp-keymap-prefix "C-c l")
  (lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  :hook ((java-mode python-mode go-mode
		    js-mode js2-mode typescript-mode web-mode
		    c-mode c++-mode objc-mode) . lsp))

;; Python support
(use-package lsp-python-ms
  :custom
  (lsp-python-ms-auto-install-server t)
  :hook (python-mode . lsp-deferred))

;; Ui for lsp
(use-package lsp-ui
  :after lsp-mode
  :diminish
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-header t)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-doc-border (face-foreground 'default))
  (lsp-ui-sideline-enable nil)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-ui-sideline-show-code-actions nil)
  :config
  ;; Use lsp-ui-doc-webkit only in GUI
  (if (display-graphic-p)
      (setq lsp-ui-doc-use-webkit t))
  ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
  ;; https://github.com/emacs-lsp/lsp-ui/issues/243
  (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
    (setq mode-line-format nil)))

;; Display references of a symbol, or diagnostic
(use-package lsp-treemacs
  :after lsp)

;; (use-package eglot
;;   :commands eglot
;;   :config
;;   (add-hook 'c-mode-common-hook 'eglot)
;;   (add-to-list 'eglot-server-programs
;; 	       '(c-mode . ("clangd"))))

;; Debugger
(use-package dap-mode
  :commands dap-debug
  :bind
  (:map dap-mode-map
   (("<f5>" . dap-debug)
    ("<f8>" . dap-continue)
    ("<f9>" . dap-next)
    ("<f11>" . dap-step-in)
    ("<f10>" . dap-step-out)
    ("<f2>" . dap-breakpoint-toggle))))


;; Org Stuff
;; ------------------------------------

;; Org Mode
(use-package org
  ;; :straight (:type built-in)
  :hook ((ediff-prepare-buffer . outline-show-all)
	 ((org-capture-mode org-src-mode) . my/discard-history))
  :commands (org-capture org-agenda)
  :custom
  (org-ellipsis " ▼")
  (org-startup-with-inline-images nil)
  (org-log-done 'time)
  (org-journal-date-format "%B %d, %Y (%A) ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-hide-emphasis-markers t)
  (org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
   '(("google" . "http://www.google.com/search?q=")
     ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
     ("ddg" . "https://duckduckgo.com/?q=")
     ("wiki" . "https://en.wikipedia.org/wiki/")))
  (org-todo-keywords
   '((sequence
      "TODO(t)"
      "BUG(b)"
      "WAIT(w)"
      "|"                ; The pipe necessary to separate "active" states and "inactive" states
      "DONE(d)"
      "CANCELLED(c)" )))
  (org-hide-leading-stars t)
  (org-directory (let ((dir (file-name-as-directory (expand-file-name "org" user-emacs-directory))))
		   (make-directory dir :parents)
		   dir))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-capture-templates
   `(("t" "Tasks / Projects")
     ("tt" "Task" entry (file+olp+datetree "tasks.org")
      "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

     ("j" "Journal Entries")
     ("jj" "Journal" entry (file+olp+datetree "journal.org")
      "\n* %<%I:%M %p> - Journal :journal:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)
     ("jm" "Meeting" entry
      (file+olp+datetree "meetings.org")
      "* %<%I:%M %p> - %a :meetings:\n\n%?\n\n"
      :clock-in :clock-resume
      :empty-lines 1)))
  :config
  ;; add plantuml to org sources
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (when (not (version<= org-version "9.1.9"))
    (use-package org-tempo
      :straight nil
      :config
      ;; add source templates
      (add-to-list 'org-structure-template-alist '("sh"  . "src shell"))
      (add-to-list 'org-structure-template-alist '("el"  . "src emacs-lisp"))
      (add-to-list 'org-structure-template-alist '("py"  . "src python"))
      (add-to-list 'org-structure-template-alist '("cpp"   . "src cpp"))
      (add-to-list 'org-structure-template-alist '("go"  . "src go"))
      ))

  ;; change header size on different levels
  (dolist (face '((org-level-1 . 1.2)
		  (org-level-2 . 1.1)
		  (org-level-3 . 1.05)
		  (org-level-4 . 1.0)
		  (org-level-5 . 1.1)
		  (org-level-6 . 1.1)
		  (org-level-7 . 1.1)
		  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :height (cdr face)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (defun my/discard-history ()
    "Discard undo history of org src and capture blocks."
    (setq buffer-undo-list nil)
    (set-buffer-modified-p nil))
  (defun my/org-babel-tangle-dont-ask ()
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))
  (defun my/org-babel-tangle-async (file)
    "Invoke `org-babel-tangle-file' asynchronously."
    (message "Tangling %s..." (buffer-file-name))
    (async-start
     (let ((args (list file)))
       `(lambda ()
	  (require 'org)
	  (let ((start-time (current-time)))
	    (apply #'org-babel-tangle-file ',args)
	    (format "%.2f" (float-time (time-since start-time))))))
     (let ((message-string (format "Tangling %S completed after " file)))
       `(lambda (tangle-time)
	  (message (concat ,message-string
			   (format "%s seconds" tangle-time)))))))

  (defun my/org-babel-tangle-current-buffer-async ()
    "Tangle current buffer asynchronously."
    (my/org-babel-tangle-async (buffer-file-name)))
  (add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-dont-ask))))

;; Show bullets in a nice way
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Org Margins
(use-package visual-fill-column
  :after org
  :config
  (defun my/org-mode-visual-fill()
    "Set some margins in org documents"
    (setq visual-fill-column-width 100
	  visual-fill-column-center-text t)
    (visual-fill-column-mode 1))
  :hook (org-mode . my/org-mode-visual-fill))

;; Table of contents using `:toc:`
(use-package toc-org
  :hook
  (org-mode . toc-org-mode)
  (markdown-mode . toc-org-mode))


;; Programming Languages
;; ------------------------------------
;; C source files
(use-package prog-mode
  :straight nil
  :hook (prog-mode . hl-line-mode))

;; Python
(use-package pyvenv
  :mode ("\\.py\\'" . python-mode)
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

;; C / C++
(use-package cc-mode
  :straight nil
  :mode (("\\.c\\'" . c-mode)
	 ("\\.cpp\\'" . c-mode)
	 ("\\.h\\'" . c-mode)
	 ("\\.hpp\\'" . c-mode))
  :defines (lsp-clients-clangd-args)
  :config (defun my/cc-mode-setup ()
	    (c-set-offset 'case-label '+)
	    (setq c-basic-offset 4
		  c-default-style "linux"
		  indent-tabs-mode t
		  comment-start "//"
		  comment-end ""
		  tab-width 4))
  (with-eval-after-load 'lsp-mode
    (setq lsp-clients-clangd-args
	  '("-j=2"
	    "--background-index"
	    "--clang-tidy"
	    "--completion-style=bundled"
	    "--pch-storage=memory"
	    "--suggest-missing-includes")))
  :hook ((c-mode-common . my/cc-mode-setup)))

;; Format C code with Clang Format
(use-package clang-format
  :if (executable-find "clang")
  :after cc-mode
  :bind (:map c-mode-base-map
	 ("C-c C-M-f" . clang-format-buffer)))

;; (use-package
;;   irony
;;   :hook (c-mode . irony-mode))

;; Rust syntax highlighting
(use-package rust-mode
  :commands (rust-format-buffer)
  :bind (:map rust-mode-map
	 ("C-c C-M-f" . rust-format-buffer)))

;; Rust - Use racer when RLS in not available (ex. Org mode)
(use-package racer
  :if (executable-find "racer")
  :hook (racer-mode . eldoc-mode)
  :init (defun org-babel-edit-prep:rust (&optional _babel-info)
	  "Run racer mode for Org Babel."
	  (racer-mode 1)))

;; Rust - Cargo integration
(use-package cargo
  :if (executable-find "cargo")
  :hook ((rust-mode toml-mode) . cargo-minor-mode))

;; Syntax highlighting of TOML files
(use-package toml-mode
  :mode ("\\.toml\\'" . toml-mode))

;; Lisp and ELisp mode
(use-package elisp-mode
  :straight nil
  :commands (my/emacs-lisp-indent-function)
  :hook ((emacs-lisp-mode . eldoc-mode)
	 (emacs-lisp-mode . (lambda ()
			      (setq-local lisp-indent-function
					  #'my/emacs-lisp-indent-function))))
  :config
  (defun my/emacs-lisp-indent-function (indent-point state)
    "A replacement for `lisp-indent-function'.
Indents plists more sensibly. Adapted from DOOM Emacs:
https://github.com/hlissner/doom-emacs/commit/a634e2c8125ed692bb76b2105625fe902b637998"
    (let ((normal-indent (current-column))
	  (orig-point (point)))
      (goto-char (1+ (elt state 1)))
      (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
      (cond ((and (elt state 2)
		  (or (not (looking-at-p "\\sw\\|\\s_"))
		      (eq (char-after) ?:)))
	     (unless (> (save-excursion (forward-line 1) (point))
			calculate-lisp-indent-last-sexp)
	       (goto-char calculate-lisp-indent-last-sexp)
	       (beginning-of-line)
	       (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t))
	     (backward-prefix-chars)
	     (current-column))
	    ((and (save-excursion
		    (goto-char indent-point)
		    (skip-syntax-forward " ")
		    (not (eq (char-after) ?:)))
		  (save-excursion
		    (goto-char orig-point)
		    (eq (char-after) ?:)))
	     (save-excursion
	       (goto-char (+ 2 (elt state 1)))
	       (current-column)))
	    ((let* ((function (buffer-substring (point) (progn (forward-sexp 1) (point))))
		    (method (or (function-get (intern-soft function) 'lisp-indent-function)
				(get (intern-soft function) 'lisp-indent-hook))))
	       (cond ((or (eq method 'defun)
			  (and (null method)
			       (> (length function) 3)
			       (string-match-p "\\`def" function)))
		      (lisp-indent-defform state indent-point))
		     ((integerp method)
		      (lisp-indent-specform method state
					    indent-point normal-indent))
		     (method
		      (funcall method indent-point state))))))))
  (defun org-babel-edit-prep:emacs-lisp (&optional _babel-info)
    "Setup Emacs Lisp buffer for Org Babel."
    (setq lexical-binding t)
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))))

;; Yaml support
(use-package yaml-mode
  :mode ("\\.yaml\\'" . yaml-mode)
  :custom (yaml-indent-offset 4))

;; Yaml linter with flycheck
(use-package flycheck-yamllint
  :when (executable-find "yamllint")
  :hook ((yaml-mode . flycheck-yamllint-setup)
	 (yaml-mode . flycheck-mode)))

;; Shell scripting
(use-package sh-script
  :straight nil
  :hook (sh-mode . flycheck-mode))

;; Lua mode
(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))

;; CSS mode
(use-package css-mode
  :straight nil
  :mode ("\\.css\\'" . css-mode)
  :custom
  (css-indent-offset 2))

;; JSON mode
(use-package json-mode
  :hook (json-mode . flycheck-mode)
  :custom (js-indent-level 2))

;; Plantuml mode
(use-package plantuml-mode
  :mode (("\\.plantuml\\'" . plantuml-mode)
	 ("\\.pu\\'" . plantuml-mode))
  :config
  (add-to-list 'auto-mode-alist
	       '("\\.plantuml\\'" . plantuml-mode)))
;; ------------------------------------

;; Containers
;; ------------------------------------

;; Kubernetes
(use-package kubel-evil
  :commands kubel)

;; Docker
(use-package docker
  :commands docker)

;; ------------------------------------


;; Tools
;; ------------------------------------

(use-package disk-usage
  :commands (disk-usage))

;; Nerdtree like side bar
(use-package treemacs-evil)

;; RSS feed
(use-package elfeed
  :commands (elfeed)
  :custom
  (elfeed-feeds '(
		  ;;dev.to
		  "http://dev.to/feed"

		  ;;reddit
		  "http://reddit.com/r/cpp/.rss"
		  "http://reddit.com/r/emacs/.rss"
		  "http://reddit.com/r/golang/.rss"
		  "http://reddit.com/r/rust/.rss"
		  "http://reddit.com/r/bindingofisaac/.rss"

		  ;;hackernews
		  "https://news.ycombinator.com/rss")))

;; ------------------------------------


(use-package projectile
  :diminish projectile-mode
  :config
  (projectile-mode +1)
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'alien))

;; Display keys in a menu
(use-package which-key
  :config
  (which-key-mode t))

;; Search engines
(use-package engine-mode
  :straight (:branch "main")
  :hook (prog-mode . engine-mode)
  :config
  (defengine archwiki
    "https://wiki.archlinux.org/index.php?title=Special:Search&search=%s")
  (defengine cppreference
    "https://en.cppreference.com/mwiki/index.php?search=%s")
  (defengine cmake
    "https://cmake.org/cmake/help/latest/search.html?q=%s&check_keywords=yes&area=default")
  (defengine google
    "https://google.com/search?q=%s")
  (defengine youtube
    "https://www.youtube.com/results?search_query=%s")
  (defengine dockerhub
    "https://hub.docker.com/search?q=%s&type=image")
  (defengine github
    "https://github.com/search?q=%s")
  (defengine rustdoc
    "https://doc.rust-lang.org/rustdoc/what-is-rustdoc.html?search=%s")
  (defengine wikipedia
    "https://en.wikipedia.org/wiki/%s"))

;; Keybindings
;; ------------------------------------
;; Using general for key describtions

;; Use escape to close
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(use-package general
  :config
  (general-create-definer my/leader-keys
    :prefix "SPC")

  (my/leader-keys
    :states 'normal
    :keymaps 'override

    ;; Evilmotion
    "<SPC>"  '(:ignore t :which-key "Evilmotion")
    "<SPC>j" '(evilem-motion-next-line :which-key "Sneak down")
    "<SPC>k" '(evilem-motion-previous-line :which-key "Sneak up")

    ;; Apps
    "a"  '(:ignore t :which-key "Apps")
    "gd" '(docker :which-key "Docker")
    "gc" '(docker-compose :which-key "Docker compose")
    "gk" '(kubel :which-key "Kubernetes")
    "an" '(elfeed :which-key "Feeds")

    ;; Buffers & windows
    "b"  '(:ignore t :which-key "Buffer")
    "bs" '(switch-to-buffer :which-key "Switch buffer")
    "bi" '(my/indent-buffer :which-key "Indent buffer")
    "be" '(ediff-buffers :which-key "Difference")

    ;; Files
    "f"  '(:ignore t :which-key "Files")
    "fb" '(treemacs :which-key "File browser")
    "fd" '(dired-jump :which-key "Dired")
    "ff" '(find-file :which-key "Find file")
    "fj" '(find-journal :which-key "Journal")
    "fr" '(consult-recent-file :which-key "Recent files")

    ;; Git
    "g"  '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Magit")
    "gm" '(magit-blame-addition :which-key "Blame")

    ;; Org
    "o"  '(:ignore t :which-key "Org")
    "oa" '(org-agenda :which-key "Agenda")
    "oc" '(org-capture :which-key "Capture")
    "ot" '(org-todo-list :which-key "Todo")

    ;; Quiting
    "q"  '(:ignore t :which-key "Quit")
    "qq" '(kill-buffer-and-window :which-key "Quit now")

    ;; Resize buffers
    "r"  '(:ignore t :which-key "Resize")
    "rh" '(hydra-window-resize/my/resize-window-left :which-key "Left")
    "rj" '(hydra-window-resize/my/resize-window-down :which-key "Down")
    "rk" '(hydra-window-resize/my/resize-window-up :which-key "Up")
    "rl" '(hydra-window-resize/my/resize-window-right :which-key "Right")

    ;;engine
    "s"  '(:ignore t :which-key "Search")
    "sa" '(engine/search-archwiki :which-key "Archwiki")
    "sc" '(engine/search-cppreference :which-key "Cpp")
    "sb" '(engine/search-cmake :which-key "Cmake")
    "sy" '(engine/search-youtube :which-key "Youtube")
    "sd" '(engine/search-dockerhub :which-key "Dockerhub")
    "sr" '(engine/search-rustdoc :which-key "Rustdocs")
    "sw" '(engine/search-wikipedia :which-key "Wikipedia")
    "sg" '(engine/search-google :which-key "Google")
    "sG" '(engine/search-github :which-key "Github")

    "w"  '(:ignore t :which-key "Windows")
    "ww" '(tear-off-window :which-key "Tear off")
    "wh" '(windmove-swap-states-left :which-key "Swap left")
    "wj" '(windmove-swap-states-down :which-key "Swap down")
    "wk" '(windmove-swap-states-up :which-key "Swap up")
    "wl" '(windmove-swap-states-right :which-key "Swap right"))
  )

;; Mode Keybindings
(general-define-key
 :states 'normal
 :keymaps 'emacs-lisp-mode-map
 :prefix "SPC"
 :global-prefix "C-SPC"
 "k" '(eval-buffer :which-key "Eval-buffer"))

;; `general-def' can be used instead for `evil-define-key'-like syntax
(general-def nil global-map
  "C-c C-M-f" 'my/indent-buffer
  "C-c l" 'org-store-link
  "C-c a" 'org-todo-list
  "C-c c" 'org-capture)

(general-def 'normal prog-mode-map
  "K" 'lsp-describe-thing-at-point)

(general-def 'normal emacs-lisp-mode-map
  "K" 'describe-thing-at-point)

(general-def nil 'org-mode-map
  "M-H" 'org-shiftleft
  "M-J" 'org-shiftdown
  "M-K" 'org-shiftup
  "M-L" 'org-shiftright
  "M-h" 'org-metaleft
  "M-j" 'org-metadown
  "M-k" 'org-metaup
  "M-l" 'org-metaright)

(general-def 'normal 'compilation-mode-map
  "C-n" 'compilation-next-error
  "C-p" 'compilation-previous-error)

(general-def nil 'go-mode-map
  "C-c C-c" 'go-run)

(general-def '(normal insert) 'company-mode-map
  "C-SPC" 'company-complete)

(general-def 'normal 'global-map
  "Q" 'insert-output-of-executed-line
  "C-/" 'evilnc-comment-or-uncomment-lines
  "gcc" 'evilnc-comment-or-uncomment-lines)

(general-def 'visual 'global-map
  "S" 'evil-surround-region
  "gc" 'evilnc-comment-or-uncomment-lines)

(general-def 'normal 'lsp-mode-map
  "gr" 'lsp-find-references)

;; Increase / Decrease font
(general-define-key "C-=" 'text-scale-increase)
(general-define-key "C--" 'text-scale-decrease)
(general-define-key "<C-mouse-4>" 'text-scale-increase)
(general-define-key "<C-mouse-5>" 'text-scale-decrease)

;; Move between buffers
;; (general-define-key "C-h" 'evil-window-left)
;; (general-define-key "C-j" 'evil-window-down)
;; (general-define-key "C-k" 'evil-window-up)
;; (general-define-key "C-l" 'evil-window-right)

;; ------------------------------------

;; Enable server
(use-package server
  :init
  (progn
    (when (equal window-system 'w32)
      (setq server-use-tcp t)))
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init)
;;; init.el ends here
