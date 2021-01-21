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
  (async-bytecomp-package-mode 1))

;; Disable bells
(setq ring-bell-function 'ignore)

;; Set transparent background
(defvar my/frame-transparency '(95 . 80))
(set-frame-parameter (selected-frame) 'alpha my/frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,my/frame-transparency))

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Decrease echo keystrokes
(add-hook 'after-init-hook (lambda () (setq echo-keystrokes 5)))

;; Don't center curser at off screen
(setq scroll-conservatively 101)

;; Create a margin for off screen
(setq scroll-margin 5)

;; Auto refresh changed buffers
(global-auto-revert-mode t)

;; Save history between sessions
(use-package savehist
  :straight nil
  :config (savehist-mode 1))

;; UTF-8 if possible
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Scratch is clean and for normal text
(use-package startup
  :no-require t
  :straight nil
  :custom
  (initial-major-mode 'fundamental-mode)
  (initial-scratch-message ""))

;; No splash screen on empty emacs
(use-package startup
  :no-require t
  :straight nil
  :custom
  (inhibit-splash-screen t))
;; (setq inhibit-startup-screen t)

;; No line wrap be default
(setq-default truncate-lines t)

;; Dont use a bar - use Ctrl + mouse-r instead
(menu-bar-mode -1)

;; ------------------------------------


;; EVIL - Load it as fast as possible
;; ------------------------------------

;; Evil basic configs
(setq evil-want-integration t
      evil-want-keybinding nil
      evil-vsplit-window-right t
      evil-want-C-i-jump nil)

;; Use leader key for Evil
(use-package evil-leader
   :init
   (global-evil-leader-mode)
   (evil-leader/set-leader "<SPC>"))

;; Collection of bindings Evil does not cover
(use-package evil-collection
   :after evil-leader
   :custom
   (evil-collection-company-use-tng nil)
   :init
   (evil-collection-init))

;; Basic Evil mode
(use-package evil
   :after evil-collection
   :init
    (evil-mode 1))

;; Move quickly in the document
(use-package evil-easymotion
  :after evil)

;; Comment code efficiently
(use-package evil-nerd-commenter
  :after evil)

;; Terminal cursor mode support
(use-package evil-terminal-cursor-changer
  :init
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; |
  (setq evil-emacs-state-cursor  'hbar) ; _
  )

;; View matches info
(use-package evil-anzu)

;; Vim like surround package
(use-package evil-surround
   :config
   (global-evil-surround-mode))

;; View Registers and marks
(use-package evil-owl
   :custom
   (evil-owl-display-method 'posframe)
   (evil-owl-extra-posfram-args '(:width 50 :height 20))
   (evil-owl-idle-delay 0)
   :init
   (evil-owl-mode))

;; Increment / Decrement binary, octal, decimal and hex literals
(use-package evil-numbers)

;; Jump between opening/closing tags
(use-package evil-matchit)

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
  :init (rainbow-mode))

;; Rainbow colors for brackets
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
;; ------------------------------------

;; Functions
;; ------------------------------------
(defun my/real-buffer-p (&optional buffer)
  "Determines whether BUFFER is real."
  (not (or (string-match-p
            (regexp-opt '("*Treemacs"
                          "*vterm*"
                          " *Minibuf"
                          " *Echo Area"
                          "*Process List*"
                          "*Ediff"
                          " *LV*"
                          "*Ilist*"))
            (buffer-name buffer))
           (minibufferp))))

(defun my/font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun my/indent-buffer ()
  "Indent whole buffer."
  (interactive)
  (save-excursion
    (save-restriction
      (indent-region (point-min) (point-max)))))

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


(defun my/escape ()
  "Quit in current context.

When there is an active minibuffer and we are not inside it close
it.  When we are inside the minibuffer use the regular
`minibuffer-keyboard-quit' which quits any active region before
exiting.  When there is no minibuffer `keyboard-quit' unless we
are defining or executing a macro."
  (interactive)
  (cond ((active-minibuffer-window)
         (if (minibufferp)
             (minibuffer-keyboard-quit)
           (abort-recursive-edit)))
        ((bound-and-true-p iedit-mode)
         (iedit-quit))
        (t
         (unless (or defining-kbd-macro
                     executing-kbd-macro)
           (keyboard-quit)))))
(global-set-key [remap keyboard-quit] #'my/escape)

;; Remove useless whitespace before saving a file
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

;; Distinguish buffers that are for utilities
(use-package solaire-mode
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))

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

;; Minor mode that highlights source code uniquely
(use-package color-identifiers-mode
  :defer t)

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
  :custom (help-window-select t))

;; Lightweight syntax highlighting improvement for numbers
(use-package highlight-numbers
  :hook (prog-mode . highlight-numbers-mode))

;; Lightweight syntax highlighting improvement for escape sequences (e.g. \n, \t).
(use-package highlight-escape-sequences
  :hook (prog-mode . hes-mode))

;; Light narrowing framework
(use-package ivy
  :commands ivy-mode
  :hook ((minibuffer-setup . my/minibuffer-defer-garbage-collection)
         (minibuffer-exit . my/minibuffer-restore-garbage-collection))
  :bind (:map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :custom-face
  (ivy-org ((t (:inherit default))))
  :custom
  (ivy-ignore-buffers '("\\` " "\\`\\*"))
  (ivy-use-selectable-prompt t)
  (ivy-count-format "(%d/%d) ")
  (ivy-display-style 'fancy)
  (ivy-dynamic-exhibit-delay-ms 200)
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-extra-directories nil)
  :init
  (defun my/minibuffer-defer-garbage-collection ()
    "Defer garbage collection for minibuffer"
    (setq gc-cons-threshold most-positive-fixnum))
  (defun my/minibuffer-restore-garbage-collection ()
    "Resotre garbage collection settings."
    (run-at-time
     1 nil (lambda () (setq gc-cons-threshold my/gc-cons-threshold))))
  (ivy-mode 1))

;; Alternative & better menu
(use-package counsel
  :after ivy
  :commands (counsel-M-x
	counsel-find-file
	counsel-file-jump
	counsel-recentf
	counsel-rg
	counsel-describe-function
	counsel-describe-variable
	counsel-find-library)
  :config
  (use-package smex)
  (when (executable-find "fd")
    (define-advice counsel-file-jump (:around (foo &optional initial-input initial-directory) aorst:counsel-fd)
      (let ((find-program "fd")
            (counsel-file-jump-args (split-string "-L --type f --hidden")))
        (funcall foo initial-input initial-directory))))
  (when (executable-find "rg")
    (setq counsel-rg-base-command
          "rg -S --no-heading --hidden --line-number --color never %s .")))

;; A better ivy with decriptions
(use-package ivy-rich
  :after counsel
  :config
  (ivy-rich-mode 1))

;; Completion framwork for anything
(use-package company
  :bind (:map company-active-map
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

;; Syntax checking
(use-package flycheck
  :bind (:map flycheck-mode-map
         ("C-c ! C-h" . hydrant/flycheck/body))
  :custom
  (flycheck-indication-mode 'right-fringe)
  (flycheck-display-errors-delay 86400 "86400 seconds is 1 day")
  :config
  (when (fboundp #'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-double-exclamation-mark
      (vector #b00000000
              #b00000000
              #b00000000
              #b01100110
              #b01100110
              #b01100110
              #b01100110
              #b01100110
              #b01100110
              #b01100110
              #b01100110
              #b00000000
              #b01100110
              #b01100110
              #b00000000
              #b00000000
              #b00000000))
    (define-fringe-bitmap 'flycheck-exclamation-mark
      (vector #b00000000
              #b00000000
              #b00000000
              #b00011000
              #b00011000
              #b00011000
              #b00011000
              #b00011000
              #b00011000
              #b00011000
              #b00011000
              #b00000000
              #b00011000
              #b00011000
              #b00000000
              #b00000000
              #b00000000))
    (define-fringe-bitmap 'flycheck-question-mark
      (vector #b00000000
              #b00000000
              #b00000000
              #b00111100
              #b01111110
              #b01100110
              #b01100110
              #b00000110
              #b00001100
              #b00011000
              #b00011000
              #b00000000
              #b00011000
              #b00011000
              #b00000000
              #b00000000
              #b00000000))
    (flycheck-define-error-level 'error
      :severity 100
      :compilation-level 2
      :overlay-category 'flycheck-error-overlay
      :fringe-bitmap 'flycheck-double-exclamation-mark
      :fringe-face 'flycheck-fringe-error
      :error-list-face 'flycheck-error-list-error)
    (flycheck-define-error-level 'warning
      :severity 100
      :compilation-level 1
      :overlay-category 'flycheck-warning-overlay
      :fringe-bitmap 'flycheck-exclamation-mark
      :fringe-face 'flycheck-fringe-warning
      :error-list-face 'flycheck-error-list-warning)
    (flycheck-define-error-level 'info
      :severity 100
      :compilation-level 0
      :overlay-category 'flycheck-info-overlay
      :fringe-bitmap 'flycheck-question-mark
      :fringe-face 'flycheck-fringe-info
      :error-list-face 'flycheck-error-list-info))

 (when (fboundp #'defhydra)
   (defhydra hydrant/flycheck (:color blue :hint nil)
     "
 ^Flycheck^         ^Errors^       ^Checker^
 _q_: quit          _<_: previous  _?_: describe
 _M_: manual        _>_: next      _d_: disable
 _v_: verify setup  _f_: check     _m_: mode
 ^ ^                _l_: list      _s_: select"
     ("q" ignore :exit t)
     ("M" flycheck-manual)
     ("v" flycheck-verify-setup)
     ("<" flycheck-previous-error :color pink)
     (">" flycheck-next-error :color pink)
     ("f" flycheck-buffer)
     ("l" flycheck-list-errors)
     ("?" flycheck-describe-checker)
     ("d" flycheck-disable-checker)
     ("m" flycheck-mode)
     ("s" flycheck-select-checker))))

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

;; Smart sorting and filtering for ivy
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode))

;; Be smart when using parens, and highlight content
(use-package smartparens
  :init
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook 'smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  :config
  ;; highligh matching brackets
  (show-smartparens-global-mode 0)
  ;; so that paren highlights do not override region marking (aka selecting)
  (setq show-paren-priority -1)
  (setq show-paren-when-point-inside-paren t)
  (setq sp-show-pair-from-inside t)
  (setq show-paren-style 'mixed))

;; ;; LSP client
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

;; Search symbols with ivy
(use-package lsp-ivy)

;; Debugger
(use-package dap-mode
  :diminish
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
  :config
  (defvar my/one-org-agenda-file (expand-file-name "agenda.files" org-directory)
      "One file to contain a list of all Org agenda files.")
  (setq org-agenda-files (expand-file-name "agenda.files" org-directory))
  (unless (file-exists-p my/one-org-agenda-file)
      ;; http://stackoverflow.com/a/14072295/1219634
      ;; touch `my/one-org-agenda-file'
      (write-region "" :ignore my/one-org-agenda-file))
  (when (not (version<= org-version "9.1.9"))
    (use-package org-tempo
      :straight nil))
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("cc" . "src c++"))
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

(use-package org-bullets
  :after org
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))

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

;; Table of contents
(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

;; Structure Templates
;; ------------------------------------


;; Programming Languages
;; ------------------------------------
;; C source files
(use-package prog-mode
  :straight nil
  :hook (prog-mode . hl-line-mode))

;; Python
(use-package pyvenv
  :demand t
  :config
  (setq pyvenv-workon "emacs")  ; Default venv
  (pyvenv-tracking-mode 1))  ; Automatically use pyvenv-workon via dir-locals

(use-package cc-mode
  :straight nil
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
  :after cc-mode
  :bind (:map c-mode-base-map
         ("C-c C-M-f" . clang-format-buffer)))

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
(use-package toml-mode)

;; Nice LISP/Scheme language
(use-package racket-mode
  :bind (:map racket-mode-map
         (")" . self-insert-command)
         ("]" . self-insert-command)
         ("}" . self-insert-command))
  :config
  (set-face-attribute 'racket-debug-break-face nil :background (face-attribute 'error :foreground) :foreground (face-attribute 'default :background))
  (set-face-attribute 'racket-debug-result-face nil :foreground (face-attribute 'font-lock-comment-face :foreground) :box nil)
  (set-face-attribute 'racket-debug-locals-face nil :foreground (face-attribute 'font-lock-comment-face :foreground) :box nil)
  (set-face-attribute 'racket-selfeval-face nil :foreground (face-attribute 'default :foreground)))

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
(use-package lua-mode)

;; CSS mode
(use-package css-mode
  :straight nil
  :custom
  (css-indent-offset 2))

;; JSON mode
(use-package json-mode
  :hook (json-mode . flycheck-mode)
  :custom (js-indent-level 2))

;; ------------------------------------

;; Containers
;; ------------------------------------

;; Kubernetes
(use-package kubel-evil)

;; Docker
(use-package docker)

;; ------------------------------------


;; Tools
;; ------------------------------------

(use-package disk-usage
  :commands (disk-usage))

;; Nerdtree like side bar
(use-package treemacs-evil)

;; RSS feed
(use-package elfeed
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


(use-package counsel-projectile
  :config
  (counsel-projectile-mode +1))

(use-package projectile
  :diminish projectile-mode
  :after counsel-projectile
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'ivy) ;So projectile works with ivy
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'alien))

;; Display keys in a menu
(use-package which-key
  :config
  (which-key-mode t))

;; Search engines
(use-package engine-mode
    :straight (:branch "main")
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
  "bs" '(ivy-switch-buffer :which-key "Switch buffer")
  "bi" '(my/indent-buffer :which-key "Indent buffer")
  "be" '(ediff-buffers :which-key "Difference")

  ;; Files
  "f"  '(:ignore t :which-key "Files")
  "fb" '(treemacs :which-key "File browser")
  "fd" '(dired-jump :which-key "Dired")
  "ff" '(find-file :which-key "Find file")
  "fj" '(find-journal :which-key "Journal")
  "fr" '(counsel-recentf :which-key "Recent files")

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
 "C-x C-f" 'find-file
 "C-x C-b" 'ivy-switch-buffer
 "M-x" 'counsel-M-x
 "C-c v" 'counsel-describe-variable
 "C-c f" 'counsel-describe-function
 "C-c C-f" 'counsel-find-file
 "C-c C-d" 'racket-run-with-debugging
 "C-c C-M-f" 'my/indent-buffer
 "C-c p f" 'counsel-projectile-find-file
 "M-p" 'emmet-expand-yas
 "C-S-c" 'aya-create
 "C-S-e" 'aya-expand
 "C-s" 'save-buffer
 "C-c l" 'org-store-link
 "C-c a" 'org-todo-list
 "C-k" 'kill-buffer-and-window
 "C-c c" 'org-capture
 "C-;" 'shell-pop
 "C-'" 'grugru)

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

;; (general-def 'normal dired-mode-map
;;   "Y" '(lambda () (interactive) (dired-copy-filename-as-kill 0))
;;   "y" 'dired-copy-filename-as-kill)

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

;; Increase / Decrease font
(general-define-key "<M-up>" 'text-scale-increase)
(general-define-key "<M-down>" 'text-scale-decrease)

;; Move between buffers
(general-define-key "C-h" 'evil-window-left)
(general-define-key "C-j" 'evil-window-down)
(general-define-key "C-k" 'evil-window-up)
(general-define-key "C-l" 'evil-window-right)

(define-key global-map "F" '("foo" . find-file))

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
