;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Use Straight el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;(require 'cl-lib)
(require 'subr-x)
(setq my/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

;; Install use-package
(straight-use-package 'use-package)
;; makes :straight t by default
(setq straight-use-package-by-default t)

;; Speed up bootstrapping
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)
(add-hook 'after-init-hook `(lambda ()
                              (setq gc-cons-threshold 800000
                                    gc-cons-percentage 0.1)
                              (garbage-collect)) t)

;; maybe improve performance on windows
(setq w32-pipe-read-delay 0)

(unless my/is-termux
    (tool-bar-mode -1)
    (scroll-bar-mode -1))

(global-hl-line-mode 1)
(winner-mode t)
(menu-bar-mode -1)

;; Don't Lock Files
(setq-default create-lockfiles nil)

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

;; Line numbers
(column-number-mode)
;; If `display-line-numbers-mode' is available (only in Emacs 26),
;; use it! Otherwise, install and run nlinum-relative.
(if (functionp 'display-line-numbers-mode)
    (and (add-hook 'display-line-numbers-mode-hook
                   (lambda () (setq display-line-numbers-type 'relative)))
         (add-hook 'prog-mode-hook #'display-line-numbers-mode))
  (use-package nlinum-relative
    :ensure t
    :config
    (nlinum-relative-setup-evil)
    (setq nlinum-relative-redisplay-delay 0)
    (add-hook 'prog-mode-hook #'nlinum-relative-mode)))

(setq auto-window-vscroll nil) 		;avoid next-line to trigger line-move-partial
(setq mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control) . nil)))
(setq ring-bell-function 'ignore)
(setq mouse-wheel-progressive-speed nil)
(setq inhibit-startup-screen t)
(setq user-full-name "David Delarosa"
      user-mail-address "xdavidel@gmail.com")

;; UTF-8 please
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(unless my/is-termux
    ;; set a default font
    (set-face-attribute 'default nil
                        :family "Cascadia Code"
                ;; :family "IBM Plex Mono Medium"
                        :height 90
                        :weight 'normal
                        :width 'normal)

    ;; specify font for all unicode characters
    (set-fontset-font t
                      'unicode
                      (font-spec :family "Cascadia Code"
                                 :width 'normal
                                 :height 100
                                 :weight 'normal) nil 'prepend)
    )

;; Don't create backups
(setq make-backup-files nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

(blink-cursor-mode 1)

;; Don't beep at me
(setq visible-bell t)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

; (general-def 'normal org-mode-map "key" 'def ) example with 2 positional arguments
(use-package general)

(use-package diminish :defer t)

(use-package color-identifiers-mode
  :defer t)

(use-package paren
  :defer 10
  :config (show-paren-mode 1))

(use-package recentf
  :defer 10
  :config
  (recentf-mode t)
  (setq recentf-max-saved-items 500
          recentf-max-menu-items 15
          recentf-auto-cleanup 60))

(use-package autorevert
  :defer 10
  :config (auto-revert-mode 1))

(use-package swiper
  :bind (("C-s" . swiper)))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-c v" . counsel-describe-variable)
         ("C-c f" . counsel-describe-function)
         ("C-x C-f" . counsel-find-file))
  :general
  ('normal org-mode-map "C-c C-j" 'counsel-org-goto)
  :config
  (use-package smex :straight t))

(use-package ivy
  :diminish ivy-mode
  :general
  ('normal :prefix "SPC" "x b" 'ivy-switch-buffer)
  ('normal "C-x b" 'ivy-switch-buffer)
  :bind (:map ivy-minibuffer-map
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line))
  :config
  ;; Disable ido
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))
  (setq ivy-display-style 'fancy)
  (setq ivy-dynamic-exhibit-delay-ms 200)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  ;; C-M-j imediate done ivy
  ;; ;; Show recently killed buffers when calling ivy-switch-buffer
  (setq ivy-use-virtual-buffers t)
  ;; (setq ivy-virtual-abbreviate 'full) ; Show the full virtual file paths
  ;; ;; Do not show "./" and "../" in the counsel-find-file completion list
  (setq ivy-extra-directories nil))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode))

(use-package all-the-icons-ivy
    :after (all-the-icons ivy)
    :custom (all-the-icons-ivy-file-commands '(counsel-dired-jump
                                               counsel-find-file
                                               counsel-file-jump
                                               counsel-find-library
                                               counsel-git
                                               counsel-projectile-find-dir
                                               counsel-projectile-find-file
                                               counsel-recentf))
    :config (all-the-icons-ivy-setup))

(use-package smartparens
  :diminish smartparens-mode
  :commands smartparens-mode
  :general
  ('normal smartparens-mode-map "M-l" 'sp-next-sexp)
  ('normal smartparens-mode-map "M-h" 'sp-previous-sexp)
  ('normal smartparens-mode-map "M-k" 'sp-up-sexp)
  ('normal smartparens-mode-map "M-j" 'sp-down-sexp)
  ('normal smartparens-mode-map "C-M-l" 'sp-forward-sexp)
  :init
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'c++-mode-hook 'smartparens-mode)
  (add-hook 'lisp-interaction-mode-hook 'smartparens-mode)
  (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
  (add-hook 'LaTeX-mode-hook 'smartparens-mode)
  (add-hook 'org-mode-hook 'smartparens-mode)
  :config
  (sp-local-pair 'org-mode "_" "_" )
  (sp-local-pair 'org-mode "*" "*" )
  (sp-local-pair 'latex-mode "$" "$")
  (sp-local-pair 'latex-mode "\\left(" "\\right)" :trigger "\\l(")
  ;; highligh matching brackets
  (show-smartparens-global-mode 0)
  ;; so that paren highlights do not override region marking (aka selecting)
  (setq show-paren-priority -1)
  (setq show-paren-when-point-inside-paren t)
  (setq sp-show-pair-from-inside t)
  (setq show-paren-style 'mixed))

(use-package flycheck
  :disabled
  :hook (python-mode . flycheck-mode))

(use-package evil-mc
  :after evil
  :general ('visual
	    "M-n" 'evil-mc-make-and-goto-next-cursor
	    "C-n" 'evil-mc-make-and-goto-next-match)
  :config (global-evil-mc-mode +1))

(use-package evil-multiedit
  :after evil
  :general
  ('normal "C-;" 'evil-multiedit-match-all)
  (:states '(normal visual) "M-d" 'evil-multiedit-match-and-next)
  (evil-multiedit-state-map "C-n" 'evil-multiedit-next)
  (evil-multiedit-state-map "C-p" 'evil-multiedit-prev)
  (evil-multiedit-insert-state-map "C-n" 'evil-multiedit-next)
  (evil-multiedit-insert-state-map "C-p" 'evil-multiedit-prev)
  )

(use-package evil
  :diminish evil-mode
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (evil-mode 1)
  :general
  ("<C-tab>" 'evil-window-next)
  ("<C-S-iso-left-tab>" 'evil-window-prev)
  ("C-h" 'evil-window-left)
  ("C-j" 'evil-window-down)
  ("C-k" 'evil-window-up)
  ("C-l" 'evil-window-right)
  ('normal ";" 'evil-search-forward)
  ('normal "M-p" 'evil-paste-from-register)
  ('normal :prefix "SPC" "l" 'evil-last-non-blank)
  ('normal :prefix "SPC" "h" 'evil-first-non-blank)
  :config
  (setq
   lazy-highlight-cleanup nil
   lazy-highlight-max-at-a-time nil
   lazy-highlight-initial-delay 0))

(use-package evil-easymotion
  :after evil)

(use-package evil-nerd-commenter
  :after evil
  :general
  ('normal "C-/" 'evilnc-comment-or-uncomment-lines)
  ('normal "gcc" 'evilnc-comment-or-uncomment-lines)
  ('visual "gc" 'evilnc-comment-or-uncomment-lines))

(use-package evil-terminal-cursor-changer
  :init
  (evil-terminal-cursor-changer-activate)
  (setq evil-motion-state-cursor 'box)  ; █
  (setq evil-visual-state-cursor 'box)  ; █
  (setq evil-normal-state-cursor 'box)  ; █
  (setq evil-insert-state-cursor 'bar)  ; ⎸
  (setq evil-emacs-state-cursor  'hbar) ; _
  )

;; Sensible Evil Splits
(setq evil-vsplit-window-right t)

(general-define-key
 :states '(normal visual insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
  "b" '(iterm-focus :which-key "iterm")
  "?" '(iterm-goto-filedir-or-home :which-key "iterm - goto dir")
  "/" '(counsel-ag :wich-key "ag")
  "TAB" '(ivy-switch-buffer :which-key "prev buffer")
  "." '(avy-goto-word-or-subword-1  :which-key "go to word")
  "SPC" '(counsel-M-x :which-key "M-x")
  "a" '(hydra-launcher/body :which-key "Applications")
  "b" '(hydra-buffer/body t :which-key "Buffer")
  "c" '(:ignore t :which-key "Comment")
  "cl" '(comment-or-uncomment-region-or-line :which-key "comment line")
  "w" '(hydra-window/body :which-key "Window")
  "f" '(:ignore t :which-key "Files")
  "fd" '(counsel-git :which-key "find in git dir")
  ;; ...
  )

(use-package evil-leader
  :after evil
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "SPC")
  (evil-leader/set-key
      "a" 'evil-append-line
      "d" 'downcase-word
      "p" 'find-file
      "q" 'counsel-org-goto-all
      "s" 'counsel-imenu
      "g c" 'my-magit-stage-all-and-commit
      "b d" 'kill-buffer
      "b ?" 'counsel-switch-buffer
      "o c" 'org-checkbox
      "o j" 'org-clock-goto
      "o l" 'org-toggle-latex-fragment
      "o t" 'org-toggle-checkbox
      "o v" 'org-toggle-inline-images
      "o x" 'org-clock-in-last
      "w u" 'upcase-word
      "x u" 'outline-up-heading
      "x s" 'save-buffer
      "z s" 'org-narrow-to-subtree
      "z w" 'widen
      "<tab>" 'next-multiframe-window
      "," 'flyspell-correct-at-point)

  ;; function to toggle case
  (defun xah-toggle-letter-case ()
    "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.
URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2017-04-19"
  (interactive)
  (let (
        (deactivate-mark nil)
        $p1 $p2)
    (if (use-region-p)
        (setq $p1 (region-beginning)
              $p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]-_")
        (setq $p1 (point))
        (skip-chars-forward "[:alnum:]-_")
        (setq $p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region $p1 $p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region $p1 $p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region $p1 $p2)
      (put this-command 'state 0))))))

(use-package evil-snipe
  :diminish (evil-snipe-mode evil-snipe-local-mode evil-snipe-override-mode)
  :general ('normal "f" 'evil-snipe-f)
  :after evil
  :config
  (evil-snipe-override-mode 1)
  (setq evil-snipe-spillover-scope 'visible
	evil-snipe-smart-case t))

(use-package evil-numbers
  :after evil
  :general
  ('normal "C-c =" 'evil-numbers/inc-at-pt)
  ('normal "C-c -" 'evil-numbers/dec-at-pt)
  :diminish evil-numbers-modes)

(use-package evil-goggles
  :defer 30
  :after evil
  :diminish evil-goggles-mode
  :config
  (evil-goggles-mode)
  (setq evil-goggles-pulse nil)
  (setq evil-goggles-duration 0.2)
  (evil-goggles-use-diff-faces))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-outline-bind-tab-p nil)
  ;; (delete 'paren evil-collection-mode-list)
  (evil-collection-init))

(use-package rg
  :general ('normal "C-c r" 'rg-menu))

(use-package evil-org
  :diminish evil-org-mode
  :after org
  :init
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-exchange
  :after evil
  :general ('normal "g x" 'evil-exchange)
  :config (evil-exchange-install))

(use-package evil-matchit
  :after evil python
  :config
  (global-evil-matchit-mode 4))

(use-package beacon
  :diminish beacon-mode
  :defer 25
  :config
  (setq beacon-blink-delay .5)
  (setq beacon-size 8)
  (setq beacon-blink-when-focused t)
  (setq beacon-blink-duration .5)
  (setq beacon-blink-when-window-scrolls nil)
  (beacon-mode 1))

(use-package  undo-tree)

(with-eval-after-load 'evil
  (global-undo-tree-mode -1)
  (evil-define-key 'normal 'global "u" 'undo-only))

(use-package undo-propose
  :after evil
  :general
  ('normal 'global "C-c u" 'undo-propose)
  ('normal 'global "u" 'undo-only)
  :init
  :config
  (setq undo-propose-pop-to-buffer t))

(use-package magit
  :bind ("C-c g" . magit-status)
  :commands my-magit-stage-all-and-commit
  :config
  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

  ;;This setting is needed to use ivy completion:
  (setq magit-completing-read-function 'ivy-completing-read)

  ;; full screen magit-status
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun my-magit-stage-all-and-commit(message)
    (interactive "sCommit Message: ")
    (magit-stage-modified)
    (magit-commit (list "-m" message)))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (jump-to-register :magit-fullscreen)))

(use-package evil-magit
  :after evil magit)

(use-package rainbow-mode
  :defer 5
  :diminish rainbow-mode
  :config (rainbow-mode))

;; Org Mode
;; Turn on indentation and auto-fill mode for Org files
(defun my/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil)
  (diminish org-indent-mode))

(use-package org
  :defer t
  :hook (org-mode . my/org-mode-setup)
  :config
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-hide-block-startup nil
        org-src-preserve-indentation nil
        org-startup-folded 'content
        org-cycle-separator-lines 2)

  (setq org-modules
    '(org-crypt
        org-habit
        org-bookmark
        org-eshell
        org-irc))

  (setq org-refile-targets '((nil :maxlevel . 3)
                            (org-agenda-files :maxlevel . 3)))
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-j") 'org-metadown)
  (evil-define-key '(normal insert visual) org-mode-map (kbd "M-k") 'org-metaup)

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (ledger . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes)

  ;; Auto Tangle at save
  ;; Since we don't want to disable org-confirm-babel-evaluate all
  ;; of the time, do it around the after-save-hook
  (defun my/org-babel-tangle-dont-ask ()
  ;; Dynamic scoping to the rescue
  (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle)))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'my/org-babel-tangle-dont-ask
                                              'run-at-end 'only-in-org-mode))))
;;Use bullet characters instead of asterisks, plus set the header font sizes to something more palatable.  A fair amount of inspiration has been taken from [[https://zzamboni.org/post/beautifying-org-mode-in-emacs/][this blog post]].

(use-package org-superstar
  :if (not my/is-termux)
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list '("◉" "○" "●" "○" "●" "○" "●")))

;; Make sure org-indent face is available
(require 'org-indent)

;; Ensure that anything that should be fixed-pitch in Org files appears that way
(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; These templates enable you to type things like =<el= and then hit =Tab= to expand
;; the template.  More documentation can be found at the Org Mode [[https://orgmode.org/manual/Easy-templates.html][Easy Templates]]
;; documentation page.

;; This is needed as of Org 9.2
(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc" . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts" . "src typescript"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))

;; This is probably not needed if I plan to use custom functions that are invoked
;; through =emacsclient.=
(require 'org-protocol)

;; Org Margins
(defun my/org-mode-visual-fill()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))
(use-package visual-fill-column
  :defer t
  :hook (org-mode . my/org-mode-visual-fill))

;; It's nice to have a table of contents section for long literate configuration files (like this one!) so I use =org-make-toc= to automatically update the ToC in any header with a property named =TOC=.
(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package org-cliplink
  :after org
  :bind (:map org-mode-map
	      ("C-x p i" . 'org-cliplink)))

(use-package crux
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ("C-c o" . crux-open-with))
  :general ('normal :prefix "SPC" "c" 'crux-capitalize-region))

(use-package flyspell
  :diminish flyspell-mode
  :commands flyspell-mode
  :hook (('LaTeX-mode . flyspell-mode)
	 ('org-mode . flyspell-mode))
  :general
  ('normal flyspell-mode-map "C-," 'flyspell-goto-next-error)
  :config
 ;; (setq ispell-program-name "hunspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (setq ispell-dictionary "en_US,pt_BR")
  (ispell-set-spellchecker-params))
  ;;(ispell-hunspell-add-multi-dic "en_US,pt_BR"))

(use-package flyspell-lazy
  :commands flyspell-lazy-mode
  :hook ((LaTeX-mode . flyspell-lazy-mode)
	 (org-mode . flyspell-lazy-mode)))

(use-package flyspell-correct-ivy
  :demand t
  :after flyspell-lazy
  :bind (:map flyspell-mode-map
	      ("C-c C-SPC" . flyspell-correct-wrapper)
	      ("C-c C-;" . flyspell-correct-at-point))
  :custom (flyspell-correct-interface 'flyspell-correct-ivy))

(use-package company
  :diminish company-mode
  :commands company-mode
  :bind (:map company-active-map
            ("<down>"   . company-select-next)
            ("<up>"   . company-select-previous)
            ("RET"   . company-complete-selection)
            ("<ret>" . company-complete-selection))
  :general ('(normal insert) company-mode-map "C-SPC" 'company-complete)
  :hook (
	 ;; (python-mode . company-mode)
	 (LaTeX-mode . company-mode)
	 (c++-mode . company-mode)
	 (emacs-lisp-mode . company-mode)
	 (org-mode . company-mode)))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode))

(use-package company-statistics
  :after company
  :config
  (company-statistics-mode))

(use-package company-posframe
  :diminish company-posframe-mode
  :after company
  :config
  (company-posframe-mode 1))

(use-package dired
  :straight nil
  :commands dired
  :config
  (setq dired-omit-files "^\\.\\|^#.#$\\|.~$")
  (setq dired-auto-revert-buffer t)
  (setq dired-hide-details-mode t)
  (defun xah-dired-mode-setup ()
    "to be run as hook for `dired-mode'."
    (dired-hide-details-mode 1))
  (add-hook 'dired-mode-hook 'xah-dired-mode-setup))

(use-package treemacs
  :defer t
  ;; :init
  ;; (with-eval-after-load 'winum
  ;;   (define-key winum- (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs              (if (executable-find "python") 3 0)
	  treemacs-deferred-git-apply-delay   0.5
	  treemacs-display-in-side-window     t
	  treemacs-file-event-delay           5000
	  treemacs-file-follow-delay          0.2
	  treemacs-follow-after-init          nil
	  treemacs-follow-recenter-distance   0
	  treemacs-git-command-pipe           ""
	  treemacs-goto-tag-strategy'refetch-index
	  treemacs-indentation                1
	  treemacs-indentation-string         " "
	  treemacs-is-never-other-window      nil
	  treemacs-max-git-entries            5000
	  treemacs-no-png-images              nil
	  treemacs-no-delete-other-windows    t
	  treemacs-project-follow-cleanup     nil
	  treemacs-persist-file               (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-recenter-after-file-follow nil
	  treemacs-recenter-after-tag-follow  nil
	  treemacs-show-cursor                nil
	  treemacs-show-hidden-files          t
	  treemacs-silent-filewatch           nil
	  treemacs-silent-refresh             nil
	  treemacs-sorting 'alphabetic-desc
	  treemacs-space-between-root-nodes   t
	  treemacs-tag-follow-cleanup         t
	  treemacs-tag-follow-delay           1.5
	  treemacs-width                      25)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    (treemacs-resize-icons 12)
    ;; (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil)

(use-package solaire-mode
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-mode-swap-bg))

(use-package doom-themes :defer t)
(load-theme 'doom-palenight t)
(doom-themes-visual-bell-config)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package doom-modeline
  :defer 3
  :config
  (doom-modeline-mode)
  ;; Don’t compact font caches during GC.
  (setq inhibit-compacting-font-caches t)
  (unless my/is-termux
    (setq doom-modeline-icon t)))

(use-package lsp-mode
  :defer t
  :commands lsp
  :general (
            ('normal "K" 'lsp-describe-thing-at-point)
            ('normal "g d" 'lsp-find-definition)
            ('normal "g e" 'lsp-find-references))
  :hook ((python-mode . lsp)
	 (c++-mode . lsp))
  :config
  (setq lsp-highlight-symbol-at-point nil
	lsp-document-highlight-delay 1
	lsp-enable-snippet nil
        lsp-completion-provider :capf))

(use-package lsp-ui
  :disabled
  :after lsp-mode
  :commands lsp-ui-mode
  :commands
  (setq lsp-ui-doc-enable nil))

(use-package company-lsp
  :after lsp-mode
  :config
  (push 'company-lsp company-backends)
  (setq company-lsp-enable-snippet nil
	lsp-enable-snippet nil))

(use-package flymake
  :commands flymake-mode
  :general
  ('normal "C-," 'flymake-goto-next-error)
  :config
  (setq flymake-max-parallel-syntax-checks 2
	flymake-no-changes-timeout 10
	flymake-number-of-errors-to-display 2))

(use-package flymake-diagnostic-at-point
  :after flymake
  :config
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  (setq flymake-diagnostic-at-point-timer-delay 2))

(use-package dap-mode
  :after lsp-mode
  :commands dap-debug
  :hook ((python-mode . dap-ui-mode)
	 (python-mode . dap-mode))
  :config
  (require 'dap-python)
  (require 'dap-lldb))

(use-package python
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("python" . python-mode)
  :hook (python-mode . toggle-truncate-lines)
  :config
  ;; dont guess the indent offset
  (setq python-indent-guess-indent-offset nil))

(use-package highlight-symbol
  :after python anaconda-mode
  :hook (python-mode . highlight-symbol-mode))

(use-package highlight-indent-guides
  :after python
  :diminish highlight-indent-guides-mode
  :init
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (add-hook 'lisp-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(use-package adaptive-wrap
  :straight adaptive-wrap
  :hook (visual-line-mode . adaptive-wrap-prefix-mode))

(use-package hydra
  :defer 5
  :bind (("C-c C-w" . hydra-window-resize/body)
         ("C-c C-u" . hydra-outline/body)
         ("C-x C-m " . multiple-cursors-hydra/body)
         ("C-x C-'" . hydra-fold/body))
  :config
  (defhydra hydra-expand-region ()
    "region: "
    ("k" er/expand-region "expand")
    ("j" er/contract-region "contract"))
  (general-def 'visual 'global "v" 'hydra-expand-region/body)

  (defhydra hydra-fold (:pre (hs-minor-mode 1))
    "fold"
    ("t" fold-dwim-toggle "toggle")
    ("h" fold-dwim-hide-all "hide-all")
    ("s" fold-dwim-show-all "show-all")
    ("q" nil "quit"))

  (defun my-funcs/resize-window-down ()
    "Resize a window downwards."
    (interactive)
    (if (window-in-direction 'below)
        (enlarge-window 1)
      (shrink-window 1)))
  (defun my-funcs/resize-window-up ()
    "Resize a window upwards."
    (interactive)
    (if (window-in-direction 'above)
        (enlarge-window 1)
      (shrink-window 1)))
  (defun my-funcs/resize-window-left ()
    "Resize a window leftwards."
    (interactive)
    (if (window-in-direction 'left)
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1)))
  (defun my-funcs/resize-window-right ()
    "Resize a window rightwards."
    (interactive)
    (if (window-in-direction 'right)
        (enlarge-window-horizontally 1)
      (shrink-window-horizontally 1)))

  (defhydra hydra-window-resize (global-map "C-c w")
    "Window resizing"
    ("j" my-funcs/resize-window-down "down")
    ("k" my-funcs/resize-window-up "up")
    ("l" my-funcs/resize-window-right "right")
    ("h" my-funcs/resize-window-left "left"))

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
     ("q" nil)))

;; Increase / Decrease font
(global-set-key (kbd "<M-up>") 'text-scale-increase)
(global-set-key (kbd "<M-down>") 'text-scale-decrease)

(use-package goto-last-change
  :general ('normal "g b" 'goto-last-change)
  :bind ("C-x C-j" . goto-last-change))

(use-package outline-mode
  :straight nil
  :hook ((python-mode . outline-minor-mode)
	 (LaTeX-mode . outline-minor-mode)
	 (emacs-lisp-mode . outline-minor-mode))
  :commands outline-minor-mode)

(use-package realgud
  :disabled
  :commands realgud:ipdb)

(use-package counsel-projectile
  :general
  ("C-c p f" 'counsel-projectile-find-file)
  :config
  (counsel-projectile-mode +1))

(use-package projectile
  :diminish projectile-mode
  :after counsel-projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-completion-system 'ivy) ;So projectile works with ivy
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'alien))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package which-key
  :defer 10
  :config
  (which-key-mode t))

(use-package hl-todo
  :after python
  :hook (python-mode . hl-todo-mode))
;; abbrev for speed and less strain
(setq-default abbrev-mode t)
(diminish 'abbrev-mode)
(setq save-abbrevs 'silently)

;; open cmd
(defun my-open-cmd ()
  "open cmd at file location"
  (interactive)
  (start-process-shell-command (format "powershell(%s)" default-directory) nil "start powershell"))
(if (eq system-type 'windows-nt)
    (bind-key "C-x m" 'my-open-cmd))

;; set 80 width columns
(defun set-window-width (n)
  "Set the selected window's width."
  (adjust-window-trailing-edge (selected-window) (- n (window-width)) t))

(defun set-80-columns ()
  "Set the selected window to 80 columns."
  (interactive)
  (set-window-width 94))

(general-def 'normal "C-w 8" 'set-80-columns)
(use-package server
  :init
  (progn
    (when (equal window-system 'w32)
      ;; Set EMACS_SERVER_FILE to `server-auth-dir'\`server-name'
      ;; e.g. c:\Users\Aaron\emacs.d\server\server
      (setq server-use-tcp t)))
  :config
  (or (eq (server-running-p) t)
      (server-start)))
