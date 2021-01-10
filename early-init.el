;;; early-init.el --- early configurations -*- lexical-binding: t -*-
;;;
;;; Commentary:
;;; Emacs early init file
;;; This file is executed before any graphics (Emacs Version > 27)
;;;
;;; Code:


;; Profile Emacs startup speed
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Get current garbage collection setting to restore later
(defvar my/gc-cons-threshold gc-cons-threshold)
(defvar my/gc-cons-percentage gc-cons-percentage)
(defvar my/file-name-handler-alist file-name-handler-alist)

;; Raising GC threshold to prevent its running
(setq-default gc-cons-threshold 402653184
              gc-cons-percentage 0.6
              inhibit-compacting-font-caches t
              message-log-max 16384
              file-name-handler-alist nil)

;; Restore GC after init
(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold my/gc-cons-threshold
                  gc-cons-percentage my/gc-cons-percentage
                  file-name-handler-alist my/file-name-handler-alist)))


;; Disable UI elements as early as possible
(setq initial-frame-alist '((tool-bar-lines . 0)
                            (bottom-divider-width . 0)
                            (right-divider-width . 1))
      default-frame-alist initial-frame-alist)

;; Check if running inside termux
;; Might be replace with 'no graphics check'
(require 'subr-x)
(defvar my/is-termux
      (string-suffix-p "Android" (string-trim (shell-command-to-string "uname -a"))))

;; Disable toolbar and scrollbar if not in termux
(unless my/is-termux
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

;; Inhibit resizing frame.
;; Setting x-gtk-resize-child-frames variable to resize-mode fixes issue with GNOME Shell.
(setq frame-inhibit-implied-resize t
      x-gtk-resize-child-frames 'resize-mode)

;; maybe improve performance on windows
(setq w32-pipe-read-delay 0)

;; gccmacs?
(defvar comp-deferred-compilation)
(setq comp-deferred-compilation t)

;; Disable truncated lines arrows.
(setq-default fringe-indicator-alist
              (assq-delete-all 'truncation fringe-indicator-alist))

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

;; Install use-package
(straight-use-package 'use-package)

;; Makes :straight t by default
(defvar straight-use-package-by-default)
(setq straight-use-package-by-default t)

;; Disable package.el initialization at startup:
(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
