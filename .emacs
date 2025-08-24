;; -*- lexical-binding: t; -*-
(setq custom-file "~/.emacs.custom.el")
(add-to-list 'load-path "~/em_pl")
(setq ring-bell-function 'ignore)
(setq global-visual-line-mode '1)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)

(use-package direnv
  :config
  (direnv-mode))

;;; NIX (NIXOS)

(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))
(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")
(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))
(use-package nix-repl
  :ensure nix-mode
  :commands (nix-repl))


;; ORG MODE

(when (display-graphic-p)
  (setq org-hide-emphasis-markers t)
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "OpenDyslexicAlt Nerd Font Propo" :height 180 :weight thin))))
   '(fixed-pitch ((t ( :family "OpenDyslexicM Nerd Font Mono" :height 160)))))

  (add-hook 'org-mode-hook 'visual-line-mode)

  (let* ((variable-tuple
          (cond ((x-list-fonts "OpenDyslexicAlt Nerd Font Propo")         '(:font "OpenDyslexicAlt Nerd Font Propo"))
                ((x-list-fonts "OpenDyslexicAlt Nerd Font Propo") '(:font "OpenDyslexicAlt Nerd Font Propo"))
                ((x-list-fonts "OpenDyslexicAlt Nerd Font Propo")   '(:font "OpenDyslexicAlt Nerd Font Propo"))
                ((x-list-fonts "OpenDyslexicAlt Nerd Font Propo")         '(:font "OpenDyslexicAlt Nerd Font Propo"))
                ((x-family-fonts "OpenDyslexicAlt Nerd Font Propo")    '(:family "OpenDyslexicAlt Nerd Font Propo"))
                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
         (base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))
  )
;;;

;;; ODIN
    
(require 'odin-mode)

;;;

;;; RUST

(require 'rust-mode)    
(add-hook 'rust-mode-hook   
          (lambda () (setq indent-tabs-mode nil)))    

;;;
    
;;; OCAML
    
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save))))

(setq font-lock-maximum-decoration '3)
(setq tuareg-font-double-semicolon-face '((t (:inherit font-lock-type-face))))
(add-hook 'tuareg-mode-hook
          (lambda() (setq tuareg-mode-name "🐫")))

(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure)
  (ocaml-eglot . (lambda () 
                 (add-hook #'before-save-hook #'eglot-format nil t))))

(use-package ocamlformat
  :custom
  (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))
;;;
    
(setq package-enable-at-startup nil)

;;; Basic Emacs Stuff
(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode 1)
(global-font-lock-mode 1)
(toggle-truncate-lines 0)
(setq truncate-partial-width-windows nil)
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(setq-default tab-width 2)
(setq inhibit-startup-screen 0)
(setq column-number-mode 1)
(global-display-line-numbers-mode 0)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'insert-tab)
(setq make-backup-files nil)

(keymap-global-set "M-\\" 'other-window)
;;(keymap-global-set "M-[" 'split-window-below)
;;(keymap-global-set "M-]" 'split-window-right)
(keymap-global-set "M-#" 'delete-window)
(keymap-global-set "M-u" 'compile)
(keymap-global-set "M-o" 'recompile)

;;; MELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; ido
(require 'ido)
(ido-mode 1)
(ido-everywhere 1)

;;; Corfu auto-complete
(use-package corfu
  :init
  (global-corfu-mode)
  (corfu-history-mode))
  (corfu-terminal-mode +1)

;;Dabbrev with Corfu!
(use-package dabbrev                    
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  :config
  (add-to-list 'dabbrev-ignored-buffer-regexps "\\` ")
  (add-to-list 'dabbrev-ignored-buffer-modes 'authinfo-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'doc-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'pdf-view-mode)
  (add-to-list 'dabbrev-ignored-buffer-modes 'tags-table-mode))

(add-to-list
 'load-path
 (replace-regexp-in-string
  "\n" "/share/emacs/site-lisp"
  (shell-command-to-string "opam var prefix")))

;; aesthetics

(load-theme 'doom-flatwhite t)

(load-theme 'doom- t)

(global-hl-line-mode 0)
(set-face-attribute 'default nil :height 165)
(add-to-list 'default-frame-alist
            '(font . "OpenDyslexicM Nerd Font Mono"))

;; Mx437 IBM PS/55 re.
;; OpenDyslexicM Nerd Font Mono
