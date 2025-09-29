;; -*- lexical-binding: t; -*- ;;
;;; Commentary:
;;; Code:
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

;;; #flymake
(use-package flymake
  :ensure t
  :pin gnu
  :config
  (setq flymake-diagnostic-format-alist
        '((t . (origin code message)))))

(setq flymake-mode-line-format '(" ₥"))

;;; cmode
(require 'simpc-mode)
(add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

;; NIX (NIXOS)
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

;;; #org
(custom-set-faces
 '(org-default ((t (:inherit default))))
 '(org-document-title ((t (:inherit bold :height 1.5 :slant italic :overline t))))
 '(org-level-1 ((t (:height 1.4 :slant italic :overline t))))
 '(org-level-2 ((t (:height 1.1 :slant italic :overline t))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(font-lock-add-keywords
 'org-mode
 '(("^ *\\([-]\\) "
    (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

(setq org-hide-leading-stars 't)  
(setq org-hide-emphasis-markers t)
  
;;; #rust
(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;; #ocaml
(require 'tuareg)
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(add-hook 'tuareg-mode-hook (lambda ()
  (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
  (add-hook 'before-save-hook #'ocamlformat-before-save)))

(setq tuareg-prettify-symbols-full t)

(add-hook 'tuareg-mode-hook
          (lambda() (setq tuareg-mode-name "🐫")))

(custom-set-faces '(tuareg-font-double-semicolon-face ((t (:inherit default)))))

(use-package ocamlformat		
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save))
    
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
(keymap-global-set "M-#" 'delete-window)
(keymap-global-set "M-u" 'compile)
(keymap-global-set "M-o" 'recompile)

;;; MELPA packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;(package-initialize)
  
;;; #corfu
(use-package corfu
   :custom
   (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
   (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
   (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
   (corfu-preview-current nil)    ;; Disable current candidate preview
   (corfu-preselect 'prompt)      ;; Preselect the prompt
   (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  :init
  (global-corfu-mode))

;; Enable Vertico.
(use-package vertico
  :custom
  (vertico-scroll-margin 0) ;; Different scroll margin
  ;; (vertico-count 20) ;; Show more candidates
  ;; (vertico-resize t) ;; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle t) ;; Enable cycling for `vertico-next/previous'
  :init
  (vertico-mode))

(defvar +vertico-current-arrow t)

(cl-defmethod vertico--format-candidate :around
  (cand prefix suffix index start &context ((and +vertico-current-arrow
                                                 (not (bound-and-true-p vertico-flat-mode)))
                                            (eql t)))
  (setq cand (cl-call-next-method cand prefix suffix index start))
  (if (bound-and-true-p vertico-grid-mode)
      (if (= vertico--index index)
          (concat #("▶" 0 1 (face vertico-current)) cand)
        (concat #("_" 0 1 (display " ")) cand))
    (if (= vertico--index index)
        (concat
         #(" " 0 1 (display (left-fringe right-triangle vertico-current)))
         cand)
      cand)))  

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Emacs minibuffer configurations.
(use-package emacs
  :custom
  ;; Enable context menu. `vertico-multiform-mode' adds a menu in the minibuffer
  ;; to switch display modes.
  (context-menu-mode t)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.  Vertico
  ;; commands are hidden in normal buffers. This setting is useful beyond
  ;; Vertico.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt
  (minibuffer-prompt-properties
   '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package orderless
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion))))
  (completion-category-defaults nil) ;; Disable defaults, use our settings
  (completion-pcm-leading-wildcard t)) ;; Emacs 31: partial-completion behaves like substring
  
  
;;; #aesthetics
(load-theme 'doric-oak t)
(setq mode-line-compact 'nil)
(setq font-lock-maximum-decoration 't)
(global-prettify-symbols-mode)
(blink-cursor-mode 0)
(tooltip-mode -1)
    
(global-hl-line-mode 1) 

(when (member "APL386 Unicode" (font-family-list))
  (set-face-attribute 'default nil :font "APL386 Unicode"
  :height 200 :weight 'book)
  (set-face-attribute 'fixed-pitch nil :family "APL386 Unicode"))
  
(when (member "OpenDyslexicAlt Nerd Font Propo" (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family "OpenDyslexicAlt Nerd Font Propo"
                      :height 1.0 :weight 'medium))

(add-to-list 'default-frame-alist
             '(font . "APL386 Unicode")
             '(font . "OpenDyslexicAlt Nerd Font Propo"))
  
(provide '.emacs)
;;; .emacs ends here
