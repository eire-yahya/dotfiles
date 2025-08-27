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

;;;

;;; RUST

(require 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;;; #ocaml

(use-package ocaml-eglot
  :ensure t
  :after tuareg
  :hook
  (tuareg-mode . ocaml-eglot)
  (ocaml-eglot . eglot-ensure)
  (eglot-managed-mode . (lambda () (flycheck-eglot-mode 1)))
  :config
  (setq ocaml-eglot-syntax-checker 'flycheck))
  
(add-hook 'tuareg-mode-hook
            (lambda() (setq tuareg-mode-name "🐫")))
  
(add-hook 'tuareg-mode-hook
  (lambda () (setq tuareg-font-double-semicolon-face '(t (:inherit font-lock-preprocessor-face)))
))

(use-package ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project))
  
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
(keymap-global-set "M-[" 'split-window-below)
(keymap-global-set "M-]" 'split-window-right)
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

;;; flycheck
  
(use-package flycheck
  :ensure t)

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :custom (flycheck-eglot-exclusive t))

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

;;; MEOW

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-colemak)
  (meow-motion-define-key
   ;; Use e to move up, n to move down.
   ;; Since special modes usually use n to move down, we only overwrite e here.
   '("e" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   '("?" . meow-cheatsheet)
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("1" . meow-expand-1)
   '("2" . meow-expand-2)
   '("3" . meow-expand-3)
   '("4" . meow-expand-4)
   '("5" . meow-expand-5)
   '("6" . meow-expand-6)
   '("7" . meow-expand-7)
   '("8" . meow-expand-8)
   '("9" . meow-expand-9)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("/" . meow-visit)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("e" . meow-prev)
   '("E" . meow-prev-expand)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-right)
   '("I" . meow-right-expand)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-line)
   '("L" . meow-goto-line)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("s" . meow-insert)
   '("S" . meow-open-above)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-search)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-delete)
   '("X" . meow-backward-delete)
   '("y" . meow-save)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
  
(require 'meow)
(meow-setup)
(meow-global-mode 1)
(setq meow-expand-hint-counts '(word (0)))
(setq meow-cursor-type-insert '(box))
;; aesthetics

(load-theme 'acme t)
(setq font-lock-maximum-decoration '(1))
(global-hl-line-mode 0)
(set-face-attribute 'default nil :height 220)
(add-to-list 'default-frame-alist
             '(font . "APL386 Unicode"))
  
(provide '.emacs)
;;; .emacs ends here
