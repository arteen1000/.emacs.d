;; useful: delete-blank-lines
;; begin
(setq ns-function-modifier 'hyper)
(global-hl-line-mode)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
;; (setq indent-line-function 'insert-tab)
(setq tramp-auto-save-directory "/tmp")
(global-auto-revert-mode)
(setq use-short-answers t)
(delete-selection-mode)
(setq scroll-error-top-bottom t)
(winner-mode) ;; C-c <left> and C-c <right>
(defun transpose-windows ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))
(windmove-default-keybindings 'control)
(windmove-swap-states-default-keybindings '(shift control))
;; begin hack
(global-set-key (kbd "M-<left>") 'windmove-delete-left)
(global-set-key (kbd "M-<right>") 'windmove-delete-right)
(global-set-key (kbd "M-<up>") 'windmove-delete-up)
(global-set-key (kbd "M-<down>") 'windmove-delete-down)
;; end hack
(defun connect-hexaconta ()
  "let me `ssh' into hexaconta"
  (interactive)
  (dired "/ssh:hexaconta:/home/arteen"))

(defun connect-lug ()
  "let me `ssh' into lug"
  (interactive)
  (dired "/ssh:root@lug:/var/www/html"))
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")
              (seq bol "." (not (any ".")))
              (seq "~" eol))))
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "b" 'dired-create-empty-file)
     (define-key dired-mode-map "e" 'dired-omit-mode)
     (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)))
;; begin
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "*Help*")
;; end
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 4)
        ("nongnu" . 3)
        ("melpa-stable" . 2)
        ("melpa" . 1)))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-vibrant t)
  (doom-themes-visual-bell-config))
(use-package dired-subtree
  :ensure t)
(use-package dired-sidebar
  :ensure t
  :bind (("H-k" . dired-sidebar-toggle-sidebar)))
(use-package vterm
  :ensure t
  :config
  (setq vterm-timer-delay 0.01))
;; begin
(use-package counsel-projectile
  :ensure t)
(counsel-mode)
(counsel-projectile-mode)
(define-key projectile-mode-map (kbd "H-p") 'projectile-command-map)
(global-set-key "\C-s" 'counsel-grep-or-swiper)
(global-set-key "\C-r" 'counsel-grep-or-swiper)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(define-key ivy-minibuffer-map "\C-r" 'ivy-previous-line-or-history)
(define-key ivy-minibuffer-map "\C-s" 'ivy-next-line-or-history)
(add-to-list 'ivy-sort-matches-functions-alist '(counsel-find-file . ivy-sort-function-buffer))
(add-to-list 'ivy-sort-matches-functions-alist '(counsel-switch-buffer . ivy-sort-function-buffer))
(add-to-list 'ivy-sort-matches-functions-alist '(counsel-projectile-find-file . ivy-sort-function-buffer))
(add-to-list 'ivy-sort-matches-functions-alist '(counsel-M-x . ivy-sort-function-buffer))
(setq ivy-height-alist
      '((t
         lambda (_caller)
         (/ (frame-height) 2))))
(add-to-list 'ivy-height-alist (cons 'counsel-find-file
                                     (lambda (_caller)
                                       (/ (frame-height) 3))))
(setq counsel-find-file-ignore-regexp "\\`[#.]\\|[#~]\\'")
(setq ivy-use-selectable-prompt t)
;; begin
(add-to-list 'load-path "~/.emacs.d/my-packages/llvm")
(add-to-list 'load-path "~/.emacs.d/my-packages/clang-format-lite")
(require 'tablegen-mode)
;; end
;; end
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(package-lint flycheck treesit-auto dired-subtree use-package doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; begin
(setq treesit-font-lock-level 4)
;; end
;; begin
(electric-indent-mode 0)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)
;; end
;; begin
(require 'clang-format-lite)
(add-hook 'c++-mode-hook #'clang-format-save-hook)
;; end
(use-package flycheck
  :ensure t)
(use-package package-lint
  :ensure t)
