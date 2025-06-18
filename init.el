;; useful: delete-blank-lines -- C-x C-o -- like M-<space> for lines
;; begin
(setq ns-function-modifier 'hyper)
(global-hl-line-mode)
(electric-indent-mode 0)
(add-hook 'prog-mode-hook #'electric-indent-local-mode)
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq column-number-mode t)
(setq-default indent-tabs-mode nil) ; no tabs
(setq-default tab-width 2)
(setq tramp-auto-save-directory "/tmp")
(setf kill-buffer-delete-auto-save-files t)
(setq use-short-answers t)
(delete-selection-mode) ; paste over text
(setq scroll-error-top-bottom t) ; move to top/bot before error'ing
(winner-mode) ;; C-c <left> and C-c <right>
(defun rotate-windows (arg)
  "Rotate your windows; use the prefix argument to rotate the other direction"
  (interactive "P")
  (if (not (> (count-windows) 1))
      (message "You can't rotate a single window!")
    (let* ((rotate-times (prefix-numeric-value arg))
           (direction (if (or (< rotate-times 0) (equal arg '(4)))
                          'identity 'reverse))) ; clockwise default
      (dotimes (_ (abs rotate-times))
        (dotimes (i (- (count-windows) 1))
          (let* ((w1 (elt (funcall direction (window-list)) i))
                 (w2 (elt (funcall direction (window-list)) (+ i 1)))
                 (b1 (window-buffer w1))
                 (b2 (window-buffer w2))
                 (s1 (window-start w1))
                 (s2 (window-start w2))
                 (p1 (window-point w1))
                 (p2 (window-point w2)))
            (set-window-buffer-start-and-point w1 b2 s2 p2)
            (set-window-buffer-start-and-point w2 b1 s1 p1)))))))
(global-set-key (kbd "H-r") #'rotate-windows)
(windmove-default-keybindings 'control) ; move cursor
(windmove-swap-states-default-keybindings '(shift control)) ; swap the states
(windmove-delete-default-keybindings 'none 'meta) ; delete
(defun transpose-windows ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))
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
     (define-key dired-mode-map "e" 'dired-omit-mode)))
;; begin
(add-to-list 'same-window-buffer-names "*Buffer List*")
(add-to-list 'same-window-buffer-names "*Help*")
;; end
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; begin
(add-hook 'c++-mode-hook (lambda ()
                           (setq-local compile-command
                                       (format "g++ --std=c++17 %s" (shell-quote-argument (file-name-nondirectory buffer-file-name))))
                           (define-key c++-mode-map (kbd "H-k") 'compile)
                           ))

;; end
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
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
   (exec-path-from-shell-initialize)))
(use-package clang-format-lite
  :ensure t
  :config
  (add-hook 'c++-mode-hook #'clang-format-lite-save-hook))
(use-package python-black
  :ensure t
  :after python
  :hook (python-mode . python-black-on-save-mode)) ; -enable-dwim for only in black project
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(go-mode latex-preview-pane clang-format-lite exec-path-from-shell doom-themes))
 '(shell-escape-mode "-shell-escape"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable)
  )
(use-package go-mode
  :ensure t)
(setq create-lockfiles nil)
