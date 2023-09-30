;; --:**- init.el --  -*- lexical-binding: t; -*-
;; Code:

;; Note: git-timemachine may be useful at some point

(setq ns-function-modifier 'hyper) ;; nextstep fn (macOS) to hyper
(setq view-read-only t) ;; view mode on when read-only
(setq column-number-mode t) ;; want to see column number
(setq-default indent-tabs-mode nil) ;; indentation can't insert tabs
(setq-default tab-width 4) ;; make tab-width 4 (spaces)
(setq tramp-auto-save-directory "/tmp")
(global-auto-revert-mode) ;; refresh buffer if file changed on disk
;; (fset 'yes-or-no-p 'y-or-n-p) ;; set yes/no prompt to y/n
(setq use-short-answers t) ;; cleaner way to do the above, affects more funcs too
;; (visual-line-mode) ;; make it work on "visual" lines instead of logical
(delete-selection-mode) ;; if I have a region active and I type, get rid of the region
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(setq scroll-error-top-bottom t) ;; if my movement makes me go past end of buf (or beg), just take me there, instead of signalling error straight away


(winner-mode) ;; allow to undo frame window-split changes with C-c <left> and re-do with C-c <right>



;; makes consistent with shell
;; but this messes with mark being set before things like M->
;; (defadvice kill-region (before unix-werase activate compile)
;;   "When called interactively with no active region, delete a single word backwards instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (save-excursion (backward-word 1) (point)) (point)))))

(setq native-comp-async-report-warnings-errors 'silent)
(add-hook 'prog-mode-hook #'hs-minor-mode) ;; code folding
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(Man-switches "-a")
 '(inhibit-startup-screen t)
 '(package-native-compile t)
 '(package-selected-packages
   '(expand-region all-the-icons-dired all-the-icons dired-sidebar dired-subtree doom-themes))
 '(treesit-font-lock-level 4))

(defun man-other-window ()
  "open a `man' in a new window."
  (interactive)
  (let ((buf (call-interactively 'man)))
	(switch-to-buffer (other-buffer buf))
	(switch-to-buffer-other-window buf)))

(defun eshell-other-window ()
 "Open an `eshell' in a new window."
 (interactive)
 (let ((buf (eshell)))
   (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(global-set-key (kbd "H-s o") 'eshell-other-window)
(global-set-key (kbd "H-s H-s") 'eshell)

(global-set-key (kbd "H-m o") 'man-other-window)
(global-set-key (kbd "H-m H-m") 'man)

(global-set-key (kbd "H-o") 'occur) ;; grep-like find lines action within same file

(defun toggle-window-split ()
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

;; (add-to-list 'load-path "~/.emacs.d/orphan-packages/")
;; (require 'transpose-frame)
(global-set-key (kbd "H-t") 'toggle-window-split)

;; want an easy way to move between windows
;; firstly, remove C-<arrow keys> keybindings for macOS
;; Settings -> Keyboard Shortcuts -> Mission Control
;; rebind to use <CMD> key instead, then apply this
;; to let us navigate windows with C-<arrow keys>
;; notably: the UP and DOWN keys are bound to "Mission Control"
;; and "Applications Windows"
(windmove-default-keybindings 'control)

;; now, these allow us to swap our window states (buffers)
(windmove-swap-states-default-keybindings '(shift control))

;; and finally, (this overrides previous M-<up> and M-<down>
;; but it's fine, because equivalent functionality for left-to-right
;; languages is present in M-f and M-b
(windmove-delete-default-keybindings 'none 'meta)


;; this will delete surrounding newlines
(global-set-key (kbd "C-S-<backspace>") 'delete-blank-lines)


(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil)) ;; darwin ls doesn't support --dired

;; (fix?) man auto-complete slowness on MacOS

(declare-function 'Man-default-man-entry "man")

(define-advice man (:around (orig-func &rest args) no-completing-read)
  "Inhibit `completing-read'."
  (interactive
   (list (let* ((default-entry (Man-default-man-entry))
                (input (read-string
                        (format-prompt "Manual entry"
                                       (and (not (equal default-entry ""))
                                            default-entry))
                        nil 'Man-topic-history default-entry)))
           (if (string= input "")
               (error "No man args given")
             input))))
  (apply orig-func args))

;; Undo with:
;; (advice-remove 'man 'man@no-completing-read)

(defun connect-hexaconta ()
  "let me `ssh' into hexaconta"
  (interactive)
  (dired "/ssh:hexaconta:/home/arteen"))

(defun connect-lug ()
  "let me `ssh' into lug"
  (interactive)
  (dired "/ssh:root@lug:/var/www/html"))

;; initialize package sources
(require 'package)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("gnu" . 4)
        ("nongnu" . 3)
        ("melpa-stable" . 2)
        ("melpa" . 1))
      )

;; make sure that environment variables correctly inherited from shell on MacOS
;; even when not started from inside the shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  )

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package dired-subtree
        :ensure t
        :bind (:map dired-mode-map
                    ("<tab>" . dired-subtree-toggle)
                    ("<backtab>" . dired-subtree-cycle)))

;; M-x all-the-icons-install-fonts

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :if (display-graphic-p)
  :hook (dired-mode . (lambda ()
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))
                    )
  )

(use-package dired-sidebar
  :bind (("H-k" . dired-sidebar-toggle-sidebar))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar)
  :config
  (advice-add 'dired-subtree-toggle :after (lambda () ;; else icons don't show
                                             (when all-the-icons-dired-mode
                                               (revert-buffer)))))

(setq dired-omit-files
    (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
        (seq bol "." (not (any "."))) ;; dot-files
        (seq "~" eol)                 ;; backup-files
        )))

(eval-after-load "dired"
  '(progn
	 (define-key dired-mode-map "b" 'dired-create-empty-file)
     (define-key dired-mode-map "e" 'dired-omit-mode)
	 )
  )

(setq treesit-language-source-alist ;; set up treesit grammars
      '(
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        )
      )

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist)) ;; run this to install grammars

(setq major-mode-remap-alist ;; a bit hacky, for treesit modes
      '((css-mode . css-ts-mode)
        (javascript-mode . js-ts-mode)
        (js-json-mode . json-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)
        )
      )

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(defun advice--all-the-icons-dired--icon (file)
  "Return the icon for FILE."
  (if (file-directory-p file)
      (all-the-icons-material "folder"
                              :face 'all-the-icons-dired-dir-face
                              :v-adjust all-the-icons-dired-v-adjust)
    (apply (car all-the-icons-default-file-icon) (cdr all-the-icons-default-file-icon))))

(advice-add 'all-the-icons-dired--icon :override #'advice--all-the-icons-dired--icon)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Old:

;; (defun connect-cs111 ()
;;   "let me `ssh' into cs111 VM"
;;   (interactive)
;;   (dired "/ssh:cs111@localhost#2222:/home/cs111/Desktop/cs111-assignments/lab4/lab4"))

;; (require 'epa-file) ;; hot-fix, no longer needed
;; (epa-file-enable)

;; don't pop windows up without my permission
;; (setq pop-up-windows nil)

;; (setq switch-to-buffer-obey-display-actions t)
;; (add-to-list 'display-buffer-alist '("\\*grep\\*.*" display-buffer-reuse-window))

;; (use-package rainbow-delimiters
;;   :ensure t)

;; for rainbow delims when programming
;; (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; try a fix for grep ...

;; (defun my-compile-goto-error-same-window ()
;;   (interactive)
;;   (let ((display-buffer-overriding-action
;;          '((display-buffer-reuse-window
;;             display-buffer-same-window)
;;            (inhibit-same-window . nil))))
;;     (call-interactively #'compile-goto-error)))

;; (defun my-compilation-mode-hook ()
;;   (local-set-key (kbd "<return>") #'my-compile-goto-error-same-window))

;; (add-hook 'compilation-mode-hook #'my-compilation-mode-hook)

;; i really don't want you to mess with my windows

;; (customize-set-variable 'display-buffer-base-action
;;                         '((display-buffer-reuse-window display-buffer-same-window)
;;                           (reusable-frames . t)))

;; hello
;; (use-package dired-subtree
;;         :ensure t
;;         :bind (:map dired-mode-map
;;                     ("i" . dired-subtree-insert)
;;                     (";" . dired-subtree-remove)
;;                     ("<tab>" . dired-subtree-toggle)
;;                     ("<backtab>" . dired-subtree-cycle)))
