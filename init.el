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

;; restore window configuration, window history
;; allow to undo frame window-split changes with C-c <left> and re-do with C-c <right>
(winner-mode) 



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
   '(pdf-tools latex-preview-pane utop tuareg dired-sidebar dired-subtree expand-region doom-themes))
 '(send-mail-function 'mailclient-send-it)
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

(global-set-key (kbd "H-w") 'window-swap-states)

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


(setq dired-omit-files
    (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
        (seq bol "." (not (any "."))) ;; dot-files
        (seq "~" eol)                 ;; backup-files
        )))

(setq treesit-language-source-alist ;; set up treesit grammars
      '(
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (java "https://github.com/tree-sitter/tree-sitter-java")
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
        (c++-mode . c++-ts-mode)
        (python-mode . python-ts-mode)
        (java-mode . java-ts-mode)
        )
      )

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package dired-subtree
  :ensure t)

(eval-after-load "dired"
  '(progn
	 (define-key dired-mode-map "b" 'dired-create-empty-file)
     (define-key dired-mode-map "e" 'dired-omit-mode)
     (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)
     (define-key dired-mode-map (kbd "H-f") 'find-name-dired)
     (define-key dired-mode-map (kbd "H-g") 'find-grep-dired)
	 )
  )

(add-to-list 'auto-mode-alist '("\\.sm\\'" . c++-mode))

(setq c-basic-offset 2)
(setq c-ts-mode-indent-offset 2)

(setq java-ts-mode-indent-offset 2)

(use-package dired-sidebar
  :ensure t
  :bind (("H-k" . dired-sidebar-toggle-sidebar))
  )

;; Ocaml
(use-package tuareg
  :ensure t
  :mode (("\\.ocamlinit\\'" . tuareg-mode)))

(use-package utop
  :ensure t)

(setq utop-command "opam exec -- utop -emacs")

(add-to-list 'same-window-buffer-names "*utop*")

(use-package latex-preview-pane
  :ensure t)

(latex-preview-pane-enable)
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

(use-package pdf-tools
  :ensure t)

(pdf-tools-install) 
(pdf-loader-install)

(setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))

;; Prolog vs. Perl
(add-to-list 'auto-mode-alist '("\\.\\(pl\\|pro\\|lgt\\)" . prolog-mode))
