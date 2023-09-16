
;; nextstep fn (macOS) to hyper
(setq ns-function-modifier 'hyper)

;; view mode on when read-only

(setq view-read-only t)

;; want to see column number

(setq column-number-mode t)

;; the next two lines make tabs into 4 spaces

(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; the following series of lines was created with M-x customize-...

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(Man-switches "-a")
 '(custom-enabled-themes '(tango-dark))
 '(inhibit-startup-screen t)
 )

;; convenience features

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

;; bind `eshell's
(global-set-key (kbd "H-s o") 'eshell-other-window)
(global-set-key (kbd "H-s H-s") 'eshell)

;; bind `man's
(global-set-key (kbd "H-m o") 'man-other-window)
(global-set-key (kbd "H-m H-m") 'man)


;; fix alias warning since ls doesn't support --dired

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

;; fix man-auto complete slowness

;; (declare-function 'Man-default-man-entry "man")

;; (define-advice man (:around (orig-func &rest args) no-completing-read)
;;   "Inhibit `completing-read'."
;;   (interactive
;;    (list (let* ((default-entry (Man-default-man-entry))
;;                 (input (read-string
;;                         (format-prompt "Manual entry"
;;                                        (and (not (equal default-entry ""))
;;                                             default-entry))
;;                         nil 'Man-topic-history default-entry)))
;;            (if (string= input "")
;;                (error "No man args given")
;;              input))))
;;   (apply orig-func args))

;; ;; Undo with:
;; (advice-remove 'man 'man@no-completing-read)

(defun connect-hexaconta ()
  "let me `ssh' into hexaconta"
  (interactive)
  (dired "/ssh:hexaconta:/home/arteen"))

;; (defun connect-cs111 ()
;;   "let me `ssh' into cs111 VM"
;;   (interactive)
;;   (dired "/ssh:cs111@localhost#2222:/home/cs111/Desktop/cs111-assignments/lab4/lab4"))

(defun connect-lug ()
  "let me `ssh' into lug"
  (interactive)
  (dired "/ssh:root@lug:/var/www/html"))

;; set-up gpg
;; (require 'epa-file)
;; (epa-file-enable)

;; tramp is annoying
(setq tramp-auto-save-directory "/tmp")

;; don't pop windows up without my permission
;; (setq pop-up-windows nil)

;; (setq switch-to-buffer-obey-display-actions t)
;; (add-to-list 'display-buffer-alist '("\\*grep\\*.*" display-buffer-reuse-window))

;; dired or so god help me

(eval-after-load "dired"
  '(progn
	 (define-key dired-mode-map "c" 'dired-create-empty-file)
	 (define-key dired-mode-map "r" 'dired-do-compress-to)
	 )
  )

;; god help me

;; (require 'package)
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; because i'm too lazy to install tree-sitter
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

;; (setq native-comp-async-report-warnings-errors 'silent)
