
;; nextstep fn (macOS) to hyper
(setq ns-function-modifier 'hyper)

;; view mode on when read-only

(setq view-read-only t)

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
 '(package-selected-packages '(neotree rainbow-delimiters)))

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

;; fix alias warning since ls doesn't support --dired

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

;; fix man-auto complete slowness

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

;; (defun connect-cs111 ()
;;   "let me `ssh' into cs111 VM"
;;   (interactive)
;;   (dired "/ssh:cs111@localhost#2222:/home/cs111/Desktop/cs111-assignments/lab4/lab4"))

(defun connect-lug ()
  "let me `ssh' into lug"
  (interactive)
  (dired "/ssh:lug:/"))

;; set-up gpg
;; (require 'epa-file)
;; (epa-file-enable)

;; for rainbow delims when programming
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; backup files go away but not completely
(setq backup-directory-alist '(("." . "~/.emacs.d/my-backups")))

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

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; amazing
(require 'neotree)

;; stays where it was
(global-set-key (kbd "H-k") 'neotree-toggle)

;; comes here

(global-set-key (kbd "H-m") 'neotree-find)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; what if I wanna see my hidden files?

(global-set-key (kbd "H-t") 'neotree-hidden-file-toggle)

;; try a fix for grep ...

(defun my-compile-goto-error-same-window ()
  (interactive)
  (let ((display-buffer-overriding-action
         '((display-buffer-reuse-window
            display-buffer-same-window)
           (inhibit-same-window . nil))))
    (call-interactively #'compile-goto-error)))

(defun my-compilation-mode-hook ()
  (local-set-key (kbd "<return>") #'my-compile-goto-error-same-window))

(add-hook 'compilation-mode-hook #'my-compilation-mode-hook)

;; let us try

(customize-set-variable 'display-buffer-base-action
                        '((display-buffer-reuse-window display-buffer-same-window)
                          (reusable-frames . t)))
