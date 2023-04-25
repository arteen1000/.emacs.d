;; bind hyper key

(setq ns-function-modifier 'hyper)

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
 '(package-selected-packages '(markdown-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; the next lines are intended to make 'man-mode' easier to use

(global-set-key (kbd "H-m s") 'man)

(defun man-other-window ()
  "open a `man' in a new window."
  (interactive)
  (let ((Man-notify-method 'aggressive))
    (call-interactively 'man)))

(global-set-key (kbd "H-m o") 'man-other-window)

;; the next few are for eshell

(defun eshell-other-window ()
 "Open a `shell' in a new window."
 (interactive)
 (let ((buf (eshell)))
   (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(global-set-key (kbd "H-s o") 'eshell-other-window)
(global-set-key (kbd "H-s H-s") 'eshell)

;; make my filing life easier

(global-set-key (kbd "H-f s") 'find-file)
(global-set-key (kbd "H-f o") 'find-file-other-window)

;; fix alias warning since ls doesn't support --dired

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))


;; fix M-x man RET [man-number] command-name RET slowness...

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

