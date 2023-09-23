;; --:**- early-init.el --
;; Code:

;; these affect the GUI and the way it looks
;; and cause an unpleasant hang (and a white flash,
;; if done before the theme is initialized) which
;; is unpleasant for the soul

;; we do them before the GUI is initialized

(tool-bar-mode -1) ;; get rid of top level toolbar
(scroll-bar-mode -1) ;; get rid of scroll bars
