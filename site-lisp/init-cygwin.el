;; path
(setq exec-path
      (append
       (list "/bin"
             "/usr/sbin")
       exec-path))
(setenv "PATH"
        (concat 
         "/bin:"
         "/usr/sbin:"
         "/usr/bin:"
         (getenv "PATH")))
;; shell-command
(defadvice shell-command (around shell-command-around)
  (let (
        (coding-system-for-write 'utf-8-dos))
    ad-do-it))
;; copy and paste
(ad-activate 'shell-command)
(when (not (window-system))
  (global-set-key "\C-cw" 'cb-copy)
  (global-set-key "\C-cy" 'cb-paste)
  (defun cb-copy ()
    (interactive)
    (let ((coding-system-for-write 'utf-8-dos))
      ;;(shell-command-on-region (region-beginning) (region-end) "cat > /dev/clipboard" nil nil nil)
      (write-region (region-beginning) (region-end) "/dev/clipboard" nil 'nomessage))
    (kill-ring-save (region-beginning) (region-end))
    (message ""))
  (defun cb-paste ()
    (interactive)
    (let ((coding-system-for-read 'utf-8-dos))
      (goto-char
     (+ (point) (cadr (insert-file-contents "/dev/clipboard")))))))
;;;
;; ediff
;;;
(defadvice ediff (around ediff-around)
  (let (
        (coding-system-for-write 'utf-8-dos))
        ad-do-it))
(ad-activate 'ediff)
;;;
;; shell-mode
;;;
(setq explicit-shell-file-name "bash.exe")
(setq shell-file-name "bash.exe")
;;(setq shell-file-name "f_bash")
(setq shell-command-switch "-c")
;;;;;;;;
;; Additional Lisp Settings
;;;;;;;;
;;;
;; skk
;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/skk"))
(require 'skk-autoloads nil t)
(setq skk-egg-like-newline t)
(setq skk-large-jisyo "~/.emacs.d/leim/SKK-DIC/SKK-JISYO.L")
(global-set-key "\C-x\C-j" 'skk-mode)
;; ▽モードと▼モード時のアンドゥ情報を記録しない
(setq skk-undo-kakutei-word-only t)
;;;;;;;;
;; *-coding-system
;;;;;;;;
(set-language-environment "English")
(prefer-coding-system 'utf-8-dos)
(set-default-coding-systems 'utf-8-dos)
(set-keyboard-coding-system 'utf-8-dos)
(set-clipboard-coding-system 'utf-8-dos)
(set-terminal-coding-system 'utf-8-dos)
(setq file-name-coding-system 'utf-8-dos)
;;;
;; 日本語の設定
;;;
(when (window-system)
  (set-fontset-font (frame-parameter nil 'font)
                    'japanese-jisx0208
                    '("VL Gothic:spacing=22")))

;;;
;; my-utf-8-eaw-fullwidth_cygwin
;;;
(load "my-utf-8-eaw-fullwidth_cygwin")

;;;
;; color theme
;;;
(load-theme 'zenburn t)
(require 'ansi-color)
(setq ansi-color-names-vector ["black" "red" "green" "yellow" "blue" "magenta" "cyan" "white"])
(setq ansi-color-map (ansi-color-make-color-map))
(set-face-attribute 'region nil :inverse-video t)

(provide 'init-cygwin)
;;; init-cygwin.el ends here
