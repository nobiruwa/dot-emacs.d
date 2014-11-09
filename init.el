;;; init.el --- my Emacs settings
;;; Commentary:
;;;  My Emacs settings.
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; グローバルな設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; ファイルの扱い
;;;;;;;;
;; load-path
(setq load-path (append (list "~/.emacs.d/site-lisp") load-path))
;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)

;;;;;;;;
;; 初期化
;;;;;;;;
;; 起動画面がいつまでも消えてくれないのではなから使わない
(setq inhibit-startup-message t)
;; emacsclient
(server-start)
;; デバッグをする場合はt
(setq debug-on-error nil)

;;;;;;;;
;; キーの設定
;;;;;;;;
;; C-zを無効にする
(global-set-key "\C-z" nil)
;; C-h キーでカーソルの左の文字が消えるようにする。
(global-set-key "\C-h" 'backward-delete-char)
;; Ref: http://q.hatena.ne.jp/1137478760 の回答20
;; ミニバッファ内でC-wで単語削除です。上位パスのファイルを選択する際に便利です。
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)
;; xclip.elの代替
;; Ref: http://garin.jp/doc/unix/xwindow_clipboard
(defun my-cut-function (text)
  "Copy TEXT to clipboard selection of X Window System.
TEXT should be UTF-8"
  (interactive)
  (let ((process-connection-type nil)
        (coding-system-for-write 'utf-8))
    (let ((proc (start-process "xclip" "*Messages*" "xclip" "-selection" "clipboard")))
      (process-send-string proc text)
      (process-send-eof proc))))
;; pasteをセットすると、yank時に同内容のテキストが2つずつ入っているように見える
;; pasteはShift-Insertで行えばよいのでnilとする
(when (and (not window-system) (not (eq system-type 'cygwin))
         (executable-find "xclip"))
  (setq interprogram-cut-function 'my-cut-function)
  (setq interprogram-paste-function nil))

;;;;;;;;
;; タブと空白の設定
;;;;;;;;
;; 8-character tab length is too long. set 4 character.
;; タブキーを押したときのインデント幅は
;; M-x edit-tab-stopsで設定
;; tab-stop-listを直接編集してもOK
(setq-default tab-width 4)
(setq tab-stop-list
'(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76))
;; 字下げをタブではなく空白にする
(setq-default indent-tabs-mode nil)

;;;;;;;;
;; 日本語表示の設定
;;;;;;;;
;; 日本語 info が文字化けしないように
(auto-compression-mode t)
;; 日本語 grep
(if (file-exists-p "/usr/bin/lgrep")
    (setq grep-command "lgrep -n "))
;;;;;;;;
;;settings for utf-8
;;;;;;;;
;; Ref: http://forum.ubuntulinux.jp/viewtopic.php?pid=909#p909
(set-language-environment "English")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(setq locale-coding-system 'utf-8)
(setq file-name-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))
;; my-utf-8-eaw-fullwidth.el
(if (eq system-type 'cygwin)
    (load "my-utf-8-eaw-fullwidth_cygwin")
    (load "my-utf-8-eaw-fullwidth"))

;;;;;;;;;;;;;; Emacs 標準Lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; all CC Mode modes
;;;;;;;;
(add-hook 'c-mode-common-hook (lambda ()
   (c-set-style "bsd")
   (setq c-basic-offset 4)
   (setq indent-tabs-mode nil)))

;;;;;;;;
;; C++ mode (c++-mode)
;;;;;;;;
(add-hook 'c++-mode-hook '(lambda ()
                            (setq indent-tabs-mode nil)
                            (setq c-basic-offset 4)))

;;;;;;;;
;; ediff
;;;;;;;;
(require 'ediff)
;; This is what you probably want if you are using a tiling window
;; manager under X, such as ratpoison.
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; This will split the window horizontally if the frame is wider than 150 chars and vertically otherwise.
(setq ediff-split-window-function (if (> (frame-width) 150)
                                      'split-window-horizontally
                                    'split-window-vertically))

;;;;;;;;
;; ido-mode
;;;;;;;;
(require 'ido)
(ido-mode 1)
(setq ido-auto-merge-work-directories-length -1)

;;;;;;;;
;; java-mode
;;;;;;;;
(add-hook 'java-mode-hook '(lambda ()
                             (setq indent-tabs-mode nil)
                             (setq c-basic-offset 4)))

;;;;;;;;
;; mouse, mwheel
;;;;;;;;
(require 'mouse)
(require 'mwheel)
(if (eq system-type 'gnu/linux)
    (xterm-mouse-mode 1))
(mouse-wheel-mode t)

;;;;;;;;
;; org-mode
;;;;;;;;
(require 'org)
(setq org-startup-folded nil)
(setq org-startup-truncated nil)

;;;;;;;;
;; package.el (MELPA)
;;;;;;;;
;; (See also: https://github.com/milkypostman/melpa#usage)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;;;;;;;;
;; shell-mode
;;;;;;;;
;; shell-modeの拡張
;; lsなどの色の設定
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color"
;;  "Set `ansi-color-for-comint-mode' to t." t)
;;(setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "navy" "magenta3" "cyan3" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;;;;;;;
;; uniquify
;; Ref: http://q.hatena.ne.jp/1137478760 の回答24
;; a/index.html と b/index.html をひらいたときに、
;; バッファ名を index.html<a>, index.html<b> としてくれます。
;; デフォルトの連番だとどれがどれだかわからなくなるので。
;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;;;;;;;;;;;; 関数宣言 ;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;
;; my-insert-file-name
;; Emacs Wiki: Insert File Name
;; http://www.emacswiki.org/emacs-ja/InsertFileName
;; Running
;;   M-x my-insert-file-name ~/.emacs RET
;; will insert the file name as it appears in the MiniBuffer.
;; If you want the full path to the file, you can have it "expanded"
;; with a PrefixArgument.
;; Running
;;   C-u M-x my-insert-file-name ~/.emacs RET
;; will insert
;;   /home/username/.emacs
;; into the buffer.
;;;;;;;;;;;;
(defun my-insert-file-name (arg filename)
  "If ARG is non nil, insert name of file FILENAME into buffer after point.
Set mark after the inserted text.

Prefixed with \\[universal-argument], expand the file name to
its fully canocalized path.

See `expand-file-name'."
  ;; Based on insert-file in Emacs -- ashawley 2008-09-26
  (interactive "*P\nfInsert file name: ")
  (if arg
      (insert (expand-file-name filename))
    (insert filename)))
;;;
;; previous-lineのオーバーライド
;;;
;; バッファの最初の行で previous-line しても、
;; "beginning-of-buffer" と注意されないようにする。
;; http://www.bookshelf.jp/2ch/unix/1001393679.html のNo. 8
(defun previous-line (arg)
  (interactive "p")
  (if (interactive-p)
      (condition-case nil
          (line-move (- arg))
        ((beginning-of-buffer end-of-buffer)))
    (line-move (- arg)))
  nil)

;;;;;;;;;;;;; 以下、ELispファイルを追加する必要があるものを設定 ;;;;;;
;;;;;;;;;;;;; アルファベット順になるよう努力 ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; auto-complete
;; URL: http://cx4a.org/software/auto-complete/
;;;
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(ac-config-default)

;; auto-complete-modeが有効なバッファでのキーバインド
;;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
(define-key ac-mode-map (kbd "C-c C-i") 'auto-complete)
;; 自動で補完しない
(setq ac-auto-start nil)
;; yasnippetを情報源に追加する
(add-to-list 'ac-sources 'ac-source-yasnippet)

;; 補完メニュー表示時のキーマップ
(setq ac-use-menu-map t)
;; Enter, C-m で補完を終了させる
;; 改行をさせない
;;(define-key ac-menu-map [return] 'ac-complete)

;; html-modeでac-modeを有効にする
(add-to-list 'ac-modes 'html-mode)
;; js2-modeでac-modeを有効にする
(add-to-list 'ac-modes 'js2-mode)

;; 全てのバッファーでauto-complete modeを有効にする
(global-auto-complete-mode t)

;;;
;; bash-completion
;;;
;; (autoload 'bash-completion-dynamic-complete "bash-completion"
;;   "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions
;;           'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions
;;           'bash-completion-dynamic-complete)

;;;
;; cedet, ede, semantic, etc.
;; Ref: Emacs Part 31
;; URL: http://pc12.2ch.net/test/read.cgi/unix/1251665639/312
;;;
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component (including
;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
(load-file (expand-file-name "~/repo/cedet.git/cedet-devel-load.el"))
;; DBファイルを一ヶ所に集約
(setq semanticdb-default-save-directory "~/.emacs.d/semantic")
;; disable semantic-mode and global-*-mode in CEDET
;; CEDET conflicts js2-mode, python-mode
(semantic-mode -1)

;;;
;; emacs-eclim
;; auto-completeの後に読み込む
;;;
;; シンボリックリンクを開いた場合、名前を解決する
(setq-default find-file-visit-truename t)

(require 'eclim)
(setq eclim-auto-save t)
(setq eclim-executable "~/opt/eclipse/eclim")
(global-eclim-mode 1)

;;(setq help-at-pt-display-when-idle t)
;;(setq help-at-pt-timer-delay 0.9)
;;(help-at-pt-set-timer)

;; configuration for auto-complete
(require 'ac-emacs-eclim-source)
(ac-emacs-eclim-config)
;; M-TAB での補完に eclim-complete ではなく auto-complete を用いる
;;(define-key eclim-mode-map (kbd "M-TAB") 'auto-complete)
(define-key eclim-mode-map (kbd "C-c C-i") 'auto-complete)
;; define eclimd start function
(defun eclim-start-eclimd (workspace)
  (interactive (list (read-directory-name "workspace: "
                                          "~/workspace/")))
  (let* ((eclimd-executable (expand-file-name "~/opt/eclipse/eclimd"))
        (eclimd-workspace-option (mapconcat 'identity (list "-Dosgi.instance.area.default=\"" (expand-file-name workspace) "\"") ""))
        (command (mapconcat 'identity (list eclimd-executable eclimd-workspace-option "&") " ")))
    ;; body
    (if workspace
        (shell-command command))))
;; define eclimd shutdown function
(defun eclim-shutdown-eclimd ()
  (interactive)
  (let* ((eclimd-executable (expand-file-name "~/opt/eclipse/eclim"))
        (command (mapconcat 'identity (list eclimd-executable "-command" "shutdown") " ")))
    ;; body
    (shell-command command)))

;;;;;;;;
;; GCL GNU Common Lisp
;;;;;;;;
(setq inferior-lisp-program "/usr/bin/gcl")

;;;
;; emacs-jedi
;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;;;
;;(add-to-list 'load-path "~/repo/emacs-jedi.git")
(require 'python-environment)
(require 'jedi)
(setq jedi:environment-virtualenv
      (list "virtualenv" "--system-site-packages"))
(defun jedi:install-server2 ()
  (interactive)
  (let ((python-environment-virtualenv (list "virtualenv" "--system-site-packages" "--quiet" "--python" "/usr/bin/python2.7"))
        (jedi:environment-virtualenv (list "virtualenv" "--system-site-packages" "--python" "/usr/bin/python2.7"))
        (jedi:environment-root "python2.7"))
    (jedi:install-server)))
(defun jedi:start-dedicated-server2 ()
  (interactive)
  (let* ((cmds '("~/.emacs.d/.python-environments/python2.7/bin/jediepcserver"))
        (args '("--sys-path" "/usr/lib/python2.7/dist-packages")))
    (when cmds (set (make-local-variable 'jedi:server-command) cmds))
    (when args (set (make-local-variable 'jedi:server-args) args))
    (setq jedi:epc nil)
    (jedi:start-server)))
;;(setq jedi:key-complete (kbd "<M-tab>"))
(setq jedi:key-complete (kbd "C-c C-i"))
(setq jedi:key-goto-definition (kbd "C-c ."))
(setq jedi:key-show-doc (kbd "C-c d"))
(setq jedi:key-related-names (kbd "C-c r"))
(setq jedi:goto-definition-pop-marker (kbd "C-c ,"))
(setq jedi:setup-keys t)
(setq jedi:get-in-function-call-delay 200)
(setq jedi:complete-on-dot t)
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)

;;;;;;;;
;;  emmet-mode
;;;;;;;;
(eval-after-load "emmet-mode"
  '(progn
     (message "[emmet] redefine emmet-preview-accpet")
     (defun emmet-preview-accept
       ()
       "Original emmet-preview-accept does not work.
Temporarily, bind expr to the return value of emmet-expr-on-line."
       (interactive)
       (let ((ovli emmet-preview-input))
         (if (not (and (overlayp ovli)
                       (bufferp (overlay-buffer ovli))))
             (message "Preview is not active")
           (let* ((indent (current-indentation))
                  (markup (emmet-preview-transformed indent))
                  (expr (emmet-expr-on-line)))
             (when markup
               (delete-region (overlay-start ovli) (overlay-end ovli))
               (emmet-insert-and-flash markup)
               (emmet-reposition-cursor expr)))))
       (emmet-preview-abort))
     ))

(add-hook 'html-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'emmet-mode-hook
          (lambda ()
            (setq emmet-insert-flash-time 0.001)
            (define-key emmet-mode-keymap (kbd "C-j") nil)
            (define-key emmet-mode-keymap (kbd "<C-return>") nil)
            (define-key emmet-mode-keymap (kbd "C-c C-v") 'emmet-expand-line)))

;;;;;;;;
;; flycheck-mode
;;;;;;;;
(setq flycheck-flake8-maximum-complexity 10)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;
;; hilight-indentation
;;;;;;;;
(require 'highlight-indentation)
(add-hook 'python-mode-hook
          (lambda ()
            "turn on highlight-indentation-mode"
            (highlight-indentation-mode 1)))

;;;;;;;;
;; howm
;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/.emacs.d/howm/site-lisp"))
;; キーの再割り当て
(setq howm-prefix "\C-z,")
;; howm開始
(global-set-key "\C-z,," 'howm-menu)
;; メニューの言語
(setq howm-menu-lang 'en)
;; ロード
(require 'howm)
;;ファイルの関連付け
(setq howm-view-external-viewer-assoc
      '(("\.\\(pdf\\)$"      . "evince %s")
        ("\.\\(ps\\|eps\\)$" .   "gv %s")))
;;howmのスケジュールは2週間分
(setq howm-menu-schedule-days 14)
;;howmのToDoの表示個数
(setq howm-menu-todo-num 15)
;; ファイル名で逆順ソート（デフォルトはファイルの mtime）
;; http://slashdot.jp/journal.pl?op=display&id=254132&uid=3793
(setq howm-normalizer 'howm-sort-items-by-reverse-date)
;; ToDo リストで「.」が新しい順に並んでほしい. また, 
;;「[2008-02-15]． [2008-02-15]:!」は ToDo リストに出したくない. -- 2ch3:443-446n
;;; Ref: http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?ReverseDoneReminder
;; ToDo リストで「.」が新しい順に並んでほしい. 
(defadvice howm-todo-priority-done 
  (around reverse-order (late lz item) activate) 
  (setq late (- late)) ad-do-it) 
;; しかもメニューに「.」が表示されてほしい. 
(defvar howm-huge++ 99999) 
(setq howm-menu-todo-priority (- howm-huge++))

;; Not to treat menu as memo
;; after mv ~/howm/0000-00-00-000000.howm ~/hoge/fuga/menu.howm
;; (setq howm-menu-file "~/hoge/fuga/menu.howm")
;; (setq howm-menu-file "~/howm/menu.howm")

;; org-modeとhowmの併用
;; http://howm.sourceforge.jp/cgi-bin/hiki/hiki.org?OrgMode
(add-hook 'org-mode-hook 'howm-mode)
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))

;; howmディレクトリ以下のtxtをhowmモードで開く
(add-to-list 'auto-mode-alist '("howm/.*\\.txt$" . org-mode))
;;(add-to-list 'auto-mode-alist '("howm/.*\\.txt$" . howm-mode))

;; Ref: http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?DateFormat
;入力は通常どおりして、表示のときだけ曜日を表示
(defun howm-add-day-of-week-overlay-region (start end)
  "START と END の範囲内にあるリマインダの日付に曜日を追加する。
曜日は overlay の after-string として追加される。"
  (save-excursion
    (save-restriction
      (narrow-to-region (progn (goto-char start) (line-beginning-position))
                        (progn (goto-char end) (line-end-position)))
      (let ((ovrs (overlays-in (point-min) (point-max))))
        (dolist (ovr ovrs)
          (when (overlay-get ovr 'howm-day-of-week)
            (delete-overlay ovr))))
      (goto-char (point-min))
      (let ((regexp (howm-reminder-regexp howm-reminder-types)))
        (while (re-search-forward regexp nil t)
          (let ((ovr (make-overlay
                      (match-beginning howm-reminder-regexp-day-pos)
                      (match-end howm-reminder-regexp-day-pos)))
                (dow (nth 4 (howm-todo-parse-string (match-string 0)))))
            (overlay-put ovr 'howm-day-of-week t)
            (overlay-put ovr 'after-string
                         (concat " " (howm-day-of-week-string dow)))
            (overlay-put ovr 'evaporate t)))))))
;; jit-lock に登録 -> howm-mode-hookに追加
;; (add-hook 'howm-mode-hook
;;           (lambda ()
;;             (jit-lock-register 'howm-add-day-of-week-overlay-region)))

; (add-hook 'howm-view-summary-mode-hook
;     (lambda ()
;             (jit-lock-register 'howm-add-day-of-week-overlay-region)))

;; C-c C-c 現バッファの内容を保存してバッファを消す
;; Ref: http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?SaveAndKillBuffer
(defun my-save-and-kill-buffer ()
  (interactive)
  (save-buffer)
  (kill-buffer nil))

;; howm-mode-hook
(add-hook 'howm-mode-hook
          (lambda ()
            ;; C-c C-c 現バッファの内容を保存してバッファを消す
            (define-key howm-mode-map "\C-z\C-c\C-c" 'my-save-and-kill-buffer)
            ;; jit-lock に登録
            (jit-lock-register 'howm-add-day-of-week-overlay-region)
            ;; タイトル色
            ;;(set-face-foreground 'howm-mode-title-face "chartreuse")
            ))

;; *.howm を outline-mode に
;; Ref: http://howm.sourceforge.jp/cgi-bin/hiki/hiki.cgi?OutlineMode
;;(add-to-list 'auto-mode-alist '("\\.howm$" . outline-mode))

;;;;;;;;
;; java-mode-indent-annotations.el --- Indentation for Java 5 annotations.
;;;;;;;;
(require 'java-mode-indent-annotations)
(add-hook 'java-mode-hook '(lambda ()
                             (java-mode-indent-annotations-setup)))

;;;;;;;
;; jshint-mode
;;;;;;;
(add-to-list 'load-path (expand-file-name "~/repo/jshint-mode.git"))
(require 'flymake-jshint)
;; 下記の内容のjshint-curlと組み合わせて使う
;; #!/bin/sh
;; FILENAME="$1"
;; JSHINTMODE="$2"
;; JSHINTRC="$3"
;; URL="$4"
;; curl --silent --form source=\<"$FILENAME" --form filename="$FILENAME" --form mode="$JSHINTMODE" --form jshintrc="$JSHINTRC" $URL | grep '\(^Lint at \|No problems \)' | sed -e "s;^\(Lint\);$FILENAME:\1;"
(defun quote-name (name)
  (if (string-match " " name)
      (concat "'" name "'")
    name))
(defun jshint-make-curl-command ()
  (let* ((local-file (quote-name buffer-file-name))
         (jshint-url (format "http://%s:%d/check" jshint-mode-host jshint-mode-port))
         (jshintrc
          (quote-name
           (if (string= "" jshint-mode-jshintrc)
               (expand-file-name ".jshintrc" (locate-dominating-file default-directory ".jshintrc"))
             jshint-mode-jshintrc))))
     (mapconcat 'identity (list "jshint-curl" local-file jshint-mode-mode jshintrc jshint-url) " ")))
(defun jshint-curl ()
  (interactive)
  (shell-command (jshint-make-curl-command)))
(setq jshint-cli "jshint-curl ")
(add-to-list 'compilation-error-regexp-alist-alist
             '(jshint-cli "^\\(.*\\):Lint at line \\([[:digit:]]+\\) character \\([[:digit:]]+\\):" 1 2 3 ))
(add-to-list 'compilation-error-regexp-alist 'jshint-cli)

;;;;;;;;
;; js-mode
;;;;;;;;
(add-hook 'js-mode-hook
     (lambda ()
       (make-local-variable 'compile-command)
       (setq compile-command (jshint-make-curl-command))
       (setq js-indent-level 2)))

;;;;;;;;
;; js2-mode
;; It will refuse to run unless you have byte-compiled it. 
;; You must byte-compile it with your version of Emacs because 
;; different versions of Emacs have different byte-compiled formats. 
;;;;;;;;
;;(add-to-list 'load-path (expand-file-name "~/repo/js2-mode.git"))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
     (lambda ()
       (make-local-variable 'compile-command)
       (setq compile-command (jshint-make-curl-command))
       (setq flymake-gui-warnings-enabled nil)
       (setq js2-basic-offset 2)
       (flymake-mode 1)))

;;;;;;;;
;; navi2ch
;;;;;;;;
(add-to-list 'load-path (expand-file-name "~/repo/navi2ch.git"))
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)

;;;;;;;;
;; omnisharp-mode
;;;;;;;;
(setq omnisharp-server-executable-path (expand-file-name "~/repo/omnisharp-server.git/OmniSharp/bin/Debug/OmniSharp.exe"))
(add-hook 'csharp-mode-hook 'omnisharp-mode)

;;;;;;;;
;; reopen-as-root
;; 下記URLのEmacs Lispをファイルreopen-as-root.elに保存した
;; Ref: http://ubulog.blogspot.com/2010/08/emacs-sudo2.html
;;;;;;;;
(require 'reopen-as-root)
;;;;;;;;
;; reopen-file
;;;;;;;;
;; http://namazu.org/~satoru/diary/?200203c&to=200203272#200203272
;; 編集中のファイルを開き直す
;; - yes/no の確認が不要;;   - revert-buffer は yes/no の確認がうるさい
;; - 「しまった! 」というときにアンドゥで元のバッファの状態に戻れる
;;   - find-alternate-file は開き直したら元のバッファの状態に戻れない
;;
(defun reopen-file ()
  "Reopen file without confirm yes/no."
  (interactive)
  (let ((file-name (buffer-file-name))
        (old-supersession-threat
         (symbol-function 'ask-user-about-supersession-threat))
        (point (point)))
    (when file-name
      (fset 'ask-user-about-supersession-threat (lambda (fn)))
      (unwind-protect
          (progn
            (erase-buffer)
            (insert-file file-name)
            (set-visited-file-modtime)
            (goto-char point))
        (fset 'ask-user-about-supersession-threat
              old-supersession-threat)))))
;; reopen-fileをC-x C-rにバインド
(define-key ctl-x-map "\C-r"  'reopen-file)

;;;;;;;;
;; skk
;;;;;;;;
(setq skk-aux-large-jisyo nil)
;; ▽モードと▼モード時のアンドゥ情報を記録しない
(setq skk-undo-kakutei-word-only t)

;;;;;;;;
;; undo-tree
;;;;;;;;
(require 'undo-tree)
(global-undo-tree-mode)
;; rxvt-unicode detects C-c C-/ as C-c C-_
(define-key undo-tree-map (kbd "C-c C-/") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-c C-_") 'undo-tree-redo)

;;;;;;;;
;; wdired
;;;;;;;;;
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)

;;;;;;;;
;; web-mode
;;;;;;;;
;;(add-to-list 'load-path (expand-file-name "~/repo/web-mode.git"))
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-hook 'web-mode-hook (lambda ()
                           (setq web-mode-tag-auto-close-style 1)))

;;;;;;;;
;; yasnippet
;;;;;;;;
;;;
;;(add-to-list 'load-path
;;             (expand-file-name "~/repo/yasnippet.git"))
(require 'yasnippet)
(setq yas-prompt-functions '(yas/ido-prompt))
(yas-global-mode 1)
(add-to-list 'yas-snippet-dirs
             (expand-file-name "~/repo/nobiruwa.github/yasnippet-snippets.git"))
(yas-load-directory (expand-file-name "~/repo/nobiruwa.github/yasnippet-snippets.git"))

;; あるバッファで YASnippet マイナーモードを OFF にしたい
;;(set-default 'yas/dont-activate
;;             #'(lambda ()
;;                 (and yas/root-directory
;;                      (null (yas--get-snippet-tables)))))

;; ~/.emacs.d/snippets/java-mode/getAset のための関数
(defun downcase-initial (obj)
  "It downcases the first letter of OBJ."
  (if (and
       (boundp 'obj)
       (stringp obj)
       (< 0 (length obj)))
      (concat (downcase (substring obj 0 1)) (substring obj 1))
    ""))



;;;;;;;;;;;;;;;
;; MeadowMemo http://www.bookshelf.jp/soft/ の管理人が
;; 自作したEmacs Lisp
;;;;;;;;;;;;;;;
;;color-moccur
;;すべてのバッファを対象に occur を行う．
;;ついでに，今開いているすべてのファイルを対象に grep もできる．
;;さらに，M-x search-buffers の後でスペースで区切って単語を入れると，
;;バッファの全文検索ができる．
(require 'color-moccur)
(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "*Completions*" "*Messages*"
        "newsrc.eld"
        " *migemo*" ".bbdb"))
(setq dmoccur-exclusion-mask
      (append (remove "\\.git/.+" dmoccur-exclusion-mask) '("/\\.git/.+")))
(setq dmoccur-exclusion-mask )
(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
(setq dmoccur-use-list t)
(setq dmoccur-use-project t)
(setq dmoccur-list
      '(
        ;(任意の名前 実際のディレクトリ 検索したいファイルの正規表現 オプション)
        ("dir" default-directory (".*") dir)
        ("current" default-directory (".*") nil)
        ;;("soft" "~/www/soft/" ("\\.texi$") nil)
        ;;("config" "~/mylisp/"  ("\\.js" "\\.el$") nil)
        ;;("1.99" "d:/unix/Meadow2/1.99a6/" (".*") sub)
        ))
(define-key dired-mode-map "O" 'dired-do-moccur)
(setq moccur-split-word t)
(setq color-moccur-default-ime-status nil)
;;(global-set-key "\C-c\C-x\C-o" 'moccur)
;別のキーバインドにしたい
;;(global-set-key "\C-c\C-o" 'search-buffers)
;; If this value is t, cursor motion in the moccur-grep buffer causes
;; automatic display of the corresponding source code location.
(setq moccur-grep-following-mode-toggle t)

;; moccur-edit.el
;; color-moccur の検索結果を直接編集し，ファイルに変更を適用できる．
;; 関数名の変更などが簡単にできる.
;;(autoload 'moccur-edit "moccur-edit" "edit moccur buffer" nil t)
(require 'moccur-edit)

;; grep-edit
;; grep の結果を編集し，その結果をもとにファイルを変更する．
;(autoload 'grep-edit "edit grep result" nil t)
(require 'grep-edit)

;;;
;; customize theme, color
;;;
;;(load-theme 'tango-dark t)
;;;;;;;;
;; 色の設定
;;;;;;;;
(require 'font-lock)
(if (not (featurep 'xemacs)) (global-font-lock-mode t))
;; 全角スペースとかに色を付ける
;; 色はM-x list-color-displayで確認できる
(defface my-face-b-1 '((t (:background "#9e9e9e"))) nil) ; color-247
(defface my-face-b-2 '((t (:background "#d480d4"))) nil) ; color-219
(defface my-face-u-1 '((t (:foreground "#8055aa" :underline t))) nil) ; color-140
(defvar my-face-b-1 'my-face-b-1)
(defvar my-face-b-2 'my-face-b-2)
(defvar my-face-u-1 'my-face-u-1)
;;just in timeな色付け
(setq font-lock-support-mode 'jit-lock-mode)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords major-mode
                          '(("　" 0 my-face-b-1 append)
                            ("\t" 0 my-face-b-2 append)
                            ("[ ]+$" 0 my-face-u-1 append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks
          '(lambda () (if font-lock-mode nil (font-lock-mode t))))

(if (not (eq system-type 'cygwin))
    (progn (show-paren-mode 1)
           (set-face-attribute 'show-paren-match nil
                               :foreground "brightyellow"
                               :weight 'bold)
           (set-face-attribute 'font-lock-comment-delimiter-face nil
                               :foreground "green")
           (set-face-attribute 'font-lock-comment-face nil
                               :foreground "green")))

;;;
;; cygwin
;;;
(if (eq system-type 'cygwin)
    (progn (load "init-cygwin")))

;;;
;; custom-set-*
;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (quote browse-url-firefox))
 '(browse-url-netscape-program "netscape")
 '(column-number-mode t)
 '(line-number-mode t)
 '(menu-bar-mode nil)
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((t (:underline "red"))))
 '(flymake-warnline ((t (:underline "yellow")))))

(provide 'init)
;;; init.el ends here
