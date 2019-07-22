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
;; カーソルを点灯したままにする
(setq visible-cursor nil)

;;;;;;;;
;; 特定のモードに関連付けられないキーの設定
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
(defun my-xclip-cut-function (text)
  "Copy TEXT to clipboard selection of X Window System.
TEXT should be UTF-8.
This requires xclip command."
  (interactive)
  (let ((process-connection-type nil)
        (coding-system-for-write 'utf-8))
    (let ((proc (start-process "xclip" "*Messages*" "xclip" "-selection" "clipboard")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun my-xclip-paste-function ()
  "Paste the text from the X clipboard to the current buffer.
This requires xclip command."
  (interactive)
  (let ((process-connection-type nil)
        (coding-system-for-write 'utf-8))
    (let ((xclip-output (shell-command-to-string "xclip -o -selection clipboard")))
      (unless (string= (car kill-ring) xclip-output)
        (insert xclip-output)))))

(global-set-key (kbd "C-c C-y") 'my-xclip-paste-function)

;; pasteをセットすると、yank時に同内容のテキストが2つずつ入っているように見える
;; pasteはM-x my-xclip-paste-functionかShift-Insertで行えばよいのでnilとする
;; 前者はM-x my-p TABで展開できるうちはキーの割り当ては不要だろう
(when (and (not window-system) (not (eq system-type 'cygwin))
         (executable-find "xclip"))
  (setq interprogram-cut-function 'my-xclip-cut-function)
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
(add-hook 'c-mode-common-hook
          (lambda ()
            (put 'c-file-offsets 'safe-local-variable 'listp)
            (c-set-style "bsd")
            (c-set-offset 'arglist-close 0)
            (c-set-offset 'case-label 2)
            (setq c-basic-offset 2)
            (setq indent-tabs-mode nil)))

;;;;;;;;
;; C++ mode (c++-mode)
;;;;;;;;
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (c-set-style "bsd")
;;             (setq indent-tabs-mode nil)
;;             (setq c-basic-offset 2)))

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
;; See: http://www.gnu.org/software/emacs/manual/html_mono/ido.html#Ignoring
;; foo.gitという名前でリポジトリをcloneするポリシーのため、hidden extensionsから削除する。
;; .gitという隠しディレクトリも見えるようになるが、挙動として問題ない。
;; ちなみに、ido-find-fileに隠されたアイテムを表示するにはC-a (ido-toggle-ignore) を使う。
(setq completion-ignored-extensions (delete ".git/" completion-ignored-extensions))

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
;; nxml-mode
;;;;;;;;
(require 'xml)
(setq nxml-slash-auto-complete-flag t)

;;;;;;;;
;; org-mode
;;;;;;;;
(require 'org)
(setq org-startup-folded nil)
(setq org-startup-truncated nil)
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-lisp.html
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

;;;;;;;;
;; package.el (MELPA)
;;;;;;;;
;; (See also: https://github.com/milkypostman/melpa#usage)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;;;;;;;
;; shell-mode
;;;;;;;;
;; *shell*バッファを現在のウィンドウで開く
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*\\(<[0-9]+>\\)?$" . (display-buffer-same-window)))
;; shell-modeの拡張
;; lsなどの色の設定
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color"
;;  "Set `ansi-color-for-comint-mode' to t." t)
;;(setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "navy" "magenta3" "cyan3" "white"])
(defun first-to-last (suffix list)
  "先頭の要素がSUFFIXを含む場合、LISTの先頭要素を末尾に移動した新しいリストを返します。"
  (if (string-suffix-p suffix (nth 0 list))
      (let* ((first (car list))
            (deleted (remove first list)))
        (add-to-list 'deleted first t))
    list))

(defun company--sort-with-making-special-name-at-the-end (candidates)
  "`../`があればリストの後ろに、`./`があればリストの後ろに置きます。"
  (first-to-last "./" (first-to-last "../" candidates)))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; 存在しないファイル名の入力をスムーズにするため
            (setq-local company-require-match nil)
            ;; 色付け
            (ansi-color-for-comint-mode-on)
            ;; "../" "./"を後ろに回す
            (setq-local company-transformers '(company--sort-with-making-special-name-at-the-end))))

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
;; ;;;
;; ;; auto-complete
;; ;; URL: http://cx4a.org/software/auto-complete/
;; ;;;
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/repo/nobiruwa.github/dot-emacs.d.git/ac-dict")
;; (ac-config-default)

;; ;; auto-complete-modeが有効なバッファでのキーバインド
;; ;;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; (define-key ac-mode-map (kbd "C-c C-i") 'auto-complete)
;; ;; 自動で補完しない
;; (setq ac-auto-start nil)
;; ;; yasnippetを情報源に追加する
;; (add-to-list 'ac-sources 'ac-source-yasnippet)

;; ;; 補完メニュー表示時のキーマップ
;; (setq ac-use-menu-map t)
;; ;; Enter, C-m で補完を終了させる
;; ;; 改行をさせない
;; ;;(define-key ac-menu-map [return] 'ac-complete)

;; ;; html-modeでac-modeを有効にする
;; (add-to-list 'ac-modes 'html-mode)
;; ;; js2-modeでac-modeを有効にする
;; (add-to-list 'ac-modes 'js2-mode)

;;;; 全てのバッファーでauto-complete modeを有効にする
;; (global-auto-complete-mode t)

;;;
;; bash-completion
;;;
;; (autoload 'bash-completion-dynamic-complete "bash-completion"
;;   "BASH completion hook")
;; (add-hook 'shell-dynamic-complete-functions
;;           'bash-completion-dynamic-complete)
;; (add-hook 'shell-command-complete-functions
;;           'bash-completion-dynamic-complete)

;;;;;;;;
;; ccls
;;;;;;;;
(require 'ccls)
(setq ccls-executable (expand-file-name "~/repo/ccls.git/Release/ccls"))

;;;
;; cedet, ede, semantic, etc.
;; Ref: Emacs Part 31
;; URL: http://pc12.2ch.net/test/read.cgi/unix/1251665639/312
;;;
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component (including
;; EIEIO) gets activated by another package (Gnus, auth-source, ...).
(let ((cedet-devel (expand-file-name "~/repo/cedet.git/cedet-devel-load.el")))
  (when (file-exists-p cedet-devel)
    (load-file cedet-devel)))
;; DBファイルを一ヶ所に集約
(setq semanticdb-default-save-directory "~/.emacs.d/semantic")
;; disable semantic-mode and global-*-mode in CEDET
;; CEDET conflicts js2-mode, python-mode
(semantic-mode -1)

;;;;;;;;
;; clang-format
;;;;;;;;
(require 'clang-format)
(setq clang-format-executable "/usr/bin/clang-format-7")

;;;
;; company-mode
;; company-*
;;;
(require 'company)

;; company-backends
(require 'company-clang)
(setq company-clang-executable (executable-find "/usr/bin/clang-7"))
(setq company-clang--version '(normal . 7.0))

(require 'company-dict)
(require 'company-lsp)

(setq company-dict-dir "~/repo/nobiruwa.github/dot-emacs.d.git/company-dict")

(with-eval-after-load "company"
  (global-company-mode +1)
  ;; C-[ C-i
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
  (define-key lisp-interaction-mode-map (kbd "C-M-i") 'company-complete)

  (setq company-backends
        '(company-bbdb
          company-nxml
          company-css
          company-eclim
          company-semantic
          company-lsp
          company-clang
          company-xcode
          company-cmake
          company-capf
          company-files
          (company-dabbrev-code company-etags company-keywords company-dict)
          company-oddmuse
          company-dabbrev)))

;;;
;; emacs-eclim
;; auto-completeの後に読み込む
;;;
(when (require 'eclim nil 'noerror)
  (progn
    ;; シンボリックリンクを開いた場合、名前を解決する
    (setq-default find-file-visit-truename t)
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
        (shell-command command)))))

;;;;;;;;
;; GCL GNU Common Lisp
;;;;;;;;
(setq inferior-lisp-program "sbcl")

;;;
;; emacs-jedi
;; Type:
;;     M-x package-install RET jedi RET
;;     M-x jedi:install-server RET
;;;
(require 'python-environment)
(require 'jedi)

(setq jedi:environment-virtualenv
      (append python-environment-virtualenv
              '("--python" "/usr/bin/python3")))

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
(global-set-key (kbd "<f8>") 'flycheck-mode)
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;
;; ghc ( -> intero)
;;;;;;;;
;; (autoload 'ghc-init "ghc" nil t)
;; (autoload 'ghc-debug "ghc" nil t)
;; (add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;;;;;;;;
;; god-mode
;;;;;;;;
(global-set-key (kbd "\C-\\") 'god-local-mode)

;;;;;;;;
;; graphviz-dot-mode
;;;;;;;;
;; cogre-dot-modeがgraphviz-dot-modeを発見できるようrequire
(require 'graphviz-dot-mode)
(setq graphviz-dot-auto-indent-on-semi nil)
(add-hook 'graphviz-dot-mode-hook (lambda () (auto-complete-mode)))

;;;;;;;;
;; haskell-mode
;;;;;;;;
;; See https://github.com/syl20bnr/spacemacs/issues/706
;; and https://github.com/haskell/haskell-mode/wiki/Indentation
;; haskell-indentation-mode is the current implementataion,
;; but it's too buggy.
(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (setq haskell-indent-offset 2)))

;;;;;;;;
;; highlight-indentation
;;;;;;;;
(require 'highlight-indentation)
(add-hook 'python-mode-hook
          (lambda ()
            "turn on highlight-indentation-mode"
            (highlight-indentation-mode 1)))

;;;;;;;;
;; howm
;;;;;;;;
;; ロード
(require 'howm)
;; キーの再割り当て
(setq howm-prefix "\C-z,")
;; howm開始
(global-set-key "\C-z,," 'howm-menu)
;; メニューの言語
(setq howm-menu-lang 'en)
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

;; リージョンの単語をソートする
(defvar separators-per-mode
  '((emacs-lisp-mode " +" " ")
    (haskell-mode ", *" ", ")
    (otherwise ", *" ", ")))

(defun my-sort-words-in-region (start end)
  "sort words separated white spaces in the current region."
  (interactive "r")
  (let* ((separators (if (assoc major-mode separators-per-mode)
                        (assoc major-mode separators-per-mode)
                       (assoc 'otherwise separators-per-mode)))
         (sep-regexp (cadr separators))
         (sep-fixed (caddr separators)))
    (replace-string
     (buffer-substring start end)
     (my-sort-words-in-line (buffer-substring start end) sep-regexp sep-fixed)
     nil start end)))

;; 文字列内の単語をソートする
(defun my-sort-words-in-line (text sep-regexp sep-fixed)
  "sort words separated white spaces in a line."
  (mapconcat 'identity (sort
   (split-string text sep-regexp) 'string<) sep-fixed))

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
;; intero
;;;;;;;;
(require 'intero)
(setq intero-blacklist '("~/haskellprojects/fay-example" "~/haskellprojects/ghcjs-example"))
(intero-global-mode 1)

;;;;;;;;
;; js-mode
;;;;;;;;
(add-hook 'js-mode-hook
     (lambda ()
       (setq js-indent-level 2)))

;;;;;;;;
;; js2-mode
;; It will refuse to run unless you have byte-compiled it.
;; You must byte-compile it with your version of Emacs because
;; different versions of Emacs have different byte-compiled formats.
;;;;;;;;
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-hook 'js2-mode-hook
     (lambda ()
       (setq js2-basic-offset 2)))

;;;;;;;;
;; lsp-java
;; 補完が効かない場合はM-x lsp-java-update-project-configurationを試すこと
;;;;;;;;
(require 'lsp-java)
(setq lsp-java-java-path (expand-file-name "~/.jenv/shims/java"))
(add-hook 'java-mode-hook #'lsp)

;;;;;;;;
;; lsp-mode
;;;;;;;;
(require 'lsp-mode)
(setq lsp-clients-clangd-executable "/usr/bin/clangd-7")
(setq lsp-prefer-flymake nil)
;; # apt-get install clang-tools-7 # libclang-devのメジャーバージョンと合わせる
;; C++ではclang-formatが必要
(add-hook 'c-mode-hook #'lsp)
(add-hook 'c++-mode-hook #'lsp)

;;;;;;;;
;; lsp-ui
;;;;;;;;
(require 'lsp-ui)

;;;;;;;;
;; navi2ch
;;;;;;;;
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
;; オープン2chを見る C-u s で強制更新
(setq navi2ch-list-bbstable-url
      "http://menu.open2ch.net/bbsmenu.html")
(setq navi2ch-list-valid-host-regexp
      (concat "\\("
              (regexp-opt '(".machibbs.com" ".machi.to" ".open2ch.net"))
              "\\)\\'"))
;;;;;;;;
;; omnisharp-mode
;;;;;;;;
(when (require 'omnisharp nil 'noerror)
  (progn
   (setq omnisharp-server-executable-path (expand-file-name "~/repo/omnisharp-server.git/OmniSharp/bin/Debug/OmniSharp.exe"))
   (add-hook 'csharp-mode-hook 'omnisharp-mode)))

;;;;;;;;
;; purescript-mode
;;;;;;;;
(add-hook 'purescript-mode-hook
          (lambda ()
            (setq haskell-literate nil)
            (haskell-indentation-mode)))

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
;; skk-modeが有効になると、C-jがskk-kakutei-keyにバインドされる
;; 使用頻度の殆どないC-oにnewlineをバインドする
(add-hook 'skk-load-hook
          '(lambda ()
             (progn
               (if (functionp 'electric-newline-and-maybe-indent)
                   (progn
                     (define-key skk-abbrev-mode-map "\C-o" 'electric-newline-and-maybe-indent)
                     (define-key skk-latin-mode-map "\C-o" 'electric-newline-and-maybe-indent)
                     (define-key skk-jisx0208-latin-mode-map "\C-o" 'electric-newline-and-maybe-indent)
                     (define-key skk-j-mode-map "\C-o" 'electric-newline-and-maybe-indent))))))

(setq skk-aux-large-jisyo nil)
;; ▽モードと▼モード時のアンドゥ情報を記録しない
(setq skk-undo-kakutei-word-only t)
;; C-x j のskk-auto-fill-modeは使わない
(global-set-key "\C-xj" 'skk-mode)
(global-set-key "\C-x\C-j" 'skk-mode)

;;;;;;;;
;; slime-helper
;;;;;;;;
;; sbcl REPLで(ql:quickload "quicklisp-slime-helper")を実行した後に
(let ((slime-helper (expand-file-name "~/quicklisp/slime-helper.el")))
  (when (file-exists-p slime-helper) (load slime-helper)))

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
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode))
(add-hook 'web-mode-hook (lambda ()
                           (progn
                             (auto-fill-mode -1)
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-css-indent-offset 2)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-auto-close-style 1))))

;;;;;;;;
;; yasnippet
;;;;;;;;
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
 '(package-selected-packages
   (quote
    (ac-slime bash-completion browse-kill-ring ccls clang-format coffee-mode company-dict company-lsp ddskk dockerfile-mode elm-mode elpy emmet-mode f flycheck flycheck-pyflakes god-mode gradle-mode graphviz-dot-mode groovy-mode haskell-mode howm idomenu intero jedi js2-mode lsp-java lsp-mode lsp-ui lua-mode markdown-mode navi2ch powershell purescript-mode restclient shakespeare-mode slime swiper treemacs typescript-mode undo-tree vue-mode web-mode xclip yaml-mode yasnippet yasnippet-snippets)))
 '(safe-local-variable-values
   (quote
    ((haskell-process-use-ghci . t)
     (haskell-indent-spaces . 4))))
 '(show-paren-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-error ((t (:underline "red"))))
 '(flymake-warning ((t (:underline "yellow")))))

(provide 'init)
;;; init.el ends here
