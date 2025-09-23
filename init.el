;;; init.el --- my Emacs settings
;;; Commentary:
;;;  My Emacs settings.
;;; Code:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initファイルロード関数の定義
(defun load-env-if-exists (env-path)
  "与えられたENV-PATHが存在する場合、initファイルとみなしてロードします。"
  (let ((init-env-el (expand-file-name env-path)))
    (when (file-exists-p init-env-el)
      (load-file init-env-el))))

;;;
;; 環境ごとの設定を~/.emacs.d/init_env_pre.elに書く
;;;
(load-env-if-exists (expand-file-name "init_env_pre.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; グローバルな設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; ファイルの扱い
;;;;;;;;
;; load-path
(setq load-path (append (list (expand-file-name "site-lisp" user-emacs-directory)) load-path))

;; バックアップファイル(foo.txt~)
;; バックアップファイルを作らない
(setq make-backup-files nil)
;; 自動保存ファイル(#foo.txt#)
;; 自動保存ファイルの保存場所を"~/.emacs.d/tmp"に変更する
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "tmp/" user-emacs-directory) t)))

;; 自動保存リストファイル(~/.emacs.d/auto-save-list/.saves-xxxx)
;; 自動保存リストファイルはデフォルトの作るので設定を変更しない

;; ロックファイル(.#foo.txt)
;; ロックファイルを作らない
(setq create-lockfiles nil)

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Customizations.html
;; カスタマイズをinit.elではなく~/.emacs.d/emacs-custom.elに保存する
(setq custom-file (expand-file-name "emacs-custom.el" user-emacs-directory))

;;;;;;;;
;; 初期化
;;;;;;;;
;; メニューバーを表示する場合はnon-nil
(menu-bar-mode -1)
;; emacsclient
(server-start)
;; 対となる括弧を強調表示する場合はnon-nil
(show-paren-mode 1)
;; ツールバーを表示する場合はnon-nil
(tool-bar-mode -1)
;; カーソルのブリンクを有効にする場合はnon-nil
(setq blink-cursor-mode nil)
;; カーソル位置の桁を表示する場合はnon-nil
(setq column-number-mode t)
;; デバッグをする場合はnon-nil
(setq debug-on-error nil)
;; 起動画面がいつまでも消えてくれないのではなから使わない
(setq inhibit-startup-message t)
;; 行数を表示する場合はnon-nil
(setq line-number-mode t)
;; ベル音が不要な場合はnon-nilな関数かシンボル
(setq ring-bell-function 'ignore)
;; ベル音を画面のフラッシュに変更する場合はnon-nil
(setq visible-bell nil)
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

;; emacs-noxでのXアプリケーションのクリップボードの使用
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
;; settings for utf-8
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

;;;;;;;;
;; Native Compile
;;;;;;;;
(when (and (functionp 'native-comp-available-p) (native-comp-available-p))
  ;; コンパイル時の警告やエラーを出力するとき*Warning*バッファのウィンドウをポップアップさせない
  (setq native-comp-async-report-warnings-errors 'silent))

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
;; cedet, ede, semantic, etc.
;; Ref: Emacs Part 31
;; URL: http://pc12.2ch.net/test/read.cgi/unix/1251665639/312
;;;;;;;;
;; DBファイルを一ヶ所に集約
(setq semanticdb-default-save-directory (expand-file-name "semantic" user-emacs-directory))
;; disable semantic-mode and global-*-mode in CEDET
;; CEDET conflicts js2-mode, python-mode
(semantic-mode -1)

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
(add-hook 'java-mode-hook (lambda ()
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
(add-to-list 'package-archives
             '("gnu-devel" . "https://elpa.gnu.org/devel/") t)
;; Emacs 27から、パッケージはinit.elのロードよりも前にロードされるようになり、
;; package-initializeを呼ぶ必要はなくなりました。
(when (< emacs-major-version 27)
  (package-initialize))
;; package-selected-packagesに存在するパッケージでインストールしていないパッケージがあればインストールする関数です。
(defun my-install-packages-if-not-installed ()
  "install packages listed in package-selected-packages if they have not been installed yet."
  (interactive)
  (when (cl-find-if-not #'package-installed-p package-selected-packages)
    (package-refresh-contents)
    (mapc #'package-install package-selected-packages)))

;;;;;;;;
;; project.el
;;;;;;;;
(require 'project)

;; プロジェクトを探す関数
;; Ref: https://christiantietze.de/posts/2022/03/mark-local-project.el-directories/
;; Ref: https://www.reddit.com/r/emacs/comments/lfbyq5/comment/ivyzm0q/?utm_source=share&utm_medium=web2x&context=3
;; Ref: https://andreyorst.gitlab.io/posts/2022-07-16-project-el-enhancements/
(defcustom project-root-markers
  '(;; add file identifier below
    ;; General
    ".project"
    ;; C/C++
    "meson.build"
    ;; Haskell
    "cabal.project"
    "stack.yaml"
    ;; Java
    "build.gradle"
    ;; Python
    "pyrightconfig.json"
    ;; Rust
    "Cargo.toml"
    )
  "Files or directories that indicate the root of a project."
  :type '(repeat string)
  :group 'project)

(defun project-root-p (path)
  "Check if the current PATH has any of the project root markers."
  (catch 'found
    (dolist (marker project-root-markers)
      (when (file-exists-p (concat path marker))
        (throw 'found marker)))))

(defun project-find-root (path)
  "Search up the PATH for `project-root-markers'."
  ;; trampで扱うファイルはスキップする
  (when (not (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p path)))
    (let ((path (expand-file-name path)))
      (catch 'found
        (while (not (equal "/" path))
          (if (not (project-root-p path))
              (setq path (file-name-directory (directory-file-name path)))
            (throw 'found (cons 'transient path))))))))

(add-to-list 'project-find-functions #'project-find-root)

;;;;;;;;
;; shell-mode
;;;;;;;;
(require 'shell)
;; *shell*バッファを現在のウィンドウで開く
(add-to-list 'display-buffer-alist
             '("^\\*shell\\*\\(<[0-9]+>\\)?$" . (display-buffer-same-window)))

;; solarized-darkと組み合わせた時のプロンプトの色
(when (eq window-system 'x)
  (set-face-foreground 'comint-highlight-prompt "#268bd2"))

;; shell-modeの拡張
;; lsなどの色の設定
;; (autoload 'ansi-color-for-comint-mode-on "ansi-color"
;;  "Set `ansi-color-for-comint-mode' to t." t)
;;(setq ansi-color-names-vector ["black" "red3" "green3" "yellow3" "navy" "magenta3" "cyan3" "white"])

;; company-modeのパス補完動作のカスタマイズ
(defun first-to-last (suffix list)
  "先頭の要素がSUFFIXを含む場合、LISTの先頭要素を末尾に移動した新しいリストを返します。"
  (if (and list (string-suffix-p suffix (nth 0 list)))
      (let* ((first (car list))
            (deleted (remove first list)))
        (add-to-list 'deleted first t))
    list))

(defun company--sort-with-making-special-name-at-the-end (candidates)
  "`../`があればリストの後ろに、`./`があればリストの後ろに置きます。"
  (first-to-last "./" (first-to-last "../" candidates)))

(add-hook 'shell-mode-hook
          (lambda ()
            ;; TAB幅は8
            (setq tab-width 8)
            ;; 存在しないファイル名の入力をスムーズにするため
            (setq-local company-require-match nil)
            ;; 色付け
            (ansi-color-for-comint-mode-on)
            ;; "../" "./"を後ろに回す
            (setq-local company-transformers '(company--sort-with-making-special-name-at-the-end))))

;;;
;; shell-modeにおいて、company-modeのcomapny-capfが実行ファイル名の解決を行うとき
;; 探索するディレクトリを制限します。
;;;
(defcustom shell-command-ignored-exec-path-regexp "/mnt/.*"
  "a REGEXP to be ignored when searching executables from `exec-path' directories."
  :type 'regexp)

(defvar shell-command-original-exec-path nil
  "the original exec-path value before running shell-command-completion.")

(defun deep-copy-sequence (x)
  "Make a deep copy of the given sequence X."
  (mapcar #'copy-sequence x))

(defun shell-command-backup-exec-path ()
  "backup `exec-path'"
  (setq shell-command-original-exec-path (deep-copy-sequence exec-path)))

(defun shell-command-remove-from-exec-path ()
  "remove elements matching `shell-command-ignored-exec-path-regexp' from `exec-path'"
  (cl-delete-if
   (lambda (path)
     (string-match-p shell-command-ignored-exec-path-regexp path)) exec-path))

(defun shell-command-restore-exec-path (&rest args)
  "restore `exec-path' from `shell-command-ignored-exec-path-regexp'"
  (when shell-command-original-exec-path
    (setq exec-path shell-command-original-exec-path)
    (setq shell-command-original-exec-path nil)))

(defun shell-command-comint-completion-at-point-around (orig-func &rest args)
  "An advice function which change `exec-path' during calling ORIG-FUNC. Restores `exec-path' at the end. It works only in shell-mode."
  (when (derived-mode-p 'shell-mode)
    (shell-command-backup-exec-path)
    (shell-command-remove-from-exec-path)
    (let ((res (ignore-errors (apply orig-func args))))
      (shell-command-restore-exec-path)
      res)))

;; To enable this, uncomment the following line.
;; (advice-add 'comint-completion-at-point :around #'shell-command-comint-completion-at-point-around)

;;;;;;;;
;; uniquify
;; Ref: http://q.hatena.ne.jp/1137478760 の回答24
;; a/index.html と b/index.html をひらいたときに、
;; バッファ名を index.html<a>, index.html<b> としてくれます。
;; デフォルトの連番だとどれがどれだかわからなくなるので。
;;;;;;;;
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;;;;;;;
;; url-http.el
;;;;;;;;
;; Emacs 28.1未満ではProxy-Authorizationヘッダーが送信されない
;; url-https-proxy-connect関数をアドホックで修正する
;; https://github.com/syl20bnr/spacemacs/issues/4807#issuecomment-723332754
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=42422
(when (or (< emacs-major-version 28) (and (= emacs-major-version 28) (< emacs-minor-version 1)))
  (with-eval-after-load 'url-http
    (defun url-https-proxy-connect (connection)
      (setq url-http-after-change-function 'url-https-proxy-after-change-function)
      (process-send-string connection
                           (format
                            (concat "CONNECT %s:%d HTTP/1.1\r\n"
                                    "Host: %s\r\n"
                                    (let ((proxy-auth (let ((url-basic-auth-storage
                                                             'url-http-proxy-basic-auth-storage))
                                                        (url-get-authentication url-http-proxy nil 'any nil))))
                                      (if proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))
                                    "\r\n")
                            (url-host url-current-object)
                            (or (url-port url-current-object)
                                url-https-default-port)
                            (url-host url-current-object))))))

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

;;;
;; requireの代わりに使います。
;;;
(setq require-if-not-loaded-packages '())
(defun require-if-not (feature &optional else-body)
  "パッケージをロードします。パッケージのロードに失敗した場合はELSE-BODYを実行します。ロードに失敗した場合FEATUREがrequire-if-not-loaded-packages変数に追加されます。パッケージをロードした場合はtを、ロードに失敗した場合にはnilを返します。"
  (if (require feature nil t)
      (progn
        (message "require-if-not: [%s] is loaded." feature)
        t)
    (progn
      (add-to-list 'require-if-not-loaded-packages feature)
      (if (and (boundp 'else-body) (functionp 'else-body))
          (funcall else-body)
        (message "require-if-not: [%s] is not loaded." feature))
      nil)))

;;;;;;;;;;;;; 以下、ELispファイルを追加する必要があるものを設定 ;;;;;;
;;;;;;;;;;;;; アルファベット順になるよう努力 ;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;
;; cargo-minor-mode
;;;;;;;;
(require-if-not 'rust-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)

;;;;;;;;
;; ccls
;;;;;;;;
;; install clang environments and build by the following commands.
;; $ sudo apt install clang clang-format clang-tools clangd libclang-dev llvm
;; $ cmake -H. -Brelease -DCMAKE_BUILD_TYPE=Release
;; $ cmake --build release
(require-if-not 'ccls)
(setq ccls-executable (expand-file-name "~/repo/ccls.git/release/ccls"))

;;;;;;;;
;; clang-format
;;;;;;;;
(require-if-not 'clang-format)

;;;
;; company-mode
;; company-*
;;;
(require-if-not 'company)

;; company-backends
(require-if-not 'company-clang)
(require-if-not 'company-dict)
(require-if-not 'company-lsp)

(setq company-dict-dir (expand-file-name "~/repo/nobiruwa.github/dot-emacs.d.git/company-dict"))

(with-eval-after-load "company"
  (global-company-mode +1)
  ;; C-[ C-i
  (global-set-key (kbd "C-M-i") 'company-complete)
  (define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete)
  (define-key lisp-interaction-mode-map (kbd "C-M-i") 'company-complete)
  ;; デフォルト値を保存copy-treeが再帰的にリストをコピーできる
  (setq company-backends-default (copy-tree company-backends))
  (setq company-backends
        '(company-bbdb
          company-nxml
          company-css
          company-semantic
          ;; lsp-modeが以下の警告を表示するので、とりあえずコメントアウト
          ;; `company-lsp` is not supported anymore. Using `company-capf` as the `lsp-completion-provider`.
          ;; company-lsp
          company-clang
          company-cmake
          company-capf
          company-files
          (company-dabbrev-code company-gtags company-etags company-keywords company-dict)
          company-oddmuse
          company-dabbrev)))

;;;;;;;;
;; GCL GNU Common Lisp
;;;;;;;;
(setq inferior-lisp-program "sbcl")

;;;;;;;;
;; counsel
;;;;;;;;
(require-if-not 'counsel)
(with-eval-after-load "counsel"
  ;; counsel and swiper use M-s as the prefix.
  (global-set-key (kbd "M-s M-x") 'counsel-M-x)
  (global-set-key (kbd "M-s M-f") 'counsel-find-file)
  (global-set-key (kbd "M-s <f1> f") 'counsel-describe-function)
  (global-set-key (kbd "M-s <f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "M-s <f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "M-s <f1> l") 'counsel-find-library)
  (global-set-key (kbd "M-s <f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "M-s <f2> u") 'counsel-unicode-char)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
  (global-set-key (kbd "C-c f") 'find-file))

;;;;;;;;
;; eglot
;;;;;;;;
(require-if-not 'eglot)
(with-eval-after-load "eglot"
  (setq eglot-autoshutdown t)
  ;; C/C++
  ;; git cloneしてソースからビルドする
  (add-to-list 'eglot-server-programs
               `(c-mode . (,(expand-file-name "~/repo/ccls.git/release/ccls"))))
  (add-to-list 'eglot-server-programs
               `(c++-mode . (,(expand-file-name "~/repo/ccls.git/release/ccls"))))
  ;; Java
  ;; java-language-serverを用いる場合:
  ;; git cloneしてソースからビルドする
  ;; (add-to-list 'eglot-server-programs
  ;;              `(java-mode . (,(expand-file-name "~/repo/java-language-server.git/dist/lang_server_linux.sh"))))
  ;; Python
  ;; npmでインストールする
  ;; $ mkdir ~/opt/pyright
  ;; $ cd ~/opt/pyright
  ;; $ npm init
  ;; $ npm -g --prefix ~/opt/pyright install pyright
  (add-to-list 'eglot-server-programs
               `(python-mode . (,(expand-file-name "~/opt/pyright/bin/pyright-langserver") "--stdio")))
  (setq-default eglot-workspace-configuration
                (list
                 :haskell '(:formattingProvider "fourmolu")
                 :java #s(hash-table)
                 :python `(:venvPath ,(expand-file-name "~/.venvs")))))

;;;;;;;;
;; eglot-java
;;;;;;;;
(require-if-not 'eglot-java)
;; (add-hook 'java-mode-hook 'eglot-java-mode)
(with-eval-after-load 'eglot-java

  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh))

;;;;;;;;
;;  emmet-mode
;;;;;;;;
(require-if-not 'emmet-mode)
(with-eval-after-load "emmet-mode"
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

(add-hook 'html-mode-hook
          (lambda ()
            (setq-local emmet-self-closing-tag-style "")
            (emmet-mode)))
(add-hook 'web-mode-hook
          (lambda ()
            (setq-local emmet-self-closing-tag-style "")
            (emmet-mode)))
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
(require-if-not 'flycheck)
(setq flycheck-flake8-maximum-complexity 10)
(with-eval-after-load "flycheck"
    (global-set-key (kbd "<f8>") 'flycheck-mode))
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;
;; god-mode
;;;;;;;;
(require-if-not 'god-mode)
(with-eval-after-load "god-mode"
  (global-set-key (kbd "\C-\\") 'god-local-mode))

;;;;;;;;
;; graphviz-dot-mode
;;;;;;;;
;; cogre-dot-modeがgraphviz-dot-modeを発見できるようrequire
(require-if-not 'graphviz-dot-mode)
(setq graphviz-dot-auto-indent-on-semi nil)
(add-hook 'graphviz-dot-mode-hook (lambda () (auto-complete-mode)))

;;;;;;;;
;; haskell-mode
;;;;;;;;
;; See https://github.com/syl20bnr/spacemacs/issues/706
;; and https://github.com/haskell/haskell-mode/wiki/Indentation
;; haskell-indentation-mode is the current implementataion,
;; but it's too buggy.
(require-if-not 'haskell-mode)

;; flycheck標準のhaskell用checkersを使わない
(setq flycheck-checkers
      (cl-reduce
       (lambda (acc x) (remove x acc))
       '(haskell-stack-ghc haskell-ghc haskell-hlint)
       :initial-value flycheck-checkers))

;; rust-modeを参考にバッファーを保存する時にフォーマットする
(defcustom haskell-format-on-save nil
  "Format future haskell buffers before saving."
  :type 'boolean
  :safe #'booleanp
  :group 'haskell-mode)

(defun haskell-enable-format-on-save ()
  "Enable formatting when saving buffer."
  (interactive)
  (setq-local haskell-format-on-save t))

(defun haskell-disable-format-on-save ()
  "Disable formatting when saving buffer."
  (interactive)
  (setq-local haskell-format-on-save nil))

(defun haskell-format-save-hook ()
  "Enable formatting when saving buffer."
  (when haskell-format-on-save
    (cond ((bound-and-true-p eglot--managed-mode)
           (eglot-format-buffer))
          ((bound-and-true-p lsp-mode)
           (lsp-format-buffer)))))

(add-hook 'before-save-hook #'haskell-format-save-hook)

(add-hook 'haskell-mode-hook
          (lambda ()
            (turn-on-haskell-indentation)
            (setq-local tab-width 2)
            (setq-local haskell-format-on-save t)))

;;;;;;;;
;; highlight-indentation
;;;;;;;;
(require-if-not 'highlight-indentation)
(add-hook 'python-mode-hook
          (lambda ()
            "turn on highlight-indentation-mode"
            (highlight-indentation-mode 1)))

;;;;;;;;
;; howm
;;;;;;;;
;;;
;; howmのロード前に書くべきカスタマイズ内容
;; テンプレートのカスタマイズも合わせてここで指定する
;;;
;; 1行目のタイトルヘッダーをorg-modeのExport Settingsに合わせる
(setq howm-view-title-header "#+TITLE:")
(setq howm-template-date-header "#+DATE:") ;; 独自の変数
(setq howm-template (concat howm-view-title-header " %title%cursor\n"  howm-template-date-header " %date\n%file\n\n"))
;; ロード
(require-if-not 'howm)
;; キーの再割り当て
(setq howm-prefix "\C-z,")
;; howm開始
(with-eval-after-load "howm-mode"
  (global-set-key "\C-z,," 'howm-menu))
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

;; (add-hook 'howm-view-summary-mode-hook
;;     (lambda ()
;;             (jit-lock-register 'howm-add-day-of-week-overlay-region)))

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

;; ;;;;;;;;
;; ;; intero (-> lsp-haskell)
;; ;;;;;;;;
;; (require-if-not 'intero)
;; (setq intero-blacklist '("~/haskellprojects/fay-example" "~/haskellprojects/ghcjs-example"))
;; (intero-global-mode 1)

;;;;;;;;
;; ivy-mode
;;;;;;;;
(require-if-not 'ivy)
(with-eval-after-load "ivy"
  ;; use timer to improve the ivy-read performance #1218
  ;; https://github.com/abo-abo/swiper/issues/1218
  (setq ivy-dynamic-exhibit-delay-ms 250)
  ;; M-x lsp-java-generate-overrides や M-x lsp-java-spring-initializr など、複数の選択肢から選択する際に使う
  ;; ivyのキーマップには登録されていないが必要不可欠な関数なので、ここで登録する
  (define-key ivy-minibuffer-map (kbd "M-RET") 'ivy-mark)
  ;; `./`と`../`を先頭に表示する必要はない
  ;; リストの末尾に置けるならばよかったのだが
  (setq ivy-extra-directories nil))

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
(add-to-list 'auto-mode-alist '("\\.cjs$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.mjs$" . js2-mode))
(add-hook 'js2-mode-hook
     (lambda ()
       (setq js2-basic-offset 2)))

;;;;;;;;
;; lsp (lsp-mode)
;;;;;;;;
(require-if-not 'lsp)
(with-eval-after-load "lsp"
  (setq lsp-prefer-flymake nil)
  ;; core
  ;; https://emacs-lsp.github.io/lsp-mode/page/settings/mode/
  (setq lsp-keep-workspace-alive nil)
  ;; performance
  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq gc-cons-threshold 100000000)
  ;; rust
  (setq lsp-rust-analyzer-proc-macro-enable t)
  ;; modelineにprogressトークンのメッセージが溜まっていくことがある
  ;; 手動でクリアできるようにする
  (defun my-lsp-clear-workspace-work-done-tokens ()
    "Clear lsp--workspace-work-done-tokens."
    (interactive)
    (let ((workspaces? (lsp-workspaces)))
      (when (and (listp workspaces?) (> (length workspaces?) 0))
        (let* ((cur-workspace (cl-first workspaces?))
               (cur-workdone-tokens (lsp--workspace-work-done-tokens cur-workspace)))
          (when (hash-table-p cur-workdone-tokens)
            (clrhash cur-workdone-tokens))))))
  ;; 簡単にサーバーを再起動できるようにする
  (define-key lsp-mode-map (kbd "<f5>") 'lsp-workspace-restart)
  ;; 簡単にmodelineをリフレッシュできるようにする
  (define-key lsp-mode-map (kbd "<f6>") 'my-lsp-clear-workspace-work-done-tokens)
  )

;;;;;;;;
;; lsp-haskell
;;;;;;;;
(require-if-not 'lsp-haskell)
(setq lsp-haskell-formatting-provider "fourmolu")
;; lsp-format-buffer, lsp-format-regionが使用するインデント幅を
;; haskell-modeのhaskell-indentation-layout-offsetに合わせる
(add-to-list 'lsp--formatting-indent-alist '(haskell-mode . haskell-indentation-layout-offset))

;;;;;;;;
;; lsp-java
;; 補完が効かない場合はM-x lsp-java-update-project-configurationを試すこと
;;;;;;;;
(require-if-not 'lsp-java)
(with-eval-after-load "lsp-java"
  (setq lsp-java-java-path (expand-file-name "~/.jenv/shims/java"))
  )

;;;;;;;;
;; lsp-pyright
;;;;;;;;
(require-if-not 'lsp-pyright)
(with-eval-after-load "lsp-pyright"
  )

;; ;;;;;;;;
;; ;; lsp-python-ms (-> lsp-pyright)
;; ;;;;;;;;
;; (require-if-not 'lsp-python-ms)
;; (with-eval-after-load "lsp-python-ms"
;;   (setq lsp-python-ms-auto-install-server t)
;;   )


;;;;;;;;
;; lsp-ui
;;;;;;;;
(require-if-not 'lsp-ui)
(with-eval-after-load "lsp-ui"
  ;; C-s/C-rで検索中にlsp-ui-docウィンドウが開き、検索が中断される
  ;; トグルをF7にバインドする
  (defun toggle-lsp-ui-doc ()
    (interactive)
    (progn (lsp-ui-doc-enable (not lsp-ui-doc-enable))
           (setq lsp-ui-doc-enable (not lsp-ui-doc-enable))))
  (define-key lsp-mode-map (kbd "<f7>") 'toggle-lsp-ui-doc))

;;;;;;;;
;; navi2ch
;;;;;;;;
(autoload 'navi2ch "navi2ch" "Navigator for 2ch for Emacs" t)
;; おーぷん２ちゃんねるを見る C-u s で強制更新
(setq navi2ch-list-bbstable-url
      "https://menu.open2ch.net/bbsmenu.html")
;; おーぷん２ちゃんねるがgzipを返さなくなった
(setq navi2ch-net-accept-gzip nil)

;; ２ちゃんねるとみなす掲示板をおーぷん２ちゃんねるとまちBBSに限定する
(setq navi2ch-list-valid-host-regexp
      (concat "\\("
              (regexp-opt '(".machibbs.com" ".machi.to" ".open2ch.net"))
              "\\)\\'"))

;;;;;;;;
;; omnisharp-mode
;;;;;;;;
(when (require-if-not 'omnisharp)
  ;; You can install by M-x omnisharp-install-server
  ;; but I chose install requirements manually.
  ;; download .NET Core Binaries from: https://dotnet.microsoft.com/download/dotnet-core/2.2 and add it to PATH.
  ;; $ wget https://roslynomnisharp.blob.core.windows.net/releases/latest/omnisharp-linux-x64.tar.gz
  ;; $ tar -C ~/opt/omnisharp-linux-x64
  ;; $ cd ~/opt && ln -s omnisharp-linux-x64 omnisharp
  (setq omnisharp-server-executable-path (expand-file-name "~/opt/omnisharp/run"))
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (eval-after-load
      'company
    '(add-to-list 'company-backends 'company-omnisharp))
  (add-hook 'csharp-mode-hook #'company-mode))

;;;;;;;;
;; purescript-mode
;;;;;;;;
(require-if-not 'purescript-mode)
(with-eval-after-load "purescript-mode"
  (add-hook 'purescript-mode-hook
            (lambda ()
              (setq haskell-literate nil)
            (haskell-indentation-mode))))

;;;;;;;;
;; reopen-as-root
;; 下記URLのEmacs Lispをファイルreopen-as-root.elに保存した
;; Ref: http://ubulog.blogspot.com/2010/08/emacs-sudo2.html
;;;;;;;;
(require-if-not 'reopen-as-root)

;;;;;;;;
;; plantuml-mode
;;;;;;;;
(require-if-not 'plantuml-mode)
(add-hook 'plantuml-mode-hook
          (lambda ()
            (setq plantuml-jar-path (expand-file-name "~/opt/plantuml/plantuml.jar"))))

;;;;;;;;
;; rust-mode
;;;;;;;;
(require-if-not 'rust-mode)
(add-hook 'rust-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            )

  ;; Formatting is bound to C-c C-f.
  ;; The folowing enables automatic formatting on save.
  (setq rust-format-on-save t))

;;;;;;;;
;; skk
;;;;;;;;
(require-if-not 'skk)
;; skk-modeが有効になると、C-jがskk-kakutei-keyにバインドされる
;; 使用頻度の殆どないC-oにnewlineをバインドする
(add-hook 'skk-load-hook
          (lambda ()
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
(with-eval-after-load "skk"
  ;; C-x j のskk-auto-fill-modeは使わない
  (global-set-key "\C-xj" 'skk-mode)
  (global-set-key "\C-x\C-j" 'skk-mode))

;;;;;;;;
;; slime
;;;;;;;;
(require-if-not 'slime)

;;;;;;;;
;; slime-helper
;;;;;;;;
;; with roswell
;; $ cd ~/repo
;; $ git clone https://github.com/roswell/roswell roswell.git
;; $ cd roswell.git
;; $ ./bootstrap
;; $ ./configure --prefix=$HOME/opt/roswell
;; $ make
;; $ make install
;; after adding $HOME/opt/roswell/bin to PATH
;; $ ros setup
;; $ ros install sbcl-bin
;; $ ros install slime
;; https://github.com/roswell/roswell/wiki/Initial-Recommended-Setup#for-emacs
(let ((slime-helper (expand-file-name "~/.roswell/helper.el")))
  (when (file-exists-p slime-helper)
    (load slime-helper)

    ;; SLIMEとSKKとの衝突を回避する設定
    ;; 特定の場面で、SLIMEとSKKとの間でスペースキーのキーバインドが競合して、SKKでの変換ができなくなります。
    ;; https://lisphub.jp/common-lisp/cookbook/index.cgi?SLIME#H-33uy3rfpe0845
    (defun my-slime-space (n)
      (interactive "p")
      (if (and (boundp 'skk-henkan-mode) skk-henkan-mode)
          (skk-insert n)
        (slime-autodoc-space n)))
    (define-key slime-autodoc-mode-map " " 'my-slime-space)
    (setq inferior-lisp-program "ros -Q run")))

;;;;;;;;
;; solarized-theme
;;;;;;;;
;; make the fringe stand out from the background
;; (setq solarized-distinct-fringe-background t)

;; Don't change the font for some headings and titles
(setq solarized-use-variable-pitch nil)

;; make the modeline high contrast
;; (setq solarized-high-contrast-mode-line t)

;; Use less bolding
;; (setq solarized-use-less-bold t)

;; Use more italics
;; (setq solarized-use-more-italic t)

;; Use less colors for indicators such as git:gutter, flycheck and similar
;; (setq solarized-emphasize-indicators nil)

;; Don't change size of org-mode headlines (but keep other size-changes)
(setq solarized-scale-org-headlines nil)

;; Avoid all font-size changes
(setq solarized-height-minus-1 1.0)
(setq solarized-height-plus-1 1.0)
(setq solarized-height-plus-2 1.0)
(setq solarized-height-plus-3 1.0)
(setq solarized-height-plus-4 1.0)

;;;;;;;;
;; swiper
;;;;;;;;
(require-if-not 'swiper)
(with-eval-after-load "swiper"
  ;; counsel and swiper use M-s as the prefix.
  (global-set-key (kbd "M-s M-s") 'swiper)
  (global-set-key (kbd "M-s C-s") 'swiper-isearch)
  (global-set-key (kbd "M-s C-r") 'swiper-isearch-backward)
  (global-set-key (kbd "M-s s") 'swiper-thing-at-point))

;;;;;;;;
;; undo-tree
;;;;;;;;
(require-if-not 'undo-tree)
(with-eval-after-load "undo-tree"
  (global-undo-tree-mode)
  ;; rxvt-unicode detects C-c C-/ as C-c C-_
  (define-key undo-tree-map (kbd "C-c C-/") 'undo-tree-redo)
  (define-key undo-tree-map (kbd "C-c C-_") 'undo-tree-redo)
  (setq undo-tree-auto-save-history nil))

;;;;;;;;
;; vue-mode
;;;;;;;;
(require-if-not 'vue-mode)
(add-hook 'vue-mode-hook
          (lambda ()
            (setq vue-html-extra-indent 2)
            (setq mmm-js-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))
            (setq mmm-typescript-mode-enter-hook (lambda () (setq syntax-ppss-table nil)))))

;;;;;;;;
;; wdired
;;;;;;;;;
(require-if-not 'wdired)
(with-eval-after-load "wdired"
  (define-key dired-mode-map "r" 'wdired-change-to-wdired-mode))

;;;;;;;;
;; web-mode
;;;;;;;;
(require-if-not 'web-mode)
(with-eval-after-load "web-mode"
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[s]?css\\'" . web-mode)))
(add-hook 'web-mode-hook (lambda ()
                           (progn
                             (auto-fill-mode -1)
                             (setq web-mode-markup-indent-offset 2)
                             (setq web-mode-css-indent-offset 2)
                             (setq web-mode-code-indent-offset 2)
                             (setq web-mode-auto-close-style 1))))

;;;;;;;;
;; wgrep
;;;;;;;;
(require-if-not 'wgrep)

;;;;;;;;
;; yasnippet
;;;;;;;;
(require-if-not 'yasnippet)
(with-eval-after-load "yasnippet"
  (setq yas-prompt-functions '(yas/ido-prompt))
  (yas-global-mode 1)
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "~/repo/nobiruwa.github/yasnippet-snippets.git"))
  (yas-load-directory (expand-file-name "~/repo/nobiruwa.github/yasnippet-snippets.git")))

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
(require-if-not 'color-moccur)
(with-eval-after-load "color-moccur"
  (define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
  (define-key dired-mode-map "O" 'dired-do-moccur))
(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "*Completions*" "*Messages*"
        "newsrc.eld"
        " *migemo*" ".bbdb"))
(setq dmoccur-exclusion-mask
      (append (remove "\\.git/.+" dmoccur-exclusion-mask) '("/\\.git/.+")))
(setq dmoccur-use-list t)
(setq dmoccur-use-project t)
(setq dmoccur-list
      '(
        ;;(任意の名前 実際のディレクトリ 検索したいファイルの正規表現 オプション)
        ("dir" default-directory (".*") dir)
        ("current" default-directory (".*") nil)
        ;;("soft" "~/www/soft/" ("\\.texi$") nil)
        ;;("config" "~/mylisp/"  ("\\.js" "\\.el$") nil)
        ;;("1.99" "d:/unix/Meadow2/1.99a6/" (".*") sub)
        ))
(setq moccur-split-word t)
(setq color-moccur-default-ime-status nil)
;;(global-set-key "\C-c\C-x\C-o" 'moccur)
;;別のキーバインドにしたい
;;(global-set-key "\C-c\C-o" 'search-buffers)
;; If this value is t, cursor motion in the moccur-grep buffer causes
;; automatic display of the corresponding source code location.
(setq moccur-grep-following-mode-toggle t)

;; moccur-edit.el
;; color-moccur の検索結果を直接編集し，ファイルに変更を適用できる．
;; 関数名の変更などが簡単にできる.
;; (autoload 'moccur-edit "moccur-edit" "edit moccur buffer" nil t)
(require-if-not 'moccur-edit)

;; grep-edit -> wgrepに置き換えました。
;; grep の結果を編集し，その結果をもとにファイルを変更する．
;; (autoload 'grep-edit "edit grep result" nil t)
;; (require-if-not 'grep-edit)

;;;
;; customize font
;;;
;; Ref: https://www.shimmy1996.com/en/posts/2018-06-24-fun-with-fonts-in-emacs/
;; Ref: https://qiita.com/melito/items/238bdf72237290bc6e42
;; Ref: http://misohena.jp/blog/2017-09-26-symbol-font-settings-for-emacs25.html
;; Ref: https://www.reddit.com/r/emacs/comments/ggd90c/color_emoji_in_emacs_27/
(defvar user--default-font-size 18
  "Default font size.
カスタマイズする際はdefvarを~/.init_env_pre.elで定義してください。")

(defvar user--cjk-font "VL Gothic"
  "Default font for CJK characters")

(defvar user--latin-font "VL Gothic"
  "Default font for Latin characters")

(defvar user--cjk-proportional-font "VL PGothic"
  "Default font for Latin characters")

(defvar user--unicode-font "Noto Sans Mono CJK JP"
  "Default font for Unicode characters. including emojis")

(defvar user--unicode-emoji-font "Noto Color Emoji"
  "Default font for Unicode emoji characters.")

;; Notoフォントでベンガル語(charset名はbengali)を表示するとクラッシュする。
;; バックトレースを見るとlibm17n/libotf0でクラッシュしているようだ。
;; $ fc-list :lang=bn
;; を実行してベンガル語をサポートするフォント一覧を出力すると、
;; Notoフォント以外にFreeSansがある。
;; ので、FreeSansをフォールバックフォントとして用いる。
(defvar user--unicode-font-fallback "FreeSans"
  "Fallback font for Unicode characters.")

(defvar user--standard-fontset "fontset-user"
  "Standard fontset for user.")

(defun user--set-font ()
  "Set Unicode, Latin and CJK font for user--standard-fontset."
  ;; 記号にはデフォルトのフォントではなく指定のフォントを使いたい
  (setq use-default-font-for-symbols nil)
  (create-fontset-from-ascii-font user--cjk-font nil (replace-regexp-in-string "fontset-" "" user--standard-fontset))
  ;; unicodeに対してuser--cjk-fontがグリフを持っていればそれを使い、
  ;; 持っていない場合にはuser--unicode-fontで補完する
  (set-fontset-font user--standard-fontset 'unicode
                    (font-spec :family user--cjk-font :size user--default-font-size)
                    nil)
  (set-fontset-font user--standard-fontset 'unicode
                    (font-spec :family user--unicode-font :size user--default-font-size)
                    nil 'append)
  ;; latinに対してuser--latin-fontを使う
  (set-fontset-font user--standard-fontset 'latin
                    (font-spec :family user--latin-font :size user--default-font-size)
                    nil 'prepend)
  ;; CJKに対してuser--cjk-fontを使う
  (dolist (charset '(kana han cjk-misc hangul kanbun bopomofo))
    (set-fontset-font user--standard-fontset charset
                  (font-spec :family user--cjk-font :size user--default-font-size)
                  nil 'prepend))
  ;; symbolに対してuser--unicode-emoji-fontを使う
  (set-fontset-font t 'symbol (format "%s-%s" user--unicode-emoji-font user--default-font-size) nil 'append)
  ;; TODO 日本語フォントではU+2018とU+2019は全角幅だがWeb上の英文ではアポストロフィに使われていて
  ;; 見栄えが悪い。現状は全角で表示し必要に応じてU+0027に置換する。よい方法はないものか。
  (dolist (charset '((#x2018 . #x2019)    ;; Curly single quotes "‘’"
                     (#x201c . #x201d)))  ;; Curly double quotes "“”"
    (set-fontset-font user--standard-fontset charset
                      (font-spec :family user--cjk-font :size user--default-font-size)
                      nil)) ; 上書きするために第5引数ADDは省略する
  ;; フォールバックフォントを用いる言語(charsetは C-u C-x = のscriptセクションの名前を用いる)
  (dolist (charset '(bengali bengali-akruti bengali-cdac))
    (set-fontset-font user--standard-fontset charset
                      (font-spec :family user--unicode-font-fallback :size user--default-font-size)
                      nil 'prepend)))

(when window-system
  ;; create fontset-user
  (user--set-font)
  ;; Ensure user--standard-fontset gets used for new frames.
  (add-to-list 'default-frame-alist `(font . ,user--standard-fontset))
  (add-to-list 'initial-frame-alist `(font . ,user--standard-fontset)))

;;;
;; customize theme, color
;;;
(if (or (eq window-system 'x) (eq window-system 'w32))
    (if (package-installed-p 'solarized-theme)
        (load-theme 'solarized-dark t)
      (load-theme 'tango-dark t)))

;;;;;;;;
;; 色の設定
;;;;;;;;
(require-if-not 'font-lock)
(if (not (featurep 'xemacs)) (global-font-lock-mode t))
;; 全角スペースとかに色を付ける
;; 色はM-x list-color-displayで確認できる
(defface my-face-background-fullwidth-space '((t (:background "#9e9e9e"))) nil)
(defface my-face-background-tab-character '((t (:background "#003e4d"))) nil)
(defface my-face-underline-space '((t (:foreground "#8055aa" :underline t))) nil)
(defvar my-face-background-fullwidth-space 'my-face-background-fullwidth-space)
(defvar my-face-background-tab-character 'my-face-background-tab-character)
(defvar my-face-underline-space 'my-face-underline-space)
;;just in timeな色付け
(setq font-lock-support-mode 'jit-lock-mode)
(defadvice font-lock-mode (before my-font-lock-mode ())
  (font-lock-add-keywords major-mode
                          '(("　" 0 my-face-background-fullwidth-space append)
                            ("\t" 0 my-face-background-tab-character append)
                            ("[ ]+$" 0 my-face-underline-space append))))
(ad-enable-advice 'font-lock-mode 'before 'my-font-lock-mode)
(ad-activate 'font-lock-mode)
(add-hook 'find-file-hooks
          (lambda () (if font-lock-mode nil (font-lock-mode t))))

(if (not (or (eq system-type 'cygwin) (eq window-system 'x)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 追加の設定
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;; custom-fileをロードする
;;;
(load-env-if-exists custom-file)

;;;
;; 環境ごとの設定を~/.emacs.d/init_env_post.elに書く
;;;
(load-env-if-exists (expand-file-name "init_env_post.el" user-emacs-directory))

(provide 'init)
;;; init.el ends here
