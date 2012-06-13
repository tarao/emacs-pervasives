;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; emacs-pervasives ver. 1.00
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Include ~/.emacs.d in the load path
;; ~/.emacs.d下に入れたファイルをload,require等で読み込めるようにする
(setq load-path (cons "~/.emacs.d" load-path))

;; Include (sub-)directories under ~/.emacs.d/site-lisp in the load path
;; ~/.emacs.d/site-lisp下に入れたファイルを(サブディレクトリ内のものも
;; 含め)load,require等で読み込めるようにする
(when (fboundp 'normal-top-level-add-subdirs-to-load-path)
  (let* ((dir "~/.emacs.d/site-lisp")
         (default-directory dir))
    (when (file-directory-p dir)
      (setq load-path (cons dir load-path))
      (normal-top-level-add-subdirs-to-load-path))))

;; Do not show startup messages
;; 起動時に表示されるメッセージ, *scratch*バッファのメッセージ等を表示しない
(setq inhibit-startup-message t
      inhibit-startup-screen t
      initial-scratch-message nil)

;; Ask y/n instead of yes/no
;; yes/noではなくy/nで訊くようにする
(fset 'yes-or-no-p 'y-or-n-p)

;; Show row and column of the cursor position
;; カーソル位置の行番号と桁数を表示する
(setq column-number-mode t)

;; Hide toolbar
;; ツールバーを非表示にする
;(setq tool-bar-mode nil)

;; Hide menubar
;; メニューバーを非表示にする
;(setq menu-bar-mode nil)

;; Highlight matching parenthesis
;; 対応する括弧をハイライトする
(show-paren-mode t)

;; Highlight selected region
;; 選択範囲を表示する
(setq transient-mark-mode t)

;; Kill selected region by BS
;; 範囲選択中にバックスペースで選択範囲を削除する
(delete-selection-mode t)

;; Show line number
;; 行番号を(常に)表示する
(global-linum-mode)

;; Highlight the current line
;; カーソル位置の行をハイライトする
;(global-hl-line-mode)

;; Do not blink cursor
;; カーソルを点滅しない
(blink-cursor-mode nil)

;; Show special character (») for wrapped line end
;; 長い行を折り返したときに記号(»)を表示する
(defface wrap-face
  '((((class color) (min-colors 88) (background dark))
     :foreground "aquamarine4")
    (((class color) (min-colors 88) (background light))
     :foreground "aquamarine2")
    (((class color) (min-colors 16))
     :foreground "DarkCyan")
    (((class color) (min-colors 8))
     :foreground "gray")
    (((type tty) (class mono))
     :inverse-video t))
  "Face of the wrap."
  :group 'convenience)
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code #xbb 'wrap-face))

;; Highlight whitespace at EOL
;; 行末の空白をハイライトする
(setq-default show-trailing-whitespace t)

;; Show buffer boundary indicator
;; バッファの範囲を示すマークを表示する
(setq-default indicate-buffer-boundaries 'left)

;; C-x C-b shows buffer selector
;; C-x C-bでバッファ選択画面を開く
(global-set-key (kbd "C-x C-b") 'bs-show)

;; Put directory names for the same name of different files
;; 同じ名前のファイルを開いたときにfile<2>ではなくdir/fileという表示にする
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Auto completion like IntelliSense
;; [Key Bindings]
;;   M-n  next candidate
;;   M-p  previous candidate
;;   TAB  do complete
;;   C-m  do complete
;; IntelliSenseのようなオートコンプリート
;; [補完候補のキーバインド]
;;   M-n  次の候補
;;   M-p  前の候補
;;   TAB  候補の確定
;;   C-m  候補の確定
(when (require 'auto-complete nil t)
  (global-auto-complete-mode t)
  (setq ac-auto-show-menu 0.5))

;; Selecting candidates in a sophisticated way
;; [Key Bindings]
;;   C-n    next candidate
;;   Down   next candidate
;;   C-p    previous candidate
;;   Up     previous candidate
;;   Left   next source
;;   Right  previous source
;; 様々なものの選択を賢くする
;; [候補選択のキーバインド]
;;   C-n    次の候補
;;   Down   次の候補
;;   C-p    前の候補
;;   Up     前の候補
;;   Left   次の生成元
;;   Right  前の生成元
(when (require 'anything-config nil t)
  (setq dired-bind-jump nil) ; prevent binding to C-x C-j

  ;; Source of file list
  ;; ファイル選択で使うファイルリストの生成元
  (setq anything-for-files-prefered-list
      '(anything-c-source-buffers+
        anything-c-source-ffap-line
        anything-c-source-ffap-guesser
        anything-c-source-recentf
        anything-c-source-bookmarks
        anything-c-source-files-in-current-dir+
        anything-c-source-locate))

  ;; Replace switch-to-buffer with anything-for-files
  ;; バッファ選択(switch-to-buffer)の代わりにanythingを使う
  (global-set-key (kbd "C-x b") 'anything-for-files)

  ;; Enable anything on M-x
  ;; M-xでanythingを有効にする
  (when (require 'anything-complete nil t)
    (setq anything-complete-sort-candidates t)
    (substitute-key-definition 'execute-extended-command
                               'anything-execute-extended-command global-map))

  ;; Enable anything on M-x describe-bindings
  ;; M-x describe-bindingsでanythingを有効にする
  (when (require 'descbinds-anything nil t)
    (descbinds-anything-install)))

;; Use 4-space soft tab by default
;; デフォルトタブ幅4, タブはスペースを挿入
(setq-default tab-width 4
              indent-tabs-mode nil)

;; Treat undo history as a tree
;; [Key Bindings]
;;   C-/    undo
;;   M-_    redo
;;   C-x u  show history as a tree
;;     p    go up the tree (undo)
;;     n    go down the tree (redo)
;;     f    select right branch of the tree
;;     b    select left branch of the tree
;; 元に戻す/やり直しをツリー状に管理
;; [キーバインド]
;;   C-/    元に戻す
;;   M-_    やり直し
;;   C-x u  編集履歴ツリーを表示
;;     p    ツリーを上る (元に戻す)
;;     n    ツリーを下る (やり直し)
;;     f    右の枝を選択
;;     b    左の枝を選択
;;     q    ツリーを閉じる
(when (require 'undo-tree nil t)
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter nil))

;; Highlight the current line on GDB source buffer
;; GDBで実行中の行をハイライト
(defadvice gdb-display-source-buffer
  (after ad-hl-line-source-buffer (buffer) activate)
  (with-current-buffer buffer (hl-line-mode 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
;;   Compile background and show errors on the fly
;;   裏で自動コンパイルしてエラー箇所を表示

(require 'flymake)

;; Compilation runs when it is idle for 3 seconds
;; 3秒間操作しなかったらコンパイルする
(setq flymake-no-change-timeout 3)

;; colors
;; 色設定
(set-face-background 'flymake-errline "red4")
(set-face-foreground 'flymake-errline "black")
(set-face-background 'flymake-warnline "yellow")
(set-face-foreground 'flymake-warnline "black")

;; Shows errors on the minibuffer
;; エラーをミニバッファに表示
;; http://d.hatena.ne.jp/xcezx/20080314/1205475020
(defun flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no (flymake-current-line-no))
         (line-err-info-list
          (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count
          (length line-err-info-list)))
    (while (> count 0)
      (when line-err-info-list
        (let* ((file
                (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file
                (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line
                (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)))
      (setq count (1- count)))))

;; Automatically show errors on the minibuffer
;; エラーをミニバッファに表示するのを自動的にやる
(defadvice flymake-post-syntax-check
  (after flymake-display-err-minibuf-auto last activate)
  (flymake-display-err-minibuf))

;; Do nothing if visiting something other than a file
;; ファイル以外を編集中は何もしない
(defadvice flymake-get-init-function
  (around flymake-ignore-non-file (file-name) activate)
  (if file-name ad-do-it '(lambda () nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C/C++

;; Compiler paths
;; コンパイラのパス
(setq flymake-c-command "/usr/bin/gcc")
(setq flymake-cc-command "/usr/bin/g++")

;; Compiler options
;; コンパイラに渡すオプション
(setq flymake-cc-command-opt
      '("-fsyntax-only"
        ;; "-std=c++98" "-pedantic-errors"
        "-Wall" "-Wextra"
        ;; "-Wcast-qual" "-Wwrite-strings"
        ;; "-Wno-missing-field-initializers" "-Wnon-virtual-dtor"
        ;; "-Weffc++" "-Wold-style-cast" "-Woverloaded-virtual"
        ))

;; Pass include directory settings in Makefile to the compiler
;; Makefileにインクルード設定があったらそれもコンパイラに渡す関数
;; (ライブラリの利用のためにインクルードディレクトリを追加した場合に必要)
(defun flymake-extract-includes-from-makefile ()
  (let ((buf (current-buffer))
        (dir (file-name-directory (or (buffer-file-name) ""))))
    (with-temp-buffer
      (if (file-readable-p (concat dir "Makefile"))
          (progn
            (insert-file-contents (concat dir "Makefile") nil nil nil t)
            (goto-char 0)
            (if (re-search-forward "^INCLUDE\\s-*=\\s-*\\(.*\\)$" nil t nil)
                (let ((includes (split-string (match-string 1) " \t\r\n")))
                  (with-current-buffer buf
                    (set (make-local-variable 'flymake-cc-command-opt)
                         (append includes flymake-cc-command-opt))))))))))

;; Initialization for C/C++
;; C/C++用にflymakeを初期化する関数
(defun flymake-cc-init ()
  (let ((cmd (cond
              ((eq major-mode 'c-mode) flymake-c-command)
              ((eq major-mode 'c++-mode) flymake-cc-command)
              (t nil))))
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list cmd (append flymake-cc-command-opt (list local-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C

;; Enable c-mode and flymake automatically for the specific file extensions
;; C言語用モードを特定の拡張子で有効にする
(autoload 'c-mode "cc-mode")
(setq auto-mode-alist
      (append '(("\\.h$" . c-mode)
                ("\\.c$" . c-mode))
              auto-mode-alist))
(push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)

;; c-mode initialization
;; C言語用モード起動時にやること
(add-hook 'c-mode-hook
          '(lambda ()
             ;; indentation styles
             ;; インデントスタイル
             (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (setq c-basic-offset 2)
             ;; activate flymake
             ;; flymakeを有効にする
             (flymake-extract-includes-from-makefile)
             (flymake-mode t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C++

;; Enable cc-mode and flymake automatically for the specific file extensions
;; C++用モードを特定の拡張子で有効にする
(autoload 'c++-mode "cc-mode")
(setq auto-mode-alist
      (append '(;("\\.h$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.hxx$" . c++-mode)
                ("\\.cxx$" . c++-mode))
              auto-mode-alist))
(push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)

;; cc-mode initialization
;; C++用モード起動時にやること
(add-hook 'c++-mode-hook
          '(lambda ()
             ;; indentation styles
             ;; インデントスタイル
             (c-set-style "stroustrup")
             (c-set-offset 'innamespace 0)
             (setq c-basic-offset 2)
             ;; activate flymake
             ;; flymakeを有効にする
             (flymake-extract-includes-from-makefile)
             (flymake-mode t)))

;; emacs-pervasives ends here
