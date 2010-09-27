;; load-pathを追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
            (add-to-list 'load-path default-directory)
            (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
                (normal-top-level-add-subdirs-to-load-path))))))

;; elispとconfディレクトリをサブディレクトリごとload-pathに追加
(add-to-load-path "elisp" "conf")

;; cake el
(require 'cl)
(require 'anything)
(require 'historyf)
(require 'cake)
;; cake.elが自動的にカレントファイルがCakePHPプロジェクト内のファイルか
;; どうかを判定し、マイナーモードを有効にします。
(global-cake t)
;; デフォルトのキーバインドを有効化します。
(cake-set-default-keymap)
;; AutoComplete Support
(require 'ac-cake)
(add-hook 'php-mode-hook
          (lambda ()
            (make-local-variable 'ac-sources)
            (setq ac-sources '(ac-source-cake
                               ac-source-gtags
                               ac-source-yasnippet
                               ac-source-php-completion
                               ))))
;; .ctp用にnxml-modeをload
(load "~/.emacs.d/elisp/nxml-mode-20041004/rng-auto.el")

;; (install-elisp "http://www.emacswiki.org/emacs/download/auto-install.el")
(when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する 初期値は ~/.emacs.d/auto-install/
  (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  (auto-install-update-emacswiki-package-name t)
  ;; install-elispの関数を利用可能にする
  (auto-install-compatibility-setup))

;;起動時のメッセージを消す
(setq inhibit-startup-message t)

;; menu-barを非表示
(menu-bar-mode 0)

;;tool-barを非表示
(tool-bar-mode 0)

;;文字コードのデフォルトはUTF-8
(set-default-coding-systems 'utf-8)

;;コード中の文字コードを解釈しないようにする設定
(setq auto-coding-functions nil)

;;elisp load
(setq load-path
      (append
       (list
        (expand-file-name "~/.emacs.d/elisp/")
        )
       load-path))

;;C-hをバックスペースに割り当てる
(define-key global-map "\C-h" 'delete-backward-char)

;;動的略語展開
(define-key global-map "\C-o" 'dabbrev-expand)

;;右端で折り返さない
(setq truncate-lines t)
(setq truncate-partial-width-windows t)

;;自動バックアップを作成しない
(setq backup-inhibited t)

;;auto-save
(require 'auto-save-buffers)
(run-with-idle-timer 0.7 t 'auto-save-buffers)

;; 対応する括弧をハイライト
(show-paren-mode t)

;; バッファ移動
(iswitchb-mode 1)

;; ファイルパス表示 -> screenだとダメ!?
;;(setq frame-title-format
;;      (format "%%f - Emacs@%s" (system-name)))

;; auto-complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elisp/ac-dict")
(ac-config-default)

;; Egg( Git fornt end )
;; (install-elisp "http://github.com/byplayer/egg/raw/master/egg.el")
(when (executable-find "git"))

;; tramp
(add-to-list 'load-path "~/.emacs.d/elisp/tramp/lisp/")
(require 'tramp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; .emacs より

;; php-mode
(load-library "php-mode")
(require 'php-mode)
(setq php-mode-force-pear t)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-indent-level 4)
(setq indent-tabs-mode t)

;;; dired を使って、一気にファイルの coding system (漢字) を変換する
(require 'dired-aux)
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key (current-local-map) "T"
              'dired-do-convert-coding-system)))

(defvar dired-default-file-coding-system nil
  "*Default coding system for converting file (s).")

(defvar dired-file-coding-system 'no-conversion)

(defun dired-convert-coding-system ()
  (let ((file (dired-get-filename))
        (coding-system-for-write dired-file-coding-system)
        failure)
    (condition-case err
        (with-temp-buffer
          (insert-file file)
          (write-region (point-min) (point-max) file))
      (error (setq failure err)))
    (if (not failure)
        nil
      (dired-log "convert coding system error for %s:\n%s\n" file failure)
      (dired-make-relative file))))

(defun dired-do-convert-coding-system (coding-system &optional arg)
  "Convert file (s) in specified coding system."
  (interactive
   (list (let ((default (or dired-default-file-coding-system
                            buffer-file-coding-system)))
           (read-coding-system
            (format "Coding system for converting file (s) (default, %s): "
                    default)
            default))
         current-prefix-arg))
  (check-coding-system coding-system)
  (setq dired-file-coding-system coding-system)
  (dired-map-over-marks-check
   (function dired-convert-coding-system) arg 'convert-coding-system t))

(setq dired-default-file-coding-system 'utf-8)

;; タブ, 全角スペース、改行直前の半角スペースを表示する
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'text-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?.))
  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                (face-spec-set 'jaspace-highlight-jaspace-face
                               '((((class color) (background light))
                                  (:foreground "blue"))
                                 (t (:foreground "green"))))
                (face-spec-set 'jaspace-highlight-tab-face
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "purple"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))
                (face-spec-set 'trailing-whitespace
                               '((((class color) (background light))
                                  (:foreground "red"
                                               :background "unspecified"
                                               :strike-through nil
                                               :underline t))
                                 (t (:foreground "purple"
                                                 :background "unspecified"
                                                 :strike-through nil
                                                 :underline t))))))))