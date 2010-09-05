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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; .emacs より

;; php-mode
(autoload 'php-mode "php-mode" "PHP mode" t)

(defcustom php-file-patterns (list "\\.php[s34]?\\'" "\\.phtml\\'" "\\.inc\\'" "\\.ctp\\'")
  "*List of file patterns for which to automatically invoke php-mode."
  :type '(repeat (regexp :tag "Pattern"))
  :group 'php)

(let ((php-file-patterns-temp php-file-patterns))
  (while php-file-patterns-temp
    (add-to-list 'auto-mode-alist
                 (cons (car php-file-patterns-temp) 'php-mode))
    (setq php-file-patterns-temp (cdr php-file-patterns-temp))))

;;php-modeのインデントとタブ幅設定                                                                                                                                                
(add-hook 'php-mode-user-hook '(lambda ()
             (setq c-basic-offset 4)
             (setq c-tab-width 4)
             (setq c-indent-level 4)
             (setq tab-width 4)
             (setq indent-tabs-mode t)
             (setq-default tab-width 4)
))

;;php CodeIgniter style
(defun php-ci ()
  "php mode with adjusted defaults for use with the CodeIgniter Framework."
  (interactive)
  (php-mode)
  (setq c-basic-offset 4)
  (setq c-tab-width 4)
  (setq c-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode t)
  (setq-default tab-width 4)
  )

;; php CakePHP style
(defun php-cake ()
  "php mode with adjusted defaults for use with the CakePHP Framework."
  (interactive)
  (php-mode)
  (setq c-basic-offset 4)
  (setq c-tab-width 4)
  (setq c-indent-level 4)
  (setq tab-width 4)
  (setq indent-tabs-mode nil)
  (setq-default tab-width 4)
  )

;; ;; ruby-mode
;; (autoload 'ruby-mode "ruby-mode"
;;   "Mode for editing ruby source files" t)
;; (setq auto-mode-alist
;;       (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
;; (setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
;;                                      interpreter-mode-alist))
;; (autoload 'run-ruby "inf-ruby"
;;   "Run an inferior Ruby process")
;; (autoload 'inf-ruby-keys "inf-ruby"
;;   "Set local key defs for inf-ruby in ruby-mode")
;; (add-hook 'ruby-mode-hook
;;           '(lambda () (inf-ruby-keys)))

;; ;; ruby-electric
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; ;; rubydb
;; (autoload 'rubydb "rubydb3x"
;;   "run rubydb on program file in buffer *gud-file*.
;; the directory containing file becomes the initial working directory
;; and source-file directory for your debugger." t)

;; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;;(setq js2-indent-level 4)
;;(setq indent-tabs-mode t)

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