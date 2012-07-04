(require 'cl)
(when
  (load (expand-file-name "package.el" (file-name-directory load-file-name)))
  (load (expand-file-name "melpa.el" (file-name-directory load-file-name)))
  (package-initialize))

(add-to-list 'load-path (expand-file-name "." (file-name-directory load-file-name)))

(require 'package)
(setq package-user-dir (concat (expand-file-name "." (file-name-directory load-file-name)) "/elpa"))

(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("gnu" . "http://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-ruby starter-kit-js starter-kit-lisp starter-kit-bindings
                                  yaml-mode
                                  ruby-mode rinari flymake-ruby ruby-end ruby-tools rspec-mode ruby-block ruby-test-mode rvm
                                  coffee-mode flymake-coffee
                                  css-mode flymake-css flymake-csslint
                                  ir-black-theme
                                  flymake-jshint flymake-jslint
                                  haml-mode sass-mode scss-mode flymake-sass
                                  flymake-shell
                                  apache-mode
                                  full-ack save-visited-files
                                  projectile ; required by helm-projectile
                                  helm helm-projectile
                                  markdown-mode textile-mode
                                  )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(menu-bar-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; autosave on switching buffer
(defadvice switch-to-buffer (before save-buffer-now activate)
           (when buffer-file-name (save-buffer)))
(defadvice other-window (before other-window-now activate)
           (when buffer-file-name (save-buffer)))
(defadvice other-frame (before other-frame-now activate)
           (when buffer-file-name (save-buffer)))

; save visited files across sessions
(require 'save-visited-files)
(save-visited-files-mode t)

(require 'rspec-mode)
(require 'flymake)
(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)

(require 'helm-projectile)

; set the theme
; (require 'color-theme-ir-black)
(load-theme 'ir-black t)

; additional stuff to find projectile
(eval-after-load 'projectile
                 '(progn
                    (setq projectile-ignored-directories
                          (append projectile-ignored-directories '(".git" ".bundle" "tmp" "log" "coverage")))
                    (setq projectile-ignored-file-extensions
                          (append projectile-ignored-file-extensions '("gem" "jar" "o" "obj" "rbc" "DS_Store")))
                    )
                 )

(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "s-t") 'helm-projectile)
(global-set-key (kbd "s-F") 'ack)

; (setq auto-mode-alist      (cons '("\\.md" . markdown-mode) auto-mode-alist))

(add-to-list 'auto-mode-alist '("\\.text" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))

; Separate custom file
(setq custom-file (expand-file-name "custom.el" (file-name-directory load-file-name)))
(load custom-file 'noerror)
