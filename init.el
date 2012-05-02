(require 'cl)
(when
  (load (expand-file-name "~/.emacs.d/package.el"))
  (load (expand-file-name "~/.emacs.d/melpa.el"))
  (package-initialize))

(add-to-list 'load-path (expand-file-name "~/.emacs.d"))

(require 'package)
(add-to-list 'package-archives
             '("elpa" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(starter-kit starter-kit-ruby starter-kit-js starter-kit-lisp starter-kit-bindings
                                  yaml-mode ruby-mode rinari flymake-ruby
                                  coffee-mode flymake-coffee
                                  css-mode flymake-css flymake-csslint
                                  ir-black-theme
                                  flymake-jshint flymake-jslint
                                  flymake-sass
                                  flymake-shell
                                  apache-mode
                                  full-ack save-visited-files
                                  projectile ; required by helm-projectile
                                  helm helm-projectile
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

; set the theme
(load-theme 'ir-black)

; additional stuff to find projectile
(eval-after-load 'projectile
                 '(progn
                    (setq projectile-ignored-directories 
                          (append projectile-ignored-directories '(".git" ".bundle" "tmp" "log")))
                    (setq projectile-ignored-file-extensions 
                          (append projectile-ignored-file-extensions '("gem" "jar" "o" "obj" "rbc")))
                    )
                 )

(global-set-key (kbd "C-x C-f") 'helm-find-files)

; Separate custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

