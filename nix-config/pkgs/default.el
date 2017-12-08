;; globals 

(defun lambda-interactive (f &rest args)
  (lambda ()
    (interactive)
    (apply f args)))

;; config sections

(defun cfg_recent-file ()
  (require 'recentf)
  (recentf-mode t)  
  (setq recentf-max-saved-items 50)

  (defun find-file-ido ()
    "Find a recent file using Ido."
    (interactive)
    (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))

  (global-set-key (kbd "C-x C-r") #'find-file-ido))


(defun cfg_simpler ()
  (setq inhibit-splash-screen t)
  (setq initial-scratch-message "") 
  (setq inhibit-startup-echo-area-message "m")
  (blink-cursor-mode 0)
  (fringe-mode 4))


(defun cfg_editing ()
  (defun duplicate-line ()
    (interactive)
    (move-beginning-of-line 1)
    (kill-line)
    (yank)
    (open-line 1)
    (forward-line 1)
    (yank))

  (global-set-key (kbd "C-c d") #'duplicate-line)
  (global-set-key (kbd "C-c r") #'replace-string))


(defun cfg_paredit ()
  (defun duplicate-sexpr ()
    (interactive)
    (copy-region-as-kill (save-excursion
                           (paredit-backward)
                           (point))
                         (point))
    (paredit-newline)
    (yank))

  (defun copy-sexpr ()
    (interactive)
    (copy-region-as-kill (save-excursion
                           (paredit-backward)
                           (point))
                         (point)))

  (global-set-key (kbd "C-c x") #'duplicate-sexpr)
  (global-set-key (kbd "C-c c") #'copy-sexpr)
  (global-set-key (kbd "C-c p") #'paredit-mode))


(defun cfg_elisp ()
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'auto-complete-mode)
  (add-hook 'emacs-lisp-mode-hook #'company-mode))


(defun cfg_cider ()
  (add-hook 'cider-mode-hook #'aggressive-indent-mode)
  (add-hook 'cider-mode-hook #'paredit-mode)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-mode-hook #'auto-complete-mode)
  (add-hook 'cider-mode-hook #'company-mode)

  (add-hook 'cider-repl-mode-hook #'aggressive-indent-mode)
  (add-hook 'cider-repl-mode-hook #'paredit-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'auto-complete-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode))


(defun cfg_clojure ()
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-mode-hook #'paredit-mode)
  (add-hook 'clojure-mode-hook #'eldoc-mode)
  (add-hook 'clojure-mode-hook #'auto-complete-mode)
  (add-hook 'clojure-mode-hook #'company-mode))


(defun cfg_clojure-script ()
  (add-hook 'clojure-script-mode-hook #'aggressive-indent-mode)
  (add-hook 'clojure-script-mode-hook #'paredit-mode)
  (add-hook 'clojure-script-mode-hook #'eldoc-mode)
  (add-hook 'clojure-script-mode-hook #'auto-complete-mode)
  (add-hook 'clojure-script-mode-hook #'company-mode))


(defun cfg_startup ()
  (shell))


(defun cfg_shell () 
  (global-set-key (kbd "C-c s") 'shell))


(defun cfg_windows ()
  
  (defun set-window-abs-height (height)
    (enlarge-window (- height (window-total-height))))

  (defun set-window-abs-width (width)
    (enlarge-window-horizontally (- width (window-total-width))))

  (defun toggle-width () 
    (interactive)
    (let ((width (round (/ (frame-total-cols) 4))))
      (set-window-abs-width (if (= (window-total-width) width)
                                (- (frame-total-cols) width 1)
                              width))))
  
  (defun toggle-height ()
    (interactive)
    (let ((height (round (/ (frame-total-lines) 4))))
      (set-window-abs-height (if (= (window-total-height) height)
                                 (- (frame-total-lines) height 1)
                               height))))

  (windmove-default-keybindings)
  
  (global-set-key (kbd "C-c h") #'toggle-height)
  (global-set-key (kbd "C-c w") #'toggle-width))


(defun cfg_typography () 
  (global-set-key (kbd "C-x C-+")
                  (lambda-interactive 'text-scale-increase 0.2))

  (global-set-key (kbd "C-x C--")
                  (lambda-interactive 'text-scale-increase -0.2))

  (set-frame-font "DejaVu Sans Mono")
  (set-face-attribute 'default nil :height 87))


(defun cfg_language ()
  (setq lexical-binding t))


(defun cfg_minibuffer ()
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'show-paren-mode))


(defun cfg_elm ()
  (require 'flycheck)
  (require 'elm-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'flycheck-elm-setup))


(defun cfg_typescript ()

  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1) 
  (company-mode +1)

  ;; aligns annotation to the right hand side
  (setq company-tooltip-align-annotations t)

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)

  ;; tsx

  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)

  ;;
  (setq sp-base-key-bindings 'smartparens)

  ;; Always start smartparens mode in js-mode.
  (add-hook 'typescript-mode-hook #'smartparens-mode))


(progn
  (cfg_simpler)
  (cfg_language)
  (cfg_startup)
  (cfg_editing)
  (cfg_elm)
  (cfg_paredit)
  (cfg_recent-file) 
  (cfg_elisp)
  (cfg_clojure)
  (cfg_clojure-script)
  (cfg_cider)
  (cfg_shell)
  (cfg_windows)
  (cfg_typography)
  (cfg_minibuffer)
  (cfg_typescript))
