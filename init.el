(require 'package)

;; Set up font and font size
(set-default-font "DejaVu Sans Mono")
(set-face-attribute 'default nil :height 105)

;; Activate linum-mode and format it
(global-linum-mode t)
(setq linum-format " %3d  " )

;; Disable linum-mode for ansi-term
(add-hook 'term-mode-hook (lambda()
                (linum-mode -1)))

;; Hilights current line
(global-hl-line-mode 0)

;; Add melpa package source when using package list
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

;; Load emacs packages and activate them
;; This must come brfore configurations of installed packages.
;; Don't delete this line.
(setq package-enable-at-startup nil) (package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#eaeaea" "#d54e53" "DarkOliveGreen3" "#e7c547" "DeepSkyBlue1" "#c397d8" "#70c0b1" "#181a26"))
 '(ansi-term-color-vector
   [unspecified "#1B2B34" "#EC5f67" "#99C794" "#FAC863" "#6699CC" "#C594C5" "#6699CC" "#C0C5CE"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (solarized-dark)))
 '(custom-safe-themes
   (quote
    ("f9574c9ede3f64d57b3aa9b9cef621d54e2e503f4d75d8613cbcc4ca1c962c21" "e43ef5f6c3ab5b692f457120bb5b227f1c2177777d2e2f6603059f08f4af1112" "4e753673a37c71b07e3026be75dc6af3efbac5ce335f3707b7d6a110ecb636a3" "7f968c172d6ec46766773a8304c7570bdff45f1220d3700008a437d9529ca3e4" "1d7e67fe9d8deacf470ffb2c6ccb181ac5c1af580f9edbdba90e6e0f1ba56ace" "db2ecce0600e3a5453532a89fc19b139664b4a3e7cbefce3aaf42b6d9b1d6214" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8ac2745fb5d9dad05f42228655508e14e4ce3a5adf64c9bedaa6e570a55f60be" default)))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(hl-paren-colors
   (quote
    ("#B9F" "#B8D" "#B7B" "#B69" "#B57" "#B45" "#B33" "#B11")))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (tide typescript-mode zenburn-theme yascroll web-mode use-package solarized-theme smooth-scrolling smex smartparens relative-line-numbers rainbow-mode protobuf-mode powerline oceanic-theme nord-theme nlinum nix-mode neotree monky mode-icons markdown-mode linum-relative less-css-mode lb-datalog-mode json-mode js2-mode ggtags flycheck fill-column-indicator fic-mode exec-path-from-shell evil-surround evil-mc dumb-jump counsel-projectile avy auto-complete arjen-grey-theme anything-project all-the-icons afternoon-theme abyss-theme)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 105 :width normal))))
 '(mode-line ((t (:background "#444c59" :foreground "#c9cfd8" :box nil :weight normal :height 1.0))))
 '(mode-line-inactive ((t (:foreground "#f9f9f9" :background "#666666" :box nil))))
 '(show-paren-match ((t (:background "burlywood")))))

;; Set cursor color
(set-cursor-color "#7FD6D6")

;; Disable scrollbar mode
(scroll-bar-mode -1)

;; Enable powerline and setup theme  #EAE6E6 #80ff00 DarkOrange #3BCE7B OliveDrab3  powerline-evil-center-color-theme
;; (add-to-list 'load-path "~/.emacs.d/vendor/powerline")
;; (require 'powerline)
;; (powerline-evil-theme)

;; Enable column number
(column-number-mode 1)
 ;;'(powerline-evil-normal-face ((t (:foreground "#282828" :background "#33E220"))))
 ;;'(powerline-evil-insert-face ((t (:foreground "#282828" :background "#2167DF")))))

;; Require neotree and set key bindings
(require 'neotree)
(global-set-key [f12] 'neotree-toggle)
(global-set-key [f5] 'neotree-toggle)

;; Activate EVIL MODE
(require 'evil)
  (evil-mode 1)

;; Disable evil mode when in netotree
(add-hook 'neotree-mode-hook 'evil-emacs-state)


;; Hilights matching parenthesis and customization.
(require 'paren)
(setq show-paren-delay 0)
(show-paren-mode 1)
(set-face-background 'show-paren-match "#ffc04d")
(set-face-foreground 'show-paren-match "#1B2B34")
(set-face-attribute 'show-paren-match nil :weight 'extra-bold)


;; Rule 80
(require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-column 80)
(setq fci-rule-color "#394C57")

;; Disable fci-mode for non required modes (buffers-messages, shell, ...)
(define-globalized-minor-mode global-fci-mode fci-mode
    (lambda ()
      (if (and
           (not (string-match "^\*.*\*$" (buffer-name)))
           (not (eq major-mode 'dired-mode)))
          (fci-mode 1))))
  (global-fci-mode 1)

;; Disable fci-mode when in netotree
(add-hook 'neotree-mode-hook (lambda()
                (fci-mode -1)))

;; Disable fci-mode in Org mode
(add-hook 'org-mode-hook (lambda()
                (fci-mode -1)))

;; Disable fci-mode when in term mode
;;(add-hook 'term-mode-hook (lambda()
;;                (fci-mode -1)))


;; Avoid using this: Function to disdable fci-mode for all buffers
(defun disable-fci-mode-each-buffer ()
  (interactive)
  (mapc (lambda (buffer)
          (condition-case nil
              (with-current-buffer buffer
                (global-fci-mode -1))
            (buffer-read-only nil)))
	(buffer-list)))

;; Avoid using this: Function to enable fci-mode for all buffers
(defun enable-fci-mode-each-buffer ()
  (interactive)
  (mapc (lambda (buffer)
          (condition-case nil
              (with-current-buffer buffer
                (global-fci-mode 1))
            (buffer-read-only nil)))
	(buffer-list)))

;; Disable hl-line-mode when in org mode
(add-hook 'org-mode-hook (lambda ()
     (setq-local global-hl-line-mode
           nil)))

;; Disable hl-line-mode within ansi-term/eshell
(add-hook 'eshell-mode-hook (lambda ()
     (setq-local global-hl-line-mode
           nil)))
(add-hook 'term-mode-hook (lambda ()
     (setq-local global-hl-line-mode
           nil)))

;; Disable highlight line when VISUAL mode. NOTE: The snippet below needs to be fixed
;;(defvar-local was-hl-line-mode-on nil)
;;(defun hl-line-on-maybe ()  (if was-hl-line-mode-on (hl-line-mode +1)))
;;(defun hl-line-off-maybe () (if was-hl-line-mode-on (hl-line-mode -1)))
;;(add-hook 'hl-line-mode-hook 
;;  (lambda () (if hl-line-mode (setq was-hl-line-mode-on t)))))
;;
;;(add-hook 'evil-visual-state-entry-hook 'hl-line-off-maybe)
;;(add-hook 'evil-visual-state-exit-hook 'hl-line-on-maybe)

;;(add-hook 'evil-visual-state-entry-hook (lambda() (global-hl-line-mode -1)))
;;(add-hook 'evil-visual-state-exit-hook (lambda() (global-hl-line-mode +1)))


;; Move line up/down. TODO: extract to another file.
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-p") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-n") 'move-line-down)
;; End move line


;; Add yasnippet load-path and enbale it globally
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)
(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'js-mode)))


;; Disable yasnippet when in term mode
(add-hook 'term-mode-hook (lambda()
                (yas-minor-mode -1)))

;; Set the indentation for javascript files
(setq js-indent-level 4)

;; Set block selection background color
(set-face-attribute 'region nil :background "#5f6f7f")

;; Enable lb-datalog-mode in emacs
(require 'lb-datalog-mode)

;; Enable winner mode (fast window configuration)
(when (fboundp 'winner-mode)
      (winner-mode 1))

;; Auto revert buffers when file changes on disk (useful for version controll)
(global-auto-revert-mode 1)

;; Enable evil surround mode.
(require 'evil-surround)
(global-evil-surround-mode 1)

;; Set up ivy, counsel and swiper
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; number of result lines to display
(setq ivy-height 13)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key read-expression-map (kbd "C-r") 'counsel-expression-history)

;; M-x enhancement for Emacs.
(require 'smex) 
(smex-initialize) 

;; Save emacs session
(desktop-save-mode 1)

;; Enable smartparens (auto pairs)
(require 'smartparens-config)
(add-hook 'js-mode-hook #'smartparens-mode)
(add-hook 'js2-mode-hook #'smartparens-mode)
(add-hook 'web-mode-hook #'smartparens-mode)
(add-hook 'python-mode-hook #'smartparens-mode)
(add-hook 'html-mode-hook #'smartparens-mode)
(add-hook 'lb-datalog-mode-hook #'smartparens-mode)
(add-hook 'sh-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)
;;(add-hook 'rjsx-mode-hook #'smartparens-mode)

;; Set line bar theme
(add-to-list 'load-path (expand-file-name "~/.emacs.d/custom-mode-line"))
(require 'custom-mode-line)

;; Enable flycheck mode
(add-hook 'js-mode-hook #'flycheck-mode)
(add-hook 'js2-mode-hook #'flycheck-mode)
(add-hook 'sh-mode-hook #'flycheck-mode)
(add-hook 'web-mode-hook #'flycheck-mode)
;;(add-hook 'rjsx-mode-hook #'flycheck-mode)
;;(setq-default flycheck-disabled-checkers
;;  (append flycheck-disabled-checkers
;;    '(javascript-jshint)))
;;(add-hook 'emacs-lisp-mode-hook #'flycheck-mode)
;;(global-flycheck-mode)

;; Set key binding to avy
(global-set-key (kbd "M-é") 'avy-goto-char)

;; Enable smooth-scrolling. (Scrolling like others ides)
(require 'smooth-scrolling)
(smooth-scrolling-mode 1)
(setq smooth-scroll-margin 3)
;;(setq scroll-conservatively 101) ;; move minimum when cursor exits view, instead of recentering
;;(setq mouse-wheel-scroll-amount '(1)) ;; mouse scroll moves 1 line at a time, instead of 5 lines
;;(setq mouse-wheel-progressive-speed nil) ;; on a long mouse scroll keep scrolling by 1 line

;; Disable this annoying thing :\ No newline at end of file
(setq require-final-newline nil)

;; Enable projectile-mode (should be integrated with counsel via counsel-projectile library)
(projectile-mode 1)

;; Integrate projectile with counsel_projectile.
(counsel-projectile-on)

;; Disable backup files.
(setq make-backup-files nil) ; stop creating backup~ files
(setq auto-save-default nil) ; stop creating #autosave# files

;; Enable auto-complete globally
(ac-config-default)
(global-auto-complete-mode t)
(setq ac-auto-show-menu 0.1) ;; Setting 0.1 not 0.0 to avoid conflict with Yasnippet.
(set-face-background 'ac-candidate-face "#4f687a")
(set-face-foreground 'ac-candidate-face "#dae3ea")
(set-face-underline 'ac-candidate-face "#536b72")
(set-face-background 'ac-selection-face "steelblue")

;; Markdow live preview plugin
(add-to-list 'load-path (expand-file-name "~/.emacs.d/emacs-livedown"))
(require 'livedown)

;; Temporary disable fci-mode/lines wrap when a pop_up is displayed (fix bug with auto-complete)

;;(defvar sanityinc/fci-mode-suppressed nil)
;;(defadvice popup-create (before suppress-fci-mode activate)
;;  "Suspend fci-mode while popups are visible"
;;  (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
;;  (when fci-mode
;;    (toggle-truncate-lines 0)
;;    (turn-off-fci-mode)))
;;(defadvice popup-delete (after restore-fci-mode activate)
;;  "Restore fci-mode when all popups have closed"
;;  (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
;;    (setq sanityinc/fci-mode-suppressed nil)
;;    (toggle-truncate-lines t)
;;    (turn-on-fci-mode)))

;; Enable EVIL MODE multi cursor.
(require 'evil-mc)
(global-evil-mc-mode  1)

;; Add gtags load-path and enbale it globally
;;(add-to-list 'load-path "~/.emacs.d/plugins/gtags")
;;(require 'gtags)

;; Set frames.
;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
;;(set-frame-parameter (selected-frame) 'alpha <both>)
;;(set-frame-parameter (selected-frame) 'alpha '(95 . 50))
;;(add-to-list 'default-frame-alist '(alpha . (95 . 50)))

;; Enable relative linum mode.
(require 'linum-relative)
(linum-relative-mode 1)
(setq linum-relative-format " %3s  ")
(setq linum-relative-current-symbol "")
(setq linum-relative-current-face t)


;; Set up prettier
;;(require 'prettier-js)
;;(add-hook 'js-mode-hook 'prettier-js-mode)
;;(add-hook 'js2-mode-hook 'prettier-js-mode)
;;(add-hook 'rjsx-mode-hook 'prettier-js-mode)
;;(add-hook 'web-mode-hook 'prettier-js-mode)


;; Set up Typescript dev env
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  ;;(setq flycheck-check-syntax-automatically '(save mode-enabled))
  (setq tide-format-options '(:indentSize 2 :tabSize 2))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
 ;; (company-mode +1))

;; aligns annotation to the right hand side
;;(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

(require 'flycheck)
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)


