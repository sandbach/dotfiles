;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Asher Sandbach"
      user-mail-address "ajsandbach@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;; (setq doom-font (font-spec :family "Fira Code" :size 18)
;; (setq doom-font (font-spec :family "Cascadia Code" :size 18)
;; (setq doom-font (font-spec :family "Fixedsys Excelsior" :size 20)
;; (setq doom-font (font-spec :family "Go Mono" :size 18)
(setq doom-font (font-spec :family "Recursive Mono Linear Static" :size 18)
;; (setq doom-font (font-spec :family "Iosevka Term" :size 20 :style "Medium Extended")
      doom-variable-pitch-font (font-spec :family "Fira Sans")
      doom-unicode-font (font-spec :family "DejaVu Sans Mono"))
;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)
(setq doom-theme 'doom-gruvbox)
;; (setq doom-theme 'doom-monokai-pro)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Enable escape with "kj":
(key-chord-mode 1)
(key-seq-define-global "kj" 'evil-escape)
;; (setq-default evil-escape-key-sequence "kj")

;; My preferred mappings
(map! :n "C-l" 'evil-window-right
      :n "C-h" 'evil-window-left
      :n "C-j" 'evil-window-down
      :n "C-k" 'evil-window-up
      :n "C-s" 'evil-write
      :n "C-q" 'evil-quit
      :n "C-#" 'comment-line)

(map!
 :leader
 :prefix "r"
 :desc "evil-ex-nohighlight"
 "r" #'evil-ex-nohighlight)

(map!
 :leader
 :prefix "r"
 :desc "run-latexmk"
 "l" #'run-latexmk)

(map!
 :leader
 :prefix "r"
 :desc "toggle-visual-line-movement"
 "v" #'toggle-visual-line-movement)

;; Lisp
(setq inferior-lisp-program "sbcl")

(defun lisp-settings ()
  (rainbow-delimiters-mode)
  (display-fill-column-indicator-mode)
  (yas-minor-mode -1))

(add-hook 'lisp-mode-hook 'lisp-settings)
(add-hook 'slime-mode-hook 'lisp-settings)

(global-smart-tab-mode)

(setq-default indicate-empty-lines nil)

(setq-default scroll-margin 7)

(setq visual-line-movement nil)

(defun enable-visual-line-movement ()
  (interactive)
  (map! :n "j" 'evil-next-visual-line
        :n "k" 'evil-previous-visual-line)
  (setq visual-line-movement t))

(defun disable-visual-line-movement ()
  (interactive)
  (map! :n "j" 'evil-next-line
        :n "k" 'evil-previous-line)
  (setq visual-line-movement nil))

;; (defun toggle-visual-line-movement ()
;;   (interactive)
;;   (cond ((not visual-line-movement)
;;          (map! :n "j" 'evil-next-visual-line
;;                :n "k" 'evil-previous-visual-line)
;;          (setq visual-line-movement t))
;;         (t
;;          (map! :n "j" 'evil-next-line
;;                :n "k" 'evil-previous-line)
;;          (setq visual-line-movement nil))))

(defun toggle-visual-line-movement ()
  (interactive)
  (cond ((not visual-line-movement)
         (enable-visual-line-movement)
         (setq visual-line-movement t))
        (t
         (disable-visual-line-movement)
         (setq visual-line-movement nil))))

;; LaTeX
(setq-default TeX-engine 'xetex)

(defun run-latexmk ()
  (interactive)
  (let* ((file (buffer-file-name))
         (command-string
          (format "latexmk -xelatex -synctex=1 -cd -recorder- \"%s\"" file)))
    (compile command-string)))

(defun latex-settings ()
  (progn (setq visual-line-movement t)
         (enable-visual-line-movement)))

(add-hook 'latex-mode-hook 'latex-settings)

;; Org(-roam)
(org-roam-db-autosync-mode)

(let ((dropbox-org "~/Dropbox/org-roam"))
  (if (file-directory-p dropbox-org)
      (progn (setq org-roam-directory (file-truename dropbox-org))
             (org-roam-db-sync))))

(add-hook 'org-mode-hook 'enable-visual-line-movement)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)
   (emacs-lisp t)
   (python . t)))

(setq org-babel-lisp-eval-fn #'slime-eval)

;; C
(defun c-settings ()
  (display-fill-column-indicator-mode)
  (format-all-mode)
  (setq c-default-style "my-style"))

(add-hook 'c-mode-hook 'c-settings)

(setq-default tab-width 4)

(c-add-style "my-style"
             `("linux"
               (c-basic-offset . ,tab-width)))


;; (setq c-default-style "linux")

(use-package ligature
  :load-path "path-to-ligature-repo"
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                       ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                       "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                       "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                       "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                       "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                       "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                       "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                       ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                       "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                       "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                       "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                       "\\\\" "://"))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  ;; (global-ligature-mode t)
  )
