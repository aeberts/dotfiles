;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Alexander Eberts"
      user-mail-address "alex.eberts@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 13))
;; (setq doom-font (font-spec :family "Menlo" :size 12))
;; (setq doom-font (font-spec :family "SF Mono" :size 12))
;; (setq doom-variable-pitch-font (font-spec :family "sans"))

;; Change local leader key to , instead of SPC m
;; we lose the evil-snipe-backwards-f but I don't use it anyway
(setq doom-localleader-key ",")

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;; Configure themes
(use-package doom-themes
  ;; https://github.com/hlissner/emacs-doom-themes/tree/screenshots
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'leuven t) ; my main light theme
  ;; (load-theme 'leuven-dark t) ; dark version of leuven (not my favourite)
  ;; (setq doom-theme 'doom-one) ; my main dark theme
  ;; (setq doom-theme 'doom-spacegrey)
  ;; (setq doom-theme 'doom-wilmersdorf)
  ;; (setq doom-theme 'doom-palenight)
  ;; (setq doom-theme 'doom-tomorrow-day)
  ;; (setq doom-theme 'doom-one-light)
  ;; (setq doom-theme 'spacemacs-light)
  ;; (setq doom-theme 'cyberpunk-theme)
  ;; (load-theme 'adwaita t)            ; buit in light theme
  ;;
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;; Configure Highlight Mode in visual line mode:
;; from: https://stackoverflow.com/questions/31541907/highlight-a-single-line-in-a-larger-word-wrapped-line;
(defun visual-line-range ()
  (save-excursion
    (cons
     (progn (beginning-of-visual-line) (point))
     (progn (end-of-visual-line) (point)))))

(setq hl-line-range-function 'visual-line-range)

;; Helm Configuration
;; Use max length of buffer names before truncating
;; https://emacs.stackexchange.com/questions/485/how-to-widen-helm-buffer-names-in-helm-buffers-list
(setq helm-buffer-max-length nil)

;; Company Mode
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/completion/company/README.org
;;
;; Trigger company files with C-x C-f
(setq company-backends '(company-files company-capf))

;; Disable company-mode by default in org-mode files
;;(setq company-global-modes '(not org-mode))

;; Turn off auto completion
;; https://github.com/company-mode/company-mode/issues/773
(setq company-idle-delay nil)

(map! :after company
      :map company-mode-map
      :desc "C-x j" :i "C-x j" #'company-ispell)

;; Not a company-mode binding but hippie-expand can be useful.
;; Default binding is M-/

;; Flyspell and Spell-fu Settings
;;
;; Disable spell-fu by default - doesn't work
;; (spell-fu-mode-disable)
;; (remove-hook 'org-mode-hook #'spell-fu-mode)

;; Set the default language for the personal dictionary:
(setq ispell-dictionary "en")

;; Don't spellcheck links in markdown documents
(defun +markdown-flyspell-word-p ()
  "Return t if point is on a word that should be spell checked.

Return nil if on a link url, markup, html, or references."
  (let ((faces (doom-enlist (get-text-property (point) 'face))))
    (or (and (memq 'font-lock-comment-face faces)
             (memq 'markdown-code-face faces))
        (not (cl-loop with unsafe-faces = '(markdown-reference-face
                                            markdown-url-face
                                            markdown-markup-face
                                            markdown-comment-face
                                            markdown-html-attr-name-face
                                            markdown-html-attr-value-face
                                            markdown-html-tag-name-face
                                            markdown-code-face)
                      for face in faces
                      if (memq face unsafe-faces)
                      return t)))))

(set-flyspell-predicate! '(markdown-mode gfm-mode)
  #'+markdown-flyspell-word-p)

;; Org-Mode configuration
;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Dropbox/org/")
(setq org-startup-folded 1)
;; (setq org-ellipsis " ▼ ")
(require 'org-tempo) ;; org-tempo allows us to type <s TAB and complete org-mode templates.

;; ;; Customize org-mode structure templates
;; https://orgmode.org/manual/Structure-Templates.html#Structure-Templates
;; https://emacs.stackexchange.com/questions/63875/emacs-org-mode-shortcut-to-create-code-block
(add-to-list 'org-structure-template-alist
             '("z" . "src clojure"))

;reset the org-structure-template-alist in case of errors
;; (custom-reevaluate-setting 'org-structure-template-alist)

;; Org-Roam Settings:
;;
(setq org-roam-directory "~/Dropbox/org/org-roam")
(setq org-roam-index-file "index.org")
(setq org-roam-capture-templates
      '(
        ("d" "default" plain #'org-roam-capture--get-point
         "%?"
         :file-name "${slug}"
         :head "#+title: ${title}\n#+roam_tags:\n\nSummary ::\nSource ::\n\n"
         :unnarrowed t)))

;; See /Users/zand/.doom-emacs/.local/straight/repos/org-mode/lisp/org.el:1163
(setq org-show-context-detail
      '((agenda . local)
        (bookmark-jump . lineage)
        (isearch . lineage)
        (default . lineage)))

;; Autocompletion for org-roam
;; add company-dabbrev to the set-company-backend to complete regular words in org files.
;; (require 'company-org-roam)
;; (use-package company-org-roam
;;   :when (featurep! :completion company)
;;   :after org-roam
;;   :config
;;   (set-company-backend! 'org-mode '(company-org-roam company-yasnippet)))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.
;;

;; Base Functions and Navigation

;; Switch to the new window after splitting
(setq evil-split-window-below t
      evil-vsplit-window-right t);

;; evil-escape allows you to type chorded key sequences to quit
(after! evil-escape
  (setq evil-escape-key-sequence "fd")
  (setq evil-escape-unordered-key-sequence t))

;; Treemacs Configuration
(use-package treemacs
  :bind
  (:map global-map
    ("M-0"       . treemacs-select-window)
    ("C-x t 1"   . treemacs-delete-other-windows)
    ("C-x t t"   . treemacs)
    ("C-x t B"   . treemacs-bookmark)
    ("C-x t C-t" . treemacs-find-file)
    ("C-x t M-t" . treemacs-find-tag)))

;; Lispy Configuration
(setq lispyville-key-theme
        '((operators normal)
          c-w
          (prettify insert)
          (atom-movement t)
          slurp/barf-lispy
          additional
          additional-insert
          additional-motions
          commentary))

;; Clojure / Cider mode configurations.
;;

;; Display an overlay of the currently selected history item in buffer
(setq cider-repl-history-show-preview t)
;; Syntax highlight overlays in cider buffers
(setq cider-overlays-use-font-lock t)
;; Set the default cljs repl type
(setq cider-default-cljs-repl 'shadow)

;; The following is supposed to fix cider mode from the Doom Emacs discord channel
;; (after! cider
;;   (add-hook 'company-completion-started-hook 'custom/set-company-maps)
;;   (add-hook 'company-completion-finished-hook 'custom/unset-company-maps)
;;   (add-hook 'company-completion-cancelled-hook 'custom/unset-company-maps))

;; (defun custom/unset-company-maps (&rest unused)
;;   "Set default mappings (outside of company).
;;     Arguments (UNUSED) are ignored."
;;   (general-def
;;     :states 'insert
;;     :keymaps 'override
;;     "<down>" nil
;;     "<up>"   nil
;;     "RET"    nil
;;     [return] nil
;;     "C-n"    nil
;;     "C-p"    nil
;;     "C-j"    nil
;;     "C-k"    nil
;;     "C-h"    nil
;;     "C-u"    nil
;;     "C-d"    nil
;;     "C-s"    nil
;;     "C-S-s"   (cond ((featurep! :completion helm) nil)
;;                     ((featurep! :completion ivy)  nil))
;;     "C-SPC"   nil
;;     "TAB"     nil
;;     [tab]     nil
;;     [backtab] nil))

;; (defun custom/set-company-maps (&rest unused)
;;   "Set maps for when you're inside company completion.
;;     Arguments (UNUSED) are ignored."
;;   (general-def
;;     :states 'insert
;;     :keymaps 'override
;;     "<down>" #'company-select-next
;;     "<up>" #'company-select-previous
;;     "RET" #'company-complete
;;     [return] #'company-complete
;;     "C-w"     nil  ; don't interfere with `evil-delete-backward-word'
;;     "C-n"     #'company-select-next
;;     "C-p"     #'company-select-previous
;;     "C-j"     #'company-select-next
;;     "C-k"     #'company-select-previous
;;     "C-h"     #'company-show-doc-buffer
;;     "C-u"     #'company-previous-page
;;     "C-d"     #'company-next-page
;;     "C-s"     #'company-filter-candidates
;;     "C-S-s"   (cond ((featurep! :completion helm) #'helm-company)
;;                     ((featurep! :completion ivy)  #'counsel-company))
;;     "C-SPC"   #'company-complete-common
;;     "TAB"     #'company-complete-common-or-cycle
;;     [tab]     #'company-complete-common-or-cycle
;;     [backtab] #'company-select-previous))

;; {{{ ==== Begin Custom Keybindings
;;
;; General Information on customizing key bindings:
;; https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-do-i-bind-my-own-keys-or-change-existing-ones
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/+evil-bindings.el

;; unbind C-m from RET - doesn't work because C-m is the default?
;;(map! :map "C-m" nil)
;;
;; Custom Movement Keys in Insert Mode (doesn't work for org-mode argh)

;; (map! :i "C-j" #'evil-next-line) ;; replaces +default/newline
;; (map! :i "C-k" #'evil-previous-line) ;; replaces n: C-k kill-line i: C-k evil-insert-digraph
;; (map! :i "C-h" #'evil-backward-char) ;; replaces C-h for help which is also bound to F1
;; (map! :i "C-l" #'evil-forward-char) ;; replaces i: recenter-top-bottom n: recenter-top-bottom
;; (map! :i "C-;" #'doom/forward-to-last-non-comment-or-eol) ;; i: C-; nil by default

;; (map! :i "C-M-h" #'evil-backward-word-begin) ;; replaces mark-defun use expand-region instead?
;; (map! :i "C-M-l" #'evil-forward-word-begin) ;; replaces reposition-window
;;
;; Unbind text-scale-decrease because I hit it by accident
(map! :n "C--" nil)

;; Reference https://github.com/hlissner/doom-emacs/issues/2403
(map! :map general-override-mode-map
      :i "C-j" #'evil-next-line         ;; replaces +default/newline
      :i "C-k" #'evil-previous-line     ;; replaces n: C-k kill-line i: C-k evil-insert-digraph
      :i "C-h" #'evil-backward-char     ;; replaces C-h for help which is also bound to F1
      :i "C-l" #'evil-forward-char      ;; replaces i: recenter-top-bottom n: recenter-top-bottom
      :i "C-M-h" #'evil-backward-word-begin ;; replaces mark-defun consider using expand-region instead?
      :in "C-;" #'doom/forward-to-last-non-comment-or-eol ;; i: C-; nil by default
      :in "C-'" #'doom/forward-to-last-non-comment-or-eol ;; n: C-' nil by default
      )
;;
;; Expand Region
(map!
 :nv "C-+" #'er/contract-region
 :nv "C-=" #'er/expand-region
 :nv "C-z" #'evil-undo)

;; Map C-- to expand region because I always miss it.
(map! :nv "C--" #'er/expand-region)

;; ==== End Custom Keybinding }}}

;; Toggles
;;
;; Toggle company-mode completions globally
(map! :leader
      (:prefix ("t" . "toggle")
       (:desc "Autocomplete" "a" #'+company/toggle-auto-completion)))

;; My Custom Functions

;; Adapted from
;; https://github.com/abo-abo/avy/issues/89

(defun ae/avy-goto-char-in-defun (char)
  "Jump to a character in the defun at point."
  (interactive "cchar: ")
  (let ((beg nil) (end nil))
    (save-excursion
      (beginning-of-defun)
      (setq beg (point))
      (end-of-defun)
      (setq end (point)))
    (avy--generic-jump (string char) nil beg end)))

(defun ae/avy-goto-paren-in-defun ()
  "Jump to an open paren in the defun at point."
  (interactive)
  (let ((beg nil) (end nil))
    (save-excursion
      (beginning-of-defun)
      (setq beg (point))
      (end-of-defun)
      (setq end (point)))
    (avy--generic-jump (regexp-quote "(" ) nil beg end)))

(defun xah-unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive)
  (let ((fill-column most-positive-fixnum))
    (fill-paragraph)))

(defun xah-unfill-region (start end)
  "Replace newline chars in region by single spaces.
This command does the inverse of `fill-region'.

URL `http://ergoemacs.org/emacs/emacs_unfill-paragraph.html'
Version 2016-07-13"
  (interactive "r")
  (let ((fill-column most-positive-fixnum))
    (fill-region start end)))

;; From https://stackoverflow.com/questions/10681766/emacs-org-mode-textual-reference-to-a-fileline
;; Copy file and position reference to the kill ring
(defun position-to-kill-ring ()
  "Copy to the kill ring a string in the format \"file-name:line-number\"
   for the current buffer's file name, and the line number at point."
  (interactive)
  (kill-new
   (format "%s:%d" (buffer-file-name) (save-restriction
                                        (widen) (line-number-at-pos)))))

;; From https://stackoverflow.com/questions/2926088/how-to-indent-a-buffer-in-ess
;; Re-indent whole buffer
(defun z-indent-buffer()
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max))))

(defun flush-blank-lines (start end)
  "This will flush blank lines and works with a region.

URL `https://masteringemacs.com/article/removing-blank-lines-buffer'
"
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

;; Set the initial frame size
(setq initial-frame-alist '((top . 1) (left . 1) (width . 157) (height . 81)))
