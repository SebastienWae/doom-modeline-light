;;; doom-modeline-light.el --- Slimmed down version of `doom-modeline' -*- lexical-binding: t -*-

;; Author: Sebastien Waegeneire
;; Created: 2020
;; License: GPL-3.0-or-later
;; Version: 0.1
;; Package-Requires: ((all-the-icons "4.0.0"))
;; Homepage: https://github.com/sebastienwae/doom-modeline-light

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a slimmed down version of `doom-modeline' that manipulates
;; `mode-line-format' directly

;;; Credits:

;; This package was extracted from `doom-emacs' by hlissner.
;; https://github.com/hlissner/doom-emacs
;; https://github.com/hlissner/doom-emacs/blob/develop/modules/ui/modeline/%2Blight.el

(require 'all-the-icons)

(defun +modeline--set-var-and-refresh-bars-fn (&optional symbol value)
  (when symbol
    (set-default symbol value))
  (when (fboundp '+modeline-refresh-bars-h)
    (+modeline-refresh-bars-h)))

;;
;;; Variables

(defcustom +modeline-height 31
  "The height of the modeline.
This is enforced by the xpm bitmap bar in `+modeline-bar'. Without it (and in
the terminal), this variable does nothing.
Use `setq!' to adjust this variable live, as it will trigger an refresh of the
bars in the modeline. `setq' will not."
  :type 'integer
  :set #'+modeline--set-var-and-refresh-bars-fn)

(defcustom +modeline-bar-width 3
  "The width of the bar in the modeline.
If nil, the bar will be made transparent and 1 pixel wide, as to be invisible,
but without sacrificing its ability to enforce `+modeline-height'.
Use `setq!' to adjust this variable live, as it will trigger an refresh of the
bars in the modeline. `setq' will not."
  :type 'integer
  :set #'+modeline--set-var-and-refresh-bars-fn)

(defvar +modeline-format-alist ()
  "An alist of modeline formats defined with `def-modeline!'.
Each entry's CAR is the name and CDR is a cons cell whose CAR is the left-hand
side of the modeline, and whose CDR is the right-hand side.")


;;
;;; Faces

(defface doom-modeline-bar '((t (:inherit highlight)))
  "Face used for left-most bar on the mode-line of an active window.")

(defface doom-modeline-bar-inactive '((t (:inherit mode-line-inactive)))
  "Face used for left-most bar on the mode-line of an inactive window.")

(defface doom-modeline-highlight
  '((t (:inherit mode-line-highlight)))
  "Face used for highlighted modeline panels (like search counts).")

(defface doom-modeline-alternate-highlight
  '((t (:inherit mode-line-highlight)))
  "Alternative face used for highlighted modeline panels (like search counts).")


;;
;;; Helpers

;;; `active'
(defvar +modeline--active-window (selected-window))

(defun +modeline-active ()
  "Return non-nil if the selected window has an active modeline."
  (eq (selected-window) +modeline--active-window))

(defun +modeline-set-selected-window-h (&rest _)
  "Track the active modeline's window in `+modeline--active-window'."
  (let ((win (selected-window)))
    (unless (minibuffer-window-active-p win)
      (setq +modeline--active-window (frame-selected-window)))))
(add-hook 'pre-display-functions #'modeline-set-selected-window-h)

(defun +modeline--make-xpm (color width height)
  "Create an XPM bitmap via COLOR, WIDTH and HEIGHT. Inspired by `powerline''s `pl/+modeline--make-xpm'."
  (propertize
   " " 'display
   (let ((data (make-list height (make-list width 1)))
         (color (or color "None")))
     (ignore-errors
       (create-image
        (concat
         (format "/* XPM */\nstatic char * percent[] = {\n\"%i %i 2 1\",\n\". c %s\",\n\"  c %s\","
                 (length (car data))
                 (length data)
                 color
                 color)
         (apply #'concat
                (cl-loop with idx = 0
                         with len = (length data)
                         for dl in data
                         do (cl-incf idx)
                         collect
                         (concat "\""
                                 (cl-loop for d in dl
                                          if (= d 0) collect (string-to-char " ")
                                          else collect (string-to-char "."))
                                 (if (eq idx len) "\"};" "\",\n")))))
        'xpm t :ascent 'center)))))

(defun +modeline-format-icon (icon label &optional face help-echo voffset)
  (propertize (concat (all-the-icons-material
                       icon
                       :face face
                       :height 1.1
                       :v-adjust (or voffset -0.225))
                      (propertize label 'face face))
              'help-echo help-echo))

(defun set-modeline! (name &optional default)
  "Set the modeline to NAME.
If DEFAULT is non-nil, apply to all future buffers. Modelines are defined with
`def-modeline!'."
  (if-let (format (assq name +modeline-format-alist))
      (cl-destructuring-bind (lhs . rhs) (cdr format)
        (if default
            (setq-default +modeline-format-left lhs
                          +modeline-format-right rhs)
          (setq +modeline-format-left lhs
                +modeline-format-right rhs)))
    (error "Could not find %S modeline format" name)))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun set-modeline-hook! (hooks name)
  "Set the modeline to NAME on HOOKS.
See `def-modeline!' on how modelines are defined."
  (let ((fn (intern (format "+modeline-set-%s-format-h" name))))
    (dolist (hook (doom-enlist hooks))
      (when after-init-time
        (dolist (name (mapcar #'car +modeline-format-alist))
          (remove-hook hook (intern (format "+modeline-set-%s-format-h" name)))))
      (add-hook hook fn))))

(defun def-modeline! (name lhs rhs)
  "Define a modeline format by NAME.
LHS and RHS are the formats representing the left and right hand side of the
mode-line, respectively. See the variable `format-mode-line' for details on what
LHS and RHS will accept."
  (setf (alist-get name +modeline-format-alist)
        (cons lhs rhs))
  (fset (intern (format "+modeline-set-%s-format-h" name))
        (lambda (&rest _) (set-modeline! name))))

(defmacro def-modeline-var! (name body &optional docstring &rest plist)
  "Define a modeline segment variable."
  (unless (stringp docstring)
    (push docstring plist)
    (setq docstring nil))
  `(progn
     (defconst ,name ,body ,docstring)
     ,@(if (plist-get plist :local) `((make-variable-buffer-local ',name)))
     (put ',name 'risky-local-variable t)))


;;
;;; Segments

(def-modeline-var! +modeline-format-left nil
  "The left-hand side of the modeline."
  :local t)

(def-modeline-var! +modeline-format-right nil
  "The right-hand side of the modeline."
  :local t)


;;; `+modeline-bar'
(def-modeline-var! +modeline-bar "")

(defvar +modeline-active-bar "")
(defvar +modeline-inactive-bar "")

(defun +modeline-refresh-bars-h ()
  (let ((width (or +modeline-bar-width 1))
	(height (max +modeline-height 0))
	(active-bg (face-background 'doom-modeline-bar nil t))
	(inactive-bg (face-background 'doom-modeline-bar-inactive nil t)))
    (when (or (null +modeline-bar-width)
	      (= +modeline-bar-width 0))
      (setq active-bg nil
	    inactive-bg nil))
    (setq +modeline-active-bar
	  (+modeline--make-xpm (and +modeline-bar-width active-bg)
			       width height)
	  +modeline-inactive-bar
	  (+modeline--make-xpm (and +modeline-bar-width inactive-bg)
			       width height)
	  +modeline-bar
	  '(:eval (if (+modeline-active)
		      +modeline-active-bar
		    +modeline-inactive-bar)))))
(add-hook 'window-setup-hook #'+modeline-refresh-bars-h)

(defun +modeline-adjust-height-h ()
  (defvar +modeline--old-height +modeline-height)
  (let ((default-height +modeline--old-height)
	(scale (or (frame-parameter nil 'font-scale) 0)))
    (setq +modeline-height
	  (if (> scale 0)
	      (+ default-height (* (or (frame-parameter nil 'font-scale) 1)
				   2))
	    default-height))
    (when (fboundp '+modeline-refresh-bars-h)
      (+modeline-refresh-bars-h))))
(add-hook 'after-setting-font-hook #'+modeline-adjust-height-h)

;;; `+modeline-matches'
(defun +modeline-setup-anzu ()
  (when (require 'anzu nil 'noerror)
    ;; We manage our own modeline segments
    (setq anzu-cons-mode-line-p nil)
    ;; Ensure anzu state is cleared when searches & iedit are done
    (add-hook 'iedit-mode-end-hook #'anzu--reset-status)
    (advice-add #'evil-force-normal-state :before #'anzu--reset-status)
    ;; Fix matches segment mirroring across all buffers
    (mapc #'make-variable-buffer-local
	  '(anzu--total-matched
	    anzu--current-position
	    anzu--state
	    anzu--cached-count
	    anzu--cached-positions anzu--last-command
	    anzu--last-isearch-string anzu--overflow-p))))
(add-hook 'isearch-mode #'+modeline-setup-anzu)

(defun +modeline-setup-evil-anzu ()
  (when (require 'evil-anzu nil 'noerror)
    (global-anzu-mode +1)))
(when evil-mode
  (add-hook 'evil-ex-start-search #'+modeline-setup-evil-anzu)
  (add-hook 'evil-ex-start-word-search #'+modeline-setup-evil-anzu)
  (add-hook 'evil-ex-search-activate-highlight #'+modeline-setup-evil-anzu))

(defun +modeline--anzu ()
  "Show the match index and total number thereof.
Requires `anzu', also `evil-anzu' if using `evil-mode' for compatibility with
`evil-search'."
  (when (and (bound-and-true-p anzu--state)
             (not (bound-and-true-p iedit-mode)))
    (propertize
     (let ((here anzu--current-position)
           (total anzu--total-matched))
       (cond ((eq anzu--state 'replace-query)
              (format " %d replace " anzu--cached-count))
             ((eq anzu--state 'replace)
              (format " %d/%d " (1+ here) total))
             (anzu--overflow-p
              (format " %s+ " total))
             (t
              (format " %s/%d " here total))))
     'face (if (+modeline-active) 'doom-modeline-highlight))))

(defun +modeline--evil-substitute ()
  "Show number of matches for evil-ex substitutions and highlights in real time."
  (when (and (bound-and-true-p evil-local-mode)
             (or (assq 'evil-ex-substitute evil-ex-active-highlights-alist)
                 (assq 'evil-ex-global-match evil-ex-active-highlights-alist)
                 (assq 'evil-ex-buffer-match evil-ex-active-highlights-alist)))
    (propertize
     (let ((range (if evil-ex-range
                      (cons (car evil-ex-range) (cadr evil-ex-range))
                    (cons (line-beginning-position) (line-end-position))))
           (pattern (car-safe (evil-delimited-arguments evil-ex-argument 2))))
       (if pattern
           (format " %s matches " (how-many pattern (car range) (cdr range)))
         " - "))
     'face (if (+modeline-active) 'doom-modeline-highlight))))

(defun +modeline--multiple-cursors ()
  "Show the number of multiple cursors."
  (when (bound-and-true-p evil-mc-cursor-list)
    (let ((count (length evil-mc-cursor-list)))
      (when (> count 0)
        (let ((face (cond ((not (+modeline-active)) 'mode-line-inactive)
                          (evil-mc-frozen 'doom-modeline-highlight)
                          ('doom-modeline-alternate-highlight))))
          (concat (propertize " " 'face face)
                  (all-the-icons-faicon "i-cursor" :face face :v-adjust -0.0575)
                  (propertize " " 'face `(:inherit (variable-pitch ,face)))
                  (propertize (format "%d " count)
                              'face face)))))))

(defun +modeline--overlay< (a b)
  "Sort overlay A and B."
  (< (overlay-start a) (overlay-start b)))

(defun +modeline--iedit ()
  "Show the number of iedit regions matches + what match you're on."
  (when (and (bound-and-true-p iedit-mode)
             (bound-and-true-p iedit-occurrences-overlays))
    (propertize
     (let ((this-oc (or (let ((inhibit-message t))
                          (iedit-find-current-occurrence-overlay))
                        (save-excursion
                          (iedit-prev-occurrence)
                          (iedit-find-current-occurrence-overlay))))
           (length (length iedit-occurrences-overlays)))
       (format " %s/%d "
               (if this-oc
                   (- length
                      (length (memq this-oc (sort (append iedit-occurrences-overlays nil)
                                                  #'+modeline--overlay<)))
                      -1)
                 "-")
               length))
     'face (if (+modeline-active) 'doom-modeline-highlight))))

(defun +modeline--macro-recording ()
  "Display current Emacs or evil macro being recorded."
  (when (and (+modeline-active)
             (or defining-kbd-macro
                 executing-kbd-macro))
    (let ((sep (propertize " " 'face 'doom-modeline-highlight)))
      (concat sep
              (propertize (if (bound-and-true-p evil-this-macro)
                              (char-to-string evil-this-macro)
                            "Macro")
                          'face 'doom-modeline-highlight)
              sep
              (all-the-icons-octicon "triangle-right"
                                     :face 'doom-modeline-highlight
                                     :v-adjust -0.05)
              sep))))

(def-modeline-var! +modeline-matches
  '(:eval
    (let ((meta (concat (+modeline--macro-recording)
                        (+modeline--anzu)
                        (+modeline--evil-substitute)
                        (+modeline--iedit)
                        (+modeline--multiple-cursors))))
      (or (and (not (equal meta "")) meta)
          " %I "))))


;;; `+modeline-modes'
(def-modeline-var! +modeline-modes ; remove minor modes
  '(""
    (:propertize mode-name
     face bold
     mouse-face doom-modeline-highlight)
    mode-line-process
    "%n"
    " "))

(defun doom-project-root (&optional dir)
  "Return the project root of DIR (defaults to `default-directory').
Returns nil if not in a project."
  (if (featurep 'projectile)
      (let ((projectile-project-root (unless dir projectile-project-root))
	    projectile-require-project-root)
	(projectile-project-root dir))
    default-directory))

;;; `+modeline-buffer-identification'
(defvar-local +modeline--buffer-id-cache nil)

;; REVIEW Generating the buffer's file name can be relatively expensive.
;;        Compounded with how often the modeline updates this can add up, so
;;        we cache it ahead of time.
(defun +modeline--generate-buffer-id-cache-h ()
  (when after-init-time
    (setq +modeline--buffer-id-cache
	  (let ((file-name (buffer-file-name (buffer-base-buffer))))
	    (unless (or (null default-directory)
			(null file-name)
			(file-remote-p file-name))
	      (when-let (project-root (doom-project-root))
		(file-relative-name (or buffer-file-truename (file-truename file-name))
				    (concat project-root ".."))))))))
(add-hook 'change-major-mode-after-body-hook #'+modeline--generate-buffer-id-cache-h) 
(add-hook 'after-save-hook #'+modeline--generate-buffer-id-cache-h) 
(add-hook 'focus-in-hook #'+modeline--generate-buffer-id-cache-h) 
(add-hook 'projectile-after-switch-project-hook #'+modeline--generate-buffer-id-cache-h) 
(add-hook 'after-set-visited-file-name-hook #'+modeline--generate-buffer-id-cache-h) 
(add-hook 'after-revert-hook #'+modeline--generate-buffer-id-cache-h) 

(def-modeline-var! +modeline-buffer-identification ; slightly more informative buffer id
  '((:eval
     (propertize
      (or +modeline--buffer-id-cache "%b")
      'face (cond ((buffer-modified-p) '(error bold mode-line-buffer-id))
                  ((+modeline-active)  'mode-line-buffer-id))
      'help-echo (or +modeline--buffer-id-cache (buffer-name))))
    (buffer-read-only (:propertize " RO" face warning))))


;;; `+modeline-position'
(def-modeline-var! +modeline-position '("  %l:%C %p  "))


;;; `+modeline-checker'
(def-modeline-var! +modeline-checker nil
  "Displays color-coded error status & icon for the current buffer."
  :local t)

(defun +modeline-checker-update (&optional status)
  "Update flycheck text via STATUS."
  (setq +modeline-checker
	(pcase status
	  (`finished
	   (if flycheck-current-errors
	       (let-alist (flycheck-count-errors flycheck-current-errors)
		 (let ((error (or .error 0))
		       (warning (or .warning 0))
		       (info (or .info 0)))
		   (+modeline-format-icon "do_not_disturb_alt"
					  (number-to-string (+ error warning info))
					  (cond ((> error 0)   'error)
						((> warning 0) 'warning)
						('success))
					  (format "Errors: %d, Warnings: %d, Debug: %d"
						  error
						  warning
						  info))))
	     (+modeline-format-icon "check" "" 'success)))
	  (`running     (+modeline-format-icon "access_time" "*" 'mode-line-inactive "Running..."))
	  (`errored     (+modeline-format-icon "sim_card_alert" "!" 'error "Errored!"))
	  (`interrupted (+modeline-format-icon "pause" "!" 'mode-line-inactive "Interrupted"))
	  (`suspicious  (+modeline-format-icon "priority_high" "!" 'error "Suspicious")))))
(add-hook 'flycheck-status-changed-functions #'+modeline-checker-update)
(add-hook 'flycheck-mode-hook #'+modeline-checker-update)


;;; `+modeline-selection-info'
(defsubst +modeline--column (pos)
  "Get the column of the position `POS'."
  (save-excursion (goto-char pos)
                  (current-column)))

(def-modeline-var! +modeline-selection-info
  '(:eval
    (when (or (and (bound-and-true-p evil-local-mode)
                   (eq evil-state 'visual))
              mark-active)
      (cl-destructuring-bind (beg . end)
          (if (bound-and-true-p evil-visual-selection)
              (cons evil-visual-beginning evil-visual-end)
            (cons (region-beginning) (region-end)))
        (propertize
         (let ((lines (count-lines beg (min end (point-max)))))
           (concat " "
                   (cond ((or (bound-and-true-p rectangle-mark-mode)
                              (and (bound-and-true-p evil-visual-selection)
                                   (eq 'block evil-visual-selection)))
                          (let ((cols (abs (- (+modeline--column end)
                                              (+modeline--column beg)))))
                            (format "%dx%dB" lines cols)))
                         ((and (bound-and-true-p evil-visual-selection)
                               (eq evil-visual-selection 'line))
                          (format "%dL" lines))
                         ((> lines 1)
                          (format "%dC %dL" (- end beg) lines))
                         ((format "%dC" (- end beg))))
                   (when (derived-mode-p 'text-mode)
                     (format " %dW" (count-words beg end)))
                   " "))
         'face (if (+modeline-active) 'success)))))
  "Information about the current selection, such as how many characters and
lines are selected, or the NxM dimensions of a block selection.")

(defmacro delq! (elt list &optional fetcher)
  "`delq' ELT from LIST in-place.
If FETCHER is a function, ELT is used as the key in LIST (an alist)."
  `(setq ,list
         (delq ,(if fetcher
                    `(funcall ,fetcher ,elt ,list)
                  elt)
               ,list)))

(defun +modeline-add-selection-segment-h ()
  (add-to-list '+modeline-format-left '+modeline-selection-info 'append))
(defun +modeline-remove-selection-segment-h ()
  (delq! '+modeline-selection-info +modeline-format-left))

(if (featurep 'evil)
    (progn
      (add-hook 'evil-visual-state-entry-hook #'+modeline-add-selection-segment-h)
      (add-hook 'evil-visual-state-exit-hook #'+modeline-remove-selection-segment-h))
  (add-hook 'activate-mark-hook #'+modeline-add-selection-segment-h)
  (add-hook 'deactivate-mark-hook #'+modeline-remove-selection-segment-h))


;;; `+modeline-encoding'
(defconst IS-WINDOWS (memq system-type '(cygwin windows-nt ms-dos)))
(def-modeline-var! +modeline-encoding
  `(:eval
    (let ((sys (coding-system-plist buffer-file-coding-system))
          (eol (coding-system-eol-type-mnemonic buffer-file-coding-system)))
      (concat (unless (equal eol ,(if IS-WINDOWS "CRLF" "LF"))
                (concat "  " eol " "))
              (if (memq (plist-get sys :category)
                        '(coding-category-undecided coding-category-utf-8))
                  (unless (string-match-p "utf-8" (symbol-name buffer-file-coding-system))
                    "UTF-8  ")
                (concat (upcase (symbol-name (plist-get sys :name)))
                        "  "))))))

;; Clearer mnemonic labels for EOL styles
(setq eol-mnemonic-dos "CRLF"
      eol-mnemonic-mac "CR"
      eol-mnemonic-unix "LF"
      eol-mnemonic-undecided "??")


;;
;;; Default modeline

(def-modeline! :main
  '(""
    +modeline-matches
    " "
    +modeline-buffer-identification
    +modeline-position)
  `(""
    mode-line-misc-info
    +modeline-modes
    (vc-mode ("  "
              ,(all-the-icons-octicon "git-branch" :v-adjust 0.0)
              vc-mode " "))
    "  "
    +modeline-encoding
    (+modeline-checker ("" +modeline-checker "   "))))

(def-modeline! 'project
  `(" "
    ,(all-the-icons-octicon
      "file-directory"
      :face 'bold
      :v-adjust -0.06
      :height 1.1)
    (:propertize (" " (:eval (abbreviate-file-name default-directory)))
                 face bold))
  '("" mode-line-misc-info +modeline-modes))

(def-modeline! 'special
  '("" +modeline-matches
    " " +modeline-buffer-identification)
  '("" +modeline-modes))


;; Other modes
(set-modeline! :main 'default)
(set-modeline-hook! '(special-mode-hook
                      image-mode-hook
                      circe-mode-hook)
                    'special)

(defun +modeline-init-project-or-hide-h ()
  (if (eq major-mode 'magit-status-mode)
      (set-modeline! 'project)
    (hide-mode-line-mode +1)))
(add-hook 'magit-mode-hook #'+modeline-init-project-or-hide-h)


;;
;;; Bootstrap

(defvar +modeline--old-format (default-value 'mode-line-format))

(define-minor-mode +modeline-mode
  "TODO"
  :init-value nil
  :global nil
  (cond
   (+modeline-mode
    (setq mode-line-format
          (cons
           "" '(+modeline-bar
                +modeline-format-left
                (:eval
                 (propertize
                  " "
                  'display
                  `((space :align-to (- (+ right right-fringe right-margin)
                                        ,(string-width
                                          (format-mode-line '("" +modeline-format-right))))))))
                +modeline-format-right))))
   ((setq mode-line-format +modeline--old-format))))

(define-global-minor-mode +modeline-global-mode +modeline-mode +modeline-mode)
