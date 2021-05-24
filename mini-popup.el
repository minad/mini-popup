;;; mini-popup.el --- Show the minibuffer in a popup -*- lexical-binding: t -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Daniel Mendler <mail@daniel-mendler.de>
;; Maintainer: Daniel Mendler <mail@daniel-mendler.de>
;; Created: 2021
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/minad/mini-popup

;; This file is part of GNU Emacs.

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

;; Mini popup

;;; Code:

(defgroup mini-popup nil
  "Mini popup."
  :group 'convenience
  :prefix "mini-popup-")

(defface mini-popup-background
  '((((class color) (min-colors 88) (background dark))
     :background "#191a1b")
    (((class color) (min-colors 88) (background light))
     :background "#f0f0f0")
    (t :background "gray"))
  "Face used to for the popup background.")

(defface mini-popup-border
  '((((class color) (min-colors 88) (background dark)) :background "#323232")
    (((class color) (min-colors 88) (background light)) :background "#d7d7d7")
    (t :background "gray"))
  "The background color used for the thin border.")

(defvar mini-popup--height-function nil
  "Height function.")

(defvar mini-popup--frame-parameters
  '((no-accept-focus . t)
    (min-width . t)
    (min-height . t)
    (top . 0.25)
    (left . 0.5)
    (width . 0.8)
    (height . 0.25)
    (border-width . 0)
    (child-frame-border-width . 1)
    (left-fringe . 20)
    (right-fringe . 20)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (cursor-type . t)
    (minibuffer . nil)
    (visibility . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t)))

(defvar mini-popup--frame nil)
(defvar-local mini-popup--overlay nil)

;;;###autoload
(define-minor-mode mini-popup-mode
  "Mini popup."
  :global t
  (if mini-popup-mode
      (progn
        (add-hook 'minibuffer-setup-hook #'mini-popup--setup)
        (add-hook 'minibuffer-exit-hook #'mini-popup--exit))
    (remove-hook 'minibuffer-setup-hook #'mini-popup--setup)
    (remove-hook 'minibuffer-exit-hook #'mini-popup--exit)
    (when mini-popup--frame
      (delete-frame mini-popup--frame)
      (setq mini-popup--frame nil))))

(defun mini-popup--setup ()
  "Minibuffer setup hook."
  (when mini-popup--overlay
    (delete-overlay mini-popup--overlay))
  (when mini-popup-mode
    (mini-popup--setup-buffer)
    (mini-popup--setup-frame)
    (mini-popup--setup-overlay)))

(defun mini-popup--exit ()
  "Minibuffer exit hook."
  ;; Execute after leaving the minibuffer
  (run-at-time 0 nil #'mini-popup--hide))

(defun mini-popup--hide ()
  (when mini-popup-mode
    (let ((win (active-minibuffer-window)))
      (if win
          (with-selected-window win (mini-popup--setup))
        (make-frame-invisible mini-popup--frame)))))

(defun mini-popup--resize ()
  "Resize according to `mini-popup--height-function'."
  (when mini-popup--height-function
    (let ((window-min-height 1)
          (window-min-width 1)
          (frame-resize-pixelwise t))
      (set-frame-height mini-popup--frame
                      (funcall mini-popup--height-function)
                      nil 'pixelwise))))

(defun mini-popup--setup-buffer ()
  "Setup minibuffer local variables."
  (setq-local mode-line-format nil
              resize-mini-windows nil
              header-line-format nil
              frame-title-format ""
              cursor-in-non-selected-windows 'box
              cursor-type '(bar . 0)
              show-trailing-whitespace nil
              display-line-numbers nil
              left-fringe-width nil
              right-fringe-width nil
              left-margin-width 0
              right-margin-width 0
              fringes-outside-margins 0)
  (add-hook 'post-command-hook #'mini-popup--setup 99 'local))

;; Function adapted from posframe.el by tumashu
(defun mini-popup--setup-frame ()
  "Show child frame."
  (let ((x-gtk-resize-child-frames
         (let ((case-fold-search t))
           (and
            ;; XXX Hack to fix resizing on gtk3/gnome taken from posframe.el
            ;; More information:
            ;; * https://github.com/minad/mini-popup/issues/17
            ;; * https://gitlab.gnome.org/GNOME/mutter/-/issues/840
            ;; * https://lists.gnu.org/archive/html/emacs-devel/2020-02/msg00001.html
            (string-match-p "gtk3" system-configuration-features)
            (string-match-p "gnome\\|cinnamon" (or (getenv "XDG_CURRENT_DESKTOP")
                                                   (getenv "DESKTOP_SESSION") ""))
            'resize-mode)))
        (after-make-frame-functions))
    (unless (and (frame-live-p mini-popup--frame)
                 (eq (frame-parent mini-popup--frame) (window-frame)))
      (when mini-popup--frame (delete-frame mini-popup--frame))
      (setq mini-popup--frame (make-frame
                               `((parent-frame . ,(window-frame))
                                 (line-spacing . ,line-spacing)
                                 ;; Set `internal-border-width' for Emacs 27
                                 (internal-border-width
                                  . ,(alist-get 'child-frame-border-width mini-popup--frame-parameters))
                                 ,@mini-popup--frame-parameters))))
    (set-face-background
     (if (facep 'child-frame-border) 'child-frame-border 'internal-border)
     (face-attribute 'mini-popup-border :background) mini-popup--frame)
    (set-face-background
     'fringe
     (face-attribute 'mini-popup-background :background) mini-popup--frame)
    (set-frame-parameter
     mini-popup--frame 'background-color
     (face-attribute 'mini-popup-background :background))
    (set-window-buffer (frame-root-window mini-popup--frame) (current-buffer))
    (mini-popup--resize)
    (make-frame-visible mini-popup--frame)))

(defun mini-popup--setup-overlay ()
  (let ((pt (point)))
    (with-selected-window (frame-root-window mini-popup--frame)
      (goto-char pt)))
  (setq mini-popup--overlay (make-overlay (point-max) (point-max) nil t t))
  (overlay-put mini-popup--overlay 'window (selected-window))
  (overlay-put mini-popup--overlay 'priority 1000)
  (overlay-put mini-popup--overlay 'after-string "\n\n")
  (set-window-vscroll nil 100)
  (window-resize nil (- (window-height))))

(provide 'mini-popup)
;;; mini-popup.el ends here
