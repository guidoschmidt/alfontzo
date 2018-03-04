;;; afontzo.el --- Manage font sizes across machines -*- lexical-binding: t -*-

;; Copyright (C) 2017 Guido Schmidt

;; Author: Guido Schmidt
;; Maintainer: Guido Schmidt <git@guidoschmidt.cc>
;; URL: https://github.com/guidoschmidt/alfontzo
;; Version: 0.0.1
;; Keywords: faces, themes
;; Package-Requires: ((emacs "24.4"))

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Possible names:
;; - Typocalypse Now
;; - Alfontzo
;; - Fontanella
;; - Al Fonto
;; - Rollercoaster Typecoon
;; - Mike Typeson
;; - Typelord
;; - The Fontfather
;; - Typerider
;; - Michael Type
;; - Alfontzo


;;; Code:
(require 'cl-lib)

(defconst alfontzo-os-windows "windows-nt")
(defconst alfontzo-os-mac "darwin")
(defconst alfontzo-os-linux "linux")

(defcustom alfontzo-os-font-size-map
  (list `((,alfontzo-os-windows . 13)
          (,alfontzo-os-mac . 14)
          (,alfontzo-os-linux . 11)))
  "Associative list that stores OS specific font sizes."
  :type 'alist
  :group 'alfontzo)

(defcustom alfontzo-os-font-name-map
  (list `((,alfontzo-os-windows . "Consolas")
          (,alfontzo-os-mac . "Menlo")
          (,alfontzo-os-linux . "Courier")))
  "Associative list that stores OS specific font names."
  :type 'alist
  :group 'alfontzo)

;; (defconst host-emma "Emma.local")
;; (defconst host-roxy "Roxy.local")

(defcustom alfontzo-host-font-scales-map '()
  "Associative list of typographic scalars to use at different machines."
  :type 'alist
  :group 'alfontzo)

;; (add-to-list 'alfontzo-host-font-scales-map
;;              `(,host-roxy . 0.875))
;; (add-to-list 'alfontzo-host-font-scales-map
;;              `(,host-emma . 1.125))

(defcustom alfontzo-host-font-name-map '()
  "Associative list of font names to use at different machines."
  :type 'alist
  :group 'alfontzo)

;; (add-to-list 'alfontzo-host-font-name-map
;;              `(,host-roxy . "PragmataPro"))
;; (add-to-list 'alfontzo-host-font-name-map
;;              `(,host-emma . "SourceCodePro"))


(defun alfontzo-set-font (font size)
  "Set Emacs font via `set-frame-font' with a FONT name and SIZE."
  (let ((font-string (concat font "-" (int-to-string size))))
    (add-to-list 'initial-frame-alist `(font . ,font-string))
    (add-to-list 'default-frame-alist `(font . ,font-string))
    (if (member font (font-family-list))
        (progn
          (set-frame-font font-string nil t))
      (message (concat font " not available")))
    (message (concat "Emacs typeface: "
                     font
                     " @ size "
                     (number-to-string size)))))

(defun alfontzo-resize (size scalar)
  "Scale a typeface SIZE with a SCALAR."
  (round (* size scalar)))

(defun alfontzo-match-os (list)
  "Return the first match in LIST for the current OS."
  (cl-first
    (cl-remove-if-not
     (lambda (entry)
       (let ((os (car entry)))
         (string-equal system-type os)))
     list)))

(defun alfontzo-match-host (list)
  "Return the first match in LIST for the current host."
  (cl-first
   (cl-remove-if-not
    (lambda (entry)
      (let ((host (car entry)))
        (string-equal (system-name) host)))
    list)))

(defun alfontzo-scale-for-os ()
  "Determine the typographic scale for the current OS."
  (cdr (alfontzo-match-os alfontzo-os-font-size-map)))

(defun alfontzo-font-for-os ()
  "Determine the font name for the current OS."
  (cdr (alfontzo-match-os alfontzo-os-font-name-map)))

(defun alfontzo-scale-for-host ()
  "Determine the typographic scale for the current machine."
  (cdr (alfontzo-match-host alfontzo-host-font-scales-map)))

(defun alfontzo-font-for-host ()
  "Determine the font name for the current machine."
  (cdr (alfontzo-match-host alfontzo-host-font-name-map)))


;; TODO:
;; (defun typescale (scale)
;;   "Adjust the typescale of the current system."
;;   (interactive)
;;   ;; TODO: add interactive prompt for parameters
;;   ;; TODO: add assoc list update
;;   (let ((host-scalar (cdr (scale-from-host)))
;;         (os-typescale (cdr (font-for-os)))
;;         (host (car (scale-from-host))))
;;     (let ((scalar (/ scale (float os-typescale))))
;;       (add-to-list 'host-type-scales `(,host . ,scalar))
;;       (set-font font-typeface (scale-with os-typescale scalar)))))

(defun alfontzo-typescale (scale)
  (interactive))

(provide 'alfontzo)
;;; alfontzo.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:update-interval 2))
