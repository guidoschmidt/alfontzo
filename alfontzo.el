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
  `((,alfontzo-os-windows . 13)
    (,alfontzo-os-mac . 14)
    (,alfontzo-os-linux . 11))
  "Associative list that stores OS specific font sizes."
  :type 'alist
  :group 'alfontzo)

(defcustom alfontzo-os-font-name-map
  `((,alfontzo-os-windows . "Consolas")
    (,alfontzo-os-mac . "Menlo")
    (,alfontzo-os-linux . "Courier"))
  "Associative list that stores OS specific font names."
  :type 'alist
  :group 'alfontzo)

(defcustom alfontzo-host-font-scales-map
  '()
  "Associative list of typographic scalars to use at different machines."
  :type 'alist
  :group 'alfontzo)

(defcustom alfontzo-host-font-name-map
  '()
  "Associative list of font names to use at different machines."
  :type 'alist
  :group 'alfontzo)

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

(defun alfontzo-font-for-os ()
  "Determine the font name for the current OS."
  (cdr (alfontzo-match-os alfontzo-os-font-name-map)))

(defun alfontzo-scale-for-os ()
  "Determine the typographic scale for the current OS."
  (cdr (alfontzo-match-os alfontzo-os-font-size-map)))

(defun alfontzo-font-for-host ()
  "Determine the font name for the current machine."
  (cdr (alfontzo-match-host alfontzo-host-font-name-map)))

(defun alfontzo-scale-for-host ()
  "Determine the typographic scale for the current machine."
  (cdr (alfontzo-match-host alfontzo-host-font-scales-map)))

(defun alfontzo-set-font-for-host (font)
  "Set the FONT for the current host."
  (let ((old-value (assoc (system-name) alfontzo-host-font-name-map)))
    (if old-value
        (setf (cdr old-value) font)
      (add-to-list 'alfontzo-host-font-name-map
                   `(,(system-name) . ,font)))))

(defun alfontzo-set-scale-for-host (size)
  "Set the font SIZE for the current host."
  (let ((old-value (assoc (system-name) alfontzo-host-font-scales-map)))
    (if old-value
        (setf (cdr old-value) size)
      (add-to-list 'alfontzo-host-font-scales-map
                   `(,(system-name) . ,size)))))

(defun alfontzo-interlock-font-config ()
  "Adjust the typescale of the current system."
  (let ((os-font-name (alfontzo-font-for-os))
        (os-font-size (alfontzo-scale-for-os))
        (host-font-name (alfontzo-font-for-host))
        (host-font-size (alfontzo-scale-for-host)))
    (let ((font-name (if host-font-name
                         host-font-name
                       os-font-name))
          (font-size (if host-font-size
                         host-font-size
                       os-font-size)))
      (print (concat "Setting font: " font-name "-"
                     (number-to-string font-size)))
      (alfontzo-set-font font-name font-size))))

(defun alfontzo-typescale (scale)
  "Prompt for font SCALE for usage at the current machine."
  (interactive
   (list
    (read-from-minibuffer
     (concat "Fontsize for " (system-name) ": ")
     (if (alfontzo-scale-for-host)
         (number-to-string (alfontzo-scale-for-host))
       "")
     nil
     nil
     nil)))
  (let ((scale (string-to-number scale)))
    (alfontzo-set-scale-for-host scale))
  (alfontzo-interlock-font-config)
  (customize-save-variable 'alfontzo-host-font-scales-map
                           alfontzo-host-font-scales-map))

(defun alfontzo-fontface (font)
  "Prompt for FONT face for usage at the current machine."
  (interactive
   (list
    (read-from-minibuffer
     (concat "Font for " (system-name) ": ")
     (alfontzo-font-for-host)
     nil
     nil
     nil)))
  (alfontzo-set-font-for-host font)
  (alfontzo-interlock-font-config)
  (customize-save-variable 'alfontzo-host-font-name-map
                           alfontzo-host-font-name-map))


;;;###autoload
(defun alfontzo-init ()
  "Load font configurations from custom-el."
  (add-hook 'after-init-hook
            (lambda ()
              (load custom-file)
              (alfontzo-interlock-font-config))))

;;; TODO:
;; returns true if Symbola exists
;;(member "Symbola" (font-family-list))

(provide 'alfontzo)
;;; alfontzo.el ends here
