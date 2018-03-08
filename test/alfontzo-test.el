;;; alfontzo-test.el --- Testing alfontzo
;;; Commentary:

;;; Code:
(require 'cl-lib)
(require 'el-mock)

(load (expand-file-name "alfontzo.el" default-directory))

(ert-deftest test-alfontzo-set-font ()
  "Test setting Emacs font face and size via alfontzo."
  (let ((font-name "Arial")
        (font-size 20))
    (alfontzo-set-font font-name font-size)
    (let ((font-should (concat font-name "-" (number-to-string font-size)))
          (font-is-initial-frame-alist (cdr (assoc 'font initial-frame-alist)))
          (font-is-default-frame-alist (cdr (assoc 'font default-frame-alist))))
      (should (equal font-should font-is-default-frame-alist))
      (should (equal font-should font-is-initial-frame-alist)))))


(ert-deftest test-alfontzo-match-os ()
  (let ((os-list `(("linux" . "Penguin")
                   ("darwin" . "Apple")
                   ("windows-nt" . "Window"))))
    ;; Positive tests
    (let ((system-type "linux"))
      (should (equal "Penguin" (cdr (alfontzo-match-os os-list)))))
    (let ((system-type "windows-nt"))
      (should (equal "Window" (cdr (alfontzo-match-os os-list)))))
    (let ((system-type "darwin"))
      (should (equal "Apple" (cdr (alfontzo-match-os os-list)))))
    ;; Unknown system-type
    (let ((system-type "not-a-known-system"))
      (should (equal nil (cdr (alfontzo-match-os os-list)))))
    ;; Negative tests
    (let ((system-type "linux"))
      (should (not (equal "Apple" (cdr (alfontzo-match-os os-list))))))
    (let ((system-type "darwin"))
      (should (not (equal "Window" (cdr (alfontzo-match-os os-list))))))
    (let ((system-type "windows-nt"))
      (should (not (equal "Penguin" (cdr (alfontzo-match-os os-list))))))))

(ert-deftest test-alfontzo-match-host ()
  (let ((host-list `(("macbook" . 0)
                     ("desktop" . 1)
                     ("thinkpad" . 2))))
    ;; Positive tests
    (cl-letf ((system-name "macbook"))
      (should (equal 0 (cdr (alfontzo-match-host host-list)))))
    (cl-letf ((system-name "desktop"))
      (should (equal 1 (cdr (alfontzo-match-host host-list)))))
    (cl-letf ((system-name "thinkpad"))
      (should (equal 2 (cdr (alfontzo-match-host host-list)))))
    ;; Unknown host
    (cl-letf ((system-name "host-not-known"))
      (should (equal nil (cdr (alfontzo-match-host host-list)))))
    ;; Negative tests
    (cl-letf ((system-name "macbook"))
      (should (not (equal 1 (cdr (alfontzo-match-host host-list))))))
    (cl-letf ((system-name "desktop"))
      (should (not (equal 0 (cdr (alfontzo-match-host host-list))))))
    (cl-letf ((system-name "thinkpad"))
      (should (not (equal 0 (cdr (alfontzo-match-host host-list))))))))

(ert-deftest test-alfontzo-font-for-os ()
  (setq alfontzo-os-font-name-map
        `((,alfontzo-os-mac . "Fira Code")
          (,alfontzo-os-linux . "PragmataPro")
          (,alfontzo-os-windows . "Hasklig")))
  (let ((system-type "linux"))
    (should (equal "PragmataPro" (alfontzo-font-for-os))))
  (let ((system-type "darwin"))
    (should (equal "Fira Code" (alfontzo-font-for-os))))
  (let ((system-type "windows-nt"))
    (should (equal "Hasklig" (alfontzo-font-for-os)))))

(ert-deftest test-alfontzo-scale-for-os ()
  (setq alfontzo-os-font-size-map 
        `((,alfontzo-os-mac . 10)
          (,alfontzo-os-windows . 20)
          (,alfontzo-os-linux . 13)))
  (let ((system-type "linux"))
    (should (equal 13 (alfontzo-scale-for-os))))
  (let ((system-type "darwin"))
    (should (equal 10 (alfontzo-scale-for-os))))
  (let ((system-type "windows-nt"))
    (should (equal 20 (alfontzo-scale-for-os)))))

(ert-deftest alfontzo-font-for-host ()
  (setq alfontzo-host-font-name-map
        '(("macbook" . "Hasklig")
          ("desktop" . "SourceCodePro")
          ("lisp-machine" . "Fira Code")))
  (cl-letf ((system-name "macbook"))
    (should (equal "Hasklig" (alfontzo-font-for-host))))
  (cl-letf ((system-name "desktop"))
    (should (equal "SourceCodePro" (alfontzo-font-for-host))))
  (cl-letf ((system-name "lisp-machine"))
    (should (equal "Fira Code" (alfontzo-font-for-host)))))

(ert-deftest alfontzo-scale-for-host ()
  (setq alfontzo-host-font-scales-map
        '(("macbook" . 17)
          ("lisp-machine" . 42)
          ("foobar" . 9)))
  (cl-letf ((system-name "macbook"))
    (should (equal 17 (alfontzo-scale-for-host))))
  (cl-letf ((system-name "lisp-machine"))
    (should (equal 42 (alfontzo-scale-for-host))))
  (cl-letf ((system-name "foobar"))
    (should (equal 9 (alfontzo-scale-for-host)))))

(provide 'alfontzo-test)
;;; alfontzo-test.el ends here

