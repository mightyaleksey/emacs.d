(defun test-dir (dirname)
  (expand-file-name
    dirname
    (file-name-directory (or (buffer-file-name)
                           default-directory))))


(ert-deftest dt-dirname ()
  (should (equal (dt-dirname "foo/bar") "foo/"))
  (should (equal (dt-dirname "foo/bar/") "foo/"))
  (should (equal (dt-dirname "foo/bar//") "foo/")))

(ert-deftest dt-project-root-p ()
  (should (equal
            (dt-project-root-p
              (test-dir "fixture/common-pj1"))
            t)))

(ert-deftest dt-project-dir ()
  (should (equal
            (dt-project-dir
              (test-dir "fixture/common-pj1/foo"))
            (test-dir "fixture/common-pj1/"))))
