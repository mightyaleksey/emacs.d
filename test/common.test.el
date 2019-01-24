(defun test-dir (dirname)
  (expand-file-name
    dirname
    default-directory))


(ert-deftest dt-current-dir ()
  (should (equal (dt-current-dir) default-directory)))

(ert-deftest dt-parent-dir ()
  (should (equal (dt-parent-dir "foo/bar") "foo/"))
  (should (equal (dt-parent-dir "foo/bar/") "foo/")))

(ert-deftest dt-project-dir-p ()
  (should (equal
            (dt-project-dir-p
              (test-dir "fixture/common-pj1"))
            t)))

(ert-deftest dt-find-project-dir ()
  (should (equal
            (dt-find-project-dir
              (test-dir "fixture/common-pj1/foo"))
            (test-dir "fixture/common-pj1/"))))

(ert-deftest dt-project-dir ()
  (should (equal
            (dt-project-dir)
            (dt-parent-dir default-directory))))
