.PHONY: test

test:
	@emacs --batch \
				 --no-init-file \
				 --no-splash \
				 --load ./teatime-emacs/teatime-tests.el \
				 --funcall ert-run-tests-batch-and-exit
