#!/bin/sh

_dirname=$(dirname "$0")

# run tests from the test folder,
# so the emacs default-directory will point into it.
cd "$_dirname"

for file in $(ls); do
  if [[ "$file" =~ el$ ]]; then
    src="../core/$(echo "$file" | sed "s/\.test//")"

    emacs -batch \
          -l ert \
          -l "$src" \
          -l "$file" \
          -f ert-run-tests-batch-and-exit
  fi
done
