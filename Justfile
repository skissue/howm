emacs := "emacs"

el_files := "howm.el howm-menu.el howm-reminder.el howm-date.el howm-misc.el \
    howm-mode.el howm-view.el howm-backend.el howm-common.el howm-vars.el \
    howm-lang-en.el howm-lang-fr.el howm-lang-ja.el \
    howm-menu-en.el howm-menu-fr.el howm-menu-ja.el \
    honest-report.el action-lock.el riffle.el gfunc.el illusion.el \
    iigrep.el cheat-font-lock.el"

# Run the interactive test suite
test:
    HOWM_TEST=t {{ emacs }} -q --no-site-file -l sample/dot.emacs

# Run ERT unit tests in batch mode
test-ert:
    {{ emacs }} -Q --batch -L . -l test/howm-search-test.el -f ert-run-tests-batch-and-exit

# Byte-compile all .el files to .elc
byte-compile:
    {{ emacs }} --batch -L . \
        --eval '(dolist (f (split-string "{{ el_files }}")) (byte-compile-file f))'

# Regenerate howm-menu-{en,fr,ja}.el from their text templates
mkmenu:
    {{ emacs }} -q --no-site-file --batch -l howm-mkmenu.el

# Remove all byte-compiled .elc files
clean:
    rm -f *.elc
