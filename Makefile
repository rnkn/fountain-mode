EMACS ?= emacs

REQUIREMENTS = package-lint seq

INIT='(progn \
  (require (quote package)) \
  (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
  (package-initialize) \
  (mapc (lambda (pkg) \
          (unless (package-installed-p pkg) \
            (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
            (package-install pkg))) \
        (quote (${REQUIREMENTS}))))'

all: clean check compile

check:
	$(EMACS) -Q --eval $(INIT) --batch -f package-lint-batch-and-exit *.el

compile: clean
	$(EMACS) -Q --eval $(INIT) -L . --batch -f batch-byte-compile *.el

clean:
	rm -f *.elc

.PHONY:	all check compile
