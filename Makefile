PROGRAM		:= fountain-mode
DEPS		:= package-lint seq
EMACS		?= emacs
CSS_FILE	?= stylesheet.css
DOCS_DIR	?= docs
LISP_FILE	:= $(PROGRAM).el
TEXI_FILE	:= $(DOCS_DIR)/$(PROGRAM).texi
INFO_FILE	:= $(DOCS_DIR)/$(PROGRAM).info

INIT = '(progn \
  (require (quote package)) \
  (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
  (package-initialize) \
  (mapc (lambda (pkg) \
          (unless (package-installed-p pkg) \
            (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
            (package-install pkg))) \
        (quote ($(DEPS)))))'

all: clean check compile

check: $(LISP_FILE)
	$(EMACS) -Q --eval $(INIT) --batch -f package-lint-batch-and-exit $(LISP_FILE)

compile: $(LISP_FILE)
	$(EMACS) -Q --eval $(INIT) -L . --batch -f batch-byte-compile $(LISP_FILE)

info-manual: $(TEXI_FILE)
	makeinfo $(TEXI_FILE) && \
	install-info $(INFO_FILE) dir

html-manual: $(TEXI_FILE)
	makeinfo --html --css-ref=$(CSS_FILE) --output $(DOCS_DIR) $(TEXI_FILE)

pdf-manual: $(TEXI_FILE)
	pdftex $(TEXI_FILE)

clean:
	rm -f $(PROGRAM).elc
	rm -f dir
	rm -f $(DOCS_DIR)/*.html
	rm -f **$(PROGRAM).aux
	rm -f **$(PROGRAM).fn
	rm -f **$(PROGRAM).log
	rm -f **$(PROGRAM).toc
	rm -f **$(PROGRAM).vr
	rm -f **$(PROGRAM).pdf

.PHONY:	all check compile
