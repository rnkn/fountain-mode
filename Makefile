.POSIX:
PROGRAM		= fountain-mode
LISP_FILE	= $(PROGRAM).el
DEPS		= seq package-lint
NEWS_FILE	= NEWS
DOCS_DIR	= docs
TEXI_FILE	= $(DOCS_DIR)/$(PROGRAM).texi
INFO_FILE	= $(DOCS_DIR)/$(PROGRAM).info
CSS_FILE	= stylesheet.css
HTML_DIR	= $(DOCS_DIR)/html
VERS		= $(shell grep -oE -m1 'Version:[ 0-9.]+' $(LISP_FILE) | tr -d :)
TAG			= $(shell echo $(VERS) | sed -E 's/Version:? ([0-9.]+)/v\1/')
INIT		= '(progn \
  (require (quote package)) \
  (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
  (package-initialize) \
  (mapc (lambda (pkg) \
          (unless (package-installed-p pkg) \
            (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
            (package-install pkg))) \
        (quote ($(DEPS)))))'

all: clean check compile info-manual html-manual

check: $(LISP_FILE)
	emacs -Q --eval $(INIT) --batch -f package-lint-batch-and-exit $(LISP_FILE)

compile: $(LISP_FILE)
	emacs -Q --eval $(INIT) -L . --batch -f batch-byte-compile $(LISP_FILE)

info-manual: $(TEXI_FILE)
	makeinfo $(TEXI_FILE) --output $(INFO_FILE)
	install-info $(INFO_FILE) dir

html-manual: $(TEXI_FILE)
	makeinfo --html --css-ref=$(CSS_FILE) --output $(HTML_DIR) $(TEXI_FILE)
	cp $(DOCS_DIR)/$(CSS_FILE) $(HTML_DIR)/$(CSS_FILE)

pdf-manual: $(TEXI_FILE)
	pdftex $(TEXI_FILE)

tag-release: check compile
	sed -i~ '1 s/.*/* $(VERS)/' $(NEWS_FILE)
	git commit -m 'Update $(NEWS_FILE)' $(NEWS_FILE)
	awk '/^* Version/ {v ++ 1} v == 1' $(NEWS_FILE) | sed 's/^* //' | git tag -sF - $(TAG)

clean:
	rm -f $(PROGRAM).elc
	rm -f $(INFO_FILE)
	rm -f dir
	rm -f **$(PROGRAM).aux
	rm -f **$(PROGRAM).fn
	rm -f **$(PROGRAM).log
	rm -f **$(PROGRAM).toc
	rm -f **$(PROGRAM).vr
	rm -f **$(PROGRAM).pdf
	rm -rf $(DOCS_DIR)/html
