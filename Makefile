PATH		:= /usr/local/opt/texinfo/bin:${PATH}
NAME		:= fountain-mode
DEPS		:= package-lint seq
EMACS		?= emacs
CSS_FILE	?= stylesheet.css
DOCS_DIR	?= docs
LISP_FILE	:= $(NAME).el
TEXI_FILE	:= $(DOCS_DIR)/$(NAME).texi
INFO_FILE	:= $(DOCS_DIR)/$(NAME).info

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

all: clean check docs compile

check: $(LISP_FILE)
	$(EMACS) -Q --eval $(INIT) --batch -f package-lint-batch-and-exit $(LISP_FILE)

compile: $(LISP_FILE)
	$(EMACS) -Q --eval $(INIT) -L . --batch -f batch-byte-compile $(LISP_FILE)

info-manual: $(TEXI_FILE)
	PATH=$(PATH) \
	makeinfo $(TEXI_FILE) && \
	install-info $(INFO_FILE) dir

html-manual: $(TEXI_FILE)
	PATH=$(PATH) \
	makeinfo --html --css-ref=$(CSS_FILE) --output $(DOCS_DIR) $(TEXI_FILE)

pdf-manual: $(TEXI_FILE)
	pdftex $(TEXI_FILE)

clean:
	rm -f $(NAME).elc
	rm -f $(INFO_FILE)
	rm -f dir
	rm -f $(DOCS_DIR)/*.html
	rm -f **$(NAME).aux
	rm -f **$(NAME).fn
	rm -f **$(NAME).log
	rm -f **$(NAME).toc
	rm -f **$(NAME).vr
	rm -f **$(NAME).pdf

.PHONY:	all check compile
