.POSIX:
PROG		= fountain-mode
LISP_FILE	= ${PROG}.el
DEPS		= seq package-lint
NEWS_FILE	= NEWS
DOCS_DIR	= doc
TEXI_FILE	= ${DOCS_DIR}/${PROG}.texi
INFO_FILE	= ${DOCS_DIR}/${PROG}.info
CSS_FILE	= ${DOCS_DIR}/style.css
HTML_DIR	= ${DOCS_DIR}/html
VERS		= ${shell grep -oE -m1 'Version:[ 0-9.]+' ${LISP_FILE} | tr -d :}
TAG		= ${shell echo ${VERS} | sed -E 's/Version:? ([0-9.]+)/v\1/'}
INIT		= (progn (require (quote package)) \
  (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
  (package-initialize) \
  (mapc (lambda (pkg) \
          (unless (package-installed-p pkg) \
            (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
            (package-install pkg))) \
        (quote (${DEPS}))))

help:
	@echo check
	@echo compile
	@echo info-manual
	@echo html-manual
	@echo pdf-manual
	@echo manuals: info-manual html-manual pdf-manual
	@echo all: clean check compile manuals
	@echo tag-release: check compile
	@echo clean

all: clean check compile info-manual html-manual pdf-manual

check:
	emacs -Q --eval '${INIT}' --batch -f package-lint-batch-and-exit ${LISP_FILE}

compile:
	emacs -Q --eval '${INIT}' -L . --batch -f batch-byte-compile ${LISP_FILE}

manuals: info-manual html-manual pdf-manual

info-manual:
	makeinfo --output ${INFO_FILE} ${TEXI_FILE}
	install-info ${INFO_FILE} dir

html-manual:
	makeinfo --html --css-include=${CSS_FILE} --output ${HTML_DIR} ${TEXI_FILE}

pdf-manual:
	texi2pdf --clean ${TEXI_FILE}

tag-release: check compile
	sed -i~ '1 s/.*/* ${VERS}/' ${NEWS_FILE}
	git commit -m 'Add ${VERS} to ${NEWS_FILE}' ${NEWS_FILE}
	awk '/^* Version/ { v ++1 } v == 1' ${NEWS_FILE} | sed 's/^* //' | git tag -F - ${TAG}

clean:
	rm -f ${PROG}.elc
	rm -f ${INFO_FILE}
	rm -f dir
	rm -f ${DOCS_DIR}/${PROG}.{aux,fn,log,toc,vr,pdf}
	rm -rf ${HTML_DIR}/*
