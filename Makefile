# n.b. This Makefile is for development convenience only. It is not
# required to build or install Fountain Mode.

.POSIX:
PROG		= fountain-mode
LISP_FILE	= ${PROG}.el
DEPS		= seq package-lint
NEWS_FILE	= NEWS.md
DOCS_DIR	= doc
TEXI_FILE	= ${DOCS_DIR}/${PROG}.texi
INFO_FILE	= ${DOCS_DIR}/${PROG}.info
CSS_FILE	= ${DOCS_DIR}/style.css
HTML_DIR	= ${DOCS_DIR}/html
VERS		= ${shell grep -oE -m1 'Version:[ 0-9.]+' ${LISP_FILE} | tr -d :}
TAG			= ${shell echo ${VERS} | sed -E 's/Version:? ([0-9.]+)/v\1/'}
INIT		= \
(progn (require (quote package)) \
       (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
       (package-initialize) \
       (dolist (pkg (quote (${DEPS}))) \
         (unless (package-installed-p pkg) \
           (package-refresh-contents) \
           (package-install pkg))))

all: clean check compile manuals

check:
	@emacs -Q --eval '${INIT}' --batch -f package-lint-batch-and-exit ${LISP_FILE}

compile:
	@emacs -Q --eval '${INIT}' -L . --batch -f batch-byte-compile ${LISP_FILE}

manuals: info-manual html-manual epub-manual pdf-manual

info-manual:
	makeinfo --output ${INFO_FILE} ${TEXI_FILE}
	install-info ${INFO_FILE} ${DOCS_DIR}/dir

html-manual:
	texi2any --html --css-include=${CSS_FILE} --output ${HTML_DIR} ${TEXI_FILE}

epub-manual:
	texi2any --epub3 --output ${DOCS_DIR}/${PROG}.epub ${TEXI_FILE}

pdf-manual:
	texi2pdf --clean --output ${DOCS_DIR}/${PROG}.pdf ${TEXI_FILE}

tag-release: check compile
	printf '%s\n' '/^## master/ s/master/${VERS}/' . w | ed -s ${NEWS_FILE}
	git commit -m 'Add ${VERS} to ${NEWS_FILE}' ${NEWS_FILE}
	awk '/^## Version/ { v ++1 } v == 1' ${NEWS_FILE} \
	| sed 's/^## //' | tr -d \` \
	| git tag -F - ${TAG}

clean:
	rm -f ${PROG}.elc
	rm -f ${INFO_FILE}
	rm -f ${DOCS_DIR}/dir
	rm -f ${DOCS_DIR}/${PROG}.{aux,fn,log,toc,vr,epub,pdf}
	rm -rf ${HTML_DIR}/*.html
