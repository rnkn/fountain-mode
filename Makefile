.POSIX:
PROG		= fountain-mode
LISP_FILE	= ${PROG}.el
DEPS		= seq package-lint
NEWS_FILE	= NEWS
DOCS_DIR	= docs
TEXI_FILE	= ${DOCS_DIR}/${PROG}.texi
INFO_FILE	= ${DOCS_DIR}/${PROG}.info
CSS_FILE	= ${DOCS_DIR}/style.css
HTML_DIR	= ${DOCS_DIR}/html
VERS		!= grep -oE -m1 'Version:[ 0-9.]+' ${LISP_FILE} | tr -d :
TAG		!= echo ${VERS} | sed -E 's/Version:? ([0-9.]+)/v\1/'
INIT		= '(progn \
  (require (quote package)) \
  (push (cons "melpa" "https://melpa.org/packages/") package-archives) \
  (package-initialize) \
  (mapc (lambda (pkg) \
          (unless (package-installed-p pkg) \
            (unless (assoc pkg package-archive-contents) \
              (package-refresh-contents)) \
            (package-install pkg))) \
        (quote (${DEPS}))))'

all: clean check compile info-manual html-manual

check:
	emacs -Q --eval ${INIT} --batch -f package-lint-batch-and-exit ${LISP_FILE}

compile:
	emacs -Q --eval ${INIT} -L . --batch -f batch-byte-compile ${LISP_FILE}

info-manual:
	makeinfo ${TEXI_FILE} --output ${INFO_FILE}
	install-info ${INFO_FILE} dir

html-manual:
	makeinfo --html --css-include=${CSS_FILE} --output ${HTML_DIR} ${TEXI_FILE}

pdf-manual:
	pdftex -output-directory=${DOCS_DIR} ${TEXI_FILE}

tag-release: check compile
	sed -i~ '1 s/.*/* ${VERS}/' ${NEWS_FILE}
	git commit -m 'Add ${VERS} to ${NEWS_FILE}' ${NEWS_FILE}
	awk '/^* Version/ {v ++ 1} v == 1' ${NEWS_FILE} | sed 's/^* //' | git tag -sF - ${TAG}

clean:
	rm -f ${PROG}.elc
	rm -f ${INFO_FILE}
	rm -f dir
	rm -f ${DOCS_DIR}/${PROG}.aux
	rm -f ${DOCS_DIR}/${PROG}.fn
	rm -f ${DOCS_DIR}/${PROG}.log
	rm -f ${DOCS_DIR}/${PROG}.toc
	rm -f ${DOCS_DIR}/${PROG}.vr
	rm -f ${DOCS_DIR}/${PROG}.pdf
	rm -rf ${DOCS_DIR}/html
