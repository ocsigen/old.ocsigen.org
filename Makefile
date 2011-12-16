
APP_NAME := site

SERVER_FILES :=     \
  src/site_ocsimore.ml  \
  src/site_doc.ml       \
  src/site_doc_link.ml  \
  src/site_doc_menu.ml  \

PORT := 8080

SERVER_PACKAGE := extlib,ocsimore.wiki_site
ELIOMCFLAGS := -I _server/src
ELIOMOPTFLAGS := -I _server/src
ELIOMDEPFLAGS_SERVER := -I src

-include Makefile.local

OCSIMORE_STATIC ?= /var/www/ocsimore/static

###

all: local byte opt

LIBDIR := local/var/www/lib
JSDIR  := local/var/www/static

include Makefile.common
-include Makefile.local

distclean::
	-rm -r local

####

DIRS = local/var/lib/ocsidbm local/var/run local/var/log \
       local/var/www/lib local/etc

local: ${DIRS} local/var/www/static local/var/www/ocsimore_static

local/var/www/static/:
	mkdir -p $@
	sh files/update-symlinks.sh
	cd files/data && for f in $$(ls); do ln -s ../../../../files/data/$$f ../../local/var/www/static/; done
	cp $$(ocamlfind query ocsimore)/ocsimore.js $@
local/var/www/ocsimore_static:
	mkdir -p $@
	cp ${OCSIMORE_STATIC}/* $@

${DIRS}:
	mkdir -p $@

local/etc/${APP_NAME}.p${PORT}.conf: files/${APP_NAME}.conf.in
	sed -e "s|%%SRC%%|$$(pwd)|" \
	    -e "s|%%APPNAME%%|${APP_NAME}|" \
	    -e "s|%%LIBDIR%%|${LIBDIR}|" \
	    -e "s|%%JSDIR%%|${JSDIR}|" \
	    -e "s|%%PORT%%|${PORT}|" \
	    $< > $@

run.local: local/etc/${APP_NAME}.p${PORT}.conf
	ocsigenserver -c local/etc/${APP_NAME}.p${PORT}.conf ${DEBUG}

run.opt.local: local/etc/${APP_NAME}.p${PORT}.conf
	ocsigenserver.opt -c local/etc/${APP_NAME}.p${PORT}.conf ${DEBUG}

####

INSTALL_DIR := /var/www/${APP_NAME}

install::
	cd files/data && \
	  find \( -type d -and ! -type l \) -exec install -d -m 775 ${INSTALL_USER} ${INSTALL_DIR}/static/{} \;
	cd files/data && \
	  find ! \( -type d -or -type l \) -exec install -D -m 664 ${INSTALL_USER} {} ${INSTALL_DIR}/static/{} \;
	cd files/data && \
	  sudo -u www-data find -type l -exec cp -d {} ${INSTALL_DIR}/static/{} \;
	install -d -m 775 ${INSTALL_USER} ${INSTALL_DIR}/ocsimore_static
	install -m 664 ${INSTALL_USER} \
	  ${OCSIMORE_STATIC}/* \
	  ${INSTALL_DIR}/ocsimore_static
	install -m 644 ${INSTALL_USER} \
	  /opt/ocsigen/src/ocsimore/_build/src/site/client/ocsimore.js \
	  ${INSTALL_DIR}/static
