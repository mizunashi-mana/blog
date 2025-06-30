PY?=python3
PELICAN?=uv run pelican
WEBPACK?=npx webpack
PELICANOPTS=
CP=cp

BASEDIR=$(CURDIR)
INPUTDIR=$(BASEDIR)/content
OUTPUTDIR=$(BASEDIR)/output
PUBLICDIR=$(BASEDIR)/public
CONFFILE=$(BASEDIR)/pelicanconf.py
PUBLISHCONF=$(BASEDIR)/publishconf.py


DEBUG ?= 0
ifeq ($(DEBUG), 1)
	PELICANOPTS += -D
endif

RELATIVE ?= 0
ifeq ($(RELATIVE), 1)
	PELICANOPTS += --relative-urls
endif

.PHONY: help
help:
	@echo 'Makefile for a pelican Web site                                           '
	@echo '                                                                          '
	@echo 'Usage:                                                                    '
	@echo '   make html                           (re)generate the web site          '
	@echo '   make clean                          remove the generated files         '
	@echo '   make regenerate                     regenerate files upon modification '
	@echo '   make publish                        generate using production settings '
	@echo '   make serve [PORT=8000]              serve site at http://localhost:8000'
	@echo '   make serve-global [SERVER=0.0.0.0]  serve (as root) to $(SERVER):80    '
	@echo '   make devserver [PORT=8000]          serve and regenerate together      '
	@echo '   make github                         upload the web site via gh-pages   '
	@echo '                                                                          '
	@echo 'Set the DEBUG variable to 1 to enable debugging, e.g. make DEBUG=1 html   '
	@echo 'Set the RELATIVE variable to 1 to enable relative urls                    '
	@echo '                                                                          '

.PHONY: js-build
js-build:
	$(WEBPACK) --config webpack.prod.config.js

.PHONY: js-build-dev
js-build-dev:
	$(WEBPACK) --config webpack.dev.config.js

.PHONY: html-pelican
html-pelican: js-build
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)

.PHONY: html
html: html-pelican

.PHONY: clean
clean:
	[ ! -d $(CONTENT_DIST_ASSET_DIR) ] || rm -rf $(CONTENT_DIST_ASSET_DIR)
	[ ! -d $(OUTPUTDIR) ] || rm -rf $(OUTPUTDIR)

.PHONY: regenerate
regenerate: js-build
	$(PELICAN) -r $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)

.PHONY: serve
serve:
ifdef PORT
	$(PELICAN) -l $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS) -p $(PORT)
else
	$(PELICAN) -l $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)
endif

.PHONY: serve-global
serve-global:
ifdef SERVER
	$(PELICAN) -l $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS) -p $(PORT) -b $(SERVER)
else
	$(PELICAN) -l $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS) -p $(PORT) -b 0.0.0.0
endif

.PHONY: devserver
devserver: js-build-dev
ifdef PORT
	$(PELICAN) -lr $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS) -p $(PORT)
else
	$(PELICAN) -lr $(INPUTDIR) -o $(OUTPUTDIR) -s $(CONFFILE) $(PELICANOPTS)
endif

.PHONY: publish-pelican
publish-pelican: js-build
	$(PELICAN) $(INPUTDIR) -o $(OUTPUTDIR) -s $(PUBLISHCONF) --fatal errors $(PELICANOPTS)

.PHONY: publish
publish: publish-pelican
