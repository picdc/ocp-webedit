.PHONY: server editor ocamlc toplevel html

include config/Makefile

PKG_MAIN= main-src
# PKG_OCAMLC= ocamlc-src
# PKG_TOPLEVEL= toplevel-src

DIR_TOPLEVEL= src/toplevel-src
DIR_OCAMLC= src/ocamlc-src
DIR_MAIN= src/main-src

MAIN= _obuild/$(PKG_MAIN)/$(PKG_MAIN)
# OCAMLC= _obuild/$(PKG_OCAMLC)/$(PKG_OCAMLC)
TOPLEVEL= _obuild/$(PKG_TOPLEVEL)/$(PKG_TOPLEVEL)


CONFIGBYTE= ./ocpwe-config.byte

JSFLAGS= -pretty -noinline
JSINCLUDES= -I $(DIR_TOPLEVEL)/cmicomp -I $(COMPILER_LIBS) -I $(JS_COMPILER_LIBS) -I $(JS_OF_OCAML)

all: admin server editor html

html: www/index.html

HTML_FILES=\
	html/index.html \
	html/welcome.html \
	html/fixed-top.html \
	html/static-navbar.html \
        \
	html/modals/about.html \
	html/modals/run.html \
	html/modals/create-new-project.html \
	html/modals/import-new-project.html \
	html/modals/download-project.html \
	html/modals/create-new-module.html \
	html/modals/import-new-file.html \
	html/modals/delete-file.html \
	html/modals/delete-project.html \
	html/modals/delete-directory.html \
	html/modals/compile-project.html \
	html/modals/compilation-settings.html \
	html/modals/create-new-interface.html \
	html/modals/copy-paste-howto.html \
	html/modals/login.html\
	html/modals/login-fail.html\
	\
	html/dropdowns/navbar-project.html \
	html/dropdowns/navbar-file.html \
	html/dropdowns/navbar-edit.html \
	html/dropdowns/navbar-compile.html \
	html/dropdowns/navbar-run.html \
	html/dropdowns/navbar-help.html \
	\
	html/rcdialogs.html \


www/index.html: $(HTML_FILES)
	cpp -P -I . html/index.html > www/index.html

### Rules to build the toplevel

toplevel:  
	ocp-build build $(PKG_TOPLEVEL)	
	$(MAKE) toplevel-js

toplevel-js: www/toplevel.js

www/toplevel.js: toplevel_expunged.byte
	js_of_ocaml $(JSFLAGS) -toplevel $(JSINCLUDES) \
	   -o www/toplevel.js \
	   -I _obuild/$(PKG_TOPLEVEL) \
	   $(DIR_TOPLEVEL)/toplevel_runtime.js $(JS_OF_OCAML)/weak.js \
	   $(DIR_TOPLEVEL)/stdout.js toplevel_expunged.byte

toplevel_expunged.byte: $(TOPLEVEL).byte modules.list
	ls -l $(TOPLEVEL).byte
	`ocamlc -where`/expunge $(TOPLEVEL).byte toplevel_expunged.byte \
		`cat modules.list`
	ls -l toplevel_expunged.byte

### Rules to build the administrator tool
admin:
	ocp-build webedit_adminCmd
	ocp-build install webedit_admin webedit_adminCmd

### Rules to build the server
server:
	ocp-build build common
	$(MAKE) -C src/server-src

### Rules to build the editor

editor: 
	ocp-build build $(PKG_MAIN)
	$(MAKE) editor-js

editor-js: www/main.js
www/main.js: $(MAIN).byte
	js_of_ocaml $(JSFLAGS) -o www/main.js $(MAIN).byte


### Rules to build ocamlc

# ocamlc:
# 	$(MAKE) -C $(DIR_OCAMLC)
# 	cp $(DIR_OCAMLC)/ocamlc.js www/ocamlc.js

$(CONFIGBYTE):
	$(MAKE) -C build

config: $(CONFIGBYTE)
	$(CONFIGBYTE) config

install: 
	mkdir -p $(ROOT_DIRECTORY)
	mkdir -p $(WWW_DIRECTORY)
	mkdir -p $(DATA_DIRECTORY)
	cp -f src/server-src/server.asm $(ROOT_DIRECTORY)/server.asm
	rsync -auv ./www/. $(WWW_DIRECTORY)/.
	rsync -auv ./data/. $(DATA_DIRECTORY)/.

upload:
	rsync -auv ./www/. ocamledit@ocamlpro.com:/home/edit.ocamlpro.com/www/.
	rsync -auv ./data/. ocamledit@ocamlpro.com:/home/edit.ocamlpro.com/data/.

start: $(CONFIGBYTE)
	$(CONFIGBYTE) -start server

stop: $(CONFIGBYTE)
	$(CONFIGBYTE) -stop server

restart: $(CONFIGBYTE)
	$(CONFIGBYTE) -restart server

rebuild:
	 $(CONFIGBYTE) -conf .ocp-we.conf -all config

clean:
	ocp-build clean
	rm -f ocpwe-config.byte .ocp-we.conf
	rm -rf www/main.js
	$(MAKE) -C $(DIR_OCAMLC) clean
	$(MAKE) -C server clean
	$(MAKE) -C build clean


