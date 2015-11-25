# te - simple ncurses editor
# See LICENSE file for copyright and license details.

include build/config.mk

.POSIX:
.SUFFIXES: .c .o

HDR = src/arg.h

SRC = src/te.c

OBJ = $(SRC:.c=.o)

all: options te

options:
	@echo te build options:
	@echo "CFLAGS   = $(CFLAGS)"
	@echo "LDFLAGS  = $(LDFLAGS)"
	@echo "CC       = $(CC)"

$(OBJ): config.h build/config.mk

.o:
	@echo LD $@
	@$(LD) -o $@ $< $(LDFLAGS)

.c.o:
	@echo CC $<
	@$(CC) -c -o $@ $< $(CFLAGS)

config.h:
	@echo creating $@ from config.def.h
	@cp config.def.h $@

te: $(OBJ)
	@echo CC -o $@
	@$(CC) -o $@ $(OBJ) $(LDFLAGS)

clean:
	@echo cleaning
	@rm -f te $(OBJ) te-$(VERSION).tar.gz

full-clean: clean
	@echo full cleaning
	@rm -f config.h

dist: clean
	@echo creating dist tarball
	@mkdir -p te-$(VERSION)
	@cp -R LICENSE Makefile config.mk config.def.h \
		README TODO te.1 $(HDR) $(SRC) te-$(VERSION)
	@tar -cf te-$(VERSION).tar te-$(VERSION)
	@gzip te-$(VERSION).tar
	@rm -rf te-$(VERSION)

install: all
	@echo installing executable file to $(DESTDIR)$(PREFIX)/bin
	@mkdir -p $(DESTDIR)$(PREFIX)/bin
	@cp -f te $(DESTDIR)$(PREFIX)/bin
	@chmod 755 $(DESTDIR)$(PREFIX)/bin/te
	@echo installing manual page to $(DESTDIR)$(MANPREFIX)/man1
	@mkdir -p $(DESTDIR)$(MANPREFIX)/man1
	@sed "s/VERSION/$(VERSION)/g" < te.1 > $(DESTDIR)$(MANPREFIX)/man1/te.1
	@chmod 644 $(DESTDIR)$(MANPREFIX)/man1/te.1

uninstall:
	@echo removing executable file from $(DESTDIR)$(PREFIX)/bin
	@rm -f $(DESTDIR)$(PREFIX)/bin/te
	@echo removing manual page from $(DESTDIR)$(MANPREFIX)/man1
	@rm -f $(DESTDIR)$(MANPREFIX)/man1/te.1

.PHONY: all options clean dist install uninstall
