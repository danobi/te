# te version
VERSION = 0.0

# Customize below to fit your system

# paths
PREFIX = /opt/te
MANPREFIX = ${PREFIX}/share/man

# includes and libs (ncurses)
INCS = -I. -I/usr/include
LIBS = -L/usr/lib -lc -lncurses

# flags
CPPFLAGS = -DVERSION=\"${VERSION}\" -D_BSD_SOURCE -D_POSIX_C_SOURCE=200809L
#CFLAGS = -std=c99 -pedantic -Wall -Os ${INCS} ${CPPFLAGS}
#LDFLAGS = -s ${LIBS}
CFLAGS = -ggdb -std=c99 -pedantic -Wall -Wextra -pedantic -O0 ${INCS} ${CPPFLAGS}
LDFLAGS = ${LIBS}

# Solaris
#CFLAGS = -fast ${INCS} -DVERSION=\"${VERSION}\"
#LDFLAGS = ${LIBS}

# compiler and linker
CC = cc
