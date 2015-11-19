Requirements
============
In order to build `te` you need the ncurses header files.


Installation
============
Edit `config.mk` to match your local setup (`te` is installed into the
/usr/local namespace by default). Optionally, create a `config.h` file to 
further configure the editor at compile time. An examples file is provided as 
`config.def.h`.

Afterwards enter the following command to build and install `te` (use root if
needed):

    make clean install


Running `te`
============
Use the following syntax:

	te [-r] [-S | -s SYNTAX] [-t TABSTOP] [File]

Where:
-a starts with autoindent

-r opens the file read-only

-S use no syntax colors at all.

-s SYNTAX  lets you specify the syntax colors for this file

-t TABSTOP sets the tabstop for this instance of sandy


Name
============
`te` is short for Text Editor


Known issues
============
Mouse scroll down works if ncurses is compiled with `--enable-ext-mouse`.
