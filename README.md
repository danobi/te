Background
============
`te` is primarily a hacking project.
`te` is a fork of `sandy` made at suckless.

Requirements
============
In order to build `te` you need: 

- ncurses header files

- an OS that supports the SIGWINCH signal.


Installation
============
Edit `config.mk` to match your local setup (`te` is installed into the
`/usr/local` namespace by default). Optionally, create a `config.h` file to 
further configure the editor at compile time. An examples file is provided as 
`config.def.h`.

Afterwards enter the following command to build and install `te` (use root if
needed):

    make clean install


Running `te`
============
Use the following syntax:

	te [-adrv] [-t TABSTOP] [file]

Where:

- `-a` starts with autoindent

- `-d` dumps to stdout instead of saving to a file

- `-r` opens the file read-only

- `-t TABSTOP` sets the tabstop for this instance of `te`

- `-v` prints version info


Name
============
`te` is short for Text Editor


Known issues
============
- Can't execute commands (via ':' operator) 
