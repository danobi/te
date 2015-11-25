/* See LICENSE file for copyright and license details. */
#include <errno.h>
#include <fcntl.h>
#include <locale.h>
#include <ncurses.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/select.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <limits.h>

#include "arg.h"

#define LINSIZ        128
#define LEN(x)        (sizeof (x) / sizeof *(x))
#define UTF8LEN(ch)   ((unsigned char)ch>=0xFC ? 6 : \
                      ((unsigned char)ch>=0xF8 ? 5 : \
                      ((unsigned char)ch>=0xF0 ? 4 : \
                      ((unsigned char)ch>=0xE0 ? 3 : \
                      ((unsigned char)ch>=0xC0 ? 2 : 1)))))
#define ISASCII(ch)   ((unsigned char)ch < 0x80)
#define ISCTRL(ch)    (((unsigned char)ch < ' ') || (ch == 0x7F))
#define ISFILL(ch)    (isutf8 && !ISASCII(ch) && (unsigned char)ch<=0xBF)
#define ISBLANK(ch)   (ch == ' ' || ch == '\t' || ch == '\0')
#define ISWORDBRK(ch) (ISASCII(ch) && (ch < 0x30 || \
                      (ch > 0x39 && ch < 0x41) || \
                      (ch > 0x5A && ch < 0x5F) || \
                      ch == 0x60 || \
                      ch > 0x7A)) /* A bit flawed because we assume multibyte UTF8 chars are alnum */
#define VLEN(ch,col)  (ch=='\t' ? tabstop-(col%tabstop) : (ISCTRL(ch) ? 2: (ISFILL(ch) ? 0: 1)))  /* Length "on screen" of character */
#define VLINES(l)     (1+ (l ? l->vlen/cols: 0))
#define FIXNEXT(pos)  while(isutf8 && ISFILL(pos.line->content[pos.offset]) && ++pos.offset < pos.line->len)
#define FIXPREV(pos)  while(isutf8 && ISFILL(pos.line->content[pos.offset]) && --pos.offset > 0)
#define LINESABS      (lines - (titlewin==NULL?0:1))

/* Typedefs */
typedef struct Line Line;
struct Line {          /** The internal representation of a line of text */
	char *content;     /* Line content */
	size_t len;        /* Line byte length */
	size_t vlen;       /* On-screen line-length */
	size_t mul;        /* How many times LINSIZ is `content` malloc'd to */
	bool dirty;        /* Should I repaint on screen? */
	Line *next;        /* Next line, NULL if I'm last */
	Line *prev;        /* Previous line, NULL if I'm first */
};

typedef struct {  /** A position in the file */
	Line *line;    /* Line */
	size_t offset; /* Offset inside the line */
} Filepos;

typedef union { /** An argument to a f_* function, generic */
	long i;
	const void *v;
	Filepos(*m)(Filepos);
} Arg;

typedef struct {                    /** A keybinding */
	union {
		char c[6];                  /* Standard chars */
		int i;                      /* NCurses code */
	} keyv;
	bool(*test[4])(void);           /* Conditions to match, make sure the last one is 0x00 */
	void (*func)(const Arg * arg);  /* Function to perform */
	const Arg arg;                  /* Argument to func() */
} Key;

typedef struct Undo Undo;
struct Undo {                   /** Undo information */
	char flags;                 /* Flags: is insert/delete?, should concatenate with next undo/redo? */
	unsigned long startl, endl; /* Line number for undo/redo start and end */
	size_t starto, endo;        /* Character offset for undo/redo start and end */
	char *str;                  /* Content (added or deleted text) */
	Undo *prev;                 /* Previous undo/redo in the ring */
};

/* ENUMS */
/* Colors */
enum { DefFG, CurFG, SelFG, SpcFG, CtrlFG, Syn0FG, Syn1FG, Syn2FG, Syn3FG,
	Syn4FG, Syn5FG, Syn6FG, Syn7FG, LastFG,
};
enum { DefBG, CurBG, SelBG, LastBG, }; /* NOTE: BGs MUST have a matching FG */

/* arg->i to use in f_extsel() */
enum { ExtDefault, ExtWord, ExtLines, ExtAll, };

/* To use in lastaction */
enum { LastNone, LastDelete, LastInsert, };

/* Environment variables index */
enum { EnvFind, EnvLine, EnvOffset, EnvFile, EnvLast, };

enum {                      /* To use in statusflags */
	S_Running = 1,          /* Keep the editor running, flip to stop */
	S_Readonly = 1 << 1,    /* File is readonly, this should be enforced so that file is not modified */
	S_InsEsc = 1 << 2,      /* Insert next character as-is, even if it's a control sequence */
	S_Modified = 1 << 4,    /* File has been modified, set automatically */
	S_DirtyScr = 1 << 5,    /* Screen is dirty and should be repainted, set automatically */
	S_DirtyDown = 1 << 6,   /* Line has changed so that we must repaint from here down, set automatically */
	S_NeedResize = 1 << 7,  /* Need to resize screen in next update, set automatically */
	S_Warned = 1 << 8,      /* Set to warn about file not being saved */
	S_GroupUndo = 1 << 9,   /* Last action was an insert, so another insert should group with it, set automatically */
	S_AutoIndent = 1 << 10, /* Perform autoindenting on RET */
	S_DumpStdout = 1 << 11, /* Dump to stdout instead of writing to a file */
	S_Command = 1 << 12,    /* Command mode */
	S_Sentence = 1 << 13,   /* Sentence mode. Pass the next command's parameters (if adjective) to the verb's function */
	S_Parameter = 1 << 14,  /* Parameter mode. Pass the next character as parameter to the function of the command */
	S_Multiply = 1 << 15,   /* Multiply mode. Replay a command x times */
	S_Visual = 1 << 16,     /* Visual mode. You just select things */
};

enum {                 /* To use in Undo.flags */
	UndoIns = 1,       /* This undo is an insert (otherwise a delete) */
	UndoMore = 1 << 1, /* This undo must be chained with the next one when redoing */
	RedoMore = 1 << 2, /* This undo must be chained with the next one when undoing */
};

/* Constants */
static const char *envs[EnvLast] = {
	[EnvFind] = "TE_FIND",
	[EnvLine] = "TE_LINE",
	[EnvOffset] = "TE_OFFSET",
	[EnvFile] = "TE_FILE",
};

/* Variables */
static Line   *fstline;            /* First line */
static Line   *lstline;            /* Last line */
static Line   *scrline;            /* First line seen on screen */
static Filepos fsel;               /* Selection point on file */
static Filepos fcur;               /* Insert position on file, cursor, current position */
static Filepos fmrk = { NULL, 0 }; /* Mark */

static int      ch;                                  /* Used to store input */
static char     c[7];                                /* Used to store input */
static char    *filename = NULL;                     /* Path to file loade on buffer */
static char     title[BUFSIZ];                       /* Screen title */
static char    *tmptitle = NULL;                     /* Screen title, temporary */
static char    *tsl_str = NULL;                      /* String to print to status line */
static char    *fsl_str = NULL;                      /* String to come back from status line */
static WINDOW  *titlewin = NULL;                     /* Title ncurses window, NULL if there is a status line */
static WINDOW  *textwin = NULL;                      /* Main ncurses window */
static Undo    *undos;                               /* Undo ring */
static Undo    *redos;                               /* Redo ring */
static int      textattrs[LastFG][LastBG];           /* Text attributes for each color pair */
static int      savestep = 0;                        /* Index to determine the need to save in undo/redo action */
static long     statusflags = S_Running | S_Command; /* Status flags, very important, OR'd (see enums above) */
static int      lastaction = LastNone;               /* The last action we took (see enums above) */
static int      cols, lines;                         /* Ncurses: to use instead of COLS and LINES, wise */
static void   (*verb)(const Arg * arg);              /* Verb of current sentence */
static Arg      varg;                                /* Arguments of the verb (some will be overwritten by adjective) */
static int      vi;                                  /* Helping var to store place of verb in key chain */
static int      multiply = 1;                        /* Times to replay a command */

/* Allocate memory or die. */
static void *ecalloc(size_t, size_t);
static void *erealloc(void *, size_t);
static char *estrdup(const char *);

/* Functions */
/* f_* functions can be linked to an action or keybinding */
static void f_adjective(const Arg *);
static void f_center(const Arg *);
static void f_delete(const Arg *);
static void f_extsel(const Arg *);
static void f_insert(const Arg *);
static void f_line(const Arg *);
static void f_mark(const Arg *);
static void f_move(const Arg *);
static void f_save(const Arg *);
static void f_select(const Arg *);
static void f_spawn(const Arg *);
static void f_suspend(const Arg *);
static void f_toggle(const Arg * arg);
static void f_undo(const Arg *);

/* i_* funcions are called from inside the main code only */
static Filepos       i_addtext(char *, Filepos);
static void          i_addtoundo(Filepos, const char *);
static void          i_addundo(bool, Filepos, Filepos, char *);
static void          i_calcvlen(Line * l);
static void          i_cleanup(int);
static bool          i_deltext(Filepos, Filepos);
static void          i_die(const char *str);
static void          i_dirtyrange(Line *, Line *);
static bool          i_dotests(bool(*const a[])(void));
static void          i_dokeys(const Key[], unsigned int);
static void          i_edit(void);
static char         *i_gettext(Filepos, Filepos);
static void          i_killundos(Undo **);
static Line         *i_lineat(unsigned long);
static unsigned long i_lineno(Line *);
static void          i_multiply(void (*func)(const Arg * arg), const Arg arg);
static void          i_readfile(char *);
static void          i_resize(void);
static void          i_setup(void);
static void          i_sigwinch(int);
static void          i_sigcont(int);
static void          i_sortpos(Filepos *, Filepos *);
static void          i_termwininit(void);
static void          i_update(void);
static void          i_usage(void);
static bool          i_writefile(char *);

/* t_* functions to know whether to process an action or keybinding */
static bool t_ai(void);
static bool t_bol(void);
static bool t_eol(void);
static bool t_ins(void);
static bool t_mod(void);
static bool t_rw(void);
static bool t_redo(void);
static bool t_sel(void);
static bool t_sent(void);
static bool t_undo(void);
static bool t_vis(void);
static bool t_warn(void);

/* m_ functions represent a cursor movement and can be passed in an Arg */
static Filepos m_bof(Filepos);
static Filepos m_bol(Filepos);
static Filepos m_smartbol(Filepos);
static Filepos m_eof(Filepos);
static Filepos m_eol(Filepos);
static Filepos m_nextchar(Filepos);
static Filepos m_prevchar(Filepos);
static Filepos m_nextword(Filepos);
static Filepos m_prevword(Filepos);
static Filepos m_nextline(Filepos);
static Filepos m_prevline(Filepos);
static Filepos m_nextscr(Filepos);
static Filepos m_prevscr(Filepos);
static Filepos m_parameter(Filepos);
static Filepos m_sentence(Filepos);
static Filepos m_stay(Filepos);
static Filepos m_tomark(Filepos);
static Filepos m_tosel(Filepos);

#include "config.h"

char *argv0;

static void *
ecalloc(size_t nmemb, size_t size) {
	void *p;

	p = calloc(nmemb, size);
	if(p == NULL)
		i_die("Can't calloc.\n");
	return p;
}

static void *
erealloc(void *ptr, size_t size) {
	void *p;

	p = realloc(ptr, size);
	if(p == NULL) {
		free(ptr);
		i_die("Can't realloc.\n");
	}
	return p;
}

static char *
estrdup(const char *s) {
	char *p;

	p = strdup(s);
	if(p == NULL)
		i_die("Can't strdup.\n");
	return p;
}

/**************************************************** 
 * f_* FUNCTIONS
 *
 * Can be linked to an action or keybinding. Always return void and take
 * const Arg* 
****************************************************/

void
f_adjective(const Arg * arg) {
	statusflags &= ~S_Sentence;

	if(arg->m)
		varg.m = arg->m;
	if(arg->i)
		varg.i = arg->i;
	if(arg->v)
		varg.v = arg->v;

	i_multiply(verb, varg);
}

/* Make cursor line the one in the middle of the screen if possible,
 * refresh screen */
void
f_center(const Arg * arg) {
	(void) arg;

	int i = LINESABS / 2;

	scrline = fcur.line;
	while((i -= VLINES(scrline)) > 0 && scrline->prev)
		scrline = scrline->prev;
	i_resize();
}

/* Delete text as per arg->m. Your responsibility: call only if t_rw() */
void
f_delete(const Arg * arg) {
	char *s;
	Filepos pos0 = fcur, pos1 = arg->m(fcur);

#ifdef HOOK_DELETE_ALL
	HOOK_DELETE_ALL;
#endif
	i_sortpos(&pos0, &pos1);
	s = i_gettext(pos0, pos1);
	i_addundo(FALSE, pos0, pos1, s);
	free(s);
	if(i_deltext(pos0, pos1))
		fcur = pos0;
	else
		fcur = fsel = pos0;
	if(fsel.offset > fsel.line->len)
		fsel.offset = fsel.line->len;
	statusflags |= S_Modified;
	lastaction = LastDelete;
}

/* Extend the selection as per arg->i (see enums above) */
void
f_extsel(const Arg * arg) {
	fmrk = fcur;
	i_sortpos(&fsel, &fcur);
	switch (arg->i) {
	case ExtWord:
		if(fsel.offset > 0 && !ISWORDBRK(fsel.line->content[fsel.offset - 1]))
			fsel = m_prevword(fsel);
		if(!ISWORDBRK(fcur.line->content[fcur.offset]))
			fcur = m_nextword(fcur);
		break;
	case ExtLines:
		fsel.offset = 0;
		fcur.offset = fcur.line->len;
		break;
	case ExtAll:
		fsel.line = fstline;
		fcur.line = lstline;
		f_extsel(&(const Arg) { .i = ExtLines });
		break;
	case ExtDefault:
	default:
		if(fsel.offset == 0 && fcur.offset == fcur.line->len)
			f_extsel(&(const Arg) { .i = ExtAll });
		else if(t_sel() || ISWORDBRK(fcur.line->content[fcur.offset]))
			f_extsel(&(const Arg) { .i = ExtLines });
		else
			f_extsel(&(const Arg) { .i = ExtWord });
	}
}

/* Insert arg->v at cursor position. Your responsibility: call only if t_rw() */
void
f_insert(const Arg * arg) {
	Filepos newcur;

	newcur = i_addtext((char *) arg->v, fcur);

	if((statusflags & S_GroupUndo) && undos && (undos->flags & UndoIns) &&
	    fcur.offset == undos->endo && undos->endl == i_lineno(fcur.line) &&
	    ((char *) arg->v)[0] != '\n') {
		i_addtoundo(newcur, arg->v);
	} else {
		i_addundo(TRUE, fcur, newcur, (char *) arg->v);
		if(fcur.line != newcur.line)
			fsel = newcur;
	}
	fcur = fsel = newcur;
	statusflags |= (S_Modified | S_GroupUndo);
	lastaction = LastInsert;
}

/* Go to atoi(arg->v) line */
void
f_line(const Arg * arg) {
	long int l;

	l = atoi(arg->v);
	if(!l)
		l = 1;
	fcur.line = i_lineat(l);
	if(fcur.offset > fcur.line->len)
		fcur.offset = fcur.line->len;
	FIXNEXT(fcur);
}

/* Set mark at current position */
void
f_mark(const Arg * arg) {
	(void) arg;

	fmrk = fcur;
}

/* Move cursor and extend/shrink selection as per arg->m */
void
f_move(const Arg * arg) {
	fcur = arg->m(fcur);
	if(!t_vis())
		fsel = fcur;
}

/* Save file with arg->v filename, same if NULL. Your responsibility: call
   only if t_mod() && t_rw() */
void
f_save(const Arg * arg) {
	Undo *u;

	if(arg && arg->v && *((char *) arg->v)) {
		statusflags &= ~S_DumpStdout;
		free(filename);
		filename = estrdup((char *) arg->v);
		setenv(envs[EnvFile], filename, 1);
	} else if(filename == NULL && !(statusflags & S_DumpStdout)) {
		unsetenv(envs[EnvFile]);
#ifdef HOOK_SAVE_NO_FILE
		HOOK_SAVE_NO_FILE;
#endif
		return;
	}

	if(i_writefile((statusflags & S_DumpStdout) ? NULL : filename)) {
		statusflags &= ~S_Modified;
		for(savestep = 0, u = undos; u; u = u->prev, savestep++)
			;
	}
	if(statusflags & S_DumpStdout)
		statusflags &= ~S_Running;
}

/* Move cursor as per arg->m, then move the selection point to previous cursor */
void
f_select(const Arg * arg) {
	/* for f_select(m_tosel), which reverses the selection */
	Filepos tmppos = fcur;

	fcur = arg->m(fcur);
	fsel = tmppos;
}

/* Spawn (char **)arg->v */
void
f_spawn(const Arg * arg) {
	pid_t pid = -1;

	reset_shell_mode();
	if((pid = fork()) == 0) {
		/* setsid() used to be called here, but it does not look as a good idea
		   anymore. TODO: test and delete! */
		execvp(((char **) arg->v)[0], (char **) arg->v);
		fprintf(stderr, "te: execvp %s", ((char **) arg->v)[0]);
		perror(" failed");
		exit(EXIT_SUCCESS);
	} else if(pid > 0) {
		waitpid(pid, NULL, 0);
	}
	reset_prog_mode();
	if(titlewin)
		redrawwin(titlewin);
	redrawwin(textwin); /* TODO: make this work! */
}

void
f_suspend(const Arg * arg) {
	(void) arg;

	endwin();
	signal(SIGCONT, i_sigcont);
	raise(SIGSTOP);
}

/* Toggle the arg->i statusflag. Careful with this one! */
void
f_toggle(const Arg * arg) {
	statusflags ^= (long) arg->i;

	/* Specific operations for some toggles */
	switch (arg->i) {
	case S_Warned: /* Set warning title */
		tmptitle = "WARNING! File Modified!!!";
		break;
	}
}

/* Undo last action if arg->i >=0, redo otherwise. Your responsibility:
 * call only if t_undo() / t_redo() */
void
f_undo(const Arg * arg) {
	Filepos start, end;
	const bool isredo = (arg->i < 0);
	Undo *u;
	int n;

	u = (isredo ? redos : undos);
	fsel.offset = u->starto, fsel.line = i_lineat(u->startl);
	fcur = fsel;

	while(u) {
		start.offset = u->starto, start.line = i_lineat(u->startl);
		end.offset = u->endo, end.line = i_lineat(u->endl);

		if(isredo ^ (u->flags & UndoIns)) {
			i_sortpos(&start, &end);
			i_deltext(start, end);
			fcur = fsel = start;
		} else {
			fcur = fsel = i_addtext(u->str, fcur);
		}
		if(isredo)
			redos = u->prev, u->prev = undos, undos = u;
		else
			undos = u->prev, u->prev = redos, redos = u;

		if(!(u->flags & (isredo ? RedoMore : UndoMore)))
			break;
		u = (isredo ? redos : undos);
	}

	for(n = 0, u = undos; u; u = u->prev, n++)
		;
	/* True if we saved at this undo point */
	if(n == savestep)
		statusflags ^= S_Modified;
	else
		statusflags |= S_Modified;
}
 
/**************************************************** 
 * i_* FUNCTIONS
 * 
 * Internal functions used within main loop
 ****************************************************/

/* Add information to the last undo in the ring */
void
i_addtoundo(Filepos newend, const char *s) {
	size_t oldsiz, newsiz;

	if(!undos)
		return;
	oldsiz = strlen(undos->str);
	newsiz = strlen(s);
	undos->endl = i_lineno(newend.line);
	undos->endo = newend.offset;

	undos->str = (char *) erealloc(undos->str, 1 + oldsiz + newsiz);
	strncat(undos->str, s, newsiz);
}

/* Add new undo information to the undo ring */
void
i_addundo(bool ins, Filepos start, Filepos end, char *s) {
	Undo *u;

	if(!s || !*s)
		return;
	if(undos && (statusflags & S_GroupUndo)) {
		end.line = i_lineat((undos->endl - undos->startl) + i_lineno(end.line));
		i_addtoundo(end, s);
		return;
	}

	/* Once you make a change, the old redos go away */
	if(redos)
		i_killundos(&redos);
	u = (Undo *) ecalloc(1, sizeof(Undo));

	u->flags = (ins ? UndoIns : 0);
	u->startl = i_lineno(start.line);
	u->endl = i_lineno(end.line);
	u->starto = start.offset;
	u->endo = end.offset;
	u->str = estrdup(s);
	u->prev = undos;
	undos = u;
}

/* Add text at pos, return the position after the inserted text */
Filepos
i_addtext(char *buf, Filepos pos) {
	Line *l = pos.line, *lnew = NULL;
	size_t o = pos.offset, vlines, i = 0, il = 0;
	Filepos f;
	char c;

	vlines = VLINES(l);
	for(c = buf[0]; c != '\0'; c = buf[++i]) {
		/* newline / line feed */
		if(c == '\n' || c == '\r') {
			lnew = (Line *)ecalloc(1, sizeof(Line));
			lnew->content = ecalloc(1, LINSIZ);
			lnew->dirty = l->dirty = TRUE;
			lnew->len = lnew->vlen = 0;
			lnew->mul = 1;
			lnew->next = l->next;
			lnew->prev = l;
			if(l->next)
				l->next->prev = lnew;
			else
				lstline = lnew;
			l->next = lnew;
			l = lnew;
			/* \n in the middle of a line */
			if(o + il < l->prev->len) {
				f.line = l;
				f.offset = 0;
				i_addtext(&(l->prev->content[o + il]), f);
				l->prev->len = o + il;
				l->prev->content[o + il] = '\0';
			}
			i_calcvlen(l->prev);
			o = il = 0;
		} else {
			/* Regular char */
			if(2 + (l->len) >= LINSIZ * (l->mul))
				l->content = (char *) erealloc(l->content, LINSIZ * (++(l->mul)));
			memmove(l->content + il + o + 1, l->content + il + o,
			    (1 + l->len - (il + o)));
			l->content[il + o] = c;
			l->dirty = TRUE;
			if(il + o >= (l->len)++)
				l->content[il + o + 1] = '\0';
			il++;
		}
	}
	i_calcvlen(l);
	f.line = l;
	f.offset = il + o;
	if(lnew != NULL || vlines != VLINES(pos.line))
		statusflags |= S_DirtyDown;
	return f;
}

/* Update the vlen value of a Line */
void
i_calcvlen(Line * l) {
	size_t i;

	l->vlen = 0;
	for(i = 0; i < l->len; i++)
		l->vlen += VLEN(l->content[i], l->vlen);
}

/* Cleanup and exit */
void
i_cleanup(int sig) {
	i_killundos(&undos);
	i_killundos(&redos);
	free(filename);
	endwin();
	exit(sig > 0 ? 128 + sig : t_mod()? EXIT_FAILURE : EXIT_SUCCESS);
}

/* Quit less gracefully */
void
i_die(const char *str) {
	reset_shell_mode();
	fputs(str, stderr);
	exit(EXIT_FAILURE);
}

/* The lines between l0 and l1 should be redrawn to the screen.
 * So set the dirty flag to true for all lines between l0 and l1 */
void
i_dirtyrange(Line * l0, Line * l1) {
	Line *l;
	bool d = FALSE;

	/* Warning: l0 and/or l1 may not even exist!!! */
	for(l = fstline; l; l = l->next) {
		if(d && (l == l0 || l == l1)) {
			l->dirty = TRUE;
			break;
		}
		if(l == l0 || l == l1)
			d ^= 1;
		if(d)
			l->dirty = TRUE;
	}
}

/* Delete text between pos0 and pos1, which MUST be in order, fcur integrity
   is NOT assured after deletion, fsel integrity is returned as a bool */
bool
i_deltext(Filepos pos0, Filepos pos1) {
	Line *ldel = NULL;
	size_t vlines = 1;
	bool integrity = TRUE;

	if(pos0.line == fsel.line)
		integrity = (fsel.offset <= pos0.offset || (pos0.line == pos1.line
			&& fsel.offset > pos1.offset));
	if(pos0.line == pos1.line) {
		vlines = VLINES(pos0.line);
		memmove(pos0.line->content + pos0.offset, pos0.line->content + pos1.offset,
		    (pos0.line->len - pos1.offset));
		pos0.line->dirty = TRUE;
		pos0.line->len -= (pos1.offset - pos0.offset);
		pos0.line->content[pos0.line->len] = '\0';
		i_calcvlen(pos0.line);
	} else {
		pos0.line->len = pos0.offset;
		pos0.line->content[pos0.line->len] = '\0';
		pos0.line->dirty = TRUE; /* <<-- glitch in screen updates! */
		/* i_calcvlen is unneeded here, because we call i_addtext later */
		while(pos1.line != ldel) {
			if(pos1.line == pos0.line->next)
				i_addtext(&(pos0.line->next->content[pos1.offset]), pos0);
			if(pos0.line->next->next)
				pos0.line->next->next->prev = pos0.line;
			ldel = pos0.line->next;
			pos0.line->next = pos0.line->next->next;
			if(scrline == ldel)
				scrline = ldel->prev;
			if(lstline == ldel)
				lstline = ldel->prev;
			if(fsel.line == ldel)
				integrity = FALSE;
			free(ldel->content);
			free(ldel);
		}
	}
	if(ldel != NULL || vlines != VLINES(pos0.line))
		statusflags |= S_DirtyDown;
	return integrity;
}

/* test an array of t_ functions */
bool
i_dotests(bool(*const a[])(void)) {
	int i;

	for(i = 0; a[i]; i++) {
		if(!a[i]())
			return FALSE;
	}
	return TRUE;
}

/* Execute actions associated with the current key stored in `c` */
void
i_dokeys(const Key bindings[], unsigned int length_bindings) {
	unsigned int index, i, j;

	for(index = 0; index < length_bindings; index++) {
		/* First try to match the currently pressed key with `bindings` */
		if(((bindings[index].keyv.c &&
		     memcmp(c, bindings[index].keyv.c,
		            sizeof bindings[index].keyv.c) == 0) ||
		    (bindings[index].keyv.i && ch == bindings[index].keyv.i)) &&
		     i_dotests(bindings[index].test)) {

			/* We only want to group undo text entered in insert mode */
			if(bindings[index].func != f_insert)
				statusflags &= ~(S_GroupUndo);

			/* Handle sentences */
			if(t_sent()) {
				if(bindings[index].func == verb) {
					varg.m = m_nextline;
					i_multiply(verb, varg);
					statusflags &= ~S_Sentence;
				} else if(bindings[index].func != f_adjective) {
					statusflags &= ~S_Sentence;
					break;
				}
			} else if(bindings[index].arg.m == m_sentence) {
				statusflags |= (long) S_Sentence;
				verb = bindings[index].func;
				varg = bindings[index].arg;
				vi = index;
				break;
			}

			/* Handle parameter sentences (verb is used here to define the
			   command to execute) */
			if(statusflags & S_Parameter) {
				statusflags &= ~S_Parameter;
				i_multiply(verb, (const Arg) { .v = c });
				break;
			} else if(bindings[index].arg.m == m_parameter) {
				statusflags |= (long) S_Parameter;
				verb = bindings[index].func;
				break;
			}

			i_multiply(bindings[index].func, bindings[index].arg);
			if(t_sent() && bindings[index].func == f_adjective)
				i = vi;
			else
				i = index;

			/* Handle multi-function commands */
			if(i + 1 < length_bindings) {
				if((bindings[i + 1].keyv.c &&
				    memcmp(bindings[i + 1].keyv.c, bindings[i].keyv.c,
					       sizeof bindings[i].keyv.c) == 0) ||
				    (bindings[i + 1].keyv.i	&&
					bindings[i + 1].keyv.i == bindings[index].keyv.i)) {

					for(j = 0; j < LEN(bindings[i].test); j++) {
						if(bindings[i].test[j] != bindings[i + 1].test[j])
							break;
					}
					if(j == LEN(bindings[i].test))
						continue;
				}
			}
			break;
		}
	}
}

/* Main editing loop */
void
i_edit(void) {
	int i, tch;
	fd_set fds;
	Filepos oldsel, oldcur;

	oldsel.line = oldcur.line = fstline;
	oldsel.offset = oldcur.offset = 0;

	while(statusflags & S_Running) {
		if(fsel.line != oldsel.line)
			i_dirtyrange(oldsel.line, fsel.line);
		else if(fsel.offset != oldsel.offset)
			fsel.line->dirty = TRUE;
		if(fcur.line != oldcur.line)
			i_dirtyrange(oldcur.line, fcur.line);
		else if(fcur.offset != oldcur.offset)
			fcur.line->dirty = TRUE;
		oldsel = fsel, oldcur = fcur;
		i_update();

#ifdef HOOK_SELECT_ALL
		if(fsel.line != fcur.line || fsel.offset != fcur.offset)
			HOOK_SELECT_ALL;
#endif
		FD_ZERO(&fds);
		FD_SET(0, &fds);
		signal(SIGWINCH, i_sigwinch);
		if(select(FD_SETSIZE, &fds, NULL, NULL, NULL) == -1 &&
		   errno == EINTR) {
			signal(SIGWINCH, SIG_IGN);
			continue;
		}
		signal(SIGWINCH, SIG_IGN);
		if(!FD_ISSET(0, &fds))
			continue;
		if((ch = wgetch(textwin)) == ERR) {
			tmptitle = "ERR";
			continue;
		}

		/* NCurses special chars are processed first to avoid UTF-8 collision */
		if(ch >= KEY_MIN) {
			/* These are not really chars */
			i_dokeys(curskeys, LEN(curskeys));
			continue;
		}

		/* Mundane characters are processed later */
		c[0] = (char) ch;
		if(c[0] == 0x1B || (isutf8 && !ISASCII(c[0]))) {
			/* Multi-byte char or escape sequence */
			wtimeout(textwin, 1);
			for(i = 1; i < (c[0] == 0x1B ? 6 : UTF8LEN(c[0])); i++) {
				tch = wgetch(textwin);
				c[i] = (char)tch;
				if(tch == ERR)
					break;
			}
			for(; i < 7; i++)
				c[i] = '\0';
			wtimeout(textwin, 0);
		} else {
			c[1] = c[2] = c[3] = c[4] = c[5] = c[6] = '\0';
		}

		if(!(statusflags & S_InsEsc) && ISCTRL(c[0])) {
			i_dokeys(stdkeys, LEN(stdkeys));
			continue;
		}

		statusflags &= ~(S_InsEsc);

		if(t_rw() && t_ins()) {
			f_insert(&(const Arg) { .v = c });
		}
		else if(!t_ins()) {
			if(ch >= '0' && ch <= '9' && !(statusflags & S_Parameter)) {
				if(statusflags & S_Multiply) {
					multiply *= 10;
					multiply += (int) ch - '0';
				} else {
					statusflags |= S_Multiply;
					multiply = (int) ch - '0';
				}
			} else {
				i_dokeys(commkeys, LEN(commkeys));
			}
		}
		else {
			tmptitle = "WARNING! File is read-only!!!";
		}
	}
}

/* Return text between pos0 and pos1, which MUST be in order; you MUST free
   the returned string after use */
char *
i_gettext(Filepos pos0, Filepos pos1) {
	Line *l;
	unsigned long long i = 1;
	char *buf;

	for(l = pos0.line; l != pos1.line->next; l = l->next)
		i += 1 + (l == pos1.line ? pos1.offset : l->len) - (l == pos0.line ? pos0.offset : 0);
	buf = ecalloc(1, i);
	for(l = pos0.line, i = 0; l != pos1.line->next; l = l->next) {
		memcpy(buf + i, l->content + (l == pos0.line ? pos0.offset : 0),
		    (l == pos1.line ? pos1.offset : l->len) - (l == pos0.line ? pos0.offset : 0));
		i += (l == pos1.line ? pos1.offset : l->len) - (l == pos0.line ? pos0.offset : 0);
		if(l != pos1.line)
			buf[i++] = '\n';
	}
	return buf;
}

/* Kill (ie free) the content of the &list undo/redo ring */
void
i_killundos(Undo ** list) {
	Undo *u;

	for(; *list;) {
		u = (*list)->prev;
		free((*list)->str);
		free(*list);
		*list = u;
	}
}

/* Return the Line numbered il */
Line *
i_lineat(unsigned long il) {
	unsigned long i;
	Line *l;

	for(i = 1, l = fstline; i != il && l && l->next; i++)
		l = l->next;
	return l;
}

/* Return the line number for l0 */
unsigned long
i_lineno(Line * l0) {
	unsigned long i;
	Line *l;
	for(i = 1, l = fstline; l != l0; l = l->next)
		i++;
	return i;
}

/* Handle multiplication */
void
i_multiply(void (*func)(const Arg * arg), const Arg arg) {
	int i;

	if(statusflags & S_Multiply) {
		func(&arg);
		statusflags |= S_GroupUndo;

		for(i = 1; i < multiply; i++)
			func(&arg);

		statusflags &= ~S_GroupUndo;
		statusflags &= ~S_Multiply;
		multiply = 1;
	} else
		func(&arg);
}

/* Read file content into the Line* structure */
void
i_readfile(char *fname) {
	int fd;
	ssize_t n;
	char *buf = NULL, *linestr = "1";

	if(fname == NULL || !strcmp(fname, "-")) {
		fd = 0;
		reset_shell_mode();
	} else {
		free(filename);
		filename = estrdup(fname);
		setenv(envs[EnvFile], filename, 1);
		if((fd = open(filename, O_RDONLY)) == -1) {
			/* start at line number if specified, NOTE that filenames
			 * containing ':' are possible. */
			if((linestr = strrchr(filename, ':'))) {
				*(linestr++) = '\0';
				setenv(envs[EnvFile], filename, 1);
				fd = open(filename, O_RDONLY);
			} else {
				linestr = "1";
			}
			if(fd == -1) {
				tmptitle = "WARNING! Can't read file!!!";
				return;
			}
		}
	}

	buf = ecalloc(1, BUFSIZ + 1);
	while((n = read(fd, buf, BUFSIZ)) > 0) {
		buf[n] = '\0';
		fcur = i_addtext(buf, fcur);
	}
	if(fd > 0) {
		close(fd);
	} else {
		if((fd = open("/dev/tty", O_RDONLY)) == -1)
			i_die("Can't reopen stdin.\n");
		dup2(fd, 0);
		close(fd);
		reset_prog_mode();
	}
	free(buf);
	fcur.line = fstline;
	fcur.offset = 0;
	fsel = fcur;
	f_line(&(const Arg) { .v = linestr });
}

/* Handle term resize, ugly. TODO: clean and change */
void
i_resize(void) {
	const char *tty;
	int fd, result;
	struct winsize ws;

	if((tty = ttyname(0)) == NULL)
		return;
	fd = open(tty, O_RDWR);
	if(fd == -1)
		return;
	result = ioctl(fd, TIOCGWINSZ, &ws);
	close(fd);
	if(result == -1)
		return;
	cols = ws.ws_col;
	lines = ws.ws_row;
	endwin();
	doupdate();
	i_termwininit();
	statusflags |= S_DirtyScr;
}

/* Setup everything */
void
i_setup(void) {
	unsigned int i, j;
	Line *l = NULL;

	/* Signal handling, default */
	signal(SIGWINCH, SIG_IGN);
	signal(SIGINT, i_cleanup);
	signal(SIGTERM, i_cleanup);

	/* Some allocs */
	title[0] = '\0';

	if(!newterm(NULL, stderr, stdin)) {
		if(!(newterm("xterm", stderr, stdin)))
			i_die("Can't fallback $TERM to xterm, exiting...\n");
		tmptitle = "WARNING! $TERM not recognized, using xterm as fallback!!!";
	}

	if(has_colors()) {
		start_color();
		use_default_colors();
		for(i = 0; i < LastFG; i++) {
			for(j = 0; j < LastBG; j++) {
				/* Handle more than 8 colors */
				if(fgcolors[i] > 7)
					init_color(fgcolors[i], fgcolors[i] >> 8,
					           (fgcolors[i] >> 4) & 0xF, fgcolors[i] & 0xFF);

				if(bgcolors[j] > 7)
					init_color(bgcolors[j], bgcolors[j] >> 8, (bgcolors[j] >> 4) & 0xF,
					           bgcolors[j] & 0xFF);
				init_pair((i * LastBG) + j, fgcolors[i], bgcolors[j]);
				textattrs[i][j] = COLOR_PAIR((i * LastBG) + j) | colorattrs[i];
			}
		}
	} else {
		for(i = 0; i < LastFG; i++) {
			for(j = 0; j < LastBG; j++)
				textattrs[i][j] = bwattrs[i];
		}
	}
	lines = LINES;
	cols = COLS;
	i_termwininit();

	/* Init line structure */
	l = (Line *) ecalloc(1, sizeof(Line));
	l->content = ecalloc(1, LINSIZ);
	l->dirty = FALSE;
	l->len = l->vlen = 0;
	l->mul = 1;
	l->next = NULL;
	l->prev = NULL;
	fstline = lstline = scrline = fcur.line = l;
	fcur.offset = 0;
	fsel = fcur;
}

/* Process SIGWINCH, the terminal has been resized */
void
i_sigwinch(int unused) {
	(void) unused;

	statusflags |= S_NeedResize;
}

/* Process SIGCONT to return after STOP */
void
i_sigcont(int unused) {
	(void) unused;

	i_resize();
}

/* Exchange file positions, pos0 and pos1, if not in order */
void
i_sortpos(Filepos * pos0, Filepos * pos1) {
	Filepos p;

	for(p.line = fstline; p.line; p.line = p.line->next) {
		if(p.line == pos0->line || p.line == pos1->line) {
			if((p.line == pos0->line && (p.line == pos1->line
				    && pos1->offset < pos0->offset)) || (p.line == pos1->line
				&& p.line != pos0->line))
				p = *pos0, *pos0 = *pos1, *pos1 = p;
			break;
		}
	}
}

/* Initialize terminal */
void
i_termwininit(void) {
	raw();
	noecho();
	nl();
	if(textwin)
		delwin(textwin);
	if(USE_TERM_STATUS && tigetflag("hs") > 0) {
		tsl_str = tigetstr("tsl");
		fsl_str = tigetstr("fsl");
		textwin = newwin(lines, cols, 0, 0);
	} else {
		if(titlewin)
			delwin(titlewin);
		titlewin = newwin(1, cols, BOTTOM_TITLE ? lines - 1 : 0, 0);
		wattron(titlewin, A_REVERSE);
		textwin = newwin(lines - 1, cols, BOTTOM_TITLE ? 0 : 1, 0);
	}
	idlok(textwin, TRUE);
	keypad(textwin, TRUE);
	meta(textwin, TRUE);
	nodelay(textwin, FALSE);
	wtimeout(textwin, 0);
	curs_set(1);
	ESCDELAY = 20;
	scrollok(textwin, FALSE);
}

/* Repaint screen. This is where everything happens. Apologies for the
   unnecessary complexity and length */
void
i_update(void) {
	int iline, irow, ixrow, ivchar, i, ifg, ibg, vlines;
	int cursor_r = 0, cursor_c = 0;
	int lines3; /* How many lines fit on screen */
	long int nscr, ncur = 1, nlst = 1; /* Line number for scrline, fcur.line and lstline */
	size_t ichar;
	bool selection;
	Line *l;
	char c[7], buf[16];

	/* Check if we need to resize */
	if(statusflags & S_NeedResize)
		i_resize();

	/* Check offset */
	scrollok(textwin, TRUE); /* Here I scroll */
	for(selection = FALSE, l = fstline, iline = 1;
	    l && scrline->prev && l != scrline; iline++, l = l->next) {
		if(l == fcur.line) { /* Can't have fcur.line before scrline, move scrline up */
			i = 0;
			while(l != scrline) {
				if(VLINES(scrline) > 1) {
					i = -1;
					break;
				}
				i++;
				scrline = scrline->prev;
			}
			if(i < 0 || i > LINESABS) {
				scrline = l;
				statusflags |= S_DirtyScr;
			} else
				wscrl(textwin, -i);
			break;
		}
		if(l == fsel.line) /* Selection starts before screen view */
			selection = !selection;
	}
	for(i = irow = 0, l = scrline; l; l = l->next, irow += vlines) {
		if((vlines = VLINES(l)) > 1)
			statusflags |= S_DirtyDown; /* if any line before fcur.line has vlines>1 */
		if(fcur.line == l) {
			if(irow + vlines > 2 * LINESABS)
				statusflags |= S_DirtyScr;
			/* Can't have fcur.line after screen end, move scrline down */
			while(irow + vlines > LINESABS && scrline->next) {
				irow -= VLINES(scrline);
				i += VLINES(scrline);
				if(scrline == fsel.line)
					selection = !selection; /* We just scrolled past the selection point */
				scrline = scrline->next;
				iline++;
			}
			if(!(statusflags & S_DirtyScr))
				wscrl(textwin, i);
			break;
		}
	}
	scrollok(textwin, FALSE);
	nscr = iline;

	/* Actually update lines on screen */
	for(irow = lines3 = 0, l = scrline; irow < LINESABS;
	    irow += vlines, lines3++, iline++) {
		vlines = VLINES(l);
		if(fcur.line == l) {
			ncur = iline;
			/* Update screen cursor position */
			cursor_c = 0;
			cursor_r = irow;
			for(ichar = 0; ichar < fcur.offset; ichar++)
				cursor_c += VLEN(fcur.line->content[ichar], cursor_c);
			while(cursor_c >= cols) {
				cursor_c -= cols;
				cursor_r++;
			}
		}
		if(statusflags & S_DirtyScr || (l && l->dirty
			&& (statusflags & S_DirtyDown ? statusflags |= S_DirtyScr : 1))) {
			/* Print line content */
			if(l)
				l->dirty = FALSE;
			for(ixrow = ichar = ivchar = 0; ixrow < vlines && (irow + ixrow) < LINESABS; ixrow++) {
				wmove(textwin, (irow + ixrow), (ivchar % cols));
				while(ivchar < (1 + ixrow) * cols) {
					if(fcur.line == l && ichar == fcur.offset)
						selection = !selection;
					if(fsel.line == l && ichar == fsel.offset)
						selection = !selection;
					ifg = DefFG, ibg = DefBG;
					if(fcur.line == l)
						ifg = CurFG, ibg = CurBG;
					if(selection)
						ifg = SelFG, ibg = SelBG;
					wattrset(textwin, textattrs[ifg][ibg]);
					if(l && ichar < l->len) {
						/* Tab nightmare */
						if(l->content[ichar] == '\t') {
							wattrset(textwin, textattrs[SpcFG][ibg]);
							for(i = 0; i < VLEN('\t', ivchar); i++)
								waddstr(textwin, ((i == 0 && isutf8) ? tabstr : " "));
						} else if(l->content[ichar] == ' ') { /* Space */
							wattrset(textwin, textattrs[SpcFG][ibg]);
							waddstr(textwin, (isutf8 ? spcstr : " "));
						} else if(ISCTRL(l->content[ichar])) {
							/* Add Ctrl-char as string to avoid problems at right screen end */
							wattrset(textwin, textattrs[CtrlFG][ibg]);
							waddstr(textwin, unctrl(l->content[ichar]));
						} else if(isutf8 && !ISASCII(l->content[ichar])) {
							/* Begin multi-byte char, dangerous at right screen end */
							for(i = 0; i < UTF8LEN(l->content[ichar]); i++) {
								if(ichar + i < l->len)
									c[i] = l->content[ichar + i];
								else
									c[i] = '\0';
							}
							c[i] = '\0'; /* WARNING: we use i later... */
							waddstr(textwin, c);
						} else {
							waddch(textwin, l->content[ichar]);
						}
						ivchar += VLEN(l->content[ichar], ivchar);
						if(isutf8 && !ISASCII(l->content[ichar]) && i)
							ichar += i; /* ...here */
						else
							ichar++;
					} else {
						ifg = DefFG, ibg = DefBG;
						if(fcur.line == l) {
							ifg = CurFG;
							ibg = CurBG;
						}
						if(selection) {
							ifg = SelFG;
							ibg = SelBG;
						}
						wattrset(textwin, textattrs[ifg][ibg]);
						waddch(textwin, ' ');
						ivchar++;
						ichar++;
					}
				}
			}
		} else if(l == fsel.line || l == fcur.line) {
			selection = !selection;
		}
		if(l)
			l = l->next;
	}

	/* Calculate nlst */
	for(iline = ncur, l = fcur.line; l; l = l->next, iline++) {
		if(l == lstline)
			nlst = iline;
	}

	/* Position cursor */
	wmove(textwin, cursor_r, cursor_c);

	/* Update env */
	snprintf(buf, 16, "%ld", ncur);
	setenv(envs[EnvLine], buf, 1);
	snprintf(buf, 16, "%d", (int) fcur.offset);
	setenv(envs[EnvOffset], buf, 1);

	/* Update title */
	if(tmptitle) {
		snprintf(title, sizeof(title), "%s", tmptitle);
	} else {
		statusflags &= ~S_Warned;	/* Reset warning */
		snprintf(buf, 4, "%ld%%", (100 * ncur) / nlst);
		snprintf(title, BUFSIZ, "%s%s [%s]%s%s %ld,%d  %s",
		    t_vis()? "Visual " :
		    (t_ins()? "Insert " : "Command "),
		    (statusflags & S_DumpStdout ? "<Stdout>" : (filename == NULL ? "<No file>" : filename)),
		    (t_mod()? "[+]" : ""), (!t_rw()? "[RO]" : ""),
		    (statusflags & S_AutoIndent ? "[ai]" : ""), ncur,
		    (int) fcur.offset,
		    (scrline == fstline ? (nlst <
			    lines3 ? "All" : "Top") : (nlst - nscr <
			    lines3 ? "Bot" : buf)
		    ));
	}
	if(titlewin) {
		wmove(titlewin, 0, 0);
		for(i = 0; i < cols; i++)
			waddch(titlewin, ' ');
		mvwaddnstr(titlewin, 0, 0, title, cols);
	} else {
		putp(tsl_str);
		putp(title);
		putp(fsl_str);
	}

	/* Clean global dirty bits */
	statusflags &= ~(S_DirtyScr | S_DirtyDown | S_NeedResize);
	tmptitle = NULL;

	/* And go.... */
	if(titlewin)
		wnoutrefresh(titlewin);
	wnoutrefresh(textwin);
	doupdate();
}

/* Print help, die */
void
i_usage(void) {
	i_die("te - simple editor\n"
	      "usage: te [-a] [-d] [-r] [-u] [-t TABSTOP] [file | -]\n");
}

/* Write buffer to disk */
bool
i_writefile(char *fname) {
	int fd = 1; /* default: write to stdout */
	bool wok = TRUE;
	Line *l;

	if(fname != NULL
	    && (fd = open(fname, O_WRONLY | O_TRUNC | O_CREAT,
		    S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH |
		    S_IWOTH)) == -1) {
		/* error */
		tmptitle = "WARNING! Can't save file!!!";
		return FALSE;
	}

	for(l = fstline; wok && l; l = l->next) {
		if(write(fd, l->content, l->len) == -1 ||
		    (l->next && write(fd, "\n", 1) == -1))
			wok = FALSE;
	}
	if(fd != 1)
		close(fd);
	return wok;
}

/****************************************************
 * m_* FUNCTIONS
 *
 * Represent a cursor motion, always take a Filepos and return a new Filepos 
 * based on motion that was applied
 ****************************************************/

/* Go to beginning of file */
Filepos
m_bof(Filepos pos) {
	pos.line = fstline;
	pos.offset = 0;
	return pos;
}

/* Go to beginning of line */
Filepos
m_bol(Filepos pos) {
	pos.offset = 0;
	return pos;
}

/* Go to smart beginning of line, ie first non-whitespace character of line */
Filepos
m_smartbol(Filepos pos) {
	Filepos vbol = pos;

	vbol.offset = 0;
	while(ISBLANK(vbol.line->content[vbol.offset]) && ++vbol.offset < vbol.line->len) ;

	// Do not move the cursor forward unless we're at the very beginning of the line
	if(pos.offset != 0 && pos.offset <= vbol.offset)
		vbol.offset = 0;
	return vbol;
}

/* Go to end of file */
Filepos
m_eof(Filepos pos) {
	pos.line = lstline;
	pos.offset = pos.line->len;
	return pos;
}

/* Go to end of line */
Filepos
m_eol(Filepos pos) {
	pos.offset = pos.line->len;
	return pos;
}

/* Advance one char, next line if needed */
Filepos
m_nextchar(Filepos pos) {
	if(pos.offset < pos.line->len) {
		pos.offset++;
		FIXNEXT(pos);
	} else if(pos.line->next) {
		pos.line = pos.line->next;
		pos.offset = 0;
	}
	return pos;
}

/* Backup one char, previous line if needed */
Filepos
m_prevchar(Filepos pos) {
	if(pos.offset > 0) {
		pos.offset--;
		FIXPREV(pos);
	} else if(pos.line->prev) {
		pos.line = pos.line->prev;
		pos.offset = pos.line->len;
	}
	return pos;
}

/* Advance one word, next line if needed */
Filepos
m_nextword(Filepos pos) {
	Filepos p0 = m_nextchar(pos);

	while((p0.offset != pos.offset || p0.line != pos.line) && ISWORDBRK(pos.line->content[pos.offset]))
		pos = p0, p0 = m_nextchar(pos);	/* Find the current or next word */

	do {
		/* Move to word end */
		p0 = pos, pos = m_nextchar(pos);
	} while((p0.offset != pos.offset || p0.line != pos.line) && !ISWORDBRK(pos.line->content[pos.offset]));
	return pos;
}

/* Backup one word, previous line if needed */
Filepos
m_prevword(Filepos pos) {
	Filepos p0 = m_prevchar(pos);

	if(ISWORDBRK(pos.line->content[pos.offset]))
		while((p0.offset != pos.offset || p0.line != pos.line)
		    && ISWORDBRK(pos.line->content[pos.offset]))
			pos = p0, p0 = m_prevchar(pos);	/* Find the current or previous word */
	else
		pos = p0;

	do {
		/* Move to word start */
		p0 = pos, pos = m_prevchar(pos);
	} while((p0.offset != pos.offset || p0.line != pos.line) && !ISWORDBRK(pos.line->content[pos.offset]));
	return p0;
}

/* Advance one line, or to eol if at last line */
Filepos
m_nextline(Filepos pos) {
	size_t ivchar, ichar;

	for(ivchar = ichar = 0; ichar < pos.offset; ichar++)
		ivchar += VLEN(pos.line->content[ichar], ivchar);

	if(pos.line->next) {
		/* Remember: here we re-use ichar as a second ivchar */
		for(pos.line = pos.line->next, pos.offset = ichar = 0; ichar < ivchar && pos.offset < pos.line->len; pos.offset++)
			ichar += VLEN(pos.line->content[pos.offset], ichar);
		FIXNEXT(pos);
	} else {
		pos.offset = pos.line->len;
	}
	return pos;
}

/* Backup one line, or to bol if at first line */
Filepos
m_prevline(Filepos pos) {
	size_t ivchar, ichar;

	for(ivchar = ichar = 0; ichar < pos.offset; ichar++)
		ivchar += VLEN(pos.line->content[ichar], (ivchar % (cols - 1)));

	if(pos.line->prev) {
		/* Remember: here we re-use ichar as a second ivchar */
		for(pos.line = pos.line->prev, pos.offset = ichar = 0; ichar < ivchar && pos.offset < pos.line->len; pos.offset++)
			ichar += VLEN(pos.line->content[pos.offset], ichar);
		FIXNEXT(pos);
	} else {
		pos.offset = 0;
	}
	return pos;
}

/* Advance as many vertical lines as the screen size */
Filepos
m_nextscr(Filepos pos) {
	int i;
	Line *l;

	for(i = LINESABS, l = pos.line; l->next && i > 0; i -= VLINES(l), l = l->next)
		;
	pos.line = l;
	pos.offset = pos.line->len;
	return pos;
}

/* Go to where the adjective says */
Filepos
m_parameter(Filepos pos) {
	/* WARNING: this code is actually not used */
	return pos;
}

/* Backup as many vertical lines as the screen size */
Filepos
m_prevscr(Filepos pos) {
	int i;
	Line *l;

	for(i = LINESABS, l = pos.line; l->prev && i > 0; i -= VLINES(l), l = l->prev)
		;
	pos.line = l;
	pos.offset = 0;
	return pos;
}

/* Go to where the adjective says */
Filepos
m_sentence(Filepos pos) {
	/* WARNING: this code is actually not used */
	return pos;
}

/* Do not move */
Filepos
m_stay(Filepos pos) {
	return pos;
}

/* Go to mark if valid, stay otherwise */
Filepos
m_tomark(Filepos pos) {
	/* Be extra careful when moving to mark, as it might not exist */
	Line *l;
	for(l = fstline; l; l = l->next) {
		if(l != NULL && l == fmrk.line) {
			pos.line = fmrk.line;
			pos.offset = fmrk.offset;
			if(pos.offset > pos.line->len)
				pos.offset = pos.line->len;
			FIXNEXT(pos);
			f_mark(NULL);
			break;
		}
	}
	return pos;
}

/* Go to selection point */
Filepos
m_tosel(Filepos pos) {
	(void) pos;

	return fsel;
}

/****************************************************
 * t_* FUNCTIONS
 * Used to test for conditions, take no arguments and return bool.
 ****************************************************/

/* Returns TRUE if autoindent is on */
bool
t_ai(void) {
	return (statusflags & S_AutoIndent);
}

/* Returns TRUE if the cursor is at the beginning of the current line */
bool
t_bol(void) {
	return (fcur.offset == 0);
}

/* Returns TRUE if cursor is at the end of the current line */
bool
t_eol(void) {
	return (fcur.offset == fcur.line->len);
}

/* Returns TRUE if the file has been modified */
bool
t_mod(void) {
	return (statusflags & S_Modified);
}

/* Returns TRUE if we are not in command mode, ergo insert mode */
bool
t_ins(void) {
	return !(statusflags & S_Command);
}

/* Returns TRUE if the file is writable */
bool
t_rw(void) {
	return !(statusflags & S_Readonly);
}

/* Returns TRUE if there is anything to redo */
bool
t_redo(void) {
	return (redos != NULL);
}

/* Returns TRUE if any text is selected */
bool
t_sel(void) {
	return !(fcur.line == fsel.line && fcur.offset == fsel.offset);
}

/* Returns TRUE if a sentence has started */
bool
t_sent(void) {
	return (statusflags & S_Sentence);
}

/* Returns TRUE if there is anything to undo */
bool
t_undo(void) {
	return (undos != NULL);
}

/* Returns TRUE if we are in visual mode */
bool
t_vis(void) {
	return (statusflags & S_Visual);
}

/* 
 * Returns TRUE if we have warned the file is modified 
 *
 * Usually we have warned that the file is modified when the user
 * wants to quit
 */
bool
t_warn(void) {
	return (statusflags & S_Warned);
}

/* Main entry point */
int
main(int argc, char *argv[]) {
	/* Use system locale, hopefully UTF-8 */
	setlocale(LC_ALL, "");

	ARGBEGIN {
	case 'r':
		statusflags |= S_Readonly;
		break;
	case 'a':
		statusflags |= S_AutoIndent;
		break;
	case 'd':
		statusflags |= S_DumpStdout;
		break;
	case 't':
		tabstop = atoi(EARGF(i_usage()));
		break;
	case 'v':
		i_die("te-" VERSION "\n");
		break;
	default:
		i_usage();
		break;
	} ARGEND;

	i_setup();

	if(argc > 0)
		i_readfile(argv[0]);

	i_edit();
	i_cleanup(EXIT_SUCCESS);
	return EXIT_SUCCESS;
}
