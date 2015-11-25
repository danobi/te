/* A simplified way to customize */
#define USE_TERM_STATUS 1
#define BOTTOM_TITLE    1
#define HILIGHT_CURRENT 1
#define HILIGHT_SYNTAX  1
#define SHOW_NONPRINT   0

/* Things unlikely to be changed, yet still in the config.h file */
static const bool   isutf8     = TRUE;
static       int    tabstop    = 8; /* Not const, as it may be changed via param */
/* static const char   systempath[]  = "/etc/te"; */
/* static const char   userpath[]    = ".te"; */ /* Relative to $HOME */

#if SHOW_NONPRINT /* TODO: show newline character too (as $) */
static const char   tabstr[3]  = { (char)0xC2, (char)0xBB, 0x00 }; /* Double right arrow */
static const char   spcstr[3]  = { (char)0xC2, (char)0xB7, 0x00 }; /* Middle dot */
static const char   nlstr[2]   = { '$', 0x00 }; /* '$' is tradition for EOL */
#else
static const char   tabstr[2]  = { ' ', 0 };
static const char   spcstr[2]  = { ' ', 0 };
static const char   nlstr[1]   = { 0 };
#endif

/* Args to f_spawn */
#define PROMPT(prompt, default, cmd) { .v = (const char *[]){ "/bin/sh", "-c", \
	"dmenu -v >/dev/null 2>&1 || DISPLAY=\"\";"\
	"if [ -n \"$DISPLAY\" ]; then arg=\"`echo \\\"" default "\\\" | dmenu $DMENU_OPTS -p '" prompt "'`\";" \
	"else if slmenu -v >/dev/null 2>&1; then arg=\"`echo \\\"" default "\\\" | slmenu -t -p '" prompt "'`\";" \
	"else printf \"\033[0;0H\033[7m"prompt"\033[K\033[0m \" >&2; read -r arg; fi; fi &&" \
	"echo " cmd "\"$arg\" > ${TE_FIFO}", NULL } }

#define FIND    PROMPT("Find:",        "${TE_FIND}",   "/")
#define FINDBW  PROMPT("Find (back):", "${TE_FIND}",   "?")
#define SAVEAS  PROMPT("Save as:",     "${TE_FILE}",   "w")
#define REPLACE PROMPT("Replace:",     "",                "!echo -n ")
#define SED     PROMPT("Sed:",         "",                "!sed ")
#define CMD_P   PROMPT("Command:",     "/\n?\nw\nq\n!\nsyntax\noffset\nicase\nro\nai\ndump", "")

/* Hooks are launched from the main code */
#define HOOK_SAVE_NO_FILE f_spawn (&(const Arg)SAVEAS)
#undef  HOOK_DELETE_ALL   /* This affects every delete */
#undef  HOOK_SELECT_ALL   /* This affects every selection */

/* Key-bindings and stuff */
/* WARNING: use CONTROL(ch) ONLY with '@', (caps)A-Z, '[', '\', ']', '^', '_' or '?' */
/*          otherwise it may not mean what you think. See man 7 ascii for more info */
#define CONTROL(ch)   {(ch ^ 0x40)}
#define META(ch)      { 0x1B, ch }

static const Key curskeys[] = { /* Plain keys here, no CONTROL or META */
/* keyv.i,                  tests,                     func,       arg */
{ .keyv.i = KEY_BACKSPACE,  { t_rw,  t_ins,0,   0 },   f_delete,    { .m = m_prevchar } },
{ .keyv.i = KEY_BACKSPACE,  { 0,     0,    0,   0 },   f_move,      { .m = m_prevchar } },
{ .keyv.i = KEY_DC,         { t_sel, t_rw, 0,   0 },   f_delete,    { .m = m_tosel    } },
{ .keyv.i = KEY_DC,         { t_rw,  0,    0,   0 },   f_delete,    { .m = m_nextchar } },
{ .keyv.i = KEY_SDC,        { t_sel, t_rw, 0,   0 },   f_delete,    { .m = m_tosel    } },
{ .keyv.i = KEY_SDC,        { t_rw,  0,    0,   0 },   f_delete,    { .m = m_nextchar } },
{ .keyv.i = KEY_HOME,       { t_ai,  0,    0,   0 },   f_move,      { .m = m_smartbol } },
{ .keyv.i = KEY_HOME,       { 0,     0,    0,   0 },   f_move,      { .m = m_bol      } },
{ .keyv.i = KEY_END,        { 0,     0,    0,   0 },   f_move,      { .m = m_eol      } },
{ .keyv.i = KEY_SHOME,      { 0,     0,    0,   0 },   f_move,      { .m = m_bof      } },
{ .keyv.i = KEY_SEND,       { 0,     0,    0,   0 },   f_move,      { .m = m_eof      } },
{ .keyv.i = KEY_PPAGE,      { 0,     0,    0,   0 },   f_move,      { .m = m_prevscr  } },
{ .keyv.i = KEY_NPAGE,      { 0,     0,    0,   0 },   f_move,      { .m = m_nextscr  } },
{ .keyv.i = KEY_UP,         { t_sent,0,    0,   0 },   f_adjective, { .m = m_prevline } },
{ .keyv.i = KEY_UP,         { 0,     0,    0,   0 },   f_move,      { .m = m_prevline } },
{ .keyv.i = KEY_DOWN,       { t_sent,0,    0,   0 },   f_adjective, { .m = m_nextline } },
{ .keyv.i = KEY_DOWN,       { 0,     0,    0,   0 },   f_move,      { .m = m_nextline } },
{ .keyv.i = KEY_LEFT,       { t_sent,0,    0,   0 },   f_adjective, { .m = m_prevchar } },
{ .keyv.i = KEY_LEFT,       { 0,     0,    0,   0 },   f_move,      { .m = m_prevchar } },
{ .keyv.i = KEY_RIGHT,      { t_sent,0,    0,   0 },   f_adjective, { .m = m_nextchar } },
{ .keyv.i = KEY_RIGHT,      { 0,     0,    0,   0 },   f_move,      { .m = m_nextchar } },
{ .keyv.i = KEY_SLEFT,      { 0,     0,    0,   0 },   f_move,      { .m = m_prevword } },
{ .keyv.i = KEY_SRIGHT,     { 0,     0,    0,   0 },   f_move,      { .m = m_nextword } },
};

static const Key stdkeys[] = {
/* keyv.c,                test,                     func,        arg */
{ .keyv.c = CONTROL('A'), { t_ai,  0,    0,   0 },  f_move,      { .m = m_smartbol } },
{ .keyv.c = CONTROL('A'), { 0,     0,    0,   0 },  f_move,      { .m = m_bol } },
{ .keyv.c = CONTROL('B'), { 0,     0,    0,   0 },  f_move,      { .m = m_prevchar } },
{ .keyv.c = CONTROL('C'), { t_warn,t_mod,0,   0 },  f_toggle,    { .i = S_Running     } },
{ .keyv.c = CONTROL('C'), { t_mod, 0,    0,   0 },  f_toggle,    { .i = S_Warned      } },
{ .keyv.c = CONTROL('C'), { 0,     0,    0,   0 },  f_toggle,    { .i = S_Running     } },
{ .keyv.c = CONTROL('E'), { 0,     0,    0,   0 },  f_move,      { .m = m_eol         } },
{ .keyv.c = CONTROL('F'), { 0,     0,    0,   0 },  f_move,      { .m = m_nextchar    } },
{ .keyv.c = CONTROL('R'), { t_redo,t_rw, 0,   0 },  f_undo,      { .i = -1            } },
{ .keyv.c = CONTROL('Z'), { 0,     0,    0,   0 },  f_suspend,   { 0                  } },
};

/* TODO: add better paste support (if whole line was yanked, append above,
 *       not where you are) */
static const Key commkeys[] = { /* Vim command mode keys here */
/* keyv.c,                  tests,                     func,       arg */
{ .keyv.c = { '$' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_eol          } },
{ .keyv.c = { '$' },      { 0,     0,    0,   0 },  f_move,      { .m = m_eol          } },
{ .keyv.c = { '^' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_bol          } },
{ .keyv.c = { '^' },      { 0,     0,    0,   0 },  f_move,      { .m = m_bol          } },
{ .keyv.c = { 'A' },      { 0,     0,    0,   0 },  f_move,      { .m = m_eol          } },
{ .keyv.c = { 'A' },      { 0,     0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'a' },      { t_eol, 0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'a' },      { 0,     0,    0,   0 },  f_move,      { .m = m_nextchar     } },
{ .keyv.c = { 'a' },      { 0,     0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'b' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_prevword     } },
{ .keyv.c = { 'b' },      { 0,     0,    0,   0 },  f_move,      { .m = m_prevword     } },
{ .keyv.c = { 'c' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_sentence     } },
{ .keyv.c = { 'c' },      { t_rw,  0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'C' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_eol          } },
{ .keyv.c = { 'C' },      { t_rw,  0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'd' },      { t_sel, t_rw, 0,   0 },  f_delete,    { .m = m_tosel        } },
{ .keyv.c = { 'd' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_sentence     } },
{ .keyv.c = { 'D' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_eol          } },
{ .keyv.c = { 'g' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_bof          } },
{ .keyv.c = { 'g' },      { 0,     0,    0,   0 },  f_move,      { .m = m_bof          } },
{ .keyv.c = { 'G' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_eof          } },
{ .keyv.c = { 'G' },      { 0,     0,    0,   0 },  f_move,      { .m = m_eof          } },
{ .keyv.c = { 'h' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_prevchar     } },
{ .keyv.c = { 'h' },      { 0,     0,    0,   0 },  f_move,      { .m = m_prevchar     } },
{ .keyv.c = { 'i' },      { 0,     0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'j' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_nextline     } },
{ .keyv.c = { 'j' },      { 0,     0,    0,   0 },  f_move,      { .m = m_nextline     } },
{ .keyv.c = { 'k' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_prevline     } },
{ .keyv.c = { 'k' },      { 0,     0,    0,   0 },  f_move,      { .m = m_prevline     } },
{ .keyv.c = { 'l' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_nextchar     } },
{ .keyv.c = { 'l' },      { 0,     0,    0,   0 },  f_move,      { .m = m_nextchar     } },
{ .keyv.c = { 'm' },      { 0,     0,    0,   0 },  f_mark,      { .i = 0              } },
{ .keyv.c = { 'o' },      { t_rw,  t_ai, 0,   0 },  f_move,      { .m = m_eol          } },
{ .keyv.c = { 'o' },      { t_rw,  t_ai, 0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'o' },      { t_rw,  0,    0,   0 },  f_move,      { .m = m_eol          } },
{ .keyv.c = { 'o' },      { t_rw,  0,    0,   0 },  f_insert,    { .v = "\n"           } },
{ .keyv.c = { 'o' },      { t_rw,  0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'O' },      { t_rw,  t_ai, 0,   0 },  f_move,      { .m = m_bol          } },
{ .keyv.c = { 'O' },      { t_rw,  t_ai, 0,   0 },  f_move,      { .m = m_prevline     } },
{ .keyv.c = { 'O' },      { t_rw,  t_ai, 0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'O' },      { t_rw,  0,    0,   0 },  f_move,      { .m = m_bol          } },
{ .keyv.c = { 'O' },      { t_rw,  0,    0,   0 },  f_insert,    { .v = "\n"           } },
{ .keyv.c = { 'O' },      { t_rw,  0,    0,   0 },  f_move,      { .m = m_prevline     } },
{ .keyv.c = { 'O' },      { t_rw,  0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 's' },      { t_sel, t_rw, 0,   0 },  f_delete,    { .m = m_tosel        } },
{ .keyv.c = { 's' },      { t_sel, t_rw, 0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 's' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_nextchar     } },
{ .keyv.c = { 's' },      { t_rw,  0,    0,   0 },  f_toggle,    { .i = S_Command      } },
{ .keyv.c = { 'u' },      { t_undo,t_rw, 0,   0 },  f_undo,      { .i = 1              } },
{ .keyv.c = { 'v' },      { 0,     0,    0,   0 },  f_toggle,    { .i = S_Visual       } },
{ .keyv.c = { 'w' },      { t_sent,0,    0,   0 },  f_adjective, { .m = m_nextword     } },
{ .keyv.c = { 'w' },      { 0,     0,    0,   0 },  f_move,      { .m = m_nextword     } },
{ .keyv.c = { 'x' },      { t_sel, t_rw, 0,   0 },  f_delete,    { .m = m_tosel        } },
{ .keyv.c = { 'x' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_nextchar     } },
{ .keyv.c = { 'X' },      { t_sel, t_rw, 0,   0 },  f_delete,    { .m = m_tosel        } },
{ .keyv.c = { 'X' },      { t_rw,  0,    0,   0 },  f_delete,    { .m = m_prevchar     } },
{ .keyv.c = { ';' },      { 0,     0,    0,   0 },  f_spawn,     CMD_P                   },
{ .keyv.c = { ':' },      { 0,     0,    0,   0 },  f_spawn,     CMD_P                   },
{ .keyv.c = { '\'' },     { 0,     0,    0,   0 },  f_move,      { .m = m_tomark       } },
{ .keyv.c = { '/' },      { 0,     0,    0,   0 },  f_spawn,     FIND                    },
{ .keyv.c = { ' ' },      { 0,     0,    0,   0 },  f_move,      { .m = m_nextchar     } },
/* TODO: Keybindings left:
 * e/E go to the end of the word (adj) (?)
 * r replace char (verb)
 * t/T do until char (adj)
 * i do inside (adj) (ex. diw deletes current word)
 * </> ident
 */
};

/* Colors */
static const short  fgcolors[LastFG] = {
	[DefFG]  = -1,
	[CurFG]  = (HILIGHT_CURRENT?COLOR_BLACK : -1),
	[SelFG]  = COLOR_BLACK,
	[SpcFG]  = COLOR_WHITE,
	[CtrlFG] = COLOR_RED,
	[Syn0FG] = COLOR_RED,
	[Syn1FG] = COLOR_GREEN,
	[Syn2FG] = COLOR_GREEN,
	[Syn3FG] = COLOR_MAGENTA,
	[Syn4FG] = COLOR_MAGENTA,
	[Syn5FG] = COLOR_BLUE,
	[Syn6FG] = COLOR_RED,
	[Syn7FG] = COLOR_BLUE,
};

static const int colorattrs[LastFG] = {
	[DefFG]  = 0,
	[CurFG]  = 0,
	[SelFG]  = 0,
	[SpcFG]  = A_DIM,
	[CtrlFG] = A_DIM,
	[Syn0FG] = A_BOLD,
	[Syn1FG] = A_BOLD,
	[Syn2FG] = 0,
	[Syn3FG] = A_BOLD,
	[Syn4FG] = 0,
	[Syn5FG] = A_BOLD,
	[Syn6FG] = 0,
	[Syn7FG] = 0,
};

static const int bwattrs[LastFG] = {
	[DefFG]  = 0,
	[CurFG]  = 0,
	[SelFG]  = A_REVERSE,
	[SpcFG]  = A_DIM,
	[CtrlFG] = A_DIM,
	[Syn0FG] = A_BOLD,
	[Syn1FG] = A_BOLD,
	[Syn2FG] = A_BOLD,
	[Syn3FG] = A_BOLD,
	[Syn4FG] = A_BOLD,
	[Syn5FG] = A_BOLD,
	[Syn6FG] = A_BOLD,
	[Syn7FG] = A_BOLD,
};

static const short bgcolors[LastBG] = {
	[DefBG] = -1,
	[CurBG] = (HILIGHT_CURRENT ? COLOR_CYAN : -1),
	[SelBG] = COLOR_YELLOW,
};
