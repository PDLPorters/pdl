/*
 *	Gnu.xs --- GNU Readline wrapper module
 *
 *	$Id: Gnu.xs,v 1.112 2009/02/27 12:44:41 hiroo Exp $
 *
 *	Copyright (c) 2009 Hiroo Hayashi.  All rights reserved.
 *
 *	This program is free software; you can redistribute it and/or
 *	modify it under the same terms as Perl itself.
 */

#ifdef __cplusplus
extern "C" {
#endif
#define PERLIO_NOT_STDIO 0
#include "EXTERN.h"
#include "perl.h"
#include "XSUB.h"
#include "ppport.h"
#ifdef __cplusplus
}
#endif

#include <stdio.h>
#ifdef __CYGWIN__
#include <sys/termios.h>
#endif /* __CYGWIN__ */
#include <readline/readline.h>
#include <readline/history.h>

/*
 * Perl 5.005 requires an ANSI C Compiler.  Good news.
 * But I should still support legacy C compilers now.
 */
/* Adapted from BSD /usr/include/sys/cdefs.h. */
#if defined (__STDC__)
#  if !defined (PARAMS)
#    define PARAMS(protos) protos
#  endif
#else /* !__STDC__ */
#  if !defined (PARAMS)
#    define PARAMS(protos) ()
#  endif
#endif /* !__STDC__ */

/*
 * In Readline 4.2 many variables, function arguments, and function
 * return values are now declared `const' where appropriate.
 */
#if (RL_READLINE_VERSION < 0x0402)
#define CONST
#else  /* (RL_READLINE_VERSION >= 0x0402) */
#define CONST const
#endif /* (RL_READLINE_VERSION >= 0x0402) */

typedef char *	t_xstr;		/* string which must be xfreed */

/*
 * compatibility definitions
 */

/* rl_last_func() is defined in rlprivate.h */
extern Function *rl_last_func;

/* features introduced by GNU Readline 2.2 */
#if (RL_READLINE_VERSION < 0x0202)
static int rl_unbind_function_in_map(){ return 0; }
static int rl_unbind_command_in_map(){ return 0; }
#endif /* (RL_READLINE_VERSION < 0x0202) */

/* features introduced by GNU Readline 4.0 */
#if (RL_VERSION_MAJOR < 4)
extern void rl_extend_line_buffer PARAMS((int));
extern char **rl_funmap_names PARAMS((void));
/* documented by Readline 4.2 but already implemented by 2.0. */
extern int rl_add_funmap_entry PARAMS((CONST char *, Function *));

/* dummy variable/function definition */
static int rl_erase_empty_line = 0;
static int rl_catch_signals = 1;
static int rl_catch_sigwinch = 1;
static Function *rl_pre_input_hook;
static VFunction *rl_completion_display_matches_hook;
static VFunction *rl_prep_term_function;
static VFunction *rl_deprep_term_function;

static void rl_cleanup_after_signal(){}
static void rl_free_line_state(){}
static void rl_reset_after_signal(){}
static void rl_resize_terminal(){}
static void rl_prep_terminal(){}
static void rl_deprep_terminal(){}
static int rl_execute_next(){ return 0; }
static void rl_display_match_list(){}
/*
 * Before GNU Readline Library Version 4.0, rl_save_prompt() was
 * _rl_save_prompt and rl_restore_prompt() was _rl_restore_prompt().
 */
extern void _rl_save_prompt PARAMS((void));
extern void _rl_restore_prompt PARAMS((void));
static void rl_save_prompt() { _rl_save_prompt(); }
static void rl_restore_prompt() { _rl_restore_prompt(); }
#endif /* (RL_VERSION_MAJOR < 4) */

/* features introduced by GNU Readline 4.1 */
#if (RL_READLINE_VERSION < 0x0401)
static int rl_already_prompted = 0;
static int rl_num_chars_to_read = 0;
static int rl_gnu_readline_p = 0;
static int rl_on_new_line_with_prompt(){ return 0; }
#endif /* (RL_READLINE_VERSION < 0x0401) */

/* features introduced by GNU Readline 4.2 */
#if (RL_READLINE_VERSION < 0x0402)
static int rl_set_prompt(){ return 0; }
static int rl_clear_pending_input(){ return 0; }
static int rl_set_keyboard_input_timeout(){ return 0; }
static int rl_alphabetic(){ return 0; }
static int rl_set_paren_blink_timeout(){ return 0; }
static void rl_set_screen_size(int row, int col){}
static void rl_get_screen_size(int *row, int *col){}

typedef int rl_command_func_t PARAMS((int, int));
typedef char *rl_compentry_func_t PARAMS((const char *, int));

static char *rl_executing_macro = NULL;
static int rl_explicit_arg = 0;
static int rl_numeric_arg = 0;
static int rl_editing_mode = 0;
static int rl_readline_state = 0;
static Function *rl_directory_rewrite_hook = NULL;
static char *history_word_delimiters = " \t\n;&()|<>";

/* Provide backwards-compatible entry points for old function names
   which are rename from readline-4.2. */
static void
rl_free_undo_list ()
{
  free_undo_list ();
}

static int
rl_crlf ()
{
  return crlf ();
}

static void
rl_tty_set_default_bindings (keymap)
Keymap keymap;
{
#if (RL_VERSION_MAJOR >= 4)
  rltty_set_default_bindings (keymap);
#endif /* (RL_VERSION_MAJOR >= 4) */
}

static int
rl_ding ()
{
  return ding ();
}

static char **
rl_completion_matches (s, f)
     char *s;
     rl_compentry_func_t *f;
{
  return completion_matches (s, (CPFunction *)f);
}

static char *
rl_username_completion_function (s, i)
     const char *s;
     int i;
{
  return username_completion_function ((char *)s, i);
}

static char *
rl_filename_completion_function (s, i)
     const char *s;
     int i;
{
  return filename_completion_function ((char *)s, i);
}
#endif /* (RL_READLINE_VERSION >= 0x0402) */

#if (RL_READLINE_VERSION < 0x0403)
/* features introduced by GNU Readline 4.2a */
static int rl_readline_version = RL_READLINE_VERSION;
/* This function does not link with win32 readline...   why?
 * See XSUB defs below when fixing (termcap, is this called?)
 * */
/* extern char *rl_get_termcap PARAMS((const char *)); */

/* features introduced by GNU Readline 4.3 */
static int rl_completion_suppress_append = 0;
static int rl_completion_mark_symlink_dirs = 0;
static void rl_replace_line(){}
static int rl_completion_mode(){ return 0; }
#endif /* (RL_READLINE_VERSION < 0x0403) */

#if (RL_VERSION_MAJOR < 5)
/* features introduced by GNU Readline 5.0 */
static int history_write_timestamps = 0;
static int rl_completion_quote_character = 0;
static int rl_completion_suppress_quote = 0;
static int rl_completion_found_quote = 0;
static Function *rl_completion_word_break_hook = NULL;
static int rl_bind_key_if_unbound_in_map(){ return 0; }
static int rl_bind_keyseq_in_map(){ return 0; }
static int rl_bind_keyseq_if_unbound_in_map(){ return 0; }
/* This function does not link with win32 readline...   why?
 * See XSUB defs below when fixing (termcap, is this called?)
 * */
/* static void rl_tty_unset_default_bindings(){} */
static void add_history_time(){}
static time_t history_get_time(){ return 0; }
#endif /* (RL_VERSION_MAJOR < 5) */

#if (RL_READLINE_VERSION < 0x0501)
/* features introduced by GNU Readline 5.1 */
static int rl_prefer_env_winsize = 0;
static char *rl_variable_value(CONST char * v){ return NULL; }
static void rl_reset_screen_size(){}

#endif /* (RL_READLINE_VERSION < 0x0501) */

#if (RL_VERSION_MAJOR < 6)
/* features introduced by GNU Readline 6.0 */
static char *rl_display_prompt = NULL;
static int rl_sort_completion_matches = 0;
static int rl_completion_invoking_key = 0;
/*
  NOT IMPLEMENTED YET !!!FIXIT!!!
static int rl_save_state(struct readline_state *sp){ return 0; }
static int rl_restore_state(struct readline_state *sp){ return 0; }
 */
static void rl_echo_signal_char(int sig){}
#endif /* (RL_VERSION_MAJOR < 6) */

/*
 * utility/dummy functions
 */                                                                                
/* from GNU Readline:xmalloc.h */
#ifndef PTR_T
#ifdef __STDC__
#  define PTR_T void *
#else
#  define PTR_T char *
#endif
#endif /* !PTR_T */

/* from GNU Readline:xmalloc.c */
extern PTR_T xmalloc PARAMS((int));
/* This function does not link with win32 readline...why?
 * See XSUB defs below when fixing (from curses)
 * */
extern char *tgetstr PARAMS((const char *, char **));
/* This function does not link with win32 readline...why?
 * See XSUB defs below when fixing (from curses)
 * */
extern int tputs PARAMS((const char *, int, int (*)(int)));

/*
 * Using xfree() in GNU Readline Library causes problem with Solaris
 * 2.5.  It seems that the DLL mechanism of Solaris 2.5 links another
 * xfree() that does not do NULL argument check.
 * I choose this as default since some other OSs may have same problem.
 * usemymalloc=n is required.
 */
#ifdef OS2_USEDLL
/* from GNU Readline:xmalloc.c */
extern PTR_T xfree PARAMS((PTR_T));

#else /* not OS2_USEDLL */
static void
xfree (string)
     PTR_T string;
{
  if (string)
    free (string);
}
#endif /* not OS2_USEDLL */

static char *
dupstr(s)			/* duplicate string */
     CONST char * s;
{
  /*
   * Use xmalloc(), because allocated block will be freed in the GNU
   * Readline Library routine.
   * Don't make a macro, because the variable 's' is evaluated twice.
   */
  int len = strlen(s) + 1;
  char *d = xmalloc(len);
  Copy(s, d, len, char);	/* Is Copy() better than strcpy() in XS? */
  return d;
}

/*
 * for tputs XS routine
 */
static char *tputs_ptr;
static int
tputs_char(c)
     int c;
{
  return *tputs_ptr++ = c;
}

/*
 * return name of FUNCTION.
 * I asked Chet Ramey to add this function in readline/bind.c.  But he
 * did not, since he could not find any reasonable excuse.
 */
static const char *
rl_get_function_name (function)
     rl_command_func_t *function;
{
  register int i;

  rl_initialize_funmap ();

  for (i = 0; funmap[i]; i++)
    if (funmap[i]->function == function)
      return ((const char *)funmap[i]->name); /* cast is for oldies */
  return NULL;
}

/*
 * from readline-4.0:complete.c
 * Redefine here since the function defined as static in complete.c.
 * This function is used for default value for rl_filename_quoting_function.
 */
static char * rl_quote_filename PARAMS((char *s, int rtype, char *qcp));

static char *
rl_quote_filename (s, rtype, qcp)
     char *s;
     int rtype;
     char *qcp;
{
  char *r;

  r = xmalloc (strlen (s) + 2);
  *r = *rl_completer_quote_characters;
  strcpy (r + 1, s);
  if (qcp)
    *qcp = *rl_completer_quote_characters;
  return r;
}

/*
 *	string variable table for _rl_store_str(), _rl_fetch_str()
 */

static struct str_vars {
  char **var;
  int accessed;
  int read_only;
} str_tbl[] = {
  /* When you change length of rl_line_buffer, you must call
     rl_extend_line_buffer().  See _rl_store_rl_line_buffer() */
  { NULL,                                     0, 0 }, /* 0 */
  { NULL,                                             0, 1 }, /* 1 */
  { NULL,                     0, 1 }, /* 2 */
  { NULL,                             0, 0 }, /* 3 */
  { NULL,                             0, 0 }, /* 4 */
  
  { NULL,         0, 0 }, /* 5 */
  { NULL,              0, 0 }, /* 6 */
  { NULL,     0, 0 }, /* 7 */
  { NULL,          0, 0 }, /* 8 */
  { NULL,           0, 0 }, /* 9 */
  { NULL,                    0, 0 }, /* 10 */
  
  { NULL,                             0, 0 }, /* 11 */
  { NULL,                     0, 0 }, /* 12 */

  { NULL,                             0, 0 }, /* 13 */
  { NULL,                             0, 0 }, /* 14 */
  { NULL,                                     0, 0 }  /* 15 */
};

/* 
 * Must call this routine to complete initialization of str_tbl[]  
 * before it is used the first time  
 */  
void gnu_xs_rl_ini_str_tbl () {
      str_tbl[0].var = &rl_line_buffer;
      str_tbl[1].var = &rl_prompt;
      str_tbl[2].var = (char **)&rl_library_version;
      str_tbl[3].var = (char **)&rl_terminal_name;
      str_tbl[4].var = (char **)&rl_readline_name;
      str_tbl[5].var = (char **)&rl_basic_word_break_characters;
      str_tbl[6].var = (char **)&rl_basic_quote_characters;
      str_tbl[7].var = (char **)&rl_completer_word_break_characters;
      str_tbl[8].var = (char **)&rl_completer_quote_characters;
      str_tbl[9].var = (char **)&rl_filename_quote_characters;
      str_tbl[10].var = (char **)&rl_special_prefixes;
      str_tbl[11].var = &history_no_expand_chars;
      str_tbl[12].var = &history_search_delimiter_chars;
      str_tbl[13].var = &rl_executing_macro;
      str_tbl[14].var = &history_word_delimiters;
      str_tbl[15].var = &rl_display_prompt;
};

/*
 *	integer variable table for _rl_store_int(), _rl_fetch_int()
 */
static struct int_vars {
  int *var;
  int charp;
  int read_only;
} int_tbl[] = {
	{ NULL,                                     0, 0 }, /* 0 */
	{ NULL,                                     0, 0 }, /* 1 */
	{ NULL,                                     0, 0 }, /* 2 */
	{ NULL,                                     0, 0 }, /* 3 */
	{ NULL,                             0, 0 }, /* 4 */

	{ NULL,                     0, 0 }, /* 5 */
	{ NULL,             0, 0 }, /* 6 */
	{ NULL,             0, 0 }, /* 7 */
	{ NULL,             0, 0 }, /* 8 */
	{ NULL,             0, 0 }, /* 9 */
	{ NULL,                     0, 0 }, /* 10 */

	{ NULL,                             0, 0 }, /* 11 */
	{ NULL,                             0, 0 }, /* 12 */
#if (RL_READLINE_VERSION >= 0x0402)
	{ NULL,			0, 1 },	/* 13 */
#else /* (RL_READLINE_VERSION < 0x0402) */
	{ NULL,				0, 1 },	/* 13 */
#endif /* (RL_READLINE_VERSION < 0x0402) */
	{ NULL,                     0, 0 }, /* 14 */
	{ (int *)NULL,              1, 0 }, /* 15 */
	{ (int *)NULL,                      1, 0 }, /* 16 */
	{ (int *)NULL,              1, 0 }, /* 17 */
	{ NULL,             0, 0 }, /* 18 */
	{ NULL,                     0, 0 }, /* 19 */
	{ NULL,                             0, 0 }, /* 20 */
	{ NULL,                             0, 0 }, /* 21 */
	{ NULL,                     0, 0 }, /* 22 */
	{ NULL,                     0, 0 }, /* 23 */
	{ NULL,                             0, 0 }, /* 24 */
	{ NULL,                             0, 1 }, /* 25 */
	{ NULL,                             0, 0 }, /* 26 */
	{ NULL,                             0, 0 }, /* 27 */
	{ NULL,                             0, 0 }, /* 28 */
	{ NULL,                             0, 0 }, /* 29 */
	{ NULL,             0, 0 }, /* 30 */
	{ NULL,                     0, 0 }, /* 31 */
	{ NULL,                     0, 1 }, /* 32 */
	{ NULL,             0, 0 }, /* 33 */
	{ NULL,             0, 0 }, /* 34 */
	{ NULL,             0, 0 }, /* 35 */
	{ NULL,                     0, 0 }, /* 36 */
	{ NULL,             0, 0 }, /* 37 */
	{ NULL,                     0, 0 }, /* 38 */
	{ NULL,             0, 0 }, /* 39 */
	{ NULL,             1, 0 }  /* 40 */
};
/*
 * Must call this routine to complete initialization of int_tbl[]  
 * before it is used the first time  
 */  
VOID GNU_xs_rl_ini_int_tbl () {
	int_tbl[ 0].var = &rl_point;
	int_tbl[ 1].var = &rl_end;
	int_tbl[ 2].var = &rl_mark;
	int_tbl[ 3].var = &rl_done;
        int_tbl[ 4].var = &rl_pending_input;
	int_tbl[ 5].var = &rl_completion_query_items;
	int_tbl[ 6].var = &rl_completion_append_character;
	int_tbl[ 7].var = &rl_ignore_completion_duplicates;
	int_tbl[ 8].var = &rl_filename_completion_desired;
	int_tbl[ 9].var = &rl_filename_quoting_desired;
	int_tbl[10].var = &rl_inhibit_completion;
	int_tbl[11].var = &history_base;
	int_tbl[12].var = &history_length;
#if (RL_READLINE_VERSION >= 0x0402)
	int_tbl[13].var = &history_max_entries;
#else /* (RL_READLINE_VERSION < 0x0402) */
	int_tbl[13].var = &max_input_history;
#endif /* (RL_READLINE_VERSION < 0x0402) */
	int_tbl[14].var = &history_write_timestamps;
	int_tbl[15].var = (int *)&history_expansion_char;
	int_tbl[16].var = (int *)&history_subst_char;
	int_tbl[17].var = (int *)&history_comment_char;
	int_tbl[18].var = &history_quotes_inhibit_expansion;
	int_tbl[19].var = &rl_erase_empty_line;
	int_tbl[20].var = &rl_catch_signals;
	int_tbl[21].var = &rl_catch_sigwinch;
	int_tbl[22].var = &rl_already_prompted;
	int_tbl[23].var = &rl_num_chars_to_read;
	int_tbl[24].var = &rl_dispatching;
	int_tbl[25].var = &rl_gnu_readline_p;
	int_tbl[26].var = &rl_readline_state;
	int_tbl[27].var = &rl_explicit_arg;
	int_tbl[28].var = &rl_numeric_arg;
	int_tbl[29].var = &rl_editing_mode;
	int_tbl[30].var = &rl_attempted_completion_over;
	int_tbl[31].var = &rl_completion_type;
	int_tbl[32].var = &rl_readline_version;
	int_tbl[33].var = &rl_completion_suppress_append;
	int_tbl[34].var = &rl_completion_quote_character;
	int_tbl[35].var = &rl_completion_suppress_quote;
	int_tbl[36].var = &rl_completion_found_quote;
	int_tbl[37].var = &rl_completion_mark_symlink_dirs;
	int_tbl[38].var = &rl_prefer_env_winsize;
	int_tbl[39].var = &rl_sort_completion_matches;
	int_tbl[40].var = &rl_completion_invoking_key;
};

/*
 *	PerlIO variables for _rl_store_iostream(), _rl_fetch_iostream()
 */
static PerlIO *instreamPIO = NULL;
static PerlIO *outstreamPIO = NULL;

/*
 *	function pointer variable table for _rl_store_function(),
 *	_rl_fetch_funtion()
 */

static int startup_hook_wrapper PARAMS((void));
static int event_hook_wrapper PARAMS((void));
static int getc_function_wrapper PARAMS((PerlIO *));
static void redisplay_function_wrapper PARAMS((void));
static char *completion_entry_function_wrapper PARAMS((const char *, int));;
static char **attempted_completion_function_wrapper PARAMS((char *, int, int));
static char *filename_quoting_function_wrapper PARAMS((char *text, int match_type,
						    char *quote_pointer));
static char *filename_dequoting_function_wrapper PARAMS((char *text,
						      int quote_char));
static int char_is_quoted_p_wrapper PARAMS((char *text, int index));
static void ignore_some_completions_function_wrapper PARAMS((char **matches));
static int directory_completion_hook_wrapper PARAMS((char **textp));
static int history_inhibit_expansion_function_wrapper PARAMS((char *str, int i));
static int pre_input_hook_wrapper PARAMS((void));
static void completion_display_matches_hook_wrapper PARAMS((char **matches,
							 int len, int max));
static char *completion_word_break_hook_wrapper PARAMS((void));
static int prep_term_function_wrapper PARAMS((int meta_flag));
static int deprep_term_function_wrapper PARAMS((void));
static int directory_rewrite_hook_wrapper PARAMS((char **));

enum { STARTUP_HOOK, EVENT_HOOK, GETC_FN, REDISPLAY_FN,
       CMP_ENT, ATMPT_COMP,
       FN_QUOTE, FN_DEQUOTE, CHAR_IS_QUOTEDP,
       IGNORE_COMP, DIR_COMP, HIST_INHIBIT_EXP,
       PRE_INPUT_HOOK, COMP_DISP_HOOK, COMP_WD_BRK_HOOK,
       PREP_TERM, DEPREP_TERM, DIR_REWRITE
};

static struct fn_vars {
  Function **rlfuncp;		/* GNU Readline Library variable */
  Function *defaultfn;		/* default function */
  Function *wrapper;		/* wrapper function */
  SV *callback;			/* Perl function */
} fn_tbl[] = {
  { NULL, NULL,	NULL, NULL }, /* 0 */
  { NULL, NULL,	NULL, NULL }, /* 1 */
  { NULL, NULL, NULL, NULL }, /* 2 */
  { NULL, NULL, NULL, NULL }, /* 3 */
  { NULL, NULL, NULL, NULL }, /* 4 */
  { NULL, NULL, NULL, NULL }, /* 5 */
  { NULL, NULL, NULL, NULL }, /* 6 */
  { NULL, NULL, NULL, NULL }, /* 7 */
  { NULL, NULL, NULL, NULL }, /* 8 */
  { NULL, NULL, NULL, NULL }, /* 9 */
  { NULL, NULL, NULL, NULL }, /* 10 */
  { NULL, NULL, NULL, NULL }, /* 11 */
  { NULL, NULL, NULL, NULL }, /* 12 */
  { NULL, NULL, NULL, NULL }, /* 13 */
  { NULL, NULL, NULL, NULL }, /* 14 */
  { NULL, NULL, NULL, NULL }, /* 15 */
  { NULL, NULL, NULL, NULL }, /* 16 */
  { NULL, NULL, NULL, NULL }  /* 17 */
};

/* Must call this routine to complete initialization of fn_tbl[]  
 * before it is used the first time  
 */  
void gnu_xs_rl_ini_fn_tbl () {
	/*  0 */
	fn_tbl[ 0].rlfuncp  = &rl_startup_hook;
	fn_tbl[ 0].defaultfn = NULL;
	fn_tbl[ 0].wrapper   = startup_hook_wrapper;
	fn_tbl[ 0].callback  = NULL;

	/* 1 */
	fn_tbl[ 1].rlfuncp  = &rl_event_hook;
	fn_tbl[ 1].defaultfn = NULL;
	fn_tbl[ 1].wrapper   = event_hook_wrapper;
	fn_tbl[ 1].callback  = NULL;

	/* 2 */
	fn_tbl[ 2].rlfuncp  = &rl_getc_function;
	fn_tbl[ 2].defaultfn = rl_getc;
	fn_tbl[ 2].wrapper   = getc_function_wrapper;
	fn_tbl[ 2].callback  = NULL;

	/* 3 */
	fn_tbl[ 3].rlfuncp  = (Function **)&rl_redisplay_function;
	fn_tbl[ 3].defaultfn =  (Function *)rl_redisplay;
	fn_tbl[ 3].wrapper   =  (Function *)redisplay_function_wrapper;
	fn_tbl[ 3].callback  =  NULL;

	/* 4 */
	fn_tbl[ 4].rlfuncp  = (Function **)&rl_completion_entry_function;
	fn_tbl[ 4].defaultfn = 	NULL;
	fn_tbl[ 4].wrapper   =  (Function *)completion_entry_function_wrapper;
	fn_tbl[ 4].callback  =  NULL;

	/* 5 */
	fn_tbl[ 5].rlfuncp  = (Function **)&rl_attempted_completion_function;
	fn_tbl[ 5].defaultfn = NULL;
	fn_tbl[ 5].wrapper   =  (Function *)attempted_completion_function_wrapper;
	fn_tbl[ 5].callback  =  NULL;

	/* 6 */
	fn_tbl[ 6].rlfuncp  = (Function **)&rl_filename_quoting_function;
	fn_tbl[ 6].defaultfn =  (Function *)rl_quote_filename;
	fn_tbl[ 6].wrapper   =  (Function *)filename_quoting_function_wrapper;
	fn_tbl[ 6].callback  =  NULL;

	/* 7 */
	fn_tbl[ 7].rlfuncp  = (Function **)&rl_filename_dequoting_function;
	fn_tbl[ 7].defaultfn =  NULL;
	fn_tbl[ 7].wrapper   =  (Function *)filename_dequoting_function_wrapper;
	fn_tbl[ 7].callback  =  NULL;

	/* 8 */
	fn_tbl[ 8].rlfuncp  = (Function **)&rl_char_is_quoted_p;
	fn_tbl[ 8].defaultfn =  NULL;
	fn_tbl[ 8].wrapper   =  (Function *)char_is_quoted_p_wrapper;
	fn_tbl[ 8].callback  =  NULL;

	/* 9 */
	fn_tbl[ 9].rlfuncp  = (Function **)&rl_ignore_some_completions_function;
	fn_tbl[ 9].defaultfn =  NULL;
	fn_tbl[ 9].wrapper   =  (Function *)ignore_some_completions_function_wrapper;
	fn_tbl[ 9].callback  =  NULL;

	/* 10 */
	fn_tbl[10].rlfuncp  = (Function **)&rl_directory_completion_hook;
	fn_tbl[10].defaultfn = 	NULL;
	fn_tbl[10].wrapper   =  (Function *)directory_completion_hook_wrapper;
	fn_tbl[10].callback  =  NULL;

	/* 11 */
	fn_tbl[11].rlfuncp  = (Function **)&history_inhibit_expansion_function;
	fn_tbl[11].defaultfn =  NULL;
	fn_tbl[11].wrapper   =  (Function *)history_inhibit_expansion_function_wrapper;
	fn_tbl[11].callback  =  NULL;

	/* 12 */
	fn_tbl[12].rlfuncp  =   &rl_pre_input_hook;
	fn_tbl[12].defaultfn = 	NULL;
	fn_tbl[12].wrapper   =  pre_input_hook_wrapper;
	fn_tbl[12].callback  =  NULL;

	/* 13 */
	fn_tbl[13].rlfuncp  = (Function **)&rl_completion_display_matches_hook;
	fn_tbl[13].defaultfn =  NULL;
	fn_tbl[13].wrapper   =  (Function *)completion_display_matches_hook_wrapper;
	fn_tbl[13].callback  =  NULL;

	/* 14 */
	fn_tbl[14].rlfuncp  = (Function **)&rl_completion_word_break_hook;
	fn_tbl[14].defaultfn = NULL;
	fn_tbl[14].wrapper   =  (Function *)completion_word_break_hook_wrapper;
	fn_tbl[14].callback  =  NULL;

	/* 15 */
	fn_tbl[15].rlfuncp  = (Function **)&rl_prep_term_function;
	fn_tbl[15].defaultfn =  (Function *)rl_prep_terminal;
	fn_tbl[15].wrapper   =  (Function *)prep_term_function_wrapper;
	fn_tbl[15].callback  =  NULL;

	/* 16 */
	fn_tbl[16].rlfuncp  = (Function **)&rl_deprep_term_function;
	fn_tbl[16].defaultfn =  (Function *)rl_deprep_terminal;
	fn_tbl[16].wrapper   =  (Function *)deprep_term_function_wrapper;
	fn_tbl[16].callback  =  NULL;

	/* 17 */
	fn_tbl[17].rlfuncp  = (Function **)&rl_directory_rewrite_hook;
	fn_tbl[17].defaultfn =  NULL;
	fn_tbl[17].wrapper   =  (Function *)directory_rewrite_hook_wrapper;
	fn_tbl[17].callback  =  NULL;
};


/*
 * Perl function wrappers
 */

/*
 * for rl_voidfunc_t : void fn(void)
 */
static int
voidfunc_wrapper(type)
     int type;
{
  dSP;
  int count;
  int ret;
  SV *svret;

  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  count = perl_call_sv(fn_tbl[type].callback, G_SCALAR);
  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:voidfunc_wrapper: Internal error\n");

  svret = POPs;
  ret = SvIOK(svret) ? SvIV(svret) : -1;
  PUTBACK;
  FREETMPS;
  LEAVE;
  return ret;
}

/*
 * for rl_vintfunc_t : void fn(int)
 */
static int
vintfunc_wrapper(type, arg)
     int type;
     int arg;
{
  dSP;
  int count;
  int ret;
  SV *svret;

  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  XPUSHs(sv_2mortal(newSViv(arg)));
  PUTBACK;
  count = perl_call_sv(fn_tbl[type].callback, G_SCALAR);
  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:vintfunc_wrapper: Internal error\n");

  svret = POPs;
  ret = SvIOK(svret) ? SvIV(svret) : -1;
  PUTBACK;
  FREETMPS;
  LEAVE;
  return ret;
}

/*
 * for rl_icppfunc_t : int fn(char **)
 */
static int
icppfunc_wrapper(type, arg)
     int type;
     char **arg;
{
  dSP;
  int count;
  SV *sv;
  int ret;
  char *rstr;
  
  ENTER;
  SAVETMPS;

  if (arg && *arg) {
    sv = sv_2mortal(newSVpv(*arg, 0));
  } else {
    sv = &PL_sv_undef;
  }

  PUSHMARK(sp);
  XPUSHs(sv);
  PUTBACK;

  count = perl_call_sv(fn_tbl[type].callback, G_SCALAR);

  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:icppfunc_wrapper: Internal error\n");

  ret = POPi;

  rstr = SvPV(sv, PL_na);
  if (strcmp(*arg, rstr) != 0) {
    xfree(*arg);
    *arg = dupstr(rstr);
  }

  PUTBACK;
  FREETMPS;
  LEAVE;

  return ret;
}

#if 0
/*
 * for rl_icpfunc_t : int fn(char *)
 */
static int
icpfunc_wrapper(type, text)
     int type;
     char *text;
{
  dSP;
  int count;
  int ret;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  if (text) {
    XPUSHs(sv_2mortal(newSVpv(text, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  PUTBACK;

  count = perl_call_sv(fn_tbl[type].callback, G_SCALAR);

  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:icpfunc_wrapper: Internal error\n");

  ret = POPi;			/* warns unless integer */
  PUTBACK;
  FREETMPS;
  LEAVE;
  return ret;
}
#endif

/*
 * for rl_cpvfunc_t : (char *)fn(void)
 */
static char *
cpvfunc_wrapper(type)
     int type;
{
  dSP;
  int count;
  char *str;
  SV *svret;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  count = perl_call_sv(fn_tbl[type].callback, G_SCALAR);
  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:cpvfunc_wrapper: Internal error\n");

  svret = POPs;
  str = SvOK(svret) ? dupstr(SvPV(svret, PL_na)) : NULL;
  PUTBACK;
  FREETMPS;
  LEAVE;
  return str;
}

/*
 * for rl_linebuf_func_t : int fn(char *, int)
 */
static int
icpintfunc_wrapper(type, text, index)
     int type;
     char *text;
     int index;
{
  dSP;
  int count;
  int ret;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  if (text) {
    XPUSHs(sv_2mortal(newSVpv(text, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  XPUSHs(sv_2mortal(newSViv(index)));
  PUTBACK;

  count = perl_call_sv(fn_tbl[type].callback, G_SCALAR);

  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:icpintfunc_wrapper: Internal error\n");

  ret = POPi;			/* warns unless integer */
  PUTBACK;
  FREETMPS;
  LEAVE;
  return ret;
}

static int
startup_hook_wrapper()		{ return voidfunc_wrapper(STARTUP_HOOK); }
static int
event_hook_wrapper()		{ return voidfunc_wrapper(EVENT_HOOK); }

static int
getc_function_wrapper(fp)
     PerlIO *fp;
{
  /*
   * 'PerlIO *fp' is ignored.  Use rl_instream instead in the getc_function.
   * How can I pass 'PerlIO *fp'?
   */
  return voidfunc_wrapper(GETC_FN);
}

static void
redisplay_function_wrapper()	{ voidfunc_wrapper(REDISPLAY_FN); }

/*
 * call a perl function as rl_completion_entry_function
 * for rl_compentry_func_t : (char *)fn(const char *, int)
 */

static char *
completion_entry_function_wrapper(text, state)
     const char *text;
     int state;
{
  dSP;
  int count;
  SV *match;
  char *str;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  if (text) {
    XPUSHs(sv_2mortal(newSVpv(text, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  XPUSHs(sv_2mortal(newSViv(state)));
  PUTBACK;

  count = perl_call_sv(fn_tbl[CMP_ENT].callback, G_SCALAR);

  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:completion_entry_function_wrapper: Internal error\n");

  match = POPs;
  str = SvOK(match) ? dupstr(SvPV(match, PL_na)) : NULL;

  PUTBACK;
  FREETMPS;
  LEAVE;
  return str;
}

/*
 * call a perl function as rl_attempted_completion_function
 * for rl_completion_func_t : (char **)fn(const char *, int, int)
 */

static char **
attempted_completion_function_wrapper(text, start, end)
     char *text;
     int start;
     int end;
{
  dSP;
  int count;
  char **matches;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  if (text) {
    XPUSHs(sv_2mortal(newSVpv(text, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  if (rl_line_buffer) {
    XPUSHs(sv_2mortal(newSVpv(rl_line_buffer, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  XPUSHs(sv_2mortal(newSViv(start)));
  XPUSHs(sv_2mortal(newSViv(end)));
  PUTBACK;

  count = perl_call_sv(fn_tbl[ATMPT_COMP].callback, G_ARRAY);

  SPAGAIN;

  /* cf. ignore_some_completions_function_wrapper() */
  if (count > 0) {
    int i;
    int dopack = -1;

    /*
     * The returned array may contain some undef items.
     * Pack the array in such case.
     */
    matches = (char **)xmalloc (sizeof(char *) * (count + 1));
    matches[count] = NULL;
    for (i = count - 1; i >= 0; i--) {
      SV *v = POPs;
      if (SvOK(v)) {
	matches[i] = dupstr(SvPV(v, PL_na));
      } else {
	matches[i] = NULL;
	if (i != 0)
	  dopack = i;		/* lowest index of hole */
      }
    }
    /* pack undef items */
    if (dopack > 0) {		/* don't pack matches[0] */
      int j = dopack;
      for (i = dopack; i < count; i++) {
	if (matches[i])
	  matches[j++] = matches[i];
      }
      matches[count = j] = NULL;
    }
    if (count == 2) {	/* only one match */
      xfree(matches[0]);
      matches[0] = matches[1];
      matches[1] = NULL;
    } else if (count == 1 && !matches[0]) { /* in case of a list of undef */
      xfree(matches);
      matches = NULL;
    }
  } else {
    matches = NULL;
  }

  PUTBACK;
  FREETMPS;
  LEAVE;

  return matches;
}

/*
 * call a perl function as rl_filename_quoting_function
 * for rl_quote_func_t : (char *)fn(char *, int, char *)
 */

static char *
filename_quoting_function_wrapper(text, match_type, quote_pointer)
     char *text;
     int match_type;
     char *quote_pointer;
{
  dSP;
  int count;
  SV *replacement;
  char *str;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  if (text) {
    XPUSHs(sv_2mortal(newSVpv(text, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  XPUSHs(sv_2mortal(newSViv(match_type)));
  if (quote_pointer) {
    XPUSHs(sv_2mortal(newSVpv(quote_pointer, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  PUTBACK;

  count = perl_call_sv(fn_tbl[FN_QUOTE].callback, G_SCALAR);

  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:filename_quoting_function_wrapper: Internal error\n");

  replacement = POPs;
  str = SvOK(replacement) ? dupstr(SvPV(replacement, PL_na)) : NULL;

  PUTBACK;
  FREETMPS;
  LEAVE;
  return str;
}

/*
 * call a perl function as rl_filename_dequoting_function
 * for rl_dequote_func_t : (char *)fn(char *, int)
 */

static char *
filename_dequoting_function_wrapper(text, quote_char)
     char *text;
     int quote_char;
{
  dSP;
  int count;
  SV *replacement;
  char *str;
  
  ENTER;
  SAVETMPS;

  PUSHMARK(sp);
  if (text) {
    XPUSHs(sv_2mortal(newSVpv(text, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  XPUSHs(sv_2mortal(newSViv(quote_char)));
  PUTBACK;

  count = perl_call_sv(fn_tbl[FN_DEQUOTE].callback, G_SCALAR);

  SPAGAIN;

  if (count != 1)
    croak("Gnu.xs:filename_dequoting_function_wrapper: Internal error\n");

  replacement = POPs;
  str = SvOK(replacement) ? dupstr(SvPV(replacement, PL_na)) : NULL;

  PUTBACK;
  FREETMPS;
  LEAVE;
  return str;
}

/*
 * call a perl function as rl_char_is_quoted_p
 */

static int
char_is_quoted_p_wrapper(text, index)
     char *text;
     int index;
{
  return icpintfunc_wrapper(CHAR_IS_QUOTEDP, text, index);
}

/*
 * call a perl function as rl_ignore_some_completions_function
 * for rl_compignore_func_t : int fn(char **)
 */

static void
ignore_some_completions_function_wrapper(matches)
     char **matches;
{
  dSP;
  int count, i, only_one_match;
  
  only_one_match = matches[1] == NULL ? 1 : 0;

  ENTER;
  SAVETMPS;

  PUSHMARK(sp);

  /* matches[0] is the maximal matching substring.  So it may NULL, even rest
   * of matches[] has values. */
  if (matches[0]) {
    XPUSHs(sv_2mortal(newSVpv(matches[0], 0)));
    /* xfree(matches[0]);*/
  } else {
    XPUSHs(&PL_sv_undef);
  }
  for (i = 1; matches[i]; i++) {
      XPUSHs(sv_2mortal(newSVpv(matches[i], 0)));
      xfree(matches[i]);
  }
  /*xfree(matches);*/
  PUTBACK;

  count = perl_call_sv(fn_tbl[IGNORE_COMP].callback, G_ARRAY);

  SPAGAIN;

  if (only_one_match) {
    if (count == 0) {		/* no match */
      xfree(matches[0]);
      matches[0] = NULL;
    } /* else only one match */
  } else if (count > 0) {
    int i;
    int dopack = -1;

    /*
     * The returned array may contain some undef items.
     * Pack the array in such case.
     */
    matches[count] = NULL;
    for (i = count - 1; i > 0; i--) { /* don't pop matches[0] */
      SV *v = POPs;
      if (SvOK(v)) {
	matches[i] = dupstr(SvPV(v, PL_na));
      } else {
	matches[i] = NULL;
	dopack = i;		/* lowest index of undef */
      }
    }
    /* pack undef items */
    if (dopack > 0) {		/* don't pack matches[0] */
      int j = dopack;
      for (i = dopack; i < count; i++) {
	if (matches[i])
	  matches[j++] = matches[i];
      }
      matches[count = j] = NULL;
    }
    if (count == 1) {		/* no match */
      xfree(matches[0]);
      matches[0] = NULL;
    } else if (count == 2) {	/* only one match */
      xfree(matches[0]);
      matches[0] = matches[1];
      matches[1] = NULL;
    }
  } else {			/* no match */
    xfree(matches[0]);
    matches[0] = NULL;
  }

  PUTBACK;
  FREETMPS;
  LEAVE;
}

/*
 * call a perl function as rl_directory_completion_hook
 */

static int
directory_completion_hook_wrapper(textp)
     char **textp;
{
  return icppfunc_wrapper(DIR_COMP, textp);
}

/*
 * call a perl function as history_inhibit_expansion_function
 */

static int
history_inhibit_expansion_function_wrapper(text, index)
     char *text;
     int index;
{
  return icpintfunc_wrapper(HIST_INHIBIT_EXP, text, index);
}

static int
pre_input_hook_wrapper() { return voidfunc_wrapper(PRE_INPUT_HOOK); }

#if (RL_VERSION_MAJOR >= 4)
/*
 * call a perl function as rl_completion_display_matches_hook
 * for rl_compdisp_func_t : void fn(char **, int, int)
 */

static void
completion_display_matches_hook_wrapper(matches, len, max)
     char **matches;
     int len;
     int max;
{
  dSP;
  int i;
  AV *av_matches;
  
  /* copy C matches[] array into perl array */
  av_matches = newAV();

  /* matches[0] is the maximal matching substring.  So it may NULL, even rest
   * of matches[] has values. */
  if (matches[0]) {
    av_push(av_matches, sv_2mortal(newSVpv(matches[0], 0)));
  } else {
    av_push(av_matches, &PL_sv_undef);
  }

  for (i = 1; matches[i]; i++)
    if (matches[i]) {
      av_push(av_matches, sv_2mortal(newSVpv(matches[i], 0)));
    } else {
      av_push(av_matches, &PL_sv_undef);
    }

  PUSHMARK(sp);
  XPUSHs(sv_2mortal(newRV_inc((SV *)av_matches))); /* push reference of array */
  XPUSHs(sv_2mortal(newSViv(len)));
  XPUSHs(sv_2mortal(newSViv(max)));
  PUTBACK;

  perl_call_sv(fn_tbl[COMP_DISP_HOOK].callback, G_DISCARD);
}
#else /* (RL_VERSION_MAJOR < 4) */
static void
completion_display_matches_hook_wrapper(matches, len, max)
     char **matches;
     int len;
     int max;
{
  /* dummy */
}
#endif /* (RL_VERSION_MAJOR < 4) */

static char *
completion_word_break_hook_wrapper()
{
  return cpvfunc_wrapper(COMP_WD_BRK_HOOK);
}

static int
prep_term_function_wrapper(meta_flag)
     int meta_flag;
{
  return vintfunc_wrapper(PREP_TERM, meta_flag);
}

static int
deprep_term_function_wrapper()	{ return voidfunc_wrapper(DEPREP_TERM); }

/*
 * call a perl function as rl_directory_completion_hook
 */
static int
directory_rewrite_hook_wrapper(dirname)
     char **dirname;
{
  return icppfunc_wrapper(DIR_REWRITE, dirname);
}

/*
 *	If you need more custom functions, define more funntion_wrapper_xx()
 *	and add entry on fntbl[].
 */

static int function_wrapper PARAMS((int count, int key, int id));

static int fw_00(c, k) int c; int k; { return function_wrapper(c, k,  0); }
static int fw_01(c, k) int c; int k; { return function_wrapper(c, k,  1); }
static int fw_02(c, k) int c; int k; { return function_wrapper(c, k,  2); }
static int fw_03(c, k) int c; int k; { return function_wrapper(c, k,  3); }
static int fw_04(c, k) int c; int k; { return function_wrapper(c, k,  4); }
static int fw_05(c, k) int c; int k; { return function_wrapper(c, k,  5); }
static int fw_06(c, k) int c; int k; { return function_wrapper(c, k,  6); }
static int fw_07(c, k) int c; int k; { return function_wrapper(c, k,  7); }
static int fw_08(c, k) int c; int k; { return function_wrapper(c, k,  8); }
static int fw_09(c, k) int c; int k; { return function_wrapper(c, k,  9); }
static int fw_10(c, k) int c; int k; { return function_wrapper(c, k, 10); }
static int fw_11(c, k) int c; int k; { return function_wrapper(c, k, 11); }
static int fw_12(c, k) int c; int k; { return function_wrapper(c, k, 12); }
static int fw_13(c, k) int c; int k; { return function_wrapper(c, k, 13); }
static int fw_14(c, k) int c; int k; { return function_wrapper(c, k, 14); }
static int fw_15(c, k) int c; int k; { return function_wrapper(c, k, 15); }

static struct fnnode {
  Function *wrapper;		/* C wrapper function */
  SV *pfn;			/* Perl function */
} fntbl[] = {
  { fw_00,	NULL },
  { fw_01,	NULL },
  { fw_02,	NULL },
  { fw_03,	NULL },
  { fw_04,	NULL },
  { fw_05,	NULL },
  { fw_06,	NULL },
  { fw_07,	NULL },
  { fw_08,	NULL },
  { fw_09,	NULL },
  { fw_10,	NULL },
  { fw_11,	NULL },
  { fw_12,	NULL },
  { fw_13,	NULL },
  { fw_14,	NULL },
  { fw_15,	NULL }
};

static int
function_wrapper(count, key, id)
     int count;
     int key;
     int id;
{
  dSP;

  PUSHMARK(sp);
  XPUSHs(sv_2mortal(newSViv(count)));
  XPUSHs(sv_2mortal(newSViv(key)));
  PUTBACK;

  perl_call_sv(fntbl[id].pfn, G_DISCARD);

  return 0;
}

static SV *callback_handler_callback = NULL;

static void
callback_handler_wrapper(line)
     char *line;
{
  dSP;

  PUSHMARK(sp);
  if (line) {
    XPUSHs(sv_2mortal(newSVpv(line, 0)));
  } else {
    XPUSHs(&PL_sv_undef);
  }
  PUTBACK;

  perl_call_sv(callback_handler_callback, G_DISCARD);
}

/*
 * make separate name space for low level XS functions and their methods
 */

MODULE = Term::ReadLine::Gnu		PACKAGE = Term::ReadLine::Gnu::XS

 ########################################################################
 #
 #	Gnu Readline Library
 #
 ########################################################################
 #
 #	2.1 Basic Behavior
 #

 # The function name "readline()" is reserved for a method name.

t_xstr
rl_readline(prompt = NULL)
	CONST char *	prompt
    PROTOTYPE: ;$
    CODE:
	RETVAL = readline(prompt);
    OUTPUT:
	RETVAL

 #
 #	2.4 Readline Convenience Functions
 #
 #
 #	2.4.1 Naming a Function
 #
rl_command_func_t *
rl_add_defun(name, fn, key = -1)
	CONST char *	name
	SV *		fn
	int key
    PROTOTYPE: $$;$
    CODE:
	{
	  int i;
	  int nentry = sizeof(fntbl)/sizeof(struct fnnode);

	  /* search an empty slot */
	  for (i = 0; i < nentry; i++)
	    if (! fntbl[i].pfn)
	      break;
	  
	  if (i >= nentry) {
	    warn("Gnu.xs:rl_add_defun: custom function table is full. The maximum number of custum function is %d.\n",
		 nentry);
	    XSRETURN_UNDEF;
	  }

	  fntbl[i].pfn = newSVsv(fn);
	  
	  /* rl_add_defun() always returns 0. */
	  rl_add_defun(dupstr(name), fntbl[i].wrapper, key);
	  RETVAL = fntbl[i].wrapper;
	}
    OUTPUT:
	RETVAL

 #
 #	2.4.2 Selection a Keymap
 #
Keymap
rl_make_bare_keymap()
    PROTOTYPE:
	  
Keymap
_rl_copy_keymap(map)
	Keymap map
    PROTOTYPE: $
    CODE:
	RETVAL = rl_copy_keymap(map);
    OUTPUT:
	RETVAL

Keymap
rl_make_keymap()
    PROTOTYPE:

Keymap
_rl_discard_keymap(map)
	Keymap map
    PROTOTYPE: $
    CODE:
	rl_discard_keymap(map);
	RETVAL = map;
    OUTPUT:
	RETVAL

Keymap
rl_get_keymap()
    PROTOTYPE:

Keymap
_rl_set_keymap(map)
	Keymap map
    PROTOTYPE: $
    CODE:
	rl_set_keymap(map);
	RETVAL = map;
    OUTPUT:
	RETVAL

Keymap
rl_get_keymap_by_name(name)
	CONST char *	name
    PROTOTYPE: $

 # Do not free the string returned.
char *
rl_get_keymap_name(map)
	Keymap map
    PROTOTYPE: $

 #
 #	2.4.3 Binding Keys
 #
int
_rl_bind_key(key, function, map = rl_get_keymap())
	int key
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_bind_key_in_map(key, function, map);
    OUTPUT:
	RETVAL

int
_rl_bind_key_if_unbound(key, function, map = rl_get_keymap())
	int key
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_bind_key_if_unbound_in_map(key, function, map);
    OUTPUT:
	RETVAL

int
_rl_unbind_key(key, map = rl_get_keymap())
	int key
	Keymap map
    PROTOTYPE: $;$
    CODE:
	RETVAL = rl_unbind_key_in_map(key, map);
    OUTPUT:
	RETVAL

 # rl_unbind_function_in_map() and rl_unbind_command_in_map() are introduced
 # by readline-2.2.

int
_rl_unbind_function(function, map = rl_get_keymap())
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $;$
    CODE:
	RETVAL = rl_unbind_function_in_map(function, map);
    OUTPUT:
	RETVAL

int
_rl_unbind_command(command, map = rl_get_keymap())
	CONST char *	command
	Keymap map
    PROTOTYPE: $;$
    CODE:
	RETVAL = rl_unbind_command_in_map(command, map);
    OUTPUT:
	RETVAL

int
_rl_bind_keyseq(keyseq, function, map = rl_get_keymap())
	const char *keyseq
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_bind_keyseq_in_map(keyseq, function, map);
    OUTPUT:
	RETVAL

 # rl_set_key() is introduced by readline-4.2 and equivalent with
 # rl_generic_bind(ISFUNC, keyseq, (char *)function, map).
int
_rl_set_key(keyseq, function, map = rl_get_keymap())
	CONST char *	keyseq
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $$;$
    CODE:
#if (RL_READLINE_VERSION >= 0x0402)
	RETVAL = rl_set_key(keyseq, function, map);
#else
	RETVAL = rl_generic_bind(ISFUNC, keyseq, (char *)function, map);
#endif
    OUTPUT:
	RETVAL

int
_rl_bind_keyseq_if_unbound(keyseq, function, map = rl_get_keymap())
	const char *keyseq
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_bind_keyseq_if_unbound_in_map(keyseq, function, map);
    OUTPUT:
	RETVAL

int
_rl_generic_bind_function(keyseq, function, map = rl_get_keymap())
	CONST char *	keyseq
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_generic_bind(ISFUNC, keyseq, (char *)function, map);
    OUTPUT:
	RETVAL

int
_rl_generic_bind_keymap(keyseq, keymap, map = rl_get_keymap())
	CONST char *	keyseq
	Keymap keymap
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_generic_bind(ISKMAP, keyseq, (char *)keymap, map);
    OUTPUT:
	RETVAL

int
_rl_generic_bind_macro(keyseq, macro, map = rl_get_keymap())
	CONST char *	keyseq
	CONST char *	macro
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_generic_bind(ISMACR, keyseq, dupstr(macro), map);
    OUTPUT:
	RETVAL

void
rl_parse_and_bind(line)
	char *	line
    PROTOTYPE: $
    CODE:
	{
	  char *s = dupstr(line);
	  rl_parse_and_bind(s); /* Some NULs may be inserted in "s". */
	  xfree(s);
	}

int
rl_read_init_file(filename = NULL)
	CONST char *	filename
    PROTOTYPE: ;$

 #
 #	2.4.4 Associating Function Names and Bindings
 #
int
_rl_call_function(function, count = 1, key = -1)
	rl_command_func_t *	function
	int count
	int key
    PROTOTYPE: $;$$
    CODE:
	RETVAL = (*function)(count, key);
    OUTPUT:
	RETVAL

rl_command_func_t *
rl_named_function(name)
	CONST char *	name
    PROTOTYPE: $

 # Do not free the string returned.
const char *
rl_get_function_name(function)
	rl_command_func_t *	function
    PROTOTYPE: $

void
rl_function_of_keyseq(keyseq, map = rl_get_keymap())
	CONST char *	keyseq
	Keymap map
    PROTOTYPE: $;$
    PPCODE:
	{
	  int type;
	  rl_command_func_t *p = rl_function_of_keyseq(keyseq, map, &type);
	  SV *sv;

	  if (p) {
	    sv = sv_newmortal();
	    switch (type) {
	    case ISFUNC:
	      sv_setref_pv(sv, "rl_command_func_tPtr", (void*)p);
	      break;
	    case ISKMAP:
	      sv_setref_pv(sv, "Keymap", (void*)p);
	      break;
	    case ISMACR:
	      if (p) {
		sv_setpv(sv, (char *)p);
	      }
	      break;
	    default:
	      warn("Gnu.xs:rl_function_of_keyseq: illegal type `%d'\n", type);
	      XSRETURN_EMPTY;	/* return NULL list */
	    }
	    EXTEND(sp, 2);
	    PUSHs(sv);
	    PUSHs(sv_2mortal(newSViv(type)));
	  } else
	    ;			/* return NULL list */
	}
	  
void
_rl_invoking_keyseqs(function, map = rl_get_keymap())
	rl_command_func_t *	function
	Keymap map
    PROTOTYPE: $;$
    PPCODE:
	{
	  char **keyseqs;
	  
	  keyseqs = rl_invoking_keyseqs_in_map(function, map);

	  if (keyseqs) {
	    int i, count;

	    /* count number of entries */
	    for (count = 0; keyseqs[count]; count++)
	      ;

	    EXTEND(sp, count);
	    for (i = 0; i < count; i++) {
	      PUSHs(sv_2mortal(newSVpv(keyseqs[i], 0)));
	      xfree(keyseqs[i]);
	    }
	    xfree((char *)keyseqs);
	  } else {
	    /* return null list */
	  }
	}

void
rl_function_dumper(readable = 0)
	int readable
    PROTOTYPE: ;$

void
rl_list_funmap_names()
    PROTOTYPE:

 # return list of all function name. (Term::Readline::Gnu specific function)
void
rl_get_all_function_names()
    PROTOTYPE:
    PPCODE:
	{
	  int i, count;
	  /* count number of entries */
	  for (count = 0; funmap[count]; count++)
	    ;
	  
	  EXTEND(sp, count);
	  for (i = 0; i < count; i++) {
	    PUSHs(sv_2mortal(newSVpv(funmap[i]->name, 0)));
	  }
	}

void
rl_funmap_names()
    PROTOTYPE:
    PPCODE:
	{
	  const char **funmap;

	  /* don't free returned memory */
	  funmap = (const char **)rl_funmap_names();/* cast is for oldies */

	  if (funmap) {
	    int i, count;

	    /* count number of entries */
	    for (count = 0; funmap[count]; count++)
	      ;

	    EXTEND(sp, count);
	    for (i = 0; i < count; i++) {
	      PUSHs(sv_2mortal(newSVpv(funmap[i], 0)));
	    }
	  } else {
	    /* return null list */
	  }
	}

int
_rl_add_funmap_entry(name, function)
	CONST char *		name
	rl_command_func_t *	function
    PROTOTYPE: $$
    CODE:
	RETVAL = rl_add_funmap_entry(name, function);
    OUTPUT:
	RETVAL

 #
 #	2.4.5 Allowing Undoing
 #
int
rl_begin_undo_group()
    PROTOTYPE:

int
rl_end_undo_group()
    PROTOTYPE:

void
rl_add_undo(what, start, end, text)
	int what
	int start
	int end
	char *	text
    PROTOTYPE: $$$$
    CODE:
	/* rl_free_undo_list will free the duplicated memory */
	rl_add_undo((enum undo_code)what, start, end, dupstr(text));

void
rl_free_undo_list()
    PROTOTYPE:

int
rl_do_undo()
    PROTOTYPE:

int
rl_modifying(start = 0, end = rl_end)
	int start
	int end
    PROTOTYPE: ;$$

 #
 #	2.4.6 Redisplay
 #
void
rl_redisplay()
    PROTOTYPE:

int
rl_forced_update_display()
    PROTOTYPE:

int
rl_on_new_line()
    PROTOTYPE:

int
rl_on_new_line_with_prompt()
    PROTOTYPE:

int
rl_reset_line_state()
    PROTOTYPE:

int
rl_show_char(i)
	int i
    PROTOTYPE: $

int
_rl_message(text)
	const char *	text
    PROTOTYPE: $
    CODE:
	RETVAL = rl_message(text);
    OUTPUT:
	RETVAL

int
rl_crlf()
    PROTOTYPE:

int
rl_clear_message()
    PROTOTYPE:

void
rl_save_prompt()
    PROTOTYPE:

void
rl_restore_prompt()
    PROTOTYPE:

int
rl_expand_prompt(prompt)
	# should be defined as 'const char *'
	char *		prompt

int
rl_set_prompt(prompt)
	const char *	prompt

 #
 #	2.4.7 Modifying Text
 #
int
rl_insert_text(text)
	CONST char *	text
    PROTOTYPE: $

int
rl_delete_text(start = 0, end = rl_end)
	int start
	int end
    PROTOTYPE: ;$$

t_xstr
rl_copy_text(start = 0, end = rl_end)
	int start
	int end
    PROTOTYPE: ;$$

int
rl_kill_text(start = 0, end = rl_end)
	int start
	int end
    PROTOTYPE: ;$$

 # rl_push_macro_input() is documented by readline-4.2 but it has been
 # implemented from 2.2.1.

void
rl_push_macro_input(macro)
	CONST char *	macro
    PROTOTYPE: $
    CODE:
	rl_push_macro_input(dupstr(macro));

 #
 #	2.4.8 Character Input
 #
int
rl_read_key()
    PROTOTYPE:

int
rl_getc(stream)
	FILE *	stream
    PROTOTYPE: $

int
rl_stuff_char(c)
	int c
    PROTOTYPE: $

int
rl_execute_next(c)
	int c
    PROTOTYPE: $

int
rl_clear_pending_input()
    PROTOTYPE:

int
rl_set_keyboard_input_timeout(usec)
	int usec
    PROTOTYPE: $

 #
 #	2.4.9 Terminal Management
 #

void
rl_prep_terminal(meta_flag)
	int meta_flag
    PROTOTYPE: $

void
rl_deprep_terminal()
    PROTOTYPE:

void
_rl_tty_set_default_bindings(kmap = rl_get_keymap())
	Keymap kmap
    PROTOTYPE: ;$
    CODE:
	rl_tty_set_default_bindings(kmap);

 # void
 # _rl_tty_unset_default_bindings(kmap = rl_get_keymap())
 #	Keymap kmap
 #    PROTOTYPE: ;$
 #    CODE:
 #	rl_tty_unset_default_bindings(kmap);

int
rl_reset_terminal(terminal_name = NULL)
	CONST char *	terminal_name
    PROTOTYPE: ;$

 #
 #	2.4.10 Utility Functions
 #
void
rl_replace_line(text, clear_undo = 0)
	const char *text
	int clear_undo
    PROTOTYPE: $$

int
rl_initialize()
    PROTOTYPE:

int
rl_ding()
    PROTOTYPE:

int
rl_alphabetic(c)
	int c
    PROTOTYPE: $

void
rl_display_match_list(pmatches, plen = -1, pmax = -1)
	SV *	pmatches
	int plen
	int pmax
    PROTOTYPE: $;$$
    CODE:
	{
	  unsigned int len, max, i;
	  STRLEN l;
	  char **matches;
	  AV *av_matches;
	  SV **pvp;

	  if (SvTYPE(SvRV(pmatches)) != SVt_PVAV) {
	    warn("Gnu.xs:_rl_display_match_list: the 1st arguments must be a reference of an array\n");
	    return;
	  }
	  av_matches = (AV *)SvRV(ST(0));
	  /* index zero contains possible match and is ignored */
	  if ((len = av_len(av_matches) + 1 - 1) == 0)
	    return;
	  matches = (char **)xmalloc (sizeof(char *) * (len + 2));
	  max = 0;
	  for (i = 1; i <= len; i++) {
	    pvp = av_fetch(av_matches, i, 0);
	    if (SvPOKp(*pvp)) {
	      matches[i] = dupstr(SvPV(*pvp, l));
	      if (l > max)
		max = l;
	    }
	  }
	  matches[len + 1] = NULL;

	  rl_display_match_list(matches,
				plen < 0 ? len : plen,
				pmax < 0 ? max : pmax);

	  for (i = 1; i <= len; i++)
	    xfree(matches[i]);
	  xfree(matches);
	}

 #
 #	2.4.11 Miscellaneous Functions
 #

 # rl_macro_bind() is documented by readline-4.2 but it has been implemented 
 # from 2.2.1.
 # It is equivalent with 
 # rl_generic_bind(ISMACR, keyseq, (char *)macro_keys, map).
int
_rl_macro_bind(keyseq, macro, map = rl_get_keymap())
	CONST char *	keyseq
	CONST char *	macro
	Keymap map
    PROTOTYPE: $$;$
    CODE:
	RETVAL = rl_macro_bind(keyseq, macro, map);
    OUTPUT:
	RETVAL

 # rl_macro_dumper is documented by Readline 4.2,
 # but have been implemented for 2.2.1.

void
rl_macro_dumper(readable = 0)
	int readable
    PROTOTYPE: ;$

 # rl_variable_bind() is documented by readline-4.2 but it has been implemented
 # from 2.2.1.

int
rl_variable_bind(name, value)
	CONST char *	name
	CONST char *	value
    PROTOTYPE: $$

 # rl_variable_dumper is documented by Readline 4.2,
 # but have been implemented for 2.2.1.

 # Do not free the string returned.
char *
rl_variable_value(variable)
	CONST char *	variable
    PROTOTYPE: $

void
rl_variable_dumper(readable = 0)
	int readable
    PROTOTYPE: ;$

int
rl_set_paren_blink_timeout(usec)
	int usec
    PROTOTYPE: $

 # rl_get_termcap() is documented by readline-4.2 but it has been implemented 
 # from 2.2.1.

 # Do not free the string returned.
 # char *
 # rl_get_termcap(cap)
 #   CONST char *	cap
 #   PROTOTYPE: $
 #
 #
 #	2.4.12 Alternate Interface
 #

void
rl_callback_handler_install(prompt, lhandler)
	const char *	prompt
	SV *		lhandler
    PROTOTYPE: $$
    CODE:
	{
	  static char *cb_prompt = NULL;
	  int len = strlen(prompt) + 1;

	  /* The value of prompt may be used after return from this routine. */
	  if (cb_prompt) {
	    Safefree(cb_prompt);
	  }
	  New(0, cb_prompt, len, char);
	  Copy(prompt, cb_prompt, len, char);

	  /*
	   * Don't remove braces. The definition of SvSetSV() of
	   * Perl 5.003 has a problem.
	   */
	  if (callback_handler_callback) {
	    SvSetSV(callback_handler_callback, lhandler);
	  } else {
	    callback_handler_callback = newSVsv(lhandler);
	  }

	  rl_callback_handler_install(cb_prompt, callback_handler_wrapper);
	}

void
rl_callback_read_char()
    PROTOTYPE:

void
rl_callback_handler_remove()
    PROTOTYPE:

 #
 #	2.5 Readline Signal Handling
 #

void
rl_cleanup_after_signal()
    PROTOTYPE:

void
rl_free_line_state()
    PROTOTYPE:

void
rl_reset_after_signal()
    PROTOTYPE:

void
rl_echo_signal_char(sig)
	int sig
    PROTOTYPE: $

void
rl_resize_terminal()
    PROTOTYPE:

void
rl_set_screen_size(rows, cols)
	int rows
	int cols
    PROTOTYPE: $$

void
rl_get_screen_size()
    PROTOTYPE:
    PPCODE:
	{
	  int rows, cols;
	  rl_get_screen_size(&rows, &cols);
	  EXTEND(sp, 2);
	  PUSHs(sv_2mortal(newSViv(rows)));
	  PUSHs(sv_2mortal(newSViv(cols)));
	}

void
rl_reset_screen_size()
    PROTOTYPE:

int
rl_set_signals()
    PROTOTYPE:

int
rl_clear_signals()
    PROTOTYPE:

 #
 #	2.6 Custom Completers
 #

int
rl_complete_internal(what_to_do = TAB)
	int what_to_do
    PROTOTYPE: ;$

int
_rl_completion_mode(function)
	rl_command_func_t *	function
    PROTOTYPE: $
    CODE:
	RETVAL = rl_completion_mode(function);
    OUTPUT:
	RETVAL

void
rl_completion_matches(text, fn = NULL)
	CONST char *	text
	SV *		fn
    PROTOTYPE: $;$
    PPCODE:
	{
	  char **matches;

	  if (SvTRUE(fn)) {
	    /* use completion_entry_function temporarily */
	    Function *rlfunc_save = *(fn_tbl[CMP_ENT].rlfuncp);
	    SV *callback_save = fn_tbl[CMP_ENT].callback;
	    fn_tbl[CMP_ENT].callback = newSVsv(fn);

	    matches = rl_completion_matches(text,
					    completion_entry_function_wrapper);

	    SvREFCNT_dec(fn_tbl[CMP_ENT].callback);
	    fn_tbl[CMP_ENT].callback = callback_save;
	    *(fn_tbl[CMP_ENT].rlfuncp) = rlfunc_save;
	  } else
	    matches = rl_completion_matches(text, NULL);

	  /*
	   * Without the next line the Perl internal stack is broken
	   * under some condition.  Perl bug or undocumented feature
	   * !!!?
	   */
	  SPAGAIN; sp -= 2;
	  
	  if (matches) {
	    int i, count;

	    /* count number of entries */
	    for (count = 0; matches[count]; count++)
	      ;

	    EXTEND(sp, count);
	    for (i = 0; i < count; i++) {
	      PUSHs(sv_2mortal(newSVpv(matches[i], 0)));
	      xfree(matches[i]);
	    }
	    xfree((char *)matches);
	  } else {
	    /* return null list */
	  }
	}

t_xstr
rl_filename_completion_function(text, state)
	const char *	text
	int state
    PROTOTYPE: $$

t_xstr
rl_username_completion_function(text, state)
	const char *	text
	int state
    PROTOTYPE: $$


 ########################################################################
 #
 #	Gnu History Library
 #
 ########################################################################

 #
 #	2.3.1 Initializing History and State Management
 #
void
using_history()
    PROTOTYPE:

 #  history_get_history_state() and history_set_history_state() are useless
 #  and too dangerous to be used in Perl code
 # void
 # history_get_history_state()
 #     PROTOTYPE:
 #     PPCODE:
 # 	{
 # 	  HISTORY_STATE *state;
 #
 # 	  state = history_get_history_state();
 # 	  EXTEND(sp, 4);
 # 	  PUSHs(sv_2mortal(newSViv(state->offset)));
 # 	  PUSHs(sv_2mortal(newSViv(state->length)));
 # 	  PUSHs(sv_2mortal(newSViv(state->size)));
 # 	  PUSHs(sv_2mortal(newSViv(state->flags)));
 # 	  xfree((char *)state);
 # 	}

 #
 #	2.3.2 History List Management
 #

void
add_history(string)
	CONST char *	string
    PROTOTYPE: $

void
add_history_time(string)
	CONST char *	string
    PROTOTYPE: $

HIST_ENTRY *
remove_history(which)
	int which
    PROTOTYPE: $
    OUTPUT:
	RETVAL
    CLEANUP:
	if (RETVAL) {
	  xfree(RETVAL->line);
#if (RL_VERSION_MAJOR >= 5)
	  xfree(RETVAL->timestamp);
#endif /* (RL_VERSION_MAJOR >= 5) */
	  xfree(RETVAL->data);
	  xfree((char *)RETVAL);
	}

 # free_history_entry() is introduced by GNU Readline Library 5.0.
 # Since Term::ReadLine::Gnu does not support the member 'data' of HIST_ENTRY
 # structure, remove_history() covers it.

 # The 3rd parameter (histdata_t) is not supported. Does anyone use it?
HIST_ENTRY *
replace_history_entry(which, line)
	int which
	CONST char *	line
    PROTOTYPE: $$
    CODE:
	RETVAL = replace_history_entry(which, line, (char *)NULL);
    OUTPUT:
	RETVAL
    CLEANUP:
	if (RETVAL) {
	  xfree(RETVAL->line);
#if (RL_VERSION_MAJOR >= 5)
	  xfree(RETVAL->timestamp);
#endif /* (RL_VERSION_MAJOR >= 5) */
	  xfree(RETVAL->data);
	  xfree((char *)RETVAL);
	}

void
clear_history()
    PROTOTYPE:

int
stifle_history(i)
	SV *	i
    PROTOTYPE: $
    CODE:
	{
	  if (SvOK(i)) {
	    int max = SvIV(i);
	    stifle_history(max);
	    RETVAL = max;
	  } else {
	    RETVAL = unstifle_history();
	  }
	}
    OUTPUT:
	RETVAL

int
unstifle_history()
    PROTOTYPE:

int
history_is_stifled()
    PROTOTYPE:

 #
 #	2.3.3 Information about the History List
 #

 # history_list() is implemented as a perl function in Gnu.pm.

int
where_history()
    PROTOTYPE:

HIST_ENTRY *
current_history()
    PROTOTYPE:

HIST_ENTRY *
history_get(offset)
	int offset
    PROTOTYPE: $

 # To keep compatibility, I cannot make a function whose argument
 # is "HIST_ENTRY *".
time_t
history_get_time(offset)
	int offset
    PROTOTYPE: $
    CODE:
	{
	  HIST_ENTRY *he = history_get(offset);
	  if (he)
	    RETVAL = history_get_time(he);
	  else
	    RETVAL = 0;
	}
    OUTPUT:
	RETVAL

int
history_total_bytes()
    PROTOTYPE:

 #
 #	2.3.4 Moving Around the History List
 #
int
history_set_pos(pos)
	int pos
    PROTOTYPE: $

HIST_ENTRY *
previous_history()
    PROTOTYPE:

HIST_ENTRY *
next_history()
    PROTOTYPE:

 #
 #	2.3.5 Searching the History List
 #
int
history_search(string, direction = -1)
	CONST char *	string
	int direction
    PROTOTYPE: $;$

int
history_search_prefix(string, direction = -1)
	CONST char *	string
	int direction
    PROTOTYPE: $;$

int
history_search_pos(string, direction = -1, pos = where_history())
	CONST char *	string
	int direction
	int pos
    PROTOTYPE: $;$$

 #
 #	2.3.6 Managing the History File
 #
int
read_history_range(filename = NULL, from = 0, to = -1)
	CONST char *	filename
	int from
	int to
    PROTOTYPE: ;$$$

int
write_history(filename = NULL)
	CONST char *	filename
    PROTOTYPE: ;$

int
append_history(nelements, filename = NULL)
	int nelements
	CONST char *	filename
    PROTOTYPE: $;$

int
history_truncate_file(filename = NULL, nlines = 0)
	CONST char *	filename
	int nlines
    PROTOTYPE: ;$$

 #
 #	2.3.7 History Expansion
 #
void
history_expand(line)
	# should be defined as 'const char *'
	char *	line
    PROTOTYPE: $
    PPCODE:
	{
	  char *expansion;
	  int result;

	  result = history_expand(line, &expansion);
	  EXTEND(sp, 2);
	  PUSHs(sv_2mortal(newSViv(result)));
	  PUSHs(sv_2mortal(newSVpv(expansion, 0)));
	  xfree(expansion);
	}

void
_get_history_event(string, cindex, qchar = 0)
	CONST char *	string
	int cindex
	int qchar
    PROTOTYPE: $$;$
    PPCODE:
	{
	  char *text;

	  text = get_history_event(string, &cindex, qchar);
	  EXTEND(sp, 2);
	  if (text) {		/* don't free `text' */
	    PUSHs(sv_2mortal(newSVpv(text, 0)));
	  } else {
	    PUSHs(&PL_sv_undef);
	  }
	  PUSHs(sv_2mortal(newSViv(cindex)));
	}

void
history_tokenize(text)
	CONST char *	text
    PROTOTYPE: $
    PPCODE:
	{
	  char **tokens;

	  tokens = history_tokenize(text);
	  if (tokens) {
	    int i, count;

	    /* count number of entries */
	    for (count = 0; tokens[count]; count++)
	      ;

	    EXTEND(sp, count);
	    for (i = 0; i < count; i++) {
	      PUSHs(sv_2mortal(newSVpv(tokens[i], 0)));
	      xfree(tokens[i]);
	    }
	    xfree((char *)tokens);
	  } else {
	    /* return null list */
	  }
	}

#define DALLAR '$'		/* define for xsubpp bug */

t_xstr
_history_arg_extract(line, first = 0 , last = DALLAR)
	CONST char *	line
	int first
	int last
    PROTOTYPE: $;$$
    CODE:
	RETVAL = history_arg_extract(first, last, line);
    OUTPUT:
	RETVAL


 #
 #	GNU Readline/History Library Variable Access Routines
 #

MODULE = Term::ReadLine::Gnu		PACKAGE = Term::ReadLine::Gnu::Var

void
_rl_store_str(pstr, id)
	const char *	pstr
	int id
    PROTOTYPE: $$
    CODE:
	{
	  size_t len;

	  ST(0) = sv_newmortal();
	  if (id < 0 || id >= sizeof(str_tbl)/sizeof(struct str_vars)) {
	    warn("Gnu.xs:_rl_store_str: Illegal `id' value: `%d'", id);
	    XSRETURN_UNDEF;
	  }

	  if (str_tbl[id].read_only) {
	    warn("Gnu.xs:_rl_store_str: store to read only variable");
	    XSRETURN_UNDEF;
	  }

	  /*
	   * Use xmalloc() and xfree() instead of New() and Safefree(),
	   * because this block may be reallocated by the GNU Readline Library.
	   */
	  if (str_tbl[id].accessed && *str_tbl[id].var) {
	    /*
	     * First time a variable is used by this routine,
	     * it may be a static area.  So it cannot be freed.
	     */
	    xfree(*str_tbl[id].var);
	    *str_tbl[id].var = NULL;
	  }
	  str_tbl[id].accessed = 1;

	  len = strlen(pstr) + 1;
	  *str_tbl[id].var = xmalloc(len);
	  Copy(pstr, *str_tbl[id].var, len, char);

	  /* return variable value */
	  if (*str_tbl[id].var) {
	    sv_setpv(ST(0), *str_tbl[id].var);
	  }
	}

void
_rl_store_rl_line_buffer(pstr)
	const char *	pstr
    PROTOTYPE: $
    CODE:
	{
	  size_t len;

	  ST(0) = sv_newmortal();
	  if (pstr) {
	    len = strlen(pstr);

	    /*
	     * Old manual did not document this function, but can be
	     * used.
	     */
	    rl_extend_line_buffer(len + 1);

	    Copy(pstr, rl_line_buffer, len + 1, char);
	    /* rl_line_buffer is not NULL here */
	    sv_setpv(ST(0), rl_line_buffer);

	    /* fix rl_end and rl_point */
	    rl_end = len;
	    if (rl_point > len)
		    rl_point = len;
	  }
	}

void
_rl_fetch_str(id)
	int id
    PROTOTYPE: $
    CODE:
	{
	  ST(0) = sv_newmortal();
	  if (id < 0 || id >= sizeof(str_tbl)/sizeof(struct str_vars)) {
	    warn("Gnu.xs:_rl_fetch_str: Illegal `id' value: `%d'", id);
	  } else {
	    if (*(str_tbl[id].var)) {
	      sv_setpv(ST(0), *(str_tbl[id].var));
	    }
	  }
	}

void
_rl_store_int(pint, id)
	int pint
	int id
    PROTOTYPE: $$
    CODE:
	{
	  ST(0) = sv_newmortal();
	  if (id < 0 || id >= sizeof(int_tbl)/sizeof(struct int_vars)) {
	    warn("Gnu.xs:_rl_store_int: Illegal `id' value: `%d'", id);
	    XSRETURN_UNDEF;
	  }

	  if (int_tbl[id].read_only) {
	    warn("Gnu.xs:_rl_store_int: store to read only variable");
	    XSRETURN_UNDEF;
	  }

	  /* set C variable */
	  if (int_tbl[id].charp)
	    *((char *)(int_tbl[id].var)) = (char)pint;
	  else
	    *(int_tbl[id].var) = pint;

	  /* return variable value */
	  sv_setiv(ST(0), pint);
	}

void
_rl_fetch_int(id)
	int id
    PROTOTYPE: $
    CODE:
	{
	  ST(0) = sv_newmortal();
	  if (id < 0 || id >= sizeof(int_tbl)/sizeof(struct int_vars)) {
	    warn("Gnu.xs:_rl_fetch_int: Illegal `id' value: `%d'", id);
	    /* return undef */
	  } else {
	    sv_setiv(ST(0),
		     int_tbl[id].charp ? (int)*((char *)(int_tbl[id].var))
		     : *(int_tbl[id].var));
	  }
	}

PerlIO *
_rl_store_iostream(stream, id)
	PerlIO *stream
	int id
    PROTOTYPE: $$
    CODE:
	{
	  switch (id) {
	  case 0:
#if 0	  /* PerlIO_releaseFILE must be called only before closing FILE *. */
	    if (instreamPIO != NULL)
	      PerlIO_releaseFILE(instreamPIO, rl_instream);
#endif
	    rl_instream = PerlIO_findFILE(stream);
	    RETVAL = instreamPIO = stream;
	    break;
	  case 1:
#if 0	  /* PerlIO_releaseFILE must be called only before closing FILE *. */
	    if (outstreamPIO != NULL)
	      PerlIO_releaseFILE(outstreamPIO, rl_outstream);
#endif
	    rl_outstream = PerlIO_findFILE(stream);
	    RETVAL = outstreamPIO = stream;
#ifdef __CYGWIN__
	    {
	      /* Cygwin b20.1 library converts NL to CR-NL
		 automatically.  But it does not do it on a file
		 stream made by Perl.  Set terminal attribute
		 explicitly */
		struct termios tio;
		tcgetattr(fileno(rl_outstream), &tio);
		tio.c_iflag |= ICRNL;
		tio.c_oflag |= ONLCR;
		tcsetattr(fileno(rl_outstream), TCSADRAIN, &tio);
	    }
#endif /* __CYGWIN__ */
	    break;
	  default:
	    warn("Gnu.xs:_rl_store_iostream: Illegal `id' value: `%d'", id);
	    XSRETURN_UNDEF;
	    break;
	  }
	  PerlIO_debug("TRG:store_iostream id %d fd %d\n",
		       id, PerlIO_fileno(RETVAL));
	}
    OUTPUT:
	RETVAL

PerlIO *
_rl_fetch_iostream(id)
	int id
    PROTOTYPE: $
    CODE:
	{
	  switch (id) {
	  case 0:
	    if (instreamPIO == NULL)
	      RETVAL = instreamPIO = PerlIO_importFILE(rl_instream, NULL);
	    else
	      RETVAL = instreamPIO;
	    break;
	  case 1:
	    if (outstreamPIO == NULL)
	      RETVAL = outstreamPIO = PerlIO_importFILE(rl_outstream, NULL);
	    else
	      RETVAL = outstreamPIO;
	    break;
	  default:
	    warn("Gnu.xs:_rl_fetch_iostream: Illegal `id' value: `%d'", id);
	    XSRETURN_UNDEF;
	    break;
	  }
	  PerlIO_debug("TRG:fetch_iostream id %d fd %d\n", 
		       id, PerlIO_fileno(RETVAL));
	}
    OUTPUT:
	RETVAL

Keymap
_rl_fetch_keymap(id)
	int id
    PROTOTYPE: $
    CODE:
	{
	  switch (id) {
	  case 0:
	    RETVAL = rl_executing_keymap;
	    break;
	  case 1:
	    RETVAL = rl_binding_keymap;
	    break;
	  default:
	    warn("Gnu.xs:_rl_fetch_keymap: Illegal `id' value: `%d'", id);
	    XSRETURN_UNDEF;
	    break;
	  }
	}
    OUTPUT:
	RETVAL

void
_rl_store_function(fn, id)
	SV *	fn
	int id
    PROTOTYPE: $$
    CODE:
	{
	  /*
	   * If "fn" is undef, default value of the GNU Readline
	   * Library is set.
	   */
	  ST(0) = sv_newmortal();
	  if (id < 0 || id >= sizeof(fn_tbl)/sizeof(struct fn_vars)) {
	    warn("Gnu.xs:_rl_store_function: Illegal `id' value: `%d'", id);
	    XSRETURN_UNDEF;
	  }
	  
	  if (SvTRUE(fn)) {
	    /*
	     * Don't remove braces. The definition of SvSetSV() of
	     * Perl 5.003 has a problem.
	     */
	    if (fn_tbl[id].callback) {
	      SvSetSV(fn_tbl[id].callback, fn);
	    } else {
	      fn_tbl[id].callback = newSVsv(fn);
	    }
	    *(fn_tbl[id].rlfuncp) = fn_tbl[id].wrapper;
	  } else {
	    if (fn_tbl[id].callback) {
	      SvSetSV(fn_tbl[id].callback, &PL_sv_undef);
	    }
	    *(fn_tbl[id].rlfuncp) = fn_tbl[id].defaultfn;
	  }

	  /* return variable value */
	  sv_setsv(ST(0), fn);
	}

void
_rl_fetch_function(id)
	int id
    PROTOTYPE: $
    CODE:
	{
	  ST(0) = sv_newmortal();
	  if (id < 0 || id >= sizeof(fn_tbl)/sizeof(struct fn_vars)) {
	    warn("Gnu.xs:_rl_fetch_function: Illegal `id' value: `%d'", id);
	    /* return undef */
	  } else if (fn_tbl[id].callback && SvTRUE(fn_tbl[id].callback)) {
	    sv_setsv(ST(0), fn_tbl[id].callback);
	  }
	}

Function *
_rl_fetch_last_func()
    PROTOTYPE:
    CODE:
	RETVAL = rl_last_func;
    OUTPUT:
	RETVAL

MODULE = Term::ReadLine::Gnu		PACKAGE = Term::ReadLine::Gnu::XS

void
tgetstr(id)
	const char *	id
    PROTOTYPE: $
    CODE:
	ST(0) = sv_newmortal();
	if (id) {
	  /*
	   * The magic number `2032' is derived from bash
	   * terminal.c:_rl_init_terminal_io().
	   */
	  char buffer[2032];
	  char *bp = buffer;
	  char *t;
	  t = tgetstr(id, &bp); /* don't free returned string */
	  if (t) {
	    char buf[2032];
	    /* call tputs() to apply padding information */
	    tputs_ptr = buf;
	    tputs(t, 1, tputs_char);
	    *tputs_ptr = '\0';
	    sv_setpv(ST(0), buf);
	  }
	}

 #
 # Local Variables:
 # c-default-style: "gnu"
 # End:
 #
