#include <stdio.h>
#include <stdlib.h>
#ifdef bool
#undef bool
#endif
#ifdef CURSES
#include <curses.h>
#endif
#ifdef NCURSES
#include <ncurses.h>
#endif
#include <string.h>
#include "pdl.h"

#define CHBUF  256
#ifndef MIN
#define MIN(a,b) ((a)<(b)?(a):(b))
#endif

/* enum pdl_datatypes { PDL_B, PDL_S, PDL_US, PDL_L, PDL_F, PDL_D }; */
#define HLAB   4

static int colwid, dcols, drows;
char *format[] = {
  "%3d", "%6d", "%6hd", "%11ld", "%10.4g", "%11.4lg" };
int  width[] = {
  4,7,7,12,11,12};

char *str_value(int x, int y,
		int type, int nx, void *data, char *str)
{
  switch (type) {
  case PDL_B:
    sprintf(str,format[type],*(((char *)data)+y*nx+x));
    break;
  case PDL_S:
    sprintf(str,format[type],*(((short *)data)+y*nx+x));
    break;
  case PDL_US:
    sprintf(str,format[type],*(((unsigned short *)data)+y*nx+x));
    break;
  case PDL_L:
    sprintf(str,format[type],*(((int *)data)+y*nx+x));
    break;
  case PDL_F:
    sprintf(str,format[type],*(((float *)data)+y*nx+x));
    break;
  case PDL_D:
    sprintf(str,format[type],*(((double *)data)+y*nx+x));
    break;
  default:
    croak("type (val=%d) not implemented",type);
    break;
  }
  return str;
}

void set_value(int x, int y,
		int type, int nx, void *data, char *str)
{
  switch (type) {
  case PDL_B:
    *(((PDL_Byte *)data)+y*nx+x) = atol(str);
    break;
  case PDL_S:
    *(((PDL_Short *)data)+y*nx+x) = atol(str);
    break;
  case PDL_US:
    *(((PDL_Ushort *)data)+y*nx+x) = atol(str);
    break;
  case PDL_L:
    *(((PDL_Long *)data)+y*nx+x) = atol(str);
    break;
  case PDL_F:
    *(((PDL_Float *)data)+y*nx+x) = atof(str);
    break;
  case PDL_D:
    *(((PDL_Double *)data)+y*nx+x) = atof(str);
    break;
  default:
    croak("type (val=%d) not implemented",type);
    break;
  }
  return;
}

void update_vlab(WINDOW *win, int x, int ioff)
{
  char line[BUFSIZ];
  int len, k, d;
  chtype chline[BUFSIZ];
  extern int colwid;

  for (k=0;k<colwid;k++)
    chline[k] = ' ';

  sprintf(line,"%d",ioff+x);
  len = strlen(line);
  d = (colwid-len)/2;
  for (k=0;k<len;k++)
    chline[k+d] = line[k] | A_BOLD;
  chline[colwid-1] = '|' | A_BOLD;
  chline[colwid] = 0;

  mvwaddchnstr(win,0,x*colwid,chline,colwid);
}
void update_hlab(WINDOW *win, int y, int joff)
{
  char line[BUFSIZ];
  int len, k, d;
  chtype chline[BUFSIZ];

  for (k=0;k<HLAB;k++)
    chline[k] = ' ';

  sprintf(line,"%-4d",joff+y);
  len = strlen(line);
  d = (HLAB-len)/2;

  for (k=0;k<len;k++)
    chline[k+d] = line[k] | A_BOLD;
  chline[HLAB] = 0;
  mvwaddchnstr(win,y,0,chline,HLAB);
}
void clear_cell(WINDOW *win, int i, int j)
{
  char line[BUFSIZ];
  int len, k, d;
  chtype chline[BUFSIZ];
  extern int colwid;

  for (k=0;k<colwid-1;k++)
    chline[k] = ' ';
  chline[colwid-1] = '|' | A_BOLD;
  chline[colwid] = '\0';
  mvwaddchnstr(win,j,i*colwid,chline,colwid);
}
void set_cell(WINDOW *win, int i, int j, int ioff, int joff,
	      int type, int nx, void *data)
{
  char line[BUFSIZ];
  int len, k, d;
  chtype chline[BUFSIZ];
  extern int colwid;

  for (k=0;k<colwid-1;k++)
    chline[k] = ' ';
  str_value(i,j,type,nx,data,line);
  len = strlen(line);
  for (k=0;k<len;k++)
    chline[k] = line[k];
  chline[len] = ' ';
  chline[colwid-1] = '|' | A_BOLD;
  chline[colwid] = '\0';
  mvwaddchnstr(win,j-joff,(i-ioff)*colwid,chline,colwid);
}

void update_row(WINDOW *win, int y, int ioff, int joff,
		int type, int nx, void *data)
{
  char line[BUFSIZ];
  int len, k, d, i;
  chtype chline[BUFSIZ];
  extern int colwid, dcols;

  for (i=0;i<dcols;i++) {
    for (k=0;k<colwid-1;k++)
      chline[k] = ' ';
    str_value(i+ioff,y+joff,type,nx,data,line);
    len = strlen(line);
    for (k=0;k<len;k++)
      chline[k] = line[k];
    chline[len] = ' ';
    chline[colwid-1] = '|' | A_BOLD;
    chline[colwid] = '\0';
    mvwaddchnstr(win,y,i*colwid,chline,colwid);
  }
}
void update_col(WINDOW *win, int x, int ioff, int joff,
		int type, int nx, void *data)
{
  char line[BUFSIZ];
  int len, k, d, j;
  chtype chline[BUFSIZ];
  extern int colwid, drows;

  for (j=0;j<drows;j++) {
    for (k=0;k<colwid-1;k++)
      chline[k] = ' ';
    str_value(x+ioff,j+joff,type,nx,data,line);
    len = strlen(line);
    for (k=0;k<len;k++)
      chline[k] = line[k];
    chline[len] = ' ';
    chline[colwid-1] = '|' | A_BOLD;
    chline[colwid] = '\0';
    mvwaddchnstr(win,j,x*colwid,chline,colwid);
  }
}



void browse(int type, int nc, int nr, void *in)
{
  WINDOW *stdscr, *wmenu, *wscroll, *warray, *whlab, *wvlab, *wtmp;
  char s[CHBUF],echobuf[CHBUF],line[CHBUF];
  chtype ch;
  int i,j,eps,ioff,joff,iecho;
  int ncols, nrows, mycols;
  extern int colwid, dcols, drows, width[];

  stdscr = initscr();  /* sets LINES, COLS (which aren't macro constants...) */

  colwid = width[type];
  ncols = (COLS-HLAB)/colwid;
  dcols = MIN(nc,ncols);
  mycols = dcols*colwid;

  nrows = LINES-3;
  drows = MIN(nr,nrows);

  cbreak();
  noecho();
  nonl();
  intrflush(stdscr,FALSE);
  keypad(stdscr,TRUE);
  /* Menu bar */
  wmenu  = subwin(stdscr,1,COLS,0,0);
  wvlab  = subwin(stdscr,1,mycols,1,HLAB);
  wscroll= subwin(stdscr,drows,mycols+HLAB,2,0);
  warray = subwin(wscroll,drows,mycols,2,HLAB);
  whlab  = subwin(wscroll,drows,HLAB,2,0);

  keypad(warray,TRUE);
  scrollok(stdscr,TRUE);
  scrollok(wscroll,TRUE);

  wmenu  = subwin(stdscr,1,COLS,0,0);

  sprintf(s,"Perldl data browser: type %d, (%d,%d), type q to quit\n",
	  type,nr,nc);
  mvwaddstr(wmenu,0,10,s);
  wrefresh(wmenu);

  for (i=0;i<dcols;i++) {
    update_vlab(wvlab,i,0);
  }
  wrefresh(wvlab);

  for (j=0;j<drows;j++) {
    update_hlab(whlab,j,0);
  }
  wrefresh(whlab);

  for (j=0;j<drows;j++) {
    update_row(warray,j,0,0,type,nc,in);
  }

  i = j = eps = 0;
  ioff = joff = 0;
  while (tolower(ch=mvwgetch(warray,j-joff,(i-ioff)*colwid+
		      MIN(eps,colwid-2))) != 'q') {
    /* #define ECHOCH */
#ifdef ECHOCH
    sprintf(echobuf,"%8o",ch);
    mvwaddstr(wmenu,0,iecho,echobuf);
    iecho = (iecho < 72) ? iecho+8 :0;
    wrefresh(wmenu);
#endif
    switch (ch) {
    case KEY_LEFT:
    case KEY_RIGHT:
    case KEY_UP:
    case KEY_DOWN:
    case '\t':
    case '\015':
      if (eps) {
	line[eps] = '\0';
	set_value(i,j,type,nc,in,line);
      }
      set_cell(warray,i,j,ioff,joff,type,nc,in);
      eps = 0;
      wrefresh(warray);
      break;
    case '\b':
    case KEY_DL:
    case 0177:
      if (eps) {
	eps--;
	mvwaddch(warray,j-joff,(i-ioff)*colwid+MIN(eps,colwid-2),' ');
	wrefresh(warray);
      }
      continue;
    default:
      if (!eps && ch >= 32 && ch <= 127) {
	clear_cell(warray,i-ioff,j-joff);
	wrefresh(warray);
      }
      mvwaddch(warray,j-joff,(i-ioff)*colwid+MIN(eps,colwid-2),ch|A_UNDERLINE);
      line[eps++]=ch;
      continue;
    }

    switch (ch) {
    case KEY_LEFT:
      i = (i<2)?0:i-1;
      if (i-ioff == -1) {
	ioff--;
	wtmp = newwin(1,mycols-colwid,1,HLAB);
	overwrite(wvlab,wtmp);
	mvwin(wtmp,1,HLAB+colwid);
	overwrite(wtmp,wvlab);
	delwin(wtmp);
	update_vlab(wvlab,0,ioff);
	wtmp = newwin(drows,mycols-colwid,2,HLAB);
	overwrite(warray,wtmp);
	mvwin(wtmp,2,HLAB+colwid);
	overwrite(wtmp,warray);
	delwin(wtmp);
	update_col(warray,0,ioff,joff,type,nc,in);
	wrefresh(warray);
	wrefresh(wvlab);
      }
      break;
    case KEY_RIGHT:
    case '\015':
      i = (i>nc-2)?nc-1:i+1;
      if (i-ioff == dcols) {
	ioff++;
	wtmp = newwin(1,mycols-colwid,1,HLAB+colwid);
	overwrite(wvlab,wtmp);
	mvwin(wtmp,1,HLAB);
	overwrite(wtmp,wvlab);
	delwin(wtmp);
	update_vlab(wvlab,dcols-1,ioff);
	wtmp = newwin(drows,mycols-colwid,2,HLAB+colwid);
	overwrite(warray,wtmp);
	mvwin(wtmp,2,HLAB);
	overwrite(wtmp,warray);
	delwin(wtmp);
	update_col(warray,dcols-1,ioff,joff,type,nc,in);
	wrefresh(warray);
	wrefresh(wvlab);
      }
      break;
    case KEY_UP:
      j = (j<2)?0:j-1;
      if (j-joff == -1) {
	joff--;
	wscrl(wscroll,-1);
	wrefresh(wscroll);
	update_hlab(whlab,0,joff);
	wrefresh(whlab);
	update_row(warray,0,ioff,joff,type,nc,in);
	wrefresh(warray);
      }
      break;
    case KEY_DOWN:
    case '\t':
      j = (j>nr-2)?nr-1:j+1;
      if (j-joff == drows) {
	joff++;
	wscrl(wscroll,1);
	wrefresh(wscroll);
	update_hlab(whlab,drows-1,joff);
	wrefresh(whlab);
	update_row(warray,drows-1,ioff,joff,type,nc,in);
	wrefresh(warray);
      }
      break;
    }
  }
  nl();
  echo();
  nocbreak();
  endwin();
}
