#ifndef __CURSES_H
#define __CURSES_H

#define ERR -1
#define OK 0 

typedef void WINDOW; // fake
extern WINDOW * curscr;
extern WINDOW * newscr;
extern WINDOW * stdscr;
extern char *ttytype;
extern int COLORS;
extern int COLOR_PAIRS;
extern int COLS;
extern int ESCDELAY;
extern int LINES;
extern int TABSIZE;

typedef /* unsigned */ int chtype;


#include<stdbool.h>
#include <unistd.h>
#define TRUE 1
#define FALSE 0
/*
 * Curses uses a helper function.  Define our type for this to simplify
 * extending it for the sp-funcs feature.
 */
typedef int (*NCURSES_OUTC)(int arg0);

extern int mvaddch (int arg0, int arg1, const chtype arg2);		/* generated */

extern WINDOW * initscr (void);				/* implemented */

extern int noecho (void);				/* implemented */

extern int curs_set (int arg0);				/* implemented */

extern int nodelay (WINDOW * arg0,bool arg1);			/* implemented */

extern int leaveok (WINDOW * arg0 ,bool arg1);			/* implemented */

extern int scrollok (WINDOW * arg0,bool arg1);			/* implemented */

extern int getch (void);				/* generated */

extern int refresh (void);				/* generated */


extern int mvcur (int arg0,int arg1,int arg2,int arg3);			/* implemented */

extern int endwin (void);				/* implemented */

#endif