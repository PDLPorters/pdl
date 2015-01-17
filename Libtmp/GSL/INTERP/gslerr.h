
static int status;
static char buf[200];

/* Turn off GSL default handler. 10/18/2010 Jason Lin */
#define GSLERR(x,y) gsl_set_error_handler_off (); if ((status = x y)) {sprintf(buf,"Error in %s: %s",# x ,gsl_strerror(status));barf(buf);}

