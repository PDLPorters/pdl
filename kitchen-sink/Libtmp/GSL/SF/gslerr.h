
static int status;
static char buf[200];

#define GSLERR(x,y) if ((status = x y)) {snprintf(buf,200,"Error in %s: %s", #x, gsl_strerror(status));barf("%s", buf);}
