#include "EXTERN.h"
#include "perl.h"
#include "pdl.h"

#define SWALLOWLINE(fp) while ((s = PerlIO_getc(fp)) != '\n' && s != EOF)
#define    TRAILING_WHITESPACE_CHECK(s) \
   if (s!=' ' && s!='\t' && s!='\r' && s!='\n' && s!=',')  return -1

int getfloat(PerlIO *fp, PDL_Float *fz)
{
  PDL_Float f = 0;
  int nread = 0;
  int i, s = PerlIO_getc(fp);
  int afterp = 0, aftere=0;
  int expo = 0;
  PDL_Float sig = 1.0, esig = 1.0;
  PDL_Float div = 1.0;

  if (s == EOF) return 0;
  while (1) {
    if (s == EOF)
      return 0;   /* signal end of line */
    if (s == '#')
      SWALLOWLINE(fp);
    if ((s >='0' && s <='9') || s =='.' || s == 'e' || s == 'E' 
        || s == '+' || s == '-') break; 
    if (s!=' ' && s!='\t' && s!='\r' && s!='\n' && s!=',')
      return -1;  /* garbage */      
    s = PerlIO_getc(fp); /* else skip whitespace */
  }
  /* parse number */
  while (1) {
    switch (s) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      if (aftere)
        expo = (expo*10) + (s - '0');
      else if (afterp) {
        div /= 10.0;
        f += div*(s - '0');
      } else
        f = (f*10) + (s - '0');
      break;
    case '+':
      /* ignore */
      break;
    case '-':
      if (aftere)
        esig = -1;
      else
        sig = -1;
      break;
    case 'e':
    case 'E':
      if (aftere)
        return -1;
      aftere = 1;
      break;
    case '.':
      if (afterp || aftere)
        return -1;
      afterp = 1;
      break;
    default:
      goto endread;
      break;
    }
    nread++;
    s = PerlIO_getc(fp);
  }
endread:
  f *= sig;
  for (i=0;i<expo; i++)
    f *= (esig > 0 ? 10.0 : 0.1);

  *fz = f;
  TRAILING_WHITESPACE_CHECK(s);
  return nread;
}

int getdouble(PerlIO *fp, PDL_Double *fz)
{
  PDL_Double f = 0;
  int nread = 0;
  int i, s = PerlIO_getc(fp);
  int afterp = 0, aftere=0;
  int expo = 0;
  PDL_Double sig = 1.0, esig = 1.0;
  PDL_Double div = 1.0;

  if (s == EOF) return 0;
  while (1) {
    if (s == EOF)
      return 0;   /* signal end of line */
    if (s == '#')
      SWALLOWLINE(fp);
    if ((s >='0' && s <='9') || s =='.' || s == 'e' || s == 'E' 
        || s == '+' || s == '-') break; 
    if (s!=' ' && s!='\t' && s!='\r' && s!='\n' && s!=',')
      return -1;  /* garbage */      
    s = PerlIO_getc(fp); /* else skip whitespace */
  }
  /* parse number */
  while (1) {
    switch (s) {
    case '0':
    case '1':
    case '2':
    case '3':
    case '4':
    case '5':
    case '6':
    case '7':
    case '8':
    case '9':
      if (aftere)
        expo = (expo*10) + (s - '0');
      else if (afterp) {
        div /= 10.0;
        f += div*(s - '0');
      } else
        f = (f*10) + (s - '0');
      break;
    case '+':
      /* ignore */
      break;
    case '-':
      if (aftere)
        esig = -1;
      else
        sig = -1;
      break;
    case 'e':
    case 'E':
      if (aftere)
        return -1;
      aftere = 1;
      break;
    case '.':
      if (afterp || aftere)
        return -1;
      afterp = 1;
      break;
    default:
      goto endread;
      break;
    }
    nread++;
    s = PerlIO_getc(fp);
  }
endread:
  f *= sig;
  for (i=0;i<expo; i++)
    f *= (esig > 0 ? 10.0 : 0.1);

  *fz = f;
  TRAILING_WHITESPACE_CHECK(s);
  return nread;
}
