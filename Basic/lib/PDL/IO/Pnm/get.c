#include "get.h"

/* process one input line from an ascii pnm file
 * and store data into a pdl data component
 * returns number of elements read
 * returns -1 if garbage was encountered
 */

/* get the next number from the input string
 * return values:  len : number of characters read
 *                 0 : end of string or skip rest of string because comment
 *                -1 : found garbage
 */
#define    TRAILING_WHITESPACE_CHECK(s) \
   if (s!=' ' && s!='\t' && s!='\r' && s!='\n' && s!=',')  return -1
int getint(PerlIO *fp, PDL_Long *ip)
{
  PDL_Long i = 0;
  int nread = 0;
  int s = PerlIO_getc(fp);

  if (s == EOF) return 0;
  while (1) {
    if (s == EOF)
      return 0;   /* signal end of line */
    if (s == '#')
      SWALLOWLINE(fp);
    if (s >='0' && s <='9') break;
    if (s!=' ' && s!='\t' && s!='\r' && s!='\n' && s!=',')
      return -1;  /* garbage */
    s = PerlIO_getc(fp); /* else skip whitespace */
  }
  /* parse number */
  while (1) {
    i = (i*10) + (s - '0');
    nread++;
    if ((s = PerlIO_getc(fp)) == EOF) break; /* we could loose that */
    if (s<'0' || s>'9') break;
  }
  *ip = i;
  TRAILING_WHITESPACE_CHECK(s);
  return nread;
}
