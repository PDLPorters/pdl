int fits_rcomp(char **ret, int a[], int nx, unsigned char *c, int clen,int nblock);
int fits_rcomp_short(char **ret, short a[], int nx, unsigned char *c, int clen,int nblock);
int fits_rcomp_byte(char **ret, signed char a[], int nx, unsigned char *c, int clen,int nblock);
char *fits_rdecomp (unsigned char *c, int clen, unsigned int array[], int nx,
             int nblock);
char *fits_rdecomp_short (unsigned char *c, int clen, unsigned short array[], int nx,
             int nblock);
char *fits_rdecomp_byte (unsigned char *c, int clen, unsigned char array[], int nx,
             int nblock);
