/***************************************************************

   pdliisdisplay.c

****************************************************************/

/* Redefine iis_error to use Perl's croak() */

void iis_error( char* error1, char*error2 ) {
      croak (error1,error2);
}


/*****************************************************/


/* Rest of the subroutines are identical to libiis.c v1.0 */


/******************* iis_cur ************************/

/* Return cursor position and character typed */

void iis_cur(float*x, float*y, char* ch) {

   unsigned short hdr[8];
   short buf[SZ_WCSTEXT];
   int   nbytes,wcs;

   /* Send read request */

   hdr[TRANSFER_ID] = IREAD;
   hdr[THING_COUNT] = 0;
   hdr[SUB_UNIT]   = IMCURSOR;
   hdr[CHECK_SUM]  = 0;
   hdr[X_REGISTER] = 0;
   hdr[Y_REGISTER] = 0;
   hdr[Z_REGISTER] = 0;
   hdr[T_REGISTER] = 0;
   iis_checksum(hdr);
   iis_write((char*)hdr, 8*sizeof(short));

   /* Read however many bytes it send in this case */

   if ((nbytes = read (iispipe_i, buf, SZ_WCSTEXT)) <= 0)
      iis_error ("iis_cur: cannot read IIS pipe\n","");

   if (sscanf ((char*)buf, "%f %f %d %c", x, y, &wcs, ch) != 4)
      iis_error ("iis_cur: can't parse '%s'\n", (char*)buf);
}


/******************* iis_drawcirc *******************/

/* Draw a circle on the image display at a given position */

void iis_drawcirc(float xcen, float ycen, float radius, int colour, int frame) {

   unsigned short hdr[8];
   unsigned char *data;
   int i,j,y;
   int ymin,ymax,ntrans,nlines,nbytes;
   float xcen2,ycen2,dd;
   char wcsbuf[SZ_WCSTEXT];
   float xx, yx, xy, yy, xo, yo;	/* wcs matrix values */
   float xx2, yx2, xy2, yy2, xo2, yo2;	/* wcs inverse matrix values */
   char label[1024];		        /* wcs file title */
   int  w_type;			        /* wcs scaling code */
   float low, high;	                /* wcs scaling limits */
   int chan;
   float rr;

   chan = iis_chan(frame);


   /* Send WCS read request */

   hdr[TRANSFER_ID] = -IREAD;
   hdr[THING_COUNT] = 0;
   hdr[SUB_UNIT]   = WCS;
   hdr[CHECK_SUM]  = 0;
   hdr[X_REGISTER] = 0;
   hdr[Y_REGISTER] = 0;
   hdr[Z_REGISTER] = chan;
   hdr[T_REGISTER] = 0;
   iis_checksum(hdr);
   iis_write((char*)hdr, 8*sizeof(short));

   iis_read ((char*)wcsbuf, SZ_WCSTEXT); /* Get WCS data */

   sscanf(wcsbuf, "%[^\n]\n%f%f%f%f%f%f%f%f%d", label,
         &xx, &yx, &xy, &yy, &xo, &yo, &low, &high, &w_type);

   /* Invert transform (I don't care about non-square coord systems! */

   xcen2 = (xcen-xo)/xx;
   ycen2 = frameY - (ycen-yo)/yy - 1;

   /* Correct scale factor - OK for square images don't want to
      draw ellipses for non-square ones so take geometric mean  */

   rr =  radius / sqrt(iis_abs(xx*yy));

   /* Transfer limits (with buffer to allow for edge effects) */

   ymin = ycen2-rr-2;
   if (ymin<0)
      ymin=0;
   ymax = ycen2+rr+2;
   if (ymax>=frameY)
      ymax=frameY-1;

   /* Work out how many lines to transfer at a go */

   ntrans = RBUFFSZ/frameX;
   if (ntrans<1)
      ntrans = 1;

   /* Allocate buffer for data transfers */

   data = (unsigned char*) calloc(ntrans*frameX, sizeof(unsigned char));
   if (data==NULL)
      iis_error("iis_drawcirc: out of memory for buffer","");

   /* Loop over blocks */

   for (y = ymin; y < ymax; y+=ntrans) {

      nlines = ntrans;  /* Number of lines to transfer */
      if (y+ntrans>ymax)
         nlines = ymax - y;

      /* Read data */

      hdr[TRANSFER_ID] = -IREAD | PACKED | BLOCKXFER;
      hdr[THING_COUNT] = -nlines*frameX;
      hdr[SUB_UNIT] = REFRESH;
      hdr[CHECK_SUM] = 0;
      hdr[X_REGISTER] = ADVXONTC;
      hdr[Y_REGISTER] = ADVYONXOV+frameY-y-nlines;
      hdr[Z_REGISTER] = chan;
      hdr[T_REGISTER] = ALLBITPL;
      iis_checksum(hdr);
      iis_write((char*)hdr, 8*sizeof(short));
      iis_read((char*)data, nlines*frameX*sizeof(char));

      /* Write data */

      hdr[TRANSFER_ID] = IWRITE | PACKED | BLOCKXFER;
      hdr[THING_COUNT] = -nlines*frameX;
      hdr[SUB_UNIT] = REFRESH;
      hdr[CHECK_SUM] = 0;
      hdr[X_REGISTER] = ADVXONTC;
      hdr[Y_REGISTER] = ADVYONXOV+frameY-y-nlines;
      hdr[Z_REGISTER] = chan;
      hdr[T_REGISTER] = ALLBITPL;
      iis_checksum(hdr);
      iis_write((char*)hdr, 8*sizeof(short));

      /* Change Data  - draw in i and j to fill circle gaps via symmetry */

      for (j=0; j<nlines; j++) {
          dd = rr*rr - (y+j-ycen2)*(y+j-ycen2);
          if (dd>=0) {
             dd = sqrt(dd);
             i = iis_round( (float)xcen2 - dd  );
             if (i>=0 && i<frameX)
                data[ (nlines-j-1)*frameX + i ] = colour;
             i = iis_round( (float)xcen2 + dd );
             if (i>=0 && i<frameX)
                data[ (nlines-j-1)*frameX + i ] = colour;
          }
      }

      for (i=0; i<frameX; i++) {
          dd = rr*rr - (i-xcen2)*(i-xcen2);
          if (dd>=0) {
             dd = sqrt(dd);
             j = iis_round( (float)ycen2 - (float)y - dd );
             if (j>=0 && j<nlines)
                data[ (nlines-j-1)*frameX + i ] = colour;
             j = iis_round( (float)ycen2 - (float)y + dd );
             if (j>=0 && j<nlines)
                data[ (nlines-j-1)*frameX + i ] = colour;
          }
      }

      iis_write((char*)data, nlines*frameX*sizeof(char));

   }
   free(data);
}


/******************* iis_open ****************/

/*
   Open IIS connection - if inpipe or outpipe are "" default
   pipes are searched for in the environment variable $IMTDEV,
   then in the directories (with the usual filenames) $HOME/iraf/dev,
   $HOME/dev, and finally /dev.

   Note the frame buffer configuration number and dimensions
   must be suppled by hand - life is too short to write
   imtoolrc parsing code in C!  If these don't match those in the
   appropriate imtoolrc file problems will occur.

*/

void iis_open(char* inpipe, char* outpipe, int fb, int fbx, int fby) {

   FILE *syspipe;
   char *home, *imtdev, *tok=NULL;
   char	iname[STRSIZE],oname[STRSIZE];
   int  i,j;

   home    = getenv("HOME");

   imtdev  = getenv("IMTDEV");
   if (imtdev != NULL)  /* Start parsing IMTDEV environment variable */
       tok = strtok(imtdev,":");
   if (tok!=NULL && strcmp(tok,"fifo")!=0) /* Ignore if not fifo */
      tok = NULL;

   /* Get input fifo name */

   if (strcmp(inpipe,"")==0) {

      if (tok!=NULL) {  /* Check next bit of IMTDEV */
         tok = strtok(NULL,":");
         if (tok != NULL) {
            strncpy(iname,tok,STRSIZE);
            goto gotin;
         }
      }

      /* Else look in standard places */

      strncpy(iname,home,STRSIZE); strncat(iname,"/iraf/dev/imt1i",STRSIZE);
      if (!access(iname,F_OK))
         goto gotin;
      strncpy(iname,home,STRSIZE); strncat(iname,"/dev/imt1i",STRSIZE);
      if (!access(iname,F_OK))
         goto gotin;
      strncpy(iname,"/dev/imt1i",STRSIZE);
      if (!access(iname,F_OK))
         goto gotin;
   }
   else {
      strncpy(iname,inpipe,STRSIZE); /* Use supplied arg */
      goto gotin;
   }
   iis_error("Unable to locate input FIFO in any of $HOME/dev/imt1i or %s",
      "$HOME/dev/imt1i or /dev/imt1i\n");

   gotin:

   if (strcmp(outpipe,"")==0) { /* Get output fifo name */

      if (tok!=NULL) {  /* Check next bit of IMTDEV */
         tok = strtok(NULL,":");
         if (tok != NULL) {
            strncpy(oname,tok,STRSIZE);
            goto gotout;
         }
      }
      /* Else look in standard places */

      strncpy(oname,home,STRSIZE); strncat(oname,"/iraf/dev/imt1o",STRSIZE);
      if (!access(oname,F_OK))
         goto gotout;
      strncpy(oname,home,STRSIZE); strncat(oname,"/dev/imt1o",STRSIZE);
      if (!access(oname,F_OK))
         goto gotout;
      strncpy(oname,"/dev/imt1o",STRSIZE);
      if (!access(oname,F_OK))
         goto gotout;
   }
   else {
      strncpy(oname,outpipe,STRSIZE); /* Use supplied arg */
         goto gotout;
   }

   iis_error("Unable to locate output FIFO in any of $HOME/iraf/dev/imt1o or %s",
      "$HOME/dev/imt1o or /dev/imt1o\n");

   gotout:

   /*
      Open the output fifo.  We have to open it ourselves first as a client to
      get around the fifo open-no-client error.
   */


    if ((iispipe_i = open (oname, O_RDONLY | O_NONBLOCK)) != -1) {
	if ((iispipe_o = open (oname, O_WRONLY | O_NONBLOCK)) != -1) {
	    fcntl (iispipe_o, F_SETFL, O_WRONLY);
	} else
            iis_error("iis_open: cannot open IIS output pipe %s\n",oname);
	close (iispipe_i);
    } else
       iis_error("iis_open: cannot open IIS output pipe %s\n",oname);

   /* Open the input fifo */

   if ((iispipe_i = open (iname, O_RDONLY | O_NONBLOCK)) != -1) {

      /* Clear input for reading. */
      fcntl (iispipe_i, F_SETFL, O_RDONLY);
   } else
      iis_error("iis_open: cannot open IIS input pipe %s\n",iname);

   fbconfig = fb; frameX = fbx; frameY = fby; /* Frame buffer globals */
}

/******************* iis_close ****************/

/* Close the IIS connection */

void iis_close() {
  close(iispipe_o);
  close(iispipe_i);
}

/******************* Private routines ****************/

/* write to pipe */

void iis_write (char* buf, int size) {
    int n = 0;
    int total = 0;

    while (total < size) {
	n = write (iispipe_o, buf, size - total);
	if (n <= 0)
            iis_error ("iis_write: can't write to pipe\n","");
	total += n;
    }
}

/* read from pipe */

void iis_read (char* buf, int size) {
    int n = 0;
    int total = 0;

    while (total < size) {
	n = read (iispipe_i, buf, size - total);
	if (n <= 0)
            iis_error ("iis_read: can't read from pipe\n","");
	total += n;
    }
}

void iis_checksum ( unsigned short *hdr ) {
      int indx;
      int checksum = 0;
      for (indx = 0; indx < 8; indx++) {
         checksum += hdr[indx];
      }
      hdr[CHECK_SUM] = CHECKSUMVAL - (unsigned short) checksum;
}

/* Return the channel number associated with a display frame */

int iis_chan(int frame) {
   int chan[5];

   chan[1]=CHAN1; chan[2]=CHAN2; chan[3]=CHAN3; chan[4]=CHAN4;
   if (frame>0 && frame<5)
      return chan[frame];
   else
      iis_error("iis_display: invalid frame number, must be 1-4\n","");
}

/* Round to nearest int symmetrically about zero */

int iis_round ( float i ) {
    if (i>=0)
       return (int) (i+0.5);
    else
       return -( (int)(0.5-i) );
}

float iis_abs(float x) {
   if (x<0)
      return (-x);
   else
      return x;
}
