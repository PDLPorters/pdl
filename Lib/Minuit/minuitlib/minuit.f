cdeck  id>, minuit. 
      subroutine minuit(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c
c  cpnam   parameter name (10 characters)
c  u       external (visible to user in fcn) value of parameter
c  alim, blim lower and upper parameter limits. if both zero, no limits.
c  erp,ern positive and negative minos errors, if calculated.
c  werr    external parameter error (standard deviation, defined by up)
c  globcc  global correlation coefficient
c  nvarl   =-1 if parameter undefined,      =0 if constant,
c          = 1 if variable without limits,  =4 if variable with limits
c   (note that if parameter has been fixed, nvarl=1 or =4, and niofex=0)
c  niofex  internal parameter number, or zero if not currently variable
c  nexofi  external parameter number for currently variable parameters
c  x, xt   internal parameter values (x are sometimes saved in xt)
c  dirin   (internal) step sizes for current step
c  variables with names ending in ..s are saved values for fixed params
c  vhmat   (internal) error matrix stored as half matrix, since
c                it is symmetric
c  vthmat  vhmat is sometimes saved in vthmat, especially in mnmnot
c
c  isw definitions:
c      isw(1) =0 normally, =1 means call limit exceeded
c      isw(2) =0 means no error matrix
c             =1 means only approximate error matrix
c             =2 means full error matrix, but forced pos-def.
c             =3 means good normal full error matrix exists
c      isw(3) =0 if minuit is calculating the first derivatives
c             =1 if first derivatives calculated inside fcn
c      isw(4) =-1 if most recent minimization did not converge.
c             = 0 if problem redefined since most recent minimization.
c             =+1 if most recent minimization did converge.
c      isw(5) is the print level.  see sho printlevel
c      isw(6) = 0 for batch mode, =1 for interactive mode
c
c  lwarn is true if warning messges are to be put out (default=true)
c            set warn turns it on, set nowarn turns it off
c  lrepor is true if exceptional conditions are put out (default=false)
c            set debug turns it on, set nodebug turns it off
c  limset is true if a parameter is up against limits (for minos)
c  lnolim is true if there are no limits on any parameters (not yet used)
c  lnewmn is true if the previous process has unexpectedly improved fcn
c  lphead is true if a heading should be put out for the next parameter
c        definition, false if a parameter has just been defined
c
      external fcn,futil
      character*40 cwhyxt
      data cwhyxt/'for unknown reasons                     '/
      data jsysrd,jsyswr,jsyssa/5,6,7/
c                                 . . . . . . . . . . initialize minuit
      write (jsyswr,'(1x,75(1h*))')
      call mninit (jsysrd,jsyswr,jsyssa)
c                                      . . . . initialize new data block
  100 continue
      write (isyswr,'(1x,75(1h*))')
      nblock = nblock + 1
      write (isyswr,'(26x,a,i4)')  'minuit data block no.',nblock
      write (isyswr,'(1x,75(1h*))')
c               . . . . . . . . . . .   set parameter lists to undefined
      call mncler
c                                             . . . . . . . . read title
      call mnread(fcn,1,iflgut,futil)
      if (iflgut .eq. 2)  go to 500
      if (iflgut .eq. 3)  go to 600
c                                        . . . . . . . . read parameters
      call mnread(fcn,2,iflgut,futil)
      if (iflgut .eq. 2)  go to 500
      if (iflgut .eq. 3)  go to 600
      if (iflgut .eq. 4)  go to 700
c                              . . . . . . verify fcn not time-dependent
      write (isyswr,'(/a,a)') ' minuit: first call to user function,',
     +    ' with iflag=1'
      nparx = npar
      call mninex(x)
      fzero = undefi
      call fcn(nparx,gin,fzero,u,1,futil)
      first = undefi
      call fcn(nparx,gin,first,u,4,futil)
      nfcn = 2
      if (fzero.eq.undefi .and. first.eq.undefi)  then
          cwhyxt = 'by error in user function.  '
          write (isyswr,'(/a,a/)') ' user has not calculated function',
     +    ' value when iflag=1 or 4'
          go to 800
      endif
      amin = first
      if (first .eq. undefi) amin=fzero
      call mnprin(1,amin)
      nfcn = 2
      if (first .eq. fzero)  go to 300
      fnew = 0.0
      call fcn(nparx,gin,fnew,u,4,futil)
      if  (fnew .ne. amin) write (isyswr,280) amin, fnew
  280 format (/' minuit warning: probable error in user function.'/
     +         ' for fixed values of parameters, fcn is time-dependent'/
     +         ' f =',e22.14,' for first call'/
     +         ' f =',e22.14,' for second call.'/)
      nfcn = 3
  300 fval3 = 2.0*amin+1.0
c                                   . . . . . . . . . . . read commands
      call mnread(fcn,3,iflgut,futil)
      if (iflgut .eq. 2)  go to 500
      if (iflgut .eq. 3)  go to 600
      if (iflgut .eq. 4)  go to 700
      cwhyxt = 'by minuit command: '//cword
      if (index(cword,'stop').gt. 0)  go to 800
      if (index(cword,'exi') .gt. 0)  go to 800
      if (index(cword,'ret') .eq. 0)  go to 100
      cwhyxt = 'and returns to user program.    '
      write (isyswr,'(a,a)')  ' ..........minuit terminated ',cwhyxt
      return
c                                           . . . . . . stop conditions
  500 continue
      cwhyxt = 'by end-of-data on primary input file.   '
      go to 800
  600 continue
      cwhyxt = 'by unrecoverable read error on input.   '
      go to 800
  700 continue
      cwhyxt = ': fatal error in parameter definitions. '
  800 write (isyswr,'(a,a)')  ' ..........minuit terminated ',cwhyxt
      stop
c
c  ......................entry to set unit numbers  - - - - - - - - - -
      entry mintio(i1,i2,i3)
      jsysrd = i1
      jsyswr = i2
      jsyssa = i3
      return
      end
cdeck  id>, mnamin. 
      subroutine mnamin(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called  from many places.  initializes the value of amin by
cc        calling the user function. prints out the function value and
cc        parameter values if print flag value is high enough.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      nparx = npar
      if (isw(5) .ge. 1) write (isyswr,'(/a,a)') ' first call to ',
     + 'user function at new start point, with iflag=4.'
      call mnexin(x)
      call fcn(nparx,gin,fnew,u,4,futil)
      nfcn = nfcn + 1
      amin = fnew
      edm = bigedm
      return
      end
cdeck  id>, mnbins. 
      subroutine mnbins(a1,a2,naa,bl,bh,nb,bwid)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
c         subroutine to determine reasonable histogram intervals
c         given absolute upper and lower bounds  a1 and a2
c         and desired maximum number of bins naa
c         program makes reasonable binning from bl to bh of width bwid
c         f. james,   august, 1974 , stolen for minuit, 1988
      parameter (zero=0.0)
      al = min(a1,a2)
      ah = max(a1,a2)
      if (al.eq.ah)  ah = al + 1.
c         if naa .eq. -1 , program uses bwid input from calling routine
      if (naa .eq. -1)  go to 150
   10 na = naa - 1
      if (na .lt. 1)  na = 1
c          get nominal bin width in expon form
   20 awid = (ah-al)/float(na)
      log = int(log10(awid))
      if (awid .le. 1.0)  log=log-1
      sigfig = awid * (10.00 **(-log))
c         round mantissa up to 2, 2.5, 5, or 10
      if(sigfig .gt. 2.0)  go to 40
      sigrnd = 2.0
      go to 100
   40 if (sigfig .gt. 2.5)  go to 50
      sigrnd = 2.5
      go to 100
   50 if(sigfig .gt. 5.0)  go to 60
      sigrnd =5.0
      go to 100
   60 sigrnd = 1.0
      log = log + 1
  100 continue
      bwid = sigrnd*10.0**log
      go to 200
c         get new bounds from new width bwid
  150 if (bwid .le. zero)  go to 10
  200 continue
      alb = al/bwid
      lwid=alb
      if (alb .lt. zero)  lwid=lwid-1
      bl = bwid*float(lwid)
      alb = ah/bwid + 1.0
      kwid = alb
      if (alb .lt. zero)  kwid=kwid-1
      bh = bwid*float(kwid)
      nb = kwid-lwid
      if (naa .gt. 5)  go to 240
      if (naa .eq. -1)  return
c          request for one bin is difficult case
      if (naa .gt. 1 .or. nb .eq. 1)  return
      bwid =  bwid*2.0
       nb  = 1
       return
  240 if (2*nb .ne. naa)  return
      na = na + 1
      go to 20
      end
cdeck  id>, mncalf. 
      subroutine mncalf(fcn,pvec,ycalf,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called only from mnimpr.  transforms the function fcn
cc        by dividing out the quadratic part in order to find further
cc        minima.    calculates  ycalf = (f-fmin)/(x-xmin)*v*(x-xmin)
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension pvec(15)
      nparx = npar
      call mninex(pvec)
      call fcn(nparx,gin,f,u,4,futil)
      nfcn = nfcn + 1
      do 200 i= 1, npar
      grd(i) = 0.
         do 200 j= 1, npar
         m = max(i,j)
         n = min(i,j)
         ndex = m*(m-1)/2 + n
  200    grd(i) = grd(i) + vthmat(ndex) * (xt(j)-pvec(j))
      denom = 0.
      do 210 i= 1, npar
  210 denom = denom + grd(i) * (xt(i)-pvec(i))
      if (denom .le. zero)  then
         dcovar = 1.
         isw(2) = 0
         denom = 1.0
      endif
      ycalf = (f-apsi) / denom
      return
      end
cdeck  id>, mncler. 
      subroutine mncler
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called from minuit and by option from mnexcm
cc        resets the parameter list to undefined
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      npfix = 0
      nu = 0
      npar = 0
      nfcn = 0
      nwrmes(1) = 0
      nwrmes(2) = 0
      do 10 i= 1, maxext
      u(i) = 0.0
      cpnam(i) = cundef
      nvarl(i) = -1
   10 niofex(i) = 0
      call mnrset(1)
      cfrom = 'clear   '
      nfcnfr = nfcn
      cstatu ='undefined '
      lnolim = .true.
      lphead = .true.
      return
      end
cdeck  id>, mncntr. 
      subroutine mncntr(fcn,ke1,ke2,ierrf,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       to print function contours in two variables, on line printer
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      parameter (numbcs=20,nxmax=115)
      dimension contur(numbcs), fcna(nxmax),fcnb(nxmax)
      character clabel*(numbcs)
      character chln*(nxmax),chmid*(nxmax),chzero*(nxmax)
      data clabel/'0123456789abcdefghij'/
c                 input arguments: parx, pary, devs, ngrid
      if (ke1.le.0 .or. ke2.le.0)  go to 1350
      if (ke1.gt.nu .or. ke2.gt.nu)  go to 1350
      ki1 = niofex(ke1)
      ki2 = niofex(ke2)
      if (ki1.le.0 .or. ki2.le.0)  go to 1350
      if (ki1 .eq. ki2)  go to 1350
c
      if (isw(2) .lt. 1)  then
          call mnhess(fcn,futil)
          call mnwerr
          endif
      nparx = npar
      xsav = u(ke1)
      ysav = u(ke2)
      devs = word7(3)
      if (devs .le. zero)  devs=2.
      xlo = u(ke1) - devs*werr(ki1)
      xup = u(ke1) + devs*werr(ki1)
      ylo = u(ke2) - devs*werr(ki2)
      yup = u(ke2) + devs*werr(ki2)
      ngrid = word7(4)
      if (ngrid .le. 0)  then
          ngrid=25
          nx = min(npagwd-15,ngrid)
          ny = min(npagln-7, ngrid)
      else
          nx = ngrid
          ny = ngrid
      endif
      if (nx .lt. 11) nx=11
      if (ny .lt. 11) ny=11
      if (nx .ge. nxmax)  nx=nxmax-1
c         ask if parameter outside limits
      if (nvarl(ke1) .gt. 1)  then
         if (xlo .lt. alim(ke1))  xlo = alim(ke1)
         if (xup .gt. blim(ke1))  xup = blim(ke1)
      endif
      if (nvarl(ke2) .gt. 1)   then
         if (ylo .lt. alim(ke2))  ylo = alim(ke2)
         if (yup .gt. blim(ke2))  yup = blim(ke2)
      endif
      bwidx = (xup-xlo)/real(nx)
      bwidy = (yup-ylo)/real(ny)
      ixmid = int((xsav-xlo)*real(nx)/(xup-xlo)) + 1
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      do 185 i= 1, numbcs
      contur(i) = amin + up*float(i-1)**2
  185 continue
      contur(1) = contur(1) + 0.01*up
c                fill fcnb to prepare first row, and find column zero
      u(ke2) = yup
      ixzero = 0
      xb4 = one
      do 200 ix= 1, nx+1
      u(ke1) = xlo + real(ix-1)*bwidx
      call fcn(nparx,gin,ff,u,4,futil)
      fcnb(ix) = ff
      if (xb4.lt.zero .and. u(ke1).gt.zero)  ixzero = ix-1
      xb4 = u(ke1)
      chmid(ix:ix) = '*'
      chzero(ix:ix)= '-'
  200 continue
      write (isyswr,'(a,i3,a,a)') ' y-axis: parameter ',
     +      ke2,': ',cpnam(ke2)
      if (ixzero .gt. 0)  then
         chzero(ixzero:ixzero) = '+'
         chln = ' '
         write (isyswr,'(12x,a,a)') chln(1:ixzero),'x=0'
      endif
c                 loop over rows
      do 280 iy= 1, ny
      unext = u(ke2) - bwidy
c                 prepare this line's background pattern for contour
      chln = ' '
      chln(ixmid:ixmid) = '*'
      if (ixzero .ne. 0) chln(ixzero:ixzero) = ':'
      if (u(ke2).gt.ysav .and. unext.lt.ysav) chln=chmid
      if (u(ke2).gt.zero .and. unext.lt.zero) chln=chzero
      u(ke2) = unext
      ylabel = u(ke2) + 0.5*bwidy
c                 move fcnb to fcna and fill fcnb with next row
      do 220 ix= 1, nx+1
      fcna(ix) = fcnb(ix)
      u(ke1) = xlo + real(ix-1)*bwidx
      call fcn(nparx,gin,ff,u,4,futil)
      fcnb(ix) = ff
  220 continue
c                 look for contours crossing the fcnxy squares
      do 250 ix= 1, nx
      fmx = max(fcna(ix),fcnb(ix),fcna(ix+1),fcnb(ix+1))
      fmn = min(fcna(ix),fcnb(ix),fcna(ix+1),fcnb(ix+1))
      do 230 ics= 1, numbcs
      if (contur(ics) .gt. fmn)  go to 240
  230 continue
      go to 250
  240 if (contur(ics) .lt. fmx) chln(ix:ix)=clabel(ics:ics)
  250 continue
c                 print a row of the contour plot
      write (isyswr,'(1x,g12.4,1x,a)') ylabel,chln(1:nx)
  280 continue
c                 contours printed, label x-axis
      chln = ' '
      chln( 1: 1) = 'i'
      chln(ixmid:ixmid) = 'i'
      chln(nx:nx) = 'i'
      write (isyswr,'(14x,a)') chln(1:nx)
c                the hardest of all: print x-axis scale!
      chln = ' '
      if (nx .le. 26) then
          nl = max(nx-12,2)
          nl2 = nl/2
          write (isyswr,'(8x,g12.4,a,g12.4)') xlo,chln(1:nl),xup
          write (isyswr,'(14x,a,g12.4)')   chln(1:nl2),xsav
      else
          nl = max(nx-24,2)/2
          nl2 = nl
          if (nl .gt. 10) nl2=nl-6
          write (isyswr,'(8x,g12.4,a,g12.4,a,g12.4)')  xlo,
     +      chln(1:nl),xsav,chln(1:nl2),xup
      endif
      write (isyswr,'(6x,a,i3,a,a,a,g12.4)') ' x-axis: parameter',
     +    ke1,': ',cpnam(ke1),'  one column=',bwidx
      write (isyswr,'(a,g12.4,a,g12.4,a)') ' function values: f(i)=',
     +    amin,' +',up,' *i**2'
c                 finished.  reset input values
      u(ke1) = xsav
      u(ke2) = ysav
      ierrf = 0
      return
 1350 write (isyswr,1351)
 1351 format (' invalid parameter number(s) requested.  ignored.' /)
      ierrf = 1
      return
      end
cdeck  id>, mncont. 
      subroutine mncont(fcn,ke1,ke2,nptu,xptu,yptu,ierrf,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       find nptu points along a contour where the function
cc             fmin (x(ke1),x(ke2)) =  amin+up
cc       where fmin is the minimum of fcn with respect to all
cc       the other npar-2 variable parameters (if any).
cc   ierrf on return will be equal to the number of points found:
cc     nptu if normal termination with nptu points found
cc     -1   if errors in the calling sequence (ke1, ke2 not variable)
cc      0   if less than four points can be found (using mnmnot)
cc     n>3  if only n points can be found (n < nptu)
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension xptu(nptu), yptu(nptu), w(mni),gcc(mni)
      character chere*10
      parameter (chere='mncontour ')
      logical ldebug
      external fcn,futil
c                 input arguments: parx, pary, devs, ngrid
      ldebug = (idbg(6) .ge. 1)
      if (ke1.le.0 .or. ke2.le.0)  go to 1350
      if (ke1.gt.nu .or. ke2.gt.nu)  go to 1350
      ki1 = niofex(ke1)
      ki2 = niofex(ke2)
      if (ki1.le.0 .or. ki2.le.0)  go to 1350
      if (ki1 .eq. ki2)  go to 1350
      if (nptu .lt. 4)  go to 1400
c
      nfcnco = nfcn
      nfcnmx = 100*(nptu+5)*(npar+1)
c           the minimum
      call mncuve(fcn,futil)
      u1min = u(ke1)
      u2min = u(ke2)
      ierrf = 0
      cfrom = chere
      nfcnfr = nfcnco
      if (isw(5) .ge. 0)  then
         write (isyswr,'(1x,a,i4,a)')
     +   'start mncontour calculation of',nptu,' points on contour.'
         if (npar .gt. 2) then
            if (npar .eq. 3) then
              ki3 = 6 - ki1 - ki2
              ke3 = nexofi(ki3)
              write (isyswr,'(1x,a,i3,2x,a)')
     +        'each point is a minimum with respect to parameter ',
     +        ke3, cpnam(ke3)
            else
              write (isyswr,'(1x,a,i3,a)')
     +        'each point is a minimum with respect to the other',
     +        npar-2, ' variable parameters.'
            endif
         endif
      endif
c
c           find the first four points using mnmnot
c              ........................ first two points
      call mnmnot(fcn,ke1,ke2,val2pl,val2mi,futil)
      if (ern(ki1) .eq. undefi)  then
         xptu(1) = alim(ke1)
         call mnwarn('w',chere,'contour squeezed by parameter limits.')
      else
         if (ern(ki1) .ge. zero)  go to 1500
         xptu(1) = u1min+ern(ki1)
      endif
      yptu(1) = val2mi
c
      if (erp(ki1) .eq. undefi)  then
         xptu(3) = blim(ke1)
         call mnwarn('w',chere,'contour squeezed by parameter limits.')
      else
         if (erp(ki1) .le. zero)  go to 1500
         xptu(3) = u1min+erp(ki1)
      endif
      yptu(3) = val2pl
      scalx = 1.0/(xptu(3) - xptu(1))
c              ........................... next two points
      call mnmnot(fcn,ke2,ke1,val2pl,val2mi,futil)
      if (ern(ki2) .eq. undefi)  then
         yptu(2) = alim(ke2)
         call mnwarn('w',chere,'contour squeezed by parameter limits.')
      else
         if (ern(ki2) .ge. zero)  go to 1500
         yptu(2) = u2min+ern(ki2)
      endif
      xptu(2) = val2mi
      if (erp(ki2) .eq. undefi)  then
         yptu(4) = blim(ke2)
         call mnwarn('w',chere,'contour squeezed by parameter limits.')
      else
         if (erp(ki2) .le. zero)  go to 1500
         yptu(4) = u2min+erp(ki2)
      endif
      xptu(4) = val2pl
      scaly = 1.0/(yptu(4) - yptu(2))
      nowpts = 4
      next = 5
      if (ldebug) then
         write (isyswr,'(a)') ' plot of four points found by minos'
         xpt(1) = u1min
         ypt(1) = u2min
         chpt(1) = ' '
         nall = min(nowpts+1,maxcpt)
         do 85 i= 2, nall
           xpt(i) = xptu(i-1)
           ypt(i) = yptu(i-1)
   85    continue
           chpt(2)= 'a'
           chpt(3)= 'b'
           chpt(4)= 'c'
           chpt(5)= 'd'
         call mnplot(xpt,ypt,chpt,nall,isyswr,npagwd,npagln)
      endif
c
c               ..................... save some values before fixing
      isw2 = isw(2)
      isw4 = isw(4)
      sigsav = edm
      istrav = istrat
      dc = dcovar
      apsi  = epsi*0.5
      abest=amin
      mpar=npar
      nfmxin = nfcnmx
      do 125 i= 1, mpar
  125 xt(i) = x(i)
      do 130 j= 1, mpar*(mpar+1)/2
  130 vthmat(j) = vhmat(j)
      do 135 i= 1, mpar
      gcc(i) = globcc(i)
  135 w(i) = werr(i)
c                           fix the two parameters in question
      kints = niofex(ke1)
      call mnfixp (kints,ierr)
      kints = niofex(ke2)
      call mnfixp (kints,ierr)
c               ......................fill in the rest of the points
      do 900 inew= next, nptu
c            find the two neighbouring points with largest separation
      bigdis = 0.
         do 200  iold = 1, inew-1
         i2 = iold + 1
         if (i2 .eq. inew) i2 = 1
         dist = (scalx*(xptu(iold)-xptu(i2)))**2 +
     +          (scaly*(yptu(iold)-yptu(i2)))**2
         if (dist .gt. bigdis) then
            bigdis = dist
            idist = iold
         endif
  200    continue
      i1 = idist
      i2 = i1 + 1
      if (i2 .eq. inew) i2 = 1
c                   next point goes between i1 and i2
      a1 = half
      a2 = half
  300 xmidcr = a1*xptu(i1) + a2*xptu(i2)
      ymidcr = a1*yptu(i1) + a2*yptu(i2)
      xdir = yptu(i2) - yptu(i1)
      ydir = xptu(i1) - xptu(i2)
      sclfac = max(abs(xdir*scalx), abs(ydir*scaly))
      xdircr = xdir/sclfac
      ydircr = ydir/sclfac
      ke1cr = ke1
      ke2cr = ke2
c                find the contour crossing point along dir
      amin = abest
      call mncros(fcn,aopt,iercr,futil)
      if (iercr .gt. 1)  then
c              if cannot find mid-point, try closer to point 1
         if (a1 .gt. half) then
            write (isyswr,'(a,a,i3,a)') ' mncont cannot find next',
     +           ' point on contour.  only ',nowpts,' points found.'
            go to 950
         endif
         call mnwarn('w',chere,'cannot find midpoint, try closer.')
         a1 = 0.75
         a2 = 0.25
         go to 300
      endif
c                contour has been located, insert new point in list
         do 830 move= nowpts,i1+1,-1
         xptu(move+1) = xptu(move)
         yptu(move+1) = yptu(move)
  830    continue
      nowpts = nowpts + 1
      xptu(i1+1) = xmidcr + xdircr*aopt
      yptu(i1+1) = ymidcr + ydircr*aopt
  900 continue
  950 continue
c     ierrf = nowpts
      cstatu = 'successful'
      if (nowpts .lt. nptu)  cstatu = 'incomplete'
c                make a lineprinter plot of the contour
      if (isw(5) .ge. 0) then
         xpt(1) = u1min
         ypt(1) = u2min
         chpt(1) = ' '
         nall = min(nowpts+1,maxcpt)
         do 1000 i= 2, nall
           xpt(i) = xptu(i-1)
           ypt(i) = yptu(i-1)
           chpt(i)= 'x'
 1000    continue
         write (isyswr,'(a,i3,2x,a)') ' y-axis: parameter ',ke2,
     +        cpnam(ke2)
         call mnplot(xpt,ypt,chpt,nall,isyswr,npagwd,npagln)
         write (isyswr,'(25x,a,i3,2x,a)') 'x-axis: parameter ',
     +         ke1,cpnam(ke1)
      endif
c                 print out the coordinates around the contour
      if (isw(5) .ge. 1)  then
         npcol = (nowpts+1)/2
         nfcol = nowpts/2
         write (isyswr,'(/i5,a,g13.5,a,g11.3)') nowpts,
     +    ' points on contour.   fmin=',abest,'   errdef=',up
         write (isyswr,'(9x,a,3x,a,18x,a,3x,a)')
     +         cpnam(ke1),cpnam(ke2),cpnam(ke1),cpnam(ke2)
         do 1050 line = 1, nfcol
           lr = line + npcol
           write (isyswr,'(1x,i5,2g13.5,10x,i5,2g13.5)')
     +     line,xptu(line),yptu(line),lr,xptu(lr),yptu(lr)
 1050    continue
         if (nfcol .lt. npcol) write (isyswr,'(1x,i5,2g13.5)')
     +                         npcol,xptu(npcol),yptu(npcol)
      endif
c                                    . . contour finished. reset v
      itaur = 1
      call mnfree(1)
      call mnfree(1)
      do 1100 j= 1, mpar*(mpar+1)/2
 1100 vhmat(j) = vthmat(j)
      do 1120 i= 1, mpar
      globcc(i) = gcc(i)
      werr(i) = w(i)
 1120 x(i) = xt(i)
      call mninex (x)
      edm = sigsav
      amin = abest
      isw(2) = isw2
      isw(4) = isw4
      dcovar = dc
      itaur = 0
      nfcnmx = nfmxin
      istrat = istrav
      u(ke1) = u1min
      u(ke2) = u2min
      go to 2000
c                                     error returns
 1350 write (isyswr,'(a)') ' invalid parameter numbers.'
      go to 1450
 1400 write (isyswr,'(a)') ' less than four points requested.'
 1450 ierrf = -1
      cstatu = 'user error'
      go to 2000
 1500 write (isyswr,'(a)') ' mncont unable to find four points.'
      u(ke1) = u1min
      u(ke2) = u2min
      ierrf = 0
      cstatu = 'failed'
 2000 continue
      cfrom = chere
      nfcnfr = nfcnco
      return
      end
cdeck  id>, mncrck. 
      subroutine mncrck(crdbuf,maxcwd,comand,lnc,
     +                         mxp,   plist, llist,ierr,isyswr)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc
cc       called from mnread.
cc       cracks the free-format input, expecting zero or more
cc         alphanumeric fields (which it joins into comand(1:lnc))
cc         followed by one or more numeric fields separated by
cc         blanks and/or one comma.  the numeric fields are put into
cc         the llist (but at most mxp) elements of plist.
cc      ierr = 0 if no errors,
cc           = 1 if error(s).
cc      diagnostic messages are written to isyswr
cc
      parameter (maxelm=25, mxlnel=19)
      character*(*) comand, crdbuf
      character cnumer*13, celmnt(maxelm)*(mxlnel), cnull*15
      dimension lelmnt(maxelm),plist(mxp)
      data cnull /')null string   '/
      data cnumer/'123456789-.0+'/
      ielmnt = 0
      lend = len(crdbuf)
      nextb = 1
      ierr = 0
c                                   . . . .  loop over words celmnt
   10 continue
      do 100 ipos= nextb,lend
         ibegin = ipos
         if (crdbuf(ipos:ipos).eq.' ')  go to 100
         if (crdbuf(ipos:ipos).eq.',')  go to 250
         go to 150
  100 continue
         go to 300
  150 continue
c               found beginning of word, look for end
         do 180 ipos = ibegin+1,lend
         if (crdbuf(ipos:ipos).eq.' ')  go to 250
         if (crdbuf(ipos:ipos).eq.',')  go to 250
  180    continue
      ipos = lend+1
  250 iend = ipos-1
      ielmnt = ielmnt + 1
      if (iend .ge. ibegin) then
         celmnt(ielmnt) = crdbuf(ibegin:iend)
      else
         celmnt(ielmnt) = cnull
      endif
      lelmnt(ielmnt) = iend-ibegin+1
      if (lelmnt(ielmnt) .gt. mxlnel)  then
         write (isyswr, 253) crdbuf(ibegin:iend),celmnt(ielmnt)
  253    format (' minuit warning: input data word too long.'
     +   /'     original:',a
     +   /' truncated to:',a)
         lelmnt(ielmnt) = mxlnel
         endif
      if (ipos .ge. lend) go to 300
      if (ielmnt .ge. maxelm)  go to 300
c                     look for comma or beginning of next word
         do 280 ipos= iend+1,lend
         if (crdbuf(ipos:ipos) .eq. ' ') go to 280
         nextb = ipos
         if (crdbuf(ipos:ipos) .eq. ',') nextb = ipos+1
         go to 10
  280    continue
c                 all elements found, join the alphabetic ones to
c                                form a command
  300 continue
      nelmnt = ielmnt
      comand = ' '
      lnc = 1
      plist(1) = 0.
      llist = 0
      if (ielmnt .eq. 0)  go to 900
      kcmnd = 0
         do 400 ielmnt = 1, nelmnt
         if (celmnt(ielmnt) .eq. cnull)  go to 450
            do 350 ic= 1, 13
            if (celmnt(ielmnt)(1:1) .eq. cnumer(ic:ic)) go to 450
  350       continue
         if (kcmnd .ge. maxcwd) go to 400
         left = maxcwd-kcmnd
         ltoadd = lelmnt(ielmnt)
         if (ltoadd .gt. left) ltoadd=left
         comand(kcmnd+1:kcmnd+ltoadd) = celmnt(ielmnt)(1:ltoadd)
         kcmnd = kcmnd + ltoadd
         if (kcmnd .eq. maxcwd)  go to 400
         kcmnd = kcmnd + 1
         comand(kcmnd:kcmnd) = ' '
  400    continue
      lnc = kcmnd
      go to 900
  450 continue
      lnc = kcmnd
c                      . . . .  we have come to a numeric field
      llist = 0
      do 600 ifld= ielmnt,nelmnt
      llist = llist + 1
      if (llist .gt. mxp) then
         nreq = nelmnt-ielmnt+1
         write (isyswr,511) nreq,mxp
  511 format (/' minuit warning in mncrck: '/ ' command has input',i5,
     + ' numeric fields, but minuit can accept only',i3)
         go to 900
      endif
      if (celmnt(ifld) .eq. cnull)  then
          plist(llist) = 0.
        else
          read (celmnt(ifld), '(bn,f19.0)',err=575) plist(llist)
      endif
      go to 600
  575 write (isyswr,'(a,a,a)') ' format error in numeric field: "',
     + celmnt(ifld)(1:lelmnt(ifld)),'"'
      ierr = 1
      plist(llist) = 0.
  600 continue
c                                  end loop over numeric fields
  900 continue
      if (lnc .le. 0)  lnc=1
      return
      end
cdeck  id>, mncros. 
      subroutine mncros(fcn,aopt,iercr,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       find point where mneval=amin+up, along the line through
cc       xmid,ymid with direction xdir,ydir,   where x and y are
cc       parameters ke1 and ke2.  if ke2=0 (from minos), then
cc       only ke1 is varied.  from mncont, both are varied.
cc       crossing point is at
cc        (u(ke1),u(ke2)) = (xmid,ymid) + aopt*(xdir,ydir)
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character chere*10, charal*28, chsign*4
      parameter (chere='mncontour ', mlsb=3, maxitr=15, tlr=0.01)
      dimension flsb(mlsb),alsb(mlsb), coeff(3)
      logical ldebug
      external fcn,futil
      data  charal/' .abcdefghijklmnopqrstuvwxyz'/
      ldebug = (idbg(6) .ge. 1)
      aminsv = amin
      aim = amin + up
      tlf = tlr*up
      tla = tlr*0.1
      xpt(1) = 0.0
      ypt(1) = aim
      chpt(1) = ' '
      xpt(2) = -1.0
      ypt(2) = amin
      chpt(2) = '.'
      ipt = 2
c                    find the largest allowed a
      aulim = 100.
      do 100 ik= 1, 2
         if (ik .eq. 1)  then
            kex = ke1cr
            zmid = xmidcr
            zdir = xdircr
         else
            if (ke2cr .eq. 0)  go to 100
            kex = ke2cr
            zmid = ymidcr
            zdir = ydircr
         endif
         if (nvarl(kex) .le. 1) go to 100
         if (zdir .eq. zero)      go to 100
         zlim = alim(kex)
         if (zdir .gt. zero) zlim = blim(kex)
         aulim = min(aulim,(zlim-zmid)/zdir)
  100 continue
c                  lsb = line search buffer
c          first point
      anext = 0.
      aopt = anext
      limset = .false.
        if (aulim .lt. aopt+tla)  limset = .true.
      call mneval(fcn,anext,fnext,ierev,futil)
c debug printout:
      if (ldebug) write (isyswr,'(a,i8,a,f10.5,a,2f10.5)')
     + ' mncros: calls=',nfcn,'   aim=',aim,'  f,a=',fnext,aopt
      if (ierev .gt. 0)  go to 900
      if (limset .and. fnext .le. aim)  go to 930
      ipt = ipt + 1
      xpt(ipt) = anext
      ypt(ipt) = fnext
      chpt(ipt)= charal(ipt:ipt)
      alsb(1) = anext
      flsb(1) = fnext
      fnext = max(fnext,aminsv+0.1*up)
      aopt =  dsqrt((up)/(fnext-aminsv)) - 1.0
      if (abs(fnext-aim) .lt. tlf)  go to 800
c
      if (aopt .lt. -0.5)  aopt = -0.5
      limset = .false.
      if (aopt .gt. aulim)  then
              aopt = aulim
              limset = .true.
      endif
      call mneval(fcn,aopt,fnext,ierev,futil)
c debug printout:
      if (ldebug) write (isyswr,'(a,i8,a,f10.5,a,2f10.5)')
     + ' mncros: calls=',nfcn,'   aim=',aim,'  f,a=',fnext,aopt
      if (ierev .gt. 0)  go to 900
      if (limset .and. fnext .le. aim)  go to 930
      alsb(2) = aopt
      ipt = ipt + 1
      xpt(ipt) = alsb(2)
      ypt(ipt) = fnext
      chpt(ipt)= charal(ipt:ipt)
      flsb(2) = fnext
      dfda = (flsb(2)-flsb(1))/ (alsb(2)-alsb(1))
      ilsb = 2
c                   dfda must be positive on the contour
      if (dfda .gt. zero)  go to 460
  300    call mnwarn('d',chere,'looking for slope of the right sign')
         maxlk = maxitr - ipt
         do 400 it= 1, maxlk
            alsb(1) = alsb(2)
            flsb(1) = flsb(2)
            aopt = alsb(1) + 0.2*real(it)
            limset = .false.
            if (aopt .gt. aulim)  then
              aopt = aulim
              limset = .true.
            endif
            call mneval(fcn,aopt,fnext,ierev,futil)
c debug printout:
      if (ldebug) write (isyswr,'(a,i8,a,f10.5,a,2f10.5)')
     + ' mncros: calls=',nfcn,'   aim=',aim,'  f,a=',fnext,aopt
            if (ierev .gt. 0)  go to 900
            if (limset .and. fnext .le. aim)  go to 930
               alsb(2) = aopt
               ipt = ipt + 1
               xpt(ipt) = alsb(2)
               ypt(ipt) = fnext
               chpt(ipt)= charal(ipt:ipt)
            flsb(2) = fnext
            dfda = (flsb(2)-flsb(1))/ (alsb(2)-alsb(1))
            if (dfda .gt. zero)  go to 450
  400    continue
         call mnwarn('w',chere,'cannot find slope of the right sign')
         go to 950
  450    continue
c                    we have two points with the right slope
  460 aopt = alsb(2) + (aim-flsb(2))/dfda
      if (min(abs(aopt-alsb(1)),abs(aopt-alsb(2))).lt. tla) go to 800
      if (ipt .ge. maxitr)  go to 950
      bmin = min(alsb(1),alsb(2)) - 1.0
      if (aopt .lt. bmin)  aopt = bmin
      bmax = max(alsb(1),alsb(2)) + 1.0
      if (aopt .gt. bmax)  aopt = bmax
c                    try a third point
      call mneval(fcn,aopt,fnext,ierev,futil)
c debug printout:
      if (ldebug) write (isyswr,'(a,i8,a,f10.5,a,2f10.5)')
     + ' mncros: calls=',nfcn,'   aim=',aim,'  f,a=',fnext,aopt
      if (ierev .gt. 0)  go to 900
      alsb(3) = aopt
      ipt = ipt + 1
      xpt(ipt) = alsb(3)
      ypt(ipt) = fnext
      chpt(ipt)= charal(ipt:ipt)
      flsb(3) = fnext
      inew = 3
c                now we have three points, ask how many <aim
      ecarmn = abs(fnext-aim)
      ibest = 3
      ecarmx = 0.
      noless = 0
      do 480 i= 1, 3
         ecart = abs(flsb(i) - aim)
         if (ecart .gt. ecarmx) then
            ecarmx = ecart
            iworst = i
         endif
         if (ecart .lt. ecarmn) then
            ecarmn = ecart
            ibest = i
         endif
         if (flsb(i) .lt. aim) noless = noless + 1
  480 continue
c           if at least one on each side of aim, fit a parabola
      if (noless.eq.1 .or. noless.eq.2) go to 500
c           if all three are above aim, third must be closest to aim
      if (noless .eq. 0 .and. ibest .ne. 3)  go to 950
c           if all three below, and third is not best, then slope
c             has again gone negative, look for positive slope.
      if (noless .eq. 3 .and. ibest .ne. 3) then
          alsb(2) = alsb(3)
          flsb(2) = flsb(3)
          go to 300
      endif
c           in other cases, new straight line thru last two points
      alsb(iworst) = alsb(3)
      flsb(iworst) = flsb(3)
      dfda = (flsb(2)-flsb(1))/ (alsb(2)-alsb(1))
      go to 460
c                parabola fit
  500 call mnpfit(alsb,flsb,3,coeff,sdev)
      if (coeff(3) .le. zero)  call mnwarn ('d',chere,
     +             'curvature is negative near contour line.')
      determ =  coeff(2)**2 - 4.*coeff(3)*(coeff(1)-aim)
      if (determ .le. zero)   then
          call mnwarn('d',chere,'problem 2, impossible determinant')
          go to 950
      endif
c                find which root is the right one
      rt = dsqrt(determ)
      x1 = (-coeff(2) + rt)/(2.*coeff(3))
      x2 = (-coeff(2) - rt)/(2.*coeff(3))
      s1 = coeff(2) + 2.*x1*coeff(3)
      s2 = coeff(2) + 2.*x2*coeff(3)
      if (s1*s2 .gt. zero) write (isyswr,'(a)') ' mncontour problem 1'
      aopt = x1
      if (s2 .gt. zero)  aopt = x2
      if (abs(aopt-alsb(inew)) .lt. tla)  go to 800
c                  evaluate function at parabolic optimum
      if (ipt .ge. maxitr)  go to 950
      limset = .false.
      if (aopt .gt. aulim)  then
              aopt = aulim
              limset = .true.
      endif
      call mneval(fcn,aopt,fnext,ierev,futil)
c debug printout:
      if (ldebug) write (isyswr,'(a,i8,a,f10.5,a,2f10.5)')
     + ' mncros: calls=',nfcn,'   aim=',aim,'  f,a=',fnext,aopt
      if (ierev .gt. 0)  go to 900
      if (limset .and. fnext .le. aim)  go to 930
      ipt = ipt + 1
      xpt(ipt) = aopt
      ypt(ipt) = fnext
      chpt(ipt)= charal(ipt:ipt)
c                replace unneeded point by new one
c            find nearest, farthest, (and hence middle) points,
      inear = 1
      anear = alsb(1)
      ifar = 1
      afar = alsb(1)
      do 620 i= 1, 3
      if (alsb(i) .lt. anear) then
         anear = alsb(i)
         inear = i
      endif
      if (alsb(i) .gt. afar)  then
         afar = alsb(i)
         ifar = i
      endif
  620 continue
      imid = 6 - inear - ifar
      fdist = flsb(imid)-aim
      if (fdist*(flsb(inear)-aim) .gt. zero) then
         inew = inear
      else
         inew = ifar
      endif
      alsb(inew) = aopt
      flsb(inew) = fnext
      go to 500
c       contour has been located, return point to mncont or minos
  800 continue
      iercr = 0
      go to 1000
c                error in the minimization
  900 if (ierev .eq. 1)  go to 940
      go to 950
c                parameter up against limit
  930 iercr = 1
      go to 1000
c                too many calls to fcn
  940 iercr = 2
      go to 1000
c                cannot find next point
  950 iercr = 3
c                in any case
 1000 continue
      if (ldebug) then
         itoohi = 0
         do 1100 i= 1, ipt
         if (ypt(i) .gt. aim+up) then
            ypt(i) = aim+up
            chpt(i) = '+'
            itoohi = 1
         endif
 1100    continue
         chsign = 'posi'
         if (xdircr .lt. zero)  chsign = 'nega'
         if (ke2cr .eq. 0)  write (isyswr, '(2x,a,a,i3)')
     +            chsign,'tive minos error, parameter ',ke1cr
         if (itoohi .eq. 1)  write (isyswr, '(10x,a)')
     +            'points labelled "+" were too high to plot.'
         if (iercr .eq. 1) write (isyswr,'(10x,a)')
     +            'rightmost point is up against limit.'
         call mnplot(xpt,ypt,chpt,ipt,isyswr,npagwd,npagln)
      endif
      return
      end
cdeck  id>, mncuve. 
      subroutine mncuve(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        makes sure that the current point is a local
cc        minimum and that the error matrix exists,
cc        or at least something good enough for minos and mncont
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      if (isw(4) .lt. 1) then
          write (isyswr,'(/a,a)')
     +    ' function must be minimized before calling ',cfrom
          apsi = epsi
          call mnmigr(fcn,futil)
      endif
      if (isw(2) .lt. 3)  then
         call mnhess(fcn,futil)
         if (isw(2) .lt. 1)  then
            call mnwarn('w',cfrom,'no error matrix.  will improvise.')
            do 555 i=1,npar
              ndex = i*(i-1)/2
              do 554 j=1,i-1
              ndex = ndex + 1
  554         vhmat(ndex) = 0.
            ndex = ndex + 1
            if (g2(i) .le. zero)  then
              wint = werr(i)
              iext = nexofi(i)
              if (nvarl(iext) .gt. 1) then
                 call mndxdi(x(i),i,dxdi)
                 if (abs(dxdi) .lt. .001) then
                    wint = .01
                 else
                    wint = wint/abs(dxdi)
                 endif
              endif
              g2(i) = up/wint**2
            endif
            vhmat(ndex) = 2./g2(i)
  555       continue
            isw(2) = 1
            dcovar = 1.
         else
           call mnwerr
         endif
      endif
      return
      end
cdeck  id>, mnderi. 
      subroutine mnderi(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        calculates the first derivatives of fcn (grd),
cc        either by finite differences or by transforming the user-
cc        supplied derivatives to internal coordinates,
cc        according to whether isw(3) is zero or one.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      logical ldebug
      character cbf1*22
      nparx = npar
      ldebug = (idbg(2) .ge. 1)
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      if (isw(3) .eq. 1)  go to 100
      if (ldebug) then
c                       make sure starting at the right place
        call mninex(x)
        nparx = npar
        call fcn(nparx,gin,fs1,u,4,futil)
        nfcn = nfcn + 1
        if (fs1 .ne. amin) then
           df = amin - fs1
           write (cbf1(1:12),'(g12.3)') df
           call mnwarn('d','mnderi',
     +         'function value differs from amin by '//cbf1(1:12) )
           amin = fs1
        endif
          write
     +   (isyswr,'(/''  first derivative debug printout.  mnderi''/
     +   '' par    deriv     step      minstep   optstep '',
     +   '' d1-d2    2nd drv'')')
      endif
      dfmin = 8. * epsma2*(abs(amin)+up)
      if (istrat .le. 0) then
         ncyc = 2
         tlrstp = 0.5
         tlrgrd = 0.1
      else if (istrat .eq. 1) then
         ncyc = 3
         tlrstp = 0.3
         tlrgrd = 0.05
      else
         ncyc = 5
         tlrstp = 0.1
         tlrgrd = 0.02
      endif
c                                loop over variable parameters
      do 60  i=1,npar
      epspri = epsma2 + abs(grd(i)*epsma2)
c         two-point derivatives always assumed necessary
c         maximum number of cycles over step size depends on strategy
      xtf = x(i)
      stepb4 = 0.
c                               loop as little as possible here!
      do 45 icyc= 1, ncyc
c                 ........ theoretically best step
      optstp = dsqrt(dfmin/(abs(g2(i))+epspri))
c                     step cannot decrease by more than a factor of ten
      step = max(optstp, abs(0.1*gstep(i)))
c                 but if parameter has limits, max step size = 0.5
      if (gstep(i).lt.zero .and. step.gt.0.5)  step=0.5
c                 and not more than ten times the previous step
      stpmax = 10.*abs(gstep(i))
      if (step .gt. stpmax)  step = stpmax
c                 minimum step size allowed by machine precision
      stpmin = 8. * abs(epsma2*x(i))
      if (step .lt. stpmin)  step = stpmin
c                 end of iterations if step change less than factor 2
      if (abs((step-stepb4)/step) .lt. tlrstp)  go to 50
c         take step positive
      gstep(i) = sign(step, gstep(i))
      stepb4 = step
      x(i) = xtf + step
      call mninex(x)
      call fcn(nparx,gin,fs1,u,4,futil)
      nfcn=nfcn+1
c         take step negative
      x(i) = xtf - step
      call mninex(x)
      call fcn(nparx,gin,fs2,u,4,futil)
      nfcn=nfcn+1
      grbfor = grd(i)
      grd(i) = (fs1-fs2)/(2.0*step)
      g2(i) = (fs1+fs2-2.0*amin)/(step**2)
      x(i) = xtf
      if (ldebug) then
         d1d2 = (fs1+fs2-2.0*amin)/step
         write (isyswr,41) i,grd(i),step,stpmin,optstp,d1d2,g2(i)
   41    format (i4,2g11.3,5g10.2)
      endif
c         see if another iteration is necessary
      if (abs(grbfor-grd(i))/(abs(grd(i))+dfmin/step) .lt. tlrgrd)
     +        go to 50
   45 continue
c                           end of icyc loop. too many iterations
      if (ncyc .eq. 1)  go to 50
         write (cbf1,'(2e11.3)')  grd(i),grbfor
         call mnwarn('d','mnderi',
     +         'first derivative not converged. '//cbf1)
   50 continue
c
   60 continue
      call mninex(x)
      return
c                                        .  derivatives calc by fcn
  100 do 150 iint= 1, npar
      iext = nexofi(iint)
      if (nvarl(iext) .gt. 1)  go to 120
      grd(iint) = gin(iext)
      go to 150
  120 dd = (blim(iext)-alim(iext))*0.5 *dcos(x(iint))
      grd(iint) = gin(iext)*dd
  150 continue
  200 return
      end
cdeck  id>, mndxdi. 
      subroutine mndxdi(pint,ipar,dxdi)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        calculates the transformation factor between external and
cc        internal parameter values.     this factor is one for
cc        parameters which are not limited.     called from mnemat.
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      i = nexofi(ipar)
      dxdi = 1.0
      if (nvarl(i) .gt. 1)
     +      dxdi = 0.5 *abs((blim(i)-alim(i)) * dcos(pint))
      return
      end
cdeck  id>, mneig.  
      subroutine mneig(a,ndima,n,mits,work,precis,ifault)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
c
      dimension a(ndima,*),work(*)
      data zero,one,two/0.0,1.0,2.0/
      data tol/1.0e-35/
c          precis is the machine precision epsmac
      ifault = 1
c
      i = n
      do 70 i1 = 2,n
      l = i-2
      f = a(i,i-1)
      gl = zero
c
      if(l .lt. 1) go to 25
c
      do 20 k = 1,l
   20 gl = gl+a(i,k)**2
   25 h = gl + f**2
c
      if(gl .gt. tol) go to 30
c
      work(i) = zero
      work(n+i) = f
      go to 65
   30 l = l+1
c
      gl = dsqrt(h)
c
      if(f .ge. zero) gl = -gl
c
      work(n+i) = gl
      h = h-f*gl
      a(i,i-1) = f-gl
      f = zero
      do 50 j = 1,l
      a(j,i) = a(i,j)/h
      gl = zero
      do 40 k = 1,j
   40 gl = gl+a(j,k)*a(i,k)
c
      if(j .ge. l) go to 47
c
      j1 = j+1
      do 45 k = j1,l
   45 gl = gl+a(k,j)*a(i,k)
   47 work(n+j) = gl/h
      f = f+gl*a(j,i)
   50 continue
      hh = f/(h+h)
      do 60 j = 1,l
      f = a(i,j)
      gl = work(n+j)-hh*f
      work(n+j) = gl
      do 60 k = 1,j
      a(j,k) = a(j,k)-f*work(n+k)-gl*a(i,k)
   60 continue
      work(i) = h
   65 i = i-1
   70 continue
      work(1) = zero
      work(n+1) = zero
      do 110 i = 1,n
      l = i-1
c
      if(work(i) .eq. zero .or. l .eq. 0) go to 100
c
      do 90 j = 1,l
      gl = zero
      do 80 k = 1,l
   80 gl = gl+a(i,k)*a(k,j)
      do 90 k = 1,l
      a(k,j) = a(k,j)-gl*a(k,i)
   90 continue
  100 work(i) = a(i,i)
      a(i,i) = one
c
      if(l .eq. 0) go to 110
c
      do 105 j = 1,l
      a(i,j) = zero
      a(j,i) = zero
  105 continue
  110 continue
c
c
      n1 = n-1
      do 130 i = 2,n
      i0 = n+i-1
  130 work(i0) = work(i0+1)
      work(n+n) = zero
      b = zero
      f = zero
      do 210 l = 1,n
      j = 0
      h = precis*(abs(work(l))+abs(work(n+l)))
c
      if(b .lt. h) b = h
c
      do 140 m1 = l,n
      m = m1
c
      if(abs(work(n+m)) .le. b) go to 150
c
  140 continue
c
  150 if(m .eq. l) go to 205
c
  160 if(j .eq. mits) return
c
      j = j+1
      pt = (work(l+1)-work(l))/(two*work(n+l))
      r = dsqrt(pt*pt+one)
      pr = pt+r
c
      if(pt .lt. zero) pr=pt-r
c
      h = work(l)-work(n+l)/pr
      do 170 i=l,n
  170 work(i) = work(i)-h
      f = f+h
      pt = work(m)
      c = one
      s = zero
      m1 = m-1
      i = m
      do 200 i1 = l,m1
      j = i
      i = i-1
      gl = c*work(n+i)
      h = c*pt
c
      if(abs(pt) .ge. abs(work(n+i))) go to 180
c
      c = pt/work(n+i)
      r = dsqrt(c*c+one)
      work(n+j) = s*work(n+i)*r
      s = one/r
      c = c/r
      go to 190
  180 c = work(n+i)/pt
      r = dsqrt(c*c+one)
      work(n+j) = s*pt*r
      s = c/r
      c = one/r
  190 pt = c*work(i)-s*gl
      work(j) = h+s*(c*gl+s*work(i))
      do 200 k = 1,n
      h = a(k,j)
      a(k,j) = s*a(k,i)+c*h
      a(k,i) = c*a(k,i)-s*h
  200 continue
      work(n+l) = s*pt
      work(l) = c*pt
c
      if(abs(work(n+l)) .gt. b) go to 160
c
  205 work(l) = work(l)+f
  210 continue
      do 240 i=1,n1
      k = i
      pt = work(i)
      i1 = i+1
      do 220 j = i1,n
c
      if(work(j) .ge. pt) go to 220
c
      k = j
      pt = work(j)
  220 continue
c
      if(k .eq. i) go to 240
c
      work(k) = work(i)
      work(i) = pt
      do 230 j=1,n
      pt = a(j,i)
      a(j,i) = a(j,k)
      a(j,k) = pt
  230 continue
  240 continue
      ifault = 0
c
      return
      end
cdeck  id>, mnemat. 
      subroutine mnemat(emat,ndim)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
      dimension emat(ndim,ndim)
cc        calculates the external error matrix from the internal
cc        to be called by user, who must dimension emat at (ndim,ndim)
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      if (isw(2) .lt. 1)  return
      if (isw(5) .ge. 2)  write (isyswr,'(/a,i4,a,i3,a,g10.2)')
     +    ' external error matrix.    ndim=',ndim,'    npar=',npar,
     +    '    err def=',up
c                    size of matrix to be printed
      npard = npar
      if (ndim .lt. npar)  then
        npard = ndim
        if (isw(5) .ge. 0) write (isyswr,'(a,a)') ' user-dimensioned ',
     +      ' array emat not big enough. reduced matrix calculated.'
      endif
c                 nperln is the number of elements that fit on one line
      nperln = (npagwd-5)/10
      nperln = min(nperln,13)
      if (isw(5).ge. 1 .and. npard.gt.nperln)  write (isyswr,'(a)')
     +     ' elements above diagonal are not printed.'
c                 i counts the rows of the matrix
      do 110 i= 1, npard
         call mndxdi(x(i),i,dxdi)
         kga = i*(i-1)/2
         do 100 j= 1, i
            call mndxdi(x(j),j,dxdj)
            kgb = kga + j
            emat(i,j) = dxdi * vhmat(kgb) * dxdj * up
            emat(j,i) = emat(i,j)
  100    continue
  110 continue
c                    iz is number of columns to be printed in row i
      if (isw(5) .ge. 2)  then
      do 160 i= 1, npard
         iz = npard
         if (npard .ge. nperln)  iz = i
         do 150 k= 1, iz, nperln
           k2 = k + nperln - 1
           if (k2 .gt. iz)  k2=iz
           write (isyswr,'(1x,13e10.3)')  (emat(i,kk),kk=k,k2)
  150    continue
  160 continue
      endif
      return
      end
cdeck  id>, mnerrs. 
      subroutine mnerrs(number,eplus,eminus,eparab,gcc)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc    called by user, utility routine to get minos errors
cc    if number is positive, then it is external parameter number,
cc                  if negative, it is -internal number.
cc    values returned by mnerrs:
cc       eplus, eminus are minos errors of parameter number,
cc       eparab is 'parabolic' error (from error matrix).
cc                 (errors not calculated are set = 0.)
cc       gcc is global correlation coefficient from error matrix
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c
      iex = number
      if (number .lt. 0)  then
         iin = -number
         if (iin .gt. npar)  go to 900
         iex = nexofi(iin)
      endif
      if (iex .gt. nu .or. iex .le. 0)  go to 900
      iin = niofex(iex)
      if (iin .le. 0)  go to 900
c             iex is external number, iin is internal number
      eplus = erp(iin)
        if (eplus.eq.undefi)  eplus=0.
      eminus= ern(iin)
        if (eminus.eq.undefi) eminus=0.
      call mndxdi(x(iin),iin,dxdi)
      ndiag = iin*(iin+1)/2
      eparab = abs(dxdi*dsqrt(abs(up*vhmat(ndiag))))
c              global correlation coefficient
      gcc = 0.
      if (isw(2) .lt. 2)  go to 990
      gcc = globcc(iin)
      go to 990
c                  error.  parameter number not valid
  900 eplus = 0.
      eminus = 0.
      eparab = 0.
      gcc = 0.
  990 return
      end
cdeck  id>, mneval. 
      subroutine mneval(fcn,anext,fnext,ierev,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc      evaluates the function being analyzed by mncros, which is
cc      generally the minimum of fcn with respect to all remaining
cc      variable parameters.  common block /mn7xcr/ contains the
cc      data necessary to know the values of u(ke1cr) and u(ke2cr)
cc      to be used, namely     u(ke1cr) = xmidcr + anext*xdircr
cc      and (if ke2cr .ne. 0)  u(ke2cr) = ymidcr + anext*ydircr
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
cc
      external fcn,futil
                          u(ke1cr) = xmidcr + anext*xdircr
      if ( ke2cr .ne. 0)  u(ke2cr) = ymidcr + anext*ydircr
      call mninex(x)
      nparx = npar
      call fcn(nparx,gin,fnext,u,4,futil)
      nfcn = nfcn + 1
      ierev = 0
      if (npar .gt. 0)  then
         itaur = 1
         amin = fnext
         isw(1) = 0
         call mnmigr(fcn,futil)
         itaur = 0
         fnext = amin
         if (isw(1) .ge. 1)  ierev = 1
         if (isw(4) .lt. 1)  ierev = 2
      endif
      return
      end
cdeck  id>, mnexcm. 
      subroutine mnexcm(fcn,comand,plist,llist,ierflg,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        interprets a command and takes appropriate action,
cc        either directly by skipping to the corresponding code in
cc        mnexcm, or by setting up a call to a subroutine
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      character*(*) comand
c   cannot say dimension plist(llist) since llist can be =0.
      dimension plist(*)
      parameter (mxpt=101)
      dimension xptu(mxpt), yptu(mxpt)
c  alphabetical order of command names!
      dimension    isort(40)
      character*10 cname(40), cneway, chwhy*18, c26*30, cvblnk*2
      logical ltofix, lfixed, lfreed
c  recognized minuit commands:
      data cname( 1) / 'minimize  ' /
      data cname( 2) / 'seek      ' /
      data cname( 3) / 'simplex   ' /
      data cname( 4) / 'migrad    ' /
      data cname( 5) / 'minos     ' /
      data cname( 6) / 'set xxx   ' /
      data cname( 7) / 'show xxx  ' /
      data cname( 8) / 'top of pag' /
      data cname( 9) / 'fix       ' /
      data cname(10) / 'restore   ' /
      data cname(11) / 'release   ' /
      data cname(12) / 'scan      ' /
      data cname(13) / 'contour   ' /
      data cname(14) / 'hesse     ' /
      data cname(15) / 'save      ' /
      data cname(16) / 'improve   ' /
      data cname(17) / 'call fcn  ' /
      data cname(18) / 'standard  ' /
      data cname(19) / 'end       ' /
      data cname(20) / 'exit      ' /
      data cname(21) / 'return    ' /
      data cname(22) / 'clear     ' /
      data cname(23) / 'help      ' /
      data cname(24) / 'mncontour ' /
      data cname(25) / 'stop      ' /
      data cname(26) / 'jump      ' /
       data nname/26/
      data cname(27) / '          ' /
      data cname(28) / '          ' /
      data cname(29) / '          ' /
      data cname(30) / '          ' /
      data cname(31) / '          ' /
      data cname(32) / '          ' /
      data cname(33) / '          ' /
c  obsolete commands:
      data cname(34) / 'covariance' /
      data cname(35) / 'printout  ' /
      data cname(36) / 'gradient  ' /
      data cname(37) / 'matout    ' /
      data cname(38) / 'error def ' /
      data cname(39) / 'limits    ' /
      data cname(40) / 'punch     ' /
      data nntot/40/
c                  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
      data isort/ 17,22,13,19,20, 9,23,14,16,26, 4, 1, 5,24,11,
     +            10,21,15,12, 2, 6, 3, 7,18,25, 8, 1, 1, 1, 1,
     + 1,1,1,1,1,1,1,1,1,1/
c
      lk = len(comand)
      if (lk .gt. maxcwd) lk=maxcwd
      cword = comand(1:lk)
c           copy the first maxp arguments into common (word7), making
c           sure that word7(1)=0. if llist=0
      do 20 iw= 1, maxp
      word7(iw) = zero
      if (iw .le. llist) word7(iw) = plist(iw)
   20 continue
      icomnd = icomnd + 1
      nfcnlc = nfcn
      if (cword(1:7).ne.'set pri' .or. word7(1).ge.0.)  then
        if (isw(5) .ge. 0) then
         lnow = llist
         if (lnow .gt. 4)  lnow=4
         write (isyswr,25) icomnd,cword(1:lk),(plist(i),i=1,lnow)
   25    format (1h ,10(1h*)/' **',i5,' **',a,4g12.4)
         if (llist .gt. lnow) then
           write (cvblnk,'(i2)') lk
           c26 = '(11h **********,'//cvblnk//'x,4g12.4)'
           write (isyswr,c26) (plist(i),i=lnow+1,llist)
         endif
         write (isyswr, '(1h ,10(1h*))' )
        endif
      endif
      nfcnmx = word7(1)
      if (nfcnmx .le. 0)  nfcnmx = 200 + 100*npar + 5*npar**2
      epsi = word7(2)
      if (epsi .le. zero)  epsi = 0.1 * up
      lnewmn = .false.
      lphead = .true.
      isw(1) = 0
      ierflg = 0
c                look for command in list cname . . . . . . . . . .
      do 80 i= 1, nntot
      if (cword(1:3) .eq. cname(i)(1:3))  go to 90
   80 continue
      write (isyswr,'(11x,''unknown command ignored:'',a)') comand
      ierflg = 2
      go to 5000
c                normal case: recognized minuit command . . . . . . .
   90 continue
      if (cword(1:4) .eq. 'mino') i = 5
      if (i.ne.6 .and. i.ne.7 .and. i.ne.8 .and. i.ne.23)  then
         cfrom = cname(i)
         nfcnfr = nfcn
      endif
c              1    2    3    4    5    6    7    8    9   10
      go to ( 400, 200, 300, 400, 500, 700, 700, 800, 900,1000,
     1       1100,1200,1300,1400,1500,1600,1700,1800,1900,1900,
     2       1900,2200,2300,2400,1900,2600,3300,3300,3300,3300,
     3       3300,3300,3300,3400,3500,3600,3700,3800,3900,4000) , i
c                                        . . . . . . . . . . seek
  200 call mnseek(fcn,futil)
      go to 5000
c                                        . . . . . . . . . . simplex
  300 call mnsimp(fcn,futil)
      go to 5000
c                                        . . . . . . migrad, minimize
  400 continue
      nf = nfcn
      apsi = epsi
      call mnmigr(fcn,futil)
      call mnwerr
      if (isw(4) .ge. 1)         go to 5000
      if (isw(1) .eq. 1)         go to 5000
      if (cword(1:3) .eq. 'mig') go to 5000
      nfcnmx = nfcnmx + nf - nfcn
      nf = nfcn
      call mnsimp(fcn,futil)
      if (isw(1) .eq. 1)  go to 5000
      nfcnmx = nfcnmx + nf - nfcn
      call mnmigr(fcn,futil)
      call mnwerr
      go to 5000
c                                        . . . . . . . . . . minos
  500 continue
      nsuper = nfcn + 2*(npar+1)*nfcnmx
c          possible loop over new minima
      epsi = 0.1 * up
  510 continue
      call mncuve(fcn,futil)
      call mnmnos(fcn,futil)
      if (.not. lnewmn)  go to 5000
      call mnrset(0)
      call mnmigr(fcn,futil)
      call mnwerr
      if (nfcn .lt. nsuper)  go to 510
      write (isyswr,'(/'' too many function calls. minos gives up''/)')
      ierflg = 1
      go to 5000
c                                        . . . . . . . . . .set, show
  700 call mnset(fcn,futil)
      go to 5000
c                                        . . . . . . . . . . top of page
  800 continue
      write (isyswr,'(1h1)')
      go to 5000
c                                        . . . . . . . . . . fix
  900 ltofix = .true.
c                                        . . (also release) ....
  901 continue
      lfreed = .false.
      lfixed = .false.
      if (llist .eq. 0)  then
         write (isyswr,'(a,a)') cword,':  no parameters requested '
         go to 5000
      endif
      do 950 ilist= 1, llist
      iext = plist(ilist)
      chwhy = ' is undefined.'
      if (iext .le. 0)         go to 930
      if (iext .gt. nu)        go to 930
      if (nvarl(iext) .lt. 0)  go to 930
      chwhy = ' is constant.  '
      if (nvarl(iext) .eq. 0)  go to 930
      iint = niofex(iext)
      if (ltofix) then
         chwhy = ' already fixed.'
         if (iint .eq. 0)      go to 930
         call mnfixp(iint,ierr)
         if (ierr .eq. 0) then
            lfixed = .true.
         else
            ierflg = 1
         endif
      else
         chwhy = ' already variable.'
         if (iint .gt. 0)      go to 930
         krl = -iabs(iext)
         call mnfree(krl)
         lfreed = .true.
      endif
      go to 950
  930 write (isyswr,'(a,i4,a,a)') ' parameter',iext,chwhy,' ignored.'
  950 continue
      if (lfreed .or. lfixed)  call mnrset(0)
      if (lfreed)  then
          isw(2) = 0
          dcovar = 1.
          edm = bigedm
          isw(4) = 0
      endif
      call mnwerr
      if (isw(5) .gt. 1)  call mnprin(5,amin)
      go to 5000
c                                        . . . . . . . . . . restore
 1000 it = word7(1)
      if (it.gt.1 .or. it.lt.0)  go to 1005
      lfreed = (npfix .gt. 0)
      call mnfree(it)
      if (lfreed) then
         call mnrset(0)
         isw(2) = 0
         dcovar = 1.
         edm = bigedm
      endif
      go to 5000
 1005 write (isyswr,'(a,i4)') ' ignored.  unknown argument:',it
      go to 5000
c                                        . . . . . . . . . . release
 1100 ltofix = .false.
      go to 901
c                                       . . . . . . . . . . scan . . .
 1200 continue
      iext = word7(1)
      if (iext .le. 0)  go to 1210
      it2 = 0
      if (iext .le. nu)  it2 = niofex(iext)
      if (it2 .le. 0)  go to 1250
 1210 call mnscan(fcn,futil)
      go to 5000
 1250 write (isyswr,'(a,i4,a)') ' parameter',iext,' not variable.'
      go to 5000
c                                        . . . . . . . . . . contour
 1300 continue
      ke1 = word7(1)
      ke2 = word7(2)
      if (ke1 .eq. 0)  then
         if (npar .eq. 2)  then
            ke1 = nexofi(1)
            ke2 = nexofi(2)
         else
            write (isyswr,'(a,a)') cword,':  no parameters requested '
            go to 5000
         endif
      endif
      nfcnmx = 1000
      call mncntr(fcn,ke1,ke2,ierrf,futil)
      ierflg = ierrf
      go to 5000
c                                        . . . . . . . . . . hesse
 1400 continue
      call mnhess(fcn,futil)
      call mnwerr
      if (isw(5) .ge. 0)  call mnprin(2, amin)
      if (isw(5) .ge. 1)  call mnmatu(1)
      go to 5000
c                                        . . . . . . . . . . save
 1500 continue
      call mnsave
      go to 5000
c                                        . . . . . . . . . . improve
 1600 continue
      call mncuve(fcn,futil)
      call mnimpr(fcn,futil)
      if (lnewmn)  go to 400
      go to 5000
c                                        . . . . . . . . . . call fcn
 1700 iflag = word7(1)
      nparx = npar
      f = undefi
      call fcn(nparx,gin,f,u,iflag,futil)
      nfcn = nfcn + 1
      nowprt = 0
      if (f .ne. undefi)  then
         if (amin .eq. undefi)  then
             amin = f
             nowprt = 1
         else if (f .lt. amin)  then
             amin = f
             nowprt = 1
         endif
         if (isw(5).ge.0 .and. iflag.le.5 .and. nowprt.eq.1)
     +          call mnprin(5,amin)
         if (iflag .eq. 3)  fval3=f
      endif
      if (iflag .gt. 5)  call mnrset(1)
      go to 5000
c                                        . . . . . . . . . . standard
 1800 call stand
      go to 5000
c                                       . . . . . . . stop, end, exit
 1900 it = plist(1)
      if (fval3 .eq. amin .or. it .gt. 0)  go to 5000
      iflag = 3
      write (isyswr,'(/a/)') ' call to user function with iflag = 3'
      nparx = npar
      call fcn(nparx,gin,f,u,iflag,futil)
      nfcn = nfcn + 1
      go to 5000
c                                        . . . . . . . . . . clear
 2200 continue
      call mncler
      if (isw(5) .ge. 1)  write (isyswr,'(a)')
     + ' minuit memory cleared. no parameters now defined.'
      go to 5000
c                                        . . . . . . . . . . help
 2300 continue
      if (index(cword,'sho') .gt. 0)  go to 700
      if (index(cword,'set') .gt. 0)  go to 700
      write (isyswr,2301)  (cname(isort(i)),i=1,nname),'parameters'
 2301 format (' the commands recognized by minuit are:'/6(2x,a10))
      write (isyswr,'(a)') ' see also: help set and help show'
      go to 5000
c                                       . . . . . . . . . . mncontour
 2400 continue
      epsi = 0.05 * up
      ke1 = word7(1)
      ke2 = word7(2)
      if (ke1.eq.0 .and. npar.eq.2) then
         ke1 = nexofi(1)
         ke2 = nexofi(2)
         endif
      nptu = word7(3)
      if (nptu .le. 0)  nptu=20
      if (nptu .gt. mxpt)  nptu = mxpt
      nfcnmx =  100*(nptu+5)*(npar+1)
      call mncont(fcn,ke1,ke2,nptu,xptu,yptu,ierrf,futil)
      go to 5000
c                                      . . . . . . . . . . jump
 2600 continue
      step = word7(1)
      if (step .le. zero)  step = 2.
      rno = 0.
      izero = 0
      do 2620 i= 1, npar
        call mnrn15(rno,izero)
        rno = 2.0*rno - 1.0
 2620   x(i) = x(i) + rno*step*werr(i)
      call mninex(x)
      call mnamin(fcn,futil)
      call mnrset(0)
      go to 5000
c                                      . . . . . . . . . . blank line
 3300 continue
      write (isyswr,'(10x,a)') ' blank command ignored.'
      go to 5000
c  . . . . . . . . obsolete commands     . . . . . . . . . . . . . .
c                                      . . . . . . . . . . covariance
 3400 continue
      write (isyswr, '(a)') ' the "covariance" command is osbsolete.',
     + ' the covariance matrix is now saved in a different format',
     + ' with the "save" command and read in with:"set covariance"'
      go to 5000
c                                        . . . . . . . . . . printout
 3500 continue
      cneway = 'set print '
      go to 3100
c                                        . . . . . . . . . . gradient
 3600 continue
      cneway = 'set grad  '
      go to 3100
c                                        . . . . . . . . . . matout
 3700 continue
      cneway = 'show covar'
      go to 3100
c                                        . . . . . . . . . error def
 3800 continue
      cneway = 'set errdef'
      go to 3100
c                                        . . . . . . . . . . limits
 3900 continue
      cneway = 'set limits'
      go to 3100
c                                        . . . . . . . . . . punch
 4000 continue
      cneway = 'save      '
c                                ....... come from obsolete commands
 3100 write (isyswr, 3101) cword,cneway
 3101 format (' obsolete command:',1x,a10,5x,'please use:',1x,a10)
      cword = cneway
      if (cword .eq. 'save      ') go to 1500
      go to 700
c                                 . . . . . . . . . . . . . . . . . .
 5000 return
      end
cdeck  id>, mnexin. 
      subroutine mnexin(pint)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        transforms the external parameter values u to internal
cc        values in the dense array pint. subroutine mnpint is used.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension pint(*)
      limset = .false.
      do 100  iint= 1, npar
      iext = nexofi(iint)
      call mnpint(u(iext),iext,pinti)
      pint(iint) = pinti
  100 continue
      return
      end
cdeck  id>, mnfixp. 
      subroutine mnfixp(iint,ierr)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        removes parameter iint from the internal (variable) parameter
cc        list, and arranges the rest of the list to fill the hole.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension yy(mni)
c                           first see if it can be done
      ierr = 0
      if (iint.gt.npar .or. iint.le.0)  then
         ierr = 1
         write (isyswr,'(a,i4)')
     +       ' minuit error.  argument to mnfixp=',iint
         go to 300
      endif
      iext = nexofi(iint)
      if (npfix .ge. mni) then
         ierr = 1
         write (isyswr,'(a,i4,a,i4)') ' minuit cannot fix parameter',
     +   iext,' maximum number that can be fixed is',mni
         go to 300
      endif
c                           reduce number of variable parameters by one
      niofex(iext) = 0
      nold = npar
      npar = npar - 1
c                       save values in case parameter is later restored
      npfix = npfix + 1
      ipfix(npfix) = iext
      lc = iint
      xs(npfix) = x(lc)
      xts(npfix) = xt(lc)
      dirins(npfix) = werr(lc)
      grds(npfix) = grd(lc)
      g2s(npfix) = g2(lc)
      gsteps(npfix) = gstep(lc)
c                        shift values for other parameters to fill hole
      do 100  ik= iext+1, nu
         if  (niofex(ik) .gt. 0)  then
         lc = niofex(ik) - 1
         niofex(ik) = lc
         nexofi(lc) = ik
         x(lc)     = x(lc+1)
         xt(lc)    = xt(lc+1)
         dirin(lc) = dirin(lc+1)
         werr(lc)  = werr(lc+1)
         grd(lc)   = grd(lc+1)
         g2(lc)    = g2(lc+1)
         gstep(lc) = gstep(lc+1)
         endif
  100 continue
      if (isw(2) .le. 0)  go to 300
c                    remove one row and one column from variance matrix
      if (npar .le. 0)  go to 300
      do 260 i= 1, nold
      m = max(i,iint)
      n = min(i,iint)
      ndex = m*(m-1)/2 + n
  260 yy(i)=vhmat(ndex)
      yyover = 1.0/yy(iint)
      knew = 0
      kold = 0
      do 294 i= 1, nold
      do 292 j= 1, i
      kold = kold + 1
      if (j.eq.iint .or. i.eq.iint)  go to 292
      knew = knew + 1
      vhmat(knew) = vhmat(kold) - yy(j)*yy(i)*yyover
  292 continue
  294 continue
  300 return
      end
cdeck  id>, mnfree. 
      subroutine mnfree(k)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        restores one or more fixed parameter(s) to variable status
cc        by inserting it into the internal parameter list at the
cc        appropriate place.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c--       k = 0 means restore all parameters
c--       k = 1 means restore the last parameter fixed
c--       k = -i means restore external parameter i (if possible)
c--       iq = fix-location where internal parameters were stored
c--       ir = external number of parameter being restored
c--       is = internal number of parameter being restored
      if (k .gt. 1)  write (isyswr,510)
      if (npfix .lt. 1)  write (isyswr,500)
      if (k.eq.1 .or. k.eq.0)  go to 40
c                   release parameter with specified external number
      ka = iabs(k)
      if (niofex(ka) .eq. 0)  go to 15
      write (isyswr,540)
  540 format (' ignored.  parameter specified is already variable.')
      return
   15 if (npfix .lt. 1)  go to 21
      do 20 ik= 1, npfix
      if (ipfix(ik) .eq. ka)  go to 24
   20 continue
   21 write (isyswr,530) ka
  530 format (' parameter',i4,' not fixed.  cannot be released.')
      return
   24 if (ik .eq. npfix)  go to 40
c                   move specified parameter to end of list
      ipsav = ka
      xv = xs(ik)
      xtv = xts(ik)
      dirinv = dirins(ik)
      grdv = grds(ik)
      g2v = g2s(ik)
      gstepv = gsteps(ik)
         do 30 i= ik+1,npfix
         ipfix(i-1) = ipfix(i)
         xs(i-1) = xs(i)
         xts(i-1) = xts(i)
         dirins(i-1) = dirins(i)
         grds(i-1) = grds(i)
         g2s(i-1) = g2s(i)
         gsteps(i-1) = gsteps(i)
   30    continue
      ipfix(npfix) = ipsav
      xs(npfix) = xv
      xts(npfix) = xtv
      dirins(npfix) = dirinv
      grds(npfix) = grdv
      g2s(npfix) = g2v
      gsteps(npfix) = gstepv
c                restore last parameter in fixed list  -- ipfix(npfix)
   40 continue
      if (npfix .lt. 1)  go to 300
      ir = ipfix(npfix)
      is = 0
      do 100 ik= nu, ir, -1
        if (niofex(ik) .gt. 0) then
         lc = niofex(ik) + 1
         is = lc - 1
         niofex(ik) = lc
         nexofi(lc) = ik
         x(lc)     = x(lc-1)
         xt(lc)    = xt(lc-1)
         dirin(lc) = dirin(lc-1)
         werr(lc)  = werr(lc-1)
         grd(lc)   = grd(lc-1)
         g2(lc)    = g2(lc-1)
         gstep(lc) = gstep(lc-1)
        endif
  100 continue
      npar = npar + 1
      if (is .eq. 0)   is = npar
      niofex(ir) = is
      nexofi(is) = ir
      iq = npfix
      x(is) = xs(iq)
      xt(is) = xts(iq)
      dirin(is) = dirins(iq)
      werr(is)  = dirins(iq)
      grd(is) = grds(iq)
      g2(is) = g2s(iq)
      gstep(is) = gsteps(iq)
      npfix = npfix - 1
      isw(2) = 0
      dcovar = 1.
      if (itaur .lt. 1)  write(isyswr,520) ir,cpnam(ir)
      if (k.eq.0)  go to 40
  300 continue
c         if different from internal, external values are taken
      call mnexin(x)
  400 return
  500 format (' call to mnfree ignored.  there are no fixed pa',
     + 'rameters'/)
  510 format (' call to mnfree ignored.  argument greater than one'/)
  520 format (20x, 9hparameter,i4,2h, ,a10,' restored to variable.')
      end
cdeck  id>, mngrad. 
      subroutine mngrad(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       called from mnset
cc       interprets the set grad command, which informs minuit whether
cc       the first derivatives of fcn will be calculated by the user
cc       inside fcn.  it can check the user's derivative calculation
cc       by comparing it with a finite difference approximation.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c
      external fcn,futil
      character*4 cgood,cbad,cnone,cwd
      logical lnone
      dimension gf(mni)
      parameter (cgood='good',cbad=' bad',cnone='none')
c
      isw(3) = 1
      nparx = npar
      if (word7(1) .gt. zero)  go to 2000
c                  get user-calculated first derivatives from fcn
      do 30 i= 1, nu
   30 gin(i) = undefi
      call mninex(x)
      call fcn(nparx,gin,fzero,u,2,futil)
      nfcn = nfcn + 1
      call mnderi(fcn,futil)
      do 40 i= 1, npar
   40 gf(i) = grd(i)
c                    get minuit-calculated first derivatives
      isw(3) = 0
      istsav = istrat
      istrat = 2
      call mnhes1(fcn,futil)
      istrat = istsav
      write (isyswr,51)
   51 format(/' check of gradient calculation in fcn'/12x,'parameter',
     + 6x,9hg(in fcn) ,3x,9hg(minuit) ,2x,'dg(minuit)',3x,9hagreement)
      isw(3) = 1
      lnone = .false.
      do 100 lc = 1, npar
      i = nexofi(lc)
      cwd = cgood
      err = dgrd(lc)
      if (abs(gf(lc)-grd(lc)) .gt. err)  cwd = cbad
      if (gin(i) .eq. undefi)  then
          cwd = cnone
          lnone = .true.
          gf(lc) = 0.
          endif
      if (cwd .ne. cgood)  isw(3) = 0
      write (isyswr,99) i,cpnam(i),gf(lc),grd(lc),err,cwd
   99 format (7x,i5,2x ,a10,3e12.4,4x ,a4)
  100 continue
      if (lnone) write (isyswr,'(a)')
     +  '  agreement=none  means fcn did not calculate the derivative'
      if (isw(3) .eq. 0)  write (isyswr,1003)
 1003 format(/' minuit does not accept derivative calculations by fcn'/
     + ' to force acceptance, enter "set grad    1"'/)
c
 2000 continue
      return
      end
cdeck  id>, mnhess. 
      subroutine mnhess(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        calculates the full second-derivative matrix of fcn
cc        by taking finite differences. when calculating diagonal
cc        elements, it may iterate so that step size is nearly that
cc        which gives function change= up/10. the first derivatives
cc        of course come as a free side effect, but with a smaller
cc        step size in order to obtain a known accuracy.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension yy(mni)
      logical ldebug
      character cbf1*22
c
      ldebug = (idbg(3) .ge. 1)
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      if (istrat .le. 0) then
         ncyc = 3
         tlrstp = 0.5
         tlrg2  = 0.1
      else if (istrat .eq. 1) then
         ncyc = 5
         tlrstp = 0.3
         tlrg2  = 0.05
      else
         ncyc = 7
         tlrstp = 0.1
         tlrg2  = 0.02
      endif
      if (isw(5).ge.2 .or. ldebug)  write (isyswr,'(a)')
     +   '   start covariance matrix calculation.'
      cfrom = 'hesse   '
      nfcnfr = nfcn
      cstatu= 'ok        '
      npard = npar
c                 make sure starting at the right place
      call mninex(x)
      nparx = npar
      call fcn(nparx,gin,fs1,u,4,futil)
      nfcn = nfcn + 1
      if (fs1 .ne. amin) then
         df = amin - fs1
         write (cbf1(1:12),'(g12.3)') df
         call mnwarn('d','mnhess',
     +       'function value differs from amin by '//cbf1(1:12) )
      endif
      amin = fs1
      if (ldebug) write (isyswr,'(a,a)') ' par d   gstep          ',
     +' d          g2         grd         sag    '
c                                        . . . . . . diagonal elements .
c         isw(2) = 1 if approx, 2 if not posdef, 3 if ok
c         aimsag is the sagitta we are aiming for in second deriv calc.
      aimsag = dsqrt(epsma2)*(abs(amin)+up)
c         zero the second derivative matrix
      npar2 = npar*(npar+1)/2
      do 10 i= 1,npar2
   10 vhmat(i) = 0.
c
c         loop over variable parameters for second derivatives
      idrv = 2
      do 100 id= 1, npard
      i = id + npar - npard
      if (g2(i) .eq. zero) then
        call mnwarn('d','mnhess',
     +        'a second derivative is zero on entering.')
        wint = werr(i)
        iext = nexofi(i)
        if (nvarl(iext) .gt. 1) then
           call mndxdi(x(i),i,dxdi)
           if (abs(dxdi) .lt. .001) then
              wint = .01
           else
              wint = wint/abs(dxdi)
           endif
        endif
        g2(i) = up/wint**2
      endif
      xtf = x(i)
      dmin = 8.*epsma2*abs(xtf)
c
c                               find step which gives sagitta = aimsag
      d = abs(gstep(i))
      do 40 icyc= 1, ncyc
c                               loop here only if sag=0.
      do 25 multpy= 1, 5
c           take two steps
         x(i) = xtf + d
         call mninex(x)
         nparx = npar
         call fcn(nparx,gin,fs1,u,4,futil)
         nfcn = nfcn + 1
         x(i) = xtf - d
         call mninex(x)
         call fcn(nparx,gin,fs2,u,4,futil)
         nfcn = nfcn + 1
         x(i) = xtf
         sag = 0.5*(fs1+fs2-2.0*amin)
         if (sag .ne. zero) go to 30
         if (gstep(i) .lt. zero) then
           if (d .ge. .5)  go to 26
           d = 10.*d
           if (d .gt. 0.5)  d = 0.51
           go to 25
         endif
         d = 10.*d
   25 continue
   26      write (cbf1(1:4),'(i4)') iext
           call mnwarn('w','hesse',
     +      'second derivative zero for parameter'//cbf1(1:4) )
           go to 390
c                             sag is not zero
   30 g2bfor = g2(i)
      g2(i) = 2.*sag/d**2
      grd(i) = (fs1-fs2)/(2.*d)
      if (ldebug) write (isyswr,31) i,idrv,gstep(i),d,g2(i),grd(i),sag
   31 format (i4,i2,6g12.5)
      gstep(i) = sign(d,gstep(i))
      dirin(i) = d
      yy(i) = fs1
      dlast = d
      d = dsqrt(2.0*aimsag/abs(g2(i)))
c         if parameter has limits, max int step size = 0.5
      stpinm = 0.5
      if (gstep(i) .lt. zero)  d = min(d,stpinm)
      if (d .lt. dmin)  d = dmin
c           see if converged
      if (abs((d-dlast)/d)          .lt. tlrstp)  go to 50
      if (abs((g2(i)-g2bfor)/g2(i)) .lt. tlrg2 )  go to 50
      d = min(d, 10.*dlast)
      d = max(d, 0.1*dlast)
   40 continue
c                       end of step size loop
      write (cbf1,'(i2,2e10.2)') iext,sag,aimsag
      call mnwarn('d','mnhess','second deriv. sag,aim= '//cbf1)
c
   50 continue
      ndex = i*(i+1)/2
      vhmat(ndex) = g2(i)
  100 continue
c                              end of diagonal second derivative loop
      call mninex(x)
c                                     refine the first derivatives
      if (istrat .gt. 0) call mnhes1(fcn,futil)
      isw(2) = 3
      dcovar = 0.
c                                        . . . .  off-diagonal elements
      if (npar .eq. 1)  go to 214
      do 200 i= 1, npar
      do 180 j= 1, i-1
      xti = x(i)
      xtj = x(j)
      x(i) = xti + dirin(i)
      x(j) = xtj + dirin(j)
      call mninex(x)
      call fcn(nparx,gin,fs1,u,4,futil)
      nfcn = nfcn + 1
      x(i) = xti
      x(j) = xtj
      elem = (fs1+amin-yy(i)-yy(j)) / (dirin(i)*dirin(j))
      ndex = i*(i-1)/2 + j
      vhmat(ndex) = elem
  180 continue
  200 continue
  214 call mninex(x)
c                  verify matrix positive-definite
      call mnpsdf
      do 220 i= 1, npar
      do 219 j= 1, i
      ndex = i*(i-1)/2 + j
      p(i,j) = vhmat(ndex)
  219 p(j,i) = p(i,j)
  220 continue
      call mnvert(p,maxint,maxint,npar,ifail)
      if (ifail .gt. 0)  then
        call mnwarn('w','hesse', 'matrix inversion fails.')
        go to 390
      endif
c                                        . . . . . . .  calculate  e d m
      edm = 0.
        do 230 i= 1, npar
c                              off-diagonal elements
        ndex = i*(i-1)/2
          do 225 j= 1, i-1
          ndex = ndex + 1
          ztemp = 2.0 * p(i,j)
          edm = edm + grd(i)*ztemp*grd(j)
  225     vhmat(ndex) = ztemp
c                              diagonal elements
        ndex = ndex + 1
        vhmat(ndex) = 2.0 * p(i,i)
        edm = edm  + p(i,i) * grd(i)**2
  230   continue
      if (isw(5).ge.1 .and. isw(2).eq.3 .and. itaur.eq.0)
     + write(isyswr,'(a)')' covariance matrix calculated successfully'
      go to 900
c                              failure to invert 2nd deriv matrix
  390 isw(2) = 1
      dcovar = 1.
      cstatu = 'failed    '
      if (isw(5) .ge. 0) write (isyswr,'(a)')
     +        '  mnhess fails and will return diagonal matrix. '
      do 395 i= 1, npar
      ndex = i*(i-1)/2
      do 394 j= 1, i-1
      ndex = ndex + 1
  394 vhmat(ndex) = 0.0
      ndex = ndex +1
      g2i = g2(i)
      if (g2i .le. zero)  g2i = 1.0
  395 vhmat(ndex) = 2.0/g2i
  900 return
      end
cdeck  id>, mnhes1. 
      subroutine mnhes1(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc      called from mnhess and mngrad
cc      calculate first derivatives (grd) and uncertainties (dgrd)
cc         and appropriate step sizes gstep
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      logical ldebug
      character cbf1*22
      ldebug = (idbg(5) .ge. 1)
      if (istrat .le. 0) ncyc = 1
      if (istrat .eq. 1) ncyc = 2
      if (istrat .gt. 1) ncyc = 6
      idrv = 1
      nparx = npar
      dfmin = 4.*epsma2*(abs(amin)+up)
c                                     main loop over parameters
      do 100 i= 1, npar
      xtf = x(i)
      dmin = 4.*epsma2*abs(xtf)
      epspri = epsma2 + abs(grd(i)*epsma2)
      optstp = dsqrt(dfmin/(abs(g2(i))+epspri))
      d = 0.2 * abs(gstep(i))
      if (d .gt. optstp)  d = optstp
      if (d .lt. dmin)  d = dmin
      chgold = 10000.
c                                       iterate reducing step size
      do 50 icyc= 1, ncyc
      x(i) = xtf + d
      call mninex(x)
      call fcn(nparx,gin,fs1,u,4,futil)
      nfcn = nfcn + 1
      x(i) = xtf - d
      call mninex(x)
      call fcn(nparx,gin,fs2,u,4,futil)
      nfcn = nfcn + 1
      x(i) = xtf
c                                       check if step sizes appropriate
      sag = 0.5*(fs1+fs2-2.0*amin)
      grdold = grd(i)
      grdnew = (fs1-fs2)/(2.0*d)
      dgmin = epsmac*(abs(fs1)+abs(fs2))/d
      if (ldebug) write (isyswr,11) i,idrv,gstep(i),d,g2(i),grdnew,sag
   11 format (i4,i2,6g12.5)
      if (grdnew .eq. zero)  go to 60
      change = abs((grdold-grdnew)/grdnew)
      if (change.gt.chgold .and. icyc.gt.1)  go to 60
      chgold = change
      grd(i) = grdnew
      gstep(i) = sign(d,gstep(i))
c                  decrease step until first derivative changes by <5%
      if (change .lt. 0.05) go to 60
      if (abs(grdold-grdnew) .lt. dgmin)  go to 60
      if (d .lt. dmin)  then
         call mnwarn('d','mnhes1','step size too small for 1st drv.')
         go to 60
      endif
      d = 0.2*d
   50 continue
c                                       loop satisfied = too many iter
      write (cbf1,'(2g11.3)') grdold,grdnew
      call mnwarn('d','mnhes1','too many iterations on d1.'//cbf1)
   60 continue
      dgrd(i) = max(dgmin,abs(grdold-grdnew))
  100 continue
c                                        end of first deriv. loop
      call mninex(x)
      return
      end
cdeck  id>, mnimpr. 
      subroutine mnimpr(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        attempts to improve on a good local minimum by finding a
cc        better one.   the quadratic part of fcn is removed by mncalf
cc        and this transformed function is minimized using the simplex
cc        method from several random starting points.
cc        ref. -- goldstein and price, math.comp. 25, 569 (1971)
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension dsav(mni), y(mni+1)
      parameter (alpha=1.,beta=0.5,gamma=2.0)
      data rnum/0./
      if (npar .le. 0)  return
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      cstatu = 'unchanged '
      itaur = 1
      epsi = 0.1*up
      npfn=nfcn
      nloop = word7(2)
      if (nloop .le. 0)  nloop = npar + 4
      nparx = npar
      nparp1=npar+1
      wg = 1.0/float(npar)
      sigsav = edm
      apsi = amin
         do 2 i= 1, npar
         xt(i) = x(i)
         dsav(i) = werr(i)
           do 2 j = 1, i
           ndex = i*(i-1)/2 + j
           p(i,j) = vhmat(ndex)
    2      p(j,i) = p(i,j)
      call mnvert(p,maxint,maxint,npar,ifail)
      if (ifail .ge. 1)  go to 280
c               save inverted matrix in vt
         do 12 i= 1, npar
         ndex = i*(i-1)/2
           do 12 j= 1, i
           ndex = ndex + 1
   12      vthmat(ndex) = p(i,j)
      loop = 0
c
   20 continue
         do 25 i= 1, npar
         dirin(i) = 2.0*dsav(i)
         call mnrn15(rnum,iseed)
   25    x(i) = xt(i) + 2.0*dirin(i)*(rnum-0.5)
      loop = loop + 1
      reg = 2.0
      if (isw(5) .ge. 0)   write (isyswr, 1040) loop
   30 call  mncalf(fcn,x,ycalf,futil)
      amin = ycalf
c                                        . . . . set up  random simplex
      jl = nparp1
      jh = nparp1
      y(nparp1) = amin
      amax = amin
         do 45 i= 1, npar
         xi = x(i)
         call mnrn15(rnum,iseed)
         x(i) = xi - dirin(i) *(rnum-0.5)
         call mncalf(fcn,x,ycalf,futil)
         y(i) = ycalf
         if (y(i) .lt. amin)  then
            amin = y(i)
            jl = i
         else if (y(i) .gt. amax)  then
            amax = y(i)
            jh = i
         endif
            do 40 j= 1, npar
   40       p(j,i) = x(j)
         p(i,nparp1) = xi
         x(i) = xi
   45    continue
c
      edm = amin
      sig2 = edm
c                                        . . . . . . .  start main loop
   50 continue
      if (amin .lt. zero)  go to 95
      if (isw(2) .le. 2)  go to 280
      ep = 0.1*amin
      if (sig2 .lt. ep   .and. edm.lt.ep  )     go to 100
      sig2 = edm
      if ((nfcn-npfn) .gt. nfcnmx)  go to 300
c         calculate new point * by reflection
      do 60 i= 1, npar
      pb = 0.
      do 59 j= 1, nparp1
   59 pb = pb + wg * p(i,j)
      pbar(i) = pb - wg * p(i,jh)
   60 pstar(i)=(1.+alpha)*pbar(i)-alpha*p(i,jh)
      call mncalf(fcn,pstar,ycalf,futil)
      ystar = ycalf
      if(ystar.ge.amin) go to 70
c         point * better than jl, calculate new point **
      do 61 i=1,npar
   61 pstst(i)=gamma*pstar(i)+(1.-gamma)*pbar(i)
      call mncalf(fcn,pstst,ycalf,futil)
      ystst = ycalf
   66 if (ystst .lt. y(jl))  go to 67
      call mnrazz(ystar,pstar,y,jh,jl)
      go to 50
   67 call mnrazz(ystst,pstst,y,jh,jl)
      go to 50
c         point * is not as good as jl
   70 if (ystar .ge. y(jh))  go to 73
      jhold = jh
      call mnrazz(ystar,pstar,y,jh,jl)
      if (jhold .ne. jh)  go to 50
c         calculate new point **
   73 do 74 i=1,npar
   74 pstst(i)=beta*p(i,jh)+(1.-beta)*pbar(i)
      call mncalf(fcn,pstst,ycalf,futil)
      ystst = ycalf
      if(ystst.gt.y(jh)) go to 30
c     point ** is better than jh
      if (ystst .lt. amin)  go to 67
      call mnrazz(ystst,pstst,y,jh,jl)
      go to 50
c                                        . . . . . .  end main loop
   95 if (isw(5) .ge. 0)  write (isyswr,1000)
      reg = 0.1
c                                        . . . . . ask if point is new
  100 call mninex(x)
      call fcn(nparx,gin,amin,u,4,futil)
      nfcn = nfcn + 1
      do 120 i= 1, npar
      dirin(i) = reg*dsav(i)
      if (abs(x(i)-xt(i)) .gt. dirin(i)) go to 150
  120 continue
      go to 230
  150 nfcnmx = nfcnmx + npfn - nfcn
      npfn = nfcn
      call mnsimp(fcn,futil)
      if (amin .ge. apsi)  go to 325
      do 220 i= 1, npar
      dirin(i) = 0.1 *dsav(i)
      if (abs(x(i)-xt(i)) .gt. dirin(i)) go to 250
  220 continue
  230 if (amin .lt. apsi)  go to 350
      go to 325
c                                        . . . . . . truly new minimum
  250 lnewmn = .true.
      if (isw(2) .ge. 1) then
          isw(2) = 1
          dcovar = max(dcovar,half)
      else
          dcovar = 1.
      endif
      itaur = 0
      nfcnmx = nfcnmx + npfn - nfcn
      cstatu = 'new minimu'
      if (isw(5) .ge. 0)      write (isyswr,1030)
      return
c                                        . . . return to previous region
  280 if (isw(5) .gt. 0) write (isyswr,1020)
      go to 325
  300 isw(1) = 1
  325 do 330 i= 1, npar
      dirin(i) = 0.01*dsav(i)
  330 x(i) = xt(i)
      amin = apsi
      edm = sigsav
  350 call mninex(x)
      if (isw(5) .gt. 0)    write (isyswr,1010)
      cstatu= 'unchanged '
      call mnrset(0)
      if (isw(2) .lt. 2)  go to 380
      if (loop .lt. nloop .and. isw(1) .lt. 1)  go to 20
  380 call mnprin (5,amin)
      itaur = 0
      return
 1000 format (54h an improvement on the previous minimum has been found)
 1010 format (51h improve has returned to region of original minimum)
 1020 format (/44h covariance matrix was not positive-definite)
 1030 format (/38h improve has found a truly new minimum/1h ,37(1h*)/)
 1040 format (/18h start attempt no.,i2,  20h to find new minimum)
      end
cdeck  id>, mninex. 
      subroutine mninex(pint)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        transforms from internal coordinates (pint) to external
cc        parameters (u).   the minimizing routines which work in
cc        internal coordinates call this routine before calling fcn.
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension pint(*)
      do 100 j= 1, npar
      i = nexofi(j)
      if (nvarl(i) .eq. 1) then
         u(i) = pint(j)
      else
         u(i) = alim(i) + 0.5*(dsin(pint(j)) +1.0) * (blim(i)-alim(i))
      endif
  100 continue
      return
      end
cdeck  id>, mninit. 
      subroutine mninit (i1,i2,i3)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        this is the main initialization subroutine for minuit
cc     it initializes some constants in common
cc                (including the logical i/o unit nos.),
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c
      external intrac
      logical  intrac
c            i/o unit numbers
      isysrd = i1
      isyswr = i2
        istkwr(1) = isyswr
        nstkwr = 1
      isyssa = i3
      nstkrd = 0
c               version identifier
      cvrsn = '90.10 '
c               some constant constants in common
      maxint=mni
      maxext=mne
      undefi = -54321.
      bigedm = 123456.
      cundef = ')undefined'
      covmes(0) = 'no error matrix       '
      covmes(1) = 'err matrix approximate'
      covmes(2) = 'err matrix not pos-def'
      covmes(3) = 'error matrix accurate '
c                some starting values in common
      nblock = 0
      icomnd = 0
      ctitl = cundef
      cfrom = 'input   '
      nfcnfr = nfcn
      cstatu= 'initialize'
      isw(3) = 0
      isw(4) = 0
      isw(5) = 1
c         isw(6)=0 for batch jobs,  =1 for interactive jobs
      isw(6) = 0
      if (intrac(dummy))  isw(6) = 1
c        debug options set to default values
      do 10 idb= 0, maxdbg
   10 idbg(idb) = 0
      lrepor = .false.
      lwarn  = .true.
      limset = .false.
      lnewmn = .false.
      istrat = 1
      itaur = 0
c        default page dimensions and 'new page' carriage control integer
      npagwd = 120
      npagln = 56
      newpag = 1
      if (isw(6) .gt. 0) then
         npagwd = 80
         npagln = 30
         newpag = 0
      endif
      up = 1.0
      updflt = up
c                   determine machine accuracy epsmac
      epstry = 0.5
      do 33 i= 1, 100
      epstry = epstry * 0.5
      epsp1 = one + epstry
      call mntiny(epsp1, epsbak)
      if (epsbak .lt. epstry)  go to 35
   33 continue
      epstry = 1.0e-7
      epsmac = 4.0*epstry
      write (isyswr,'(a,a,e10.2)') ' mninit unable to determine',
     + ' arithmetic precision. will assume:',epsmac
   35 epsmac = 8.0 * epstry
      epsma2 = 2.0 * dsqrt(epsmac)
c                 the vlims are a non-negligible distance from pi/2
c         used by mnpint to set variables "near" the physical limits
      piby2 = 2.0*atan(1.0)
      distnn = 8.0*dsqrt(epsma2)
      vlimhi =  piby2 - distnn
      vlimlo = -piby2 + distnn
      call mncler
      write (isyswr,'(3a,i3,a,i3,a,e10.2)')  '  minuit release ',cvrsn,
     +' initialized.   dimensions ',mne,'/',mni,'  epsmac=',epsmac
      return
      end
cdeck  id>, mnintr. 
      subroutine mnintr(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       called by user. interfaces to mnread to allow user to change
cc       easily from fortran-callable to interactive mode.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      iflgin = 3
      call mnread(fcn,iflgin,iflgut,futil)
      write (isyswr,'(2a/)')  ' end of minuit command input. ',
     +      '   return to user program.'
      return
      end
cdeck  id>, mnlims. 
      subroutine mnlims(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       called from mnset
cc       interprets the set lim command, to reset the parameter limits
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
c
      cfrom = 'set lim '
      nfcnfr = nfcn
      cstatu= 'no change '
      i2 = word7(1)
      if (i2 .gt. maxext .or. i2 .lt. 0)  go to 900
      if (i2 .gt. 0)  go to 30
c                                     set limits on all parameters
      newcod = 4
      if (word7(2) .eq. word7(3))  newcod = 1
      do 20 inu= 1, nu
      if (nvarl(inu) .le. 0)  go to 20
      if (nvarl(inu).eq.1 .and. newcod.eq.1)  go to 20
      kint = niofex(inu)
c             see if parameter has been fixed
      if (kint .le. 0)  then
         if (isw(5) .ge. 0)  write (isyswr,'(11x,a,i3)')
     +      ' limits not changed for fixed parameter:',inu
         go to 20
      endif
      if (newcod .eq. 1)  then
c            remove limits from parameter
         if (isw(5) .gt. 0)     write (isyswr,134)  inu
         cstatu = 'new limits'
         call mndxdi(x(kint),kint,dxdi)
         snew = gstep(kint)*dxdi
         gstep(kint) = abs(snew)
         nvarl(inu) = 1
      else
c             put limits on parameter
         alim(inu) = min(word7(2),word7(3))
         blim(inu) = max(word7(2),word7(3))
         if (isw(5) .gt. 0) write (isyswr,237)  inu,alim(inu),blim(inu)
         nvarl(inu) = 4
         cstatu = 'new limits'
         gstep(kint) = -0.1
      endif
   20 continue
      go to 900
c                                       set limits on one parameter
   30 if (nvarl(i2) .le. 0)  then
        write (isyswr,'(a,i3,a)') ' parameter ',i2,' is not variable.'
        go to 900
      endif
      kint = niofex(i2)
c                                       see if parameter was fixed
      if (kint .eq. 0)  then
         write (isyswr,'(a,i3)')
     +     ' request to change limits on fixed parameter:',i2
         do 82 ifx= 1, npfix
         if (i2 .eq. ipfix(ifx)) go to 92
   82    continue
         write (isyswr,'(a)') ' minuit bug in mnlims. see f. james'
   92    continue
      endif
      if (word7(2) .ne. word7(3))  go to 235
c                                       remove limits
      if (nvarl(i2) .ne. 1)  then
         if (isw(5) .gt. 0)  write (isyswr,134)  i2
  134    format (30h limits removed from parameter  ,i4)
         cstatu = 'new limits'
         if (kint .le. 0)  then
            gsteps(ifx) = abs(gsteps(ifx))
         else
            call mndxdi(x(kint),kint,dxdi)
            if (abs(dxdi) .lt. 0.01)  dxdi=0.01
            gstep(kint) = abs(gstep(kint)*dxdi)
            grd(kint) = grd(kint)*dxdi
         endif
         nvarl(i2) = 1
      else
         write (isyswr,'(a,i3)') ' no limits specified.  parameter ',
     +        i2,' is already unlimited.  no change.'
      endif
      go to 900
c                                        put on limits
  235 alim(i2) = min(word7(2),word7(3))
      blim(i2) = max(word7(2),word7(3))
      nvarl(i2) = 4
      if (isw(5) .gt. 0)   write (isyswr,237)  i2,alim(i2),blim(i2)
  237 format (10h parameter ,i3, 14h limits set to  ,2g15.5)
      cstatu = 'new limits'
      if (kint .le. 0)  then
         gsteps(ifx) = -0.1
      else
         gstep(kint) = -0.1
      endif
c
  900 continue
      if (cstatu .ne. 'no change ')  then
        call mnexin(x)
        call mnrset(1)
      endif
      return
      end
cdeck  id>, mnline. 
      subroutine mnline(fcn,start,fstart,step,slope,toler,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        perform a line search from position start
cc        along direction step, where the length of vector step
cc                   gives the expected position of minimum.
cc        fstart is value of function at start
cc        slope (if non-zero) is df/dx along step at start
cc        toler is initial tolerance of minimum in direction step
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension start(*), step(*)
      parameter (maxpt=12)
      dimension xpq(maxpt),ypq(maxpt)
      character*1 chpq(maxpt)
      dimension xvals(3),fvals(3),coeff(3)
      character*26 charal
      character*60 cmess
      parameter (slambg=5.,alpha=2.)
c slambg and alpha control the maximum individual steps allowed.
c the first step is always =1. the max length of second step is slambg.
c the max size of subsequent steps is the maximum previous successful
c   step multiplied by alpha + the size of most recent successful step,
c   but cannot be smaller than slambg.
      logical ldebug
      data charal / 'abcdefghijklmnopqrstuvwxyz' /
      ldebug = (idbg(1).ge.1)
c                  starting values for overall limits on total step slam
      overal = 1000.
      undral = -100.
c                              debug check if start is ok
      if (ldebug)  then
         call mninex(start)
         call fcn(nparx,gin,f1,u,4,futil)
         nfcn=nfcn+1
         if (f1 .ne. fstart) then
             write (isyswr,'(a/2e14.5/2x,10f10.5)')
     + ' mnline start point not consistent, f values, parameters=',
     +  (x(kk),kk=1,npar)
         endif
      endif
c                                      . set up linear search along step

      fvmin = fstart
      xvmin = 0.
      nxypt = 1
      chpq(1) = charal(1:1)
      xpq(1) = 0.
      ypq(1) = fstart
c               slamin = smallest possible value of abs(slam)
      slamin = 0.
      do 20 i= 1, npar
      if (step(i) .eq. zero)  go to 20
      ratio = abs(start(i)/step(i))
      if (slamin .eq. zero)     slamin = ratio
      if (ratio .lt. slamin)  slamin = ratio
   20 x(i) = start(i) + step(i)
      if (slamin .eq. zero)  slamin = epsmac
      slamin = slamin*epsma2
      nparx = npar
c
      call mninex(x)
      call fcn(nparx,gin,f1,u,4,futil)
      nfcn=nfcn+1
      nxypt = nxypt + 1
      chpq(nxypt) = charal(nxypt:nxypt)
      xpq(nxypt) = 1.
      ypq(nxypt) = f1
      if (f1 .lt. fstart) then
         fvmin = f1
         xvmin = 1.0
      endif
c                         . quadr interp using slope gdel and two points
      slam = 1.
      toler8 = toler
      slamax = slambg
      flast = f1
c                         can iterate on two-points (cut) if no imprvmnt
   25 continue
      denom = 2.0*(flast-fstart-slope*slam)/slam**2
c     if (denom .eq. zero)  denom = -0.1*slope
                            slam  = 1.
      if (denom .ne. zero)  slam = -slope/denom
      if (slam  .lt. zero)  slam = slamax
      if (slam .gt. slamax)  slam = slamax
      if (slam .lt. toler8)  slam = toler8
      if (slam .lt. slamin)  go to 80
      if (abs(slam-1.0).lt.toler8 .and. f1.lt.fstart)  go to 70
      if (abs(slam-1.0).lt.toler8) slam = 1.0+toler8
      if (nxypt .ge. maxpt) go to 65
      do 30 i= 1, npar
   30 x(i) = start(i) + slam*step(i)
      call mninex(x)
      call fcn(npar,gin,f2,u,4,futil)
      nfcn = nfcn + 1
      nxypt = nxypt + 1
      chpq(nxypt) = charal(nxypt:nxypt)
      xpq(nxypt) = slam
      ypq(nxypt) = f2
      if (f2 .lt. fvmin)  then
         fvmin = f2
         xvmin = slam
      endif
      if (fstart .eq. fvmin) then
         flast = f2
         toler8 = toler*slam
         overal = slam-toler8
         slamax = overal
         go to 25
      endif
c                                        . quadr interp using 3 points
      xvals(1) = xpq(1)
      fvals(1) = ypq(1)
      xvals(2) = xpq(nxypt-1)
      fvals(2) = ypq(nxypt-1)
      xvals(3) = xpq(nxypt)
      fvals(3) = ypq(nxypt)
c                             begin iteration, calculate desired step
   50 continue
      slamax = max(slamax,alpha*abs(xvmin))
      call mnpfit(xvals,fvals,3,coeff,sdev)
      if (coeff(3) .le. zero)  then
         slopem = 2.0*coeff(3)*xvmin + coeff(2)
         if (slopem .le. zero) then
            slam = xvmin + slamax
         else
            slam = xvmin - slamax
         endif
      else
         slam = -coeff(2)/(2.0*coeff(3))
         if (slam .gt. xvmin+slamax)  slam = xvmin+slamax
         if (slam .lt. xvmin-slamax)  slam = xvmin-slamax
      endif
      if (slam .gt. zero) then
          if (slam .gt. overal) slam = overal
      else
          if (slam .lt. undral) slam = undral
      endif
c               come here if step was cut below
   52 continue
      toler9 = max(toler8,abs(toler8*slam))
      do 55 ipt= 1, 3
      if (abs(slam-xvals(ipt)) .lt. toler9)  go to 70
   55 continue
c                take the step
      do 60 i= 1, npar
   60 x(i) = start(i)+slam*step(i)
      call mninex(x)
      call fcn(nparx,gin,f3,u,4,futil)
      nfcn = nfcn + 1
      nxypt = nxypt + 1
      chpq(nxypt) = charal(nxypt:nxypt)
      xpq(nxypt) = slam
      ypq(nxypt) = f3
c             find worst previous point out of three
      fvmax = fvals(1)
      nvmax = 1
      if (fvals(2) .gt. fvmax) then
         fvmax = fvals(2)
         nvmax = 2
      endif
      if (fvals(3) .gt. fvmax) then
         fvmax = fvals(3)
         nvmax = 3
      endif
c              if latest point worse than all three previous, cut step
      if (f3 .ge. fvmax)  then
          if (nxypt .ge. maxpt) go to 65
          if (slam .gt. xvmin) overal = min(overal,slam-toler8)
          if (slam .lt. xvmin) undral = max(undral,slam+toler8)
          slam = 0.5*(slam+xvmin)
          go to 52
      endif
c              prepare another iteration, replace worst previous point
      xvals(nvmax) = slam
      fvals(nvmax) = f3
      if (f3 .lt. fvmin)  then
         fvmin = f3
         xvmin = slam
      else
         if (slam .gt. xvmin) overal = min(overal,slam-toler8)
         if (slam .lt. xvmin) undral = max(undral,slam+toler8)
      endif
      if (nxypt .lt. maxpt)  go to 50
c                                            . . end of iteration . . .
c            stop because too many iterations
   65 cmess = ' line search has exhausted the limit of function calls '
      if (ldebug) then
        write (isyswr,'(a/(2x,6g12.4))') ' mnline debug: steps=',
     +    (step(kk),kk=1,npar)
      endif
      go to 100
c            stop because within tolerance
   70 continue
      cmess = ' line search has attained tolerance '
      go to 100
   80 continue
      cmess = ' step size at arithmetically allowed minimum'
  100 continue
      amin = fvmin
      do 120 i= 1, npar
      dirin(i) = step(i)*xvmin
  120 x(i) = start(i) + dirin(i)
      call mninex(x)
      if (xvmin .lt. 0.)      call mnwarn('d','mnline',
     +                   ' line minimum in backwards direction')
      if (fvmin .eq. fstart)  call mnwarn('d','mnline',
     +                     ' line search finds no improvement ')
      if (ldebug)  then
         write (isyswr,'('' after'',i3,'' points,'',a)') nxypt,cmess
         call mnplot(xpq,ypq,chpq,nxypt,isyswr,npagwd,npagln)
      endif
      return
      end
cdeck  id>, mnmatu. 
      subroutine mnmatu(kode)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        prints the covariance matrix v when kode=1.
cc        always prints the global correlations, and
cc        calculates and prints the individual correlation coefficients
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension vline(mni)
      isw2 = isw(2)
      if (isw2 .lt. 1)  then
          write (isyswr,'(1x,a)')  covmes(isw2)
          go to 500
      endif
      if (npar .eq. 0)  then
          write (isyswr,'('' mnmatu: npar=0'')')
          go to 500
          endif
c                                       . . . . .external error matrix
      if (kode .eq. 1)  then
         isw5 = isw(5)
         isw(5) = 2
         call mnemat(p,maxint)
           if (isw2.lt.3)  write (isyswr,'(1x,a)')  covmes(isw2)
         isw(5) = isw5
      endif
c                                       . . . . . correlation coeffs. .
      if (npar .le. 1)   go to 500
      call mnwerr
c     ncoef is number of coeff. that fit on one line, not to exceed 20
      ncoef = (npagwd-19)/6
      ncoef = min(ncoef,20)
      nparm = min(npar,ncoef)
      write (isyswr, 150) (nexofi(id),id=1,nparm)
  150 format (/36h parameter  correlation coefficients  /
     +         18h       no.  global   ,20i6)
      do 200 i= 1, npar
         ix = nexofi(i)
         ndi = i*(i+1)/2
           do 170 j= 1, npar
           m = max(i,j)
           n = min(i,j)
           ndex = m*(m-1)/2 + n
           ndj = j*(j+1)/2
  170      vline(j) = vhmat(ndex)/dsqrt(abs(vhmat(ndi)*vhmat(ndj)))
         nparm = min(npar,ncoef)
         write (isyswr,171)   ix, globcc(i), (vline(it),it=1,nparm)
  171    format (6x,i3,2x,f7.5,1x,20f6.3)
         if (i.le.nparm) go to 200
            do 190 iso= 1, 10
            nsofar = nparm
            nparm = min(npar,nsofar+ncoef)
            write (isyswr,181)  (vline(it),it=nsofar+1,nparm)
  181       format (19x,20f6.3)
            if (i .le. nparm) go to 192
  190       continue
  192    continue
  200 continue
      if (isw2.lt.3)  write (isyswr,'(1x,a)')  covmes(isw2)
  500 return
      end
cdeck  id>, mnmigr. 
      subroutine mnmigr(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        performs a local function minimization using basically the
cc        method of davidon-fletcher-powell as modified by fletcher
cc        ref. -- fletcher, comp.j. 13,317 (1970)   "switching method"
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension gs(mni), step(mni),  xxs(mni), flnu(mni), vg(mni)
      logical ldebug
      parameter (toler=0.05)
      if (npar .le. 0)  return
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      ldebug = (idbg(4) .ge. 1)
      cfrom = 'migrad  '
      nfcnfr = nfcn
      nfcnmg = nfcn
      cstatu= 'initiate  '
      iswtr = isw(5) - 2*itaur
      npfn = nfcn
      nparx = npar
      vlen = npar*(npar+1)/2
      nrstrt = 0
      npsdf = 0
      lined2 = 0
      isw(4) = -1
      rhotol = 1.0e-3*apsi
      if (iswtr .ge. 1)  write (isyswr,470) istrat,rhotol
  470 format (' start migrad minimization.  strategy',i2,
     +'.  convergence when edm .lt.',e9.2)
c                                           initialization strategy
      if (istrat.lt.2 .or. isw(2).ge.3)  go to 2
c                                come (back) here to restart completely
    1 continue
      if (nrstrt .gt. istrat)  then
         cstatu= 'failed    '
         isw(4) = -1
         go to 230
         endif
c                                      . get full covariance and gradient
      call mnhess(fcn,futil)
      call mnwerr
      npsdf = 0
      if (isw(2) .ge. 1)  go to 10
c                                        . get gradient at start point
    2 continue
      call mninex(x)
      if (isw(3) .eq. 1) then
          call fcn(nparx,gin,fzero,u,2,futil)
          nfcn = nfcn + 1
      endif
      call mnderi(fcn,futil)
      if (isw(2) .ge. 1)  go to 10
c                                   sometimes start with diagonal matrix
      do 3 i= 1, npar
         xxs(i) = x(i)
         step(i) = zero
    3 continue
c                           do line search if second derivative negative
      lined2 = lined2 + 1
      if (lined2 .lt. 2*npar) then
      do 5 i= 1, npar
         if (g2(i) .gt. 0.)  go to 5
         step(i) = -sign(gstep(i),grd(i))
         gdel = step(i)*grd(i)
         fs = amin
         call mnline(fcn,xxs,fs,step,gdel,toler,futil)
         call mnwarn('d','mnmigr','negative g2 line search')
         iext = nexofi(i)
         if (ldebug) write (isyswr,'(a,i3,2g13.3)')
     +    ' negative g2 line search, param ',iext,fs,amin
         go to 2
    5 continue
      endif
c                           make diagonal error matrix
      do 8 i=1,npar
         ndex = i*(i-1)/2
           do 7 j=1,i-1
           ndex = ndex + 1
    7      vhmat(ndex) = 0.
         ndex = ndex + 1
         if (g2(i) .le. zero)  g2(i) = 1.
         vhmat(ndex) = 2./g2(i)
    8 continue
      dcovar = 1.
      if (ldebug) write (isyswr,'(a,a/(1x,10g10.2))') ' debug mnmigr,',
     +  ' starting matrix diagonal,  vhmat=', (vhmat(kk),kk=1,vlen)
c                                         ready to start first iteration
   10 continue
      impruv = 0
      nrstrt = nrstrt + 1
      if (nrstrt .gt. istrat+1)  then
         cstatu= 'failed    '
         go to 230
         endif
      fs = amin
c                                        . . . get edm and set up loop
      edm = 0.
         do 18 i= 1, npar
         gs(i) = grd(i)
         xxs(i) = x(i)
         ndex = i*(i-1)/2
           do 17 j= 1, i-1
           ndex = ndex + 1
   17      edm = edm + gs(i)*vhmat(ndex)*gs(j)
         ndex = ndex + 1
   18    edm = edm + 0.5 * gs(i)**2 *vhmat(ndex)
      edm = edm * 0.5 * (1.0+3.0*dcovar)
        if (edm .lt. zero)  then
        call mnwarn('w','migrad','starting matrix not pos-definite.')
        isw(2) = 0
        dcovar = 1.
        go to 2
        endif
      if (isw(2) .eq. 0)  edm=bigedm
      iter = 0
      call mninex(x)
      call mnwerr
      if (iswtr .ge. 1)  call mnprin(3,amin)
      if (iswtr .ge. 2)  call mnmatu(0)
c                                        . . . . .  start main loop
   24 continue
      if (nfcn-npfn .ge. nfcnmx)  go to 190
      gdel = 0.
      gssq = 0.
         do 30  i=1,npar
         ri = 0.
         gssq = gssq + gs(i)**2
           do 25 j=1,npar
           m = max(i,j)
           n = min(i,j)
           ndex = m*(m-1)/2 + n
   25      ri = ri + vhmat(ndex) *gs(j)
         step(i) = -0.5*ri
   30    gdel = gdel + step(i)*gs(i)
      if (gssq .eq. zero)  then
          call mnwarn('d','migrad',
     +             ' first derivatives of fcn are all zero')
          go to 300
      endif
c                 if gdel positive, v not posdef
      if (gdel .ge. zero)  then
         call mnwarn('d','migrad',' newton step not descent.')
         if (npsdf .eq. 1)  go to 1
         call mnpsdf
         npsdf = 1
         go to 24
         endif
c                                        . . . . do line search
      call mnline(fcn,xxs,fs,step,gdel,toler,futil)
      if (amin .eq. fs) go to 200
      cfrom  = 'migrad  '
      nfcnfr = nfcnmg
      cstatu= 'progress  '
c                                        . get gradient at new point
      call mninex(x)
      if (isw(3) .eq. 1) then
          call fcn(nparx,gin,fzero,u,2,futil)
          nfcn = nfcn + 1
      endif
      call mnderi(fcn,futil)
c                                         . calculate new edm
      npsdf = 0
   81 edm = 0.
      gvg = 0.
      delgam = 0.
      gdgssq = 0.
         do 100 i= 1, npar
         ri = 0.
         vgi = 0.
           do 90 j= 1, npar
           m = max(i,j)
           n = min(i,j)
           ndex = m*(m-1)/2 + n
           vgi = vgi + vhmat(ndex)*(grd(j)-gs(j))
   90      ri  =  ri + vhmat(ndex)* grd(j)
      vg(i) = vgi*0.5
      gami = grd(i) - gs(i)
      gdgssq = gdgssq + gami**2
      gvg = gvg + gami*vg(i)
      delgam = delgam + dirin(i)*gami
  100 edm = edm + grd(i)*ri*0.5
      edm = edm * 0.5 * (1.0 + 3.0*dcovar)
c                          . if edm negative,  not positive-definite
      if (edm .lt. zero .or. gvg .le. zero)  then
         call mnwarn('d','migrad','not pos-def. edm or gvg negative.')
         cstatu = 'not posdef'
         if (npsdf .eq. 1)  go to 230
         call mnpsdf
         npsdf = 1
         go to 81
      endif
c                            print information about this iteration
      iter = iter + 1
      if (iswtr.ge.3 .or. (iswtr.eq.2.and.mod(iter,10).eq.1)) then
         call mnwerr
         call mnprin(3,amin)
      endif
      if (gdgssq .eq. zero)  call mnwarn('d','migrad',
     +           'no change in first derivatives over last step')
      if (delgam .lt. zero) call mnwarn('d','migrad',
     +          'first derivatives increasing along search line')
c                                        .  update covariance matrix
      cstatu = 'improvemnt'
        if (ldebug) write (isyswr,'(a,(1x,10g10.3))') ' vhmat 1 =',
     +             (vhmat(kk),kk=1,10)
      dsum = 0.
      vsum = 0.
         do  120  i=1, npar
           do  120  j=1, i
           d = dirin(i)*dirin(j)/delgam - vg(i)*vg(j)/gvg
           dsum = dsum + abs(d)
           ndex = i*(i-1)/2 + j
           vhmat(ndex) = vhmat(ndex) + 2.0*d
           vsum = vsum + abs(vhmat(ndex))
  120      continue
c                smooth local fluctuations by averaging dcovar
      dcovar = 0.5*(dcovar + dsum/vsum)
      if (iswtr.ge.3 .or. ldebug) write (isyswr,'(a,f5.1,a)')
     +      ' relative change in cov. matrix=',dcovar*100.,'%'
      if (ldebug) write (isyswr,'(a,(1x,10g10.3))') ' vhmat 2 =',
     +             (vhmat(kk),kk=1,10)
      if (delgam .le. gvg)  go to 135
      do 125 i= 1, npar
  125 flnu(i) = dirin(i)/delgam - vg(i)/gvg
      do 130 i= 1, npar
      do 130 j= 1, i
      ndex = i*(i-1)/2 + j
  130 vhmat(ndex) = vhmat(ndex) + 2.0*gvg*flnu(i)*flnu(j)
  135 continue
c                                              and see if converged
      if (edm .lt. 0.1*rhotol)  go to 300
c                                    if not, prepare next iteration
      do 140 i= 1, npar
      xxs(i) = x(i)
      gs(i) = grd(i)
  140 continue
      fs = amin
      impruv = impruv + 1
      if (isw(2) .eq. 0  .and. dcovar.lt. 0.5 )  isw(2) = 1
      if (isw(2) .eq. 3  .and. dcovar.gt. 0.1 )  isw(2) = 1
      if (isw(2) .eq. 1  .and. dcovar.lt. 0.05)  isw(2) = 3
      go to 24
c                                        . . . . .  end main loop
c                                         . . call limit in mnmigr
  190 isw(1) = 1
      if (isw(5) .ge. 0)
     +     write (isyswr,'(a)')  ' call limit exceeded in migrad.'
      cstatu = 'call limit'
      go to 230
c                                         . . fails to improve . .
  200 if (iswtr .ge. 1)  write (isyswr,'(a)')
     +           ' migrad fails to find improvement'
      do 210 i= 1, npar
  210 x(i) = xxs(i)
      if (edm .lt. rhotol)  go to 300
      if (edm .lt. abs(epsma2*amin))  then
         if (iswtr .ge. 0)  write (isyswr, '(a)')
     +      ' machine accuracy limits further improvement.'
         go to 300
         endif
      if (istrat .lt. 1)  then
         if (isw(5) .ge. 0) write (isyswr, '(a)')
     +    ' migrad fails with strategy=0.   will try with strategy=1.'
         istrat = 1
      endif
         go to 1
c                                         . . fails to converge
  230 if (iswtr .ge. 0)  write (isyswr,'(a)')
     +    ' migrad terminated without convergence.'
      if (isw(2) .eq. 3)  isw(2) = 1
      isw(4) = -1
      go to 400
c                                         . . apparent convergence
  300 if (iswtr .ge. 0) write(isyswr,'(/a)')
     +   ' migrad minimization has converged.'
      if (itaur .eq. 0) then
        if (istrat .ge. 2 .or. (istrat.eq.1.and.isw(2).lt.3)) then
           if (isw(5) .ge. 0)  write (isyswr, '(/a)')
     +      ' migrad will verify convergence and error matrix.'
           call mnhess(fcn,futil)
           call mnwerr
           npsdf = 0
           if (edm .gt. rhotol) go to 10
        endif
      endif
      cstatu='converged '
      isw(4) = 1
c                                           come here in any case
  400 continue
      cfrom = 'migrad  '
      nfcnfr = nfcnmg
      call  mninex(x)
      call mnwerr
      if (iswtr .ge. 0)  call mnprin (3,amin)
      if (iswtr .ge. 1)  call mnmatu(1)
      return
      end
cdeck  id>, mnmnos. 
      subroutine mnmnos(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        performs a minos error analysis on those parameters for
cc        which it is requested on the minos command.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      if (npar .le. 0)  go to 700
      ngood = 0
      nbad = 0
      nfcnmi = nfcn
c                                      . loop over parameters requested
      do 570 knt= 1, npar
      if (int(word7(2)) .eq. 0) then
          ilax = nexofi(knt)
      else
          if (knt .ge. 7)  go to 580
          ilax = int(word7(knt+1))
          if (ilax .eq. 0)  go to 580
          if (ilax .gt. 0 .and. ilax .le. nu) then
             if (niofex(ilax) .gt. 0)  go to 565
          endif
          write (isyswr,564) ilax
  564     format (' parameter number ',i5,' not variable. ignored.')
          go to 570
      endif
  565 continue
c                                         calculate one pair of m e's
      ilax2 = 0
      call mnmnot(fcn,ilax,ilax2,val2pl,val2mi,futil)
      if (lnewmn)  go to 650
c                                          update ngood and nbad
      iin = niofex(ilax)
      if (erp(iin) .gt. zero) then
         ngood=ngood+1
      else
         nbad=nbad+1
      endif
      if (ern(iin) .lt. zero) then
         ngood=ngood+1
      else
         nbad=nbad+1
      endif
  570 continue
c                                           end of loop . . . . . . .
  580 continue
c                                        . . . . printout final values .
      cfrom = 'minos   '
      nfcnfr = nfcnmi
      cstatu= 'unchanged '
      if (ngood.eq.0.and.nbad.eq.0) go to 700
      if (ngood.gt.0.and.nbad.eq.0) cstatu='successful'
      if (ngood.eq.0.and.nbad.gt.0) cstatu='failure   '
      if (ngood.gt.0.and.nbad.gt.0) cstatu='problems  '
      if (isw(5) .ge. 0) call mnprin(4,amin)
      if (isw(5) .ge. 2) call mnmatu(0)
      go to 900
c                                        . . . new minimum found . . . .
  650 continue
      cfrom = 'minos   '
      nfcnfr = nfcnmi
      cstatu= 'new minimu'
      if (isw(5) .ge. 0) call mnprin(4,amin)
      write (isyswr,675)
  675 format(/50h new minimum found.  go back to minimization step./1h ,
     +60(1h=)/60x,1hv/60x,1hv/60x,1hv/57x,7hvvvvvvv/58x,5hvvvvv/59x,
     +3hvvv/60x,1hv//)
      go to 900
  700 write (isyswr,'(a)') ' there are no minos errors to calculate.'
  900 return
      end
cdeck  id>, mnmnot. 
      subroutine mnmnot(fcn,ilax,ilax2,val2pl,val2mi,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        performs a minos error analysis on one parameter.
cc        the parameter ilax is varied, and the minimum of the
cc        function with respect to the other parameters is followed
cc        until it crosses the value fmin+up.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension xdev(mni),w(mni),gcc(mni)
      character*4 cpos,cneg,csig
      character*1 cdot,cstar,cblank
      parameter (cpos='posi',cneg='nega',cdot='.',cstar='*',cblank=' ')
      logical  lovflo,  lright, lleft
c                                        . . save and prepare start vals
      isw2 = isw(2)
      isw4 = isw(4)
      sigsav = edm
      istrav = istrat
      dc = dcovar
      lovflo = .false.
      lnewmn = .false.
      toler = epsi*0.5
      apsi  = epsi*0.5
      abest=amin
      aim = amin + up
      mpar=npar
      nfmxin = nfcnmx
      do 125 i= 1, mpar
  125 xt(i) = x(i)
      do 130 j= 1, mpar*(mpar+1)/2
  130 vthmat(j) = vhmat(j)
      do 135 i= 1, mpar
      gcc(i) = globcc(i)
  135 w(i) = werr(i)
      it = niofex(ilax)
      erp(it) = 0.
      ern(it) = 0.
      call mninex(xt)
      ut = u(ilax)
      if (nvarl(ilax) .eq. 1) then
         alim(ilax) = ut -100.*w(it)
         blim(ilax) = ut +100.*w(it)
         endif
      ndex = it*(it+1)/2
      xunit = dsqrt(up/vthmat(ndex))
      marc = 0
      do 162 i= 1, mpar
      if (i .eq. it)  go to 162
      marc = marc + 1
         imax = max(it,i)
         indx = imax*(imax-1)/2 + min(it,i)
      xdev(marc) = xunit*vthmat(indx)
  162 continue
c                           fix the parameter in question
      call mnfixp (it,ierr)
      if (ierr .gt. 0)  then
         write (isyswr,'(a,i5,a,i5)')
     +    ' minuit error. cannot fix parameter',ilax,'    internal',it
         go to 700
      endif
c                       . . . . . nota bene: from here on, npar=mpar-1
c      remember: mnfixp squeezes it out of x, xt, werr, and vhmat,
c                                                    not w, vthmat
      do 500 isig= 1,2
      if (isig .eq. 1) then
         sig = 1.0
         csig = cpos
      else
         sig = -1.0
         csig = cneg
      endif
c                                        . sig=sign of error being calcd
      if (isw(5) .gt. 1) write (isyswr,806)  csig,ilax,cpnam(ilax)
  806 format (/' determination of ',a4,'tive minos error for parameter',
     +    i3, 2x ,a)
      if (isw(2).le.0) call mnwarn('d','minos','no covariance matrix.')
      nlimit = nfcn + nfmxin
      istrat = max(istrav-1,0)
      du1 = w(it)
      u(ilax) = ut + sig*du1
         fac = sig*du1/w(it)
         do 185 i= 1, npar
  185    x(i) = xt(i) + fac*xdev(i)
      if (isw(5) .gt. 1) write (isyswr,801)  ilax,ut,sig*du1,u(ilax)
  801 format (/' parameter',i4,' set to',e11.3,' + ',e10.3,' = ',e12.3)
c                                        loop to hit aim
      ke1cr = ilax
      ke2cr = 0
      xmidcr = ut + sig*du1
      xdircr = sig*du1
c
      amin = abest
      nfcnmx = nlimit - nfcn
      call mncros(fcn,aopt,iercr,futil)
      if (abest-amin .gt. 0.01*up)  go to 650
      if (iercr .eq. 1)  go to 440
      if (iercr .eq. 2)  go to 450
      if (iercr .eq. 3)  go to 460
c                                        . error successfully calculated
      eros = sig*du1 + aopt*xdircr
      if (isw(5) .gt. 1) write (isyswr,808)  csig,ilax,cpnam(ilax),eros
  808 format (/9x,4hthe ,a4,  29htive minos error of parameter,i3,   2h
     +, ,a10,      4h, is ,e12.4)
      go to 480
c                                        . . . . . . . . failure returns
  440 if (isw(5) .ge. 1) write(isyswr,807)  csig,ilax,cpnam(ilax)
  807 format (5x,'the ',a4,'tive minos error of parameter',i3,', ',a,
     +', exceeds its limit.'/)
      eros = undefi
      go to 480
  450 if (isw(5) .ge. 1) write (isyswr, 802)  csig,ilax,nfmxin
  802 format (9x,'the ',a,'tive minos error',i4,' requires more than',
     +   i5,' function calls.'/)
      eros = 0.
      go to 480
  460 if (isw(5) .ge. 1) write (isyswr, 805) csig,ilax
  805 format (25x,a,'tive minos error not calculated for parameter',i4/)
      eros = 0.
c
  480 if (isw(5) .gt. 1) write (isyswr,'(5x, 74(1h*))')
      if (sig .lt. zero)  then
         ern(it) = eros
         if (ilax2.gt.0 .and. ilax2.le.nu)  val2mi = u(ilax2)
      else
         erp(it) = eros
         if (ilax2.gt.0 .and. ilax2.le.nu)  val2pl = u(ilax2)
      endif
  500 continue
c                                        . . parameter finished. reset v
c                       normal termination
      itaur = 1
      call mnfree(1)
      do 550 j= 1, mpar*(mpar+1)/2
  550 vhmat(j) = vthmat(j)
      do 595 i= 1, mpar
      werr(i) = w(i)
      globcc(i) = gcc(i)
  595 x(i) = xt(i)
      call mninex (x)
      edm = sigsav
      amin = abest
      isw(2) = isw2
      isw(4) = isw4
      dcovar = dc
      go to 700
c                       new minimum
  650 lnewmn = .true.
      isw(2) = 0
      dcovar = 1.
      isw(4) = 0
      sav = u(ilax)
      itaur = 1
      call mnfree(1)
      u(ilax) = sav
      call mnexin(x)
      edm = bigedm
c                       in any case
  700 continue
      itaur = 0
      nfcnmx = nfmxin
      istrat = istrav
      return
      end
cdeck  id>, mnparm. 
      subroutine mnparm(k,cnamj,uk,wk,a,b,ierflg)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called from mnread and user-callable
cc    implements one parameter definition, that is:
cc          k     (external) parameter number
cc          cnamk parameter name
cc          uk    starting value
cc          wk    starting step size or uncertainty
cc          a, b  lower and upper physical parameter limits
cc    and sets up (updates) the parameter lists.
cc    output: ierflg=0 if no problems
cc                  >0 if mnparm unable to implement definition
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character*(*) cnamj
      character  cnamk*10, chbufi*4
c
      cnamk = cnamj
      kint = npar
      if (k.lt.1 .or. k.gt.maxext) then
c                     parameter number exceeds allowed maximum value
        write (isyswr,9)  k,maxext
    9   format (/' minuit user error.  parameter number is',i11/
     +         ',  allowed range is one to',i4/)
        go to 800
      endif
c                     normal parameter request
      ktofix = 0
      if (nvarl(k) .lt. 0) go to 50
c         previously defined parameter is being redefined
c                                     find if parameter was fixed
      do 40 ix= 1, npfix
      if (ipfix(ix) .eq. k)  ktofix = k
   40 continue
      if (ktofix .gt. 0)  then
         call mnwarn('w','param def','redefining a fixed parameter.')
         if (kint .ge. maxint)  then
            write (isyswr,'(a)') ' cannot release. max npar exceeded.'
            go to 800
            endif
         call mnfree(-k)
         endif
c                       if redefining previously variable parameter
      if(niofex(k) .gt. 0) kint = npar-1
   50 continue
c
c                                      . . .print heading
      if (lphead .and. isw(5).ge.0) then
        write (isyswr,61)
        lphead = .false.
      endif
   61 format(/' parameter definitions:'/
     +        '    no.   name         value      step size      limits')
      if (wk .gt. zero)  go to 122
c                                        . . .constant parameter . . . .
      if (isw(5) .ge. 0)  write (isyswr, 82)  k,cnamk,uk
   82 format (1x,i5,1x,1h',a10,1h',1x,g13.5, '  constant')
      nvl = 0
      go to 200
  122 if (a.eq.zero .and. b.eq.zero) then
c                                      variable parameter without limits
      nvl = 1
      if (isw(5) .ge. 0)  write (isyswr, 127)  k,cnamk,uk,wk
  127 format (1x,i5,1x,1h',a10,1h',1x,2g13.5, '     no limits')
      else
c                                         variable parameter with limits
      nvl = 4
      lnolim = .false.
      if (isw(5) .ge. 0)  write (isyswr, 132)  k,cnamk,uk,wk,a,b
  132 format(1x,i5,1x,1h',a10,1h',1x,2g13.5,2x,2g13.5)
      endif
c                             . . request for another variable parameter
      kint = kint + 1
      if (kint .gt. maxint)  then
         write (isyswr,135)  maxint
  135    format (/' minuit user error.   too many variable parameters.'/
     +   ' this version of minuit dimensioned for',i4//)
         go to 800
         endif
      if (nvl .eq. 1)  go to 200
      if (a .eq. b)  then
        write (isyswr,'(/a,a/a/)') ' user error in minuit parameter',
     +   ' definition',' upper and lower limits equal.'
        go to 800
        endif
      if (b .lt. a) then
         sav = b
         b = a
         a = sav
         call mnwarn('w','param def','parameter limits were reversed.')
         if (lwarn) lphead=.true.
         endif
      if ((b-a) .gt. 1.0e7)  then
         write (chbufi,'(i4)') k
         call mnwarn('w','param def',
     +               'limits on param'//chbufi//' too far apart.')
         if (lwarn) lphead=.true.
      endif
      danger = (b-uk)*(uk-a)
      if (danger .lt. 0.)
     +     call mnwarn('w','param def','starting value outside limits.')
      if (danger .eq. 0.)
     +     call mnwarn('w','param def','starting value is at limit.')
  200 continue
c                           . . . input ok, set values, arrange lists,
c                                    calculate step sizes gstep, dirin
      cfrom = 'parametr'
      nfcnfr = nfcn
      cstatu= 'new values'
      nu = max(nu,k)
      cpnam(k) = cnamk
      u(k) = uk
      alim(k) = a
      blim(k) = b
      nvarl(k) = nvl
      call mnrset(1)
c                             k is external number of new parameter
c           lastin is the number of var. params with ext. param. no.< k
      lastin = 0
      do 240 ix= 1, k-1
      if (niofex(ix) .gt. 0)  lastin=lastin+1
  240 continue
c                 kint is new number of variable params, npar is old
      if (kint .eq. npar)  go to 280
      if (kint .gt. npar) then
c                          insert new variable parameter in list
         do 260 in= npar,lastin+1,-1
         ix = nexofi(in)
         niofex(ix) = in+1
         nexofi(in+1)= ix
         x    (in+1) = x    (in)
         xt   (in+1) = xt   (in)
         dirin(in+1) = dirin(in)
         g2   (in+1) = g2   (in)
         gstep(in+1) = gstep(in)
  260    continue
      else
c                          remove variable parameter from list
         do 270 in= lastin+1,kint
         ix = nexofi(in+1)
         niofex(ix) = in
         nexofi(in)= ix
         x     (in)= x    (in+1)
         xt    (in)= xt   (in+1)
         dirin (in)= dirin(in+1)
         g2    (in)= g2   (in+1)
         gstep (in)= gstep(in+1)
  270    continue
      endif
  280 continue
      ix = k
      niofex(ix) = 0
      npar = kint
c                                       lists are now arranged . . . .
      if (nvl .gt. 0)  then
         in = lastin+1
         nexofi(in) = ix
         niofex(ix) = in
         sav = u(ix)
         call mnpint(sav,ix,pinti)
         x(in) = pinti
         xt(in) = x(in)
         werr(in) = wk
         sav2 = sav + wk
         call mnpint(sav2,ix,pinti)
         vplu = pinti - x(in)
         sav2 = sav - wk
         call mnpint(sav2,ix,pinti)
         vminu = pinti - x(in)
         dirin(in) = 0.5 * (abs(vplu) +abs(vminu))
         g2(in) = 2.0*up / dirin(in)**2
         gsmin = 8.*epsma2*abs(x(in))
         gstep(in) = max (gsmin, 0.1*dirin(in))
         if (amin .ne. undefi) then
             small = dsqrt(epsma2*(amin+up)/up)
             gstep(in) = max(gsmin, small*dirin(in))
         endif
         grd  (in) = g2(in)*dirin(in)
c                   if parameter has limits
         if (nvarl(k) .gt. 1) then
            if (gstep(in).gt. 0.5)  gstep(in)=0.5
            gstep(in) = -gstep(in)
         endif
      endif
      if (ktofix .gt. 0)  then
         kinfix = niofex(ktofix)
         if (kinfix .gt. 0)  call mnfixp(kinfix,ierr)
         if (ierr .gt. 0)  go to 800
      endif
      ierflg = 0
      return
c                   error on input, unable to implement request  . . . .
  800 continue
      ierflg = 1
      return
      end
cdeck  id>, mnpfit. 
      subroutine mnpfit(parx2p,pary2p,npar2p,coef2p,sdev2p)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
c
c     to fit a parabola to npar2p points
c
c   npar2p   no. of points
c   parx2p(i)   x value of point i
c   pary2p(i)   y value of point i
c
c   coef2p(1...3)  coefficients of the fitted parabola
c   y=coef2p(1) + coef2p(2)*x + coef2p(3)*x**2
c   sdev2p= variance
c   method : chi**2 = min equation solved explicitly
      dimension parx2p(npar2p),pary2p(npar2p),coef2p(npar2p)
      dimension cz(3)
c
      do 3  i=1,3
    3 cz(i)=0.
      sdev2p=0.
      if(npar2p.lt.3) go to 10
      f=npar2p
c--- center x values for reasons of machine precision
      xm=0.
      do 2  i=1,npar2p
    2 xm=xm+parx2p(i)
      xm=xm/f
      x2=0.
      x3=0.
      x4=0.
      y=0.
      y2=0.
      xy=0.
      x2y=0.
      do 1  i=1,npar2p
      s=parx2p(i)-xm
      t=pary2p(i)
      s2=s*s
      x2=x2+s2
      x3=x3+s*s2
      x4=x4+s2*s2
      y=y+t
      y2=y2+t*t
      xy=xy+s*t
      x2y=x2y+s2*t
    1 continue
      a=(f*x4-x2**2)*x2-f*x3**2
      if(a.eq.0.)  goto 10
      cz(3)=(x2*(f*x2y-x2*y)-f*x3*xy)/a
      cz(2)=(xy-x3*cz(3))/x2
      cz(1)=(y-x2*cz(3))/f
      if(npar2p.eq.3)  goto 6
      sdev2p=y2-(cz(1)*y+cz(2)*xy+cz(3)*x2y)
      if(sdev2p.lt.0.)  sdev2p=0.
      sdev2p=sdev2p/(f-3.)
    6 cz(1)=cz(1)+xm*(xm*cz(3)-cz(2))
      cz(2)=cz(2)-2.*xm*cz(3)
   10 continue
      do 11  i=1,3
   11 coef2p(i)=cz(i)
      return
      end
cdeck  id>, mnpint. 
      subroutine mnpint(pexti,i,pinti)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        calculates the internal parameter value pinti corresponding
cc        to the external value pexti for parameter i.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      logical limloc
      character chbufi*4, chbuf2*30
      limloc = .false.
      pinti = pexti
      igo = nvarl(i)
      if (igo .eq. 4)  then
c--                          there are two limits
        alimi = alim(i)
        blimi = blim(i)
        yy=2.0*(pexti-alimi)/(blimi-alimi) - 1.0
        yy2 = yy**2
        if (yy2 .ge. (1.0- epsma2))  then
           if (yy .lt. 0.) then
               a = vlimlo
               chbuf2 = ' is at its lower allowed limit.'
           else
               a = vlimhi
               chbuf2 = ' is at its upper allowed limit.'
           endif
           pinti = a
           pexti = alimi + 0.5* (blimi-alimi) *(dsin(a) +1.0)
           limset = .true.
           write (chbufi,'(i4)') i
           if (yy2 .gt. 1.0) chbuf2 = ' brought back inside limits.'
           call mnwarn('w',cfrom,'variable'//chbufi//chbuf2)
         else
           pinti = dasin(yy)
         endif
      endif
      return
      end
cdeck  id>, mnplot. 
      subroutine mnplot(xpt,ypt,chpt,nxypt,nunit,npagwd,npagln)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        plots points in array xypt onto one page with labelled axes
cc        nxypt is the number of points to be plotted
cc        xpt(i) = x-coord. of ith point
cc        ypt(i) = y-coord. of ith point
cc        chpt(i) = character to be plotted at this position
cc        the input point arrays xpt, ypt, chpt are destroyed.
cc
      dimension   xpt(*), ypt(*), sav(2)
      character*1 chpt(*) ,  chsav,  chbest, cdot, cslash, cblank
      parameter (maxwid=100)
      character cline*100, chmess*30
      dimension xvalus(12)
      logical overpr
      data cdot,cslash,cblank/ '.' , '/' , ' '/
      maxnx = min(npagwd-20,maxwid)
      if (maxnx .lt. 10)  maxnx = 10
      maxny = npagln
      if (maxny .lt. 10)  maxny = 10
      if (nxypt .le. 1)  return
      xbest = xpt(1)
      ybest = ypt(1)
      chbest = chpt(1)
c         order the points by decreasing y
      km1 = nxypt - 1
      do 150 i= 1, km1
      iquit = 0
      ni = nxypt - i
      do 140 j= 1, ni
      if (ypt(j) .gt. ypt(j+1)) go to 140
        savx = xpt(j)
        xpt(j) = xpt(j+1)
        xpt(j+1) = savx
        savy = ypt(j)
        ypt(j) = ypt(j+1)
        ypt(j+1) = savy
        chsav = chpt(j)
        chpt(j) = chpt(j+1)
        chpt(j+1) = chsav
      iquit = 1
  140 continue
      if (iquit .eq. 0) go to 160
  150 continue
  160 continue
c         find extreme values
      xmax = xpt(1)
      xmin = xmax
      do 200 i= 1, nxypt
        if (xpt(i) .gt. xmax)  xmax = xpt(i)
        if (xpt(i) .lt. xmin)  xmin = xpt(i)
  200 continue
      dxx = 0.001*(xmax-xmin)
      xmax = xmax + dxx
      xmin = xmin - dxx
      call mnbins(xmin,xmax,maxnx,xmin,xmax,nx,bwidx)
      ymax = ypt(1)
      ymin = ypt(nxypt)
      if (ymax .eq. ymin)  ymax=ymin+1.0
      dyy = 0.001*(ymax-ymin)
      ymax = ymax + dyy
      ymin = ymin - dyy
      call mnbins(ymin,ymax,maxny,ymin,ymax,ny,bwidy)
      any = ny
c         if first point is blank, it is an 'origin'
      if (chbest .eq. cblank)  go to 50
      xbest = 0.5 * (xmax+xmin)
      ybest = 0.5 * (ymax+ymin)
   50 continue
c         find scale constants
      ax = 1.0/bwidx
      ay = 1.0/bwidy
      bx = -ax*xmin + 2.0
      by = -ay*ymin - 2.0
c         convert points to grid positions
      do 300 i= 1, nxypt
      xpt(i) = ax*xpt(i) + bx
  300 ypt(i) = any-ay*ypt(i) - by
      nxbest = ax*xbest + bx
      nybest = any  - ay*ybest - by
c         print the points
      ny = ny + 2
      nx = nx + 2
      isp1 = 1
      linodd = 1
      overpr=.false.
      do 400 i= 1, ny
      do 310 ibk= 1, nx
  310 cline (ibk:ibk) = cblank
      cline(1:1) = cdot
      cline(nx:nx) = cdot
      cline(nxbest:nxbest) = cdot
      if (i.ne.1 .and. i.ne.nybest .and. i.ne.ny)  go to 320
      do 315 j= 1, nx
  315 cline(j:j) = cdot
  320 continue
      yprt = ymax - float(i-1)*bwidy
      if (isp1 .gt. nxypt)  go to 350
c         find the points to be plotted on this line
        do 341 k= isp1,nxypt
      ks = ypt(k)
      if (ks .gt. i)  go to 345
      ix = xpt(k)
      if (cline(ix:ix) .eq.   cdot)  go to 340
      if (cline(ix:ix) .eq. cblank)  go to 340
      if (cline(ix:ix) .eq.chpt(k))  go to 341
      overpr = .true.
c         overpr is true if one or more positions contains more than
c            one point
      cline(ix:ix) = '&'
      go to 341
  340 cline(ix:ix) = chpt(k)
  341 continue
        isp1 = nxypt + 1
        go to 350
  345   isp1 = k
  350 continue
      if (linodd .eq. 1 .or. i .eq. ny)  go to 380
      linodd = 1
      write (nunit, '(18x,a)')       cline(:nx)
      go to 400
  380 write (nunit,'(1x,g14.7,a,a)') yprt, ' ..', cline(:nx)
      linodd = 0
  400 continue
c         print labels on x-axis every ten columns
      do 410 ibk= 1, nx
      cline(ibk:ibk) = cblank
      if (mod(ibk,10) .eq. 1)  cline(ibk:ibk) = cslash
  410 continue
      write (nunit, '(18x,a)')       cline(:nx)
c
      do 430 ibk= 1, 12
  430 xvalus(ibk) = xmin + float(ibk-1)*10.*bwidx
      iten = (nx+9) / 10
      write (nunit,'(12x,12g10.4)')  (xvalus(ibk), ibk=1,iten)
      chmess = ' '
      if (overpr) chmess='   overprint character is &'
      write (nunit,'(25x,a,g13.7,a)') 'one column=',bwidx, chmess
  500 return
      end
cdeck  id>, mnpout. 
      subroutine mnpout(iuext,chnam,val,err,xlolim,xuplim,iuint)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc     user-called
cc   provides the user with information concerning the current status
cc          of parameter number iuext. namely, it returns:
cc        chnam: the name of the parameter
cc        val: the current (external) value of the parameter
cc        err: the current estimate of the parameter uncertainty
cc        xlolim: the lower bound (or zero if no limits)
cc        xuplim: the upper bound (or zero if no limits)
cc        iuint: the internal parameter number (or zero if not variable,
cc           or negative if undefined).
cc  note also:  if iuext is negative, then it is -internal parameter
cc           number, and iuint is returned as the external number.
cc     except for iuint, this is exactly the inverse of mnparm
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character*(*) chnam
      xlolim = 0.
      xuplim = 0.
      err = 0.
      if (iuext .eq. 0)  go to 100
      if (iuext .lt. 0)  then
c                   internal parameter number specified
         iint = -iuext
         if (iint .gt. npar) go to 100
         iext = nexofi(iint)
         iuint = iext
      else
c                    external parameter number specified
         iext = iuext
         if (iext .eq. 0)   go to 100
         if (iext .gt. nu)  go to 100
         iint = niofex(iext)
         iuint = iint
      endif
c                     in both cases
         nvl = nvarl(iext)
         if (nvl .lt. 0) go to 100
      chnam = cpnam(iext)
      val = u(iext)
      if (iint .gt. 0)  err = werr(iint)
      if (nvl .eq. 4) then
         xlolim = alim(iext)
         xuplim = blim(iext)
      endif
      return
c                parameter is undefined
  100 iuint = -1
      chnam = 'undefined'
      val = 0.
      return
      end
cdeck  id>, mnprin. 
      subroutine mnprin  (inkode,fval)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        prints the values of the parameters at the time of the call.
cc        also prints other relevant information such as function value,
cc        estimated distance to minimum, parameter errors, step sizes.
cc
c         according to the value of ikode, the printout is:
c    ikode=inkode= 0    only info about function value
c                  1    parameter values, errors, limits
c                  2    values, errors, step sizes, internal values
c                  3    values, errors, step sizes, first derivs.
c                  4    values, parabolic errors, minos errors
c    when inkode=5, mnprin chooses ikode=1,2, or 3, according to isw(2)
c
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c
      character*14 colhdu(6),colhdl(6), cx2,cx3,cgetx
      character*11 cnambf, cblank
      character  chedm*10, cheval*15
      parameter (cgetx='please get x..')
      data cblank/'          '/
c
      if (nu .eq. 0)  then
       write (isyswr,'(a)') ' there are currently no parameters defined'
       go to 700
      endif
c                  get value of ikode based in inkode, isw(2)
      ikode = inkode
      if (inkode .eq. 5) then
         ikode = isw(2)+1
         if (ikode .gt. 3)  ikode=3
      endif
c                  set 'default' column headings
      do 5 k= 1, 6
      colhdu(k) = 'undefined'
    5 colhdl(k) = 'column head'
c              print title if minos errors, and title exists.
      if (ikode.eq.4 .and. ctitl.ne.cundef)
     +            write (isyswr,'(/a,a)')  ' minuit task: ',ctitl
c              report function value and status
      if (fval .eq. undefi) then
         cheval = ' unknown       '
      else
         write (cheval,'(g15.7)') fval
      endif
         if (edm .eq. bigedm) then
            chedm = ' unknown  '
         else
            write (chedm, '(e10.2)') edm
         endif
      nc = nfcn-nfcnfr
      write (isyswr,905)  cheval,cfrom,cstatu,nc,nfcn
  905 format (/' fcn=',a,' from ',a8,'  status=',a10,i6,' calls',
     +         i9,' total')
      m = isw(2)
      if (m.eq.0 .or. m.eq.2 .or. dcovar.eq.zero) then
        write (isyswr,907) chedm,istrat,covmes(m)
  907   format (21x,'edm=',a,'    strategy=',i2,6x,a)
      else
        dcmax = 1.
        dc = min(dcovar,dcmax) * 100.
        write (isyswr,908) chedm,istrat,dc
  908   format (21x,'edm=',a,'  strategy=',i1,'  error matrix',
     +     ' uncertainty=',f5.1,'%')
      endif
c
      if (ikode .eq. 0)  go to 700
c               find longest name (for rene!)
      ntrail = 10
      do 20 i= 1, nu
         if (nvarl(i) .lt. 0)  go to 20
         do 15 ic= 10,1,-1
            if (cpnam(i)(ic:ic) .ne. ' ') go to 16
   15    continue
         ic = 1
   16    lbl = 10-ic
         if (lbl .lt. ntrail)  ntrail=lbl
   20 continue
      nadd = ntrail/2 + 1
      if (ikode .eq. 1)  then
         colhdu(1) = '              '
         colhdl(1) = '      error   '
         colhdu(2) = '      physical'
         colhdu(3) = ' limits       '
         colhdl(2) = '    negative  '
         colhdl(3) = '    positive  '
      endif
      if (ikode .eq. 2)  then
         colhdu(1) = '              '
         colhdl(1) = '      error   '
         colhdu(2) = '    internal  '
         colhdl(2) = '    step size '
         colhdu(3) = '    internal  '
         colhdl(3) = '      value   '
      endif
      if (ikode .eq. 3)  then
         colhdu(1) = '              '
         colhdl(1) = '      error   '
         colhdu(2) = '       step   '
         colhdl(2) = '       size   '
         colhdu(3) = '      first   '
         colhdl(3) = '   derivative '
      endif
      if (ikode .eq. 4)  then
         colhdu(1) = '    parabolic '
         colhdl(1) = '      error   '
         colhdu(2) = '        minos '
         colhdu(3) = 'errors        '
         colhdl(2) = '   negative   '
         colhdl(3) = '   positive   '
      endif
c
      if (ikode .ne. 4)  then
         if (isw(2) .lt. 3) colhdu(1)='  approximate '
         if (isw(2) .lt. 1) colhdu(1)=' current guess'
      endif
      ncol = 3
      write (isyswr, 910) (colhdu(kk),kk=1,ncol)
      write (isyswr, 911) (colhdl(kk),kk=1,ncol)
  910 format (/'  ext parameter ',     13x       ,6a14)
  911 format ( '  no.   name    ','    value    ',6a14)
c
c                                        . . . loop over parameters . .
      do 200 i= 1, nu
      if (nvarl(i) .lt. 0)  go to 200
      l = niofex(i)
      cnambf = cblank(1:nadd)//cpnam(i)
      if (l .eq. 0)  go to 55
c              variable parameter.
      x1 = werr(l)
      cx2 = cgetx
      cx3 = cgetx
      if (ikode .eq. 1) then
         if (nvarl(i) .le. 1) then
            write (isyswr, 952)  i,cnambf,u(i),x1
            go to 200
         else
         x2 = alim(i)
         x3 = blim(i)
         endif
      endif
      if (ikode .eq. 2) then
         x2 = dirin(l)
         x3 = x(l)
      endif
      if (ikode .eq. 3) then
         x2 = dirin(l)
         x3 = grd(l)
         if (nvarl(i).gt.1 .and. abs(dcos(x(l))) .lt. 0.001)
     +      cx3 = '** at limit **'
      endif
      if (ikode .eq. 4) then
         x2 = ern(l)
           if (x2.eq.zero)   cx2=' '
           if (x2.eq.undefi) cx2='   at limit   '
         x3 = erp(l)
           if (x3.eq.zero)   cx3=' '
           if (x3.eq.undefi) cx3='   at limit   '
      endif
      if (cx2.eq.cgetx) write (cx2,'(g14.5)') x2
      if (cx3.eq.cgetx) write (cx3,'(g14.5)') x3
      write (isyswr,952)   i,cnambf,u(i),x1,cx2,cx3
  952 format (i4,1x,a11,2g14.5,2a)
c               check if parameter is at limit
      if (nvarl(i) .le. 1 .or. ikode .eq. 3)  go to 200
      if (abs(dcos(x(l))) .lt. 0.001)  write (isyswr,1004)
 1004 format (1h ,32x,42hwarning -   - above parameter is at limit.)
      go to 200
c
c                                print constant or fixed parameter.
   55 continue
                          colhdu(1) = '   constant   '
      if (nvarl(i).gt.0)  colhdu(1) = '     fixed    '
      if (nvarl(i).eq.4 .and. ikode.eq.1) then
        write (isyswr,'(i4,1x,a11,g14.5,a,2g14.5)')
     +     i,cnambf,u(i),colhdu(1),alim(i),blim(i)
      else
        write (isyswr,'(i4,1x,a11,g14.5,a)')  i,cnambf,u(i),colhdu(1)
      endif
  200 continue
c
      if (up.ne.updflt)  write (isyswr,'(31x,a,g10.2)') 'err def=',up
  700 continue
      return
      end
cdeck  id>, mnpsdf. 
      subroutine mnpsdf
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        calculates the eigenvalues of v to see if positive-def.
cc        if not, adds constant along diagonal to make positive.
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character chbuff*12
      dimension s(mni)
      epsmin = 1.0e-6
      epspdf = max(epsmin, epsma2)
      dgmin = vhmat(1)
c                        check if negative or zero on diagonal
      do 200 i= 1, npar
      ndex = i*(i+1)/2
      if (vhmat(ndex) .le. zero) then
          write (chbuff(1:3),'(i3)') i
          call mnwarn('w',cfrom,
     +'negative diagonal element'//chbuff(1:3)//' in error matrix')
      endif
      if (vhmat(ndex) .lt. dgmin)  dgmin = vhmat(ndex)
  200 continue
      if (dgmin .le. 0.) then
         dg = 1.0 - dgmin
         write (chbuff,'(e12.2)') dg
         call mnwarn('w',cfrom,
     +     chbuff//' added to diagonal of error matrix')
      else
         dg = 0.
      endif
c                    store vhmat in p, make sure diagonal pos.
      do 213 i= 1, npar
      ndex = i*(i-1)/2
      ndexd = ndex + i
      vhmat(ndexd) = vhmat(ndexd) + dg
      s(i) = 1.0/dsqrt(vhmat(ndexd))
      do 213 j= 1, i
      ndex =  ndex + 1
  213 p(i,j) = vhmat(ndex) * s(i)*s(j)
c      call eigen (p,p,maxint,npar,pstar,-npar)
      call mneig(p,maxint,npar,maxint,pstar,epspdf,ifault)
      pmin = pstar(1)
      pmax = pstar(1)
      do 215 ip= 2, npar
      if (pstar(ip) .lt. pmin)  pmin = pstar(ip)
      if (pstar(ip) .gt. pmax)  pmax = pstar(ip)
  215 continue
      pmax = max(abs(pmax), one)
      if ((pmin .le. zero .and. lwarn) .or.  isw(5) .ge. 2) then
         write (isyswr,550)
         write (isyswr,551) (pstar(ip),ip=1,npar)
      endif
      if (pmin .gt. epspdf*pmax)  go to 217
      if (isw(2) .eq. 3)  isw(2)=2
      padd = 1.0e-3*pmax - pmin
      do 216 ip= 1, npar
      ndex = ip*(ip+1)/2
  216 vhmat(ndex) = vhmat(ndex) *(1.0 + padd)
      cstatu= 'not posdef'
      write (chbuff,'(g12.5)') padd
      call mnwarn('w',cfrom,
     +   'matrix forced pos-def by adding '//chbuff//' to diagonal.')
  217 continue
c
  550 format (' eigenvalues of second-derivative matrix:' )
  551 format (7x,6e12.4)
      return
      end
cdeck  id>, mnrazz. 
      subroutine mnrazz(ynew,pnew,y,jh,jl)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called only by mnsimp (and mnimpr) to add a new point
cc        and remove an old one from the current simplex, and get the
cc        estimated distance to minimum.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension pnew(*), y(*)
      do 10 i=1,npar
   10 p(i,jh) = pnew(i)
      y(jh)=ynew
      if(ynew .lt. amin) then
        do 15 i=1,npar
   15   x(i) = pnew(i)
        call mninex(x)
        amin = ynew
        cstatu = 'progress  '
        jl=jh
      endif
      jh = 1
      nparp1 = npar+1
   20 do 25 j=2,nparp1
      if (y(j) .gt. y(jh))  jh = j
   25 continue
      edm = y(jh) - y(jl)
      if (edm .le. zero)  go to 45
      us = 1.0/edm
      do 35 i= 1, npar
      pbig = p(i,1)
      plit = pbig
      do 30 j= 2, nparp1
      if (p(i,j) .gt. pbig)  pbig = p(i,j)
      if (p(i,j) .lt. plit)  plit = p(i,j)
   30 continue
      dirin(i) = pbig - plit
   35 continue
   40 return
   45 write (isyswr, 1000)  npar
      go to 40
 1000 format ('   function value does not seem to depend on any of the',
     +    i3,' variable parameters.' /10x,'verify that step sizes are',
     +    ' big enough and check fcn logic.'/1x,79(1h*)/1x,79(1h*)/)
      end
cdeck  id>, mnread. 
      subroutine mnread(fcn,iflgin,iflgut,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called from minuit.  reads all user input to minuit.
cc     this routine is highly unstructured and defies normal logic.
cc
cc     iflgin indicates the function originally requested:
cc           = 1: read one-line title
cc             2: read parameter definitions
cc             3: read minuit commands
cc
cc     iflgut= 1: reading terminated normally
cc             2: end-of-data on input
cc             3: unrecoverable read error
cc             4: unable to process parameter requests
cc internally,
cc     iflgdo indicates the subfunction to be performed on the next
cc         input record: 1: read a one-line title
cc                       2: read a parameter definition
cc                       3: read a command
cc                       4: read in covariance matrix
cc     for example, when iflgin=3, but iflgdo=1, then it should read
cc       a title, but this was requested by a command, not by minuit.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension plist(maxp)
      character cnamk*10, crdbuf*80, celmnt*20
      character comand*(maxcwd)
      character cpromt(3)*40, clower*26, cupper*26
      logical leof
      data cpromt/' enter minuit title, or "set input n" : ',
     +            ' enter minuit parameter definition:     ',
     +            ' enter minuit command:                  '/
c
      data clower/'abcdefghijklmnopqrstuvwxyz'/
      data cupper/'abcdefghijklmnopqrstuvwxyz'/
c
      iflgut = 1
      iflgdo = iflgin
      ifatal = 0
      leof = .false.
c                                           . . . . read next record
   10 continue
      if (isw(6) .eq. 1) write (isyswr,'(a)') cpromt(iflgdo)
      crdbuf = '   '
      read (isysrd,'(a)',err=500,end=45)  crdbuf
c                                           . .   preemptive commands
      leof = .false.
      if (index(crdbuf,'*eof') .eq. 1 .or.
     +    index(crdbuf,'*eof') .eq. 1)    then
         write (isyswr,'(a,i3)') ' *eof encountered on unit no.',isysrd
         lphead = .true.
         go to 50
         endif
      if (index(crdbuf,'set inp') .eq. 1 .or.
     +    index(crdbuf,'set inp') .eq. 1)    then
         icomnd = icomnd + 1
         write (isyswr, 21) icomnd,crdbuf(1:50)
   21    format (' **********'/' **',i5,' **',a/' **********')
         lphead = .true.
         go to 50
         endif
      go to 80
c                                    . . hardware eof on current isysrd
   45 crdbuf = '*eof '
      write (isyswr,'(a,i3)') ' end of data on unit no.',isysrd
c                                     or set input command
   50 continue
         call mnstin(crdbuf,ierr)
         if (ierr .eq. 0)  go to 10
         if (ierr .eq. 2)  then
            if (.not. leof) then
               write (isyswr,'(a,a/)') ' two consecutive eofs on ',
     +              'primary input file will terminate execution.'
               leof = .true.
               go to 10
            endif
         endif
         iflgut = ierr
         go to 900
   80 if (iflgdo .gt. 1) go to 100
c                            read title        . . . . .   iflgdo = 1
c              if title is 'set title', skip and read again
      if (index(crdbuf,'set tit') .eq. 1)  go to 10
      if (index(crdbuf,'set tit') .eq. 1)  go to 10
      ctitl = crdbuf(1:50)
      write (isyswr,'(1x,a50)')  ctitl
      write (isyswr,'(1x,78(1h*))')
         lphead = .true.
      if (iflgin .eq. iflgdo)  go to 900
      iflgdo = iflgin
      go to 10
c                            data record is not a title. get upper case
  100 continue
      do 110 i= 1, maxcwd
      if (crdbuf(i:i) .eq. '''') go to 111
         do 108 ic= 1, 26
         if (crdbuf(i:i) .eq. clower(ic:ic)) crdbuf(i:i)=cupper(ic:ic)
  108    continue


  110 continue
  111 continue
c                            read parameter definitions.   iflgdo = 2
      if (iflgdo .gt. 2)  go to 300
c              if parameter def is 'parameter', skip and read again
      if (index(crdbuf,'par') .eq. 1)  go to 10
c              if line starts with set title, read a title first
      if (index(crdbuf,'set tit') .eq. 1)  then
         iflgdo = 1
         go to 10
         endif
c              find out whether fixed or free-field format
      kapo1 = index(crdbuf,'''')
      if (kapo1 .eq. 0)  go to 150
      kapo2 = index(crdbuf(kapo1+1:),'''')
      if (kapo2 .eq. 0)  go to 150
c          new (free-field) format
      kapo2 = kapo2 + kapo1
c                             skip leading blanks if any
         do 115 istart=1, kapo1-1
         if (crdbuf(istart:istart) .ne. ' ')  go to 120
  115    continue
         istart = kapo1-1
  120 continue
c                               parameter number integer
      if (istart .lt. 1)  go to 210
      celmnt = crdbuf(istart:kapo1-1)
      read (celmnt,'(bn,f20.0)',err=180) fk
      k = fk
      if (k .eq. 0)  go to 210
      cnamk = 'param '//celmnt
      if (kapo2-kapo1 .gt. 1) cnamk = crdbuf(kapo1+1:kapo2-1)
      call mncrck(crdbuf(kapo2+1:),maxcwd,comand,lnc,
     +                             maxp,plist,llist, ierr,isyswr)
      if (ierr .gt. 0)  go to 180
      uk = plist(1)
      wk = 0.
      if (llist .ge. 2)  wk = plist(2)
      a = 0.
      if (llist .ge. 3)  a = plist(3)
      b = 0.
      if (llist .ge. 4)  b = plist(4)
      go to 170
c          old (fixed-field) format
  150 continue
      read (crdbuf, 158,err=180)  xk,cnamk,uk,wk,a,b
  158 format (bn,f10.0, a10, 4f10.0)
      k = xk
      if (k .eq. 0)  go to 210
c          parameter format cracked, implement parameter definition
  170 call mnparm(k,cnamk,uk,wk,a,b,ierr)
      if (ierr .eq. 0)  go to 10
c          format error
  180 continue
      if (isw(6) .eq. 1)  then
          write (isyswr,'(a)') ' format error.  ignored.  enter again.'
      else
          write (isyswr,'(a)') ' error in parameter definition'
          ifatal = ifatal + 1
      endif
      go to 10
c                                       . . . end parameter requests
  210 write (isyswr,'(4x,75(1h*))')
      if (ifatal.gt.0 .and. isw(6).ne.1)  then
         iflgut = 4
         go to 900
      endif
      if (iflgin .eq. iflgdo)  go to 900
      iflgdo = iflgin
      go to 10
c                                              . . . . .   iflgdo = 3
c                                           read commands
  300 continue
c               crack the next command . . . . . . . . . . . . . . . .
         do 350 ipos= 1, 80
         if (crdbuf(ipos:ipos) .ne. ' ') go to 355
  350    continue
      write (isyswr,'(a)') ' blank command ignored.'
      go to 10
  355 ibegin = ipos
      call mncrck(crdbuf(ibegin:),maxcwd,comand,lnc,
     +                            maxp,  plist, llist, ierr,isyswr)
      if (ierr .gt. 0) then
         if (isw(6) .eq. 1) then
            write (isyswr,'(a)') ' command ignored '
            go to 10
         else
            write (isyswr,'(a)') ' command cannot be interpreted'
            go to 500
         endif
      endif
c                    certain commands are trapped here already
         lphead = .true.
         if (index(comand,'par' ) .eq. 1)  go to 440
             if (index(comand,'set') .ne. 1)  go to 370
             if (index(comand,'cov') .eq. 5)  go to 400
             if (index(comand,'tit') .eq. 5)  go to 460
  370 continue
      call mnexcm(fcn,comand(1:lnc),plist,llist,ierr,futil)
         if (comand(1:3).eq.'end')  go to 900
         if (comand(1:3).eq.'exi')  go to 900
         if (comand(1:3).eq.'ret')  go to 900
         if (comand(1:3).eq.'sto')  go to 900
      go to 10
c                                        . . . . . . . . . . set covar
  400 nrape = plist(1)
      icomnd = icomnd + 1
      write (isyswr,405) icomnd,comand(1:lnc),(plist(i),i=1,llist)
  405 format (1h ,10(1h*)/' **',i5,' **',a,4g12.4/20x,5g12.4)
      write (isyswr, '(1h ,10(1h*))' )
      if (nrape .ne. npar)  go to 425
      npar2 = npar*(npar+1)/2
      read (isysrd,420,err=500,end=45)  (vhmat(i),i=1,npar2)
  420 format (bn,7e11.4,3x)
      isw(2) = 3
      dcovar = 0.0
      if (isw(5) .ge. 0)  call mnmatu(1)
      if (isw(5) .ge. 1)  call mnprin(2,amin)
      go to 10
  425 continue
      write (isyswr,428)
  428 format(' size of covariance matrix to be read does not',
     + ' correspond to'/' number of currently variable parameters.',
     + '    command ignored.'/)
      read (isysrd,420,err=500,end=45)  ((dummy,i=1,j),j=1,nrape)
      go to 10
c                                           . . . . . parameter command
  440 continue
      iflgdo = 2
      ifatal = 0
c        go and read parameter definitions
      go to 10
c                                              . . . . set title
  460 continue
      iflgdo = 1
      go to 10
c                                              . . . . error conditions
  500 iflgut = 3
  900 return
      end
cdeck  id>, mnrn15. 
      subroutine mnrn15(val,inseed)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
c         this is a super-portable random number generator.
c         it should not overflow on any 32-bit machine.
c         the cycle is only ~10**9, so use with care!
c         note especially that val must not be undefined on input.
c                    set default starting seed
      parameter (three=3.0)
      data iseed/12345/
      if (val .eq. three)  go to 100
c
      inseed = iseed
      k = iseed/53668
      iseed = 40014*(iseed-k*53668) - k*12211
      if (iseed .lt. 0) iseed = iseed + 2147483563
      val = real(iseed) * 4.656613e-10
      return
c               "entry" to set seed, flag is val=3.
  100 iseed = inseed
      return
      end
cdeck  id>, mnrset. 
      subroutine mnrset(iopt)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called from mncler and whenever problem changes, for example
cc        after set limits, set param, call fcn 6
cc    if iopt=1,
cc        resets function value and errors to undefined
cc    if iopt=0, sets only minos errors to undefined
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      cstatu = 'reset     '
      if (iopt .ge. 1)  then
        amin = undefi
        fval3 = 2.0*abs(amin) + 1.
        edm = bigedm
        isw(4) = 0
        isw(2) = 0
        dcovar = 1.
        isw(1) = 0
      endif
      lnolim = .true.
      do 10 i= 1, npar
      iext = nexofi(i)
      if (nvarl(iext) .ge. 4) lnolim=.false.
      erp(i) = zero
      ern(i) = zero
      globcc(i) = zero
   10 continue
      if (isw(2) .ge. 1)  then
         isw(2) = 1
         dcovar = max(dcovar,half)
      endif
      return
      end
cdeck  id>, mnsave. 
      subroutine mnsave
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       writes current parameter values and step sizes onto file isyssa
cc          in format which can be reread by minuit for restarting.
cc       the covariance matrix is also output if it exists.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension vc(7)
      logical lopen,lname
      character cgname*64, cfname*64, canswr*1
c
      inquire(unit=isyssa,opened=lopen,named=lname,name=cgname)
      if (lopen) then
         if (.not.lname) cgname='unnamed file'
         write (isyswr,32) isyssa,cgname
   32    format (' current values will be saved on unit',i3,': ',a/)
      else
c                new file, open it
         write (isyswr,35) isyssa
   35    format (' unit',i3,' is not opened.')
         if (isw(6) .eq. 1) then
            write (isyswr,'(a)') ' please give file name:'
            read (isysrd,'(a)') cfname
            open (unit=isyssa,file=cfname,status='new',err=600)
            cgname = cfname
         else
            go to 650
         endif
      endif
c                               file is now correctly opened
      if (isw(6) .eq. 1)  then
         write (isyswr,37)  isyssa
   37    format (' should unit',i3,' be rewound before writing to it?' )
         read  (isysrd,'(a)')  canswr
         if (canswr.eq.'y' .or. canswr.eq.'y') rewind isyssa
      endif
c                               and rewound if requested
      write (isyssa,'(10hset title )',err=700)
      write (isyssa,'(a)')  ctitl
      write (isyssa,'(10hparameters)')
      nlines = 3
c                                write out parameter values
      do 200 i= 1, nu
      if (nvarl(i) .lt. 0)  go to 200
      nlines = nlines + 1
      iint = niofex(i)
      if (nvarl(i) .gt. 1)  go to 100
c         parameter without limits
      write (isyssa,1001)  i,cpnam(i),u(i),werr(iint)
      go to 200
c         parameter with limits
  100 continue
      write (isyssa,1001) i,cpnam(i),u(i),werr(iint),alim(i),blim(i)
 1001 format (1x,i5,1h',a10,1h',4e13.5)
  200 continue
      write (isyssa,'(a)')  ' '
      nlines = nlines + 1
c                                  write out covariance matrix, if any
      if (isw(2) .lt. 1)  go to 750
      write (isyssa,1003,err=700)  npar
 1003 format ('set covariance',i6)
      npar2 = npar*(npar+1)/2
      write (isyssa,1004) (vhmat(i),i=1,npar2)
 1004 format (bn,7e11.4,3x)
      ncovar = npar2/7 + 1
      if (mod(npar2,7) .gt. 0)  ncovar = ncovar + 1
      nlines = nlines + ncovar
      write (isyswr, 501) nlines,isyssa,cgname(1:45)
  501 format (1x,i5,' records written to unit',i4,':',a)
      if (ncovar .gt. 0) write (isyswr, 502) ncovar
  502 format (' including',i5,' records for the covariance matrix.'/)
      go to 900
c                                           some error conditions
  600 write (isyswr,'(a,i4)') ' i/o error: unable to open unit',isyssa
      go to 900
  650 write (isyswr,'(a,i4,a)') ' unit',isyssa,' is not opened.'
      go to 900
  700 write (isyswr,'(a,i4)') ' error: unable to write to unit',isyssa
      go to 900
  750 write (isyswr,'(a)') ' there is no covariance matrix to save.'
c
  900 return
      end
cdeck  id>, mnscan. 
      subroutine mnscan(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        scans the values of fcn as a function of one parameter
cc        and plots the resulting values as a curve using mnplot.
cc        it may be called to scan one parameter or all parameters.
cc        retains the best function and parameter values found.
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      xlreq = min(word7(3),word7(4))
      xhreq = max(word7(3),word7(4))
      ncall = word7(2) + 0.01
      if (ncall .le. 1)  ncall = 41
      if (ncall .gt. maxcpt)  ncall = maxcpt
      nccall = ncall
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      iparwd = word7(1) + 0.1
      ipar = max(iparwd, 0)
      iint = niofex(ipar)
      cstatu = 'no change'
      if (iparwd .gt. 0)  go to 200
c
c         equivalent to a loop over parameters requested
  100 ipar = ipar + 1
      if (ipar .gt. nu)  go to 900
      iint = niofex(ipar)
      if (iint .le. 0)  go to 100
c         set up range for parameter ipar
  200 continue
      ubest = u(ipar)
      xpt(1) = ubest
      ypt(1) = amin
      chpt(1)= ' '
      xpt(2) = ubest
      ypt(2) = amin
      chpt(2)= 'x'
      nxypt = 2
      if (nvarl(ipar) .gt. 1)  go to 300
c         no limits on parameter
      if (xlreq .eq. xhreq)  go to 250
      unext = xlreq
      step = (xhreq-xlreq)/float(ncall-1)
      go to 500
  250 continue
      xl = ubest - werr(iint)
      xh = ubest+  werr(iint)
      call mnbins(xl,xh,ncall, unext,uhigh,nbins,step)
      nccall = nbins + 1
      go to 500
c         limits on parameter
  300 continue
      if (xlreq .eq. xhreq)  go to 350
      xl = max(xlreq,alim(ipar))
      xh = min(xhreq,blim(ipar))
      if (xl .ge. xh)  go to 700
      unext = xl
      step = (xh-xl)/float(ncall-1)
      go to 500
  350 continue
      unext = alim(ipar)
      step = (blim(ipar)-alim(ipar))/float(ncall-1)
c         main scanning loop over parameter ipar
  500 continue
      do 600 icall = 1, nccall
      u(ipar) = unext
      nparx = npar
      call fcn(nparx,gin,fnext,u,4,futil)
      nfcn = nfcn + 1
      nxypt = nxypt + 1
      xpt(nxypt) = unext
      ypt(nxypt) = fnext
      chpt(nxypt) = '*'
      if (fnext .lt. amin)  then
        amin = fnext
        ubest = unext
        cstatu= 'improved  '
        endif
  530 continue
      unext = unext + step
  600 continue
c         finished with scan of parameter ipar
      u(ipar) = ubest
      call mnexin(x)
      write (isyswr,1001)  newpag,ipar,cpnam(ipar)
      nunit = isyswr
      call mnplot(xpt,ypt,chpt,nxypt,nunit,npagwd,npagln)
      go to 800
  700 continue
      write (isyswr,1000) ipar
  800 continue
      if (iparwd .le. 0)  go to 100
c         finished with all parameters
  900 continue
      call mnprin(5,amin)
      return
 1000 format (46h requested range outside limits for parameter  ,i3/)
 1001 format (i1,'scan of parameter no.',i3,3h,   ,a10)
      end
cdeck  id>, mnseek. 
      subroutine mnseek(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc   performs a rough (but global) minimization by monte carlo search.
cc        each time a new minimum is found, the search area is shifted
cc        to be centered at the best value.  random points are chosen
cc        uniformly over a hypercube determined by current step sizes.
cc   the metropolis algorithm accepts a worse point with probability
cc      exp(-d/up), where d is the degradation.  improved points
cc      are of course always accepted.  actual steps are random
cc      multiples of the nominal steps (dirin).
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      parameter (twopi=2.0*3.141593)
      dimension step(mni), xbest(mni), xmid(mni)
      mxfail = word7(1)
      if (mxfail .le. 0)  mxfail=100+20*npar
      mxstep = 10*mxfail
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      alpha = word7(2)
      if (alpha .le. zero)  alpha=3.
      if (isw(5) .ge. 1)  write (isyswr, 3) mxfail,mxstep,alpha
    3 format (' mnseek: monte carlo minimization using metropolis',
     + ' algorithm'/' to stop after',i6,' successive failures, or',
     + i7,' steps'/' maximum step size is',f9.3,' error bars.')
      cstatu= 'initial  '
      if (isw(5) .ge. 2)  call mnprin(2,amin)
      cstatu = 'unchanged '
      ifail = 0
      rnum = zero
      rnum1 = zero
      rnum2 = zero
      nparx = npar
      flast = amin
c              set up step sizes, starting values
      do 10 ipar =  1, npar
      iext = nexofi(ipar)
      dirin(ipar) = 2.0*alpha*werr(ipar)
      if (nvarl(iext) .gt. 1)  then
c              parameter with limits
         call mndxdi(x(ipar),ipar,dxdi)
         if (dxdi .eq. zero)  dxdi=1.
         dirin(ipar) = 2.0*alpha*werr(ipar)/dxdi
         if (abs(dirin(ipar)).gt.twopi)  dirin(ipar)=twopi
         endif
      xmid(ipar) = x(ipar)
   10 xbest(ipar) = x(ipar)
c                              search loop
      do 500 istep= 1, mxstep
      if (ifail .ge. mxfail)  go to 600
        do 100 ipar= 1, npar
        call mnrn15(rnum1,iseed)
        call mnrn15(rnum2,iseed)
  100   x(ipar) = xmid(ipar) + 0.5*(rnum1+rnum2-1.)*dirin(ipar)
      call mninex(x)
      call fcn(nparx,gin,ftry,u,4,futil)
      nfcn = nfcn + 1
      if (ftry .lt. flast)  then
         if (ftry .lt. amin)  then
            cstatu = 'improvemnt'
            amin = ftry
            do 200 ib= 1, npar
  200       xbest(ib) = x(ib)
            ifail = 0
            if (isw(5) .ge. 2) call mnprin(2,amin)
            endif
         go to 300
      else
         ifail = ifail + 1
c                   metropolis algorithm
         bar = exp((amin-ftry)/up)
         call mnrn15(rnum,iseed)
         if (bar .lt. rnum)  go to 500
      endif
c                    accept new point, move there
  300 continue
      do 350 j= 1, npar
      xmid(j) = x(j)
  350 continue
      flast = ftry
  500 continue
c                               end search loop
  600 continue
      if (isw(5) .gt. 1) write (isyswr,601) ifail
  601 format(' mnseek:',i5,' successive unsuccessful trials.')
      do 700 ib= 1, npar
  700 x(ib) = xbest(ib)
      call mninex(x)
      if (isw(5) .ge. 1)  call mnprin(2,amin)
      if (isw(5) .eq. 0)  call mnprin(0,amin)
      return
      end
cdeck  id>, mnset.  
      subroutine mnset(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        called from mnexcm
cc        interprets the commands that start with set and show
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c
      external fcn,futil
c        file characteristics for set input
      logical lopen,lname
      character*1 canswr
      character cfname*64, cmode*16
c       'set ' or 'show',  'on ' or 'off', 'suppressed' or 'reported  '
      character ckind*4,    copt*3,         cwarn*10
c        explanation of print level numbers -1:3  and strategies 0:2
      character cprlev(-1:3)*34 ,cstrat(0:2)*44
c        identification of debug options
      parameter (numdbg = 6)
      character*40 cdbopt(0:numdbg)
c        things that can be set or shown
      character*10 cname(30)
      data cname( 1)/'fcn value '/
      data cname( 2)/'parameters'/
      data cname( 3)/'limits    '/
      data cname( 4)/'covariance'/
      data cname( 5)/'correlatio'/
      data cname( 6)/'print levl'/
      data cname( 7)/'nogradient'/
      data cname( 8)/'gradient  '/
      data cname( 9)/'error def '/
      data cname(10)/'input file'/
      data cname(11)/'width page'/
      data cname(12)/'lines page'/
      data cname(13)/'nowarnings'/
      data cname(14)/'warnings  '/
      data cname(15)/'random gen'/
      data cname(16)/'title     '/
      data cname(17)/'strategy  '/
      data cname(18)/'eigenvalue'/
      data cname(19)/'page throw'/
      data cname(20)/'minos errs'/
      data cname(21)/'epsmachine'/
      data cname(22)/'outputfile'/
      data cname(23)/'batch     '/
      data cname(24)/'interactiv'/
          data nname/24/
c        options not intended for normal users
      data cname(25)/'reserve   '/
      data cname(26)/'reserve   '/
      data cname(27)/'nodebug   '/
      data cname(28)/'debug     '/
      data cname(29)/'show      '/
      data cname(30)/'set       '/
          data nntot/30/
c
      data cprlev(-1)/'-1: no output except from "show"  '/
      data cprlev( 0)/' 0: reduced output                '/
      data cprlev( 1)/' 1: normal output                 '/
      data cprlev( 2)/' 2: extra output for problem cases'/
      data cprlev( 3)/' 3: maximum output                '/
c
      data cstrat( 0)/' 0: minimize the number of calls to function'/
      data cstrat( 1)/' 1: try to balance speed against reliability'/
      data cstrat( 2)/' 2: make sure minimum true, errors correct  '/
c
      data cdbopt(0)/'report all exceptional conditions      '/
      data cdbopt(1)/'mnline: line search minimization       '/
      data cdbopt(2)/'mnderi: first derivative calculations  '/
      data cdbopt(3)/'mnhess: second derivative calculations '/
      data cdbopt(4)/'mnmigr: covariance matrix updates      '/
      data cdbopt(5)/'mnhes1: first derivative uncertainties '/
      data cdbopt(6)/'mncont: mncontour plot (mncros search) '/
c
c
      do 2 i= 1, nntot
      if (index(cword(4:10),cname(i)(1:3)) .gt. 0)  go to 5
    2 continue
      i = 0
    5 kname = i
c
c           command could be set xxx, show xxx,  help set or help show
      if (index(cword(1:4),'hel') .gt. 0)  go to 2000
      if (index(cword(1:4),'sho') .gt. 0)  go to 1000
      if (index(cword(1:4),'set') .eq. 0)  go to 1900
c                           ---
      ckind = 'set '
c                                        . . . . . . . . . . set unknown
      if (kname .le. 0)  go to 1900
c                                        . . . . . . . . . . set known
      go to(3000,  20,  30,  40,3000,  60,  70,  80,  90, 100,
     +       110, 120, 130, 140, 150, 160, 170,3000, 190,3000,
     +       210, 220, 230, 240,1900,1900, 270, 280, 290, 300) , kname
c
c                                        . . . . . . . . . . set param
   20 continue
      iprm = word7(1)
      if (iprm .gt. nu)  go to 25
      if (iprm .le. 0)   go to 25
      if (nvarl(iprm) .lt. 0)  go to 25
      u(iprm) = word7(2)
      call mnexin(x)
      isw2 = isw(2)
      call mnrset(1)
c        keep approximate covariance matrix, even if new param value
      isw(2) = min(isw2,1)
      cfrom = 'set parm'
      nfcnfr = nfcn
      cstatu = 'new values'
      go to 4000
   25 write (isyswr,'(a/)') ' undefined parameter number.  ignored.'
      go to 4000
c                                        . . . . . . . . . . set limits
   30 call mnlims(fcn,futil)
      go to 4000
c                                        . . . . . . . . . . set covar
   40 continue
c   this command must be handled by mnread, and is not fortran-callable
      go to 3000
c                                        . . . . . . . . . . set print
   60 isw(5) = word7(1)
      go to 4000
c                                        . . . . . . . . . . set nograd
   70 isw(3) = 0
      go to 4000
c                                        . . . . . . . . . . set grad
   80 call mngrad(fcn,futil)
      go to 4000
c                                        . . . . . . . . . . set errdef
   90 if (word7(1) .eq. up)  go to 4000
      if (word7(1) .le. zero)  then
         if (up .eq. updflt)  go to 4000
         up = updflt
      else
         up = word7(1)
      endif
      do 95 i= 1, npar
      ern(i) = 0.
   95 erp(i) = 0.
      call mnwerr
      go to 4000
c                                        . . . . . . . . . . set input
c this command must be handled by mnread. if it gets this far,
c         it is illegal.
  100 continue
      go to 3000
c                                        . . . . . . . . . . set width
  110 npagwd = word7(1)
      npagwd = max(npagwd,50)
      go to 4000
c                                        . . . . . . . . . . set lines
  120 npagln = word7(1)
      go to 4000
c                                        . . . . . . . . . . set nowarn
  130 lwarn = .false.
      go to 4000
c                                        . . . . . . . . . . set warn
  140 lwarn = .true.
      call mnwarn('w','sho','sho')
      go to 4000
c                                        . . . . . . . . . . set random
  150 jseed = int(word7(1))
      val = 3.
      call mnrn15(val, jseed)
      if (isw(5) .gt. 0) write (isyswr, 151) jseed
  151 format (' minuit random number seed set to ',i10)
      go to 4000
c                                        . . . . . . . . . . set title
  160 continue
c   this command must be handled by mnread, and is not fortran-callable
      go to 3000
c                                        . . . . . . . . . set strategy
  170 istrat = word7(1)
      istrat = max(istrat,0)
      istrat = min(istrat,2)
      if (isw(5) .gt. 0)  go to 1172
      go to 4000
c                                       . . . . . . . . . set page throw
  190 newpag = word7(1)
      go to 1190
c                                        . . . . . . . . . . set epsmac
  210 if (word7(1).gt.zero .and. word7(1).lt.0.1) epsmac = word7(1)
      epsma2 = dsqrt(epsmac)
      go to 1210
c                                        . . . . . . . . . . set outputfile
  220 continue
      iunit = word7(1)
      isyswr = iunit
      istkwr(1) = iunit
      if (isw(5) .ge. 0) go to 1220
      go to 4000
c                                        . . . . . . . . . . set batch
  230 isw(6) = 0
      if (isw(5) .ge. 0)  go to 1100
      go to 4000
c                                        . . . . . . . . . . set interactive
  240 isw(6) = 1
      if (isw(5) .ge. 0)  go to 1100
      go to 4000
c                                        . . . . . . . . . . set nodebug
  270 iset = 0
      go to 281
c                                        . . . . . . . . . . set debug
  280 iset = 1
  281 continue
      idbopt = word7(1)
      if (idbopt .gt. numdbg) go to 288
      if (idbopt .ge. 0) then
          idbg(idbopt) = iset
          if (iset .eq. 1)  idbg(0) = 1
      else
c             set debug -1  sets all debug options
          do 285 id= 0, numdbg
  285     idbg(id) = iset
      endif
      lrepor = (idbg(0) .ge. 1)
      call mnwarn('d','sho','sho')
      go to 4000
  288 write (isyswr,289) idbopt
  289 format (' unknown debug option',i6,' requested. ignored')
      go to 4000
c                                        . . . . . . . . . . set show
  290 continue
c                                        . . . . . . . . . . set set
  300 continue
      go to 3000
c                -----------------------------------------------------
 1000 continue
c               at this point, cword must be 'show'
      ckind = 'show'
      if (kname .le. 0)  go to 1900
      go to (1010,1020,1030,1040,1050,1060,1070,1070,1090,1100,
     +       1110,1120,1130,1130,1150,1160,1170,1180,1190,1200,
     +       1210,1220,1100,1100,1900,1900,1270,1270,1290,1300),kname
c
c                                        . . . . . . . . . . show fcn
 1010 continue
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      call mnprin (0,amin)
      go to 4000
c                                        . . . . . . . . . . show param
 1020 continue
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      call mnprin (5,amin)
      go to 4000
c                                        . . . . . . . . . . show limits
 1030 continue
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      call mnprin (1,amin)
      go to 4000
c                                        . . . . . . . . . . show covar
 1040 call mnmatu(1)
      go to 4000
c                                        . . . . . . . . . . show corre
 1050 call mnmatu(0)
      go to 4000
c                                        . . . . . . . . . . show print
 1060 continue
      if (isw(5) .lt.-1)  isw(5) = -1
      if (isw(5) .gt. 3)  isw(5) = 3
      write (isyswr,'(a)') ' allowed print levels are:'
      write (isyswr,'(27x,a)') cprlev
      write (isyswr,1061)  cprlev(isw(5))
 1061 format (/' current printout level is ',a)
      go to 4000
c                                        . . . . . . . show nograd, grad
 1070 continue
      if (isw(3) .le. 0) then
         write (isyswr, 1081)
 1081    format(' nograd is set.  derivatives not computed in fcn.')
      else
         write (isyswr, 1082)
 1082    format('   grad is set.  user computes derivatives in fcn.')
      endif
      go to 4000
c                                       . . . . . . . . . . show errdef
 1090 write (isyswr, 1091)  up
 1091 format (' errors correspond to function change of',g13.5)
      go to 4000
c                                       . . . . . . . . . . show input,
c                                                batch, or interactive
 1100 continue
      inquire(unit=isysrd,opened=lopen,named=lname,name=cfname)
      cmode = 'batch mode      '
      if (isw(6) .eq. 1)  cmode = 'interactive mode'
      if (.not. lname)  cfname='unknown'
      write (isyswr,1002) cmode,isysrd,cfname
 1002 format (' input now being read in ',a,' from unit no.',i3/
     + ' filename: ',a)
      go to 4000
c                                       . . . . . . . . . . show width
 1110 write (isyswr,1111) npagwd
 1111 format (10x,'page width is set to',i4,' columns')
      go to 4000
c                                       . . . . . . . . . . show lines
 1120 write (isyswr,1121) npagln
 1121 format (10x,'page length is set to',i4,' lines')
      go to 4000
c                                       . . . . . . .show nowarn, warn
 1130 continue
                 cwarn = 'suppressed'
      if (lwarn) cwarn = 'reported  '
      write (isyswr,1141) cwarn
 1141 format (' minuit warning messages are ',a)
      if (.not. lwarn) call mnwarn('w','sho','sho')
      go to 4000
c                                      . . . . . . . . . . show random
 1150 val = 0.
      call mnrn15(val,igrain)
      ikseed = igrain
      write (isyswr, 1151)  ikseed
 1151 format (' minuit rndm seed is currently=',i10/)
      val = 3.0
      iseed = ikseed
      call mnrn15(val,iseed)
      go to 4000
c                                        . . . . . . . . . show title
 1160 write (isyswr,'(a,a)') ' title of current task is:',ctitl
      go to 4000
c                                        . . . . . . . show strategy
 1170 write (isyswr, '(a)') ' allowed strategies are:'
      write (isyswr, '(20x,a)') cstrat
 1172 write (isyswr, 1175) cstrat(istrat)
 1175 format (/' now using strategy ',a/)
      go to 4000
c                                          . . . . . show eigenvalues
 1180 continue
      iswsav = isw(5)
      isw(5) = 3
      if (isw(2) .lt. 1)  then
         write (isyswr,'(1x,a)') covmes(0)
      else
         call mnpsdf
      endif
      isw(5) = iswsav
      go to 4000
c                                            . . . . . show page throw
 1190 write (isyswr,'(a,i3)') ' page throw carriage control =',newpag
      if (newpag .eq. 0)
     +    write (isyswr,'(a)') ' no page throws in minuit output'
      go to 4000
c                                        . . . . . . show minos errors
 1200 continue
      do 1202 ii= 1, npar
      if (erp(ii).gt.zero .or. ern(ii).lt.zero)  go to 1204
 1202 continue
      write (isyswr,'(a)')
     +   '       there are no minos errors currently valid.'
      go to 4000
 1204 continue
      call mnprin(4,amin)
      go to 4000
c                                        . . . . . . . . . show epsmac
 1210 write (isyswr,'(a,e12.3)')
     +  ' floating-point numbers assumed accurate to',epsmac
      go to 4000
c                                        . . . . . . show outputfiles
 1220 continue
      write (isyswr,'(a,i4)') '  minuit primary output to unit',isyswr
      go to 4000
c                                        . . . . . . show nodebug, debug
 1270 continue
      do 1285 id= 0, numdbg
      copt = 'off'
      if (idbg(id) .ge. 1)  copt = 'on '
 1285 write (isyswr,1286) id, copt, cdbopt(id)
 1286 format (10x,'debug option',i3,' is ',a3,' :',a)
      if (.not. lrepor) call mnwarn('d','sho','sho')
      go to 4000
c                                        . . . . . . . . . . show show
 1290 ckind = 'show'
      go to 2100
c                                        . . . . . . . . . . show set
 1300 ckind = 'set '
      go to 2100

c                -----------------------------------------------------
c                              unknown command
 1900 write (isyswr, 1901) cword
 1901 format (' the command:',a10,' is unknown.'/)
      go to 2100
c                -----------------------------------------------------
c                    help show,  help set,  show set, or show show
 2000 ckind = 'set '
      if (index(cword(4:10),'sho') .gt. 0)  ckind = 'show'
 2100 write (isyswr, 2101)  ckind,ckind, (cname(kk),kk=1,nname)
 2101 format (' the format of the ',a4,' command is:'//
     +   1x,a4,' xxx    [numerical arguments if any]'//
     +   ' where xxx may be one of the following:'/
     +   (7x,6a12))
      go to 4000
c                -----------------------------------------------------
c                               illegal command
 3000 write (isyswr,'('' above command is illegal.   ignored'')')
 4000 return
      end
cdeck  id>, mnseti. 
      subroutine mnseti(tit)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       called by user to set or change title of current task.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character*(*) tit
      ctitl = tit
      return
      end
cdeck  id>, mnsimp. 
      subroutine mnsimp(fcn,futil)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        performs a minimization using the simplex method of nelder
cc        and mead (ref. -- comp. j. 7,308 (1965)).
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      external fcn,futil
      dimension y(mni+1)
      data alpha,beta,gamma,rhomin,rhomax / 1.0, 0.5, 2.0, 4.0, 8.0/
      if (npar .le. 0)  return
      if (amin .eq. undefi)  call mnamin(fcn,futil)
      cfrom = 'simplex '
      nfcnfr = nfcn
      cstatu= 'unchanged '
      npfn=nfcn
      nparp1=npar+1
      nparx = npar
      rho1 = 1.0 + alpha
      rho2 = rho1 + alpha*gamma
      wg = 1.0/float(npar)
      if (isw(5) .ge. 0) write(isyswr,100) epsi
  100 format(' start simplex minimization.    convergence when edm .lt.'
     +,e10.2 )
         do 2 i= 1, npar
         dirin(i) = werr(i)
           call mndxdi(x(i),i,dxdi)
           if (dxdi .ne. zero) dirin(i)=werr(i)/dxdi
         dmin = epsma2*abs(x(i))
         if (dirin(i) .lt. dmin)  dirin(i)=dmin
    2    continue
c**       choose the initial simplex using single-parameter searches
    1 continue
      ynpp1 = amin
      jl = nparp1
      y(nparp1) = amin
      absmin = amin
      do 10 i= 1, npar
      aming = amin
      pbar(i) = x(i)
      bestx = x(i)
      kg = 0
      ns = 0
      nf = 0
    4 x(i) = bestx + dirin(i)
      call mninex(x)
      call fcn(nparx,gin, f, u, 4, futil)
      nfcn = nfcn + 1
      if (f .le. aming)  go to 6
c         failure
      if (kg .eq. 1)  go to 8
      kg = -1
      nf = nf + 1
      dirin(i) = dirin(i) * (-0.4)
      if (nf .lt. 3)  go to 4
      ns = 6
c         success
    6 bestx = x(i)
      dirin(i) = dirin(i) * 3.0
      aming = f
      cstatu= 'progress  '
      kg = 1
      ns = ns + 1
      if (ns .lt. 6)  go to 4
c         local minimum found in ith direction
    8 y(i) = aming
      if (aming .lt. absmin)  jl = i
      if (aming .lt. absmin)  absmin = aming
      x(i) = bestx
      do 9 k= 1, npar
    9 p(k,i) = x(k)
   10 continue
      jh = nparp1
      amin=y(jl)
      call mnrazz(ynpp1,pbar,y,jh,jl)
      do 20 i= 1, npar
   20 x(i) = p(i,jl)
      call mninex(x)
      cstatu = 'progress  '
      if (isw(5) .ge. 1)  call mnprin(5,amin)
      edm = bigedm
      sig2 = edm
      ncycl=0
c                                        . . . . .  start main loop
   50 continue
      if (sig2 .lt. epsi .and. edm.lt.epsi)     go to 76
      sig2 = edm
      if ((nfcn-npfn) .gt. nfcnmx)  go to 78
c         calculate new point * by reflection
      do 60 i= 1, npar
      pb = 0.
      do 59 j= 1, nparp1
   59 pb = pb + wg * p(i,j)
      pbar(i) = pb - wg * p(i,jh)
   60 pstar(i)=(1.+alpha)*pbar(i)-alpha*p(i,jh)
      call mninex(pstar)
      call fcn(nparx,gin,ystar,u,4,futil)
      nfcn=nfcn+1
      if(ystar.ge.amin) go to 70
c         point * better than jl, calculate new point **
      do 61 i=1,npar
   61 pstst(i)=gamma*pstar(i)+(1.-gamma)*pbar(i)
      call mninex(pstst)
      call fcn(nparx,gin,ystst,u,4,futil)
      nfcn=nfcn+1
c         try a parabola through ph, pstar, pstst.  min = prho
      y1 = (ystar-y(jh)) * rho2
      y2 = (ystst-y(jh)) * rho1
      rho = 0.5 * (rho2*y1 -rho1*y2) / (y1 -y2)
      if (rho .lt. rhomin)  go to 66
      if (rho .gt. rhomax)  rho = rhomax
      do 64 i= 1, npar
   64 prho(i) = rho*pbar(i) + (1.0-rho)*p(i,jh)
      call mninex(prho)
      call fcn(nparx,gin,yrho, u,4,futil)
      nfcn = nfcn + 1
      if (yrho .lt. y(jl) .and. yrho .lt. ystst)  go to 65
      if (ystst .lt. y(jl))  go to 67
      if (yrho .gt. y(jl))  go to 66
c         accept minimum point of parabola, prho
   65 call mnrazz (yrho,prho,y,jh,jl)
      go to 68
   66 if (ystst .lt. y(jl))  go to 67
      call mnrazz(ystar,pstar,y,jh,jl)
      go to 68
   67 call mnrazz(ystst,pstst,y,jh,jl)
   68 ncycl=ncycl+1
      if (isw(5) .lt. 2)  go to 50
      if (isw(5) .ge. 3 .or. mod(ncycl, 10) .eq. 0) call mnprin(5,amin)
      go to 50
c         point * is not as good as jl
   70 if (ystar .ge. y(jh))  go to 73
      jhold = jh
      call mnrazz(ystar,pstar,y,jh,jl)
      if (jhold .ne. jh)  go to 50
c         calculate new point **
   73 do 74 i=1,npar
   74 pstst(i)=beta*p(i,jh)+(1.-beta)*pbar(i)
      call mninex (pstst)
      call fcn(nparx,gin,ystst,u,4,futil)
      nfcn=nfcn+1
      if(ystst.gt.y(jh)) go to 1
c     point ** is better than jh
      if (ystst .lt. amin)  go to 67
      call mnrazz(ystst,pstst,y,jh,jl)
      go to 50
c                                        . . . . . .  end main loop
   76 if (isw(5) .ge. 0)  write(isyswr,'(a)')
     +                    ' simplex minimization has converged.'
      isw(4) = 1
      go to 80
   78 if (isw(5) .ge. 0)  write(isyswr,'(a)')
     +                    ' simplex terminates without convergence.'
      cstatu= 'call limit'
      isw(4) = -1
      isw(1) = 1
   80 do 82 i=1,npar
      pb = 0.
      do 81 j=1,nparp1
   81 pb = pb + wg * p(i,j)
   82 pbar(i) = pb - wg * p(i,jh)
      call mninex(pbar)
      call fcn(nparx,gin,ypbar,u,4,futil)
      nfcn=nfcn+1
      if (ypbar .lt. amin)  call mnrazz(ypbar,pbar,y,jh,jl)
      call mninex(x)
      if (nfcnmx+npfn-nfcn .lt. 3*npar)  go to 90
      if (edm .gt. 2.0*epsi)  go to 1
   90 if (isw(5) .ge. 0)  call mnprin(5, amin)
      return
      end
cdeck  id>, mnstat. 
      subroutine mnstat(fmin,fedm,errdef,npari,nparx,istat)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc       user-called
cc       provides the user with information concerning the current status
cc          of the current minimization. namely, it returns:
cc        fmin: the best function value found so far
cc        fedm: the estimated vertical distance remaining to minimum
cc        errdef: the value of up defining parameter uncertainties
cc        npari: the number of currently variable parameters
cc        nparx: the highest (external) parameter number defined by user
cc        istat: a status integer indicating how good is the covariance
cc           matrix:  0= not calculated at all
cc                    1= approximation only, not accurate
cc                    2= full matrix, but forced positive-definite
cc                    3= full accurate covariance matrix
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      fmin = amin
      fedm = edm
      errdef = up
      npari = npar
      nparx = nu
      istat = isw(2)
        if (edm  .eq. bigedm)  then
            fedm = up
        endif
        if (amin .eq. undefi)  then
            fmin = 0.0
            fedm = up
            istat= 0
        endif
      return
      end
cdeck  id>, mnstin. 
      subroutine mnstin(crdbuf,ierr)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc called from mnread.
cc implements the set input command to change input units.
cc if command is: 'set input'   'set input 0'   or  '*eof',
cc                 or 'set input , ,  ',
cc                reverts to previous input unit number,if any.
cc
cc      if it is: 'set input n'  or  'set input n filename',
cc                changes to new input file, added to stack
cc
cc      ierr = 0: reading terminated normally
cc             2: end-of-data on primary input file
cc             3: unrecoverable read error
cc             4: unable to process request
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character crdbuf*(*),cunit*10,cfname*64,cgname*64,canswr*1
      character cmode*16
      logical lopen,lrewin,noname,lname,mnunpt
      noname = .true.
      ierr = 0
      if (index(crdbuf,'*eof') .eq. 1) go to 190
      if (index(crdbuf,'*eof') .eq. 1) go to 190
      lend = len(crdbuf)
c                               look for end of set input command
        do 20 ic= 8,lend
        if (crdbuf(ic:ic) .eq. ' ') go to 25
        if (crdbuf(ic:ic) .eq. ',') go to 53
   20   continue
      go to 200
   25 continue
c         look for end of separator between command and first argument
      icol = ic+1
         do 50 ic= icol,lend
         if (crdbuf(ic:ic) .eq. ' ') go to 50
         if (crdbuf(ic:ic) .eq. ',') go to 53
         go to 55
   50 continue
      go to 200
   53 ic = ic + 1
   55 ic1 = ic
c                      see if "rewind" was requested in command
      lrewin = .false.
      if (index(crdbuf(1:ic1),'rew') .gt. 5)  lrewin=.true.
      if (index(crdbuf(1:ic1),'rew') .gt. 5)  lrewin=.true.
c                      first argument begins in or after col ic1
      do 75 ic= ic1,lend
      if (crdbuf(ic:ic) .eq. ' ') go to 75
      if (crdbuf(ic:ic) .eq. ',') go to 200
      go to 80
   75 continue
      go to 200
   80 ic1 = ic
c                        first argument really begins in col ic1
      do 100 ic= ic1+1,lend
      if (crdbuf(ic:ic) .eq. ' ') go to 108
      if (crdbuf(ic:ic) .eq. ',') go to 108
  100 continue
      ic = lend + 1
  108 ic2 = ic-1
c                            end of first argument is in col ic2
  110 continue
      cunit = crdbuf(ic1:ic2)
      write (isyswr,'(a,a)') ' unit no. :',cunit
      read (cunit,'(bn,f10.0)',err=500) funit
      iunit = funit
      if (iunit .eq. 0)  go to 200
c                             skip blanks and commas, find file name
      do 120 ic= ic2+1,lend
      if (crdbuf(ic:ic) .eq. ' ') go to 120
      if (crdbuf(ic:ic) .eq. ',') go to 120
      go to 130
  120 continue
      go to 131
  130 continue
      cfname = crdbuf(ic:lend)
      noname = .false.
      write (isyswr, '(a,a)') ' file name is:',cfname
c              ask if file exists, if not ask for name and open it
  131 continue
      inquire(unit=iunit,opened=lopen,named=lname,name=cgname)
      if (lopen) then
         if (noname) then
             go to 136
         else
             if (.not.lname) cgname='unknown'
             write (isyswr,132) iunit,cgname,cfname
  132        format (' unit',i3,' already opened with name:',a/
     +                  '                 new name ignored:',a)
         endif
      else
c                new file, open it
         write (isyswr,135) iunit
  135    format (' unit',i3,' is not opened.')
         if (noname) then
            write (isyswr,'(a)') ' no file name given in command.'
            if (isw(6) .ne. 1)  go to 800
            write (isyswr,'(a)') ' please give file name:'
            read (isysrd,'(a)') cfname
         endif
         open (unit=iunit,file=cfname,status='old',err=600)
         write (isyswr,'(a)') ' file opened successfully.'
      endif
c                                     . .   file is correctly opened
  136 if (lrewin) go to 150
      if (isw(6) .ne. 1)  go to 300
      write (isyswr,137)  iunit
  137 format (' should unit',i3,' be rewound?' )
      read  (isysrd,'(a)')  canswr
      if (canswr.ne.'y' .and. canswr.ne.'y') go to 300
  150 rewind iunit
      go to 300
c                      *eof
  190 continue
      if (nstkrd .eq. 0)  then
         ierr = 2
         go to 900
         endif
c                      revert to previous input file
  200 continue
      if (nstkrd .eq. 0)  then
          write (isyswr, '(a,a)') ' command ignored:',crdbuf
          write (isyswr, '(a)') ' already reading from primary input'
      else
        isysrd = istkrd(nstkrd)
        nstkrd = nstkrd - 1
        if (nstkrd .eq. 0)  isw(6) = iabs(isw(6))
        if (isw(5) .ge. 0)  then
          inquire(unit=isysrd,named=lname,name=cfname)
          cmode = 'batch mode      '
          if (isw(6) .eq. 1)  cmode = 'interactive mode'
          if (.not.lname) cfname='unknown'
          if (mnunpt(cfname))  cfname='unprintable'
          write (isyswr,290) cmode,isysrd,cfname
  290     format (' input will now be read in ',a,' from unit no.',i3/
     +    ' filename: ',a)
        endif
      endif
      go to 900
c                      switch to new input file, add to stack
  300 continue
      if (nstkrd .ge. maxstk)  then
          write (isyswr, '(a)') ' input file stack size exceeded.'
          go to 800
          endif
      nstkrd = nstkrd + 1
      istkrd(nstkrd) = isysrd
      isysrd = iunit
c                   isw(6) = 0 for batch, =1 for interactive, and
c                      =-1 for originally interactive temporarily batch
      if (isw(6) .eq. 1)  isw(6) = -1
      go to 900
c                      format error
  500 continue
      write (isyswr,'(a,a)') ' cannot read following as integer:',cunit
      go to 800
  600 continue
      write (isyswr, 601) cfname
  601 format (' system is unable to open file:',a)
c                      serious error
  800 continue
      ierr = 3
  900 continue
      return
      end
cdeck  id>, mntiny. 
      subroutine mntiny(epsp1,epsbak)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        compares its argument with the value 1.0, and returns
cc        the value .true. if they are equal.  to find epsmac
cc        safely by foiling the fortran optimizer
cc
      parameter (one=1.0)
      epsbak =  epsp1  - one
      return
      end
cdeck  id>, mnunpt. 
      logical function mnunpt(cfname)
c           is .true. if cfname contains unprintable characters.
      character cfname*(*)
      character cpt*80, cp1*40,cp2*40
      parameter (cp1=' abcdefghijklmnopqrstuvwxyzabcdefghijklm')
      parameter (cp2='nopqrstuvwxyz1234567890./;:[]$%*_!@#&+()')
      cpt=cp1//cp2
      mnunpt = .false.
      l = len(cfname)
      do 100 i= 1, l
         do 50 ic= 1, 80
         if (cfname(i:i) .eq. cpt(ic:ic))  go to 100
   50    continue
      mnunpt = .true.
      go to 150
  100 continue
  150 continue
      return
      end
cdeck  id>, mnvert. 
      subroutine mnvert(a,l,m,n,ifail)
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        inverts a symmetric matrix.   matrix is first scaled to
cc        have all ones on the diagonal (equivalent to change of units)
cc        but no pivoting is done since matrix is positive-definite.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      dimension a(l,m) ,pp(mni), q(mni),  s(mni)
      ifail=0
      if (n .lt. 1)  go to 100
      if (n .gt. maxint)  go to 100
c                   scale matrix by dsqrt of diag elements
      do 8  i=1,n
      si = a(i,i)
      if (si) 100,100,8
    8 s(i) = 1.0/dsqrt(si)
      do 20 i= 1, n
      do 20 j= 1, n
   20 a(i,j) = a(i,j) *s(i)*s(j)
c                                        . . . start main loop . . . .
      do 65 i=1,n
      k = i
c                   preparation for elimination step1
      q(k)=1./a(k,k)
      pp(k) = 1.0
      a(k,k)=0.0
      kp1=k+1
      km1=k-1
      if(km1)100,50,40
   40 do 49 j=1,km1
      pp(j)=a(j,k)
      q(j)=a(j,k)*q(k)
   49 a(j,k)=0.
   50 if(k-n)51,60,100
   51 do 59 j=kp1,n
      pp(j)=a(k,j)
      q(j)=-a(k,j)*q(k)
   59 a(k,j)=0.0
c                   elimination proper
   60 do 65 j=1,n
      do 65 k=j,n
   65 a(j,k)=a(j,k)+pp(j)*q(k)
c                   elements of left diagonal and unscaling
      do 70 j= 1, n
      do 70 k= 1, j
      a(k,j) = a(k,j) *s(k)*s(j)
   70 a(j,k) = a(k,j)
      return
c                   failure return
  100 ifail=1
      return
      end
cdeck  id>, mnwarn. 
      subroutine mnwarn(copt,corg,cmes)
c     if copt='w', cmes is a warning message from corg.
c     if copt='d', cmes is a debug message from corg.
c         if set warnings is in effect (the default), this routine
c             prints the warning message cmes coming from corg.
c         if set nowarnings is in effect, the warning message is
c             stored in a circular buffer of length maxmes.
c         if called with corg=cmes='sho', it prints the messages in
c             the circular buffer, fifo, and empties the buffer.
c ************ double precision version *************
      implicit double precision (a-h,o-z)
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
      character copt*1, corg*(*), cmes*(*), ctyp*7
      parameter (maxmes=10)
      character     origin(maxmes,2)*10, warmes(maxmes,2)*60
      common/mn7wrc/origin,              warmes
      common/mn7wri/nfcwar(maxmes,2),icirc(2)
      character englsh*20
c
      if (corg(1:3).eq.'sho' .and. cmes(1:3).eq.'sho')  go to 200
c             either print warning or put in buffer
      if (copt .eq. 'w')  then
        ityp = 1
        if (lwarn) then
          write (isyswr,'(a,a/a,a)') ' minuit warning in ',corg,
     +              ' ============== ',cmes
          return
        endif
      else
        ityp = 2
        if (lrepor) then
          write (isyswr,'(a,a/a,a)') ' minuit debug for  ',corg,
     +              ' ============== ',cmes
          return
        endif
      endif
c                 if appropriate flag is off, fill circular buffer
         if (nwrmes(ityp) .eq. 0)  icirc(ityp) = 0
         nwrmes(ityp) = nwrmes(ityp) + 1
         icirc(ityp) = icirc(ityp) + 1
         if (icirc(ityp) .gt. maxmes) icirc(ityp) = 1
         ic = icirc(ityp)
         origin(ic,ityp) = corg
         warmes(ic,ityp) = cmes
         nfcwar(ic,ityp) = nfcn
      return
c
c             'sho warnings', ask if any suppressed mess in buffer
  200 continue
      if (copt .eq. 'w') then
        ityp = 1
        ctyp = 'warning'
      else
        ityp = 2
        ctyp = '*debug*'
      endif
      if (nwrmes(ityp) .gt. 0) then
         englsh = ' was suppressed.  '
         if (nwrmes(ityp) .gt. 1) englsh = 's were suppressed.'
         write (isyswr,'(/1x,i5,a,a,a,a/)') nwrmes(ityp),
     +    ' minuit ',ctyp,' message', englsh
         nm = nwrmes(ityp)
         ic = 0
         if (nm .gt. maxmes) then
              write (isyswr,'(a,i2,a)')  ' only the most recent ',
     +          maxmes,' will be listed below.'
              nm = maxmes
              ic = icirc(ityp)
         endif
         write (isyswr,'(a)') '  calls  origin         message'
           do 300 i= 1, nm
           ic = ic + 1
           if (ic .gt. maxmes)  ic = 1
           write (isyswr,'(1x,i6,1x,a,1x,a)')
     +           nfcwar(ic,ityp),origin(ic,ityp),warmes(ic,ityp)
 300       continue
         nwrmes(ityp) = 0
         write (isyswr,'(1h )')
      endif
      return
      end
cdeck  id>, mnwerr. 
      subroutine mnwerr
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc          calculates the werr, external parameter errors,
cc      and the global correlation coefficients, to be called
cc      whenever a new covariance matrix is available.
cc
      parameter (mne=100 , mni=50)
      parameter (mnihl=mni*(mni+1)/2)
      character*10 cpnam
      common
     1/mn7nam/ cpnam(mne)
     2/mn7ext/ u(mne)     ,alim(mne)  ,blim(mne)
     3/mn7err/ erp(mni)   ,ern(mni)   ,werr(mni)  ,globcc(mni)
     4/mn7inx/ nvarl(mne) ,niofex(mne),nexofi(mni)
     5/mn7int/ x(mni)     ,xt(mni)    ,dirin(mni)
     6/mn7fx2/ xs(mni)    ,xts(mni)   ,dirins(mni)
     7/mn7der/ grd(mni)   ,g2(mni)    ,gstep(mni) ,gin(mne) ,dgrd(mni)
     8/mn7fx3/ grds(mni)  ,g2s(mni)   ,gsteps(mni)
     9/mn7fx1/ ipfix(mni) ,npfix
     a/mn7var/ vhmat(mnihl)
     b/mn7vat/ vthmat(mnihl)
     c/mn7sim/ p(mni,mni+1),pstar(mni),pstst(mni) ,pbar(mni),prho(mni)
c
      parameter (maxdbg=10, maxstk=10, maxcwd=20, maxp=30, maxcpt=101)
      parameter (zero=0.0,  one=1.0,   half=0.5)
      common
     d/mn7npr/ maxint ,npar   ,maxext ,nu
     e/mn7iou/ isysrd ,isyswr ,isyssa ,npagwd ,npagln ,newpag
     e/mn7io2/ istkrd(maxstk) ,nstkrd ,istkwr(maxstk) ,nstkwr
     f/mn7tit/ cfrom  ,cstatu ,ctitl  ,cword  ,cundef ,cvrsn ,covmes
     g/mn7flg/ isw(7) ,idbg(0:maxdbg) ,nblock ,icomnd
     h/mn7min/ amin   ,up     ,edm    ,fval3  ,epsi   ,apsi  ,dcovar
     i/mn7cnv/ nfcn   ,nfcnmx ,nfcnlc ,nfcnfr ,itaur,istrat,nwrmes(2)
     j/mn7arg/ word7(maxp)
     k/mn7log/ lwarn  ,lrepor ,limset ,lnolim ,lnewmn ,lphead
     l/mn7cns/ epsmac ,epsma2 ,vlimlo ,vlimhi ,undefi ,bigedm,updflt
     m/mn7rpt/ xpt(maxcpt)    ,ypt(maxcpt)
     n/mn7cpt/ chpt(maxcpt)
     o/mn7xcr/ xmidcr ,ymidcr ,xdircr ,ydircr ,ke1cr  ,ke2cr
      character ctitl*50, cword*(maxcwd), cundef*10, cfrom*8,
     +          cvrsn*6,  covmes(0:3)*22, cstatu*10, chpt*1
      logical   lwarn, lrepor, limset, lnolim, lnewmn, lphead
c                         calculate external error if v exists
      if (isw(2) .ge. 1) then
      do 100 l= 1, npar
        ndex = l*(l+1)/2
        dx = dsqrt(abs(vhmat(ndex)*up))
        i = nexofi(l)
        if (nvarl(i) .gt. 1)  then
          al = alim(i)
          ba = blim(i) - al
          du1 = al + 0.5 *(dsin(x(l)+dx) +1.0) * ba - u(i)
          du2 = al + 0.5 *(dsin(x(l)-dx) +1.0) * ba - u(i)
          if (dx .gt. 1.0)  du1 = ba
          dx = 0.5 * (abs(du1) + abs(du2))
        endif
        werr(l) = dx
  100 continue
      endif
c                          global correlation coefficients
      if (isw(2) .ge. 1) then
         do 130 i= 1, npar
            globcc(i) = 0.
            k1 = i*(i-1)/2
            do 130 j= 1, i
               k = k1 + j
               p(i,j) = vhmat(k)
  130          p(j,i) = p(i,j)
         call mnvert(p,maxint,maxint,npar,ierr)
         if (ierr .eq. 0)   then
            do 150 iin= 1, npar
               ndiag = iin*(iin+1)/2
               denom = p(iin,iin)*vhmat(ndiag)
               if (denom.le.one .and. denom.ge.zero)  then
                   globcc(iin) = 0.
               else
                   globcc(iin) = dsqrt(1.0-1.0/denom)
               endif
  150       continue
         endif
      endif
      return
      end
cdeck  id>, stand.  
      subroutine stand
c ************ double precision version *************
      implicit double precision (a-h,o-z)
cc        optional user-supplied subroutine is called whenever the
cc        command "standard" appears.
cc
      return
      end
