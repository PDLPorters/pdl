*DECK CHFIE
      REAL FUNCTION CHFIE (X1, X2, F1, F2, D1, D2, A, B)
C***BEGIN PROLOGUE  CHFIE
C***SUBSIDIARY
C***PURPOSE  Evaluates integral of a single cubic for PCHIA
C***LIBRARY   SLATEC (PCHIP)
C***TYPE      SINGLE PRECISION (CHFIE-S, DCHFIE-D)
C***AUTHOR  Fritsch, F. N., (LLNL)
C***DESCRIPTION
C
C          CHFIE:  Cubic Hermite Function Integral Evaluator.
C
C     Called by  PCHIA  to evaluate the integral of a single cubic (in
C     Hermite form) over an arbitrary interval (A,B).
C
C ----------------------------------------------------------------------
C
C  Calling sequence:
C
C        REAL  X1, X2, F1, F2, D1, D2, A, B
C        REAL  VALUE, CHFIE
C
C        VALUE = CHFIE (X1, X2, F1, F2, D1, D2, A, B)
C
C   Parameters:
C
C     VALUE -- (output) value of the requested integral.
C
C     X1,X2 -- (input) endpoints if interval of definition of cubic.
C
C     F1,F2 -- (input) function values at the ends of the interval.
C
C     D1,D2 -- (input) derivative values at the ends of the interval.
C
C     A,B -- (input) endpoints of interval of integration.
C
C***SEE ALSO  PCHIA
C***ROUTINES CALLED  (NONE)
C***REVISION HISTORY  (YYMMDD)
C   820730  DATE WRITTEN
C   820805  Converted to SLATEC library version.
C   870813  Minor cosmetic changes.
C   890411  1. Added SAVE statements (Vers. 3.2).
C           2. Added SIX to REAL declaration.
C   890411  REVISION DATE from Version 3.2
C   891214  Prologue converted to Version 4.0 format.  (BAB)
C   900315  CALLs to XERROR changed to CALLs to XERMSG.  (THJ)
C   900328  Added TYPE section.  (WRB)
C   910408  Updated AUTHOR section in prologue.  (WRB)
C   930503  Corrected to set VALUE=0 when IERR.ne.0.  (FNF)
C   930504  Eliminated IERR and changed name from CHFIV to CHFIE.  (FNF)
C***END PROLOGUE  CHFIE
C
C  Programming notes:
C  1. There is no error return from this routine because zero is
C     indeed the mathematically correct answer when X1.EQ.X2 .
C**End
C
C  DECLARE ARGUMENTS.
C
      implicit integer*8(i-n)
      REAL  X1, X2, F1, F2, D1, D2, A, B
C
C  DECLARE LOCAL VARIABLES.
C
      REAL  DTERM, FOUR, FTERM, H, HALF, PHIA1, PHIA2, PHIB1, PHIB2,
     *      PSIA1, PSIA2, PSIB1, PSIB2, SIX, TA1, TA2, TB1, TB2, THREE,
     *      TWO, UA1, UA2, UB1, UB2
      SAVE HALF, TWO, THREE, FOUR, SIX
C
C  INITIALIZE.
C
      DATA  HALF /0.5/,  TWO /2./,  THREE /3./,  FOUR /4./,  SIX /6./
C
C  VALIDITY CHECK INPUT.
C
C***FIRST EXECUTABLE STATEMENT  CHFIE
      IF (X1 .EQ. X2)  THEN
         CHFIE = 0
      ELSE
         H = X2 - X1
         TA1 = (A - X1) / H
         TA2 = (X2 - A) / H
         TB1 = (B - X1) / H
         TB2 = (X2 - B) / H
C
         UA1 = TA1**3
         PHIA1 = UA1 * (TWO - TA1)
         PSIA1 = UA1 * (THREE*TA1 - FOUR)
         UA2 = TA2**3
         PHIA2 =  UA2 * (TWO - TA2)
         PSIA2 = -UA2 * (THREE*TA2 - FOUR)
C
         UB1 = TB1**3
         PHIB1 = UB1 * (TWO - TB1)
         PSIB1 = UB1 * (THREE*TB1 - FOUR)
         UB2 = TB2**3
         PHIB2 =  UB2 * (TWO - TB2)
         PSIB2 = -UB2 * (THREE*TB2 - FOUR)
C
         FTERM =   F1*(PHIA2 - PHIB2) + F2*(PHIB1 - PHIA1)
         DTERM = ( D1*(PSIA2 - PSIB2) + D2*(PSIB1 - PSIA1) )*(H/SIX)
C
         CHFIE = (HALF*H) * (FTERM + DTERM)
      ENDIF
C
      RETURN
C------------- LAST LINE OF CHFIE FOLLOWS ------------------------------
      END
