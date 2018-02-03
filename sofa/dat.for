      SUBROUTINE iau_DAT ( IY, IM, ID, FD, DELTAT, J )
*+
*  - - - - - - - -
*   i a u _ D A T
*  - - - - - - - -
*
*  For a given UTC date, calculate delta(AT) = TAI-UTC.
*
*     :------------------------------------------:
*     :                                          :
*     :                 IMPORTANT                :
*     :                                          :
*     :  A new version of this routine must be   :
*     :  produced whenever a new leap second is  :
*     :  announced.  There are two items to      :
*     :  change on each such occasion:           :
*     :                                          :
*     :  1) The parameter NDAT must be           :
*     :     incremented by 1.                    :
*     :                                          :
*     :  2) A new line must be added to the set  :
*     :     of DATA statements that initialize   :
*     :     the arrays IDATE and DATS.           :
*     :                                          :
*     :  Latest leap second:  1999 January 1     :
*     :                                          :
*     :__________________________________________:
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     IY       i     UTC:  year (Notes 1 and 2)
*     IMO      i           month (Note 2)
*     ID       i           day (Notes 2 and 3)
*     FD       d           fraction of day (Note 4)
*
*  Returned:
*     DELTAT   d     TAI minus UTC, seconds
*     J        i     status (Note 5):
*                       1 = dubious year (Note 1: DAT computed)
*                       0 = OK
*                      -1 = bad year (DAT not computed)
*                      -2 = bad month (DAT not computed)
*                      -3 = bad day (Note 3: DAT not computed)
*                      -4 = bad fraction (Note 4: DAT computed)
*
*  Notes:
*
*  1) UTC began at 1960 January 1.0 (JD 2436934.5) and it is improper
*     to call the routine with an earlier epoch.  However, if this
*     is attempted, the TAI-UTC expression for the year 1960 is used.
*
*     Because leap seconds cannot, in principle, be predicted in
*     advance, a reliable check for dates beyond the valid range is
*     impossible.  To guard against gross errors, a year more than
*     three after that of the final leap second is considered dubious.
*
*     For both too-early and too-late years, a warning status of J=1
*     is returned.  This is distinct from the error status J=-1, which
*     signifies a year so early that JD could not be computed.
*
*  2) If the specified date is for a day which ends with a leap second,
*     the UTC-TAI value returned is for the period leading up to the
*     leap second.  If the date is for a day which begins as a leap
*     second ends, the UTC-TAI returned is for the period following the
*     leap second.
*
*  3) The day number must be in the normal calendar range, for example
*     1 through 30 for April.  The "almanac" convention of allowing
*     such dates as January 0 and December 32 is not supported in this
*     routine, in order to avoid confusion near leap seconds.
*
*  4) The fraction of day is used only for dates before the introduction
*     of leap seconds, the first of which occurred at the end of 1971.
*     It is tested for validity (zero to less than 1 is the valid range)
*     even if not used;  if invalid, zero is used and status J=4 is
*     returned.  For many applications, setting FD to zero is
*     acceptable;  the resulting error is always less than 3 ms (and
*     only pre-1972).
*
*  5) The status value returned in the case where there are multiple
*     errors refers to the first error detected.  For example, if the
*     month and day are 13 and 32 respectively, JSTAT=-2 (bad month)
*     will be returned.
*
*  References:
*
*  1) For epochs from 1961 January 1 onwards, the expressions from the
*     file ftp://maia.usno.navy.mil/ser7/tai-utc.dat are used.
*
*  2) The 5ms timestep at 1961 January 1 is taken from 2.58.1 (p87) of
*     the 1992 Explanatory Supplement.
*
*  Called:
*     iau_CAL2JD  Gregorian calendar to Julian Day Number
*
*  This revision:  2001 February 27
*
*  Copyright (C) 2001 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      INTEGER IY, IM, ID
      DOUBLE PRECISION FD, DELTAT
      INTEGER J

*  Number of Delta(AT) changes (increase by 1 for each new leap second).
      INTEGER NDAT
      PARAMETER ( NDAT = 37 )

*  Number of Delta(AT) expressions before leap seconds were introduced.
      INTEGER NERA1
      PARAMETER ( NERA1 = 14 )

*  Dates (year, month) on which new Delta(AT) came into force.
      INTEGER IDATE(2,NDAT)

*  New Delta(AT) which came into force on the given dates.
      DOUBLE PRECISION DATS(NDAT)

*  Reference dates (MJD) and drift rates (s/day), pre leap seconds.
      DOUBLE PRECISION DRIFT(2,NERA1)

*  Miscellaneous local variables.
      LOGICAL MORE
      INTEGER I, M
      DOUBLE PRECISION W, DJM

*  Dates and Delta(AT)s.
      DATA (IDATE(I, 1),I=1,2),DATS(1)  / 1960,  1,  1.4178180D0 /
      DATA (IDATE(I, 2),I=1,2),DATS(2)  / 1961,  1,  1.4228180D0 /
      DATA (IDATE(I, 3),I=1,2),DATS(3)  / 1961,  8,  1.3728180D0 /
      DATA (IDATE(I, 4),I=1,2),DATS(4)  / 1962,  1,  1.8458580D0 /
      DATA (IDATE(I, 5),I=1,2),DATS(5)  / 1963, 11,  1.9458580D0 /
      DATA (IDATE(I, 6),I=1,2),DATS(6)  / 1964,  1,  3.2401300D0 /
      DATA (IDATE(I, 7),I=1,2),DATS(7)  / 1964,  4,  3.3401300D0 /
      DATA (IDATE(I, 8),I=1,2),DATS(8)  / 1964,  9,  3.4401300D0 /
      DATA (IDATE(I, 9),I=1,2),DATS(9)  / 1965,  1,  3.5401300D0 /
      DATA (IDATE(I,10),I=1,2),DATS(10) / 1965,  3,  3.6401300D0 /
      DATA (IDATE(I,11),I=1,2),DATS(11) / 1965,  7,  3.7401300D0 /
      DATA (IDATE(I,12),I=1,2),DATS(12) / 1965,  9,  3.8401300D0 /
      DATA (IDATE(I,13),I=1,2),DATS(13) / 1966,  1,  4.3131700D0 /
      DATA (IDATE(I,14),I=1,2),DATS(14) / 1968,  2,  4.2131700D0 /
      DATA (IDATE(I,15),I=1,2),DATS(15) / 1972,  1, 10D0 /
      DATA (IDATE(I,16),I=1,2),DATS(16) / 1972,  7, 11D0 /
      DATA (IDATE(I,17),I=1,2),DATS(17) / 1973,  1, 12D0 /
      DATA (IDATE(I,18),I=1,2),DATS(18) / 1974,  1, 13D0 /
      DATA (IDATE(I,19),I=1,2),DATS(19) / 1975,  1, 14D0 /
      DATA (IDATE(I,20),I=1,2),DATS(20) / 1976,  1, 15D0 /
      DATA (IDATE(I,21),I=1,2),DATS(21) / 1977,  1, 16D0 /
      DATA (IDATE(I,22),I=1,2),DATS(22) / 1978,  1, 17D0 /
      DATA (IDATE(I,23),I=1,2),DATS(23) / 1979,  1, 18D0 /
      DATA (IDATE(I,24),I=1,2),DATS(24) / 1980,  1, 19D0 /
      DATA (IDATE(I,25),I=1,2),DATS(25) / 1981,  7, 20D0 /
      DATA (IDATE(I,26),I=1,2),DATS(26) / 1982,  7, 21D0 /
      DATA (IDATE(I,27),I=1,2),DATS(27) / 1983,  7, 22D0 /
      DATA (IDATE(I,28),I=1,2),DATS(28) / 1985,  7, 23D0 /
      DATA (IDATE(I,29),I=1,2),DATS(29) / 1988,  1, 24D0 /
      DATA (IDATE(I,30),I=1,2),DATS(30) / 1990,  1, 25D0 /
      DATA (IDATE(I,31),I=1,2),DATS(31) / 1991,  1, 26D0 /
      DATA (IDATE(I,32),I=1,2),DATS(32) / 1992,  7, 27D0 /
      DATA (IDATE(I,33),I=1,2),DATS(33) / 1993,  7, 28D0 /
      DATA (IDATE(I,34),I=1,2),DATS(34) / 1994,  7, 29D0 /
      DATA (IDATE(I,35),I=1,2),DATS(35) / 1996,  1, 30D0 /
      DATA (IDATE(I,36),I=1,2),DATS(36) / 1997,  7, 31D0 /
      DATA (IDATE(I,37),I=1,2),DATS(37) / 1999,  1, 32D0 /

*  Reference dates and drift rates.
      DATA (DRIFT(I, 1),I=1,2) / 37300D0, 0.001296D0 /
      DATA (DRIFT(I, 2),I=1,2) / 37300D0, 0.001296D0 /
      DATA (DRIFT(I, 3),I=1,2) / 37300D0, 0.001296D0 /
      DATA (DRIFT(I, 4),I=1,2) / 37665D0, 0.0011232D0 /
      DATA (DRIFT(I, 5),I=1,2) / 37665D0, 0.0011232D0 /
      DATA (DRIFT(I, 6),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I, 7),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I, 8),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I, 9),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,10),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,11),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,12),I=1,2) / 38761D0, 0.001296D0 /
      DATA (DRIFT(I,13),I=1,2) / 39126D0, 0.002592D0 /
      DATA (DRIFT(I,14),I=1,2) / 39126D0, 0.002592D0 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  If invalid fraction, give up.
      IF ( FD.LT.0D0 .OR. FD.GE.1D0 ) GO TO 9999

*  Convert the date into an MJD.
      CALL iau_CAL2JD ( IY, IM, ID, W, DJM, J )

*  If invalid year, month, or day, give up.
      IF ( J .EQ. -1 ) J = 1
      IF ( J .NE. 0 ) GO TO 9999

*  Check for dubious year and set status accordingly.
      IF ( IY.LT.IDATE(1,1) .OR. IY-IDATE(1,NDAT).GT.3 ) THEN
         J = 1
      ELSE
         J = 0
      END IF

*  Combine year and month.
      M = 12*IY+IM

*  Prepare to search the tables.
      MORE = .TRUE.

*  Look at each table entry, starting with the most recent.
      DO 1 I=NDAT,1,-1

*     Proceed only if necessary.
         IF ( MORE ) THEN

*        Copy the Delta(AT).
            W = DATS(I)

*        If pre-1972, adjust for drift.
            IF ( I .LE. NERA1 )
     :         W = W + ( DJM + FD - DRIFT(1,I) ) * DRIFT(2,I)

*        Have we finished yet?
            MORE = M .LT. ( 12*IDATE(1,I) + IDATE(2,I) )

         END IF
 1    CONTINUE

*  Return the Delta(AT) value.
      DELTAT = W

*  Finished.
 9999 CONTINUE

*+----------------------------------------------------------------------
*
*  Copyright (C) 2001
*  Standards Of Fundamental Astronomy Review Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING TERMS AND CONDITIONS
*  WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Review Board ("the Board").
*
*  2. The Software is made available free of charge for use by:
*
*     a) private individuals for non-profit research; and
*
*     b) non-profit educational, academic and research institutions.
*
*  3. Commercial use of the Software is specifically excluded from the
*     terms and conditions of this license.  Commercial use of the
*     Software is subject to the prior written agreement of the Board on
*     terms to be agreed.
*
*  4. The provision of any version of the Software under the terms and
*     conditions specified herein does not imply that future versions
*     will also be made available under the same terms and conditions.
*
*  5. The user may modify the Software for his/her own purposes.  The
*     user may distribute the modified software provided that the Board
*     is informed and that a copy of the modified software is made
*     available to the Board on request.  All modifications made by the
*     user shall be clearly identified to show how the modified software
*     differs from the original Software, and the name(s) of the
*     affected routine(s) shall be changed.  The original SOFA Software
*     License text must be present.
*
*  6. In any published work produced by the user and which includes
*     results achieved by using the Software, the user shall acknowledge
*     that the Software was used in producing the information contained
*     in such publication.
*
*  7. The user may incorporate or embed the Software into other software
*     products which he/she may then give away free of charge but not
*     sell provided the user makes due acknowledgement of the use which
*     he/she has made of the Software in creating such software
*     products.  Any redistribution of the Software in this way shall be
*     made under the same terms and conditions under which the user
*     received it from the SOFA Center.
*
*  8. The user shall not cause the Software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  9. The Software is provided to the user "as is" and the Board makes
*     no warranty as to its use or performance.   The Board does not and
*     cannot warrant the performance or results which the user may
*     obtain by using the Software.  The Board makes no warranties,
*     express or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*     Internet email: sofa@rl.ac.uk
*     Postal address: IAU SOFA Center
*                     Rutherford Appleton Laboratory
*                     Chilton, Didcot, Oxon OX11 0QX
*                     United Kingdom
*
*
*-----------------------------------------------------------------------

      END
