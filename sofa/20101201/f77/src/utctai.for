      SUBROUTINE iau_UTCTAI ( UTC1, UTC2, TAI1, TAI2, J )
*+
*  - - - - - - - - - - -
*   i a u _ U T C T A I
*  - - - - - - - - - - -
*
*  Time scale transformation:  Coordinated Universal Time, UTC, to
*  International Atomic Time, TAI.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical.
*
*  Given:
*     UTC1,UTC2    d      UTC as a 2-part quasi Julian Date (Notes 1-4)
*
*  Returned:
*     TAI1,TAI2    d      TAI as a 2-part Julian Date (Note 5)
*     J            i      status: +1 = dubious year (Note 3)
*                                  0 = OK
*                                 -1 = unacceptable date
*
*  Notes:
*
*  1) UTC1+UTC2 is quasi Julian Date (see Note 2), apportioned in any
*     convenient way between the two arguments, for example where UTC1
*     is the Julian Day Number and UTC2 is the fraction of a day.
*
*  2) JD cannot unambiguously represent UTC during a leap second unless
*     special measures are taken.  The convention in the present routine
*     is that the JD day represents UTC days whether the length is
*     86399, 86400 or 86401 SI seconds.
*
*  3) The warning status "dubious year" flags UTCs that predate the
*     introduction of the time scale and that are too far in the future
*     to be trusted.  See iau_DAT for further details.
*
*  4) The routine iau_DTF2D converts from calendar date and time of day
*     into 2-part Julian Date, and in the case of UTC implements the
*     leap-second-ambiguity convention described above.
*
*  5) The returned TAI1,TAI2 are such that their sum is the TAI Julian
*     Date.
*
*  Called:
*     iau_JD2CAL   JD to Gregorian calendar
*     iau_DAT      delta(AT) = TAI-UTC
*     iau_CAL2JD   Gregorian calendar to JD
*
*  References:
*
*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG (2004)
*
*     Explanatory Supplement to the Astronomical Almanac,
*     P. Kenneth Seidelmann (ed), University Science Books (1992)
*
*  This revision:  2010 September 10
*
*  SOFA release 2010-12-01
*
*  Copyright (C) 2010 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE
      DOUBLE PRECISION UTC1, UTC2, TAI1, TAI2
      INTEGER J

*  Days to seconds
      DOUBLE PRECISION D2S
      PARAMETER ( D2S = 86400D0 )

      LOGICAL BIG1
      INTEGER IY, IM, ID, JS, IYT, IMT, IDT
      DOUBLE PRECISION U1, U2, FD, DATS, FDT, DATST, DDAT, Z1, Z2, A2

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Put the two parts of the UTC into big-first order.
      BIG1 = UTC1.GE.UTC2
      IF ( BIG1 ) THEN
         U1 = UTC1
         U2 = UTC2
      ELSE
         U1 = UTC2
         U2 = UTC1
      END IF

*  Get TAI-UTC now.
      CALL iau_JD2CAL ( U1, U2, IY, IM, ID, FD, JS )
      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IY, IM, ID, FD, DATS, JS )
      IF ( JS.LT.0 ) GO TO 9

*  Get TAI-UTC tomorrow.
      CALL iau_JD2CAL ( U1+1.5D0, U2-FD, IYT, IMT, IDT, FDT, JS )
      IF ( JS.NE.0 ) GO TO 9
      CALL iau_DAT ( IYT, IMT, IDT, FDT, DATST, JS )
      IF ( JS.LT.0 ) GO TO 9

*  If today ends in a leap second, scale the fraction into SI days.
      DDAT = DATST - DATS
      IF ( ABS(DDAT).GT.0.5D0 ) FD = FD + FD*DDAT/D2S

*  Today's calendar date to 2-part JD.
      CALL iau_CAL2JD ( IY, IM, ID, Z1, Z2, JS )
      IF ( JS.NE.0 ) GO TO 9

*  Assemble the TAI result, preserving the UTC split and order.
      A2 = Z1 - U1
      A2 = ( A2 + Z2 ) + ( FD + DATS/D2S )
      IF ( BIG1 ) THEN
         TAI1 = U1
         TAI2 = A2
      ELSE
         TAI1 = A2
         TAI2 = U1
      END IF

*  Status.
 9    CONTINUE
      IF ( JS.NE.0 ) JS = -1
      J = JS

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2010
*  Standards Of Fundamental Astronomy Board
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
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The name(s) of all routine(s) in your derived work shall not
*        include the prefix "iau".
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. In any published work or commercial products which includes
*     results achieved by using the SOFA software, you shall
*     acknowledge that the SOFA software was used in obtaining those
*     results.
*
*  5. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  6. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  7. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*----------------------------------------------------------------------

      END
