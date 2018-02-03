      SUBROUTINE iau_PFW06 ( DATE1, DATE2, GAMB, PHIB, PSIB, EPSA )
*+
*  - - - - - - - - - -
*   i a u _ P F W 0 6
*  - - - - - - - - - -
*
*  Precession angles, IAU 2006 (Fukushima-Williams 4-angle formulation).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2   d    TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     GAMB          d    F-W angle gamma_bar (radians)
*     PHIB          d    F-W angle phi_bar (radians)
*     PSIB          d    F-W angle psi_bar (radians)
*     EPSA          d    F-W angle epsilon_A (radians)
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others
*
*            DATE1          DATE2
*
*         2450123.7D0        0D0        (JD method)
*          2451545D0      -1421.3D0     (J2000 method)
*         2400000.5D0     50123.2D0     (MJD method)
*         2450123.5D0       0.2D0       (date & time method)
*
*     The JD method is the most natural and convenient to use in
*     cases where the loss of several decimal digits of resolution
*     is acceptable.  The J2000 method is best matched to the way
*     the argument is handled internally and will deliver the
*     optimum resolution.  The MJD method and the date & time methods
*     are both good compromises between resolution and convenience.
*
*  2) Naming the following points:
*
*           e = J2000 ecliptic pole,
*           p = GCRS pole,
*           E = mean ecliptic pole of date,
*     and   P = mean pole of date,
*
*     the four Fukushima-Williams angles are as follows:
*
*        GAMB = gamma_bar = epE
*        PHIB = phi_bar = pE
*        PSIB = psi_bar = pEP
*        EPSA = epsilon_A = EP
*
*  3) The matrix representing the combined effects of frame bias and
*     precession is:
*
*        PxB = R_1(-EPSA).R_3(-PSIB).R_1(PHIB).R_3(GAMB)
*
*  4) The matrix representing the combined effects of frame bias,
*     precession and nutation is simply:
*
*        NxPxB = R_1(-EPSA-dE).R_3(-PSIB-dP).R_1(PHIB).R_3(GAMB)
*
*     where dP and dE are the nutation components with respect to the
*     ecliptic of date.
*
*  Reference:
*
*     Hilton, J. et al., 2006, Celest.Mech.Dyn.Astron. 94, 351
*
*  Called:
*     iau_OBL06    mean obliquity, IAU 2006
*
*  This revision:  2007 June 8
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, GAMB, PHIB, PSIB, EPSA

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

      DOUBLE PRECISION T

      DOUBLE PRECISION iau_OBL06

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental date J2000.0 and given date (JC).
      T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC

*  P03 bias+precession angles.
      GAMB =        (    -0.052928D0    +
     :              (    10.556378D0    +
     :              (     0.4932044D0   +
     :              (    -0.00031238D0  +
     :              (    -0.000002788D0 +
     :              (     0.0000000260D0 )
     :                             * T ) * T ) * T ) * T ) * T ) * DAS2R
      PHIB =        ( 84381.412819D0    +
     :              (   -46.811016D0    +
     :              (     0.0511268D0   +
     :              (     0.00053289D0  +
     :              (    -0.000000440D0 +
     :              (    -0.0000000176D0 )
     :                             * T ) * T ) * T ) * T ) * T ) * DAS2R
      PSIB =        (    -0.041775D0    +
     :              (  5038.481484D0    +
     :              (     1.5584175D0   +
     :              (    -0.00018522D0  +
     :              (    -0.000026452D0 +
     :              (    -0.0000000148D0 )
     :                             * T ) * T ) * T ) * T ) * T ) * DAS2R
      EPSA = iau_OBL06 ( DATE1, DATE2 )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2008
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
