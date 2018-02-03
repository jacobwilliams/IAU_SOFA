      DOUBLE PRECISION FUNCTION iau_EECT00 ( DATE1, DATE2 )
*+
*  - - - - - - - - - - -
*   i a u _ E E C T 0 0
*  - - - - - - - - - - -
*
*  Equation of the equinoxes complementary terms, consistent with
*  IAU 2000 resolutions.
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
*     iau_EECT00    d    complementary terms (Note 2)
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the two arguments.  For example,
*     JD(TT)=2450123.7 could be expressed in any of these ways,
*     among others:
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
*  2) The "complementary terms" are part of the equation of the
*     equinoxes (EE), classically the difference between apparent and
*     mean Sidereal Time:
*
*        GAST = GMST + EE
*
*     with:
*
*        EE = dpsi * cos(eps)
*
*     where dpsi is the nutation in longitude and eps is the obliquity
*     of date.  However, if the rotation of the Earth were constant in
*     an inertial frame the classical formulation would lead to apparent
*     irregularities in the UT1 timescale traceable to side-effects of
*     precession-nutation.  In order to eliminate these effects from
*     UT1, "complementary terms" were introduced in 1994 (IAU, 1994) and
*     took effect from 1997 (Capitaine and Gontier, 1993):
*
*        GAST = GMST + CT + EE
*
*     By convention, the complementary terms are included as part of the
*     equation of the equinoxes rather than as part of the mean Sidereal
*     Time.  This slightly compromises the "geometrical" interpretation
*     of mean sidereal time but is otherwise inconsequential.
*
*     The present routine computes CT in the above expression,
*     compatible with IAU 2000 resolutions (Capitaine et al., 2002, and
*     IERS Conventions 2003).
*
*  Called:
*     iau_FAL03    mean anomaly of the Moon
*     iau_FALP03   mean anomaly of the Sun
*     iau_FAF03    mean argument of the latitude of the Moon
*     iau_FAD03    mean elongation of the Moon from the Sun
*     iau_FAOM03   mean longitude of the Moon's ascending node
*     iau_FAVE03   mean longitude of Venus
*     iau_FAE03    mean longitude of Earth
*     iau_FAPA03   general accumulated precession in longitude
*
*  References:
*
*     Capitaine, N. & Gontier, A.-M., Astron. Astrophys., 275,
*     645-650 (1993)
*
*     Capitaine, N., Wallace, P.T. and McCarthy, D.D., "Expressions to
*     implement the IAU 2000 definition of UT1", Astronomy &
*     Astrophysics, 406, 1135-1149 (2003)
*
*     IAU Resolution C7, Recommendation 3 (1994)
*
*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG (2004)
*
*  This revision:  2007 February 9
*
*  Copyright (C) 2007 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2

*  2Pi
      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

*  Arcseconds to radians
      DOUBLE PRECISION DAS2R
      PARAMETER ( DAS2R = 4.848136811095359935899141D-6 )

*  Reference epoch (J2000), JD
      DOUBLE PRECISION DJ0
      PARAMETER ( DJ0 = 2451545D0 )

*  Days per Julian century
      DOUBLE PRECISION DJC
      PARAMETER ( DJC = 36525D0 )

*  Time since J2000, in Julian centuries
      DOUBLE PRECISION T

*  Miscellaneous
      INTEGER I, J
      DOUBLE PRECISION A, S0, S1
      DOUBLE PRECISION iau_FAL03, iau_FALP03, iau_FAF03,
     :                 iau_FAD03, iau_FAOM03, iau_FAVE03, iau_FAE03,
     :                 iau_FAPA03

*  Fundamental arguments
      DOUBLE PRECISION FA(14)

*  -----------------------------------------
*  The series for the EE complementary terms
*  -----------------------------------------

*  Number of terms in the series
      INTEGER NE0, NE1
      PARAMETER ( NE0=33, NE1=1 )

*  Coefficients of l,l',F,D,Om,LVe,LE,pA
      INTEGER KE0 ( 8, NE0 ),
     :        KE1 ( 8, NE1 )

*  Sine and cosine coefficients
      DOUBLE PRECISION SE0 ( 2, NE0 ),
     :                 SE1 ( 2, NE1 )

*  Argument coefficients for t^0
      DATA ( ( KE0(I,J), I=1,8), J=1,10 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,
     :  0,  0,  0,  0,  2,  0,  0,  0,
     :  0,  0,  2, -2,  3,  0,  0,  0,
     :  0,  0,  2, -2,  1,  0,  0,  0,
     :  0,  0,  2, -2,  2,  0,  0,  0,
     :  0,  0,  2,  0,  3,  0,  0,  0,
     :  0,  0,  2,  0,  1,  0,  0,  0,
     :  0,  0,  0,  0,  3,  0,  0,  0,
     :  0,  1,  0,  0,  1,  0,  0,  0,
     :  0,  1,  0,  0, -1,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,8), J=11,20 ) /
     :  1,  0,  0,  0, -1,  0,  0,  0,
     :  1,  0,  0,  0,  1,  0,  0,  0,
     :  0,  1,  2, -2,  3,  0,  0,  0,
     :  0,  1,  2, -2,  1,  0,  0,  0,
     :  0,  0,  4, -4,  4,  0,  0,  0,
     :  0,  0,  1, -1,  1, -8, 12,  0,
     :  0,  0,  2,  0,  0,  0,  0,  0,
     :  0,  0,  2,  0,  2,  0,  0,  0,
     :  1,  0,  2,  0,  3,  0,  0,  0,
     :  1,  0,  2,  0,  1,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,8), J=21,30 ) /
     :  0,  0,  2, -2,  0,  0,  0,  0,
     :  0,  1, -2,  2, -3,  0,  0,  0,
     :  0,  1, -2,  2, -1,  0,  0,  0,
     :  0,  0,  0,  0,  0,  8,-13, -1,
     :  0,  0,  0,  2,  0,  0,  0,  0,
     :  2,  0, -2,  0, -1,  0,  0,  0,
     :  1,  0,  0, -2,  1,  0,  0,  0,
     :  0,  1,  2, -2,  2,  0,  0,  0,
     :  1,  0,  0, -2, -1,  0,  0,  0,
     :  0,  0,  4, -2,  4,  0,  0,  0 /
      DATA ( ( KE0(I,J), I=1,8), J=31,NE0 ) /
     :  0,  0,  2, -2,  4,  0,  0,  0,
     :  1,  0, -2,  0, -3,  0,  0,  0,
     :  1,  0, -2,  0, -1,  0,  0,  0 /

*  Argument coefficients for t^1
      DATA ( ( KE1(I,J), I=1,8), J=1,NE1 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0 /

*  Sine and cosine coefficients for t^0
      DATA ( ( SE0(I,J), I=1,2), J =    1,   10 ) /
     :            +2640.96D-6,          -0.39D-6,
     :              +63.52D-6,          -0.02D-6,
     :              +11.75D-6,          +0.01D-6,
     :              +11.21D-6,          +0.01D-6,
     :               -4.55D-6,          +0.00D-6,
     :               +2.02D-6,          +0.00D-6,
     :               +1.98D-6,          +0.00D-6,
     :               -1.72D-6,          +0.00D-6,
     :               -1.41D-6,          -0.01D-6,
     :               -1.26D-6,          -0.01D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   11,   20 ) /
     :               -0.63D-6,          +0.00D-6,
     :               -0.63D-6,          +0.00D-6,
     :               +0.46D-6,          +0.00D-6,
     :               +0.45D-6,          +0.00D-6,
     :               +0.36D-6,          +0.00D-6,
     :               -0.24D-6,          -0.12D-6,
     :               +0.32D-6,          +0.00D-6,
     :               +0.28D-6,          +0.00D-6,
     :               +0.27D-6,          +0.00D-6,
     :               +0.26D-6,          +0.00D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   21,   30 ) /
     :               -0.21D-6,          +0.00D-6,
     :               +0.19D-6,          +0.00D-6,
     :               +0.18D-6,          +0.00D-6,
     :               -0.10D-6,          +0.05D-6,
     :               +0.15D-6,          +0.00D-6,
     :               -0.14D-6,          +0.00D-6,
     :               +0.14D-6,          +0.00D-6,
     :               -0.14D-6,          +0.00D-6,
     :               +0.14D-6,          +0.00D-6,
     :               +0.13D-6,          +0.00D-6 /
      DATA ( ( SE0(I,J), I=1,2), J =   31,  NE0 ) /
     :               -0.11D-6,          +0.00D-6,
     :               +0.11D-6,          +0.00D-6,
     :               +0.11D-6,          +0.00D-6 /

*  Sine and cosine coefficients for t^1
      DATA ( ( SE1(I,J), I=1,2), J =    1,  NE1 ) /
     :               -0.87D-6,          +0.00D-6 /

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Interval between fundamental epoch J2000.0 and current date (JC).
      T = ( ( DATE1-DJ0 ) + DATE2 ) / DJC

*  Fundamental Arguments (from IERS Conventions 2003)

*  Mean anomaly of the Moon.
      FA(1) = iau_FAL03 ( T )

*  Mean anomaly of the Sun.
      FA(2) = iau_FALP03 ( T )

*  Mean longitude of the Moon minus that of the ascending node.
      FA(3) = iau_FAF03 ( T )

*  Mean elongation of the Moon from the Sun.
      FA(4) = iau_FAD03 ( T )

*  Mean longitude of the ascending node of the Moon.
      FA(5) = iau_FAOM03 ( T )

*  Mean longitude of Venus.
      FA(6) = iau_FAVE03 ( T )

*  Mean longitude of Earth.
      FA(7) = iau_FAE03 ( T )

*  General precession in longitude.
      FA(8) = iau_FAPA03 ( T )

*  Evaluate the EE complementary terms.
      S0 = 0D0
      S1 = 0D0

      DO 2 I = NE0,1,-1
         A = 0D0
         DO 1 J=1,8
            A = A + DBLE(KE0(J,I))*FA(J)
 1       CONTINUE
         S0 = S0 + ( SE0(1,I)*SIN(A) + SE0(2,I)*COS(A) )
 2    CONTINUE
      DO 4 I = NE1,1,-1
         A = 0D0
         DO 3 J=1,8
            A = A + DBLE(KE1(J,I))*FA(J)
 3       CONTINUE
         S1 = S1 + ( SE1(1,I)*SIN(A) + SE1(2,I)*COS(A) )
 4    CONTINUE
      iau_EECT00 = ( S0 + S1 * T ) * DAS2R

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2007
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
