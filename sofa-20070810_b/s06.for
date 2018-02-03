      DOUBLE PRECISION FUNCTION iau_S06 ( DATE1, DATE2, X, Y )
*+
*  - - - - - - - -
*   i a u _ S 0 6
*  - - - - - - - -
*
*  The CIO locator s, positioning the Celestial Intermediate Origin on
*  the equator of the Celestial Intermediate Pole, given the CIP's X,Y
*  coordinates.  Compatible with IAU 2006/2000A precession-nutation.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical model.
*
*  Given:
*     DATE1,DATE2    d      TT as a 2-part Julian Date (Note 1)
*     X,Y            d      CIP coordinates (Note 3)
*
*  Returned:
*     iau_S06        d      the CIO locator s in radians (Note 2)
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
*  2) The CIO locator s is the difference between the right ascensions
*     of the same point in two systems:  the two systems are the GCRS
*     and the CIP,CIO, and the point is the ascending node of the
*     CIP equator.  The quantity s remains below 0.1 arcsecond
*     throughout 1900-2100.
*
*  3) The series used to compute s is in fact for s+XY/2, where X and Y
*     are the x and y components of the CIP unit vector;  this series is
*     more compact than a direct series for s would be.  This routine
*     requires X,Y to be supplied by the caller, who is responsible for
*     providing values that are consistent with the supplied date.
*
*  4) The model is consistent with the "P03" precession (Capitaine et
*     al. 2003), adopted by IAU 2006 Resolution 1, 2006, and the
*     IAU 2000A nutation (with P03 adjustments).
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
*     Capitaine, N., Wallace, P.T. & Chapront, J., 2003, Astron.
*     Astrophys. 432, 355
*
*     McCarthy, D.D., Petit, G. (eds.) 2004, IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG
*
*  Last revision:   2007 May 29
*
*  Copyright (C) 2007 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, X, Y

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

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
      DOUBLE PRECISION A, S0, S1, S2, S3, S4, S5
      DOUBLE PRECISION iau_FAL03, iau_FALP03, iau_FAF03,
     :                 iau_FAD03, iau_FAOM03, iau_FAVE03, iau_FAE03,
     :                 iau_FAPA03

*  Fundamental arguments
      DOUBLE PRECISION FA(8)

*  ---------------------
*  The series for s+XY/2
*  ---------------------

*  Number of terms in the series
      INTEGER NSP, NS0, NS1, NS2, NS3, NS4
      PARAMETER ( NSP=6, NS0=33, NS1=3, NS2=25, NS3=4, NS4=1 )

*  Polynomial coefficients
      DOUBLE PRECISION SP ( NSP )

*  Coefficients of l,l',F,D,Om,LVe,LE,pA
      INTEGER KS0 ( 8, NS0 ),
     :        KS1 ( 8, NS1 ),
     :        KS2 ( 8, NS2 ),
     :        KS3 ( 8, NS3 ),
     :        KS4 ( 8, NS4 )

*  Sine and cosine coefficients
      DOUBLE PRECISION SS0 ( 2, NS0 ),
     :                 SS1 ( 2, NS1 ),
     :                 SS2 ( 2, NS2 ),
     :                 SS3 ( 2, NS3 ),
     :                 SS4 ( 2, NS4 )

*  Polynomial coefficients
      DATA SP /    94    D-6,
     :           3808.65 D-6,
     :           -122.68 D-6,
     :         -72574.11 D-6,
     :             27.98 D-6,
     :             15.62 D-6 /

*  Argument coefficients for t^0
      DATA ( ( KS0(I,J), I=1,8), J=1,10 ) /
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
      DATA ( ( KS0(I,J), I=1,8), J=11,20 ) /
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
      DATA ( ( KS0(I,J), I=1,8), J=21,30 ) /
     :  0,  0,  2, -2,  0,  0,  0,  0,
     :  0,  1, -2,  2, -3,  0,  0,  0,
     :  0,  1, -2,  2, -1,  0,  0,  0,
     :  0,  0,  0,  0,  0,  8,-13, -1,
     :  0,  0,  0,  2,  0,  0,  0,  0,
     :  2,  0, -2,  0, -1,  0,  0,  0,
     :  0,  1,  2, -2,  2,  0,  0,  0,
     :  1,  0,  0, -2,  1,  0,  0,  0,
     :  1,  0,  0, -2, -1,  0,  0,  0,
     :  0,  0,  4, -2,  4,  0,  0,  0 /
      DATA ( ( KS0(I,J), I=1,8), J=31,NS0 ) /
     :  0,  0,  2, -2,  4,  0,  0,  0,
     :  1,  0, -2,  0, -3,  0,  0,  0,
     :  1,  0, -2,  0, -1,  0,  0,  0 /

*  Argument coefficients for t^1
      DATA ( ( KS1(I,J), I=1,8), J=1,NS1 ) /
     :  0,  0,  0,  0,  2,  0,  0,  0,
     :  0,  0,  0,  0,  1,  0,  0,  0,
     :  0,  0,  2, -2,  3,  0,  0,  0 /

*  Argument coefficients for t^2
      DATA ( ( KS2(I,J), I=1,8), J=1,10 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,
     :  0,  0,  2, -2,  2,  0,  0,  0,
     :  0,  0,  2,  0,  2,  0,  0,  0,
     :  0,  0,  0,  0,  2,  0,  0,  0,
     :  0,  1,  0,  0,  0,  0,  0,  0,
     :  1,  0,  0,  0,  0,  0,  0,  0,
     :  0,  1,  2, -2,  2,  0,  0,  0,
     :  0,  0,  2,  0,  1,  0,  0,  0,
     :  1,  0,  2,  0,  2,  0,  0,  0,
     :  0,  1, -2,  2, -2,  0,  0,  0 /
      DATA ( ( KS2(I,J), I=1,8), J=11,20 ) /
     :  1,  0,  0, -2,  0,  0,  0,  0,
     :  0,  0,  2, -2,  1,  0,  0,  0,
     :  1,  0, -2,  0, -2,  0,  0,  0,
     :  0,  0,  0,  2,  0,  0,  0,  0,
     :  1,  0,  0,  0,  1,  0,  0,  0,
     :  1,  0, -2, -2, -2,  0,  0,  0,
     :  1,  0,  0,  0, -1,  0,  0,  0,
     :  1,  0,  2,  0,  1,  0,  0,  0,
     :  2,  0,  0, -2,  0,  0,  0,  0,
     :  2,  0, -2,  0, -1,  0,  0,  0 /
      DATA ( ( KS2(I,J), I=1,8), J=21,NS2 ) /
     :  0,  0,  2,  2,  2,  0,  0,  0,
     :  2,  0,  2,  0,  2,  0,  0,  0,
     :  2,  0,  0,  0,  0,  0,  0,  0,
     :  1,  0,  2, -2,  2,  0,  0,  0,
     :  0,  0,  2,  0,  0,  0,  0,  0 /

*  Argument coefficients for t^3
      DATA ( ( KS3(I,J), I=1,8), J=1,NS3 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0,
     :  0,  0,  2, -2,  2,  0,  0,  0,
     :  0,  0,  2,  0,  2,  0,  0,  0,
     :  0,  0,  0,  0,  2,  0,  0,  0 /

*  Argument coefficients for t^4
      DATA ( ( KS4(I,J), I=1,8), J=1,NS4 ) /
     :  0,  0,  0,  0,  1,  0,  0,  0 /

*  Sine and cosine coefficients for t^0
      DATA ( ( SS0(I,J), I=1,2), J=1,10 ) /
     :            -2640.73D-6,          +0.39D-6,
     :              -63.53D-6,          +0.02D-6,
     :              -11.75D-6,          -0.01D-6,
     :              -11.21D-6,          -0.01D-6,
     :               +4.57D-6,           0.00D-6,
     :               -2.02D-6,           0.00D-6,
     :               -1.98D-6,           0.00D-6,
     :               +1.72D-6,           0.00D-6,
     :               +1.41D-6,          +0.01D-6,
     :               +1.26D-6,          +0.01D-6 /
      DATA ( ( SS0(I,J), I=1,2), J=11,20 ) /
     :               +0.63D-6,           0.00D-6,
     :               +0.63D-6,           0.00D-6,
     :               -0.46D-6,           0.00D-6,
     :               -0.45D-6,           0.00D-6,
     :               -0.36D-6,           0.00D-6,
     :               +0.24D-6,          +0.12D-6,
     :               -0.32D-6,           0.00D-6,
     :               -0.28D-6,           0.00D-6,
     :               -0.27D-6,           0.00D-6,
     :               -0.26D-6,           0.00D-6 /
      DATA ( ( SS0(I,J), I=1,2), J=21,30 ) /
     :               +0.21D-6,           0.00D-6,
     :               -0.19D-6,           0.00D-6,
     :               -0.18D-6,           0.00D-6,
     :               +0.10D-6,          -0.05D-6,
     :               -0.15D-6,           0.00D-6,
     :               +0.14D-6,           0.00D-6,
     :               +0.14D-6,           0.00D-6,
     :               -0.14D-6,           0.00D-6,
     :               -0.14D-6,           0.00D-6,
     :               -0.13D-6,           0.00D-6 /
      DATA ( ( SS0(I,J), I=1,2), J=31,NS0 ) /
     :               +0.11D-6,           0.00D-6,
     :               -0.11D-6,           0.00D-6,
     :               -0.11D-6,           0.00D-6 /

*  Sine and cosine coefficients for t^1
      DATA ( ( SS1(I,J), I=1,2), J=1,NS1 ) /
     :               -0.07D-6,          +3.57D-6,
     :               +1.73D-6,          -0.03D-6,
     :                0.00D-6,          +0.48D-6 /

*  Sine and cosine coefficients for t^2
      DATA ( ( SS2(I,J), I=1,2), J=1,10 ) /
     :             +743.52D-6,          -0.17D-6,
     :               56.91D-6,          +0.06D-6,
     :               +9.84D-6,          -0.01D-6,
     :               -8.85D-6,          +0.01D-6,
     :               -6.38D-6,          -0.05D-6,
     :               -3.07D-6,           0.00D-6,
     :               +2.23D-6,           0.00D-6,
     :               +1.67D-6,           0.00D-6,
     :               +1.30D-6,           0.00D-6,
     :               +0.93D-6,           0.00D-6 /
      DATA ( ( SS2(I,J), I=1,2), J=11,20 ) /
     :               +0.68D-6,           0.00D-6,
     :               -0.55D-6,           0.00D-6,
     :               +0.53D-6,           0.00D-6,
     :               -0.27D-6,           0.00D-6,
     :               -0.27D-6,           0.00D-6,
     :               -0.26D-6,           0.00D-6,
     :               -0.25D-6,           0.00D-6,
     :               +0.22D-6,           0.00D-6,
     :               -0.21D-6,           0.00D-6,
     :               +0.20D-6,           0.00D-6 /
      DATA ( ( SS2(I,J), I=1,2), J=21,NS2 ) /
     :               +0.17D-6,           0.00D-6,
     :               +0.13D-6,           0.00D-6,
     :               -0.13D-6,           0.00D-6,
     :               -0.12D-6,           0.00D-6,
     :               -0.11D-6,           0.00D-6 /

*  Sine and cosine coefficients for t^3
      DATA ( ( SS3(I,J), I=1,2), J=1,NS3 ) /
     :               +0.30D-6,         -23.42D-6,
     :               -0.03D-6,          -1.46D-6,
     :               -0.01D-6,          -0.25D-6,
     :                0.00D-6,          +0.23D-6 /

*  Sine and cosine coefficients for t^4
      DATA ( ( SS4(I,J), I=1,2), J=1,NS4 ) /
     :               -0.26D-6,          -0.01D-6 /

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

*  Evaluate S.
      S0 = SP(1)
      S1 = SP(2)
      S2 = SP(3)
      S3 = SP(4)
      S4 = SP(5)
      S5 = SP(6)

      DO 2 I = NS0,1,-1
         A = 0D0
         DO 1 J=1,8
            A = A + DBLE(KS0(J,I))*FA(J)
 1       CONTINUE
         S0 = S0 + ( SS0(1,I)*SIN(A) + SS0(2,I)*COS(A) )
 2    CONTINUE

      DO 4 I = NS1,1,-1
         A = 0D0
         DO 3 J=1,8
            A = A + DBLE(KS1(J,I))*FA(J)
 3       CONTINUE
         S1 = S1 + ( SS1(1,I)*SIN(A) + SS1(2,I)*COS(A) )
 4    CONTINUE

      DO 6 I = NS2,1,-1
         A = 0D0
         DO 5 J=1,8
            A = A + DBLE(KS2(J,I))*FA(J)
 5       CONTINUE
         S2 = S2 + ( SS2(1,I)*SIN(A) + SS2(2,I)*COS(A) )
 6    CONTINUE

      DO 8 I = NS3,1,-1
         A = 0D0
         DO 7 J=1,8
            A = A + DBLE(KS3(J,I))*FA(J)
 7       CONTINUE
         S3 = S3 + ( SS3(1,I)*SIN(A) + SS3(2,I)*COS(A) )
 8    CONTINUE

      DO 10 I = NS4,1,-1
         A = 0D0
         DO 9 J=1,8
            A = A + DBLE(KS4(J,I))*FA(J)
 9       CONTINUE
         S4 = S4 + ( SS4(1,I)*SIN(A) + SS4(2,I)*COS(A) )
 10   CONTINUE

      iau_S06 = ( S0 +
     :          ( S1 +
     :          ( S2 +
     :          ( S3 +
     :          ( S4 +
     :            S5 * T ) * T ) * T ) * T ) * T ) * DAS2R - X*Y/2D0

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
