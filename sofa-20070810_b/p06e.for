      SUBROUTINE iau_P06E ( DATE1, DATE2,
     :                      EPS0, PSIA, OMA, BPA, BQA, PIA, BPIA,
     :                      EPSA, CHIA, ZA, ZETAA, THETAA, PA,
     :                      GAM, PHI, PSI )
*+
*  - - - - - - - - -
*   i a u _ P 0 6 E
*  - - - - - - - - -
*
*  Precession angles, IAU 2006, equinox based.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  canonical models.
*
*  Given:
*     DATE1,DATE2   d    TT as a 2-part Julian Date (Note 1)
*
*  Returned (see Note 2):
*     EPS0          d    epsilon_0
*     PSIA          d    psi_A
*     OMA           d    omega_A
*     BPA           d    P_A
*     BQA           d    Q_A
*     PIA           d    pi_A
*     BPIA          d    Pi_A
*     EPSA          d    obliquity epsilon_A
*     CHIA          d    chi_A
*     ZA            d    z_A
*     ZETAA         d    zeta_A
*     THETAA        d    theta_A
*     PA            d    p_A
*     GAM           d    F-W angle gamma_J2000
*     PHI           d    F-W angle phi_J2000
*     PSI           d    F-W angle psi_J2000
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
*  2) This routine returns the set of equinox based angles for the
*     Capitaine et al. "P03" precession theory, adopted by the IAU in
*     2006.  The angles are set out in Table 1 of Hilton et al. (2006):
*
*      EPS0   epsilon_0   obliquity at J2000
*      PSIA   psi_A       luni-solar precession
*      OMA    omega_A     inclination of equator wrt J2000 ecliptic
*      BPA    P_A         ecliptic pole x, J2000 ecliptic triad
*      BQA    Q_A         ecliptic pole -y, J2000 ecliptic triad
*      PIA    pi_A        angle between moving and J2000 ecliptics
*      BPIA   Pi_A        longitude of ascending node of the ecliptic
*      EPSA   epsilon_A   obliquity of the ecliptic
*      CHIA   chi_A       planetary precession
*      ZA     z_A         equatorial precession: -3rd 323 Euler angle
*      ZETAA  zeta_A      equatorial precession: -1st 323 Euler angle
*      THETAA theta_A     equatorial precession: 2nd 323 Euler angle
*      PA     p_A         general precession
*      GAM    gamma_J2000 J2000 RA difference of ecliptic poles
*      PHI    phi_J2000   J2000 codeclination of ecliptic pole
*      PSI    psi_J2000   longitude difference of equator poles, J2000
*
*     The returned values are all radians.
*
*  3) Hilton et al. (2006) Table 1 also contains angles that depend on
*     models distinct from the P03 precession theory itself, namely the
*     IAU 2000A frame bias and nutation.  The quoted polynomials are
*     used in other SOFA routines:
*
*     . iau_XY06 contains the polynomial parts of the X and Y series.
*
*     . iau_S06 contains the polynomial part of the s+XY/2 series.
*
*     . iau_PFW06 implements the series for the Fukushima-Williams
*       angles that are with respect to the GCRS pole (i.e. the variants
*       that include frame bias).
*
*  4) The IAU resolution stipulated that the choice of parameterization
*     was left to the user, and so an IAU compliant precession
*     implementation can be constructed using various combinations of
*     the angles returned by the present routine.
*
*  5) The parameterization used by SOFA is the Fukushima-Williams angles
*     referred directly to the GCRS pole.  These are the final four
*     arguments returned by the present routine, but are more
*     efficiently calculated by calling the routine iau_PFW06.  SOFA
*     also supports the direct computation of the CIP GCRS X,Y by
*     series, available by calling iau_XY06.
*
*  6) The agreement between the different parameterizations is at the
*     1 microarcsecond level in the present era.
*
*  7) When constructing a precession formulation that refers to the GCRS
*     pole rather than the dynamical pole, it may (depending on the
*     choice of angles) be necessary to introduce the frame bias
*     explicitly.
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
*  Copyright (C) 2007 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2,
     :                 EPS0, PSIA, OMA, BPA, BQA, PIA, BPIA,
     :                 EPSA, CHIA, ZA, ZETAA, THETAA, PA,
     :                 GAM, PHI, PSI

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

*  Obliquity at J2000.

      EPS0 = 84381.406D0 * DAS2R

*  Luni-solar precession.

      PSIA = ( 5038.481507D0     +
     :       (   -1.0790069D0    +
     :       (   -0.00114045D0   +
     :       (    0.000132851D0  +
     :       (   -0.0000000951D0 )
     :       * T ) * T ) * T ) * T ) * T * DAS2R

*  Inclination of mean equator with respect to the J2000 ecliptic.

      OMA = EPS0 + ( -0.025754D0     +
     :             (  0.0512623D0    +
     :             ( -0.00772503D0   +
     :             ( -0.000000467D0  +
     :             (  0.0000003337D0 )
     :             * T ) * T ) * T ) * T ) * T * DAS2R

*  Ecliptic pole x, J2000 ecliptic triad.

      BPA = (  4.199094D0     +
     :      (  0.1939873D0    +
     :      ( -0.00022466D0   +
     :      ( -0.000000912D0  +
     :      (  0.0000000120D0 )
     :      * T ) * T ) * T ) * T ) * T * DAS2R

*  Ecliptic pole -y, J2000 ecliptic triad.

      BQA = ( -46.811015D0     +
     :      (   0.0510283D0    +
     :      (   0.00052413D0   +
     :      (  -0.000000646D0  +
     :      (  -0.0000000172D0 )
     :      * T ) * T ) * T ) * T ) * T * DAS2R

*  Angle between moving and J2000 ecliptics.

      PIA = ( 46.998973D0     +
     :      ( -0.0334926D0    +
     :      ( -0.00012559D0   +
     :      (  0.000000113D0  +
     :      ( -0.0000000022D0 )
     :      * T ) * T ) * T ) * T ) * T * DAS2R

*  Longitude of ascending node of the moving ecliptic.

      BPIA = ( 629546.7936D0      +
     :       (   -867.95758D0     +
     :       (      0.157992D0    +
     :       (     -0.0005371D0   +
     :       (     -0.00004797D0  +
     :       (      0.000000072D0 )
     :       * T ) * T ) * T ) * T ) * T ) * DAS2R

*  Mean obliquity of the ecliptic.

      EPSA = iau_OBL06 ( DATE1, DATE2 )

*  Planetary precession.

      CHIA = ( 10.556403D0     +
     :       ( -2.3814292D0    +
     :       ( -0.00121197D0   +
     :       (  0.000170663D0  +
     :       ( -0.0000000560D0 )
     :       * T ) * T ) * T ) * T ) * T * DAS2R

*  Equatorial precession: minus the third of the 323 Euler angles.

      ZA = (   -2.650545D0     +
     :     ( 2306.077181D0     +
     :     (    1.0927348D0    +
     :     (    0.01826837D0   +
     :     (   -0.000028596D0  +
     :     (   -0.0000002904D0 )
     :     * T ) * T ) * T ) * T ) * T ) * DAS2R

*  Equatorial precession: minus the first of the 323 Euler angles.

      ZETAA = (    2.650545D0     +
     :        ( 2306.083227D0     +
     :        (    0.2988499D0    +
     :        (    0.01801828D0   +
     :        (   -0.000005971D0  +
     :        (   -0.0000003173D0 )
     :        * T ) * T ) * T ) * T ) * T ) * DAS2R

*  Equatorial precession: second of the 323 Euler angles.

      THETAA = ( 2004.191903D0     +
     :         (   -0.4294934D0    +
     :         (   -0.04182264D0   +
     :         (   -0.000007089D0  +
     :         (   -0.0000001274D0 )
     :         * T ) * T ) * T ) * T ) * T * DAS2R

*  General precession.

      PA = ( 5028.796195D0     +
     :     (    1.1054348D0    +
     :     (    0.00007964D0   +
     :     (   -0.000023857D0  +
     :     (    0.0000000383D0 )
     :     * T ) * T ) * T ) * T ) * T * DAS2R

*  Fukushima-Williams angles for precession.

      GAM = ( 10.556403D0     +
     :      (  0.4932044D0    +
     :      ( -0.00031238D0   +
     :      ( -0.000002788D0  +
     :      (  0.0000000260D0 )
     :      * T ) * T ) * T ) * T ) * T * DAS2R

      PHI = EPS0 + ( -46.811015D0     +
     :             (   0.0511269D0    +
     :             (   0.00053289D0   +
     :             (  -0.000000440D0  +
     :             (  -0.0000000176D0 )
     :             * T ) * T ) * T ) * T ) * T * DAS2R

      PSI = ( 5038.481507D0     +
     :      (    1.5584176D0    +
     :      (   -0.00018522D0   +
     :      (   -0.000026452D0  +
     :      (   -0.0000000148D0 )
     :      * T ) * T ) * T ) * T ) * T * DAS2R

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
