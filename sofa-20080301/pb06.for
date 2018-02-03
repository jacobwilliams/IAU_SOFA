      SUBROUTINE iau_PB06 ( DATE1, DATE2, BZETA, BZ, BTHETA )
*+
*  - - - - - - - - -
*   i a u _ P B 0 6
*  - - - - - - - - -
*
*  This routine forms three Euler angles which implement general
*  precession from epoch J2000.0, using the IAU 2006 model.  Frame
*  bias (the offset between ICRS and mean J2000.0) is included.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     DATE1,DATE2   d    TT as a 2-part Julian Date (Note 1)
*
*  Returned:
*     BZETA         d    1st rotation: radians clockwise around z
*     BZ            d    3rd rotation: radians clockwise around z
*     BTHETA        d    2nd rotation: radians counterclockwise around y
*
*  Notes:
*
*  1) The TT date DATE1+DATE2 is a Julian Date, apportioned in any
*     convenient way between the arguments DATE1 and DATE2.  For
*     example, JD(TT)=2450123.7 could be expressed in any of these
*     ways, among others:
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
*  2) The traditional accumulated precession angles zeta_A, z_A, theta_A
*     cannot be obtained in the usual way, namely through polynomial
*     expressions, because of the frame bias.  The latter means that two
*     of the angles undergo rapid changes near this date.  They are
*     instead the results of decomposing the precession-bias matrix
*     obtained by using the Fukushima-Williams method, which does not
*     suffer from the problem.  The decomposition returns values which
*     can be used in the conventional formulation and which include
*     frame bias.
*
*  3) The three angles are returned in the conventional order, which
*     is not the same as the order of the corresponding Euler rotations.
*     The precession-bias matrix is R_3(-z) x R_2(+theta) x R_3(-zeta).
*
*  4) Should zeta_A, z_A, theta_A angles be required that do not contain
*     frame bias, they are available by calling the SOFA routine
*     iau_P06E.
*
*  Called:
*     iau_PMAT06   PB matrix, IAU 2006
*     iau_RZ       rotate around Z-axis
*
*  This revision:  2007 June 8
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION DATE1, DATE2, BZETA, BZ, BTHETA

      DOUBLE PRECISION R(3,3), R31, R32

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Precession matrix via Fukushima-Williams angles.
      CALL iau_PMAT06 ( DATE1, DATE2, R )

*  Solve for z.
      BZ = ATAN2 ( R(2,3), R(1,3) )

*  Remove it from the matrix.
      CALL iau_RZ ( BZ, R )

*  Solve for the remaining two angles.
      BZETA = ATAN2 ( R(2,1), R(2,2) )
      R31 = R(3,1)
      R32 = R(3,2)
      BTHETA = ATAN2 ( -SIGN(SQRT(R31*R31+R32*R32),R(1,3)), R(3,3) )

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
