      SUBROUTINE iau_FW2XY ( GAMB, PHIB, PSI, EPS, X, Y )
*+
*  - - - - - - - - - -
*   i a u _ F W 2 X Y
*  - - - - - - - - - -
*
*  CIP X,Y given Fukushima-Williams bias-precession-nutation angles.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     GAMB       d      F-W angle gamma_bar (radians)
*     PHIB       d      F-W angle phi_bar (radians)
*     PSI        d      F-W angle psi (radians)
*     EPS        d      F-W angle epsilon (radians)
*
*  Returned:
*     X,Y        d      CIP X,Y ("radians")
*
*  Notes:
*
*  1) Naming the following points:
*
*           e = J2000 ecliptic pole,
*           p = GCRS pole
*           E = ecliptic pole of date,
*     and   P = CIP,
*
*     the four Fukushima-Williams angles are as follows:
*
*        GAMB = gamma = epE
*        PHIB = phi = pE
*        PSI = psi = pEP
*        EPS = epsilon = EP
*
*  2) The matrix representing the combined effects of frame bias,
*     precession and nutation is:
*
*        NxPxB = R_1(-EPSA).R_3(-PSI).R_1(PHIB).R_3(GAMB)
*
*     X,Y are elements (3,1) and (3,2) of the matrix.
*
*  Called:
*     iau_FW2M     F-W angles to r-matrix
*     iau_BPN2XY   extract CIP X,Y coordinates from NPB matrix
*
*  Reference:
*
*     Hilton, J. et al., 2006, Celest.Mech.Dyn.Astron. 94, 351
*
*  This revision:   2007 May 9
*
*  Copyright P.T.Wallace.  All rights reserved.
*-

      IMPLICIT NONE

      DOUBLE PRECISION GAMB, PHIB, PSI, EPS, X, Y

      DOUBLE PRECISION R(3,3)


*  Form NxPxB matrix.
      CALL iau_FW2M ( GAMB, PHIB, PSI, EPS, R )

*  Extract CIP X,Y.
      CALL iau_BPN2XY ( R, X, Y )

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
