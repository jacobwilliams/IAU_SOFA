      SUBROUTINE iau_C2TEQX ( RBPN, GST, RPOM, RC2T )
*+
*  - - - - - - - - - - -
*   i a u _ C 2 T E Q X
*  - - - - - - - - - - -
*
*  Assemble the celestial to terrestrial matrix from equinox-based
*  components (the celestial-to-true matrix, the Greenwich Apparent
*  Sidereal Time and the polar motion matrix).
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     RBPN     d(3,3)    celestial-to-true matrix
*     GST        d       Greenwich (apparent) Sidereal Time
*     RPOM     d(3,3)    polar-motion matrix
*
*  Returned:
*     RC2T     d(3,3)    celestial-to-terrestrial matrix (Note 2)
*
*  Notes:
*
*  1) This routine constructs the rotation matrix that transforms
*     vectors in the celestial system into vectors in the terrestrial
*     system.  It does so starting from precomputed components, namely
*     the matrix which rotates from celestial coordinates to the
*     true equator and equinox of date, the Greenwich Apparent Sidereal
*     Time and the polar motion matrix.  One use of the present routine
*     is when generating a series of celestial-to-terrestrial matrices
*     where only the Sidereal Time changes, avoiding the considerable
*     overhead of recomputing the precession-nutation more often than
*     necessary to achieve given accuracy objectives.
*
*  2) The relationship between the arguments is as follows:
*
*        [TRS]  =  RPOM * R_3(GST) * RBPN * [CRS]
*
*               =  RC2T * [CRS]
*
*     where [CRS] is a vector in the Geocentric Celestial Reference
*     System and [TRS] is a vector in the International Terrestrial
*     Reference System (see IERS Conventions 2003).
*
*  Called:
*     iau_CR       copy r-matrix
*     iau_RZ       rotate around Z-axis
*     iau_RXR      product of two r-matrices
*
*  Reference:
*
*     McCarthy, D. D., Petit, G. (eds.), IERS Conventions (2003),
*     IERS Technical Note No. 32, BKG (2004)
*
*  This revision:  2006 November 13
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION RBPN(3,3), GST, RPOM(3,3), RC2T(3,3)

      DOUBLE PRECISION R(3,3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Construct the matrix.
      CALL iau_CR ( RBPN, R )
      CALL iau_RZ ( GST, R )
      CALL iau_RXR ( RPOM, R, RC2T )

*  Finished.

*+-----------------------------------------------------------------------
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
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and 
*     restrictions listed below.
*
*  3. You (the user) may copy and adapt the SOFA software and its 
*     algorithms for your own purposes and you may copy and distribute
*     a resulting "derived work" to others on a world-wide, royalty-free 
*     basis, provided that the derived work complies with the following
*     requirements: 
*
*     a) Your work shall be marked or carry a statement that it (i) uses
*        routines and computations derived by you from software provided 
*        by SOFA under license to you; and (ii) does not contain
*        software provided by SOFA or software that has been distributed
*        by or endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon and/or differs from the
*        original SOFA software.
*
*     c) The name(s) of all routine(s) that you distribute shall differ
*        from the SOFA names, even when the SOFA content has not been
*        otherwise changed.
*
*     d) The routine-naming prefix "iau" shall not be used.
*
*     e) The origin of the SOFA components of your derived work must not
*        be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     f) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have granted 
*        a further right to modify the source code of your derived work.
*
*  4. In any published work or commercial products which includes
*     results achieved by using the SOFA software, you shall acknowledge
*     that the SOFA software was used in obtaining those results.
*
*  5. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or by
*     inappropriate modification.
*
*  6. The SOFA software is provided "as is" and the Board makes no 
*     warranty as to its use or performance.   The Board does not and 
*     cannot warrant the performance or results which the user may obtain 
*     by using the SOFA software.  The Board makes no warranties, express 
*     or implied, as to non-infringement of third party rights,
*     merchantability, or fitness for any particular purpose.  In no
*     event will the Board be liable to the user for any consequential,
*     incidental, or special damages, including any lost profits or lost
*     savings, even if a Board representative has been advised of such
*     damages, or for any claim by any third party.
*
*  7. The provision of any version of the SOFA software under the terms 
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
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
*-----------------------------------------------------------------------

      END
