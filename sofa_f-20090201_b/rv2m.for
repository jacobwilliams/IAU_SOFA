      SUBROUTINE iau_RV2M ( W, R )
*+
*  - - - - - - - - -
*   i a u _ R V 2 M
*  - - - - - - - - -
*
*  Form the r-matrix corresponding to a given r-vector.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     W        d(3)      rotation vector (Note 1)
*
*  Returned:
*     R        d(3,3)    rotation matrix
*
*  Notes:
*
*  1) A rotation matrix describes a rotation through some angle about
*     some arbitrary axis called the Euler axis.  The "rotation vector"
*     supplied to this routine has the same direction as the Euler axis,
*     and its magnitude is the angle in radians.
*
*  2) If W is null, the unit matrix is returned.
*
*  3) The reference frame rotates clockwise as seen looking along the
*     rotation vector from the origin.
*
*  This revision:  2008 May 10
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION W(3), R(3,3)

      DOUBLE PRECISION X, Y, Z, PHI, S, C, F

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Euler angle (magnitude of rotation vector) and functions.
      X = W(1)
      Y = W(2)
      Z = W(3)
      PHI = SQRT(X*X + Y*Y + Z*Z)
      S = SIN(PHI)
      C = COS(PHI)
      F = 1D0 - C

*  Euler axis (direction of rotation vector), perhaps null.
      IF ( PHI .NE. 0D0 ) THEN
         X = X / PHI
         Y = Y / PHI
         Z = Z / PHI
      END IF

*  Form the rotation matrix.
      R(1,1) = X*X*F + C
      R(1,2) = X*Y*F + Z*S
      R(1,3) = X*Z*F - Y*S
      R(2,1) = Y*X*F - Z*S
      R(2,2) = Y*Y*F + C
      R(2,3) = Y*Z*F + X*S
      R(3,1) = Z*X*F + Y*S
      R(3,2) = Z*Y*F - X*S
      R(3,3) = Z*Z*F + C

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
