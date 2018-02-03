      SUBROUTINE iau_PV2S ( PV, THETA, PHI, R, TD, PD, RD )
*+
*  - - - - - - - - -
*   i a u _ P V 2 S
*  - - - - - - - - -
*
*  Convert position/velocity from Cartesian to spherical coordinates.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     PV       d(3,2)    pv-vector
*
*  Returned:
*     THETA    d         longitude angle (radians)
*     PHI      d         latitude angle (radians)
*     R        d         radial distance
*     TD       d         rate of change of THETA
*     PD       d         rate of change of PHI
*     RD       d         rate of change of R
*
*  Notes:
*
*  1) If the position part of PV is null, THETA, PHI, TD and PD
*     are indeterminate.  This is handled by extrapolating the
*     position through unit time by using the velocity part of
*     PV.  This moves the origin without changing the direction
*     of the velocity component.  If the position and velocity
*     components of PV are both null, zeroes are returned for all
*     six results.
*
*  2) If the position is a pole, THETA, TD and PD are indeterminate.
*     In such cases zeroes are returned for all three.
*
*  This revision:  2008 May 10
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION PV(3,2), THETA, PHI, R, TD, PD, RD

      DOUBLE PRECISION X, Y, Z, XD, YD, ZD, RXY2, RXY, R2,
     :                 RTRUE, RW, XYP

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Components of position/velocity vector.
      X =  PV(1,1)
      Y =  PV(2,1)
      Z =  PV(3,1)
      XD = PV(1,2)
      YD = PV(2,2)
      ZD = PV(3,2)

*  Component of R in XY plane squared.
      RXY2 = X*X + Y*Y

*  Modulus squared.
      R2 = RXY2 + Z*Z

*  Modulus.
      RTRUE = SQRT(R2)

*  If null vector, move the origin along the direction of movement.
      RW = RTRUE
      IF ( RTRUE .EQ. 0D0 ) THEN
         X = XD
         Y = YD
         Z = ZD
         RXY2 = X*X + Y*Y
         R2 = RXY2 + Z*Z
         RW = SQRT(R2)
      END IF

*  Position and velocity in spherical coordinates.
      RXY = SQRT(RXY2)
      XYP = X*XD + Y*YD
      IF ( RXY2 .NE. 0D0 ) THEN
         THETA = ATAN2(Y,X)
         PHI = ATAN2(Z,RXY)
         TD = ( X*YD - Y*XD ) / RXY2
         PD = ( ZD*RXY2 - Z*XYP ) / ( R2*RXY )
      ELSE
         THETA = 0D0
         IF ( Z.NE.0D0 ) THEN
            PHI = ATAN2(Z,RXY)
         ELSE
            PHI = 0D0
         END IF
         TD = 0D0
         PD = 0D0
      END IF
      R = RTRUE
      IF ( RW.NE.0D0 ) THEN
         RD = ( XYP + Z*ZD ) / RW
      ELSE
         RD = 0D0
      END IF

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
