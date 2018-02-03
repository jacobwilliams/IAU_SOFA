      SUBROUTINE iau_FK52H ( R5, D5, DR5, DD5, PX5, RV5,
     :                       RH, DH, DRH, DDH, PXH, RVH )
*+
*  - - - - - - - - - -
*   i a u _ F K 5 2 H
*  - - - - - - - - - -
*
*  Transform FK5 (J2000) star data into the Hipparcos system.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given (all FK5, equinox J2000, epoch J2000):
*     R5        d      RA (radians)
*     D5        d      Dec (radians)
*     DR5       d      proper motion in RA (dRA/dt, rad/Jyear)
*     DD5       d      proper motion in Dec (dDec/dt, rad/Jyear)
*     PX5       d      parallax (arcsec)
*     RV5       d      radial velocity (km/s, positive = receding)
*
*  Returned (all Hipparcos, epoch J2000):
*     RH        d      RA (radians)
*     DH        d      Dec (radians)
*     DRH       d      proper motion in RA (dRA/dt, rad/Jyear)
*     DDH       d      proper motion in Dec (dDec/dt, rad/Jyear)
*     PXH       d      parallax (arcsec)
*     RVH       d      radial velocity (km/s, positive = receding)
*
*  Notes:
*
*  1) This routine transforms FK5 star positions and proper motions into
*     the system of the Hipparcos catalog.
*
*  2) The proper motions in RA are dRA/dt rather than cos(Dec)*dRA/dt,
*     and are per year rather than per century.
*
*  3) The FK5 to Hipparcos transformation is modeled as a pure rotation
*     and spin;  zonal errors in the FK5 catalog are not taken into
*     account.
*
*  4) See also iau_H2FK5, iau_FK5HZ, iau_HFK5Z.
*
*  Called:
*     iau_STARPV   star catalog data to space motion pv-vector
*     iau_FK5HIP   FK5 to Hipparcos rotation and spin
*     iau_RXP      product of r-matrix and p-vector
*     iau_PXP      vector product of two p-vectors
*     iau_PPP      p-vector plus p-vector
*     iau_PVSTAR   space motion pv-vector to star catalog data
*
*  Reference:
*
*     F.Mignard & M.Froeschle, Astron. Astrophys. 354, 732-739 (2000).
*
*  This revision:  2009 March 16
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION R5, D5, DR5, DD5, PX5, RV5,
     :                 RH, DH, DRH, DDH, PXH, RVH

      DOUBLE PRECISION PV5(3,2), R5H(3,3), S5H(3), WXP(3), VV(3),
     :                 PVH(3,2)
      INTEGER J, I

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  FK5 barycentric position/velocity pv-vector (normalized).
      CALL iau_STARPV ( R5, D5, DR5, DD5, PX5, RV5, PV5, J )

*  FK5 to Hipparcos orientation matrix and spin vector.
      CALL iau_FK5HIP ( R5H, S5H )

*  Make spin units per day instead of per year.
      DO 1 I=1,3
         S5H(I) = S5H(I) / 365.25D0
 1    CONTINUE

*  Orient the FK5 position into the Hipparcos system.
      CALL iau_RXP ( R5H, PV5(1,1), PVH(1,1) )

*  Apply spin to the position giving an extra space motion component.
      CALL iau_PXP ( PV5(1,1), S5H, WXP )

*  Add this component to the FK5 space motion.
      CALL iau_PPP ( WXP, PV5(1,2), VV )

*  Orient the FK5 space motion into the Hipparcos system.
      CALL iau_RXP ( R5H, VV, PVH(1,2) )

*  Hipparcos pv-vector to spherical.
      CALL iau_PVSTAR ( PVH, RH, DH, DRH, DDH, PXH, RVH, J )

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
