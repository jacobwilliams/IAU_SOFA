      SUBROUTINE iau_SEPS ( AL, AP, BL, BP, S )
*+
*  - - - - - - - - -
*   i a u _ S E P S
*  - - - - - - - - -
*
*  Angular separation between two sets of spherical coordinates.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  vector/matrix support routine.
*
*  Given:
*     AL       d         first longitude (radians)
*     AP       d         first latitude (radians)
*     BL       d         second longitude (radians)
*     BP       d         second latitude (radians)
*
*  Returned:
*     S        d         angular separation (radians)
*
*  Called:
*     iau_S2C      spherical coordinates to unit vector
*     iau_SEPP     angular separation between two p-vectors
*
*  This revision:  2006 November 13
*
*  SOFA release 2009-12-31
*
*  Copyright (C) 2009 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION AL, AP, BL, BP, S

      DOUBLE PRECISION AC(3), BC(3)

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Spherical to Cartesian.
      CALL iau_S2C ( AL, AP, AC )
      CALL iau_S2C ( BL, BP, BC )

*  Angle between the vectors.
      CALL iau_SEPP ( AC, BC, S )

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2009
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
*  1. The Software is owned by the IAU SOFA Review Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The name(s) of all routine(s) in your derived work shall not
*        include the prefix "iau_".
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. In any published work or commercial products which includes
*     results achieved by using the SOFA software, you shall
*     acknowledge that the SOFA software was used in obtaining those
*     results.
*
*  5. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  6. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  7. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@rl.ac.uk
*      By post:   IAU SOFA Center
*                 STFC Rutherford Appleton Laboratory
*                 Harwell Science and Innovation Campus
*                 Didcot, Oxfordshire, OX11 0QX
*                 United Kingdom
*
*----------------------------------------------------------------------

      END
