      DOUBLE PRECISION FUNCTION iau_EORS ( RNPB, S )
*+
*  - - - - - - - - -
*   i a u _ E O R S
*  - - - - - - - - -
*
*  Equation of the origins, given the classical NPB matrix and the
*  quantity s.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     RNPB    d(3,3)    classical nutation x precession x bias matrix
*     S         d       the quantity s (the CIO locator)
*
*  Returned:
*     iau_EORS  d       the equation of the origins in radians.
*
*  Notes:
*
*  1)  The equation of the origins is the distance between the true
*      equinox and the celestial intermediate origin and, equivalently,
*      the difference between Earth rotation angle and Greenwich
*      apparent sidereal time (ERA-GST).  It comprises the precession
*      (since J2000.0) in right ascension plus the equation of the
*      equinoxes (including the small correction terms).
*
*  2)  The algorithm is from Wallace & Capitaine (2006).
*
*  References:
*
*     Capitaine, N. & Wallace, P.T., 2006, Astron.Astrophys. 450, 855
*
*     Wallace, P. & Capitaine, N., 2006, Astron.Astrophys. 459, 981
*
*  This revision:  2008 February 24
*
*  Copyright (C) 2008 IAU SOFA Review Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION RNPB(3,3), S

      DOUBLE PRECISION X, AX, XS, YS, ZS, P, Q

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

*  Evaluate Wallace & Capitaine (2006) expression (16).
      X = RNPB(3,1)
      AX = X / ( 1D0 + RNPB(3,3) )
      XS = 1D0 - AX*X
      YS = -AX*RNPB(3,2)
      ZS = -X
      P = RNPB(1,1)*XS + RNPB(1,2)*YS + RNPB(1,3)*ZS
      Q = RNPB(2,1)*XS + RNPB(2,2)*YS + RNPB(2,3)*ZS
      IF ( P.NE.0D0 .OR. Q.NE.0D0 ) THEN
         iau_EORS = S - ATAN2 ( Q, P )
      ELSE
         iau_EORS = S
      END IF

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
