      PROGRAM T_SOFA_F
*+
*  - - - - - - - - -
*   t _ s o f a _ f
*  - - - - - - - - -
*
*  Validate the SOFA subroutines and functions.
*
*  Each SOFA routine is at least called and a usually quite basic test
*  is performed.  Successful completion is signalled by a confirming
*  message.  Failure of a given function or group of functions results
*  in error messages.
*
*  All messages go to standard output.
*
*  This revision:  2010 September 7
*
*  SOFA release 2010-12-01
*
*  Copyright (C) 2010 IAU SOFA Board.  See notes at end.
*
*----------------------------------------------------------------------

      IMPLICIT NONE

      LOGICAL STATUS
      INTEGER JESTAT


*  Preset the status to success.
      STATUS = .TRUE.

*  Test all of the SOFA routines and functions.
      CALL T_iau_A2AF ( STATUS )
      CALL T_iau_A2TF ( STATUS )
      CALL T_iau_AF2A ( STATUS )
      CALL T_iau_ANP ( STATUS )
      CALL T_iau_ANPM ( STATUS )
      CALL T_iau_BI00 ( STATUS )
      CALL T_iau_BP00 ( STATUS )
      CALL T_iau_BP06 ( STATUS )
      CALL T_iau_BPN2XY ( STATUS )
      CALL T_iau_C2I00A ( STATUS )
      CALL T_iau_C2I00B ( STATUS )
      CALL T_iau_C2I06A ( STATUS )
      CALL T_iau_C2IBPN ( STATUS )
      CALL T_iau_C2IXY ( STATUS )
      CALL T_iau_C2IXYS ( STATUS )
      CALL T_iau_C2S ( STATUS )
      CALL T_iau_C2T00A ( STATUS )
      CALL T_iau_C2T00B ( STATUS )
      CALL T_iau_C2T06A ( STATUS )
      CALL T_iau_C2TCIO ( STATUS )
      CALL T_iau_C2TEQX ( STATUS )
      CALL T_iau_C2TPE ( STATUS )
      CALL T_iau_C2TXY ( STATUS )
      CALL T_iau_CAL2JD ( STATUS )
      CALL T_iau_CP ( STATUS )
      CALL T_iau_CPV ( STATUS )
      CALL T_iau_CR ( STATUS )
      CALL T_iau_D2DTF ( STATUS )
      CALL T_iau_D2TF ( STATUS )
      CALL T_iau_DAT ( STATUS )
      CALL T_iau_DTDB ( STATUS )
      CALL T_iau_DTF2D ( STATUS )
      CALL T_iau_EE00 ( STATUS )
      CALL T_iau_EE00A ( STATUS )
      CALL T_iau_EE00B ( STATUS )
      CALL T_iau_EE06A ( STATUS )
      CALL T_iau_EECT00 ( STATUS )
      CALL T_iau_EFORM ( STATUS )
      CALL T_iau_EO06A ( STATUS )
      CALL T_iau_EORS ( STATUS )
      CALL T_iau_EPB ( STATUS )
      CALL T_iau_EPB2JD ( STATUS )
      CALL T_iau_EPJ ( STATUS )
      CALL T_iau_EPJ2JD ( STATUS )
      CALL T_iau_EPV00 ( STATUS )
      CALL T_iau_EQEQ94 ( STATUS )
      CALL T_iau_ERA00 ( STATUS )
      CALL T_iau_FAD03 ( STATUS )
      CALL T_iau_FAE03 ( STATUS )
      CALL T_iau_FAF03 ( STATUS )
      CALL T_iau_FAJU03 ( STATUS )
      CALL T_iau_FAL03 ( STATUS )
      CALL T_iau_FALP03 ( STATUS )
      CALL T_iau_FAMA03 ( STATUS )
      CALL T_iau_FAME03 ( STATUS )
      CALL T_iau_FANE03 ( STATUS )
      CALL T_iau_FAOM03 ( STATUS )
      CALL T_iau_FAPA03 ( STATUS )
      CALL T_iau_FASA03 ( STATUS )
      CALL T_iau_FAUR03 ( STATUS )
      CALL T_iau_FAVE03 ( STATUS )
      CALL T_iau_FK52H ( STATUS )
      CALL T_iau_FK5HIP ( STATUS )
      CALL T_iau_FK5HZ ( STATUS )
      CALL T_iau_FW2M ( STATUS )
      CALL T_iau_FW2XY ( STATUS )
      CALL T_iau_GC2GD ( STATUS )
      CALL T_iau_GC2GDE ( STATUS )
      CALL T_iau_GD2GC ( STATUS )
      CALL T_iau_GD2GCE ( STATUS )
      CALL T_iau_GMST00 ( STATUS )
      CALL T_iau_GMST06 ( STATUS )
      CALL T_iau_GMST82 ( STATUS )
      CALL T_iau_GST00A ( STATUS )
      CALL T_iau_GST00B ( STATUS )
      CALL T_iau_GST06 ( STATUS )
      CALL T_iau_GST06A ( STATUS )
      CALL T_iau_GST94 ( STATUS )
      CALL T_iau_H2FK5 ( STATUS )
      CALL T_iau_HFK5Z ( STATUS )
      CALL T_iau_IR ( STATUS )
      CALL T_iau_JD2CAL ( STATUS )
      CALL T_iau_JDCALF ( STATUS )
      CALL T_iau_NUM00A ( STATUS )
      CALL T_iau_NUM00B ( STATUS )
      CALL T_iau_NUM06A ( STATUS )
      CALL T_iau_NUMAT ( STATUS )
      CALL T_iau_NUT00A ( STATUS )
      CALL T_iau_NUT00B (STATUS )
      CALL T_iau_NUT06A ( STATUS )
      CALL T_iau_NUT80 ( STATUS )
      CALL T_iau_NUTM80 ( STATUS )
      CALL T_iau_OBL06 ( STATUS )
      CALL T_iau_OBL80 ( STATUS )
      CALL T_iau_P06E ( STATUS )
      CALL T_iau_P2PV ( STATUS )
      CALL T_iau_P2S ( STATUS )
      CALL T_iau_PAP ( STATUS )
      CALL T_iau_PAS ( STATUS )
      CALL T_iau_PB06 ( STATUS )
      CALL T_iau_PDP ( STATUS )
      CALL T_iau_PFW06 ( STATUS )
      CALL T_iau_PLAN94 ( STATUS )
      CALL T_iau_PMAT00 ( STATUS )
      CALL T_iau_PMAT06 ( STATUS )
      CALL T_iau_PMAT76 ( STATUS )
      CALL T_iau_PM ( STATUS )
      CALL T_iau_PMP ( STATUS )
      CALL T_iau_PN ( STATUS )
      CALL T_iau_PN00 ( STATUS )
      CALL T_iau_PN00A  ( STATUS )
      CALL T_iau_PN00B ( STATUS )
      CALL T_iau_PN06A ( STATUS )
      CALL T_iau_PN06 ( STATUS )
      CALL T_iau_PNM00A ( STATUS )
      CALL T_iau_PNM00B ( STATUS )
      CALL T_iau_PNM06A ( STATUS )
      CALL T_iau_PNM80 ( STATUS )
      CALL T_iau_POM00 ( STATUS )
      CALL T_iau_PPP ( STATUS )
      CALL T_iau_PPSP ( STATUS )
      CALL T_iau_PR00 ( STATUS )
      CALL T_iau_PREC76 ( STATUS )
      CALL T_iau_PV2P ( STATUS )
      CALL T_iau_PV2S ( STATUS )
      CALL T_iau_PVDPV ( STATUS )
      CALL T_iau_PVM ( STATUS )
      CALL T_iau_PVMPV ( STATUS )
      CALL T_iau_PVPPV ( STATUS )
      CALL T_iau_PVSTAR ( STATUS )
      CALL T_iau_PVU ( STATUS )
      CALL T_iau_PVUP ( STATUS )
      CALL T_iau_PVXPV ( STATUS )
      CALL T_iau_PXP ( STATUS )
      CALL T_iau_RM2V ( STATUS )
      CALL T_iau_RV2M ( STATUS )
      CALL T_iau_RX ( STATUS )
      CALL T_iau_RXP ( STATUS )
      CALL T_iau_RXPV ( STATUS )
      CALL T_iau_RXR ( STATUS )
      CALL T_iau_RY ( STATUS )
      CALL T_iau_RZ ( STATUS )
      CALL T_iau_S00A ( STATUS )
      CALL T_iau_S00B ( STATUS )
      CALL T_iau_S00 ( STATUS )
      CALL T_iau_S06A ( STATUS )
      CALL T_iau_S06 ( STATUS )
      CALL T_iau_S2C ( STATUS )
      CALL T_iau_S2P ( STATUS )
      CALL T_iau_S2PV ( STATUS )
      CALL T_iau_S2XPV ( STATUS )
      CALL T_iau_SEPP ( STATUS )
      CALL T_iau_SEPS ( STATUS )
      CALL T_iau_SP00 ( STATUS )
      CALL T_iau_STARPM ( STATUS )
      CALL T_iau_STARPV ( STATUS )
      CALL T_iau_SXP ( STATUS )
      CALL T_iau_SXPV ( STATUS )
      CALL T_iau_TAITT ( STATUS )
      CALL T_iau_TAIUT1 ( STATUS )
      CALL T_iau_TAIUTC ( STATUS )
      CALL T_iau_TCBTDB ( STATUS )
      CALL T_iau_TCGTT ( STATUS )
      CALL T_iau_TDBTCB ( STATUS )
      CALL T_iau_TDBTT ( STATUS )
      CALL T_iau_TF2A ( STATUS )
      CALL T_iau_TF2D ( STATUS )
      CALL T_iau_TR ( STATUS )
      CALL T_iau_TRXP ( STATUS )
      CALL T_iau_TRXPV ( STATUS )
      CALL T_iau_TTTAI ( STATUS )
      CALL T_iau_TTTCG ( STATUS )
      CALL T_iau_TTTDB ( STATUS )
      CALL T_iau_TTUT1 ( STATUS )
      CALL T_iau_UT1TAI ( STATUS )
      CALL T_iau_UT1TT ( STATUS )
      CALL T_iau_UT1UTC ( STATUS )
      CALL T_iau_UTCTAI ( STATUS )
      CALL T_iau_UTCUT1 ( STATUS )
      CALL T_iau_XY06 ( STATUS )
      CALL T_iau_XYS00A ( STATUS )
      CALL T_iau_XYS00B ( STATUS )
      CALL T_iau_XYS06A ( STATUS )
      CALL T_iau_ZP ( STATUS )
      CALL T_iau_ZPV ( STATUS )
      CALL T_iau_ZR ( STATUS )

*  Report any errors and set up an appropriate exit status:  0 on
*  success, 1 on any error -- Unix-style.  The EXIT intrinsic is
*  non-standard but common (which is portable enough for a
*  regression test).

      IF ( STATUS ) THEN
         WRITE (*,'(1X,''T_SOFA_F validation successful'')')
         JESTAT = 0
      ELSE
         WRITE (*,'(1X,''T_SOFA_F validation failed!'')')
         JESTAT = 1
      END IF

      CALL EXIT(JESTAT)

      END

      SUBROUTINE VIV ( IVAL, IVALOK, FUNC, TEST, STATUS )
*+
*  - - - -
*   V I V
*  - - - -
*
*  Validate an integer result.
*
*  Internal routine used by T_SOFA_F program.
*
*  Given:
*     IVAL     INTEGER      value computed by routine under test
*     IVALOK   INTEGER      correct value
*     FUNC     CHARACTER    name of routine under test
*     TEST     CHARACTER    name of individual test (or ' ')
*
*  Given and returned:
*     STATUS   LOGICAL      set to .FALSE. if test fails
*
*  Called:  ERR
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      INTEGER IVAL, IVALOK
      CHARACTER*(*) FUNC, TEST
      LOGICAL STATUS


      IF ( IVAL .NE. IVALOK ) THEN
         CALL ERR ( FUNC, TEST, STATUS )
         WRITE (*,'(1X,''  expected ='',I10)') IVALOK
         WRITE (*,'(1X,''  actual =  '',I10)') IVAL
      END IF

      END

      SUBROUTINE VVD ( VAL, VALOK, DVAL, FUNC, TEST, STATUS )
*+
*  - - - -
*   V V D
*  - - - -
*
*  Validate a double result.
*
*  Internal routine used by T_SOFA_F program.
*
*  Given:
*     VAL      DOUBLE       value computed by routine under test
*     VALOK    DOUBLE       correct value
*     DVAL     DOUBLE       maximum allowable error
*     FUNC     CHARACTER    name of routine under test
*     TEST     CHARACTER    name of individual test (or ' ')
*
*  Given and returned:
*     STATUS   LOGICAL      set to .FALSE. if test fails
*
*  Called:  ERR
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      DOUBLE PRECISION VAL, VALOK, DVAL
      CHARACTER*(*) FUNC, TEST
      LOGICAL STATUS


      IF ( DABS ( VAL - VALOK ) .GT. DVAL ) THEN
         CALL ERR ( FUNC, TEST, STATUS )
         WRITE (*,'(1X,''  expected ='',G30.19)') VALOK
         WRITE (*,'(1X,''  actual =  '',G30.19)') VAL
      END IF

      END

      SUBROUTINE ERR ( FUNC, TEST, STATUS )
*+
*  - - - -
*   E R R
*  - - - -
*
*  Report a failed test.
*
*  Internal routine used by T_SOFA_F program.
*
*  Given:
*     FUNC     CHARACTER    name of routine under test
*     TEST     CHARACTER    name of individual test (or ' ')
*
*  Given and returned:
*     STATUS   LOGICAL      set to .FALSE.
*
*  This revision:  2008 November 29
*-
      IMPLICIT NONE

      CHARACTER*(*) FUNC, TEST
      LOGICAL STATUS


      WRITE (*,'(1X,A,'' test '',A,'' fails:'')') FUNC, TEST
      STATUS = .FALSE.

      END

      SUBROUTINE T_iau_A2AF ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ A 2 A F
*  - - - - - - - - - - -
*
*  Test iau_A2AF routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_A2AF, VIV
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IDMSF(4)
      CHARACTER S


      CALL iau_A2AF ( 4, 2.345D0, S, IDMSF )

      CALL VIV ( ICHAR( S ), ICHAR( '+' ), 'iau_A2AF', 'S', STATUS )
      CALL VIV ( IDMSF(1),  134, 'iau_A2AF', '1', STATUS )
      CALL VIV ( IDMSF(2),   21, 'iau_A2AF', '2', STATUS )
      CALL VIV ( IDMSF(3),   30, 'iau_A2AF', '3', STATUS )
      CALL VIV ( IDMSF(4), 9706, 'iau_A2AF', '4', STATUS )

      END

      SUBROUTINE T_iau_A2TF ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ A 2 T F
*  - - - - - - - - - - -
*
*  Test iau_A2TF routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_A2TF, VIV
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IHMSF(4)
      CHARACTER S


      CALL iau_A2TF ( 4, -3.01234D0, S, IHMSF )

      CALL VIV ( ICHAR( S ), ICHAR( '-' ), 'iau_A2TF', 'S', STATUS )
      CALL VIV ( IHMSF(1),   11, 'iau_A2TF', '1', STATUS )
      CALL VIV ( IHMSF(2),   30, 'iau_A2TF', '2', STATUS )
      CALL VIV ( IHMSF(3),   22, 'iau_A2TF', '3', STATUS )
      CALL VIV ( IHMSF(4), 6484, 'iau_A2TF', '4', STATUS )

      END

      SUBROUTINE T_iau_AF2A ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ A F 2 A
*  - - - - - - - - - - -
*
*  Test iau_AF2A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_AF2A, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A
      INTEGER J


      CALL iau_AF2A ( '-', 45, 13, 27.2D0, A, J )

      CALL VVD ( A, -0.7893115794313644842D0, 1D-12,
     :           'iau_AF2A', 'A', STATUS )
      CALL VIV ( J, 0, 'iau_AF2A', 'J', STATUS )

      END

      SUBROUTINE T_iau_ANP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ A N P
*  - - - - - - - - - -
*
*  Test iau_ANP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_ANP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_ANP


      CALL VVD ( iau_ANP ( -0.1D0 ), 6.183185307179586477D0, 1D-12,
     :           'iau_ANP', ' ', STATUS )

      END

      SUBROUTINE T_iau_ANPM ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ A N P M
*  - - - - - - - - - - -
*
*  Test iau_ANPM routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_ANPM, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_ANPM


      CALL VVD ( iau_ANPM ( -4D0 ), 2.283185307179586477D0, 1D-12,
     :           'iau_ANPM', ' ', STATUS )

      END

      SUBROUTINE T_iau_BI00 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ B I 0 0
*  - - - - - - - - - - -
*
*  Test iau_BI00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_BI00, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSIBI, DEPSBI, DRA


      CALL iau_BI00 ( DPSIBI, DEPSBI, DRA )

      CALL VVD ( DPSIBI, -0.2025309152835086613D-6, 1D-12,
     :           'iau_BI00', 'DPSIBI', STATUS )
      CALL VVD ( DEPSBI, -0.3306041454222147847D-7, 1D-12,
     :           'iau_BI00', 'DEPSBI', STATUS )
      CALL VVD ( DRA, -0.7078279744199225506D-7, 1D-12,
     :           'iau_BI00', 'DRA', STATUS )

      END

      SUBROUTINE T_iau_BP00 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ B P 0 0
*  - - - - - - - - - - -
*
*  Test iau_BP00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_BP00, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RB(3,3), RP(3,3), RBP(3,3)


      CALL iau_BP00 ( 2400000.5D0, 50123.9999D0, RB, RP, RBP )

      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,
     :           'iau_BP00', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-16,
     :           'iau_BP00', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-16,
     :           'iau_BP00', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-16,
     :           'iau_BP00', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_BP00', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-16,
     :           'iau_BP00', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-16,
     :           'iau_BP00', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-16,
     :           'iau_BP00', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_BP00', 'RB33', STATUS )

      CALL VVD ( RP(1,1), 0.9999995504864048241D0, 1D-12,
     :           'iau_BP00', 'RP11', STATUS )
      CALL VVD ( RP(1,2), 0.8696113836207084411D-3, 1D-14,
     :           'iau_BP00', 'RP12', STATUS )
      CALL VVD ( RP(1,3), 0.3778928813389333402D-3, 1D-14,
     :           'iau_BP00', 'RP13', STATUS )
      CALL VVD ( RP(2,1), -0.8696113818227265968D-3, 1D-14,
     :           'iau_BP00', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999996218879365258D0, 1D-12,
     :           'iau_BP00', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.1690679263009242066D-6, 1D-14,
     :           'iau_BP00', 'RP23', STATUS )
      CALL VVD ( RP(3,1), -0.3778928854764695214D-3, 1D-14,
     :           'iau_BP00', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.1595521004195286491D-6, 1D-14,
     :           'iau_BP00', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999999285984682756D0, 1D-12,
     :           'iau_BP00', 'RP33', STATUS )

      CALL VVD ( RBP(1,1), 0.9999995505175087260D0, 1D-12,
     :           'iau_BP00', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695405883617884705D-3, 1D-14,
     :           'iau_BP00', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779734722239007105D-3, 1D-14,
     :           'iau_BP00', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695405990410863719D-3, 1D-14,
     :           'iau_BP00', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219494925900D0, 1D-12,
     :           'iau_BP00', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.1360775820404982209D-6, 1D-14,
     :           'iau_BP00', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734476558184991D-3, 1D-14,
     :           'iau_BP00', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.1925857585832024058D-6, 1D-14,
     :           'iau_BP00', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285680153377D0, 1D-12,
     :           'iau_BP00', 'RBP33', STATUS )

      END

      SUBROUTINE T_iau_BP06 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ B P 0 6
*  - - - - - - - - - - -
*
*  Test iau_BP06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_BP06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RB(3,3), RP(3,3), RBP(3,3)


      CALL iau_BP06 ( 2400000.5D0, 50123.9999D0, RB, RP, RBP )

      CALL VVD ( RB(1,1), 0.9999999999999942497D0, 1D-12,
     :           'iau_BP06', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078368960971557145D-7, 1D-14,
     :           'iau_BP06', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056213977613185606D-7, 1D-14,
     :           'iau_BP06', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078368694637674333D-7, 1D-14,
     :           'iau_BP06', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_BP06', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3305943742989134124D-7, 1D-14,
     :           'iau_BP06', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056214211620056792D-7, 1D-14,
     :           'iau_BP06', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3305943172740586950D-7, 1D-14,
     :           'iau_BP06', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_BP06', 'RB33', STATUS )

      CALL VVD ( RP(1,1), 0.9999995504864960278D0, 1D-12,
     :           'iau_BP06', 'RP11', STATUS )
      CALL VVD ( RP(1,2), 0.8696112578855404832D-3, 1D-14,
     :           'iau_BP06', 'RP12', STATUS )
      CALL VVD ( RP(1,3), 0.3778929293341390127D-3, 1D-14,
     :           'iau_BP06', 'RP13', STATUS )
      CALL VVD ( RP(2,1), -0.8696112560510186244D-3, 1D-14,
     :           'iau_BP06', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999996218880458820D0, 1D-12,
     :           'iau_BP06', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.1691646168941896285D-6, 1D-14,
     :           'iau_BP06', 'RP23', STATUS )
      CALL VVD ( RP(3,1), -0.3778929335557603418D-3, 1D-14,
     :           'iau_BP06', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.1594554040786495076D-6, 1D-14,
     :           'iau_BP06', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999999285984501222D0, 1D-12,
     :           'iau_BP06', 'RP33', STATUS )

      CALL VVD ( RBP(1,1), 0.9999995505176007047D0, 1D-12,
     :           'iau_BP06', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695404617348208406D-3, 1D-14,
     :           'iau_BP06', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779735201865589104D-3, 1D-14,
     :           'iau_BP06', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695404723772031414D-3, 1D-14,
     :           'iau_BP06', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219496027161D0, 1D-12,
     :           'iau_BP06', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.1361752497080270143D-6, 1D-14,
     :           'iau_BP06', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734957034089490D-3, 1D-14,
     :           'iau_BP06', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.1924880847894457113D-6, 1D-14,
     :           'iau_BP06', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285679971958D0, 1D-12,
     :           'iau_BP06', 'RBP33', STATUS )

      END

      SUBROUTINE T_iau_BPN2XY ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ B P N 2 X Y
*  - - - - - - - - - - - - -
*
*  Test iau_BPN2XY routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_BPN2XY, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3), X, Y


      RBPN(1,1) = 9.999962358680738D-1
      RBPN(1,2) = -2.516417057665452D-3
      RBPN(1,3) = -1.093569785342370D-3

      RBPN(2,1) = 2.516462370370876D-3
      RBPN(2,2) = 9.999968329010883D-1
      RBPN(2,3) = 4.006159587358310D-5

      RBPN(3,1) = 1.093465510215479D-3
      RBPN(3,2) = -4.281337229063151D-5
      RBPN(3,3) = 9.999994012499173D-1

      CALL iau_BPN2XY ( RBPN, X, Y )

      CALL VVD ( X, 1.093465510215479D-3, 1D-12,
     :           'iau_BPN2XY', 'X', STATUS )
      CALL VVD ( Y, -4.281337229063151D-5, 1D-12,
     :           'iau_BPN2XY', 'Y', STATUS )

      END

      SUBROUTINE T_iau_C2I00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 I 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_C2I00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2I00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3)


      CALL iau_C2I00A ( 2400000.5D0, 53736D0, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037165557D0, 1D-12,
     :           'iau_C2I00A', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581526348992140183D-9, 1D-12,
     :           'iau_C2I00A', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308477073443415D-3, 1D-12,
     :           'iau_C2I00A', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384266227870752452D-7, 1D-12,
     :           'iau_C2I00A', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917405258D0, 1D-12,
     :           'iau_C2I00A', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020594955028209745D-4, 1D-12,
     :           'iau_C2I00A', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308472168152904D-3, 1D-12,
     :           'iau_C2I00A', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020595661591500259D-4, 1D-12,
     :           'iau_C2I00A', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954572304D0, 1D-12,
     :           'iau_C2I00A', '33', STATUS )

      END

      SUBROUTINE T_iau_C2I00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 I 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_C2I00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2I00B, VVD
*
*  This revision:  2008 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3)


      CALL iau_C2I00B ( 2400000.5D0, 53736D0, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323040954356D0, 1D-12,
     :           'iau_C2I00B', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581526349131823372D-9, 1D-12,
     :           'iau_C2I00B', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791301934855394005D-3, 1D-12,
     :           'iau_C2I00B', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384239285499175543D-7, 1D-12,
     :           'iau_C2I00B', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917574043D0, 1D-12,
     :           'iau_C2I00B', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020552974819030066D-4, 1D-12,
     :           'iau_C2I00B', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791301929950208873D-3, 1D-12,
     :           'iau_C2I00B', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020553681373720832D-4, 1D-12,
     :           'iau_C2I00B', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314958529887D0, 1D-12,
     :           'iau_C2I00B', '33', STATUS )

      END

      SUBROUTINE T_iau_C2I06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 I 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_C2I06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2I06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3)


      CALL iau_C2I06A ( 2400000.5D0, 53736D0, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037159379D0, 1D-12,
     :           'iau_C2I06A', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581121329587613787D-9, 1D-12,
     :           'iau_C2I06A', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308487740529749D-3, 1D-12,
     :           'iau_C2I06A', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384253169452306581D-7, 1D-12,
     :           'iau_C2I06A', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917467827D0, 1D-12,
     :           'iau_C2I06A', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020579392895682558D-4, 1D-12,
     :           'iau_C2I06A', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308482835292617D-3, 1D-12,
     :           'iau_C2I06A', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020580099454020310D-4, 1D-12,
     :           'iau_C2I06A', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954628695D0, 1D-12,
     :           'iau_C2I06A', '33', STATUS )

      END

      SUBROUTINE T_iau_C2IBPN ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 I B P N
*  - - - - - - - - - - - - -
*
*  Test iau_C2IBPN routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2IBPN, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3), RC2I(3,3)


      RBPN(1,1) = 9.999962358680738D-1
      RBPN(1,2) = -2.516417057665452D-3
      RBPN(1,3) = -1.093569785342370D-3

      RBPN(2,1) = 2.516462370370876D-3
      RBPN(2,2) = 9.999968329010883D-1
      RBPN(2,3) = 4.006159587358310D-5

      RBPN(3,1) = 1.093465510215479D-3
      RBPN(3,2) = -4.281337229063151D-5
      RBPN(3,3) = 9.999994012499173D-1

      CALL iau_C2IBPN ( 2400000.5D0, 50123.9999D0, RBPN, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999994021664089977D0, 1D-12,
     :           'iau_C2IBPN', '11', STATUS )
      CALL VVD ( RC2I(1,2), -0.3869195948017503664D-8, 1D-12,
     :           'iau_C2IBPN', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.1093465511383285076D-2, 1D-12,
     :           'iau_C2IBPN', '13', STATUS )
      CALL VVD ( RC2I(2,1), 0.5068413965715446111D-7, 1D-12,
     :           'iau_C2IBPN', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999990835075686D0, 1D-12,
     :           'iau_C2IBPN', '22', STATUS )
      CALL VVD ( RC2I(2,3), 0.4281334246452708915D-4, 1D-12,
     :           'iau_C2IBPN', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.1093465510215479000D-2, 1D-12,
     :           'iau_C2IBPN', '31', STATUS )
      CALL VVD ( RC2I(3,2), -0.4281337229063151000D-4, 1D-12,
     :           'iau_C2IBPN', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999994012499173103D0, 1D-12,
     :           'iau_C2IBPN', '33', STATUS )

      END

      SUBROUTINE T_iau_C2IXY ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ C 2 I X Y
*  - - - - - - - - - - - -
*
*  Test iau_C2IXY routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2IXY, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, RC2I(3,3)


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      CALL iau_C2IXY ( 2400000.5D0, 53736D0, X, Y, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037157138D0, 1D-12,
     :           'iau_C2IXY', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581526349032241205D-9, 1D-12,
     :           'iau_C2IXY', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308491611263745D-3, 1D-12,
     :           'iau_C2IXY', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384257057469842953D-7, 1D-12,
     :           'iau_C2IXY', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917468964D0, 1D-12,
     :           'iau_C2IXY', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020579110172324363D-4, 1D-12,
     :           'iau_C2IXY', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308486706011000D-3, 1D-12,
     :           'iau_C2IXY', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020579816732961219D-4, 1D-12,
     :           'iau_C2IXY', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954627590D0, 1D-12,
     :           'iau_C2IXY', '33', STATUS )

      END

      SUBROUTINE T_iau_C2IXYS ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 I X Y S
*  - - - - - - - - - - - - -
*
*  Test iau_C2IXYS routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2IXYS, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, S, RC2I(3,3)


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4
      S = -0.1220040848472271978D-7

      CALL iau_C2IXYS ( X, Y, S, RC2I )

      CALL VVD ( RC2I(1,1), 0.9999998323037157138D0, 1D-12,
     :           'iau_C2IXYS', '11', STATUS )
      CALL VVD ( RC2I(1,2), 0.5581984869168499149D-9, 1D-12,
     :           'iau_C2IXYS', '12', STATUS )
      CALL VVD ( RC2I(1,3), -0.5791308491611282180D-3, 1D-12,
     :           'iau_C2IXYS', '13', STATUS )
      CALL VVD ( RC2I(2,1), -0.2384261642670440317D-7, 1D-12,
     :           'iau_C2IXYS', '21', STATUS )
      CALL VVD ( RC2I(2,2), 0.9999999991917468964D0, 1D-12,
     :           'iau_C2IXYS', '22', STATUS )
      CALL VVD ( RC2I(2,3), -0.4020579110169668931D-4, 1D-12,
     :           'iau_C2IXYS', '23', STATUS )
      CALL VVD ( RC2I(3,1), 0.5791308486706011000D-3, 1D-12,
     :           'iau_C2IXYS', '31', STATUS )
      CALL VVD ( RC2I(3,2), 0.4020579816732961219D-4, 1D-12,
     :           'iau_C2IXYS', '32', STATUS )
      CALL VVD ( RC2I(3,3), 0.9999998314954627590D0, 1D-12,
     :           'iau_C2IXYS', '33', STATUS )

      END

      SUBROUTINE T_iau_C2S ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ C 2 S
*  - - - - - - - - - -
*
*  Test iau_C2S routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2S, VVD
*
*  This revision:   2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), THETA, PHI


      P(1) = 100D0
      P(2) = -50D0
      P(3) = 25D0

      CALL iau_C2S ( P, THETA, PHI )

      CALL VVD ( THETA, -0.4636476090008061162D0, 1D-14,
     :           'iau_C2S', 'THETA', STATUS )
      CALL VVD ( PHI, 0.2199879773954594463D0, 1D-14,
     :           'iau_C2S', 'PHI', STATUS )

      END

      SUBROUTINE T_iau_C2T00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 T 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_C2T00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2T00A, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL iau_C2T00A ( TTA, TTB, UTA, UTB, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128307182668D0, 1D-12,
     :           'iau_C2T00A', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938457836D0, 1D-12,
     :           'iau_C2T00A', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555535638688341725D-4, 1D-12,
     :           'iau_C2T00A', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134135984552D0, 1D-12,
     :           'iau_C2T00A', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649520727D0, 1D-12,
     :           'iau_C2T00A', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749801116141056317D-3, 1D-12,
     :           'iau_C2T00A', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474014081406921D-3, 1D-12,
     :           'iau_C2T00A', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961832391770163647D-4, 1D-12,
     :           'iau_C2T00A', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501692289D0, 1D-12,
     :           'iau_C2T00A', '33', STATUS )

      END

      SUBROUTINE T_iau_C2T00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 T 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_C2T00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2T00B, VVD
*
*  This revision:  2008 November 29
*-
      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL iau_C2T00B ( TTA, TTB, UTA, UTB, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128439678965D0, 1D-12,
     :           'iau_C2T00B', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806913872359D0, 1D-12,
     :           'iau_C2T00B', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555565082458415611D-4, 1D-12,
     :           'iau_C2T00B', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134115435923D0, 1D-12,
     :           'iau_C2T00B', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203784001946D0, 1D-12,
     :           'iau_C2T00B', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749793922030017230D-3, 1D-12,
     :           'iau_C2T00B', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773467471863534901D-3, 1D-12,
     :           'iau_C2T00B', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961790411549945020D-4, 1D-12,
     :           'iau_C2T00B', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325505635738D0, 1D-12,
     :           'iau_C2T00B', '33', STATUS )

      END

      SUBROUTINE T_iau_C2T06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 T 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_C2T06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2T06A, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL iau_C2T06A ( TTA, TTB, UTA, UTB, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128305897282D0, 1D-12,
     :           'iau_C2T06A', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938592296D0, 1D-12,
     :           'iau_C2T06A', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555550962998436505D-4, 1D-12,
     :           'iau_C2T06A', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134136214897D0, 1D-12,
     :           'iau_C2T06A', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649130832D0, 1D-12,
     :           'iau_C2T06A', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749800844905594110D-3, 1D-12,
     :           'iau_C2T06A', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474024748545878D-3, 1D-12,
     :           'iau_C2T06A', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961816829632690581D-4, 1D-12,
     :           'iau_C2T06A', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501747785D0, 1D-12,
     :           'iau_C2T06A', '33', STATUS )

      END

      SUBROUTINE T_iau_C2TCIO ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 T C I O
*  - - - - - - - - - - - - -
*
*  Test iau_C2TCIO routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2TCIO, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RC2I(3,3), ERA, RPOM(3,3), RC2T(3,3)


      RC2I(1,1) = 0.9999998323037164738D0
      RC2I(1,2) = 0.5581526271714303683D-9
      RC2I(1,3) = -0.5791308477073443903D-3

      RC2I(2,1) = -0.2384266227524722273D-7
      RC2I(2,2) = 0.9999999991917404296D0
      RC2I(2,3) = -0.4020594955030704125D-4

      RC2I(3,1) = 0.5791308472168153320D-3
      RC2I(3,2) = 0.4020595661593994396D-4
      RC2I(3,3) = 0.9999998314954572365D0

      ERA = 1.75283325530307D0

      RPOM(1,1) = 0.9999999999999674705D0
      RPOM(1,2) = -0.1367174580728847031D-10
      RPOM(1,3) = 0.2550602379999972723D-6

      RPOM(2,1) = 0.1414624947957029721D-10
      RPOM(2,2) = 0.9999999999982694954D0
      RPOM(2,3) = -0.1860359246998866338D-5

      RPOM(3,1) = -0.2550602379741215275D-6
      RPOM(3,2) = 0.1860359247002413923D-5
      RPOM(3,3) = 0.9999999999982369658D0

      CALL iau_C2TCIO ( RC2I, ERA, RPOM, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128307110439D0, 1D-12,
     :           'iau_C2TCIO', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938470149D0, 1D-12,
     :           'iau_C2TCIO', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555535638685466874D-4, 1D-12,
     :           'iau_C2TCIO', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134135996657D0, 1D-12,
     :           'iau_C2TCIO', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649448367D0, 1D-12,
     :           'iau_C2TCIO', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749801116141106528D-3, 1D-12,
     :           'iau_C2TCIO', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474014081407076D-3, 1D-12,
     :           'iau_C2TCIO', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961832391772658944D-4, 1D-12,
     :           'iau_C2TCIO', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501691969D0, 1D-12,
     :           'iau_C2TCIO', '33', STATUS )

      END

      SUBROUTINE T_iau_C2TEQX ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C 2 T E Q X
*  - - - - - - - - - - - - -
*
*  Test iau_C2TEQX routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2TEQX, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3), GST, RPOM(3,3), RC2T(3,3)


      RBPN(1,1) = 0.9999989440476103608D0
      RBPN(1,2) = -0.1332881761240011518D-2
      RBPN(1,3) = -0.5790767434730085097D-3

      RBPN(2,1) = 0.1332858254308954453D-2
      RBPN(2,2) = 0.9999991109044505944D0
      RBPN(2,3) = -0.4097782710401555759D-4

      RBPN(3,1) = 0.5791308472168153320D-3
      RBPN(3,2) = 0.4020595661593994396D-4
      RBPN(3,3) = 0.9999998314954572365D0

      GST = 1.754166138040730516D0

      RPOM(1,1) = 0.9999999999999674705D0
      RPOM(1,2) = -0.1367174580728847031D-10
      RPOM(1,3) = 0.2550602379999972723D-6

      RPOM(2,1) = 0.1414624947957029721D-10
      RPOM(2,2) = 0.9999999999982694954D0
      RPOM(2,3) = -0.1860359246998866338D-5

      RPOM(3,1) = -0.2550602379741215275D-6
      RPOM(3,2) = 0.1860359247002413923D-5
      RPOM(3,3) = 0.9999999999982369658D0

      CALL iau_C2TEQX ( RBPN, GST, RPOM, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128528685730D0, 1D-12,
     :           'iau_C2TEQX', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806897685071D0, 1D-12,
     :           'iau_C2TEQX', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555535639982634449D-4, 1D-12,
     :           'iau_C2TEQX', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134095211257D0, 1D-12,
     :           'iau_C2TEQX', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203871023800D0, 1D-12,
     :           'iau_C2TEQX', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749801116126438962D-3, 1D-12,
     :           'iau_C2TEQX', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474014081539467D-3, 1D-12,
     :           'iau_C2TEQX', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961832391768640871D-4, 1D-12,
     :           'iau_C2TEQX', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501691969D0, 1D-12,
     :           'iau_C2TEQX', '33', STATUS )

      END

      SUBROUTINE T_iau_C2TPE ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ C 2 T P E
*  - - - - - - - - - - - -
*
*  Test iau_C2TPE routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2TPE, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      DEPS = 0.4090789763356509900D0
      DPSI = -0.9630909107115582393D-5

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL iau_C2TPE ( TTA, TTB, UTA, UTB, DPSI, DEPS, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1813677995763029394D0, 1D-12,
     :           'iau_C2TPE', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9023482206891683275D0, 1D-12,
     :           'iau_C2TPE', '12', STATUS )
      CALL VVD ( RC2T(1,3), -0.3909902938641085751D0, 1D-12,
     :           'iau_C2TPE', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834147641476804807D0, 1D-12,
     :           'iau_C2TPE', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1659883635434995121D0, 1D-12,
     :           'iau_C2TPE', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.7309763898042819705D-1, 1D-12,
     :           'iau_C2TPE', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.1059685430673215247D-2, 1D-12,
     :           'iau_C2TPE', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3977631855605078674D0, 1D-12,
     :           'iau_C2TPE', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9174875068792735362D0, 1D-12,
     :           'iau_C2TPE', '33', STATUS )

      END

      SUBROUTINE T_iau_C2TXY ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ C 2 T X Y
*  - - - - - - - - - - - -
*
*  Test iau_C2TXY routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_C2TXY, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION TTA, TTB, UTA, UTB, X, Y, XP, YP, RC2T(3,3)


      TTA = 2400000.5D0
      UTA = 2400000.5D0

      TTB = 53736D0
      UTB = 53736D0

      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      XP = 2.55060238D-7
      YP = 1.860359247D-6

      CALL iau_C2TXY ( TTA, TTB, UTA, UTB, X, Y, XP, YP, RC2T )

      CALL VVD ( RC2T(1,1), -0.1810332128306279253D0, 1D-12,
     :           'iau_C2TXY', '11', STATUS )
      CALL VVD ( RC2T(1,2), 0.9834769806938520084D0, 1D-12,
     :           'iau_C2TXY', '12', STATUS )
      CALL VVD ( RC2T(1,3), 0.6555551248057665829D-4, 1D-12,
     :           'iau_C2TXY', '13', STATUS )
      CALL VVD ( RC2T(2,1), -0.9834768134136142314D0, 1D-12,
     :           'iau_C2TXY', '21', STATUS )
      CALL VVD ( RC2T(2,2), -0.1810332203649529312D0, 1D-12,
     :           'iau_C2TXY', '22', STATUS )
      CALL VVD ( RC2T(2,3), 0.5749800843594139912D-3, 1D-12,
     :           'iau_C2TXY', '23', STATUS )
      CALL VVD ( RC2T(3,1), 0.5773474028619264494D-3, 1D-12,
     :           'iau_C2TXY', '31', STATUS )
      CALL VVD ( RC2T(3,2), 0.3961816546911624260D-4, 1D-12,
     :           'iau_C2TXY', '32', STATUS )
      CALL VVD ( RC2T(3,3), 0.9999998325501746670D0, 1D-12,
     :           'iau_C2TXY', '33', STATUS )

      END

      SUBROUTINE T_iau_CAL2JD ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ C A L 2 J D
*  - - - - - - - - - - - - -
*
*  Test iau_CAL2JD routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_CAL2JD, VVD, VIV
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION DJM0, DJM


      CALL iau_CAL2JD ( 2003, 06, 01, DJM0, DJM, J )

      CALL VVD ( DJM0 + DJM, 2452791.5D0, 0D0,
     :           'iau_CAL2JD', 'JD + MJD', STATUS )
      CALL VIV ( J, 0, 'iau_CAL2JD', 'J', STATUS )

      END

      SUBROUTINE T_iau_CP ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ C P
*  - - - - - - - - -
*
*  Test iau_CP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_CP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), C(3)


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL iau_CP ( P, C )

      CALL VVD ( C(1), 0.3D0, 0D0, 'iau_CP', '1', STATUS )
      CALL VVD ( C(2), 1.2D0, 0D0, 'iau_CP', '2', STATUS )
      CALL VVD ( C(3), -2.5D0, 0D0, 'iau_CP', '3', STATUS )

      END

      SUBROUTINE T_iau_CPV ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ C P V
*  - - - - - - - - - -
*
*  Test iau_CPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_CPV, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), C(3,2)


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL iau_CPV ( PV, C )

      CALL VVD ( C(1,1), 0.3D0, 0D0, 'iau_CPV', 'P1', STATUS )
      CALL VVD ( C(2,1), 1.2D0, 0D0, 'iau_CPV', 'P2', STATUS )
      CALL VVD ( C(3,1), -2.5D0, 0D0, 'iau_CPV', 'P3', STATUS )
      CALL VVD ( C(1,2), -0.5D0, 0D0, 'iau_CPV', 'V1', STATUS )
      CALL VVD ( C(2,2), 3.1D0, 0D0, 'iau_CPV', 'V2', STATUS )
      CALL VVD ( C(3,2), 0.9D0, 0D0, 'iau_CPV', 'V3', STATUS )

      END

      SUBROUTINE T_iau_CR ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ C R
*  - - - - - - - - -
*
*  Test iau_CR routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_CR, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), C(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_CR ( R, C )

      CALL VVD ( C(1,1), 2D0, 0D0, 'iau_CR', '11', STATUS )
      CALL VVD ( C(1,2), 3D0, 0D0, 'iau_CR', '12', STATUS )
      CALL VVD ( C(1,3), 2D0, 0D0, 'iau_CR', '13', STATUS )
      CALL VVD ( C(2,1), 3D0, 0D0, 'iau_CR', '21', STATUS )
      CALL VVD ( C(2,2), 2D0, 0D0, 'iau_CR', '22', STATUS )
      CALL VVD ( C(2,3), 3D0, 0D0, 'iau_CR', '23', STATUS )
      CALL VVD ( C(3,1), 3D0, 0D0, 'iau_CR', '31', STATUS )
      CALL VVD ( C(3,2), 4D0, 0D0, 'iau_CR', '32', STATUS )
      CALL VVD ( C(3,3), 5D0, 0D0, 'iau_CR', '33', STATUS )

      END

      SUBROUTINE T_iau_D2DTF ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ D 2 D T F
*  - - - - - - - - - - - -
*
*  Test iau_D2DTF routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_D2DTF, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IY, IM, ID, IHMSF(4), J


      CALL iau_D2DTF ( 'UTC', 5, 2400000.5D0, 49533.99999D0,
     :                 IY, IM, ID, IHMSF, J )

      CALL VIV ( IY, 1994, 'iau_D2DTF', 'Y', STATUS )
      CALL VIV ( IM, 6, 'iau_D2DTF', 'Mo', STATUS )
      CALL VIV ( ID, 30, 'iau_D2DTF', 'D', STATUS )
      CALL VIV ( IHMSF(1), 23, 'iau_D2DTF', 'H', STATUS )
      CALL VIV ( IHMSF(2), 59, 'iau_D2DTF', 'M', STATUS )
      CALL VIV ( IHMSF(3), 60, 'iau_D2DTF', 'S', STATUS )
      CALL VIV ( IHMSF(4), 13599, 'iau_D2DTF', 'F', STATUS )
      CALL VIV ( J, 0, 'iau_D2DTF', 'J', STATUS )

      END

      SUBROUTINE T_iau_D2TF ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ D 2 T F
*  - - - - - - - - - - -
*
*  Test iau_D2TF routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_D2TF, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER IHMSF(4)
      CHARACTER S


      CALL iau_D2TF ( 4, -0.987654321D0, S, IHMSF )

      CALL VIV ( ICHAR( S ), ICHAR( '-' ), 'iau_D2TF', 'S', STATUS )
      CALL VIV ( IHMSF(1), 23, 'iau_D2TF', '1', STATUS )
      CALL VIV ( IHMSF(2), 42, 'iau_D2TF', '2', STATUS )
      CALL VIV ( IHMSF(3), 13, 'iau_D2TF', '3', STATUS )
      CALL VIV ( IHMSF(4), 3333, 'iau_D2TF', '4', STATUS )

      END

      SUBROUTINE T_iau_DAT ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ D A T
*  - - - - - - - - - -
*
*  Test iau_DAT routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_DAT, VVD, VIV
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION DELTAT


      CALL iau_DAT ( 2003, 06, 01, 0D0, DELTAT, J )
      CALL VVD ( DELTAT, 32D0, 0D0, 'iau_DAT', 'DELTAT', STATUS )
      CALL VIV ( J, 0, 'iau_DAT', 'J', STATUS )
      CALL iau_DAT ( 2008, 01, 17, 0D0, DELTAT, J )
      CALL VVD ( DELTAT, 33D0, 0D0, 'iau_DAT', 'DELTAT', STATUS )
      CALL VIV ( J, 0, 'iau_DAT', 'J', STATUS )

      END

      SUBROUTINE T_iau_DTDB ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ D T D B
*  - - - - - - - - - - -
*
*  Test iau_DTDB routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_DTDB, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_DTDB


      CALL VVD ( iau_DTDB ( 2448939.5D0, 0.123D0,
     :                      0.76543D0, 5.0123D0, 5525.242D0, 3190D0 ),
     :           -0.1280368005936998991D-2, 1D-15,
     :           'iau_DTDB', ' ', STATUS )

      END

      SUBROUTINE T_iau_DTF2D ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ D T F 2 D
*  - - - - - - - - - - - -
*
*  Test iau_DTF2D routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_DTF2D, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_DTF2D ( 'UTC', 1994, 6, 30, 23, 59, 60.13599D0,
     :                 U1, U2, J )

      CALL VVD ( U1+U2,
     :           2449534.49999D0, 1D-6, 'iau_DTF2D', 'U', STATUS )
      CALL VIV ( J, 0, 'iau_DTF2D', 'J', STATUS )

      END

      SUBROUTINE T_iau_EE00 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ E E 0 0
*  - - - - - - - - - - -
*
*  Test iau_EE00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EE00, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EE00, EPSA, DPSI


      EPSA = 0.4090789763356509900D0
      DPSI = -0.9630909107115582393D-5

      CALL VVD ( iau_EE00 ( 2400000.5D0, 53736D0, EPSA, DPSI ),
     :           -0.8834193235367965479D-5, 1D-18,
     :           'iau_EE00', ' ', STATUS )

      END

      SUBROUTINE T_iau_EE00A ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ E E 0 0 A
*  - - - - - - - - - - - -
*
*  Test iau_EE00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EE00A, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EE00A


      CALL VVD ( iau_EE00A ( 2400000.5D0, 53736D0 ),
     :           -0.8834192459222588227D-5, 1D-18,
     :           'iau_EE00A', ' ', STATUS )

      END

      SUBROUTINE T_iau_EE00B ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ E E 0 0 B
*  - - - - - - - - - - - -
*
*  Test iau_EE00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EE00B, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EE00B


      CALL VVD ( iau_EE00B ( 2400000.5D0, 53736D0 ),
     :           -0.8835700060003032831D-5, 1D-18,
     :           'iau_EE00B', ' ', STATUS )

      END

      SUBROUTINE T_iau_EE06A ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ E E 0 6 A
*  - - - - - - - - - - - -
*
*  Test iau_EE06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EE06A, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EE06A


      CALL VVD ( iau_EE06A ( 2400000.5D0, 53736D0 ),
     :           -0.8834195072043790156D-5, 1D-15,
     :           'iau_EE06A', ' ', STATUS )

      END

      SUBROUTINE T_iau_EECT00 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ E E C T 0 0
*  - - - - - - - - - - - - -
*
*  Test iau_EECT00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EECT00, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EECT00


      CALL VVD ( iau_EECT00 ( 2400000.5D0, 53736D0 ),
     :           0.2046085004885125264D-8, 1D-20,
     :           'iau_EECT00', ' ', STATUS )

      END

      SUBROUTINE T_iau_EFORM ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ E F O R M
*  - - - - - - - - - - - -
*
*  Test iau_EFORM routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EFORM, VVD, VIV
*
*  This revision:  2010 January 26
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION A, F


      CALL iau_EFORM ( 0, A, F, J )

      CALL VIV ( J, -1, 'iau_EFORM', 'J0', STATUS )

      CALL iau_EFORM ( 1, A, F, J )

      CALL VIV ( J, 0, 'iau_EFORM', 'J1', STATUS )
      CALL VVD ( A, 6378137D0, 1D-10, 'iau_EFORM', 'A1', STATUS )
      CALL VVD ( F, 0.0033528106647474807D0, 1D-18,
     :           'iau_EFORM', 'F1', STATUS )

      CALL iau_EFORM( 2, A, F, J )

      CALL VIV ( J, 0, 'iau_EFORM', 'J2', STATUS )
      CALL VVD ( A, 6378137D0, 1D-10, 'iau_EFORM', 'A2', STATUS )
      CALL VVD ( F, 0.0033528106811823189D0, 1D-18,
     :           'iau_EFORM', 'F2', STATUS )

      CALL iau_EFORM( 3, A, F, J )

      CALL VIV ( J, 0, 'iau_EFORM', 'J3', STATUS )
      CALL VVD ( A, 6378135D0, 1D-10, 'iau_EFORM', 'A3', STATUS )
      CALL VVD ( F, 0.0033527794541675049D0, 1D-18,
     :           'iau_EFORM', 'F3', STATUS )

      CALL iau_EFORM( 4, A, F, J )
      CALL VIV ( J, -1, 'iau_EFORM', 'J4', STATUS )

      END

      SUBROUTINE T_iau_EO06A ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ E O 0 6 A
*  - - - - - - - - - - - -
*
*  Test iau_EO06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EO06A, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EO06A


      CALL VVD ( iau_EO06A ( 2400000.5D0, 53736D0 ),
     :           -0.1332882371941833644D-2, 1D-15,
     :           'iau_EO06A', ' ', STATUS )

      END

      SUBROUTINE T_iau_EORS ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ E O R S
*  - - - - - - - - - - -
*
*  Test iau_EORS routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EORS, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EORS, RNPB(3,3), S


      RNPB(1,1) = 0.9999989440476103608D0
      RNPB(1,2) = -0.1332881761240011518D-2
      RNPB(1,3) = -0.5790767434730085097D-3

      RNPB(2,1) = 0.1332858254308954453D-2
      RNPB(2,2) = 0.9999991109044505944D0
      RNPB(2,3) = -0.4097782710401555759D-4

      RNPB(3,1) = 0.5791308472168153320D-3
      RNPB(3,2) = 0.4020595661593994396D-4
      RNPB(3,3) = 0.9999998314954572365D0

      S = -0.1220040848472271978D-7

      CALL VVD ( iau_EORS ( RNPB, S ), -0.1332882715130744606D-2, 1D-14,
     :           'iau_EORS', ' ', STATUS )

      END

      SUBROUTINE T_iau_EPB ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ E P B
*  - - - - - - - - - -
*
*  Test iau_EPB routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EPB, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EPB


      CALL VVD ( iau_EPB ( 2415019.8135D0, 30103.18648D0 ),
     :           1982.418424159278580D0, 1D-12, 'iau_EPB', ' ', STATUS )

      END

      SUBROUTINE T_iau_EPB2JD ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ E P B 2 J D
*  - - - - - - - - - - - - -
*
*  Test iau_EPB2JD routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EPB2JD, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPB, DJM0, DJM


      EPB = 1957.3D0

      CALL iau_EPB2JD ( EPB, DJM0, DJM )
      CALL VVD ( DJM0, 2400000.5D0, 1D-9,
     :           'iau_EPB2JD', 'DJM0', STATUS )
      CALL VVD ( DJM, 35948.1915101513D0, 1D-9,
     :           'iau_EPB2JD', 'MJD', STATUS )

      END

      SUBROUTINE T_iau_EPJ ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ E P J
*  - - - - - - - - - -
*
*  Test iau_EPJ routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EPJ, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EPJ


      CALL VVD ( iau_EPJ ( 2451545D0, -7392.5D0 ),
     :           1979.760438056125941D0, 1D-12, 'iau_EPJ', ' ', STATUS )

      END

      SUBROUTINE T_iau_EPJ2JD ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ E P J 2 J D
*  - - - - - - - - - - -  -
*
*  Test iau_EPJ2JD routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EPJ2JD, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPJ, DJM0, DJM


      EPJ = 1996.8D0

      CALL iau_EPJ2JD ( EPJ, DJM0, DJM )

      CALL VVD ( DJM0, 2400000.5D0, 1D-9,
     :           'iau_EPJ2JD', 'DJM0', STATUS )
      CALL VVD ( DJM, 50375.7D0, 1D-9,
     :           'iau_EPJ2JD', 'MJD', STATUS )

      END

      SUBROUTINE T_iau_EPV00 ( STATUS )
*+
*  - - - - - - - -
*   T _ E P V 0 0
*  - - - - - - - -
*
*  Test iau_EPV00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called: iau_EPV00, VVD, VIV
*
*  This revision:  2009 July 11
*-
      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PVH(3,2), PVB(3,2)
      INTEGER JSTAT


      CALL iau_EPV00 ( 2400000.5D0, 53411.52501161D0, PVH, PVB,
     :                 JSTAT )

      CALL VVD ( PVH(1,1), -0.7757238809297706813D0, 1D-14,
     :           'iau_EPV00', 'PVH(X)', STATUS )
      CALL VVD ( PVH(2,1), +0.5598052241363340596D0, 1D-14,
     :           'iau_EPV00', 'PVH(Y)', STATUS )
      CALL VVD ( PVH(3,1), +0.2426998466481686993D0, 1D-14,
     :           'iau_EPV00', 'PVH(Z)', STATUS )
      CALL VVD ( PVH(1,2), -0.1091891824147313846D-1, 1D-15,
     :          'iau_EPV00', 'PVH(X)', STATUS )
      CALL VVD ( PVH(2,2), -0.1247187268440845008D-1, 1D-15,
     :          'iau_EPV00', 'PVH(Y)', STATUS )
      CALL VVD ( PVH(3,2), -0.5407569418065039061D-2, 1D-15,
     :          'iau_EPV00', 'PVH(Z)', STATUS )
      CALL VVD ( PVB(1,1), -0.7714104440491111971D0, 1D-14,
     :           'iau_EPV00', 'PVB(X)', STATUS )
      CALL VVD ( PVB(2,1), +0.5598412061824171323D0, 1D-14,
     :           'iau_EPV00', 'PVB(Y)', STATUS )
      CALL VVD ( PVB(3,1), +0.2425996277722452400D0, 1D-14,
     :           'iau_EPV00', 'PVB(Z)', STATUS )
      CALL VVD ( PVB(1,2), -0.1091874268116823295D-1, 1D-15,
     :           'iau_EPV00', 'PVB(X)', STATUS )
      CALL VVD ( PVB(2,2), -0.1246525461732861538D-1, 1D-15,
     :           'iau_EPV00', 'PVB(Y)', STATUS )
      CALL VVD ( PVB(3,2), -0.5404773180966231279D-2, 1D-15,
     :           'iau_EPV00', 'PVB(Z)', STATUS )
      CALL VIV ( JSTAT, 0, 'iau_EPV00', 'JSTAT', STATUS )

      END

      SUBROUTINE T_iau_EQEQ94 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ E Q E Q 9 4
*  - - - - - - - - - - - - -
*
*  Test iau_EQEQ94 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_EQEQ94, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_EQEQ94


      CALL VVD ( iau_EQEQ94 ( 2400000.5D0, 41234D0 ),
     :           0.5357758254609256894D-4, 1D-17,
     :           'iau_EQEQ94', ' ', STATUS )

      END

      SUBROUTINE T_iau_ERA00 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ E R A 0 0
*  - - - - - - - - - - - -
*
*  Test iau_ERA00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_ERA00, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_ERA00


      CALL VVD ( iau_ERA00 ( 2400000.5D0, 54388D0 ),
     :           0.4022837240028158102D0, 1D-12,
     :           'iau_ERA00', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAD03 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F A D 0 3
*  - - - - - - - - - - - -
*
*  Test iau_FAD03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAD03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAD03


      CALL VVD ( iau_FAD03 ( 0.80D0 ),
     :           1.946709205396925672D0, 1D-12,
     :           'iau_FAD03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAE03 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F A E 0 3
*  - - - - - - - - - - - -
*
*  Test iau_FAE03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAE03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAE03


      CALL VVD ( iau_FAE03 ( 0.80D0 ),
     :           1.744713738913081846D0, 1D-12,
     :           'iau_FAE03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAF03 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F A F 0 3
*  - - - - - - - - - - - -
*
*  Test iau_FAF03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAF03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAF03


      CALL VVD ( iau_FAF03 ( 0.80D0 ),
     :           0.2597711366745499518D0, 1D-12,
     :           'iau_FAF03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAJU03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A J U 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAJU03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAJU03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAJU03


      CALL VVD ( iau_FAJU03 ( 0.80D0 ),
     :           5.275711665202481138D0, 1D-12,
     :           'iau_FAJU03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAL03 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F A L 0 3
*  - - - - - - - - - - - -
*
*  Test iau_FAL03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAL03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAL03


      CALL VVD ( iau_FAL03 ( 0.80D0 ),
     :           5.132369751108684150D0, 1D-12,
     :           'iau_FAL03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FALP03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A L P 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FALP03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FALP03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FALP03


      CALL VVD ( iau_FALP03 ( 0.80D0 ),
     :           6.226797973505507345D0, 1D-12,
     :           'iau_FALP03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAMA03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A M A 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAMA03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAMA03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAMA03


      CALL VVD ( iau_FAMA03 ( 0.80D0 ),
     :           3.275506840277781492D0, 1D-12,
     :           'iau_FAMA03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAME03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A M E 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAME03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAME03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAME03


      CALL VVD ( iau_FAME03 ( 0.80D0 ),
     :           5.417338184297289661D0, 1D-12,
     :           'iau_FAME03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FANE03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A N E 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FANE03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FANE03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FANE03


      CALL VVD ( iau_FANE03 ( 0.80D0 ),
     :           2.079343830860413523D0, 1D-12,
     :           'iau_FANE03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAOM03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A O M 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAOM03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAOM03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAOM03


      CALL VVD ( iau_FAOM03 ( 0.80D0 ),
     :           -5.973618440951302183D0, 1D-12,
     :           'iau_FAOM03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAPA03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A P A 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAPA03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAPA03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAPA03


      CALL VVD ( iau_FAPA03 ( 0.80D0 ), 0.195088476224D-1, 1D-12,
     :           'iau_FAPA03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FASA03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A S A 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FASA03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FASA03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FASA03


      CALL VVD ( iau_FASA03 ( 0.80D0 ),
     :           5.371574539440827046D0, 1D-12,
     :           'iau_FASA03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAUR03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A U R 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAUR03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAUR03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAUR03


      CALL VVD ( iau_FAUR03 ( 0.80D0 ),
     :           5.180636450180413523D0, 1D-12,
     :           'iau_FAUR03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FAVE03 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F A V E 0 3
*  - - - - - - - - - - - - -
*
*  Test iau_FAVE03 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FAVE03, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_FAVE03


      CALL VVD ( iau_FAVE03 ( 0.80D0 ),
     :           3.424900460533758000D0, 1D-12,
     :           'iau_FAVE03', ' ', STATUS )

      END

      SUBROUTINE T_iau_FK52H ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F K 5 2 H
*  - - - - - - - - - - - -
*
*  Test iau_FK52H routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FK52H, VVD
*
*  This revision:  2009 November 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R5, D5, DR5, DD5, PX5, RV5,
     :                RH, DH, DRH, DDH, PXH, RVH


      R5 = 1.76779433D0
      D5 = -0.2917517103D0
      DR5 = -1.91851572D-7
      DD5 = -5.8468475D-6
      PX5 = 0.379210D0
      RV5 = -7.6D0

      CALL iau_FK52H ( R5, D5, DR5, DD5, PX5, RV5,
     :                 RH, DH, DRH, DDH, PXH, RVH )

      CALL VVD ( RH, 1.767794226299947632D0, 1D-14,
     :           'iau_FK52H', 'RA', STATUS )
      CALL VVD ( DH, -0.2917516070530391757D0, 1D-14,
     :           'iau_FK52H', 'DEC', STATUS )
      CALL VVD ( DRH, -0.19618741256057224D-6, 1D-19,
     :           'iau_FK52H', 'DR5', STATUS )
      CALL VVD ( DDH, -0.58459905176693911D-5, 1D-19,
     :           'iau_FK52H', 'DD5', STATUS )
      CALL VVD ( PXH, 0.37921D0, 1D-14,
     :           'iau_FK52H', 'PX', STATUS )
      CALL VVD ( RVH, -7.6000000940000254D0, 1D-11,
     :           'iau_FK52H', 'RV', STATUS )

      END

      SUBROUTINE T_iau_FK5HIP ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ F K 5 H I P
*  - - - - - - - - - - - - -
*
*  Test iau_FK5HIP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FK5HIP, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R5H(3,3), S5H(3)


      CALL iau_FK5HIP ( R5H, S5H )

      CALL VVD ( R5H(1,1), 0.9999999999999928638D0, 1D-14,
     :           'iau_FK5HIP', 'R511', STATUS )
      CALL VVD ( R5H(1,2), 0.1110223351022919694D-6, 1D-17,
     :           'iau_FK5HIP', 'R512', STATUS )
      CALL VVD ( R5H(1,3), 0.4411803962536558154D-7, 1D-17,
     :           'iau_FK5HIP', 'R513', STATUS )
      CALL VVD ( R5H(2,1), -0.1110223308458746430D-6, 1D-17,
     :           'iau_FK5HIP', 'R521', STATUS )
      CALL VVD ( R5H(2,2), 0.9999999999999891830D0, 1D-14,
     :           'iau_FK5HIP', 'R522', STATUS )
      CALL VVD ( R5H(2,3), -0.9647792498984142358D-7, 1D-17,
     :           'iau_FK5HIP', 'R523', STATUS )
      CALL VVD ( R5H(3,1), -0.4411805033656962252D-7, 1D-17,
     :           'iau_FK5HIP', 'R531', STATUS )
      CALL VVD ( R5H(3,2), 0.9647792009175314354D-7, 1D-17,
     :           'iau_FK5HIP', 'R532', STATUS )
      CALL VVD ( R5H(3,3), 0.9999999999999943728D0, 1D-14,
     :           'iau_FK5HIP', 'R533', STATUS )
      CALL VVD ( S5H(1), -0.1454441043328607981D-8, 1D-17,
     :           'iau_FK5HIP', 'S51', STATUS )
      CALL VVD ( S5H(2), 0.2908882086657215962D-8, 1D-17,
     :           'iau_FK5HIP', 'S52', STATUS )
      CALL VVD ( S5H(3), 0.3393695767766751955D-8, 1D-17,
     :           'iau_FK5HIP', 'S53', STATUS )

      END

      SUBROUTINE T_iau_FK5HZ ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F K 5 H Z
*  - - - - - - - - - - - -
*
*  Test iau_FK5HZ routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FK5HZ, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R5, D5, RH, DH


      R5 = 1.76779433D0
      D5 = -0.2917517103D0

      CALL iau_FK5HZ ( R5, D5, 2400000.5D0, 54479D0, RH, DH )

      CALL VVD ( RH, 1.767794191464423978D0, 1D-12,
     :           'iau_FK5HZ', 'RA', STATUS )
      CALL VVD ( DH, -0.2917516001679884419D0, 1D-12,
     :           'iau_FK5HZ', 'DEC', STATUS )

      END

      SUBROUTINE T_iau_FW2M ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ F W 2 M
*  - - - - - - - - - - -
*
*  Test iau_FW2M routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FW2M, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION GAMB, PHIB, PSI, EPS, R(3,3)


      GAMB = -0.2243387670997992368D-5
      PHIB = 0.4091014602391312982D0
      PSI = -0.9501954178013015092D-3
      EPS = 0.4091014316587367472D0

      CALL iau_FW2M ( GAMB, PHIB, PSI, EPS, R )

      CALL VVD ( R(1,1), 0.9999995505176007047D0, 1D-12,
     :           'iau_FW2M', '11', STATUS )
      CALL VVD ( R(1,2), 0.8695404617348192957D-3, 1D-12,
     :           'iau_FW2M', '12', STATUS )
      CALL VVD ( R(1,3), 0.3779735201865582571D-3, 1D-12,
     :           'iau_FW2M', '13', STATUS )
      CALL VVD ( R(2,1), -0.8695404723772016038D-3, 1D-12,
     :           'iau_FW2M', '21', STATUS )
      CALL VVD ( R(2,2), 0.9999996219496027161D0, 1D-12,
     :           'iau_FW2M', '22', STATUS )
      CALL VVD ( R(2,3), -0.1361752496887100026D-6, 1D-12,
     :           'iau_FW2M', '23', STATUS )
      CALL VVD ( R(3,1), -0.3779734957034082790D-3, 1D-12,
     :           'iau_FW2M', '31', STATUS )
      CALL VVD ( R(3,2), -0.1924880848087615651D-6, 1D-12,
     :           'iau_FW2M', '32', STATUS )
      CALL VVD ( R(3,3), 0.9999999285679971958D0, 1D-12,
     :           'iau_FW2M', '33', STATUS )

      END

      SUBROUTINE T_iau_FW2XY ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ F W 2 X Y
*  - - - - - - - - - - - -
*
*  Test iau_FW2XY routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_FW2XY, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION GAMB, PHIB, PSI, EPS, X, Y


      GAMB = -0.2243387670997992368D-5
      PHIB = 0.4091014602391312982D0
      PSI = -0.9501954178013015092D-3
      EPS = 0.4091014316587367472D0

      CALL iau_FW2XY ( GAMB, PHIB, PSI, EPS, X, Y )

      CALL VVD ( X, -0.3779734957034082790D-3, 1D-14,
     :           'iau_FW2XY', 'X', STATUS )
      CALL VVD ( Y, -0.1924880848087615651D-6, 1D-14,
     :           'iau_FW2XY', 'Y', STATUS )

      END

      SUBROUTINE T_iau_GC2GD ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ G C 2 G D
*  - - - - - - - - - - - -
*
*  Test iau_GC2GD routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GC2GD, VVD, VIV
*
*  This revision:  2010 January 26
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION XYZ(3), E, P, H
      DATA XYZ / 2D6, 3D6, 5.244D6 /


      CALL iau_GC2GD ( 0, XYZ, E, P, H, J )

      CALL VIV ( J, -1, 'iau_GC2GD', 'J0', STATUS )

      CALL iau_GC2GD( 1, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'iau_GC2GD', 'J1', STATUS )
      CALL VVD ( E, 0.98279372324732907D0, 1D-14,
     :           'iau_GC2GD', 'E1', STATUS )
      CALL VVD ( P, 0.97160184819075459D0, 1D-14,
     :           'iau_GC2GD', 'P1', STATUS )
      CALL VVD ( H, 331.41724614260599D0, 1D-8,
     :           'iau_GC2GD', 'H1', STATUS )

      CALL iau_GC2GD ( 2, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'iau_GC2GD', 'J2', STATUS )
      CALL VVD ( E, 0.98279372324732907D0, 1D-14,
     :           'iau_GC2GD', 'E2', STATUS )
      CALL VVD ( P, 0.97160184820607853D0, 1D-14,
     :           'iau_GC2GD', 'P2', STATUS )
      CALL VVD ( H, 331.41731754844348D0, 1D-8,
     :           'iau_GC2GD', 'H2', STATUS )

      CALL iau_GC2GD ( 3, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'iau_GC2GD', 'J3', STATUS )
      CALL VVD ( E, 0.98279372324732907D0, 1D-14,
     :           'iau_GC2GD', 'E3', STATUS )
      CALL VVD ( P, 0.97160181811015119D0, 1D-14,
     :           'iau_GC2GD', 'P3', STATUS )
      CALL VVD ( H, 333.27707261303181D0, 1D-8,
     :           'iau_GC2GD', 'H3', STATUS )

      CALL iau_GC2GD ( 4, XYZ, E, P, H, J )

      CALL VIV ( J, -1, 'iau_GC2GD', 'J4', STATUS )

      END

      SUBROUTINE T_iau_GC2GDE ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G C 2 G D E
*  - - - - - - - - - - - - -
*
*  Test iau_GC2GDE routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GC2GDE, VVD, VIV
*
*  This revision:  2009 November 8
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J

      DOUBLE PRECISION A, F, XYZ(3), E, P, H
      DATA A, F, XYZ / 6378136D0, 0.0033528D0, 2D6, 3D6, 5.244D6 /


      CALL iau_GC2GDE ( a, f, XYZ, E, P, H, J )

      CALL VIV ( J, 0, 'iau_GC2GDE', 'J', STATUS )
      CALL VVD ( E, 0.98279372324732907D0, 1D-14,
     :           'iau_GC2GDE', 'E', STATUS )
      CALL VVD ( P, 0.97160183775704115D0, 1D-14,
     :           'iau_GC2GDE', 'P', STATUS )
      CALL VVD ( H, 332.36862495764397D0, 1D-8,
     :           'iau_GC2GDE', 'H', STATUS )

      END

      SUBROUTINE T_iau_GD2GC ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ G D 2 G C
*  - - - - - - - - - - - -
*
*  Test iau_GD2GC routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GD2GC, VVD, VIV
*
*  This revision:  2010 January 26
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION E, P, H, XYZ(3)
      DATA E, P, H / 3.1D0, -0.5D0, 2500D0 /


      CALL iau_GD2GC ( 0, E, P, H, XYZ, J )

      CALL VIV ( J, -1, 'iau_GD2GC', 'J0', STATUS )

      CALL iau_GD2GC( 1, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'iau_GD2GC', 'J1', STATUS )
      CALL VVD ( XYZ(1), -5599000.5577049947D0, 1D-7,
     :           'iau_GD2GC', '1/1', STATUS )
      CALL VVD ( XYZ(2), 233011.67223479203D0, 1D-7,
     :           'iau_GD2GC', '2/1', STATUS )
      CALL VVD ( XYZ(3), -3040909.4706983363D0, 1D-7,
     :           'iau_GD2GC', '3/1', STATUS )

      CALL iau_GD2GC( 2, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'iau_GD2GC', 'J2', STATUS )
      CALL VVD ( XYZ(1), -5599000.5577260984D0, 1D-7,
     :           'iau_GD2GC', '1/2', STATUS )
      CALL VVD ( XYZ(2), 233011.6722356703D0, 1D-7,
     :           'iau_GD2GC', '2/2', STATUS )
      CALL VVD ( XYZ(3), -3040909.4706095476D0, 1D-7,
     :           'iau_GD2GC', '3/2', STATUS )

      CALL iau_GD2GC( 3, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'iau_GD2GC', 'J3', STATUS )
      CALL VVD ( XYZ(1), -5598998.7626301490D0, 1D-7,
     :           'iau_GD2GC', '1/3', STATUS )
      CALL VVD ( XYZ(2), 233011.5975297822D0, 1D-7,
     :           'iau_GD2GC', '2/3', STATUS )
      CALL VVD ( XYZ(3), -3040908.6861467111D0, 1D-7,
     :           'iau_GD2GC', '3/3', STATUS )

      CALL iau_GD2GC( 4, E, P, H, XYZ, J )

      CALL VIV ( J, -1, 'iau_GD2GC', 'J4', STATUS )

      END

      SUBROUTINE T_iau_GD2GCE ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G D 2 G C E
*  - - - - - - - - - - - - -
*
*  Test iau_GD2GCE routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GD2GCE, VVD, VIV
*
*  This revision:  2009 November 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      INTEGER J
      DOUBLE PRECISION A, F, E, P, H, XYZ(3)
      DATA A, F, E, P, H / 6378136D0, 0.0033528D0,
     :                     3.1D0, -0.5D0, 2500D0 /


      CALL iau_GD2GCE ( A, F, E, P, H, XYZ, J )

      CALL VIV ( J, 0, 'iau_GD2GCE', 'J', STATUS )
      CALL VVD ( XYZ(1), -5598999.6665116328D0, 1D-7,
     :           'iau_GD2GCE', '0', STATUS )
      CALL VVD ( XYZ(2), 233011.63514630572D0, 1D-7,
     :           'iau_GD2GCE', '1', STATUS )
      CALL VVD ( XYZ(3), -3040909.0517314132D0, 1D-7,
     :           'iau_GD2GCE', '2', STATUS )

      END

      SUBROUTINE T_iau_GMST00 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G M S T 0 0
*  - - - - - - - - - - - - -
*
*  Test iau_GMST00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GMST00, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GMST00


      CALL VVD ( iau_GMST00 ( 2400000.5D0, 53736D0,
     :                        2400000.5D0, 53736D0 ),
     :           1.754174972210740592D0, 1D-12,
     :           'iau_GMST00', ' ', STATUS )

      END

      SUBROUTINE T_iau_GMST06 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G M S T 0 6
*  - - - - - - - - - - - - -
*
*  Test iau_GMST06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GMST06, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GMST06


      CALL VVD ( iau_GMST06 ( 2400000.5D0, 53736D0,
     :                        2400000.5D0, 53736D0 ),
     :           1.754174971870091203D0, 1D-12,
     :           'iau_GMST06', ' ', STATUS )

      END

      SUBROUTINE T_iau_GMST82 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G M S T 8 2
*  - - - - - - - - - - - - -
*
*  Test iau_GMST82 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GMST82, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GMST82


      CALL VVD ( iau_GMST82 ( 2400000.5D0, 53736D0 ),
     :           1.754174981860675096D0, 1D-12,
     :           'iau_GMST82', ' ', STATUS )

      END

      SUBROUTINE T_iau_GST00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G S T 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_GST00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GST00A, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GST00A


      CALL VVD ( iau_GST00A ( 2400000.5D0, 53736D0,
     :                        2400000.5D0, 53736D0 ),
     :           1.754166138018281369D0, 1D-12,
     :           'iau_GST00A', ' ', STATUS )

      END

      SUBROUTINE T_iau_GST00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G S T 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_GST00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GST00B, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GST00B


      CALL VVD ( iau_GST00B ( 2400000.5D0, 53736D0 ),
     :           1.754166136510680589D0, 1D-12,
     :           'iau_GST00B', ' ', STATUS )

      END

      SUBROUTINE T_iau_GST06 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ G S T 0 6
*  - - - - - - - - - - - -
*
*  Test iau_GST06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GST06, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GST06, RNPB(3,3)


      RNPB(1,1) = 0.9999989440476103608D0
      RNPB(1,2) = -0.1332881761240011518D-2
      RNPB(1,3) = -0.5790767434730085097D-3

      RNPB(2,1) = 0.1332858254308954453D-2
      RNPB(2,2) = 0.9999991109044505944D0
      RNPB(2,3) = -0.4097782710401555759D-4

      RNPB(3,1) = 0.5791308472168153320D-3
      RNPB(3,2) = 0.4020595661593994396D-4
      RNPB(3,3) = 0.9999998314954572365D0

      CALL VVD ( iau_GST06 ( 2400000.5D0, 53736D0,
     :                       2400000.5D0, 53736D0, RNPB ),
     :           1.754166138018167568D0, 1D-12,
     :           'iau_GST06', ' ', STATUS )

      END

      SUBROUTINE T_iau_GST06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ G S T 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_GST06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GST06A, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GST06A


      CALL VVD ( iau_GST06A ( 2400000.5D0, 53736D0,
     :                        2400000.5D0, 53736D0 ),
     :           1.754166137675019159D0, 1D-12,
     :           'iau_GST06A', ' ', STATUS )

      END

      SUBROUTINE T_iau_GST94 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ G S T 9 4
*  - - - - - - - - - - - -
*
*  Test iau_GST94 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_GST94, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_GST94


      CALL VVD ( iau_GST94 ( 2400000.5D0, 53736D0 ),
     :           1.754166136020645203D0, 1D-12,
     :           'iau_GST94', ' ', STATUS )

      END

      SUBROUTINE T_iau_H2FK5 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ H 2 F K 5
*  - - - - - - - - - - - -
*
*  Test iau_H2FK5 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_H2FK5, VVD
*
*  This revision:  2009 November 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RH, DH, DRH, DDH, PXH, RVH,
     :                R5, D5, DR5, DD5, PX5, RV5


      RH = 1.767794352D0
      DH = -0.2917512594D0
      DRH = -2.76413026D-6
      DDH = -5.92994449D-6
      PXH = 0.379210D0
      RVH = -7.6D0

      CALL iau_H2FK5 ( RH, DH, DRH, DDH, PXH, RVH,
     :                 R5, D5, DR5, DD5, PX5, RV5 )

      CALL VVD ( R5, 1.767794455700065506D0, 1D-13,
     :           'iau_H2FK5', 'RA', STATUS )
      CALL VVD ( D5, -0.2917513626469638890D0, 1D-13,
     :           'iau_H2FK5', 'DEC', STATUS )
      CALL VVD ( DR5, -0.27597945024511204D-5, 1D-18,
     :           'iau_H2FK5', 'DR5', STATUS )
      CALL VVD ( DD5, -0.59308014093262838D-5, 1D-18,
     :           'iau_H2FK5', 'DD5', STATUS )
      CALL VVD ( PX5, 0.37921D0, 1D-13,
     :           'iau_H2FK5', 'PX', STATUS )
      CALL VVD ( RV5, -7.6000001309071126D0, 1D-10,
     :           'iau_H2FK5', 'RV', STATUS )

      END

      SUBROUTINE T_iau_HFK5Z ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ H F K 5 Z
*  - - - - - - - - - - - -
*
*  Test iau_HFK5Z routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_HFK5Z, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RH, DH, R5, D5, DR5, DD5


      RH = 1.767794352D0
      DH = -0.2917512594D0

      CALL iau_HFK5Z ( RH, DH, 2400000.5D0, 54479D0,
     :                 R5, D5, DR5, DD5 )

      CALL VVD ( R5, 1.767794490535581026D0, 1D-13,
     :           'iau_HFK5Z', 'RA', STATUS )
      CALL VVD ( D5, -0.2917513695320114258D0, 1D-14,
     :           'iau_HFK5Z', 'DEC', STATUS )
      CALL VVD ( DR5, 0.4335890983539243029D-8, 1D-22,
     :           'iau_HFK5Z', 'DR5', STATUS )
      CALL VVD ( DD5, -0.8569648841237745902D-9, 1D-23,
     :           'iau_HFK5Z', 'DD5', STATUS )

      END

      SUBROUTINE T_iau_IR ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ I R
*  - - - - - - - - -
*
*  Test iau_IR routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_IR, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  R(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_IR ( R )

      CALL VVD ( R(1,1), 1D0, 0D0, 'iau_IR', '11', STATUS )
      CALL VVD ( R(1,2), 0D0, 0D0, 'iau_IR', '12', STATUS )
      CALL VVD ( R(1,3), 0D0, 0D0, 'iau_IR', '13', STATUS )
      CALL VVD ( R(2,1), 0D0, 0D0, 'iau_IR', '21', STATUS )
      CALL VVD ( R(2,2), 1D0, 0D0, 'iau_IR', '22', STATUS )
      CALL VVD ( R(2,3), 0D0, 0D0, 'iau_IR', '23', STATUS )
      CALL VVD ( R(3,1), 0D0, 0D0, 'iau_IR', '31', STATUS )
      CALL VVD ( R(3,2), 0D0, 0D0, 'iau_IR', '32', STATUS )
      CALL VVD ( R(3,3), 1D0, 0D0, 'iau_IR', '33', STATUS )

      END

      SUBROUTINE T_iau_JD2CAL ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ J D 2 C A L
*  - - - - - - - - - - - - -
*
*  Test iau_JD2CAL routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_JD2CAL, VIV, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J


      DJ1 = 2400000.5D0
      DJ2 = 50123.9999D0

      CALL iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )

      CALL VIV ( IY, 1996, 'iau_JD2CAL', 'Y', STATUS )
      CALL VIV ( IM, 2, 'iau_JD2CAL', 'M', STATUS )
      CALL VIV ( ID, 10, 'iau_JD2CAL', 'D', STATUS )
      CALL VVD ( FD, 0.9999D0, 1D-7, 'iau_JD2CAL', 'FD', STATUS )
      CALL VIV ( J, 0, 'iau_JD2CAL', 'J', STATUS )

      END

      SUBROUTINE T_iau_JDCALF ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ J D C A L F
*  - - - - - - - - - - - - -
*
*  Test iau_JDCALF routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_JDCALF, VIV
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IYDMF(4)
      INTEGER J


      DJ1 = 2400000.5D0
      DJ2 = 50123.9999D0

      CALL iau_JDCALF ( 4, DJ1, DJ2, IYDMF, J )

      CALL VIV ( IYDMF(1), 1996, 'iau_JDCALF', 'Y', STATUS )
      CALL VIV ( IYDMF(2), 2, 'iau_JDCALF', 'M', STATUS )
      CALL VIV ( IYDMF(3), 10, 'iau_JDCALF', 'D', STATUS )
      CALL VIV ( IYDMF(4), 9999, 'iau_JDCALF', 'F', STATUS )
      CALL VIV ( J, 0, 'iau_JDCALF', 'J', STATUS )

      END

      SUBROUTINE T_iau_NUM00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U M 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_NUM00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUM00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL iau_NUM00A ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536227949D0, 1D-12,
     :           'iau_NUM00A', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8836238544090873336D-5, 1D-12,
     :           'iau_NUM00A', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3830835237722400669D-5, 1D-12,
     :           'iau_NUM00A', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8836082880798569274D-5, 1D-12,
     :           'iau_NUM00A', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354655028D0, 1D-12,
     :           'iau_NUM00A', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063240865362499850D-4, 1D-12,
     :           'iau_NUM00A', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831194272065995866D-5, 1D-12,
     :           'iau_NUM00A', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063237480216291775D-4, 1D-12,
     :           'iau_NUM00A', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671660338D0, 1D-12,
     :           'iau_NUM00A', '33', STATUS )

      END

      SUBROUTINE T_iau_NUM00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U M 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_NUM00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUM00B, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL iau_NUM00B ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536069682D0, 1D-12,
     :           'iau_NUM00B', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8837746144871248011D-5, 1D-12,
     :           'iau_NUM00B', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3831488838252202945D-5, 1D-12,
     :           'iau_NUM00B', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8837590456632304720D-5, 1D-12,
     :           'iau_NUM00B', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354692733D0, 1D-12,
     :           'iau_NUM00B', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063198798559591654D-4, 1D-12,
     :           'iau_NUM00B', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831847930134941271D-5, 1D-12,
     :           'iau_NUM00B', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063195412258168380D-4, 1D-12,
     :           'iau_NUM00B', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671806225D0, 1D-12,
     :           'iau_NUM00B', '33', STATUS )

      END

      SUBROUTINE T_iau_NUM06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U M 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_NUM06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUM06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL iau_NUM06A ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536227668D0, 1D-12,
     :           'iau_NUM06A', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8836241998111535233D-5, 1D-12,
     :           'iau_NUM06A', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3830834608415287707D-5, 1D-12,
     :           'iau_NUM06A', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8836086334870740138D-5, 1D-12,
     :           'iau_NUM06A', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354657474D0, 1D-12,
     :           'iau_NUM06A', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063240188248455065D-4, 1D-12,
     :           'iau_NUM06A', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831193642839398128D-5, 1D-12,
     :           'iau_NUM06A', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063236803101479770D-4, 1D-12,
     :           'iau_NUM06A', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671663114D0, 1D-12,
     :           'iau_NUM06A', '33', STATUS )

      END

      SUBROUTINE T_iau_NUMAT ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ N U M A T
*  - - - - - - - - - - - -
*
*  Test iau_NUMAT routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUMAT, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPSA, DPSI, DEPS, RMATN(3,3)


      EPSA = 0.4090789763356509900D0
      DPSI = -0.9630909107115582393D-5
      DEPS = 0.4063239174001678826D-4

      CALL iau_NUMAT ( EPSA, DPSI, DEPS, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999536227949D0, 1D-12,
     :           'iau_NUMAT', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8836239320236250577D-5, 1D-12,
     :           'iau_NUMAT', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3830833447458251908D-5, 1D-12,
     :           'iau_NUMAT', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8836083657016688588D-5, 1D-12,
     :           'iau_NUMAT', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991354654959D0, 1D-12,
     :           'iau_NUMAT', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4063240865361857698D-4, 1D-12,
     :           'iau_NUMAT', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3831192481833385226D-5, 1D-12,
     :           'iau_NUMAT', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4063237480216934159D-4, 1D-12,
     :           'iau_NUMAT', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991671660407D0, 1D-12,
     :           'iau_NUMAT', '33', STATUS )

      END

      SUBROUTINE T_iau_NUT00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U T 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_NUT00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUT00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL iau_NUT00A ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9630909107115518431D-5, 1D-13,
     :           'iau_NUT00A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063239174001678710D-4, 1D-13,
     :           'iau_NUT00A', 'DEPS', STATUS )

      END

      SUBROUTINE T_iau_NUT00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U T 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_NUT00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUT00B, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL iau_NUT00B ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9632552291148362783D-5, 1D-13,
     :           'iau_NUT00B', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063197106621159367D-4, 1D-13,
     :           'iau_NUT00B', 'DEPS', STATUS )

      END

      SUBROUTINE T_iau_NUT06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U T 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_NUT06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUT06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL iau_NUT06A ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9630912025820308797D-5, 1D-13,
     :           'iau_NUT06A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063238496887249798D-4, 1D-13,
     :           'iau_NUT06A', 'DEPS', STATUS )

      END

      SUBROUTINE T_iau_NUT80 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ N U T 8 0
*  - - - - - - - - - - - -
*
*  Test iau_NUT80 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUT80, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS


      CALL iau_NUT80 ( 2400000.5D0, 53736D0, DPSI, DEPS )

      CALL VVD ( DPSI, -0.9643658353226563966D-5, 1D-13,
     :           'iau_NUT80', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4060051006879713322D-4, 1D-13,
     :           'iau_NUT80', 'DEPS', STATUS )

      END

      SUBROUTINE T_iau_NUTM80 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ N U T M 8 0
*  - - - - - - - - - - - - -
*
*  Test iau_NUTM80 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_NUTM80, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATN(3,3)


      CALL iau_NUTM80 ( 2400000.5D0, 53736D0, RMATN )

      CALL VVD ( RMATN(1,1), 0.9999999999534999268D0, 1D-12,
     :           'iau_NUTM80', '11', STATUS )
      CALL VVD ( RMATN(1,2), 0.8847935789636432161D-5, 1D-12,
     :           'iau_NUTM80', '12', STATUS )
      CALL VVD ( RMATN(1,3), 0.3835906502164019142D-5, 1D-12,
     :           'iau_NUTM80', '13', STATUS )
      CALL VVD ( RMATN(2,1), -0.8847780042583435924D-5, 1D-12,
     :           'iau_NUTM80', '21', STATUS )
      CALL VVD ( RMATN(2,2), 0.9999999991366569963D0, 1D-12,
     :           'iau_NUTM80', '22', STATUS )
      CALL VVD ( RMATN(2,3), -0.4060052702727130809D-4, 1D-12,
     :           'iau_NUTM80', '23', STATUS )
      CALL VVD ( RMATN(3,1), -0.3836265729708478796D-5, 1D-12,
     :           'iau_NUTM80', '31', STATUS )
      CALL VVD ( RMATN(3,2), 0.4060049308612638555D-4, 1D-12,
     :           'iau_NUTM80', '32', STATUS )
      CALL VVD ( RMATN(3,3), 0.9999999991684415129D0, 1D-12,
     :           'iau_NUTM80', '33', STATUS )

      END

      SUBROUTINE T_iau_OBL06 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ O B L 0 6
*  - - - - - - - - - - - -
*
*  Test iau_OBL06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_OBL06, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_OBL06


      CALL VVD ( iau_OBL06 ( 2400000.5D0, 54388D0 ),
     :           0.4090749229387258204D0, 1D-14,
     :           'iau_OBL06', ' ', STATUS )

      END

      SUBROUTINE T_iau_OBL80 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ O B L 8 0
*  - - - - - - - - - - - -
*
*  Test iau_OBL80 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_OBL80, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_OBL80


      CALL VVD ( iau_OBL80 ( 2400000.5D0, 54388D0 ),
     :           0.4090751347643816218D0, 1D-14,
     :           'iau_OBL06', ' ', STATUS )

      END

      SUBROUTINE T_iau_P06E ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P 0 6 E
*  - - - - - - - - - - -
*
*  Test iau_P06E routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_P06E, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EPS0, PSIA, OMA, BPA, BQA, PIA, BPIA,
     :                 EPSA, CHIA, ZA, ZETAA, THETAA, PA,
     :                 GAM, PHI, PSI


      CALL iau_P06E ( 2400000.5D0, 52541D0, EPS0, PSIA, OMA, BPA,
     :                BQA, PIA, BPIA, EPSA, CHIA, ZA,
     :                ZETAA, THETAA, PA, GAM, PHI, PSI )

      CALL VVD ( EPS0, 0.4090926006005828715D0, 1D-14,
     :           'iau_P06E', 'EPS0', STATUS )
      CALL VVD ( PSIA, 0.6664369630191613431D-3, 1D-14,
     :           'iau_P06E', 'PSIA', STATUS )
      CALL VVD ( OMA, 0.4090925973783255982D0, 1D-14,
     :           'iau_P06E', 'OMA', STATUS )
      CALL VVD ( BPA, 0.5561149371265209445D-6, 1D-14,
     :           'iau_P06E', 'BPA', STATUS )
      CALL VVD ( BQA, -0.6191517193290621270D-5, 1D-14,
     :           'iau_P06E', 'BQA', STATUS )
      CALL VVD ( PIA, 0.6216441751884382923D-5, 1D-14,
     :           'iau_P06E', 'PIA', STATUS )
      CALL VVD ( BPIA, 3.052014180023779882D0, 1D-14,
     :           'iau_P06E', 'BPIA', STATUS )
      CALL VVD ( EPSA, 0.4090864054922431688D0, 1D-14,
     :           'iau_P06E', 'EPSA', STATUS )
      CALL VVD ( CHIA, 0.1387703379530915364D-5, 1D-14,
     :           'iau_P06E', 'CHIA', STATUS )
      CALL VVD ( ZA, 0.2921789846651790546D-3, 1D-14,
     :           'iau_P06E', 'ZA', STATUS )
      CALL VVD ( ZETAA, 0.3178773290332009310D-3, 1D-14,
     :           'iau_P06E', 'ZETAA', STATUS )
      CALL VVD ( THETAA, 0.2650932701657497181D-3, 1D-14,
     :           'iau_P06E', 'THETAA', STATUS )
      CALL VVD ( PA, 0.6651637681381016344D-3, 1D-14,
     :           'iau_P06E', 'PA', STATUS )
      CALL VVD ( GAM, 0.1398077115963754987D-5, 1D-14,
     :           'iau_P06E', 'GAM', STATUS )
      CALL VVD ( PHI, 0.4090864090837462602D0, 1D-14,
     :           'iau_P06E', 'PHI', STATUS )
      CALL VVD ( PSI, 0.6664464807480920325D-3, 1D-14,
     :           'iau_P06E', 'PSI', STATUS )

      END

      SUBROUTINE T_iau_P2PV ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P 2 P V
*  - - - - - - - - - - -
*
*  Test iau_P2PV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_P2PV, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), PV(3,2)


      P(1) = 0.25D0
      P(2) = 1.2D0
      P(3) = 3D0

      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL iau_P2PV ( P, PV )

      CALL VVD ( PV(1,1), 0.25D0, 0D0, 'iau_P2PV', 'P1', STATUS )
      CALL VVD ( PV(2,1), 1.2D0, 0D0, 'iau_P2PV', 'P2', STATUS )
      CALL VVD ( PV(3,1), 3D0, 0D0, 'iau_P2PV', 'P3', STATUS )
      CALL VVD ( PV(1,2), 0D0, 0D0, 'iau_P2PV', 'V1', STATUS )
      CALL VVD ( PV(2,2), 0D0, 0D0, 'iau_P2PV', 'V2', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0, 'iau_P2PV', 'V3', STATUS )

      END

      SUBROUTINE T_iau_P2S ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P 2 S
*  - - - - - - - - - -
*
*  Test iau_P2S routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_P2S, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), THETA, PHI, R


      P(1) = 100D0
      P(2) = -50D0
      P(3) = 25D0

      CALL iau_P2S ( P, THETA, PHI, R )

      CALL VVD ( THETA, -0.4636476090008061162D0, 1D-12,
     :           'iau_P2S', 'THETA', STATUS )
      CALL VVD ( PHI, 0.2199879773954594463D0, 1D-12,
     :           'iau_P2S', 'PHI', STATUS )
      CALL VVD ( R, 114.5643923738960002D0, 1D-9,
     :           'iau_P2S', 'R', STATUS )

      END

      SUBROUTINE T_iau_PAP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P A P
*  - - - - - - - - - -
*
*  Test iau_PAP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PAP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), THETA


      A(1) = 1D0
      A(2) = 0.1D0
      A(3) = 0.2D0

      B(1) = -3D0
      B(2) = 1D-3
      B(3) = 0.2D0

      CALL iau_PAP ( A, B, THETA )

      CALL VVD ( THETA, 0.3671514267841113674D0, 1D-12,
     :           'iau_PAP', ' ', STATUS )

      END

      SUBROUTINE T_iau_PAS ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P A S
*  - - - - - - - - - -
*
*  Test iau_PAS routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PAS, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION AL, AP, BL, BP, THETA


      AL = 1D0
      AP = 0.1D0

      BL = 0.2D0
      BP = -1D0

      CALL iau_PAS ( AL, AP, BL, BP, THETA )

      CALL VVD ( THETA, -2.724544922932270424D0, 1D-12,
     :           'iau_PAS', ' ', STATUS )

      END

      SUBROUTINE T_iau_PB06 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P B 0 6
*  - - - - - - - - - - -
*
*  Test iau_PB06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PB06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION BZETA, BZ, BTHETA


      CALL iau_PB06 ( 2400000.5D0, 50123.9999D0, BZETA, BZ, BTHETA )

      CALL VVD ( BZETA, -0.5092634016326478238D-3, 1D-12,
     :           'iau_PB06', 'BZETA', STATUS )
      CALL VVD ( BZ, -0.3602772060566044413D-3, 1D-12,
     :           'iau_PB06', 'BZ', STATUS )
      CALL VVD ( BTHETA, -0.3779735537167811177D-3, 1D-12,
     :           'iau_PB06', 'BTHETA', STATUS )

      END

      SUBROUTINE T_iau_PDP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P D P
*  - - - - - - - - - -
*
*  Test iau_PDP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PDP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), ADB


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL iau_PDP ( A, B, ADB )
      CALL VVD ( ADB, 20D0, 1D-12, 'iau_PDP', ' ', STATUS )

      END

      SUBROUTINE T_iau_PFW06 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P F W 0 6
*  - - - - - - - - - - - -
*
*  Test iau_PFW06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PFW06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION GAMB, PHIB, PSIB, EPSA


      CALL iau_PFW06 ( 2400000.5D0, 50123.9999D0,
     :                 GAMB, PHIB, PSIB, EPSA )

      CALL VVD ( GAMB, -0.2243387670997995690D-5, 1D-16,
     :           'iau_PFW06', 'GAMB', STATUS )
      CALL VVD ( PHIB, 0.4091014602391312808D0, 1D-12,
     :           'iau_PFW06', 'PHIB', STATUS )
      CALL VVD ( PSIB, -0.9501954178013031895D-3, 1D-14,
     :           'iau_PFW06', 'PSIB', STATUS )
      CALL VVD ( EPSA, 0.4091014316587367491D0, 1D-12,
     :           'iau_PFW06', 'EPSA', STATUS )

      END

      SUBROUTINE T_iau_PLAN94 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P L A N 9 4
*  - - - - - - - - - - - - -
*
*  Test iau_PLAN94 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PLAN94, VVD, VIV
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2)
      INTEGER J


      CALL iau_PLAN94 ( 2400000.5D0, 1D6, 0, PV, J )

      CALL VVD ( PV(1,1), 0D0, 0D0,
     :           'iau_PLAN94', 'PV(1) 1', STATUS )
      CALL VVD ( PV(2,1), 0D0, 0D0,
     :           'iau_PLAN94', 'PV(2) 1', STATUS )
      CALL VVD ( PV(3,1), 0D0, 0D0,
     :           'iau_PLAN94', 'PV(3) 1', STATUS )
      CALL VVD ( PV(1,2), 0D0, 0D0,
     :           'iau_PLAN94', 'PV(4) 1', STATUS )
      CALL VVD ( PV(2,2), 0D0, 0D0,
     :           'iau_PLAN94', 'PV(5) 1', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0,
     :           'iau_PLAN94', 'PV(6) 1', STATUS )
      CALL VIV ( J, -1, 'iau_PLAN94', 'J 1', STATUS )

      CALL iau_PLAN94 ( 2400000.5D0, 1D6, 10, PV, J )

      CALL VIV ( J, -1, 'iau_PLAN94', 'J 2', STATUS )

      CALL iau_PLAN94 ( 2400000.5D0, -320000D0, 3, PV, J )

      CALL VVD ( PV(1,1), 0.9308038666832975759D0, 1D-11,
     :           'iau_PLAN94', 'PV(1) 3', STATUS )
      CALL VVD ( PV(2,1), 0.3258319040261346000D0, 1D-11,
     :           'iau_PLAN94', 'PV(2) 3', STATUS )
      CALL VVD ( PV(3,1), 0.1422794544481140560D0, 1D-11,
     :           'iau_PLAN94', 'PV(3) 3', STATUS )
      CALL VVD ( PV(1,2), -0.6429458958255170006D-2, 1D-11,
     :           'iau_PLAN94', 'PV(4) 3', STATUS )
      CALL VVD ( PV(2,2), 0.1468570657704237764D-1, 1D-11,
     :           'iau_PLAN94', 'PV(5) 3', STATUS )
      CALL VVD ( PV(3,2), 0.6406996426270981189D-2, 1D-11,
     :           'iau_PLAN94', 'PV(6) 3', STATUS )
      CALL VIV ( J, 1, 'iau_PLAN94', 'J 3', STATUS )

      CALL iau_PLAN94 ( 2400000.5D0, 43999.9D0, 1, PV, J )

      CALL VVD ( PV(1,1), 0.2945293959257430832D0, 1D-11,
     :           'iau_PLAN94', 'PV(1) 4', STATUS )
      CALL VVD ( PV(2,1), -0.2452204176601049596D0, 1D-11,
     :           'iau_PLAN94', 'PV(2) 4', STATUS )
      CALL VVD ( PV(3,1), -0.1615427700571978153D0, 1D-11,
     :           'iau_PLAN94', 'PV(3) 4', STATUS )
      CALL VVD ( PV(1,2), 0.1413867871404614441D-1, 1D-11,
     :           'iau_PLAN94', 'PV(4) 4', STATUS )
      CALL VVD ( PV(2,2), 0.1946548301104706582D-1, 1D-11,
     :           'iau_PLAN94', 'PV(5) 4', STATUS )
      CALL VVD ( PV(3,2), 0.8929809783898904786D-2, 1D-11,
     :           'iau_PLAN94', 'PV(6) 4', STATUS )
      CALL VIV ( J, 0, 'iau_PLAN94', 'J 4', STATUS )

      END

      SUBROUTINE T_iau_PMAT00 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P M A T 0 0
*  - - - - - - - - - - - - -
*
*  Test iau_PMAT00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PMAT00, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBP(3,3)


      CALL iau_PMAT00 ( 2400000.5D0, 50123.9999D0, RBP )

      CALL VVD ( RBP(1,1), 0.9999995505175087260D0, 1D-12,
     :           'iau_PMAT00', '11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695405883617884705D-3, 1D-14,
     :           'iau_PMAT00', '12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779734722239007105D-3, 1D-14,
     :           'iau_PMAT00', '13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695405990410863719D-3, 1D-14,
     :           'iau_PMAT00', '21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219494925900D0, 1D-12,
     :           'iau_PMAT00', '22', STATUS )
      CALL VVD ( RBP(2,3), -0.1360775820404982209D-6, 1D-14,
     :           'iau_PMAT00', '23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734476558184991D-3, 1D-14,
     :           'iau_PMAT00', '31', STATUS )
      CALL VVD ( RBP(3,2), -0.1925857585832024058D-6, 1D-14,
     :           'iau_PMAT00', '32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285680153377D0, 1D-12,
     :           'iau_PMAT00', '33', STATUS )

      END

      SUBROUTINE T_iau_PMAT06 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P M A T 0 6
*  - - - - - - - - - - - - -
*
*  Test iau_PMAT06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PMAT06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBP(3,3)


      CALL iau_PMAT06 ( 2400000.5D0, 50123.9999D0, RBP )

      CALL VVD ( RBP(1,1), 0.9999995505176007047D0, 1D-12,
     :           'iau_PMAT06', '11', STATUS )
      CALL VVD ( RBP(1,2), 0.8695404617348208406D-3, 1D-14,
     :           'iau_PMAT06', '12', STATUS )
      CALL VVD ( RBP(1,3), 0.3779735201865589104D-3, 1D-14,
     :           'iau_PMAT06', '13', STATUS )
      CALL VVD ( RBP(2,1), -0.8695404723772031414D-3, 1D-14,
     :           'iau_PMAT06', '21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999996219496027161D0, 1D-12,
     :           'iau_PMAT06', '22', STATUS )
      CALL VVD ( RBP(2,3), -0.1361752497080270143D-6, 1D-14,
     :           'iau_PMAT06', '23', STATUS )
      CALL VVD ( RBP(3,1), -0.3779734957034089490D-3, 1D-14,
     :           'iau_PMAT06', '31', STATUS )
      CALL VVD ( RBP(3,2), -0.1924880847894457113D-6, 1D-14,
     :           'iau_PMAT06', '32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999999285679971958D0, 1D-12,
     :           'iau_PMAT06', '33', STATUS )

      END

      SUBROUTINE T_iau_PMAT76 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P M A T 7 6
*  - - - - - - - - - - - - -
*
*  Test iau_PMAT76 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PMAT76, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATP(3,3)


      CALL iau_PMAT76 ( 2400000.5D0, 50123.9999D0, RMATP )

      CALL VVD ( RMATP(1,1), 0.9999995504328350733D0, 1D-12,
     :           'iau_PMAT76', '11', STATUS )
      CALL VVD ( RMATP(1,2), 0.8696632209480960785D-3, 1D-14,
     :           'iau_PMAT76', '12', STATUS )
      CALL VVD ( RMATP(1,3), 0.3779153474959888345D-3, 1D-14,
     :           'iau_PMAT76', '13', STATUS )
      CALL VVD ( RMATP(2,1), -0.8696632209485112192D-3, 1D-14,
     :           'iau_PMAT76', '21', STATUS )
      CALL VVD ( RMATP(2,2), 0.9999996218428560614D0, 1D-12,
     :           'iau_PMAT76', '22', STATUS )
      CALL VVD ( RMATP(2,3), -0.1643284776111886407D-6, 1D-14,
     :           'iau_PMAT76', '23', STATUS )
      CALL VVD ( RMATP(3,1), -0.3779153474950335077D-3, 1D-14,
     :           'iau_PMAT76', '31', STATUS )
      CALL VVD ( RMATP(3,2), -0.1643306746147366896D-6, 1D-14,
     :           'iau_PMAT76', '32', STATUS )
      CALL VVD ( RMATP(3,3), 0.9999999285899790119D0, 1D-12,
     :           'iau_PMAT76', '33', STATUS )

      END

      SUBROUTINE T_iau_PM ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ P M
*  - - - - - - - - -
*
*  Test iau_PM routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PM, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), R


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL iau_PM ( P, R )

      CALL VVD ( R, 2.789265136196270604D0, 1D-12,
     :           'iau_PM', ' ', STATUS )

      END

      SUBROUTINE T_iau_PMP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P M P
*  - - - - - - - - - -
*
*  Test iau_PMP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PMP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), AMB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL iau_PMP ( A, B, AMB )

      CALL VVD ( AMB(1) + AMB(2) + AMB(3), -1D0, 1D-12,
     :           'iau_PMP', ' ', STATUS )

      END

      SUBROUTINE T_iau_PN ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ P N
*  - - - - - - - - -
*
*  Test iau_PN routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PN, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3), R, U(3)


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL iau_PN ( P, R, U )

      CALL VVD ( R, 2.789265136196270604D0, 1D-12,
     :           'iau_PN', 'R', STATUS )
      CALL VVD ( U(1), 0.1075552109073112058D0, 1D-12,
     :           'iau_PN', 'U1', STATUS )
      CALL VVD ( U(2), 0.4302208436292448232D0, 1D-12,
     :           'iau_PN', 'U2', STATUS )
      CALL VVD ( U(3), -0.8962934242275933816D0, 1D-12,
     :           'iau_PN', 'U3', STATUS )

      END

      SUBROUTINE T_iau_PN00 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P N 0 0
*  - - - - - - - - - - -
*
*  Test iau_PN00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PN00, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,
     :                 RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      DPSI = -0.9632552291149335877D-5
      DEPS = 0.4063197106621141414D-4

      CALL iau_PN00 ( 2400000.5D0, 53736D0,
     :                DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( EPSA, 0.4090791789404229916D0, 1D-12,
     :           'iau_PN00', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,
     :           'iau_PN00', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-18,
     :           'iau_PN00', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-18,
     :           'iau_PN00', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-18,
     :           'iau_PN00', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_PN00', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-18,
     :           'iau_PN00', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-18,
     :           'iau_PN00', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-18,
     :           'iau_PN00', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_PN00', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300532289018D0, 1D-12,
     :           'iau_PN00', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341647226791824349D-2, 1D-18,
     :           'iau_PN00', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880927190296547D-3, 1D-18,
     :           'iau_PN00', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341647231069759008D-2, 1D-18,
     :           'iau_PN00', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999908750433D0, 1D-12,
     :           'iau_PN00', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3837444441583715468D-6, 1D-14,
     :           'iau_PN00', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880828740957684D-3, 1D-14,
     :           'iau_PN00', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3984203267708834759D-6, 1D-14,
     :           'iau_PN00', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623538046D0, 1D-12,
     :           'iau_PN00', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300052243993D0, 1D-12,
     :           'iau_PN00', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717990239703727D-2, 1D-14,
     :           'iau_PN00', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075749891684053D-3, 1D-14,
     :           'iau_PN00', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341718013831739992D-2, 1D-14,
     :           'iau_PN00', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998959191343D0, 1D-12,
     :           'iau_PN00', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3505759733565421170D-6, 1D-14,
     :           'iau_PN00', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075206857717883D-3, 1D-14,
     :           'iau_PN00', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4315219955198608970D-6, 1D-14,
     :           'iau_PN00', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093036269D0, 1D-12,
     :           'iau_PN00', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536069682D0, 1D-12,
     :           'iau_PN00', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8837746144872140812D-5, 1D-16,
     :           'iau_PN00', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3831488838252590008D-5, 1D-16,
     :           'iau_PN00', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8837590456633197506D-5, 1D-16,
     :           'iau_PN00', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354692733D0, 1D-12,
     :           'iau_PN00', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063198798559573702D-4, 1D-16,
     :           'iau_PN00', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831847930135328368D-5, 1D-16,
     :           'iau_PN00', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063195412258150427D-4, 1D-16,
     :           'iau_PN00', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671806225D0, 1D-12,
     :           'iau_PN00', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440499982806D0, 1D-12,
     :           'iau_PN00', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332880253640848301D-2, 1D-14,
     :           'iau_PN00', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790760898731087295D-3, 1D-14,
     :           'iau_PN00', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332856746979948745D-2, 1D-14,
     :           'iau_PN00', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109064768883D0, 1D-12,
     :           'iau_PN00', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097740555723063806D-4, 1D-14,
     :           'iau_PN00', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791301929950205000D-3, 1D-14,
     :           'iau_PN00', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020553681373702931D-4, 1D-14,
     :           'iau_PN00', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314958529887D0, 1D-12,
     :           'iau_PN00', 'RBPN33', STATUS )

      END

      SUBROUTINE T_iau_PN00A ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P N 0 0 A
*  - - - - - - - - - - - -
*
*  Test iau_PN00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PN00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,
     :                 RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      CALL iau_PN00A ( 2400000.5D0, 53736D0,
     :                 DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( DPSI, -0.9630909107115518431D-5, 1D-12,
     :           'iau_PN00A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063239174001678710D-4, 1D-12,
     :           'iau_PN00A', 'DEPS', STATUS )
      CALL VVD ( EPSA, 0.4090791789404229916D0, 1D-12,
     :           'iau_PN00A', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,
     :           'iau_PN00A', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-16,
     :           'iau_PN00A', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-16,
     :           'iau_PN00A', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-16,
     :           'iau_PN00A', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_PN00A', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-16,
     :           'iau_PN00A', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-16,
     :           'iau_PN00A', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-16,
     :           'iau_PN00A', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_PN00A', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300532289018D0, 1D-12,
     :           'iau_PN00A', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341647226791824349D-2, 1D-14,
     :           'iau_PN00A', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880927190296547D-3, 1D-14,
     :           'iau_PN00A', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341647231069759008D-2, 1D-14,
     :           'iau_PN00A', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999908750433D0, 1D-12,
     :           'iau_PN00A', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3837444441583715468D-6, 1D-14,
     :           'iau_PN00A', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880828740957684D-3, 1D-14,
     :           'iau_PN00A', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3984203267708834759D-6, 1D-14,
     :           'iau_PN00A', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623538046D0, 1D-12,
     :           'iau_PN00A', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300052243993D0, 1D-12,
     :           'iau_PN00A', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717990239703727D-2, 1D-14,
     :           'iau_PN00A', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075749891684053D-3, 1D-14,
     :           'iau_PN00A', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341718013831739992D-2, 1D-14,
     :           'iau_PN00A', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998959191343D0, 1D-12,
     :           'iau_PN00A', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3505759733565421170D-6, 1D-14,
     :           'iau_PN00A', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075206857717883D-3, 1D-14,
     :           'iau_PN00A', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4315219955198608970D-6, 1D-14,
     :           'iau_PN00A', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093036269D0, 1D-12,
     :           'iau_PN00A', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536227949D0, 1D-12,
     :           'iau_PN00A', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8836238544090873336D-5, 1D-14,
     :           'iau_PN00A', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3830835237722400669D-5, 1D-14,
     :           'iau_PN00A', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8836082880798569274D-5, 1D-14,
     :           'iau_PN00A', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354655028D0, 1D-12,
     :           'iau_PN00A', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063240865362499850D-4, 1D-14,
     :           'iau_PN00A', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831194272065995866D-5, 1D-14,
     :           'iau_PN00A', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063237480216291775D-4, 1D-14,
     :           'iau_PN00A', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671660338D0, 1D-12,
     :           'iau_PN00A', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440476103435D0, 1D-12,
     :           'iau_PN00A', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332881761240011763D-2, 1D-14,
     :           'iau_PN00A', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790767434730085751D-3, 1D-14,
     :           'iau_PN00A', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332858254308954658D-2, 1D-14,
     :           'iau_PN00A', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109044505577D0, 1D-12,
     :           'iau_PN00A', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097782710396580452D-4, 1D-14,
     :           'iau_PN00A', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791308472168152904D-3, 1D-14,
     :           'iau_PN00A', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020595661591500259D-4, 1D-14,
     :           'iau_PN00A', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314954572304D0, 1D-12,
     :           'iau_PN00A', 'RBPN33', STATUS )

      END

      SUBROUTINE T_iau_PN00B ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P N 0 0 B
*  - - - - - - - - - - - -
*
*  Test iau_PN00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PN00B, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,
     :                 RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      CALL iau_PN00B ( 2400000.5D0, 53736D0,
     :                 DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( DPSI, -0.9632552291148362783D-5, 1D-12,
     :           'iau_PN00B', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063197106621159367D-4, 1D-12,
     :           'iau_PN00B', 'DEPS', STATUS )
      CALL VVD ( EPSA, 0.4090791789404229916D0, 1D-12,
     :           'iau_PN00B', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942498D0, 1D-12,
     :           'iau_PN00B', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078279744199196626D-7, 1D-16,
     :           'iau_PN00B', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056217146976134152D-7, 1D-16,
     :           'iau_PN00B', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078279477857337206D-7, 1D-16,
     :           'iau_PN00B', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_PN00B', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3306041454222136517D-7, 1D-16,
     :           'iau_PN00B', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056217380986972157D-7, 1D-16,
     :           'iau_PN00B', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3306040883980552500D-7, 1D-16,
     :           'iau_PN00B', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_PN00B', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300532289018D0, 1D-12,
     :           'iau_PN00B', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341647226791824349D-2, 1D-14,
     :           'iau_PN00B', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880927190296547D-3, 1D-14,
     :           'iau_PN00B', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341647231069759008D-2, 1D-14,
     :           'iau_PN00B', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999908750433D0, 1D-12,
     :           'iau_PN00B', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3837444441583715468D-6, 1D-14,
     :           'iau_PN00B', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880828740957684D-3, 1D-14,
     :           'iau_PN00B', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3984203267708834759D-6, 1D-14,
     :           'iau_PN00B', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623538046D0, 1D-12,
     :           'iau_PN00B', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300052243993D0, 1D-12,
     :           'iau_PN00B', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717990239703727D-2, 1D-14,
     :           'iau_PN00B', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075749891684053D-3, 1D-14,
     :           'iau_PN00B', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341718013831739992D-2, 1D-14,
     :           'iau_PN00B', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998959191343D0, 1D-12,
     :           'iau_PN00B', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3505759733565421170D-6, 1D-14,
     :           'iau_PN00B', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075206857717883D-3, 1D-14,
     :           'iau_PN00B', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4315219955198608970D-6, 1D-14,
     :           'iau_PN00B', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093036269D0, 1D-12,
     :           'iau_PN00B', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536069682D0, 1D-12,
     :           'iau_PN00B', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8837746144871248011D-5, 1D-14,
     :           'iau_PN00B', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3831488838252202945D-5, 1D-14,
     :           'iau_PN00B', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8837590456632304720D-5, 1D-14,
     :           'iau_PN00B', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354692733D0, 1D-12,
     :           'iau_PN00B', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063198798559591654D-4, 1D-14,
     :           'iau_PN00B', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831847930134941271D-5, 1D-14,
     :           'iau_PN00B', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063195412258168380D-4, 1D-14,
     :           'iau_PN00B', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671806225D0, 1D-12,
     :           'iau_PN00B', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440499982806D0, 1D-12,
     :           'iau_PN00B', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332880253640849194D-2, 1D-14,
     :           'iau_PN00B', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790760898731091166D-3, 1D-14,
     :           'iau_PN00B', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332856746979949638D-2, 1D-14,
     :           'iau_PN00B', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109064768883D0, 1D-12,
     :           'iau_PN00B', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097740555723081811D-4, 1D-14,
     :           'iau_PN00B', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791301929950208873D-3, 1D-14,
     :           'iau_PN00B', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020553681373720832D-4, 1D-14,
     :           'iau_PN00B', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314958529887D0, 1D-12,
     :           'iau_PN00B', 'RBPN33', STATUS )

      END

      SUBROUTINE T_iau_PN06A ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P N 0 6 A
*  - - - - - - - - - - - -
*
*  Test iau_PN06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PN06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,
     :                 RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      CALL iau_PN06A ( 2400000.5D0, 53736D0,
     :                 DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( DPSI, -0.9630912025820308797D-5, 1D-12,
     :           'iau_PN06A', 'DPSI', STATUS )
      CALL VVD ( DEPS, 0.4063238496887249798D-4, 1D-14,
     :           'iau_PN06A', 'DEPS', STATUS )
      CALL VVD ( EPSA, 0.4090789763356509926D0, 1D-14,
     :           'iau_PN06A', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942497D0, 1D-12,
     :           'iau_PN06A', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078368960971557145D-7, 1D-14,
     :           'iau_PN06A', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056213977613185606D-7, 1D-14,
     :           'iau_PN06A', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078368694637674333D-7, 1D-14,
     :           'iau_PN06A', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_PN06A', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3305943742989134124D-7, 1D-14,
     :           'iau_PN06A', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056214211620056792D-7, 1D-14,
     :           'iau_PN06A', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3305943172740586950D-7, 1D-14,
     :           'iau_PN06A', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_PN06A', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300536854831D0, 1D-12,
     :           'iau_PN06A', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341646886204443795D-2, 1D-14,
     :           'iau_PN06A', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880933488627759D-3, 1D-14,
     :           'iau_PN06A', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341646890569782183D-2, 1D-14,
     :           'iau_PN06A', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999913319321D0, 1D-12,
     :           'iau_PN06A', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3835944216374477457D-6, 1D-14,
     :           'iau_PN06A', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880833027867368D-3, 1D-14,
     :           'iau_PN06A', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3985701514686976112D-6, 1D-14,
     :           'iau_PN06A', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623534950D0, 1D-12,
     :           'iau_PN06A', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300056797893D0, 1D-12,
     :           'iau_PN06A', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717650545059598D-2, 1D-14,
     :           'iau_PN06A', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075756493728856D-3, 1D-14,
     :           'iau_PN06A', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341717674223918101D-2, 1D-14,
     :           'iau_PN06A', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998963748448D0, 1D-12,
     :           'iau_PN06A', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3504269280170069029D-6, 1D-14,
     :           'iau_PN06A', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075211461454599D-3, 1D-14,
     :           'iau_PN06A', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4316708436255949093D-6, 1D-14,
     :           'iau_PN06A', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093032943D0, 1D-12,
     :           'iau_PN06A', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536227668D0, 1D-12,
     :           'iau_PN06A', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8836241998111535233D-5, 1D-14,
     :           'iau_PN06A', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3830834608415287707D-5, 1D-14,
     :           'iau_PN06A', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8836086334870740138D-5, 1D-14,
     :           'iau_PN06A', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354657474D0, 1D-12,
     :           'iau_PN06A', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063240188248455065D-4, 1D-14,
     :           'iau_PN06A', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831193642839398128D-5, 1D-14,
     :           'iau_PN06A', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063236803101479770D-4, 1D-14,
     :           'iau_PN06A', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671663114D0, 1D-12,
     :           'iau_PN06A', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440480669738D0, 1D-12,
     :           'iau_PN06A', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332881418091915973D-2, 1D-14,
     :           'iau_PN06A', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790767447612042565D-3, 1D-14,
     :           'iau_PN06A', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332857911250989133D-2, 1D-14,
     :           'iau_PN06A', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109049141908D0, 1D-12,
     :           'iau_PN06A', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097767128546784878D-4, 1D-14,
     :           'iau_PN06A', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791308482835292617D-3, 1D-14,
     :           'iau_PN06A', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020580099454020310D-4, 1D-14,
     :           'iau_PN06A', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314954628695D0, 1D-12,
     :           'iau_PN06A', 'RBPN33', STATUS )

      END

      SUBROUTINE T_iau_PN06 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P N 0 6
*  - - - - - - - - - - -
*
*  Test iau_PN06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PN06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSI, DEPS, EPSA,
     :                 RB(3,3), RP(3,3), RBP(3,3), RN(3,3), RBPN(3,3)


      DPSI = -0.9632552291149335877D-5
      DEPS = 0.4063197106621141414D-4

      CALL iau_PN06 ( 2400000.5D0, 53736D0,
     :                DPSI, DEPS, EPSA, RB, RP, RBP, RN, RBPN )

      CALL VVD ( EPSA, 0.4090789763356509926D0, 1D-12,
     :           'iau_PN06', 'EPSA', STATUS )
      CALL VVD ( RB(1,1), 0.9999999999999942497D0, 1D-12,
     :           'iau_PN06', 'RB11', STATUS )
      CALL VVD ( RB(1,2), -0.7078368960971557145D-7, 1D-14,
     :           'iau_PN06', 'RB12', STATUS )
      CALL VVD ( RB(1,3), 0.8056213977613185606D-7, 1D-14,
     :           'iau_PN06', 'RB13', STATUS )
      CALL VVD ( RB(2,1), 0.7078368694637674333D-7, 1D-14,
     :           'iau_PN06', 'RB21', STATUS )
      CALL VVD ( RB(2,2), 0.9999999999999969484D0, 1D-12,
     :           'iau_PN06', 'RB22', STATUS )
      CALL VVD ( RB(2,3), 0.3305943742989134124D-7, 1D-14,
     :           'iau_PN06', 'RB23', STATUS )
      CALL VVD ( RB(3,1), -0.8056214211620056792D-7, 1D-14,
     :           'iau_PN06', 'RB31', STATUS )
      CALL VVD ( RB(3,2), -0.3305943172740586950D-7, 1D-14,
     :           'iau_PN06', 'RB32', STATUS )
      CALL VVD ( RB(3,3), 0.9999999999999962084D0, 1D-12,
     :           'iau_PN06', 'RB33', STATUS )
      CALL VVD ( RP(1,1), 0.9999989300536854831D0, 1D-12,
     :           'iau_PN06', 'RP11', STATUS )
      CALL VVD ( RP(1,2), -0.1341646886204443795D-2, 1D-14,
     :           'iau_PN06', 'RP12', STATUS )
      CALL VVD ( RP(1,3), -0.5829880933488627759D-3, 1D-14,
     :           'iau_PN06', 'RP13', STATUS )
      CALL VVD ( RP(2,1), 0.1341646890569782183D-2, 1D-14,
     :           'iau_PN06', 'RP21', STATUS )
      CALL VVD ( RP(2,2), 0.9999990999913319321D0, 1D-12,
     :           'iau_PN06', 'RP22', STATUS )
      CALL VVD ( RP(2,3), -0.3835944216374477457D-6, 1D-14,
     :           'iau_PN06', 'RP23', STATUS )
      CALL VVD ( RP(3,1), 0.5829880833027867368D-3, 1D-14,
     :           'iau_PN06', 'RP31', STATUS )
      CALL VVD ( RP(3,2), -0.3985701514686976112D-6, 1D-14,
     :           'iau_PN06', 'RP32', STATUS )
      CALL VVD ( RP(3,3), 0.9999998300623534950D0, 1D-12,
     :           'iau_PN06', 'RP33', STATUS )
      CALL VVD ( RBP(1,1), 0.9999989300056797893D0, 1D-12,
     :           'iau_PN06', 'RBP11', STATUS )
      CALL VVD ( RBP(1,2), -0.1341717650545059598D-2, 1D-14,
     :           'iau_PN06', 'RBP12', STATUS )
      CALL VVD ( RBP(1,3), -0.5829075756493728856D-3, 1D-14,
     :           'iau_PN06', 'RBP13', STATUS )
      CALL VVD ( RBP(2,1), 0.1341717674223918101D-2, 1D-14,
     :           'iau_PN06', 'RBP21', STATUS )
      CALL VVD ( RBP(2,2), 0.9999990998963748448D0, 1D-12,
     :           'iau_PN06', 'RBP22', STATUS )
      CALL VVD ( RBP(2,3), -0.3504269280170069029D-6, 1D-14,
     :           'iau_PN06', 'RBP23', STATUS )
      CALL VVD ( RBP(3,1), 0.5829075211461454599D-3, 1D-14,
     :           'iau_PN06', 'RBP31', STATUS )
      CALL VVD ( RBP(3,2), -0.4316708436255949093D-6, 1D-14,
     :           'iau_PN06', 'RBP32', STATUS )
      CALL VVD ( RBP(3,3), 0.9999998301093032943D0, 1D-12,
     :           'iau_PN06', 'RBP33', STATUS )
      CALL VVD ( RN(1,1), 0.9999999999536069682D0, 1D-12,
     :           'iau_PN06', 'RN11', STATUS )
      CALL VVD ( RN(1,2), 0.8837746921149881914D-5, 1D-14,
     :           'iau_PN06', 'RN12', STATUS )
      CALL VVD ( RN(1,3), 0.3831487047682968703D-5, 1D-14,
     :           'iau_PN06', 'RN13', STATUS )
      CALL VVD ( RN(2,1), -0.8837591232983692340D-5, 1D-14,
     :           'iau_PN06', 'RN21', STATUS )
      CALL VVD ( RN(2,2), 0.9999999991354692664D0, 1D-12,
     :           'iau_PN06', 'RN22', STATUS )
      CALL VVD ( RN(2,3), -0.4063198798558931215D-4, 1D-14,
     :           'iau_PN06', 'RN23', STATUS )
      CALL VVD ( RN(3,1), -0.3831846139597250235D-5, 1D-14,
     :           'iau_PN06', 'RN31', STATUS )
      CALL VVD ( RN(3,2), 0.4063195412258792914D-4, 1D-14,
     :           'iau_PN06', 'RN32', STATUS )
      CALL VVD ( RN(3,3), 0.9999999991671806293D0, 1D-12,
     :           'iau_PN06', 'RN33', STATUS )
      CALL VVD ( RBPN(1,1), 0.9999989440504506688D0, 1D-12,
     :           'iau_PN06', 'RBPN11', STATUS )
      CALL VVD ( RBPN(1,2), -0.1332879913170492655D-2, 1D-14,
     :           'iau_PN06', 'RBPN12', STATUS )
      CALL VVD ( RBPN(1,3), -0.5790760923225655753D-3, 1D-14,
     :           'iau_PN06', 'RBPN13', STATUS )
      CALL VVD ( RBPN(2,1), 0.1332856406595754748D-2, 1D-14,
     :           'iau_PN06', 'RBPN21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999991109069366795D0, 1D-12,
     :           'iau_PN06', 'RBPN22', STATUS )
      CALL VVD ( RBPN(2,3), -0.4097725651142641812D-4, 1D-14,
     :           'iau_PN06', 'RBPN23', STATUS )
      CALL VVD ( RBPN(3,1), 0.5791301952321296716D-3, 1D-14,
     :           'iau_PN06', 'RBPN31', STATUS )
      CALL VVD ( RBPN(3,2), 0.4020538796195230577D-4, 1D-14,
     :           'iau_PN06', 'RBPN32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999998314958576778D0, 1D-12,
     :           'iau_PN06', 'RBPN33', STATUS )

      END

      SUBROUTINE T_iau_PNM00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P N M 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_PNM00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PNM00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3)


      CALL iau_PNM00A ( 2400000.5D0, 50123.9999D0, RBPN )

      CALL VVD ( RBPN(1,1), 0.9999995832793134257D0, 1D-12,
     :           'iau_PNM00A', '11', STATUS )
      CALL VVD ( RBPN(1,2), 0.8372384254137809439D-3, 1D-14,
     :           'iau_PNM00A', '12', STATUS )
      CALL VVD ( RBPN(1,3), 0.3639684306407150645D-3, 1D-14,
     :           'iau_PNM00A', '13', STATUS )
      CALL VVD ( RBPN(2,1), -0.8372535226570394543D-3, 1D-14,
     :           'iau_PNM00A', '21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999996486491582471D0, 1D-12,
     :           'iau_PNM00A', '22', STATUS )
      CALL VVD ( RBPN(2,3), 0.4132915262664072381D-4, 1D-14,
     :           'iau_PNM00A', '23', STATUS )
      CALL VVD ( RBPN(3,1), -0.3639337004054317729D-3, 1D-14,
     :           'iau_PNM00A', '31', STATUS )
      CALL VVD ( RBPN(3,2), -0.4163386925461775873D-4, 1D-14,
     :           'iau_PNM00A', '32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999999329094390695D0, 1D-12,
     :           'iau_PNM00A', '33', STATUS )

      END

      SUBROUTINE T_iau_PNM00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P N M 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_PNM00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PNM00B, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3)


      CALL iau_PNM00B ( 2400000.5D0, 50123.9999D0, RBPN )

      CALL VVD ( RBPN(1,1), 0.9999995832776208280D0, 1D-12,
     :           'iau_PNM00B', '11', STATUS )
      CALL VVD ( RBPN(1,2), 0.8372401264429654837D-3, 1D-14,
     :           'iau_PNM00B', '12', STATUS )
      CALL VVD ( RBPN(1,3), 0.3639691681450271771D-3, 1D-14,
     :           'iau_PNM00B', '13', STATUS )
      CALL VVD ( RBPN(2,1), -0.8372552234147137424D-3, 1D-14,
     :           'iau_PNM00B', '21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999996486477686123D0, 1D-12,
     :           'iau_PNM00B', '22', STATUS )
      CALL VVD ( RBPN(2,3), 0.4132832190946052890D-4, 1D-14,
     :           'iau_PNM00B', '23', STATUS )
      CALL VVD ( RBPN(3,1), -0.3639344385341866407D-3, 1D-14,
     :           'iau_PNM00B', '31', STATUS )
      CALL VVD ( RBPN(3,2), -0.4163303977421522785D-4, 1D-14,
     :           'iau_PNM00B', '32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999999329092049734D0, 1D-12,
     :           'iau_PNM00B', '33', STATUS )

      END

      SUBROUTINE T_iau_PNM06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P N M 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_PNM06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PNM06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RBPN(3,3)


      CALL iau_PNM06A ( 2400000.5D0, 50123.9999D0, RBPN )

      CALL VVD ( RBPN(1,1), 0.9999995832794205484D0, 1D-12,
     :           'iau_PNM06A', '11', STATUS )
      CALL VVD ( RBPN(1,2), 0.8372382772630962111D-3, 1D-14,
     :           'iau_PNM06A', '12', STATUS )
      CALL VVD ( RBPN(1,3), 0.3639684771140623099D-3, 1D-14,
     :           'iau_PNM06A', '13', STATUS )
      CALL VVD ( RBPN(2,1), -0.8372533744743683605D-3, 1D-14,
     :           'iau_PNM06A', '21', STATUS )
      CALL VVD ( RBPN(2,2), 0.9999996486492861646D0, 1D-12,
     :           'iau_PNM06A', '22', STATUS )
      CALL VVD ( RBPN(2,3), 0.4132905944611019498D-4, 1D-14,
     :           'iau_PNM06A', '23', STATUS )
      CALL VVD ( RBPN(3,1), -0.3639337469629464969D-3, 1D-14,
     :           'iau_PNM06A', '31', STATUS )
      CALL VVD ( RBPN(3,2), -0.4163377605910663999D-4, 1D-14,
     :           'iau_PNM06A', '32', STATUS )
      CALL VVD ( RBPN(3,3), 0.9999999329094260057D0, 1D-12,
     :           'iau_PNM06A', '33', STATUS )

      END

      SUBROUTINE T_iau_PNM80 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P N M 8 0
*  - - - - - - - - - - - -
*
*  Test iau_PNM80 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PNM80, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RMATPN(3,3)


      CALL iau_PNM80 ( 2400000.5D0, 50123.9999D0, RMATPN )

      CALL VVD ( RMATPN(1,1), 0.9999995831934611169D0, 1D-12,
     :           'iau_PNM80', '11', STATUS )
      CALL VVD ( RMATPN(1,2), 0.8373654045728124011D-3, 1D-14,
     :           'iau_PNM80', '12', STATUS )
      CALL VVD ( RMATPN(1,3), 0.3639121916933106191D-3, 1D-14,
     :           'iau_PNM80', '13', STATUS )
      CALL VVD ( RMATPN(2,1), -0.8373804896118301316D-3, 1D-14,
     :           'iau_PNM80', '21', STATUS )
      CALL VVD ( RMATPN(2,2), 0.9999996485439674092D0, 1D-12,
     :           'iau_PNM80', '22', STATUS )
      CALL VVD ( RMATPN(2,3), 0.4130202510421549752D-4, 1D-14,
     :           'iau_PNM80', '23', STATUS )
      CALL VVD ( RMATPN(3,1), -0.3638774789072144473D-3, 1D-14,
     :           'iau_PNM80', '31', STATUS )
      CALL VVD ( RMATPN(3,2), -0.4160674085851722359D-4, 1D-14,
     :           'iau_PNM80', '32', STATUS )
      CALL VVD ( RMATPN(3,3), 0.9999999329310274805D0, 1D-12,
     :           'iau_PNM80', '33', STATUS )

      END

      SUBROUTINE T_iau_POM00 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P O M 0 0
*  - - - - - - - - - - - -
*
*  Test iau_POM00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_POM00, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION XP, YP, SP, RPOM(3,3)


      XP = 2.55060238D-7
      YP = 1.860359247D-6
      SP = -0.1367174580728891460D-10

      CALL iau_POM00 ( XP, YP, SP, RPOM )

      CALL VVD ( RPOM(1,1), 0.9999999999999674721D0, 1D-12,
     :           'iau_POM00', '11', STATUS )
      CALL VVD ( RPOM(1,2), -0.1367174580728846989D-10, 1D-16,
     :           'iau_POM00', '12', STATUS )
      CALL VVD ( RPOM(1,3), 0.2550602379999972345D-6, 1D-16,
     :           'iau_POM00', '13', STATUS )
      CALL VVD ( RPOM(2,1), 0.1414624947957029801D-10, 1D-16,
     :           'iau_POM00', '21', STATUS )
      CALL VVD ( RPOM(2,2), 0.9999999999982695317D0, 1D-12,
     :           'iau_POM00', '22', STATUS )
      CALL VVD ( RPOM(2,3), -0.1860359246998866389D-5, 1D-16,
     :           'iau_POM00', '23', STATUS )
      CALL VVD ( RPOM(3,1), -0.2550602379741215021D-6, 1D-16,
     :           'iau_POM00', '31', STATUS )
      CALL VVD ( RPOM(3,2), 0.1860359247002414021D-5, 1D-16,
     :           'iau_POM00', '32', STATUS )
      CALL VVD ( RPOM(3,3), 0.9999999999982370039D0, 1D-12,
     :           'iau_POM00', '33', STATUS )

      END

      SUBROUTINE T_iau_PPP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P P P
*  - - - - - - - - - -
*
*  Test iau_PPP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PPP, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), APB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL iau_PPP ( A, B, APB )

      CALL VVD ( APB(1) + APB(2) + APB(3), 15D0, 1D-12,
     :           'iau_PPP', ' ', STATUS )

      END

      SUBROUTINE T_iau_PPSP ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P P S P
*  - - - - - - - - - - -
*
*  Test iau_PPSP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PPSP, VVD
*
*  This revision:  2008 November 30
*-
      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), S, B(3), APSB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      S = 5D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL iau_PPSP ( A, S, B, APSB )

      CALL VVD ( APSB(1) + APSB(2) + APSB(3), 47D0, 1D-12,
     :           'iau_PPSP', ' ', STATUS )

      END

      SUBROUTINE T_iau_PR00 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P R 0 0
*  - - - - - - - - - - -
*
*  Test iau_PR00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PR00, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION DPSIPR, DEPSPR


      CALL iau_PR00 ( 2400000.5D0, 53736D0, DPSIPR, DEPSPR )

      CALL VVD ( DPSIPR, -0.8716465172668347629D-7, 1D-22,
     :           'iau_PR00', 'DPSIPR', STATUS )
      CALL VVD ( DEPSPR, -0.7342018386722813087D-8, 1D-22,
     :           'iau_PR00', 'DEPSPR', STATUS )

      END

      SUBROUTINE T_iau_PREC76 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P R E C 7 6
*  - - - - - - - - - - - - -
*
*  Test iau_PREC76 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PREC76, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION EP01, EP02, EP11, EP12, ZETA, Z, THETA


      EP01 = 2400000.5D0
      EP02 = 33282D0

      EP11 = 2400000.5D0
      EP12 = 51544D0

      CALL iau_PREC76 ( EP01, EP02, EP11, EP12, ZETA, Z, THETA )

      CALL VVD ( ZETA, 0.5588961642000161243D-2, 1D-12,
     :           'iau_PREC76', 'ZETA', STATUS )
      CALL VVD ( Z, 0.5589922365870680624D-2, 1D-12,
     :           'iau_PREC76', 'Z', STATUS )
      CALL VVD ( THETA, 0.4858945471687296760D-2, 1D-12,
     :           'iau_PREC76', 'THETA', STATUS )

      END

      SUBROUTINE T_iau_PV2P ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P V 2 P
*  - - - - - - - - - - -
*
*  Test iau_PV2P routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PV2P, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), P(3)


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL iau_PV2P ( PV, P )

      CALL VVD ( P(1), 0.3D0, 0D0, 'iau_PV2P', '1', STATUS )
      CALL VVD ( P(2), 1.2D0, 0D0, 'iau_PV2P', '2', STATUS )
      CALL VVD ( P(3), -2.5D0, 0D0, 'iau_PV2P', '3', STATUS )

      END

      SUBROUTINE T_iau_PV2S ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P V 2 S
*  - - - - - - - - - - -
*
*  Test iau_PV2S routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PV2S, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), THETA, PHI, R, TD, PD, RD


      PV(1,1) = -0.4514964673880165D0
      PV(2,1) = 0.03093394277342585D0
      PV(3,1) = 0.05594668105108779D0

      PV(1,2) = 1.292270850663260D-5
      PV(2,2) = 2.652814182060692D-6
      PV(3,2) = 2.568431853930293D-6

      CALL iau_PV2S ( PV, THETA, PHI, R, TD, PD, RD )

      CALL VVD ( THETA, 3.073185307179586515D0, 1D-12,
     :           'iau_PV2S', 'THETA', STATUS )
      CALL VVD ( PHI, 0.1229999999999999992D0, 1D-12,
     :           'iau_PV2S', 'PHI', STATUS )
      CALL VVD ( R, 0.4559999999999999757D0, 1D-12,
     :           'iau_PV2S', 'R', STATUS )
      CALL VVD ( TD, -0.7800000000000000364D-5, 1D-16,
     :           'iau_PV2S', 'TD', STATUS )
      CALL VVD ( PD, 0.9010000000000001639D-5, 1D-16,
     :           'iau_PV2S', 'PD', STATUS )
      CALL VVD ( RD, -0.1229999999999999832D-4, 1D-16,
     :           'iau_PV2S', 'RD', STATUS )

      END

      SUBROUTINE T_iau_PVDPV ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P V D P V
*  - - - - - - - - - - - -
*
*  Test iau_PVDPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVDPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), ADB(2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 6D0
      A(2,2) = 0D0
      A(3,2) = 4D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 0D0
      B(2,2) = 2D0
      B(3,2) = 8D0

      CALL iau_PVDPV ( A, B, ADB )

      CALL VVD ( ADB(1), 20D0, 1D-12, 'iau_PVDPV', '1', STATUS )
      CALL VVD ( ADB(2), 50D0, 1D-12, 'iau_PVDPV', '2', STATUS )

      END

      SUBROUTINE T_iau_PVM ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P V M
*  - - - - - - - - - -
*
*  Test iau_PVM routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVM, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), R, S


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = 0.45D0
      PV(2,2) = -0.25D0
      PV(3,2) = 1.1D0

      CALL iau_PVM ( PV, R, S )

      CALL VVD ( R, 2.789265136196270604D0, 1D-12,
     :           'iau_PVM', 'R', STATUS )

      CALL VVD ( S, 1.214495780149111922D0, 1D-12,
     :           'iau_PVM', 'S', STATUS )

      END

      SUBROUTINE T_iau_PVMPV ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P V M P V
*  - - - - - - - - - - - -
*
*  Test iau_PVMPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVMPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), AMB(3,2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 5D0
      A(2,2) = 6D0
      A(3,2) = 3D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 3D0
      B(2,2) = 2D0
      B(3,2) = 1D0

      CALL iau_PVMPV ( A, B, AMB )

      CALL VVD ( AMB(1,1), 1D0, 1D-12, 'iau_PVMPV', '11', STATUS )
      CALL VVD ( AMB(2,1), -1D0, 1D-12, 'iau_PVMPV', '21', STATUS )
      CALL VVD ( AMB(3,1), -1D0, 1D-12, 'iau_PVMPV', '31', STATUS )
      CALL VVD ( AMB(1,2), 2D0, 1D-12, 'iau_PVMPV', '12', STATUS )
      CALL VVD ( AMB(2,2), 4D0, 1D-12, 'iau_PVMPV', '22', STATUS )
      CALL VVD ( AMB(3,2), 2D0, 1D-12, 'iau_PVMPV', '32', STATUS )

      END

      SUBROUTINE T_iau_PVPPV ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P V P P V
*  - - - - - - - - - - - -
*
*  Test iau_PVPPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVPPV, VVD
*
*  This revision:  2008 November 20
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), APB(3,2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 5D0
      A(2,2) = 6D0
      A(3,2) = 3D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 3D0
      B(2,2) = 2D0
      B(3,2) = 1D0

      CALL iau_PVPPV ( A, B, APB )

      CALL VVD ( APB(1,1), 3D0, 1D-12, 'iau_PVPPV', '11', STATUS )
      CALL VVD ( APB(2,1), 5D0, 1D-12, 'iau_PVPPV', '21', STATUS )
      CALL VVD ( APB(3,1), 7D0, 1D-12, 'iau_PVPPV', '31', STATUS )
      CALL VVD ( APB(1,2), 8D0, 1D-12, 'iau_PVPPV', '12', STATUS )
      CALL VVD ( APB(2,2), 8D0, 1D-12, 'iau_PVPPV', '22', STATUS )
      CALL VVD ( APB(3,2), 4D0, 1D-12, 'iau_PVPPV', '32', STATUS )

      END

      SUBROUTINE T_iau_PVSTAR ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ P V S T A R
*  - - - - - - - - - - - - -
*
*  Test iau_PVSTAR routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVSTAR, VVD, VIV
*
*  This revision:  2009 November 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  PV(3,2), RA, DEC, PMR, PMD, PX, RV
      INTEGER J


      PV(1,1) = 126668.5912743160601D0
      PV(2,1) = 2136.792716839935195D0
      PV(3,1) = -245251.2339876830091D0

      PV(1,2) = -0.4051854035740712739D-2
      PV(2,2) = -0.6253919754866173866D-2
      PV(3,2) = 0.1189353719774107189D-1

      CALL iau_PVSTAR ( PV, RA, DEC, PMR, PMD, PX, RV, J )

      CALL VVD ( RA, 0.1686756D-1, 1D-12,
     :           'iau_PVSTAR', 'RA', STATUS )
      CALL VVD ( DEC, -1.093989828D0, 1D-12,
     :           'iau_PVSTAR', 'DEC', STATUS )
      CALL VVD ( PMR, -0.178323516D-4, 1D-16,
     :           'iau_PVSTAR', 'PMR', STATUS )
      CALL VVD ( PMD, 0.2336024047D-5, 1D-16,
     :           'iau_PVSTAR', 'PMD', STATUS )
      CALL VVD ( PX, 0.74723D0, 1D-12,
     :           'iau_PVSTAR', 'PX', STATUS )
      CALL VVD ( RV, -21.6D0, 1D-11,
     :           'iau_PVSTAR', 'RV', STATUS )
      CALL VIV ( J, 0, 'iau_PVSTAR', 'J', STATUS )

      END

      SUBROUTINE T_iau_PVU( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P V U
*  - - - - - - - - - -
*
*  Test iau_PVU routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVU, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), UPV(3,2)


      PV(1,1) = 126668.5912743160734D0
      PV(2,1) = 2136.792716839935565D0
      PV(3,1) = -245251.2339876830229D0

      PV(1,2) = -0.4051854035740713039D-2
      PV(2,2) = -0.6253919754866175788D-2
      PV(3,2) = 0.1189353719774107615D-1

      CALL iau_PVU ( 2920D0, PV, UPV )

      CALL VVD ( UPV(1,1), 126656.7598605317105D0, 1D-12,
     :           'iau_PVU', '11', STATUS )
      CALL VVD ( UPV(2,1), 2118.531271155726332D0, 1D-12,
     :           'iau_PVU', '21', STATUS )
      CALL VVD ( UPV(3,1), -245216.5048590656190D0, 1D-12,
     :           'iau_PVU', '31', STATUS )
      CALL VVD ( UPV(1,2), -0.4051854035740713039D-2, 1D-12,
     :           'iau_PVU', '12', STATUS )
      CALL VVD ( UPV(2,2), -0.6253919754866175788D-2, 1D-12,
     :           'iau_PVU', '22', STATUS )
      CALL VVD ( UPV(3,2), 0.1189353719774107615D-1, 1D-12,
     :           'iau_PVU', '32', STATUS )

      END

      SUBROUTINE T_iau_PVUP( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ P V U P
*  - - - - - - - - - - -
*
*  Test iau_PVUP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVUP, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2), P(3)


      PV(1,1) = 126668.5912743160734D0
      PV(2,1) = 2136.792716839935565D0
      PV(3,1) = -245251.2339876830229D0

      PV(1,2) = -0.4051854035740713039D-2
      PV(2,2) = -0.6253919754866175788D-2
      PV(3,2) = 0.1189353719774107615D-1

      CALL iau_PVUP ( 2920D0, PV, P )

      CALL VVD ( P(1), 126656.7598605317105D0, 1D-12,
     :           'iau_PVUP', '1', STATUS )
      CALL VVD ( P(2), 2118.531271155726332D0, 1D-12,
     :           'iau_PVUP', '2', STATUS )
      CALL VVD ( P(3), -245216.5048590656190D0, 1D-12,
     :           'iau_PVUP', '3', STATUS )

      END

      SUBROUTINE T_iau_PVXPV ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ P V X P V
*  - - - - - - - - - - - -
*
*  Test iau_PVXPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PVXPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,2), B(3,2), AXB(3,2)


      A(1,1) = 2D0
      A(2,1) = 2D0
      A(3,1) = 3D0

      A(1,2) = 6D0
      A(2,2) = 0D0
      A(3,2) = 4D0

      B(1,1) = 1D0
      B(2,1) = 3D0
      B(3,1) = 4D0

      B(1,2) = 0D0
      B(2,2) = 2D0
      B(3,2) = 8D0

      CALL iau_PVXPV ( A, B, AXB )

      CALL VVD ( AXB(1,1), -1D0, 1D-12, 'iau_PVXPV', '11', STATUS )
      CALL VVD ( AXB(2,1), -5D0, 1D-12, 'iau_PVXPV', '21', STATUS )
      CALL VVD ( AXB(3,1), 4D0, 1D-12, 'iau_PVXPV', '31', STATUS )
      CALL VVD ( AXB(1,2), -2D0, 1D-12, 'iau_PVXPV', '12', STATUS )
      CALL VVD ( AXB(2,2), -36D0, 1D-12, 'iau_PVXPV', '22', STATUS )
      CALL VVD ( AXB(3,2), 22D0, 1D-12, 'iau_PVXPV', '32', STATUS )

      END

      SUBROUTINE T_iau_PXP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ P X P
*  - - - - - - - - - -
*
*  Test iau_PXP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_PXP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), AXB(3)


      A(1) = 2D0
      A(2) = 2D0
      A(3) = 3D0

      B(1) = 1D0
      B(2) = 3D0
      B(3) = 4D0

      CALL iau_PXP ( A, B, AXB )

      CALL VVD ( AXB(1), -1D0, 1D-12, 'iau_PXP', '1', STATUS )
      CALL VVD ( AXB(2), -5D0, 1D-12, 'iau_PXP', '2', STATUS )
      CALL VVD ( AXB(3), 4D0, 1D-12, 'iau_PXP', '3', STATUS )

      END

      SUBROUTINE T_iau_RM2V ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ R M 2 V
*  - - - - - - - - - - -
*
*  Test iau_RM2V routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RM2V, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), W(3)


      R(1,1) = 0D0
      R(1,2) = -0.80D0
      R(1,3) = -0.60D0

      R(2,1) = 0.80D0
      R(2,2) = -0.36D0
      R(2,3) = 0.48D0

      R(3,1) = 0.60D0
      R(3,2) = 0.48D0
      R(3,3) = -0.64D0

      CALL iau_RM2V ( R, W )

      CALL VVD ( W(1), 0D0, 1D-12,
     :           'iau_RM2V', '1', STATUS )
      CALL VVD ( W(2), 1.413716694115406957D0, 1D-12,
     :           'iau_RM2V', '2', STATUS )
      CALL VVD ( W(3), -1.884955592153875943D0, 1D-12,
     :           'iau_RM2V', '3', STATUS )

      END

      SUBROUTINE T_iau_RV2M ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ R V 2 M
*  - - - - - - - - - - -
*
*  Test iau_RV2M routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RV2M, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  W(3), R(3,3)


      W(1) = 0D0
      W(2) = 1.41371669D0
      W(3) = -1.88495559D0

      CALL iau_RV2M ( W, R )

      CALL VVD ( R(1,1), -0.7071067782221119905D0, 1D-14,
     :           'iau_RV2M', '11', STATUS )
      CALL VVD ( R(1,2), -0.5656854276809129651D0, 1D-14,
     :           'iau_RV2M', '12', STATUS )
      CALL VVD ( R(1,3), -0.4242640700104211225D0, 1D-14,
     :           'iau_RV2M', '13', STATUS )
      CALL VVD ( R(2,1), 0.5656854276809129651D0, 1D-14,
     :           'iau_RV2M', '21', STATUS )
      CALL VVD ( R(2,2), -0.9254833945322742462D-1, 1D-14,
     :           'iau_RV2M', '22', STATUS )
      CALL VVD ( R(2,3), -0.8194112531408833269D0, 1D-14,
     :           'iau_RV2M', '23', STATUS )
      CALL VVD ( R(3,1), 0.4242640700104211225D0, 1D-14,
     :           'iau_RV2M', '31', STATUS )
      CALL VVD ( R(3,2), -0.8194112531408833269D0, 1D-14,
     :           'iau_RV2M', '32', STATUS )
      CALL VVD ( R(3,3), 0.3854415612311154341D0, 1D-14,
     :           'iau_RV2M', '33', STATUS )

      END

      SUBROUTINE T_iau_RX ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ R X
*  - - - - - - - - -
*
*  Test iau_RX routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RX, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PHI, R(3,3)


      PHI = 0.3456789D0

      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_RX ( PHI, R )

      CALL VVD ( R(1,1), 2D0, 0D0,
     :           'iau_RX', '11', STATUS )
      CALL VVD ( R(1,2), 3D0, 0D0,
     :           'iau_RX', '12', STATUS )
      CALL VVD ( R(1,3), 2D0, 0D0,
     :           'iau_RX', '13', STATUS )
      CALL VVD ( R(2,1), 3.839043388235612460D0, 1D-12,
     :           'iau_RX', '21', STATUS )
      CALL VVD ( R(2,2), 3.237033249594111899D0, 1D-12,
     :           'iau_RX', '22', STATUS )
      CALL VVD ( R(2,3), 4.516714379005982719D0, 1D-12,
     :           'iau_RX', '23', STATUS )
      CALL VVD ( R(3,1), 1.806030415924501684D0, 1D-12,
     :           'iau_RX', '31', STATUS )
      CALL VVD ( R(3,2), 3.085711545336372503D0, 1D-12,
     :           'iau_RX', '32', STATUS )
      CALL VVD ( R(3,3), 3.687721683977873065D0, 1D-12,
     :           'iau_RX', '33', STATUS )

      END

      SUBROUTINE T_iau_RXP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ R X P
*  - - - - - - - - - -
*
*  Test iau_RXP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RXP, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), P(3), RP(3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0
      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0
      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      P(1) = 0.2D0
      P(2) = 1.5D0
      P(3) = 0.1D0

      CALL iau_RXP ( R, P, RP )

      CALL VVD ( RP(1), 5.1D0, 1D-12, 'iau_RXP', '1', STATUS )
      CALL VVD ( RP(2), 3.9D0, 1D-12, 'iau_RXP', '2', STATUS )
      CALL VVD ( RP(3), 7.10D0, 1D-12, 'iau_RXP', '3', STATUS )

      END

      SUBROUTINE T_iau_RXPV ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ R X P V
*  - - - - - - - - - - -
*
*  Test iau_RXPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RXPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), PV(3,2), RPV(3,2)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      PV(1,1) = 0.2D0
      PV(2,1) = 1.5D0
      PV(3,1) = 0.1D0

      PV(1,2) = 1.5D0
      PV(2,2) = 0.2D0
      PV(3,2) = 0.1D0

      CALL iau_RXPV ( R, PV, RPV )

      CALL VVD ( RPV(1,1), 5.1D0, 1D-12, 'iau_RXPV', '11', STATUS )
      CALL VVD ( RPV(1,2), 3.8D0, 1D-12, 'iau_RXPV', '12', STATUS )
      CALL VVD ( RPV(2,1), 3.9D0, 1D-12, 'iau_RXPV', '21', STATUS )
      CALL VVD ( RPV(2,2), 5.2D0, 1D-12, 'iau_RXPV', '22', STATUS )
      CALL VVD ( RPV(3,1), 7.1D0, 1D-12, 'iau_RXPV', '31', STATUS )
      CALL VVD ( RPV(3,2), 5.8D0, 1D-12, 'iau_RXPV', '32', STATUS )

      END

      SUBROUTINE T_iau_RXR ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ R X R
*  - - - - - - - - - -
*
*  Test iau_RXR routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RXR, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3,3), B(3,3), ATB(3,3)


      A(1,1) = 2D0
      A(1,2) = 3D0
      A(1,3) = 2D0

      A(2,1) = 3D0
      A(2,2) = 2D0
      A(2,3) = 3D0

      A(3,1) = 3D0
      A(3,2) = 4D0
      A(3,3) = 5D0

      B(1,1) = 1D0
      B(1,2) = 2D0
      B(1,3) = 2D0

      B(2,1) = 4D0
      B(2,2) = 1D0
      B(2,3) = 1D0

      B(3,1) = 3D0
      B(3,2) = 0D0
      B(3,3) = 1D0

      CALL iau_RXR ( A, B, ATB )

      CALL VVD ( ATB(1,1), 20D0, 1D-12, 'iau_RXR', '11', STATUS )
      CALL VVD ( ATB(1,2), 7D0, 1D-12, 'iau_RXR', '12', STATUS )
      CALL VVD ( ATB(1,3), 9D0, 1D-12, 'iau_RXR', '13', STATUS )
      CALL VVD ( ATB(2,1), 20D0, 1D-12, 'iau_RXR', '21', STATUS )
      CALL VVD ( ATB(2,2), 8D0, 1D-12, 'iau_RXR', '22', STATUS )
      CALL VVD ( ATB(2,3), 11D0, 1D-12, 'iau_RXR', '23', STATUS )
      CALL VVD ( ATB(3,1), 34D0, 1D-12, 'iau_RXR', '31', STATUS )
      CALL VVD ( ATB(3,2), 10D0, 1D-12, 'iau_RXR', '32', STATUS )
      CALL VVD ( ATB(3,3), 15D0, 1D-12, 'iau_RXR', '33', STATUS )

      END

      SUBROUTINE T_iau_RY ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ R Y
*  - - - - - - - - -
*
*  Test iau_RY routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RY, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION THETA, R(3,3)


      THETA = 0.3456789D0

      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_RY ( THETA, R )

      CALL VVD ( R(1,1), 0.8651847818978159930D0, 1D-12,
     :           'iau_RY', '11', STATUS )
      CALL VVD ( R(1,2), 1.467194920539316554D0, 1D-12,
     :           'iau_RY', '12', STATUS )
      CALL VVD ( R(1,3), 0.1875137911274457342D0, 1D-12,
     :           'iau_RY', '13', STATUS )
      CALL VVD ( R(2,1), 3D0, 1D-12,
     :           'iau_RY', '21', STATUS )
      CALL VVD ( R(2,2), 2D0, 1D-12,
     :           'iau_RY', '22', STATUS )
      CALL VVD ( R(2,3), 3D0, 1D-12,
     :           'iau_RY', '23', STATUS )
      CALL VVD ( R(3,1), 3.500207892850427330D0, 1D-12,
     :           'iau_RY', '31', STATUS )
      CALL VVD ( R(3,2), 4.779889022262298150D0, 1D-12,
     :           'iau_RY', '32', STATUS )
      CALL VVD ( R(3,3), 5.381899160903798712D0, 1D-12,
     :           'iau_RY', '33', STATUS )

      END

      SUBROUTINE T_iau_RZ ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ R Z
*  - - - - - - - - -
*
*  Test iau_RZ routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_RZ, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PSI, R(3,3)


      PSI = 0.3456789D0

      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_RZ ( PSI, R )

      CALL VVD ( R(1,1), 2.898197754208926769D0, 1D-12,
     :           'iau_RZ', '11', STATUS )
      CALL VVD ( R(1,2), 3.500207892850427330D0, 1D-12,
     :           'iau_RZ', '12', STATUS )
      CALL VVD ( R(1,3), 2.898197754208926769D0, 1D-12,
     :           'iau_RZ', '13', STATUS )
      CALL VVD ( R(2,1), 2.144865911309686813D0, 1D-12,
     :           'iau_RZ', '21', STATUS )
      CALL VVD ( R(2,2), 0.8651847818978159930D0, 1D-12,
     :           'iau_RZ', '22', STATUS )
      CALL VVD ( R(2,3), 2.144865911309686813D0, 1D-12,
     :           'iau_RZ', '23', STATUS )
      CALL VVD ( R(3,1), 3D0, 1D-12,
     :           'iau_RZ', '31', STATUS )
      CALL VVD ( R(3,2), 4D0, 1D-12,
     :           'iau_RZ', '32', STATUS )
      CALL VVD ( R(3,3), 5D0, 1D-12,
     :           'iau_RZ', '33', STATUS )

      END

      SUBROUTINE T_iau_S00A ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S 0 0 A
*  - - - - - - - - - - -
*
*  Test iau_S00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_S00A


      CALL VVD ( iau_S00A ( 2400000.5D0, 52541D0 ),
     :           -0.1340684448919163584D-7, 1D-18,
     :           'iau_S00A', ' ', STATUS )

      END

      SUBROUTINE T_iau_S00B ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S 0 0 B
*  - - - - - - - - - - -
*
*  Test iau_S00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S00B, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_S00B


      CALL VVD ( iau_S00B ( 2400000.5D0, 52541D0 ),
     :           -0.1340695782951026584D-7, 1D-18,
     :           'iau_S00B', ' ', STATUS )

      END

      SUBROUTINE T_iau_S00 ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ S 0 0
*  - - - - - - - - - -
*
*  Test iau_S00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S00, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, iau_S00


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      CALL VVD ( iau_S00 ( 2400000.5D0, 53736D0, X, Y ),
     :           -0.1220036263270905693D-7, 1D-18,
     :           'iau_S00', ' ', STATUS )

      END

      SUBROUTINE T_iau_S06A ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S 0 6 A
*  - - - - - - - - - - -
*
*  Test iau_S06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_S06A


      CALL VVD ( iau_S06A ( 2400000.5D0, 52541D0 ),
     :           -0.1340680437291812383D-7, 1D-18,
     :           'iau_S06A', ' ', STATUS )

      END

      SUBROUTINE T_iau_S06 ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ S 0 6
*  - - - - - - - - - -
*
*  Test iau_S06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, iau_S06


      X = 0.5791308486706011000D-3
      Y = 0.4020579816732961219D-4

      CALL VVD ( iau_S06 ( 2400000.5D0, 53736D0, X, Y ),
     :           -0.1220032213076463117D-7, 1D-18,
     :           'iau_S06', ' ', STATUS )

      END

      SUBROUTINE T_iau_S2C ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ S 2 C
*  - - - - - - - - - -
*
*  Test iau_S2C routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S2C, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION C(3)


      CALL iau_S2C ( 3.0123D0, -0.999D0, C )

      CALL VVD ( C(1), -0.5366267667260523906D0, 1D-12,
     :           'iau_S2C', '1', STATUS )
      CALL VVD ( C(2), 0.6977111097651453650D-1, 1D-12,
     :           'iau_S2C', '2', STATUS )
      CALL VVD ( C(3), -0.8409302618566214041D0, 1D-12,
     :           'iau_S2C', '3', STATUS )

      END

      SUBROUTINE T_iau_S2P ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ S 2 P
*  - - - - - - - - - -
*
*  Test iau_S2P routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S2P, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3)


      CALL iau_S2P ( -3.21D0, 0.123D0, 0.456D0, P )

      CALL VVD ( P(1), -0.4514964673880165228D0, 1D-12,
     :           'iau_S2P', 'X', STATUS )
      CALL VVD ( P(2),  0.3093394277342586880D-1, 1D-12,
     :           'iau_S2P', 'Y', STATUS )
      CALL VVD ( P(3),  0.5594668105108779333D-1, 1D-12,
     :           'iau_S2P', 'Z', STATUS )

      END

      SUBROUTINE T_iau_S2PV ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S 2 P V
*  - - - - - - - - - - -
*
*  Test iau_S2PV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S2PV, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2)


      CALL iau_S2PV ( -3.21D0, 0.123D0, 0.456D0, -7.8D-6, 9.01D-6,
     :                -1.23D-5, PV )

      CALL VVD ( PV(1,1), -0.4514964673880165228D0, 1D-12,
     :           'iau_S2PV', 'X', STATUS )
      CALL VVD ( PV(2,1),  0.3093394277342586880D-1, 1D-12,
     :           'iau_S2PV', 'Y', STATUS )
      CALL VVD ( PV(3,1),  0.5594668105108779333D-1, 1D-12,
     :           'iau_S2PV', 'Z', STATUS )
      CALL VVD ( PV(1,2),  0.1292270850663260170D-4, 1D-16,
     :           'iau_S2PV', 'VX', STATUS )
      CALL VVD ( PV(2,2),  0.2652814182060691422D-5, 1D-16,
     :           'iau_S2PV', 'VY', STATUS )
      CALL VVD ( PV(3,2),  0.2568431853930292259D-5, 1D-16,
     :           'iau_S2PV', 'VZ', STATUS )

      END

      SUBROUTINE T_iau_S2XPV ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ S 2 X P V
*  - - - - - - - - - - - -
*
*  Test iau_S2XPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_S2XPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION S1, S2, PV(3,2), SPV(3,2)


      S1 = 2D0
      S2 = 3D0

      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = 0.5D0
      PV(2,2) = 2.3D0
      PV(3,2) = -0.4D0

      CALL iau_S2XPV ( S1, S2, PV, SPV )

      CALL VVD ( SPV(1,1), 0.6D0, 1D-12, 'iau_S2XPV', 'P1', STATUS )
      CALL VVD ( SPV(2,1), 2.4D0, 1D-12, 'iau_S2XPV', 'P2', STATUS )
      CALL VVD ( SPV(3,1), -5.0D0, 1D-12, 'iau_S2XPV', 'P3', STATUS )
      CALL VVD ( SPV(1,2), 1.5D0, 1D-12, 'iau_S2XPV', 'V1', STATUS )
      CALL VVD ( SPV(2,2), 6.9D0, 1D-12, 'iau_S2XPV', 'V2', STATUS )
      CALL VVD ( SPV(3,2), -1.2D0, 1D-12, 'iau_S2XPV', 'V3', STATUS )

      END

      SUBROUTINE T_iau_SEPP ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S E P P
*  - - - - - - - - - - -
*
*  Test iau_SEPP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_SEPP, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A(3), B(3), S


      A(1) = 1D0
      A(2) = 0.1D0
      A(3) = 0.2D0

      B(1) = -3D0
      B(2) = 1D-3
      B(3) = 0.2D0

      CALL iau_SEPP ( A, B, S )

      CALL VVD ( S, 2.860391919024660768D0, 1D-12,
     :           'iau_SEPP', ' ', STATUS )

      END

      SUBROUTINE T_iau_SEPS ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S E P S
*  - - - - - - - - - - -
*
*  Test iau_SEPS routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_SEPS, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION AL, AP, BL, BP, S


      AL = 1D0
      AP = 0.1D0

      BL = 0.2D0
      BP = -3D0

      CALL iau_SEPS ( AL, AP, BL, BP, S )

      CALL VVD ( S, 2.346722016996998842D0, 1D-14,
     :           'iau_SEPS', ' ', STATUS )

      END

      SUBROUTINE T_iau_SP00 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S P 0 0
*  - - - - - - - - - - -
*
*  Test iau_SP00 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_SP00, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION iau_SP00


      CALL VVD ( iau_SP00 (2400000.5D0, 52541D0),
     :           -0.6216698469981019309D-11, 1D-12,
     :           'iau_SP00', ' ', STATUS )

      END

      SUBROUTINE T_iau_STARPM ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ S T A R P M
*  - - - - - - - - - - - - -
*
*  Test iau_STARPM routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_STARPM, VVD, VIV
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RA1, DEC1, PMR1, PMD1, PX1, RV1,
     :                 RA2, DEC2, PMR2, PMD2, PX2, RV2
      INTEGER J


      RA1 = 0.01686756D0
      DEC1 = -1.093989828D0
      PMR1 = -1.78323516D-5
      PMD1 = 2.336024047D-6
      PX1 = 0.74723D0
      RV1 = -21.6D0

      CALL iau_STARPM ( RA1, DEC1, PMR1, PMD1, PX1, RV1,
     :                  2400000.5D0, 50083D0, 2400000.5D0, 53736D0,
     :                  RA2, DEC2, PMR2, PMD2, PX2, RV2, J )

      CALL VVD ( RA2, 0.1668919069414242368D-1, 1D-13,
     :           'iau_STARPM', 'RA', STATUS )
      CALL VVD ( DEC2, -1.093966454217127879D0, 1D-13,
     :           'iau_STARPM', 'DEC', STATUS )
      CALL VVD ( PMR2, -0.1783662682155932702D-4, 1D-17,
     :           'iau_STARPM', 'PMR', STATUS )
      CALL VVD ( PMD2, 0.2338092915987603664D-5, 1D-17,
     :           'iau_STARPM', 'PMD', STATUS )
      CALL VVD ( PX2, 0.7473533835323493644D0, 1D-13,
     :           'iau_STARPM', 'PX', STATUS )
      CALL VVD ( RV2, -21.59905170476860786D0, 1D-11,
     :           'iau_STARPM', 'RV', STATUS )
      CALL VIV ( J, 0, 'iau_STARPM', 'J', STATUS )

      END

      SUBROUTINE T_iau_STARPV ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ S T A R P V
*  - - - - - - - - - - - - -
*
*  Test iau_STARPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_STARPV, VVD, VIV
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION RA, DEC, PMR, PMD, PX, RV, PV(3,2)
      INTEGER J


      RA = 0.01686756D0
      DEC = -1.093989828D0
      PMR = -1.78323516D-5
      PMD = 2.336024047D-6
      PX = 0.74723D0
      RV = -21.6D0

      CALL iau_STARPV ( RA, DEC, PMR, PMD, PX, RV, PV, J )

      CALL VVD ( PV(1,1), 126668.5912743160601D0, 1D-10,
     :           'iau_STARPV', '11', STATUS )
      CALL VVD ( PV(2,1), 2136.792716839935195D0, 1D-12,
     :           'iau_STARPV', '21', STATUS )
      CALL VVD ( PV(3,1), -245251.2339876830091D0, 1D-10,
     :           'iau_STARPV', '31', STATUS )
      CALL VVD ( PV(1,2), -0.4051854035740712739D-2, 1D-13,
     :           'iau_STARPV', '12', STATUS )
      CALL VVD ( PV(2,2), -0.6253919754866173866D-2, 1D-15,
     :           'iau_STARPV', '22', STATUS )
      CALL VVD ( PV(3,2), 0.1189353719774107189D-1, 1D-13,
     :           'iau_STARPV', '32', STATUS )
      CALL VIV ( J, 0, 'iau_STARPV', 'J', STATUS )

      END

      SUBROUTINE T_iau_SXP ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ S X P
*  - - - - - - - - - -
*
*  Test iau_SXP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_SXP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION S, P(3), SP(3)


      S = 2D0

      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL iau_SXP ( S, P, SP )

      CALL VVD ( SP(1), 0.6D0, 0D0, 'iau_SXP', '1', STATUS )
      CALL VVD ( SP(2), 2.4D0, 0D0, 'iau_SXP', '2', STATUS )
      CALL VVD ( SP(3), -5D0, 0D0, 'iau_SXP', '3', STATUS )

      END


      SUBROUTINE T_iau_SXPV ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ S X P V
*  - - - - - - - - - - -
*
*  Test iau_SXPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_SXPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION S, PV(3,2), SPV(3,2)


      S = 2D0

      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = 0.5D0
      PV(2,2) = 3.2D0
      PV(3,2) = -0.7D0

      CALL iau_SXPV ( S, PV, SPV )

      CALL VVD ( SPV(1,1), 0.6D0, 0D0, 'iau_SXPV', 'P1', STATUS )
      CALL VVD ( SPV(2,1), 2.4D0, 0D0, 'iau_SXPV', 'P2', STATUS )
      CALL VVD ( SPV(3,1), -5D0, 0D0, 'iau_SXPV', 'P3', STATUS )
      CALL VVD ( SPV(1,2), 1D0, 0D0, 'iau_SXPV', 'V1', STATUS )
      CALL VVD ( SPV(2,2), 6.4D0, 0D0, 'iau_SXPV', 'V2', STATUS )
      CALL VVD ( SPV(3,2), -1.4D0, 0D0, 'iau_SXPV', 'V3', STATUS )

      END

      SUBROUTINE T_iau_TAITT ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T A I T T
*  - - - - - - - - - - - -
*
*  Test iau_TAITT routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TAITT, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL iau_TAITT ( 2453750.5D0, 0.892482639D0, T1, T2, J )

      CALL VVD ( T1, 2453750.5D0, 1D-6, 'iau_TAITT', 'T1', STATUS )
      CALL VVD ( T2, 0.892855139D0, 1D-12, 'iau_TAITT', 'T2', STATUS )
      CALL VIV ( J, 0, 'iau_TAITT', 'J', STATUS )

      END

      SUBROUTINE T_iau_TAIUT1 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ T A I U T 1
*  - - - - - - - - - - - - -
*
*  Test iau_TAIUT1 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TAIUT1, VVD, VIV
*
*  This revision:  2010 September 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_TAIUT1 ( 2453750.5D0, 0.892482639D0, -32.6659D0,
     :                  U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'iau_TAIUT1', 'U1', STATUS )
      CALL VVD ( U2, 0.8921045614537037037D0, 1D-12,
     :           'iau_TAIUT1', 'U2', STATUS )
      CALL VIV ( J, 0, 'iau_TAIUT1', 'J', STATUS )

      END

      SUBROUTINE T_iau_TAIUTC ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ T A I U T C
*  - - - - - - - - - - - - -
*
*  Test iau_TAIUTC routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TAIUTC, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_TAIUTC ( 2453750.5D0, 0.892482639D0, U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'iau_TAIUTC', 'U1', STATUS )
      CALL VVD ( U2, 0.8921006945555555556D0, 1D-12,
     :           'iau_TAIUTC', 'U2', STATUS )
      CALL VIV ( J, 0, 'iau_TAIUTC', 'J', STATUS )

      END

      SUBROUTINE T_iau_TCBTDB ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ T C B T D B
*  - - - - - - - - - - - - -
*
*  Test iau_TCBTDB routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TCBTDB, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION B1, B2
      INTEGER J


      CALL iau_TCBTDB ( 2453750.5D0, 0.893019599D0, B1, B2, J )

      CALL VVD ( B1, 2453750.5D0, 1D-6, 'iau_TCBTDB', 'B1', STATUS )
      CALL VVD ( B2, 0.8928551362746343397D0, 1D-12,
     :           'iau_TCBTDB', 'B2', STATUS )
      CALL VIV ( J, 0, 'iau_TCBTDB', 'J', STATUS )

      END

      SUBROUTINE T_iau_TCGTT ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T C G T T
*  - - - - - - - - - - - -
*
*  Test iau_TCGTT routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TCGTT, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL iau_TCGTT ( 2453750.5D0, 0.892862531D0, T1, T2, J )

      CALL VVD ( T1, 2453750.5D0, 1D-6, 'iau_TCGTT', 'T1', STATUS )
      CALL VVD ( T2, 0.8928551387488816828D0, 1D-12,
     :           'iau_TCGTT', 'T2', STATUS )
      CALL VIV ( J, 0, 'iau_TCGTT', 'J', STATUS )

      END

      SUBROUTINE T_iau_TDBTCB ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ T D B T C B
*  - - - - - - - - - - - - -
*
*  Test iau_TDBTCB routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TDBTCB, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION B1, B2
      INTEGER J


      CALL iau_TDBTCB ( 2453750.5D0, 0.892855137D0, B1, B2, J )

      CALL VVD ( B1, 2453750.5D0, 1D-6, 'iau_TDBTCB', 'B1', STATUS )
      CALL VVD ( B2, 0.8930195997253656716D0, 1D-12,
     :           'iau_TDBTCB', 'B2', STATUS )
      CALL VIV ( J, 0, 'iau_TDBTCB', 'J', STATUS )

      END

      SUBROUTINE T_iau_TDBTT ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T D B T T
*  - - - - - - - - - - - -
*
*  Test iau_TDBTT routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TDBTT, VVD, VIV
*
*  This revision:  2010 September 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL iau_TDBTT ( 2453750.5D0, 0.892855137D0, -0.000201D0,
     :                 T1, T2, J )

      CALL VVD ( T1,  2453750.5D0, 1D-6, 'iau_TDBTT', 'T1', STATUS )
      CALL VVD ( T2, 0.8928551393263888889D0, 1D-12,
     :           'iau_TDBTT', 'T2', STATUS )
      CALL VIV ( J, 0, 'iau_TDBTT', 'J', STATUS )

      END

      SUBROUTINE T_iau_TF2A ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ T F 2 A
*  - - - - - - - - - - -
*
*  Test iau_TF2A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TF2A, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A
      INTEGER J


      CALL iau_TF2A ( '+', 4, 58, 20.2D0, A, J )

      CALL VVD ( A, 1.301739278189537429D0, 1D-12,
     :           'iau_TF2A', 'A', STATUS )
      CALL VIV ( J, 0, 'iau_TF2A', 'J', STATUS )

      END

      SUBROUTINE T_iau_TF2D ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ T F 2 D
*  - - - - - - - - - - -
*
*  Test iau_TF2D routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TF2D, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION D
      INTEGER J


      CALL iau_TF2D ( ' ', 23, 55, 10.9D0, D, J )

      CALL VVD ( D, 0.9966539351851851852D0, 1D-12,
     :           'iau_TF2D', 'D', STATUS )
      CALL VIV ( J, 0, 'iau_TF2D', 'J', STATUS )

      END

      SUBROUTINE T_iau_TR ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ T R
*  - - - - - - - - -
*
*  Test iau_TR routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TR, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), RT(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_TR ( R, RT )

      CALL VVD ( RT(1,1), 2D0, 0D0, 'iau_TR', '11', STATUS )
      CALL VVD ( RT(1,2), 3D0, 0D0, 'iau_TR', '12', STATUS )
      CALL VVD ( RT(1,3), 3D0, 0D0, 'iau_TR', '13', STATUS )
      CALL VVD ( RT(2,1), 3D0, 0D0, 'iau_TR', '21', STATUS )
      CALL VVD ( RT(2,2), 2D0, 0D0, 'iau_TR', '22', STATUS )
      CALL VVD ( RT(2,3), 4D0, 0D0, 'iau_TR', '23', STATUS )
      CALL VVD ( RT(3,1), 2D0, 0D0, 'iau_TR', '31', STATUS )
      CALL VVD ( RT(3,2), 3D0, 0D0, 'iau_TR', '32', STATUS )
      CALL VVD ( RT(3,3), 5D0, 0D0, 'iau_TR', '33', STATUS )

      END

      SUBROUTINE T_iau_TRXP ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ T R X P
*  - - - - - - - - - - -
*
*  Test iau_TRXP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TRXP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), P(3), TRP(3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      P(1) = 0.2D0
      P(2) = 1.5D0
      P(3) = 0.1D0

      CALL iau_TRXP ( R, P, TRP )

      CALL VVD ( TRP(1), 5.2D0, 1D-12, 'iau_TRXP', '1', STATUS )
      CALL VVD ( TRP(2), 4D0, 1D-12, 'iau_TRXP', '2', STATUS )
      CALL VVD ( TRP(3), 5.4D0, 1D-12, 'iau_TRXP', '3', STATUS )

      END

      SUBROUTINE T_iau_TRXPV ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T R X P V
*  - - - - - - - - - - - -
*
*  Test iau_TRXPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TRXPV, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION R(3,3), PV(3,2), TRPV(3,2)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      PV(1,1) = 0.2D0
      PV(2,1) = 1.5D0
      PV(3,1) = 0.1D0

      PV(1,2) = 1.5D0
      PV(2,2) = 0.2D0
      PV(3,2) = 0.1D0

      CALL iau_TRXPV ( R, PV, TRPV )

      CALL VVD ( TRPV(1,1), 5.2D0, 1D-12, 'iau_TRXPV', 'P1', STATUS )
      CALL VVD ( TRPV(2,1), 4D0,   1D-12, 'iau_TRXPV', 'P2', STATUS )
      CALL VVD ( TRPV(3,1), 5.4D0, 1D-12, 'iau_TRXPV', 'P3', STATUS )
      CALL VVD ( TRPV(1,2), 3.9D0, 1D-12, 'iau_TRXPV', 'V1', STATUS )
      CALL VVD ( TRPV(2,2), 5.3D0, 1D-12, 'iau_TRXPV', 'V2', STATUS )
      CALL VVD ( TRPV(3,2), 4.1D0, 1D-12, 'iau_TRXPV', 'V3', STATUS )

      END

      SUBROUTINE T_iau_TTTAI ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T T T A I
*  - - - - - - - - - - - -
*
*  Test iau_TTTAI routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TTTAI, VVD, VIV
*
*  This revision:  2010 September 7
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A1, A2
      INTEGER J


      CALL iau_TTTAI ( 2453750.5D0, 0.892482639D0, A1, A2, J )

      CALL VVD ( A1, 2453750.5D0, 1D-6, 'iau_TTTAI', 'A1', STATUS )
      CALL VVD ( A2, 0.892110139D0, 1D-12,
     :           'iau_TTTAI', 'A2', STATUS )
      CALL VIV ( J, 0, 'iau_TTTAI', 'J', STATUS )

      END

      SUBROUTINE T_iau_TTTCG ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T T T C G
*  - - - - - - - - - - - -
*
*  Test iau_TTTCG routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TTTCG, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION G1, G2
      INTEGER J


      CALL iau_TTTCG ( 2453750.5D0, 0.892482639D0, G1, G2, J )

      CALL VVD ( G1, 2453750.5D0, 1D-6, 'iau_TTTCG', 'G1', STATUS )
      CALL VVD ( G2, 0.8924900312508587113D0, 1D-12,
     :           'iau_TTTCG', 'G2', STATUS )
      CALL VIV ( J, 0, 'iau_TTTCG', 'J', STATUS )

      END

      SUBROUTINE T_iau_TTTDB ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T T T D B
*  - - - - - - - - - - - -
*
*  Test iau_TTTDB routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TTTDB, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION B1, B2
      INTEGER J


      CALL iau_TTTDB ( 2453750.5D0, 0.892855139D0, -0.000201D0,
     :                 B1, B2, J )

      CALL VVD ( B1, 2453750.5D0, 1D-6, 'iau_TTTDB', 'B1', STATUS )
      CALL VVD ( B2, 0.8928551366736111111D0, 1D-12,
     :           'iau_TTTDB', 'B2', STATUS )
      CALL VIV ( J, 0, 'iau_TTTDB', 'J', STATUS )

      END

      SUBROUTINE T_iau_TTUT1 ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ T T U T 1
*  - - - - - - - - - - - -
*
*  Test iau_TTUT1 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_TTUT1, VVD, VIV
*
*  This revision:  2010 September 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_TTUT1 ( 2453750.5D0, 0.892855139D0, 64.8499D0,
     :                 U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'iau_TTUT1', 'U1', STATUS )
      CALL VVD ( U2, 0.8921045614537037037D0, 1D-12,
     :           'iau_TTUT1', 'U2', STATUS )
      CALL VIV ( J, 0, 'iau_TTUT1', 'J', STATUS )

      END

      SUBROUTINE T_iau_UT1TAI ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ U T 1 T A I
*  - - - - - - - - - - - - -
*
*  Test iau_UT1TAI routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_UT1TAI, VVD, VIV
*
*  This revision:  2010 September 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION A1, A2
      INTEGER J


      CALL iau_UT1TAI ( 2453750.5D0, 0.892104561D0, -32.6659D0,
     :                  A1, A2, J )

      CALL VVD ( A1, 2453750.5D0, 1D-6, 'iau_UT1TAI', 'A1', STATUS )
      CALL VVD ( A2, 0.8924826385462962963D0, 1D-12,
     :           'iau_UT1TAI', 'A2', STATUS )
      CALL VIV ( J, 0, 'iau_UT1TAI', 'J', STATUS )

      END

      SUBROUTINE T_iau_UT1TT ( STATUS )
*+
*  - - - - - - - - - - - -
*   T _ i a u _ U T 1 T T
*  - - - - - - - - - - - -
*
*  Test iau_UT1TT routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_UT1TT, VVD, VIV
*
*  This revision:  2010 September 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION T1, T2
      INTEGER J


      CALL iau_UT1TT ( 2453750.5D0, 0.892104561D0, 64.8499D0,
     :                 T1, T2, J )

      CALL VVD ( T1, 2453750.5D0, 1D-6, 'iau_UT1TT', 'T1', STATUS )
      CALL VVD ( T2, 0.8928551385462962963D0, 1D-12,
     :           'iau_UT1TT', 'T2', STATUS )
      CALL VIV ( J, 0, 'iau_UT1TT', 'J', STATUS )

      END

      SUBROUTINE T_iau_UT1UTC ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ U T 1 U T C
*  - - - - - - - - - - - - -
*
*  Test iau_UT1UTC routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_UT1UTC, VVD, VIV
*
*  This revision:  2010 September 6
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_UT1UTC ( 2453750.5D0, 0.892104561D0, 0.3341D0,
     :                  U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'iau_UT1UTC', 'U1', STATUS )
      CALL VVD ( U2, 0.8921006941018518519D0, 1D-12,
     :           'iau_UT1UTC', 'U2', STATUS )
      CALL VIV ( J, 0, 'iau_UT1UTC', 'J', STATUS )

      END

      SUBROUTINE T_iau_UTCTAI ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ U T C T A I
*  - - - - - - - - - - - - -
*
*  Test iau_UTCTAI routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_UTCTAI, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_UTCTAI ( 2453750.5D0, 0.892100694D0, U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'iau_UTCTAI', 'U1', STATUS )
      CALL VVD ( U2, 0.8924826384444444444D0, 1D-12,
     :           'iau_UTCTAI', 'U2', STATUS )
      CALL VIV ( J, 0, 'iau_UTCTAI', 'J', STATUS )

      END

      SUBROUTINE T_iau_UTCUT1 ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ U T C U T 1
*  - - - - - - - - - - - - -
*
*  Test iau_UTCUT1 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_UTCUT1, VVD, VIV
*
*  This revision:  2010 September 5
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION U1, U2
      INTEGER J


      CALL iau_UTCUT1 ( 2453750.5D0, 0.892100694D0, 0.3341D0,
     :                  U1, U2, J )

      CALL VVD ( U1, 2453750.5D0, 1D-6, 'iau_UTCUT1', 'U1', STATUS )
      CALL VVD ( U2, 0.8921045608981481481D0, 1D-12,
     :           'iau_UTCUT1', 'U2', STATUS )
      CALL VIV ( J, 0, 'iau_UTCUT1', 'J', STATUS )

      END

      SUBROUTINE T_iau_XY06 ( STATUS )
*+
*  - - - - - - - - - - -
*   T _ i a u _ X Y 0 6
*  - - - - - - - - - - -
*
*  Test iau_XY06 routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_XY06, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y


      CALL iau_XY06 ( 2400000.5D0, 53736D0, X, Y )

      CALL VVD ( X, 0.5791308486706010975D-3, 1D-15,
     :           'iau_XY06', 'X', STATUS )
      CALL VVD ( Y, 0.4020579816732958141D-4, 1D-16,
     :           'iau_XY06', 'Y', STATUS )

      END

      SUBROUTINE T_iau_XYS00A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ X Y S 0 0 A
*  - - - - - - - - - - - - -
*
*  Test iau_XYS00A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_XYS00A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION X, Y, S


      CALL iau_XYS00A ( 2400000.5D0, 53736D0, X, Y, S )

      CALL VVD ( X, 0.5791308472168152904D-3, 1D-14,
     :           'iau_XYS00A', 'X', STATUS )
      CALL VVD ( Y, 0.4020595661591500259D-4, 1D-15,
     :           'iau_XYS00A', 'Y', STATUS )
      CALL VVD ( S, -0.1220040848471549623D-7, 1D-18,
     :           'iau_XYS00A', 'S', STATUS )

      END

      SUBROUTINE T_iau_XYS00B ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ X Y S 0 0 B
*  - - - - - - - - - - - - -
*
*  Test iau_XYS00B routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_XYS00B, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  X, Y, S


      CALL iau_XYS00B ( 2400000.5D0, 53736D0, X, Y, S )

      CALL VVD ( X, 0.5791301929950208873D-3, 1D-14,
     :           'iau_XYS00B', 'X', STATUS )
      CALL VVD ( Y, 0.4020553681373720832D-4, 1D-15,
     :           'iau_XYS00B', 'Y', STATUS )
      CALL VVD ( S, -0.1220027377285083189D-7, 1D-18,
     :           'iau_XYS00B', 'S', STATUS )

      END

      SUBROUTINE T_iau_XYS06A ( STATUS )
*+
*  - - - - - - - - - - - - -
*   T _ i a u _ X Y S 0 6 A
*  - - - - - - - - - - - - -
*
*  Test iau_XYS06A routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_XYS06A, VVD
*
*  This revision:  2009 July 11
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  X, Y, S


      CALL iau_XYS06A ( 2400000.5D0, 53736D0, X, Y, S )

      CALL VVD ( X, 0.5791308482835292617D-3, 1D-14,
     :           'iau_XYS06A', 'X', STATUS )
      CALL VVD ( Y, 0.4020580099454020310D-4, 1D-15,
     :           'iau_XYS06A', 'Y', STATUS )
      CALL VVD ( S, -0.1220032294164579896D-7, 1D-18,
     :           'iau_XYS06A', 'S', STATUS )

      END

      SUBROUTINE T_iau_ZP ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ Z P
*  - - - - - - - - -
*
*  Test iau_ZP routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_ZP, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION P(3)


      P(1) = 0.3D0
      P(2) = 1.2D0
      P(3) = -2.5D0

      CALL iau_ZP ( P )

      CALL VVD ( P(1), 0D0, 0D0, 'iau_ZP', '1', STATUS )
      CALL VVD ( P(2), 0D0, 0D0, 'iau_ZP', '2', STATUS )
      CALL VVD ( P(3), 0D0, 0D0, 'iau_ZP', '3', STATUS )

      END

      SUBROUTINE T_iau_ZPV ( STATUS )
*+
*  - - - - - - - - - -
*   T _ i a u _ Z P V
*  - - - - - - - - - -
*
*  Test iau_ZPV routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_ZPV, VVD
*
*  This revision:  2008 November 29
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION PV(3,2)


      PV(1,1) = 0.3D0
      PV(2,1) = 1.2D0
      PV(3,1) = -2.5D0

      PV(1,2) = -0.5D0
      PV(2,2) = 3.1D0
      PV(3,2) = 0.9D0

      CALL iau_ZPV ( PV )

      CALL VVD ( PV(1,1), 0D0, 0D0, 'iau_ZPV', 'P1', STATUS )
      CALL VVD ( PV(2,1), 0D0, 0D0, 'iau_ZPV', 'P2', STATUS )
      CALL VVD ( PV(3,1), 0D0, 0D0, 'iau_ZPV', 'P3', STATUS )
      CALL VVD ( PV(1,2), 0D0, 0D0, 'iau_ZPV', 'V1', STATUS )
      CALL VVD ( PV(2,2), 0D0, 0D0, 'iau_ZPV', 'V2', STATUS )
      CALL VVD ( PV(3,2), 0D0, 0D0, 'iau_ZPV', 'V3', STATUS )

      END

      SUBROUTINE T_iau_ZR ( STATUS )
*+
*  - - - - - - - - -
*   T _ i a u _ Z R
*  - - - - - - - - -
*
*  Test iau_ZR routine.
*
*  Returned:
*     STATUS    LOGICAL     .TRUE. = success, .FALSE. = fail
*
*  Called:  iau_ZR, VVD
*
*  This revision:  2008 November 30
*-

      IMPLICIT NONE

      LOGICAL STATUS

      DOUBLE PRECISION  R(3,3)


      R(1,1) = 2D0
      R(1,2) = 3D0
      R(1,3) = 2D0

      R(2,1) = 3D0
      R(2,2) = 2D0
      R(2,3) = 3D0

      R(3,1) = 3D0
      R(3,2) = 4D0
      R(3,3) = 5D0

      CALL iau_ZR ( R )

      CALL VVD ( R(1,1), 0D0, 0D0, 'iau_ZR', '11', STATUS )
      CALL VVD ( R(1,2), 0D0, 0D0, 'iau_ZR', '12', STATUS )
      CALL VVD ( R(1,3), 0D0, 0D0, 'iau_ZR', '13', STATUS )
      CALL VVD ( R(2,1), 0D0, 0D0, 'iau_ZR', '21', STATUS )
      CALL VVD ( R(2,2), 0D0, 0D0, 'iau_ZR', '22', STATUS )
      CALL VVD ( R(2,3), 0D0, 0D0, 'iau_ZR', '23', STATUS )
      CALL VVD ( R(3,1), 0D0, 0D0, 'iau_ZR', '31', STATUS )
      CALL VVD ( R(3,2), 0D0, 0D0, 'iau_ZR', '32', STATUS )
      CALL VVD ( R(3,3), 0D0, 0D0, 'iau_ZR', '33', STATUS )

*-----------------------------------------------------------------------

*+----------------------------------------------------------------------
*
*  Copyright (C) 2010
*  Standards Of Fundamental Astronomy Board
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
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
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
*        include the prefix "iau".
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
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*----------------------------------------------------------------------

      END
