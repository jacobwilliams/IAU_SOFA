#-----------------------------------------------------------------------
#
# Description:  make file for the Fortran 77 version of SOFA.  This
# make file creates a Unix .a library.  Designed for Linux/g77 but
# can be adapted for other platforms or run in an appropriate way
# by means of the macros FC (Fortran compiler command), FF (qualifiers
# for compiling subprograms) and FX (qualifiers for compiling
# executables).  For example, to build, install and test the library:
#
#    on lf95 (Lahey), type:  make FC=lf95 "FX=-O --warn" test
#    on Sun/Solaris, type:   make FC=f77 FX=-O test
#
# Usage:
#
#    To build the library:
#
#      make
#
#    To install the library:
#
#      make install
#
#    To delete all object files:
#
#      make clean
#
#    To build and run the test program using the installed library:
#
#      make test
#
#    Also:
#      make all           same as make
#      make deinstall     deinstall the library
#      make check         test the build
#      make installcheck  same as make test
#      make distclean     clean up and deinstall
#      make realclean     same as make clean
#
# Last revision:   2010 September 5
#
# Copyright International Astronomical Union.  All rights reserved.
#
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
#
#  DEFINITIONS
#
#-----------------------------------------------------------------------

#----YOU MAY HAVE TO MODIFY THE NEXT FEW DEFINITIONS-----

# Specify the installation home directory.

INSTALL_DIR = $(HOME)

# Specify the installation directory for the library.

SOFA_LIB_DIR = $(INSTALL_DIR)/lib/

# This software is compilable only by ANSI Fortran 77 compilers - give
# the name of your preferred Fortran compiler (FC) and compilation flags
# (FF for subprograms and FX for executables) here.

FC = g77
FX = -O -Wall
FF = -c $(FX)

#----YOU SHOULDN'T HAVE TO MODIFY ANYTHING BELOW THIS LINE---------

SHELL = /bin/sh

# The list of installation directories.

INSTALL_DIRS = $(SOFA_LIB_DIR)

# Name the SOFA/F library in its source and target locations.

SOFA_LIB_NAME = libsofa.a
SOFA_LIB = $(SOFA_LIB_DIR)$(SOFA_LIB_NAME)

# Name the SOFA/F testbed in its source and target locations

SOFA_TEST_NAME = t_sofa_f.for
SOFA_TEST = t_sofa_f

# The list of SOFA library object files.

SOFA_OBS = iau_A2AF.o \
           iau_A2TF.o \
           iau_AF2A.o \
           iau_ANP.o \
           iau_ANPM.o \
           iau_BI00.o \
           iau_BP00.o \
           iau_BP06.o \
           iau_BPN2XY.o \
           iau_C2I00A.o \
           iau_C2I00B.o \
           iau_C2I06A.o \
           iau_C2IBPN.o \
           iau_C2IXY.o \
           iau_C2IXYS.o \
           iau_C2S.o \
           iau_C2T00A.o \
           iau_C2T00B.o \
           iau_C2T06A.o \
           iau_C2TCEO.o \
           iau_C2TCIO.o \
           iau_C2TEQX.o \
           iau_C2TPE.o \
           iau_C2TXY.o \
           iau_CAL2JD.o \
           iau_CP.o \
           iau_CPV.o \
           iau_CR.o \
           iau_D2DTF.o \
           iau_D2TF.o \
           iau_DAT.o \
           iau_DTDB.o \
           iau_DTF2D.o \
           iau_EE00.o \
           iau_EE00A.o \
           iau_EE00B.o \
           iau_EE06A.o \
           iau_EECT00.o \
           iau_EFORM.o \
           iau_EO06A.o \
           iau_EORS.o \
           iau_EPB.o \
           iau_EPB2JD.o \
           iau_EPJ.o \
           iau_EPJ2JD.o \
           iau_EPV00.o \
           iau_EQEQ94.o \
           iau_ERA00.o \
           iau_FAD03.o \
           iau_FAE03.o \
           iau_FAF03.o \
           iau_FAJU03.o \
           iau_FAL03.o \
           iau_FALP03.o \
           iau_FAMA03.o \
           iau_FAME03.o \
           iau_FANE03.o \
           iau_FAOM03.o \
           iau_FAPA03.o \
           iau_FASA03.o \
           iau_FAUR03.o \
           iau_FAVE03.o \
           iau_FK52H.o \
           iau_FK5HIP.o \
           iau_FK5HZ.o \
           iau_FW2M.o \
           iau_FW2XY.o \
           iau_GC2GD.o \
           iau_GC2GDE.o \
           iau_GD2GC.o \
           iau_GD2GCE.o \
           iau_GMST00.o \
           iau_GMST06.o \
           iau_GMST82.o \
           iau_GST00A.o \
           iau_GST00B.o \
           iau_GST06.o \
           iau_GST06A.o \
           iau_GST94.o \
           iau_H2FK5.o \
           iau_HFK5Z.o \
           iau_IR.o \
           iau_JD2CAL.o \
           iau_JDCALF.o \
           iau_NUM00A.o \
           iau_NUM00B.o \
           iau_NUM06A.o \
           iau_NUMAT.o \
           iau_NUT00A.o \
           iau_NUT00B.o \
           iau_NUT06A.o \
           iau_NUT80.o \
           iau_NUTM80.o \
           iau_OBL06.o \
           iau_OBL80.o \
           iau_P06E.o \
           iau_P2PV.o \
           iau_P2S.o \
           iau_PAP.o \
           iau_PAS.o \
           iau_PB06.o \
           iau_PDP.o \
           iau_PFW06.o \
           iau_PLAN94.o \
           iau_PM.o \
           iau_PMAT00.o \
           iau_PMAT06.o \
           iau_PMAT76.o \
           iau_PMP.o \
           iau_PN.o \
           iau_PN00.o \
           iau_PN00A.o \
           iau_PN00B.o \
           iau_PN06.o \
           iau_PN06A.o \
           iau_PNM00A.o \
           iau_PNM00B.o \
           iau_PNM06A.o \
           iau_PNM80.o \
           iau_POM00.o \
           iau_PPP.o \
           iau_PPSP.o \
           iau_PR00.o \
           iau_PREC76.o \
           iau_PV2P.o \
           iau_PV2S.o \
           iau_PVDPV.o \
           iau_PVM.o \
           iau_PVMPV.o \
           iau_PVPPV.o \
           iau_PVSTAR.o \
           iau_PVU.o \
           iau_PVUP.o \
           iau_PVXPV.o \
           iau_PXP.o \
           iau_RM2V.o \
           iau_RV2M.o \
           iau_RX.o \
           iau_RXP.o \
           iau_RXPV.o \
           iau_RXR.o \
           iau_RY.o \
           iau_RZ.o \
           iau_S00.o \
           iau_S00A.o \
           iau_S00B.o \
           iau_S06.o \
           iau_S06A.o \
           iau_S2C.o \
           iau_S2P.o \
           iau_S2PV.o \
           iau_S2XPV.o \
           iau_SEPP.o \
           iau_SEPS.o \
           iau_SP00.o \
           iau_STARPM.o \
           iau_STARPV.o \
           iau_SXP.o \
           iau_SXPV.o \
           iau_TAITT.o \
           iau_TAIUT1.o \
           iau_TAIUTC.o \
           iau_TCBTDB.o \
           iau_TCGTT.o \
           iau_TDBTCB.o \
           iau_TDBTT.o \
           iau_TF2A.o  \
           iau_TF2D.o  \
           iau_TR.o \
           iau_TRXP.o \
           iau_TRXPV.o \
           iau_TTTAI.o \
           iau_TTTCG.o \
           iau_TTTDB.o \
           iau_TTUT1.o \
           iau_UT1TAI.o \
           iau_UT1TT.o \
           iau_UT1UTC.o \
           iau_UTCTAI.o \
           iau_UTCUT1.o \
           iau_XY06.o \
           iau_XYS00A.o \
           iau_XYS00B.o \
           iau_XYS06A.o \
           iau_ZP.o \
           iau_ZPV.o \
           iau_ZR.o

#-----------------------------------------------------------------------
#
#  TARGETS
#
#-----------------------------------------------------------------------

# Build (but do not install) the library.
all : $(SOFA_LIB_NAME)
	-@ echo ""
	-@ echo "*** Now type 'make test'" \
                " to install the library and run tests ***"
	-@ echo ""

# Install the library.
install $(SOFA_LIB) : $(INSTALL_DIRS) $(SOFA_LIB_NAME)
	cp $(SOFA_LIB_NAME) $(SOFA_LIB)

# Deinstall the library.
deinstall:
	rm -f $(SOFA_LIB)

# Test the build.
check: $(SOFA_TEST_NAME) $(SOFA_LIB_NAME)
	$(FC) $(FX) $(SOFA_TEST_NAME) $(SOFA_LIB_NAME) -o $(SOFA_TEST)
	./$(SOFA_TEST)
	rm -f $(SOFA_TEST)

# Test the installed library.
installcheck test: $(SOFA_TEST_NAME) $(SOFA_LIB)
	$(FC) $(FX) $(SOFA_TEST_NAME) -L$(SOFA_LIB_DIR) -lsofa -o $(SOFA_TEST)
	./$(SOFA_TEST)
	rm -f $(SOFA_TEST)

# Local clean up.
clean realclean:
	rm -f $(SOFA_OBS) $(SOFA_LIB_NAME)

# Clean up and deinstall.
distclean:
	rm -f $(SOFA_OBS) $(SOFA_LIB_NAME) $(SOFA_LIB)

# Create the installation directory if not already present.
$(INSTALL_DIRS):
	mkdir -p $@

# Build the library.
$(SOFA_LIB_NAME): $(SOFA_OBS)
	ar ru $(SOFA_LIB_NAME) $?

#-----------------------------------------------------------------------

# The list of object file dependencies.



iau_A2AF.o : a2af.for
	$(FC) $(FF) -o $@ a2af.for
iau_A2TF.o : a2tf.for
	$(FC) $(FF) -o $@ a2tf.for
iau_AF2A.o : af2a.for
	$(FC) $(FF) -o $@ af2a.for
iau_ANP.o : anp.for
	$(FC) $(FF) -o $@ anp.for
iau_ANPM.o : anpm.for
	$(FC) $(FF) -o $@ anpm.for
iau_BI00.o : bi00.for
	$(FC) $(FF) -o $@ bi00.for
iau_BP00.o : bp00.for
	$(FC) $(FF) -o $@ bp00.for
iau_BP06.o : bp06.for
	$(FC) $(FF) -o $@ bp06.for
iau_BPN2XY.o : bpn2xy.for
	$(FC) $(FF) -o $@ bpn2xy.for
iau_C2I00A.o : c2i00a.for
	$(FC) $(FF) -o $@ c2i00a.for
iau_C2I00B.o : c2i00b.for
	$(FC) $(FF) -o $@ c2i00b.for
iau_C2I06A.o : c2i06a.for
	$(FC) $(FF) -o $@ c2i06a.for
iau_C2IBPN.o : c2ibpn.for
	$(FC) $(FF) -o $@ c2ibpn.for
iau_C2IXY.o : c2ixy.for
	$(FC) $(FF) -o $@ c2ixy.for
iau_C2IXYS.o : c2ixys.for
	$(FC) $(FF) -o $@ c2ixys.for
iau_C2S.o : c2s.for
	$(FC) $(FF) -o $@ c2s.for
iau_C2T00A.o : c2t00a.for
	$(FC) $(FF) -o $@ c2t00a.for
iau_C2T00B.o : c2t00b.for
	$(FC) $(FF) -o $@ c2t00b.for
iau_C2T06A.o : c2t06a.for
	$(FC) $(FF) -o $@ c2t06a.for
iau_C2TCEO.o : c2tceo.for
	$(FC) $(FF) -o $@ c2tceo.for
iau_C2TCIO.o : c2tcio.for
	$(FC) $(FF) -o $@ c2tcio.for
iau_C2TEQX.o : c2teqx.for
	$(FC) $(FF) -o $@ c2teqx.for
iau_C2TPE.o : c2tpe.for
	$(FC) $(FF) -o $@ c2tpe.for
iau_C2TXY.o : c2txy.for
	$(FC) $(FF) -o $@ c2txy.for
iau_CAL2JD.o : cal2jd.for
	$(FC) $(FF) -o $@ cal2jd.for
iau_CP.o : cp.for
	$(FC) $(FF) -o $@ cp.for
iau_CPV.o : cpv.for
	$(FC) $(FF) -o $@ cpv.for
iau_CR.o : cr.for
	$(FC) $(FF) -o $@ cr.for
iau_D2DTF.o : d2dtf.for
	$(FC) $(FF) -o $@ d2dtf.for
iau_D2TF.o : d2tf.for
	$(FC) $(FF) -o $@ d2tf.for
iau_DAT.o : dat.for
	$(FC) $(FF) -o $@ dat.for
iau_DTDB.o : dtdb.for
	$(FC) $(FF) -o $@ dtdb.for
iau_DTF2D.o : dtf2d.for
	$(FC) $(FF) -o $@ dtf2d.for
iau_EFORM.o : eform.for
	$(FC) $(FF) -o $@ eform.for
iau_EE00.o : ee00.for
	$(FC) $(FF) -o $@ ee00.for
iau_EE00A.o : ee00a.for
	$(FC) $(FF) -o $@ ee00a.for
iau_EE00B.o : ee00b.for
	$(FC) $(FF) -o $@ ee00b.for
iau_EE06A.o : ee06a.for
	$(FC) $(FF) -o $@ ee06a.for
iau_EECT00.o : eect00.for
	$(FC) $(FF) -o $@ eect00.for
iau_EO06A.o : eo06a.for
	$(FC) $(FF) -o $@ eo06a.for
iau_EORS.o : eors.for
	$(FC) $(FF) -o $@ eors.for
iau_EPB.o : epb.for
	$(FC) $(FF) -o $@ epb.for
iau_EPB2JD.o : epb2jd.for
	$(FC) $(FF) -o $@ epb2jd.for
iau_EPJ.o : epj.for
	$(FC) $(FF) -o $@ epj.for
iau_EPJ2JD.o : epj2jd.for
	$(FC) $(FF) -o $@ epj2jd.for
iau_EPV00.o : epv00.for
	$(FC) $(FF) -o $@ epv00.for
iau_EQEQ94.o : eqeq94.for
	$(FC) $(FF) -o $@ eqeq94.for
iau_ERA00.o : era00.for
	$(FC) $(FF) -o $@ era00.for
iau_FAD03.o : fad03.for
	$(FC) $(FF) -o $@ fad03.for
iau_FAE03.o : fae03.for
	$(FC) $(FF) -o $@ fae03.for
iau_FAF03.o : faf03.for
	$(FC) $(FF) -o $@ faf03.for
iau_FAJU03.o : faju03.for
	$(FC) $(FF) -o $@ faju03.for
iau_FAL03.o : fal03.for
	$(FC) $(FF) -o $@ fal03.for
iau_FALP03.o : falp03.for
	$(FC) $(FF) -o $@ falp03.for
iau_FAMA03.o : fama03.for
	$(FC) $(FF) -o $@ fama03.for
iau_FAME03.o : fame03.for
	$(FC) $(FF) -o $@ fame03.for
iau_FANE03.o : fane03.for
	$(FC) $(FF) -o $@ fane03.for
iau_FAOM03.o : faom03.for
	$(FC) $(FF) -o $@ faom03.for
iau_FAPA03.o : fapa03.for
	$(FC) $(FF) -o $@ fapa03.for
iau_FASA03.o : fasa03.for
	$(FC) $(FF) -o $@ fasa03.for
iau_FAUR03.o : faur03.for
	$(FC) $(FF) -o $@ faur03.for
iau_FAVE03.o : fave03.for
	$(FC) $(FF) -o $@ fave03.for
iau_FK52H.o : fk52h.for
	$(FC) $(FF) -o $@ fk52h.for
iau_FK5HIP.o : fk5hip.for
	$(FC) $(FF) -o $@ fk5hip.for
iau_FK5HZ.o : fk5hz.for
	$(FC) $(FF) -o $@ fk5hz.for
iau_FW2M.o : fw2m.for
	$(FC) $(FF) -o $@ fw2m.for
iau_FW2XY.o : fw2xy.for
	$(FC) $(FF) -o $@ fw2xy.for
iau_GC2GD.o : gc2gd.for
	$(FC) $(FF) -o $@ gc2gd.for
iau_GC2GDE.o : gc2gde.for
	$(FC) $(FF) -o $@ gc2gde.for
iau_GD2GC.o : gd2gc.for
	$(FC) $(FF) -o $@ gd2gc.for
iau_GD2GCE.o : gd2gce.for
	$(FC) $(FF) -o $@ gd2gce.for
iau_GMST00.o : gmst00.for
	$(FC) $(FF) -o $@ gmst00.for
iau_GMST06.o : gmst06.for
	$(FC) $(FF) -o $@ gmst06.for
iau_GMST82.o : gmst82.for
	$(FC) $(FF) -o $@ gmst82.for
iau_GST00A.o : gst00a.for
	$(FC) $(FF) -o $@ gst00a.for
iau_GST00B.o : gst00b.for
	$(FC) $(FF) -o $@ gst00b.for
iau_GST06.o : gst06.for
	$(FC) $(FF) -o $@ gst06.for
iau_GST06A.o : gst06a.for
	$(FC) $(FF) -o $@ gst06a.for
iau_GST94.o : gst94.for
	$(FC) $(FF) -o $@ gst94.for
iau_H2FK5.o : h2fk5.for
	$(FC) $(FF) -o $@ h2fk5.for
iau_HFK5Z.o : hfk5z.for
	$(FC) $(FF) -o $@ hfk5z.for
iau_IR.o : ir.for
	$(FC) $(FF) -o $@ ir.for
iau_JD2CAL.o : jd2cal.for
	$(FC) $(FF) -o $@ jd2cal.for
iau_JDCALF.o : jdcalf.for
	$(FC) $(FF) -o $@ jdcalf.for
iau_NUM00A.o : num00a.for
	$(FC) $(FF) -o $@ num00a.for
iau_NUM00B.o : num00b.for
	$(FC) $(FF) -o $@ num00b.for
iau_NUM06A.o : num06a.for
	$(FC) $(FF) -o $@ num06a.for
iau_NUMAT.o : numat.for
	$(FC) $(FF) -o $@ numat.for
iau_NUT00A.o : nut00a.for
	$(FC) $(FF) -o $@ nut00a.for
iau_NUT00B.o : nut00b.for
	$(FC) $(FF) -o $@ nut00b.for
iau_NUT06A.o : nut06a.for
	$(FC) $(FF) -o $@ nut06a.for
iau_NUT80.o : nut80.for
	$(FC) $(FF) -o $@ nut80.for
iau_NUTM80.o : nutm80.for
	$(FC) $(FF) -o $@ nutm80.for
iau_OBL06.o : obl06.for
	$(FC) $(FF) -o $@ obl06.for
iau_OBL80.o : obl80.for
	$(FC) $(FF) -o $@ obl80.for
iau_P06E.o : p06e.for
	$(FC) $(FF) -o $@ p06e.for
iau_P2PV.o : p2pv.for
	$(FC) $(FF) -o $@ p2pv.for
iau_P2S.o : p2s.for
	$(FC) $(FF) -o $@ p2s.for
iau_PAP.o : pap.for
	$(FC) $(FF) -o $@ pap.for
iau_PAS.o : pas.for
	$(FC) $(FF) -o $@ pas.for
iau_PB06.o : pb06.for
	$(FC) $(FF) -o $@ pb06.for
iau_PDP.o : pdp.for
	$(FC) $(FF) -o $@ pdp.for
iau_PFW06.o : pfw06.for
	$(FC) $(FF) -o $@ pfw06.for
iau_PLAN94.o : plan94.for
	$(FC) $(FF) -o $@ plan94.for
iau_PM.o : pm.for
	$(FC) $(FF) -o $@ pm.for
iau_PMAT00.o : pmat00.for
	$(FC) $(FF) -o $@ pmat00.for
iau_PMAT06.o : pmat06.for
	$(FC) $(FF) -o $@ pmat06.for
iau_PMAT76.o : pmat76.for
	$(FC) $(FF) -o $@ pmat76.for
iau_PMP.o : pmp.for
	$(FC) $(FF) -o $@ pmp.for
iau_PN.o : pn.for
	$(FC) $(FF) -o $@ pn.for
iau_PN00.o : pn00.for
	$(FC) $(FF) -o $@ pn00.for
iau_PN00A.o : pn00a.for
	$(FC) $(FF) -o $@ pn00a.for
iau_PN00B.o : pn00b.for
	$(FC) $(FF) -o $@ pn00b.for
iau_PN06.o : pn06.for
	$(FC) $(FF) -o $@ pn06.for
iau_PN06A.o : pn06a.for
	$(FC) $(FF) -o $@ pn06a.for
iau_PNM00A.o : pnm00a.for
	$(FC) $(FF) -o $@ pnm00a.for
iau_PNM00B.o : pnm00b.for
	$(FC) $(FF) -o $@ pnm00b.for
iau_PNM06A.o : pnm06a.for
	$(FC) $(FF) -o $@ pnm06a.for
iau_PNM80.o : pnm80.for
	$(FC) $(FF) -o $@ pnm80.for
iau_POM00.o : pom00.for
	$(FC) $(FF) -o $@ pom00.for
iau_PPP.o : ppp.for
	$(FC) $(FF) -o $@ ppp.for
iau_PPSP.o : ppsp.for
	$(FC) $(FF) -o $@ ppsp.for
iau_PR00.o : pr00.for
	$(FC) $(FF) -o $@ pr00.for
iau_PREC76.o : prec76.for
	$(FC) $(FF) -o $@ prec76.for
iau_PV2P.o : pv2p.for
	$(FC) $(FF) -o $@ pv2p.for
iau_PV2S.o : pv2s.for
	$(FC) $(FF) -o $@ pv2s.for
iau_PVDPV.o : pvdpv.for
	$(FC) $(FF) -o $@ pvdpv.for
iau_PVM.o : pvm.for
	$(FC) $(FF) -o $@ pvm.for
iau_PVMPV.o : pvmpv.for
	$(FC) $(FF) -o $@ pvmpv.for
iau_PVPPV.o : pvppv.for
	$(FC) $(FF) -o $@ pvppv.for
iau_PVSTAR.o : pvstar.for
	$(FC) $(FF) -o $@ pvstar.for
iau_PVU.o : pvu.for
	$(FC) $(FF) -o $@ pvu.for
iau_PVUP.o : pvup.for
	$(FC) $(FF) -o $@ pvup.for
iau_PVXPV.o : pvxpv.for
	$(FC) $(FF) -o $@ pvxpv.for
iau_PXP.o : pxp.for
	$(FC) $(FF) -o $@ pxp.for
iau_RM2V.o : rm2v.for
	$(FC) $(FF) -o $@ rm2v.for
iau_RV2M.o : rv2m.for
	$(FC) $(FF) -o $@ rv2m.for
iau_RX.o : rx.for
	$(FC) $(FF) -o $@ rx.for
iau_RXP.o : rxp.for
	$(FC) $(FF) -o $@ rxp.for
iau_RXPV.o : rxpv.for
	$(FC) $(FF) -o $@ rxpv.for
iau_RXR.o : rxr.for
	$(FC) $(FF) -o $@ rxr.for
iau_RY.o : ry.for
	$(FC) $(FF) -o $@ ry.for
iau_RZ.o : rz.for
	$(FC) $(FF) -o $@ rz.for
iau_S00.o : s00.for
	$(FC) $(FF) -o $@ s00.for
iau_S00A.o : s00a.for
	$(FC) $(FF) -o $@ s00a.for
iau_S00B.o : s00b.for
	$(FC) $(FF) -o $@ s00b.for
iau_S06.o : s06.for
	$(FC) $(FF) -o $@ s06.for
iau_S06A.o : s06a.for
	$(FC) $(FF) -o $@ s06a.for
iau_S2C.o : s2c.for
	$(FC) $(FF) -o $@ s2c.for
iau_S2P.o : s2p.for
	$(FC) $(FF) -o $@ s2p.for
iau_S2PV.o : s2pv.for
	$(FC) $(FF) -o $@ s2pv.for
iau_S2XPV.o : s2xpv.for
	$(FC) $(FF) -o $@ s2xpv.for
iau_SEPP.o : sepp.for
	$(FC) $(FF) -o $@ sepp.for
iau_SEPS.o : seps.for
	$(FC) $(FF) -o $@ seps.for
iau_SP00.o : sp00.for
	$(FC) $(FF) -o $@ sp00.for
iau_STARPM.o : starpm.for
	$(FC) $(FF) -o $@ starpm.for
iau_STARPV.o : starpv.for
	$(FC) $(FF) -o $@ starpv.for
iau_SXP.o : sxp.for
	$(FC) $(FF) -o $@ sxp.for
iau_SXPV.o : sxpv.for
	$(FC) $(FF) -o $@ sxpv.for
iau_TAITT.o : taitt.for
	$(FC) $(FF) -o $@ taitt.for
iau_TAIUT1.o : taiut1.for
	$(FC) $(FF) -o $@ taiut1.for
iau_TAIUTC.o : taiutc.for
	$(FC) $(FF) -o $@ taiutc.for
iau_TCBTDB.o : tcbtdb.for
	$(FC) $(FF) -o $@ tcbtdb.for
iau_TCGTT.o : tcgtt.for
	$(FC) $(FF) -o $@ tcgtt.for
iau_TDBTCB.o : tdbtcb.for
	$(FC) $(FF) -o $@ tdbtcb.for
iau_TDBTT.o : tdbtt.for
	$(FC) $(FF) -o $@ tdbtt.for
iau_TF2A.o : tf2a.for
	$(FC) $(FF) -o $@ tf2a.for
iau_TF2D.o : tf2d.for
	$(FC) $(FF) -o $@ tf2d.for
iau_TR.o : tr.for
	$(FC) $(FF) -o $@ tr.for
iau_TRXP.o : trxp.for
	$(FC) $(FF) -o $@ trxp.for
iau_TRXPV.o : trxpv.for
	$(FC) $(FF) -o $@ trxpv.for
iau_TTTAI.o : tttai.for
	$(FC) $(FF) -o $@ tttai.for
iau_TTTCG.o : tttcg.for
	$(FC) $(FF) -o $@ tttcg.for
iau_TTTDB.o : tttdb.for
	$(FC) $(FF) -o $@ tttdb.for
iau_TTUT1.o : ttut1.for
	$(FC) $(FF) -o $@ ttut1.for
iau_UT1TAI.o : ut1tai.for
	$(FC) $(FF) -o $@ ut1tai.for
iau_UT1TT.o : ut1tt.for
	$(FC) $(FF) -o $@ ut1tt.for
iau_UT1UTC.o : ut1utc.for
	$(FC) $(FF) -o $@ ut1utc.for
iau_UTCTAI.o : utctai.for
	$(FC) $(FF) -o $@ utctai.for
iau_UTCUT1.o : utcut1.for
	$(FC) $(FF) -o $@ utcut1.for
iau_XY06.o : xy06.for
	$(FC) $(FF) -o $@ xy06.for
iau_XYS00A.o : xys00a.for
	$(FC) $(FF) -o $@ xys00a.for
iau_XYS00B.o : xys00b.for
	$(FC) $(FF) -o $@ xys00b.for
iau_XYS06A.o : xys06a.for
	$(FC) $(FF) -o $@ xys06a.for
iau_ZP.o : zp.for
	$(FC) $(FF) -o $@ zp.for
iau_ZPV.o : zpv.for
	$(FC) $(FF) -o $@ zpv.for
iau_ZR.o : zr.for
	$(FC) $(FF) -o $@ zr.for

#-----------------------------------------------------------------------
