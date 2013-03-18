@echo off
rem
rem
rem  ###########################################################
rem  ##                                                       ##
rem  ##  compile.bat  --  compile each of the TINKER modules  ##
rem  ##      (Intel Fortran Compiler for Windows Version)     ##
rem  ##                                                       ##
rem  ###########################################################
rem
rem
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp active.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp alchemy.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp analysis.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp analyze.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp angles.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp anneal.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp archive.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp attach.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp basefile.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp beeman.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp bicubic.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp bitors.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp bonds.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp born.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp bounds.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp bussi.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp calendar.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp center.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp chkpole.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp chkring.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp chkxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp cholesky.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp clock.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp cluster.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp column.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp /4Yportlib command.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp connect.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp connolly.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp control.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp correlate.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp crystal.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp cspline.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp cutoffs.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp deflate.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp delete.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp diagq.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp diffeq.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp diffuse.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp distgeom.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp document.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp dynamic.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangang.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangang1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangang2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangang3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangle.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangle1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangle2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eangle3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebond.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebond1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebond2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebond3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebuck.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebuck1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebuck2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ebuck3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echarge.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echarge1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echarge2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echarge3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echgdpl.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echgdpl1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echgdpl2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp echgdpl3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp edipole.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp edipole1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp edipole2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp edipole3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egauss.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egauss1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egauss2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egauss3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egeom.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egeom1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egeom2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp egeom3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ehal.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ehal1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ehal2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ehal3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimprop.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimprop1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimprop2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimprop3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimptor.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimptor1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimptor2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eimptor3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp elj.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp elj1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp elj2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp elj3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp embed.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emetal.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emetal1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emetal2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emetal3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emm3hb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emm3hb1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emm3hb2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp emm3hb3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp empole.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp empole1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp empole2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp empole3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp energy.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopbend.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopbend1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopbend2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopbend3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopdist.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopdist1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopdist2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eopdist3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp epitors.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp epitors1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp epitors2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp epitors3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp erf.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp erxnfld.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp erxnfld1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp erxnfld2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp erxnfld3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp esolv.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp esolv1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp esolv2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp esolv3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrbnd.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrbnd1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrbnd2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrbnd3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrtor.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrtor1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrtor2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp estrtor3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etors.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etors1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etors2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etors3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etortor.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etortor1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etortor2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp etortor3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eurey.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eurey1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eurey2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp eurey3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp evcorr.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp extra.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp extra1.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp extra2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp extra3.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp fatal.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp fft3d.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp fftpack.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp field.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp final.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp flatten.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp freeunit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp gda.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp geometry.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getint.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getkey.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getmol.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getmol2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getnumb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getpdb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getprm.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getref.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getstring.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp gettext.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getword.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp getxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ghmcstep.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp gradient.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp gradrgd.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp gradrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp groups.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp grpline.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp gyrate.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp hessian.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp hessrgd.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp hessrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp hybrid.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp image.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp impose.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp induce.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp inertia.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp initatom.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp initial.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp initprm.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp initres.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp initrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp insert.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp intedit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp intxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp invbeta.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp invert.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp jacobi.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kangang.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kangle.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp katom.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kbond.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kcharge.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kdipole.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kewald.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kextra.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kgeom.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kimprop.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kimptor.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kinetic.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kmetal.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kmpole.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kopbend.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kopdist.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp korbit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kpitors.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kpolar.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ksolv.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kstrbnd.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kstrtor.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ktors.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ktortor.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kurey.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp kvdw.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp lattice.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp lbfgs.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp lights.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp makeint.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp makeref.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp makexyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp maxwell.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp mdinit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp mdrest.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp mdsave.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp mdstat.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp mechanic.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp merge.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp minimize.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp minirot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp minrigid.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp molecule.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp molxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp moments.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp monte.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp mutate.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp nblist.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp newton.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp newtrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp nextarg.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp nexttext.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp nose.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp nspline.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp nucleic.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp number.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp numeral.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp numgrad.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp ocvm.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp openend.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp optimize.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp optirot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp optrigid.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp optsave.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp orbital.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp orient.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp orthog.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp overlap.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp path.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pdbxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp picalc.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pmestuff.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pmpb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp polarize.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp poledit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp polymer.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp potential.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp precise.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pressure.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prmedit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prmkey.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp promo.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp protein.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtdyn.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prterr.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtint.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtmol2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtpdb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtprm.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtseq.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp prtxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pss.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pssrigid.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp pssrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp quatfit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp radial.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp random.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp rattle.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readdyn.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readgau.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readint.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readmol.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readmol2.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readpdb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readprm.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readseq.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp readxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp replica.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp respa.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp rgdstep.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp rings.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp rmsfit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp rotlist.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp rotpole.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp saddle.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp scan.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp sdstep.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp search.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp server.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp shakeup.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp sigmoid.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp sktstuff.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp sniffer.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp sort.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp spacefill.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp spectrum.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp square.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp suffix.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp superpose.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp surface.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp surfatom.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp switch.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp sybylxyz.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp temper.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp testgrad.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp testhess.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp testpair.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp testpol.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp testrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp timer.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp timerot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp tncg.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp torphase.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp torque.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp torsfit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp torsions.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp trimtext.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp unitcell.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp valence.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp verlet.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp version.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp vibbig.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp vibrate.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp vibrot.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp volume.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xtalfit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xtalmin.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xyzatm.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xyzedit.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xyzint.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xyzpdb.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp xyzsybyl.f
ifort /c /O3 /Qip- /Qprec-div- /w /assume:cc_omp /Qopenmp zatom.f
