//FRCOMP JOB ,NOTIFY=&SYSUID,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=544M,COND=(16,LT)
//*
//***************************************************************
// SET COBPGM='SUPPLIER'
//**************************************************************
//SUPPLIER EXEC PGM=IGYCRCTL,
// PARM='TEST(NOEJPD,SOURCE) LIST'
//STEPLIB    DD  DISP=SHR,
//       DSNAME=IGY.V6R2M0.SIGYCOMP
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN2
//SYSIN    DD  DISP=SHR,
//       DSNAME=FRANKAR.LEARN.FINAL.COBOL(&COBPGM.)
//*SYSPRINT DD  DISP=OLD,
//*       DSNAME=FRANKAR.LEARN.FINAL.LISTING(&COBPGM.)
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DISP=OLD,
//       DSNAME=FRANKAR.LEARN.FINAL.OBJECT(&COBPGM.)
//*
//SYSUT1   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT2   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT3   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT4   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT5   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT6   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT7   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT8   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT9   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT10  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT11  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT12  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT13  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT14  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT15  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSMDECK DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//*
//SYSLIB    DD DISP=SHR,DSNAME=FRANKAR.LEARN.FINAL.COPY
//* end eqaspbc6

//***************************************************************
// SET COBPGM='SUPPADDR'
//**************************************************************
//SUPPADDR EXEC PGM=IGYCRCTL,
// PARM='TEST(NOEJPD,SOURCE) LIST'
//STEPLIB    DD  DISP=SHR,
//       DSNAME=IGY.V6R2M0.SIGYCOMP
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN2
//SYSIN    DD  DISP=SHR,
//       DSNAME=FRANKAR.LEARN.FINAL.COBOL(&COBPGM.)
//*SYSPRINT DD  DISP=OLD,
//*       DSNAME=FRANKAR.LEARN.FINAL.LISTING(&COBPGM.)
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DISP=OLD,
//       DSNAME=FRANKAR.LEARN.FINAL.OBJECT(&COBPGM.)
//*
//SYSUT1   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT2   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT3   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT4   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT5   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT6   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT7   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT8   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT9   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT10  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT11  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT12  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT13  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT14  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT15  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSMDECK DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//*
//SYSLIB    DD DISP=SHR,DSNAME=FRANKAR.LEARN.FINAL.COPY
//* end eqaspbc6

//***************************************************************
// SET COBPGM='PARTS'
//**************************************************************
//PARTS EXEC PGM=IGYCRCTL,
// PARM='TEST(NOEJPD,SOURCE) LIST'
//STEPLIB    DD  DISP=SHR,
//       DSNAME=IGY.V6R2M0.SIGYCOMP
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN2
//SYSIN    DD  DISP=SHR,
//       DSNAME=FRANKAR.LEARN.FINAL.COBOL(&COBPGM.)
//*SYSPRINT DD  DISP=OLD,
//*       DSNAME=FRANKAR.LEARN.FINAL.LISTING(&COBPGM.)
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DISP=OLD,
//       DSNAME=FRANKAR.LEARN.FINAL.OBJECT(&COBPGM.)
//*
//SYSUT1   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT2   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT3   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT4   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT5   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT6   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT7   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT8   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT9   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT10  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT11  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT12  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT13  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT14  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT15  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSMDECK DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//*
//SYSLIB    DD DISP=SHR,DSNAME=FRANKAR.LEARN.FINAL.COPY
//* end eqaspbc6

//***************************************************************
// SET COBPGM='PURCHORN'
//**************************************************************
//PURCHORN EXEC PGM=IGYCRCTL,
// PARM='TEST(NOEJPD,SOURCE) LIST'
//STEPLIB    DD  DISP=SHR,
//       DSNAME=IGY.V6R2M0.SIGYCOMP
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN2
//SYSIN    DD  DISP=SHR,
//       DSNAME=FRANKAR.LEARN.FINAL.COBOL(&COBPGM.)
//*SYSPRINT DD  DISP=OLD,
//*       DSNAME=FRANKAR.LEARN.FINAL.LISTING(&COBPGM.)
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DISP=OLD,
//       DSNAME=FRANKAR.LEARN.FINAL.OBJECT(&COBPGM.)
//*
//SYSUT1   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT2   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT3   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT4   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT5   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT6   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT7   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT8   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT9   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT10  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT11  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT12  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT13  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT14  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT15  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSMDECK DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//*
//SYSLIB    DD DISP=SHR,DSNAME=FRANKAR.LEARN.FINAL.COPY
//* end eqaspbc6

//***************************************************************
// SET COBPGM='PRTSUPP'
//**************************************************************
//MAIN EXEC PGM=IGYCRCTL,
// PARM='TEST(NOEJPD,SOURCE) LIST'
//STEPLIB    DD  DISP=SHR,
//       DSNAME=IGY.V6R2M0.SIGYCOMP
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN
//    DD  DISP=SHR,
//       DSNAME=CEE.SCEERUN2
//SYSIN    DD  DISP=SHR,
//       DSNAME=FRANKAR.LEARN.FINAL.COBOL(&COBPGM.)
//*SYSPRINT DD  DISP=OLD,
//*       DSNAME=FRANKAR.LEARN.FINAL.LISTING(&COBPGM.)
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLIN   DD  DISP=OLD,
//       DSNAME=FRANKAR.LEARN.FINAL.OBJECT(&COBPGM.)
//*
//SYSUT1   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT2   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT3   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT4   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT5   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT6   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT7   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT8   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT9   DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT10  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT11  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT12  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT13  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT14  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSUT15  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SYSMDECK DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//*
//SYSLIB    DD DISP=SHR,DSNAME=FRANKAR.LEARN.FINAL.COPY
//* end eqaspbc6
// SET COBPGM='PRTSUPP'
//EQASPBCL EXEC PGM=IEWL,REGION=4096K,
// PARM=''
//SYSLIN   DD  DISP=SHR,
//       DSNAME=FRANKAR.LEARN.FINAL.OBJECT(&COBPGM.)
//SYSPRINT DD  SYSOUT=*
//SYSTERM  DD  SYSOUT=*
//SYSLMOD  DD  DISP=OLD,
//       DSNAME=FRANKAR.LEARN.FINAL.LOAD(&COBPGM.)
//*
//SYSLIB    DD DISP=SHR,DSNAME=CEE.SCEERUN
//          DD DISP=SHR,DSNAME=CEE.SCEELKED
//          DD DISP=SHR,DSNAME=FRANKAR.LEARN.FINAL.OBJECT
//* end eqaspbcl
















































