//FRGONOD JOB ,NOTIFY=&SYSUID,
// MSGCLASS=H,MSGLEVEL=(1,1),TIME=(,4),REGION=144M,COND=(16,LT)
//*
//***************************************************************
//* STEP1 : HOUSE KEEPING
//***************************************************************
//DELFILE  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//SYSUDUMP DD SYSOUT=*
//SYSIN    DD *
      DELETE FRANKAR.LEARN.FINAL.PARTS.SUPP.SRT   PURGE
      DELETE FRANKAR.LEARN.FINAL.GPART.SUPP.FILE  PURGE

  SET LASTCC=0
  SET MAXCC=0
/*
//*************************************************************
//* STEP2 : PART SUPPLIER FILE SORTED BY PART NUMBER and REMOVE
//*         DUPLICATES
//*************************************************************
//SORTSTEP EXEC PGM=SORT
//SYSPRINT DD SYSOUT=*
//SYSOUT   DD SYSOUT=*
//SORTWK1  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SORTWK2  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SORTWK3  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SORTWK4  DD  SPACE=(CYL,(1,1)),UNIT=SYSALLDA
//SORTIN   DD DSN=FRANKAR.LEARN.FINAL.PARTS.UNSOR,DISP=SHR
//SORTOUT  DD DSN=FRANKAR.LEARN.FINAL.PARTS.SUPP.SRT,
//            DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),
//            DCB=(LRECL=480,BLKSIZE=0,RECFM=FB,DSORG=PS)
//SYSIN DD *
  SORT FIELDS=(1,23,CH,A)
  SUM FIELDS=NONE
/*
//********************************************************************
//* STEP3 : CREATES PARTS,SUPPLIER,SUPPLIER ADDRESS,PURCHASE ORDER
//*         GOOD PART SUPPLIER AND ERROR FILES CREATED
//*******************************************************************
// SET COBPGM='PRTSUPP'
//**** Compile JCL ******
//** Go (Run) Step. Add //DD cards when needed ******
//GO    EXEC   PGM=&COBPGM.
//STEPLIB DD DSN=&SYSUID..LEARN.FINAL.LOAD,DISP=SHR
//        DD DSN=DTOOLBLD.VER2M0.P4J3K3.SEQAMOD,DISP=SHR
//PARTSUPP  DD DSN=FRANKAR.LEARN.FINAL.PARTS.SUPP.SRT,DISP=SHR
//ZIPCODE   DD DSN=FRANKAR.LEARN.FINAL.ZIPSTATE,DISP=SHR
//GPARTSUP  DD DSN=FRANKAR.LEARN.FINAL.GPART.SUPP.FILE,
//            DISP=(NEW,CATLG,DELETE),
//            STORCLAS=USRBASE,SPACE=(TRK,(12,5),RLSE),
//            DCB=(LRECL=480,BLKSIZE=0,RECFM=FB,DSORG=PS)
//PARTS     DD SYSOUT=*
//PURCHRDS  DD SYSOUT=*
//SUPPLIER  DD SYSOUT=*
//SUPPADDR  DD SYSOUT=*
//ERROR     DD SYSOUT=*
//******************************************************************
//* STEP4 : GENERATE CONTROL REPORT FOR PART AND SUPPLIER
//******************************************************************
//GORPT    EXEC   PGM=RPTPGM
//STEPLIB DD DSN=&SYSUID..LEARN.FINAL.LOAD,DISP=SHR
//        DD DSN=DTOOLBLD.VER2M0.P4J3K3.SEQAMOD,DISP=SHR
//******* ADDITIONAL RUNTIME JCL HERE ******
//GPARTSUP  DD DSN=FRANKAR.LEARN.FINAL.GPART.SUPP.FILE,DISP=SHR
//RPT       DD SYSOUT=*
/*
//