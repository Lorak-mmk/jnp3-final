//WBVSCUST JOB 1,'UW',REGION=0M,NOTIFY=&SYSUID
//STEP1 EXEC PGM=IDCAMS
//SYSPRINT  DD SYSOUT=*
//SYSIN     DD *
 DELETE (KC03H71.CICSLABS.CUSTFILE) CLUSTER PURGE
 DEFINE CLUSTER ( NAME (KC03H71.CICSLABS.CUSTFILE) -
        CONTROLINTERVALSIZE(2048) -
        KEYS(5 0) INDEXED         -
        RECORDS(50 10)            -
        RECORDSIZE(385 400)       -
        FREESPACE(10 10)          -
        SHAREOPTIONS(2)  )        -
  DATA ( NAME (KC03H71.CICSLABS.CUSTFILE.DATA) ) -
  INDEX ( NAME (KC03H71.CICSLABS.CUSTFILE.INDEX) )
//STEP2 EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//DD1      DD *
99999
//DD2      DD DSN=KC03H73.CICSLABS.CUSTFILE,DISP=OLD
//SYSIN  DD *
    REPRO  INFILE(DD1) OUTFILE(DD2)
//*******************************************************************
//*  END OF VSAM REPROS
//*******************************************************************
//