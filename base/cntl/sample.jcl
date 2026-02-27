//TECN013A JOB (CTS,ISURANCE),'GIRI MFT',NOTIFY=&SYSUID,PRTY=1   
//DSCRT    PROC                                                         
//STEP001  EXEC PGM=IEFBR14                                             
//DD1      DD DSN=&DSNAME..PS2,DISP=(MOD,DELETE,DELETE),                
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD2      DD DSN=&DSNAME..PS3,DISP=(MOD,DELETE,DELETE),                
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD3      DD DSN=&DSNAME..PS4,DISP=(MOD,DELETE,DELETE),                
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD4      DD DSN=&DSNAME..PS5,DISP=(MOD,DELETE,DELETE),                
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD5      DD DSN=&DSNAME..PS6,DISP=(MOD,DELETE,DELETE),                
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//STEP002  EXEC PGM=IEFBR14                                             
//DD6      DD DSN=&DSNAME..PS2,DISP=(NEW,CATLG,DELETE),                 
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD7      DD DSN=&DSNAME..PS3,DISP=(NEW,CATLG,DELETE),                 
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD8      DD DSN=&DSNAME..PS4,DISP=(NEW,CATLG,DELETE),                 
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DD9      DD DSN=&DSNAME..PS5,DISP=(NEW,CATLG,DELETE),                 
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1)),                      
//            DCB=(LRECL=80,BLKSIZE=800,RECFM=FB)                       
//DD10     DD DSN=&DSNAME..PS6,DISP=(NEW,CATLG,DELETE),                 
//            VOLUME=SER=ZAPRD4,SPACE=(TRK,(1,1))                       
//DSCRT    PEND                                                         
//STEP003  EXEC PROC=DSCRT,DSNAME=TECN013.JCL.ASSMT04.INVNT             
//STEP004  EXEC PGM=IDCAMS
//SYSPRINT DD SYSOUT=*
//FILEIN DD DSN=TECN013.JCL.ASSMT04.INVT.PS1,DISP=SHR
//SYSIN DD *
  PRINT INFILE(FILEIN) CHARACTER COUNT(1)
/*
//IFCHECK IF (CHKFILE.RC = 0 ) THEN 
//STEP004  EXEC PGM=SORT                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=TECN013.JCL.ASSMT04.INVNT.PS1,DISP=SHR                
//SORTOUT  DD DSN=TECN013.JCL.ASSMT04.INVNT.PS2,DISP=SHR                
//SYSIN    DD *                                                         
  SORT FIELDS=(1,20,CH,A,29,2,ZD,D),SKIPREC(1)                          
  OMIT COND=(48,1,CH,EQ,C' ')                                           
/*                                                                      
//STEP005  EXEC PGM=SORT                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=TECN013.JCL.ASSMT04.INVNT.PS2,DISP=SHR                
//SORTOUT  DD DSN=TECN013.JCL.ASSMT04.INVNT.PS3,DISP=SHR                
//SYSIN    DD *                                                         
  SORT FIELDS=COPY                                                      
  INCLUDE COND=(29,2,ZD,GT,50)                                          
/*                                                                      
//STEP006  EXEC PGM=SORT                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=TECN013.JCL.ASSMT04.INVNT.PS2,DISP=SHR                
//SORTOUT  DD DSN=TECN013.JCL.ASSMT04.INVNT.PS4,DISP=SHR                
//SYSIN    DD *                                                         
  SORT FIELDS=COPY                                                      
  INCLUDE COND=(32,5,CH,EQ,C'JAPAN',OR,32,11,CH,EQ,C'PHILIPPINES',OR,   
               32,7,CH,EQ,C'MOROCCO',OR,32,8,CH,EQ,C'MALAYSIA')         
  INREC BUILD=(1:48,21,1X,23:32,15,1X,29,2,ZD,MUL,70,2,ZD,              
              EDIT=(TTTT),LENGTH=4)                                     
/*                                                                      
//STEP007  EXEC PGM=SORT                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=TECN013.JCL.ASSMT04.INVNT.PS2,DISP=SHR                
//SORTOUT  DD DSN=TECN013.JCL.ASSMT04.INVNT.PS5,DISP=SHR                
//SYSIN    DD *                                                         
  SORT FIELDS=COPY                                                      
  OUTFIL REMOVECC,                                                      
  HEADER1=(11X,C'***INVNT DETAILS***'),                                 
  TRAILER1=(C' TOTAL NUMBER OF ITEM_QTYS: ',TOT=(29,2,ZD,LENGTH=4))     
/*                                                                      
//STEP008  EXEC PGM=SORT                                                
//SYSPRINT DD SYSOUT=*                                                  
//SYSOUT   DD SYSOUT=*                                                  
//SORTIN   DD DSN=TECN013.JCL.ASSMT04.INVNT.PS2,DISP=SHR                
//SORTOUT  DD DSN=TECN013.JCL.ASSMT04.INVNT.PS6,DISP=SHR                
//SYSIN    DD *                                                         
  SORT FIELDS=COPY                                                      
  INREC FINDREP=(IN=C'HOS',OUT=C'INV',STARTPOS=22,ENDPOS=25)            
/*                                                                      
//IFCHECK ELSE
//STEP009  EXEC PGM=IEBGENER                                            
//SYSPRINT DD SYSOUT=*                                                  
//SYSUT1   DD * 
INPUT DATASET IS EMPTY
/*  
//SYSUT2   DD SYSOUT=*                                                  
//IFCHECK ENDIF
//
