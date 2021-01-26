      ******************************************************************
      *                                                                *
      * RESTful file access                                            *
      *                                                                *
      ******************************************************************

       IDENTIFICATION DIVISION.
       PROGRAM-ID. RESTFI09

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      *  Container name declarations
      *  Channel and container names are case sensitive
       01 DATE-CONT          PIC X(16) VALUE 'CICSTIME'.
       01 INPUT-CONT         PIC X(16) VALUE 'INPUTDATA'.
       01 OUTPUT-CONT        PIC X(16) VALUE 'OUTPUTDATA'.
       01 LENGTH-CONT        PIC X(16) VALUE 'INPUTDATALENGTH'.
       01 ERROR-CONT         PIC X(16) VALUE 'ERRORDATA'.
       01 RESP-CONT          PIC X(16) VALUE 'CICSRC'.


      *  Data fields used by the program
       01 INPUTLENGTH        PIC S9(8) COMP-4.
       01 DATALENGTH         PIC S9(8) COMP-4.
       01 CURRENTTIME        PIC S9(15) COMP-3.
       01 ABENDCODE          PIC X(4) VALUE SPACES.
       01 CHANNELNAME        PIC X(16) VALUE SPACES.
       01 INPUTSTRING        PIC X(72) VALUE SPACES.
       01 QUESTIONNO         PIC X(5)  VALUE SPACES.
       01 OUTPUTSTRING       PIC X(400) VALUE SPACES.
       01 RESPCODE           PIC S9(8) COMP-4 VALUE 0.
       01 RESPCODE2          PIC S9(8) COMP-4 VALUE 0.
       01 DATE-TIME.
         03 DATESTRING         PIC X(10) VALUE SPACES.
         03 TIME-SEP           PIC X(1) VALUE SPACES.
         03 TIMESTRING         PIC X(8) VALUE SPACES.
       01 RC-RECORD          PIC S9(8) COMP-4 VALUE 0.
       01 ERR-RECORD.
         03 ERRORCMD           PIC X(16) VALUE SPACES.
         03 ERRORSTRING        PIC X(32) VALUE SPACES.
       01 WORK-AREAS.
           05  ERR-CODE         PIC S9(8) COMP.
           05  RECLEN           PIC S9(4) COMP VALUE 385.
      *
      *  VSAM CUSTOMER RECORD LAYOUT
      *
       01  WBVSCUST-RECORD-LAYOUT.
           05 QUESTIONNO-FILE       PIC 9(5).
           05 QUESTION-FILE         PIC X(98).
           05 ANSWERONE-FILE        PIC X(90).
           05 ANSONE-FILE           PIC 9(4).
           05 ANSWERTWO-FILE        PIC X(90).
           05 ANSTWO-FILE           PIC 9(4).
           05 ANSWERTHREE-FILE      PIC X(90).
           05 ANSTHREE-FILE         PIC 9(4).

       PROCEDURE DIVISION.
      *  -----------------------------------------------------------
       MAIN-PROCESSING SECTION.
      *  -----------------------------------------------------------

      *  Get name of channel
           EXEC CICS ASSIGN CHANNEL(CHANNELNAME)
                            END-EXEC.

      *  If no channel passed in, terminate with abend code NOCH
           IF CHANNELNAME = SPACES THEN
               MOVE 'NOCH' TO ABENDCODE
               PERFORM ABEND-ROUTINE
           END-IF.


      *  Read content and length of input container
           MOVE LENGTH OF INPUTSTRING TO INPUTLENGTH.
           EXEC CICS GET CONTAINER(INPUT-CONT)
                            CHANNEL(CHANNELNAME)
                            FLENGTH(INPUTLENGTH)
                            INTO(INPUTSTRING)
                            RESP(RESPCODE)
                            RESP2(RESPCODE2)
                            END-EXEC.

      *  Place RC in binary container for return to caller
           MOVE RESPCODE TO RC-RECORD.
           EXEC CICS PUT CONTAINER(RESP-CONT)
                            FROM(RC-RECORD)
                            FLENGTH(LENGTH OF RC-RECORD)
                            BIT
                            RESP(RESPCODE)
                            END-EXEC.

           IF RESPCODE NOT = DFHRESP(NORMAL)
             PERFORM RESP-ERROR
           END-IF.

      *  Check customer number

           MOVE INPUTSTRING TO QUESTIONNO.

           IF QUESTIONNO IS NUMERIC
              CONTINUE
           ELSE
              MOVE 'QUESTION NUMBER IS NOT NUMERIC' TO OUTPUTSTRING
              PERFORM PUT-TO-CONTAINER.
      *       PERFORM NORMAL-RETURN.

      *READ-CUSTOMER-FILE.
      *
      *    USING THE VALID CUSTNO, READ THE FILE,
      *       CHECK FOR ERRORS, AND IF NONE, MOVE DATA TO CONTAINER
      *
           EXEC CICS READ FILE('MIMKK21B')
                          INTO(WBVSCUST-RECORD-LAYOUT)
                          RIDFLD(QUESTIONNO)
                          LENGTH(RECLEN)
                          RESP(ERR-CODE)
                          END-EXEC
           EVALUATE ERR-CODE
               WHEN DFHRESP(NORMAL)
                  STRING
                     QUESTION-FILE DELIMITED BY SIZE
                     ANSWERONE-FILE DELIMITED BY SIZE
                     ANSONE-FILE DELIMITED BY SIZE
                     ANSWERTWO-FILE DELIMITED BY SIZE
                     ANSTWO-FILE DELIMITED BY SIZE
                     ANSWERTHREE-FILE DELIMITED BY SIZE
                     ANSTHREE-FILE DELIMITED BY SIZE
                     INTO OUTPUTSTRING
                  END-STRING
               WHEN DFHRESP(NOTFND)
                  MOVE 'CUSTOMER RECORD NOT FOUND' TO OUTPUTSTRING
                  PERFORM PUT-TO-CONTAINER
               WHEN OTHER
                  MOVE 'FILE READ ERROR. CONTACT WB SUPPORT' TO
                        OUTPUTSTRING
                  PERFORM PUT-TO-CONTAINER
           END-EVALUATE.

       PUT-TO-CONTAINER.
      *
      *    MOVE DATA TO OUTPUT CONTAINER
      *

           EXEC CICS PUT CONTAINER(OUTPUT-CONT)
                            FROM(OUTPUTSTRING)
                            FLENGTH(LENGTH OF OUTPUTSTRING)
                            CHAR
                            RESP(RESPCODE)
                            END-EXEC.

           IF RESPCODE NOT = DFHRESP(NORMAL)
             PERFORM RESP-ERROR
           END-IF.

      *
      *    MOVE DATA TO ERROR CONTAINER
      *

           EXEC CICS PUT CONTAINER(ERROR-CONT)
                            FROM(ERRORSTRING)
                            FLENGTH(LENGTH OF ERRORSTRING)
                            CHAR
                            RESP(RESPCODE)
                            END-EXEC.

           IF RESPCODE NOT = DFHRESP(NORMAL)
             PERFORM RESP-ERROR
           END-IF.

      *  Get the current time
           EXEC CICS ASKTIME ABSTIME(CURRENTTIME)
                            END-EXEC.

      *  Format date and time
           EXEC CICS FORMATTIME
                     ABSTIME(CURRENTTIME)
                     DDMMYYYY(DATESTRING)
                     DATESEP('/')
                     TIME(TIMESTRING)
                     TIMESEP(':')
                     RESP(RESPCODE)
                     END-EXEC.

      *  Check return code
           IF RESPCODE NOT = DFHRESP(NORMAL)
               STRING 'Failed' DELIMITED BY SIZE
                            INTO DATESTRING END-STRING
           END-IF.

      *  Place current date in container CICSTIME
           EXEC CICS PUT CONTAINER(DATE-CONT)
                            FROM(DATE-TIME)
                            FLENGTH(LENGTH OF DATE-TIME)
                            CHAR
                            RESP(RESPCODE)
                            END-EXEC.
      *  Check return code
           IF RESPCODE NOT = DFHRESP(NORMAL)
             PERFORM RESP-ERROR
           END-IF.



      *  Return back to caller
           PERFORM NORMAL-RETURN.

      *  -----------------------------------------------------------
       RESP-ERROR.
             MOVE 'EDUC' TO ABENDCODE
             PERFORM ABEND-ROUTINE.

           PERFORM NORMAL-RETURN.

      *  -----------------------------------------------------------
      *  Abnormal end
      *  -----------------------------------------------------------
       ABEND-ROUTINE.
           EXEC CICS ABEND ABCODE(ABENDCODE) END-EXEC.

      *  -----------------------------------------------------------
      *  Finish
      *  -----------------------------------------------------------

       NORMAL-RETURN.
           EXEC CICS RETURN END-EXEC.
           GOBACK.
