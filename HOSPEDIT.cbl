       IDENTIFICATION DIVISION.
       PROGRAM-ID.  HOSPEDIT.
       AUTHOR. JON SAYLES.
       INSTALLATION. COBOL DEV Center.
       DATE-WRITTEN. 01/01/08.
       DATE-COMPILED. 01/01/08.
       SECURITY. NON-CONFIDENTIAL.
      ***************************************************************
      *  THIS PROGRAM CREATES AN INPATIENT/OUTPATIENT REPORT SUMMARY
      *  MODIFICATION LOG:
      *  7/24/2020
      *
      *  MODIFICATIONS INCLUDE:
      *    CLEAN UP OF CODE AND LOGIC
      *    REWRITE OF THE REPORT
      *    TABLE ADDED TO VALIDATE INSURANCE TYPES
      *    REMOVED REDUNDANCY OF WRITING VALID DATA TO
      *       OUTPUT FILE AND REPORT - NOW WRITES TO REPORT ONLY
      *    CORRECTED FINAL STATS AND ADDED ADDITIONAL ONES
      *    MOVED WS-INPUT-REC TO PATIENT COPYBOOK
      *    ADDED ERROR MESSAGE TO ERROR FILE RECORDS
      *    THE GROSS TOTAL ON THE REPORT IS THE TOTAL OF DAILY AMOUNTS
      *        FROM VALID RECORDS
      *
      ***************************************************************

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-390.
       OBJECT-COMPUTER. IBM-390.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INFILE
           ASSIGN TO HOSPIN
             FILE STATUS IS IFCODE.

           SELECT RPTFILE
           ASSIGN TO RPTFILE
             FILE STATUS IS RFCODE.

           SELECT ERRFILE
           ASSIGN TO ERRFILE
             FILE STATUS IS EFCODE.

           SELECT INS-TYPE-FILE
           ASSIGN TO INSTYPE
             FILE STATUS IS ITCODE.

       DATA DIVISION.
       FILE SECTION.
       FD  INFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS IN-REC.
       01  IN-REC  PIC X(100).

       FD  ERRFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 100 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS ERR-REC.
       01  ERR-REC      PIC X(100).

       FD  RPTFILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPT-REC.
       01  RPT-REC      PIC X(132).

        FD INS-TYPE-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS INS-TYPE-RECORD.
       01 INS-TYPE-RECORD.
          05 INS-TYPE-REC PIC X(3).
          05 FILLER       PIC X(77).

       WORKING-STORAGE SECTION.

       01  FILE-STATUS-CODES.
           05  IFCODE                  PIC X(2).
               88 CODE-READ     VALUE SPACES.
               88 NO-MORE-DATA  VALUE "10".
           05  ITCODE                  PIC X(2).
               88 END-OF-FILE   VALUE '10'.
           05  EFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.
           05  RFCODE                  PIC X(2).
               88 CODE-WRITE    VALUE SPACES.

       77  INS-COVERAGE-PERC           PIC 9(3) VALUE 10.
       77  REPORT-MAX-LINES            PIC 9(2) VALUE 60.
       77  MORE-RECORDS-SW             PIC X(1) VALUE SPACE.
           88 NO-MORE-RECORDS  VALUE 'N'.

       01  COUNT-INS-TYPE              PIC X(3).
           88 HMO              VALUE 'HMO'.
           88 PRI              VALUE 'PRI'.
           88 PPO              VALUE 'PPO'.
           88 AFF              VALUE 'AFF'.
           88 MED              VALUE 'MED'.
           88 MAN              VALUE 'MAN'.
           88 POS              VALUE 'POS'.
           88 GOV              VALUE 'GOV'.

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR    PIC  9(4).
               10  WS-CURRENT-MONTH   PIC  9(2).
               10  WS-CURRENT-DAY     PIC  9(2).

       01  COUNTERS-AND-ACCUMULATORS.
           05 RECORDS-READ             PIC S9(4) COMP.
           05 RECORDS-WRITTEN          PIC S9(4) COMP.
           05 ERROR-RECS               PIC S9(4) COMP.
           05 NBR-INPATIENTS           PIC S9(4) COMP.
           05 NBR-OUTPATIENTS          PIC S9(4) COMP.
           05 NBR-HMO                  PIC S9(4) COMP.
           05 NBR-GOV                  PIC S9(4) COMP.
           05 NBR-PRI                  PIC S9(4) COMP.
           05 NBR-PPO                  PIC S9(4) COMP.
           05 NBR-AFF                  PIC S9(4) COMP.
           05 NBR-MED                  PIC S9(4) COMP.
           05 NBR-POS                  PIC S9(4) COMP.
           05 NBR-MAN                  PIC S9(4) COMP.
           05 NBR-NO-COVERAGE          PIC S9(4) COMP.
           05 PAT-TOTAL-AMT-NET        PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-GROSS          PIC S9(7)V99 COMP-3.
           05 TOTAL-AMT-NET            PIC S9(7)V99 COMP-3.
           05 PAGE-NUM                 PIC 9(3).
           05 LINE-COUNT               PIC 9(2).

      *** TABLE TO VERIFY INSURANCE TYPE
        01 INSURANCE-TYPES-TABLE.
           05 INSURANCE-TYPE OCCURS 20 TIMES
                   ASCENDING KEY IS INSURANCE-TYPE-CODE
                   INDEXED BY INS-TYPE-TAB-IDX.
               10 INSURANCE-TYPE-CODE  PIC X(3).

           COPY PATIENT.

       01  WS-OUTPUT-REC.
           05  PATIENT-NBR-O           PIC 9(5).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-NAME-O          PIC X(20).
           05  PATIENT-PHONE-O         PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-TYPE-O          PIC X(2).
           05  BED-IDENTITY-O          PIC ZZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  CURR-DATE-O             PIC X(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-O   PIC $$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-COVERAGE-PERC-O     PIC 999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-TYPE-O              PIC X(4).
           05  HOSPITAL-STAY-LTH-O     PIC 999.
           05  FILLER                  PIC X(11) VALUE SPACES.

       01  WS-ERROR-REC.
           05  PATIENT-NBR-E           PIC 9(5).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-NAME-E          PIC X(20).
           05  PATIENT-PHONE-E         PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-TYPE-E          PIC X(2).
           05  BED-IDENTITY-E          PIC ZZZ9.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  CURR-DATE-E             PIC X(6).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  PATIENT-AMT-PER-DAY-E   PIC $$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-COVERAGE-PERC-E     PIC 999.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  INS-TYPE-E              PIC X(4).
           05  HOSPITAL-STAY-LTH-E     PIC 999.
           05  ERR-MSG                 PIC X(15).

      *** WORKING STORAGE FOR THE REPORT
       01  BLANK-LINE                  PIC X(132) VALUE SPACES.

      ***  HEADERS

       01  HEADER-LINE1.
           05  FILLER                  PIC X(6) VALUE 'DATE: '.
           05  HL1-DATE.
               10 HL1-MONTH            PIC 9(2).
               10 SLASH-1              PIC X VALUE '/'.
               10 HL1-DAY              PIC 9(2).
               10 SLASH-2              PIC X VALUE '/'.
               10 HL1-YEAR             PIC 9(4).
           05  FILLER                  PIC X(43) VALUE SPACES.
           05  HL1-REPORT-TITLE        PIC X(35) VALUE
                   'INPATIENT/OUTPATIENT REPORT SUMMARY'.
           05  FILLER                  PIC X(28) VALUE SPACES.
           05  HL1-PAGE-NUM.
               10 FILLER               PIC X(6) VALUE 'PAGE: '.
               10 HL1-PAGE-NUMBER      PIC ZZ9.
           05  FILLER                  PIC X VALUE SPACE.

       01  HEADER-LINE2.
           05  FILLER                  PIC X(5)
                             VALUE 'PT # '.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(20)
                             VALUE 'PATIENT NAME'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(13)
                             VALUE 'PHONE NUMBER'.
           05  FILLER                  PIC X(4)
                             VALUE 'TYPE'.
           05  FILLER                  PIC X(1) VALUE SPACES.
           05  FILLER                  PIC X(4)
                             VALUE 'BED'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(10)
                             VALUE 'ADMIT DATE'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(9)
                             VALUE 'AMT @ DAY'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(4)
                             VALUE 'DIAG'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(3)
                             VALUE 'INS'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(4)
                             VALUE 'STAY'.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  FILLER                  PIC X(9)
                             VALUE 'NETWORK'.
           05  FILLER                  PIC X(1) VALUE SPACES.
           05  FILLER                  PIC X(5)
                             VALUE 'COPAY'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(6)
                             VALUE 'DEDUCT'.
           05  FILLER                  PIC X(15) VALUE SPACES.

      *** DETAIL LINE

       01  DETAIL-LINE1.
           05  DL1-PAT-NBR             PIC 9(5).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-PAT-NAME            PIC X(20).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-PHONE-NUMBER        PIC X(13).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-PAT-TYPE            PIC X(1).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-BED                 PIC 9(4).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-DATE-ADMIT          PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-AMT-PER-DAY         PIC $$,$$9.99.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-DIAG-CODE           PIC 9(4).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-INS-TYPE            PIC X(3).
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  DL1-HOSP-STAY-LTH       PIC 9(3).
           05  FILLER                  PIC X(6) VALUE SPACES.
           05  DL1-IN-OUT-NET          PIC X(3).
           05  FILLER                  PIC X(6) VALUE SPACES.
           05  DL1-COPAY               PIC $99.
           05  FILLER                  PIC X(3) VALUE SPACES.
           05  DL1-DEDUCTIBLE          PIC $,$99.
           05  FILLER                  PIC X(16) VALUE SPACES.


      ***  TOTAL LINES

       01  TOTAL-LINE1.
           05  FILLER                  PIC X(25)
                             VALUE 'REC IN: '.
           05  TL1-REC-IN              PIC ZZZ9.
           05  FILLER                  PIC X(103) VALUE SPACES.

       01  TOTAL-LINE2.
           05  FILLER                  PIC X(25)
                             VALUE 'REC WRITTEN: '.
           05  TL2-REC-WRITTEN         PIC ZZZ9.
           05  FILLER                  PIC X(103) VALUE SPACES.

       01  TOTAL-LINE3.
           05  FILLER                  PIC X(25)
                             VALUE 'REC ERRORS: '.
           05  TL3-REC-ERR             PIC ZZZ9.
           05  FILLER                  PIC X(103) VALUE SPACES.

       01  TOTAL-LINE4.
           05  FILLER                  PIC X(25)
                             VALUE 'INPATIENT: '.
           05  TL4-INPATIENT           PIC ZZZ9.
           05  FILLER                  PIC X(103) VALUE SPACES.

       01  TOTAL-LINE5.
           05  FILLER                  PIC X(25)
                             VALUE 'OUTPATIENT: '.
           05  TL5-OUTPATIENT          PIC ZZZ9.
           05  FILLER                  PIC X(103) VALUE SPACES.

       01  TOTAL-LINE6.
           05  FILLER                  PIC X(25)
                             VALUE 'HMO: '.
           05  TL6-HMO                 PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE7.
           05  FILLER                  PIC X(25)
                             VALUE 'PRI: '.
           05  TL7-PRI                 PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE8.
           05  FILLER                  PIC X(25)
                             VALUE 'PPO: '.
           05  TL8-PPO                 PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE9.
           05  FILLER                  PIC X(25)
                             VALUE 'AFF: '.
           05  TL9-AFF                 PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE10.
           05  FILLER                  PIC X(25)
                             VALUE 'MED: '.
           05  TL10-MED                PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE11.
           05  FILLER                  PIC X(25)
                             VALUE 'MAN: '.
           05  TL11-MAN                PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE12.
           05  FILLER                  PIC X(25)
                             VALUE 'POS: '.
           05  TL12-POS                PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE13.
           05  FILLER                  PIC X(25)
                             VALUE 'GOV: '.
           05  TL13-GOV                PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE14.
           05  FILLER                  PIC X(25)
                             VALUE 'NO COVERAGE: '.
           05  TL14-NO-COV             PIC ZZZ9.
           05  FILLER                  PIC X(101) VALUE SPACES.

       01  TOTAL-LINE15.
           05  FILLER                  PIC X(25)
                   VALUE "GROSS DAILY AMOUNT:".
           05  TL15-GROSS-OUT          PIC $,$$$,$99.99.
           05  FILLER                  PIC X(95) VALUE SPACES.


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING THRU 000-EXIT.
           PERFORM 100-MAINLINE THRU 100-EXIT
                   UNTIL NO-MORE-RECORDS.
           PERFORM 150-WRITE-TOTAL-LINES THRU 150-EXIT.
           PERFORM 200-CLEANUP THRU 200-EXIT.
           MOVE +0 TO RETURN-CODE.
           GOBACK.

       000-HOUSEKEEPING.
           DISPLAY "HOUSEKEEPING".
           OPEN INPUT INFILE.
           OPEN INPUT INS-TYPE-FILE.
           OPEN OUTPUT RPTFILE.
           OPEN OUTPUT ERRFILE.

           INITIALIZE  COUNTERS-AND-ACCUMULATORS,
                       WS-INPUT-REC,
                       WS-OUTPUT-REC,
                       WS-ERROR-REC.

           SET INS-TYPE-TAB-IDX TO 1.

           PERFORM 055-READ-INS-TYPE-FILE THRU 055-EXIT.

           PERFORM 050-LOAD-TABLE THRU 050-EXIT
                       UNTIL END-OF-FILE.

           PERFORM 110-READ-INFILE THRU 110-EXIT.

           PERFORM 010-WRITE-REPORT-HEADERS THRU 010-EXIT.
       000-EXIT.
           EXIT.

       050-LOAD-TABLE.
      *** READ FILE AND POPULATE INSURANCE TYPE TABLE
           MOVE INS-TYPE-REC TO INSURANCE-TYPE-CODE(INS-TYPE-TAB-IDX).
           SET INS-TYPE-TAB-IDX UP BY 1.

           PERFORM 055-READ-INS-TYPE-FILE THRU 055-EXIT.
       050-EXIT.
           EXIT.

       055-READ-INS-TYPE-FILE.
            READ INS-TYPE-FILE
           AT END
              MOVE '10' TO ITCODE.
       055-EXIT.
           EXIT.

       010-WRITE-REPORT-HEADERS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR   TO HL1-YEAR.
           MOVE WS-CURRENT-MONTH  TO HL1-MONTH.
           MOVE WS-CURRENT-DAY    TO HL1-DAY.

           MOVE 1                 TO PAGE-NUM.
           MOVE PAGE-NUM          TO HL1-PAGE-NUMBER.

           MOVE HEADER-LINE1      TO RPT-REC.
           WRITE RPT-REC.
           MOVE BLANK-LINE        TO RPT-REC.
           WRITE RPT-REC.
           MOVE HEADER-LINE2      TO RPT-REC.
           WRITE RPT-REC.
           MOVE BLANK-LINE        TO RPT-REC.
           WRITE RPT-REC.

           ADD +1                 TO PAGE-NUM.
           ADD +4                 TO LINE-COUNT.

       010-EXIT.
           EXIT.

       100-MAINLINE.
      *  Validate insurance coverage by searching table
           SET INS-TYPE-TAB-IDX TO 1.
           SEARCH INSURANCE-TYPE
           AT END
              MOVE WS-INPUT-REC  TO WS-ERROR-REC
              MOVE 'BAD INS TYPE' TO ERR-MSG
              WRITE ERR-REC FROM WS-ERROR-REC
              ADD +1 TO ERROR-RECS

              PERFORM 110-READ-INFILE THRU 110-EXIT
              GO TO 100-EXIT
           WHEN INSURANCE-TYPE-CODE(INS-TYPE-TAB-IDX) = INS-TYPE
              MOVE INSURANCE-TYPE-CODE(INS-TYPE-TAB-IDX)
                                                    TO COUNT-INS-TYPE
           END-SEARCH.
      *  Validate patient type
           IF VALID-TYPE
              CONTINUE
           ELSE
               MOVE WS-INPUT-REC TO WS-ERROR-REC
               MOVE 'BAD PAT TYPE' TO ERR-MSG
               WRITE ERR-REC FROM WS-ERROR-REC
               ADD +1 TO ERROR-RECS

               PERFORM 110-READ-INFILE THRU 110-EXIT
               GO TO 100-EXIT
           END-IF.

      *  Add to counters and total amounts
           EVALUATE TRUE
               WHEN HMO ADD +1 TO NBR-HMO
               WHEN PRI ADD +1 TO NBR-PRI
               WHEN PPO ADD +1 TO NBR-PPO
               WHEN AFF ADD +1 TO NBR-AFF
               WHEN MED ADD +1 TO NBR-MED
               WHEN POS ADD +1 TO NBR-POS
               WHEN MAN ADD +1 TO NBR-MAN
               WHEN GOV ADD +1 TO NBR-GOV
               WHEN OTHER ADD +1 TO NBR-NO-COVERAGE
           END-EVALUATE.

           IF INPATIENT
               ADD +1 TO NBR-INPATIENTS
           ELSE
               ADD +1 TO NBR-OUTPATIENTS
           END-IF.

           COMPUTE PAT-TOTAL-AMT-NET =
               (PATIENT-TOT-AMT  +
                   (AMT-PER-DAY * ((100 - INS-COVERAGE-PERC) / 100)))
           END-COMPUTE.

            ADD  PAT-TOTAL-AMT-NET  TO PATIENT-TOT-AMT
                                   GIVING DL1-AMT-PER-DAY.


           ADD PAT-TOTAL-AMT-NET    TO TOTAL-AMT-GROSS.

           PERFORM 120-WRITE-REPORT-DETAIL THRU 120-EXIT.
           PERFORM 110-READ-INFILE THRU 110-EXIT.
       100-EXIT.
           EXIT.


       110-READ-INFILE.
           READ INFILE INTO WS-INPUT-REC
               AT END
               MOVE 'N' TO MORE-RECORDS-SW
               GO TO 110-EXIT
           END-READ.
           ADD +1 TO RECORDS-READ.
       110-EXIT.
           EXIT.

       120-WRITE-REPORT-DETAIL.
      *** CHECK IF ENOUGH SPACE ON PAGE TO PRINT DETAIL LINE
           IF LINE-COUNT < REPORT-MAX-LINES
              CONTINUE
           ELSE
              PERFORM 010-WRITE-REPORT-HEADERS THRU 010-EXIT
           END-IF.

           MOVE PATIENT-NBR        TO DL1-PAT-NBR.
           MOVE PATIENT-NAME       TO DL1-PAT-NAME.

           STRING '(' PATIENT-PHONE(1:3) ')' PATIENT-PHONE(4:3) '-'
                  PATIENT-PHONE(7:4)
                  DELIMITED BY SIZE
                  INTO DL1-PHONE-NUMBER.


           MOVE PATIENT-TYPE       TO DL1-PAT-TYPE.
           MOVE BED-IDENTITY       TO DL1-BED.

           MOVE DATE-ADMIT         TO DL1-DATE-ADMIT.

           MOVE DIAGNOSTIC-CODE    TO DL1-DIAG-CODE.
           MOVE INS-TYPE           TO DL1-INS-TYPE.
           ADD  +1                 TO HOSPITAL-STAY-LTH
                                   GIVING  DL1-HOSP-STAY-LTH.

           IF IN-NETWORK
              MOVE 'IN'            TO DL1-IN-OUT-NET
           ELSE
              MOVE 'OUT'           TO DL1-IN-OUT-NET
           END-IF.

           MOVE COPAY              TO DL1-COPAY.
           MOVE DEDUCTIBLE         TO DL1-DEDUCTIBLE.

           MOVE DETAIL-LINE1       TO RPT-REC.
           WRITE RPT-REC.
           ADD +1                  TO LINE-COUNT.
           ADD +1                  TO RECORDS-WRITTEN.

       120-EXIT.
           EXIT.

       150-WRITE-TOTAL-LINES.
      *** CHECK IF ENOUGH SPACE ON PAGE TO PRINT TOTAL LINES
           IF LINE-COUNT > 45
              PERFORM 010-WRITE-REPORT-HEADERS THRU 010-EXIT
           ELSE
               MOVE BLANK-LINE     TO RPT-REC
               WRITE RPT-REC
           END-IF.

           MOVE RECORDS-READ       TO TL1-REC-IN.
           MOVE TOTAL-LINE1        TO RPT-REC.
           WRITE RPT-REC.

           MOVE RECORDS-WRITTEN    TO TL2-REC-WRITTEN.
           MOVE TOTAL-LINE2        TO RPT-REC.
           WRITE RPT-REC.

           MOVE ERROR-RECS         TO TL3-REC-ERR.
           MOVE TOTAL-LINE3        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-INPATIENTS     TO TL4-INPATIENT.
           MOVE TOTAL-LINE4        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-OUTPATIENTS    TO TL5-OUTPATIENT.
           MOVE TOTAL-LINE5        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-HMO            TO TL6-HMO.
           MOVE TOTAL-LINE6        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-PRI            TO TL7-PRI.
           MOVE TOTAL-LINE7        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-PPO            TO TL8-PPO.
           MOVE TOTAL-LINE8        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-AFF            TO TL9-AFF.
           MOVE TOTAL-LINE9        TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-MED            TO TL10-MED.
           MOVE TOTAL-LINE10       TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-MAN            TO TL11-MAN.
           MOVE TOTAL-LINE11       TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-POS            TO TL12-POS.
           MOVE TOTAL-LINE12       TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-GOV            TO TL13-GOV.
           MOVE TOTAL-LINE13       TO RPT-REC.
           WRITE RPT-REC.

           MOVE NBR-NO-COVERAGE    TO TL14-NO-COV.
           MOVE TOTAL-LINE14       TO RPT-REC.
           WRITE RPT-REC.

           MOVE TOTAL-AMT-GROSS    TO TL15-GROSS-OUT.
           MOVE TOTAL-LINE15       TO RPT-REC.
           WRITE RPT-REC.

       150-EXIT.
           EXIT.

       200-CLEANUP.
           DISPLAY "CLEAN-UP".
           CLOSE INFILE.
           CLOSE RPTFILE.
           CLOSE ERRFILE.
           CLOSE INS-TYPE-FILE.
           DISPLAY "NORMAL END OF JOB".
       200-EXIT.
           EXIT.