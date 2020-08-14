      *-----------------------
       IDENTIFICATION DIVISION.
      *-----------------------
       PROGRAM-ID.    CNTRLBRK.
       AUTHOR.        SAYLES.
      *--------------------
       ENVIRONMENT DIVISION.
      *--------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PRINT-LINE ASSIGN TO PRTLINE.
           SELECT ACCT-REC   ASSIGN TO ACCTSSRT.
      *-------------
       DATA DIVISION.
      *-------------
       FILE SECTION.
       FD  PRINT-LINE RECORDING MODE F.
       01  PRINT-REC.
           05 FILLER                    PIC X(03)     VALUE SPACE.
           05 USA-STATE-O               PIC X(18).
           05 FIRST-NAME-O              PIC X(15).
           05 LAST-NAME-O               PIC X(20).
           05 ELECTED-O                 PIC X(6).
           05 LAST-YEAR-O               PIC X(6).
           05 ACCT-LIMIT-O              PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(03)     VALUE SPACES.
           05 ACCT-BALANCE-O            PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(02)     VALUE SPACES.
           05 SALARY-ACCUM-O            PIC $$,$$$,$$9.99.
           05 FILLER                    PIC X(14)     VALUE SPACES.
      *
       FD  ACCT-REC RECORDING MODE F.
       01  ACCT-FIELDS.
           05  ACCT-NO                 PIC X(8).
           05  ACCT-YEAR REDEFINES ACCT-NO.
               10  YEAR-IN             PIC 9(4).
               10  YEAR-OUT            PIC 9(4).
           05  ACCT-LIMIT              PIC S9(7)V99. *> Salary
           05  ACCT-BALANCE            PIC S9(7)V99. *> Net Worth
           05  LAST-NAME               PIC X(20).
           05  FIRST-NAME              PIC X(15).
           05  CLIENT-ADDR.
               10  STREET-ADDR         PIC X(25).
               10  CITY-COUNTY         PIC X(20).
               10  USA-STATE           PIC X(15).  *> Input Sort Key
           05  RESERVED                PIC X(7).
           05  COMMENTS                PIC X(42).
      *
       WORKING-STORAGE SECTION.
       01 PROGRAM-INDICATOR-SWITCHES.
           05 WS-EOF-INPUT-SW          PIC X(1)       VALUE 'N'.
               88 EOF-INPUT                           VALUE 'Y'.

       01 NUMBER-OF-YEARS              PIC 9(2)       VALUE 0.
       01 SALARY-ACCUM                 PIC 9(7)V99    VALUE 0.
       01 VALUE-FOUND-SW               PIC X(1)       VALUE 'N'.
          88 VALUE-FOUND                              VALUE 'Y'.
       01 FOUND-IDX                    PIC 9(2)       VALUE 0.

       01 WS-BREAK-CONTROLS.
           05 WS-CONTROL-KEY           PIC X(15). *> Hold/Control Key

      *** TABLE TO ACCUMULATE SALARY DATA
       01  SALARY-TABLE.
           05  SALARY-TAB OCCURS 44 TIMES
                          INDEXED BY SAL-IDX.
               10 SAL-TAB-LNAME        PIC X(20).
               10 SAL-TAB-FNAME        PIC X(15).
               10 SAL-ACCUM            PIC 9(7)V99.

       01 SALARIES.
          05  TOTAL-SALARIES           PIC 9(10)V99.
          05  HIGHEST-SALARY           PIC 9(7)V99.
          05  LOWEST-SALARY            PIC 9(7)V99.
          05  AVERAGE-SALARY           PIC 9(7)V99.


      *************************************************************
      ****** Report headings begin here ******
      *************************************************************
       01 WS-BLANK-LINE                 PIC X(133)     VALUE SPACES.

       01 WS-HEADER-1.
           05 FILLER                    PIC X(1)       VALUE SPACES.
           05 FILLER                    PIC X(12)      VALUE
                                                         'Report: A124'.
           05 DATE-O                    PIC X(10)      VALUE SPACE.
           05 FILLER                    PIC X(13)      VALUE SPACES.
           05 FILLER                    PIC X(47)
                                                       VALUE
                              'Presidents Broken Out By State of Birth'.
           05 RPT-DATE                  PIC XXXX/XX/XX.
           05 FILLER                    PIC X(10)      VALUE SPACES.
           05 FILLER                    PIC X(5)       VALUE 'PAGE '.
           05 RPT-PAGE-NO               PIC ZZ.
           05 FILLER                    PIC X(12)      VALUE SPACES.

       01 WS-HEADER-2.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 FILLER                    PIC X(18)      VALUE 'STATE'.
           05 FILLER                    PIC X(9)       VALUE 'PRESIDENT'
                                                                      .
           05 FILLER                    PIC X(24)      VALUE SPACES.
           05 FILLER                    PIC X(7)       VALUE 'ELECTED'.
           05 FILLER                    PIC X(1)       VALUE SPACES.
           05 FILLER                    PIC X(8)       VALUE 'THRU'.
           05 FILLER                    PIC X(14)     VALUE 'SALARY'.
           05 FILLER                    PIC X(16)  VALUE '   NET WORTH'.
           05 FILLER                    PIC X(25)  VALUE 'SALARY ACCUM'.

       01  WS-HEADER-3.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 FILLER                    PIC X(17)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(32)      VALUE ALL '='.
           05 FILLER                    PIC X(01)      VALUE SPACE.
           05 FILLER                    PIC X(7)       VALUE '======='.
           05 FILLER                    PIC X(1)       VALUE SPACES.
           05 FILLER                    PIC X(7)        VALUE '====='.
           05 FILLER                    PIC X(01)      VALUE SPACES.
           05 FILLER                    PIC X(12)       VALUE ALL '='.
           05 FILLER                    PIC X(2)       VALUE SPACES.
           05 FILLER                    PIC X(13)      VALUE
                                                        '============='.
           05 FILLER                    PIC X(3)       VALUE SPACES.
           05 FILLER                    PIC X(12)   VALUE ALL '='.
      *************************************************************
      ****** Control Break Subtotal Line ******
      *************************************************************
       01  WS-TRLR-LINE-1.
           05 FILLER                    PIC X(03)       VALUE SPACES.
           05 FILLER                    PIC X(12) VALUE 'Sub Totals:'.
           05 STATE-TRLR-LINE           PIC X(15).
           05 FILLER                    PIC X(16) VALUE SPACE.
           05 FILLER                    PIC X(21)
                            VALUE 'Salary | Net Worth: ' JUST RIGHT.
           05 SALARY-SUB-TOT-OUT        PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(02)       VALUE SPACES.
           05 NET-WORTH-SUB-TOT-OUT     PIC $$$,$$$,$$$.99.
           05 FILLER                    PIC X(17)      VALUE SPACE.
      *************************************************************
      ****** FINAL REPORT LINES
      *************************************************************
       01 FINAL-LINE1.
          05  FILLER                   PIC X(35) VALUE
               "Total of all presidents' Salaries:".
          05  FILLER                   PIC X(5)  VALUE SPACES.
          05  FL1-TOTAL-SALARIES       PIC $,$$$,$$$,999.99.
          05  FILLER                   PIC X(76) VALUE SPACES.

       01 FINAL-LINE2.
          05  FILLER                   PIC X(35) VALUE
               'President that has highest Salary: '.
          05  FILLER                   PIC X(5)  VALUE SPACES.
          05  FL2-HIGHEST-SALARY       PIC $,$$$,$$$,999.99.
          05  FILLER                   PIC X(5)  VALUE SPACES.
          05 FL2-FNAME                 PIC X(15).
          05 FL2-LNAME                 PIC X(20).
          05  FILLER                   PIC X(36) VALUE SPACES.

       01  FINAL-LINE3.
          05  FILLER                   PIC X(38) VALUE
               'President that has the lowest Salary: '.
          05  FILLER                   PIC X(2)  VALUE SPACES.
          05  FL3-LOWEST-SALARY        PIC $,$$$,$$$,999.99.
          05  FILLER                   PIC X(5)  VALUE SPACES.
          05  FL3-FNAME                PIC X(15).
          05  FL3-LNAME                PIC X(20).
          05  FILLER                   PIC X(36) VALUE SPACES.

       01 FINAL-LINE4.
          05  FILLER                   PIC X(37) VALUE
               'The average salary for all presidents'.
          05  FILLER                   PIC X(3)  VALUE SPACES.
          05  FL1-AVG-SALARIES         PIC $,$$$,$$$,999.99.
          05  FILLER                   PIC X(76) VALUE SPACES.

       01 WS-COUNTERS-AND-ACCUMULATORS.
           05 WS-CONTROL-BREAK-TOTAL    PIC S9(7)V99 COMP-3.
           05 WS-STATE-CTR              PIC  9(2) COMP.


       01 WS-FLAGS.
           05 WS-LASTREC                PIC X          VALUE SPACE.
           05 WS-LINE-KTR               PIC 9(4) COMP  VALUE 0.
           05 WS-SALARY-SUB-TOT          PIC 9(09)V99 VALUE 0.
           05 WS-NET-WORTH-SUB-TOT      PIC 9(09)V99 VALUE 0.
      *------------------
       PROCEDURE DIVISION.
      *------------------
           PERFORM 100-INIT-RTN *> Housekeeping, Initial Report Headings
           PERFORM 300-PROCESS-RECORDS UNTIL EOF-INPUT
           PERFORM 500-CONTROL-BREAK *> Final Control Break paragraphs
           PERFORM 800-WRITE-FINAL-LINES
           PERFORM 900-WRAP-UP
           GOBACK
           .
       100-INIT-RTN.
           MOVE FUNCTION CURRENT-DATE TO RPT-DATE.
           PERFORM 110-INIT-TABLE.
           PERFORM 150-INIT-WS-FIELDS
           PERFORM 200-OPEN-FILES
           MOVE SPACES TO PRINT-REC
           PERFORM 700-READ-RECORD
           PERFORM 500-CONTROL-BREAK *> Initial Control creates Rpt Head
           .

       110-INIT-TABLE.
      *** NOT ADDED TO 150 SINCE NEEDS TO BE DONE ONLY ONCE
           INITIALIZE SALARY-TABLE,
                      SALARIES.
            SET SAL-IDX TO 1.
       150-INIT-WS-FIELDS.
           INITIALIZE WS-COUNTERS-AND-ACCUMULATORS
           .
       200-OPEN-FILES.
           OPEN INPUT ACCT-REC
           OPEN OUTPUT PRINT-LINE
           .
       300-PROCESS-RECORDS.
           IF NOT EOF-INPUT   *> No duplicating last record
               IF WS-CONTROL-KEY = USA-STATE *> Control Break Conditiona
                   PERFORM 400-MOVE-DATA
                   PERFORM 600-WRITE-DATA
                   PERFORM 700-READ-RECORD
               ELSE
                   PERFORM 500-CONTROL-BREAK
               END-IF
           END-IF
           .
       400-MOVE-DATA.
           MOVE SPACES TO PRINT-REC
           ADD +1 TO WS-STATE-CTR
           IF WS-STATE-CTR > 1 *> Logic to create outline view in State
                MOVE SPACES TO USA-STATE-O
           ELSE
                MOVE USA-STATE TO USA-STATE-O,  *> MOVE IN-STATE -> HOLD
                                  STATE-TRLR-LINE
           END-IF
           ADD ACCT-LIMIT TO WS-SALARY-SUB-TOT.
           ADD ACCT-BALANCE TO WS-NET-WORTH-SUB-TOT
      *** The ACCT file is actually a repurposed file for the presidents
      *** The first four bytes is their inaugural yr => last year in off
           MOVE ACCT-NO(1:4) TO ELECTED-O
           MOVE ACCT-NO(5:4) TO LAST-YEAR-O
           MOVE ACCT-LIMIT TO ACCT-LIMIT-O
           MOVE ACCT-BALANCE TO ACCT-BALANCE-O
           MOVE LAST-NAME TO LAST-NAME-O
           MOVE FIRST-NAME TO FIRST-NAME-O.

      *** SALARY ACCUM = # OF YEARS IN OFFICE * SALARY
           COMPUTE NUMBER-OF-YEARS = YEAR-OUT - YEAR-IN.
           COMPUTE SALARY-ACCUM = NUMBER-OF-YEARS * ACCT-LIMIT.
           MOVE SALARY-ACCUM TO SALARY-ACCUM-O.

      *** SAVE NAME AND SALARY FOR END OF REPORT DATA
           MOVE LAST-NAME  TO SAL-TAB-LNAME (SAL-IDX).
           MOVE FIRST-NAME TO SAL-TAB-FNAME (SAL-IDX).
           MOVE ACCT-LIMIT TO SAL-ACCUM (SAL-IDX).
           SET SAL-IDX UP BY 1.

       500-CONTROL-BREAK.
           IF WS-LINE-KTR > 0  *> Check for first time (beginning of pro
                MOVE WS-SALARY-SUB-TOT TO SALARY-SUB-TOT-OUT
                MOVE WS-NET-WORTH-SUB-TOT TO NET-WORTH-SUB-TOT-OUT
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-TRLR-LINE-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-BLANK-LINE
           END-IF
           IF NOT EOF-INPUT
                ADD +1 TO WS-LINE-KTR
                MOVE ZERO TO WS-SALARY-SUB-TOT, WS-NET-WORTH-SUB-TOT
                MOVE WS-LINE-KTR TO RPT-PAGE-NO
                MOVE USA-STATE TO WS-CONTROL-KEY *> SET NEW CONTROL KEY
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-1
                WRITE PRINT-REC FROM WS-BLANK-LINE
                WRITE PRINT-REC FROM WS-HEADER-2
                WRITE PRINT-REC FROM WS-HEADER-3
                PERFORM 150-INIT-WS-FIELDS
           END-IF
           .
       600-WRITE-DATA.
           WRITE PRINT-REC
           .
       700-READ-RECORD.
           READ ACCT-REC
           AT END
              MOVE 'Y' TO WS-EOF-INPUT-SW
           END-READ.

       800-WRITE-FINAL-LINES.
      *** TOTAL OF ALL PRESIDENTS' SALARIES
           COMPUTE TOTAL-SALARIES = FUNCTION SUM (SAL-ACCUM (ALL)).
           MOVE TOTAL-SALARIES TO FL1-TOTAL-SALARIES.
           WRITE PRINT-REC FROM FINAL-LINE1.
           WRITE PRINT-REC FROM WS-BLANK-LINE.

      *** PRESIDENT THAT HAS HIGHEST SALARY
           COMPUTE HIGHEST-SALARY = FUNCTION MAX (SAL-ACCUM (ALL)).
           MOVE HIGHEST-SALARY TO FL2-HIGHEST-SALARY.

           MOVE 'N' TO VALUE-FOUND-SW.

           PERFORM VARYING SAL-IDX FROM 1 BY 1
               UNTIL SAL-IDX > 44 OR VALUE-FOUND
               IF SAL-ACCUM (SAL-IDX) = HIGHEST-SALARY
                  MOVE 'Y' TO VALUE-FOUND-SW
                  MOVE SAL-TAB-FNAME (SAL-IDX) TO FL2-FNAME
                  MOVE SAL-TAB-LNAME (SAL-IDX) TO FL2-LNAME
               END-IF
           END-PERFORM.

           WRITE PRINT-REC FROM FINAL-LINE2.
           WRITE PRINT-REC FROM WS-BLANK-LINE.

      *** THE AVERAGE SALARY FOR ALL PRESIDENTS
           COMPUTE AVERAGE-SALARY = FUNCTION MEAN (SAL-ACCUM (ALL)).
           MOVE AVERAGE-SALARY TO FL1-AVG-SALARIES.
           WRITE PRINT-REC FROM FINAL-LINE4.
           WRITE PRINT-REC FROM WS-BLANK-LINE.


      *** PRESIDENT THAT HAS LOWEST SALARY
           COMPUTE LOWEST-SALARY = FUNCTION MIN (SAL-ACCUM (ALL)).
           MOVE LOWEST-SALARY TO FL3-LOWEST-SALARY.

           MOVE 'N' TO VALUE-FOUND-SW.

           PERFORM VARYING SAL-IDX FROM 1 BY 1
               UNTIL SAL-IDX > 44 OR VALUE-FOUND
               IF SAL-ACCUM (SAL-IDX) = LOWEST-SALARY
                     MOVE 'Y' TO VALUE-FOUND-SW
                     MOVE SAL-TAB-FNAME (SAL-IDX) TO FL3-FNAME
                     MOVE SAL-TAB-LNAME (SAL-IDX) TO FL3-LNAME
                     WRITE PRINT-REC FROM FINAL-LINE3
                     SET FOUND-IDX TO SAL-IDX
                     ADD 1 TO FOUND-IDX
               END-IF
           END-PERFORM.

      *** THE FILE CONTAINS MORE THAN ONE PRESIDENT WITH THE
      *** LOWEST SALARY - READ TABLE AND PRINT EACH OCCURANCE
           PERFORM VARYING SAL-IDX FROM FOUND-IDX BY 1
               UNTIL SAL-IDX > 44
               IF SAL-ACCUM (SAL-IDX) = LOWEST-SALARY
                     MOVE SAL-TAB-FNAME (SAL-IDX) TO FL3-FNAME
                     MOVE SAL-TAB-LNAME (SAL-IDX) TO FL3-LNAME
                     WRITE PRINT-REC FROM FINAL-LINE3
               ELSE
                     CONTINUE
               END-IF
           END-PERFORM.

       900-WRAP-UP.
           CLOSE ACCT-REC
           CLOSE PRINT-LINE
           .