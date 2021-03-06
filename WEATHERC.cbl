       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEATHERC.
      ***
      *** THIS PROGRAM WILL UPDATE THE TEMP FROM AN HOURLY FILE
      *** THEN PRODUCE A REPORT
      *** ERROR CHECKING OF THE DATA HAS BEEN ADDED
      ***
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT WEATHIN  ASSIGN TO WEATHER.
           SELECT HOURIN   ASSIGN TO HOURLY.
           SELECT WEATHOUT ASSIGN TO WREPORT.
           SELECT WEATHERR ASSIGN TO WERROR.
       DATA DIVISION.
       FILE SECTION.
       FD  WEATHIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 20 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS WEATHIN-RECORD.
       01  WEATHIN-RECORD PIC X(20).

       FD  HOURIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 20 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS HOURIN-RECORD.
       01  HOURIN-RECORD PIC X(20).

       FD  WEATHOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS WEATHOUT-RECORD.
       01  WEATHOUT-RECORD PIC X(132).

       FD  WEATHERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS WEATHERR-RECORD.
       01  WEATHERR-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  WEATHER-REC-WS.
           05 DAY-WS                   PIC X(01).
           05 TIME-WS                  PIC X(07).
           05 TEMP-WS                  PIC X(02).
           05 WIND-DIRECTION-WS        PIC X(01).
           05 WIND-SPEED-WS            PIC X(02).
           05 HUMIDITY-WS              PIC X(02).

       01  HOURLY-REC-WS.
           05 DAY-HR-WS                PIC X(01).
           05 TIME-HR-WS               PIC X(07).
           05 TEMP-HR-WS               PIC X(02).

      *** TABLES

       01  WEEKDAYS-TABLE.
           05  MONDAY                  PIC X(9) VALUE 'Monday'.
           05  TUESDAY                 PIC X(9) VALUE 'Tuesday'.
           05  WEDNESDAY               PIC X(9) VALUE 'Wednesday'.
           05  THURSDAY                PIC X(9) VALUE 'Thursday'.
           05  FRIDAY                  PIC X(9) VALUE 'Friday'.
           05  SATURDAY                PIC X(9) VALUE 'Saturday'.
       01 DAY-OF-WK REDEFINES WEEKDAYS-TABLE.
           05  DT-OF-WK   OCCURS 7 TIMES PIC X(9).

       01  WIND-DIRECTION-TABLE.
           05  N                       PIC X(2)  VALUE 'N'.
           05  NE                      PIC X(2)  VALUE 'NE'.
           05  E                       PIC X(2)  VALUE 'E'.
           05  SE                      PIC X(2)  VALUE 'SE'.
           05  S                       PIC X(2)  VALUE 'S'.
           05  SW                      PIC X(2)  VALUE 'SW'.
           05  W                       PIC X(2)  VALUE 'W'.
           05  NW                      PIC X(2)  VALUE 'NW'.

       01 WIND-DIRECTIONS-TABLE REDEFINES WIND-DIRECTION-TABLE.
           05  WIND-DIR OCCURS 8 TIMES
                       INDEXED BY WIND-IDX.
              10 WIND-LETTERS          PIC X(02).


       01 WEATHER-TABLE.
           05 DAYS-TABLE OCCURS 3 TIMES
                         INDEXED BY DAY-IDX.
              10  DAY-OF-THE-WK        PIC 9(1).
              10  HOURS-TABLE OCCURS 24 TIMES
                              INDEXED BY HOUR-IDX.
                15 HT-TIME-OF-DAY      PIC X(7).
                15 HT-TEMP             PIC X(2).
                15 HT-WIND-DIR         PIC 9(1).
                15 HT-WIND-SPD         PIC X(2).
                15 HT-HUMIDITY         PIC X(2).

       01 HOURLY-UPDATE-TABLE.
          05 HOURLY-UP-TABLE OCCURS 3 TIMES
                             INDEXED BY UP-IDX.
            10 DAY-HR-TB               PIC X(01).
            10 TEMP-TABLE OCCURS 24 TIMES
                          INDEXED BY TEMP-IDX.
               15 TIME-HR-TB           PIC X(07).
               15 TEMP-HR-TB           PIC X(02).

      *** MORE VARIABLES

       01  SUBSCRIPTS-AND-FLAGS.
           05 WKDAY-SUB                PIC 99 COMP-3.
           05 END-OF-RECORDS           PIC 99.
           05 EOF                      PIC X(1)  VALUE SPACE.
              88 END-OF-FILE VALUE 'Y'.
           05 FIRST-READ               PIC X(1)  VALUE SPACE.
              88 FIRST-TIME  VALUE 'Y'.
           05 HEADERS                 PIC X(1)   VALUE SPACE.
              88 NEW-HEADER  VALUE 'Y'.
           05 ERROR-FLAG               PIC X(1)  VALUE SPACE.
              88 ERROR-FOUND VALUE 'Y'.

       01  WS-WORKING-DAY              PIC 9(1).
       01  WS-DAY-OF-WEEK              PIC X(9).
       01  WS-CURRENT-DATE-AND-TIME.
           05  WS-CUR-YEAR             PIC 9(4).
           05  WS-CUR-MONTH            PIC 9(2).
           05  WS-CUR-DAY              PIC 9(2).
           05  WS-CUR-HOURS            PIC 9(2).
           05  WS-CUR-MINUTES          PIC 9(2).
           05  WS-CUR-SECONDS          PIC 9(2).
       01  CHECK-TIME                  PIC X(7).
           88 VALID-TIME                        VALUE '12:00AM'
                                                      '01:00AM'
                                                      '02:00AM'
                                                      '03:00AM'
                                                      '04:00AM'
                                                      '05:00AM'
                                                      '06:00AM'
                                                      '07:00AM'
                                                      '08:00AM'
                                                      '09:00AM'
                                                      '10:00AM'
                                                      '11:00AM'
                                                      '12:00PM'
                                                      '01:00PM'
                                                      '02:00PM'
                                                      '03:00PM'
                                                      '04:00PM'
                                                      '05:00PM'
                                                      '06:00PM'
                                                      '07:00PM'
                                                      '08:00PM'
                                                      '09:00PM'
                                                      '10:00PM'
                                                      '11:00PM'.
       01  CHECK-WIND                  PIC X(1).
           88 VALID-WIND                          VALUE '1'
                                                        '2'
                                                        '3'
                                                        '4'
                                                        '5'
                                                        '6'
                                                        '7'
                                                        '8'.

      *** REPORT SECTION
       01 BLANK-LINE                   PIC X(132) VALUE SPACES.
       01 REPORT-MAX-LINES             PIC 9(2)   VALUE 60.
       01 PAGE-NUM                     PIC 99     VALUE 0.
       01 LINE-COUNT                   PIC 99     VALUE 0.
       01 ERROR-LINE.
          05 ERROR-DATA                PIC X(15)  VALUE SPACES.
          05 ERROR-MSG                 PIC X(65)  VALUE SPACES.

       01 HEADER-LINE1.
          05 FILLER                    PIC X(124) VALUE
             'Hourly Weather: Bangor, ME'.
          05  FILLER                   PIC X(6)  VALUE 'PAGE: '.
          05  HL1-PAGE-NUM             PIC Z9.

       01 HEADER-LINE2.
          05  FILLER                   PIC X(6)  VALUE 'DATE: '.
          05  HL2-DATE.
              10  HL2-MM               PIC 9(2).
              10  SLASH-1              PIC X(1)  VALUE "/".
              10  HL2-DD               PIC 9(2).
              10  SLASH-2              PIC X(1)  VALUE "/".
              10  HL2-YY               PIC 9(4).
          05  FILLER                   PIC X(5)  VALUE SPACES.
          05  FILLER                   PIC X(6)  VALUE 'TIME: '.
          05  HL2-TIME.
              10  HL2-HH               PIC 9(2).
              10  COLON-1              PIC X(1)  VALUE ":".
              10  HL2-MIN              PIC 9(2).
              10  COLON-2              PIC X(1)  VALUE ":".
              10  HL2-SS               PIC 9(2).
          05  FILLER                   PIC X(97) VALUE SPACES.

       01 HEADER-LINE3.
          05 FILLER                    PIC X(2)  VALUE SPACES.
          05 FILLER                    PIC X(4)  VALUE 'DATE'.
          05 FILLER                    PIC X(10) VALUE SPACES.
          05 FILLER                    PIC X(4)  VALUE 'TIME'.
          05 FILLER                    PIC X(08) VALUE SPACES.
          05 FILLER                    PIC X(4)  VALUE 'TEMP'.
          05 FILLER                    PIC X(12) VALUE SPACES.
          05 FILLER                    PIC X(4)  VALUE 'WIND'.
          05 FILLER                    PIC X(8)  VALUE SPACES.
          05 FILLER                    PIC X(8)  VALUE 'HUMIDITY'.
          05 FILLER                    PIC X(16) VALUE SPACES.


       01 HEADER-LINE4.
          05 FILLER                    PIC X(9)  VALUE ALL '='.
          05 FILLER                    PIC X(5)  VALUE SPACES.
          05 FILLER                    PIC X(8)  VALUE ALL '='.
          05 FILLER                    PIC X(6)  VALUE SPACES.
          05 FILLER                    PIC X(4)  VALUE ALL '='.
          05 FILLER                    PIC X(10) VALUE SPACES.
          05 FILLER                    PIC X(9)  VALUE ALL '='.
          05 FILLER                    PIC X(5)  VALUE SPACES.
          05 FILLER                    PIC X(8)  VALUE ALL '='.
          05 FILLER                    PIC X(16) VALUE SPACES.

       01 DETAIL-LINE1.
          05 DL1-WEEKDAY               PIC X(9).
          05 FILLER                    PIC X(5)  VALUE SPACES.
          05 DL1-TIME                  PIC X(8).
          05 FILLER                    PIC X(7)  VALUE SPACES.
          05 DL1-TEMP                  PIC X(2).
          05 FILLER                    PIC X(11) VALUE SPACES.
          05 DL1-WIND-DIR              PIC X(2).
          05 FILLER                    PIC X(1)  VALUE SPACES.
          05 DL1-WIND-SPD              PIC X(2).
          05 FILLER                    PIC X(4)  VALUE ' mph'.
          05 FILLER                    PIC X(8)  VALUE SPACES.
          05 DL1-HUMIDITY              PIC X(2).
          05 FILLER                    PIC X(1)  VALUE '%'.
          05 FILLER                    PIC X(18) VALUE SPACES.

        01 DETAIL-LINE2.
          05 FILLER                    PIC X(14) VALUE SPACES.
          05 DL2-TIME                  PIC X(8).
          05 FILLER                    PIC X(7)  VALUE SPACES.
          05 DL2-TEMP                  PIC X(2).
          05 FILLER                    PIC X(11) VALUE SPACES.
          05 DL2-WIND-DIR              PIC X(2).
          05 FILLER                    PIC X(1)  VALUE SPACES.
          05 DL2-WIND-SPD              PIC X(2).
          05 FILLER                    PIC X(4)  VALUE ' mph'.
          05 FILLER                    PIC X(8)  VALUE SPACES.
          05 DL2-HUMIDITY              PIC X(2).
          05 FILLER                    PIC X(1)  VALUE '%'.
          05 FILLER                    PIC X(18) VALUE SPACES.


       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-HOUSEKEEPING THRU 100-EXIT.
           PERFORM 500-PROCESS-RECORDS UNTIL END-OF-FILE.
           PERFORM 110-HOURLY-PREP THRU 110-EXIT.
           PERFORM 550-PROCESS-HOURLY UNTIL END-OF-FILE.

           PERFORM 570-UPDATE-TEMP VARYING UP-IDX
              FROM 1 BY 1 UNTIL UP-IDX > 3.

           SET HOUR-IDX TO 1.
           PERFORM 600-PRINT-REPORT VARYING DAY-IDX
              FROM 1 BY 1 UNTIL DAY-IDX > 3.
           PERFORM 900-CLOSE-FILES.
           GOBACK.

       100-HOUSEKEEPING.
           INITIALIZE WEATHER-REC-WS,
                      WEATHER-TABLE,
                      WS-CURRENT-DATE-AND-TIME,
                      SUBSCRIPTS-AND-FLAGS,
                      WS-WORKING-DAY,
                      WS-DAY-OF-WEEK,
                      WS-CURRENT-DATE-AND-TIME.

           PERFORM 200-OPEN-FILES THRU 200-EXIT.
           PERFORM 210-REPORT-PREP THRU 210-EXIT.
           PERFORM 300-WRITE-REPORT-HEADERS THRU 300-EXIT.
           PERFORM 400-READ-WEATHER THRU 400-EXIT.
           PERFORM 410-READ-HOURLY THRU 410-EXIT.
           MOVE 'Y' TO FIRST-READ.
       100-EXIT.
           EXIT.

       110-HOURLY-PREP.
           MOVE ZEROES       TO WS-WORKING-DAY.
           MOVE 'N'          TO EOF.
           SET UP-IDX        TO 1.
           MOVE 'Y'          TO FIRST-READ.
       110-EXIT.
           EXIT.

       200-OPEN-FILES.
           OPEN INPUT WEATHIN, HOURIN.
           OPEN OUTPUT WEATHOUT, WEATHERR.
       200-EXIT.
           EXIT.

       210-REPORT-PREP.
           MOVE 1 TO PAGE-NUM.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-AND-TIME.
           MOVE WS-CUR-YEAR     TO HL2-YY.
           MOVE WS-CUR-MONTH    TO HL2-MM.
           MOVE WS-CUR-DAY      TO HL2-DD.
           MOVE WS-CUR-HOURS    TO HL2-HH.
           MOVE WS-CUR-MINUTES  TO HL2-MIN.
           MOVE WS-CUR-SECONDS  TO HL2-SS.
       210-EXIT.
           EXIT.

       300-WRITE-REPORT-HEADERS.
           MOVE PAGE-NUM          TO HL1-PAGE-NUM.

           MOVE HEADER-LINE1      TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.
           MOVE HEADER-LINE2      TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.
           MOVE BLANK-LINE        TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.
           MOVE HEADER-LINE3      TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.
           MOVE HEADER-LINE4      TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.

           ADD 1             TO PAGE-NUM.
           MOVE 6            TO LINE-COUNT.
       300-EXIT.
           EXIT.

       400-READ-WEATHER.
           MOVE 'N' TO ERROR-FLAG.
           READ WEATHIN
             AT END MOVE 'Y' TO EOF
           END-READ.
           IF NOT END-OF-FILE
             MOVE WEATHIN-RECORD TO WEATHER-REC-WS
             PERFORM 420-ERROR-CHECK-WEATHER THRU 420-EXIT
           END-IF.

       400-EXIT.
           EXIT.

       410-READ-HOURLY.
           MOVE 'N' TO ERROR-FLAG.
           READ HOURIN
             AT END MOVE 'Y' TO EOF
           END-READ.
           IF NOT END-OF-FILE
             MOVE HOURIN-RECORD TO HOURLY-REC-WS
             PERFORM 430-ERROR-CHECK-HOURLY THRU 430-EXIT
           END-IF.

       410-EXIT.
           EXIT.

       420-ERROR-CHECK-WEATHER.
      *** DAY MUST BE 1, 2 OR 3
           IF DAY-WS = 1 OR 2 OR 3
              CONTINUE
           ELSE
              MOVE WEATHER-REC-WS       TO ERROR-DATA
              MOVE 'DAY NOT 1, 2, OR 3' TO ERROR-MSG
              MOVE ERROR-LINE           TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** TIME MUST BE 12:00AM THRU 11:00PM
           MOVE TIME-WS     TO CHECK-TIME.
           IF VALID-TIME
              CONTINUE
           ELSE
              MOVE WEATHER-REC-WS          TO ERROR-DATA
              MOVE 'TIME FORMAT NOT VALID' TO ERROR-MSG
              MOVE ERROR-LINE              TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** TEMP MUST BE BETWEEN 32 AND 99F
           IF TEMP-WS >= 32 AND <= 99
              CONTINUE
           ELSE
              MOVE WEATHER-REC-WS                TO ERROR-DATA
              MOVE 'TEMP NOT BETWEEN 32 AND 99F' TO ERROR-MSG
              MOVE ERROR-LINE                    TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** WIND DIRECTION MUST BE 1 THRU 8
           MOVE WIND-DIRECTION-WS TO CHECK-WIND.
           IF VALID-WIND
              CONTINUE
           ELSE
              MOVE WEATHER-REC-WS           TO ERROR-DATA
              MOVE 'WIND DIRECTION NOT 1-8' TO ERROR-MSG
              MOVE ERROR-LINE               TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** WIND SPEED MUST BE >0 AND <99
           IF WIND-SPEED-WS > 0 AND < 99
              CONTINUE
           ELSE
              MOVE WEATHER-REC-WS              TO ERROR-DATA
              MOVE 'WIND SPEED NOT >0 AND <99' TO ERROR-MSG
              MOVE ERROR-LINE                  TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** HUMIDITY MUST BE >30 AND <80
           IF HUMIDITY-WS > 30 AND < 80
              CONTINUE
           ELSE
              MOVE WEATHER-REC-WS              TO ERROR-DATA
              MOVE 'HUMIDITY NOT >30 AND >80'  TO ERROR-MSG
              MOVE ERROR-LINE                  TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

       420-EXIT.
           EXIT.

       430-ERROR-CHECK-HOURLY.
      *** DAY MUST BE 1, 2 OR 3
           IF DAY-HR-WS = 1 OR 2 OR 3
              CONTINUE
           ELSE
              MOVE HOURLY-REC-WS         TO ERROR-DATA
              MOVE ' DAY NOT 1, 2, OR 3' TO ERROR-MSG
              MOVE ERROR-LINE            TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** TIME MUST BE 12:00AM THRU 11:00PM
           MOVE TIME-HR-WS     TO CHECK-TIME.
           IF VALID-TIME
              CONTINUE
           ELSE
              MOVE HOURLY-REC-WS            TO ERROR-DATA
              MOVE ' TIME FORMAT NOT VALID' TO ERROR-MSG
              MOVE ERROR-LINE               TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.

      *** TEMP MUST BE BETWEEN 32 AND 99F
           IF TEMP-HR-WS >= 32 AND <= 99
              CONTINUE
           ELSE
              MOVE HOURLY-REC-WS                  TO ERROR-DATA
              MOVE ' TEMP NOT BETWEEN 32 AND 99F' TO ERROR-MSG
              MOVE ERROR-LINE                     TO WEATHERR-RECORD
              WRITE WEATHERR-RECORD
              MOVE 'Y' TO ERROR-FLAG
           END-IF.


       430-EXIT.
           EXIT.

       500-PROCESS-RECORDS.
           IF ERROR-FOUND
              MOVE 'N' TO ERROR-FLAG
              CONTINUE
           ELSE
             IF WS-WORKING-DAY = DAY-WS
               SET HOUR-IDX UP BY 1
               PERFORM 510-MOVE-FIELDS THRU 510-EXIT
             ELSE
               MOVE DAY-WS             TO WS-WORKING-DAY
               IF FIRST-TIME
                SET DAY-IDX TO 1
                MOVE 'N' TO FIRST-READ
               ELSE
                SET DAY-IDX UP BY 1
               END-IF
               SET HOUR-IDX TO 1
               MOVE DAY-WS             TO DAY-OF-THE-WK(DAY-IDX)
               PERFORM 510-MOVE-FIELDS
             END-IF
           END-IF.

           PERFORM 400-READ-WEATHER THRU 400-EXIT.

       500-EXIT.
           EXIT.

       510-MOVE-FIELDS.
           MOVE TIME-WS            TO HT-TIME-OF-DAY(DAY-IDX, HOUR-IDX).
           MOVE TEMP-WS            TO HT-TEMP(DAY-IDX, HOUR-IDX).
           MOVE WIND-DIRECTION-WS  TO HT-WIND-DIR(DAY-IDX, HOUR-IDX).
           MOVE WIND-SPEED-WS      TO HT-WIND-SPD(DAY-IDX, HOUR-IDX).
           MOVE HUMIDITY-WS        TO HT-HUMIDITY(DAY-IDX, HOUR-IDX).
       510-EXIT.
           EXIT.


       550-PROCESS-HOURLY.
           IF WS-WORKING-DAY = DAY-HR-WS
             SET TEMP-IDX UP BY 1
             PERFORM 560-MOVE-FIELDS THRU 560-EXIT
           ELSE
             MOVE DAY-HR-WS             TO WS-WORKING-DAY
             IF FIRST-TIME
                SET UP-IDX TO 1
                MOVE 'N' TO FIRST-READ
             ELSE
                SET UP-IDX UP BY 1
             END-IF
             SET TEMP-IDX TO 1
             MOVE DAY-HR-WS            TO DAY-HR-TB(UP-IDX)
             PERFORM 560-MOVE-FIELDS
           END-IF.

           PERFORM 410-READ-HOURLY THRU 410-EXIT.

       550-EXIT.
           EXIT.

       560-MOVE-FIELDS.
           MOVE TIME-HR-WS         TO TIME-HR-TB(UP-IDX, TEMP-IDX).
           MOVE TEMP-HR-WS         TO TEMP-HR-TB(UP-IDX, TEMP-IDX).

       560-EXIT.
           EXIT.


       570-UPDATE-TEMP.
           PERFORM VARYING TEMP-IDX FROM 1 BY 1
                   UNTIL TEMP-IDX > 24
                 PERFORM 580-UPDATE-TEMP
           END-PERFORM.
       570-EXIT.
           EXIT.

       580-UPDATE-TEMP.
           SET DAY-IDX TO UP-IDX.
           SET HOUR-IDX TO TEMP-IDX.
           MOVE TEMP-HR-TB(UP-IDX, TEMP-IDX) TO
                                           HT-TEMP(DAY-IDX, HOUR-IDX).
       580-EXIT.
           EXIT.


       600-PRINT-REPORT.
      *** DID THE DAY CHANGE?
           IF DAY-OF-THE-WK(DAY-IDX) NOT = WKDAY-SUB
              SET HOUR-IDX TO 1
           END-IF.
           PERFORM 640-TRANSLATE-DAY THRU 640-EXIT.
           PERFORM 610-TRANSLATE-WIND THRU 610-EXIT.
           PERFORM 620-PRINT-DETAIL-LINE1 THRU 620-EXIT.

           PERFORM VARYING HOUR-IDX FROM 2 BY 1
                   UNTIL HOUR-IDX > 24
                 IF HT-TIME-OF-DAY(DAY-IDX, HOUR-IDX) = SPACES
                    CONTINUE
                 ELSE
                    PERFORM 610-TRANSLATE-WIND
                    PERFORM 630-PRINT-DETAIL-LINE2
                 END-IF
           END-PERFORM.

       600-EXIT.
           EXIT.

       610-TRANSLATE-WIND.
           SET WIND-IDX TO HT-WIND-DIR(DAY-IDX, HOUR-IDX).
           MOVE WIND-LETTERS(WIND-IDX) TO DL1-WIND-DIR,
                                          DL2-WIND-DIR.
       610-EXIT.
           EXIT.

       620-PRINT-DETAIL-LINE1.
           IF LINE-COUNT >= REPORT-MAX-LINES
              PERFORM 300-WRITE-REPORT-HEADERS THRU 300-EXIT
           END-IF.

           MOVE BLANK-LINE TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.

           MOVE WS-DAY-OF-WEEK                    TO DL1-WEEKDAY.
           MOVE HT-TIME-OF-DAY(DAY-IDX, HOUR-IDX) TO DL1-TIME.
           MOVE HT-TEMP(DAY-IDX, HOUR-IDX)        TO DL1-TEMP.
           MOVE HT-WIND-SPD(DAY-IDX, HOUR-IDX)    TO DL1-WIND-SPD.
           MOVE HT-HUMIDITY(DAY-IDX, HOUR-IDX)    TO DL1-HUMIDITY.
           MOVE DETAIL-LINE1                      TO WEATHOUT-RECORD.
           WRITE WEATHOUT-RECORD.
           ADD 2                                  TO LINE-COUNT.

       620-EXIT.
           EXIT.

       630-PRINT-DETAIL-LINE2.
           IF LINE-COUNT >= REPORT-MAX-LINES
              PERFORM 300-WRITE-REPORT-HEADERS THRU 300-EXIT
              PERFORM 620-PRINT-DETAIL-LINE1
              MOVE 'Y' TO HEADERS
           END-IF.

           IF NEW-HEADER
              MOVE 'N' TO HEADERS
              CONTINUE
           ELSE
              MOVE HT-TIME-OF-DAY(DAY-IDX, HOUR-IDX) TO DL2-TIME
              MOVE HT-TEMP(DAY-IDX, HOUR-IDX)        TO DL2-TEMP
              MOVE HT-WIND-SPD(DAY-IDX, HOUR-IDX)    TO DL2-WIND-SPD
              MOVE HT-HUMIDITY(DAY-IDX, HOUR-IDX)    TO DL2-HUMIDITY
              MOVE DETAIL-LINE2                      TO WEATHOUT-RECORD
              WRITE WEATHOUT-RECORD
              ADD 1                                  TO LINE-COUNT
           END-IF.

       630-EXIT.
           EXIT.


       640-TRANSLATE-DAY.
           MOVE DAY-OF-THE-WK(DAY-IDX) TO WKDAY-SUB.
           MOVE DT-OF-WK(WKDAY-SUB)    TO WS-DAY-OF-WEEK.
       640-EXIT.
           EXIT.



       900-CLOSE-FILES.
           CLOSE WEATHIN,
                 HOURIN,
                 WEATHOUT,
                 WEATHERR.
       900-EXIT.
           EXIT.