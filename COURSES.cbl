
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COURSES.
      **********************************************************
      *  THIS PROGRAM CREATES A REPORT TO LIST STUDENTS AND
      *  A BREAKOUT OF THEIR COURSES
      *  REPORT ALSO LISTS THE FOLLOWING:
      *    TOTAL NUMBER OF STUDENTS
      *    STUDENT WITH HIGHEST QPA (QUALITY POINT AVERAGE)
      *    STUDENT WITH LOWEST QPA
      *    AVERAGE QPA FOR ALL STUDENTS
      *
      *  THERE IS ALSO A PARAGRAPH TO ANSWER QUERIES
      *
      **********************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT COURIN  ASSIGN TO COURIN.

           SELECT COUROUT ASSIGN TO COUROUT.

           SELECT COURERR ASSIGN TO COURERR.
       DATA DIVISION.
       FILE SECTION.
       FD  COURIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS COURIN-RECORD.
       01  COURIN-RECORD.
           05  STUDENT-NAME            PIC X(20).
           05  STUDENT-COURSES.
               10 STUDENT-COURSE-TAB OCCURS 5 TIMES.
                   15  COURSE-NBR      PIC X(7).
                   15  COURSE-GRADE    PIC X(1).
           05  FILLER                  PIC X(20).

       FD  COUROUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS COUROUT-RECORD.
       01  COUROUT-RECORD PIC X(80).

       FD  COURERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 132 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS COURERR-RECORD.
       01  COURERR-RECORD.
           05 COURERR-DATA                 PIC X(80).
           05 COURERR-MESSAGE              PIC X(52).

       WORKING-STORAGE SECTION.
       01  SUBSCRIPTS-AND-COUNTERS.
           05  CTR-STUDENTS                 PIC 99    VALUE 0.
           05  STUDENT-SUB                  PIC 99    VALUE 0 COMP.
           05  COURSES-SUB                  PIC 99    VALUE 0 COMP.
           05  QPA-SUB                      PIC 99    VALUE 0 COMP.
           05  RECS-READ                    PIC 99    VALUE 0.
           05  REC-IN-SUB                   PIC 99    VALUE 0 COMP.
           05  GRADE-ACCUM                  PIC 99    VALUE 0 COMP.
           05  HI-QPA                       PIC 99V99 VALUE 0.
           05  LOW-QPA                      PIC 99V99 VALUE 0.
           05  AVERAGE-QPA                  PIC 99V99 VALUE 0.

       01  SEARCH-FIELDS.
           05  S-NAME                       PIC X(20) VALUE SPACES.
           05  S-COURSE1                    PIC X(7)  VALUE SPACES.
           05  S-COURSE2                    PIC X(7)  VALUE SPACES.


       01  WS-STUDENT-RECORD.
           05  WS-STUDENT-TABLE OCCURS 5 TIMES.
             10  WS-STUDENT-NAME            PIC X(20).
             10  WS-STUDENT-COURSES.
               15 WS-STUDENT-COURSE-TAB OCCURS 5 TIMES.
                   20  WS-COURSE-NBR        PIC X(7).
                   20  WS-COURSE-GRADE      PIC X(1).

       01  WS-STUDENT-RECORD2.
           05  WS-STUDENT-TABLE2 OCCURS 5 TIMES
                                 INDEXED BY S-REC-IDX.
             10  WS-STUDENT-NAME2            PIC X(20).
             10  WS-STUDENT-COURSES2.
               15 WS-STUDENT-COURSE-TAB2 OCCURS 5 TIMES
                                         INDEXED BY S-COR-IDX.
                   20  WS-COURSE-NBR2        PIC X(7).
                   20  WS-COURSE-GRADE2      PIC X(1).
                       88 VALID-GRADES   VALUE 'A' 'B' 'C' 'D' 'F'.


       01  WS-QPA-TABLE.
           05  WS-QPA-TAB OCCURS 5 TIMES.
               10 WS-QPA-NAME               PIC X(20).
               10 WS-QPA-AVG                PIC 9(2)V99.


       01 FLAGS-SWITCHES.
          05 EOF                            PIC X(01) VALUE SPACE.
             88 END-OF-FILE VALUE 'Y'.

          05 ERROR-FLAG                     PIC X(01) VALUE SPACE.
             88 ERROR-FOUND     VALUE 'Y'.
             88 ERROR-NOT-FOUND VALUE 'N'.

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR          PIC  9(4).
               10  WS-CURRENT-MONTH         PIC  9(2).
               10  WS-CURRENT-DAY           PIC  9(2).


      ***  REPORT SECTION
       01 BLANK-LINE                   PIC X(80) VALUE SPACES.
       01 REPORT-MAX-LINES             PIC 9(2)  VALUE 60.
       01 PAGE-NUM                     PIC 99    VALUE 1.

       01 HEADER-LINE1.
           05  FILLER                  PIC X(6)  VALUE 'DATE: '.
           05  HL1-DATE.
               10  HL1-MM              PIC 9(2).
               10  SLASH-1             PIC X(1)  VALUE "/".
               10  HL1-DD              PIC 9(2).
               10  SLASH-2             PIC X(1)  VALUE "/".
               10  HL1-YY              PIC 9(4).
           05  FILLER                  PIC X(14) VALUE SPACES.
           05  FILLER                  PIC X(24) VALUE
                   'Student Courses Breakout'.
           05  FILLER                  PIC X(18) VALUE SPACES.
           05  FILLER                  PIC X(6)  VALUE 'PAGE: '.
           05  HL1-PAGE-NUM            PIC Z9.

       01 DETAIL-LINE1.
          05 FILLER                    PIC X(10) VALUE SPACES.
          05 DL1-NAME                  PIC X(20).
          05 FILLER                    PIC X(50) VALUE SPACES.

       01 DETAIL-LINE2.
          05 FILLER                    PIC X(16) VALUE SPACES.
          05 FILLER                    PIC X(8)  VALUE 'Course: '.
          05 DL2-COURSE                PIC X(7).
          05 FILLER                    PIC X(5)  VALUE SPACES.
          05 FILLER                    PIC X(7)  VALUE 'Grade: '.
          05 DL2-GRADE                 PIC X.
          05 FILLER                    PIC X(11) VALUE SPACES.

       01 FOOTER-LINE1                 PIC X(80) VALUE ALL '-'.

       01 FOOTER-LINE2.
          05 FILLER                    PIC X(26) VALUE
              'Total number of students: '.
          05 FL2-NUM-OF-STU            PIC X(2).
          05 FILLER                    PIC X(52) VALUE SPACES.

       01 FOOTER-LINE3.
          05 FILLER                    PIC X(26) VALUE
               'Student with highest QPA: '.
          05 FL3-HI-NAME               PIC X(20).
          05 FILLER                    PIC X(34) VALUE SPACES.

       01 FOOTER-LINE4.
          05 FILLER                    PIC X(25) VALUE
               'Student with lowest QPA: '.
          05 FL4-LOW-NAME              PIC X(20).
          05 FILLER                    PIC X(35) VALUE SPACES.

       01 FOOTER-LINE5.
          05 FILLER                    PIC X(30) VALUE
               'Average QPA for all students: '.
          05 FL5-AVERAGE               PIC Z9.99.
          05 FILLER                    PIC X(45) VALUE SPACES.



       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-HOUSEKEEPING THRU 100-EXIT.

           PERFORM 500-PROCESS-RECORDS VARYING STUDENT-SUB
                FROM 1 BY 1 UNTIL END-OF-FILE OR STUDENT-SUB > 5.

           PERFORM 600-PRINT-STUDENT VARYING STUDENT-SUB
                FROM 1 BY 1 UNTIL END-OF-FILE OR STUDENT-SUB > 5.

           PERFORM 650-PRINT-SUMMARY-LINES.

           PERFORM 700-QUERY-TABLE.

           PERFORM 900-CLOSE-FILES.
           GOBACK.

       100-HOUSEKEEPING.
           INITIALIZE SUBSCRIPTS-AND-COUNTERS,
                      WS-STUDENT-RECORD,
                      WS-STUDENT-RECORD2,
                      FLAGS-SWITCHES,
                      WS-CURRENT-DATE-FIELDS,
                      WS-QPA-TABLE.
           PERFORM 200-OPEN-FILES THRU 200-EXIT.
           MOVE 1 TO PAGE-NUM.
           PERFORM 300-WRITE-REPORT-HEADERS THRU 300-EXIT.
           MOVE 'N' TO ERROR-FLAG.
           PERFORM 400-READ-COURIN THRU 400-EXIT.
       100-EXIT.
           EXIT.

       200-OPEN-FILES.
           OPEN INPUT COURIN.
           OPEN OUTPUT COUROUT, COURERR.
       200-EXIT.
           EXIT.

       300-WRITE-REPORT-HEADERS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR   TO HL1-YY.
           MOVE WS-CURRENT-MONTH  TO HL1-MM.
           MOVE WS-CURRENT-DAY    TO HL1-DD.

           MOVE PAGE-NUM          TO HL1-PAGE-NUM.

           MOVE HEADER-LINE1 TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.
           MOVE BLANK-LINE TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

           ADD 1                   TO PAGE-NUM.
       300-EXIT.
           EXIT.

       400-READ-COURIN.
           READ COURIN
             AT END MOVE 'Y' TO EOF
           END-READ.
           IF NOT END-OF-FILE
             ADD 1 TO RECS-READ
           END-IF.
       400-EXIT.
           EXIT.

       500-PROCESS-RECORDS.
      *** MOVE DATA FROM FILE INTO TABLE
           MOVE COURIN-RECORD TO WS-STUDENT-TABLE (STUDENT-SUB).
           ADD 1 TO CTR-STUDENTS.

      *** MOVE DATA ALSO INTO INDEXED TABLE
           SET S-REC-IDX TO STUDENT-SUB.
           MOVE COURIN-RECORD TO WS-STUDENT-TABLE2 (S-REC-IDX).

      *** CHECK FOR VALID GRADE
           PERFORM VARYING COURSES-SUB FROM 1 BY 1
               UNTIL COURSES-SUB > 5
             IF WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB) =
                ('A' OR 'B' OR 'C' OR 'D' OR 'F')
             THEN CONTINUE
             ELSE
               STRING WS-STUDENT-NAME (STUDENT-SUB) SPACE
                      WS-COURSE-NBR (STUDENT-SUB, COURSES-SUB) SPACE
                      WS-COURSE-GRADE (STUDENT-SUB, COURSES-SUB)
                DELIMITED BY SIZE
                INTO COURERR-DATA
                END-STRING
                MOVE '  INVALID GRADE' TO COURERR-MESSAGE
                WRITE COURERR-RECORD
             END-IF
            END-PERFORM.

      *** ASSIGN NUMBERIC VALUE TO GRADE
           MOVE 0 TO GRADE-ACCUM,
                     AVERAGE-QPA.

           PERFORM VARYING COURSES-SUB FROM 1 BY 1
               UNTIL COURSES-SUB > 5
             EVALUATE WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                 WHEN 'A' MOVE '4' TO
                   WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                   ADD 4           TO GRADE-ACCUM
                 WHEN 'B' MOVE '3' TO
                   WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                   ADD 3           TO GRADE-ACCUM
                 WHEN 'C' MOVE '2' TO
                   WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                   ADD 2           TO GRADE-ACCUM
                 WHEN 'D' MOVE '1' TO
                   WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                   ADD 1           TO GRADE-ACCUM
                 WHEN 'F' MOVE '0' TO
                   WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
             END-EVALUATE
           END-PERFORM.

      *** FIND AVERAGE QPA FOR ALL COURSES
           COMPUTE AVERAGE-QPA = GRADE-ACCUM / 5
           END-COMPUTE.

      *** SAVE STUDENT NAME AND AVERAGE QPA TO TABLE FOR REPORT
           MOVE STUDENT-SUB       TO QPA-SUB.
           MOVE WS-STUDENT-NAME (STUDENT-SUB) TO WS-QPA-NAME (QPA-SUB).
           MOVE AVERAGE-QPA                   TO WS-QPA-AVG (QPA-SUB).

           PERFORM 400-READ-COURIN THRU 400-EXIT.

      *** END OF 500-PROCESS-RECORDS


       600-PRINT-STUDENT.
           MOVE WS-STUDENT-NAME (STUDENT-SUB) TO DL1-NAME.
           MOVE DETAIL-LINE1 TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

      *** PRINT EACH COURSE FOR THE STUDENT
           PERFORM VARYING COURSES-SUB FROM 1 BY 1
               UNTIL COURSES-SUB > 5
             EVALUATE WS-COURSE-GRADE(STUDENT-SUB, COURSES-SUB)
                 WHEN '4' MOVE 'A' TO DL2-GRADE
                          PERFORM 610-PRINT-COURSE
                 WHEN '3' MOVE 'B' TO DL2-GRADE
                          PERFORM 610-PRINT-COURSE
                 WHEN '2' MOVE 'C' TO DL2-GRADE
                          PERFORM 610-PRINT-COURSE
                 WHEN '1' MOVE 'D' TO DL2-GRADE
                          PERFORM 610-PRINT-COURSE
                 WHEN '0' MOVE 'F' TO DL2-GRADE
                          PERFORM 610-PRINT-COURSE
             END-EVALUATE
           END-PERFORM.

      *** PRINT BLANK LINE AFTER COURSES
           MOVE BLANK-LINE   TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

       610-PRINT-COURSE.
           MOVE WS-COURSE-NBR (STUDENT-SUB, COURSES-SUB)
                                   TO DL2-COURSE.
           MOVE DETAIL-LINE2       TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

       650-PRINT-SUMMARY-LINES.
           MOVE FOOTER-LINE1       TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

           MOVE CTR-STUDENTS       TO FL2-NUM-OF-STU.
           MOVE FOOTER-LINE2       TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

           COMPUTE HI-QPA = FUNCTION MAX (WS-QPA-AVG (ALL)).
           COMPUTE LOW-QPA = FUNCTION MIN (WS-QPA-AVG (ALL)).
           COMPUTE AVERAGE-QPA = FUNCTION MEAN (WS-QPA-AVG (ALL)).

           PERFORM VARYING QPA-SUB FROM 1 BY 1
               UNTIL QPA-SUB > 5
               IF WS-QPA-AVG (QPA-SUB) = HI-QPA
                  MOVE WS-QPA-NAME (QPA-SUB) TO FL3-HI-NAME
                  MOVE FOOTER-LINE3          TO COUROUT-RECORD
                  WRITE COUROUT-RECORD
               END-IF
           END-PERFORM.

           PERFORM VARYING QPA-SUB FROM 1 BY 1
               UNTIL QPA-SUB > 5
               IF WS-QPA-AVG (QPA-SUB) = LOW-QPA
                  MOVE WS-QPA-NAME (QPA-SUB) TO FL4-LOW-NAME
                  MOVE FOOTER-LINE4          TO COUROUT-RECORD
                  WRITE COUROUT-RECORD
               END-IF
           END-PERFORM.

           MOVE AVERAGE-QPA        TO FL5-AVERAGE.
           MOVE FOOTER-LINE5       TO COUROUT-RECORD.
           WRITE COUROUT-RECORD.

       700-QUERY-TABLE.
      *** Find anyone who's studied TRIG551 or DRUM310
           MOVE 'TRIG551'          TO S-COURSE1.
           MOVE 'DRUM310'          TO S-COURSE2.

           PERFORM VARYING S-REC-IDX FROM 1 BY 1
                   UNTIL S-REC-IDX > 5
              PERFORM VARYING S-COR-IDX FROM 1 BY 1
                      UNTIL S-COR-IDX > 5
                 SEARCH WS-STUDENT-COURSE-TAB2
                 AT END
                     CONTINUE
              WHEN WS-COURSE-NBR2 (S-REC-IDX S-COR-IDX) = S-COURSE1 OR
                   WS-COURSE-NBR2 (S-REC-IDX S-COR-IDX) = S-COURSE2
                   DISPLAY 'STUDENT: ' WS-STUDENT-NAME2 (S-REC-IDX)
                       'HAS TAKEN: ' S-COURSE1 ' ' S-COURSE2
                  END-SEARCH
               END-PERFORM
           END-PERFORM.

      *** Is SALLY HARRIS taking ear-training (EART164)?
           MOVE 'SALLY HARRIS'     TO S-NAME.
           MOVE 'EART164'          TO S-COURSE1.

           PERFORM VARYING S-REC-IDX FROM 1 BY 1
                   UNTIL S-REC-IDX > 5
              PERFORM VARYING S-COR-IDX FROM 1 BY 1
                      UNTIL S-COR-IDX > 5
                 SEARCH WS-STUDENT-COURSE-TAB2
                 AT END
                     CONTINUE
              WHEN WS-STUDENT-NAME2 (S-REC-IDX) = S-NAME   AND
                   WS-COURSE-NBR2 (S-REC-IDX S-COR-IDX) = S-COURSE1
                   DISPLAY 'STUDENT: ' WS-STUDENT-NAME2 (S-REC-IDX)
                       'HAS TAKEN: ' S-COURSE1
                  END-SEARCH
               END-PERFORM
           END-PERFORM.

      *** What did LISA CRUDUP get in PSYCH23A?
           MOVE 'LISA CRUDUP'       TO S-NAME.
           MOVE 'PSYC23A'           TO S-COURSE1.

           PERFORM VARYING S-REC-IDX FROM 1 BY 1
                   UNTIL S-REC-IDX > 5
              PERFORM VARYING S-COR-IDX FROM 1 BY 1
                      UNTIL S-COR-IDX > 5
                 SEARCH WS-STUDENT-COURSE-TAB2
                 AT END
                     CONTINUE
              WHEN WS-STUDENT-NAME2 (S-REC-IDX) = S-NAME   AND
                   WS-COURSE-NBR2 (S-REC-IDX S-COR-IDX) = S-COURSE1
                   DISPLAY 'STUDENT: ' WS-STUDENT-NAME2 (S-REC-IDX)
                       'HAS TAKEN: ' S-COURSE1 ' THE GRADE IS: '
                       WS-COURSE-GRADE2 (S-REC-IDX S-COR-IDX)
                  END-SEARCH
               END-PERFORM
           END-PERFORM.

      *** Are there any records with invalid grades
           PERFORM VARYING S-REC-IDX FROM 1 BY 1
                   UNTIL S-REC-IDX > 5
              PERFORM VARYING S-COR-IDX FROM 1 BY 1
                      UNTIL S-COR-IDX > 5
                 SEARCH WS-STUDENT-COURSE-TAB2
                 AT END
                     CONTINUE
              WHEN
                 NOT  VALID-GRADES (S-REC-IDX S-COR-IDX)
                 DISPLAY 'STUDENT: ' WS-STUDENT-NAME2 (S-REC-IDX)
                    'IN COURSE: ' WS-COURSE-NBR2 (S-REC-IDX S-COR-IDX)
                    ' HAD AN INVALID GRADE OF: '
                     WS-COURSE-GRADE2 (S-REC-IDX S-COR-IDX)
                 END-SEARCH
              END-PERFORM
           END-PERFORM.


       900-CLOSE-FILES.
           CLOSE COURIN, COUROUT, COURERR.