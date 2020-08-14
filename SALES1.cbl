       IDENTIFICATION DIVISION.
       PROGRAM-ID. SALES1.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE  ASSIGN TO SALES.
           SELECT OUTPUT-FILE ASSIGN TO SALRPT.
       DATA DIVISION.
       FILE SECTION.
       FD  INPUT-FILE RECORDING MODE F.
       01  SALE-TABLE-I.
           05 REGION-I.
              10 SALES-REGION-I   PIC X(02).
              10 Q1-SALES-I       PIC 9(5)V99 VALUE ZEROES.
              10 Q2-SALES-I       PIC 9(5)V99 VALUE ZEROES.
              10 Q3-SALES-I       PIC 9(5)V99 VALUE ZEROES.
              10 Q4-SALES-I       PIC 9(5)V99 VALUE ZEROES.
              10 FILLER           PIC X(50)   VALUE SPACES.

       FD  OUTPUT-FILE
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RPTOUT-RECORD.
       01  RPTOUT-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  TABLE-MAX              PIC S9(4) COMP VALUE 20.
       01  SW-END-OF-FILE         PIC X(01) VALUE SPACES.
                88 END-OF-FILE    VALUE 'Y'.

       01  SALE-TABLE.
           05 REGION OCCURS 8 TIMES INDEXED BY SALE-IDX.
              10 SALES-REGION     PIC X(02).
              10 Q1-SALES         PIC 9(5)V99 VALUE ZEROES.
              10 Q2-SALES         PIC 9(5)V99 VALUE ZEROES.
              10 Q3-SALES         PIC 9(5)V99 VALUE ZEROES.
              10 Q4-SALES         PIC 9(5)V99 VALUE ZEROES.

       01  SALES-OUT              PIC 9(15)V99 VALUE 0.
       01  SALES-OUT-DISPLAY      PIC $$$,$$$,$$$,$$$,$99.99.

       01  REGION-SALES-ACCUMULATORS.
           05  NE-Q1-SALES        PIC 9(15)V99 VALUE 0.
           05  NE-Q2-SALES        PIC 9(15)V99 VALUE 0.
           05  NE-Q3-SALES        PIC 9(15)V99 VALUE 0.
           05  NE-Q4-SALES        PIC 9(15)V99 VALUE 0.
           05  SE-Q1-SALES        PIC 9(15)V99 VALUE 0.
           05  SE-Q2-SALES        PIC 9(15)V99 VALUE 0.
           05  SE-Q3-SALES        PIC 9(15)V99 VALUE 0.
           05  SE-Q4-SALES        PIC 9(15)V99 VALUE 0.
           05  NW-Q1-SALES        PIC 9(15)V99 VALUE 0.
           05  NW-Q2-SALES        PIC 9(15)V99 VALUE 0.
           05  NW-Q3-SALES        PIC 9(15)V99 VALUE 0.
           05  NW-Q4-SALES        PIC 9(15)V99 VALUE 0.
           05  SW-Q1-SALES        PIC 9(15)V99 VALUE 0.
           05  SW-Q2-SALES        PIC 9(15)V99 VALUE 0.
           05  SW-Q3-SALES        PIC 9(15)V99 VALUE 0.
           05  SW-Q4-SALES        PIC 9(15)V99 VALUE 0.

       01 HEADER-LINE1.
           05  FILLER             PIC X(34) VALUE SPACES.
           05  FILLER             PIC X(12) VALUE 'SALES REPORT'.
           05  FILLER             PIC X(34) VALUE SPACES.

       01  BLANK-LINE             PIC X(80) VALUE SPACES.

       01  REPORT-LINE2.
           05  FILLER             PIC X(11) VALUE 'TOTALS FOR '.
           05  RL2-REGION         PIC X(02).
           05  FILLER             PIC X(07) VALUE ' REGION'.
           05  FILLER             PIC X(60) VALUE SPACES.

       01  REPORT-LINE3.
           05  RL3-QUARTER        PIC X(02).
           05  FILLER             PIC X(09) VALUE ' TOTALS: '.
           05  RL3-SALES          PIC $$$,$$$,$$$,$$$,$99.99.
           05  FILLER             PIC X(47) VALUE SPACES.

       01  REPORT-LINE4.
           05  RL4-TEXT           PIC X(40).
           05  RL4-SALES-OUT      PIC $$$,$$$,$$$,$$$,$99.99.
           05  FILLER             PIC X(18) VALUE SPACES.


       PROCEDURE DIVISION.
           PERFORM 000-HOUSEKEEPING.
           PERFORM 100-PROCESS-TABLE-DATA.
           PERFORM 150-PRINT-REGION-REPORT.
           PERFORM 900-WRAP-UP
           GOBACK.

       000-HOUSEKEEPING.
      ***
      ***  OPEN FILE AND READ DATA INTO TABLE
      ***
           INITIALIZE SALE-TABLE.
           OPEN INPUT INPUT-FILE.
           OPEN OUTPUT OUTPUT-FILE.
           READ INPUT-FILE
           AT END MOVE 'Y' TO SW-END-OF-FILE
           END-READ.

           PERFORM VARYING SALE-IDX FROM 1 BY 1
              UNTIL SALE-IDX = TABLE-MAX
           OR END-OF-FILE
                MOVE SALES-REGION-I TO SALES-REGION (SALE-IDX)
                MOVE Q1-SALES-I     TO Q1-SALES (SALE-IDX)
                MOVE Q2-SALES-I     TO Q2-SALES (SALE-IDX)
                MOVE Q3-SALES-I     TO Q3-SALES (SALE-IDX)
                MOVE Q4-SALES-I     TO Q4-SALES (SALE-IDX)
                READ INPUT-FILE
                    AT END MOVE 'Y' TO  SW-END-OF-FILE
                END-READ
           END-PERFORM.

           MOVE HEADER-LINE1  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.


       100-PROCESS-TABLE-DATA.
      ***
      ***  REPORT ALL Q1 SALES FOR ALL REGIONS
      ***
           COMPUTE SALES-OUT = FUNCTION SUM (Q1-SALES(ALL)).
           MOVE 'ALL Q1 SALES FOR ALL REGIONS: ' TO RL4-TEXT.
           MOVE SALES-OUT                        TO RL4-SALES-OUT.
           MOVE REPORT-LINE4  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.


      ***
      ***  REPORT HIGHEST Q1 SALES FOR ALL REGIONS
      ***
           MOVE ZEROES TO SALES-OUT.
           COMPUTE SALES-OUT = FUNCTION MAX (Q1-SALES(ALL)).
           MOVE 'HIGHEST Q1 SALES FOR ALL REGIONS: ' TO RL4-TEXT.
           MOVE SALES-OUT                        TO RL4-SALES-OUT.
           MOVE REPORT-LINE4  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

      ***
      ***  REPORT LOWEST Q1 SALES FOR ALL REGIONS
      ***
           MOVE ZEROES TO SALES-OUT.
           COMPUTE SALES-OUT = FUNCTION MIN (Q1-SALES(ALL)).
           MOVE 'LOWEST Q1 SALES FOR ALL REGIONS: ' TO RL4-TEXT.
           MOVE SALES-OUT                        TO RL4-SALES-OUT.
           MOVE REPORT-LINE4  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

      ***
      ***  REPORT ALL SALES FOR ALL REGIONS
      ***
           MOVE ZEROES TO SALES-OUT.
           COMPUTE SALES-OUT = (FUNCTION SUM (Q1-SALES(ALL))) +
                               (FUNCTION SUM (Q2-SALES(ALL))) +
                               (FUNCTION SUM (Q3-SALES(ALL))) +
                               (FUNCTION SUM (Q4-SALES(ALL)))
           END-COMPUTE.

           MOVE 'TOTAL SALES FOR ALL REGIONS: ' TO RL4-TEXT.
           MOVE SALES-OUT                        TO RL4-SALES-OUT.
           MOVE REPORT-LINE4  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.


      ***
      ***  ACCUMULATE SALES BY QUARTER FOR EACH REGION
      ***  REGIONS ARE NE, SE, NW, AND SW
      ***
            PERFORM VARYING SALE-IDX FROM 1 BY 1
              UNTIL SALE-IDX = TABLE-MAX
              IF SALES-REGION (SALE-IDX) = 'NE'
                 ADD Q1-SALES (SALE-IDX)  TO NE-Q1-SALES
                 ADD Q2-SALES (SALE-IDX)  TO NE-Q2-SALES
                 ADD Q3-SALES (SALE-IDX)  TO NE-Q3-SALES
                 ADD Q4-SALES (SALE-IDX)  TO NE-Q4-SALES
              ELSE
                 IF SALES-REGION (SALE-IDX) = 'SE'
                    ADD Q1-SALES (SALE-IDX)  TO SE-Q1-SALES
                    ADD Q2-SALES (SALE-IDX)  TO SE-Q2-SALES
                    ADD Q3-SALES (SALE-IDX)  TO SE-Q3-SALES
                    ADD Q4-SALES (SALE-IDX)  TO SE-Q4-SALES
                 ELSE
                    IF SALES-REGION (SALE-IDX) = 'NW'
                      ADD Q1-SALES (SALE-IDX)  TO NW-Q1-SALES
                      ADD Q2-SALES (SALE-IDX)  TO NW-Q2-SALES
                      ADD Q3-SALES (SALE-IDX)  TO NW-Q3-SALES
                      ADD Q4-SALES (SALE-IDX)  TO NW-Q4-SALES
                    ELSE
                       IF SALES-REGION (SALE-IDX) = 'SW'
                         ADD Q1-SALES (SALE-IDX)  TO SW-Q1-SALES
                         ADD Q2-SALES (SALE-IDX)  TO SW-Q2-SALES
                         ADD Q3-SALES (SALE-IDX)  TO SW-Q3-SALES
                         ADD Q4-SALES (SALE-IDX)  TO SW-Q4-SALES
                       END-IF
                    END-IF
                 END-IF
              END-IF
           END-PERFORM.

       150-PRINT-REGION-REPORT.
      ***
      ***  REPORT SALES BY QUARTER FOR EACH REGION
      ***  REGIONS ARE NE, SE, NW, AND SW
      ***
      *** NE REGION
           MOVE 'NE'          TO RL2-REGION.
           MOVE REPORT-LINE2  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q1' TO RL3-QUARTER.
           MOVE NE-Q1-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q2' TO RL3-QUARTER.
           MOVE NE-Q2-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q3' TO RL3-QUARTER.
           MOVE NE-Q3-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q4' TO RL3-QUARTER.
           MOVE NE-Q4-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

      *** SE REGION
           MOVE 'SE'          TO RL2-REGION.
           MOVE REPORT-LINE2  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q1' TO RL3-QUARTER.
           MOVE SE-Q1-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q2' TO RL3-QUARTER.
           MOVE SE-Q2-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q3' TO RL3-QUARTER.
           MOVE SE-Q3-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q4' TO RL3-QUARTER.
           MOVE NE-Q4-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

      *** NW REGION
           MOVE 'NW'          TO RL2-REGION.
           MOVE REPORT-LINE2  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q1' TO RL3-QUARTER.
           MOVE NW-Q1-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q2' TO RL3-QUARTER.
           MOVE NW-Q2-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q3' TO RL3-QUARTER.
           MOVE NW-Q3-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q4' TO RL3-QUARTER.
           MOVE NW-Q4-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE BLANK-LINE  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

      *** SW REGION
           MOVE 'SW'          TO RL2-REGION.
           MOVE REPORT-LINE2  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q1' TO RL3-QUARTER.
           MOVE SW-Q1-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q2' TO RL3-QUARTER.
           MOVE SW-Q2-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q3' TO RL3-QUARTER.
           MOVE SW-Q3-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.

           MOVE 'Q4' TO RL3-QUARTER.
           MOVE SW-Q4-SALES TO RL3-SALES.
           MOVE REPORT-LINE3  TO RPTOUT-RECORD.
           WRITE RPTOUT-RECORD.



       900-WRAP-UP.
           CLOSE INPUT-FILE, OUTPUT-FILE.