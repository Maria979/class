       IDENTIFICATION DIVISION.
       PROGRAM-ID. FAVRFP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT RFPIN  ASSIGN TO RFPIN.

           SELECT RFPOUT ASSIGN TO RFPOUT.

           SELECT RFPERR ASSIGN TO RFPERR.
       DATA DIVISION.
       FILE SECTION.
       FD  RFPIN
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFPIN-RECORD.
       01  RFPIN-RECORD PIC X(80).

       FD  RFPOUT
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFPOUT-RECORD.
       01  RFPOUT-RECORD PIC X(80).

       FD  RFPERR
           RECORDING MODE IS F
           LABEL RECORDS ARE STANDARD
           RECORD CONTAINS 80 CHARACTERS
           BLOCK CONTAINS 0 RECORDS
           DATA RECORD IS RFPERR-RECORD.
       01  RFPERR-RECORD PIC X(80).

       WORKING-STORAGE SECTION.
       01  RFPIN-REC.
           05 ARTIST-ACCOUNT-NO.
              10 ARTIST-ACCT-X                   PIC X(07).
              10 ARTIST-ACCT-NUM                 PIC 9(01).
           05 ARTIST-MUSICAL-GENRE               PIC X(09).
              88 ROCK                  VALUE 'ROCK'.
              88 JAZZ                  VALUE 'JAZZ'.
              88 FUSION                VALUE 'FUSION'.
              88 FOLK                  VALUE 'FOLK'.
              88 CLASSICAL             VALUE 'CLASSICAL'.
              88 COUNTRY               VALUE 'COUNTRY'.
           05 MUSICIAN.
              10 MUSICIAN-LNAME                  PIC X(15).
              10 MUSICIAN-FNAME                  PIC X(15).
           05 MUSICIAN-INSTRUMENT-TYPE           PIC X(06).
              88 KEYBOARD              VALUE 'KEYS'.
              88 VOCALS                VALUE 'VOCALS'.
              88 GUITAR                VALUE 'GUITAR'.
              88 BASS                  VALUE 'BASS'.
              88 DRUMS                 VALUE 'DRUMS'.
              88 PERCUSSION            VALUE 'PERC'.
           05 INSTRUMENT-QUALITY                 PIC X(01).
              88 USED                  VALUE 'U'.
              88 NEW                   VALUE 'N'.
              88 PREMIUM               VALUE 'P'.
           05 MAX-MUSICIAN-BUDGET-AMOUNT         PIC 9(4)V99.
           05 SHIP-TO                            PIC X(03).
              88 IN-COUNTRY            VALUE 'IN'.
              88 OUT-OF-COUNTRY        VALUE 'OUT'.
           05 FILLER                             PIC X(17).

       01  RFPOUT-REC.
           05 MUSICIAN-O.
              10 MUSICIAN-FNAME-O                PIC X(12).
              10 MUSICIAN-LNAME-O                PIC X(15).
           05 ARTIST-MUSICAL-GENRE-O             PIC X(06).
           05 FILLER                             PIC X(06) VALUE SPACES.
           05 MUSICIAN-INSTRUMENT-TYPE-O         PIC X(06).
           05 FILLER                             PIC X(07) VALUE SPACES.
           05 INSTRUMENT-QUALITY-O               PIC X(07).
           05 FILLER                             PIC X(06) VALUE SPACES.
           05 TOTAL-COST-O                       PIC $,$$$,$99.99.

       01  COST.
           05 COST-PER-INSTRUMENT                PIC S9(7)V99.
           05 TOTAL-COST                         PIC S9(7)V99.
           05 GRAND-TOTAL                        PIC S9(10)V99.
           05 TOTAL-TAX                          PIC S9(3)V99.
           05 ADDITIONAL-COSTS.
              10 SHIPPING-COST                   PIC S9(4)V99.
              10 TAX                             PIC S9(3)V99.
              10 UPLIFT                          PIC S9(7)V99.

       01 GRAND-TOTALS.
           05 RECS-READ               PIC 9(4).
           05 NUM-GOOD                PIC 9(4).
           05 NUM-BAD                 PIC 9(4).
           05 OVERALL-GRAND-TOTAL     PIC S9(10)V99.

       01 GRAND-TOTALS-O.
           05 RECS-READ-O             PIC Z,ZZ9.
           05 NUM-GOOD-O              PIC Z,ZZ9.
           05 NUM-BAD-O               PIC Z,ZZ9.
           05 GRAND-TOTAL-O           PIC $,$$$,$$$,$99.99.

       01 EOF                         PIC X(01) VALUE SPACE.
           88 END-OF-FILE VALUE 'Y'.

       01 ERROR-FLAG                  PIC X(01) VALUE SPACE.
           88 ERROR-FOUND     VALUE 'Y'.
           88 ERROR-NOT-FOUND VALUE 'N'.

       01  WS-CURRENT-DATE-FIELDS.
           05  WS-CURRENT-DATE.
               10  WS-CURRENT-YEAR    PIC  9(4).
               10  WS-CURRENT-MONTH   PIC  9(2).
               10  WS-CURRENT-DAY     PIC  9(2).

       01 REPORT-LINE1.
           05 STORE          PIC X(19)  VALUE "MARIA'S MUSIC STORE".
           05 FILLER         PIC X(45)  VALUE SPACES.
           05 FILLER         PIC X(6)   VALUE 'DATE: '.
           05  RL1-DATE.
               10  RL1-MM              PIC 9(2).
               10  SLASH-1             PIC X(1) VALUE "/".
               10  RL1-DD              PIC 9(2).
               10  SLASH-2             PIC X(1) VALUE "/".
               10  RL1-YY              PIC 9(4).

       01 REPORT-LINE2                PIC X(80) VALUE SPACES.

       01 REPORT-LINE3.
          05 H1-MUSICIAN               PIC X(26) VALUE 'MUSICIAN'.
          05 FILLER                    PIC X(1)  VALUE SPACE.
          05 H1-GENRE                  PIC X(6)  VALUE 'GENRE'.
          05 FILLER                    PIC X(5)  VALUE SPACE.
          05 H1-INSTRUMENT             PIC X(10) VALUE 'INSTRUMENT'.
          05 FILLER                    PIC X(4)  VALUE SPACE.
          05 H1-QUALITY                PIC X(7)  VALUE 'QUALITY'.
          05 FILLER                    PIC X(7)  VALUE SPACE.
          05 H1-TOTAL-COST             PIC X(10) VALUE 'TOTAL COST'.

       01 REPORT-LINE4                 PIC X(80) VALUE ALL '*'.

       01 REPORT-LINE5.
          05 FILLER        PIC X(24) VALUE 'NUMBER OF RECORDS READ: '.
          05 RL5-RECS-READ PIC Z,ZZ9.
          05 FILLER        PIC X(50) VALUE SPACES.

       01 REPORT-LINE6.
          05 FILLER        PIC X(24) VALUE 'GOOD RECORDS READ: '.
          05 RL6-NUM-GOOD  PIC Z,ZZ9.
          05 FILLER        PIC X(50) VALUE SPACES.

       01 REPORT-LINE7.
          05 FILLER        PIC X(24) VALUE 'BAD RECORDS READ: '.
          05 RL7-NUM-BAD   PIC Z,ZZ9.
          05 FILLER        PIC X(50) VALUE SPACES.

       01 REPORT-LINE8.
          05 FILLER        PIC X(24) VALUE 'INSTRUMENT GRAND TOTAL: '.
          05 RL8-GRAND-TOTAL PIC $,$$$,$$$,$99.99.
          05 FILLER        PIC X(48) VALUE SPACES.

       PROCEDURE DIVISION.
       000-MAIN.
           PERFORM 100-HOUSEKEEPING.
           PERFORM 500-PROCESS-FILE UNTIL END-OF-FILE.
           PERFORM 800-WRITE-FINAL-LINES.
           PERFORM 900-CLOSE-FILES.
           GOBACK.

       100-HOUSEKEEPING.
           INITIALIZE RFPIN-REC, RFPOUT-REC.
           INITIALIZE COST, GRAND-TOTALS, GRAND-TOTALS-O.
           PERFORM 200-OPEN-FILES.
           PERFORM 300-WRITE-REPORT-HEADERS.
           MOVE 'N' TO ERROR-FLAG.
           PERFORM 400-READ-RFPIN.

       200-OPEN-FILES.
           OPEN INPUT RFPIN.
           OPEN OUTPUT RFPOUT, RFPERR.

       300-WRITE-REPORT-HEADERS.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE-FIELDS.
           MOVE WS-CURRENT-YEAR   TO RL1-YY.
           MOVE WS-CURRENT-MONTH  TO RL1-MM.
           MOVE WS-CURRENT-DAY    TO RL1-DD.

           MOVE REPORT-LINE1 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.
           MOVE REPORT-LINE2 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.
           MOVE REPORT-LINE3 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.
           MOVE REPORT-LINE4 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.

       400-READ-RFPIN.
           READ RFPIN INTO RFPIN-REC
             AT END MOVE 'Y' TO EOF
           END-READ.
           IF NOT END-OF-FILE
             ADD 1 TO RECS-READ
           END-IF.

       500-PROCESS-FILE.
           PERFORM 600-ERROR-CHECKING.
           IF ERROR-NOT-FOUND
             PERFORM 700-PROCESS-RECORD
           END-IF.
           MOVE 'N' TO ERROR-FLAG.
           PERFORM 400-READ-RFPIN.

       600-ERROR-CHECKING.
           IF ARTIST-ACCT-NUM IS NUMERIC
              CONTINUE
           ELSE
              MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           END-IF.

           IF ARTIST-MUSICAL-GENRE = 'ROCK' OR 'JAZZ' OR 'FUSION' OR 'FO
      -    'LK' OR 'CLASSICAL' OR 'COUNTRY'
               CONTINUE
            ELSE
               MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           END-IF.

           IF MUSICIAN-LNAME = SPACES OR MUSICIAN-FNAME = SPACES
              MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           ELSE
              CONTINUE
           END-IF.

           IF MUSICIAN-INSTRUMENT-TYPE = 'KEYS' OR 'VOCALS' OR 'GUITAR'
      -     OR 'BASS' OR 'DRUMS' OR 'PERC'
              CONTINUE
           ELSE
              MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           END-IF.

           IF INSTRUMENT-QUALITY = 'U' OR 'N' OR 'P'
              CONTINUE
           ELSE
              MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           END-IF.

           IF MAX-MUSICIAN-BUDGET-AMOUNT >= 1000 AND <= 9999.99
             CONTINUE
           ELSE
              MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           END-IF.

           IF SHIP-TO = 'IN' OR 'OUT'
             CONTINUE
           ELSE
              MOVE 'Y' TO ERROR-FLAG
              PERFORM 650-WRITE-ERROR-RECORD
           END-IF.

       650-WRITE-ERROR-RECORD.
           MOVE RFPIN-REC TO RFPERR-RECORD.
           WRITE RFPERR-RECORD.
           ADD 1 TO NUM-BAD.

       700-PROCESS-RECORD.
           EVALUATE TRUE
             WHEN KEYBOARD
               MOVE 3017.89  TO COST-PER-INSTRUMENT
             WHEN VOCALS
               MOVE 599.05   TO COST-PER-INSTRUMENT
             WHEN GUITAR
               MOVE 1000.00  TO COST-PER-INSTRUMENT
             WHEN BASS
               MOVE 18761.00 TO COST-PER-INSTRUMENT
             WHEN DRUMS
               MOVE 3087.22  TO COST-PER-INSTRUMENT
             WHEN PERCUSSION
               MOVE 799.99   TO COST-PER-INSTRUMENT
           END-EVALUATE.

           EVALUATE SHIP-TO
              WHEN 'IN'
                COMPUTE SHIPPING-COST = COST-PER-INSTRUMENT * .10
              WHEN 'OUT'
                COMPUTE SHIPPING-COST = COST-PER-INSTRUMENT * .20
            END-EVALUATE.

           EVALUATE INSTRUMENT-QUALITY
             WHEN 'U'
               COMPUTE UPLIFT = ((COST-PER-INSTRUMENT * .20) * -1)
               MOVE 'USED' TO INSTRUMENT-QUALITY-O
             WHEN 'N'
               MOVE ZEROES TO UPLIFT
               MOVE 'NEW'  TO INSTRUMENT-QUALITY-O
             WHEN 'P'
               COMPUTE UPLIFT = (COST-PER-INSTRUMENT * .20)
               MOVE 'PREMIUM' TO INSTRUMENT-QUALITY-O
            END-EVALUATE.

            MOVE .08 TO TAX.

            COMPUTE TOTAL-TAX =
      -        ((COST-PER-INSTRUMENT + UPLIFT) * TAX)

            COMPUTE TOTAL-COST =
      -       COST-PER-INSTRUMENT + UPLIFT + TOTAL-TAX + SHIPPING-COST.

           PERFORM 750-WRITE-OUTPUT.
           ADD 1 TO NUM-GOOD.
           ADD TOTAL-COST TO OVERALL-GRAND-TOTAL.

       750-WRITE-OUTPUT.
           MOVE ARTIST-MUSICAL-GENRE     TO ARTIST-MUSICAL-GENRE-O.
           MOVE MUSICIAN-LNAME           TO MUSICIAN-LNAME-O.
           MOVE MUSICIAN-FNAME           TO MUSICIAN-FNAME-O.
           MOVE MUSICIAN-INSTRUMENT-TYPE TO MUSICIAN-INSTRUMENT-TYPE-O.
           MOVE TOTAL-COST               TO TOTAL-COST-O.
           WRITE RFPOUT-RECORD FROM RFPOUT-REC.

       800-WRITE-FINAL-LINES.
           MOVE REPORT-LINE2 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.

           MOVE RECS-READ         TO RL5-RECS-READ.
           MOVE REPORT-LINE5 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.

           MOVE NUM-GOOD TO RL6-NUM-GOOD.
           MOVE REPORT-LINE6 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.

           MOVE NUM-BAD TO RL7-NUM-BAD.
           MOVE REPORT-LINE7 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.

           MOVE OVERALL-GRAND-TOTAL TO RL8-GRAND-TOTAL.
           MOVE REPORT-LINE8 TO RFPOUT-RECORD.
           WRITE RFPOUT-RECORD.

       900-CLOSE-FILES.
           CLOSE RFPIN, RFPOUT, RFPERR.