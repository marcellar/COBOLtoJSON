       IDENTIFICATION DIVISION.
       PROGRAM-ID. INDEXING.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT OPTIONAL TTPLAYER
          ASSIGN TO "PLFIL.DAT"
          ORGANIZATION IS INDEXED
          ACCESS MODE IS DYNAMIC
          RECORD KEY IS PL-ID OF PL-RECORD
          ALTERNATE RECORD KEY IS PL-KEY1 OF PL-RECORD
              WITH DUPLICATES
          ALTERNATE RECORD KEY IS PL-FNAME OF PL-RECORD
              WITH DUPLICATES              
          .
          SELECT PRINT-FILE ASSIGN TO "OUTPUT.JSON".

       DATA DIVISION.
       FILE SECTION.
       COPY "TTPLAYER.FD". 
       FD  PRINT-FILE.
        01  PRINT-LINE                         PIC X(1000).

      *****************************************************************
       WORKING-STORAGE                 SECTION.   
        01 CONTROL-FLAG         PIC X.
          88 NO-MORE-DUPLICATES          VALUE 1
             WHEN SET TO FALSE IS              0.
          88 NO-MORE-RECORDS             VALUE 1
             WHEN SET TO FALSE IS              0.
        01 WS-INPUT-NAME                 PIC X(15).
        01 FILLER REDEFINES WS-INPUT-NAME.
           05 WS-FIRST-INITIAL           PIC X(01).
           05 WS-REST-NAME               PIC X(14).
        01 WS-INPUT-NAME-UPPER           PIC X(15).
        01 WS-STRING                     PIC X(1000).
        01 WS-VALUE-PAIR.
           05 WS-ITEM-NAME               PIC X(50).
           05 WS-VALUE                   PIC X(50).
           05 WS-VALUE-NUMERIC           PIC 9(04).
        78 C-UPPER      VALUE "ABCDEFGHIJKLMNOPQRSTUVWXYZ".
        78 C-LOWER      VALUE "abcdefghijklmnopqrstuvwxyz".
        78 C-OPEN-CURLY-BRACKET       VALUE "{".
        78 C-CLOSE-CURLY-BRACKET      VALUE "}".
        78 C-OPEN-SQUARE-BRACKET      VALUE "[".
        78 C-CLOSE-SQUARE-BRACKET     VALUE "]".
        78 C-COMMA                    VALUE ",".
        78 C-COLON                    VALUE ':'.
        78 C-QUOTE                    VALUE '"'.
        78 C-DOUBLE-SPACE             VALUE "  ".
        78 C-BACKSLASH                VALUE "/".
        78 C-SPACE                    VALUE " ".
        78 C-RESULTS                  VALUE "results".
        78 C-ID                       VALUE "ID".
        78 C-FIRST-NAME               VALUE "firstName".
        78 C-LAST-NAME                VALUE "lastName".
        78 C-ADDRESS-LINE1            VALUE "streetAddress".
        78 C-CITY                     VALUE "city".
        78 C-STATE                    VALUE "state".
        78 C-POSTCODE                 VALUE "postcode".
        78 C-EMPLOYEE                 VALUE "employee".
        78 C-VIP                      VALUE "VIP".
        78 C-BIRTHDAY                 VALUE "birthday".
        78 C-YES                      VALUE "Yes".
        78 C-NO                       VALUE "No".
        01 FILLER                     PIC X(01).
          88 FL-NUMERIC-NO            VALUE "N".
          88 FL-NUMERIC-YES           VALUE "Y".
        01 FILLER                     PIC X(01).
          88 FL-FIRST-FIELD-NO         VALUE "N".
          88 FL-FIRST-FIELD-YES        VALUE "Y".
        01 FILLER                     PIC X(01).
          88 FL-FIRST-RECORD-NO       VALUE "N".
          88 FL-FIRST-RECORD-YES      VALUE "Y".
      *****************************************************************
       PROCEDURE DIVISION.

         OPEN I-O TTPLAYER
         OPEN OUTPUT PRINT-FILE
         SET FL-FIRST-RECORD-YES  TO TRUE
         INITIALIZE WS-STRING
         DISPLAY 'Enter a name:'
         ACCEPT WS-INPUT-NAME


         IF WS-INPUT-NAME NOT = SPACES
             MOVE FUNCTION UPPER-CASE(WS-FIRST-INITIAL) 
                                  TO WS-FIRST-INITIAL
             MOVE FUNCTION LOWER-CASE(WS-REST-NAME)
                                  TO WS-REST-NAME
             DISPLAY 'You entered: 'WS-INPUT-NAME
             PERFORM FIND-LAST-NAMES
             PERFORM FIND-FIRST-NAMES
         END-IF
         STRING
             WS-STRING              DELIMITED BY C-DOUBLE-SPACE
             C-CLOSE-SQUARE-BRACKET DELIMITED BY SIZE
             C-CLOSE-CURLY-BRACKET  DELIMITED BY SIZE
                                    INTO WS-STRING
         END-STRING
         MOVE WS-STRING TO PRINT-LINE
         WRITE PRINT-LINE
         CLOSE TTPLAYER
         CLOSE PRINT-FILE
         STOP RUN
         .


     *****************************************************************
       FIND-LAST-NAMES                 SECTION.

           INITIALIZE PL-RECORD
           MOVE LOW-VALUES TO PL-KEY1
           MOVE WS-INPUT-NAME TO PL-LNAME
           PERFORM TTPLAYER1-START
           PERFORM 
              UNTIL NO-MORE-RECORDS
               IF PL-LNAME = WS-INPUT-NAME
                   PERFORM TTPLAYER-DISPLAY-RECORD
                   PERFORM WRITE-JSON-RECORD
                   PERFORM TTPLAYER1-READ-NEXT
               ELSE
                   SET NO-MORE-RECORDS TO TRUE
               END-IF
           END-PERFORM
           .


      *****************************************************************
       FIND-FIRST-NAMES                SECTION.

           INITIALIZE PL-RECORD
           MOVE WS-INPUT-NAME TO PL-FNAME
           PERFORM TTPLAYER2-START
           PERFORM 
              UNTIL NO-MORE-RECORDS
               IF PL-FNAME = WS-INPUT-NAME
                   PERFORM TTPLAYER-DISPLAY-RECORD
                   PERFORM WRITE-JSON-RECORD
                   PERFORM TTPLAYER2-READ-NEXT
               ELSE
                   SET NO-MORE-RECORDS TO TRUE
               END-IF
           END-PERFORM
           .

      *****************************************************************
       TTPLAYER-DISPLAY-RECORD         SECTION.

           DISPLAY 'ID: 'PL-ID
           DISPLAY 'Lname: 'PL-LNAME
           DISPLAY 'Fname: 'PL-FNAME
           DISPLAY 'City: 'PL-CITY
           DISPLAY 'State: 'PL-STATE
           DISPLAY 'FLAGS: 'PL-FLAGS
           DISPLAY 'PL-PCS-PAS-DATE: 'PL-PCS-PAS-DATE
           .
      *****************************************************************
       WRITE-JSON-RECORD               SECTION.

           IF FL-FIRST-RECORD-YES
               PERFORM ADD-STARTING-STRING
               SET FL-FIRST-RECORD-NO TO TRUE
           ELSE
               PERFORM ADD-COMMA
           END-IF

           STRING
               WS-STRING             DELIMITED BY C-DOUBLE-SPACE
               C-SPACE               DELIMITED BY SIZE
               C-OPEN-CURLY-BRACKET  DELIMITED BY SIZE
               INTO WS-STRING
           END-STRING

           SET FL-FIRST-FIELD-YES    TO TRUE
           PERFORM ADD-LNAME
           SET FL-FIRST-FIELD-NO     TO TRUE
           PERFORM ADD-FNAME
           PERFORM ADD-ADDRESS-LINE1
           PERFORM ADD-CITY
           PERFORM ADD-STATE
           PERFORM ADD-POSTCODE
           PERFORM ADD-EMPLOYEE
           PERFORM ADD-VIP

           PERFORM ADD-BIRTHDAY

           STRING
               WS-STRING              DELIMITED BY C-DOUBLE-SPACE
               C-SPACE                DELIMITED BY SIZE
               C-CLOSE-CURLY-BRACKET  DELIMITED BY SIZE
               INTO WS-STRING
           END-STRING
           .

      *****************************************************************
       ADD-STARTING-STRING             SECTION.

           STRING
               C-OPEN-CURLY-BRACKET  DELIMITED BY SIZE
               C-SPACE               DELIMITED BY SIZE
               C-QUOTE               DELIMITED BY SIZE
               C-RESULTS             DELIMITED BY SIZE
               C-QUOTE               DELIMITED BY SIZE
               C-COLON               DELIMITED BY SIZE
               C-OPEN-SQUARE-BRACKET DELIMITED BY SIZE
               INTO WS-STRING
           END-STRING
           .

      *****************************************************************
       ADD-COMMA                       SECTION.

           STRING
               WS-STRING             DELIMITED BY C-DOUBLE-SPACE
               C-COMMA               DELIMITED BY SIZE
               C-SPACE               DELIMITED BY SIZE
               INTO WS-STRING
           END-STRING
           .
           
      *****************************************************************
       ADD-LNAME                       SECTION.

           MOVE C-LAST-NAME          TO WS-ITEM-NAME
           MOVE PL-LNAME             TO WS-VALUE
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .

      *****************************************************************
       ADD-FNAME                       SECTION.

           MOVE C-FIRST-NAME         TO WS-ITEM-NAME
           MOVE PL-FNAME             TO WS-VALUE
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .

      *****************************************************************
       ADD-ADDRESS-LINE1               SECTION.

           MOVE C-ADDRESS-LINE1      TO WS-ITEM-NAME
           MOVE PL-ADDR1-FIRST-30    TO WS-VALUE
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .
               
      *****************************************************************
       ADD-CITY                        SECTION.

           MOVE C-CITY               TO WS-ITEM-NAME
           MOVE PL-CITY              TO WS-VALUE
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .
      *****************************************************************
       ADD-STATE                       SECTION.

           MOVE C-STATE              TO WS-ITEM-NAME
           MOVE PL-STATE             TO WS-VALUE
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .
           
      *****************************************************************
       ADD-POSTCODE                    SECTION.

           MOVE C-POSTCODE           TO WS-ITEM-NAME
           MOVE PL-ZIP               TO WS-VALUE-NUMERIC
           SET FL-NUMERIC-YES         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .
      
      *****************************************************************
       ADD-EMPLOYEE                    SECTION.

           MOVE C-EMPLOYEE           TO WS-ITEM-NAME
           IF PL-EMPLOYEE  = "Y"
               MOVE C-YES            TO WS-VALUE
           ELSE 
               MOVE C-NO             TO WS-VALUE 
           END-IF
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .
              
      *****************************************************************
       ADD-VIP                         SECTION.

           MOVE C-VIP                TO WS-ITEM-NAME
           IF PL-VIP-YES  
               MOVE C-YES            TO WS-VALUE 
           ELSE
               MOVE C-NO             TO WS-VALUE 
           END-IF
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .

      *****************************************************************
       ADD-BIRTHDAY                    SECTION.

           MOVE C-BIRTHDAY           TO WS-ITEM-NAME
           STRING
               PL-BDAY               DELIMITED BY SIZE
               C-BACKSLASH           DELIMITED BY SIZE
               PL-BMONTH             DELIMITED BY SIZE
               C-BACKSLASH           DELIMITED BY SIZE 
               PL-BYEAR              DELIMITED BY SIZE
               INTO WS-VALUE 
           END-STRING
           SET FL-NUMERIC-NO         TO TRUE
           PERFORM ADD-STRING-VALUE-PAIR
           .
                          
      *****************************************************************
       ADD-STRING-VALUE-PAIR           SECTION.

           IF FL-FIRST-FIELD-NO
               PERFORM ADD-COMMA
           END-IF

           STRING   
               WS-STRING            DELIMITED BY C-DOUBLE-SPACE
               C-SPACE              DELIMITED BY SIZE
               C-QUOTE              DELIMITED BY SIZE           
               WS-ITEM-NAME         DELIMITED BY C-DOUBLE-SPACE
               C-QUOTE              DELIMITED BY SIZE
               C-COLON              DELIMITED BY SIZE
                                    INTO WS-STRING
           END-STRING

           IF FL-NUMERIC-YES
               PERFORM ADD-NUMERIC-VALUE
           ELSE
               PERFORM ADD-STRING-VALUE
           END-IF
           .

      *****************************************************************
       ADD-NUMERIC-VALUE               SECTION.

          STRING 
              WS-STRING            DELIMITED BY C-DOUBLE-SPACE
              C-SPACE              DELIMITED BY SIZE          
              WS-VALUE-NUMERIC     DELIMITED BY SIZE
                                   INTO WS-STRING
           END-STRING     
           .

      *****************************************************************
       ADD-STRING-VALUE                SECTION.

           STRING 
              WS-STRING            DELIMITED BY C-DOUBLE-SPACE
              C-QUOTE              DELIMITED BY SIZE           
              WS-VALUE             DELIMITED BY C-DOUBLE-SPACE
              C-QUOTE              DELIMITED BY SIZE

                                   INTO WS-STRING
           END-STRING
           . 

      *****************************************************************
         COPY 'TTPLAYER.IO'. .

