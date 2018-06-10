     *************************************************************
       TTPLAYER-WRITE                  SECTION.

           WRITE PL-RECORD
             INVALID KEY
                 DISPLAY
                     "REWRITE KEY: " PL-ID OF PL-RECORD
                 END-DISPLAY
                   REWRITE PL-RECORD
                       INVALID KEY
                           DISPLAY
                               "ERROR BAD KEY: "
                               PL-ID OF PL-RECORD
                           END-DISPLAY
                   END-REWRITE
           END-WRITE
           .

      *************************************************************
       TTPLAYER-START                 SECTION.

           DISPLAY "PRIME < " PL-ID OF PL-RECORD END-DISPLAY
           SET NO-MORE-RECORDS TO FALSE
           START TTPLAYER
              KEY IS GREATER THAN OR EQUAL TO
                  PL-ID OF PL-RECORD
              INVALID KEY
                  DISPLAY
                      "ERROR BAD START: " PL-ID OF PL-RECORD
                  END-DISPLAY
                  SET NO-MORE-RECORDS TO TRUE
              NOT INVALID KEY
                  READ TTPLAYER NEXT RECORD
                      AT END SET NO-MORE-RECORDS TO TRUE
                  END-READ
           END-START
           .

      *************************************************************
        TTPLAYER-READ-NEXT             SECTION.

           READ TTPLAYER NEXT RECORD
               AT END SET NO-MORE-RECORDS TO TRUE
           END-READ           
           .

      *************************************************************
       TTPLAYER1-START                 SECTION.

           SET NO-MORE-RECORDS TO FALSE
           DISPLAY "SEEKING >= " PL-KEY1 OF PL-RECORD END-DISPLAY
           START TTPLAYER
              KEY IS GREATER THAN OR EQUAL TO
                  PL-KEY1 OF PL-RECORD
              INVALID KEY
                  DISPLAY
                      "Doesn't: " PL-KEY1 OF PL-RECORD
                  END-DISPLAY
                  SET NO-MORE-RECORDS TO TRUE
              NOT INVALID KEY
                  READ TTPLAYER NEXT RECORD
                      AT END SET NO-MORE-RECORDS TO TRUE
                  END-READ
           END-START
           .

      *************************************************************
        TTPLAYER1-READ-NEXT            SECTION.

           READ TTPLAYER NEXT RECORD
               AT END SET NO-MORE-RECORDS TO TRUE
           END-READ           
           .

      *************************************************************
       TTPLAYER2-START                 SECTION.

           SET NO-MORE-RECORDS TO FALSE
           DISPLAY "SEEKING >= " PL-FNAME OF PL-RECORD END-DISPLAY
           START TTPLAYER
              KEY IS GREATER THAN OR EQUAL TO
                  PL-FNAME OF PL-RECORD
              INVALID KEY
                  DISPLAY
                      "Doesn't exist: " PL-FNAME OF PL-RECORD
                  END-DISPLAY
                  SET NO-MORE-RECORDS TO TRUE
              NOT INVALID KEY
                  READ TTPLAYER NEXT RECORD
                      AT END SET NO-MORE-RECORDS TO TRUE
                  END-READ
           END-START
           .

      *************************************************************
        TTPLAYER2-READ-NEXT            SECTION.

           READ TTPLAYER NEXT RECORD
               AT END SET NO-MORE-RECORDS TO TRUE
           END-READ           
           .

