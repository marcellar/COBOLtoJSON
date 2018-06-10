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

       DATA DIVISION.
       FILE SECTION.
       COPY "TTPLAYER.FD". 
       WORKING-STORAGE                 SECTION.   
        01 CONTROL-FLAG         PIC X.
          88 NO-MORE-DUPLICATES          VALUE 1
             WHEN SET TO FALSE IS              0.
          88 NO-MORE-RECORDS             VALUE 1
             WHEN SET TO FALSE IS              0.

      *****************************************************************
       PROCEDURE DIVISION.

        OPEN  I-O TTPLAYER

        MOVE '0000000001Smith          John           1 High St            
      - '            Perth          WA6000                         NDNNN
      - 'C608MY01036820180203281234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000002Smith          Jane           1 High St                   
      - '            Perth          WA6000                         NDNNN
      - 'C608MY01036820180203281234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000003Walsh          Steven         1 Main St                   
      - '            Melbourne      VI3000                         NDNNN
      - 'C608MY15084420180204281334         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE

        MOVE '0000000004Smithers       Paul           17 Second St                 
      - '            Chadstone      VI3148                         NDNNN
      - 'C608MY22065620180205341537         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000005Smithe         Mary           45 Third St                
      - '            Murrumbeena    VI3163                         NDNNN
      - 'C608MY11038320180203281834         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000006Walsh          John           11 Warrigul Rd               
      - '            Ashburton      VI3147                         NDNNN
      - 'C608MY01036820180203281234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000007Parker         Peter          101 High St                   
      - '            Ashburton      VI3147                         NDNNN
      - 'C608MY11048320170403681834         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000008Jones          Parker         25 John St              
      - '            Ashburton      VI3147                         NDNNN
      - 'C608MY11048320170403681834         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE

        MOVE '0000000009Richards       Susan          19 Paul St                  
      - '            Oakleigh       VI3166                         NDNNN
      - 'C608MY17038520180603281234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000010Peters         Greg           101 High St                  
      - '            Chadville      WA6345                         NDNNN
      - 'C608MY01036820180203281234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000011Clarke         Susan          45 Entally Dr              
      - '            Holmsglen      VI3120                         NDNNN
      - 'C608MY18036820180203281237         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000012Pig            Peppa          34 Yarra Blvd          
      - '            Jordanville    VI3276                         NDNNN
      - 'C608MY01056820170403121234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000013Peppa          Kylie          11 Fence Ln              
      - '            Hawthorne      VI3456                         NDNNN
      - 'C608MY01036820180303231234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000014Lennon         John           15 Gate Rd                
      - '            Kooyong        VI3234                         NDNNN
      - 'C608MY20056820180203241234         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000015Kent           Clarke         17 Mark Ave              
      - '            Perth          WA6000                         NDNNN
      - 'C608MY01056820180203281234         ' 
            TO PL-RECORD
        PERFORM WRITE-TO-FILE
        MOVE '0000000016Smith          John           29 Highbury Rd            
      - '            Burwood        VI3125                         NDNNN
      - 'C608MY19048820280109284321         '
            TO PL-RECORD
        PERFORM WRITE-TO-FILE     PERFORM READ-AND-DISPLAY-KEY0
        PERFORM READ-AND-DISPLAY-KEY1

        CLOSE TTPLAYER
        STOP RUN.

      *************************************************************
       WRITE-TO-FILE                   SECTION.

       PERFORM TTPLAYER-WRITE
       PERFORM TTPLAYER-DISPLAY-RECORD
       .

      *************************************************************
       READ-AND-DISPLAY-KEY0                SECTION.

           MOVE 0000000001 TO PL-ID
           PERFORM TTPLAYER-START
           PERFORM 
              UNTIL NO-MORE-RECORDS
               PERFORM TTPLAYER-DISPLAY-RECORD
               PERFORM TTPLAYER-READ-NEXT
           END-PERFORM
           .

      *************************************************************
       READ-AND-DISPLAY-KEY1                SECTION.

           MOVE LOW-VALUES TO PL-KEY1
           PERFORM TTPLAYER1-START
           PERFORM 
              UNTIL NO-MORE-RECORDS
               PERFORM TTPLAYER-DISPLAY-RECORD
               PERFORM TTPLAYER1-READ-NEXT
           END-PERFORM
           .

      *************************************************************
       TTPLAYER-DISPLAY-RECORD         SECTION.

           DISPLAY 'ID: 'PL-ID
           DISPLAY 'Lname: 'PL-LNAME
           DISPLAY 'Fname: 'PL-FNAME
           DISPLAY 'FLAGS: 'PL-FLAGS
           DISPLAY 'PL-PCS-PAS-DATE: 'PL-PCS-PAS-DATE
           .

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
                               "REALLY BAD KEY: "
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
                      "BAD START: " PL-ID OF PL-RECORD
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
                      "BAD START: " PL-KEY1 OF PL-RECORD
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

