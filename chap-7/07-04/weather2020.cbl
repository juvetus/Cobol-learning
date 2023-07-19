       IDENTIFICATION DIVISION.
       PROGRAM-ID. WEATHER2020.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
          SELECT WEATHER-FILE ASSIGN TO "WEATHER2020.DAT"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.

       FD WEATHER-FILE.
       01 WEATHER-RECORD PIC X(100).

       WORKING-STORAGE SECTION.
       01 WEATHER-TABLE.
          02 WEATHER-ENTRY OCCURS 100 TIMES.
             03 NUMERO-ENTRY PIC X(10).
             03 MOIS-ENTRY PIC XX.
             03 JOUR-ENTRY PIC XX.
             03 ANNEE-ENTRY PIC X(4).
             03 TEMPERATURE-ENTRY PIC X(4).
             03 VALEUR-1-ENTRY PIC X.
             03 VALEUR-2-ENTRY PIC X.
             03 VALEUR-3-ENTRY PIC X.
             03 VALEUR-4-ENTRY PIC X.
             03 VALEUR-5-ENTRY PIC X.
             03 VALEUR-6-ENTRY PIC X.

       01 TABLE-SIZE PIC 9(4) VALUE 0.
       01 USER-INPUT PIC X.
       01 SEARCH-NUMERO PIC X(10).
       01 WEATHER-FILE-STATUS PIC X.
       01 I PIC 9(4).

       PROCEDURE DIVISION.
       MAIN-LOGIC.
          OPEN INPUT WEATHER-FILE
          READ WEATHER-FILE INTO WEATHER-RECORD
              AT END MOVE '35' TO WEATHER-FILE-STATUS
          NOT AT END
              PERFORM READ-PROCESS-RECORD
          END-READ
          CLOSE WEATHER-FILE
          PERFORM DISPLAY-MENU
          STOP RUN.

       READ-PROCESS-RECORD.
          MOVE WEATHER-RECORD(1:10) TO NUMERO-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(12:2) TO MOIS-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(15:2) TO JOUR-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(18:4) TO ANNEE-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(23:4) TO TEMPERATURE-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(28:1) TO VALEUR-1-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(30:1) TO VALEUR-2-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(32:1) TO VALEUR-3-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(34:1) TO VALEUR-4-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(36:1) TO VALEUR-5-ENTRY(TABLE-SIZE)
          MOVE WEATHER-RECORD(38:1) TO VALEUR-6-ENTRY(TABLE-SIZE)
          ADD 1 TO TABLE-SIZE.

       DISPLAY-MENU.
          DISPLAY "MENU:"
          DISPLAY "1. Afficher toutes les données"
          DISPLAY "2. Rechercher par numéro"
          DISPLAY "3. Quitter"
          ACCEPT USER-INPUT
          IF USER-INPUT = '1' THEN
              PERFORM DISPLAY-ALL-DATA
          ELSE IF USER-INPUT = '2' THEN
              PERFORM SEARCH-BY-NUMERO
          ELSE IF USER-INPUT = '3' THEN
              PERFORM QUIT-PROGRAM
          ELSE
              DISPLAY "Option invalide"
              PERFORM DISPLAY-MENU
          END-IF.

       DISPLAY-ALL-DATA.
          DISPLAY "Données disponibles :"
          PERFORM VARYING I FROM 1 BY 1 UNTIL I > TABLE-SIZE
              DISPLAY "Numéro : " NUMERO-ENTRY(I)
              DISPLAY "Mois : " MOIS-ENTRY(I)
              DISPLAY "Jour : " JOUR-ENTRY(I)
              DISPLAY "Année : " ANNEE-ENTRY(I)
              DISPLAY "Température : " TEMPERATURE-ENTRY(I)
              DISPLAY "Valeur 1 : " VALEUR-1-ENTRY(I)
              DISPLAY "Valeur 2 : " VALEUR-2-ENTRY(I)
              DISPLAY "Valeur 3 : " VALEUR-3-ENTRY(I)
              DISPLAY "Valeur 4 : " VALEUR-4-ENTRY(I)
              DISPLAY "Valeur 5 : " VALEUR-5-ENTRY(I)
              DISPLAY "Valeur 6 : " VALEUR-6-ENTRY(I)
          END-PERFORM
          PERFORM DISPLAY-MENU.

       SEARCH-BY-NUMERO.
          DISPLAY "Entrez le numéro à rechercher :"
          ACCEPT SEARCH-NUMERO
          PERFORM VARYING I FROM 1 BY 1 UNTIL I > TABLE-SIZE
              IF NUMERO-ENTRY(I) = SEARCH-NUMERO THEN
                  DISPLAY "Numéro : " NUMERO-ENTRY(I)
                  DISPLAY "Mois : " MOIS-ENTRY(I)
                  DISPLAY "Jour : " JOUR-ENTRY(I)
                  DISPLAY "Année : " ANNEE-ENTRY(I)
                  DISPLAY "Température : " TEMPERATURE-ENTRY(I)
                  DISPLAY "Valeur 1 : " VALEUR-1-ENTRY(I)
                  DISPLAY "Valeur 2 : " VALEUR-2-ENTRY(I)
                  DISPLAY "Valeur 3 : " VALEUR-3-ENTRY(I)
                  DISPLAY "Valeur 4 : " VALEUR-4-ENTRY(I)
                  DISPLAY "Valeur 5 : " VALEUR-5-ENTRY(I)
                  DISPLAY "Valeur 6 : " VALEUR-6-ENTRY(I)
                  EXIT PERFORM
              END-IF
          END-PERFORM
          IF I > TABLE-SIZE THEN
              DISPLAY "Numéro non trouvé."
          END-IF
          PERFORM DISPLAY-MENU.

       QUIT-PROGRAM.
          DISPLAY "Quitting..."
          CLOSE WEATHER-FILE.

