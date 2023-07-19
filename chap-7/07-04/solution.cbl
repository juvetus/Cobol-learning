	   IDENTIFICATION DIVISION.
	   PROGRAM-ID. WEATHER-PROGRAM.

	   ENVIRONMENT DIVISION.
	   INPUT-OUTPUT SECTION.
	   FILE-CONTROL.
		  SELECT WEATHER-FILE ASSIGN TO "weather2020.DAT"
			  ORGANIZATION IS LINE SEQUENTIAL.

	   DATA DIVISION.
	   FILE SECTION.
	   FD WEATHER-FILE.
	   01 WEATHER-RECORD.
		  02 WEATHER-NUMBER PIC 9(6).
		  02 WEATHER-TEMPERATURE PIC S9(3).

	   WORKING-STORAGE SECTION.
	   01 TABLE-ENTRY.
		  02 TABLE-NUMBER PIC 9(6).
		  02 TABLE-TEMPERATURE PIC S9(3).
	   01 TABLE OCCURS 100 TIMES.
		  02 TABLE-ROW.
			 03 TABLE-NUMBER PIC 9(6).
			 03 TABLE-TEMPERATURE PIC S9(3).
	   01 TABLE-ROW-INDEX PIC 99.
	   01 SEARCH-NUMBER PIC 9(6).
	   01 PRINT-OPTION PIC X.

	   PROCEDURE DIVISION.
	   MAIN-LOGIC.
		  OPEN INPUT WEATHER-FILE
		  PERFORM READ-WEATHER-FILE
		  PERFORM MENU-SELECTION
		  CLOSE WEATHER-FILE
		  STOP RUN.

	   READ-WEATHER-FILE.
		  READ WEATHER-FILE
			  AT END SET TABLE-NUMBER(INDEX) OF TABLE(1) TO -1
			  NOT AT END
				  MOVE WEATHER-NUMBER TO TABLE-NUMBER(INDEX)
				   OF TABLE(TABLE-ROW-INDEX)
				  MOVE WEATHER-TEMPERATURE 
				  TO TABLE-TEMPERATURE(INDEX) OF TABLE(TABLE-ROW-INDEX)
				  ADD 1 TO TABLE-ROW-INDEX
				  PERFORM READ-WEATHER-FILE.

	   MENU-SELECTION.
		  DISPLAY "Menu:"
		  DISPLAY "1. Print entire table"
		  DISPLAY "2. Search for a number"
		  DISPLAY "Enter your choice (1 or 2): "
		  ACCEPT PRINT-OPTION
		  IF PRINT-OPTION = "1" THEN
			  PERFORM PRINT-ENTIRE-TABLE
		  ELSE IF PRINT-OPTION = "2" THEN
			  DISPLAY "Enter the number to search for: "
			  ACCEPT SEARCH-NUMBER
			  PERFORM SEARCH-TABLE
		  ELSE
			  DISPLAY "Invalid choice"
			  PERFORM MENU-SELECTION
		  END-IF.

	   PRINT-ENTIRE-TABLE.
		  DISPLAY "Table content:"
		  PERFORM VARYING TABLE-ROW-INDEX FROM 1 BY 1 UNTIL TABLE-NUMBER
		  (INDEX) OF TABLE(TABLE-ROW-INDEX) = -1
			  DISPLAY "Number: " TABLE-NUMBER(INDEX) OF TABLE
			  (TABLE-ROW-INDEX)
			  DISPLAY "Temperature: " TABLE-TEMPERATURE(INDEX) 
			  OF TABLE(TABLE-ROW-INDEX)
		  END-PERFORM.

	   SEARCH-TABLE.
		  SET TABLE-ROW-INDEX TO 1
		  PERFORM UNTIL TABLE-NUMBER(INDEX) OF TABLE(TABLE-ROW-INDEX) = -1
			  IF TABLE-NUMBER(INDEX) OF TABLE(TABLE-ROW-INDEX) = SEARCH-NUMBER
				  DISPLAY "Number found:"
				  DISPLAY "Number: " TABLE-NUMBER(INDEX) OF TABLE
				  (TABLE-ROW-INDEX)
				  DISPLAY "Temperature: " TABLE-TEMPERATURE(INDEX) OF 
				  TABLE(TABLE-ROW-INDEX)
				  SET TABLE-ROW-INDEX TO 9999
			  ELSE
				  ADD 1 TO TABLE-ROW-INDEX
			  END-IF
		  END-PERFORM
		  IF TABLE-ROW-INDEX > 100
			  DISPLAY "Number not found"
		  END-IF.

	   END PROGRAM WEATHER-PROGRAM.
