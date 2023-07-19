       IDENTIFICATION DIVISION.
       PROGRAM-ID. "BMICALCULATOR".
       AUTHOR.     PEGGY FISHER.
      *This program reads input from the user
       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WEIGHT PIC 999.
       01 HEIGHT_INCHES PIC 999. 
       01 BMI    PIC 999V99.
         
       PROCEDURE DIVISION. 
       0100-START-HERE.
            DISPLAY "Entrez votre poids".
            ACCEPT HEIGHT_INCHES.
            DISPLAY "Entrez votre taille (ex: 175) :  ".
            ACCEPT WEIGHT.
         
            COMPUTE BMI = WEIGHT * 703 /(HEIGHT_INCHES * HEIGHT_INCHES).

            DISPLAY "votre BMI est : " , BMI , "%".

       STOP RUN.
       END PROGRAM BMICALCULATOR.
