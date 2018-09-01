       IDENTIFICATION DIVISION.
       PROGRAM-ID. P18201.
       AUTHOR. EITOR E LJ.
      **********************
      * CADASTRO DE ALUNOS *
      **********************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT AMIGO TO ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    FILE STATUS  IS ST-ERRO
      *
      *----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD AMIGO
              LABEL RECORD IS STANDARD
              VALUE OF FILE-ID IS "CADAmigo.DAT".
       01 REGAmigo.
              03 AMG-APELIDO       PIC X(12).
              03 AMG-NOME          PIC X(35).
              03 AMG-EMAIL         PIC X(35).
      *
      *----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
       INICIO.
      *
       INI-000.
           OPEN I-O AMIGO
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT AMIGO
                      CLOSE PRODUTO
                      MOVE "*** ARQUIVO AMIGO SENDO CRIADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-000.
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO AMIGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
		INI-001.
				MOVE SPACES TO AMG-APELIDO AMG-EMAIL AMG-NOME
				DISPLAY TP18201.
		INC-002.
				ACCEPT TAMG-APELIDO
				ACCEPT W-ACT FROM ESCAPE KEY
					IF W-ACT = 02
						CLOSE AMIGO
						GO TO ROT-FIM.
					IF AMG-APELIDO = ""
						MOVE "*** APELIDO INVALIDO ***" TO MENS
						PERFORM ROT-MENS THRU ROT-MENS-FIM
						GO TO INC-002.
		LER-AMIGO01.
					MOVE 0 TO W-SEL
					READ AMIGO
					IF ST-ERRO NOT ="23"
						IF ST-ERRO ="00"
							DISPLAY TP18201
							MOVE "*** APELIDO JA CADASTRADO ***" TO MENS
							PERFORM ROT-MENS THRU ROT-MENS-FIM
							GO TO ACE-001.
						ELSE 
							MOVE "ERRO NA LEITURA DO ARQUIVO AMIGO" TO MENS
							PERFORM ROT-MENS THRU ROT-MENS-FIM
							GO TO ROT-FIM
					ELSE
						NEXT SENTENCE.