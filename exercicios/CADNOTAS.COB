       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADNOTAS
       AUTHOR. RICARDO DE LUCAS DO NASCIMENTO.
      **************************************
      * MANUTENCAO DO CADASTRO DE NOTAS    *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT NOTAS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS RA
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD NOTAS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADNOTAS.DAT".
       01 REGNOTAS.
                03 RA                   PIC X(13).
                03 NOME                 PIC X(32).
                03 NOTA1                PIC 9(02).
                03 NOTA2                PIC 9(02).
                03 MEDIA                PIC 9(02).
                03 FALTA1               PIC 9(02).
                03 FALTA2               PIC 9(02).
                03 TOTAL-FALTAS         PIC 9(02).
                03 SITUACAO             PIC X(20).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      * NOTE: Eu acho que isso é pra guardar as variaveis que eu 
      * preciso pra manipular as coisas
       01 MASC1        PIC ZZZ.ZZ9,99.
       01 MASC2        PIC ZZZZ.ZZZ.ZZ9,99.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
      *-----------------------------------------------------------------
      *
       PROCEDURE DIVISION.
       INICIO.

       INC-OP0.
           OPEN I-O NOTAS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT NOTAS
                      CLOSE NOTAS
                      MOVE "*** ARQUIVO NOTAS SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO NOTAS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       CLEAR-SCREEN.
                MOVE ZEROS TO NOTA1 NOTA2 MEDIA 
                MOVE ZEROS TO FALTA1 FALTA2 TOTAL-FALTAS
                MOVE SPACES TO RA NOME SITUACAO
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "CADASTRO DE NOTAS"
                DISPLAY (03, 01) "RA               : "
                DISPLAY (04, 01) "NOME             : "
                DISPLAY (05, 01) "NOTA1            : "
                DISPLAY (06, 01) "NOTA2            : "
                DISPLAY (07, 01) "MEDIA            : "
                DISPLAY (08, 01) "FALTA1           : "
                DISPLAY (09, 01) "FALTA2           : "
                DISPLAY (10, 01) "TOTAL FALTAS     : "
                DISPLAY (15, 01) "SITUACAO         : ".
      
       LER-RA.
           ACCEPT (3, 19) RA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE NOTAS
                   GO TO ROT-FIM.
           
                IF RA = SPACES
                   MOVE "*** RA INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO LER-RA.
       SHOW-DADOS.
      * TODO: Criar função para a regra da situação do aluno
                MOVE 0 TO W-SEL
                READ NOTAS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (04, 19) NOME
                      DISPLAY (05, 19) NOTA1 
                      DISPLAY (06, 19) NOTA2
                      DISPLAY (07, 19) MEDIA
                      DISPLAY (08, 19) FALTA1
                      DISPLAY (09, 19) FALTA2
                      DISPLAY (10, 19) TOTAL-FALTAS
                      DISPLAY (15, 19) SITUACAO
                      MOVE "*** NOTA JÁ CADASTRADA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO NOTAS"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
      * "Rotinas" de leitura das variaveis
      * TODO: Corrigir as rotinas de leitura da variavel
      
       LER-NOME.
                ACCEPT (04, 19) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-NOTAS01.
       
       LER-NOTAS01.
                ACCEPT (05, 19) NOTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-NOTAS02.
      
       LER-NOTAS02.
                ACCEPT (06, 19) NOTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO CALC-MEDIA.

       CALC-MEDIA.
           COMPUTE MEDIA = (NOTA1 + NOTA2) / 2
           DISPLAY(07, 19) MEDIA
           GO TO LER-FALTAS01.

       LER-FALTAS01.
                ACCEPT (08, 19) FALTA1
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-FALTAS02.

       LER-FALTAS02.
                ACCEPT (09, 19) FALTA2
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO CALC-TOTALFALTAS.

       CALC-TOTALFALTAS.
           COMPUTE TOTAL-FALTAS = (FALTA1 + FALTA2)
           DISPLAY (10, 19) TOTAL-FALTAS.

       SHOW-SITUACAO.
           IF TOTAL-FALTAS > 20
                IF MEDIA > 4 
                        MOVE "RECUPERACAO - FALTAS" TO SITUACAO
                ELSE
                        MOVE "REPROVADO" TO SITUACAO
           ELSE
               IF MEDIA > 4
                        MOVE "APROVADO" TO SITUACAO
               ELSE
                        MOVE "RECUPERACAO - NOTAS" TO SITUACAO
           
           DISPLAY (15, 19) SITUACAO
           IF W-SEL = 03 GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/n) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO CLEAR-SCREEN.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      * Rotina de Escrita
       WRITE-REG.
           WRITE REGNOTAS
           IF ST-ERRO = "00" OR "02"
                MOVE "*** DADOS GRAVADOS *** " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO CLEAR-SCREEN.
      * TODO: Ver exatamente o que esse numero magico "22" faz     
                IF ST-ERRO = "22"
                      MOVE "*** NOTA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO CLEAR-SCREEN
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE NOTAS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTERAR   F3=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO CLEAR-SCREEN.
                IF W-ACT = 03
                  MOVE 03 TO W-SEL      
                  PERFORM LER-NOME THRU SHOW-SITUACAO
                  GO TO ALT-OPC.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE NOTAS RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO CLEAR-SCREEN.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
           REWRITE REGNOTAS
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO NOTAS"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                EXIT PROGRAM.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 1500
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       ROT-ALFA-FIM.
