       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADASTROCEP
       AUTHOR. RICARDO DE LUCAS DO NASCIMENTO.
      **************************************
      * MANUTENCAO DO CADASTRO DE CEP      *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CEPS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CEPS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEPS.DAT".
       01 REGCEPS.
                03 CEP                  PIC 9(8).
                03 LOGRADOURO           PIC X(35).
                03 BAIRRO               PIC X(20).
                03 CIDADE               PIC X(20).
                03 UF                   PIC X(02).
                03 LATITUDE             PIC X(20).
                03 LONGITUDE            PIC X(20).
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
      
       SCREEN SECTION

       01  TELACEP.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                               Cadastro".
           05  LINE 02  COLUMN 41 
               VALUE  "de Cep".
           05  LINE 04  COLUMN 01 
               VALUE  "    CEP:".
           05  LINE 06  COLUMN 01 
               VALUE  "    Logradouro:".
           05  LINE 07  COLUMN 01 
               VALUE  "    Bairro    :".
           05  LINE 08  COLUMN 01 
               VALUE  "    Cidade    :".
           05  LINE 08  COLUMN 41 
               VALUE  "  UF:".
           05  LINE 10  COLUMN 01 
               VALUE  "    Latitude  :".
           05  LINE 11  COLUMN 01 
               VALUE  "    Longitude :".
           05  TCEP
               LINE 04  COLUMN 10  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 06  COLUMN 17  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 07  COLUMN 17  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 08  COLUMN 17  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 08  COLUMN 47  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TLATITUDE
               LINE 10  COLUMN 17  PIC X(20)
               USING  LATITUDE
               HIGHLIGHT.
           05  TLONGITUDE
               LINE 11  COLUMN 17  PIC X(20)
               USING  LONGITUDE
               HIGHLIGHT.
           05  TMENS
               LINE 15  COLUMN 13  PIC X(50)
               USING  MENS
               HIGHLIGHT.
      *
       PROCEDURE DIVISION.
       INICIO.

       INC-OP0.
           OPEN I-O CEPS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CEPS
                      CLOSE CEPS
                      MOVE "*** ARQUIVO CEPS SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CEPS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       CLEAR-SCREEN.
                MOVE ZEROS TO CEP 
                MOVE SPACES TO LOGRADOURO BAIRRO CIDADE
                MOVE SPACES TO UF LATITUDE LONGITUDE
                DISPLAY TELACEP.
      
       LER-CEP.
           ACCEPT TCEP
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CEPS
                   GO TO ROT-FIM.
           
                IF CEP = SPACES
                   MOVE "*** CEP INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO LER-CEP.
       SHOW-DADOS.
      * TODO: Criar função para a regra da situação do aluno
                MOVE 0 TO W-SEL
                READ CEPS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TCEP
                      DISPLAY TLOGRADOURO 
                      DISPLAY TBAIRRO
                      DISPLAY TCIDADE
                      DISPLAY TUF
                      DISPLAY TLATITUDE
                      DISPLAY TLONGITUDE
                      MOVE "*** CEP JÁ CADASTRADO ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA DO ARQUIVO CEPS"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
      * "Rotinas" de leitura das variaveis
      * TODO: Corrigir as rotinas de leitura da variavel
      
       LER-LOGRADOURO.
                ACCEPT TLOGRADOURO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-BAIRRO.
       
       LER-BAIRRO.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-CIDADE.
      
       LER-CIDADE.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-UF.

       LER-UF.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-LATITUDE.

       LER-LATITUDE.
                ACCEPT TLATITUDE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-LONGITUDE.

       LER-LONGITUDE.
                ACCEPT TLONGITUDE
                ACCEPT W-ACT FROM ESCAPE KEY
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
           WRITE REGCEPS
           IF ST-ERRO = "00" OR "02"
                MOVE "*** DADOS GRAVADOS *** " TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO CLEAR-SCREEN.
      * TODO: Ver exatamente o que esse numero magico "22" faz     
                IF ST-ERRO = "22"
                      MOVE "*** CEP JA EXISTE ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO CLEAR-SCREEN
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CEPS"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTECEPCAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "F1=NOVO REGISTRO   F2=ALTECEPR   F3=EXCLUIR"
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
                  PERFORM LER-LOGRADOURO THRU LER-LONGITUDE 
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
                DELETE CEPS RECORD
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
           REWRITE REGCEPS
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CEPS"   TO MENS
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
               DISPLAY TMENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 1500
                   GO TO ROT-MENS2.
       ROT-MENS-FIM.
                EXIT.
       ROT-ALFA-FIM.
