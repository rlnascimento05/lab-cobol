       IDENTIFICATION DIVISION.
       PROGRAM-ID. P18201.COB
       AUTHOR. RICARDO DE LUCAS DO NASCIMENTO.
      **************************************
      * MANUTENCAO DO CADASTRO DE AMIGOS  * TODO: CHANGE THIS LATER
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT AMIGOS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS AMIGO-APELIDO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS AMIGO-NOME
                                            WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD AMIGOS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADAMIGO.DAT".
       01 REGAMIGO.
                03 AMIGO-APELIDO   PIC X(13).
                03 AMIGO-NOME      PIC X(32).
                03 AMIGO-EMAIL     PIC X(32).
                03 AMIGO-TELEFONE. 
                   05 AMIGO-DDD    PIC 9(02).
                   05 AMIGO-NUMTEL PIC 9(09).
                03 AMIGO-DATANASC.
                   05 AMIGO-DIA    PIC 9(02).
                   05 AMIGO-MES    PIC 9(02).
                   05 AMIGO-ANO    PIC 9(04).
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
       SCREEN SECTION.

       01  TELAAMIGO.
           05  LINE 02  COLUMN 01
               VALUE  "                      CADASTRO DE AMIGOS".
           05  LINE 03  COLUMN 01
               VALUE  "  旼컴컴컴컴컴컴컴쩡컴컴컴컴컴컴컴컴컴컴".
           05  LINE 03  COLUMN 41
               VALUE  "컴컴컴컴컴컴컴?.
           05  LINE 04  COLUMN 01
               VALUE  "  ?PELIDO        ?".
           05  LINE 04  COLUMN 41
               VALUE  "              ?.
           05  LINE 05  COLUMN 01
               VALUE  "  ?              ?.
           05  LINE 05  COLUMN 41
               VALUE  "              ?.
           05  LINE 06  COLUMN 01
               VALUE  "  ?OME           ?".
           05  LINE 06  COLUMN 41
               VALUE  "              ?.
           05  LINE 07  COLUMN 01
               VALUE  "  ?              ?.
           05  LINE 07  COLUMN 41
               VALUE  "              ?.
           05  LINE 08  COLUMN 01
               VALUE  "  ?MAIL          ?".
           05  LINE 08  COLUMN 41
               VALUE  "              ?.
           05  LINE 09  COLUMN 01
               VALUE  "  ?              ?.
           05  LINE 09  COLUMN 41
               VALUE  "              ?.
           05  LINE 10  COLUMN 01
               VALUE  "  ?ELEFONE       ? (  )".
           05  LINE 10  COLUMN 41
               VALUE  "              ?.
           05  LINE 11  COLUMN 01
               VALUE  "  ?              ?.
           05  LINE 11  COLUMN 41
               VALUE  "              ?.
           05  LINE 12  COLUMN 01
               VALUE  "  ?ATA NASCIMENTO?".
           05  LINE 12  COLUMN 41
               VALUE  "              ?.
           05  LINE 13  COLUMN 01
               VALUE  "  읕컴컴컴컴컴컴컴좔컴컴컴컴컴컴컴컴컴컴".
           05  LINE 13  COLUMN 41
               VALUE  "컴컴컴컴컴컴컴?.
           05  LINE 23  COLUMN 01
               VALUE  "    MENSAGEM :".
           05  TAPELIDO
               LINE 04  COLUMN 22  PIC X(13)
               USING  AMIGO-APELIDO.
           05  TNOME
               LINE 06  COLUMN 22  PIC X(32)
               USING  AMIGO-NOME.
           05  TEMAIL
               LINE 08  COLUMN 22  PIC X(32)
               USING  AMIGO-EMAIL.
           05  TDDD
               LINE 10  COLUMN 23  PIC 9(02)
               USING  AMIGO-DDD.
           05  TTELEFONE
               LINE 10  COLUMN 27  PIC Z9999.9999
               USING  AMIGO-NUMTEL.
           05  TDATANASC
               LINE 12  COLUMN 22  PIC XX/XX/XXXX
               USING  AMIGO-DATA-NASC.
       
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O AMIGOS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT AMIGOS
                      CLOSE AMIGOS
                      MOVE "*** ARQUIVO AMIGOS SENDO CRIADO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO AMIGOS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
<<<<<<< Updated upstream
                MOVE SPACES TO APELIDO NOME EMAIL.
=======
                MOVE ZEROS TO AMIGO-DDD AMIGO-NUMTEL DATANASC
                MOVE SPACES TO AMIGO-APELIDO AMIGO-NOME AMIGO-EMAIL.
>>>>>>> Stashed changes
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "CADASTRO DE AMIGOS"
                DISPLAY (03, 01) "APELIDO: "
                DISPLAY (04, 01) "NOME   : "
                DISPLAY (05, 01) "EMAIL  : ".
      
      * Le o apelido do amigo
       READAP.
<<<<<<< Updated upstream
                ACCEPT  (03, 09) APELIDO
=======
                ACCEPT  TAPELIDO
>>>>>>> Stashed changes
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE AMIGOS
                   GO TO ROT-FIM.
           
<<<<<<< Updated upstream
      * NOTE: Talvez a linha de baixo quebre             
                IF APELIDO = SPACES
=======
                IF AMIGO-APELIDO = SPACES
>>>>>>> Stashed changes
                   MOVE "*** APELIDO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO READAP.
       LER-AMIGOS01.
                MOVE 0 TO W-SEL
                READ AMIGOS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
<<<<<<< Updated upstream
                      DISPLAY (03, 9) APELIDO
                      DISPLAY (04, 9) NOME
                      DISPLAY (05, 9) EMAIL
=======
                      DISPLAY TELAAMIGO
>>>>>>> Stashed changes
                      MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO AMIGOS"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
      * "Rotinas" de leitura das variaveis
      
      * Le o nome do amigo
       READNAME.
                ACCEPT (04, 9) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READEMAIL.
      
      * Le o email do amigo
       READEMAIL.
                ACCEPT (05, 9) EMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-001.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      * Rotina de Escrita
       WRITE-REG.
                WRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
      * TODO: Ver exatamente o que esse numero magico "22" faz     
                IF ST-ERRO = "22"
                      MOVE "*** AMIGO JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE AMIGOS"
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
                   GO TO INC-001.
                IF W-ACT = 03
                   GO TO READNAME.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE AMIGOS RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-001.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGAMIGO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO AMIGOS"   TO MENS
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
