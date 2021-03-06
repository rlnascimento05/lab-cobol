       IDENTIFICATION DIVISION.
       PROGRAM-ID. SGB-002 SISTEMA GERENCIADOR DE BICICLETAS.
       AUTHOR. FELIPE VENANCIO DE SENA.
      **************************************
      * CADASTRO DE BIKES                  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQBIKE ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS NUMERO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS MARCA
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQBIKE
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQBIKE.DAT".
       01 REGBIKE.
                03 NUMERO            PIC 9(4).
                03 MARCA             PIC X(20).
                03 MODELO            PIC X(20).
                03 CATEGORIA         PIC X(01).
                03 ARO               PIC 9(2).
                03 COR               PIC 9(1).
                03 CORDESCRICAO      PIC X(13).
                03 VALORCOMPRA       PIC 9(6)V99.
                03 VALORLOCACAO      PIC 9(6)V99.
                03 DATACOMPRA        PIC 9(8).
                03 DATAUTILIZACAO    PIC 9(8).
                03 SITUACAO          PIC X(1).
                03 SITUACAODESCRICAO PIC X(13).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.

       01 TABCOR.
          03 T4 PIC X(55) VALUE
          "BRANCA   PRETA    AMARELA  VERDE    ".
          03 T5 PIC X(56) VALUE
          "VERMELHA AZUL     LARANJA  ROSA     ROXA     ".
       01 TABAUX REDEFINES TABCOR.
           03 TBCOR        PIC X(9) OCCURS 09 TIMES. 
       01 ALFACOR         PIC X(9).

       01 TABSITUACAO.
           03 T6 PIC X(24) VALUE
           "AATIVA      DDESATIVADA ".
           03 T7 PIC X(24) VALUE
           "MMANUTENCAO LLOCADA     ".
           03 T8 PIC X(24) VALUE
           "RROUBADA    BBAIXADA    ".
       01 TABSIT REDEFINES TABSITUACAO.
           03 TSIT        PIC X(12) OCCURS 6 TIMES.
       01 ALFASIT.
             03 ALFASIT1 PIC X(01).
             03 ALFASIT2 PIC X(11).   
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELABIKE.
           05  LINE 02  COLUMN 01 
               VALUE  "  CADASTRO DE BICICLETA".
           05  LINE 04  COLUMN 01 
               VALUE  "  NUMERO:".
           05  LINE 05  COLUMN 01 
               VALUE  "  MARCA:".
           05  LINE 06  COLUMN 01 
               VALUE  "  MODELO:".
           05  LINE 07  COLUMN 01 
               VALUE  "  CATEGORIA:".
           05  LINE 08  COLUMN 01 
               VALUE  "  ARO:".
           05  LINE 09  COLUMN 01 
               VALUE  "  COR:".
           05  LINE 10  COLUMN 01 
               VALUE  "  VALOR COMPRA:".
           05  LINE 11  COLUMN 01 
               VALUE  "  VALOR LOCACAO:".
           05  LINE 12  COLUMN 01 
               VALUE  "  DATA DA COMPRA:".
           05  LINE 13  COLUMN 01 
               VALUE  "  DATA ULTIMA UTILIZACAO:".
           05  LINE 14  COLUMN 01 
               VALUE  "  SITUACAO:".
           05  TNUMERO
               LINE 04  COLUMN 11  PIC 9(04)
               USING  NUMERO
               AUTO          HIGHLIGHT.
           05  TMARCA
               LINE 05  COLUMN 10  PIC X(20)
               USING  MARCA
               AUTO          HIGHLIGHT.
           05  TMODELO
               LINE 06  COLUMN 11  PIC X(20)
               USING  MODELO
               AUTO          HIGHLIGHT.
           05  TCATEGORIA
               LINE 07  COLUMN 14  PIC X(01)
               USING  CATEGORIA
               AUTO          HIGHLIGHT.
           05  TARO
               LINE 08  COLUMN 08  PIC 9(02)
               USING  ARO
               AUTO          HIGHLIGHT.
           05  TCOR
               LINE 09  COLUMN 08  PIC 9(01)
               USING  COR
               AUTO          HIGHLIGHT.
           05  TCORDESCRICAO
               LINE 09  COLUMN 10  PIC X(13)
               USING  CORDESCRICAO.
           05  TVALORCOMPRA
               LINE 10  COLUMN 17  PIC ZZZZZ9,99
               USING  VALORCOMPRA
               AUTO          HIGHLIGHT.
           05  TVALORLOCACAO
               LINE 11  COLUMN 18  PIC ZZZZZ9,99
               USING  VALORLOCACAO
               AUTO          HIGHLIGHT.
           05  TDATACOMPRA
               LINE 12  COLUMN 19  PIC XX/XX/XXXX
               USING  DATACOMPRA
               AUTO          HIGHLIGHT.
           05  TDATAUTILIZACAO
               LINE 13  COLUMN 27  PIC XX/XX/XXXX
               USING  DATAUTILIZACAO
               AUTO          HIGHLIGHT.
           05  TSITUACAO
               LINE 14  COLUMN 13  PIC X(01)
               USING  SITUACAO
               AUTO          HIGHLIGHT.
           05  TSITUACAODESCRICAO
               LINE 14  COLUMN 15  PIC X(13)
               USING  SITUACAODESCRICAO.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP0.
           OPEN I-O ARQBIKE
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT ARQBIKE
                      CLOSE ARQBIKE
                      MOVE "* ARQUIVO BIKE SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO BIKE" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE SPACES TO MARCA MODELO CATEGORIA SITUACAO.
                MOVE SPACES TO CORDESCRICAO SITUACAODESCRICAO.
                MOVE ZEROS  TO NUMERO ARO COR VALORCOMPRA VALORLOCACAO.
                MOVE ZEROS  TO DATACOMPRA DATAUTILIZACAO.
                DISPLAY TELABIKE.
                GO TO INC-002.
       INC-002.
                ACCEPT TNUMERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE ARQBIKE
                   GO TO ROT-FIM.
                IF NUMERO = ZEROS
                   MOVE "*** NUMERO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       READ-REG.
                MOVE 0 TO W-SEL
                READ ARQBIKE
                IF ST-ERRO NOT = "23"
                    IF ST-ERRO = "00"
                        DISPLAY TELABIKE
                        MOVE "BIKE EXISTENTE" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        MOVE 1 TO W-SEL
                        GO TO CRUD-001
                    ELSE
                        MOVE "ERRO NA LEITURA DO REGISTRO" TO MENS
                        PERFORM ROT-MENS THRU ROT-MENS-FIM
                        GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-003.
                ACCEPT TMARCA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF MARCA = SPACES 
                   MOVE "MARCA DEVE SER DIFERENTE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       INC-004.
                ACCEPT TMODELO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF MODELO = SPACES 
                   MOVE "MODELO DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
                ACCEPT TCATEGORIA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF CATEGORIA = SPACES 
                   MOVE "CATEGORIA DEVE SER DIF DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-005.
       INC-006.
                ACCEPT TARO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
                IF ARO = ZEROS 
                   MOVE "ARO DEVE SER DIFERENTE DE ZEROS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-006.
       INC-007.
                ACCEPT TCOR
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
                IF COR = 0 OR COR > 9
                    MOVE "COR INVÁLIDA" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-006.
                MOVE TBCOR(COR) TO ALFACOR
                DISPLAY CORDESCRICAO ALFACOR.
       INC-008.
                ACCEPT TVALORCOMPRA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
                IF VALORCOMPRA = ZEROS 
                   MOVE "VL DE COMPRA DEVE SER DIF DE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-008.
       INC-009.
                ACCEPT TVALORLOCACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
                IF VALORLOCACAO = ZEROS 
                   MOVE "VL DE LOCACAO DEVE SER DIF DE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-009.
       INC-010.
                ACCEPT TDATACOMPRA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
                IF DATACOMPRA = ZEROS 
                   MOVE "DT DE COMPRA DEVE SER DIF DE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-010.
       INC-011.
                ACCEPT TDATAUTILIZACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
                IF DATAUTILIZACAO = ZEROS 
                   MOVE "DT DE UTIL. DEVE SER DIF DE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-011.
       INC-012.
                ACCEPT TSITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                IF SITUACAO = SPACES 
                   MOVE "SITUACAO DEVE SER DIFERENTE DE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-012.
                MOVE 0 TO IND.
       INC-012A.
                MOVE TSIT(IND) TO ALFASIT
                IF ALFASIT1 NOT = SITUACAO
                   ADD 1 TO IND
                   IF IND > 6
                       MOVE "*** SITUACAO INVALIDA ***" TO MENS
                       PERFORM ROT-MENS THRU ROT-MENS-FIM
                       GO TO INC-012
                   ELSE
                       GO TO INC-012A
                ELSE
                   MOVE ALFASIT2 TO SITUACAODESCRICAO
                   DISPLAY TSITUACAODESCRICAO.
       INC-OPC.
                MOVE "S" TO W-OPCAO.
                DISPLAY(23, 40) "DADOS OK? (S / N):".
                ACCEPT(23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF W-OPCAO = "N" OR "n"
                    MOVE "GRAVACAO CANCELADA PELO USUARIO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-002.
                IF W-OPCAO NOT = "S" AND "s"
                    MOVE "OPCAO INVALIDA. DIGITE S OU N" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-OPC.
                IF W-SEL = 03
                    GO TO ALT-OPC.
       INC-GRAVACAO.
                WRITE REGBIKE
                IF ST-ERRO = "00" OR "02"
                    MOVE "REGISTRO GRAVADO COM SUCESSO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                IF ST-ERRO = "22"
                    MOVE "REGISTRO DE RA EXISTENTE" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001
                ELSE
                    MOVE "ERRO AO GRAVAR O REGISTRO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ROT-FIM.
       CRUD-001.
                DISPLAY(25, 10)
                 "F1 - NOVO REGISTRO   F2 - ALTERAR   F3 - EXCLUIR"
                ACCEPT (25, 58) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                    GO TO CRUD-001.
                MOVE SPACES TO MENS
                DISPLAY (25, 10) MENS
                IF W-ACT = 02
                    MOVE 02 TO W-SEL
                    GO TO INC-001.
                IF W-ACT = 03
                    MOVE 03 TO W-SEL
                    GO TO INC-003.
       EXC-OPC.
                DISPLAY(25, 10) "EXCLUIR? (S / N)".
                ACCEPT (25, 26) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                    MOVE "EXCLUSAO CANCELADA PELO USUARIO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                    MOVE "OPCAO INVALIDA. DIGITE S OU N" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO EXC-OPC.
       EXC-PROC.
                DELETE ARQBIKE RECORD.
                IF ST-ERRO = "00"
                    MOVE "REGISTRO EXCLUIDO COM SUCESSO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       ALT-OPC.
                DISPLAY(25, 10) "ALTERAR? (S / N)".
                ACCEPT(25, 26) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                    MOVE "ALTERACAO CANCELADA PELO USUARIO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                    MOVE "OPCAO INVALIDA. DIGITE S OU N" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO ALT-OPC.
       ALT-PROC.
                REWRITE REGBIKE.
                IF ST-ERRO = "00" OR "02"
                    MOVE "REGISTRO ALTERADO COM SUCESSO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                    GO TO INC-001.
                MOVE "ERRO NA ALTERAÇÃO DO REGISTRO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
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
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
