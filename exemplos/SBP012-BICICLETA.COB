       IDENTIFICATION DIVISION.
       PROGRAM-ID. SBP012.
      *******************************
      * CONSULTA DE BICICLETA       *
      *******************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
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
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 W-CONT       PIC 9(04) VALUE ZEROS.
       01 W-OPCAO      PIC X(01) VALUE SPACES.
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-ACT        PIC 9(02) VALUE ZEROS.
       01 MENS         PIC X(50) VALUE SPACES.
       01 LIMPA        PIC X(55) VALUE SPACES.
       01 SOLIC        PIC X(20) VALUE SPACES.
       01 CONLIN       PIC 9(03) VALUE 001.
       01 W-NUMERO     PIC 9(06) VALUE ZEROS.
       01 W-MARCA     PIC X(35) VALUE SPACES.
       01 OCW03803     PIC X(12) VALUE SPACES.
       01 OCW03810     PIC 9(15) VALUE ZEROS.
       01 W-EMPRESA    PIC X(20) VALUE "FATEC ZL".
      *-----------------------------------------------------------------
      *              *** CAMPOS PARA TRATAMENTO DE DATA ***
       01 DATA-EQ.
          03 ANO-EQ    PIC 99.
          03 MES-EQ    PIC 99.
          03 DIA-EQ    PIC 99.
       01 DATA-N.
          03 DIA       PIC 99.
          03 MES       PIC 99.
          03 ANO       PIC 99.
       01 DATA-INV.
          03 ANO-INV   PIC 99.
          03 MES-INV   PIC 99.
          03 DIA-INV   PIC 99.
       01 DATA-FOR.
          03 DIA-FOR   PIC 99.
          03 BR1-FOR   PIC X(01) VALUE "/".
          03 MES-FOR   PIC 99.
          03 BR2-FOR   PIC X(01) VALUE "/".
          03 ANO-FOR   PIC 99.
       01 W-01         PIC 9(04).
       01 FLAG1        PIC 9(01).
      *-----------------------------------------------------------------
      *
      ******************
      * REPORT SECTION *
      ******************
      *
       01 DETAILS.
          03 FILLER    PIC X(16) VALUE SPACES.
          03 DETNUMERO  PIC 9(04) VALUE ZEROS.
          03 FILLER    PIC X(04) VALUE SPACES.
          03 DETMARCA  PIC X(20) VALUE SPACES.
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELABIKE.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01
               VALUE  "浜様様様様様様様様様様様様様様様様様様様".
           05  LINE 01  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様様様様様融".
           05  LINE 02  COLUMN 01
               VALUE  "�                      *** CONSULTA DE B".
           05  LINE 02  COLUMN 41
               VALUE  "ICICLETAS ***                          �".
           05  LINE 03  COLUMN 01
               VALUE  "把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳".
           05  LINE 03  COLUMN 41
               VALUE  "陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超".
           05  LINE 04  COLUMN 01
               VALUE  "�              NUMERO                 MA".
           05  LINE 04  COLUMN 41
               VALUE  "RCA                                    �".
           05  LINE 05  COLUMN 01
               VALUE  "�              ------   ----------------".
           05  LINE 05  COLUMN 41
               VALUE  "------------------------               �".
           05  LINE 06  COLUMN 01
               VALUE  "�".
           05  LINE 06  COLUMN 41
               VALUE  "                                       �".
           05  LINE 07  COLUMN 01
               VALUE  "�".
           05  LINE 07  COLUMN 41
               VALUE  "                                       �".
           05  LINE 08  COLUMN 01
               VALUE  "�".
           05  LINE 08  COLUMN 41
               VALUE  "                                       �".
           05  LINE 09  COLUMN 01
               VALUE  "�".
           05  LINE 09  COLUMN 41
               VALUE  "                                       �".
           05  LINE 10  COLUMN 01
               VALUE  "�".
           05  LINE 10  COLUMN 41
               VALUE  "                                       �".
           05  LINE 11  COLUMN 01
               VALUE  "�".
           05  LINE 11  COLUMN 41
               VALUE  "                                       �".
           05  LINE 12  COLUMN 01
               VALUE  "�".
           05  LINE 12  COLUMN 41
               VALUE  "                                       �".
           05  LINE 13  COLUMN 01
               VALUE  "�".
           05  LINE 13  COLUMN 41
               VALUE  "                                       �".
           05  LINE 14  COLUMN 01
               VALUE  "�".
           05  LINE 14  COLUMN 41
               VALUE  "                                       �".
           05  LINE 15  COLUMN 01
               VALUE  "�".
           05  LINE 15  COLUMN 41
               VALUE  "                                       �".
           05  LINE 16  COLUMN 01
               VALUE  "�".
           05  LINE 16  COLUMN 41
               VALUE  "                                       �".
           05  LINE 17  COLUMN 01
               VALUE  "�".
           05  LINE 17  COLUMN 41
               VALUE  "                                       �".
           05  LINE 18  COLUMN 01
               VALUE  "�".
           05  LINE 18  COLUMN 41
               VALUE  "                                       �".
           05  LINE 19  COLUMN 01
               VALUE  "�".
           05  LINE 19  COLUMN 41
               VALUE  "                                       �".
           05  LINE 20  COLUMN 01
               VALUE  "�".
           05  LINE 20  COLUMN 41
               VALUE  "                                       �".
           05  LINE 21  COLUMN 01
               VALUE  "�".
           05  LINE 21  COLUMN 41
               VALUE  "                                       �".
           05  LINE 22  COLUMN 01
               VALUE  "把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳".
           05  LINE 22  COLUMN 41
               VALUE  "陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳堕陳陳陳超".
           05  LINE 23  COLUMN 01
               VALUE  "�MENSAGEM :".
           05  LINE 23  COLUMN 41
               VALUE  "                              � OCP038 �".
           05  LINE 24  COLUMN 01
               VALUE  "藩様様様様様様様様様様様様様様様様様様様".
           05  LINE 24  COLUMN 41
               VALUE  "様様様様様様様様様様様様様様様詫様様様夕".
           05  T-EMPRESA
               LINE 02  COLUMN 02  PIC X(20)
               USING  W-EMPRESA
               HIGHLIGHT.
           05  DATELA
               LINE 02  COLUMN 69  PIC XX/XX/XX
               USING  DATA-EQ
               HIGHLIGHT.
           05  T-NUMERO
               LINE 06  COLUMN 18  PIC 9(04)
               USING NUMERO.
           05  T-MARCA
               LINE 06  COLUMN 25  PIC X(20)
               USING MARCA.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           ACCEPT DATA-EQ FROM DATE.
      *
       ROT-INC.
           MOVE   DATA-EQ TO DATA-INV.
           PERFORM INVNOR.
           MOVE DATA-N TO DATA-EQ.
       INC-OP1.
           OPEN I-O   ARQBIKE
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE BIKE NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                 MOVE "** ERRO NA ABERTURA DO ARQBIKE **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
           ELSE
               NEXT SENTENCE.
      *
       SHOW-TELABIKE.
           DISPLAY TELABIKE.
       BIKE-OP-1.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/MARCA."
           ACCEPT T-NUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQBIKE
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO BIKE-OP-2.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO BIKE-OP-1.
      *  
           START ARQBIKE KEY IS NOT LESS NUMERO INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO SHOW-TELABIKE.
           GO TO INC-RD2.
       BIKE-OP-2.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA     F2=CONSULTA P/CODIGO "
           ACCEPT TELABIKE02
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQBIKE
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO BIKE-OP-1.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO BIKE-OP-1.
           START ARQBIKE KEY IS NOT LESS MARCA INVALID KEY
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO BIKE-OP-2.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQBIKE NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DE PROCESSAMENTO ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE NUMERO TO DETNUMERO
           MOVE MARCA TO DETMARCA
      * esse '5' � a quantidade de linhas no cabe�alho da tela
      * TODO: Mudar esse numero 5 para uma variavel global no
      * workstorage section
           COMPUTE LIN = CONLIN + 5
           DISPLAY (LIN, 02) DETAILS
           ADD 1 TO CONLIN
      * esse 17 s�o quantas linhas de detalhes eu consigo mostrar na 
      * minha tela, quantas linhas tem em cada pagina艫o.
      * TODO: Tambem deveria virar uma variavel global
           IF CONLIN < 17
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELABIKE
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO SHOW-TELABIKE.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  MOVE ZEROS  TO NUMERO
                  MOVE SPACES TO MARCA
                  GO TO SHOW-TELABIKE.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQBIKE.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *
      ************************************
      *      *** ROTINA DE DATA ***      *
      ************************************
      *
      *
      *
       FORNOR.
           MOVE DIA-FOR TO DIA
           MOVE MES-FOR TO MES
           MOVE ANO-FOR TO ANO.
      *
      * FUNCAO : CONVERTE DATA FORMATADA EM NORMAL
      *
       NORINV.
           MOVE DIA     TO DIA-INV DIA-FOR
           MOVE MES     TO MES-INV MES-FOR
           MOVE ANO     TO ANO-INV ANO-FOR.
      *
      * FUNCAO : CONVERTE DATA NORMAL P/INVERTIDA E FORMATADA
      *
       INVNOR.
           MOVE DIA-INV TO DIA DIA-FOR
           MOVE MES-INV TO MES MES-FOR
           MOVE ANO-INV TO ANO ANO-FOR.
      *
      * FUNCAO : CONVERTE DATA INVERTIDA PARA NORMA
      *
       VALDATA.
           MOVE 0 TO FLAG1.
           GO TO D31 D28 D31 D30 D31 D30 D31 D31 D30 D31 D30 D31
                                          DEPENDING ON MES
           MOVE 1 TO FLAG1
           GO TO VALDATA-FIM.
       D31.
           IF DIA > 31
              MOVE 2 TO FLAG1
              GO TO VALDATA-FIM.
       D30.
           IF DIA > 30
              MOVE 2 TO FLAG1
              GO TO VALDATA-FIM.
       D28.
           COMPUTE W-01 = ANO / 4
           COMPUTE W-01 = W-01 * 4
           COMPUTE W-01 = ANO - W-01
           IF W-01 = ZEROS
                IF DIA > 29
                  MOVE 2 TO FLAG1
                  GO TO VALDATA-FIM
                ELSE
                  IF DIA > 28
                    MOVE 2 TO FLAG1
                    GO TO VALDATA-FIM.
       VALDATA-FIM.
      *
      *---------------------*** FIM DE PROGRAMA ***--------------------*
