       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG001.
       AUTHOR. JÉSSICA SILVA CARMO.
      **************************************
      * MANUTENCAO DO CADASTRO DE AMIGOS   *
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
                    RECORD KEY   IS APELIDO
                    FILE STATUS  IS ST-ERRO
                    ALTERNATE RECORD KEY IS NOME
                                   WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD AMIGOS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADAMIGO.DAT".
       01 REGAMI.
                03 APELIDO    PIC X(15).
                03 NOME       PIC X(30).
                03 LOGRADOURO PIC X(35).
                03 BAIRRO     PIC X(20).
                03 CIDADE     PIC X(20).
                03 UF         PIC X(02). 
                03 EMAIL      PIC X(30).
                03 TELEFONE.
                   05 DDD     PIC 9(03).
                   05 NUMERO  PIC 9(09).
                03 DATANASC.
                   05 DIANASC PIC 9(02).
                   05 MESNASC PIC 9(02).
                   05 ANONASC PIC 9(04).
                03 SEXO       PIC X(01).
                03 GENERO     PIC X(01).
                03 TIPOAMIGO  PIC 9(01).
                   
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00". 
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.
       01 TEXTOSEXO    PIC X(10) VALUE SPACES.
       01 TEXTOGENERO  PIC X(15) VALUE SPACES.
       01 TEXTOTAMIGO  PIC X(15) VALUE SPACES.
       01 TABAMI.
          03 TABAMI1   PIC X(15) VALUE "AAAAAAAAAAAAAAA".
          03 TABAMI2   PIC X(15) VALUE "BBBBBBAAAAAAAAA".
          03 TABAMI3   PIC X(15) VALUE "CCCCCCAAAAAAAAA".
          03 TABAMI4   PIC X(15) VALUE "DDDDDDAAAAAAAAA".
          03 TABAMI5   PIC X(15) VALUE "EEEEEEAAAAAAAAA".
          03 TABAMI6   PIC X(15) VALUE "FFFFFFAAAAAAAAA".
          03 TABAMI7   PIC X(15) VALUE "GGGGGGAAAAAAAAA".
          03 TABAMI8   PIC X(15) VALUE "HHHHHHAAAAAAAAA".
          03 TABAMI9   PIC X(15) VALUE "IIIIIIAAAAAAAAA".
       01 TABAMIGO REDEFINES TABAMI.
          03 TA        PIC X(15) OCCURS 9 TIMES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 01  COLUMN 01 
               VALUE  "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 01  COLUMN 41 
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»".
           05  LINE 02  COLUMN 01 
               VALUE  "º                         *** CADASTRO D".
           05  LINE 02  COLUMN 41 
               VALUE  "E AMIGOS ***                           º".
           05  LINE 03  COLUMN 01 
               VALUE  "ºÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄº".
           05  LINE 04  COLUMN 01 
               VALUE  "ºÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 04  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 05  COLUMN 01 
               VALUE  "º³ APELIDO                     ³  NOME".
           05  LINE 05  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 06  COLUMN 01 
               VALUE  "ºÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
           05  LINE 06  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 07  COLUMN 01 
               VALUE  "º  LOGRADOURO".
           05  LINE 07  COLUMN 41 
               VALUE  "          BAIRRO                       º".
           05  LINE 08  COLUMN 01 
               VALUE  "º".
           05  LINE 08  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 09  COLUMN 01 
               VALUE  "º  CIDADE                      UF     EM".
           05  LINE 09  COLUMN 41 
               VALUE  "AIL                                    º".
           05  LINE 10  COLUMN 01 
               VALUE  "º".
           05  LINE 10  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 11  COLUMN 01 
               VALUE  "º  TELEFONE (   )                     DA".
           05  LINE 11  COLUMN 41 
               VALUE  "TA NASCIMENTO                          º".
           05  LINE 12  COLUMN 01 
               VALUE  "º".
           05  LINE 12  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 13  COLUMN 01 
               VALUE  "º".
           05  LINE 13  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 14  COLUMN 01 
               VALUE  "º".
           05  LINE 14  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 15  COLUMN 01 
               VALUE  "º  SEXO                 GENERO".
           05  LINE 15  COLUMN 41 
               VALUE  "         TIPO AMIGO                    º".
           05  LINE 16  COLUMN 01 
               VALUE  "º".
           05  LINE 16  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 17  COLUMN 01 
               VALUE  "º".
           05  LINE 17  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 18  COLUMN 01 
               VALUE  "º".
           05  LINE 18  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 19  COLUMN 01 
               VALUE  "º".
           05  LINE 19  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 20  COLUMN 01 
               VALUE  "º".
           05  LINE 20  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 21  COLUMN 01 
               VALUE  "º".
           05  LINE 21  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 22  COLUMN 01 
               VALUE  "º".
           05  LINE 22  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 23  COLUMN 01 
               VALUE  "ºMENSAGEM :".
           05  LINE 23  COLUMN 41 
               VALUE  "                                       º".
           05  LINE 24  COLUMN 01 
               VALUE  "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ".
           05  LINE 24  COLUMN 41 
               VALUE  "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼".
           05  TAPELIDO
               LINE 05  COLUMN 12  PIC X(15)
               USING  APELIDO
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 40  PIC X(30)
               USING  NOME
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 07  COLUMN 15  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 07  COLUMN 58  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 09  COLUMN 11  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 09  COLUMN 35  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TEMAIL
               LINE 09  COLUMN 45  PIC X(35)
               USING  EMAIL
               HIGHLIGHT.
           05  TDDD
               LINE 11  COLUMN 14  PIC 999
               USING  DDD
               HIGHLIGHT.
           05  TNUMERO
               LINE 11  COLUMN 19  PIC Z9999.9999
               USING  NUMERO
               HIGHLIGHT.
           05  TDATANASC
               LINE 11  COLUMN 55  PIC XX/XX/XXXX
               USING  DATANASC
               HIGHLIGHT.
           05  TSEXO
               LINE 15  COLUMN 09  PIC X(01)
               USING  SEXO
               HIGHLIGHT.
           05  TEXSEXO
               LINE 15  COLUMN 11  PIC X(10)
               USING  TEXTOSEXO
               HIGHLIGHT.
           05  TGENERO
               LINE 15  COLUMN 32  PIC X(01)
               USING  GENERO
               HIGHLIGHT.
           05  TEXGENERO
               LINE 15  COLUMN 34  PIC X(15)
               USING  TEXTOGENERO
               HIGHLIGHT.
           05  TAMIGO
               LINE 15  COLUMN 61  PIC 9(01)
               USING  TIPOAMIGO
               HIGHLIGHT.
           05  TEXTAMIGO
               LINE 15  COLUMN 63  PIC X(15)
               USING  TEXTOTAMIGO
               HIGHLIGHT.
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
                      MOVE "* ARQUIVO AMIGOS SENDO CRIADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO PRODTUO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE SPACES TO APELIDO NOME EMAIL LOGRADOURO
                               BAIRRO CIDADE UF SEXO GENERO
                               TEXTOSEXO TEXTOGENERO TEXTOTAMIGO
                MOVE ZEROS  TO DDD NUMERO TIPOAMIGO.

                DISPLAY TELA2.
        INC-002.
                ACCEPT TAPELIDO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE AMIGOS
                   GO TO ROT-FIM.
                IF APELIDO = SPACES 
                   MOVE "*** APELIDO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-002.
       LER-PRODUTO01.
                MOVE 0 TO W-SEL
                READ AMIGOS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      PERFORM INC-012A
                      PERFORM INC-014A
                      DISPLAY TELA2
                      MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQ. PRODUTO"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       INC-003.
                ACCEPT TNOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-002.
                IF NOME = SPACES 
                   MOVE "NOME DEVE SER DIFERENTE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-003.
       INC-004.
                ACCEPT TLOGRADOURO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-003.
                IF LOGRADOURO = SPACES 
                   MOVE "LOGRADOURO DEVE SER DIFERENTE BRANCOS" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-004.
       INC-005.
                ACCEPT TBAIRRO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
       INC-006.
                ACCEPT TCIDADE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-005.
       INC-007.
                ACCEPT TUF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-006.
       INC-008.
                ACCEPT TEMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-007.
       INC-009.
                ACCEPT TDDD
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-008.
       INC-010.
                ACCEPT TNUMERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-009.
       INC-011.
                ACCEPT TDATANASC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-010.
       INC-012.
                ACCEPT TSEXO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-011.
                IF SEXO NOT = "M" AND "F"
                   MOVE "SEXO VALIDO = M OU F" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-012.
       INC-012A.
                IF SEXO = "M"
                      MOVE "MASCULINO" TO TEXTOSEXO
                   ELSE
                      MOVE "FEMENINO"  TO TEXTOSEXO.
                DISPLAY TELA2.
       INC-013.
                ACCEPT TGENERO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-012.
       INC-014.
                ACCEPT TAMIGO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-013.
                IF TIPOAMIGO = 0 OR >4
                   MOVE "TIPO DE AMIGO INVALIDO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-014.
        INC-014A.

                MOVE TA(TIPOAMIGO) TO TEXTOTAMIGO

      *         IF TIPOAMIGO = 1
      *             MOVE "FACULDADE" TO TEXTOTAMIGO
      *          ELSE
      *             IF TIPOAMIGO = 2
      *               MOVE "IGREJA"    TO TEXTOTAMIGO
      *             ELSE
      *               IF TIPOAMIGO = 3 
      *                 MOVE "BALADA" TO TEXTOTAMIGO
      *               ELSE
      *                 MOVE "NAMORADO DA EX" TO TEXTOTAMIGO.
                DISPLAY TELA2.


      *
                IF W-SEL = 1
                              GO TO ALT-OPC.

       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO OPERADOR *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGAMI
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
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
                   GO TO INC-003.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE AMIGOS RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***        " TO MENS
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
                IF W-ACT = 02 GO TO INC-004.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGAMI
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA ALTERACAO DO REGISTRO AMIGOS"   TO MENS
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
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
