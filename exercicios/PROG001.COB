       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG001
       AUTHOR. RICARDO DE LUCAS DO NASCIMENTO.
      **************************************
      * CADASTRO DE AMIGOS  * TODO: CHANGE THIS LATER
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
                    FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD AMIGOS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADAMIGO.DAT".
      * TODO: Inserir os campos de telefone (DDD) e numero
      * Deve ter no exemplo que eu peguei do professor
      * Colocar tambem os campos:
      *    nome do pai
      *    nome da mãe
       01 REGAMIGO.
            03 APELIDO    PIC X(15).
            03 NOME       PIC X(30).
            03 NOME-MAE   PIC X(30).
            03 NOME-PAI   PIC X(30).
            03 EMAIL      PIC X(30).
            03 TELEFONE.
                05 DDD     PIC 9(03).
                05 NUMERO  PIC 9(09).
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
                MOVE SPACES TO APELIDO NOME EMAIL.
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "CADASTRO DE AMIGOS"
                DISPLAY (03, 01) "APELIDO        : "
                DISPLAY (04, 01) "NOME           : "
                DISPLAY (05, 01) "NOME DA MAE    : "
                DISPLAY (06, 01) "NOME DO PAI    : "
                DISPLAY (07, 01) "EMAIL          : " 
                DISPLAY (08, 01) "TELEFONE (DDD) : "
                DISPLAY (09, 01) "  NUMERO       : ".
      * Le o apelido do amigo
       READAP.
                ACCEPT  (03, 17) APELIDO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE AMIGOS
                   GO TO ROT-FIM.
                IF APELIDO = SPACES
                   MOVE "*** APELIDO INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO READAP.

       LER-AMIGOS01.
                MOVE 0 TO W-SEL
                READ AMIGOS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (03, 17) APELIDO
                      DISPLAY (04, 17) NOME
                      DISPLAY (05, 17) NOME-MAE
                      DISPLAY (06, 17) NOME-PAI
                      DISPLAY (07, 17) EMAIL
                      DISPLAY (08, 17) DDD
                      DISPLAY (09, 17) NUMERO
                      MOVE "*** AMIGO JA CADASTRAD0 ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO AMIGOS"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE ST-ERRO   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
      
      * Le o nome do amigo
       READNAME.
                ACCEPT (04, 17) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READNOMEMAE.
      
      * Le o nome da mãe do amigo
       READNOMEMAE.
                ACCEPT (05, 17) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READNOMEPAI.
      
      * Le o nome do pai do amigo
       READNOMEPAI.
                ACCEPT (06, 17) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READEMAIL.

      * Le o email do amigo
       READEMAIL.
                ACCEPT (07, 17) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READTELDDD.
      
      * Le o DDD to telefone do amigo
       READTELDDD.
                ACCEPT (08, 17) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READTELNUM.
      
      * Le o Numero do Telefone do amigo
       READTELNUM.
                ACCEPT (09, 17) NOME
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
      * Rotina de Escrita no "banco de dados"
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
                   GO TO INC-001.
      * W-ACT = 03 -> O usuário teclou F2 
                IF W-ACT = 03
                   MOVE 02 TO W-SEL
                   PERFORM READNAME THRU READTELNUM
                   GO TO ALT-OPC.
       
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
