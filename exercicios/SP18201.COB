       IDENTIFICATION DIVISION.
       PROGRAM-ID. SP18201.COB
       AUTHOR. RICARDO DE LUCAS DO NASCIMENTO.
      **************************************
      * MANUTENCAO DO CADASTRO DE DEPARTAMENTOS    *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT DEPARTAMENTOS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS COD
                    FILE STATUS  IS ST-ERRO
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD DEPARTAMENTOS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CAD-DEPT.DAT".
       01 REGDEPT.
                03 CODIGO               PIC 9(3).
                03 DENOMINACAO          PIC X(25).
                03 NIVEL                PIC 9(1).
                03 SITUACAO             PIC 9(1).
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
       77 NIVEL-DESC   PIC X(25) VALUE SPACES.
      * TODO: Colocar aqui a tabela de relção de niveis com as
      * descrições
      *
      *-----------------------------------------------------------------
      *
       INC-OP0.
           OPEN I-O DEPARTAMENTOS
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT DEPARTAMENTOS
                      CLOSE DEPARTAMENTOS
                      MOVE "*** INICIALIZANDO... ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                      MOVE "FALHA NO ARQUIVO DE DADOS!" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       INC-001.
                MOVE ZEROS TO CODIGO 
                MOVE ZEROS TO FALTA1 FALTA2 TOTAL-FALTAS
                MOVE SPACES TO RA NOME SITUACAO.
                DISPLAY (01, 01) ERASE.
                DISPLAY (01, 20) "CADASTRO DE DEPARTAMENTOS"
                DISPLAY (03, 01) "RA                    : "
                DISPLAY (04, 01) "NOME                  : "
                DISPLAY (05, 01) "NOTA1                 : "
                DISPLAY (06, 01) "NOTA2                 : "
                DISPLAY (07, 01) "MEDIA                 : "
                DISPLAY (08, 01) "FALTA1                : "
                DISPLAY (09, 01) "FALTA2                : "
                DISPLAY (10, 01) "TOTAL FALTAS          : "
                DISPLAY (15, 01) "SITUAÇÃO              : "
      
      * Le o RA do aluno
       READRA.
                ACCEPT  (3,24) RA
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE DEPARTAMENTOS
                   GO TO ROT-FIM.
           
                IF RA = SPACES
                   MOVE "*** RA INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO READRA.
       LER-DEPARTAMENTOS01.
      * TODO: Criar função para a regra da situação do aluno
                MOVE 0 TO W-SEL
                READ DEPARTAMENTOS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY (04, 24) NOME
                      DISPLAY (05, 24) NOTA1 
                      DISPLAY (06, 24) NOTA2
                      DISPLAY (07, 24) MEDIA
                      DISPLAY (08, 24) FALTA1
                      DISPLAY (09, 24) FALTA2
                      DISPLAY (10, 24) TOTAL-FALTAS
                      DISPLAY (15, 24) SITUACAO
                      MOVE "*** NOTA JÁ CADASTRADA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ACE-001
                   ELSE
                      MOVE "ERRO NA LEITURA ARQUIVO DEPARTAMENTOS"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
      * "Rotinas" de leitura das variaveis
      * TODO: Corrigir as rotinas de leitura da variavel
      
       READNAME.
                ACCEPT (04, 24) NOME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO READNOTA1.
      
       READNOTA1.
                ACCEPT (05, 24) NOTA1
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
                WRITE REGNOTA
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001.
      * TODO: Ver exatamente o que esse numero magico "22" faz     
                IF ST-ERRO = "22"
                      MOVE "*** NOTA JA EXISTE ***       " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPARTAMENTOS"
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
                DELETE DEPARTAMENTOS RECORD
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
                REWRITE REGNOTA
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-001.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DEPARTAMENTOS"   TO MENS
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
