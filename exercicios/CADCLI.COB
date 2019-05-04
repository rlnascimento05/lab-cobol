       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCLI
       AUTHOR. RICARDO DE LUCAS DO NASCIMENTO.
      **************************************
      * MANUTENCAO DO CADASTRO DE CLIENTES *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

      * NOTA: Por algum motivo, sempre que você tem 2 selects no mesmo
      * arquivo elas tem que começar na coluna 12 ao inves da coluna 8
      
           SELECT CLIENTES ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CPF
                    FILE STATUS  IS ST-ERRO.
           
           SELECT CEPS ASSIGN TO DISK
                    ORGANIZATION IS INDEXED
                    ACCESS MODE  IS DYNAMIC
                    RECORD KEY   IS CEP
                    FILE STATUS  IS ST-ERRO.
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       
       FD CEPS
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEPS.DAT".
      * Esse cara é de outro programa, tenho que verificar se as
      * variaveis estão compativeis.
      *
       01 REGCEPS.
                03 CEP                  PIC 9(8).
                03 LOGRADOURO           PIC X(35).
                03 BAIRRO               PIC X(20).
                03 CIDADE               PIC X(20).
                03 UF                   PIC X(02).
      *         03 LATITUDE             PIC X(20).
      *         03 LONGITUDE            PIC X(20).
      * NOTA: Eu comentei as variaveis acima pq eu não sei se
      * eu vou precisar delas ou não
      *

       FD CLIENTES
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCLIENTES.DAT".
       
       01 REGCLIENTES.
                03 CPF                  PIC 9(11).
                03 NOME                 PIC X(35).
                03 DATANASC.             
                    05 DIA              PIC 9(2).
                    05 MES              PIC 9(2).
                    05 ANO              PIC 9(4).
                03 RG                   PIC 9(20).
                03 CLICEP               PIC 9(08).
                03 ENDNUM               PIC 9(4).
                03 COMPLEMENTO          PIC X(12).
                03 EMAIL                PIC X(35).
                03 TELEFONE.             
                   05 DDD               PIC 9(02).
                   05 TELNUM            PIC 9(9).       
                03 SITUACAO             PIC X(1).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
      * NOTE: Eu acho que isso é pra guardar as variaveis que eu 
      * preciso pra manipular as coisas
       01 MASC1        PIC ZZZ.ZZ9,99.
       01 MASC2        PIC ZZZZ.ZZZ.ZZ9,99.
       77 W-SEL        PIC 9(01) VALUE ZEROS.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 TOPTION      PIC X(01) VALUE SPACES.
       77 ST-ERRO      PIC X(02) VALUE "00".
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES.

      *-----------------------------------------------------------------

       SCREEN SECTION     
       
       01  TELACLIENTE.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                            CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  "CLIENTE".
           05  LINE 04  COLUMN 01 
               VALUE  "  CPF:                             NOME:".
           05  LINE 05  COLUMN 01 
               VALUE  "  RG :                             DATA".
           05  LINE 05  COLUMN 41 
               VALUE  "NASCIMENTO:   -  -".
           05  LINE 07  COLUMN 01 
               VALUE  "  CEP:           LOGRADOURO:".
           05  LINE 07  COLUMN 41 
               VALUE  "                           N:".
           05  LINE 08  COLUMN 01 
               VALUE  "  BAIRRO:                      CIDADE:".
           05  LINE 08  COLUMN 41 
               VALUE  "                          UF:".
           05  LINE 09  COLUMN 01 
               VALUE  "  COMPLEMENTO:".
           05  LINE 11  COLUMN 01 
               VALUE  "  EMAIL:".
           05  LINE 11  COLUMN 41 
               VALUE  "         TELEFONE: (  )".
           05  LINE 12  COLUMN 01 
               VALUE  "  SITUAAO:".
           05  TCPF
               LINE 04  COLUMN 08  PIC 999.999.999.99
               USING  CPF
               HIGHLIGHT.
           05  TNOME
               LINE 04  COLUMN 42  PIC X(35)
               USING  NOME
               HIGHLIGHT.
           05  TRG
               LINE 05  COLUMN 08  PIC 9(20)
               USING  RG
               HIGHLIGHT.
           05  TDIA
               LINE 05  COLUMN 53  PIC 9(02)
               USING  DIA
               HIGHLIGHT.
           05  TMES
               LINE 05  COLUMN 56  PIC 9(02)
               USING  MES
               HIGHLIGHT.
           05  TANO
               LINE 05  COLUMN 59  PIC 9(04)
               USING  ANO
               HIGHLIGHT.
           05  TCEP
               LINE 07  COLUMN 08  PIC 99999.999
               USING  CLICEP
               HIGHLIGHT.
           05  TLOGR
               LINE 07  COLUMN 30  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TENDNUM
               LINE 07  COLUMN 71  PIC 9(04)
               USING  ENDNUM
               HIGHLIGHT.
           05  TBAIRRO
               LINE 08  COLUMN 11  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 08  COLUMN 40  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 08  COLUMN 71  PIC X(02)
               USING  UF
               HIGHLIGHT.
           05  TCOMPL
               LINE 09  COLUMN 16  PIC X(12)
               USING  COMPLEMENTO
               HIGHLIGHT.
           05  TEMAIL
               LINE 11  COLUMN 10  PIC X(35)
               USING  EMAIL
               HIGHLIGHT.
           05  TDDD
               LINE 11  COLUMN 61  PIC 9(02)
               USING  DDD
               HIGHLIGHT.
           05  TTELNUM
               LINE 11  COLUMN 65  PIC 99999.9999
               USING  TELNUM
               HIGHLIGHT.
           05  TSITUACAO
               LINE 12  COLUMN 13  PIC X(01)
               USING  SITUACAO
               HIGHLIGHT.
      *05  TSITDESC
      *        LINE 12  COLUMN 15  PIC X(10)
      *        USING  SITDESC
      *        HIGHLIGHT.
           05  TMENS
               LINE 19  COLUMN 25  PIC X(50)
               USING  MENS.
           05  TOPTION
               LINE 19  COLUMN 62  PIC X(1)
               USING  OPTION.
       
      *-----------------------------------------------------------------
       
       PROCEDURE DIVISION.
       INICIO.

       INC-OP0.
           OPEN I-O CLIENTES
           IF ST-ERRO NOT = "00"
               IF ST-ERRO = "30"
                      OPEN OUTPUT CLIENTES
                      CLOSE CLIENTES
                      MOVE "* CRIANDO ARQUIVO CLIENTES *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-OP0
                   ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                    NEXT SENTENCE.
       CLEAR-SCREEN.
                MOVE ZEROS  TO CPF DIA MES ANO RG CLICEP ENDNUM CEP
                MOVE ZEROS  TO TELEFONE
                MOVE SPACES TO NOME COMPLEMENTO SITUACAO
                DISPLAY TELACLIENTE.

      *------------------------------------------------------------
       LER-CPF.
           ACCEPT TCPF
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02
                   CLOSE CLIENTES
                   GO TO ROT-FIM.
           
                IF CPF = SPACES
                   MOVE "*** CPF INVALIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO LER-CPF.
       
      *-------------------------------------------------------------
      * rotinas de leituras das chaves (cliente e cep)
       SHOW-DADOS-CEP.
                MOVE CLICEP TO CEP
                READ CEPS
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TLOGR 
                      DISPLAY TBAIRRO
                      DISPLAY TCIDADE
                      DISPLAY TUF
                   ELSE
                      MOVE "ERRO NA LEITURA DO CEP"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO LER-CEP
                ELSE
                   NEXT SENTENCE.
       
       
       SHOW-DADOS-CLI.
                MOVE 0 TO W-SEL
                READ CLIENTES
                IF ST-ERRO NOT = "23"
                   IF ST-ERRO = "00"
                      DISPLAY TCEP
                      DISPLAY TLOGRADOURO 
                      DISPLAY TBAIRRO
                      DISPLAY TCIDADE
                      DISPLAY TUF
                      DISPLAY TLATITUDE
                      DISPLAY TLONGITUDE

                      PERFORM SHOW-DADOS-CEP
                      MOVE "* CLIENTE JÁ CADASTRADO *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      MOVE 1 TO W-SEL
                      GO TO ALTER-REG-OP
                   ELSE:
                      MOVE "ERRO NO ARQUIVO CLIENTES"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.

      *-------------------------------------------------------------
      
      
      *-------------------------------------------------------------
      * "Rotinas" de leitura das variaveis
      * TODO: Corrigir as rotinas de leitura da variavel
      
       LER-NAME.
                ACCEPT TNAME
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-DATANASC.
       
       LER-DATANASC.
      *TODO: Ajustar a tela - Juntar os tres campos e deixar um só
                ACCEPT TDATANASC
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-RG.
       
       LER-RG.
                ACCEPT TRG
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-CEP.
       
       LER-CEP.
           ACCEPT TCEP
           IF CLICEP = SPACES
               MOVE "*** CEP INVALIDO ***" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO LER-CEP.
           ELSE
              PERFORM SHOW-DADOS-CEP
       
       LER-ENDNUM.
                ACCEPT TENDNUM
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-COMPL.
      
       LER-COMPL.
                ACCEPT TCOMPL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-EMAIL.

       LER-EMAIL.
                ACCEPT TEMAIL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-TEL.

       LER-TEL.
                ACCEPT TTEL
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO LER-SITUACAO

       LER-SITUACAO.
                ACCEPT TSITUACAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-SEL = 03 GO TO ALTER-REG.

       
      *-------------------------------------------------------------
       
       FINISH-NEW-REG.
                MOVE "S" TO TOPTION
                MOVE "DADOS OK (S/n) : " TO MENS
                DISPLAY TMENS
                ACCEPT TOPTION WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO CLEAR-SCREEN.
                IF OPTION = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                IF OPTION NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO FINISH-NEW-REG.
      * Rotina de Escrita
       WRITE-REG.
           WRITE REGCLIENTES
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
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CLIENTES"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ALTER-REG-OP.
                MOVE "F1=NOVO REGISTRO  F2=ALTERAR  F3=EXCLUIR" TO MENS
                DISPLAY TMENS
                ACCEPT TOPTION
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT NOT = 02 AND W-ACT NOT = 03 AND W-ACT NOT = 04
                   GO TO ALTER-REG-OP.
                MOVE SPACES TO MENS
                DISPLAY TMENS
                IF W-ACT = 02
                   MOVE 02 TO W-SEL
                   GO TO CLEAR-SCREEN.
                IF W-ACT = 03
                  MOVE 03 TO W-SEL      
                  PERFORM LER-NOME THRU LER-SITUACAO
                  GO TO ALTER-REG.
      *
       DELETE-REG-OP.
                MOVE "EXCLUIR   (S/N) : " TO MENS
                DISPLAY TMENS
                ACCEPT TOPTION
                IF OPTION = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                IF OPTION NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO DELETE-REG-OP.
       DELETE-REG.
                DELETE CLIENTES RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO EXCLUIDO ***           " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALTER-REG.
                MOVE "ALTERAR  (S/N) : " TO MENS
                DISPLAY TMENS
                ACCEPT TOPTION
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 02 GO TO CLEAR-SCREEN.
                IF OPTION = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                IF OPTION NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALTER-REG.
       REWRITE-REG.
           REWRITE REGCLIENTES
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO CLEAR-SCREEN.
                MOVE "ERRO NA REESCRITA DO REGISTRO CLIENTES"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
                DISPLAY (01, 01) ECLIENTESE
                EXIT PROGCEPM.
       ROT-FIMP.
                EXIT PROGCEPM.

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
                   GO TO ROT-MENS2
       ROT-MENS-FIM.
                EXIT.
       ROT-ALFA-FIM.
