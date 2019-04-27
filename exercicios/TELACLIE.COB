
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
               VALUE  "  SITUA€AO:".
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
               USING  CEP
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
           05  TCOMP
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
           05  TSITDESC
               LINE 12  COLUMN 15  PIC X(10)
               USING  SITDESC
               HIGHLIGHT.
           05  TMENS
               LINE 19  COLUMN 25  PIC X(35)
               USING  MENS.
