
       01  TELAAMIGO.
           05  LINE 02  COLUMN 01 
               VALUE  "                      CADASTRO DE AMIGOS".
           05  LINE 03  COLUMN 01 
               VALUE  "  ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 03  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿".
           05  LINE 04  COLUMN 01 
               VALUE  "  ³APELIDO        ³:".
           05  LINE 04  COLUMN 41 
               VALUE  "              ³".
           05  LINE 05  COLUMN 01 
               VALUE  "  ³               ³".
           05  LINE 05  COLUMN 41 
               VALUE  "              ³".
           05  LINE 06  COLUMN 01 
               VALUE  "  ³NOME           ³:".
           05  LINE 06  COLUMN 41 
               VALUE  "              ³".
           05  LINE 07  COLUMN 01 
               VALUE  "  ³               ³".
           05  LINE 07  COLUMN 41 
               VALUE  "              ³".
           05  LINE 08  COLUMN 01 
               VALUE  "  ³EMAIL          ³:".
           05  LINE 08  COLUMN 41 
               VALUE  "              ³".
           05  LINE 09  COLUMN 01 
               VALUE  "  ³               ³".
           05  LINE 09  COLUMN 41 
               VALUE  "              ³".
           05  LINE 10  COLUMN 01 
               VALUE  "  ³TELEFONE       ³: (  )".
           05  LINE 10  COLUMN 41 
               VALUE  "              ³".
           05  LINE 11  COLUMN 01 
               VALUE  "  ³               ³".
           05  LINE 11  COLUMN 41 
               VALUE  "              ³".
           05  LINE 12  COLUMN 01 
               VALUE  "  ³DATA NASCIMENTO³:".
           05  LINE 12  COLUMN 41 
               VALUE  "              ³".
           05  LINE 13  COLUMN 01 
               VALUE  "  ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ".
           05  LINE 13  COLUMN 41 
               VALUE  "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ".
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
               USING  TDDD.
           05  TTELEFONE
               LINE 10  COLUMN 27  PIC Z9999.9999
               USING  AMIGO-TELEFONE.
           05  TDATANASC
               LINE 12  COLUMN 22  PIC XX/XX/XXXX
               USING  AMIGO-DATA-NASC.
