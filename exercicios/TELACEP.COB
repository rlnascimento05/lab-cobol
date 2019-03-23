
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
