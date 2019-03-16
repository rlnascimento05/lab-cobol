
       01  TELANOTAS.
           05  LINE 02  COLUMN 01 
               VALUE  "                             CADASTRO DE".
           05  LINE 02  COLUMN 41 
               VALUE  " NOTAS".
           05  LINE 04  COLUMN 01 
               VALUE  "   RA  :".
           05  LINE 05  COLUMN 01 
               VALUE  "   NOME:".
           05  LINE 07  COLUMN 01 
               VALUE  "   NOTA 1  :".
           05  LINE 07  COLUMN 41 
               VALUE  "  FALTA 1 :".
           05  LINE 08  COLUMN 01 
               VALUE  "   NOTA 2  :".
           05  LINE 08  COLUMN 41 
               VALUE  "  FALTA 2 :".
           05  LINE 09  COLUMN 01 
               VALUE  "   MEDIA   :".
           05  LINE 09  COLUMN 41 
               VALUE  "  TOTAL   :".
           05  LINE 11  COLUMN 01 
               VALUE  "   SITUACAO:".
           05  TRA
               LINE 04  COLUMN 09  PIC X(13
               USING  RA
               HIGHLIGHT.
           05  TNOME
               LINE 05  COLUMN 09  PIC X(32
               USING  NOME
               HIGHLIGHT.
           05  TNOTA1
               LINE 07  COLUMN 14  PIC 9(02)
               USING  NOTA1
               HIGHLIGHT.
           05  TFALTA1
               LINE 07  COLUMN 53  PIC 9(02)
               USING  FALTA1
               HIGHLIGHT.
           05  TNOTA2
               LINE 08  COLUMN 14  PIC 9(02)
               USING  NOTA2
               HIGHLIGHT.
           05  TFALTA2
               LINE 08  COLUMN 53  PIC 9(02)
               USING  FALTA2
               HIGHLIGHT.
           05  TMEDIA
               LINE 09  COLUMN 14  PIC 9(02)
               USING  MEDIA
               HIGHLIGHT.
           05  TTOTALFALTA
               LINE 09  COLUMN 53  PIC 9(02)
               USING  TOTAL-FALTAS
               HIGHLIGHT.
           05  TSITUACAO
               LINE 11  COLUMN 14  PIC X(20)
               USING  SITUACAO
               HIGHLIGHT.
