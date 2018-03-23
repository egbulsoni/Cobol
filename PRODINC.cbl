       IDENTIFICATION DIVISION.
       PROGRAM-ID.    PRODUTO.

       ENVIRONMENT DIVISION.
       SPECIAL-NAMES.        DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
              SELECT PRODUTOS ASSIGN TO DISK
              ORGANIZATION SEQUENTIAL
              ACCESS MODE SEQUENTIAL
              FILE STATUS ARQST.
      
       DATA DIVISION.
       FILE SECTION.
       FD PRODUTOS LABEL RECORD STANDARD
                DATA RECORD IS REG-PROD
                VALUE OF FILE-ID IS "PRODUTOS.DAT".
          01 REG-PROD.
                02 CODIGO         PIC 9(04).
                02 NOME           PIC X(30).
                02 QTDADE         PIC 9(04).
                02 UNITARIO       PIC 9(05)V99.
                02 TOTAL          PIC 9(06)V99.

       WORKING-STORAGE SECTION.
          01 REG-PROD-E.
                02 CODIGO-E       PIC Z.ZZ9.
                02 NOME-E         PIC X(30).
                02 QTDADE-E       PIC Z.ZZ9.
                02 UNITARIO-E     PIC ZZ.ZZ9,99.
                02 TOTAL-E        PIC ZZZ.ZZ9,99.
          01 REG-PROD-W
                02 CODIGO-W         PIC 9(04).
                02 NOME-W           PIC X(30).
                02 QTDADE-W         PIC 9(04).
                02 UNITARIO-W       PIC 9(05)V99.
                02 TOTAL-W          PIC 9(06)V99.
          01 DATA-SIS.
                02 ANO            PIC 9(04).
                02 MES            PIC 9(02).
                02 DIA            PIC 9(02).

         01 ARQST                   PIC X(02).
         01 WS-OPCAO                PIC X(01) VALUE SPACES.
         01 WS-SALVA                PIC X(01) VALUE SPACES.
         01 WS-ESPACO               PIC X(30) VALUE SPACES.
         01 WS-MENS1                PIC X(20) VALUE "FIM DE PROGRAMA".
         01 WS-FL                   PIC 9(01) VALUE ZEROS.

       SCREEN SECTION.
         01 TELA.
              02 LINE 2  COL 5  VALUE "  /  /  ".
              02 COL 29  VALUE "CONTROLE DE MERCADORIAS".
              02 LINE 4  COL 19 VALUE "CODIGO DO PRODUTO:".
              02 LINE 6  COL 19 VALUE "NOME DO PRODUTO:".
              02 LINE 8  COL 19 VALUE "QUANTIDADE:".
              02 LINE 10 COL 19 VALUE "CUSTO UNITARIO:".
              02 LINE 12 COL 19 VALUE "CUSTO TOTAL:".
              02 LINE 15 COL 25 VALUE "MENSAGEM:".
 
       PROCEDURE DIVISION.
       INICIO.
              PERFORM ABRE-ARQ.
              PERFORM PROCESSO UNTIL WS-OPCAO = "N".
              PERFORM FINALIZA.

       ABRE-ARQ.
              OPEN I-O PRODUTOS.
              IF ARQST NOT = "00"
                     CLOSE PRODUTOS
                     OPEN OUTPUT PRODUTOS.

       PROCESSO.
              PERFORM IMP-TELA.
              PERFORM ENTRA-DADOS.
              PERFORM CALCULO-TOTAL.
              PERFORM GRAVAR  UNTIL WS-SALVA = "S" OR "N".
              IF WS-SALVA = "S"
                 PERFORM GRAVA-REG
              ELSE
                 DISPLAY "REGISTRO NAO GRAVADO" AT 2030.
              PERFORM CONTINUA  UNTIL WS-OPCAO = "S" OR "N".

       IMP-TELA.
              DISPLAY ERASE AT 0101.
              DISPLAY TELA.
              MOVE FUNCTION CURRENT-DATE TO DATA-SIS.
              DISPLAY DIA   AT 0205.
              DISPLAY MES   AT 0208.
              DISPLAY ANO   AT 0211.
      * ----------------------------- Inicialização das variáveis
              MOVE SPACE  TO 	WS-OPCAO 
				WS-SALVA
              			NOME-E.
              MOVE ZEROS  TO 	CODIGO-E 
				QTDADE-E
				UNITARIO-E 
                                TOTAL-E
                                WS-FL.
              DISPLAY WS-ESPACO AT 1535.

       ENTRA-DADOS.
              PERFORM ENTRA-CODIGO UNTIL WS-FL = 1.
              ACCEPT NOME-E     AT 0636 WITH PROMPT AUTO.
              ACCEPT QTDADE-E   AT 0831 WITH PROMPT AUTO.
              ACCEPT UNITARIO-E AT 1035 WITH PROMPT AUTO.
              MOVE   CODIGO-E   TO CODIGO-W.
              MOVE   NOME-E     TO NOME-W.
              MOVE   QTDADE-E   TO QTDADE-W.
              MOVE   UNITARIO-E TO UNITARIO-W.

       ENTRA-CODIGO.
              ACCEPT CODIGO-E   AT 0438 WITH PROMPT AUTO.
              MOVE   CODIGO-E   TO CODIGO-W.
              IF CODIGO-W = 9999
                 DISPLAY WS-MENS1 AT 1535
                 CLOSE PRODUTOS
                 STOP RUN.
              CLOSE PRODUTOS.
              PERFORM ABRE-ARQ.
              MOVE ZEROS TO WS-FL.
              PERFORM LER-REGISTRO UNTIL WS-FL >= 1.
              IF WS-FL = 2
                 DISPLAY "REGISTO JA CADSTRADO" AT 2030.

       LER-REGISTRO.
              READ PRODUTOS NEXT AT END MOVE 1 TO WS-FL.
              IF ARQST = "00"
                 IF CODIGO-W = CODIGO
                    MOVE 2 TO WS-FL.

       CALCULO-TOTAL.
              COMPUTE TOTAL-W = QTDADE-W * UNITARIO-W.
              MOVE    TOTAL-W TO TOTAL-E.
              DISPLAY TOTAL-E AT 1232.

       GRAVAR.
              DISPLAY "SALVAR (S/N)? [ ]" AT 1430.
              ACCEPT WS-SALVA AT 1445 WITH PROMPT AUTO.

       GRAVA-REG.
              CLOSE PRODUTOS.
              OPEN EXTEND PRODUTOS.
              MOVE REG-PROD-W TO REG-PROD.
              WRITE REG-PROD.
              IF ARQST NOT = "00"
                   DISPLAY "ERRO DE GRAVA€ÇO" AT 1535
                   STOP " ".
              CLOSE PRODUTOS.
              PERFORM ABRE-ARQ.

       CONTINUA.
              DISPLAY "CONTINUA (S/N)? [ ]" AT 1430.
              ACCEPT WS-OPCAO AT 1447 WITH PROMPT AUTO.
              IF WS-OPCAO = "S" OR = "N"
                     DISPLAY WS-ESPACO AT 1430
                     DISPLAY WS-ESPACO AT 1535
              ELSE
                     DISPLAY WS-ESPACO AT 1535
                     DISPLAY "DIGITE S OU N" AT 1535.

       FINALIZA.
              DISPLAY WS-MENS1 AT 1535.
              CLOSE PRODUTOS.	
              STOP RUN.
