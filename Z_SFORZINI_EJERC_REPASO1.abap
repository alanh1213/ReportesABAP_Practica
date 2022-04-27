*&---------------------------------------------------------------------*
*& Report Z_SFORZINI_EJERC_REPASO1
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT Z_SFORZINI_EJERC_REPASO1.

INCLUDE Z_SFORZINI_EJERC_REPASO1_TOP.
INCLUDE Z_SFORZINI_EJERC_REPASO1_F01.

START-OF-SELECTION.
PERFORM ZACCEDER_BD.

END-OF-SELECTION.
PERFORM ZPROCESAR_DATOS.

IF P01 = 'X'.
    PERFORM ZIMPRIMIR_WRITE.
ELSE.
    PERFORM ZIMPRIMIR_ALV.
ENDIF.