*&---------------------------------------------------------------------*
*& Report ZGENERARORDEN_GP1
*&---------------------------------------------------------------------*
*& Autor: Alan Sforzini.
*&---------------------------------------------------------------------*
REPORT ZGENERARORDEN_GP1.

INCLUDE ZGENERARORDEN_GP1_TOP.
INCLUDE ZMATERIALES_GP1_F02.
INCLUDE ZGENERARORDEN_GP1_F01.


AT SELECTION-SCREEN OUTPUT.
PERFORM ZPARAMETROS.


START-OF-SELECTION.
IF GV_OP <> 3.
    PERFORM ZPROCESAR_USER_INPUT.
    PERFORM ZVALIDACIONES_OPERACIONES.
ELSE.
    MESSAGE 'DEBE COMPLETAR CORRECTAMENTE LOS CAMPOS OBLIGATORIOS.' TYPE 'I'.
ENDIF.