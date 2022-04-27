*&---------------------------------------------------------------------*
*& Report ZCLIENTESPROVEEDORES_GP1
*&---------------------------------------------------------------------*
*& Autor: Alan Sforzini
*&---------------------------------------------------------------------*
REPORT ZCLIENTESPROVEEDORES_GP1.

INCLUDE ZCLIENTESPROVEEDORES_GP1_TOP.
INCLUDE ZCLIENTESPROVEEDORES_GP1_F01.

**************************************************************************

AT SELECTION-SCREEN OUTPUT.

PERFORM ZPARAMETROS.

**************************************************************************

START-OF-SELECTION.

PERFORM ZARMAR_USER_INPUT.

**************************************************************************

END-OF-SELECTION.

PERFORM ZOPERACION.