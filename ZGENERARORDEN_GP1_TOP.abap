*&---------------------------------------------------------------------*
*& Include          ZGENERARORDEN_GP1_TOP
*&---------------------------------------------------------------------*
"TIPOS

TYPES: BEGIN OF TY_MATERIALES,
         MANDT    TYPE ZMATERIALES_GP1-MANDT,
         ID       TYPE ZMATERIALES_GP1-ID,
         PLANTA   TYPE ZMATERIALES_GP1-PLANTA,
         LABST    TYPE STRING,
         UNIDAD   TYPE ZMATERIALES_GP1-UNIDAD,
         MATERIAL TYPE ZMATERIALES_GP1-MATERIAL,
       END OF TY_MATERIALES.

*&---------------------------------------------------------------------*
"PARAMETROS PARA EL USUARIO

SELECTION-SCREEN BEGIN OF BLOCK B1 WITH FRAME TITLE text-003.
PARAMETERS: P01 RADIOBUTTON GROUP RB1 USER-COMMAND UC DEFAULT 'X',   " Parametros de programa
            P02 RADIOBUTTON GROUP RB1.

DATA: GV_OP TYPE I VALUE 3.  "----> OP == Operation, es la variable que indica que va a hacer el programa (OC / OV)

SELECTION-SCREEN END OF BLOCK B1.


SELECTION-SCREEN BEGIN OF BLOCK B2 WITH FRAME TITLE text-002.
PARAMETERS: P_ZMENG TYPE ZDZMENG_GP1,   "---> Parametros de ORDENES (AMBAS)
            P_VBELNF TYPE ZVBELN_VF_GP1.

PARAMETERS: P_VBELN TYPE ZVBELN_VA_GP1 MODIF ID MD1,   "---> Parametros de ORDEN DE VENTA
            P_KUNNR TYPE ZKUNNR_GP1 MODIF ID MD1.

PARAMETERS: P_EBELN TYPE ZEBELN_GP1 MODIF ID MD2,     "---> Parametros de ORDEN DE COMPRA
            P_LIFNR TYPE ZLIFNR_GP1 MODIF ID MD2.
SELECTION-SCREEN END OF BLOCK B2.


SELECTION-SCREEN BEGIN OF BLOCK B3 WITH FRAME TITLE text-001.       "---> Formulario del material
PARAMETERS: P_BUKRS TYPE ZBUKRS_GP1,
            P_MATNR TYPE ZMATNR_GP1,
            P_WERKS TYPE ZWERKS_D_GP1,
            P_LABST TYPE ZLABST_GP1,
            P_MEINS TYPE ZMEINS_GP1,
            P_MAKTX TYPE ZMAKTX_GP1.
SELECTION-SCREEN END OF BLOCK B3.

*&---------------------------------------------------------------------*
"VARIABLES, TABLAS Y WAs

DATA: BI_ZMATERIALES TYPE STANDARD TABLE OF TY_MATERIALES,
      WA_USER_ORDENCOMPRA TYPE ZORDENCOMPRA_GP1,
      WA_DB_ORDENCOMPRA TYPE ZORDENCOMPRA_GP1,
      WA_USER_ORDENVENTA TYPE ZORDENVENTA_GP1,
      WA_ZORDENVENTA TYPE ZORDENVENTA_GP1,
      WA_ZMATERIALES TYPE ZMATERIALES_GP1,   "---> Para modificar un material
      WA_ZMATERIALES_BI TYPE TY_MATERIALES.   "---> Para agregar usando batch input


DATA: WA_BDC_TAB TYPE BDCDATA,
      BDC_TAB    TYPE TABLE OF BDCDATA.