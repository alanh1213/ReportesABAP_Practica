*&---------------------------------------------------------------------*
*& Include          ZGENERARORDEN_GP1_F01
*&---------------------------------------------------------------------*

FORM ZPARAMETROS.  "---> Modifica dinamicamente los parametros que tiene en pantalla el usuario en base a que radiobutton tenga seleccionado y ademas le indica al programa que hacer.

    IF P01 = 'X'.  "---> HACER ORDEN VENTA
       LOOP AT SCREEN.
          IF SCREEN-GROUP1 = 'MD2'.
             SCREEN-ACTIVE = 0.
             MODIFY SCREEN.
          ENDIF.
       ENDLOOP.
       GV_OP = 0.
    ELSEIF P02 = 'X'.          "---> HACER ORDEN COMPRA
       LOOP AT SCREEN.
          IF SCREEN-GROUP1 = 'MD1'.
             SCREEN-ACTIVE = 0.
             MODIFY SCREEN.
          ENDIF.
       ENDLOOP.
       GV_OP = 1.
    ELSE.
       GV_OP = 3.
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZPROCESAR_USER_INPUT.  "---> Mete todos los parametros en WA para su posterior uso.
    
    WA_ZMATERIALES_BI-ID = P_MATNR.
    WA_ZMATERIALES_BI-PLANTA = P_WERKS.
    WA_ZMATERIALES_BI-LABST = P_LABST.
    WA_ZMATERIALES_BI-UNIDAD = P_MEINS.
    WA_ZMATERIALES_BI-MATERIAL = P_MAKTX.
    
    APPEND WA_ZMATERIALES_BI TO BI_ZMATERIALES.
    
    IF GV_OP = 0.
        WA_USER_ORDENVENTA-VBELN = P_VBELN.
        WA_USER_ORDENVENTA-MATNR = P_MATNR.
        WA_USER_ORDENVENTA-ZMENG = P_ZMENG.
        WA_USER_ORDENVENTA-MEINS = P_MEINS.
        WA_USER_ORDENVENTA-WERKS = P_WERKS.
        WA_USER_ORDENVENTA-VBELN_F = P_VBELNF.
        WA_USER_ORDENVENTA-KUNNR = P_KUNNR.
    ELSE.
        WA_USER_ORDENCOMPRA-EBELN = P_EBELN.
        WA_USER_ORDENCOMPRA-MATNR = P_MATNR.
        WA_USER_ORDENCOMPRA-ZMENG = P_ZMENG.
        WA_USER_ORDENCOMPRA-MEINS = P_MEINS.
        WA_USER_ORDENCOMPRA-WERKS = P_WERKS.
        WA_USER_ORDENCOMPRA-VBELN_F = P_VBELNF.
        WA_USER_ORDENCOMPRA-LIFNR = P_LIFNR.
    
    ENDIF.
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZVALIDACIONES_OPERACIONES.  "---> Valida las condiciones necesarias para efectuar las operaciones, en caso de estar todo OK, las ejecuta y en caso opuesto, emite un mensaje.
    
        DATA: LV_ZMATERIALES_GP1 TYPE ZMATERIALES_GP1,
              LV_ZSOCIEDAD_GP1 TYPE ZSOCIEDAD_GP1.
    
        SELECT SINGLE *                                     "----> VALIDA SI EXISTE LA SOCIEDAD
        FROM ZSOCIEDAD_GP1
        INTO LV_ZSOCIEDAD_GP1
        WHERE BUKRS = P_BUKRS.
    
        IF SY-SUBRC = 0.
            "*EXISTE ESA SOCIEDAD
    
            SELECT SINGLE *
            FROM ZMATERIALES_GP1                           "----> VALIDA SI EXISTEN PLANTA Y MATERIAL
            INTO LV_ZMATERIALES_GP1
            WHERE ID     = P_MATNR
            AND   PLANTA = P_WERKS.
    
            IF SY-SUBRC = 0 AND GV_OP = 0. "---> OV: Existe el material en esa planta y sociedad, se emite OV y se borra de la tabla materiales el material vendido.
                PERFORM ZOPERACION_OV.
    
            ELSEIF SY-SUBRC <> 0 AND GV_OP = 0.  "---> OV: No existe ese material y sociedad, no se puede emitir OV por que no hay stock.
                MESSAGE 'NO PUEDE, NO HAY STOCK DE MATERIALES PARA ESTA OPERACION' TYPE 'I'.
    
            ELSEIF SY-SUBRC = 0 AND GV_OP = 1.  "---> OC: Existe ese material en esa planta y sociedad, se emite OC con valor de 10000 y se modifica el stock en la tabla materiales.
                PERFORM ZOPERACION_OC.
                PERFORM ZMODMAT.
    
            ELSEIF SY-SUBRC <> 0 AND GV_OP = 1.  "---> OC: No existe ese material en esa planta y sociedad, se emite OC con valor de 50000 y se agrega el nuevo material a la tabla materiales.
                PERFORM ZOPERACION_OC.
                PERFORM ZBATCH_INPUT_MATERIALES TABLES BI_ZMATERIALES.
    
            ENDIF.
    
        ELSE.
            MESSAGE 'NO EXISTE ESA SOCIEDAD' TYPE 'I'.
        ENDIF.
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZOPERACION_OC.  "---> Valida si existe la OC con ese documento, si no existe, crea la OC con los datos pasados por el user.
    
        SELECT SINGLE *
        FROM ZORDENCOMPRA_GP1
        INTO WA_DB_ORDENCOMPRA
        WHERE EBELN = P_EBELN.
    
        IF SY-SUBRC = 0.
            MESSAGE 'Existe una orden de compra con ese documento' TYPE 'I'.
        ELSE.
            MESSAGE 'SE GENERO LA ORDEN DE COMPRA: ' TYPE 'S'.
            INSERT ZORDENCOMPRA_GP1 FROM WA_USER_ORDENCOMPRA.
            "INSERTO la nueva orden de compra
        ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZOPERACION_OV.  "---> Valida si existe la OV con ese documento, si no existe, crea la OV con los datos pasados por parametros y borra el material vendido de la tabla de materiales
        SELECT SINGLE *
        FROM ZORDENVENTA_GP1
        INTO WA_ZORDENVENTA
        WHERE VBELN = P_VBELN.
    
        IF SY-SUBRC = 0.
           MESSAGE 'Existe una orden de venta con ese documento' TYPE 'I'.
        ELSE.
           "INSERTO la nueva orden de venta y DELETEO el respectivo material con f02 del stock
           MESSAGE 'AÑADIENDO...' TYPE 'S'.
           INSERT ZORDENVENTA_GP1 FROM WA_USER_ORDENVENTA.
           PERFORM ZBAJAMAT.
        ENDIF.
    ENDFORM.