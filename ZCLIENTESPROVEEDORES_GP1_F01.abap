*&---------------------------------------------------------------------*
*& Include          ZCLIENTESPROVEEDORES_GP1_F01
*&---------------------------------------------------------------------*

FORM ZPARAMETROS.  "---> Se encarga en esconder dinamicamente los parametros de clientes/proveedor en caso de que el user elija el opuesto, ademas de indicarle al programa como ejecutarse.

    IF P01 = 'X'.  "---> AGREGAR
       OP_AGREGAR = 0.
    ELSE.          "---> QUITAR
       LOOP AT SCREEN.
          IF SCREEN-GROUP1 = 'MD3'.
             SCREEN-ACTIVE = 0.
             MODIFY SCREEN.
          ENDIF.
       ENDLOOP.
       OP_AGREGAR = 1.
    ENDIF.
    
    IF P03 = 'X'.  "---> CLIENTE
       LOOP AT SCREEN.
          IF SCREEN-GROUP1 = 'MD2'.
             SCREEN-ACTIVE = 0.
             MODIFY SCREEN.
          ENDIF.
       ENDLOOP.
       OP_CLIENTE = 0.
    ELSE.          "---> PROVEEDOR
       LOOP AT SCREEN.
          IF SCREEN-GROUP1 = 'MD1'.
             SCREEN-ACTIVE = 0.
             MODIFY SCREEN.
          ENDIF.
       ENDLOOP.
       OP_CLIENTE = 1.
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZOPERACION.
    
    IF OP_AGREGAR = 0.   "---> AGREGAR
    
        IF OP_CLIENTE = 0.
            "Agregar cliente.
            IF STRLEN( P_KUNNR ) = 10.
                PERFORM ZAGREGAR_CLIENTE.
            ELSE.
                MESSAGE 'EL CAMPO KUNNR DEBE TENER 10 CARACTERES' TYPE 'I'.
            ENDIF.
        ELSE.
            "Agregar proveedor.
            IF STRLEN( P_LIFNR ) = 10.
                PERFORM ZAGREGAR_PROVEEDOR.
            ELSE.
                MESSAGE 'EL CAMPO LIFNR DEBE TENER 10 CARACTERES' TYPE 'I'.
            ENDIF.
        ENDIF.
    
    ELSE.   "---> QUITAR
    
        IF OP_CLIENTE = 0.
            "Eliminar cliente.
            IF STRLEN( P_KUNNR ) = 10.
                PERFORM ZELIMINAR_CLIENTE.
            ELSE.
                WRITE: / 'EL CAMPO KUNNR DEBE TENER 10 CARACTERES'.
            ENDIF.
        ELSE.
            "Eliminar proveedor.
            IF STRLEN( P_LIFNR ) = 10.
                PERFORM ZELIMINAR_PROVEEDOR.
            ELSE.
                WRITE: / 'EL CAMPO LIFNR DEBE TENER 10 CARACTERES'.
            ENDIF.
        ENDIF.
    
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZELIMINAR_CLIENTE.  "---> Valida si el cliente tiene OV, en caso negativo, lo elimina, en caso positivo, dispara un mensaje diciendo q no puede ser eliminado.
    
    SELECT *
    FROM zordenventa_gp1
    INTO TABLE GT_CLIENTES_SFACTURA
    WHERE vbeln_f = ''
    AND   KUNNR   = P_KUNNR.
    
    IF SY-SUBRC = 0.
       MESSAGE 'Existe orden de venta para ese cliente, no puede ser eliminado.' TYPE 'I'.
    ELSE.
       DELETE FROM ZCLIENTES_GP1 WHERE KUNNR = P_KUNNR. "---> Borro de la tabla usando su clave primaria
       IF SY-SUBRC = 0.
           COMMIT WORK.
           MESSAGE 'No existe orden de venta para ese cliente, CLIENTE ELIMINADO.' TYPE 'S'.
       ELSE.
           MESSAGE 'Algo salio mal...' TYPE 'S'.
       ENDIF.
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZELIMINAR_PROVEEDOR.  "---> Valida si el proveedor tiene OC, en caso negativo, lo elimina, en caso positivo, dispara un mensaje diciendo q no puede ser eliminado.
    
    
    SELECT *
    FROM ZORDENCOMPRA_GP1
    INTO TABLE GT_PROVEEDORES_SFACTURA
    WHERE vbeln_f = ''
    AND   LIFNR   = P_LIFNR.
    
    IF SY-SUBRC = 0.
       MESSAGE 'Existe orden de compra para ese proveedor, no puede ser eliminado.' TYPE 'I'.
    ELSE.
       DELETE FROM ZPROVEEDORES_GP1 WHERE LIFNR = P_LIFNR. "---> Borro de la tabla usando su clave primaria
       IF SY-SUBRC = 0.
           COMMIT WORK.
           MESSAGE 'No existe orden de compra para ese proveedor, PROVEEDOR ELIMINADO.' TYPE 'S'.
       ELSE.
           MESSAGE 'Algo salio mal...' TYPE 'S'.
       ENDIF.
    ENDIF.
    
    
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZARMAR_USER_INPUT.   "---> Procesa los parametros de entrada en tablas internas, dependiendo de que haya elegido el user por parametros.
    
    IF OP_CLIENTE = 0.
        WA_ZCLIENTES-KUNNR = P_KUNNR.
        WA_ZCLIENTES-NAME1 = P_NAME1.
        WA_ZCLIENTES-NAME2 = P_NAME2.
        WA_ZCLIENTES-BUKRS = P_BUKRS.
        WA_ZCLIENTES-STKZN = P_STKZN.
        WA_ZCLIENTES-REGIO = P_REGIO.
        WA_ZCLIENTES-PSTLZ = P_PSTLZ.
    ELSE.
        WA_ZPROVEEDORES-LIFNR = P_LIFNR.
        WA_ZPROVEEDORES-NAME1 = P_NAME1.
        WA_ZPROVEEDORES-NAME2 = P_NAME2.
        WA_ZPROVEEDORES-BUKRS = P_BUKRS.
        WA_ZPROVEEDORES-STKZN = P_STKZN.
        WA_ZPROVEEDORES-REGIO = P_REGIO.
        WA_ZPROVEEDORES-PSTLZ = P_PSTLZ.
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZAGREGAR_CLIENTE. "---> Valida si existe el cliente en la base de datos, en caso negativo, lo agrega.
    
    SELECT SINGLE *
    FROM ZCLIENTES_GP1
    INTO WA_ZCLIENTES_AUX
    WHERE KUNNR = P_KUNNR.
    
    IF SY-SUBRC = 0.
        "Ya existe el cliente en la tabla
        WRITE: / 'EL NUMERO DE CLIENTE YA EXISTE EN LOS REGISTROS.'.
    ELSE.
        "No existe ese cliente en la tabla. CREAR REGISTRO
        PERFORM ZAGREGAR_BATCHINPUT_CLIENTE.
    
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZAGREGAR_PROVEEDOR.  "---> Valida si existe el proveedor en la base de datos, en caso negativo, lo agrega.
    
    SELECT SINGLE *
    FROM ZPROVEEDORES_GP1
    INTO WA_ZPROVEEDORES_AUX
    WHERE LIFNR = P_LIFNR.
    
    IF SY-SUBRC = 0.
        "Ya existe el cliente en la tabla
        MESSAGE 'EL NUMERO DE PROVEEDOR YA EXISTE EN LOS REGISTROS.' TYPE 'I'.
    ELSE.
        "No existe ese cliente en la tabla. CREAR REGISTRO
        PERFORM ZAGREGAR_BATCHINPUT_PROVEEDOR.
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZAGREGAR_BATCHINPUT_CLIENTE.
    
    PERFORM ZHACER_TABLA_BDC USING:
    'X' 'SAPMSVMA' '0100',
    ' ' 'BDC_OKCODE' '=UPD',
    ' ' 'VIEWNAME' 'ZCLIENTES_GP1',
    ' ' 'VIMDYNFLDS-LTD_DTA_NO' 'X',
    
    'X' 'SAPLZSFORZINI' '0007',
    ' ' 'BDC_OKCODE' '=NEWL',
    
    'X' 'SAPLZSFORZINI' '0007',
    ' ' 'BDC_OKCODE' '=BACK',
    ' ' 'ZCLIENTES_GP1-KUNNR(01)' WA_ZCLIENTES-KUNNR,
    ' ' 'ZCLIENTES_GP1-NAME1(01)' WA_ZCLIENTES-NAME1,
    ' ' 'ZCLIENTES_GP1-REGIO(01)' WA_ZCLIENTES-REGIO,
    ' ' 'ZCLIENTES_GP1-PSTLZ(01)' WA_ZCLIENTES-PSTLZ,
    
    'X' 'SAPLZSFORZINI' '0007',
    ' ' 'BDC_OKCODE' '=SAVE',
    
    'X' 'SAPLZSFORZINI' '0007',
    ' ' 'BDC_OKCODE' '=BACK',
    
    'X' 'SAPMSVMA' '0100',
    ' ' 'BDC_OKCODE' '/EBACK'.
    
    CALL TRANSACTION 'SM30' USING bdc_tab MODE 'N'.
    
    IF SY-SUBRC = 0.
    
        MESSAGE 'EL CLIENTE SE A AGREGADO CORRECTAMENTE.' TYPE 'S'.
    ELSE.
        MESSAGE 'EL CLIENTE NO PUDO SER AGREGADO CORRECTAMENTE.' TYPE 'I'.
    ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZAGREGAR_BATCHINPUT_PROVEEDOR.
    
        PERFORM ZHACER_TABLA_BDC USING:
        'X' 'SAPMSVMA' '0100',
        ' ' 'BDC_OKCODE' '=UPD',
        ' ' 'VIEWNAME' 'ZPROVEEDORES_GP1',
        ' ' 'VIMDYNFLDS-LTD_DTA_NO' 'X',
    
        'X' 'SAPLZSFORZINI' '0008',
        ' ' 'BDC_OKCODE' '=NEWL',
    
        'X' 'SAPLZSFORZINI' '0008',
        ' ' 'BDC_OKCODE' '=BACK',
        ' ' 'ZPROVEEDORES_GP1-LIFNR(01)' WA_ZPROVEEDORES-LIFNR,
        ' ' 'ZPROVEEDORES_GP1-NAME1(01)' WA_ZPROVEEDORES-NAME1,
        ' ' 'ZPROVEEDORES_GP1-REGIO(01)' WA_ZPROVEEDORES-REGIO,
        ' ' 'ZPROVEEDORES_GP1-PSTLZ(01)' WA_ZPROVEEDORES-PSTLZ,
    
        'X' 'SAPLZSFORZINI' '0008',
        ' ' 'BDC_OKCODE' '=SAVE',
    
        'X' 'SAPLZSFORZINI' '0008',
        ' ' 'BDC_OKCODE' '=BACK',
    
        'X' 'SAPMSVMA' '0100',
        ' ' 'BDC_OKCODE' '/EBACK'.
    
        CALL TRANSACTION 'SM30' USING bdc_tab MODE 'N'.
    
        IF SY-SUBRC = 0.
    
            MESSAGE 'EL CLIENTE SE A AGREGADO CORRECTAMENTE.' TYPE 'S'.
        ELSE.
            MESSAGE 'EL CLIENTE NO PUDO SER AGREGADO CORRECTAMENTE.' TYPE 'I'.
        ENDIF.
    
    ENDFORM.
    
    **************************************************************************
    **************************************************************************
    **************************************************************************
    
    FORM ZHACER_TABLA_BDC USING dynbegin name value.            "---> Algoritmo con la logica del BI
    
    IF dynbegin = 'X'.
    
    CLEAR wa_bdc_tab.
    wa_bdc_tab-program = name.
    wa_bdc_tab-dynpro = value.
    wa_bdc_tab-dynbegin = 'X'.
    APPEND wa_bdc_tab TO bdc_tab.
    
    ELSE.
    
    CLEAR wa_bdc_tab.
    wa_bdc_tab-fnam = name.
    wa_bdc_tab-fval = value.
    APPEND wa_bdc_tab TO bdc_tab.
    
    ENDIF.
    ENDFORM.