*&---------------------------------------------------------------------*
*& Include          Z_SFORZINI_EJERC_REPASO1_F01
*&---------------------------------------------------------------------*

FORM ZACCEDER_BD.

    SELECT EBELN BSART RLWRT WAERS AEDAT ERNAM LIFNR
    FROM EKKO
    INTO TABLE GT_EKKO
    WHERE EBELN IN P_EBELN
    AND   BSART IN P_BSART
    AND   ERNAM IN P_ERNAM
    AND   LIFNR IN P_LIFNR.
    
    IF SY-SUBRC = 0.
    
        SELECT EBELN BSTYP PSTYP LOEKZ
        FROM EKPO
        INTO TABLE GT_EKPO
        FOR ALL ENTRIES IN GT_EKKO
        WHERE EBELN = GT_EKKO-EBELN.
        IF SY-SUBRC <> 0.
            MESSAGE 'No se pudo acceder a la base de datos para la tabla EKPO' TYPE 'I'.
        ENDIF.
    
        SELECT LIFNR ADRNR NAME1
        FROM LFA1
        INTO TABLE GT_LFA1
        FOR ALL ENTRIES IN GT_EKKO
        WHERE LIFNR = GT_EKKO-LIFNR.
        IF SY-SUBRC = 0.
    
            SELECT ADDRNUMBER SMTP_SRCH
            FROM ADR6
            INTO TABLE GT_ADR6
            FOR ALL ENTRIES IN GT_LFA1
            WHERE ADDRNUMBER = GT_LFA1-ADRNR.
            IF SY-SUBRC <> 0.
    
            ENDIF.
        ELSE.
            MESSAGE 'No se pudo acceder a la base de datos para la tabla LFA1' TYPE 'I'.
        ENDIF.
    
        SELECT SPRAS BSART BSTYP BATXT
        FROM T161T
        INTO TABLE GT_T161T
        FOR ALL ENTRIES IN GT_EKKO
        WHERE BSART = GT_EKKO-BSART.
        IF SY-SUBRC <> 0.
            MESSAGE 'No se pudo acceder a la base de datos para la tabla T161T' TYPE 'I'.
        ENDIF.
    
        SELECT BNAME NAME_TEXT
        FROM V_USR_NAME
        INTO TABLE GT_V_USR_NAME
        FOR ALL ENTRIES IN GT_EKKO
        WHERE BNAME = GT_EKKO-ERNAM.
        IF SY-SUBRC <> 0.
            MESSAGE 'No se pudo acceder a la base de datos para la tabla V_USR_NAME' TYPE 'I'.
        ENDIF.
    ELSE.
    
        MESSAGE 'No se pudo acceder a la base de datos para la tabla EKKO' TYPE 'I'.
    
    ENDIF.
    
    ENDFORM.
    ********************************************************************************************************
    FORM ZPROCESAR_DATOS.
    
        LOOP AT GT_EKKO INTO DATA(WA_EKKO).
    
            DATA: LV_MONTO_IMP TYPE FLOAT,
            LV_EKPO_REGISTROS TYPE I,
            LV_POS_TIPO TYPE STRING,
            LV_EMAIL_VAL TYPE STRING,
            WA_ZCONSULTA TYPE ZCONSULTA_SFORZI.
    
            "******************************************
            READ TABLE GT_EKPO INTO DATA(WA_EKPO) WITH KEY EBELN = WA_EKKO-EBELN. "---> Vengo a buscar los datos del chabon, no las compras q hizo.
            READ TABLE GT_T161T INTO DATA(WA_T161T) WITH KEY SPRAS = 'ES' BSART = WA_EKKO-BSART BSTYP = WA_EKPO-BSTYP.
            READ TABLE GT_LFA1 INTO DATA(WA_LFA1) WITH KEY LIFNR = WA_EKKO-LIFNR.
            READ TABLE GT_ADR6 INTO DATA(WA_ADR6) WITH KEY ADDRNUMBER = WA_LFA1-ADRNR. "---> Voy a una lista con datos repetidos, no importa, xq solo vengo a buscar datos.
            READ TABLE GT_V_USR_NAME INTO DATA(WA_V_USR_NAME) WITH KEY BNAME = WA_EKKO-ERNAM.
            READ TABLE GT_EKPO INTO DATA(WA_EKPO_INDEX1) INDEX 1.  "---> Lee el primer registro de la EKPO, si PSTYP = 9 “Servicio”, si no “Material”
            IF SY-SUBRC = 0.
                IF WA_EKPO_INDEX1-PSTYP = 9.
                    LV_POS_TIPO = 'Servicio'.
                ELSE.
                    LV_POS_TIPO = 'Material'.
                ENDIF.
            ENDIF.
    
            "******************************************
            CALL FUNCTION 'ZAPR_USER_SF' "---> Determina que nivel es tanto de los montos como de los aprobadores.
            EXPORTING
                P_RLWRT       = WA_EKKO-RLWRT
            IMPORTING
                V_NIVEL       = WA_FINAL-NIVEL
                V_APROBADORES = WA_FINAL-APROBADORES.
    
            "******************************************
            CONCATENATE WA_EKKO-LIFNR '-' WA_LFA1-NAME1 INTO WA_FINAL-PROVEEDOR.
            "******************************************
            CONCATENATE WA_EKKO-BSART '-' WA_T161T-BATXT INTO WA_FINAL-DOC_TYPE.
            "******************************************
            IF WA_EKKO-RLWRT > 0.
            LV_MONTO_IMP = P_IMP * WA_EKKO-RLWRT / 100.   "---> Calculo el porcentaje en base al numero de monto
            ENDIF.
            "******************************************
            CALL FUNCTION 'CONVERSION_EXIT_LDATE_OUTPUT'  "--->Fecha convertida
            EXPORTING
                INPUT         = WA_EKKO-AEDAT
            IMPORTING
                OUTPUT        = WA_FINAL-FECHA.
            "******************************************
            TRANSLATE WA_LFA1-STRAS TO UPPER CASE.  "---> Direccion en mayusculas
            "******************************************
            REPLACE ALL OCCURRENCES OF '-' IN WA_LFA1-TELF1 WITH ' '.  "---> Saca los - del numero de telefono
            WA_FINAL-TELEFONO = WA_LFA1-TELF1.
            "******************************************
    
            IF WA_ADR6-SMTP_SRCH CO '@'.    "---> Chequea la cadena de texto por chars para ver si el hay algun @
                LV_EMAIL_VAL = 'SI'.
            ELSE.
                LV_EMAIL_VAL = 'NO'.
            ENDIF.
    
            "******************************************
            SELECT COUNT(*) FROM ZCONSULTA_SFORZI.   "---> Determina el numero de ID para una nueva consulta
            WA_ZCONSULTA-REPORTE = SY-REPID.
            WA_ZCONSULTA-REGISTROS_MOSTRADOS = SY-DBCNT + 1.
            WA_ZCONSULTA-USUARIO = WA_V_USR_NAME-NAME_TEXT.
            WA_ZCONSULTA-FECHA = SY-DATUM.
            WA_ZCONSULTA-HORA = SY-UZEIT.
            "******************************************
            LOOP AT GT_EKPO INTO DATA(LOOP_EKPO).   "---> Determina el numero de registros con 'L'.
                IF LOOP_EKPO-LOEKZ = 'L'.
                    LV_EKPO_REGISTROS = LV_EKPO_REGISTROS + 1.
                ENDIF.
            ENDLOOP.
            "******************************************
    
            WA_FINAL-DOC = WA_EKKO-EBELN.
            WA_FINAL-MONTO = WA_EKKO-RLWRT.
            WA_FINAL-MONTO_IMP = WA_EKKO-RLWRT + LV_MONTO_IMP.
            WA_FINAL-MONEDA = WA_EKKO-WAERS.
            WA_FINAL-NAME = WA_V_USR_NAME-NAME_TEXT.
            WA_FINAL-POS = lines( GT_EKPO ).
            WA_FINAL-POS_B = LV_EKPO_REGISTROS.
            WA_FINAL-POS_TYPE = LV_POS_TIPO.
            WA_FINAL-DIRECCION = WA_LFA1-STRAS.
            WA_FINAL-EMAIL = WA_ADR6-SMTP_SRCH.
            WA_FINAL-EMAIL_VAL = LV_EMAIL_VAL.
            WA_FINAL-CONS_NUM = WA_ZCONSULTA-REGISTROS_MOSTRADOS.
    
            APPEND WA_FINAL TO GT_FINAL.
            IF SY-SUBRC = 0.
                INSERT ZCONSULTA_SFORZI FROM WA_ZCONSULTA.
                COMMIT WORK.
            ENDIF.
        ENDLOOP.
    
    
    
    ENDFORM.
    ********************************************************************************************************
    FORM ZIMPRIMIR_ALV.
    
    DATA: ST_LAYOUT TYPE SLIS_LAYOUT_ALV,   "---> Declaraciones para reporte ALV
          V_REPID LIKE SY-REPID.
    
    DATA(LT_CATALOGO) = VALUE SLIS_T_FIELDCAT_ALV( ( fieldname = 'DOC' seltext_m = 'Doc.compras' )
                           ( fieldname = 'DOC_TYPE' seltext_m = 'Clase de documento' )
                           ( fieldname = 'MONTO' seltext_m = 'Monto' )
                           ( fieldname = 'MONTO_IMP' seltext_m = 'Monto con impuestos' )
                           ( fieldname = 'MONEDA' seltext_m = 'Moneda' )
                           ( fieldname = 'NAME' seltext_m = 'Creador por' )
                           ( fieldname = 'FECHA' seltext_m = 'Creado el' )
                           ( fieldname = 'POS' seltext_m = 'Cant. Posiciones' )
                           ( fieldname = 'POS_B' seltext_m = 'Cant. Posiciones Borradas' )
                           ( fieldname = 'POS_TYPE' seltext_m = 'Tipo de posición' )
                           ( fieldname = 'NIVEL' seltext_m = 'Estrategia' )
                           ( fieldname = 'APROBADORES' seltext_m = 'Aprobadores' )
                           ( fieldname = 'PROVEEDOR' seltext_m = 'Proveedor' )
                           ( fieldname = 'DIRECCION' seltext_m = 'Dirección' )
                           ( fieldname = 'TELEFONO' seltext_m = 'Teléfono' )
                           ( fieldname = 'EMAIL' seltext_m = 'Email' )
                           ( fieldname = 'EMAIL_VAL' seltext_m = 'Email válido' )
                           ( fieldname = 'CONS_NUM' seltext_m = 'Consulta número' ) ).
    
    CALL FUNCTION 'REUSE_ALV_LIST_DISPLAY'
     EXPORTING
       I_CALLBACK_PROGRAM             = V_REPID
       IS_LAYOUT                      = ST_LAYOUT
       IT_FIELDCAT                    = LT_CATALOGO
      TABLES
        T_OUTTAB                       = GT_FINAL
     EXCEPTIONS
       PROGRAM_ERROR                  = 1
       OTHERS                         = 2
              .
    IF SY-SUBRC <> 0.
        MESSAGE 'ERROR AL IMPRIMIR LA TABLA' TYPE 'I'.
    ENDIF.
    
    ENDFORM.
    ********************************************************************************************************
    FORM ZIMPRIMIR_WRITE.
    LOOP AT GT_FINAL INTO DATA(LOOP_FINAL).
    WRITE: / 'DOC_TYPE:', LOOP_FINAL-DOC_TYPE,
           / 'MONTO:', LOOP_FINAL-MONTO,
           / 'MONTO_IMP:', LOOP_FINAL-MONTO_IMP,
           / 'MONEDA:', LOOP_FINAL-MONEDA,
           / 'NAME:', LOOP_FINAL-NAME,
           / 'FECHA:', LOOP_FINAL-FECHA,
           / 'POS:', LOOP_FINAL-POS,
           / 'POS_B:', LOOP_FINAL-POS_B,
           / 'POS_TYPE:', LOOP_FINAL-POS_TYPE,
           / 'NIVEL:', LOOP_FINAL-NIVEL,
           / 'APROBADORES:', LOOP_FINAL-APROBADORES,
           / 'PROVEEDOR:', LOOP_FINAL-PROVEEDOR,
           / 'DIRECCION:', LOOP_FINAL-DIRECCION,
           / 'TELEFONO:', LOOP_FINAL-TELEFONO,
           / 'EMAIL:', LOOP_FINAL-EMAIL,
           / 'EMAIL_VAL:', LOOP_FINAL-EMAIL_VAL,
           / 'CONS_NUM:', LOOP_FINAL-CONS_NUM.
    
    ENDLOOP.
    ENDFORM.