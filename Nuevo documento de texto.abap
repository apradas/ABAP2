FUNCTION ZWS_COURIER_GENERAETIQUETAS .
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(PE_LIFNR) TYPE  LIFNR DEFAULT 'CORREOS'
*"     VALUE(PE_TKNUM) TYPE  TKNUM OPTIONAL
*"     VALUE(PE_VBELN_PED) TYPE  VBELN OPTIONAL
*"     VALUE(PE_AGRUPADO) TYPE  STRING
*"     VALUE(PE_FORMATO) TYPE  STRING
*"     VALUE(PE_TIPO_ETIQUETA) TYPE  ZZTIPOETIQUETA
*"  EXPORTING
*"     VALUE(PS_ETIQUETA_RESP) TYPE  ZWS_GENERA_ETIQUETAS_RESPONSE
*"  TABLES
*"      IT_ERROR STRUCTURE  ZB2C_ERROR
*"----------------------------------------------------------------------
* 0.- Declaración de variables
*==========================================================================
  data: cl_logistica              type REF TO ZWS_CO_LOGISTICA,
        lit_vttp                  like        vttp occurs 0 WITH HEADER LINE,
        lit_vbap                  like        vbap occurs 0 WITH HEADER LINE,
        lr_genera_etiquetas_req   TYPE        ZWS_GENERA_ETIQUETAS,
        lr_etiqueta               type        ZWS_GENERA_ETIQUETAS_ETIQUETA,
        lr_etiqueta_resp          type        ZWS_GENERA_ETIQUETAS_RESPONSE1,
        lr_item                   type        ZWS_GENERA_ETIQUETAS_ITEM,
        lo_sys_exception          TYPE REF TO cx_ai_system_fault,
        ld_bstkd                  like        vbkd-bstkd,
        ld_campo1                 TYPE        string,
        ld_campo2                 TYPE        string,
        ld_campo3                 TYPE        string,
        ld_campo4                 TYPE        string,
        ld_region_tda_t           type        BEZEI20,
        ld_region_tda_t_t110      type        BEZEI20,
        ld_region_t               type        BEZEI20,
        ld_proxy_error            type        string,
        wa_adrc                   LIKE        adrc,
        wa_adrc_tda               LIKE        adrc,
        wa_zhardcodes             LIKE        zhardcodes,
        wa_adr6                   LIKE        adr6,
        wa_vbpa                   LIKE        vbpa,
        ld_t001w_adrnr            like        t001w-adrnr,
        begin of lit_vbeln_ped occurs 0,
          vbeln like vbak-vbeln,
        end of lit_vbeln_ped,
        ld_vsbed                  like        vbak-vsbed,
        wa_t001w_t110             LIKE        t001w,
        wa_adrc_t110              LIKE        adrc.
.

* 1.- Lógica
*==========================================================================
  CREATE OBJECT CL_LOGISTICA.

* Verificamos que no pasen pedido y transporte informado en la llamada
  if pe_vbeln_ped is not initial and pe_tknum is not initial.
*   MsgError: Informar pedido o transporte en la llamada.
    CALL FUNCTION 'Z_B2C_CREAR_ERROR_B2C'
        EXPORTING
          i_tipo   = 'E'
          i_codigo = '756'
*         MESSAGE_V1 =
*         MESSAGE_V2 =
*         MESSAGE_V3 =
*         MESSAGE_V4 =
*         PARAMETER  =
*         ROW      =
*         FIELD    =
        TABLES
          it_error = it_error.

    EXIT.
  endif.

  if pe_vbeln_ped is not initial.
*   Si nos pasan un pedido...

*   Verificamos que el pedido exista
    select single vbeln
      from vbak
      into pe_vbeln_ped
     where vbeln = pe_vbeln_ped.

    if sy-subrc = 0.
*     Si pedido existe, lo agregamos
      lit_vbeln_ped-vbeln = pe_vbeln_ped.
      append lit_vbeln_ped.
    else.
*     Si pedido no existe, Error + Fin

*     MsgError: El pedido & no existe en el sistema
      CALL FUNCTION 'Z_B2C_CREAR_ERROR_B2C'
          EXPORTING
            i_tipo   = 'E'
            i_codigo = '755'
*          MESSAGE_V1 = pe_vbeln_ped
*           MESSAGE_V2 =
*           MESSAGE_V3 =
*           MESSAGE_V4 =
*           PARAMETER  =
*           ROW      =
*           FIELD    =
          TABLES
            it_error = it_error.

      EXIT.
    endif.
  elseif pe_tknum is not initial.
*   Si lo que nos pasan es un transporte, agregamos todos los pedidos contenidos en ese transporte

*   Recuperamos las entregas del transporte
    select *
      from vttp
      into table lit_vttp
     where tknum = pe_tknum.

    if sy-subrc <> 0.
*     MsgError: El transporte & no existe en el sistema
      CALL FUNCTION 'Z_B2C_CREAR_ERROR_B2C'
          EXPORTING
            i_tipo   = 'E'
            i_codigo = '757'
*           MESSAGE_V1 = pe_tknum
*           MESSAGE_V2 =
*           MESSAGE_V3 =
*           MESSAGE_V4 =
*           PARAMETER  =
*           ROW      =
*           FIELD    =
          TABLES
            it_error = it_error.

      EXIT.
    else.
*     Agregamos todos los pedidos de la entrega
      loop at lit_vttp.
*       Para cada entrega del transporte...

*       Recuperamos pedido origen
        select single vgbel
          from lips
          into lit_vbeln_ped-vbeln
         where vbeln = lit_vttp-vbeln.

        if sy-subrc = 0.
          append lit_vbeln_ped.
        endif.
      endloop.
    endif.

  else.
*   MsgError: No se ha especificado pedido o transporte en la llamada.
    CALL FUNCTION 'Z_B2C_CREAR_ERROR_B2C'
        EXPORTING
          i_tipo   = 'E'
          i_codigo = '754'
*         MESSAGE_V1 =
*         MESSAGE_V2 =
*         MESSAGE_V3 =
*         MESSAGE_V4 =
*         PARAMETER  =
*         ROW      =
*         FIELD    =
        TABLES
          it_error = it_error.

      EXIT.
  endif.

* WS: Datos generales
  lr_genera_etiquetas_req-PROVEEDOR     = pe_lifnr.
  if pe_tknum is not initial.
    lr_genera_etiquetas_req-ID_TRANSPORTE = pe_tknum.
  elseif pe_vbeln_ped is not initial.
    lr_genera_etiquetas_req-ID_TRANSPORTE = pe_vbeln_ped.
  endif.
  lr_genera_etiquetas_req-AGRUPADO = pe_agrupado.


* WS: Etiquetas (Generamos una etiqueta por pedido)
  loop at lit_vbeln_ped.
    clear: lr_etiqueta.

*   Recuperamos Condición de expedición
    select single vsbed
      from vbak
      into ld_vsbed
     where vbeln = lit_vbeln_ped-vbeln.

*   Recuperamos numero de pedido cliente
    clear: ld_bstkd.
    select single bstkd
      from vbkd
      into ld_bstkd
     where vbeln = lit_vbeln_ped-vbeln
       and posnr = '000000'.

*   Recuperamos lineas del pedido
    refresh lit_vbap.
    select *
      from vbap
      into table lit_vbap
     where vbeln = lit_vbeln_ped-vbeln
       and abgru = space.

*   Recuperamos numero de dirección del interlocutor WE del pedido
    clear wa_vbpa.
    select single *
      from vbpa
      into wa_vbpa
     where vbeln = lit_vbeln_ped-vbeln
       and parvw = 'WE'.

    if sy-subrc = 0.
*     Recuperamos datos de direccion
      clear wa_adrc.
      select single *
        from adrc
        into wa_adrc
       where addrnumber = wa_vbpa-adrnr.

      if ld_vsbed = '05'.
*       Si condición de expedición 05 debemos hacer split para sacar la dirección

*       Extraemos el punto de entrega y la calle del cliente(en ld_campo2 tendremos el punto de entrega y en ld_campo3 la dir del cliente)
        CONCATENATE wa_adrc-str_suppl1  wa_adrc-str_suppl2  wa_adrc-street  wa_adrc-str_suppl3 INTO ld_campo1.
        split ld_campo1 at '#' into ld_campo1 ld_campo2 ld_campo3 ld_campo4.
      else.
*       Si condición de expedición no es 05 no es necesario hacer el split.

*       No tenemos punto de recogida
        ld_campo2 = ''.

*       La dirección del cliente será tal cual la del maestro
        CONCATENATE wa_adrc-str_suppl1  wa_adrc-str_suppl2  wa_adrc-street  wa_adrc-str_suppl3 INTO ld_campo4.
      endif.

*     Recuperamos nombre de la region
      select single bezei
        from t005u
        into ld_region_t
       where spras = sy-langu
         and land1 = wa_adrc-country
         and bland = wa_adrc-region.

*     Recuperamos email
      select single *
        from adr6
        into wa_adr6
       where addrnumber = wa_vbpa-adrnr
         and smtp_addr <> space.
    endif.

*   Recuperamos CodProducto
    select single *
      from zhardcodes
      into wa_zhardcodes
     where programa = 'ZWS_COURIER_GENERAETIQUETAS'
       and param    = 'COD_PRODUCTO'
       and valor1   = pe_lifnr.

*   Recuperamos numero de direccion de la tienda del pedido
    select single t001w~adrnr
      into ld_t001w_adrnr
      from t001w join vbap on vbap~werks = t001w~werks
     where vbap~vbeln = lit_vbeln_ped-vbeln.

    if sy-subrc = 0.
*     Recuperamos datos de dirección de la tienda del pedido
      select single *
        from adrc
        into wa_adrc_tda
       where addrnumber = ld_t001w_adrnr.

*     Recuperamos nombre de la region
      select single bezei
        from t005u
        into ld_region_tda_t
       where spras = sy-langu
         and land1 = wa_adrc_tda-country
         and bland = wa_adrc_tda-region.

    endif.

*   Etiqueta: Datos etiqueta
    lr_etiqueta-REFERENCIA_CLIENTE1 = lit_vbeln_ped-vbeln.
    lr_etiqueta-REFERENCIA_CLIENTE2 = ld_bstkd.
    lr_etiqueta-PUNTO_ENTREGA       = ld_campo2.
    lr_etiqueta-cod_producto        = wa_zhardcodes-valor2.
    lr_etiqueta-formato_etiqueta    = pe_formato.
    lr_etiqueta-tipo_etiqueta       = pe_tipo_etiqueta.

*   Etiqueta: Datos remitente
    if PE_LIFNR = 'SWISSPOST'.

    else.
      lr_etiqueta-REMITENTE-NOMBRE           = 'CAMPER'.
      lr_etiqueta-REMITENTE-DIRECCION        = wa_adrc_tda-street.
      lr_etiqueta-REMITENTE-COD_PROVINCIA    = wa_adrc_tda-region.
      lr_etiqueta-REMITENTE-NOMBRE_PROVINCIA = ld_region_tda_t.
      lr_etiqueta-REMITENTE-LOCALIDAD        = wa_adrc_tda-city1.
      lr_etiqueta-REMITENTE-COD_POSTAL       = wa_adrc_tda-post_code1.
    endif.

*   Etiqueta: Datos destinatario
    lr_etiqueta-destinatario-NOMBRE               = wa_adrc-name1.
    CONCATENATE wa_adrc-name2 wa_adrc-name3 wa_adrc-name4 into lr_etiqueta-destinatario-APELLIDOS SEPARATED BY space.
    lr_etiqueta-destinatario-DIRECCION            = ld_campo4.
    lr_etiqueta-destinatario-COD_PROVINCIA        = wa_adrc-region.
    lr_etiqueta-destinatario-NOMBRE_PROVINCIA     = ld_region_t.
    lr_etiqueta-destinatario-LOCALIDAD            = wa_adrc-city1.
    lr_etiqueta-destinatario-COD_POSTAL           = wa_adrc-post_code1.
    lr_etiqueta-destinatario-TELEFONOCONTACTO     = wa_adrc-tel_number.
    lr_etiqueta-destinatario-EMAIL                = wa_adr6-smtp_addr.

*   APRADAS-Inicio-28.06.2017
*   Etqiueta: Datos DestinatarioDevolucion
    if pe_tipo_etiqueta = 'DEVOLUCION' or
       pe_tipo_etiqueta = 'ENVIOYDEVOLUCION'.
      if wa_adrc-country = 'CH'.
*       Si el país del destinatario es suiza registramos en la etiqueta los
*       datos de dirección de la tienda T110.

*       Recuperamos datos de direccion de la tienda T110:
*       1) Datos generales de la tienda
        SELECT SINGLE *
          FROM t001w
          INTO wa_t001w_t110
         WHERE werks = 'T110'.
*       2) Datos de dirección de la tienda
        SELECT SINGLE *
          FROM adrc
          INTO wa_adrc_t110
         WHERE addrnumber = wa_t001w_t110-adrnr.
*       3) Denominación de la región de la tienda
        select single bezei
          from t005u
          into ld_region_tda_t_t110
         where spras = sy-langu
           and land1 = wa_adrc_t110-country
           and bland = wa_adrc_t110-region.

*       Rellenamos etiqueta siguiendo la misma logica que se sigue en Z_ESHOP_ORDER_CREATE
*       para recogida en tienda
        CONCATENATE wa_adrc_t110-name1  wa_adrc_t110-name2  into  lr_etiqueta-destinatario_devolucion-NOMBRE SEPARATED BY space.
        CONCATENATE wa_adrc_t110-str_suppl1  wa_adrc_t110-str_suppl2  wa_adrc_t110-street  wa_adrc_t110-str_suppl3 INTO lr_etiqueta-destinatario_devolucion-DIRECCION.
        lr_etiqueta-destinatario_devolucion-COD_PROVINCIA    = wa_adrc_t110-region.
        lr_etiqueta-destinatario_devolucion-NOMBRE_PROVINCIA = ld_region_tda_t_t110.
        lr_etiqueta-destinatario_devolucion-LOCALIDAD        = wa_adrc_t110-city1.
        lr_etiqueta-destinatario_devolucion-COD_POSTAL       = wa_adrc_t110-post_code1.
      else.
*       Si el país del destinatario no es suiza, entonces registramos en la
*       etiqueta los mismos datos que en la etiqueta remitente
        lr_etiqueta-destinatario_devolucion-NOMBRE           = 'CAMPER'.
        lr_etiqueta-destinatario_devolucion-DIRECCION        = wa_adrc_tda-street.
        lr_etiqueta-destinatario_devolucion-COD_PROVINCIA    = wa_adrc_tda-region.
        lr_etiqueta-destinatario_devolucion-NOMBRE_PROVINCIA = ld_region_tda_t.
        lr_etiqueta-destinatario_devolucion-LOCALIDAD        = wa_adrc_tda-city1.
        lr_etiqueta-destinatario_devolucion-COD_POSTAL       = wa_adrc_tda-post_code1.
      endif.
    endif.
*   APRADAS-Fin-28.06.2017


*   Etiqueta: Productos
    loop at lit_vbap.
      lr_item-sku = lit_vbap-matnr.
      lr_item-opcion = lit_vbap-pmatn.

      append lr_item to lr_etiqueta-productos-item.
    endloop.

    append lr_etiqueta to lr_genera_etiquetas_req-ETIQUETA.
  endloop.


  TRY.
    CALL METHOD CL_LOGISTICA->GENERA_ETIQUETAS
      EXPORTING
        GENERA_ETIQUETAS          = lr_genera_etiquetas_req
      IMPORTING
        GENERA_ETIQUETAS_RESPONSE = PS_ETIQUETA_RESP
        .
      .
   CATCH cx_ai_system_fault into lo_sys_exception .
     ld_proxy_error = lo_sys_exception->get_text( ).

*     MsgError: Proxy Error: &
      CALL FUNCTION 'Z_B2C_CREAR_ERROR_B2C'
          EXPORTING
            i_tipo   = 'E'
            i_codigo = '758'
            MESSAGE_V1 = ld_proxy_error
*           MESSAGE_V2 =
*           MESSAGE_V3 =
*           MESSAGE_V4 =
*           PARAMETER  =
*           ROW      =
*           FIELD    =
          TABLES
            it_error = it_error.

  ENDTRY.

  loop at PS_ETIQUETA_RESP-ETIQUETA into lr_etiqueta_resp where error is not initial.


*   MsgError: Proxy Error: &
    CALL FUNCTION 'Z_B2C_CREAR_ERROR_B2C'
        EXPORTING
          i_tipo   = 'E'
          i_codigo = '758'
          MESSAGE_V1 = lr_etiqueta_resp-error
*         MESSAGE_V2 =
*         MESSAGE_V3 =
*         MESSAGE_V4 =
*         PARAMETER  =
*         ROW      =
*         FIELD    =
        TABLES
          it_error = it_error.
  endloop.

ENDFUNCTION.