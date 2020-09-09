*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

**********************************************************************
* Define global attributes
**********************************************************************
CLASS lcl_buffer DEFINITION.

  PUBLIC SECTION.

    TYPES: BEGIN OF ty_buffer.
             INCLUDE TYPE ztbooking AS data.
    TYPES:   flag TYPE c LENGTH 1,
           END OF ty_buffer.

    TYPES: tt_bookings TYPE SORTED TABLE OF ty_buffer
           WITH UNIQUE KEY booking.

    CLASS-DATA: mt_buffer TYPE tt_bookings.

ENDCLASS.

**********************************************************************
* Implement operations
**********************************************************************
CLASS lcl_handler DEFINITION FINAL
INHERITING FROM cl_abap_behavior_handler.

  PRIVATE SECTION.

    METHODS: modify FOR BEHAVIOR
      IMPORTING
        roots_to_create FOR CREATE booking
        roots_to_update FOR UPDATE booking
        roots_to_delete FOR DELETE booking.

    METHODS: read FOR BEHAVIOR
      IMPORTING
        it_booking_key FOR READ booking RESULT et_booking.

    METHODS: lock FOR BEHAVIOR
      IMPORTING
        it_booking_key FOR LOCK booking.

ENDCLASS.

CLASS lcl_handler IMPLEMENTATION.

  METHOD lock.
* Provide the appropriate lock handling if required
  ENDMETHOD.

  METHOD modify.

    LOOP AT roots_to_delete INTO DATA(ls_delete).

* %CID = Control field
      IF ls_delete-booking IS INITIAL.
        ls_delete-booking = mapped-booking[ %cid = ls_delete-%cid_ref ]-booking.
      ENDIF.

      READ TABLE lcl_buffer=>mt_buffer ASSIGNING FIELD-SYMBOL(<ls_buffer>)
      WITH KEY booking = ls_delete-booking.

      IF sy-subrc IS INITIAL.
        IF <ls_buffer>-flag = 'C'.
          DELETE TABLE lcl_buffer=>mt_buffer
          WITH TABLE KEY booking = ls_delete-booking.
        ELSE.
          <ls_buffer>-flag = 'D'.
        ENDIF.
      ELSE.
        INSERT VALUE #( flag = 'D' booking = ls_delete-booking )
        INTO TABLE lcl_buffer=>mt_buffer.
      ENDIF.

    ENDLOOP.

* Handle creation process
    IF roots_to_create IS NOT INITIAL.
      SELECT SINGLE MAX( booking )
      FROM ztbooking
      INTO @DATA(lv_max_booking).
    ENDIF.

    LOOP AT roots_to_create INTO DATA(ls_create).

      ADD 1 TO lv_max_booking.
      ls_create-%data-booking = lv_max_booking.
      GET TIME STAMP FIELD DATA(zv_tsl).
      ls_create-%data-lastchangedat = zv_tsl.

      INSERT VALUE #( flag = 'C' data = CORRESPONDING #( ls_create-%data ) )
      INTO TABLE lcl_buffer=>mt_buffer.

      IF ls_create-%cid IS NOT INITIAL.
        INSERT VALUE #( %cid = ls_create-%cid booking = ls_create-booking )
        INTO TABLE mapped-booking.
      ENDIF.

    ENDLOOP.

* Handle update operations
    IF roots_to_update IS NOT INITIAL.
      LOOP AT roots_to_update INTO DATA(ls_update).

        IF ls_update-booking IS INITIAL.
          ls_update-booking = mapped-booking[ %cid = ls_update-%cid_ref ]-booking.
        ENDIF.

        READ TABLE lcl_buffer=>mt_buffer ASSIGNING <ls_buffer>
        WITH KEY booking = ls_update-booking.
        IF sy-subrc <> 0.

          SELECT SINGLE *
          FROM ztbooking
          WHERE booking = @ls_update-booking
          INTO @DATA(ls_db).

          INSERT VALUE #( flag = 'U' data = ls_db )
          INTO TABLE lcl_buffer=>mt_buffer ASSIGNING <ls_buffer>.

        ENDIF.

        IF ls_update-%control-customername IS NOT INITIAL.
          <ls_buffer>-customername = ls_update-customername.
        ENDIF.

        IF ls_update-%control-cost  IS NOT INITIAL.
          <ls_buffer>-cost = ls_update-cost.
        ENDIF.

        IF ls_update-%control-dateoftravel   IS NOT INITIAL.
          <ls_buffer>-dateoftravel  = ls_update-dateoftravel .
        ENDIF.

        IF ls_update-%control-currencycode  IS NOT INITIAL.
          <ls_buffer>-currencycode = ls_update-currencycode.
        ENDIF.

        GET TIME STAMP FIELD DATA(zv_tsl2).
        <ls_buffer>-lastchangedat = zv_tsl2.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD read.

    LOOP AT it_booking_key INTO DATA(ls_booking_key).

* Check if it is in the buffer (and not deleted).
      READ TABLE lcl_buffer=>mt_buffer INTO DATA(ls_booking)
      WITH KEY booking = ls_booking_key-booking .

      IF sy-subrc IS INITIAL
      AND ls_booking-flag NE 'U'.
        INSERT CORRESPONDING #( ls_booking-data )
        INTO TABLE et_booking.
      ELSE.
        SELECT SINGLE *
        FROM ztbooking
        WHERE booking = @ls_booking_key-booking
        INTO @DATA(ls_db).

        IF sy-subrc IS INITIAL.
          INSERT CORRESPONDING #( ls_db )
          INTO TABLE et_booking.
        ELSE.
          INSERT VALUE #( booking = ls_booking_key-booking )
          INTO TABLE failed-booking.
        ENDIF.

      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.

**********************************************************************
* Redefine events
**********************************************************************
CLASS lcl_saver DEFINITION INHERITING FROM cl_abap_behavior_saver.

  PROTECTED SECTION.

    METHODS: finalize REDEFINITION.

    METHODS: check_before_save REDEFINITION.

    METHODS: save REDEFINITION.

ENDCLASS.

CLASS lcl_saver IMPLEMENTATION.

  METHOD check_before_save.
* Event redefinition
  ENDMETHOD.

  METHOD finalize.
* Event redefinition
  ENDMETHOD.

  METHOD save.

    DATA lt_data TYPE STANDARD TABLE OF ztbooking.

* Create operation
    lt_data = VALUE #(  FOR row IN lcl_buffer=>mt_buffer WHERE  ( flag = 'C' ) ( row-data ) ).
    IF lt_data IS NOT INITIAL.
      INSERT ztbooking FROM TABLE @lt_data.
    ENDIF.

* Update operation
    lt_data = VALUE #(  FOR row IN lcl_buffer=>mt_buffer WHERE  ( flag = 'U' ) ( row-data ) ).
    IF lt_data IS NOT INITIAL.
      UPDATE ztbooking FROM TABLE @lt_data.
    ENDIF.

* Delete operation
    lt_data = VALUE #(  FOR row IN lcl_buffer=>mt_buffer WHERE  ( flag = 'D' ) ( row-data ) ).
    IF lt_data IS NOT INITIAL.
      DELETE ztbooking FROM TABLE @lt_data.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
