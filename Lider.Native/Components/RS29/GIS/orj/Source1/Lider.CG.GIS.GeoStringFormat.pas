//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  String formatting utilities.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoStringFormat ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoStringFormat"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.Variants ;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.Text,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF COCOA}
  uses
    TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   String formatting utilities.
  /// </summary>
  TGIS_StringFormat = {$IFDEF OXYGENE} public {$ENDIF} class
     private
       {$IFDEF JAVA}
         /// <summary>
         ///   Format provided value according to a format string for
         ///   date/time. Detailed formatting is described in a documentation
         ///   for TGIS_StringFormat.Format.
         /// </summary>
         /// <param name="_format">
         ///   format string
         /// </param>
         /// <param name="_value">
         ///   value to be formatted
         /// </param>
         /// <returns>
         ///   Formatted string
         /// </returns>
         class function parseDateTime( const _format : String ;
                                       const _value  : TDateTime
                                     ) : String ;
                                     {$IFDEF GIS_STATIC} static; {$ENDIF}
       {$ENDIF}
       {$IFDEF DCC}
         /// <summary>
         ///   Format provided value according to a format string for
         ///   date/time. Detailed formatting is described in a documentation
         ///   for TGIS_StringFormat.Format.
         /// </summary>
         /// <param name="_format">
         ///   format string
         /// </param>
         /// <param name="_value">
         ///   value to be formatted
         /// </param>
         /// <returns>
         ///   Formatted string
         /// </returns>
         class function parseDateTime( const _format : String ;
                                       const _value  : TDateTime
                                     ) : String ;
                                     {$IFDEF GIS_STATIC} static; {$ENDIF}
       {$ENDIF}
       {$IFDEF JAVA}  
         /// <summary>
         ///   Format provided value according to a format string for Numbers.
         ///   Detailed formatting is described in a documentation for
         ///   TGIS_StringFormat.Format.
         /// </summary>
         /// <param name="_format">
         ///   format string
         /// </param>
         /// <param name="_value">
         ///   value to be formatted
         /// </param>
         /// <returns>
         ///   Formatted string
         /// </returns>
         class function parseNumber  ( const _format : String ;
                                        const _value : Variant
                                     ) : String ;
                                     {$IFDEF GIS_STATIC} static; {$ENDIF}
       {$ENDIF}
       {$IFDEF ISLAND}  
         /// <summary>
         ///   Format provided value according to a format string for Numbers.
         ///   Detailed formatting is described in a documentation for
         ///   TGIS_StringFormat.Format.
         /// </summary>
         /// <param name="_format">
         ///   format string
         /// </param>
         /// <param name="_value">
         ///   value to be formatted
         /// </param>
         /// <returns>
         ///   Formatted string
         /// </returns>
         class function parseNumber  ( const _format : String ;
                                        const _value : Variant
                                     ) : String ;
                                     {$IFDEF GIS_STATIC} static; {$ENDIF}
       {$ENDIF}
       {$IFDEF DCC}  
         /// <summary>
         ///   Format provided value according to a format string for Numbers.
         ///   Detailed formatting is described in a documentation for
         ///   TGIS_StringFormat.Format.
         /// </summary>
         /// <param name="_format">
         ///   format string
         /// </param>
         /// <param name="_value">
         ///   value to be formatted
         /// </param>
         /// <returns>
         ///   Formatted string
         /// </returns>
         class function parseNumber  ( const _format : String ;
                                        const _value : Variant
                                     ) : String ;
                                     {$IFDEF GIS_STATIC} static; {$ENDIF}
       {$ENDIF}
       /// <summary>
       ///   Format provided value according to a format string for in a
       ///   documentation for TGIS_StringFormat.Format.
       /// </summary>
       /// <param name="_format">
       ///   format string
       /// </param>
       /// <param name="_value">
       ///   value to be formatted
       /// </param>
       /// <returns>
       ///   Formatted string
       /// </returns>
       class function parseString  ( const _format : String ;
                                     const _value  : String
                                   ) : String ;
                                   {$IFDEF GIS_STATIC} static; {$ENDIF}

       /// <summary>
       ///   Format provided value according to a format string for Booleans.
       ///   Detailed formatting is described in a documentation for
       ///   TGIS_StringFormat.Format.
       /// </summary>
       /// <param name="_format">
       ///   format string
       /// </param>
       /// <param name="_value">
       ///   value to be formatted
       /// </param>
       /// <returns>
       ///   Formatted string
       /// </returns>
       class function parseBoolean ( const _format : String ;
                                     const _value  : Boolean
                                   ) : String ;
                                   {$IFDEF GIS_STATIC} static; {$ENDIF}
     public

       /// <summary>
       ///   Format provided value according to a format string.
       /// </summary>
       /// <param name="_format">
       ///   format string
       /// </param>
       /// <param name="_value">
       ///   value to be formatted
       /// </param>
       /// <returns>
       ///   Formatted string
       /// </returns>
       class function Format       ( const _format : String ;
                                     const _value  : Variant
                                   ) : String ;
                                   {$IFDEF GIS_STATIC} static; {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,
    System.DateUtils,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

const
  MAX_STR = 1024 ;

{$IFNDEF OXYGENE}

  class function TGIS_StringFormat.parseNumber(
    const _format : String ;
    const _value  : Variant
  ) : String ;
  var
    res              : TStringBuilder ;
    sfmt             : String  ; // format string
    fval             : Double  ; // value
    cval_sign        : Char    ; // sign of the value
    sval_integral    : String  ; // integral part of the value
    sval_decimal     : String  ; // decimal part of the value
    bval_significant : Boolean ; // if intergral part non zero
    ifmt_integral    : Integer ; // number of intergral digits
    ifmt_zeropos     : Integer ; // position of last '0' position
    ifmt_decimal     : Integer ; // number of decimal places

    // Parse custom format string.
    procedure prepare_fmt(
      _custom_format : String
    ) ;
    var
      i           : Integer ;
      isdecimal   : Boolean ;
      isbackslash : Boolean ;
    begin
      sfmt := _custom_format ;

      // count digits in a format before and after decimal point
      ifmt_integral := 0 ;
      ifmt_decimal  := 0 ;
      ifmt_zeropos  := MAX_STR ;
      isdecimal     := False ;
      isbackslash   := False ;
      for i := 1 to length( sfmt ) do begin
        if isbackslash then begin
          isbackslash := False ; // just ignore
        end
        else begin
          case sfmt[i] of
            '#' : begin
                    if isdecimal then inc( ifmt_decimal  )
                                 else inc( ifmt_integral ) ;
                  end;
            '0' : begin
                    if isdecimal then inc( ifmt_decimal  )
                                 else inc( ifmt_integral ) ;
                    if ifmt_zeropos = MAX_STR then
                      ifmt_zeropos := i ;
                  end ;
            '.',
            ',' : begin
                    isdecimal := True ;
                  end ;
            '\' : begin
                    isbackslash := True ;
                  end ;
          end ;
        end ;

      end ;
    end ;

    // Parse value - extract integral and decimal part.
    procedure prepare_val(
      const _custom_value : Variant ;
      const _abs          : Boolean
    ) ;
    var
      i        : Integer ;
      txt      : String  ;
      idecimal : Integer ;
      isign    : Integer ;
      tmp      : Double ;
    begin

      if _abs then
        tmp := Abs( Double( _custom_value ) )
      else
        tmp := Double( _custom_value ) ;

      fval := RoundTo( tmp, -ifmt_decimal )  ;

      if VarType( _value ) = varDouble then
        txt := System.SysUtils.Format( '%.15f', [ fval ] )
      else
        txt := FloatToStr( RoundS( tmp ) ) + '.00000000000000000' ;

      isign    := 0 ;
      idecimal := MAX_STR ;
      for i := 1 to length( txt ) do begin
        case txt[i] of
          '+' ,
          '-' : begin
                  isign := i ;
                end ;
          ',' ,
          '.' : begin
                  idecimal := i ;
                  break ;
                end ;
        end ;
      end ;

      if isign > 0 then
        cval_sign := txt[ isign ]
      else
        cval_sign := #0 ;

      sval_integral := Copy( txt, isign    + 1, idecimal - isign - 1 ) ;
      sval_decimal  := Copy( txt, idecimal + 1, Min( MAX_STR, ifmt_decimal ) ) ;
      bval_significant := sval_integral <> '0' ;
    end ;

    // parse custom number formatting
    function do_custom(
      const _custom_format : String  ;
      const _custom_value  : Variant ;
      const _abs           : Boolean
    ) : String ;
    var
      i             : Integer ;
      isdecimal     : Boolean ;
      isbackslash   : Boolean ;
      c  : Char ;
      cd : Char ;

      ar_output     : array of Char ;
      cnt_output    : Integer ;

      bsignificant  : Boolean ;
      cnt_placement : Integer ; // number of pending placements
      cnt_decimal   : Integer ; // number of pending integral digits
      cnt_integral  : Integer ; // number of pending decimal digits

      function next_digit : Char ;
      begin
        if      cnt_decimal  > 0 then begin
                  Result := sval_decimal[ cnt_decimal ] ;
                  dec( cnt_decimal ) ;
                  isdecimal := cnt_decimal > 0 ;
                end
        else if cnt_integral > 0 then begin
                  Result := sval_integral[ cnt_integral ] ;
                  dec( cnt_integral ) ;
                end
        else    begin
                  Result := #0 ;
                end ;
      end;

      procedure to_result( const _c : Char ) ;
      begin
        if _c = #0 then exit ;

        inc( cnt_output ) ;
        ar_output[ cnt_output ] := _c ;
      end ;

    begin
      prepare_fmt( _custom_format ) ;
      prepare_val( _custom_value  , _abs ) ;

      SetLength( ar_output, MAX_STR ) ;
      cnt_output := -1 ;

      cnt_integral := length( sval_integral ) ;
      cnt_decimal  := length( sval_decimal  ) ;

      isdecimal    := ifmt_decimal > 0 ;
      bsignificant := False ;
      isbackslash  := False ;

      cnt_placement := ifmt_integral + ifmt_decimal ;

      for i := length( sfmt ) downto 1 do begin
        c := sfmt[i] ;

        if ( i > 1 ) and ( sfmt[i-1] = '\' ) then begin
          to_result( c ) ;
        end
        else begin
          case c of
            '#' : begin
                    if isdecimal then begin
                     cd := next_digit ;
                      if ( cd <> '0' ) or bsignificant then begin
                        bsignificant := True ;
                        to_result( cd ) ;
                      end ;
                    end
                    else begin
                      cd := next_digit ;
                      if ( cd = #0 ) and ( i > ifmt_zeropos ) then
                        cd := '0' ;

                      // avoid output of '0' like string
                      if bsignificant or bval_significant then
                        to_result( cd ) ;
                    end ;

                    dec( cnt_placement ) ;
                  end ;
            '0' : begin
                    if isdecimal then begin
                      bsignificant := True ;
                      cd := next_digit ;
                      to_result( cd ) ;
                    end
                    else begin
                      cd := next_digit ;
                      if cd = #0 then
                        cd := '0' ;
                      to_result( cd ) ;
                    end ;

                    dec( cnt_placement ) ;
                  end ;
            '.' : begin
                    if bsignificant then
                      to_result( '.' ) ;
                  end ;
            '\' : begin
                    // ignore ;
                  end
            else  begin
                    to_result( c ) ;
                  end ;
          end ;
        end ;

        // post all pending digits to avoid truncating of the beginning of the
        // number
        if ( cnt_placement = 0 ) and
           ( ( ifmt_integral + ifmt_decimal ) > 0 ) then
        begin
          while True do begin
            cd := next_digit ;
            if cd = #0 then break ;
            to_result( cd ) ;
          end ;
          to_result( cval_sign ) ;
          dec( cnt_placement ) ;
        end ;
      end ;

      // rewrite output buffer to the string
      res := TStringBuilder.Create ;
      try
        for i:=1 to cnt_output + 1 do begin
          res.Append(  ar_output[ cnt_output ]  ) ;
          dec( cnt_output ) ;
        end ;

        Result := res.ToString ;
      finally
        FreeObject( res ) ;
      end ;
    end ;

    // parse number format string; if custom - forward to do_custom
    function do_parse( const _format : String; _value : Variant ) :String ;
    var
      i      : Integer ;
      c      : Char ;
      state  : Integer ;
      mode   : Integer ;
      prec   : Integer ;

      ipart2 : Integer ;
      ipart3 : Integer ;

      itmp1  : Int64 ;
      stmp1  : String  ;
      stmp2  : String  ;

    const
      ST_START = 1;
      ST_NUMBER_START = 2;
      ST_NUMBER_COLLECT = 3;
      ST_PART1 = 4 ;
      ST_PART2 = 5 ;
      ST_PART3 = 6 ;

      MD_NONE = 0 ;
      MD_CURRENCY = 1 ;
      MD_DECIMAL = 2 ;
      MD_SCIENTIFIC_UPPERCASE  = 3 ;
      MD_SCIENTIFIC_LOWERCASE  = 4 ;
      MD_FIXED = 5 ;
      MD_GENERAL_UPPERCASE = 6 ;
      MD_GENERAL_LOWERCASE = 7 ;
      MD_NUMBER = 8 ;
      MD_PERCENT = 9 ;
      MD_ROUNDTRIP = 10 ;
      MD_HEXADECIMAL_UPPERCASE = 11 ;
      MD_HEXADECIMAL_LOWERCASE = 12 ;

      function general_precision : Integer ;
      begin
        case VarType( _value ) of
          varWord      : Result :=  5 ;
          varLongWord  : Result := 10 ;
          varByte      : Result :=  3 ;
          varCurrency  : Result := 15 ;
          varShortInt  : Result :=  3 ;
          varSmallInt  : Result :=  5 ;
          varInteger   : Result := 10 ;
          varInt64     : Result := 19 ;
          varSingle    : Result :=  7 ;
          varDouble    : Result := 15 ;
          else           Result := 10 ;
        end;
      end;

      function is_integral : Boolean ;
      begin
        case VarType( _value ) of
          varWord      : Result := True  ;
          varLongWord  : Result := True  ;
          varByte      : Result := True  ;
          varCurrency  : Result := False ;
          varShortInt  : Result := True  ;
          varSmallInt  : Result := True  ;
          varInteger   : Result := True  ;
          varInt64     : Result := True  ;
          varSingle    : Result := False ;
          varDouble    : Result := False ;
          else           Result := False ;
        end;
      end;

    begin
      i := 1 ;

      ipart2 := MAX_STR ;
      ipart3 := MAX_STR ;

      mode := MD_NONE ;
      prec := 0 ;
      state := ST_START ;
      while i <= length( _format  ) do begin
        c := _format[i] ;
        case state of
          ST_START :
            begin
              case c of
                'C',
                'c' : begin
                        prec := 2 ;
                        mode := MD_CURRENCY ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'D',
                'd' : begin
                        mode := MD_DECIMAL ;
                        prec := 0 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'E' : begin
                        mode := MD_SCIENTIFIC_UPPERCASE ;
                        prec := 6 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'e' : begin
                        mode := MD_SCIENTIFIC_LOWERCASE ;
                        prec := 6 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'F',
                'f' : begin
                        mode := MD_FIXED ;
                        prec := 2 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'G' : begin
                        mode := MD_GENERAL_UPPERCASE ;
                        prec := general_precision ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'g' : begin
                        mode := MD_GENERAL_LOWERCASE ;
                        prec := general_precision ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'N',
                'n' : begin
                        mode := MD_NUMBER ;
                        prec := 2 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'P',
                'p' : begin
                        mode := MD_PERCENT ;
                        prec := 2 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'R',
                'r' : begin
                        mode := MD_ROUNDTRIP ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'X' : begin
                        mode := MD_HEXADECIMAL_UPPERCASE ;
                        prec := 0 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                'x' : begin
                        mode := MD_HEXADECIMAL_LOWERCASE ;
                        prec := 0 ;
                        inc( i ) ;
                        state := ST_NUMBER_START ;
                      end ;
                else  begin
                        state := ST_PART1 ;
                      end ;
              end ;
            end ;
          ST_NUMBER_START :
            begin
              if ( c >= '0' ) and ( c <= '9' ) then begin
                prec := 0 ;
                state := ST_NUMBER_COLLECT ;
              end
              else
                state := ST_PART1 ;
            end;
          ST_NUMBER_COLLECT :
            begin
              if ( c >= '0' ) and ( c <= '9' ) then begin
                prec := prec * 10 + ( ord( c ) - ord( '0' ) ) ;
                inc( i ) ;
              end
              else
                state := ST_PART1 ;
            end;
          ST_PART1 :
            begin
              mode := MD_NONE ;
              if c = ';' then begin
                ipart2 := i + 1 ;
                state := ST_PART2 ;
              end ;
              inc( i ) ;
            end;
          ST_PART2 :
            begin
              mode := MD_NONE ;
              if c = ';' then begin
                ipart3 := i + 1 ;
                state := ST_PART2 ;
              end ;
              inc( i ) ;
            end;
          else
            begin
              assert( False, GIS_RS_ERR_UNTESTED ) ;
            end;
        end ;
      end;

      case mode of
        MD_NONE :
          begin
            if ipart3 < MAX_STR then begin // zero part exists
              if      Double(_value) > 0 then begin
                        Result := do_custom(
                                    Copy( _format, 1, ipart2-2 ),
                                    _value,
                                    False
                                  ) ;
                      end
              else if Double(_value) < 0 then begin
                        Result := do_custom(
                                    Copy( _format, ipart2, ipart3-ipart2-1 ),
                                    _value,
                                    True
                                  ) ;
                      end
              else    begin
                        Result := do_custom(
                                    Copy( _format, ipart3, MAX_STR ),
                                    0,
                                    False
                                  ) ;
                      end;
            end
            else if ipart2 < MAX_STR then begin // minus part
              if      Double(_value) >= 0 then begin
                        Result := do_custom(
                                    Copy( _format, 1, ipart2-2 ),
                                    _value,
                                    False
                                  ) ;
                      end
              else    begin
                        Result := do_custom(
                                    Copy( _format, ipart2, MAX_STR ),
                                    _value,
                                    False
                                  ) ;
                      end
            end
            else
              Result := do_custom( _format, _value, False ) ;
          end ;
        MD_CURRENCY :
          begin
            Result := FloatToStrF( Double( _value ), ffCurrency, 100, prec ) ;
          end ;
        MD_DECIMAL :
          begin
            itmp1 := Int64( _value ) ; // force Int64
            Result := IntToStr( itmp1 ) ;
            stmp1 := IntToStr( Abs( itmp1 ) ) ;
            if prec > length( stmp1 ) then begin
              stmp2 := IntToHex(0, prec) ;
              if itmp1 < 0 then
                Result := '-' + Copy( stmp2, 1, prec - length( stmp1 ) ) + stmp1
              else
                Result := Copy( stmp2, 1, prec - length( stmp1 ) )  + stmp1 ;
            end ;
          end ;
        MD_SCIENTIFIC_UPPERCASE :
          begin
            Result := UpperCase( FloatToStrF( Double( _value ), ffExponent,prec+1, 3 ) ) ;
          end ;
        MD_SCIENTIFIC_LOWERCASE :
          begin
            Result := LowerCase( FloatToStrF( Double( _value ), ffExponent,prec+1, 3 ) ) ;
          end ;
        MD_FIXED :
          begin
            Result := FloatToStrF( Double( _value ), ffFixed,100, prec ) ;
          end ;
        MD_GENERAL_UPPERCASE :
          begin
            if is_integral then begin
              itmp1 := Int64( _value ) ;
              Result := IntToStr( itmp1 ) ;
              if length( Result ) > prec then
                Result := UpperCase( FloatToStrF( itmp1, ffGeneral, prec, 3 ) ) ;
            end
            else
              Result := UpperCase( FloatToStrF( Double( _value ), ffGeneral, prec, 3 ) ) ;
          end ;
        MD_GENERAL_LOWERCASE :
          begin
            if is_integral then begin
              itmp1 := Int64( _value ) ; //? ;
              Result := IntToStr( itmp1 ) ;
              if length( Result ) > prec then
                Result := LowerCase( FloatToStrF( itmp1, ffGeneral, prec, 3 ) ) ;
            end
            else
              Result := LowerCase( FloatToStrF( Double( _value ), ffGeneral, prec, 3 ) ) ;
          end ;
        MD_NUMBER :
          begin
            Result := FloatToStrF( Double( _value ), ffNumber, 100, prec ) ;
          end ;
        MD_PERCENT :
          begin
            Result := FloatToStrF( Double( _value ) * 100, ffNumber, 100, prec ) + '%';
          end ;
        MD_ROUNDTRIP :
          begin
            Result := FloatToStr( Double( _value ) ) ;
          end ;
        MD_HEXADECIMAL_UPPERCASE :
          begin
            itmp1 := Int64( _value ) ; //?
            Result := UpperCase( IntToHex( itmp1, prec ) ) ;
          end ;
        MD_HEXADECIMAL_LOWERCASE :
          begin
            itmp1 := Int64( _value ) ; //?
            Result := LowerCase( IntToHex( itmp1, prec ) ) ;
          end ;
        else
          begin
            assert( False, GIS_RS_ERR_UNTESTED ) ;
          end;
      end;
    end;
  begin
    Result := do_parse( _format, _value ) ;
  end;

  class function TGIS_StringFormat.parseDateTime(
    const _format : String    ;
    const _value  : TDateTime
  ) : String ;
  var
    i    : Integer ;
    mode : Integer ;
    cnt  : Integer ;
    year,
    month,
    day,
    hour,
    min,
    sec,
    msec : WORD ;
    res  : String  ;
  const
    MD_NONE              =  0 ;
    MD_DAY               =  1 ;
    MD_FRACTION          =  2 ;
    MD_FRACTION_OPTIONAL =  3 ;
    MD_ERA               =  4 ;
    MD_HOUR12            =  5 ;
    MD_HOUR24            =  6 ;
    MD_KIND              =  7 ;
    MD_MINUTE            =  8 ;
    MD_MONTH             =  9 ;
    MD_SECOND            = 10 ;
    MD_AM                = 11 ;
    MD_YEAR              = 12 ;
    MD_ZONE              = 13 ;

    procedure set_day ;
    begin
      case cnt of
        1 :   res := res + FormatDateTime( 'd'      , _value ) ;
        2 :   res := res + FormatDateTime( 'dd'     , _value ) ;
        3 :   res := res + FormatDateTime( 'ddd'    , _value ) ;
        else  res := res + FormatDateTime( 'dddd'   , _value ) ;
      end;
    end ;

    procedure set_fraction ;
    var
      s : String ;
    begin
      s := IntToStr( msec) + '0000000' ;
      case cnt of
        1 :   res := res + Copy( s, 1, 1 ) ;
        2 :   res := res + Copy( s, 1, 2 ) ;
        3 :   res := res + Copy( s, 1, 3 ) ;
        4 :   res := res + Copy( s, 1, 4 ) ;
        5 :   res := res + Copy( s, 1, 5 ) ;
        6 :   res := res + Copy( s, 1, 6 ) ;
        else  res := res + Copy( s, 1, 7 ) ;
      end;
    end;

    procedure set_fraction_optional ;
    var
      s : String ;
    begin
      s := IntToStr( msec ) ;
      case cnt of
        1 :   res := res + Copy( s, 1, 1 ) ;
        2 :   res := res + Copy( s, 1, 2 ) ;
        3 :   res := res + Copy( s, 1, 3 ) ;
        4 :   res := res + Copy( s, 1, 4 ) ;
        5 :   res := res + Copy( s, 1, 5 ) ;
        6 :   res := res + Copy( s, 1, 6 ) ;
        else  res := res + Copy( s, 1, 7 ) ;
      end;
    end;

    procedure set_hour12 ;
    var
      h : Integer ;
      s : String ;
    begin
      if      hour > 12 then h := hour - 12
      else if hour = 0  then h := 12
      else                   h := hour ;

      s := IntToStr( h ) ;
      case cnt of
        1 :   res := res + s ;
        else  begin
                if length( s ) = 1 then
                  res := res + '0' + s
                else
                  res := res + s ;
              end ;
      end;
    end;

    procedure set_hour24 ;
    var
      s : String ;
    begin
      s := IntToStr( hour ) ;
      case cnt of
        1 :   res := res + s ;
        else  begin
                if length( s ) = 1 then
                  res := res + '0' + s
                else
                  res := res + s ;
              end ;
      end;
    end;

    procedure set_minute ;
    begin
      case cnt of
        1 :   res := res + FormatDateTime( 'n'      , _value ) ;
        else  res := res + FormatDateTime( 'nn'     , _value ) ;
      end ;
    end;

    procedure set_month ;
    begin
      case cnt of
        1 :   res := res + FormatDateTime( 'm'      , _value ) ;
        2 :   res := res + FormatDateTime( 'mm'     , _value ) ;
        3 :   res := res + FormatDateTime( 'mmm'    , _value ) ;
        else  res := res + FormatDateTime( 'mmmm'   , _value ) ;
      end;
    end;

    procedure set_second ;
    begin
      case cnt of
        1 :   res := res + FormatDateTime( 's'      , _value ) ;
        else  res := res + FormatDateTime( 'ss'     , _value ) ;
      end ;
    end;

    procedure set_am ;
    var
      s : String ;
    begin
      s := FormatDateTime( 'ampm', _value ) ;
      case cnt of
        1 :   res := res + Copy( s, 1, 1 ) ;
        else  res := res + s ;
      end ;
    end;

    procedure set_year ;
    var
      s : String ;
    begin
      case cnt of
        1 :   begin
                if year < 10 then
                  res := res + IntToStr( year )
                else if ( year > 2000 ) and ( year < 2010 ) then
                  res := res + IntToStr( year - 2000 )
                else
                  res := res + FormatDateTime( 'y'      , _value ) ;
              end ;
        else  begin
                s := '00000' + IntToStr( year ) ;

                if ( cnt = 3 ) and ( year >= 1000 ) then
                  inc( cnt ) ;

                res := res + Copy( s, length( s ) - cnt + 1, MAX_STR ) ;
              end ;
      end;
    end;

    procedure set_mode( const _mode : Integer ) ;
    begin
      if mode <> _mode then begin
        case mode of
          MD_NONE :
            begin
              // do nothing
            end;
          MD_DAY :
            begin
              set_day ;
            end;
          MD_FRACTION :
            begin
              set_fraction ;
            end;
          MD_FRACTION_OPTIONAL :
            begin
              set_fraction_optional ;
            end;
          MD_ERA :
            begin
              { TODO -cReview : to be implemented }
            end;
          MD_HOUR12 :
            begin
              set_hour12 ;
            end;
          MD_HOUR24 :
            begin
              set_hour24 ;
            end;
          MD_KIND :
            begin
              { TODO -cReview : to be implemented }
            end;
          MD_MINUTE :
            begin
              set_minute ;
            end;
          MD_MONTH :
            begin
              set_month ;
            end;
          MD_SECOND :
            begin
              set_second ;
            end;
          MD_AM :
            begin
              set_am ;
            end;
          MD_YEAR :
            begin
              set_year ;
            end;
          MD_ZONE :
            begin
              { TODO -cReview : to be implemented }
            end;
          else
            begin
              assert( False, GIS_RS_ERR_UNTESTED ) ;
            end;
        end ;
        cnt := 0 ;
      end ;
      mode := _mode ;
      inc( cnt ) ;
    end ;

  begin
    Result := '' ;
    res    := '' ;

    DecodeDate( _value, year, month, day ) ;
    DecodeTime( _value, hour, min, sec, msec ) ;

    mode := MD_NONE ;
    cnt  := 0       ;

    if length( _format ) = 1 then begin
      // parse stanadard
      case _format[1] of
        'd' : begin
                Result :=  FormatDateTime( 'ddddd'      , _value ) ;
                exit ;
              end;
        'D' : begin
                Result :=  FormatDateTime( 'dddddd'     , _value ) ;
                exit ;
              end;
        'f' : begin
                Result :=  FormatDateTime( 'dddddd'     , _value ) + ' ' +
                           FormatDateTime( 't'          , _value ) ;
                exit ;
              end;
        'F' : begin
                Result :=  FormatDateTime( 'dddddd'     , _value ) + ' ' +
                           FormatDateTime( 'tt'         , _value ) ;
                exit ;
              end;
        'g' : begin
                Result :=  FormatDateTime( 'ddddd'      , _value ) + ' ' +
                           FormatDateTime( 't'          , _value ) ;
                exit ;
              end;
        'G' : begin
                Result :=  FormatDateTime( 'ddddd'      , _value ) + ' ' +
                           FormatDateTime( 'tt'         , _value ) ;
                exit ;
              end;
        'm' ,
        'M' : begin
                Result :=  FormatDateTime( 'mmmm dd'    , _value ) ;
                exit ;
              end;
        'o' ,
        'O' : begin
                Result :=  FormatDateTime( 'yyyy-mm-dd', _value ) + 'T' +
                           FormatDateTime( 'hh:nn:ss'  , _value ) + '.' +
                           Copy( IntToStr( msec ) + '0000000', 1, 7 ) ;
                exit ;
              end;
        'r' ,
        'R' : begin
                Result :=  FormatDateTime( 'ddd, dd mmm yyyy hh:nn:ss',
                                           _value
                                         ) + ' GMT' ;
                exit ;
              end;
        's' : begin
                Result :=  FormatDateTime( 'yyyy-mm-dd' , _value ) + 'T' +
                           FormatDateTime( 'hh:mm:ss'   , _value ) ;
                exit ;
              end;
        't' : begin
                Result :=  FormatDateTime( 't'          , _value ) ;
                exit ;
              end;
        'T' : begin
                Result :=  FormatDateTime( 'tt'         , _value ) ;
                exit ;
              end;
        'u' : begin
                Result :=  FormatDateTime( 'yyyy-mm-dd hh:mm:ss',
                                           _value
                                         ) + 'Z' ;
                exit ;
              end;
        'U' : begin
                Result :=  FormatDateTime( 'dddddd tt'   ,
                                            DateTimeToUTC( _value )
                                         ) ;
                exit ;
              end;
        'y' ,
        'Y' : begin
                Result :=  FormatDateTime( 'mmmm, yyyy' , _value ) ;
                exit ;
              end;
      end;
    end ;

    for i:= 1 to length( _format ) do begin
      case _format[i] of
        'd' : set_mode( MD_DAY               ) ;
        'f' : set_mode( MD_FRACTION          ) ;
        'F' : set_mode( MD_FRACTION_OPTIONAL ) ;
        'g' : set_mode( MD_ERA               ) ;
        'h' : set_mode( MD_HOUR12            ) ;
        'H' : set_mode( MD_HOUR24            ) ;
        'K' : set_mode( MD_KIND              ) ;
        'm' : set_mode( MD_MINUTE            ) ;
        'M' : set_mode( MD_MONTH             ) ;
        's' : set_mode( MD_SECOND            ) ;
        't' : set_mode( MD_AM                ) ;
        'y' : set_mode( MD_YEAR              ) ;
        'z' : set_mode( MD_ZONE              ) ;
        else  begin
                set_mode( MD_NONE            ) ;
                res := res + _format[i] ;
              end ;
      end;
    end;
    set_mode( MD_NONE ) ;

    Result := res ;
  end;

{$ENDIF}

  class function TGIS_StringFormat.parseString(
    const _format : String ;
    const _value  : String
  ) : String ;
  var
    i         : Integer ;
    c         : Char    ;
    len       : Integer ;
    isfrom    : Boolean ;
    isplain   : Boolean ;
    from_pos  : Integer ;
    from_sign : Integer ;
    to_pos    : Integer ;
    to_sign   : Integer ;
    bbad      : Boolean ;
    blower    : Boolean ;
    bupper    : Boolean ;

    start     : Integer ;
    count     : Integer ;
  begin
    from_pos  := 0 ;
    from_sign := 1 ;
    to_pos    := 0 ;
    to_sign   := 1 ;

    bbad    := False ;
    bupper  := False ;
    blower  := False ;
    isfrom  := True  ;
    isplain := False ;

    for i := StringFirst to StringLast( _format ) do begin
      c := _format[i] ;
      case c of
        'H' :  begin
                 isplain := True ;
               end ;
        '$' :  begin
                 if isfrom then
                   isfrom := False ;
               end ;
        'S' :  begin
                 bupper := True ;
                 if isfrom then
                   isfrom := False ;
               end ;
        's' :  begin
                 blower := True ;
                 if isfrom then
                   isfrom := False ;
               end ;
        '-' :  begin
                 if isfrom then
                   from_sign := from_sign * -1
                 else
                   to_sign := to_sign * -1
               end
        else   begin
                 if ( c >= '0' ) and ( c <= '9' ) then begin
                   if isfrom then
                     from_pos := from_pos * 10 + ( ord( c ) - ord( '0' ) )
                   else
                     to_pos   := to_pos   * 10 + ( ord( c ) - ord( '0' ) ) ;
                 end
                 else begin
                   // unexpected char
                   bbad := True ;
                   break ;
                 end ;
               end ;
      end;
    end ;

    if bbad then begin
      // unexpected values
      Result := _format ;
      exit ;
    end ;

    if ( from_pos = 0 ) or ( to_pos = 0 ) then begin
      from_pos := 1    ;
      to_pos   := 8192 ;
    end ;

    from_pos := from_sign * from_pos ;
    to_pos   := to_sign   * to_pos   ;

    len := length( _value ) ;

    if from_pos > 0 then
      start := from_pos
    else
      start := len + from_pos  + 1 ;

    if to_pos > 0 then
      count := to_pos - start + 1
    else
      count := ( len + to_pos  + 1 ) - start + 1;

    if start <= 0 then begin
      count := count + start - 1 ;
      start := 1 ;
    end;

    if count <= 0 then begin
      // unexpected values
      Result := '' ;
      exit ;
    end;

    start := start + StringFirst - 1 ;

    if bupper then
      {$IFDEF DCC}
        Result := AnsiUpperCase( Copy( _value, start, count ) )
      {$ELSE}
        Result := UpperCase( Copy( _value, start, count ) )
      {$ENDIF}
    else if blower then
      {$IFDEF DCC}
        Result := AnsiLowerCase( Copy( _value, start, count ) )
      {$ELSE}
        Result := LowerCase( Copy( _value, start, count ) )
      {$ENDIF}
    else
      Result := Copy( _value, start, count ) ;

    if isplain then begin
      Result := StringReplaceAll( Result, '<', '&lt;' ) ;
      Result := StringReplaceAll( Result, '>', '&gt;' ) ;
    end ;
  end ;

  class function TGIS_StringFormat.parseBoolean(
    const _format : String  ;
    const _value  : Boolean
  ) : String ;
  var
    i : Integer ;
    true_from   : Integer ;
    true_cnt    : Integer ;
    false_from  : Integer ;
    false_cnt   : Integer ;
    istrue      : Boolean ;
  begin
    true_from   := -1 ;
    true_cnt    := 0 ;
    false_from  := -1 ;
    false_cnt   := 0 ;

    istrue      := True ;

    for i:= StringFirst to StringLast( _format ) do begin
      case _format[i] of
        ';' :  begin
                 if istrue then
                   istrue := False
                 else begin
                   inc( false_cnt  ) ;
                 end ;
               end
         else  begin
                 if istrue then begin
                   if true_from =  -1 then
                     true_from := i ;
                   inc( true_cnt ) ;
                 end
                 else begin
                   if false_from = -1 then
                     false_from := i ;
                   inc( false_cnt  ) ;
                 end ;
               end ;
       end ;
    end ;

    if _value then
      Result := Copy( _format, true_from , true_cnt  )
    else
      Result := Copy( _format, false_from, false_cnt ) ;
  end ;
 
  {$IFDEF JAVA}
    class function TGIS_StringFormat.parseDateTime(
        const _format : String    ;
        const _value  : TDateTime
      ) : String ;
      var
        cnt  : Integer ;
        len  : Integer ;
        mode: Integer;
        i: Integer;
        res : String ;
        second,
        minute,
        hour,
        day,
        month,
        year: Integer;
        dt: TDateTime;
      const
        MD_NONE              =  0 ;
        MD_DAY               =  1 ;
        MD_FRACTION          =  2 ;
        MD_FRACTION_OPTIONAL =  3 ;
        MD_ERA               =  4 ;
        MD_HOUR12            =  5 ;
        MD_HOUR24            =  6 ;
        MD_KIND              =  7 ;
        MD_MINUTE            =  8 ;
        MD_MONTH             =  9 ;
        MD_SECOND            = 10 ;
        MD_AM                = 11 ;
        MD_YEAR              = 12 ;
        MD_ZONE              = 13 ;
      
      procedure set_day ;
      begin
        case cnt of
          1 :   res := res + 'd' ;
          2 :   res := res + 'dd' ;
          3 :   res := res + 'EEE' ;
          else  res := res + 'EEEE' ;
        end;
      end ;  

      procedure set_hour12 ;
      var
        h : Integer ;
        s : String ;
      begin
        if      hour > 12 then h := hour - 12
        else if hour = 0  then h := 12
        else                   h := hour ;

        s := IntToStr( h ) ;
        case cnt of
          1 :   res := res + s ;
          else  begin
                  if length( s ) = 1 then
                    res := res + '0' + s
                  else
                    res := res + s ;
                end ;
        end;
      end;

      procedure set_hour24 ;
      var
        s : String ;
      begin
        s := IntToStr( hour ) ;
        case cnt of
          1 :   res := res + s ;
          else  begin
                  if length( s ) = 1 then
                    res := res + '0' + s
                  else
                    res := res + s ;
                end ;
        end;
      end;

      procedure set_minute ;
      begin
        case cnt of
          1 :   res := res + 'm' ;
          else  res := res + 'mm' ;
        end ;
      end;

      procedure set_month ;
      begin
        case cnt of
          1 :   res := res + 'M' ;
          2 :   res := res + 'MM' ;
          3 :   res := res + 'MMM' ;
          else  res := res + 'MMMM' ;
        end;
      end;

      procedure set_second ;
      begin
        case cnt of
          1 :   res := res + 's' ;
          else  res := res + 'ss' ;
        end ;
      end;

      procedure set_am ;
      var
        s : String ;
      begin
        s := dt.ToString('a') ;
        case cnt of
          1 :   res := res + QuotedStr( Copy( s, 0, 1 ) ) ;
          else  res := res + QuotedStr(s) ;
        end ;
      end;

      procedure set_year ;
      var
        s : String ;
      begin
        case cnt of
          1 :   begin
                  if year < 10 then
                    res := res + IntToStr( year )
                  else if ( year > 2000 ) and ( year < 2010 ) then
                    res := res + IntToStr( year - 2000 )
                  else
                    res := res + 'y' ; ;
                end ;
          else  begin
                  s := '00000' + IntToStr( year ) ;
                  if ( cnt = 3 ) and ( year >= 1000 ) then
                    inc( cnt ) ;

                  res := res + Copy( s, length( s ) - cnt , MAX_STR ) ;
                end ;
        end;
      end;

      procedure set_mode( const _mode : Integer ) ;
      begin
        if mode <> _mode then begin
          case mode of
            MD_NONE :
              begin
                // do nothing
              end;
            MD_DAY :
              begin
                set_day ;
              end;
            MD_HOUR12 :
              begin
                set_hour12 ;
              end;
            MD_HOUR24 :
              begin
                set_hour24 ;
              end;
            MD_MINUTE :
              begin
                set_minute ;
              end;
            MD_MONTH :
              begin
                set_month ;
              end;
            MD_SECOND :
              begin
                set_second ;
              end;
            MD_AM :
              begin
                set_am ;
              end;
            MD_YEAR :
              begin
                set_year ;
              end;
            MD_ERA :
              begin
              { TODO -cReview : to be implemented }
              end;
            MD_KIND :
              begin
              { TODO -cReview : to be implemented }
              end;
            MD_ZONE :
              begin
              { TODO -cReview : to be implemented }
              end;
          { TODO -cReview : problem with milis in java
            MD_FRACTION :
              begin
                set_fraction ;
              end;
            MD_FRACTION_OPTIONAL :
              begin
                set_fraction_optional ;
              end;}
            else
              begin
                assert( False, GIS_RS_ERR_UNTESTED ) ;
              end;
          end ;
          cnt := 0 ;
        end ;
        mode := _mode ;
        inc( cnt ) ;
      end ; 

    begin
      dt := VarToDateTime( _value ) ;
      second := dt.Second;
      minute := dt.Minute;
      hour := dt.Hour;
      day := dt.Day;
      month := dt.Month;
      year := dt.Year;
      mode := MD_NONE ;
      cnt  := 0       ;
      len := length( _format ) ;

      if len = 1 then begin //standard
        case _format of
          'd' : begin
            Result :=  dt.ToString('M/d/yyyy') ;
            exit ;
          end;
          'D' : begin
            Result :=  dt.ToString('EEEE, MMMM dd, yyyy') ;
            exit ;
          end;
          'f' : begin
            Result :=  dt.ToString('EEEE, MMMM dd, yyyy h:mm a',TimeZone.Local) ;
            exit ;
          end;
          'F' : begin
            Result :=  dt.ToString('EEEE, MMMM dd, yyyy h:mm:ss a',TimeZone.Local) ;
            exit ;
          end;
          'g' : begin
            Result :=  dt.ToString('M/d/yyyy h:mm a',TimeZone.Local) ;
            exit ;
          end;
          'G' : begin
            Result :=  dt.ToString('M/d/yyyy h:mm:ss a',TimeZone.Local) ;
            exit ;
          end;
          'm' ,
          'M' : begin
            Result :=  dt.ToString('MMMM dd') ;
            exit ;
          end;
          { TODO -cReview : no milisseconds on TDateTime }
          //'o' ,
          //'O' : begin
          //  Result :=  dt.ToString('{yyyy}-{MM}-{dd}T{HH}:{mm}:{ss}.') + Copy(IntToStr( msec ) + '0000000', 1, 7);
          //  //Copy( IntToStr( msec ) + '0000000', 1, 7 ) ;
          //  exit ;
          // end;
          'r' ,
          'R' : begin
            Result :=  dt.ToString('EEE, dd MMM yyyy HH:mm:ss ''GMT''',TimeZone.Local);
            exit ;
          end;
          's' : begin
            Result :=  dt.ToString('yyyy-MM-dd''T''HH:mm:ss',TimeZone.Local) ;
            exit ;
          end;
          't' : begin
            Result :=  dt.ToString('h:mm a',TimeZone.Local) ;
            exit ;
          end;
          'T' : begin
            Result :=  dt.ToString('h:mm:ss a',TimeZone.Local) ;
            exit ;
          end;
          'u' : begin
            Result := dt.ToString('yyyy-MM-dd HH:mm:ss''Z''',TimeZone.Local) ;
            exit ;
          end;
          { TODO -cReview : Problem with conversion to UTC }
          // 'U' : begin
          //   Result :=  dt.ToString('{dddd}, {MMMM} {dd}, {yyyy} {h}:{mm}:{ss} {a}');
          //   exit ;
          // end;
          'y' ,
          'Y' : begin
            Result :=  dt.ToString('MMMM, yyyy') ;
            exit ;
          end;
        end;
      end ;
      for i:= 0 to len -1 do begin 
        case _format[i] of
          'd' : set_mode( MD_DAY               ) ;
          'h' : set_mode( MD_HOUR12            ) ;
          'H' : set_mode( MD_HOUR24            ) ;
          'm' : set_mode( MD_MINUTE            ) ;
          'M' : set_mode( MD_MONTH             ) ;
          's' : set_mode( MD_SECOND            ) ;
          't' : set_mode( MD_AM                ) ;
          'y' : set_mode( MD_YEAR              ) ;
          'g' : set_mode( MD_ERA               ) ;
          'K' : set_mode( MD_KIND              ) ;
          'z' : set_mode( MD_ZONE              ) ;
          { TODO -cReview : problem with conversion to UTC }
          //'f' : set_mode( MD_FRACTION          ) ;
          //'F' : set_mode( MD_FRACTION_OPTIONAL ) ;
          else
            begin
              set_mode( MD_NONE            ) ;
              res := res + _format[i] ;
            end ;
        end;
      end;
      set_mode( MD_NONE ) ;
      Result := dt.ToString(res) ;
    end;

    class function TGIS_StringFormat.parseNumber(
      const _format : String ;
      const _value  : Variant
    ) : String ;
    var
      res              : String ;
      sfmt             : String  ; // format string
      fval             : Double  ; // value
      cval_sign        : Char    ; // sign of the value
      sval_integral    : String  ; // integral part of the value
      sval_decimal     : String  ; // decimal part of the value
      bval_significant : Boolean ; // if intergral part non zero
      ifmt_integral    : Integer ; // number of intergral digits
      ifmt_zeropos     : Integer ; // position of last '0' position
      ifmt_decimal     : Integer ; // number of decimal places

      // Parse custom format string.
      procedure prepare_fmt(
        _custom_format : String
      ) ;
      var
        i           : Integer ;
        isdecimal   : Boolean ;
        isbackslash : Boolean ;
      begin
        sfmt := _custom_format ;
        // count digits in a format before and after decimal point
        ifmt_integral := 0 ;
        ifmt_decimal  := 0 ;
        ifmt_zeropos  := MAX_STR ;
        isdecimal     := False ;
        isbackslash   := False ;

        for i := 0 to (sfmt.toString.length -1) do begin

          if isbackslash then begin
            isbackslash := False ; // just ignore
          end
          else begin
            case sfmt[i] of
              '#' : begin
                      if isdecimal then inc( ifmt_decimal  )
                                   else inc( ifmt_integral ) ;
                    end;
              '0' : begin
                      if isdecimal then inc( ifmt_decimal  )
                                   else inc( ifmt_integral ) ;
                      if ifmt_zeropos = MAX_STR then
                        ifmt_zeropos := i ;
                    end ;
              '.',
              ',' : begin
                      isdecimal := True ;
                    end ;
              '\' : begin
                      isbackslash := True ;
                    end ;
            end ;
          end ;
        end ;
      end ;

      // Parse value - extract integral and decimal part.
      procedure prepare_val(
        const _custom_value : Variant ;
        const _abs          : Boolean
      ) ;
      var
        i         : Integer ;
        txt       : String  ;
        idecimal  : Integer ;
        isign     : Integer ;
        tmp       : Double  ;
        precision : Integer ; 
        digits    : Integer ; 
      begin
        if _abs then
          tmp := Abs( new java.lang.Double( _custom_value.toString ) )
        else
          tmp := new java.lang.Double( _custom_value.toString ) ;
        fval := RoundTo( tmp, -ifmt_decimal )  ;
        digits := 15;
        precision := fval.tostring.length + digits - 1;
        if Integer(fval) = fval then
          precision := fval.tostring.length + digits;
        if VarType( fval ) = varDouble then begin
          txt := java.lang.String.Format( "%" + precision.toString + "." + digits.toString + "f",  fval  ) end
        else begin
          txt := FloatToStr( RoundS( tmp ) ) + '.00000000000000000' ;
        end;
        isign    := 0 ;
        idecimal := MAX_STR ;
        for i := 1 to length( txt ) do begin
          case txt[i] of
            '+' ,
            '-' : begin
                    isign := i ;
                  end ;
            ',' ,
            '.' : begin
                    idecimal := i ;
                    break ;
                  end ;
          end ;
        end ;

        if isign > 0 then
          cval_sign := txt[ isign ]
        else
          cval_sign := #0 ;

        sval_integral := Copy( txt, isign    + 1, idecimal - isign - 1 ) ;
        sval_decimal  := Copy( txt, idecimal + 1, Min( MAX_STR, ifmt_decimal ) ) ;
        bval_significant := sval_integral <> '0' ;
      end ;

      // parse custom number formatting
      function do_custom(
        const _custom_format : String  ;
        const _custom_value  : Variant ;
        const _abs           : Boolean
      ) : String ;
      var
        i             : Integer ;
        isdecimal     : Boolean ;
        isbackslash   : Boolean ;
        c  : Char ;
        cd : Char ;

        ar_output     : array of Char ;
        cnt_output    : Integer ;

        bsignificant  : Boolean ;
        cnt_placement : Integer ; // number of pending placements
        cnt_decimal   : Integer ; // number of pending integral digits
        cnt_integral  : Integer ; // number of pending decimal digits

        function next_digit : Char ;
        begin
          if      cnt_decimal  > 0 then begin
                    Result := sval_decimal[ cnt_decimal - 1 ] ;
                    dec( cnt_decimal ) ;
                    isdecimal := cnt_decimal > 0 ;
                  end
          else if cnt_integral > 0 then begin
                    Result := sval_integral[ cnt_integral - 1 ] ;
                    dec( cnt_integral ) ;
                  end
          else    begin
                    Result := #0 ;
                  end ;
        end;

        procedure to_result( const _c : Char ) ;
        begin
          if _c = #0 then exit ;
          inc( cnt_output ) ;
          ar_output[ cnt_output ] := _c ;
        end ;

      begin
        prepare_fmt( _custom_format ) ;
        prepare_val( _custom_value  , _abs ) ;

        ar_output := new Char[MAX_STR];
        cnt_output := -1 ;
        cnt_integral := length( sval_integral ) ;
        cnt_decimal  := length( sval_decimal  ) ;
        isdecimal    := ifmt_decimal > 0 ;
        bsignificant := False ;
        isbackslash  := False ;

        cnt_placement := ifmt_integral + ifmt_decimal ;
        for i := sfmt.toString.length -1  downto 0 do begin
          c := sfmt[i] ;
          if ( i > 1 ) and ( sfmt[i-1] = '\' ) then begin
            to_result( c ) ;
          end
          else begin
            case c of
              '#' : begin
                      if isdecimal then begin
                       cd := next_digit ;
                        if ( cd <> '0' ) or bsignificant then begin
                          bsignificant := True ;
                          to_result( cd ) ;
                        end ;
                      end
                      else begin
                        cd := next_digit ;
                        if ( cd = #0 ) and ( i > ifmt_zeropos ) then
                          cd := '0' ;

                        // avoid output of '0' like string
                        if bsignificant or bval_significant then
                          to_result( cd ) ;
                      end ;

                      dec( cnt_placement ) ;
                    end ;
              '0' : begin
                      if isdecimal then begin
                        bsignificant := True ;
                        cd := next_digit ;
                        to_result( cd ) ;
                      end
                      else begin
                        cd := next_digit ;
                        if cd = #0 then
                          cd := '0' ;
                        to_result( cd ) ;
                      end ;

                      dec( cnt_placement ) ;
                    end ;
              '.' : begin
                      if bsignificant then
                        to_result( '.' ) ;
                    end ;
              '\' : begin
                      // ignore ;
                    end
              else  begin
                      to_result( c ) ;
                    end ;
            end ;
          end ;

          // post all pending digits to avoid truncating of the beginning of the
          // number
          if ( cnt_placement = 0 ) and
             ( ( ifmt_integral + ifmt_decimal ) > 0 ) then
          begin
            while True do begin
              cd := next_digit ;
              if cd = #0 then break ;
              to_result( cd ) ;
            end ;
            to_result( cval_sign ) ;
            dec( cnt_placement ) ;
          end ;
        end ;

        // rewrite output buffer to the string
        res := "" ;
        try
          for i:=1 to cnt_output + 1 do begin
            res := res + ar_output[ cnt_output ] ;
            dec( cnt_output ) ;
          end ;

          Result := res.ToString ;
        finally
          FreeObject( res ) ;
        end ;
      end ;

      // parse number format string; if custom - forward to do_custom
      function do_parse( const _format : String; _value : Variant ) :String ;
      var
        i      : Integer ;
        c      : Char ;
        state  : Integer ;
        mode   : Integer ;
        prec   : Integer ;

        ipart2 : Integer ;
        ipart3 : Integer ;

        itmp1  : java.lang.Long ;
        stmp1  : String  ;
        stmp2  : String  ;

      const
        ST_START = 1;
        ST_NUMBER_START = 2;
        ST_NUMBER_COLLECT = 3;
        ST_PART1 = 4 ;
        ST_PART2 = 5 ;
        ST_PART3 = 6 ;

        MD_NONE = 0 ;
        MD_CURRENCY = 1 ;
        MD_DECIMAL = 2 ;
        MD_SCIENTIFIC_UPPERCASE  = 3 ;
        MD_SCIENTIFIC_LOWERCASE  = 4 ;
        MD_FIXED = 5 ;
        MD_GENERAL_UPPERCASE = 6 ;
        MD_GENERAL_LOWERCASE = 7 ;
        MD_NUMBER = 8 ;
        MD_PERCENT = 9 ;
        MD_ROUNDTRIP = 10 ;
        MD_HEXADECIMAL_UPPERCASE = 11 ;
        MD_HEXADECIMAL_LOWERCASE = 12 ;
        
        function general_precision : Integer ;
        begin
          case VarType( _value ) of
            varWord      : Result :=  5 ;
            varLongWord  : Result := 10 ;
            varByte      : Result :=  3 ;
            varCurrency  : Result := 15 ;
            varShortInt  : Result :=  3 ;
            varSmallInt  : Result :=  5 ;
            varInteger   : Result := 10 ;
            varInt64     : Result := 19 ;
            varUInt64    : Result := 19 ;
            varSingle    : Result :=  7 ;
            varDouble    : Result := 15 ;
            else           Result := 10 ;
          end;
        end;

        function is_integral : Boolean ;
        begin
          case VarType( _value ) of
            varWord      : Result := True  ;
            varLongWord  : Result := True  ;
            varByte      : Result := True  ;
            varCurrency  : Result := False ;
            varShortInt  : Result := True  ;
            varSmallInt  : Result := True  ;
            varInteger   : Result := True  ;
            varInt64     : Result := True  ;
            varUInt64    : Result := True  ;
            varSingle    : Result := False ;
            varDouble    : Result := False ;
            else           Result := False ;
          end;
        end;

        function displayCurrency(number: Float; precision: Integer) : String ;
        var
          currencyFormatter    : java.text.NumberFormat ;
          i                    : Integer                ;
          withoutDecimalPlaces : Integer                ;
          resultString         : String                 ;
        begin
          currencyFormatter := java.text.NumberFormat.getCurrencyInstance(java.util.Locale.getDefault);
          resultString := currencyFormatter.format(number);
          withoutDecimalPlaces := Integer(number);
          if withoutDecimalPlaces.toString.length <= precision then begin
            for i := 0 to precision - withoutDecimalPlaces.toString.length do begin
              resultString := resultString + "0";
            end;
          end;
          Result := resultString;
        end;

        function floatToScientific(number: Float; precision: Integer; digits : Integer): String;
        var
          tmp1      : Char;
          tmp2      : Char;
          finished  : StringBuilder;
        begin
          finished := new StringBuilder(java.lang.String.format("%" + precision + "." + digits + "e" , number));
          if finished.Length <= 6 + digits then begin
            finished.Length := 6+digits+1;
            tmp1 := Char(finished[4+digits]);
            tmp2 := Char(finished[5+digits]);
            finished[4+digits] := '0';
            finished[5+digits] := tmp1;
            finished[6+digits] := tmp2;
          end;
          Result := finished.toString;
        end;

        function fixedNumber(number: Float ; digits: Integer) : String;
        var
          i             : Integer ;
          finished      : String  ;
          decimalPlaces : Integer ;
        begin
          decimalPlaces := Integer(Number);
          finished := number.toString;
            for i := (number.toString.length - decimalPlaces.toString.length) to digits do begin
              finished := finished + "0";
            end;
          Result := finished;
        end;

        function generalNumber(number: Variant; precision: Integer; digits: Integer) : String;
        var
          resultString         : String                 ;
        begin
          resultString := number.toString;
          if number.toString.length - 1 > digits then begin
            number := Double.parseDouble(resultString);
            resultString := java.lang.String.format("%" + precision + "." + (digits - 1) + "e" , number);
          end;
          Result := resultString;
        end;

        function ordinarNumber(number: Double; precision: Integer; digits: Integer)  : String;
        var
          finished      : String                          ;
          comas         : Integer                         ;
          decimalPlaces : Integer                         ;
          i             : Integer                         ;
          helper        : Integer                         ;
        begin
          decimalPlaces := Integer(number);
          finished := java.text.NumberFormat.getNumberInstance(java.util.Locale.getDefault).format(number);
          helper := 4;
          for i := 0 to decimalPlaces.toString.length do begin
            if i = helper then begin
              comas := comas + 1;
              helper := helper + 3;
            end;
          end;
          for i := (finished.length - decimalPlaces.toString.length - comas ) to digits do begin
            finished := finished + "0";
          end;
          if ((finished.length - comas) - (decimalPlaces.toString.length + 1) ) < 2 then begin
            finished := finished + "0";
          end;
          Result := finished
        end;

        function percent(number: Double; digits: Integer ) : String;
        begin
          Result := ordinarNumber(number*100, 100, digits) + "%" ;
        end;

        function hex(number: Int64; digits: Integer) : String ;
        var
          finished : String   ;
          i        : Integer  ;
        begin
          finished := java.lang.Long.toHexString(number);
            for i := finished.length to digits - 1 do begin
              finished := "0" + finished;
            end;
          Result := finished;
        end;

      begin
        i := 0 ;

        ipart2 := MAX_STR ;
        ipart3 := MAX_STR ;

        mode := MD_NONE ;
        prec := 0 ;
        state := ST_START ;
        while i <= length( _format  ) -1 do begin
          c := _format[i] ;
          case state of
            ST_START :
              begin
                case c of
                  'C',
                  'c' : begin
                          prec := 2 ;
                          mode := MD_CURRENCY ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'D',
                  'd' : begin
                          mode := MD_DECIMAL ;
                          prec := 0 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'E' : begin
                          mode := MD_SCIENTIFIC_UPPERCASE ;
                          prec := 6 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'e' : begin
                          mode := MD_SCIENTIFIC_LOWERCASE ;
                          prec := 6 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'F',
                  'f' : begin
                          mode := MD_FIXED ;
                          prec := 2 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'G' : begin
                          mode := MD_GENERAL_UPPERCASE ;
                          prec := general_precision ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'g' : begin
                          mode := MD_GENERAL_LOWERCASE ;
                          prec := general_precision ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'N',
                  'n' : begin
                          mode := MD_NUMBER ;
                          prec := 2 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'P',
                  'p' : begin
                          mode := MD_PERCENT ;
                          prec := 2 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'R',
                  'r' : begin
                          mode := MD_ROUNDTRIP ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'X' : begin
                          mode := MD_HEXADECIMAL_UPPERCASE ;
                          prec := 0 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  'x' : begin
                          mode := MD_HEXADECIMAL_LOWERCASE ;
                          prec := 0 ;
                          inc( i ) ;
                          state := ST_NUMBER_START ;
                        end ;
                  else  begin
                          state := ST_PART1 ;
                        end ;
                end ;
              end ;
            ST_NUMBER_START :
              begin
                if ( c >= '0' ) and ( c <= '9' ) then begin
                  prec := 0 ;
                  state := ST_NUMBER_COLLECT ;
                end
                else
                  state := ST_PART1 ;
              end;
            ST_NUMBER_COLLECT :
              begin
                if ( c >= '0' ) and ( c <= '9' ) then begin
                  prec := prec * 10 + ( ord( c ) - ord( '0' ) ) ;
                  inc( i ) ;
                end
                else
                  state := ST_PART1 ;
              end;
            ST_PART1 :
              begin
                mode := MD_NONE ;
                if c = ';' then begin
                  ipart2 := i + 1 ;
                  state := ST_PART2 ;
                end ;
                inc( i ) ;
              end;
            ST_PART2 :
              begin
                mode := MD_NONE ;
                if c = ';' then begin
                  ipart3 := i + 1 ;
                  state := ST_PART2 ;
                end ;
                inc( i ) ;
              end;
            else
              begin
                assert( False, GIS_RS_ERR_UNTESTED ) ;
              end;
          end ;
        end;

        case mode of
          MD_NONE :
            begin
              if ipart3 < MAX_STR then begin // zero part exists
                if      new java.lang.Double(_value.toString) > 0 then begin
                          Result := do_custom(
                                      Copy( _format, 0, ipart2-1 ),
                                      _value,
                                      False
                                    ) ;
                        end
                else if new java.lang.Double(_value.toString) < 0 then begin
                          Result := do_custom(
                                      Copy( _format, ipart2, ipart3-ipart2-1 ),
                                      _value,
                                      True
                                    ) ;
                        end
                else    begin
                          Result := do_custom(
                                      Copy( _format, ipart3, MAX_STR ),
                                      0,
                                      False
                                    ) ;
                        end;
              end
              else if ipart2 < MAX_STR then begin // minus part
                if      new java.lang.Double(_value.toString) >= 0 then begin                   
                          Result := do_custom(
                                      Copy( _format, 0, ipart2-1 ),
                                      _value,
                                      False
                                    ) ;
                        end
                else    begin                   
                          Result := do_custom(
                                      Copy( _format, ipart2, MAX_STR ),
                                      _value,
                                      False
                                    ) ;
                        end
              end
              else
                Result := do_custom( _format, _value, False ) ;
            end ;
          MD_CURRENCY :
            begin
              Result := displayCurrency(Double(_value), prec);
            end ;
          MD_DECIMAL :
            begin
              itmp1 := Integer( _value ) ; 
              Result := IntToStr( itmp1 ) ;
              stmp1 := IntToStr( Abs( itmp1 ) ) ;
              if prec > length( stmp1 ) then begin
                stmp2 := IntToHex(0, prec) ;
                if itmp1 < 0 then
                  Result := '-' + Copy( stmp2, 1, prec - length( stmp1 ) ) + stmp1
                else
                  Result := Copy( stmp2, 1, prec - length( stmp1 ) )  + stmp1 ;
              end ;
            end ;
          MD_SCIENTIFIC_UPPERCASE :
            begin
              Result := UpperCase( floatToScientific(Double(_value), 3 , prec ) ) ;
            end ;
          MD_SCIENTIFIC_LOWERCASE :
            begin
              Result := LowerCase( floatToScientific(Double(_value), 3 , prec ) ) ;
            end ;
          MD_FIXED :
            begin
              Result := fixedNumber(Double(_value), prec)
            end ;
          MD_GENERAL_UPPERCASE :
            begin
              if is_integral then begin
                itmp1 := new java.lang.Long( _value.toString ) ;
                Result := IntToStr( itmp1 ) ;
                if length( Result ) > prec then
                  Result := UpperCase( generalNumber(itmp1, 3 , prec ) ) ;
              end
              else begin
                Result := UpperCase( generalNumber(Double(_value), 3 , prec ) ) ;
              end;
            end ;
          MD_GENERAL_LOWERCASE :
            begin
              if is_integral then begin
                itmp1 := new java.lang.Long( _value.toString ) ; 
                Result := IntToStr( itmp1 ) ;
                if _value.toString.length > prec then
                  Result := LowerCase( generalNumber(itmp1, 3 , prec ) ) ;
              end
              else
                Result := LowerCase( generalNumber(Double(_value), 3 , prec ) ) ;
            end ;
          MD_NUMBER :
            begin
              Result := ordinarNumber(Double(_value), 100, prec);
            end ;
          MD_PERCENT :
            begin
              Result := percent( Double(_value), prec);
            end ;
          MD_ROUNDTRIP :
            begin
              Result := _value.toString ;
            end ;
          MD_HEXADECIMAL_UPPERCASE :
            begin
              itmp1 := new java.lang.Long( _value.toString ) ; 
              Result := UpperCase( hex( itmp1, prec ) ) ;
            end ;
          MD_HEXADECIMAL_LOWERCASE :
            begin
              itmp1 := new java.lang.Long( _value.toString ) ; 
              Result := LowerCase( hex( itmp1, prec ) ) ;
            end ;
          else
            begin
              assert( False, GIS_RS_ERR_UNTESTED ) ;
            end;
        end;
      end;
    begin
      Result := do_parse( _format, _value ) ;
    end;
  {$ENDIF}

  {$IFDEF ISLAND}
    class function TGIS_StringFormat.parseNumber(
      const _format : String ;
      const _value  : Variant
    ) : String ;
    begin
      //?? TODO
      {$WARNING '### Verify ISLAND code'}
      Result := Format( _format, _value ) ;
    end ;
  {$ENDIF}

  class function TGIS_StringFormat.Format(
    const _format : String  ;
    const _value  : Variant
  ) : String ;
  {$IFNDEF OXYGENE}
    var
      i64 : Int64  ;
  {$ENDIF} 
  begin
    try
      if IsStringEmpty( _format ) then begin
        {$IFDEF CLR}
          case VarTypeEx( _value ) of
              varExFloat, 
              varExFixed : Result := VarToDouble( _value ).ToString('G15')               
          else             Result := VarToString( _value ) ;
          end ;        
        {$ELSE}
        Result := VarToString( _value ) ;
        {$ENDIF}
      end
      else begin
        case VarTypeEx( _value ) of
          varExNothing :
            Result := '' ;
          varExAnsiString :
            Result := parseString  ( _format, VarToString( _value ) ) ;
          varExWideString :
            Result := parseString  ( _format, VarToString( _value ) ) ;
          varExDateTime :
            {$IFNDEF OXYGENE}
              Result := parseDateTime( _format, TDateTime( _value ) ) ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseDateTime(_format, TDateTime(_value));
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToDateTime( _value ).ToString( _format ) ;
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := VarToDateTime( _value ).ToString ;
              {$ENDIF}
            {$ENDIF}
          varExBoolean :
            Result := parseBoolean ( _format, VarToBoolean( _value ) ) ;

          varExInt   :
            {$IFNDEF OXYGENE}
              Result := parseNumber  ( _format, _value ) ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToInt32( _value ).ToString( _format ) ;
              {$ENDIF}
            {$ENDIF}
          varExUInt  :
            {$IFNDEF OXYGENE}
              Result := parseNumber  ( _format, _value ) ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToUInt32( _value ).ToString( _format ) ;
              {$ENDIF}
            {$ENDIF}
          varExFloat :
            {$IFNDEF OXYGENE}
              Result := parseNumber  ( _format, _value ) ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseNumber( _format, _value);
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToDouble( _value ).ToString( _format ) ;
              {$ENDIF}
            {$ENDIF}
          varExFixed :
            {$IFNDEF OXYGENE}
              Result := parseNumber  ( _format, Double( _value ) ) ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToDouble( _value ).ToString( _format ) ;
              {$ENDIF}
            {$ENDIF}
          varExInt64  :
            {$IFNDEF OXYGENE}
              begin
                i64 := Int64(_value) ; // forced Int64 cast
                Result := parseNumber( _format, i64 ) ;
              end ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseNumber(_format, _value) ;
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToInt64( _value ).ToString( _format ) ;
              {$ENDIF}
            {$ENDIF}
          varExUInt64 :
            {$IFNDEF OXYGENE}
            begin
              i64 := Int64(_value) ; // forced Int64 cast
              Result := parseNumber( _format, i64 ) ;
            end ;
            {$ELSE}
              {$IFDEF JAVA}
                Result := parseNumber(_format, _value) ;
              {$ENDIF}
              {$IFDEF ISLAND}
                Result := parseNumber(_format, _value);
              {$ENDIF}
              {$IFDEF CLR}
                Result := VarToUInt64( _value ).ToString( _format ) ;
              {$ENDIF}
            {$ENDIF}
          else begin
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          end;
        end ;
      end ;
    except
       Result := 'ERR' ;
    end ;
  end ;

//==================================== END =====================================
end.

