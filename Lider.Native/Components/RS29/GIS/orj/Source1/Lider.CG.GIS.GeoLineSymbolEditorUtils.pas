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
  Utilities for Extended Line Symbology Editor.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLineSymbolEditorUtils ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLineSymbolEditorUtils"'}
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
    System.Generics.Collections ;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Utility for dealing with a string that is to become a
  ///   TGIS_SymbolLineEx. Should be used only in the context of
  ///   the extended line symbology editor (TGIS_LineSymbolEditor).
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  TGIS_LineSymbolParser = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      parser : TObject ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    {$IFNDEF MANAGED}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Performs a preliminary syntax check of the symbol defining string.
      /// </summary>
      /// <param name="_str">
      ///   string to be verified
      /// </param>
      /// <param name="_err_s">
      ///   starting index of the error
      /// </param>
      /// <param name="_err_l">
      ///   character length of the error
      /// </param>
      /// <returns>
      ///   True if the string is correct
      /// </returns>
      function  Parse       ( const _str   : String  ;
                                var _err_s : Integer ;
                                var _err_l : Integer
                            ) : Boolean ;
      /// <summary>
      ///   Prepares a compact version of the symbol defining string.
      /// </summary>
      /// <param name="_str">
      ///   string to be compacted
      /// </param>
      /// <returns>
      ///   compacted version of the string
      /// </returns>
      function  Compactify  ( const _str   : String
                            ) : String ;
      /// <summary>
      ///   Prepares an expanded version of the symbol defining string.
      /// </summary>
      /// <param name="_str">
      ///   string to be expanded
      /// </param>
      /// <returns>
      ///   compacted version of the string
      /// </returns>
      function  Expand      ( const _str   : String
                            ) : String ;
      /// <summary>
      ///   Tells whether the symbol definition is properly closed after
      ///   all the lines/parts were parsed.
      /// </summary>
      /// <returns>
      ///   True if the symbol is properly closed
      /// </returns>
      function  IsClosed    : Boolean ;
      /// <summary>
      ///   Resets the parser state for parsing a new symbol definition.
      /// </summary>
      procedure Reset       ;
  end ;


  /// <summary>
  ///   List of predefined line symbols. Should be used only in the context of
  ///   the extended line symbology editor (TGIS_LineSymbolEditor).
  /// </summary>
  TGIS_PredefinedLineSymbolList = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      startList    : TList<String> ;
      lineList     : TList<String> ;
      endList      : TList<String> ;
      startLenList : TList<String> ;
      lineMidList  : TList<Integer> ;
      endLenList   : TList<String> ;
    private
      procedure fillStartList ;
      procedure fillLineList ;
      procedure fillEndList ;
    private
      function  fget_StartCount : Integer ;
      function  fget_LineCount  : Integer ;
      function  fget_EndCount   : Integer ;
    private
      function  hasMid ( const _idx : Integer
                       ) : Boolean ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    {$IFDEF DCC}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
    public
      /// <summary>
      ///   Retrieves a symbol start element from the list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element
      /// </param>
      /// <returns>
      ///   symbol start element
      /// </returns>
      function  GetStart      ( const _idx   : Integer
                              ) : String ;
      /// <summary>
      ///   Retrieves a symbol line element from the list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element
      /// </param>
      /// <returns>
      ///   symbol line element
      /// </returns>
      function  GetLine       ( const _idx   : Integer
                              ) : String ;
      /// <summary>
      ///   Retrieves a symbol end element from the list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element
      /// </param>
      /// <returns>
      ///   symbol end element
      /// </returns>
      function  GetEnd        ( const _idx   : Integer
                              ) : String ;
      /// <summary>
      ///   Prepares a line symbol as a combination of start, line and end
      ///   elements of specific indexes.
      /// </summary>
      /// <param name="_start">
      ///   index of the start element
      /// </param>
      /// <param name="_line">
      ///   index of the line element
      /// </param>
      /// <param name="_end">
      ///   index of the end element
      /// </param>
      /// <returns>
      ///   prepared symbol
      /// </returns>
      function  PrepareSymbol ( const _start : Integer ;
                                const _line  : Integer ;
                                const _end   : Integer
                              ) : String ;
    public
      /// <summary>
      ///   Number of symbol start elements.
      /// </summary>
      property  StartCount : Integer
                             read  fget_StartCount ;
      /// <summary>
      ///   Number of symbol line elements.
      /// </summary>
      property  LineCount  : Integer
                             read  fget_LineCount ;
      /// <summary>
      ///   Number of symbol end elements.
      /// </summary>
      property  EndCount   : Integer
                             read  fget_EndCount ;
  end ;


{$IFNDEF GENDOC}
  const
    GIS_LS_CMD_GOTO    : String = 'GOTO' ;
    GIS_LS_CMD_G       : String = 'G' ;
    GIS_LS_CMD_MOVE    : String = 'MOVE' ;
    GIS_LS_CMD_M       : String = 'M' ;
    GIS_LS_CMD_FOR     : String = 'FOR' ;
    GIS_LS_CMD_F       : String = 'F' ;
    GIS_LS_CMD_END     : String = 'END' ;
    GIS_LS_CMD_E       : String = 'E' ;
    GIS_LS_CMD_DRAW    : String = 'DRAW' ;
    GIS_LS_CMD_D       : String = 'D' ;
    GIS_LS_CMD_LINE    : String = 'LINE' ;
    GIS_LS_CMD_L       : String = 'L' ;
    GIS_LS_CMD_OUTLINE : String = 'OUTLINE' ;
    GIS_LS_CMD_O       : String = 'O' ;
    GIS_LS_CMD_FILL    : String = 'FILL' ;
    GIS_LS_CMD_I       : String = 'I' ;
    GIS_LS_CMD_WIDTH   : String = 'WIDTH' ;
    GIS_LS_CMD_W       : String = 'W' ;
    GIS_LS_CMD_COLOR   : String = 'COLOR' ;
    GIS_LS_CMD_C       : String = 'C' ;
    GIS_LS_UNITS       : String = 'X P T W S %' ;
    GIS_LS_INDENT      : String = '  ' ;
    GIS_LS_FONT_FAMILY : String = 'Courier New' ;
    GIS_LS_DEF_SYMBOL  : String = '' ;
{$ENDIF}


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    Lider.CG.GIS.GeoRtl ;
{$ENDIF}

type

  T_elsParserState = (
    Command,
    Open,
    Close,
    Comment,
    Value,
    IgnoreOut,
    IgnoreIn,
    Error
  ) ;

  T_elsParser = class
    private
      State    : T_elsParserState ;
      Previous : T_elsParserState ;
      &Loop    : Boolean ;
    public
      constructor Create ;
    public
      function  Parse       ( const _str   : String  ;
                                var _err_s : Integer ;
                                var _err_l : Integer
                            ) : Boolean ;
      function  Compactify  ( const _str   : String
                            ) : String ;
      function  Expand      ( const _str   : String
                            ) : String ;
      function  IsClosed    : Boolean ;
      procedure Reset       ;
  end ;


//==============================================================================
// T_elsParser
//==============================================================================

  constructor T_elsParser.Create ;
  begin
    inherited ;

    Reset ;
  end ;


  function T_elsParser.Parse(
    const _str   : String  ;
      var _err_s : Integer ;
      var _err_l : Integer
  ) : Boolean ;
  var
    sb    : TStringBuilder ;
    res   : Boolean ;
    tmp   : String ;
    ok    : Boolean ;
    nov   : Integer ;
    cnov  : Boolean ;
    anov  : Integer ;
    unt   : Boolean ;
    c     : Char ;
    i     : Integer ;

    procedure collect ;
    begin
      if sb.Length = 0 then
        _err_s := i ;
      sb.Append( c ) ;
    end ;

    procedure err ;
    begin
      res := False ;
      _err_l := i - _err_s ;
      State := T_elsParserState.Error ;
    end ;

    procedure start_comment ;
    begin
      Previous := State ;
      State := T_elsParserState.Comment ;
    end ;

    procedure proc_com ;
    begin
      tmp := sb.ToString ;
      sb.Clear ;

      unt := True ;

      ok := False ;
      ok := ( tmp = GIS_LS_CMD_GOTO    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_G       ) or ok ;
      if ok then begin
        nov := 1 ;
        cnov := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_MOVE    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_M       ) or ok ;
      if ok then begin
        nov := 2 ;
        cnov := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_FOR     ) or ok ;
      ok := ( tmp = GIS_LS_CMD_F       ) or ok ;
      if ok then begin
        nov := -1 ;
        cnov := False ;
        &Loop := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_END     ) or ok ;
      ok := ( tmp = GIS_LS_CMD_E       ) or ok ;
      if ok then begin
        nov := 0 ;
        cnov := True ;
        &Loop := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_DRAW    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_D       ) or ok ;
      if ok then begin
        nov := 2 ;
        cnov := False ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_LINE    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_L       ) or ok ;
      if ok then begin
        nov := 1 ;
        cnov := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_OUTLINE ) or ok ;
      ok := ( tmp = GIS_LS_CMD_O       ) or ok ;
      if ok then begin
        nov := 4 ;
        cnov := False ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_FILL    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_I       ) or ok ;
      if ok then begin
        nov := 6 ;
        cnov := False ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_WIDTH   ) or ok ;
      ok := ( tmp = GIS_LS_CMD_W       ) or ok ;
      if ok then begin
        nov := 1 ;
        cnov := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_COLOR   ) or ok ;
      ok := ( tmp = GIS_LS_CMD_C       ) or ok ;
      if ok then begin
        nov := -3 ;
        cnov := False ;
        unt := False ;
        exit ;
      end ;

      if not ok then
        err ;
    end ;

    procedure proc_val ;
    var
      cc : Char ;
      ee : Integer ;
      vv : Integer ;
      uu : Boolean ;
    begin
      if sb.Length = 0 then
        exit ;

      tmp := sb.ToString ;
      sb.Clear ;

      cc := tmp[StringFirst] ;

      ok := False ;
      case cc of
        #48..#57, '+', '-' : ok := True ;
      end ;

      if not ok then begin
        err ;
        exit ;
      end ;

      cc := tmp[StringLast( tmp )] ;

      uu := False ;
      ok := False ;
      ee := length( tmp ) ;
      case cc of
        #48..#57 :
          ok := True ;
        'X', 'T', 'W', 'S', '%', 'P' :
          begin
            uu := True ;
            ok := unt ;
            dec( ee ) ;
          end ;
      end ;

      if not ok then begin
        err ;
        if uu then
          inc( _err_l ) ;
        exit ;
      end ;

      tmp := Copy( tmp, StringFirst, ee ) ;
      if not TryStrToInt( tmp, vv ) then
        err ;

      inc( anov ) ;
    end ;

  begin
    res := True ;

    anov := 0 ;
    &Loop := False ;

    if State = T_elsParserState.Comment then
      State := Previous ;

    sb := TStringBuilder.Create ;
    try
      _err_s := StringFirst ;
      _err_l := StringLast( _str ) ;

      for i := StringFirst to StringLast( _str ) do begin

        c := _str[i] ;

        case State of
          T_elsParserState.Command :
            begin
              case c of
                ';' : start_comment ;
                #13, #10, #09, #32, ')' :
                  err ;
                '(' :
                  begin
                    State := T_elsParserState.Open ;
                    proc_com ;
                  end ;
                else
                  collect ;
              end ;
            end ;
          T_elsParserState.Comment :
            begin
              case c of
                #10 : State := Previous ;
              end ;
            end ;
          T_elsParserState.Open :
            begin
              anov := 0 ;
              case c of
                ';' : start_comment ;
                ')' :
                  begin
                    if &Loop then begin
                      State := T_elsParserState.IgnoreOut ;
                      &Loop := False ;
                    end
                    else begin
                      collect ;
                      err ;
                      _err_l := 1 ;
                    end ;
                  end ;
                '?' :
                  begin
                    collect ;
                    err ;
                    _err_l := 1 ;
                  end ;
                #13, #10, #09, #32 :
                  State := T_elsParserState.IgnoreIn ;
                else begin
                  State := T_elsParserState.Value ;
                  collect ;
                end ;
              end ;
            end ;
          T_elsParserState.Value :
            begin
              case c of
                ';' : start_comment ;
                #13, #10, #09, #32, ',' :
                  begin
                    State := T_elsParserState.IgnoreIn ;
                    proc_val ;
                  end ;
                '+', '-' :
                  begin
                    proc_val ;
                    collect ;
                  end ;
                'X', 'T', 'W', 'S', '%', 'P' :
                  begin
                    collect ;
                    State := T_elsParserState.IgnoreIn ;
                    proc_val ;
                  end ;
                ')' :
                  begin
                    State := T_elsParserState.Close ;
                    proc_val ;
                  end ;
                else
                  collect ;
              end ;
            end ;
          T_elsParserState.IgnoreOut :
            begin
              case c of
                ';' : start_comment ;
                #13, #10, #09, #32 : ;
                else
                  begin
                    State := T_elsParserState.Command ;
                    collect ;
                  end ;
              end ;
            end ;
          T_elsParserState.IgnoreIn :
            begin
              case c of
                ';' : start_comment ;
                #13, #10, #09, #32, ',' : ;
                ')' :
                  State := T_elsParserState.Close ;
                else
                  begin
                    State := T_elsParserState.Value ;
                    collect ;
                  end ;
              end ;
            end ;
        end ;

        if State = T_elsParserState.Close then begin
          State := T_elsParserState.IgnoreOut ;
          if nov = -3 then begin
            if ( anov <> 3 ) and ( anov <> 1 ) then
              err ;
          end
          else
          if nov = -1 then begin
            if ( anov <> 1 ) and ( anov <> 0 ) then
              err ;
          end
          else
          if cnov then begin
            if anov <> nov then
              err ;
          end
          else begin
            if not ( ( anov >= nov ) and ( anov mod 2 = 0 ) ) then
              err ;
          end ;
        end
        else
        if State = T_elsParserState.Error then
          break ;

      end ;

      if State <> T_elsParserState.Error then begin
        case State of
          T_elsParserState.Command :
            err ;
          T_elsParserState.Value   :
            begin
              State := T_elsParserState.IgnoreIn ;
              proc_val ;
            end ;
        end ;
      end ;

    finally
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;


  function T_elsParser.Compactify(
    const _str : String
  ) : String ;
  var
    sb    : TStringBuilder ;
    sbres : TStringBuilder ;
    res   : String ;
    tmp   : String ;
    ok    : Boolean ;
    c     : Char ;
    i     : Integer ;

    procedure collect ;
    begin
      sb.Append( c ) ;
    end ;

    procedure start_comment ;
    begin
      Previous := State ;
      State := T_elsParserState.Comment ;
    end ;

    procedure proc_com ;
    begin
      tmp := sb.ToString ;
      sb.Clear ;

      ok := False ;
      ok := ( tmp = GIS_LS_CMD_GOTO    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_G       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_G ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_MOVE    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_M       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_M ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_FOR     ) or ok ;
      ok := ( tmp = GIS_LS_CMD_F       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_F ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_END     ) or ok ;
      ok := ( tmp = GIS_LS_CMD_E       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_E ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_DRAW    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_D       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_D ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_LINE    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_L       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_L ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_OUTLINE ) or ok ;
      ok := ( tmp = GIS_LS_CMD_O       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_O ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_FILL    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_I       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_I ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_WIDTH   ) or ok ;
      ok := ( tmp = GIS_LS_CMD_W       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_W ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_COLOR   ) or ok ;
      ok := ( tmp = GIS_LS_CMD_C       ) or ok ;
      if ok then begin
        sbres.Append( GIS_LS_CMD_C ) ;
        exit ;
      end ;
    end ;

  begin
    res := '' ;

    if State = T_elsParserState.Comment then
      State := Previous ;

    sb := TStringBuilder.Create ;
    sbres := TStringBuilder.Create ;
    try
      for i := StringFirst to StringLast( _str ) do begin

        c := _str[i] ;

        case State of
          T_elsParserState.Command :
            begin
              case c of
                ';' : start_comment ;
                '(' :
                  begin
                    State := T_elsParserState.Open ;
                    proc_com ;
                  end ;
                else
                  collect ;
              end ;
            end ;
          T_elsParserState.Comment :
            begin
              case c of
                #10 : State := Previous ;
              end ;
            end ;
          T_elsParserState.Open :
            begin
              case c of
                ';' : start_comment ;
                ')' : State := T_elsParserState.Close ;
                else
                  State := T_elsParserState.IgnoreIn ;
              end ;
            end ;
          T_elsParserState.Value : ;
          T_elsParserState.IgnoreOut :
            begin
              case c of
                ';' : start_comment ;
                #13, #10, #09, #32 : ;
                else
                  begin
                    State := T_elsParserState.Command ;
                    collect ;
                  end ;
              end ;
            end ;
          T_elsParserState.IgnoreIn :
            begin
              case c of
                ';' : start_comment ;
                ')' : State := T_elsParserState.Close ;
              end ;
            end ;
        end ;

        if State <> T_elsParserState.Command then begin
          if State = T_elsParserState.IgnoreOut then begin
            case c of
              #13, #10, #09, #32 : ;
              else
                sbres.Append( c ) ;
            end
          end
          else
            sbres.Append( c ) ;
        end ;

        if State = T_elsParserState.Close then
          State := T_elsParserState.IgnoreOut ;

      end ;

      res := sbres.ToString ;

    finally
      FreeObject( sbres ) ;
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;


  function T_elsParser.Expand(
    const _str : String
  ) : String ;
  var
    sb    : TStringBuilder ;
    sbres : TStringBuilder ;
    res   : String ;
    tmp   : String ;
    ok    : Boolean ;
    c     : Char ;
    i     : Integer ;

    procedure collect ;
    begin
      sb.Append( c ) ;
    end ;

    procedure start_comment ;
    begin
      Previous := State ;
      State := T_elsParserState.Comment ;
    end ;

    procedure append( const _s : String ) ;
    begin
      if &Loop then
        sbres.Append( GIS_LS_INDENT ) ;
      sbres.Append( _s ) ;
    end ;

    procedure proc_com ;
    begin
      tmp := sb.ToString ;
      sb.Clear ;

      ok := False ;
      ok := ( tmp = GIS_LS_CMD_GOTO    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_G       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_GOTO ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_MOVE    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_M       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_MOVE ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_FOR     ) or ok ;
      ok := ( tmp = GIS_LS_CMD_F       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_FOR ) ;
        &Loop := True ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_END     ) or ok ;
      ok := ( tmp = GIS_LS_CMD_E       ) or ok ;
      if ok then begin
        &Loop := False ;
        append( GIS_LS_CMD_END ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_DRAW    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_D       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_DRAW ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_LINE    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_L       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_LINE ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_OUTLINE ) or ok ;
      ok := ( tmp = GIS_LS_CMD_O       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_OUTLINE ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_FILL    ) or ok ;
      ok := ( tmp = GIS_LS_CMD_I       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_FILL ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_WIDTH   ) or ok ;
      ok := ( tmp = GIS_LS_CMD_W       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_WIDTH ) ;
        exit ;
      end ;
      ok := ( tmp = GIS_LS_CMD_COLOR   ) or ok ;
      ok := ( tmp = GIS_LS_CMD_C       ) or ok ;
      if ok then begin
        append( GIS_LS_CMD_COLOR ) ;
        exit ;
      end ;
    end ;

  begin
    res := '' ;

    if State = T_elsParserState.Comment then
      State := Previous ;

    sb := TStringBuilder.Create ;
    sbres := TStringBuilder.Create ;
    try
      for i := StringFirst to StringLast( _str ) do begin

        c := _str[i] ;

        case State of
          T_elsParserState.Command :
            begin
              case c of
                ';' : start_comment ;
                '(' :
                  begin
                    State := T_elsParserState.Open ;
                    proc_com ;
                  end ;
                else
                  collect ;
              end ;
            end ;
          T_elsParserState.Comment :
            begin
              case c of
                #10 : State := Previous ;
              end ;
            end ;
          T_elsParserState.Open :
            begin
              case c of
                ';' : start_comment ;
                ')' : State := T_elsParserState.Close ;
                else
                  State := T_elsParserState.IgnoreIn ;
              end ;
            end ;
          T_elsParserState.Value : ;
          T_elsParserState.IgnoreOut :
            begin
              case c of
                ';' : start_comment ;
                #13, #10, #09, #32 : ;
                '&' : ;
                else
                  begin
                    State := T_elsParserState.Command ;
                    collect ;
                  end ;
              end ;
            end ;
          T_elsParserState.IgnoreIn :
            begin
              case c of
                ';' : start_comment ;
                ')' : State := T_elsParserState.Close ;
              end ;
            end ;
        end ;

        if ( State <> T_elsParserState.Command   ) and
           ( State <> T_elsParserState.IgnoreOut ) then begin
          sbres.Append( c ) ;
        end ;

        if State = T_elsParserState.Close then begin
          sbres.Append( #13#10 ) ;
          State := T_elsParserState.IgnoreOut ;
        end ;

      end ;

      res := sbres.ToString ;

    finally
      FreeObject( sbres ) ;
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;


  function T_elsParser.IsClosed : Boolean ;
  begin
    Result := ( State = T_elsParserState.IgnoreOut ) or
              ( ( State    = T_elsParserState.Comment   ) and
                ( Previous = T_elsParserState.IgnoreOut )
              ) ;
  end ;


  procedure T_elsParser.Reset ;
  begin
    State := T_elsParserState.IgnoreOut ;
    Previous := T_elsParserState.IgnoreOut ;
    &Loop := False ;
  end ;


//==============================================================================
// TGIS_LineSymbolParser
//==============================================================================

  constructor TGIS_LineSymbolParser.Create ;
  begin
    inherited ;

    parser := T_elsParser.Create ;
  end ;

  {$IFNDEF MANAGED}
    destructor TGIS_LineSymbolParser.Destroy ;
    begin
      FreeObject( parser ) ;

      inherited ;
    end ;
  {$ENDIF}

  function TGIS_LineSymbolParser.Parse(
    const _str   : String  ;
      var _err_s : Integer ;
      var _err_l : Integer
  ) : Boolean ;
  begin
    Result := T_elsParser( parser ).Parse( _str, _err_s, _err_l ) ;
  end ;


  function TGIS_LineSymbolParser.Compactify(
    const _str : String
  ) : String ;
  begin
    Result := T_elsParser( parser ).Compactify( _str ) ;
  end ;


  function TGIS_LineSymbolParser.Expand(
    const _str : String
  ) : String ;
  begin
    Result := T_elsParser( parser ).Expand( _str ) ;
  end ;


  function TGIS_LineSymbolParser.IsClosed : Boolean ;
  begin
    Result := T_elsParser( parser ).IsClosed ;
  end ;


  procedure TGIS_LineSymbolParser.Reset ;
  begin
    T_elsParser( parser ).Reset ;
  end ;


//==============================================================================
// TGIS_PredefinedLineSymbolList
//==============================================================================

  constructor TGIS_PredefinedLineSymbolList.Create ;
  begin
    inherited ;

    fillStartList ;
    fillLineList ;
    fillEndList ;
  end ;

  {$IFDEF DCC}
    destructor TGIS_PredefinedLineSymbolList.Destroy ;
    begin
      FreeObject( startList    ) ;
      FreeObject( lineList     ) ;
      FreeObject( endList      ) ;
      FreeObject( startLenList ) ;
      FreeObject( lineMidList  ) ;
      FreeObject( endLenList   ) ;

      inherited ;
    end ;
  {$ENDIF}


  function TGIS_PredefinedLineSymbolList.fget_StartCount : Integer ;
  begin
    Result := startList.Count ;
  end ;


  function TGIS_PredefinedLineSymbolList.fget_LineCount : Integer ;
  begin
    Result := lineList.Count ;
  end ;


  function TGIS_PredefinedLineSymbolList.fget_EndCount : Integer ;
  begin
    Result := endList.Count ;
  end ;


  function TGIS_PredefinedLineSymbolList.hasMid(
    const _idx : Integer
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := False ;
    for i := 0 to lineMidList.Count - 1 do begin
      if _idx = lineMidList[i] then begin
        Result := True ;
        break ;
      end ;
    end ;
  end ;


  procedure TGIS_PredefinedLineSymbolList.fillStartList ;
  begin
    startList := TList<String>.Create ;
    startLenList := TList<String>.Create ;

    // none
    startList.Add( '' ) ;
    startLenList.Add( '0%' ) ;

    // line
    startList.Add( 'LINE(40S)' ) ;
    startLenList.Add( '40S' ) ;

    // right stripe
    startList.Add( 'MOVE(0 -40S)DRAW(0 40S)LINE(40S)' ) ;
    startLenList.Add( '40S' ) ;

    // left stripe
    startList.Add( 'MOVE(0 40S)DRAW(0 -40S)LINE(40S)' ) ;
    startLenList.Add( '40S' ) ;

    // stripe
    startList.Add( 'MOVE(0 -40S)DRAW(0 80S)MOVE(0 -40S)LINE(40S)' ) ;
    startLenList.Add( '40S' ) ;

    // simple forward arrow
    startList.Add( 'OUTLINE(0 0 -80S 40S)OUTLINE(0 0 -80S -40S)LINE(40S)' ) ;
    startLenList.Add( '40S' ) ;

    // simple backward arrow
    startList.Add( 'OUTLINE(0 0 80S 40S)OUTLINE(0 0 80S -40S)LINE(120S)' ) ;
    startLenList.Add( '120S' ) ;

    // solid forward arrow
    startList.Add( 'FILL(0 40S 80S -40S -80S -40S 0 80S)MOVE(80S 0)LINE(40S)' ) ;
    startLenList.Add( '120S' ) ;

    // solid backward arrow
    startList.Add( 'FILL(0 0 80S 40S 0 -80S -80S 40S)MOVE(80S 0)LINE(40S)' ) ;
    startLenList.Add( '120S' ) ;

    // hollow forward arrow
    startList.Add( 'OUTLINE(0 40S 80S -40S -80S -40S 0 80S)MOVE(80S 0)LINE(40S)' ) ;
    startLenList.Add( '120S' ) ;

    // hollow backward arrow
    startList.Add( 'OUTLINE(0 0 80S 40S 0 -80S -80S 40S)MOVE(80S 0)LINE(40S)' ) ;
    startLenList.Add( '120S' ) ;

    // hollow square
    startList.Add( 'OUTLINE(-20S -20S 40S 0 0 40S -40S 0 0 -40S)MOVE(20S 0)LINE(40S)' ) ;
    startLenList.Add( '60S' ) ;

    // solid square
    startList.Add( 'FILL(-20S -20S 40S 0 0 40S -40S 0 0 -40S)MOVE(20S 0)LINE(40S)' ) ;
    startLenList.Add( '60S' ) ;

    // hollow diamond
    startList.Add( 'OUTLINE(-40S 0 40S 40S 40S -40S -40S -40S -40S 40S)MOVE(40S 0)LINE(40S)' ) ;
    startLenList.Add( '80S' ) ;

    // solid diamond
    startList.Add( 'FILL(-40S 0 40S 40S 40S -40S -40S -40S -40S 40S)MOVE(40S 0)LINE(40S)' ) ;
    startLenList.Add( '80S' ) ;

    //hollow octagon
    startList.Add( 'OUTLINE(-40S 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 40S)MOVE(40S 0)LINE(40S)' ) ;
    startLenList.Add( '80S' ) ;

    // solid octagon
    startList.Add( 'FILL(-40S 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 40S)MOVE(40S 0)LINE(40S)' ) ;
    startLenList.Add( '80S' ) ;
  end ;


  procedure TGIS_PredefinedLineSymbolList.fillLineList ;
  begin
    lineList := TList<String>.Create ;
    lineMidList := TList<Integer>.Create ;

    // empty
    lineList.Add( '' ) ;
    // single solid
    lineList.Add( 'LINE(%s)' ) ;
    // single dashed
    lineList.Add( 'FOR(%s)LINE(80S)MOVE(40S 0)END()' ) ;
    // three dots
    lineList.Add( 'FOR(%s)LINE(80S)MOVE(20S 0)LINE(10S)MOVE(10S 0)LINE(10S)MOVE(10S 0)LINE(10S)MOVE(20S 0)END()' ) ;

    // double solid
    lineList.Add( 'MOVE(0 20S)LINE(%s)GOTO(%s)MOVE(0 -20S)LINE(%s)MOVE(0 20S)' ) ;
    // double dashed
    lineList.Add( 'MOVE(0 20S)FOR(%s)LINE(80S)MOVE(40S 0)END()GOTO(%s)MOVE(0 -20S)FOR(%s)LINE(80S)MOVE(40S 0)END()MOVE(0 20S)' ) ;

    // solid main + dashed right
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(0 -40S)FOR(%s)LINE(80S)MOVE(40S 0)END()MOVE(0 40S)' ) ;
    // solid main + dashed left
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(0 40S)FOR(%s)LINE(80S)MOVE(40S 0)END()MOVE(0 -40S)' ) ;

    // ladder
    lineList.Add( 'MOVE(0 20S)LINE(%s)GOTO(%s)MOVE(0 -20S)LINE(%s)GOTO(%s)FOR(%s)MOVE(0 20S)DRAW(0 -40S)MOVE(80S 0)END()MOVE(0 20S)' ) ;
    // railroad
    lineList.Add( 'LINE(%s)COLOR(255,255,255)WIDTH(5W)GOTO(%s)FOR(%s)LINE(50S)MOVE(50S 0)END()' ) ;

    // square zigzag
    lineList.Add( 'FOR(%s)DRAW(0 20S 40S 0 0 -40S 40S 0 0 20S)END()' ) ;
    // saw zigzag
    lineList.Add( 'FOR(%s)DRAW(20S 20S 40S -40S 20S 20S)END()' ) ;
    // bumpy zigzag
    lineList.Add( 'FOR(%s)DRAW(0 20S 20S 20S 40S 0 20S -20S 0 -40S 20S -20S 40S 0 20S 20S 0 20S)END()' ) ;
    // wave
    lineList.Add( 'FOR(%s)DRAW(5S 10S, 20S 10S, 20S 0, 20S -10S, 10S -20S, 20S -10S, 20S 0, 20S 10S, 5S 10S)END()' ) ;

    // perpendicular stripes
    lineList.Add( 'FOR(%s)MOVE(0 20S)DRAW(0 -40S)MOVE(40S 20S)END()' ) ;
    // antidiagonal stripes
    lineList.Add( 'FOR(%s)MOVE(0 -20S)DRAW(40S 40S)MOVE(0 20S)END()' ) ;
    // diagonal stripes
    lineList.Add( 'FOR(%s)MOVE(0 20S)DRAW(40S -40S)MOVE(0 20S)END()' ) ;
    // right solid saw
    lineList.Add( 'FOR(%s)FILL(0 0 40S 40S 40S -40S -80S 0)MOVE(80S 0)END()' ) ;
    // left solid saw
    lineList.Add( 'FOR(%s)FILL(0 0 40S -40S 40S 40S -80S 0)MOVE(80S 0)END()' ) ;

    // solid main + simple forward arrow in the middle
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(40S 0)DRAW(-80S 40S)MOVE(80S -40S)DRAW(-80S -40S)GOTO(%s)MOVE(%s 0)' ) ;
    lineMidList.Add( lineList.Count - 1 ) ;
    // solid main + simple backward arrow in the middle
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(-40S 0)DRAW(80S 40S)MOVE(-80S -40S)DRAW(80S -40S)GOTO(%s)MOVE(%s 0)' ) ;
    lineMidList.Add( lineList.Count - 1 ) ;

    // solid main + simple forward arrows
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(80S 0)FOR(%s)DRAW(-80S 40S)MOVE(80S -40S)DRAW(-80S -40S)MOVE(320S 40S)END()' ) ;
    // solid main + simple backward arrows
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(80S 40S)MOVE(-80S -40S)DRAW(80S -40S)MOVE(160S 0)END()' ) ;

    // solid main + solid forward arrow in the middle
    lineList.Add( 'LINE(%s)GOTO(%s)FILL(40S 0 -80S 40S 0 -80S 80S 40S)GOTO(%s)MOVE(%s 0)' ) ;
    lineMidList.Add( lineList.Count - 1 ) ;
    // solid main + solid backward arrow in the middle
    lineList.Add( 'LINE(%s)GOTO(%s)FILL(-40S 0 80S 40S 0 -80S -80S 40S)GOTO(%s)MOVE(%s 0)' ) ;
    lineMidList.Add( lineList.Count - 1 ) ;

    // solid main + solid forward arrows
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(80S 0)FOR(%s)FILL(0 0 -80S -40S 0 80S 80S -40S)MOVE(240S 0)END()' ) ;
    // solid main + solid backward arrows
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(80S 0)FOR(%s)FILL(-80S 0 80S 40S 0 -80S -80S 40S)MOVE(240S 0)END()' ) ;

    // solid main + hollow forward arrows
    lineList.Add( 'FOR(%s)MOVE(80S 0)DRAW(-80S 40S 0 -80S 80S 40S)LINE(160S)END()' ) ;
    // solid main + hollow backward arrows
    lineList.Add( 'FOR(%s)DRAW(80S 40S 0 -80S -80S 40S)MOVE(80S 0)LINE(160S)END()' ) ;

    // solid main + right perpendicular stripes
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)MOVE(0 -40S)DRAW(0 40S)MOVE(240S 0)END()' ) ;
    // solid main + left perpendicular stripes
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)MOVE(0 40S)DRAW(0 -40S)MOVE(240S 0)END()' ) ;
    // solid main + perpendicular stripes
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)MOVE(0 40S)DRAW(0 -80S)MOVE(240S 0)END()' ) ;
    // solid main + left right stripes
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)MOVE(20S -40S)DRAW(0 40S)MOVE(80S 0S)DRAW(0 60S)MOVE(80S 0)END()' ) ;
    // solid main + double perpendicular stripes
    lineList.Add( 'LINE(%s)GOTO(%s)MOVE(20S 0)FOR(%s)MOVE(0 40S)DRAW(0 -80S)MOVE(20W 80S)DRAW(0 -80S)MOVE(120S 0)END()' ) ;
    // solid main + cross stripes
    lineList.Add( 'LINE(%s)GOTO(%s)WIDTH(8W)FOR(%s)MOVE(0 -20S)DRAW(40S 40S)MOVE(-40S 0)DRAW(40S -40S)MOVE(80S 40S)END()' ) ;

    // connected right squares
    lineList.Add( 'FOR(%s)DRAW(0 -40S 40S 0 0 40S)LINE(80S)END()' ) ;
    // connected left squares
    lineList.Add( 'FOR(%s)DRAW(0 40S 40S 0 0 -40S)LINE(80S)END()' ) ;

    // solid main + right hollow squares
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(0 -40S 40S 0 0 40S)MOVE(80S 0)END()' ) ;
    // solid main + left hollow squares
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(0 40S 40S 0 0 -40S)MOVE(80S 0)END()' ) ;

    // solid main + right solid squares
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 0 40S 40S 0 0 -40S)MOVE(120S 0)END()' ) ;
    // solid main + left solid squares
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 0 -40S 40S 0 0 40S)MOVE(120S 0)END()' ) ;

    // solid main + hollow squares
    lineList.Add( 'FOR(%s)MOVE(0 -20S)DRAW(0 40S 40S 0 0 -40S -40S 0)MOVE(40S 20S)LINE(80S)END()' ) ;
    // solid main + solid squares
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 -20S 0 40S 40S 0 0 -40S -40S 0)MOVE(120S 0)END()' ) ;

    // connected right triangles
    lineList.Add( 'FOR(%s)DRAW(40S -40S 40S 40S)LINE(80S)END()' ) ;
    // connected left triangles
    lineList.Add( 'FOR(%s)DRAW(40S 40S 40S -40S)LINE(80S)END()' ) ;

    // solid main + right hollow triangles
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(40S -40S 40S 40S)MOVE(80S 0)END()' ) ;
    // solid main + left hollow triangles
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(40S 40S 40S -40S)MOVE(80S 0)END()' ) ;

    // solid main + right solid triangles
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 40S 40S 40S -40S -80S 0)MOVE(160S 0)END()' ) ;
    // solid main + left solid triangles
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 40S -40S 40S 40S  -80S 0)MOVE(160S 0)END()' ) ;

    // solid main + hollow diamonds
    lineList.Add( 'FOR(%s)DRAW(40S -40S 40S 40S -40S 40S -40S -40S)MOVE(80S 0)LINE(80S)END()' ) ;
    // solid main + solid diamonds
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 40S -40S 40S 40S -40S 40S -40S -40S)MOVE(160S 0)END()' ) ;

    // connected right bumps
    lineList.Add( 'FOR(%s)DRAW(0 -20S 20S -20S 40S 0 20S 20S 0 20S)LINE(80S)END()' ) ;
    // connected left bumps
    lineList.Add( 'FOR(%s)DRAW(0 20S 20S 20S 40S 0 20S -20S 0 -20S)LINE(80S)END()' ) ;

    // solid main + right hollow bumps
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(0 -20S 20S -20S 40S 0 20S 20S 0 20S)MOVE(80S 0)END()' ) ;
    // solid main + left hollow bumps
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)DRAW(0 20S 20S 20S 40S 0 20S -20S 0 -20S)MOVE(80S 0)END()' ) ;

    // solid main + right solid bumps
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 0 20S 20S 20S 40S 0 20S -20S 0 -20S)MOVE(160S 0)END()' ) ;
    // solid main + left solid bumps
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 0 -20S 20S -20S 40S 0 20S 20S 0 20S)MOVE(160S 0)END()' ) ;

    // solid main + hollow octagons
    lineList.Add( 'FOR(%s)DRAW(0 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 20S)MOVE(80S 0)LINE(80S)END()' ) ;
    // solid main + solid octagons
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 0 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 20S)MOVE(160S 0)END()' ) ;
    // solid main + solid dots
    lineList.Add( 'LINE(%s)GOTO(%s)FOR(%s)FILL(0 0 0 10S 10S 10S 20S 0 10S -10S 0 -20S -10S -10S -20S 0 -10S 10S 0 10S)MOVE(120S 0)END()' ) ;


    // X's
    lineList.Add( 'FOR(%s)MOVE(0 -20S)DRAW(40S 40S)MOVE(-40S 0)DRAW(40S -40S)MOVE(80S 40S)END()' ) ;

    // hollow squares
    lineList.Add( 'FOR(%s)MOVE(0 -20S)DRAW(0 40S 40S 0 0 -40S -40S 0)MOVE(120S 20S)END()' ) ;
    // solid dot
    lineList.Add( 'FOR(-120S)FILL(0 -10S 0 20S 20S 0 0 -20S -20S 0)MOVE(60S 0)END()' ) ;
    // solid squares
    lineList.Add( 'FOR(%s)FILL(0 -20S 0 40S 40S 0 0 -40S -40S 0)MOVE(120S 0)END()' ) ;

    // hollow diamonds
    lineList.Add( 'FOR(%s)DRAW(40S -40S 40S 40S -40S 40S -40S -40S)MOVE(160S 0)END()' ) ;
    // solid diamonds
    lineList.Add( 'FOR(%s)FILL(0 0 40S -40S 40S 40S -40S 40S -40S -40S)MOVE(160S 0)END()' ) ;

    // hollow octagons
    lineList.Add( 'FOR(%s)DRAW(0 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 20S)MOVE(160S 0)END()' ) ;
    // solid octagons
    lineList.Add( 'FOR(%s)FILL(0 0 0 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 20S)MOVE(160S 0)END()' ) ;
  end ;


  procedure TGIS_PredefinedLineSymbolList.fillEndList ;
  begin
    endList := TList<String>.Create ;
    endLenList := TList<String>.Create ;

    // none
    endList.Add( '' ) ;
    endLenList.Add( '100%' ) ;

    // line
    endList.Add( 'LINE(40S)' ) ;
    endLenList.Add( '-40S' ) ;

    // right stripe
    endList.Add( 'LINE(40S)MOVE(0 -40S)DRAW(0 40S)' ) ;
    endLenList.Add( '-40S' ) ;

    // left stripe
    endList.Add( 'LINE(40S)MOVE(0 40S)DRAW(0 -40S)' ) ;
    endLenList.Add( '-40S' ) ;

    // stripe
    endList.Add( 'LINE(40S)MOVE(0 -40S)DRAW(0 80S)' ) ;
    endLenList.Add( '-40S' ) ;

    // simple forward arrow
    endList.Add( 'LINE(120S)OUTLINE(0 0 -80S 40S)OUTLINE(0 0 -80S -40S)' ) ;
    endLenList.Add( '-120S' ) ;

    // simple backward arrow
    endList.Add( 'LINE(40S)OUTLINE(0 0 80S 40S)OUTLINE(0 0 80S -40S)' ) ;
    endLenList.Add( '-40S' ) ;

    // solid forward arrow
    endList.Add( 'LINE(40S)FILL(0 40S 80S -40S -80S -40S 0 80S)' ) ;
    endLenList.Add( '-120S' ) ;

    // solid backward arrow
    endList.Add( 'LINE(40S)FILL(0 0 80S 40S 0 -80S -80S 40S)' ) ;
    endLenList.Add( '-120S' ) ;

    // hollow forward arrow
    endList.Add( 'LINE(40S)OUTLINE(0 40S 80S -40S -80S -40S 0 80S)' ) ;
    endLenList.Add( '-120S' ) ;

    // hollow backward arrow
    endList.Add( 'LINE(40S)OUTLINE(0 0 80S 40S 0 -80S -80S 40S)' ) ;
    endLenList.Add( '-120S' ) ;

    // hollow square
    endList.Add( 'LINE(40S)OUTLINE(0 -20S 40S 0 0 40S -40S 0 0 -40S)' ) ;
    endLenList.Add( '-60S' ) ;

    // solid square
    endList.Add( 'LINE(40S)FILL(0 -20S 40S 0 0 40S -40S 0 0 -40S)' ) ;
    endLenList.Add( '-60S' ) ;

    // hollow diamond
    endList.Add( 'LINE(40S)OUTLINE(0 0 40S 40S 40S -40S -40S -40S -40S 40S)' ) ;
    endLenList.Add( '-80S' ) ;

    // solid diamond
    endList.Add( 'LINE(40S)FILL(0 0 40S 40S 40S -40S -40S -40S -40S 40S)' ) ;
    endLenList.Add( '-80S' ) ;

    //hollow octagon
    endList.Add( 'LINE(40S)OUTLINE(0 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 40S)' ) ;
    endLenList.Add( '-80S' ) ;

    // solid octagon
    endList.Add( 'LINE(40S)FILL(0 20S 20S 20S 40S 0 20S -20S 0 -40S -20S -20S -40S 0 -20S 20S 0 40S)' ) ;
    endLenList.Add( '-80S' ) ;
  end ;


  function TGIS_PredefinedLineSymbolList.GetStart(
    const _idx : Integer
  ) : String ;
  begin
    Result := startList[_idx] ;
  end ;


  function TGIS_PredefinedLineSymbolList.GetLine(
    const _idx : Integer
  ) : String ;
  begin
    if hasMid( _idx ) then
      Result :=
        Format( lineList[_idx], [ '100%', '50%', '100%', '0' ] )
    else
      Result :=
        Format( lineList[_idx], [ '100%', '0%', '100%', '0%', '100%' ] ) ;
  end ;


  function TGIS_PredefinedLineSymbolList.GetEnd(
    const _idx : Integer
  ) : String ;
  begin
    Result := endList[_idx] ;
  end ;


  function TGIS_PredefinedLineSymbolList.PrepareSymbol(
    const _start : Integer ;
    const _line  : Integer ;
    const _end   : Integer
  ) : String ;
  var
    sb  : TStringBuilder ;
    sl  : String ;
    el  : String ;
    fmt : String ;
    res : String ;
  begin
    sb := TStringBuilder.Create ;
    try
      if _start > 0 then
        sb.Append( startList[_start] ) ;

      sl := startLenList[_start] ;
      el := endLenList[_end] ;
      if hasMid( _line ) then
        fmt := Format( lineList[_line], [ el, '50%', '100%', el ] )
      else
        fmt := Format( lineList[_line], [ el, sl, el, sl, el ] ) ;

      if ( _line = 0 ) and ( _end <> 0 ) then
        sb.Append( 'GOTO(100%)' )
      else
        sb.Append( fmt ) ;

      if _end > 0 then
        sb.Append( endList[_end] ) ;

      res := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    Result := res ;
  end ;


//==================================== END =====================================
end.
