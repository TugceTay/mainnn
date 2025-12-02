//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Basic print manager class.
}

{$IFDEF DCC}
  unit GisPrintUtils ;
  {$HPPEMIT '#pragma link "GisPrintUtils"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    GisTypesUI,
    GisPrintBuilder ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    tatukgis.rtl;
{$ENDIF}
{$IFDEF ISLAND}
uses
  remobjects.elements.rtl.*,
  TatukGIS.RTL ;
{$ENDIF}

type
  {#gendoc:hide}
  TGIS_CustomPrinterSettings = class
    public
      Name       : String  ;
      Landscape  : Boolean ;
      Portrait   : Boolean ;
      PageSize   : TPoint  ;
      PrintArea  : TRect   ;
      PPI        : Integer ;
    public
      constructor Create ;
  end ;

  {#gendoc:hide}
  TGIS_PageSizeEntry = record
    Name       : String  ;
    PageSize   : TPoint  ;
    PrintArea  : TRect   ;
    PPI        : Integer ;
  end ;

type
  {#gendoc:hide}
  {$IFNDEF OXYGENE}
    TGIS_PageSizeTable_class = class
  {$ELSE}
    TGIS_PageSizeTable = static class
  {$ENDIF}
    private
      {$IFDEF OXYGENE} class {$ENDIF} procedure fill_Table ;
    public
      Table : array[0..19] of TGIS_PageSizeEntry ;
    public
      {$IFDEF OXYGENE} class {$ENDIF}
        constructor Create ;
      {$IFDEF OXYGENE} class {$ENDIF}
        function IndexOf   (     _name      : String
                           ) : Integer ;
      {$IFDEF OXYGENE} class {$ENDIF}
        function GetPage   (     _index     : Integer ;
                                 _landscape : Boolean ;
                             var _name      : String  ;
                             var _pageSize  : TPoint  ;
                             var _printArea : TRect   ;
                             var _ppi       : Integer
                           ) : Boolean ;
  end ;

{$IFNDEF OXYGENE}
var
  {#gendoc:hide}
  TGIS_PageSizeTable : TGIS_PageSizeTable_class ;
{$ENDIF}

type

  {#gendoc:hide}
  TGIS_PrintUtils = class
    public
      class function ResolveCustomPrinterSettings
                                  ( _settings     : String
                                  ) : TGIS_CustomPrinterSettings ;
      class function UnitToText   ( const _u      : TGIS_PrintLayoutUnits
                                  ) : String ;
      class function TextToUnit   ( const _u      : String
                                  ) : TGIS_PrintLayoutUnits ;
      class function WidthToText  ( const _width  : TGIS_PrintLayoutWidth
                                  ) : String ;
      class function PointsToText ( const _points : Double
                                  ) : String ;
      class function ColorToText  ( const _color  : TGIS_Color
                                  ) : String ;
      class function TextToColor  ( const _color  : String
                                  ) : TGIS_Color ;
      class function FontStyleToText
                                  ( _style   : TGIS_FontStyles
                                  ) : String ;
      class function SnapToText   ( const _snap   : TGIS_PrintLayoutSnap
                                  ) : String ;
      class function ToTwips      ( const _value  : String ;
                                    const _unit   : String ;
                                    const _ppi    : Integer
                                  ) : Integer ;
      class function FromTwips    ( const _value  : Integer ;
                                    const _unit   : String  ;
                                    const _ppi    : Integer
                                  ) : Double ;
      class function ToPixels     ( const _value  : Double ;
                                    const _unit   : TGIS_PrintLayoutUnits ;
                                    const _ppi    : Integer
                                  ) : Integer ;
      class function FromPixels   ( const _value  : Integer ;
                                    const _unit   : TGIS_PrintLayoutUnits ;
                                    const _ppi    : Integer
                                  ) : Double ;
  end;

const
  {#gendoc:hide}
  TPL_SIZE_CM  = 'cm' ;           // Text for centimeters
  {#gendoc:hide}
  TPL_SIZE_MM  = 'mm' ;           // Text for millimeters
  {#gendoc:hide}
  TPL_SIZE_IN  = 'in' ;           // Text for inches
  {#gendoc:hide}
  TPL_SIZE_PT  = 'pt' ;           // Text for points
  {#gendoc:hide}
  TPL_SIZE_PX  = 'px' ;           // Text for pixels

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.SysUtils,
  GisRtl,
  GisResource,
  GisParams;
{$ENDIF}

//=============================================================================
// TGIS_CustomPrinterSettings
//=============================================================================

  constructor TGIS_CustomPrinterSettings.Create ;
  begin
    inherited ;
    Name      := '' ;
    Landscape := False ;
    Portrait  := False ;
    PageSize  := Point( 0, 0 ) ;
    PrintArea := Rect( 0, 0, 0, 0 ) ;
    PPI       := 0 ;
  end ;

//=============================================================================
// TGIS_PageSizeTable
//=============================================================================

  {$IFNDEF OXYGENE}
    constructor TGIS_PageSizeTable_class.Create ;
  {$ELSE}
    class constructor TGIS_PageSizeTable.Create ;
  {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      inherited Create;
    {$ENDIF}

    fill_Table ;
  end ;

  {$IFNDEF OXYGENE}
    procedure TGIS_PageSizeTable_class.fill_Table ;
  {$ELSE}
    class procedure TGIS_PageSizeTable.fill_Table ;
  {$ENDIF}
    procedure element(
      _idx  : Integer ;
      _name : String  ;
      _size : TPoint  ;
      _area : TRect   ;
      _ppi  : Integer
    ) ;
    begin
      Table[_idx].Name := _name ;
      Table[_idx].PageSize := _size ;
      Table[_idx].PrintArea := _area ;
      Table[_idx].PPI := _ppi ;
    end ;
  begin
    element( 0, 'A0',             Point( 19866, 28087 ), Rect( 300, 300, 19566, 27787 ), 600 ) ;
    element( 1, 'A1',             Point( 14043, 19866 ), Rect( 300, 300, 13743, 19566 ), 600 ) ;
    element( 2, 'A2',             Point(  9933, 14043 ), Rect( 300, 300,  9633, 13743 ), 600 ) ;
    element( 3, 'A3',             Point(  7016,  9933 ), Rect( 300, 300,  6716,  9633 ), 600 ) ;
    element( 4, 'A4',             Point(  4960,  7016 ), Rect( 300, 300,  4660,  6716 ), 600 ) ;
    element( 5, 'A5',             Point(  3508,  4960 ), Rect( 300, 300,  3208,  4660 ), 600 ) ;
    element( 6, 'Arch A',         Point(  5400,  7200 ), Rect( 300, 300,  5100,  6900 ), 600 ) ;
    element( 7, 'Arch B',         Point(  7200, 10800 ), Rect( 300, 300,  6900, 10500 ), 600 ) ;
    element( 8, 'Arch C',         Point( 10800, 14400 ), Rect( 300, 300, 10500, 14100 ), 600 ) ;
    element( 9, 'Arch D',         Point( 14400, 21600 ), Rect( 300, 300, 14100, 21300 ), 600 ) ;
    element(10, 'Arch E',         Point( 21600, 28800 ), Rect( 300, 300, 21300, 28500 ), 600 ) ;
    element(11, 'Arch E1',        Point( 18000, 25200 ), Rect( 300, 300, 17700, 24900 ), 600 ) ;
    element(12, 'B3',             Point(  8338, 11811 ), Rect( 300, 300,  8038, 11511 ), 600 ) ;
    element(13, 'B4',             Point(  5905,  8338 ), Rect( 300, 300,  5605,  8038 ), 600 ) ;
    element(14, 'B5',             Point(  4157,  5905 ), Rect( 300, 300,  3857,  5605 ), 600 ) ;
    element(15, 'Half Letter',    Point(  3300,  5100 ), Rect( 300, 300,  3000,  4800 ), 600 ) ;
    element(16, 'Junior Legal',   Point(  3000,  4800 ), Rect( 300, 300,  2700,  4500 ), 600 ) ;
    element(17, 'Ledger/Tabloid', Point(  6600, 10200 ), Rect( 300, 300,  6300,  9900 ), 600 ) ;
    element(18, 'Legal',          Point(  5100,  8400 ), Rect( 300, 300,  4800,  8100 ), 600 ) ;
    element(19, 'Letter',         Point(  5100,  6600 ), Rect( 300, 300,  4800,  6300 ), 600 ) ;
  end ;

  {$IFNDEF OXYGENE}
    function TGIS_PageSizeTable_class.IndexOf( _name : String ) : Integer ;
  {$ELSE}
    class function TGIS_PageSizeTable.IndexOf( _name : String ) : Integer ;
  {$ENDIF}
  var
    i : Integer ;
  begin
    Result := -1 ;
    for i := 0 to high( Table ) do
      if Table[i].Name = _name then begin
        Result := i ;
        break ;
      end ;
  end ;

  {$IFNDEF OXYGENE}
    function TGIS_PageSizeTable_class.GetPage(
  {$ELSE}
    class function TGIS_PageSizeTable.GetPage(
  {$ENDIF}
        _index     : Integer ;
        _landscape : Boolean ;
    var _name      : String  ;
    var _pageSize  : TPoint  ;
    var _printArea : TRect   ;
    var _ppi       : Integer
  ) : Boolean ;
  begin
    if ( _index < 0 ) or ( _index > high( TGIS_PageSizeTable.Table ) ) then
    begin
      Result := False ;
      exit ;
    end;
    _name      := TGIS_PageSizeTable.Table[_index].Name ;
    _pageSize  := TGIS_PageSizeTable.Table[_index].PageSize ;
    _printArea := TGIS_PageSizeTable.Table[_index].PrintArea ;
    _ppi       := TGIS_PageSizeTable.Table[_index].PPI ;
    if _landscape then begin
      _pageSize  := Point( _pageSize.Y, _pageSize.X ) ;
      _printArea := Rect( _printArea.Top, _printArea.Left,
                          _printArea.Bottom, _printArea.Right ) ;
    end ;
    Result := True ;
  end ;

  class function TGIS_PrintUtils.ResolveCustomPrinterSettings(
    _settings : String
  ) : TGIS_CustomPrinterSettings ;
  var
    prn   : TGIS_CustomPrinterSettings ;
    txt   : String ;
    state : Integer ;
    sname : String ;
    sornt : String ;
    sval1  : String ;
    sval2  : String ;
    measure1 : String ;
    measure2 : String ;
    dval1  : Double ;
    dval2  : Double ;
    idx   : Integer ;
    i     : Integer ;
    c     : Char ;

    function valid_measure( _measure : String ) : Boolean ;
    begin
      if ( _measure = TPL_SIZE_CM ) or
         ( _measure = TPL_SIZE_MM ) or
         ( _measure = TPL_SIZE_IN ) or
         ( _measure = TPL_SIZE_PT ) or
         ( _measure = TPL_SIZE_PX )
      then Result := True
      else Result := False ;
    end ;

  begin
    Result := nil ;
    txt := Trim( _settings ) ;
    if IsStringEmpty( txt ) then exit ;

    prn := TGIS_CustomPrinterSettings.Create ;
    try
      state := 0 ;
      sname := '' ;
      sornt := '' ;
      sval1  := '' ;
      measure1 := '' ;
      sval2  := '' ;
      measure2 := '' ;
      idx := 1 ;

      for i := StringFirst to length( txt )+StringFirst-1 do begin
        c := txt[ i ] ;

        case state of
          0 : if ( ( c >= 'a' ) and ( c <= 'z' ) ) or ( ( c >= 'A' ) and ( c <= 'Z' ) ) then begin
                state := 1 ;
                sname := c ;
              end
              else
              if CharInSet( c, ['0','1','2','3','4','5','6','7','8','9', '.', '-', '+' ] ) then begin
                state := 3 ;
                if idx = 1 then
                  sval1 := c
                else
                  sval2 := c ;
              end else
                break ;
          1 : if c = ';' then begin
                state := 2 ;
              end else
                sname := sname + c ;
          2 : if c = ';' then
                break
              else
                sornt := sornt + c ;
          3 : if CharInSet( c, ['0','1','2','3','4','5','6','7','8','9', '.', '-', '+', ' ' ] ) then begin
                if idx = 1 then
                  sval1 := sval1 + c
                else
                  sval2 := sval2 + c ;
              end else begin
                state := 4 ;
                if idx = 1 then
                  measure1 := c
                else
                  measure2 := c ;
              end ;
          4 : if c = ';' then begin
                if idx = 1 then begin
                  idx := 2 ;
                  state := 0 ;
                end else
                 break ;
              end else begin
                if idx = 1 then
                  measure1 := measure1 + c
                else
                  measure2 := measure2 + c ;
              end ;
        end ;
      end ;

      sname := Trim( sname ) ;
      if ( LowerCase( sname ) = 'landscape' ) or
         ( LowerCase( sname ) = 'portrait' ) then begin
        sornt := sname ;
        sname := '' ;
      end ;
      prn.Name := sname ;

      sornt := Trim( sornt ) ;
      sornt := LowerCase( sornt ) ;
      if sornt = 'landscape' then
        prn.Landscape := True ;
      if sornt = 'portrait' then
        prn.Portrait := True ;

      if IsStringEmpty( prn.Name ) and
         ( not IsStringEmpty( sval1 ) ) and
         ( not IsStringEmpty( sval2 ) ) then begin
        sval1 := Trim( sval1 ) ;
        try
          dval1 := DotStrToFloat( sval1 ) ;
        except
          dval1 := 0 ;
        end ;
        sval2 := Trim( sval2 ) ;
        try
          dval2 := DotStrToFloat( sval2 ) ;
        except
          dval2 := 0 ;
        end ;
        if ( dval1 > 0 ) and ( dval2 > 0 ) and
           valid_measure( measure1 ) and valid_measure( measure2 ) then begin
          prn.PageSize := Point(
                            TGIS_PrintUtils.ToPixels(
                              dval1, TGIS_PrintUtils.TextToUnit( measure1 ), 600 ),
                            TGIS_PrintUtils.ToPixels(
                              dval2, TGIS_PrintUtils.TextToUnit( measure2 ), 600 )
                          ) ;
          if ( prn.PageSize.X > 10000 ) or
             ( prn.PageSize.Y > 10000 ) or
             ( prn.PageSize.X < 600   ) or
             ( prn.PageSize.Y < 600   ) then
            prn.PageSize := Point( 0, 0 )
          else begin
            prn.PrintArea := Rect( 300, 300,
                                   prn.PageSize.X - 300,
                                   prn.PageSize.Y - 300 ) ;
            prn.PPI := 600 ;
          end;
        end ;
      end ;
    finally
      if not IsStringEmpty( prn.Name ) or
         prn.Landscape or prn.Portrait or
         ( ( prn.PageSize.X > 0 ) and ( prn.PageSize.Y > 0 ) )
         then Result := prn
         else FreeObject( prn ) ;
    end;
  end ;

  class function TGIS_PrintUtils.UnitToText(
    const _u : TGIS_PrintLayoutUnits
  ) : String ;
  begin
    Result := '' ;
    case _u of
      TGIS_PrintLayoutUnits.uCm : Result := TPL_SIZE_CM ;
      TGIS_PrintLayoutUnits.uMm : Result := TPL_SIZE_MM ;
      TGIS_PrintLayoutUnits.uIn : Result := TPL_SIZE_IN ;
      TGIS_PrintLayoutUnits.uPt : Result := TPL_SIZE_PT ;
      TGIS_PrintLayoutUnits.uPx : Result := TPL_SIZE_PX ;
    end ;
  end ;

  class function TGIS_PrintUtils.TextToUnit(
    const _u : String
  ) : TGIS_PrintLayoutUnits ;
  begin
    if      CompareText( _u, TPL_SIZE_CM) = 0 then Result := TGIS_PrintLayoutUnits.uCm
    else if CompareText( _u, TPL_SIZE_MM) = 0 then Result := TGIS_PrintLayoutUnits.uMm
    else if CompareText( _u, TPL_SIZE_IN) = 0 then Result := TGIS_PrintLayoutUnits.uIn
    else if CompareText( _u, TPL_SIZE_PT) = 0 then Result := TGIS_PrintLayoutUnits.uPt
    else                                           Result := TGIS_PrintLayoutUnits.uPx ;
  end ;

  class function TGIS_PrintUtils.WidthToText(
    const _width : TGIS_PrintLayoutWidth
  ) : String ;
  begin
    Result := GIS_PARAMTXT_TYPE_SIZE + ':' + _width.AsText ;
  end ;

  class function TGIS_PrintUtils.PointsToText(
    const _points : Double
  ) : String ;
  begin
    Result := GIS_PARAMTXT_TYPE_SIZE + ':' + FloatToStr( _points ) + TPL_SIZE_PT ;
  end ;

  class function TGIS_PrintUtils.ColorToText(
    const _color : TGIS_Color
  ) : String ;
  begin
    Result := GIS_PARAMTXT_TYPE_ARGB + ':' + IntToHex( _color.ARGB, 8 ) ;
  end ;

  class function TGIS_PrintUtils.TextToColor(
    const _color : String
  ) : TGIS_Color ;
  var
    k : Integer ;
  begin
    Result := TGIS_Color.None ;
    k := Pos( ':', _color ) ;
    if k >= StringFirst then
      if UpperCase( Copy( _color, StringFirst, k-StringFirst ) ) = GIS_PARAMTXT_TYPE_ARGB then
        Result := TGIS_Color.FromARGB(
                    Cardinal( StrToInt( '$' + Copy( _color, k+1, 4096 ) ) )
                  ) ;
  end ;

  class function TGIS_PrintUtils.FontStyleToText(
    _style : TGIS_FontStyles
  ) : String ;
  var
    n : Boolean ;
  begin
    Result := '[' ;
    n := False ;
    {$IFDEF ISLAND}
     //?
    {$ELSE}
      if TGIS_FontStyle.Bold in _style then
      begin
        Result := Result + _rsrc( GIS_RS_TPL_DESIGNER_FONT_BOLD ) ;
        n := True ;
      end;
      if TGIS_FontStyle.Italic in _style then
      begin
        if n then Result := Result + ',' ;
        Result := Result + _rsrc( GIS_RS_TPL_DESIGNER_FONT_ITALIC ) ;
        n := True ;
      end;
      if TGIS_FontStyle.Underline in _style then
      begin
        if n then Result := Result + ',' ;
        Result := Result + _rsrc( GIS_RS_TPL_DESIGNER_FONT_UNDERLINE ) ;
        n := True ;
      end;
      if TGIS_FontStyle.StrikeOut in _style then
      begin
        if n then Result := Result + ',' ;
        Result := Result + _rsrc( GIS_RS_TPL_DESIGNER_FONT_STRIKEOUT ) ;
      end;
    {$ENDIF}
    Result := Result + ']' ;
  end ;

  class function TGIS_PrintUtils.SnapToText(
    const _snap : TGIS_PrintLayoutSnap
  ) : String ;
  begin
    Result := GIS_PARAMTXT_TYPE_SIZE + ':' + _snap.AsText ;
  end ;

  class function TGIS_PrintUtils.ToTwips(
    const _value : String ;
    const _unit  : String ;
    const _ppi   : Integer
  ) : Integer ;
  var
    v : Double ;
  begin
    Result := 0 ;
    v := DotStrToFloat( _value ) ;
    if _unit = TPL_SIZE_CM then
      Result := RoundS( v * 1440 / 2.54 )
    else
    if _unit = TPL_SIZE_MM then
      Result := RoundS( v * 1440 / 2.54 / 10 )
    else
    if _unit = TPL_SIZE_IN then
      Result := RoundS( v * 1440 )
    else
    if _unit = TPL_SIZE_PT then
      Result := RoundS( v * 1440 / 72 )
    else
    if _unit = TPL_SIZE_PX then
      Result := RoundS( v * 1440 / _ppi ) ;
  end ;

  class function TGIS_PrintUtils.FromTwips(
    const _value : Integer ;
    const _unit  : String  ;
    const _ppi   : Integer
  ) : Double ;
  begin
    Result := 0 ;
    if _unit = TPL_SIZE_CM then
      Result := _value * 2.54 / 1440
    else
    if _unit = TPL_SIZE_MM then
      Result := _value * 2.54 * 10 / 1440
    else
    if _unit = TPL_SIZE_IN then
      Result := _value / 1440
    else
    if _unit = TPL_SIZE_PT then
      Result := _value * 72 / 1440
    else
    if _unit = TPL_SIZE_PX then
      Result := _value * _ppi / 1440 ;
  end ;

  class function TGIS_PrintUtils.ToPixels(
    const _value : Double ;
    const _unit  : TGIS_PrintLayoutUnits ;
    const _ppi   : Integer
  ) : Integer ;
  begin
    Result := 0 ;
    case _unit of
      TGIS_PrintLayoutUnits.uCm : Result := RoundS( _value * _ppi / 2.54 ) ;
      TGIS_PrintLayoutUnits.uMm : Result := RoundS( _value * _ppi / 25.4 ) ;
      TGIS_PrintLayoutUnits.uIn : Result := RoundS( _value * _ppi ) ;
      TGIS_PrintLayoutUnits.uPt : Result := RoundS( _value * _ppi / 72 ) ;
      TGIS_PrintLayoutUnits.uPx : Result := RoundS( _value ) ;
    end ;
  end ;

  class function TGIS_PrintUtils.FromPixels(
    const _value : Integer ;
    const _unit  : TGIS_PrintLayoutUnits ;
    const _ppi   : Integer
  ) : Double ;
  begin
    Result := 0 ;
    case _unit of
      TGIS_PrintLayoutUnits.uCm : Result := _value * 2.54 / _ppi ;
      TGIS_PrintLayoutUnits.uMm : Result := _value * 25.4 / _ppi ;
      TGIS_PrintLayoutUnits.uIn : Result := _value / _ppi ;
      TGIS_PrintLayoutUnits.uPt : Result := _value * 72 / _ppi ;
      TGIS_PrintLayoutUnits.uPx : Result := _value ;
    end ;
  end ;

//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    TGIS_PageSizeTable := TGIS_PageSizeTable_class.Create ;

  finalization
    FreeObject( TGIS_PageSizeTable ) ;
{$ENDIF}

{==================================== END =====================================}
end.
