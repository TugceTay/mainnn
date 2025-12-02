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
  Encapsulation of configuration file reader.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoSldFiles ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoSldFiles"'}
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

{$IFDEF CLR}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoXmlDoc ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF COCOA}
  uses
    TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL,
  TatukGIS.RTL.XML ;
{$ENDIF}

type
  /// <summary>
  ///   Encapsulation of sld configuration file reader.
  /// </summary>
  TGIS_SldFile = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    private // other private variable
        FDoc            : TGIS_XMLDocument ;
        FRoot           : IXMLNode ;
        FLayer          : TGIS_Layer ;
        FFileName       : String ;
        FActiveSection  : TGIS_ParamsSection ;
        FNumFStyles     : Integer  ;
        FNumLStyles     : Integer  ;
    private // other private functions
      procedure applyParamsList         ;
      procedure parseRule               ( const _rule       : IXMLNode
                                        ) ;
      function  parseFilter             ( const _operator   : String ;
                                          const _rule       : IXMLNode
                                        ) : String ;
      function  readValue               ( const _node       : IXMLNode
                                        ) : String ;
      function  convertCompOperator     ( const _op         : String
                                        ) : String ;
      function  convertMarkNameToStyle  ( const _mark       : String
                                        ) : TGIS_MarkerStyle ;
      function  convertMarkNameToSymbol ( const _mark       : String
                                        ) : String ;
      function  convertColor            ( const _value      : String
                                        ) : TGIS_Color ;
      function  convertLabelPosition    ( const _anchor     : TGIS_Point
                                        ) : TGIS_LabelPositions ;
      function  makeColor               ( const _color      : TGIS_Color ;
                                          const _opacity    : Double
                                        ) : TGIS_Color ;
      function  makeLineDash            ( const _dash       : String ;
                                          const _offset     : Double ;
                                          const _poffset    : Integer
                                        ) : String ;
      procedure parseProperty           ( const _prop       : IXMLNode ;
                                            var _opname     : String ;
                                            var _name       : String ;
                                            var _literal    : String
                                        ) ;
      procedure parseFill               ( const _fill       : IXMLNode ;
                                            var _color      : TGIS_Color ;
                                            var _bitmap     : TGIS_Bitmap ;
                                            var _symbol     : String ;
                                            var _opacity    : Double ;
                                            var _size       : Double ;
                                            var _rotation   : Double
                                        ) ;
      procedure parseStroke             ( const _stroke     : IXMLNode ;
                                            var _scolor     : TGIS_Color ;
                                            var _fcolor     : TGIS_Color ;
                                            var _width      : Double ;
                                            var _size       : Double ;
                                            var _dasharray  : String ;
                                            var _symbol     : String ;
                                            var _rotation   : Double ;
                                            var _opacity    : Double ;
                                            var _offset     : Double
                                        ) ;
      procedure parseGraphicsStroke     ( const _stroke     : IXMLNode ;
                                            var _scolor     : TGIS_Color ;
                                            var _fcolor     : TGIS_Color ;
                                            var _width      : Double ;
                                            var _size       : Double ;
                                            var _dasharray  : String ;
                                            var _symbol     : String ;
                                            var _rotation   : Double ;
                                            var _opacity    : Double
                                        ) ;
      procedure parsePointPlacement     ( const _placement  : IXMLNode ;
                                            var _anchor     : TGIS_Point ;
                                            var _displace   : TGIS_Point ;
                                            var _rotation   : Double
                                        ) ;
      procedure parseDisplacement       ( const _displace   : IXMLNode ;
                                            var _offsetX    : Integer ;
                                            var _offsetY    : Integer
                                        ) ;

      procedure parsePointSymbolizer    ( const _symbolizer : IXMLNode
                                        ) ;
      procedure parseLineSymbolizer     ( const _symbolizer : IXMLNode
                                        ) ;
      procedure parsePolygonSymbolizer  ( const _symbolizer : IXMLNode
                                        ) ;
      procedure parseTextSymbolizer     ( const _symbolizer : IXMLNode
                                        ) ;
      procedure parseRasterSymbolizer   ( const _symbolizer : IXMLNode
                                        ) ;
      protected
        procedure doDestroy       ; override;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_fileName">
      ///   configuration file name
      /// </param>
      constructor Create          ( const _fileName : String
                                  ) ;

      /// <summary>
      ///   Read configuration from file and apply to a layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to which the configuration will be applied
      /// </param>
      procedure ReadConfig        ( const _layer : TGIS_Layer
                                  ) ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

//==============================================================================
// TGIS_SldFile
//==============================================================================


  constructor TGIS_SldFile.Create(
    const _fileName : String
  );
  begin
    inherited Create ;

    FDoc := TGIS_XMLDocument.Create ;

    FFileName := _fileName ;

    if SafeFileExists(_fileName) then
      FDoc.LoadFromFile( _fileName ) ;

    FRoot := FDoc.DocumentElement ;
  end ;

  procedure TGIS_SldFile.doDestroy ;
  begin
    FreeObject( FDoc ) ;

    inherited ;
  end ;

  function TGIS_SldFile.readValue(
    const _node : IXMLNode
  ) : String ;
  var
    lnode : IXMLNode ;
  begin
    lnode := _node.ChildNodes.FindNode( 'Literal' ) ;
    if assigned( lnode ) then
      Result := lnode.Text
    else
      Result := _node.Text ;
  end ;

  procedure TGIS_SldFile.parseRule(
    const _rule : IXMLNode
  ) ;
  var
    i     : Integer ;
    nd    : IXMLNode ;
    val   : Double ;
    str   : String ;
    dmin  : Double ;
    dmax  : Double ;
  begin
    FActiveSection := FLayer.Params ;

    FNumLStyles := 0 ;

    for i := 0 to _rule.ChildNodes.Count-1 do begin
      nd := _rule.ChildNodes[i] ;
      if nd.LocalName = 'Name' then begin
        FActiveSection.Legend := nd.Text ;
      end
      else if nd.LocalName = 'Title' then
        FActiveSection.Legend := nd.Text
      else if nd.LocalName = 'MaxScaleDenominator' then begin
        val := DotStrToFloat( nd.Text ) ;
        if val <> 0 then
          FActiveSection.MinScale := 1 / val
        else
          FActiveSection.MinScale := val ;
        if FActiveSection.MinScale = 1 then
          FActiveSection.MinScale := 0 ;
      end
      else if nd.LocalName = 'MinScaleDenominator' then begin
        val := DotStrToFloat( nd.Text ) ;
        if val <> 0 then
          FActiveSection.MaxScale := 1 / val
        else
          FActiveSection.MaxScale := val ;
      end
      else if nd.LocalName = 'Filter' then
        TGIS_ParamsSectionVector(FActiveSection).Query := parseFilter( '', nd )
      else if nd.LocalName = 'PointSymbolizer' then
        parsePointSymbolizer( nd )
      else if nd.LocalName = 'LineSymbolizer' then begin
        if FNumLStyles > 0 then begin
          FLayer.ParamsList.Add ;
          str := FActiveSection.Legend ;
          dmin := FActiveSection.MinScale ;
          dmax := FActiveSection.MaxScale ;
          FActiveSection := FLayer.Params ;
          FActiveSection.Legend := str ;
          FActiveSection.MinScale := dmin ;
          FActiveSection.MaxScale := dmax ;
        end ;
        parseLineSymbolizer( nd ) ;
        inc( FNumLStyles ) ;
      end
      else if nd.LocalName = 'PolygonSymbolizer' then
        parsePolygonSymbolizer( nd )
      else if nd.LocalName = 'TextSymbolizer' then
        parseTextSymbolizer( nd )
      else if nd.LocalName = 'RasterSymbolizer' then
        parseRasterSymbolizer( nd )
    end ;
  end ;

  function TGIS_SldFile.convertCompOperator(
    const _op : String
  ) : String ;
  begin
    if (_op = 'PropertyIsEqualTo') or (_op = 'equalTo') then
      Result := '='
    else if (_op = 'PropertyIsNotEqualTo') or (_op = 'notEqual') then
      Result := '<>'
    else if (_op = 'PropertyIsLessThan') or (_op = 'lessThan') then
      Result := '<'
    else if (_op = 'PropertyIsLessThanOrEqualTo') or (_op = 'lessEqualThan') then
      Result := '<='
    else if (_op = 'PropertyIsGreaterThan') or (_op = 'greaterThan') then
      Result := '>'
    else if (_op = 'PropertyIsGreaterThanOrEqualTo') or (_op = 'greaterEqualThan') then
      Result := '>='
    else if (_op = 'PropertyIsLike') or (_op = 'isLike') then
      Result := ' LIKE '
    else if (_op = 'PropertyIsNull') or (_op = 'isNull') then
      Result := ' IS NULL'
    else if (_op = 'PropertyIsBetween') or (_op = 'between') or (_op = 'in') then
      Result := ' IN '
    else
      Result := ''
  end ;

  function TGIS_SldFile.convertMarkNameToStyle(
    const _mark : String
  ) : TGIS_MarkerStyle ;
  begin
    if _mark = 'circle' then
      Result := TGIS_MarkerStyle.Circle
    else if _mark = 'square' then
      Result := TGIS_MarkerStyle.Box
    else if _mark = 'triangle' then
      Result := TGIS_MarkerStyle.TriangleUp
    else if _mark = 'cross' then
      Result := TGIS_MarkerStyle.Cross
    else if _mark = 'star' then
      Result := TGIS_MarkerStyle.DiagCross
    else if _mark = 'x' then
      Result := TGIS_MarkerStyle.DiagCross
    else
      Result := TGIS_MarkerStyle.Box
  end ;

  function TGIS_SldFile.convertMarkNameToSymbol(
    const _mark : String
  ) : String ;
  var
    mstr : String ;
  begin
    mstr := UpperCase( _mark ) ;
    if mstr = 'CIRCLE' then
      Result := 'SYMBOL:LIBSVG:std:Circle01'
    else if mstr = 'SQUARE' then
      Result := 'SYMBOL:LIBSVG:std:Square01'
    else if mstr = 'TRIANGLE' then
      Result := 'SYMBOL:LIBSVG:std:Triangle01'
    else if mstr = 'CROSS' then
      Result := 'SYMBOL:LIBSVG:std:Cross02'
    else if mstr = 'SHAPE://PLUS' then
      Result := 'SYMBOL:LIBSVG:std:Cross02'
    else if mstr = 'STAR' then
      Result := 'SYMBOL:LIBSVG:std:Star01'
    else if mstr = 'X' then
      Result := 'SYMBOL:LIBSVG:std:Star01'
    else if Pos( 'TTF://', mstr ) = StringFirst then begin
      Result := 'SYMBOL:'+ Copy( _mark, StringFirst+6, length( _mark )-6 ) ;
      Result := StringReplaceAll( Result, '#U+', ':$' ) ;
    end
    else if mstr = 'SHAPE://VERTLINE' then
      Result := 'STOCK:VERTICAL'
    else if mstr = 'SHAPE://HORLINE' then
      Result := 'STOCK:HORIZONTAL'
    else if mstr = 'SHAPE://SLASH' then
      Result := 'STOCK:BDIAGONAL'
    else if mstr = 'SHAPE://BACKSLASH' then
      Result := 'STOCK:FDIAGONAL'
    else if mstr = 'SHAPE://PLUS' then
      Result := 'STOCK:CROSS'
    else if mstr = 'SHAPE://TIMES' then
      Result := 'STOCK:DIAGCROSS'
    else if mstr = 'SHAPE://OARROW' then
      Result := 'LIBSVG:std:Triangle02'
    else if mstr = 'SHAPE://CARROW' then
      Result := 'LIBSVG:std:Triangle02'
    else
      Result := 'STOCK:SOLID' ;
  end;

  function TGIS_SldFile.convertColor(
    const _value : String
  ) : TGIS_Color ;
  var
    r,
    g,
    b   : Byte ;
    sr,
    sg,
    sb  : String ;
    mlt : Integer ;
  begin
    Result := TGIS_Color.Black ;

    if IsStringEmpty( _value ) then exit ;

    if Pos( '#', _value ) = StringFirst then begin
      if length( _value ) = 4 then begin
        sr := Copy( _value, StringFirst+1, 1 ) ;
        sg := Copy( _value, StringFirst+2, 1 ) ;
        sb := Copy( _value, StringFirst+3, 1 ) ;
        mlt := 17 ;
      end
      else if length( _value ) = 7 then begin
        sr := Copy( _value, StringFirst+1, 2 ) ;
        sg := Copy( _value, StringFirst+3, 2 ) ;
        sb := Copy( _value, StringFirst+5, 2 ) ;
        mlt := 1 ;
      end
      else begin
        sr := '0' ;
        sg := '0' ;
        sb := '0' ;
        mlt := 1 ;
      end ;
    end
    else if Pos( '0x', _value ) = StringFirst then begin
      sr := Copy( _value, StringFirst+2, 2 ) ;
      sg := Copy( _value, StringFirst+4, 2 ) ;
      sb := Copy( _value, StringFirst+6, 2 ) ;
      mlt := 1 ;
    end
    else begin
      Result := ParamColor( _value, TGIS_Color.Black ) ;
      exit ;
    end ;

    try
      r := StrToInt( '$' + sr )  * mlt ;
    except
      r := 0 ;
    end ;

    try
      g := StrToInt( '$' + sg )  * mlt ;
    except
      g := 0 ;
    end ;

    try
      b := StrToInt( '$' + sb )  * mlt ;
    except
      b := 0 ;
    end ;
    Result := TGIS_Color.FromRGB( r, g, b ) ;
  end ;

  function TGIS_SldFile.convertLabelPosition(
    const _anchor : TGIS_Point
  ) : TGIS_LabelPositions ;
  begin
    if _anchor.X = 0 then begin
      if      _anchor.Y = 0.0 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.UpRight  )
      else if _anchor.Y = 0.5 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.MiddleRight )
      else if _anchor.Y = 1.0 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.DownRight   )
    end
    else if _anchor.X = 0.5 then begin
      if      _anchor.Y = 0.0 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.UpCenter  )
      else if _anchor.Y = 0.5 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter )
      else if _anchor.Y = 1.0 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.DownCenter   )
    end
    else if _anchor.X = 1.0 then begin
      if      _anchor.Y = 0.0 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.UpLeft  )
      else if _anchor.Y = 0.5 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.MiddleLeft )
      else if _anchor.Y = 1.0 then
        Result := GisGetLabelPosition( TGIS_LabelPosition.DownLeft   )
    end ;
  end ;

  function TGIS_SldFile.makeColor(
    const _color      : TGIS_Color ;
    const _opacity    : Double
  ) : TGIS_Color ;
  var
    h,s,l : Double ;
  begin
    _color.ToHSL( h, s, l ) ;

    Result := TGIS_Color.FromAHSL( _opacity, h, s, l ) ;
  end ;

  function TGIS_SldFile.makeLineDash(
    const _dash     : String ;
    const _offset   : Double ;
    const _poffset  : Integer
  ) : String ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
    sb  : TStringBuilder ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( _dash, ' ' ) ;
      sb := TStringBuilder.Create ;
      try
        i := 0 ;
        sb.Append( '&' ) ;
        if _poffset <> 0 then
          sb.Append( Format( 'M(0 %dP)', [_poffset] ) ) ;

        sb.Append( 'F(100%)' ) ;
        while i < tkn.Result.Count-1 do begin
          if _offset <> 0 then
            sb.Append( Format( 'M(%dP 0)', [RoundS(_offset)] ) ) ;

          sb.Append( Format('L(%dP)M(%dP 0)',
                            [RoundS(DotStrToFloat(tkn.Result[i])),
                             RoundS(DotStrToFloat(tkn.Result[i+1]))]
                            )
                    ) ;
          i := i + 2 ;
        end ;
        sb.Append( 'E()' ) ;
        Result := sb.ToString ;
      finally
        FreeObject( sb ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  function TGIS_SldFile.parseFilter(
    const _operator : String ;
    const _rule     : IXMLNode
  ) : String ;
  var
    op       : IXMLNode ;
    i        : Integer ;
    opname   : String ;
    pname    : String ;
    pliteral : String ;
    query    : TStringBuilder ;
    lop      : String ;
    qstr     : String ;
  begin
    query := TStringBuilder.Create ;
    try
      for i := 0 to _rule.ChildNodes.Count-1 do begin
        op := _rule.ChildNodes[i] ;

        if Pos( 'PropertyIs', op.LocalName ) >= StringFirst then begin
          parseProperty( op, opname, pname, pliteral ) ;
          if query.Length <> 0 then
            query.Append( _operator ) ;
          query.Append( Format( '%s%s%s',
                               [pname, convertCompOperator(opname), pliteral]
                              )
                      ) ;
        end
        else if op.LocalName = 'And' then begin
          lop := ' AND ' ;
          qstr := parseFilter( lop, op ) ;
          if query.Length <> 0 then
            query.Append( _operator ) ;
          query.Append( Format( '(%s)', [qstr] ) ) ;
        end
        else if op.LocalName = 'Or' then begin
          lop := ' OR ' ;
          qstr := parseFilter( lop, op ) ;
          if query.Length <> 0 then
            query.Append( _operator ) ;
          query.Append( Format( '(%s)', [qstr] ) ) ;
        end
        else if op.LocalName = 'Not' then begin
          lop := ' NOT ' ;
          qstr := parseFilter( lop, op ) ;
          if query.Length <> 0 then
            query.Append( _operator ) ;
          query.Append( Format( '(%s)', [qstr] ) ) ;
        end
      end ;
      Result := query.ToString ;
    finally
      FreeObject( query ) ;
    end ;

  end ;

  procedure TGIS_SldFile.parseProperty(
    const _prop       : IXMLNode ;
      var _opname     : String ;
      var _name       : String ;
      var _literal    : String
  ) ;
  var
    nd    : IXMLNode ;
    ndf   : IXMLNode ;
    i, j  : Integer ;
    fld   : Integer ;
    quote : Boolean ;
    lv    : TGIS_LayerVector ;
    fnc   : String ;
    llist : TArray<String> ;
    li    : Integer ;

    procedure checkQuote ;
    begin
      quote := False ;
      if FLayer.IsVector then begin
        lv := TGIS_LayerVector(FLayer) ;
        fld := lv.FindField( _name ) ;
        if (fld > -1) and (lv.FieldInfo(fld).FieldType = TGIS_FieldType.String)
        then
          quote := True ;
      end ;
    end;
    
  begin
    _name    := '' ;
    _literal := '' ;
    _opname  := _prop.LocalName ;

    for i := 0 to _prop.ChildNodes.Count-1 do begin
      nd := _prop.ChildNodes[i] ;
      if nd.LocalName = 'PropertyName' then
        _name := nd.Text
      else if nd.LocalName = 'Literal' then begin
        checkQuote ;
        
        if (nd.Text = 'true') and (length( llist ) > 0) then begin
          _literal := _literal.Join( ',', llist ) ;
          if fnc <> '' then
            _opname  := fnc ;
        end  
        else begin
          if quote or (_opname = 'PropertyIsLike' ) then
            _literal := QuotedStr( nd.Text )
          else
            _literal := nd.Text
        end ;
      end
      else if nd.LocalName = 'Function' then begin
        fnc := VarToString(nd.Attributes['name']) ;
        ndf := nd.ChildNodes.FindNode( 'PropertyName' ) ;
        if assigned( ndf ) then begin
          _name := ndf.Text ;
          SetLength( llist, nd.ChildNodes.Count-1 ) ;
          li := 0 ;
          checkQuote ;
          for j := 0 to nd.ChildNodes.Count-1 do
            if nd.ChildNodes[j].LocalName = 'Literal' then begin
              if quote then
                llist[li] := QuotedStr(nd.ChildNodes[j].Text) 
              else
                llist[li] := nd.ChildNodes[j].Text ;
              inc( li ) ;
            end ;
        end
      end ;
    end ;
  end ;

  procedure TGIS_SldFile.parseFill(
    const _fill       : IXMLNode ;
      var _color      : TGIS_Color ;
      var _bitmap     : TGIS_Bitmap ;
      var _symbol     : String ;
      var _opacity    : Double ;
      var _size       : Double ;
      var _rotation   : Double
  ) ;
  var
    k         : Integer ;
    n         : Integer ;
    ndk       : IXMLNode ;
    ndn       : IXMLNode ;
    ndi       : IXMLNode ;
    ndnn      : IXMLNode ;
    str       : String ;
    path      : String ;
    size      : Double ;
    width     : Double ;
    dasharray : String ;
    opacity   : Double ;
    scolor    : TGIS_Color ;
    fcolor    : TGIS_Color ;
  begin
    _bitmap  := nil ;
    _opacity := 1 ;
    _color   := TGIS_Color.Black ;
    _size    := 1 ;
    _symbol  := '' ;

    for k := 0 to _fill.ChildNodes.Count-1 do begin
      ndk := _fill.ChildNodes[k]  ;
      if (ndk.LocalName = 'CssParameter') or
         (ndk.LocalName = 'SvgParameter') then begin
        if VarToString( ndk.Attributes['name'] ) = 'fill' then
          _color := convertColor( readValue(ndk) )
        else if VarToString( ndk.Attributes['name'] ) = 'fill-opacity' then
          _opacity := DotStrToFloat( readValue(ndk) )
      end
      else if ndk.LocalName = 'GraphicFill' then begin
        ndn := ndk.ChildNodes.FindNode( 'Graphic' ) ;
        if assigned( ndn ) then begin
          for n := 0 to ndn.ChildNodes.Count-1 do begin
            ndnn := ndn.ChildNodes[n] ;
            if ndnn.LocalName = 'ExternalGraphic' then begin
              ndi := ndnn.ChildNodes.FindNode('OnlineResource') ;
              if assigned( ndi ) then begin
                str := VarToString( ndi.Attributes['xlink:href'] ) ;
                path := GetPathAbsolute( GetFilePath( FFileName ), str ) ;
                _bitmap := TGIS_Bitmap.Create ;
                _bitmap.LoadFromFile( path ) ;
              end ;
            end
            else if ndnn.LocalName = 'Size' then begin
              {TODO - no option to scale tiled bitmap to fill }
              _size := DotStrToFloat( ndnn.Text )
            end
            else if ndnn.LocalName = 'Mark' then begin
              parseGraphicsStroke( ndk, scolor, fcolor, width, size, dasharray,
                                  _symbol, _rotation, opacity ) ;
              _color := scolor ;
            end
          end ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_SldFile.parseStroke(
    const _stroke     : IXMLNode ;
    var _scolor     : TGIS_Color ;
    var _fcolor     : TGIS_Color ;
    var _width      : Double ;
    var _size       : Double ;
    var _dasharray  : String ;
    var _symbol     : String ;
    var _rotation   : Double ;
    var _opacity    : Double ;
    var _offset     : Double
  ) ;
  var
    k    : Integer ;
    ndk  : IXMLNode ;
  begin
    _opacity    := 1 ;
    _scolor     := TGIS_Color.Black ;
    _fcolor     := TGIS_Color.Black ;
    _width      := 1 ;
    _size       := 1 ;
    _symbol     := '' ;
    _dasharray  := '' ;
    _offset     := 0 ;

    for k := 0 to _stroke.ChildNodes.Count-1 do begin
      ndk := _stroke.ChildNodes[k] ;

      if (ndk.LocalName = 'CssParameter') or
         (ndk.LocalName = 'SvgParameter') then begin
        if VarToString( ndk.Attributes['name'] ) = 'stroke' then
          _scolor := convertColor( readValue(ndk) )
        else if VarToString( ndk.Attributes['name'] ) = 'stroke-width' then
          _width := DotStrToFloat( readValue(ndk) )
        else if VarToString( ndk.Attributes['name'] ) = 'stroke-opacity' then
          _opacity := DotStrToFloat( readValue(ndk) )
        else if VarToString( ndk.Attributes['name'] ) = 'stroke-linejoin' then

        else if VarToString( ndk.Attributes['name'] ) = 'stroke-linecap' then

        else if VarToString( ndk.Attributes['name'] ) = 'stroke-dasharray' then
          _dasharray := readValue(ndk)
        else if VarToString( ndk.Attributes['name'] ) = 'stroke-dashoffset' then
          _offset := DotStrToFloat( readValue(ndk) )
      end
      else if ndk.LocalName = 'GraphicStroke' then begin
        parseGraphicsStroke( ndk, _scolor, _fcolor, _width, _size, _dasharray, _symbol, _rotation, _opacity ) ;
      end ;
    end ;
  end ;

  procedure TGIS_SldFile.parseGraphicsStroke(
    const _stroke     : IXMLNode ;
      var _scolor     : TGIS_Color ;
      var _fcolor     : TGIS_Color ;
      var _width      : Double ;
      var _size       : Double ;
      var _dasharray  : String ;
      var _symbol     : String ;
      var _rotation   : Double ;
      var _opacity    : Double
  ) ;
  var
    ngr       : IXMLNode ;
    nd        : IXMLNode ;
    ndi       : IXMLNode ;
    i, j      : Integer ;
    str       : String ;
    iopacity  : Double ;
    opacity   : Double ;
    frgb      : TGIS_Color ;
    srgb      : TGIS_Color ;
    path      : String ;
    bmp       : TGIS_Bitmap ;
    size      : Double ;
    width     : Double ;
    rot       : Double ;
    symbol    : String ;
    adash     : String ;
    offset    : Double ;
  begin
    _scolor     := TGIS_Color.Black ;
    _fcolor     := TGIS_Color.Black ;
    ngr := _stroke.ChildNodes.FindNode( 'Graphic' ) ;
    if not assigned( ngr ) then exit ;

    opacity := 1 ;
    for i := 0 to ngr.ChildNodes.Count-1 do begin
      nd := ngr.ChildNodes[i] ;
      if nd.LocalName = 'Mark' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndi := nd.ChildNodes[j] ;
          if ndi.LocalName = 'WellKnownName' then
            _symbol := convertMarkNameToSymbol( ndi.Text )
          else if ndi.LocalName = 'Fill' then begin
            parseFill( ndi, frgb, bmp, symbol, iopacity, size, rot );

            _fcolor := makeColor( frgb, iopacity * opacity ) ;
            if assigned( bmp ) then begin
              FreeObject( bmp ) ;
            end ;
          end
          else if ndi.LocalName = 'Stroke' then begin
            parseStroke( ndi, srgb, frgb, width, size, adash, symbol, rot, iopacity, offset );
            _scolor := makeColor( srgb, opacity * iopacity ) ;
            _width := RoundS(width) ;
          end ;
        end ;
      end
      else if nd.LocalName = 'ExternalGraphic' then begin
        ndi := nd.ChildNodes.FindNode('OnlineResource') ;
        if assigned( ndi ) then begin
          str := VarToString( ndi.Attributes['xlink:href'] ) ;
          path := GetPathAbsolute( GetFilePath( FFileName ), str ) ;
          _symbol := 'SYMBOL:' + path ;
        end ;
      end
      else if nd.LocalName = 'Opacity' then
        _opacity := DotStrToFloat( readValue( nd ) )
      else if nd.LocalName = 'Size' then
        _size := RoundS(DotStrToFloat( readValue( nd ) ))
      else if nd.LocalName = 'Rotation' then
        _rotation := DotStrToFloat( readValue( nd ) ) * Pi / 180
    end ;


  end;

  procedure TGIS_SldFile.parsePointPlacement(
    const _placement  : IXMLNode ;
      var _anchor     : TGIS_Point ;
      var _displace   : TGIS_Point ;
      var _rotation   : Double
  ) ;
  var
    k    : Integer ;
    ndk  : IXMLNode ;
  begin
    _anchor   := GisPoint( 0, 0 ) ;
    _displace := GisPoint( 0, 0 ) ;
    _rotation := 0 ;

    for k := 0 to _placement.ChildNodes.Count-1 do begin
      ndk := _placement.ChildNodes[k] ;

      if ndk.LocalName = 'AnchorPoint' then begin
        _anchor.X := DotStrToFloat( ndk.ChildNodes['AnchorPointX'].Text ) ;
        _anchor.Y := DotStrToFloat( ndk.ChildNodes['AnchorPointY'].Text ) ;
      end
      else if ndk.LocalName = 'Displacement' then begin
        _displace.X := DotStrToFloat( ndk.ChildNodes['DisplacementX'].Text ) ;
        _displace.Y := DotStrToFloat( ndk.ChildNodes['DisplacementY'].Text ) ;
      end
      else if ndk.LocalName = 'Rotation' then begin
        _rotation := DotStrToFloat( ndk.Text ) ;
      end ;
    end ;
  end ;

  procedure TGIS_SldFile.parseDisplacement(
    const _displace   : IXMLNode ;
      var _offsetX    : Integer ;
      var _offsetY    : Integer
  ) ;
  var
    k    : Integer ;
    ndk  : IXMLNode ;
  begin
    _offsetX := 0 ;
    _offsetY := 0 ;

    for k := 0 to _displace.ChildNodes.Count-1 do begin
      ndk := _displace.ChildNodes[k] ;

      if ndk.LocalName = 'DisplacementX' then
        _offsetX := RoundS( DotStrToFloat( ndk.Text ) )
      else if ndk.LocalName = 'DisplacementY' then
        _offsetY := RoundS( DotStrToFloat( ndk.Text ) )
    end ;
  end;


  procedure TGIS_SldFile.parsePointSymbolizer(
    const _symbolizer : IXMLNode
  ) ;
  var
    ngr       : IXMLNode ;
    nd        : IXMLNode ;
    ndi       : IXMLNode ;
    i, j      : Integer ;
    pm        : TGIS_ParamsSectionVector ;
    str       : String ;
    iopacity  : Double ;
    opacity   : Double ;
    frgb      : TGIS_Color ;
    srgb      : TGIS_Color ;
    path      : String ;
    bmp       : TGIS_Bitmap ;
    size      : Double ;
    width     : Double ;
    rot       : Double ;
    symbol    : String ;
    dasharray : String ;
    offset    : Double ;
    http_res  : TGIS_HttpResponse ;
  begin
    pm := FActiveSection as TGIS_ParamsSectionVector ;

    ngr := _symbolizer.ChildNodes.FindNode( 'Graphic' ) ;
    if not assigned( ngr ) then exit ;

    pm.Marker.ShowLegend := True ;
    opacity := 1 ;
    for i := 0 to ngr.ChildNodes.Count-1 do begin
      nd := ngr.ChildNodes[i] ;
      if nd.LocalName = 'Mark' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndi := nd.ChildNodes[j] ;
          if ndi.LocalName = 'WellKnownName' then
            pm.Marker.Style := convertMarkNameToStyle( ndi.Text )
          else if ndi.LocalName = 'Fill' then begin
            parseFill( ndi, frgb, bmp, symbol, iopacity, size, rot );

            pm.Marker.Color  := makeColor( frgb, iopacity * opacity ) ;
            if assigned( bmp ) then begin
              pm.Marker.Bitmap := bmp ;
              FreeObject( bmp ) ;
            end ;
          end
          else if ndi.LocalName = 'Stroke' then begin
            parseStroke( ndi, srgb, frgb, width, size, dasharray, symbol, rot, iopacity, offset );
            pm.Marker.OutlineColor := makeColor( srgb, opacity * iopacity ) ;
            pm.Marker.OutlineWidth := -RoundS(width) ;
          end ;
        end ;
      end
      else if nd.LocalName = 'ExternalGraphic' then begin
        ndi := nd.ChildNodes.FindNode('OnlineResource') ;
        if assigned( ndi ) then begin
          str := VarToString( ndi.Attributes['xlink:href'] ) ;
          path := GetPathAbsolute( GetFilePath( FFileName ), str ) ;
          if IsServerPath( path ) then begin
            http_res := TGIS_WebUtils.HttpFetch( path ) ;
            if http_res.Status = GIS_HTTP_OK then begin
              try
                pm.Marker.Symbol := SymbolList.Prepare( path, http_res.Stream ) ;
              finally
                FreeObject( http_res.Stream ) ;
              end ;
            end
          end
          else
            pm.Marker.Symbol := SymbolList.Prepare( path ) ;
        end ;
      end
      else if nd.LocalName = 'Opacity' then
        opacity := DotStrToFloat( nd.Text )
      else if nd.LocalName = 'Size' then
        pm.Marker.Size := -RoundS(DotStrToFloat( nd.Text ))
      else if nd.LocalName = 'Rotation' then
        {TODO - rotation should be applied to stardard marker too}
        pm.Marker.SymbolRotate := DotStrToFloat( nd.Text ) * Pi / 180
    end ;


  end ;

  procedure TGIS_SldFile.parseLineSymbolizer(
    const _symbolizer : IXMLNode
  ) ;
  var
    nd        : IXMLNode ;
    i         : Integer ;
    pm        : TGIS_ParamsSectionVector ;
    iopacity  : Double ;
    frgb      : TGIS_Color ;
    srgb      : TGIS_Color ;
    size      : Double ;
    width     : Double ;
    rot       : Double ;
    symbol    : String ;
    dasharray : String ;
    offset    : Double ;
    poffset   : Integer ;
  begin
    pm := FActiveSection as TGIS_ParamsSectionVector ;
    pm.Line.ShowLegend := True ;
    poffset := 0 ;
    for i := 0 to _symbolizer.ChildNodes.Count-1 do begin
      nd := _symbolizer.ChildNodes[i] ;
      if nd.LocalName = 'Stroke' then begin
        parseStroke( nd, srgb, frgb, width, size, dasharray, symbol, rot, iopacity, offset );

        if IsStringEmpty( symbol ) then begin
          pm.Line.Color := makeColor( srgb, iopacity ) ;
          pm.Line.Width := -RoundS(width) ;
        end
        else begin
          pm.Line.Color := makeColor( frgb, iopacity ) ;
          pm.Line.OutlineColor := makeColor( srgb, iopacity ) ;
          pm.Line.Width := -RoundS(size) ;
          pm.Line.StyleAsText := symbol ;
          pm.Line.SymbolRotate := rot ;
        end ;
      end
      else if nd.LocalName = 'PerpendicularOffset' then
        poffset := RoundS(DotStrToFloat( nd.Text )) ;
    end ;

    if not IsStringEmpty( dasharray ) then
      pm.Line.StyleAsText := GIS_PARAMTXT_TYPE_SYMBOL + ':' + makeLineDash( dasharray, offset, poffset ) ;
  end ;

  procedure TGIS_SldFile.parsePolygonSymbolizer(
    const _symbolizer : IXMLNode
  ) ;
  var
    nd        : IXMLNode ;
    i         : Integer ;
    pm        : TGIS_ParamsSectionVector ;
    iopacity  : Double ;
    frgb      : TGIS_Color ;
    srgb      : TGIS_Color ;
    size      : Double ;
    width     : Double ;
    rot       : Double ;
    symbol    : String ;
    dasharray : String ;
    offset    : Double ;
    offX      : Integer ;
    offY      : Integer ;
    bmp       : TGIS_Bitmap ;
  begin
    pm := FActiveSection as TGIS_ParamsSectionVector ;
    pm.Area.ShowLegend := True ;
    pm.Area.Pattern := TGIS_BrushStyle.Clear ;

    for i := 0 to _symbolizer.ChildNodes.Count-1 do begin
      nd := _symbolizer.ChildNodes[i] ;
      if nd.LocalName = 'Stroke' then begin
        parseStroke( nd, srgb, frgb, width, size, dasharray, symbol, rot, iopacity, offset );
        pm.Area.OutlineColor := makeColor( srgb, iopacity ) ;
        pm.Area.OutlineWidth := -RoundS(width) ;
      end
      else if nd.LocalName = 'Fill' then begin
        parseFill( nd, frgb, bmp, symbol, iopacity, size, rot );
        pm.Area.Pattern := TGIS_BrushStyle.Solid ;
        pm.Area.Color  := makeColor( frgb, iopacity ) ;
        if assigned( bmp ) then begin
          pm.Area.Bitmap := bmp ;
          FreeObject( bmp ) ;
        end ;
        if not IsStringEmpty( symbol ) then begin
          pm.Area.PatternAsText := symbol ;
          pm.Area.SymbolSize := -RoundS(size) ;
          pm.Area.SymbolRotate := rot ;
        end;
      end
      else if nd.LocalName = 'Displacement' then begin
        parseDisplacement( nd, offX, offY );
        pm.Area.OffsetX := offX ;
        pm.Area.OffsetY := offY ;
      end
    end ;

    {TODO - Rule can have both PolygonSymbolizer and LineSymbolizer as outline symbol }
  end ;

  procedure TGIS_SldFile.parseTextSymbolizer(
    const _symbolizer : IXMLNode
  ) ;
  var
    nd        : IXMLNode ;
    ndi       : IXMLNode ;
    i, j      : Integer ;
    pm        : TGIS_ParamsSectionVector ;
    iopacity  : Double ;
    frgb      : TGIS_Color ;
    size      : Double ;
    bmp       : TGIS_Bitmap ;
    anchor    : TGIS_Point ;
    displace  : TGIS_Point ;
    rotation  : Double ;
    symbol    : String ;
  begin
    pm := FActiveSection as TGIS_ParamsSectionVector ;

    for i := 0 to _symbolizer.ChildNodes.Count-1 do begin
      nd := _symbolizer.ChildNodes[i] ;
      if nd.LocalName = 'Label' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndi := nd.ChildNodes[j] ;
          if ndi.LocalName = 'PropertyName' then
            pm.Labels.Value := Format( '{%s}', [ndi.Text] ) ;
        end ;
      end
      else if nd.LocalName = 'Font' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndi := nd.ChildNodes[j] ;
          if (ndi.LocalName = 'CssParameter') or
             (ndi.LocalName = 'SvgParameter') then begin
            if VarToString( ndi.Attributes['name'] ) = 'font-family' then
              pm.Labels.Font.Name := ndi.Text
            else if VarToString( ndi.Attributes['name'] ) = 'font-style' then begin
              if ndi.Text = 'italic' then
                pm.Labels.FontStyle := GisAddFontStyle( pm.Labels.FontStyle,
                                                        TGIS_FontStyle.Italic )
            end
            else if VarToString( ndi.Attributes['name'] ) = 'font-weight' then begin
              if ndi.Text = 'bold' then
                pm.Labels.FontStyle := GisAddFontStyle( pm.Labels.FontStyle,
                                                        TGIS_FontStyle.Bold )
            end
            else if VarToString( ndi.Attributes['name'] ) = 'font-size' then
              pm.Labels.Font.Size := StrToIntDef( ndi.Text, 0 ) ;
          end ;

        end ;
      end
      else if nd.LocalName = 'LabelPlacement' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndi := nd.ChildNodes[j] ;
          if ndi.LocalName = 'PointPlacement' then begin
            parsePointPlacement( ndi, anchor, displace, rotation ) ;
            pm.Labels.Rotate   := rotation * Pi / 180 ;
            pm.Labels.Position := convertLabelPosition( anchor ) ;
          end
          else if ndi.LocalName = 'LinePlacement' then begin
            pm.Labels.Alignment := TGIS_LabelAlignment.Follow ;
            {TODO - support for PerpendicularOffset for label}
          end ;
        end ;
      end
      else if nd.LocalName = 'Halo' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndi := nd.ChildNodes[j] ;
          if ndi.LocalName = 'Fill' then begin
            parseFill( ndi, frgb, bmp, symbol, iopacity, size, rotation );
            pm.Labels.Color      := makeColor( frgb, iopacity ) ;
          end ;
        end ;
      end
      else if nd.LocalName = 'Fill' then begin
        parseFill( nd, frgb, bmp, symbol, iopacity, size, rotation );

        pm.Labels.Color      := makeColor( frgb, iopacity ) ;
        pm.Labels.Font.Color := pm.Labels.Color ;
        if assigned( bmp ) then begin
          pm.Labels.Bitmap := bmp ;
          FreeObject( bmp ) ;
        end ;
      end
      else if nd.LocalName = 'Graphic' then begin

      end
      else if nd.LocalName = 'Priority' then begin

      end
    end ;
  end ;

  procedure TGIS_SldFile.parseRasterSymbolizer(
    const _symbolizer : IXMLNode
  ) ;
  var
    nd        : IXMLNode ;
    ndj1      : IXMLNode ;
    ndj2      : IXMLNode ;
    i, j      : Integer ;
    pm        : TGIS_ParamsSectionPixel ;
    iopacity  : Double ;
    frgb      : TGIS_Color ;
    scolor    : String ;
  begin
    if not (FActiveSection is TGIS_ParamsSectionPixel) then exit ;

    pm := FActiveSection as TGIS_ParamsSectionPixel ;
    ndj2 := nil ;
    iopacity := 1 ;
    for i := 0 to _symbolizer.ChildNodes.Count-1 do begin
      nd := _symbolizer.ChildNodes[i] ;
      if nd.LocalName = 'Opacity' then begin
        iopacity := DotStrToFloat( nd.Text )
      end
      else if nd.LocalName = 'ColorMap' then begin
        j := 0 ;
        while j < nd.ChildNodes.Count-1 do begin
          ndj1 := nd.ChildNodes[j] ;
          ndj2 := nd.ChildNodes[j+1] ;
          if ndj1.LocalName = 'ColorMapEntry' then begin
            frgb := convertColor( VarToString(ndj1.ChildValues['color']) ) ;
            frgb := makeColor( frgb, iopacity ) ;
            scolor := Format( '%d:%d:%d:%d',
                              [ frgb.R,frgb.G,frgb.B,frgb.A ]
                             ) ;

            pm.Pixel.AltitudeMapZones.Add(
              Format( '%s,%s,%s,%s',
                      [ ndj1.ChildValues['quantity'],
                        ndj2.ChildValues['quantity'],
                        scolor,
                        ndj1.ChildValues['label'] ]
                     )
              ) ;
          end ;
          j := j + 1 ;
        end ;
        if assigned( ndj2 ) then
          frgb   := convertColor( VarToString(ndj2.ChildValues['color']) ) ;
        frgb := makeColor( frgb, iopacity ) ;
        scolor := Format( '%d:%d:%d:%d',
                          [ frgb.R,frgb.G,frgb.B,frgb.A ]
                         ) ;
        pm.Pixel.AltitudeMapZones.Add(
          Format( '%s,%s,%s,%s',
                  [ ndj2.ChildValues['quantity'],
                    '',
                    scolor,
                    ndj2.ChildValues['label'] ]
                 )
          ) ;
      end
      else if nd.LocalName = 'ChannelSelection' then begin
        {TODO - add channel mapping}
      end
      else if nd.LocalName = 'ContrastEnhancement' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndj1 := nd.ChildNodes[j] ;
          if ndj1.LocalName = 'Normalize' then
            pm.Pixel.ContrastEnhanced := True
          else if ndj1.LocalName = 'Histogram' then
            pm.Pixel.Histogram := True
          else if ndj1.LocalName = 'GammaValue' then
            pm.Pixel.Brightness := TruncS( DotStrToFloat( ndj1.Text ) * 100 ) ;
        end ;
      end
      else if nd.LocalName = 'ShadedRelief' then begin
        pm.Pixel.GridShadow := True ;
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndj1 := nd.ChildNodes[j] ;
          if ndj1.LocalName = 'ReliefFactor' then
            if FLayer.IsGrid then
              pm.Pixel.GridShadowAngle := DotStrToFloat( ndj1.Text ) ;
        end
      end ;
    end ;
  end ;

  procedure TGIS_SldFile.applyParamsList ;
  var
    nlayer   : IXMLNode ;
    nstyle   : IXMLNode ;
    nftstyle : IXMLNode ;
    nrule    : IXMLNode ;
    i, j     : Integer  ;
    numrule  : Integer  ;
  begin
    nlayer := FRoot.ChildNodes.FindNode( 'NamedLayer' ) ;
    if not assigned( nlayer ) then
      nlayer := FRoot.ChildNodes.FindNode( 'UserLayer' ) ;
    if not assigned( nlayer ) then exit ;

    nstyle := nlayer.ChildNodes.FindNode( 'NamedStyle' ) ;
    if not assigned( nstyle ) then
      nstyle := nlayer.ChildNodes.FindNode( 'UserStyle' ) ;
    if not assigned( nstyle ) then exit ;

    FLayer.ParamsList.ClearAndSetDefaults ;

    FNumFStyles := 0 ;
    numrule := 0 ;
    for i := 0 to nstyle.ChildNodes.Count-1 do begin
      nftstyle := nstyle.ChildNodes[i] ;
      if nftstyle.LocalName = 'FeatureTypeStyle' then begin
        inc( FNumFStyles ) ;
        for j := 0 to nftstyle.ChildNodes.Count-1 do begin
          nrule := nftstyle.ChildNodes[j] ;
          if nrule.LocalName = 'Rule' then begin
            if numrule > 0 then
              FLayer.ParamsList.Add ;

            parseRule( nrule ) ;
            inc( numrule ) ;
          end ;
        end ;
      end ;
    end ;

    if FLayer.IsVector then
      TGIS_LayerVector(FLayer).MultipassRendering := ( FNumFStyles > 1 ) or
                                                     ( FNumLStyles > 1 )  ;
  end ;

  procedure TGIS_SldFile.ReadConfig(
    const _layer : TGIS_Layer
  ) ;
  begin
    if not assigned( _layer ) then exit ;
    if not assigned( FRoot )  then exit ;

    FLayer := _layer ;
    applyParamsList ;
  end ;

//==================================== END =====================================
end.
