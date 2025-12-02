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
  Encapsulation of Mapbox Vector Tiles styler.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoMVTStyler ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoMVTStyler"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
uses
  System.Classes,
  System.Generics.Collections,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoLayer ;
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
  ///   Encapsulation of Mapbox Vector Tiles styler.
  /// </summary>
  TGIS_MVTStyler = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      oConverter : TObject ;
    private
      function  fget_BackgroundColor : TGIS_Color ;
      function  fget_SpritesUrl      : String ;
      function  fget_PixelSizeFactor : Double ;
      procedure fset_PixelSizeFactor ( const _factor : Double
                                     ) ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Clear internal data.
      /// </summary>
      procedure Clear ;

      /// <summary>
      ///   Parse style.
      /// </summary>
      /// <param name="_path">
      ///   path to style data (url, path)
      /// </param>
      procedure ParseStyle       ( const _path : String
                                 ) ;
      /// <summary>
      ///   Parse sprites resources.
      /// </summary>
      /// <param name="_path">
      ///   path to style data (url, path)
      /// </param>
      /// <param name="_key">
      ///   key for style access
      /// </param>
      procedure ParseSprites     ( const _path : String ;
                                   const _key  : String
                                 ) ;
      /// <summary>
      ///   Build params for layer based on level.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <param name="_level">
      ///   level of styling
      /// </param>
      procedure BuildParams      ( const _layer : TGIS_Layer ;
                                   const _level : Integer
                                 ) ;
      /// <summary>
      ///   Sort layers by style order.
      /// </summary>
      /// <param name="_layers">
      ///   list of layers
      /// </param>
      procedure SortLayers       ( const _layers : TGIS_LayerAbstractList
                                 ) ;

      /// <summary>
      ///   Get sprite symbol by name.
      /// </summary>
      /// <param name="_name">
      ///   name of symbol
      /// </param>
      /// <returns>
      ///   symbol if found
      /// </returns>
      function GetSprite          ( const _name : String
                                  ) : TGIS_SymbolAbstract ;

      {$IFDEF GIS_TEST}
      procedure Dump( const _list   : TStrings
                    ) ; overload ;
      procedure Dump( const _list   : TStrings ;
                      const _layers : TGIS_LayerAbstractList
                    ) ; overload ;
      {$ENDIF}
    public
      /// <summary>
      ///   Style background color.
      /// </summary>
      property BackgroundColor : TGIS_Color read fget_BackgroundColor ;

      /// <summary>
      ///   Url to sprites resource.
      /// </summary>
      property SpritesUrl      : String read fget_SpritesUrl ;

      /// <summary>
      ///   Pixel size factor. Affects width and size properties. Default is 1.
      /// </summary>
      property PixelSizeFactor : Double read  fget_PixelSizeFactor
                                        write fset_PixelSizeFactor ;
  end ;

  {$IFDEF GENDOC}
  {$ELSE}
  TGIS_MVTQuery = class( TGIS_Object )
    public
      Field : String ;
      Cases : TDictionary<String, Variant> ;
    protected
      procedure doDestroy ; override ;
    public
      constructor Create( const _field : String
                        ) ; overload ;
      constructor Create( const _field      : String ;
                          {$IFDEF DCC}
                          const _collection : TEnumerable<TPair<String, Variant>>
                          {$ENDIF}
                          {$IFDEF CLR}
                          const _collection : IDictionary<String, Variant>
                          {$ENDIF}
                          {$IFDEF JAVA}
                          const _collection : TDictionary<String, Variant>
                          {$ENDIF}
                        ) ; overload ;

      procedure Add     ( const _key : String ;
                          const _val : Variant
                        ) ;
      function  CreateCopy : TGIS_MVTQuery ;
  end ;

  TGIS_MVTCustomSymbol = class(TGIS_ParamsUserObject)
  public
    SymbolName        : String ;
    SymbolWidth       : Single ;
    Transparency      : Single ;
    QFillColor        : TGIS_MVTQuery ;
    QFillPattern      : TGIS_MVTQuery ;
    QFillOutlineColor : TGIS_MVTQuery ;
    QFillOpacity      : TGIS_MVTQuery ;
    QLineColor        : TGIS_MVTQuery ;
    QLineWidth        : TGIS_MVTQuery ;
    QLineOpacity      : TGIS_MVTQuery ;
    QTextSize         : TGIS_MVTQuery ;
    QTextColor        : TGIS_MVTQuery ;
    QTextHaloColor    : TGIS_MVTQuery ;
    QSymbolSize       : TGIS_MVTQuery ;
  protected
    procedure doDestroy ; override ;
  public
    constructor Create ;
    function CreateCopy : TGIS_ParamsUserObject ; override ;
  end ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.Math,
  System.Rtti,
  System.SysUtils,
  System.Generics.Defaults,
  Lider.CG.GIS.GeoFileJSON,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoLogger,
  Lider.CG.GIS.GeoInternals ;
{$ENDIF}

type
  T_ParamsStops<T> = record
    Zoom  : Integer ;
    Value : T ;
    Key   : String ;
    Field : String ;
    constructor Create( const _zoom   : Integer ;
                        const _value  : T
                       ) ; overload ;
    constructor Create( const _field  : String ;
                        const _key    : String ;
                        const _value  : T
                       ) ; overload ;
    constructor Create( const _zoom   : Integer ;
                        const _field  : String ;
                        const _key    : String ;
                        const _value  : T
                       ) ; overload ;
  end ;

  T_NativePropertyType = (
    Color,
    Numeric,
    Opacity,
    Text
  ) ;

  T_NativeProperty = class
    private
      NativeType : T_NativePropertyType ;
    public
      Color     : TList< T_ParamsStops<TGIS_Color> > ;
      Numeric   : TList< T_ParamsStops<Single> > ;
      Text      : TList< T_ParamsStops<String> > ;
      BaseValue : Double ;
    public
      constructor Create( const _type : T_NativePropertyType
                        ) ; overload ;
      constructor Create( const _type : T_NativePropertyType ;
                          const _base : Double
                        ) ; overload ;
      destructor  Destroy; override ;

      procedure Add( const _field  : String ;
                     const _key    : String ;
                     const _value  : TGIS_Color
                    ) ; overload ;
      procedure Add( const _field  : String ;
                     const _key    : String ;
                     const _value  : Single
                    ) ; overload ;
      procedure Add( const _field  : String ;
                     const _key    : String ;
                     const _value  : String
                    ) ; overload ;
      procedure Add( const _zoom   : Integer ;
                     const _value  : TGIS_Color
                    ) ; overload ;
      procedure Add( const _zoom   : Integer ;
                     const _value  : Single
                    ) ; overload ;
      procedure Add( const _zoom   : Integer ;
                     const _value  : String
                    ) ; overload ;
      procedure Add( const _zoom   : Integer ;
                     const _prop   : T_NativeProperty
                    ) ; overload ;
  end ;


  T_PropertyType = (
    All,
    FillColor,
    FillPattern,
    FillOutlineColor,
    FillOpacity,
    LineColor,
    LineWidth,
    LineOpacity,
    TextSize,
    TextColor,
    TextHaloColor,
    SymbolSize
  ) ;

  T_Property = class
    private
      PType : T_PropertyType ;
    public
      BaseValue        : Double ;
      Background       : TGIS_Color ;
      FillColor        : TList< T_ParamsStops<TGIS_Color> > ;
      FillOutlineColor : TList< T_ParamsStops<TGIS_Color> > ;
      FillOpacity      : TList< T_ParamsStops<Single> > ;
      FillPattern      : String ;
      LineColor        : TList< T_ParamsStops<TGIS_Color> > ;
      LineWidth        : TList< T_ParamsStops<Single> > ;
      LineOpacity      : TList< T_ParamsStops<Single> > ;
      LineDash         : Boolean ;
      LineDashArray    : TArray<Single> ;
      LineOffset       : Single ;
      TextSize         : TList< T_ParamsStops<Single> > ;
      TextFontFamily   : String ;
      TextFontStyle    : TGIS_FontStyles ;
      TextField        : String ;
      TextAlongLine    : Boolean ;
      TextRotate       : Single ;
      TextRotateField  : String ;
      TextWrap         : Boolean ;
      TextJustify      : TGIS_LabelAlignment ;
      TextPositions    : TGIS_LabelPositions ;
      TextColor        : TList< T_ParamsStops<TGIS_Color> > ;
      TextHaloColor    : TList< T_ParamsStops<TGIS_Color> > ;
      SymbolImage      : String ;
      SymbolSize       : TList< T_ParamsStops<Single> > ;
      SymbolColor      : TGIS_Color ;
      SymbolRotate     : Single ;
    public
      constructor Create ; overload ;
      constructor Create( const _type : T_PropertyType
                        ) ; overload ;
      destructor  Destroy; override ;

      function  AsString : String ;
      procedure SetProperty( const _type : T_PropertyType ;
                             const _prop : T_NativeProperty
                            ) ;
  end ;

  T_StyleLayerType = (
    Fill,
    Line,
    Symbol,
    Background
  ) ;

  T_StyleLayer = class
    public
      Id          : String ;
      LayerType   : T_StyleLayerType ;
      SourceLayer : String ;
      MinLevel    : Integer ;
      MaxLevel    : Integer ;
      Visible     : Boolean ;
      Query       : String ;
      GLProperty  : T_Property ;
    public
      constructor Create ;
      destructor  Destroy; override ;

      function  AsString : String ;
      procedure Read( const _jlayer : TGIS_JSONObject ) ;
  end ;

  T_StyleConverter = class
    private
      FSpritesUrl      : String ;
      FSpritesDict     : TDictionary< String, TStream> ;
      FColorDict       : TDictionary< String, TGIS_Color > ;
      FShieldDict      : TDictionary< String, String > ;
      FLayers          : TList<T_StyleLayer> ;
      FPixelSizeFactor : Double ;
    private
      function  loadFromStream            ( const _stm      : TStream
                                          ) : String ;
      function  loadFromService           ( const _path     : String
                                          ) : String ;
      function  loadFromFile              ( const _path     : String
                                          ) : String ;
      function  loadImage                 ( const _path     : String
                                          ) : TGIS_Bitmap ;
    private
      procedure parseLayers               ( const _layers   : TGIS_JSONArray
                                          ) ;
      function  parseFilter               ( const _obj      : TGIS_JSONObject
                                          ) : String ;
      procedure parsePaint                ( const _obj      : TGIS_JSONObject ;
                                            const _layer    : T_StyleLayer
                                          ) ;
      procedure parseFill                 ( const _obj      : TGIS_JSONObject ;
                                            const _layer    : T_StyleLayer
                                          ) ;
      function  parseInterpolateColorByZoom( const _obj       : TGIS_JSONObject ;
                                             const _layer     : T_StyleLayer ;
                                               var _fillColor : TGIS_Color
                                           ) : T_NativeProperty ;
      function  parseInterpolateByZoom     ( const _obj       : TGIS_JSONObject ;
                                             const _layer     : T_StyleLayer ;
                                               var _defNumber : Single
                                           ) : T_NativeProperty ;
      function  parseInterpolateOpacityByZoom( const _obj     : TGIS_JSONObject ;
                                               const _layer   : T_StyleLayer
                                             ) : T_NativeProperty ;
      function  parseInterpolatePointByZoom( const _obj       : TGIS_JSONObject ;
                                             const _layer     : T_StyleLayer
                                           ) : T_NativeProperty ;
      function  parseInterpolateListByZoom( const _obj       : TGIS_JSONArray ;
                                            const _type      : T_NativePropertyType ;
                                            const _layer     : T_StyleLayer ;
                                              var _defColor  : TGIS_Color ;
                                              var _defNumber : Single
                                          ) : T_NativeProperty ;
      function  parseMatchList            ( const _obj       : TGIS_JSONArray ;
                                            const _type      : T_NativePropertyType ;
                                              var _defColor  : TGIS_Color ;
                                              var _defNumber : Single ;
                                            const _base      : Double
                                          ) : T_NativeProperty ;
      function  parseValueList            ( const _obj      : TGIS_JSONArray ;
                                            const _type     : T_NativePropertyType ;
                                            const _layer    : T_StyleLayer ;
                                              var _color    : TGIS_Color ;
                                              var _number   : Single
                                          ) : T_NativeProperty ;
      procedure parseLine                 ( const _obj      : TGIS_JSONObject ;
                                            const _layer    : T_StyleLayer
                                          ) ;
      procedure parseSymbolPaint          ( const _obj      : TGIS_JSONObject ;
                                            const _layer    : T_StyleLayer
                                          ) ;
      procedure parseSymbolLayout         ( const _obj      : TGIS_JSONObject ;
                                            const _layer    : T_StyleLayer
                                          ) ;
      procedure parseLayout               ( const _obj      : TGIS_JSONObject ;
                                            const _layer    : T_StyleLayer
                                          ) ;
      function  processColorList          ( const _level    : Integer ;
                                            const _list     : TList<T_ParamsStops<TGIS_Color>> ;
                                              var _query    : TGIS_MVTQuery
                                          ) : TGIS_Color ;
      function  processValueList          ( const _level    : Integer ;
                                            const _list     : TList<T_ParamsStops<Single>> ;
                                            const _base     : Double ;
                                              var _query    : TGIS_MVTQuery
                                          ) : Single ;
      function  buildFill                 ( const _level    : Integer ;
                                            const _glProp   : T_Property ;
                                            const _area     : TGIS_ParamsArea
                                          ) : Boolean ;
      function  buildLine                 ( const _level    : Integer ;
                                            const _glProp   : T_Property ;
                                            const _line     : TGIS_ParamsLine ;
                                            const _area     : TGIS_ParamsArea
                                          ) : Boolean ;
      function  buildSymbol               ( const _level    : Integer ;
                                            const _glProp   : T_Property ;
                                            const _marker   : TGIS_ParamsMarker ;
                                            const _label    : TGIS_ParamsLabel
                                          ) : Boolean ;
    private
      function fget_BackgroundColor : TGIS_Color ;
    public
      constructor Create ;
      destructor  Destroy; override ;

      procedure Clear ;
      procedure ParseStyle                 ( const _path : String
                                           ) ;
      procedure ParseSprites               ( const _path : String ;
                                             const _key  : String
                                           ) ;
      procedure BuildParams                ( const _layer : TGIS_Layer ;
                                             const _level : Integer
                                           ) ;
      procedure SortLayers                 ( const _layer : TGIS_LayerAbstractList
                                           ) ;
      function GetSprite                  ( const _name : String ) : TGIS_SymbolAbstract ;
    public
      property Layers          : TList<T_StyleLayer> read FLayers ;
      property SpritesUrl      : String     read  FSpritesUrl
                                            write FSpritesUrl ;
      property PixelSizeFactor : Double     read  FPixelSizeFactor
                                            write FPixelSizeFactor ;
      property BackgroundColor : TGIS_Color read  fget_BackgroundColor ;
  end ;

  T_ExpressionParser = class
    private
      class function  parseValue         ( const _val      : TGIS_JSONObject
                                          ) : String ; static ;
      class function  parseKey           ( const _val      : TGIS_JSONObject
                                          ) : String ; static ;
    public
      class function ParseExpression     ( const _expr : TGIS_JSONArray
                                         ) : String ; static ;
  end ;

  T_ConverterHelper = class
    class function  ParseColor          ( const _val        : String
                                         ) : TGIS_Color ;
    class function  ParseWidth          ( const _val        : String
                                         ) : Single ;
    class function  ParseSize           ( const _val        : String
                                         ) : Single ;
    class function  InterpolateValue    ( const _startValue : Double ;
                                          const _endValue   : Double ;
                                          const _minLevel   : Double ;
                                          const _maxLevel   : Double ;
                                          const _level      : Double
                                        ) : Double ;
    class function InterpolateValueExp  ( const _startValue : Double ;
                                          const _endValue   : Double ;
                                          const _minLevel   : Double ;
                                          const _maxLevel   : Double ;
                                          const _level      : Double ;
                                          const _base       : Double
                                        ) : Double ;

  end ;

  {$IFDEF JAVA}
  T_listSortByOrder = class( java.util.Comparator<TGIS_LayerAbstract> )
    public
      function    compare ( _item1    : TGIS_LayerAbstract ;
                            _item2    : TGIS_LayerAbstract
                          ) : Integer ;
  end ;

  function T_listSortByOrder.compare(
    _item1 : TGIS_LayerAbstract ;
    _item2 : TGIS_LayerAbstract
  ) : Integer ;
  begin
    Result := SortByOrder( _item1, _item2 ) ;
  end ;
  {$ENDIF}

//=============================================================================
// T_StyleLayer
//=============================================================================

constructor T_StyleLayer.Create ;
begin
  inherited ;

  Id          := '' ;
  SourceLayer := '' ;
  MinLevel    := -1 ;
  MaxLevel    := -1 ;
  Visible     := True ;
  Query       := '' ;

  GLProperty := T_Property.Create ;
end ;

destructor T_StyleLayer.Destroy ;
begin
  FreeObject( GLProperty ) ;

  inherited ;
end ;

procedure T_StyleLayer.Read(
  const _jlayer : TGIS_JSONObject
) ;
var
  vis,
  ltype : String ;
  v     : Double ;
begin
  Id := _jlayer.GetValue<String>('id') ;
  if not _jlayer.TryGetValue<String>('type', ltype) then exit;

  if ltype = 'fill' then
    LayerType := T_StyleLayerType.Fill
  else if ltype = 'line' then
    LayerType := T_StyleLayerType.Line
  else if ltype = 'symbol' then
    LayerType := T_StyleLayerType.Symbol
  else if ltype = 'background' then
    LayerType := T_StyleLayerType.Background ;

  vis := '' ;
  if _jlayer.TryGetValue<String>('source-layer', vis ) then
    SourceLayer := vis ;

  v := -1 ;
  if _jlayer.TryGetValue<Double>('minzoom', v) then
    MinLevel := RoundS( v ) ;

  v := -1 ;
  if _jlayer.TryGetValue<Double>('maxzoom', v) then
    MaxLevel := RoundS( v ) ;

  if _jlayer.TryGetValue<String>('visibility', vis) then
    Visible := vis <> 'none'
  else
    Visible := True ;
end ;

function T_StyleLayer.AsString : String ;
begin
  Result := 'Id: ' + Id + #13#10 +
            'Source: ' + SourceLayer + #13#10 ;
  case LayerType of
    T_StyleLayerType.Fill:
      Result := Result + 'Type: Fill' + #13#10 ;
    T_StyleLayerType.Line:
      Result := Result + 'Type: Line' + #13#10 ;
    T_StyleLayerType.Symbol:
      Result := Result + 'Type: Symbol' + #13#10 ;
  end ;
  Result := Result + 'Visible: ' + BoolToStr(Visible,True) + #13#10 ;
  Result := Result + 'Min:Max level: ' + MinLevel.ToString + ' : ' + MaxLevel.ToString + #13#10 ;
  if not IsStringEmpty( Query ) then
    Result := Result + 'Query: ' + Query + #13#10 ;

  Result := Result + GLProperty.AsString ;

end ;

//=============================================================================
// T_Property
//=============================================================================

constructor T_Property.Create ;
begin
  inherited ;

  Background        := TGIS_Color.None ;
  PType             := T_PropertyType.All ;
  FillColor         := TList< T_ParamsStops<TGIS_Color> >.Create ;
  FillOutlineColor  := TList< T_ParamsStops<TGIS_Color> >.Create ;
  FillOpacity       := TList< T_ParamsStops<Single> >.Create ;
  FillPattern       := '' ;
  LineColor         := TList< T_ParamsStops<TGIS_Color> >.Create ;
  LineWidth         := TList< T_ParamsStops<Single> >.Create ;
  LineOpacity       := TList< T_ParamsStops<Single> >.Create ;
  LineDashArray     := nil ;
  LineOffset        := 0 ;
  TextSize          := TList< T_ParamsStops<Single> >.Create ;
  TextFontFamily    := '' ;
  TextFontStyle     := [] ;
  TextRotate        := 0 ;
  TextRotateField   := '' ;
  TextColor         := TList< T_ParamsStops<TGIS_Color> >.Create ;
  TextHaloColor     := TList< T_ParamsStops<TGIS_Color> >.Create ;
  SymbolSize        := TList< T_ParamsStops<Single> >.Create ;
  SymbolColor       := TGIS_Color.None ;
end ;

constructor T_Property.Create(
  const _type : T_PropertyType
) ;
begin
  Background      := TGIS_Color.None ;
  FillColor       := nil ;
  FillOutlineColor:= nil ;
  FillOpacity     := nil ;
  FillPattern     := '' ;
  LineColor       := nil ;
  LineWidth       := nil ;
  LineOpacity     := nil ;
  LineDashArray   := nil ;
  LineOffset      := 0 ;
  TextSize        := nil ;
  TextFontFamily  := '' ;
  TextFontStyle   := [] ;
  TextColor       := nil ;
  TextRotate      := 0 ;
  TextRotateField := '' ;
  SymbolSize      := nil ;
  SymbolColor     := TGIS_Color.None ;

  PType := _type ;
  case _type of
    T_PropertyType.FillColor   :
      FillColor   := TList< T_ParamsStops<TGIS_Color> >.Create ;
    T_PropertyType.FillOutlineColor   :
      FillOutlineColor   := TList< T_ParamsStops<TGIS_Color> >.Create ;
    T_PropertyType.FillOpacity :
      FillOpacity := TList< T_ParamsStops<Single> >.Create ;
    T_PropertyType.LineColor   :
      LineColor   := TList< T_ParamsStops<TGIS_Color> >.Create ;
    T_PropertyType.LineWidth   :
      LineWidth   := TList< T_ParamsStops<Single> >.Create ;
    T_PropertyType.LineOpacity   :
      LineOpacity := TList< T_ParamsStops<Single> >.Create ;
    T_PropertyType.TextSize   :
      TextSize := TList< T_ParamsStops<Single> >.Create ;
    T_PropertyType.TextColor   :
      TextColor := TList< T_ParamsStops<TGIS_Color> >.Create ;
    T_PropertyType.SymbolSize   :
      SymbolSize := TList< T_ParamsStops<Single> >.Create ;
  end ;
end ;

destructor T_Property.Destroy ;
begin
  FreeObject( FillColor       ) ;
  FreeObject( FillOutlineColor) ;
  FreeObject( FillOpacity     ) ;
  FreeObject( LineColor       ) ;
  FreeObject( LineWidth       ) ;
  FreeObject( LineOpacity     ) ;
  FreeObject( TextSize        ) ;
  FreeObject( TextColor       ) ;
  FreeObject( TextHaloColor   ) ;
  FreeObject( SymbolSize      ) ;

  inherited ;
end ;

procedure T_Property.SetProperty(
  const _type : T_PropertyType;
  const _prop : T_NativeProperty
) ;
begin
  BaseValue := _prop.BaseValue ;
  case _type of
    T_PropertyType.FillColor   :
      FillColor.AddRange( _prop.Color ) ;
    T_PropertyType.FillOutlineColor :
      FillOutlineColor.AddRange( _prop.Color ) ;
    T_PropertyType.FillOpacity :
      FillOpacity.AddRange( _prop.Numeric ) ;
    T_PropertyType.LineColor :
      LineColor.AddRange( _prop.Color ) ;
    T_PropertyType.LineWidth :
      LineWidth.AddRange( _prop.Numeric ) ;
    T_PropertyType.LineOpacity :
      LineOpacity.AddRange( _prop.Numeric ) ;
    T_PropertyType.TextSize :
      TextSize.AddRange( _prop.Numeric ) ;
    T_PropertyType.TextColor :
      TextColor.AddRange( _prop.Color ) ;
    T_PropertyType.TextHaloColor :
      TextHaloColor.AddRange( _prop.Color ) ;
    T_PropertyType.SymbolSize :
      SymbolSize.AddRange( _prop.Numeric ) ;
  end ;
  FreeObjectNotNil( _prop ) ;
end ;

function T_Property.AsString: string;
var
  st : T_ParamsStops<TGIS_Color> ;
  sp : T_ParamsStops<Single> ;
  sl : T_ParamsStops<Single> ;
  {$IFDEF DCC}
  pos : TGIS_LabelPosition ;
  {$ENDIF}
begin
  Result := '' ;
  if Background <> TGIS_Color.None then begin
    Result := Result + 'BackgroundColor: ' + #13#10 ;
    Result := Result + Format('  $%x', [Background.ARGB] ) + #13#10 ;
  end ;

  if FillColor.Count > 0 then begin
    Result := Result + 'FillColor: ' + #13#10 ;
    for st in FillColor do
      Result := Result + Format('  %d [%s %s] $%x', [st.Zoom, st.Field, st.Key, st.Value.ARGB] ) + #13#10 ;
  end ;
  if FillOutlineColor.Count > 0 then begin
    Result := Result + 'FillOutlineColor: ' + #13#10 ;
    for st in FillOutlineColor do
      Result := Result + Format('  %d [%s] $%x', [st.Zoom, st.Key, st.Value.ARGB] ) + #13#10 ;
  end ;
  if not IsStringEmpty( FillPattern ) then
    Result := Result + 'FillPattern: ' + FillPattern + #13#10 ;

  if FillOpacity.Count > 0 then begin
    Result := Result + 'FillOpacity: ' + #13#10 ;
    for sp in FillOpacity do
      Result := Result + Format('  %d [%s %s] %f', [sp.Zoom, sp.Field, sp.Key, sp.Value] ) + #13#10 ;
  end ;

  if LineColor.Count > 0 then begin
    Result := Result + 'LineColor: ' + #13#10 ;
    for st in LineColor do
      Result := Result + Format('  %d [%s %s] $%x', [st.Zoom, st.Field, st.Key, st.Value.ARGB] ) + #13#10 ;
  end ;

  if LineWidth.Count > 0 then begin
    Result := Result + 'LineWidth: ' + #13#10 ;
    for sl in LineWidth do
      Result := Result + Format('  %d [%s %s] %f', [sl.Zoom, sl.Field, sl.Key, sl.Value] ) + #13#10 ;
  end ;

  if LineOpacity.Count > 0 then begin
    Result := Result + 'LineOpacity: ' + #13#10 ;
    for sp in LineOpacity do
      Result := Result + Format('  %d [%s %s] %f', [sp.Zoom, sp.Field, sp.Key, sp.Value] ) + #13#10 ;
  end ;

  if TextSize.Count > 0 then begin
    Result := Result + 'TextSize: ' + #13#10 ;
    for sp in TextSize do
      Result := Result + Format('  %d [%s %s] %f', [sp.Zoom, sp.Field, sp.Key, sp.Value] ) + #13#10 ;
  end ;

  if length( TextFontFamily ) > 0 then
    Result := Result + 'TextFontFamily: ' + TextFontFamily + #13#10 ;

  if TextFontStyle <> GisGetEmptyFontStyle then begin
    Result := Result + 'TextFontStyle: ' ;
    if TGIS_FontStyle.Bold in TextFontStyle then
      Result := Result + 'Bold ' ;
    if TGIS_FontStyle.Italic in TextFontStyle then
      Result := Result + 'Italic ' ;
    Result := Result + #13#10 ;
  end ;

  if length( TextField ) > 0 then begin
    Result := Result + 'TextField: ' + TextField + #13#10 ;
  end ;

  if TextAlongLine then
    Result := Result + 'TextAlongLine: TRUE' + #13#10 ;
  if TextWrap then
    Result := Result + 'TextWrap: TRUE' + #13#10 ;
  if LineDash then
    Result := Result + 'LineDash: TRUE' + #13#10 ;

  if TextColor.Count > 0 then begin
    Result := Result + 'TextColor: ' + #13#10 ;
    for st in TextColor do
      Result := Result + Format('  %d [%s %s] $%x', [st.Zoom, st.Field, st.Key, st.Value.ARGB] ) + #13#10 ;
  end ;

  if TextHaloColor.Count > 0 then begin
    Result := Result + 'TextHaloColor: ' + #13#10 ;
    for st in TextHaloColor do
      Result := Result + Format('  %d [%s %s] $%x', [st.Zoom, st.Field, st.Key, st.Value.ARGB] ) + #13#10 ;
  end ;

  if not IsStringEmpty( SymbolImage ) then
    Result := Result + 'SymbolImage: ' + SymbolImage + #13#10 ;

  if SymbolSize.Count > 0 then begin
    Result := Result + 'SymbolSize: ' + #13#10 ;
    for sl in SymbolSize do
      Result := Result + Format('  %d [%s %s] %f', [sl.Zoom, sl.Field, sl.Key, sl.Value] ) + #13#10 ;
  end ;

  if SymbolColor <> TGIS_Color.None then begin
    Result := Result + 'SymbolColor: ' + #13#10 ;
    Result := Result + Format('  $%x', [SymbolColor.ARGB] ) + #13#10 ;
  end ;

  if SymbolRotate <> 0 then
    Result := Result + 'SymbolRotate: ' + SymbolRotate.ToString + #13#10 ;

  if TextPositions <> GisGetEmptyLabelPosition then begin
    Result := Result + 'TextPositions: ' ;
    for pos in TextPositions do
      {$IFDEF DCC}
      Result := Result + TRttiEnumerationType.GetName(pos) + ' ';
      {$ENDIF}
      {$IFDEF CLR}
      Result := Result + pos.ToString() + ' ';
      {$ENDIF}
    Result := Result + #13#10 ;
  end ;

  if TextJustify <> TGIS_LabelAlignment.Single then
    {$IFDEF DCC}
    Result := Result + 'TextJustify: ' + TRttiEnumerationType.GetName(TextJustify) + #13#10 ;
    {$ENDIF}
    {$IFDEF CLR}
    Result := Result + TextJustify.ToString() + ' ';
    {$ENDIF}
end ;

//=============================================================================
// T_ParamsStops
//=============================================================================

constructor T_ParamsStops<T>.Create(
  const _zoom   : Integer ;
  const _value  : T
) ;
begin
  Zoom  := _zoom ;
  Value := _value ;
  Field := '' ;
  Key   := '' ;
end ;

constructor T_ParamsStops<T>.Create(
  const _field  : String ;
  const _key    : String ;
  const _value  : T
) ;
begin
  Zoom  := -1 ;
  Value := _value ;
  Key   := _key ;
  Field := _field ;
end ;

constructor T_ParamsStops<T>.Create(
  const _zoom   : Integer ;
  const _field  : String ;
  const _key    : String ;
  const _value  : T
) ;
begin
  Zoom  := _zoom ;
  Value := _value ;
  Key   := _key ;
  Field := _field ;
end ;

//=============================================================================
// T_NativeProperty
//=============================================================================

constructor T_NativeProperty.Create(
  const _type : T_NativePropertyType
) ;
begin
  inherited Create ;

  NativeType := _type ;
  case NativeType of
    T_NativePropertyType.Color :
      Color := TList< T_ParamsStops<TGIS_Color> >.Create ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric := TList< T_ParamsStops<Single> >.Create ;
    T_NativePropertyType.Text :
      Text := TList< T_ParamsStops<String> >.Create ;
  end ;
  BaseValue := 1 ;
end ;

constructor T_NativeProperty.Create(
  const _type : T_NativePropertyType ;
  const _base : Double
) ;
begin
  inherited Create ;

  NativeType := _type ;
  case NativeType of
    T_NativePropertyType.Color :
      Color := TList< T_ParamsStops<TGIS_Color> >.Create ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric := TList< T_ParamsStops<Single> >.Create ;
    T_NativePropertyType.Text :
      Text := TList< T_ParamsStops<String> >.Create ;
  end ;
  BaseValue := _base ;
end ;

destructor T_NativeProperty.Destroy ;
begin
  FreeObject( Color   ) ;
  FreeObject( Numeric ) ;
  FreeObject( Text    ) ;

  inherited ;
end ;

procedure T_NativeProperty.Add(
  const _field  : String ;
  const _key    : String ;
  const _value  : String
) ;
begin
  case NativeType of
    T_NativePropertyType.Color :
      Color.Add( T_ParamsStops<TGIS_Color>.Create( _field, _key, T_ConverterHelper.ParseColor( _value ) ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric.Add( T_ParamsStops<Single>.Create( _field, _key, DotStrToFloat( _value ) ) ) ;
    T_NativePropertyType.Text :
      Text.Add( T_ParamsStops<String>.Create( _field, _key, _value ) ) ;
  end ;
end ;

procedure T_NativeProperty.Add(
  const _field  : String ;
  const _key    : String ;
  const _value  : Single
) ;
begin
  case NativeType of
    T_NativePropertyType.Color :
      Color.Add( T_ParamsStops<TGIS_Color>.Create( _field, _key, TGIS_Color.FromABGR(RoundS(_value) ) ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric.Add( T_ParamsStops<Single>.Create( _field, _key, _value ) ) ;
    T_NativePropertyType.Text :
      Text.Add( T_ParamsStops<String>.Create( _field, _key, DotFloatToStr( _value ) ) ) ;
  end ;
end ;

procedure T_NativeProperty.Add(
  const _field  : String ;
  const _key    : String ;
  const _value  : TGIS_Color
) ;
begin
  case NativeType of
    T_NativePropertyType.Color :
      Color.Add( T_ParamsStops<TGIS_Color>.Create( _field, _key, _value ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric.Add( T_ParamsStops<Single>.Create( _field, _key, _value.ARGB ) ) ;
    T_NativePropertyType.Text :
      Text.Add( T_ParamsStops<String>.Create( _field, _key, IntToHex( _value.ToARGB, 8 ) ) ) ;
  end ;
end ;

procedure T_NativeProperty.Add(
  const _zoom   : Integer ;
  const _value  : String
) ;
begin
  case NativeType of
    T_NativePropertyType.Color :
      Color.Add( T_ParamsStops<TGIS_Color>.Create( _zoom, T_ConverterHelper.ParseColor( _value ) ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric.Add( T_ParamsStops<Single>.Create( _zoom, DotStrToFloat( _value ) ) ) ;
    T_NativePropertyType.Text :
      Text.Add( T_ParamsStops<String>.Create( _zoom, _value ) ) ;
  end ;
end ;

procedure T_NativeProperty.Add(
  const _zoom   : Integer ;
  const _value  : Single
) ;
begin
  case NativeType of
    T_NativePropertyType.Color :
      Color.Add( T_ParamsStops<TGIS_Color>.Create( _zoom, TGIS_Color.FromABGR(RoundS(_value) ) ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric.Add( T_ParamsStops<Single>.Create( _zoom, _value ) ) ;
    T_NativePropertyType.Text :
      Text.Add( T_ParamsStops<String>.Create( _zoom, DotFloatToStr( _value ) ) ) ;
  end ;
end ;

procedure T_NativeProperty.Add(
  const _zoom   : Integer ;
  const _value  : TGIS_Color
) ;
begin
  case NativeType of
    T_NativePropertyType.Color :
      Color.Add( T_ParamsStops<TGIS_Color>.Create( _zoom, _value ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      Numeric.Add( T_ParamsStops<Single>.Create( _zoom, _value.ARGB ) ) ;
    T_NativePropertyType.Text :
      Text.Add( T_ParamsStops<String>.Create( _zoom, IntToHex( _value.ToARGB, 8 ) ) ) ;
  end ;
end ;

procedure T_NativeProperty.Add(
  const _zoom : Integer ;
  const _prop : T_NativeProperty
) ;
var
  pc  :  T_ParamsStops<TGIS_Color> ;
  ps  :  T_ParamsStops<Single> ;
  pss :  T_ParamsStops<String> ;
begin
  BaseValue := _prop.BaseValue ;
  case _prop.NativeType of
    T_NativePropertyType.Color :
      for pc in _prop.Color do
        Color.Add( T_ParamsStops<TGIS_Color>.Create( _zoom, pc.Field, pc.Key, pc.Value ) ) ;
    T_NativePropertyType.Numeric,
    T_NativePropertyType.Opacity :
      for ps in _prop.Numeric do
        Numeric.Add( T_ParamsStops<Single>.Create( _zoom, ps.Field, ps.Key, ps.Value ) ) ;
    T_NativePropertyType.Text :
      for pss in _prop.Text do
        Text.Add( T_ParamsStops<String>.Create( _zoom, pss.Field, pss.Key, pss.Value ) ) ;
  end ;
end ;

//=============================================================================
// T_ConverterHelper
//=============================================================================

class function T_ConverterHelper.ParseColor(
  const _val  : String
) : TGIS_Color ;
var
  a,h,
  s, l    : Single ;
  colors  : TArray<String> ;
  str     : String ;
  c       : TGIS_Color ;
  r,g,
  b,aa    : Byte ;
  len     : Integer ;
  val     : String ;
begin
  if _val.StartsWith('#') then begin
    len := length(_val) ;
    r := 0 ;
    g := 0 ;
    b := 0 ;
    if len = 4 then begin  //      #fff
      val := Copy( _val, StringFirst+1, 1 ) ;
      r := StrToInt( '$' + val + val ) ;
      val := Copy( _val, StringFirst+2, 1 ) ;
      g := StrToInt( '$' + val + val ) ;
      val := Copy( _val, StringFirst+3, 1 ) ;
      b := StrToInt( '$' + val + val ) ;
    end
    else if len = 7 then begin //       #ffffff
      val := '$' + Copy( _val, StringFirst+1, 2 ) ;
      r := StrToInt( val ) ;
      val := '$' + Copy( _val, StringFirst+3, 2 ) ;
      g := StrToInt( val ) ;
      val := '$' + Copy( _val, StringFirst+5, 2 ) ;
      b := StrToInt( val ) ;
    end ;
    c := TGIS_Color.FromRGB( r, g, b ) ;
  end
  else if _val.StartsWith('hsla') then begin //      hsla(30, 19%, 90%, 0.4)
    str := Copy( _val, StringFirst+5, length(_val)-6 ) ;
    {$IFDEF JAVA OR ISLAND}
      colors := str.Split( ',' ).ToArray ;
    {$ELSE}
      colors := str.Split( [','] ) ;
    {$ENDIF}
    h := DotStrToFloat( colors[0] ) / 360.0 ;
    {$IFDEF JAVA}
      s := DotStrToFloat( colors[1].Substring( 0, length(colors[1])-1 ) ) / 100.0 ;
      l := DotStrToFloat( colors[2].Substring( 0, length(colors[2])-1 ) ) / 100.0 ;
    {$ELSE}
      s := DotStrToFloat( colors[1].Remove( length(colors[1])-1 ) ) / 100.0 ;
      l := DotStrToFloat( colors[2].Remove( length(colors[2])-1 ) ) / 100.0 ;
    {$ENDIF}
    a := DotStrToFloat( colors[3] ) ;
    c := TGIS_Color.FromAHSL( a, h, s, l ) ;
  end
  else if _val.StartsWith('hsl') then begin //      hsl(30, 19%, 90%)
    str := Copy( _val, StringFirst+4, length(_val)-5 ) ;
    {$IFDEF JAVA OR ISLAND}
      colors := str.Split( ',' ).ToArray ;
    {$ELSE}
      colors := str.Split( [','] ) ;
    {$ENDIF}
    h := DotStrToFloat( colors[0] ) / 360.0;
    {$IFDEF JAVA}
      s := DotStrToFloat( colors[1].Substring( 0, length(colors[1])-1 ) ) / 100.0 ;
      l := DotStrToFloat( colors[2].Substring( 0, length(colors[2])-1 ) ) / 100.0 ;
    {$ELSE}
      s := DotStrToFloat( colors[1].Remove( length(colors[1])-1 ) ) / 100.0 ;
      l := DotStrToFloat( colors[2].Remove( length(colors[2])-1 ) ) / 100.0 ;
    {$ENDIF}
    c := TGIS_Color.FromHSL( h, s, l ) ;
  end
  else if _val.StartsWith('rgba') then begin //      rgba(10, 20, 30, 0.5)
    str := Copy( _val, StringFirst+5, length(_val)-6 ) ;
    {$IFDEF JAVA OR ISLAND}
      colors := str.Split( ',' ).ToArray ;
    {$ELSE}
      colors := str.Split( [','] ) ;
    {$ENDIF}
    r := StrToInt( colors[0] ) ;
    g := StrToInt( colors[1] ) ;
    b := StrToInt( colors[2] ) ;
    aa := RoundS( DotStrToFloat( colors[3] ) * 255 ) ;
    c := TGIS_Color.FromARGB( aa, r, g, b ) ;
  end
  else if _val.StartsWith('rgb') then begin //       rgb(10, 20, 30)
    str := Copy( _val, StringFirst+4, length(_val)-5 ) ;
    {$IFDEF JAVA OR ISLAND}
      colors := str.Split( ',' ).ToArray ;
    {$ELSE}
      colors := str.Split( [','] ) ;
    {$ENDIF}
    r := StrToInt( colors[0] ) ;
    g := StrToInt( colors[1] ) ;
    b := StrToInt( colors[2] ) ;
    c := TGIS_Color.FromRGB( r, g, b ) ;
  end ;
  Result := c ;
end ;

class function T_ConverterHelper.parseWidth(
  const _val : String
) : Single ;
begin
  Result := 0 ;
  if IsStringEmpty( _val ) then exit ;

  try
    Result := DotStrToFloat( _val ) ;
  except
    Result := 0 ;
  end;
end ;

class function T_ConverterHelper.ParseSize(
  const _val : String
) : Single ;
begin
  Result := 0 ;
  if IsStringEmpty( _val ) then exit ;

  try
    Result := DotStrToFloat( _val ) ;
  except
    Result := 0 ;
  end;
end ;

class function T_ConverterHelper.InterpolateValue (
  const _startValue : Double ;
  const _endValue   : Double ;
  const _minLevel   : Double ;
  const _maxLevel   : Double ;
  const _level      : Double
) : Double ;
var
  val        : Double ;
  max_min    : Double ;
  p1, p2     : Double ;
begin
  if      _level < _minLevel then val := _minLevel
  else if _level > _maxLevel then val := _maxLevel
  else                            val := _level ;

  max_min:= _maxLevel - _minLevel ;
  if max_min > GIS_DOUBLE_RESOLUTION then begin
    // linear
    p1 := ( val - _minLevel ) / max_min;
    p2 := 1 - p1 ;
    Result := p1 * _endValue + p2 * _startValue ;
  end
  else
    Result := _endValue ;
end ;

class function T_ConverterHelper.InterpolateValueExp(
  const _startValue : Double ;
  const _endValue   : Double ;
  const _minLevel   : Double ;
  const _maxLevel   : Double ;
  const _level      : Double ;
  const _base       : Double
) : Double ;
var
  val        : Double ;
  max_min    : Double ;
  p, ratio   : Double ;
begin
  if      _level < _minLevel then val := _minLevel
  else if _level > _maxLevel then val := _maxLevel
  else                            val := _level ;

  max_min:= _maxLevel - _minLevel ;
  if max_min > GIS_DOUBLE_RESOLUTION then begin
    // expotential
    p := ( val - _minLevel );
    if _base = 1 then
      Result := p / max_min
    else begin
      ratio  := (Power(_base, p) - 1) / (Power(_base, max_min) - 1) ;
      Result := (_startValue * (1 - ratio)) + (_endValue * ratio) ;
    end ;
  end
  else
    Result := _endValue ;
end ;

//=============================================================================
// T_ExpressionParser
//=============================================================================

class function T_ExpressionParser.ParseExpression(
  const _expr : TGIS_JSONArray
) : String ;
var
  op    : String ;
  i,j,k : Integer ;
  lst   : TArray<String> ;
  lst2  : TArray<String> ;
  key   : String ;
  v1,v2 : String ;
  neg   : Boolean ;
  arr   : TGIS_JSONArray ;
begin
  Result := '' ;
  if not assigned( _expr ) then exit ;
  if _expr.Count = 0 then exit ;

  op := _expr[0].Value ;
  if (op = 'all') or (op = 'any') or (op = 'none') then begin
    SetLength( lst, _expr.Count-1 ) ;
    for i := 1 to _expr.Count - 1 do
      lst[i-1] := parseValue( _expr[i] ) ;

    if length( lst ) > 0 then begin
      if (op = 'all') then
        Result := Format( '(%s)', [Result.Join( ') AND (', lst )] )
      else if (op = 'any') then
        Result := Format( '(%s)', [Result.Join( ') OR (', lst )] )
      else if (op = 'none') then
        Result := Format( 'NOT (%s)', [Result.Join( ') AND NOT (', lst )] )
    end ;
  end
  else if (op = '==') or (op = '!=') or
          (op = '>=') or (op = '>' ) or
          (op = '<=') or (op = '<' ) then begin
    if (op = '==') then
      op := '='
    else if (op = '!=') then
      op := '<>' ;
    Result := Format( '[%s] %s %s', [parseKey(_expr[1]), op,
                      QuotedStr(parseKey(_expr[2]))]
                    ) ;
  end
  else if (op = '!') then begin
    Result := 'NOT ' + parseKey( _expr[1] ) ;
  end
  else if ( op = 'has' ) then begin
    key := parseKey( _expr[1] ) ;
    Result := Format( '[%s] IS NOT NULL', [key] ) ;
  end
  else if ( op = '!has' ) then begin
    key := parseKey( _expr[1] ) ;
    Result := Format( '[%s] IS NULL', [key] ) ;
  end
  else if ( op = 'in' ) or (op = '!in' ) then begin
    key := parseKey( _expr[1] ) ;
    SetLength( lst, _expr.Count-2 ) ;
    for i := 2 to _expr.Count-1 do
      lst[i-2] := QuotedStr( parseValue( _expr[i] ) ) ;
    if ( op = 'in' ) then
      Result := Format( '[%s] IN (%s)', [ key, Result.Join( ', ', lst )] )
    else
      Result := Format( '([%s] IS NULL OR [%s] NOT IN (%s))',
                        [ key, key, Result.Join( ', ', lst )]
                      );
  end
  else if ( op = 'get' ) then
    Result := parseKey( _expr[1] )
  else if ( op = 'match' ) then begin
    if (_expr.Count = 5) then begin
      v1 := parseValue(_expr[3]) ;
      v2 := parseValue(_expr[4]) ;
      if ((v1 = 'True') or (v1='False')) and ((v2 = 'True') or (v2='False')) then begin
        key := parseKey(_expr[1]) ;
        if _expr[2].IsArray then begin
          arr := _expr[2].AsArray ;
          SetLength( lst, arr.Count ) ;
          for i := 0 to arr.Count-1 do
            lst[i] := QuotedStr( parseValue( arr[i] ) ) ;
        end
        else begin
          SetLength( lst, 1 ) ;
          lst[0] := QuotedStr( parseValue( _expr[2] ) ) ;
        end ;
        neg := (v1 = 'False') and (v2 = 'True') ;
        if neg then begin
          if length( lst ) = 1 then
            Result := Format( '[%s] <> %s', [key, lst[0]] )
          else
            Result := Format( '[%s] NOT IN (%s)', [ key, Result.Join( ', ', lst )] )
        end
        else begin
          if length( lst ) = 1 then
            Result := Format( '[%s] = %s', [key, lst[0]] )
          else
            Result := Format( '[%s] IN (%s)', [ key, Result.Join( ', ', lst )] )
        end ;
      end
      else begin
        key := parseKey(_expr[1]) ;
        if _expr[2].IsArray then begin
          arr := _expr[2].AsArray ;
          SetLength( lst, arr.Count ) ;
          for i := 0 to arr.Count-1 do
            lst[i] := QuotedStr( parseValue( arr[i] ) ) ;
        end
        else begin
          SetLength( lst, 1 ) ;
          lst[0] := QuotedStr( parseValue( _expr[2] ) ) ;
        end ;
        neg := (v1 = 'False') and (v2 = 'True') ;
        if neg then begin
          if length( lst ) = 1 then
            Result := Format( '[%s] <> %s', [key, lst[0]] )
          else
            Result := Format( '[%s] NOT IN (%s)', [ key, Result.Join( ', ', lst )] )
        end
        else begin
          if length( lst ) = 1 then
            Result := Format( '[%s] = %s', [key, lst[0]] )
          else
            Result := Format( '[%s] IN (%s)', [ key, Result.Join( ', ', lst )] )
        end ;
        if ((v1 <> 'True') and (v1 <> 'False')) then
          Result := Result + ' AND ' + v1 ;
        if ((v2 <> 'True') and (v2 <> 'False')) then
          Result := Result + ' AND ' + v2 ;

      end;
    end
    else begin
      key := parseKey( _expr[1] ) ;
      i := 2 ;
      k := 0 ;
      SetLength( lst2, (_expr.Count-2) div 2 ) ;
      while i < _expr.Count-2 do begin
        if (_expr[i].IsArray) then begin
          arr := _expr[i].AsArray ;
          SetLength( lst, arr.Count ) ;
          for j := 0 to arr.Count-1 do
            lst[j] := QuotedStr( parseValue( arr[j] ) ) ;
          if length( lst ) = 1 then
            lst2[k] := Format( '([%s] = %s)', [key, lst[0]] )
          else
            lst2[k] := Format( '([%s] IN (%s))', [ key, Result.Join( ', ', lst )] )
        end
        else begin
          v1 := QuotedStr( parseValue( _expr[i] ) ) ;
          if ((v1 <> 'True') and (v1 <> 'False')) then
            lst2[k] := Format( '([%s] = %s)', [key, v1] )
        end ;
        inc( k ) ;
        inc( i, 2 ) ;
      end ;
      Result := Result.Join( ' OR ', lst2 ) ;
    end
  end
  else if ( op = 'coalesce' ) then
    Result := ParseExpression( _expr[1].AsArray )
  else if ( op = 'to-string' ) then
    Result := ParseExpression( _expr[1].AsArray )
  else if ( op = 'abs' ) then begin
    key := parseKey( _expr[0] ) ;
    Result := Format( '%s(%s)', [UpperCase(key), parseValue( _expr[1] )] ) ;
  end
  else if ( op = 'number' ) then
    Result := ParseExpression( _expr[1].AsArray )
  else if ( op = 'step' ) then begin
    i := 2 ;
    while (i < _expr.Count) and not (_expr[i].IsArray) do
      inc( i ) ;
    if (i < _expr.Count) then
      Result := ParseExpression( _expr[i].AsArray ) ;
  end
  else if ( op = 'case' ) then begin
    Result := ParseExpression( _expr[1].AsArray ) ;
  end
  else if ( op = 'literal' ) then
    Result := parseValue( _expr[1] )
  else if ( op = '-' ) then begin
    if _expr.Count = 3 then begin
      v1 := parseValue( _expr[1] ) ;
      v2 := parseValue( _expr[2] ) ;
      if _expr[2].IsArray then
        Result := v2
      else
        Result := DotFloatToStr( DotStrToFloat(v1)-DotStrToFloat(v2)) ;
    end;
  end
  else
    TGIS_Logger.AsWarning( 'ParseExpression untested case',
                           Format('%s in %s', [op, _expr.ToString])
                          ) ;
end ;

class function T_ExpressionParser.parseKey(
  const _val : TGIS_JSONObject
) : String ;
begin
  if _val.IsArray then begin
    if _val.AsArray.Count > 1 then begin
      if _val.AsArray[1].IsArray then
        Result := ParseExpression( _val.AsArray[1].AsArray )
      else
        Result := _val.AsArray[1].Value
    end
    else if _val.AsArray.Count > 0 then
      Result :=  _val.AsArray[0].Value
    else
      Result := ''
  end
  else
    Result := _val.Value ;

  if (Result = '$type') or (Result = 'geometry-type') then
    Result := 'GIS_SHAPE_TYPE' ;
end ;

class function T_ExpressionParser.parseValue(
  const _val : TGIS_JSONObject
) : String ;
begin
  if _val.IsArray then
    Result := ParseExpression( _val.AsArray )
  else
    Result := _val.Value
end ;

//=============================================================================
// T_StyleConverter
//=============================================================================

procedure T_StyleConverter.Clear;
var
  itm : T_StyleLayer ;
  ps  : TPair<String, TStream> ;
begin
  for itm in FLayers do
    FreeObjectNotNil( itm ) ;
  FLayers.Clear ;

  if assigned( FSpritesDict ) then begin
    for ps in FSpritesDict do
      FreeObjectNotNil( ps.Value ) ;
    FSpritesDict.Clear ;
  end ;
end ;

constructor T_StyleConverter.Create ;
begin
  inherited ;

  FLayers  := TList<T_StyleLayer>.Create ;

  FColorDict  := TDictionary< String, TGIS_Color >.Create ;
  FColorDict.Add('green'  , TGIS_Color.FromRGB( 86, 186, 86 )) ;
  FColorDict.Add('white'  , TGIS_Color.FromRGB( $ff, $ff, $ff )) ;
  FColorDict.Add('blue'   , TGIS_Color.FromRGB( 66, 167, 243 )) ;
  FColorDict.Add('red'    , TGIS_Color.FromRGB( 255, 78, 75 )) ;
  FColorDict.Add('black'  , TGIS_Color.FromRGB( 0, 0, 0 )) ;
  FColorDict.Add('yellow' , TGIS_Color.FromRGB( 255, 237, 103 )) ;
  FColorDict.Add('brown'  , TGIS_Color.FromRGB( 97, 97, 0) ) ;
  FColorDict.Add('orange' , TGIS_Color.FromRGB( 206, 197, 0) ) ;

  FShieldDict := TDictionary< String, String >.Create ;
  FShieldDict.Add( 'Road/Hexagon', 'LIBSVG:shield:GeneralHighway%s02' ) ;
  FShieldDict.Add( 'Road/Octagon', 'LIBSVG:shield:GeneralRoad%s01' ) ;
  FShieldDict.Add( 'Road/Pentagon', 'LIBSVG:shield:GeneralHighway%s03' ) ;
  FShieldDict.Add( 'Road/Rectangle', 'LIBSVG:shield:GeneralHighway%s01' ) ;
  FShieldDict.Add( 'Road/Rectangle hexagon', 'LIBSVG:shield:GeneralHighway%s04' ) ;
  FShieldDict.Add( 'Road/Shield', 'LIBSVG:shield:USInterstate' ) ;
  FShieldDict.Add( 'Road/U-shaped', 'LIBSVG:shield:GeneralRoad%s03' ) ;
  FShieldDict.Add( 'Road/V-shaped', 'LIBSVG:shield:GeneralRoad%s03' ) ;
  FShieldDict.Add( 'Road/Secondary', 'LIBSVG:shield:GeneralHighway%s04' ) ;

  FPixelSizeFactor := 1 ;
  FSpritesUrl      := '';
end ;

destructor T_StyleConverter.Destroy ;
begin
  Clear ;

  FreeObject( FLayers      ) ;
  FreeObject( FColorDict   ) ;
  FreeObject( FSpritesDict ) ;
  FreeObject( FShieldDict  ) ;

  inherited ;
end ;

function T_StyleConverter.fget_BackgroundColor : TGIS_Color ;
var
  la : T_StyleLayer ;
begin
  Result := TGIS_Color.None ;
  for la IN FLayers do begin
    if la.LayerType = T_StyleLayerType.Background then begin
      Result := la.GLProperty.Background ;
      break ;
    end ;
  end ;
end ;

function T_StyleConverter.loadFromFile(
  const _path : String
) : String ;
var
  lst : TStringList ;
begin
  lst := TStringList.Create ;
  try
    lst.LoadFromFile( _path ) ;
    Result := lst.Text ;
  finally
    FreeObject( lst ) ;
  end ;
end ;

function T_StyleConverter.loadFromStream(
  const _stm : TStream
) : String ;
var
  buf : TBytes ;
begin
  _stm.Position := 0 ;
  SetLength( buf, _stm.Size ) ;
  {$IFDEF OXYGENE}
    _stm.Read( buf, _stm.Size ) ;
  {$ELSE}
    _stm.ReadBuffer( buf[0], _stm.Size ) ;
  {$ENDIF}
  Result := TEncoding.UTF8.GetString( buf ) ;
end ;

function T_StyleConverter.loadImage(
  const _path : String
) : TGIS_Bitmap ;
var
  r : TGIS_HttpResponse ;
begin
  Result := nil ;
  if (Pos( 'http:' , _path ) = StringFirst) or
     (Pos( 'https:', _path ) = StringFirst) then begin
    r.Stream := nil ;
    try
      r := TGIS_WebUtils.HttpFetch( _path, nil, nil, true, 40000,
                                    GetDefaultUserAgent( 'ttkWP' ), '', '', ''
                                  ) ;
      if r.Status = GIS_HTTP_OK then begin
        Result := TGIS_Bitmap.Create ;
        Result.LoadFromStream( r.Stream ) ;
      end ;
    finally
      FreeObject( r.Stream ) ;
    end ;
  end
  else begin
    Result := TGIS_Bitmap.Create ;
    Result.LoadFromFile( _path ) ;
  end;
end ;

function T_StyleConverter.loadFromService(
  const _path : String
) : String ;
var
  r : TGIS_HttpResponse ;
begin
  Result := '' ;
  r.Stream := nil ;
  try
    r := TGIS_WebUtils.HttpFetch( _path, nil, nil, true, 40000,
                                  GetDefaultUserAgent( 'ttkWP' ), '', '', ''
                                ) ;
    if r.Status = GIS_HTTP_OK then
      Result := loadFromStream( r.Stream ) ;
  finally
    FreeObject( r.Stream ) ;
  end ;
end ;

function T_StyleConverter.parseFilter(
  const _obj  : TGIS_JSONObject
) : String ;
var
  expr : TGIS_JSONArray ;
begin
  Result := '';
  if _obj.TryGetValue<TGIS_JSONArray>('filter', expr) then
    if assigned( expr ) then
      Result := T_ExpressionParser.ParseExpression( expr ) ;
end ;

procedure T_StyleConverter.parseLayout(
  const _obj    : TGIS_JSONObject ;
  const _layer  : T_StyleLayer
) ;
var
  val    : String ;
  layout : TGIS_JSONObject ;
begin
  layout := _obj.FindObject('layout') ;
  if assigned( layout ) then begin
    if layout.TryGetValue<String>('visibility', val) then
      _layer.Visible := val <> 'none'
    else
      _layer.Visible := True ;

    if _layer.LayerType = T_StyleLayerType.Symbol then
      parseSymbolLayout( layout, _layer )
  end ;
end ;

function T_StyleConverter.parseInterpolateByZoom(
  const _obj        : TGIS_JSONObject ;
  const _layer      : T_StyleLayer ;
    var _defNumber  : Single
) : T_NativeProperty ;
var
  base  : Double ;
  stops : TGIS_JSONArray ;
  i     : Integer ;
  bz    : Integer ;
  bc    : Single ;
  ds    : Single ;
  dc    : TGIS_Color ;
  lst   : T_NativeProperty ;
  arr   : TGIS_JSONArray ;
begin
  base := 1 ;
  _obj.TryGetValue<Double>('base', base) ;
  if not _obj.TryGetValue<TGIS_JSONArray>('stops', stops) then begin
    Result := T_NativeProperty.Create( T_NativePropertyType.Numeric ) ;
    exit ;
  end ;

  if ( stops.Count > 0 ) and stops[0].IsArray then begin
    Result := T_NativeProperty.Create( T_NativePropertyType.Numeric, base ) ;
    // when level>=bz and level<tz
    for i := 0 to stops.Count-1 do begin
      arr := stops[i].AsArray ;
      bz := RoundS( DotStrToFloat( arr[0].Value ) ) ;
      if (arr[1].IsArray) and
         (arr[1].AsArray[0].Value = 'match') then begin
        lst := parseMatchList( arr[1].AsArray, T_NativePropertyType.Numeric, dc, _defNumber, base ) ;
        Result.Add( bz, lst ) ;
        FreeObject( lst ) ;
      end
      else if (arr[1].IsArray) and
         (arr[1].AsArray[0].Value = 'case') then begin
        bc := DotStrToFloat( arr[1].AsArray[2].Value ) ;
        Result.Add( bz, bc ) ;
      end
      else if (arr[1].IsArray) and
         (arr[1].AsArray[0].Value = 'step') then begin
        lst := parseMatchList( arr[1].AsArray, T_NativePropertyType.Numeric, dc, _defNumber, base ) ;
        Result.Add( bz, lst ) ;
        FreeObject( lst ) ;
      end
      else begin
        if arr[1].IsArray then
          bc := DotStrToFloat( arr[1].AsArray[1].Value )
        else
          bc := DotStrToFloat( arr[1].Value ) ;
        Result.Add( bz, bc ) ;
      end ;
    end ;
  end
  else if ( stops.Count > 0 ) and (stops[0].Value = 'match') then
    Result := parseMatchList( stops, T_NativePropertyType.Numeric, dc, ds, base )
  else
    Result := T_NativeProperty.Create( T_NativePropertyType.Numeric ) ;
end ;

function T_StyleConverter.parseInterpolateColorByZoom(
  const _obj       : TGIS_JSONObject ;
  const _layer     : T_StyleLayer ;
    var _fillColor : TGIS_Color
) : T_NativeProperty ;
var
  base  : Double ;
  stops : TGIS_JSONArray ;
  i     : Integer ;
  bz    : Integer ;
  bc    : TGIS_Color ;
  ds    : Single ;
  lst   : T_NativeProperty ;
  arr   : TGIS_JSONArray ;
begin
  Result := T_NativeProperty.Create( T_NativePropertyType.Color ) ;
  base := 1 ;
  _obj.TryGetValue<Double>('base', base) ;
  if not _obj.TryGetValue<TGIS_JSONArray>('stops', stops) then exit ;

  if ( stops.Count > 0 ) and stops[0].IsArray then begin
    for i := 0 to stops.Count-1 do begin
      arr := stops[i].AsArray ;
      bz := RoundS( DotStrToFloat( arr[0].Value ) ) ;
      if (arr[1].IsArray) and
         (arr[1].AsArray[0].Value = 'match') then begin
        lst := parseMatchList( arr[1].AsArray, T_NativePropertyType.Color, _fillColor, ds, base ) ;
        Result.Add( bz, lst ) ;
        FreeObject( lst ) ;
      end
      else begin
        bc := T_ConverterHelper.ParseColor( arr[1].Value )  ;
        Result.Add( bz, bc ) ;
      end ;

    end ;

  end
end ;

function T_StyleConverter.parseInterpolateListByZoom(
  const _obj        : TGIS_JSONArray ;
  const _type       : T_NativePropertyType ;
  const _layer      : T_StyleLayer ;
    var _defColor   : TGIS_Color ;
    var _defNumber  : Single
) : T_NativeProperty ;
var
  base      : Double ;
  i         : Integer ;
  technique : String ;
  dobj      : TGIS_JSONObject ;
  aobj      : TGIS_JSONObject ;
  arr       : TGIS_JSONArray ;
  iobj      : TGIS_JSONObject ;
begin
  if _obj[0].Value <> 'interpolate' then begin
    Result := T_NativeProperty.Create( _type ) ;
    exit ;
  end ;
  base := 1 ;
  arr := _obj[1].AsArray ;
  technique := arr[0].Value ;
  if technique = 'linear' then
    base := 1
  else if technique = 'exponential' then
    base := DotStrToFloat( arr[1].Value ) ;

  arr := _obj[2].AsArray ;
  if arr[0].Value <> 'zoom' then begin
    Result := T_NativeProperty.Create( _type ) ;
    exit ;
  end ;

  i := 3 ;
  dobj := TGIS_JSONObject.Create( TGIS_JSONType.&Object ) ;
  try
    dobj.AddPair( 'base', TGIS_JSONObject.Create( base ) ) ;

    aobj := TGIS_JSONObject.Create( TGIS_JSONType.&Array ) ;
    arr := aobj.AsArray ;
    while i < _obj.Count do begin
      iobj := TGIS_JSONObject.Create( TGIS_JSONType.&Array ) ;
      iobj.AsArray.Add( _obj[i].Clone ) ;
      iobj.AsArray.Add( _obj[i+1].Clone ) ;
      arr.Add( iobj ) ;
      inc( i, 2 ) ;
    end ;

    dobj.AddPair( 'stops', aobj ) ;

    case _type of
      T_NativePropertyType.Color   :
        Result := parseInterpolateColorByZoom( dobj, _layer, _defColor )  ;
      T_NativePropertyType.Numeric :
        Result := parseInterpolateByZoom( dobj, _layer, _defNumber )  ;
      T_NativePropertyType.Opacity :
        Result := parseInterpolateOpacityByZoom( dobj, _layer )  ;
      T_NativePropertyType.Text    :
        Result := parseInterpolatePointByZoom( dobj, _layer )  ;
    else
      Result := T_NativeProperty.Create( _type ) ;
    end ;
  finally
    FreeObject( dobj ) ;
  end ;
end ;

function T_StyleConverter.parseInterpolateOpacityByZoom(
  const _obj: TGIS_JSONObject;
  const _layer: T_StyleLayer
) : T_NativeProperty ;
var
  base  : Double ;
  stops : TGIS_JSONArray ;
  i     : Integer ;
  bz    : Integer ;
  bc    : Single ;
  ds    : Single ;
  dc    : TGIS_Color ;
  lst   : T_NativeProperty ;
  arr   : TGIS_JSONArray ;
begin
  Result := T_NativeProperty.Create( T_NativePropertyType.Opacity ) ;
  base := 1 ;
  _obj.TryGetValue<Double>('base', base) ;
  if not _obj.TryGetValue<TGIS_JSONArray>('stops', stops) then exit ;

  if ( stops.Count > 0 ) and stops[0].IsArray then begin
    // when level>=bz and level<tz
    for i := 0 to stops.Count-1 do begin
      arr := stops[i].AsArray ;
      bz := RoundS( DotStrToFloat( arr[0].Value ) ) ;
      if (arr[1].IsArray) and
         (arr[1].AsArray[0].Value = 'match') then begin
        lst := parseMatchList( arr[1].AsArray, T_NativePropertyType.Opacity, dc, ds, base ) ;
        Result.Add( bz, lst ) ;
        FreeObject( lst ) ;
      end
      else if (arr[1].IsArray) and
         (arr[1].AsArray[0].Value = 'case') then begin
        bc := DotStrToFloat( arr[1].AsArray[2].Value ) ;
        Result.Add( bz, bc ) ;
      end
      else begin
        bc := DotStrToFloat( arr[1].Value ) ;
        Result.Add( bz, bc ) ;
      end ;
    end ;
  end
  else if ( stops.Count > 0 ) and (stops[0].Value = 'match') then
    Result := parseMatchList( stops, T_NativePropertyType.Opacity, dc, ds, base ) ;
end ;

function T_StyleConverter.parseInterpolatePointByZoom(
  const _obj   : TGIS_JSONObject ;
  const _layer : T_StyleLayer
) : T_NativeProperty ;
begin
  Result := T_NativeProperty.Create( T_NativePropertyType.Text ) ;
end ;

function T_StyleConverter.parseMatchList(
  const _obj        : TGIS_JSONArray ;
  const _type       : T_NativePropertyType  ;
    var _defColor   : TGIS_Color ;
    var _defNumber  : Single ;
  const _base       : Double
) : T_NativeProperty ;
var
  attr : String ;
  i,j  : Integer ;
  keys : TArray<String> ;
  val  : String ;
  str  : String ;
  def  : String ;
  arr  : TGIS_JSONArray ;
begin
  Result := T_NativeProperty.Create( _type, _base ) ;

  attr := T_ExpressionParser.ParseExpression( _obj[1].AsArray );
  i := 2 ;
  while i < _obj.Count - 1 do begin
    if _obj[i].IsArray then begin
      arr := _obj[i].AsArray ;
      SetLength( keys, arr.Count ) ;
      for j := 0 to  arr.Count-1 do
        keys[j] := arr[j].Value ;

      if _obj[i+1].IsArray then

      else begin
        val := _obj[i+1].Value ;
        for j := 0 to  arr.Count-1 do begin
          str := keys[ j ] ;
          case _type of
            T_NativePropertyType.Color   : Result.Add( attr, str, T_ConverterHelper.ParseColor( val ) );
            T_NativePropertyType.Numeric : Result.Add( attr, str,  DotStrToFloat( val ) );
            T_NativePropertyType.Opacity : Result.Add( attr, str,  DotStrToFloat( val ) );
          end ;
        end;
      end ;
      i := i + 2 ;
    end
    else begin
      str := _obj[i].Value ;
      val := _obj[i+1].Value ;
      case _type of
        T_NativePropertyType.Color   : Result.Add( attr, str, T_ConverterHelper.ParseColor( val ) );
        T_NativePropertyType.Numeric : Result.Add( attr, str,  DotStrToFloat( val ) );
        T_NativePropertyType.Opacity : Result.Add( attr, str,  DotStrToFloat( val ) );
      end ;
      i := i + 2 ;
    end ;
  end ;

  //def can be array
  if _obj[_obj.Count-1].IsArray then
    // TODO
  else begin
    def := _obj[_obj.Count-1].Value ;
    case _type of
      T_NativePropertyType.Color   : _defColor := T_ConverterHelper.ParseColor( def );
      T_NativePropertyType.Numeric : _defNumber := DotStrToFloat( def );
      T_NativePropertyType.Opacity : _defNumber := DotStrToFloat( def );
    end ;
    Result.Add( attr, '', def) ;
  end ;
end ;

function T_StyleConverter.parseValueList(
  const _obj    : TGIS_JSONArray ;
  const _type   : T_NativePropertyType ;
  const _layer  : T_StyleLayer ;
    var _color  : TGIS_Color ;
    var _number : Single
) : T_NativeProperty ;
var
  smethod : String ;
begin
  smethod := _obj[0].Value ;
  if smethod = 'interpolate' then
    Result := parseInterpolateListByZoom( _obj, _type, _layer, _color, _number )
  else if smethod = 'match' then
    Result := parseMatchList( _obj, _type, _color, _number, 1 )
  else
    Result := T_NativeProperty.Create( _type ) ;
end ;

procedure T_StyleConverter.parseFill(
  const _obj      : TGIS_JSONObject ;
  const _layer    : T_StyleLayer
) ;
var
  val : TGIS_JSONObject ;
  dc  : TGIS_Color ;
  op  : Single ;
  ds  : Single ;
begin
  val := _obj.FindObject('fill-color') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.FillColor,
        parseInterpolateColorByZoom( val, _layer, dc )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.FillColor,
        parseValueList( val.AsArray, T_NativePropertyType.Color, _layer, dc, ds )
      ) ;
    end
    else begin
      dc := T_ConverterHelper.ParseColor( val.Value ) ;
      _layer.GLProperty.FillColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, dc ) ) ;
      _layer.GLProperty.FillColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, dc ) ) ;
    end ;
  end ;

  val := _obj.FindObject('fill-outline-color') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.FillOutlineColor,
        parseInterpolateColorByZoom( val, _layer, dc )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.FillOutlineColor,
        parseValueList( val.AsArray, T_NativePropertyType.Color, _layer, dc, ds )
      ) ;
    end
    else begin
      dc := T_ConverterHelper.ParseColor( val.Value ) ;
      _layer.GLProperty.FillOutlineColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, dc ) ) ;
      _layer.GLProperty.FillOutlineColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, dc ) ) ;
    end ;
  end ;

  // TODO make LIST
  val := _obj.FindObject('fill-pattern') ;
  if assigned( val ) then
    _layer.GLProperty.FillPattern := val.Value ;

  val := _obj.FindObject('fill-opacity') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.FillOpacity,
        parseInterpolateOpacityByZoom( val, _layer )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.FillOpacity,
        parseValueList( val.AsArray, T_NativePropertyType.Opacity, _layer, dc, ds )
       ) ;
    end
    else begin
      op := DotStrToFloat( val.Value ) ;
      _layer.GLProperty.FillOpacity.Add( T_ParamsStops<Single>.Create( -1, op ) ) ;
      _layer.GLProperty.FillOpacity.Add( T_ParamsStops<Single>.Create( -1, op ) ) ;
    end ;
  end ;

  val := _obj.FindObject('fill-translate') ;
  if assigned( val ) then begin
    // TODO
  end ;

end ;

procedure T_StyleConverter.parseLine(
  const _obj      : TGIS_JSONObject ;
  const _layer    : T_StyleLayer
) ;
var
  val   : TGIS_JSONObject ;
  c     : TGIS_Color ;
  v     : Double ;
  s     : Single ;
  op    : Single ;
  arr   : TGIS_JSONArray ;
  i     : Integer ;
begin
  val := _obj.FindObject('line-color') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.LineColor,
        parseInterpolateColorByZoom( val, _layer, c )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.LineColor,
        parseValueList( val.AsArray, T_NativePropertyType.Color, _layer , c, s)
      );
    end
    else begin
      c := T_ConverterHelper.ParseColor( val.Value ) ;
      _layer.GLProperty.LineColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, c ) ) ;
      _layer.GLProperty.LineColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, c ) ) ;
    end ;
  end ;

  val := _obj.FindObject('line-width') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.LineWidth,
        parseInterpolateByZoom( val, _layer, s )
      );
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.LineWidth,
        parseValueList( val.AsArray, T_NativePropertyType.Numeric, _layer, c, s )
      );
    end
    else begin
      v := T_ConverterHelper.ParseWidth( val.Value ) ;
      _layer.GLProperty.LineWidth.Add( T_ParamsStops<Single>.Create( -1, v ) ) ;
      _layer.GLProperty.LineWidth.Add( T_ParamsStops<Single>.Create( -1, v ) ) ;
    end ;
  end ;

  val := _obj.FindObject('line-opacity') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.LineOpacity,
        parseInterpolateOpacityByZoom( val, _layer )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.LineOpacity,
        parseValueList( val.AsArray, T_NativePropertyType.Opacity, _layer, c, s )
       ) ;
    end
    else begin
      op := DotStrToFloat( val.Value ) ;
      _layer.GLProperty.LineOpacity.Add( T_ParamsStops<Single>.Create( -1, op ) ) ;
      _layer.GLProperty.LineOpacity.Add( T_ParamsStops<Single>.Create( -1, op ) ) ;
    end ;
  end ;

  val := _obj.FindObject('line-dasharray') ;
  if assigned( val ) then begin
    _layer.GLProperty.LineDash := True ;
    if val.IsArray then begin
      arr := val.AsArray ;
      SetLength( _layer.GLProperty.LineDashArray, arr.Count ) ;
      for i := 0 to arr.Count-1 do
        _layer.GLProperty.LineDashArray[i] := arr[i].AsDouble ;
    end ;
    // TODO val.IsObject
  end ;

  val := _obj.FindObject('line-offset') ;
  if assigned( val ) then begin
    if not (val.IsArray or val.IsObject) then
      _layer.GLProperty.LineOffset := DotStrToFloat( val.Value ) ;
    // TODO
  end ;

end ;

procedure T_StyleConverter.parseSymbolPaint(
  const _obj      : TGIS_JSONObject ;
  const _layer    : T_StyleLayer
) ;
var
  val   : TGIS_JSONObject ;
  c     : TGIS_Color ;
  s     : Single ;
begin
  val := _obj.FindObject('text-color') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.TextColor,
        parseInterpolateColorByZoom( val, _layer, c )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.TextColor,
        parseValueList( val.AsArray, T_NativePropertyType.Color, _layer , c, s)
      );
    end
    else begin
      c := T_ConverterHelper.ParseColor( val.Value ) ;
      _layer.GLProperty.TextColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, c ) ) ;
      _layer.GLProperty.TextColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, c ) ) ;
    end ;
  end ;

  val := _obj.FindObject('text-halo-color') ;
  if assigned( val ) then
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.TextHaloColor,
        parseInterpolateColorByZoom( val, _layer, c )
      ) ;
    end
    else if val.IsArray then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.TextHaloColor,
        parseValueList( val.AsArray, T_NativePropertyType.Color, _layer , c, s)
      );
    end
    else begin
      c := T_ConverterHelper.ParseColor( val.Value ) ;
      _layer.GLProperty.TextHaloColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, c ) ) ;
      _layer.GLProperty.TextHaloColor.Add( T_ParamsStops<TGIS_Color>.Create( -1, c ) ) ;
    end ;

  val := _obj.FindObject('icon-halo-color') ;
  if assigned( val ) then
    _layer.GLProperty.SymbolColor := T_ConverterHelper.ParseColor( val.Value ) ;

end ;

procedure T_StyleConverter.parseSymbolLayout(
  const _obj      : TGIS_JSONObject ;
  const _layer    : T_StyleLayer
) ;
var
  val   : TGIS_JSONObject ;
  s     : Single ;
  c     : TGIS_Color ;
  str   : String ;
  i     : Integer ;
  arr   : TGIS_JSONArray ;

  procedure addLabelPosition( const _pos : String ) ;
  begin
    if _pos = 'center' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.MiddleCenter
       )
    else if _pos = 'left' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.MiddleRight
       )
    else if _pos = 'right' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.MiddleLeft
       )
    else if _pos = 'top' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.DownCenter
       )
    else if _pos = 'bottom' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.UpCenter
       )
    else if _pos = 'top-left' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.UpRight
       )
    else if _pos = 'top-right' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.UpLeft
       )
    else if _pos = 'bottom-left' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.DownRight
       )
    else if _pos = 'bottom-right' then
      _layer.GLProperty.TextPositions := GisAddLabelPosition(
         _layer.GLProperty.TextPositions,
         TGIS_LabelPosition.DownLeft
       )
  end ;

  procedure addFont( const _val : String ) ;
  var
    arr : TArray<String> ;
    str : String ;
    ff  : String ;
  begin
    {$IFDEF JAVA OR ISLAND}
      arr := _val.Split( ' ' ).ToArray ;
    {$ELSE}
      arr := _val.Split( {$IFDEF OXYGENE}' '{$ELSE}[' ']{$ENDIF} ) ;
    {$ENDIF}

    ff  := '' ;
    for str in arr do begin
      if str = 'Bold' then
        _layer.GLProperty.TextFontStyle := GisAddFontStyle(
          _layer.GLProperty.TextFontStyle,
          TGIS_FontStyle.Bold
        )
      else if str = 'SemiBold' then
        _layer.GLProperty.TextFontStyle := GisAddFontStyle(
          _layer.GLProperty.TextFontStyle,
          TGIS_FontStyle.Bold
        )
      else if str = 'Italic' then
        _layer.GLProperty.TextFontStyle := GisAddFontStyle(
          _layer.GLProperty.TextFontStyle,
          TGIS_FontStyle.Italic
        )
      else
        ff := ff + str + ' ' ;
    end ;
    _layer.GLProperty.TextFontFamily := TrimRight( ff ) ;
  end ;

  function normalizeField( const _field : String ) : String ;
  var
    str : String ;
  begin
    str := _field.Replace( #$A, '<BR>' ) ;
    if str.Contains( ':' ) then begin
      str := str.Replace( '{', '{[' ) ;
      str := str.Replace( '}', ']}' ) ;
    end ;

    if not str.StartsWith('{') then
      Result := '{' + str + '}'
    else
      Result := str
  end ;

begin
  val := _obj.FindObject('text-size') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.TextSize,
        parseInterpolateByZoom( val, _layer, s )
      );
    end
    else if val.IsArray then begin
      s := 16 ;
      _layer.GLProperty.SetProperty(
        T_PropertyType.TextSize,
        parseValueList( val.AsArray, T_NativePropertyType.Numeric, _layer, c, s )
      );
    end
    else begin
      s := T_ConverterHelper.ParseSize( val.Value ) ;
      _layer.GLProperty.TextSize.Add( T_ParamsStops<Single>.Create( -1, s ) ) ;
      _layer.GLProperty.TextSize.Add( T_ParamsStops<Single>.Create( -1, s ) ) ;
    end ;
  end ;

  val := _obj.FindObject('text-max-width') ;
  if assigned( val ) then
    _layer.GLProperty.TextWrap := True ;

  val := _obj.FindObject('text-field') ;
  if assigned( val ) then begin
    if val.IsArray then begin
      arr := val.AsArray ;
      if (arr.Count > 2) and (arr[0].Value = 'format') then begin
        for i := 1 to arr.Count-1 do begin
          if arr[i].IsArray then
            str := T_ExpressionParser.ParseExpression( arr[i].AsArray )
          else
            str := arr[i].Value ;

          _layer.GLProperty.TextField := _layer.GLProperty.TextField + normalizeField(str) ;
        end ;
      end
      else
        str := T_ExpressionParser.ParseExpression( arr ) ;

      _layer.GLProperty.TextField := normalizeField(str) ;
    end
    else begin
      str := val.Value ;
      _layer.GLProperty.TextField := normalizeField(str) ;
    end ;
  end ;

  val := _obj.FindObject('text-transform') ;
  if assigned( val ) then begin
    if val.Value = 'uppercase' then
      _layer.GLProperty.TextField := Format( '<UCP>%s</UCP>', [_layer.GLProperty.TextField] )
    else if val.Value = 'lowercase' then
      _layer.GLProperty.TextField := Format( '<SCP>%s</SCP>', [_layer.GLProperty.TextField] ) ;
  end ;

  val := _obj.FindObject('text-rotate') ;
  if assigned( val ) then begin
    if val.IsArray then
      _layer.GLProperty.TextRotateField := T_ExpressionParser.ParseExpression( val.AsArray )
    else
      _layer.GLProperty.TextRotate := T_ConverterHelper.ParseSize( val.Value ) ;
  end ;

  val := _obj.FindObject('symbol-placement') ;
  if assigned( val ) then
    _layer.GLProperty.TextAlongLine := val.Value = 'line' ;

  val := _obj.FindObject('text-offset') ;
  if assigned( val ) then begin
    // todo
  end ;

  val := _obj.FindObject('text-justify') ;
  if assigned( val ) then begin
    if val.Value = 'left' then
      _layer.GLProperty.TextJustify := TGIS_LabelAlignment.LeftJustify
    else if val.Value = 'center' then
      _layer.GLProperty.TextJustify := TGIS_LabelAlignment.Center
    else if val.Value = 'right' then
      _layer.GLProperty.TextJustify := TGIS_LabelAlignment.RightJustify
    else if val.Value = 'auto' then
      _layer.GLProperty.TextJustify := TGIS_LabelAlignment.Follow  ;
  end ;

  val := _obj.FindObject('text-anchor') ;
  if assigned( val ) then begin
    if val.IsArray then begin
      arr := val.AsArray ;
      for i := 0 to arr.Count-1 do
        addLabelPosition( arr[i].Value )
    end
    else
      addLabelPosition( val.Value ) ;
  end ;

  val := _obj.FindObject('text-font') ;
  if assigned( val ) then begin
    if val.IsArray then begin
      arr := val.AsArray ;
      if arr.Count > 0 then
        addFont( arr[0].Value )
    end
    else
      addFont( val.Value ) ;
  end ;

  val := _obj.FindObject('icon-image') ;
  if assigned( val ) then
    _layer.GLProperty.SymbolImage := val.Value ; // TODO

  val := _obj.FindObject('icon-size') ;
  if assigned( val ) then begin
    if val.IsObject then begin
      _layer.GLProperty.SetProperty(
        T_PropertyType.SymbolSize,
        parseInterpolateByZoom( val, _layer, s )
      );
    end
    else if val.IsArray then begin
      s := 16 ;
      _layer.GLProperty.SetProperty(
        T_PropertyType.SymbolSize,
        parseValueList( val.AsArray, T_NativePropertyType.Numeric, _layer, c, s )
      );
    end
    else begin
      s := T_ConverterHelper.ParseSize( val.Value ) ;
      _layer.GLProperty.SymbolSize.Add( T_ParamsStops<Single>.Create( -1, s ) ) ;
      _layer.GLProperty.SymbolSize.Add( T_ParamsStops<Single>.Create( -1, s ) ) ;
    end ;
  end ;

  val := _obj.FindObject('icon-rotate') ;
  if assigned( val ) then
    _layer.GLProperty.SymbolRotate := T_ConverterHelper.ParseSize( val.Value ) ; // TODO

end ;

procedure T_StyleConverter.parsePaint(
  const _obj    : TGIS_JSONObject ;
  const _layer  : T_StyleLayer
) ;
var
  paint : TGIS_JSONObject ;
  val   : TGIS_JSONObject ;
  dc    : TGIS_Color ;
  ds    : Single ;
  lst   : T_NativeProperty ;
begin
  paint := _obj.FindObject('paint') ;
  if not assigned( paint ) then exit ;

  if _layer.LayerType = T_StyleLayerType.Fill then
    parseFill( paint, _layer )
  else if _layer.LayerType = T_StyleLayerType.Line then
    parseLine( paint, _layer )
  else if _layer.LayerType = T_StyleLayerType.Symbol then
    parseSymbolPaint( paint, _layer )
  else if _layer.LayerType = T_StyleLayerType.Background then begin
    val := paint.FindObject('background-color') ;
    if assigned( val ) then begin
      if val.IsArray then begin
        lst := parseValueList( val.AsArray, T_NativePropertyType.Color, _layer, dc, ds ) ;
        try
          if lst.Color.Count > 0 then
            _layer.GLProperty.Background := lst.Color[0].Value ;
        finally
          FreeObject( lst ) ;
        end ;
      end
      else begin
        dc := T_ConverterHelper.ParseColor( val.Value ) ;
        _layer.GLProperty.Background := dc ;
      end ;
    end
    else
      _layer.GLProperty.Background := TGIS_Color.None ;
  end ;
end ;

procedure T_StyleConverter.parseLayers(
  const _layers : TGIS_JSONArray
) ;
var
  jlayer : TGIS_JSONObject ;
  la     : T_StyleLayer ;
  i      : Integer ;
begin
  for i := 0 to _layers.Count-1 do begin
    jlayer := _layers[i] ;
    la := T_StyleLayer.Create ;
    la.Read( jlayer );
    FLayers.Add( la ) ;

    la.Query := parseFilter( jlayer ) ;
    parsePaint ( jlayer, la ) ;
    parseLayout( jlayer, la ) ;
  end ;
end ;

function SortByOrder(
  const _p1, _p2 : TGIS_LayerAbstract
) : Integer ;
begin
  if      TGIS_Layer(_p1).Tag > TGIS_Layer(_p2).Tag then Result := 1
  else if TGIS_Layer(_p1).Tag < TGIS_Layer(_p2).Tag then Result := -1
  else                                                   Result := 0 ;
end ;

procedure T_StyleConverter.SortLayers(
  const _layer: TGIS_LayerAbstractList
);
var
  lst : TDictionary<String, Integer> ;
  i   : Integer ;
  idx : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  la  : TGIS_LayerAbstract ;
begin
  lst := TDictionary<String, Integer>.Create ;
  try
    for i := 0 to Layers.Count-1 do begin
      lst.AddOrSetValue( Layers[i].SourceLayer, i) ;
    end ;

    for la in _layer do begin
      if lst.TryGetValue( TGIS_Layer(la).Name, idx ) then
        TGIS_Layer(la).Tag := idx
      else
        TGIS_Layer(la).Tag := 999
    end ;

    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        java.util.Collections.sort( _layer, new T_listSortByOrder ) ;
      {$ELSE}
        _layer.Sort( @SortByOrder );
      {$ENDIF}
    {$ELSE}
      _layer.Sort( TComparer<TGIS_LayerAbstract>.Construct( SortByOrder ) ) ;
    {$ENDIF}
  finally
    FreeObject( lst ) ;
  end ;
end ;

procedure T_StyleConverter.ParseStyle(
  const _path : String
) ;
var
  data  : String ;
  val   : String ;
  la    : TGIS_JSONArray ;
  oRoot : TGIS_JSONObject ;
begin
  if (Pos( 'http:' , _path ) = StringFirst) or
     (Pos( 'https:', _path ) = StringFirst) then
    data := loadFromService( _path )
  else
    data := loadFromFile( _path ) ;

  oRoot := TGIS_JSONObject.ParseJSON( data ) ;
  try
    if not assigned( oRoot ) then exit ;

    if oRoot.TryGetValue<String>('sprite', val) then
      FSpritesUrl := val ;

    la := oRoot.GetValue<TGIS_JSONArray>('layers') ;
    if assigned( la ) then
      parseLayers( la ) ;
  finally
     FreeObject( oRoot ) ;
  end ;

end ;

procedure T_StyleConverter.ParseSprites(
  const _path : String ;
  const _key  : String
 ) ;
var
  sprite_url  : String ;
  sprite_json : String ;
  data        : String ;
  png         : TGIS_Bitmap ;
  oRoot, vals : TGIS_JSONObject ;
  pix         : TGIS_Pixels ;
  itr         : TGIS_JSONIter ;
  x,y,w,h     : Integer ;
  img         : TGIS_Bitmap ;
  ms          : TMemoryStream ;
  path        : String ;
begin
  if IsStringEmpty( _path ) then exit ;

  path := _path ;
  if Pos( 'mapbox://sprites', path ) = StringFirst then
    path := path.Replace( 'mapbox://sprites', 'https://api.mapbox.com/styles/v1' ) +
            '/sprite' ;

  sprite_url  := path + '.png' + _key ;
  sprite_json := path + '.json' + _key ;

  if (Pos( 'http:' , sprite_json ) = StringFirst) or
     (Pos( 'https:', sprite_json ) = StringFirst) then
    data := loadFromService( sprite_json )
  else
    data := loadFromFile( sprite_json ) ;

  oRoot := TGIS_JSONObject.ParseJSON( data ) ;
  if not assigned( oRoot ) then exit ;

  try
    png := loadImage( sprite_url ) ;
    if not assigned( png ) then exit ;

    FSpritesDict := TDictionary< String, TStream>.Create ;
    png.LockPixels(pix);
    if JSONObjectFindFirst( oRoot, itr ) then begin
      try
        while True do begin
          vals := itr.val ;
          x := vals.GetValue<Integer>('x') ;
          y := vals.GetValue<Integer>('y') ;
          w := vals.GetValue<Integer>('width') ;
          h := vals.GetValue<Integer>('height') ;
          img := TGIS_Bitmap.Create( w, h ) ;
          try
            img.DrawBitmap( png, pix, Rect( x, y, x + w, y + h ), Rect( 0, 0, w, h ) ) ;

            ms := TMemoryStream.Create ;
            img.SaveToStream( ms, TGIS_PixelFormat.ARGB, TGIS_PixelSubFormat.PNG, 90 ) ;
            ms.Position := 0 ;
            FSpritesDict.AddOrSetValue( itr.key, ms ) ;
          finally
            FreeObject( img ) ;
          end ;
          if not JSONObjectFindNext( itr ) then break ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
    end ;
    png.UnlockPixels ;
  finally
    FreeObject( png ) ;
    FreeObject( oRoot ) ;
  end ;
end ;

function T_StyleConverter.processColorList(
  const _level    : Integer ;
  const _list     : TList<T_ParamsStops<TGIS_Color>> ;
    var _query    : TGIS_MVTQuery
) : TGIS_Color ;
var
  i  : Integer ;
  j  : Integer ;
  c  : TGIS_Color ;
  bz : Integer ;
  tz : Integer ;
begin
  c := TGIS_Color.None ;
  _query := nil ;
  for i := 0 to _list.Count-2 do begin
    bz := _list[i].Zoom ;
    tz := _list[i+1].Zoom ;

    if ( bz = -1 ) or ((i = 0 ) and ( _level < bz)) then begin
      if not IsStringEmpty( _list[i].Field ) then begin
        _query := TGIS_MVTQuery.Create( _list[i].Field ) ;
        for j := i to _list.Count-1 do
          if (_list[j].Zoom = -1) or (_list[j].Zoom = bz) then
            _query.Add( _list[j].Key, _list[j].Value.ARGB ) ;

        break ;
      end ;
      c := _list[i].Value ;
      break ;
    end
    else if ((_level >= bz) and (_level < tz)) then begin
      if not IsStringEmpty( _list[i].Field ) then begin
        _query := TGIS_MVTQuery.Create( _list[i].Field ) ;
        for j := i to _list.Count-1 do
          if ((_level >= _list[i].Zoom) and (_level < _list[i+1].Zoom)) then
            _query.Add( _list[j].Key, _list[j].Value.ARGB ) ;

        break ;
      end ;
      c := GradientColor( _list[i].Value, _list[i+1].Value, bz, tz, _level, TGIS_ColorInterpolationMode.RGB ) ;
      break ;
    end
    else if (_level >= tz) then begin
      if not IsStringEmpty( _list[i].Field ) then begin
        _query := TGIS_MVTQuery.Create( _list[i].Field ) ;
        for j := i to _list.Count-1 do
          if ( _level >= _list[j].Zoom) then
            _query.Add( _list[j].Key, _list[j].Value.ARGB ) ;

        break ;
      end ;
      c := _list[i+1].Value ;
      break ;
    end
    else
      continue ;
  end ;
  Result := c ;
end ;

function T_StyleConverter.processValueList(
  const _level    : Integer ;
  const _list     : TList<T_ParamsStops<Single>> ;
  const _base     : Double ;
    var _query    : TGIS_MVTQuery
) : Single ;
var
  i  : Integer ;
  j  : Integer ;
  v  : Single ;
  bz : Integer ;
  tz : Integer ;
begin
  v := 0 ;
  _query := nil ;
  for i := 0 to _list.Count-2 do begin
    bz := _list[i].Zoom ;
    tz := _list[i+1].Zoom ;

    if ( bz = -1 ) or ((i = 0 ) and ( _level < bz)) then begin
      if not IsStringEmpty( _list[i].Field ) then begin
        _query := TGIS_MVTQuery.Create( _list[i].Field ) ;
        for j := i to _list.Count-1 do
          if (_list[j].Zoom = -1) or (_list[j].Zoom = bz) then
            _query.Add( _list[j].Key, _list[j].Value ) ;

        break ;
      end ;
      v := _list[i].Value ;
      break ;
    end
    else if ((_level >= bz) and (_level < tz)) then begin
      if not IsStringEmpty( _list[i].Field ) then begin
        _query := TGIS_MVTQuery.Create( _list[i].Field ) ;
        for j := i to _list.Count-2 do
          if ((_level >= _list[i].Zoom) and (_level < _list[i+1].Zoom)) then
            _query.Add( _list[j].Key, _list[j].Value ) ;

        break ;
      end ;
      if _base = 1 then
        v := T_ConverterHelper.InterpolateValue(
                _list[i].Value, _list[i+1].Value, bz, tz, _level
              )
      else
        v := T_ConverterHelper.InterpolateValueExp(
                _list[i].Value, _list[i+1].Value, bz, tz, _level, _base
              ) ;
      break ;
    end
    else if (_level >= tz) then begin
      if not IsStringEmpty( _list[i].Field ) then begin
        _query := TGIS_MVTQuery.Create( _list[i].Field ) ;
        for j := i to _list.Count-1 do
          if ( _level >= _list[j].Zoom) then
          _query.Add( _list[j].Key, _list[j].Value ) ;

        break ;
      end ;
      v := _list[i+1].Value ;
      break ;
    end
    else
      continue ;
  end ;
  Result := v ;
end ;

function T_StyleConverter.buildFill(
  const _level  : Integer ;
  const _glProp : T_Property ;
  const _area   : TGIS_ParamsArea
) : Boolean;
var
  c   : TGIS_Color ;
  w   : Single ;
  b   : Byte ;
  stm : TStream ;
  qry : TGIS_MVTQuery ;
begin
  Result := False ;
  _area.ShowLegend := True ;

  c := processColorList( _level, _glProp.FillColor, qry ) ;
  if c <> TGIS_Color.None then begin
    _area.Color        := c ;
    _area.OutlineColor := c ;
    Result := True ;
  end
  else if assigned( qry ) then begin
    if not assigned( _area.UserObject ) then
      _area.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_area.UserObject).QFillColor := qry ;
    Result := True ;
  end ;

  c := processColorList( _level, _glProp.FillOutlineColor, qry ) ;
  if c <> TGIS_Color.None then begin
    _area.OutlineColor := c ;
    Result := True ;
  end
  else if assigned( qry ) then begin
    if not assigned( _area.UserObject ) then
      _area.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_area.UserObject).QFillOutlineColor := qry ;
    Result := True ;
  end ;

  w := processValueList( _level, _glProp.FillOpacity, _glProp.BaseValue, qry ) ;
  if w <> 0 then begin
    c := _area.Color ;
    b := Byte(RoundS(w*255)) ;
    if assigned(_area.UserObject) and
       (assigned(TGIS_MVTCustomSymbol(_area.UserObject).QFillColor) or
        assigned(TGIS_MVTCustomSymbol(_area.UserObject).QFillColor)) then
      TGIS_MVTCustomSymbol(_area.UserObject).Transparency := w ;

    _area.Color := TGIS_Color.FromARGB( b, c.R, c.G, c.B ) ;
    c := _area.OutlineColor ;
    _area.OutlineColor := TGIS_Color.FromARGB( b, c.R, c.G, c.B ) ;
    Result := True ;
  end
  else if assigned( qry ) then begin
    if not assigned( _area.UserObject ) then
      _area.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_area.UserObject).QFillOpacity := qry ;
    Result := True ;
  end ;

  // TODO parse list
  if not IsStringEmpty( _glProp.FillPattern ) then begin
    if assigned( FSpritesDict ) then begin
      stm := nil ;
      if FSpritesDict.TryGetValue( _glProp.FillPattern, stm ) then begin
        _area.Symbol := SymbolList.Prepare( _glProp.FillPattern + '.png', stm ) ;
        _area.SymbolGap := 0 ;
      end ;
    end ;
    Result := True ;
  end ;
end ;

function T_StyleConverter.buildLine(
  const _level  : Integer ;
  const _glProp : T_Property ;
  const _line   : TGIS_ParamsLine ;
  const _area   : TGIS_ParamsArea
) : Boolean ;
var
  c   : TGIS_Color ;
  w   : Single ;
  b   : Byte ;
  qry : TGIS_MVTQuery ;

  function makeLineDash(
    const _dash     : TArray<Single> ;
    const _offset   : Single
  ) : String ;
  var
    sb  : TStringBuilder ;
    i   : Integer ;
  begin
    sb := TStringBuilder.Create ;
    try
      i := 0 ;
      sb.Append( '&' ) ;
      sb.Append( 'F(100%)' ) ;
      while i < length(_dash)-1 do begin
        if _offset <> 0 then
          sb.Append( Format( 'M(%dW 0)', [RoundS(_offset)] ) ) ;
        sb.Append( Format('L(%dW)M(%dW 0)', [RoundS(_dash[i]*10), RoundS(_dash[i+1]*10)] ) ) ;
        i := i + 2 ;
      end ;
      sb.Append( 'E()' ) ;
      Result := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

begin
  Result := False ;
  _line.ShowLegend := True ;
  _area.Pattern := TGIS_BrushStyle.Clear ;

  c := processColorList( _level, _glProp.LineColor, qry ) ;
  if c <> TGIS_Color.None then begin
    _line.Color := c ;
    _line.Width := -1 ;
    _area.OutlineColor := c ;
    Result := True ;
  end
  else if assigned( qry ) then begin
    if not assigned( _line.UserObject ) then
      _line.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_line.UserObject).QLineColor := qry ;
    Result := True ;
  end ;

  w := processValueList( _level, _glProp.LineOpacity, _glProp.BaseValue, qry ) ;
  if w <> 0 then begin
    c := _line.Color ;
    b := Byte(RoundS(w*255)) ;
    _line.Color := TGIS_Color.FromARGB( b, c.R, c.G, c.B ) ;
    _area.OutlineColor := _line.Color ;
    Result := True ;
  end
  else if assigned( qry ) then begin
    if not assigned( _line.UserObject ) then
      _line.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_line.UserObject).QLineOpacity := qry ;
    Result := True ;
  end ;

  w := processValueList( _level, _glProp.LineWidth, _glProp.BaseValue, qry ) ;
  if w <> 0 then begin
    if w < 0 then begin
      _line.Width        := RoundS(w*FPixelSizeFactor) ;
      _area.OutlineWidth := _line.Width ;
    end
    else begin
      _line.Width        := RoundS(w*FPixelSizeFactor * 20) ; // TODO use asText pt
      _area.OutlineWidth := _line.Width ;
    end ;
    Result := True ;
  end
  else if assigned( qry ) then begin
    if not assigned( _line.UserObject ) then
      _line.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_line.UserObject).QLineWidth := qry ;
    Result := True ;
  end ;

  if _glProp.LineDash then begin
    _line.Style        := TGIS_PenStyle.Dash ;
    _area.OutlineStyle := TGIS_PenStyle.Dash ;
    // TODO slow
//    if assigned( _glProp.LineDashArray ) then begin
//      _line.StyleAsText := GIS_PARAMTXT_TYPE_SYMBOL + ':' +
//                           makeLineDash( _glProp.LineDashArray, _glProp.LineOffset ) ;
//      _area.OutlineStyleAsText := _line.StyleAsText ;
//    end ;
  end
  else begin
    _line.Style        := TGIS_PenStyle.Solid ;
    _area.OutlineStyle := TGIS_PenStyle.Solid ;
  end ;
end ;

function T_StyleConverter.buildSymbol(
  const _level  : Integer ;
  const _glProp : T_Property ;
  const _marker : TGIS_ParamsMarker ;
  const _label  : TGIS_ParamsLabel
) : Boolean ;
var
  c   : TGIS_Color ;
  w   : Single ;
  sw  : Integer ;
  i   : Integer ;
  p   : Integer ;
  arr : TArray<String> ;
  stm : TStream ;
  str : String ;
  scl : String ;
  qry : TGIS_MVTQuery ;
begin
  Result := True ;
  _label.Duplicates := False ;
  _label.ShowLegend := True ;
  _label.Value      := _glProp.TextField ;
  if _label.Value = '{}' then
    _label.Value := '' ;

  if not IsStringEmpty(_glProp.TextFontFamily) then begin
    _label.FontName   := _glProp.TextFontFamily ;
    _label.Font.Style := _glProp.TextFontStyle ;
  end ;

  _label.Alignment := _glProp.TextJustify ;
  if _glProp.TextPositions <> GisGetEmptyLabelPosition then
    _label.Position  := _glProp.TextPositions
  else
    _label.Position  := [TGIS_LabelPosition.MiddleCenter] ;

  w := processValueList( _level, _glProp.TextSize, _glProp.BaseValue, qry ) ;
  if w <> 0 then begin
    _label.FontSizeAsText := Format( '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, TruncS(FPixelSizeFactor*w)] ) ;
  end
  else if assigned( qry ) then begin
    if not assigned( _label.UserObject ) then
      _label.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_label.UserObject).QTextSize := qry ;
  end ;

  if _glProp.TextAlongLine then
    _label.Alignment := TGIS_LabelAlignment.Follow
  else if _glProp.TextWrap then begin
    _label.Alignment := TGIS_LabelAlignment.Center ;
    _label.Width     := 8 * _label.FontSize ;
  end ;

  _label.Color     := TGIS_Color.White ;
  _label.FontColor := TGIS_Color.Black ;

  c := processColorList( _level, _glProp.TextColor, qry ) ;
  if c <> TGIS_Color.None then begin
    _label.Color     := c ;
    _label.FontColor := c ;
  end
  else if assigned( qry ) then begin
    if not assigned( _label.UserObject ) then
      _label.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_label.UserObject).QTextColor := qry ;
  end ;

  c := processColorList( _level, _glProp.TextHaloColor, qry ) ;
  if c <> TGIS_Color.None then begin
    _label.Color := c ;
  end
  else if assigned( qry ) then begin
    if not assigned( _label.UserObject ) then
      _label.UserObject := TGIS_MVTCustomSymbol.Create ;
    TGIS_MVTCustomSymbol(_label.UserObject).QTextHaloColor := qry ;
  end ;

  if _glProp.TextRotate <> 0 then
    _label.Rotate := DegToRad( _glProp.TextRotate )
  else if not IsStringEmpty( _glProp.TextRotateField ) then
    _label.RotateAsText := Format( 'FIELD:%s:-1 deg', [_glProp.TextRotateField] )
  else
    _glProp.TextRotate := 0 ;

  _label.Pattern      := TGIS_BrushStyle.Clear ;
  _label.OutlineStyle := TGIS_PenStyle.Clear ;

  _marker.Size := 0 ;
  if not IsStringEmpty( _glProp.SymbolImage ) then begin
    _marker.ShowLegend := True ;

    // TODO SymbolImage

    if (Pos('City', _glProp.SymbolImage ) >= StringFirst) or
       (Pos('dot_', _glProp.SymbolImage ) >= StringFirst) then begin
      _marker.Style := TGIS_MarkerStyle.Circle ;
      _marker.Color := TGIS_Color.White ;
      _marker.OutlineWidth := 1 ;
      _marker.Size := 100 ;
    end
    else if Pos('Road', _glProp.SymbolImage ) >= StringFirst then begin
      _label.OutlineColor := _label.FontColor ;
      _label.Alignment    := TGIS_LabelAlignment.Center ;

      str := StringReplaceAll( _glProp.SymbolImage, '/{_len}', '') ;
      // guess shield colors
      {$IFDEF JAVA OR ISLAND}
        arr := str.Split( ' ' ).ToArray ; //?
      {$ELSE}
        arr := str.Split( [' '] ) ;
      {$ENDIF}

      p := 0 ;
      for i := 1 to high(arr) do begin
        if FColorDict.TryGetValue( arr[i], c ) then begin
          inc( p ) ;
          case p of
            1 : begin
                  _label.Color := c ;
                  scl := arr[i] ;
                end;
            2 : _label.OutlineColor := c ;
          end ;
        end ;
      end ;
      if (high(arr) > 0) and FShieldDict.TryGetValue( arr[0], str ) then begin
        _label.Shield := SymbolList.Prepare( Format( str, [scl] ) ) ;
        w := processValueList( _level, _glProp.TextSize, _glProp.BaseValue, qry )-1 ;
        if w > 0 then begin
          _label.FontSizeAsText := Format( '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, TruncS(FPixelSizeFactor*w)] ) ;
        end ;
      end ;

      if not assigned( _label.Shield ) then begin
        _label.Pattern      := TGIS_BrushStyle.Solid ;
        _label.OutlineStyle := TGIS_PenStyle.Solid ;
      end;

      if _label.Color = _label.FontColor then
        _label.FontColor := _label.OutlineColor ;
    end
    else if assigned( FSpritesDict ) then begin
      stm := nil ;
      str := _glProp.SymbolImage ;
      if FSpritesDict.TryGetValue( str, stm ) then begin
        _marker.Symbol := SymbolList.Prepare( _glProp.SymbolImage + '.png', stm ) ;
        w := processValueList( _level, _glProp.SymbolSize, _glProp.BaseValue, qry ) ;
        if w <> 0 then
          sw := RoundS(_marker.Symbol.NativeSize * w)
        else begin
          if assigned( qry ) then begin
            if not assigned( _marker.UserObject ) then
              _marker.UserObject := TGIS_MVTCustomSymbol.Create ;
            TGIS_MVTCustomSymbol(_marker.UserObject).QSymbolSize := qry ;
          end ;
          sw := RoundS(_marker.Symbol.NativeSize) ;
        end ;
        _marker.SizeAsText := Format( '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, sw] ) ;

        if not IsStringEmpty( _label.Value ) then begin
          _label.OutlineColor := _label.FontColor ;
          _label.Alignment    := TGIS_LabelAlignment.Center ;
          _label.Shield       := _marker.Symbol ;
          w := processValueList( _level, _glProp.TextSize, _glProp.BaseValue, qry )-1 ;
          if w > 0 then
            _label.FontSizeAsText := Format( '%s:%ddip', [GIS_PARAMTXT_TYPE_SIZE, TruncS(FPixelSizeFactor*w)] ) ;
          if _label.Color = _label.FontColor then
            _label.FontColor := _label.OutlineColor ;
        end;
      end
      else begin
        _marker.UserObject := TGIS_MVTCustomSymbol.Create ;
        w := processValueList( _level, _glProp.SymbolSize, _glProp.BaseValue, qry ) ;
        if w = 0 then begin
          if assigned( qry ) then begin
            if not assigned( _marker.UserObject ) then
              _marker.UserObject := TGIS_MVTCustomSymbol.Create ;
            TGIS_MVTCustomSymbol(_marker.UserObject).QSymbolSize := qry ;
          end ;
          w := 1 ;
        end ;
        TGIS_MVTCustomSymbol(_marker.UserObject).SymbolWidth := w ;
        TGIS_MVTCustomSymbol(_marker.UserObject).SymbolName  := _glProp.SymbolImage ;
      end ;
    end ;

    if _glProp.SymbolColor <> TGIS_Color.None then
      _marker.Color := _glProp.SymbolColor ;

    _marker.SymbolRotate := DegToRad( _glProp.SymbolRotate ) ;
  end ;

end ;

procedure T_StyleConverter.BuildParams(
  const _layer : TGIS_Layer ;
  const _level : Integer
) ;
var
  la      : T_StyleLayer ;
  lv      : TGIS_LayerVector ;
begin
  if not assigned( _layer ) then exit ;

  lv := _layer as TGIS_LayerVector ;
  lv.Active := False ;

  for la in FLayers do begin
    if lv.Name <> la.SourceLayer then continue ;

    if ((la.MinLevel >=0) and (_level < la.MinLevel)) or
       ((la.MaxLevel >=0) and (_level > la.MaxLevel)) then begin
      continue ;
    end ;

    lv.Active := lv.Active or la.Visible ;

    if not IsStringEmpty( la.Query ) and
       not IsStringEmpty( lv.Params.Query ) then
      lv.ParamsList.Add ;

    if not IsStringEmpty( la.Query ) then begin
      lv.Params.Query  := la.Query ;
      lv.Params.Legend := la.Query ;
    end ;

    if la.LayerType = T_StyleLayerType.Fill then begin
      lv.Params.Marker.Size := 0 ;
      lv.Params.Visible := buildFill( _level, la.GLProperty, lv.Params.Area ) ;
    end
    else if la.LayerType = T_StyleLayerType.Line then begin
      lv.MultipassRendering := True ;
      lv.Params.Marker.Size := 0 ;
      lv.Params.Visible := buildLine( _level, la.GLProperty, lv.Params.Line,
                                      lv.Params.Area
                                    ) ;
    end
    else if la.LayerType = T_StyleLayerType.Symbol then begin
      lv.Params.Line.Width := 0 ;
      lv.Params.Visible := buildSymbol( _level, la.GLProperty, lv.Params.Marker,
                                        lv.Params.Labels
                                      ) ;
    end ;
  end ;
end ;

function T_StyleConverter.GetSprite(
  const _name : String
) : TGIS_SymbolAbstract ;
var
  stm : TStream ;
begin
  Result := nil ;
  if not assigned( FSpritesDict ) then exit ;

  if FSpritesDict.TryGetValue( _name, stm ) then
    Result := SymbolList.Prepare( _name + '.png', stm ) ;
end ;

//=============================================================================
// TGIS_MVTStyler
//=============================================================================

constructor TGIS_MVTStyler.Create ;
begin
  inherited ;

  oConverter := T_StyleConverter.Create ;
end ;

procedure TGIS_MVTStyler.doDestroy ;
begin
  FreeObject( oConverter ) ;

  inherited ;
end ;

{$IFDEF GIS_TEST}
procedure TGIS_MVTStyler.Dump(
  const _list : TStrings
) ;
var
  la : T_StyleLayer ;
begin
  for la IN T_StyleConverter(oConverter).Layers do
    _list.Append( la.AsString ) ;
end ;

procedure TGIS_MVTStyler.Dump(
  const _list   : TStrings ;
  const _layers : TGIS_LayerAbstractList
) ;
var
  la : T_StyleLayer ;
  i  : Integer ;
begin
  for la in T_StyleConverter(oConverter).Layers do begin
    for i := 0 to _layers.Count - 1 do begin
      if TGIS_Layer(_layers[i]).Name = la.SourceLayer then
        _list.Append( la.AsString ) ;
    end ;
  end ;
end ;
{$ENDIF}

function TGIS_MVTStyler.fget_BackgroundColor : TGIS_Color ;
begin
  Result := T_StyleConverter(oConverter).BackgroundColor ;
end ;

function TGIS_MVTStyler.fget_PixelSizeFactor: Double;
begin
  Result := T_StyleConverter(oConverter).PixelSizeFactor ;
end;

function TGIS_MVTStyler.fget_SpritesUrl: String;
begin
  Result := T_StyleConverter(oConverter).SpritesUrl ;
end ;

procedure TGIS_MVTStyler.fset_PixelSizeFactor(
  const _factor : Double
) ;
begin
  T_StyleConverter(oConverter).PixelSizeFactor := _factor ;
end;

procedure TGIS_MVTStyler.ParseSprites(
  const _path : String ;
  const _key  : String
) ;
begin
  try
    T_StyleConverter(oConverter).ParseSprites( _path, _key ) ;
  except
    on e : Exception do
      TGIS_Logger.AsError( e.Message, 'Error parsing sprites' ) ;
  end ;
end ;

procedure TGIS_MVTStyler.ParseStyle(
  const _path : String
) ;
begin
  try
    T_StyleConverter(oConverter).ParseStyle( _path ) ;
  except
    on e : Exception do
      TGIS_Logger.AsError( e.Message, 'Error parsing style' ) ;
  end ;
end ;

procedure TGIS_MVTStyler.SortLayers(
  const _layers  : TGIS_LayerAbstractList
);
begin
  T_StyleConverter(oConverter).SortLayers( _layers ) ;
end ;

procedure TGIS_MVTStyler.BuildParams(
  const _layer  : TGIS_Layer ;
  const _level  : Integer
);
begin
  T_StyleConverter(oConverter).BuildParams( _layer, _level ) ;
end ;

procedure TGIS_MVTStyler.Clear ;
begin
  T_StyleConverter(oConverter).Clear ;
end ;

function TGIS_MVTStyler.GetSprite(
  const _name : String
) : TGIS_SymbolAbstract ;
begin
  Result := T_StyleConverter(oConverter).GetSprite( _name ) ;
end ;

{ TGIS_MVTCustomSymbol }

constructor TGIS_MVTCustomSymbol.Create;
begin
  inherited ;

  SymbolName        := '' ;
  SymbolWidth       := 0 ;
  Transparency      := 1 ;
  QFillColor        := nil ;
  QFillPattern      := nil ;
  QFillOutlineColor := nil ;
  QFillOpacity      := nil ;
  QLineColor        := nil ;
  QLineWidth        := nil ;
  QLineOpacity      := nil ;
  QTextSize         := nil ;
  QTextColor        := nil ;
  QTextHaloColor    := nil ;
  QSymbolSize       := nil ;
end;

function TGIS_MVTCustomSymbol.CreateCopy : TGIS_ParamsUserObject ;
var
  res : TGIS_MVTCustomSymbol ;

  function copy_query( const _src : TGIS_MVTQuery ) : TGIS_MVTQuery ;
  begin
    if assigned( _src ) then
      Result := _src.CreateCopy
    else
      Result := nil ;
  end ;

begin
  res := TGIS_MVTCustomSymbol.Create ;
  res.SymbolName        := SymbolName ;
  res.SymbolWidth       := SymbolWidth ;
  res.Transparency      := Transparency ;
  res.QFillColor        := copy_query( QFillColor ) ;
  res.QFillPattern      := copy_query( QFillPattern ) ;
  res.QFillOutlineColor := copy_query( QFillOutlineColor ) ;
  res.QFillOpacity      := copy_query( QFillOpacity ) ;
  res.QLineColor        := copy_query( QLineColor ) ;
  res.QLineWidth        := copy_query( QLineWidth ) ;
  res.QLineOpacity      := copy_query( QLineOpacity ) ;
  res.QTextSize         := copy_query( QTextSize ) ;
  res.QTextColor        := copy_query( QTextColor ) ;
  res.QTextHaloColor    := copy_query( QTextHaloColor ) ;
  res.QSymbolSize       := copy_query( QSymbolSize ) ;

  Result := res ;
end;

procedure TGIS_MVTCustomSymbol.doDestroy ;
begin
  FreeObject( QFillColor        ) ;
  FreeObject( QFillPattern      ) ;
  FreeObject( QFillOutlineColor ) ;
  FreeObject( QFillOpacity      ) ;
  FreeObject( QLineColor        ) ;
  FreeObject( QLineWidth        ) ;
  FreeObject( QLineOpacity      ) ;
  FreeObject( QTextSize         ) ;
  FreeObject( QTextColor        ) ;
  FreeObject( QTextHaloColor    ) ;
  FreeObject( QSymbolSize       ) ;

  inherited ;
end ;

{ TGIS_MVTQuery }

constructor TGIS_MVTQuery.Create(
  const _field : String
) ;
begin
  inherited Create ;

  Field := _field ;
  Cases := TDictionary<String, Variant>.Create ;
end ;

procedure TGIS_MVTQuery.Add(
  const _key  : String ;
  const _val  : Variant
) ;
begin
  Cases.AddOrSetValue( _key, _val ) ;
end ;


constructor TGIS_MVTQuery.Create(
  const _field      : String ;
  {$IFDEF DCC}
  const _collection : TEnumerable<TPair<String, Variant>>
  {$ENDIF}
  {$IFDEF CLR}
  const _collection : IDictionary<String, Variant>
  {$ENDIF}
  {$IFDEF JAVA}
  const _collection : TDictionary<String, Variant>
  {$ENDIF}
) ;
begin
  inherited Create ;

  Field := _field ;
  Cases := TDictionary<String, Variant>.Create( _collection ) ;
end ;

function TGIS_MVTQuery.CreateCopy : TGIS_MVTQuery ;
begin
  Result := TGIS_MVTQuery.Create( Field, Cases ) ;
end ;

procedure TGIS_MVTQuery.doDestroy ;
begin
  FreeObject( Cases ) ;

  inherited ;
end ;

end.

