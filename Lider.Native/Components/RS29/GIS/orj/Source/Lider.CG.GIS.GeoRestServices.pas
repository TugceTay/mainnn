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
  Utilities for ESRI ArcGIS REST Services.
}

{$IFDEF DCC}
  unit GisRestServices ;
  {$HPPEMIT '#pragma link "GisRestServices"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,

    GisTypes,
    GisRtl,
    GisParams,
    GisLayerJSON ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///    Feature Service info.
  /// </summary>
  TGIS_ArcGISFeatureServerLayer = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( TGIS_ObjectDisposable )
    protected
      // destructor
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Feature Layer id.
      /// </summary>
      Id            : Integer           ;
      /// <summary>
      ///   Feature Layer name.
      /// </summary>
      Name          : String           ;
      /// <summary>
      ///   Feature Layer description.
      /// </summary>
      Description   : String           ;
      /// <summary>
      ///   Feature Layer bounding box.
      /// </summary>
      Extent        : TGIS_Extent      ;
      /// <summary>
      ///   Feature Layer coordinate system id.
      /// </summary>
      Wkid          : String           ;
      /// <summary>
      ///   Feature Layer geometry type.
      /// </summary>
      GeometryType  : String ;
      /// <summary>
      ///   Feature Layer path.
      /// </summary>
      Path          : String ;
      /// <summary>
      ///   Feature Layer drawing info.
      /// </summary>
      DrawingInfo   : TGIS_ParamsList ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   List of TGIS_ArcGISFeatureServerLayer.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_ArcGISFeatureServerLayerList = {$IFDEF OXYGENE} public {$ENDIF}
                                        TList< TGIS_ArcGISFeatureServerLayer > ;
  {$ELSE}
    TGIS_ArcGISFeatureServerLayerList = class (
                                          TList< TGIS_ArcGISFeatureServerLayer >
                                        ) ;
  {$ENDIF}

  /// <summary>
  /// Encapsulation of ArcGIS feature server.
  /// </summary>
  TGIS_ArcGISFeatureServer = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_ObjectDisposable )
    private
      FUrl    : String ;
      FLayers : TGIS_ArcGISFeatureServerLayerList ;
    private
      procedure connectToService( const _url : String ) ;
    protected
      // destructor
      procedure doDestroy ; override;

    public
      // constructors

      /// <summary>
      /// Constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      /// Open a service from url.
      /// </summary>
      /// <param name="_url">
      ///   service url
      /// </param>
      procedure Open       ( const _url : String
                           ) ;
    public
      /// <summary>
      /// List of available layers.
      /// </summary>
      property Layers : TGIS_ArcGISFeatureServerLayerList read FLayers ;
  end ;


implementation

//##############################################################################

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Variants,
    System.Math,
    {$IFDEF LEVEL_XE7_RTL}
      System.NetEncoding,
    {$ENDIF}
    GisRegistredLayers,
    GisClasses,
    GisFileJSON,
    GisFunctions,
    GisInternals,
    GisResource,
    GisSymbol,
    GisTypesUI ;
{$ENDIF}

type

  TGIS_RestServiceUtils = class( TGIS_ObjectDisposable )
    private
      tkn : TGIS_JSONTokenizer ;
    private
      function queryServiceAsJSON( const _url   : String
                                 ) : TGIS_JSONObject ;
      function makeUrl           ( const _path  : String ;
                                   const _query : String
                                 ) : String ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;
    public
      function GetServiceInfo   ( const _baseurl : String
                                ) : TGIS_JSONObject ;
      function GetLayerInfo     ( const _baseurl : String
                                ) : TGIS_JSONObject ;
      function GetObjectIds     ( const _baseurl : String
                                ) : TGIS_JSONObject ;
      function GetObjectsUrl    ( const _baseurl    : String ;
                                  const _objectIds  : String ;
                                  const _crs        : String ;
                                  const _fetchGeom  : Boolean ;
                                  const _fieldNames : String ;
                                  const _fetchZ     : Boolean ;
                                  const _fetchM     : Boolean ;
                                  const _filterRect : TGIS_Extent
                                ) : String ;
      function GetObjects       ( const _baseurl    : String ;
                                  const _objectIds  : String ;
                                  const _crs        : String ;
                                  const _fetchGeom  : Boolean ;
                                  const _fetchAttr  : String ;
                                  const _fetchZ     : Boolean ;
                                  const _fetchM     : Boolean ;
                                  const _filterRect : TGIS_Extent
                                ) : TGIS_JSONObject ;
  end ;

  TGIS_DrawingInfo = class
    private
      class function  convertColor      ( const _data   : TGIS_JSONArray
                                        ) : TGIS_Color ;
      class function  convertLineStyle  ( const _style  : String
                                        ) : TGIS_PenStyle ;
      class function  convertFillStyle  ( const _style  : String
                                        ) : TGIS_BrushStyle ;
      class function  convertMarkerStyle( const _style  : String
                                        ) : TGIS_MarkerStyle ;
      class function  convertLabelPlace( const _place   : String
                                        ) : TGIS_LabelPositions ;
      class function  convertLabelAlign ( const _align  : String
                                        ) : TGIS_LabelAlignment ;
      class function  convertBase64     ( const _data   : String
                                        ) : TBytes ;
    private
      class procedure convertSymbol     ( const _json   : TGIS_JSONObject ;
                                          const _params : TGIS_ParamsSectionVector
                                        ) ;
      class procedure parseLine         ( const _json   : TGIS_JSONObject ;
                                          const _line   : TGIS_ParamsLine
                                        ) ;
      class procedure parseFill         ( const _json   : TGIS_JSONObject ;
                                          const _area   : TGIS_ParamsArea
                                        ) ;
      class procedure parsePictureFill  ( const _json   : TGIS_JSONObject ;
                                          const _area   : TGIS_ParamsArea
                                        ) ;
      class procedure parseMarker       ( const _json   : TGIS_JSONObject ;
                                          const _marker : TGIS_ParamsMarker
                                        ) ;
      class procedure parsePictureMarker( const _json   : TGIS_JSONObject ;
                                          const _marker : TGIS_ParamsMarker
                                        ) ;
    private
      class procedure convertRenderer   ( const _json       : TGIS_JSONObject ;
                                          const _paramsList : TGIS_ParamsList
                                        ) ;
      class procedure convertLabeling   ( const _json       : TGIS_JSONObject ;
                                          const _paramsList : TGIS_ParamsList
                                        ) ;
    public
      class function GetDrawingInfo     ( const _json       : TGIS_JSONObject
                                        ) : TGIS_ParamsList ; static ;
  end ;

//==============================================================================
// TGIS_ArcGISFeatureServerLayer
//==============================================================================

  procedure TGIS_ArcGISFeatureServerLayer.doDestroy ;
  begin
    FreeObject( DrawingInfo ) ;

    inherited ;
  end ;

//==============================================================================
// TGIS_RestServiceUtils
//==============================================================================

  function TGIS_RestServiceUtils.makeUrl(
    const _path   : String ;
    const _query  : String
  ) : String ;
  var
    k     : Integer ;
    spath : String  ;
    query : String  ;

    // guarantee that query parameters will
    // replace any original URL parameters
    procedure replace_parameters ;
    var
      lst_path  : TStringList ;
      lst_query : TStringList ;
      str_path1 : String      ;
      str_path2 : String      ;
      ii        : Integer     ;
      kk        : Integer     ;
    begin
      lst_path := nil ;
      lst_query := nil ;
      try
        if IsStringEmpty( query ) then
          exit ;

        str_path2 := URLGetQuery( spath ) ;
        if IsStringEmpty( str_path2 ) then
          exit ;
        str_path1 := URLGetPath ( spath ) ;

        lst_path  := URLGetParameters( '?' + str_path2 ) ;
        lst_query := URLGetParameters( '?' + query     ) ;

        for ii := 0 to lst_query.Count - 1 do begin
          kk := lst_path.IndexOfName( lst_query.Names[ii] ) ;
          if kk >= 0 then begin
            lst_path.Delete( kk );
          end ;
        end ;

        query := '' ;
        for ii := 0 to lst_query.Count - 1 do begin
          if ii = 0 then
            query := query + lst_query[ii]
          else
            query := query + '&' + lst_query[ii] ;
        end ;

        spath := str_path1 ;
        for ii := 0 to lst_path.Count - 1 do begin
          if ii = 0 then
            spath := spath + lst_path[ii]
          else
            spath  := spath + '&' + lst_path[ii] ;
        end ;

      finally
        FreeObject( lst_path  ) ;
        FreeObject( lst_query ) ;
      end ;
    end ;

  begin
    if IsStringEmpty( _path ) then exit ;

    spath  := _path  ;
    query := _query ;

    replace_parameters ;

    // trim any '?' at the tail
    k := length( spath ) ;
    while spath[k+StringFirst-1] = '?' do begin
      dec( k ) ;
      SetLengthStr( spath, k ) ;
    end ;

    // trim any '&' at the tail
    k := length( spath ) ;
    while spath[k+StringFirst-1] = '&' do begin
      dec( k ) ;
      SetLengthStr( spath, k ) ;
    end ;

    // if '?' exist in the middle of the spath then add '&'
    // between spath and _query; otherwise add '?'
    if Pos( '?', spath ) > StringFirst then
      Result := spath + '&' + query
    else
      Result := spath + '?' + query ;
  end ;

  function TGIS_RestServiceUtils.queryServiceAsJSON(
    const _url : String
  ) : TGIS_JSONObject ;
  var
    r     : TGIS_HttpResponse ;
    buf   : TBytes ;
    data  : String ;
  begin
    Result := nil ;

    r.Stream := nil ;
    try
      r := TGIS_WebUtils.HttpFetch( _url, nil, nil, true, 40000,
                                    GetDefaultUserAgent( 'ttkWP' ), '', '', ''
                                   ) ;
      if r.Status = GIS_HTTP_OK then begin
        r.Stream.Position := 0 ;
        SetLength( buf, r.Stream.Size ) ;
       {$IFDEF OXYGENE}
         r.Stream.Read( buf, r.Stream.Size ) ;
       {$ELSE}
         r.Stream.ReadBuffer( buf[0], r.Stream.Size ) ;
       {$ENDIF}
       data := TEncoding.UTF8.GetString( buf, 0, r.Stream.Size ) ;
      end ;
    finally
      FreeObject( r.Stream ) ;
    end ;

    tkn.Reset ;
    Result := tkn.Parse( data ) ;
  end ;

  constructor TGIS_RestServiceUtils.Create ;
  begin
    inherited ;

    tkn := TGIS_JSONTokenizer.Create ;
  end ;

  procedure TGIS_RestServiceUtils.doDestroy ;
  begin
    FreeObject( tkn ) ;

    inherited ;
  end ;

  function TGIS_RestServiceUtils.GetServiceInfo(
    const _baseurl : String
  ) : TGIS_JSONObject ;
  var
    url : String ;
  begin
    url := makeUrl( _baseurl, 'f=json' ) ;
    Result := queryServiceAsJSON( url ) ;
  end ;

  function TGIS_RestServiceUtils.GetLayerInfo(
    const _baseurl : String
  ) : TGIS_JSONObject ;
  var
    url : String ;
  begin
    url := makeUrl( _baseurl, 'f=json' ) ;
    Result := queryServiceAsJSON( url ) ;
  end ;

  function TGIS_RestServiceUtils.GetObjectIds(
    const _baseurl : String
  ) : TGIS_JSONObject ;
  var
    url : String ;
  begin
    url := makeUrl( _baseurl + '/query', 'f=json' ) ;
    url := makeUrl( url, 'where=objectid%3Dobjectid' ) ;
    url := makeUrl( url, 'returnIdsOnly=true' ) ;
    Result := queryServiceAsJSON( url ) ;
  end ;

  function TGIS_RestServiceUtils.GetObjectsUrl(
    const _baseurl    : String ;
    const _objectIds  : String ;
    const _crs        : String ;
    const _fetchGeom  : Boolean ;
    const _fieldNames : String ;
    const _fetchZ     : Boolean ;
    const _fetchM     : Boolean ;
    const _filterRect : TGIS_Extent
  ) : String ;
  var
    url   : String ;
    epsx  : Double ;
    epsy  : Double ;
    precx : Integer ;
    precy : Integer ;
    fext  : TGIS_Extent ;
  begin
    url := makeUrl( _baseurl + '/query', 'f=json' ) ;
    url := makeUrl( url, 'objectIds=' + _objectIds ) ;
    url := makeUrl( url, 'inSR=' + _crs ) ;
    url := makeUrl( url, 'outSR=' + _crs ) ;
    url := makeUrl( url, 'outFields=' + _fieldNames ) ;

    if _fetchGeom then
      url := makeUrl( url, 'returnGeometry=true' )
    else
      url := makeUrl( url, 'returnGeometry=false' ) ;

    if _fetchZ then
      url := makeUrl( url, 'returnZ=true' )
    else
      url := makeUrl( url, 'returnZ=false' ) ;

    if _fetchM then
      url := makeUrl( url, 'returnM=true' )
    else
      url := makeUrl( url, 'returnM=false' ) ;

    if not GisIsNoWorld( _filterRect ) and
       not GisIsWholeWorld( _filterRect ) then begin

      precx := FloorS( Log10( Abs(_filterRect.XMax - _filterRect.XMin) ) ) - 1 ;
      precy := FloorS( Log10( Abs(_filterRect.YMax - _filterRect.YMin) ) ) - 1 ;
      epsx  := Power( 10, precx ) ;
      epsy  := Power( 10, precy ) ;
      fext  := GisExtent( _filterRect.XMin - epsx, _filterRect.YMin - epsy,
                          _filterRect.XMax + epsx, _filterRect.YMax + epsy
                        ) ;
      url := makeUrl( url, Format( 'geometry=%s,%s,%s,%s',
                                   [ DotFloatToStr( fext.XMin ),
                                     DotFloatToStr( fext.YMin ),
                                     DotFloatToStr( fext.XMax ),
                                     DotFloatToStr( fext.YMax )
                                   ] )
                    ) ;
      url := makeUrl( url, 'geometryType=esriGeometryEnvelope' ) ;
      url := makeUrl( url, 'spatialRel=esriSpatialRelEnvelopeIntersects' )
    end ;
    Result := url ;
  end ;

  function TGIS_RestServiceUtils.GetObjects(
    const _baseurl    : String ;
    const _objectIds  : String ;
    const _crs        : String ;
    const _fetchGeom  : Boolean ;
    const _fetchAttr  : String ;
    const _fetchZ     : Boolean ;
    const _fetchM     : Boolean ;
    const _filterRect : TGIS_Extent
  ) : TGIS_JSONObject ;
  var
    url   : String ;
  begin
    url :=  GetObjectsUrl( _baseurl, _objectIds, _crs, _fetchGeom, _fetchAttr,
                           _fetchZ, _fetchM, _filterRect
                         ) ;
    Result := queryServiceAsJSON( url ) ;
  end ;

//==============================================================================
// TGIS_DrawingInfo
//==============================================================================

  class function TGIS_DrawingInfo.convertColor(
    const _data : TGIS_JSONArray
  ) : TGIS_Color ;
  begin
    if not assigned( _data ) or (_data.Count < 4) then
      Result := TGIS_Color.None
    else
      Result := TGIS_Color.FromARGB(
                  _data[3].AsInteger,
                  _data[0].AsInteger,
                  _data[1].AsInteger,
                  _data[2].AsInteger
                ) ;
  end ;

  class function TGIS_DrawingInfo.convertFillStyle(
    const _style : String
  ) : TGIS_BrushStyle ;
  begin
    if       _style = 'esriSFSBackwardDiagonal' then
      Result := TGIS_BrushStyle.BDiagonal
    else if  _style = 'esriSFSCross'            then
      Result := TGIS_BrushStyle.Cross
    else if  _style = 'esriSFSDiagonalCross'    then
      Result := TGIS_BrushStyle.DiagCross
    else if  _style = 'esriSFSForwardDiagonal'  then
      Result := TGIS_BrushStyle.FDiagonal
    else if  _style = 'esriSFSHorizontal'       then
      Result := TGIS_BrushStyle.Horizontal
    else if  _style = 'esriSFSNull'             then
      Result := TGIS_BrushStyle.Clear
    else if  _style = 'esriSFSSolid'            then
      Result := TGIS_BrushStyle.Solid
    else if  _style = 'esriSFSVertical'         then
      Result := TGIS_BrushStyle.Vertical
    else
      Result := TGIS_BrushStyle.Solid ;
  end ;

  class function TGIS_DrawingInfo.convertLineStyle(
    const _style : String
  ) : TGIS_PenStyle ;
  begin
    if       _style = 'esriSLSSolid'      then
      Result := TGIS_PenStyle.Solid
    else if  _style = 'esriSLSDash'       then
      Result := TGIS_PenStyle.Dash
    else if  _style = 'esriSLSDashDot'    then
      Result := TGIS_PenStyle.DashDot
    else if  _style = 'esriSLSDashDotDot' then
      Result := TGIS_PenStyle.DashDotDot
    else if  _style = 'esriSLSDot'        then
      Result := TGIS_PenStyle.Dot
    else if  _style = 'esriSLSNull'       then
      Result := TGIS_PenStyle.Clear
    else
      Result := TGIS_PenStyle.Solid ;
  end ;

  class function TGIS_DrawingInfo.convertMarkerStyle(
    const _style : String
  ) : TGIS_MarkerStyle ;
  begin
    if       _style = 'esriSMSCircle'         then
      Result := TGIS_MarkerStyle.Circle
    else if  _style = 'esriSMSCross'          then
      Result := TGIS_MarkerStyle.Cross
    else if  _style = 'esriSMSDiamond'        then
      Result := TGIS_MarkerStyle.Box
    else if  _style = 'esriSMSSquare'         then
      Result := TGIS_MarkerStyle.Box
    else if  _style = 'esriSMSX'              then
      Result := TGIS_MarkerStyle.DiagCross
    else if  _style = 'esriSMSTriangle'       then
      Result := TGIS_MarkerStyle.TriangleUp
    else
      Result := TGIS_MarkerStyle.Box
  end ;

  class function TGIS_DrawingInfo.convertLabelPlace(
    const _place : String
  ) : TGIS_LabelPositions ;
  begin
    Result := GisGetEmptyLabelPosition ;
    if ( _place ='esriServerPointLabelPlacementAboveCenter' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.UpCenter )
    else if ( _place ='esriServerPointLabelPlacementBelowCenter' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.DownCenter )
    else if ( _place ='esriServerPointLabelPlacementCenterCenter' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.MiddleCenter )
    else if ( _place ='esriServerPointLabelPlacementAboveLeft' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.UpLeft )
    else if ( _place ='esriServerPointLabelPlacementBelowLeft' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.DownLeft )
    else if ( _place ='esriServerPointLabelPlacementCenterLeft' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.MiddleLeft )
    else if ( _place ='esriServerPointLabelPlacementAboveRight' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.MiddleRight )
    else if ( _place ='esriServerPointLabelPlacementBelowRight' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.DownRight )
    else if ( _place ='esriServerPointLabelPlacementCenterRight' ) then
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.MiddleRight )
    else
      Result := GisAddLabelPosition( Result, TGIS_SymbolPosition.UpCenter ) ;
  end ;

  class function TGIS_DrawingInfo.convertLabelAlign(
    const _align : String
  ) : TGIS_LabelAlignment ;
  begin
    if      ( _align = 'esriServerLinePlacementAboveAfter' ) or
            ( _align = 'esriServerLinePlacementAboveStart' ) or
            ( _align = 'esriServerLinePlacementAboveAlong' ) then
      Result := TGIS_LabelAlignment.Follow
    else if ( _align = 'esriServerLinePlacementBelowAfter' ) or
            ( _align = 'esriServerLinePlacementBelowStart' ) or
            ( _align = 'esriServerLinePlacementBelowAlong' ) then
      Result := TGIS_LabelAlignment.Follow
    else if ( _align = 'esriServerLinePlacementCenterAfter' ) or
            ( _align = 'esriServerLinePlacementCenterStart' ) or
            ( _align = 'esriServerLinePlacementCenterAlong' ) then
      Result := TGIS_LabelAlignment.Follow
    else if ( _align = 'esriServerPolygonPlacementAlwaysHorizontal' ) or
            ( _align = 'justify'                                    ) then
      Result := TGIS_LabelAlignment.Single
    else if ( _align = 'center' ) then
      Result := TGIS_LabelAlignment.Center
    else if ( _align = 'left' ) then
      Result := TGIS_LabelAlignment.LeftJustify
    else if ( _align = 'right' ) then
      Result := TGIS_LabelAlignment.RightJustify
  end ;

  class function TGIS_DrawingInfo.convertBase64(
    const _data : String 
  ) : TBytes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        Result := Convert.FromBase64String( _data ) ;
      {$ELSE}
        Result := Convert.Base64StringToByteArray( _data ) ;
      {$ENDIF}
    {$ELSE}
      {$IFDEF LEVEL_XE7_RTL}
        Result := TNetEncoding.Base64.DecodeStringToBytes( _data ) ;
      {$ELSE}
        {$MESSAGE WARN 'VERIY XE6'}
      {$ENDIF}
    {$ENDIF}
  end ;

  class procedure TGIS_DrawingInfo.parseFill(
    const _json : TGIS_JSONObject ;
    const _area : TGIS_ParamsArea
  ) ;
  var
    width   : Double ;
    outline : TGIS_JSONObject ;
  begin
    _area.Pattern := convertFillStyle( _json.GetValue<String>( 'style' ) ) ;
    _area.Color   := convertColor( _json.GetValue<TGIS_JSONArray>( 'color' ) ) ;

    outline := _json.FindObject( 'outline' ) ;
    if assigned( outline ) then begin
      _area.OutlineStyle := convertLineStyle( outline.GetValue<String>( 'style' ) ) ;
      _area.OutlineColor := convertColor( outline.GetValue<TGIS_JSONArray>( 'color' ) ) ;
      width := outline.GetValue<Double>( 'width' ) ;
      _area.OutlineWidthAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, width] ) ;
    end ;
    _area.ShowLegend := True ;
  end ;

  class procedure TGIS_DrawingInfo.parseLine(
    const _json : TGIS_JSONObject ;
    const _line : TGIS_ParamsLine
  ) ;
  var
    width : Double ;
  begin
    _line.Style := convertLineStyle( _json.GetValue<String>( 'style' ) ) ;
    _line.Color := convertColor( _json.GetValue<TGIS_JSONArray>( 'color' ) ) ;
    width := _json.GetValue<Double>( 'width' ) ;
    _line.WidthAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, width] ) ;
    _line.ShowLegend := True ;
  end ;

  class procedure TGIS_DrawingInfo.parseMarker(
    const _json   : TGIS_JSONObject ;
    const _marker : TGIS_ParamsMarker
  ) ;
  var
    size    : Double ;
    off     : Double ;
    width   : Double ;
    outline : TGIS_JSONObject ;
  begin
    size := _json.GetValue<Double>( 'size' ) ;
    _marker.SizeAsText    := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, size] ) ;
    _marker.Color         := convertColor( _json.GetValue<TGIS_JSONArray>( 'color' ) ) ;
    _marker.SymbolRotate  := DegToRad( -_json.GetValue<Double>( 'angle' ) ) ;
    _marker.Style         := convertMarkerStyle( _json.GetValue<String>( 'style' ) ) ;
    off := _json.GetValue<Double>( 'xoffset' ) ;
    _marker.OffsetXAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, off] ) ;
    off := _json.GetValue<Double>( 'yoffset' ) ;
    _marker.OffsetYAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, off] ) ;

    outline := _json.FindObject( 'outline' ) ;
    if assigned( outline ) then begin
      _marker.OutlineStyle := convertLineStyle( outline.GetValue<String>( 'style' ) ) ;
      _marker.OutlineColor := convertColor( outline.GetValue<TGIS_JSONArray>( 'color' ) ) ;
      width := outline.GetValue<Double>( 'width' ) ;
      _marker.OutlineWidthAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, width] ) ;
    end ;
    _marker.ShowLegend := True ;
  end ;

  class procedure TGIS_DrawingInfo.parsePictureFill(
    const _json : TGIS_JSONObject ;
    const _area : TGIS_ParamsArea
  ) ;
  var
    width  : Double ;
    height : Double ;
    off    : Double ;
    data   : String ;
    ctype  : String ;
    strm   : TMemoryStream ;
    bmp    : TGIS_Bitmap ;
    outline: TGIS_JSONObject ;
    buf    : TBytes ;
  begin
    width  := _json.GetValue<Double>( 'width' ) ;
    height := _json.GetValue<Double>( 'height' ) ;
    off    := _json.GetValue<Double>( 'xoffset' ) ;
    _area.OffsetXAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, off] ) ;
    off := _json.GetValue<Double>( 'yoffset' ) ;
    _area.OffsetYAsText    := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, off] ) ;
    _area.SymbolRotate     := DegToRad( -_json.GetValue<Double>( 'angle' ) ) ;
    _area.SymbolSizeAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, Max(width,height)] ) ;
    data  := _json.GetValue<String>( 'imageData' ) ;
    ctype := _json.GetValue<String>( 'contentType' ) ;
    strm  := TMemoryStream.Create ;
    try
      bmp := TGIS_Bitmap.Create ;
      buf := convertBase64( data ) ;
      strm.Write( buf, length( buf ) ) ;
      strm.Position := 0 ;
      bmp.LoadFromStream( strm ) ;
      _area.Bitmap := bmp ;
    finally
      FreeObject( strm ) ;
      FreeObject( bmp ) ;
    end ;

    outline := _json.FindObject( 'outline' ) ;
    if assigned( outline ) then begin
      _area.OutlineStyle := convertLineStyle( outline.GetValue<String>( 'style' ) ) ;
      _area.OutlineColor := convertColor( outline.GetValue<TGIS_JSONArray>( 'color' ) ) ;
      width := outline.GetValue<Double>( 'width' ) ;
      _area.OutlineWidthAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, width] ) ;
    end ;
    _area.ShowLegend := True ;
  end ;

  class procedure TGIS_DrawingInfo.parsePictureMarker(
    const _json   : TGIS_JSONObject ;
    const _marker : TGIS_ParamsMarker
  ) ;
  var
    width  : Double ;
    height : Double ;
    off    : Double ;
    data   : String ;
    ctype  : String ;
    url    : String ;
    strm   : TMemoryStream ;
    buf    : TBytes ;
  begin
    width  := _json.GetValue<Double>( 'width' ) ;
    height := _json.GetValue<Double>( 'height' ) ;
    off    := _json.GetValue<Double>( 'xoffset' ) ;
    _marker.OffsetXAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, off] ) ;
    off    := _json.GetValue<Double>( 'yoffset' ) ;
    _marker.OffsetYAsText := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, off] ) ;
    _marker.SymbolRotate  := DegToRad( -_json.GetValue<Double>( 'angle' ) ) ;
    _marker.SizeAsText    := Format( '%s:%fpt', [GIS_PARAMTXT_TYPE_SIZE, Max(width,height)] ) ;

    data  := _json.GetValue<String>( 'imageData' ) ;
    ctype := _json.GetValue<String>( 'contentType' ) ;
    url   := _json.GetValue<String>( 'url' ) ;
    if IsStringEmpty( url ) then
      url := _json.GetHashCode.ToString ;

    strm := TMemoryStream.Create ;
    try
      buf := convertBase64( data ) ;
      strm.Write( buf, length( buf ) ) ;
      strm.Position := 0 ;

      _marker.Symbol := SymbolList.Prepare(url+'.png', strm ) ;
    finally
      FreeObject( strm ) ;
    end ;
    _marker.ShowLegend := True ;
  end ;

  class procedure TGIS_DrawingInfo.convertSymbol(
    const _json   : TGIS_JSONObject ;
    const _params : TGIS_ParamsSectionVector
  ) ;
  var
    stype : String ;
  begin
    if not assigned( _json ) then exit ;
    stype := _json.GetValue<String>( 'type' ) ;
    if stype = 'esriSMS' then
      parseMarker( _json, _params.Marker )
    else if stype = 'esriSLS' then
      parseLine( _json, _params.Line )
    else if stype = 'esriSFS' then
      parseFill( _json, _params.Area )
    else if stype = 'esriPFS' then
      parsePictureFill( _json, _params.Area )
    else if stype = 'esriPMS' then
      parsePictureMarker( _json, _params.Marker )
    else if stype = 'esriTS' then
      parseMarker( _json, _params.Marker ) ;
  end ;

  class procedure TGIS_DrawingInfo.convertRenderer(
    const _json       : TGIS_JSONObject ;
    const _paramsList : TGIS_ParamsList
  ) ;
  var
    rtype       : String ;
    field       : String ;
    value       : String ;
    categories  : TGIS_JSONArray ;
    category    : TGIS_JSONObject ;
    unique_vals : TGIS_JSONObject ;
    i           : Integer ;
    params      : TGIS_ParamsSectionVector ;
  begin
    if not assigned( _json ) then exit ;

    rtype := _json.GetValue<String>( 'type' ) ;
    if rtype = 'simple' then begin
      params := _paramsList.SelectedObj as TGIS_ParamsSectionVector ;
      convertSymbol( _json.FindObject( 'symbol' ), params ) ;
    end
    else if rtype = 'uniqueValue' then begin
      category := _json.FindObject( 'defaultSymbol' ) ;
      if assigned( category ) then begin
        params := _paramsList.SelectedObj as TGIS_ParamsSectionVector ;
        params.Query := '' ;
        params.Legend := 'Default' ;
        convertSymbol( category, params ) ;
      end ;

      field       := _json.GetValue<String>( 'field1' ) ;
      unique_vals := _json.FindObject( 'uniqueValueInfos' ) ;
      if not assigned( unique_vals ) then exit ;

      categories := unique_vals.AsArray ;
      for i := 0 to categories.Count-1 do begin
        category := categories[i] ;
        value    := category.GetValue<String>( 'value' ) ;
        if i > 0 then
          _paramsList.Add ;
        params := _paramsList.SelectedObj as TGIS_ParamsSectionVector ;
        params.Query  := Format( '%s=%s', [ field, QuotedStr( value ) ] ) ;
        params.Legend := category.GetValue<String>( 'label' ) ;
        convertSymbol( category.FindObject( 'symbol' ), params ) ;
      end ;

    end
    else if rtype = 'classBreaks' then begin
      // TODO
    end
    else if rtype = 'heatmap' then begin
      // TODO
    end
    else if rtype = 'vectorField' then begin
      // TODO
    end
  end ;

  class procedure TGIS_DrawingInfo.convertLabeling(
    const _json       : TGIS_JSONObject ;
    const _paramsList : TGIS_ParamsList
  ) ;
  var
    i           : Integer ;
    str         : String ;
    ldata       : TGIS_JSONArray ;
    lbl         : TGIS_JSONObject ;
    symbol      : TGIS_JSONObject ;
    font        : TGIS_JSONObject ;
    params      : TGIS_ParamsSectionVector ;
    param_label : TGIS_ParamsLabel ;
  begin
    if not assigned( _json ) then exit ;
    if not _json.IsArray then exit ;

    ldata := _json.AsArray ;
    for i := 0 to ldata.Count - 1 do begin
      lbl := ldata[i] ;
      if i > 0 then
        _paramsList.Add ;
      params := _paramsList.SelectedObj as TGIS_ParamsSectionVector ;
      params.Query  := lbl.GetValue<String>( 'where' ) ;
      params.Legend := params.Query ;
      param_label   := params.Labels ;
      param_label.ShowLegend := False ;

      str := lbl.GetValue<String>( 'labelExpression' ) ;
      str := StringReplaceAll( str, 'CONCAT', '' ) ;
      str := StringReplaceAll( str, 'NEWLINE', '<BR>' ) ;
      str := StringReplaceAll( str, '"', '' ) ;
      str := StringReplaceAll( str, '[', '{' ) ;
      str := StringReplaceAll( str, ']', '}' ) ;
      param_label.Value     := str ;
      param_label.Position  := convertLabelPlace(
                                lbl.GetValue<String>( 'labelPlacement' )
                               ) ;
      param_label.Alignment := convertLabelAlign(
                                lbl.GetValue<String>( 'labelPlacement' )
                               ) ;
      symbol := lbl.FindObject( 'symbol' ) ;
      if assigned( symbol ) then begin
        param_label.FontColor := convertColor( symbol.GetValue<TGIS_JSONArray>( 'color' ) ) ;
        if symbol.GetValue<Double>('haloSize') <> 0 then
          param_label.Color   := convertColor( symbol.GetValue<TGIS_JSONArray>( 'haloColor' ) ) ;
        param_label.Rotate    := DegToRad( symbol.GetValue<Double>( 'angle' ) ) ;
        param_label.Alignment := convertLabelAlign(
                                  symbol.GetValue<String>( 'horizontalAlignment' )
                                 ) ;
        font := symbol.FindObject( 'font' ) ;
        if assigned( font ) then begin
          param_label.FontName := font.GetValue<String>( 'family' ) ;
          param_label.FontSizeAsText := Format(
            'SIZE:%dpt', [font.GetValue<Integer>( 'size' )]
          );
          if font.GetValue<String>( 'style' ) = 'italic' then
            param_label.FontStyle := GisAddFontStyle(
              param_label.FontStyle, TGIS_FontStyle.Italic
            ) ;
          if font.GetValue<String>( 'weight' ) = 'bold' then
            param_label.FontStyle := GisAddFontStyle(
              param_label.FontStyle, TGIS_FontStyle.Bold
            ) ;
          if font.GetValue<String>( 'decoration' ) = 'underline' then
            param_label.FontStyle := GisAddFontStyle(
              param_label.FontStyle, TGIS_FontStyle.Underline
            )
          else if font.GetValue<String>( 'decoration' ) = 'line-through' then
            param_label.FontStyle := GisAddFontStyle(
              param_label.FontStyle, TGIS_FontStyle.StrikeOut
            ) ;
        end ;
        break ; // TODO - don't know what to do with many label sections
      end ;
    end ;
  end ;

  class function TGIS_DrawingInfo.GetDrawingInfo(
    const _json : TGIS_JSONObject
  ) : TGIS_ParamsList ;
  begin
    if not assigned( _json ) then exit ;
    Result := TGIS_ParamsList.Create ;
    Result.SetUp( TGIS_ParamsSectionVector.Create ) ;

    convertLabeling( _json.FindObject( 'labelingInfo' ), Result ) ;
    convertRenderer( _json.FindObject( 'renderer' ), Result ) ;
  end ;

//==============================================================================
// TGIS_ArcGISFeatureServer
//==============================================================================

  constructor TGIS_ArcGISFeatureServer.Create ;
  begin
    inherited ;

    FLayers := TGIS_ArcGISFeatureServerLayerList.Create ;
  end ;

  procedure TGIS_ArcGISFeatureServer.doDestroy ;
  {$IFNDEF OXYGENE}
    var
      itm : TGIS_ArcGISFeatureServerLayer ;
  {$ENDIF}
  begin
    {$IFNDEF NEXTGEN}
    for itm in FLayers do
      FreeObjectNotNil( itm ) ;
    {$ENDIF}
    FreeObject( FLayers ) ;

    inherited ;
  end ;

  procedure TGIS_ArcGISFeatureServer.connectToService(
    const _url : String
  ) ;
  var
    rest_util    : TGIS_RestServiceUtils ;
    service_info : TGIS_JSONObject ;
    layer        : TGIS_JSONObject ;
    id           : TGIS_JSONObject ;
    ojson        : TGIS_JSONObject ;
    ofld         : TGIS_JSONObject ;
    i, j         : Integer ;
    layers_id    : TStringList ;
    fea          : TGIS_ArcGISFeatureServerLayer ;
    fields_list  : TStringList ;
    fields       : String ;
  begin
    rest_util := TGIS_RestServiceUtils.Create ;
    try
      layers_id := TStringList.Create ;
      try
        service_info := rest_util.GetServiceInfo( _url ) ;
        try
          if not assigned( service_info ) then exit ;

          ojson := JSONObjectFind( 'layers', service_info ) ;
          if assigned( ojson ) then begin
            if JSONObjectGetType( ojson ) = TGIS_JSONType.Array then begin
              for i := 0 to ojson.AsArray.Count - 1 do begin
                if assigned( TGIS_JSONObject( ojson.AsArray[ i ] ) ) then begin
                  layer := TGIS_JSONObject( ojson.AsArray[ i ] ) ;
                  id := JSONObjectFind( 'id', layer ) ;
                  if assigned( id ) then
                    layers_id.Add( id.AsString ) ;
                end ;
              end ;
            end ;
          end ;
        finally
          FreeObject( service_info ) ;
        end ;

        for i := 0 to layers_id.Count-1 do begin
          layer := rest_util.GetLayerInfo( _url + '/' + layers_id[i] ) ;
          if not assigned( layer ) then continue ;
          try
            fea := TGIS_ArcGISFeatureServerLayer.Create ;
            fea.Id           := VarToInt32( JSONObjectGetValue( layer,  'id' ) ) ;
            fea.Name         := VarToString( JSONObjectGetValue( layer, 'name' ) ) ;
            fea.Description  := VarToString( JSONObjectGetValue( layer, 'description' ) ) ;
            fea.GeometryType := VarToString( JSONObjectGetValue( layer, 'geometryType' ) ) ;
            {$IFDEF OXYGENE}
              fea.Extent := new TGIS_Extent() ;
            {$ENDIF}
            ojson := JSONObjectFind( 'extent', layer ) ;
            if assigned( ojson ) then begin
              try
                fea.Extent.XMin := VarToDouble( JSONObjectGetValue( ojson, 'xmin' ) ) ;
                fea.Extent.YMin := VarToDouble( JSONObjectGetValue( ojson, 'ymin' ) ) ;
                fea.Extent.XMax := VarToDouble( JSONObjectGetValue( ojson, 'xmax' ) ) ;
                fea.Extent.YMax := VarToDouble( JSONObjectGetValue( ojson, 'ymax' ) ) ;
              except
                // catch NAN and other invalid data
                fea.Extent := GisNoWorld ;
              end ;

              ojson := JSONObjectFind( 'spatialReference', ojson ) ;
              if assigned( ojson ) then
                fea.Wkid := VarToString( JSONObjectGetValue( ojson, 'wkid' ) ) ;
            end ;

            ojson := JSONObjectFind( 'drawingInfo', layer ) ;
            if assigned( ojson ) then begin
              // styling
              fea.DrawingInfo := TGIS_DrawingInfo.GetDrawingInfo( ojson ) ;
            end ;

            fields := '' ;
            fields_list := TStringList.Create ;
            try
              ojson := JSONObjectFind( 'fields', layer ) ;
              if assigned( ojson ) then begin
                for j := 0 to ojson.AsArray.Count-1 do begin
                  ofld := TGIS_JSONObject( ojson.AsArray[j] ) ;
                  fields_list.Add( VarToString( JSONObjectGetValue( ofld, 'name' ) ) ) ;
                end ;

              end ;
            finally
              fields_list.Delimiter := ',' ;
              fields := fields_list.DelimitedText ;
              FreeObject( fields_list ) ;
            end ;

            fea.Path := rest_util.GetObjectsUrl( _url + '/' + layers_id[i], '',
                                                 fea.Wkid, True, fields, False,
                                                 False, fea.Extent ) ;
            FLayers.Add( fea ) ;
          finally
            FreeObject( layer ) ;
          end ;
        end ;
      finally
        FreeObject( layers_id ) ;
      end ;
    finally
      FreeObject( rest_util ) ;
    end ;
  end ;

  procedure TGIS_ArcGISFeatureServer.Open(
    const _url : String
  ) ;
  begin
    FUrl := _url ;

    connectToService( FUrl ) ;
  end ;

{==================================== END =====================================}

end.
