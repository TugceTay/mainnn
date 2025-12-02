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
  Encapsulation of a JSON layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerJSON ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerJSON"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.IO,
    System.Net,
    System.Security,
    System.Runtime.InteropServices,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Variants,
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoStreams,

    Lider.CG.GIS.GeoLayerVector ;
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

  {#gendoc:hide}
  // Initialization section handler
  GisLayerJSON = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   JSON source type.
  /// </summary>
  {#gendoc:hide}
  TGIS_GeoJSONSourceType = {$IFDEF OXYGENE} public {$ENDIF}
  (
    /// <summary>
    ///   unknown source.
    /// </summary>
    Unknown,
    /// <summary>
    ///   file based source.
    /// </summary>
    &File,
    /// <summary>
    ///   text based source.
    /// </summary>
    Text,
    /// <summary>
    ///   web service source.
    /// </summary>
    Service
  ) ;

  /// <summary>
  ///   JSON protocol type.
  /// </summary>
  {#gendoc:hide}
  TGIS_GeoJSONProtocolType = {$IFDEF OXYGENE} public {$ENDIF}
  (
     /// <summary>
     ///   unknown protocol.
     /// </summary>
     Unknown,
     /// <summary>
     ///   HTTP protocol.
     /// </summary>
     HTTP,
     /// <summary>
     ///   HTTPS protocol.
     /// </summary>
     HTTPS,
     /// <summary>
     ///   FTP protocol.
     /// </summary>
     FTP
  ) ;

  /// <summary>
  ///   GeoJSON object type.
  /// </summary>
  {#gendoc:hide}
  TGIS_GeoJSONObjectType = {$IFDEF OXYGENE} public {$ENDIF}
  (
     /// <summary>
     ///   unknown type.
     /// </summary>
     Unknown,
     /// <summary>
     ///   Point type.
     /// </summary>
     Point,
     /// <summary>
     ///   LineString type.
     /// </summary>
     LineString,
     /// <summary>
     ///   Polygon type.
     /// </summary>
     Polygon,
     /// <summary>
     ///   MultiPoint type.
     /// </summary>
     MultiPoint,
     /// <summary>
     ///   MultiLineString type.
     /// </summary>
     MultiLineString,
     /// <summary>
     ///   MultiPolygon type.
     /// </summary>
     MultiPolygon,
     /// <summary>
     ///   Geometry Collection type.
     /// </summary>
     GeometryCollection,
     /// <summary>
     ///   Feature type.
     /// </summary>
     Feature,
     /// <summary>
     ///   Feature Collection type.
     /// </summary>
     FeatureCollection,
     /// <summary>
     ///   Topology type.
     /// </summary>
     Topology,
     /// <summary>
     ///   CityJSON type.
     /// </summary>
     CityJSON,
     /// <summary>
     ///   MultiSurface type.
     /// </summary>
     MultiSurface,
     /// <summary>
     ///   CompositeSurface type.
     /// </summary>
     CompositeSurface,
     /// <summary>
     ///   Solid type.
     /// </summary>
     Solid,
     /// <summary>
     ///   MultiSolid type.
     /// </summary>
     MultiSolid,
     /// <summary>
     ///   CompositeSolid type.
     /// </summary>
     CompositeSolid
  ) ;

  /// <summary>
  ///   Esri JSON object type.
  /// </summary>
  {#gendoc:hide}
  TGIS_EsriJSONObjectType = {$IFDEF OXYGENE} public {$ENDIF}
  (
     /// <summary>
     ///   Unknown type.
     /// </summary>
     Unknown,
     /// <summary>
     ///   Point type.
     /// </summary>
     Point,
     /// <summary>
     ///   MultiPoint type.
     /// </summary>
     MultiPoint,
     /// <summary>
     ///   Polyline type.
     /// </summary>
     Polyline,
     /// <summary>
     ///   Polygon type.
     /// </summary>
     Polygon
  ) ;

  {#gendoc:hide}
  TGIS_TopoJSONTransform = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Scale matrix.
    /// </summary>
    scale     : array [0..2] of Double ;

    /// <summary>
    ///   Translate matrix.
    /// </summary>
    translate : array [0..2] of Double ;
  end ;


  /// <summary>
  ///   Layer which can read a JSON text file.
  /// </summary>
  TGIS_LayerJSON = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

      /// <summary>
      ///   Current shape.
      /// </summary>
      currShape : TGIS_Shape ;

      /// <summary>
      ///   Esri layer shape type.
      /// </summary>
      esriType  : TGIS_EsriJSONObjectType ;

      /// <summary>
      ///   GeoJson layer shape type.
      /// </summary>
      geoJsonType : TGIS_GeoJSONObjectType ;

      /// <summary>
      ///   Transform matrix.
      /// </summary>
      transform : TGIS_TopoJSONTransform ;

      /// <summary>
      ///   True if need to transform.
      /// </summary>
      hasTransform : Boolean ;

      /// <summary>
      ///   Array of arcs coordinates.
      /// </summary>
      arcs : TObject ;

    private

      /// <summary>
      ///   Get source type.
      /// </summary>
      /// <param name="_path">
      ///   path to a json data
      /// </param>
      function geoJSONGetSourceType          ( const _path : String
                                             ) : TGIS_GeoJSONSourceType ;

      /// <summary>
      ///   Get protocol type.
      /// </summary>
      /// <param name="_path">
      ///   path to a json data
      /// </param>
      function geoJSONGetProtocolType        ( const _path : String
                                             ) : TGIS_GeoJSONProtocolType ;

      /// <summary>
      ///   Get geojson object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      function geoJSONGetType                ( const _obj : TObject
                                             ) : TGIS_GeoJSONObjectType ;

      /// <summary>
      ///   Get esri json object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      function esriJSONGetType               ( const _obj : TObject
                                             ) : TGIS_EsriJSONObjectType ;

      /// <summary>
      ///   Build shapes from json object.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure buildShapes                  ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read json feature collection spatial reference.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure readSpatialReference         ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson geometry object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadGeometry          ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson feature object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadFeature           ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Get geojson object attributes.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadAttributes        ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Get json object property type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      function  geoJSONPropertyToFieldType   ( const _obj : TObject
                                             ) : TGIS_FieldType ;

      /// <summary>
      ///   Read geojson feature collection object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadFeatureCollection ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson point object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadPoint             ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson multipoint object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadMultiPoint        ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson linestring object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadLineString        ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson polygon object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadPolygon           ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson multilinestring object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadMultiLineString   ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson multipolygon object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadMultiPolygon      ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson geometry collection object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure geoJSONReadGeometryCollection( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson raw point object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      /// <param name="_shp">
      ///   shape object handle
      /// </param>
      procedure geoJSONReadRawPoint          ( const _obj : TObject ;
                                               const _shp : TGIS_Shape
                                             ) ;

      /// <summary>
      ///   Read esri json feature collection object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadFeatureCollection( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json feature object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadFeature          ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json geometry object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadGeometry         ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json fields object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadFields           ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json point object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadPoint            ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json polyline object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadPolyline         ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json polygon object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadPolygon          ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json multipoint object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadMultiPoint       ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Get esri json object attributes.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure esriJSONReadAttributes       ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read esri json raw point object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      /// <param name="_shp">
      ///   shape object handle
      /// </param>
      procedure esriJSONReadRawPoint         ( const _obj : TObject ;
                                               const _shp : TGIS_Shape
                                             ) ;

      /// <summary>
      ///   Read geojson topology object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure topoJSONReadTopology         ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read geojson raw point object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      /// <param name="_shp">
      ///   shape object handle
      /// </param>
      procedure topoJSONReadArcPoints        ( const _obj : TObject ;
                                               const _shp : TGIS_Shape
                                             ) ;

      /// <summary>
      ///   Read CityJSON object type.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONRead                 ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Build shapes from json object.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      /// <param name="_id">
      ///   json object id
      /// </param>
      procedure buildCityJSON                ( const _obj : TObject ;
                                               const _id  : String
                                             ) ;

      /// <summary>
      ///   Read cityjson geometry object.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONReadGeometry         ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read cityjson MultiSurface geometry.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONReadMultiSurface     ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read cityjson Solid geometry.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONReadSolid            ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read cityjson MultiSolid geometry.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONReadMultiSolid       ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read cityjson MultiPoint geometry.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONReadMultiPoint       ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read cityjson MultiLine geometry.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      procedure cityJSONReadMultiLine        ( const _obj : TObject
                                             ) ;

      /// <summary>
      ///   Read cityjson coordinates.
      /// </summary>
      /// <param name="_obj">
      ///   json object
      /// </param>
      /// <param name="_shp">
      ///   shape object handle
      /// </param>
      procedure cityJSONReadPoints           ( const _obj : TObject ;
                                               const _shp : TGIS_Shape
                                             ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Load data from file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <returns>
      ///   json text
      /// </returns>
      function  loadFromFile                 ( const _path : String
                                             ) : String ;

      /// <summary>
      ///   Load data from service.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <returns>
      ///   json text
      /// </returns>
      function  loadFromService              ( const _path : String
                                             ) : String ;

      /// <summary>
      ///   Load data from stream.
      /// </summary>
      /// <param name="_stm">
      ///   data stream
      /// </param>
      /// <returns>
      ///   json text
      /// </returns>
      function  loadFromStream               ( const _stm   : TStream
                                             ) : String ;

      /// <summary>
      ///   Fetch data from the url
      /// </summary>
      /// <param name="_url">
      ///   Url address from which data should be fetched
      /// </param>
      /// <returns>
      ///   response info
      /// </returns>
      function  fetchHttp                    ( const _url   : String
                                             ) : TGIS_HttpResponse ;

      /// <summary>
      ///   Write shape geometry as json.
      /// </summary>
      /// <param name="_file">
      ///   file handle
      /// </param>
      /// <param name="_shp">
      ///   shape handle
      /// </param>
      procedure writeShape                   ( const _file : TGIS_Stream ;
                                               const _shp  : TGIS_Shape
                                             ) ;
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure setUp      ; override;
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create ; override;
      // new layer builder

         /// <inheritdoc/>
         procedure Build      ( const _path      : String           ;
                                const _extent    : TGIS_Extent      ;
                                const _type      : TGIS_ShapeType   ;
                                const _dim       : TGIS_DimensionType
                              ) ; override;

        /// <inheritdoc/>
        procedure ImportLayerEx( const _layer       : TGIS_LayerVector  ;
                                 const _extent      : TGIS_Extent       ;
                                 const _type        : TGIS_ShapeType    ;
                                 const _scope       : String            ;
                                 const _shape       : TGIS_Shape        ;
                                 const _de9im       : String            ;
                                 const _truncated   : Boolean
                               ) ; override;

         /// <inheritdoc/>
         procedure SaveData   ; override;

         /// <inheritdoc/>
         function  PreRecognize       ( const _path     : String ;
                                          var _new_path : String
                                      ) : Boolean ; override;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoViewer,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoFileJSON,
    Lider.CG.GIS.GeoCsSystems ;
{$ENDIF}

//=============================================================================
// TGIS_LayerJSON
//=============================================================================

  constructor TGIS_LayerJSON.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;
    hasTransform := False ;
  end ;

  procedure TGIS_LayerJSON.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_LayerJSON.geoJSONGetProtocolType(
    const _path : String
   ) : TGIS_GeoJSONProtocolType ;
  begin
    Result := TGIS_GeoJSONProtocolType.Unknown ;

    if Pos( 'http:', _path ) = StringFirst then
      Result := TGIS_GeoJSONProtocolType.HTTP
    else if Pos( 'https:', _path ) = StringFirst then
      Result := TGIS_GeoJSONProtocolType.HTTPS
    else if Pos( 'ftp:', _path ) = StringFirst then
      Result := TGIS_GeoJSONProtocolType.FTP
  end ;

  function TGIS_LayerJSON.geoJSONGetSourceType(
    const _path : String
   ) : TGIS_GeoJSONSourceType ;
  begin
    Result := TGIS_GeoJSONSourceType.Unknown ;

    if geoJSONGetProtocolType( _path ) <> TGIS_GeoJSONProtocolType.Unknown then
      Result := TGIS_GeoJSONSourceType.Service
    else if Pos( '{', _path ) >= StringFirst then
      Result := TGIS_GeoJSONSourceType.Text
    else if ( UpperCase( ExtractFileExt( _path ) ) = '.GEOJSON' ) or
            ( UpperCase( ExtractFileExt( _path ) ) = '.JSON'    ) then
      Result := TGIS_GeoJSONSourceType.File
  end ;

  function TGIS_LayerJSON.geoJSONGetType(
    const _obj : TObject
   ) : TGIS_GeoJSONObjectType ;
  var
    itr   : TGIS_JSONIter ;
    tName : String ;
  begin
    Result := TGIS_GeoJSONObjectType.Unknown ;
    try
      // check geojson
      if JSONObjectFindFirst( 'type', TGIS_JSONObject(_obj), itr ) then begin
        tName := itr.val.AsString ;

        if      tName = 'Point' then
          Result := TGIS_GeoJSONObjectType.Point
        else if tName = 'LineString' then
          Result := TGIS_GeoJSONObjectType.LineString
        else if tName = 'Polygon' then
          Result := TGIS_GeoJSONObjectType.Polygon
        else if tName = 'MultiPoint' then
          Result := TGIS_GeoJSONObjectType.MultiPoint
        else if tName = 'MultiLineString' then
          Result := TGIS_GeoJSONObjectType.MultiLineString
        else if tName = 'MultiPolygon' then
          Result := TGIS_GeoJSONObjectType.MultiPolygon
        else if tName = 'GeometryCollection' then
          Result := TGIS_GeoJSONObjectType.GeometryCollection
        else if tName = 'Feature' then
          Result := TGIS_GeoJSONObjectType.Feature
        else if tName = 'FeatureCollection' then
          Result := TGIS_GeoJSONObjectType.FeatureCollection
        else if tName = 'Topology' then
          Result := TGIS_GeoJSONObjectType.Topology
        else if tName = 'CityJSON' then
          Result := TGIS_GeoJSONObjectType.CityJSON
        else if tName = 'MultiSurface' then
          Result := TGIS_GeoJSONObjectType.MultiSurface
        else if tName = 'CompositeSurface' then
          Result := TGIS_GeoJSONObjectType.CompositeSurface
        else if tName = 'Solid' then
          Result := TGIS_GeoJSONObjectType.Solid
        else if tName = 'MultiSolid' then
          Result := TGIS_GeoJSONObjectType.MultiSolid
        else if tName = 'CompositeSolid' then
          Result := TGIS_GeoJSONObjectType.CompositeSolid
        else
          Result := TGIS_GeoJSONObjectType.Unknown ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function TGIS_LayerJSON.esriJSONGetType(
    const _obj : TObject
   ) : TGIS_EsriJSONObjectType ;
  var
    itr   : TGIS_JSONIter ;
    tName : String ;
  begin
    Result := TGIS_EsriJSONObjectType.Unknown ;
    try
      // check geojson
      if JSONObjectFindFirst( 'geometryType', TGIS_JSONObject(_obj), itr ) then
      begin
        tName := itr.val.AsString ;

        if      tName = 'esriGeometryPoint'       then
          Result := TGIS_EsriJSONObjectType.Point
        else if tName = 'esriGeometryPolyline'    then
          Result := TGIS_EsriJSONObjectType.Polyline
        else if tName = 'esriGeometryPolygon'     then
          Result := TGIS_EsriJSONObjectType.Polygon
        else if tName = 'esriGeometryMultiPoint'  then
          Result := TGIS_EsriJSONObjectType.MultiPoint
        else
          Result := TGIS_EsriJSONObjectType.Unknown ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function TGIS_LayerJSON.geoJSONPropertyToFieldType(
    const _obj : TObject
   ) : TGIS_FieldType ;
  begin
    case JSONObjectGetType( TGIS_JSONObject(_obj) ) of
      TGIS_JSONType.Null    : Result := TGIS_FieldType.String  ;
      TGIS_JSONType.Boolean : Result := TGIS_FieldType.Boolean ;
      TGIS_JSONType.Double  : Result := TGIS_FieldType.Float   ;
      TGIS_JSONType.Int     : Result := TGIS_FieldType.Number  ;
      TGIS_JSONType.Object  : Result := TGIS_FieldType.String  ;
      TGIS_JSONType.Array   : Result := TGIS_FieldType.String  ;
      TGIS_JSONType.String  : Result := TGIS_FieldType.String  ;
      else                    Result := TGIS_FieldType.String
    end ;
  end ;

  procedure TGIS_LayerJSON.buildShapes(
    const _obj : TObject
  ) ;
  var
    objType  : TGIS_GeoJSONObjectType ;
  begin
    objType := geoJSONGetType( _obj ) ;

    case objType of
      TGIS_GeoJSONObjectType.Point,
      TGIS_GeoJSONObjectType.LineString,
      TGIS_GeoJSONObjectType.Polygon,
      TGIS_GeoJSONObjectType.MultiPoint,
      TGIS_GeoJSONObjectType.MultiLineString,
      TGIS_GeoJSONObjectType.MultiPolygon,
      TGIS_GeoJSONObjectType.GeometryCollection :
        geoJSONReadGeometry( _obj ) ;
      TGIS_GeoJSONObjectType.Feature            :
        geoJSONReadFeature( _obj ) ;
      TGIS_GeoJSONObjectType.FeatureCollection  :
        geoJSONReadFeatureCollection( _obj ) ;
      TGIS_GeoJSONObjectType.Unknown            :
        esriJSONReadFeatureCollection( _obj ) ;
      TGIS_GeoJSONObjectType.Topology           :
        topoJSONReadTopology( _obj ) ;
      TGIS_GeoJSONObjectType.CityJSON           :
        cityJSONRead( _obj ) ;
    end ;

    case esriType of
      TGIS_EsriJSONObjectType.Point     :
        DefaultShapeType := TGIS_ShapeType.Point ;
      TGIS_EsriJSONObjectType.MultiPoint:
        DefaultShapeType := TGIS_ShapeType.MultiPoint ;
      TGIS_EsriJSONObjectType.Polyline  :
        DefaultShapeType := TGIS_ShapeType.Arc ;
      TGIS_EsriJSONObjectType.Polygon   :
        DefaultShapeType := TGIS_ShapeType.Polygon ;
    end ;

    case geoJsonType of
      TGIS_GeoJSONObjectType.Point              :
        DefaultShapeType := TGIS_ShapeType.Point ;
      TGIS_GeoJSONObjectType.LineString         :
        DefaultShapeType := TGIS_ShapeType.Arc ;
      TGIS_GeoJSONObjectType.Polygon            :
        DefaultShapeType := TGIS_ShapeType.Polygon ;
      TGIS_GeoJSONObjectType.MultiPoint         :
        DefaultShapeType := TGIS_ShapeType.MultiPoint ;
      TGIS_GeoJSONObjectType.MultiLineString    :
        DefaultShapeType := TGIS_ShapeType.Arc;
      TGIS_GeoJSONObjectType.MultiPolygon       :
        DefaultShapeType := TGIS_ShapeType.Polygon ;
      TGIS_GeoJSONObjectType.GeometryCollection :
        DefaultShapeType := TGIS_ShapeType.Complex ;
      TGIS_GeoJSONObjectType.MultiSurface,
      TGIS_GeoJSONObjectType.CompositeSurface,
      TGIS_GeoJSONObjectType.Solid,
      TGIS_GeoJSONObjectType.MultiSolid,
      TGIS_GeoJSONObjectType.CompositeSolid     :
        DefaultShapeType := TGIS_ShapeType.MultiPatch ;
    end ;
  end ;

  procedure TGIS_LayerJSON.buildCityJSON(
    const _obj : TObject ;
    const _id  : String
  ) ;
  var
    objGeom : TGIS_JSONObject ;
    objAttr : TGIS_JSONObject ;
    {$IFDEF DCC}
    g       : TGIS_JSONObject ;
    {$ENDIF}
  begin
    objGeom := JSONObjectFind( 'geometry', TGIS_JSONObject(_obj) ) ;
    if not assigned( objGeom ) then exit ;

    if JSONObjectGetType( objGeom ) = TGIS_JSONType.&Array then begin
      for g in objGeom do begin
        cityJSONReadGeometry( g ) ;
        if assigned( currShape ) then
          currShape.SetField( 'id', _id ) ;
      end ;
    end ;

    objAttr := JSONObjectFind( 'type', TGIS_JSONObject(_obj) ) ;
    if assigned( objAttr ) and assigned( currShape ) then
      currShape.SetField( 'type', objAttr.AsString ) ;

    objAttr := JSONObjectFind( 'attributes', TGIS_JSONObject(_obj) ) ;
    if assigned( objAttr ) then
      geoJSONReadAttributes( objAttr ) ;
  end ;

  procedure TGIS_LayerJSON.cityJSONReadGeometry(
    const _obj : TObject
  ) ;
  var
    objType  : TGIS_GeoJSONObjectType ;
    objBound : TGIS_JSONObject ;
  begin
    currShape := nil ;
    objBound := JSONObjectFind( 'boundaries', TGIS_JSONObject(_obj) ) ;

    if not assigned( objBound ) then exit ;

    objType := geoJSONGetType( _obj ) ;

    case objType of
      TGIS_GeoJSONObjectType.MultiPoint :
        cityJSONReadMultiPoint( objBound ) ;
      TGIS_GeoJSONObjectType.MultiLineString :
        cityJSONReadMultiLine( objBound ) ;
      TGIS_GeoJSONObjectType.MultiSurface,
      TGIS_GeoJSONObjectType.CompositeSurface :
        cityJSONReadMultiSurface( objBound ) ;
      TGIS_GeoJSONObjectType.Solid :
        cityJSONReadSolid( objBound ) ;
      TGIS_GeoJSONObjectType.MultiSolid,
      TGIS_GeoJSONObjectType.CompositeSolid :
        cityJSONReadMultiSolid( objBound ) ;
    end ;

    geoJsonType := objType ;
  end ;

  procedure TGIS_LayerJSON.cityJSONReadMultiSurface(
    const _obj : TObject
  ) ;
  {$IFDEF DCC}
  var
    s, p : TGIS_JSONObject ;
  {$ENDIF}
  begin
    currShape := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ ) ;
    try
      currShape.Lock( TGIS_Lock.Projection ) ;

      for s in TGIS_JSONObject(_obj) do begin
        if JSONObjectGetType( TGIS_JSONObject(s) ) = TGIS_JSONType.Array then begin
          for p in TGIS_JSONObject(s) do begin
            if JSONObjectGetType( TGIS_JSONObject(p) ) = TGIS_JSONType.Array then begin

              currShape.AddPart ;
              cityJSONReadPoints( p, currShape ) ;
            end ;
          end ;
        end ;
      end ;
    finally
      currShape.Unlock ;
    end ;

  end ;

  procedure TGIS_LayerJSON.cityJSONReadSolid(
    const _obj : TObject
  ) ;
  {$IFDEF DCC}
  var
    s, e, p : TGIS_JSONObject ;
  {$ENDIF}
  begin
    currShape := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ ) ;
    try
      currShape.Lock( TGIS_Lock.Projection ) ;

      for s in TGIS_JSONObject(_obj) do begin
        for e in TGIS_JSONObject(s) do begin
          if JSONObjectGetType( TGIS_JSONObject(e) ) = TGIS_JSONType.Array then begin
            for p in TGIS_JSONObject(e) do begin
              if JSONObjectGetType( TGIS_JSONObject(p) ) = TGIS_JSONType.Array then begin

                currShape.AddPart ;
                cityJSONReadPoints( p, currShape ) ;
              end ;
            end ;
          end ;
        end ;
      end ;
    finally
      currShape.Unlock ;
    end ;

  end ;

  procedure TGIS_LayerJSON.cityJSONReadMultiSolid(
    const _obj : TObject
  ) ;
  {$IFDEF DCC}
  var
    m, s, e, p : TGIS_JSONObject ;
  {$ENDIF}
  begin
    currShape := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ ) ;
    try
      currShape.Lock( TGIS_Lock.Projection ) ;

      for m in TGIS_JSONObject(_obj) do begin
        for s in TGIS_JSONObject(m) do begin
          for e in TGIS_JSONObject(s) do begin
            if JSONObjectGetType( TGIS_JSONObject(e) ) = TGIS_JSONType.Array then begin
              for p in TGIS_JSONObject(e) do begin
                if JSONObjectGetType( TGIS_JSONObject(p) ) = TGIS_JSONType.Array then begin

                  currShape.AddPart ;
                  cityJSONReadPoints( p, currShape ) ;
                end ;
              end ;
            end ;
          end ;
        end ;
      end ;
    finally
      currShape.Unlock ;
    end ;

  end ;

  procedure TGIS_LayerJSON.cityJSONReadMultiPoint(
    const _obj : TObject
  ) ;
  begin
    currShape := CreateShape( TGIS_ShapeType.MultiPoint, TGIS_DimensionType.XYZ ) ;
    try
      currShape.Lock( TGIS_Lock.Projection ) ;

      currShape.AddPart ;
      cityJSONReadPoints( _obj, currShape ) ;
    finally
      currShape.Unlock ;
    end ;

  end ;

  procedure TGIS_LayerJSON.cityJSONReadMultiLine(
    const _obj : TObject
  ) ;
  {$IFDEF DCC}
  var
    p : TGIS_JSONObject ;
  {$ENDIF}
  begin
    currShape := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ ) ;
    try
      currShape.Lock( TGIS_Lock.Projection ) ;

      for p in TGIS_JSONObject(_obj) do begin
        if JSONObjectGetType( TGIS_JSONObject(p) ) = TGIS_JSONType.Array then begin

          currShape.AddPart ;
          cityJSONReadPoints( p, currShape ) ;
        end ;
      end ;
    finally
      currShape.Unlock ;
    end ;

  end ;

  procedure TGIS_LayerJSON.cityJSONReadPoints(
    const _obj : TObject ;
    const _shp : TGIS_Shape
  ) ;
  var
    idx, i      : Integer ;
    num_ptg     : Integer ;
    coords      : TGIS_JSONObject ;
    coord       : TGIS_JSONArray ;
    x, y, z, m  : Double ;
  begin
    x := 0 ;
    y := 0 ;
    z := 0 ;
    m := 0 ;
    if JSONObjectGetType( TGIS_JSONObject(_obj) ) = TGIS_JSONType.Array then begin
      for i := TGIS_JSONObject(_obj).AsArray.Count - 1 downto 0 do begin

        idx := TGIS_JSONObject(TGIS_JSONObject(_obj).AsArray[i]).AsInteger ;
        if idx >= TGIS_JSONObject(arcs).AsArray.Count then exit ;

        coords  := TGIS_JSONObject(TGIS_JSONObject(arcs).AsArray[idx]) ;
        coord   := coords.AsArray ;
        num_ptg := coord.Count ;

          if num_ptg > 1 then begin
            x := TGIS_JSONObject( coord[ 0 ] ).AsDouble ;
            y := TGIS_JSONObject( coord[ 1 ] ).AsDouble ;
            z := 0 ;
            m := 0 ;
          end ;

          if num_ptg > 2  then
            z := TGIS_JSONObject( coord[ 2 ] ).AsDouble ;

          if num_ptg > 3  then
            m := TGIS_JSONObject( coord[ 3 ] ).AsDouble ;

          if hasTransform then begin
            x := x * transform.scale[0] + transform.translate[0] ;
            y := y * transform.scale[1] + transform.translate[1] ;
            z := z * transform.scale[2] + transform.translate[2] ;
          end ;

          _shp.AddPoint3D( GisPoint3D( x, y, z, m ) ) ;
        end ;

    end ;
  end ;


  procedure TGIS_LayerJSON.readSpatialReference(
    const _obj : TObject
  ) ;
  var
    objCrs     : TGIS_JSONObject ;
    objSr      : TGIS_JSONObject ;
    objSrsType : TGIS_JSONObject ;
    objSrsProps: TGIS_JSONObject ;
    objURL     : TGIS_JSONObject ;
    objCode    : TGIS_JSONObject ;
    objWkt     : TGIS_JSONObject ;
    objWkid    : TGIS_JSONObject ;
    srsType    : String ;
    srsUrl     : String ;
    srsCode    : Integer ;
  begin
    objCrs := JSONObjectFind( 'crs', TGIS_JSONObject(_obj) ) ;
    objSr  := JSONObjectFind( 'spatialReference', TGIS_JSONObject(_obj) ) ;

    // geojson
    if assigned( objCrs ) and not assigned( objSr ) then begin
      objSrsType := JSONObjectFind( 'type', objCrs ) ;
      if not assigned( objSrsType ) then exit ;

      srsType     := UpperCase( objSrsType.AsString ) ;
      objSrsProps := JSONObjectFind( 'properties', objCrs ) ;
      if not assigned( objSrsProps ) then exit ;

      if srsType = 'NAME' then begin
        objURL := JSONObjectFind( 'name', objSrsProps ) ;
        if assigned( objURL ) then begin
          srsUrl := objURL.AsString ;
          SetCSByWKT( srsUrl ) ;
        end ;
      end
      else if srsType = 'EPSG' then begin
        objCode := JSONObjectFind( 'code', objSrsProps ) ;
        if assigned( objCode ) then begin
          srsCode := objCode.AsInteger ;
          SetCSByEPSG( srsCode ) ;
        end ;
      end
      else if (srsType = 'URL') or (srsType = 'LINK') then begin
        objURL := JSONObjectFind( 'url', objSrsProps ) ;
        if not assigned( objURL ) then
          objURL := JSONObjectFind( 'href', objSrsProps ) ;

        if assigned( objURL ) then begin
          srsUrl := objURL.AsString ;
          SetCSByWKT( srsUrl ) ;
        end ;
      end
      else if srsType = 'OGC' then begin
        objURL := JSONObjectFind( 'urn', objSrsProps ) ;
        if assigned( objURL ) then begin
          srsUrl := objURL.AsString ;
          SetCSByWKT( srsUrl ) ;
        end ;
      end
    end
    else if not assigned( objCrs ) and assigned( objSr ) then begin
      // esri json
      objWkid := JSONObjectFind( 'wkid', objSr ) ;
      if assigned( objWkid ) then
        SetCSByEPSG( objWkid.AsInteger )
      else begin
        objWkt := JSONObjectFind( 'wkid', objSr ) ;
        if assigned( objWkt ) then
          SetCSByWKT( objWkt.AsString )
      end ;
    end
  end ;

  procedure TGIS_LayerJSON.geoJSONReadFeatureCollection(
    const _obj : TObject
  ) ;
  var
    objFea     : TGIS_JSONObject ;
    objFeaColl : TGIS_JSONObject ;
    itr        : TGIS_JSONIter ;
    i          : Integer ;
    abort      : Boolean ;
  begin
    try
      if JSONObjectFindFirst( 'features', TGIS_JSONObject(_obj), itr ) then begin
        objFeaColl := itr.val ;
        if assigned( objFeaColl ) and
          ( JSONObjectGetType( objFeaColl ) = TGIS_JSONType.Array ) then begin

            for i := 0 to objFeaColl.AsArray.Count - 1 do begin
              objFea := TGIS_JSONObject( objFeaColl.AsArray[ i ] ) ;
              geoJSONReadFeature( objFea ) ;
              if i mod 100 = 0 then
                abort := RaiseBusyShake( Self, i, objFeaColl.AsArray.Count ) ;
            end
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadAttributes(
    const _obj : TObject
  ) ;
  var
    itr   : TGIS_JSONIter ;
    fldt  : TGIS_FieldType ;
    fidx  : Integer ;
    fld   : TGIS_FieldInfo ;
  begin
    try
      if JSONObjectFindFirst( TGIS_JSONObject(_obj), itr ) then
        while True do begin
          fldt := geoJSONPropertyToFieldType( itr.val ) ;
          fidx := FindField( itr.key ) ;

          if fidx < 0 then begin
            case fldt of
              TGIS_FieldType.String  : AddFieldInternal( itr.key, TGIS_FieldType.String , 1 , 0 ) ;
              TGIS_FieldType.Number  : AddFieldInternal( itr.key, TGIS_FieldType.Number , 10, 0 ) ;
              TGIS_FieldType.Float   : AddFieldInternal( itr.key, TGIS_FieldType.Float  , 10, 7 ) ;
              TGIS_FieldType.Boolean : AddFieldInternal( itr.key, TGIS_FieldType.Boolean, 1 , 0 ) ;
              else                     AddFieldInternal( itr.key, TGIS_FieldType.String , 1 , 0 ) ;
            end ;
          end
          else begin
            fld := FieldInfo(fidx) ;
            if (fld.FieldType <> fldt) then begin
              if (fld.FieldType = TGIS_FieldType.Number) and (fldt = TGIS_FieldType.Float) then
              begin
                // promote to float
                fld.Decimal    := 7 ;
                fld.NewDecimal := 7 ;
              end ;
            end ;
          end;

          if assigned( itr.val ) and assigned( currShape ) then
            case fldt of
              TGIS_FieldType.String  : currShape.SetField( itr.key, itr.val.AsString  ) ;
              TGIS_FieldType.Number  : currShape.SetField( itr.key, itr.val.AsInteger ) ;
              TGIS_FieldType.Float   : currShape.SetField( itr.key, itr.val.AsDouble  ) ;
              TGIS_FieldType.Boolean : currShape.SetField( itr.key, itr.val.AsString  ) ;
              else                     currShape.SetField( itr.key, itr.val.AsString  ) ;
            end ;

          if not JSONObjectFindNext( itr ) then break ;
        end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadFeature(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
  begin
    try
      if JSONObjectFindFirst( 'geometry', TGIS_JSONObject(_obj), itr ) then
        geoJSONReadGeometry( itr.val ) ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

    try
      if JSONObjectFindFirst( 'properties', TGIS_JSONObject(_obj), itr ) then
        geoJSONReadAttributes( itr.val ) ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

  end ;

  procedure TGIS_LayerJSON.geoJSONReadGeometry(
    const _obj : TObject
  ) ;
  var
    objType : TGIS_GeoJSONObjectType ;
  begin
    objType := geoJSONGetType( _obj ) ;

    case objType of
      TGIS_GeoJSONObjectType.Point               :
        geoJSONReadPoint( _obj ) ;
      TGIS_GeoJSONObjectType.MultiPoint          :
        geoJSONReadMultiPoint( _obj ) ;
      TGIS_GeoJSONObjectType.LineString          :
        geoJSONReadLineString( _obj ) ;
      TGIS_GeoJSONObjectType.Polygon             :
        geoJSONReadPolygon( _obj ) ;
      TGIS_GeoJSONObjectType.MultiLineString     :
        geoJSONReadMultiLineString( _obj ) ;
      TGIS_GeoJSONObjectType.MultiPolygon        :
        geoJSONReadMultiPolygon( _obj ) ;
      TGIS_GeoJSONObjectType.GeometryCollection  :
        geoJSONReadGeometryCollection( _obj ) ;
    end ;
    geoJsonType := objType ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadPoint(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
  begin
    try
      if JSONObjectFindFirst( 'coordinates', TGIS_JSONObject(_obj), itr ) then begin
        currShape := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZM ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          currShape.AddPart ;

          geoJSONReadRawPoint( itr.val, currShape ) ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadMultiPoint(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
    i   : Integer ;
  begin
    try
      if JSONObjectFindFirst( 'coordinates', TGIS_JSONObject(_obj), itr ) then begin
        currShape := CreateShape( TGIS_ShapeType.MultiPoint, TGIS_DimensionType.XYZM ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            currShape.AddPart ;
            for i := 0 to itr.val.AsArray.Count - 1 do
              geoJSONReadRawPoint( itr.val.AsArray[ i ], currShape ) ;
          end ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadLineString(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
    i   : Integer ;
    fnd : Boolean ;
    id  : TGIS_JSONObject ;
  begin
    fnd := False ;
    try
      if JSONObjectFindFirst( 'coordinates', TGIS_JSONObject(_obj), itr ) then begin
        fnd := True ;
        currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZM ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          currShape.AddPart ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              geoJSONReadRawPoint( itr.val.AsArray[ i ], currShape ) ;
            end ;
          end ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

    if not fnd then begin
      try
         if JSONObjectFindFirst( 'arcs', TGIS_JSONObject(_obj), itr ) then begin
          currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZM ) ;
          try
            currShape.Lock( TGIS_Lock.Projection ) ;
            currShape.AddPart ;
            topoJSONReadArcPoints( itr.val, currShape ) ;

            id := JSONObjectFind( 'id', TGIS_JSONObject(_obj) ) ;
            if assigned( id ) then
              currShape.SetField( 'id', id.AsString ) ;
          finally
            currShape.Unlock ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;

    end ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadMultiLineString(
    const _obj : TObject
  ) ;
  var
    itr   : TGIS_JSONIter ;
    i, j  : Integer ;
    fnd   : Boolean ;
    id    : TGIS_JSONObject ;
  begin
    fnd := False ;
    try
      if JSONObjectFindFirst( 'coordinates', TGIS_JSONObject(_obj), itr ) then begin
        fnd := True ;
        currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZM ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              currShape.AddPart ;
              if assigned( TGIS_JSONObject( itr.val.AsArray[ i ] ) ) then
                for j := 0 to TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray.Count - 1 do
                  geoJSONReadRawPoint( TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray[ j ],
                                       currShape
                                      ) ;
            end ;
          end ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

    if not fnd then begin
      try
        if JSONObjectFindFirst( 'arcs', TGIS_JSONObject(_obj), itr ) then begin
          currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZM ) ;
          try
            currShape.Lock( TGIS_Lock.Projection ) ;
            if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                currShape.AddPart ;
                if assigned( TGIS_JSONObject( itr.val.AsArray[ i ] ) ) then
                  topoJSONReadArcPoints( itr.val.AsArray[ i ],
                                         currShape
                                        ) ;
              end ;
            end ;
            id := JSONObjectFind( 'id', TGIS_JSONObject(_obj) ) ;
            if assigned( id ) then
              currShape.SetField( 'id', id.AsString ) ;
          finally
            currShape.Unlock ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;

    end;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadPolygon(
    const _obj : TObject
  ) ;
  var
    itr   : TGIS_JSONIter ;
    i, j  : Integer ;
    fnd   : Boolean ;
    id    : TGIS_JSONObject ;
  begin
    fnd := False ;
    try
      if JSONObjectFindFirst( 'coordinates', TGIS_JSONObject(_obj), itr ) then begin
        fnd := True ;
        currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZM ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              currShape.AddPart ;
              if assigned( TGIS_JSONObject( itr.val.AsArray[ i ] ) ) then
                for j := 0 to TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray.Count - 1 do
                  geoJSONReadRawPoint( TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray[ j ],
                                       currShape
                                      ) ;
            end ;
          end ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

    if not fnd then begin
      try
        if JSONObjectFindFirst( 'arcs', TGIS_JSONObject(_obj), itr ) then begin
          currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZM ) ;
          try
            currShape.Lock( TGIS_Lock.Projection ) ;
            if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                currShape.AddPart ;
                if assigned( TGIS_JSONObject( itr.val.AsArray[ i ] ) ) then
                  topoJSONReadArcPoints( TGIS_JSONObject( itr.val.AsArray[ i ] ),
                                         currShape
                                        ) ;
              end ;
            end ;
            id := JSONObjectFind( 'id', TGIS_JSONObject(_obj) ) ;
            if assigned( id ) then
              currShape.SetField( 'id', id.AsString ) ;
          finally
            currShape.Unlock ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
    end;

  end ;

  procedure TGIS_LayerJSON.geoJSONReadMultiPolygon(
    const _obj : TObject
  ) ;
  var
    itr      : TGIS_JSONIter ;
    i, j, k  : Integer ;
    oPoly    : TGIS_JSONObject ;
    oRing    : TGIS_JSONObject ;
    fnd      : Boolean ;
    id       : TGIS_JSONObject ;
  begin
    fnd := False ;
    try
      if JSONObjectFindFirst( 'coordinates', TGIS_JSONObject(_obj), itr ) then begin
        fnd := True ;
        currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZM ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              oPoly := TGIS_JSONObject( itr.val.AsArray[ i ] ) ;
              if JSONObjectGetType( oPoly ) = TGIS_JSONType.Array then begin

                for j := 0 to oPoly.AsArray.Count - 1 do begin
                  oRing := TGIS_JSONObject( oPoly.AsArray[ j ] );
                  currShape.AddPart ;
                  if JSONObjectGetType( oRing ) = TGIS_JSONType.Array then begin
                    for k := 0 to oRing.AsArray.Count - 1 do
                      geoJSONReadRawPoint( oRing.AsArray[ k ], currShape ) ;
                  end ;
                end ;
              end ;
            end ;
          end ;

        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

    if not fnd then begin
      try
        if JSONObjectFindFirst( 'arcs', TGIS_JSONObject(_obj), itr ) then begin
          currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZM ) ;
          try
            currShape.Lock( TGIS_Lock.Projection ) ;
            if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
              for i := 0 to itr.val.AsArray.Count - 1 do begin
                oPoly := TGIS_JSONObject( itr.val.AsArray[ i ] ) ;
                if JSONObjectGetType( oPoly ) = TGIS_JSONType.Array then begin

                  for j := 0 to oPoly.AsArray.Count - 1 do begin
                    oRing := TGIS_JSONObject( oPoly.AsArray[ j ] );
                    currShape.AddPart ;
                    if JSONObjectGetType( oRing ) = TGIS_JSONType.Array then begin
                      topoJSONReadArcPoints( oRing, currShape ) ;
                    end ;
                  end ;
                end ;
              end ;
            end ;
            id := JSONObjectFind( 'id', TGIS_JSONObject(_obj) ) ;
            if assigned( id ) then
              currShape.SetField( 'id', id.AsString ) ;
          finally
            currShape.Unlock ;
          end ;
        end ;
      finally
        JSONObjectFindClose( itr ) ;
      end ;
    end;

  end ;

  procedure TGIS_LayerJSON.geoJSONReadGeometryCollection(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
    i   : Integer ;
  begin
    try
      if JSONObjectFindFirst( 'geometries', TGIS_JSONObject(_obj), itr ) then begin
        if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
           for i := 0 to itr.val.AsArray.Count - 1 do begin
             geoJSONReadGeometry( itr.val.AsArray[ i ] ) ;
           end ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.geoJSONReadRawPoint(
    const _obj : TObject ;
    const _shp : TGIS_Shape
   ) ;
  var
    num_ptg  : Integer ;
    x,y,z,m : Double ;
  begin
    x := 0 ;
    y := 0 ;
    z := 0 ;
    m := 0 ;
    if JSONObjectGetType( TGIS_JSONObject(_obj) ) = TGIS_JSONType.Array then begin

      num_ptg := TGIS_JSONObject(_obj).AsArray.Count ;

      if num_ptg > 1 then begin
        x := TGIS_JSONObject( TGIS_JSONObject(_obj).AsArray[ 0 ] ).AsDouble ;
        y := TGIS_JSONObject( TGIS_JSONObject(_obj).AsArray[ 1 ] ).AsDouble ;
        z := 0 ;
        m := 0 ;
      end ;

      if num_ptg > 2  then
        z := TGIS_JSONObject( TGIS_JSONObject(_obj).AsArray[ 2 ] ).AsDouble ;

      if num_ptg > 3  then
        m := TGIS_JSONObject( TGIS_JSONObject(_obj).AsArray[ 3 ] ).AsDouble ;

      if hasTransform then begin
        x := x * transform.scale[0] + transform.translate[0] ;
        y := y * transform.scale[1] + transform.translate[1] ;
      end ;

      _shp.AddPoint3D( GisPoint3D( x, y, z, m ) ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadFeatureCollection(
    const _obj : TObject
  ) ;
  var
    objFea     : TGIS_JSONObject ;
    objFeaColl : TGIS_JSONObject ;
    itr        : TGIS_JSONIter ;
    i          : Integer ;
    abort      : Boolean ;
  begin
    esriType := esriJSONGetType( _obj ) ;

    esriJSONReadFields( _obj ) ;

    try
      if JSONObjectFindFirst( 'features', TGIS_JSONObject(_obj), itr ) then begin
        objFeaColl := itr.val ;
        if assigned( objFeaColl ) and
          ( JSONObjectGetType( objFeaColl ) = TGIS_JSONType.Array ) then begin

            for i := 0 to objFeaColl.AsArray.Count - 1 do begin
              objFea := TGIS_JSONObject( objFeaColl.AsArray[ i ] ) ;
              esriJSONReadFeature( objFea ) ;
              if i mod 100 = 0 then
                abort := RaiseBusyShake( Self, i, objFeaColl.AsArray.Count ) ;
            end
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadFeature(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
  begin
    try
      if JSONObjectFindFirst( 'geometry', TGIS_JSONObject(_obj), itr ) then
        esriJSONReadGeometry( itr.val ) ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

    try
      if JSONObjectFindFirst( 'attributes', TGIS_JSONObject(_obj), itr ) then
        esriJSONReadAttributes( itr.val ) ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadGeometry(
    const _obj : TObject
  ) ;
  begin
    case esriType of
      TGIS_EsriJSONObjectType.Point       : esriJSONReadPoint     ( _obj ) ;
      TGIS_EsriJSONObjectType.MultiPoint  : esriJSONReadMultiPoint( _obj ) ;
      TGIS_EsriJSONObjectType.Polyline    : esriJSONReadPolyline  ( _obj ) ;
      TGIS_EsriJSONObjectType.Polygon     : esriJSONReadPolygon   ( _obj ) ;
      TGIS_EsriJSONObjectType.Unknown     : ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadFields(
    const _obj : TObject
  ) ;
  var
    i         : Integer ;
    objFields : TGIS_JSONObject ;
    objField  : TGIS_JSONObject ;
    fldName   : TGIS_JSONObject ;
    fldType   : TGIS_JSONObject ;
    fldLength : TGIS_JSONObject ;
    itr       : TGIS_JSONIter ;
  begin
    objFields := JSONObjectFind( 'fields', TGIS_JSONObject(_obj) ) ;
    if assigned( objFields ) then begin
      for i := 0 to objFields.AsArray.Count-1 do begin
        objField  := TGIS_JSONObject(objFields.AsArray[i]) ;
        if not assigned( objField ) then continue ;

        fldName   := JSONObjectFind( 'name'  , objField ) ;
        fldType   := JSONObjectFind( 'type'  , objField ) ;
        fldLength := JSONObjectFind( 'length', objField ) ;

        if assigned( fldName ) and assigned( fldType ) then begin
          if FindField( fldName.AsString ) < 0 then begin
            if      fldType.AsString = 'esriFieldTypeString'       then
              AddFieldInternal( fldName.AsString, TGIS_FieldType.String, 1, 0 )
            else if fldType.AsString = 'esriFieldTypeOID'          then
              AddFieldInternal( fldName.AsString, TGIS_FieldType.Number, 10, 0 )
            else if fldType.AsString = 'esriFieldTypeSmallInteger' then
              AddFieldInternal( fldName.AsString, TGIS_FieldType.Number, 5, 0 )
            else if fldType.AsString = 'esriFieldTypeInteger'      then
              AddFieldInternal( fldName.AsString, TGIS_FieldType.Number, 10, 0 )
            else if fldType.AsString = 'esriFieldTypeDouble'       then
              AddFieldInternal( fldName.AsString, TGIS_FieldType.Float, 0, 0 )
            else
              AddFieldInternal( fldName.AsString, TGIS_FieldType.String, 1, 0 ) ;

            with FieldInfo( Fields.Count -1 ) do begin
              if assigned( fldLength ) then
                Width := fldLength.AsInteger ;
              if fldType.AsString = 'esriFieldTypeOID' then
                IsUID := True ;
            end ;
          end ;
        end ;
      end ;
    end
    else begin
      objFields := JSONObjectFind( 'fieldAliases', TGIS_JSONObject(_obj) ) ;
      if assigned( objFields ) then begin
        try
          if JSONObjectFindFirst( objFields, itr ) then
            while True do begin
              if FindField( itr.key ) < 0 then
                AddFieldInternal( itr.key, TGIS_FieldType.String, 1, 0 ) ;
              if not JSONObjectFindNext( itr ) then break ;
            end ;
        finally
          JSONObjectFindClose( itr ) ;
        end ;
      end ;
    end;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadPoint(
    const _obj : TObject
  ) ;
  var
    objX : TGIS_JSONObject ;
    objY : TGIS_JSONObject ;
    x,y  : Double ;
  begin
    objX := JSONObjectFind( 'x', TGIS_JSONObject(_obj) ) ;
    objY := JSONObjectFind( 'y', TGIS_JSONObject(_obj) ) ;

    if assigned( objX ) and assigned( objY ) then begin
      currShape := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
      currShape.Lock( TGIS_Lock.Projection ) ;
        currShape.AddPart ;
        x := objX.AsDouble ;
        y := objY.AsDouble ;
        currShape.AddPoint( GisPoint( x, y ) ) ;
      currShape.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadPolyline(
    const _obj : TObject
  ) ;
  var
    itr   : TGIS_JSONIter ;
    i, j  : Integer ;
  begin
    try
      if JSONObjectFindFirst( 'paths', TGIS_JSONObject(_obj), itr ) then begin
        currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XY ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              currShape.AddPart ;
              if assigned( TGIS_JSONObject( itr.val.AsArray[ i ] ) ) then
                for j := 0 to TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray.Count - 1 do
                  geoJSONReadRawPoint( TGIS_JSONObject( itr.val.AsArray[ i ] ).AsArray[ j ],
                                       currShape
                                      ) ;
            end ;
          end ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadPolygon(
    const _obj : TObject
  ) ;
  var
    itr      : TGIS_JSONIter ;
    i, k     : Integer ;
    oRing    : TGIS_JSONObject ;
  begin
    try
      if JSONObjectFindFirst( 'rings', TGIS_JSONObject(_obj), itr ) then begin
        currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XY ) ;
        try
          currShape.Lock( TGIS_Lock.Projection ) ;
          if JSONObjectGetType( itr.val ) = TGIS_JSONType.Array then begin
            for i := 0 to itr.val.AsArray.Count - 1 do begin
              oRing := TGIS_JSONObject( itr.val.AsArray[ i ] ) ;
              if JSONObjectGetType( oRing ) = TGIS_JSONType.Array then begin
                currShape.AddPart ;
                for k := 0 to oRing.AsArray.Count - 1 do
                  geoJSONReadRawPoint( oRing.AsArray[ k ], currShape ) ;
              end ;
            end ;
          end ;
        finally
          currShape.Unlock ;
        end ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadMultiPoint(
    const _obj : TObject
  ) ;
  var
    objPoints : TGIS_JSONObject ;
    i         : Integer ;
  begin
    objPoints := JSONObjectFind( 'points', TGIS_JSONObject(_obj) ) ;

    if assigned( objPoints ) then begin
      currShape := CreateShape( TGIS_ShapeType.MultiPoint, TGIS_DimensionType.XY ) ;
      currShape.Lock( TGIS_Lock.Projection ) ;
        currShape.AddPart ;
        if JSONObjectGetType( objPoints ) = TGIS_JSONType.Array then
          for i := 0 to objPoints.AsArray.Count - 1 do
            esriJSONReadRawPoint( objPoints.AsArray[ i ], currShape ) ;

      currShape.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadAttributes(
    const _obj : TObject
  ) ;
  var
    itr : TGIS_JSONIter ;
    idx : Integer ;
  begin
    if not assigned( currShape ) then exit ;

    try
      if JSONObjectFindFirst( TGIS_JSONObject(_obj), itr ) then
        while True do begin
          if assigned( itr.val ) then begin
            idx := FindField( itr.key ) ;
            if idx >-1 then
              case FieldInfo(idx).FieldType of
                TGIS_FieldType.String  : currShape.SetField( itr.key, itr.val.AsString ) ;
                TGIS_FieldType.Number  : currShape.SetField( itr.key, itr.val.AsInteger ) ;
                TGIS_FieldType.Float   : currShape.SetField( itr.key, itr.val.AsDouble ) ;
                else                     currShape.SetField( itr.key, itr.val.AsString ) ;
              end ;
          end ;

          if not JSONObjectFindNext( itr ) then break ;
        end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.esriJSONReadRawPoint(
    const _obj : TObject ;
    const _shp : TGIS_Shape
   ) ;
  var
    ptg : TGIS_Point ;
  begin
    if JSONObjectGetType( TGIS_JSONObject(_obj) ) = TGIS_JSONType.Array then begin
      if TGIS_JSONObject(_obj).AsArray.Count > 1  then begin
        ptg.X := TGIS_JSONObject( TGIS_JSONObject(_obj).AsArray[ 0 ] ).AsDouble ;
        ptg.Y := TGIS_JSONObject( TGIS_JSONObject(_obj).AsArray[ 1 ] ).AsDouble ;
      end ;
      _shp.AddPoint( ptg ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.topoJSONReadTopology(
    const _obj : TObject
  ) ;
  var
    objects    : TGIS_JSONObject ;
    trans      : TGIS_JSONObject ;
    scale      : TGIS_JSONObject ;
    translate  : TGIS_JSONObject ;
    itr        : TGIS_JSONIter ;
    scale0     : Double ;
    scale1     : Double ;
    translate0 : Double ;
    translate1 : Double ;
    objFea     : TGIS_JSONObject ;
  begin
    trans := JSONObjectFind( 'transform', TGIS_JSONObject(_obj) ) ;

    hasTransform := False ;

    if assigned( trans ) then begin
      scale     := JSONObjectFind( 'scale', trans ) ;
      translate := JSONObjectFind( 'translate', trans ) ;

      scale0 := 0 ;
      scale1 := 0 ;
      if assigned( scale ) and
        ( JSONObjectGetType( scale ) = TGIS_JSONType.Array ) and
        ( TGIS_JSONObject(scale).AsArray.Count = 2          ) then begin

          scale0 := TGIS_JSONObject(scale.AsArray[0]).AsDouble ;
          scale1 := TGIS_JSONObject(scale.AsArray[1]).AsDouble ;
      end ;

      translate0 := 0 ;
      translate1 := 0 ;
      if assigned( translate ) and
        ( JSONObjectGetType( translate ) = TGIS_JSONType.Array ) and
        ( TGIS_JSONObject(translate).AsArray.Count = 2          ) then begin

          translate0 := TGIS_JSONObject(translate.AsArray[0]).AsDouble ;
          translate1 := TGIS_JSONObject(translate.AsArray[1]).AsDouble ;
      end ;

      hasTransform           := True ;
      {$IFDEF OXYGENE}
        transform := new TGIS_TopoJSONTransform ;
      {$ENDIF}
      transform.scale[0]     := scale0 ;
      transform.scale[1]     := scale1 ;
      transform.translate[0] := translate0 ;
      transform.translate[1] := translate1 ;
    end ;

    arcs    := JSONObjectFind( 'arcs'   , TGIS_JSONObject(_obj) ) ;
    objects := JSONObjectFind( 'objects', TGIS_JSONObject(_obj) ) ;

    if not assigned( arcs ) or not assigned( objects ) then exit ;

    try
      if JSONObjectFindFirst( TGIS_JSONObject(objects), itr ) then begin
        while true do begin
          objFea := itr.val ;
          if assigned( objFea ) and
            ( JSONObjectGetType( objFea ) = TGIS_JSONType.&Object ) then begin

              if FindField( 'id' ) = -1 then
                AddFieldInternal( 'id', TGIS_FieldType.String, 1 , 0 ) ;
              buildShapes( objFea ) ;
          end ;
          if not JSONObjectFindNext( itr ) then break ;
        end;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;

  end ;

  procedure TGIS_LayerJSON.topoJSONReadArcPoints(
    const _obj : TObject ;
    const _shp : TGIS_Shape
  ) ;
  var
    idx, i, j   : Integer ;
    num_ptg     : Integer ;
    coords      : TGIS_JSONObject ;
    coord       : TGIS_JSONObject ;
    x, y, z, m  : Double ;
    xx, yy      : Double ;
    reverse     : Boolean ;
    lst         : TGIS_Point3DList ;
  begin
    x := 0 ;
    y := 0 ;
    z := 0 ;
    m := 0 ;
    if JSONObjectGetType( TGIS_JSONObject(_obj) ) = TGIS_JSONType.Array then begin
      for i := 0 to TGIS_JSONObject(_obj).AsArray.Count - 1 do begin

        idx := TGIS_JSONObject(TGIS_JSONObject(_obj).AsArray[i]).AsInteger ;
        if idx < 0 then begin
          reverse := True ;
          lst := TGIS_Point3DList.Create ;
          idx := -idx - 1 ;
        end
        else
          reverse := False ;

        if idx >= TGIS_JSONObject(arcs).AsArray.Count then exit ;

        coords := TGIS_JSONObject(TGIS_JSONObject(arcs).AsArray[idx]) ;

        xx := 0 ;
        yy := 0 ;
          for j := 0 to coords.AsArray.Count-1 do begin
            coord   := TGIS_JSONObject(coords.AsArray[j]) ;
            num_ptg := coord.AsArray.Count ;

            if num_ptg > 1 then begin
              if hasTransform then begin
                xx := xx + TGIS_JSONObject( coord.AsArray[ 0 ] ).AsDouble ;
                yy := yy + TGIS_JSONObject( coord.AsArray[ 1 ] ).AsDouble ;
              end
              else begin
                xx := TGIS_JSONObject( coord.AsArray[ 0 ] ).AsDouble ;
                yy := TGIS_JSONObject( coord.AsArray[ 1 ] ).AsDouble ;
              end ;
              x := xx ;
              y := yy ;
              z := 0 ;
              m := 0 ;
            end ;

            if num_ptg > 2  then
              z := TGIS_JSONObject( coord.AsArray[ 2 ] ).AsDouble ;

            if num_ptg > 3  then
              m := TGIS_JSONObject( coord.AsArray[ 3 ] ).AsDouble ;

            if hasTransform then begin
              x := x * transform.scale[0] + transform.translate[0] ;
              y := y * transform.scale[1] + transform.translate[1] ;
            end ;

            if reverse then
              lst.Add( GisPoint3D( x, y, z, m ) )
            else
              _shp.AddPoint3D( GisPoint3D( x, y, z, m ) ) ;
          end ;

          if reverse then begin
            for j := lst.Count-1 downto 0 do
              _shp.AddPoint3D( lst[j] ) ;

            FreeObject( lst ) ;
          end;
        end ;

    end ;
  end ;

  procedure TGIS_LayerJSON.cityJSONRead(
    const _obj : TObject
  ) ;
  var
    objects    : TGIS_JSONObject ;
    trans      : TGIS_JSONObject ;
    scale      : TGIS_JSONObject ;
    translate  : TGIS_JSONObject ;
    metadata   : TGIS_JSONObject ;
    objFea     : TGIS_JSONObject ;
    obj        : TGIS_JSONObject ;
    itr        : TGIS_JSONIter ;
    scale0     : Double ;
    scale1     : Double ;
    scale2     : Double ;
    translate0 : Double ;
    translate1 : Double ;
    translate2 : Double ;
    ext        : TGIS_Extent3D ;
    crs        : String ;
  begin
    trans := JSONObjectFind( 'transform', TGIS_JSONObject(_obj) ) ;

    hasTransform := False ;

    if assigned( trans ) then begin
      scale     := JSONObjectFind( 'scale', trans ) ;
      translate := JSONObjectFind( 'translate', trans ) ;

      scale0 := 0 ;
      scale1 := 0 ;
      scale2 := 0 ;
      if assigned( scale ) and
        ( JSONObjectGetType( scale ) = TGIS_JSONType.Array ) and
        ( TGIS_JSONObject(scale).AsArray.Count = 3         ) then begin

          scale0 := TGIS_JSONObject(scale.AsArray[0]).AsDouble ;
          scale1 := TGIS_JSONObject(scale.AsArray[1]).AsDouble ;
          scale2 := TGIS_JSONObject(scale.AsArray[2]).AsDouble ;
      end ;

      translate0 := 0 ;
      translate1 := 0 ;
      translate2 := 0 ;
      if assigned( translate ) and
        ( JSONObjectGetType( translate ) = TGIS_JSONType.Array ) and
        ( TGIS_JSONObject(translate).AsArray.Count = 3         ) then begin

          translate0 := TGIS_JSONObject(translate.AsArray[0]).AsDouble ;
          translate1 := TGIS_JSONObject(translate.AsArray[1]).AsDouble ;
          translate2 := TGIS_JSONObject(translate.AsArray[2]).AsDouble ;
      end ;

      hasTransform           := True ;
      {$IFDEF OXYGENE}
        transform := new TGIS_TopoJSONTransform ;
      {$ENDIF}
      transform.scale[0]     := scale0 ;
      transform.scale[1]     := scale1 ;
      transform.scale[2]     := scale2 ;
      transform.translate[0] := translate0 ;
      transform.translate[1] := translate1 ;
      transform.translate[2] := translate2 ;
    end ;

    metadata := JSONObjectFind( 'metadata', TGIS_JSONObject(_obj) ) ;
    if assigned( metadata ) then begin
      obj := JSONObjectFind( 'geographicalExtent', metadata ) ;
      if not assigned( obj ) then
        obj := JSONObjectFind( 'bbox', metadata ) ;

      if assigned( obj ) then begin
        {$IFDEF GIS_NORECORDS}
          ext := new TGIS_Extent3D ;
        {$ENDIF}
        ext.XMin := obj[0].AsDouble ;
        ext.YMin := obj[1].AsDouble ;
        ext.ZMin := obj[2].AsDouble ;
        ext.XMax := obj[3].AsDouble ;
        ext.YMax := obj[4].AsDouble ;
        ext.ZMax := obj[5].AsDouble ;
        if not GisIsNoWorld3D( ext ) then
          FExtent3D := ext ;
      end ;

      obj := JSONObjectFind( 'referenceSystem', metadata ) ;
      if assigned( obj ) then begin
        crs := obj.AsString ;
        SetCSByWKT( crs ) ;
      end
      else begin
        obj := metadata['crs.epsg'] ;
        if assigned( obj ) then
          SetCSByEPSG( obj.AsInteger ) ;
      end ;
    end ;

    arcs    := JSONObjectFind( 'vertices', TGIS_JSONObject(_obj) ) ;
    objects := JSONObjectFind( 'CityObjects', TGIS_JSONObject(_obj) ) ;

    if not assigned( arcs ) or not assigned( objects ) then exit ;

    try
      if JSONObjectFindFirst( TGIS_JSONObject(objects), itr ) then begin
        if FindField( 'id' ) = -1 then
          AddFieldInternal( 'id', TGIS_FieldType.String, 1 , 0 ) ;
        if FindField( 'type' ) = -1 then
          AddFieldInternal( 'type', TGIS_FieldType.String, 1 , 0 ) ;

        while true do begin
          objFea := itr.val ;
          if assigned( objFea ) and
            ( JSONObjectGetType( objFea ) = TGIS_JSONType.&Object ) then begin

            buildCityJSON( objFea, itr.key ) ;
          end ;
          if not JSONObjectFindNext( itr ) then break ;
        end;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function TGIS_LayerJSON.loadFromFile(
    const _path : String
  ) : String ;
  var
    f : TGIS_BufferedFileStream ;
  begin
    f := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
    try
      Result := loadFromStream( f ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  function TGIS_LayerJSON.loadFromService(
    const _path : String
  ) : String ;
  var
    r : TGIS_HttpResponse ;
  begin
    Result := '' ;
    r.Stream := nil ;
    try
      r := fetchHttp( _path ) ;
      if r.Status = GIS_HTTP_OK then
        Result := loadFromStream( r.Stream ) ;
    finally
      FreeObject( r.Stream ) ;
    end ;
  end ;

  function TGIS_LayerJSON.loadFromStream(
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
   try
     Result := TEncoding.UTF8.GetString( buf, 0, _stm.Size ) ;
   except
     Result := '' ;
   end;
  end ;

  function TGIS_LayerJSON.fetchHttp(
    const _url  : String
  ) : TGIS_HttpResponse ;
  begin
    Result := TGIS_WebUtils.HttpFetch( _url, nil, nil, true, 40000,
                                       GetDefaultUserAgent( 'ttkWP' ), '', '', ''
                                      ) ;
  end ;

  procedure TGIS_LayerJSON.setUp ;
  var
    tkn     : TGIS_JSONTokenizer ;
    obj     : TGIS_JSONObject ;
    srcType : TGIS_GeoJSONSourceType ;
    data    : String ;
  begin
    inherited ;

    try
      tkn := TGIS_JSONTokenizer.Create ;

      try
        srcType := geoJSONGetSourceType( Path ) ;

        case srcType of
          TGIS_GeoJSONSourceType.File    : data := loadFromFile( Path ) ;
          TGIS_GeoJSONSourceType.Text    : data := Path ;
          TGIS_GeoJSONSourceType.Service : data := loadFromService( Path ) ;
          TGIS_GeoJSONSourceType.Unknown : if assigned( Stream ) then
                                             data := loadFromStream( Stream ) ;
        end ;

        RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;

        if assigned( Viewer ) then
          {$IFNDEF OXYGENE}
            tkn.ProgressEvent := TGIS_Viewer(Viewer.Ref).BusyShake ;
          {$ELSE}
            tkn.ProgressEvent := @Viewer.Ref.BusyShake ;
          {$ENDIF}
        try
          obj := tkn.Parse( data ) ;
        finally
          RaiseBusyRelease( Self ) ;
        end ;

        RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_DATA_LOAD ), [Name] ) ) ;
        Lock ;
        try
          if assigned( obj ) then begin
            buildShapes( obj ) ;
            readSpatialReference( obj ) ;
          end ;
        finally
          Unlock ;
          RaiseBusyRelease( Self ) ;
        end ;
      finally
        FreeObject( obj ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;

    FIsModified := False ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'GeoJSON (Vector Product Format)' ;
  end ;

  procedure TGIS_LayerJSON.writeShape(
    const _file : TGIS_Stream ;
    const _shp  : TGIS_Shape
   ) ;
  var
    i   : Integer ;
    cnt : Integer ;
    str : TStringBuilder ;
    sb  : TStringBuilder ;
    fld : TGIS_FieldInfo ;
    v   : Variant ;

    function escapeJSONString( const _val : String ) : String ;
    var
      pos : Integer ;
      len : Integer ;
      c   : Char ;
    begin
      sb.Length := 0 ;
      pos := StringFirst ;
      len := StringLast( _val ) ;

      while pos <= len do begin
        c := _val[pos] ;
        case c of
          #0  : sb.Append( '\u0000' ) ;
          '"' : sb.Append( '\"' ) ;
          '\' : sb.Append( '\\' ) ;
          '/' : sb.Append( '\/' ) ;
          else  sb.Append( c ) ;
        end ;
        inc( pos ) ;
      end ;
      Result := sb.ToString ;
    end ;

  begin
    _file.WriteString( Format('{"type":"Feature","id":"%d","geometry":', [_shp.Uid]) ) ;
    _file.WriteString( _shp.ExportToJSON ) ;

    str := TStringBuilder.Create ;
    try
      sb := TStringBuilder.Create ;
      cnt := Fields.Count -1 ;

      for i := 0 to cnt do begin
        fld := FieldInfo( i ) ;
        if fld.Deleted then continue ;

        v := _shp.GetFieldEx( fld.NewName ) ;

        if VarIsNull( v ) then
          str.Append( Format( '"%s":null', [ fld.NewName ] ) )
        else begin
          case fld.FieldType of
            TGIS_FieldType.String  :
              str.Append(
                Format( '"%s":"%s"',
                        [ fld.NewName, escapeJSONString(VarToString(v)) ]
                      )
              ) ;
            TGIS_FieldType.Number  :
              str.Append(
                Format( '"%s":%s', [ fld.NewName,
                                     DotFloatToStrPrec( VarToDouble(v), fld.NewDecimal) ]
                       )
              ) ;
            TGIS_FieldType.Float   :
              str.Append(
                Format( '"%s":%s', [ fld.NewName, DotFloatToStr( VarToDouble(v) ) ] )
              ) ;
            TGIS_FieldType.Boolean :
              if VarToBoolean( v ) then
                str.Append( Format( '"%s":%s', [ fld.NewName, 'true' ] ) )
              else
                str.Append( Format( '"%s":%s', [ fld.NewName, 'false' ] ) ) ;
            TGIS_FieldType.Date    :
              str.Append(
                Format( '"%s":"%s"',
                        [ fld.NewName,
                          DateTimeToXMLString( VarToDateTime(v), 2, True ) ]
                      )
              )
          end ;
        end ;

        if i < cnt then
          str.Append( ',' ) ;
      end ;
      _file.WriteString( Format( ',"properties":{%s}}', [str.ToString] ) ) ;
    finally
      FreeObject( sb  ) ;
      FreeObject( str ) ;
    end ;
  end ;

  procedure TGIS_LayerJSON.Build(
    const _path   : String ;
    const _extent : TGIS_Extent;
    const _type   : TGIS_ShapeType ;
    const _dim    : TGIS_DimensionType
   ) ;
  begin
    inherited ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;
  end ;

  procedure TGIS_LayerJSON.ImportLayerEx(
    const _layer       : TGIS_LayerVector  ;
    const _extent      : TGIS_Extent       ;
    const _type        : TGIS_ShapeType    ;
    const _scope       : String            ;
    const _shape       : TGIS_Shape        ;
    const _de9im       : String            ;
    const _truncated   : Boolean
  ) ;
  var
    shape_file : TGIS_BufferedStream ;
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape  ;
    {$ENDIF}
    shp_tmp    : TGIS_Shape  ;
    ex         : TGIS_Extent ;
    shape_no   : Cardinal    ;
    end_uid    : TGIS_Uid    ;
    abort      : Boolean     ;
    old_scope  : String      ;
    first      : Boolean     ;
    bis_file   : Boolean     ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try
      ImportStructure( _layer ) ;

      PrepareExportFieldNames( 32 ) ;
      ExportStructureToFLD ;

      // prepare temporary geometry
      try
        if assigned( Stream ) then
          shape_file := TGIS_BufferedStream.Create( Stream )
        else
          shape_file := TGIS_BufferedFileStream.Create( GetTemporaryName( Path ),
                                                        TGIS_StreamMode.&Create
                                                       ) ;
        bis_file := not assigned( Stream ) ;
        shape_file.CodePage    := CodePage    ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( Path ),
                                     GetLastError
                                   ) ;
       end ;

       // header start
       if CS is TGIS_CSUnknownCoordinateSystem then
         shape_file.WriteString( '{"type":"FeatureCollection","crs":{"type":"none",'+
                                 '"properties":{}},"features":['
                                )
       else begin
         if CS.EPSG = 4326 then
           shape_file.WriteString(
              Format( '{"type":"FeatureCollection","crs":{"type":"name",'+
                      '"properties":{"name":"%s"}},"features":[',
                      [ 'urn:ogc:def:crs:OGC:1.3:CRS84' ]
                     )
           )
       else
         shape_file.WriteString(
              Format( '{"type":"FeatureCollection","crs":{"type":"name",'+
                      '"properties":{"name":"urn:ogc:def:crs:EPSG::%d"}},"features":[',
                    [ CS.EPSG ]
                   )
         ) ;
       end ;

      ex := GisCommonExtent( _layer.Extent, _extent ) ;

      try
        old_scope := _layer.Scope ;
        _layer.Scope := '' ;

        first := True ;
        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( ex, _scope, _shape, _de9im ) do
        begin
          if first then
            first := False
          else
            shape_file.WriteString( ',' ) ;

          shp_tmp := shp.PrepareExportShape(
                           CS, _extent, _truncated, True
                         ) ;
          try
            if assigned( shp_tmp ) and
               ( not shp_tmp.IsDeleted ) and
               ( ( _type = shp_tmp.ShapeType   ) or
                 ( _type = TGIS_ShapeType.Unknown )
               ) then
            begin
              shp_tmp.Lock( TGIS_Lock.Projection );
              writeShape( shape_file, shp_tmp ) ;
              shp_tmp.Unlock;
            end ;
          finally
            if shp <> shp_tmp then FreeObject( shp_tmp ) ;
          end ;

          if shape_no mod 100 = 1 then begin
            abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
            if abort then break ;
          end ;
          inc( shape_no ) ;
        end ;
      finally
        // header end
        shape_file.WriteString( ']}' ) ;

        _layer.Scope := old_scope ;

        if not bis_file then
          shape_file.FlushBuffer ;

        FreeObject( shape_file ) ;

        if abort then begin
          if bis_file then
            DeleteFile( GetTemporaryName( Path ) ) ;
        end
        else begin
          if bis_file then
            DeleteFile( GetBackupName( Path ) ) ;

          if bis_file then
            RenameFile( Path, GetBackupName( Path ) ) ;

          try
            if bis_file then
              if not RenameFile( GetTemporaryName( Path ), Path ) then
                raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), Path, GetLastError ) ;
          except
            // recover ;
            if bis_file then
              RenameFile( GetBackupName( Path ), Path ) ;
            raise ;
           end ;
        end ;

        if not IsOpened then begin
          Items.Clear ;
          Fields.Clear ;

          Open ;
        end;
      end ;
    finally
      RaiseBusyRelease( _layer ) ;
   end ;
  end ;

  procedure TGIS_LayerJSON.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  function TGIS_LayerJSON.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result :=
      ( Pos( '.GEOJSON', UpperCase( _path ) ) >= StringFirst  ) or
      ( Pos( '.JSON', UpperCase( _path ) ) >= StringFirst  ) or
      ( GetParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE ) = 'JSON' ) or
      ( UpperCase( URLGetParameterValue(_path, 'f') ) = 'JSON' ) or
      ( UpperCase( URLGetParameterValue(_path, 'f') ) = 'GEOJSON' ) ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerJSON.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-JSON', 'GeoJSON',
                   TGIS_LayerJSON, '.json;.geojson',
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                    False
                  );
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerJSON.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
