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
  Clinets for the TatukGIS online OSM services.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoOSMServices;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoOSMServices"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Generics.Collections,

    Lider.CG.GIS.GeoBaseObject,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoGeocoding ;
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
  ///   Base class for TatukGIS online service clients.
  /// </summary>
  TGIS_OSMServiceAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class( TGIS_BaseObject )
    private
      userAgent : String ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FBusyEvent : TGIS_BusyEvent ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FTimeout : Integer ;
    {$IFDEF OXYGENE} assembly or {$ENDIF} protected
      /// <summary>
      ///   Non-standard address of the service.
      /// </summary>
      FAddress  : String ;
      /// <summary>
      ///   Default API prefix for the first kind of requests.
      /// </summary>
      sDefaultPrefix1 : String ;
      /// <summary>
      ///   Default API prefix for the second kind of requests.
      /// </summary>
      sDefaultPrefix2 : String ;
      /// <summary>
      ///   API prefix for the first kind of requests.
      /// </summary>
      sPrefix1  : String ;
      /// <summary>
      ///   API prefix for the second kind of requests.
      /// </summary>
      sPrefix2  : String ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function  fget_Timeout : Integer ;
      procedure fset_Timeout ( const _val : Integer
                             ) ;
    {$IFDEF OXYGENE} assembly or {$ENDIF} protected
      /// <summary>
      ///   Gets the current address of the service.
      /// </summary>
      /// <returns>
      ///   Current address of the service.
      /// </returns>
      function  httpAddress1 : String ;

      /// <summary>
      ///   Gets the current address of the service.
      /// </summary>
      /// <returns>
      ///   Current address of the service.
      /// </returns>
      function  httpAddress2 : String ;

      /// <summary>
      ///   Handles HTTP requests in a standarized manner.
      /// </summary>
      /// <param name="_url">
      ///   resource URL
      /// </param>
      /// <param name="_strm">
      ///   response stream
      /// </param>
      /// <returns>
      ///   True if succeeded.
      /// </returns>
      function  httpFetch ( const _url  : String ;
                            const _strm : TStream
                          ) : Boolean ;
    {$IFDEF OXYGENE}
      /// <summary>
      ///   Busy event for fetching the data.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_e">
      ///   arguments
      /// </param>
      procedure doBusy             ( const _sender    : Object ;
                                     const _e         : TGIS_BusyEventArgs
                                   ) ;
    {$ELSE}
      /// <summary>
      ///   Busy event for fetching the data.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_pos">
      ///   argument
      /// </param>
      /// <param name="_end">
      ///   argument
      /// </param>
      /// <param name="_abort">
      ///   argument
      /// </param>
      procedure doBusy             (       _sender    : TObject ;
                                           _pos       : Integer ;
                                           _end       : Integer ;
                                     var   _abort     : Boolean
                                   ) ;
    {$ENDIF}
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Non-standard address of the service including port number (e.g.
      ///   '127.0.0.1:8888'). If empty then the default address is used.
      /// </summary>
      property Address : String
                         read  FAddress
                         write FAddress ;
      /// <summary>
      ///   Maximum waiting time for service response.
      /// </summary>
      property Timeout : Integer
                         read  fget_Timeout
                         write fset_Timeout ;
    public
      /// <summary>
      ///   Event fired when waiting for the service response.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent
                             delegate FBusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent
                             read  FBusyEvent
                             write FBusyEvent ;
      {$ENDIF}
  end ;


  /// <summary>
  ///   Class encapsulating access to the TatukGIS online geocoding service.
  /// </summary>
  /// <remarks>
  ///   Resolved locations are returned as an in-memory vector layer containing
  ///   point shapes.
  /// </remarks>
  TGIS_OSMGeocoding = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_OSMServiceAbstract )
    private
      FLimit            : Integer ;
      FBiasPoint        : TGIS_Point ;
      FRestrictedExtent : TGIS_Extent ;
    private
      function  fget_Limit : Integer ;
      procedure fset_Limit ( const _val : Integer
                           ) ;
      function  fget_RestrictedExtent : TGIS_Extent ;
      procedure fset_RestrictedExtent ( const _val : TGIS_Extent
                                      ) ;
    private
      function  fetchJSON ( const _url : String ;
                            const _rev : Boolean
                          ) : TGIS_LayerVector ;
      function  inWorld   ( const _point : TGIS_Point
                          ) : Boolean ;
      function  isWorld   ( const _extent : TGIS_Extent
                          ) : Boolean ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Sends forward geocoding request to the online service.
      /// </summary>
      /// <param name="_name">
      ///   address/location string
      /// </param>
      /// <returns>
      ///   Layer containing resolved locations.
      /// </returns>
      function  &Forward     ( const _name : String
                             ) : TGIS_LayerVector ;
      /// <summary>
      ///   Sends forward geocoding request to the online service.
      /// </summary>
      /// <param name="_name">
      ///   address/location string
      /// </param>
      /// <returns>
      ///   First match of resolved.
      /// </returns>
      function  ForwardPoint ( const _name : String
                             ) : TGIS_Point ;
      /// <summary>
      ///   Sends reverse geocoding request to the online service.
      /// </summary>
      /// <param name="_loc">
      ///   location as a point in WGS84 coordinates
      /// </param>
      /// <returns>
      ///   Layer containing resolved.
      /// </returns>
      function  Reverse      ( const _loc  : TGIS_Point
                             ) : TGIS_LayerVector ;
    public
      /// <summary>
      ///   Limit of the number of results.
      /// </summary>
      property Limit        : Integer
                              read  fget_Limit
                              write fset_Limit ;
      /// <summary>
      ///   Point in WGS84 coordinates used for location bias.
      /// </summary>
      /// <remarks>
      ///   When active each resolved location is weighted based on the distance
      ///   to the bias point - the closer it is the higher priority it gets.
      ///   To deactivate location bias set this property to any point outside
      ///   the (-180, -90, 180, 90) extent.
      /// </remarks>
      property BiasPoint        : TGIS_Point
                                  read  FBiasPoint
                                  write FBiasPoint ;
      /// <summary>
      ///   Extent in WGS84 coordinates used to limit results.
      /// </summary>
      /// <remarks>
      ///   Only resolved locations inside the restricted extent are returned.
      ///   To deactivate restriction set this property to any extent which
      ///   contains the (-180, -90, 180, 90) extent (e.g. GisWholeWorld).
      /// </remarks>
      property RestrictedExtent : TGIS_Extent
                                  read  fget_RestrictedExtent
                                  write fset_RestrictedExtent ;
  end ;


  /// <summary>
  ///   Profiles of the TatukGIS online routong service.
  /// </summary>
  TGIS_OSMRoutingProfile = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Car profile.
    /// </summary>
    Car,
    /// <summary>
    ///   Bike profile.
    /// </summary>
    Bike,
    /// <summary>
    ///   Foot (pedestrian) profile.
    /// </summary>
    Foot
  ) ;


  /// <summary>
  ///   Class encapsulating access to the TatukGIS online routing service.
  /// </summary>
  /// <remarks>
  ///   Route is returned as an in-memory vector layer containing ordered road
  ///   fragments as line shapes and the start, intermediate and end points as
  ///   point type shapes.
  /// </remarks>
  TGIS_OSMRouting = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_OSMServiceAbstract )
    private
      FProfile : TGIS_OSMRoutingProfile ;
    private
      function  makeLayer : TGIS_LayerVector ;
      procedure fetchGPX  ( const _layer  : TGIS_LayerVector ;
                            const _points : TGIS_PointList
                          ) ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Sends routing request to the online service.
      /// </summary>
      /// <param name="_points">
      ///   ordered list of start, intermediate and end points in WGS84
      ///   coordinates
      /// </param>
      /// <returns>
      ///   Layer containing found route.
      /// </returns>
      function  Route ( const _points : TGIS_PointList
                      ) : TGIS_LayerVector ; overload ;
      /// <summary>
      ///   Sends routing request to the online service.
      /// </summary>
      /// <param name="_from">
      ///   start point in WGS84 coordinates
      /// </param>
      /// <param name="_to">
      ///   end point in WGS84 coordinates
      /// </param>
      /// <returns>
      ///   Layer containing found route.
      /// </returns>
      function  Route ( const _from   : TGIS_Point ;
                        const _to     : TGIS_Point
                      ) : TGIS_LayerVector ; overload ;
      /// <summary>
      ///   Sends routing request to the online service.
      /// </summary>
      /// <param name="_names">
      ///   ordered list of start, intermediate and end locations as
      ///   address/location strings
      /// </param>
      /// <returns>
      ///   Layer containing found route.
      /// </returns>
      function  Route ( const _names  : TGIS_StringList
                      ) : TGIS_LayerVector ; overload ;
      /// <summary>
      ///   Sends routing request to the online service.
      /// </summary>
      /// <param name="_from">
      ///   start location as an address/location string
      /// </param>
      /// <param name="_to">
      ///   end location as an address/location string
      /// </param>
      /// <returns>
      ///   Layer containing found route.
      /// </returns>
      function  Route ( const _from   : String ;
                        const _to     : String
                      ) : TGIS_LayerVector ; overload ;
    public
      /// <summary>
      ///   Routing profile (vehicle type).
      /// </summary>
      property Profile : TGIS_OSMRoutingProfile
                         read  FProfile
                         write FProfile ;
  end ;


  /// <summary>
  ///   Class encapsulating access to the TatukGIS online isochrone service.
  /// </summary>
  /// <remarks>
  ///   Isochrone is returned as an in-memory vector layer containing ordered
  ///   isochrone "buckets" as polygon shapes.
  /// </remarks>
  TGIS_OSMIsochrone = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_OSMServiceAbstract )
    private
      FProfile       : TGIS_OSMRoutingProfile ;
      FTimeLimit     : Integer ;
//?      FDistanceLimit : Integer ;
      FBuckets       : Integer ;
    private
      function  fget_TimeLimit : Integer ;
      procedure fset_TimeLimit ( const _val : Integer
                               ) ;
//?      function  fget_DistanceLimit
//?                               : Integer ;
//?      procedure fset_DistanceLimit
//?                               ( const _val : Integer
//?                               ) ;
      function  fget_Buckets   : Integer ;
      procedure fset_Buckets   ( const _val : Integer
                               ) ;
    private
      function  fetchJSON ( const _point : TGIS_Point ;
                            const _dist  : Boolean
                          ) : TGIS_LayerVector ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Sends isochrone request to the online service.
      /// </summary>
      /// <param name="_point">
      ///   start point in WGS84 coordinates
      /// </param>
      /// <returns>
      ///   Layer containing found insochrone.
      /// </returns>
      function  Isochrone   ( const _point : TGIS_Point
                            ) : TGIS_LayerVector ; overload ;
      /// <summary>
      ///   Sends isochrone request to the online service.
      /// </summary>
      /// <param name="_name">
      ///   start location as an address/location string
      /// </param>
      /// <returns>
      ///   Layer containing found insochrone.
      /// </returns>
      function  Isochrone   ( const _name  : String
                            ) : TGIS_LayerVector ; overload ;
//?      /// <summary>
//?      ///   Sends isodistance request to the online service.
//?      /// </summary>
//?      /// <param name="_point">
//?      ///   start point in WGS84 coordinates
//?      /// </param>
//?      /// <returns>
//?      ///   Layer containing found isodistance.
//?      /// </returns>
//?      function  Isodistance ( const _point : TGIS_Point
//?                            ) : TGIS_LayerVector ; overload ;
//?      /// <summary>
//?      ///   Sends isodistance request to the online service.
//?      /// </summary>
//?      /// <param name="_name">
//?      ///   start location as an address/location string
//?      /// </param>
//?      /// <returns>
//?      ///   Layer containing found isodistance.
//?      /// </returns>
//?      function  Isodistance ( const _name  : String
//?                            ) : TGIS_LayerVector ; overload ;
    public
      /// <summary>
      ///   Isochrone profile (vehicle type).
      /// </summary>
      property Profile   : TGIS_OSMRoutingProfile
                           read  FProfile
                           write FProfile ;
      /// <summary>
      ///   Isochrone time limit.
      /// </summary>
      property TimeLimit : Integer
                           read  fget_TimeLimit
                           write fset_TimeLimit ;
//?      /// <summary>
//?      ///   Isodistance distance limit.
//?      /// </summary>
//?      property DistanceLimit : Integer
//?                           read  fget_DistanceLimit
//?                           write fset_DistanceLimit ;
      /// <summary>
      ///   Number of isochrone/isodinstance intervals.
      /// </summary>
      property Buckets   : Integer
                           read  fget_Buckets
                           write fset_Buckets ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoLayerJSON,
    Lider.CG.GIS.GeoLayerGPX ;
{$ENDIF}

const
  // general
  GIS_LOCAL_DEFAULT_TIMEOUT : Integer = 100 ;

  GIS_LOCAL_DEFAULT_ADDRESS : String =
    'services.tatukgis.com' ;

  GIS_LOCAL_DEFAULT_PREFIX_FGEOCODING : String =
    '/geocoding?';

  GIS_LOCAL_DEFAULT_PREFIX_RGEOCODING : String =
    '/reversegeocoding?';

  GIS_LOCAL_DEFAULT_PREFIX_ROUTING : String =
    '/routing?';

  GIS_LOCAL_DEFAULT_PREFIX_ISOCHRONE : String =
    '/isochrone?';

  GIS_LOCAL_PREFIX_FGEOCODING : String =
    '/api?';

  GIS_LOCAL_PREFIX_RGEOCODING : String =
    '/reverse?';

  GIS_LOCAL_PREFIX_ROUTING : String =
    '/route?';

  GIS_LOCAL_PREFIX_ISOCHRONE : String =
    '/isochrone?';

  // forward geocoding
  GIS_LOCAL_FGEOCODING_LAYER_NAME : String =
    'fgeocoding' ;
  GIS_LOCAL_FGEOCODING_LAYER_LABEL : String =
    'Forward geocoding' ;

  // reverse geocoding
  GIS_LOCAL_RGEOCODING_LAYER_NAME : String =
    'rgeocoding' ;
  GIS_LOCAL_RGEOCODING_LAYER_LABEL : String =
    'Reverse geocoding' ;

  //routing
  GIS_LOCAL_ROUTING_LAYER_NAME : String =
    'route' ;
  GIS_LOCAL_ROUTING_LAYER_LABEL : String =
    'Route' ;

  // isochrone/isodistance
  GIS_LOCAL_ISOCHRONE_LAYER_NAME : String =
    'isochrone' ;
  GIS_LOCAL_ISOCHRONE_LAYER_LABEL : String =
    'Isochrone' ;
  GIS_LOCAL_ISODISTANCE_LAYER_NAME : String =
    'isodistance' ;
  GIS_LOCAL_ISODISTANCE_LAYER_LABEL : String =
    'Isodistance' ;

  // names of routing profiles
  GIS_LOCAL_ROUTING_PROFILE_CAR  : String =
    'car' ;
  GIS_LOCAL_ROUTING_PROFILE_BIKE : String =
    'bike' ;
  GIS_LOCAL_ROUTING_PROFILE_FOOT : String =
    'foot' ;

  // queries, fields and field values
  GIS_LOCAL_GPX_TYPE_TRK : String =
    '( GPX_TYPE = ''trk'' )' ;
  GIS_LOCAL_FIELD_TYPE : String =
    'type' ;
  GIS_LOCAL_TYPE_ROUTE : String =
    'route' ;
  GIS_LOCAL_TYPE_FROM : String =
    'from' ;
  GIS_LOCAL_TYPE_TO : String =
    'to' ;
  GIS_LOCAL_TYPE_THROUGH : String =
    'through' ;

//==============================================================================
// TGIS_OSMServiceAbstract
//==============================================================================

  constructor TGIS_OSMServiceAbstract.Create ;
  begin
    inherited ;

    userAgent := GetDefaultUserAgent( '' ) ;
  end ;


  procedure TGIS_OSMServiceAbstract.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_OSMServiceAbstract.fget_Timeout
    : Integer ;
  begin
    Result := FTimeout ;
  end ;


  procedure TGIS_OSMServiceAbstract.fset_Timeout(
    const _val : Integer
  ) ;
  begin
    if _val < GIS_LOCAL_DEFAULT_TIMEOUT then
      FTimeout := GIS_LOCAL_DEFAULT_TIMEOUT
    else
      FTimeout := _val ;
  end ;


  {$IFDEF OXYGENE}
    procedure TGIS_OSMServiceAbstract.doBusy(
      const _sender : Object ;
      const _e      : TGIS_BusyEventArgs
    ) ;
    begin
      if assigned( FBusyEvent ) then
        FBusyEvent( _sender, _e ) ;
    end ;
  {$ELSE}
    procedure TGIS_OSMServiceAbstract.doBusy(
           _sender : TObject ;
           _pos    : Integer ;
           _end    : Integer ;
      var  _abort  : Boolean
    ) ;
    begin
      if Assigned( FBusyEvent ) then
        FBusyEvent( _sender, _pos, _end, _abort ) ;
    end ;
  {$ENDIF}


  function TGIS_OSMServiceAbstract.httpAddress1
    : String ;
  begin
    if (IsStringEmpty(FAddress)) then
      Result := 'http://' + GIS_LOCAL_DEFAULT_ADDRESS + sDefaultPrefix1
    else
      Result := 'http://' + FAddress + sPrefix1 ;
  end ;


  function TGIS_OSMServiceAbstract.httpAddress2
    : String ;
  begin
    if (IsStringEmpty(FAddress)) then
      Result := 'http://' + GIS_LOCAL_DEFAULT_ADDRESS + sDefaultPrefix2
    else
      Result := 'http://' + FAddress + sPrefix2 ;
  end ;


  function TGIS_OSMServiceAbstract.httpFetch(
    const _url  : String ;
    const _strm : TStream
  ) : Boolean ;
  var
    resp : TGIS_HttpResponse ;
  begin
    Result := False ;

    resp := TGIS_WebUtils.HttpFetch(
      _url, _strm,
      {$IFDEF OXYGENE}
        @doBusy,
      {$ELSE}
        doBusy,
      {$ENDIF}
      False, Timeout, userAgent
    ) ;

    case resp.Status of
      GIS_HTTP_FETCH_UNKNOWN,
      GIS_HTTP_FETCH_INTERNALERROR,
      GIS_HTTP_FETCH_NOTCOMPLETED :
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_OSM_ERROR ),
          'status', resp.Status
        ) ;
      GIS_HTTP_FETCH_TIMEOUT :
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_OSM_TIMEOUT ),
          'status', resp.Status
        ) ;
      GIS_HTTP_OK : Result := True ;
      GIS_HTTP_AUTHORIZATIONREQUIRED,
      GIS_HTTP_NOTFOUND,
      GIS_HTTP_SERVICEUNAVAILABLE,
      GIS_HTTP_GATEWAYTIMEOUT :
        raise EGIS_Exception.Create(
          Format( _rsrc( GIS_RS_ERR_OSM_NO_ACCESS ), [resp.Status] ) ,
          'status', resp.Status
        ) ;
    end ;
  end ;


//==============================================================================
// TGIS_OSMGeocoding
//==============================================================================

  constructor TGIS_OSMGeocoding.Create ;
  begin
    inherited ;

    FAddress := '' ;
    sDefaultPrefix1 := GIS_LOCAL_DEFAULT_PREFIX_FGEOCODING ;
    sDefaultPrefix2 := GIS_LOCAL_DEFAULT_PREFIX_RGEOCODING ;
    sPrefix1 := GIS_LOCAL_PREFIX_FGEOCODING ;
    sPrefix2 := GIS_LOCAL_PREFIX_RGEOCODING ;

    FLimit := 10 ;
    FBiasPoint := GisPoint( GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;
    FRestrictedExtent := GisExtent( -180.0, -90.0, 180.0, 90.0 ) ;
  end ;


  procedure TGIS_OSMGeocoding.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_OSMGeocoding.fget_Limit
    : Integer ;
  begin
    Result := FLimit ;
  end ;


  procedure TGIS_OSMGeocoding.fset_Limit(
    const _val : Integer
  ) ;
  begin
    if _val > 1 then
      FLimit := _val
    else
      FLimit := 1 ;
  end ;


  function TGIS_OSMGeocoding.fget_RestrictedExtent
    : TGIS_Extent ;
  begin
    Result := FRestrictedExtent ;
  end ;


  procedure TGIS_OSMGeocoding.fset_RestrictedExtent(
    const _val : TGIS_Extent
  ) ;
  var
    ext : TGIS_Extent ;
  begin
    ext := GisExtent( -180.0, -90.0, 180.0, 90.0 ) ;

    if ( not GisIsCommonExtent( _val, ext ) ) or
       GisContainExtent( _val, ext ) then
      FRestrictedExtent := ext
    else
      FRestrictedExtent := GisCommonExtent( _val, ext ) ;
  end ;


  function TGIS_OSMGeocoding.inWorld(
    const _point : TGIS_Point
  ) : Boolean ;
  begin
    if ( _point.X >= -180.0 ) and ( _point.X <= 180.0 ) and
       ( _point.Y >= -90.0  ) and ( _point.Y <= 90.0  ) then
      Result := True
    else
      Result := False ;
  end ;


  function TGIS_OSMGeocoding.isWorld(
    const _extent : TGIS_Extent
  ) : Boolean ;
  begin
    if ( _extent.XMin <= -180.0 ) and ( _extent.XMax >= 180.0 ) and
       ( _extent.YMin <= -90.0  ) and ( _extent.YMax >= 90.0  ) then
      Result := True
    else
      Result := False ;
  end ;


  function TGIS_OSMGeocoding.fetchJSON(
    const _url : String ;
    const _rev : Boolean
  ) : TGIS_LayerVector ;
  var
    name : String ;
    capt : String ;
    lres : TGIS_LayerJSON ;
    strm : TGIS_MemoryStream ;
  begin
    if _rev then begin
      name := GIS_LOCAL_RGEOCODING_LAYER_NAME ;
      capt := GIS_LOCAL_RGEOCODING_LAYER_LABEL ;
    end
    else begin
      name := GIS_LOCAL_FGEOCODING_LAYER_NAME ;
      capt := GIS_LOCAL_FGEOCODING_LAYER_LABEL ;
    end ;

    lres := nil ;
    strm := TGIS_MemoryStream.Create ;
    try
      if httpFetch( _url, strm ) then begin
        strm.Position := 0 ;
        lres := TGIS_LayerJSON.Create ;
        lres.Stream := strm ;
        lres.Open ;
        lres.Name := name ;
        lres.Caption := capt ;
        lres.SetCSByEPSG( 4326 ) ;
        lres.Params.Marker.SizeAsText := 'SIZE:8 pt' ;
        lres.Params.Marker.OutlineWidthAsText := 'SIZE:1.5 pt' ;
        lres.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
        lres.Params.Marker.Color := TGIS_Color.White ;
        lres.Params.Marker.OutlineColor := TGIS_Color.Black ;
        lres.Params.Labels.Field := GIS_FIELD_UID ;
        lres.Params.Labels.Allocator := False ;
      end ;
    finally
      FreeObject( strm ) ;
    end ;

    Result := lres ;
  end ;


  function TGIS_OSMGeocoding.&Forward(
    const _name : String
  ) : TGIS_LayerVector ;
  var
    url  : String ;
  begin
    url := httpAddress1 + 'q=' + URLEncode( _name ) +
           '&limit=' + IntToStr( Limit ) + '&lang=en' ;

    if inWorld( BiasPoint ) then
      url := url + '&lat=' + DotFloatToStr( BiasPoint.Y ) +
                   '&lon=' + DotFloatToStr( BiasPoint.X ) +
                   '&location_bias_scale=10' ;

    if not isWorld( RestrictedExtent ) then
      url := url + '&bbox=' + DotFloatToStr( RestrictedExtent.XMin ) + ',' +
                              DotFloatToStr( RestrictedExtent.YMin ) + ',' +
                              DotFloatToStr( RestrictedExtent.XMax ) + ',' +
                              DotFloatToStr( RestrictedExtent.YMax ) ;

    Result := fetchJSON( url, False ) ;
  end ;


  function TGIS_OSMGeocoding.ForwardPoint(
    const _name : String
  ) : TGIS_Point ;
  var
    url  : String ;
    json : TGIS_LayerVector ;
    pt   : TGIS_Point ;
  begin
    Result := GisPoint( GIS_MAX_DOUBLE, GIS_MAX_DOUBLE ) ;

    url := httpAddress1 + 'q=' + URLEncode( _name ) + '&limit=1&lang=en' ;

    if inWorld( BiasPoint ) then
      url := url + '&lat=' + DotFloatToStr( BiasPoint.Y ) +
                   '&lon=' + DotFloatToStr( BiasPoint.X ) +
                   '&location_bias_scale=10' ;

    if not isWorld( RestrictedExtent ) then
      url := url + '&bbox=' + DotFloatToStr( RestrictedExtent.XMin ) + ',' +
                              DotFloatToStr( RestrictedExtent.YMin ) + ',' +
                              DotFloatToStr( RestrictedExtent.XMax ) + ',' +
                              DotFloatToStr( RestrictedExtent.YMax ) ;

    json := fetchJSON( url, False ) ;
    try
      pt := json.GetShape( 1 ).Centroid ;
    finally
      FreeObject( json ) ;
    end ;

    Result := pt ;
  end ;


  function TGIS_OSMGeocoding.Reverse(
    const _loc  : TGIS_Point
  ) : TGIS_LayerVector ;
  var
    url  : String ;
  begin
    url := httpAddress2 +
           'lat=' + DotFloatToStr( _loc.Y ) +
           '&lon=' + DotFloatToStr( _loc.X ) ;

    Result := fetchJSON( url, True ) ;
  end ;


//==============================================================================
// TGIS_OSMRouting
//==============================================================================

  constructor TGIS_OSMRouting.Create ;
  begin
    inherited ;

    FAddress := '' ;
    sDefaultPrefix1 := GIS_LOCAL_DEFAULT_PREFIX_ROUTING ;
    sDefaultPrefix2 := '' ;
    sPrefix1 := GIS_LOCAL_PREFIX_ROUTING ;
    sPrefix2 := '' ;

    FProfile := TGIS_OSMRoutingProfile.Car ;
  end ;


  procedure TGIS_OSMRouting.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_OSMRouting.makeLayer
    : TGIS_LayerVector ;
  var
    lres : TGIS_LayerVector ;
  begin
    lres := TGIS_LayerVector.Create ;
    lres.Name := GIS_LOCAL_ROUTING_LAYER_NAME ;
    lres.Caption := GIS_LOCAL_ROUTING_LAYER_LABEL ;
    lres.SetCSByEPSG( 4326 ) ;
    lres.Open ;
    lres.AddField( GIS_LOCAL_FIELD_TYPE, TGIS_FieldType.String, 0, 0 ) ;

    lres.Params.Query := '( ' + GIS_LOCAL_FIELD_TYPE + ' = '''
                              + GIS_LOCAL_TYPE_FROM + ''' )' ;
    lres.Params.Marker.SizeAsText := 'SIZE:8 pt' ;
    lres.Params.Marker.OutlineWidthAsText := 'SIZE:1.5 pt' ;
    lres.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
    lres.Params.Marker.Color := TGIS_Color.FromRGB( 128, 255, 128 ) ;
    lres.Params.Marker.OutlineColor := TGIS_Color.Black ;

    lres.ParamsList.Add ;

    lres.Params.Query := '( ' + GIS_LOCAL_FIELD_TYPE + ' = '''
                              + GIS_LOCAL_TYPE_THROUGH + ''' )' ;
    lres.Params.Marker.SizeAsText := 'SIZE:8 pt' ;
    lres.Params.Marker.OutlineWidthAsText := 'SIZE:1.5 pt' ;
    lres.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
    lres.Params.Marker.Color := TGIS_Color.White ;
    lres.Params.Marker.OutlineColor := TGIS_Color.Black ;

    lres.ParamsList.Add ;

    lres.Params.Query := '( ' + GIS_LOCAL_FIELD_TYPE + ' = ''' +
                                GIS_LOCAL_TYPE_TO + ''' )' ;
    lres.Params.Marker.SizeAsText := 'SIZE:8 pt' ;
    lres.Params.Marker.OutlineWidthAsText := 'SIZE:1.5 pt' ;
    lres.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
    lres.Params.Marker.Color := TGIS_Color.FromRGB( 255, 128, 128 ) ;
    lres.Params.Marker.OutlineColor := TGIS_Color.Black ;

    lres.ParamsList.Add ;

    lres.Params.Query := '( ' + GIS_LOCAL_FIELD_TYPE + ' = '''
                              + GIS_LOCAL_TYPE_ROUTE + ''' )' ;
    lres.Params.Line.WidthAsText := 'SIZE:3 pt' ;
    lres.Params.Line.OutlineWidthAsText := 'SIZE:1 pt' ;
    lres.Params.Line.Color := TGIS_Color.FromRGB( 0, 192, 255 ) ;
    lres.Params.Line.OutlineColor := TGIS_Color.FromRGB( 0, 128, 255 ) ;

    Result := lres ;
  end ;


  procedure TGIS_OSMRouting.fetchGPX(
    const _layer  : TGIS_LayerVector ;
    const _points : TGIS_PointList
  ) ;
  var
    url  : String ;
    pt   : TGIS_Point ;
    lgpx : TGIS_LayerGPX ;
    flds : TList<String> ;
    strm : TGIS_MemoryStream ;
    {$IFNDEF OXYGENE}
      shp  : TGIS_Shape ;
    {$ENDIF}
    shpc : TGIS_Shape ;
    name : String ;
    i    : Integer ;
  begin
    url := httpAddress1 ;
    for i := 0 to _points.Count - 1 do begin
      pt := _points[i] ;
      url := url + 'point='
             + DotFloatToStr( pt.Y ) + ','
             + DotFloatToStr( pt.X ) + '&' ;
    end ;
    url := url + 'type=gpx&locale=en&elevation=false&profile=' ;
    case Profile of
      TGIS_OSMRoutingProfile.Car  :
        url := url + GIS_LOCAL_ROUTING_PROFILE_CAR ;
      TGIS_OSMRoutingProfile.Bike :
        url := url + GIS_LOCAL_ROUTING_PROFILE_BIKE ;
      TGIS_OSMRoutingProfile.Foot :
        url := url + GIS_LOCAL_ROUTING_PROFILE_FOOT ;
    end ;
    url := url + '&gpx.route=false&gpx.track=true&gpx.waypoints=false' ;

    strm := TGIS_MemoryStream.Create ;
    try
      if httpFetch( url, strm ) then begin
        strm.Position := 0 ;
        lgpx := TGIS_LayerGPX.Create ;
        flds := TList<String>.Create ;
        try
          lgpx.Stream := strm ;
          lgpx.Open ;
          for i := 0 to lgpx.Fields.Count - 1 do begin
            name := Copy( lgpx.Fields[i].Name, StringFirst, 3 ) ;
            if CompareText( name, 'GPX' ) = 0 then
              continue ;
            _layer.AddField(
              lgpx.Fields[i].Name, TGIS_FieldType.String, 0, 0
            ) ;
            flds.Add( lgpx.Fields[i].Name ) ;
          end ;

          for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
              lgpx.Loop( lgpx.Extent, GIS_LOCAL_GPX_TYPE_TRK ) do begin
            shpc := _layer.AddShape( shp ) ;
            shpc.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_ROUTE ) ;
            for i := 0 to flds.Count - 1 do
              shpc.SetField( flds[i], shp.GetField( flds[i] ) );
          end ;
        finally
          FreeObject( lgpx ) ;
          FreeObject( flds ) ;
        end ;

      end ;
    finally
      FreeObject( strm ) ;
    end ;
  end ;


  function TGIS_OSMRouting.Route(
    const _points : TGIS_PointList
  ) : TGIS_LayerVector ;
  var
    lres : TGIS_LayerVector ;
    shp  : TGIS_Shape ;
    i    : Integer ;
  begin
    Result := nil ;

    if ( not assigned( _points ) ) or ( _points.Count = 0 ) then
      exit ;

    lres := makeLayer ;

    Self.fetchGPX( lres, _points ) ;

    for i := 0 to _points.Count - 1 do begin
      shp := lres.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
      shp.AddPart ;
      shp.AddPoint( _points[i] ) ;
      if i = 0 then
        shp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_FROM )
      else
      if i = _points.Count - 1 then
        shp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_TO )
      else
        shp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_THROUGH ) ;
    end ;

    Result := lres ;
  end ;


  function TGIS_OSMRouting.Route(
    const _from : TGIS_Point ;
    const _to   : TGIS_Point
  ) : TGIS_LayerVector ;
  var
    plst : TGIS_PointList ;
    lres : TGIS_LayerVector ;
    shp  : TGIS_Shape ;
  begin
    lres := makeLayer ;

    plst := TGIS_PointList.Create ;
    try
      plst.Add( _from ) ;
      plst.Add( _to ) ;
      Self.fetchGPX( lres, plst ) ;
    finally
      FreeObject( plst ) ;
    end ;

    shp := lres.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    shp.AddPoint( _from ) ;
    shp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_FROM ) ;

    shp := lres.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    shp.AddPoint( _to ) ;
    shp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_TO ) ;

    Result := lres ;
  end ;


  function TGIS_OSMRouting.Route(
    const _names  : TGIS_StringList
  ) : TGIS_LayerVector ;
  var
    ogeo  : TGIS_OSMGeocoding ;
    lres  : TGIS_LayerVector ;
    lmid  : TGIS_LayerVector ;
    ltmp  : TGIS_LayerVector ;
    plst  : TGIS_PointList ;
    shp   : TGIS_Shape ;
    tmp   : TGIS_Shape ;
    bok   : Boolean ;
    i     : Integer ;
  begin
    Result := nil ;

    if ( not assigned( _names ) ) or ( _names.Count < 2 ) then
      exit ;

    bok := True ;
    lmid := TGIS_LayerVector.Create ;
    plst := TGIS_PointList.Create ;
    ogeo := TGIS_OSMGeocoding.Create ;
    try

      lmid.AddField( GIS_LOCAL_FIELD_TYPE, TGIS_FieldType.String, 0, 0 ) ;
      for i := 0 to _names.Count - 1 do begin
        ltmp := ogeo.Forward( _names[i] ) ;
        try
          if ( i = 0 ) and ( assigned( ltmp ) ) then
            lmid.ImportStructure( ltmp ) ;
          if assigned( ltmp ) and ( ltmp.GetLastUid > 0 ) then begin
            shp := ltmp.GetShape( 1 ) ;
            tmp := lmid.AddShape( shp, True ) ;
            if i = 0 then
              tmp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_FROM )
            else
            if i = _names.Count - 1 then
              tmp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_TO )
            else
              tmp.SetField( GIS_LOCAL_FIELD_TYPE, GIS_LOCAL_TYPE_THROUGH ) ;
            plst.Add( shp.Centroid ) ;
          end
          else
            bok := False ;
        finally
          FreeObject( ltmp ) ;
        end ;
      end ;

      lres := makeLayer ;

      if bok then
        Self.fetchGPX( lres, plst ) ;

      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in lmid.Loop do begin
        tmp := lres.AddShape( shp ) ;
        tmp.SetField(
          GIS_LOCAL_FIELD_TYPE, shp.GetField( GIS_LOCAL_FIELD_TYPE )
        ) ;
      end ;

    finally
      FreeObject( lmid ) ;
      FreeObject( plst ) ;
      FreeObject( ogeo ) ;
    end ;

    Result := lres ;
  end ;


  function TGIS_OSMRouting.Route(
    const _from : String ;
    const _to   : String
  ) : TGIS_LayerVector ;
  var
    lres  : TGIS_LayerVector ;
    names : TGIS_StringList ;
  begin
    lres := nil ;

    names := TGIS_StringList.Create ;
    try
      names.Add( _from ) ;
      names.Add( _to ) ;
      lres := Self.Route( names ) ;
    finally
      FreeObject( names ) ;
    end ;

    Result := lres ;
  end ;


//==============================================================================
// TGIS_OSMIsochrone
//==============================================================================

  constructor TGIS_OSMIsochrone.Create ;
  begin
    inherited ;

    FAddress := '' ;
    sDefaultPrefix1 := GIS_LOCAL_DEFAULT_PREFIX_ISOCHRONE ;
    sDefaultPrefix2 := '' ;
    sPrefix1 := GIS_LOCAL_PREFIX_ISOCHRONE ;
    sPrefix2 := '' ;

    FProfile := TGIS_OSMRoutingProfile.Car ;
    FTimeLimit := 600 ;
//?    FDistanceLimit := 1000 ;
    FBuckets := 1 ;
  end ;


  procedure TGIS_OSMIsochrone.doDestroy ;
  begin

    inherited ;
  end ;


  function TGIS_OSMIsochrone.fget_TimeLimit
    : Integer ;
  begin
    Result := FTimeLimit ;
  end ;


  procedure TGIS_OSMIsochrone.fset_TimeLimit(
    const _val : Integer
  ) ;
  begin
    if _val < 1 then
      FTimeLimit := 1
    else
      FTimeLimit := _val ;
  end ;


//?  function TGIS_OSMIsochrone.fget_DistanceLimit
//?    : Integer ;
//?  begin
//?    Result := FDistanceLimit ;
//?  end ;


//?  procedure TGIS_OSMIsochrone.fset_DistanceLimit(
//?    const _val : Integer
//?  ) ;
//?  begin
//?    if _val < 1 then
//?      FDistanceLimit := 1
//?    else
//?      FDistanceLimit := _val ;
//?  end ;


  function TGIS_OSMIsochrone.fget_Buckets
    : Integer ;
  begin
    Result := FBuckets ;
  end ;


  procedure TGIS_OSMIsochrone.fset_Buckets(
    const _val : Integer
  ) ;
  begin
    if _val < 1 then
      FBuckets := 1
    else
      FBuckets := _val ;
  end ;


  function TGIS_OSMIsochrone.fetchJSON(
    const _point : TGIS_Point ;
    const _dist  : Boolean
  ) : TGIS_LayerVector ;
  var
    url  : String ;
    lres : TGIS_LayerVector ;
    ltmp : TGIS_LayerJSON ;
    strm : TGIS_MemoryStream ;
    shp  : TGIS_Shape ;
    i    : Integer ;
  begin
    url := httpAddress1 + 'point='
           + DotFloatToStr( _point.Y ) + ','
           + DotFloatToStr( _point.X ) + '&vehicle=' ;

    case Profile of
      TGIS_OSMRoutingProfile.Car  :
        url := url + GIS_LOCAL_ROUTING_PROFILE_CAR ;
      TGIS_OSMRoutingProfile.Bike :
        url := url + GIS_LOCAL_ROUTING_PROFILE_BIKE ;
      TGIS_OSMRoutingProfile.Foot :
        url := url + GIS_LOCAL_ROUTING_PROFILE_FOOT ;
    end ;

//?    if _dist then
//?      url := url + '&weighting=shortest&distance_limit=' +
//?             IntToStr( DistanceLimit )
//?    else
      url := url + '&weighting=fastest&time_limit=' +
             IntToStr( TimeLimit ) ;

    url := url + '&buckets=' + IntToStr( Buckets ) ;

    lres := nil ;
    strm := TGIS_MemoryStream.Create ;
    try
      if httpFetch( url, strm ) then begin
        strm.Position := 0 ;
        ltmp := TGIS_LayerJSON.Create ;
        try
          ltmp.Stream := strm ;
          ltmp.Open ;

          if ltmp.GetLastUid > 0 then begin

            lres := TGIS_LayerVector.Create ;
            if _dist then begin
              lres.Name := GIS_LOCAL_ISODISTANCE_LAYER_NAME ;
              lres.Caption := GIS_LOCAL_ISODISTANCE_LAYER_LABEL ;
            end
            else begin
              lres.Name := GIS_LOCAL_ISOCHRONE_LAYER_NAME ;
              lres.Caption := GIS_LOCAL_ISOCHRONE_LAYER_LABEL ;
            end ;
            lres.SetCSByEPSG( 4326 ) ;
            lres.Open ;
            if _dist then
              lres.AddField( GIS_RTR_OSM_FIELD_DISTANCE,
                             TGIS_FieldType.Number, 8, 0 )
            else
              lres.AddField( GIS_RTR_OSM_FIELD_TIME,
                             TGIS_FieldType.Number, 8, 0 ) ;

            for i := ltmp.GetLastUid downto 1 do begin
              shp := lres.AddShape( ltmp.GetShape( i ) ) ;
//?              if _dist then
//?                shp.SetField( GIS_LOCAL_FIELD_DISTANCE,
//?                              i*DistanceLimit/Buckets )
//?              else
                shp.SetField( GIS_RTR_OSM_FIELD_TIME,
                              i*TimeLimit/Buckets )
            end ;

            lres.Transparency := 50 ;

            if Buckets = 1 then begin
              lres.Params.Area.Color := TGIS_Color.Lime ;
              lres.Params.Area.OutlineColor := TGIS_Color.Lime ;
            end
            else begin
//?              if _dist then begin
//?                lres.Params.Render.Expression := GIS_RTR_OSM_FIELD_DISTANCE ;
//?                lres.Params.Render.MinVal := 1.0*DistanceLimit/Buckets ;
//?                lres.Params.Render.MaxVal := 1.0*DistanceLimit ;
//?              end
//?              else begin
                lres.Params.Render.Expression := GIS_RTR_OSM_FIELD_TIME ;
                lres.Params.Render.MinVal := 1.0*TimeLimit/Buckets ;
                lres.Params.Render.MaxVal := 1.0*TimeLimit ;
//?              end ;
              lres.Params.Render.StartColor := TGIS_Color.Lime ;
              lres.Params.Render.EndColor := TGIS_Color.Red ;
              lres.Params.Render.Zones := Buckets ;
              lres.Params.Area.Color := TGIS_Color.RenderColor ;
              lres.Params.Area.OutlineColor := TGIS_Color.RenderColor ;
            end ;

          end ;

          shp := lres.CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XY ) ;
          shp.AddPart ;
          shp.AddPoint( _point ) ;

          lres.Params.Marker.SizeAsText := 'SIZE:8 pt' ;
          lres.Params.Marker.OutlineWidthAsText := 'SIZE:1.5 pt' ;
          lres.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
          lres.Params.Marker.Color := TGIS_Color.White ;
          lres.Params.Marker.OutlineColor := TGIS_Color.Black ;

        finally
          FreeObject( ltmp ) ;
        end ;

      end ;

    finally
      FreeObject( strm ) ;
    end ;

    Result := lres ;
  end ;


  function TGIS_OSMIsochrone.Isochrone(
    const _point : TGIS_Point
  ) : TGIS_LayerVector ;
  begin
    Result := Self.fetchJSON( _point, False ) ;
  end ;


  function TGIS_OSMIsochrone.Isochrone(
    const _name  : String
  ) : TGIS_LayerVector ;
  var
    ogeo : TGIS_OSMGeocoding ;
    ltmp : TGIS_LayerVector ;
    shp  : TGIS_Shape ;
    pt   : TGIS_Point ;
  begin
    Result := nil ;

    ogeo := TGIS_OSMGeocoding.Create ;
    ltmp := nil ;
    try
      ogeo.Limit := 1 ;
      ltmp := ogeo.Forward( _name ) ;
      if assigned( ltmp ) and ( ltmp.GetLastUid > 0 ) then begin
        shp := ltmp.GetShape( ltmp.GetLastUid ) ;
        pt := shp.Centroid ;
      end
      else
        exit ;
    finally
      FreeObject( ogeo ) ;
      FreeObject( ltmp ) ;
    end ;

    Result := Self.fetchJSON( pt, False ) ;
  end ;


//?  function TGIS_OSMIsochrone.Isodistance(
//?    const _point : TGIS_Point
//?  ) : TGIS_LayerVector ;
//?  begin
//?    Result := Self.fetchJSON( _point, True )
//?  end ;


//?  function TGIS_OSMIsochrone.Isodistance(
//?    const _name  : String
//?  ) : TGIS_LayerVector ;
//?  var
//?    ogeo : TGIS_OSMGeocoding ;
//?    ltmp : TGIS_LayerVector ;
//?    shp  : TGIS_Shape ;
//?    pt   : TGIS_Point ;
//?  begin
//?    Result := nil ;
//?
//?    ogeo := TGIS_OSMGeocoding.Create ;
//?    try
//?      ogeo.Limit := 1 ;
//?      ltmp := ogeo.Forward( _name ) ;
//?      if assigned( ltmp ) and ( ltmp.GetLastUid > 0 ) then begin
//?        shp := ltmp.GetShape( ltmp.GetLastUid ) ;
//?        pt := shp.Centroid ;
//?      end
//?      else
//?        exit ;
//?    finally
//?      FreeObject( ogeo ) ;
//?      FreeObject( ltmp ) ;
//?    end ;
//?
//?    Result := Self.fetchJSON( pt, True ) ;
//?  end ;


//==================================== END =====================================
end.
