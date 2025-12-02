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
  WMS provider.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileWMS ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileWMS"'}
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
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoXmlSax ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL,
  TatukGIS.RTL.XML ;
{$ENDIF}

type
  /// <summary>
  ///   WMS Layer.
  /// </summary>
  TGIS_WMSLayer = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    public
      /// <summary>
      ///   WMS Layer name.
      /// </summary>
      Name         : String           ;
      /// <summary>
      ///   WMS Layer title.
      /// </summary>
      Title        : String           ;
      /// <summary>
      ///   WMS Layer info.
      /// </summary>
      Info         : String           ;
      /// <summary>
      ///   WMS Layer bounding box in WGS84.
      /// </summary>
      LatLonBBox   : TGIS_Extent      ;
      /// <summary>
      ///   WMS Layer sublayers list.
      /// </summary>
      SubLayers    : TGIS_ObjectList  ;
      /// <summary>
      ///   WMS Layer coordinate system name.
      /// </summary>
      CRS          : String           ;
      /// <summary>
      ///   WMS Layer coordinate system bounding box.
      /// </summary>
      CRSBBox      : TGIS_Extent      ;
      /// <summary>
      ///   Legend url.
      /// </summary>
      Legend       : String           ;
      /// <summary>
      ///   Styles available for a layer.
      /// </summary>
      Style        : String           ;
      /// <summary>
      ///   Min scale denominator of a layer.
      /// </summary>
      MinScaleDenominator : Double    ;
      /// <summary>
      ///   Max scale denominator of a layer.
      /// </summary>
      MaxScaleDenominator : Double    ;
    protected
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create  ;
  end ;

  /// <summary>
  ///   Encapsulation of the WMS file.
  /// </summary>
  TGIS_FileWMS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
   private
     wmsCapabilities : TObject ;
     FError          : String  ;
     FOnPassword     : TGIS_TemplateProducerEvent ;
     {$IFDEF DCC}
       [weak]
     {$ENDIF}
     oViewer         : IGIS_Viewer ;
     oLayer          : TObject ;

     FUserAgent      : String  ;
     FTimeOut        : Integer ;
     FDefaultLayer   : String  ;
     FSelectedSRS    : String  ;
     FForcedSRS      : String  ;
     FAxisOrderIgnored  : Boolean ;
     FAxisOrderReversed : Boolean ;
     FIgnoreInternalURI  : Boolean ;
   protected

     function  fget_ProxyUrl          : String   ;
     procedure fset_ProxyUrl          ( const _value : String ) ;
     function  fget_LastUrl           : String   ;
     function  fget_QueryableLayers   : TStrings ;
     function  fget_ImageFormats      : TStrings ;
     function  fget_AllLayers         : TStrings ;
     function  fget_AllLayersCaptions : TStrings ;
     function  fget_AllSRS            : TStrings ;
     function  fget_ForcedSRS         : String   ;
     procedure fset_ForcedSRS         ( const _value : String ) ;
     function  fget_PredefinedVersion : String   ;
     function  fget_PredefinedFormat  : String   ;
     function  fget_PredefinedStyles  : String   ;
     function  fget_PredefinedLayers  : TStrings ;
     function  fget_DefaultSRS        : String   ;
     function  fget_MaxWidth          : Integer  ;
     function  fget_MaxHeight         : Integer  ;
     function  fget_TransmittedBytes  : Int64    ;
     function  fget_ServiceVersion    : String   ;
     function  fget_ServiceInfo       : String   ;
   private
     function  isAxisReversed         : Boolean  ;

   protected

     /// <inheritdoc/>
     procedure doDestroy       ; override;

   public

     /// <summary>
     ///   Constructor.
     /// </summary>
     /// <param name="_vwr">
     ///   viewer handle
     /// </param>
     /// <param name="_layer">
     ///   layer handle
     /// </param>
     constructor Create      ( const _vwr        : IGIS_Viewer     ;
                               const _layer      : TObject
                             ) ;

     /// <summary>
     ///   Load WMS service.
     /// </summary>
     /// <param name="_path">
     ///   service path/address
     /// </param>
     /// <param name="_xml">
     ///   xml data
     /// </param>
     procedure Load          ( const _path       : String  ;
                               const _xml        : TStream
                             ) ;

     /// <summary>
     ///   Get hierarchy.
     /// </summary>
     /// <param name="_hierarchy">
     ///   list of hierarchy
     /// </param>
     /// <param name="_layers">
     ///   list of layers
     /// </param>
     /// <param name="_categories">
     ///   list of categories
     /// </param>
     procedure GetHierarchy( {$IFDEF OXYGENE}
                               const _hierarchy  : TGIS_Strings ;
                               const _layers     : TGIS_Strings ;
                               const _categories : TGIS_Strings
                             {$ELSE}
                               const _hierarchy  : TStrings    ;
                               const _layers     : TStrings    ;
                               const _categories : TStrings
                             {$ENDIF}
                           ) ;

     /// <summary>
     ///   Get map.
     /// </summary>
     /// <param name="_extent">
     ///   requested extent
     /// </param>
     /// <param name="_format">
     ///   requested format
     /// </param>
     /// <param name="_width">
     ///   requested width
     /// </param>
     /// <param name="_height">
     ///   requested height
     /// </param>
     /// <param name="_layers">
     ///   requested layers
     /// </param>
     /// <param name="_styles">
     ///   requested styles
     /// </param>
     /// <returns>
     ///    http response
     /// </returns>
     function  GetMap      ( const _extent     : TGIS_Extent ;
                             const _format     : String      ;
                             const _width      : Integer     ;
                             const _height     : Integer     ;
                             const _layers     : {$IFDEF OXYGENE}
                                                   TGIS_Strings ;
                                                 {$ELSE}
                                                   TStrings ;
                                                 {$ENDIF}
                             const _styles     : {$IFDEF OXYGENE}
                                                   TGIS_Strings
                                                 {$ELSE}
                                                   TStrings
                                                 {$ENDIF}
                           ) : TGIS_HttpResponse ;

     /// <summary>
     ///   Get feature info from WMS server.
     /// </summary>
     /// <param name="_extent">
     ///   requested extent
     /// </param>
     /// <param name="_format">
     ///   requested format
     /// </param>
     /// <param name="_width">
     ///   requested width
     /// </param>
     /// <param name="_height">
     ///   requested height
     /// </param>
     /// <param name="_layers">
     ///   requested layers
     /// </param>
     /// <param name="_infoFmt">
     ///   requested info format
     /// </param>
     /// <param name="_feaCnt">
     ///   requested feature count
     /// </param>
     /// <param name="_i">
     ///   requested position X
     /// </param>
     /// <param name="_j">
     ///   requested position Y
     /// </param>
     /// <returns>
     ///    http response
     /// </returns>
     function GetFeatureInfo
                             ( const _extent     : TGIS_Extent ;
                               const _format     : String      ;
                               const _width      : Integer     ;
                               const _height     : Integer     ;
                               const _layers     : {$IFDEF OXYGENE}
                                                     TGIS_Strings;
                                                   {$ELSE}
                                                     TStrings ;
                                                   {$ENDIF}
                               const _infoFmt    : String      ;
                               const _feaCnt     : Integer     ;
                               const _i          : Integer     ;
                               const _j          : Integer
                             ) : TGIS_HttpResponse ;

     /// <summary>
     ///   Get extent for SRS.
     /// </summary>
     /// <param name="_srs">
     ///   SRS code
     /// </param>
     /// <returns>
     ///    lary extent for coordinate system.
     /// </returns>
     function  GetExtent     ( const _srs         : String
                             ) : TGIS_Extent ;

     /// <summary>
     ///   Verify if SRS is supported by the layer.
     /// </summary>
     /// <param name="_srs">
     ///   SRS code
     /// </param>
     /// <returns>
     ///    True if coordinate system is supported.
     /// </returns>
     function  VerifySRS     ( const _srs         : String
                             ) : Boolean ;

     /// <summary>
     ///   Get all layers with sublayers.
     /// </summary>
     /// <returns>
     ///    Layer object.
     /// </returns>
     function  GetLayers     : TGIS_WMSLayer ;

     /// <summary>
     ///   Find a layer by name.
     /// </summary>
     /// <param name="_name">
     ///   layer name
     /// </param>
     /// <param name="_cs">
     ///   CS to match from supported list
     /// </param>
     /// <returns>
     ///    Layer object.
     /// </returns>
     function  FindLayer     ( const _name        : String ;
                               const _cs          : TGIS_CSCoordinateSystem
                             ) : TGIS_WMSLayer ;

     /// <summary>
     ///   Build url from a path and a query parameters.
     /// </summary>
     /// <param name="_query">
     ///   query text
     /// </param>
     /// <param name="_path">
     ///   url path
     /// </param>
     /// <returns>
     ///    Url text
     /// </returns>
     function  MakeUrl       ( const _query       : String         ;
                               const _path        : String
                             ) : String ;

      /// <summary>
      ///   Get layer legend image.
      /// </summary>
      /// <param name="_layer">
      ///   layer name
      /// </param>
      /// <returns>
      ///   http response with image
      /// </returns>
      function    GetLegendImage  ( const _layer : String
                                   ) : TGIS_HttpResponse ;
   public

      /// <summary>
      ///   Last Url fetched by client.
      /// </summary>
      property LastUrl
         : String
         read fget_LastUrl      ;

      /// <summary>
      ///   Service version.
      /// </summary>
      property ServiceVersion
         : String
         read fget_ServiceVersion ;

      /// <summary>
      ///   Service info.
      /// </summary>
      property ServiceInfo
         : String
         read fget_ServiceInfo ;

      /// <summary>
      ///   List of all layers.
      /// </summary>
      property ImageFormats :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_ImageFormats  ;

      /// <summary>
      ///   List of queryable layers.
      /// </summary>
      property QueryableLayers :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_QueryableLayers  ;

      /// <summary>
      ///   List of all layers.
      /// </summary>
      property AllLayers :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_AllLayers ;

      /// <summary>
      ///   List of all srs.
      /// </summary>
      property AllSRS  :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_AllSRS ;

      /// <summary>
      ///   List of all layers.
      /// </summary>
      property AllLayersCaptions :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_AllLayersCaptions ;

      /// <summary>
      ///   Proxy URL as for ESRI proxy.ashx.
      /// </summary>
      property ProxyUrl            : String     read  fget_ProxyUrl
                                                write fset_ProxyUrl         ;

      /// <summary>
      ///   Selected coordinate system.
      /// </summary>
      property SelectedSRS         : String     read  FSelectedSRS
                                                write FSelectedSRS         ;

      /// <summary>
      ///   Forced coordinate system.
      /// </summary>
      property ForcedSRS         : String       read  fget_ForcedSRS
                                                write fset_ForcedSRS         ;


      /// <summary>
      ///   Default coordinate system.
      /// </summary>
      property DefaultSRS          : String     read  fget_DefaultSRS      ;

      /// <summary>
      ///   Version predefined by original URL.
      /// </summary>
      property PredefinedVersion  : String      read fget_PredefinedVersion ;

      /// <summary>
      ///   Format predefined by original URL.
      /// </summary>
      property PredefinedFormat   : String      read fget_PredefinedFormat  ;

      /// <summary>
      ///   Styles predefined by original URL.
      /// </summary>
      property PredefinedStyles   : String      read fget_PredefinedStyles  ;

      /// <summary>
      ///   Layers predefined by original URL.
      /// </summary>
      property PredefinedLayers :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_PredefinedLayers  ;

      /// <summary>
      ///   Error message.
      /// </summary>
      property Error              : String  read FError ;

      /// <summary>
      ///   Maximum requested map width.
      /// </summary>
      property MaxWidth           : Integer read fget_MaxWidth ;

      /// <summary>
      ///   Maximum requested map height.
      /// </summary>
      property MaxHeight          : Integer read fget_MaxHeight ;

      /// <summary>
      ///   User agent name.
      /// </summary>
      property UserAgent          : String  read  FUserAgent
                                            write FUserAgent ;

      /// <summary>
      ///   Connection timeout.
      /// </summary>
      property TimeOut            : Integer read  FTimeOut
                                            write FTimeOut ;

      /// <summary>
      ///   Default layer name.
      /// </summary>
      property DefaultLayer       : String  read  FDefaultLayer ;

      /// <summary>
      ///   Byte counter.
      /// </summary>
      property TransmittedBytes   : Int64   read fget_TransmittedBytes;

      /// <summary>
      ///   Reverse coordinates of axis.
      /// </summary>
      property AxisOrderReversed  : Boolean read  FAxisOrderReversed
                                            write FAxisOrderReversed ;

      /// <summary>
      ///   Ignore axis order reciognition from the coordinate system.
      /// </summary>
      property AxisOrderIgnored   : Boolean read  FAxisOrderIgnored
                                            write FAxisOrderIgnored ;

      /// <summary>
      ///   ignore internal URI for requests from Capabilities.
      /// </summary>
      property IgnoreInternalURI  : Boolean read  FIgnoreInternalURI
                                            write FIgnoreInternalURI ;
   published //events
      /// <event/>
      /// <summary>
      ///   Password event.
      /// </summary>
      property PasswordEvent      : TGIS_TemplateProducerEvent
                                            read  FOnPassword
                                            write FOnPassword ;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,
    System.Variants,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoXmlDoc ;
{$ENDIF}

const
  WMS_VERSION            = '1.1.0' ;
  WMS_VERSION_FORCED     = '1.1.0' ;
  CLIENT_NAME            = 'WMSLayer/2.0 (TatukGIS)' ;
  CLIENT_TIME_OUT        = 40000 ;
  MAX_WIDTH              = 4096 ;
  MAX_HEIGHT             = 4096 ;
  DEF_WIDTH              = 1999 ;
  DEF_HEIGHT             = 1999 ;
  WMS_WGS84DD            = 'WGS84(DD)' ;
  WMS_CRS84              = 'CRS:84' ;

type

  { Bounding box.
  }
  T_wmsBoundingBox = class
    boundingBox : TGIS_Extent ;
    srs         : String      ;
  end ;

  { Legend.
  }
  T_wmsLayerLegend = class
    format : String ;
    url    : String ;
  end ;

  { Layer.
  }
  T_wmsLayer = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      level        : Integer          ;
      parent       : T_wmsLayer       ;
      isqueryable  : Boolean          ;
      title        : String           ;
      info         : String           ;
      name         : String           ;
      srs          : TGIS_StringList  ;
      latLonBndBox : TGIS_Extent      ;
      subLayers    : TGIS_ObjectList  ;
      boundingBox  : TGIS_ObjectList  ;
      legend       : T_wmsLayerLegend ;
      styles       : TGIS_StringList  ;
      minScale     : Double           ;
      maxScale     : Double           ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create  ;
  end ;

  { WMS capabilities.
  }
  T_wmsCapabilities = class( TGIS_ObjectDisposable )
   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
     xDoc              : IXMLDocument ;

     sProxyUrl         : String ;
     oldGet            : String ;
     sTitle            : String ;
     sAbstract         : String ;
     sFees             : String ;
     sAccess           : String ;
     iMaxWidth         : Integer ;
     iMaxHeight        : Integer ;
     oImageFormats     : TGIS_StringList ;
     oFeaFormat        : TGIS_StringList ;
     sVersion          : String ;
     sPath             : String ;
     sError            : String ;
     {$IFDEF DCC}
       [weak]
     {$ENDIF}
     oViewer           : IGIS_Viewer ;
     oLayer            : TObject ;
     sPredefinedVersion: String ;
     sPredefinedFormat : String ;
     sPredefinedParams : String ;
     sPredefinedBBox   : String ;
     ePredefinedBBox   : TGIS_Extent ;
     sPredefinedStyles : String ;
     oPredefinedLayers : TGIS_StringList ;
     sPredefinedSRS    : String ;
     sPredefinedInfoFormat : String ;
     sLastUrl          : String ;
     sSRSName          : String ;
     sUserName         : String ;
     sUserPass         : String ;
     sUserAgent        : String ;
     iTimeOut          : Integer ;
     iBytes            : Int64;
     bInverseXY1       : Boolean ;
   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
     oLayersHierarchy   : T_wmsLayer ;
     oQueryableLayers   : TGIS_StringList ;
     oAllLayers         : TGIS_StringList ;
     oAllSRS            : TGIS_StringList ;
     oAllLayersCaptions : TGIS_StringList ;
     oGetMapMethods     : TGIS_StringList ;
     oGetFeatureMethods : TGIS_StringList ;
     FOnPassword        : TGIS_TemplateProducerEvent ;

   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

     /// <summary>
     ///   Get element value from xml node.
     /// </summary>
     /// <param name="_node">
     ///   root node handle
     /// </param>
     /// <param name="_key">
     ///   node name
     /// </param>
     /// <returns>
     ///   node value
     /// </returns>
     function  getElementValue    ( const _node       : IXMLNode       ;
                                    const _key        : String
                                  ) : String ;

     /// <summary>
     ///   Get attribute value from xml node.
     /// </summary>
     /// <param name="_node">
     ///   node handle
     /// </param>
     /// <param name="_key">
     ///   attribute name
     /// </param>
     /// <returns>
     ///   attribute value
     /// </returns>
     function  getAttributeValue  ( const _node       : IXMLNode       ;
                                    const _key        : String
                                  ) : OleVariant ;

     /// <summary>
     ///   Form url from query and path.
     /// </summary>
     /// <param name="_query">
     ///   query string
     /// </param>
     /// <param name="_path">
     ///   path
     /// </param>
     /// <returns>
     ///   formatted url
     /// </returns>
     function  formUrl            ( const _query      : String         ;
                                    const _path       : String
                                  ) : String ;

     /// <summary>
     ///   Convert layer names into canonical names.
     /// </summary>
     /// <param name="_lst">
     ///   layer list
     /// </param>
     function  canonicalNames     ( const _lst        : TStrings
                                  ) : String ;

     /// <summary>
     ///   Fetch data from http address.
     /// </summary>
     /// <param name="_url">
     ///   http address
     /// </param>
     /// <param name="_user">
     ///   user name required for authentication
     /// </param>
     /// <param name="_pass">
     ///   user password required for authentication
     /// </param>
     function  fetchHttp          ( const _url        : String         ;
                                    const _user       : String         ;
                                    const _pass       : String
                                  ) : TGIS_HttpResponse ;
     {$IFDEF OXYGENE}
       /// <summary>
       ///   Busy event for fetching the data
       /// </summary>
       procedure doBusy           ( const _sender    : Object ;
                                    const _e         : TGIS_BusyEventArgs
                                  ) ;
     {$ELSE}
       /// <summary>
       ///   Busy event for fetching the data
       /// </summary>
       procedure doBusy           (       _sender    : TObject ;
                                          _pos, _end : Integer ;
                                    var   _abort     : Boolean
                                  ) ;
     {$ENDIF}
   protected

     /// <summary>
     ///   Load layers.
     /// </summary>
     /// <param name="_xml_layer">
     ///   layers xml node
     /// </param>
     /// <param name="_parent">
     ///   parent layer handle
     /// </param>
     procedure loadLayers         ( const _xml_layer  : IXMLNode       ;
                                    const _parent     : T_wmsLayer
                                  ) ;

     /// <summary>
     ///   Load online resources.
     /// </summary>
     /// <param name="_xmlList">
     ///   xml node
     /// </param>
     /// <param name="_list">
     ///   _list
     /// </param>
     procedure loadOnlineResource ( const _xmlList    : IXMLNode       ;
                                    const _list       : TStrings
                                  ) ;

     /// <summary>
     ///   Process tokens for embedded password tokens.
     /// </summary>
     /// <result>
     ///   Expanded value of a token or a token name itself if not handled
     /// </result>
     function  passwordCallBack   ( const _token : String
                                   ) : String ;
   protected

     /// <summary>
     ///   Destructor
     /// </summary>
     procedure doDestroy            ; override;
   public
     /// <summary>
     ///   Constructor
     /// </summary>
     constructor Create           ( const _vwr        : IGIS_Viewer    ;
                                    const _layer      : TObject
                                  ) ;

     /// <summary>
     ///   Process tokens in a SQLParameters property.
     /// </summary>
     /// <param name="_xml">
     ///   XML data
     /// </param>
     procedure Parse              ( const _xml        : TStream
                                  ) ;

     /// <summary>
     ///   Parse predefined settings.
     /// </summary>
     procedure ParsePredefined      ;

     /// <summary>
     ///   Parse predefined settings.
     /// </summary>
     /// <param name="_path">
     ///   server path/address
     /// </param>
     procedure GetCapabilities    ( const _path       : String
                                  ) ;

     /// <summary>
     ///   Get map from WMS service.
     /// </summary>
     /// <param name="_path">
     ///   server path/address
     /// </param>
     /// <param name="_version">
     ///   service version
     /// </param>
     /// <param name="_extent">
     ///   requested extent
     /// </param>
     /// <param name="_format">
     ///   requested format
     /// </param>
     /// <param name="_srs">
     ///   requested SRS
     /// </param>
     /// <param name="_width">
     ///   requested width
     /// </param>
     /// <param name="_height">
     ///   requested height
     /// </param>
     /// <param name="_layers">
     ///   requested layers
     /// </param>
     /// <param name="_ignoreURI">
     ///   ignore internal URI for requests from Capabilities
     /// </param>
     /// <param name="_axisReversed">
     ///   true, if coordinate system has axis order other then EN
     /// </param>
     /// <returns>
     ///   the data from the server
     /// </returns>
     function  GetMap             ( const _path         : String         ;
                                    const _version      : String         ;
                                    const _extent       : TGIS_Extent    ;
                                    const _format       : String         ;
                                    const _srs          : String         ;
                                    const _width        : Integer        ;
                                    const _height       : Integer        ;
                                    const _styles       : String         ;
                                    const _layers       : String         ;
                                    const _ignoreURI    : Boolean        ;
                                    const _axisReversed : Boolean
                                  ) : TGIS_HttpResponse ;

     /// <summary>
     ///   Get feature info from WMS server.
     /// </summary>
     /// <param name="_path">
     ///   server path/address
     /// </param>
     /// <param name="_version">
     ///   service version
     /// </param>
     /// <param name="_extent">
     ///   requested extent
     /// </param>
     /// <param name="_format">
     ///   requested format
     /// </param>
     /// <param name="_srs">
     ///   requested SRS
     /// </param>
     /// <param name="_width">
     ///   requested width
     /// </param>
     /// <param name="_height">
     ///   requested height
     /// </param>
     /// <param name="_layers">
     ///   requested layers
     /// </param>
     /// <param name="_info_fmt">
     ///   requested info format
     /// </param>
     /// <param name="_fea_cnt">
     ///   requested feature count
     /// </param>
     /// <param name="_i">
     ///   requested position X
     /// </param>
     /// <param name="_j">
     ///   requested position Y
     /// </param>
     /// <param name="_ignoreURI">
     ///   ignore internal URI for requests from Capabilities
     /// </param>
     /// <param name="_axisReversed">
     ///   true, if coordinate system has axis order other then EN
     /// </param>
     function  GetFeatureInfo     ( const _path         : String         ;
                                    const _version      : String         ;
                                    const _extent       : TGIS_Extent    ;
                                    const _format       : String         ;
                                    const _srs          : String         ;
                                    const _width        : Integer        ;
                                    const _height       : Integer        ;
                                    const _layers       : String         ;
                                    const _qlayers      : String         ;
                                    const _info_fmt     : String         ;
                                    const _fea_cnt      : Integer        ;
                                    const _i            : Integer        ;
                                    const _j            : Integer        ;
                                    const _ignoreURI    : Boolean        ;
                                    const _axisReversed : Boolean
                                  ) : TGIS_HttpResponse ;

     /// <summary>
     ///   Test path for wms flags.
     /// </summary>
     /// <param name="_path">
     ///   path/address string
     /// </param>
     /// <param name="_has_getcap">
     ///   flag
     /// </param>
     /// <param name="_has_getmap">
     ///   flag
     /// </param>
     procedure TestPath           ( const _path       : String         ;
                                    var   _has_getcap : Boolean        ;
                                    var   _has_getmap : Boolean
                                  ) ;

     /// <summary>
     ///   Dump all layers with hierarchy.
     /// </summary>
     /// <param name="_layer">
     ///   root layer
     /// </param>
     /// <param name="_list">
     ///   list of layers
     /// </param>
     /// <param name="_hier">
     ///   hierarchy list
     /// </param>
     /// <param name="_cat">
     ///   category list
     /// </param>
     procedure Dump               ( const _layer      : T_wmsLayer     ;
                                    const _list       : TStrings       ;
                                    const _hier       : TStrings       ;
                                    const _cat        : TStrings
                                  ) ;

     /// <summary>
     ///   Is a layer queryable.
     /// </summary>
     /// <param name="_layer">
     ///   layer root
     /// </param>
     /// <param name="_name">
     ///   layer name
     /// </param>
     /// <returns>
     ///   True if a layer is queryable
     /// </returns>
     function  IsLayerQueryAble   ( const _layer      : T_wmsLayer     ;
                                    const _name       : String
                                  ) : Boolean ;

     /// <summary>
     ///   Get layer hierarchy path.
     /// </summary>
     /// <param name="_layer">
     ///   layer root
     /// </param>
     /// <param name="_name">
     ///   layer name
     /// </param>
     /// <param name="_title">
     ///   layer title
     /// </param>
     /// <returns>
     ///   layer path
     /// </returns>
     function  GetLayerPath       ( const _layer      : T_wmsLayer     ;
                                    const _name       : String         ;
                                    const _title      : String
                                  ) : String ;

     /// <summary>
     ///   Find layer by name.
     /// </summary>
     /// <param name="_layer">
     ///   layer root
     /// </param>
     /// <param name="_name">
     ///   layer name
     /// </param>
     /// <returns>
     ///   found layer
     /// </returns>
     function  FindLayerByName    ( const _layer      : T_wmsLayer     ;
                                    const _name       : String
                                  ) : T_wmsLayer ;

     /// <summary>
     ///   Get maximum lat lon bounding box.
     /// </summary>
     /// <param name="_layer">
     ///   layer root
     /// </param>
     /// <returns>
     ///   extent
     /// </returns>
     function  GetMaxLatLonBndBox ( const _layer      : T_wmsLayer
                                  ) : TGIS_Extent ;

     /// <summary>
     ///   Get maximum bounding box.
     /// </summary>
     /// <param name="_layer">
     ///   layer root
     /// </param>
     /// <param name="_srs">
     ///   bounding box srs
     /// </param>
     /// <returns>
     ///   bounding box extent
     /// </returns>
     function  GetMaxBndBox       ( const _layer      : T_wmsLayer     ;
                                    const _srs        : String
                                  ) : TGIS_Extent ;

     /// <summary>
     ///   Find default layer.
     /// </summary>
     /// <returns>
     ///   layer name
     /// </returns>
     function  FindDefaultLayer   : String ;
   public
     property LastUrl           : String      read sLastUrl           ;
     property ImageFormats      : TGIS_StringList read oImageFormats  ;
     property FeaFormat         : TGIS_StringList read oFeaFormat     ;
     property Version           : String      read sVersion           ;
     property MaxWidth          : Integer     read iMaxWidth          ;
     property MaxHeight         : Integer     read iMaxHeight         ;
     property Path              : String      read sPath              ;
     property Error             : String      read sError             ;
     property PredefinedVersion : String      read sPredefinedVersion ;
     property PredefinedFormat  : String      read sPredefinedFormat  ;
     property PredefinedInfoFormat : String   read sPredefinedInfoFormat  ;
     property PredefinedStyles  : String      read sPredefinedStyles  ;
     property PredefinedLayers  : TGIS_StringList read oPredefinedLayers ;
     property PredefinedSRS     : String      read sPredefinedSRS     ;
     property Layer             : T_wmsLayer  read oLayersHierarchy   ;
     property QueryableLayers   : TGIS_StringList read oQueryableLayers ;
     property AllLayers         : TGIS_StringList read oAllLayers     ;
     property AllLayersCaptions : TGIS_StringList read oAllLayersCaptions ;
     property AllSRS            : TGIS_StringList read oAllSRS        ;
     property UserAgent         : String      read sUserAgent write sUserAgent ;
     property ProxyUrl          : String      read sProxyUrl  write sProxyUrl  ;
     property TimeOut           : Integer     read iTimeOut   write iTimeOut ;
     property TransmittedBytes  : Int64       read iBytes             ;
     property InverseXY1         : Boolean     read bInverseXY1 write bInverseXY1 ;
   end ;

//==============================================================================
// T_wmsCapabilities
//==============================================================================

  constructor T_wmsCapabilities.Create(
    const _vwr    : IGIS_Viewer ;
    const _layer  : TObject
  ) ;
  begin
    inherited Create ;

    oViewer := _vwr ;
    oLayer  := _layer ;

    xDoc := TGIS_XMLDocument.Create ;
    xDoc.ParseOptions := [ TParseOption.poPreserveWhiteSpace ] ;

    oAllLayers          := TGIS_StringList.Create ;
    oAllLayersCaptions  := TGIS_StringList.Create ;
    oQueryableLayers    := TGIS_StringList.Create ;
    oPredefinedLayers   := TGIS_StringList.Create ;
    oAllSRS             := TGIS_StringList.Create ;
    oAllSRS.Duplicates  := TDuplicates.dupIgnore ;
    oAllSRS.Sorted      := True ;
    oImageFormats       := TGIS_StringList.Create ;
    sSRSName            := 'SRS' ;
    iBytes              := 0 ;
    bInverseXY1         := False ;

    oLayersHierarchy        := T_wmsLayer.Create ;
    oLayersHierarchy.level  := -1 ;
    oLayersHierarchy.parent := nil ;
    oLayersHierarchy.name   := 'WMS Layers' ;
  end ;

  procedure T_wmsCapabilities.doDestroy;
  begin
    FreeObject( oImageFormats       ) ;
    FreeObject( oFeaFormat          ) ;
    FreeObject( oLayersHierarchy    ) ;
    FreeObject( oQueryableLayers    ) ;
    FreeObject( oAllLayers          ) ;
    FreeObject( oAllLayersCaptions  ) ;
    FreeObject( oGetMapMethods      ) ;
    FreeObject( oGetFeatureMethods  ) ;
    FreeObject( oPredefinedLayers   ) ;
    FreeObject( oAllSRS             ) ;

    FreeObject( xDoc ) ;

    inherited ;
  end ;

  function T_wmsCapabilities.canonicalNames(
    const _lst : TStrings
  ) : String ;
  var
    i,j : Integer ;
    str : String  ;
    c   : {$IFDEF JAVA} SByte
          {$ELSE}       Byte
          {$ENDIF}      ;
    arb : TBytes  ;
  begin
    for i := 0 to _lst.Count - 1 do begin
      str := _lst[ i ] ;
      if not IsStringEmpty( Result ) then
        Result := Result + ',' ;

      arb := TEncoding.UTF8.GetBytes(str) ;

      for j := low(arb) to high(arb) do begin
        c := arb[ j ] ;

        case Char(c) of
          // RFC 3986
          '_', '-', '.', '~',
          'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
          'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
          'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
          'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
          '1', '2', '3', '4', '5', '6', '7', '8', '9' :
            Result := Result + Char(c)
          else
            Result := Result + Format( '%%%x', [ c ] ) ;
        end;
      end ;
    end ;
  end ;

  procedure T_wmsCapabilities.Parse(
    const _xml : TStream
  ) ;
  var
    root : IXMLNode ;
    list : IXMLNodeList ;
    node : IXMLNode ;
    i    : Integer ;
    val  : String ;
  begin
    if _xml <> nil then
      xDoc.LoadFromStream( _xml ) ;

    root := xDoc.DocumentElement ;

    if root = nil then exit ;

    node      := root.ChildNodes.FindNode( 'Service' ) ;
    if assigned( node ) then begin
      sTitle     := getElementValue( node, 'Title' ) ;
      sAbstract  := getElementValue( node, 'Abstract' ) ;
      sFees      := getElementValue( node, 'Fees' ) ;
      sAccess    := getElementValue( node, 'AccessConstraints' ) ;
      val        := getElementValue( node, 'MaxWidth'  ) ;
      if IsStringEmpty( val ) then
        iMaxWidth := DEF_WIDTH
      else
        iMaxWidth := Min( StrToInt( val ), MAX_WIDTH ) ;

      val := getElementValue( node, 'MaxHeight' ) ;
      if IsStringEmpty( val ) then
        iMaxHeight  := DEF_WIDTH
      else
        iMaxHeight := Min( StrToInt( val ), MAX_HEIGHT ) ;

      oLayersHierarchy.name := getElementValue( node, 'Name' ) ;
      oLayersHierarchy.title := sTitle ;
      oLayersHierarchy.info  := sAbstract ;

      sError := '' ;
    end
    else begin
      // probably a server error
      sError := getElementValue( root, 'ServiceException' ) ;

      if IsStringEmpty( sError ) then
        sError := 'Unrecognized service type' ;
      exit ;
    end;

    sVersion  := VarToString( getAttributeValue( root, 'version' ) ) ;
    // for version > 1.3 CRS should be used
    if sVersion >= '1.3.0' then
      sSRSName := 'CRS' ;

    list := nil ;
    node := root.ChildNodes[ 'Capability' ]
                  .ChildNodes[ 'Request' ]
                    .ChildNodes[ 'GetMap' ] ;
    if assigned( node ) then
      list := node.ChildNodes ;

    if not assigned( oImageFormats ) then
      oImageFormats := TGIS_StringList.Create ;
    if assigned( list ) then begin
      for i := 0 to list.Count - 1 do begin
        if list.Nodes[ i ].NodeName = 'Format' then
          oImageFormats.Add( list.Nodes[ i ].Text ) ;
      end ;
    end ;

    list := nil ;
    node := root.ChildNodes[ 'Capability' ]
                  .ChildNodes[ 'Request' ]
                     .ChildNodes[ 'GetFeatureInfo' ] ;
    if assigned( node ) then
      list := node.ChildNodes ;

    oFeaFormat := TGIS_StringList.Create ;
    if assigned( list ) then begin
      for i := 0 to list.Count - 1 do begin
        if list.Nodes[ i ].NodeName = 'Format' then
          oFeaFormat.Add( list.Nodes[ i ].Text ) ;
      end ;
    end ;

    list := root.ChildNodes[ 'Capability' ].ChildNodes ;
    for i := 0 to list.Count - 1 do
      if list.Nodes[ i ].NodeName = 'Layer' then
        loadLayers( list.Nodes[ i ], nil ) ;

    oGetMapMethods := TGIS_StringList.Create ;
    loadOnlineResource( root.ChildNodes[ 'Capability' ]
                              .ChildNodes[ 'Request' ]
                                .ChildNodes[ 'GetMap' ],
                        oGetMapMethods
                       ) ;

    oGetFeatureMethods := TGIS_StringList.Create ;
    loadOnlineResource( root.ChildNodes[ 'Capability' ]
                              .ChildNodes[ 'Request' ]
                                .ChildNodes[ 'GetFeatureInfo' ],
                        oGetFeatureMethods
                      ) ;
  end ;

  procedure T_wmsCapabilities.ParsePredefined ;
  var
    i      : Integer ;
    wlayer : T_wmsLayer ;
    bbox   : T_wmsBoundingBox ;
  begin
    sVersion  := sPredefinedVersion ;

    if sVersion >= '1.3.0' then
      sSRSName := 'CRS' ;

    if not assigned( oImageFormats ) then
      oImageFormats := TGIS_StringList.Create ;
    if not IsStringEmpty( sPredefinedFormat ) then
      oImageFormats.Add( sPredefinedFormat )
    else
      oImageFormats.Add( 'image/png' ) ;

    oAllLayers.Assign( oPredefinedLayers ) ;
    oAllLayersCaptions.Assign( oPredefinedLayers ) ;

    if not assigned( oGetMapMethods ) then
      oGetMapMethods     := TGIS_StringList.Create ;
    if not assigned( oGetFeatureMethods ) then
      oGetFeatureMethods := TGIS_StringList.Create ;

    iMaxWidth  := DEF_WIDTH ;
    iMaxHeight := DEF_WIDTH  ;

    for i := 0 to oAllLayers.Count-1 do begin
      wlayer := T_wmsLayer.Create ;
      wlayer.level        := 0 ;
      wlayer.isqueryable  := True ;
      wlayer.title        := oAllLayers[i] ;
      wlayer.name         := oAllLayers[i] ;
      wlayer.latLonBndBox := ePredefinedBBox ;
      bbox := T_wmsBoundingBox.Create ;
      bbox.boundingBox := ePredefinedBBox ;
      bbox.srs := sPredefinedSRS ;
      wlayer.boundingBox.Add( bbox ) ;
      wlayer.srs.Add( sPredefinedSRS ) ;
      oLayersHierarchy.subLayers.Add( wlayer ) ;
    end ;
  end ;

  function T_wmsCapabilities.passwordCallBack( const _token : String
                                               ) : String ;
  begin
    if assigned( FOnPassword ) then
      {$IFDEF OXYGENE}
        Result := FOnPassword( oLayer,
                               TGIS_TemplateProducerEventArgs.Create( _token )
                             )
      {$ELSE}
        Result := FOnPassword( oLayer, _token )
      {$ENDIF}
    else
      Result := _token ;
  end ;

  procedure T_wmsCapabilities.GetCapabilities(
    const _path: String
  ) ;
  var
    stmp        : String  ;
    sservice    : String  ;
    scapversion : String  ;
    query       : String  ;
    has_getcap  : Boolean ;
    has_getmap  : Boolean ;
    tkn         : TGIS_Tokenizer ;
    len         : Integer ;
    r           : TGIS_HttpResponse ;
    {$IFNDEF OXYGENE}
      str       : String  ;
    {$ENDIF}
  begin
    sPath := _path ;

    sservice           := URLGetAndDeleteParameterValue( sPath, 'SERVICE'     ) ;
                          URLGetAndDeleteParameterValue( sPath, 'REQUEST'     ) ;
    sPredefinedVersion := URLGetAndDeleteParameterValue( sPath, 'VERSION'     ) ;
    sPredefinedFormat  := URLGetAndDeleteParameterValue( sPath, 'FORMAT'      ) ;
                          URLGetAndDeleteParameterValue( sPath, 'MODE'        ) ;
                          URLGetAndDeleteParameterValue( sPath, 'BGCOLOR'     ) ;
                          URLGetAndDeleteParameterValue( sPath, 'WIDTH'       ) ;
                          URLGetAndDeleteParameterValue( sPath, 'HEIGHT'      ) ;

    sPredefinedInfoFormat := URLGetAndDeleteParameterValue( sPath, 'INFO_FORMAT' ) ;
    sPredefinedBBox    := URLGetAndDeleteParameterValue( sPath, 'BBOX'        ) ;
    ePredefinedBBox    := GisNoWorld ;
    if not IsStringEmpty( sPredefinedBBox ) then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( sPredefinedBBox, [','], False );
        try
          if tkn.Result.Count > 3 then begin
            ePredefinedBBox.XMin := DotStrToFloat( tkn.Result[0] ) ;
            ePredefinedBBox.YMin := DotStrToFloat( tkn.Result[1] ) ;
            ePredefinedBBox.XMax := DotStrToFloat( tkn.Result[2] ) ;
            ePredefinedBBox.YMax := DotStrToFloat( tkn.Result[3] ) ;
          end
        except
          ePredefinedBBox := GisNoWorld ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

    sPredefinedSRS     := URLGetAndDeleteParameterValue( sPath, 'SRS'         ) ;
    if IsStringEmpty( sPredefinedSRS ) then
      sPredefinedSRS   := URLGetAndDeleteParameterValue( sPath, 'CRS'         ) ;
                          URLGetAndDeleteParameterValue( sPath, 'TRANSPARENT' ) ;
    sPredefinedStyles  := URLGetAndDeleteParameterValue( sPath, 'STYLES'      ) ;
    stmp               := URLGetAndDeleteParameterValue( sPath, 'LAYERS'      ) ;
    sUserName          := URLGetAndDeleteParameterValue( sPath, 'USER'        ) ;
    sUserPass          := URLGetAndDeleteParameterValue( sPath, 'PASS'        ) ;
    if IsStringEmpty( sUserPass ) then
      sUserPass := URLGetAndDeleteParameterValue( sPath, 'PASSWORD' ) ;

    sPredefinedParams  := URLGetQuery( sPath ) ;
    if not IsStringEmpty( sPredefinedParams ) then begin
      len := StringLast( sPredefinedParams ) ;
      if sPredefinedParams[len] <> '&' then
        sPredefinedParams := sPredefinedParams + '&' ;
    end ;

    sPath     := URLGetPath( sPath ) ;

    {$IFDEF OXYGENE}
      sUserName := TemplateProducer( sUserName, nil, @passwordCallBack, False ) ;
      sUserPass := TemplateProducer( sUserPass, nil, @passwordCallBack, False ) ;
    {$ELSE}
      sUserName := TemplateProducer( sUserName, nil,  passwordCallBack, False ) ;
      sUserPass := TemplateProducer( sUserPass, nil,  passwordCallBack, False ) ;
    {$ENDIF}

    {$IFDEF JAVA OR ISLAND}
      for str in stmp.Split( ',' ) do
    {$ELSE}
      for str in stmp.Split( [','] ) do
    {$ENDIF}
        if not IsStringEmpty( str ) then
          oPredefinedLayers.Add( str ) ;

    if not IsStringEmpty( sPredefinedVersion ) then
      scapversion := sPredefinedVersion
    else
      scapversion := WMS_VERSION_FORCED ;

    if IsStringEmpty( sservice ) then
      sservice := 'WMS' ;

    if UpperCase( sservice ) <> 'WMS' then
      Abort ;

    if not IsStringEmpty( scapversion ) then
      query := formUrl( Format( '%sVERSION=%s&SERVICE=WMS&REQUEST=GetCapabilities',
                                [ sPredefinedParams, scapversion ]
                               ), sPath
                       )
    else
      query := formUrl( Format('%sSERVICE=WMS&REQUEST=GetCapabilities',
                               [ sPredefinedParams ] ),
                        sPath
                      ) ;

    r.Stream := nil ;
    try
      TestPath( _path, has_getcap, has_getmap ) ;

      if has_getmap then exit ;

      if not has_getcap then
        r := fetchHttp( query, sUserName, sUserPass )
      else
        r := fetchHttp( _path, sUserName, sUserPass ) ;

      if r.Status = GIS_HTTP_OK then
        xDoc.LoadFromStream( r.Stream ) ;
    finally
      FreeObject( r.Stream ) ;
    end ;
  end ;


  procedure T_wmsCapabilities.Dump(
    const _layer : T_wmsLayer ;
    const _list  : TStrings   ;
    const _hier  : TStrings   ;
    const _cat   : TStrings
  ) ;
  var
    i : Integer ;
    p : String ;
  begin
    if _layer.subLayers.Count = 0 then begin
      p := GetLayerPath( _layer, _layer.name, _layer.title ) ;
      _hier.Values[ p ] := _layer.name + ';' + _hier.Values[ p ] ;
      if not IsStringEmpty( _layer.name ) then begin
        _list.Values[ _layer.name ] := _layer.title ;
        _cat.Add( _layer.name ) ;
      end ;
    end ;

    for i := 0 to _layer.subLayers.Count - 1 do
      Dump( T_wmsLayer( _layer.subLayers.Items[ i ] ), _list, _hier, _cat ) ;
  end ;

  {$IFDEF OXYGENE}

    procedure T_wmsCapabilities.doBusy(
      const _sender : Object ;
      const _e      : TGIS_BusyEventArgs
    ) ;
    begin
      if assigned( oViewer ) then
        _e.Abort := oViewer.HourglassShake ;
    end ;
  {$ELSE}

    procedure T_wmsCapabilities.doBusy(
          _sender    : TObject ;
          _pos, _end : Integer ;
      var _abort     : Boolean
    ) ;
    begin
      if assigned( oLayer ) then
        _abort := TGIS_Layer( oLayer ).HourglassShake ;
    end ;
  {$ENDIF}

  function T_wmsCapabilities.fetchHttp(
    const _url     : String  ;
    const _user    : String  ;
    const _pass    : String
  ) : TGIS_HttpResponse ;
  begin
    sLastUrl := _url ;

    Result := TGIS_WebUtils.HttpFetch(
                ProxyUrl + _url,
                nil,
                {$IFDEF OXYGENE}
                  @doBusy,
                {$ELSE}
                  doBusy,
                {$ENDIF}
                True,
                TimeOut,
                UserAgent,
                '',
                _user,
                _pass
              ) ;
    if assigned( Result.Stream ) then
      iBytes := iBytes + Result.Stream.Size;
  end ;

  function T_wmsCapabilities.FindLayerByName(
    const _layer : T_wmsLayer ;
    const _name  : String
  ) : T_wmsLayer ;
  var
    i : Integer ;
  begin
    Result := nil ;
    if not assigned( _layer ) then exit ;

    if _layer.name = _name then begin
      Result := _layer ;
      exit ;
    end ;

    for i := 0 to _layer.subLayers.Count - 1 do begin
      Result := FindLayerByName( T_wmsLayer( _layer.subLayers[ i ] ), _name ) ;
      if assigned( Result ) then break ;
    end ;
  end ;

  function T_wmsCapabilities.formUrl(
    const _query : String;
    const _path  : String
  ) : String;
  var
    k       : Integer ;
    strpath : String  ;
    query   : String  ;

    // guarantee that query parameters will
    // replace any original URL parameters
    procedure replace_parameters ;
    var
      lst_path  : TGIS_StringList ;
      lst_query : TGIS_StringList ;
      str_path1 : String      ;
      str_path2 : String      ;
      ii        : Integer     ;
      kk        : Integer     ;
    begin
      if IsStringEmpty( query ) then exit ;

      lst_path  := nil ;
      lst_query := nil ;
      try
        str_path2 := URLGetQuery( strpath ) ;
        if IsStringEmpty( str_path2 ) then exit ;
        str_path1 := URLGetPath ( strpath ) ;

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

        strpath := str_path1 ;
        for ii := 0 to lst_path.Count - 1 do begin
          if ii = 0 then
            strpath := strpath + lst_path[ii]
          else
            strpath  := strpath + '&' + lst_path[ii] ;
        end ;

      finally
        FreeObject( lst_path  ) ;
        FreeObject( lst_query ) ;
      end ;
    end ;

  begin
    if IsStringEmpty( _path ) then exit ;

    strpath  := _path  ;
    query := _query ;

    replace_parameters ;

    // trim any '?' at the tail
    k := length( strpath ) ;
    while strpath[k+StringFirst-1] = '?' do begin
      dec( k ) ;
      SetLengthStr( strpath, k ) ;
    end ;

    // trim any '&' at the tail
    k := length( strpath ) ;
    while strpath[k+StringFirst-1] = '&' do begin
      dec( k ) ;
      SetLengthStr( strpath, k ) ;
    end ;

    // if '?' exist in the middle of the Path then add '&'
    // between Path and _query; otherwise add '?'
    if Pos( '?', strpath ) > StringFirst then
      Result := strpath + '&' + query
    else
      Result := strpath + '?' + query ;
  end ;

  function T_wmsCapabilities.getAttributeValue(
    const _node  : IXMLNode ;
    const _key   : String
  ) : OleVariant ;
  var
    attr : IXMLNode ;
  begin
    Result := Unassigned ;
    if not assigned( _node ) then exit ;

    attr := _node.AttributeNodes.FindNode( _key ) ;
    if assigned( attr ) then
      Result := attr.Text ;
  end ;

  function T_wmsCapabilities.getElementValue(
    const _node : IXMLNode ;
    const _key  : String
  ) : String ;
  var
    attr : IXMLNode ;
  begin
    if not assigned( _node ) then exit ;

    attr := _node.ChildNodes.FindNode( _key ) ;

    if assigned( attr ) then
      Result := attr.Text
    else
      Result := '' ;
  end ;

  function T_wmsCapabilities.GetFeatureInfo(
    const _path         : String      ;
    const _version      : String      ;
    const _extent       : TGIS_Extent ;
    const _format       : String      ;
    const _srs          : String      ;
    const _width        : Integer     ;
    const _height       : Integer     ;
    const _layers       : String      ;
    const _qlayers      : String      ;
    const _info_fmt     : String      ;
    const _fea_cnt      : Integer     ;
    const _i            : Integer     ;
    const _j            : Integer     ;
    const _ignoreURI    : Boolean     ;
    const _axisReversed : Boolean
  ) : TGIS_HttpResponse ;
  var
    query   : String  ;
    tmppath : String  ;
    ext     : TGIS_Extent ;
    sx, sy  : String ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      sVersion := sPredefinedVersion
    else
      sVersion := _version ;

    if not _ignoreURI then
      tmppath := oGetFeatureMethods.Values[ 'Get' ]
    else
      tmppath := '' ;

    if IsStringEmpty( tmppath ) then
      tmppath := _path
    else if ( Pos( '://localhost', tmppath ) >= StringFirst ) and
            ( Pos( '://localhost', _path   ) <  StringFirst ) then
      tmppath := _path ;

    if _axisReversed then begin
      // swap extent XY
      ext.XMin := _extent.YMin ;
      ext.YMin := _extent.XMin ;
      ext.XMax := _extent.YMax ;
      ext.YMax := _extent.XMax ;
    end
    else
      ext := _extent ;

    if sVersion >= '1.3.0' then begin
      sx := 'I' ;
      sy := 'J' ;
    end
    else begin
      sx := 'X' ;
      sy := 'Y' ;
    end ;

    query := formUrl( Format( '%sVERSION=%s&'                             +
                              'SERVICE=WMS&'                              +
                              'REQUEST=GetFeatureInfo&BBOX=%s,%s,%s,%s&'  +
                              'FORMAT=%s&'                                +
                              'WIDTH=%d&'                                 +
                              'HEIGHT=%d&'                                +
                              '%s=%s&'                                    +
                              'INFO_FORMAT=%s&'                           +
                              'FEATURE_COUNT=%d&'                         +
                              '%s=%d&'                                    +
                              '%s=%d&'                                    +
                              'LAYERS=%s&'                                +
                              'QUERY_LAYERS=%s'                           ,
                             [
                               sPredefinedParams,
                               _version,
                               DotFloatToStr( ext.XMin ),
                               DotFloatToStr( ext.YMin ),
                               DotFloatToStr( ext.XMax ),
                               DotFloatToStr( ext.YMax ),
                               _format,
                               _width,
                               _height,
                               sSRSName,
                               _srs,
                               _info_fmt,
                               _fea_cnt,
                               sx, _i,
                               sy, _j,
                               _qlayers,
                               _qlayers
                             ]
                            ), tmppath
                     ) ;

    Result := fetchHttp( query, sUserName, sUserPass ) ;
  end ;

  function T_wmsCapabilities.GetMap(
    const _path         : String      ;
    const _version      : String      ;
    const _extent       : TGIS_Extent ;
    const _format       : String      ;
    const _srs          : String      ;
    const _width        : Integer     ;
    const _height       : Integer     ;
    const _styles       : String      ;
    const _layers       : String      ;
    const _ignoreURI    : Boolean     ;
    const _axisReversed : Boolean
  ) : TGIS_HttpResponse ;
  var
    query   : String  ;
    tmppath : String  ;
    ext     : TGIS_Extent ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      sVersion := sPredefinedVersion
    else
      sVersion := _version ;

    if not _ignoreURI then
      tmppath := oGetMapMethods.Values[ 'Get' ]
    else
      tmppath := '' ;

    if IsStringEmpty( tmppath ) then
      tmppath := _path
    else if ( Pos( '://localhost', tmppath ) >= StringFirst ) and
            ( Pos( '://localhost', _path   ) <  StringFirst ) then
      tmppath := _path ;

    if _axisReversed then begin
      // swap extent XY
      {$IFDEF GIS_NORECORDS}
        ext := new TGIS_Extent() ;
      {$ENDIF}
      ext.XMin := _extent.YMin ;
      ext.YMin := _extent.XMin ;
      ext.XMax := _extent.YMax ;
      ext.YMax := _extent.XMax ;
    end
    else
      ext := _extent ;

    query := formUrl( Format( 'VERSION=%s&'                       +
                              'SERVICE=WMS&'                      +
                              'REQUEST=GetMap&BBOX=%s,%s,%s,%s&'  +
                              'FORMAT=%s&'                        +
                              'WIDTH=%d&'                         +
                              'HEIGHT=%d&'                        +
                              '%s=%s&'                            +
                              'TRANSPARENT=TRUE&'                 +
                              'STYLES=%s&'                        +
                              '%s'                                +
                              'LAYERS=%s'                         ,
                             [ _version,
                               DotFloatToStr( ext.XMin ),
                               DotFloatToStr( ext.YMin ),
                               DotFloatToStr( ext.XMax ),
                               DotFloatToStr( ext.YMax ),
                               _format,
                               _width,
                               _height,
                               sSRSName,
                               _srs,
                               _styles,
                               sPredefinedParams,
                               _layers
                             ]
                            ), tmppath
                     ) ;

    if oldGet <> query then begin
      Result := fetchHttp( query, sUserName, sUserPass) ;
      oldGet := query ;
    end
    else begin
      Result.Stream := nil ;
    end;
  end ;

  function T_wmsCapabilities.GetMaxBndBox(
    const _layer  : T_wmsLayer;
    const _srs    : String
  ) : TGIS_Extent ;
  var
    i   : Integer ;
  begin
    Result := GisNoWorld ;
    if not assigned( _layer ) then exit ;

    for i := 0 to _layer.boundingBox.Count - 1 do begin
      if CompareText( _srs, T_wmsBoundingBox( _layer.boundingBox[ i ] ).srs ) = 0 then
      begin
         Result := T_wmsBoundingBox( _layer.boundingBox[ i ] ).boundingBox ;
         break ;
      end ;
    end ;

    for i := 0 to _layer.subLayers.Count - 1 do
      Result := GisMaxExtent(
                  Result,
                  GetMaxBndBox( T_wmsLayer( _layer.subLayers[ i ] ), _srs )
                ) ;
  end ;

  function T_wmsCapabilities.GetMaxLatLonBndBox(
    const _layer : T_wmsLayer
  ) : TGIS_Extent;
  var
    i   : Integer ;
  begin
    Result := GisNoWorld ;
    if not assigned( _layer ) then exit ;

    Result := GisMaxExtent( Result, _layer.latLonBndBox ) ;

    for i := 0 to _layer.subLayers.Count - 1 do
      Result := GisMaxExtent(
                  Result,
                  GetMaxLatLonBndBox( T_wmsLayer( _layer.subLayers[ i ] ) )
                ) ;
  end ;

  function T_wmsCapabilities.GetLayerPath(
    const _layer : T_wmsLayer ;
    const _name  : String     ;
    const _title : String
  ) : String;
  var
    wlayer : T_wmsLayer ;
  begin
    wlayer := FindLayerByName( _layer, _name ) ;

    while assigned( wlayer ) do begin

      wlayer := wlayer.parent ;
      if assigned( wlayer ) then
        Result := wlayer.title + '\' + Result ;
    end ;
    SetLengthStr( Result, length( Result ) - 1 ) ;
  end ;

  function T_wmsCapabilities.IsLayerQueryAble(
    const _layer : T_wmsLayer ;
    const _name  : String
  ) : Boolean ;
  var
    wlayer : T_wmsLayer ;
  begin
    wlayer := FindLayerByName( _layer, _name ) ;
    Result := False ;

    if not assigned( wlayer ) then exit ;

    if ( wlayer.parent = nil ) and ( wlayer.isqueryable ) then begin
      Result := True ;
      exit ;
    end ;

    while assigned( wlayer ) do begin

      if wlayer.isqueryable then begin
        Result := wlayer.isqueryable ;
        break ;
      end ;

      wlayer := wlayer.parent ;
    end ;
  end ;

  procedure T_wmsCapabilities.loadLayers(
    const _xml_layer : IXMLNode ;
    const _parent   : T_wmsLayer
  ) ;
  var
    wlayer    : T_wmsLayer       ;
    attr      : IXMLNode         ;
    subnodes  : IXMLNodeList     ;
    i, j      : Integer          ;
    bbox      : T_wmsBoundingBox ;
    tkn       : TGIS_Tokenizer   ;
    srsVal    : String           ;
    scale_min : String ;
    scale_max : String ;
    scaption  : String ;
    val       : String ;
  begin
    wlayer := T_wmsLayer.Create ;

    if _parent = nil then
      oLayersHierarchy.subLayers.Add( wlayer )
    else
      _parent.subLayers.Add( wlayer ) ;

    wlayer.parent := _parent ;
    if not assigned( _parent ) then
      wlayer.level := 0
    else
      wlayer.level := _parent.level + 1;

    try
      wlayer.isqueryable := VarToBoolean(
                              getAttributeValue( _xml_layer, 'queryable' )
                            ) ;
    except
      wlayer.isqueryable := False ;
    end;

    wlayer.title  := getElementValue( _xml_layer, 'Title' ) ;
    wlayer.name   := getElementValue( _xml_layer, 'Name' ) ;
    wlayer.info   := getElementValue( _xml_layer, 'Abstract' ) ;

    attr := _xml_layer.ChildNodes.FindNode( 'LatLonBoundingBox' ) ;
    if assigned( attr ) then begin
      wlayer.latLonBndBox.XMin := DotStrToFloat( VarToString(
                                    getAttributeValue( attr, 'minx' )
                                    )
                                  ) ;
      wlayer.latLonBndBox.XMax := DotStrToFloat( VarToString(
                                    getAttributeValue( attr, 'maxx' )
                                    )
                                  ) ;
      wlayer.latLonBndBox.YMin := DotStrToFloat( VarToString(
                                    getAttributeValue( attr, 'miny' )
                                    )
                                  ) ;
      wlayer.latLonBndBox.YMax := DotStrToFloat( VarToString(
                                    getAttributeValue( attr, 'maxy' )
                                    )
                                  ) ;
    end
    else begin
      wlayer.latLonBndBox := GisNoWorld ;

      attr := _xml_layer.ChildNodes.FindNode( 'EX_GeographicBoundingBox' ) ;
      if assigned( attr ) then begin
        val := getElementValue( attr, 'westBoundLongitude' ) ;
        if IsStringEmpty( val) then
          val := VarToString( getAttributeValue( attr, 'westBoundLongitude' ) ) ;
        wlayer.latLonBndBox.XMin := DotStrToFloat( val ) ;

        val := getElementValue( attr, 'eastBoundLongitude' ) ;
        if IsStringEmpty( val) then
          val := VarToString( getAttributeValue( attr, 'eastBoundLongitude' ) ) ;
        wlayer.latLonBndBox.XMax := DotStrToFloat( val ) ;

        val := getElementValue( attr, 'southBoundLatitude' ) ;
        if IsStringEmpty( val) then
          val := VarToString( getAttributeValue( attr, 'southBoundLatitude' ) ) ;
        wlayer.latLonBndBox.YMin := DotStrToFloat( val ) ;

        val := getElementValue( attr, 'northBoundLatitude' ) ;
        if IsStringEmpty( val) then
          val := VarToString( getAttributeValue( attr, 'northBoundLatitude' ) ) ;
        wlayer.latLonBndBox.YMax := DotStrToFloat( val ) ;
      end
      else
        wlayer.latLonBndBox := GisNoWorld ;
    end;

    scale_min := '' ;
    scale_max := '' ;

    subnodes := _xml_layer.ChildNodes ;
    if assigned( subnodes ) then begin
      for i := 0 to subnodes.Count - 1 do begin
        if subnodes.Nodes[ i ].NodeName = 'BoundingBox' then begin
          bbox := T_wmsBoundingBox.Create ;
          {$IFDEF GIS_NORECORDS}
            bbox.boundingBox := new TGIS_Extent ;
          {$ENDIF}
          bbox.boundingBox.XMin := DotStrToFloat( VarToString(
                                       getAttributeValue(
                                         subnodes.Nodes[ i ], 'minx'
                                       )
                                     )
                                   ) ;
          bbox.boundingBox.XMax := DotStrToFloat( VarToString(
                                       getAttributeValue(
                                         subnodes.Nodes[ i ], 'maxx'
                                       )
                                     )
                                   ) ;
          bbox.boundingBox.YMin := DotStrToFloat( VarToString(
                                       getAttributeValue(
                                         subnodes.Nodes[ i ], 'miny'
                                       )
                                     )
                                   ) ;
          bbox.boundingBox.YMax := DotStrToFloat( VarToString(
                                       getAttributeValue(
                                         subnodes.Nodes[ i ], 'maxy'
                                       )
                                     )
                                   ) ;
          bbox.srs              := VarToString( getAttributeValue(
                                     subnodes.Nodes[ i ], 'SRS'
                                     )
                                   ) ;
          if IsStringEmpty( bbox.srs ) then
            bbox.srs := VarToString( getAttributeValue( subnodes.Nodes[ i ], 'CRS' ) ) ;

          if CompareText( bbox.srs, 'EPSG:WGS84(DD)' ) = 0 then
            bbox.srs := 'EPSG:4326' ;

          if GisIsNoWorld( bbox.boundingBox ) then
            FreeObject( bbox )
          else
            wlayer.boundingBox.Add( bbox ) ;
        end
        else if ( subnodes.Nodes[ i ].NodeName = 'SRS' ) or
                ( subnodes.Nodes[ i ].NodeName = 'CRS' ) then begin

          tkn := TGIS_Tokenizer.Create ;
          try
            tkn.Execute( subnodes.Nodes[ i ].Text, [ ',',' ',';' ] ) ;
            for j := 0 to tkn.Result.Count - 1 do begin
              srsVal := tkn.Result[ j ] ;
              if CompareText( srsVal, 'EPSG:WGS84(DD)' ) = 0 then
                srsVal := 'EPSG:4326' ;
              wlayer.srs.Add( srsVal ) ;
              oAllSRS.Add( srsVal ) ;
            end;
          finally
            FreeObject( tkn ) ;
          end ;
        end
        else if ( subnodes.Nodes[ i ].NodeName = 'Style' ) then begin
          wlayer.styles.Add( getElementValue( subnodes.Nodes[ i ], 'Name' ) ) ;
        end
        else if ( subnodes.Nodes[ i ].NodeName = 'MaxScaleDenominator' ) then begin
          scale_max := subnodes.Nodes[ i ].Text ;
        end
        else if ( subnodes.Nodes[ i ].NodeName = 'MinScaleDenominator' ) then begin
          scale_min := subnodes.Nodes[ i ].Text ;
        end
        else if ( subnodes.Nodes[ i ].NodeName = 'ScaleHint' ) then begin
          scale_min := VarToString( getAttributeValue( subnodes.Nodes[ i ], 'min' ) ) ;
          scale_max := VarToString( getAttributeValue( subnodes.Nodes[ i ], 'max' ) ) ;
        end;

      end ;
    end ;

    wlayer.legend.format := getElementValue(
                              _xml_layer.ChildNodes[ 'Style' ]
                                          .ChildNodes[ 'LegendURL' ],
                              'Format'
                            ) ;
    attr := _xml_layer.ChildNodes[ 'Style' ]
                        .ChildNodes[ 'LegendURL' ]
                          .ChildNodes[ 'OnlineResource' ] ;
    if assigned( attr ) then
      wlayer.legend.url := VarToString( getAttributeValue( attr, 'xlink:href' ) ) ;

    if assigned( wlayer.parent ) then begin
      if wlayer.parent.name = wlayer.name then begin
        wlayer.parent.legend.format := wlayer.legend.format ;
        wlayer.parent.legend.url    := wlayer.legend.url ;
      end ;
    end ;

    if wlayer.styles.Count = 0 then
      wlayer.styles.Add( '' ) ; // default

    if not IsStringEmpty( scale_min ) then
      try
        wlayer.minScale := DotStrToFloat( scale_min )
      except
        wlayer.minScale := 0 ; // can be NAN or Infinity
      end
    else
      wlayer.minScale := 0 ;

    if not IsStringEmpty( scale_max ) then
      try
        wlayer.maxScale := DotStrToFloat( scale_max )
      except
        wlayer.maxScale := 0 ; // can be NAN or Infinity
      end
    else
      wlayer.maxScale := 0 ;

    if not IsStringEmpty( wlayer.name ) then begin
      if oAllLayers.IndexOf( wlayer.name ) = -1 then begin
        oAllLayers.Add( wlayer.name ) ;
        if not IsStringEmpty( wlayer.title ) then
          scaption := wlayer.title
        else
          scaption := wlayer.name ;
        oAllLayersCaptions.Add( scaption ) ;
      end ;
    end ;

    subnodes := _xml_layer.ChildNodes ;
    for i := 0 to subnodes.Count - 1 do
      if subnodes.Nodes[ i ].NodeName = 'Layer' then
        loadLayers( subnodes.Nodes[ i ], wlayer ) ;
  end ;

  procedure T_wmsCapabilities.loadOnlineResource(
    const _xmlList : IXMLNode ;
    const _list    : TStrings
  ) ;
  var
    attr : IXMLNode ;
    url  : String ;
  begin
    if not assigned( _xmlList ) then exit ;

    attr := _xmlList.ChildNodes[ 'DCPType' ]
                       .ChildNodes['HTTP']
                         .ChildNodes['Get']
                           .ChildNodes['OnlineResource' ] ;
    if assigned( attr ) then begin
      url := VarToString( getAttributeValue( attr, 'xlink:href' ) ) ;
      _list.Values[ 'Get' ] := url ;
    end ;

    attr := _xmlList.ChildNodes[ 'DCPType' ]
                      .ChildNodes['HTTP']
                        .ChildNodes['Post']
                          .ChildNodes['OnlineResource' ] ;
    if assigned( attr ) then begin
      url := VarToString( getAttributeValue( attr, 'xlink:href' ) ) ;
      _list.Values[ 'Post' ] := url ;
    end ;
  end ;

  procedure T_wmsCapabilities.TestPath(
    const _path     : String ;
    var _has_getcap : Boolean ;
    var _has_getmap : Boolean
  ) ;
  begin
    _has_getcap := Pos( UpperCase( 'request=GetCapabilities' ),
                        UpperCase( _path )
                      ) > ( StringFirst - 1 ) ;
    _has_getmap := Pos( UpperCase( 'request=GetMap'          ),
                        UpperCase( _path )
                      ) > ( StringFirst - 1 ) ;

  end ;

  function T_wmsCapabilities.FindDefaultLayer : String ;
  var
    wlayer : T_wmsLayer ;
  begin
    wlayer := oLayersHierarchy ;

    while assigned( wlayer ) do begin
      if assigned( wlayer.subLayers ) and ( wlayer.subLayers.Count > 0 ) then
        wlayer := T_wmsLayer( wlayer.subLayers[0] )
      else
        break ;
    end ;
    if assigned( wlayer ) then
      Result := wlayer.name
    else
      Result := '' ;
  end ;


//==============================================================================
// T_wmsLayer
//==============================================================================

  constructor T_wmsLayer.Create ;
  begin
    inherited;

    subLayers    := TGIS_ObjectList.Create ;
    boundingBox  := TGIS_ObjectList.Create ;
    legend       := T_wmsLayerLegend.Create ;
    srs          := TGIS_StringList.Create ;
    styles       := TGIS_StringList.Create ;
    latLonBndBox := GisNoWorld ;
  end ;

  procedure T_wmsLayer.doDestroy ;
  begin
    FreeObject( subLayers   ) ;
    FreeObject( boundingBox ) ;
    FreeObject( legend      ) ;
    FreeObject( srs         ) ;
    FreeObject( styles      ) ;

    inherited;
  end ;

//==============================================================================
// TGIS_WMSLayer
//==============================================================================

  {@@ Constructor.
  }
  constructor TGIS_WMSLayer.Create ;
  begin
    inherited ;

    Name         := '' ;
    Title        := '' ;
    LatLonBBox   := GisNoWorld ;
    CRS          := '' ;
    CRSBBox      := GisNoWorld ;
    SubLayers    := TGIS_ObjectList.Create( True ) ;
    Legend       := '' ;
    Style        := '' ;
  end ;


  {@@ Destructor.
  }
  procedure TGIS_WMSLayer.doDestroy ;
  begin
    FreeObject( SubLayers ) ;

    inherited ;
  end ;

//==============================================================================
// TGIS_FileWMS
//==============================================================================

  constructor TGIS_FileWMS.Create(
    const _vwr    : IGIS_Viewer ;
    const _layer  : TObject
  ) ;
  begin
    inherited Create ;

    oViewer := _vwr ;
    oLayer  := _layer ;

    FDefaultLayer := '';
    FSelectedSRS  := '';
    FForcedSRS    := '';

    FAxisOrderReversed := False ;
    FAxisOrderIgnored  := False ;

    wmsCapabilities := T_wmsCapabilities.Create( oViewer, oLayer ) ;
  end ;

  procedure TGIS_FileWMS.doDestroy ;
  begin
    FreeObject( wmsCapabilities ) ;

    inherited ;
  end ;

  function TGIS_FileWMS.fget_ProxyUrl
    : String  ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).sProxyUrl ;
  end ;

  procedure TGIS_FileWMS.fset_ProxyUrl(
    const _value : String
  ) ;
  begin
    T_wmsCapabilities( wmsCapabilities ).sProxyUrl := _value ;
  end ;

  function TGIS_FileWMS.fget_LastUrl
    : String ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).LastUrl
  end ;

  function TGIS_FileWMS.fget_ImageFormats
    : TStrings ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).ImageFormats
  end ;

  function TGIS_FileWMS.fget_AllLayers
    : TStrings ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).AllLayers ;
  end ;

  function TGIS_FileWMS.fget_AllSRS
    : TStrings ;
  var
    wmsc : T_wmsCapabilities ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;
    if not IsStringEmpty( wmsc.PredefinedSRS ) then
      wmsc.oAllSRS.Text := wmsc.PredefinedSRS ;

    Result := wmsc.AllSRS ;
  end ;

  function  TGIS_FileWMS.fget_ForcedSRS
    : String ;
  var
    wmsc : T_wmsCapabilities ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    if not IsStringEmpty( FForcedSRS ) then
      Result := FForcedSRS
    else
      Result := wmsc.PredefinedSRS ;
  end;

  procedure TGIS_FileWMS.fset_ForcedSRS(
    const _value : String
  ) ;
  begin
    FForcedSRS := _value ;
  end;

  function TGIS_FileWMS.fget_AllLayersCaptions
    : TStrings ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).AllLayersCaptions ;
  end ;

  function TGIS_FileWMS.fget_PredefinedVersion
    : String ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).PredefinedVersion ;
  end ;

  function TGIS_FileWMS.fget_MaxHeight: Integer;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).MaxHeight ;
  end;

  function TGIS_FileWMS.fget_MaxWidth: Integer;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).MaxWidth ;
  end;

  function TGIS_FileWMS.fget_PredefinedFormat
    : String ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).PredefinedFormat ;
  end ;

  function TGIS_FileWMS.fget_PredefinedStyles
    : String ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).PredefinedStyles ;
  end ;

  function TGIS_FileWMS.fget_PredefinedLayers
    : TStrings ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).PredefinedLayers ;
  end ;

  function TGIS_FileWMS.GetExtent(
    const _srs : String
  ) : TGIS_Extent ;
  var
    ext  : TGIS_Extent       ;
    ext2 : TGIS_Extent       ;
    wmsc : T_wmsCapabilities ;
    tkn  : TGIS_Tokenizer    ;
    lcs  : TGIS_CSCoordinateSystem ;
    wgs  : TGIS_CSCoordinateSystem ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    ext := wmsc.GetMaxBndBox( wmsc.Layer, _srs ) ;

    if isAxisReversed then begin
      // swap extent XY
      ext2 := ext ;
      ext.XMin := ext2.YMin ;
      ext.YMin := ext2.XMin ;
      ext.XMax := ext2.YMax ;
      ext.YMax := ext2.XMax ;
    end ;

    if GisIsNoWorld( ext ) then begin
      ext := wmsc.GetMaxLatLonBndBox( wmsc.Layer ) ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _srs, [':'] ) ;
        if tkn.Result.Count > 1 then
          try
            if CompareText( tkn.Result[ 1 ], WMS_WGS84DD ) = 0 then
              lcs := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 )
            else
              lcs := TGIS_CSFactory.ByEPSG( StrToInt( tkn.Result[ 1 ] ) ) ;
          except
            lcs := CSUnknownCoordinateSystem ;
          end
        else
          lcs := CSUnknownCoordinateSystem ;
      finally
        FreeObject( tkn ) ;
      end ;

      wgs := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;

      if not ( lcs is TGIS_CSUnknownCoordinateSystem ) and
         not GisIsNoWorld( ext ) then begin
        ext := lcs.ExtentFromCS( wgs, ext ) ;
        if not GisIsValidExtent( ext ) then
          ext := GisNoWorld ;
      end;
    end;

    if not GisIsNoWorld( wmsc.ePredefinedBBox ) then
      if not GisIsNoWorld( ext ) then
        ext := GisCommonExtent( ext, wmsc.ePredefinedBBox )
      else
        ext := wmsc.ePredefinedBBox ;

    Result := ext ;
  end ;

  function TGIS_FileWMS.VerifySRS(
    const _srs : String
  ) : Boolean ;
  var
    wmsc : T_wmsCapabilities ;
    ext  : TGIS_Extent       ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    ext := wmsc.GetMaxBndBox( wmsc.Layer, _srs ) ;

    Result := not GisIsNoWorld( ext ) ;
  end ;

  function TGIS_FileWMS.fget_QueryableLayers
    : TStrings ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).QueryableLayers ;
  end ;

  function TGIS_FileWMS.fget_TransmittedBytes
    : Int64 ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).TransmittedBytes ;
  end ;

  function TGIS_FileWMS.fget_ServiceVersion
    : String ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).Version ;
  end ;

  function TGIS_FileWMS.fget_ServiceInfo
    : String ;
  begin
    Result := T_wmsCapabilities( wmsCapabilities ).sTitle + #13#10 +
              T_wmsCapabilities( wmsCapabilities ).sAbstract ;
  end ;

  function TGIS_FileWMS.fget_DefaultSRS
    : String ;
  var
    wmsc : T_wmsCapabilities ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    if assigned( wmsc.AllSRS ) then begin
      if wmsc.AllSRS.Count > 0 then
        Result := T_wmsCapabilities( wmsCapabilities ).AllSRS[ 0 ] ;
    end
    else
      Result := '' ;
  end ;

  function TGIS_FileWMS.isAxisReversed : Boolean ;
  var
    wmsc : T_wmsCapabilities ;
    brev : Boolean ;
  begin
    brev := False ;
    Result := False ;

    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    if assigned( TGIS_Layer( wmsc.oLayer ).CS ) then begin
      if wmsc.sVersion >= '1.3.0' then begin
        brev := TGIS_Layer( wmsc.oLayer ).CS.EPSG = 4326 ;
        brev := brev or TGIS_Layer( wmsc.oLayer ).CS.ReversedCoordinates ;
      end;
    end;

    if not FAxisOrderIgnored then
      Result := brev ;

    if FAxisOrderReversed then
      Result := not Result ;
  end ;

  procedure TGIS_FileWMS.Load(
    const _path  : String ;
    const _xml   : TStream
  ) ;
  var
    wmsc       : T_wmsCapabilities ;
    has_getcap : Boolean           ;
    has_getmap : Boolean           ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    wmsc.TestPath( _path, has_getcap, has_getmap ) ;

    wmsc.FOnPassword := FOnPassword ;
    wmsc.TimeOut     := TimeOut ;
    wmsc.UserAgent   := UserAgent ;
    wmsc.ProxyUrl    := ProxyUrl ;
//?    wmsc.InverseXY   := bInverseXY ;

    if not assigned( _xml ) then
      wmsc.GetCapabilities( _path ) ;
    if not has_getmap then
      wmsc.Parse( _xml )
    else
      wmsc.ParsePredefined ;

    FDefaultLayer := wmsc.FindDefaultLayer ;
    FSelectedSRS  := '' ;
    FError := wmsc.Error ;
  end ;

  function TGIS_FileWMS.GetFeatureInfo(
    const _extent   : TGIS_Extent ;
    const _format   : String      ;
    const _width    : Integer     ;
    const _height   : Integer     ;
    const _layers   : {$IFDEF OXYGENE}
                        TGIS_Strings;
                      {$ELSE}
                        TStrings ;
                      {$ENDIF}
    const _infoFmt  : String      ;
    const _feaCnt   : Integer     ;
    const _i        : Integer     ;
    const _j        : Integer
  ) : TGIS_HttpResponse ;
  var
    wmsc     : T_wmsCapabilities ;
    sversion : String            ;
    slay     : String            ;
    sqlay    : String            ;
    ssrs     : String            ;
    sfeafmt  : String            ;
    sfmt     : String            ;
    i        : Integer           ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    if not IsStringEmpty( wmsc.PredefinedVersion ) then
      sversion := wmsc.PredefinedVersion
    else
      sversion := wmsc.Version ;

    if _layers.Count > 0 then
      slay := wmsc.canonicalNames( _layers )
    else
      slay := wmsc.canonicalNames( AllLayers ) ;

    for i := _layers.Count - 1 downto 0 do
      if not wmsc.IsLayerQueryAble( wmsc.Layer, _layers[ i ] ) then
        _layers.Delete( i ) ;

    sqlay := wmsc.canonicalNames( _layers ) ;

    if not IsStringEmpty( PredefinedFormat ) then
      sfmt := PredefinedFormat
    else
      sfmt := _format ;

    ssrs := FSelectedSRS ;

    if IsStringEmpty( ssrs ) then
      ssrs := 'EPSG:4326' ;

    if IsStringEmpty( _infoFmt ) and
      assigned( wmsc.FeaFormat ) and ( wmsc.FeaFormat.Count > 0 ) then begin
      if IsStringEmpty( wmsc.PredefinedInfoFormat ) then
        sfeafmt := wmsc.FeaFormat[ 0 ]
      else begin
        if wmsc.FeaFormat.IndexOf( wmsc.PredefinedInfoFormat ) >= 0 then
          sfeafmt := wmsc.PredefinedInfoFormat
        else
          sfeafmt := wmsc.FeaFormat[ 0 ]
      end ;
    end
    else
      sfeafmt := _infoFmt ;

    Result := wmsc.GetFeatureInfo( wmsc.Path, sversion, _extent, sfmt,
                                   ssrs, _width, _height, slay, sqlay,
                                   sfeafmt, _feaCnt, _i, _j, FIgnoreInternalURI,
                                   isAxisReversed
                                  ) ;
  end ;

  procedure TGIS_FileWMS.GetHierarchy(
    {$IFDEF OXYGENE}
      const _hierarchy  : TGIS_Strings ;
      const _layers     : TGIS_Strings ;
      const _categories : TGIS_Strings
    {$ELSE}
       const _hierarchy  : TStrings ;
       const _layers     : TStrings ;
       const _categories : TStrings
    {$ENDIF}
  ) ;
  var
    wmsc : T_wmsCapabilities;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    wmsc.Dump( wmsc.Layer, _layers, _hierarchy, _categories ) ;
  end ;

  function TGIS_FileWMS.GetMap(
    const _extent : TGIS_Extent ;
    const _format : String      ;
    const _width  : Integer     ;
    const _height : Integer     ;
    const _layers : {$IFDEF OXYGENE}
                      TGIS_Strings ;
                    {$ELSE}
                      TStrings ;
                    {$ENDIF}
    const _styles : {$IFDEF OXYGENE}
                      TGIS_Strings
                    {$ELSE}
                      TStrings
                    {$ENDIF}
  ) : TGIS_HttpResponse ;
  var
    wmsc     : T_wmsCapabilities ;
    sversion : String            ;
    slay     : String            ;
    sstyle   : String            ;
    ssrs     : String            ;
    sfmt     : String            ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    if not IsStringEmpty( wmsc.PredefinedVersion ) then
      sversion := wmsc.PredefinedVersion
    else
      sversion := wmsc.Version ;

    if _layers.Count > 0 then
      slay := wmsc.canonicalNames( _layers )
    else
      slay := wmsc.canonicalNames( AllLayers ) ;

    sstyle := '' ;
    if _styles.Count > 0 then
      sstyle := wmsc.canonicalNames( _styles ) ;

    if not IsStringEmpty( PredefinedFormat ) then
      sfmt := PredefinedFormat
    else
      sfmt := _format ;

    ssrs := FSelectedSRS ;

    if IsStringEmpty( ssrs ) then
      ssrs := 'EPSG:4326' ;

    try
      if not IsStringEmpty( wmsc.PredefinedStyles ) then
        Result := wmsc.GetMap( wmsc.Path, sversion, _extent, sfmt,
                               ssrs, _width, _height, wmsc.PredefinedStyles,
                               slay, FIgnoreInternalURI, isAxisReversed
                             )
      else
        Result := wmsc.GetMap( wmsc.Path, sversion, _extent, sfmt,
                               ssrs, _width, _height, sstyle,
                               slay, FIgnoreInternalURI, isAxisReversed
                             ) ;
    except
      FreeObject( Result.Stream ) ;
      raise ;
    end;
  end ;

  function TGIS_FileWMS.GetLayers : TGIS_WMSLayer ;
  var
    wmsc : T_wmsCapabilities ;

    procedure fillSublayers( const _layer : T_wmsLayer;
                             const _list  : TGIS_ObjectList
                            ) ;
    var
      i, j : Integer ;
      lyr  : T_wmsLayer ;
      la   : TGIS_WMSLayer ;
    begin

      for i := 0 to _layer.subLayers.Count - 1 do begin
        lyr := T_wmsLayer( _layer.subLayers.Items[ i ] ) ;
        la := nil;
        for j := 0 to lyr.styles.Count - 1 do begin
          la := TGIS_WMSLayer.Create ;
          la.Name       := lyr.name ;
          la.Title      := lyr.title ;
          la.Info       := lyr.info ;
          la.LatLonBBox := lyr.latLonBndBox ;
          if GisIsNoWorld( la.LatLonBBox ) then
            la.LatLonBBox := _layer.latLonBndBox ;

          if lyr.srs.Count > 0 then
            la.CRS      := lyr.srs[0]
          else if _layer.srs.Count > 0 then
            la.CRS      := _layer.srs[0];
          if lyr.boundingBox.Count > 0 then
            la.CRSBBox  := T_wmsBoundingBox(lyr.boundingBox[0]).boundingBox
          else if _layer.boundingBox.Count > 0 then
            la.CRSBBox  := T_wmsBoundingBox(_layer.boundingBox[0]).boundingBox ;

          _list.Add( la ) ;
          la.Legend := lyr.legend.url ;
          la.Style  := lyr.styles[j] ;
          la.MinScaleDenominator := lyr.minScale ;
          la.MaxScaleDenominator := lyr.maxScale ;
        end ;

        if lyr.subLayers.Count > 0 then
          fillSublayers( lyr, la.SubLayers ) ;
      end ;

    end ;

  begin
    Result := nil ;
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;
    if not assigned( wmsc.oLayersHierarchy ) then exit ;

    Result := TGIS_WMSLayer.Create ;
    Result.Name  := wmsc.oLayersHierarchy.name ;
    Result.Title := wmsc.oLayersHierarchy.title ;
    Result.Info  := wmsc.oLayersHierarchy.info ;
    if wmsc.oLayersHierarchy.srs.Count > 0 then
      Result.CRS   := wmsc.oLayersHierarchy.srs[0] ;
    Result.LatLonBBox := wmsc.oLayersHierarchy.latLonBndBox ;
    if wmsc.oLayersHierarchy.boundingBox.Count > 0 then
      Result.CRSBBox := T_wmsBoundingBox(wmsc.oLayersHierarchy.boundingBox[0]).boundingBox ;
    Result.Legend := wmsc.oLayersHierarchy.legend.url ;

    fillSublayers( wmsc.oLayersHierarchy, Result.SubLayers ) ;
  end ;

  function TGIS_FileWMS.GetLegendImage(
    const _layer : String
  ) : TGIS_HttpResponse ;
  var
    wmsc : T_wmsCapabilities ;
    lyr  : TGIS_WMSLayer ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;
    lyr := FindLayer( _layer, nil ) ;
    if assigned( lyr ) then begin
      try
        try
          Result := wmsc.fetchHttp( lyr.Legend, wmsc.sUserName, wmsc.sUserPass ) ;
        except
          FreeObject( Result.Stream ) ;
        end ;
      finally
        FreeObject( lyr ) ;
      end ;
    end ;
  end ;

  function TGIS_FileWMS.FindLayer(
    const _name : String ;
    const _cs   : TGIS_CSCoordinateSystem
  ) : TGIS_WMSLayer ;
  var
    wmsc : T_wmsCapabilities ;
    lyr  : T_wmsLayer ;
    i, k : Integer ;
    tkn  : TGIS_Tokenizer ;
  begin
    Result := nil ;
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;
    if not assigned( wmsc.oLayersHierarchy ) then exit ;

    lyr := wmsc.FindLayerByName( wmsc.oLayersHierarchy, _name ) ;
    if assigned( lyr ) then begin

      Result := TGIS_WMSLayer.Create ;
      Result.Name  := lyr.name ;
      Result.Title := lyr.title ;

      if assigned( _cs ) and ( _cs.EPSG > 0 ) then begin
        tkn := TGIS_Tokenizer.Create ;
        try
          k := -1 ;
          for i := 0 to lyr.srs.Count-1 do begin
            tkn.Execute( lyr.srs[i], [':'] ) ;
            if ( tkn.Result.Count > 1               ) and
               ( _cs.EPSG = StrToInt(tkn.Result[1]) ) then begin
              k := i ;
              break ;
            end ;
          end ;

          if ( k >= 0 ) and (k < lyr.srs.Count) then begin
            Result.CRS := lyr.srs[k] ;
            if k < lyr.boundingBox.Count then
              Result.CRSBBox := T_wmsBoundingBox(lyr.boundingBox[k]).boundingBox ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end
      else begin
        if lyr.srs.Count > 0 then
          Result.CRS      := lyr.srs[0] ;
        if lyr.boundingBox.Count > 0 then
          Result.CRSBBox  := T_wmsBoundingBox(lyr.boundingBox[0]).boundingBox ;
      end ;
      Result.LatLonBBox := lyr.latLonBndBox ;
      Result.Legend := lyr.legend.url ;
      if lyr.styles.Count > 0 then
        Result.Style := lyr.styles[0] ;

      Result.MinScaleDenominator := lyr.minScale ;
      Result.MaxScaleDenominator := lyr.maxScale ;
    end ;
  end ;

  function TGIS_FileWMS.MakeUrl(
    const _query  : String ;
    const _path   : String
  ): String;
  var
    wmsc : T_wmsCapabilities ;
  begin
    wmsc := T_wmsCapabilities( wmsCapabilities ) ;

    Result := wmsc.formUrl( _query, _path ) ;
  end ;

{==================================== END =====================================}
end.
