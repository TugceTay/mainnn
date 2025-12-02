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
  WFS provider.
}

{$IFDEF DCC}
  unit GisFileWFS ;
  {$HPPEMIT '#pragma link "GisFileWFS"'}
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
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    GisRtl,
    GisInterfaces,
    GisTypes,
    GisXmlDoc ;
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
  ///   Feature class implementation.
  /// </summary>
  TGIS_WFSFeature = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   thread state
      /// </summary>
      State : Integer ;
      /// <summary>
      ///   start index
      /// </summary>
      FStartIndex : Integer ;
      /// <summary>
      ///   total read count
      /// </summary>
      FReadCount  : Integer ;
    public
      /// <summary>
      ///   Feature name
      /// </summary>
      Name          : String ;
      /// <summary>
      ///   Feature title
      /// </summary>
      Title         : String ;
      /// <summary>
      ///   Feature description
      /// </summary>
      Description   : String ;
      /// <summary>
      ///   Feature keywords
      /// </summary>
      Keywords      : String ;
      /// <summary>
      ///   Feature default SRS
      /// </summary>
      DefaultSRS    : String ;
      /// <summary>
      ///   Feature alternative SRS list
      /// </summary>
      OtherSRS      : TGIS_StringList ;
      /// <summary>
      ///   Feature bounding box in WGS84
      /// </summary>
      WGS84BBox     : TGIS_Extent ;
      /// <summary>
      ///   Feature metadata url
      /// </summary>
      MetadataUrl   : String ;
      /// <summary>
      ///   Feature extra description
      /// </summary>
      ExDescription : String ;
      /// <summary>
      ///   Feature last url
      /// </summary>
      LastUrl       : String ;
      /// <summary>
      ///   Feature last bounding box
      /// </summary>
      LastExtent    : TGIS_Extent ;
      /// <summary>
      ///   Feature extent axis order
      /// </summary>
      ExtentOrder   : String ;
      /// <summary>
      ///   Feature data
      /// </summary>
      Data          : TStream ;
      /// <summary>
      ///   HTTP Status
      /// </summary>
      HTTPStatus : Integer ;
      /// <summary>
      ///   Namespace url
      /// </summary>
      NamespaceUrl : String ;
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Are feature data fetched and ready to use.
      /// </summary>
      /// <returns>
      ///   True if data were fetched.
      /// </returns>
      function IsDataReady : Boolean ;

      /// <summary>
      ///   True, if feature data are valid or False if any error occurred.
      /// </summary>
      /// <returns>
      ///   True if data are valid.
      /// </returns>
      function IsDataValid : Boolean ;

      /// <summary>
      ///   Reset current state.
      /// </summary>
      procedure ResetState ;

      /// <summary>
      ///   Reset current data.
      /// </summary>
      procedure ResetDataPage ;

      /// <summary>
      ///   Set next page range.
      /// </summary>
      /// <param name="_offset">
      ///    offset to increase start index
      /// </param>
      procedure NextDataPage( const _offset : Integer ) ;

      /// <summary>
      ///   Increment data counter.
      /// </summary>
      /// <param name="_count">
      ///    number to increase data counter
      /// </param>
      procedure IncreaseDataCount( const _count : Integer ) ;

    public
      /// <summary>
      ///   Total read data counter.
      /// </summary>
      property ReadCount  : Integer read FReadCount ;
      /// <summary>
      ///   Start index of data.
      /// </summary>
      property StartIndex : Integer read FStartIndex ;
  end ;

  /// <summary>
  ///   Encapsulation of a WFS provider.
  /// </summary>
  TGIS_FileWFS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      FPath                   : String ;
      FUserAgent              : String ;
      FProxyUrl               : String ;
      FVersion                : String ;
      FServiceInfo            : TGIS_StringList ;
      FDataFormats            : TGIS_StringList ;
      FError                  : String  ;
      FPredefinedLayers       : TGIS_StringList ;
      FPredefinedSrsName      : String ;
      FTimeOut                : Integer ;
      FPagingEnabled          : Boolean ;
      FMaxFeatures            : Integer ;
      FPageSize               : Integer ;
      FQueryStructure         : Boolean ;
      oldMaxFeatures          : Integer ;
      xDoc                    : IXMLDocument ;
      oLayer                  : TObject ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oViewer                 : IGIS_Viewer ;
      oFeatureTypeList        : TGIS_ObjectList ;
      sError                  : String ;
      sPredefinedVersion      : String ;
      sPredefinedBox          : String ;
      sPredefinedFilter       : String ;
      sPredefinedCQFilter     : String ;
      sPredefinedStartIndex   : String ;
      sPredefinedMaxFeatures  : String ;
      sPredefinedOutputFormat : String ;

      bAxisOrderIgnored       : Boolean ;
      bBBoxIgnored            : Boolean ;
      bAxisOrderReversed      : Boolean ;

      sExtentOrder            : String ;
      FOnPassword             : TGIS_TemplateProducerEvent ;
      FMetadata               : TGIS_StringList ;
    {$IFDEF OXYGENE} unit {$ELSE} private {$ENDIF}
      sUserName               : String ;
      sUserPass               : String ;
    private
      function  fget_FeaturesCount   : Integer ;
      function  fget_Feature         ( const _indexOrName : Variant
                                     ) : TGIS_WFSFeature ;
      function  fget_PredefinedBox   : TGIS_Extent ;
      procedure fset_QueryStructure  ( const _value : Boolean
                                     ) ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}

       /// <summary>
       ///    Get xml element value.
       /// </summary>
       /// <param name="_node">
       ///    xml node
       /// </param>
       /// <param name="_key">
       ///   key name
       /// </param>
       /// <returns>
       ///   element value
       /// </returns>
       function  getElementValue     ( const _node        : IXMLNode       ;
                                       const _key         : String
                                     ) : String ;

       /// <summary>
       ///   Get xml element attribute value.
       /// </summary>
       /// <param name="_node">
       ///    xml node
       /// </param>
       /// <param name="_key">
       ///   key name
       /// </param>
       /// <param name="_attr">
       ///    attribute name
       /// </param>
       /// <returns>
       ///   element value
       /// </returns>
       function  getElementAttribute ( const _node        : IXMLNode       ;
                                       const _key         : String         ;
                                       const _attr        : String
                                     ) : String ;

       /// <summary>
       ///    Get attribute value.
       /// </summary>
       /// <param name="_node">
       ///    xml node
       /// </param>
       /// <param name="_key">
       ///   key name
       /// </param>
       /// <returns>
       ///   element value
       /// </returns>
       function  getAttributeValue   ( const _node        : IXMLNode       ;
                                       const _key         : String
                                     ) : OleVariant ;

       /// <summary>
       ///   Fetch http data.
       /// </summary>
       /// <param name="_url">
       ///   server path
       /// </param>
       /// <param name="_user">
       ///   user name
       /// </param>
       /// <param name="_pass">
       ///   password
       /// </param>
       /// <returns>
       ///   server response
       /// </returns>
       function  fetchHttp           ( const _url         : String         ;
                                       const _user        : String         ;
                                       const _pass        : String
                                     ) : TGIS_HttpResponse ;

       /// <summary>
       ///   Form url from query and path.
       /// </summary>
       /// <param name="_query">
       ///   query string
       /// </param>
       /// <param name="_path">
       ///   path string
       /// </param>
       /// <returns>
       ///   formed url
       /// </returns>
       function  formUrl             ( const _query       : String         ;
                                       const _path        : String
                                     ) : String ;

       /// <summary>
       ///   Read capabilities data.
       /// </summary>
       /// <param name="_path">
       ///   server path
       /// </param>
       procedure readCapabilities    ( const _path        : String
                                     ) ;

       /// <summary>
       ///  Parse predefined parameters.
       /// </summary>
       /// <param name="_path">
       ///   server path
       /// </param>
       procedure parsePredefined     ( const _path        : String
                                     ) ;

       /// <summary>
       ///  Read predefined parameters.
       /// </summary>
       /// <param name="_path">
       ///   server path
       /// </param>
       procedure readPredefined      ( const _path        : String
                                     ) ;

       /// <summary>
       ///   Parse capabilities data.
       /// </summary>
       procedure parse ;

       /// <summary>
       ///   Get feature from the server.
       /// </summary>
       /// <param name="_feature">
       ///   feature type
       /// </param>
       /// <param name="_extent">
       ///   range of data
       /// </param>
       /// <returns>
       ///    True if data were fetched
       /// </returns>
       function readFeatureData     ( const _feature    : TGIS_WFSFeature ;
                                      const _extent     : TGIS_Extent
                                     ) : Boolean ; overload;

       /// <summary>
       ///   Get feature from the server.
       /// </summary>
       /// <param name="_feature">
       ///   feature type
       /// </param>
       /// <param name="_extent">
       ///   range of data
       /// </param>
       /// <param name="_filter">
       ///   filter for get feature
       /// </param>
       /// <param name="_readData">
       ///   if True, then a stream data will be read
       /// </param>
       /// <returns>
       ///    True if data were fetched
       /// </returns>
       function readFeatureData      ( const _feature     : TGIS_WFSFeature ;
                                       const _extent      : TGIS_Extent ;
                                       const _filter      : String ;
                                       const _readData    : Boolean
                                     ) : Boolean ; overload;

       /// <summary>
       ///   build url from parameters
       /// </summary>
       /// <param name="_name">
       ///   feature name
       /// </param>
       /// <param name="_namespace">
       ///   feature namespace
       /// </param>
       /// <param name="_extent">
       ///   range of data
       /// </param>
       /// <param name="_filter">
       ///   filter for get feature
       /// </param>
       /// <param name="_startIndex">
       ///   start index of data
       /// </param>
       /// <param name="_maxFeatures">
       ///   number of features
       /// </param>
       /// <returns>
       ///    url text
       /// </returns>
       function buildURL             ( const _name        : String ;
                                       const _namespace   : String ;
                                       const _extent      : TGIS_Extent ;
                                       const _filter      : String ;
                                       const _startIndex  : Integer ;
                                       const _maxFeatures : Integer
                                     ) : String ;

       /// <summary>
       ///   Get bounding box filter string.
       /// </summary>
       /// <param name="_extent">
       ///   extent box
       /// </param>
       /// <returns>
       ///   Extent as string
       /// </returns>
       function  getBBOXFilter       ( const _extent      : TGIS_Extent
                                     ) : String ;

       /// <summary>
       ///   Test path for wms flags.
       /// </summary>
       /// <param name="_path">
       ///   path/address string
       /// </param>
       /// <param name="_hasGetCap">
       ///   flag, True if path has GetCapabilities request
       /// </param>
       /// <param name="_hasGetFea">
       ///   flag, True if path has GetFeatures request
       /// </param>
       procedure testPath            ( const _path        : String         ;
                                         var _hasGetCap   : Boolean        ;
                                         var _hasGetFea   : Boolean
                                     ) ;

       /// <summary>
       ///   Get extent for epsg.
       /// </summary>
       /// <param name="_extent">
       ///   source extent
       /// </param>
       /// <param name="_epsg">
       ///   destination epsg
       /// </param>
       /// <returns>
       ///    new extent
       /// </returns>
       function  getCSExtentFromWGS84( const _extent      : TGIS_Extent ;
                                       const _epsg        : Integer
                                     ) : TGIS_Extent ;

       /// <summary>
       ///   Get extent from epsg to wgs84.
       /// </summary>
       /// <param name="_extent">
       ///   source extent
       /// </param>
       /// <param name="_srs">
       ///   source epsg
       /// </param>
       /// <returns>
       ///    new extent
       /// </returns>
       function  getCSExtentToWGS84  ( const _extent      : TGIS_Extent ;
                                       const _srs         : String
                                     ) : TGIS_Extent ;

       /// <summary>
       ///   Parse WGS84 like bounding box.
       /// </summary>
       /// <param name="_node">
       ///   xml node
       /// </param>
       /// <param name="_extent">
       ///   result extent
       /// </param>
       /// <param name="_order">
       ///   coordinate order
       /// </param>
       /// <param name="_srs">
       ///   source epsg
       /// </param>
       procedure parseWGS84BBox      ( const _node        : IXMLNode ;
                                       var   _extent      : TGIS_Extent ;
                                       const _order       : String ;
                                       const _srs         : String
                                     ) ;

       /// <summary>
       ///   Parse other srs values.
       /// </summary>
       /// <param name="_node">
       ///   xml node
       /// </param>
       /// <param name="_list">
       ///   list to fill with srs values
       /// </param>
       procedure parseOtherSRS       ( const _node        : IXMLNode ;
                                       const _list        : TStrings
                                     ) ;

       /// <summary>
       ///   Read feature type schema.
       /// </summary>
       /// <param name="_feature">
       ///   feature object
       /// </param>
       /// <returns>
       ///    stream data
       /// </returns>
       function readFeatureTypeSchema( const _feature     : TGIS_WFSFeature
                                     ) : TStream ;
    protected
       /// <summary>
       ///   Destroy an instance.
       /// </summary>
       procedure doDestroy          ; override;

       /// <summary>
       ///   Process tokens for embedded password tokens.
       /// </summary>
       /// <param name="_token">
       ///   token value
       /// </param>
       /// <returns>
       ///   Expanded value of a token or a token name itself if not handled
       /// </returns>
       function  passwordCallBack   ( const _token : String
                                     ) : String ;
     {$IFDEF OXYGENE}
       /// <summary>
       ///   Busy event for fetching the data
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
       ///   Busy event for fetching the data
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
                                            _pos, _end : Integer ;
                                      var   _abort     : Boolean
                                    ) ;
     {$ENDIF}
    public

       /// <summary>
       ///   Create an instance to open a layer.
       /// </summary>
       /// <param name="_vwr">
       ///   viewer handle
       /// </param>
       /// <param name="_layer">
       ///   layer handle
       /// </param>
       constructor Create            ( const _vwr       : IGIS_Viewer ;
                                       const _layer     : TObject
                                     ) ; overload ;

       /// <summary>
       ///   Create an instance for pure file analizing.
       /// </summary>
       constructor Create            ; overload ;

       /// <summary>
       ///   Load capabilities from the server path.
       /// </summary>
       /// <param name="_path">
       ///   server path
       /// </param>
       procedure   Load              ( const _path      : String
                                     ) ;

       /// <summary>
       ///   Get layer data.
       /// </summary>
       /// <param name="_name">
       ///   layer name
       /// </param>
       /// <param name="_extent">
       ///    extent to load the data from
       /// </param>
       /// <param name="_epsg">
       ///    destination epsg
       /// </param>
       /// <returns>
       ///   data stream
       /// </returns>
       function    GetLayerData      ( const _name      : String ;
                                       const _extent    : TGIS_Extent ;
                                       const _epsg      : Integer
                                     ) : TStream ; overload;

       /// <summary>
       ///   Get layer data.
       /// </summary>
       /// <param name="_name">
       ///   layer name
       /// </param>
       /// <param name="_extent">
       ///    extent to load the data from
       /// </param>
       /// <param name="_epsg">
       ///    destination epsg
       /// </param>
       /// <returns>
       ///   data stream
       /// </returns>
       function    GetLayerDataUrl   ( const _name      : String ;
                                       const _extent    : TGIS_Extent ;
                                       const _epsg      : Integer
                                     ) : String ; overload;

       /// <summary>
       ///   Test if srs is supported.
       /// </summary>
       /// <param name="_srs">
       ///   srs name
       /// </param>
       /// <returns>
       ///   True if supported.
       /// </returns>
       function    VerifySRS         ( const _srs       : String
                                     ) : Boolean ;

       /// <summary>
       ///   Get common extent.
       /// </summary>
       /// <param name="_epsg">
       ///   requested epsg code
       /// </param>
       /// <returns>
       ///    new extent
       /// </returns>
       function    GetCommonExtent   ( const _epsg      : Integer
                                     ) : TGIS_Extent ;

       /// <summary>
       ///   Get common epsg code.
       /// </summary>
       /// <returns>
       ///    SRS code
       /// </returns>
       function    GetCommonSRS      : String ;

       /// <summary>
       ///   Get layer extent.
       /// </summary>
       /// <param name="_name">
       ///   layer name
       /// </param>
       /// <param name="_epsg">
       ///   requested epsg code
       /// </param>
       /// <returns>
       ///    new extent
       /// </returns>
       function    GetLayerExtent    ( const _name      : String ;
                                       const _epsg      : Integer
                                     ) : TGIS_Extent ;

       /// <summary>
       ///   Get feature data.
       /// </summary>
       /// <param name="_feature">
       ///   feature object
       /// </param>
       /// <param name="_extent">
       ///   range of data
       /// </param>
       /// <returns>
       ///    True if data is fetched
       /// </returns>
       function    GetFeatureData    ( const _feature  : TGIS_WFSFeature ;
                                       const _extent   : TGIS_Extent
                                     ) : Boolean ;

       /// <summary>
       ///   Build url from a path and a query parameters.
       /// </summary>
       /// <param name="_query">
       ///   query text
       /// </param>
       /// <param name="_path">
       ///   path text
       /// </param>
       /// <returns>
       ///    new url
       /// </returns>
       function    MakeUrl           ( const _query     : String         ;
                                       const _path      : String
                                     ) : String ;

       /// <summary>
       ///   Compute from Coordinate System and URL parameters AxisOrderIgnored
       ///   and AxisOrderReveres a final axis order reversal (other then
       ///   Easting-Norting).
       /// </summary>
       /// <returns>
       ///    True axis order should be reversed
       /// </returns>
       function   IsAxisReversed     : Boolean ;

       /// <summary>
       ///   Describe feature type from schema.
       /// </summary>
       /// <param name="_name">
       ///   layer name
       /// </param>
       /// <returns>
       ///   data stream with schema
       /// </returns>
       function   DescribeFeatureType( const _name      : String
                                     ) : TStream ;
    public

        /// <summary>
        ///   Proxy URL as for ESRI proxy.ashx.
        /// </summary>
        property UserAgent        : String          read  FUserAgent
                                                    write FUserAgent     ;

        /// <summary>
        ///   Proxy URL as for ESRI proxy.ashx.
        /// </summary>
        property ProxyUrl         : String          read  FProxyUrl
                                                    write FProxyUrl      ;

        /// <summary>
        ///   Connection timeout.
        /// </summary>
        property TimeOut          : Integer         read  FTimeOut
                                                    write FTimeOut ;

        /// <summary>
        ///   Number of features.
        /// </summary>
        property FeaturesCount    : Integer         read fget_FeaturesCount ;

        /// <summary>
        ///   Feature list.
        /// </summary>
        /// <param name="_indexOrName">
        ///   index or name of feature
        /// </param>
        property Feature[const _indexOrName: Variant] : TGIS_WFSFeature
                                                    read fget_Feature ;

        /// <summary>
        ///   Service info list.
        /// </summary>
        property ServiceInfo      : TGIS_StringList read FServiceInfo ;

        /// <summary>
        ///   Data formats list.
        /// </summary>
        property DataFormats      : TGIS_StringList read FDataFormats ;

        /// <summary>
        ///   Path url.
        /// </summary>
        property Path             : String          read FPath ;

        /// <summary>
        ///   Error message.
        /// </summary>
        property Error            : String          read FError ;

        /// <summary>
        ///   Predefined layers.
        /// </summary>
        property PredefinedLayers : TGIS_StringList read FPredefinedLayers ;

        /// <summary>
        ///   Predefined bounding box.
        /// </summary>
        property PredefinedBox    : TGIS_Extent     read fget_PredefinedBox ;

        /// <summary>
        ///   Predefined SRS.
        /// </summary>
        property PredefinedSRS    : String          read FPredefinedSrsName ;

        /// <summary>
        ///   Predefined version.
        /// </summary>
        property Version          : String          read FVersion ;

        /// <summary>
        ///   Metadata list.
        /// </summary>
        property Metadata         : TGIS_StringList read FMetadata ;

        /// <summary>
        ///   Is paging enabled for data fetching.
        /// </summary>
        property PagingEnabled    : Boolean         read  FPagingEnabled
                                                    write FPagingEnabled ;
        /// <summary>
        ///   Page size used for data fetching.
        /// </summary>
        property PageSize         : Integer         read  FPageSize
                                                    write FPageSize ;
        /// <summary>
        ///   If true, use options for fast query structure.
        /// </summary>
        property QueryStructure   : Boolean         read  FQueryStructure
                                                    write fset_QueryStructure ;
     published //events
        /// <event/>
        /// <summary>
        ///   Password event.
        /// </summary>
        property PasswordEvent    : TGIS_TemplateProducerEvent
                                                    read  FOnPassword
                                                    write FOnPassword ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Variants,
    GisClasses,
    GisCsSystems,
    GisCsFactory,
    GisFunctions,
    GisInternals,
    GisLayer,
    GisLayerGML,
    GisCompression,
    GisResource ;
{$ENDIF}

const
  WFS_VERSION = '1.1.0' ;

  WFSFEATURE_THREAD_WAITING      = 0 ;
  WFSFEATURE_THREAD_RUNNING      = 1 ;
  WFSFEATURE_THREAD_DONE         = 2 ;
  WFSFEATURE_THREAD_ERROR        = 3 ;

type
  { Thread to fetch a single feature.
  }
  T_WFS_thread = class( TThread )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      Parent      : TGIS_FileWFS ;
      Feature     : TGIS_WFSFeature ;
      Query       : String ;
    protected
      // <summary>
      //  Execute feature loading thread.
      // </summary>
      procedure Execute ; override;
  end ;

  procedure T_WFS_thread.Execute ;
  var
    r : TGIS_HttpResponse ;
  begin
    if Feature.LastUrl <> Query then begin
      FreeObject( Feature.Data ) ;

      try
        r := Parent.fetchHttp( Query, Parent.sUserName, Parent.sUserPass ) ;

        Feature.HTTPStatus := r.Status ;

        if r.Status = GIS_HTTP_OK then begin
          Feature.Data := r.Stream ;

          Feature.Data.Position := 0 ;
          Feature.LastUrl := Query ;
          Feature.State   := WFSFEATURE_THREAD_DONE ;
        end
        else begin
          Feature.Data    := r.Stream ;
          Feature.LastUrl := Query ;
          Feature.State   := WFSFEATURE_THREAD_ERROR ;
        end ;
      except
        Feature.State := WFSFEATURE_THREAD_ERROR ;
      end ;
    end
    else begin
      if assigned( Feature.Data ) then
        Feature.Data.Position := 0 ;
      Feature.State   := WFSFEATURE_THREAD_DONE ;
    end ;
  end ;

//==============================================================================
// TGIS_WFSFeature
//==============================================================================

  constructor TGIS_WFSFeature.Create ;
  begin
    inherited ;

    OtherSRS := TStringList.Create ;
    {$IFDEF GIS_NORECORDS}
      WGS84BBox := new TGIS_Extent ;
    {$ENDIF}
    State := WFSFEATURE_THREAD_WAITING ;
  end ;

  procedure TGIS_WFSFeature.doDestroy ;
  begin
    FreeObject( OtherSRS ) ;
    FreeObject( Data     ) ;

    inherited ;
  end ;

  function TGIS_WFSFeature.IsDataReady : Boolean ;
  begin
    Result := ( State = WFSFEATURE_THREAD_DONE  ) or
              ( State = WFSFEATURE_THREAD_ERROR )  ;
  end ;

  function TGIS_WFSFeature.IsDataValid : Boolean ;
  begin
    Result := ( State <> WFSFEATURE_THREAD_ERROR )  ;
  end ;

  procedure TGIS_WFSFeature.ResetState ;
  begin
    State := WFSFEATURE_THREAD_WAITING ;
  end ;

  procedure TGIS_WFSFeature.ResetDataPage ;
  begin
    FStartIndex := 0 ;
    FReadCount  := 0 ;
  end ;

  procedure TGIS_WFSFeature.NextDataPage( const _offset : Integer ) ;
  begin
    FStartIndex := FStartIndex + _offset ;
  end ;

  procedure TGIS_WFSFeature.IncreaseDataCount( const _count : Integer ) ;
  begin
    FReadCount := FReadCount + _count ;
  end;

//==============================================================================
// TGIS_FileWFS
//==============================================================================

  constructor TGIS_FileWFS.Create(
    const _vwr    : IGIS_Viewer ;
    const _layer  : TObject
  ) ;
  begin
    inherited Create ;

    xDoc := TGIS_XMLDocument.Create ;
    xDoc.ParseOptions := [ TParseOption.poPreserveWhiteSpace ] ;

    oFeatureTypeList   := TGIS_ObjectList.Create ;
    FMetadata          := TGIS_StringList.Create ;
    FDataFormats       := TGIS_StringList.Create ;
    FServiceInfo       := TGIS_StringList.Create ;
    oLayer             := _layer ;
    oViewer            := _vwr ;
    bAxisOrderReversed := False ;
    bAxisOrderIgnored  := False ;
    bBBoxIgnored       := False ;
    FQueryStructure    := False ;

    FPredefinedLayers := TGIS_StringList.Create ;
  end ;

  constructor TGIS_FileWFS.Create ;
  begin
    Create( nil, nil ) ;
  end ;

  procedure TGIS_FileWFS.doDestroy ;
  begin
    FreeObject( oFeatureTypeList  ) ;
    FreeObject( FPredefinedLayers ) ;
    FreeObject( FMetadata         ) ;
    FreeObject( FDataFormats      ) ;
    FreeObject( FServiceInfo      ) ;

    FreeObject( xDoc ) ;

    inherited ;
  end ;

  function TGIS_FileWFS.formUrl(
    const _query : String ;
    const _path  : String
  ) : String ;
  var
    k     : Integer ;
    spath  : String  ;
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

  function TGIS_FileWFS.fetchHttp(
    const _url     : String  ;
    const _user    : String  ;
    const _pass    : String
  ) : TGIS_HttpResponse ;
  begin
    Result := TGIS_WebUtils.HttpFetch( FProxyUrl + _url,
                         nil,
                         {$IFDEF OXYGENE}
                          @doBusy,
                         {$ELSE}
                          doBusy,
                         {$ENDIF}
                         True,
                         FTimeOut,
                         FUserAgent,
                         '',
                         _user,
                         _pass
                        ) ;
  end ;

  function TGIS_FileWFS.getAttributeValue(
    const _node : IXMLNode ;
    const _key  : String
  ) : OleVariant ;
  var
    attr : IXMLNode ;
  begin
    Result := Unassigned ;
    if not assigned( _node ) then exit ;

    attr := _node.AttributeNodes.FindNode( _key ) ;
    if assigned( attr ) then
      Result := attr.Text
  end ;

  function TGIS_FileWFS.getBBOXFilter(
    const _extent : TGIS_Extent
  ) : String ;
  begin
    if not IsAxisReversed then
      Result := Format( '&BBOX=%s,%s,%s,%s',
                        [ DotFloatToStr( _extent.XMin ),
                          DotFloatToStr( _extent.YMin ),
                          DotFloatToStr( _extent.XMax ),
                          DotFloatToStr( _extent.YMax ) ]
                      )
    else
      Result := Format( '&BBOX=%s,%s,%s,%s',
                        [ DotFloatToStr( _extent.YMin ),
                          DotFloatToStr( _extent.XMin ),
                          DotFloatToStr( _extent.YMax ),
                          DotFloatToStr( _extent.XMax ) ]
                      )
  end ;

  procedure TGIS_FileWFS.testPath(
    const _path     : String ;
    var _hasGetCap  : Boolean ;
    var _hasGetFea  : Boolean
  ) ;
  begin
    _hasGetCap := Pos( UpperCase( 'request=GetCapabilities' ),
                       UpperCase( _path )
                     ) > StringFirst ;
    _hasGetFea := Pos( UpperCase( 'request=GetFeature'      ),
                       UpperCase( _path )
                     ) > StringFirst ;
  end ;

  function TGIS_FileWFS.getElementValue(
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

  function TGIS_FileWFS.getElementAttribute(
    const _node : IXMLNode ;
    const _key  : String ;
    const _attr : String
  ) : String ;
  var
    elm  : IXMLNode ;
  begin
    if not assigned( _node ) then exit ;

    elm := _node.ChildNodes.FindNode( _key ) ;

    if assigned( elm ) then
      Result := VarToString( getAttributeValue( elm, _attr ) )
    else
      Result := '' ;
  end ;

  function TGIS_FileWFS.fget_Feature(
    const _indexOrName: Variant
  ) : TGIS_WFSFeature ;
  var
    i : Integer ;
  begin
    Result := nil ;
    if VarIsOrdinal(_indexOrName) then begin
      i := VarToInt32( _indexOrName ) ;
      if (i >= 0 ) or ( i < oFeatureTypeList.Count ) then
        Result := TGIS_WFSFeature( oFeatureTypeList[i] ) ;
    end
    else begin
      for i := 0 to oFeatureTypeList.Count-1 do
        if TGIS_WFSFeature( oFeatureTypeList[i] ).Name = VarToString( _indexOrName ) then begin
          Result := TGIS_WFSFeature( oFeatureTypeList[i] ) ;
          break ;
        end ;
    end ;
  end ;

  function TGIS_FileWFS.fget_FeaturesCount : Integer ;
  begin
    Result := oFeatureTypeList.Count ;
  end ;

  procedure TGIS_FileWFS.parseOtherSRS(
    const _node : IXMLNode ;
    const _list : TStrings
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _node.ChildNodes.Count - 1 do begin
      if _node.ChildNodes[ i ].LocalName = 'OtherSRS' then begin
        _list.Add( _node.ChildNodes[ i ].Text ) ;
      end
      else if _node.ChildNodes[ i ].LocalName = 'OtherCRS' then begin
        _list.Add( _node.ChildNodes[ i ].Text ) ;
      end ;
    end ;
  end ;

  procedure TGIS_FileWFS.parseWGS84BBox(
    const _node   : IXMLNode ;
      var _extent : TGIS_Extent ;
    const _order  : String ;
    const _srs    : String
  ) ;
  var
    box  : IXMLNode ;
    tkn  : TGIS_Tokenizer ;
    i    : Integer ;

      function getAttributeValue( const _node : IXMLNode ;
                                  const _key  : String
                                ) : OleVariant ;
      var
        attr : IXMLNode ;
      begin
        Result := Unassigned ;
        if not assigned( _node ) then exit ;

        attr := _node.AttributeNodes.FindNode( _key ) ;
        if assigned( attr ) then
          Result := attr.Text
      end ;

  begin
    box := nil ;
    for i := 0 to _node.ChildNodes.Count - 1 do begin
      if _node.ChildNodes[ i ].NodeName = 'ows:WGS84BoundingBox' then begin
        box := _node.ChildNodes[ i ] ;
        break ;
      end ;
    end ;

    if box <> nil then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        for i := 0 to box.ChildNodes.Count - 1 do begin
          if box.ChildNodes[ i ].NodeName = 'ows:LowerCorner' then begin
            tkn.ExecuteEx( box.ChildNodes[ i ].Text, ' ' ) ;
            if tkn.Result.Count > 1 then begin
              if _order = 'XX' then begin
                _extent.XMin := DotStrToFloat( tkn.Result[ 0 ] ) ;
                _extent.XMax := DotStrToFloat( tkn.Result[ 1 ] ) ;
              end
              else begin
                _extent.XMin := DotStrToFloat( tkn.Result[ 0 ] ) ;
                _extent.YMin := DotStrToFloat( tkn.Result[ 1 ] ) ;
              end ;
            end ;
          end
          else if box.ChildNodes[ i ].NodeName = 'ows:UpperCorner' then begin
            tkn.ExecuteEx( box.ChildNodes[ i ].Text, ' ' ) ;
            if tkn.Result.Count > 1 then begin
              if _order = 'XX' then begin
                _extent.YMin := DotStrToFloat( tkn.Result[ 0 ] ) ;
                _extent.YMax := DotStrToFloat( tkn.Result[ 1 ] ) ;
              end
              else begin
                _extent.XMax := DotStrToFloat( tkn.Result[ 0 ] ) ;
                _extent.YMax := DotStrToFloat( tkn.Result[ 1 ] ) ;
              end ;
            end ;
          end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end
    else begin
      box := _node.ChildNodes.FindNode( 'LatLongBoundingBox' ) ;
      if box <> nil then begin
        _extent.XMin := DotStrToFloat( VarToString( getAttributeValue( box, 'minx' ) ) ) ;
        _extent.YMin := DotStrToFloat( VarToString( getAttributeValue( box, 'miny' ) ) ) ;
        _extent.XMax := DotStrToFloat( VarToString( getAttributeValue( box, 'maxx' ) ) ) ;
        _extent.YMax := DotStrToFloat( VarToString( getAttributeValue( box, 'maxy' ) ) ) ;
      end ;
      // Despite the name LatLongBoundingBox, the coordinates are supposed to
      // be expressed in <SRS>. But some servers reproject to WGS84.
      if not GisIsContainExtent( _extent, TGIS_CSFactory.ByEPSG(4326).ValidityExtent ) then
        _extent := getCSExtentToWGS84( _extent, _srs) ;
    end ;
  end ;

  procedure TGIS_FileWFS.parsePredefined(
    const _path : String
  ) ;
  var
    sservice   : String  ;
    stmp       : String  ;
    stype      : String  ;
    tkn        : TGIS_Tokenizer ;
  begin
    sservice                := URLGetAndDeleteParameterValue( FPath, 'SERVICE'     ) ;
                               URLGetAndDeleteParameterValue( FPath, 'REQUEST'     ) ;
    sPredefinedVersion      := URLGetAndDeleteParameterValue( FPath, 'VERSION'     ) ;
    sPredefinedBox          := URLGetAndDeleteParameterValue( FPath, 'BBOX'        ) ;
    sPredefinedCQFilter     := URLGetAndDeleteParameterValue( FPath, 'CQL_FILTER'  ) ;
    sPredefinedFilter       := URLGetAndDeleteParameterValue( FPath, 'FILTER'      ) ;
    stype                   := URLGetAndDeleteParameterValue( FPath, 'TYPENAME'    ) ;
    sUserName               := URLGetAndDeleteParameterValue( FPath, 'USER'        ) ;
    sUserPass               := URLGetAndDeleteParameterValue( FPath, 'PASS'        ) ;
    sPredefinedStartIndex   := URLGetAndDeleteParameterValue( FPath, 'STARTINDEX'  ) ;
    FPredefinedSrsName      := URLGetAndDeleteParameterValue( FPath, 'SRSNAME'     ) ;
    sPredefinedMaxFeatures  := URLGetAndDeleteParameterValue( FPath, 'MAXFEATURES' ) ;
    if IsStringEmpty( sPredefinedMaxFeatures ) then
      sPredefinedMaxFeatures:= URLGetAndDeleteParameterValue( FPath, 'COUNT' ) ;
    sPredefinedOutputFormat := URLGetAndDeleteParameterValue( FPath, 'OUTPUTFORMAT') ;

    if not IsStringEmpty( sPredefinedMaxFeatures ) then
      FMaxFeatures := StrToIntDef( sPredefinedMaxFeatures, 0 )
    else
      FMaxFeatures := 0 ;

    if FMetadata.Count > 0 then begin
      // old parameter
      stmp               := LstReadString(
                              FMetadata,
                              GIS_INI_AXIS_ORDER,
                              ''
                            ) ;
      if stmp = GIS_INI_AXIS_ORDER_NE then
        bAxisOrderReversed := True ;

      bAxisOrderReversed := LstReadBoolean(
                              FMetadata,
                              GIS_INI_AXIS_ORDER_REVERSED,
                              bAxisOrderReversed
                            ) ;
      bAxisOrderIgnored  := LstReadBoolean(
                              FMetadata,
                              GIS_INI_AXIS_ORDER_IGNORED,
                               bAxisOrderIgnored
                            ) ;
      bBBoxIgnored  := LstReadBoolean(
                              FMetadata,
                              GIS_INI_BBOX_IGNORED,
                               bBBoxIgnored
                            ) ;
      sExtentOrder       := LstReadString(
                              FMetadata,
                              'ExtentOrder',
                              ''
                            ) ;
    end
    else begin
      // old parameter
      stmp               := URLGetAndDeleteParameterValue(
                              FPath,
                              GIS_INI_AXIS_ORDER
                            ) ;
      if stmp = GIS_INI_AXIS_ORDER_NE then
        bAxisOrderReversed := True ;

      bAxisOrderReversed := StrToBoolean(
                              URLGetAndDeleteParameterValue(
                                FPath,
                                GIS_INI_AXIS_ORDER_REVERSED
                              ),
                              bAxisOrderReversed
                            ) ;
      bAxisOrderIgnored  := StrToBoolean(
                              URLGetAndDeleteParameterValue(
                                FPath,
                                GIS_INI_AXIS_ORDER_IGNORED
                              ),
                              bAxisOrderIgnored
                            ) ;
      bBBoxIgnored  := StrToBoolean(
                              URLGetAndDeleteParameterValue(
                                FPath,
                                GIS_INI_BBOX_IGNORED
                              ),
                              bBBoxIgnored
                            ) ;

      sExtentOrder       := URLGetAndDeleteParameterValue(
                              FPath,
                              'EXTENTORDER'
                            ) ;
      FPagingEnabled  := StrToBoolean(
                              URLGetAndDeleteParameterValue(
                                FPath,
                                'PAGINGENABLED'
                              ),
                              FPagingEnabled
                            ) ;
      stmp               := URLGetAndDeleteParameterValue(
                              FPath,
                              'PAGESIZE'
                            ) ;
      if not IsStringEmpty( stmp ) then
        FPageSize := StrToIntDef( stmp, FPageSize ) ;
    end ;

    bAxisOrderReversed := GisMetadataAsBoolean(
                            'TGIS_LayerWFS.AxisOrderReversed',
                            bAxisOrderReversed
                          ) ;
    bAxisOrderReversed := StrToBoolean(
                            URLGetAndDeleteParameterValue(
                                FPath,
                                GIS_INI_AXIS_ORDER_REVERSED
                            ),
                            bAxisOrderReversed
                          ) ;
    bAxisOrderIgnored  := GisMetadataAsBoolean(
                            'TGIS_LayerWFS.AxisOrderIgnored',
                            bAxisOrderIgnored
                          ) ;
    bBBoxIgnored  := GisMetadataAsBoolean(
                            'TGIS_LayerWFS.BBoxIgnored',
                            bBBoxIgnored
                          ) ;
    bAxisOrderIgnored  := StrToBoolean(
                            URLGetAndDeleteParameterValue(
                                FPath,
                                GIS_INI_AXIS_ORDER_IGNORED
                            ),
                            bAxisOrderIgnored
                          ) ;
    bBBoxIgnored  := StrToBoolean(
                            URLGetAndDeleteParameterValue(
                                FPath,
                                GIS_INI_BBOX_IGNORED
                            ),
                            bBBoxIgnored
                          ) ;

    {$IFNDEF OXYGENE}
      sUserName := TemplateProducer( sUserName, nil,  passwordCallBack, False ) ;
      sUserPass := TemplateProducer( sUserPass, nil,  passwordCallBack, False ) ;
    {$ELSE}
      sUserName := TemplateProducer( sUserName, nil, @passwordCallBack, False ) ;
      sUserPass := TemplateProducer( sUserPass, nil, @passwordCallBack, False ) ;
    {$ENDIF}

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( stype, [';'], False );
      FPredefinedLayers.Assign( tkn.Result );
    finally
      FreeObject( tkn ) ;
    end ;

    if IsStringEmpty( sservice ) then
      sservice := 'WFS' ;
  end ;

  procedure TGIS_FileWFS.readCapabilities(
    const _path : String
  ) ;
  var
    query      : String ;
    ver        : String  ;
    has_getcap : Boolean ;
    has_getfea : Boolean ;
    r          : TGIS_HttpResponse ;
    lst        : TStringList ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      ver := sPredefinedVersion
    else
      ver := '' ;

    if not IsStringEmpty( ver ) then
      query := formUrl( Format( 'VERSION=%s&SERVICE=WFS&REQUEST=GetCapabilities',
                                [ ver ]
                               ), FPath
                       )
    else
      query := formUrl( 'SERVICE=WFS&REQUEST=GetCapabilities',
                        FPath
                       ) ;
    sError := '' ;
    r.Stream := nil ;
    try
      testPath( _path, has_getcap, has_getfea ) ;

      if not has_getcap then
        r := fetchHttp( query, sUserName, sUserPass )
      else
        r := fetchHttp( _path, sUserName, sUserPass ) ;

      if r.Status = GIS_HTTP_OK then
        xDoc.LoadFromStream( r.Stream )
      else begin
        lst := TStringList.Create ;
        try
          lst.LoadFromStream( r.Stream ) ;
          sError := Format( _rsrc( GIS_RS_ERR_SERVER_ERROR ), [ lst.Text ] ) ;
        finally
          FreeObject( lst ) ;
        end ;
      end ;
    finally
      FreeObject( r.Stream ) ;
    end ;

    if xDoc.Active then
      parse ;

    FError := sError ;
  end ;

  procedure TGIS_FileWFS.readPredefined(
    const _path : String
  ) ;
  var
    i  : Integer ;
    ft : TGIS_WFSFeature ;
  begin
    FVersion  := sPredefinedVersion ;
    if IsStringEmpty( FVersion ) then
      FVersion := WFS_VERSION ;

    for i := 0 to FPredefinedLayers.Count - 1 do begin
      ft := TGIS_WFSFeature.Create ;
      ft.Name        := FPredefinedLayers[i] ;
      ft.Title       := FPredefinedLayers[i] ;
      ft.Description := FPredefinedLayers[i] ;

      ft.DefaultSRS  := FPredefinedSrsName ;
      ft.ExtentOrder := sExtentOrder ;
      ft.WGS84BBox   := getCSExtentToWGS84( PredefinedBox, FPredefinedSrsName ) ;

      oFeatureTypeList.Add( ft ) ;
    end ;
  end ;

  procedure TGIS_FileWFS.parse ;
  var
    root : IXMLNode ;
    list : IXMLNodeList ;
    listo: IXMLNodeList ;
    node : IXMLNode ;
    i    : Integer ;
    j    : Integer ;
    k    : Integer ;
    ft   : TGIS_WFSFeature ;
    pref : String ;
  begin
    root := xDoc.DocumentElement ;

    if root = nil then exit ;

    FVersion  := VarToString( getAttributeValue( root, 'version' ) ) ;
    for i := 0 to root.ChildNodes.Count-1 do begin
      if root.ChildNodes[i].LocalName = 'ServiceIdentification'  then begin
        FServiceInfo.Add( getElementValue( root.ChildNodes[i], 'Title' ) ) ;
        FServiceInfo.Add( getElementValue( root.ChildNodes[i], 'Abstract' ) ) ;
        FServiceInfo.Add( getElementValue( root.ChildNodes[i], 'ServiceType' ) ) ;
        FServiceInfo.Add( getElementValue( root.ChildNodes[i], 'ServiceTypeVersion' ) ) ;
      end
      else if root.ChildNodes[i].LocalName = 'OperationsMetadata'  then begin
        listo := nil ;
        list := root.ChildNodes[i].ChildNodes ;
        for k := 0 to list.Count - 1 do
          if ( list.Nodes[k].LocalName = 'Operation' ) and
             ( VarToString( list.Nodes[k].Attributes['name'] ) = 'GetFeature' ) then
            for j := 0 to list.Nodes[k].ChildNodes.Count-1 do
              if ( list.Nodes[k].ChildNodes[j].LocalName = 'Parameter' ) and
                 ( VarToString( list.Nodes[k].ChildNodes[j].Attributes['name'] ) = 'outputFormat') then begin
                listo := list.Nodes[k].ChildNodes[j].ChildNodes['AllowedValues'].ChildNodes ;
                break ;
               end ;

        if assigned( listo ) then
          for k := 0 to listo.Count - 1 do
            FDataFormats.Add( listo[k].Text ) ;
      end ;
    end ;

    node := root.ChildNodes.FindNode( 'FeatureTypeList' ) ;
    if assigned( node ) then
      list := node.ChildNodes
    else begin
      node := root.ChildNodes.FindNode( 'Exception' ) ;
      if assigned( node ) then
        sError := getElementValue( node, 'ExceptionText' )
      else
        sError := root.XML ;
      exit ;
    end ;

    for i := 0 to list.Count - 1 do begin
      if list.Nodes[ i ].LocalName = 'FeatureType' then begin
        ft := TGIS_WFSFeature.Create ;
        ft.Name := getElementValue( list.Nodes[ i ], 'Name' ) ;
        k := Pos( ':', ft.Name ) ;
        if k > StringFirst then begin
          pref := &Copy( ft.Name, StringFirst, k-1 ) ;
          ft.NamespaceUrl := getElementAttribute( list.Nodes[ i ], 'Name', 'xmlns:' + pref ) ;
        end ;

        ft.Title      := getElementValue( list.Nodes[ i ], 'Title' ) ;
        ft.Description:= getElementValue( list.Nodes[ i ], 'Abstract' ) ;

        ft.DefaultSRS := getElementValue( list.Nodes[ i ], 'DefaultSRS' ) ;
        if IsStringEmpty( ft.DefaultSRS ) then
          ft.DefaultSRS := getElementValue( list.Nodes[ i ], 'SRS' ) ;
        if IsStringEmpty( ft.DefaultSRS ) then
          ft.DefaultSRS := getElementValue( list.Nodes[ i ], 'DefaultCRS' ) ;

        ft.MetadataUrl := getElementAttribute( list.Nodes[ i ], 'MetadataURL', 'xlink:href' ) ;
        ft.ExtentOrder := sExtentOrder ;
        parseWGS84BBox( list.Nodes[ i ], ft.WGS84BBox, sExtentOrder, ft.DefaultSRS ) ;
        parseOtherSRS ( list.Nodes[ i ], ft.OtherSRS ) ;
        oFeatureTypeList.Add( ft ) ;
      end ;
    end ;

  end ;

  function TGIS_FileWFS.readFeatureData(
    const _feature : TGIS_WFSFeature ;
    const _extent  : TGIS_Extent
  ) : Boolean ;
  begin
    Result := readFeatureData( _feature, _extent, '', True ) ;
  end ;

  function TGIS_FileWFS.readFeatureData(
    const _feature  : TGIS_WFSFeature ;
    const _extent   : TGIS_Extent ;
    const _filter   : String ;
    const _readData : Boolean
   ) : Boolean ;
  var
    th    : T_WFS_thread ;
    query : String ;
    page_size : Integer ;
  begin
    Result := True ;

    if _feature.State = WFSFEATURE_THREAD_DONE    then exit ;
    if _feature.State = WFSFEATURE_THREAD_RUNNING then exit ;

    if _readData then begin
      _feature.State := WFSFEATURE_THREAD_RUNNING ;

      if FPagingEnabled then begin

        if (FMaxFeatures > 0) then begin
          if  ((_feature.ReadCount + FPageSize) < FMaxFeatures) then
            page_size := FPageSize
          else
            page_size := FMaxFeatures - _feature.ReadCount ;
        end
        else
          page_size := FPageSize  ;

        if page_size = 0 then begin
          Result := False ;
          exit ;
        end ;

        query := buildURL( _feature.Name, _feature.NamespaceUrl, _extent, _filter, _feature.StartIndex, page_size ) ;
      end
      else begin
        if FQueryStructure then
          query := buildURL( _feature.Name, _feature.NamespaceUrl, _extent, _filter, 0, 1 )
        else
          query := buildURL( _feature.Name, _feature.NamespaceUrl, _extent, _filter, -1, -1 ) ;
      end;

      th := T_WFS_thread.Create( True ) ;
      th.Parent   := Self ;
      th.Feature  := _feature ;
      th.Query    := query ;
      th.FreeOnTerminate := True ;
      th.Start ;
    end
    else
      _feature.LastUrl := buildURL( _feature.Name, _feature.NamespaceUrl, _extent, _filter, -1, -1 ) ;
  end ;

  function TGIS_FileWFS.buildURL(
    const _name        : String ;
    const _namespace   : String ;
    const _extent      : TGIS_Extent ;
    const _filter      : String ;
    const _startIndex  : Integer ;
    const _maxFeatures : Integer
  ) : String ;
  var
    query : String  ;
    ver   : String  ;
    k     : Integer ;
    pref  : String  ;
    srs   : String  ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      ver := sPredefinedVersion
    else
      ver := Version ;

    if Pos( 'HTTP://WWW.OPENGIS.NET/GML/SRS/EPSG.XML#', UpperCase(FPredefinedSrsName) ) = StringFirst then
      srs := Copy( FPredefinedSrsName, StringFirst+40, length(FPredefinedSrsName)-40 )
    else if Pos( 'HTTP://WWW.OPENGIS.NET/DEF/CRS/EPSG/0/', UpperCase(FPredefinedSrsName) ) = StringFirst then
      srs := Copy( FPredefinedSrsName, StringFirst+38, length(FPredefinedSrsName)-38 )
    else
      srs := FPredefinedSrsName ;

    query := formUrl( Format( 'SERVICE=WFS&VERSION=%s&REQUEST=GetFeature', [ver] ), FPath ) ;
    if ver = '2.0.0' then
      query := query + Format( '&TYPENAMES=%s', [_name] )
    else
      query := query + Format( '&TYPENAME=%s', [_name] ) ;

    // Only one of the parameters RESOURCEID, FILTER and BBOX may be specified
    if not IsStringEmpty( sPredefinedBox ) then
      query := query + Format( '&BBOX=%s', [sPredefinedBox] )
    else if not IsStringEmpty( sPredefinedFilter ) then
      query := query + Format( '&FILTER=%s', [sPredefinedFilter] )
    else if not IsStringEmpty( sPredefinedCQFilter ) then
      query := query + Format( '&CQL_FILTER=%s', [URLEncode(sPredefinedCQFilter)] )
    else if not GisIsNoWorld( _extent ) and not bBBoxIgnored then begin
      query := query + getBBOXFilter( _extent ) ;
      if not ver.StartsWith( '1.0') and not IsStringEmpty( FPredefinedSrsName ) then
        query := query + Format( ',%s', [srs] ) ;
    end
    else
      if not IsStringEmpty( _filter ) then
        query := query + _filter ;

    if not IsStringEmpty( sPredefinedMaxFeatures ) and ( _maxFeatures = 0 ) then begin
      if ver = '2.0.0' then
        query := query + Format( '&COUNT=%s', [sPredefinedMaxFeatures] )
      else
        query := query + Format( '&MAXFEATURES=%s', [sPredefinedMaxFeatures] ) ;
    end
    else if _maxFeatures > 0 then begin
      if ver = '2.0.0' then
        query := query + Format( '&COUNT=%d', [_maxFeatures] )
      else
        query := query + Format( '&MAXFEATURES=%d', [_maxFeatures] ) ;
    end ;

    if not IsStringEmpty( sPredefinedStartIndex ) and ( _maxFeatures = 0 ) then
      query := query + Format( '&STARTINDEX=%s', [sPredefinedStartIndex] )
    else if _startIndex >= 0 then
      query := query + Format( '&STARTINDEX=%d', [_startIndex] ) ;

    if not IsStringEmpty( FPredefinedSrsName ) then
      query := query + Format( '&SRSNAME=%s', [FPredefinedSrsName] ) ;

    if not IsStringEmpty( sPredefinedOutputFormat ) then begin
      query := query + Format( '&OUTPUTFORMAT=%s', [URLEncode(sPredefinedOutputFormat)] ) ;
    end ;

    if not IsStringEmpty( _namespace ) then begin
      k := Pos( ':', _name ) ;
      if k > StringFirst then begin
        pref := &Copy( _name, StringFirst, k-1 ) ;
        if ver = '2.0.0' then
          query := query + Format( '&NAMESPACES=xmlns(%s,%s)', [pref, _namespace] )
        else
          query := query + Format( '&NAMESPACE=xmlns(%s=%s)', [pref, _namespace] ) ;
      end ;
    end ;

    Result := query ;
  end ;


  function TGIS_FileWFS.fget_PredefinedBox : TGIS_Extent ;
  var
    tkn : TGIS_Tokenizer ;
  begin
    Result := GisNoWorld ;
    try
      if not IsStringEmpty( sPredefinedBox ) then begin

        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( sPredefinedBox, [','], False );
          if tkn.Result.Count = 4 then
            Result := GisExtent( DotStrToFloat( tkn.Result[ 0 ] ),
                                 DotStrToFloat( tkn.Result[ 1 ] ),
                                 DotStrToFloat( tkn.Result[ 2 ] ),
                                 DotStrToFloat( tkn.Result[ 3 ] )
                                ) ;
        finally
          FreeObject( tkn ) ;
        end ;
      end ;
    except
    end ;
  end ;

  procedure TGIS_FileWFS.fset_QueryStructure(
    const _value : Boolean
  ) ;
  begin
    FQueryStructure := _value ;
    if _value then begin
      oldMaxFeatures := FMaxFeatures ;
      FMaxFeatures   := 1 ;
    end
    else begin
      FMaxFeatures := oldMaxFeatures ;
    end ;
  end ;

  function TGIS_FileWFS.readFeatureTypeSchema(
    const _feature : TGIS_WFSFeature
  ) : TStream ;
  var
    query : String ;
    ver   : String ;
    r     : TGIS_HttpResponse ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      ver := sPredefinedVersion
    else
      ver := '1.1.0' ;

    query := formUrl( Format('SERVICE=WFS&REQUEST=DescribeFeatureType&VERSION=%s&TYPENAME=%s',
                             [ ver, _feature.Name ] ),
                      FPath
                     ) ;
    Result := nil ;
    r := fetchHttp( query, sUserName, sUserPass ) ;
    if r.Status = GIS_HTTP_OK then
      Result := r.Stream ;
  end ;

  function TGIS_FileWFS.VerifySRS(
    const _srs : String
  ) : Boolean ;
  var
    i, j : Integer ;
  begin
    Result := False ;

    for i := 0 to oFeatureTypeList.Count - 1 do begin
      if Pos( _srs, TGIS_WFSFeature( oFeatureTypeList[ i ] ).DefaultSRS ) > StringFirst then
        Result := True
      else begin
        for j := 0 to TGIS_WFSFeature( oFeatureTypeList[ i ] ).OtherSRS.Count - 1 do
          if Pos( _srs, TGIS_WFSFeature( oFeatureTypeList[ i ] ).OtherSRS[ j ] ) > StringFirst then
            Result := True ;
      end ;
    end ;
  end ;

  function TGIS_FileWFS.GetLayerData(
    const _name   : String ;
    const _extent : TGIS_Extent ;
    const _epsg   : Integer
   ) : TStream ;
  var
    i : Integer ;
  begin
    Result := nil ;
    for i := 0 to oFeatureTypeList.Count - 1 do
      if TGIS_WFSFeature( oFeatureTypeList[ i ] ).Name = _name then begin

        readFeatureData( TGIS_WFSFeature( oFeatureTypeList[ i ] ),
                         _extent,
                         '',
                         True
                       ) ;

        Result := TGIS_WFSFeature( oFeatureTypeList[ i ] ).Data ;
        break ;
      end ;
  end ;

  function TGIS_FileWFS.GetLayerDataUrl(
    const _name       : String ;
    const _extent     : TGIS_Extent ;
    const _epsg       : Integer
   ) : String ;
  var
    i : Integer ;
  begin
    Result := '' ;
    for i := 0 to oFeatureTypeList.Count - 1 do
      if TGIS_WFSFeature( oFeatureTypeList[ i ] ).Name = _name then begin

        readFeatureData( TGIS_WFSFeature( oFeatureTypeList[ i ] ),
                         _extent,
                         '',
                         False
                        ) ;

        Result := TGIS_WFSFeature( oFeatureTypeList[ i ] ).LastUrl ;
        break ;
      end ;
  end ;

  function TGIS_FileWFS.GetFeatureData(
    const _feature  : TGIS_WFSFeature ;
    const _extent   : TGIS_Extent
  ) : Boolean ;
  begin
    if assigned( _feature ) then
      Result := readFeatureData( _feature, _extent )
    else
      Result := False ;
  end ;

  function TGIS_FileWFS.getCSExtentFromWGS84(
    const _extent : TGIS_Extent ;
    const _epsg   : Integer
  ) : TGIS_Extent ;
  var
    lcs  : TGIS_CSCoordinateSystem ;
    wgs  : TGIS_CSCoordinateSystem ;
  begin
    Result := GisNoWorld ;
    try
      lcs := TGIS_CSFactory.ByEPSG( _epsg ) ;
    except
      lcs := CSUnknownCoordinateSystem ;
    end ;

    wgs := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;

    if not ( lcs is TGIS_CSUnknownCoordinateSystem ) and
       not GisIsNoWorld( _extent ) then
    begin
      Result := lcs.ExtentFromCS( wgs, _extent ) ;
      if not GisIsValidExtent( Result ) then
        Result := GisNoWorld ;
    end ;
  end ;

  function TGIS_FileWFS.getCSExtentToWGS84(
    const _extent : TGIS_Extent ;
    const _srs    : String
  ) : TGIS_Extent ;
  var
    lcs  : TGIS_CSCoordinateSystem ;
    wgs  : TGIS_CSCoordinateSystem ;
  begin
    Result := GisNoWorld ;
    try
      lcs := TGIS_CSFactory.ByWKT( _srs ) ;
    except
      lcs := CSUnknownCoordinateSystem ;
    end ;

    wgs := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;

    if not ( lcs is TGIS_CSUnknownCoordinateSystem ) and
       not GisIsNoWorld( _extent ) then
    begin
      Result := wgs.ExtentFromCS( lcs, _extent ) ;
      if not GisIsValidExtent( Result ) then
        Result := GisNoWorld ;
    end ;
  end ;

  function TGIS_FileWFS.passwordCallBack(
    const _token : String
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

  {$IFDEF OXYGENE}

    procedure TGIS_FileWFS.doBusy(
      const _sender    : Object ;
      const _e         : TGIS_BusyEventArgs
    ) ;
    begin
      if assigned( oViewer ) then
        _e.Abort := oViewer.HourglassShake ;
    end ;
  {$ELSE}

    procedure TGIS_FileWFS.doBusy(
            _sender    : TObject ;
            _pos, _end : Integer ;
      var   _abort     : Boolean
    ) ;
    begin
      if Assigned( oLayer ) then
        _abort := TGIS_Layer( oLayer ).HourglassShake ;
    end ;
  {$ENDIF}

  function TGIS_FileWFS.GetCommonExtent(
    const _epsg : Integer
  ) : TGIS_Extent ;
  var
    i    : Integer ;
    ext  : TGIS_Extent ;
  begin
    ext := GisNoWorld ;

    for i := 0 to oFeatureTypeList.Count - 1 do
      ext := GisMaxExtent(  TGIS_WFSFeature( oFeatureTypeList[ i ] ).WGS84BBox, ext ) ;

    Result := getCSExtentFromWGS84( ext, _epsg ) ;
  end ;

  function TGIS_FileWFS.GetLayerExtent(
    const _name   : String ;
    const _epsg   : Integer
  ) : TGIS_Extent ;
  var
    i   : Integer ;
    ext : TGIS_Extent ;
  begin
    Result := GisNoWorld ;

    for i := 0 to oFeatureTypeList.Count - 1 do
      if TGIS_WFSFeature(oFeatureTypeList[i]).Name = _name then begin
        ext := TGIS_WFSFeature(oFeatureTypeList[i]).WGS84BBox ;

        Result := getCSExtentFromWGS84( ext, _epsg ) ;
        break ;
      end ;
  end ;

  function TGIS_FileWFS.GetCommonSRS : String ;
  var
    i : Integer ;
  begin
    Result := '' ;
    for i := 0 to oFeatureTypeList.Count - 1 do begin
      Result := TGIS_WFSFeature( oFeatureTypeList[ i ] ).DefaultSRS ;
      if not IsStringEmpty( Result ) then
        break ;
    end ;
  end ;

  procedure TGIS_FileWFS.Load(
    const _path : String
  ) ;
  var
    has_getcap : Boolean ;
    has_getfea : Boolean ;
  begin
    FPath := _path ;

    oFeatureTypeList.Clear ;
    FMetadata.Clear ;
    FServiceInfo.Clear ;
    FDataFormats.Clear ;

    parsePredefined( FPath ) ;

    testPath( _path, has_getcap, has_getfea ) ;
    if not has_getfea then
      readCapabilities( _path )
    else
      readPredefined( _path ) ;
  end ;

  function TGIS_FileWFS.MakeUrl(
    const _query  : String ;
    const _path   : String
  ): String;
  begin
    Result := formUrl( _query, _path ) ;
  end ;

  function TGIS_FileWFS.IsAxisReversed : Boolean ;
  var
    brev : Boolean ;
  begin
    Result := False ;
    brev := False ;
    if assigned( TGIS_Layer( oLayer ).CS ) then begin
      brev := TGIS_Layer( oLayer ).CS.ReversedCoordinates
    end;

    if not bAxisOrderIgnored then
      Result := brev ;

    if bAxisOrderReversed then
      Result := not Result ;
  end;

  function TGIS_FileWFS.DescribeFeatureType(
    const _name : String
  ) : TStream ;
  var
    i   : Integer ;
    fea : TGIS_WFSFeature ;
  begin
    Result := nil ;
    for i := 0 to oFeatureTypeList.Count - 1 do begin
      fea := TGIS_WFSFeature( oFeatureTypeList[ i ] ) ;
      if fea.Name = _name then begin
        Result := readFeatureTypeSchema( fea ) ;
        break ;
      end ;
    end ;
  end;


{==================================== END =====================================}
end.
