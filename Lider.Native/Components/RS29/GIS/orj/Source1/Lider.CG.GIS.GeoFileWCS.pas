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
  WCS provider.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileWCS ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileWCS"'}
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
    System.Generics.Collections,
    System.Types,
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
  ///   WCS coverage bounding box.
  /// </summary>
  TGIS_WCSCoverageBoundingBox = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   WCS coverage coordinate system bounding box.
      /// </summary>
      BoundingBox : TGIS_Extent ;
      /// <summary>
      ///   WCS coverage coordinate system name.
      /// </summary>
      CRS         : String      ;
  end ;

  /// <summary>
  ///   WCS coverage.
  /// </summary>
  TGIS_WCSCoverage = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    public
      /// <summary>
      ///   WCS coverage name.
      /// </summary>
      Name         : String           ;
      /// <summary>
      ///   WCS coverage title.
      /// </summary>
      Title        : String           ;
      /// <summary>
      ///   WCS coverage info.
      /// </summary>
      Info         : String           ;
      /// <summary>
      ///   WCS coverage bounding box in WGS84.
      /// </summary>
      LatLonBBox   : TGIS_Extent      ;
      /// <summary>
      ///   WCS coverage coordinate system name.
      /// </summary>
      CRS          : TGIS_StringList  ;
      /// <summary>
      ///   WCS coverage coordinate system bounding box.
      /// </summary>
      CRSBBox      : TObjectList<TGIS_WCSCoverageBoundingBox> ;
      /// <summary>
      ///   Format available for a coverage.
      /// </summary>
      Formats      : TGIS_StringList  ;
      /// <summary>
      ///   Time position available for a coverage.
      /// </summary>
      TimePosition : TGIS_StringList  ;
    protected
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create  ;
  end ;

  /// <summary>
  ///   Encapsulation of the WCS file.
  /// </summary>
  TGIS_FileWCS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
   private
     {$IFDEF DCC}
       [weak]
     {$ENDIF}
     oViewer            : IGIS_Viewer ;
     WCSCaps            : TObject ;
     FError             : String  ;
     FOnPassword        : TGIS_TemplateProducerEvent ;
     oLayer             : TObject ;
     FUserAgent         : String  ;
     FTimeOut           : Integer ;
     FDefaultCoverage   : String  ;
     FSelectedCRS       : String  ;
     FForcedCRS         : String  ;
     FAxisOrderIgnored  : Boolean ;
     FAxisOrderReversed : Boolean ;
     FIgnoreInternalURI : Boolean ;
   protected
     function  fget_ProxyUrl          : String   ;
     procedure fset_ProxyUrl          ( const _value : String ) ;
     function  fget_LastUrl           : String   ;
     function  fget_ImageFormats      : TStrings ;
     function  fget_AllCoverages      : TStrings ;
     function  fget_AllCRS            : TStrings ;
     function  fget_ForcedCRS         : String   ;
     procedure fset_ForcedCRS         ( const _value : String ) ;
     function  fget_PredefinedVersion : String   ;
     function  fget_PredefinedFormat  : String   ;
     function  fget_PredefinedStyles  : String   ;
     function  fget_PredefinedCoverages : TStrings ;
     function  fget_DefaultCRS        : String   ;
     function  fget_TransmittedBytes  : Int64    ;
     function  fget_ServiceVersion    : String   ;
     function  fget_ServiceInfo       : String   ;
     function  fget_CoveragesCount    : Integer ;
     function  fget_Coverage          ( const _indexOrName : Variant
                                      ) : TGIS_WCSCoverage ;
     function  fget_DescribedSize     : TPoint    ;
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
     ///   Load WCS service.
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
     /// <param name="_coverage">
     ///   requested coverage
     /// </param>
     /// <returns>
     ///    http response
     /// </returns>
     function  GetMap      ( const _extent     : TGIS_Extent ;
                             const _format     : String      ;
                             const _width      : Integer     ;
                             const _height     : Integer     ;
                             const _coverage   : String
                           ) : TGIS_HttpResponse ;
     /// <summary>
     ///   Get extent for CRS.
     /// </summary>
     /// <param name="_crs">
     ///   CRS code
     /// </param>
     /// <returns>
     ///    lary extent for coordinate system.
     /// </returns>
     function  GetExtent     ( const _crs         : String
                             ) : TGIS_Extent ;
     /// <summary>
     ///   Verify if CRS is supported by the coverage.
     /// </summary>
     /// <param name="_crs">
     ///   CRS code
     /// </param>
     /// <returns>
     ///    True if coordinate system is supported.
     /// </returns>
     function  VerifyCRS     ( const _crs         : String
                             ) : Boolean ;
     /// <summary>
     ///   Find a coverage by name.
     /// </summary>
     /// <param name="_name">
     ///   coverage name
     /// </param>
     /// <param name="_cs">
     ///   CS to match from supported list
     /// </param>
     /// <returns>
     ///    coverage object.
     /// </returns>
     function  FindCoverage  ( const _name        : String ;
                               const _cs          : TGIS_CSCoordinateSystem
                             ) : TGIS_WCSCoverage ;
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
     ///   Describe a coverage from request to get more info.
     /// </summary>
     /// <param name="_cov">
     ///   coverage
     /// </param>
     procedure DescribeCoverage( const _cov : TGIS_WCSCoverage
                               ) ;
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
      ///   List of all Coverages.
      /// </summary>
      property ImageFormats :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_ImageFormats  ;

      /// <summary>
      ///   List of all coverages.
      /// </summary>
      property AllCoverages :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_AllCoverages ;

      /// <summary>
      ///   List of all CRS.
      /// </summary>
      property AllCRS  :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_AllCRS ;

      /// <summary>
      ///   Number of coverages.
      /// </summary>
      property CoveragesCount    : Integer         read fget_CoveragesCount ;

      /// <summary>
      ///   Coverage list.
      /// </summary>
      /// <param name="_indexOrName">
      ///   index or name of coverage
      /// </param>
      property Coverage[const _indexOrName: Variant] : TGIS_WCSCoverage
                                                  read fget_Coverage ;
      /// <summary>
      ///   Proxy URL as for ESRI proxy.ashx.
      /// </summary>
      property ProxyUrl            : String     read  fget_ProxyUrl
                                                write fset_ProxyUrl         ;

      /// <summary>
      ///   Selected coordinate system.
      /// </summary>
      property SelectedCRS         : String     read  FSelectedCRS
                                                write FSelectedCRS         ;

      /// <summary>
      ///   Forced coordinate system.
      /// </summary>
      property ForcedCRS         : String       read  fget_ForcedCRS
                                                write fset_ForcedCRS         ;

      /// <summary>
      ///   Default coordinate system.
      /// </summary>
      property DefaultCRS          : String     read  fget_DefaultCRS      ;

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
      ///   Coverages predefined by original URL.
      /// </summary>
      property PredefinedCoverages :
        {$IFDEF OXYGENE}
          TGIS_Strings
        {$ELSE}
          TStrings
        {$ENDIF}
        read fget_PredefinedCoverages  ;

      /// <summary>
      ///   Error message.
      /// </summary>
      property Error              : String  read FError ;

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
      ///   Default coverage name.
      /// </summary>
      property DefaultCoverage    : String  read  FDefaultCoverage ;

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
      ///   Ignore internal URI for requests from Capabilities.
      /// </summary>
      property IgnoreInternalURI  : Boolean read  FIgnoreInternalURI
                                            write FIgnoreInternalURI ;

      /// <summary>
      ///   Described size from Capabilities.
      /// </summary>
      property DescribedSize      : TPoint  read fget_DescribedSize ;

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
  WCS_VERSION_FORCED     = '1.0.0' ;

type

  { WCS capabilities.
  }
  T_WCSCapabilities = class( TGIS_ObjectDisposable )
   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
     xDoc              : IXMLDocument ;

     sProxyUrl         : String ;
     oldGet            : String ;
     sTitle            : String ;
     sAbstract         : String ;
     oImageFormats     : TGIS_StringList ;
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
     oPredefinedCoverages : TGIS_StringList ;
     sPredefinedCRS    : String ;
     sLastUrl          : String ;
     sCRSName          : String ;
     sUserName         : String ;
     sUserPass         : String ;
     sUserAgent        : String ;
     iTimeOut          : Integer ;
     iBytes            : Int64;
     bInverseXY1       : Boolean ;
     odescribedSize    : TPoint ;
   {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
     oCoverages           : TObjectList<TGIS_WCSCoverage> ;
     oAllCoverages        : TGIS_StringList ;
     oAllCRS              : TGIS_StringList ;
     oGetCovMethods       : TGIS_StringList ;
     oDescribeCovMethods  : TGIS_StringList ;
     FOnPassword          : TGIS_TemplateProducerEvent ;


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
     ///   Convert layer name into canonical name.
     /// </summary>
     /// <param name="_name">
     ///   layer name
     /// </param>
     function  canonicalName      ( const _name       : String
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
     ///   Load Coverages.
     /// </summary>
     /// <param name="_xmlCoverage">
     ///   Coverages xml node
     /// </param>
     procedure loadCoverages      ( const _xmlCoverage  : IXMLNode
                                  ) ;

     function  findExtent         ( const _box        : IXMLNode
                                  ) : TGIS_Extent ;

     procedure findTimePosition   ( const _box        : IXMLNode ;
                                    const _list       : TGIS_StringList
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
                                    const _list       : TStrings       ;
                                    const _newVersion : Boolean
                                  ) ;

     /// <summary>
     ///   Process tokens for embedded password tokens.
     /// </summary>
     /// <result>
     ///   Expanded value of a token or a token name itself if not handled
     /// </result>
     function  passwordCallBack   ( const _token : String
                                   ) : String ;

     procedure parseDescribeCoverage( const _cov : TGIS_WCSCoverage
                                    ) ;
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
     ///   Parse describe coverage.
     /// </summary>
     /// <param name="_coverage">
     ///   coverage name
     /// </param>
     procedure GetDescribeCoverage( const _coverage : String
                                  ) ;

     /// <summary>
     ///   Get map from WCS service.
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
     /// <param name="_crs">
     ///   requested CRS
     /// </param>
     /// <param name="_width">
     ///   requested width
     /// </param>
     /// <param name="_height">
     ///   requested height
     /// </param>
     /// <param name="_coverage">
     ///   requested coverage
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
                                    const _crs          : String         ;
                                    const _width        : Integer        ;
                                    const _height       : Integer        ;
                                    const _styles       : String         ;
                                    const _coverage     : String         ;
                                    const _ignoreURI    : Boolean        ;
                                    const _axisReversed : Boolean
                                  ) : TGIS_HttpResponse ;


     /// <summary>
     ///   Test path for WCS flags.
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
     ///   Find coverage by name.
     /// </summary>
     /// <param name="_name">
     ///   coverage name
     /// </param>
     /// <returns>
     ///   coverage layer
     /// </returns>
     function  FindCoverageByName ( const _name       : String
                                  ) : TGIS_WCSCoverage ;

     /// <summary>
     ///   Get maximum lat lon bounding box.
     /// </summary>
     /// <returns>
     ///   extent
     /// </returns>
     function  GetMaxLatLonBndBox  : TGIS_Extent ;

     /// <summary>
     ///   Get maximum bounding box.
     /// </summary>
     /// <param name="_CRS">
     ///   bounding box CRS
     /// </param>
     /// <returns>
     ///   bounding box extent
     /// </returns>
     function  GetMaxBndBox       ( const _CRS        : String
                                  ) : TGIS_Extent ;

     /// <summary>
     ///   Find default coverage.
     /// </summary>
     /// <returns>
     ///   coverage name
     /// </returns>
     function  FindDefaultCoverage   : String ;

     /// <summary>
     ///   Describe a coverage from request to get more info.
     /// </summary>
     /// <param name="_cov">
     ///   coverage
     /// </param>
     /// <param name="_ignoreURI">
     ///   ignore internal URI for requests from Capabilities
     /// </param>
     procedure DescribeCoverage( const _cov       : TGIS_WCSCoverage ;
                                 const _ignoreURI : Boolean
                               ) ;
   public
     property LastUrl           : String      read sLastUrl           ;
     property ImageFormats      : TGIS_StringList read oImageFormats  ;
     property Version           : String      read sVersion           ;
     property Path              : String      read sPath              ;
     property Error             : String      read sError             ;
     property PredefinedVersion : String      read sPredefinedVersion ;
     property PredefinedFormat  : String      read sPredefinedFormat  ;
     property PredefinedStyles  : String      read sPredefinedStyles  ;
     property PredefinedCoverages : TGIS_StringList read oPredefinedCoverages ;
     property PredefinedCRS     : String      read sPredefinedCRS     ;
     property Coverage          : TObjectList<TGIS_WCSCoverage>  read oCoverages   ;
     property AllCoverages      : TGIS_StringList read oAllCoverages     ;
     property AllCRS            : TGIS_StringList read oAllCRS        ;
     property UserAgent         : String      read sUserAgent write sUserAgent ;
     property ProxyUrl          : String      read sProxyUrl  write sProxyUrl  ;
     property TimeOut           : Integer     read iTimeOut   write iTimeOut ;
     property TransmittedBytes  : Int64       read iBytes             ;
     property InverseXY1        : Boolean     read bInverseXY1 write bInverseXY1 ;
     property DescribedSize     : TPoint      read odescribedSize ;
   end ;

//==============================================================================
// T_WCSCapabilities
//==============================================================================

  constructor T_WCSCapabilities.Create(
    const _vwr    : IGIS_Viewer ;
    const _layer  : TObject
  ) ;
  begin
    inherited Create ;

    oViewer := _vwr ;
    oLayer  := _layer ;

    xDoc := TGIS_XMLDocument.Create ;
    xDoc.ParseOptions := [ TParseOption.poPreserveWhiteSpace ] ;

    oAllCoverages         := TGIS_StringList.Create ;
    oPredefinedCoverages  := TGIS_StringList.Create ;
    oAllCRS               := TGIS_StringList.Create ;
    oAllCRS.Duplicates    := TDuplicates.dupIgnore ;
    oAllCRS.Sorted        := True ;
    sCRSName              := 'CRS' ;
    iBytes                := 0 ;
    bInverseXY1           := False ;
    {$IFDEF GIS_NORECORDS}
    odescribedSize        := Point( 0, 0);
    {$ENDIF}

    oCoverages    := TObjectList<TGIS_WCSCoverage>.Create( True ) ;
  end ;

  procedure T_WCSCapabilities.doDestroy;
  begin
    FreeObject( oImageFormats         ) ;
    FreeObject( oCoverages            ) ;
    FreeObject( oAllCoverages         ) ;
    FreeObject( oGetCovMethods        ) ;
    FreeObject( oDescribeCovMethods   ) ;
    FreeObject( oPredefinedCoverages  ) ;
    FreeObject( oAllCRS               ) ;

    FreeObject( xDoc ) ;

    inherited ;
  end ;

  function T_WCSCapabilities.canonicalName(
    const _name : String
  ) : String ;
  var
    j   : Integer ;
    str : String  ;
    c   : {$IFDEF JAVA} SByte
          {$ELSE}       Byte
          {$ENDIF}      ;
    arb : TBytes  ;
  begin
    str := _name ;
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
        '1', '2', '3', '4', '5', '6', '7', '8', '9', '0' :
          Result := Result + Char(c)
        else
          Result := Result + Format( '%%%x', [ c ] ) ;
      end;
    end ;
  end ;

  procedure T_WCSCapabilities.Parse(
    const _xml : TStream
  ) ;
  var
    root : IXMLNode ;
    list : IXMLNodeList ;
    node : IXMLNode ;
    i    : Integer ;
    cs   : TGIS_CSCoordinateSystem ;
    satt : String ;
  begin
    if _xml <> nil then
      xDoc.LoadFromStream( _xml ) ;

    root := xDoc.DocumentElement ;

    if root = nil then exit ;

    node := root.ChildNodes.FindNode( 'Service' ) ;
    if not assigned( node ) then
      node := root.ChildNodes.FindNode( 'ServiceIdentification' ) ;

    if assigned( node ) then begin
      sTitle := getElementValue( node, 'Title' ) ;
      if IsStringEmpty( sTitle ) then
        sTitle := getElementValue( node, 'name' ) ;

      sAbstract := getElementValue( node, 'Abstract' ) ;
      if IsStringEmpty( sAbstract ) then
        sAbstract := getElementValue( node, 'description' ) ;

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

    node := root.ChildNodes[ 'Capability' ]
                  .ChildNodes[ 'Request' ]
                    .ChildNodes[ 'GetCoverage' ] ;
    if assigned( node ) then
      list := node.ChildNodes ;

    if not assigned( oImageFormats ) then
      oImageFormats := TGIS_StringList.Create ;

    list := root.ChildNodes[ 'ServiceMetadata' ].ChildNodes ;
    if assigned( list ) then begin
      for i := 0 to list.Count - 1 do begin
        if list.Nodes[ i ].NodeName = 'formatSupported' then
          oImageFormats.Add( list.Nodes[ i ].Text ) ;
      end ;
    end ;

    list := root.ChildNodes[ 'ContentMetadata' ].ChildNodes ;
    if not assigned( list ) or (list.Count = 0) then
      list := root.ChildNodes[ 'Contents' ].ChildNodes ;

    for i := 0 to list.Count - 1 do
      if (list.Nodes[ i ].LocalName = 'CoverageOfferingBrief') or
         (list.Nodes[ i ].LocalName = 'CoverageSummary') then
        loadCoverages( list.Nodes[ i ] )
      else if list.Nodes[ i ].LocalName = 'SupportedFormat' then
        oImageFormats.Add( list.Nodes[ i ].Text )
      else if list.Nodes[ i ].LocalName = 'SupportedCRS' then begin
        cs := TGIS_CSFactory.ByWKT( list.Nodes[ i ].Text ) ;
        if not ( cs is TGIS_CSUnknownCoordinateSystem ) then begin
          oAllCRS.Add( Format( 'EPSG:%d', [cs.EPSG] ) ) ;
        end ;
      end ;

    if oAllCRS.Count = 0 then begin
      if not IsStringEmpty( sPredefinedCRS ) then
        oAllCRS.Add( sPredefinedCRS ) ;
      oAllCRS.Add( 'EPSG:4326' ) ;
    end;

    if not IsStringEmpty( sPredefinedFormat ) then
      oImageFormats.Add( sPredefinedFormat )
    else
      oImageFormats.Add( 'image/tiff' ) ;

    oGetCovMethods := TGIS_StringList.Create ;
    loadOnlineResource( root.ChildNodes[ 'Capability' ]
                              .ChildNodes[ 'Request' ]
                                .ChildNodes[ 'GetCoverage' ],
                        oGetCovMethods,
                        False
                       ) ;

    oDescribeCovMethods := TGIS_StringList.Create ;
    loadOnlineResource( root.ChildNodes[ 'Capability' ]
                              .ChildNodes[ 'Request' ]
                                .ChildNodes[ 'DescribeCoverage' ],
                        oDescribeCovMethods,
                        False
                      ) ;

    node := root.ChildNodes[ 'OperationsMetadata' ] ;
    if assigned( node ) then begin
      for i := 0 to node.ChildNodes.Count - 1 do begin
        if node.ChildNodes[i].LocalName = 'Operation' then begin
          satt := VarToString( getAttributeValue( node.ChildNodes[i], 'name' ) ) ;
          if satt = 'DescribeCoverage' then
            loadOnlineResource( node.ChildNodes[i], oDescribeCovMethods, True )
          else if satt = 'GetCoverage' then
            loadOnlineResource( node.ChildNodes[i], oGetCovMethods, True )
        end ;
      end ;
    end ;

  end ;

  procedure T_WCSCapabilities.ParsePredefined ;
  var
    i         : Integer ;
    wCoverage : TGIS_WCSCoverage ;
    bbox      : TGIS_WCSCoverageBoundingBox ;
  begin
    sVersion  := sPredefinedVersion ;

    if sVersion >= '1.3.0' then
      sCRSName := 'CRS' ;

    if not assigned( oImageFormats ) then
      oImageFormats := TGIS_StringList.Create ;
    if not IsStringEmpty( sPredefinedFormat ) then
      oImageFormats.Add( sPredefinedFormat )
    else
      oImageFormats.Add( 'image/tiff' ) ;

    oAllCoverages.Assign( oPredefinedCoverages ) ;

    if not assigned( oGetCovMethods ) then
      oGetCovMethods := TGIS_StringList.Create ;
    if not assigned( oDescribeCovMethods ) then
      oDescribeCovMethods := TGIS_StringList.Create ;

    for i := 0 to oAllCoverages.Count-1 do begin
      wCoverage := TGIS_WCSCoverage.Create ;
      wCoverage.Title        := oAllCoverages[i] ;
      wCoverage.Name         := oAllCoverages[i] ;
      wCoverage.LatLonBBox := ePredefinedBBox ;
      bbox := TGIS_WCSCoverageBoundingBox.Create ;
      bbox.BoundingBox := ePredefinedBBox ;
      bbox.CRS := sPredefinedCRS ;
      wCoverage.CRSBBox.Add( bbox ) ;
      wCoverage.CRS.Add( sPredefinedCRS ) ;
      oCoverages.Add( wCoverage ) ;
      oAllCRS.Add( sPredefinedCRS ) ;
    end ;
  end ;

  function T_WCSCapabilities.passwordCallBack( const _token : String
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

  procedure T_WCSCapabilities.GetCapabilities(
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
    if IsStringEmpty( sPredefinedVersion ) then
      sPredefinedVersion := URLGetAndDeleteParameterValue( sPath, 'ACCEPTVERSIONS') ;
    sPredefinedFormat  := URLGetAndDeleteParameterValue( sPath, 'FORMAT'      ) ;
                          URLGetAndDeleteParameterValue( sPath, 'WIDTH'       ) ;
                          URLGetAndDeleteParameterValue( sPath, 'HEIGHT'      ) ;

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

    sPredefinedCRS     := URLGetAndDeleteParameterValue( sPath, 'CRS'         ) ;
                          URLGetAndDeleteParameterValue( sPath, 'TRANSPARENT' ) ;
    sPredefinedStyles  := URLGetAndDeleteParameterValue( sPath, 'STYLES'      ) ;
    stmp               := URLGetAndDeleteParameterValue( sPath, 'COVERAGE'    ) ;
    sUserName          := URLGetAndDeleteParameterValue( sPath, 'USER'        ) ;
    sUserPass          := URLGetAndDeleteParameterValue( sPath, 'PASS'        ) ;

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
          oPredefinedCoverages.Add( str ) ;

    if not IsStringEmpty( sPredefinedVersion ) then
      scapversion := sPredefinedVersion
    else
      scapversion := WCS_VERSION_FORCED ;

    if IsStringEmpty( sservice ) then
      sservice := 'WCS' ;

    if UpperCase( sservice ) <> 'WCS' then
      Abort ;

    if not IsStringEmpty( scapversion ) then
      query := formUrl( Format( '%sVERSION=%s&SERVICE=WCS&REQUEST=GetCapabilities',
                                [ sPredefinedParams, scapversion ]
                               ), sPath
                       )
    else
      query := formUrl( Format('%sSERVICE=WCS&REQUEST=GetCapabilities',
                               [ sPredefinedParams ] ),
                        sPath
                      ) ;

    r.Stream := nil ;
    try
      TestPath( _path, has_getcap, has_getmap ) ;

      if has_getmap then exit ;

      r := fetchHttp( query, sUserName, sUserPass ) ;

      if r.Status = GIS_HTTP_OK then
        xDoc.LoadFromStream( r.Stream ) ;
    finally
      FreeObject( r.Stream ) ;
    end ;
  end ;

  procedure T_WCSCapabilities.GetDescribeCoverage(
    const _coverage : String
  ) ;
  var
    query       : String  ;
    r           : TGIS_HttpResponse ;
    scapversion : String  ;
    root        : IXMLNode ;
    node        : IXMLNode ;
    enode       : IXMLNode ;
    limits      : IXMLNode ;
    glow, ghigh : TArray<String> ;
    i           : Integer;
    bbox        : TGIS_WCSCoverageBoundingBox ;
    cov         : TGIS_WCSCoverage ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      scapversion := sPredefinedVersion
    else
      scapversion := WCS_VERSION_FORCED ;

    if not IsStringEmpty( scapversion ) then
      query := formUrl( Format( '%sVERSION=%s&SERVICE=WCS&REQUEST=DescribeCoverage&COVERAGE=%s',
                                [ sPredefinedParams, scapversion, _coverage ]
                               ), sPath
                       )
    else
      query := formUrl( Format('%sSERVICE=WCS&REQUEST=DescribeCoverage&COVERAGE=%s',
                               [ sPredefinedParams, _coverage ] ),
                        sPath
                      ) ;

    r.Stream := nil ;
    try
      r := fetchHttp( query, sUserName, sUserPass ) ;

      if r.Status = GIS_HTTP_OK then
        xDoc.LoadFromStream( r.Stream ) ;
    finally
      FreeObject( r.Stream ) ;
    end ;

    root := xDoc.DocumentElement ;
    if root = nil then exit ;
    try
      node := root.ChildNodes['CoverageOffering'].ChildNodes['domainSet']
                  .ChildNodes['spatialDomain'].ChildNodes['RectifiedGrid'] ;
      if assigned( node ) then begin
        limits := node.ChildNodes['limits'].ChildNodes['GridEnvelope'] ;
        if assigned( limits ) then begin
          {$IFDEF JAVA OR ISLAND}
          glow  := limits.ChildNodes.FindNode('low').Text.Split(' ').ToArray ;
          ghigh := limits.ChildNodes.FindNode('high').Text.Split(' ').ToArray ;
          {$ELSE}
          glow  := limits.ChildNodes.FindNode('low').Text.Split([' ']) ;
          ghigh := limits.ChildNodes.FindNode('high').Text.Split([' ']) ;
          {$ENDIF}
          odescribedSize.X := StrToInt(ghigh[0])-StrToInt(glow[0])+1;
          odescribedSize.Y := StrToInt(ghigh[1])-StrToInt(glow[1])+1;
        end ;
      end ;
      node := root.ChildNodes['CoverageOffering'].ChildNodes['domainSet']
                  .ChildNodes['spatialDomain'] ;
      cov := FindCoverageByName( _coverage ) ;
      for i := 0 to node.ChildNodes.Count-1 do begin
        enode := node.ChildNodes[i] ;
        if enode.LocalName = 'Envelope' then begin
          bbox := TGIS_WCSCoverageBoundingBox.Create ;
          bbox.BoundingBox := findExtent( enode ) ;
          bbox.CRS         := VarToString( getAttributeValue( enode, 'srsName' ) ) ;
          cov.CRSBBox.Add( bbox ) ;
        end;

      end;
    except
      odescribedSize.X := 0 ;
      odescribedSize.Y := 0 ;
    end ;
  end ;

  {$IFDEF OXYGENE}

    procedure T_WCSCapabilities.doBusy(
      const _sender : Object ;
      const _e      : TGIS_BusyEventArgs
    ) ;
    begin
      if assigned( oViewer ) then
        _e.Abort := oViewer.HourglassShake ;
    end ;
  {$ELSE}

    procedure T_WCSCapabilities.doBusy(
          _sender    : TObject ;
          _pos, _end : Integer ;
      var _abort     : Boolean
    ) ;
    begin
      if assigned( oLayer ) then
        _abort := TGIS_Layer( oLayer ).HourglassShake ;
    end ;
  {$ENDIF}

  function T_WCSCapabilities.fetchHttp(
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

  function T_WCSCapabilities.FindCoverageByName(
    const _name  : String
  ) : TGIS_WCSCoverage ;
  var
    i  : Integer ;
    la : TGIS_WCSCoverage ;
  begin
    Result := nil ;

    for i := 0 to oCoverages.Count - 1 do begin
      la := TGIS_WCSCoverage( oCoverages[ i ] ) ;
      if la.Name = _name then begin
        Result := la ;
        exit ;
      end ;
    end ;
  end ;

  function T_WCSCapabilities.formUrl(
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

  function T_WCSCapabilities.getAttributeValue(
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

  function T_WCSCapabilities.getElementValue(
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

  function T_WCSCapabilities.GetMap(
    const _path         : String      ;
    const _version      : String      ;
    const _extent       : TGIS_Extent ;
    const _format       : String      ;
    const _crs          : String      ;
    const _width        : Integer     ;
    const _height       : Integer     ;
    const _styles       : String      ;
    const _coverage     : String      ;
    const _ignoreURI    : Boolean     ;
    const _axisReversed : Boolean
  ) : TGIS_HttpResponse ;
  var
    query   : String  ;
    tmppath : String  ;
    ext     : TGIS_Extent ;
    cid     : String ;
    sformat : String ;
    bid     : String ;
    surn    : String ;
    sepsg   : String ;
  begin
    if not IsStringEmpty( sPredefinedVersion ) then
      sVersion := sPredefinedVersion
    else
      sVersion := _version ;

    if not _ignoreURI then
      tmppath := oGetCovMethods.Values[ 'Get' ]
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

    if sVersion = '1.0.0' then begin
      cid  := 'COVERAGE' ;
      bid  := 'BBOX' ;
      surn := '' ;
    end
    else if Pos( '1.1', sVersion ) = StringFirst then begin
      cid  := 'IDENTIFIER' ;
      bid  := 'BOUNDINGBOX' ;
      {$IFDEF JAVA OR ISLAND}
        sepsg := _crs.Split(':')[1] ;
      {$ELSE}
        sepsg := _crs.Split([':'])[1] ;
      {$ENDIF}
      surn := ',urn:ogc:def:crs:EPSG::' + sepsg ;
    end
    else
    if sVersion >= '2.0.0' then begin
      cid  := 'COVERAGEID' ;
      bid  := 'BBOX' ;
      surn := '' ;
    end ;

    if IsStringEmpty( _format ) then
      sformat := 'GeoTIFF'
    else
      sformat := _format ;

    query := formUrl( Format( 'VERSION=%s&'           +
                              'SERVICE=WCS&'          +
                              'REQUEST=GetCoverage&'  +
                              '%s=%s,%s,%s,%s%s&'     +
                              'FORMAT=%s&'            +
                              'WIDTH=%d&'             +
                              'HEIGHT=%d&'            +
                              '%s=%s&'                +
                              'STYLES=%s&'            +
                              '%s'                    +
                              '%s=%s',
                             [ _version,
                               bid,
                               DotFloatToStr( ext.XMin ),
                               DotFloatToStr( ext.YMin ),
                               DotFloatToStr( ext.XMax ),
                               DotFloatToStr( ext.YMax ),
                               surn,
                               sformat,
                               _width,
                               _height,
                               sCRSName,
                               _crs,
                               sPredefinedStyles,
                               sPredefinedParams,
                               cid,
                               _coverage
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

  function T_WCSCapabilities.GetMaxBndBox(
    const _CRS    : String
  ) : TGIS_Extent ;
  var
    i, j : Integer ;
    la : TGIS_WCSCoverage ;
  begin
    Result := GisNoWorld ;

    for j := 0 to oCoverages.Count - 1 do begin
      la := oCoverages[ j ]  ;
      if assigned( la.CRSBBox ) then
        for i := 0 to la.CRSBBox.Count - 1 do begin
          if CompareText( _CRS, TGIS_WCSCoverageBoundingBox( la.CRSBBox[ i ] ).CRS ) = 0 then
          begin
             Result := GisMaxExtent( Result,
                                     TGIS_WCSCoverageBoundingBox( la.CRSBBox[ i ] ).BoundingBox
                                   ) ;
             break ;
          end ;
        end ;
    end ;
  end ;

  function T_WCSCapabilities.GetMaxLatLonBndBox : TGIS_Extent ;
  var
    i   : Integer ;
  begin
    Result := GisNoWorld ;

    for i := 0 to oCoverages.Count - 1 do
      Result := GisMaxExtent(
                  Result,
                  oCoverages[ i ].LatLonBBox
                ) ;
  end ;

  function T_WCSCapabilities.findExtent(
    const _box : IXMLNode
  ) : TGIS_Extent ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
  begin
    Result := GisNoWorld ;

    tkn := TGIS_Tokenizer.Create ;
    try
      for i := 0 to _box.ChildNodes.Count - 1 do begin
        if _box.ChildNodes[ i ].NodeName = 'ows:LowerCorner' then begin
          tkn.ExecuteEx( _box.ChildNodes[ i ].Text, ' ' ) ;
          if tkn.Result.Count = 2 then begin
            Result.XMin := DotStrToFloat( tkn.Result[ 0 ] ) ;
            Result.YMin := DotStrToFloat( tkn.Result[ 1 ] ) ;
          end ;
        end
        else if _box.ChildNodes[ i ].NodeName = 'ows:UpperCorner' then begin
          tkn.ExecuteEx( _box.ChildNodes[ i ].Text, ' ' ) ;
          if tkn.Result.Count = 2 then begin
            Result.XMax := DotStrToFloat( tkn.Result[ 0 ] ) ;
            Result.YMax := DotStrToFloat( tkn.Result[ 1 ] ) ;
          end ;
        end
        else if _box.ChildNodes[ i ].NodeName = 'gml:pos' then begin
          tkn.ExecuteEx( _box.ChildNodes[ i ].Text, ' ' ) ;
          if tkn.Result.Count = 2 then begin
            if i = 0 then begin
              Result.XMin := DotStrToFloat( tkn.Result[ 0 ] ) ;
              Result.YMin := DotStrToFloat( tkn.Result[ 1 ] ) ;
            end
            else begin
              Result.XMax := DotStrToFloat( tkn.Result[ 0 ] ) ;
              Result.YMax := DotStrToFloat( tkn.Result[ 1 ] ) ;
            end ;
          end ;
        end;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure T_WCSCapabilities.findTimePosition(
    const _box  : IXMLNode ;
    const _list : TGIS_StringList
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _box.ChildNodes.Count - 1 do begin
      if _box.ChildNodes[ i ].NodeName = 'gml:timePosition' then
        _list.Add( _box.ChildNodes[ i ].Text ) ;
    end ;
  end ;

  procedure T_WCSCapabilities.loadCoverages(
    const _xmlCoverage : IXMLNode
  ) ;
  var
    wCoverage    : TGIS_WCSCoverage       ;
    attr      : IXMLNode         ;
    subnodes  : IXMLNodeList     ;
    i, j      : Integer          ;
    bbox      : TGIS_WCSCoverageBoundingBox ;
    tkn       : TGIS_Tokenizer   ;
    CRSVal    : String           ;
    cs        : TGIS_CSCoordinateSystem ;
  begin
    wCoverage := TGIS_WCSCoverage.Create ;

    oCoverages.Add( wCoverage ) ;

    wCoverage.Title  := getElementValue( _xmlCoverage, 'label' ) ;
    if IsStringEmpty( wCoverage.Title ) then
      wCoverage.Title  := getElementValue( _xmlCoverage, 'Title' ) ;

    wCoverage.Name   := getElementValue( _xmlCoverage, 'name' ) ;
    if IsStringEmpty( wCoverage.Name ) then
      wCoverage.Name   := getElementValue( _xmlCoverage, 'Identifier' ) ;
    if IsStringEmpty( wCoverage.Name ) then
      wCoverage.Name   := getElementValue( _xmlCoverage, 'CoverageId' ) ;

    wCoverage.Info   := getElementValue( _xmlCoverage, 'description' ) ;
    if IsStringEmpty( wCoverage.Info ) then
     wCoverage.Info   := getElementValue( _xmlCoverage, 'Abstract' ) ;

    attr := _xmlCoverage.ChildNodes.FindNode( 'WGS84BoundingBox' ) ;
    if assigned( attr ) then
      wCoverage.LatLonBBox := findExtent( attr )
    else begin
      attr := _xmlCoverage.ChildNodes.FindNode( 'lonLatEnvelope' ) ;
      if assigned( attr ) then begin
        wCoverage.LatLonBBox := findExtent( attr ) ;
        findTimePosition( attr, wCoverage.TimePosition ) ;
      end
      else
        wCoverage.LatLonBBox := GisNoWorld ;

    end ;

    subnodes := _xmlCoverage.ChildNodes ;
    if assigned( subnodes ) then begin
      for i := 0 to subnodes.Count - 1 do begin
        attr := subnodes.Nodes[ i ] ;
        if ( attr.NodeName = 'SupportedCRS' ) then begin
          tkn := TGIS_Tokenizer.Create ;
          try
            tkn.Execute( attr.Text, [ ',',' ',';' ] ) ;
            for j := 0 to tkn.Result.Count - 1 do begin
              CRSVal := tkn.Result[ j ] ;
              cs := TGIS_CSFactory.ByWKT( CRSVal ) ;
              if not ( cs is TGIS_CSUnknownCoordinateSystem ) then begin
                wCoverage.CRS.Add( Format( 'EPSG:%d', [cs.EPSG] ) ) ;
                oAllCRS.Add( Format( 'EPSG:%d', [cs.EPSG] ) ) ;
              end ;
            end;
          finally
            FreeObject( tkn ) ;
          end ;
        end
        else if ( attr.NodeName = 'ows:BoundingBox' ) then begin
          bbox := TGIS_WCSCoverageBoundingBox.Create ;
          bbox.BoundingBox := findExtent( attr ) ;
          bbox.CRS         := VarToString( getAttributeValue( attr, 'crs' ) ) ;
          wCoverage.CRSBBox.Add( bbox ) ;
        end
        else if ( attr.NodeName = 'SupportedFormat' ) then
          wCoverage.Formats.Add( attr.Text ) ;
      end ;
    end ;

    if wCoverage.CRS.Count = 0 then begin
      if not IsStringEmpty( sPredefinedCRS) then
        wCoverage.CRS.Add( sPredefinedCRS ) ;

      wCoverage.CRS.Add( 'EPSG:4326' ) ;
    end ;

    if not IsStringEmpty( wCoverage.Name ) then
      oAllCoverages.Add( wCoverage.Name ) ;
  end ;

  procedure T_WCSCapabilities.loadOnlineResource(
    const _xmlList    : IXMLNode ;
    const _list       : TStrings ;
    const _newVersion : Boolean
  ) ;
  var
    attr : IXMLNode ;
    url  : String ;
  begin
    if not assigned( _xmlList ) then exit ;

    if not _newVersion then
      attr := _xmlList.ChildNodes[ 'DCPType' ]
                         .ChildNodes['HTTP']
                           .ChildNodes['Get']
                             .ChildNodes['OnlineResource' ]
    else
      attr := _xmlList.ChildNodes[ 'DCP' ]
                         .ChildNodes['HTTP']
                           .ChildNodes['Get'] ;

    if assigned( attr ) then begin
      url := VarToString( getAttributeValue( attr, 'xlink:href' ) ) ;
      _list.Values[ 'Get' ] := url ;
    end ;
  end ;

  procedure T_WCSCapabilities.TestPath(
    const _path     : String ;
    var _has_getcap : Boolean ;
    var _has_getmap : Boolean
  ) ;
  begin
    _has_getcap := Pos( UpperCase( 'request=GetCapabilities' ),
                        UpperCase( _path )
                      ) > ( StringFirst - 1 ) ;
    _has_getmap := Pos( UpperCase( 'request=GetCoverage'     ),
                        UpperCase( _path )
                      ) > ( StringFirst - 1 ) ;

  end ;

  function T_WCSCapabilities.FindDefaultCoverage : String ;
  var
    wCoverage : TGIS_WCSCoverage ;
  begin
    if oCoverages.Count > 0 then
      wCoverage := oCoverages[0]
    else
      wCoverage := nil ;

    if assigned( wCoverage ) then
      Result := wCoverage.Name
    else
      Result := '' ;
  end ;

  procedure T_WCSCapabilities.DescribeCoverage(
    const _cov        : TGIS_WCSCoverage ;
    const _ignoreURI  : Boolean
  ) ;
  var
    query   : String  ;
    r       : TGIS_HttpResponse ;
    cname   : String ;
    tmppath : String ;
  begin
    if not assigned( _cov ) then exit ;

    cname := canonicalName( _cov.Name ) ;

    if not _ignoreURI then
      tmppath := oDescribeCovMethods.Values[ 'Get' ]
    else
      tmppath := '' ;

    if IsStringEmpty( tmppath ) then
      tmppath := sPath
    else if ( Pos( '://localhost', tmppath ) >= StringFirst ) and
            ( Pos( '://localhost', sPath   ) <  StringFirst ) then
      tmppath := sPath ;

    query := formUrl( Format( '%sVERSION=1.0.0&SERVICE=WCS&REQUEST=DescribeCoverage'+
                              '&COVERAGE=%s', [ sPredefinedParams, cname ]
                             ), tmppath
                     ) ;
    r.Stream := nil ;
    try
      r := fetchHttp( query, sUserName, sUserPass ) ;

      if r.Status = GIS_HTTP_OK then
        xDoc.LoadFromStream( r.Stream ) ;
    finally
      FreeObject( r.Stream ) ;
    end ;
     parseDescribeCoverage( _cov ) ;
  end ;

  procedure T_WCSCapabilities.parseDescribeCoverage(
    const _cov : TGIS_WCSCoverage
  ) ;
  var
    root : IXMLNode ;
    node : IXMLNode ;
    attr : IXMLNode ;
    i, j : Integer ;
  begin
    root := xDoc.DocumentElement ;

    if root = nil then exit ;

    node := root.ChildNodes.FindNode( 'CoverageOffering' ) ;
    if assigned( node ) then begin
      for i := 0 to node.ChildNodes.Count - 1 do begin
        if node.ChildNodes[i].LocalName = 'supportedCRSs' then begin
          for j := 0 to node.ChildNodes[i].ChildNodes.Count - 1 do begin
            attr := node.ChildNodes[i].ChildNodes[j] ;
            if (attr.LocalName = 'requestCRSs') or
               (attr.LocalName = 'requestResponseCRSs' ) or
               (attr.LocalName = 'nativeCRSs' ) then
              if _cov.CRS.IndexOf( attr.Text ) = -1 then
                _cov.CRS.Add( attr.Text ) ;
          end ;
        end
        else if node.ChildNodes[ i ].LocalName = 'supportedFormats' then begin
          for j := 0 to node.ChildNodes[i].ChildNodes.Count - 1 do begin
            attr := node.ChildNodes[i].ChildNodes[j] ;
            if attr.LocalName = 'formats' then
              if _cov.Formats.IndexOf( attr.Text ) = -1 then
                _cov.Formats.Add( attr.Text ) ;
          end ;
        end
        else if node.ChildNodes[ i ].LocalName = 'lonLatEnvelope' then begin
          if GisIsNoWorld( _cov.LatLonBBox ) then
            _cov.LatLonBBox := findExtent( node.ChildNodes[ i ] ) ;
        end;
      end ;
    end ;

  end ;

//==============================================================================
// TGIS_WCSCoverage
//==============================================================================

  constructor TGIS_WCSCoverage.Create ;
  begin
    inherited ;

    Name         := '' ;
    Title        := '' ;
    LatLonBBox   := GisNoWorld ;
    CRS          := TGIS_StringList.Create ;
    CRSBBox      := TObjectList<TGIS_WCSCoverageBoundingBox>.Create( True ) ;
    Formats      := TGIS_StringList.Create ;
    TimePosition := TGIS_StringList.Create ;
  end ;


  procedure TGIS_WCSCoverage.doDestroy ;
  begin
    FreeObject( CRSBBox ) ;
    FreeObject( CRS     ) ;
    FreeObject( Formats ) ;
    FreeObject( TimePosition ) ;

    inherited ;
  end ;

//==============================================================================
// TGIS_FileWCS
//==============================================================================

  constructor TGIS_FileWCS.Create(
    const _vwr    : IGIS_Viewer ;
    const _layer  : TObject
  ) ;
  begin
    inherited Create ;

    oViewer := _vwr ;
    oLayer  := _layer ;

    FDefaultCoverage := '';
    FSelectedCRS  := '';
    FForcedCRS    := '';

    FAxisOrderReversed := False ;
    FAxisOrderIgnored  := False ;

    WCSCaps := T_WCSCapabilities.Create( oViewer, oLayer ) ;
  end ;

  procedure TGIS_FileWCS.doDestroy ;
  begin
    FreeObject( WCSCaps ) ;

    inherited ;
  end ;

  function TGIS_FileWCS.fget_ProxyUrl
    : String  ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).sProxyUrl ;
  end ;

  procedure TGIS_FileWCS.fset_ProxyUrl(
    const _value : String
  ) ;
  begin
    T_WCSCapabilities( WCSCaps ).sProxyUrl := _value ;
  end ;

  function TGIS_FileWCS.fget_LastUrl
    : String ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).LastUrl
  end ;

  function TGIS_FileWCS.fget_ImageFormats
    : TStrings ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).ImageFormats
  end ;

  function TGIS_FileWCS.fget_AllCoverages
    : TStrings ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).AllCoverages ;
  end ;

  function TGIS_FileWCS.fget_AllCRS
    : TStrings ;
  var
    owcs : T_WCSCapabilities ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;
    if not IsStringEmpty( owcs.PredefinedCRS ) then
      owcs.oAllCRS.Text := owcs.PredefinedCRS ;

    Result := owcs.AllCRS ;
  end ;

  function  TGIS_FileWCS.fget_ForcedCRS
    : String ;
  var
    owcs : T_WCSCapabilities ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    if not IsStringEmpty( FForcedCRS ) then
      Result := FForcedCRS
    else
      Result := owcs.PredefinedCRS ;
  end;

  procedure TGIS_FileWCS.fset_ForcedCRS(
    const _value : String
  ) ;
  begin
    FForcedCRS := _value ;
  end;

  function TGIS_FileWCS.fget_PredefinedVersion
    : String ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).PredefinedVersion ;
  end ;

  function TGIS_FileWCS.fget_PredefinedFormat
    : String ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).PredefinedFormat ;
  end ;

  function TGIS_FileWCS.fget_PredefinedStyles
    : String ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).PredefinedStyles ;
  end ;

  function TGIS_FileWCS.fget_PredefinedCoverages
    : TStrings ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).PredefinedCoverages ;
  end ;

  function TGIS_FileWCS.GetExtent(
    const _crs : String
  ) : TGIS_Extent ;
  var
    ext  : TGIS_Extent       ;
    ext2 : TGIS_Extent       ;
    owcs : T_WCSCapabilities ;
    tkn  : TGIS_Tokenizer    ;
    lcs  : TGIS_CSCoordinateSystem ;
    wgs  : TGIS_CSCoordinateSystem ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    ext := owcs.GetMaxBndBox( _crs ) ;

    if isAxisReversed then begin
      // swap extent XY
      ext2 := ext ;
      ext.XMin := ext2.YMin ;
      ext.YMin := ext2.XMin ;
      ext.XMax := ext2.YMax ;
      ext.YMax := ext2.XMax ;
    end ;

    if GisIsNoWorld( ext ) then begin
      ext := owcs.GetMaxLatLonBndBox ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _crs, [':'] ) ;
        if tkn.Result.Count > 1 then
          try
            if (CompareText( tkn.Result[ 1 ], 'WGS84(DD)' ) = 0) or
               (Pos( 'CRS:84', tkn.Result[ 1 ] ) >= StringFirst) or
               (Pos( 'CRS84' , tkn.Result[ 1 ] ) >= StringFirst)  then
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

    if not GisIsNoWorld( owcs.ePredefinedBBox ) then
      if not GisIsNoWorld( ext ) then
        ext := GisCommonExtent( ext, owcs.ePredefinedBBox )
      else
        ext := owcs.ePredefinedBBox ;

    Result := ext ;
  end ;

  function TGIS_FileWCS.VerifyCRS(
    const _crs : String
  ) : Boolean ;
  var
    owcs : T_WCSCapabilities ;
    ext  : TGIS_Extent       ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    ext := owcs.GetMaxBndBox(_crs ) ;

    Result := not GisIsNoWorld( ext ) ;
  end ;

  function TGIS_FileWCS.fget_TransmittedBytes
    : Int64 ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).TransmittedBytes ;
  end ;

  function TGIS_FileWCS.fget_DescribedSize
    : TPoint ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).DescribedSize ;
  end ;

  function TGIS_FileWCS.fget_ServiceVersion
    : String ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).Version ;
  end ;

  function TGIS_FileWCS.fget_ServiceInfo
    : String ;
  begin
    Result := T_WCSCapabilities( WCSCaps ).sTitle + #13#10 +
              T_WCSCapabilities( WCSCaps ).sAbstract ;
  end ;

  function TGIS_FileWCS.fget_DefaultCRS
    : String ;
  var
    owcs : T_WCSCapabilities ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    if assigned( owcs.AllCRS ) then begin
      if owcs.AllCRS.Count > 0 then
        Result := T_WCSCapabilities( WCSCaps ).AllCRS[ 0 ] ;
    end
    else
      Result := '' ;
  end ;

  function TGIS_FileWCS.fget_Coverage(
    const _indexOrName: Variant
  ) : TGIS_WCSCoverage ;
  var
    i : Integer ;
    owcs : T_WCSCapabilities ;
  begin
    Result := nil ;

    owcs := T_WCSCapabilities( WCSCaps ) ;

    if VarIsOrdinal(_indexOrName) then begin
      i := VarToInt32( _indexOrName ) ;
      if (i >= 0 ) or ( i < owcs.oCoverages.Count ) then
        Result := TGIS_WCSCoverage( owcs.oCoverages[i] ) ;
    end
    else begin
      for i := 0 to owcs.oCoverages.Count-1 do
        if TGIS_WCSCoverage( owcs.oCoverages[i] ).Name = VarToString( _indexOrName ) then begin
          Result := TGIS_WCSCoverage( owcs.oCoverages[i] ) ;
          break ;
        end ;
    end ;
  end ;

  function TGIS_FileWCS.fget_CoveragesCount : Integer ;
  var
    owcs : T_WCSCapabilities ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;
    Result := owcs.oCoverages.Count ;
  end ;

  function TGIS_FileWCS.isAxisReversed : Boolean ;
  var
    owcs : T_WCSCapabilities ;
    brev : Boolean ;
  begin
    brev := False ;
    Result := False ;

    owcs := T_WCSCapabilities( WCSCaps ) ;

    if assigned( TGIS_Layer( owcs.oLayer ).CS ) then begin
      if owcs.sVersion >= '1.1.0' then begin
        brev := TGIS_Layer( owcs.oLayer ).CS.EPSG = 4326 ;
        brev := brev or TGIS_Layer( owcs.oLayer ).CS.ReversedCoordinates ;
      end;
    end;

    if not FAxisOrderIgnored then
      Result := brev ;

    if FAxisOrderReversed then
      Result := not Result ;
  end ;

  procedure TGIS_FileWCS.Load(
    const _path  : String ;
    const _xml   : TStream
  ) ;
  var
    owcs       : T_WCSCapabilities ;
    has_getcap : Boolean           ;
    has_getmap : Boolean           ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    owcs.TestPath( _path, has_getcap, has_getmap ) ;

    owcs.FOnPassword := FOnPassword ;
    owcs.TimeOut     := TimeOut ;
    owcs.UserAgent   := UserAgent ;
    owcs.ProxyUrl    := ProxyUrl ;
//?    owcs.InverseXY   := bInverseXY ;

    if not assigned( _xml ) then
      owcs.GetCapabilities( _path ) ;
    if not has_getmap then
      owcs.Parse( _xml )
    else
      owcs.ParsePredefined ;

    if owcs.PredefinedCoverages.Count > 0 then
      FDefaultCoverage := owcs.PredefinedCoverages[0]
    else
      FDefaultCoverage := owcs.FindDefaultCoverage ;

    owcs.GetDescribeCoverage( FDefaultCoverage ) ;

    FSelectedCRS  := '' ;
    FError := owcs.Error ;
  end ;

  function TGIS_FileWCS.GetMap(
    const _extent   : TGIS_Extent ;
    const _format   : String      ;
    const _width    : Integer     ;
    const _height   : Integer     ;
    const _coverage : String
  ) : TGIS_HttpResponse ;
  var
    owcs     : T_WCSCapabilities ;
    sversion : String            ;
    slay     : String            ;
    sCRS     : String            ;
    sfmt     : String            ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    if not IsStringEmpty( owcs.PredefinedVersion ) then
      sversion := owcs.PredefinedVersion
    else
      sversion := owcs.Version ;

    if not IsStringEmpty( _coverage ) then
      slay := owcs.canonicalName( _coverage )
    else
      slay := '' ;

    if not IsStringEmpty( PredefinedFormat ) then
      sfmt := PredefinedFormat
    else
      sfmt := _format ;

    sCRS := FSelectedCRS ;

    if IsStringEmpty( sCRS ) then
      sCRS := 'EPSG:4326' ;

    try
      if not IsStringEmpty( owcs.PredefinedStyles ) then
        Result := owcs.GetMap( owcs.Path, sversion, _extent, sfmt,
                               sCRS, _width, _height, owcs.PredefinedStyles,
                               slay, FIgnoreInternalURI, isAxisReversed
                             )
      else
        Result := owcs.GetMap( owcs.Path, sversion, _extent, sfmt,
                               sCRS, _width, _height, '',
                               slay, FIgnoreInternalURI, isAxisReversed
                             ) ;
    except
      FreeObject( Result.Stream ) ;
      raise ;
    end;
  end ;

  function TGIS_FileWCS.FindCoverage(
    const _name : String ;
    const _cs   : TGIS_CSCoordinateSystem
  ) : TGIS_WCSCoverage ;
  var
    owcs : T_WCSCapabilities ;
  begin
    Result := nil ;
    owcs := T_WCSCapabilities( WCSCaps ) ;
    if not assigned( owcs.oCoverages ) then exit ;

    Result := owcs.FindCoverageByName( _name ) ;
  end ;

  function TGIS_FileWCS.MakeUrl(
    const _query  : String ;
    const _path   : String
  ) : String ;
  var
    owcs : T_WCSCapabilities ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    Result := owcs.formUrl( _query, _path ) ;
  end ;

  procedure TGIS_FileWCS.DescribeCoverage(
    const _cov : TGIS_WCSCoverage
  ) ;
  var
    owcs : T_WCSCapabilities ;
  begin
    owcs := T_WCSCapabilities( WCSCaps ) ;

    owcs.DescribeCoverage( _cov, FIgnoreInternalURI ) ;
  end ;

{==================================== END =====================================}
end.
