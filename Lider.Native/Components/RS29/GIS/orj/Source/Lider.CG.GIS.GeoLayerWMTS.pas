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
  Encapsulation of a OpenGIS WMTS Layer.
}

{$IFDEF DCC}
  unit GisLayerWMTS ;
  {$HPPEMIT '#pragma link "GisLayerWMTS"'}
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

{$INCLUDE GisInclude.inc}
interface

{$IFDEF CLR}
  uses
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Classes,

    GisTypes,
    GisLayerWebTiles,
    GisCsSystems
 ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerWMTS = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  /// Encapsulation of an OpenGIS WMTS layer.
  /// </summary>
  TGIS_LayerWMTS = {$IFDEF OXYGENE} public {$ENDIF}
                   class( TGIS_LayerWebTiles )
    private // properties internal values
      FWMTS : TObject ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // property access routines

      procedure fset_CS     ( const _value  : TGIS_CSCoordinateSystem
                            ) ; override;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_SERVER_WRONGURL
      /// </exception>
      procedure setUp       ; override;

     /// <summary>
     ///   Process tokens for embedded password tokens.
     /// </summary>
     /// <param name="_token">
     ///   token value
     /// </param>
     /// <returns>
     ///   Expanded value of a token or a token name itself if not handled
     /// </returns>
     function  passwordCallBackX( const _token : String
                                ) : String ;

      /// <summary>
      ///   Covert coordinate system units to meters.
      /// </summary>
      /// <returns>
      ///   value in meters
      /// </returns>
      function  unitsToMeters : Double ;

      /// <summary>
      ///   Check if coordinates must be reversed.
      /// </summary>
      /// <returns>
      ///   True if must invert axis
      /// </returns>
      function  isAxisReversed : Boolean ;

      /// <summary>
      ///   Parse config file.
      /// </summary>
      procedure parseConfig ;

      /// <summary>
      ///   Prepare parameters of wmts.
      /// </summary>
      /// <returns>
      ///   url text
      /// </returns>
      function prepareUrlParameters : String ;

      /// <summary>
      ///   Configure tiles and layer based on config and metadata.
      /// </summary>
      procedure configureTiles ;

    private // other private values
      sImageFormat       : String ;
      cInfo              : String ;
      cCopyright         : String ;
      sPath              : String ;
      sOptionalParams    : String ;
      sLayer             : String ;
      sInfoFormat        : String ;
      sTileMatrixSet     : String ;
      sDimensions        : String ;
      FUserAgent         : String ;
      FReferer           : String ;
      FProxyUrl          : String ;
      iTimeOut           : Integer ;
      bAxisOrderIgnored  : Boolean ;
      bAxisOrderReversed : Boolean ;
      bForceAxisOrder    : Boolean ;
      sUserName          : String ;
      sUserPass          : String ;
      iMaxThreads        : Integer ;
    protected

      procedure doDestroy  ; override;

      /// <inheritdoc/>
      procedure customLod         ; override;

    public // various public routines

      /// <inheritdoc/>
      constructor Create   ; override;

      /// <summary>
      ///   Get feature info.
      /// </summary>
      /// <param name="_point">
      ///   coordinate
      /// </param>
      /// <returns>
      ///   info text
      /// </returns>
      function    GetFeatureInfo   ( const _point : TGIS_Point
                                   ) : String ;

      /// <inheritdoc/>
      function    PreRecognize     ( const _path     : String ;
                                       var _new_path : String
                                   ) : Boolean ; override;

      /// <inheritdoc/>
      function  GetAvailableLayers : TGIS_LayerInfoList ; override;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Variants,
    System.Generics.Collections,
    System.Generics.Defaults,
    GisRtl,
    GisClasses,
    GisCsBase,
    GisCsFactory,
    GisFunctions,
    GisInternals,
    GisResource,
    GisTypesUI,
    GisRegistredLayers,
    GisXmlDoc ;
{$ENDIF}

const
  WMTS_FILE_INFO = 'OpenGIS Web Map Tile Service (WMTS)' ;
  WMTS_IMAGE_RES_SCALE = 1.2 ;

type

  TWMTS_MetadataReader = class
    protected
     function  getVal    ( const _node      : IXMLNode       ;
                           const _key       : String
                         ) : String ;
     function  getAttr  ( const _node       : IXMLNode       ;
                          const _name       : String       ;
                          const _key        : String
                        ) : OleVariant ; overload;
     function  getAttr  ( const _node       : IXMLNode       ;
                          const _key        : String
                        ) : OleVariant ; overload;
  end ;

  TWMTS_ServiceIdentification = class( TWMTS_MetadataReader )
    private
      FTitle              : String ;
      FAbstract           : String ;
      FServiceType        : String ;
      FServiceTypeVersion : String ;
    public
      procedure Read( const _node : IXMLNode ) ;
    public
      property Title              : String read FTitle ;
      property &Abstract          : String read FAbstract ;
      property ServiceType        : String read FServiceType ;
      property ServiceTypeVersion : String read FServiceTypeVersion ;
  end ;

  TWMTS_ServiceProvider = class( TWMTS_MetadataReader )
    private
      FProviderName   : String ;
      FProviderSite   : String ;
      FServiceContact : String ;
    public
      procedure Read( const _node : IXMLNode ) ;
  end ;

  TWMTS_Operation = class
    private
      FXlinkHref : String ;
      FEncoding  : String ;
    public
      property XlinkHref : String read FXlinkHref write FXlinkHref ;
      property Encoding  : String read FEncoding  write  FEncoding;
  end ;

  TWMTS_Operations = TList<TWMTS_Operation> ;

  TWMTS_OperationsMetadata = class( TWMTS_MetadataReader )
    private
      FOperations : TDictionary<String,TWMTS_Operations> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property Operations : TDictionary<String,TWMTS_Operations> read FOperations ;
  end ;

  TWMTS_Dimension = class( TWMTS_MetadataReader )
    private
      FIdentifier         : String ;
      FTitle              : String ;
      FAbstract           : String ;
      FUOM                : String ;
      FUnitSymbol         : String ;
      FDefaultValue       : String ;
      FCurrent            : Boolean ;
      FValues             : TGIS_StringList ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property Identifier    : String          read FIdentifier ;
      property Title         : String          read FTitle ;
      property &Abstract     : String          read FAbstract ;
      property UOM           : String          read FUOM ;
      property UnitSymbol    : String          read FUnitSymbol ;
      property DefaultValue  : String          read FDefaultValue;
      property Current       : Boolean         read FCurrent ;
      property Values        : TGIS_StringList read FValues ;
  end ;

  TWMTS_LegendURL = class( TWMTS_MetadataReader )
    private
      FFormat             : String ;
      FHref               : String ;
      FMinScale           : Double ;
      FMaxScale           : Double ;
      FWidth              : Integer ;
      FHeight             : Integer ;
    public
      procedure Read( const _node : IXMLNode ) ;
    public
      property Href    : String          read FHref ;
  end ;

  TWMTS_Style = class( TWMTS_MetadataReader )
    private
      FIdentifier         : String ;
      FTitle              : String ;
      FAbstract           : String ;
      FIsDefault          : Boolean ;
      FLegendURLs         : TList<TWMTS_LegendURL> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property Identifier  : String                 read FIdentifier ;
      property Title       : String                 read FTitle ;
      property &Abstract   : String                 read FAbstract ;
      property IsDefault   : Boolean                read FIsDefault ;
      property LegendURLs  : TList<TWMTS_LegendURL> read FLegendURLs ;
  end ;

  TWMTS_TileMatrixLimits = class( TWMTS_MetadataReader )
    private
      FTileMatrix         : String ;
      FMinTileRow         : Integer ;
      FMaxTileRow         : Integer ;
      FMinTileCol         : Integer ;
      FMaxTileCol         : Integer ;
    public
      procedure Read( const _node : IXMLNode ) ;
    public
      property TileMatrix : String  read FTileMatrix ;
      property MinTileRow : Integer read FMinTileRow ;
      property MaxTileRow : Integer read FMaxTileRow ;
      property MinTileCol : Integer read FMinTileCol ;
      property MaxTileCol : Integer read FMaxTileCol ;
  end ;

  TWMTS_TileMatrixSetLink = class( TWMTS_MetadataReader )
    private
      FTileMatrixSet      : String ;
      FLimits             : TList<TWMTS_TileMatrixLimits> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property TileMatrixSet : String read FTileMatrixSet ;
      property Limits      : TList<TWMTS_TileMatrixLimits> read FLimits ;
  end ;

  TWMTS_Layer = class( TWMTS_MetadataReader )
    private
      FIdentifier         : String ;
      FTitle              : String ;
      FAbstract           : String ;
      FWGS84BoundingBox   : TGIS_Extent ;
      FBoundingBox        : TGIS_Extent ;
      FFormats            : TGIS_StringList ;
      FInfoFormats        : TGIS_StringList ;
      FResourceURL        : TGIS_StringList ;
      FStyle            : TList<TWMTS_Style> ;
      FTileMatrixSetLink: TList<TWMTS_TileMatrixSetLink> ;
      FDimensions       : TList<TWMTS_Dimension> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
      function  GetInfo : String ;
    public
      property Identifier        : String                         read FIdentifier ;
      property Title             : String                         read FTitle ;
      property &Abstract         : String                         read FAbstract ;
      property WGS84BoundingBox  : TGIS_Extent                    read FWGS84BoundingBox ;
      property BoundingBox       : TGIS_Extent                    read FBoundingBox ;
      property Formats           : TGIS_StringList                read FFormats ;
      property InfoFormats       : TGIS_StringList                read FInfoFormats ;
      property ResourceURL       : TGIS_StringList                read FResourceURL ;
      property Style             : TList<TWMTS_Style>             read FStyle ;
      property TileMatrixSetLink : TList<TWMTS_TileMatrixSetLink> read FTileMatrixSetLink ;
      property Dimensions        : TList<TWMTS_Dimension>         read FDimensions ;
  end ;

  TWMTS_TileMatrix = class( TWMTS_MetadataReader )
    private
      FIdentifier       : String ;
      FTitle            : String ;
      FAbstract         : String ;
      FScaleDenominator : Double ;
      FTopLeftCorner    : TGIS_Point ;
      FTileWidth        : Integer ;
      FTileHeight       : Integer ;
      FMatrixWidth      : Cardinal ;
      FMatrixHeight     : Cardinal ;
    public
      procedure Read( const _node : IXMLNode ) ;
    public
      property Identifier       : String      read FIdentifier ;
      property Title            : String      read FTitle ;
      property &Abstract        : String      read FAbstract ;
      property ScaleDenominator : Double      read FScaleDenominator ;
      property TopLeftCorner    : TGIS_Point  read FTopLeftCorner ;
      property TileWidth        : Integer     read FTileWidth ;
      property TileHeight       : Integer     read FTileHeight ;
      property MatrixWidth      : Cardinal    read FMatrixWidth ;
      property MatrixHeight     : Cardinal    read FMatrixHeight ;
  end ;

  TWMTS_TileMatrixSet = class( TWMTS_MetadataReader )
    private
      FIdentifier         : String ;
      FTitle              : String ;
      FAbstract           : String ;
      FSupportedCRS       : String ;
      FTileMatrixs        : TList<TWMTS_TileMatrix> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property Identifier    : String                   read FIdentifier ;
      property Title         : String                   read FTitle ;
      property &Abstract     : String                   read FAbstract ;
      property SupportedCRS  : String                   read FSupportedCRS ;
      property TileMatrixs : TList<TWMTS_TileMatrix>  read FTileMatrixs ;
  end ;

  TWMTS_Contents = class( TWMTS_MetadataReader )
    private
      FLayers        : TList<TWMTS_Layer> ;
      FTileMatrixSet : TList<TWMTS_TileMatrixSet> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property Layers        : TList<TWMTS_Layer>
                               read FLayers ;
      property TileMatrixSet : TList<TWMTS_TileMatrixSet>
                               read FTileMatrixSet ;
  end ;

  TWMTS_Capabilities = class( TWMTS_MetadataReader )
    private
      FServiceIdentification : TWMTS_ServiceIdentification ;
      FServiceProvider       : TWMTS_ServiceProvider ;
      FOperationsMetadata    : TWMTS_OperationsMetadata ;
      FContents              : TWMTS_Contents ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _node : IXMLNode ) ;
    public
      property ServiceIdentification : TWMTS_ServiceIdentification read FServiceIdentification ;
      property ServiceProvider       : TWMTS_ServiceProvider       read FServiceProvider ;
      property OperationsMetadata    : TWMTS_OperationsMetadata    read FOperationsMetadata ;
      property Contents              : TWMTS_Contents              read FContents ;
  end ;

  TWMTS = class
    private
      xDoc             : IXMLDocument ;
      FCapabilities    : TWMTS_Capabilities ;
      sUserAgent       : String ;
      sReferer         : String ;
      sUserName        : String ;
      sUserPass        : String ;
      sProxyUrl        : String ;
      iTimeOut         : Integer ;
      sError           : String  ;
      FActiveLayer     : TWMTS_Layer ;
      FActiveMatrixSet : TWMTS_TileMatrixSet ;
    private
      procedure parseCapabilities ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ; override;
      {$ENDIF}
      procedure Read( const _path  : String ;
                      const _xml   : TStream = nil
                     ) ;
    public
      property UserAgent       : String               read  sUserAgent
                                                      write sUserAgent ;
      property Referer         : String               read  sReferer
                                                      write sReferer ;
      property UserName       : String                read  sUserName
                                                      write sUserName ;
      property UserPass       : String                read  sUserPass
                                                      write sUserPass ;
      property ProxyUrl        : String               read  sProxyUrl
                                                      write sProxyUrl ;
      property TimeOut         : Integer              read  iTimeOut
                                                      write iTimeOut ;
      property Error           : String               read  sError               ;
      property Capabilities    : TWMTS_Capabilities   read  FCapabilities ;
      property ActiveLayer     : TWMTS_Layer          read  FActiveLayer
                                                      write FActiveLayer ;
      property ActiveMatrixSet : TWMTS_TileMatrixSet  read  FActiveMatrixSet
                                                      write FActiveMatrixSet;
  end ;

//==============================================================================
// WMTS helpers
//==============================================================================

{ TWMTS_MetadataReader }

  function TWMTS_MetadataReader.getAttr(
    const _node : IXMLNode     ;
    const _name : String       ;
    const _key  : String
  ) : OleVariant ;
  var
    attr : IXMLNode ;
    node : IXMLNode ;
  begin
    Result := Unassigned ;
    node := _node.ChildNodes.FindNode( _name ) ;
    if not assigned( node ) then exit ;

    attr := node.AttributeNodes.FindNode( _key ) ;
    if assigned( attr ) then
      Result := attr.Text ;
  end ;

  function TWMTS_MetadataReader.getAttr(
    const _node : IXMLNode       ;
    const _key  : String
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

  function TWMTS_MetadataReader.getVal(
    const _node : IXMLNode ;
    const _key  : String
  ) : String ;
  var
    attr : IXMLNode ;
  begin
    if not assigned( _node ) then exit ;

    attr := _node.ChildNodes.FindNode( _key ) ;

    if assigned( attr ) and attr.IsTextElement then
      Result := attr.Text
    else
      Result := '' ;
  end ;

  { TWMTS_ServiceIdentification }

  procedure TWMTS_ServiceIdentification.Read(
    const _node : IXMLNode
  ) ;
  begin
    if not assigned( _node ) then exit ;

    FTitle              := getVal( _node, 'ows:Title'              ) ;
    FAbstract           := getVal( _node, 'ows:Abstract'           ) ;
    FServiceType        := getVal( _node, 'ows:ServiceType'        ) ;
    FServiceTypeVersion := getVal( _node, 'ows:ServiceTypeVersion' ) ;
  end ;

  { TWMTS_ServiceProvider }

  procedure TWMTS_ServiceProvider.Read(
    const _node : IXMLNode
  ) ;
  begin
    if not assigned( _node ) then exit ;

    FProviderName   := getVal( _node, 'ows:ProviderName'                ) ;
    FProviderSite   := VarToString( getAttr( _node, 'ows:ProviderSite', 'xlink:href' ) ) ;
    FServiceContact := getVal( _node.ChildNodes.FindNode('ows:ServiceContact'),
                               'ows:IndividualName'
                              ) ;
  end ;

  { TWMTS_OperationsMetadata }

  constructor TWMTS_OperationsMetadata.Create ;
  begin
    inherited ;

    FOperations := TDictionary<String,TWMTS_Operations>.Create(
                   {$IFDEF OXYGENE}
                     {$IFDEF JAVA}
                       java.lang.String.CASE_INSENSITIVE_ORDER
                     {$ELSE}
                       StringComparer.OrdinalIgnoreCase
                     {$ENDIF}
                   {$ELSE}
                     TIStringComparer.Ordinal
                   {$ENDIF}
                 ) ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_OperationsMetadata.Destroy ;
    var
      itm : TPair<String,TWMTS_Operations> ;
      obj : TObject ;

    begin
      {$IFNDEF NEXTGEN}
      for itm in FOperations do begin
        for obj in itm.Value do
          FreeObjectNotNil( obj ) ;
        FreeObjectNotNil( itm.Value ) ;
      end ;
      {$ENDIF}

      FreeObject( FOperations ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_OperationsMetadata.Read(
    const _node : IXMLNode
  ) ;
  var
    i,j   : Integer ;
    node  : IXMLNode ;
    nd    : IXMLNode ;
    gn    : IXMLNode ;
    oname : String ;
    ops   : TWMTS_Operations ;
    op    : TWMTS_Operation ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      node := _node.ChildNodes[i] ;
      if node.NodeName = 'ows:Operation' then begin
        oname := VarToString( getAttr( node, 'name' ) ) ;
        gn := node.ChildNodes['ows:DCP'].ChildNodes['ows:HTTP'] ;

        ops := TWMTS_Operations.Create ;
        for j := 0 to gn.ChildNodes.Count-1 do begin
          nd := gn.ChildNodes[j] ;
          if nd.NodeType <> TNodeType.ntElement then continue ;
          op := TWMTS_Operation.Create ;
          try
            op.XlinkHref := VarToString( getAttr( nd, 'xlink:href' ) ) ;
            op.Encoding  := nd.ChildNodes['ows:Constraint']
                              .ChildNodes['ows:AllowedValues']
                              .ChildNodes['ows:Value'].Text ;
            ops.Add( op ) ;
          except
            FreeObject( op ) ;
          end ;
        end ;

        FOperations.Add( oname, ops ) ;
      end ;
    end ;
  end ;

  { TWMTS_Dimension }

  constructor TWMTS_Dimension.Create ;
  begin
    inherited ;

    FValues := TGIS_StringList.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_Dimension.Destroy ;
    begin
      FreeObject( FValues ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_Dimension.Read(
    const _node : IXMLNode
  ) ;
  var
    i   : Integer ;
    nd  : IXMLNode ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'ows:Identifier' then
        FIdentifier := nd.Text
      else if nd.NodeName = 'ows:Title' then
        FTitle := nd.Text
      else if nd.NodeName = 'ows:Abstract' then
        FAbstract := nd.Text
      else if nd.NodeName = 'Default' then
        FDefaultValue := nd.Text
      else if nd.NodeName = 'UOM' then
        FUOM := nd.Text
      else if nd.NodeName = 'unitSymbol' then
        FUnitSymbol := nd.Text
      else if nd.NodeName = 'current' then
        FCurrent := nd.Text = 'true'
      else if nd.NodeName = 'Value' then
        FValues.Add( nd.Text )
      else if nd.NodeName = 'Value' then
        FValues.Add( nd.Text ) ;
    end ;
  end ;

  { TWMTS_TileMatrixLimits }

  procedure TWMTS_TileMatrixLimits.Read(
    const _node : IXMLNode
  ) ;
  var
    i   : Integer ;
    nd  : IXMLNode ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'TileMatrix' then
        FTileMatrix := nd.Text
      else if nd.NodeName = 'MinTileRow' then
        FMinTileRow := StrToInt( nd.Text )
      else if nd.NodeName = 'MaxTileRow' then
        FMaxTileRow := StrToInt( nd.Text )
      else if nd.NodeName = 'MinTileCol' then
        FMinTileCol := StrToInt( nd.Text )
      else if nd.NodeName = 'MaxTileCol' then
        FMaxTileCol := StrToInt( nd.Text )
    end ;
  end ;

  { TWMTS_LegendURL }

  procedure TWMTS_LegendURL.Read(
    const _node : IXMLNode
  ) ;
  begin
    FFormat   := VarToString( getAttr( _node, 'format'     )  ) ;
    FHref     := VarToString( getAttr( _node, 'xlink:href' )  ) ;
    FMinScale := VarToDouble( getAttr( _node, 'minScale'   )  ) ;
    FMaxScale := VarToDouble( getAttr( _node, 'maxScale'   )  ) ;
    FWidth    := VarToInt32(  getAttr( _node, 'width'      )  ) ;
    FHeight   := VarToInt32(  getAttr( _node, 'height'     )  ) ;
  end ;

  { TWMTS_TileMatrixSetLink }

  constructor TWMTS_TileMatrixSetLink.Create ;
  begin
    inherited ;

    FLimits := TList<TWMTS_TileMatrixLimits>.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_TileMatrixSetLink.Destroy ;
    var
      itm : TObject ;
    begin
      {$IFNDEF NEXTGEN}
      for itm in FLimits do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}

      FreeObject( FLimits ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_TileMatrixSetLink.Read(
    const _node : IXMLNode
  ) ;
  var
    i,j : Integer ;
    nd  : IXMLNode ;
    ndn : IXMLNode ;
    tml : TWMTS_TileMatrixLimits ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'TileMatrixSet' then
        FTileMatrixSet := nd.Text
      else if nd.NodeName = 'TileMatrixSetLimits' then begin
        for j := 0 to nd.ChildNodes.Count-1 do begin
          ndn := nd.ChildNodes[j] ;
          if ndn.NodeName = 'TileMatrixLimits' then begin
            tml := TWMTS_TileMatrixLimits.Create ;
            try
              tml.Read( ndn );
              FLimits.Add( tml ) ;
            except
              FreeObject( tml ) ;
            end ;
          end ;
        end ;
      end ;
    end ;
  end ;

  { TWMTS_Layer }

  constructor TWMTS_Layer.Create ;
  begin
    inherited ;

    FFormats             := TGIS_StringList.Create ;
    FInfoFormats         := TGIS_StringList.Create ;
    FResourceURL         := TGIS_StringList.Create ;
    FTileMatrixSetLink   := TList<TWMTS_TileMatrixSetLink>.Create ;
    FStyle               := TList<TWMTS_Style>.Create ;
    FDimensions          := TList<TWMTS_Dimension>.Create ;

    FWGS84BoundingBox := GisNoWorld ;
    FBoundingBox      := GisNoWorld ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_Layer.Destroy ;
    var
      itm : TObject ;
    begin
      FreeObject( FFormats     ) ;
      FreeObject( FInfoFormats ) ;

      {$IFNDEF NEXTGEN}
      for itm in FTileMatrixSetLink do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}
      FreeObject( FTileMatrixSetLink ) ;

      {$IFNDEF NEXTGEN}
      for itm in FStyle do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}
      FreeObject( FStyle ) ;

      {$IFNDEF NEXTGEN}
      for itm in FDimensions do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}
      FreeObject( FDimensions ) ;

      FreeObject( FResourceURL ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_Layer.Read(
    const _node: IXMLNode
  ) ;
  var
    i     : Integer ;
    nd    : IXMLNode ;
    tkn   : TGIS_Tokenizer ;
    stl   : TWMTS_Style ;
    dim   : TWMTS_Dimension ;
    tmsl  : TWMTS_TileMatrixSetLink ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'ows:Identifier' then
        FIdentifier := nd.Text
      else if nd.NodeName = 'ows:Title' then
        FTitle := nd.Text
      else if nd.NodeName = 'ows:Abstract' then
        FAbstract := nd.Text
      else if nd.NodeName = 'ows:WGS84BoundingBox' then begin
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( getVal( nd, 'ows:LowerCorner' ), [' '] ) ;
          {$IFDEF GIS_NORECORDS}
            FWGS84BoundingBox := new TGIS_Extent ;
          {$ENDIF}
          if tkn.Result.Count = 2 then begin
            FWGS84BoundingBox.XMin := DotStrToFloat( tkn.Result[0] ) ;
            FWGS84BoundingBox.YMin := DotStrToFloat( tkn.Result[1] ) ;
          end ;
          tkn.Execute( getVal( nd, 'ows:UpperCorner' ), [' '] ) ;
          if tkn.Result.Count = 2 then begin
            FWGS84BoundingBox.XMax := DotStrToFloat( tkn.Result[0] ) ;
            FWGS84BoundingBox.YMax := DotStrToFloat( tkn.Result[1] ) ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end
      else if nd.NodeName = 'ows:BoundingBox' then begin
        if not GisIsNoWorld( FBoundingBox ) then continue ;

        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( getVal( nd, 'ows:LowerCorner' ), [' '] ) ;
          {$IFDEF GIS_NORECORDS}
            FBoundingBox := new TGIS_Extent ;
          {$ENDIF}
          if tkn.Result.Count = 2 then begin
            FBoundingBox.XMin := DotStrToFloat( tkn.Result[0] ) ;
            FBoundingBox.YMin := DotStrToFloat( tkn.Result[1] ) ;
          end ;
          tkn.Execute( getVal( nd, 'ows:UpperCorner' ), [' '] ) ;
          if tkn.Result.Count = 2 then begin
            FBoundingBox.XMax := DotStrToFloat( tkn.Result[0] ) ;
            FBoundingBox.YMax := DotStrToFloat( tkn.Result[1] ) ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end
      else if nd.NodeName = 'Style' then begin
        stl := TWMTS_Style.Create ;
        try
          stl.Read( nd ) ;
        except
          FreeObject( stl ) ;
        end ;
        FStyle.Add( stl ) ;
      end
      else if nd.NodeName = 'Dimension' then begin
        dim := TWMTS_Dimension.Create ;
        try
          dim.Read( nd ) ;
        except
          FreeObject( dim ) ;
        end ;
        FDimensions.Add( dim ) ;
      end
      else if nd.NodeName = 'Format' then begin
        FFormats.Add( nd.Text ) ;
      end
      else if nd.NodeName = 'InfoFormat' then begin
        FInfoFormats.Add( nd.Text ) ;
      end
      else if nd.NodeName = 'TileMatrixSetLink' then begin
        tmsl := TWMTS_TileMatrixSetLink.Create ;
        try
          tmsl.Read( nd ) ;
        except
          FreeObject( tmsl ) ;
        end ;
        FTileMatrixSetLink.Add( tmsl ) ;
      end
      else if nd.NodeName = 'ResourceURL' then begin
        FResourceURL.Add( VarToString( getAttr( nd, 'format' ) ) + '|' +
                          VarToString( getAttr( nd, 'template' ) ) ) ;
      end
    end ;

  end ;

  function TWMTS_Layer.GetInfo : String ;
  var
    i : Integer ;
    {$IFDEF DCC}
      lnk : TWMTS_TileMatrixSetLink ;
    {$ENDIF}
  begin
    Result := 'Id : ' + Identifier + #13#10 +
              'Title : ' + Title + #13#10 +
              'Abstract : ' + &Abstract + #13#10 +
              'Formats : ' ;
    for i := 0 to Formats.Count-1 do
      Result := Result + Formats[i] + ' ' ;
    Result := Result + #13#10 + 'TileMatrixSet : ' ;
    for lnk in TileMatrixSetLink do
      Result := Result + lnk.TileMatrixSet + ' ' ;
  end ;

  { TWMTS_TileMatrix }

  procedure TWMTS_TileMatrix.Read(
    const _node : IXMLNode
  ) ;
  var
    i   : Integer ;
    nd  : IXMLNode ;
    tkn : TGIS_Tokenizer ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'ows:Identifier' then
        FIdentifier := nd.Text
      else if nd.NodeName = 'ows:Title' then
        FTitle := nd.Text
      else if nd.NodeName = 'ows:Abstract' then
        FAbstract := nd.Text
      else if nd.NodeName = 'ScaleDenominator' then
        FScaleDenominator := DotStrToFloat( nd.Text )
      else if nd.NodeName = 'TopLeftCorner' then begin
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( nd.Text, [' '] ) ;
          if tkn.Result.Count = 2 then begin
            {$IFDEF GIS_NORECORDS}
              FTopLeftCorner := new TGIS_Point ;
            {$ENDIF}
            FTopLeftCorner.X := DotStrToFloat( tkn.Result[0] ) ;
            FTopLeftCorner.Y := DotStrToFloat( tkn.Result[1] ) ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      end
      else if nd.NodeName = 'TileWidth' then begin
        FTileWidth := StrToInt( nd.Text )
      end
      else if nd.NodeName = 'TileHeight' then begin
        FTileHeight := StrToInt( nd.Text )
      end
      else if nd.NodeName = 'MatrixWidth' then begin
        FMatrixWidth := StrToInt64( nd.Text )
      end
      else if nd.NodeName = 'MatrixHeight' then begin
        FMatrixHeight := StrToInt64( nd.Text )
      end
    end ;
  end ;

  { TWMTS_TileMatrixSet }

  constructor TWMTS_TileMatrixSet.Create ;
  begin
    inherited ;

    FTileMatrixs := TList<TWMTS_TileMatrix>.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_TileMatrixSet.Destroy ;
    var
      itm : TWMTS_TileMatrix ;
    begin
      {$IFNDEF NEXTGEN}
      for itm in FTileMatrixs do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}

      FreeObject( FTileMatrixs ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_TileMatrixSet.Read(
    const _node : IXMLNode
  ) ;
  var
    i   : Integer ;
    nd  : IXMLNode ;
    mat : TWMTS_TileMatrix ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'ows:Identifier' then
        FIdentifier := nd.Text
      else if nd.NodeName = 'ows:Title' then
        FTitle := nd.Text
      else if nd.NodeName = 'ows:Abstract' then
        FAbstract := nd.Text
      else if nd.NodeName = 'ows:SupportedCRS' then
        FSupportedCRS := nd.Text
      else if nd.NodeName = 'TileMatrix' then begin
        mat := TWMTS_TileMatrix.Create ;
        try
          mat.Read( nd ) ;
          FTileMatrixs.Add( mat ) ;
        except
          FreeObject( mat ) ;
        end ;
      end ;
    end ;
  end ;

  { TWMTS_Style }

  constructor TWMTS_Style.Create ;
  begin
    inherited ;

    FLegendURLs := TList<TWMTS_LegendURL>.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_Style.Destroy ;
    var
      itm : TWMTS_LegendURL ;
    begin
      {$IFNDEF NEXTGEN}
      for itm in FLegendURLs do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}

      FreeObject( FLegendURLs ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_Style.Read(
    const _node : IXMLNode
  ) ;
  var
    i     : Integer ;
    nd    : IXMLNode ;
    lurl  : TWMTS_LegendURL ;
  begin
    if not assigned( _node ) then exit ;

    FIsDefault := getAttr( _node, 'isDefault' ) = 'true' ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      nd := _node.ChildNodes[i] ;
      if nd.NodeName = 'ows:Identifier' then
        FIdentifier := nd.Text
      else if nd.NodeName = 'ows:Title' then
        FTitle := nd.Text
      else if nd.NodeName = 'ows:Abstract' then
        FAbstract := nd.Text
      else if nd.NodeName = 'LegendURL' then begin
        lurl := TWMTS_LegendURL.Create ;
        try
          lurl.Read( nd ) ;
        except
          FreeObject( lurl ) ;
        end ;
        FLegendURLs.Add( lurl ) ;
      end ;
    end ;

  end ;

  { TWMTS_Contents }

  constructor TWMTS_Contents.Create ;
  begin
    inherited ;

    FLayers        := TList<TWMTS_Layer>.Create ;
    FTileMatrixSet := TList<TWMTS_TileMatrixSet>.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_Contents.Destroy ;
    var
      itm : TObject ;
    begin
      {$IFNDEF NEXTGEN}
      for itm in FLayers do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}

      FreeObject( FLayers ) ;

      {$IFNDEF NEXTGEN}
      for itm in FTileMatrixSet do
        FreeObjectNotNil( itm ) ;
      {$ENDIF}

      FreeObject( FTileMatrixSet ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_Contents.Read(
    const _node : IXMLNode
  ) ;
  var
    i     : Integer ;
    node  : IXMLNode ;
    layer : TWMTS_Layer ;
    mset  : TWMTS_TileMatrixSet ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      node := _node.ChildNodes[i] ;
      if node.NodeName = 'Layer' then begin
        layer := TWMTS_Layer.Create ;
        try
          layer.Read( node ) ;
          FLayers.Add( layer ) ;
        except
          FreeObject( layer ) ;
        end ;
      end
      else if node.NodeName = 'TileMatrixSet' then begin
        mset := TWMTS_TileMatrixSet.Create ;
        try
          mset.Read( node ) ;
          FTileMatrixSet.Add( mset ) ;
        except
          FreeObject( mset ) ;
        end ;
      end ;
    end ;
  end ;

  { TWMTS_Capabilities }

  constructor TWMTS_Capabilities.Create ;
  begin
    inherited ;

    FServiceIdentification := TWMTS_ServiceIdentification.Create ;
    FServiceProvider       := TWMTS_ServiceProvider.Create ;
    FOperationsMetadata    := TWMTS_OperationsMetadata.Create ;
    FContents              := TWMTS_Contents.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS_Capabilities.Destroy ;
    begin
      FreeObject( FServiceIdentification ) ;
      FreeObject( FServiceProvider       ) ;
      FreeObject( FOperationsMetadata    ) ;
      FreeObject( FContents              ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS_Capabilities.Read(
    const _node : IXMLNode
   ) ;
  var
    i    : Integer ;
    node : IXMLNode ;
  begin
    if not assigned( _node ) then exit ;

    for i := 0 to _node.ChildNodes.Count-1 do begin
      node := _node.ChildNodes[i] ;
      if node.NodeName = 'ows:ServiceIdentification' then
        FServiceIdentification.Read( node )
      else if node.NodeName = 'ows:ServiceProvider' then
        FServiceProvider.Read( node )
      else if node.NodeName = 'ows:OperationsMetadata' then
        FOperationsMetadata.Read( node )
      else if node.NodeName = 'Contents' then
        FContents.Read( node )
    end ;

  end ;

//==============================================================================
// TWMTS
//==============================================================================

  constructor TWMTS.Create ;
  begin
    inherited ;

    xDoc := TGIS_XMLDocument.Create ;
    xDoc.ParseOptions := [ TParseOption.poPreserveWhiteSpace ] ;

    FCapabilities := TWMTS_Capabilities.Create ;
  end ;

  {$IFDEF DCC}
    destructor TWMTS.Destroy ;
    begin
      FreeObject( xDoc ) ;
      FreeObject( FCapabilities ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TWMTS.parseCapabilities ;
  var
    root : IXMLNode ;
  begin
    root := xDoc.DocumentElement ;

    if root = nil then exit ;

    if root.NodeName = 'Capabilities' then begin
      FreeObject( FCapabilities ) ;
      FCapabilities := TWMTS_Capabilities.Create ;
      FCapabilities.Read( root ) ;
    end
    else if (root.NodeName = 'ExceptionReport'       ) or
            (root.NodeName = 'ServiceExceptionReport')
    then
      sError := Format( _rsrc( GIS_RS_ERR_SERVER_ERROR ),
                        [root.ChildNodes['Exception'].ChildNodes['ExceptionText'].Text]
                       ) ;
  end ;

  procedure TWMTS.Read(
    const _path : String ;
    const _xml  : TStream
  ) ;
  var
    r : TGIS_HttpResponse  ;
  begin
    sError := '' ;
    if not assigned( _xml ) then begin
      r.Stream := nil ;
      try
        r := TGIS_WebUtils.HttpFetch(
                sProxyUrl + _path,
                nil,
                nil,
                True,
                40000,
                sUserAgent,
                sReferer,
                sUserName,
                sUserPass
              ) ;

        if r.Status = GIS_HTTP_OK then
          xDoc.LoadFromStream( r.Stream )
        else begin
          sError := Format( '%s (%d)', [r.Headers, r.Status] ) ;
          exit ;
        end ;
      finally
        FreeObject( r.Stream ) ;
      end ;
    end
    else
      xDoc.LoadFromStream( _xml ) ;

    parseCapabilities ;
  end ;

//==============================================================================
// TGIS_LayerWMTS
//==============================================================================

  constructor TGIS_LayerWMTS.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

    FUserAgent := GetDefaultUserAgent( 'ttkWP' ) ;
    FProxyUrl  := '' ;
    FReferer   := '' ;
    iTimeOut   := 40000 ;
  end ;

  procedure TGIS_LayerWMTS.doDestroy ;
  begin
    FreeObject( FWMTS ) ;

    inherited ;
  end ;

  procedure TGIS_LayerWMTS.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    // disable change right now
  end ;

  function TGIS_LayerWMTS.unitsToMeters : Double ;
  begin
    if FCS is TGIS_CSProjectedCoordinateSystem then
      Result := CSUnitsList.ByEPSG( TGIS_CSProjectedCoordinateSystem(FCS).Units.EPSG
                                   ).ToBase(1)
    else if FCS is TGIS_CSGeographicCoordinateSystem then
      Result := CSUnitsList.ByEPSG( TGIS_CSGeographicCoordinateSystem(FCS).Units.EPSG
                                   ).ToUnits( CSUnitsList.ByEPSG( 9122 ),
                                              111319.49079327358
                                             )
    else
      Result := 1 ;
  end ;

  function TGIS_LayerWMTS.isAxisReversed : Boolean ;
  begin
    Result := False ;
    if not bAxisOrderIgnored then
      Result := bForceAxisOrder ;

    if bAxisOrderReversed then
      Result := not Result ;
  end ;

  function TGIS_LayerWMTS.passwordCallBackX(
    const _token : String
  ) : String ;
  begin
    Result := passwordCallBackW( _token ) ;
  end;

  procedure TGIS_LayerWMTS.setUp ;
  var
    tmp     : String ;
    url     : String ;
    stmp    : String ;
  begin
    if not assigned( FWMTS ) then
      FWMTS := TWMTS.Create ;

    try
      parseConfig ;

      TWMTS(FWMTS).UserAgent  := FUserAgent ;
      TWMTS(FWMTS).Referer    := FReferer ;
      TWMTS(FWMTS).UserName   := sUserName ;
      TWMTS(FWMTS).UserPass   := sUserPass ;
      TWMTS(FWMTS).ProxyUrl   := FProxyUrl  ;
      TWMTS(FWMTS).TimeOut    := iTimeOut ;

      url := prepareUrlParameters ;

      TWMTS(FWMTS).Read( url, nil ) ;
    except
      on ex : Exception do begin
        if FileExists( Path ) then
          tmp := Path
        else if not IsStringEmpty( Name ) then
          tmp := Name
        else if not IsStringEmpty( Caption ) then
          tmp := Name
        else
          tmp := GetClassName( Self ) ;

        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_LAYERBADFORMAT ),
          tmp,
          0,
          ex
        ) ;
      end ;
    end ;

    if not IsStringEmpty( TWMTS(FWMTS).Error ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), TWMTS(FWMTS).Error, 0 ) ;

    configureTiles ;

    try
      inherited setUp ;
    except
      on e : Exception do
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ), e.Message, 0 ) ;
    end ;

    FFileInfo := WMTS_FILE_INFO + #13#10 +
                 TWMTS(FWMTS).Capabilities.ServiceIdentification.ServiceType + ' ' +
                 TWMTS(FWMTS).Capabilities.ServiceIdentification.ServiceTypeVersion + #13#10 + #13#10 ;

    stmp := TWMTS(FWMTS).Capabilities.ServiceIdentification.Title ;
    if not IsStringEmpty( stmp ) then
      FFileInfo := FFileInfo + stmp + #13#10 ;

    stmp := TWMTS(FWMTS).Capabilities.ServiceIdentification.Abstract ;
    if not IsStringEmpty( stmp ) then
      FFileInfo := FFileInfo + stmp + #13#10 ;

    if assigned( TWMTS(FWMTS).ActiveLayer ) then
      FFileInfo := FFileInfo + TWMTS(FWMTS).ActiveLayer.GetInfo ;
  end ;

  function TGIS_LayerWMTS.prepareUrlParameters : String ;
  var
    url     : String ;
    urlup   : String ;
    lstprms : TStringList ;
    iprm    : Integer ;
    sprm    : String ;
  begin
    url := URLGetPath( sPath ) ;

    lstprms := URLGetParameters( sPath ) ;
    try
      for iprm :=0 to lstprms.Count -1 do begin
        sprm := lstprms[iprm] ;

        if Pos( '<!', sprm ) = StringFirst  then begin
          sprm := Copy( sprm, StringFirst + 2, length(sprm) - 4 ) ;

          if IsStringEmpty( sOptionalParams ) then
            sOptionalParams := sprm
          else
            sOptionalParams := sOptionalParams + '&' + sprm ;
        end
        else if (Pos( 'key=', sprm ) >= StringFirst) or
                (Pos( 'token', sprm ) = StringFirst) then begin
          if IsStringEmpty( sOptionalParams ) then
            sOptionalParams := sprm
          else
            sOptionalParams := sOptionalParams + '&' + sprm ;
        end
        else if (Pos( 'username=', sprm ) >= StringFirst) or
                (Pos( 'password=', sprm ) = StringFirst) then begin
          if IsStringEmpty( sOptionalParams ) then
            sOptionalParams := sprm
          else
            sOptionalParams := sOptionalParams + '&' + sprm ;
        end
        else
          url := URLAddParameter( url, sprm ) ;
      end;

      urlup := UpperCase( url ) ;
      if (Pos( 'REQUEST=GETCAPABILITIES', urlup ) >= StringFirst) or
         (Pos( 'WMTSCAPABILITIES.XML'   , urlup ) >= StringFirst) then
         // assume that url returns xml with Capabilities
      else begin
        url := URLAddParameter( url, 'Request=GetCapabilities' ) ;
        url := URLAddParameter( url, 'Service=WMTS' ) ;
      end ;

      url := URLAddParameter( url, sOptionalParams ) ;
    finally
      FreeObject( lstprms ) ;
    end;

    {$IFDEF OXYGENE}
      url := TemplateProducer( url, nil, @passwordCallBackX, False ) ;
    {$ELSE}
      url := TemplateProducer( url, nil, passwordCallBackX, False ) ;
    {$ENDIF}

    Result := url ;
  end ;

  procedure TGIS_LayerWMTS.parseConfig ;
  var
    k    : Integer ;
    lst  : TGIS_StringList ;
    stmp : String ;
  const
    S_URL           = 'Url'           ;
    S_LAYER         = 'Layer'         ;
    S_CAPTION       = 'Caption'       ;
    S_INFO          = 'Info'          ;
    S_COPYRIGHT     = 'Copyright'     ;
    S_IMAGEFORMAT   = 'ImageFormat'   ;
    S_INFOFORMAT    = 'InfoFormat'    ;
    S_TILEMATRIXSET = 'TileMatrixSet' ;
    S_DIMENSIONS    = 'Dimensions'    ;
    S_USERAGENT     = 'UserAgent'     ;
    S_REFERER       = 'Referer'       ;
    S_PROXYURL      = 'ProxyUrl'      ;
    S_TIMEOUT       = 'TimeOut'       ;
    S_INVERTEDAXIS  = 'InvertAxis'    ;
    S_USER          = 'User'          ;
    S_PASS          = 'Pass'          ;
    S_TEXTPLAIN     = 'text/plain'    ;
    S_IMAGEPNG      = 'image/png'     ;
    S_TRUE          = 'True'          ;
    S_FALSE         = 'False'         ;
    S_MAXTHREADS    = 'MaxThreads'    ;
  begin
    lst := TGIS_StringList.Create ;
    try
      ReadParamsFromPath( Path, lst ) ;
      if lst.Count > 0 then begin
        sPath           := LstReadString( lst,
                                          S_URL,
                                          ''
                                        ) ;
        sLayer          := LstReadString( lst,
                                          S_LAYER,
                                          ''
                                        ) ;
        Caption         := LstReadString( lst,
                                          S_CAPTION,
                                          Caption
                                         ) ;
        cInfo           := LstReadString( lst,
                                          S_INFO,
                                          ''
                                        ) ;
        cCopyright      := LstReadString( lst,
                                          S_COPYRIGHT,
                                          ''
                                        ) ;
        sImageFormat    := LstReadString( lst,
                                          S_IMAGEFORMAT,
                                          S_IMAGEPNG
                                        ) ;
        sInfoFormat     := LstReadString( lst,
                                          S_INFOFORMAT,
                                          S_TEXTPLAIN
                                        ) ;
        sTileMatrixSet  := LstReadString ( lst,
                                          S_TILEMATRIXSET,
                                          ''
                                        ) ;
        sDimensions     := LstReadString ( lst,
                                          S_DIMENSIONS,
                                          ''
                                        ) ;
        FUserAgent      := LstReadString ( lst,
                                          S_USERAGENT,
                                          FUserAgent
                                        ) ;
        FReferer        := LstReadString ( lst,
                                          S_REFERER,
                                          FReferer
                                        ) ;
        FProxyUrl       := LstReadString ( lst,
                                          S_PROXYURL,
                                          FProxyUrl
                                        ) ;
        if not IsStringEmpty( FProxyUrl ) then begin
          if FProxyUrl[ StringLast( FProxyUrl ) ] <> '?' then
            FProxyUrl := FProxyUrl + '?' ;
        end ;

        iTimeOut           := LstReadInteger( lst,
                                              S_TIMEOUT,
                                              40000
                                            ) ;

        // obsolate parameter
        bAxisOrderReversed := LstReadBoolean( lst,
                                              S_INVERTEDAXIS,
                                              bAxisOrderReversed
                                            ) ;

        // new parameter
        bAxisOrderReversed := LstReadBoolean( lst,
                                              GIS_INI_AXIS_ORDER_REVERSED,
                                              bAxisOrderReversed
                                            ) ;

        bAxisOrderIgnored  := LstReadBoolean( lst,
                                              GIS_INI_AXIS_ORDER_IGNORED,
                                              bAxisOrderIgnored
                                            ) ;

        sUserName          := LstReadString ( lst,
                                              S_USER,
                                              ''
                                             ) ;
        sUserPass          := LstReadString ( lst,
                                              S_PASS,
                                              ''
                                             ) ;
        {$IFDEF OXYGENE}
          sUserName := TemplateProducer( sUserName, nil, @passwordCallBackX, False ) ;
          sUserPass := TemplateProducer( sUserPass, nil, @passwordCallBackX, False ) ;
        {$ELSE}
          sUserName := TemplateProducer( sUserName, nil, passwordCallBackX, False ) ;
          sUserPass := TemplateProducer( sUserPass, nil, passwordCallBackX, False ) ;
        {$ENDIF}

        iMaxThreads        := LstReadInteger( lst,
                                              S_MAXTHREADS,
                                              8
                                            ) ;
      end
      else begin
        sPath := Path ;

        k := Pos( '?http://', sPath ) ;
        if k <= StringFirst then
          k := Pos( '?https://', sPath ) ;

        if k > StringFirst then begin
          FProxyUrl := Copy( sPath, StringFirst, k ) ;
          sPath     := Copy( sPath, k+1, 4096 ) ;
        end ;

        {$IFDEF OXYGENE}
          sPath := TemplateProducer( sPath, nil, @passwordCallBackX, False ) ;
        {$ELSE}
          sPath := TemplateProducer( sPath, nil, passwordCallBackX, False ) ;
        {$ENDIF}

        sTileMatrixSet     := '' ;
        sDimensions        := '' ;
        bAxisOrderIgnored  := False ;
        bAxisOrderReversed := False ;
        sImageFormat       := S_IMAGEPNG ;

        stmp := URLGetAndDeleteParameterValue( sPath, S_LAYER ) ;
        if not IsStringEmpty( stmp ) then
          sLayer := stmp
        else
          sLayer := Name ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_TIMEOUT
                              ) ;
        if not IsStringEmpty( stmp ) then
          iTimeOut := StrToInt( stmp ) ;

        // obsolate parameter
        bAxisOrderReversed := StrToBoolean(
                                URLGetAndDeleteParameterValue(
                                  sPath,
                                  S_INVERTEDAXIS
                                ),
                                bAxisOrderReversed
                              ) ;

        // new parameter
        bAxisOrderReversed := StrToBoolean(
                                URLGetAndDeleteParameterValue(
                                  sPath,
                                  GIS_INI_AXIS_ORDER_REVERSED
                                ),
                                bAxisOrderReversed
                              ) ;

        bAxisOrderIgnored  := StrToBoolean(
                                URLGetAndDeleteParameterValue(
                                  sPath,
                                  GIS_INI_AXIS_ORDER_IGNORED
                                ),
                                False
                              ) ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_TILEMATRIXSET
                              ) ;
        if not IsStringEmpty( stmp ) then
          sTileMatrixSet := URLDecode( stmp ) ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_DIMENSIONS
                              ) ;
        if not IsStringEmpty( stmp ) then
          sDimensions := URLDecode( stmp ) ;


        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_IMAGEFORMAT
                              ) ;
        if not IsStringEmpty( stmp ) then
          sImageFormat := stmp ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_INFOFORMAT
                              ) ;
        if not IsStringEmpty( stmp ) then
          sInfoFormat := stmp ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_USER
                              ) ;
        if not IsStringEmpty( stmp ) then
          sUserName := URLDecode( stmp ) ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_PASS
                              ) ;
        if not IsStringEmpty( stmp ) then
          sUserPass := URLDecode( stmp ) ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_CAPTION
                              ) ;
        if not IsStringEmpty( stmp ) then
          Caption := stmp ;

        stmp               := URLGetAndDeleteParameterValue(
                                sPath,
                                S_REFERER
                              ) ;
        if not IsStringEmpty( stmp ) then
          FReferer := URLDecode( stmp ) ;

      end ;

      sPath := URLFixed( sPath ) ;


      bAxisOrderReversed := GisMetadataAsBoolean(
                              'TGIS_LayerWMTS.AxisOrderReversed',
                              bAxisOrderReversed
                            ) ;
      bAxisOrderReversed := StrToBoolean(
                              ReadConfigParam( GIS_INI_AXIS_ORDER_REVERSED ),
                              bAxisOrderReversed
                            ) ;

      bAxisOrderIgnored  := GisMetadataAsBoolean(
                              'TGIS_LayerWMTS.AxisOrderIgnored',
                              bAxisOrderIgnored
                            ) ;
      bAxisOrderIgnored  := StrToBoolean(
                              ReadConfigParam( GIS_INI_AXIS_ORDER_IGNORED ),
                              bAxisOrderIgnored
                            ) ;


      if IsStringEmpty( sPath )  then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                     'Url=', 0
                                    ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_LayerWMTS.configureTiles ;
  var
    i         : Integer        ;
    j         : Integer        ;
    tkn       : TGIS_Tokenizer ;
    vstr      : String         ;
    vtmp      : String         ;
    astr      : TArray<String> ;
    {$IFDEF DCC}
      ms      : TWMTS_TileMatrixSet ;
      msl     : TWMTS_TileMatrixSetLink ;
      itm     : TWMTS_Layer    ;
      dim     : TWMTS_Dimension ;
      op      : TWMTS_Operation ;
      si      : String ;
    {$ENDIF}
    tm        : TWMTS_TileMatrix ;
    tpath     : String         ;
    sstyle    : String ;
    mpu       : Double ;
    res       : Double ;
    tl        : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    tcmin     : Integer ;
    tcmax     : Cardinal ;
    trmin     : Integer ;
    trmax     : Cardinal ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF};
    ext2      : TGIS_Extent ;
    bbox      : TGIS_Extent ;
    ops       : TWMTS_Operations ;
    sflags    : TReplaceFlags ;
    has_rest  : Boolean ;
    stl       : TWMTS_Style ;
    vwr_epsg  : Integer ;

    function getLegendImage( const _url : String ) : TGIS_Bitmap ;
    var
      hres   : TGIS_HttpResponse ;
      res    : TGIS_Bitmap ;
    begin
      res := nil ;

      try
        hres := TGIS_WebUtils.HttpFetch( _url, nil, nil, True, 0, FUserAgent,
                                         FReferer, sUserName, sUserPass ) ;
      except
        FreeObject( hres.Stream ) ;
      end ;

      if hres.Status = GIS_HTTP_OK then begin
        try
          res := TGIS_Bitmap.Create ;
          try
            res.LoadFromStream( hres.Stream ) ;
          except
            FreeObject( res ) ;
          end ;
        finally
          FreeObject( hres.Stream ) ;
        end ;
      end ;
      Result := res ;
    end ;

  begin
    // Find active layer
    if not IsStringEmpty( sLayer ) then begin
      for itm in TWMTS(FWMTS).Capabilities.Contents.Layers do
        if CompareText( itm.Identifier, sLayer ) = 0 then
          TWMTS(FWMTS).ActiveLayer := itm ;
    end ;

    if not assigned( TWMTS(FWMTS).ActiveLayer ) then
      if TWMTS(FWMTS).Capabilities.Contents.Layers.Count > 0 then begin
        TWMTS(FWMTS).ActiveLayer := TWMTS(FWMTS).Capabilities.Contents.Layers[0] ;
        sLayer := TWMTS(FWMTS).ActiveLayer.Identifier ;
      end
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERNOEXIST ),
                                     sLayer, 0
                                    ) ;
    // find active matrix
    if not IsStringEmpty( sTileMatrixSet ) then begin
      for msl in TWMTS(FWMTS).ActiveLayer.TileMatrixSetLink do
        if CompareText( msl.TileMatrixSet, sTileMatrixSet ) = 0 then
          for ms in TWMTS(FWMTS).Capabilities.Contents.TileMatrixSet do
            if CompareText( ms.Identifier, sTileMatrixSet ) = 0 then
              TWMTS(FWMTS).ActiveMatrixSet := ms ;
    end
    else begin
      // try to match the viewer CS
      if assigned( Viewer ) then
        vwr_epsg := Viewer.Ref.CS.EPSG
      else
        vwr_epsg := 0 ;

      if vwr_epsg > 0 then begin
        for msl in TWMTS(FWMTS).ActiveLayer.TileMatrixSetLink do
          for ms in TWMTS(FWMTS).Capabilities.Contents.TileMatrixSet do
            if ( CompareText( msl.TileMatrixSet, ms.Identifier ) = 0 ) and
               ( Pos( IntToStr(vwr_epsg), ms.SupportedCRS ) >= StringFirst ) then begin
              TWMTS(FWMTS).ActiveMatrixSet := ms ;
              sTileMatrixSet := ms.Identifier ;
              break ;
            end ;
      end ;
    end ;

    if not assigned( TWMTS(FWMTS).ActiveMatrixSet ) then
      if TWMTS(FWMTS).ActiveLayer.TileMatrixSetLink.Count > 0 then begin
        sTileMatrixSet := TWMTS(FWMTS).ActiveLayer.TileMatrixSetLink[0].TileMatrixSet ;
        for ms in TWMTS(FWMTS).Capabilities.Contents.TileMatrixSet do
          if CompareText( ms.Identifier, sTileMatrixSet ) = 0 then
            TWMTS(FWMTS).ActiveMatrixSet := ms ;
      end
      else
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                     sTileMatrixSet, 0
                                    ) ;

    if TWMTS(FWMTS).ActiveLayer.Formats.IndexOf( sImageFormat ) = -1 then
      if TWMTS(FWMTS).ActiveLayer.Formats.Count > 0 then
        sImageFormat := TWMTS(FWMTS).ActiveLayer.Formats[0] ;

    if TWMTS(FWMTS).ActiveLayer.InfoFormats.IndexOf( sInfoFormat ) = -1 then
      if TWMTS(FWMTS).ActiveLayer.InfoFormats.Count > 0 then
        sInfoFormat := TWMTS(FWMTS).ActiveLayer.InfoFormats[0] ;

    // setup layer
    FCS := TGIS_CSFactory.ByWKT( TWMTS(FWMTS).ActiveMatrixSet.SupportedCRS ) ;
    if assigned( FCS ) then begin
      bForceAxisOrder := ( FCS.EPSG=4326 ) and
                         ( TGIS_CSFactory.Authority( TWMTS(FWMTS).ActiveMatrixSet.SupportedCRS )
                         <> 'OGC:CRS84' ) ;

      bForceAxisOrder := bForceAxisOrder or FCS.ReversedCoordinates ;
    end ;

    tm := TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs[
            TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs.Count-1
          ] ;

    tcmin := 0  ;
    tcmax := tm.MatrixWidth-1  ;
    trmin := 0  ;
    trmax := tm.MatrixHeight-1  ;

    mpu := unitsToMeters ;

    if isAxisReversed then begin
      tl.X := tm.TopLeftCorner.Y ;
      tl.Y := tm.TopLeftCorner.X ;
    end
    else begin
      tl := tm.TopLeftCorner ;
    end;

    // extent of a matrix
    res := tm.ScaleDenominator * 0.00028 / mpu ;
    ext.XMin := tl.X + res * tm.TileWidth  * tcmin ;
    ext.YMax := tl.Y - res * tm.TileHeight * trmin ;
    ext.XMax := tl.X + res * tm.TileWidth  * tcmax ;
    ext.YMin := tl.Y - res * tm.TileHeight * trmax ;

    // extent of a layer
    if not GisIsNoWorld( TWMTS(FWMTS).ActiveLayer.WGS84BoundingBox ) then
      ext2 := FCS.ExtentFromCS( TGIS_CSFactory.ByEPSG(GIS_EPSG_WGS84),
                                TWMTS(FWMTS).ActiveLayer.WGS84BoundingBox
                               )
    else
      ext2 := GisNoWorld ;

    bbox := TWMTS(FWMTS).ActiveLayer.BoundingBox ;

    if not GisIsNoWorld( bbox ) then begin
      if isAxisReversed then
        Extent := GisExtent( bbox.XMax, bbox.YMax, bbox.XMin, bbox.YMin )
      else
        Extent := bbox ;
    end
    else if not GisIsNoWorld( ext2 ) then
      Extent := ext2
    else
      Extent := ext ;

    if TWMTS(FWMTS).ActiveLayer.Style.Count > 0 then begin
      stl := TWMTS(FWMTS).ActiveLayer.Style[0] ;
      sstyle := stl.Identifier ;
      if stl.LegendURLs.Count > 0 then begin
        Params.Pixel.LegendImage := getLegendImage( stl.LegendURLs[0].Href )
      end
    end
    else
      sstyle := 'default' ;

    has_rest := False ;
    if TWMTS(FWMTS).Capabilities.OperationsMetadata.Operations.TryGetValue(
        'GetTile', ops
       ) then begin
        for op in ops do
          if (op.Encoding = 'RESTful') or (op.Encoding = 'REST') then
            has_rest := True ;
    end ;

    // if ows:OperationsMetadata is empty, assume rest
    if not has_rest then
      if (TWMTS(FWMTS).Capabilities.OperationsMetadata.Operations.Count = 0) and
         ( Pos( 'WMTSCapabilities.xml', sPath ) > StringFirst ) then
        has_rest := True ;

    // parse resourceurl template and prepare url
    if has_rest and (TWMTS(FWMTS).ActiveLayer.ResourceURL.Count > 0) then begin
      vstr := '' ;
      tkn := TGIS_Tokenizer.Create ;
      try
        j := 1 ;
        for i := 0 to TWMTS(FWMTS).ActiveLayer.ResourceURL.Count-1 do begin
          tkn.Execute( TWMTS(FWMTS).ActiveLayer.ResourceURL[i], ['|'] ) ;
          if tkn.Result.Count <> 2 then continue ;
          if tkn.Result[0] = sImageFormat then begin
            vtmp := tkn.Result[1] ;

            {$IFDEF DCC}
              sflags := [ rfIgnoreCase,
                          rfReplaceAll
                        ] ;
            {$ELSE}
              sflags := [ TReplaceFlag.rfIgnoreCase,
                          TReplaceFlag.rfReplaceAll
                        ] ;
            {$ENDIF}

            vtmp := StringReplace( vtmp, '{Style}', sstyle,
                                   sflags
                                 ) ;
            vtmp := StringReplace( vtmp, '{TileMatrixSet}', sTileMatrixSet,
                                   sflags
                                 ) ;
            vtmp := StringReplace( vtmp, '{TileMatrix}', '{level}',
                                   sflags
                                 ) ;
            vtmp := StringReplace( vtmp, '{TileRow}', '{row}',
                                   sflags
                                 ) ;
            vtmp := StringReplace( vtmp, '{TileCol}', '{col}',
                                   sflags
                                 ) ;
            if not IsStringEmpty( sDimensions ) then begin
              {$IFDEF JAVA OR ISLAND}
                for si in sDimensions.Split( ',' ) do begin
                  astr := si.Split( '=' ).ToArray ;
              {$ELSE}
                for si in sDimensions.Split( [','] ) do begin
                  astr := si.Split( ['='] ) ;
              {$ENDIF}
                  if length( astr ) = 2 then
                    vtmp := StringReplace( vtmp, '{'+astr[0]+'}',
                                           astr[1],
                                           sflags
                                         ) ;
                end;
            end
            else
              for dim in TWMTS(FWMTS).ActiveLayer.Dimensions do
                vtmp := StringReplace( vtmp, '{'+dim.Identifier+'}',
                                       dim.DefaultValue,
                                       sflags
                                     ) ;

            vtmp := URLAddParameter( vtmp, sOptionalParams ) ;
            vstr := vstr + Format( 'Url%d=', [j] ) + vtmp + '\n' ;
            inc( j ) ;
          end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
      tpath := vstr ;
    end
    else begin
      if TWMTS(FWMTS).Capabilities.OperationsMetadata.Operations.TryGetValue(
          'GetTile', ops
         ) then begin
          for op in ops do
            if (op.Encoding = 'KVP') or IsStringEmpty( op.Encoding ) then
              sPath := op.XlinkHref ;
      end ;
      vtmp := URLGetPath( sPath ) +
              Format( 'Service=WMTS&Request=GetTile&Version=1.0.0&Layer=%s&'+
                      'Format=%s&style=%s&TileMatrixSet=%s&TileMatrix={level}&'+
                      'TileRow={row}&TileCol={col}',
                      [sLayer, sImageFormat, sstyle, sTileMatrixSet]
                    ) ;
      vtmp := URLAddParameter( vtmp, sOptionalParams ) ;

      tpath := Format( 'Url%d=', [1] ) + vtmp + '\n' ;
    end ;

    FPathEx := '[TatukGIS Layer]\n' +
              'Storage=WEBTILES\n' +
              'Name=%s\n' +
              'Caption=%s\n' +
              '%s\n' +
              'EPSG='+ IntToStr( FCS.EPSG ) + '\n' +
              'StartLevel=%d\n' +
              'EndLevel=%d\n' +
              'XMin='         + DotFloatToStr( ext.XMin ) + '\n' +
              'YMin='         + DotFloatToStr( ext.YMin ) + '\n' +
              'XMax='         + DotFloatToStr( ext.XMax ) + '\n' +
              'YMax='         + DotFloatToStr( ext.YMax ) + '\n' +
              'Extent.XMin='  + DotFloatToStr( FExtent.XMin ) + '\n' +
              'Extent.YMin='  + DotFloatToStr( FExtent.YMin ) + '\n' +
              'Extent.XMax='  + DotFloatToStr( FExtent.XMax ) + '\n' +
              'Extent.YMax='  + DotFloatToStr( FExtent.YMax ) + '\n' +
              'Info=%s\n' +
              'Referer=%s\n' +
              'User=%s\n' +
              'Pass=%s\n' +
              'MaxThreads=%d\n' +
              'Copyright=%s\n.ttkwp' ;
      FPathEx := Format( FPathEx,
        [ sLayer, Caption, tpath, 0,
          TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs.Count, cInfo, FReferer,
          sUserName, sUserPass, iMaxThreads, cCopyright ]
      ) ;
  end ;


  {$IFDEF DCC}
    function SortMatrix( const Item1, Item2 : TWMTS_TileMatrix ) : Integer ;
  {$ELSE}
    function SortMatrix( const Item1, Item2 : TObject ) : Integer ;
  {$ENDIF}
  begin
    if StrToInt( TWMTS_TileMatrix( Item1 ).Identifier ) <
       StrToInt( TWMTS_TileMatrix( Item2 ).Identifier ) then
      Result := -1
    else if StrToInt( TWMTS_TileMatrix( Item1 ).Identifier ) >
            StrToInt( TWMTS_TileMatrix( Item2 ).Identifier ) then
      Result := 1
    else
      Result := 0
  end ;

  procedure TGIS_LayerWMTS.customLod ;
  var
    m       : TWMTS_TileMatrix ;
    um      : Double ;
    i       : Integer ;
    lod     : TGIS_LayerWebTileLod ;
    topleft : TGIS_Point ;
  begin
    um := unitsToMeters ;

    try
      {$IFNDEF OXYGENE}
        TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs.Sort(
          TComparer<TWMTS_TileMatrix>.Construct( SortMatrix )
        ) ;
      {$ELSE}
        TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs.Sort( @SortMatrix ) ;
      {$ENDIF}
    except

    end;

    for i := 0 to TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs.Count-1 do begin
        m := TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs[i] ;

        if isAxisReversed then
          topleft := GisPoint( m.TopLeftCorner.Y, m.TopLeftCorner.X)
        else
          topleft := GisPoint( m.TopLeftCorner.X, m.TopLeftCorner.Y ) ;

        lod := TGIS_LayerWebTileLod.Create(
                 m.Identifier,
                 m.ScaleDenominator * 0.00028 / um,
                 topleft,
                 Point( m.TileWidth, m.TileHeight ),
                 Rect( 0, 0, m.MatrixWidth, m.MatrixHeight )
               ) ;

        lstLod.Add( lod ) ;
    end ;
  end ;

  function TGIS_LayerWMTS.GetFeatureInfo(
    const _point : TGIS_Point
  ) : String ;
  var
    j       : Integer      ;
    ext     : TGIS_Extent  ;
    ext2    : TGIS_Extent  ;
    pt      : TPoint       ;
    buf     : TBytes       ;
    r       : TGIS_HttpResponse ;
    ptoff   : TPoint       ;
   {$IFDEF DCC}
      mm    : TWMTS_TileMatrix ;
      lnk   : TWMTS_TileMatrixSetLink ;
      lim   : TWMTS_TileMatrixLimits ;
      dim   : TWMTS_Dimension ;
      op    : TWMTS_Operation ;
    {$ENDIF}
    m       : TWMTS_TileMatrix ;
    um      : Double ;
    res     : Double ;
    s       : Double ;
    i       : Integer ;
    tsx     : Double ;
    tsy     : Double ;
    tx      : Double ;
    ty      : Double ;
    tl      : TGIS_Point ;
    col     : Integer ;
    row     : Integer ;
    vres    : Double ;
    vstr    : String ;
    tkn     : TGIS_Tokenizer ;
    sstyle  : String ;
    tpath   : String ;
    ptg     : TGIS_Point ;
    ops     : TWMTS_Operations ;
    sflags  : TReplaceFlags ;
  begin
    if not assigned( FWMTS ) then exit ;

    // check if server supports such request
    if not TWMTS(FWMTS).Capabilities.OperationsMetadata.Operations.TryGetValue(
        'GetFeatureInfo', ops
       ) then begin
      Result := '' ;
      exit ;
    end ;

    pt := Viewer.Ref.MapToScreen( _point ) ;

    // correct pixel position based for sitution when layer extent is not
    // fully encomapssed by visible extent
    ext := self.ProjectedExtent ;
    ext := GisCommonExtent( ext, Viewer.Ref.VisibleExtent ) ;

    ptoff := Viewer.Ref.MapToScreen( GisPoint( ext.XMin, ext.YMax ) ) ;

    pt.X := pt.X - ptoff.X ;
    pt.Y := pt.Y - ptoff.Y ;

    ptg := _point ;

    // caulculate width/height for situation when layer extent is not
    // fully encomapsseed by visible extent
    ext2 := self.UnprojectExtent( Viewer.Ref.VisibleExtent ) ;
    ext  := GisCommonExtent( ext2, Extent ) ;

    vres := Viewer.Ref.ViewerParent.ControlCanvasWidth/(ext.XMax - ext.XMin) ;

    um := unitsToMeters ;
    s := 1/vres ;
    for mm in TWMTS(FWMTS).ActiveMatrixSet.TileMatrixs do begin
      res := mm.ScaleDenominator * 0.00028 / um ;
      m := mm ;
      if res/WMTS_IMAGE_RES_SCALE < s then
        break ;
    end ;

    res := m.ScaleDenominator * 0.00028 / um ;

    if isAxisReversed then begin
      tl.X := m.TopLeftCorner.Y ;
      tl.Y := m.TopLeftCorner.X ;
    end
    else begin
      tl := m.TopLeftCorner ;
    end;

    tsx := m.TileWidth*res ;
    tsy := m.TileHeight*res ;

    col := FloorS(( ptg.X - tl.X ) / tsx ) ;
    row := FloorS(( tl.Y - ptg.Y ) / tsy ) ;
    tx  := tl.X + col * tsx ;
    ty  := tl.Y - row * tsy ;
    i   := TruncS(( ptg.X - tx ) / res ) ;
    j   := TruncS(( ty - ptg.Y ) / res ) ;

    if TWMTS(FWMTS).ActiveLayer.Style.Count > 0 then
      sstyle := TWMTS(FWMTS).ActiveLayer.Style[0].Identifier
    else
      sstyle := 'default' ;

    // parse resourceurl template and prepare url
    if TWMTS(FWMTS).ActiveLayer.ResourceURL.Count > 0 then begin
      vstr := '' ;
      tkn := TGIS_Tokenizer.Create ;
      try
        for i := 0 to TWMTS(FWMTS).ActiveLayer.ResourceURL.Count-1 do begin
          tkn.Execute( TWMTS(FWMTS).ActiveLayer.ResourceURL[i], [';'] ) ;
          if tkn.Result.Count <> 2 then continue ;
          if tkn.Result[0] = sInfoFormat then begin
            vstr := tkn.Result[1] ;

            {$IFDEF DCC}
              sflags := [ rfIgnoreCase,
                          rfReplaceAll
                        ] ;
            {$ELSE}
              sflags := [ TReplaceFlag.rfIgnoreCase,
                          TReplaceFlag.rfReplaceAll
                        ] ;
            {$ENDIF}

            vstr := StringReplace( vstr, '{Style}', sstyle,
                                   sflags
                                 ) ;
            vstr := StringReplace( vstr, '{TileMatrixSet}', sTileMatrixSet,
                                   sflags
                                 ) ;
            vstr := StringReplace( vstr, '{TileMatrix}', m.Identifier,
                                   sflags
                                 ) ;
            vstr := StringReplace( vstr, '{TileRow}', IntToStr( row ),
                                   sflags
                                 ) ;
            vstr := StringReplace( vstr, '{TileCol}', IntToStr( col ),
                                   sflags
                                 ) ;
            vstr := StringReplace( vstr, '{I}', IntToStr( i ),
                                   sflags
                                 ) ;
            vstr := StringReplace( vstr, '{J}', IntToStr( j ),
                                   sflags
                                 ) ;
            for dim in TWMTS(FWMTS).ActiveLayer.Dimensions do
              vstr := StringReplace( vstr, '{'+dim.Identifier+'}',
                                     dim.DefaultValue,
                                     sflags
                                   ) ;

            break ;
          end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
      tpath := vstr ;
    end ;

    if IsStringEmpty( tpath ) then begin
      if TWMTS(FWMTS).Capabilities.OperationsMetadata.Operations.TryGetValue(
          'GetFeatureInfo', ops
         ) then begin
          for op in ops do
            if op.Encoding = 'KVP' then
              sPath := op.XlinkHref ;
      end ;

      tpath := URLGetPath( sPath ) +
               Format( 'Service=WMTS&Request=GetFeatureInfo&Version=1.0.0&Layer=%s&'+
                       'Format=%s&style=%s&TileMatrixSet=%s&TileMatrix=%s&'+
                       'INFOFORMAT=%s&TileRow=%d&TileCol=%d&I=%d&J=%d',
                       [ sLayer, sImageFormat, sstyle, sTileMatrixSet, m.Identifier,
                         sInfoFormat, row, col, i, j ]
                      ) ;
    end ;

    tpath := URLAddParameter( tpath, sOptionalParams ) ;

    {$IFDEF OXYGENE}
      tpath := TemplateProducer( tpath, nil, @passwordCallBackX, False ) ;
    {$ELSE}
      tpath := TemplateProducer( tpath, nil, passwordCallBackX, False ) ;
    {$ENDIF}

    r.Stream := nil ;
    try
      r := TGIS_WebUtils.HttpFetch(
             FProxyUrl + tpath, nil, nil, True, 40000, FUserAgent, FReferer,
             sUserName, sUserPass
           ) ;
      if r.Status = GIS_HTTP_OK then begin
        SetLength( buf, r.Stream.Size ) ;
        if r.Stream.Size <= 0  then
          Result := ''
        else begin
          {$IFDEF OXYGENE}
            r.Stream.Read( buf, r.Stream.Size ) ;
          {$ELSE}
            r.Stream.Read( buf[0], r.Stream.Size ) ;
          {$ENDIF}
          Result := ConvertAnsiString( buf ) ;
        end ;
      end ;
    finally
      FreeObject( r.Stream ) ;
    end ;
  end ;

  function TGIS_LayerWMTS.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  begin
    Result :=
      ( GetParamFromPath( _path, GIS_INI_LAYERSQL_STORAGE ) = 'WMTS' ) or
      ( UpperCase( URLGetParameterValue(_path, 'SERVICE') ) = 'WMTS' ) or
      ( Pos( '/WMTS', UpperCase( _path ) ) >= StringFirst  ) ;
  end ;

  function TGIS_LayerWMTS.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    {$IFDEF DCC}
      layer : TWMTS_Layer ;
      cs    : TWMTS_TileMatrixSetLink ;
      dim   : TWMTS_Dimension ;
    {$ENDIF}
    i       : Integer ;
    fmt     : String ;
    wmts    : TObject;
    pf      : TGIS_PixelSubFormat ;
    url     : String ;
    dstr    : String ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    wmts := TWMTS.Create ;
    try
      parseConfig ;

      TWMTS(wmts).UserAgent  := FUserAgent ;
      TWMTS(wmts).Referer    := FReferer ;
      TWMTS(wmts).UserName   := sUserName ;
      TWMTS(wmts).UserPass   := sUserPass ;
      TWMTS(wmts).ProxyUrl   := FProxyUrl  ;
      TWMTS(wmts).TimeOut    := iTimeOut ;

      url := prepareUrlParameters ;

      TWMTS(wmts).Read( url, nil ) ;

      if not IsStringEmpty( TWMTS(wmts).Error ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SERVER_ERROR ),
                                     TWMTS(wmts).Error, 0
                                    ) ;

      for layer in TWMTS(wmts).Capabilities.Contents.Layers do begin
        dstr := '' ;
        for dim in layer.Dimensions do begin
          dim.Values.Delimiter := ',' ;
          dstr := dim.Identifier + '=' + dim.Values.DelimitedText ;
        end ;
        if dstr <> '' then
          dstr := ';' + dstr ;
        for i := 0 to layer.Formats.Count-1 do begin
          fmt := layer.Formats[i] ;
          if (fmt = GIS_CONTENTTYPE_JPEG) or (fmt = GIS_CONTENTTYPE_JPG) then
            pf := TGIS_PixelSubFormat.JPEG
          else if (fmt = GIS_CONTENTTYPE_PNG) or (fmt = GIS_CONTENTTYPE_PNG24) then
            pf := TGIS_PixelSubFormat.PNG
          else
            pf := TGIS_PixelSubFormat.None ;

          for cs in layer.TileMatrixSetLink do
            Result.Add( TGIS_LayerInfo.Create(
                          layer.Identifier + ';' + fmt + ';' + cs.TileMatrixSet + dstr,
                          layer.Title,
                          TGIS_RegisteredLayerType.Pixel,
                          TGIS_ShapeType.Unknown,
                          pf,
                          nil
                        )
                      ) ;
        end ;
      end
    finally
      FreeObject( wmts ) ;
    end ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerWMTS.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-TTKWP', GIS_PROTOCOL_LAYER_CONNECTOR,
                   TGIS_LayerWMTS, GIS_TTKLAYER_WEB_FILTER,
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Protocol,
                    [ TGIS_RegisteredOperationType.Read
                    ],
                   False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
initialization
    Unit_GisLayerWMTS.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.
