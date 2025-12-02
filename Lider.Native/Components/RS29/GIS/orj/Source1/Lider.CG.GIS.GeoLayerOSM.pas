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
  Encapsulation of a OpenStreetMap (OSM) Layer ( http://wiki.openstreetmap.org )
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerOSM ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerOSM"'}
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
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Variants,
    System.Generics.Collections,
    System.Generics.Defaults,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerCompound,
    Lider.CG.GIS.GeoCsSystems ;
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
  GisLayerOSM = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {#GENDOC:HIDE}
  /// <summary>
  ///   OSM attributes definition.
  /// </summary>
  TGIS_AttributeOSM = {$IFDEF OXYGENE} unit {$ENDIF} record
    /// <summary>
    ///   Attribute name.
    /// </summary>
    Name  : String ;
    /// <summary>
    ///   Attribute value.
    /// </summary>
    Value : String ;
  end ;

  /// <summary>
  ///   Encapsulation of a OSM Layer.
  /// </summary>
  TGIS_LayerOSM = {$IFDEF OXYGENE} public {$ENDIF}
                  class( TGIS_LayerCompoundVector )
    private
      FAdditionalNodes : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // various private variables
      addingObject    : Boolean ;
      fieldName       : String ;
      fieldValue      : String ;
      elementCount    : Cardinal ;
      elementNo       : Cardinal ;
      oAttributes     : TList<TGIS_AttributeOSM> ;
      currShp         : TGIS_Shape ;
      currPtg         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
      currId          : Int64 ;
      currRef         : Int64 ;
      firstRef        : Int64 ;
      lastRef         : Int64 ;
      currType        : Integer ;
      members         : TStringList ;
      nodeList        : TGIS_TemporaryFileStream ;
      wayList         : TGIS_TemporaryFileStream ;
      ptgList         : TGIS_PointList ;
      attrHelper      : TObject ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function  attrNameIndex       ( const _name  : String
                                    ) : Integer ;
      function  attrPairIndex       ( const _name  : String ;
                                      const _value : String
                                    ) : Integer ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // various private routines

      /// <summary>
      ///   Add new point.
      /// </summary>
      /// <param name="_id">
      ///   feature if
      /// </param>
      procedure addPoint      ( const _id      : Int64
                              ) ;

      /// <summary>
      ///   Add new line.
      /// </summary>
      /// <param name="_id">
      ///   feature if
      /// </param>
      procedure addLine       ( const _id      : Int64
                              ) ;

      /// <summary>
      ///   Add new polygon.
      /// </summary>
      /// <param name="_id">
      ///   feature if
      /// </param>
      procedure addPolygon    ( const _id      : Int64
                              ) ;

      /// <summary>
      ///   Add attributes to shape
      /// </summary>
      /// <param name="_atrname">
      ///   field name
      /// </param>
      /// <param name="_atrvalue">
      ///   field value
      /// </param>
      /// <param name="_shp">
      ///   shape
      /// </param>
      procedure addAtrToShp   ( const _atrname  : String     ;
                                const _atrvalue : String     ;
                                const _shp      : TGIS_Shape
                              ) ;

      /// <summary>
      ///   Remember attributes.
      /// </summary>
      /// <param name="_atrname">
      ///   attribute name to remember
      /// </param>
      /// <param name="_atrvalue">
      ///   attribute value to remember
      /// </param>
      procedure addAtrToArray ( const _atrname  : String     ;
                                const _atrvalue : String
                              ) ;

      /// <summary>
      ///   Parse file in XML format.
      /// </summary>
      procedure parseXML ;

      /// <summary>
      ///   Parse file in PBF format.
      /// </summary>
      procedure parsePBF ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp         ; override;

      /// <inheritdoc/>
      procedure fset_CS       ( const _value  : TGIS_CSCoordinateSystem
                              ) ; override;
    protected
      // destructor

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  PreRecognize  ( const _path        : String           ;
                                var   _new_path    : String
                              ) : Boolean ; override;
    public

      /// <summary>
      ///   If True, additional points built on standalone nodes will be added.
      /// </summary>
      property AdditionalNodes : Boolean    read  FAdditionalNodes
                                            write FAdditionalNodes;
   end ;


   {$IFNDEF ANDROID}
   /// <summary>
   ///   Encapsulation of OSM importer class.
   /// </summary>
   TGIS_OSMImporter = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_ObjectDisposable )
    private
      osmp         : TObject ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      vwrObj       : IGIS_Viewer ;
      FOnImportLog : TGetStrProc ;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer handle
      /// </param>
      constructor Create          ( const _viewer : IGIS_Viewer
                                  ) ;
    protected
      // destructor

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Import osm file into a database structure.
      /// </summary>
      /// <param name="_file">
      ///   file path
      /// </param>
      procedure ImportOSMFile     ( const _file : String
                                  ) ;

      /// <summary>
      ///   Export points from osm layers.
      /// </summary>
      /// <param name="_layer">
      ///   destination layer
      /// </param>
      procedure ExportOsmPoints   ( const _layer : TGIS_LayerVector
                                  ) ;

      /// <summary>
      ///   Export lines from osm layers.
      /// </summary>
      /// <param name="_layer">
      ///   destination layer
      /// </param>
      procedure ExportOsmLines    ( const _layer : TGIS_LayerVector
                                  ) ;

      /// <summary>
      ///   Export polygons from osm layers.
      /// </summary>
      /// <param name="_layer">
      ///   destination layer
      /// </param>
      procedure ExportOsmPolygons ( const _layer : TGIS_LayerVector
                                  ) ;

    published // events
      /// <event/>
      /// <summary>
      ///   Will be fired upon any progress to trace import.
      /// </summary>
      property ImportLogEvent : TGetStrProc    read  FOnImportLog
                                               write FOnImportLog ;
   end ;
   {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
uses
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoXmlSax,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoRegistredLayers,
  Lider.CG.GIS.GeoCsFactory,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoGeometryFactory,
  Lider.CG.GIS.GeoDbSqlite,
  Lider.CG.GIS.GeoFilePBF,
  Lider.CG.GIS.GeoConfig ;
{$ENDIF}

type

  T_OsmAttributes = class( TGIS_ObjectDisposable )
    private
      dictIgnoredName : TDictionary<String,Integer> ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function  attrNameIgnored   ( const _name  : String
                                  ) : Boolean ;
      function  attrNameNormalized( const _name  : String
                                  ) : String ;
  end;

  // Encapsulation of sax handler for OSM files.
  T_OsmSaxMemHdl = class( TGIS_SAXContentHandler )
     private
      // OSM layer handle.
      layerOSM   : TGIS_LayerOSM ;
      attrHelper : T_OsmAttributes ;
    public
      // StartElement event.
      // Catches startElement event, gives element name and attributes.
      // _strmnamespaceuri   element Namespace URI
      // _strlocalmame       element name
      // _strqname           element QName
      // _oattributes        element attributes
      procedure StartElement ( const _uri     : String ;
                               const _lname   : String ;
                               const _qname   : String ;
                               const _attribs : IVBSAXAttributes
                             ) ; override;

      // EndElement event handler.
      // Catches endElement event, gives element name.
      // _strnamespaceuri   element Namespace URI
      // _strlocalname      element name
      // _strqname          element QName
      procedure EndElement   ( const _uri     : String ;
                               const _lname   : String ;
                               const _qname   : String
                             ) ; override;

      // Characters - Value of element
      // _strchars   element value
      procedure Characters   ( const _chars   : String
                             ) ; override;
    public
      procedure FatalError   ( const _locator : IVBSAXLocator ;
                               const _message : String        ;
                               const _code    : HResult
                             ) ; override;
    public
      // Create an instance for provided OSM layer
      // _layer   OSM layer
      constructor Create( const _layer : TGIS_LayerOSM ) ;

    {$IFDEF DCC}
      public
        /// <summary>
        ///   Destroys the instance.
        /// </summary>
        destructor Destroy ; override;
    {$ENDIF}
  end ;

  {$IFNDEF ANDROID}
  T_OsmProvider = class( TGIS_ObjectDisposable )
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      {$IFDEF JAVA}
        Db      : TGIS_DbJdbc ;
      {$ELSE}
        Db      : TGIS_DbSqlite ;
      {$ENDIF}
      DbPath    : String ;
      FilePath  : String ;
      ObjectId  : Int64 ;
      PosId     : Int64 ;
      FirstId   : Int64 ;
      LastId    : Int64 ;
      ObjectType: Integer ;
      RelType   : String ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      vwr       : IGIS_Viewer ;
      lastPos   : Integer ;
      maxPos    : Integer ;
      logEvent  : TGetStrProc ;
      elapsed   : Int64 ;
      logText   : String ;
      attrHelper: T_OsmAttributes ;
    private
      // Parse OSM file.
      procedure parseFile ;

      // Create database.
      procedure createDatabase ;

      // Create schema for import.
      procedure createSchema ;

      // Close statements.
      procedure unprepareStatements ;
      procedure createIndexes1 ;
      procedure createIndexes2 ;
      procedure createIndexes3 ;

      // Post parsing and clean up.
      procedure postParsing1 ;

      // Post parsing and clean up.
      procedure postParsing2 ;

      // Post parsing and clean up.
      procedure postParsing3 ;

      // Update way geometry.
      // _wayId    way id
      // _closed   is closed way
      procedure updateWayGeometry ( const _wayId  : Int64 ;
                                    const _closed : Integer
                                  ) ;

      // Write config file.
      // _layer   layer handle
      procedure writeConfig       ( const _layer  : TGIS_LayerVector
                                  ) ;

      // Read feature attributes.
      // _shp   shape handle
      // _id    feature if
      procedure readAttributes    ( const _shp    : TGIS_Shape ;
                                    const _id     : Int64
                                  ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // Build points.
      // _layer   destination layer
      procedure buildPoints       ( const _layer  : TGIS_LayerVector
                                  ) ;

      // Build lines.
      // _layer   destination layer
      procedure buildLines        ( const _layer  : TGIS_LayerVector
                                  ) ;

      // Build polygons.
      // _layer   destination layer
      procedure buildPolygons     ( const _layer  : TGIS_LayerVector
                                  ) ;

      // Build relations.
      // _dwh    direct write helper
      // _poly   if True, crete polygons
      procedure buildRelations    ( const _dwh    :
                                      TGIS_LayerVectorDirectWriteHelper ;
                                    const _poly   : Boolean
                                  ) ;

      // Make BusyShake.
      procedure busyShake         ( const _pos : Integer ;
                                    const _end : Integer
                                  ) ;

      // Make HourglassShake.
      procedure busyShakeEx       ;

      // Log import event
      procedure log              ( const _txt : String
                                  ) ;
      // Log import event
      procedure logBegin         ( const _txt : String
                                  ) ;
      // Log import event
      procedure logEnd           ( const _count : Int64
                                  ) ;

      // Start Stopwatch measure
      procedure startStopwatch    ;

      // Stop Stopwatch measure and get elapsed time
      function  getElapsedTime    : String ;

      // Get elements count.
      function  getElementsCount  ( const _sql : String
                                  ) : Int64 ;
    protected
      // Destructor.
      procedure doDestroy ; override;
    public
      // Constructor.
      // _path     path to osm file
      // _viewer   viewer handle
      constructor Create( const _path     : String ;
                          const _viewer   : IGIS_Viewer ;
                          const _logEvent : TGetStrProc
                         ) ;

      // Open database.
      function  OpenDatabase    : Boolean ;

      // Close database.
      function  CloseDatabase   : Boolean ;

      // True if database is valid.
      function  IsDatabaseValid : Boolean ;

      // Load osm file into database.
      function  LoadOSMFile     : Boolean ;
  end ;


  // Encapsulation of sax sql handler for OSM files.
  T_OsmSaxSqlHdl = class( TGIS_SAXContentHandler )
    private
      // OSM layer handle.
      provider  : T_OsmProvider ;
      elementNo : Int64 ;
      nodes     : Int64 ;
      ways      : Int64 ;
      relations : Int64 ;
      doNodes   : Boolean ;
      doWays    : Boolean ;
      doRels    : Boolean ;
    public
      // StartElement event.
      // Catches startElement event, gives element name and attributes.
      // _strmnamespaceuri   element Namespace URI
      // _strlocalmame       element name
      // _strqname           element QName
      // _oattributes        element attributes
      procedure StartElement ( const _uri     : String ;
                               const _lname   : String ;
                               const _qname   : String ;
                               const _attribs : IVBSAXAttributes
                             ) ; override;

      // EndElement event handler.
      // Catches endElement event, gives element name.
      // _strnamespaceuri   element Namespace URI
      // _strlocalname      element name
      // _strqname          element QName
      procedure EndElement   ( const _uri     : String ;
                               const _lname   : String ;
                               const _qname   : String
                             ) ; override;

      // Characters - Value of element
      // _strchars   element value
      procedure Characters   ( const _chars   : String
                             ) ; override;
    public
      procedure FatalError   ( const _locator : IVBSAXLocator ;
                               const _message : String        ;
                               const _code    : HResult
                             ) ; override;
    public
      // Create an instance for provided OSM layer
      // _layer   OSM layer
      constructor Create( const _provider : T_OsmProvider ) ;
  end ;
  {$ENDIF}

  T_OsmStyler = class
    public
      // Apply style for points.
      // _layer   layer handle
      class procedure SetPointLayer   ( const _layer : TGIS_LayerVector
                                      ) ;

      // Apply style for lines.
      // _layer   layer handle
      class procedure SetLineLayer    ( const _layer : TGIS_LayerVector
                                      ) ;

      // Apply style for polygons.
      // _layer   layer handle
      class procedure SetPolygonLayer ( const _layer : TGIS_LayerVector
                                      ) ;
  end ;

  T_OsmBuilder = class( TGIS_ObjectDisposable )
    private
      layerOSM    : TGIS_LayerOSM ;
      attrHelper  : T_OsmAttributes ;
    private
      procedure buildMultiPolygon   ( const _relation : TGIS_OSMRelation
                                     ) ;
      procedure buildMultiLineString( const _relation : TGIS_OSMRelation
                                     ) ;
    protected
      // Destructor.
      procedure doDestroy ; override;
    public
      procedure doNode     ( const _node      : TGIS_OSMNode
                            )  ;
      procedure doWay      ( const _way       : TGIS_OSMWay
                            ) ;
      procedure doRelation ( const _relation  : TGIS_OSMRelation
                            ) ;
      procedure doExtent   ( const _extent    : TGIS_Extent
                            ) ;
    public
      constructor Create( const _layer : TGIS_LayerOSM ) ;
  end ;

  {$IFNDEF ANDROID}
  T_PbfBuilder = class
    private
      // OSM layer handle.
      provider  : T_OsmProvider ;
      elementNo : Int64 ;
      nodes     : Int64 ;
      ways      : Int64 ;
      relations : Int64 ;
      doNodes   : Boolean ;
      doWays    : Boolean ;
      doRels    : Boolean ;
    protected
      procedure doBusy ;
    public
      constructor Create   ( const _provider  : T_OsmProvider
                           ) ;
      procedure doNode     ( const _node      : TGIS_OSMNode
                           )  ;
      procedure doWay      ( const _way       : TGIS_OSMWay
                           ) ;
      procedure doRelation ( const _relation  : TGIS_OSMRelation
                           ) ;
      procedure doExtent   ( const _extent    : TGIS_Extent
                           ) ;
  end ;
  {$ENDIF}


    {$IFDEF JAVA}
      T_listSortByArea = class( java.util.Comparator<TGIS_Shape> )
        public
          function    compare ( _item1    : TGIS_Shape ;
                                _item2    : TGIS_Shape
                              ) : Integer ;
      end ;

      function T_listSortByArea.compare(
        _item1 : TGIS_Shape ;
        _item2 : TGIS_Shape
      ) : Integer ;
      begin
        Result := ShapesAreasCompare( _item1, _item2 ) ;
      end ;
  {$ENDIF}

const
    OSM_AMENITY        = 'amenity';
    OSM_HIGHWAY        = 'highway';
    OSM_ID             = 'id';
    OSM_LANDUSE        = 'landuse';
    OSM_LAT            = 'lat';
    OSM_LEISURE        = 'leisure' ;
    OSM_LON            = 'lon';
    OSM_MEMBER         = 'member';
    OSM_NAME           = 'name';
    OSM_NATURAL        = 'natural' ;
    OSM_NODE           = 'node' ;
    OSM_RAILWAY        = 'railway' ;
    OSM_REF            = 'ref' ;
    OSM_RELATION       = 'relation' ;
    OSM_ROLE           = 'role';
    OSM_TAG            = 'tag';
    OSM_TAGS           = 'tags';
    OSM_TAG_AREA       = 'area';
    OSM_TAG_KEY        = 'k';
    OSM_TAG_KEY_POI    = 'amenity';
    OSM_TAG_NAME       = 'name';
    OSM_TAG_VAL        = 'v';
    OSM_TYPE           = 'type';
    OSM_VALUE_MPOLYGON = 'multipolygon' ;
    OSM_VALUE_MLINE    = 'multilinestring' ;
    OSM_VALUE_YES      = 'yes';
    OSM_WATERWAY       = 'waterway' ;
    OSM_WAY            = 'way';
    OSM_WAY_NODE       = 'nd' ;

    OSM_SQL_TYPE_NODE       = 0 ;
    OSM_SQL_TYPE_WAY        = 1 ;
    OSM_SQL_TYPE_RELATION   = 2 ;

    OSM_SQL_STMT_NODE       = 0 ;
    OSM_SQL_STMT_WAY        = 1 ;
    OSM_SQL_STMT_RELATION   = 2 ;
    OSM_SQL_STMT_TAG        = 3 ;
    OSM_SQL_STMT_WAY_MEMBER = 4 ;
    OSM_SQL_STMT_REL_MEMBER = 5 ;

    OSM_TAGS_MAX_SIZE       = 250 ;

    GIS_OSM_ID              = 'osm_id';

//==============================================================================
//  T_OsmSaxMemHdl
//==============================================================================

  constructor T_OsmSaxMemHdl.Create(
    const _layer : TGIS_LayerOSM
  ) ;
  begin
    inherited Create ;

    layerOSM   := _layer ;
    attrHelper := T_OsmAttributes.Create ;
  end ;

  {$IFDEF DCC}
    destructor T_OsmSaxMemHdl.Destroy ;
    begin
      FreeObject( attrHelper ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure T_OsmSaxMemHdl.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
   ) ;
  var
    i   : Integer ;
  begin
    inherited ;

    if ( _lname = OSM_NODE ) or
       ( _lname = OSM_WAY  ) then begin
      layerOSM.addingObject  := True ;
      layerOSM.oAttributes.Clear ;
    end ;

    if layerOSM.addingObject then begin

      if ( _attribs.Length > 0 ) then begin
        if ( _lname = OSM_NODE ) then begin
          for i := 0 to _attribs.Length - 1 do begin
            if      _attribs.GetLocalName( i ) = OSM_LAT then
              layerOSM.currPtg.Y := DotStrToFloat( _attribs.GetValue( i ) )
            else if _attribs.GetLocalName( i ) = OSM_LON then
              layerOSM.currPtg.X := DotStrToFloat( _attribs.GetValue( i ) )
            else if _attribs.GetLocalName( i ) = OSM_ID  then
              layerOSM.currId := StrToInt64( _attribs.GetValue( i ) );
          end ;

          layerOSM.nodeList.AddFeature( layerOSM.currId, layerOSM.currPtg ) ;
        end
        else if ( _lname = OSM_WAY ) then begin
          for i := 0 to _attribs.Length - 1 do begin
            if _attribs.GetLocalName( i ) = OSM_ID then begin
              layerOSM.currId := StrToInt64( _attribs.GetValue( i ) ) ;
              break ;
            end ;
          end ;

          layerOSM.currType := 1 ;
          layerOSM.firstRef := -1 ;
          layerOSM.lastRef  := -1 ;
        end
        else if ( _lname = OSM_WAY_NODE ) then begin
          layerOSM.currRef := -1 ;
          for i := 0 to _attribs.Length - 1 do begin
            if _attribs.GetLocalName( i ) = OSM_REF then begin
              layerOSM.currRef := StrToInt64( _attribs.GetValue( i ) ) ;
              break ;
            end ;
          end ;

          if layerOSM.firstRef = -1 then
            layerOSM.firstRef := layerOSM.currRef ;
          layerOSM.lastRef := layerOSM.currRef ;

          if (layerOSM.currRef <> -1) and
             layerOSM.nodeList.GetFeature2D( layerOSM.currRef, layerOSM.currPtg )
          then
            layerOSM.ptgList.Add( layerOSM.currPtg ) ;
        end
        else if ( _lname = OSM_TAG ) then begin
          for i := 0 to _attribs.Length - 1 do begin
            if      _attribs.GetLocalName( i ) = OSM_TAG_KEY then
              layerOSM.fieldName := _attribs.GetValue( i )
            else if _attribs.GetLocalName( i ) = OSM_TAG_VAL then
              layerOSM.fieldValue := _attribs.GetValue( i ) ;
          end ;
          layerOSM.addAtrToArray( layerOSM.fieldName, layerOSM.fieldValue ) ;
        end
        else if ( _lname = OSM_RELATION ) then begin
          for i := 0 to _attribs.Length - 1 do begin
            if _attribs.GetLocalName( i ) = OSM_ID then begin
              layerOSM.currId := StrToInt64( _attribs.GetValue( i ) ) ;
              break ;
            end ;
          end ;
        end
        else if ( _lname = OSM_MEMBER ) then begin
          layerOSM.currRef := -1 ;
          for i := 0 to _attribs.Length - 1 do begin
            if _attribs.GetLocalName( i ) = OSM_REF then begin
              layerOSM.currRef := StrToInt64( _attribs.GetValue( i ) ) ;
              break ;
            end ;
          end ;
          if layerOSM.currRef > 0 then
            layerOSM.members.Add( IntToStr( layerOSM.currRef ) ) ;
        end

      end
    end
  end ;

  procedure T_OsmSaxMemHdl.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  var
    i, j    : Integer ;
    k       : Integer ;
    shp     : TGIS_Shape ;
    fld     : TGIS_FieldInfo ;
    val     : Variant ;
    buf     : TBytes ;
    uid     : TGIS_Uid ;
    useful  : Boolean ;
  begin
    inherited;

    inc( layerOSM.elementNo );
    if layerOSM.elementNo mod 200 = 0 then
      layerOSM.HourglassShake ;

    if ( _lname = OSM_NODE ) then begin
      if layerOSM.AdditionalNodes then begin
        useful := False ;
        for j := 0 to layerOSM.oAttributes.Count - 1 do begin
          if not attrHelper.attrNameIgnored( layerOSM.oAttributes[j].Name ) then begin
            useful := True ;
            break ;
          end ;
        end ;
        if useful then begin
          layerOSM.addPoint( layerOSM.currId ) ;
          layerOSM.currShp.AddPoint( layerOSM.currPtg ) ;
          for j := 0 to layerOSM.oAttributes.Count - 1 do
            layerOSM.addAtrToShp( layerOSM.oAttributes[j].Name,
                                  layerOSM.oAttributes[j].Value,
                                  layerOSM.currShp
                                ) ;
          layerOSM.currShp.Unlock ;
        end ;
      end ;

      layerOSM.addingObject := False ;
      layerOSM.oAttributes.Clear ;
    end
    else if ( _lname = OSM_WAY ) then begin
      if layerOSM.firstRef = layerOSM.lastRef then
        layerOSM.currType := 0 ;

      if layerOSM.currType = 0 then begin
        if layerOSM.attrPairIndex( 'area', 'yes' ) > -1 then
          layerOSM.currType := 0
        else begin
          if layerOSM.attrPairIndex( 'area', 'no' ) > -1 then
            layerOSM.currType := 1
          else if layerOSM.attrPairIndex( 'junction', 'roundabout' ) > -1 then
            layerOSM.currType := 1
          else if layerOSM.attrNameIndex( 'barrier' ) > -1 then
            layerOSM.currType := 1
          else if layerOSM.attrPairIndex( 'highway', 'services' ) > -1 then
            layerOSM.currType := 1 ;
        end;
      end ;

      if layerOSM.currType > -1 then begin
        case layerOSM.currType of
          1 : layerOSM.addLine( layerOSM.currId ) ;
          0 : layerOSM.addPolygon( layerOSM.currId ) ;
        end ;

        SetLength( buf, sizeOf(TGIS_Uid)+1 ) ;
        {$IFDEF JAVA}
          System.arraycopy( BitConverter.GetBytes( layerOSM.currShp.Uid ), 0, buf, 0, sizeOf(TGIS_Uid) ) ;
        {$ENDIF}
        {$IFDEF DCC}
          Move( layerOSM.currShp.Uid, buf[0], sizeOf(TGIS_Uid) ) ;
        {$ENDIF}
        {$IFDEF CLR}
          System.Buffer.BlockCopy(
            BitConverter.GetBytes( layerOSM.currShp.Uid ),
            0, buf, 0, sizeOf(TGIS_Uid)
          ) ;
        {$ENDIF}
        buf[sizeOf(TGIS_Uid)] := layerOSM.currType ;
        layerOSM.wayList.AddFeature( layerOSM.currId, buf ) ;

        // add points to object
        for i := 0 to layerOSM.ptgList.Count - 1 do
          layerOSM.currShp.AddPoint( layerOSM.ptgList[ i ] ) ;

        // add attributes to object
        for i := 0 to layerOSM.oAttributes.Count - 1 do
          layerOSM.addAtrToShp( layerOSM.oAttributes[i].Name,
                                layerOSM.oAttributes[i].Value,
                                layerOSM.currShp
                              ) ;
        layerOSM.oAttributes.Clear ;
        layerOSM.currType := -1 ;
        layerOSM.ptgList.Clear ;
        layerOSM.currShp.Unlock ;
      end ;
    end
    else if ( _lname = OSM_RELATION ) then begin
      layerOSM.currType := 1 ;
      if layerOSM.attrPairIndex( OSM_TYPE, OSM_VALUE_MPOLYGON ) > -1 then
        layerOSM.currType := 0 ;

      case layerOSM.currType of
        1 : layerOSM.addLine( layerOSM.currId ) ;
        0 : layerOSM.addPolygon( layerOSM.currId ) ;
      end ;

      if layerOSM.currType>=0 then begin
        layerOSM.currShp.Reset ;
        for i := 0 to layerOSM.members.Count-1 do begin
          shp := nil ;
          if layerOSM.wayList.GetFeature(
               StrToInt64( layerOSM.members[i] ), buf ) then
          begin
            {$IFDEF OXYGENE}
              uid := BitConverter.ToInt64( buf, 0 ) ;
            {$ELSE}
              Move( buf[0], uid, sizeOf(TGIS_Uid) ) ;
            {$ENDIF}
            shp := TGIS_LayerVector( layerOSM.Layer[buf[sizeOf(TGIS_Uid)]] ).GetShape( uid ) ;
          end;

          if assigned( shp ) then begin
            shp.Lock( TGIS_Lock.Projection ) ;
            for j := 0 to shp.GetNumParts - 1 do begin
              layerOSM.currShp.AddPart ;
              for k := 0 to shp.GetPartSize( j ) - 1 do
                layerOSM.currShp.AddPoint3D( shp.GetPoint3D( j, k ) ) ;
            end ;
            shp.Unlock ;
            shp.IsHidden := True ;

            for j := 0 to layerOSM.currShp.Layer.Fields.Count-1 do begin
              fld := TGIS_FieldInfo(layerOSM.currShp.Layer.Fields[j]) ;
              if fld.Name = GIS_OSM_ID then continue ;
              if shp.Layer.FindField( fld.Name ) > -1 then begin
                val := shp.GetFieldEx( fld.Name ) ;
                if VarIsEmpty( val ) or VarIsNull( val ) then
                  continue
                else
                  layerOSM.currShp.SetField( fld.Name, val );
              end;
            end ;
          end ;
        end ;

        // add attributes to object
        for i := 0 to layerOSM.oAttributes.Count - 1 do
          layerOSM.addAtrToShp( layerOSM.oAttributes[i].Name,
                                layerOSM.oAttributes[i].Value,
                                layerOSM.currShp
                              ) ;
        layerOSM.currShp.Unlock ;
      end;
      layerOSM.oAttributes.Clear ;
      layerOSM.currType   := -1 ;
      layerOSM.members.Clear ;

    end ;

  end ;

  procedure T_OsmSaxMemHdl.Characters(
    const _chars   : String
  ) ;
  begin
    inherited ;

  end ;

  procedure T_OsmSaxMemHdl.FatalError(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    if not IsStringEmpty( _message ) then begin
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_TXTFILESTRUCT ) + '; ' + _message,
        sPath,
        _locator.LineNumber
      ) ;
    end ;
  end ;

{$IFNDEF ANDROID}
//==============================================================================
//  T_OsmSaxSqlHdl
//==============================================================================

  constructor T_OsmSaxSqlHdl.Create(
    const _provider : T_OsmProvider
  ) ;
  begin
    inherited Create;

    provider := _provider ;
    elementNo := 0 ;
    nodes     := 0 ;
    ways      := 0 ;
    relations := 0 ;
    doNodes   := False ;
    doWays    := False ;
    doRels    := False ;
  end ;

  procedure T_OsmSaxSqlHdl.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
   ) ;
  var
    id  : Int64 ;
    ref : Int64 ;
    tp  : Integer ;
    lat : Double ;
    lon : Double ;
    val : String ;
    k,v : String ;
  begin
    inherited ;

    if ( _lname = OSM_NODE ) then begin
      id  := StrToInt64( _attribs.GetValueFromQName( OSM_ID ) ) ;
      lat := DotStrToFloat( _attribs.GetValueFromQName( OSM_LAT ) ) ;
      lon := DotStrToFloat( _attribs.GetValueFromQName( OSM_LON ) ) ;
      provider.ObjectType := OSM_SQL_TYPE_NODE ;
      provider.ObjectId := id ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_NODE,
        'INSERT INTO node(id, lat, lon) VALUES(:id, :lat, :lon)'
       ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'id' , id,  -1 ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'lat', lat, -1 ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'lon', lon, -1 ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_NODE ) ;

      inc( nodes ) ;

      if not doNodes then
        provider.logBegin( ' nodes' ) ;
      doNodes := True ;
    end
    else if ( _lname = OSM_WAY ) then begin
      id  := StrToInt64( _attribs.GetValueFromQName( OSM_ID ) ) ;
      provider.ObjectId := id ;
      provider.PosId    := 1 ;
      provider.FirstId  := -1 ;
      provider.ObjectType := OSM_SQL_TYPE_WAY ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_WAY,
        'INSERT INTO way (id, closed) VALUES (:id, :closed)'
       ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_WAY, 'id' , id,  -1 ) ;

      if doNodes then begin
        provider.logEnd( nodes ) ;
        inc( provider.lastPos ) ;
        provider.busyShakeEx ;
      end ;
      doNodes := False ;

      if not doWays then
        provider.logBegin( ' ways' ) ;
      doWays  := True ;
    end
    else if ( _lname = OSM_WAY_NODE ) then begin
      ref := StrToInt64( _attribs.GetValueFromQName( OSM_REF ) ) ;
      if provider.FirstId = -1 then
        provider.FirstId := ref ;
      provider.LastId := ref ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_WAY_MEMBER,
        'INSERT INTO way_member ( way_id, pos_id, node_id ) ' +
        'VALUES (:way_id, :pos_id, :node_id )'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_WAY_MEMBER, 'way_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_WAY_MEMBER, 'pos_id' , provider.PosId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_WAY_MEMBER, 'node_id', ref, -1
      ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_WAY_MEMBER ) ;
      inc( provider.PosId ) ;
    end
    else if ( _lname = OSM_RELATION ) then begin
      id  := StrToInt64( _attribs.GetValueFromQName( OSM_ID ) ) ;
      provider.ObjectId := id ;
      provider.PosId    := 1 ;
      provider.ObjectType := OSM_SQL_TYPE_RELATION ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_RELATION,
        'INSERT INTO relation ( id, type ) ' +
        'VALUES (:id, :type)'
       ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_RELATION, 'id', id, -1 ) ;

      if doWays then begin
        provider.logEnd( ways ) ;
        inc( provider.lastPos ) ;
        provider.busyShakeEx ;
      end ;
      doWays  := False ;

      if not doRels then
        provider.logBegin( ' relations' ) ;
      doRels  := True ;
    end
    else if ( _lname = OSM_MEMBER ) then begin
      ref := StrToInt64( _attribs.GetValueFromQName( OSM_REF ) ) ;
      val := _attribs.GetValueFromQName( OSM_TYPE ) ;

      if val = OSM_WAY then
        tp := OSM_SQL_TYPE_WAY
      else if val = OSM_NODE then
        tp := OSM_SQL_TYPE_NODE
      else if val = OSM_RELATION then
        tp := OSM_SQL_TYPE_RELATION
      else
        tp := -1 ;

      val := _attribs.GetValueFromQName( OSM_ROLE ) ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_REL_MEMBER,
        'INSERT INTO relation_member '+
        '( relation_id, pos_id, member_id, member_type, role ) ' +
        'VALUES (:relation_id, :pos_id, :member_id, :member_type, :role )'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'relation_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'pos_id' , provider.PosId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'member_id' , ref, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'member_type' , tp, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'role' , val, -1
      ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_REL_MEMBER ) ;

      inc( provider.PosId ) ;
    end
    else if ( _lname = OSM_TAG ) then begin
      k := _attribs.GetValueFromQName( OSM_TAG_KEY ) ;
      v := _attribs.GetValueFromQName( OSM_TAG_VAL ) ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_TAG,
        'INSERT INTO tag ( object_id, object_type, key, val ) ' +
        'VALUES (:object_id, :object_type, :key, :val)'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_type' , provider.ObjectType, -1
      ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'key' , k, -1 ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'val' , v, -1 ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_TAG ) ;

      if provider.ObjectType = OSM_SQL_TYPE_RELATION then
        if k = OSM_TYPE then
          provider.RelType := v ;
    end ;
  end ;

  procedure T_OsmSaxSqlHdl.EndElement(
    const _uri   : String ;
    const _lname : String ;
    const _qname : String
   ) ;
  begin
    inherited ;

    if ( _lname = OSM_WAY ) then begin
      if provider.FirstId = provider.LastId then
        provider.Db.sqlTableSetField( OSM_SQL_STMT_WAY, 'closed' , 1, -1 )
      else
        provider.Db.sqlTableSetField( OSM_SQL_STMT_WAY, 'closed' , 0, -1 ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_WAY ) ;
      provider.FirstId := -1 ;
      inc( ways ) ;
    end
    else if ( _lname = OSM_RELATION ) then begin
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_RELATION, 'type', provider.RelType, -1
      ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_RELATION ) ;
      inc( relations ) ;
    end
    else if ( _lname = 'osm' ) then begin
      provider.logEnd( relations ) ;
      inc( provider.lastPos ) ;
      provider.busyShakeEx ;
    end ;

    inc( elementNo ) ;
    if elementNo mod 10000 = 0 then begin
      if assigned( provider.vwr ) then
        provider.vwr.HourglassShake ;
    end ;

    if elementNo mod 1000000 = 0 then begin
      if doNodes then
        provider.log( Format( ' processed %d', [nodes] ) )
      else if doWays then
        provider.log( Format( ' processed %d', [ways] ) )
      else if doRels then
        provider.log( Format( ' processed %d', [relations] ) )
    end ;

  end ;

  procedure T_OsmSaxSqlHdl.Characters(
    const _chars : String
  ) ;
  begin
    inherited ;

  end ;

  procedure T_OsmSaxSqlHdl.FatalError(
    const _locator : IVBSAXLocator ;
    const _message : String        ;
    const _code    : HResult
  ) ;
  begin
    if not IsStringEmpty( _message ) then begin
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_TXTFILESTRUCT ) + '; ' + _message,
        sPath,
        _locator.LineNumber
      ) ;
    end ;
  end ;


//==============================================================================
// T_OsmProvider
//==============================================================================

  constructor T_OsmProvider.Create(
    const _path     : String ;
    const _viewer   : IGIS_Viewer ;
    const _logEvent : TGetStrProc
  ) ;
  begin
    inherited Create ;
    vwr       := _viewer ;
    DbPath    := GetFilePath( _path ) + GetFileName( _path ) + '.ttkdb' ;
    FilePath  := _path ;
    logEvent  := _logEvent ;
    attrHelper:= T_OsmAttributes.Create ;

    {$IFDEF JAVA}
      Db := TGIS_DbJdbc.Create ;
    {$ELSE}
      Db := TGIS_DbSqlite.Create ;
    {$ENDIF}
    Db.InitializeProvider ;
  end ;

  procedure T_OsmProvider.doDestroy ;
  begin
    FreeObject( attrHelper ) ;
    FreeObject( Db ) ;

    inherited ;
  end ;

  procedure T_OsmProvider.parseFile ;
  var
    sax   : T_OsmSaxSqlHdl ;
    pbf   : TGIS_PBFParser ;
    pbfb  : T_PbfBuilder ;
  begin
    lastPos := 3 ;
    log( 'Parsing file' ) ;

    Db.sqlTransactGlobalUpdateStart ;
    try
      Db.InBatchMode := True ;
      if UpperCase( GetFileExt( FilePath ) ) = '.PBF' then begin
        pbfb := T_PbfBuilder.Create( Self ) ;
        try
          pbf := TGIS_PBFParser.Create ;
          try
            pbf.NodeEvent     := {$IFDEF OXYGENE}@{$ENDIF}pbfb.doNode ;
            pbf.WayEvent      := {$IFDEF OXYGENE}@{$ENDIF}pbfb.doWay ;
            pbf.RelationEvent := {$IFDEF OXYGENE}@{$ENDIF}pbfb.doRelation ;
            pbf.ExtentEvent   := {$IFDEF OXYGENE}@{$ENDIF}pbfb.doExtent ;

            pbf.Parse( FilePath ) ;
          finally
            FreeObject( pbf ) ;
          end ;
        finally
          FreeObject( pbfb ) ;
        end ;
      end
      else begin
        sax := T_OsmSaxSqlHdl.Create( Self ) ;
        try
          try
            if SafeFileExists( FilePath ) then
              sax.LoadFromFile( FilePath )
          except
            on e : Exception do begin
              raise EGIS_Exception.Create(
                      _rsrc( GIS_RS_ERR_FILEBADFORMAT ) + '; ' +
                      e.Message, FilePath, 0
                    ) ;
            end ;
          end ;
        finally
          FreeObject( sax ) ;
        end ;
      end ;
    finally
      Db.InBatchMode := False ;
      Db.sqlTransactGlobalUpdateCommit ;
    end;
  end ;

  procedure T_OsmProvider.createDatabase ;
  var
    cfg : TStringList ;
  begin
    lastPos := 1 ;
    log( 'Create Database' ) ;

    cfg := TStringList.Create ;
    try
      {$IFDEF JAVA}
        cfg.Values[ GIS_INI_LAYERSQL_CONNECTOR_JDBC  ] := Format( 'jdbc:sqlite:%s', [DbPath] ) ;
        cfg.Values[ GIS_INI_LAYERSQL_CONNECTOR_DRIVER] := 'org.sqlite.JDBC' ;
      {$ELSE}
        cfg.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ] := DbPath ;
      {$ENDIF}
      cfg.Values[ GIS_SQL_PARAMS_ENGINEOPTIONS      ] := '16' ;
      cfg.Values[ 'PRAGMA synchronous' ] := 'NORMAL' ;
      cfg.Values[ 'PRAGMA journal_mode' ] := 'OFF' ;
//      cfg.Values[ 'PRAGMA temp_store' ] := 'MEMORY' ;
//      cfg.Values[ 'PRAGMA page_size' ] := '4096' ;
//      cfg.Values[ 'PRAGMA cache_size' ] := '400000' ;
//      cfg.Values[ 'PRAGMA count_changes' ] := 'OFF' ;
//      cfg.Values[ 'PRAGMA locking_mode' ] := 'EXCLUSIVE' ;
//      cfg.Values[ 'PRAGMA auto_vacuum' ] := 'NONE' ;
      cfg.Text := cfg.Text + #13#10 + GetSQLDialect( GIS_SQL_DIALECT_NAME_SQLITE ) ;
      Db.sqlInitialize( cfg, cfg ) ;
      Db.sqlConnect( '', cfg ) ;
    finally
      FreeObject( cfg ) ;
    end ;
  end ;

  procedure T_OsmProvider.createSchema ;
  begin
    lastPos := 2 ;
    log( 'Create Schema' ) ;

    Db.sqlExec( 'CREATE TABLE node (' +
                ' id INTEGER PRIMARY KEY,' +
                ' lat DOUBLE, lon DOUBLE, ' +
                ' usage INTEGER DEFAULT 0,' +
                ' poi INTEGER DEFAULT 0' +
                ')'
               ) ;
    Db.sqlExec( 'CREATE TABLE way (' +
                ' id INTEGER PRIMARY KEY,' +
                ' usage INTEGER DEFAULT 0,' +
                ' closed INTEGER DEFAULT 0,' +
                ' xmin DOUBLE, xmax DOUBLE, ymin DOUBLE, ymax DOUBLE,' +
                ' geom BLOB' +
                ')'
               ) ;
    Db.sqlExec( 'CREATE TABLE relation (' +
                ' id INTEGER PRIMARY KEY,' +
                ' type VARCHAR2' +
                ')'
               ) ;
    Db.sqlExec( 'CREATE TABLE tag (' +
                ' id INTEGER PRIMARY KEY,' +
                ' object_id INTEGER,' +
                ' object_type INTEGER,' +
                ' key VARCHAR2,' +
                ' val VARCHAR2,' +
                ' closed INTEGER DEFAULT 0' +
                ')'
               ) ;
    Db.sqlExec( 'CREATE TABLE way_member (' +
                ' id INTEGER PRIMARY KEY,' +
                ' way_id INTEGER,' +
                ' pos_id INTEGER,' +
                ' node_id INTEGER' +
                ')'
               ) ;
    Db.sqlExec( 'CREATE TABLE relation_member (' +
                ' id INTEGER PRIMARY KEY,' +
                ' relation_id INTEGER,' +
                ' pos_id INTEGER,' +
                ' member_id INTEGER,' +
                ' member_type INTEGER,' +
                ' role VARCHAR2' +
                ')'
               ) ;
  end ;

  procedure T_OsmProvider.unprepareStatements ;
  begin
    inc( lastPos ) ;
    logBegin( 'Cleanup' ) ;
    Db.sqlTableClose( OSM_SQL_STMT_NODE       ) ;
    Db.sqlTableClose( OSM_SQL_STMT_WAY        ) ;
    Db.sqlTableClose( OSM_SQL_STMT_RELATION   ) ;
    Db.sqlTableClose( OSM_SQL_STMT_TAG        ) ;
    Db.sqlTableClose( OSM_SQL_STMT_WAY_MEMBER ) ;
    Db.sqlTableClose( OSM_SQL_STMT_REL_MEMBER ) ;
    logEnd(0) ;
  end ;


  procedure T_OsmProvider.createIndexes1 ;
  begin
    inc( lastPos ) ;
    log( 'Create Indexes' ) ;
    busyShakeEx ;
    logBegin(' idx_way_member 1') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_wm_w ON way_member (way_id)' ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' idx_way_member 2') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_wm_n ON way_member (node_id)' ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' idx_rel_member') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_rm_m ON relation_member (member_id)' ) ;
    logEnd(0) ;
    busyShakeEx ;
  end ;

  procedure T_OsmProvider.createIndexes2 ;
  begin
    inc( lastPos ) ;
    log( 'Create Indexes' ) ;
    logBegin(' idx_tag 1') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_tag_t ON tag (object_id)' ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' idx_tag 2') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_tag_o ON tag (object_type)' ) ;
    logEnd(0) ;
    busyShakeEx ;
  end ;

  procedure T_OsmProvider.createIndexes3 ;
  begin
    inc( lastPos ) ;
    log( 'Create Indexes' ) ;
    logBegin(' idx_node') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_node_u ON node (usage)' ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' idx_node_poi') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_node_p ON node (poi)' ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' idx_way') ;
    Db.sqlExec( 'CREATE INDEX IF NOT EXISTS idx_way_c ON way (closed,usage)' ) ;
    logEnd(0) ;
    busyShakeEx ;
  end ;

  procedure T_OsmProvider.postParsing1 ;
  begin
    inc( lastPos ) ;
    log( 'Do postparsing usage' ) ;
    logBegin(' node usage') ;
    Db.sqlExec( 'update node set usage=1 where node.id = '+
                '(select wm.node_id from way_member wm where wm.node_id=node.id)'
               );
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' way usage') ;
    Db.sqlExec( 'update way set usage=1 where way.id = (select member_id from'+
                ' relation_member rm where rm.member_id=way.id and rm.member_type=1)'
               ) ;
    logEnd(0) ;
    busyShakeEx ;
  end ;

  procedure T_OsmProvider.postParsing2 ;
  begin
    inc( lastPos ) ;
    log( 'Do postparsing closed' ) ;

    logBegin(' tag') ;
    Db.sqlExec( 'update tag set closed=1 where object_type=1 and '+
                '(key=''area'' and val=''yes'')'
               ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' way 1') ;
    Db.sqlExec( 'update way set closed=1 where id in ( select object_id from tag '+
                'where object_type=1 and closed=1)'
               ) ;
    logEnd(0) ;
    busyShakeEx ;
    logBegin(' way 2') ;
    Db.sqlExec( 'update way set closed=0 where closed=1 and id in '+
                '( select object_id from tag where object_type=1 and '+
                '(key=''area'' and val=''no'') or (key=''junction'' and '+
                'val=''roundabout'') or (key=''barrier'') or (key=''highway'''+
                ' and val<>''services''))'
               ) ;
    logEnd(0) ;
    busyShakeEx ;

  end ;

  procedure T_OsmProvider.postParsing3 ;
  var
    wayid  : Int64 ;
    closed : Integer ;
    elm    : Int64 ;
  begin
    inc( lastPos ) ;
    log( 'Do postparsing way geometry' ) ;

    elm := 0 ;
    Db.sqlTransactGlobalUpdateStart ;
    try
      Db.sqlQueryOpen( 'SELECT id, closed FROM way', 0 ) ;
      Db.cursorOpen(1) ;
      Db.sqlTableClose(0) ;
      logBegin(' geometry') ;
      try
        while not Db.sqlQueryEof(0) do begin
          wayid  := VarToInt64( Db.sqlQueryGetFieldById(0,0) ) ;
          closed := VarToInt32( Db.sqlQueryGetFieldById(1,0) ) ;

          updateWayGeometry( wayid, closed ) ;
          inc( elm ) ;
          if ( elm mod 1000 ) = 0 then
            busyShakeEx ;

          Db.sqlQueryMoveNext(0) ;
        end ;
      finally
        Db.sqlQueryClose(0) ;
        Db.cursorClose(1) ;
        Db.sqlTableClose(0) ;
        logEnd(0) ;
      end;
      busyShakeEx ;
    finally
      Db.sqlTransactGlobalUpdateCommit ;
    end;

    logBegin(' tags') ;
    Db.sqlExec( 'delete from tag where (key=''created_by'' or key=''converted_by'''+
                ' or key=''source'')'
               ) ;
    logEnd(0) ;
    busyShakeEx ;
  end ;

  procedure T_OsmProvider.updateWayGeometry(
    const _wayId  : Int64 ;
    const _closed : Integer
   ) ;
  var
    store : OleVariant ;
    x, y  : Double ;
    shp   : TGIS_Shape ;
  begin
    Db.sqlTableOpenWrite(
      0,
      'UPDATE way SET geom=:geom, xmin=:xmin, xmax=:xmax, ymin=:ymin, ymax=:ymax'+
      ' WHERE id=:id'
    ) ;
    Db.sqlQueryOpen( 'SELECT n.lon, n.lat, n.id FROM way_member wm, node n ' +
                     'WHERE wm.way_id=' + IntToStr(_wayId) + ' AND wm.node_id=n.id ' +
                     'ORDER BY wm.pos_id ASC', 1
                    ) ;
    if _closed = 0 then
      shp := TGIS_ShapeArc.Create
    else
      shp := TGIS_ShapePolygon.Create ;
    try
      shp.Lock( TGIS_Lock.Internal ) ;
      shp.AddPart ;
      while not Db.sqlQueryEof(1) do begin
        x := VarToDouble( Db.sqlQueryGetFieldById(0,1) ) ;
        y := VarToDouble( Db.sqlQueryGetFieldById(1,1) ) ;
        shp.AddPoint( GisPoint( x, y ) ) ;

        Db.sqlQueryMoveNext(1) ;
      end ;
      Db.sqlQueryClose(1) ;
      shp.Unlock ;
      shp.ExportToVAR( store ) ;

      Db.sqlTableSetField( 0, 'id', _wayId, -1 ) ;
      Db.sqlTableSetField( 0, 'xmin', shp.Extent.XMin, -1 ) ;
      Db.sqlTableSetField( 0, 'xmax', shp.Extent.XMax, -1 ) ;
      Db.sqlTableSetField( 0, 'ymin', shp.Extent.YMin, -1 ) ;
      Db.sqlTableSetField( 0, 'ymax', shp.Extent.YMax, -1 ) ;
      Db.sqlTableSetGeometry( 0, 'geom', store, nil ) ;
    finally
      FreeObject( shp ) ;
    end ;

    Db.sqlTablePost(0) ;
  end ;

  procedure T_OsmProvider.writeConfig(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    if assigned( _layer.ConfigFile ) then begin
      _layer.ParamsList.SaveToConfig( _layer.ConfigFile ) ;
      TGIS_Config( _layer.ConfigFile ).Save ;
    end
    else
      _layer.WriteConfig ;
  end ;

  procedure T_OsmProvider.readAttributes(
    const _shp : TGIS_Shape ;
    const _id  : Int64
  ) ;
  var
    key   : String ;
    val   : String ;
    cval  : String ;
    nval  : String ;
    res   : Integer ;
  begin
    _shp.SetField( GIS_OSM_ID, _id ) ;

    Db.sqlQueryOpen( 'SELECT key, val FROM tag WHERE object_id=' +
                     IntToStr(_id), 1
                    ) ;
    try
      cval := VarToString( _shp.GetField( OSM_TAGS ) );
      while not Db.sqlQueryEof(1) do begin
        key := Trim( VarToString( Db.sqlQueryGetFieldById(0,1) ) ) ;
        val := Trim( VarToString( Db.sqlQueryGetFieldById(1,1) ) ) ;

        if not IsStringEmpty( val ) then begin
          if not attrHelper.attrNameIgnored( key ) then begin
            key := attrHelper.attrNameNormalized( key ) ;

            res := _shp.Layer.FindField( key ) ;
            if res = -1 then begin
              nval := key + '=' + val + ';' ;
              if Pos( nval, cval ) < StringFirst then
                _shp.SetField( OSM_TAGS, cval + nval ) ;
            _shp.Layer.FieldInfo(_shp.Layer.Fields.Count-1).NewWidth := OSM_TAGS_MAX_SIZE ;
            end
            else begin
              _shp.SetField( key, val ) ;
            if _shp.Layer.FieldInfo(res).NewWidth<>_shp.Layer.FieldInfo(res).Width then
              _shp.Layer.FieldInfo(res).NewWidth := _shp.Layer.FieldInfo(res).Width ;
            end;
          end ;
        end ;
        Db.sqlQueryMoveNext(1) ;
      end ;
    finally
      Db.sqlQueryClose(1) ;
    end ;

  end ;

  procedure T_OsmProvider.buildPoints(
    const _layer  : TGIS_LayerVector
  ) ;
  var
    dwh  : TGIS_LayerVectorDirectWriteHelper ;
    shp  : TGIS_Shape ;
    id   : Int64 ;
    elmno : Int64 ;
    elmmax: Int64 ;
  begin
    log( 'Build points layer' ) ;

    dwh := TGIS_LayerVectorDirectWriteHelper.Create( _layer ) ;
    try
      T_OsmStyler.SetPointLayer( _layer ) ;

      dwh.Build( _layer.Path, _layer.Extent,
                 TGIS_ShapeType.Point, TGIS_DimensionType.XY
               ) ;

      elmmax := getElementsCount('SELECT count(id) FROM node where usage=0 or poi=1') ;

      Db.cursorOpen(1) ;
      Db.sqlQueryOpen( 'SELECT id, lon, lat FROM node where usage=0 or poi=1', 0 ) ;
      try
        elmno := 0 ;
        while not Db.sqlQueryEof(0) do begin
          shp := TGIS_ShapePoint.Create ;
          try
            shp.Layer := _layer ;
            shp.Reset ;
            shp.Lock( TGIS_Lock.Internal ) ;
            shp.AddPart ;
            shp.AddPoint( GisPoint( VarToDouble( Db.sqlQueryGetFieldById(1,0) ),
                                    VarToDouble( Db.sqlQueryGetFieldById(2,0) )
                                   )
                         ) ;
            shp.Unlock ;

            id := VarToInt64( Db.sqlQueryGetFieldById(0,0) ) ;
            readAttributes( shp, id ) ;

            dwh.AddShape( shp ) ;
            Db.sqlQueryMoveNext(0) ;
          finally
            FreeObject( shp ) ;
          end ;

          inc( elmno ) ;
          if elmno mod GIS_PROGRESS_TRESHOLD = 0 then
            busyShake( elmno, elmmax ) ;
        end ;
      finally
        Db.sqlQueryClose(0) ;
        Db.cursorClose(1) ;
      end;
    finally
      dwh.Close ;
      FreeObject( dwh ) ;
    end ;
    writeConfig( _layer ) ;
  end ;

  procedure T_OsmProvider.buildLines(
    const _layer  : TGIS_LayerVector
  ) ;
  var
    dwh  : TGIS_LayerVectorDirectWriteHelper ;
    shp  : TGIS_Shape ;
    id   : Int64 ;
    buf  : OleVariant ;
    elmno : Int64 ;
    elmmax: Int64 ;
  begin
    log( 'Build lines layer' ) ;
    dwh := TGIS_LayerVectorDirectWriteHelper.Create( _layer ) ;
    try
      T_OsmStyler.SetLineLayer( _layer ) ;

      dwh.Build( _layer.Path, _layer.Extent,
                 TGIS_ShapeType.Arc, TGIS_DimensionType.XY
               ) ;
      elmmax := getElementsCount('SELECT count(id) FROM way where closed=0 and usage=0') ;

      // single lines
      Db.cursorOpen(1) ;
      Db.sqlQueryOpen( 'SELECT id, geom FROM way where closed=0 and usage=0', 0 ) ;
      try
        elmno := 0 ;
        while not Db.sqlQueryEof(0) do begin
          buf := Db.sqlQueryGetGeomVAR('geom','',0 ) ;
          if not (VarIsEmpty( buf ) or VarIsNull( buf )) then begin
            shp := TGIS_ShapeArc.Create ;
            try
              shp.Layer := _layer ;
              shp.Reset ;
              shp.Lock( TGIS_Lock.Internal ) ;
              shp.AddPart ;
              shp.ImportFromVAR( buf ) ;
              shp.Unlock ;

              id := VarToInt64( Db.sqlQueryGetFieldById(0,0) ) ;
              readAttributes( shp, id ) ;

              dwh.AddShape( shp ) ;
            finally
              FreeObject( shp ) ;
            end ;
          end ;
          Db.sqlQueryMoveNext(0) ;

          inc( elmno ) ;
          if elmno mod GIS_PROGRESS_TRESHOLD = 0 then
            busyShake( elmno, elmmax ) ;
        end ;
      finally
        Db.sqlQueryClose(0) ;
        Db.cursorClose(1) ;
      end ;
      buildRelations( dwh, False ) ;
    finally
      dwh.Close ;
      FreeObject( dwh ) ;
    end ;
    writeConfig( _layer ) ;
  end ;

  procedure T_OsmProvider.buildPolygons(
    const _layer  : TGIS_LayerVector
  ) ;
  var
    dwh  : TGIS_LayerVectorDirectWriteHelper ;
    shp  : TGIS_Shape ;
    id   : Int64 ;
    buf  : OleVariant ;
    elmno : Int64 ;
    elmmax: Int64 ;
  begin
    log( 'Build polygons layer' ) ;
    dwh := TGIS_LayerVectorDirectWriteHelper.Create( _layer ) ;
    try
      T_OsmStyler.SetPolygonLayer( _layer ) ;

      dwh.Build( _layer.Path, _layer.Extent,
                 TGIS_ShapeType.Polygon, TGIS_DimensionType.XY
               ) ;
      elmmax := getElementsCount('SELECT count(id) FROM way where closed=1 and usage=0') ;

      Db.cursorOpen(1) ;
      Db.sqlQueryOpen( 'SELECT id, geom FROM way where closed=1 and usage=0', 0 ) ;
      try
        elmno := 0 ;
        while not Db.sqlQueryEof(0) do begin
          buf := Db.sqlQueryGetGeomVAR('geom','',0 ) ;
          if not (VarIsEmpty( buf ) or VarIsNull( buf )) then begin
            shp := TGIS_ShapePolygon.Create ;
            try
              shp.Layer := _layer ;
              shp.Reset ;
              shp.Lock( TGIS_Lock.Internal ) ;
              shp.ImportFromVAR( buf ) ;
              shp.Unlock ;

              id := VarToInt64( Db.sqlQueryGetFieldById(0,0) ) ;
              readAttributes( shp, id ) ;

              dwh.AddShape( shp ) ;
            finally
              FreeObject( shp ) ;
            end ;
          end ;
          Db.sqlQueryMoveNext(0) ;

          inc( elmno ) ;
          if elmno mod GIS_PROGRESS_TRESHOLD = 0 then
            busyShake( elmno, elmmax ) ;
        end ;
      finally
        Db.sqlQueryClose(0) ;
        Db.cursorClose(1) ;
      end ;
      buildRelations( dwh, True ) ;
    finally
      dwh.Close ;
      FreeObject( dwh ) ;
    end ;
    writeConfig( _layer ) ;
  end ;

  procedure T_OsmProvider.buildRelations(
    const _dwh   : TGIS_LayerVectorDirectWriteHelper ;
    const _poly  : Boolean
   ) ;
  var
    shp  : TGIS_Shape ;
    lshp : TGIS_Shape ;
    pshp : TGIS_Shape ;
    id   : Int64 ;
    idw  : Int64 ;
    buf  : OleVariant ;
    shplist : TGIS_ObjectList ;
    i,j,k : Integer ;
    elmno : Int64 ;
    elmmax: Integer ;
  begin
    if _poly then
      log( 'Build relations of polygon layer' )
    else
      log( 'Build relations of line layer' ) ;
    // relations
    Db.cursorOpen(1) ;
    if _poly then begin
      elmmax := getElementsCount('SELECT count(id) from relation where type='''+OSM_VALUE_MPOLYGON+'''') ;
      Db.sqlQueryOpen( 'select id from relation where type='''+OSM_VALUE_MPOLYGON+'''', 0 )
    end
    else begin
      elmmax := getElementsCount('SELECT count(id) from relation where type<>'''+OSM_VALUE_MPOLYGON+'''') ;
      Db.sqlQueryOpen( 'select id from relation where type<>'''+OSM_VALUE_MPOLYGON+'''', 0 ) ;
    end;

    elmno := 0 ;
    while not Db.sqlQueryEof(0) do begin
      id := VarToInt64( Db.sqlQueryGetFieldById(0,0) ) ;

      if _poly then
        shp := TGIS_ShapePolygon.Create
      else
        shp := TGIS_ShapeArc.Create ;
      try
        shp.Layer := _dwh.Layer ;
        shp.Lock( TGIS_Lock.Internal ) ;
        shp.Reset ;
        Db.sqlQueryOpen( 'select w.geom, w.id from relation r, relation_member rm,'+
                         ' way w where r.id=' + IntToStr(id) + ' and r.id='+
                         'rm.relation_id and rm.member_id='+
                         'w.id and rm.member_type=1 order by rm.pos_id ', 1
                        ) ;
        shplist := TGIS_ObjectList.Create ;
        try
          while not Db.sqlQueryEof(1) do begin
            buf := Db.sqlQueryGetGeomVAR('geom','',1 ) ;
            idw := VarToInt64( Db.sqlQueryGetField('id',1 ) ) ;

            lshp := TGIS_ShapeArc.Create( nil, nil, False, idw, nil, TGIS_DimensionType.XY ) ;
            lshp.ImportFromVAR( buf ) ;
            if not lshp.IsEmpty then
              shplist.Add( lshp ) ;

            Db.sqlQueryMoveNext(1) ;
          end;
          Db.sqlQueryClose(1) ;

          for i := 0 to shplist.Count - 1 do begin
            pshp := TGIS_Shape( shplist[i] ) ;
            pshp.Lock( TGIS_Lock.Projection ) ;
            if not pshp.IsEmpty then begin
              for j := 0 to pshp.GetNumParts - 1 do begin
                shp.AddPart ;
                for k := 0 to pshp.GetPartSize( j ) - 1 do
                  shp.AddPoint3D(  pshp.GetPoint3D( j, k) ) ;
              end ;
              readAttributes( shp, pshp.Uid ) ;
            end ;
            pshp.Unlock ;
          end ;
        finally
          FreeObject( shplist ) ;
        end ;

        shp.Unlock ;
        readAttributes( shp, id ) ;

        if not shp.IsEmpty then
          _dwh.AddShape( shp ) ;
      finally
        FreeObject( shp ) ;
      end ;

      inc( elmno ) ;
      if elmno mod 10 = 0 then
        busyShake( elmno, elmmax ) ;

      Db.sqlQueryMoveNext(0) ;
    end ;
    Db.sqlQueryClose(0) ;
    Db.cursorClose(1) ;
  end ;

  procedure T_OsmProvider.busyShake(
    const _pos : Integer ;
    const _end : Integer
  ) ;
  var
    abrt : Boolean ;
  begin
    lastPos := _pos ;
    maxPos  := _end ;

    if assigned( vwr ) then
      vwr.BusyShake( nil, _pos, _end, abrt ) ;
  end ;

  procedure T_OsmProvider.busyShakeEx ;
  var
    abrt : Boolean ;
  begin
    if assigned( vwr ) then
      vwr.BusyShake( nil, lastPos, maxPos, abrt ) ;
  end ;


  function T_OsmProvider.getElementsCount(
    const _sql : String
  ) : Int64 ;
  begin
    Db.sqlQueryOpen( _sql, 0 ) ;
    try
      Result := VarToInt64( Db.sqlQueryGetFieldById(0,0) ) ;
    finally
      Db.sqlQueryClose(0) ;
    end ;
  end ;

  procedure T_OsmProvider.logBegin(
    const _txt : String
  ) ;
  begin
    logText := _txt ;

    if assigned( logEvent ) then
      logEvent( logText ) ;
    startStopwatch ;
  end ;

  procedure T_OsmProvider.logEnd(
    const _count : Int64
  ) ;
  begin
    if _count > 0 then
      logText :=  Format(' %d done in %s', [_count, getElapsedTime ] )
    else
      logText :=  ' done in ' + getElapsedTime ;

    if assigned( logEvent ) then
      logEvent( logText ) ;
  end ;

  procedure T_OsmProvider.log(
    const _txt : String
  ) ;
  begin
    if assigned( logEvent ) then
      logEvent( _txt ) ;
  end ;

  procedure T_OsmProvider.startStopwatch;
  begin
    elapsed := GetTickCount ;
  end ;

  function T_OsmProvider.getElapsedTime : String ;
  begin
    Result := FloatToStr((GetTickCount - elapsed) / 1000 ) + ' s' ;
  end ;

  function T_OsmProvider.OpenDatabase : Boolean ;
  begin
    Result := False ;

    if not SafeFileExists( DbPath ) then exit ;

    createDatabase ;
    Result := True ;
  end ;

  function T_OsmProvider.CloseDatabase : Boolean ;
  begin
    Db.sqlDisconnect ;
    Result := True ;
  end ;

  function T_OsmProvider.IsDatabaseValid : Boolean ;
  begin
    Result := True ;
  end ;

  function T_OsmProvider.LoadOSMFile : Boolean ;
  const
    MAX_STAGES = 14 ;
  begin
    if assigned( vwr ) then
      vwr.BusyPrepare( nil, Format( _rsrc( GIS_RS_BUSY_DATA_LOAD ), [FilePath] ) ) ;
    try
      CloseDatabase ;
      createDatabase ;
        busyShake( lastPos, MAX_STAGES ) ;
      createSchema ;
        busyShake( lastPos, MAX_STAGES ) ;
      parseFile ;
        busyShake( lastPos, MAX_STAGES ) ;
      unprepareStatements ;
        busyShake( lastPos, MAX_STAGES ) ;
      createIndexes1 ;
        busyShake( lastPos, MAX_STAGES ) ;
      postParsing1 ;
        busyShake( lastPos, MAX_STAGES ) ;
      createIndexes2 ;
        busyShake( lastPos, MAX_STAGES ) ;
      postParsing2 ;
        busyShake( lastPos, MAX_STAGES ) ;
      createIndexes3 ;
        busyShake( lastPos, MAX_STAGES ) ;
      postParsing3 ;
        busyShake( lastPos, MAX_STAGES ) ;

      log( 'Finish' ) ;
      busyShake( MAX_STAGES, MAX_STAGES ) ;

      Result := True ;
    finally
      if assigned( vwr ) then
        vwr.BusyRelease( nil ) ;
    end ;
  end ;


//==============================================================================
// TGIS_OSMImporter implementation
//==============================================================================

  constructor TGIS_OSMImporter.Create(
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    vwrObj := _viewer ;
  end ;

  procedure TGIS_OSMImporter.doDestroy ;
  begin
    FreeObject( osmp ) ;

    inherited ;
  end ;

  procedure TGIS_OSMImporter.ImportOSMFile(
    const _file : String
  ) ;
  begin
    osmp := T_OsmProvider.Create( _file, vwrObj, FOnImportLog ) ;
    if T_OsmProvider(osmp).OpenDatabase    and
       T_OsmProvider(osmp).IsDatabaseValid then
      // nothing to do
    else
      T_OsmProvider(osmp).LoadOSMFile ;
  end ;

  procedure TGIS_OSMImporter.ExportOsmPoints(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    if assigned( T_OsmProvider(osmp).vwr ) then
      T_OsmProvider(osmp).vwr.BusyPrepare(
        _layer,
        Format( _rsrc( GIS_RS_BUSY_EXPORT ), [_layer.Name] )
      ) ;
    try
    T_OsmProvider(osmp).buildPoints( _layer ) ;
    finally
      if assigned( T_OsmProvider(osmp).vwr ) then
        T_OsmProvider(osmp).vwr.BusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_OSMImporter.ExportOsmLines(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    if assigned( T_OsmProvider(osmp).vwr ) then
      T_OsmProvider(osmp).vwr.BusyPrepare(
        _layer,
        Format( _rsrc( GIS_RS_BUSY_EXPORT ), [_layer.Name] )
      ) ;
    try
    T_OsmProvider(osmp).buildLines( _layer ) ;
    finally
      if assigned( T_OsmProvider(osmp).vwr ) then
        T_OsmProvider(osmp).vwr.BusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_OSMImporter.ExportOsmPolygons(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    if assigned( T_OsmProvider(osmp).vwr ) then
      T_OsmProvider(osmp).vwr.BusyPrepare(
        _layer,
        Format( _rsrc( GIS_RS_BUSY_EXPORT ), [_layer.Name] )
      ) ;
    try
    T_OsmProvider(osmp).buildPolygons( _layer ) ;
    finally
      if assigned( T_OsmProvider(osmp).vwr ) then
        T_OsmProvider(osmp).vwr.BusyRelease( _layer ) ;
    end ;
  end ;
  {$ENDIF}

//==============================================================================
// T_OsmStyler implementation
//==============================================================================

  class procedure T_OsmStyler.SetPointLayer(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    _layer.SetCSByEPSG( GIS_EPSG_WGS84 ) ;
    _layer.AddField( GIS_OSM_ID,  TGIS_FieldType.Number, 18 , 0 ) ;
    _layer.AddField( OSM_NAME,    TGIS_FieldType.String, 100 , 0 ) ;
    _layer.AddField( OSM_AMENITY, TGIS_FieldType.String, 100 , 0 ) ;
    _layer.AddField( 'barrier',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'highway',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'tourism',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'shop',      TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'historic',  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'addr_housenumber',  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'addr_street',    TGIS_FieldType.String, 25 , 0 ) ;

    _layer.AddField( OSM_TAGS, TGIS_FieldType.String, OSM_TAGS_MAX_SIZE , 0 ) ;

    _layer.Params.Labels.Value := '{name}{addr_housenumber}' ;
    _layer.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
    _layer.Params.Marker.Size  := 80 ;
    _layer.Params.Labels.Color := TGIS_Color.FromARGB( $FFFFFFFF ) ;
    _layer.DefaultShapeType    := TGIS_ShapeType.Point ;
  end ;

  class procedure T_OsmStyler.SetLineLayer(
    const _layer : TGIS_LayerVector
  ) ;

    procedure addLineStyle(
      const _query        : String ;
      const _linecolor    : TGIS_Color ;
      const _linewidth    : Integer ;
      const _linestyle    : TGIS_PenStyle ;
      const _outlineColor : TGIS_Color ;
      const _outlineWidth : Integer ;
      const _outlineStyle : TGIS_PenStyle
    ) ;
    begin
      _layer.ParamsList.Add ;
      _layer.Params.Query             := _query ;
      _layer.Params.Line.Color        := _linecolor ;
      _layer.Params.Line.Width        :=
        RoundS( _linewidth * 1440 / 2.54 * 100 + GIS_AUTOSIZE_SIZE )  ;
      _layer.Params.Line.Style        := _linestyle ;
      _layer.Params.Line.OutlineColor := _outlineColor ;
      _layer.Params.Line.OutlineWidth := -_outlineWidth ;
      _layer.Params.Line.OutlineStyle := _outlineStyle ;
      _layer.Params.Line.ShowLegend   := True ;
      _layer.Params.Legend            := _query ;
    end ;

  begin
    _layer.SetCSByEPSG( GIS_EPSG_WGS84 ) ;
    _layer.AddField( GIS_OSM_ID,  TGIS_FieldType.Number,  18 , 0 ) ;
    _layer.AddField( OSM_NAME,    TGIS_FieldType.String, 100 , 0 ) ;
    _layer.AddField( OSM_AMENITY, TGIS_FieldType.String, 100 , 0 ) ;
    _layer.AddField( OSM_HIGHWAY, TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_WATERWAY,TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_RAILWAY, TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'boundary',  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'natural',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'barrier',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'junction',  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'surface',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'service',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'aeroway',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'bridge',    TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'oneway',    TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_TAGS, TGIS_FieldType.String, OSM_TAGS_MAX_SIZE , 0 ) ;

    _layer.Params.Labels.Field := OSM_NAME ;
    _layer.Params.Labels.Alignment := TGIS_LabelAlignment.Follow ;
    _layer.Params.Labels.Position := GisGetLabelPosition(TGIS_LabelPosition.MiddleCenter) ;
    _layer.Params.Labels.Color := TGIS_Color.FromARGB( $FFFFFFFF ) ;
    _layer.DefaultShapeType    := TGIS_ShapeType.Arc ;
    _layer.MultipassRendering  := True ;

    addLineStyle( 'highway=''private''',
                  TGIS_Color.FromABGR( $FFD4D4F7 ),  4, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''motorway''',
                  TGIS_Color.FromABGR( $FFBD9B84 ),  4, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''motorway_link''',
                  TGIS_Color.FromABGR( $FFBD9B84 ),  4, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''primary''',
                  TGIS_Color.FromABGR( $FFA3A2EC ), 11, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''secondary''',
                  TGIS_Color.FromABGR( $FFA4D6FD ), 10, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''tertiary''',
                  TGIS_Color.FromABGR( $FFB2FEFE ), 10, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''trunk_link''',
                  TGIS_Color.FromABGR( $FFB2FEFE ), 10, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''primary_link''',
                  TGIS_Color.FromABGR( $FFA3A2EC ), 10, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''living_street''',
                  TGIS_Color.FromABGR( $FFA3A2EC ),  5, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''road''',
                  TGIS_Color.FromABGR( $FFB2FEFE ), 10, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''trunk''',
                  TGIS_Color.FromABGR( $FFB2FEFE ), 10, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''service''',
                  TGIS_Color.FromABGR( $FFFFFFFF ),  3, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''pedestrian''',
                  TGIS_Color.FromABGR( $FFEDEDED ),  7, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''residential''',
                  TGIS_Color.FromABGR( $FFFFFFFF ),  6, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''unclassified''',
                  TGIS_Color.FromABGR( $FFFFFFFF ),  6, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''track''',
                  TGIS_Color.FromABGR( $FF17759D ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''footway''',
                  TGIS_Color.FromABGR( $FF7484F6 ),  2, TGIS_PenStyle.Dot,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''path''',
                  TGIS_Color.FromABGR( $FF6D7C6E ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''steps''',
                  TGIS_Color.FromABGR( $FF7280F9 ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''cycleway''',
                  TGIS_Color.FromABGR( $FFFF0000 ),  2, TGIS_PenStyle.Dot,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'highway=''bridleway''',
                  TGIS_Color.FromABGR( $FFFF0000 ),  2, TGIS_PenStyle.Dot,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'waterway=''river''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ),  7, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'waterway=''stream''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ),  7, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'waterway=''canal''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ),  7, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'waterway=''drain''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ),  7, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'waterway=''ditch''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ),  7, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'railway=''rail''',
                  TGIS_Color.FromABGR( $FFFFFFFF ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FF808080 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'railway=''tram''',
                  TGIS_Color.FromABGR( $FFFFFFFF ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FF808080 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'railway=''razed''',
                  TGIS_Color.FromABGR( $FFFFFFFF ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FF808080 ),  1, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'natural=''coastline''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ),  2, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'boundary=''administrative''',
                  TGIS_Color.FromABGR( $FF991A9E ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'barrier=''fence''',
                  TGIS_Color.FromABGR( $FF6D7C6E ),  2, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'tags like ''%addr:%''',
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  1, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'tags like ''%power=%''',
                  TGIS_Color.FromABGR( $FF0000FF ),  1, TGIS_PenStyle.Dash,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
    addLineStyle( 'tags like ''%aeroway=%''',
                  TGIS_Color.FromABGR( $FFFED1E9 ),  1, TGIS_PenStyle.Solid,
                  TGIS_Color.FromABGR( $FFE0E0E0 ),  0, TGIS_PenStyle.Solid
                ) ;
  end ;

  class procedure T_OsmStyler.SetPolygonLayer(
    const _layer : TGIS_LayerVector
  ) ;

    procedure addAreaStyle(
      const _query      : String ;
      const _color      : TGIS_Color ;
      const _brushStyle : TGIS_BrushStyle
    ) ;
    begin
      _layer.ParamsList.Add ;
      _layer.Params.Query        := _query ;
      _layer.Params.Area.Color   := _color ;
      _layer.Params.Area.Pattern := _brushStyle ;
      if _brushStyle = TGIS_BrushStyle.Clear then
        _layer.Params.Area.OutlineColor := _color ;

      _layer.Params.Area.ShowLegend := True ;
      _layer.Params.Legend          := _query ;
    end;

  begin
    _layer.SetCSByEPSG( GIS_EPSG_WGS84 ) ;
    _layer.AddField( GIS_OSM_ID,   TGIS_FieldType.Number,  18 , 0 ) ;
    _layer.AddField( OSM_NAME,     TGIS_FieldType.String, 100 , 0 ) ;
    _layer.AddField( OSM_AMENITY,  TGIS_FieldType.String, 100 , 0 ) ;
    _layer.AddField( OSM_LANDUSE,  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_LEISURE,  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_NATURAL,  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_WATERWAY, TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'building',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'boundary',   TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_HIGHWAY,  TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'railway',    TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'area',       TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'type',       TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'service',    TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'aeroway',    TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'parking',    TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( 'sport',      TGIS_FieldType.String, 25 , 0 ) ;
    _layer.AddField( OSM_TAGS, TGIS_FieldType.String, OSM_TAGS_MAX_SIZE , 0 ) ;
    _layer.DefaultShapeType    := TGIS_ShapeType.Polygon ;
    _layer.Params.Area.Pattern := TGIS_BrushStyle.Clear ;

    addAreaStyle( 'landuse=''forest''',
                  TGIS_Color.FromABGR( $FF6CC58D ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''beach''',
                   TGIS_Color.FromABGR( $FFC0FEFE ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''scrub''',
                  TGIS_Color.FromABGR( $FFAED289 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''industrial''',
                  TGIS_Color.FromABGR( $FFD6D1DF ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''pitch''',
                  TGIS_Color.FromABGR( $FFAED289 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''grass''',
                  TGIS_Color.FromABGR( $FFA8ECCF ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''cemetery''',
                  TGIS_Color.FromABGR( $FFAECAA9 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''allotments''',
                  TGIS_Color.FromABGR( $FF84B0C8 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''railway''',
                  TGIS_Color.FromABGR( $FFD6D1DF ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse=''residential''',
                  TGIS_Color.FromABGR( $FFF9F9F9 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'landuse like ''farm%''',
                  TGIS_Color.FromABGR( $FFBDD8E9 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'building=''yes''',
                  TGIS_Color.FromABGR( $FFA9A9BC ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'building IS NOT NULL',
                  TGIS_Color.FromABGR( $FFD1D1E5 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'leisure=''park''',
                  TGIS_Color.FromABGR( $FFB0F6C0 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'leisure=''pitch''',
                  TGIS_Color.FromABGR( $FFAED289 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'natural=''water''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'natural=''wood''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'amenity=''cemetery''',
                  TGIS_Color.FromABGR( $FFAECAA9 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'amenity=''grave_yard''',
                  TGIS_Color.FromABGR( $FFAECAA9 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'amenity=''parking''',
                  TGIS_Color.FromABGR( $FFB7EEF6 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'amenity=''school''',
                  TGIS_Color.FromABGR( $FFA9A9BC ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'boundary=''administrative''',
                  TGIS_Color.FromABGR( $FF991A9E ), TGIS_BrushStyle.Clear
                ) ;
    addAreaStyle( 'boundary=''national_park''',
                  TGIS_Color.FromABGR( $FF6CC58D ), TGIS_BrushStyle.Clear
                ) ;
    addAreaStyle( 'waterway=''riverbank''',
                  TGIS_Color.FromABGR( $FFD0D0B5 ), TGIS_BrushStyle.Solid
                ) ;
    addAreaStyle( 'highway=''pedestrian''',
                  TGIS_Color.FromABGR( $FFEDEDED ), TGIS_BrushStyle.Solid
                ) ;
  end ;

//==============================================================================
// TGIS_LayerOSM implementation
//==============================================================================

  constructor TGIS_LayerOSM.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;
    currShp          := nil ;
    currType         := -1 ;
    currRef          := -1 ;
    oAttributes      := TList<TGIS_AttributeOSM>.Create ;
    elementCount     := 0 ;
    elementNo        := 0 ;
    FAdditionalNodes := True ;
    addingObject     := False ;
    attrHelper       := T_OsmAttributes.Create ;

    FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
  end ;

  procedure TGIS_LayerOSM.doDestroy ;
  begin
    FreeObject( oAttributes ) ;
    FreeObject( attrHelper  ) ;

    inherited ;
  end ;

  function TGIS_LayerOSM.attrNameIndex(
    const _name  : String
  ) : Integer ;
  var
    fnd : Boolean ;
    i   : Integer ;
  begin
    fnd := False ;
    for i := 0 to oAttributes.Count - 1 do
      if UpperCase( oAttributes[i].Name ) = UpperCase( _name ) then begin
        fnd := True ;
        break ;
      end ;

    if fnd then
      Result := i
    else
      Result := -1 ;
  end ;


  function TGIS_LayerOSM.attrPairIndex(
    const _name  : String ;
    const _value : String
  ) : Integer ;
  var
    fnd : Boolean ;
    i   : Integer ;
  begin
    fnd := False ;
    for i := 0 to oAttributes.Count - 1 do
      if ( UpperCase( oAttributes[i].Name  ) = UpperCase( _name  ) ) and
         ( UpperCase( oAttributes[i].Value ) = UpperCase( _value ) ) then begin
        fnd := True ;
        break ;
      end ;

    if fnd then
      Result := i
    else
      Result := -1 ;
  end ;

  procedure TGIS_LayerOSM.addPoint(
    const _id : Int64
  ) ;
  begin
    currShp :=
      TGIS_LayerVector( Layer[2] ).CreateShape( TGIS_ShapeType.Point ) ;
    currShp.Lock( TGIS_Lock.Projection ) ;
    currShp.Reset ;
    currShp.AddPart;
    currShp.SetField( GIS_OSM_ID, _id ) ;
  end ;

  procedure TGIS_LayerOSM.addLine( const _id : Int64 );
  begin
    currShp :=
      TGIS_LayerVector( Layer[1] ).CreateShape( TGIS_ShapeType.Arc ) ;
    currShp.Lock( TGIS_Lock.Projection ) ;
    currShp.Reset ;
    currShp.AddPart ;
    currShp.SetField( GIS_OSM_ID, _id ) ;
  end ;

  procedure TGIS_LayerOSM.addPolygon(
    const _id : Int64
  ) ;
  begin
    currShp :=
      TGIS_LayerVector( Layer[0] ).CreateShape( TGIS_ShapeType.Polygon ) ;
    currShp.Lock( TGIS_Lock.Projection ) ;
    currShp.Reset ;
    currShp.AddPart;
    currShp.SetField( GIS_OSM_ID, _id ) ;
  end ;

  procedure TGIS_LayerOSM.addAtrToShp(
    const _atrname  : String ;
    const _atrvalue : String ;
    const _shp      : TGIS_Shape
  ) ;
  var
    res : Integer ;
    vle : String  ;
    atr : String  ;
  begin
    if T_OsmAttributes(attrHelper).attrNameIgnored( _atrname ) then exit ;

    if assigned( _shp ) then begin
      atr := T_OsmAttributes(attrHelper).attrNameNormalized( _atrname ) ;
      vle := _atrvalue ;

      if not IsStringEmpty( vle ) then begin
        res := _shp.Layer.FindField( atr ) ;
        if (res = -1) or (res=0) then
          _shp.SetField( OSM_TAGS,
                         VarToString( _shp.GetField( OSM_TAGS ) ) +
                         atr + '=' + vle + ';'
                        )
        else
          _shp.SetField( atr, vle ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerOSM.addAtrToArray(
    const _atrname  : String ;
    const _atrvalue : String
  ) ;
  var
    attr : TGIS_AttributeOSM ;
  begin
    if not IsStringEmpty( _atrname ) then
      if not T_OsmAttributes(attrHelper).attrNameIgnored( _atrname ) then begin
        attr.Name  := _atrname  ;
        attr.Value := _atrvalue ;
        oAttributes.Add( attr ) ;
      end ;
  end ;

  procedure TGIS_LayerOSM.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    // only default CS can be used
  end ;

  function ShapesAreasCompare( const _s1, _s2 : TGIS_Shape ) : Integer ;
  begin
    if   _s1.Area < _s2.Area then Result := 1
    else begin
      if _s1.Area > _s2.Area then Result := -1
      else                        Result := 0 ;
    end ;
  end ;

  procedure TGIS_LayerOSM.setUp ;
  var
    lv  : TGIS_LayerVector ;
    i   : Integer ;
  begin
    inherited;

    nodeList := TGIS_TemporaryFileStream.Create( Path ) ;
    wayList  := TGIS_TemporaryFileStream.Create( Path ) ;
    ptgList  := TGIS_PointList.Create ;
    members  := TStringList.Create ;

    lv := TGIS_LayerVector.Create ;
    lv.Name := 'Polygons' ;
    lv.CS := CS ;
    Add( lv ) ;
    {$IFDEF JAVA}
      lv.UseRTree := False ;
      {$WARNING '### remove when Rtree build in memory is fixed'}
    {$ENDIF}
    T_OsmStyler.SetPolygonLayer( lv ) ;
    lv.Collapsed := True ;
    lv.SupportedShapes := GisGetShapeType( TGIS_ShapeType.Polygon ) ;

    lv := TGIS_LayerVector.Create ;
    lv.Name := 'Lines' ;
    lv.CS := CS ;
    Add( lv ) ;
    {$IFDEF JAVA}
      lv.UseRTree := False ;
      {$WARNING '### remove when Rtree build in memory is fixed'}
    {$ENDIF}
    T_OsmStyler.SetLineLayer( lv ) ;
    lv.Collapsed := True ;
    lv.SupportedShapes := GisGetShapeType( TGIS_ShapeType.Arc ) ;

    lv := TGIS_LayerVector.Create ;
    lv.Name := 'Points' ;
    lv.CS := CS ;
    Add( lv ) ;
    {$IFDEF JAVA}
      lv.UseRTree := False ;
      {$WARNING '### remove when Rtree build in memory is fixed'}
    {$ENDIF}
    T_OsmStyler.SetPointLayer( lv ) ;
    lv.Collapsed := True ;
    lv.SupportedShapes := GisGetShapeType( TGIS_ShapeType.Point ) ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    try
      if not IsStringEmpty( Path ) then begin
        try
          if SafeFileExists( Path ) or IsServerPath( Path ) then begin
            if UpperCase( GetFileExt( Path ) ) = '.PBF' then
              parsePBF
            else
              parseXML ;
          end ;
        except
          on e : EGIS_Exception do begin
            raise ;
          end ;
          on e : Exception do begin
            raise EGIS_Exception.Create(
                    _rsrc( GIS_RS_ERR_FILEBADFORMAT ) + '; ' +
                    e.Message, Path, 0
                  ) ;
          end ;
        end ;
      end ;

      // clean up hidden shapes
      for i := TGIS_LayerVector(Layer[0]).Items.Count -1 downto 0 do
        if TGIS_Shape( TGIS_LayerVector(Layer[0]).Items[i] ).IsHidden then
          TGIS_LayerVector(Layer[0]).Items.Delete( i ) ;

      // sort polygons by area and fix uids order
      {$IFDEF OXYGENE}
        {$IFDEF JAVA}
          java.util.Collections.sort( TGIS_LayerVector(Layer[0]).Items, new T_listSortByArea ) ;
        {$ELSE}
          TGIS_LayerVector(Layer[0]).Items.Sort( @ShapesAreasCompare ) ;
        {$ENDIF}
      {$ELSE}
        TGIS_LayerVector(Layer[0]).Items.Sort( TComparer<TGIS_Shape>.Construct( ShapesAreasCompare ) ) ;
      {$ENDIF}
      TGIS_LayerVector(Layer[0]).ReorderShapeUids ;

      for i := TGIS_LayerVector(Layer[1]).Items.Count -1 downto 0 do
        if TGIS_Shape( TGIS_LayerVector(Layer[1]).Items[i] ).IsHidden then
          TGIS_LayerVector(Layer[1]).Items.Delete( i ) ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := 'OpenStreetMap Format (OSM)' ;
      // RecalcExtent below added temporarily
      // verify if necessary when GisCompoundLayer is fixed
      RecalcExtent ;

      ClearModified ;
    finally
      Unlock ;
      FreeObject( nodeList ) ;
      FreeObject( wayList  ) ;
      FreeObject( ptgList  ) ;
      FreeObject( members  ) ;

      RaiseBusyRelease( Self ) ;
    end ;
  end ;

  procedure TGIS_LayerOSM.parsePBF ;
  var
    pbf  : TGIS_PBFParser ;
    osmb : T_OsmBuilder ;
  begin
    pbf := TGIS_PBFParser.Create ;
    try
      osmb := T_OsmBuilder.Create( self ) ;
      try
        pbf.NodeEvent     := {$IFDEF OXYGENE}@{$ENDIF}osmb.doNode ;
        pbf.WayEvent      := {$IFDEF OXYGENE}@{$ENDIF}osmb.doWay ;
        pbf.RelationEvent := {$IFDEF OXYGENE}@{$ENDIF}osmb.doRelation ;
        pbf.ExtentEvent   := {$IFDEF OXYGENE}@{$ENDIF}osmb.doExtent ;

        pbf.Parse( Path ) ;
      finally
        FreeObject( osmb ) ;
      end ;
    finally
      FreeObject( pbf ) ;
    end ;
  end ;

  procedure TGIS_LayerOSM.parseXML ;
  var
    sax : T_OsmSaxMemHdl ;
  begin
    sax := T_OsmSaxMemHdl.Create( Self ) ;
    try
      sax.LoadFromFile( Path ) ;
    finally
      FreeObject( sax ) ;
    end ;
  end ;


  function TGIS_LayerOSM.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    oStm : TGIS_BufferedFileStream ;
    buf  : TBytes ;
    i    : Integer ;
  begin
    if SafeFileExists( _path ) then begin
      if UpperCase( GetFileExt( _path ) ) = '.OSM' then begin
        Result := True ;
        exit ;
      end;

      Result := False ;
      oStm := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
      try
        SetLength( buf, 1024 ) ;
        {$IFDEF OXYGENE}
         oStm.Read( buf, 1024 ) ;
        {$ELSE}
         oStm.Read( buf[ 0 ], 1024 ) ;
        {$ENDIF}
        i := 0 ;
        while i < 1023 do begin
          if ( chr( buf[ i ] )   = 'O' ) and
             ( chr( buf[ i+1 ] ) = 'S' ) and
             ( chr( buf[ i+2 ] ) = 'M' ) then begin
               Result := True ;
               break ;
             end ;
          inc( i ) ;
        end ;
      finally
        FreeObject( oStm ) ;
      end ;
    end
    else
      Result := True ;

    Result := inherited PreRecognize( _path, _new_path ) and Result ;
  end ;

//==============================================================================
//  T_OsmBuilder
//==============================================================================

  constructor T_OsmBuilder.Create(
    const _layer : TGIS_LayerOSM
  ) ;
  begin
    inherited Create ;

    layerOSM   := _layer ;
    attrHelper := T_OsmAttributes.Create ;
  end;

  procedure T_OsmBuilder.doDestroy ;
  begin
    FreeObject( attrHelper  ) ;

    inherited ;
  end ;

  procedure T_OsmBuilder.doExtent(
    const _extent : TGIS_Extent
  ) ;
  begin
    layerOSM.Extent := _extent ;
  end ;

  procedure T_OsmBuilder.doNode(
    const _node : TGIS_OSMNode
  ) ;
  var
    j   : Integer ;
    poi : Boolean ;
  begin
    inc( layerOSM.elementNo );
    if layerOSM.elementNo mod 200 = 0 then
      layerOSM.HourglassShake ;

    layerOSM.nodeList.AddFeature( _node.id, _node.ptg ) ;

    if layerOSM.AdditionalNodes then begin
      poi := False ;
      for j := 0 to _node.attrs.Count-1 do
        if not attrHelper.attrNameIgnored(_node.attrs[j].Key ) then begin
          poi := True ;
          break ;
        end ;

      if poi then begin
        layerOSM.addPoint( _node.id ) ;
        layerOSM.currShp.AddPoint( _node.ptg ) ;
        for j := 0 to _node.attrs.Count - 1 do
          layerOSM.addAtrToShp( _node.attrs[j].Key,
                                _node.attrs[j].Value,
                                layerOSM.currShp
                              ) ;
        layerOSM.currShp.Unlock ;
      end ;
    end ;

  end ;

  procedure T_OsmBuilder.doWay(
    const _way : TGIS_OSMWay
  ) ;
  var
    i     : Integer ;
    buf   : TBytes ;
  begin
    inc( layerOSM.elementNo );
    if layerOSM.elementNo mod 200 = 0 then
      layerOSM.HourglassShake ;

    layerOSM.currType := 1 ;

    if _way.refs.Count > 0 then begin
      layerOSM.firstRef := _way.refs[0] ;
      layerOSM.lastRef  := _way.refs[_way.refs.Count-1] ;
    end ;

    if layerOSM.firstRef = layerOSM.lastRef then
      layerOSM.currType := 0 ;

    if layerOSM.currType = 0 then begin
      for i := 0 to _way.attrs.Count-1 do begin
        if (_way.attrs[i].Key = 'area') and (_way.attrs[i].Value = 'yes') then
          layerOSM.currType := 0
        else begin
          if (_way.attrs[i].Key = 'area') and (_way.attrs[i].Value = 'no') then
            layerOSM.currType := 1
          else if (_way.attrs[i].Key = 'junction') and (_way.attrs[i].Value = 'roundabout') then
            layerOSM.currType := 1
          else if (_way.attrs[i].Key = 'barrier') then
            layerOSM.currType := 1
          else if (_way.attrs[i].Key = 'highway') and (_way.attrs[i].Value = 'services') then
            layerOSM.currType := 1 ;
        end;
      end ;
    end ;

    if layerOSM.currType > -1 then begin
      case layerOSM.currType of
        1 : layerOSM.addLine( _way.id ) ;
        0 : layerOSM.addPolygon( _way.id ) ;
      end ;

      SetLength( buf, sizeOf(TGIS_Uid)+1 ) ;
      {$IFDEF JAVA}
        System.arraycopy( BitConverter.GetBytes( layerOSM.currShp.Uid ), 0, buf, 0, sizeOf(TGIS_Uid) ) ;
      {$ENDIF}
      {$IFDEF DCC}
        Move( layerOSM.currShp.Uid, buf[0], sizeOf(TGIS_Uid) ) ;
      {$ENDIF}
      {$IFDEF CLR}
        System.Buffer.BlockCopy(
          BitConverter.GetBytes( layerOSM.currShp.Uid ),
          0, buf, 0, sizeOf(TGIS_Uid)
        ) ;
      {$ENDIF}
      buf[sizeOf(TGIS_Uid)] := layerOSM.currType ;
      layerOSM.wayList.AddFeature( _way.id, buf ) ;

      // add points to object
      for i := 0 to _way.refs.Count - 1 do begin
        if layerOSM.nodeList.GetFeature2D( _way.refs[i], layerOSM.currPtg ) then
          layerOSM.currShp.AddPoint( layerOSM.currPtg ) ;
      end ;

      // add attributes to object
      for i := 0 to _way.attrs.Count - 1 do
        layerOSM.addAtrToShp( _way.attrs[i].Key,
                              _way.attrs[i].Value,
                              layerOSM.currShp
                            ) ;
      layerOSM.currShp.Unlock ;
    end ;
  end ;

  procedure T_OsmBuilder.doRelation(
    const _relation : TGIS_OSMRelation
  ) ;
  var
    i     : Integer ;
    ismp  : Boolean ;
    isml  : Boolean ;
  begin
    inc( layerOSM.elementNo );
    if layerOSM.elementNo mod 200 = 0 then
      layerOSM.HourglassShake ;

    ismp := False ;
    isml := False ;
    for i := 0 to _relation.attrs.Count-1 do begin
      if ((_relation.attrs[i].Key    = OSM_TYPE          ) and
          (_relation.attrs[i].Value  = OSM_VALUE_MPOLYGON)) then
      begin
        ismp := True ;
      end
      else if ((_relation.attrs[i].Key    = OSM_TYPE       ) and
               (_relation.attrs[i].Value  = OSM_VALUE_MLINE)) then
      begin
        isml := True ;
      end
      else if ((_relation.attrs[i].Key    = OSM_TYPE       ) and
               (_relation.attrs[i].Value  = 'route'        )) then
      begin
        isml := True ;
      end
      else if ((_relation.attrs[i].Key    = OSM_TYPE       ) and
               (_relation.attrs[i].Value  = 'boundary'     )) then
      begin
        isml := True ;
      end ;
    end ;

    if ismp then
      buildMultiPolygon( _relation )
    else if isml then
      buildMultiLineString( _relation )
    else
      exit ;

    // add attributes to object
    for i := 0 to _relation.attrs.Count - 1 do
      layerOSM.addAtrToShp( _relation.attrs[i].Key,
                            _relation.attrs[i].Value,
                            layerOSM.currShp
                          ) ;
    layerOSM.currShp.Unlock ;

  end ;

  procedure T_OsmBuilder.buildMultiLineString(
    const _relation : TGIS_OSMRelation
  ) ;
  var
    i, j  : Integer ;
    shp   : TGIS_Shape ;
    mshp  : TGIS_Shape ;
    buf   : TBytes ;
    uid   : TGIS_Uid ;
    edges : TGIS_ObjectList ;
    fld   : TGIS_FieldInfo ;
    val   : Variant ;
  begin
    layerOSM.addLine( _relation.id ) ;

    edges := TGIS_ObjectList.Create( False ) ;
    try
      for i := 0 to _relation.members.Count-1 do begin
        shp := nil ;
        if layerOSM.wayList.GetFeature( _relation.members[i].id, buf ) then begin
          {$IFDEF OXYGENE}
            uid := BitConverter.ToInt64( buf, 0 ) ;
          {$ELSE}
            Move( buf[0], uid, sizeOf(TGIS_Uid) ) ;
          {$ENDIF}
          shp := TGIS_LayerVector( layerOSM.Layer[buf[sizeOf(TGIS_Uid)]] ).GetShape( uid ) ;
        end ;

        if not assigned( shp ) then continue ;

        if shp.ShapeType = TGIS_ShapeType.Arc then begin
          edges.Add( shp ) ;
          shp.IsHidden := True ;
        end ;

        for j := 0 to layerOSM.currShp.Layer.Fields.Count-1 do begin
          fld := TGIS_FieldInfo(layerOSM.currShp.Layer.Fields[j]) ;
          if fld.Name = GIS_OSM_ID then continue ;
          if shp.Layer.FindField( fld.Name ) > -1 then begin
            val := shp.GetFieldEx( fld.Name ) ;
            if VarIsEmpty( val ) or VarIsNull( val ) then
              continue
            else
              layerOSM.currShp.SetField( fld.Name, val );
          end;
        end ;
      end ;

      if edges.Count > 0 then begin
        mshp := TGIS_GeometryFactory.GisBuildShapeFromEdges(
                  edges, TGIS_ShapeType.Arc, 0, nil, nil, False, -1, nil, False
                ) ;
        if assigned( mshp ) then
          layerOSM.currShp.AppendGeometry( mshp ) ;
          FreeObject( mshp ) ;
      end ;

    finally
      FreeObject( edges ) ;
    end ;
  end ;

  procedure T_OsmBuilder.buildMultiPolygon(
    const _relation : TGIS_OSMRelation
  ) ;
  var
    i, j  : Integer ;
    shp   : TGIS_Shape ;
    mshp  : TGIS_Shape ;
    buf   : TBytes ;
    uid   : TGIS_Uid ;
    edges : TGIS_ObjectList ;
    polys : TGIS_ObjectList ;
    fld   : TGIS_FieldInfo ;
    val   : Variant ;
  begin
    layerOSM.addPolygon( _relation.id ) ;

    edges := TGIS_ObjectList.Create( False ) ;
    polys := TGIS_ObjectList.Create( False ) ;
    try
      for i := 0 to _relation.members.Count-1 do begin
        shp := nil ;
        if layerOSM.wayList.GetFeature( _relation.members[i].id, buf ) then begin
          {$IFDEF OXYGENE}
            uid := BitConverter.ToInt64( buf, 0 ) ;
          {$ELSE}
            Move( buf[0], uid, sizeOf(TGIS_Uid) ) ;
          {$ENDIF}
          shp := TGIS_LayerVector( layerOSM.Layer[buf[sizeOf(TGIS_Uid)]] ).GetShape( uid ) ;
        end ;

        if not assigned( shp ) then continue ;

        shp.IsHidden := True ;

        if shp.ShapeType = TGIS_ShapeType.Polygon then
          polys.Add( shp )
        else if shp.ShapeType = TGIS_ShapeType.Arc then
          edges.Add( shp )
        else
          shp.IsHidden := False ;

        for j := 0 to layerOSM.currShp.Layer.Fields.Count-1 do begin
          fld := TGIS_FieldInfo(layerOSM.currShp.Layer.Fields[j]) ;
          if fld.Name = GIS_OSM_ID then continue ;
          if shp.Layer.FindField( fld.Name ) > -1 then begin
            val := shp.GetFieldEx( fld.Name ) ;
            if VarIsEmpty( val ) or VarIsNull( val ) then
              continue
            else
              layerOSM.currShp.SetField( fld.Name, val );
          end;
        end ;
      end ;

      if polys.Count > 0 then begin
        for i := 0 to polys.Count-1 do
          layerOSM.currShp.AppendGeometry( TGIS_Shape(polys[i]) ) ;
      end ;

      if edges.Count > 0 then begin
        mshp := TGIS_GeometryFactory.GisBuildShapeFromEdges(
                  edges, TGIS_ShapeType.Polygon, 0, nil, nil, False, -1, nil, False
                ) ;
        if assigned( mshp ) then
          layerOSM.currShp.AppendGeometry( mshp ) ;
          FreeObject( mshp ) ;
      end ;

    finally
      FreeObject( edges ) ;
      FreeObject( polys ) ;
    end ;

  end ;

  { T_PbfBuilder }
  {$IFNDEF ANDROID}
  constructor T_PbfBuilder.Create(
    const _provider : T_OsmProvider
  ) ;
  begin
    inherited Create ;

    provider := _provider ;
    elementNo := 0 ;
    nodes     := 0 ;
    ways      := 0 ;
    relations := 0 ;
    doNodes   := False ;
    doWays    := False ;
    doRels    := False ;
  end ;

  procedure T_PbfBuilder.doBusy ;
  begin
    inc( elementNo ) ;
    if elementNo mod 10000 = 0 then begin
      if assigned( provider.vwr ) then
        provider.vwr.HourglassShake ;
    end ;

    if elementNo mod 1000000 = 0 then begin
      if doNodes then
        provider.log( Format( ' processed %d', [nodes] ) )
      else if doWays then
        provider.log( Format( ' processed %d', [ways] ) )
      else if doRels then
        provider.log( Format( ' processed %d', [relations] ) )
    end ;
  end;

  procedure T_PbfBuilder.doExtent(
    const _extent : TGIS_Extent
  );
  begin

  end ;

  procedure T_PbfBuilder.doNode(
    const _node : TGIS_OSMNode
  );
  var
    id  : Int64 ;
    lat : Double ;
    lon : Double ;
    i   : Integer;
    k,v : String ;
    poi : Integer ;
  begin
    id  := _node.id ;
    lat := _node.ptg.Y ;
    lon := _node.ptg.X ;

    provider.ObjectType := OSM_SQL_TYPE_NODE ;
    provider.ObjectId   := id ;

    provider.Db.sqlTableAppend(
      OSM_SQL_STMT_NODE,
      'INSERT INTO node(id, lat, lon, poi) VALUES(:id, :lat, :lon, :poi)'
     ) ;
    provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'id' , id,  -1 ) ;
    provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'lat', lat, -1 ) ;
    provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'lon', lon, -1 ) ;

    poi := 0 ;
    for i := 0 to _node.attrs.Count -1 do begin
      k := _node.attrs[i].Key ;
      v := _node.attrs[i].Value ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_TAG,
        'INSERT INTO tag ( object_id, object_type, key, val ) ' +
        'VALUES (:object_id, :object_type, :key, :val)'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_type' , provider.ObjectType, -1
      ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'key' , k, -1 ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'val' , v, -1 ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_TAG ) ;

      if (k = 'created_by') or (k = 'converted_by') or (k = 'ele') or
         (k = 'source') or (k = 'time') or (k = 'attribution') or
         (k = 'note') or (k = 'fixme') then
         poi := 0
      else
        poi := 1 ;
    end ;

    provider.Db.sqlTableSetField( OSM_SQL_STMT_NODE, 'poi', poi, -1 ) ;
    provider.Db.sqlTablePost( OSM_SQL_STMT_NODE ) ;
    inc( nodes ) ;

    if not doNodes then
      provider.logBegin( ' nodes' ) ;
    doNodes := True ;

    doBusy ;
  end ;

  procedure T_PbfBuilder.doWay(
    const _way: TGIS_OSMWay
  ) ;
  var
    id  : Int64 ;
    ref : Int64 ;
    i   : Integer ;
    k,v : String ;
  begin
    id  := _way.id ;
    provider.ObjectId   := id ;
    provider.PosId      := 1 ;
    provider.FirstId    := -1 ;
    provider.ObjectType := OSM_SQL_TYPE_WAY ;

    provider.Db.sqlTableAppend(
      OSM_SQL_STMT_WAY,
      'INSERT INTO way (id, closed) VALUES (:id, :closed)'
     ) ;
    provider.Db.sqlTableSetField( OSM_SQL_STMT_WAY, 'id' , id,  -1 ) ;

    if doNodes then begin
      provider.logEnd( nodes ) ;
      inc( provider.lastPos ) ;
      provider.busyShakeEx ;
    end ;
    doNodes := False ;

    if not doWays then
      provider.logBegin( ' ways' ) ;
    doWays  := True ;

    for i := 0 to _way.refs.Count -1 do begin
      ref := _way.refs[i] ;
      if provider.FirstId = -1 then
        provider.FirstId := ref ;
      provider.LastId := ref ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_WAY_MEMBER,
        'INSERT INTO way_member ( way_id, pos_id, node_id ) ' +
        'VALUES (:way_id, :pos_id, :node_id )'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_WAY_MEMBER, 'way_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_WAY_MEMBER, 'pos_id' , provider.PosId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_WAY_MEMBER, 'node_id', ref, -1
      ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_WAY_MEMBER ) ;
      inc( provider.PosId ) ;
    end ;

    if provider.FirstId = provider.LastId then
      provider.Db.sqlTableSetField( OSM_SQL_STMT_WAY, 'closed' , 1, -1 )
    else
      provider.Db.sqlTableSetField( OSM_SQL_STMT_WAY, 'closed' , 0, -1 ) ;

    provider.Db.sqlTablePost( OSM_SQL_STMT_WAY ) ;
    provider.FirstId := -1 ;
    inc( ways ) ;

    for i := 0 to _way.attrs.Count -1 do begin
      k := _way.attrs[i].Key ;
      v := _way.attrs[i].Value ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_TAG,
        'INSERT INTO tag ( object_id, object_type, key, val ) ' +
        'VALUES (:object_id, :object_type, :key, :val)'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_type' , provider.ObjectType, -1
      ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'key' , k, -1 ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'val' , v, -1 ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_TAG ) ;
    end ;

    doBusy ;
  end ;

  procedure T_PbfBuilder.doRelation(
    const _relation : TGIS_OSMRelation
  );
  var
    id  : Int64 ;
    ref : Int64 ;
    i   : Integer ;
    k,v : String ;
    val : String ;
    tp  : Integer ;
  begin
    id  := _relation.id ;
    provider.ObjectId := id ;
    provider.PosId    := 1 ;
    provider.ObjectType := OSM_SQL_TYPE_RELATION ;

    provider.Db.sqlTableAppend(
      OSM_SQL_STMT_RELATION,
      'INSERT INTO relation ( id, type ) ' +
      'VALUES (:id, :type)'
     ) ;
    provider.Db.sqlTableSetField( OSM_SQL_STMT_RELATION, 'id', id, -1 ) ;

    if doWays then begin
      provider.logEnd( ways ) ;
      inc( provider.lastPos ) ;
      provider.busyShakeEx ;
    end ;
    doWays  := False ;

    if not doRels then
      provider.logBegin( ' relations' ) ;
    doRels  := True ;

    for i := 0 to _relation.members.Count -1 do begin
      ref := _relation.members[i].id ;

      case _relation.members[i].mtype of
        TGIS_OSMMemberType.node :
          tp := OSM_SQL_TYPE_NODE ;
        TGIS_OSMMemberType.way  :
          tp := OSM_SQL_TYPE_WAY ;
        TGIS_OSMMemberType.relation :
          tp := OSM_SQL_TYPE_RELATION
        else
          tp := -1 ;
      end;

      val := _relation.members[i].role ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_REL_MEMBER,
        'INSERT INTO relation_member '+
        '( relation_id, pos_id, member_id, member_type, role ) ' +
        'VALUES (:relation_id, :pos_id, :member_id, :member_type, :role )'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'relation_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'pos_id' , provider.PosId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'member_id' , ref, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'member_type' , tp, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_REL_MEMBER, 'role' , val, -1
      ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_REL_MEMBER ) ;

      inc( provider.PosId ) ;
    end ;

    for i := 0 to _relation.attrs.Count -1 do begin
      k := _relation.attrs[i].Key ;
      v := _relation.attrs[i].Value ;

      provider.Db.sqlTableAppend(
        OSM_SQL_STMT_TAG,
        'INSERT INTO tag ( object_id, object_type, key, val ) ' +
        'VALUES (:object_id, :object_type, :key, :val)'
       ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_id' , provider.ObjectId, -1
      ) ;
      provider.Db.sqlTableSetField(
        OSM_SQL_STMT_TAG, 'object_type' , provider.ObjectType, -1
      ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'key' , k, -1 ) ;
      provider.Db.sqlTableSetField( OSM_SQL_STMT_TAG, 'val' , v, -1 ) ;
      provider.Db.sqlTablePost( OSM_SQL_STMT_TAG ) ;

      if provider.ObjectType = OSM_SQL_TYPE_RELATION then
        if k = OSM_TYPE then
          provider.RelType := v ;
    end ;

    provider.Db.sqlTableSetField(
      OSM_SQL_STMT_RELATION, 'type', provider.RelType, -1
    ) ;
    provider.Db.sqlTablePost( OSM_SQL_STMT_RELATION ) ;
    inc( relations ) ;

    doBusy ;
  end ;
  {$ENDIF}

  constructor T_OsmAttributes.Create ;
  begin
    inherited ;

    dictIgnoredName := TDictionary<String, Integer>.Create ;

    dictIgnoredName.Add( 'created_by', 0 ) ;
    dictIgnoredName.Add( 'converted_by', 0 ) ;
    dictIgnoredName.Add( 'ele', 0 ) ;
    dictIgnoredName.Add( 'source', 0 ) ;
    dictIgnoredName.Add( 'time', 0 ) ;
    dictIgnoredName.Add( 'note', 0 ) ;
    dictIgnoredName.Add( 'fixme', 0 ) ;
    dictIgnoredName.Add( 'FIXME', 0 ) ;
    dictIgnoredName.Add( 'email', 0 ) ;
    dictIgnoredName.Add( 'source:addr', 0 ) ;
    dictIgnoredName.Add( 'wikipedia', 0 ) ;
    dictIgnoredName.Add( 'wikdata', 0 ) ;
    dictIgnoredName.Add( 'website', 0 ) ;
    dictIgnoredName.Add( 'TODO', 0 ) ;
    dictIgnoredName.Add( 'url', 0 ) ;
  end;

  procedure T_OsmAttributes.doDestroy ;
  begin
    FreeObject( dictIgnoredName ) ;
    inherited ;
  end ;

  function T_OsmAttributes.attrNameIgnored(
    const _name : String
  ) : Boolean ;
  begin
    Result := dictIgnoredName.ContainsKey( _name ) ;
  end ;

  function T_OsmAttributes.attrNameNormalized(
    const _name  : String
  ) : String ;
  var
    i     : Integer        ;
    k     : Integer        ;
    c     : Char           ;
    sname : String         ;
    tmp   : TStringBuilder ;
    chs   : TCharSet ;
  begin
    tmp := TStringBuilder.Create ;
    try
      sname := _name ;
      chs := PrepareCharSet( [ 'AZ', 'az', '_', '09' ] ) ;
      k := 0 ;
      for i := StringFirst to StringLast( sname )  do begin
        c := sname[i] ;
        if ord( c ) > 127 then  begin
          // character should be a ANSI letter
          tmp.Append( '_' ) ;
          inc( k ) ;
        end
        else if not InCharSet( c, chs ) then begin
          tmp.Append( '_' ) ;
          inc( k ) ;
        end
        else
          tmp.Append( c ) ;
      end ;

      Result := tmp.ToString ;
    finally
      FreeObject( tmp ) ;
    end ;
  end ;

//==============================================================================
// Lider.CG.GIS.GeoLayerOSM
//==============================================================================

  class procedure GisLayerOSM.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-OSM', 'OpenStreetMap Format',
                   TGIS_LayerOSM, '.osm;.pbf',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                    [ TGIS_RegisteredOperationType.Read
                    ],
                   True
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    GisLayerOSM.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
