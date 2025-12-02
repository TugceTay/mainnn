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
  Encapsulation of a GML Layer.

  For more information see GML 3.1.1 specification at www.opengeospatial.org.
}

{$IFDEF DCC}
  unit GisLayerGML ;
  {$HPPEMIT '#pragma link "GisLayerGML"'}
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
    System.IO,
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,

    GisTypes,
    GisClasses,
    GisLayerVector,
    GisXmlDoc,
    GisUtils,
    GisXmlSax ;
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

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerGML = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   After Adding Member event
  /// </summary>
  /// <param name="_sender">
  ///   object
  /// </param>
  /// <param name="_member_name">
  ///   name of member found
  /// </param>
  /// <param name="_member_attr">
  ///   array of attribute' records, must be casted as type of array of array
  ///   [0..1] of string
  /// </param>
  TGIS_AfterAddingMemberEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure (
    _sender      : TObject ;
    _member_name : String  ;
    _member_attr : TObject
  ) of object;


  {#GENDOC:HIDE}
  /// <summary>
  ///   GML attribute definition.
  /// </summary>
  TGIS_AttributeGML = {$IFDEF OXYGENE} unit {$ENDIF} record
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
  ///   Encapsulation of a GML Layer.
  /// </summary>
  TGIS_LayerGML = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // property internal values
      FUseLongCoord            : Boolean ;
      FUseOldStyleCoord        : Boolean ;
      FSaveFields              : Boolean ;
      FAxisOrderReversed       : Boolean ;
      FFlattenXML              : Boolean ;
      FSchemaTypes             : TObject ;
      FFeatures                : TGIS_Strings ;
      FAfterAddingMember       : TGIS_AfterAddingMemberEvent ;
      FAfterAddingMemberFilter : TGIS_Strings ;
      FDetectFieldTypes        : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private variable
      addingMember    : Boolean ;
      addingMultiGeo  : Boolean ;
      addingObject    : Boolean ;
      addingPolygon   : Boolean ;
      finishedPolygon : Boolean ;
      addingSurface   : Boolean ;
      addingSolid     : Boolean ;
      addingMSurface  : Boolean ;
      addingMCurve    : Boolean ;
      addingCCurve    : Boolean ;
      addingRing      : Boolean ;
      addingMPolygon  : Boolean ;
      addingLine      : Boolean ;
      addingArc       : Boolean ;
      addingCircle    : Boolean ;
      addingMLine     : Boolean ;
      addingCoords    : Boolean ;
      addingPoint     : Boolean ;
      addingMPoint    : Boolean ;
      addingSTriangle : Boolean ;
      usingCoord      : Boolean ;
      usingPos        : Boolean ;
      UseAttrName     : Boolean ;
      lastElement     : String ;
      fieldName       : String ;
      fieldValue      : Variant ;
      elementNo       : Cardinal;
      coordDim        : Integer ;
      forceCoordDim   : Integer ;
      bSrsDimUsed     : Boolean ;
      srsName         : String  ;
      foundSRS        : Boolean ;
      hasGeomPrefix   : Boolean ;
      hasPrefix       : Boolean ;
      schemaPath      : String  ;
      foundSchema     : Boolean ;
      schemaChecks    : Integer ;
      SAXWriter       : TGIS_SAXWriter ;
      FXMLPath        : TStringList ;
      FXMLFields      : TStringList ;
      FDepth          : Integer ;
      FDepthFeature   : Integer ;
      FDepthAttribute : Integer ;
      FDepthGeometry  : Integer ;
      lAttributes     : TList<TGIS_AttributeGML> ;
      isCityGML       : Boolean ;
      cityGMLField    : String ;
      cityGMLSplit    : Boolean ;
      cityGMLId       : String ;
      cityGMLType     : String ;
      cityGMLDepth    : Integer ;
      cityGMLName     : String ;

      curFeatureLayer : String ;
      isFilterByLayer : Boolean ;
      filterLayerName : String ;
      filterShapeType : TGIS_ShapeType ;

      isPrescanMode   : Boolean ;
      prescanLayers   : TObject ;
      prescanTypeList : TList<TGIS_ShapeType> ;

      oldItemsCount   : Integer ;
      numberReturned  : Integer ;

      FFieldValuesSeparator : String ;

      /// <summary>
      ///   Current shape.
      /// </summary>
      currShp : TGIS_Shape;

      /// <summary>
      ///   Current feature shapes.
      /// </summary>
      shpList : TList<TGIS_Shape> ;

      /// <summary>
      ///   Current point.
      /// </summary>
      currPtg : TGIS_Point3D;

      /// <summary>
      ///   Current shape point list.
      /// </summary>
      ptgList : TGIS_Point3DList;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private routine
      procedure pushPath              ( const _name : String
                                      ) ;
      procedure popPath ;

      function  lastPathElement       : String ;

      function  getFeatureLayer       ( const _element : String
                                      ) : Integer ;

      /// <summary>
      ///   Add new multipoint.
      /// </summary>
      procedure addMultiPoint         ;

      /// <summary>
      ///   Add new point.
      /// </summary>
      procedure addPoint              ;

      /// <summary>
      ///   Add new line.
      /// </summary>
      procedure addLine               ;

      /// <summary>
      ///   Add shape part.
      /// </summary>
      procedure addShapePart          ;

      /// <summary>
      ///   Add new polygon.
      /// </summary>
      procedure addPolygon            ;

      /// <summary>
      ///   Add points to shape using long format coords.
      /// </summary>
      /// <param name="_str">
      ///   string of coords to parse and add to shape
      /// </param>
      procedure addPointsToShape      ( const _str          : String
                                      ) ;

      /// <summary>
      ///   Add attributes to shape
      /// </summary>
      /// <param name="_currShp">
      ///   current shape
      /// </param>
      /// <param name="_atrname">
      ///   field name
      /// </param>
      /// <param name="_atrvalue">
      ///   field value
      /// </param>
      procedure addAtributesToShape   ( const _currShp      : TGIS_Shape ;
                                        const _atrname      : String     ;
                                        const _atrvalue     : String
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
      procedure addAtrToArray         ( const _atrname      : String         ;
                                        const _atrvalue     : String
                                      ) ; overload;

      /// <summary>
      ///   Remember attributes.
      /// </summary>
      /// <param name="_atrname">
      ///   attribute name to remember
      /// </param>
      /// <param name="_atrvalue">
      ///   attribute value to remember
      /// </param>
      procedure addAtrToArray         ( const _atrname      : String         ;
                                        const _atrvalue     : String         ;
                                        const _useAttrName  : Boolean
                                      ) ; overload;

      /// <summary>
      ///   Converts TGIS_Point into string in order to write it to file.
      /// </summary>
      /// <param name="_point">
      ///   point to convert
      /// </param>
      function  pointToString         ( const _point        : TGIS_Point
                                      ) : String ; overload;

      /// <summary>
      ///   Converts TGIS_Point into string in order to write it to file.
      /// </summary>
      /// <param name="_point">
      ///   point to convert
      /// </param>
      function  pointToString         ( const _point        : TGIS_Point3D
                                      ) : String ; overload;

      /// <summary>
      ///   Converts TGIS_Point into string in order to write it in a file.
      /// </summary>
      /// <param name="_point">
      ///   point to convert
      /// </param>
      /// <param name="_yorx">
      ///   if True, then return Y coord, otherwise return X coord
      /// </param>
      function  pointCToString        ( const _point        : TGIS_Point     ;
                                        const _yorx         : Boolean
                                      ) : String ;

      /// <summary>
      ///   Writes geometry data and properties to GML File.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save (data only)
      /// </param>
      procedure writeGeometry         ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes information, properties to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeData             ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Format long coordinates and save to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      /// <param name="_partno">
      ///   shape part to write
      /// </param>
      /// <param name="_partsize">
      ///   part size to write
      /// </param>
      procedure addChars              ( const _shp          : TGIS_Shape     ;
                                        const _partno       : Integer        ;
                                        const _partsize     : Integer
                                      ) ;

      /// <summary>
      ///   Writes Polygon to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writePolygon          ( const  _shp         : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes MultiPolygon to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeMultiPolygon     ( const  _shp         : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes Point to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writePoint            ( const  _shp         : TGIS_Shape
                                      ) ; overload;

      /// <summary>
      ///   Writes Point to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      /// <param name="_partno">
      ///   shape part to save
      /// </param>
      procedure writePoint            ( const  _shp         : TGIS_Shape     ;
                                        const _partno       : Integer
                                      ) ; overload;

      /// <summary>
      ///   Writes MultiPoint to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeMultiPoint       ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes LineString to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeLine             ( const _shp          : TGIS_Shape
                                      ) ; overload;

      /// <summary>
      ///   Writes LineString to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      /// <param name="_partno">
      ///   shape part to save
      /// </param>
      procedure writeLine             ( const _shp          : TGIS_Shape     ;
                                        const _partno       : Integer
                                      ) ; overload;

      /// <summary>
      ///   Writes MultiLine to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeMultiLine        ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes BoundedBy and Box section to GML file.
      /// </summary>
      /// <param name="_extent">
      ///   layer extent to save
      /// </param>
      procedure writeBox              ( const _extent       : TGIS_Extent
                                      ) ;

      /// <summary>
      ///   Writes coordinates section in GML file
      /// </summary>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      /// <param name="_partno">
      ///   shape part to write
      /// </param>
      procedure writeCoordinates      ( const _shp          : TGIS_Shape     ;
                                        const _partno       : Integer
                                      ) ;

      /// <summary>
      ///   Set attributes values to write to file
      /// </summary>
      /// <param name="_namespaceuri">
      ///   always empty
      /// </param>
      /// <param name="_localname">
      ///   always empty
      /// </param>
      /// <param name="_qname">
      ///   keeps 'gml:xxx' schema
      /// </param>
      /// <param name="_val">
      ///   value added to QName
      /// </param>
      procedure setAttributes         ( var   _namespaceuri : String         ;
                                        var   _localname    : String         ;
                                        var   _qname        : String         ;
                                        const _val          : String
                                      );

      /// <summary>
      ///   Save StartElement node
      /// </summary>
      /// <param name="_namespaceuri">
      ///   NameSpace URI value
      /// </param>
      /// <param name="_localname">
      ///   LocalName value
      /// </param>
      /// <param name="_qname">
      ///   keeps 'gml:xxx' schema
      /// </param>
      /// <param name="_attributes">
      ///   element attribute value
      /// </param>
      procedure startTag              ( const _namespaceuri : String         ;
                                        const _localname    : String         ;
                                        const _qname        : String         ;
                                        const _attributes   : SAXAttributes
                                      ) ;

      /// <summary>
      ///   Save EndElement node to GML file
      /// </summary>
      /// <param name="_namespaceuri">
      ///   NameSpace URI value
      /// </param>
      /// <param name="_localname">
      ///   LocalName value
      /// </param>
      /// <param name="_qname">
      ///   keeps 'gml:xxx' schema
      /// </param>
      procedure endTag                ( const _namespaceuri : String         ;
                                        const _localname    : String         ;
                                        const _qname        : String
                                      ) ;

      /// <summary>
      ///   Get schema file path.
      /// </summary>
      /// <param name="_path">
      ///   Schema file path
      /// </param>
      function  getSchemaPath         ( const _path : String
                                      ) : String;

      /// <summary>
      ///   Write schema file.
      /// </summary>
      procedure writeSchema           ( const _path : String
                                      );

      /// <summary>
      ///   Parse schema file.
      /// </summary>
      /// <param name="_path">
      ///   Schema file path
      /// </param>
      procedure parseSchemaFile       ( const _path   : String
                                      );

      /// <summary>
      ///   Parse schema.
      /// </summary>
      /// <param name="_schemadef">
      ///   Schema document
      /// </param>
      procedure parseSchema           ( const _schemadef : IXMLNode
                                      ) ;

      /// <summary>
      ///   Add new arc points.
      /// </summary>
      /// <param name="_lst">
      ///   list of points to be added
      /// </param>
      procedure doDrawArc             ( const _lst    : TGIS_Point3DList );

      /// <summary>
      ///   Add new circle points.
      /// </summary>
      /// <param name="_radius">
      ///   circle radius
      /// </param>
      procedure addCirclePointsToShape( const _radius : String );

      /// <summary>
      ///   Is xml element a feature.
      /// </summary>
      /// <param name="_name">
      ///   element name
      /// </param>
      /// <param name="_full">
      ///   full element name
      /// </param>
      function  isFeature             ( const _name   : String ;
                                        const _full   : String
                                       ) : Boolean;

      /// <summary>
      ///   Is element a geometry feature.
      /// </summary>
      /// <param name="_name">
      ///   element name
      /// </param>
      function  isGeometry            ( const _name   : String
                                       ) : Boolean ;

      /// <summary>
      ///   Is element a multigeometry feature.
      /// </summary>
      /// <param name="_name">
      ///   element name
      /// </param>
      function  isMultiGeometry       ( const _name   : String
                                       ) : Boolean ;

      /// <summary>
      ///   Prepare geometry feature.
      /// </summary>
      /// <param name="_name">
      ///   element name
      /// </param>
      procedure prepareGeometry       ( const _name   : String
                                       );

      /// <summary>
      ///   Unprepare geometry.
      /// </summary>
      procedure unprepareGeometry ;

      /// <summary>
      ///   Check if attribute is from citygml schema.
      /// </summary>
      function isCityGmlAttribute ( const _name : String
                                   ) : Boolean ;
      /// <summary>
      ///   Check if feature is from citygml schema.
      /// </summary>
      function isCityGmlFeature ( const _name : String ;
                                    var _lod  : Integer
                                   ) : Boolean ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp    ; override;

    protected
      // destructor

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      // procedures and functions

      /// <inheritdoc/>
      procedure Build         ( const _path      : String ;
                                const _extent    : TGIS_Extent      ;
                                const _type      : TGIS_ShapeType   ;
                                const _dim       : TGIS_DimensionType
                              ) ; override;

      /// <inheritdoc/>
      procedure ImportLayerEx ( const _layer       : TGIS_LayerVector ;
                                const _extent      : TGIS_Extent      ;
                                const _type        : TGIS_ShapeType   ;
                                const _scope       : String           ;
                                const _shape       : TGIS_Shape       ;
                                const _de9im       : String           ;
                                const _truncated   : Boolean
                              ) ; override;

      /// <inheritdoc/>
      procedure SaveData      ; override;

      /// <inheritdoc/>
      function  PreRecognize  ( const _path        : String           ;
                                var   _new_path    : String
                              ) : Boolean ; override;

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

      {$IFNDEF GENXDK}
        /// <summary>
        ///   Open a data from the Stream keeping the current layer state.
        ///   Used internally by WFS layer for data paging.
        /// </summary>
        /// <returns>
        ///   number of shappes create from a data
        /// </returns>
        function AddFromStream : Integer ;

        /// <summary>
        ///   Parse schema from stream.
        ///   Used internally by WFS layer for describing feature type.
        /// </summary>
        /// <param name="_stream">
        ///   stream to parse
        /// </param>
        procedure ParseSchemaStream( const _stream : TStream ) ;
      {$ENDIF}
    public // properties

        /// <summary>
        ///   If true, long coordinates format will be used, otherwise short
        ///   \&lt;x\&gt;\&lt;y\&gt;.
        /// </summary>
        property UseLongCoord : Boolean read  FUseLongCoord
                                        write FUseLongCoord
                                        {$IFNDEF OXYGENE}
                                          default True
                                        {$ENDIF} ;

        /// <summary>
        ///   If true, gml 2.1 coordinates format will be used
        ///   (\&lt;coordinates\&gt;), otherwise 3.1 \&lt;posList\&gt;.
        /// </summary>
        property UseOldStyleCoord : Boolean read  FUseOldStyleCoord
                                            write FUseOldStyleCoord
                                            {$IFNDEF OXYGENE}
                                              default True
                                            {$ENDIF} ;

        /// <summary>
        ///   If true, import saves object fields &amp; properties.
        /// </summary>
        property SaveFields : Boolean read  FSaveFields
                                      write FSaveFields
                                      {$IFNDEF OXYGENE}
                                        default False
                                      {$ENDIF} ;
      /// <summary>
      ///   Filters list for AfterAddingMember event - fires event only if
      ///   element name is on filter list.
      /// </summary>
      property AfterAddingMemberFilter :
        TGIS_Strings  read FAfterAddingMemberFilter
                      write FAfterAddingMemberFilter;

      /// <summary>
      ///   Use AxisOrderReversed instead.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Obsoleted!
      ///    </note>
      /// </remarks>
      property InverseXY : Boolean read  FAxisOrderReversed
                                   write FAxisOrderReversed
                                    {$IFNDEF OXYGENE}
                                      default False
                                    {$ENDIF} ;



      /// <summary>
      ///   If true, then reverse coordinates of axis.
      /// </summary>
      property AxisOrderReversed : Boolean read  FAxisOrderReversed
                                           write FAxisOrderReversed
                                           {$IFNDEF OXYGENE}
                                              default False
                                           {$ENDIF} ;

      /// <summary>
      ///   If true, nested attributes names will be concatenated with _ separator.
      /// </summary>
      property FlattenXML : Boolean read  FFlattenXML
                                    write FFlattenXML
                                    {$IFNDEF OXYGENE}
                                      default False
                                    {$ENDIF} ;

    published // events
      /// <event/>
      /// <summary>
      ///   Event fired After Adding Member - for data not connected with
      ///   graphics. Uses AfterAddingMemberFilter list to set filter event
      ///   firing.
      /// </summary>
      property AfterAddingMemberEvent : TGIS_AfterAddingMemberEvent
                                        read  FAfterAddingMember
                                        write FAfterAddingMember;
   end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Variants,
    System.Math,

    GisRtl,
    GisFunctions,
    GisInternals,
    GisResource,
    GisRegistredLayers,
    GisStreams,
    GisTypesUI,
    GisLogger,
    GisGeometryFactory ;
{$ENDIF}

type

  T_gmlFieldDef = record
    _name : String ;
    _type : TGIS_FieldType ;
  end ;

  { Encapsulation of sax handler for GML files.
  }
  T_saxHandlerGML = class( TGIS_SAXContentHandler )
    private
      // GML layer handle.
      oGML : TGIS_LayerGML ;
    private
      function isGMLPrefix( const _qname      : String ;
                            const _attribs    : IVBSAXAttributes ;
                              var _hasPrefix  : Boolean
                          ) : Boolean ;
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
      // Create an instance for provided GML layer
      // _layer GML layer
      constructor Create ( const _layer : TGIS_LayerGML
                         ) ;
  end ;

  T_FeatureType = class
    public
      SType : TGIS_ShapeType ;
      Count : TGIS_Uid ;
  end ;

  T_MultiValueDictionary = class
    private
      oDic : TDictionary<String, TObjectList<T_FeatureType>> ;
    public
      constructor Create ;
      {$IFDEF DCC}
        destructor  Destroy ;  override ;
      {$ENDIF}
      procedure Add( const _key   : String ;
                     const _value : TGIS_ShapeType
                   ) ;
      function  GetKeys : TArray<String> ;
      function  GetValues( const _key   : String ) : TObjectList<T_FeatureType>;
  end ;

const
   // GML constants
    GML_FEATURE_MEMBER  = 'gml:featureMember' ;
    GML_GEOMETRY_MEMBER = 'geometryMember';
    GML_POLYGON_MEMBER  = 'polygonMember';
    GML_MPOLYGON_MEMBER = 'gml:polygonMember';
    GML_MULTI_GEOMETRY  = 'gml:MultiGeometry';
    GML_FEATURE_PREFIX  = 'Member';
    GML_FEATURES_PREFIX = 'Members';
    GML_FEATURE_POSTFIX = 'feature';

    GML_POINT           = 'gml:Point' ;
    GML_MULTI_POINT     = 'gml:MultiPoint' ;
    GML_LINE_STRING     = 'gml:LineString' ;
    GML_MULTI_LINE      = 'gml:MultiLineString' ;
    GML_LINE_MEMBER     = 'gml:lineStringMember' ;
    GML_POLYGON         = 'gml:Polygon' ;
    GML_MULTI_POLYGON   = 'gml:MultiPolygon';
    GML_MULTI_CURVE     = 'gml:MultiCurve';
    GML_COMPOSITE_CURVE = 'gml:CompositeCurve';
    GML_CURVE           = 'gml:Curve';
    GML_CURVE_MEMBER    = 'curveMember' ;
    GML_SURFACE         = 'gml:Surface' ;
    GML_SOLID           = 'gml:Solid' ;
    GML_CSURFACE        = 'gml:CompositeSurface' ;
    GML_LINEAR_RING     = 'gml:LinearRing' ;
    GML_MULTI_SURFACE   = 'gml:MultiSurface';
    GML_SURFACE_MEMBER  = 'surfaceMember' ;
    GML_STRIANGLE       = 'gml:TriangulatedSurface' ;
    GML_TRIANGLE        = 'gml:Triangle' ;

    GML_INNERBOUND      = 'gml:innerBoundaryIs' ;
    GML_OUTERBOUND      = 'gml:outerBoundaryIs' ;
    GML_INTERIOR        = 'gml:interior' ;
    GML_EXTERIOR        = 'gml:exterior' ;

    GML_COORDINATES     = 'gml:coordinates' ;
    GML_COORDINATES2    = 'gml:pos' ;
    GML_COORDINATES3    = 'gml:posList' ;
    GML_COORD           = 'gml:coord' ;

    GML_BOX             = 'gml:Box' ;
    GML_BOUNDED_BY      = 'gml:boundedBy' ;
    GML_ENVELOPE        = 'gml:Envelope' ;

    GML_ARC             = 'gml:Arc';
    GML_CIRCLE_CENTER   = 'gml:CircleByCenterPoint';
    GML_POINT_MEMBER    = 'pointMember' ;
    GML_RADIUS          = 'gml:radius';
    GML_COORD_X         = 'gml:X' ;
    GML_COORD_Y         = 'gml:Y' ;
    GML_COORD_Z         = 'gml:Z' ;
    GML_DESCRIPTION     = 'description';
    GML_NAME            = 'name';
    GML_SRS_DIMENSION   = 'srsDimension';
    GML_SRS_NAME        = 'srsName';

   // Property constants
    GML_IDENTIFIER      = 'gml:identifier' ;
    GML_IDENTIFIER_SHORT= 'gml:id' ;
    GML_NAME_FULL       = 'gml:name' ;
    GML_PROPERTY_FULL   = 'gml:Property' ;
    GML_PROPERTY_MEMBER = 'g:PROPERTIES' ;
    GML_PROPERTY        = 'Property' ;
    GML_GEOMETRY        = 'GEOMETRY';

   // Prefixex
    GML                 = 'gml:' ;
    GML_G               = 'g:' ;
    GML_XMLNS_GML       = 'xmlns:gml' ;
    GML_XMLNS_GML_G     = 'xmlns:g' ;
    GML_XMLNS_GML_WWW   = 'http://www.opengis.net/gml' ;
    GML_XMLNS_GML_G_WWW = 'http://www.tatukgis.com/GML' ;
    GML_XMLNS_XSI       = 'xmlns:xsi';
    GML_XMLNS_XSI_WWW   = 'http://www.w3.org/2001/XMLSchema-instance';
    GML_XMLNS_SCHEMA    = 'xsi:schemaLocation';
    GML_XMLNS_SCHEMA_L  = 'http://www.opengis.net/gml'+
                          ' feature.xsd http://www.tatukgis.com/GML ' ;
    GML_TATUKGIS_MAP    = 'TATUKGIS';

    GML_SCHEMA_CITYGML  = 'CityModel' ;

    GML_NOVAL : String  = ' ' ;

//==============================================================================
//  T_MultiValueDictionary
//==============================================================================

  constructor T_MultiValueDictionary.Create ;
  begin
    inherited ;

    oDic := TDictionary<String,TObjectList<T_FeatureType>>.Create ;
  end ;

  {$IFDEF DCC}
    destructor T_MultiValueDictionary.Destroy;
    var
      p : TPair<String, TObjectList<T_FeatureType>> ;
    begin
      {$IFNDEF NEXTGEN}
        for p in oDic do
          p.Value.Free ;
      {$ENDIF}

      FreeObject( oDic ) ;
      inherited ;
    end ;
  {$ENDIF}

  procedure T_MultiValueDictionary.Add(
    const _key    : String ;
    const _value  : TGIS_ShapeType
  ) ;
  var
    lst : TObjectList<T_FeatureType> ;
    fea : T_FeatureType ;
    fnd : Boolean ;
  begin
    if oDic.TryGetValue( _key, lst ) then begin
      fnd := False ;
      for fea in lst do begin
        if fea.SType = _value then begin
          inc( fea.Count ) ;
          fnd := True ;
          break ;
        end ;
      end ;
      if not fnd then begin
        fea := T_FeatureType.Create ;
        fea.SType := _value ;
        fea.Count := 1 ;
        lst.Add( fea ) ;
      end ;
    end
    else begin
      lst := TObjectList<T_FeatureType>.Create( True ) ;
      fea := T_FeatureType.Create ;
      fea.SType := _value ;
      fea.Count := 1 ;
      lst.Add( fea ) ;
      oDic.Add( _key, lst ) ;
    end ;
  end ;

  function T_MultiValueDictionary.GetKeys : TArray<String> ;
  begin
    {$IFDEF DCC}
      Result := oDic.Keys.ToArray ;
    {$ENDIF}
    {$IFDEF CLR}
      SetLength( Result, oDic.Keys.Count ) ;
      oDic.Keys.CopyTo( Result, 0 ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      SetLength( Result, oDic.Keys.size ) ;
      oDic.Keys.toArray( Result ) ;
    {$ENDIF}
  end ;

  function T_MultiValueDictionary.GetValues(
    const _key : String
  ) : TObjectList<T_FeatureType> ;
  var
    lst :  TObjectList<T_FeatureType> ;
  begin
    if oDic.TryGetValue( _key, lst ) then
      Result := lst
    else
      Result := nil ;
  end ;

//==============================================================================
//  T_saxHandlerGML
//==============================================================================

  constructor T_saxHandlerGML.Create(
    const _layer : TGIS_LayerGML
  ) ;
  begin
    inherited Create ;

    oGML := _layer ;
  end ;

  function T_saxHandlerGML.isGMLPrefix(
    const _qname      : String ;
    const _attribs    : IVBSAXAttributes ;
      var _hasPrefix  : Boolean
  ) : Boolean ;
  var
    i     : Integer ;
    aname : String ;
  begin
    _hasPrefix := False ;
    Result := Pos( GML, _qname ) > StringFirst - 1 ;
    if Result then
      _hasPrefix := True ;

    if not Result and assigned( _attribs) then begin

      for i := 0 to _attribs.Length - 1 do begin
        aname := _attribs.GetLocalName( i ) ;
        if (aname = 'xmlns') and
           (Pos( GML_XMLNS_GML_WWW, _attribs.GetValue( i ) ) = StringFirst ) then
        begin
          Result := True ;
          break ;
        end ;
      end ;
    end ;
  end ;

  procedure T_saxHandlerGML.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
   ) ;
  var
    i          : Integer ;
    lname      : String ;
    nsname     : String ;
  begin
    inherited ;

      oGML.hasGeomPrefix := isGMLPrefix( _qname, _attribs, oGML.hasPrefix ) ;
      if not oGML.addingMember and
         oGML.isFeature( _lname, _qname ) then
      begin
        oGML.addingMember  := True;
        oGML.lAttributes.Clear ;
        oGML.FDepthFeature   := oGML.FDepth ;
        oGML.FDepthAttribute := oGML.FDepth + 1 ;
      end
      else if not oGML.addingObject and
              oGML.isGeometry( _qname ) then
      begin
        oGML.addingObject  := True;
        if not oGML.addingMember then begin
          oGML.lAttributes.Clear ;
          oGML.FXMLFields.Clear ;
          oGML.FDepthAttribute := oGML.FDepth ;
        end ;
      end
      else if oGML.isMultiGeometry( _qname ) then begin
        oGML.addingMultiGeo := True ;
      end
      else if ( _qname = GML_BOX ) or ( _qname = GML_ENVELOPE ) then begin
        if ( _attribs.Length > 0 ) then
          for i := 0 to _attribs.Length - 1 do
            if _attribs.GetLocalName( i ) = GML_SRS_NAME then
              oGML.srsName := _attribs.GetValue( i ) ;

        if (oGML.CS.EPSG = 0) and not oGML.foundSRS then begin
          // German adv authority for ALKIS and NAS
          if Pos( 'ETRS89_UTM32', oGML.srsName ) > StringFirst then
            oGML.SetCSByEPSG( 25832 )
          else if Pos( 'ETRS89_UTM33', oGML.srsName ) > StringFirst then
            oGML.SetCSByEPSG( 25833 )
          else if Pos( 'DE_DHDN_3GK3_', oGML.srsName ) > StringFirst then
            oGML.SetCSByEPSG( 31467 )
          else if Pos( 'DE_DHDN_3GK2_', oGML.srsName ) > StringFirst then
            oGML.SetCSByEPSG( 31466 )
          else
            oGML.SetCSByWKT( oGML.srsName ) ;
          oGML.foundSRS := True ;
        end ;
      end
      else if _lname = GML_SCHEMA_CITYGML then begin
        oGML.isCityGML := True ;
        oGML.forceCoordDim := 3 ;
        oGML.coordDim := 3 ;
        if oGML.cityGMLSplit then begin
          oGML.AddFieldInternal( 'CITYGML_LOD', TGIS_FieldType.String, 1, 0 ) ;
          oGML.AddFieldInternal( 'CITYGML_TYPE', TGIS_FieldType.String, 1, 0 ) ;
        end;
      end ;

      // if adding geometry
      if oGML.addingMember or oGML.addingObject then begin

        if not oGML.hasGeomPrefix and
          (oGML.FDepth <= oGML.FDepthAttribute) then
        begin
          i := oGML.getFeatureLayer( _lname ) ;
          if i = 1 then
            oGML.curFeatureLayer := _lname
        end ;

        // remember object property name
        if ( _attribs.Length > 0 ) and not oGML.isPrescanMode then begin
          for i := 0 to _attribs.Length - 1 do begin
            lname  := _attribs.GetLocalName( i ) ;
            nsname := _attribs.GetQName( i ) ;

            if ( _qname = GML_PROPERTY_MEMBER ) then
              continue
            else if lname = GML_SRS_DIMENSION then begin
              oGML.coordDim := StrToIntDef( _attribs.GetValue( i ), 2 ) ;
              oGML.bSrsDimUsed := True ;
            end
            else begin
              if not oGML.hasGeomPrefix then begin
                if oGML.isCityGML and oGML.isCityGmlAttribute( _lname ) then begin
                  oGML.cityGMLField := _attribs.GetValue( i );
                  oGML.fieldName := '' ;
                end
                else begin
                  oGML.fieldName   := _lname;
                  if ( nsname = 'xsi:nil'   ) or
                     ( nsname = 'nilReason' ) or
                     ( nsname = 'xlink:type') or
                     ( nsname = 'xmlns'     ) or
                     ( nsname = 'ring'      ) then
                    oGML.UseAttrName := False
                  else if ( nsname = 'codeSpace' ) then
                    oGML.UseAttrName := True
                  else begin
                    if not IsStringEmpty( lname ) then begin
                      if ( nsname = GML_IDENTIFIER_SHORT ) then begin
                        oGML.addAtrToArray( GML_IDENTIFIER_SHORT,
                                            _attribs.GetValue( i )
                                           ) ;
                        if oGML.isCityGML then
                          oGML.cityGMLId := _attribs.GetValue( i ) ;
                      end
                      else begin
                        if oGML.isCityGML then begin
                          if (_lname = 'value') and not IsStringEmpty( oGML.cityGMLField )
                          then
                            oGML.fieldName := oGML.cityGMLField ;
                        end
                        else
                          oGML.addAtrToArray( _lname + '_' + lname,
                                              _attribs.GetValue( i )
                                             )
                      end ;
                    end ;
                    oGML.UseAttrName := True ;
                  end ;
                end
              end
              else begin
                if ( _qname = GML_PROPERTY_FULL ) then begin
                  oGML.fieldName   := _attribs.GetValue( i ) ;
                  oGML.UseAttrName := True ;
                end
                else if ( _qname = GML_IDENTIFIER ) then begin
                  oGML.fieldName := _lname;
                  oGML.UseAttrName := True ;
                end ;
              end ;
            end ;
          end
        end
        else begin
          if not oGML.hasGeomPrefix then begin
            if IsStringEmpty( oGML.cityGMLField ) then
              oGML.fieldName   := _lname
            else
              oGML.fieldName   := oGML.cityGMLField ;
            oGML.UseAttrName := True;
          end
          else if (_qname = GML_NAME_FULL ) then begin
            oGML.fieldName   := GML_NAME_FULL ;
            oGML.UseAttrName := True;
          end
          else
            oGML.fieldName := '';
        end ;

        if oGML.hasGeomPrefix then begin
          if oGML.hasPrefix then
            oGML.prepareGeometry( _qname )
          else
            oGML.prepareGeometry( GML + _qname ) ;
        end ;

        if oGML.isCityGML and oGML.isCityGmlFeature( _lname, i ) then
          oGML.cityGMLType := _lname ;
      end ;

      if not oGML.foundSchema then begin
        for i := 0 to _attribs.Length - 1 do begin
          if _attribs.GetLocalName( i ) = 'schemaLocation' then begin
            {$IFDEF ISLAND}
              // we cannot access schema file in WebAssembly
            {$ELSE}
              {$IFNDEF CLR}
                oGML.schemaPath := _attribs.GetValue( i ) ;
              {$ENDIF}
              oGML.foundSchema  := True ;
              oGML.parseSchemaFile( oGML.Path ) ;
            {$ENDIF}
          end
          else if _attribs.GetLocalName( i ) = 'numberReturned' then begin
            oGML.numberReturned := StrToIntDef( _attribs.GetValue( i ), 0 ) ;
          end ;
        end ;
      end ;

    oGML.lastElement := _qname ;
    oGML.pushPath( _lname ) ;
    inc( oGML.FDepth ) ;
  end ;

  procedure T_saxHandlerGML.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
   ) ;
  var
    i     : Integer;
    val   : Double ;
    qname : String ;
  begin
    inherited;

    inc( oGML.elementNo );
    if oGML.elementNo mod 200 = 0 then
     if assigned( oGML.Viewer ) then
       oGML.HourglassShake ;

    dec( oGML.FDepth ) ;

    if oGML.hasGeomPrefix then begin
      if oGML.hasPrefix then
        qname := _qname
      else
        qname := GML + _qname ;
    end
    else
      qname := _qname ;

    // remember read point
    if oGML.hasGeomPrefix and (oGML.addingMember or oGML.addingObject) and
       ( qname = GML_COORD ) then begin
      if oGML.AxisOrderReversed then begin
        val := oGML.currPtg.X ;
        oGML.currPtg.X := oGML.currPtg.Y ;
        oGML.currPtg.Y := val ;
      end ;
      oGML.ptgList.Add( oGML.currPtg ) ;
      oGML.addingCoords := False ;
    end ;

    if oGML.UseAttrName then begin
      if not IsStringEmpty( oGML.fieldName ) then
        oGML.addAtrToArray( oGML.fieldName, VarToString(oGML.fieldValue), oGML.UseAttrName ) ;

      if oGML.isCityGML and (oGML.fieldName = GML_NAME_FULL) then
        oGML.cityGMLName := VarToString(oGML.fieldValue) ;
      oGML.UseAttrName := False ;
      oGML.fieldValue  := Unassigned ;
      oGML.cityGMLField := '' ;
    end ;

    if ( qname = GML_ARC ) then
      oGML.addingArc := False;

    if ( qname = GML_POLYGON ) then
      oGML.finishedPolygon := True ;

    if oGML.isCityGML then begin
      if oGML.isCityGmlFeature( _lname, i ) then begin
        oGML.cityGMLId := '' ;
        oGML.cityGMLType := '' ;
      end
    end ;

    if     oGML.isMultiGeometry( _qname ) then begin
      oGML.addingMultiGeo := False ;
    end
    else if oGML.hasGeomPrefix and ( ( qname = GML_COORDINATES  ) or
                                     ( qname = GML_COORDINATES2 ) or
                                     ( qname = GML_COORDINATES3 )
                                   ) then begin
      oGML.addingCoords := False ;
    end
    else if oGML.hasGeomPrefix and ( ( qname = GML_INNERBOUND ) or
                                     ( qname = GML_OUTERBOUND )
                                   ) then begin
      if oGML.usingCoord then
        for i := 0 to oGML.ptgList.Count-1 do
          oGML.currShp.AddPoint3D( oGML.ptgList.Items[ i ] );

      oGML.usingCoord := false;
      oGML.ptgList.Clear;
    end
    else if oGML.isFeature( _lname, qname ) or
            ( oGML.hasGeomPrefix and (
             ( qname = GML_MULTI_POINT  ) or
             ( ( qname = GML_POINT       ) and not oGML.addingMPoint ) or
             ( ( qname = GML_LINE_STRING ) and not oGML.addingMLine  ) or
             ( qname = GML_POLYGON     ) or
             (( qname = GML_SURFACE     ) and not oGML.addingMSurface ) or
             ( qname = GML_MULTI_LINE  ) or
             ( qname = GML_MULTI_POLYGON  ) or
             ( qname = GML_MULTI_CURVE  ) or
             ( qname = GML_MULTI_SURFACE  ) or
             ( ( qname = GML_LINEAR_RING ) and oGML.addingRing )
            ) and
            (
             ( not oGML.addingMember and not oGML.addingMPolygon ) or
             ( not oGML.addingMember and not oGML.addingMSurface ) or oGML.addingMultiGeo
             )
            ) or ( oGML.FDepth = oGML.FDepthFeature) then begin
      oGML.unprepareGeometry ;
    end
    else if ( qname = GML_MULTI_SURFACE  ) then
      oGML.addingMSurface := False
    else if assigned( oGML.FAfterAddingMember ) and
            assigned( oGML.FAfterAddingMemberFilter ) then begin
        for i := 0 to oGML.FAfterAddingMemberFilter.Count - 1 do
          if _lname = oGML.FAfterAddingMemberFilter.Strings[ i ] then
             oGML.AfterAddingMemberEvent( Self, _lname,
                                  TObject( oGML.lAttributes ) ) ;
     end ;

    oGML.popPath ;
  end ;

  procedure T_saxHandlerGML.Characters(
    const _chars   : String
  ) ;
  var
    str     : String ;
    strTrim : String ;
  begin
    inherited;

    if oGML.isPrescanMode then exit ;

    str     := RemoveWhitesFast( _chars ) ;
    strTrim := Trim( str ) ;
    if not IsStringEmpty( strTrim ) then begin
      // if file has short coords format (X,Y), remember points
      if oGML.usingCoord and (oGML.addingMember or oGML.addingObject) then begin
        if oGML.lastElement = GML_COORD_X then
          oGML.currPtg.X := DotStrToFloat( strTrim )
        else if oGML.lastElement = GML_COORD_Y then
          oGML.currPtg.Y := DotStrToFloat( strTrim )
        else if oGML.lastElement = GML_COORD_Z then
          oGML.currPtg.Z := DotStrToFloat( strTrim ) ;
      end ;
      // if adding coordinates in long format, add to shape
      if oGML.addingCoords and ( not oGML.usingCoord ) then
          oGML.addPointsToShape( strTrim )
      else if oGML.addingCircle and ( oGML.lastElement = GML_RADIUS ) then
          oGML.addCirclePointsToShape( strTrim )
      else if oGML.addingMember then begin
          // if FAdding object, remember its properties
          oGML.fieldValue := str ;
      end
    end
    else if oGML.UseAttrName and oGML.addingMember then begin
      oGML.fieldValue := str ;
    end ;
  end ;

  procedure T_saxHandlerGML.FatalError(
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
// GML Layer implementation
//==============================================================================

  constructor TGIS_LayerGML.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;

    currShp           := nil ;
    lAttributes       := TList<TGIS_AttributeGML>.Create ;
    FUseLongCoord     := True;
    FUseOldStyleCoord := True;
    FSaveFields       := True ;
    FAxisOrderReversed:= False ;
    FDetectFieldTypes := True ;
    elementNo         := 0;
    coordDim          := 2;
    forceCoordDim     := -1 ;
    bSrsDimUsed       := False ;
    shpList           := TList<TGIS_Shape>.Create ;
    FDepth            := 0 ;
    FDepthAttribute   := 0 ;
    FDepthGeometry    := 0 ;
    FDepthFeature     := 0 ;
    isCityGML         := False ;
    cityGMLField      := '' ;
    cityGMLSplit      := False ;
    isFilterByLayer   := False ;
    filterLayerName   := '' ;
    cityGMLDepth      := 0 ;
    filterShapeType   := TGIS_ShapeType.Unknown ;
    isPrescanMode     := False ;
    prescanTypeList   := TList<TGIS_ShapeType>.Create ;

    FSchemaTypes      := TList<T_gmlFieldDef>.Create ;

    FXMLPath          := TStringList.Create;
    FXMLFields        := TStringList.Create;
    FXMLFields.Sorted := True ;

    ptgList           := TGIS_Point3DList.Create;
    FFeatures         := TStringList.Create;
    FFieldValuesSeparator := '|';
  end ;

  procedure TGIS_LayerGML.doDestroy ;
  begin
    FreeObject( shpList         ) ;
    FreeObject( lAttributes     ) ;
    FreeObject( ptgList         ) ;
    FreeObject( FSchemaTypes    ) ;
    FreeObject( FFeatures       ) ;
    FreeObject( FXMLFields      ) ;
    FreeObject( FXMLPath        ) ;
    FreeObject( prescanTypeList ) ;

    inherited ;
  end ;

  procedure TGIS_LayerGML.addMultiPoint ;
  begin
    if isPrescanMode then
      prescanTypeList.Add( TGIS_ShapeType.MultiPoint )
    else begin
      if coordDim > 2 then
        currShp := CreateShape( TGIS_ShapeType.MultiPoint, TGIS_DimensionType.XYZ )
      else
        currShp := CreateShape( TGIS_ShapeType.MultiPoint ) ;

      currShp.Lock(TGIS_Lock.Projection);
      currShp.Reset;
      currShp.AddPart;
      shpList.Add( currShp ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.addPoint ;
  begin
    if isPrescanMode then
      prescanTypeList.Add( TGIS_ShapeType.Point )
    else begin
      currShp := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;
      currShp.Lock(TGIS_Lock.Projection);
      currShp.Reset;
      shpList.Add( currShp ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.addLine ;
  var
    i : Integer ;
  begin
    if isPrescanMode then
      prescanTypeList.Add( TGIS_ShapeType.Arc )
    else begin
      currShp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
      currShp.Lock(TGIS_Lock.Projection) ;
      currShp.Reset ;
      shpList.Add( currShp ) ;

      if isCityGML and cityGMLSplit then begin
        if not IsStringEmpty( cityGMLType ) and isCityGmlFeature(cityGMLType, i) then begin
          currShp.SetField( 'CITYGML_LOD', i - 1 ) ;
          currShp.SetField( 'CITYGML_TYPE', cityGMLType ) ;
          addAtributesToShape( currShp, GML_IDENTIFIER_SHORT, cityGMLId ) ;
          addAtributesToShape( currShp, GML_NAME_FULL, cityGMLName ) ;
        end
      end
    end ;
  end ;

  procedure TGIS_LayerGML.addShapePart ;
  var
    num_parts : Integer ;
  begin
    if assigned( currShp ) and not isPrescanMode then begin
      if currShp.IsEmpty and ( currShp.GetNumParts = 0 ) and (currShp.GetNumPoints = 0) then
        currShp.AddPart
      else begin
        num_parts := currShp.GetNumParts ;
        if (num_parts > 0) and (currShp.GetPartSize(num_parts-1) = 0) then
          exit ;

        currShp.AddPart;
      end ;
    end ;
  end ;

  procedure TGIS_LayerGML.addPolygon ;
  var
    i : Integer ;
  begin
    if isPrescanMode then
      prescanTypeList.Add( TGIS_ShapeType.Polygon )
    else begin
      if isCityGML or addingSTriangle then
        currShp := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ )
      else
        currShp := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ ) ;

      currShp.Lock(TGIS_Lock.Projection);
      currShp.Reset;
      shpList.Add( currShp ) ;

      if isCityGML and cityGMLSplit then begin
        if not IsStringEmpty( cityGMLType ) and isCityGmlFeature(cityGMLType, i) then begin
          currShp.SetField( 'CITYGML_LOD', i - 1 ) ;
          currShp.SetField( 'CITYGML_TYPE', cityGMLType ) ;
          addAtributesToShape( currShp, GML_IDENTIFIER_SHORT, cityGMLId ) ;
          addAtributesToShape( currShp, GML_NAME_FULL, cityGMLName ) ;
        end
      end
    end
  end ;

  procedure TGIS_LayerGML.addCirclePointsToShape(
    const _radius : String
  ) ;
  var
    radius : Double;
    center : TGIS_Point3D;
  begin
    if assigned( currShp ) then begin
      radius := DotStrToFloat( _radius );
      center := currShp.GetPoint3D( 0, 0 ) ;

      if radius > 0 then begin
        currShp.Reset;
        currShp.AddPart;
        currShp.StrokeArc( center, radius, radius, 0, 2*Pi, 0, 40 );
      end ;
    end ;
  end ;

  procedure TGIS_LayerGML.doDrawArc(
    const _lst : TGIS_Point3DList
  ) ;
   const
    M_PI   = Pi ;
    M_PI_2 = Pi/2 ;
  var
    a1, a2,
    a3, sweep,
    increment,
    angle       : Double ;
    ptcount, i  : Integer ;
    radius,
    start,
    stop    : Double ;
    center,
    axis,
    first,
    last    : TGIS_Point3D;
    asin, acos : Double ;
  begin
    assert( _lst.Count = 3 ) ;
      first := _lst[0];
      axis  := _lst[1];
      last  := _lst[2];

    if TGIS_GeometryFactory.GisArcFrom3Points3D( first, axis, last, center, radius, start, stop)
    then begin
      a1 := ArcTan2( first.Y - center.Y, first.X - center.X ) ;
      a2 := ArcTan2( axis.Y - center.Y, axis.X - center.X ) ;
      a3 := ArcTan2( last.Y - center.Y, last.X - center.X ) ;

      if ( Abs( first.X - last.X ) < 1.0e-8 ) and ( Abs( first.Y - last.Y ) < 1.0e-8 ) then
        sweep := 2*Pi
      else if(a1 > a2) and (a2 > a3) then
        sweep := a3 - a1
      else if (a1 < a2) and (a2 < a3) then
        sweep := a3 - a1
      else if ((a1 < a2) and (a1 > a3)) or ((a2 < a3) and (a1 > a3)) then
        sweep := a3 - a1 + 2*Pi
      else if ((a1 > a2) and (a1 < a3)) or ((a2 > a3) and (a1 < a3)) then
        sweep := a3 - a1 - 2*Pi
      else
        sweep := 0.0;

      ptcount := FloorS( Abs(sweep * 32 / M_PI_2 ) ) - 1 ;

      increment := M_PI_2 / Max( 4, 32 ) ;
      if (sweep < 0) then
        increment := increment * -1.0 ;
      angle := a1 ;

      currShp.AddPoint3D( first )  ;
      for i := 0 to ptcount - 1 do begin
        angle := angle + increment ;
        if (increment > 0.0) and ( angle > Pi) then
          angle := angle - 2*Pi;
        if (increment < 0.0 ) and ( angle < -1*Pi) then
          angle := angle - 2*Pi;

        SinCos( angle, asin, acos ) ;
        currShp.AddPoint3D( GisPoint3D( center.X + radius * acos,
                                        center.Y + radius * asin,
                                        first.Z
                                       )
                         ) ;
      end ;
      currShp.AddPoint3D( last )  ;
    end ;
  end ;

  procedure TGIS_LayerGML.addPointsToShape(
    const _str : String
  ) ;
  var
    i, p  : Integer        ;
    tkn   : TGIS_Tokenizer ;
    ptg   : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF}  ;
    str   : String ;
    val   : Double ;
    plst  : TGIS_Point3DList ;
  begin
    if not assigned( currShp ) then exit ;

    if currShp.IsEmpty then
      currShp.AddPart ;

    tkn := TGIS_Tokenizer.Create;
    try
      // test coordinates dimension
      if (forceCoordDim > 0) then begin
        if bSrsDimUsed then
          // prefer defined srsDimension
        else
          coordDim := forceCoordDim
      end
      else if bSrsDimUsed then
        // do not detect dimension
      else begin
        if usingPos then
          str := _str
        else begin
          p := Pos(' ', _str ) ;
          if p > StringFirst then
            str := Copy( _str, StringFirst, p )
          else
            str := _str ; // <gml:coord>X,Y,Z</gml:coord>
        end ;

        tkn.Execute( str, [ ' ', ',' ] ) ;
        if tkn.Result.Count > coordDim then
          coordDim := Max( tkn.Result.Count, 3 )
        else
        if tkn.Result.Count = coordDim then

        else
          coordDim := 2 ;
      end ;

      tkn.Execute( _str, [ ' ', ',' ] );
      i := 0 ;

      if addingArc or isCityGML then
        plst := TGIS_Point3DList.Create ;

      while i < tkn.Result.Count - 1 do begin
        ptg.X := DotStrToFloat( tkn.Result[ i ] ) ;
        inc( i );
        ptg.Y := DotStrToFloat( tkn.Result[ i ] ) ;
        inc( i );
        if (coordDim > 2) and (i < tkn.Result.Count) then begin
          ptg.Z := DotStrToFloat( tkn.Result[ i ] ) ;
          inc( i );
        end
        else
          ptg.Z := 0 ;
        ptg.M := 0 ;

        if AxisOrderReversed then begin
          val := ptg.X ;
          ptg.X := ptg.Y ;
          ptg.Y := val ;
        end ;

        if addingArc then begin
          plst.Add( ptg ) ;
          if ( plst.Count = 3 ) then begin
            doDrawArc( plst ) ;
            plst.Clear ;
          end ;
        end
        else if isCityGML then
          plst.Add( ptg )
        else begin
          if coordDim > 2 then
            currShp.AddPoint3D( ptg )
          else
            currShp.AddPoint( GisPoint2DFrom3D( ptg ) ) ;
        end ;
      end ;

      if isCityGML then begin
        for i := 0 to plst.Count - 1 do
          if coordDim > 2 then
            currShp.AddPoint3D( plst[i] )
          else
            currShp.AddPoint( GisPoint2DFrom3D( plst[i] ) ) ;
      end;

      if addingArc or isCityGML then
        FreeObject( plst ) ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.addAtributesToShape(
    const _currShp  : TGIS_Shape ;
    const _atrname  : String ;
    const _atrvalue : String
  ) ;
  var
    res    : Integer ;
    val    : String  ;
    atr    : String  ;
    canAdd : Boolean ;
    fld    : TGIS_FieldInfo ;
    fldvt  : TGIS_FieldType ;
  begin
    if not assigned( _currShp ) then exit;

    atr := _atrname  ;
    val := _atrvalue ;

    canAdd := False ;
    res := FindField( atr ) ;
    if res < 0 then begin
      if (TList<T_gmlFieldDef>( FSchemaTypes ).Count = 0) or FlattenXML or
         ( atr = GML_IDENTIFIER_SHORT) or ( atr = GML_NAME_FULL ) then
      begin
        canAdd := True ;
        AddFieldInternal( atr, TGIS_FieldType.String, 1, 0 ) ;
        res := Fields.Count - 1 ;
      end ;
    end
    else
      canAdd := True ;

    if isCityGML and _currShp.IsFieldModifiedEx( res ) then
      canAdd := False ;

    if canAdd and ( not IsStringEmpty( val ) ) then begin
      // concatenate duplicated fields
      if _currShp.IsFieldModifiedEx( res ) then begin
        if ( atr = GML_IDENTIFIER_SHORT) then
          val := VarToString( _currShp.GetFieldEx( atr ) )
        else
          val := VarToString( _currShp.GetFieldEx( atr ) ) + FFieldValuesSeparator + val ;
      end;

      fld := FieldInfo(res) ;
      if FDetectFieldTypes then begin
        // do we need to update the type
        fldvt := DetectValueType( val ) ;

        if ( fld.FieldType = TGIS_FieldType.String ) and
           ( fldvt <> TGIS_FieldType.String        ) and
           ( fld.Binary = 0                        ) then
        begin
          fld.FieldType := fldvt ;
          case fldvt of
            TGIS_FieldType.Number : begin
              // promote to integer
              fld.Width      := 10 ;
              fld.NewWidth   := 10 ;
            end;
            TGIS_FieldType.Float  : begin
              // promote to float
              fld.Width      := 10 ;
              fld.NewWidth   := 10 ;
              fld.Decimal    := 7 ;
              fld.NewDecimal := 7 ;
            end;
          end ;
        end
        else if (fld.FieldType = TGIS_FieldType.Number) and ( fldvt = TGIS_FieldType.Float ) then
        begin
          fld.FieldType := fldvt ;
          // promote to float
          fld.Width      := 10 ;
          fld.NewWidth   := 10 ;
          fld.Decimal    := 7 ;
          fld.NewDecimal := 7 ;
        end
        else if (fld.FieldType = TGIS_FieldType.Number) and ( fldvt = TGIS_FieldType.Boolean ) then
        begin
          fld.FieldType := fldvt ;
        end
        else if (fld.FieldType <> TGIS_FieldType.String) and ( fldvt = TGIS_FieldType.String ) then
        begin
          // downgrade to string
          fld.FieldType := fldvt ;
          fld.Binary := 1 ; // mark as not upgrade-able
        end
        else if (fld.FieldType = TGIS_FieldType.String) and ( fldvt = TGIS_FieldType.String ) then
        begin
          // keep string
          fld.Binary := 1 ; // mark as not upgrade-able
        end ;
      end ;

      if fld.FieldType = TGIS_FieldType.Date then
        _currShp.SetField( atr, XMLStringToDateTime( val ) )
      else
        _currShp.SetField( atr, val ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.addAtrToArray(
    const _atrname     : String ;
    const _atrvalue    : String
   ) ;
  begin
    addAtrToArray( _atrname, _atrvalue, False ) ;
  end ;

  procedure TGIS_LayerGML.addAtrToArray(
    const _atrname     : String ;
    const _atrvalue    : String ;
    const _useAttrName : Boolean
   ) ;
  var
    attr    : TGIS_AttributeGML ;
    i       : Integer;
    atrname : String ;
  begin
    if ( ( not IsStringEmpty( _atrvalue ) ) or _useAttrName ) and
       ( not IsStringEmpty( _atrname ) ) then begin

      atrname := _atrname ;

      if FFlattenXML then begin
        if FDepth > FDepthAttribute then
          for i := FXMLPath.Count-2 downto FDepthAttribute do
            atrname := FXMLPath[i] + '_' + atrname ;

        if FXMLFields.IndexOf( atrname ) =-1 then
          FXMLFields.Add( atrname ) ;
      end ;

      attr.Name  := atrname ;
      attr.Value := _atrvalue ;
      lAttributes.Add( attr ) ;
    end ;
  end ;

  function  TGIS_LayerGML.pointToString(
    const _point : TGIS_Point
  ) : String ;
  begin
    Result := DotFloatToStr( _point.X ) + ',' +
              DotFloatToStr( _point.Y ) ;
  end ;

  function  TGIS_LayerGML.pointToString(
    const _point : TGIS_Point3D
  ) : String ;
  begin
    Result := DotFloatToStr( _point.X ) + ',' +
              DotFloatToStr( _point.Y ) + ',' +
              DotFloatToStr( _point.Z )
  end ;


  procedure TGIS_LayerGML.pushPath(
    const _name : String
  ) ;
  begin
    FXMLPath.Add( _name ) ;
  end ;

  function TGIS_LayerGML.getFeatureLayer(
    const _element : String
  ) : Integer ;
  var
    last : String ;
  begin
    Result := 1 ;
    last := lastPathElement ;

    if (CompareText( Copy( last, length( last ) - 6 + StringFirst, 6 ), 'member'  ) = 0) or
       (CompareText( Copy( last, length( last ) - 7 + StringFirst, 7 ), 'members' ) = 0) then
       // default feature name
    else begin
      if (CompareText( last , 'dane'  ) = 0) then
         // tbd
      else if (CompareText( Copy( last, length( last ) - 6 + StringFirst, 6 ), '_layer'  ) = 0) or
              (CompareText( Copy( _element, length( _element ) - 8 + StringFirst, 8 ), '_feature' ) = 0) then
         // wms response
      else if (CompareText( Copy( _element, length( _element ) - 6 + StringFirst, 6 ), 'object'  ) = 0) then
      else begin
        Result := -1 ;
        exit ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerGML.popPath ;
  begin
    if FXMLPath.Count > 0 then
      FXMLPath.Delete( FXMLPath.Count-1 ) ;
  end ;

 function TGIS_LayerGML.lastPathElement : String ;
  begin
    if FXMLPath.Count > 0 then
      Result :=  FXMLPath[ FXMLPath.Count-1 ]
    else
      Result := '' ;
  end ;

  function  TGIS_LayerGML.pointCToString(
    const _point : TGIS_Point;
    const _yorx  : Boolean
  ) : String ;
  begin
    if _yorx then
      Result := DotFloatToStr( _point.X )
    else
      Result := DotFloatToStr( _point.Y )
  end ;

  procedure TGIS_LayerGML.writeGeometry(
    const _shp : TGIS_Shape
  ) ;
  var
    oattributes : IMXAttributes ;
    numparts    : Integer       ;
    slocalname  : String        ;
    slocalqname : String        ;
    sname       : String        ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      numparts := _shp.GetNumParts ;
      if numparts <= 0 then
        exit ;

      oattributes.Clear ;
      slocalname := GML_FEATURE_MEMBER ;
      sname      := GML_PROPERTY_MEMBER + ' fid="UID.' +
                    IntToStr( VarToInt32( _shp.GetField( GIS_FIELD_UID ) ) ) + '"';
      slocalqname:= GML_G + GML_GEOMETRY ;

      startTag( GML_NOVAL, GML_NOVAL, slocalname, oattributes ) ;
        startTag( GML_NOVAL, GML_NOVAL, sname, oattributes ) ;

        if _shp is TGIS_ShapePoint then begin
          // save point
          if FSaveFields then writeData( _shp ) ;
          startTag( GML_NOVAL, GML_NOVAL, slocalqname, oattributes ) ;
            writePoint( _shp ) ;
          endTag( GML_NOVAL, GML_NOVAL, slocalqname ) ;

        end
        else if _shp is TGIS_ShapeMultiPoint then begin
          // save multipoint
          if FSaveFields then writeData( _shp ) ;
          startTag( GML_NOVAL, GML_NOVAL, slocalqname, oattributes ) ;
            writeMultiPoint( _shp ) ;
          endTag( GML_NOVAL, GML_NOVAL, slocalqname ) ;

        end
        else if _shp is TGIS_ShapeArc then begin
          if numparts = 1 then begin
            // save arc
            if FSaveFields then writeData( _shp ) ;
            startTag( GML_NOVAL, GML_NOVAL, slocalqname, oattributes ) ;
              writeLine( _shp ) ;
            endTag( GML_NOVAL, GML_NOVAL, slocalqname ) ;

          end
          else begin
            // save multiline
            if FSaveFields then writeData( _shp ) ;
            startTag( GML_NOVAL, GML_NOVAL, slocalqname, oattributes ) ;
              writeMultiLine( _shp ) ;
            endTag( GML_NOVAL, GML_NOVAL, slocalqname ) ;

          end ;
        end
        else if (_shp is TGIS_ShapePolygon) or (_shp is TGIS_ShapeMultiPatch) then begin
          // save polygon
          if FSaveFields then writeData( _shp ) ;
          startTag( GML_NOVAL, GML_NOVAL, slocalqname, oattributes ) ;
            if _shp.GetNumParts = 1 then
              writePolygon( _shp )
            else
              writeMultiPolygon( _shp ) ;
          endTag( GML_NOVAL, GML_NOVAL, slocalqname ) ;
        end
        else
            ;// unknown object type

        sname := GML_PROPERTY_MEMBER ;
        endTag( GML_NOVAL, GML_NOVAL, sname ) ;
      endTag( GML_NOVAL, GML_NOVAL, slocalname  ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;


  // Property section is organized as below:
  // <pre>
  //   <elmProperty>
  //     <Property name="XXX">VALUE</Property>
  //   </elmProperty>
  // </pre>
  procedure TGIS_LayerGML.writeData(
    const _shp   : TGIS_Shape
  ) ;
  var
    oattributes   : SAXAttributes ;
    sname         : String    ;
    sqname        : String    ;
    snamespaceuri : String    ;
    chars         : String    ;
    fname         : String    ;
    i             : Integer   ;
    i64           : Int64     ;
    v             : Variant   ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      for i:=0 to Fields.Count-1 do begin
        if FieldInfo( i ).Deleted   then continue ;
        if FieldInfo( i ).Temporary then continue ;

        // get field name, value and save to file
        fname := GisCanonicalSQLName( FieldInfo( i ).ExportName ) ;
        sname := GML_G + fname ;

        SAXWriter.StartElement(
          snamespaceuri, sqname, sname, oattributes
        ) ;

        v := _shp.GetFieldEx( FieldInfo( i ).NewName ) ;

        if VarIsNull( v ) or VarIsEmpty( v ) then
          chars := ''
        else begin
          case FieldInfo( i ).FieldType of
            TGIS_FieldType.Date :
              begin
               {$IFDEF JAVA OR ISLAND}
                 var dt1 : TDateTime ;
                 dt1 := EncodeDateTime( 1899, 12, 31, 0, 0, 0, 0 ) ;
                 var dt2 : TDateTime ;
                 dt2 := VarToDateTime( v ) ;
               {$ENDIF}

               if {$IFDEF DCC}
                    ( VarToDateTime(0) = v )
                  {$ENDIF}
                  {$IFDEF CLR}
                    DateTime.Equals( v, EncodeDateTime( 1899, 12, 31, 0, 0, 0, 0 ) )
                  {$ENDIF}
                  {$IFDEF JAVA OR ISLAND}
                    ( dt1.CompareTo( dt2 ) = 0 )
                  {$ENDIF}
                then
                  chars := ''
                else
                  chars := DateTimeToXMLString( VarToDateTime( v ), 2, True ) ;
              end;

            TGIS_FieldType.Float :
              begin
                chars := DotFloatToStr( VarToDouble(v) ) ;
              end;

            TGIS_FieldType.Number :
              begin
                case VarType( v ) of
                  varSmallInt  : chars := IntToStr( VarToInt16( v ) ) ;
                  varInteger   : chars := IntToStr( VarToInt32( v ) ) ;
                  varInt64     : begin
                                   i64 := VarToInt64( v ) ;
                                   chars := IntToStr( i64 ) ;
                                 end ;
                  varShortInt  : chars := IntToStr( VarToSByte ( v ) ) ;
                  varWord      : chars := IntToStr( VarToUInt16( v ) ) ;
                  varLongWord  : chars := IntToStr( VarToUInt32( v ) ) ;
                  varByte      : chars := IntToStr( VarToByte  ( v ) ) ;
                  varNull      : chars := '' ;
                  else           chars := Format( '%.*f',
                                                  [ FieldInfo( i ).Decimal,
                                                    VarToDouble(v)
                                                  ]
                                                ) ;
                end
              end;
            TGIS_FieldType.Boolean :
              begin
                chars := BoolToStr( VarToBoolean( v ), True ) ;
              end;
            else
              begin
                chars := VarToString( v ) ;
              end;
          end;
        end;

        SAXWriter.Characters( chars );
        sname := GML_G + fname ;
        SAXWriter.EndElement(
          snamespaceuri, sqname, sname
        ) ;
      end ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.addChars(
    const _shp      : TGIS_Shape ;
    const _partno   : Integer    ;
    const _partsize : Integer
  ) ;
  var
    j     : Integer ;
    chars : String ;
    is3D  : Boolean ;
  begin
    is3D := _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ;

    for j := 0 to _partsize - 1 do begin
      if is3D then
        chars := pointToString( _shp.GetPoint3D( _partno, j ) )
      else
        chars := pointToString( _shp.GetPoint( _partno, j ) ) ;

      if j < ( _partsize -1 ) then
        chars := chars + ' ' ;
      SAXWriter.Characters( chars );
    end ;
  end;

  procedure TGIS_LayerGML.writeMultiPolygon(
    const _shp : TGIS_Shape
  ) ;
  var
    count         : Cardinal ;
    r, rings      : Cardinal ;
    num           : Cardinal ;
    part_no       : Cardinal ;
    int_count     : Cardinal ;
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    oattributes   : SAXAttributes ;

    function isExtContainIntPart(
      const _extPartNo : Integer ;
      const _intPartNo : Integer
    ) : Boolean ;
    var
      point_no     : Integer ;
      points_count : Integer ;
      next_pt      : Integer ;
      npar, p1     : Integer ;
      line_a       : TGIS_Point ;
      line_b       : TGIS_Point ;
      ptg1         : TGIS_Point ;
    begin
      Result := False ;
      for p1 := 0 to _shp.GetPartSize( _intPartNo )-1 do begin
        ptg1 := _shp.GetPoint( _intPartNo, p1 ) ;
        with _shp.Extent do begin
          if (XMax   = Xmin) and (YMax   = YMin) and
             (ptg1.X = Xmin) and (ptg1.X = YMin) then begin
            Result := True ;
            exit ;
          end ;
        end ;
        points_count := _shp.GetPartSize( _extPartNo ) ;
        next_pt := points_count - 1 ;
        npar := 0 ;
        for point_no:=0 to points_count-1 do  begin // all points
          line_a := _shp.GetPoint( _extPartNo, point_no ) ;
          line_b := _shp.GetPoint( _extPartNo, next_pt  ) ;
          next_pt := point_no ;
          try
            if ( ( ( ( line_a.Y <= ptg1.Y ) and ( ptg1.Y < line_b.Y ) ) or
                   ( ( line_b.Y <= ptg1.Y ) and ( ptg1.Y < line_a.Y ) )
                 ) and
                 ( ptg1.X < ( line_b.X - line_a.X ) * ( ptg1.Y - line_a.Y ) /
                            ( line_b.Y - line_a.Y ) + line_a.X
                 )
               ) then
              npar := npar + 1 ;
          except
          end ;
        end ;
        if ( npar mod 2) = 1 then
          Result := True ; // even - point is inside part
        if not Result then Break ;
      end ;
    end ;

    procedure calcBoundary( const _partNo   : Cardinal ;
                            var   _intCount : Cardinal
                          ) ;
    var
       k : Cardinal ;
    begin
      if ( _partNo > rings-1 ) then exit ;
      _intCount := 0 ;
      for k := _partNo to rings-1 do begin
        if ( k = _partNo ) then
        else if isExtContainIntPart( _partNo, k ) then begin
          inc( _intCount ) ;
        end
        else
          Break ;
      end ;
    end ;

    procedure calcCollection( var _count : Cardinal ) ;
    var
       k           : Cardinal ;
       extNo       : Cardinal ;
       newExterior : Boolean ;
    begin
      _count      := 0 ;
      extNo       := 0 ;
      newExterior := True ;
      for k := 0 to rings-1 do begin
        if newExterior then begin
          extNo := k ;
          inc( _count ) ;
          newExterior := False ;
        end
        else if isExtContainIntPart( extNo, k ) then begin
        end ;
        if k < rings-1 then
          newExterior := not isExtContainIntPart( extNo, k+1 ) ;
      end ;
    end ;

  begin
    rings  := _shp.GetNumParts ;
    calcCollection( count ) ;
    // write Polygon Geometry record
    if count = 1 then begin
      writePolygon( _shp ) ;
      exit ;
    end ;

    oattributes := CoSAXAttributes.Create ;
    try
      // start polygon section
      setAttributes( snamespaceuri, sname, sqname, GML_MULTI_POLYGON );
      SAXWriter.StartElement(
        snamespaceuri, sname, sqname, oattributes
      ) ;

      oattributes.Clear ;

      part_no := 0 ;
      for num := 1 to count do begin
        calcBoundary( part_no, int_count ) ;
        if ( int_count > 0 ) then begin
          // Polygon Geometry record with holes
          setAttributes( snamespaceuri, sname, sqname, GML_MPOLYGON_MEMBER );
          SAXWriter.StartElement(
            snamespaceuri, sname, sqname, oattributes
          ) ;
            // start polygon section
            setAttributes( snamespaceuri, sname, sqname, GML_POLYGON );
            SAXWriter.StartElement(
              snamespaceuri, sname, sqname, oattributes
            ) ;
            oattributes.Clear ;

            for r := part_no to part_no + int_count do begin
              if ( r = part_no ) then begin
                setAttributes( snamespaceuri, sname, sqname, GML_OUTERBOUND ) ;
                // start inner/outer boundary section
                SAXWriter.StartElement(
                  snamespaceuri, sname, sqname, oattributes
                ) ;

                  setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;
                  // start linearring section
                  SAXWriter.StartElement(
                    snamespaceuri, sname, sqname, oattributes
                  ) ;

                  // save coordinates
                  writeCoordinates( _shp, r ) ;

                  setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;
                  // end linearring section
                  SAXWriter.EndElement(
                    snamespaceuri, sname, sqname
                  ) ;

                setAttributes( snamespaceuri, sname, sqname, GML_OUTERBOUND ) ;
                // end inner/outer boundary section
                SAXWriter.EndElement(
                  snamespaceuri, sname, sqname
                ) ;
              end
              else if isExtContainIntPart( part_no, r ) then begin
                setAttributes( snamespaceuri, sname, sqname, GML_INNERBOUND ) ;
                // start inner/outer boundary section
                SAXWriter.StartElement(
                  snamespaceuri, sname, sqname, oattributes
                ) ;

                  setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;
                  // start linearring section
                  SAXWriter.StartElement(
                    snamespaceuri, sname, sqname, oattributes
                  ) ;

                  // save coordinates
                  writeCoordinates( _shp, r ) ;

                  setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;
                  // end linearring section
                  SAXWriter.EndElement(
                    snamespaceuri, sname, sqname
                  ) ;

                setAttributes( snamespaceuri, sname, sqname, GML_INNERBOUND ) ;
                // end inner/outer boundary section
                SAXWriter.EndElement(
                  snamespaceuri, sname, sqname
                ) ;
              end ;
            end ;

            // end polygon section
            setAttributes( snamespaceuri, sname, sqname, GML_POLYGON ) ;
            SAXWriter.EndElement(
              snamespaceuri, sname, sqname
            ) ;

          setAttributes( snamespaceuri, sname, sqname, GML_MPOLYGON_MEMBER ) ;
          SAXWriter.EndElement(
            snamespaceuri, sname, sqname
          ) ;
        end
        else begin
          // Polygon Geometry record without holes
          setAttributes( snamespaceuri, sname, sqname, GML_MPOLYGON_MEMBER );
          SAXWriter.StartElement(
            snamespaceuri, sname, sqname, oattributes
          ) ;

            // start polygon section
            setAttributes( snamespaceuri, sname, sqname, GML_POLYGON );
            SAXWriter.StartElement(
              snamespaceuri, sname, sqname, oattributes
            ) ;
            oattributes.Clear ;

            setAttributes( snamespaceuri, sname, sqname, GML_OUTERBOUND ) ;
            // start inner/outer boundary section
            SAXWriter.StartElement(
              snamespaceuri, sname, sqname, oattributes
            ) ;

              setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;
              // start linearring section
              SAXWriter.StartElement(
                snamespaceuri, sname, sqname, oattributes
              ) ;

              // save coordinates
              writeCoordinates( _shp, part_no ) ;

              setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;
              // end linearring section
              SAXWriter.EndElement(
                snamespaceuri, sname, sqname
              ) ;

            setAttributes( snamespaceuri, sname, sqname, GML_OUTERBOUND ) ;
            // end inner/outer boundary section
            SAXWriter.EndElement(
              snamespaceuri, sname, sqname
            ) ;

            // end polygon section
            setAttributes( snamespaceuri, sname, sqname, GML_POLYGON ) ;
            SAXWriter.EndElement(
              snamespaceuri, sname, sqname
            ) ;

          setAttributes( snamespaceuri, sname, sqname, GML_MPOLYGON_MEMBER ) ;
          SAXWriter.EndElement(
            snamespaceuri, sname, sqname
          ) ;
        end ;
        part_no := part_no + int_count + 1 ;
      end ;

      // end polygon section
      setAttributes( snamespaceuri, sname, sqname, GML_MULTI_POLYGON ) ;
      SAXWriter.EndElement(
        snamespaceuri, sname, sqname
      ) ;
    finally
      FreeObject( oattributes ) ;
    end ;

  end ;

  procedure TGIS_LayerGML.writePolygon(
    const _shp : TGIS_Shape
  ) ;
  var
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    oattributes   : SAXAttributes ;
    numparts      : Integer ;
    i             : Integer ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      // start polygon section
      setAttributes( snamespaceuri, sname, sqname, GML_POLYGON );
      SAXWriter.StartElement(
        snamespaceuri, sname, sqname, oattributes
      ) ;

      oattributes.Clear ;

      numparts := _shp.GetNumParts ;
      for i := 0 to numparts - 1 do begin
        if i = 0 then
          setAttributes( snamespaceuri, sname, sqname, GML_OUTERBOUND )
        else
          setAttributes( snamespaceuri, sname, sqname, GML_INNERBOUND ) ;

        // start inner/outer boundary section
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;
        setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;

        // start linearring section
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;

        // save coordinates
        writeCoordinates( _shp, i ) ;
        setAttributes( snamespaceuri, sname, sqname, GML_LINEAR_RING ) ;

        // end linearring section
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        ) ;

        if i = 0 then
          setAttributes( snamespaceuri, sname, sqname, GML_OUTERBOUND )
        else
          setAttributes( snamespaceuri, sname, sqname, GML_INNERBOUND ) ;

        // end inner/outer boundary section
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        ) ;
      end ;

      // end polygon section
      setAttributes( snamespaceuri, sname, sqname, GML_POLYGON ) ;
      SAXWriter.EndElement(
        snamespaceuri, sname, sqname
      ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.writePoint(
    const _shp    : TGIS_Shape
  ) ;
  begin
    writePoint( _shp, 0 ) ;
  end ;

  procedure TGIS_LayerGML.writePoint(
    const _shp    : TGIS_Shape ;
    const _partno : Integer
  ) ;
  var
    oattributes : SAXAttributes ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      // save point
      startTag( GML_NOVAL, GML_NOVAL, GML_POINT, oattributes ) ;
      writeCoordinates( _shp, _partno ) ;
      endTag( GML_NOVAL, GML_NOVAL, GML_POINT ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.writeMultiPoint(
    const _shp : TGIS_Shape
  ) ;
  var
    oattributes : SAXAttributes ;
    numparts    : Integer ;
    i           : Integer ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      startTag( GML_NOVAL, GML_NOVAL, GML_MULTI_POINT, oattributes) ;

      numparts := _shp.GetNumParts ;
      for i := 0 to numparts - 1 do begin
        // save points
        startTag( GML_NOVAL, GML_NOVAL, GML_POINT_MEMBER, nil ) ;
        writePoint( _shp, i ) ;
        endTag( GML_NOVAL, GML_NOVAL, GML_POINT_MEMBER ) ;
      end ;

      endTag( GML_NOVAL, GML_NOVAL, GML_MULTI_POINT ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.writeLine(
    const _shp    : TGIS_Shape
  ) ;
  begin
    writeLine( _shp, 0 ) ;
  end ;

  procedure TGIS_LayerGML.writeLine(
    const _shp    : TGIS_Shape ;
    const _partno : Integer
  ) ;
  var
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    oattributes   : SAXAttributes ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      // save line
      setAttributes( snamespaceuri, sname, sqname, GML_LINE_STRING ) ;
      SAXWriter.StartElement(
        snamespaceuri, sname, sqname, oattributes
      ) ;
      writeCoordinates( _shp, _partno ) ;
      SAXWriter.EndElement(
        snamespaceuri, sname, sqname
      ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.writeMultiLine(
    const  _shp : TGIS_Shape
  )  ;
  var
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    oattributes   : SAXAttributes ;
    numparts      : Integer ;
    i             : Integer ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      // start multiline section
      setAttributes( snamespaceuri, sname, sqname, GML_MULTI_LINE );
      SAXWriter.StartElement(
        snamespaceuri, sname, sqname, oattributes
      ) ;

      numparts := _shp.GetNumParts ;
      for  i := 0  to  numparts - 1 do begin
        // save line section
        setAttributes( snamespaceuri, sname, sqname, GML_LINE_MEMBER );
        oattributes.Clear ;
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;
        writeLine( _shp, i  );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        ) ;
      end ;

      // end multiline section
      setAttributes( snamespaceuri, sname, sqname, GML_MULTI_LINE ) ;
      SAXWriter.EndElement( snamespaceuri, sname, sqname ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.writeBox(
    const _extent : TGIS_Extent
  ) ;
  var
    sname          : String ;
    sqname         : String ;
    chars          : String ;
    snamespaceuri  : String ;
    oattributes    : SAXAttributes;
  begin
    oattributes := CoSAXAttributes.Create;
    try
      // start BoundedBy and Box section
      setAttributes( snamespaceuri, sname, sqname, GML_BOUNDED_BY ) ;
      SAXWriter.StartElement(
        snamespaceuri, sname, sqname, oattributes
      ) ;
      setAttributes( snamespaceuri, sname, sqname, GML_BOX );

      oattributes.Clear ;
      if Self.CS.EPSG > 0 then
        oattributes.AddAttribute( snamespaceuri, GML_SRS_NAME, GML_SRS_NAME,
                                  'CDATA', Format( 'EPSG:%d',[Self.CS.EPSG] )
                                 ) ;
      SAXWriter.StartElement(
        snamespaceuri, sname, sqname, oattributes
      ) ;
      oattributes.Clear ;

      // for long coord format
      if FUseLongCoord then begin
        if FUseOldStyleCoord then
          setAttributes( snamespaceuri, sname, sqname, GML_COORDINATES )
        else
          setAttributes( snamespaceuri, sname, sqname, GML_COORDINATES3 );

        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;

        chars := pointToString( GisPoint( _extent.XMin, _extent.YMin ) ) + ' ' +
                 pointToString( GisPoint( _extent.XMax, _extent.YMax ) ) ;
        SAXWriter.Characters( chars ) ;
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        ) ;
      end
      else begin
        // XYMin section
        setAttributes( snamespaceuri, sname, sqname, GML_COORD ) ;
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;

        setAttributes( snamespaceuri, sname, sqname, GML_COORD_X );
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;
        chars := pointCToString( GisPoint( _extent.XMin, _extent.YMin ), True );
        SAXWriter.Characters( chars );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        );

        setAttributes( snamespaceuri, sname, sqname, GML_COORD_Y );
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;
        chars := pointCToString( GisPoint( _extent.XMin, _extent.YMin ), False );
        SAXWriter.Characters( chars );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
         );

        setAttributes( snamespaceuri, sname, sqname, GML_COORD );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        );

        // XYMax section
        setAttributes( snamespaceuri, sname, sqname, GML_COORD );
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;

        setAttributes( snamespaceuri, sname, sqname, GML_COORD_X);
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;
        chars := pointCToString( GisPoint( _extent.XMax, _extent.YMax ), True );
        SAXWriter.Characters( chars );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
         );

        setAttributes( snamespaceuri, sname, sqname, GML_COORD_Y );
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;
        chars := pointCToString( GisPoint( _extent.XMax, _extent.YMax ), False );
        SAXWriter.Characters( chars );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
         );

        setAttributes( snamespaceuri, sname, sqname, GML_COORD );
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        );
      end ;

      // end BoundedBy and Box section
      setAttributes( snamespaceuri, sname, sqname, GML_BOX );
      SAXWriter.EndElement(
        snamespaceuri, sname, sqname
      ) ;
      setAttributes( snamespaceuri, sname, sqname, GML_BOUNDED_BY );
      SAXWriter.EndElement(
        snamespaceuri, sname, sqname
      ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.writeCoordinates(
    const _shp    : TGIS_Shape ;
    const _partno : Integer
  ) ;
  var
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    chars         : String ;
    oattributes   : SAXAttributes ;
    partsize      : Integer ;
    j             : Integer ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      // for long format coordinates write full block
      if FUseLongCoord then begin
        // start coord section
        if FUseOldStyleCoord then
          setAttributes( snamespaceuri, sname, sqname, GML_COORDINATES )
        else
          setAttributes( snamespaceuri, sname, sqname, GML_COORDINATES3 );

        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes
        ) ;

        // save coordinates
        partsize := _shp.GetPartSize( _partno ) ;
        addChars( _shp, _partno, partsize ) ;

        // end coord section
        SAXWriter.EndElement(
          snamespaceuri, sname, sqname
        );
      end
      else begin
        // for short format coordinates write all blocks
        partsize := _shp.GetPartSize( _partno ) ;
        for j := 0 to partsize - 1 do begin
          // start coord section
          setAttributes( snamespaceuri, sname, sqname, GML_COORD );
          SAXWriter.StartElement(
            snamespaceuri, sname, sqname, oattributes
          ) ;

          // X section
          setAttributes( snamespaceuri, sname, sqname, GML_COORD_X );
          SAXWriter.StartElement(
            snamespaceuri, sname, sqname, oattributes
          ) ;
          chars := pointCToString( _shp.GetPoint( _partno, j ), True );
          SAXWriter.Characters( chars );
          SAXWriter.EndElement(
            snamespaceuri, sname, sqname
          ) ;

          // Y Section
          setAttributes( snamespaceuri, sname, sqname, GML_COORD_Y );
          SAXWriter.StartElement(
            snamespaceuri, sname, sqname, oattributes
          ) ;
          chars := pointCToString( _shp.GetPoint( _partno, j ), False );
          SAXWriter.Characters( chars );
          SAXWriter.EndElement(
            snamespaceuri, sname, sqname
          ) ;

          // end coord section
          setAttributes( snamespaceuri, sname, sqname, GML_COORD );
          SAXWriter.EndElement(
            snamespaceuri, sname, sqname
          ) ;
        end ;
      end ;
    finally
      FreeObject( oattributes ) ;
    end;
  end ;

  procedure TGIS_LayerGML.setAttributes(
    var   _namespaceuri : String ;
    var   _localname    : String ;
    var   _qname        : String ;
    const _val          : String
  ) ;
  begin
    _namespaceuri := GML_NOVAL    ;
    _localname    := GML_NOVAL    ;
    _qname        := _val ;
  end ;

  procedure TGIS_LayerGML.startTag(
    const _namespaceuri : String ;
    const _localname    : String ;
    const _qname        : String ;
    const _attributes   : SAXAttributes
  ) ;
  var
    sqname        : String ;
    snamespaceuri : String ;
    slocalname    : String ;
  begin
    snamespaceuri := _namespaceuri ;
    slocalname    := _localname    ;
    sqname        := _qname        ;

    // save StartElement node
    SAXWriter.StartElement( snamespaceuri,
                            slocalname,
                            sqname,
                            _attributes
                          ) ;
  end ;

  procedure TGIS_LayerGML.endTag(
    const _namespaceuri : String ;
    const _localname    : String ;
    const _qname        : String
  ) ;
  var
    sqname        : String ;
    snamespaceuri : String ;
    slocalname    : String ;
  begin
    slocalname    := _localname    ;
    snamespaceuri := _namespaceuri ;
    sqname        := _qname        ;

    // save EndElement node
    SAXWriter.EndElement(
      snamespaceuri, slocalname, sqname
    ) ;
  end ;

  function TGIS_LayerGML.getSchemaPath(
    const _path : String
  ) : String ;
  begin
    Result := GetPathNoExt( _path ) + '.xsd' ;
  end ;

  function TGIS_LayerGML.isFeature(
    const _name : String ;
    const _full : String
   ) : Boolean ;
  var
    str  : String ;
    last : String ;
  begin
    Result := _full = GML_FEATURE_MEMBER ;

    if not Result then begin
      Result := (FFeatures.IndexOf( _name ) >= 0) or (FFeatures.IndexOf( _full ) >= 0);

      if not Result then begin
        str := Copy( _name, length( _name ) - 6 + StringFirst, 6 ) ;

        Result := (CompareText( str, GML_FEATURE_PREFIX ) = 0) and
                  ( _name <> GML_GEOMETRY_MEMBER ) and
                  ( _name <> GML_POLYGON_MEMBER  ) and
                  ( ( _name <> GML_LINE_MEMBER   ) and not addingMLine ) and
                  ( _name <> GML_CURVE_MEMBER    ) and
                  ( _name <> GML_SURFACE_MEMBER  ) and
                  ( _name <> GML_POINT_MEMBER    ) and
                  ( _name <> 'appearanceMember'  ) and
                  ( _name <> 'surfaceDataMember' ) ;

        if not Result then
          Result := CompareText( Copy( _name, length(_name)-7+StringFirst, 7 ),
                                 GML_FEATURE_POSTFIX ) = 0 ;
        if not Result then begin
          last   := lastPathElement ;
          Result := CompareText( Copy( last, length( last ) - 7 + StringFirst, 7 ),
                                 GML_FEATURES_PREFIX ) = 0 ;
          if not Result then
            Result := last = 'Dataset'
        end ;

        if not Result then
          Result := _name = 'Dane'
      end ;
    end ;
  end ;

  function TGIS_LayerGML.isGeometry(
    const _name : String
  ) : Boolean ;
  var
    gname : String ;
  begin
    if hasGeomPrefix then begin
      if hasPrefix then
        gname := _name
      else
        gname := GML + _name ;

      Result := ( gname = GML_POINT          ) or
                ( gname = GML_MULTI_POINT    ) or
                ( gname = GML_LINE_STRING    ) or
                ( gname = GML_MULTI_LINE     ) or
                ( gname = GML_POLYGON        ) or
                ( gname = GML_MULTI_POLYGON  ) or
                ( gname = GML_MULTI_CURVE    ) or
                ( gname = GML_CURVE          ) or
                ( gname = GML_MULTI_SURFACE  ) or
                ( gname = GML_SURFACE        ) or
                ( gname = GML_CSURFACE       ) or
                ( gname = GML_COMPOSITE_CURVE) or
                ( gname = GML_LINEAR_RING    )
    end
    else
      Result := False ;
  end ;

  function TGIS_LayerGML.isMultiGeometry(
    const _name : String
  ) : Boolean ;
  begin
    if hasGeomPrefix then begin
      if hasPrefix then
        Result := ( _name = GML_MULTI_GEOMETRY )
      else
        Result := ( (GML + _name) = GML_MULTI_GEOMETRY )
    end
    else
      Result := False ;
  end ;

  procedure TGIS_LayerGML.prepareGeometry(
    const _name : String
  ) ;
  begin
    if ( _name = GML_MULTI_POINT ) then begin
      addingMPoint := True ;
      addMultiPoint ;
    end
    else if ( _name = GML_POINT ) then begin
      if not addingMPoint then begin
        addingPoint := True ;
        addPoint ;
      end
      else
        addShapePart ;
    end
    else if ( _name = GML_MULTI_LINE ) then begin
      addingMLine := True ;
      addLine;
    end
    else if ( _name = GML_LINE_STRING ) then begin
      if not addingMLine and not addingMCurve and not addingCCurve and ( not addingSurface ) and ( not addingMSurface ) then begin
          addingLine := True;
          addLine;
      end
      else if not addingCCurve and ( not addingSurface ) and ( not addingMSurface ) then
        addShapePart;
    end
    else if ( _name = GML_ARC ) then begin
      addingArc := True;
    end
    else if ( _name = GML_CIRCLE_CENTER ) then begin
      addingCircle := True;
    end
    else if ( _name = GML_MULTI_SURFACE ) then begin
      addingMSurface := True ;
      addPolygon;
    end
    else if ( _name = GML_SURFACE ) then begin
      if not addingMSurface then begin
        addingSurface := True;
        addPolygon;
      end
      else
        addShapePart ;
    end
    else if ( _name = GML_SOLID) then
      addingSolid := True
    else if ( _name = GML_STRIANGLE) and addingSolid then begin
      addingSTriangle := True ;
      addPolygon;
    end
    else if ( _name = GML_STRIANGLE) then begin
      addingSTriangle := True ;
      addPolygon;
    end
    else if ( _name = GML_CSURFACE ) and not addingMSurface then begin
      addingSurface := True;
      addPolygon;
    end
    else if ( _name = GML_CURVE ) then begin
      if not addingSurface and not addingMSurface and
         ( not addingPolygon or finishedPolygon ) and
         not addingMCurve  and
         not addingCCurve then
      begin
        addingLine := True;
        addLine;
        addShapePart;
      end
      else if not addingSurface and not addingCCurve then
        addShapePart ;
    end
    else if ( _name = GML_MULTI_POLYGON ) then begin
      addingMPolygon := True ;
      addPolygon;
    end
    else if ( _name = GML_MULTI_CURVE ) then begin
      addingMCurve := True ;
      addLine;
    end
    else if ( _name = GML_COMPOSITE_CURVE ) then begin
      if ( not addingPolygon ) and
         ( not addingMPolygon ) and ( not addingSurface ) and ( not addingMSurface ) then
      begin
        addingCCurve := True ;
        addLine;
      end
      else
        addingCCurve := True ;
    end
    else if ( _name = GML_POLYGON ) then begin
      if not addingMPolygon and not addingMSurface and not addingSurface then begin
          addingPolygon := True;
          addPolygon;
      end
      else
        addShapePart;
    end
    else if ( _name = GML_TRIANGLE ) and addingSTriangle then begin
      addShapePart;
    end
    else if ( _name = GML_LINEAR_RING ) and ( not addingPolygon ) and
            ( not addingMPolygon ) and ( not addingSurface ) and ( not addingMSurface ) and
            ( not addingSTriangle )
             then begin
      addingRing := True;
      addPolygon;
    end
    else if ( _name = GML_INNERBOUND ) or ( _name = GML_INTERIOR ) or
            ( _name = GML_OUTERBOUND ) or
            ( (_name = GML_EXTERIOR) and not (addingSurface or addingMSurface or addingSolid or addingMPolygon )) then begin
      addShapePart;
    end
    else if ( _name = GML_COORDINATES  ) or ( _name = GML_COORDINATES2 ) or
            ( _name = GML_COORDINATES3 ) or ( _name = GML_COORD        ) then
    begin
      if (addingPolygon or addingLine or addingPoint or addingMPoint or
         addingMLine or addingArc or addingCircle or addingRing or addingMPolygon or
         addingSurface or addingMSurface or addingMCurve or addingCCurve or addingSTriangle ) then
        addingCoords := True ;

      usingCoord := _name = GML_COORD ;
      usingPos   := (_name = GML_COORDINATES2) ;
    end ;
  end ;

  procedure TGIS_LayerGML.unprepareGeometry ;
  var
    i, j            : Integer ;
    ignore_feature  : Boolean ;
    ishp            : TGIS_Shape ;
  begin
    if not addingMultiGeo then begin
      addingMember    := False ;
      addingLine      := False ;
      addingArc       := False ;
      addingCircle    := False ;
      addingMLine     := False ;
      addingPolygon   := False ;
      finishedPolygon := False ;
      addingRing      := False ;
      addingMPolygon  := False ;
      addingPoint     := False ;
      addingMPoint    := False ;
      addingObject    := False ;
      addingMSurface  := False ;
      addingMCurve    := False ;
      addingCCurve    := False ;
      addingSurface   := False ;
      addingSTriangle := False ;
    end ;

    ignore_feature := False ;
    if isFilterByLayer then begin
      ignore_feature := curFeatureLayer <> filterLayerName ;
    end ;

    if not ignore_feature and not isPrescanMode then begin
      if assigned( currShp ) then begin

        // if any other type found
        if usingCoord then begin
          if currShp.IsEmpty and ( currShp.GetNumParts = 0 ) and (currShp.GetNumPoints = 0) then
            currShp.AddPart ;
          for i := 0 to ptgList.Count - 1 do
            currShp.AddPoint3D( ptgList.Items[ i ] );

          usingCoord := false;
          ptgList.Clear;
        end ;
      end ;

      if isCityGML then begin
        if shpList.Count >= 1 then begin
          currShp := TGIS_Shape( shpList[ 0 ] ) ;

          ignore_feature := False ;
          if filterShapeType <> TGIS_ShapeType.Unknown then
            ignore_feature := (filterShapeType <> currShp.ShapeType) ;

          if not ignore_feature then begin
            currShp.Unlock;
            if Items.Count = 1 then
              Extent := currShp.Extent ;

            if currShp.IsEmpty then
              currShp.Reset ;
            // add attributes to object
            for i := 0 to lAttributes.Count - 1 do
              addAtributesToShape( currShp, lAttributes[i].Name, lAttributes[i].Value ) ;
          end
          else
            Revert( currShp.Uid ) ;

          for j := 1 to shpList.Count-1 do begin
            ishp := TGIS_Shape( shpList[ j ] ) ;

            if cityGMLSplit then begin
              ishp.Unlock ;

              for i := 0 to lAttributes.Count - 1 do
                addAtributesToShape( ishp, lAttributes[i].Name, lAttributes[i].Value ) ;
            end
            else begin
              currShp.AppendGeometry( ishp ) ;

              Revert( ishp.Uid ) ;
            end
          end
        end
      end
      else begin
        for j := 0 to shpList.Count-1 do begin
          currShp := TGIS_Shape( shpList[ j ] ) ;

          ignore_feature := False ;
          if filterShapeType <> TGIS_ShapeType.Unknown then
            ignore_feature := (filterShapeType <> currShp.ShapeType) ;

          if not ignore_feature then begin
            currShp.Unlock;
            if Items.Count = 1 then
              Extent := currShp.Extent ;

            // add attributes to object
            for i := 0 to lAttributes.Count - 1 do
              addAtributesToShape( currShp, lAttributes[i].Name, lAttributes[i].Value ) ;
          end
          else
            Revert( currShp.Uid ) ;
        end ;
      end ;
    end
    else begin
      if not isPrescanMode then begin
        for j := 0 to shpList.Count-1 do begin
          currShp := TGIS_Shape( shpList[ j ] ) ;
          Revert( currShp.Uid ) ;
        end ;
      end
      else begin
        for j := 0 to prescanTypeList.Count-1 do
          T_MultiValueDictionary(prescanLayers).Add(
            curFeatureLayer,
            prescanTypeList[j]
          ) ;
      end;
    end ;

    if not addingMultiGeo then begin
      lAttributes.Clear ;
      FXMLFields.Clear ;
    end ;

    shpList.Clear ;
    currShp := nil ;
    curFeatureLayer := '' ;
    prescanTypeList.Clear ;
    bSrsDimUsed := False ;
  end ;

  function TGIS_LayerGML.isCityGmlAttribute(
    const _name : String
  ) : Boolean ;
  begin
    Result := (_name = 'stringAttribute') or (_name = 'intAttribute') or
              (_name = 'doubleAttribute') or (_name = 'measureAttribute') ;
  end ;

  function TGIS_LayerGML.isCityGmlFeature(
    const _name : String ;
      var _lod  : Integer
  ) : Boolean ;
  begin
    if (_name = 'lod0FootPrint') or (_name = 'lod0RoofEdge') or
       (_name = 'GenericCityObject') or (_name = 'WaterBody') or
       (_name = 'ReliefFeature') or (_name = 'Address') or
       (_name = 'BreaklineRelief') or (_name = 'Road') then begin
      Result := True ;
      _lod   := 1 ;
    end
    else if (_name = 'lod1Solid') or (_name = 'lod1TerrainIntersection') or
            (_name = 'lod2TerrainIntersection') then begin
      Result := True ;
      _lod   := 2 ;
    end
    else if (_name = 'RoofSurface'  ) or (_name = 'WallSurface') or
            (_name = 'GroundSurface') or (_name = 'FloorSurface') or
            (_name = 'BuildingInstallation') or (_name = 'Building') then begin
      Result := True ;
      _lod   := 3 ;
    end
    else if (_name = 'Window') or (_name = 'Door') then begin
      Result := True ;
      _lod   := 4 ;
    end
    else if (_name = 'CityFurniture') or (_name = 'Railway') or
            (_name = 'Bridge') or (_name = 'Tunnel')  or (_name = 'LandUse') or
            (_name = 'SolitaryVegetationObject') then begin
      Result := True ;
      _lod   := 4 ;
    end
    else if (_name = 'InteriorWallSurface') or (_name = 'CeilingSurface') or
            (_name = 'ClosureSurface') or (_name = 'OuterCeilingSurface')  or
            (_name = 'OuterFloorSurface')  then begin
      Result := True ;
      _lod   := 5 ;
    end
    else if (_name = 'BuildingFurniture') or (_name = 'IntBuildingInstallation') then begin
      Result := True ;
      _lod   := 6 ;
    end
    else begin
      Result := False ;
      _lod   := -1 ;
    end;
  end ;

  procedure TGIS_LayerGML.writeSchema(
    const _path : String
  ) ;
  const
    schemaDef =
     '<?xml version="1.0" encoding="UTF-8"?>'+
     '<xsd:schema targetNamespace="http://www.tatukgis.com/GML"'+
     ' xmlns:g="http://www.tatukgis.com/GML"'+
     ' xmlns:gml="http://www.opengis.net/gml"'+
     ' xmlns:xsd="http://www.w3.org/2001/XMLSchema" '+
     'elementFormDefault="qualified" version="1.0">'+
     '<xsd:import namespace="http://www.opengis.net/gml" '+
     'schemaLocation="feature.xsd"/>'+
     '<xsd:element name="TATUKGIS" type="g:TATUKGISType" '+
     'substitutionGroup="gml:_FeatureCollection"/>'+
     '<xsd:element name="PROPERTIES" type="g:PROPERTIESType" '+
     'substitutionGroup="gml:_Feature"/>'+
     '<xsd:complexType name="TATUKGISType"><xsd:complexContent>'+
     '<xsd:extension base="gml:AbstractFeatureCollectionType"/>'+
     '</xsd:complexContent></xsd:complexType>'+
     '<xsd:complexType name="PROPERTIESType"><xsd:complexContent>'+
     '<xsd:extension base="gml:AbstractFeatureType"><xsd:sequence>'+
     '%s'+
     '<xsd:element name="GEOMETRY" type="gml:GeometryPropertyType"'+
     ' nillable="true" minOccurs="1" maxOccurs="1" /></xsd:sequence>'+
     '</xsd:extension></xsd:complexContent></xsd:complexType></xsd:schema>';
    attribDef =
     '<xsd:element name="%s" type="xsd:%s" nillable="true" minOccurs="1" maxOccurs="1"/>';
  var
    strm   : TXMLOutputStream ;
    wrtr   : TXMLStreamWriter ;
    schema : String ;
    fname  : String ;
    attrib : String ;
    i      : Integer ;

    function getType( const _type : TGIS_FieldType ) : String;
    begin
      case _type of
        TGIS_FieldType.String  : Result := 'string';
        TGIS_FieldType.Number  : Result := 'decimal';
        TGIS_FieldType.Float   : Result := 'float';
        TGIS_FieldType.Boolean : Result := 'boolean';
        TGIS_FieldType.Date    : Result := 'date';
      end ;
    end ;

  begin
    attrib := '';
    try
      {$IFDEF DCC}
        strm := TFileStream.Create( _path, fmCreate ) ;
      {$ENDIF}
      {$IFDEF CLR}
        strm := TFileStream.Create( _path, FileMode.&Create, FileAccess.Write ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        strm := java.io.FileOutputStream.Create( _path ) ;
      {$ENDIF}
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), _path,
                                   GetLastError
                                  ) ;
    end ;

    {$IFDEF JAVA}
      wrtr := TXMLStreamWriter.Create( strm, 'UTF-8' ) ;
    {$ELSE}
      wrtr := TXMLStreamWriter.Create( strm, TEncoding.UTF8 ) ;
    {$ENDIF}
    try
      if FSaveFields then
        for i := 0 to Fields.Count - 1 do begin
          if FieldInfo( i ).Deleted   then continue ;
          if FieldInfo( i ).Temporary then continue ;

          fname := GisCanonicalSQLName( FieldInfo( i ).ExportName ) ;
          attrib := attrib + Format( attribDef,
                                    [fname, getType( FieldInfo( i ).FieldType )]
                                    ) ;
        end ;

      schema := Format( schemaDef, [ attrib ] ) ;

      wrtr.Write( schema ) ;
    finally
      {$IFDEF JAVA}
        wrtr.close ;
        strm.close ;
      {$ENDIF}
      FreeObject( wrtr ) ;
      FreeObject( strm ) ;
    end ;
  end ;

  procedure TGIS_LayerGML.parseSchema(
    const _schemadef : IXMLNode
  ) ;
  var
    k     : Integer ;
    fname : String ;

    procedure parse_feature_elements;
    var
      absLst  : TStringList ;
      feaLst  : TStringList ;
      subGR   : String ;
      def     : IXMLNode ;
      i, j    : Integer ;
    begin
      absLst := TStringList.Create ;
      feaLst := TStringList.Create ;
      try
        for i := 0 to _schemadef.ChildNodes.Count - 1 do begin
          def := _schemadef.ChildNodes[ i ] ;
          if def.LocalName <> 'element' then continue ;

          if def.HasAttribute( 'substitutionGroup' ) then begin
            subGR := VarToString( def.Attributes[ 'substitutionGroup' ] );
            if (subGR = 'gml:_Feature') or (subGR = 'gml:AbstractFeature') then begin
              absLst.Add( VarToString( def.Attributes['name'] ) ) ;
            end ;
          end ;
        end ;

        for i := 0 to _schemadef.ChildNodes.Count - 1 do begin
          def := _schemadef.ChildNodes[ i ] ;
          if def.LocalName <> 'element' then continue ;

          if def.HasAttribute( 'substitutionGroup' ) then begin
            subGR := VarToString(def.Attributes[ 'substitutionGroup' ]) ;
            for j := 0 to absLst.Count - 1 do begin
              if Pos( absLst[ j ], subGR ) > 0 then
                feaLst.Add( VarToString(def.Attributes['name']) ) ;
            end ;
          end ;
        end ;

        FFeatures.AddStrings( absLst );
        FFeatures.AddStrings( feaLst );
      finally
        FreeObject( absLst ) ;
        FreeObject( feaLst ) ;
      end ;
    end ;

    function ends_with( const _val : String ; const _postfix : String ) : Boolean ;
    begin
      Result := Pos( _postfix, _val ) >= StringFirst ;
    end ;

    function parse_field_element( const _node : IXMLNode ) : Boolean ;
    var
      fdef    : T_gmlFieldDef ;
      stype   : String ;
      bignore : Boolean ;
    begin
      Result := False ;

      if not assigned( _node ) then exit;
      if _node.LocalName <> 'element' then exit ;

      fdef._name := VarToString( _node.Attributes['name'] ) ;
      fdef._type := TGIS_FieldType.String ;
      stype := VarToString( _node.Attributes['type'] ) ;
      if IsStringEmpty( stype ) then
        stype := VarToString( _node.ChildNodes['simpleType'].ChildNodes['restriction'].Attributes['base'] ) ;

      bignore := False ;
      if ends_with( stype, 'string' ) then
        fdef._type := TGIS_FieldType.String
      else
      if ends_with( stype, 'integer' ) or
         ends_with( stype, 'decimal' ) or
         ends_with( stype, 'long   ' ) then
        fdef._type := TGIS_FieldType.Number
      else
      if ends_with( stype, 'boolean' ) then
        fdef._type := TGIS_FieldType.Boolean
      else
      if ends_with( stype, 'float'  ) or
         ends_with( stype, 'double' ) then
        fdef._type := TGIS_FieldType.Float
      else
      if ends_with( stype, 'date' ) then
        fdef._type := TGIS_FieldType.Date
      else
        bignore := True ;

      if not bignore then
        TList<T_gmlFieldDef>( FSchemaTypes ).Add( fdef ) ;
    end ;

    function extract_field_types : Boolean ;
    var
      ctp    : IXMLNode ;
      elm    : IXMLNode ;
      seq    : IXMLNode ;
      ii,j   : Integer ;
    begin
      for ii := 0 to _schemadef.ChildNodes.Count - 1 do begin
        ctp := _schemadef.ChildNodes[ii] ;
        if ctp.LocalName <> 'complexType' then continue ;

        seq := ctp.ChildNodes['complexContent'].ChildNodes['extension'].ChildNodes['sequence'] ;
        if not assigned( seq ) then continue ;

        for j := 0 to seq.ChildNodes.Count-1 do begin
          elm := seq.ChildNodes[j] ;
          parse_field_element( elm ) ;
        end ;
      end;

      Result := True ;
    end ;

  begin
    parse_feature_elements ;
    extract_field_types ;

    if not FlattenXML then
      for k := 0 to TList<T_gmlFieldDef>( FSchemaTypes ).Count - 1 do begin
        fname := TList<T_gmlFieldDef>( FSchemaTypes )[k]._name ;
        if FindField( fname ) > -1 then continue ;

        case TList<T_gmlFieldDef>( FSchemaTypes )[k]._type of
          TGIS_FieldType.String  : AddFieldInternal( fname, TGIS_FieldType.String , 1 , 0 ) ;
          TGIS_FieldType.Number  : AddFieldInternal( fname, TGIS_FieldType.Number , 10, 0 ) ;
          TGIS_FieldType.Float   : AddFieldInternal( fname, TGIS_FieldType.Float  , 10, 7 ) ;
          TGIS_FieldType.Boolean : AddFieldInternal( fname, TGIS_FieldType.Boolean, 1 , 0 ) ;
          TGIS_FieldType.Date    : AddFieldInternal( fname, TGIS_FieldType.Date   , 8 , 0 ) ;
          else                     AddFieldInternal( fname, TGIS_FieldType.String , 1 , 0 ) ;
        end ;
      end ;
  end ;

  procedure TGIS_LayerGML.parseSchemaFile(
    const _path : String
  ) ;
  var
    doc       : TGIS_XMLDocument ;
    spath     : String ;
    tpath     : String ;
    tkn       : TGIS_Tokenizer ;
    i         : Integer ;
    schemadef : IXMLNode ;
  begin
    inc( schemaChecks ) ;
    if schemaChecks > 2 then exit ;

    try
      spath := getSchemaPath( _path ) ;

      if foundSchema then begin
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( schemaPath, [' '] );
          for i := 0 to tkn.Result.Count - 1 do begin
            tpath := GetPathAbsolute( ExtractFileDir( Path ), tkn.Result[ i ] ) ;
            if ( GetFileExt( tpath ) = '.xsd' ) and
               SafeFileExists( tpath ) then begin
              spath := tpath ;
            end
          end ;
        finally
          FreeObject( tkn );
        end ;
      end ;

      if not SafeFileExists( spath ) then exit ;

      doc := TGIS_XMLDocument.Create ;
      try
        doc.LoadFromFile( spath ) ;

        schemadef := doc.DocumentElement ;
        if not assigned( schemadef ) then exit ;

        if schemadef.LocalName <> 'schema' then
          schemadef := schemadef.ChildNodes.FindNode( 'xsd:schema' ) ;
        if not assigned( schemadef ) then
          exit ;

        parseSchema( schemadef ) ;
      finally
        FreeObject( doc ) ;
      end ;
    except
      // schema is not available
    end ;
  end ;

  procedure TGIS_LayerGML.setUp ;
  var
    saxReader : TGIS_SAXContentHandler ;
    vstr      : String ;
    gtype     : String ;
    tkn       : TGIS_Tokenizer ;
    i         : Integer ;
  begin
    inherited;

    addingPolygon   := False ;
    finishedPolygon := False ;
    addingMPolygon  := False ;
    addingCoords    := False ;
    addingPoint     := False ;
    addingMPoint    := False ;
    addingMember    := False ;
    addingLine      := False ;
    addingArc       := False ;
    addingMSurface  := False ;
    addingMCurve    := False ;
    addingCCurve    := False ;
    usingCoord      := False ;
    addingObject    := False ;
    addingRing      := False ;
    lastElement     := ''    ;
    UseAttrName     := False ;
    foundSchema     := False ;
    foundSRS        := False ;

    schemaChecks   := 0 ;
    numberReturned := 0 ;
    oldItemsCount  := Items.Count ;

    // old parameter
    vstr := GisMetadataAsString( 'TGIS_LayerGML.AxisOrder', '' ) ;
    if IsStringEmpty( vstr ) then
      vstr := ReadConfigParam( GIS_INI_AXIS_ORDER ) ;
    if not IsStringEmpty( vstr ) then
      FAxisOrderReversed := vstr = GIS_INI_AXIS_ORDER_NE ;

    // new parameter
    FAxisOrderReversed := GisMetadataAsBoolean(
                            'TGIS_LayerGML.AxisOrderReversed',
                            FAxisOrderReversed
                          ) ;
    FAxisOrderReversed := StrToBoolean(
                            ReadConfigParam( GIS_INI_AXIS_ORDER_REVERSED ),
                            FAxisOrderReversed
                          ) ;
    FDetectFieldTypes  := GisMetadataAsBoolean(
                            'TGIS_LayerGML.DetectFieldTypesFromValues',
                            FDetectFieldTypes
                          ) ;
    vstr := GisMetadataAsString( 'TGIS_LayerGML.FlattenXML', '' ) ;
    if IsStringEmpty( vstr ) then
      vstr := ReadConfigParam( GIS_INI_FLATTEN_XML ) ;
    if not IsStringEmpty( vstr ) then
      FFlattenXML := StrToBoolean( vstr, False ) ;

    forceCoordDim := GisMetadataAsInteger( 'TGIS_LayerGML.ForceCoordinateDimension', -1 ) ;

    FFieldValuesSeparator := GisMetadataAsString( 'TGIS_LayerGML.FieldValuesSeparator',
                                                  FFieldValuesSeparator
                                                ) ;
    cityGMLSplit := GisMetadataAsBoolean(
                      'TGIS_LayerGML.CityGMLSplitFeatures',
                      cityGMLSplit
                    ) ;
    DefaultDimension := TGIS_DimensionType.XYZ ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( Name, ';' ) ;
      if tkn.Result.Count <= 1 then begin
        isFilterByLayer := False ;
        filterLayerName := '' ;
        filterShapeType := TGIS_ShapeType.Unknown ;
      end
      else begin
        isFilterByLayer := True ;
        filterLayerName := tkn.Result[1] ;

        if tkn.Result.Count > 2 then begin
          gtype := UpperCase( tkn.Result[2] ) ;
          if      gtype = 'POINT'              then filterShapeType := TGIS_ShapeType.Point
          else if gtype = 'MULTIPOINT'         then filterShapeType := TGIS_ShapeType.MultiPoint
          else if gtype = 'LINESTRING'         then filterShapeType := TGIS_ShapeType.Arc
          else if gtype = 'MULITLINESTRING'    then filterShapeType := TGIS_ShapeType.Arc
          else if gtype = 'POLYGON'            then filterShapeType := TGIS_ShapeType.Polygon
          else if gtype = 'MULTIPOLYGON'       then filterShapeType := TGIS_ShapeType.Polygon
          else if gtype = 'GEOMETRYCOLLECTION' then filterShapeType := TGIS_ShapeType.Complex
          else if gtype = 'GEOMETRY'           then filterShapeType := TGIS_ShapeType.Unknown
          else if gtype = 'CIRCULARSTRING'     then filterShapeType := TGIS_ShapeType.Arc
          else if gtype = 'COMPOUNDCURVE'      then filterShapeType := TGIS_ShapeType.Arc
          else if gtype = 'MULTICURVE'         then filterShapeType := TGIS_ShapeType.Arc
          else if gtype = 'CURVEPOLYGON'       then filterShapeType := TGIS_ShapeType.Polygon
          else if gtype = 'MULTISURFACE'       then filterShapeType := TGIS_ShapeType.Polygon
          else                                      filterShapeType := TGIS_ShapeType.Unknown ;
        end ;
      end ;
    finally
      FreeObject( tkn ) ;
    end;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    saxReader := T_saxHandlerGML.Create( Self ) ;
    try
      try
        if not IsStringEmpty( Path ) then begin
          if SafeFileExists( Path ) then begin
            parseSchemaFile( Path );
            saxReader.LoadFromFile( Path ) ;
          end
          else if IsServerPath( Path ) then begin
            saxReader.LoadFromFile( Path ) ;
          end ;
          end
        else if assigned( Stream ) then begin
          saxReader.LoadFromStream( Stream ) ;
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

      if Items.Count > 0 then
        DefaultShapeType := TGIS_Shape( Items[0] ).ShapeType ;

      // reset flag
      for i := 0 to Fields.Count-1 do
        FieldInfo(i).Binary := 0 ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := 'Geographic Markup Language (GML)' ;
    finally
      Unlock ;
      FreeObject( saxReader ) ;
      RaiseBusyRelease( Self ) ;

      FIsModified := False ;
      lAttributes.Clear ;
    end ;
  end ;

  {$IFNDEF GENXDK}
    function TGIS_LayerGML.AddFromStream : Integer ;
    begin
      setUp ;
      Result := Max( numberReturned, Items.Count-oldItemsCount ) ;
    end ;

    procedure TGIS_LayerGML.ParseSchemaStream( const _stream : TStream ) ;
    var
      doc       : TGIS_XMLDocument ;
      schemadef : IXMLNode ;
    begin
      if not assigned( _stream ) then exit ;

      try
        doc := TGIS_XMLDocument.Create ;
        try
          doc.LoadFromStream( _stream ) ;

          schemadef := doc.DocumentElement ;
          if not assigned( schemadef ) then exit ;

          if schemadef.LocalName <> 'schema' then
            schemadef := schemadef.ChildNodes.FindNode( 'xsd:schema' ) ;
          if not assigned( schemadef ) then
            exit ;

          parseSchema( schemadef ) ;
        finally
          FreeObject( doc ) ;
        end ;
      except
        // schema is not available
      end ;
    end ;
  {$ENDIF}

  procedure TGIS_LayerGML.SaveData  ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  procedure TGIS_LayerGML.Build(
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

  procedure TGIS_LayerGML.ImportLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent      ;
    const _type        : TGIS_ShapeType   ;
    const _scope       : String           ;
    const _shape       : TGIS_Shape       ;
    const _de9im       : String           ;
    const _truncated   : Boolean
  ) ;
  var
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape  ;
    {$ENDIF}
    shp_tmp     : TGIS_Shape      ;
    same_name   : Boolean         ;
    ex          : TGIS_Extent     ;
    shape_no    : Integer         ;
    end_uid     : TGIS_Uid        ;
    abort       : Boolean         ;
    old_scope   : String          ;
    {$IFDEF JAVA}
      stream    : TXMLOutputStream ;
    {$ELSE}
      stream    : TGIS_FileStream  ;
    {$ENDIF}
    sattributes : SAXAttributes   ;
    val         : String          ;
    path_xsd    : String          ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    shape_no := 0 ;
    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    path_xsd := getSchemaPath( Path ) ;

    RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [Name] ) ) ;
    try

      ImportStructure( _layer ) ;
      PrepareExportFieldNames( 32 ) ;
      ExportStructureToFLD ;

      same_name := CompareText( GetPathAbsolute( '', Path        ),
                                GetPathAbsolute( '', _layer.Path )
                              ) = 0  ;

      // prepare output stream
      try
        {$IFDEF JAVA}
          stream := java.io.FileOutputStream.Create( GetTemporaryName( Path ) ) ;
        {$ELSE}
          stream := TGIS_FileStream.Create(
                      GetTemporaryName( Path ), fmCreate
                    ) ;
        {$ENDIF}
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( Path ), GetLastError
                                   ) ;
      end ;

      // prepare and save GML file
      SAXWriter := TGIS_SAXWriter.Create( stream ) ;
      SAXWriter.SaveCompact := True ;
      SAXWriter.StartDocument ;
      SAXWriter._XMLDecl( '1.0', 'UTF-8', '' ) ;

      sattributes := CoSAXAttributes.Create ;
      try
        sattributes.AddAttribute( GML_NOVAL, GML_NOVAL, GML_XMLNS_GML,
                                  GML_NOVAL, GML_XMLNS_GML_WWW
                                ) ;
        sattributes.AddAttribute( GML_NOVAL, GML_NOVAL, GML_XMLNS_GML_G,
                                  GML_NOVAL, GML_XMLNS_GML_G_WWW
                                ) ;
        sattributes.AddAttribute( GML_NOVAL, GML_NOVAL, GML_XMLNS_XSI,
                                  GML_NOVAL, GML_XMLNS_XSI_WWW
                                ) ;
        sattributes.AddAttribute( GML_NOVAL, GML_NOVAL, GML_XMLNS_SCHEMA,
                                  GML_NOVAL, GML_XMLNS_SCHEMA_L +
                                  GetFileName( getSchemaPath( Path ) )
                                ) ;

        startTag( GML_NOVAL, GML_NOVAL, GML_G+GML_TATUKGIS_MAP, sattributes ) ;
        sattributes.RemoveAttribute( 0 ) ;
      finally
        FreeObject( sattributes ) ;
      end ;

      val := '';
      sattributes := CoSAXAttributes.Create ;
      try
        sattributes.Clear ;
        startTag( GML_NOVAL, GML_NOVAL, GML+GML_DESCRIPTION, sattributes ) ;
        val := _layer.FileInfo ;
        if not IsStringEmpty( val ) then
          SAXWriter.Characters( val );
        endTag( GML_NOVAL, GML_NOVAL, GML+GML_DESCRIPTION ) ;
        startTag( GML_NOVAL, GML_NOVAL, GML+GML_NAME, sattributes ) ;
        val := _layer.Name ;
        SAXWriter.Characters( val );
        endTag( GML_NOVAL, GML_NOVAL, GML+GML_NAME ) ;
      finally
        FreeObject( sattributes ) ;
      end ;

      ex := GisCommonExtent( _layer.Extent, _extent ) ;
      writeBox( ex ) ;

      try
        old_scope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF}
            in _layer.Loop( ex, _scope, _shape, _de9im ) do
        begin
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
              writeGeometry( shp_tmp ) ;
              shp_tmp.Unlock;
            end ;
          finally
            if shp <> shp_tmp then
              FreeObject( shp_tmp ) ;
          end ;

          if shape_no mod 100 = 1 then begin
            abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
            if abort then break ;
          end ;
          inc( shape_no ) ;
        end ;

        writeSchema( GetTemporaryName( path_xsd ) );
      finally
        _layer.Scope := old_scope ;

        endTag( GML_NOVAL, GML_NOVAL, GML_G+GML_TATUKGIS_MAP ) ;
        SAXWriter.EndDocument ;
        FreeObject( SAXWriter ) ;
        FreeObject( stream ) ;

        if abort then begin
          DeleteFile( GetTemporaryName( Path ) ) ;
          DeleteFile( GetTemporaryName( path_xsd ) ) ;
        end
        else begin
          DeleteFile( GetBackupName( Path ) ) ;
          RenameFile( Path, GetBackupName( Path ) ) ;
          try
            if not RenameFile( GetTemporaryName( Path ), Path ) then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                           Path, GetLastError
                                         ) ;
          except
            RenameFile( GetBackupName( Path ), Path ) ;
            raise ;
          end ;

          DeleteFile( GetBackupName( path_xsd ) ) ;
          RenameFile( path_xsd, GetBackupName( path_xsd ) ) ;
          try
            if not RenameFile( GetTemporaryName( path_xsd ), path_xsd ) then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                           Path, GetLastError
                                         ) ;
          except
            RenameFile( GetBackupName( path_xsd ), path_xsd ) ;
            raise ;
          end ;

        end ;

        if not IsOpened then begin
          Items.Clear ;
          Fields.Clear ;
          TList<T_gmlFieldDef>( FSchemaTypes ).Clear ;

          Open ;
        end ;
      end ;
    finally
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  function TGIS_LayerGML.GetAvailableLayers : TGIS_LayerInfoList ;
  {$IFDEF DCC}
  var
    sname : String ;
    otype : T_FeatureType ;
  {$ENDIF}
  begin
    Result := TGIS_LayerInfoList.Create ;

    try
      isPrescanMode := True ;
      prescanLayers := T_MultiValueDictionary.Create ;
      try
        Open ;

        for sname in T_MultiValueDictionary(prescanLayers).GetKeys  do
          for otype in T_MultiValueDictionary(prescanLayers).GetValues( sname ) do
            Result.Add(
              TGIS_LayerInfo.Create(
                sname,
                TGIS_RegisteredLayerType.Vector,
                otype.SType,
                otype.Count
              )
            ) ;
      finally
        isPrescanMode := False ;
        FreeObject( prescanLayers ) ;
      end ;
    except
      // wrong connection
      on E : EGIS_Exception do
        TGIS_Logger.AsError( GetClassName(Self), E.Message ) ;
    end ;
  end ;

  function TGIS_LayerGML.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  const
    MAX_BUFFER_SIZE = 2048 ;
  var
    oStm : TGIS_BufferedFileStream ;
    buf  : TBytes ;
    i    : Integer ;
  begin
    if SafeFileExists( _path ) then begin
      Result := False ;
      oStm := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
      try
        SetLength( buf, MAX_BUFFER_SIZE ) ;
        {$IFDEF OXYGENE}
         oStm.Read( buf, MAX_BUFFER_SIZE ) ;
        {$ELSE}
         oStm.Read( buf[ 0 ], MAX_BUFFER_SIZE ) ;
        {$ENDIF}
        i := 0 ;
        while i < MAX_BUFFER_SIZE-4 do begin
          if (( chr( buf[ i ] )   = 'g' ) and
              ( chr( buf[ i+1 ] ) = 'm' ) and
              ( chr( buf[ i+2 ] ) = 'l' ) and
              ( chr( buf[ i+3 ] ) = ':' )) or
             (( chr( buf[ i ] )   = ':' ) and
              ( chr( buf[ i+1 ] ) = 'g' ) and
              ( chr( buf[ i+2 ] ) = 'm' ) and
              ( chr( buf[ i+3 ] ) = 'l' )) or
             (( chr( buf[ i ] )   = 'g' ) and
              ( chr( buf[ i+2 ] ) = 'm' ) and
              ( chr( buf[ i+4 ] ) = 'l' ) and
              ( chr( buf[ i+6 ] ) = ':' )) then
             begin
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
// Unit_GisLayerGML
//==============================================================================

  class procedure Unit_GisLayerGML.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-GML', 'Geographic Markup Language',
                   TGIS_LayerGML, '.gml;.xml',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                    [ TGIS_RegisteredOperationType.Read,
                      TGIS_RegisteredOperationType.Write,
                      TGIS_RegisteredOperationType.&Create,
                      TGIS_RegisteredOperationType.Merge
                    ],
                   True
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisLayerGML.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
