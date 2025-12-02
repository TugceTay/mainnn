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
  Encapsulation of Google Earth Keyhole Markup Language (KML) Layer.
}

{$IFDEF DCC}
  unit GisLayerKML ;
  {$HPPEMIT '#pragma link "GisLayerKML"'}
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
    System.Variants,
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,

    GisClasses,
    GisTypes,
    GisTypesUI,

    GisLayerVector,
    GisLayerPixel,
    GisCsSystems,
    GisTransform,
    GisXmlSax ;
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
  Unit_GisLayerKML = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFDEF CLR}
    {#GENDOC:HIDE}
    IUnknown = TObject ;
  {$ENDIF}

  {#GENDOC:HIDE}
  /// <summary>
  ///   KML attribute definition.
  /// </summary>
  TGIS_AttributeKML = {$IFDEF OXYGENE} unit {$ENDIF} record
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
  ///   Encapsulation of the KML vector Layer.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     <list type="bullet">
  ///       <item>
  ///         Supported shape types: TGIS_ShapeType.Point,
  ///         TGIS_ShapeType.Arc, TGIS_ShapeType.Polygon.
  ///       </item>
  ///       <item>
  ///         Upon importing shape of type TGIS_ShapeType.MultiPoint the
  ///         shape will be converted to a set of TGIS_ShapeType.Point
  ///         shapes.
  ///       </item>
  ///       <item>
  ///         GroundOverlay image will be represented as a read-only pixel sublayer.
  ///       </item>
  ///     </list>
  ///   </note>
  /// </remarks>
  TGIS_LayerKML = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      FNameField    : String ;
      FExtrude      : Boolean ;
      FTessellate   : Boolean ;
      FAltitudeMode : Boolean ;
      FZCoordinate  : Integer ;
      FLookAt       : Boolean ;
      FPreserveZ    : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private variable
      addingObject  : Boolean ;
      useAttrName   : Boolean ;
      fieldName     : String ;
      elementNo     : Cardinal;
      addingPoly    : Boolean ;
      addingCoords  : Boolean ;
      addingColor   : Boolean ;
      addingWidth   : Boolean ;
      addingFill    : Boolean ;
      usingDesc     : Boolean ;
      usingLookAt   : Boolean ;
      usingMultiGeo : Boolean ;
      addingLabelC  : Boolean ;
      addingMarkerC : Boolean ;
      addingLineC   : Boolean ;
      addingAreaC   : Boolean ;
      addingStyleUrl: Boolean ;
      addingExtended: Boolean ;
      addingStyleMap: Boolean ;
      bIsKMZ        : Boolean ;
      multiList     : TGIS_ObjectList ;
      tkn           : TGIS_Tokenizer ;
      lAttributes   : TList<TGIS_AttributeKML> ;

      // GroundOverlay
      addingLatLonBox   : Boolean ;
      addingLatLonQuad  : Boolean ;
      latLonBox         : TGIS_Extent ;
      latLonBoxRotation : Double ;
      overlayIconHref   : String ;
      addingOverlay     : Boolean ;

      /// <summary>
      ///   SAX reader handle.
      /// </summary>
      SAXReader       : TGIS_SAXContentHandler ;

      /// <summary>
      ///   SAX writer handle.
      /// </summary>
      SAXWriter       : TGIS_SAXWriter ;

      /// <summary>
      ///   Current shape.
      /// </summary>
      currShp         : TGIS_Shape;

      /// <summary>
      ///   Current shape point list.
      /// </summary>
      ptgList         : TGIS_PointList  ;
      curColor        : TGIS_Color      ;
      hasColor        : Boolean         ;
      curStyleUrl     : String          ;
      curStyleOvr     : String          ;
      curCoord        : Double          ;
      styleList       : TStringList     ;
      styleMap        : TStringList     ;
      styleObj        : TObject         ;
      importshapeNo   : Integer         ;
      importendUid    : TGIS_Uid        ;
      importExtent    : TGIS_Extent     ;
      importTrunc     : Boolean         ;
      importShpType   : TGIS_ShapeType  ;
      importLayerObj  : TGIS_LayerVector;
      importStyleType : Integer         ;
      coordDim        : Integer         ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private routine

      /// <summary>
      ///   Add shape part.
      /// </summary>
      procedure addShapePart ;

      /// <summary>
      ///   Write coordinates section.
      /// </summary>
      /// <param name="_shp">
      ///   shape to write
      /// </param>
      /// <param name="_partno">
      ///   shape part to write
      /// </param>
      procedure writeCoordinates      ( const _shp          : TGIS_Shape ;
                                        const _partno       : Integer ;
                                        const _pointno      : Integer
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
      procedure addAtrToArray         ( const _atrname      : String ;
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
      procedure addAtrToArray         ( const _atrname      : String ;
                                        const _atrvalue     : String ;
                                        const _useAttrName  : Boolean
                                      ) ; overload;

      /// <summary>
      ///   Converts TGIS_Point to string in order to write it to file.
      /// </summary>
      /// <param name="_point">
      ///   point to convert
      /// </param>
      function  pointToString         ( const _point        : TGIS_Point3D
                                      ) : String ;

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
      procedure buildCoords           ( const _shp          : TGIS_Shape ;
                                        const _partno       : Integer ;
                                        const _partsize     : Integer
                                      ) ;

      /// <summary>
      ///   Add points to shape using long format coords.
      /// </summary>
      /// <param name="_str">
      ///   string of coords to parse and add to shape
      /// </param>
      procedure addPointsToShape      ( const _str           : String
                                      ) ;

      /// <summary>
      ///   Add style to shape.
      /// </summary>
      /// <param name="_str">
      ///   color
      /// </param>
      procedure addStyleToShape       ( const _str           : String
                                      ) ;

      /// <summary>
      ///   Add new point.
      /// </summary>
      procedure addPoint;

      /// <summary>
      ///   Add new line.
      /// </summary>
      procedure addLine;

      /// <summary>
      ///   Add new polygon.
      /// </summary>
      procedure addPolygon            ;

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
      procedure addAtrToShp           ( const _atrname      : String ;
                                        const _atrvalue     : String ;
                                        const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes geometry data and properties to KML File.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save (data only)
      /// </param>
      procedure writeGeometry         ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes information, properties to KML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeData             ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes shape style to KML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeStyle            ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes layer styles to KML file.
      /// </summary>
      procedure writeStyles           ( const _layer        : TGIS_LayerVector
                                      ) ;

      /// <summary>
      ///   Writes Point to KML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      /// <param name="_partno">
      ///   shape part to save
      /// </param>
      /// <param name="_pointno">
      ///   shape point to save
      /// </param>
      procedure writePoint            ( const _shp          : TGIS_Shape ;
                                        const _partno       : Integer    ;
                                        const _pointno      : Integer
                                      );

      /// <summary>
      ///   Writes LineString to KML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeLine             ( const _shp          : TGIS_Shape
                                      ) ; overload;

      /// <summary>
      ///   Writes LineString to KML file.
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
      ///   Writes MultiLine to KML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeMultiLine        ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes Polygon to GML file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writePolygon          ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Save StartTag node
      /// </summary>
      /// <param name="_namespaceuri">
      ///   NameSpace URI value
      /// </param>
      /// <param name="_localname">
      ///   LocalName value
      /// </param>
      /// <param name="_qname">
      ///   keeps 'KML:xxx' schema
      /// </param>
      procedure startTag              ( const _namespaceuri : String ;
                                        const _localname    : String ;
                                        const _qname        : String
                                      ) ; overload;

      /// <summary>
      ///   Save StartTag node
      /// </summary>
      /// <param name="_namespaceuri">
      ///   NameSpace URI value
      /// </param>
      /// <param name="_localname">
      ///   LocalName value
      /// </param>
      /// <param name="_qname">
      ///   keeps 'KML:xxx' schema
      /// </param>
      /// <param name="_attributes">
      ///   element attribute value
      /// </param>
      procedure startTag              ( const _namespaceuri : String ;
                                        const _localname    : String ;
                                        const _qname        : String ;
                                        const _attributes   : IMXAttributes
                                      ) ; overload;

      /// <summary>
      ///   Save EndElement node to KML file
      /// </summary>
      /// <param name="_namespaceuri">
      ///   NameSpace URI value
      /// </param>
      /// <param name="_localname">
      ///   LocalName value
      /// </param>
      /// <param name="_qname">
      ///   keeps 'KML:xxx' schema
      /// </param>
      procedure endTag                ( const _namespaceuri : String ;
                                        const _localname    : String ;
                                        const _qname        : String
                                      ) ;

      /// <summary>
      ///   Parse description tag for attributes.
      /// </summary>
      /// <param name="_str">
      ///   tag string
      /// </param>
      procedure parseDesc             ( const _str          : String
                                      );

      /// <summary>
      ///   Check if a field name is specified as internal format tag.
      /// </summary>
      /// <param name="_name">
      ///   name of field
      /// </param>
      function  isInternalName        ( const _name         : String
                                      ) : Boolean ;

      /// <summary>
      ///   Export shape callback.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_abort">
      ///   if True, export will be stopped
      /// </param>
      procedure exportForEach         (       _shp          : TGIS_Shape ;
                                        var   _abort        : Boolean
                                      ) ;

      /// <summary>
      ///   Prepare color string.
      /// </summary>
      /// <param name="_color">
      ///   color value
      /// </param>
      /// <param name="_transparency">
      ///   color transparency
      /// </param>
      function  getColor              ( const _color        : TGIS_Color ;
                                        const _transparency : Integer
                                      ) : String ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp    ; override;

      procedure fset_CS  ( const _value  : TGIS_CSCoordinateSystem
                         ) ; override;
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
      procedure Build         ( const _path      : String           ;
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
      procedure SaveData       ; override;

      /// <inheritdoc/>
      function GetAvailableLayers : TGIS_LayerInfoList ; override;

      /// <inheritdoc/>
      function    Draw            : Boolean ; override;

      /// <inheritdoc/>
      function    DrawEx          ( const _extent  : TGIS_Extent
                                  ) : Boolean ; override;

    public

      /// <summary>
      ///   Field name used to set KML name values.
      /// </summary>
      property NameField  : String read FNameField write FNameField ;

      /// <summary>
      ///   Use tessellation to allow lines and paths to follow the terrain.
      /// </summary>
      property Tessellate : Boolean read FTessellate write FTessellate;

      /// <summary>
      ///   Allows vertical extrusion of two-dimensional features to form
      ///   three-dimensional objects.
      /// </summary>
      property Extrude    : Boolean read FExtrude write FExtrude ;

      /// <summary>
      ///   Defines the observation coordinates or eye point of the placemark.
      /// </summary>
      property LookAt     : Boolean read FLookAt write FLookAt;

      /// <summary>
      ///   Preserve Z coordinate from imported shapes to avoid reprojection.
      /// </summary>
      property PreserveZ  : Boolean read FPreserveZ write FPreserveZ ;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Math,
    GisParams,
    GisCompression,
    GisResource,
    GisFunctions,
    GisRtl,
    GisInternals,
    GisStreams,
    GisLayerPNG,
    GisLayerGIF,
    GisLayerJPG,
    GisTopology,
    GisSymbol,
    GisUtils,
    GisInterfaces,
    GisRegistredLayers,
    GisCsFactory ;
{$ENDIF}

type
  { Encapsulation of sax handler for KML files.
  }
  T_saxHandlerKML = class( TGIS_SAXContentHandler )
     private
      // KML layer handle.
      layerKML : TGIS_LayerKML ;
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

      // Characters - Value of element.
      // _strchars   element value
      procedure Characters   ( const _chars   : String
                             ) ; override;
    public
      procedure FatalError   ( const _locator : IVBSAXLocator ;
                               const _message : String        ;
                               const _code    : HResult
                             ) ; override;
    public
      // Create an instance for provided KML layer
      // _layer KML layer
      constructor Create( const _layer : TGIS_LayerKML ) ;
  end ;

  T_styleKML = class
    public
      name        : String  ;
      markerColor : TGIS_Color ;
      labelColor  : TGIS_Color ;
      lineColor   : TGIS_Color ;
      areaColor   : TGIS_Color ;
      width       : Integer ;
      fill        : Integer ;
    public
      constructor Create ;
  end ;

  T_intListKML = TList<Integer> ;

const
   // Maximum coordinates length parsed without cutting apart
    MAX_PARAMS_LENGTH  = 4000;

    KML_DOCUMENT       = 'Document';
    KML_PLACEMARK      = 'Placemark';
    KML_GROUNDOVERLAY  = 'GroundOverlay';
    KML_DESCRIPTION    = 'description';
    KML_NAME           = 'name';

    KML_LOOK_AT        = 'LookAt';
    KML_LONGITUDE      = 'longitude';
    KML_LATITUDE       = 'latitude';
    KML_RANGE          = 'range';
    KML_TILT           = 'tilt';
    KML_HEADING        = 'heading';
    KML_VISIBILITY     = 'visibility';

    KML_STYLE          = 'Style';
    KML_STYLE_MAP      = 'StyleMap' ;
    KML_STYLE_URL      = 'styleUrl' ;
    KML_LINE_STYLE     = 'LineStyle';
    KML_COLOR          = 'color';
    KML_FILL           = 'fill';
    KML_WIDTH          = 'width';
    KML_POLY_STYLE     = 'PolyStyle';
    KML_ICON_STYLE     = 'IconStyle';
    KML_LABEL_STYLE    = 'LabelStyle';
    KML_BALOON_STYLE   = 'BalloonStyle';
    KML_TEXT           = 'text';
    KML_ID             = 'id';
    KML_SCALE          = 'scale';
    KML_SCALE_DEF      = '0.6';
    KML_ICON           = 'Icon';
    KML_HREF           = 'href';
    KML_DESC           = '$[description]';
    KML_EXTENDED_DATA  = 'ExtendedData' ;

    KML_POINT          = 'Point';
    KML_LINESTRING     = 'LineString';
    KML_POLYGON        = 'Polygon';
    KML_OUTERBOUNDARY  = 'outerBoundaryIs';
    KML_LINEAR_RING    = 'LinearRing';
    KML_INNERBOUNDARY  = 'innerBoundaryIs';
    KML_TESSELLATE     = 'tessellate';
    KML_EXTRUDE        = 'extrude';
    KML_ALTITUDE       = 'altitude';
    KML_ALTITUDE_MODE  = 'altitudeMode';
    KML_COORDINATES    = 'coordinates';
    KML_MULTIGEOMETRY  = 'MultiGeometry' ;
    KML_LATLONBOX      = 'LatLonBox';
    KML_LATLONQUAD     = 'LatLonQuad';

   // Prefixex
    KML_XMLNS_KML_W    = 'xmlns' ;
    KML_XMLNS_KML_W_WWW= 'http://earth.google.com/kml/2.2';
    KML_TATUKGIS_MAP   = 'kml';
    KML_GOOGLE_SYMBOL  = 'http://maps.google.com/mapfiles/kml/shapes/placemark_square.png';

    KML_TATUKGIS_SYMBOL_PATH = 'http://www.tatukgis.com/Library/Symbols/KML/' ;

    KML_TATUKGIS_SYMBOL_BOX           = 'BOX.png' ;
    KML_TATUKGIS_SYMBOL_CIRCLE        = 'CIRCLE.png' ;
    KML_TATUKGIS_SYMBOL_CROSS         = 'CROSS.png' ;
    KML_TATUKGIS_SYMBOL_DIAGCROSS     = 'DIAGCROSS.png' ;
    KML_TATUKGIS_SYMBOL_TRIANGLEUP    = 'TRIANGLEUP.png' ;
    KML_TATUKGIS_SYMBOL_TRIANGLEDOWN  = 'TRIANGLEDOWN.png' ;
    KML_TATUKGIS_SYMBOL_TRIANGLELEFT  = 'TRIANGLELEFT.png' ;
    KML_TATUKGIS_SYMBOL_TRIANGLERIGHT = 'TRIANGLERIGHT.png' ;

    KML_NOVAL : String = ' ' ;


  constructor T_styleKML.Create ;
  begin
    inherited ;

    name        := ''  ;
    markerColor := TGIS_Color.Black ;
    labelColor  := TGIS_Color.Black ;
    lineColor   := TGIS_Color.Black ;
    areaColor   := TGIS_Color.Black ;
    width       := 0 ;
    fill        := 0 ;
  end ;

//==============================================================================
//  T_saxHandler
//==============================================================================

  constructor T_saxHandlerKML.Create( const _layer : TGIS_LayerKML ) ;
  begin
    inherited Create;

    layerKML := _layer ;
  end ;

  procedure T_saxHandlerKML.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i      : Integer ;
    stName : String ;
  begin
    inherited;

    if ( _lname = KML_PLACEMARK ) then
    begin
       layerKML.addingObject := True ;
       layerKML.lAttributes.Clear ;
       layerKML.hasColor := False ;
    end
    else if ( _lname = KML_GROUNDOVERLAY ) then
    begin
       layerKML.addingObject := True ;
       layerKML.addingOverlay := True ;
       layerKML.lAttributes.Clear ;
       layerKML.hasColor := False ;
    end
    else if _lname = KML_STYLE then begin
      if ( _attribs.Length > 0 ) then begin
        if _attribs.GetLocalName( 0 ) = 'id' then
          stName := '#' + _attribs.GetValue( 0 ) ;
        end ;
        layerKML.styleObj := T_styleKML.Create ;
        T_styleKML( layerKML.styleObj ).name := stName ;
        T_styleKML( layerKML.styleObj ).fill := 1 ;
        if layerKML.addingObject then
          layerKML.curStyleOvr := Trim( stName ) ;
    end
    else if _lname = KML_STYLE_MAP then begin
      if ( _attribs.Length > 0 ) then begin
        if _attribs.GetLocalName( 0 ) = 'id' then
          stName := '#' + _attribs.GetValue( 0 ) ;
        end ;
        layerKML.curStyleOvr := Trim( stName ) ;
        layerKML.addingStyleMap := True ;
    end
    else if _lname = KML_LABEL_STYLE then
      layerKML.addingLabelC := True
    else if _lname = KML_ICON_STYLE then
      layerKML.addingMarkerC := True
    else if _lname = KML_LINE_STYLE then
      layerKML.addingLineC := True
    else if _lname = KML_POLY_STYLE then
      layerKML.addingAreaC := True
    else if ( _lname = KML_COLOR ) then
      layerKML.addingColor := True
    else if ( _lname = KML_WIDTH ) then
      layerKML.addingWidth := True
    else if ( _lname = KML_FILL ) then
      layerKML.addingFill := True
    else if layerKML.addingStyleMap and ( _lname = KML_STYLE_URL ) then
      layerKML.addingStyleUrl := True ;

    if layerKML.addingObject then begin

      if _lname = KML_MULTIGEOMETRY then begin
        layerKML.usingMultiGeo := True ;
        layerKML.multiList := TGIS_ObjectList.Create( True ) ;
      end ;

      if _lname = KML_POINT then
        layerKML.addPoint;

      if _lname = KML_LINESTRING then
        layerKML.addLine;

      if _lname = KML_POLYGON then begin
        layerKML.addPolygon;
        layerKML.addingPoly := True ;
      end ;

      if _lname = KML_LATLONBOX then begin
        layerKML.addingLatLonBox := True ;
      end ;

      if _lname = KML_LATLONQUAD then begin
        layerKML.addingLatLonQuad := True ;
        layerKML.addPolygon;
        layerKML.addShapePart;
      end ;

      if not layerKML.addingPoly and
         ( _lname = KML_LINEAR_RING ) then begin
        layerKML.addLine ;
      end ;

      if ( _lname = KML_INNERBOUNDARY ) or
         ( _lname = KML_OUTERBOUNDARY ) then
        layerKML.addShapePart;

      if ( _lname = KML_COORDINATES ) then
        layerKML.addingCoords := True ;

      if ( _lname = KML_DESCRIPTION ) then
        layerKML.usingDesc := True ;

      if ( _lname = KML_LOOK_AT ) then
        layerKML.usingLookAt := True ;

      if ( _lname = KML_COLOR ) then
        layerKML.addingColor := True ;

      if ( _lname = KML_FILL ) then
        layerKML.addingFill := True ;

      if ( _lname = KML_STYLE_URL ) then
        layerKML.addingStyleUrl := True ;

      if ( _lname = KML_EXTENDED_DATA ) then
        layerKML.addingExtended := True ;

      if layerKML.addingExtended then begin
        if ( _attribs.Length > 0 ) then begin
          for i := 0 to _attribs.Length - 1 do begin
            if _attribs.GetLocalName( i ) = KML_NAME then
              layerKML.fieldName := _attribs.GetValue( i ) ;
          end
        end
        else begin
          layerKML.fieldName := _lname ;
        end ;
      end
      else begin
        // remember object property name
        if ( _attribs.Length > 0 ) then begin
          for i := 0 to _attribs.Length - 1 do begin
            layerKML.addAtrToArray( _lname + '_' +
                                    _attribs.GetLocalName( i ),
                                    _attribs.GetValue( i )
                                   )
          end
        end
        else begin
          layerKML.fieldName := _lname ;
        end ;
      end ;
    end ;
  end ;

  procedure T_saxHandlerKML.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  var
    i         : Integer ;
    style     : T_styleKML ;
    sname     : String ;
    stype     : TGIS_ShapeType ;
    same_type : Boolean ;
    ext       : TGIS_Extent ;
    img       : TGIS_LayerPixel ;
    mstm      : TStream ;
    r         : TGIS_HttpResponse ;
    lpath     : String ;
    trn       : TGIS_TransformPolynomial ;
    cptg, ptg : TGIS_Point ;
    rot_angle : Double ;

      procedure apply_style( const _shp : TGIS_Shape ; const _style : T_styleKML ) ;
      var
        j : Integer ;
      begin
        if not assigned( _shp ) then exit ;

        case _shp.ShapeType of
          TGIS_ShapeType.Point   :
            _shp.Params.Marker.Color        := _style.markerColor ;
          TGIS_ShapeType.Arc     : begin
            _shp.Params.Line.Color          := _style.lineColor ;
            _shp.Params.Line.Width          := -_style.width ;
          end ;
          TGIS_ShapeType.Polygon : begin
            if style.fill = 1 then begin
              _shp.Params.Area.Color        := _style.areaColor ;
              _shp.Params.Area.Pattern      := TGIS_BrushStyle.Solid ;
              _shp.Params.Area.OutlineColor := _style.lineColor ;
              _shp.Params.Area.OutlineWidth := -_style.width  ;
            end
            else begin
              _shp.Params.Area.OutlineColor := _style.lineColor ;
              _shp.Params.Area.Pattern      := TGIS_BrushStyle.Clear ;
              _shp.Params.Area.OutlineWidth := -_style.width  ;
            end ;
          end ;
          TGIS_ShapeType.Complex : begin
            for j := 0 to TGIS_ShapeComplex(_shp).ShapesCount-1 do
              apply_style( TGIS_ShapeComplex(_shp).GetShape(j), _style ) ;
          end ;
        end ;
        _shp.Params.Labels.Color            := TGIS_Color.Black ;
        _shp.Params.Labels.FontColor        := _style.labelColor ;
      end ;

      procedure addOverlay( const _img : TGIS_LayerPixel ) ;
      begin
        try
          _img.Open ;
          _img.Viewer  := layerKML.Viewer ;
          _img.CS      := layerKML.CS ;
          _img.Name    := layerKML.overlayIconHref ;
          _img.Caption := img.Name ;

          trn := TGIS_TransformPolynomial.Create ;

          if assigned( layerKML.currShp ) then begin
            // LatLonQuad
            ext := _img.Extent ;

            ptg := GisPoint( ext.XMin, ext.YMin )  ;
            trn.AddPoint( ptg, layerKML.currShp.GetPoint( 0, 0 ), 0, True ) ;
            ptg := GisPoint( ext.XMax, ext.YMin )  ;
            trn.AddPoint( ptg, layerKML.currShp.GetPoint( 0, 1 ), 1, True ) ;
            ptg := GisPoint( ext.XMax, ext.YMax )  ;
            trn.AddPoint( ptg, layerKML.currShp.GetPoint( 0, 2 ), 2, True ) ;
            ptg := GisPoint( ext.XMin, ext.YMax )  ;
            trn.AddPoint( ptg, layerKML.currShp.GetPoint( 0, 3 ), 3, True ) ;

            layerKML.currShp.Delete ;
          end
          else begin
            _img.Extent := layerKML.latLonBox ;

            cptg      := GisCenterPoint( layerKML.latLonBox ) ;
            ext       := layerKML.latLonBox ;
            rot_angle := layerKML.latLonBoxRotation ;

            ptg := GisPoint( 0, -_img.BitHeight )  ;
            trn.AddPoint( ptg, GisRotatePoint( cptg, ptg, rot_angle ), 0, True ) ;
            ptg := GisPoint( 0, 0 )  ;
            trn.AddPoint( ptg, GisRotatePoint( cptg, ptg, rot_angle ), 1, True ) ;
            ptg := GisPoint( _img.BitWidth, 0 )  ;
            trn.AddPoint( ptg, GisRotatePoint( cptg, ptg, rot_angle ), 2, True ) ;
            ptg := GisPoint( _img.BitWidth, -_img.BitHeight )  ;
            trn.AddPoint( ptg, GisRotatePoint( cptg, ptg, rot_angle ), 3, True ) ;
          end ;

          trn.Prepare( TGIS_PolynomialOrder.First ) ;
          _img.Transform := trn ;
          _img.Transform.Active := True ;
          _img.RecalcProjectedExtent ;

          if not assigned( layerKML.SubLayers ) then
            layerKML.SubLayers := TGIS_LayerAbstractList.Create( False );
          layerKML.SubLayers.Add( _img ) ;
        except
          FreeObjectNotNil( _img ) ;
        end ;
      end ;

  begin
    inherited;

    inc( layerKML.elementNo );
    if layerKML.elementNo mod 200 = 0 then
      layerKML.HourglassShake ;

    if _lname = KML_STYLE then begin
      if assigned( layerKML.styleObj ) then
        layerKML.styleList.AddObject( T_styleKML( layerKML.styleObj ).name,
                                      layerKML.styleObj
                                     ) ;
    end ;

    if _lname = KML_STYLE_MAP then begin
      layerKML.addingStyleMap := False ;
    end ;

    if layerKML.addingStyleMap and (_lname = KML_STYLE_URL) then
      layerKML.styleMap.Add( layerKML.curStyleOvr+'='+layerKML.curStyleUrl ) ;

    if _lname = KML_ICON_STYLE then
      layerKML.addingMarkerC := False ;
    if _lname = KML_LABEL_STYLE then
      layerKML.addingLabelC := False ;
    if _lname = KML_LINE_STYLE then
      layerKML.addingLineC := False ;
    if _lname = KML_POLY_STYLE then
      layerKML.addingAreaC := False ;
    if ( _lname = KML_COLOR ) then
      layerKML.addingColor  := False ;

    if ( _lname = KML_WIDTH ) then
      layerKML.addingWidth := False ;

    if ( _lname = KML_FILL ) then
      layerKML.addingFill := False ;

    if ( _lname = KML_COORDINATES ) then
      layerKML.addingCoords  := False ;

    if _lname = KML_MULTIGEOMETRY then begin
      layerKML.usingMultiGeo := False ;
    end ;

    if _lname = KML_POLYGON then begin
      layerKML.addingPoly := False ;
    end ;

    if _lname = KML_LATLONBOX then begin
      layerKML.addingLatLonBox := False ;
      layerKML.addPolygon ;
      layerKML.addShapePart ;

      ext := layerKML.latLonBox ;
      rot_angle := layerKML.latLonBoxRotation ;

      ptg := GisPoint( ext.XMin, ext.YMin ) ;
      cptg := GisCenterPoint( layerKML.latLonBox ) ;
      layerKML.currShp.AddPoint( GisRotatePoint( cptg, ptg, rot_angle ) ) ;
      ptg := GisPoint( ext.XMax, ext.YMin )  ;
      layerKML.currShp.AddPoint( GisRotatePoint( cptg, ptg, rot_angle ) ) ;
      ptg := GisPoint( ext.XMax, ext.YMax )  ;
      layerKML.currShp.AddPoint( GisRotatePoint( cptg, ptg, rot_angle ) ) ;
      ptg := GisPoint( ext.XMin, ext.YMax )  ;
      layerKML.currShp.AddPoint( GisRotatePoint( cptg, ptg, rot_angle ) ) ;
    end ;

    if _lname = KML_LATLONQUAD then begin
      layerKML.addingLatLonQuad := False ;
    end ;

    if layerKML.addingLatLonBox then begin
      if _lname = 'north' then
        layerKML.latLonBox.YMax := layerKML.curCoord
      else if _lname = 'south' then
        layerKML.latLonBox.YMin := layerKML.curCoord
      else if _lname = 'east' then
        layerKML.latLonBox.XMax := layerKML.curCoord
      else if _lname = 'west' then
        layerKML.latLonBox.XMin := layerKML.curCoord
      else if _lname = 'rotation' then
        layerKML.latLonBoxRotation := DegToRad( layerKML.curCoord ) ;
    end ;

    if ( _lname = KML_STYLE_URL ) then begin
      layerKML.addingStyleUrl := False ;
    end ;

    if ( _lname = KML_EXTENDED_DATA ) then
      layerKML.addingExtended := False ;

    if ( _lname = KML_PLACEMARK ) or ( _lname = KML_GROUNDOVERLAY ) then
    begin
      layerKML.addingObject  := False ;
      layerKML.addingCoords  := False ;

      if assigned( layerKML.multiList ) and
          ( layerKML.multiList.Count > 0 ) then begin
        if ( layerKML.multiList.Count = 1 ) then
          layerKML.currShp := layerKML.AddShape( TGIS_Shape( layerKML.multiList[0] ) )
        else begin
          same_type := True ;
          stype := TGIS_Shape( layerKML.multiList[0] ).ShapeType ;

          for i := 1 to layerKML.multiList.Count - 1 do
            if stype <> TGIS_Shape( layerKML.multiList[i] ).ShapeType then
              same_type := False ;

          if same_type then begin
            layerKML.currShp := layerKML.CreateShape( stype, TGIS_DimensionType.XYZ ) ;
            layerKML.currShp.Reset ;
            layerKML.currShp.Lock( TGIS_Lock.Projection ) ;
            try
              for i := 0 to layerKML.multiList.Count - 1 do
                layerKML.currShp.AppendGeometry(
                    TGIS_Shape( layerKML.multiList[i] )
                ) ;
            finally
              layerKML.currShp.Unlock ;
            end;
          end
          else begin
            layerKML.currShp := layerKML.CreateShape( TGIS_ShapeType.Complex ) ;
            for i := 0 to layerKML.multiList.Count - 1 do
              TGIS_ShapeComplex( layerKML.currShp ).AddShape(
                  TGIS_Shape( layerKML.multiList[i] ).CreateCopy
              ) ;
          end ;
        end ;
        FreeObject( layerKML.multiList ) ;
      end ;

      if assigned( layerKML.currShp ) then begin

        if IsStringEmpty( layerKML.curStyleUrl ) then begin
          if assigned( layerKML.styleObj ) then begin
            style := T_styleKML(layerKML.styleObj) ;
            apply_style( layerKML.currShp, style ) ;
          end
          else if layerKML.hasColor then begin
            case layerKML.currShp.ShapeType of
              TGIS_ShapeType.Point   :
                layerKML.currShp.Params.Marker.Color := layerKML.curColor ;
              TGIS_ShapeType.Arc     :
                layerKML.currShp.Params.Line.Color   := layerKML.curColor ;
              TGIS_ShapeType.Polygon :
                layerKML.currShp.Params.Area.Color   := layerKML.curColor ;
            end ;
          end ;
        end
        else begin
          if layerKML.styleMap.Count > 0 then begin
            sname := layerKML.styleMap.Values[ layerKML.curStyleUrl ] ;
            if not IsStringEmpty( sname ) then
              i := layerKML.styleList.IndexOf( sname )
            else
              i := layerKML.styleList.IndexOf( layerKML.curStyleUrl ) ;
          end
          else
            i := layerKML.styleList.IndexOf( layerKML.curStyleUrl ) ;

          if i > - 1 then begin
            style := T_styleKML( layerKML.styleList.Objects[i] ) ;
            apply_style( layerKML.currShp, style ) ;
          end ;
          layerKML.curStyleUrl := '' ;

          i := layerKML.styleList.IndexOf( layerKML.curStyleOvr ) ;
          if i > - 1 then begin
            style := T_styleKML( layerKML.styleList.Objects[i] ) ;
            apply_style( layerKML.currShp, style ) ;

            FreeObjectNotNil( layerKML.styleList.Objects[ i ] ) ;
            layerKML.styleList.Delete( i ) ;
          end ;
          layerKML.curStyleOvr := '' ;
        end ;

        // add attributes to object
        for i := 0 to layerKML.lAttributes.Count - 1 do
          layerKML.addAtrToShp( layerKML.lAttributes[i].Name,
                                layerKML.lAttributes[i].Value,
                                layerKML.currShp
                              ) ;

        layerKML.lAttributes.Clear ;

        layerKML.currShp.Unlock ;
        layerKML.currShp.Params.Labels.Field := KML_NAME ;
        if layerKML.Items.Count = 1 then
          layerKML.Extent := layerKML.currShp.Extent ;
      end ;

      if ( _lname = KML_GROUNDOVERLAY ) then begin
        layerKML.addingOverlay := False ;
        if assigned( layerKML.currShp ) then
          layerKML.currShp.Params.Labels.Field := '' ;

        if not IsStringEmpty( layerKML.overlayIconHref ) then begin
          if IsServerPath( layerKML.overlayIconHref ) then begin
            r.Stream := nil ;
            try
              r := TGIS_WebUtils.HttpFetch( layerKML.overlayIconHref, nil, nil, True, 40000,
                                            GetDefaultUserAgent( 'ttkWP' ), '', '', ''
                                          ) ;
              if r.Status = GIS_HTTP_OK then begin
                case DecodeContentType( r.ContentType, True ) of
                  TGIS_ContentType.Gif   : img := TGIS_LayerGIF.Create ;
                  TGIS_ContentType.Jpg   : img := TGIS_LayerJPG.Create ;
                  TGIS_ContentType.Png   : img := TGIS_LayerPNG.Create
                  else                     img := nil ;
                end ;

                if assigned( img ) then begin
                  img.Stream := r.Stream ;
                  addOverlay( img ) ;
                end ;
              end ;
            finally
              FreeObject( r.Stream ) ;
            end ;
          end
          else begin
            lpath := GetPathAbsolute( GetFileDir( layerKML.Path ),
                                      layerKML.overlayIconHref
                                    ) ;
            if SafeFileExists( lpath ) then begin
              img := GisCreateLayer( lpath, lpath ) as TGIS_LayerPixel ;
              if assigned( img ) then
                addOverlay( img ) ;
            end
            else begin
              if layerKML.bIsKMZ then begin
                mstm := DecompressZipFile( layerKML.Path, layerKML.overlayIconHref ) ;
                if assigned( mstm ) then begin
                  try
                    mstm.Position := 0 ;
                    img := GisCreateLayer( lpath, lpath ) as TGIS_LayerPixel ;
                    if assigned( img ) then begin
                      img.Path := '' ;
                      img.Stream := mstm ;
                      addOverlay( img ) ;
                    end ;
                  finally
                    FreeObject( mstm ) ;
                  end ;
                end ;
              end
            end ;
          end ;
        end ;
      end ;
    end
    else if ( _lname = KML_DESCRIPTION ) then begin
        layerKML.usingDesc := False ;
    end
    else if ( _lname = KML_LOOK_AT ) then
        layerKML.usingLookAt := False
    else if ( layerKML.usingMultiGeo )
            and
            (
              ( _lname = KML_POINT      ) or
              ( _lname = KML_LINESTRING ) or
              ( _lname = KML_POLYGON    )
            ) then begin
        layerKML.currShp.Unlock ;
        layerKML.multiList.Add( layerKML.currShp ) ;
    end
  end ;

  procedure T_saxHandlerKML.Characters(
    const _chars   : String
  ) ;
  var
    str : String ;
  begin
    inherited;

    str := RemoveWhitesFast( _chars ) ;
    if not IsStringEmpty( Trim( str ) ) then begin
      if layerKML.addingCoords or layerKML.addingLatLonQuad then
        layerKML.addPointsToShape( Trim( str ) )
      else if layerKML.addingColor then
        layerKML.addStyleToShape( Trim( str ) )
      else if layerKML.addingFill then
        layerKML.addStyleToShape( Trim( str ) )
      else if layerKML.addingWidth then
        layerKML.addStyleToShape( Trim( str ) )
      else if layerKML.addingStyleUrl then
        layerKML.curStyleUrl := Trim( str )
      else if layerKML.addingLatLonBox then
        layerKML.curCoord := DotStrToFloat( Trim( str ) )
      else if layerKML.addingOverlay and ( layerKML.fieldName = KML_HREF ) then
        layerKML.overlayIconHref := Trim( str )
      else if layerKML.addingObject then begin
        if layerKML.usingDesc then
          layerKML.parseDesc( str );

        if not layerKML.isInternalName( layerKML.fieldName ) then
          layerKML.addAtrToArray( layerKML.fieldName, str ) ;

        layerKML.useAttrName := False ;
      end
    end
    else if layerKML.useAttrName then begin
      layerKML.addAtrToArray( layerKML.fieldName, str, layerKML.useAttrName ) ;
      layerKML.useAttrName := False ;
    end ;
  end ;

  procedure T_saxHandlerKML.FatalError(
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
// KML Layer implementation
//==============================================================================

  constructor TGIS_LayerKML.Create;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;

    lAttributes     := TList<TGIS_AttributeKML>.Create ;
    currShp         := nil;
    elementNo       := 0;
    FExtrude        := False;
    FTessellate     := False;
    FAltitudeMode   := False;
    FLookAt         := False;
    FPreserveZ      := False;
    addingPoly      := False;
    addingLatLonBox := False;
    addingLatLonQuad:= False;
    addingOverlay   := False;
    FNameField      := '' ;
    ptgList         := TGIS_PointList.Create;
    tkn             := TGIS_Tokenizer.Create;
    styleList       := TStringList.Create ;
    styleMap        := TStringList.Create ;

    {$IFDEF GIS_NORECORDS}
    latLonBox         := new TGIS_Extent() ;
    {$ENDIF}
    latLonBoxRotation := 0 ;
    overlayIconHref   := '' ;

    FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
  end ;

  procedure TGIS_LayerKML.doDestroy ;
  var
    i : Integer ;
  begin
    FreeObject( lAttributes ) ;
    FreeObject( ptgList     ) ;
    FreeObject( tkn         ) ;

    if assigned( styleList ) then
      for i := 0 to styleList.Count - 1 do
        FreeObjectNotNil( styleList.Objects[ i ] ) ;

    FreeObject( styleMap  ) ;
    FreeObject( styleList ) ;

    inherited ;
  end ;

  procedure TGIS_LayerKML.addPoint;
  begin
    if usingMultiGeo then
      currShp := TGIS_ShapePoint.Create( nil, nil, False, -1, nil,
                                         TGIS_DimensionType.XYZ
                                       )
    else
      currShp := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;
    currShp.Lock( TGIS_Lock.Projection );
    currShp.Reset;
  end ;

  procedure TGIS_LayerKML.addLine;
  begin
    if usingMultiGeo then
      currShp := TGIS_ShapeArc.Create( nil, nil, False, -1, nil,
                                       TGIS_DimensionType.XYZ
                                     )
    else
      currShp := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShp.Lock( TGIS_Lock.Projection ) ;
    currShp.Reset ;
    currShp.AddPart;
  end ;

  procedure TGIS_LayerKML.addPolygon;
  begin
    if usingMultiGeo then
      currShp := TGIS_ShapePolygon.Create( nil, nil, False, -1, nil,
                                           TGIS_DimensionType.XYZ
                                         )
    else
      currShp := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ ) ;
    currShp.Lock( TGIS_Lock.Projection );
    currShp.Reset;
  end ;

  procedure TGIS_LayerKML.addShapePart;
  begin
    if assigned( currShp ) then begin
      currShp.AddPart;
    end ;
  end ;

  procedure TGIS_LayerKML.buildCoords( const _shp      : TGIS_Shape ;
                                       const _partno   : Integer    ;
                                       const _partsize : Integer
                                      ) ;
  var
    j     : Integer ;
    chars : String ;
  begin
    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      for j := 0 to _partsize - 1 do begin
        chars := pointToString( _shp.GetPoint3D( _partno, j ) );
        if j < ( _partsize -1 ) then
          chars := chars + ' ' ;
        SAXWriter.Characters( chars ) ;
      end ;
    finally
      _shp.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerKML.writeCoordinates( const _shp      : TGIS_Shape ;
                                            const _partno   : Integer ;
                                            const _pointno  : Integer
                                           ) ;
  var
    partsize  : Integer    ;
    i         : Integer    ;
    chars     : String ;
  begin
    startTag( KML_NOVAL, KML_NOVAL, KML_COORDINATES );
      if ( _partno <> -1 ) and ( _pointno = -1 ) then begin
        partsize := _shp.GetPartSize( _partno ) ;
        buildCoords( _shp, _partno, partsize ) ;
      end
      else if _pointno <> -1 then begin
        _shp.Lock( TGIS_Lock.Projection ) ;
        try
          chars := pointToString( _shp.GetPoint3D( _partno, _pointno ) ) ;
          SAXWriter.Characters( chars ) ;
        finally
          _shp.Unlock ;
        end ;
      end
      else begin
        for i := _pointno to _shp.GetNumParts - 1 do begin
          partsize := _shp.GetPartSize( i ) ;
          buildCoords( _shp, i, partsize ) ;
          chars := ' ' ;
          SAXWriter.Characters( chars ) ;
        end ;
      end ;
    endTag( KML_NOVAL, KML_NOVAL, KML_COORDINATES ) ;
  end ;

  procedure TGIS_LayerKML.parseDesc(
    const _str : String
  ) ;
  var
    part,
    val  : String ;
    k    : Integer ;
    att  : TGIS_AttributeKML ;
  begin
    val := _str ;
    // check if there are any items like fields=values<BR>
    if ( Pos( '<BR>', val ) = StringFirst - 1 ) and
       ( Pos( '='   , val ) = StringFirst - 1 ) then begin
      att.Name  := KML_DESCRIPTION ;
      att.Value := _str ;
      lAttributes.Add( att ) ;
      exit ;
    end ;

    while not IsStringEmpty( val ) do begin
      k := Pos( '<BR>', val ) ;
      if k > StringFirst - 1 then begin
        part := Copy( val, StringFirst, k - StringFirst ) ;
        tkn.ExecuteEx( part, '=' ) ;
        if tkn.Result.Count = 2 then
          addAtrToArray( tkn.Result[ 0 ], tkn.Result[ 1 ] ) ;
        val := Copy( val, k + 4, length( val ) - k - 3 ) ;
      end
      else break ;
    end ;

    if not IsStringEmpty( val ) then begin
      att.Name  := KML_DESCRIPTION ;
      att.Value := _str ;
      lAttributes.Add( att ) ;
    end ;

  end ;

  procedure TGIS_LayerKML.addStyleToShape(
    const _str : String
  ) ;
  var
    r,g,b,a   : Integer ;
    vr,vg,vb  : Byte    ;
    val       : String  ;
    h,s,l     : Single  ;
    clvwr     : TGIS_Color  ;
  begin
    if addingFill then begin
      if assigned( styleObj ) and addingAreaC then
        T_styleKML( styleObj ).fill := StrToInt( _str ) ;
    end
    else if addingWidth then begin
      if assigned( styleObj ) then
        T_styleKML( styleObj ).width := TruncS( DotStrToFloat( _str ) ) ;
    end
    else begin
      // format aabbggrr
      try
        val := '$' + Copy( _str, StringFirst, 2 ) ;
        a := StrToInt( val ) ;

        val := '$' + Copy( _str, StringFirst+2, 2 ) ;
        b := StrToInt( val ) ;

        val := '$' + Copy( _str, StringFirst+4, 2 ) ;
        g := StrToInt( val ) ;

        val := '$' + Copy( _str, StringFirst+6, 2 ) ;
        r := StrToInt( val ) ;

        curColor := TGIS_Color.FromRGB( r,g,b ) ;
        hasColor := True ;

        h := curColor.H ;
        s := curColor.S ;
        l := curColor.L ;

        l := l + (1- l)*(255-a)/255 ;
        curColor := TGIS_Color.FromHSL( h, s, l ) ;

        if assigned( Viewer ) then
          clvwr := Viewer.Ref.Color
        else
          clvwr := TGIS_Color.White ;

        r  := curColor.R ;
        g  := curColor.G ;
        b  := curColor.B ;
        vr := clvwr.R ;
        vg := clvwr.G ;
        vb := clvwr.B ;

        if ( Abs( vr - r ) < 16 ) and
           ( Abs( vg - g ) < 16 ) and
           ( Abs( vb - b ) < 16 )
        then // color very close to the window color
          curColor :=  TGIS_Color.FromARGB( a, r xor vr, g xor vg, b xor vb ) ;

        if assigned( styleObj ) then begin
          if addingMarkerC then
            T_styleKML( styleObj ).markerColor := curColor
          else if addingLabelC then
            T_styleKML( styleObj ).labelColor  := curColor
          else if addingLineC then
            T_styleKML( styleObj ).lineColor := curColor
          else if addingAreaC then
            T_styleKML( styleObj ).areaColor := curColor ;
        end ;
      except
        // ignore invalid format
      end ;
    end ;
  end ;

  procedure TGIS_LayerKML.addPointsToShape(
    const _str : String
  ) ;
  var
    i     : Integer        ;
    tknEx : TGIS_Tokenizer ;
    ptg   : TGIS_Point3D {$IFDEF JAVA} := new TGIS_Point3D {$ENDIF} ;
    str   : String ;
  begin
    if not assigned( currShp ) then exit ;

    if currShp.IsEmpty then
      currShp.AddPart;

    tknEx := TGIS_Tokenizer.Create;
    try
      // test coordinates dimension
      if coordDim = -1 then begin
        str := Copy( _str, StringFirst, Pos(' ', _str )-StringFirst ) ;
        if IsStringEmpty( str ) then
          str := _str ;
        tknEx.Execute( str, [ ',' ] ) ;
        if tknEx.Result.Count < 2 then begin
          str := Copy( _str, StringFirst, Pos(',', _str )-StringFirst ) ;
          if IsStringEmpty( str ) then
            str := _str ;
          tknEx.Execute( str, [ ' ' ] ) ;
        end ;

        if tknEx.Result.Count > coordDim then
          coordDim := Min( tknEx.Result.Count, 3 ) ;
      end ;

      tknEx.Execute( _str, [ ' ', ',' ] ) ;
      i := 0 ;
      while i < tknEx.Result.Count - 1 do begin
        if IsStringEmpty( tknEx.Result[ i ] ) then inc( i ) ;

        ptg.X := DotStrToFloat( tknEx.Result[ i ] ) ;
        inc( i );
        ptg.Y := DotStrToFloat( tknEx.Result[ i ] ) ;
        inc( i );
        if (coordDim > 2) and (i < tknEx.Result.Count ) then begin
          ptg.Z := DotStrToFloat( tknEx.Result[ i ] ) ;
          inc( i );
        end
        else
          ptg.Z := 0 ;
        ptg.M := 0 ;

        currShp.AddPoint3D( ptg )
      end ;
    finally
      FreeObject( tknEx ) ;
    end ;
  end ;

  procedure TGIS_LayerKML.addAtrToShp(
    const _atrname  : String ;
    const _atrvalue : String ;
    const _shp      : TGIS_Shape
  ) ;
  var
    res : Integer ;
    val : String ;
    atr : String ;
  begin
    if assigned( _shp ) then begin
      atr := _atrname  ;
      val := _atrvalue ;
      res := FindField( atr ) ;
      if res = -1 then
        AddFieldInternal( atr, TGIS_FieldType.String, 1, 0);

      if not IsStringEmpty( val ) then
        _shp.SetField( atr, VarToString( _shp.GetField( atr ) ) + val ) ;
    end ;
  end ;

  procedure TGIS_LayerKML.addAtrToArray(
    const _atrname  : String ;
    const _atrvalue : String
  ) ;
  begin
    addAtrToArray( _atrname, _atrvalue, False ) ;
  end ;

  procedure TGIS_LayerKML.addAtrToArray(
    const _atrname     : String ;
    const _atrvalue    : String ;
    const _useAttrName : Boolean
  ) ;
  var
    attr : TGIS_AttributeKML ;
  begin
    if ( ( not IsStringEmpty( _atrvalue ) ) or _useAttrName ) and
       ( not IsStringEmpty( _atrname ) ) then begin
      if isInternalName( _atrname ) then exit ;
      attr.Name  := _atrname ;
      attr.Value := _atrvalue ;
      lAttributes.Add( attr ) ;
    end ;
  end ;

  procedure TGIS_LayerKML.writeGeometry( const _shp : TGIS_Shape ) ;
  var
    numparts, i : Integer ;
  begin
    numparts := _shp.GetNumParts ;
    if numparts <= 0 then exit ;

    if _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] then begin
      FAltitudeMode := True ;
      FExtrude      := True ;
      FZCoordinate := 1 ;
    end
    else begin
      FZCoordinate := 0 ;
      FAltitudeMode := False ;
    end ;

    if _shp is TGIS_ShapePoint then begin
      writePoint( _shp, numparts, 0 ) ;
    end
    else if _shp is TGIS_ShapeMultiPoint then begin
      for i := 0 to _shp.GetPartSize(0) - 1 do
        writePoint( _shp, 0, i ) ;
    end
    else if ( _shp is TGIS_ShapeArc ) then begin
      if numparts = 1 then
        writeLine( _shp )
      else
        writeMultiLine( _shp ) ;
    end
    else if (_shp is TGIS_ShapePolygon) or (_shp is TGIS_ShapeMultiPatch) then
      writePolygon( _shp )
    else ;// unknown object type
  end ;

  function TGIS_LayerKML.getColor(
    const _color        : TGIS_Color ;
    const _transparency : Integer
  ) : String ;
  var
    trans : Integer ;
    r,g,b : Byte ;
    color : String ;
  begin
    r := _color.R ;
    g := _color.G ;
    b := _color.B ;
    trans  := RoundS( _transparency * 2.55 ) ;
    color  := IntToHex( b, 2 ) +
              IntToHex( g, 2 ) +
              IntToHex( r, 2 ) ;
    Result := IntToHex( trans, 2 ) + color ;
  end ;

  { Help internal method.
  }
  function twipsToPixels( const _layer : TGIS_LayerVector ;
                          const _size  : Integer
                         ) : Integer ;
  var
    tmp : Double ;
  begin
    if assigned( _layer.Viewer ) then
      Result := _layer.Viewer.Ref.TwipsToPixels( _size )
    else begin
      if      _size < 0 then Result := -_size // minus so real pixels
      else if _size > 0 then begin
                               tmp := 1.0 * _size * 96 ;
                               if tmp > 1440 then Result := RoundS( tmp /1440 )
                                             else Result := 1 ;
                             end
      else                   Result := 0 ;
    end ;
  end ;

  procedure TGIS_LayerKML.writeStyles(
    const _layer : TGIS_LayerVector
  ) ;
  var
    sattributes : SAXAttributes ;
    color       : String ;
  begin
    sattributes := CoSAXAttributes.Create ;
    try
      sattributes.AddAttribute( KML_NOVAL, KML_NOVAL, KML_ID,
                                KML_NOVAL, 'ttkStyle'
                              ) ;
      startTag( KML_NOVAL, KML_NOVAL, KML_STYLE, sattributes ) ;
    finally
      FreeObject( sattributes ) ;
    end ;

      startTag( KML_NOVAL, KML_NOVAL, KML_ICON_STYLE ) ;
        startTag( KML_NOVAL, KML_NOVAL, KML_SCALE ) ;
          color := KML_SCALE_DEF ;
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_SCALE );
        startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
          color := getColor( _layer.Params.Marker.Color, _layer.Transparency );
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
        startTag( KML_NOVAL, KML_NOVAL, KML_ICON ) ;
          startTag( KML_NOVAL, KML_NOVAL, KML_HREF ) ;
            color := KML_TATUKGIS_SYMBOL_PATH ;
            case _layer.Params.Marker.Style of
                TGIS_MarkerStyle.Box           :
                  color := color + KML_TATUKGIS_SYMBOL_BOX ;
                TGIS_MarkerStyle.Circle        :
                  color := color + KML_TATUKGIS_SYMBOL_CIRCLE ;
                TGIS_MarkerStyle.Cross         :
                  color := color + KML_TATUKGIS_SYMBOL_CROSS ;
                TGIS_MarkerStyle.DiagCross     :
                  color := color + KML_TATUKGIS_SYMBOL_DIAGCROSS ;
                TGIS_MarkerStyle.TriangleUp    :
                  color := color + KML_TATUKGIS_SYMBOL_TRIANGLEUP ;
                TGIS_MarkerStyle.TriangleDown  :
                  color := color + KML_TATUKGIS_SYMBOL_TRIANGLEDOWN ;
                TGIS_MarkerStyle.TriangleLeft  :
                  color := color + KML_TATUKGIS_SYMBOL_TRIANGLELEFT ;
                TGIS_MarkerStyle.TriangleRight :
                  color := color + KML_TATUKGIS_SYMBOL_TRIANGLERIGHT
            else  color := KML_GOOGLE_SYMBOL ;
            end ;
            SAXWriter.Characters( color );
          endTag( KML_NOVAL, KML_NOVAL, KML_HREF );
        endTag( KML_NOVAL, KML_NOVAL, KML_ICON );
      endTag( KML_NOVAL, KML_NOVAL, KML_ICON_STYLE );

      startTag( KML_NOVAL, KML_NOVAL, KML_LABEL_STYLE );
        startTag( KML_NOVAL, KML_NOVAL, KML_SCALE ) ;
          color := KML_SCALE_DEF ;
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_SCALE );
        startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
          color := getColor( _layer.Params.Labels.Color, _layer.Transparency );
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
      endTag( KML_NOVAL, KML_NOVAL, KML_LABEL_STYLE );

      startTag( KML_NOVAL, KML_NOVAL, KML_BALOON_STYLE ) ;
        startTag( KML_NOVAL, KML_NOVAL, KML_TEXT ) ;
          color := KML_DESC ;
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_TEXT );
      endTag( KML_NOVAL, KML_NOVAL, KML_BALOON_STYLE );

      startTag( KML_NOVAL, KML_NOVAL, KML_LINE_STYLE );
        startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
        if _layer.Params.Area.Pattern = TGIS_BrushStyle.Clear then
          color := getColor( _layer.Params.Area.OutlineColor, _layer.Transparency )
        else
          color := getColor( _layer.Params.Line.Color, _layer.Transparency );
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
        startTag( KML_NOVAL, KML_NOVAL, KML_WIDTH );
          color := IntToStr( twipsToPixels( _layer, _layer.Params.Line.Width ) ) ;
          SAXWriter.Characters( color );
        endTag( KML_NOVAL, KML_NOVAL, KML_WIDTH );
      endTag( KML_NOVAL, KML_NOVAL, KML_LINE_STYLE );

      startTag( KML_NOVAL, KML_NOVAL, KML_POLY_STYLE );
        if _layer.Params.Area.Pattern = TGIS_BrushStyle.Solid then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
            color := getColor( _layer.Params.Area.Color, _layer.Transparency );
            SAXWriter.Characters( color );
          endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
        end
        else if _layer.Params.Area.Pattern = TGIS_BrushStyle.Clear then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
            color := getColor( _layer.Params.Area.OutlineColor, _layer.Transparency );
            SAXWriter.Characters( color );
          endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );

          startTag( KML_NOVAL, KML_NOVAL, KML_FILL );
            color := '0' ;
            SAXWriter.Characters( color );
          endTag( KML_NOVAL, KML_NOVAL, KML_FILL );
        end ;
      endTag( KML_NOVAL, KML_NOVAL, KML_POLY_STYLE );

    endTag( KML_NOVAL, KML_NOVAL, KML_STYLE );
  end ;

  procedure TGIS_LayerKML.writeStyle( const _shp : TGIS_Shape );
  var
    sattributes : SAXAttributes ;
    color       : String ;
    val         : String ;

  begin
    if importStyleType = 1 then begin
      startTag( KML_NOVAL, KML_NOVAL, KML_STYLE_URL );
        val := '#ttkStyle' ;
        SAXWriter.Characters( val );
      endTag( KML_NOVAL, KML_NOVAL, KML_STYLE_URL );
    end
    else begin

      sattributes := CoSAXAttributes.Create ;
      try
        startTag( KML_NOVAL, KML_NOVAL, KML_STYLE, sattributes ) ;
      finally
        FreeObject( sattributes ) ;
      end ;

        case _shp.ShapeType of
          TGIS_ShapeType.Point,
          TGIS_ShapeType.MultiPoint       :
            begin
              startTag( KML_NOVAL, KML_NOVAL, KML_ICON_STYLE ) ;
                startTag( KML_NOVAL, KML_NOVAL, KML_SCALE ) ;
                  val := KML_SCALE_DEF ;
                  SAXWriter.Characters( val );
                endTag( KML_NOVAL, KML_NOVAL, KML_SCALE );

                startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
                  color := getColor( _shp.Params.Marker.Color, _shp.Layer.Transparency );
                  SAXWriter.Characters( color );
                endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );

                startTag( KML_NOVAL, KML_NOVAL, KML_ICON ) ;
                  startTag( KML_NOVAL, KML_NOVAL, KML_HREF ) ;
                    color := KML_TATUKGIS_SYMBOL_PATH ;
                    case _shp.Params.Marker.Style of
                        TGIS_MarkerStyle.Box           :
                          color := color + KML_TATUKGIS_SYMBOL_BOX ;
                        TGIS_MarkerStyle.Circle        :
                          color := color + KML_TATUKGIS_SYMBOL_CIRCLE ;
                        TGIS_MarkerStyle.Cross         :
                          color := color + KML_TATUKGIS_SYMBOL_CROSS ;
                        TGIS_MarkerStyle.DiagCross     :
                          color := color + KML_TATUKGIS_SYMBOL_DIAGCROSS ;
                        TGIS_MarkerStyle.TriangleUp    :
                          color := color + KML_TATUKGIS_SYMBOL_TRIANGLEUP ;
                        TGIS_MarkerStyle.TriangleDown  :
                          color := color + KML_TATUKGIS_SYMBOL_TRIANGLEDOWN ;
                        TGIS_MarkerStyle.TriangleLeft  :
                          color := color + KML_TATUKGIS_SYMBOL_TRIANGLELEFT ;
                        TGIS_MarkerStyle.TriangleRight :
                          color := color + KML_TATUKGIS_SYMBOL_TRIANGLERIGHT
                    else  color := KML_GOOGLE_SYMBOL ;
                    end ;

                    SAXWriter.Characters( color );
                  endTag( KML_NOVAL, KML_NOVAL, KML_HREF );
                endTag( KML_NOVAL, KML_NOVAL, KML_ICON );

              endTag( KML_NOVAL, KML_NOVAL, KML_ICON_STYLE );

              startTag( KML_NOVAL, KML_NOVAL, KML_LABEL_STYLE );
                startTag( KML_NOVAL, KML_NOVAL, KML_SCALE ) ;
                  val := KML_SCALE_DEF ;
                  SAXWriter.Characters( val );
                endTag( KML_NOVAL, KML_NOVAL, KML_SCALE );
                startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
                  color := getColor( _shp.Params.Labels.Color, _shp.Layer.Transparency );
                  SAXWriter.Characters( color );
                endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
              endTag( KML_NOVAL, KML_NOVAL, KML_LABEL_STYLE );

              startTag( KML_NOVAL, KML_NOVAL, KML_BALOON_STYLE ) ;
                startTag( KML_NOVAL, KML_NOVAL, KML_TEXT ) ;
                  val := KML_DESC ;
                  SAXWriter.Characters( val );
                endTag( KML_NOVAL, KML_NOVAL, KML_TEXT );
              endTag( KML_NOVAL, KML_NOVAL, KML_BALOON_STYLE );
            end ;
          TGIS_ShapeType.Arc         :
            begin
              startTag( KML_NOVAL, KML_NOVAL, KML_LINE_STYLE );
                startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
                    color := getColor( _shp.Params.Line.Color, _shp.Layer.Transparency );
                  SAXWriter.Characters( color );
                endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
                startTag( KML_NOVAL, KML_NOVAL, KML_WIDTH );
                  val := IntToStr( twipsToPixels( _shp.Layer,_shp.Params.Line.Width ) ) ;
                  SAXWriter.Characters( val );
                endTag( KML_NOVAL, KML_NOVAL, KML_WIDTH );
              endTag( KML_NOVAL, KML_NOVAL, KML_LINE_STYLE );
            end ;
          TGIS_ShapeType.Polygon     :
            begin
              startTag( KML_NOVAL, KML_NOVAL, KML_POLY_STYLE );
                startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
                    color := getColor( _shp.Params.Area.Color, _shp.Layer.Transparency ) ;
                  SAXWriter.Characters( color );
                endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );

                if _shp.Params.Area.Pattern = TGIS_BrushStyle.Clear then begin
                  startTag( KML_NOVAL, KML_NOVAL, KML_FILL );
                    color := '0' ;
                    SAXWriter.Characters( color );
                  endTag( KML_NOVAL, KML_NOVAL, KML_FILL );
                end ;

              endTag( KML_NOVAL, KML_NOVAL, KML_POLY_STYLE );

              startTag( KML_NOVAL, KML_NOVAL, KML_LINE_STYLE );
                startTag( KML_NOVAL, KML_NOVAL, KML_COLOR );
                  color := getColor( _shp.Params.Area.OutlineColor, _shp.Layer.Transparency ) ;
                  SAXWriter.Characters( color );
                endTag( KML_NOVAL, KML_NOVAL, KML_COLOR );

                startTag( KML_NOVAL, KML_NOVAL, KML_WIDTH );
                  val := IntToStr( twipsToPixels( _shp.Layer,_shp.Params.Area.OutlineWidth ) ) ;
                  SAXWriter.Characters( val );
                endTag( KML_NOVAL, KML_NOVAL, KML_WIDTH );
              endTag( KML_NOVAL, KML_NOVAL, KML_LINE_STYLE );
            end ;
        end ;

      endTag( KML_NOVAL, KML_NOVAL, KML_STYLE );
    end ;
  end ;

  function TGIS_LayerKML.isInternalName( const _name : String
                                        ) : Boolean ;
  begin
    if addingExtended then
      Result := False
    else
    if ( LowerCase( _name ) = KML_DESCRIPTION   ) or
      ( LowerCase( _name ) = KML_COLOR         ) or
      ( LowerCase( _name ) = KML_LONGITUDE     ) or
      ( LowerCase( _name ) = KML_LATITUDE      ) or
      ( LowerCase( _name ) = KML_RANGE         ) or
      ( LowerCase( _name ) = KML_TILT          ) or
      ( LowerCase( _name ) = KML_HEADING       ) or
      ( LowerCase( _name ) = KML_EXTRUDE       ) or
      ( LowerCase( _name ) = KML_STYLE_URL     ) or
      ( LowerCase( _name ) = KML_SCALE         ) or
      ( LowerCase( _name ) = KML_HREF          ) or
      ( LowerCase( _name ) = KML_TEXT          ) or
      ( LowerCase( _name ) = KML_TESSELLATE    ) or
      ( LowerCase( _name ) = KML_VISIBILITY    ) or
      ( LowerCase( _name ) = KML_ALTITUDE      ) or
      ( LowerCase( _name ) = LowerCase( KML_ALTITUDE_MODE ) ) then
      Result := True
    else
      Result := False ;
  end ;

  procedure TGIS_LayerKML.writeData( const _shp : TGIS_Shape ) ;
  var
    chars : String ;
    val   : String ;
    fname : String ;
    i     : Integer    ;
  begin
    startTag( KML_NOVAL, KML_NOVAL, KML_NAME );

      if IsStringEmpty( FNameField ) then
        val := _shp.GetLabelPlain( False )
      else
        val := VarToString( _shp.GetField( FNameField ) ) ;

      if IsStringEmpty( Trim( val ) ) then begin
        val := VarToString( _shp.GetField( KML_NAME ) ) ;
      end ;

      if not IsStringEmpty( Trim( val ) ) then
        SAXWriter.Characters( val );

    endTag( KML_NOVAL, KML_NOVAL, KML_NAME );

    startTag( KML_NOVAL, KML_NOVAL, KML_DESCRIPTION );

      if IsStringEmpty( FNameField ) then
        val := _shp.GetLabel
      else
        val := VarToString( _shp.GetField( FNameField ) ) ;

      SAXWriter.StartCDATA ;
        if not IsStringEmpty( Trim( val ) ) then begin
           chars := Format( '%s<BR>', [ val ] );
           SAXWriter.Characters( chars );
        end ;

        for i := 0 to Fields.Count-1 do begin
          if FieldInfo( i ).Deleted   then continue ;
          if FieldInfo( i ).Temporary then continue ;
          if VarIsNull( _shp.GetFieldEx( FieldInfo( i ).NewName ) ) then continue;

          if FieldInfo( i ).FieldType = TGIS_FieldType.Date then
            val := DateTimeToXMLString(
                                  VarToDateTime( _shp.GetField( FieldInfo( i ).NewName ) ),
                                  2, True
                                )
          else
            val := VarToString( _shp.GetField( FieldInfo( i ).NewName ) ) ;

          if IsStringEmpty( Trim( val ) ) then continue;

          // get field name, value and save to file
          fname := GisCanonicalSQLName( FieldInfo( i ).ExportName ) ;
          if isInternalName( fname ) or
            ( LowerCase( fname ) = KML_NAME ) then Continue ;

          chars := Format( '%s=%s<BR>', [ fname, val ] );
          SAXWriter.Characters( chars );
        end ;
        if IsStringEmpty( chars ) then begin
          chars := VarToString( _shp.GetField( KML_DESCRIPTION ) ) ;
          SAXWriter.Characters( chars );
        end ;

      SAXWriter.EndCDATA ;
    endTag( KML_NOVAL, KML_NOVAL, KML_DESCRIPTION );

    if FLookAt then begin
      startTag( KML_NOVAL, KML_NOVAL, KML_LOOK_AT );

        chars := VarToString( _shp.GetField( KML_LONGITUDE ) ) ;
        if not IsStringEmpty( chars ) then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_LONGITUDE );
          SAXWriter.Characters( chars );
          endTag( KML_NOVAL, KML_NOVAL, KML_LONGITUDE );
        end ;

        chars := VarToString( _shp.GetField( KML_LATITUDE ) ) ;
        if not IsStringEmpty( chars ) then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_LATITUDE );
          SAXWriter.Characters( chars );
          endTag( KML_NOVAL, KML_NOVAL, KML_LATITUDE );
        end ;

        chars := VarToString( _shp.GetField( KML_RANGE ) ) ;
        if not IsStringEmpty( chars ) then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_RANGE );
          SAXWriter.Characters( chars );
          endTag( KML_NOVAL, KML_NOVAL, KML_RANGE );
        end ;

        chars := VarToString( _shp.GetField( KML_TILT ) ) ;
        if not IsStringEmpty( chars ) then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_TILT );
          SAXWriter.Characters( chars );
          endTag( KML_NOVAL, KML_NOVAL, KML_TILT );
        end ;

        chars := VarToString( _shp.GetField( KML_HEADING ) ) ;
        if not IsStringEmpty( chars ) then begin
          startTag( KML_NOVAL, KML_NOVAL, KML_HEADING );
          SAXWriter.Characters( chars );
          endTag( KML_NOVAL, KML_NOVAL, KML_HEADING );
        end ;

      endTag( KML_NOVAL, KML_NOVAL, KML_LOOK_AT );
    end ;

    if FExtrude then begin
      startTag( KML_NOVAL, KML_NOVAL, KML_EXTRUDE );
      chars := '1';
      SAXWriter.Characters( chars );
      endTag( KML_NOVAL, KML_NOVAL, KML_EXTRUDE );
    end ;

    if FTessellate then begin
      startTag( KML_NOVAL, KML_NOVAL, KML_TESSELLATE );
      chars := '1';
      SAXWriter.Characters( chars );
      endTag( KML_NOVAL, KML_NOVAL, KML_TESSELLATE );
    end ;

    if FAltitudeMode then begin
      startTag( KML_NOVAL, KML_NOVAL, KML_ALTITUDE_MODE );
      chars := 'relativeToGround';
      SAXWriter.Characters( chars );
      endTag( KML_NOVAL, KML_NOVAL, KML_ALTITUDE_MODE );
    end ;
  end ;

  function TGIS_LayerKML.pointToString(
    const _point : TGIS_Point3D
  ) : String ;
  begin
    if FZCoordinate = 1 then
      Result := DotFloatToStr( _point.X ) + ',' +
                DotFloatToStr( _point.Y ) + ',' +
                DotFloatToStr( _point.Z )
    else
      Result := DotFloatToStr( _point.X ) + ',' +
                DotFloatToStr( _point.Y ) ;
  end ;

  procedure TGIS_LayerKML.writePoint( const _shp          : TGIS_Shape ;
                                      const _partno       : Integer ;
                                      const _pointno      : Integer
                                     );
  begin
    startTag( KML_NOVAL, KML_NOVAL, KML_PLACEMARK ) ;
      writeData( _shp );
      writeStyle( _shp );
      startTag( KML_NOVAL, KML_NOVAL, KML_POINT ) ;
        writeCoordinates( _shp, _partno, _pointno ) ;
      endTag( KML_NOVAL, KML_NOVAL, KML_POINT ) ;
    endTag( KML_NOVAL, KML_NOVAL, KML_PLACEMARK ) ;
  end ;

  procedure TGIS_LayerKML.writeLine( const _shp    : TGIS_Shape
                                   ) ;
  begin
    writeLine( _shp, 0 ) ;
  end ;

  procedure TGIS_LayerKML.writeLine( const _shp    : TGIS_Shape ;
                                     const _partno : Integer
                                   ) ;
  begin
    // save point
    startTag( KML_NOVAL, KML_NOVAL, KML_PLACEMARK ) ;
      writeData( _shp );
      writeStyle( _shp );
      startTag( KML_NOVAL, KML_NOVAL, KML_LINESTRING ) ;
        writeCoordinates( _shp, _partno, -1 ) ;
      endTag( KML_NOVAL, KML_NOVAL, KML_LINESTRING ) ;
    endTag( KML_NOVAL, KML_NOVAL, KML_PLACEMARK ) ;
  end ;

  procedure TGIS_LayerKML.writeMultiLine( const _shp : TGIS_Shape )  ;
  var
    i : Integer ;
  begin
    for i := 0 to _shp.GetNumParts - 1 do
      writeLine( _shp, i ) ;
  end ;

  procedure TGIS_LayerKML.writePolygon( const _shp : TGIS_Shape ) ;
  var
    numparts : Integer ;
    i, p, sts: Integer ;
    tpl      : TGIS_Topology ;
      ap     : TList<T_intListKML> ;
    useMulti : Boolean ;

      procedure buildPoly( const _offset : Integer ) ;
      var
        k, cnt : Integer ;
        chars  : String ;
      begin
        cnt := ap[_offset].Count ;

        if cnt > 0 then begin

          startTag( KML_NOVAL, KML_NOVAL, KML_POLYGON ) ;
            for k := 0 to cnt - 1 do begin
              if k = 0 then begin // write main outerring
                startTag( KML_NOVAL, KML_NOVAL, KML_OUTERBOUNDARY ) ;

                  startTag( KML_NOVAL, KML_NOVAL, KML_LINEAR_RING ) ;
                    writeCoordinates( _shp, ap[_offset][k], -1 ) ;
                  endTag( KML_NOVAL, KML_NOVAL, KML_LINEAR_RING ) ;

                endTag( KML_NOVAL, KML_NOVAL, KML_OUTERBOUNDARY ) ;
              end
              else begin // write all inner rings
                startTag( KML_NOVAL, KML_NOVAL, KML_INNERBOUNDARY ) ;

                  startTag( KML_NOVAL, KML_NOVAL, KML_LINEAR_RING ) ;
                    writeCoordinates( _shp, ap[_offset][k], -1 ) ;
                  endTag( KML_NOVAL, KML_NOVAL, KML_LINEAR_RING ) ;

                endTag( KML_NOVAL, KML_NOVAL, KML_INNERBOUNDARY ) ;
              end
            end ;

            if FExtrude then begin
              startTag( KML_NOVAL, KML_NOVAL, KML_EXTRUDE );
              chars := '1';
              SAXWriter.Characters( chars );
              endTag( KML_NOVAL, KML_NOVAL, KML_EXTRUDE );
            end ;

            if FAltitudeMode then begin
              startTag( KML_NOVAL, KML_NOVAL, KML_ALTITUDE_MODE );
              chars := 'relativeToGround';
              SAXWriter.Characters( chars );
              endTag( KML_NOVAL, KML_NOVAL, KML_ALTITUDE_MODE );
            end ;

          endTag( KML_NOVAL, KML_NOVAL, KML_POLYGON ) ;
        end ;
      end ;

  begin
    tpl := TGIS_Topology.Create ;
    try
      useMulti := False ;
      numparts := _shp.GetNumParts ;
      ap := TList<T_intListKML>.Create ;
      for i := 0 to numparts - 1 do
        ap.Add( T_intListKML.Create ) ;
      // KML doesn't support multipart polygons well, we must split them
      // and write only one outer ring and many inner rings
      for i := 0 to numparts - 1 do begin
        sts  := tpl.PartStatus( TGIS_ShapePolygon( _shp ), i, p ) ;
        if ( sts = 0 ) or ( sts = 1 ) then
          p := i ;

        ap[p].Add( i ) ;

        if ( sts = 1 ) or ( ( i > 0 ) and ( sts = 0 ) ) then
          useMulti := True ;
      end ;

      startTag( KML_NOVAL, KML_NOVAL, KML_PLACEMARK ) ;
        writeData( _shp );
        writeStyle( _shp );

          if useMulti then  // write multigeometry
            startTag( KML_NOVAL, KML_NOVAL, KML_MULTIGEOMETRY ) ;

          for i := 0 to numparts - 1 do
            buildPoly( i ) ;

          if useMulti then
            endTag( KML_NOVAL, KML_NOVAL, KML_MULTIGEOMETRY ) ;

      endTag( KML_NOVAL, KML_NOVAL, KML_PLACEMARK ) ;
    finally
      for i := 0 to ap.Count - 1 do
        FreeObjectNotNil( ap[i] ) ;

      FreeObject( ap ) ;
      FreeObject( tpl ) ;
    end ;
  end ;

  procedure TGIS_LayerKML.exportForEach(
          _shp    : TGIS_Shape;
      var _abort  : Boolean
  ) ;
  var
    shp_tmp  : TGIS_Shape ;
    abort    : Boolean ;

    procedure remap_geometry_z ;
    var
      part_no  : Integer ;
      point_no : Integer ;
      cshp     : TGIS_Shape ;
      ptg      : TGIS_Point3D ;
    begin
      cshp := shp_tmp.CreateCopy ;
      try
        shp_tmp.Reset ;
        shp_tmp.Lock( TGIS_Lock.Internal2 );
        try
          for part_no := 0 to cshp.GetNumParts - 1 do begin
            shp_tmp.AddPart ;
            shp_tmp.SetPartType( part_no, cshp.GetPartType( part_no ) ) ;
            for point_no := 0 to cshp.GetPartSize( part_no ) - 1 do begin
              ptg   := cshp.GetPoint3D( part_no, point_no ) ;
              ptg.Z := _shp.GetPoint3D( part_no, point_no ).Z ;
              shp_tmp.AddPoint3D( ptg ) ;
            end ;
          end ;
        finally
          shp_tmp.Unlock ;
        end ;
      finally
        FreeObject( cshp ) ;
      end ;
    end ;

  begin
    shp_tmp := _shp.PrepareExportShape( CS, importExtent, importTrunc, True ) ;
    try
      if assigned( shp_tmp ) and
         ( not shp_tmp.IsDeleted ) and
         ( ( importShpType = shp_tmp.ShapeType      ) or
           ( importShpType = TGIS_ShapeType.Unknown )
         ) then
      begin
        if FPreserveZ and
          (shp_tmp.Dimension in [TGIS_DimensionType.XYZ,TGIS_DimensionType.XYZM]) then
        begin
          remap_geometry_z ;
          writeGeometry( shp_tmp ) ;
        end
        else
          writeGeometry( shp_tmp ) ;
      end ;
    finally
      if _shp <> shp_tmp then
        FreeObject( shp_tmp ) ;
    end ;

    if importshapeNo mod 100 = 1 then begin
      abort := RaiseBusyShake( importLayerObj, _shp.Uid, importendUid ) ;
    end ;

    inc( importshapeNo ) ;
  end ;

  procedure TGIS_LayerKML.startTag( const _namespaceuri : String ;
                                    const _localname    : String ;
                                    const _qname        : String
                                  ) ;
  begin
    startTag( _namespaceuri, _localname, _qname, nil ) ;
  end ;

  procedure TGIS_LayerKML.startTag( const _namespaceuri : String ;
                                    const _localname    : String ;
                                    const _qname        : String ;
                                    const _attributes   : IMXAttributes
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
    SAXWriter.StartElement( snamespaceuri, slocalname, sqname, _attributes ) ;
  end ;

  procedure TGIS_LayerKML.endTag(
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
    SAXWriter.EndElement( snamespaceuri, slocalname, sqname ) ;
  end ;

  procedure TGIS_LayerKML.fset_CS(
    const _value : TGIS_CSCoordinateSystem
  ) ;
  begin
    // only default CS can be used

  end ;

  procedure TGIS_LayerKML.setUp ;
  var
    mstm : TStream ;
    ext  : String ;
  begin
    inherited ;

    FSupportedShapes := GisGetEmptyShapeType ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Point ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Arc ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Polygon ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Complex ) ;

    DefaultDimension := TGIS_DimensionType.XYZ ;

    addingObject  := False ;
    usingDesc     := False ;
    usingLookAt   := False ;
    usingMultiGeo := False ;
    useAttrName   := False ;
    addingExtended:= False ;
    bIsKMZ        := False ;
    coordDim      := -1    ;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    SAXReader := T_saxHandlerKML.Create( Self ) ;
    try
      try
        if not IsStringEmpty( Path ) then begin
          if SafeFileExists( Path ) or IsServerPath( Path ) then begin
            ext := UpperCase( GetFileExt( Path ) ) ;
            if ext = '.KMZ' then begin
              bIsKMZ := True ;
              mstm := DecompressZipFile( Path, Name + '.kml' ) ;
              try
                mstm.Position := 0 ;
                SAXReader.LoadFromStream( mstm ) ;
              finally
                FreeObject( mstm ) ;
              end ;
            end
            else
              SAXReader.LoadFromFile( Path ) ;
          end ;
        end
        else if assigned( Stream ) then begin
          SAXReader.LoadFromStream( Stream ) ;
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

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := 'Google Earth KML';
    finally
      Unlock ;
      FreeObject( SAXReader ) ;
      RaiseBusyRelease( Self ) ;

      FIsModified := False ;
      lAttributes.Clear ;
    end ;
  end ;


  procedure TGIS_LayerKML.SaveData ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  procedure TGIS_LayerKML.Build( const _path   : String ;
                                 const _extent : TGIS_Extent;
                                 const _type   : TGIS_ShapeType ;
                                 const _dim    : TGIS_DimensionType) ;
  begin
    inherited ;

    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;
  end ;

  procedure TGIS_LayerKML.ImportLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent      ;
    const _type        : TGIS_ShapeType   ;
    const _scope       : String           ;
    const _shape       : TGIS_Shape       ;
    const _de9im       : String           ;
    const _truncated   : Boolean
  ) ;
  var
    same_name   : Boolean         ;
    old_scope   : String          ;
    abort       : Boolean         ;
    {$IFDEF JAVA}
      stream    : TXMLOutputStream ;
    {$ELSE}
      stream    : TGIS_FileStream ;
    {$ENDIF}
    sattributes : SAXAttributes   ;
    scale       : Double ;
  begin
    if not assigned( _layer ) then exit ;

    if not CheckFileWriteAccessEx( Path, True, True, True ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERSAVERROR ), Path, 0 ) ;

    importshapeNo   := 0 ;
    importendUid    := _layer.GetLastUid ;
    abort           := False ;

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

      // prepare and save KML file
      SAXWriter := TGIS_SAXWriter.Create( stream ) ;
      SAXWriter.Indent := True ;
      SAXWriter.StartDocument ;

      sattributes := CoSAXAttributes.Create ;
      try
        sattributes.AddAttribute( KML_NOVAL, KML_NOVAL, KML_XMLNS_KML_W,
                                  KML_NOVAL, KML_XMLNS_KML_W_WWW
                                ) ;
        startTag( KML_NOVAL, KML_NOVAL, KML_TATUKGIS_MAP, sattributes ) ;
        sattributes.RemoveAttribute( 0 ) ;
      except
      end ;

      startTag( KML_NOVAL, KML_NOVAL, KML_DOCUMENT, sattributes ) ;

      writeStyles( _layer ) ;

      try
        old_scope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        if assigned( _layer.Viewer ) then
          scale := _layer.Viewer.Ref.ScaleAsFloat
        else
          scale := 1 ;

        importExtent    := _extent ;
        importTrunc     := _truncated ;
        importShpType   := _type ;
        importLayerObj  := _layer ;

        if ( _layer.ParamsList.Count > 1 ) or
           ( not IsStringEmpty( _layer.Params.Render.Expression ) )
        then
          importStyleType := 0
        else
          importStyleType := 1 ;

        abort := not _layer.ForEach( _extent, _scope, _shape, _de9im, True, scale,
                                     {$IFNDEF OXYGENE}
                                       exportForEach
                                     {$ELSE}
                                       @exportForEach
                                     {$ENDIF}
                                   ) ;
      finally
        _layer.Scope := old_scope ;

        endTag( KML_NOVAL, KML_NOVAL, KML_DOCUMENT ) ;
        endTag( KML_NOVAL, KML_NOVAL, KML_TATUKGIS_MAP ) ;
        SAXWriter.EndDocument ;
        FreeObject( sattributes ) ;
        FreeObject( SAXWriter ) ;
        FreeObject( stream ) ;

        if abort then begin
          DeleteFile( GetTemporaryName( Path ) ) ;
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
        end ;


        if not IsOpened then begin
          Items.Clear ;
          Fields.Clear ;

          Open ;
        end;
      end ;
    finally
      abort := RaiseBusyShake( _layer, -1, -1 ) ;
      RaiseBusyRelease( _layer ) ;
    end ;

  end ;

  function TGIS_LayerKML.GetAvailableLayers : TGIS_LayerInfoList ;
  var
    ext     : String ;
    afiles  : TArray<String> ;
    i       : Integer ;
  begin
    Result := TGIS_LayerInfoList.Create ;

    ext := UpperCase( GetFileExt( Path ) ) ;
    if ext = '.KMZ' then begin
      afiles := GetZipFilesNames( Path ) ;
      for i := 0 to length(afiles)-1 do begin
        if UpperCase( GetFileExt( afiles[i] ) ) = '.KML' then
          Result.Add(
            TGIS_LayerInfo.Create(
              afiles[i],
              TGIS_RegisteredLayerType.Vector,
              TGIS_ShapeType.Unknown,
              -1
            )
          ) ;
      end ;
    end
    else
      Result.Add(
        TGIS_LayerInfo.Create(
          Name,
          TGIS_RegisteredLayerType.Vector,
          TGIS_ShapeType.Unknown,
          -1
        )
      ) ;
  end ;

  function TGIS_LayerKML.Draw : Boolean ;
  begin
    resetMustReproject ;

    if assigned( Viewer ) then
      Result := DrawEx( drawExtentEx )
    else
      Result := DrawEx( ProjectedExtent ) ;
  end ;

  function TGIS_LayerKML.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    img : TGIS_LayerPixel ;
    i   : Integer ;
  begin
    inherited DrawEx( _extent ) ;

    if assigned( Viewer ) and assigned( SubLayers ) then begin
      for i := 0 to SubLayers.Count-1 do begin
        img := SubLayers[i] as TGIS_LayerPixel ;
        img.Renderer := Renderer ;
        img.RecalcProjectedExtent ;
        img.Draw ;
      end ;
    end ;

    Result := True ;
  end ;


  { Perform initialization section.
  }
  class procedure Unit_GisLayerKML.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-KML', 'Google Earth Keyhole Markup Language',
                   TGIS_LayerKML, '.kml;.kmz',
                   TGIS_RegisteredLayerType.Vector,
                   TGIS_RegisteredFormatType.Local,
                    [ TGIS_RegisteredOperationType.Read,
                      TGIS_RegisteredOperationType.Write,
                      TGIS_RegisteredOperationType.&Create
                    ],
                   True
                 ) ;
  end ;


{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerKML.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

