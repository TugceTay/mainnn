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
  Encapsulation of a GPS Exchange Format (GPX) Layer
}

{$IFDEF DCC}
  unit GisLayerGPX ;
  {$HPPEMIT '#pragma link "GisLayerGPX"'}
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
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,

    GisClasses,
    GisTypes,
    GisLayerVector,
    GisCsSystems,
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
  Unit_GisLayerGPX = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  {$IFDEF OXYGENE}

    /// <summary>
    ///   GPX data types to use.
    /// </summary>
    TGIS_LayerGPXDataType = public flags (
      /// <summary>
      ///   Waypoint.
      /// </summary>
      WPT = 1,

      /// <summary>
      ///   Route (one segment).
      /// </summary>
      RTE = 2,

      /// <summary>
      ///   Track (multi-segment).
      /// </summary>
      TRK = 4
    ) ;

    /// <summary>
    ///   Set of GPX internal data types.
    /// </summary>
    TGIS_LayerGPXDataTypes = public TGIS_LayerGPXDataType ;
  {$ELSE}

    /// <summary>
    ///   GPX internal data types.
    /// </summary>
    TGIS_LayerGPXDataType = (
      /// <summary>
      ///   Waypoint.
      /// </summary>
      WPT,

      /// <summary>
      ///   Route (one segment).
      /// </summary>
      RTE,

      /// <summary>
      ///   Track (multi-segment).
      /// </summary>
      TRK
    ) ;

    /// <summary>
    ///   Set of GPX internal data types.
    /// </summary>
    TGIS_LayerGPXDataTypes = set of TGIS_LayerGPXDataType ;
  {$ENDIF}

  {#GENDOC:HIDE}
  /// <summary>
  ///   GPX attribute definition.
  /// </summary>
  TGIS_AttributeGPX = {$IFDEF OXYGENE} unit {$ENDIF} record
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
  ///   Encapsulation of a GPX Layer.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     <list type="bullet">
  ///       <item>
  ///         Supported shape types: TGIS_ShapeType.Point,
  ///         TGIS_ShapeType.Arc.
  ///       </item>
  ///       <item>
  ///         Upon importing shape of type TGIS_ShapeType.MultiPoint the
  ///         shape will be converted to a set of TGIS_ShapeType.Point
  ///         shapes.
  ///       </item>
  ///       <item>
  ///         Upon importing shapes of type TGIS_ShapeType.Polygon only
  ///         the outline of shape will be imported as a
  ///         TGIS_ShapeType.Arc shape.
  ///       </item>
  ///     </list>
  ///   </note>
  /// </remarks>
  TGIS_LayerGPX = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FDataType       : TGIS_LayerGPXDataTypes ;
      FAdditionalWPT  : Boolean ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private variable
      addingObject    : Boolean ;
      addingMetadata  : Boolean ;
      usingCoord      : Boolean ;
      UseAttrName     : Boolean ;
      fieldName       : String ;
      elementNo       : Cardinal;
      addingPtgInt    : Boolean;
      hadSegment      : Boolean;
      layerInfo       : String;
      nativeLayer     : Boolean ;
      currType        : String  ;
      wptCounter      : Integer ;

      SAXReader       : TGIS_SAXContentHandler ;
      SAXWriter       : TGIS_SAXWriter ;
      lAttributes   : TList<TGIS_AttributeGPX> ;
      lPtgAttr      : TList<TGIS_AttributeGPX> ;

        /// <summary>
        ///   Current shape.
        /// </summary>
        currShp       : TGIS_Shape;
        currPtg       : TGIS_Point;

        /// <summary>
        ///   Current point.
        /// </summary>
        currPShp      : TGIS_Shape;

        /// <summary>
        ///   Current shape point list.
        /// </summary>
        ptgList       : TGIS_PointList;
        currShpUid    : TGIS_Uid ;
        currShpPart   : Integer ;
        FFindLayer    : TGIS_LayerVector ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF} // various private routine

      /// <summary>
      ///   Add shape part.
      /// </summary>
      procedure addShapePart ;

      /// <summary>
      ///   Format coordinates.
      /// </summary>
      /// <param name="_val">
      ///   value
      /// </param>
      function  formatCoord           ( const _val          : Double
                                      ) : String;

      /// <summary>
      ///   Add new point.
      /// </summary>
      /// <param name="_ptg">
      ///   point coordinates
      /// </param>
      procedure addPointInternal      ( const _ptg          : TGIS_Point
                                      );

      /// <summary>
      ///   Add new point.
      /// </summary>
      /// <param name="_type">
      ///   point type
      /// </param>
      procedure addPoint              ( const _type         : String
                                      ) ;

      /// <summary>
      ///   Add new line.
      /// </summary>
      /// <param name="_type">
      ///   line type
      /// </param>
      procedure addLine               ( const _type         : String
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
      procedure addAtrToShp           ( const _atrname      : String       ;
                                        const _atrvalue     : String       ;
                                        const _shp          : TGIS_Shape
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
      procedure addAtrToArray         ( const _atrname      : String       ;
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
      procedure addAtrToArray         ( const _atrname      : String       ;
                                        const _atrvalue     : String       ;
                                        const _useAttrName  : Boolean
                                      ) ; overload;

      /// <summary>
      ///   Writes geometry data and properties to GPX File.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save (data only)
      /// </param>
      procedure writeGeometry         ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes information, properties to GPX file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeData             ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Writes Point to GPX file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      /// <param name="_part">
      ///   shape part to save
      /// </param>
      procedure writePoint            ( const _shp          : TGIS_Shape       ;
                                        const _type         : String           ;
                                        const _part         : Integer
                                      ) ; overload;

      /// <summary>
      ///   Writes Point to GPX file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      /// <param name="_part">
      ///   shape part to save
      /// </param>
      procedure writePoint            ( const _shp          : TGIS_Shape       ;
                                        const _type         : String           ;
                                        const _part         : Integer          ;
                                        const _internal     : Boolean
                                      ) ; overload;

      /// <summary>
      ///   Writes LineString to GPX file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeLine             ( const _shp          : TGIS_Shape
                                      ) ; overload;

      /// <summary>
      ///   Writes LineString to GPX file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      /// <param name="_partno">
      ///   shape part to save
      /// </param>
      /// <param name="_multi">
      ///   True if it's multiline
      /// </param>
      /// <param name="_gpxType">
      ///   type of gpx element
      /// </param>
      procedure writeLine             ( const _shp          : TGIS_Shape       ;
                                        const _partno       : Integer          ;
                                        const _multi        : Boolean          ;
                                        const _gpxType      : String
                                      ) ; overload;

      /// <summary>
      ///   Writes MultiLine to GPX file.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save
      /// </param>
      procedure writeMultiLine        ( const _shp          : TGIS_Shape
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
      ///   keeps 'GPX:xxx' schema
      /// </param>
      /// <param name="_val">
      ///   value added to QName
      /// </param>
      procedure setAttributes         ( var   _namespaceuri : String     ;
                                        var   _localname    : String     ;
                                        var   _qname        : String     ;
                                        const _val          : String
                                      );

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
      ///   keeps 'GPX:xxx' schema
      /// </param>
      /// <param name="_attributes">
      ///   element attribute value
      /// </param>
      procedure startTag              ( const _namespaceuri : String     ;
                                        const _localname    : String     ;
                                        const _qname        : String     ;
                                        const _attributes   : IMXAttributes
                                      ) ;

      /// <summary>
      ///   Save EndElement node to GPX file
      /// </summary>
      /// <param name="_namespaceuri">
      ///   NameSpace URI value
      /// </param>
      /// <param name="_localname">
      ///   LocalName value
      /// </param>
      /// <param name="_qname">
      ///   keeps 'GPX:xxx' schema
      /// </param>
      procedure endTag                ( const _namespaceuri : String     ;
                                        const _localname    : String     ;
                                        const _qname        : String
                                      ) ;

      /// <summary>
      ///   Find attributes of points used in track or route.
      /// </summary>
      /// <param name="_src">
      ///   parent shape
      /// </param>
      procedure findPoints            ( const _src          : TGIS_Shape     ;
                                        const _type         : String         ;
                                        const _part         : Integer
                                      );

      /// <summary>
      ///   Writes geometry waypoints to GPX File.
      /// </summary>
      /// <param name="_shp">
      ///   shape to save (data only)
      /// </param>
      procedure writeWPT              ( const _shp          : TGIS_Shape
                                      ) ;

      /// <summary>
      ///   Prepare layer to locate points.
      /// </summary>
      /// <param name="_layer">
      ///   source layer to copy
      /// </param>
      procedure prepareFind           ( const _layer : TGIS_LayerVector
                                      );
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp    ; override;

      procedure fset_CS               ( const _value  : TGIS_CSCoordinateSystem
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

    public

      /// <summary>
      ///   Data type to read and parse from file.
      /// </summary>
      property DataType : TGIS_LayerGPXDataTypes read  FDataType
                                                 write FDataType;

      /// <summary>
      ///   If True, additional points built on waypoints from tracks and
      ///   routes will be added to layer.
      /// </summary>
      property AdditionalWayPoints : Boolean  read  FAdditionalWPT
                                              write FAdditionalWPT;
   end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    System.Variants,
    GisFunctions,
    GisRtl,
    GisInternals,
    GisResource,
    GisStreams,
    GisUtils,
    GisRegistredLayers,
    GisCsFactory ;
{$ENDIF}

type
  { Encapsulation of sax handler for GPX files.
  }
  T_saxHandlerGPX = class( TGIS_SAXContentHandler )
     private
      // GPX layer handle.
      layerGPX : TGIS_LayerGPX ;
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
      // _strnamespaceuri  element Namespace URI
      // _strlocalname     element name
      // _strqname         element QName
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
      // Create an instance for provided GPX layer
      // _layer   GPX layer
      constructor Create( const _layer : TGIS_LayerGPX ) ;
  end ;

const
    GPX_POINT          = 'wpt' ;
    GPX_POINT_INT      = 'wptI';
    GPX_ROUTE          = 'rte';
    GPX_ROUTE_POINT    = 'rtept';
    GPX_TRACK          = 'trk';
    GPX_TRACK_SEG      = 'trkseg';
    GPX_TRACK_POINT    = 'trkpt';
    GPX_LAT            = 'lat';
    GPX_LON            = 'lon';
    GPX_METADATA       = 'metadata';
    GPX_VERSION        = 'version';
    GPX_CREATOR        = 'creator';
    GPX_TYPE_NAME      = 'GPX_TYPE';
    GPX_UID_NAME       = 'GPX_UID';
    GPX_PART_NAME      = 'GPX_PART';

   // Prefixex
    GPX                = 'GPX:' ;
    GPX_G              = 'g:' ;
    GPX_XMLNS_GPX      = 'xmlns:xsi' ;
    GPX_XMLNS_GPX_W    = 'xmlns' ;
    GPX_XMLNS_GPX_G    = 'xsi:schemaLocation' ;
    GPX_XMLNS_GPX_WWW  = 'http://www.w3.org/2001/XMLSchema-instance';
    GPX_XMLNS_GPX_W_WWW= 'http://www.topografix.com/GPX/1/1';
    GPX_XMLNS_GPX_G_WWW= 'http://www.topografix.com/GPX/1/1 ' +
                         'http://www.topografix.com/GPX/1/1/gpx.xsd' ;
    GPX_TATUKGIS_MAP   = 'gpx';
    GPX_TATUKGIS_NAME  = 'TATUKGIS';

    GPX_NOVAL : String = ' ' ;

//==============================================================================
//  TGis_SaxHandler
//==============================================================================

  constructor T_saxHandlerGPX.Create( const _layer : TGIS_LayerGPX ) ;
  begin
    inherited Create;

    layerGPX := _layer ;
  end ;

  procedure T_saxHandlerGPX.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i : Integer ;
  begin
    inherited;

    if ( ( _lname = GPX_POINT         ) and
         ( TGIS_LayerGPXDataType.WPT in layerGPX.DataType ) ) or
       ( ( _lname = GPX_ROUTE         ) and
         ( TGIS_LayerGPXDataType.RTE in layerGPX.DataType ) ) or
       ( ( _lname = GPX_TRACK         ) and
         ( TGIS_LayerGPXDataType.TRK in layerGPX.DataType ) ) then
    begin
       layerGPX.addingObject := True;
       layerGPX.lAttributes.Clear ;
    end
    else if ( _lname = GPX_METADATA ) then
      layerGPX.addingMetadata := True ;

    if layerGPX.addingObject then begin
       if ( _lname = GPX_POINT ) then
          layerGPX.addPoint( _lname )
       else if ( _lname = GPX_ROUTE ) or
               ( _lname = GPX_TRACK ) then
          layerGPX.addLine( _lname );

      if ( _lname = GPX_TRACK_SEG ) then begin
        layerGPX.addShapePart ;
        layerGPX.hadSegment := True ;
      end ;

       if ( _lname = GPX_ROUTE_POINT ) or
          ( _lname = GPX_TRACK_POINT ) then
       begin
        layerGPX.addingPtgInt := True ;
        layerGPX.lPtgAttr.Clear ;
       end ;


       {$IFDEF GIS_NORECORDS}
          if not assigned(layerGPX.currPtg) then
            layerGPX.currPtg := TGIS_Point.Create;
       {$ENDIF}

       // remember object property name
       if ( _attribs.Length > 0 ) then
       begin
         for i := 0 to _attribs.Length - 1 do begin
            if _attribs.GetLocalName( i ) = GPX_LAT then
              layerGPX.currPtg.Y := DotStrToFloat( _attribs.GetValue( i ) )
            else if _attribs.GetLocalName( i ) = GPX_LON then
              layerGPX.currPtg.X := DotStrToFloat( _attribs.GetValue( i ) );
            layerGPX.usingCoord := True ;
         end ;

         if ( _lname = GPX_POINT       ) or
            ( _lname = GPX_ROUTE_POINT ) or
            ( _lname = GPX_TRACK_POINT ) then
           layerGPX.ptgList.Add( layerGPX.currPtg ) ;
       end
       else
       begin
          layerGPX.fieldName := _lname;
       end ;
    end
    else if layerGPX.addingMetadata then
      layerGPX.fieldName := _lname;
  end ;

  procedure T_saxHandlerGPX.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  var
    i : Integer;
  begin
    inherited;

    inc( layerGPX.elementNo );
    if layerGPX.elementNo mod 200 = 0 then
      layerGPX.HourglassShake ;

    if ( ( _lname = GPX_POINT         ) and
         ( TGIS_LayerGPXDataType.WPT in layerGPX.DataType ) ) or
       ( ( _lname = GPX_ROUTE         ) and
         ( TGIS_LayerGPXDataType.RTE in layerGPX.DataType ) ) or
       ( ( _lname = GPX_TRACK         ) and
         ( TGIS_LayerGPXDataType.TRK in layerGPX.DataType ) ) then
    begin
      layerGPX.addingObject  := False ;

      if assigned( layerGPX.currShp ) then begin
        // add attributes to object
        for i := 0 to layerGPX.lAttributes.Count - 1 do
          layerGPX.addAtrToShp( layerGPX.lAttributes[i].Name,
                                layerGPX.lAttributes[i].Value,
                                layerGPX.currShp
                              ) ;
        if not ( ( _lname = GPX_TRACK ) and layerGPX.hadSegment ) then begin
          // if any other type found
          if layerGPX.usingCoord then begin
            for i := 0 to layerGPX.ptgList.Count - 1 do
              layerGPX.currShp.AddPoint( layerGPX.ptgList.Items[ i ] );

            layerGPX.usingCoord    := false;
            layerGPX.ptgList.Clear;
          end ;
        end ;

        layerGPX.lAttributes.Clear ;

        layerGPX.currShp.Unlock;
        if layerGPX.Items.Count = 1 then
          layerGPX.Extent := layerGPX.currShp.Extent ;
      end ;
    end
    else if ( _lname = GPX_TRACK_SEG     ) and
            ( TGIS_LayerGPXDataType.TRK in layerGPX.DataType ) then begin
      if layerGPX.usingCoord then begin
        for i := 0 to layerGPX.ptgList.Count - 1 do
          layerGPX.currShp.AddPoint( layerGPX.ptgList.Items[ i ] );

        layerGPX.usingCoord    := false;
        layerGPX.ptgList.Clear;
      end ;
    end
    else if ( ( _lname = GPX_ROUTE_POINT   ) and
              ( TGIS_LayerGPXDataType.RTE in layerGPX.DataType ) ) or
            ( ( _lname = GPX_TRACK_POINT   ) and
              ( TGIS_LayerGPXDataType.TRK in layerGPX.DataType ) ) then begin
      if layerGPX.FAdditionalWPT then begin
        layerGPX.addPointInternal( layerGPX.currPtg );

        for i := 0 to layerGPX.lPtgAttr.Count - 1 do
         layerGPX.addAtrToShp( layerGPX.lPtgAttr[i].Name,
                               layerGPX.lPtgAttr[i].Value,
                               layerGPX.currPShp
                             ) ;
      end ;
      layerGPX.addingPtgInt := False;

      layerGPX.lPtgAttr.Clear ;
    end
    else if ( _lname = GPX_METADATA ) then begin
      for i := 0 to layerGPX.lAttributes.Count - 1 do
          layerGPX.layerInfo := layerGPX.layerInfo +
                                layerGPX.lAttributes[i].Name +
                                ' : ' +
                                layerGPX.lAttributes[i].Value +
                                #13#10 ;
      layerGPX.addingMetadata := False;
    end
  end ;

  procedure T_saxHandlerGPX.Characters(
    const _chars   : String
  ) ;
  var
    str : String ;
  begin
    inherited;

    str := RemoveWhitesFast( _chars ) ;
    if not IsStringEmpty( Trim( str ) ) then begin
      if layerGPX.addingObject or layerGPX.addingMetadata then
          layerGPX.addAtrToArray( layerGPX.fieldName, str )
    end
    else if layerGPX.UseAttrName and layerGPX.addingObject then begin
      layerGPX.addAtrToArray( layerGPX.fieldName, str, layerGPX.UseAttrName ) ;
      layerGPX.UseAttrName := False ;
    end ;
  end ;

  procedure T_saxHandlerGPX.FatalError(
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
// GPX Layer implementation
//==============================================================================

  constructor TGIS_LayerGPX.Create;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;

    currShp        := nil;
    lAttributes    := TList<TGIS_AttributeGPX>.Create ;
    lPtgAttr       := TList<TGIS_AttributeGPX>.Create ;
    elementNo      := 0;
    addingPtgInt   := False;
    addingMetadata := False;
    FAdditionalWPT := True ;
    {$IFDEF OXYGENE}
      FDataType    := TGIS_LayerGPXDataType.WPT or
                      TGIS_LayerGPXDataType.RTE or
                      TGIS_LayerGPXDataType.TRK ;
    {$ELSE}
      FDataType    := [ TGIS_LayerGPXDataType.WPT,
                        TGIS_LayerGPXDataType.RTE,
                        TGIS_LayerGPXDataType.TRK
                      ] ;
    {$ENDIF}
    ptgList        := TGIS_PointList.Create;

    FCS := TGIS_CSFactory.ByEPSG( GIS_EPSG_WGS84 ) ;
  end ;

  procedure TGIS_LayerGPX.doDestroy ;
  begin
    FreeObject( lAttributes ) ;
    FreeObject( lPtgAttr ) ;
    FreeObject( ptgList ) ;

    inherited ;
  end ;

  procedure TGIS_LayerGPX.addPointInternal( const _ptg : TGIS_Point );
  begin
    currPShp := CreateShape( TGIS_ShapeType.Point ) ;
    currPShp.Lock( TGIS_Lock.Projection );
    currPShp.Reset;
    currPShp.AddPart;
    currPShp.AddPoint( _ptg );
    currPShp.Unlock;
    currPShp.SetField( GPX_TYPE_NAME, GPX_POINT_INT );
    currPShp.SetField( GPX_UID_NAME , currShpUid    );
    currPShp.SetField( GPX_PART_NAME, currShpPart   );
  end ;

  procedure TGIS_LayerGPX.addPoint( const _type : String );
  begin
    currShp := CreateShape( TGIS_ShapeType.Point ) ;
    currShp.Lock( TGIS_Lock.Projection );
    currShp.Reset;
    currShp.AddPart;
    currShp.SetField( GPX_TYPE_NAME, _type );
  end ;

  procedure TGIS_LayerGPX.addLine( const _type : String );
  begin
    currShp := CreateShape( TGIS_ShapeType.Arc ) ;
    currShp.Lock( TGIS_Lock.Projection ) ;
    currShp.Reset ;
    currShp.AddPart;
    currShp.SetField( GPX_TYPE_NAME, _type );
    currShpUid := currShp.Uid ;
  end ;

  procedure TGIS_LayerGPX.addShapePart;
  begin
    if assigned( currShp ) then begin
      currShp.AddPart;
      currShpPart := currShp.GetNumParts - 1;
    end ;
  end ;

  procedure TGIS_LayerGPX.addAtrToShp(  const _atrname  : String ;
                                        const _atrvalue : String ;
                                        const _shp      : TGIS_Shape
                                      ) ;
  var
    res : Integer     ;
    val : String  ;
    atr : String  ;
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

  procedure TGIS_LayerGPX.addAtrToArray( const _atrname     : String ;
                                         const _atrvalue    : String
                                        ) ;
  begin
    addAtrToArray( _atrname, _atrvalue, False ) ;
  end ;

  procedure TGIS_LayerGPX.addAtrToArray( const _atrname     : String ;
                                         const _atrvalue    : String ;
                                         const _useAttrName : Boolean
                                        ) ;
  var
    attr : TGIS_AttributeGPX ;
  begin
    if ( ( not IsStringEmpty( _atrvalue ) ) or _useAttrName ) and
       ( not IsStringEmpty( _atrname ) ) then begin
      if not addingPtgInt then begin
        attr.Name  := _atrname ;
        attr.Value := _atrvalue ;
        lAttributes.Add( attr ) ;
      end
      else begin
        attr.Name  := _atrname ;
        attr.Value := _atrvalue ;
        lPtgAttr.Add( attr ) ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerGPX.writeGeometry( const _shp : TGIS_Shape ) ;
  var
    numparts, i : Integer ;
  begin
    numparts := _shp.GetNumParts ;
    if numparts <= 0 then exit ;

    if _shp is TGIS_ShapePoint then begin
      writePoint( _shp, GPX_POINT, 0 ) ;
    end
    else if ( _shp is TGIS_ShapeMultiPoint ) then begin
      for i := 0 to _shp.GetPartSize(0) - 1 do
        writePoint( _shp, GPX_POINT, i ) ;
    end
    else if ( _shp is TGIS_ShapeArc        ) or
            ( _shp is TGIS_ShapePolygon    ) or
            ( _shp is TGIS_ShapeMultiPatch ) then begin
      if numparts = 1 then
        writeLine( _shp )
      else
        writeMultiLine( _shp ) ;
    end
    else ;// unknown object type
  end ;

  procedure TGIS_LayerGPX.writeData( const _shp : TGIS_Shape ) ;
  var
    oattributes   : SAXAttributes ;
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    chars         : String ;
    fname         : String ;
    i             : Integer    ;
    hasName       : Boolean    ;
  begin

    oattributes := CoSAXAttributes.Create ;
    try
      hasName := False;
      for i := 0 to Fields.Count-1 do begin
        if FieldInfo( i ).Deleted   then continue ;
        if FieldInfo( i ).Temporary then continue ;

        if FieldInfo( i ).FieldType = TGIS_FieldType.Date then
          chars := DateTimeToXMLString(
                                  VarToDateTime( _shp.GetField( FieldInfo( i ).NewName ) ),
                                  2, True
                                )
        else
          chars := VarToString( _shp.GetField( FieldInfo( i ).NewName ) ) ;

        if IsStringEmpty( Trim( chars ) ) then continue;

        // get field name, value and save to file
        fname := TGIS_Utils.GisCanonicalSQLName( FieldInfo( i ).Name ) ;
        if ( fname = GPX_TYPE_NAME ) or ( fname = GPX_UID_NAME ) or
           ( fname = GPX_PART_NAME ) then continue;

        fname := LowerCase( fname ) ;  // required by GPX schema
        if fname = 'name' then
          hasName := True ;

        sname := fname ;
        SAXWriter.StartElement( snamespaceuri, sqname, sname, oattributes ) ;
        SAXWriter.Characters( chars ) ;
        sname := fname ;
        SAXWriter.EndElement( snamespaceuri, sqname, sname ) ;
      end ;

      if not hasName then begin
          oattributes.Clear ;
          startTag( GPX_NOVAL, GPX_NOVAL, 'name', oattributes ) ;
            chars := Format( currType + '%d', [ _shp.Uid ] ) ;
            SAXWriter.Characters( chars );
          endTag( GPX_NOVAL, GPX_NOVAL, 'name' ) ;
      end ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGPX.writePoint( const _shp      : TGIS_Shape ;
                                      const _type     : String ;
                                      const _part     : Integer
                                    ) ;
  begin
    writePoint( _shp, _type, _part, False ) ;
  end ;

  procedure TGIS_LayerGPX.writePoint( const _shp      : TGIS_Shape ;
                                      const _type     : String ;
                                      const _part     : Integer ;
                                      const _internal : Boolean
                                    ) ;
  var
    oattributes : SAXAttributes ;
    ptg         : TGIS_Point;
  begin
    if not nativeLayer then
      // internal point
      if not _internal then
        if VarToString( _shp.GetField( GPX_TYPE_NAME ) ) = GPX_POINT_INT then exit;

    // save point

    oattributes := CoSAXAttributes.Create ;
    try
      ptg := _shp.GetPoint( 0, _part ) ;

      oattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_LAT,
                                GPX_NOVAL, formatCoord( ptg.Y )
                               ) ;
      oattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_LON,
                                GPX_NOVAL, formatCoord( ptg.X )
                               ) ;
      startTag( GPX_NOVAL, GPX_NOVAL, _type, oattributes ) ;
      writeData( _shp ) ;
      endTag( GPX_NOVAL, GPX_NOVAL, _type ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGPX.writeWPT( const _shp : TGIS_Shape );
  var
    i, j        : Integer ;
    oattributes : SAXAttributes ;
    ptg         : TGIS_Point;
    chars       : String ;
    sname       : String ;
  begin
    // skip point shapes
    if ( _shp.ShapeType = TGIS_ShapeType.Point      ) or
       ( _shp.ShapeType = TGIS_ShapeType.MultiPoint ) then exit;

    oattributes := CoSAXAttributes.Create ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      for i := 0 to _shp.GetNumParts - 1 do
        for j := 0 to _shp.GetPartSize( i ) - 1 do begin
          ptg := _shp.GetPoint( i, j ) ;
          oattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_LAT,
                                    GPX_NOVAL, formatCoord( ptg.Y )
                                   ) ;
          oattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_LON,
                                    GPX_NOVAL, formatCoord( ptg.X )
                                   ) ;
          startTag( GPX_NOVAL, GPX_NOVAL, GPX_POINT, oattributes ) ;
            oattributes.Clear ;
            startTag( GPX_NOVAL, GPX_NOVAL, 'name', oattributes ) ;
              sname := VarToString( _shp.GetField( 'name' ) ) ;
              if not IsStringEmpty( sname ) then
                sname := Copy( sname, StringFirst, 3 )
              else
              if not IsStringEmpty( Name ) then
                sname := Copy( Name, StringFirst, 3 )
              else
                sname := Copy( GetFileName( Path ), StringFirst, 3 ) ;

              chars := Format( '%s%d', [ sname, wptCounter ] ) ;
              SAXWriter.Characters( chars ) ;
            endTag( GPX_NOVAL, GPX_NOVAL, 'name' ) ;
          endTag( GPX_NOVAL, GPX_NOVAL, GPX_POINT ) ;
          inc( wptCounter );
        end ;
    finally
      _shp.Unlock ;
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGPX.writeLine( const _shp     : TGIS_Shape
                                   ) ;
  begin
    writeLine( _shp, 0, False, '' ) ;
  end ;

  procedure TGIS_LayerGPX.writeLine( const _shp     : TGIS_Shape ;
                                     const _partno  : Integer    ;
                                     const _multi   : Boolean    ;
                                     const _gpxType : String
                                   ) ;
  var
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    oattributes   : SAXAttributes ;
    gpxtype,
    pointtype     : String ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      // save line
      if nativeLayer then begin
        if IsStringEmpty( _gpxType ) then begin
          if      TGIS_LayerGPXDataType.RTE in FDataType then
                  gpxtype := GPX_ROUTE
          else if TGIS_LayerGPXDataType.TRK in FDataType then
                  gpxtype := GPX_TRACK
          else    gpxtype := '' ;
        end
        else
          gpxtype := _gpxType ;
      end
      else begin
        gpxtype := VarToString( _shp.GetField( GPX_TYPE_NAME ) ) ;
        if IsStringEmpty( gpxtype ) then
          gpxtype := GPX_TRACK ;
      end ;

      currType := UpperCase( gpxtype ) ;
      if not _multi and ( not IsStringEmpty( gpxtype ) ) then
      begin
        setAttributes( snamespaceuri, sname, sqname, gpxtype  ) ;
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes as IVBSAXAttributes
        ) ;
      end ;
        if not _multi and ( not IsStringEmpty( gpxtype ) ) then
          writeData( _shp ) ;

        if gpxtype = GPX_TRACK then begin
          startTag( GPX_NOVAL, GPX_NOVAL, GPX_TRACK_SEG, oattributes );
          pointtype := GPX_TRACK_POINT;
        end
        else if IsStringEmpty( gpxtype ) then
          pointtype := ''
        else
          pointtype := GPX_ROUTE_POINT;

            findPoints( _shp, pointtype, _partno );

        if gpxtype = GPX_TRACK then
          endTag( GPX_NOVAL, GPX_NOVAL, GPX_TRACK_SEG );

      if not _multi and ( not IsStringEmpty( gpxtype ) ) then
        SAXWriter.EndElement( snamespaceuri, sname, sqname ) ;
    finally
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGPX.writeMultiLine( const  _shp : TGIS_Shape )  ;
  var
    sname         : String ;
    sqname        : String ;
    snamespaceuri : String ;
    oattributes   : SAXAttributes ;
    numparts      : Integer ;
    i             : Integer ;
    skip          : Boolean ;
    gpxtype       : String ;
  begin
    oattributes := CoSAXAttributes.Create ;
    try
      {$IFDEF OXYGENE}
        skip := FDataType =   TGIS_LayerGPXDataType.WPT  ;
      {$ELSE}
        skip := FDataType = [ TGIS_LayerGPXDataType.WPT ];
      {$ENDIF}
      gpxtype := VarToString( _shp.GetField( GPX_TYPE_NAME ) ) ;
      if IsStringEmpty( gpxtype ) then
        gpxtype := GPX_TRACK ;

      currType := UpperCase( gpxtype ) ;

      // start multiline section
      if not nativeLayer then
        setAttributes( snamespaceuri, sname, sqname, gpxtype )
      else if not skip then
        setAttributes( snamespaceuri, sname, sqname, GPX_TRACK );

      if not skip then begin
        SAXWriter.StartElement(
          snamespaceuri, sname, sqname, oattributes as IVBSAXAttributes
        ) ;
        writeData( _shp ) ;
      end ;
      numparts := _shp.GetNumParts ;
      for  i := 0  to  numparts - 1 do begin
        // save line section
        writeLine( _shp, i, True, gpxtype );
      end ;

      // end multiline section
      if not nativeLayer then
        setAttributes( snamespaceuri, sname, sqname, gpxtype )
      else if not skip then
        setAttributes( snamespaceuri, sname, sqname, GPX_TRACK );
      if not skip then
        SAXWriter.EndElement( snamespaceuri, sname, sqname ) ;
    except
      FreeObject( oattributes ) ;
    end ;
  end ;

  procedure TGIS_LayerGPX.prepareFind( const _layer : TGIS_LayerVector );
  var
    shp : TGIS_Shape ;
  begin
    FFindLayer := TGIS_LayerVector.Create ;
    FFindLayer.ImportStructure( _layer );

    shp := _layer.FindFirst( GisWholeWorld );
    while assigned( shp ) do begin
      if assigned( shp ) then
        if ( shp.ShapeType = TGIS_ShapeType.Point             ) and
           ( VarToString( shp.GetField( GPX_TYPE_NAME ) ) = GPX_POINT_INT ) then
          FFindLayer.AddShape( shp, True );

      shp := _layer.FindNext;
    end ;
  end ;

  function TGIS_LayerGPX.formatCoord( const _val : Double ) : String;
  var
    {$IFDEF OXYGENE}
      oldSep : String ;
    {$ELSE}
      oldSep : Char ;
    {$ENDIF}
  begin
    oldSep := {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator ;
    try
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := '.' ;
      Result := Format( '%.9f', [ _val ] );
    finally
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := oldSep ;
    end ;
  end ;

  procedure TGIS_LayerGPX.findPoints( const _src    : TGIS_Shape;
                                      const _type   : String ;
                                      const _part   : Integer
                                     );
  var
    shp : TGIS_Shape ;

      procedure savePoints ;
      var
        j           : Integer ;
        partsize    : Integer ;
        oattributes : SAXAttributes ;
        ptg         : TGIS_Point ;
        sname       : String ;
        chars       : String ;
      begin
        _src.Lock( TGIS_Lock.Projection ) ;
        try
          partsize := _src.GetPartSize( _part ) ;

          oattributes := CoSAXAttributes.Create ;

          for j := 0 to partsize - 1 do begin
            ptg := _src.GetPoint( _part, j ) ;
            if not IsStringEmpty( _type ) then begin
              oattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_LAT,
                                        GPX_NOVAL, formatCoord( ptg.Y )
                                       ) ;
              oattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_LON,
                                        GPX_NOVAL, formatCoord( ptg.X )
                                       ) ;
              startTag( GPX_NOVAL, GPX_NOVAL, _type, oattributes ) ;
                oattributes.Clear ;
                startTag( GPX_NOVAL, GPX_NOVAL, 'name', oattributes ) ;
                  sname := VarToString( _src.GetField( 'name' ) ) ;
                  if not IsStringEmpty( sname ) then
                    sname := Copy( sname, StringFirst, 3 )
                  else
                  if not IsStringEmpty( Name ) then
                    sname := Copy( Name, StringFirst, 3 )
                  else
                    sname := Copy( GetFileName( Path ), StringFirst, 3 ) ;

                  chars := Format( '%s%d', [ sname, wptCounter ] ) ;
                  SAXWriter.Characters( chars ) ;
                endTag( GPX_NOVAL, GPX_NOVAL, 'name' ) ;
                inc( wptCounter );

              if _type <> GPX_POINT then
                endTag( GPX_NOVAL, GPX_NOVAL, _type ) ;

              oattributes.Clear ;
            end ;
          end ;
      finally
        _src.Unlock ;
        FreeObject( oattributes ) ;
      end ;
    end ;

  begin
    if nativeLayer then begin
      savePoints;
    end
    else begin
      shp := FFindLayer.FindFirst( GisWholeWorld, GPX_UID_NAME + '=' +
                                   QuotedStr( IntToStr( _src.Uid ) ) +
                                   ' AND ' + GPX_PART_NAME + '=' +
                                   QuotedStr( IntToStr( _part ) ) +
                                   ' AND ' + GPX_TYPE_NAME + '=' +
                                   QuotedStr( GPX_POINT_INT )
                                  );
      if not assigned( shp ) then
        savePoints
      else begin
        while assigned( shp ) do begin
          writePoint( shp, _type, 0, True );

          shp := FFindLayer.FindNext ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerGPX.setAttributes(  var   _namespaceuri : String ;
                                          var   _localname    : String ;
                                          var   _qname        : String ;
                                          const _val          : String
                                        ) ;
  begin
    _namespaceuri := GPX_NOVAL ;
    _localname    := GPX_NOVAL ;
    _qname        := _val ;
  end ;

  procedure TGIS_LayerGPX.startTag( const _namespaceuri : String ;
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

  procedure TGIS_LayerGPX.endTag( const _namespaceuri : String ;
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

  procedure TGIS_LayerGPX.fset_CS( const _value : TGIS_CSCoordinateSystem ) ;
  begin
    // only default CS can be used
  end ;

  procedure TGIS_LayerGPX.setUp ;
  begin
    inherited ;

    FSupportedShapes := GisGetEmptyShapeType ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Point ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Arc ) ;

    usingCoord    := False ;
    addingObject  := False ;
    UseAttrName   := False ;
    hadSegment    := False ;

    AddFieldInternal( GPX_TYPE_NAME, TGIS_FieldType.String, 1 , 0 );
    AddFieldInternal( GPX_UID_NAME,  TGIS_FieldType.Number, 10, 0 );
    AddFieldInternal( GPX_PART_NAME, TGIS_FieldType.Number, 10, 0 );

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    SAXReader := T_saxHandlerGPX.Create( Self ) ;
    try
      try
        if not IsStringEmpty( Path ) then begin
          if SafeFileExists( Path ) then begin
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

      FFileInfo := 'GPS Exchange Format (GPX)' + #13#10 + layerInfo;
    finally
      Unlock ;
      FreeObject( SAXReader ) ;
      RaiseBusyRelease( Self ) ;

      FIsModified := False ;
      lAttributes.Clear ;
    end ;
  end ;

  procedure TGIS_LayerGPX.SaveData ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;

  procedure TGIS_LayerGPX.Build( const _path   : String ;
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

  procedure TGIS_LayerGPX.ImportLayerEx(
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
      shp       : TGIS_Shape      ;
    {$ENDIF}
    shp_tmp     : TGIS_Shape      ;
    same_name   : Boolean         ;
    shape_no    : Integer         ;
    end_uid     : TGIS_Uid        ;
    abort       : Boolean         ;
    old_scope   : String          ;
    {$IFDEF JAVA}
      stream    : TXMLOutputStream ;
    {$ELSE}
      stream    : TGIS_FileStream ;
    {$ENDIF}
    sattributes : SAXAttributes   ;
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

      // prepare and save GPX file
      SAXWriter := TGIS_SAXWriter.Create( stream ) ;
      SAXWriter.Indent := True ;
      SAXWriter.StartDocument ;

      sattributes := CoSAXAttributes.Create ;
      try
        sattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_VERSION,
                                  GPX_NOVAL, '1.1'
                                ) ;
        sattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_CREATOR,
                                  GPX_NOVAL, GPX_TATUKGIS_NAME
                                ) ;
        sattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_XMLNS_GPX,
                                  GPX_NOVAL, GPX_XMLNS_GPX_WWW
                                ) ;
        sattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_XMLNS_GPX_W,
                                  GPX_NOVAL, GPX_XMLNS_GPX_W_WWW
                                ) ;
        sattributes.AddAttribute( GPX_NOVAL, GPX_NOVAL, GPX_XMLNS_GPX_G,
                                  GPX_NOVAL, GPX_XMLNS_GPX_G_WWW
                                ) ;

        startTag( GPX_NOVAL, GPX_NOVAL, GPX_TATUKGIS_MAP, sattributes ) ;
        sattributes.RemoveAttribute( 0 ) ;
      finally
        FreeObject( sattributes ) ;
      end ;

      // is it real GPX layer or other to import
      nativeLayer := _layer.FindField( GPX_TYPE_NAME ) = -1 ;
      try
        if not nativeLayer then
          prepareFind( _layer );

        old_scope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        if TGIS_LayerGPXDataType.WPT in FDataType then begin
          wptCounter := 1;

          for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
              _layer.Loop( _extent, _scope, _shape, _de9im ) do
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
                 writeWPT( shp_tmp ) ;
              end ;
            finally
              if shp <> shp_tmp then
                FreeObject( shp_tmp ) ;
            end ;
          end ;
        end ;

        wptCounter := 1;

        for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
            _layer.Loop( _extent, _scope, _shape, _de9im ) do
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
               writeGeometry( shp_tmp ) ;
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

      finally
        if not nativeLayer then
          FreeObject( FFindLayer );

        _layer.Scope := old_scope ;

        endTag( GPX_NOVAL, GPX_NOVAL, GPX_TATUKGIS_MAP ) ;
        SAXWriter.EndDocument ;
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
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerGPX.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-GPX', 'GPS Exchange Format',
                   TGIS_LayerGPX, '.gpx',
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
    Unit_GisLayerGPX.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================

end.

