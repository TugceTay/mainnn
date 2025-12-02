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
  Encapsulation of the DXF file access.
}

{$IFDEF DCC}
  unit GisLayerDXF ;
  {$HPPEMIT '#pragma link "GisLayerDXF"'}
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
    System.Drawing,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Generics.Defaults,
    System.Generics.Collections,
    System.Math,

    GisTypes,
    GisTypesUI,
    GisStreams,
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerDXF = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  // Matrix for rotating, scaling etc. For internal use only.
  {#gendoc:hide}
  {$IFDEF JAVA}
    T_Matrix = array[0..3] of array[0..3] of Double ;
  {$ELSE}
    T_Matrix = array[0..3,0..3] of Double ;
  {$ENDIF}

  // T_FaceTransform & T_Faces has been created to avoid Builder2006 HPP
  // generation error for construct like "array of array[0..3] of Integer".
  {#gendoc:hide}
  {$IFDEF OXYGENE}
    T_FaceTransform = array of Integer ;
  {$ELSE}
    T_FaceTransform = array [0..3] of Integer ;
  {$ENDIF}

  // T_FaceTransform & T_Faces has been created to avoid Builder2006 HPP
  // generation error for construct like "array of array[0..3] of Integer".
  {#gendoc:hide}
  T_Faces = array of T_FaceTransform ;

  //----------------------------------------------------------------------------
  // general GIS types and declarations
  //----------------------------------------------------------------------------
    {$IFDEF OXYGENE}
      /// <summary>
      ///   Provides data for the DXF extended entity definition event
      ///  ( code: 1001).
      /// </summary>
      TGIS_LayerDXFExtendedDataEventArgs = public class ( EventArgs )

        // properties internal value
        {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}
          FAppName : String          ;
          FShape   : TGIS_Shape      ;
          FData    : TGIS_StringList ;

        public
          /// <summary>
          ///   Create an event object.
          /// </summary>
          /// <param name="_appname">
          ///   application name
          /// </param>
          /// <param name="_shape">
          ///   shape to which event is related
          /// </param>
          /// <param name="_data">
          ///   strings with extended data
          /// </param>
          constructor Create  ( const _appname : String          ;
                                const _shape   : TGIS_Shape      ;
                                const _data    : TGIS_StringList
                              ) ;

        public
          /// <summary>
          ///   Application name (1001 entity creator).
          /// </summary>
          property AppName     : String
                                read FAppName ;

          /// <summary>
          ///   Shape object.
          /// </summary>
          property Shape      : TGIS_Shape
                                read FShape ;

          /// <summary>
          ///   Strings defining extended data.
          /// </summary>
          property Data       : TGIS_StringList
                                read FData ;
      end ;

      /// <summary>
      ///   Delegate for ExtendedDataEvent (code: 1001).
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_e">
      ///   event object
      /// </param>
      TGIS_LayerDXFExtendedDataEvent = public procedure(
        _sender : Object ;
        _e      : TGIS_LayerDXFExtendedDataEventArgs
      ) of object ;

    {$ELSE}

      /// <summary>
      ///   ExtendedDataEvent (code: 1001).
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_appname">
      ///   application name (1001 entity creator)
      /// </param>
      /// <param name="_shape">
      ///   shape to which event is related
      /// </param>
      /// <param name="_data">
      ///   strings with extended data
      /// </param>
      {$IFDEF GENXDK}
        TGIS_LayerDXFExtendedDataEvent = procedure(
          var _translated : Boolean     ;
              _sender     : TObject     ;
              _appname    : String      ;
              _shape      : TGIS_Shape  ;
              _data       : TStringList
        ) of object ;
      {$ELSE}
        TGIS_LayerDXFExtendedDataEvent = procedure(
          _sender  : TObject     ;
          _appname : String      ;
          _shape   : TGIS_Shape  ;
          _data    : TStringList
        ) of object ;
      {$ENDIF}
    {$ENDIF}


  /// <summary>
  ///   Layer that can read DXF file.
  /// </summary>
  TGIS_LayerDXF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private // properties

      /// <summary>
      ///   Force layer extent - cut shapes outside layer extent
      /// </summary>
      FForceExtent   : Boolean ;

      /// <summary>
      ///   Lst of layers visibility
      /// </summary>
      FLayers : TDictionary<String, TObject> ;

    private // events

      /// <summary>
      ///   Extended data event (code: 1001)
      /// </summary>
      FExtendedDataEvent : TGIS_LayerDXFExtendedDataEvent ;

    private // properties access routines

      function  fget_LayerVisibility ( const _name      : String
                                     ) : Boolean ;

      procedure fset_LayerVisibility ( const _name      : String ;
                                       const _visible   : Boolean
                                     ) ;
      {$IFDEF JAVA}
        procedure add_ExtendedDataEvent(
                                        _e : TGIS_LayerDXFExtendedDataEvent
                                       ) ;
        procedure remove_ExtendedDataEvent(
                                        _e : TGIS_LayerDXFExtendedDataEvent
                                       ) ;
      {$ENDIF}

    private

      /// <summary>
      ///   DXF file.
      /// </summary>
      dxfFile : TGIS_BufferedStream ;

      /// <summary>
      ///   is binary DXF
      /// </summary>
      isBinaryDXF : Boolean ;

      /// <summary>
      ///   Eof marker.
      /// </summary>
      dxfEof : Boolean ;

      /// <summary>
      ///   if layer found
      /// </summary>
      lfound : Boolean ;

      /// <summary>
      ///   Translated line.
      /// </summary>
      lineNo : Integer ;

      /// <summary>
      ///   Last Translated line.
      /// </summary>
      maxLineNo : Integer ;

      /// <summary>
      ///   Last position in file.
      /// </summary>
      maxPos : Integer ;

      /// <summary>
      ///   Code line.
      /// </summary>
      lineCode : Integer ;

      /// <summary>
      ///   Name line.
      /// </summary>
      lineName : String ;

      /// <summary>
      ///   Current shape.
      /// </summary>
      currShape : TGIS_Shape ;

      /// <summary>
      ///   List of blocks.
      /// </summary>
      lstBlock : TDictionary<String,Int64> ;

      /// <summary>
      ///   Matrix for rotating, scaling etc.
      /// </summary>
      arTransform : T_Matrix ;

      /// <summary>
      ///   Matrix for rotating, scaling etc of inserted element.
      /// </summary>
      arTransformInsert : T_Matrix ;

      /// <summary>
      ///   If &gt; 0 then insert operation is active.
      /// </summary>
      inInsert : Integer ;

      /// <summary>
      ///   ExtMin &amp; ExtMax parameter of GisNoWorld3D if not set.
      /// </summary>
      extMinMax : TGIS_Extent3D ;

      /// <summary>
      ///   Points buffer for internal operations.
      /// </summary>
      arPoint  : array of TGIS_Point3D ;

      /// <summary>
      ///   Faces buffer for internal operations.
      /// </summary>
      arFaces  : T_Faces ;

      /// <summary>
      ///   CAD DXF version
      /// </summary>
      iCadVersion : String ;

      /// <summary>
      ///   True if codepage should be forced by DXF version
      /// </summary>
      bForceCodePage : Boolean ;

      /// <summary>
      ///   insert point
      /// </summary>
      insert : TGIS_Point3D ;

      /// <summary>
      ///   insert scale
      /// </summary>
      scale : TGIS_Point3D ;

      /// <summary>
      ///   insert name
      /// </summary>
      insertName : String       ;

      /// <summary>
      ///   last shape color
      /// </summary>
      lastColor : Integer ;

      /// <summary>
      ///   line style list
      /// </summary>
      lstyles   : TDictionary<String, String> ;

    private // various private routines

      /// <summary>
      ///   Test if layer given by _name is visible.
      /// </summary>
      /// <param name="_name">
      ///   name of layer
      /// </param>
      function  isVisibleLayer     ( const _name      : String
                                   ) : Boolean ;

      /// <summary>
      ///   Prepare lists of DXF visual styles.
      /// </summary>
      procedure prepareLists         ;

      /// <summary>
      ///   Prepare matrix for DXF extrusion.
      /// </summary>
      /// <param name="_extrusion">
      ///   extrusion X,Y,Z
      /// </param>
      /// <remarks>
      ///   To prepare matrix for insert see prepareMatrixInsert to
      /// </remarks>
      procedure prepareMatrix      ( const _extrusion : TGIS_Point3D
                                   ) ;

      /// <summary>
      ///   Prepare matrix for DXF extrusion form BLOK.INSERT feature.
      /// </summary>
      /// <param name="_extrusion">
      ///   extrusion X,Y,Z
      /// </param>
      /// <param name="_scale">
      ///   scale for X,Y,Z
      /// </param>
      /// <param name="_rotation">
      ///   rotation in radians
      /// </param>
      /// <param name="_insert">
      ///   insertation point
      /// </param>
      /// <remarks>
      ///    To prepare matrix for standard features see prepareMatrix.
      /// </remarks>
      procedure prepareMatrixInsert( const _extrusion : TGIS_Point3D ;
                                     const _scale     : TGIS_Point3D ;
                                     const _rotation  : Double       ;
                                     const _insert    : TGIS_Point3D
                                   ) ;

      /// <summary>
      ///   Transform single point based on extrusion matrix.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be transformed
      /// </param>
      function  transformPtg       ( const _ptg       : TGIS_Point3D
                                   ) : TGIS_Point3D ;

      /// <summary>
      ///   Parse EXTMIN section.
      /// </summary>
      procedure doExtMin           ;

      /// <summary>
      ///   Parse EXTMAX section.
      /// </summary>
      procedure doExtMax           ;

      /// <summary>
      ///   Parse BLOCK section.
      /// </summary>
      procedure doBlock            ;

      /// <summary>
      ///   Parse Insert section.
      /// </summary>
      procedure doInsert           ; overload;

      /// <summary>
      ///   Parse Insert section.
      /// </summary>
      procedure doInsert           ( const _block     : String
                                   ) ; overload;

      /// <summary>
      ///   Parse extended entity definition section.
      /// </summary>
      procedure doExtendedData      ;

      /// <summary>
      ///   Parse embedded entity definition section.
      /// </summary>
      procedure doEmbeddedData      ;

      /// <summary>
      ///   Parse ADE data section.
      /// </summary>
      procedure doAde              ;

      /// <summary>
      ///   Parse SOLID.
      /// </summary>
      procedure doSolid            ;

      /// <summary>
      ///   Parse 3DFace.
      /// </summary>
      procedure do3DFace           ;

      /// <summary>
      ///   Parse LINE.
      /// </summary>
      procedure doLine             ;

      /// <summary>
      ///   Parse MLINE.
      /// </summary>
      procedure doMLine            ;

      /// <summary>
      ///   Parse Arc.
      /// </summary>
      procedure doArc              ;

      /// <summary>
      ///   Parse Ellipse.
      /// </summary>
      procedure doEllipse          ;

      /// <summary>
      ///   Parse Circle.
      /// </summary>
      procedure doCircle           ;

      /// <summary>
      ///   Parse POINT.
      /// </summary>
      procedure doPoint            ;

      /// <summary>
      ///   Parse TEXT.
      /// </summary>
      procedure doText             ;

      /// <summary>
      ///   Parse SPLINE.
      /// </summary>
      procedure doSpline           ;

      /// <summary>
      ///   Parse POLYLINE.
      /// </summary>
      procedure doPolyline         ;

      /// <summary>
      ///   Parse VERTEX for POLYLINES.
      /// </summary>
      procedure doPolylineVertex   ( const _elev : Double
                                    )  ;

      /// <summary>
      ///   Parse LWPOLYLINE (Autocad &gt;= 14).
      /// </summary>
      procedure doLwPolyline       ;

      /// <summary>
      ///   Parse MPoligon
      /// </summary>
      procedure doMPolygon         ;

      /// <summary>
      ///   Parse Hatch
      /// </summary>
      procedure doHatch            ;

      /// <summary>
      ///   Read dxf file version.
      /// </summary>
      /// <remarks>
      ///   Check dxf format version
      /// </remarks>
      procedure doVersion          ;

      /// <summary>
      ///   Read dxf file code page.
      /// </summary>
      /// <remarks>
      ///   Check dxf code page
      /// </remarks>
      procedure doCodePage         ;


      /// <summary>
      ///   Read tables section
      /// </summary>
      /// <remarks>
      ///   Used to get information about style, layers
      /// </remarks>
      procedure doTables           ;

      /// <summary>
      ///   Calculate arc params from bulge and two points.
      /// </summary>
      /// <param name="_v1">
      ///   point begin angle
      /// </param>
      /// <param name="_v2">
      ///   point end angle
      /// </param>
      /// <remarks>
      ///   See also TGIS_Layer.SaveAll.
      /// </remarks>
      procedure calculateBulge     ( const _v1        : TGIS_Point3D        ;
                                     const _v2        : TGIS_Point3D
                                   );

      /// <summary>
      ///   Build arc
      /// </summary>
      /// <param name="_center">
      ///   center point
      /// </param>
      /// <param name="_radiusA">
      ///   radius A
      /// </param>
      /// <param name="_radiusB">
      ///   radius B
      /// </param>
      /// <param name="_start">
      ///   start angle
      /// </param>
      /// <param name="_angle">
      ///   angle do be drawn
      /// </param>
      /// <remarks>
      ///   Used in arcs
      /// </remarks>
      procedure drawArc            ( const _center    : TGIS_Point3D        ;
                                     const _radiusA   : Double              ;
                                     const _radiusB   : Double              ;
                                     const _start     : Double              ;
                                     const _angle     : Double
                                   ) ;

      /// <summary>
      ///   Set shape color from color table
      /// </summary>
      /// <param name="_idx">
      ///   color index
      /// </param>
      /// <remarks>
      ///   Use element attributes to define shape color
      /// </remarks>
      procedure setShapeColor      ( const _idx       : Integer
                                   ) ;

      /// <summary>
      ///   Set shape color from color table
      /// </summary>
      /// <param name="_idx">
      ///   color index
      /// </param>
      function  getPaletteColor    ( const _idx       : Integer
                                   ) : TGIS_Color ;

      /// <summary>
      ///   Set shape line style from tables
      /// </summary>
      procedure setLineStyle       ( const _layer     : String ;
                                     const _name      : String
                                   ) ;

      /// <summary>
      ///   Get shape line style from tables
      /// </summary>
      function getLineStyle        ( const _layer     : String ;
                                     const _name      : String ;
                                       var _pattern   : String
                                    ) : TGIS_PenStyle ;

      /// <summary>
      ///   Get shape line width from tables
      /// </summary>
      function getLineWidth        ( const _val       : String
                                    ) : Integer ;

      /// <summary>
      ///   Set shape line weight from tables
      /// </summary>
      /// <param name="_name">
      ///   line wight name
      /// </param>
      /// <remarks>
      ///   Use element attributes to define shape line weight
      /// </remarks>
      procedure setLineWeight      ( const _name      : String
                                   ) ;

      /// <summary>
      ///   Fetch line form DXF file. In fact, two lines will be fetched: code
      ///   and name line.
      /// </summary>
      procedure dxfFetchLine       ;

      /// <summary>
      ///   Test if _code and _name in the current line matches.
      /// </summary>
      /// <param name="_code">
      ///   code which means end of reading
      /// </param>
      function  dxfTestLine        ( const _code      : Integer
                                   ) : Boolean ; overload;

      /// <summary>
      ///   Test if _code and _name in the current line matches.
      /// </summary>
      /// <param name="_code">
      ///   code which means end of reading
      /// </param>
      /// <param name="_name">
      ///   name which means end of reading; if empty, then only _code will be
      ///   used
      /// </param>
      function  dxfTestLine        ( const _code      : Integer             ;
                                     const _name      : String
                                   ) : Boolean ; overload;

      /// <summary>
      ///   Write a shape geometry to the output stream.
      /// </summary>
      /// <param name="_strm">
      ///   stream to be written
      /// </param>
      procedure writeGeometry      ( const _strm      : TGIS_BufferedStream ;
                                     const _shp       : TGIS_Shape
                                   ) ;

      /// <summary>
      ///   Prepare sublayers.
      /// </summary>
      procedure prepareSubLayers ;

      /// <summary>
      ///   Sort sublayers.
      /// </summary>
      procedure sortSubLayers ;

      /// <summary>
      ///   Get sublayer by name.
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      function  getSubLayerByName  ( const _name      : String
                                   ) : TGIS_LayerVector ;

      /// <summary>
      ///   Prepare layer fields.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure prepareLayerFields ( const _layer     : TGIS_LayerVector
                                   ) ;

      /// <summary>
      ///   Create sublayer.
      /// </summary>
      /// <param name="_layer">
      ///   layer data
      /// </param>
      function createSublayer     ( const _layer      : TObject
                                   ) : TGIS_LayerVector ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
    // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp           ; override;


      function fget_Layers : {$IFDEF OXYGENE}
                               TGIS_StringList ;
                             {$ELSE}
                               TStringList ;
                             {$ENDIF}
    protected
      // destructor

      /// <inheritdoc/>
      procedure doDestroy  ; override;
    public

      /// <inheritdoc/>
      constructor Create   ; override;


      /// <inheritdoc/>
      procedure Build           ( const _path      : String              ;
                                  const _extent    : TGIS_Extent         ;
                                  const _type      : TGIS_ShapeType      ;
                                  const _dim       : TGIS_DimensionType
                                ) ; override;

      /// <inheritdoc/>
      procedure ImportLayerEx   ( const _layer       : TGIS_LayerVector ;
                                  const _extent      : TGIS_Extent      ;
                                  const _type        : TGIS_ShapeType   ;
                                  const _scope       : String           ;
                                  const _shape       : TGIS_Shape       ;
                                  const _de9im       : String           ;
                                  const _truncated   : Boolean
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveData       ; override;

    public // properties

      /// <summary>
      ///   Set layer visibility by name.
      /// </summary>
      /// <param name="_name">
      ///   layer name
      /// </param>
      property LayerVisibility[ const _name : String ] : Boolean
                                           read  fget_LayerVisibility
                                           write fset_LayerVisibility ;

      /// <summary>
      ///   List of all found layers.
      /// </summary>
      property Layers : {$IFDEF OXYGENE}
                          TGIS_StringList
                        {$ELSE}
                          TStringList
                        {$ENDIF}
                        read fget_Layers ;

      /// <summary>
      ///   Force layer extent - cut shapes outside layer extent.
      /// </summary>
      property ForceExtent   : Boolean read FForceExtent
                                       write FForceExtent
                                       {$IFNDEF OXYGENE}
                                         default False
                                       {$ENDIF} ;

    published // events

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Extended entity definition data event (DXF code 1001).
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Event will context in extended entity definition data as a
        ///    list of strings ready for further parsing.
        ///   </para>
        ///   <para>
        ///     If not attached then ADE extended entity definition (if exists)
        ///     will be parsed internally.
        ///   </para>
        /// </remarks>
        event ExtendedDataEvent    : TGIS_LayerDXFExtendedDataEvent
                                     delegate FExtendedDataEvent ;
      {$ENDIF}
      {$IFDEF JAVA}
        /// <event/>
        /// <summary>
        ///   Extended entity definition data event (DXF code 1001).
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Event will context in extended entity definition data as a
        ///    list of strings ready for further parsing.
        ///   </para>
        ///   <para>
        ///     If not attached then ADE extended entity definition (if exists)
        ///     will be parsed internally.
        ///   </para>
        /// </remarks>
        event ExtendedDataEvent    : TGIS_LayerDXFExtendedDataEvent
                                     add    add_ExtendedDataEvent
                                     remove remove_ExtendedDataEvent ;
      {$ENDIF}
      {$IFDEF DCC}
        /// <event/>
        /// <summary>
        ///   Extended entity definition data event (DXF code 1001).
        /// </summary>
        /// <remarks>
        ///   <para>
        ///     Event will context in extended entity definition data as a
        ///    list of strings ready for further parsing.
        ///   </para>
        ///   <para>
        ///     If not attached then ADE extended entity definition (if exists)
        ///     will be parsed internally.
        ///   </para>
        /// </remarks>
        property ExtendedDataEvent : TGIS_LayerDXFExtendedDataEvent
                                     read  FExtendedDataEvent
                                     write FExtendedDataEvent ;
      {$ENDIF}

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Variants,
    GisInterfaces,
    GisRtl,
    GisLayer,
    GisParams,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisResource,
    GisLayerSublayer,
    GisTessellation,
    GisRegistredLayers ;
{$ENDIF}

{$IFDEF OXYGENE}
{$ELSE}
  type

    { Conversion string<->double<->integer type }
    T_val = record
      case Integer of
            0 : ( double_val : Double               ) ;
            1 : ( int_val    : Integer              ) ;
            2 : ( byte_val   : array [0..7] of Byte ) ;
    end ;
{$ENDIF}

const
  // DFX file strings

  GIS_DXF_NSEC            = '  0'           ;
  GIS_DXF_C0              =  0              ;
  GIS_DXF_C1              =  1              ;
  GIS_DXF_C10             = 10              ;
  GIS_DXF_E10             = ' 10'           ;
  GIS_DXF_C20             = 20              ;
  GIS_DXF_E20             = ' 20'           ;
  GIS_DXF_C30             = 30              ;
  GIS_DXF_E30             = ' 30'           ;
  GIS_DXF_C11             = 11              ;
  GIS_DXF_E11             = ' 11'           ;
  GIS_DXF_C12             = 12              ;
  GIS_DXF_E12             = ' 12'           ;
  GIS_DXF_C13             = 13              ;
  GIS_DXF_E13             = ' 13'           ;
  GIS_DXF_C21             = 21              ;
  GIS_DXF_E21             = ' 21'           ;
  GIS_DXF_C22             = 22              ;
  GIS_DXF_E22             = ' 22'           ;
  GIS_DXF_C23             = 23              ;
  GIS_DXF_E23             = ' 23'           ;
  GIS_DXF_C31             = 31              ;
  GIS_DXF_E31             = ' 31'           ;
  GIS_DXF_C32             = 32              ;
  GIS_DXF_E32             = ' 32'           ;
  GIS_DXF_C33             = 33              ;
  GIS_DXF_E33             = ' 33'           ;
  GIS_DXF_C38             = 38              ;
  GIS_DXF_C40             = 40              ;
  GIS_DXF_E40             = ' 40'           ;
  GIS_DXF_C41             = 41              ;
  GIS_DXF_E41             = ' 41'           ;
  GIS_DXF_C42             = 42              ;
  GIS_DXF_E42             = ' 42'           ;
  GIS_DXF_C43             = 43              ;
  GIS_DXF_C44             = 44              ;
  GIS_DXF_E43             = ' 43'           ;
  GIS_DXF_C50             = 50              ;
  GIS_DXF_E50             = ' 50'           ;
  GIS_DXF_C51             = 51              ;
  GIS_DXF_E51             = ' 51'           ;
  GIS_DXF_C70             = 70              ;
  GIS_DXF_C71             = 71              ;
  GIS_DXF_C72             = 72              ;
  GIS_DXF_C73             = 73              ;
  GIS_DXF_C74             = 74              ;
  GIS_DXF_C210            = 210             ;
  GIS_DXF_E210            = '210'           ;
  GIS_DXF_C220            = 220             ;
  GIS_DXF_E220            = '220'           ;
  GIS_DXF_C230            = 230             ;
  GIS_DXF_E230            = '230'           ;
  GIS_DXF_C1011           = 1011            ;
  GIS_DXF_E1011           = '1011'          ;
  GIS_DXF_C1021           = 1021            ;
  GIS_DXF_E1021           = '1021'          ;
  GIS_DXF_C1031           = 1021            ;
  GIS_DXF_E1031           = '1031'          ;
  GIS_DXF_CADE            = 1001            ;
  GIS_DXF_EADE            = '1001'          ;
  GIS_DXF_EADE_MAIN       = '  2'           ;
  GIS_DXF_NADE            = 'ADE'           ;
  GIS_DXF_CADE_DATA       = 1000            ;
  GIS_DXF_EADE_DATA       = '1000'          ;
  GIS_DXF_CADE_MARKER     = 1002            ;
  GIS_DXF_EADE_MARKER     = '1002'          ;
  GIS_DXF_NUONWTAP        = 'NUONWTAP'      ;
  GIS_DXF_NBEGIN          = '{'             ;
  GIS_DXF_NEND            = '}'             ;
  GIS_DXF_CSECTION        =  0              ;
  GIS_DXF_ESECTION        =  '  0'          ;
  GIS_DXF_NSECTION        = 'SECTION'       ;
  GIS_DXF_CTABLES         = 2               ;
  GIS_DXF_CNTABLES        = 0               ;
  GIS_DXF_ETABLES         = '   2'          ;
  GIS_DXF_ENTABLES        = 'ENDTAB'        ;
  GIS_DXF_NTABLES         = 'TABLES'        ;
  GIS_DXF_NLTYPE          = 'LTYPE'         ;
  GIS_DXF_CTYPE           = 2               ;
  GIS_DXF_CNTYPE          = 3               ;
  GIS_DXF_CNTYPEN         = 73              ;
  GIS_DXF_CNTYPES         = 40              ;
  GIS_DXF_CNTYPEL         = 49              ;
  GIS_DXF_ETABLE          =  '  0'          ;
  GIS_DXF_NTABLE          = 'TABLE'         ;
  GIS_DXF_NLIMIT          =  ' 70'          ;
  GIS_DXF_EAPPID          = '   0'          ;
  GIS_DXF_EAPPID_MAIN     = '   2'          ;
  GIS_DXF_NAPPID          = 'APPID'         ;
  GIS_DXF_NAPPIDLIMIT     =  '     1'       ;
  GIS_DXF_NADELIMIT       =  '    64'       ;
  GIS_DXF_EENDTAB         =  '  0'          ;
  GIS_DXF_NENDTAB         = 'ENDTAB'        ;
  GIS_DXF_CENDSEC         =  0              ;
  GIS_DXF_EENDSEC         =  '  0'          ;
  GIS_DXF_NENDSEC         = 'ENDSEC'        ;
  GIS_DXF_CHEADER         =  2              ;
  GIS_DXF_EHEADER         =  '  2'          ;
  GIS_DXF_NHEADER         = 'HEADER'        ;
  GIS_DXF_CACADVER        =  9              ;
  GIS_DXF_EACADVER        =  '  9'          ;
  GIS_DXF_NACADVER        =  '$ACADVER'     ;
  GIS_DXF_CDWGCODEPAGE    =  9              ;
  GIS_DXF_EDWGCODEPAGE    =  '  9'          ;
  GIS_DXF_NDWGCODEPAGE    =  '$DWGCODEPAGE' ;
  GIS_DXF_ECODEPAGE       =  '  3'          ;

  GIS_DXF_CEXTMIN         =  9              ;
  GIS_DXF_EEXTMIN         =  '  9'          ;
  GIS_DXF_NEXTMIN         = '$EXTMIN'       ;
  GIS_DXF_CEXTMAX         =  9              ;
  GIS_DXF_EEXTMAX         =  '  9'          ;
  GIS_DXF_NEXTMAX         = '$EXTMAX'       ;
  GIS_DXF_CENTITIES       =  2              ;
  GIS_DXF_EENTITIES       =  '  2'          ;
  GIS_DXF_NENTITIES       = 'ENTITIES'      ;
  GIS_DXF_E1              =  '  1'          ;
  GIS_DXF_CBLOCK          =  0              ;
  GIS_DXF_EBLOCK          =  '  0'          ;
  GIS_DXF_NBLOCK          =  'BLOCK'        ;
  GIS_DXF_CENDBLCK        =  0              ;
  GIS_DXF_EENDBLCK        =  '  0'          ;
  GIS_DXF_NENDBLCK        =  'ENDBLK'       ;
  GIS_DXF_CBLOCKNAME      =  2              ;
  GIS_DXF_EBLOCKNAME      =  '  2'          ;
  GIS_DXF_CHANDLE         =  5              ;
  GIS_DXF_EHANDLE         =  '  5'          ;
  GIS_DXF_TLAYER          =  2              ;
  GIS_DXF_TELAYER         =  0              ;
  GIS_DXF_CLAYER          =  8              ;
  GIS_DXF_NLAYER          =  'LAYER'        ;
  GIS_DXF_ELAYER          =  '  8'          ;
  GIS_DXF_CCOLOR          =  62             ;
  GIS_DXF_ECOLOR          =  ' 62'          ;
  GIS_DXF_CPOINT          =  0              ;
  GIS_DXF_EPOINT          =  '  0'          ;
  GIS_DXF_NPOINT          = 'POINT'         ;
  GIS_DXF_CTEXT           =  0              ;
  GIS_DXF_NTEXT           = 'TEXT'          ;
  GIS_DXF_NATTRIB         = 'ATTRIB'        ;
  GIS_DXF_NMTEXT          = 'MTEXT'         ;
  GIS_DXF_CINSERT         =  0              ;
  GIS_DXF_NINSERT         = 'INSERT'        ;
  GIS_DXF_CSOLID          =  0              ;
  GIS_DXF_NSOLID          = 'SOLID'         ;
  GIS_DXF_CLINE           =  0              ;
  GIS_DXF_NLINE           = 'LINE'          ;
  GIS_DXF_CMLINE          =  0              ;
  GIS_DXF_NMLINE          = 'MLINE'         ;
  GIS_DXF_CARC            =  0              ;
  GIS_DXF_NARC            = 'ARC'           ;
  GIS_DXF_CCIRCLE         =  0              ;
  GIS_DXF_NELLIPSE        = 'ELLIPSE'       ;
  GIS_DXF_CELLIPSE        =  0              ;
  GIS_DXF_NCIRCLE         = 'CIRCLE'        ;
  GIS_DXF_CPOLYLINE       =  0              ;
  GIS_DXF_EPOLYLINE       = '  0'           ;
  GIS_DXF_NPOLYLINE       = 'POLYLINE'      ;
  GIS_DXF_CLWPOLYLINE     =  0              ;
  GIS_DXF_NMPOLYGON       = 'MPOLYGON'      ;
  GIS_DXF_NHATCH          = 'HATCH'         ;
  GIS_DXF_NLWPOLYLINE     = 'LWPOLYLINE'    ;
  GIS_DXF_CVERTEXFOLLOW   =  66             ;
  GIS_DXF_EVERTEXFOLLOW   = ' 66'           ;
  GIS_DXF_NVERTEXFOLLOW   = ' 1'            ;
  GIS_DXF_CVERTEXATTR70   =  70             ;
  GIS_DXF_EVERTEXATTR70   = ' 70'           ;
  GIS_DXF_CVERTEXATTR71   =  71             ;
  GIS_DXF_EVERTEXATTR71   = ' 71'           ;
  GIS_DXF_CVERTEXATTR72   =  72             ;
  GIS_DXF_EVERTEXATTR72   = ' 72'           ;
  GIS_DXF_CVERTEXATTR73   =  73             ;
  GIS_DXF_EVERTEXATTR73   = ' 73'           ;
  GIS_DXF_CVERTEXATTR74   =  74             ;
  GIS_DXF_EVERTEXATTR74   = ' 74'           ;
  GIS_DXF_EVERTEXATTR0    = ' 0'            ;
  GIS_DXF_EVERTEXATTR1    = ' 1'            ;
  GIS_DXF_CVERTEX         =  0              ;
  GIS_DXF_EVERTEX         =  '  0'          ;
  GIS_DXF_NVERTEX         = 'VERTEX'        ;
  GIS_DXF_CSEQEND         =  0              ;
  GIS_DXF_ESEQEND         =  '  0'          ;
  GIS_DXF_NSEQEND         = 'SEQEND'        ;
  GIS_DXF_CEOF            =  0              ;
  GIS_DXF_EEOF            =  '  0'          ;
  GIS_DXF_NEOF            = 'EOF'           ;
  GIS_DXF_CLLSTYLE        = 6               ;
  GIS_DXF_CLLWEIGHT       = 370             ;
  GIS_DXF_E3DFACE         = '  0'           ;
  GIS_DXF_N3DFACE         = '3DFACE'        ;
  GIS_DXF_C3DFACE         =  0              ;
  GIS_DXF_NSPLINE         = 'SPLINE'        ;
  GIS_DXF_CSPLINE         =  0              ;

  // Built-in fields
  GIS_DXF_FLD_LAYER_NAME  = 'DXF_LAYER'     ;
  GIS_DXF_FLD_ELEVATION   = 'DXF_ELEVATION' ;
  GIS_DXF_FLD_LABEL       = 'DXF_LABEL'     ;
  GIS_DXF_FLD_LABEL_ANGLE = 'DXF_LABEL_ANGLE'  ;
  GIS_DXF_FLD_LABEL_HEIGHT= 'DXF_LABEL_HEIGHT' ;
  GIS_DXF_FLD_COLOR       = 'DXF_COLOR'     ;
  GIS_DXF_FLD_WEIGHT      = 'DXF_WEIGHT'    ;

  GIS_DXF_BINARY          = 'AutoCAD Binary DXF';

  DXF_METADATA_EXPORTMESH = 'TGIS_LayerDXF.ExportPolygon3DAsMesh' ;


  GIS_DXF_COLOR : array [0..255] of array[0..2] of Integer =
  (
   (255,255,255),
   (255,  0,  0), (255,255,  0), (  0,255,  0), (  0,255,255), (  0,  0,255),
   (255,  0,255), (255,255,255), (128,128,128), (192,192,192), (255,  0,  0), // 10
   (255,127,127), (204,  0,  0), (204,102,102), (153,  0,  0), (153, 76, 76), // 15
   (127,  0,  0), (127, 63, 63), ( 76,  0,  0), ( 76, 38, 38), (255, 63,  0), // 20
   (255,159,127), (204, 51,  0), (204,127,102), (153, 38,  0), (153, 95, 76), // 25
   (127, 31,  0), (127, 79, 63), ( 76, 19,  0), ( 76, 47, 38), (255,127,  0), // 30
   (255,191,127), (204,102,  0), (204,153,102), (153, 76,  0), (153,114, 76), // 35
   (127, 63,  0), (127, 95, 63), ( 76, 38,  0), ( 76, 57, 38), (255,191,  0), // 40
   (255,223,127), (204,153,  0), (204,178,102), (153,114,  0), (153,133, 76), // 45
   (127, 95,  0), (127,111, 63), ( 76, 57,  0), ( 76, 66, 38), (255,255,  0), // 50
   (255,255,127), (204,204,  0), (204,204,102), (153,153,  0), (153,153, 76), // 55
   (127,127,  0), (127,127, 63), ( 76, 76,  0), ( 76, 76, 38), (191,255,  0), // 60
   (223,255,127), (153,204,  0), (178,204,102), (114,153,  0), (133,153, 76), // 65
   ( 95,127,  0), (111,127, 63), ( 57, 76,  0), ( 66, 76, 38), (127,255,  0), // 70
   (191,255,127), (102,204,  0), (153,204,102), ( 76,153,  0), (114,153, 76), // 75
   ( 63,127,  0), ( 95,127, 63), ( 38, 76,  0), ( 57, 76, 38), ( 63,255,  0), // 80
   (159,255,127), ( 51,204,  0), (127,204,102), ( 38,153,  0), ( 95,153, 76), // 85
   ( 31,127,  0), ( 79,127, 63), ( 19, 76,  0), ( 47, 76, 38), (  0,255,  0), // 90
   (127,255,127), (  0,204,  0), (102,204,102), (  0,153,  0), ( 76,153, 76), // 95
   (  0,127,  0), ( 63,127, 63), (  0, 76,  0), ( 38, 76, 38), (  0,255, 63), // 100
   (127,255,129), (  0,204, 51), (102,204,127), (  0,153, 38), ( 76,153, 95), // 105
   (  0,127, 31), ( 63,127, 79), (  0, 76, 19), ( 38, 76, 47), (  0,255,127), // 110
   (127,255,191), (  0,204,102), (102,204,153), (  0,153, 76), ( 76,153,114), // 115
   (  0,127, 63), ( 63,127, 95), (  0, 76, 38), ( 38, 76, 57), (  0,255,191), // 120
   (127,255,223), (  0,204,153), (102,204,178), (  0,153,114), ( 76,153,133), // 125
   (  0,127, 95), ( 63,127,111), (  0, 76, 57), ( 38, 76, 66), (  0,255,255), // 130
   (127,255,255), (  0,204,204), (102,204,204), (  0,153,153), ( 76,153,153), // 135
   (  0,127,127), ( 63,127,127), (  0, 76, 76), ( 38, 76, 76), (  0,191,255), // 140
   (127,223,255), (  0,153,204), (102,178,204), (  0,114,153), ( 76,133,153), // 145
   (  0, 95,127), ( 63,111,127), (  0, 57, 76), ( 38, 66, 76), (  0,127,255), // 150
   (127,191,255), (  0,102,204), (102,153,204), (  0, 76,153), ( 76,114,153), // 155
   (  0, 63,127), ( 63, 95,127), (  0, 38, 76), ( 38, 57, 76), (  0, 63,255), // 160
   (127,159,255), (  0, 51,204), (102,127,204), (  0, 38,153), ( 76, 95,153), // 165
   (  0, 31,127), ( 63, 79,127), (  0, 19, 76), ( 38, 47, 76), (  0,  0,255), // 170
   (127,127,255), (  0,  0,204), (102,102,204), (  0,  0,153), ( 76, 76,153), // 175
   (  0,  0,127), ( 63, 63,127), (  0,  0, 76), ( 38, 38, 76), ( 63,  0,255), // 180
   (159,127,255), ( 51,  0,204), (127,102,204), ( 38,  0,153), ( 95, 76,153), // 185
   ( 31,  0,127), ( 79, 63,127), ( 19,  0, 76), ( 47, 38, 76), (127,  0,255), // 190
   (191,127,255), (102,  0,204), (153,102,204), ( 76,  0,153), (114, 76,153), // 195
   ( 63,  0,127), ( 95, 63,127), ( 38,  0, 76), ( 57, 38, 76), (191,  0,255), // 200
   (223,127,255), (153,  0,204), (178,102,204), (114,  0,153), (133, 76,153), // 205
   ( 95,  0,127), (111, 63,127), ( 57,  0, 76), ( 66, 38, 76), (255,  0,255), // 210
   (255,127,255), (204,  0,204), (204,102,204), (153,  0,153), (153, 76,153), // 215
   (127,  0,127), (127, 63,127), ( 76,  0, 76), ( 76, 38, 76), (255,  0,191), // 220
   (255,127,223), (204,  0,153), (204,102,178), (153,  0,114), (153, 76,133), // 225
   (127,  0, 95), (127, 63,111), ( 76,  0, 57), ( 76, 38, 66), (255,  0,127), // 230
   (255,127,191), (204,  0,102), (204,102,153), (153,  0, 76), (153, 76,114), // 235
   (127,  0, 63), (127, 63, 95), ( 76,  0, 38), ( 76, 38, 57), (255,  0, 63), // 240
   (255,127,159), (204,  0, 51), (204,102,127), (153,  0, 38), (153, 76, 95), // 245
   (127,  0, 31), (127, 63, 79), ( 76,  0, 19), ( 76, 38, 47), ( 51, 51, 51), // 250
   ( 91, 91, 91), (132,132,132), (173,173,173), (214,214,214), (255,255,255)  // 255
  ) ;

type
  T_Polyline = class
    class function CalculateBulge(  const _shp      : TGIS_Shape   ;
                                    const _v1       : TGIS_Point3D ;
                                    const _v2       : TGIS_Point3D ;
                                      var _center   : TGIS_Point3D ;
                                      var _radiusA  : Double       ;
                                      var _radiusB  : Double       ;
                                      var _start    : Double       ;
                                      var _stop     : Double       ;
                                      var _rotation : Double
                                   ) : Boolean ;
  end ;

  T_DxfLayer = class
    public
      Name        : String ;
      Visible     : Boolean ;
      Color       : Integer ;
      Width       : Integer ;
      PenStyle    : TGIS_PenStyle ;
      PenName     : String ;
      PenPattern  : String ;
      NativeLayer : TObject ;
    public
      class function CreateDefault( const _name : String ) : TObject ;
  end ;

  // B-spline interpolation of control points using de Boor's algorithm.
  TDXF_BSplineInterpolator = class
    private
      degree  : Integer ;
      pts     : array of TGIS_Point3D ;
      weights : array of Double ;
      knots   : array of Double ;
      domain  : array of Integer ;
    public
      function Prepare(
        const _degree  : Integer;
        const _points  : array of TGIS_Point3D;
        const _knots   : array of Double ;
        const _weights : array of Double
      ) : Boolean ;

      function InterpolatePoint(
        const _t : Double
      ) : TGIS_Point3D ;
  end ;

  {$IFDEF JAVA}
  T_listSortByName = class( java.util.Comparator<TGIS_LayerAbstract> )
    public
      function    compare ( _item1    : TGIS_LayerAbstract ;
                            _item2    : TGIS_LayerAbstract
                          ) : Integer ;
  end ;

  function T_listSortByName.compare(
    _item1 : TGIS_LayerAbstract ;
    _item2 : TGIS_LayerAbstract
  ) : Integer ;
  begin
    Result := sort_by_name( _item1, _item2 ) ;
  end ;
  {$ENDIF}

{ T_DxfLayer }

  class function T_DxfLayer.CreateDefault( const _name : String ) : TObject;
  var
    la : T_DxfLayer ;
  begin
    la := T_DxfLayer.Create ;
    la.Name     := _name ;
    la.Visible  := True ;
    la.Color    := 1 ;
    la.Width    := 1 ;
    la.PenStyle := TGIS_PenStyle.Solid ;
    la.PenPattern := '' ;
    la.PenName := '' ;
    la.NativeLayer := nil ;

    Result := la ;
  end ;

//=============================================================================
// T_Polyline
//=============================================================================

  class function T_Polyline.CalculateBulge(
    const _shp      : TGIS_Shape ;
    const _v1       : TGIS_Point3D ;
    const _v2       : TGIS_Point3D ;
      var _center   : TGIS_Point3D ;
      var _radiusA  : Double       ;
      var _radiusB  : Double       ;
      var _start    : Double       ;
      var _stop     : Double       ;
      var _rotation : Double
   ) : Boolean ;
  var
    len       : Double ;
    radius    : Double ;
    h         : Double ;
    bulge     : Double ;
    saggita   : Double ;
    apo       : Double ;
    clockwise : Boolean ;
    v         : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    midpoint  : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    pperp     : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    nl        : Double ;
    linedir   : Double ;
    a         : Double ;
    arccenter     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    arcradius     : Double ;
    arcstartangle : Double ;
    arcendangle   : Double ;
    arcrotation   : Double ;
  begin
    Result := False ;
    len := Sqrt( Sqr( _v2.X - _v1.X ) + Sqr( _v2.Y - _v1.Y) ) ;
    if ( len = 0 ) or ( _v1.M = 0 ) then exit ;

    bulge := _v1.M ;
    h := (bulge * len) / 2 ;
    radius := (h/2) + (len*len / (8*h)) ;
    arcradius := Abs(radius) ;

    clockwise := (bulge < 0) ;
    if clockwise then
      bulge := bulge * -1 ;

    saggita := Abs( bulge * (len / 2.0) ) ;
    if clockwise then
      apo := -(arcradius - saggita)
    else
      apo := -(saggita - arcradius) ;

    v.X := _v1.X - _v2.X ;
    v.Y := _v1.Y - _v2.Y ;

    midpoint.X := _v2.X + 0.5 * v.X ;
    midpoint.Y := _v2.Y + 0.5 * v.Y ;
    pperp.X := v.Y ;
    pperp.Y := -v.X ;
    nl := Sqrt( pperp.X*pperp.X + pperp.Y*pperp.Y ) ;
    pperp.X := pperp.X / nl ;
    pperp.Y := pperp.Y / nl ;

    arccenter.X := midpoint.X + (pperp.X * apo) ;
    arccenter.Y := midpoint.Y + (pperp.Y * apo) ;
    arccenter.Z := _v1.Z ;
    arccenter.M := 0 ;
    if _v2.Y > _v1.Y then
      linedir := 1
    else
      linedir := -1 ;

    a := ArcTan2((arccenter.Y - _v1.Y), (arccenter.X - _v1.X)) * 180.0 / Pi ;
    if clockwise and (linedir = 1.0) then
      a := a + (linedir * 180.0);

    if a > 0.0 then
      arcstartangle := -(a - 180.0)
    else
      arcstartangle := -(a + 180.0) ;

    a := ArcTan2((arccenter.Y - _v2.Y), (arccenter.X - _v2.X)) * 180.0 / Pi ;
    if clockwise and (linedir = 1.0) then
      a := a + (linedir * 180.0);

    if a > 0.0 then
      arcendangle := -(a - 180.0)
    else
      arcendangle := -(a + 180.0) ;

    if not clockwise and (arcstartangle < arcendangle) then
      arcendangle := -180 + (linedir * a) ;

    if (clockwise and (arcstartangle > arcendangle)) then
      arcendangle := arcendangle + 360.0;

    arcrotation := 0 ;
    if clockwise and (linedir = 1.0) then
      arcrotation := (linedir * 180.0);

    _center   := arccenter ;
    _radiusA  := arcradius ;
    _radiusB  := arcradius ;
    _start    := DegToRad( arcstartangle ) ;
    _stop     := DegToRad( arcendangle ) ;
    _rotation := DegToRad( arcrotation ) ;
    Result := True ;

  end ;

//=============================================================================
// TDXF_BSplineInterpolator
//=============================================================================

  function TDXF_BSplineInterpolator.Prepare(
    const _degree  : Integer;
    const _points  : array of TGIS_Point3D;
    const _knots   : array of Double ;
    const _weights : array of Double
  ) : Boolean ;
  var
    n, i, j : Integer ;
  begin
    Result := False ;

    n := length( _points ) ;

    if (_degree < 1)     then exit ;
    if (_degree > (n-1)) then exit ;

    SetLength( pts, length( _points ) ) ;
    for i := 0 to length(_points)-1 do
      pts[i] := _points[i] ;

    degree := _degree ;

    SetLength( weights, n ) ;
    if (length(_weights) = 0) then begin
      for i := 0 to n-1 do
        weights[i] := 1.0 ;
    end
    else begin
      for i := 0 to n-1 do
        weights[i] := _weights[i] ;
    end ;

    if (length(_knots) = 0) then begin
      // build knot vector of length [n + degree + 1]
      SetLength( knots, n+degree+1 ) ;
      for i := 0 to n+degree do
        knots[i] := i ;
    end
    else begin
      SetLength( knots, n+degree+1 ) ;
      if (length(_knots) <> (n+degree+1)) then begin
        j := 1 ;
        for i := degree+1 to (n+degree) do begin
          knots[i] := j ;
          if (i mod degree) = 0 then
            inc(j) ;
        end ;
      end
      else begin
        for i := 0 to length(_knots)-1 do
          knots[i] := _knots[i] ;
      end ;
    end ;

    SetLength( domain, 2 ) ;
    domain[0] := degree ;
    domain[1] := length(knots)-1 - degree ;

    Result := True ;
  end;

  function TDXF_BSplineInterpolator.InterpolatePoint(
    const _t : Double
  ) : TGIS_Point3D ;
  var
    i, s, l, n : Integer ;
    lo, hi, alpha, t : Double ;
    v : array of TGIS_Point3D ;
  begin
    // remap t to the domain where the spline is defined
    lo := knots[domain[0]] ;
    hi := knots[domain[1]] ;
    t := _t * (hi - lo) + lo ;

    assert(not ((t < lo) or (t > hi)), 'out of bounds') ;

    // find s (the spline segment) for the [t] value provided
    s := domain[0] ;
    while (s < domain[1] ) do begin
      if (t >= knots[s]) and (t <= knots[s+1]) then
        break;
      s := s + 1 ;
    end ;

    n := length(pts) ;
    // convert points to homogeneous coordinates
    SetLength( v, n ) ;
    for i := 0 to n-1 do begin
      {$IFDEF GIS_NORECORDS}
      v[i] := new TGIS_Point3D() ;
      {$ENDIF}
      v[i].X := pts[i].X * weights[i] ;
      v[i].Y := pts[i].Y * weights[i] ;
      v[i].Z := pts[i].Z * weights[i] ;
      v[i].M := weights[i] ;
    end ;

    // l (level) goes from 1 to the curve degree + 1
    for l := 1 to degree+1 do begin
      // build level l of the pyramid
      i := s ;
      while (i > s-degree-1+l ) do begin
        alpha := (t - knots[i]) / (knots[i+degree+1-l] - knots[i]);
        // interpolate each component
        v[i].X := (1 - alpha) * v[i-1].X + alpha * v[i].X;
        v[i].Y := (1 - alpha) * v[i-1].Y + alpha * v[i].Y;
        v[i].Z := (1 - alpha) * v[i-1].Z + alpha * v[i].Z;
        v[i].M := (1 - alpha) * v[i-1].M + alpha * v[i].M;
        i := i - 1 ;
      end ;
    end ;

    // convert back to cartesian and return
    {$IFDEF GIS_NORECORDS}
    Result := new TGIS_Point3D() ;
    {$ENDIF}
    Result.X := v[s].X / v[s].M ;
    Result.Y := v[s].Y / v[s].M ;
    Result.Z := v[s].Z / v[s].M ;
    Result.M := 0 ;
  end;

//=============================================================================
// Utilities
//=============================================================================

  // Supporting routines for matrix operations.
  // Prepare null transformation coordinate.
  // return  prepared matrix
  function mtrx_fill : T_Matrix ;
  var
    i, j : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new T_Matrix ;
      InitializeDoubleArray(Result, 4) ;
    {$ENDIF}
    for i:=0 to 3 do
      for j:=0 to 3 do
        if i=j then Result[i,j] := 1
               else Result[i,j] := 0 ;
  end ;

  // Supporting routines for matrix operations.
  // Normalize coordinate.
  // _pt      coordinate to be normalized
  // return   normalized coordinate
  function mtrx_normalize( const _pt : TGIS_Point3D ) : TGIS_Point3D ;
  var
    dvd : Double ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    dvd := Sqrt( Sqr( _pt.X ) + Sqr( _pt.Y ) + Sqr( _pt.Z ) ) ;
    Result.X := _pt.X / dvd ;
    Result.Y := _pt.Y / dvd ;
    Result.Z := _pt.Z / dvd ;
  end ;

  // Supporting routines for matrix operations.
  // Cross two coordinates.
  // _pt1    first coordinate to be crossed
  // _pt2    second coordinate to be crossed
  // return  crossed coordinate
  function mtrx_cross( const _pt1, _pt2 : TGIS_Point3D ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _pt1.Y * _pt2.Z - _pt1.Z * _pt2.Y ;
    Result.Y := _pt1.Z * _pt2.X - _pt1.X * _pt2.Z ;
    Result.Z := _pt1.X * _pt2.Y - _pt1.Y * _pt2.X ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare rotation matrix based on provided angle.
  // _dcos   cosines of rotation angle
  // _dsin   sinus rotation angle
  // return  rotation matrix
  function mtrx_rotate( const _dcos, _dsin : Double ) : T_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 0, 0 ] :=   _dcos ;
    Result[ 0, 1 ] := - _dsin ;
    Result[ 1, 0 ] :=   _dsin ;
    Result[ 1, 1 ] :=   _dcos ;
  end ;

  // Supporting routines for matrix operations.
  // Multiply matrices.
  // _mtrx1  first  matrix to be multiplied
  // _mtrx1  second matrix to be multiplied
  // return  resulting matrix
  function mtrx_multiply( const _mtrx1 , _mtrx2 : T_Matrix) : T_Matrix ;
  var
    i,j : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new T_Matrix ;
      InitializeDoubleArray(Result, 4) ;
    {$ENDIF}
    for i := 0 to 3 do begin
      for j := 0 to 3 do
        Result[ i, j ] := _mtrx1[ i, 0 ] * _mtrx2[ 0, j ] +
                          _mtrx1[ i, 1 ] * _mtrx2[ 1, j ] +
                          _mtrx1[ i, 2 ] * _mtrx2[ 2, j ] +
                          _mtrx1[ i, 3 ] * _mtrx2[ 3, j ] ;
    end ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare scaling matrix based on _scale.
  // _scale  scale
  // return  scaled matrix
  function mtrx_scale( const _scale : TGIS_Point3D ) : T_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 0, 0 ] := _scale.X ;
    Result[ 1, 1 ] := _scale.Y ;
    Result[ 2, 2 ] := _scale.Z ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare translation matrix based on _translate vector.
  // _translate translation vector
  // return     translation matrix
  function mtrx_translate( const _translate : TGIS_Point3D ) : T_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 3, 0 ] := _translate.X ;
    Result[ 3, 1 ] := _translate.Y ;
    Result[ 3, 2 ] := _translate.Z ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare inverse translation matrix based on _translate vector.
  // _translate translation vector
  // return     translation matrix
  function mtrx_inv_translate( const _translate : TGIS_Point3D ) : T_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 3, 0 ] := -_translate.X ;
    Result[ 3, 1 ] := -_translate.Y ;
    Result[ 3, 2 ] := -_translate.Z ;
  end ;

  // Supporting routines for matrix operations.
  // Prepare transformation matrix based on provided vectors.
  // _vx    transformation vector
  // _vy    transformation vector
  // _vz    transformation vector
  // return transformation matrix
  function mtrx_transform( const _vx, _vy, _vz : TGIS_Point3D ) : T_Matrix ;
  begin
    Result := mtrx_fill ;
    Result[ 0, 0 ] := _vx.X ;
    Result[ 1, 0 ] := _vy.X ;
    Result[ 2, 0 ] := _vz.X ;
    Result[ 0, 1 ] := _vx.Y ;
    Result[ 1, 1 ] := _vy.Y ;
    Result[ 2, 1 ] := _vz.Y ;
    Result[ 0, 2 ] := _vx.Z ;
    Result[ 1, 2 ] := _vy.Z ;
    Result[ 2, 2 ] := _vz.Z ;
  end ;

//=============================================================================
// TGIS_LayerDXFExtendedDataEvent
//=============================================================================

{$IFDEF OXYGENE}
  constructor TGIS_LayerDXFExtendedDataEventArgs.Create(
    const _appname : String          ;
    const _shape   : TGIS_Shape      ;
    const _data    : TGIS_StringList
  ) ;
  begin
    inherited Create ;

    FAppName := _appname ;
    FShape   := _shape   ;
    FData    := _data    ;
  end;
{$ENDIF}


//=============================================================================
// TGIS_LayerDXF
//=============================================================================

  constructor TGIS_LayerDXF.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory  ,
                             TGIS_LayerSubType.Exportable
                           ] ;

    FLayers := TDictionary<String,TObject>.Create ;
    lstyles := TDictionary<String, String>.Create ;

    inInsert := 0 ;
    FForceExtent := True ;

    FCodePage := GisSystemCodePage ;

    SupportsAutoStyle := False ;
  end ;

  procedure TGIS_LayerDXF.doDestroy ;
  {$IFNDEF OXYGENE}
    var
      itm : TPair<String,TObject> ;
  {$ENDIF}
  begin
    for itm in FLayers do
      FreeObjectNotNil( itm.Value ) ;

    FreeObject( FLayers ) ;
    FreeObject( lstyles ) ;

    // those object can be alredy destroyed
    FreeObject( dxfFile  ) ;
    FreeObject( lstBlock ) ;

    inherited ;
  end ;

  function TGIS_LayerDXF.fget_LayerVisibility(
    const _name : String
  ) : Boolean ;
  var
    la : TObject ;
  begin
    Result := False ;
    lfound := False ;

    if FLayers.TryGetValue( _name, la ) then begin
      lfound := True ;
      Result := T_DxfLayer(la).Visible ;
    end ;
  end ;

  procedure TGIS_LayerDXF.fset_LayerVisibility(
    const _name    : String ;
    const _visible : Boolean
  ) ;
  var
    la : TObject ;
  begin
    if FLayers.TryGetValue( _name, la ) then
      T_DxfLayer(la).Visible := _visible ;
  end ;

  {$IFDEF JAVA}
    procedure TGIS_LayerDXF.add_ExtendedDataEvent(
      _e : TGIS_LayerDXFExtendedDataEvent
    ) ;
    begin
      FExtendedDataEvent := _e ;
    end;

    procedure TGIS_LayerDXF.remove_ExtendedDataEvent(
      _e : TGIS_LayerDXFExtendedDataEvent
    ) ;
    begin
      FExtendedDataEvent := nil ;
    end;
  {$ENDIF}

  function TGIS_LayerDXF.isVisibleLayer(
    const _name : String
  ) : Boolean ;
  var
    obj : TObject ;
  begin
    if FLayers.TryGetValue( _name, obj ) then
      Result := T_DxfLayer(obj).Visible
    else begin
      Result := True ;

      obj := T_DxfLayer.CreateDefault( _name ) ;
      FLayers.AddOrSetValue( _name, obj ) ;
    end ;
  end ;

  procedure TGIS_LayerDXF.prepareLists ;
  begin
    lstBlock  := TDictionary<String, Int64>.Create(
                     {$IFDEF OXYGENE}
                       {$IFDEF JAVA}
                         java.lang.String.CASE_INSENSITIVE_ORDER
                       {$ENDIF}
                       {$IFDEF CLR}
                         StringComparer.OrdinalIgnoreCase
                       {$ENDIF}
                     {$ELSE}
                       TIStringComparer.Ordinal
                     {$ENDIF}
                  ) ;
  end ;

  procedure TGIS_LayerDXF.prepareMatrix(
    const _extrusion : TGIS_Point3D
  ) ;
  var
    vx : TGIS_Point3D ;
    vy : TGIS_Point3D ;
  begin
    arTransform := mtrx_fill ;

    if not GisIsSamePoint3D( _extrusion, GisPoint3D( 0, 0, 1 ) ) then begin

      if ( Abs( _extrusion.X ) < 1.0/64 ) and ( Abs( _extrusion.Y) < 1.0/64 ) then
        vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 1, 0 ), _extrusion) )
      else
        vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 0, 1 ), _extrusion) ) ;

      vy  := mtrx_normalize( mtrx_cross( _extrusion, vx ) ) ;

      arTransform := mtrx_transform( vx, vy, _extrusion ) ;
    end ;

    if inInsert > 0 then
      arTransform := mtrx_multiply( arTransform, arTransformInsert ) ;
  end ;

  procedure TGIS_LayerDXF.prepareMatrixInsert(
    const _extrusion : TGIS_Point3D ;
    const _scale     : TGIS_Point3D ;
    const _rotation  : Double       ;
    const _insert    : TGIS_Point3D
  ) ;
  var
    vx : TGIS_Point3D ;
    vy : TGIS_Point3D ;
  begin
    arTransformInsert := mtrx_fill ;

    if ( Abs( _extrusion.X ) < 1.0/64 ) and ( Abs( _extrusion.Y) < 1.0/64 ) then
      vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 1, 0 ), _extrusion) )
    else
      vx := mtrx_normalize( mtrx_cross( GisPoint3D( 0, 0, 1 ), _extrusion) ) ;

    vy  := mtrx_normalize( mtrx_cross( _extrusion, vx ) ) ;

    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_scale( _scale )
                         ) ;
    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_rotate( Cos( -_rotation ), Sin( -_rotation ) )
                         ) ;
    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_translate( _insert )
                         ) ;
    arTransformInsert := mtrx_multiply( arTransformInsert,
                           mtrx_transform( vx, vy, _extrusion )
                        ) ;
  end ;


  { Compare layers by name.
  }
  function sort_by_name( const _item1, _item2 : TGIS_LayerAbstract ) : Integer ;
  begin
    Result := CompareText( TGIS_Layer(_item1).Name, TGIS_Layer(_item2).Name ) ;
  end ;

  procedure TGIS_LayerDXF.prepareSubLayers ;
  var
    i : Integer ;
    {$IFNDEF OXYGENE}
      itm : TPair<String,TObject> ;
    {$ENDIF}
  begin
    if not assigned( SubLayers ) then
      SubLayers := TGIS_LayerAbstractList.Create( False ) ;

    for i := 0 to FSubLayers.Count-1 do
      FreeObjectNotNil( TGIS_Layer( FSubLayers[i] ) ) ;

    SubLayers.Clear ;
    for itm in FLayers do
      createSublayer( itm.Value ) ;
  end ;

  procedure TGIS_LayerDXF.sortSubLayers ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        java.util.Collections.sort( SubLayers, new T_listSortByName ) ;
      {$ELSE}
        SubLayers.Sort( @sort_by_name );
      {$ENDIF}
    {$ELSE}
      SubLayers.Sort( TComparer<TGIS_LayerAbstract>.Construct( sort_by_name ) );
    {$ENDIF}
  end ;

  function TGIS_LayerDXF.createSublayer(
    const _layer : TObject
  ) : TGIS_LayerVector ;
  var
    lv        : TGIS_LayerSublayerVector ;
    la        : T_DxfLayer ;
    clvwr, cl : TGIS_Color ;
    r,g,b     : Byte ;
    vr,vg,vb  : Byte ;
  begin
    la := T_DxfLayer( _layer ) ;
    lv := TGIS_LayerSublayerVector.Create ;

    lv.Name      := la.Name ;
    lv.Caption   := la.Name ;
    lv.Active    := la.Visible ;
    lv.Viewer    := Viewer;
    lv.Renderer  := Renderer ;

    lv.SupportedShapes  := GisGetEmptyShapeType ;
    lv.SupportedShapes  := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Point ) ;
    lv.SupportedShapes  := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.MultiPoint ) ;
    lv.SupportedShapes  := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Arc ) ;
    lv.SupportedShapes  := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Polygon ) ;
    lv.SupportedShapes  := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.MultiPatch ) ;

    lv.Params.Assign( Self.Params );
    lv.IgnoreShapeParams        := IgnoreShapeParams ;
    lv.UseParentParams          := False ;
    lv.ParentLayer              := Self ;
    lv.Params.Marker.SmartSize  := 0 ;
    lv.Params.Labels.SmartSize  := 0 ;
    lv.Params.Ground            := TGIS_3DGroundType.AboveZero ;
    lv.Params.Labels.Alignment  := TGIS_LabelAlignment.LeftJustify ;

    SubLayers.Add( lv ) ;

    if assigned( Viewer ) then
      clvwr :=  Viewer.Ref.Color
    else
      clvwr := TGIS_Color.White ;

    cl := getPaletteColor( la.Color ) ;
    r := cl.R ;
    g := cl.G ;
    b := cl.B ;

    vr := clvwr.R ;
    vg := clvwr.G ;
    vb := clvwr.B ;

    if ( Abs( vr - r ) < 16 ) and
       ( Abs( vg - g ) < 16 ) and
       ( Abs( vb - b ) < 16 )
    then // color very close to the window color
      cl := TGIS_Color.FromRGB( r xor vr, g xor vg, b xor vb ) ;

    lv.Params.Marker.SmartSize  := 0 ;
    lv.Params.Marker.Color      := cl ;
    lv.Params.Marker.Size       := 1;
    lv.Params.Line.Color        := cl;
    lv.Params.Line.Style        := la.PenStyle;
    if not IsStringEmpty( la.PenPattern ) then
      lv.Params.Line.StyleAsText := GIS_PARAMTXT_TYPE_SYMBOL + ':' + la.PenPattern ;
    lv.Params.Line.Width        := TruncS(la.Width);
    lv.Params.Area.Color        := cl;
    lv.Params.Area.OutlineColor := cl;
    lv.Params.Area.OutlineStyle := la.PenStyle;
    if not IsStringEmpty( la.PenPattern ) then
      lv.Params.Area.OutlineStyleAsText := GIS_PARAMTXT_TYPE_SYMBOL + ':' + la.PenPattern ;
    lv.Params.Area.Pattern      := TGIS_BrushStyle.Solid ;
    lv.Params.Ground            := TGIS_3DGroundType.AboveZero ;
    lv.Params.Labels.Alignment  := TGIS_LabelAlignment.LeftJustify ;
    lv.Params.Labels.Allocator  := False ;
    lv.Params.Labels.Duplicates := True ;
    lv.Params.Labels.Color      := lv.Params.Marker.Color ;
    lv.Params.Labels.FontColor  := lv.Params.Marker.Color ;
    lv.Params.Labels.Position   := GisGetLabelPosition( TGIS_LabelPosition.UpRight ) ;
    lv.Params.Labels.SmartSize  := 0 ;
    lv.Params.Labels.Field      := GIS_DXF_FLD_LABEL ;

    la.NativeLayer := lv ;

    prepareLayerFields( lv );
    Result := lv ;
  end ;

  procedure TGIS_LayerDXF.prepareLayerFields(
    const _layer : TGIS_LayerVector
  ) ;
  begin
    if _layer.FindField( GIS_DXF_FLD_LAYER_NAME )   < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_LAYER_NAME  , TGIS_FieldType.String,  1, 0 ) ;
    if _layer.FindField( GIS_DXF_FLD_ELEVATION )    < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_ELEVATION   , TGIS_FieldType.Number, 18, 6 ) ;
    if _layer.FindField( GIS_DXF_FLD_LABEL )        < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_LABEL       , TGIS_FieldType.String,  1, 0 ) ;
    if _layer.FindField( GIS_DXF_FLD_LABEL_ANGLE )  < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_LABEL_ANGLE , TGIS_FieldType.Number,  5, 6 ) ;
    if _layer.FindField( GIS_DXF_FLD_COLOR )        < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_COLOR       , TGIS_FieldType.Number,  5, 0 ) ;
    if _layer.FindField( GIS_DXF_FLD_WEIGHT )       < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_WEIGHT      , TGIS_FieldType.Number,  5, 0 ) ;
    if _layer.FindField( GIS_DXF_FLD_LABEL_HEIGHT ) < 0 then
      _layer.AddFieldInternal( GIS_DXF_FLD_LABEL_HEIGHT, TGIS_FieldType.Number,  5, 0 ) ;
  end ;

  function TGIS_LayerDXF.getSubLayerByName(
    const _name : String
  ) : TGIS_LayerVector ;
  var
    obj : TObject ;
  begin
    Result := Self ;

    if FLayers.TryGetValue( _name, obj ) then begin
      Result := T_DxfLayer(obj).NativeLayer as TGIS_LayerVector ;
      if not assigned( Result ) then
        Result := createSublayer( obj ) ;
    end
    else begin
      obj := T_DxfLayer.CreateDefault( _name ) ;
      FLayers.Add( _name, obj ) ;
      Result := createSublayer( obj ) ;
    end ;
  end ;

  function TGIS_LayerDXF.transformPtg(
    const _ptg : TGIS_Point3D
  ) : TGIS_Point3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point3D ;
    {$ENDIF}
    Result.X := _ptg.X * arTransform[ 0, 0 ] +
                _ptg.Y * arTransform[ 1, 0 ] +
                _ptg.Z * arTransform[ 2, 0 ] +
                         arTransform[ 3, 0 ] ;
    Result.Y := _ptg.X * arTransform[ 0, 1 ] +
                _ptg.Y * arTransform[ 1, 1 ] +
                _ptg.Z * arTransform[ 2, 1 ] +
                         arTransform[ 3, 1 ] ;
    Result.Z := _ptg.X * arTransform[ 0, 2 ] +
                _ptg.Y * arTransform[ 1, 2 ] +
                _ptg.Z * arTransform[ 2, 2 ] +
                         arTransform[ 3, 2 ] ;
    Result.M := _ptg.M                       ;
  end ;

  procedure TGIS_LayerDXF.doExtMin ;
  begin
    dxfFetchLine ;
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
    begin
      if      dxfTestLine( GIS_DXF_C10, '' ) then begin
                extMinMax.XMin := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                extMinMax.YMin := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                extMinMax.ZMin := DotStrToFloat( lineName ) ;
                break ;
              end ;
      dxfFetchLine ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doExtMax ;
  begin
    dxfFetchLine ;
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
    begin
      if      dxfTestLine( GIS_DXF_C10, '' ) then begin
                extMinMax.XMax := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                extMinMax.YMax := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                extMinMax.ZMax := DotStrToFloat( lineName ) ;
                break ;
              end ;
      dxfFetchLine ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doBlock ;
  var
    ipos  : Integer ;
    first : Boolean ;
  begin
    ipos  := dxfFile.Position ;
    first := True ;
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_CENDBLCK , GIS_DXF_NENDBLCK ) and
            not dxfTestLine( GIS_DXF_C1       , GIS_DXF_NENDBLCK ) ) do
    begin
      if first and dxfTestLine( GIS_DXF_CBLOCKNAME, '' ) then begin
        lstBlock.AddOrSetValue( lineName, ipos ) ;
        first := False ;
      end ;
      dxfFetchLine ;
    end ;

  end ;

  procedure TGIS_LayerDXF.doInsert ;
  begin
    doInsert( '' ) ;
  end ;

  procedure TGIS_LayerDXF.doInsert(
    const _block : String
  ) ;
  var
    ipos          : {$IFDEF JAVA} nullable {$ENDIF} Int64      ;
    ioldpos       : Integer      ;
    rot           : Double       ;
    extr          : TGIS_Point3D ;
    old_transform : T_Matrix     ;
    tmp_pos       : Integer      ;
    block_mtrx    : T_Matrix     ;
    block_insert  : TGIS_Point3D ;
    gotBlockpt    : Boolean      ;
    sname         : String       ;
    icolor        : Integer      ;
    lstyle        : String       ;
  begin
    old_transform := arTransformInsert ;
    try
      extr         := GisPoint3D( 0, 0, 1 ) ;
      scale        := GisPoint3D( 1, 1, 1 ) ;
      rot          := 0 ;
      block_insert := GisPoint3D( 0, 0, 0 ) ;

      gotBlockpt   := False ;
      insertName   := '0' ;
      dxfFetchLine ;

      if dxfTestLine( GIS_DXF_CLAYER, '' ) and
        ( not isVisibleLayer( lineName ) )
      then exit ;

      tmp_pos := dxfFile.Position ;
      icolor  := 0 ;
      lstyle  := 'BYLAYER' ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CBLOCKNAME, '' ) then begin
                  sname := lineName ;
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  insertName := lineName ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  insert.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  insert.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  insert.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C41, '' ) then begin
                  scale.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C42, '' ) then begin
                  scale.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C43, '' ) then begin
                  scale.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C50, '' ) then begin
                  rot := DegToRad( DotStrToFloat( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  icolor := StrToInt( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                  lstyle := lineName ;
                end
        else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                  extr.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                  extr.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                  extr.Z := DotStrToFloat( lineName ) ;
                end ;
        tmp_pos := dxfFile.Position ;
        dxfFetchLine ;
      end ;

      prepareMatrixInsert( extr, scale, rot, insert ) ;
      arTransformInsert := mtrx_multiply( arTransformInsert, old_transform ) ;

      ioldpos := tmp_pos ;

      if ( sname <> _block ) then begin
        // non a recursive call
        if ( sname <> _block ) and lstBlock.TryGetValue( sname, ipos ) then begin
          dxfFile.Position := ipos ;
          dxfFetchLine ;

          inc( inInsert ) ;
          while ( not dxfEof ) and
                ( not dxfTestLine( GIS_DXF_CENDBLCK, GIS_DXF_NENDBLCK ) ) do
          begin
            // remember block insert point if any
            if not gotBlockpt then begin
              if      dxfTestLine( GIS_DXF_C10        , ''                ) then
                      block_insert.X := DotStrToFloat( lineName )
              else if dxfTestLine( GIS_DXF_C20        , ''                ) then
                      block_insert.Y := DotStrToFloat( lineName )
              else if dxfTestLine( GIS_DXF_C30        , ''                ) then
              begin
                block_insert.Z := DotStrToFloat( lineName )  ;
                gotBlockpt := True ;
              end
              else if dxfTestLine( GIS_DXF_C0        , ''                 ) then
              begin
                block_insert.Z := 0 ;
                gotBlockpt := True ;
              end ;

              if gotBlockpt then begin
                block_mtrx        := mtrx_fill ;
                block_mtrx        := mtrx_inv_translate( block_insert ) ;
                arTransformInsert := mtrx_multiply( block_mtrx, arTransformInsert ) ;
              end ;
            end ;
            currShape := nil ;
            lastColor := 0 ;
            if      dxfTestLine( GIS_DXF_CPOINT     , GIS_DXF_NPOINT      ) then
                    doPoint
            else if dxfTestLine( GIS_DXF_CINSERT    , GIS_DXF_NINSERT     ) then
                    doInsert( sname )
            else if dxfTestLine( GIS_DXF_CTEXT      , GIS_DXF_NTEXT       ) then
                    doText
            else if dxfTestLine( GIS_DXF_CTEXT      , GIS_DXF_NMTEXT      ) then
                    doText
            else if dxfTestLine( GIS_DXF_CSOLID     , GIS_DXF_NSOLID      ) then
                    doSolid
            else if dxfTestLine( GIS_DXF_CLINE      , GIS_DXF_NLINE       ) then
                    doLine
            else if dxfTestLine( GIS_DXF_CMLINE     , GIS_DXF_NMLINE      ) then
                    doMLine
            else if dxfTestLine( GIS_DXF_CARC       , GIS_DXF_NARC        ) then
                    doArc
            else if dxfTestLine( GIS_DXF_CCIRCLE    , GIS_DXF_NCIRCLE     ) then
                    doCircle
            else if dxfTestLine( GIS_DXF_CPOLYLINE  , GIS_DXF_NPOLYLINE   ) then
                    doPolyline
            else if dxfTestLine( GIS_DXF_CLWPOLYLINE, GIS_DXF_NLWPOLYLINE ) then
                    doLwPolyline
            else if dxfTestLine( GIS_DXF_CLWPOLYLINE, GIS_DXF_NHATCH      ) then
                    doHatch
            else if dxfTestLine( GIS_DXF_C3DFACE    , GIS_DXF_N3DFACE     ) then
                    do3DFace
            else if dxfTestLine( GIS_DXF_CSPLINE    , GIS_DXF_NSPLINE     ) then
                    doSpline
            else if dxfTestLine( GIS_DXF_CELLIPSE   , GIS_DXF_NELLIPSE    ) then
                    doEllipse
            else if dxfTestLine( GIS_DXF_CLWPOLYLINE, GIS_DXF_NMPOLYGON   ) then
                    doMPolygon
            else dxfFetchLine ;

            if assigned( currShape ) then begin
              if ( lastColor = 0 ) and (icolor > 0) and (icolor < 256) then
                setShapeColor( icolor ) ;
            end ;

          end ;
          dec( inInsert ) ;

          dxfFile.Position := ioldpos ;
          dxfFetchLine ;
        end ;
      end ;
    finally
      arTransformInsert := old_transform ;
      insertName   := '0' ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doAde ;
  var
    i   : Integer ;
    lv  : TGIS_LayerVector ;
    tkn : TArray<String> ;
  begin
    dxfFetchLine ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_CADE_MARKER, GIS_DXF_NEND ) and
            not dxfTestLine( GIS_DXF_CADE_DATA, GIS_DXF_NEND ) ) do
    begin
      if dxfTestLine( GIS_DXF_CADE_DATA, '' ) then begin
        {$IFDEF JAVA OR ISLAND}
        tkn := lineName.Split( '=' ).ToArray() ;
        {$ELSE}
        tkn := lineName.Split( ['='] ) ;
        {$ENDIF}
        if length( tkn ) > 0 then begin
          try
            if FindField( tkn[0] ) < 0 then
              AddFieldInternal( tkn[0], TGIS_FieldType.String, 1, 0 ) ;

            for i := 0 to SubLayers.Count - 1 do begin
              lv := TGIS_LayerVector( SubLayers.Items[ i ] );
              if lv.FindField( tkn[0] ) < 0 then
                lv.AddFieldInternal( tkn[0], TGIS_FieldType.String, 1, 0 ) ;
            end ;
            if length( tkn ) > 1 then
              currShape.SetField( tkn[0], ParamString( tkn[1], '' ) ) ;
          except
          end ;
        end ;
      end ;
      dxfFetchLine ;
    end ;

    if  dxfTestLine( GIS_DXF_CADE_MARKER, GIS_DXF_NEND ) or
        dxfTestLine( GIS_DXF_CADE_DATA, GIS_DXF_NEND )
    then
      dxfFetchLine ;
  end ;

  procedure TGIS_LayerDXF.doExtendedData ;
  var
    stmp     : String ;
    sappname : String           ;
    odata    : TStringList      ;
    {$IFDEF OXYGENE}
      e : TGIS_LayerDXFExtendedDataEventArgs ;
    {$ENDIF}
  begin
    if assigned( FExtendedDataEvent ) then begin
      sappname := lineName ;

      odata := TStringList.Create ;
      try
        dxfFetchLine ;

        while ( not dxfEof ) and
              ( lineCode >= GIS_DXF_CADE_DATA ) do
        begin
          stmp := '     ' + IntToStr( lineCode ) ;
          odata.Add( Copy( stmp, length( stmp ) - 4 + StringFirst, 4 ) ) ;
          odata.Add( lineName ) ;
          dxfFetchLine ;
        end ;

        {$IFDEF OXYGENE}
          e := TGIS_LayerDXFExtendedDataEventArgs.Create(
                 sappname,
                 currShape,
                 odata
               ) ;
          try
            FExtendedDataEvent( self, e ) ;
          finally
            FreeObject( e ) ;
          end ;
        {$ELSE}
          FExtendedDataEvent(
            self,
            sappname,
            currShape,
            odata
          ) ;
        {$ENDIF}
      finally
        FreeObject( odata ) ;
      end;
    end
    else begin
      if lineName = GIS_DXF_NADE then
        doAde
      else
        dxfFetchLine;
    end ;
  end ;

  procedure TGIS_LayerDXF.doEmbeddedData ;
  begin
    dxfFetchLine ;
    while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
    begin
      dxfFetchLine ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doSolid ;
  var
    cnt  : Integer      ;
    ptg1 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg2 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg3 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg4 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr : TGIS_Point3D ;
    layername : String ;
  begin
    extr := GisPoint3D( 0, 0, 1 ) ;
    cnt := 0 ;

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;

                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName( layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  ptg1.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  ptg1.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  ptg1.Z := DotStrToFloat( lineName ) ;
                  currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg1.Z ) ;
                end
        else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                  ptg2.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                  ptg2.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                  ptg2.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C12, '' ) then begin
                  ptg3.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C22, '' ) then begin
                  ptg3.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C32, '' ) then begin
                  ptg3.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C13, '' ) then begin
                  ptg4.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C23, '' ) then begin
                  ptg4.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C33, '' ) then begin
                  ptg4.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                  extr.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                  extr.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                  extr.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                  doExtendedData ;
                  continue ;
                end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
        dxfFetchLine ;
      end ;

    prepareMatrix( extr ) ;

    if cnt >= 1 then currShape.AddPoint3D( transformPtg( ptg1 ) ) ;
    if cnt >= 2 then currShape.AddPoint3D( transformPtg( ptg2 ) ) ;
    if cnt >= 4 then currShape.AddPoint3D( transformPtg( ptg4 ) ) ;
    if cnt >= 3 then currShape.AddPoint3D( transformPtg( ptg3 ) ) ;
    if cnt >= 1 then currShape.AddPoint3D( transformPtg( ptg1 ) ) ;
    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doSpline ;
  var
    stptg : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    etptg : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    cptg  : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    fptg  : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr  : TGIS_Point3D ;
    ptg   : TGIS_Point3D ;
    fpcnt : Integer      ;
    finp  : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF} ;
    fidx  : Integer      ;
    cpcnt : Integer      ;
    cidx  : Integer      ;
    cinp  : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF} ;
    kncnt : Integer      ;
    knidx : Integer      ;
    kninp : array of Double ;
    widx  : Integer      ;
    winp  : array of Double ;
    kw, t : Double ;
    layername : String ;
    ii, i   : Integer ;
    sflag   : Integer     ;
    scdeg   : Integer ;
    splint  : TDXF_BSplineInterpolator ;

    procedure drawSpline(
      const _cp       : array of TGIS_Point3D ;
      const _cpCount  : Integer ;
      const _knot     : array of Double ;
      const _knotCount: Integer
     ) ;
    var
      i    : Integer ;
      step : Double ;
      t    : Double ;

      function N( _n, _i : Integer ; _t : Double ) : Double ;
      var
        v1, d1, v2, d2 : Double ;
      begin
        v2 := 0 ;
        if _n = 0 then begin
          if ((_knot[_i] <= _t) and (_t < _knot[_i+1])) then
            Result := 1.0
          else
            Result := 0.0 ;
          exit ;
        end
        else begin
          if (_i+_n) < _knotCount then begin
            d1 := _knot[_i+_n] - _knot[_i] ;
            v1 := (_t - _knot[_i]) * N(_n - 1, _i, _t ) ;
          end
          else
            d1 := 0.0 ;

          if d1 = 0 then
            v1 := 0
          else
            v1 := (v1 / d1) ;

          if (_i+_n+1) < _knotCount then begin
            d2 := _knot[_i+_n+1] - _knot[_i+1] ;
            v2 := (_knot[_i + _n + 1] - _t) * N(_n - 1, _i + 1, _t ) ;
          end
          else
            d2 := 0 ;

          if d2 = 0 then
            v2 := 0
          else
            v2 := (v2 / d2) ;

          Result := v1 + v2 ;
        end ;
      end ;

      function NURBS_3( _j : Integer ; _t : Double ) : TGIS_Point3D ;
      var
        ni : Double ;
        k  : Integer ;
      begin
        {$IFDEF GIS_NORECORDS}
        Result := new TGIS_Point3D() ;
        {$ENDIF}
        Result.X := 0 ;
        Result.Y := 0 ;
        Result.Z := 0 ;

        for k := _j-3 to _j do begin
          ni := N(3, k, _t) ;
          Result.X := Result.X + _cp[k].X * ni ;
          Result.Y := Result.Y + _cp[k].Y * ni ;
          Result.Z := Result.Z + _cp[k].Z * ni ;
        end ;
      end ;

    begin
      // draw nurbs
      currShape.AddPoint3D( _cp[0] ) ;

      for i := 3 to _cpCount-1 do begin

        step := (_knot[i+1]-_knot[i]) / 25 ;
        t := _knot[i] ;
        while ( step > 0 ) and ( t < _knot[i+1] ) do begin
          currShape.AddPoint3D( NURBS_3( i, t ) ) ;
          t := t + step ;
        end ;
      end ;

      currShape.AddPoint3D( _cp[_cpCount-1] ) ;
    end ;

  begin
    cidx   := 0 ;
    fidx   := 0 ;
    knidx  := 0 ;
    widx   := 0 ;
    fpcnt  := 0 ;
    cpcnt  := 0 ;
    kncnt  := 0 ;
    scdeg  := -1 ;
    sflag  := 0 ;
    dxfFetchLine ;
    extr := GisPoint3D( 0, 0, 1 ) ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      prepareMatrix( extr ) ;
      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;

                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName(  layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  cptg.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  cptg.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  cptg.Z := DotStrToFloat( lineName ) ;
                  if cidx >= length( cinp ) then
                    SetLength( cinp, cidx + 1 ) ;
                  cinp[cidx] :=  transformPtg( cptg ) ;
                  inc( cidx ) ;
                end
        else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                  fptg.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                  fptg.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                  fptg.Z := DotStrToFloat( lineName ) ;
                  if fidx >= length( finp ) then
                    SetLength( finp, fidx + 1 ) ;
                  finp[fidx] :=  transformPtg( fptg ) ;
                  inc( fidx ) ;
                end
        else if dxfTestLine( GIS_DXF_C12, '' ) then begin
                  stptg.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C22, '' ) then begin
                  stptg.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C32, '' ) then begin
                  stptg.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C13, '' ) then begin
                  etptg.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C23, '' ) then begin
                  etptg.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C33, '' ) then begin
                  etptg.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C40, '' ) then begin
                   kw := DotStrToFloat( lineName ) ;
                  if knidx >= length( kninp ) then
                    SetLength( kninp, knidx + 1 ) ;
                  kninp[knidx] :=  kw ;
                  inc( knidx ) ;
                end
        else if dxfTestLine( GIS_DXF_C41, '' ) then begin
                   kw := DotStrToFloat( lineName ) ;
                  if widx >= length( winp ) then
                    SetLength( winp, widx + 1 ) ;
                  winp[widx] :=  kw ;
                  inc( widx ) ;
                end
        else if dxfTestLine( GIS_DXF_C70, '' ) then begin
                   sflag := StrToInt( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C71, '' ) then begin
                    // Degree
                   scdeg := StrToInt( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C72, '' ) then begin
                   kncnt := StrToInt( lineName ) ;
                   SetLength( kninp, kncnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C73, '' ) then begin
                   cpcnt := StrToInt( lineName ) ;
                   SetLength( cinp, cpcnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C74, '' ) then begin
                  fpcnt := StrToInt( lineName ) ;
                  SetLength( finp, fpcnt ) ;
                end
        else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                  doExtendedData ;
                  continue ;
                end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
          dxfFetchLine ;
      end ;

    if fpcnt > 0 then begin
      for ii := 0 to fpcnt-1 do
        currShape.AddPoint3D( finp[ii] ) ;

      currShape.Smooth( 25, False ) ;
    end
    else begin
      // drawSpline( cinp, cpcnt, kninp, kncnt ) ;
      // new interpolator
      splint := TDXF_BSplineInterpolator.Create ;
      try
        if splint.Prepare( scdeg, cinp, kninp, winp ) then
        begin
          currShape.AddPoint3D( cinp[0] ) ;
          t := 0 ;
          while t < 1 do begin
            ptg := splint.InterpolatePoint( t ) ;
            currShape.AddPoint3D( ptg ) ;
            t := t + 1/25 ;
          end ;
          currShape.AddPoint3D( cinp[cpcnt-1] ) ;
        end
        else begin
          // draw polyline
          for i := 0 to cpcnt-1 do
            currShape.AddPoint3D( cinp[i] ) ;
        end;
      finally
        FreeObject( splint ) ;
      end ;
    end ;

    currShape.Unlock ;

  end ;

  procedure TGIS_LayerDXF.do3DFace ;
  var
    cnt  : Integer      ;
    ptg1 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg2 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg3 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg4 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    vedge: Integer      ;
    extr : TGIS_Point3D ;
    layername : String ;
  begin
    cnt := 0 ;

    dxfFetchLine ;
    vedge := 0 ;
    extr := GisPoint3D( 0, 0, 1 ) ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    currShape := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;

                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName(  layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  ptg1.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  ptg1.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  ptg1.Z := DotStrToFloat( lineName ) ;
                  currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg1.Z ) ;
                end
        else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                  ptg2.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                  ptg2.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                  ptg2.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C12, '' ) then begin
                  ptg3.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C22, '' ) then begin
                  ptg3.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C32, '' ) then begin
                  ptg3.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C13, '' ) then begin
                  ptg4.X := DotStrToFloat( lineName ) ;
                  inc( cnt ) ;
                end
        else if dxfTestLine( GIS_DXF_C23, '' ) then begin
                  ptg4.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C33, '' ) then begin
                  ptg4.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C70, '' ) then begin
                  vedge := StrToInt ( lineName ) ;
                end ;
        dxfFetchLine ;
      end ;

    prepareMatrix( extr ) ;

    ptg1.M := 0 ;
    ptg2.M := 0 ;
    ptg3.M := 0 ;
    ptg4.M := 0 ;

    currShape.AddPoint3D( transformPtg( ptg1 ) ) ;
    currShape.AddPoint3D( transformPtg( ptg2 ) ) ;
    currShape.AddPoint3D( transformPtg( ptg3 ) ) ;
    if not GisIsSamePoint3D( ptg3, ptg4 ) then
      currShape.AddPoint3D( transformPtg( ptg4 ) ) ;

    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doMPolygon ;
  var
    hndl      : String       ;
    layername : String       ;
    lstyle    : String       ;
    lweight   : String       ;
    color     : Integer      ;
    elev      : Double       ;
    extr      : TGIS_Point3D ;
    cnt       : Integer      ;
    ar        : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF};
    i         : Integer      ;

    procedure create_shape ;
    begin
      currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ ) ;
      currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;
      currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
      currShape.Layer := getSubLayerByName(  layername ) ;

      currShape.SetField( GIS_DXF_FLD_ELEVATION , elev  ) ;
      if color <> 0 then
        setShapeColor( color ) ;

      if not IsStringEmpty( lstyle ) then
        setLineStyle( layername, lstyle ) ;

      if not IsStringEmpty( lweight ) then begin
        setLineWeight( lweight ) ;
      end ;
    end ;
  begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    currShape := nil ;
    hndl      := ''  ;
    layername := ''  ;
    color     := 0   ;
    elev      := 0   ;
    cnt       := 0 ;

    dxfFetchLine ;
    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) ) do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                hndl := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                color := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                lstyle := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                lweight := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR70, '' ) then begin
                // solid := StrToInt( lineName ) = 1 ;
              end
      else if dxfTestLine( GIS_DXF_C10, '' )  then begin
                inc( cnt ) ;
                SetLength( ar, cnt ) ;
                {$IFDEF GIS_NORECORDS}
                  ar[cnt-1] := new TGIS_Point3D ;
                {$ENDIF}
                ar[cnt-1].X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' )  then begin
                ar[cnt-1].Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                extr.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                extr.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                extr.Z := DotStrToFloat( lineName ) ;
              end ;

      dxfFetchLine ;
    end ;

    if length( ar ) > 0 then begin
      create_shape ;
      prepareMatrix( extr ) ;
      i := 1 ;
      while i < length( ar ) do begin
          currShape.AddPoint3D( transformPtg( ar[ i ] ) ) ;
          inc( i ) ;
      end ;

      currShape.Unlock ;
    end ;
    {$IFDEF OXYGENE}
      if assigned( ar ) then
        ar := nil ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerDXF.doHatch ;
  var
    hndl      : String       ;
    layername : String       ;
    lstyle    : String       ;
    lweight   : String       ;
    color     : Integer      ;
    elev      : Double       ;
    extr      : TGIS_Point3D ;
    ptg       : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptgCenter : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptgEnd    : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    cnt       : Integer      ;
    cntl      : Integer      ;
    cnt_cp    : Integer      ;
    ar        : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF};
    al        : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF};
    cp        : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF};
    hatchType : Integer      ;
    pathType  : Integer      ;
    isPolyline: Boolean      ;
    first     : Boolean      ;
    pathCnt   : Integer      ;
    bndCnt    : Integer      ;
    start     : Double       ;
    stop      : Double       ;
    radius    : Double       ;
    isPattern : Boolean      ;
    attr      : Integer      ;

    procedure create_shape( const _type : TGIS_ShapeType ) ;
    begin
      currShape := CreateShape( _type, TGIS_DimensionType.XYZ ) ;
      currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername      ) ;
      currShape.Layer := getSubLayerByName(  layername ) ;

      currShape.SetField( GIS_DXF_FLD_ELEVATION , elev  ) ;

      if color <> 0 then
        setShapeColor( color ) ;

      if not IsStringEmpty( lstyle ) then
        setLineStyle( layername, lstyle ) ;

      if not IsStringEmpty( lweight ) then begin
        setLineWeight( lweight ) ;
      end ;
    end ;

    procedure parsePath( const _addPart : Boolean ) ;
    var
      i,
      len     : Integer ;
      swgang  : Double  ;
      a, b    : Double  ;
    begin
      if _addPart then
        currShape.AddPart;

      if isPolyline then begin
        if length( ar ) > 0 then begin
          i   := 0;
          len := length( ar ) ;
          while i < len do begin
            if ( ar[ i ].M = 0 ) then
            begin
              currShape.AddPoint3D( transformPtg( ar[ i ] ) ) ;
              inc( i )
            end
            else begin
              if ( i >= len - 1 ) then
              begin
                currShape.AddPoint3D( transformPtg( ar[ i ] ) ) ;
                inc( i );
              end
              else begin
                calculateBulge( ar[ i ], ar[ i+1 ] );
                inc( i );
              end ;
            end ;
          end ;
        end
      end
      else begin
        if pathType = 1 then begin    // line
          if length( ar ) > 0 then begin
            i   := 0;
            len := length( ar ) ;
            while i < len do begin
              currShape.AddPoint3D( transformPtg( ar[ i ] ) ) ;
              currShape.AddPoint3D( transformPtg( al[ i ] ) ) ;
              inc( i );
            end ;
          end ;
        end
        else if pathType = 2 then begin   // arc
          if ( start > stop ) then
            swgang := ( stop - start + 360 )
          else
            swgang := ( stop - start );

          drawArc( ptg, radius, radius, DegToRad( start ), DegToRad( swgang ) ) ;
        end
        else if pathType = 3 then begin   // ellipse
          ptgEnd.X := ptgEnd.X + ptgCenter.X;
          ptgEnd.Y := ptgEnd.Y + ptgCenter.Y;
          a := GisPoint2Point( GisPoint( ptgCenter.X, ptgCenter.Y ),
                               GisPoint( ptgEnd.X, ptgEnd.Y )
                              ) ;
          b := a * radius ;

          if ( start > stop ) then
            swgang := ( stop - start + 2*Pi )
          else
            swgang := ( stop - start );

          drawArc( ptgCenter, a, b, DegToRad( start ), DegToRad( swgang ) ) ;
        end
        else if pathType = 4 then begin   // spline
          if length( cp ) > 0 then begin
            i   := 0;
            len := length( cp ) ;
            while i < len do begin
              currShape.AddPoint3D( transformPtg( cp[ i ] ) ) ;
              inc( i );
            end ;
          end ;
        end ;
      end ;
    end ;

  begin
    extr        := GisPoint3D( 0, 0, 1 ) ;

    currShape   := nil ;
    hndl        := ''  ;
    layername   := ''  ;
    color       := 0   ;
    elev        := 0   ;
    cnt         := 0 ;
    cntl        := 0 ;
    cnt_cp      := 0 ;
    attr        := 0 ;
    hatchType   := -1 ;
    isPolyline  := False ;
    pathCnt     := 0;
    pathType    := -1;
    first       := True ;
    bndCnt      := 0 ;
    isPattern   := False ;

    dxfFetchLine ;
    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) ) {and
          ( not cancel )}  do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                hndl := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
              end
      else if dxfTestLine( GIS_DXF_C70, '' ) then begin
                attr := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                color := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                lstyle := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                lweight := lineName ;
              end
      else if dxfTestLine( 92, '' ) then begin
                if first then begin
                  if (attr and 1) = 1 then
                    create_shape( TGIS_ShapeType.Polygon )
                  else
                    create_shape( TGIS_ShapeType.Arc ) ;
                  prepareMatrix( extr ) ;
                  first := False ;
                end ;
                inc( bndCnt );
                if hatchType > -1 then
                  parsePath( True ) ;

                hatchType   := StrToInt( lineName ) ;
                isPolyline  := ( hatchType and 2 ) = 2 ;
                cnt         := 0 ;
                cntl        := 0 ;
                cnt_cp      := 0 ;
              end
      else if dxfTestLine( 93, '' ) then begin
                pathCnt := StrToInt( lineName ) + 1 ;
              end
      else if dxfTestLine( 72, '' ) then begin
                if not isPolyline then begin
                  pathType := StrToInt( lineName );
                end ;
              end
      else if dxfTestLine( 75, '' ) then begin
                isPattern := True ;
              end
      else if dxfTestLine( 98, '' ) then begin
                isPattern := True ;
              end
      else if dxfTestLine( GIS_DXF_C10, '' )  then begin
                if not isPattern then begin
                  if ( hatchType > -1 ) and ( cnt <= pathCnt ) and ( pathType < 2 )
                  then begin
                    inc( cnt ) ;
                    SetLength( ar, cnt ) ;
                    {$IFDEF GIS_NORECORDS}
                      ar[cnt-1] := new TGIS_Point3D ;
                    {$ENDIF}
                    ar[cnt-1].X := DotStrToFloat( lineName ) ;
                  end ;
                  if pathType = 2 then // arc
                    ptg.X := DotStrToFloat( lineName ) ;
                  if pathType = 3 then // ellipse
                    ptgCenter.X := DotStrToFloat( lineName ) ;
                  if pathType = 4 then begin
                    inc( cnt_cp ) ;
                    SetLength( cp, cnt_cp ) ;
                    {$IFDEF GIS_NORECORDS}
                      cp[cnt_cp-1] := new TGIS_Point3D ;
                    {$ENDIF}
                    cp[cnt_cp-1].X := DotStrToFloat( lineName ) ;
                  end;
                end ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' )  then begin
                if not isPattern then begin
                  if ( hatchType > -1 ) and ( cnt <= pathCnt+1 ) and ( pathType < 2 )
                  then begin
                    ar[cnt-1].Y := DotStrToFloat( lineName ) ;
                  end ;
                  if pathType = 2 then // arc
                    ptg.Y := DotStrToFloat( lineName ) ;
                  if pathType = 3 then // ellipse
                    ptgCenter.Y := DotStrToFloat( lineName ) ;
                  if pathType = 4 then begin
                    cp[cnt_cp-1].Y := DotStrToFloat( lineName ) ;
                  end;
                end ;
              end
      else if dxfTestLine( GIS_DXF_C11, '' )  then begin
                if ( hatchType > -1 ) and ( cntl <= pathCnt ) and ( pathType < 2 )
                then begin
                  inc( cntl ) ;
                  SetLength( al, cntl ) ;
                  {$IFDEF GIS_NORECORDS}
                    al[cntl-1] := new TGIS_Point3D ;
                  {$ENDIF}
                  al[cntl-1].X := DotStrToFloat( lineName ) ;
                end ;
                if pathType = 3 then // ellipse
                  ptgEnd.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C21, '' )  then begin
                if ( hatchType > -1 ) and ( cntl <= pathCnt+1 ) and ( pathType < 2 )
                then begin
                  al[cntl-1].Y := DotStrToFloat( lineName ) ;
                end ;
                if pathType = 3 then // ellipse
                  ptgEnd.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C40, '' ) then begin
                radius := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C50, '' ) then begin
                start := DotStrToFloat( lineName )  ;
              end
      else if dxfTestLine( GIS_DXF_C51, '' ) then begin
                stop := DotStrToFloat( lineName )  ;
              end
      else if dxfTestLine( GIS_DXF_C42, '' ) then begin
                if ( hatchType > -1 ) and ( cnt <= pathCnt+1 ) and (cnt>0) then begin
                  ar[cnt-1].M := DotStrToFloat( lineName ) ;
                end ;
              end
      else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                extr.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                extr.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                extr.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                doExtendedData ;
                continue ;
              end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;

      dxfFetchLine ;
    end ;

    if ( bndCnt > 0 ) and isPattern then
      parsePath( True ) ;

    if assigned( currShape ) then begin
      currShape.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doLine ;
  var
    ptg1 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptg2 : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr : TGIS_Point3D ;
    layername : String ;
  begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and not ( isVisibleLayer( lineName ) )
    then exit ;

    currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName(  layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                  setLineStyle( layername, lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                  setLineWeight( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  ptg1.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  ptg1.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  ptg1.Z := DotStrToFloat( lineName ) ;
                  currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg1.Z ) ;
                end
        else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                  ptg2.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                  ptg2.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                  ptg2.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                  doExtendedData ;
                  continue ;
                end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
        dxfFetchLine ;
      end ;

      prepareMatrix( extr ) ;
      currShape.AddPoint3D( transformPtg( ptg1 ) ) ;
      currShape.AddPoint3D( transformPtg( ptg2 ) ) ;
    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doMLine ;
  var
    ptg1  : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr  : TGIS_Point3D ;
    attrn : Integer ;
    idx   : Integer ;
    layername : String ;
  begin
    extr  := GisPoint3D( 0, 0, 1 ) ;
    idx   := -1 ;
    attrn := 0 ;

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and not ( isVisibleLayer( lineName ) )
    then exit ;

    currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName(  layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                  setLineStyle( layername, lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                  setLineWeight( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  ptg1.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  ptg1.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  ptg1.Z := DotStrToFloat( lineName ) ;
                  currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg1.Z ) ;
                end
        else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                  inc( idx ) ;
                  if idx < attrn then
                    arPoint[ idx ].X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                  if idx < attrn then
                    arPoint[ idx ].Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                  if idx < attrn then
                    arPoint[ idx ].Z := DotStrToFloat( lineName ) ;
                end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR72, '' ) then begin
                attrn := StrToInt( lineName ) ;
                SetLength( arPoint, attrn ) ;
                {$IFDEF GIS_NORECORDS}
                  for idx := 0 to attrn - 1 do
                    arPoint[ idx ] := new TGIS_Point3D ;
                  idx   := -1 ;
                {$ENDIF}
              end ;
        dxfFetchLine ;
      end ;

      prepareMatrix( extr ) ;
      currShape.AddPoint3D( transformPtg( ptg1 ) ) ;
      for idx := 0 to attrn - 1 do
        currShape.AddPoint3D( transformPtg( arPoint[ idx ] ) ) ;
    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doEllipse;
  var
    ptgCenter : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptgEnd    : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr      : TGIS_Point3D ;
    ratio     : Double       ;
    start     : Double       ;
    stop      : Double       ;
    layername : String       ;
    sa,ea,a   : Double ;
    rx,ry,ra  : Double ;
    i         : Integer ;
    p         : TGIS_Point3D ;
    isa, iea  : Integer ;

    procedure calculatePtg( var _p : TGIS_Point3D ; const _c : TGIS_Point3D ; const _a : Double ) ;
    var
      dx, dy : Double ;
      rsin, rcos : Double ;
    begin
      SinCos( _a, rsin, rcos ) ;
      dx := _p.X*rcos - _p.Y*rsin ;
      dy := _p.X*rsin + _p.Y*rcos ;
      _p.X := _c.X + dx ;
      _p.Y := _c.Y + dy ;
      _p.Z := _c.Z ;
    end ;

  begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
      ( not isVisibleLayer( lineName ) )
    then exit ;

    start  := 0 ;
    stop   := 0 ;
    ratio  := 0 ;

    ptgCenter := GisPoint3D( 0, 0, 0, 0 ) ;

    currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName(  layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                  setLineStyle( layername, lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                  setLineWeight( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  ptgCenter.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                  ptgEnd.X := DotStrToFloat( lineName ) + ptgCenter.X;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  ptgCenter.Y := DotStrToFloat( lineName ) + ptgCenter.Y;
                end
        else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                  ptgEnd.Y := DotStrToFloat( lineName ) + ptgCenter.Y;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  ptgCenter.Z := DotStrToFloat( lineName ) ;
                  currShape.SetField( GIS_DXF_FLD_ELEVATION, ptgCenter.Z ) ;
                end
        else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                  ptgEnd.Z := DotStrToFloat( lineName ) + ptgCenter.Z;
                end
        else if dxfTestLine( GIS_DXF_C40, '' ) then begin
                  ratio := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C41, '' ) then begin
                  start := DotStrToFloat( lineName )  ;
                end
        else if dxfTestLine( GIS_DXF_C42, '' ) then begin
                  stop := DotStrToFloat( lineName )  ;
                end
        else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                  extr.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                  extr.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                  extr.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                  doExtendedData ;
                  continue ;
                end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
        dxfFetchLine ;
      end ;

      prepareMatrix( extr ) ;

      if (start > stop) then
        stop := stop + 2*Pi ;

      sa := start ;
      ea := stop ;

      isa := RoundS( sa*180/Pi ) ;
      iea := RoundS( ea*180/Pi ) ;

      if not GisIsSamePoint3D( extr, GisPoint3D( 0, 0, 1 ) ) then begin
        ptgCenter:= transformPtg( ptgCenter ) ;
        ptgEnd:= transformPtg( ptgEnd ) ;
      end ;

      rx := GisPoint2Point3D( ptgCenter, ptgEnd ) ;
      ry := rx * ratio ;
      ra := ArcTan2( ptgEnd.Y - ptgCenter.Y, ptgEnd.X - ptgCenter.X ) ;

      if ( sa <= ea ) then begin
        a := sa;
        p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
        calculatePtg( p, ptgCenter, ra ) ;
        currShape.AddPoint3D( transformPtg(p) ) ;

        for i := 1 to (iea-isa) do begin
          a := (isa + i)*Pi/180.0 ;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, ptgCenter, ra ) ;
          currShape.AddPoint3D( transformPtg(p) ) ;
        end ;
        a := ea ;
        p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
        calculatePtg( p, ptgCenter, ra ) ;
        currShape.AddPoint3D( transformPtg(p) ) ;
      end
      else begin
        a := sa ;
        p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
        calculatePtg( p, ptgCenter, ra ) ;
        currShape.AddPoint3D( transformPtg(p) ) ;
        for i := 1 to (360-isa)-1 do begin
          a := (isa+i)*Pi/180.0 ;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, ptgCenter, ra ) ;
          currShape.AddPoint3D( transformPtg(p) ) ;
        end ;

        for i := (360-isa)+1 to (360-isa)+iea do begin
          a := (i-(360-isa))*Pi/180.0 ;
          p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
          calculatePtg( p, ptgCenter, ra ) ;
          currShape.AddPoint3D( transformPtg(p) ) ;
        end ;
        a := ea;
        p := GisPoint3D( rx * Cos(a),ry * Sin(a), 0 ) ;
        calculatePtg( p, ptgCenter, ra ) ;
        currShape.AddPoint3D( transformPtg(p) ) ;
      end ;

    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doArc ;
  var
    ptg      : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    start    : Double       ;
    stop     : Double       ;
    swgang   : Double       ;
    radius   : Double       ;
    extr     : TGIS_Point3D ;
    layername: String       ;
  begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
      ( not isVisibleLayer( lineName ) )
    then exit ;

    start  := 0 ;
    stop   := 0 ;
    radius := 0 ;

    ptg := GisPoint3D( 0, 0, 0, 0 ) ;

    currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
      currShape.AddPart ;

      while ( not dxfEof ) and
            ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
      begin
        if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                end
        else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                  layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
                  currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                  currShape.Layer := getSubLayerByName(  layername ) ;
                end
        else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                  setShapeColor( StrToInt( lineName ) ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                  setLineStyle( layername, lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                  setLineWeight( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                  ptg.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                  ptg.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                  ptg.Z := DotStrToFloat( lineName ) ;
                  currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg.Z ) ;
                end
        else if dxfTestLine( GIS_DXF_C40, '' ) then begin
                  radius := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C50, '' ) then begin
                  start := DotStrToFloat( lineName )  ;
                end
        else if dxfTestLine( GIS_DXF_C51, '' ) then begin
                  stop := DotStrToFloat( lineName )  ;
                end
        else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                  extr.X := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                  extr.Y := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                  extr.Z := DotStrToFloat( lineName ) ;
                end
        else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                  doExtendedData ;
                  continue ;
                end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
        dxfFetchLine ;
      end ;

      prepareMatrix( extr ) ;

      if ( start > stop ) then
        swgang := ( stop - start + 360 )
      else
        swgang := ( stop - start );

      drawArc( ptg, radius, radius, DegToRad( start ), DegToRad( swgang ) ) ;

    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doCircle ;
  var
    cnt      : Integer      ;
    ptg      : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptga     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptgb     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    start    : Double       ;
    step     : Double       ;
    steps    : Integer      ;
    radius   : Double       ;
    arcangle : Double       ;
    nx,ny    : Double       ;
    extr     : TGIS_Point3D ;
    layername: String       ;
    rsin,rcos: Double       ;
  const
    MAX_STEPS = 90 ;
  begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    start  := 0 ;
    radius := 0 ;

    ptg := GisPoint3D( 0, 0, 0, 0 ) ;

    currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
    currShape.AddPart ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' )  ) do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName ;
                if layername = '0' then
                  layername := insertName ;
                currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                currShape.Layer := getSubLayerByName(  layername ) ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                setShapeColor(  StrToInt( lineName ) ) ;
              end
      else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                setLineStyle( layername, lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                ptg.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                ptg.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                ptg.Z := DotStrToFloat( lineName ) ;
                currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg.Z ) ;
              end
      else if dxfTestLine( GIS_DXF_C40, '' ) then begin
                radius := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                extr.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                extr.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                extr.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                doExtendedData ;
                continue ;
              end
      else if dxfTestLine( 101, '' ) then begin
                doEmbeddedData ;
                continue ;
            end ;
      dxfFetchLine ;
    end ;

    prepareMatrix( extr ) ;

    ptga := transformPtg(
              GisPoint3D( ptg.X - radius, ptg.Y - radius, ptg.Z - radius )
            ) ;
    ptgb := transformPtg(
              GisPoint3D( ptg.X + radius, ptg.Y + radius, ptg.Z + radius )
            ) ;

    ptg := transformPtg( ptg ) ;

    ptg := GisPoint3D( ptga.X + (ptgb.X - ptga.X) / 2,
                       ptga.Y + (ptgb.Y - ptga.Y) / 2,
                       ptga.Z + (ptgb.Z - ptga.Z) / 2
                     ) ;

    radius := Abs( ptgb.X - ptga.X ) / 2  ;

    arcangle := 2 * Pi ;

    steps := MAX_STEPS - 1 ;

    step := Abs( arcangle ) / steps ;

    start := start + Pi/2 ;

    // calculate elliptical arc
    for cnt := 0 to steps do begin
      SinCos( start, rsin, rcos ) ;
      ny := radius * rcos ;
      nx := radius * rsin ;
      currShape.AddPoint3D(  GisPoint3D( ptg.X+nx, ptg.Y-ny, ptg.Z ) ) ;
      start := start + step ;
    end ;
    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doPoint ;
  var
    ptg  : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr : TGIS_Point3D ;
    layername : String ;
  begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    dxfFetchLine ;
    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    ptg := GisPoint3D( 0, 0, 0, 0 ) ;

    currShape := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
    currShape.AddPart ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) ) do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName ;
                if layername = '0' then
                  layername := insertName ;
                currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                currShape.Layer := getSubLayerByName(  layername ) ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                setShapeColor(  StrToInt( lineName ) ) ;
              end
      else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                ptg.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                ptg.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                ptg.Z := DotStrToFloat( lineName ) ;
                currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg.Z ) ;
              end
      else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                extr.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                extr.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                extr.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                doExtendedData ;
                continue ;
              end
      else if dxfTestLine( 101, '' ) then begin
                doEmbeddedData ;
                continue ;
            end ;
      dxfFetchLine ;
    end ;

    prepareMatrix( extr ) ;
    currShape.AddPoint3D( transformPtg( ptg ) ) ;
    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doText ;
  var
    ptg       : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    ptgEx     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    extr      : TGIS_Point3D ;
    hpos      : Integer ;
    vpos      : Integer ;
    name      : String  ;
    layername : String ;
    angle     : Double ;
    state     : Integer ;
    iPos      : Integer ;
    len       : Integer ;
    c         : Char    ;
    code      : Char ;
    text      : TStringBuilder ;
    codevalue : TStringBuilder ;
    tkn       : TGIS_Tokenizer ;
    revert    : TStringList ;
    cval      : Integer ;
    color     : TGIS_Color ;
    special   : String ;
    hex       : Integer ;
    hdl       : String ;

    procedure next_char ;
    begin
      inc( iPos ) ;
      if iPos < StringFirst + len then
        c := lineName[ iPos ]
      else
        c := #0 ;
    end ;

    procedure collect_code ;
    begin
      codevalue.Clear ;
      while (c <> ';') and (c <> #0) do begin
        next_char ;
        if c <> ';' then
          codevalue.Append( c ) ;
      end ;
    end ;

    function parse_format : String ;
    var
      ii : Integer ;
    begin
      Result := '' ;
      iPos  := StringFirst ;
      len   := length( lineName ) ;
      if len = 0 then exit ;

      c     := lineName[ iPos ] ;
      state := 0 ;

      text      := TStringBuilder.Create ;
      codevalue := TStringBuilder.Create ;
      tkn       := TGIS_Tokenizer.Create ;
      revert    := TStringList.Create ;
      try
        while iPos < StringFirst + len do begin
          case state of
            // start
            0 : state := 1 ;
            // start format
            1 : if c = '{' then begin
                  next_char ;
                end
                else if c = '}' then begin
                  for ii := 0 to revert.Count-1 do
                    text.Append( revert[ii] ) ;
                  revert.Clear ;
                  next_char
                end
                else if c = '\' then begin
                  state := 3 ;
                  next_char ;
                end
                else if c = '%' then begin
                  special := UpperCase( &Copy( lineName, iPos, 3) ) ;
                  if      special = '%%C' then begin
                    text.Append( #$2300 ) ;
                    next_char ;
                    next_char ;
                    next_char ;
                  end
                  else if special = '%%D' then begin
                    text.Append( #$B0 ) ;
                    next_char ;
                    next_char ;
                    next_char ;
                  end
                  else if special = '%%P' then begin
                    text.Append( #$B1 ) ;
                    next_char ;
                    next_char ;
                    next_char ;
                  end
                  else if special = '%%U' then begin
                    text.Append( '</U>' ) ;
                    next_char ;
                    next_char ;
                    next_char ;
                  end
                  else if special = '%%O' then begin
                    next_char ;
                    next_char ;
                    next_char ;
                  end
                  else
                    next_char ;
                  state := 1 ;
                end
                else
                  state := 4 ;
            // format code
            3 : begin
                  code := c ;

                  if (code = 'f') or (code = 'F') then begin
                    // read font
                    collect_code ;

                    for ii := 0 to revert.Count-1 do
                      text.Append( revert[ii] ) ;

                    tkn.Execute( codevalue.ToString, ['|'] ) ;
                    for ii := 0 to tkn.Result.Count-1 do begin
                      if ii = 0 then begin
                        text.Append( Format( '<FNT NAME="%s">', [tkn.Result[0]] ) ) ;
                        revert.Add( '</FNT>' ) ;
                      end
                      else if tkn.Result[ii] = 'b1' then begin
                        text.Append( '<B>' ) ;
                      end
                      else if tkn.Result[ii] = 'b0' then begin
                        text.Append( '</B>' ) ;
                      end
                      else if tkn.Result[ii] = 'i1' then begin
                        text.Append( '<I>' ) ;
                      end
                      else if tkn.Result[ii] = 'i0' then begin
                        text.Append( '</I>' ) ;
                      end ;
                    end ;
                  end
                  else if (code = 's') or (code = 'S') then begin
                    // read stacking
                    collect_code ;
                    text.Append( codevalue.ToString ) ;
                  end
                  else if (code = 'a') or (code = 'A') then begin
                    // read alignment
                    collect_code ;
                  end
                  else if (code = 'c') or (code = 'C') then begin
                    // read color
                    collect_code ;
                    if TryStrToInt( codevalue.ToString, cval ) then begin
                      if (cval >= 0) and (cval < 256) then begin
                        color := getPaletteColor( cval ) ;
                        text.Append( Format( '<CLR red="%d" green="%d" blue="%d">',
                                             [ color.R, color.G, color.B ]
                                           )
                                    ) ;
                        revert.Add( '</CLR>' ) ;
                      end ;
                    end
                  end
                  else if (code = 'L') then begin
                    // start underline
                    text.Append( '<U>' ) ;
                  end
                  else if (code = 'l') then begin
                    // stop underline
                    text.Append( '</U>' ) ;
                  end
                  else if (code = 'O') then begin
                    // start overstrike
                  end
                  else if (code = 'o') then begin
                    // stop overstrike
                  end
                  else if (code = 'K') then begin
                    // start strike-through
                  end
                  else if (code = 'k') then begin
                    // stop strike-through
                  end
                  else if (code = 'P') then begin
                    // numbered paragraph
                    text.Append( '<BR>' ) ;
                  end
                  else if (code = 'p') then begin
                    // control codes
                    collect_code ;
                  end
                  else if (code = 'X') then begin
                    // paragraph wrap
                  end
                  else if (code = 'Q') then begin
                    // slanting text
                    collect_code ;
                  end
                  else if (code = 'H') then begin
                    // text height
                    collect_code ;
                  end
                  else if (code = 'W') then begin
                    // text width
                    collect_code ;
                  end
                  else if (code = 'T') then begin
                    // tracking spacing
                    collect_code ;
                  end
                  else if (code = '~') then begin
                    // hard space
                    text.Append( ' ') ;
                  end
                  else if (code = '\') then begin
                    // escape char
                    text.Append( '\') ;
                  end
                  else if (code = 'U') then begin
                    // unicode
                    next_char ;
                    if c = '+' then begin
                      next_char ;
                      hex := StrToIntDef( '$'+&Copy( lineName, iPos, 4 ), 0 ) ;
                      text.Append( Char( hex ) ) ;
                      next_char ;
                      next_char ;
                      next_char ;
                    end
                    else begin
                      text.Append( '\') ;
                      text.Append( 'U') ;
                      text.Append( c ) ;
                    end;
                  end
                  else begin
                    text.Append( '\') ;
                    text.Append( c ) ;
                  end;
                  next_char ;
                  state := 1 ;
                end ;
            // collect text
            4 : begin
                  text.Append( c ) ;
                  next_char ;
                  state := 1 ;
                end ;
          end ;
        end ;
      finally
        Result := text.ToString ;

        FreeObject( codevalue ) ;
        FreeObject( text      ) ;
        FreeObject( tkn       ) ;
        FreeObject( revert    ) ;
      end ;
    end ;

    function get_positions : TGIS_LabelPositions ;
    begin
      // any value to initialize
      case hpos of
        0,3,5 :
          case vpos of
            0,2 : Result := GisGetLabelPosition( TGIS_LabelPosition.MiddleRight  ) ;
            1   : Result := GisGetLabelPosition( TGIS_LabelPosition.UpRight      ) ;
            3   : Result := GisGetLabelPosition( TGIS_LabelPosition.DownRight    ) ;
          end ;
        4,1   :
          case vpos of
            0,2 : Result := GisGetLabelPosition( TGIS_LabelPosition.MiddleCenter ) ;
            1   : Result := GisGetLabelPosition( TGIS_LabelPosition.UpCenter     ) ;
            3   : Result := GisGetLabelPosition( TGIS_LabelPosition.DownCenter   ) ;
          end ;
        2     :
          case vpos of
            0,2 : Result := GisGetLabelPosition( TGIS_LabelPosition.MiddleLeft   ) ;
            1   : Result := GisGetLabelPosition( TGIS_LabelPosition.UpLeft       ) ;
            3   : Result := GisGetLabelPosition( TGIS_LabelPosition.DownLeft     ) ;
          end ;
      end ;
    end ;

  begin
    extr  := GisPoint3D( 0, 0, 1 ) ;
    ptgEx := GisPoint3D( 0, 0, 1 ) ;
    vpos  := 0;
    hpos  := 0;
    name  := lineName ;

    dxfFetchLine ;
    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    currShape := CreateShape( TGIS_ShapeType.Point, TGIS_DimensionType.XYZ ) ;
    currShape.Lock( TGIS_Lock.Projection ) ;
    currShape.AddPart ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) ) do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                hdl := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName ;
                if layername = '0' then
                  layername := insertName ;
                currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername ) ;
                currShape.Layer := getSubLayerByName(  layername ) ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                setShapeColor(  StrToInt( lineName ) ) ;
              end
      else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                ptg.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                ptg.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                ptg.Z := DotStrToFloat( lineName ) ;
                currShape.SetField( GIS_DXF_FLD_ELEVATION, ptg.Z ) ;
              end
      else if dxfTestLine( GIS_DXF_C11, '' ) then begin
                ptgEx.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C21, '' ) then begin
                ptgEx.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C31, '' ) then begin
                ptgEx.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C1, '' ) then begin
                currShape.SetField( GIS_DXF_FLD_LABEL,
                                    parse_format
                                  ) ;
                currShape.Params.Labels.Field := GIS_DXF_FLD_LABEL ;
              end
      else if dxfTestLine( GIS_DXF_C50, '' ) then begin
                currShape.Params.Labels.Rotate :=
                  -DegToRad( DotStrToFloat( lineName ) ) ;
                currShape.SetField( GIS_DXF_FLD_LABEL_ANGLE,
                                    DotStrToFloat( lineName )
                                   ) ;
              end
      else if dxfTestLine( GIS_DXF_C40, '' ) then begin
                currShape.SetField( GIS_DXF_FLD_LABEL_HEIGHT,
                                    DotStrToFloat( lineName )
                                   ) ;
                currShape.Params.Labels.FontSizeAsText := 'SIZE:' +
                            DotFloatToStr( DotStrToFloat( lineName ) *7/10 ) +
                            'mu' ;
                currShape.Params.Labels.Width := -999999;
              end
      else if dxfTestLine( GIS_DXF_C72, '' ) then begin
               hpos := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C73, '' ) then begin
               vpos := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C74, '' ) then begin
               vpos := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                doExtendedData ;
                continue ;
            end
      else if dxfTestLine( 101, '' ) then begin
                doEmbeddedData ;
                continue ;
            end ;
      dxfFetchLine ;
    end ;
    currShape.Params.Labels.Position := get_positions ;

    prepareMatrix( extr ) ;

    if name = 'MTEXT' then begin
      // rotation for mtext
      if ( currShape.Params.Labels.Rotate = 0 ) and
         ( ( ptgEx.X <> 0 ) or ( ptgEx.Y <> 0 ) ) then begin
        if Abs( ptgEx.X ) < 1.0e-6 then begin
          if ( ptgEx.Y > 0.0 ) then
            angle := Pi/2.0
           else
            angle := Pi/2.0*3.0
        end
        else
          angle := ArcTan( ptgEx.Y/ptgEx.X ) ;

        currShape.Params.Labels.Rotate := - angle ;
        currShape.SetField( GIS_DXF_FLD_LABEL_ANGLE, RadToDeg( angle ) ) ;
      end ;
      currShape.AddPoint3D( transformPtg( ptg ) ) ;
    end
    else begin
      // Second alignment point
      if ( ( hpos <> 0 ) or ( vpos <> 0 ) ) and
         ( ( ptgEx.X <> 0 ) and ( ptgEx.Y <> 0 ) ) then
        currShape.AddPoint3D( transformPtg( ptgEx ) )
      else
        currShape.AddPoint3D( transformPtg( ptg ) ) ;
    end ;
    currShape.Unlock ;
  end ;

  procedure TGIS_LayerDXF.doPolyline ;
  var
    i         : Integer ;
    m, n      : Integer ;
    attr      : Integer ;
    hndl      : String  ;
    layername : String  ;
    color     : Integer ;
    elev      : Double  ;
    lstyle    : String  ;
    lweight   : String  ;
    attrm     : Integer ;
    attrn     : Integer ;
    sm        : Integer ;
    sn        : Integer ;
    len       : Integer ;
    extr      : TGIS_Point3D  ;

    procedure create_shape ;
    begin
      if not assigned( currShape ) then begin
        prepareMatrix( extr ) ;
        if ( attr and 1 ) = 1 then
          currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ )
        else if ( attr and 64 ) = 64 then
          currShape := CreateShape( TGIS_ShapeType.MultiPatch, TGIS_DimensionType.XYZ )
        else
          currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;

        currShape.Lock( TGIS_Lock.Projection ) ;  // unlocked in doPolylineVertex ;
        currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername         ) ;
        currShape.Layer := getSubLayerByName(  layername ) ;
        if ( attr and 1 ) = 1 then
          currShape.Params.Area.Pattern := TGIS_BrushStyle.Clear ;

        currShape.SetField( GIS_DXF_FLD_ELEVATION , elev  ) ;

        if color <> 0 then
          setShapeColor(  color ) ;

        if not IsStringEmpty( lstyle ) then
          setLineStyle( layername, lstyle ) ;

        if not IsStringEmpty( lweight ) then begin
          setLineWeight( lweight ) ;
        end ;

      end ;
    end ;

 begin
    extr := GisPoint3D( 0, 0, 1 ) ;

    currShape := nil ;
    attr      := 0   ;
    hndl      := ''  ;
    layername := ''  ;
    color     := 0   ;
    elev      := 0   ;
    attrm     := 0   ;
    attrn     := 0   ;
    sm        := 0   ;
    sn        := 0   ;

    SetLength( arPoint, 0 ) ;
    SetLength( arFaces, 0 ) ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    dxfFetchLine ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) ) do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                hndl := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName  ;
                  if layername = '0' then
                    layername := insertName ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                color := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
               lstyle := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
               lweight := lineName ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                elev := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                extr.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                extr.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                extr.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR70, '' ) then begin
                attr := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR71, '' ) then begin
                attrm := StrToInt( lineName )
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR72, '' ) then begin
                attrn := StrToInt( lineName )
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR73, '' ) then begin
                sm := StrToInt( lineName )
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR74, '' ) then begin
                sn := StrToInt( lineName )
              end
      else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                create_shape ;
                doExtendedData ;
                continue ;
              end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
      dxfFetchLine ;
    end ;

    prepareMatrix( extr ) ;
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_CSEQEND, GIS_DXF_NSEQEND  ) {and
          ( not cancel )} ) do
      if dxfTestLine( GIS_DXF_CVERTEX, GIS_DXF_NVERTEX ) then
        doPolylineVertex( elev ) ;

    dxfFetchLine ;
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) {and
          ( not cancel )} ) do
      dxfFetchLine ;

    if length( arPoint ) > 0 then begin
      if (( attr and 16) <> 0) then begin
        // 3D polygon mesh - cross lines
        create_shape ;
        if sm <> 0 then
          attrm := sm ;
        if sn <> 0 then
          attrn := sn ;

        currShape.AddPart ;

        for m := 0 to attrm-1 do begin
          for n := 0 to attrn-1 do begin

            if (m < (attrm - 1)) then begin
              currShape.AddPoint3D(
                transformPtg( arPoint[ m*attrn + n ] )
              ) ;
              currShape.AddPoint3D(
                transformPtg( arPoint[ (m+1)*attrn + n ] )
              ) ;
            end
            else begin
              if ((attr and 1) = 1) then begin
                currShape.AddPoint3D(
                  transformPtg( arPoint[ m*attrn + n ] )
                ) ;
                currShape.AddPoint3D(
                  transformPtg( arPoint[ n ] )
                ) ;
              end
            end ;

            if (n < (attrn - 1)) then begin
              currShape.AddPoint3D(
                transformPtg( arPoint[ m*attrn + n ] )
              ) ;
              currShape.AddPoint3D(
                transformPtg( arPoint[ m*attrn + n+1 ] )
              ) ;
            end
            else begin
              if ((attr and 32) = 32) then begin
                currShape.AddPoint3D(
                   transformPtg( arPoint[ m*attrn + n ] )
                ) ;
                currShape.AddPoint3D(
                  transformPtg( arPoint[ m      * attrn] )
                ) ;
              end
            end
          end
        end
      end
      else if ( attr and 64 ) = 64 then begin
        // 3D polyface mesh
        create_shape ;
        for i := 0 to length( arFaces ) - 1 do begin
          if (arFaces[i][0] = 0) or (arFaces[i][1] = 0) then continue ;

          currShape.AddPart ;
          if (arFaces[i][0]<>0) then
            currShape.AddPoint3D( transformPtg(arPoint[Abs(arFaces[i][0])-1])) ;
          if (arFaces[i][1]<>0) then
            currShape.AddPoint3D( transformPtg(arPoint[Abs(arFaces[i][1])-1])) ;
          if (arFaces[i][2]<>0) then
            currShape.AddPoint3D( transformPtg(arPoint[Abs(arFaces[i][2])-1])) ;
          if (arFaces[i][3]<>0) then
            currShape.AddPoint3D( transformPtg(arPoint[Abs(arFaces[i][3])-1])) ;
        end
      end
      else begin
        // standard polyline
        create_shape ;
        currShape.AddPart ;
        i := 0;
        len := length( arPoint ) ;
        while i < len do begin
          if ( arPoint[ i ].M = 0 ) then
          begin
            currShape.AddPoint3D( transformPtg( arPoint[ i ] ) ) ;
            inc( i )
          end
          else begin
            if ( i >= len - 1 ) then
            begin
              currShape.AddPoint3D( transformPtg( arPoint[ i ] ) ) ;
              inc( i );
            end
            else begin
              calculateBulge( arPoint[ i ], arPoint[ i+1 ] );
              inc( i );
            end ;
          end ;
        end ;
      end
    end ;

    if assigned( currShape ) then begin
      currShape.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doPolylineVertex(
    const _elev : Double
  ) ;
  var
    ptg   : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    attr  : Integer      ;
  begin
    dxfFetchLine ;

    ptg := GisPoint3D( 0, 0, 0, 0 ) ;
    attr := 0 ;
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) ) do
    begin
      if      dxfTestLine( GIS_DXF_C10, '' ) then begin
                ptg.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                ptg.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C30, '' ) then begin
                ptg.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C42, '' ) then begin
                try
                  ptg.M := DotStrToFloat( lineName ) ;
                except
                  // ignore improper ptg.M values
                end ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR70, '' ) then begin
                attr := StrToInt( lineName ) ;
                if ( attr and 64 ) = 0 then
                  {$IFDEF OXYGENE}
                    SetLength( arFaces, length( arFaces ) + 1, 4 ) ;
                  {$ELSE}
                    SetLength( arFaces, Length( arFaces ) + 1 ) ;
                  {$ENDIF}
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR71, '' ) then begin
                arFaces[ length( arFaces )-1][0] := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR72, '' ) then begin
                arFaces[ length( arFaces )-1][1] := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR73, '' ) then begin
                arFaces[ length( arFaces )-1][2] := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR74, '' ) then begin
                arFaces[ length( arFaces )-1][3] := StrToInt( lineName ) ;
              end ;

      dxfFetchLine ;
    end ;

    // skip Spline frame control point
    if (attr <> 16) then begin
      SetLength( arPoint, length( arPoint ) + 1 ) ;
      arPoint[ length( arPoint ) - 1 ]   := ptg   ;
      arPoint[ length( arPoint ) - 1 ].M := ptg.M ;
      if _elev <> 0 then
        arPoint[ length( arPoint ) - 1 ].Z := _elev ;
    end ;
  end ;

  procedure TGIS_LayerDXF.doLwPolyline ;
  var
    attr      : Integer      ;
    hndl      : String       ;
    layername : String       ;
    lstyle    : String       ;
    lweight   : String       ;
    cwidth    : String       ;
    color     : Integer      ;
    elev      : Double       ;
    extr      : TGIS_Point3D ;
    cnt       : Integer      ;
    ar        : array of TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D[1] {$ENDIF};
    i         : Integer      ;

    procedure create_shape ;
    begin
      if not assigned( currShape ) then begin
        // if is closed, make polygon
        if ( ( attr and 1 ) <> 0 ) then
          currShape := CreateShape( TGIS_ShapeType.Polygon, TGIS_DimensionType.XYZ )
        else
          currShape := CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XYZ ) ;

        currShape.Lock( TGIS_Lock.Projection ) ; // unlocked in doPolylineVertex ;
        currShape.AddPart ;
        currShape.SetField( GIS_DXF_FLD_LAYER_NAME, layername           ) ;
        currShape.Layer := getSubLayerByName(  layername ) ;
        // show only outline to avoid overlapping fill
        currShape.Params.Area.Pattern := TGIS_BrushStyle.Clear ;

        currShape.SetField( GIS_DXF_FLD_ELEVATION , elev  ) ;
        if color<>0 then
          setShapeColor( color ) ;

        if not IsStringEmpty( lstyle ) then
          setLineStyle( layername, lstyle ) ;

        if not IsStringEmpty( lweight ) then
          setLineWeight( lweight )
        else if not IsStringEmpty( cwidth ) then
          setLineWeight( cwidth ) ;

      end ;
    end ;

  begin

    extr := GisPoint3D( 0, 0, 1 ) ;

    currShape := nil ;
    attr      := 0   ;
    hndl      := ''  ;
    layername := ''  ;
    color     := 0   ;
    elev      := 0   ;
    cnt       := 0   ;
    cwidth    := ''  ;
    {$IFDEF GIS_NORECORDS}
      ar[0] := new TGIS_Point3D ;
    {$ENDIF}

    dxfFetchLine ;

    if dxfTestLine( GIS_DXF_CLAYER, '' ) and
       ( not isVisibleLayer( lineName ) )
    then exit ;

    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_C0, '' ) )  do
    begin
      if      dxfTestLine( GIS_DXF_CHANDLE, '' ) then begin
                hndl := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLAYER, '' ) then begin
                layername := lineName ;
                  if layername = '0' then
                    layername := insertName ;
              end
      else if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
                color := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
                lstyle := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
                lweight := lineName ;
              end
      else if dxfTestLine( GIS_DXF_C43, '' ) then begin
                cwidth := lineName ;
              end
      else if dxfTestLine( GIS_DXF_CVERTEXATTR70, '' ) then begin
                attr := StrToInt( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C10, '' ) then begin
                inc( cnt ) ;
                SetLength( ar, cnt ) ;
                {$IFDEF GIS_NORECORDS}
                  ar[cnt-1] := new TGIS_Point3D ;
                {$ENDIF}
                ar[cnt-1].X := DotStrToFloat( lineName ) ;
                ar[cnt-1].Z := elev ;
              end
      else if dxfTestLine( GIS_DXF_C20, '' ) then begin
                ar[cnt-1].Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C38, '' ) then begin
                elev := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C42, '' ) then begin
                ar[cnt-1].M := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C210, '' ) then begin
                extr.X := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C220, '' ) then begin
                extr.Y := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_C230, '' ) then begin
                extr.Z := DotStrToFloat( lineName ) ;
              end
      else if dxfTestLine( GIS_DXF_CADE, '' ) then begin
                create_shape ;
                doExtendedData ;
                continue ;
              end
        else if dxfTestLine( 101, '' ) then begin
                  doEmbeddedData ;
                  continue ;
              end ;
      dxfFetchLine ;
    end ;

    if attr=1 then begin
      SetLength( ar, length( ar ) + 1 ) ;
      ar[ length( ar) - 1 ] := ar[0] ;
    end ;

    create_shape ;
    if assigned( currShape ) then begin
      prepareMatrix( extr ) ;
      i := 0 ;
      while i < length( ar ) do begin
        if ( ar[ i ].M <> 0 ) then begin
          if ( length( ar ) <= i + 1 ) then begin
            currShape.AddPoint3D( transformPtg( ar[ i ] ) ) ;
            inc( i );
          end
          else begin
            calculateBulge( ar[ i ], ar[ i+1 ] ) ;
            inc( i );
          end ;
        end
        else begin
          currShape.AddPoint3D( transformPtg( ar[i] ) ) ;
          inc(i) ;
        end ;
      end ;
      currShape.Unlock ;
    end ;
    {$IFDEF OXYGENE}
      if assigned( ar ) then
        ar := nil ;
    {$ENDIF}
  end ;

  procedure TGIS_LayerDXF.doVersion ;
  var
    icodepage : Integer ;
    iver      : Integer ;
  begin
    icodepage := 0 ;
    dxfFetchLine ;
    try
      iver := StrToInt( Copy( lineName, StringFirst + 2, 4 )  ) ;
    except
      iver := 0 ;
    end ;

    case iver of
      1006 : iCadVersion := 'Release 10';
      1009 : iCadVersion := 'Release 11/12 (LT R1/R2)' ;
      1012 : iCadVersion := 'Release 13 (LT95)' ;
      1014 : iCadVersion := 'Release 14, 14.01 (LT97/LT98)' ;
      1015 : iCadVersion := 'AutoCAD 2000/2000i/2002 ' ;
      1018 : iCadVersion := 'AutoCAD 2004/2005/2006' ;
      1021 : iCadVersion := 'AutoCAD 2007/2008/2009' ;
      1024 : iCadVersion := 'AutoCAD 2010/2011/2012 ' ;
      1027 : iCadVersion := 'AutoCAD 2013/2014/2015/2016/2017' ;
      1032 : iCadVersion := 'AutoCAD 2018/2019/2020' ;
      else   iCadVersion := 'Unknown' ;
    end ;

    if iver >= 1021 then
      icodepage := 65001 ;

    if isBinaryDXF then
      iCadVersion := iCadVersion + ' binary' ;

    // Starting with DXF version R2007 the files are saved with UTF-8 encoding
    if ( not bForceCodePage ) and ( icodepage <> 0 ) then begin
      CodePage := 65001 ;
      bForceCodePage := True ;
      dxfFile.CodePage := CodePage ;
      dxfFile.FixCodePage := False ;
    end;
  end ;

  procedure TGIS_LayerDXF.doCodePage ;
  var
    cp  : String ;
    icp : Integer ;
  begin
    dxfFetchLine ;

    cp := LowerCase( lineName ) ;

    if ( cp = 'latin1' ) or ( cp = 'ansi_1252' ) or ( cp = 'iso-8859-1' ) or
       ( cp = 'cp819' ) or ( cp = 'csiso' ) or ( cp = 'ibm819' ) or
       ( cp = 'iso_8859-1' ) or ( cp = 'iso8859-1' ) or ( cp = 'iso-ir-100' ) or
       ( cp = 'l1' ) then
        icp :=  28591
     else if ( cp = 'big5' ) or ( cp = 'ansi_950' ) or ( cp = 'cn-big5' ) or
             ( cp = 'csbig5' ) or ( cp = 'x-x-big5' ) or ( cp = 'big5-hkscs' ) then
        icp :=  950
     else if ( cp = 'eucjp' ) or ( cp = 'euc-jp' ) or ( cp = 'cseucpkdfmtjapanese' ) or
             ( cp = 'x-euc' ) or ( cp = 'x-euc-jp' ) then
        icp :=  20932
     else if ( cp = 'euckr' ) then
        icp :=  51949
     else if ( cp = 'gb2312' ) or ( cp = 'chinese' ) or ( cp = 'cn-gb' ) or
             ( cp = 'csgb2312' ) or ( cp = 'csgb231280' ) or
             ( cp = 'csiso58gb231280' ) or ( cp = 'gb_2312-80' ) or
             ( cp = 'gb231280' ) or ( cp = 'gb2312-80' ) or ( cp = 'gbk' ) or
             ( cp = 'iso-ir-58' ) or ( cp = 'ansi_936' ) then
        icp :=  936
     else if ( cp = 'ansi_949' ) then
        icp :=  949
     else if ( cp = 'gb18030' ) then
        icp := 54936
     else if ( cp = 'shift-jis' ) or ( cp = 'ansi_932' ) or ( cp = 'shift_jis' ) or
             ( cp = 'csShiftJIS' ) or ( cp = 'cswindows31j' ) or ( cp = 'ms_kanji' ) or
             ( cp = 'x-ms-cp932' ) or ( cp = 'x-sjis' ) then
        icp :=  932
     else if ( cp = 'utf88-bit' ) or ( cp = 'utf16' ) or ( cp = 'utf8' ) or
             ( cp = 'utf-8' ) then
        icp := 65001
     else if ( cp = 'koi8-r' ) then
        icp :=  20866
     else if ( cp = 'koi8-u' ) then
        icp :=  21866
     else if ( cp = 'iso8859-1' ) then
        icp :=  28591
     else if ( cp = 'iso8859-2' ) then
        icp :=  28592
     else if ( cp = 'iso8859-3' ) then
        icp :=  28593
     else if ( cp = 'iso8859-4' ) or ( cp = 'ansi_1257' ) then
        icp :=  28594
     else if ( cp = 'iso8859-5' ) then
        icp :=  28595
     else if ( cp = 'iso8859-6' ) or ( cp = 'ansi_1256' ) then
        icp :=  28596
     else if ( cp = 'iso8859-7' ) or ( cp = 'ansi_1253' ) then
        icp :=  28597
     else if ( cp = 'iso8859-8' ) then
        icp :=  28598
     else if ( cp = 'iso8859-8-i' ) or ( cp = 'ansi_1255' ) then
        icp :=  862
     else if ( cp = 'iso8859-9' ) or ( cp = 'ansi_1254' ) then
        icp :=  28599
     else if ( cp = 'iso8859-10' ) then
        icp :=  28600
     else if ( cp = 'iso8859-13' ) then
        icp :=  28603
     else if ( cp = 'iso8859-14' ) then
        icp :=  28604
     else if ( cp = 'iso8859-15' ) then
        icp :=  28605
     else if ( cp = 'ibm 850' ) then
        icp :=  850
     else if ( cp = 'ibm 866' ) then
        icp :=  866
     else if ( cp = 'cp874' ) or ( cp='ansi_874' ) then
        icp :=  874
     else if ( cp = 'cp1250' ) or ( cp='ansi_1250' ) then
        icp :=  1250
     else if ( cp = 'cp1251' ) or ( cp = 'ansi_1251' ) then
        icp :=  1251
     else if ( cp = 'cp1252' ) then
        icp :=  1252
     else if ( cp = 'cp1253' ) then
        icp :=  1253
     else if ( cp = 'cp1254' ) then
        icp :=  1254
     else if ( cp = 'cp1255' ) then
        icp :=  1255
     else if ( cp = 'cp1256' ) then
        icp :=  1256
     else if ( cp = 'cp1257' ) then
        icp :=  1257
     else if ( cp = 'cp1258' ) then
        icp :=  1258
     else
        icp := 65001 ;

     if not bForceCodePage then begin
       CodePage := icp ;
       dxfFile.CodePage := CodePage ;
       dxfFile.FixCodePage := False ;
     end ;
  end ;

  procedure TGIS_LayerDXF.doTables ;
  var
    layername : String ;
    lpattern  : String ;
    icolor    : Integer ;
    lstyle    : String  ;
    weigth    : Integer ;
    attr      : Boolean ;
    layeroff  : Boolean ;
    la        : TObject ;
    vis       : Integer ;
    len       : Integer ;
    ptrn      : String  ;
    ltype_name    : String ;
    ltype_desc    : String ;
    lstype_format : String ;
    ltype_pt_len  : Double ;
    ltype_pt_cnt  : Integer ;
    ltype_idx     : Integer ;
    ltype_pt_arr  : TGIS_DoubleArray ;
  begin
    while ( not dxfEof ) and
          ( not dxfTestLine( GIS_DXF_CENDSEC, GIS_DXF_NSECTION ) ) do
    begin

      // look for layers colors
      if dxfTestLine( GIS_DXF_TELAYER, GIS_DXF_NLAYER ) then begin
        layername := '0' ;
        icolor := 0 ;
        weigth := 1 ;
        la := T_DxfLayer.CreateDefault( layername ) ;
        dxfFetchLine ;

        while not dxfTestLine( GIS_DXF_CNTABLES, GIS_DXF_ENTABLES ) and
              not dxfTestLine( GIS_DXF_TELAYER, GIS_DXF_NLAYER ) do begin
          layeroff := False ;
          if dxfTestLine( GIS_DXF_TLAYER, '' ) then begin
            T_DxfLayer(la).Name := lineName ;
            layername := lineName ;
          end;

          if dxfTestLine( GIS_DXF_CCOLOR, '' ) then begin
            if not IsStringEmpty( Trim( lineName ) ) then
              icolor := StrToInt(lineName) ;
            T_DxfLayer(la).Color := icolor ;
            if icolor < 0 then begin
              T_DxfLayer(la).Visible := False ;
              layeroff := True ;
            end;
          end ;

          if dxfTestLine( GIS_DXF_C70, '' ) then begin
             attr := True ;
             if not IsStringEmpty( Trim( lineName ) ) then begin
               vis := StrToInt( lineName ) ;
               if ( 65 - vis = 0 ) or
                  ( (vis and 1) = 1 ) or
                  layeroff
               then attr := False
               else attr := True ;
             end;
             T_DxfLayer(la).Visible := attr ;
          end ;

          if dxfTestLine( GIS_DXF_CLLSTYLE, '' ) then begin
             if not IsStringEmpty( Trim( lineName ) ) then begin
               lstyle := lineName ;
               T_DxfLayer(la).PenName  := lstyle ;
               T_DxfLayer(la).PenStyle := getLineStyle( layername, lstyle, lpattern )
             end ;
          end ;

          if dxfTestLine( GIS_DXF_CLLWEIGHT, '' ) then begin
             if not IsStringEmpty( Trim( lineName ) ) then
               weigth := getLineWidth( lineName ) ;
             if weigth > 0 then
               T_DxfLayer(la).Width := weigth ;
          end ;

          dxfFetchLine ;
        end ;
        FLayers.AddOrSetValue( layername, la ) ;
      end ;

      // look for line style
      if dxfTestLine( GIS_DXF_TELAYER, GIS_DXF_NLTYPE ) then begin
        ltype_name := '';
        ltype_desc := '' ;
        ltype_pt_len := 0.0 ;
        ltype_pt_cnt := 0 ;
        ltype_idx := 0 ;
        ltype_pt_arr := nil ;

        dxfFetchLine ;
        while not dxfTestLine( GIS_DXF_CNTABLES, GIS_DXF_ENTABLES ) and
              not dxfTestLine( GIS_DXF_TELAYER, GIS_DXF_NLTYPE ) do begin

          if dxfTestLine( 2, '' ) then
            ltype_name := lineName
          else if dxfTestLine( 3, '' ) then
            ltype_desc := lineName
          else if dxfTestLine( 73, '' ) then begin
            ltype_pt_cnt := StrToInt(lineName) ;
            SetLength( ltype_pt_arr, ltype_pt_cnt ) ;
          end
          else if dxfTestLine( 40, '' ) then
            ltype_pt_len := DotStrToFloat(lineName)
          else if dxfTestLine( 49, '' ) then begin
            if ltype_idx < ltype_pt_cnt then
              ltype_pt_arr[ltype_idx] := DotStrToFloat(lineName);
            inc( ltype_idx ) ;
          end ;

          dxfFetchLine ;
        end ;

        if ltype_pt_cnt > 0 then begin
          if ltype_pt_len = 0 then
            ltype_pt_len := 1 ;
          lstype_format := '&F(100%)';
          for ltype_idx := 0 to ltype_pt_cnt-1 do begin
            // > 0 line, < 0 gap, = 0 point
            len := RoundS(ltype_pt_arr[ltype_idx] * ltype_pt_len * 1440/96/2.54) ;
            if len > 0 then
              lstype_format := lstype_format + Format('LINE(%dS)', [len])
            else if len = 0 then
              lstype_format := lstype_format + 'LINE(10S)'
            else
              lstype_format := lstype_format + Format('MOVE(%dS 0)', [Abs(len)])
          end ;
          lstype_format := lstype_format + 'E()';
          lstyles.AddOrSetValue( ltype_name, lstype_format ) ;
        end ;
      end ;

      if not dxfTestLine( GIS_DXF_TELAYER, GIS_DXF_NLAYER ) and
         not dxfTestLine( GIS_DXF_TELAYER, GIS_DXF_NLTYPE ) then
        dxfFetchLine ;
    end ;

    for la in FLayers.Values do begin
      ptrn := '' ;
      if lstyles.TryGetValue( T_DxfLayer(la).PenName, ptrn ) then
        T_DxfLayer(la).PenPattern := ptrn ;
    end ;
  end ;

  procedure TGIS_LayerDXF.calculateBulge(
    const _v1, _v2 : TGIS_Point3D
  );
  var
    arccenter     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    arcradiusa    : Double ;
    arcradiusb    : Double ;
    arcstartangle : Double ;
    arcendangle   : Double ;
    arcrotation   : Double ;
    swglen        : Double ;
    sdist         : Double ;

    procedure strokeArc(
      const _center   : TGIS_Point3D ;
      const _radiusA  : Double       ;
      const _radiusB  : Double       ;
      const _start    : Double       ;
      const _stop     : Double       ;
      const _rotation : Double       ;
      const _segments : Integer
    ) ;
    var
      cnt      : Integer ;
      angle    : Double  ;
      arcangle : Double  ;
      delta    : Double  ;
      step     : Double  ;
      steps    : Integer ;
      rsin     : Double  ;
      rcos     : Double  ;
      asin     : Double  ;
      acos     : Double  ;
      ptg      : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;

      function fmod( a, b : Double ) : Double ;
      var
       f : Integer ;
      begin
        f := TruncS( a/b ) ;
        Result := a - (b*f) ;
      end ;

    begin
      // rotation
      SinCos( -_rotation, rsin, rcos ) ;

      delta := (_stop-_start) ;

      if delta > 0 then
        arcangle := fmod( (_stop-_start) + 4*Pi, 2*Pi)
      else
        arcangle := fmod( (_stop-_start) -4*Pi, 2*Pi) ;

      if arcangle = 0 then begin
        // full rotation
        if _stop <> _start then
          arcangle := 2*Pi ;
      end ;

      // calculate number of segments - and minimize it
      if Abs( arcangle ) < Pi/4 then
        steps := _segments div 4
      else
        steps := RoundS( Abs( arcangle ) / (2*Pi) * _segments ) ;

      step := arcangle / Max( 4, steps ) ;

      // calculate elliptical arc
      angle := _start ;
      for cnt := 0 to steps do begin
        SinCos( angle, asin, acos ) ;

        ptg.Y := -_radiusB * asin * rcos +
                  _radiusA * acos * rsin + _center.Y ;
        ptg.X :=  _radiusA * acos * rcos +
                  _radiusB * asin * rsin + _center.X ;
        ptg.Z := _center.Z ;
        currShape.AddPoint3D( transformPtg( ptg ) ) ;

        angle := angle + step ;
      end ;
    end ;

  begin
    if T_Polyline.CalculateBulge(
          currShape,
          _v1,
          _v2,
          arccenter,
          arcradiusa,
          arcradiusb,
          arcstartangle,
          arcendangle,
          arcrotation
      ) then begin
          swglen := Abs( arcendangle - arcstartangle ) ;
          sdist  := (arcradiusa) ;

          if ( swglen + 1e-15 ) < ( 0.2*Pi / sdist ) then begin
            currShape.AddPoint3D( transformPtg( _v1 ) );
            currShape.AddPoint3D( transformPtg( _v2 ) );
          end
          else
            strokeArc( arccenter,
                       arcradiusa,
                       arcradiusb,
                       arcstartangle,
                       arcendangle,
                       arcrotation,
                       90
                      )
      end
      else begin
        currShape.AddPoint3D( transformPtg( _v1 ) );
        currShape.AddPoint3D( transformPtg( _v2 ) );
      end ;
  end ;

  procedure TGIS_LayerDXF.drawArc(
    const _center   : TGIS_Point3D ;
    const _radiusA  : Double       ;
    const _radiusB  : Double       ;
    const _start    : Double       ;
    const _angle   : Double
  ) ;
  const
    EPS = 1e-10;
  var
    i          : Integer ;
    ptg        : TGIS_Point3D;
    seg_no_2pi : Integer ;
    seg_len    : Double  ;
    seg_cnt    : Integer ;
    delta      : Double  ;
    tmp        : Double  ;
    rsin, rcos : Double  ;
  begin
    seg_no_2pi := Min( Max( RoundS( _radiusA / 2 ), 6 ), 256 ) ;
    seg_len :=  0.5*Pi / seg_no_2pi ;

    seg_cnt := TruncS( _angle / (0.5*Pi) * seg_no_2pi ) ;

    delta := Sign( _angle ) * seg_len ;

    ptg := GisPoint3D( _radiusA * Cos( _start ) + _center.X,
                       _radiusB * Sin( _start ) + _center.Y,
                       _center.Z
                     );
    currShape.AddPoint3D( transformPtg( ptg ) );

    if ( Abs( Abs( _angle ) - seg_len ) < EPS ) then begin
      ptg := GisPoint3D( _radiusA * Cos( _start + _angle ) + _center.X,
                         _radiusB * Sin( _start + _angle ) + _center.Y,
                         _center.Z
                       );
      currShape.AddPoint3D( transformPtg( ptg ) );
      exit;
    end ;

    i   := 1 ;
    tmp := _start + delta / 2 ;
    while ( i < seg_cnt  ) do begin
      SinCos( tmp, rsin, rcos ) ;
      ptg := GisPoint3D( _radiusA * rcos + _center.X,
                         _radiusB * rsin + _center.Y,
                         _center.Z
                       );

      currShape.AddPoint3D( transformPtg( ptg ) );

      tmp := tmp + delta ;
      inc( i );
    end ;

    ptg := GisPoint3D( _radiusA * Cos( _start + _angle ) + _center.X,
                       _radiusB * Sin( _start + _angle ) + _center.Y,
                       _center.Z
                     );

    currShape.AddPoint3D( transformPtg( ptg ) );
  end ;

  function TGIS_LayerDXF.getPaletteColor(
    const _idx : Integer
  ) : TGIS_Color ;
  var
    cl       : TGIS_Color ;
    r,g,b    : Byte    ;
  begin
    cl := TGIS_Color.Black ;
    if _idx in [ 0..255 ] then begin
      r := GIS_DXF_COLOR[ _idx ][ 0 ] ;
      g := GIS_DXF_COLOR[ _idx ][ 1 ] ;
      b := GIS_DXF_COLOR[ _idx ][ 2 ] ;
      cl := TGIS_Color.FromRGB( r, g, b ) ;
    end ;

    Result := cl ;
  end;

  procedure TGIS_LayerDXF.setShapeColor(
    const _idx  : Integer
  ) ;
  var
    cl       : TGIS_Color ;
    clvwr    : TGIS_Color  ;
    r,g,b    : Byte    ;
    vr,vg,vb : Byte    ;
    ischanged: Boolean ;
  begin
    lastColor := _idx ;

    if _idx < 0 then exit ;

    cl := getPaletteColor( _idx ) ;

    if assigned( Viewer ) then
      clvwr := Viewer.Ref.Color
    else
      clvwr := TGIS_Color.White ;

    r  := cl.R ;
    g  := cl.G ;
    b  := cl.B ;
    vr := clvwr.R ;
    vg := clvwr.G ;
    vb := clvwr.B ;

    if ( Abs( vr - r ) < 16 ) and
       ( Abs( vg - g ) < 16 ) and
       ( Abs( vb - b ) < 16 )
    then // color very close to the window color
      cl :=  TGIS_Color.FromRGB( r xor vr, g xor vg, b xor vb ) ;

    ischanged := False ;
    if currShape is TGIS_ShapeArc then begin
      if cl.ARGB <> currShape.Layer.Params.Line.Color.ARGB then
      begin
        currShape.Params.Line.Color := cl ;
        currShape.Params.Line.Style := TGIS_PenStyle.Solid ;
        ischanged := True ;
      end ;
    end
    else if currShape is TGIS_ShapePolygon then begin
      if cl.ARGB <> currShape.Layer.Params.Area.Color.ARGB then
      begin
        ischanged := True ;
        if cl.ARGB <> TGIS_Color.White.ARGB then begin
          currShape.Params.Area.OutlineColor := cl ;
          currShape.Params.Area.Color        := cl
        end
        else begin
          currShape.Params.Area.OutlineColor := cl ;
          if assigned( Viewer ) then
            currShape.Params.Area.Color := Viewer.Ref.Color     ;
        end ;
      end ;
    end
    else begin
      if cl.ARGB <> currShape.Layer.Params.Marker.Color.ARGB then
      begin
        ischanged := True ;
        currShape.Params.Marker.Size      := 1;
        currShape.Params.Marker.Color     := cl ;
        currShape.Params.Labels.Color     := cl ;
        currShape.Params.Labels.FontColor := cl ;
      end ;
    end ;

    currShape.SetField( GIS_DXF_FLD_COLOR, _idx ) ;
    if ischanged then
      currShape.Params.Ground := TGIS_3DGroundType.AboveZero ;
  end ;

  function TGIS_LayerDXF.getLineStyle(
    const _layer     : String ;
    const _name      : String ;
      var _pattern   : String
  ) : TGIS_PenStyle ;
  var
    lname : String ;
    res   : TGIS_PenStyle ;
    obj   : TObject ;
  begin
    lname := UpperCase( _name ) ;
    res := TGIS_PenStyle.Solid ;
    _pattern := '' ;

    if IsStringEmpty( lname ) or (lname = 'BYLAYER') then begin
      if FLayers.TryGetValue( _layer, obj ) then begin
        res := T_DxfLayer(obj).PenStyle ;
        _pattern := T_DxfLayer(obj).PenPattern ;
      end ;
    end
    else if (lname = 'CONTINUOUS') or (lname = 'ACAD_ISO01W100') then
      res := TGIS_PenStyle.Solid
    else if (lname = 'ACAD_ISO07W100') or (lname = 'DOT') then
      res := TGIS_PenStyle.Dot
    else if (lname = 'DOTTINY') or (lname = 'ISO DOT') then
      res := TGIS_PenStyle.Dot
    else if (lname = 'DOT2') then
      res := TGIS_PenStyle.Dot
    else if (lname = 'DOTX2') then
      res := TGIS_PenStyle.Dot
    else if (lname = 'ACAD_ISO02W100') or (lname = 'ACAD_ISO03W100') or
            (lname = 'DASHED') or (lname = 'HIDDEN') or (lname = 'ISO DASH') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'DASHEDTINY') or (lname = 'HIDDEN2') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'DASHED2') or (lname = 'HIDDEN2') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'DASHEDX2') or (lname = 'HIDDENX2') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'ACAD_ISO10W100') or (lname = 'DASHDOT') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'DASHDOTTINY') or (lname = 'ISO DASH DOT') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'DASHDOT2') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'ACAD_ISO04W100') or (lname = 'DASHDOTX2') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'ACAD_ISO12W100') or (lname = 'DIVIDE') then
      res := TGIS_PenStyle.DashDotDot
    else if (lname = 'DIVIDETINY') or (lname = 'ISO DASH Double-DOT') then
      res := TGIS_PenStyle.DashDotDot
    else if (lname = 'DIVIDE2') or (lname = 'ISO DASH TRIPLE-DOT') then
      res := TGIS_PenStyle.DashDotDot
    else if (lname = 'ACAD_ISO05W100') or (lname = 'DIVIDEX2') then
      res := TGIS_PenStyle.DashDotDot
    else if (lname = 'CENTER') or (lname = 'ISO DASH LONG GAPS') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'CENTERTINY') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'CENTER2') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'CENTERX2') then
      res := TGIS_PenStyle.Dash
    else if (lname = 'BORDER') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'BORDERTINY') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'BORDER2') then
      res := TGIS_PenStyle.DashDot
    else if (lname = 'BORDERX2') then
      res := TGIS_PenStyle.DashDot
    else if lstyles.TryGetValue( _name, lname ) then
      _pattern := lname
    else
      res := TGIS_PenStyle.Solid ;

    Result := res ;
  end ;

  procedure TGIS_LayerDXF.setLineStyle(
    const _layer  : String ;
    const _name   : String
  ) ;
  var
    res      : TGIS_PenStyle ;
    lpattern : String ;
  begin
    res := getLineStyle( _layer, _name, lpattern ) ;

    if      currShape is TGIS_ShapeArc then begin
      if IsStringEmpty( lpattern ) then begin
        if res <> currShape.Layer.Params.Line.Style then
          currShape.Params.Line.Style := res
      end
      else begin
        lpattern := GIS_PARAMTXT_TYPE_SYMBOL + ':' + lpattern ;
        if lpattern <> currShape.Layer.Params.Line.StyleAsText then
          currShape.Params.Line.StyleAsText := lpattern
      end ;
    end
    else if currShape is TGIS_ShapePolygon then begin
      if IsStringEmpty( lpattern ) then begin
        if res <> currShape.Layer.Params.Area.OutlineStyle then
          currShape.Params.Area.OutlineStyle := res ;
      end
      else begin
        lpattern := GIS_PARAMTXT_TYPE_SYMBOL + ':' + lpattern ;
        if lpattern <> currShape.Layer.Params.Area.OutlineStyleAsText then
          currShape.Params.Area.OutlineStyleAsText := lpattern
      end ;
    end ;
  end ;

  function TGIS_LayerDXF.getLineWidth(
    const _val : String
  ) : Integer ;
  var
    lw : Integer ;
  begin
    if TryStrToInt( _val, lw ) then begin
      if lw > 0 then
        Result := RoundS(24 * lw / 56.693 ) // mm to twips
      else
        Result := lw ;
    end
    else
      Result := 1 ;
  end ;

  procedure TGIS_LayerDXF.setLineWeight(
    const _name : String
  ) ;
  var
    size : Integer ;
  begin
    size := getLineWidth( _name ) ;
    if size = 0 then
      size := 1 ;
    // negative is by layer or block
    if size > 0 then begin
      if currShape is TGIS_ShapeArc then begin
        if size <> currShape.Layer.Params.Line.Width then
          currShape.Params.Line.Width := size
      end
      else if currShape is TGIS_ShapePolygon then
        if size <> currShape.Layer.Params.Area.OutlineWidth then
          currShape.Params.Area.OutlineWidth := size ;
    end ;

    currShape.SetField( GIS_DXF_FLD_WEIGHT , size ) ;
  end ;

  procedure TGIS_LayerDXF.dxfFetchLine ;
  var
    abort : Boolean ;
    _len  : Integer ;

    function get_str : String ;
    begin
      Result := TrimRight( dxfFile.ReadLine ) ;
      inc( lineNo ) ;
      dxfEof := dxfFile.Position >= dxfFile.Size ;
    end ;

    function get_int : Integer ;
    begin
      try
        Result := StrToInt( get_str ) ;
      except
        Result := 0 ;
      end ;
    end ;

    { convert string value to Double }
    function get_val ( const _val : TBytes ) : Double ;
    var
      {$IFDEF OXYGENE}
        bb : TBytes ;
      {$ELSE}
        val : T_val ;
        i   : Integer ;
      {$ENDIF}
    begin
      Result := 0 ;
      {$IFDEF OXYGENE}
      {$ELSE}
        val.double_val := 0 ;
      {$ENDIF}
      if ( length( _val ) = 0 ) or ( _len>8 ) then exit ;
      {$IFDEF OXYGENE}
        bb := new Byte[8] ;
        GisCopyMemory( _val, 0, bb, 0, _len ) ;
      {$ELSE}
        for i := 0 to _len-1 do
          val.byte_val[i] := _val[i] ;
      {$ENDIF}
      if _len < 5 then
        {$IFDEF OXYGENE}
          Result := BitConverter.ToInt32( bb, 0 )
        {$ELSE}
          Result := val.int_val
        {$ENDIF}
      else
        {$IFDEF OXYGENE}
          Result :=  BitConverter.ToDouble( bb, 0 ) ;
        {$ELSE}
          Result := val.double_val ;
        {$ENDIF}
    end ;

    function get_bstr : TBytes ;
    var
      i  : Integer ;
      j  : Integer ;
      st : TBytes  ;
      ch : Byte    ;
    begin
      SetLength( st, 256 ) ;
      i := 0 ;
      j := 0 ;
      st[0] := 0 ;
      repeat
        {$IFNDEF OXYGENE}
          dxfFile.Read(ch,1) ;
        {$ELSE}
          dxfFile.ReadByte(ch) ;
        {$ENDIF}
        inc( i ) ;
        if ch <> 0 then begin
          st[j] := ch ;
          inc( j ) ;
        end
      until ( ch = 0 ) and ( ( i>1 ) or ( j>1 ) ) ;

      if j > 0 then
        Result := Copy( st, 0, j )
      else
        Result := Copy( st, 0, 1 ) ;

      if (lineCode = 1) and ( length( Result )=0 ) then
        Result := get_bstr ;

      _len := j ;
      inc( lineNo ) ;
      dxfEof := dxfFile.Position >= dxfFile.Size ;
    end ;

    function get_bdouble : TBytes ;
    begin
      SetLength( Result, 8 ) ;
      {$IFNDEF OXYGENE}
        dxfFile.Read( Result[0], 8 ) ;
      {$ELSE}
        dxfFile.Read( Result, 0, 8 ) ;
      {$ENDIF}
      _len   := 8 ;

      inc( lineNo ) ;
      dxfEof := dxfFile.Position >= dxfFile.Size ;
    end ;

    function get_bcode : Variant ;
    var
      chi  : array [0..1] of Byte ;
    begin
      {$IFNDEF OXYGENE}
        dxfFile.Read( chi[0], 2 ) ;
      {$ELSE}
        dxfFile.Read( chi, 2 ) ;
      {$ENDIF}
      if lineCode = 90 then
        {$IFNDEF OXYGENE}
          dxfFile.Read( chi[0], 2 ) ;
        {$ELSE}
          dxfFile.Read( chi, 2 ) ;
        {$ENDIF}

      Result := IntToStr( chi[1] + chi[0] ) ;

      inc( lineNo ) ;
      dxfEof := dxfFile.Position >= dxfFile.Size ;
    end ;

  begin
    if isBinaryDXF then begin
      lineCode := VarToInt32( get_bcode ) ;
      if ( lineCode in [10..59,79] ) or ( lineCode in [210..230] )  then
        lineName := DotFloatToStr( get_val( get_bdouble ) )
      else if ( lineCode in [60..74,90] ) or
              (( lineCode >230) and ( lineCode <370 ))  then
        lineName := DotFloatToStr( get_val( get_bstr ) )
      else
        lineName := ConvertAnsiString( get_bstr, _len ) ;
    end
    else begin
      if not dxfEof then
        lineCode := get_int ;
      if not dxfEof then
        lineName := get_str ;
    end ;

    maxLineNo := Max( maxLineNo, lineNo ) ;
    maxPos    := Max( maxPos, dxfFile.Position ) ;

    if maxLineNo mod 5000 = 0 then begin
      abort := RaiseBusyShake( Self, maxPos, dxfFile.Size ) ;
    end ;

  end ;

  function TGIS_LayerDXF.dxfTestLine(
    const _code : Integer
  ) : Boolean ;
  begin
    Result := dxfTestLine( _code, '' ) ;
  end ;

  function TGIS_LayerDXF.dxfTestLine(
    const _code : Integer ;
    const _name : String
  ) : Boolean ;
  begin
    Result := lineCode = _code ;
    if Result and ( not IsStringEmpty( _name ) ) then
      Result := Result and ( lineName = _name ) ;
  end ;

  procedure TGIS_LayerDXF.writeGeometry(
    const _strm : TGIS_BufferedStream ;
    const _shp  : TGIS_Shape
  ) ;
  var
    part_no    : Integer      ;
    part_size  : Integer      ;
    point_no   : Integer      ;
    ptg        : TGIS_Point3D ;
    lname      : String       ;
    has_dif_z  : Boolean      ;
    is_3D      : Boolean      ;
    zval       : Double       ;
    bpoly_mesh : Boolean      ;
    mb         : TGIS_MultiPatchBuilder ;
    v          : TGIS_SingleVector ;
    f1, f2, f3 : Integer ;
    vv         : Integer ;

    function get_positionsH( const _pos : TGIS_LabelPositions ) : String ;
    begin
      if GisTestLabelPosition( TGIS_LabelPosition.UpCenter, _pos      ) then
        Result := '1'
      else if GisTestLabelPosition( TGIS_LabelPosition.UpRight, _pos       ) then
        Result := '0'
      else if GisTestLabelPosition( TGIS_LabelPosition.UpLeft, _pos        ) then
        Result := '2'
      else if GisTestLabelPosition( TGIS_LabelPosition.MiddleLeft, _pos    ) then
        Result := '2'
      else if GisTestLabelPosition( TGIS_LabelPosition.MiddleCenter, _pos  ) then
        Result := '1'
      else if GisTestLabelPosition( TGIS_LabelPosition.MiddleRight, _pos   ) then
        Result := '0'
      else if GisTestLabelPosition( TGIS_LabelPosition.DownLeft, _pos      ) then
        Result := '2'
      else if GisTestLabelPosition( TGIS_LabelPosition.DownRight, _pos     ) then
        Result := '0'
      else if GisTestLabelPosition( TGIS_LabelPosition.DownCenter, _pos    ) then
        Result := '1'
      else
        Result := '2' ;
    end ;

    function get_positionsV( const _pos : TGIS_LabelPositions ) : String ;
    begin
      if GisTestLabelPosition( TGIS_LabelPosition.UpCenter, _pos     ) then
        Result := '1'
      else if GisTestLabelPosition( TGIS_LabelPosition.UpRight, _pos      ) then
        Result := '1'
      else if GisTestLabelPosition( TGIS_LabelPosition.UpLeft, _pos       ) then
        Result := '1'
      else if GisTestLabelPosition( TGIS_LabelPosition.MiddleLeft, _pos   ) then
        Result := '0'
      else if GisTestLabelPosition( TGIS_LabelPosition.MiddleCenter, _pos ) then
        Result := '0'
      else if GisTestLabelPosition( TGIS_LabelPosition.MiddleRight, _pos  ) then
        Result := '0'
      else if GisTestLabelPosition( TGIS_LabelPosition.DownLeft, _pos     ) then
        Result := '3'
      else if GisTestLabelPosition( TGIS_LabelPosition.DownRight, _pos    ) then
        Result := '3'
      else if GisTestLabelPosition( TGIS_LabelPosition.DownCenter, _pos   ) then
        Result := '3'
      else
        Result := '0' ;
    end ;

    procedure write_layer ;
    begin
      _strm.WriteLine( GIS_DXF_ELAYER ) ;
      _strm.WriteLine( lname          ) ;
    end ;

    procedure write_color( const _color : TGIS_Color ;
                            const _idx   : String
                          ) ;
    var
      cl       : String ;
      i        : Integer ;
      vr,vg,vb : Byte    ;
    begin
      cl := '0' ;
      _strm.WriteLine( GIS_DXF_ECOLOR );

      if ( not IsStringEmpty( _idx ) ) and ( _idx <> '0' ) then
        cl := _idx
      else
        for i := low( GIS_DXF_COLOR ) to high( GIS_DXF_COLOR ) do begin
          vr := _color.R ;
          vb := _color.B ;
          vg := _color.G ;
          if (Abs( GIS_DXF_COLOR[i][0]-vr ) < 10) and
             (Abs( GIS_DXF_COLOR[i][1]-vg ) < 10) and
             (Abs( GIS_DXF_COLOR[i][2]-vb ) < 10) then begin
            cl := IntToStr( i ) ;
            Break;
          end ;
        end ;

      _strm.WriteLine( cl );
    end ;

    function fix_special_chars( const _text : String ) : String ;
    begin
      Result := _text ;
      Result := StringReplaceAll( Result, #$B0, '%%D' ) ;
      Result := StringReplaceAll( Result, #$B1, '%%P' ) ;
      Result := StringReplaceAll( Result, #$2300, '%%C' ) ;
      Result := ConstructParamString( Result ) ;
    end ;

    procedure write_text( {$IFNDEF OXYGENE}
                            const _custom : Boolean  = False
                          {$ELSE}
                            const _custom : Boolean := False
                          {$ENDIF}
                        ) ;
    var
      size : String ;
      angle: String ;
      pt   : TGIS_Point3D ;
      ptC  : TGIS_Point ;
      v,h  : String ;
      str  : String ;
    begin
      _strm.WriteLine( IntToStr( GIS_DXF_CTEXT ) ) ;
      _strm.WriteLine(           GIS_DXF_NTEXT   ) ;

      _strm.WriteLine( IntToStr( GIS_DXF_C1    ) ) ;

      if _custom then
        str := VarToString( _shp.GetField( _shp.Params.Labels.Field ) )
      else
        str := VarToString( _shp.GetField( GIS_DXF_FLD_LABEL ) ) ;

      _strm.WriteLine( fix_special_chars( str ) ) ;
      _strm.WriteLine( IntToStr( GIS_DXF_C40 ) ) ;
      size := VarToString( _shp.GetField( GIS_DXF_FLD_LABEL_HEIGHT ) ) ;
      if ( size = '0' ) or ( IsStringEmpty( size ) ) then
        size := '0.5';

      _strm.WriteLine( size );

      _strm.WriteLine( IntToStr( GIS_DXF_C50 ) ) ;
      angle := VarToString( _shp.GetField( GIS_DXF_FLD_LABEL_ANGLE ) ) ;
      if not IsStringEmpty( angle ) then
        _strm.WriteLine( angle )
      else
        _strm.WriteLine( '0' );

      h := get_positionsH( _shp.Params.Labels.Position ) ;
      v := get_positionsV( _shp.Params.Labels.Position ) ;
      _strm.WriteLine( IntToStr( GIS_DXF_C72 ) ) ;
      _strm.WriteLine( h ) ;
      _strm.WriteLine( IntToStr( GIS_DXF_C73 ) ) ;
      _strm.WriteLine( v ) ;

      if _custom then begin
        ptC := _shp.Centroid ;
        pt  := GisPoint3DFrom2D( ptC ) ;
      end
      else
        pt := _shp.GetPoint3D( 0, 0 ) ;

      _strm.WriteLine( '  7' ) ;
      _strm.WriteLine( 'STANDARD' ) ;

      _strm.WriteLine( GIS_DXF_E10            ) ;
      _strm.WriteLine( DotFloatToStr( pt.X  ) ) ;
      _strm.WriteLine( GIS_DXF_E20            ) ;
      _strm.WriteLine( DotFloatToStr( pt.Y  ) ) ;
      _strm.WriteLine( GIS_DXF_E30            ) ;
      _strm.WriteLine( DotFloatToStr( pt.Z  ) ) ;

      if (h<>'0') or (v<>'0') then begin
        _strm.WriteLine( GIS_DXF_E11            ) ;
        _strm.WriteLine( DotFloatToStr( pt.X  ) ) ;
        _strm.WriteLine( GIS_DXF_E21            ) ;
        _strm.WriteLine( DotFloatToStr( pt.Y  ) ) ;
        _strm.WriteLine( GIS_DXF_E31            ) ;
        _strm.WriteLine( DotFloatToStr( pt.Z  ) ) ;
      end
    end ;

    procedure write_ade ;
    var
      i         : Integer ;
      fieldname : String  ;
      exname    : String  ;
      str       : String  ;
    begin
      if _shp.Layer.Fields.Count = 0 then exit ;

      _strm.WriteLine( GIS_DXF_EADE          ) ;
      _strm.WriteLine( GIS_DXF_NADE          ) ;
      _strm.WriteLine( GIS_DXF_EADE_MARKER   ) ;
      _strm.WriteLine( GIS_DXF_NBEGIN        ) ;
      _strm.WriteLine( GIS_DXF_E1011         ) ;
      _strm.WriteLine( DotFloatToStr( 0    ) ) ;
      _strm.WriteLine( GIS_DXF_E1021         ) ;
      _strm.WriteLine( DotFloatToStr( 0    ) ) ;
      _strm.WriteLine( GIS_DXF_E1031         ) ;
      _strm.WriteLine( DotFloatToStr( 0    ) ) ;

      for i:=0 to Fields.Count -1 do begin
        fieldname := FieldInfo( i ).NewName ;
        exname    := FieldInfo( i ).NewName ;

        if (exname = GIS_DXF_FLD_LAYER_NAME)   or
           (exname = GIS_DXF_FLD_ELEVATION)    or
           (exname = GIS_DXF_FLD_LABEL)        or
           (exname = GIS_DXF_FLD_LABEL_ANGLE)  or
           (exname = GIS_DXF_FLD_LABEL_HEIGHT) or
           (exname = GIS_DXF_FLD_COLOR)        or
           (exname = GIS_DXF_FLD_WEIGHT) then continue ;

        _strm.WriteLine( GIS_DXF_EADE_DATA ) ;
        str := fix_special_chars( VarToString(_shp.GetField( fieldname )) ) ;
        _strm.WriteLine( Format('%s=%s', [ exname, str ] ) ) ;
      end ;
      _strm.WriteLine( GIS_DXF_EADE_MARKER ) ;
      _strm.WriteLine( GIS_DXF_NEND        ) ;
    end ;

  begin
    assert( assigned( _shp ) ) ;
    assert( _shp.GetNumParts > 0 ) ;

    {$IFDEF OXYGENE}
      if not assigned( _shp.Params.Labels.Field ) then
        _shp.Params.Labels.Field := '' ;
    {$ENDIF}

    is_3D := _shp.Dimension in [ TGIS_DimensionType.XYZ..TGIS_DimensionType.XYZM ] ;
    has_dif_z := False ;
    zval := 0 ;
    // check if the line contains different heights
    if (_shp is TGIS_ShapeArc) or (_shp is TGIS_ShapePolygon) then begin
      for part_no := 0 to _shp.GetNumParts - 1 do begin
        for point_no := 0 to _shp.GetPartSize( part_no ) - 1 do begin
          if point_no = 0 then
            zval := _shp.GetPoint3D( part_no, point_no ).Z ;
          if zval <> _shp.GetPoint3D( part_no, point_no ).Z then
            has_dif_z := True ;
        end ;
      end ;
    end ;

    bpoly_mesh := GisMetadataAsBoolean( DXF_METADATA_EXPORTMESH, False ) and
                  has_dif_z ;

    _shp.Lock( TGIS_Lock.Projection ) ;
    try
      lname := Trim( VarToString( _shp.GetField( GIS_DXF_FLD_LAYER_NAME ) ) ) ;
      if IsStringEmpty( lname ) then begin
        lname := Trim( GetFileNameNoExt( GetFileName( _shp.Layer.Name ) ) ) ;
        if ( FLayers.Count > 0 ) and not FLayers.ContainsKey( lname ) then
          lname := '0' ;
      end;

      if      _shp is TGIS_ShapePoint then
              begin
                if not IsStringEmpty(
                         VarToString( _shp.GetField( GIS_DXF_FLD_LABEL ) )
                       ) then begin
                  write_text  ;
                  write_layer ;
                end
                else if not IsStringEmpty( _shp.Params.Labels.Field ) then begin
                  write_text( True )  ;
                  write_layer ;
                end
                else begin
                  _strm.WriteLine( GIS_DXF_EPOINT ) ;
                  _strm.WriteLine( GIS_DXF_NPOINT ) ;
                  write_layer ;

                  ptg := _shp.GetPoint3D( 0, 0 ) ;
                  _strm.WriteLine( GIS_DXF_E10            ) ;
                  _strm.WriteLine( DotFloatToStr( ptg.X ) ) ;
                  _strm.WriteLine( GIS_DXF_E20            ) ;
                  _strm.WriteLine( DotFloatToStr( ptg.Y ) ) ;
                  _strm.WriteLine( GIS_DXF_E30            ) ;
                  _strm.WriteLine( DotFloatToStr( ptg.Z ) ) ;
                end ;
                write_color( _shp.Params.Marker.Color,
                             VarToString(
                               _shp.GetField( GIS_DXF_FLD_COLOR )
                             )
                           ) ;
                write_ade ;
              end
      else if _shp is TGIS_ShapeMultiPoint then
              begin
                for point_no := 0 to _shp.GetPartSize( 0 ) - 1 do begin
                  _strm.WriteLine( GIS_DXF_EPOINT ) ;
                  _strm.WriteLine( GIS_DXF_NPOINT ) ;
                  write_layer ;
                  write_color( _shp.Params.Marker.Color,
                               VarToString(
                                 _shp.GetField( GIS_DXF_FLD_COLOR )
                               )
                             )  ;

                  ptg := _shp.GetPoint3D( 0, point_no ) ;
                  _strm.WriteLine( GIS_DXF_E10            ) ;
                  _strm.WriteLine( DotFloatToStr( ptg.X ) ) ;
                  _strm.WriteLine( GIS_DXF_E20            ) ;
                  _strm.WriteLine( DotFloatToStr( ptg.Y ) ) ;
                  _strm.WriteLine( GIS_DXF_E30            ) ;
                  _strm.WriteLine( DotFloatToStr( ptg.Z ) ) ;
                  write_ade ;
                end ;
              end
      else if _shp is TGIS_ShapeArc then
              begin
                for part_no := 0 to _shp.GetNumParts - 1 do begin
                  _strm.WriteLine( GIS_DXF_EPOLYLINE      ) ;
                  _strm.WriteLine( GIS_DXF_NPOLYLINE      ) ;
                  write_layer ;
                  write_color( _shp.Params.Line.Color,
                               VarToString(
                                 _shp.GetField( GIS_DXF_FLD_COLOR )
                               )
                             ) ;

                  _strm.WriteLine( GIS_DXF_EVERTEXFOLLOW  ) ;
                  _strm.WriteLine( GIS_DXF_NVERTEXFOLLOW  ) ;
                  _strm.WriteLine( GIS_DXF_E10            ) ;
                  _strm.WriteLine( DotFloatToStr( 0     ) ) ;
                  _strm.WriteLine( GIS_DXF_E20            ) ;
                  _strm.WriteLine( DotFloatToStr( 0     ) ) ;
                  _strm.WriteLine( GIS_DXF_E30            ) ;
                  _strm.WriteLine( DotFloatToStr( 0     ) ) ;
                  _strm.WriteLine( GIS_DXF_EVERTEXATTR70  ) ;
                    if has_dif_z then
                      _strm.WriteLine( ' 8' )
                    else
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR0  ) ;

                  write_ade ;

                  for point_no := 0 to _shp.GetPartSize( part_no ) - 1 do
                  begin
                    _strm.WriteLine( GIS_DXF_EVERTEX       ) ;
                    _strm.WriteLine( GIS_DXF_NVERTEX       ) ;
                    write_layer ;
                    _strm.WriteLine( GIS_DXF_EVERTEXATTR70 ) ;
                    if has_dif_z then
                      _strm.WriteLine( ' 32' )
                    else
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR0  ) ;

                    ptg := _shp.GetPoint3D( part_no, point_no ) ;
                    _strm.WriteLine( GIS_DXF_E10            ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.X ) ) ;
                    _strm.WriteLine( GIS_DXF_E20            ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Y ) ) ;
                    _strm.WriteLine( GIS_DXF_E30            ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Z ) ) ;
                  end ;
                  _strm.WriteLine( GIS_DXF_ESEQEND ) ;
                  _strm.WriteLine( GIS_DXF_NSEQEND ) ;
                end ;
              end
      else if (_shp is TGIS_ShapePolygon) or (_shp is TGIS_ShapeMultiPatch) then
              begin
                if bpoly_mesh then begin

                  _strm.WriteLine( GIS_DXF_EPOLYLINE       ) ;
                  _strm.WriteLine( GIS_DXF_NPOLYLINE       ) ;
                  write_layer ;
                  write_color( _shp.Params.Area.OutlineColor,
                               VarToString(
                                 _shp.GetField( GIS_DXF_FLD_COLOR )
                               )
                             ) ;

                  _strm.WriteLine( GIS_DXF_EVERTEXFOLLOW   ) ;
                  _strm.WriteLine( GIS_DXF_NVERTEXFOLLOW   ) ;
                  _strm.WriteLine( GIS_DXF_E10             ) ;
                  _strm.WriteLine( DotFloatToStr( 0      ) ) ;
                  _strm.WriteLine( GIS_DXF_E20             ) ;
                  _strm.WriteLine( DotFloatToStr( 0      ) ) ;
                  _strm.WriteLine( GIS_DXF_E30             ) ;
                  _strm.WriteLine( DotFloatToStr( 0      ) ) ;
                  _strm.WriteLine( GIS_DXF_EVERTEXATTR70   ) ;
                  _strm.WriteLine( ' 64' ) ;
                  write_ade ;

                  mb := TGIS_MultiPatchBuilder.Create ;
                  try
                    mb.BuildMesh( _shp ) ;

                    for vv := 0 to mb.VertexesCount-1 do begin
                      mb.GetVertex( vv, v ) ;

                      _strm.WriteLine( GIS_DXF_EVERTEX       ) ;
                      _strm.WriteLine( GIS_DXF_NVERTEX       ) ;
                      write_layer ;
                      _strm.WriteLine( GIS_DXF_E10           ) ;
                      _strm.WriteLine( DotFloatToStr( v.X )  ) ;
                      _strm.WriteLine( GIS_DXF_E20           ) ;
                      _strm.WriteLine( DotFloatToStr( v.Y )  ) ;
                      _strm.WriteLine( GIS_DXF_E30           ) ;
                      _strm.WriteLine( DotFloatToStr( v.Z )  ) ;
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR70 ) ;
                      _strm.WriteLine( ' 192' ) ;
                    end ;

                    for vv := 0 to mb.FacesCount-1 do begin
                      mb.GetFace( vv, f1, f2, f3 ) ;

                      _strm.WriteLine( GIS_DXF_EVERTEX       ) ;
                      _strm.WriteLine( GIS_DXF_NVERTEX       ) ;
                      write_layer ;
                      _strm.WriteLine( GIS_DXF_E10           ) ;
                      _strm.WriteLine( DotFloatToStr( 0 )    ) ;
                      _strm.WriteLine( GIS_DXF_E20           ) ;
                      _strm.WriteLine( DotFloatToStr( 0 )    ) ;
                      _strm.WriteLine( GIS_DXF_E30           ) ;
                      _strm.WriteLine( DotFloatToStr( 0 )    ) ;
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR70 ) ;
                      _strm.WriteLine( ' 128' ) ;
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR71 ) ;
                      _strm.WriteLine( IntToStr( f1 )        ) ;
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR72 ) ;
                      _strm.WriteLine( IntToStr( f2 )        ) ;
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR73 ) ;
                      _strm.WriteLine( IntToStr( f3 )        ) ;
                    end ;
                    _strm.WriteLine( GIS_DXF_ESEQEND ) ;
                    _strm.WriteLine( GIS_DXF_NSEQEND ) ;

                  finally
                    FreeObject( mb ) ;
                  end ;
                  if not IsStringEmpty(
                           VarToString( _shp.GetField( _shp.Params.Labels.Field ) )
                         ) then
                    write_text( True ) ;
                    write_layer ;
                end
                else begin
                  if is_3D and ( _shp.GetNumParts = 1 ) and
                     ( (_shp.GetPartSize(0)>=3) and (_shp.GetPartSize(0)<=5) )
                  then begin
                    part_size := _shp.GetPartSize( 0 ) ;

                    _strm.WriteLine( GIS_DXF_E3DFACE ) ;
                    _strm.WriteLine( GIS_DXF_N3DFACE ) ;
                    write_layer ;
                    write_color( _shp.Params.Area.OutlineColor,
                                 VarToString(
                                   _shp.GetField( GIS_DXF_FLD_COLOR )
                                 )
                               ) ;
                    ptg := _shp.GetPoint3D( 0, 0 ) ;
                    _strm.WriteLine( GIS_DXF_E10             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.X )  ) ;
                    _strm.WriteLine( GIS_DXF_E20             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Y )  ) ;
                    _strm.WriteLine( GIS_DXF_E30             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Z )  ) ;

                    ptg := _shp.GetPoint3D( 0, 1 ) ;
                    _strm.WriteLine( GIS_DXF_E11             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.X )  ) ;
                    _strm.WriteLine( GIS_DXF_E21             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Y )  ) ;
                    _strm.WriteLine( GIS_DXF_E31             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Z )  ) ;

                    ptg := _shp.GetPoint3D( 0, 2 ) ;
                    _strm.WriteLine( GIS_DXF_E12             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.X )  ) ;
                    _strm.WriteLine( GIS_DXF_E22             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Y )  ) ;
                    _strm.WriteLine( GIS_DXF_E32             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Z )  ) ;

                    if part_size > 3 then begin
                      ptg := _shp.GetPoint3D( 0, 3 ) ;
                      if GisIsSamePoint3D( ptg, _shp.GetPoint3D( 0, 0 ) ) then
                        ptg := _shp.GetPoint3D( 0, 2 ) ;
                    end ;

                    _strm.WriteLine( GIS_DXF_E13             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.X )  ) ;
                    _strm.WriteLine( GIS_DXF_E23             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Y )  ) ;
                    _strm.WriteLine( GIS_DXF_E33             ) ;
                    _strm.WriteLine( DotFloatToStr( ptg.Z )  ) ;

                    _strm.WriteLine( GIS_DXF_EVERTEXATTR70 ) ;
                    _strm.WriteLine( GIS_DXF_EVERTEXATTR0  ) ;
                    write_ade ;
                  end
                  else begin
                    for part_no := 0 to _shp.GetNumParts - 1 do begin
                      _strm.WriteLine( GIS_DXF_EPOLYLINE       ) ;
                      _strm.WriteLine( GIS_DXF_NPOLYLINE       ) ;
                      write_layer ;
                      write_color( _shp.Params.Area.OutlineColor,
                                   VarToString(
                                     _shp.GetField( GIS_DXF_FLD_COLOR )
                                   )
                                 ) ;

                      _strm.WriteLine( GIS_DXF_EVERTEXFOLLOW   ) ;
                      _strm.WriteLine( GIS_DXF_NVERTEXFOLLOW   ) ;
                      _strm.WriteLine( GIS_DXF_E10             ) ;
                      _strm.WriteLine( DotFloatToStr( 0      ) ) ;
                      _strm.WriteLine( GIS_DXF_E20             ) ;
                      _strm.WriteLine( DotFloatToStr( 0      ) ) ;
                      _strm.WriteLine( GIS_DXF_E30             ) ;
                      _strm.WriteLine( DotFloatToStr( 0      ) ) ;
                      _strm.WriteLine( GIS_DXF_EVERTEXATTR70   ) ;
                        if has_dif_z then
                          _strm.WriteLine( ' 9' )
                        else
                          _strm.WriteLine( GIS_DXF_EVERTEXATTR1  ) ;
                      write_ade ;

                      for point_no := 0 to _shp.GetPartSize( part_no ) - 1 do
                      begin
                        _strm.WriteLine( GIS_DXF_EVERTEX       ) ;
                        _strm.WriteLine( GIS_DXF_NVERTEX       ) ;
                        write_layer ;
                        _strm.WriteLine( GIS_DXF_EVERTEXATTR70 ) ;
                        if has_dif_z then
                          _strm.WriteLine( ' 32' )
                        else
                          _strm.WriteLine( GIS_DXF_EVERTEXATTR1  ) ;

                        ptg := _shp.GetPoint3D( part_no, point_no ) ;
                        _strm.WriteLine( GIS_DXF_E10             ) ;
                        _strm.WriteLine( DotFloatToStr( ptg.X )  ) ;
                        _strm.WriteLine( GIS_DXF_E20             ) ;
                        _strm.WriteLine( DotFloatToStr( ptg.Y )  ) ;
                        _strm.WriteLine( GIS_DXF_E30             ) ;
                        _strm.WriteLine( DotFloatToStr( ptg.Z )  ) ;
                      end ;
                      _strm.WriteLine( GIS_DXF_ESEQEND ) ;
                      _strm.WriteLine( GIS_DXF_NSEQEND ) ;
                    end ;
                  end ;
                  if not IsStringEmpty(
                           VarToString( _shp.GetField( _shp.Params.Labels.Field ) )
                         ) then
                    write_text( True ) ;
                    write_layer ;
                end ;
              end;
    finally
      _shp.Unlock ;
    end ;
  end ;

  procedure TGIS_LayerDXF.setUp ;
  var
    found : Boolean ;
    i     : Integer ;
    lname : String ;
  begin
    inherited ;

    FSupportedShapes := GisGetEmptyShapeType ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Point ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Arc ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.Polygon ) ;
    FSupportedShapes := GisAddShapeType( FSupportedShapes, TGIS_ShapeType.MultiPatch ) ;
    Params.Ground    := TGIS_3DGroundType.AboveZero ;

    prepareLists ;

    if not IsStringEmpty( Path ) then begin
      prepareLayerFields( self );

      dxfFile := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;
      dxfFile.CodePage    := CodePage    ;

      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        lineNo      := 0 ;
        maxLineNo   := 0 ;
        maxPos      := 0 ;
        dxfEof      := dxfFile.Position >= dxfFile.Size ;
        lineName    := '' ;
        isBinaryDXF := False ;
        insertName  := '0' ;
        lastColor   := 0 ;
        {$IFDEF GIS_NORECORDS}
          insert := new TGIS_Point3D ;
        {$ENDIF}
        arTransform       := mtrx_fill ;
        arTransformInsert := mtrx_fill ;

        extMinMax := GisNoWorld3D ;

        if dxfFile.ReadLine = GIS_DXF_BINARY then begin
          isBinaryDXF := True ;
          dxfFile.Position := 32 ;
        end
        else
          dxfFile.Position := 0 ;

        dxfFile.CodePage := FCodePageForced ;

        if FCodePageForced = 0 then begin
          dxfFile.FixCodePage := True ;
        end
        else begin
          dxfFile.FixCodePage := False ;
          bForceCodePage := True ;
        end;


        try
          found := False ;
          while ( not dxfEof ) and
                ( not dxfTestLine( GIS_DXF_CEOF, GIS_DXF_NEOF ) ) do
          begin
            if      dxfTestLine( GIS_DXF_CACADVER    , GIS_DXF_NACADVER    ) then
                    doVersion
            else if dxfTestLine( GIS_DXF_CDWGCODEPAGE, GIS_DXF_NDWGCODEPAGE) then
                    doCodePage
            else if dxfTestLine( GIS_DXF_CTABLES     , GIS_DXF_NTABLES     ) then
                    doTables
            else if dxfTestLine( GIS_DXF_CBLOCK      , GIS_DXF_NBLOCK      ) then
                    doBlock
            else if dxfTestLine( GIS_DXF_CEXTMIN     , GIS_DXF_NEXTMIN     ) then
                    doExtMin
            else if dxfTestLine( GIS_DXF_CEXTMAX     , GIS_DXF_NEXTMAX     ) then
                    doExtMax
            else if dxfTestLine( GIS_DXF_CENTITIES   , GIS_DXF_NENTITIES   ) then
                    begin
                      found := True ;
                      break ;
                    end ;
            dxfFetchLine ;
          end ;

          if FLayers.Count = 0 then
            FLayers.AddOrSetValue( '0', T_DxfLayer.CreateDefault('0') ) ; // add default label for inserts

          prepareSubLayers ;

          if found then begin
            dxfFetchLine ;
            while ( not dxfEof ) and
                  ( not dxfTestLine( GIS_DXF_CEOF, GIS_DXF_NEOF ) ) do
            begin
              if      dxfTestLine( GIS_DXF_CPOINT     , GIS_DXF_NPOINT      ) then
                      doPoint
              else if dxfTestLine( GIS_DXF_CTEXT      , GIS_DXF_NTEXT       ) then
                      doText
              else if dxfTestLine( GIS_DXF_CTEXT      , GIS_DXF_NMTEXT      ) then
                      doText
              else if dxfTestLine( GIS_DXF_CTEXT      , GIS_DXF_NATTRIB     ) then
                      doText
              else if dxfTestLine( GIS_DXF_CINSERT    , GIS_DXF_NINSERT     ) then
                      doInsert
              else if dxfTestLine( GIS_DXF_CSOLID     , GIS_DXF_NSOLID      ) then
                      doSolid
              else if dxfTestLine( GIS_DXF_C3DFACE    , GIS_DXF_N3DFACE     ) then
                      do3DFace
              else if dxfTestLine( GIS_DXF_CSPLINE    , GIS_DXF_NSPLINE     ) then
                      doSpline
              else if dxfTestLine( GIS_DXF_CLINE      , GIS_DXF_NLINE       ) then
                      doLine
              else if dxfTestLine( GIS_DXF_CMLINE     , GIS_DXF_NMLINE      ) then
                      doMLine
              else if dxfTestLine( GIS_DXF_CARC       , GIS_DXF_NARC        ) then
                      doArc
              else if dxfTestLine( GIS_DXF_CELLIPSE   , GIS_DXF_NELLIPSE    ) then
                      doEllipse
              else if dxfTestLine( GIS_DXF_CCIRCLE    , GIS_DXF_NCIRCLE     ) then
                      doCircle
              else if dxfTestLine( GIS_DXF_CPOLYLINE  , GIS_DXF_NPOLYLINE   ) then
                      doPolyline
              else if dxfTestLine( GIS_DXF_CLWPOLYLINE, GIS_DXF_NLWPOLYLINE ) then
                      doLwPolyline
              else if dxfTestLine( GIS_DXF_CLWPOLYLINE, GIS_DXF_NMPOLYGON   ) then
                      doMPolygon
              else if dxfTestLine( GIS_DXF_CLWPOLYLINE, GIS_DXF_NHATCH      ) then
                      doHatch
              else dxfFetchLine ;

            end ;
          end ;

          sortSubLayers ;

          if assigned( SubLayers ) then begin
            for i := 0 to SubLayers.Count - 1 do begin
              Extent := GisMaxExtent( TGIS_LayerVector( SubLayers[ i ] ).Extent,
                                      Extent
                                     ) ;
              MergeStructure( TGIS_LayerVector( SubLayers[ i ] ), False ) ;
            end ;
          end ;

          if FForceExtent then
            if not GisIsNoWorld3D( extMinMax ) then begin
              if GisIsCommonExtent( Extent,
                                    GisExtent( extMinMax.XMin, extMinMax.YMin,
                                               extMinMax.XMax, extMinMax.YMax
                                           )
                                   ) then
                Extent := GisCommonExtent( Extent,
                                           GisExtent( extMinMax.XMin, extMinMax.YMin,
                                                      extMinMax.XMax, extMinMax.YMax
                                                     )
                                           ) ;
            // eliminate shapes outside the extent
              for i := Items.Count -1 downto 0 do begin
                if not GisIsCommonExtent( TGIS_Shape( Items[i] ).Extent, Extent )
                then
                  Items.Delete( i ) ;
              end ;
            end ;

          // if there is no layers definition in table section,
          // add them from attributes
          if FLayers.Count = 0 then
            for i := 0 to Items.Count -1 do begin
              lname := VarToString( TGIS_Shape(Items[i]).GetField(GIS_DXF_FLD_LAYER_NAME) ) ;
              FLayers.AddOrSetValue( lname, T_DxfLayer.CreateDefault( lname ) ) ;
            end ;
        except
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo ) ;
        end ;
      finally
        Unlock ;
        ClearModified ;

        RaiseBusyRelease( Self ) ;

        CodePage := dxfFile.CodePage ;
        FreeObject( dxfFile        ) ;
        FreeObject( lstBlock       ) ;
      end ;
    end ;

    SetCSByEPSG(0) ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'AutoCAD Export ('+iCadVersion+' DXF)' ;
  end ;

  function TGIS_LayerDXF.fget_Layers :
    {$IFDEF OXYGENE}
      TGIS_StringList ;
    {$ELSE}
      TStringList ;
    {$ENDIF}
  {$IFNDEF OXYGENE}
    var
      itm : TPair<String,TObject> ;
  {$ENDIF}
  begin
    Result := TStringList.Create ;
    for itm in FLayers do
      Result.Add( itm.Key ) ;
  end ;

  procedure TGIS_LayerDXF.Build(
    const _path   : String ;
    const _extent : TGIS_Extent ;
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

  procedure TGIS_LayerDXF.ImportLayerEx(
    const _layer       : TGIS_LayerVector ;
    const _extent      : TGIS_Extent      ;
    const _type        : TGIS_ShapeType   ;
    const _scope       : String           ;
    const _shape       : TGIS_Shape       ;
    const _de9im       : String           ;
    const _truncated   : Boolean
  ) ;
  var
    shape_file : TGIS_BufferedStream ;
    {$IFNDEF OXYGENE}
      shp      : TGIS_Shape  ;
    {$ENDIF}
    scodepage  : String      ;
    shp_tmp    : TGIS_Shape  ;
    same_name  : Boolean     ;
    ex         : TGIS_Extent ;
    shape_no   : Cardinal    ;
    end_uid    : TGIS_Uid    ;
    abort      : Boolean     ;
    old_scope  : String      ;
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

      ex := GisCommonExtent( _layer.Extent, _extent ) ;

      // prepare temporary geometry
      try
        shape_file := TGIS_BufferedFileStream.Create(
                        GetTemporaryName( Path ), TGIS_StreamMode.&Create
                      ) ;
        shape_file.CodePage    := CodePage    ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                     GetTemporaryName( Path ),
                                     GetLastError
                                   ) ;
      end ;

      if CodePage = 65001 then
        CodePage := GisSystemCodePage ;
      case CodePage of
        874   : scodepage := 'ANSI_874'  ;
        932   : scodepage := 'ANSI_932'  ;
        936   : scodepage := 'ANSI_936'  ;
        949   : scodepage := 'ANSI_949'  ;
        950   : scodepage := 'ANSI_950'  ;
        1250  : scodepage := 'ANSI_1250' ;
        1251  : scodepage := 'ANSI_1251' ;
        1252  : scodepage := 'ANSI_1252' ;
        1253  : scodepage := 'ANSI_1253' ;
        1254  : scodepage := 'ANSI_1254' ;
        1255  : scodepage := 'ANSI_1255' ;
        1256  : scodepage := 'ANSI_1256' ;
        1257  : scodepage := 'ANSI_1257' ;
        1258  : scodepage := 'ANSI_1258' ;
        else    scodepage := '' ;
      end ;

      // write header
      shape_file.WriteLine( GIS_DXF_ESECTION  ) ;
      shape_file.WriteLine( GIS_DXF_NSECTION  ) ;
        shape_file.WriteLine( GIS_DXF_EHEADER  ) ;
        shape_file.WriteLine( GIS_DXF_NHEADER  ) ;

        if scodepage <> '' then begin
          shape_file.WriteLine( GIS_DXF_EDWGCODEPAGE ) ;
          shape_file.WriteLine( GIS_DXF_NDWGCODEPAGE ) ;
          shape_file.WriteLine( GIS_DXF_ECODEPAGE ) ;
          shape_file.WriteLine( scodepage ) ;
        end;

          shape_file.WriteLine( GIS_DXF_EEXTMIN ) ;
          shape_file.WriteLine( GIS_DXF_NEXTMIN ) ;
            shape_file.WriteLine( GIS_DXF_E10              ) ;
            shape_file.WriteLine( DotFloatToStr( ex.XMin ) ) ;
            shape_file.WriteLine( GIS_DXF_E20              ) ;
            shape_file.WriteLine( DotFloatToStr( ex.YMin ) ) ;
          shape_file.WriteLine( GIS_DXF_EEXTMAX   ) ;
          shape_file.WriteLine( GIS_DXF_NEXTMAX   ) ;
            shape_file.WriteLine( GIS_DXF_E10              ) ;
            shape_file.WriteLine( DotFloatToStr( ex.XMax ) ) ;
            shape_file.WriteLine( GIS_DXF_E20              ) ;
            shape_file.WriteLine( DotFloatToStr( ex.YMax ) ) ;
      shape_file.WriteLine( GIS_DXF_EENDSEC ) ;
      shape_file.WriteLine( GIS_DXF_NENDSEC ) ;

      // write ADE section
      shape_file.WriteLine( GIS_DXF_ESECTION ) ;
      shape_file.WriteLine( GIS_DXF_NSECTION ) ;
        shape_file.WriteLine( GIS_DXF_ETABLES ) ;
        shape_file.WriteLine( GIS_DXF_NTABLES ) ;
          shape_file.WriteLine( GIS_DXF_ETABLE ) ;
          shape_file.WriteLine( GIS_DXF_NTABLE ) ;
            shape_file.WriteLine( GIS_DXF_EAPPID_MAIN  ) ;
            shape_file.WriteLine( GIS_DXF_NAPPID       ) ;
            shape_file.WriteLine( GIS_DXF_NLIMIT       ) ;
            shape_file.WriteLine( GIS_DXF_NAPPIDLIMIT  ) ;
            shape_file.WriteLine( GIS_DXF_NSEC         ) ;
            shape_file.WriteLine( GIS_DXF_NAPPID       ) ;
            shape_file.WriteLine( GIS_DXF_EADE_MAIN    ) ;
            shape_file.WriteLine( GIS_DXF_NADE         ) ;
            shape_file.WriteLine( GIS_DXF_NLIMIT       ) ;
            shape_file.WriteLine( GIS_DXF_NADELIMIT    ) ;
          shape_file.WriteLine( GIS_DXF_EENDTAB ) ;
          shape_file.WriteLine( GIS_DXF_NENDTAB ) ;

          // STYLE
          shape_file.WriteLine( GIS_DXF_ETABLE ) ;
          shape_file.WriteLine( GIS_DXF_NTABLE ) ;
            shape_file.WriteLine( '  2' ) ;
            shape_file.WriteLine( 'STYLE' ) ;
            shape_file.WriteLine( '  5' ) ;
            shape_file.WriteLine( '22' ) ;
            shape_file.WriteLine( '  0' ) ;
            shape_file.WriteLine( 'STYLE' ) ;
            shape_file.WriteLine( '  5' ) ;
            shape_file.WriteLine( '2' ) ;
            shape_file.WriteLine( '  2' ) ;
            shape_file.WriteLine( 'STANDARD' ) ;
            shape_file.WriteLine( ' 70' ) ;
            shape_file.WriteLine( '0' ) ;
            shape_file.WriteLine( ' 40' ) ;
            shape_file.WriteLine( '0' ) ;
            shape_file.WriteLine( ' 41' ) ;
            shape_file.WriteLine( '1' ) ;
            shape_file.WriteLine( ' 50' ) ;
            shape_file.WriteLine( '0' ) ;
            shape_file.WriteLine( ' 71' ) ;
            shape_file.WriteLine( '0' ) ;
            shape_file.WriteLine( ' 42' ) ;
            shape_file.WriteLine( '2.5' ) ;
            shape_file.WriteLine( '  3' ) ;
            if not IsStringEmpty( Params.Labels.FontName ) then
              shape_file.WriteLine( Params.Labels.FontName + '.ttf' )
            else
              shape_file.WriteLine('arial.ttf' ) ;
          shape_file.WriteLine( GIS_DXF_EENDTAB ) ;
          shape_file.WriteLine( GIS_DXF_NENDTAB ) ;
      shape_file.WriteLine( GIS_DXF_EENDSEC ) ;
      shape_file.WriteLine( GIS_DXF_NENDSEC ) ;

      // write ENTITIES section
      shape_file.WriteLine( GIS_DXF_ESECTION ) ;
      shape_file.WriteLine( GIS_DXF_NSECTION ) ;
        shape_file.WriteLine( GIS_DXF_EENTITIES ) ;
        shape_file.WriteLine( GIS_DXF_NENTITIES ) ;

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
              writeGeometry( shape_file, shp_tmp ) ;
            end ;
          finally
            if shp <> shp_tmp then FreeObject( shp_tmp ) ;
          end ;

          if shape_no mod 5000 = 1 then begin
            abort := RaiseBusyShake( _layer, shp.Uid, end_uid ) ;
            if abort then break ;
          end ;
          inc( shape_no ) ;
        end ;
      finally
        _layer.Scope := old_scope ;

        if abort then begin
           FreeObject( shape_file ) ;
           DeleteFile( GetTemporaryName( Path ) ) ;
        end
        else begin
           // write footer
           shape_file.WriteLine( GIS_DXF_EENDSEC ) ;
           shape_file.WriteLine( GIS_DXF_NENDSEC ) ;
           shape_file.WriteLine( GIS_DXF_EEOF    ) ;
           shape_file.WriteLine( GIS_DXF_NEOF    ) ;

           FreeObject( shape_file ) ;

           DeleteFile( GetBackupName( Path ) ) ;

           RenameFile( Path, GetBackupName( Path ) ) ;

           try
             if not RenameFile( GetTemporaryName( Path ), Path ) then
               raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), Path,
                                            GetLastError
                                          ) ;
           except
              // recover ;
              RenameFile( GetBackupName( Path ), Path ) ;
              raise ;
           end ;
        end ;

        if not IsOpened then begin
          Items.Clear ;
          Fields.Clear ;

          Open ;
        end ;
      end ;

    finally
      RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_LayerDXF.SaveData ;
  begin
    SaveFieldRules ;

    if MustSave then
      ImportLayer( self, GisWholeWorld, TGIS_ShapeType.Unknown, '', False ) ;

    inherited ;
  end ;


//==============================================================================
// Unit_GisLayerDXF
//==============================================================================

  class procedure Unit_GisLayerDXF.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-DXF', 'AutoCAD DXF', TGIS_LayerDXF, '.dxf',
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   True
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisLayerDXF.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

