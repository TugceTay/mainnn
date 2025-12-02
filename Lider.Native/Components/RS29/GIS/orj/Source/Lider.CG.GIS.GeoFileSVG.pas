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
  Scalable Vector Graphics (SVG) Tiny 1.2 Specification reader.

  Not supported yet : gradients, xlinks, defs.
}

{$IFDEF DCC}
  unit GisFileSVG ;
  {$HPPEMIT '#pragma link "GisFileSVG"'}
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

{$DEFINE SVG_TRANSPARENCY}

interface

{$IFDEF CLR}
  uses
    System.Text,
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisRtl,
    GisTypesUI,
    GisRendererAbstract ;
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
  ///   Class that can read SVG file.
  /// </summary>
  TGIS_FileSVG = class ( TGIS_ObjectDisposable )

    private
      oSvg : TObject ;
    protected

      function fget_ViewBox     : TRectF  ;
      function fget_CenterPoint : TPointF ;
      function fget_LabelBox    : TRectF  ;
      function fget_BoundryBox  : TRectF  ;

    protected

      /// <summary>
      ///   Free resources.
      /// </summary>
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Load a file.
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      /// <param name="_stream">
      ///   stream; used if path does not point to a file; to proper recognize
      ///   type of SVG enter into _path '.svg' or '.svgz'
      /// </param>
      procedure Load( const _path       : String  ;
                      const _stream     : TStream
                     ) ;

      /// <summary>
      ///   Draw the symbol on canvas.
      /// </summary>
      /// <param name="_canvas">
      ///   handle to canvas
      /// </param>
      /// <param name="_origin">
      ///   origin point
      /// </param>
      /// <param name="_offset">
      ///   offset from origin
      /// </param>
      /// <param name="_symSin">
      ///   sinus value for rotation
      /// </param>
      /// <param name="_symCos">
      ///   cosinus value for rotation
      /// </param>
      /// <param name="_symRot">
      ///   rotation value in radians
      /// </param>
      /// <param name="_symScale">
      ///   symbol scale
      /// </param>
      /// <param name="_symScaleX">
      ///   extra width scalling
      /// </param>
      /// <param name="_symColor1">
      ///   symbol brush color
      /// </param>
      /// <param name="_symColor2">
      ///   symbol pen color
      /// </param>
      procedure Draw( const _canvas     : TGIS_RendererAbstract ;
                      const _origin     : TPoint ;
                      const _offset     : TPoint ;
                      const _symSin     : Double ;
                      const _symCos     : Double ;
                      const _symRot     : Double ;
                      const _symScale   : Double ;
                      const _symScaleX  : Double ;
                      const _symColor1  : TGIS_Color ;
                      const _symColor2  : TGIS_Color
                     ) ; overload ;

      /// <summary>
      ///   Draw the symbol on canvas.
      /// </summary>
      /// <param name="_canvas">
      ///   handle to canvas
      /// </param>
      /// <param name="_origin">
      ///   origin point
      /// </param>
      /// <param name="_offset">
      ///   offset from origin
      /// </param>
      /// <param name="_symSin">
      ///   sinus value for rotation
      /// </param>
      /// <param name="_symCos">
      ///   cosinus value for rotation
      /// </param>
      /// <param name="_symRot">
      ///   rotation value in radians
      /// </param>
      /// <param name="_symScale">
      ///   symbol scale
      /// </param>
      /// <param name="_symScaleX">
      ///   extra width scalling
      /// </param>
      /// <param name="_symColor1">
      ///   symbol brush color
      /// </param>
      /// <param name="_symColor2">
      ///   symbol pen color
      /// </param>
      procedure Draw( const _canvas     : TGIS_RendererAbstract ;
                      const _origin     : TPointF ;
                      const _offset     : TPointF ;
                      const _symSin     : Double  ;
                      const _symCos     : Double  ;
                      const _symRot     : Double  ;
                      const _symScale   : Double  ;
                      const _symScaleX  : Double  ;
                      const _symColor1  : TGIS_Color ;
                      const _symColor2  : TGIS_Color
                     ) ; overload ;
    public
      /// <summary>
      ///   SVG symbol drawing area as specified by viewbox attribute
      /// </summary>
      property SVGViewBox : TRectF read fget_ViewBox ;

      /// <summary>
      ///   SVG symbol center point (rotation point).
      /// </summary>
      property SVGCenterPoint : TPointF read fget_CenterPoint ;

      /// <summary>
      ///   SVG label box for shields.
      /// </summary>
      property SVGLabelBox : TRectF read fget_LabelBox ;

      /// <summary>
      ///   SVG boundry box for shields.
      /// </summary>
      property SVGBoundryBox : TRectF read fget_BoundryBox ;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
  {$IFDEF LEVEL_XE6_RTL}
    System.Math.Vectors,
  {$ENDIF}
    System.SysUtils,
    System.Math,
    System.Variants,
    System.ZLib,

    GisStreams,
    GisResource,
    GisInternals,
    GisTypes,
    GisClasses,
    GisXmlSax ;
{$ENDIF}

const
  SVG_ELEM_SVG              = 'svg' ;
  SVG_ELEM_RECT             = 'rect' ;
  SVG_ELEM_CIRCLE           = 'circle' ;
  SVG_ELEM_ELLIPSE          = 'ellipse' ;
  SVG_ELEM_LINE             = 'line' ;
  SVG_ELEM_POLYLINE         = 'polyline' ;
  SVG_ELEM_POLYGON          = 'polygon' ;
  SVG_ELEM_PATH             = 'path' ;
  SVG_ELEM_TEXT             = 'text' ;
  SVG_ELEM_TSPAN            = 'tspan' ;
  SVG_ELEM_G                = 'g' ;

  SVG_STYLE                 = 'style' ;
  SVG_STYLE_DISPLAY         = 'display' ;
  SVG_STYLE_VISIBILITY      = 'visibility' ;
  SVG_STYLE_VISIBLE         = 'visible' ;
  SVG_STYLE_FILL            = 'fill' ;
  SVG_STYLE_FILL_OPACITY    = 'fill-opacity' ;
  SVG_STYLE_STROKE          = 'stroke' ;
  SVG_STYLE_STROKE_WIDTH    = 'stroke-width' ;
  SVG_STYLE_STROKE_OPACITY  = 'stroke-opacity' ;
  SVG_STYLE_STROKE_LINECUP  = 'stroke-linecap' ;
  SVG_STYLE_STROKE_LINEJOIN = 'stroke-linejoin' ;
  SVG_STYLE_STROKE_DASH     = 'stroke-dasharray' ;
  SVG_STYLE_NONE            = 'none' ;
  SVG_STYLE_SQUARE          = 'square' ;
  SVG_STYLE_ROUND           = 'round' ;
  SVG_STYLE_FLAT            = 'flat' ;
  SVG_STYLE_MITER           = 'miter' ;
  SVG_STYLE_BEVEL           = 'bevel' ;
  SVG_STYLE_INHERIT         = 'inherit' ;
  SVG_STYLE_OPACITY         = 'opacity' ;

  SVG_FONT_SIZE             = 'font-size' ;
  SVG_FONT_FAMILY           = 'font-family' ;
  SVG_TEXT_ANCHOR           = 'text-anchor' ;
  SVG_FONT_WEIGHT           = 'font-weight' ;

  SVG_COLOR_HEX             = '#' ;
  SVG_COLOR_RGB             = 'rgb' ;

  SVG_TRANSFORM             = 'transform' ;
  SVG_TRANSFORM_TRANSLATE   = 'translate' ;
  SVG_TRANSFORM_MATRIX      = 'matrix' ;
  SVG_TRANSFORM_SCALE       = 'scale' ;
  SVG_TRANSFORM_ROTATE      = 'rotate' ;
  SVG_TRANSFORM_SKEWX       = 'skewX' ;
  SVG_TRANSFORM_SKEWY       = 'skewY' ;

  SVG_ELEM_WIDTH            = 'width' ;
  SVG_ELEM_HEIGHT           = 'height' ;
  SVG_ELEM_CENTER_X         = 'center-x' ;
  SVG_ELEM_CENTER_Y         = 'center-y' ;
  SVG_ELEM_VIEWBOX          = 'viewBox' ;

  SVG_ELEM_RECT_X           = 'x' ;
  SVG_ELEM_RECT_Y           = 'y' ;
  SVG_ELEM_RECT_WIDTH       = 'width' ;
  SVG_ELEM_RECT_HEIGHT      = 'height' ;
  SVG_ELEM_RECT_RX          = 'rx' ;
  SVG_ELEM_RECT_RY          = 'ry' ;

  SVG_ELEM_LINE_X1          = 'x1' ;
  SVG_ELEM_LINE_Y1          = 'y1' ;
  SVG_ELEM_LINE_X2          = 'x2' ;
  SVG_ELEM_LINE_Y2          = 'y2' ;

  SVG_ELEM_CIRCLE_CX        = 'cx' ;
  SVG_ELEM_CIRCLE_CY        = 'cy' ;
  SVG_ELEM_CIRCLE_R         = 'r'  ;

  SVG_ELEM_ELLIPSE_CX        = 'cx' ;
  SVG_ELEM_ELLIPSE_CY        = 'cy' ;
  SVG_ELEM_ELLIPSE_RX        = 'rx' ;
  SVG_ELEM_ELLIPSE_RY        = 'ry' ;

  SVG_ELEM_POLY_POINTS       = 'points' ;

  SVG_ELEM_TEXT_X            = 'x' ;
  SVG_ELEM_TEXT_Y            = 'y' ;
  SVG_ELEM_TEXT_ROTATE       = 'rotate' ;

  SVG_ELEM_PATH_D            = 'd' ;

  SVG_ELEM_DEFS              = 'defs' ;

  SVG_SHIELD_LABEL           = 'LABEL' ;
  SVG_SHIELD_BOUNDRY         = 'BOUNDRY' ;

  SVG_ENTRY_LINE            = 1 ;
  SVG_ENTRY_SURFACE         = 2 ;
  SVG_ENTRY_TEXT            = 3 ;
  SVG_ENTRY_MULTISURFACE    = 4 ;
  SVG_ENTRY_END             = 0 ;

  SVG_NUMERIC               = '0123456789+-.eE' ;
  {$IFDEF JAVA OR ISLAND}
    cNumericSet : TSysCharSet = ('0','1','2','3','4','5','6','7','8','9','-','+','.','e','E') ;
  {$ENDIF}

type

  // Record storing svg primitive attributes.

  T_SymbolSvgPrimitive = {$IFDEF OXYGENE} class {$ELSE} record {$ENDIF}
    Kind         : Integer        ;
    BrushColor   : Cardinal       ;
    BrushStyle   : TGIS_BrushStyle ;
    PenColor     : Cardinal       ;
    PenWidth     : Integer        ;
    PenStyle     : TGIS_PenStyle  ;
    LineCap      : TGIS_LineCap   ;
    LineJoin     : TGIS_LineJoin  ;
    Points       : Integer        ;
    Visible      : Byte           ;
    {$IFDEF OXYGENE}

      // Read object from a stream.
      function Read  ( {$IFDEF OXYGENE}
                         _stream : TGIS_BaseStream
                       {$ELSE}
                         _stream : TStream
                       {$ENDIF}
                     ) : Integer ;

      // Write object to a stream.
      function Write ( {$IFDEF OXYGENE}
                         _stream : TGIS_BaseStream
                       {$ELSE}
                         _stream : TStream
                       {$ENDIF}
                     ) : Integer ;
    {$ENDIF}

  end ;

//------------------------------------------------------------------------------

  // Class storing svg element attributes.
  T_SVGAttrib = class
    public
      matrix         : TGIS_DoubleArray ;
      fillColor      : TGIS_Color ;
      fillOpacity    : Double  ;
      strokeColor    : TGIS_Color ;
      strokeDash     : TGIS_PenStyle ;
      strokeOpacity  : Double  ;
      strokeWidth    : Double  ;
      strokeLineCap  : TGIS_LineCap  ;
      strokeLineJoin : TGIS_LineJoin ;
      opacity        : Double  ;
      hasFill        : Byte    ;
      hasStroke      : Byte    ;
      visible        : Byte    ;
      fontSize       : Double  ;
      fontFamily     : String  ;
      fontWeight     : String  ;
      textAnchor     : String ;
    public
      // Constructor.
      // _source handle
      constructor Create( const _source : T_SVGAttrib ) ;
  end ;

//------------------------------------------------------------------------------

  // Class implementing svg parser.
  T_SVGParser = class( TGIS_Object )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      svgAttr   : TStack< TObject >;
      {$IFDEF OXYGENE}
        svgMeta   : TObject    ;
      {$ELSE}
        svgMeta   : Pointer    ;
      {$ENDIF}
      nPoints     : Integer    ;
      nPartPoints : Integer    ;
      nParts      : Integer    ;
      scaleX      : Double     ;
      scaleY      : Double     ;
      setFirst    : Boolean    ;
      firstX      : Double     ;
      firstY      : Double     ;
      viewBox     : TRectF     ;
      labelBox    : TRectF     ;
      boundryBox  : TRectF     ;
      centerPoint : TPointF    ;
      {$IFDEF OXYGENE}
        points    : TGIS_MemoryStream ;
      {$ELSE}
        points    : TMemoryStream ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        parts     : TGIS_MemoryStream ;
      {$ELSE}
        parts     : TMemoryStream ;
      {$ENDIF}
      colors      : TDictionary<String,TGIS_Color> ;
      fonts       : TStringList ;
      tol         : Double ;
      defsFlag    : Integer ;
      textFlag    : Boolean ;
      {$IFDEF OXYGENE}
        text      : TGIS_MemoryStream ;
      {$ELSE}
        text      : TMemoryStream ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        metaStream : TGIS_MemoryStream ;
      {$ELSE}
        metaStream : TMemoryStream ;
      {$ENDIF}

    private
      // Read flag type.
      // _value value
      function  readFlag      ( const _value : String
                              ) : Byte ;

      // Read flag type.
      // _value value
      function  readVisible   ( const _value : String
                              ) : Byte ;

      // Read linecap type.
      // _value value
      function  readLinecap   ( const _value : String
                              ) : TGIS_LineCap ;

      // Read linejoin type.
      // _value value
      function  readLineJoin  ( const _value : String
                              ) : TGIS_LineJoin ;

      // Read color type.
      // _value value
      function  readColor     ( const _value : String
                              ) : TGIS_Color ;

      // Read double type.
      // _value value
      // return result
      function  readDouble    ( const _value : String
                              ) : Double ;

      // Read dash type.
      // _value value
      function  readDash     ( const _value : String
                              ) : TGIS_PenStyle ;

      // Read double type.
      // _value   input value
      // _params  parameter values
      // _count   number of parameters
      procedure readParams    ( const _value : String ;
                                  var _params: TGIS_DoubleArray ;
                                  var _count : Integer
                               ) ;
      function  distPtSeg     ( const _x, _y,
                                      _px, _py,
                                      _qx, _qy : Double
                               ) : Double ;
      procedure cubicBezRec   ( const _x1, _y1,
                                      _x2, _y2,
                                      _x3, _y3,
                                      _x4, _y4  : Double ;
                                const _level    : Integer
                               ) ;
      procedure quadBezRec    ( const _x1, _y1,
                                      _x2, _y2,
                                      _x3, _y3  : Double ;
                                const _level    : Integer
                               ) ;

      // Prepare an elliptic arc element.
      // _cx,_cy  center
      // _ax,_ay  first axis
      // _bx,_by  second axis
      // _sx,_sy  start point
      // _ex,_ey  end point
      procedure toEllipticArc ( const _cx,_cy,
                                      _ax,_ay,
                                      _bx,_by,
                                      _sx,_sy,
                                      _ex,_ey : Double
                                ) ;
      procedure toEllipticalArc(  var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toLine        (   var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toHLine       (   var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toVLine       (   var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toCubicBezier (   var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toCubicBezierS (  var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toQuadBezier  (   var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;
      procedure toQuadBezierS  (  var _cpx  : Double ;
                                  var _cpy  : Double ;
                                  var _cpx2 : Double ;
                                  var _cpy2 : Double ;
                                const _args : TGIS_DoubleArray ;
                                const _rel  : Boolean
                               ) ;

    protected
      procedure doDestroy      ; override;

    public
      constructor Create       ;

      // Draw svg.
      // _canvas  canvas handle
      procedure Draw           ( const _canvas    : TGIS_RendererAbstract ;
                                 const _origin    : TPointF ;
                                 const _offset    : TPointF ;
                                 const _symSin    : Double  ;
                                 const _symCos    : Double  ;
                                 const _symRot    : Double  ;
                                 const _symScale  : Double  ;
                                 const _symXScale : Double  ;
                                 const _symColor1 : TGIS_Color ;
                                 const _symColor2 : TGIS_Color
                                ) ;

      procedure svgInitColor   ;
      procedure svgPushAttr    ;
      procedure svgPopAttr     ;

      procedure svgPathPrepare ;
      procedure svgPathAddPart ;
      procedure svgPathClose ;

      // Add point to a path.
      // _x value
      // _y value
      procedure svgPathAddPoint( const _x       : Double ;
                                 const _y       : Double
                                ) ;

      // Create path to draw.
      // _close if True, shape will be closed
      procedure svgPathCreate  ( const _kind    : Integer
                               ) ;
      procedure svgPathEnd      ;

      // Parse svg attributes.
      // _attribs list of attributes
      procedure svgParseAttribs( const _attribs : IVBSAXAttributes
                                ) ;

      // Parse svg style.
      // _style value
      procedure svgParseStyle  ( const _style   : String
                               ) ;

      // Parse svg attribute.
      // _name  attr name
      // _value attr value
      procedure svgParseAttr   ( const _name    : String ;
                                 const _value   : String
                               ) ;

      // Parse svg transform.
      // _value value
      procedure svgParseTrans  ( const _value   : String
                               ) ;


      // Parse matrix.
      //  _value value
      procedure svgParseMatrix ( const _value   : String
                               ) ;

      // Apply matrix.
      // _params parameters
      procedure svgApplyMatrix ( const _params  : TGIS_DoubleArray
                               ) ;

      // Multiply matrix.
      // _matrix parameters
      // _params parameters
      procedure svgMatrixMultiply( const _matrix : TGIS_DoubleArray ;
                                   const _params  : TGIS_DoubleArray
                                ) ;

      // Parse translate.
      // _value value
      procedure svgParseTranslate( const _value   : String
                                 ) ;

      // Apply translate.
      // _tx value
      // _ty value
      procedure svgApplyTranslate( const _tx      : Double ;
                                   const _ty      : Double
                                ) ;

      // Parse scale.
      // _value value
      procedure svgParseScale   ( const _value   : String
                                ) ;

      // Apply scale.
      // _sx value
      // _sy value
      procedure svgApplyScale  ( const _sx      : Double ;
                                 const _sy      : Double
                               ) ;

      // Parse rotate.
      // _value value
      procedure svgParseRotate ( const _value   : String
                               ) ;

      // Apply rotate.
      // _angle value
      procedure svgApplyRotate ( const _angle   : Double
                               ) ;

      // Apply skewX.
      // _angle value
      procedure svgApplySkewX  ( const _angle   : Double
                               ) ;

      // Apply skewY.
      // _angle value
      procedure svgApplySkewY  ( const _angle   : Double
                               ) ;

      // Parse svg element.
      // _attribs attributes
      procedure svgParseSvg    ( const _attribs : IVBSAXAttributes
                               ) ;

     // Parse svg rect element.
     // _attribs attributes
     procedure svgParseRect    ( const _attribs : IVBSAXAttributes
                               ) ;

     // Parse svg line element.
     // _attribs attributes
     procedure svgParseLine    ( const _attribs : IVBSAXAttributes
                               ) ;

     // Parse svg circle element.
     // _attribs attributes
     procedure svgParseCircle ( const _attribs : IVBSAXAttributes
                                ) ;

      // Parse svg ellipse element.
      // _attribs attributes
      procedure svgParseEllipse( const _attribs : IVBSAXAttributes
                                ) ;

      // Parse svg polyline element.
      // _attribs attributes
      procedure svgParsePolyline( const _attribs : IVBSAXAttributes
                                ) ;

      // Parse svg polygon element.
      // _attribs attributes
      procedure svgParsePolygon( const _attribs : IVBSAXAttributes
                                ) ;

      // Parse svg text element.
      // _attribs attributes
      procedure svgParseText   ( const _attribs : IVBSAXAttributes
                                ) ;

      // Parse svg path element.
      // _attribs attributes

      procedure svgParsePath   ( const _attribs : IVBSAXAttributes
                                ) ;
  end ;

//------------------------------------------------------------------------------

  // SAX handler.
  T_SAXHandlerSVG = class( TGIS_SAXContentHandler )
    private
      FSVG  : T_SVGParser ;
    public

      // Catches startElement event, gives element name and attributes.
      // _uri     element Namespace URI
      // _lname   element name
      // _qname   element QName
      // _attribs element attributes
      procedure StartElement ( const _uri     : String ;
                               const _lname   : String ;
                               const _qname   : String ;
                               const _attribs : IVBSAXAttributes
                             ) ; override;


      // Catches endElement event, gives element name.
      // _uri   element Namespace URI
      // _lname element name
      // _qname element QName
      procedure EndElement   ( const _uri     : String ;
                               const _lname   : String ;
                               const _qname   : String
                             ) ; override;

      // Characters - Value of element
      // _strchars  element value
      procedure Characters   ( const _chars   : String
                             ) ; override;
    public
      procedure FatalError   ( const _locator : IVBSAXLocator ;
                               const _message : String        ;
                               const _code    : HResult
                             ) ; override;
    public

      // Create an instance for provided svg parser.
      // _svg svg parser
      constructor Create     ( const _svg  : T_SVGParser
                             ) ;
  end ;

//==============================================================================
// Utility routines
//==============================================================================

{$IFDEF OXYGENE}

  function T_SymbolSvgPrimitive.Read(
    {$IFDEF OXYGENE}
      _stream : TGIS_BaseStream
    {$ELSE}
      _stream : TStream
    {$ENDIF}
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := _stream.ReadInteger( Kind ) ;
    Result := Result + _stream.ReadCardinal( BrushColor ) ;
    Result := Result + _stream.ReadInteger( i ) ;
    case i of
      0 : BrushStyle := TGIS_BrushStyle.Solid      ;
      1 : BrushStyle := TGIS_BrushStyle.Clear      ;
      2 : BrushStyle := TGIS_BrushStyle.Horizontal ;
      3 : BrushStyle := TGIS_BrushStyle.Vertical   ;
      4 : BrushStyle := TGIS_BrushStyle.FDiagonal  ;
      5 : BrushStyle := TGIS_BrushStyle.BDiagonal  ;
      6 : BrushStyle := TGIS_BrushStyle.Cross      ;
      7 : BrushStyle := TGIS_BrushStyle.DiagCross
    else  BrushStyle := TGIS_BrushStyle.Solid      ;
    end ;

    Result := Result + _stream.ReadCardinal( PenColor ) ;
    Result := Result + _stream.ReadInteger( PenWidth ) ;
    Result := Result + _stream.ReadInteger( i ) ;

    case i of
      0 :  PenStyle := TGIS_PenStyle.Solid       ;
      1 :  PenStyle := TGIS_PenStyle.Dash        ;
      2 :  PenStyle := TGIS_PenStyle.Dot         ;
      3 :  PenStyle := TGIS_PenStyle.DashDot     ;
      4 :  PenStyle := TGIS_PenStyle.DashDotDot  ;
      5 :  PenStyle := TGIS_PenStyle.Clear       ;
      else PenStyle := TGIS_PenStyle.Solid       ;
    end ;

    Result := Result + _stream.ReadInteger( i ) ;
    case i of
      0 :  LineCap := TGIS_LineCap.Flat   ;
      1 :  LineCap := TGIS_LineCap.Square ;
      2 :  LineCap := TGIS_LineCap.Round  ;
      else LineCap := TGIS_LineCap.Round  ;
    end ;

    Result := Result + _stream.ReadInteger( i ) ;
    case i of
      0 :  LineJoin := TGIS_LineJoin.Bevel ;
      1 :  LineJoin := TGIS_LineJoin.Miter ;
      2 :  LineJoin := TGIS_LineJoin.Round ;
      else LineJoin := TGIS_LineJoin.Round ;
    end ;

    Result := Result + _stream.ReadInteger( Points ) ;
    Result := Result + _stream.ReadByte( Visible ) ;
  end ;

  function T_SymbolSvgPrimitive.Write(
    {$IFDEF OXYGENE}
      _stream : TGIS_BaseStream
    {$ELSE}
      _stream : TStream
    {$ENDIF}
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := _stream.WriteInteger( Kind ) ;
    Result := Result + _stream.WriteCardinal( BrushColor ) ;
    case BrushStyle of
      TGIS_BrushStyle.Solid       : i := 0 ;
      TGIS_BrushStyle.Clear       : i := 1 ;
      TGIS_BrushStyle.Horizontal  : i := 2 ;
      TGIS_BrushStyle.Vertical    : i := 3 ;
      TGIS_BrushStyle.FDiagonal   : i := 4 ;
      TGIS_BrushStyle.BDiagonal   : i := 5 ;
      TGIS_BrushStyle.Cross       : i := 6 ;
      TGIS_BrushStyle.DiagCross   : i := 7
      else                          i := 0 ;
    end ;
    Result := Result + _stream.WriteInteger( i ) ;
    Result := Result + _stream.WriteCardinal( PenColor ) ;
    Result := Result + _stream.WriteInteger( PenWidth ) ;
    case PenStyle of
      TGIS_PenStyle.Solid         : i := 0 ;
      TGIS_PenStyle.Dash          : i := 1 ;
      TGIS_PenStyle.Dot           : i := 2 ;
      TGIS_PenStyle.DashDot       : i := 3 ;
      TGIS_PenStyle.DashDotDot    : i := 4 ;
      TGIS_PenStyle.Clear         : i := 5 ;
      else                          i := 0 ;
    end;
    Result := Result + _stream.WriteInteger( i ) ;
    case LineCap of
      TGIS_LineCap.Flat           : i := 0 ;
      TGIS_LineCap.Square         : i := 1 ;
      TGIS_LineCap.Round          : i := 2 ;
      else                          i := 2 ;
    end ;
    Result := Result + _stream.WriteInteger( i ) ;
    case LineJoin of
      TGIS_LineJoin.Bevel         : i := 0 ;
      TGIS_LineJoin.Miter         : i := 1 ;
      TGIS_LineJoin.Round         : i := 2 ;
      else                          i := 2 ;
    end ;
    Result := Result + _stream.WriteInteger( i ) ;
    Result := Result + _stream.WriteInteger( Points ) ;
    Result := Result + _stream.WriteByte( Visible ) ;
  end ;
{$ENDIF}

//==============================================================================
//  T_SVGAttrib
//==============================================================================

  constructor T_SVGAttrib.Create(
    const _source : T_SVGAttrib
  ) ;
  begin
    inherited Create ;

    if assigned( _source ) then begin
      matrix         := _source.matrix         ;
      fillColor      := _source.fillColor      ;
      fillOpacity    := _source.fillOpacity    ;
      strokeColor    := _source.strokeColor    ;
      strokeOpacity  := _source.strokeOpacity  ;
      strokeWidth    := _source.strokeWidth    ;
      strokeLineCap  := _source.strokeLineCap  ;
      strokeLineJoin := _source.strokeLineJoin ;
      strokeDash     := _source.strokeDash     ;
      opacity        := _source.opacity        ;
      hasFill        := _source.hasFill        ;
      hasStroke      := _source.hasStroke      ;
      visible        := _source.visible        ;
      fontSize       := _source.fontSize       ;
      fontFamily     := _source.fontFamily     ;
      fontWeight     := _source.fontWeight     ;
      textAnchor     := _source.textAnchor     ;
    end
    else begin
      SetLength( matrix, 6 ) ;
      matrix[0]      := 1.0              ;
      matrix[1]      := 0.0              ;
      matrix[2]      := 0.0              ;
      matrix[3]      := 1.0              ;
      matrix[4]      := 0.0              ;
      matrix[5]      := 0.0              ;
      fillColor      := TGIS_Color.Black ;
      fillOpacity    := 1                ;
      opacity        := 1                ;
      strokeColor    := TGIS_Color.None  ;
      strokeOpacity  := 1                ;
      strokeWidth    := 1                ;
      strokeLineCap  := TGIS_LineCap.Flat   ;
      strokeLineJoin := TGIS_LineJoin.Miter ;
      strokeDash     := TGIS_PenStyle.Solid ;
      hasFill        := 1                ;
      hasStroke      := 0                ;
      visible        := 1                ;
      fontSize       := 10               ;
      fontFamily     := 'Arial'          ;
      fontWeight     := 'normal'         ;
      textAnchor     := 'start'          ;
    end ;
  end;

//==============================================================================
//  T_SVGParser
//==============================================================================

  { Constructor.
  }
  constructor T_SVGParser.Create;
  begin
    inherited ;

    {$IFDEF OXYGENE}
      svgAttr  := TStack< TObject >.Create ;
    {$ELSE}
      svgAttr  := TStack< TObject >.Create ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      points   := TGIS_MemoryStream.Create ;
    {$ELSE}
      points   := TMemoryStream.Create ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      parts   := TGIS_MemoryStream.Create ;
    {$ELSE}
      parts   := TMemoryStream.Create ;
    {$ENDIF}
    colors     := TDictionary<String, TGIS_Color>.Create ;
    {$IFDEF OXYGENE}
      text     := TGIS_MemoryStream.Create ;
    {$ELSE}
      text     := TMemoryStream.Create ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      metaStream := TGIS_MemoryStream.Create ;
    {$ELSE}
      metaStream := TMemoryStream.Create ;
    {$ENDIF}
    tol      := 0.1 ;
    defsFlag := 0 ;
    textFlag := False ;
    svgInitColor ;
    fonts := TStringList.Create ;
    fonts.Add('Arial') ;

    {$IFDEF OXYGENE}
      svgMeta := T_SymbolSvgPrimitive.Create ;
    {$ELSE}
      GetMem( svgMeta, sizeOf( T_SymbolSvgPrimitive ) ) ;
    {$ENDIF}
  end ;

  procedure T_SVGParser.doDestroy;
  var
    obj : TObject ;
  begin
    while svgAttr.Count > 0 do begin
      obj := TObject( svgAttr.Pop ) ;
      FreeObject( obj ) ;
    end ;

    FreeObject( svgAttr    ) ;
    FreeObject( metaStream ) ;
    FreeObject( points     ) ;
    FreeObject( parts      ) ;
    FreeObject( colors     ) ;
    FreeObject( text       ) ;
    FreeObject( fonts      ) ;

    {$IFDEF OXYGENE}
      FreeObject( svgMeta ) ;
    {$ELSE}
      Dispose( svgMeta ) ;
    {$ENDIF}

    inherited ;
  end ;

  function T_SVGParser.readFlag(
    const _value : String
  ) : Byte ;
  begin
    if _value = SVG_STYLE_NONE then
      Result := 0
    else
      Result := 1 ;
  end ;

  function T_SVGParser.readVisible(
    const _value : String
  ) : Byte ;
  begin
    if _value = SVG_STYLE_DISPLAY then
      Result := 1
    else
      Result := 0 ;
  end ;

  function T_SVGParser.readLinecap(
    const _value : String
  ) : TGIS_LineCap ;
  begin
    if _value = SVG_STYLE_ROUND then
      Result := TGIS_LineCap.Round
    else
    if _value = SVG_STYLE_SQUARE then
      Result := TGIS_LineCap.Square
    else
    if _value = SVG_STYLE_FLAT then
      Result := TGIS_LineCap.Flat
    else
    if _value = SVG_STYLE_INHERIT then begin
      // do nothing
    end
    else
      Result := TGIS_LineCap.Round ;
  end;

  function T_SVGParser.readLineJoin(
    const _value : String
  ) : TGIS_LineJoin ;
  begin
    if _value = SVG_STYLE_ROUND then
      Result := TGIS_LineJoin.Round
    else
    if _value = SVG_STYLE_MITER then
      Result := TGIS_LineJoin.Miter
    else
    if _value = SVG_STYLE_BEVEL then
      Result := TGIS_LineJoin.Bevel
    else
    if _value = SVG_STYLE_INHERIT then begin
      // do nothing
    end
    else
      Result := TGIS_LineJoin.Round ;
  end;

  function T_SVGParser.readDash(
    const _value : String
  ) : TGIS_PenStyle ;
  begin
    // todo
    Result := TGIS_PenStyle.Solid ;
    if not IsStringEmpty( _value ) and
      ( _value <> 'none' ) and ( _value <> 'inherit' ) then
      Result := TGIS_PenStyle.Dash ;
  end ;

  function T_SVGParser.readColor(
    const _value : String
  ) : TGIS_Color ;
  var
    r,
    g,
    b   : Byte ;
    sr,
    sg,
    sb  : String ;
    tkn : TGIS_Tokenizer ;
    mlt : Integer ;
    cl  : {$IFDEF JAVA} nullable {$ENDIF} TGIS_Color ;
  begin
    Result := TGIS_Color.Black ;

    if IsStringEmpty( _value ) then exit ;

    if Pos( SVG_COLOR_HEX, _value ) = StringFirst then begin
      if length( _value ) = 4 then begin
        sr := Copy( _value, StringFirst+1, 1 ) ;
        sg := Copy( _value, StringFirst+2, 1 ) ;
        sb := Copy( _value, StringFirst+3, 1 ) ;
        mlt := 17 ;
      end
      else if length( _value ) = 7 then begin
        sr := Copy( _value, StringFirst+1, 2 ) ;
        sg := Copy( _value, StringFirst+3, 2 ) ;
        sb := Copy( _value, StringFirst+5, 2 ) ;
        mlt := 1 ;
      end
      else begin
        sr := '0' ;
        sg := '0' ;
        sb := '0' ;
        mlt := 1 ;
      end ;

      try
        r := StrToInt( '$' + sr )  * mlt ;
      except
        r := 0 ;
      end ;

      try
        g := StrToInt( '$' + sg )  * mlt ;
      except
        g := 0 ;
      end ;

      try
        b := StrToInt( '$' + sb )  * mlt ;
      except
        b := 0 ;
      end ;
      Result := TGIS_Color.FromRGB( r, g, b ) ;
   end
   else if Pos( SVG_COLOR_RGB, _value ) = StringFirst then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        try
          tkn.Execute( _value, [ ',','(',')' ] ) ;
          r  := StrToInt( tkn.Result[ 1 ] ) ;
          g  := StrToInt( tkn.Result[ 2 ] ) ;
          b  := StrToInt( tkn.Result[ 3 ] ) ;
          Result := TGIS_Color.FromRGB( r, g, b ) ;
        except
        end;
      finally
        FreeObject( tkn ) ;
      end;
   end
   else begin
     if colors.TryGetValue( _value, cl ) then
       Result := cl ;
   end;
  end ;

  function T_SVGParser.readDouble(
    const _value : String
  ) : Double ;
  var
    c,len : Integer ;
    val   : String ;
    un    : String ;
    dval  : Double  ;

    function t( const _txt : String ) : Boolean ;
    begin
      Result := CompareText( _txt, un ) = 0 ;
    end ;

  begin
    if IsStringEmpty( _value ) then
      Result := 0
    else begin
      c   := 0 ;
      len := length( _value ) ;
      {$IFDEF JAVA OR ISLAND}
        while (c<len) and CharInSet( _value[c+StringFirst], cNumericSet ) do
          inc( c ) ;
      {$ELSE}
        while (c<len) and CharInSet( _value[c+StringFirst], ['0'..'9','-','+','.','e','E'] ) do
          inc( c ) ;
      {$ENDIF}

      val := Copy( _value, StringFirst, c )  ;
      un  := Copy( _value, c+StringFirst, MaxInt )  ;

      try
        if not IsStringEmpty( val ) then
          dval := DotStrToFloat( val )
        else
          dval := 0 ;
      except
        dval := 0 ;
      end;

      if      t( 'cm' ) then Result := dval * 1440 / 2.54
      else if t( 'mm' ) then Result := dval * 1440 / 2.54 / 10
      else if t( 'in' ) then Result := dval * 1440
      else if t( 'pt' ) then Result := dval * 1440 / 72
      else                   Result := dval ;
    end ;
  end ;

  procedure T_SVGParser.readParams(
    const _value  : String ;
      var _params : TGIS_DoubleArray ;
      var _count  : Integer
   ) ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _value, [',',' '] ) ;

      _count := tkn.Result.Count ;
      SetLength( _params, _count ) ;

      for i := 0 to tkn.Result.Count-1 do
        _params[i] := DotStrToFloat( tkn.Result[i] ) ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  { Calculate distanse from point to segment.
  }
  function T_SVGParser.distPtSeg(
    const _x, _y, _px, _py, _qx, _qy : Double
  ) : Double ;
  var
    pqx, pqy, dx, dy, d, t : Double ;
  begin
    pqx := _qx-_px ;
    pqy := _qy-_py ;
    dx  := _x-_px ;
    dy  := _y-_py ;
    d   := pqx*pqx + pqy*pqy ;
    t   := pqx*dx + pqy*dy ;

    if (d > 0) then
      t := t/d ;
    if (t < 0) then
      t := 0
    else if (t > 1) then
      t := 1 ;

    dx := _px + t*pqx - _x ;
    dy := _py + t*pqy - _y ;

    Result := dx*dx + dy*dy ;
  end ;

  { Cubic bezier record.
  }
  procedure T_SVGParser.cubicBezRec(
   const _x1, _y1,
         _x2, _y2,
         _x3, _y3,
         _x4, _y4  : Double ;
   const _level    : Integer
  ) ;
  var
    x12,y12,x23,y23,x34,y34,x123,y123,x234,y234,x1234,y1234,d : Double ;
  begin
    if (_level > 12) then exit ;

    x12   := (_x1+_x2)*0.5;
    y12   := (_y1+_y2)*0.5;
    x23   := (_x2+_x3)*0.5;
    y23   := (_y2+_y3)*0.5;
    x34   := (_x3+_x4)*0.5;
    y34   := (_y3+_y4)*0.5;
    x123  := (x12+x23)*0.5;
    y123  := (y12+y23)*0.5;
    x234  := (x23+x34)*0.5;
    y234  := (y23+y34)*0.5;
    x1234 := (x123+x234)*0.5;
    y1234 := (y123+y234)*0.5;

    d := distPtSeg(x1234, y1234, _x1,_y1, _x4,_y4) ;
    if (_level > 0) and (d < tol*tol) then begin
      svgPathAddPoint(x1234, y1234) ;
      exit ;
    end ;

    cubicBezRec(_x1,_y1, x12,y12, x123,y123, x1234,y1234, _level+1) ;
    cubicBezRec(x1234,y1234, x234,y234, x34,y34, _x4,_y4, _level+1) ;
  end ;

  { Quad bezier record.
  }
  procedure T_SVGParser.quadBezRec(
    const _x1, _y1,
          _x2, _y2,
          _x3, _y3  : Double ;
    const _level    : Integer
   ) ;
   var
    x12,y12,x23,y23,x123,y123,d : Double ;
   begin
    if (_level > 12) then exit ;

    x12  := (_x1+_x2)*0.5;
    y12  := (_y1+_y2)*0.5;
    x23  := (_x2+_x3)*0.5;
    y23  := (_y2+_y3)*0.5;
    x123 := (x12+x23)*0.5;
    y123 := (y12+y23)*0.5;

    d := distPtSeg(x123, y123, _x1,_y1, _x3,_y3);
    if (_level > 0) and (d < tol*tol) then begin
      svgPathAddPoint(x123, y123) ;
      exit ;
    end ;

    quadBezRec(_x1,_y1, x12,y12, x123,y123, _level+1) ;
    quadBezRec(x123,y123, x23,y23, _x3,_y3, _level+1) ;
   end ;

  procedure T_SVGParser.toEllipticArc(
    const _cx,_cy,
          _ax,_ay,
          _bx,_by,
          _sx,_sy,
          _ex,_ey : Double
  ) ;
  const
    MAX_STEPS = 90 ;
  var
    xa,ya,
    xb,yb,
    xc,yc,
    xs,ys,
    xe,ye     : Double ;
    cnt       : Integer ;
    da,db     : Double  ;
    xn,yn     : Double ;
    start     : Double  ;
    stop      : Double  ;
    arcangle  : Double  ;
    step      : Double  ;
    steps     : Integer ;
    rotate    : Double  ;
    rotateSin : Double  ;
    rotateCos : Double  ;
    ssin, scos: Double  ;

    function fmod( a, b : Double ) : Double ;
    var
     f : Integer ;
    begin
      f := TruncS( a/b ) ;
      Result := a - (b*f) ;
    end ;

  begin
    // rotation
       rotate := ArcTan2( _ay - _cy, -_ax + _cx ) ;
       SinCos( rotate, rotateSin, rotateCos ) ;

       if rotate = 0 then begin
          xc := _cx ; yc := _cy ;
          xa := _ax ; ya := _ay ;
          xb := _bx ; yb := _by ;
          xs := _sx ; ys := _sy ;
          xe := _ex ; ye := _ey ;
       end
       else begin
          xc := ( _cx * rotateCos  -  _cy * rotateSin ) ;
          yc := ( _cx * rotateSin  +  _cy * rotateCos ) ;

          xa := ( _ax * rotateCos  -  _ay * rotateSin ) ;
          ya := ( _ax * rotateSin  +  _ay * rotateCos ) ;

          xb := ( _bx * rotateCos  -  _by * rotateSin ) ;
          yb := ( _bx * rotateSin  +  _by * rotateCos ) ;

          xs := ( _sx * rotateCos  -  _sy * rotateSin ) ;
          ys := ( _sx * rotateSin  +  _sy * rotateCos ) ;

          xe := ( _ex * rotateCos  -  _ey * rotateSin ) ;
          ye := ( _ex * rotateSin  +  _ey * rotateCos ) ;
       end ;

       da := Sqrt( Sqr( xa - xc ) + Sqr( ya - yc ) ) ;
       db := Sqrt( Sqr( xb - xc ) + Sqr( yb - yc ) ) ;

    // calculate angle of pie
       if      ys = yc then begin
                              if xs < xc then start := Pi
                                         else start := 0  ;
                            end
       else if xs = xc then begin
                              if ys < yc then start := -Pi/2
                                         else start :=  Pi/2 ;
                            end
       else begin
         if Abs(xs - xc) > Abs(da) then start := da
                                   else start := ArcCos( (xs - xc)/ da ) ;
         if ys < yc then start := - start ;

       end ;

       if      ye = yc then begin
                              if xe < xc then stop := Pi
                                         else stop := 0  ;
                            end
       else if xe = xc then begin
                              if ye < yc then stop := -Pi/2
                                         else stop :=  Pi/2 ;
                            end
       else begin
         if Abs(ye - yc) > Abs(db) then stop := db
                                   else stop := ArcSin( (ye - yc) / db )  ;

         if ye < yc then begin
                             if xe < xc then stop := -Pi - stop ;
                           end
         else              begin
                             if xe < xc then stop := Pi - stop ;
                          end ;
       end ;

       if (xe = xs) and (ye = ys) then begin
         // full ellipse
         start := 0    ;
         arcangle := 2*Pi ;
       end
       else
         arcangle := fmod( (-stop+start) +4*Pi, 2*Pi) ;

    // calculate number of segments - and minimize it
       if      arcangle < Pi/4 then steps := MAX_STEPS div 8
       else if arcangle < Pi/2 then steps := MAX_STEPS div 4
       else if arcangle < Pi   then steps := MAX_STEPS div 2
       else steps := MAX_STEPS - 1 ;

       step := - Abs( arcangle ) / steps ;

    // calculate elliptical arc
       for cnt := 0 to steps do begin
         SinCos( start, ssin, scos ) ;
         yn := -db * ssin * rotateCos + da * scos * rotateSin ;
         xn :=  da * scos * rotateCos + db * ssin * rotateSin ;

         svgPathAddPoint( _cx+xn, _cy-yn ) ;

         start := start + step ;
       end ;
  end ;

  { Prepare an elliptical arc element.
  }
  procedure T_SVGParser.toEllipticalArc(
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
  ) ;

    procedure arc_segment( var cpx, cpy, cpx2, cpy2: Double ;
                           const xc, yc, th0, th1, rx, ry, x_axis_rot : Double ) ;
    var
      x1, y1, x2, y2, x3, y3 : Double;
      t: Double;
      th_half: Double;
      f, sinf, cosf: Double;
      pm : TGIS_DoubleArray ;
    begin
      f := x_axis_rot * Pi / 180.0;
      SinCos( f, sinf, cosf ) ;

      th_half := 0.5 * (th1 - th0);
      t := (8.0 / 3.0) * Sin (th_half * 0.5) * Sin (th_half * 0.5) / Sin (th_half);
      x1 := rx*(Cos (th0) - t * Sin (th0));
      y1 := ry*(Sin (th0) + t * Cos (th0));
      x3 := rx*Cos (th1);
      y3 := ry*Sin (th1);
      x2 := x3 + rx*(t * Sin (th1));
      y2 := y3 + ry*(-t * Cos (th1));

      SetLength(pm,6);
      pm[0] :=  xc + cosf*x1 - sinf*y1;
      pm[1] :=  yc + sinf*x1 + cosf*y1;
      pm[2] :=  xc + cosf*x2 - sinf*y2;
      pm[3] :=  yc + sinf*x2 + cosf*y2;
      pm[4] :=  xc + cosf*x3 - sinf*y3;
      pm[5] :=  yc + sinf*x3 + cosf*y3;

      toCubicBezier( cpx, cpy, cpx2, cpy2, pm, false ) ;
    end ;

  var
    rx, ry,
    x_axis_rotation   : Double ;
    large,
    sweep             : Boolean ;
    f, sinf, cosf     : Double ;
    x1, y1, x2, y2    : Double ;
    x1_, y1_          : Double ;
    cx_, cy_, cx, cy  : Double ;
    gamma             : Double ;
    theta1,delta_theta: Double ;
    k1, k2, k3, k4, k5: Double ;
    i, n_segs         : Integer;
  begin
    rx    := _args[0] ;
    ry    := _args[1] ;
    x_axis_rotation := _args[2] ;
    large := _args[3] = 1 ;
    sweep := _args[4] = 1 ;

    if _rel then begin
      x1 := _cpx;
      y1 := _cpy;
      x2 := _cpx + _args[5];
      y2 := _cpy + _args[6];
    end
    else begin
      x1 := _cpx;
      y1 := _cpy;
      x2 := _args[5];
      y2 := _args[6];
    end ;

    if (x1 = x2) and (y1 = y2) then exit ;

    f := x_axis_rotation * Pi / 180.0;
    SinCos( f, sinf, cosf ) ;

    if (rx < 0) then
      rx := -rx;
    if (ry < 0) then
      ry := -ry;

    k1 := (x1 - x2)/2;
    k2 := (y1 - y2)/2;

    x1_ := cosf * k1 + sinf * k2;
    y1_ := -sinf * k1 + cosf * k2;

    if ry*ry > 0 then
      gamma := (x1_*x1_)/(rx*rx) + (y1_*y1_)/(ry*ry)
    else
      gamma := 0.5 ;
    if (gamma > 1) then begin
      rx := rx * Sqrt(gamma);
      ry := ry * Sqrt(gamma);
    end ;

    // Compute the center
    k1 := rx*rx*y1_*y1_ + ry*ry*x1_*x1_;
    if (k1 = 0) then exit ;

    k1 := Sqrt(Abs((rx*rx*ry*ry)/k1 - 1));
    if (sweep = large ) then
      k1 := -k1;

    cx_ := k1*rx*y1_/ry;
    cy_ := -k1*ry*x1_/rx;

    cx := cosf*cx_ - sinf*cy_ + (x1+x2)/2;
    cy := sinf*cx_ + cosf*cy_ + (y1+y2)/2;

    // Compute start angle
    k1 := (x1_ - cx_)/rx;
    k2 := (y1_ - cy_)/ry;
    k3 := (-x1_ - cx_)/rx;
    k4 := (-y1_ - cy_)/ry;

    k5 := Sqrt(Abs(k1*k1 + k2*k2));
    if (k5 = 0) then exit;

    k5 := k1/k5;
    if (k5 < -1) then
      k5 := -1
    else if (k5 > 1) then
      k5 := 1;
    theta1 := ArcCos(k5);
    if (k2 < 0) then
      theta1 := -theta1;

    // Compute delta_theta */
    k5 := Sqrt(Abs((k1*k1 + k2*k2)*(k3*k3 + k4*k4)));
    if (k5 = 0) then exit ;

    k5 := (k1*k3 + k2*k4)/k5;
    if (k5 < -1) then
      k5 := -1
    else if (k5 > 1) then
      k5 := 1;
    delta_theta := ArcCos(k5);
    if (k1*k4 - k3*k2 < 0) then
      delta_theta := -delta_theta;

    if sweep and (delta_theta < 0) then
      delta_theta := delta_theta + Pi*2
    else if not sweep and ( delta_theta > 0) then
      delta_theta := delta_theta - Pi*2;

    // Now draw the arc */
    n_segs := CeilS(Abs (delta_theta / (Pi * 0.5 + 0.001)));

    for i := 0 to n_segs-1 do
      arc_segment( x1,y1,_cpx2,_cpy2, cx, cy,
                   theta1 + i * delta_theta / n_segs,
                   theta1 + (i + 1) * delta_theta / n_segs,
                   rx, ry, x_axis_rotation
                  );

    if _rel then begin
      _cpx := _cpx + _args[5] ;
      _cpy := _cpy + _args[6] ;
    end
    else begin
      _cpx := _args[5] ;
      _cpy := _args[6] ;
    end ;
    _cpx2 := x2 ;
    _cpy2 := y2 ;
  end ;

  { Prepare a line element.
  }
  procedure T_SVGParser.toLine(
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
  ) ;
  var
    cpx, cpy : Double ;
  begin
    if IsNan( _cpx ) then cpx := 0
                     else cpx := _cpx ;
    if IsNan( _cpy ) then cpy := 0
                     else cpy := _cpy ;

    if _rel then begin
      _cpx := cpx + _args[0] ;
      _cpy := cpy + _args[1] ;
    end
    else begin
      _cpx := _args[0] ;
      _cpy := _args[1] ;
    end ;

    svgPathAddPoint( _cpx, _cpy ) ;

    _cpx2 := NaN ;
    _cpy2 := NaN ;
  end ;

  { Prepare a horizontal line element.
  }
  procedure T_SVGParser.toHLine(
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
   ) ;
  var
    cpx : Double ;
  begin
    if IsNan( _cpx ) then cpx := 0
                     else cpx := _cpx ;

    if _rel then
      _cpx := cpx + _args[0]
    else
      _cpx := _args[0] ;

    svgPathAddPoint( _cpx, _cpy ) ;

    _cpx2 := NaN ;
    _cpy2 := NaN ;
  end ;

  { Prepare a vertical line element.
  }
  procedure T_SVGParser.toVLine(
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
   ) ;
  var
    cpy : Double ;
  begin
    if IsNan( _cpy ) then cpy := 0
                     else cpy := _cpy ;

    if _rel then
      _cpy := cpy + _args[0]
    else
      _cpy := _args[0] ;

    svgPathAddPoint( _cpx, _cpy ) ;

    _cpx2 := NaN ;
    _cpy2 := NaN ;
  end ;

  { Prepare a cubic bezier element.
  }
  procedure T_SVGParser.toCubicBezier (
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
   ) ;
  var
    x1, y1, x2, y2, cx1, cy1, cx2, cy2 : Double ;
    cpx, cpy : Double ;
  begin
    if IsNan( _cpx ) then cpx := 0
                     else cpx := _cpx ;
    if IsNan( _cpy ) then cpy := 0
                     else cpy := _cpy ;

    x1 := cpx ;
    y1 := cpy ;

    if _rel then begin
      cx1 := cpx + _args[0] ;
      cy1 := cpy + _args[1] ;
      cx2 := cpx + _args[2] ;
      cy2 := cpy + _args[3] ;
      x2  := cpx + _args[4] ;
      y2  := cpy + _args[5] ;
    end
    else begin
      cx1 := _args[0] ;
      cy1 := _args[1] ;
      cx2 := _args[2] ;
      cy2 := _args[3] ;
      x2  := _args[4] ;
      y2  := _args[5] ;
    end ;

    cubicBezRec( x1,y1, cx1,cy1, cx2,cy2, x2,y2, 0);
    svgPathAddPoint( x2, y2 ) ;

    _cpx2 := cx2 ;
    _cpy2 := cy2 ;
    _cpx  := x2 ;
    _cpy  := y2 ;
  end ;

  { Prepare a cubic bezier simple element.
  }
  procedure T_SVGParser.toCubicBezierS (
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
   ) ;
  var
    x1, y1, x2, y2, cx1, cy1, cx2, cy2 : Double ;
    cpx, cpy : Double ;
  begin
    if IsNan( _cpx ) then cpx := 0
                     else cpx := _cpx ;
    if IsNan( _cpy ) then cpy := 0
                     else cpy := _cpy ;

    x1 := cpx ;
    y1 := cpy ;

    if _rel then begin
      cx2 := cpx + _args[0] ;
      cy2 := cpy + _args[1] ;
      x2  := cpx + _args[2] ;
      y2  := cpy + _args[3] ;
    end
    else begin
      cx2 := _args[0] ;
      cy2 := _args[1] ;
      x2  := _args[2] ;
      y2  := _args[3] ;
    end ;

    if IsNan( _cpx2 ) then
      cx1 := x1
    else
      cx1 := 2*x1 - _cpx2 ;

    if IsNan( _cpy2 ) then
      cy1 := y1
    else
      cy1 := 2*y1 - _cpy2 ;

    cubicBezRec(x1,y1, cx1,cy1, cx2,cy2, x2,y2,0) ;
    svgPathAddPoint(x2, y2) ;

    _cpx2 := cx2 ;
    _cpy2 := cy2 ;
    _cpx  := x2 ;
    _cpy  := y2 ;
  end ;

  { Prepare a quad bezier element.
  }
  procedure T_SVGParser.toQuadBezier(
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
   ) ;
  var
    x1, y1, x2, y2, cx, cy : Double ;
    cpx, cpy : Double ;
  begin
    if IsNan( _cpx ) then cpx := 0
                     else cpx := _cpx ;
    if IsNan( _cpy ) then cpy := 0
                     else cpy := _cpy ;

    x1 := cpx ;
    y1 := cpy ;

    if _rel then begin
      cx := cpx + _args[0] ;
      cy := cpy + _args[1] ;
      x2 := cpx + _args[2] ;
      y2 := cpy + _args[3] ;
    end
    else begin
      cx := _args[0] ;
      cy := _args[1] ;
      x2 := _args[2] ;
      y2 := _args[3] ;
    end ;

    quadBezRec(x1,y1, cx,cy, x2,y2,0);

    _cpx2 := cx ;
    _cpy2 := cy ;
    _cpx  := x2 ;
    _cpy  := y2 ;
  end ;

  { Prepare a quad bezier simple element.
  }
  procedure T_SVGParser.toQuadBezierS(
      var _cpx  : Double ;
      var _cpy  : Double ;
      var _cpx2 : Double ;
      var _cpy2 : Double ;
    const _args : TGIS_DoubleArray ;
    const _rel  : Boolean
   ) ;
  var
    x1, y1, x2, y2, cx, cy : Double ;
    cpx, cpy : Double ;
  begin
    if IsNan( _cpx ) then cpx := 0
                     else cpx := _cpx ;
    if IsNan( _cpy ) then cpy := 0
                     else cpy := _cpy ;

    x1 := cpx ;
    y1 := cpy ;

    if _rel then begin
      x2 := cpx + _args[0] ;
      y2 := cpy + _args[1] ;
    end
    else begin
      x2 := _args[0] ;
      y2 := _args[1] ;
    end ;

    cx := 2*x1 - _cpx2 ;
    cy := 2*y1 - _cpy2 ;

    quadBezRec(x1,y1, cx,cy, x2,y2,0);

    _cpx2 := cx ;
    _cpy2 := cy ;
    _cpx  := x2 ;
    _cpy  := y2 ;
  end ;

  { Initialize color table.
  }
  procedure T_SVGParser.svgInitColor ;
  begin
    colors.Add( 'aliceblue',            TGIS_Color.FromRGB(240, 248, 255) );
    colors.Add( 'antiquewhite',         TGIS_Color.FromRGB(250, 235, 215) );
    colors.Add( 'aqua',                 TGIS_Color.FromRGB(0,   255, 255) );
    colors.Add( 'aquamarine',           TGIS_Color.FromRGB(127, 255, 212) );
    colors.Add( 'azure',                TGIS_Color.FromRGB(240, 255, 255) );
    colors.Add( 'beige',                TGIS_Color.FromRGB(245, 245, 220) );
    colors.Add( 'bisque',               TGIS_Color.FromRGB(255, 228, 196) );
    colors.Add( 'black',                TGIS_Color.FromRGB(0,   0,   0  ) );
    colors.Add( 'blanchedalmond',       TGIS_Color.FromRGB(255, 235, 205) );
    colors.Add( 'blue',                 TGIS_Color.FromRGB(0,   0,   255) );
    colors.Add( 'blueviolet',           TGIS_Color.FromRGB(138, 43,  226) );
    colors.Add( 'brown',                TGIS_Color.FromRGB(165, 42,  42 ) );
    colors.Add( 'burlywood',            TGIS_Color.FromRGB(222, 184, 135) );
    colors.Add( 'cadetblue',            TGIS_Color.FromRGB(95,  158, 160) );
    colors.Add( 'chartreuse',           TGIS_Color.FromRGB(127, 255, 0  ) );
    colors.Add( 'chocolate',            TGIS_Color.FromRGB(210, 105, 30 ) );
    colors.Add( 'coral',                TGIS_Color.FromRGB(255, 127, 80 ) );
    colors.Add( 'cornflowerblue',       TGIS_Color.FromRGB(100, 149, 237) );
    colors.Add( 'cornsilk',             TGIS_Color.FromRGB(255, 248, 220) );
    colors.Add( 'crimson',              TGIS_Color.FromRGB(220, 20,  60 ) );
    colors.Add( 'cyan',                 TGIS_Color.FromRGB(0,   255, 255) );
    colors.Add( 'darkblue',             TGIS_Color.FromRGB(0,   0,   139) );
    colors.Add( 'darkcyan',             TGIS_Color.FromRGB(0,   139, 139) );
    colors.Add( 'darkgoldenrod',        TGIS_Color.FromRGB(184, 134, 11 ) );
    colors.Add( 'darkgray',             TGIS_Color.FromRGB(169, 169, 169) );
    colors.Add( 'darkgreen',            TGIS_Color.FromRGB(0,   100, 0  ) );
    colors.Add( 'darkgrey',             TGIS_Color.FromRGB(169, 169, 169) );
    colors.Add( 'darkkhaki',            TGIS_Color.FromRGB(189, 183, 107) );
    colors.Add( 'darkmagenta',          TGIS_Color.FromRGB(139, 0,   139) );
    colors.Add( 'darkolivegreen',       TGIS_Color.FromRGB(85,  107, 47 ) );
    colors.Add( 'darkorange',           TGIS_Color.FromRGB(255, 140, 0  ) );
    colors.Add( 'darkorchid',           TGIS_Color.FromRGB(153, 50,  204) );
    colors.Add( 'darkred',              TGIS_Color.FromRGB(139, 0,   0  ) );
    colors.Add( 'darksalmon',           TGIS_Color.FromRGB(233, 150, 122) );
    colors.Add( 'darkseagreen',         TGIS_Color.FromRGB(143, 188, 143) );
    colors.Add( 'darkslateblue',        TGIS_Color.FromRGB(72,  61,  139) );
    colors.Add( 'darkslategray',        TGIS_Color.FromRGB(47,  79,  79 ) );
    colors.Add( 'darkslategrey',        TGIS_Color.FromRGB(47,  79,  79 ) );
    colors.Add( 'darkturquoise',        TGIS_Color.FromRGB(0,   206, 209) );
    colors.Add( 'darkviolet',           TGIS_Color.FromRGB(148, 0,   211) );
    colors.Add( 'deeppink',             TGIS_Color.FromRGB(255, 20,  147) );
    colors.Add( 'deepskyblue',          TGIS_Color.FromRGB(0,   191, 255) );
    colors.Add( 'dimgray',              TGIS_Color.FromRGB(105, 105, 105) );
    colors.Add( 'dimgrey',              TGIS_Color.FromRGB(105, 105, 105) );
    colors.Add( 'dodgerblue',           TGIS_Color.FromRGB(30,  144, 255) );
    colors.Add( 'firebrick',            TGIS_Color.FromRGB(178, 34,  34 ) );
    colors.Add( 'floralwhite',          TGIS_Color.FromRGB(255, 250, 240) );
    colors.Add( 'forestgreen',          TGIS_Color.FromRGB(34,  139, 34 ) );
    colors.Add( 'fuchsia',              TGIS_Color.FromRGB(255, 0,   255) );
    colors.Add( 'gainsboro',            TGIS_Color.FromRGB(220, 220, 220) );
    colors.Add( 'ghostwhite',           TGIS_Color.FromRGB(248, 248, 255) );
    colors.Add( 'gold',                 TGIS_Color.FromRGB(255, 215, 0  ) );
    colors.Add( 'goldenrod',            TGIS_Color.FromRGB(218, 165, 32 ) );
    colors.Add( 'gray',                 TGIS_Color.FromRGB(128, 128, 128) );
    colors.Add( 'green',                TGIS_Color.FromRGB(0,   128, 0  ) );
    colors.Add( 'greenyellow',          TGIS_Color.FromRGB(173, 255, 47 ) );
    colors.Add( 'grey',                 TGIS_Color.FromRGB(128, 128, 128) );
    colors.Add( 'honeydew',             TGIS_Color.FromRGB(240, 255, 240) );
    colors.Add( 'hotpink',              TGIS_Color.FromRGB(255, 105, 180) );
    colors.Add( 'indianred',            TGIS_Color.FromRGB(205, 92,  92 ) );
    colors.Add( 'indigo',               TGIS_Color.FromRGB(75,  0,   130) );
    colors.Add( 'ivory',                TGIS_Color.FromRGB(255, 255, 240) );
    colors.Add( 'khaki',                TGIS_Color.FromRGB(240, 230, 140) );
    colors.Add( 'lavender',             TGIS_Color.FromRGB(230, 230, 250) );
    colors.Add( 'lavenderblush',        TGIS_Color.FromRGB(255, 240, 245) );
    colors.Add( 'lawngreen',            TGIS_Color.FromRGB(124, 252, 0  ) );
    colors.Add( 'lemonchiffon',         TGIS_Color.FromRGB(255, 250, 205) );
    colors.Add( 'lightblue',            TGIS_Color.FromRGB(173, 216, 230) );
    colors.Add( 'lightcoral',           TGIS_Color.FromRGB(240, 128, 128) );
    colors.Add( 'lightcyan',            TGIS_Color.FromRGB(224, 255, 255) );
    colors.Add( 'lightgoldenrodyellow', TGIS_Color.FromRGB(250, 250, 210) );
    colors.Add( 'lightgray',            TGIS_Color.FromRGB(211, 211, 211) );
    colors.Add( 'lightgreen',           TGIS_Color.FromRGB(144, 238, 144) );
    colors.Add( 'lightgrey',            TGIS_Color.FromRGB(211, 211, 211) );
    colors.Add( 'lightpink',            TGIS_Color.FromRGB(255, 182, 193) );
    colors.Add( 'lightsalmon',          TGIS_Color.FromRGB(255, 160, 122) );
    colors.Add( 'lightseagreen',        TGIS_Color.FromRGB(32,  178, 170) );
    colors.Add( 'lightskyblue',         TGIS_Color.FromRGB(135, 206, 250) );
    colors.Add( 'lightslategray',       TGIS_Color.FromRGB(119, 136, 153) );
    colors.Add( 'lightslategrey',       TGIS_Color.FromRGB(119, 136, 153) );
    colors.Add( 'lightsteelblue',       TGIS_Color.FromRGB(176, 196, 222) );
    colors.Add( 'lightyellow',          TGIS_Color.FromRGB(255, 255, 224) );
    colors.Add( 'lime',                 TGIS_Color.FromRGB(0,   255, 0  ) );
    colors.Add( 'limegreen',            TGIS_Color.FromRGB(50,  205, 50 ) );
    colors.Add( 'linen',                TGIS_Color.FromRGB(250, 240, 230) );
    colors.Add( 'magenta',              TGIS_Color.FromRGB(255, 0,   255) );
    colors.Add( 'maroon',               TGIS_Color.FromRGB(128, 0,   0  ) );
    colors.Add( 'mediumaquamarine',     TGIS_Color.FromRGB(102, 205, 170) );
    colors.Add( 'mediumblue',           TGIS_Color.FromRGB(0,   0,   205) );
    colors.Add( 'mediumorchid',         TGIS_Color.FromRGB(186, 85,  211) );
    colors.Add( 'mediumpurple',         TGIS_Color.FromRGB(147, 112, 219) );
    colors.Add( 'mediumseagreen',       TGIS_Color.FromRGB(60,  179, 113) );
    colors.Add( 'mediumslateblue',      TGIS_Color.FromRGB(123, 104, 238) );
    colors.Add( 'mediumspringgreen',    TGIS_Color.FromRGB(0,   250, 154) );
    colors.Add( 'mediumturquoise',      TGIS_Color.FromRGB(72,  209, 204) );
    colors.Add( 'mediumvioletred',      TGIS_Color.FromRGB(199, 21,  133) );
    colors.Add( 'midnightblue',         TGIS_Color.FromRGB(25,  25,  112) );
    colors.Add( 'mintcream',            TGIS_Color.FromRGB(245, 255, 250) );
    colors.Add( 'mistyrose',            TGIS_Color.FromRGB(255, 228, 225) );
    colors.Add( 'moccasin',             TGIS_Color.FromRGB(255, 228, 181) );
    colors.Add( 'navajowhite',          TGIS_Color.FromRGB(255, 222, 173) );
    colors.Add( 'navy',                 TGIS_Color.FromRGB(0,   0,   128) );
    colors.Add( 'oldlace',              TGIS_Color.FromRGB(253, 245, 230) );
    colors.Add( 'olive',                TGIS_Color.FromRGB(128, 128, 0  ) );
    colors.Add( 'olivedrab',            TGIS_Color.FromRGB(107, 142, 35 ) );
    colors.Add( 'orange',               TGIS_Color.FromRGB(255, 165, 0  ) );
    colors.Add( 'orangered',            TGIS_Color.FromRGB(255, 69,  0  ) );
    colors.Add( 'orchid',               TGIS_Color.FromRGB(218, 112, 214) );
    colors.Add( 'palegoldenrod',        TGIS_Color.FromRGB(238, 232, 170) );
    colors.Add( 'palegreen',            TGIS_Color.FromRGB(152, 251, 152) );
    colors.Add( 'paleturquoise',        TGIS_Color.FromRGB(175, 238, 238) );
    colors.Add( 'palevioletred',        TGIS_Color.FromRGB(219, 112, 147) );
    colors.Add( 'papayawhip',           TGIS_Color.FromRGB(255, 239, 213) );
    colors.Add( 'peachpuff',            TGIS_Color.FromRGB(255, 218, 185) );
    colors.Add( 'peru',                 TGIS_Color.FromRGB(205, 133, 63 ) );
    colors.Add( 'pink',                 TGIS_Color.FromRGB(255, 192, 203) );
    colors.Add( 'plum',                 TGIS_Color.FromRGB(221, 160, 221) );
    colors.Add( 'powderblue',           TGIS_Color.FromRGB(176, 224, 230) );
    colors.Add( 'purple',               TGIS_Color.FromRGB(128, 0,   128) );
    colors.Add( 'red',                  TGIS_Color.FromRGB(255, 0,   0  ) );
    colors.Add( 'rosybrown',            TGIS_Color.FromRGB(188, 143, 143) );
    colors.Add( 'royalblue',            TGIS_Color.FromRGB(65,  105, 225) );
    colors.Add( 'saddlebrown',          TGIS_Color.FromRGB(139, 69,  19 ) );
    colors.Add( 'salmon',               TGIS_Color.FromRGB(250, 128, 114) );
    colors.Add( 'sandybrown',           TGIS_Color.FromRGB(244, 164, 96 ) );
    colors.Add( 'seagreen',             TGIS_Color.FromRGB(46,  139, 87 ) );
    colors.Add( 'seashell',             TGIS_Color.FromRGB(255, 245, 238) );
    colors.Add( 'sienna',               TGIS_Color.FromRGB(160, 82,  45 ) );
    colors.Add( 'silver',               TGIS_Color.FromRGB(192, 192, 192) );
    colors.Add( 'skyblue',              TGIS_Color.FromRGB(135, 206, 235) );
    colors.Add( 'slateblue',            TGIS_Color.FromRGB(106, 90,  205) );
    colors.Add( 'slategray',            TGIS_Color.FromRGB(112, 128, 144) );
    colors.Add( 'slategrey',            TGIS_Color.FromRGB(112, 128, 144) );
    colors.Add( 'snow',                 TGIS_Color.FromRGB(255, 250, 250) );
    colors.Add( 'springgreen',          TGIS_Color.FromRGB(0,   255, 127) );
    colors.Add( 'steelblue',            TGIS_Color.FromRGB(70,  130, 180) );
    colors.Add( 'tan',                  TGIS_Color.FromRGB(210, 180, 140) );
    colors.Add( 'teal',                 TGIS_Color.FromRGB(0,   128, 128) );
    colors.Add( 'thistle',              TGIS_Color.FromRGB(216, 191, 216) );
    colors.Add( 'tomato',               TGIS_Color.FromRGB(255, 99,  71 ) );
    colors.Add( 'turquoise',            TGIS_Color.FromRGB(64,  224, 208) );
    colors.Add( 'violet',               TGIS_Color.FromRGB(238, 130, 238) );
    colors.Add( 'wheat',                TGIS_Color.FromRGB(245, 222, 179) );
    colors.Add( 'white',                TGIS_Color.FromRGB(255, 255, 255) );
    colors.Add( 'whitesmoke',           TGIS_Color.FromRGB(245, 245, 245) );
    colors.Add( 'yellow',               TGIS_Color.FromRGB(255, 255, 0  ) );
    colors.Add( 'yellowgreen',          TGIS_Color.FromRGB(154, 205, 50 ) );
    colors.Add( 'none',                 TGIS_Color.FromRGB(255, 255, 255) );
  end ;

  { Push svg attribtues.
  }
  procedure T_SVGParser.svgPushAttr ;
  begin
    if svgAttr.Count > 0 then
      svgAttr.Push( T_SVGAttrib.Create( T_SVGAttrib( svgAttr.Peek ) ) )
    else
      svgAttr.Push( T_SVGAttrib.Create( nil ) ) ;
  end ;

  { Pop svg attribtues.
  }
  procedure T_SVGParser.svgPopAttr ;
  var
    attr : T_SVGAttrib ;
  begin
    if svgAttr.Count > 0 then
      attr := T_SVGAttrib( svgAttr.Pop )
    else
      attr := nil ;

    if assigned( attr ) then
      FreeObject( attr ) ;
  end ;

  procedure T_SVGParser.svgParseTrans(
    const _value : String
  ) ;
  var
    i     : Integer ;
    tkn   : TGIS_Tokenizer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _value, [ '(', ')' ] ) ;

      i := 0 ;
      while i < tkn.Result.Count do begin

        if      tkn.Result[i] = SVG_TRANSFORM_MATRIX    then begin
          inc( i ) ;
          svgParseMatrix( tkn.Result[i] ) ;
          inc( i ) ;
        end
        else if tkn.Result[i] = SVG_TRANSFORM_TRANSLATE then begin
          inc( i ) ;
          svgParseTranslate( tkn.Result[i] ) ;
          inc( i ) ;
        end
        else if tkn.Result[i] = SVG_TRANSFORM_SCALE     then begin
          inc( i ) ;
          svgParseScale( tkn.Result[i] ) ;
          inc( i ) ;
        end
        else if tkn.Result[i] = SVG_TRANSFORM_ROTATE    then begin
          inc( i ) ;
          svgParseRotate( tkn.Result[i] ) ;
          inc( i ) ;
        end
        else if tkn.Result[i] = SVG_TRANSFORM_SKEWX     then begin
          inc( i ) ;
          svgApplySkewX( DotStrToFloat( tkn.Result[i] ) ) ;
          inc( i ) ;
        end
        else if tkn.Result[i] = SVG_TRANSFORM_SKEWY     then begin
          inc( i ) ;
          svgApplySkewY( DotStrToFloat( tkn.Result[i] ) ) ;
          inc( i ) ;
        end
        else
          inc( i ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure T_SVGParser.svgParseMatrix(
    const _value   : String
  ) ;
  var
    pv  : TGIS_DoubleArray ;
    cnt : Integer ;
  begin
    readParams( _value, pv, cnt ) ;
    if cnt = 6 then
      svgApplyMatrix( pv ) ;
  end ;

  procedure T_SVGParser.svgMatrixMultiply(
    const _matrix  : TGIS_DoubleArray ;
    const _params  : TGIS_DoubleArray
  ) ;
  var
    t0, t2, t4 : Double ;
  begin
    t0 := _matrix[0] * _params[0] + _matrix[1] * _params[2] ;
    t2 := _matrix[2] * _params[0] + _matrix[3] * _params[2] ;
    t4 := _matrix[4] * _params[0] + _matrix[5] * _params[2] + _params[4] ;

    _matrix[1] := _matrix[0] * _params[1] + _matrix[1] * _params[3] ;
    _matrix[3] := _matrix[2] * _params[1] + _matrix[3] * _params[3] ;
    _matrix[5] := _matrix[4] * _params[1] + _matrix[5] * _params[3] + _params[5] ;
    _matrix[0] := t0 ;
    _matrix[2] := t2 ;
    _matrix[4] := t4 ;
  end ;

  procedure T_SVGParser.svgApplyMatrix(
    const _params  : TGIS_DoubleArray
  ) ;
  var
    attr : T_SVGAttrib ;
  begin
    attr := T_SVGAttrib( svgAttr.Peek ) ;

    svgMatrixMultiply( _params, attr.matrix ) ;
    attr.matrix := _params ;
  end ;

  procedure T_SVGParser.svgParseTranslate(
    const _value   : String
  ) ;
  var
    pv  : TGIS_DoubleArray ;
    cnt : Integer ;
  begin
    readParams( _value, pv, cnt ) ;
    if cnt = 2 then
      svgApplyTranslate( pv[0], pv[1] )
    else
      svgApplyTranslate( pv[0], pv[0] ) ;
  end ;

  procedure T_SVGParser.svgApplyTranslate(
    const _tx      : Double ;
    const _ty      : Double
  ) ;
  var
    t : TGIS_DoubleArray ;
  begin
    SetLength( t, 6 ) ;
    t[0] := 1.0 ;
    t[1] := 0.0 ;
    t[2] := 0.0 ;
    t[3] := 1.0 ;
    t[4] := _tx ;
    t[5] := _ty ;

    svgApplyMatrix( t ) ;
  end ;


  procedure T_SVGParser.svgParseScale(
    const _value : String
  ) ;
  var
    pv  : TGIS_DoubleArray ;
    cnt : Integer ;
  begin
    readParams( _value, pv, cnt ) ;
    if cnt = 2 then
      svgApplyScale( pv[0], pv[1] )
    else
      svgApplyScale( pv[0], pv[0] ) ;
  end ;

  procedure T_SVGParser.svgApplyScale(
    const _sx  : Double ;
    const _sy  : Double
  ) ;
  var
    t : TGIS_DoubleArray ;
  begin
    SetLength( t, 6 ) ;
    t[0] := _sx ;
    t[1] := 0 ;
    t[2] := 0 ;
    t[3] := _sy ;
    t[4] := 0 ;
    t[5] := 0 ;

    svgApplyMatrix( t ) ;
  end ;

  procedure T_SVGParser.svgParseRotate(
    const _value   : String
  ) ;
  var
    pv  : TGIS_DoubleArray ;
    cnt : Integer ;
  begin
    readParams( _value, pv, cnt ) ;
    if cnt = 3 then begin
      svgApplyTranslate( pv[1], pv[2] );
      svgApplyRotate( pv[0] ) ;
      svgApplyTranslate( -1*pv[1], -1*pv[2] );
    end
    else
      svgApplyRotate( pv[0] ) ;
  end ;

  procedure T_SVGParser.svgApplyRotate(
    const _angle   : Double
  ) ;
  var
    t : TGIS_DoubleArray ;
    a,s,c : Double ;
  begin
    SetLength( t, 6 ) ;
    a := _angle * Pi / 180 ;
    SinCos( a, s, c ) ;
    t[0] := c ;
    t[1] := s ;
    t[2] := -s ;
    t[3] := c ;
    t[4] := 0 ;
    t[5] := 0 ;

    svgApplyMatrix( t ) ;
  end ;

  procedure T_SVGParser.svgApplySkewX(
    const _angle   : Double
  ) ;
  var
    t : TGIS_DoubleArray ;
    a : Double ;
  begin
    a := _angle * Pi / 180 ;
    SetLength( t, 6 ) ;
    t[0] := 1 ;
    t[1] := 0 ;
    t[2] := Tan(a) ;
    t[3] := 1 ;
    t[4] := 0 ;
    t[5] := 0 ;

    svgApplyMatrix( t ) ;
  end ;

    procedure T_SVGParser.svgApplySkewY(
    const _angle : Double
  ) ;
  var
    t : TGIS_DoubleArray ;
    a : Double ;
  begin
    a := _angle * Pi / 180 ;
    SetLength( t, 6 ) ;
    t[0] := 1 ;
    t[1] := Tan(a) ;
    t[2] := 0 ;
    t[3] := 1 ;
    t[4] := 0 ;
    t[5] := 0 ;

    svgApplyMatrix( t ) ;
  end ;

  procedure T_SVGParser.svgParseAttribs(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _attribs.Length-1 do begin
      if _attribs.GetLocalName( i ) = SVG_STYLE then
        svgParseStyle( _attribs.GetValue(i) )
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;
  end ;

  procedure T_SVGParser.svgParseStyle(
    const _style : String
  ) ;
  var
    tkn : TGIS_Tokenizer ;
    i   : Integer ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( _style, [ ';',':' ] ) ;
      i := 0 ;
      while i < tkn.Result.Count do begin
        svgParseAttr( tkn.Result[i], tkn.Result[i+1] ) ;
        inc( i, 2 ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end;
  end ;

  procedure T_SVGParser.svgParseAttr(
    const _name   : String ;
    const _value  : String
  ) ;
  var
    attr : T_SVGAttrib ;
  begin
    attr := T_SVGAttrib( svgAttr.Peek ) ;

    if not assigned( attr ) then exit ;

    if      _name = SVG_STYLE                 then
      svgParseStyle( _value )
    else if _name = SVG_STYLE_DISPLAY         then begin
      if attr.visible = 1 then
        attr.visible := readFlag( _value )
    end
    else if _name = SVG_STYLE_VISIBILITY      then begin
      if attr.visible = 1 then
        attr.visible := readVisible( _value ) ;
    end
    else if _name = SVG_STYLE_FILL            then begin
      attr.hasFill       := readFlag( _value ) ;
      attr.fillColor     := readColor( _value ) ;
    end
    else if _name = SVG_STYLE_FILL_OPACITY    then
      attr.fillOpacity   := readDouble( _value )
    else if _name = SVG_STYLE_STROKE          then begin
      attr.hasStroke     := readFlag( _value ) ;
      attr.strokeColor   := readColor( _value )
    end
    else if _name = SVG_STYLE_STROKE_WIDTH    then
      attr.strokeWidth   := readDouble( _value )
    else if _name = SVG_STYLE_STROKE_LINECUP  then
      attr.strokeLineCap := readLinecap( _value )
    else if _name = SVG_STYLE_STROKE_LINEJOIN  then
      attr.strokeLineJoin := readLineJoin( _value )
    else if _name = SVG_STYLE_STROKE_OPACITY  then
      attr.strokeOpacity := readDouble( _value )
    else if _name = SVG_STYLE_STROKE_DASH  then
      attr.strokeDash := readDash( _value )
    else if _name = SVG_STYLE_OPACITY  then
      attr.opacity := readDouble( _value )
    else if _name = SVG_FONT_SIZE  then
      attr.fontSize := readDouble( _value )
    else if _name = SVG_FONT_FAMILY  then
      attr.fontFamily := _value
    else if _name = SVG_FONT_WEIGHT  then
      attr.fontWeight := _value
    else if _name = SVG_TEXT_ANCHOR  then
      attr.textAnchor := _value
    else if _name = SVG_TRANSFORM then
      svgParseTrans( _value ) ;
  end ;

  { Prepare path.
  }
  procedure T_SVGParser.svgPathPrepare ;
  begin
    nPoints := 0 ;
    points.Clear ;

    nPartPoints := 0 ;
    nParts := 0 ;
    parts.Clear ;

    setFirst := False ;
  end ;

  procedure T_SVGParser.svgPathAddPoint(
    const _x : Double ;
    const _y : Double
   ) ;
   var
    x, y : Double ;
    attr : T_SVGAttrib ;
  begin
    attr := T_SVGAttrib( svgAttr.Peek ) ;

    x := _x*attr.matrix[0] + _y*attr.matrix[2] + attr.matrix[4] ;
    y := _x*attr.matrix[1] + _y*attr.matrix[3] + attr.matrix[5] ;

    {$IFDEF OXYGENE}
      points.WriteDouble( x ) ;
      points.WriteDouble( y ) ;
    {$ELSE}
      points.Write( x, sizeOf( x ) ) ;
      points.Write( y, sizeOf( y ) ) ;
    {$ENDIF}

    if setFirst then begin
      firstX := _x ;
      firstY := _y ;
      setFirst := False ;
    end ;

    inc( nPoints ) ;
  end ;

  { Close a path.
  }
  procedure T_SVGParser.svgPathClose ;
  begin
    svgPathAddPoint( firstX, firstY ) ;
  end ;

  { Add part to a path.
  }
  procedure T_SVGParser.svgPathAddPart ;
  var
    num : Integer ;
  begin
    num := nPoints-nPartPoints ;
    {$IFDEF OXYGENE}
      parts.WriteInteger( num ) ;
    {$ELSE}
      parts.Write( num, sizeOf( num ) ) ;
    {$ENDIF}
    nPartPoints := nPoints ;
    inc( nParts ) ;
  end ;

  procedure T_SVGParser.svgPathCreate(
    const _kind : Integer
  ) ;
  var
    attr    : T_SVGAttrib ;
    len     : Integer ;
    {$IFDEF SVG_TRANSPARENCY}
      h,s,l,t : Single ;
    {$ENDIF}
    twidth  : Double ;
  begin
    attr := T_SVGAttrib( svgAttr.Peek ) ;

    {$IFDEF OXYGENE}
     with T_SymbolSvgPrimitive( svgMeta  ) do begin
    {$ELSE}
     with T_SymbolSvgPrimitive( svgMeta^ ) do begin
    {$ENDIF}
        Kind := _kind ;

        {$IFDEF SVG_TRANSPARENCY}
          h := attr.fillColor.H ;
          s := attr.fillColor.S ;
          l := attr.fillColor.L ;

          t := attr.fillOpacity * attr.opacity ;

          BrushColor := TGIS_Color.FromAHSL( t, h, s, l ).ARGB ;
        {$ELSE}
          BrushColor := attr.fillColor.ARGB  ;
        {$ENDIF}

        if attr.hasFill = 1 then
          BrushStyle := TGIS_BrushStyle.Solid
        else
          BrushStyle := TGIS_BrushStyle.Clear ;

        {$IFDEF SVG_TRANSPARENCY}
          h := attr.strokeColor.H ;
          s := attr.strokeColor.S ;
          l := attr.strokeColor.L ;

          t := attr.strokeOpacity * attr.opacity ;

          PenColor := TGIS_Color.FromAHSL( t, h, s, l ).ARGB ;
        {$ELSE}
          PenColor := attr.strokeColor.ARGB ;
        {$ENDIF}

        if (attr.strokeWidth > 0) and (attr.strokeColor <> TGIS_Color.None) then begin
          twidth := attr.strokeWidth *
                    Sqrt( Abs( attr.matrix[0]*attr.matrix[3] -
                               attr.matrix[2]*attr.matrix[1]
                             )
                        ) ;
          PenWidth := RoundS( twidth * 1000 + 1 ) ;
        end
        else begin
          PenWidth := 0 ;
          attr.hasStroke := 0 ;
        end;

        LineCap  := attr.strokeLineCap ;
        LineJoin := attr.strokeLineJoin ;

        if attr.hasStroke = 1 then
          PenStyle := attr.strokeDash
        else begin
          if ( (Kind = SVG_ENTRY_SURFACE) or (Kind = SVG_ENTRY_MULTISURFACE) )
             and
             ( attr.hasFill = 1)
          then
            PenStyle := TGIS_PenStyle.Clear ;
        end ;

        Points := nPoints ;
        {$IFDEF SVG_TRANSPARENCY}
          Visible := attr.visible ;
        {$ELSE}
          Visible := TruncS(attr.opacity) and attr.visible ;
        {$ENDIF}
    end ;

    {$IFDEF OXYGENE}
      T_SymbolSvgPrimitive(svgMeta).Write( metaStream ) ;
    {$ELSE}
      metaStream.Write( svgMeta^, sizeOf( T_SymbolSvgPrimitive ) ) ;
    {$ENDIF}
    points.SaveToStream( metaStream ) ;

    if _kind = SVG_ENTRY_TEXT then begin
      len := text.Size ;
      {$IFDEF OXYGENE}
        metaStream.WriteInteger( len ) ;
      {$ELSE}
        metaStream.Write( len, sizeOf( len ) ) ;
      {$ENDIF}
      text.SaveToStream( metaStream ) ;
    end ;

    if _kind = SVG_ENTRY_MULTISURFACE then begin
      {$IFDEF OXYGENE}
        metaStream.WriteInteger( nParts ) ;
      {$ELSE}
        metaStream.Write( nParts, sizeOf( nParts ) ) ;
      {$ENDIF}
      parts.SaveToStream( metaStream ) ;
    end ;
  end ;

  { Write metastream end marker.
  }
   procedure T_SVGParser.svgPathEnd ;
   begin
    {$IFDEF OXYGENE}
      T_SymbolSvgPrimitive(svgMeta).Kind := 0 ;
      T_SymbolSvgPrimitive(svgMeta).Write( metaStream ) ;
    {$ELSE}
      T_SymbolSvgPrimitive(svgMeta^).Kind := 0 ;
      metaStream.Write( T_SymbolSvgPrimitive(svgMeta^),sizeOf(T_SymbolSvgPrimitive)) ;
    {$ENDIF}
   end ;

  procedure T_SVGParser.svgParseSvg(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i        : Integer ;
    w, h     : String ;
    vbox     : String ;
    vx,vy    : Double ;
    vw,vh    : Double ;
    cx, cy   : String ;
    vcx,vcy  : Double ;
    dw,dh    : Double ;
    tkn      : TGIS_Tokenizer ;
  begin
    vx := 0 ;
    vy := 0 ;

    for i := 0 to _attribs.Length-1 do begin
      if      _attribs.GetLocalName( i ) = SVG_ELEM_WIDTH      then
        w := _attribs.GetValue(i)
      else if _attribs.GetLocalName( i ) = SVG_ELEM_HEIGHT     then
        h := _attribs.GetValue(i)
      else if _attribs.GetLocalName( i ) = SVG_ELEM_CENTER_X   then
        cx := _attribs.GetValue(i)
      else if _attribs.GetLocalName( i ) = SVG_ELEM_CENTER_Y   then
        cy := _attribs.GetValue(i)
      else if _attribs.GetLocalName( i ) = SVG_ELEM_VIEWBOX    then
        vbox := _attribs.GetValue(i)
    end ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( vbox, ' ' ) ;
      if tkn.Result.Count > 3 then begin
        vx := DotStrToFloat( tkn.Result[ 0 ] ) ;
        vy := DotStrToFloat( tkn.Result[ 1 ] ) ;
        vw := DotStrToFloat( tkn.Result[ 2 ] ) ;
        vh := DotStrToFloat( tkn.Result[ 3 ] ) ;

        //RectEx
        viewBox    := RectF( vx, vy, vx+vw, vy+vh ) ;
        labelBox   := RectF( vx, vy, vx+vw, vy+vh ) ;
        boundryBox := RectF( vx, vy, vx+vw, vy+vh ) ;

        dw := readDouble(w) ;
        dh := readDouble(h) ;

        if Pos( '%', w ) >= StringFirst then
          dw := 600* dw/100 ;
        if Pos( '%', h ) >= StringFirst then
          dh := 600* dh/100 ;

        if dw = 0 then
          dw := 600 ;
        if dh = 0 then
          dh := 600 ;

        if dw <> 0 then
          scaleX := dw / vw
        else
          scaleX := 1 ;

        if dh <> 0 then
          scaleY := dh / vh
        else
          scaleY := 1 ;
      end
      else begin
        vw := readDouble(w) ;
        vh := readDouble(h) ;

        if Pos( '%', w ) >= StringFirst then
          vw := 600* vw/100 ;
        if Pos( '%', h ) >= StringFirst then
          vh := 600* vh/100 ;

        if vw = 0 then
          vw := 600 ;
        if vh = 0 then
          vh := 600 ;

        //RectEx
        viewBox    := RectF( vx, vy, vw, vh ) ;
        labelBox   := RectF( vx, vy, vw, vh ) ;
        boundryBox := RectF( vx, vy, vw, vh ) ;

        if vw <> 0 then
          scaleX := 600 / vw
        else
          scaleX := 1 ;

        if vh <> 0 then
          scaleY := 600 / vh
        else
          scaleY := 1 ;
      end ;

      vcx := RoundS( viewBox.Left + ( viewBox.Right  - viewBox.Left ) / 2 ) ;
      vcy := RoundS( viewBox.Top + ( viewBox.Bottom - viewBox.Top  ) / 2 ) ;

      if not IsStringEmpty( cx ) then
        vcx := readDouble(cx) ;
      if not IsStringEmpty( cy ) then
        vcy := readDouble(cy) ;

      if Pos( '%', cx ) >= StringFirst then
        vcx := 600* vcx/100 ;
      if Pos( '%', cy ) >= StringFirst then
        vcy := 600* vcy/100 ;

      centerPoint := PointF( vcx, vcy ) ;

    finally
      FreeObject( tkn ) ;
    end ;

    if scaleX > scaleY then
      scaleX := scaleY
    else
      scaleY := scaleX ;
  end ;

  procedure T_SVGParser.svgParseRect(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i       : Integer ;
    x, y,
    w, h,
    rx,ry,f : Double ;
    id      : String ;
    fx,fy   : Double ;
    first   : Boolean ;
  begin
    x  := 0 ;
    y  := 0 ;
    w  := 0 ;
    h  := 0 ;
    rx := 0 ;
    ry := 0 ;

    for i := 0 to _attribs.Length-1 do begin
      if      _attribs.GetLocalName( i ) = 'id'      then
        id := _attribs.GetValue(i)
      else if _attribs.GetLocalName( i ) = SVG_ELEM_RECT_X      then
        x := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_RECT_Y      then
        y := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_RECT_WIDTH  then
        w := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_RECT_HEIGHT then
        h := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_RECT_RX     then
        rx := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_RECT_RY     then
        ry := readDouble( _attribs.GetValue(i) )
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    if id = SVG_SHIELD_LABEL then begin
      labelBox := RectF( x, y, x+w, y+h ) ;
    end
    else
    if id = SVG_SHIELD_BOUNDRY then begin
      boundryBox := RectF( x, y, x+w, y+h ) ;
    end
    else begin
      svgPathPrepare ;
      if (ry = 0) then
        ry := rx ;
      if (rx = 0) and (ry=0) then begin
        svgPathAddPoint( x  , y   ) ;
        svgPathAddPoint( x+w, y   ) ;
        svgPathAddPoint( x+w, y+h ) ;
        svgPathAddPoint( x  , y+h ) ;
        svgPathAddPoint( x  , y   ) ;
      end
      else begin
        first := True ;
        f := Pi; // CW direction
        while f <= 1.5 * Pi do begin
          svgPathAddPoint(rx * Cos(f) + x + rx, ry * Sin(f) + y + ry); // Top Left
          if first then begin
            fx := rx * Cos(f) + x + rx ;
            fy := ry * Sin(f) + y + ry ;
            first := False ;
          end;
          f := f + 0.1 ;
        end;

        f := 1.5*Pi ;
        while f<=2*Pi do begin
          svgPathAddPoint( rx*Cos(f)+x+w-rx, ry*Sin(f)+y+ry ) ; //Top Right
          f := f + 0.1 ;
        end ;

        f := 0 ;
        while f<=0.5*Pi do begin
          svgPathAddPoint( rx*Cos(f)+x+w-rx, ry*Sin(f)+y+h-ry ) ; //Bottom Right
          f := f + 0.1 ;
        end ;

        f := 0.5*Pi ;
        while f<=Pi do begin
          svgPathAddPoint( rx*Cos(f)+x+rx, ry*Sin(f)+y+h-ry ) ;  //Bottom Left
          f := f + 0.1 ;
        end ;

        svgPathAddPoint( fx, fy ) ; // close the gap
      end ;

      svgPathCreate( SVG_ENTRY_SURFACE ) ;
    end;
  end ;

  procedure T_SVGParser.svgParseLine(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i      : Integer ;
    x1, y1,
    x2, y2 : Double ;
  begin
    x1  := 0 ;
    y1  := 0 ;
    x2  := 0 ;
    y2  := 0 ;

    for i := 0 to _attribs.Length-1 do begin
      if      _attribs.GetLocalName( i ) = SVG_ELEM_LINE_X1  then
        x1 := DotStrToFloat( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_LINE_Y1  then
        y1 := DotStrToFloat( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_LINE_X2  then
        x2 := DotStrToFloat( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_LINE_Y2  then
        y2 := DotStrToFloat( _attribs.GetValue(i) )
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    svgPathPrepare ;
    svgPathAddPoint( x1, y1 ) ;
    svgPathAddPoint( x2, y2 ) ;
    svgPathCreate( SVG_ENTRY_LINE ) ;
  end ;

  procedure T_SVGParser.svgParseCircle(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i       : Integer ;
    cx,cy,r : Double ;
  begin
    cx := 0 ;
    cy := 0 ;
    r  := 0 ;

    for i := 0 to _attribs.Length-1 do begin
      if      _attribs.GetLocalName( i ) = SVG_ELEM_CIRCLE_CX  then
        cx := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_CIRCLE_CY  then
        cy := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_CIRCLE_R   then
        r := readDouble( _attribs.GetValue(i) )
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    svgPathPrepare ;
    toEllipticArc( cx, cy, cx-r, cy, cx, cy + r, cx-r, cy, cx-r, cy ) ;
    svgPathCreate( SVG_ENTRY_SURFACE ) ;
  end  ;

  procedure T_SVGParser.svgParseEllipse(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i       : Integer ;
    cx,cy,
    rx,ry   : Double ;
  begin
    cx := 0 ;
    cy := 0 ;
    rx := 0 ;
    ry := 0 ;

    for i := 0 to _attribs.Length-1 do begin
      if      _attribs.GetLocalName( i ) = SVG_ELEM_ELLIPSE_CX  then
        cx := DotStrToFloat( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_ELLIPSE_CY  then
        cy := DotStrToFloat( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_ELLIPSE_RX   then
        rx := DotStrToFloat( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_ELLIPSE_RY   then
        ry := DotStrToFloat( _attribs.GetValue(i) )
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    svgPathPrepare ;
    toEllipticArc( cx, cy, cx-rx, cy, cx, cy + ry, cx-rx, cy, cx-rx, cy ) ;
    svgPathCreate( SVG_ENTRY_SURFACE ) ;
  end ;

  procedure T_SVGParser.svgParsePolyline(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i, j   : Integer ;
    spts   : String ;
    pts    : TGIS_DoubleArray ;
    cnt    : Integer ;
  begin
    for i := 0 to _attribs.Length-1 do begin
      if _attribs.GetLocalName( i ) = SVG_ELEM_POLY_POINTS  then
        spts := _attribs.GetValue(i)
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    svgPathPrepare ;

    readParams( spts, pts, cnt ) ;
    j := 0 ;
    while j < cnt-1 do begin
      svgPathAddPoint( pts[j], pts[j+1] );
      inc( j, 2 ) ;
    end ;

    svgPathCreate( SVG_ENTRY_LINE ) ;
  end ;

  procedure T_SVGParser.svgParsePolygon(
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i, j   : Integer ;
    spts   : String ;
    pts    : TGIS_DoubleArray ;
    cnt    : Integer ;
  begin
    for i := 0 to _attribs.Length-1 do begin
      if _attribs.GetLocalName( i ) = SVG_ELEM_POLY_POINTS  then
        spts := _attribs.GetValue(i)
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    svgPathPrepare ;

    readParams( spts, pts, cnt ) ;
    j := 0 ;
    while j < cnt-1 do begin
      svgPathAddPoint( pts[j], pts[j+1] );
      inc( j, 2 ) ;
    end ;

    svgPathCreate( SVG_ENTRY_SURFACE ) ;
  end ;

  procedure T_SVGParser.svgParseText   (
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i   : Integer ;
    x   : Double ;
    y   : Double ;
    rot : Double ;
    attr : T_SVGAttrib ;
    ta  : Integer ;
    fw  : Integer ;
    fi  : Integer ;
  begin
    x   := 0 ;
    y   := 0 ;
    rot := 0 ;

    for i := 0 to _attribs.Length-1 do begin
      if      _attribs.GetLocalName( i ) = SVG_ELEM_TEXT_X       then
        x := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_TEXT_Y       then
        y := readDouble( _attribs.GetValue(i) )
      else if _attribs.GetLocalName( i ) = SVG_ELEM_TEXT_ROTATE  then
        rot := readDouble( _attribs.GetValue(i) )
      else
        svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
    end ;

    svgPathPrepare ;
    svgPathAddPoint( x, y ) ;

    rot := rot * Pi / 180 ;
    text.Clear ;
    {$IFDEF OXYGENE}
      text.WriteDouble( rot ) ;
    {$ELSE}
      text.Write( rot, sizeOf(rot) ) ;
    {$ENDIF}
    attr := T_SVGAttrib( svgAttr.Peek ) ;
    if attr.fontSize = 0 then
      attr.fontSize := 10 ;
    {$IFDEF OXYGENE}
      text.WriteDouble( attr.fontSize ) ;
    {$ELSE}
      text.Write( attr.fontSize, sizeOf(Double) ) ;
    {$ENDIF}
    if attr.textAnchor = 'start' then
      ta := 1
    else if attr.textAnchor = 'middle' then
      ta := 2
    else if attr.textAnchor = 'end' then
      ta := 3
    else
      ta := 0 ;
    {$IFDEF OXYGENE}
    text.WriteInteger( ta ) ;
    {$ELSE}
    text.Write( ta, sizeOf(Integer) ) ;
    {$ENDIF}
    if (attr.fontWeight = 'bold') or (attr.fontWeight = 'bolder') then
      fw := 1
    else
      fw := 0 ;
    {$IFDEF OXYGENE}
    text.WriteInteger( fw ) ;
    {$ELSE}
    text.Write( fw, sizeOf(Integer) ) ;
    {$ENDIF}
    fi := 0 ;
    if not IsStringEmpty( attr.fontFamily ) then begin
      fi := fonts.IndexOf( attr.fontFamily ) ;
      if fi = -1 then
        fi := fonts.Add( attr.fontFamily ) ;
    end ;
    {$IFDEF OXYGENE}
    text.WriteInteger( fi ) ;
    {$ELSE}
    text.Write( fi, sizeOf(Integer) ) ;
    {$ENDIF}

  end ;

  procedure T_SVGParser.svgParsePath   (
    const _attribs : IVBSAXAttributes
  ) ;
  var
    i,len       : Integer ;
    d           : TStringBuilder ;
    nargs,
    rargs,
    idx         : Integer ;
    item        : TStringBuilder ;
    cmd         : String ;
    cpx,cpy     : Double ;
    cpx2, cpy2  : Double ;
    closed      : Boolean ;
    args        : TGIS_DoubleArray ;

    procedure getNextPathItem( var _off  : Integer ) ;
    var
      n_dot : Integer ;
    begin
      item.Length := 0 ;

      // Skip white spaces and commas
      while (_off<len) and ((ord(d[_off])<=32) or (d[_off]=',')) do
        inc( _off ) ;

      if (_off>=len) then exit ;

      {$IFDEF JAVA OR ISLAND}
        if (d[_off]='-') or (d[_off]='+') or
          CharInSet( d[_off], cNumericSet ) then begin
      {$ELSE}
        if (d[_off]='-') or (d[_off]='+') or
          CharInSet( d[_off], ['0'..'9','-','+','.','e','E'] ) then begin
      {$ENDIF}
        while (d[_off]='-') or ( d[_off]='+') do begin
          item.Append(d[_off]) ;
          inc( _off ) ;
        end ;

        n_dot := 0 ;
        {$IFDEF JAVA OR ISLAND}
          while (_off<len) and
                (((d[_off]<>'-') and (d[_off]<>'+')) or
                 ((d[_off-1]='e') or (d[_off-1]='E'))) and
                CharInSet( d[_off], cNumericSet ) do begin
        {$ELSE}
          while (_off<len) and
                (((d[_off]<>'-') and (d[_off]<>'+')) or
                 ((d[_off-1]='e') or (d[_off-1]='E'))) and
                CharInSet( d[_off], ['0'..'9','-','+','.','e','E'] ) do begin
        {$ENDIF}
          if d[_off] = '.' then
            inc( n_dot ) ;
          if n_dot > 1 then
            break ;
          item.Append(d[_off]) ;

          inc( _off ) ;
        end ;
      end
      else begin
        item.Append(d[_off]) ;
        inc( _off ) ;
      end ;
    end ;

  begin
    d := TStringBuilder.Create ;
    try
      for i := 0 to _attribs.Length-1 do begin
        if _attribs.GetLocalName( i ) = SVG_ELEM_PATH_D  then
          d.Append( _attribs.GetValue(i) )
        else
          svgParseAttr( _attribs.GetLocalName(i), _attribs.GetValue(i) ) ;
      end ;

      nargs  := 0 ;
      rargs  := 0 ;
      idx    := 0 ;
      len    := d.Length ;
      closed := False ;

      SetLength( args, 10 ) ;

      svgPathPrepare ;

      firstX := NaN ;
      firstY := NaN ;
      cpx  := NaN ;
      cpy  := NaN ;
      cpx2 := NaN ;
      cpy2 := NaN ;

      item := TStringBuilder.Create;
      try
        while idx < len do begin
          getNextPathItem( idx ) ;

          if item.Length = 0 then break ;

          {$IFDEF JAVA OR ISLAND}
            if CharInSet( item.Chars[0], cNumericSet ) then
          {$ELSE}
            if CharInSet( item.Chars[0], ['0'..'9','-','+','.','e','E'] ) then
          {$ENDIF}
          begin
            if (nargs < 10) then begin
              args[nargs] := DotStrToFloat( item.ToString ) ;
              inc( nargs ) ;
            end ;

            if ( nargs >= rargs ) then begin
              case cmd[StringFirst] of
                'm','M' : toLine         ( cpx, cpy, cpx2, cpy2, args, (cmd='m')  ) ;
                'l','L' : toLine         ( cpx, cpy, cpx2, cpy2, args, (cmd='l')  ) ;
                'h','H' : toHLine        ( cpx, cpy, cpx2, cpy2, args, (cmd='h')  ) ;
                'v','V' : toVLine        ( cpx, cpy, cpx2, cpy2, args, (cmd='v')  ) ;
                'a','A' : toEllipticalArc( cpx, cpy, cpx2, cpy2, args, (cmd='a')  ) ;
                'c','C' : toCubicBezier  ( cpx, cpy, cpx2, cpy2, args, (cmd='c')  ) ;
                's','S' : toCubicBezierS ( cpx, cpy, cpx2, cpy2, args, (cmd='s')  ) ;
                'q','Q' : toQuadBezier   ( cpx, cpy, cpx2, cpy2, args, (cmd='q')  ) ;
                't','T' : toQuadBezierS  ( cpx, cpy, cpx2, cpy2, args, (cmd='t')  ) ;
              else
                if nargs >=2 then begin
                  cpx := args[nargs-2] ;
                  cpy := args[nargs-1] ;
                end
              end ;
              nargs := 0 ;
            end ;
          end
          else begin
            cmd := item.ToString ;
            {$IFDEF OXYGENE}
              case LowCase( cmd[StringFirst] ) of
            {$ELSE}
              case LowerCase( cmd )[StringFirst] of
            {$ENDIF}
               'v', 'h'   : rargs := 1 ;
               'm','l','t': rargs := 2 ;
               'q','s'    : rargs := 4 ;
               'c'        : rargs := 6 ;
               'a'        : rargs := 7
            else            rargs := 0
            end ;

            if (cmd = 'm') or (cmd = 'M') then begin
              setFirst := True ;
              if nPoints > 0 then
                svgPathAddPart ;
              nargs  := 0 ;
              if cmd = 'M' then begin
                cpx := firstX ;
                cpy := firstY ;
              end ;
            end
            else if (cmd = 'z') or (cmd = 'Z') then begin
              closed := True ;
              svgPathClose ;
              cpx := firstX ;
              cpy := firstY ;
              nargs  := 0 ;
            end ;
          end ;
        end ;
      finally
        FreeObject( item ) ;
      end ;

      if nPoints > 0 then begin
        svgPathAddPart ;
        if nParts = 1 then begin
          if closed then
            svgPathCreate( SVG_ENTRY_SURFACE )
          else
            svgPathCreate( SVG_ENTRY_LINE )
        end
        else
          svgPathCreate( SVG_ENTRY_MULTISURFACE ) ;
      end ;
    finally
      FreeObject( d ) ;
    end ;
  end ;

  procedure T_SVGParser.Draw(
    const _canvas     : TGIS_RendererAbstract ;
    const _origin     : TPointF ;
    const _offset     : TPointF ;
    const _symSin     : Double ;
    const _symCos     : Double ;
    const _symRot     : Double ;
    const _symScale   : Double ;
    const _symXScale   : Double ;
    const _symColor1  : TGIS_Color ;
    const _symColor2  : TGIS_Color
  ) ;
  var
    i        : Integer ;
    pt       : TPointF ;
    ext      : TPoint  ;
    rct      : TRectF  ;
    rcti     : TRect   ;
    rot      : Double  ;
    fs       : Double  ;
    fntmlt   : Double  ;
    ta       : Integer ;
    fw       : Integer ;
    fi       : Integer ;
    htxt     : Integer ;
    len      : Integer ;
    draw_buf : TGIS_DrawBufF ;
    part_buf : TGIS_IntegerArray ;
    btext    : TBytes ;
    txt      : String ;
    pt_X     : Double ;
    pt_Y     : Double ;
    iwidth   : Integer ;
    iheight  : Integer ;
    twidth   : Integer ;
    {$IFDEF WINFORMS}
      height : Integer ;
    {$ENDIF}

      function realizeColor1( const _color : TGIS_Color ) : TGIS_Color ;
      begin
        if _color.ARGB = TGIS_Color.FromRGB( 0, 0, 1 ).ARGB then begin
          Result := _symColor1 ;
          if Result.ARGB = TGIS_Color.RenderColor.ARGB then
            Result := TGIS_Color.Black ;
        end
        else
        if _color.ARGB = TGIS_Color.FromRGB( 0, 0, 2 ).ARGB then begin
          Result := _symColor2 ;
          if Result.ARGB = TGIS_Color.RenderColor.ARGB then
            Result := TGIS_Color.DimGray ;
        end
        else
        if  _color.ARGB = TGIS_Color.Black.ARGB             then begin
          Result := _symColor1 ;
          if Result.ARGB = TGIS_Color.RenderColor.ARGB then
            Result := TGIS_Color.Black ;
        end
        else
          Result := _color ;
      end ;

      function realizeColor2( const _color : TGIS_Color ) : TGIS_Color ;
      begin
        if _color.ARGB = TGIS_Color.FromRGB( 0, 0, 1 ).ARGB then begin
          Result := _symColor1 ;
          if Result.ARGB = TGIS_Color.RenderColor.ARGB then
            Result := TGIS_Color.Black ;
        end
        else
        if _color.ARGB = TGIS_Color.FromRGB( 0, 0, 2 ).ARGB then begin
          Result := _symColor2 ;
          if Result.ARGB = TGIS_Color.RenderColor.ARGB then
            Result := TGIS_Color.DimGray ;
        end
        else
        if  _color.ARGB = TGIS_Color.Black.ARGB             then begin
          Result := _symColor2 ;
          if Result.ARGB = TGIS_Color.RenderColor.ARGB then
            Result := TGIS_Color.Black ;
        end
        else
          Result := _color ;
      end ;

  begin
    try
      iwidth  := RoundS( viewBox.Right  - viewBox.Left ) ;
      iheight := RoundS( viewBox.Bottom - viewBox.Top  ) ;

      metaStream.Position := 0 ;

      while (metaStream.Position < metaStream.Size) do begin
       {$IFDEF OXYGENE}
        T_SymbolSvgPrimitive(svgMeta).Read( metaStream ) ;
       {$ELSE}
        metaStream.Read( svgMeta^, sizeOf( T_SymbolSvgPrimitive ) ) ;
       {$ENDIF}

        {$IFDEF OXYGENE}
         with T_SymbolSvgPrimitive(svgMeta)  do begin
        {$ELSE}
         with T_SymbolSvgPrimitive(svgMeta^) do begin
        {$ENDIF}
          if Kind = SVG_ENTRY_END then break ;

          _canvas.CanvasBrush.Color := realizeColor1(
                                         TGIS_Color.FromARGB( BrushColor ) ) ;
          _canvas.CanvasBrush.Style := BrushStyle ;

          if PenWidth = 0 then
            _canvas.CanvasPen.Width := 0
          else
            _canvas.CanvasPen.Width := Max(
                                         1,
                                         RoundS( PenWidth / 1000.0 * _symScale )
                                       ) ;
          _canvas.CanvasPen.Style   := PenStyle ;
          _canvas.CanvasPen.Color   := TGIS_Color.FromARGB( PenColor ) ;

          if _canvas.CanvasPen.Style <> TGIS_PenStyle.Clear then
            _canvas.CanvasPen.Color := realizeColor2(
                                         TGIS_Color.FromARGB( PenColor ) ) ;

          if _canvas.CanvasBrush.Style <> TGIS_BrushStyle.Clear then
            _canvas.CanvasPen.Color := realizeColor1(
                                         TGIS_Color.FromARGB( PenColor ) ) ;

          _canvas.CanvasPen.LineCap  := LineCap ;
          _canvas.CanvasPen.LineJoin := LineJoin ;

          if ( Kind = SVG_ENTRY_SURFACE ) and //first_surface and
             (_canvas.CanvasBrush.Style <> TGIS_BrushStyle.Clear ) and
             (_canvas.CanvasPen.Style = TGIS_PenStyle.Clear ) then begin
            // some renderers do not draw the surface border
            // so it must be drawn as a line
            _canvas.CanvasPen.Style := TGIS_PenStyle.Solid ;
            _canvas.CanvasPen.Color := _canvas.CanvasBrush.Color ;
            _canvas.CanvasPen.Width := 1 ;
          end ;

          if Points > 0 then begin

            if ( Kind = SVG_ENTRY_LINE ) or ( Kind = SVG_ENTRY_SURFACE ) or
               ( Kind = SVG_ENTRY_MULTISURFACE ) then begin

              if Points > high( draw_buf ) then
                SetLength( draw_buf, Points ) ;

              for i := 0 to Points-1 do begin
                {$IFDEF OXYGENE}
                  metaStream.ReadDouble( pt_X ) ;
                  metaStream.ReadDouble( pt_Y ) ;
                {$ELSE}
                  metaStream.Read( pt_X, sizeOf(pt_X) ) ;
                  metaStream.Read( pt_Y, sizeOf(pt_Y) ) ;
                {$ENDIF}

                pt_X :=  ( pt_X - iwidth  / 2  ) ;
                pt_Y :=  ( pt_Y - iheight / 2  ) ;

                {$IFDEF GIS_NORECORDS}
                  pt := new TPointF(0,0) ;
                {$ENDIF}
                pt.X := ( ( pt_X * _symCos - pt_Y * _symSin )
                          * _symScale * _symXScale
                        ) + _origin.X + _offset.X  ;
                pt.Y := ( ( pt_X * _symSin + pt_Y * _symCos )
                          * _symScale
                        ) + _origin.Y + _offset.Y  ;

                draw_buf[ i ] := pt ;
              end ;

              if Kind = SVG_ENTRY_MULTISURFACE then begin
                {$IFDEF OXYGENE}
                  metaStream.ReadInteger( len ) ;
                {$ELSE}
                  metaStream.Read( len, sizeOf(Integer) ) ;
                {$ENDIF}
                SetLength( part_buf, len ) ;
                for i := 0 to len-1 do begin
                  {$IFDEF OXYGENE}
                    metaStream.ReadInteger( part_buf[i] ) ;
                  {$ELSE}
                    metaStream.Read( part_buf[i], sizeOf( Integer ) ) ;
                  {$ENDIF}
                end ;
              end ;

              if Visible = 0 then continue ;

              if Kind = SVG_ENTRY_LINE then begin
                if BrushStyle = TGIS_BrushStyle.Clear then
                  _canvas.CanvasDrawPolyLine( draw_buf, Points )
                else begin
                  // not closed polygon case
                  twidth := _canvas.CanvasPen.Width ;
                  _canvas.CanvasPen.Width  := 0 ;
                  _canvas.CanvasDrawPolygon( draw_buf, Points ) ;
                  _canvas.CanvasPen.Width   := twidth  ;
                  _canvas.CanvasDrawPolyLine( draw_buf, Points )
                end ;
              end
              else if Kind = SVG_ENTRY_SURFACE then begin
                if _canvas.CanvasBrush.Style = TGIS_BrushStyle.Clear then
                  _canvas.CanvasDrawPolyLine( draw_buf, Points )
                else
                  _canvas.CanvasDrawPolygon( draw_buf, Points )
              end
              else begin
                if _canvas.CanvasBrush.Style = TGIS_BrushStyle.Clear then
                  _canvas.CanvasDrawPolyLine( draw_buf, part_buf )
                else
                  _canvas.CanvasDrawPolygon( draw_buf, part_buf ) ;
              end
            end
            else begin
              assert( Kind = SVG_ENTRY_TEXT );
              _canvas.CanvasFont.Color  := realizeColor1(
                                             TGIS_Color.FromARGB( BrushColor ) ) ;
              _canvas.CanvasFont.Style  := GisGetEmptyFontStyle ;
              _canvas.CanvasBrush.Style := TGIS_BrushStyle.Clear ;
              // SVG_ENTRY_TEXT
              {$IFDEF OXYGENE}
                metaStream.ReadDouble( pt_X ) ;
                metaStream.ReadDouble( pt_Y ) ;
              {$ELSE}
                metaStream.Read( pt_X, sizeOf(pt_X) ) ;
                metaStream.Read( pt_Y, sizeOf(pt_Y) ) ;
              {$ENDIF}

              pt_X :=  ( pt_X - iwidth  / 2  ) ;
              pt_Y :=  ( pt_Y - iheight / 2  ) ;
              {$IFDEF GIS_NORECORDS}
                pt := new TPointF(0,0) ;
              {$ENDIF}
              pt.X := ( ( pt_X * _symCos - pt_Y * _symSin )
                        * _symScale * _symXScale
                      ) + _origin.X + _offset.X  ;
              pt.Y := ( ( pt_X * _symSin + pt_Y * _symCos )
                        * _symScale
                      ) + _origin.Y + _offset.Y  ;

              {$IFDEF OXYGENE}
                metaStream.ReadInteger( len ) ;
                metaStream.ReadDouble( rot ) ;
                metaStream.ReadDouble( fs  ) ;
                metaStream.ReadInteger( ta ) ;
                metaStream.ReadInteger( fw ) ;
                metaStream.ReadInteger( fi ) ;
              {$ELSE}
                metaStream.Read( len, sizeOf(Integer) ) ;
                metaStream.Read( rot, sizeOf(Double)  ) ;
                metaStream.Read( fs,  sizeOf(Double)  ) ;
                metaStream.Read( ta,  sizeOf(Integer)  ) ;
                metaStream.Read( fw,  sizeOf(Integer)  ) ;
                metaStream.Read( fi,  sizeOf(Integer)  ) ;
              {$ENDIF}

              len := len - 2*sizeOf(Double) - 3*sizeOf(Integer) ;
              if ( len > 0 ) then begin
                SetLength( btext, len ) ;
                {$IFDEF OXYGENE}
                  metaStream.ReadBuffer( btext, len ) ;
                {$ELSE}
                  metaStream.ReadBuffer( btext[0], len ) ;
                {$ENDIF}

                txt := TEncoding.UTF8.GetString( btext ) ;
                _canvas.CanvasFont.Name := 'Verdana' ;
                _canvas.CanvasFont.Size := 72 ;

                ext := _canvas.CanvasTextExtent( 'Wy' ) ;
                fntmlt := 72.0 / ext.Y ;
                _canvas.CanvasFont.Size := RoundS( fs * fntmlt * _symScale ) ;

                ext := _canvas.CanvasTextExtent( 'Wy' ) ;
                htxt := ext.Y ;

                if fw = 1 then
                  _canvas.CanvasFont.Style := [TGIS_FontStyle.Bold]
                else
                  _canvas.CanvasFont.Style := [] ;

                if fi < fonts.Count then
                  _canvas.CanvasFont.Name := fonts[fi] ;

                ext := _canvas.CanvasTextExtent( txt ) ;

                if ta = 2 then
                  pt.X := pt.X - ext.X / 2
                else if ta = 3 then
                  pt.X := pt.X - ext.X ;

                //RectEx
                rct := RectF( pt.X, pt.Y - htxt, pt.X + 32000, pt.Y + 32000 ) ;
                rot := rot + _symRot ;

                if Visible = 0 then continue ;

                if Abs( _canvas.CanvasFont.Size ) > 1 then begin
                  if rot <> 0 then begin
                    rct := RectF( 0, - htxt, rct.Right - rct.Left, rct.Bottom - rct.Top ) ;
                    _canvas.CanvasSetTransformation( rot, RoundS( pt.X ), RoundS( pt.Y ) )  ;
                  end ;
                  rcti := Rect( RoundS( rct.Left   ), RoundS( rct.Top    ),
                                RoundS( rct.Right  ), RoundS( rct.Bottom )
                              ) ; // make it RectF some day
                  _canvas.CanvasDrawText( rcti, txt ) ;
                  if rot <> 0 then
                    _canvas.CanvasClearTransformation ;
                end ;
              end ;
            end ;
          end ;
        end ;
      end ;
    except
    end ;
  end ;

//==============================================================================
//  T_SAXHandlerSVG
//==============================================================================

  constructor T_SAXHandlerSVG.Create(
    const _svg  : T_SVGParser
  ) ;
  begin
    inherited Create ;

    FSVG := _svg ;
  end ;

  procedure T_SAXHandlerSVG.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  begin
    inherited ;

    if _lname = SVG_ELEM_DEFS then
      inc( FSVG.defsFlag ) ;

    if FSVG.defsFlag = 0 then begin

      if _lname = SVG_ELEM_SVG      then begin
        FSVG.svgParseSvg( _attribs ) ;
      end
      else if _lname = SVG_ELEM_G        then begin
        FSVG.svgPushAttr ;
        FSVG.svgParseAttribs( _attribs ) ;
      end
      else if _lname = SVG_ELEM_RECT     then begin
        FSVG.svgPushAttr ;
        FSVG.svgParseRect( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_CIRCLE   then begin
        FSVG.svgPushAttr ;
        FSVG.svgParseCircle( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_ELLIPSE  then begin
        FSVG.svgPushAttr ;
        FSVG.svgParseEllipse( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_LINE     then begin
        FSVG.svgPushAttr ;
        FSVG.svgParseLine( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_POLYLINE then begin
        FSVG.svgPushAttr ;
        FSVG.svgParsePolyline( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_POLYGON  then begin
        FSVG.svgPushAttr ;
        FSVG.svgParsePolygon( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_PATH     then begin
        FSVG.svgPushAttr ;
        FSVG.svgParsePath( _attribs ) ;
        FSVG.svgPopAttr ;
      end
      else if _lname = SVG_ELEM_TEXT     then begin
        FSVG.textFlag := True ;
        FSVG.svgPushAttr ;
        FSVG.svgParseText( _attribs ) ;
      end
      else if _lname = SVG_ELEM_TSPAN    then begin
        FSVG.svgParseText( _attribs ) ;
      end
    end;
  end ;

  procedure T_SAXHandlerSVG.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  begin
    inherited ;

    if      _lname = SVG_ELEM_G    then
      FSVG.svgPopAttr
    else if _lname = SVG_ELEM_SVG  then
      FSVG.svgPathEnd
    else if _lname = SVG_ELEM_DEFS then
      dec( FSVG.defsFlag )
    else if _lname = SVG_ELEM_TEXT then begin
      if FSVG.textFlag then begin
        FSVG.svgPathCreate( SVG_ENTRY_TEXT ) ;
        FSVG.svgPopAttr ;
        FSVG.textFlag := False ;
      end ;
    end ;
  end ;

  procedure T_SAXHandlerSVG.Characters(
    const _chars   : String
  ) ;
  var
    buf : TBytes ;
    str : String ;
  begin
    inherited ;

    if FSVG.textFlag then begin
      str := Trim( RemoveWhitesFast( _chars ) ) ;
      if not IsStringEmpty( str ) then begin
        buf := TEncoding.UTF8.GetBytes( _chars ) ;
        {$IFDEF OXYGENE}
          FSVG.text.WriteBuffer( buf, length( buf ) ) ;
        {$ELSE}
          FSVG.text.WriteBuffer( buf[0], length( buf ) ) ;
        {$ENDIF}
      end ;
    end ;
  end ;

  procedure T_SAXHandlerSVG.FatalError(
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
//  TGIS_FileSVG
//==============================================================================

  constructor TGIS_FileSVG.Create ;
  begin
    inherited ;

    oSvg := T_SVGParser.Create ;
  end ;

  procedure TGIS_FileSVG.doDestroy ;
  begin
    FreeObject( oSvg ) ;
    inherited ;
  end ;

  function TGIS_FileSVG.fget_ViewBox : TRectF ;
  begin
    Result := T_SVGParser( oSvg ).viewBox ;
  end ;

  function TGIS_FileSVG.fget_CenterPoint : TPointF ;
  begin
    Result := T_SVGParser( oSvg ).centerPoint ;
  end ;

  function TGIS_FileSVG.fget_LabelBox : TRectF ;
  begin
    Result := T_SVGParser( oSvg ).labelBox ;
  end ;

  function TGIS_FileSVG.fget_BoundryBox : TRectF ;
  begin
    Result := T_SVGParser( oSvg ).boundryBox ;
  end ;

  procedure TGIS_FileSVG.Load(
    const _path       : String  ;
    const _stream     : TStream
  ) ;
  var
    oSAX : T_SAXHandlerSVG ;
    ext  : String ;
    fstream  : TGIS_FileStream ;
    stm      : TMemoryStream ;
    {$IFNDEF JAVA}
      {$IFNDEF BLAZOR}
        dcstream : TZDecompressionStream ;
      {$ENDIF}
    {$ELSE}
      siz     : Integer                         ;
      bytes   : array of Byte                   ;
      buf     : array of Byte                   ;
      i       : Integer                         ;
      strm    : TGIS_BaseStream                 ;
      gzip    : java.util.zip.GZIPInputStream   ;
    {$ENDIF}
  begin
    oSAX := T_SAXHandlerSVG.Create( T_SVGParser( oSvg ) ) ;
    try
      ext := UpperCase( GetFileExt( _path ) ) ;
      if ( not IsStringEmpty( _path ) ) and SafeFileExists( _path ) then begin
        if ext = '.SVG' then
          oSAX.LoadFromFile( _path )
        else if ext = '.SVGZ' then begin
          fstream  := TGIS_FileStream.Create( _path,
                                              fmOpenRead or
                                              fmShareDenyWrite
                                            ) ;
          {$IFNDEF JAVA}
            {$IFNDEF BLAZOR}
              try
                dcstream := TZDecompressionStream.Create( fstream, 15 + 16 ) ;
                stm := TMemoryStream.Create ;
                try
                  stm.CopyFrom( dcstream, dcstream.Size ) ;
                  oSAX.LoadFromStream( stm ) ;
                finally
                  FreeObject( stm ) ;
                end ;
              finally
                FreeObject( dcstream ) ;
                FreeObject( fstream  ) ;
              end ;
            {$ENDIF}
          {$ELSE}
            try
              strm := new TGIS_BaseStream(fstream) ;
              buf := new array of Byte(strm.Size);
              bytes := new array of Byte( strm.Size );
              strm.ReadBuffer( bytes, strm.Size ) ;

              gzip := new java.util.zip.GZIPInputStream( new java.io.ByteArrayInputStream( bytes ) ) ;
              stm := new TMemoryStream() ;
              repeat
                i := gzip.read( buf ) ;
                if i > 0 then
                  stm.write(buf, 0, i) ;
              until i <= 0;
              stm.Position := 0;
              oSAX.LoadFromStream(stm);
            finally
              gzip.close ;
              FreeObject( strm ) ;
              FreeObject( fstream  ) ;
            end;
          {$ENDIF}
        end ;
      end
      else if assigned( _stream ) then begin
        if ext = '.SVGZ' then begin
          fstream  := TGIS_FileStream.Create( _path,
                                              fmOpenRead or
                                              fmShareDenyWrite
                                            ) ;
          {$IFNDEF JAVA}
            {$IFNDEF BLAZOR}
              try
                dcstream := TZDecompressionStream.Create( fstream, 15 + 16 ) ;
                stm := TMemoryStream.Create ;
                try
                  stm.CopyFrom( dcstream, dcstream.Size ) ;
                  oSAX.LoadFromStream( stm ) ;
                finally
                  FreeObject( stm ) ;
                end ;
              finally
                FreeObject( dcstream ) ;
                FreeObject( fstream  ) ;
              end ;
            {$ENDIF}
          {$ELSE}
            try
              strm := new TGIS_BaseStream(fstream) ;

              buf := new array of Byte(strm.Size);

              strm.Position := 15 + 16 ;

              strm.ReadInteger( siz ) ;

              bytes := new array of Byte( siz );
              strm.ReadBuffer( bytes, siz ) ;

              gzip := new java.util.zip.GZIPInputStream( new java.io.ByteArrayInputStream( bytes ) ) ;
              stm := new TMemoryStream() ;

              repeat
                i := gzip.read( buf ) ;
                if i > 0 then
                  stm.write(buf, 0, i) ;
              until i <= 0;

              stm.Position := 0;

              oSAX.LoadFromStream(stm);

            finally
              gzip.close ;
              FreeObject( fstream  ) ;
              FreeObject( strm ) ;
            end;
          {$ENDIF}
        end
        else
          oSAX.LoadFromStream( _stream ) ;
      end ;
    finally
      FreeObject( oSAX ) ;
    end ;
  end ;

  procedure TGIS_FileSVG.Draw(
    const _canvas     : TGIS_RendererAbstract ;
    const _origin     : TPoint ;
    const _offset     : TPoint ;
    const _symSin     : Double ;
    const _symCos     : Double ;
    const _symRot     : Double ;
    const _symScale   : Double ;
    const _symScaleX  : Double ;
    const _symColor1  : TGIS_Color ;
    const _symColor2  : TGIS_Color
  ) ;
  begin
    T_SVGParser( oSvg ).Draw( _canvas,
                              PointF( _origin.X, _origin.Y ),
                              PointF( _offset.X, _offset.Y ),
                              _symSin, _symCos, _symRot,
                              _symScale, _symScaleX,
                              _symColor1, _symColor2
                             ) ;
  end ;

  procedure TGIS_FileSVG.Draw(
    const _canvas     : TGIS_RendererAbstract ;
    const _origin     : TPointF ;
    const _offset     : TPointF ;
    const _symSin     : Double  ;
    const _symCos     : Double  ;
    const _symRot     : Double  ;
    const _symScale   : Double  ;
    const _symScaleX  : Double  ;
    const _symColor1  : TGIS_Color ;
    const _symColor2  : TGIS_Color
  ) ;
  begin
    T_SVGParser( oSvg ).Draw( _canvas,
                              _origin,
                              _offset,
                              _symSin, _symCos, _symRot,
                              _symScale, _symScaleX,
                              _symColor1, _symColor2
                             ) ;
  end ;

//==================================== END =====================================
end.
