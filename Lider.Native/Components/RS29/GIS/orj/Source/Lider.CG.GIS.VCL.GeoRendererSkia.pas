//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
//    ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Skia based renderer.
}

unit VCL.GisRendererSkia ;
{$HPPEMIT '#pragma link "VCL.GisRendererSkia"'}

interface

{$INCLUDE GisInclude.inc}

// Skia is supported on RAD Studio 12 by default.
// For previous versions download: https://skia4delphi.org/
// and uncomment define below


{$IFDEF LEVEL_RX12_VCL}
  {$DEFINE GIS_SKIA_VERSION_6}
  {$DEFINE GIS_SKIA_SUPPORTED}
{$ELSE}
  // {$DEFINE GIS_SKIA_VERSION_6}
  // {$DEFINE GIS_SKIA_SUPPORTED}
{$ENDIF}

{$IFNDEF GIS_SKIA_SUPPORTED}

implementation

{$ELSE}

uses
  Winapi.Windows,
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Math.Vectors,

  {$IFDEF GIS_SKIA_VERSION_6}
    System.Skia.API,
    System.Skia,
    VCL.Skia,
  {$ELSE}
    Skia,
    Skia.VCL,
  {$ENDIF}

  VCL.Graphics,

  GisRtl,
  GisTypes,
  GisTypesUI,
  GisFunctions,
  GisResource,
  GisParams,
  GisInterfaces,
  GisSymbol,
  GisLayerVector,
  GisRendererAbstract,
  VCL.GisRendererSkia.Common,
  VCL.GisRenderer;


type
  {#gendoc:hide}
  TGIS_PaintSkia = class
    Canvas : ISkCanvas ;
    Surface : ISkSurface ;
  end ;

  /// <summary>
  ///   Skia renderer for VCL.
  /// </summary>
  TGIS_RendererVclSkia = class( TGIS_RendererVclAbstract )
    private
      penCache    : TObject ;
      brushCache  : TObject ;
    private
      /// <summary>
      ///   Map canvas encapsulation.
      /// </summary>
      oCanvas            : TObject ;

      /// <summary>
      ///   Selection canvas encapsulation.
      /// </summary>
      oSelectionCanvas   : TObject ;

      /// <summary>
      ///   Charts canvas encapsulation.
      /// </summary>
      oChartsCanvas      : TObject ;

      /// <summary>
      ///   Labels canvas encapsulation.
      /// </summary>
      oLabelsCanvas      : TObject ;

      /// <summary>
      ///   Edit canvas encapsulation.
      /// </summary>
      oEditCanvas        : TObject ;

      /// <summary>
      ///   Transparency canvas encapsulation.
      /// </summary>
      oTransparentCanvas : TObject ;

      /// <summary>
      ///   Selection color with transparency altered for topmost layers.
      /// </summary>
      colorSelection     : TGIS_Color ;

      /// <summary>
      ///   Reference to current canvas encapsulation.
      /// </summary>
      FCanvas            : TObject ;

      /// <summary>
      ///   Stored canvas for PrapreDraw*
      /// </summary>
      prevCanvas         : TObject ;


      /// <summary>
      ///   Stored transformation.
      /// </summary>
      storedTransform : TMatrix ;

    private
      procedure prepareSelectionCanvas
                                 ( _transparently : Boolean ;
                                   _useBaseMap    : Boolean ;
                                   _shp           : TGIS_Shape
                                 ) ;
      procedure prepareChartsCanvas ;
      procedure prepareLabelsCanvas ;

    private
      inEdit        : Boolean ;
      iTolerance    : Integer ;
      iToleranceSel : Integer ;
      sourceShape   : TObject ;
      flashed       : Boolean ;
      pextra        : Boolean ;

    protected
      function  fget_BitmapFactory : TGIS_BitmapFactory ; override ;
      function  fget_CanvasPen     : TGIS_Pen ; override;
      procedure fset_CanvasPen     ( const _value : TGIS_Pen
                                   ) ; override;
      function  fget_CanvasBrush   : TGIS_Brush ; override;
      procedure fset_CanvasBrush   ( const _value : TGIS_Brush
                                   ) ; override;
      function  fget_CanvasFont    : TGIS_Font ; override;
      procedure fset_CanvasFont    ( const _value : TGIS_Font
                                   ) ; override;
      function  fget_Info          : String ; override;

    private

      /// <summary>
      ///   Get left-up corner of the original shape's projected extent.
      /// </summary>
      function  getShapeOrigin   : TPoint ;

      function  prepareStrokeStyle
                                 ( const _style : TGIS_PenStyle ;
                                   const _width : Integer
                                 ) : ISkPathEffect ;
      function  prepareDashStyle
                                 ( const _dash  : TGIS_DashArray ;
                                   const _width : Integer
                                 ) : ISkPathEffect ;
      function  preparePatternFill
                                 ( const _pattern       : TGIS_BrushStyle ;
                                   const _color         : TGIS_Color
                                 ) : ISkImage ;

      procedure preparePen       ( const _canvas        : TObject ;
                                   const _color         : TGIS_Color      ;
                                   const _style         : TGIS_PenStyle   ;
                                   const _bitmap        : TGIS_Bitmap     ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _origin        : TPoint          ;
                                   const _cap           : TGIS_LineCap    ;
                                   const _join          : TGIS_LineJoin   ;
                                   const _width         : Integer
                                 ) ; overload ;
      procedure preparePen       ( const _canvas        : TObject ;
                                   const _color         : TGIS_Color      ;
                                   const _style         : TGIS_PenStyle   ;
                                   const _bitmap        : TGIS_Bitmap     ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _origin        : TPoint          ;
                                   const _cap           : TGIS_LineCap    ;
                                   const _join          : TGIS_LineJoin   ;
                                   const _dash          : TGIS_DashArray  ;
                                   const _width         : Integer
                                 ) ; overload ;
      procedure prepareBrush     ( const _canvas        : TObject ;
                                   const _color         : TGIS_Color  ;
                                   const _bitmap        : TGIS_Bitmap ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _origin        : TPoint
                                 ) ;
      procedure prepareFont      ( const _canvas        : TObject     ;
                                   const _name          : String      ;
                                   const _size          : Integer     ;
                                   const _style         : TGIS_FontStyles ;
                                   const _color         : TGIS_Color
                                 ) ;

      /// <summary>
      ///   Draw an ellipse on a given canvas.
      /// </summary>
      /// <param name="_x1">
      ///   x coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_y1">
      ///   y coordinate of the upper-left corner of the bounding rectangle
      /// </param>
      /// <param name="_x2">
      ///   x coordinate of the bottom-right corner of the bounding rectangle
      /// </param>
      /// <param name="_y2">
      ///   y coordinate of the bottom-right corner of the bounding rectangle
      /// </param>
      /// <param name="_canvas">
      ///   canvas to be drawn on
      /// </param>
      procedure drawEllipse      ( const _x1            : Integer             ;
                                   const _y1            : Integer             ;
                                   const _x2            : Integer             ;
                                   const _y2            : Integer             ;
                                   const _canvas        : TObject
                                 ) ;

      procedure drawPolyline     ( const _path          : ISkPath     ;
                                   const _shp           : TGIS_Shape
                                 ) ;

      procedure drawPolyPolygon  ( const _path          : ISkPath     ;
                                   const _shp           : TGIS_Shape
                                 ) ;

      procedure drawMarker       ( const _pt            : TPoint      ;
                                   const _style         : TGIS_MarkerStyle ;
                                   const _size          : Integer     ;
                                   const _shp           : TGIS_Shape  ;
                                   const _asselected    : Boolean
                                 ) ;
      /// <summary>
      ///   Render a point shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape to render
      /// </param>
      /// <param name="_source">
      ///   source shape ( not truncated )
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shape selection is rendered
      /// </param>
      procedure doShapePoint     ( const _shp           : TGIS_ShapePoint ;
                                   const _source        : TGIS_ShapePoint ;
                                   const _selectionOnly : Boolean
                                 ) ;

      /// <summary>
      ///   Render a multi point shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape to render
      /// </param>
      /// <param name="_source">
      ///   source shape ( not truncated )
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shapeselection is rendered
      /// </param>
      procedure doShapeMultiPoint( const _shp           : TGIS_ShapeMultiPoint ;
                                   const _source        : TGIS_ShapeMultiPoint ;
                                   const _selectionOnly : Boolean
                                 ) ;

      /// <summary>
      ///   Render a line shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape to render
      /// </param>
      /// <param name="_source">
      ///   source shape ( not truncated )
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shape selection is rendered
      /// </param>
      /// <param name="_outlineMode">
      ///   outline drawin mode
      /// </param>
      procedure doShapeLine      ( const _shp           : TGIS_ShapeArc ;
                                   const _source        : TGIS_ShapeArc ;
                                   const _selectionOnly : Boolean ;
                                   const _outlineMode   : TGIS_RendererMultipassMode
                                 ) ;

      /// <summary>
      ///   Render a polygon shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape to render
      /// </param>
      /// <param name="_source">
      ///   source shape ( not truncated )
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shapeselection is rendered
      /// </param>
      /// <param name="_part">
      ///   if -1 then all parts will be drawn
      ///   else part to draw
      /// </param>
      procedure doShapePolygon   ( const _shp           : TGIS_ShapePolygon ;
                                   const _source        : TGIS_ShapePolygon ;
                                   const _selectionOnly : Boolean ;
                                   const _part          : Integer
                                 ) ;

      /// <summary>
      ///   Render a multi patch shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape to render
      /// </param>
      /// <param name="_source">
      ///   source shape ( not truncated )
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shapeselection is rendered
      /// </param>
      procedure doShapeMultiPatch( const _shp           : TGIS_ShapePolygon ;
                                   const _source        : TGIS_ShapePolygon ;
                                   const _selectionOnly : Boolean
                                 ) ;

      /// <summary>
      ///   Render a polygon shape.
      /// </summary>
      /// <param name="_shp">
      ///   shape to render
      /// </param>
      /// <param name="_source">
      ///   source shape ( not truncated )
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shapeselection is rendered
      /// </param>
      /// <param name="_outlineMode">
      ///   outline drawin mode
      /// </param>
      procedure doShapeComplex   ( const _shp           : TGIS_ShapeComplex ;
                                   const _source        : TGIS_ShapeComplex ;
                                   const _selectionOnly : Boolean ;
                                   const _outlineMode   : TGIS_RendererMultipassMode
                                 ) ;

      procedure doLabelPoint     ( const _shp           : TGIS_Shape ;
                                   const _savePoints    : Boolean ;
                                   var   _points        : TGIS_DrawBuf
                                 ) ;

      procedure doLabelArc       ( const _shp           : TGIS_ShapeArc ;
                                   const _savePoints    : Boolean ;
                                   var   _points        : TGIS_DrawBuf
                                 ) ;

      procedure doChart          ( const _shp           : TGIS_Shape
                                 ) ;

      procedure drawEditingLines ( const _shp           : TGIS_Shape
                                 ) ;

      procedure drawEditingPointMarkers
                                 ( const _shp           : TGIS_Shape
                                 ) ;

      procedure drawEditingPoints( const _context       : TObject ;
                                   const _shp           : TGIS_Shape
                                 ) ;

      procedure drawEditingEdgeLengths
                                 ( const _shp           : TGIS_Shape
                                 ) ;

    protected
      procedure doDestroy ; override ;

    private
      useOpenGL : Boolean ;

    public

      /// <inheritdoc/>
      constructor Create             ; override ;

      /// <inheritdoc/>
      function    CreateInstance     : TGIS_RendererAbstract ; override;

      /// <inheritdoc/>
      procedure   CreateContext      ( const _parent    : IGIS_ViewerParent ;
                                       const _viewer    : IGIS_Viewer ;
                                       const _context   : TGIS_RendererContext ;
                                       const _shift     : TPoint  ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer
                                     ) ; overload; override;

      /// <inheritdoc/>
      procedure   CreateContext      ( const _parent    : IGIS_ViewerParent ;
                                       const _viewer    : IGIS_Viewer ;
                                       const _context   : TGIS_RendererContext ;
                                       const _shift     : TPoint  ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer ;
                                       const _tilerect  : TRect
                                     ) ; overload; override;

      /// <inheritdoc/>
      procedure   RestoreContext     ; override;

      /// <inheritdoc/>
      procedure   ReleaseContext     ; override;

      /// <inheritdoc/>
      procedure   PrepareHourglassContext ; override;

      /// <inheritdoc/>
      procedure   AfterDraw          ; override;

      /// <inheritdoc/>
      procedure   PrepareDrawCharts  ; override;

      /// <inheritdoc/>
      procedure   AfterDrawCharts    ; override;

      /// <inheritdoc/>
      procedure   PrepareDrawLabels  ; override;

      /// <inheritdoc/>
      procedure   AfterDrawLabels    ; override;

      /// <inheritdoc/>
      procedure   LockTransparent    ( const _transparency : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   UnlockTransparent  ; override;

      /// <inheritdoc/>
      procedure   RenderShape        ( const _shp      : TObject ;
                                       const _selectionOnly
                                                       : Boolean = False ;
                                       const _outlineMode
                                                       : TGIS_RendererMultipassMode =
                                                         TGIS_RendererMultipassMode.Single
                                     ) ; overload; override;

      /// <inheritdoc/>
      procedure   RenderShape        ( const _shp      : TObject ;
                                       const _source   : TObject ;
                                       const _selectionOnly
                                                       : Boolean = False ;
                                       const _outlineMode
                                                       : TGIS_RendererMultipassMode =
                                                         TGIS_RendererMultipassMode.Single
                                     ) ; overload; override;

      /// <inheritdoc/>
      procedure   RenderLabel        ( const _shp      : TObject
                                     ) ; overload ; override ;

      /// <inheritdoc/>
      procedure   RenderLabel        ( const _shp      : TObject ;
                                       var   _points   : TGIS_DrawBuf
                                     ) ; overload ; override ;

      /// <inheritdoc/>
      procedure   RenderChart        ( const _shp      : TObject
                                     ) ; override;

      /// <inheritdoc/>
      procedure   RenderShapeFlashed ( const _shp      : TObject
                                     ) ; override;

      /// <inheritdoc/>
      function    RenderBitmapBegin  : TObject ; override;

      /// <inheritdoc/>
      procedure   RenderBitmapEnd    ( const _handle   : TObject
                                     ) ; override;

      /// <inheritdoc/>
      procedure   RenderBitmap       ( const _handle   : TObject           ;
                                       const _bmp      : TGIS_Pixels       ;
                                       const _size     : TPoint            ;
                                       const _dst      : TRect             ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) ; override;

      /// <inheritdoc/>
      procedure   RenderBitmap       ( const _handle   : TObject           ;
                                       const _bmp      : TGIS_Bitmap       ;
                                       const _dst      : TRect             ;
                                       const _antialias: Boolean
                                     ) ; override;

      /// <inheritdoc/>
      function    PrepareBitmapCache ( const _bmp      : TGIS_Pixels      ;
                                       const _extent   : TGIS_Extent      ;
                                       const _size     : TPoint           ;
                                       const _serial   : Integer          ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) : TGIS_RendererAbstractCache ;
                                     override;

       /// <inheritdoc/>
      procedure   RenderBitmapCache  ( const _handle   : TObject          ;
                                       const _cache    : TGIS_RendererAbstractCache ;
                                       const _dst      : TRect
                                     ) ; override;

      /// <inheritdoc/>
      procedure   RenderEditor       ( const _context  : TObject
                                     ) ; override;

      /// <inheritdoc/>
      procedure   PaintExtra         ( const _sender   : TObject ;
                                       const _context  : TObject ;
                                       const _event    : TGIS_RendererEvent
                                     ) ; override ;

      /// <inheritdoc/>
      procedure   PaintExtra         ( const _sender   : TObject ;
                                       const _context  : TObject ;
                                       const _event    : TGIS_PaintEvent
                                     ) ; override ;

      /// <inheritdoc/>
      procedure   Update             ; override;

      /// <inheritdoc/>
      procedure   Flush              ; override;

      /// <inheritdoc/>
      function FriendlyName          : String ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      function  ViewerCreateWndPaint ( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _canvas       : TObject
                                     ) : TObject ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerFreeWndPaint   ( var   _paint        : TObject
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerFlushToWndPaint( const _localCanvas  : TObject ;
                                       const _canvas       : TObject
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerDrawBackground ( const _canvas       : TObject ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _color        : TGIS_Color
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      function ViewerCreateTemporaryPaint
                                     ( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _context      : TObject
                                     ) : TObject ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      function ViewerCreateTemporaryPaint
                                     ( const _context      : TGIS_Bitmap
                                     ) : TObject ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerFreeTemporaryPaint
                                     ( var   _paint        : TObject
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerFreeTemporaryPaint
                                     ( var   _paint        : TObject ;
                                       const _bitmap       : TGIS_Bitmap
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerClearTemporaryPaint
                                     ( const _paint        : TObject
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerFlushTemporaryPaint
                                     ( const _canvas       : TObject ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerFlushTemporaryPaint
                                     ( const _canvas       : TObject ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _paint        : TObject ;
                                       const _fullCache    : TGIS_Bitmap ;
                                       const _rect         : TRect
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerStretchBitmapFast
                                     ( const _src          : TObject ;
                                       const _dst          : TObject ;
                                       const _rect         : TRect
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerStretchBitmapFast
                                     ( const _src          : TObject ;
                                       const _dst          : TObject ;
                                       const _rect         : TRect   ;
                                       const _transparency : Integer
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerBlendBitmaps   ( const _src          : TObject ;
                                       const _dst          : TObject ;
                                       const _transparency : Integer ;
                                       const _merge_alpha  : Boolean
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerBlendLabelBitmaps
                                     ( const _src          : TObject ;
                                       const _dst          : TObject ;
                                       const _merge_alpha  : Boolean
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      function  ViewerCreateFullCache( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _paint        : TObject ;
                                       const _addObj       : TObject
                                     ) : TGIS_Bitmap ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerDrawCache      ( const _cache        : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerDrawCache      ( const _cache        : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect   ;
                                       const _transparency : Integer
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerDrawProgressBitmap
                                     ( const _bitmap       : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerDrawZoomingRect( const _canvas       : TObject ;
                                       const _x            : Integer ;
                                       const _y            : Integer ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _color        : TGIS_Color
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ViewerDrawDraggingTrack
                                     ( const _canvas       : TObject ;
                                       const _x1           : Integer ;
                                       const _y1           : Integer ;
                                       const _x2           : Integer ;
                                       const _y2           : Integer ;
                                       const _color        : TGIS_Color
                                     ) ; override ;

      {#gendoc:hide}
      /// <inheritdoc/>
      procedure ControlDrawTransparent
                                     ( const _context      : TObject ;
                                       const _bitmap       : TObject ;
                                       const _x            : Integer ;
                                       const _y            : Integer
                                     ) ; override ;

    // lo-level API
    public
      /// <inheritdoc/>
      /// <returns>
      ///   Canvas object.
      /// </returns>
      function    CanvasNative       : TObject ; override;

      /// <inheritdoc/>
      procedure   CanvasFontMetrics  ( var   _break_char  : Char    ;
                                       var   _height      : Integer ;
                                       var   _ascent      : Integer ;
                                       var   _true_type   : Boolean
                                     ) ; override;

      /// <inheritdoc/>
      function    CanvasTextBaseline ( const _text        : String
                                     ) : Single ; override;

      /// <inheritdoc/>
      function    CanvasTextExtent   ( const _text        : String
                                     ) : TPoint ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawText     ( const _rect        : TRect   ;
                                       const _text        : String
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawLine     ( const _x1          : Integer ;
                                       const _y1          : Integer ;
                                       const _x2          : Integer ;
                                       const _y2          : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points      : TGIS_DrawBuf
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points      : TGIS_DrawBufF
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points      : TGIS_DrawBuf ;
                                       const _count       : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points      : TGIS_DrawBufF ;
                                       const _count       : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points      : TGIS_DrawBuf ;
                                       const _parts       : TGIS_IntegerArray
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points      : TGIS_DrawBufF ;
                                       const _parts       : TGIS_IntegerArray
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawRectangle( const _rect        : TRect
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawEllipse  ( const _x           : Integer ;
                                       const _y           : Integer ;
                                       const _width       : Integer ;
                                       const _height      : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPie      ( const _angle_0     : Double  ;
                                       const _angle_1     : Double  ;
                                       const _radius      : Integer ;
                                       const _origin_x    : Integer ;
                                       const _origin_y    : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure CanvasDrawArc       ( const _x           : Integer ;
                                      const _y           : Integer ;
                                      const _width       : Integer ;
                                      const _height      : Integer ;
                                      const _startX      : Integer ;
                                      const _startY      : Integer ;
                                      const _endX        : Integer ;
                                      const _endY        : Integer
                                    ) ; override;

      /// <inheritdoc/>
      procedure CanvasDrawArc       ( const _x          : Integer ;
                                      const _y          : Integer ;
                                      const _radius     : Integer ;
                                      const _startAngle : Single ;
                                      const _sweepAngle : Single
                                    ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points      : TGIS_DrawBuf
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points      : TGIS_DrawBufF
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points      : TGIS_DrawBuf ;
                                       const _count       : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points      : TGIS_DrawBufF ;
                                       const _count       : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points      : TGIS_DrawBuf ;
                                       const _parts       : TGIS_IntegerArray
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points      : TGIS_DrawBufF ;
                                       const _parts       : TGIS_IntegerArray
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawBitmap   ( const _bmp      : TGIS_Pixels ;
                                       const _size     : TPoint  ;
                                       const _dst      : TRect   ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawBitmap   ( const _bmp      : TGIS_Bitmap       ;
                                       const _dst      : TRect
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasSetTransformation(
                                       const _angle       : Double  ;
                                       const _origin_x    : Integer ;
                                       const _origin_y    : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasClearTransformation ; override;

   end;

  /// <summary>
  ///   Cached topmost pixel layer bitmap object to be used with
  ///   PrepareBitmapCache and RenderBitmapCache.
  /// </summary>
  TGIS_RendererVclSkiaCache = class( TGIS_RendererAbstractCache )
    private
      oBitmap : TGIS_Bitmap ;
    protected
      procedure doDestroy ; override ;
  end ;

type
  /// <summary>
  ///   Platform dependent Bitmap implementation.
  /// </summary>
  TGIS_BitmapSkia = class( TGIS_BitmapAbstract )
    private
      FBitmap    : ISkImage ;
      FPixelData : TGIS_Pixels ;
      FWritable  : Boolean ;
      FFormat    : TGIS_BitmapFormat ;
      FLineOrder : TGIS_BitmapLinesOrder ;
      FViewer    : TComponent ;
    protected
      function  fget_Width        : Integer ; override;
      function  fget_Height       : Integer ; override;
      function  fget_PPI          : Integer ; override;
      procedure fset_PPI          ( const _value : Integer
                                  ) ; override ;
      function  fget_Data         : TObject ; override;
      procedure fset_Data         ( const _value : TObject
                                  ) ; override ;
    protected
      procedure doDestroy         ; override;
    public

      /// <summary>
      ///   Standard constructor
      /// </summary>
      constructor Create          ; overload ;

      /// <summary>
      ///   Construct bitmap with given dimensions.
      /// </summary>
      /// <param name="_width">
      ///   width in pixels
      /// </param>
      /// <param name="_height">
      ///   height in pixels
      /// </param>
      constructor Create          ( const _width  : Integer ;
                                    const _height : Integer
                                  ) ; overload ;

      /// <summary>
      ///   Construct bitmap with given dimensions.
      /// </summary>
      /// <param name="_image">
      ///   image to copy
      /// </param>
      constructor Create          ( const _image  : ISkImage
                                  ) ; overload ;

      /// <inheritdoc/>
      class function FromBitmap   ( const _bmp    : TObject
                                  ) : TGIS_BitmapAbstract  ; override;

      /// <inheritdoc/>
      class function FromFile     ( const _path   : String
                                  ) : TGIS_BitmapAbstract  ; override;

      /// <inheritdoc/>
      class function FromStream   ( const _stream : TObject
                                  ) : TGIS_BitmapAbstract  ; override;
    public

      /// <inheritdoc/>
      procedure   ToFile          ( const _path   : String
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToFile          ( const _path         : String ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToStream        ( const _stream : TObject
                                  ) ; override;

      /// <inheritdoc/>
      procedure   ToStream        ( const _stream       : TObject ;
                                    const _format       : TGIS_PixelFormat ;
                                    const _subformat    : TGIS_PixelSubFormat ;
                                    const _compression  : Integer
                                  ) ; override;

      /// <inheritdoc/>
      procedure   MakeTransparent ; override;

      /// <inheritdoc/>
      procedure   Clear           ( const _color        : TGIS_Color
                                  ) ; override;

      /// <inheritdoc/>
      procedure   LockPixels      ( var   _pixels   : TGIS_Pixels ;
                                    const _writable : Boolean     ;
                                    const _format   : TGIS_BitmapFormat ;
                                    const _order    : TGIS_BitmapLinesOrder
                                  ) ; override;

      /// <inheritdoc/>
      procedure   UnlockPixels    ; override;

      /// <inheritdoc/>
      procedure   DrawShape       ( const _shape    :  TObject ;
                                    const _outline  :  Boolean ;
                                    var   _scale    :  Double  ;
                                    var   _offset   :  TPoint
                                  ) ; overload; override;
      /// <inheritdoc/>
      procedure   DrawShape       ( const _shape    :  TObject ;
                                    const _ppi      :  Integer ;
                                    const _outline  :  Boolean ;
                                    var   _scale    :  Double  ;
                                    var   _offset   :  TPoint
                                  ) ; overload; override;

      /// <inheritdoc/>
      procedure   DrawShape       ( const _shape    :  TObject    ;
                                    const _ppi      :  Integer    ;
                                    const _outline  :  Boolean    ;
                                    const _areacolor:  TGIS_Color ;
                                    const _linecolor:  TGIS_Color ;
                                    var   _scale    :  Double     ;
                                    var   _offset   :  TPoint
                                  ) ; overload; override;

      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String
                                  ) ; overload ; override;

      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String ;
                                    const _ppi      :  Integer
                                  ) ; overload ; override;

      /// <inheritdoc/>
      procedure   DrawSymbol      ( const _name     :  String     ;
                                    const _ppi      :  Integer    ;
                                    const _areacolor:  TGIS_Color ;
                                    const _linecolor:  TGIS_Color
                                  ) ; overload ; override ;

      /// <inheritdoc/>
      procedure   DrawGlyph       ( const _symbol   :  TObject    ;
                                    const _ppi      :  Integer    ;
                                    const _color    :  TGIS_Color ;
                                    const _enabled  :  Boolean
                                  ) ; overload ; override ;

      /// <inheritdoc/>
      function    CreateViewer    : IInterface ; override ;

  end;

  /// <summary>
  ///   Factory for platform dependent Bitmap implementation.
  /// </summary>
  TGIS_BitmapFactorySkia = class( TGIS_BitmapFactory )
    public
      /// <inheritdoc/>
      function DoCreate           ( const _parent : TGIS_Bitmap ;
                                    const _width  : Integer ;
                                    const _height : Integer
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromBitmap ( const _parent : TGIS_Bitmap ;
                                    const _bmp    : TObject
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromFile   ( const _parent : TGIS_Bitmap ;
                                    const _path   : String
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromStream ( const _parent : TGIS_Bitmap ;
                                    const _stream : TObject
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function DoCreateFromResource(
                                    const _parent : TGIS_Bitmap ;
                                    const _ref    : IntPtr ;
                                    const _name   : String
                                  ) : TGIS_BitmapAbstract ;
                                  override;

      /// <inheritdoc/>
      function NativeFormat       : TGIS_BitmapFormat ;
                                  override;

      /// <inheritdoc/>
      function NativeLineOrder    : TGIS_BitmapLinesOrder ;
                                  override;

      /// <inheritdoc/>
      function BitmapType         : TGIS_BitmapType ;
                                  override;

  end;

var

  /// <summary>
  ///   Bitmap renderer helper for Skia.
  /// </summary>
  BitmapFactorySkia : TGIS_BitmapFactory ;

//##############################################################################
implementation

uses
  System.UITypes,

  GisInternals,
  GisHtmlLabel,
  GisChart,
  GisCsBase,
  GisCsSystems,
  GisArcLabel,
  VCL.GisFramework,
  VCL.GisViewerBmp;

const
  // Maximum number of points for pline/polygon draws
  GDI_MAXPOINT_COUNT : Integer = 20000000 ;

{$REGION OpenGL}
type

  {#gendoc:hide}
  /// <summary>
  ///   Encapsulation of OpenGL connection.
  ///   Used by Skia rendering.
  /// </summary>
  T_OpenGLSkiaConnection= class( TGIS_OpenGLSkiaConnection )
    private
      hWindow             : HWND ;
      hDeviceContext      : HDC ;
      hGLRenderingContext : IntPtr ;

      oInterface : IGrGlInterface ;
      oContext   : IGrDirectContext ;
      oBackendRT : IGrBackendRenderTarget ;
      oSurface   : ISkSurface ;

    protected

      /// <inheritdoc/>
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates a OpenGL connection with a window.
      /// </summary>
      /// <param name="_handle">
      ///   window handle
      /// </param>
      constructor Create     ( const _handle : HWND ) ;

      /// <inheritdoc/>
      function  CreateCanvas ( const _width  : Integer ;
                               const _height : Integer
                             ) : TObject ; override;

      /// <summary>
      ///   Creates a SKCanvas object connected to the connection.
      /// </summary>
      /// <param name="_width">
      ///   canvas width
      /// </param>
      /// <param name="_height">
      ///   canvas height
      /// </param>
      /// <returns>
      ///   skia surface
      /// </returns>
      function  CreateRendererCanvas
                             ( const _width   : Integer ;
                               const _height  : Integer
                             ) : ISkSurface ;

      /// <inheritdoc/>
      procedure FreeCanvas   ( var   _canvas : TObject
                             ) ; override;

      /// <inheritdoc/>
      procedure Flush        ; override;

      /// <summary>
      ///   Saves the rendered data.
      /// </summary>
      /// <param name="_surface">
      ///   surface to draw from
      /// </param>
      /// <param name="_image">
      ///   image created from surface
      /// </param>
      procedure RendererFlush( const _surface : ISkSurface ;
                               var   _image   : ISkImage
                             ) ;
  end ;

{$ENDREGION}

type

  {#gendoc:hide}
  /// <summary>
  ///   Encapsulate all objects connected with the native Skia canvas.
  /// </summary>
  T_CanvasEncapsulation = class( TGIS_ObjectDisposable )

    GIS_Bitmap   : TGIS_Bitmap ;
    OpenGL       : T_OpenGLSkiaConnection ;
    Surface      : ISkSurface ;
    Canvas       : ISkCanvas  ;
    Image        : ISkImage   ;

    Pen          : TGIS_Pen   ;
    Brush        : TGIS_Brush ;
    Font         : TGIS_Font  ;

    SkiaPen      : TObject    ;
    SkiaBrush    : TObject    ;
    SkiaFont     : TObject    ;

    iTransparency : Integer ;
    usePen        : Boolean ;
    useBrush      : Boolean ;

    skipStroke     : Boolean ;
    skipFill       : Boolean ;
    isBitmapStroke : Boolean ;
    isBitmapFill   : Boolean ;

    protected
      procedure doDestroy ; override;

    private
      procedure init ;

    public
      {#GENDOC:HIDE}
      constructor Create ( const _width  : Integer ;
                           const _height : Integer ;
                           const _openGL : TObject
                         ) ; overload ;
      {#GENDOC:HIDE}
      constructor Create ( const _bitmap : TGIS_Bitmap ;
                           const _openGL : TObject
                         ) ; overload ;
      {#GENDOC:HIDE}
      constructor Create ( const _canvas : ISkCanvas
                         ) ; overload ;

    public
      procedure DrawLine    ( const _point0 : TPointF ;
                              const _point1 : TPointF
                            ) ; overload ;
      procedure DrawLine    ( const _x0     : Single ;
                              const _y0     : Single ;
                              const _x1     : Single ;
                              const _y1     : Single
                            ) ; overload ;
      procedure DrawArcLabel( const _shp    : TGIS_Shape ;
                              const _part   : Integer ;
                              const _text   : String  ;
                              const _tiled  : Boolean ;
                              const _savePoints
                                            : Boolean ;
                              var   _points : TGIS_DrawBuf
                            ) ;
      procedure DrawArc     ( const _rect   : TRectF ;
                              const _start  : Single ;
                              const _sweep  : Single ;
                              const _center : Boolean
                            ) ;
      procedure DrawRect    ( const _rect   : TRectF
                            ) ;
      procedure FillRect    ( const _rect   : TRectF
                            ) ;
      procedure DrawOval    ( const _rect   : TRectF
                            ) ;
      procedure FillOval    ( const _rect   : TRectF
                            ) ;
      procedure DrawPath    ( const _path   : ISkPath
                            ) ;
      procedure FillPath    ( const _path   : ISkPath
                            ) ;
      function  MeasureText ( const _text   : String
                            ) : TPoint ;
      function  TextBaseline( const _text   : String
                            ) : Single ;
      procedure DrawText    ( const _text   : String ;
                              const _x      : Single ;
                              const _y      : Single
                            ) ;
      procedure DrawImage   ( const _image  : ISkImage ;
                              const _src    : TRectF ;
                              const _dst    : TRectF
                            ) ; overload ;
      procedure DrawImage   ( const _image  : ISkImage ;
                              const _src    : TRectF ;
                              const _dst    : TRectF ;
                              const _paint  : ISkPaint
                            ) ; overload ;
      procedure Flush       ( const _useOpenGL : Boolean
                            ) ;
  end ;


  T_skiaUtils = class
    public
      class procedure FlipPixels       ( const _width      : Integer ;
                                         const _height     : Integer ;
                                         const _srcPixels  : PByte   ;
                                         const _srcStride  : Integer ;
                                         const _destPixels : PByte   ;
                                         const _destStride : Integer
                                       ) ;
      class procedure CopyPixels       ( const _width      : Integer ;
                                         const _height     : Integer ;
                                         const _srcPixels  : PByte   ;
                                         const _srcStride  : Integer ;
                                         const _destPixels : PByte   ;
                                         const _destStride : Integer
                                       ) ;
      class procedure SurfaceToBitmap  ( const _surface : ISkSurface ;
                                         const _bitmap  : TBitmap
                                       ) ;
      //class procedure BitmapToSurface  ( const _bitmap  : TBitmap ;
      //                                   const _surface : ISkSurface
      //                                 ) ;
      //class procedure SurfaceToImage   ( const _surface : ISkSurface ;
      //                                   const _image   : ISkImage
      //                                 ) ;
      class procedure ImageToSurface   ( const _image   : ISkImage ;
                                         const _surface : ISkSurface
                                       ) ;
      //class procedure ImageToBitmap    ( const _image   : ISkImage ;
      //                                   const _bitmap  : TBitmap
      //                                 ) ; overload ;
      class function  BitmapToImage    ( const _bitmap  : TBitmap
                                       ) : ISkImage ;
      class function  GisBitmapToImage ( const _bitmap  : TGIS_Bitmap
                                       ) : ISkImage ;
      class procedure ImageToGisBitmap ( const _image   : ISkImage ;
                                         const _bitmap  : TGIS_Bitmap
                                       ) ; overload ;
  end ;

  class procedure T_skiaUtils.FlipPixels(
    const _width      : Integer ;
    const _height     : Integer ;
    const _srcPixels  : PByte   ;
    const _srcStride  : Integer ;
    const _destPixels : PByte   ;
    const _destStride : Integer
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _height - 1 do
      Move( _srcPixels[ i * _srcStride ],
            _destPixels[ (_height - i - 1) * _destStride ],
            _width * 4
          ) ;
  end ;

  class procedure T_skiaUtils.CopyPixels(
    const _width      : Integer ;
    const _height     : Integer ;
    const _srcPixels  : PByte   ;
    const _srcStride  : Integer ;
    const _destPixels : PByte   ;
    const _destStride : Integer
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _height - 1 do
      Move( _srcPixels[ i * _srcStride ],
            _destPixels[ i * _destStride ],
            _width * 4
          ) ;
  end ;

  class procedure T_skiaUtils.SurfaceToBitmap(
    const _surface : ISkSurface ;
    const _bitmap  : TBitmap
  ) ;
  var
    pixmap : ISkPixmap ;
  begin
    pixmap := _surface.PeekPixels ;
    T_SkiaUtils.FlipPixels( _bitmap.Width, _bitmap.Height,
                            pixmap.Pixels, pixmap.RowBytes,
                            _bitmap.ScanLine[ _bitmap.Height - 1 ],
                            BytesPerScanLine( _bitmap.Width, 32, 32 )
                          ) ;
  end ;

  (*class procedure T_skiaUtils.BitmapToSurface(
    const _bitmap  : TBitmap ;
    const _surface : ISkSurface
  ) ;
  var
    pixmap : ISkPixmap ;
  begin
    pixmap := _surface.PeekPixels ;
    T_SkiaUtils.FlipPixels( _bitmap.Width, _bitmap.Height,
                            _bitmap.ScanLine[ _bitmap.Height - 1 ],
                            BytesPerScanLine( _bitmap.Width, 32, 32 ),
                            pixmap.Pixels, pixmap.RowBytes
                          ) ;
  end ;

  class procedure T_skiaUtils.SurfaceToImage(
    const _surface : ISkSurface ;
    const _image   : ISkImage
  ) ;
  var
    spixmap : ISkPixmap ;
    ipixmap : ISkPixmap ;
  begin
    spixmap := _surface.PeekPixels ;
    ipixmap := _image.PeekPixels ;
    T_SkiaUtils.FlipPixels( _image.Width, _image.Height,
                            spixmap.Pixels, spixmap.RowBytes,
                            ipixmap.Pixels, ipixmap.RowBytes
                          ) ;
  end ;*)

  class procedure T_skiaUtils.ImageToSurface(
    const _image   : ISkImage ;
    const _surface : ISkSurface
  ) ;
  var
    spixmap : ISkPixmap ;
    ipixmap : ISkPixmap ;
  begin
    spixmap := _surface.PeekPixels ;
    ipixmap := _image.PeekPixels ;
    T_SkiaUtils.CopyPixels( _image.Width, _image.Height,
                            ipixmap.Pixels, ipixmap.RowBytes,
                            spixmap.Pixels, spixmap.RowBytes
                          ) ;
  end ;

  (*class procedure T_skiaUtils.ImageToBitmap(
    const _image  : ISkImage ;
    const _bitmap : TBitmap
  ) ;
  var
    pixmap : ISkPixmap ;
  begin
    pixmap := _image.PeekPixels ;
    T_SkiaUtils.CopyPixels( _bitmap.Width, _bitmap.Height,
                            pixmap.Pixels, pixmap.RowBytes,
                            _bitmap.ScanLine[ _bitmap.Height - 1 ],
                            BytesPerScanLine( _bitmap.Width, 32, 32 )
                          ) ;
  end ;  *)

  class function T_SkiaUtils.BitmapToImage(
    const _bitmap  : TBitmap
  ) : ISkImage ;
  begin
    Result := TBitmap( _bitmap ).ToSkImage
  end ;

  class function T_SkiaUtils.GisBitmapToImage(
    const _bitmap  : TGIS_Bitmap
  ) : ISkImage ;
  begin
    Result := nil ;
    if _bitmap.BitmapType = TGIS_BitmapType.VCL then
      Result := TBitmap( _bitmap.GetData( NativeBitmapFactory ) ).ToSkImage
    else
    if _bitmap.BitmapType = TGIS_BitmapType.Skia then
      Result := TSkImage( _bitmap.GetData( BitmapFactorySkia ) ) ;
  end ;

  class procedure T_SkiaUtils.ImageToGisBitmap(
    const _image  : ISkImage ;
    const _bitmap : TGIS_Bitmap
  ) ;
  var
    tbmp : TBitmap ;
  begin
    if _bitmap.BitmapType = TGIS_BitmapType.Skia then
      _bitmap.SetData( BitmapFactorySkia, TSkImage( _image ) )
    else begin
      tbmp := TBitmap.CreateFromSkImage( _image ) ;
      try
        _bitmap.SetData( NativeBitmapFactory, tbmp ) ;
      finally
        FreeObject( tbmp ) ;
      end ;
    end ;
  end ;

type

  T_PenCache = class ( TGIS_Object )
    private
      iDash         : Integer ;
      arrDash       : TArray<Single> ;
      skDash        : ISkPathEffect ;
      iDot          : Integer ;
      arrDot        : TArray<Single> ;
      skDot         : ISkPathEffect ;
      iDashDot      : Integer ;
      arrDashDot    : TArray<Single> ;
      skDashDot     : ISkPathEffect ;
      iDashDotDot   : Integer ;
      arrDashDotDot : TArray<Single> ;
      skDashDotDot  : ISkPathEffect ;
      arrCustomDash : TArray<Single> ;
      skCustomDash  : ISkPathEffect ;
    public
      constructor Create ;
    public
      function  GetPathEffect ( const _style : TGIS_PenStyle ;
                                const _width : Integer
                              ) : ISkPathEffect ;
      function  GetDashEffect ( const _dash  : TGIS_DashArray ;
                                const _width : Integer
                              ) : ISkPathEffect ;
  end ;

  T_BrushCache = class
    private
      imgHorizontal : ISkImage ;
      clrHorizontal : TGIS_Color ;
      imgVertical   : ISkImage ;
      clrVertical   : TGIS_Color ;
      imgFDiagonal  : ISkImage ;
      clrFDiagonal  : TGIS_Color ;
      imgBDiagonal  : ISkImage ;
      clrBDiagonal  : TGIS_Color ;
      imgCross      : ISkImage ;
      clrCross      : TGIS_Color ;
      imgDiagCross  : ISkImage ;
      clrDiagCross  : TGIS_Color ;

    public
      function  GetBitmap ( const _style : TGIS_BrushStyle ;
                            const _color : TGIS_Color
                          ) : ISkImage ;
  end ;

  // Substitute of a pen object.
  T_Pen = class ( TGIS_ObjectDisposable )
    // properties internal values
    public
      // Native Skia object.
      Paint       : ISkPaint ;

      // Native Skia object.
      BrushPaint  : ISkPaint ;

    private
      // Color of the pen.
      FColor      : TGIS_Color ;

      // Width of the pen in pixels.
      FWidth      : Integer ;

      // Style in which the pen draws.
      FStyle      : TGIS_PenStyle ;

      // Cap style used at the end of a line.
      FLineCap    : TGIS_LineCap ;

      // Join style for the ends of two consecutive lines.
      FLineJoin   : TGIS_LineJoin ;

      // Dash array which the pen uses to draw dashed line.
      FLineDash   : TGIS_DashArray ;

      FPattern    : TGIS_BrushStyle ;

      FBitmap     : TGIS_Bitmap ;

      // Origin point for the Bitmap.
      FBitmapOrigin : TPoint ;

      // Set if any setting has changed.
      FChanged    : Boolean ;

    protected  // property access routines

      procedure fset_Color           ( const _value : TGIS_Color
                                     ) ;
      procedure fset_Width           ( const _value : Integer
                                     ) ;
      procedure fset_Style           ( const _value : TGIS_PenStyle
                                     ) ;
      procedure fset_LineCap         ( const _value : TGIS_LineCap
                                     ) ;
      procedure fset_LineJoin        ( const _value : TGIS_LineJoin
                                     ) ;
      procedure fset_LineDash        ( const _value   : TGIS_DashArray
                                     ) ;
      procedure fset_Pattern         ( const _value : TGIS_BrushStyle
                                     ) ;
      procedure fset_Bitmap          ( const _value : TGIS_Bitmap
                                     ) ;

    public     // public methods

      // Create an instance.
      constructor Create             ;

      // Select the pen object according to earlier settings.
      procedure SelectPen            ( const _rnd    : TGIS_RendererVclSkia ;
                                       const _canvas : T_CanvasEncapsulation ;
                                       const _shp    : TGIS_Shape
                                     ) ;

    public     // public properties

      // Color of the pen.
      property Color                 : TGIS_Color
                                       read  FColor
                                       write fset_Color ;

      // Width of the pen in pixels.
      property Width                 : Integer
                                       read  FWidth
                                       write fset_Width
                                       default 1 ;

      // Style in which the pen draws.
      property Style                 : TGIS_PenStyle
                                       read  FStyle
                                       write fset_Style
                                       default TGIS_PenStyle.Solid ;

      // Cap style used at the end of a line.
      property LineCap               : TGIS_LineCap
                                       read  FLineCap
                                       write fset_LineCap ;

      // Join style of a line.
      property LineJoin              : TGIS_LineJoin
                                       read  FLineJoin
                                       write fset_LineJoin ;

      // Dash array to draw a dashed line.
      property LineDash              : TGIS_DashArray
                                       read  FLineDash
                                       write fset_LineDash ;

      // Pattern for hatched pens.
      property Pattern               : TGIS_BrushStyle
                                       read  FPattern
                                       write fset_Pattern ;

     // External bitmap image that defines a pattern for the patterned pens.
      property Bitmap                : TGIS_Bitmap
                                       read  FBitmap
                                       write fset_Bitmap ;

      // Origin point for the Bitmap.
      property BitmapOrigin          : TPoint
                                       read  FBitmapOrigin
                                       write FBitmapOrigin ;

  end ;

  // Substitute of a brush object.
  T_Brush = class ( TGIS_ObjectDisposable )
    // properties internal values
    public
      // Native Skia object.
      Paint     : ISkPaint ;

    private
      // Color of the brush.
      FColor    : TGIS_Color ;

      // Pattern for the brush.
      FStyle    : TGIS_BrushStyle ;

      FBitmap   : TGIS_Bitmap ;

      // Origin point for the Bitmap.
      FBitmapOrigin : TPoint ;

      // Set if any setting has changed.
      FChanged  : Boolean ;

    protected   // property access routines

      procedure fset_Color           ( const _value : TGIS_Color
                                     ) ;
      procedure fset_Style           ( const _value : TGIS_BrushStyle
                                     ) ;
      procedure fset_Bitmap          ( const _value : TGIS_Bitmap
                                     ) ;

    public // public methods

      // Create an instance.
      constructor Create             ;

      // Select the brush object according to earlier settings.
      procedure SelectBrush          ( const _rnd    : TGIS_RendererVclSkia ;
                                       const _canvas : T_CanvasEncapsulation ;
                                       const _shp    : TGIS_Shape
                                     ) ; overload ;
      procedure SelectBrush          ( const _canvas : T_CanvasEncapsulation ;
                                       const _img    : ISkImage ;
                                       const _origin : TPoint
                                     ) ; overload ;
    public // public properties

      // Color of the brush.
      property Color                 : TGIS_Color
                                       read  FColor
                                       write fset_Color  ;

      // Pattern for the brush.
      property Style                 : TGIS_BrushStyle
                                       read  FStyle
                                       write fset_Style  ;

     // External bitmap image that defines a pattern for the brush.
      property Bitmap                : TGIS_Bitmap
                                       read  FBitmap
                                       write fset_Bitmap ;

      // Origin point for the Bitmap.
      property BitmapOrigin        : TPoint
                                     read  FBitmapOrigin
                                     write FBitmapOrigin ;

  end ;

  T_Font = class ( TGIS_ObjectDisposable )
    public
      // Native font paragraph
      Paragraph : ISkParagraph ;

    private
      // Font family name.
      FFamily : String ;

      // Font size.
      FSize : Integer ;

      // Font style.
      FStyle : TGIS_FontStyles ;

      // Font color.
      FColor : TGIS_Color ;

      // Paragraph text
      FParagraphText : String ;

      // Set if any setting has changed.
      FChanged : Boolean ;

    protected
      procedure fset_Family ( const _value : String
                            ) ;
      procedure fset_Size   ( const _value : Integer
                            ) ;
      procedure fset_Style  ( const _value : TGIS_FontStyles
                            ) ;
      procedure fset_Color  ( const _value : TGIS_Color
                            ) ;

    public // public methods

      // Create an instance.
      constructor Create    ;

    public // public methods

      procedure CreateParagraph
                            ( const _text   : String
                            ) ;

    public
      property Family : String  read  FFamily
                                write fset_Family ;

      property Size   : Integer read  FSize
                                write fset_Size ;

      property Style  : TGIS_FontStyles
                                read  FStyle
                                write fset_Style ;
      property Color  : TGIS_Color
                                read  FColor
                                write fset_Color ;
  end ;


//=============================================================================
// T_CanvasEncapsulation
//=============================================================================

  constructor T_CanvasEncapsulation.Create(
    const _width  : Integer ;
    const _height : Integer ;
    const _openGL : TObject
  ) ;
  begin
    GIS_Bitmap := nil ;
    Image := nil ;

    if assigned( _openGL ) and
       not ( _openGL is TGIS_OpenGLSkiaConnection ) then exit ;

    OpenGL := T_OpenGLSkiaConnection( _openGL ) ;

    // prepare Surface
    if assigned( _openGL ) then
      Surface := OpenGL.CreateRendererCanvas( _width, _height )
    else
      Surface := TSkSurface.MakeRaster( _width, _height ) ;
    Surface.Canvas.Clear( TAlphaColors.Null ) ;

    Canvas := Surface.Canvas ;

    init ;
  end ;

  constructor T_CanvasEncapsulation.Create(
    const _bitmap : TGIS_Bitmap ;
    const _openGL : TObject
  ) ;
  begin
    GIS_Bitmap := _bitmap ;
    Image := nil ;

    if assigned( _openGL ) and
       not ( _openGL is T_OpenGLSkiaConnection ) then exit ;

    OpenGL := T_OpenGLSkiaConnection( _openGL ) ;

    // prepare Surface
    if assigned( _openGL ) then
      Surface := OpenGL.CreateRendererCanvas( GIS_Bitmap.Width, GIS_Bitmap.Height )
    else
      Surface := TSkSurface.MakeRaster( GIS_Bitmap.Width, GIS_Bitmap.Height ) ;
    if not assigned( _bitmap ) then
      Surface.Canvas.Clear( TAlphaColors.Null )
    else
      Surface.Canvas.DrawImage(
        TSkImage( GIS_Bitmap.GetData( BitmapFactorySkia ) ), 0, 0
      ) ;

    Canvas := Surface.Canvas ;

    init ;
  end ;

  constructor T_CanvasEncapsulation.Create(
    const _canvas : ISkCanvas
  ) ;
  begin
    GIS_Bitmap := nil ;
    Image := nil ;
    OpenGL := nil ;
    Surface := nil ;

    Canvas := _canvas ;

    init ;
  end ;

  procedure T_CanvasEncapsulation.init ;
  begin
    Pen    := TGIS_Pen.Create ;
    Brush  := TGIS_Brush.Create ;
    Font   := TGIS_Font.Create ;

    SkiaPen   := T_Pen.Create ;
    SkiaBrush := T_Brush.Create ;
    SkiaFont  := T_Font.Create ;

    iTransparency := 100 ;
    usePen := True ;
    useBrush := True ;
  end ;

  procedure T_CanvasEncapsulation.doDestroy ;
  begin
    Canvas := nil ;
    Surface := nil ;
    Image := nil ;

    FreeObject( SkiaFont  ) ;
    FreeObject( SkiaBrush ) ;
    FreeObject( SkiaPen   ) ;

    FreeObject( Font  ) ;
    FreeObject( Brush ) ;
    FreeObject( Pen   ) ;

    inherited ;
  end ;

  procedure T_CanvasEncapsulation.DrawLine(
    const _point0 : TPointF ;
    const _point1 : TPointF
  ) ;
  begin
    if skipStroke then exit ;
    Canvas.DrawLine( _point0, _point1, T_Pen( SkiaPen ).Paint ) ;
  end ;

  procedure T_CanvasEncapsulation.DrawLine(
    const _x0 : Single ;
    const _y0 : Single ;
    const _x1 : Single ;
    const _y1 : Single
  ) ;
  begin
    Canvas.DrawLine( _x0, _y0, _x1, _y1, T_Pen( SkiaPen ).Paint ) ;
  end ;

  procedure T_CanvasEncapsulation.DrawArcLabel(
    const _shp        : TGIS_Shape ;
    const _part       : Integer ;
    const _text       : String ;
    const _tiled      : Boolean ;
    const _savePoints : Boolean ;
    var   _points     : TGIS_DrawBuf
  ) ;
  var
    points : TGIS_DrawBuf ;
  begin
    SetLength( points, 0 ) ;
    GisDrawArcLabel( _shp, _shp, _part, _text, _tiled, _savePoints, points ) ;
    SetLength( _points, Length( points ) ) ;
    Move( points, _points, Length( points ) ) ;
  end ;

  procedure T_CanvasEncapsulation.DrawArc(
    const _rect   : TRectF ;
    const _start  : Single ;
    const _sweep  : Single ;
    const _center : Boolean
  ) ;
  begin
    Canvas.DrawArc( _rect, _start, _sweep, _center, T_Pen( SkiaPen ).Paint )
  end ;

  procedure T_CanvasEncapsulation.DrawRect(
    const _rect : TRectF
  ) ;
  begin
    if skipStroke then exit ;

    Canvas.DrawRect( _rect, T_Pen( SkiaPen ).Paint )
  end ;

  procedure T_CanvasEncapsulation.FillRect(
    const _rect : TRectF
  ) ;
  begin
    if skipFill then exit ;

    if isBitmapFill then begin
      Canvas.Save ;
      Canvas.ClipRect( _rect, TSkClipOp.Intersect, True ) ;
      Canvas.DrawPaint( T_Brush( SkiaBrush ).Paint ) ;
      Canvas.Restore ;
    end
    else
      Canvas.DrawRect( _rect, T_Brush( SkiaBrush ).Paint ) ;
  end ;

  procedure T_CanvasEncapsulation.DrawOval(
    const _rect : TRectF
  ) ;
  begin
    if skipStroke then exit ;

    Canvas.DrawOval( _rect, T_Pen( SkiaPen ).Paint ) ;
  end ;

  procedure T_CanvasEncapsulation.FillOval(
    const _rect : TRectF
  ) ;
  begin
    if skipFill then exit ;

    Canvas.DrawOval( _rect, T_Brush( SkiaBrush ).Paint )
  end ;

  procedure T_CanvasEncapsulation.DrawPath(
    const _path : ISkPath
  ) ;
  var
    outlinepath : ISkPath ;
  begin
    if skipStroke then exit ;

    if isBitmapStroke then begin
      outlinepath := T_Pen( SkiaPen ).Paint.GetFillPath( _path ) ;
      if assigned( outlinepath ) then begin
        Canvas.Save ;
        Canvas.ClipPath( outlinepath, TSkClipOp.Intersect, True ) ;
        Canvas.DrawPaint( T_Pen( SkiaPen ).BrushPaint ) ;
        Canvas.Restore ;
      end
      else
        assert( False ) ;
    end
    else
      Canvas.DrawPath( _path, T_Pen( SkiaPen ).Paint ) ;
  end ;

  procedure T_CanvasEncapsulation.FillPath(
    const _path : ISkPath
  ) ;
  begin
    if skipFill then exit ;

    if isBitmapFill then begin
      Canvas.Save ;
      Canvas.ClipPath( _path, TSkClipOp.Intersect, True ) ;
      Canvas.DrawPaint( T_Brush( SkiaBrush ).Paint ) ;
      Canvas.Restore ;
    end
    else
      Canvas.DrawPath( _path, T_Brush( SkiaBrush ).Paint ) ;
  end ;

  function T_CanvasEncapsulation.MeasureText(
    const _text : String
  ) : TPoint ;
  var
    fnt : T_Font ;
    m   : TSkMetrics ;
   begin
   try
    fnt := T_Font( SkiaFont ) ;
   except
    fnt := T_Font( SkiaFont ) ;
   end ;
   try
    fnt.CreateParagraph( _text ) ;
   except
    fnt.CreateParagraph( _text ) ;
   end;
   try
    m := fnt.Paragraph.LineMetrics[0] ;
   except
    m := fnt.Paragraph.LineMetrics[0] ;
   end;
    Result := Point( RoundS( m.Width ), RoundS( m.Height ) ) ;
  end ;

  function T_CanvasEncapsulation.TextBaseline(
    const _text : String
  ) : Single ;
  var
    fnt : T_Font ;
  begin
    fnt := T_Font( SkiaFont ) ;
    fnt.CreateParagraph( _text ) ;

    if Length( fnt.Paragraph.LineMetrics ) > 0  then
      Result := fnt.Paragraph.LineMetrics[0].Baseline
    else
      Result := 0 ;
  end ;

  procedure T_CanvasEncapsulation.DrawText(
    const _text : String ;
    const _x    : Single ;
    const _y    : Single
  ) ;
  var
    fnt : T_Font ;
  begin
    fnt := T_Font( SkiaFont ) ;
    fnt.CreateParagraph( _text ) ;
    fnt.Paragraph.Paint( Canvas, _x, _y ) ;
  end ;

  procedure T_CanvasEncapsulation.DrawImage(
    const _image : ISkImage ;
    const _src   : TRectF ;
    const _dst   : TRectF
  ) ;
  begin
    Canvas.DrawImageRect( _image, _src, _dst ) ;
  end ;

  procedure T_CanvasEncapsulation.DrawImage(
    const _image : ISkImage ;
    const _src   : TRectF ;
    const _dst   : TRectF ;
    const _paint : ISkPaint
  ) ;
  begin
    Canvas.DrawImageRect( _image, _src, _dst, _paint ) ;
  end ;

  procedure T_CanvasEncapsulation.Flush(
    const _useOpenGL : Boolean
  ) ;
  begin
    if _useOpenGL then
      OpenGL.RendererFlush( Surface, Image )
    else if ( Surface <> nil ) then
      Image := Surface.MakeImageSnapshot ;
  end ;

//=============================================================================
// T_PenCache
//=============================================================================

  constructor T_PenCache.Create ;
  begin
    inherited ;

    iDash := Integer.MaxValue ;
    iDot := Integer.MaxValue ;
    iDashDot := Integer.MaxValue ;
    iDashDotDot := Integer.MaxValue ;
  end ;

  function T_PenCache.GetPathEffect(
    const _style : TGIS_PenStyle ;
    const _width : Integer
  ) : ISkPathEffect ;
  begin
    case _style of
      TGIS_PenStyle.Dash :
        begin
          if not assigned( arrDash ) then
            SetLength( arrDash, 2 ) ;
          if _width <> iDash then begin
            arrDash[0] := 3.0 * _width ;
            arrDash[1] := 1.0 * _width ;
            skDash := TSkPathEffect.MakeDash( arrDash, 0 ) ;
          end ;
          Result := skDash ;
        end ;
      TGIS_PenStyle.Dot :
        begin
          if not assigned( arrDot ) then
            SetLength( arrDot, 2 ) ;
          if _width <> iDot then begin
            arrDot[0] := 1.0 * _width ;
            arrDot[1] := 1.0 * _width ;
            skDot := TSkPathEffect.MakeDash( arrDot, 0 ) ;
          end ;
          Result := skDot ;
        end ;
      TGIS_PenStyle.DashDot :
        begin
          if not assigned( arrDashDot ) then
            SetLength( arrDashDot, 4 ) ;
          if _width <> iDashDot then begin
            arrDashDot[0] := 3.0 * _width ;
            arrDashDot[1] := 1.0 * _width ;
            arrDashDot[2] := 1.0 * _width ;
            arrDashDot[3] := 1.0 * _width ;
            skDashDot := TSkPathEffect.MakeDash( arrDashDot, 0 ) ;
          end ;
          Result := skDashDot ;
        end ;
      TGIS_PenStyle.DashDotDot :
        begin
          if not assigned( arrDashDotDot ) then
            SetLength( arrDashDotDot, 6 ) ;
          if _width <> iDashDotDot then begin
            arrDashDotDot[0] := 3.0 * _width ;
            arrDashDotDot[1] := 1.0 * _width ;
            arrDashDotDot[2] := 1.0 * _width ;
            arrDashDotDot[3] := 1.0 * _width ;
            arrDashDotDot[4] := 1.0 * _width ;
            arrDashDotDot[5] := 1.0 * _width ;
            skDashDotDot := TSkPathEffect.MakeDash( arrDashDotDot, 0 ) ;
          end;
          Result := skDashDotDot ;
        end ;
      else Result := nil ;
    end ;
  end ;

  function T_PenCache.GetDashEffect(
    const _dash  : TGIS_DashArray ;
    const _width : Integer
  ) : ISkPathEffect ;
  var
    arr   : TArray<Single> ;
    i, j  : Integer ;
    bsame : Boolean ;
  begin
    bsame := True ;
    if length( arrCustomDash ) = length( _dash ) then begin
      for i := low(_dash) to high(_dash) do
        if _dash[i] <> arrCustomDash[i] then begin
          bsame := False ;
          break ;
        end ;
    end
    else
      bsame := False ;

    if bsame then begin
      Result := skCustomDash ;
      exit ;
    end ;

    j := 0 ;
    SetLength( arr, 2*length( _dash ) ) ;
    for i := 0 to length(_dash)-1 do
      if (i = 0) and (_dash[i] < 0) then begin
        arr[j] := 0 ;
        inc(j);
        arr[j] := abs(_dash[i]) ;
        inc(j);
      end
      else begin
        arr[j] := abs(_dash[i]) ;
        inc(j);
      end;
      if (j mod 2) = 1 then
        inc( j ) ;

    SetLength( arr, j ) ;

    SetLength( arrCustomDash, length( _dash ) ) ;
    for i := low(_dash) to high(_dash) do
      arrCustomDash[i] := _dash[i] ;

    skCustomDash := TSkPathEffect.MakeDash( arr, 0 ) ;
    Result := skCustomDash ;
  end ;

//=============================================================================
// T_BrushCache
//=============================================================================

function T_BrushCache.GetBitmap(
  const _style : TGIS_BrushStyle ;
  const _color : TGIS_Color
) : ISkImage ;
var
  dim : Integer ;
  sur : ISkSurface ;
  px  : Pointer ;
  i   : Integer ;
  k   : Integer ;
  w   : Integer ;

  procedure init_bmp(
    var _surface : ISkSurface ;
        _dim     : Integer
  ) ;
  begin
    _surface := TSkSurface.MakeRaster( _dim, _dim ) ;
    _surface.Canvas.Clear( TAlphaColors.Null ) ;
  end ;

begin
  case _style of
    TGIS_BrushStyle.Horizontal :
      begin
        dim := 6 ;
        if not assigned( imgHorizontal ) or ( _color <> clrHorizontal ) then begin
          init_bmp( sur, dim ) ;
          clrHorizontal := _color ;
          px := sur.PeekPixels.Pixels ;
          for i := 0 to dim-1 do begin
            w := i * Integer(sur.PeekPixels.RowBytes) ;
            for k := 0 to dim-1 do begin
              if ( i = 3 ) then begin
                (PByte(px) + w + 4*k  )^ := _color.B ;
                (PByte(px) + w + 4*k+1)^ := _color.G ;
                (PByte(px) + w + 4*k+2)^ := _color.R ;
                (PByte(px) + w + 4*k+3)^ := 255 ;
              end ;
            end ;
          end ;
          imgHorizontal := sur.MakeImageSnapshot ;
          sur := nil ;
        end ;
        Result := imgHorizontal ;
      end ;
    TGIS_BrushStyle.Vertical   :
      begin
        dim := 6 ;
        if not assigned( imgVertical ) or ( _color <> clrVertical ) then begin
          init_bmp( sur, dim ) ;
          clrVertical := _color ;
          px := sur.PeekPixels.Pixels ;
          for i := 0 to dim-1 do begin
            w := i * Integer(sur.PeekPixels.RowBytes) ;
            for k := 0 to dim-1 do begin
              if ( k = 3 ) then begin
                (PByte(px) + w + 4*k  )^ := _color.B ;
                (PByte(px) + w + 4*k+1)^ := _color.G ;
                (PByte(px) + w + 4*k+2)^ := _color.R ;
                (PByte(px) + w + 4*k+3)^ := 255 ;
              end ;
            end;
          end;
          imgVertical := sur.MakeImageSnapshot ;
          sur := nil ;
        end ;
        Result := imgVertical ;
      end ;
    TGIS_BrushStyle.FDiagonal  :
      begin
        dim := 8 ;
        if not assigned( imgFDiagonal ) or ( _color <> clrFDiagonal ) then begin
          init_bmp( sur, dim ) ;
          clrFDiagonal := _color ;
          px := sur.PeekPixels.Pixels ;
          for i := 0 to dim-1 do begin
            w := i * Integer(sur.PeekPixels.RowBytes) ;
            for k := 0 to dim-1 do begin
              if ( i = k ) then begin
                (PByte(px) + w + 4*k  )^ := _color.B ;
                (PByte(px) + w + 4*k+1)^ := _color.G ;
                (PByte(px) + w + 4*k+2)^ := _color.R ;
                (PByte(px) + w + 4*k+3)^ := 255 ;
              end ;
            end ;
          end ;
          imgFDiagonal := sur.MakeImageSnapshot ;
          sur := nil ;
        end ;
        Result := imgFDiagonal ;
      end ;
    TGIS_BrushStyle.BDiagonal  :
      begin
        dim := 8 ;
        if not assigned( imgBDiagonal ) or ( _color <> clrBDiagonal ) then begin
          init_bmp( sur, dim ) ;
          clrBDiagonal := _color ;
          px := sur.PeekPixels.Pixels ;
          for i := 0 to dim-1 do begin
            w := i * Integer(sur.PeekPixels.RowBytes) ;
            for k := 0 to dim-1 do begin
              if ( i + k = dim-1 ) then begin
                (PByte(px) + w + 4*k  )^ := _color.B ;
                (PByte(px) + w + 4*k+1)^ := _color.G ;
                (PByte(px) + w + 4*k+2)^ := _color.R ;
                (PByte(px) + w + 4*k+3)^ := 255 ;
              end ;
            end ;
          end ;
          imgBDiagonal := sur.MakeImageSnapshot ;
          sur := nil ;
        end ;
        Result := imgBDiagonal ;
      end ;
    TGIS_BrushStyle.Cross      :
      begin
        dim := 6 ;
        if not assigned( imgCross ) or ( _color <> clrCross ) then begin
          init_bmp( sur, dim ) ;
          clrCross := _color ;
          px := sur.PeekPixels.Pixels ;
          for i := 0 to dim-1 do begin
            w := i * Integer(sur.PeekPixels.RowBytes) ;
            for k := 0 to dim-1 do begin
              if ( i = 3 ) or ( k = 3 ) then begin
                (PByte(px) + w + 4*k  )^ := _color.B ;
                (PByte(px) + w + 4*k+1)^ := _color.G ;
                (PByte(px) + w + 4*k+2)^ := _color.R ;
                (PByte(px) + w + 4*k+3)^ := 255 ;
              end ;
            end ;
          end ;
          imgCross := sur.MakeImageSnapshot ;
          sur := nil ;
        end ;
        Result := imgCross ;
      end ;
    TGIS_BrushStyle.DiagCross  :
      begin
        dim := 8 ;
        if not assigned( imgDiagCross ) or ( _color <> clrDiagCross ) then begin
          init_bmp( sur, dim ) ;
          clrDiagCross := _color ;
          px := sur.PeekPixels.Pixels ;
          for i := 0 to dim-1 do begin
            w := i * Integer(sur.PeekPixels.RowBytes) ;
            for k := 0 to dim-1 do begin
              if ( i = k ) or ( i + k = dim ) then begin
                (PByte(px) + w + 4*k  )^ := _color.B ;
                (PByte(px) + w + 4*k+1)^ := _color.G ;
                (PByte(px) + w + 4*k+2)^ := _color.R ;
                (PByte(px) + w + 4*k+3)^ := 255 ;
              end ;
            end ;
          end ;
          imgDiagCross := sur.MakeImageSnapshot ;
          sur := nil ;
        end ;
        Result := imgDiagCross ;
      end ;
    else
      begin
        Result := nil ;
        exit ;
      end ;
  end ;
end ;

//=============================================================================
// T_Pen
//=============================================================================

  constructor T_Pen.Create ;
  begin
    inherited ;

    Paint := TSkPaint.Create ;
    Paint.Antialias := True;
    Paint.Style := TSkPaintStyle.Stroke ;
    Paint.Color := TGIS_Color.Black.ToARGB ;
    Paint.StrokeWidth := 1.0 ;
    Paint.StrokeCap := TSkStrokeCap.Round ;
    Paint.StrokeJoin := TSkStrokeJoin.Round ;

    BrushPaint := TSkPaint.create ;
    BrushPaint.Style := TSkPaintStyle.Fill ;

    FColor    := TGIS_Color.Black ;
    FWidth    := 1 ;
    FStyle    := TGIS_PenStyle.Solid ;
    FLineCap  := TGIS_LineCap.Round ;
    FLineJoin := TGIS_LineJoin.Round   ;
    FPattern  := TGIS_BrushStyle.Solid ;
    FLineDash := [] ;
    FBitmap   := nil ;
    FBitmapOrigin := Point( 0, 0 ) ;
    FChanged  := False ;
  end ;

  procedure T_Pen.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    if FColor <> _value then begin
      FColor := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_Width(
    const _value  : Integer
  ) ;
  begin
    if FWidth <> _value then begin
      FWidth   := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_Style(
    const _value : TGIS_PenStyle
  ) ;
  begin
    if FStyle <> _value then begin
      FStyle   := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_LineCap(
    const _value : TGIS_LineCap
  ) ;
  begin
    if FLineCap <> _value then begin
      FLineCap := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_LineJoin(
    const _value : TGIS_LineJoin
  ) ;
  begin
    if FLineJoin <> _value then begin
      FLineJoin := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_LineDash(
    const _value : TGIS_DashArray
  ) ;
  begin
    if FLineDash <> _value then begin
      FLineDash := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_Pattern(
    const _value : TGIS_BrushStyle
  ) ;
  begin
    if FPattern <> _value then begin
      FPattern := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.fset_Bitmap(
    const _value : TGIS_Bitmap
  ) ;
  begin
    if ( FBitmap <> _value ) or
       ( assigned( FBitmap ) and
         not FBitmap.Equals( _value )
       ) then
    begin
      FBitmap := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.SelectPen(
    const _rnd    : TGIS_RendererVclSkia ;
    const _canvas : T_CanvasEncapsulation ;
    const _shp    : TGIS_Shape
  ) ;

    procedure create_pen ;
    var
      img : ISkImage ;
    begin
      _canvas.skipStroke := False ;
      _canvas.isBitmapStroke := False;

      BrushPaint.Shader := nil ;
      BrushPaint.ImageFilter := nil ;

      Paint.Color := FColor.ToARGB ;

      if not TGIS_Bitmap.IsNilOrEmpty( FBitmap ) then begin
        BrushPaint.Shader := TSkImage( FBitmap.GetData( BitmapFactorySkia ) ).MakeShader(
                               TSkTileMode.Repeat, TSkTileMode.Repeat
                             ) ;
        Paint.Color := TAlphaColors.White ;

        _canvas.isBitmapStroke := True;
      end
      else if ( FPattern <> TGIS_BrushStyle.Solid ) and
              ( FPattern <> TGIS_BrushStyle.Clear ) then begin
        img := _rnd.preparePatternFill( FPattern, FColor ) ;
        BrushPaint.Shader := img.MakeShader(
                               TSkTileMode.Repeat, TSkTileMode.Repeat ) ;
        Paint.Color := TAlphaColors.White ;

        _canvas.isBitmapStroke := True ;
      end ;

      if FStyle = TGIS_PenStyle.Clear then begin
        Paint.PathEffect := nil ;
        _canvas.skipStroke := True ;
      end
      else if length( FLineDash ) > 0 then
        Paint.PathEffect := _rnd.prepareDashStyle( FLineDash, FWidth )
      else
        Paint.PathEffect := _rnd.prepareStrokeStyle( FStyle, FWidth ) ;

      Paint.StrokeWidth := FWidth ;

      if ( FStyle = TGIS_PenStyle.Solid ) then
        case FLineCap of
                  TGIS_LineCap.Flat   : Paint.StrokeCap := TSkStrokeCap.Butt ;
                  TGIS_LineCap.Square : Paint.StrokeCap := TSkStrokeCap.Square ;
                  TGIS_LineCap.Round  : Paint.StrokeCap := TSkStrokeCap.Round ;
                  else                  Paint.StrokeCap := TSkStrokeCap.Round ;
        end
      else
        Paint.StrokeCap := TSkStrokeCap.Butt ;

      case FLineJoin of
                  TGIS_LineJoin.Bevel : Paint.StrokeJoin := TSkStrokeJoin.Bevel ;
                  TGIS_LineJoin.Miter : Paint.StrokeJoin := TSkStrokeJoin.Miter ;
                  TGIS_LineJoin.Round : Paint.StrokeJoin := TSkStrokeJoin.Round ;
                  else                  Paint.StrokeJoin := TSkStrokeJoin.Round ;
      end ;

    end ;

  begin
    if FChanged then
      create_pen ;
    if _canvas.isBitmapStroke then
      BrushPaint.ImageFilter := TSkImageFilter.MakeOffset( BitmapOrigin.X, BitmapOrigin.Y ) ;
    FChanged := false ;
  end ;

//=============================================================================
// T_Brush
//=============================================================================

  constructor T_Brush.Create ;
  begin
    inherited ;

    Paint := TSKPaint.Create ;
    Paint.AntiAlias := True;
    Paint.Style := TSkPaintStyle.Fill ;
    Paint.Color := TGIS_Color.White.ToARGB ;

    FColor   := TGIS_Color.White ;
    FStyle   := TGIS_BrushStyle.Solid ;
    FBitmap  := nil ;
    FBitmapOrigin := Point( 0, 0 ) ;
    FChanged := False ;
  end ;

  procedure T_Brush.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    if FColor <> _value then begin
      FColor   := _value ;
      FChanged := true ;
    end ;
  end ;

  procedure T_Brush.fset_Style(
    const _value : TGIS_BrushStyle
  ) ;
  begin
    if FStyle <> _value then begin
      FStyle := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Brush.fset_Bitmap(
    const _value : TGIS_Bitmap
  ) ;
  begin
    if ( FBitmap <> _value ) or
       ( assigned( FBitmap ) and
         not FBitmap.Equals( _value )
       ) then
    begin
      FBitmap := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Brush.SelectBrush(
    const _rnd    : TGIS_RendererVclSkia ;
    const _canvas : T_CanvasEncapsulation ;
    const _shp    : TGIS_Shape
  ) ;

    procedure create_brush ;
    var
      img : ISkImage ;
    begin
      _canvas.skipFill := False ;
      _canvas.isBitmapFill := False;

      Paint.PathEffect := nil ;
      Paint.Shader := nil ;
      Paint.ImageFilter := nil ;
      Paint.Color := FColor.ToARGB ;

      if not TGIS_Bitmap.IsNilOrEmpty( FBitmap ) then begin
        Paint.Shader := TSkImage( FBitmap.GetData( BitmapFactorySkia ) ).MakeShader(
                          TSkTileMode.Repeat, TSkTileMode.Repeat
                        ) ;
        Paint.Color := TAlphaColors.White ;
        _canvas.isBitmapFill := True;
      end
      else if FStyle = TGIS_BrushStyle.Clear then begin
        _canvas.skipFill := True ;
      end
      else if FStyle <> TGIS_BrushStyle.Solid then begin
        img := _rnd.preparePatternFill( FStyle, FColor ) ;
        Paint.Shader := img.MakeShader( TSkTileMode.Repeat, TSkTileMode.Repeat ) ;
        Paint.Color := TAlphaColors.White ;
        _canvas.isBitmapFill := True;
      end ;
    end ;

  begin
    if FChanged then
      create_brush ;
    if _canvas.isBitmapFill then
      Paint.ImageFilter := TSkImageFilter.MakeOffset( BitmapOrigin.X, BitmapOrigin.Y ) ;
    FChanged := false ;
  end ;


  procedure T_Brush.SelectBrush(
    const _canvas : T_CanvasEncapsulation ;
    const _img    : ISkImage ;
    const _origin : TPoint
  ) ;
  begin
    _canvas.skipFill := False ;
    Paint.Shader := _img.MakeShader( TSkTileMode.Repeat, TSkTileMode.Repeat ) ;
    Paint.ImageFilter := TSkImageFilter.MakeOffset( _origin.X, _origin.Y ) ;
    Paint.Color := TAlphaColors.White ;
    _canvas.isBitmapFill := True;
    FChanged := true ;
  end ;


//=============================================================================
// T_Font
//=============================================================================

  constructor T_Font.Create ;
  begin
    inherited ;

    Paragraph := nil ;
    FParagraphText := '' ;

    FFamily  := 'Arial' ;
    FSize    := 9 ;
    FStyle   := [] ;
    FChanged := False ;
  end ;

  procedure T_Font.fset_Family(
    const _value : String
  ) ;
  begin
    if FFamily <> _value then begin
      FFamily := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Font.fset_Size(
    const _value : Integer
  ) ;
  begin
    if FSize <> _value then begin
      FSize := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Font.fset_Style(
    const _value : TGIS_FontStyles
  ) ;
  begin
    if FStyle <> _value then begin
      FStyle := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Font.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    if FColor <> _value then begin
      FColor := _value ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Font.CreateParagraph(
    const _text : String
  );
  var
    pstyle  : ISkParagraphStyle ;
    builder : ISkParagraphBuilder ;
    tstyle  : ISkTextStyle ;
    fweight : TSkFontWeight ;
    fslant  : TSkFontSlant ;
    tdecor  : TSkTextDecorations ;
  begin
    if (Paragraph = nil) or FChanged or (FParagraphText <> _text) then begin
      pstyle := TSkParagraphStyle.Create ;
      pstyle.MaxLines := 1 ;
      builder := TSkParagraphBuilder.Create( pstyle ) ;

      tstyle := TSkTextStyle.Create ;
      tstyle.Color := FColor.ToARGB ;
      tstyle.FontSize := Size;

      if GisTestFontStyle( TGIS_FontStyle.Bold, FStyle ) then
        fweight := TSkFontWeight.Bold
      else
        fweight := TSkFontWeight.Normal ;
      if GisTestFontStyle( TGIS_FontStyle.Italic, FStyle ) then
        fslant := TSkFontSlant.Italic
      else
        fslant := TSkFontSlant.Upright ;
      tstyle.FontStyle := TSkFontStyle.Create( fweight,
                                               TSkFontWidth.Normal,
                                               fslant ) ;
      tdecor := [] ;
      if GisTestFontStyle( TGIS_FontStyle.Underline, FStyle ) then
        tdecor := tdecor + [ TSkTextDecoration.Underline ] ;
      if GisTestFontStyle( TGIS_FontStyle.StrikeOut, FStyle ) then
        tdecor := tdecor + [ TSkTextDecoration.LineThrough ] ;
      tstyle.Decorations := tdecor ;
      tstyle.FontFamilies := [Family] ;
      tstyle.HalfLeading := True ;

      builder.PushStyle( tstyle ) ;
      builder.AddText( _text ) ;

      Paragraph := builder.Build ;
      Paragraph.Layout( 10000 ) ;
      FChanged := False ;
    end ;
    FParagraphText := _text ;
  end ;

//=============================================================================
// TGIS_RendererSkia
//=============================================================================

  procedure TGIS_RendererVclSkia.prepareSelectionCanvas(
    _transparently : Boolean ;
    _useBaseMap    : Boolean ;
    _shp           : TGIS_Shape
  ) ;
  var
    transp_sel : Integer ;
  begin
    if assigned( Viewer ) then begin
      colorSelection := _shp.Layer.SelectionGisColor ;
      if _transparently then
        transp_sel := Viewer.SelectionTransparency
      else
        transp_sel := 100 ;
    end
    else begin
      colorSelection := TGIS_Color.Red ;
      transp_sel := 60 ;
    end ;

    if assigned( oSelectionCanvas ) then begin
      // when selection is drawn on BaseMap directly
      // current color must be calculated every time the method is called
      if _useBaseMap or not assigned( Context.Selection ) then
        colorSelection := TGIS_Color.FromARGB(
                            colorSelection.A * transp_sel div 100,
                            colorSelection.R,
                            colorSelection.G,
                            colorSelection.B
                          ) ;
      exit ;
    end ;

    if not _useBaseMap and Context.SelectionOnDemand then
      oSelectionCanvas := T_CanvasEncapsulation.Create( Width, Height,
                                                        Context.SourceDrawContext ) ;

    if not assigned( oSelectionCanvas ) then begin
      oSelectionCanvas := oCanvas ;
      colorSelection := TGIS_Color.FromARGB(
                          colorSelection.A * transp_sel div 100,
                          colorSelection.R,
                          colorSelection.G,
                          colorSelection.B
                        ) ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.prepareChartsCanvas ;
  begin
    if assigned( oChartsCanvas ) then exit ;

    if Context.ChartsOnDemand then
      oChartsCanvas := T_CanvasEncapsulation.Create( Width, Height,
                                                     Context.SourceDrawContext ) ;

    if not assigned( oChartsCanvas ) then
      oChartsCanvas := oCanvas ;
  end ;

  procedure TGIS_RendererVclSkia.prepareLabelsCanvas ;
  begin
    if assigned( oLabelsCanvas ) then exit ;

    if Context.LabelsOnDemand then
      oLabelsCanvas := T_CanvasEncapsulation.Create( Width, Height,
                                                     Context.SourceDrawContext ) ;

    if not assigned( oLabelsCanvas ) then
      oLabelsCanvas := oCanvas ;
  end ;

  function TGIS_RendererVclSkia.fget_BitmapFactory
    : TGIS_BitmapFactory ;
  begin
    Result := BitmapFactorySkia ;
  end ;

  function TGIS_RendererVclSkia.fget_CanvasPen
   : TGIS_Pen ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := T_CanvasEncapsulation( FCanvas ).Pen ;
  end;

  procedure TGIS_RendererVclSkia.fset_CanvasPen(
    const _value : TGIS_Pen
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    T_CanvasEncapsulation( FCanvas ).Pen := _value ;
  end;

  function TGIS_RendererVclSkia.fget_CanvasBrush
    : TGIS_Brush ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := T_CanvasEncapsulation( FCanvas ).Brush ;
  end;

  procedure TGIS_RendererVclSkia.fset_CanvasBrush(
    const _value : TGIS_Brush
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    T_CanvasEncapsulation( FCanvas ).Brush := _value ;
  end;

  function  TGIS_RendererVclSkia.fget_CanvasFont
    : TGIS_Font ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := T_CanvasEncapsulation( FCanvas ).Font ;
  end;

  procedure TGIS_RendererVclSkia.fset_CanvasFont(
    const _value : TGIS_Font
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    T_CanvasEncapsulation( FCanvas ).Font := _value ;
  end;

  function TGIS_RendererVclSkia.fget_Info
    : String ;
  var
    cnv : T_CanvasEncapsulation ;
  begin
    cnv := T_CanvasEncapsulation.Create( 1, 1, nil ) ;
    try
      Result := '[' + ClassName + ':' + TSkCanvas(cnv.Canvas).ClassName + ']' ;
    finally
      FreeObject( cnv ) ;
    end;
  end;

  constructor TGIS_RendererVclSkia.Create ;
  begin
    inherited ;

    penCache := T_PenCache.Create ;
    brushCache := T_BrushCache.Create ;
    flashed := false ;
    pextra := false ;
    useOpenGL := false ;
  end ;

  procedure TGIS_RendererVclSkia.doDestroy ;
  begin
    FreeObject( penCache ) ;
    FreeObject( brushCache ) ;

    inherited ;
  end ;

  function TGIS_RendererVclSkia.CreateInstance
    : TGIS_RendererAbstract ;
  begin
    Result := TGIS_RendererVclSkia.Create ;
  end ;

  procedure TGIS_RendererVclSkia.CreateContext(
    const _parent    : IGIS_ViewerParent ;
    const _viewer    : IGIS_Viewer ;
    const _context   : TGIS_RendererContext ;
    const _shift     : TPoint  ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _ppi       : Integer ;
    const _fontscale : Integer
  ) ;
  begin
    inherited CreateContext( _parent, _viewer, _context,
                             _shift, _width, _height, _ppi, _fontscale ) ;
    RestoreContext ;
  end ;

  procedure TGIS_RendererVclSkia.CreateContext(
    const _parent    : IGIS_ViewerParent ;
    const _viewer    : IGIS_Viewer ;
    const _context   : TGIS_RendererContext ;
    const _shift     : TPoint  ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _ppi       : Integer ;
    const _fontscale : Integer ;
    const _tilerect  : TRect
  ) ;
  begin
    inherited CreateContext( _parent, _viewer, _context,
                             _shift, _width, _height, _ppi, _fontscale,
                             _tilerect ) ;
    RestoreContext ;
  end ;

  procedure TGIS_RendererVclSkia.RestoreContext ;
  begin
    assert( not assigned( oCanvas ) ) ;
    if assigned( oCanvas ) then exit ;

    if assigned( Context ) then begin
      useOpenGL := assigned( Context.SourceDrawContext ) ;

      if assigned( Context.NativeDrawContext ) then begin
        if Context.NativeDrawContext is TGIS_PaintSkia then
          oCanvas := T_CanvasEncapsulation.Create(
                       TGIS_PaintSkia(Context.NativeDrawContext).Canvas ) ;
      end
      else if Context.BaseMapOnDemand then
        oCanvas := T_CanvasEncapsulation.Create( Width, Height,
                                                 Context.SourceDrawContext )
      else if Context.BaseMap is TGIS_Bitmap then
        oCanvas := T_CanvasEncapsulation.Create( TGIS_Bitmap( Context.BaseMap ),
                                                 Context.SourceDrawContext ) ;
    end ;

    FCanvas := oCanvas ;

    oSelectionCanvas := nil ;
    oLabelsCanvas := nil ;
    oChartsCanvas := nil ;
    oEditCanvas := nil ;
    oTransparentCanvas := nil ;
    sourceShape := nil ;

    iTolerance := 1 ;
    iToleranceSel := 2 ;
  end ;

  procedure TGIS_RendererVclSkia.ReleaseContext ;
  begin
    FreeObject( oSelectionCanvas ) ;

    FreeObject( oChartsCanvas    ) ;

    FreeObject( oLabelsCanvas ) ;

    FreeObject( oCanvas ) ;

    FCanvas := nil ;
    inherited ;
  end ;

  procedure TGIS_RendererVclSkia.PrepareHourglassContext ;
  begin
    // do nothing
  end;

  procedure TGIS_RendererVclSkia.AfterDraw ;
  var
    bmp : TGIS_Bitmap ;
  begin
    if assigned( oChartsCanvas ) and
       not oChartsCanvas.Equals( oCanvas ) then begin
      T_CanvasEncapsulation( oChartsCanvas ).Flush( useOpenGL ) ;
      if Context.ChartsOnDemand then begin
        bmp := TGIS_Bitmap.Create ;
        Context.AssignCharts( bmp, True ) ;
        bmp.SetData( BitmapFactorySkia,
                     TSkImage( T_CanvasEncapsulation( oChartsCanvas ).Image )
                   ) ;
        FreeObject( oChartsCanvas ) ;
      end ;
    end
    else
      oChartsCanvas := nil ;

    if assigned( oLabelsCanvas ) and
       not oLabelsCanvas.Equals( oCanvas ) then begin
      T_CanvasEncapsulation( oLabelsCanvas ).Flush( useOpenGL ) ;
      if Context.LabelsOnDemand then begin
        bmp := TGIS_Bitmap.Create ;
        Context.AssignLabels( bmp, True ) ;
        bmp.SetData( BitmapFactorySkia,
                     TSkImage( T_CanvasEncapsulation( oLabelsCanvas ).Image )
                   ) ;
        FreeObject( oLabelsCanvas ) ;
      end ;
    end
    else
      oLabelsCanvas := nil ;

    if assigned( oSelectionCanvas ) and
       not oSelectionCanvas.Equals( oCanvas ) then begin
      T_CanvasEncapsulation( oSelectionCanvas ).Flush( useOpenGL ) ;
      if Context.SelectionOnDemand then begin
        bmp := TGIS_Bitmap.Create ;
        Context.AssignSelection( bmp, True ) ;
        bmp.SetData( BitmapFactorySkia,
                     TSkImage( T_CanvasEncapsulation( oSelectionCanvas ).Image )
                   ) ;
        FreeObject( oSelectionCanvas ) ;
      end ;
    end
    else
      oSelectionCanvas := nil ;

    if assigned( Context.NativeDrawContext ) then exit ;

    T_CanvasEncapsulation( oCanvas ).Flush( useOpenGL ) ;
    if Context.BaseMapOnDemand and not assigned( Context.BaseMap ) then begin
      bmp := TGIS_Bitmap.Create ;
      Context.AssignBaseMap( bmp, True ) ;
    end
    else
      bmp := TGIS_Bitmap( Context.BaseMap ) ;
    bmp.SetData( BitmapFactorySkia,
                 TSkImage( T_CanvasEncapsulation( oCanvas ).Image )
               ) ;
  end ;

  procedure TGIS_RendererVclSkia.PrepareDrawCharts ;
  begin
    prevCanvas := FCanvas;
    prepareChartsCanvas;
    FCanvas := oLabelsCanvas ;
  end;

  procedure TGIS_RendererVclSkia.AfterDrawCharts ;
  begin
    FCanvas := prevCanvas ;
  end;

  procedure TGIS_RendererVclSkia.PrepareDrawLabels ;
  begin
    prevCanvas := FCanvas;
    prepareLabelsCanvas;
    FCanvas := oLabelsCanvas ;
  end;

  procedure TGIS_RendererVclSkia.AfterDrawLabels ;
  begin
    FCanvas := prevCanvas ;
  end;

  procedure TGIS_RendererVclSkia.LockTransparent(
    const _transparency : Integer
  ) ;
  begin
    assert( not assigned( oTransparentCanvas ) ) ;
    if _transparency = 100 then exit ;

    oTransparentCanvas := T_CanvasEncapsulation.Create( Width, Height,
                                                        Context.SourceDrawContext ) ;
    T_CanvasEncapsulation( oTransparentCanvas ).itransparency := _transparency ;
    FCanvas := oTransparentCanvas ;
  end ;

  procedure TGIS_RendererVclSkia.UnlockTransparent ;
  var
    paint : ISkPaint ;
    cl    : TGIS_Color ;
  begin
    if assigned( oTransparentCanvas ) then begin
      T_CanvasEncapsulation( oTransparentCanvas ).Flush( useOpenGL ) ;
      paint := TSkPaint.Create ;
      try
        cl := TGIS_Color.FromARGB(
          RoundS( T_CanvasEncapsulation( oTransparentCanvas ).iTransparency * 255 / 100 ),
          255, 255, 255
        ) ;
        paint.ColorFilter := TSkColorFilter.MakeBlend( cl.ToARGB,
                                                       TSkBlendMode.DestIn ) ;
        T_CanvasEncapsulation( oCanvas ).DrawImage(
          T_CanvasEncapsulation( oTransparentCanvas ).Image,
          RectF( 0, 0, Width, Height ),
          RectF( 0, 0, Width, Height ),
          paint
        ) ;
      finally
      end ;
      FreeObject( oTransparentCanvas ) ;
    end ;
    FCanvas := oCanvas ;
  end ;

  function TGIS_RendererVclSkia.getShapeOrigin : TPoint ;
  var
    ext       : TGIS_Extent ;
    rct_left  : Integer ;
    rct_top   : Integer ;
    drct_left : Double ;
    drct_top  : Double ;
  begin
    if not assigned( sourceShape )
    then
      Result := Point( 0, 0 )
    else begin
      ext := TGIS_Shape(sourceShape).ProjectedExtent ;

      drct_left := (  ext.XMin + FExtentX ) * FZoom ;
      drct_top  := ( -ext.YMax + FExtentY ) * FZoom ;

      if ( drct_left > -1073741824 ) and  // avoid range erros
         ( drct_top  > -1073741824 )
      then begin
        rct_left := RoundS( drct_left ) ;
        rct_top  := RoundS( drct_top  ) ;
      end
      else begin
        rct_left := RoundS( Max( drct_left, -1073741824 ))  ;
        rct_top  := RoundS( Max( drct_top , -1073741824 ))  ;
      end;
      Result := Point( rct_left, rct_top ) ;
    end;
  end ;

  function TGIS_RendererVclSkia.prepareStrokeStyle(
    const _style : TGIS_PenStyle ;
    const _width : Integer
  ) : ISkPathEffect ;
  begin
    Result := T_PenCache( penCache ).GetPathEffect( _style, _width ) ;
  end ;

  function TGIS_RendererVclSkia.prepareDashStyle(
    const _dash  : TGIS_DashArray ;
    const _width : Integer
  ) : ISkPathEffect ;
  begin
    Result := T_PenCache( penCache ).GetDashEffect( _dash, _width ) ;
  end ;

  function TGIS_RendererVclSkia.preparePatternFill(
    const _pattern : TGIS_BrushStyle ;
    const _color   : TGIS_Color
  ) : ISkImage ;
  begin
    Result := T_BrushCache( brushCache ).GetBitmap( _pattern, _color ) ;
  end ;

  procedure TGIS_RendererVclSkia.preparePen(
    const _canvas  : TObject         ;
    const _color   : TGIS_Color      ;
    const _style   : TGIS_PenStyle   ;
    const _bitmap  : TGIS_Bitmap     ;
    const _pattern : TGIS_BrushStyle ;
    const _origin  : TPoint          ;
    const _cap     : TGIS_LineCap    ;
    const _join    : TGIS_LineJoin   ;
    const _width   : Integer
  ) ;
  begin
    preparePen( _canvas, _color, _style, _bitmap, _pattern, _origin, _cap, _join, nil, _width ) ;
  end ;

  procedure TGIS_RendererVclSkia.preparePen(
    const _canvas  : TObject         ;
    const _color   : TGIS_Color      ;
    const _style   : TGIS_PenStyle   ;
    const _bitmap  : TGIS_Bitmap     ;
    const _pattern : TGIS_BrushStyle ;
    const _origin  : TPoint          ;
    const _cap     : TGIS_LineCap    ;
    const _join    : TGIS_LineJoin   ;
    const _dash    : TGIS_DashArray  ;
    const _width   : Integer
  ) ;
  begin
    if _width = 0 then
      T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Style := TGIS_PenStyle.Clear
    else
      T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Style := _style ;
    if assigned( _bitmap ) then begin
      T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Bitmap  := _bitmap ;
      T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Pattern := TGIS_BrushStyle.Solid ;
    end else begin
      T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Bitmap  := nil ;
      T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Pattern := _pattern ;
    end ;
    T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Width    := _width ;
    T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).Color    := _color ;
    T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).LineCap  := _cap   ;
    T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).LineJoin := _join  ;
    T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).LineDash := _dash  ;
    T_Pen(T_CanvasEncapsulation(_canvas).SkiaPen).BitmapOrigin := _origin ;
  end ;

  procedure TGIS_RendererVclSkia.prepareBrush(
    const _canvas  : TObject     ;
    const _color   : TGIS_Color  ;
    const _bitmap  : TGIS_Bitmap ;
    const _pattern : TGIS_BrushStyle ;
    const _origin  : TPoint
  ) ;
  begin
    if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then begin
      T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).Bitmap := _bitmap ;
      T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).Style := TGIS_BrushStyle.Solid ;
    end
    else begin
      T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).Bitmap := nil ;
      T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).Style := _pattern ;
    end ;
    if _color.ARGB = _color.RenderColor.ARGB then
      T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).Color := TGIS_Color.None
    else
      T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).Color := _color ;
    T_Brush(T_CanvasEncapsulation(_canvas).SkiaBrush).BitmapOrigin := _origin ;
  end ;

  procedure TGIS_RendererVclSkia.prepareFont(
    const _canvas : TObject ;
    const _name   : String  ;
    const _size   : Integer ;
    const _style  : TGIS_FontStyles ;
    const _color  : TGIS_Color
  ) ;
  begin
    T_Font(T_CanvasEncapsulation(_canvas).SkiaFont).Family := _name ;
    T_Font(T_CanvasEncapsulation(_canvas).SkiaFont).Size   :=
      RoundS( Max( 1.0, ( _size * PPI / 72.0 ) * ( FontScale / 100.0 ) ) ) ;
    T_Font(T_CanvasEncapsulation(_canvas).SkiaFont).Style := _style ;
    T_Font(T_CanvasEncapsulation(_canvas).SkiaFont).Color := _color ;
  end ;

  procedure TGIS_RendererVclSkia.drawEllipse(
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer ;
    const _canvas : TObject
  ) ;
  var
    rct : TRectF ;
  begin
    rct := RectF( _x1, _y1, _x2, _y2 ) ;
    if T_CanvasEncapsulation(_canvas).useBrush then
      T_CanvasEncapsulation(_canvas).FillOval( rct ) ;
    if T_CanvasEncapsulation(_canvas).usePen then
      T_CanvasEncapsulation(_canvas).DrawOval( rct ) ;
  end ;

  procedure TGIS_RendererVclSkia.drawPolyline(
    const _path : ISkPath ;
    const _shp  : TGIS_Shape
  ) ;
  begin
    if T_CanvasEncapsulation(FCanvas).usePen then begin
      T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
        Self,
        T_CanvasEncapsulation(FCanvas),
        _shp
      ) ;
      T_CanvasEncapsulation(FCanvas).DrawPath( _path ) ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.drawPolyPolygon(
    const _path : ISkPath ;
    const _shp  : TGIS_Shape
  ) ;
  begin
    if T_CanvasEncapsulation(FCanvas).useBrush then begin
      T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
        Self,
        T_CanvasEncapsulation(FCanvas),
        _shp
      ) ;
      T_CanvasEncapsulation(FCanvas).FillPath( _path ) ;
    end ;
    if T_CanvasEncapsulation(FCanvas).usePen then begin
      T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
        Self,
        T_CanvasEncapsulation(FCanvas),
        _shp
      ) ;
      T_CanvasEncapsulation(FCanvas).DrawPath( _path ) ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.drawMarker(
    const _pt         : TPoint ;
    const _style      : TGIS_MarkerStyle  ;
    const _size       : Integer ;
    const _shp        : TGIS_Shape ;
    const _asselected : Boolean
  ) ;
  var
    path  : ISkPathBuilder ;
    ppath : ISkPath ;
    isize : Integer ;

    function px( const x : Integer ) : Single ;
    begin
      if x <> 0 then Result := _pt.X + isize * x / 128
                else Result := _pt.X ;
    end ;

    function py( const y : Integer ) : Single ;
    begin
      if y <> 0 then Result := _pt.Y + isize * y / 128
                else Result := _pt.Y ;
    end ;

    function p( const x,y : Integer ) : TPointF ;
    var
      a, b : Single ;
    begin
      if x <> 0 then a := _pt.X + isize * x / 128
                else a := _pt.X ;
      if y <> 0 then b := _pt.Y - isize * y / 128
                else b := _pt.Y ;
     Result := PointF( a, b ) ;
    end ;

    procedure do_draw ;
    begin
      case _style of
        TGIS_MarkerStyle.Box           :
          begin
            path.AddRect( TRectF.Create( px(-64), py(-64), px( 64), py( 64) ) ) ;
          end ;
        TGIS_MarkerStyle.Circle        :
          begin
            path.AddOval( TRectF.Create( px(-64), py(-64), px( 64), py( 64) ) ) ;
          end ;
        TGIS_MarkerStyle.Cross         :
          begin
            path.MoveTo( p(-16, 64) ) ;
            path.LineTo( p( 16, 64) ) ;
            path.LineTo( p( 16, 16) ) ;
            path.LineTo( p( 64, 16) ) ;
            path.LineTo( p( 64,-16) ) ;
            path.LineTo( p( 16,-16) ) ;
            path.LineTo( p( 16,-64) ) ;
            path.LineTo( p(-16,-64) ) ;
            path.LineTo( p(-16,-16) ) ;
            path.LineTo( p(-64,-16) ) ;
            path.LineTo( p(-64, 16) ) ;
            path.LineTo( p(-16, 16) ) ;

            path.Close ;
          end ;
        TGIS_MarkerStyle.DiagCross     :
          begin
            path.MoveTo( p(-56, 34) ) ;
            path.LineTo( p(-34, 56) ) ;
            path.LineTo( p(  0, 22) ) ;
            path.LineTo( p( 34, 56) ) ;
            path.LineTo( p( 56, 34) ) ;
            path.LineTo( p( 22,  0) ) ;
            path.LineTo( p( 56,-34) ) ;
            path.LineTo( p( 34,-56) ) ;
            path.LineTo( p(  0,-22) ) ;
            path.LineTo( p(-34,-56) ) ;
            path.LineTo( p(-56,-34) ) ;
            path.LineTo( p(-22,  0) ) ;

            path.Close ;
          end ;
        TGIS_MarkerStyle.TriangleUp    :
          begin
            path.MoveTo( p(-64,-64) ) ;
            path.LineTo( p(  0, 64) ) ;
            path.LineTo( p( 64,-64) ) ;

            path.Close ;
          end ;
        TGIS_MarkerStyle.TriangleDown  :
          begin
            path.MoveTo( p( 0,-64) ) ;
            path.LineTo( p(-64, 64) ) ;
            path.LineTo( p( 64, 64) ) ;

            path.Close ;
          end ;
        TGIS_MarkerStyle.TriangleLeft  :
          begin
            path.MoveTo( p(-64, 0) ) ;
            path.LineTo( p( 64, 64) ) ;
            path.LineTo( p( 64,-64) ) ;

            path.Close ;
          end ;
        TGIS_MarkerStyle.TriangleRight :
          begin
            path.MoveTo( p(-64, 64) ) ;
            path.LineTo( p( 64, 0) ) ;
            path.LineTo( p(-64,-64) ) ;

            path.Close ;
          end ;
        end ;

    end ;
  begin
    if _size = 0 then exit ;
    isize := _size ;

    path := TSkPathBuilder.Create ;
    try

      if not _asselected then begin
        do_draw ;
      end ;

      if _asselected then begin
        path.AddRect(
          TRectF.Create( _pt.X - isize div 2, _pt.Y - isize div 2,
                         _pt.X + isize div 2, _pt.Y + isize div 2 )
        ) ;
      end ;

      ppath := path.Detach ;
      if T_CanvasEncapsulation(FCanvas).useBrush then begin
        T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
          Self,
          T_CanvasEncapsulation(FCanvas),
          _shp
        ) ;
        T_CanvasEncapsulation(FCanvas).FillPath( ppath ) ;
      end ;
      if T_CanvasEncapsulation(FCanvas).usePen then begin
        T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
          Self,
          T_CanvasEncapsulation(FCanvas),
          _shp
        ) ;
        T_CanvasEncapsulation(FCanvas).DrawPath( ppath ) ;
      end ;

    finally
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doShapePoint(
    const _shp           : TGIS_ShapePoint ;
    const _source        : TGIS_ShapePoint ;
    const _selectionOnly : Boolean
  ) ;
  var
    params_marker : TGIS_ParamsMarker ;
    angle         : Double ;

    procedure draw_point ;
    var
      pt     : TPoint  ;
      isize  : Integer ;
      origin : TPoint  ;
      offset : TPoint  ;
      usepen : Boolean ;
      cnv    : T_CanvasEncapsulation ;

      procedure prepare_point ;
      var
        dx, dy : Double ;
      begin
        _shp.GetPointEx( 0, 0, dx, dy ) ;

        // like MapToScreen does
        pt.X := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
        pt.Y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
      end ;

    begin
      T_CanvasEncapsulation(FCanvas).usePen := True ;
      T_CanvasEncapsulation(FCanvas).useBrush := True ;

      origin := getShapeOrigin ;
      offset := getOffsetPoint( params_marker ) ;

      try
        prepare_point ;

        if params_marker.Size <> GIS_RENDER_SIZE then
          isize := TwipsToPixels( params_marker.Size )
        else
          isize := TwipsToPixels( _shp.Layer.SelectionWidth ) ;

        if not _selectionOnly then begin

          if assigned( params_marker.Symbol ) then begin
             angle := params_marker.SymbolRotate ;
             case _shp.Layer.SymbolingMode of
              1 : begin
                    if params_marker.SymbolRotateIndirect then
                      angle := angle + Viewer.RotationAngle ;
                  end ;
              2 : begin
                    if params_marker.SymbolRotateIndirect or ( angle <> 0 ) then
                      angle := angle + Viewer.RotationAngle ;
                  end ;
              3 : begin
                    angle := angle + Viewer.RotationAngle ;
                  end ;
            end ;

            params_marker.Symbol.Prepare(
              Viewer,
              params_marker.Size,
              params_marker.Color,
              params_marker.OutlineColor,
              angle,
              0,
              TGIS_LabelPosition.MiddleCenter,
              True,
              Self
            ) ;
            params_marker.Symbol.Draw( pt.X, pt.Y ) ;
            params_marker.Symbol.Unprepare ;
          end
          else begin
            if ( params_marker.Style = TGIS_MarkerStyle.Circle ) and
               ( params_marker.OutlineStyle <> TGIS_PenStyle.Clear ) and
               ( isize < TwipsToPixels( params_marker.OutlineWidth ) ) then begin
              // special case when outline width > circle size
              // here just drawing circle in outline style over inner circle
              usepen := T_CanvasEncapsulation( FCanvas ).usePen ;
              T_CanvasEncapsulation( FCanvas ).usePen := False ;
              try
                prepareBrush(
                  FCanvas,
                  params_marker.Color,
                  params_marker.Bitmap,
                  params_marker.Pattern,
                  origin
                ) ;
                drawMarker( pt, params_marker.Style, isize,
                            _shp, False ) ;
                prepareBrush(
                  FCanvas,
                  params_marker.OutlineColor,
                  nil,
                  params_marker.OutlinePattern,
                  origin
                ) ;
                drawMarker( pt, params_marker.Style,
                            isize + TwipsToPixels( params_marker.OutlineWidth ),
                            _shp, False ) ;
              finally
                T_CanvasEncapsulation( FCanvas ).usePen := usepen ;
              end ;
            end
            else begin
              prepareBrush(
                FCanvas,
                params_marker.Color,
                params_marker.Bitmap,
                params_marker.Pattern,
                origin
              ) ;
              preparePen(
                FCanvas,
                params_marker.OutlineColor,
                params_marker.OutlineStyle,
                params_marker.OutlineBitmap,
                params_marker.OutlinePattern,
                origin,
                TGIS_LineCap.Flat,
                TGIS_LineJoin.Miter,
                TwipsToPixels( params_marker.OutlineWidth )
              ) ;
              drawMarker( pt, params_marker.Style, isize,
                          _shp, False ) ;
            end ;
          end ;
        end ;

        if _shp.IsSelected or _selectionOnly then
        begin
          prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                  or pextra, pextra, _shp ) ;

          if _shp.Layer.SelectionOutlineOnly then
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Clear,
              Point(0, 0)
            )
          else
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Solid,
              Point(0, 0)
            ) ;
          preparePen(
            oSelectionCanvas,
            colorSelection,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            Point(0, 0),
            TGIS_LineCap.Round,
            TGIS_LineJoin.Miter,
            TwipsToPixels( _shp.Layer.SelectionWidth )
          ) ;
          cnv := T_CanvasEncapsulation(FCanvas) ;
          try
            FCanvas := oSelectionCanvas ;
            drawMarker( pt, params_marker.Style, isize,
                        _shp, True ) ;
          finally
            FCanvas := cnv ;
          end ;
        end ;

      finally
      end ;
    end ;

  begin
    assert( assigned( _shp ) ) ;
    if FZoom <= 0 then
      Abort ;
    assert( not GisIsNoWorld( Viewer.Extent ) ) ;

    params_marker := _shp.Params.Marker ;
    if params_marker.Size = 0 then exit ;
    sourceShape := _source ;
    try
      try
        draw_point ;
      except
        // do nothing
      end ;
    finally
      sourceShape := nil ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doShapeMultiPoint(
    const _shp           : TGIS_ShapeMultiPoint ;
    const _source        : TGIS_ShapeMultiPoint ;
    const _selectionOnly : Boolean
  ) ;
  var
    params_marker : TGIS_ParamsMarker ;
    angle         : Double ;

    procedure draw_multi_point ;
    var
      pt     : TPoint ;
      isize  : Integer ;
      i      : Integer ;
      origin : TPoint ;
      offset : TPoint ;
      cnv    : T_CanvasEncapsulation ;

      procedure prepare_point( _idx : Integer ) ;
      var
        dx, dy : Double ;
      begin
        _shp.GetPointEx( 0, _idx, dx, dy ) ;
        pt.X := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
        pt.Y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
      end ;

    begin
      T_CanvasEncapsulation(FCanvas).usePen := True ;
      T_CanvasEncapsulation(FCanvas).useBrush := True ;

      origin := getShapeOrigin ;
      offset := getOffsetPoint( params_marker ) ;

      if params_marker.Size <> GIS_RENDER_SIZE then
        isize := TwipsToPixels( params_marker.Size )
      else
        isize := TwipsToPixels( _shp.Layer.SelectionWidth ) ;

      if not _selectionOnly then begin

        if assigned( params_marker.Symbol ) then begin
          angle := params_marker.SymbolRotate ;
          case _shp.Layer.SymbolingMode of
            1 : begin
                  if params_marker.SymbolRotateIndirect then
                    angle := angle + Viewer.RotationAngle ;
                end ;
            2 : begin
                  if params_marker.SymbolRotateIndirect or ( angle <> 0 ) then
                    angle := angle + Viewer.RotationAngle ;
                end ;
            3 : begin
                  angle := angle + Viewer.RotationAngle ;
                end ;
          end ;

          params_marker.Symbol.Prepare(
            Viewer,
            params_marker.Size,
            params_marker.Color,
            params_marker.OutlineColor,
            angle,
            0,
            TGIS_LabelPosition.MiddleCenter,
            True,
            Self
          ) ;
          for i := 0 to _shp.GetNumPoints - 1 do begin
            prepare_point( i ) ;
            params_marker.Symbol.Draw( pt.X, pt.Y ) ;
          end ;
          params_marker.Symbol.Unprepare ;
        end

        else begin

          prepareBrush(
            FCanvas,
            params_marker.Color,
            params_marker.Bitmap,
            params_marker.Pattern,
            origin
          ) ;
          preparePen(
            FCanvas,
            params_marker.OutlineColor,
            params_marker.OutlineStyle,
            params_marker.OutlineBitmap,
            params_marker.OutlinePattern,
            origin,
            TGIS_LineCap.Flat,
            TGIS_LineJoin.Round,
            TwipsToPixels( params_marker.OutlineWidth )
          ) ;
          for i := 0 to _shp.GetNumPoints - 1 do begin
            prepare_point( i ) ;
            drawMarker( pt, params_marker.Style, isize,
                        _shp, False ) ;
          end ;
        end ;
      end ;

      if _shp.IsSelected or _selectionOnly then begin
        prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                or pextra, pextra, _shp ) ;

        if _shp.Layer.SelectionOutlineOnly then
          prepareBrush(
            oSelectionCanvas,
            colorSelection,
            nil,
            TGIS_BrushStyle.Clear,
            Point(0, 0)
          )
        else
          prepareBrush(
            oSelectionCanvas,
            colorSelection,
            nil,
            TGIS_BrushStyle.Solid,
            Point(0, 0)
          ) ;
        preparePen(
          oSelectionCanvas,
          colorSelection,
          TGIS_PenStyle.Solid,
          nil,
          TGIS_BrushStyle.Solid,
          Point( 0, 0 ),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          TwipsToPixels( _shp.Layer.SelectionWidth )
        ) ;
        cnv := T_CanvasEncapsulation(FCanvas) ;
        try
          FCanvas := oSelectionCanvas ;
          for i := 0 to _shp.GetNumPoints -1 do begin
            prepare_point( i ) ;
            drawMarker( pt, params_marker.Style, isize,
                        _shp, True ) ;
          end ;
        finally
          FCanvas := cnv ;
        end ;
      end ;
    end ;

  begin
    assert( assigned( _shp ) ) ;
    if FZoom <= 0 then
      Abort ;
    assert( not GisIsNoWorld( Viewer.Extent ) ) ;

    params_marker := _shp.Params.Marker ;
    if params_marker.Size = 0 then exit ;
    sourceShape := _source ;
    try
      try
        draw_multi_point ;
      except
        // do nothing
      end ;
    finally
      sourceShape := nil ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doShapeLine(
    const _shp           : TGIS_ShapeArc ;
    const _source        : TGIS_ShapeArc ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  var
    params_line : TGIS_ParamsLine ;

    procedure draw_line ;
    var
      part_no   : Integer ;
      mypath    : ISkPathBuilder ;
      ppath     : ISkPath ;
      pt_count  : Integer ;
      selwidth  : Integer ;
      max_tlrnc : Integer ;
      origin    : TPoint ;
      offset    : TPoint ;

      function prepare_drawbufpart(
        const _max_tlrnc : Integer ;
        const _part_no   : Integer
      ) : Integer ;
      var
        part_size  : Integer ;
        cur_tlrnc  : Integer ;
        point_no   : Integer ;
        dx, dy     : Double  ;
        pt_x       : Single ;
        pt_y       : Single ;
        last_pt_x  : Single ;
        last_pt_y  : Single ;
        tmp_tlrnc  : Integer ;
        isize      : Integer ;
      begin
        Result := 0 ;

        part_size := _shp.GetPartSize( _part_no ) ;
        if part_size <= 0 then exit ;

        cur_tlrnc := _max_tlrnc ;
        repeat
          isize := 0  ;

          mypath := TSkPathBuilder.Create ;

          last_pt_x := 0 ;
          last_pt_y := 0 ;
          for point_no := 0 to part_size - 1 do begin
            _shp.GetPointEx( _part_no, point_no, dx, dy ) ;

            // like MapToScreen does
            pt_x := (  dx + FExtentX ) * FZoom + offset.X ;
            pt_y := ( -dy + FExtentY ) * FZoom + offset.Y ;

            if point_no = 0 then begin
              mypath.MoveTo( pt_x, pt_y ) ;
              last_pt_x := pt_x ;
              last_pt_y := pt_y ;
              inc( isize ) ;
            end
            else if point_no < part_size -1 then begin
              // basic simplifier
              tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                   Abs( pt_y - last_pt_y )
                                 ) ;
              if tmp_tlrnc > cur_tlrnc then begin // basic simplifier
                mypath.LineTo( pt_x, pt_y ) ;
                last_pt_x := pt_x ;
                last_pt_y := pt_y ;
                inc( isize ) ;
              end ;
            end
            else begin
              mypath.LineTo( pt_x, pt_y ) ;
              inc( isize ) ;
            end ;
          end ;

          cur_tlrnc := cur_tlrnc + 1 ;
        until isize < GDI_MAXPOINT_COUNT ;

        Result := isize ;
      end ;

      procedure draw_line_symbol ;
      var
        ar : TGIS_DrawBufF ;
        i  : Integer ;
        el : TSkPathIteratorElem ;
      begin
        params_line.Symbol.Prepare( Viewer,
                                    params_line.Width,
                                    params_line.Color,
                                    params_line.OutlineColor,
                                    params_line.SymbolRotate,
                                    params_line.SymbolGap,
                                    TGIS_LabelPosition.MiddleCenter,
                                    False,
                                    Self
                                ) ;

        // prepare an array of points
        SetLength( ar, pt_count ) ;
        i := 0 ;
        for el in ppath.GetIterator(False) do begin
          if i < pt_count then
            case el.Verb of
              TSkPathVerb.Move:
                begin
                  ar[i] := PointF( el.Points[0].X, el.Points[0].Y ) ;
                  inc( i ) ;
                end ;
              TSkPathVerb.Line:
                begin
                  ar[i] := PointF( el.Points[1].X, el.Points[1].Y ) ;
                  inc( i ) ;
                end ;
            end ;
        end ;
        TGIS_SymbolLineHelper.DrawLine( Viewer, ar, params_line.Symbol, pt_count ) ;

        params_line.Symbol.Unprepare ;
      end ;

    begin
      T_CanvasEncapsulation(FCanvas).usePen := True ;
      T_CanvasEncapsulation(FCanvas).useBrush := True ;

      origin := getShapeOrigin ;
      offset := getOffsetPoint( params_line ) ;

      for part_no := 0 to _shp.GetNumParts - 1 do
      begin
        mypath := TSkPathBuilder.Create ;
        try
          pt_count := prepare_drawbufpart( iTolerance, part_no ) ;
          if prepare_drawbufpart( iTolerance, part_no ) < 1 then exit ;
          ppath := myPath.Detach ;

          if not _selectionOnly then begin

            // symbol line
            if assigned( params_line.Symbol ) then
            begin
              draw_line_symbol ;
            end

            else begin

              // first draw outline
              if ( Integer( _outlineMode )
                 and
                 Integer( TGIS_RendererMultipassMode.Outline )
              ) <> 0
              then begin
                if ( params_line.OutlineStyle <> TGIS_PenStyle.Clear ) and
                   ( params_line.OutlineWidth <> 0                   ) then
                begin
                  // line cap mode based on multipass rendering
                  if ( Integer( _outlineMode )
                       and
                       Integer( TGIS_RendererMultipassMode.Line )
                     ) = 0
                  then
                    preparePen(
                      FCanvas,
                      params_line.OutlineColor,
                      params_line.OutlineStyle,
                      params_line.OutlineBitmap,
                      params_line.OutlinePattern,
                      origin,
                      TGIS_LineCap.Round,
                      TGIS_LineJoin.Round,
                      TwipsToPixels( params_line.Width )
                      + 2 * TwipsToPixels( params_line.OutlineWidth )
                    )
                  else
                    preparePen(
                      FCanvas,
                      params_line.OutlineColor,
                      params_line.OutlineStyle,
                      params_line.OutlineBitmap,
                      params_line.OutlinePattern,
                      origin,
                      TGIS_LineCap.Flat,
                      TGIS_LineJoin.Round,
                      TwipsToPixels( params_line.Width )
                      + 2 * TwipsToPixels( params_line.OutlineWidth )
                    ) ;
                  drawPolyline( ppath, _shp ) ;
                end ;
              end ;

              // then line itself
              if ( Integer( _outlineMode )
                   and
                   Integer( TGIS_RendererMultipassMode.Line )
                 ) <> 0 then
              begin
                preparePen(
                    FCanvas,
                    params_line.Color,
                    params_line.Style,
                    params_line.Bitmap,
                    params_line.Pattern,
                    origin,
                    TGIS_LineCap.Round,
                    TGIS_LineJoin.Round,
                    TwipsToPixels( params_line.Width )
                ) ;
                drawPolyline( ppath, _shp ) ;
              end ;
            end ;
          end ;

          if _shp.IsSelected or _selectionOnly then begin
            prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                    or pextra, pextra, _shp ) ;

            selwidth := TwipsToPixels( _shp.Layer.SelectionWidth ) ;
            if not _shp.Layer.SelectionOutlineOnly then begin
              with GisCommonExtent( Viewer.VisibleExtent, _shp.Extent ) do
                max_tlrnc := TruncS( Max( iToleranceSel,
                                          Min( selwidth div 4 ,
                                               Max( XMax - XMin, YMax - YMin )
                                               * FZoom / 10
                                             )
                                        )
                                   ) ;
            end
            else
              max_tlrnc := iTolerance ;

            if (params_line.Width <> GIS_RENDER_SIZE) and
               (params_line.OutlineWidth <> GIS_RENDER_SIZE) then
            begin
              selwidth := selwidth + TwipsToPixels( params_line.Width )
                          + 2* TwipsToPixels( params_line.OutlineWidth ) ;
            end ;

            mypath := TSkPathBuilder.Create ;

            if prepare_drawbufpart( max_tlrnc, part_no ) < 1 then exit ;
            ppath := mypath.Detach ;

            T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.PathEffect
              := nil ;
            T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.Color
              := colorSelection.ToARGB ;
            T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.StrokeWidth
              := selwidth ;
            T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.StrokeJoin
               := TSkStrokeJoin.Round ;
            T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.StrokeCap
              := TSkStrokeCap.Round ;
            T_CanvasEncapsulation(oSelectionCanvas).skipStroke := False ;

            T_CanvasEncapsulation(oSelectionCanvas).DrawPath( ppath ) ;
          end ;
        finally
        end ;
      end ;
    end ;

  begin
    assert( assigned( _shp ) ) ;
    if FZoom <= 0 then
      Abort ;
    assert( not GisIsNoWorld( Viewer.Extent ) ) ;

    if inEdit then begin
      drawEditingLines( _shp ) ;
      exit ;
    end ;

    params_line := _shp.Params.Line ;
    sourceShape := _source ;
    try
      try
        draw_line ;
      except
        // do nothing
      end ;
    finally
      sourceShape := nil ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doShapePolygon(
    const _shp           : TGIS_ShapePolygon ;
    const _source        : TGIS_ShapePolygon ;
    const _selectionOnly : Boolean ;
    const _part          : Integer
  ) ;
  var
    params_area : TGIS_ParamsArea ;

    procedure draw_polygon ;
    var
      origin    : TPoint ;
      offset    : TPoint ;
      mypath    : ISkPathBuilder ;
      ppath     : ISkPath ;
      //part_buf  : TGIS_IntegerArray ;
      selwidth  : Integer ;
      //max_tlrnc : Integer ;
      draw_buf  : TGIS_DrawBufF ;

      function prepare_drawbuf(
        const _max_tlrnc : Integer
      ) : Integer ;
      var
        part_size  : Integer    ;
        cur_size   : Integer    ;
        cur_tlrnc  : Integer    ;
        part_no    : Integer    ;
        point_no   : Integer    ;
        dx, dy     : Double     ;
        pt_x       : Single     ;
        pt_y       : Single     ;
        last_pt_x  : Single     ;
        last_pt_y  : Single     ;
        first_pt_x : Single     ;
        first_pt_y : Single     ;
        tmp_tlrnc  : Integer    ;
        //iparts     : Integer    ;
        isize      : Integer    ;
        start      : Integer    ;
        stop       : Integer    ;
      begin
       // SetLength( part_buf, _shp.GetNumParts  ) ;

        cur_tlrnc := _max_tlrnc ;
        repeat
          //iparts  := 0 ;
          isize   := 0 ;

          if _part = -1 then begin
            start := 0 ;
            stop  := _shp.GetNumParts - 1 ;
          end
          else begin
            start := _part ;
            stop  := _part ;
          end ;

          mypath := TSkPathBuilder.Create ;

          for part_no := start to stop do begin
            part_size := _shp.GetPartSize( part_no ) ;

            cur_size := 0 ;
            first_pt_x := 0 ;
            first_pt_y := 0 ;
            last_pt_x := 0 ;
            last_pt_y := 0 ;
            for point_no := 0 to part_size - 1 do begin
              _shp.GetPointEx( part_no, point_no, dx, dy ) ;

              // like MapToScreen does
              pt_x :=  (  dx + FExtentX ) * FZoom + offset.X ;
              pt_y :=  ( -dy + FExtentY ) * FZoom + offset.Y ;

              if point_no = 0 then begin
                first_pt_x := pt_x ;
                first_pt_y := pt_y ;
                last_pt_x  := pt_x ;
                last_pt_y  := pt_y ;
              end
              else if point_no < part_size -1 then begin
                // basic simplifier
                tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                     Abs( pt_y - last_pt_y )
                                   ) ;
                if tmp_tlrnc > cur_tlrnc then begin // basic simplifier
                  if cur_size = 0 then begin
                    mypath.MoveTo( first_pt_x, first_pt_y ) ;
                    inc( isize ) ;
                    inc( cur_size ) ;
                  end;

                  mypath.LineTo( pt_x, pt_y ) ;
                  last_pt_x := pt_x ;
                  last_pt_y := pt_y ;
                  inc( isize ) ;
                  inc( cur_size ) ;
                end ;
              end
              else begin
                if cur_size = 0 then begin
                  tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                       Abs( pt_y - last_pt_y )
                                     ) ;
                  if tmp_tlrnc > cur_tlrnc then  begin
                    mypath.MoveTo( first_pt_x, first_pt_y ) ;
                    inc( isize ) ;
                    inc( cur_size ) ;
                  end
                  else begin
                    mypath.MoveTo( pt_x  ,  pt_y   ) ;
                    inc( isize ) ;
                    inc( cur_size ) ;
                    mypath.LineTo( pt_x  ,  pt_y+1 ) ;
                    inc( isize ) ;
                    inc( cur_size ) ;
                    mypath.LineTo( pt_x+1,  pt_y+1 ) ;
                    inc( isize ) ;
                    inc( cur_size ) ;
                    mypath.LineTo( pt_x+1,  pt_y   ) ;
                    inc( isize ) ;
                    inc( cur_size ) ;
                  end ;
                end ;

                if cur_size > 0  then begin
                  mypath.LineTo( pt_x, pt_y ) ;
                  inc( isize ) ;
                  inc( cur_size ) ;
                  mypath.Close ;

                 // part_buf[iparts] := cur_size ;
                  //inc(iparts);
                end;
              end ;
            end ;
          end ;

          cur_tlrnc := cur_tlrnc + 1 ;
        until isize < GDI_MAXPOINT_COUNT ;

        Result := isize ;
      end ;

      function prepare_drawbufpart(
        const _max_tlrnc : Integer ;
        const _part_no   : Integer
      ) : Integer ;
      var
        part_size  : Integer ;
        cur_tlrnc  : Integer ;
        point_no   : Integer ;
        dx, dy     : Double  ;
        pt_x       : Single ;
        pt_y       : Single ;
        last_pt_x  : Single ;
        last_pt_y  : Single ;
        tmp_tlrnc  : Integer ;
        isize      : Integer ;
      begin
        Result := 0 ;
        part_size := _shp.GetPartSize( _part_no ) ;
        if part_size <= 0 then exit ;

        SetLength( draw_buf, part_size ) ;

        cur_tlrnc := _max_tlrnc ;
        last_pt_x := 0 ;
        last_pt_y := 0 ;
        repeat
          isize := 0 ;

          for point_no := 0 to part_size - 1 do begin
            // translate all points in part to screen coordinates
            _shp.GetPointEx( _part_no, point_no, dx, dy ) ;

            // like MapToScreen does
            pt_x :=  (  dx + FExtentX ) * FZoom + offset.X ;
            pt_y :=  ( -dy + FExtentY ) * FZoom + offset.Y ;

            if point_no = 0 then begin
              draw_buf[isize] := PointF( pt_x, pt_y );
              last_pt_x := pt_x ;
              last_pt_y := pt_y ;
              inc( isize ) ;
            end
            else if point_no < part_size -1 then begin
              // basic simplifier
              tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                   Abs( pt_y - last_pt_y )
                                 ) ;
              if tmp_tlrnc > cur_tlrnc then begin // basic simplifier
                draw_buf[isize] := PointF( pt_x, pt_y );
                last_pt_x := pt_x ;
                last_pt_y := pt_y ;
                inc( isize ) ;
              end ;
            end
            else begin
              draw_buf[isize] := PointF( pt_x, pt_y );
              inc( isize ) ;
            end ;
          end ;
          cur_tlrnc := cur_tlrnc + 1 ;
          assert( isize < GDI_MAXPOINT_COUNT ) ;
        until isize < GDI_MAXPOINT_COUNT ;

        SetLength( draw_buf, isize ) ;
        Result := isize ;
      end ;

      procedure draw_polygon_symbolfill ;
      var
        fill_gap_x  : Integer ;
        fill_gap_y  : Integer ;
        fill_size_x : Integer ;
        fill_size_y : Integer ;
        img : ISkImage ;
        cnv : T_CanvasEncapsulation ;
      begin
        params_area.Symbol.Prepare( Viewer,
                                    params_area.SymbolSize,
                                    params_area.Color,
                                    params_area.Color,
                                    params_area.SymbolRotate,
                                    0,
                                    TGIS_LabelPosition.MiddleCenter,
                                    False,
                                    Self
                                  ) ;
        try
          fill_gap_x  := TwipsToPixels( params_area.SymbolGap ) ;
          fill_gap_y  := TwipsToPixels( params_area.SymbolGap ) ;
          fill_size_x := params_area.Symbol.Width  + fill_gap_x ;
          fill_size_y := params_area.Symbol.Height + fill_gap_y ;

          cnv := T_CanvasEncapsulation(FCanvas) ;
          // create a new canvas; only for rendering one symbol with gaps
          FCanvas := T_CanvasEncapsulation.Create( fill_size_x, fill_size_y, nil ) ;
          try
            T_CanvasEncapsulation(FCanvas).Font.Name := cnv.Font.Name ;
            T_CanvasEncapsulation(FCanvas).Font.Style := cnv.Font.Style ;
            T_CanvasEncapsulation(FCanvas).Font.Size  := cnv.Font.Size  ;

            params_area.Symbol.Draw( params_area.Symbol.Width div 2,
                                     params_area.Symbol.Height div 2 ) ;
            T_CanvasEncapsulation(FCanvas).Flush(
              Assigned( T_CanvasEncapsulation(FCanvas).OpenGL )
            ) ;
            img := T_CanvasEncapsulation(FCanvas).Image ;
          finally
            FreeObject( FCanvas ) ;
            FCanvas := cnv ;
          end ;

          // create bitmap brush
          T_Brush( T_CanvasEncapsulation(FCanvas).SkiaBrush ).SelectBrush(
            T_CanvasEncapsulation(FCanvas),
            img,
            origin
          ) ;

          // bitmap fill
          T_CanvasEncapsulation(FCanvas).FillPath( ppath ) ;
        finally
          params_area.Symbol.Unprepare ;
        end ;
      end ;

      procedure draw_polygon_hatchfill ;
      var
        fill_gap  : TPoint ;
        fill_size : TPoint ;
        bmp  : TBitmap ;
        tbmp : TGIS_Bitmap ;
        bmp0 : TBitmap ;
        prjex : TGIS_Extent ;
        ptg_x : Double ;
        ptg_y : Double ;
        fill_offset : TPoint ;
        corner1_x : Double ;
        corner1_y : Double ;
        corner2_x : Double ;
        corner2_y : Double ;
        rvis : TRectF ;
        rct : TRect ;
        rct_left : Integer ;
        rct_top : Integer ;
        rct_right : Integer ;
        rct_bottom : Integer ;
        x, y : Integer ;
        img : ISkImage ;
        cnv : T_CanvasEncapsulation ;
      begin
        prjex := _shp.ProjectedExtent ;

        ptg_x := (  prjex.XMin + FExtentX ) * FZoom ;
        ptg_y := ( -prjex.YMax + FExtentY ) * FZoom ;

        fill_offset.X := TruncS( ptg_x ) ;
        fill_offset.Y := TruncS( ptg_y ) ;

        corner1_x := (  prjex.XMin + FExtentX ) * FZoom ;
        corner1_y := ( -prjex.YMax + FExtentY ) * FZoom ;
        corner2_x := (  prjex.XMax + FExtentX ) * FZoom ;
        corner2_y := ( -prjex.YMin + FExtentY ) * FZoom ;

        rvis := T_CanvasEncapsulation(FCanvas).Canvas.GetLocalClipBounds ;
        if corner1_x < 0 then
          rct_Left := fill_offset.X
        else
          rct_Left := RoundS( corner1_x ) ;
        if corner2_x > rvis.Right then
          rct_Right := RoundS( rvis.Right )
        else
          rct_Right := RoundS( corner2_x ) ;
        if corner1_y < 0 then
          rct_Top := fill_offset.Y
        else
          rct_Top := RoundS( corner1_y ) ;
        if corner2_y > rvis.Bottom then
          rct_Bottom := RoundS( rvis.Bottom )
        else
          rct_Bottom := RoundS( corner2_y ) ;

        rct := Rect( rct_Left, rct_Top, rct_Right, rct_Bottom ) ;

        params_area.Symbol.Prepare( Viewer,
                                    params_area.SymbolSize,
                                    params_area.Color,
                                    params_area.Color,
                                    params_area.SymbolRotate,
                                    params_area.SymbolGap,
                                    TGIS_LabelPosition.MiddleCenter,
                                    False,
                                    Self
                                  ) ;
        if (rct.Width > 0) and (rct.Height > 0) then begin
          cnv := T_CanvasEncapsulation(FCanvas) ;
          // create a new canvas; only for rendering one symbol with gaps

          FCanvas := T_CanvasEncapsulation.Create( rct.Width, rct.Height, nil ) ;
          try
            T_CanvasEncapsulation(FCanvas).Font.Name := cnv.Font.Name ;
            T_CanvasEncapsulation(FCanvas).Font.Style := cnv.Font.Style ;
            T_CanvasEncapsulation(FCanvas).Font.Size  := cnv.Font.Size  ;

            params_area.Symbol.Draw( rct.Width, rct.Height ) ;
            T_CanvasEncapsulation(FCanvas).Flush(
              Assigned( T_CanvasEncapsulation(FCanvas).OpenGL )
            ) ;
            img := T_CanvasEncapsulation(FCanvas).Image ;
          finally
            FreeObject( FCanvas ) ;
            FCanvas := cnv ;
          end ;

          T_Brush( T_CanvasEncapsulation(FCanvas).SkiaBrush ).SelectBrush(
              T_CanvasEncapsulation(FCanvas),
              img,
              origin
            ) ;

          // bitmap fill
          T_CanvasEncapsulation(FCanvas).FillPath( ppath ) ;
        end ;

        params_area.Symbol.Unprepare ;
      end ;

      procedure draw_polygon_symboloutline ;
      var
        i     : Integer ;
        r     : Integer ;
        start : Integer ;
        stop  : Integer ;
      begin
        params_area.OutlineSymbol.Prepare(
          Viewer,
          params_area.OutlineWidth,
          params_area.OutlineColor,
          params_area.OutlineColor,
          params_area.OutlineSymbolRotate,
          params_area.OutlineSymbolGap,
          TGIS_LabelPosition.MiddleCenter,
          False,
          Self
        ) ;

        if _part = -1 then begin
          start := 0 ;
          stop  := _shp.GetNumParts - 1 ;
        end
        else begin
          start := _part ;
          stop  := _part ;
        end ;

        // draw parts
        for i := start to stop do begin
          r := prepare_drawbufpart( iTolerance, i ) ;
          if r < 1 then continue ;
          TGIS_SymbolLineHelper.DrawLine(
            Viewer, draw_buf, params_area.OutlineSymbol, r ) ;
        end ;

        params_area.OutlineSymbol.Unprepare ;
      end ;

    begin
      T_CanvasEncapsulation(FCanvas).usePen := True ;
      T_CanvasEncapsulation(FCanvas).useBrush := True ;

      origin := getShapeOrigin ;
      offset := getOffsetPoint( params_area ) ;

      mypath := TSkPathBuilder.Create ;
      try
        // prepare points
        if  prepare_drawbuf( iTolerance ) < 1 then exit ;
        mypath.FillType := TSkPathFillType.EvenOdd ;
        ppath := myPath.Detach ;

        if not _selectionOnly then begin

          // inside first

          // symbol fill
          if assigned( params_area.Symbol ) then
          begin
            if params_area.Symbol is TGIS_SymbolHatch then
              draw_polygon_hatchfill
            else
              draw_polygon_symbolfill ;
          end

          // bitmap fill
          // pattern fill
          // standard full
          else begin

            T_CanvasEncapsulation(FCanvas).usePen := False ;
            try
              if ( not TGIS_Bitmap.IsNilOrEmpty( params_area.Bitmap ) ) and
                 assigned( _shp.Layer )                                 and
                 ( _shp.Layer.View3D.Mode <> TGIS_3DLayerType.Dem )
              then
                prepareBrush(
                  FCanvas,
                  params_area.Color,
                  params_area.Bitmap,
                  params_area.Pattern,
                  origin
                )
              else
                prepareBrush(
                  FCanvas,
                  params_area.Color,
                  nil,
                  params_area.Pattern,
                  origin
                ) ;
              drawPolyPolygon( ppath, _shp ) ;
            finally
              T_CanvasEncapsulation(FCanvas).usePen := True ;
            end ;

          end ;

          // line background for dashed lines
          if ( params_area.OutlineSymbol         <> nil                   ) and
             ( params_area.OutlineStyle          <> TGIS_PenStyle.Solid   ) and
             ( params_area.OutlineBackcolor.ARGB <> TGIS_Color.Crazy.ARGB ) then
          begin
            preparePen(
              FCanvas,
              params_area.OutlineBackcolor,
              TGIS_PenStyle.Solid,
              nil,
              TGIS_BrushStyle.Solid,
              Point( 0, 0 ),
              TGIS_LineCap.Round,
              TGIS_LineJoin.Round,
              TwipsToPixels( params_area.OutlineWidth )
            ) ;
            T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
              Self,
              T_CanvasEncapsulation(FCanvas),
              _shp
            ) ;
            T_CanvasEncapsulation(FCanvas).DrawPath( ppath ) ;
          end ;

          // outline symbol fill
          if assigned( params_area.OutlineSymbol ) then
          begin
            draw_polygon_symboloutline ;
          end

          // outline pattern fill
          else if ( params_area.OutlinePattern <> TGIS_BrushStyle.Clear ) and
                  ( params_area.OutlinePattern <> TGIS_BrushStyle.Solid ) then
          begin
            preparePen(
              FCanvas,
              params_area.OutlineColor,
              params_area.OutlineStyle,
              params_area.OutlineBitmap,
              params_area.OutlinePattern,
              origin,
              TGIS_LineCap.Round,
              TGIS_LineJoin.Round,
              TwipsToPixels( params_area.OutlineWidth )
            ) ;
            T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
              Self,
              T_CanvasEncapsulation(FCanvas),
              _shp
            ) ;
            T_CanvasEncapsulation(FCanvas).DrawPath( ppath ) ;
          end

          // outline bitmap fill
          // standard outline
          else begin
            T_CanvasEncapsulation(FCanvas).useBrush := False ;
            try
              if ( ( params_area.OutlineWidth = 0 ) or
                   ( params_area.OutlineStyle = TGIS_PenStyle.Clear ) )
                 and
                 ( not assigned(  params_area.Symbol ) ) and
                 ( not assigned(  params_area.Bitmap ) ) and
                 ( params_area.Pattern = TGIS_BrushStyle.Solid )
              then begin
                if params_area.Color.A = 255 then begin
                  // 1 pixel outline with the bitmap color
                  preparePen(
                    FCanvas,
                    params_area.Color,
                    TGIS_PenStyle.Solid,
                    nil  ,
                    TGIS_BrushStyle.Solid,
                    origin,
                    TGIS_LineCap.Flat,
                    TGIS_LineJoin.Round,
                    TwipsToPixels( 1 )
                  ) ;
                  drawPolyPolygon( ppath, _shp ) ;
                end ;
              end
              else
              if ( params_area.OutlineWidth <> 0 ) and
                 ( params_area.OutlineStyle <> TGIS_PenStyle.Clear )
              then begin
                preparePen(
                  FCanvas,
                  params_area.OutlineColor,
                  params_area.OutlineStyle,
                  params_area.OutlineBitmap,
                  params_area.OutlinePattern,
                  origin,
                  TGIS_LineCap.Flat,
                  TGIS_LineJoin.Round,
                  TwipsToPixels( params_area.OutlineWidth )
                ) ;
                drawPolyPolygon( ppath, _shp ) ;
              end
              else begin
                // OutlineWidth = 0 and pattern fill - do nothing
              end ;
            finally
              T_CanvasEncapsulation(FCanvas).useBrush := true ;
            end ;
          end ;
        end ;

        if _shp.IsSelected or _selectionOnly then begin
          prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                  or pextra, pextra, _shp ) ;

          selwidth := TwipsToPixels( _shp.Layer.SelectionWidth ) ;
          (*if not _shp.Layer.SelectionOutlineOnly then begin
            with GisCommonExtent( Viewer.VisibleExtent, _shp.Extent ) do
              max_tlrnc := TruncS( Max( iToleranceSel,
                                        Min( selwidth div 4 ,
                                             Max( XMax - XMin, YMax - YMin )
                                             * FZoom / 10
                                        )
                                   )
                                 ) ;
          end
          else
            max_tlrnc := iTolerance ;*)

          mypath := TSkPathBuilder.Create ;

          if  prepare_drawbuf( iTolerance ) < 1 then exit ;
          mypath.FillType := TSkPathFillType.EvenOdd ;
          ppath := mypath.Detach ;

          if params_area.OutlineWidth <> GIS_RENDER_SIZE then
            selwidth := selwidth +
                        TwipsToPixels( params_area.OutlineWidth ) ;

          if not _shp.Layer.SelectionOutlineOnly then begin
            T_Brush( T_CanvasEncapsulation(oSelectionCanvas).SkiaBrush ).Paint.PathEffect
              := nil;
            T_Brush( T_CanvasEncapsulation(oSelectionCanvas).SkiaBrush ).Paint.Color
              := colorSelection.ToARGB ;
            T_CanvasEncapsulation(oSelectionCanvas).skipFill := False ;
            T_CanvasEncapsulation(oSelectionCanvas).FillPath( ppath ) ;
          end ;

          T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.PathEffect
            := nil ;
          T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.Color
            := colorSelection.ToARGB ;
          T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.StrokeWidth
            := selwidth ;
          T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.StrokeJoin
            := TSkStrokeJoin.Round ;
          T_Pen( T_CanvasEncapsulation(oSelectionCanvas).SkiaPen ).Paint.StrokeCap
            := TSkStrokeCap.Round ;
          T_CanvasEncapsulation(oSelectionCanvas).skipStroke := False ;
          T_CanvasEncapsulation(oSelectionCanvas).DrawPath( ppath ) ;
        end ;
      finally
      end ;
    end ;

  begin
    assert( assigned( _shp ) ) ;
    if FZoom <= 0 then
      Abort ;
    assert( not GisIsNoWorld( Viewer.Extent ) ) ;

    if inEdit then begin
      drawEditingLines( _shp ) ;
      exit ;
    end ;

    params_area := _shp.Params.Area ;
    sourceShape := _source ;
    try
      try
        draw_polygon ;
      except
        // do nothing
      end ;
    finally
      sourceShape := nil ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doShapeMultiPatch(
    const _shp           : TGIS_ShapePolygon ;
    const _source        : TGIS_ShapePolygon ;
    const _selectionOnly : Boolean
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to TGIS_ShapeMultiPatch( _shp ).GetNumParts-1 do
      doShapePolygon(
        TGIS_ShapeMultiPatch( _shp    ),
        TGIS_ShapeMultiPatch( _source ),
        _selectionOnly,
        i
      ) ;
  end ;

  procedure TGIS_RendererVclSkia.doShapeComplex(
    const _shp           : TGIS_ShapeComplex ;
    const _source        : TGIS_ShapeComplex ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _shp.ShapesCount-1 do
      RenderShape( _shp.GetShape(i), _source,
                   _selectionOnly, _outlineMode ) ;
  end ;

  procedure TGIS_RendererVclSkia.RenderShape(
    const _shp           : TObject ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  begin
    RenderShape( _shp, nil, _selectionOnly, _outlineMode ) ;
  end ;

  procedure TGIS_RendererVclSkia.RenderShape(
    const _shp           : TObject ;
    const _source        : TObject ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  var
    cnv : T_CanvasEncapsulation ;
  begin
    if _shp = nil then exit ;
    if ( not inEdit ) and
       ( not assigned( oCanvas ) ) then exit ;

    cnv := T_CanvasEncapsulation(FCanvas) ;
    if inEdit then
      FCanvas := oEditCanvas
    else if assigned( oTransparentCanvas ) then
      FCanvas := oTransparentCanvas
    else
      FCanvas := oCanvas ;
    try
      case TGIS_Shape( _shp ).ShapeType of
        TGIS_ShapeType.Point      : doShapePoint(
                                      TGIS_ShapePoint( _shp ),
                                      TGIS_ShapePoint( _source ),
                                      _selectionOnly
                                    ) ;
        TGIS_ShapeType.MultiPoint : doShapeMultiPoint(
                                      TGIS_ShapeMultiPoint( _shp ),
                                      TGIS_ShapeMultiPoint( _source ),
                                      _selectionOnly
                                    ) ;
        TGIS_ShapeType.Arc        : doShapeLine(
                                      TGIS_ShapeArc( _shp ),
                                      TGIS_ShapeArc( _source ),
                                      _selectionOnly,
                                      _outlineMode
                                    ) ;
        TGIS_ShapeType.Polygon    : doShapePolygon(
                                      TGIS_ShapePolygon( _shp ),
                                      TGIS_ShapePolygon( _source ),
                                      _selectionOnly,
                                      -1
                                    ) ;
        TGIS_ShapeType.MultiPatch : doShapeMultiPatch(
                                      TGIS_ShapeMultiPatch( _shp ),
                                      TGIS_ShapeMultiPatch( _source ),
                                      _selectionOnly
                                    ) ;
        TGIS_ShapeType.Complex    : doShapeComplex(
                                      TGIS_ShapeComplex( _shp ),
                                      TGIS_ShapeComplex( _source ),
                                      _selectionOnly,
                                      _outlineMode
                                    )
        else assert( False, GIS_RS_ERR_UNTESTED ) ;
      end ;
    finally
      FCanvas := cnv ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doLabelPoint(
    const _shp        : TGIS_Shape ;
    const _savePoints : Boolean ;
    var   _points     : TGIS_DrawBuf
  ) ;
  var
    txt             : String     ;
    rct             : TRect      ;
    rct_br          : TPoint     ;
    pt_origin_x     : Integer    ;
    pt_origin_y     : Integer    ;
    angle           : Double     ;
    ssin            : Double     ;
    scos            : Double     ;
    gap             : Integer    ;
    gap2            : Integer    ;
    cpoint          : TGIS_Point ;
    offset          : TPoint     ;
    params_label    : TGIS_ParamsLabel     ;
    params_marker   : TGIS_ParamsMarker    ;
    label_color     : TGIS_Color           ;
    label_alignment : TGIS_LabelAlignment  ;
    label_positions : TGIS_LabelPositions  ;
(*    //spargs          : TGIS_ShapePosEventArgs ; *)

    function text_extent(
      const _text      : String ;
      const _rbp       : TPoint ;
      const _alignment : TGIS_LabelAlignment
    ) : TPoint ;
    var
      lbl  : TGIS_HtmlLabel ;
      rect : TRect ;
    begin
      lbl := TGIS_HtmlLabel.Create( self, _text, _alignment,
                                    _rbp.X, _rbp.Y
                                  ) ;
      try
        rect := lbl.BoundingBox ;
      finally
        FreeObject( lbl ) ;
      end ;
      Result.X := rect.Right  - rect.Left + 1 ;
      Result.Y := rect.Bottom - rect.Top  + 1 ;
    end ;

    function text_out(
      const _text      : String  ;
      const _rect      : TRect   ;
      const _shadow    : Boolean ;
      const _alignment : TGIS_LabelAlignment ;
      const _angle     : Double  ;
      const _origin    : TPoint
    ) : TRect ;
    var
      shadow_width : Integer ;
      lbl  : TGIS_HtmlLabel ;
    begin
      lbl := TGIS_HtmlLabel.Create( self, _text, _alignment,
                                    _rect.Right - _rect.Left + 1,
                                    _rect.Bottom - _rect.Top + 1
                                  ) ;
      try
        shadow_width := 0 ;
        if _shadow then begin
          // prepare size of outline in pixels
          // generally outline must be composed of 1 screen pixels (1/96 of inch)
          // for printer outline will must be a bit stronger then usual
          shadow_width := Max( 1, PPI div 96 ) ;
        end ;
        lbl.Draw( _rect, 0, _origin, shadow_width, label_color ) ;
      finally
        FreeObject( lbl ) ;
      end ;
    end ;

    function readHashCode : Integer ;
    begin
     if assigned( _shp.Layer.ParentLayer ) then
       Result := _shp.Layer.ParentLayer.GetHashCode
     else
       Result := _shp.Layer.GetHashCode ;
    end ;

    function draw_label(
      _pos : TGIS_LabelPosition
    ) : Boolean ;
    var
      pt_x            : Integer ;
      pt_y            : Integer ;
      start_x         : Integer ;
      start_y         : Integer ;
      labext_xmin     : Double  ;
      labext_ymin     : Double  ;
      labext_xmax     : Double  ;
      labext_ymax     : Double  ;
      dx              : Double  ;
      dy              : Double  ;
      xgap            : Integer ;
      ygap            : Integer ;
      rct_tmp         : TRect   ;
      rct_txt         : TRect   ;
      rct_txt_x       : Integer ;
      rct_txt_y       : Integer ;
      rct_loc_left    : Integer ;
      rct_loc_top     : Integer ;
      rct_loc_right   : Integer ;
      rct_loc_bottom  : Integer ;
      wdth            : Integer ;
      pt_a            : TGIS_Point ;
      pt_b            : TGIS_Point ;
      pt_c            : TGIS_Point ;
      pt_d            : TGIS_Point ;
      pt_left_top     : TPoint ;
      pt_right_top    : TPoint ;
      pt_right_bottom : TPoint ;
      pt_left_bottom  : TPoint ;
      sym             : TGIS_SymbolAbstract ;
      sym_rct_label   : TRectF ;
      sym_rct_boundry : TRectF ;
      sym_scale       : Double ;
      sym_scale_x     : Double ;
      bshield         : Boolean ;
      bshadow         : Boolean ;
      txt_off_x       : Single ;
      txt_off_y       : Single ;
      sym_off_x       : Single ;
      sym_off_y       : Single ;
      srect           : TRectF ;
    begin
      if angle = 0 then begin
        pt_x := pt_origin_x ;
        pt_y := pt_origin_y ;
      end
      else begin
        // transformation will be based on pt_origin
        pt_x := 0 ;
        pt_y := 0 ;
      end ;

      txt_off_x := 0 ;
      txt_off_y := 0 ;
      sym_off_x := 0 ;
      sym_off_y := 0 ;

      // calculate label shieled
      sym := params_label.Shield ;
      if assigned( sym ) then begin
        bshield := True ;

        sym_rct_label   := sym.ShieldLabel ;
        sym_rct_boundry := sym.ShieldBounds ;

        sym_scale := rct.Height / sym_rct_label.Height ;

        sym_scale_x := 1 ;

        // fit text in ShielLabel space
        if sym_scale * sym_rct_label.Width < rct.Width then begin
          sym_scale_x :=  rct.Width / ( sym_scale * sym_rct_label.Width ) ;
        end ;

        sym.Prepare( Viewer,
                     RoundS(-sym_scale*sym.NativeSize),
                     sym_scale_x,
                     params_label.Color,
                     params_label.OutlineColor,
                     0,
                     0,
                     TGIS_SymbolPosition.MiddleCenter,
                     False,
                     nil
                   ) ;

        rct_tmp := Rect( 0,
                         0,
                         RoundS( sym_scale * sym_scale_x * sym_rct_boundry.Width ),
                         RoundS( sym_scale * sym_rct_boundry.Height )
                       ) ;
        // compute the label rectangle; make sure it fits the label extent
        rct_txt_x := RoundS( sym_scale * sym_scale_x *
                             ( sym_rct_label.Left - sym_rct_boundry.Left )
                           ) ;
        rct_txt_y := RoundS( sym_scale *
                             ( sym_rct_label.Top - sym_rct_boundry.Top )
                           ) ;
        rct_txt := Rect( rct_txt_x,
                         rct_txt_y,
                         Max( RoundS( sym_scale * sym_scale_x *
                                      ( sym_rct_label.Right - sym_rct_boundry.Left )
                                    ),
                              rct_txt_x + rct.Width
                            ),
                         Max( RoundS( sym_scale *
                                      ( sym_rct_label.Bottom - sym_rct_boundry.Top )
                                    ),
                              rct_txt_y + rct.Height
                            )
                       ) ;

        sym_off_x := sym_scale * sym_scale_x *
                     (
                       sym.NativeWidth / 2 -
                       ( sym_rct_boundry.Left + sym_rct_boundry.Width / 2 )
                     ) ;
        sym_off_y := sym_scale *
                     (
                       sym.NativeHeight / 2 -
                       ( sym_rct_boundry.Top + sym_rct_boundry.Height / 2 )
                     ) ;
      end
      else
        bshield := False ;

      // background shield
      if not bshield then begin
        wdth := TwipsToPixels( params_label.OutlineWidth ) div 2 ;

        if ( params_label.Pattern <> TGIS_BrushStyle.Clear )
           or
           ( params_label.OutlineStyle <> TGIS_PenStyle.Clear ) then
        begin
          xgap := CanvasTextEm( 2 ) ;
          ygap := CanvasTextEm( 1 ) ;

          rct_tmp := Rect( 0,
                           0,
                           rct.Right  + 2*xgap + 2*wdth,
                           rct.Bottom + 2*ygap + 2*wdth
                         ) ;
          rct_txt := rct_tmp ;
          txt_off_x := xgap + wdth ;
          txt_off_y := ygap + wdth ;
        end
        else begin
          rct_tmp := rct ;
          rct_txt := rct ;
        end;
      end;

      start_x := 0 ;
      start_y := 0 ;
      case _pos of
        TGIS_LabelPosition.UpLeft       : begin
                                            start_x := pt_x - rct_tmp.Right  - gap ;
                                            start_y := pt_y - rct_tmp.Bottom - gap ;
                                          end ;
        TGIS_LabelPosition.UpCenter     : begin
                                            start_x := pt_x - rct_tmp.Right div 2 ;
                                            start_y := pt_y - rct_tmp.Bottom - gap ;
                                          end ;
        TGIS_LabelPosition.UpRight      : begin
                                            start_x := pt_x + gap ;
                                            start_y := pt_y - rct_tmp.Bottom - gap ;
                                          end ;
        TGIS_LabelPosition.MiddleLeft   : begin
                                            start_x := pt_x - rct_tmp.Right  - gap ;
                                            start_y := pt_y - rct_tmp.Bottom div 2 ;
                                          end ;
        TGIS_LabelPosition.MiddleCenter : begin
                                            start_x := pt_x - rct_tmp.Right  div 2 ;
                                            start_y := pt_y - rct_tmp.Bottom div 2 ;
                                          end ;
        TGIS_LabelPosition.MiddleRight  : begin
                                            start_x := pt_x + gap ;
                                            start_y := pt_y - rct_tmp.Bottom div 2 ;
                                          end ;
        TGIS_LabelPosition.DownLeft     : begin
                                            start_x := pt_x - rct_tmp.Right  - gap ;
                                            start_y := pt_y + gap ;
                                          end ;
        TGIS_LabelPosition.DownCenter   : begin
                                            start_x := pt_x - rct_tmp.Right  div 2 ;
                                            start_y := pt_y + gap ;
                                          end ;
        TGIS_LabelPosition.DownRight    : begin
                                            start_x := pt_x + gap ;
                                            start_y := pt_y + gap ;
                                          end ;
      end ;

      (*if _shp.Layer.AssignedLabelPosEvent then begin
        spargs := TGIS_ShapePosEventArgs.Create(
                    _shp, _pos,
                    System.Drawing.Point.Create(
                      start_x, start_y
                    ),
                    System.Drawing.Rectangle.Create(
                      rct.Left,
                      rct.Top,
                      rct.Right - rct.Left,
                      rct.Bottom - rct.Top
                    )
                  ) ;
        try
          _shp.Layer.RaiseLabelPosEvent( Self, spargs ) ;
        finally
          start_x := spargs.Point.X ;
          start_y := spargs.Point.Y ;
          FreeObject( spargs ) ;
        end ;
      end ;*)

      rct_loc_left   := rct_tmp.Left   + start_x ;
      rct_loc_top    := rct_tmp.Top    + start_y ;
      rct_loc_right  := rct_tmp.Right  + start_x ;
      rct_loc_bottom := rct_tmp.Bottom + start_y ;

      if angle <> 0 then begin
        SinCos( angle, ssin, scos ) ;

        dx := rct_loc_left - pt_x ;
        dy := rct_loc_top  - pt_y ;
        with pt_a do begin
          X := ( dx * scos  -  dy * ssin ) ;
          Y := ( dx * ssin  +  dy * scos ) ;
        end ;

        dx := rct_loc_left   - pt_x ;
        dy := rct_loc_bottom - pt_y ;
        with pt_b do begin
          X := ( dx * scos  -  dy * ssin ) ;
          Y := ( dx * ssin  +  dy * scos ) ;
        end ;

        dx := rct_loc_right - pt_x ;
        dy := rct_loc_top   - pt_y ;
        with pt_c do begin
          X := ( dx * scos  -  dy * ssin ) ;
          Y := ( dx * ssin  +  dy * scos ) ;
        end ;

        dx := rct_loc_right  - pt_x ;
        dy := rct_loc_bottom - pt_y ;
        with pt_d do begin
          X := ( dx * scos  -  dy * ssin ) ;
          Y := ( dx * ssin  +  dy * scos ) ;
        end ;

        // combining GisMovePoint and ScreenToMapEx
        with pt_a do begin
          dx := X + pt_origin_x ;
          dy := Y + pt_origin_y ;
          X :=   dx / FZoom - FExtentX ;
          Y := - dy / FZoom + FExtentY ;
        end ;
        pt_left_top := Point( RoundS(dx), RoundS(dy) ) ;
        with pt_b do begin
          dx := X + pt_origin_x ;
          dy := Y + pt_origin_y ;
          X :=   dx / FZoom - FExtentX ;
          Y := - dy / FZoom + FExtentY ;
        end ;
        pt_left_bottom := Point( RoundS(dx), RoundS(dy) ) ;
        with pt_c do begin
          dx := X + pt_origin_x ;
          dy := Y + pt_origin_y ;
          X :=   dx / FZoom - FExtentX ;
          Y := - dy / FZoom + FExtentY ;
        end ;
        pt_right_top := Point( RoundS(dx), RoundS(dy) ) ;
        with pt_d do begin
          dx := X + pt_origin_x ;
          dy := Y + pt_origin_y ;
          X :=   dx / FZoom - FExtentX ;
          Y := - dy / FZoom + FExtentY ;
        end ;
        pt_right_bottom := Point( RoundS(dx), RoundS(dy) ) ;
      end
      else begin
        labext_xmin :=    rct_loc_left   / FZoom - FExtentX ;
        labext_xmax :=    rct_loc_right  / FZoom - FExtentX ;
        labext_ymin :=  - rct_loc_bottom / FZoom + FExtentY ;
        labext_ymax :=  - rct_loc_top    / FZoom + FExtentY ;

        with pt_a do begin
          X := labext_xmin ;
          Y := labext_ymin ;
        end ;
        with pt_b do begin
          X := labext_xmin ;
          Y := labext_ymax ;
        end ;
        with pt_c do begin
          X := labext_xmax ;
          Y := labext_ymin ;
        end ;
        with pt_d do begin
          X := labext_xmax ;
          Y := labext_ymax ;
        end ;
        pt_left_top     := Point( rct_loc_left,  rct_loc_top ) ;
        pt_right_top    := Point( rct_loc_right, rct_loc_top ) ;
        pt_right_bottom := Point( rct_loc_right, rct_loc_bottom ) ;
        pt_left_bottom  := Point( rct_loc_left,  rct_loc_bottom ) ;
      end ;

      if params_label.Allocator then begin
        if FTiled then begin
          if not Viewer.LabelsReg.IsAny(
                  readHashCode,
                  _shp.Uid
                )
          then begin
            if ( not params_label.Duplicates ) and
               Viewer.LabelsReg.IsDuplicated( txt )
            then begin
              if assigned( sym ) then
                sym.Unprepare ;

              Result := False ;
              exit ;
            end ;
          end ;
        end ;

        if not Viewer.LabelsReg.AllocateEx(
                 pt_a, pt_b, pt_c, pt_d,
                 readHashCode,
                 _shp.Uid,
                 1.5/FZoom
               )
        then begin
          if assigned( sym ) then
            sym.Unprepare ;

          Result := False ;
          exit ;
        end ;
      end ;
      Result := True ;

      if ( not _shp.Params.Labels.Duplicates ) then
        Viewer.LabelsReg.AddDuplicated( txt )  ;

      if assigned( sym ) then begin
        sym.Draw( rct_loc_left / 2 + rct_loc_right  / 2 + sym_off_x,
                  rct_loc_top  / 2 + rct_loc_bottom / 2 + sym_off_y
                ) ;
        sym.Unprepare ;
      end;

      srect := RectF(
                 rct_txt.Left   + rct_loc_left,
                 rct_txt.Top    + rct_loc_top ,
                 rct_txt.Right  + rct_loc_left,
                 rct_txt.Bottom + rct_loc_top
               ) ;
      // draw background
      try
        if params_label.Pattern <> TGIS_BrushStyle.Clear then begin
          T_CanvasEncapsulation(oLabelsCanvas).useBrush := True ;
          prepareBrush(
            oLabelsCanvas,
            params_label.Color,
            params_label.Bitmap,
            params_label.Pattern,
            Point(0, 0)
          ) ;

          T_Brush( T_CanvasEncapsulation(oLabelsCanvas).SkiaBrush).SelectBrush(
            Self,
             T_CanvasEncapsulation(oLabelsCanvas),
            _shp
          ) ;
          T_CanvasEncapsulation(oLabelsCanvas).FillRect( srect ) ;
        end ;

        if params_label.OutlineStyle <> TGIS_PenStyle.Clear then begin
           T_CanvasEncapsulation(oLabelsCanvas).usePen := True ;
          preparePen(
                oLabelsCanvas,
                params_label.OutlineColor,
                params_label.OutlineStyle,
                params_label.OutlineBitmap,
                params_label.OutlinePattern,
                Point(0, 0),
                TGIS_LineCap.Round,
                TGIS_LineJoin.Round,
                TwipsToPixels( params_label.OutlineWidth )
          ) ;

          T_Pen( T_CanvasEncapsulation(oLabelsCanvas).SkiaPen).SelectPen(
            Self,
            T_CanvasEncapsulation(oLabelsCanvas),
            _shp
          ) ;
          T_CanvasEncapsulation(oLabelsCanvas).DrawRect( srect ) ;
        end ;
      finally
        T_CanvasEncapsulation(oLabelsCanvas).usePen := True ;
        T_CanvasEncapsulation(oLabelsCanvas).useBrush := True ;
      end;

      if _savePoints then begin
        SetLength( _points, 5 ) ;
        _points[0] := pt_left_top ;
        _points[1] := pt_right_top ;
        _points[2] := pt_right_bottom ;
        _points[3] := pt_left_bottom ;
        _points[4] := pt_left_top ;
      end;

      if ( params_label.Pattern = TGIS_BrushStyle.Clear  )
         and
         TGIS_Bitmap.IsNilOrEmpty( params_label.Bitmap )
         and
         ( params_label.Color.ARGB <> params_label.FontColor.ARGB )
      then
        bshadow := True
      else
        bshadow := False ;

      text_out(
        txt,
        Rect( RoundS( rct_txt.Left   + rct_loc_left + txt_off_x ),
              RoundS( rct_txt.Top    + rct_loc_top  + txt_off_y ),
              RoundS( rct_txt.Right  + rct_loc_left - txt_off_x ),
              RoundS( rct_txt.Bottom + rct_loc_top  - txt_off_y )
            ),
        bshadow,
        label_alignment,
        0,             // we do not use rotation here
        Point( 0, 0 )  // we do not use rotation here
      ) ;
    end ;

  begin
    params_label    := _shp.Params.Labels ;
    if not params_label.Visible then exit ;

    txt := _shp.GetLabel ;
    if IsStringEmpty( txt ) then exit ;

    if ( not FTiled ) and ( not params_label.Duplicates ) and
      Viewer.LabelsReg.IsDuplicated( txt )
    then
      exit ;

    params_marker   := _shp.Params.Marker ;
    label_color     := params_label.Color ;
    label_alignment := params_label.Alignment ;
    label_positions := params_label.Position  ;

    // prepare rectangle for label
    rct := Rect( 0, 0,
                 TwipsToPixels( params_label.Width,  0 ),
                 TwipsToPixels( params_label.Height, 0 )
               ) ;
    if ( ( rct.Right  - rct.Left ) <= 0 ) or
       ( ( rct.Bottom - rct.Top  ) <= 0 ) then exit ;

    cpoint := _shp.PointOnShape ;
    offset := getOffsetPoint( params_label ) ;
    // like MapToScreen does
    pt_origin_x := RoundS( (  cpoint.X + FExtentX ) * FZoom ) + offset.X ;
    pt_origin_y := RoundS( ( -cpoint.Y + FExtentY ) * FZoom ) + offset.Y ;

    CanvasFont.Name  := params_label.FontName  ;
    CanvasFont.Size  := TwipsToPoints( params_label.FontSize ) ;
    CanvasFont.Style := params_label.FontStyle ;
    if params_label.FontColor.ARGB = TGIS_Color.RenderColor.ARGB then
      CanvasFont.Color := TGIS_Color.None
    else
      CanvasFont.Color := params_label.FontColor ;

    if CanvasFont.Size < 1 then
      exit ;

    angle := params_label.Rotate ;
    case _shp.Layer.LabelingMode of
      1 : begin
            if params_label.RotateIndirect then
              angle := params_label.Rotate + Viewer.RotationAngle ;
          end ;
      2 : begin
            if params_label.RotateIndirect or ( angle <> 0 ) then
             angle := params_label.Rotate + Viewer.RotationAngle ;
          end ;
      3 : begin
            angle := params_label.Rotate + Viewer.RotationAngle ;
          end ;
    end ;

    if angle <> 0 then begin
      CanvasSetTransformation( angle, pt_origin_x, pt_origin_y );
      ssin := Sin( angle ) ;
      scos := Cos( angle ) ;
    end;

    try

      // calculate the size of the rectangle
      rct_br.X := rct.Right  ;
      rct_br.Y := rct.Bottom ;
      rct_br   := text_extent(
                    txt, rct_br, label_alignment
                  ) ;
      rct := Rect( 0, 0, rct_br.X-1, rct_br.Y-1 ) ;
      if ( ( rct.Right  - rct.Left ) <= 0 ) or
         ( ( rct.Bottom - rct.Top  ) <= 0 ) then exit ;

      if rct.Right >  TwipsToPixels( params_label.Width,  0 ) then
         rct_br.X  := TwipsToPixels( params_label.Width,  0 ) ;
      if rct.Bottom > TwipsToPixels( params_label.Height, 0 ) then
         rct_br.Y :=  TwipsToPixels( params_label.Height, 0 ) ;

      // calculate offset of rectangle
      if _shp.ShapeType in [TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint] then begin
        gap := TwipsToPixels( params_marker.Size ) div 2 ;
        gap := gap + TwipsToPixels( params_marker.OutlineWidth ) ;
      end
      else
        gap := 0 ;

      if ( _shp.Params.Chart.Size <> 0 ) and
         not IsStringEmpty( _shp.Params.Chart.Values ) then
      begin
        gap2 := TwipsToPixels( _shp.Params.Chart.Size ) div 2 ;
        if gap2 > gap then gap := gap2 ;
      end ;
      gap := gap + TwipsToPixels( params_label.OutlineWidth ) ;

      if TGIS_LabelPosition.MiddleCenter in label_positions then
        if draw_label( TGIS_LabelPosition.MiddleCenter ) then exit ;
      if TGIS_LabelPosition.MiddleRight  in label_positions then
        if draw_label( TGIS_LabelPosition.MiddleRight  ) then exit ;
      if TGIS_LabelPosition.MiddleLeft   in label_positions then
        if draw_label( TGIS_LabelPosition.MiddleLeft   ) then exit ;
      if TGIS_LabelPosition.UpCenter     in label_positions then
        if draw_label( TGIS_LabelPosition.UpCenter     ) then exit ;
      if TGIS_LabelPosition.DownCenter   in label_positions then
        if draw_label( TGIS_LabelPosition.DownCenter   ) then exit ;
      if TGIS_LabelPosition.UpRight      in label_positions then
        if draw_label( TGIS_LabelPosition.UpRight      ) then exit ;
      if TGIS_LabelPosition.UpLeft       in label_positions then
        if draw_label( TGIS_LabelPosition.UpLeft       ) then exit ;
      if TGIS_LabelPosition.DownRight    in label_positions then
        if draw_label( TGIS_LabelPosition.DownRight    ) then exit ;
      if TGIS_LabelPosition.DownLeft     in label_positions then
        if draw_label( TGIS_LabelPosition.DownLeft     ) then exit ;
    finally
      if angle <> 0 then
        CanvasClearTransformation ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doLabelArc(
    const _shp        : TGIS_ShapeArc ;
    const _savePoints : Boolean ;
    var   _points     : TGIS_DrawBuf
  ) ;
  var
    params_label : TGIS_ParamsLabel   ;
    part_no      : Integer ;
    txt          : String  ;
  begin
    params_label := _shp.Params.Labels ;

    if ( params_label.Alignment <> TGIS_LabelAlignment.Follow ) then begin
      doLabelPoint( _shp, _savePoints, _points ) ;
      exit ;
    end ;

    txt := _shp.GetLabel ;

    for part_no := 0 to _shp.GetNumParts - 1 do
      T_CanvasEncapsulation(FCanvas).DrawArcLabel(
        _shp, part_no, txt, FTiled, _savePoints, _points
      ) ;

  end ;

  procedure TGIS_RendererVclSkia.RenderLabel(
    const _shp : TObject
  ) ;
  var
    cnv : T_CanvasEncapsulation ;
    arr : TGIS_DrawBuf ;
  begin
    prepareLabelsCanvas ;

    cnv := T_CanvasEncapsulation(FCanvas) ;
    sourceShape := _shp ;
    try
      FCanvas := oLabelsCanvas ;
      case TGIS_Shape( _shp ).ShapeType of
        TGIS_ShapeType.Point       :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        TGIS_ShapeType.MultiPoint  :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        TGIS_ShapeType.Arc         :
            doLabelArc( TGIS_ShapeArc( _shp ), False, arr ) ;
        TGIS_ShapeType.Polygon     :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        TGIS_ShapeType.MultiPatch  :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        TGIS_ShapeType.Complex  :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        else
            assert( False, GIS_RS_ERR_UNTESTED ) ;
      end;
    finally
      sourceShape := nil ;
      FCanvas := cnv ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.RenderLabel(
    const _shp    : TObject ;
    var   _points : TGIS_DrawBuf
  ) ;
  var
    cnv : T_CanvasEncapsulation ;
  begin
    prepareLabelsCanvas ;

    cnv := T_CanvasEncapsulation(FCanvas) ;
    try
      FCanvas := oLabelsCanvas ;
      SetLength( _points, 0 ) ;
      case TGIS_Shape( _shp ).ShapeType of
        TGIS_ShapeType.Point       :
            doLabelPoint( TGIS_ShapeArc( _shp ), True, _points ) ;
        TGIS_ShapeType.MultiPoint  :
            doLabelPoint( TGIS_ShapeArc( _shp ), True, _points ) ;
        TGIS_ShapeType.Arc         :
            doLabelArc( TGIS_ShapeArc( _shp ), True, _points ) ;
        TGIS_ShapeType.Polygon     :
            doLabelPoint( TGIS_ShapeArc( _shp ), True, _points ) ;
        TGIS_ShapeType.MultiPatch  :
            doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
        TGIS_ShapeType.Complex  :
            doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
        else
            assert( False, GIS_RS_ERR_UNTESTED ) ;
      end;
    finally
      FCanvas := cnv ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.doChart(
    const _shp : TGIS_Shape
  ) ;
  var
    cpoint : TGIS_Point  ;
    pt_x   : Integer ;
    pt_y   : Integer ;
  begin

    assert( not GisIsNoWorld( Viewer.Extent ) ) ;

    if high( _shp.Params.Chart.ValuesInternal ) < 2 then exit ;

    cpoint := _shp.PointOnShape ;
    with cpoint do begin
      // like MapToScreen does
      pt_x := RoundS( (  X + FExtentX ) * FZoom ) ;
      pt_y := RoundS( ( -Y + FExtentY ) * FZoom ) ;
    end ;

    if assigned( _shp.Params.Chart.ColorsInternal ) and
         ( high( _shp.Params.Chart.ColorsInternal ) >= 0 )  then
      GisDrawChart( Self, Point( pt_x, pt_y ),
                    TwipsToPixels( _shp.Params.Chart.Size ),
                    _shp.Params.Chart.Style,
                    _shp.Params.Chart.ValuesInternal,
                    _shp.Params.Chart.ColorsInternal
                  )
    else
      GisDrawChart( Self, Point( pt_x, pt_y ),
                  TwipsToPixels( _shp.Params.Chart.Size ),
                  _shp.Params.Chart.Style,
                  _shp.Params.Chart.ValuesInternal
                )
  end ;

  procedure TGIS_RendererVclSkia.RenderChart(
    const _shp : TObject
  ) ;
  var
    cnv : T_CanvasEncapsulation ;
  begin
    prepareChartsCanvas ;

    cnv := T_CanvasEncapsulation(FCanvas) ;
    try
      FCanvas := oChartsCanvas ;
      doChart( TGIS_Shape( _shp ) ) ;
    finally
      FCanvas := cnv ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.RenderShapeFlashed(
    const _shp : TObject
  ) ;
  begin
    flashed := true ;
    try
      RenderShape( _shp, True ) ;
    finally
      flashed := false ;
    end ;
  end;

  function TGIS_RendererVclSkia.RenderBitmapBegin : TObject ;
  begin
    Result := nil ;
  end;

  procedure TGIS_RendererVclSkia.RenderBitmapEnd(
    const _handle   : TObject
  ) ;
  begin
    // do nothing
  end;

  procedure TGIS_RendererVclSkia.RenderBitmap(
    const _handle : TObject           ;
    const _bmp    : TGIS_Pixels       ;
    const _size   : TPoint            ;
    const _dst    : TRect             ;
    const _format : TGIS_BitmapFormat ;
    const _order  : TGIS_BitmapLinesOrder
  ) ;
  var
    bmp : TGIS_Bitmap ;
    buf : TGIS_Pixels ;
  begin
    bmp := TGIS_Bitmap.Create( _size.X, _size.Y, BitmapFactory ) ;
    try
      bmp.LockPixels( buf, True, _format, _order ) ;
      try
        Move( _bmp[0], buf[0], Length( _bmp ) * 4  )
      finally
        bmp.UnlockPixels ;
      end;
      RenderBitmap( _handle, bmp, _dst, False ) ;
    finally
      FreeObject( bmp ) ;
    end;
  end ;

  procedure TGIS_RendererVclSkia.RenderBitmap(
    const _handle    : TObject ;
    const _bmp       : TGIS_Bitmap ;
    const _dst       : TRect ;
    const _antialias : Boolean
  ) ;
  var
    cnv : T_CanvasEncapsulation ;
  begin
    if assigned( oTransparentCanvas ) then
      cnv := T_CanvasEncapsulation(oTransparentCanvas)
    else
      cnv := T_CanvasEncapsulation(FCanvas) ;

    cnv.DrawImage(
      TSkImage( _bmp.GetData( BitmapFactory ) ),
      RectF( 0, 0, _bmp.Width, _bmp.Height ),
      RectF( _dst.Left, _dst.Top, _dst.Right, _dst.Bottom)
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.drawEditingLines(
    const _shp : TGIS_Shape
  ) ;
  var
    shp      : TGIS_Shape ;
    part_no  : Integer ;
    mypath   : ISkPathBuilder ;
    ppath    : ISkPath ;

    function prepare_drawbufpart(
      const _max_tlrnc : Integer ;
      const _part_no   : Integer
    ) : Integer ;
    var
      part_size  : Integer ;
      cur_tlrnc  : Integer ;
      point_no   : Integer ;
      dx, dy     : Double  ;
      pt_x       : Double ;
      pt_y       : Double ;
      last_pt_x  : Double ;
      last_pt_y  : Double ;
      first_pt_x : Double ;
      first_pt_y : Double ;
      tmp_tlrnc  : Integer ;
      isize      : Integer ;
    begin
      Result := 0 ;

      part_size := _shp.GetPartSize( _part_no ) ;
      if part_size <= 0 then exit ;

      cur_tlrnc := _max_tlrnc ;
//?      repeat
        isize := 0  ;

 (*       T_skiaUtils.ClearPath( mypath ) ;*)

        first_pt_x := 0 ;
        first_pt_y := 0 ;
        last_pt_x := 0 ;
        last_pt_y := 0 ;
        for point_no := 0 to part_size - 1 do begin
          _shp.GetPointEx( _part_no, point_no, dx, dy ) ;

          // like MapToScreen does
          pt_x :=  ( (  dx + FExtentX ) * FZoom ) ;
          pt_y :=  ( ( -dy + FExtentY ) * FZoom ) ;

          if point_no = 0 then begin
            last_pt_x := pt_x ;
            last_pt_y := pt_y ;
            first_pt_x := pt_x ;
            first_pt_y := pt_y ;
          end
          else if point_no < part_size -1 then begin
            // basic simplifier
            tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                 Abs( pt_y - last_pt_y )
                               ) ;
            if tmp_tlrnc > cur_tlrnc then begin // basic simplifier
              if isize = 0 then begin
                mypath.MoveTo( first_pt_x, first_pt_y ) ;
                inc( isize ) ;
              end ;

              mypath.LineTo( pt_x, pt_y ) ;
              last_pt_x := pt_x ;
              last_pt_y := pt_y ;
              inc( isize ) ;
            end ;
          end
          else begin
            if isize = 0 then begin
              tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                   Abs( pt_y - last_pt_y )
                                 ) ;
              if tmp_tlrnc > cur_tlrnc then  begin
                mypath.MoveTo( first_pt_x, first_pt_y ) ;
                inc( isize ) ;
              end ;
            end ;

            if isize > 0  then begin
              mypath.LineTo( pt_x, pt_y ) ;
              inc( isize ) ;
            end;

          end ;
        end ;

//?        cur_tlrnc := cur_tlrnc + 1 ;
//?      until isize < GDI_MAXPOINT_COUNT ;

      Result := isize ;
    end ;

    procedure real_draw ;
    begin
      prepareBrush(
        oEditCanvas,
        Viewer.Editor.EditingLinesStyle.BrushColor,
        nil,
        Viewer.Editor.EditingLinesStyle.BrushStyle,
        Point(0, 0)
      ) ;
      T_Brush(T_CanvasEncapsulation(oEditCanvas).SkiaBrush).SelectBrush(
        Self,
        T_CanvasEncapsulation(oEditCanvas),
        nil
      ) ;
      if Viewer.Editor.EditingLinesStyle.PenWidth > 1 then begin
        preparePen(
          oEditCanvas,
          Viewer.Editor.EditingLinesStyle.BrushColor,
          TGIS_PenStyle.Solid,
          nil,
          TGIS_BrushStyle.Solid,
          Point( 0, 0 ),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          Viewer.Editor.EditingLinesStyle.PenWidth
        ) ;
        T_Pen(T_CanvasEncapsulation(oEditCanvas).SkiaPen).SelectPen(
          Self,
          T_CanvasEncapsulation(oEditCanvas),
          nil
        ) ;
        T_CanvasEncapsulation(oEditCanvas).DrawPath( ppath ) ;
        preparePen(
          oEditCanvas,
          Viewer.Editor.EditingLinesStyle.PenColor,
          Viewer.Editor.EditingLinesStyle.PenStyle,
          nil,
          TGIS_BrushStyle.Solid,
          Point( 0, 0 ),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          Viewer.Editor.EditingLinesStyle.PenWidth
        ) ;
        T_Pen(T_CanvasEncapsulation(oEditCanvas).SkiaPen).SelectPen(
          Self,
          T_CanvasEncapsulation(oEditCanvas),
          nil
        ) ;
        T_CanvasEncapsulation(oEditCanvas).DrawPath( ppath ) ;
      end else begin
        preparePen(
          oEditCanvas,
          Viewer.Editor.EditingLinesStyle.PenColor,
          Viewer.Editor.EditingLinesStyle.PenStyle,
          nil,
          TGIS_BrushStyle.Solid,
          Point( 0, 0 ),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          Viewer.Editor.EditingLinesStyle.PenWidth
        ) ;
        T_Pen(T_CanvasEncapsulation(oEditCanvas).SkiaPen).SelectPen(
          Self,
          T_CanvasEncapsulation(oEditCanvas),
          nil
        ) ;
        T_CanvasEncapsulation(oEditCanvas).DrawPath( ppath ) ;
      end ;
    end ;

  begin
    shp := _shp ;
    if shp = nil then exit ;

    for part_no := 0 to shp.GetNumParts - 1 do begin
      // draw_part
      mypath := TSkPathBuilder.Create ;
      try
        if prepare_drawbufpart( iTolerance, part_no ) < 1 then
          continue ;
        ppath := mypath.Detach ;
        real_draw ;
      finally
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.drawEditingPointMarkers(
    const _shp : TGIS_Shape
  ) ;
  const
    eps = 1e-15 ;
  var
    ext      : TGIS_Extent ;
    tps      : Integer ;
    part_no  : Integer ;
    cnt      : Integer ;
    old_pt_x : Integer ;
    old_pt_y : Integer ;
    old_pt_txt_x : Integer ;
    old_pt_txt_y : Integer ;
    point_no : Integer ;
    dx, dy   : Double  ;
    ptg3D    : TGIS_Point3D ;
    pt_x     : Integer ;
    pt_y     : Integer ;
    tw       : Integer ;

    procedure prepare_font ;
    begin
      CanvasFont.Name := Viewer.Editor.EditingPointsStyle.PointsFont.Name ;
      CanvasFont.Size := Viewer.Editor.EditingPointsStyle.PointsFont.Size ;
      CanvasFont.Style := Viewer.Editor.EditingPointsStyle.PointsFont.Style ;
      CanvasFont.Color := Viewer.Editor.EditingPointsStyle.PointsFont.Color ;
    end ;

    procedure prepare_active ;
    begin
      //prepareBrush
      CanvasBrush.Color := Viewer.Editor.EditingPointsStyle.ActivePoints.BrushColor ;
      CanvasBrush.Style := Viewer.Editor.EditingPointsStyle.ActivePoints.BrushStyle ;
      //preparePen
      CanvasPen.Color := Viewer.Editor.EditingPointsStyle.ActivePoints.PenColor ;
      CanvasPen.Style := Viewer.Editor.EditingPointsStyle.ActivePoints.PenStyle ;
      CanvasPen.Width := Viewer.Editor.EditingPointsStyle.ActivePoints.PenWidth ;
    end ;

    procedure prepare_inactive ;
    begin
      //prepareBrush
      CanvasBrush.Color := Viewer.Editor.EditingPointsStyle.InactivePoints.BrushColor ;
      CanvasBrush.Style := Viewer.Editor.EditingPointsStyle.InactivePoints.BrushStyle ;
      //preparePen
      CanvasPen.Color := Viewer.Editor.EditingPointsStyle.InactivePoints.PenColor ;
      CanvasPen.Style := Viewer.Editor.EditingPointsStyle.InactivePoints.PenStyle ;
      CanvasPen.Width := Viewer.Editor.EditingPointsStyle.InactivePoints.PenWidth ;
    end ;

    procedure prepare_z(
      const _param : Boolean
    ) ;
    begin
      if _param
      then CanvasBrush.Color := TGIS_Color.Olive
      else CanvasBrush.Color := TGIS_Color.Red ;
    end ;

    procedure prepare_selected ;
    begin
      //prepareBrush
      CanvasBrush.Color := Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushColor ;
      CanvasBrush.Style := Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushStyle ;
      //preparePen
      CanvasPen.Color := Viewer.Editor.EditingPointsStyle.SelectedPoints.PenColor ;
      CanvasPen.Style := Viewer.Editor.EditingPointsStyle.SelectedPoints.PenStyle ;
      CanvasPen.Width := Viewer.Editor.EditingPointsStyle.SelectedPoints.PenWidth ;
    end ;

    procedure draw_marker(
      const _x1 : Integer ;
      const _y1 : Integer ;
      const _x2 : Integer ;
      const _y2 : Integer
    ) ;
    begin
      CanvasDrawEllipse( _x1, _y1, _x2 - _x1 + 1, _y2 - _y1 + 1 ) ;
    end ;

    function get_text_width(
      const _text : String
    ) : Integer ;
    begin
      Result := CanvasTextExtent(  _text ).X ;
    end ;

    procedure draw_text(
      const _x    : Integer ;
      const _y    : Integer ;
      const _text : String
    ) ;
    var
      brstyle : TGIS_BrushStyle ;
      pt      : TPoint ;
      pnstyle : TGIS_PenStyle ;
      brcolor : TGIS_Color ;
      gap     : Integer ;
    begin
      if Viewer.Editor.EditingPointsStyle.PointsFont.Color =
         Viewer.Editor.EditingPointsStyle.PointsBackground then
        CanvasBrush.Style := TGIS_BrushStyle.Clear ;
      brstyle := CanvasBrush.Style ;
      try
        pt := CanvasTextExtent( _text ) ;
        if brstyle <> TGIS_BrushStyle.Clear then begin
          pnstyle := CanvasPen.Style ;
          try
            // space for better visibility
            gap := TwipsToPixels( 15 ) ;
            CanvasPen.Style := TGIS_PenStyle.Clear ;
            CanvasBrush.Color := Viewer.Editor.EditingPointsStyle.PointsBackground ;
            CanvasDrawRectangle( Rect( _x-gap, _y, _x-gap + pt.X+2*gap, _y + pt.Y ) ) ;
          finally
            CanvasPen.Style := pnstyle ;
          end ;
        end ;
        brcolor := CanvasBrush.Color ;
        try
          CanvasBrush.Color := CanvasFont.Color ;
          CanvasBrush.Style := TGIS_BrushStyle.Solid ;
          CanvasDrawText( Rect( _x, _y, _x + pt.X, _y + pt.Y ), _text ) ;
        finally
          CanvasBrush.Color := brcolor ;
        end ;
      finally
        CanvasBrush.Style := brstyle ;
      end ;
    end ;

  begin
    ext := Viewer.VisibleExtent ;
    tps := TwipsToPixels( GIS_TRACKING_POINT_SIZE div 2 ) ;
    prepare_font ;

    old_pt_x     := -1000 ;
    old_pt_y     := -1000 ;
    old_pt_txt_x := -1000 ;
    old_pt_txt_y := -1000 ;

    for part_no := 0 to _shp.GetNumParts - 1 do begin
      if _shp.GetPartSize( part_no ) <= 0 then continue ;

      if _shp is TGIS_ShapePolygon
      then cnt := _shp.GetPartSize( part_no ) - 2
      else cnt := _shp.GetPartSize( part_no ) - 1 ;

      for point_no := 0 to cnt do begin
        _shp.GetPointEx( part_no, point_no, dx, dy ) ;
        ptg3D := _shp.GetPoint3D( part_no, point_no ) ;

        if ( dx < ext.XMin ) or
           ( dx > ext.XMax ) or
           ( dy < ext.YMin ) or
           ( dy > ext.YMax )
        then continue ;

        // like MapToScreen does
        pt_x := RoundS( (  dx + FExtentX ) * FZoom ) ;
        pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) ;

        if part_no = Viewer.Editor.Part then prepare_active
                                        else prepare_inactive ;

        if ( Abs( old_pt_x - pt_x ) +
             Abs( pt_y - old_pt_y )
           ) >= 2 * tps
        then  begin
          old_pt_x := pt_x ;
          old_pt_y := pt_y ;

          draw_marker( pt_x - tps + 1, pt_y - tps + 1,
                       pt_x + tps - 1, pt_y + tps - 1 ) ;

          if ( Abs( old_pt_txt_x - pt_x ) +
               Abs( pt_y - old_pt_txt_y )
             ) >=  3 * Abs( CanvasFont.Size / 2 )
          then begin
            old_pt_txt_x := pt_x ;
            old_pt_txt_y := pt_y ;

            if Viewer.Editor.ShowPointsNumbers then begin
              draw_text( pt_x + tps, pt_y + tps, IntToStr( point_no ) ) ;
              if Viewer.Editor.ShowPoints3D then begin
                prepare_z( Abs(ptg3D.Z) > eps ) ;
                tw := get_text_width( Format('%.2f',[ptg3D.Z] ) ) - 2 * TwipsToPixels( 15 ) - 1 ;
                //tw := text_extent( Format('%.2f',[ptg3D.Z] ) ).X ;
                draw_text( pt_x - tps*2 - tw, pt_y - tps*2, Format('%.2f',[ptg3D.Z] ) ) ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    if ( Viewer.Editor.PointPos >= 0 ) and
       ( Viewer.Editor.PointPos < _shp.GetPartSize( Viewer.Editor.Part ) ) then begin
      // active point
      prepare_selected ;
      _shp.GetPointEx( Viewer.Editor.Part, Viewer.Editor.PointPos, dx, dy ) ;
      if ( dx >= ext.XMin ) or
         ( dx <= ext.XMax ) or
         ( dy >= ext.YMin ) or
         ( dy <= ext.YMax )
      then begin
        // like MapToScreen does
        pt_x := RoundS( (  dx + FExtentX ) * FZoom ) ;
        pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) ;
        draw_marker( pt_x - tps + 1, pt_y - tps + 1,
                     pt_x + tps - 1, pt_y + tps - 1 ) ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.drawEditingEdgeLengths(
    const _shp : TGIS_Shape
  ) ;
  const
    eps = 1e-15 ;
  var
    ext      : TGIS_Extent ;
    tps      : Integer ;
    part_no  : Integer ;
    point_no : Integer ;
    cnt      : Integer ;
    old_pt_x : Integer ;
    old_pt_y : Integer ;
    dx, dy   : Double  ;
    pt_x     : Integer ;
    pt_y     : Integer ;
    tw       : Integer ;

    old_pt_txt_x : Integer ;
    old_pt_txt_y : Integer ;
    ptg3D        : TGIS_Point3D ;
    old_ptg3D    : TGIS_Point3D ;
    cpt_x        : Integer ;
    cpt_y        : Integer ;
    dlen         : Double ;
    dtxt         : String ;
    lcs          : TGIS_CSCoordinateSystem ;
    lcs_units    : TGIS_CSUnits ;
    dangle       : Double ;
    pcnt         : Integer ;
    valid_cs     : Boolean ;
    follow_edge  : Boolean ;

    procedure prepare_edge_length ;
    begin
      T_CanvasEncapsulation(oEditCanvas).Font.Name  := Viewer.Editor.EditingEdgeLengthsStyle.Font.Name ;
      T_CanvasEncapsulation(oEditCanvas).Font.Size  := Viewer.Editor.EditingEdgeLengthsStyle.Font.Size ;
      T_CanvasEncapsulation(oEditCanvas).Font.Style := Viewer.Editor.EditingEdgeLengthsStyle.Font.Style ;
      T_CanvasEncapsulation(oEditCanvas).Font.Color := Viewer.Editor.EditingEdgeLengthsStyle.Font.Color ;
      prepareFont(
        oEditCanvas,
        T_CanvasEncapsulation(oEditCanvas).Font.Name,
        T_CanvasEncapsulation(oEditCanvas).Font.Size,
        T_CanvasEncapsulation(oEditCanvas).Font.Style,
        T_CanvasEncapsulation(oEditCanvas).Font.Color
      ) ;

      prepareBrush(
        oEditCanvas,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushColor,
        nil,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushStyle,
        Point(0, 0)
      ) ;
      T_Brush(T_CanvasEncapsulation(oEditCanvas).SkiaBrush).SelectBrush(
        Self,
        T_CanvasEncapsulation(oEditCanvas),
        nil
      ) ;

      preparePen(
        oEditCanvas,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.PenColor,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.PenStyle,
        nil,
        TGIS_BrushStyle.Solid,
        Point( 0, 0 ),
        TGIS_LineCap.Round,
        TGIS_LineJoin.Round,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.PenWidth
      ) ;
      T_Pen(T_CanvasEncapsulation(oEditCanvas).SkiaPen).SelectPen(
        Self,
        T_CanvasEncapsulation(oEditCanvas),
        nil
      ) ;
    end ;

    function get_text_width(
      const _text : String
    ) : Integer ;
    var
      cnv  : T_CanvasEncapsulation ;
      size : TPoint ;
    begin
      cnv := T_CanvasEncapsulation(FCanvas) ;
      FCanvas := oEditCanvas ;
      try
        size := CanvasTextExtent( _text ) ;
        Result := size.X ;
      finally
        FCanvas := cnv ;
      end;
    end ;

    procedure draw_text(
      const _x    : Integer ;
      const _y    : Integer ;
      const _text : String
    ) ;
    var
      pt  : TPoint ;
      cnv : T_CanvasEncapsulation ;
      gap : Integer ;
    begin
      if Viewer.Editor.EditingEdgeLengthsStyle.Font.Color <>
         Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushColor then begin
        cnv := T_CanvasEncapsulation(FCanvas) ;
        try
          FCanvas := oEditCanvas ;
          pt := CanvasTextExtent( _text ) ;
        finally
          FCanvas := cnv ;
        end;
        // some space for better visibility
        gap := TwipsToPixels( 15 ) ;
        T_CanvasEncapsulation(oEditCanvas).FillRect(
          RectF( _x - gap, _y, _x + pt.X + gap, _y + pt.Y )
        ) ;
      end ;
      T_CanvasEncapsulation(oEditCanvas).DrawText(_text, _x, _y ) ;
    end ;

    procedure prepare_cs ;
    var
      uepsg : Integer ;
    begin
      uepsg := Viewer.Editor.EditingEdgeLengthsStyle.UnitsEPSG ;
      if uepsg = 0 then begin
        if GisIsMetricSystem then
          lcs_units  := CSUnitsList.ByEPSG( 904201 )
        else
          lcs_units  := CSUnitsList.ByEPSG( 904202 ) ;
      end
      else
        lcs_units := CSUnitsList.ByEPSG( uepsg ) ;

      lcs := _shp.Layer.CS ;
      valid_cs := lcs.EPSG <> 0 ;
    end ;

    function get_length( const _ptg1 : TGIS_Point ; const _ptg2 : TGIS_Point ) : Double ;
    begin
      if valid_cs then
        Result := lcs.Distance( _ptg1, _ptg2 )
      else
        Result := GisPoint2Point( _ptg1, _ptg2 ) ;
    end ;

    procedure set_transformation(
      const _angle    : Double  ;
      const _origin_x : Integer ;
      const _origin_y : Integer
    ) ;
    var
      cnv : T_CanvasEncapsulation ;
    begin
      cnv := T_CanvasEncapsulation(FCanvas) ;
      try
        FCanvas := oEditCanvas ;
        CanvasSetTransformation( _angle, _origin_x, _origin_y ) ;
      finally
        FCanvas := cnv ;
      end ;
    end ;

    procedure clear_transformation ;
    var
      cnv : T_CanvasEncapsulation ;
    begin
      cnv := T_CanvasEncapsulation(FCanvas) ;
      try
        FCanvas := oEditCanvas ;
        CanvasClearTransformation ;
      finally
        FCanvas := cnv ;
      end ;
    end ;

  begin
    if not Viewer.Editor.ShowEdgesLengths then exit ;

    ext := Viewer.VisibleExtent ;
    tps := TwipsToPixels( GIS_TRACKING_POINT_SIZE div 2 ) ;
    follow_edge := Viewer.Editor.EditingEdgeLengthsStyle.FollowEdgeAngle ;

    prepare_cs ;

    old_pt_x     := -1000 ;
    old_pt_y     := -1000 ;
    old_pt_txt_x := -1000 ;
    old_pt_txt_y := -1000 ;

    for part_no := 0 to _shp.GetNumParts - 1 do begin
      if _shp.GetPartSize( part_no ) <= 0 then continue ;

      cnt := _shp.GetPartSize( part_no ) - 1 ;

      pcnt := 0 ;
      for point_no := 0 to cnt do begin
        _shp.GetPointEx( part_no, point_no, dx, dy ) ;

        // like MapToScreen does
        pt_x := RoundS( (  dx + FExtentX ) * FZoom ) ;
        pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) ;

        if ( Abs( old_pt_x - pt_x ) +
             Abs( pt_y - old_pt_y )
           ) >= 2 * tps
        then  begin
          old_pt_x := pt_x ;
          old_pt_y := pt_y ;

          if ( Abs( old_pt_txt_x - pt_x ) +
               Abs( pt_y - old_pt_txt_y )
             ) >=  2 * Abs( T_CanvasEncapsulation(oEditCanvas).Font.Size )
          then begin
            cpt_x := (old_pt_txt_x + pt_x) div 2 ;
            cpt_y := (old_pt_txt_y + pt_y) div 2 ;

            old_pt_txt_x := pt_x ;
            old_pt_txt_y := pt_y ;

            ptg3D := _shp.GetPoint3D( part_no, point_no ) ;

            if assigned( lcs ) then begin
              if pcnt > 0 then begin
                dlen := get_length( GisPoint2DFrom3D( old_ptg3D ),
                                    GisPoint2DFrom3D( ptg3D )
                                  ) ;
                dtxt := lcs_units.AsLinear( dlen, True ) ;
                prepare_edge_length ;
                tw := get_text_width( dtxt ) div 2 ;

                if follow_edge then begin
                  if cpt_x > pt_x then
                    dangle := ArcTan2(cpt_y-pt_y, cpt_x-pt_x)
                  else
                    dangle := ArcTan2(pt_y-cpt_y, pt_x-cpt_x) ;
                  set_transformation( dangle, cpt_x, cpt_y ) ;
                  draw_text( - tw, 0, dtxt ) ;
                  clear_transformation ;
                end
                else
                  draw_text( cpt_x - tw, cpt_y, dtxt ) ;
              end ;
            end ;

            old_ptg3D := _TGIS_Point3D(ptg3D) ;
            inc( pcnt ) ;
          end ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.drawEditingPoints(
    const _context : TObject ;
    const _shp     : TGIS_Shape
  ) ;
  var
    cnv : T_CanvasEncapsulation ;

    procedure prepare_context ;
    begin
      if _context is TGIS_PaintSkia then begin
        oEditCanvas := T_CanvasEncapsulation.Create( TGIS_PaintSkia(_context).Canvas ) ;
        FCanvas := oEditCanvas ;
      end ;
      if _context is TGIS_Bitmap then begin
        oEditCanvas := T_CanvasEncapsulation.Create( TGIS_Bitmap(_context), nil ) ;
        FCanvas := oEditCanvas ;
      end ;
    end ;

    procedure release_context ;
    begin
      if _context is TGIS_Bitmap then begin
        T_CanvasEncapsulation(oEditCanvas).Flush( False ) ;
        TGIS_Bitmap(_context).SetData(
          BitmapFactorySkia,
          TSkImage(T_CanvasEncapsulation(oEditCanvas).Image)
        ) ;
      end ;
      FreeObject( oEditCanvas ) ;
    end ;

  begin
    if not ( _context is TGIS_PaintSkia )
       and
       not ( _context is TGIS_Bitmap )
    then
      exit ;

    if _shp.IsEmpty then
      exit ;

    cnv := T_CanvasEncapsulation(FCanvas) ;
    prepare_context ;
    inEdit := True ;
    try
      if ( TGIS_Shape( _shp ).ShapeType = TGIS_ShapeType.Arc ) or
         ( TGIS_Shape( _shp ).ShapeType = TGIS_ShapeType.Polygon ) then
        _shp.Draw ;
      drawEditingPointMarkers( _shp ) ;
      drawEditingEdgeLengths( _shp ) ;
    finally
      inEdit := False ;
      FCanvas := cnv ;
      release_context ;
    end;
  end ;

 function TGIS_RendererVclSkia.PrepareBitmapCache(
    const _bmp    : TGIS_Pixels ;
    const _extent : TGIS_Extent ;
    const _size   : TPoint      ;
    const _serial : Integer     ;
    const _format : TGIS_BitmapFormat ;
    const _order  : TGIS_BitmapLinesOrder
  ) : TGIS_RendererAbstractCache ;
  var
    bmp : TGIS_Bitmap ;
  begin
    Result := TGIS_RendererVclSkiaCache.Create( _extent, _size, _serial );

    bmp := TGIS_Bitmap.Create( _size.X, _size.Y ) ;

    TBitmap( bmp.GetData( NativeBitmapFactory ) ).AlphaFormat
      := TAlphaFormat.afPremultiplied ;

    renderBitmapInternal(
      nil,
      TBitmap( bmp.GetData( NativeBitmapFactory ) ),
      _bmp,
      _size,
      Rect( 0, 0, _size.X, _size.Y ),
      _format,
      _order,
      True
    ) ;

    TGIS_RendererVclSkiaCache( Result ).oBitmap := bmp ;
  end;

  procedure TGIS_RendererVclSkia.RenderBitmapCache(
    const _handle : TObject ;
    const _cache  : TGIS_RendererAbstractCache ;
    const _dst    : TRect
  ) ;
  var
    cnv : T_CanvasEncapsulation ;
  begin
    if assigned( oTransparentCanvas ) then
      cnv := T_CanvasEncapsulation(oTransparentCanvas)
    else
      cnv := T_CanvasEncapsulation(FCanvas) ;

    cnv.DrawImage(
      T_SkiaUtils.GisBitmapToImage( TGIS_RendererVclSkiaCache( _cache ).oBitmap ),
      RectF( 0, 0, _dst.Width, _dst.Height ),
      RectF( _dst.Left, _dst.Top, _dst.Right, _dst.Bottom )
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.RenderEditor(
    const _context : TObject
  ) ;
  begin
    // _context is TGIS_PaintSkia or TGIS_Bitmap

    if assigned( Viewer ) and Viewer.Editor.InEdit then begin
      if assigned( Viewer.Editor.CurrentShape ) then begin
        // ensure proper renderer context
        TGIS_Shape( Viewer.Editor.CurrentShape ).Layer.Renderer := self ;

        if not ( _context is TGIS_PaintSkia )
           and
           not ( _context is TGIS_Bitmap ) then exit ;

        drawEditingPoints( _context,
                           TGIS_Shape( Viewer.Editor.CurrentShape ) ) ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.PaintExtra(
    const _sender  : TObject ;
    const _context : TObject ;
    const _event   : TGIS_RendererEvent
  ) ;
  var
    ocnv : T_CanvasEncapsulation ;
    fcnv : T_CanvasEncapsulation ;
    osel : T_CanvasEncapsulation ;
    uogl : Boolean ;
    (*rd_parent : IGIS_ViewerParent ;
    rd_viewer : IGIS_Viewer ;
    rd_contxt : TGIS_RendererContext ;
    rd_shift  : TPoint ;
    rd_width  : Integer ;
    rd_height : Integer ;
    rd_ppi    : Integer ;
    rd_fscale : Integer ;*)
    bmp : TBitmap ;
    w   : Integer ;
    h   : Integer ;

    (*procedure save_context ;
    begin
      rd_parent := Parent ;
      rd_viewer := Viewer ;
      rd_contxt := Context ;
      rd_shift  := Shift ;
      rd_width  := Width ;
      rd_height := Height ;
      rd_ppi    := PPI ;
      rd_fscale := FontScale ;
    end ;

    procedure restore_context ;
    begin
      inherited CreateContext(
                  rd_parent, rd_viewer,
                  rd_contxt, rd_shift,
                  rd_width, rd_height,
                  rd_ppi, rd_fscale ) ;
    end ;*)

    procedure prepare_context ;
    (*var
      sc : Single ;*)
    begin
      bmp := nil ;
      if _context is TCanvas then begin
        Parent := TComponent(_sender) as IGIS_ViewerParent ;
        (*save_context ;

        sc := Parent.ControlCanvasScale ;
        if ( SKCanvas( _context ).TotalMatrix.ScaleX = 1.0 ) and
           ( SKCanvas( _context ).TotalMatrix.ScaleY = 1.0 ) then
          sc := 1 ;

        inherited CreateContext( Parent, nil, nil, Point(0,0),
                                 RoundS( Parent.ControlCanvasWidth / sc ),
                                 RoundS( Parent.ControlCanvasHeight / sc ),
                                 RoundS( Parent.ControlPPI / sc ),
                                 100 ) ;

        FCanvas := T_CanvasEncapsulation.Create( SKCanvas( _context ) ) ;*)
        w := Parent.ControlCanvasWidth ;
        h := Parent.ControlCanvasHeight ;
        {$IFDEF LEVEL_RX11_VCL}
          bmp := TBitmap.Create( w, h ) ;
        {$ELSE}
          bmp := TBitmap.Create;
          bmp.Width  := w ;
          bmp.Height := h ;
        {$ENDIF}
        bmp.PixelFormat := pf32bit ;
        bmp.Canvas.CopyRect( Rect( 0, 0, w, h ), TCanvas( _context ), Rect( 0, 0, w, h ) ) ;
        assert( False ) ;
        (*FCanvas := T_CanvasEncapsulation.Create( bmp, False ) ;*)
      end
      else
      if _context is TBitmap then begin
        assert( False ) ;
        (*FCanvas := T_CanvasEncapsulation.Create( TBitmap( _context ), False ) ;*)
      end
      else if _context is TGIS_Bitmap then begin
        FCanvas := T_CanvasEncapsulation.Create( TGIS_Bitmap( _context ), nil )
      end
      else if _context is TGIS_PaintSkia then begin
        FCanvas := T_CanvasEncapsulation.Create( TGIS_PaintSkia( _context ).Canvas ) ;
      end ;
    end ;

    procedure release_context ;
    begin
      if _context is TCanvas then begin
        T_SkiaUtils.SurfaceToBitmap( T_CanvasEncapsulation(FCanvas).Surface, TBitmap( bmp ) ) ;
        FreeObject( FCanvas ) ;
        (*restore_context ;*)
        TCanvas( _context ).CopyRect( Rect( 0, 0, w, h ), bmp.Canvas, Rect( 0, 0, w, h ) ) ;
        FreeObject( bmp ) ;
      end
      else
      if _context is TGIS_Bitmap then begin
        T_CanvasEncapsulation(FCanvas).Flush( useOpenGL ) ;
        TGIS_Bitmap(_context).SetData(
          BitmapFactorySkia,
          TSkImage( T_CanvasEncapsulation(FCanvas).Image )
        ) ;
        FreeObject( FCanvas ) ;
      end ;
      if _context is TGIS_PaintSkia then begin
        FreeObject( FCanvas ) ;
      end ;
    end ;

  begin
    if assigned( _event ) then begin
      ocnv := T_CanvasEncapsulation(oCanvas) ;
      fcnv := T_CanvasEncapsulation(FCanvas) ;
      osel := T_CanvasEncapsulation(oSelectionCanvas) ;
      uogl := useOpenGL ;
      try
        useOpenGL := False ;
        FCanvas := nil ;
        prepare_context ;
        if not assigned( FCanvas ) then exit ;
        try
          pextra  := True ;
          oCanvas := FCanvas ;
          oSelectionCanvas := nil ;
         _event( _sender, Self, TGIS_DrawMode.All ) ;
        finally
          pextra := False ;
          release_context ;
        end ;
      finally
        useOpenGL := uogl ;
        oCanvas   := ocnv ;
        FCanvas   := fcnv ;
        oSelectionCanvas := osel ;
      end;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.PaintExtra(
    const _sender  : TObject ;
    const _context : TObject ;
    const _event   : TGIS_PaintEvent
  ) ;
(*  var
    ocnv : T_CanvasEncapsulation ;
    fcnv : T_CanvasEncapsulation ;
    osel : T_CanvasEncapsulation ;
    (*rd_parent : IGIS_ViewerParent ;
    rd_viewer : IGIS_Viewer ;
    rd_contxt : TGIS_RendererContext ;
    rd_shift  : TPoint ;
    rd_width  : Integer ;
    rd_height : Integer ;
    rd_ppi    : Integer ;
    rd_fscale : Integer ;*)
(*    bmp : TBitmap ;
    w   : Integer ;
    h   : Integer ;

    (*procedure save_context ;
    begin
      rd_parent := Parent ;
      rd_viewer := Viewer ;
      rd_contxt := Context ;
      rd_shift  := Shift ;
      rd_width  := Width ;
      rd_height := Height ;
      rd_ppi    := PPI ;
      rd_fscale := FontScale ;
    end ;

    procedure restore_context ;
    begin
      inherited CreateContext(
                  rd_parent, rd_viewer,
                  rd_contxt, rd_shift,
                  rd_width, rd_height,
                  rd_ppi, rd_fscale ) ;
    end ;*)

(*    procedure prepare_context ;
    (*var
      sc : Single ;*)
(*    begin
      FCanvas := nil ;
      bmp := nil ;
      if _context is TCanvas then begin
        Parent := TComponent(_sender) as IGIS_ViewerParent ;
        (*save_context ;

        sc := Parent.ControlCanvasScale ;
        if ( SKCanvas( _context ).TotalMatrix.ScaleX = 1.0 ) and
           ( SKCanvas( _context ).TotalMatrix.ScaleY = 1.0 ) then
          sc := 1 ;

        inherited CreateContext( Parent, nil, nil, Point(0,0),
                                 RoundS( Parent.ControlCanvasWidth / sc ),
                                 RoundS( Parent.ControlCanvasHeight / sc ),
                                 RoundS( Parent.ControlPPI / sc ),
                                 100 ) ;

        FCanvas := T_CanvasEncapsulation.Create( SKCanvas( _context ) ) ;*)
(*        w := Parent.ControlCanvasWidth ;
        h := Parent.ControlCanvasHeight ;
        {$IFDEF LEVEL_RX11_VCL}
          bmp := TBitmap.Create( w, h ) ;
        {$ELSE}
          bmp := TBitmap.Create;
          bmp.Width  := w ;
          bmp.Height := h ;
        {$ENDIF}
        bmp.PixelFormat := pf32bit ;
        bmp.Canvas.CopyRect( Rect( 0, 0, w, h ), TCanvas( _context ), Rect( 0, 0, w, h ) ) ;
        assert( False ) ;
        (*FCanvas := T_CanvasEncapsulation.Create( bmp, False ) ;*)
(*      end
      else
      if _context is TBitmap then begin
        assert( False ) ;
        (*FCanvas := T_CanvasEncapsulation.Create( TBitmap( _context ), False ) ;*)
(*      end
      else if _context is TGIS_Bitmap then begin
        if TGIS_Bitmap.BitmapType = TGIS_BitmapType.VCL then
          FCanvas := T_CanvasEncapsulation.Create( TGIS_Bitmap( _context ), nil )
        else
          Assert( False ) ;
      end
      else if _context is TGIS_SkiaPaint then begin
        FCanvas := T_CanvasEncapsulation.Create( TGIS_SkiaPaint( _context ).Canvas ) ;
      end
      else
        assert( False ) ;
      oCanvas := FCanvas ;
    end ;

    procedure release_context ;
    begin
      if _context is TCanvas then begin
        T_SkiaUtils.SurfaceToBitmap( FCanvas.Surface, TBitmap( bmp ) ) ;
        FreeObject( FCanvas ) ;
        (*restore_context ;*)
(*        TCanvas( _context ).CopyRect( Rect( 0, 0, w, h ), bmp.Canvas, Rect( 0, 0, w, h ) ) ;
        FreeObject( bmp ) ;
      end
      else
      if _context is TGIS_Bitmap then begin
        FCanvas.Flush( useOpenGL ) ;
        T_SkiaUtils.ImageToBitmap( FCanvas.Image, TGIS_Bitmap( _context ) ) ;
        FreeObject( FCanvas ) ;
      end ;
      if _context is TGIS_SkiaPaint then begin
        FreeObject( FCanvas ) ;
      end ;
    end ;

  begin
    if assigned( _event ) then begin
      ocnv := oCanvas ;
      fcnv := FCanvas ;
      osel := oSelectionCanvas ;
      try
        prepare_context ;
        if not assigned( FCanvas ) then exit ;
        try
          pextra := True ;
          oCanvas := FCanvas ;
          oSelectionCanvas := nil ;
         _event( _sender, Self, TGIS_DrawMode.All ) ;
        finally
          pextra := False ;
          release_context ;
        end ;
      finally
        oCanvas := ocnv ;
        FCanvas := fcnv ;
        oSelectionCanvas := osel ;
      end;
    end ;
  end ;*)
  var
    bmp  : TBitmap ;
    tbmp : TGIS_Bitmap ;
    cnv  : TCanvas ;

    procedure prepare_context ;
    var
      p : IGIS_ViewerParent ;
    begin
      cnv := nil ;
      if _context is TGIS_PaintSkia then begin
        if not assigned( TGIS_PaintSkia( _context ).Canvas ) then exit ;
        try
          p := TComponent(_sender) as IGIS_ViewerParent ;
          tbmp := TGIS_Bitmap.Create( p.ControlCanvasWidth, p.ControlCanvasHeight ) ;
          cnv := TBitmap(tbmp.NativeBitmap).Canvas ;
        except
          cnv := nil ;
        end ;
      end ;
      if _context is TGIS_Bitmap then begin
        try
          bmp := TBitmap( TGIS_Bitmap(_context).GetData( NativeBitmapFactory ) ) ;
          cnv := bmp.Canvas ;
        except
          cnv := nil ;
        end ;
      end ;
    end ;

    procedure release_context ;
    begin
      if _context is TGIS_PaintSkia then begin
        TGIS_PaintSkia( _context ).Canvas.DrawImage(
          ISkImage( TSkImage( tbmp.GetData( BitmapFactory ) ) ), 0, 0
        ) ;
        FreeObject( tbmp ) ;
      end ;
      if _context is TGIS_Bitmap then begin
        VCLMakeCompatibleBitmap( bmp ) ;
        TGIS_Bitmap(_context).GetData( BitmapFactorySkia ) ;
      end ;
    end ;

  begin
    if assigned( _event ) then begin
      prepare_context ;
      try
        if assigned( cnv ) then
          _event( _sender, cnv ) ;
      finally
        release_context ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.Update ;
  begin
  end ;

  procedure TGIS_RendererVclSkia.Flush ;
  var
    bmp : TGIS_Bitmap ;
  begin
    T_CanvasEncapsulation(oCanvas).Flush( useOpenGL ) ;
    if Context.BaseMapOnDemand and not assigned( Context.BaseMap ) then begin
      bmp := TGIS_Bitmap.Create ;
      Context.AssignBaseMap( bmp, True ) ;
    end
    else
      bmp := TGIS_Bitmap( Context.BaseMap ) ;
    bmp.SetData(
      BitmapFactorySkia,
      TSkImage( T_CanvasEncapsulation(oCanvas).Image )
    ) ;
  end ;

  function TGIS_RendererVclSkia.FriendlyName
    : String ;
  begin
    Result := 'Skia' ;
  end ;

  function TGIS_RendererVclSkia.ViewerCreateWndPaint(
    const _width  : Integer ;
    const _height : Integer ;
    const _canvas : TObject
  ) : TObject ;
  begin
    // _canvas is TSkCanvas (OpenGL)
    // Result is TGIS_PaintSkia

    Result := TGIS_PaintSkia.Create ;
    if assigned( _canvas ) then begin
      TGIS_PaintSkia( Result ).Surface := nil ;
      TGIS_PaintSkia( Result ).Canvas := TSkCanvas( _canvas ) ;
    end
    else begin
      TGIS_PaintSkia( Result ).Surface := TSkSurface.MakeRaster( _width, _height ) ;
      TGIS_PaintSkia( Result ).Canvas := TGIS_PaintSkia( Result ).Surface.Canvas ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerFreeWndPaint(
    var _paint : TObject
  ) ;
  begin
    // _paint is TGIS_PaintSkia

    if not ( _paint is TGIS_PaintSkia ) then exit ;

    FreeObject( _paint ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerFlushToWndPaint(
    const _localCanvas : TObject ;
    const _canvas      : TObject
  ) ;
  var
    bmp : TBitmap ;
  begin
    // _localCanvas is TGIS_PaintSkia
    // _canvas is TCanvas

    if not ( _localCanvas is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _localCanvas ).Surface ) then exit ;
    if not assigned( _canvas ) then exit ;

    TGIS_PaintSkia( _localCanvas ).Surface.Flush ;
    bmp := TBitmap.CreateFromSkImage(
             TGIS_PaintSkia( _localCanvas ).Surface.MakeImageSnapshot
           ) ;
    try
      TCanvas(_canvas).Draw( 0, 0, bmp ) ;
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerDrawBackground(
    const _canvas : TObject ;
    const _width  : Integer ;
    const _height : Integer ;
    const _color  : TGIS_Color
  ) ;
  begin
    // _canvas is TGIS_PaintSkia

    if not ( _canvas is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _canvas ).Canvas ) then exit ;

    TGIS_PaintSkia( _canvas ).Canvas.Clear( _color.ToARGB ) ;
  end ;

  function TGIS_RendererVclSkia.ViewerCreateTemporaryPaint(
    const _width   : Integer ;
    const _height  : Integer ;
    const _context : TObject
  ) : TObject ;
  begin
    // _context is not used
    // Result is TGIS_PaintSkia

    Result := TGIS_PaintSkia.Create ;
    TGIS_PaintSkia( Result ).Surface := TSkSurface.MakeRaster( _width, _height ) ;
    TGIS_PaintSkia( Result ).Canvas := TGIS_PaintSkia( Result ).Surface.Canvas ;
    TGIS_PaintSkia( Result ).Canvas.Clear( TAlphaColors.Null ) ;
  end ;

  function TGIS_RendererVclSkia.ViewerCreateTemporaryPaint(
    const _context : TGIS_Bitmap
  ) : TObject ;
  var
    bitmap : TGIS_Bitmap ;
  begin
    // Result is TGIS_PaintSkia

    Result :=  nil ;
    if not assigned( _context ) then exit ;

    bitmap := TGIS_Bitmap( _context ) ;
    Result := TGIS_PaintSkia.Create ;
    TGIS_PaintSkia( Result ).Surface := TSkSurface.MakeRaster( bitmap.Width, bitmap.Height ) ;
    T_SkiaUtils.ImageToSurface(
      TSkImage( bitmap.GetData( BitmapFactory ) ),
      TGIS_PaintSkia( Result ).Surface
    ) ;
    TGIS_PaintSkia( Result ).Canvas := TGIS_PaintSkia( Result ).Surface.Canvas ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerFreeTemporaryPaint(
    var _paint : TObject
  ) ;
  begin
    // _paint is TGIS_PaintSkia

    if not ( _paint is TGIS_PaintSkia ) then exit ;

    TGIS_PaintSkia(_paint).Canvas  :=  nil ;
    TGIS_PaintSkia(_paint).Surface :=  nil ;
    FreeObject( _paint ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerFreeTemporaryPaint(
    var   _paint  : TObject ;
    const _bitmap : TGIS_Bitmap
  ) ;
  begin
    // _paint is TGIS_PaintSkia

    if not ( _paint is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _paint ).Surface ) then exit ;
    if not assigned( _bitmap ) then exit ;

    _bitmap.SetData(
      BitmapFactory,
      TSkImage( TGIS_PaintSkia( _paint ).Surface.MakeImageSnapshot.MakeRasterImage )
    ) ;
    FreeObject( _paint ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerClearTemporaryPaint(
    const _paint : TObject
  ) ;
  begin
    // _paint is TGIS_PaintSkia

    if not ( _paint is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia(_paint).Canvas ) then exit ;

    TGIS_PaintSkia(_paint).Canvas.Clear( TGis_Color.None.ToARGB ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerFlushTemporaryPaint(
    const _canvas : TObject ;
    const _width  : Integer ;
    const _height : Integer ;
    const _paint  : TObject ;
    const _rect   : TRect
  ) ;
  begin
    // _canvas is TGIS_PaintSkia
    // _paint is TGIS_PaintSkia

    if not ( _canvas is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _canvas ).Canvas ) then exit ;
    if not ( _paint  is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _paint ).Surface ) then exit ;

    TGIS_PaintSkia( _canvas ).Canvas.DrawImageRect(
      TGIS_PaintSkia(_paint).Surface.MakeImageSnapshot,
      RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom )
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerFlushTemporaryPaint(
    const _canvas    : TObject ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _paint     : TObject ;
    const _fullCache : TGIS_Bitmap ;
    const _rect      : TRect
  ) ;
  begin
    // _canvas is TGIS_PaintSkia
    // _paint is not used

    if not ( _canvas is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _canvas ).Canvas ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( _fullCache ) then exit ;

    TGIS_PaintSkia( _canvas ).Canvas.DrawImageRect(
      TSkImage( _fullCache.GetData( BitmapFactory ) ),
      RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom )
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerStretchBitmapFast(
    const _src  : TObject ;
    const _dst  : TObject ;
    const _rect : TRect
  ) ;
  begin
    // _src is TGIS_Bitmap
    // _dst is TGIS_PaintSkia

    if not ( _src is TGIS_Bitmap ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap( _src ) ) then exit ;
    if not ( _dst is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _dst ).Canvas ) then exit ;

    TGIS_PaintSkia( _dst ).Canvas.DrawImageRect(
      TSkImage( TGIS_Bitmap( _src ).GetData( BitmapFactorySkia )),
      RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom )
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerStretchBitmapFast(
    const _src          : TObject ;
    const _dst          : TObject ;
    const _rect         : TRect   ;
    const _transparency : Integer
  ) ;
  var
    cl    : TGIS_Color ;
    paint : ISkPaint ;
  begin
    // _src is TGIS_Bitmap
    // _dst is TGIS_PaintSkia

    if not ( _src is TGIS_Bitmap ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap( _src ) ) then exit ;
    if not ( _dst is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _dst ).Canvas ) then exit ;

    paint := TSkPaint.Create ;
    cl := TGIS_Color.FromARGB( RoundS( _transparency * 255 / 100 ),
                               255, 255, 255 ) ;
    paint.ColorFilter := TSkColorFilter.MakeBlend( cl.ToARGB,
                                                   TSkBlendMode.DestIn ) ;
    TGIS_PaintSkia( _dst ).Canvas.DrawImageRect(
      TSkImage( TGIS_Bitmap( _src ).GetData( BitmapFactorySkia ) ),
      RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
      paint
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerBlendBitmaps(
    const _src          : TObject ;
    const _dst          : TObject ;
    const _transparency : Integer ;
    const _merge_alpha  : Boolean
  ) ;
  var
    image : ISkImage ;
    paint : ISkPaint ;
    cl    : TGIS_Color ;
  begin
    // _src is TGIS_Bitmap
    // _dst is TGIS_PaintSkia

    if not ( _src is TGIS_Bitmap ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap( _src ) ) then exit ;
    if not ( _dst is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _dst ).Canvas ) then exit ;

    image := TSkImage( TGIS_Bitmap( _src ).GetData( BitmapFactory ) ) ;
    if _dst is TGIS_PaintSkia then begin
      if _transparency = 100 then
        TGIS_PaintSkia( _dst ).Canvas.DrawImage( image, 0, 0 )
      else begin
        paint := TSkPaint.Create ;
        try
          cl := TGIS_Color.FromARGB( RoundS( _transparency * 255 / 100 ),
                                     255, 255, 255 ) ;
          paint.ColorFilter := TSkColorFilter.MakeBlend( cl.ToARGB,
                                                         TSkBlendMode.DestIn ) ;
          TGIS_PaintSkia( _dst ).Canvas.DrawImage( image, 0, 0, paint ) ;
        finally
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerBlendLabelBitmaps(
    const _src         : TObject ;
    const _dst         : TObject ;
    const _merge_alpha : Boolean
  ) ;
  begin
    ViewerBlendBitmaps( _src, _dst, 100, _merge_alpha ) ;
  end ;

  function TGIS_RendererVclSkia.ViewerCreateFullCache(
    const _width  : Integer ;
    const _height : Integer ;
    const _paint  : TObject ;
    const _addObj : TObject
  ) : TGIS_Bitmap ;
  var
    paint : TGIS_PaintSkia ;
    image : ISkImage ;
  begin
    // _paint is TGIS_PaintSkia created by CreateTemporaryPaint; always has assigned Surface
    // _addObj is OpenGL connection

    Result := nil ;
    if not ( _paint is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _paint ).Surface ) then exit ;

    paint  := TGIS_PaintSkia( _paint ) ;
    if assigned( _addObj ) then
      T_OpenGLSkiaConnection( _addObj ).RendererFlush( paint.Surface, image )
    else
      image := paint.Surface.MakeImageSnapshot.MakeRasterImage ;
    Result := TGIS_Bitmap.Create( _width, _height ) ;
    Result.SetData( BitmapFactorySkia, TSkImage( image ) ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerDrawCache(
    const _cache : TObject ;
    const _paint : TObject ;
    const _rect  : TRect
  ) ;
  begin
    // _cache is TGIS_Bitmap
    // _paint is TGIS_PaintSkia

    if not ( _cache is TGIS_Bitmap ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap(_cache) ) then exit ;
    if not ( _paint is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _paint ).Canvas ) then exit ;

    TGIS_PaintSkia( _paint ).Canvas.DrawImageRect(
      T_SkiaUtils.GisBitmapToImage( TGIS_Bitmap( _cache ) ),
      RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom )
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerDrawCache(
    const _cache        : TObject ;
    const _paint        : TObject ;
    const _rect         : TRect   ;
    const _transparency : Integer
  ) ;
  var
    paint : ISkPaint ;
    cl    : TGIS_Color ;
  begin
    // _cache is TGIS_Bitmap
    // _paint is TGIS_PaintSkia

    if not ( _cache is TGIS_Bitmap ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap( _cache ) )  then exit ;
    if not ( _paint is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _paint ).Canvas ) then exit ;

    if _transparency = 100 then
      TGIS_PaintSkia( _paint ).Canvas.DrawImageRect(
        T_SkiaUtils.GisBitmapToImage( TGIS_Bitmap( _cache ) ),
        RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom )
      )
    else begin
      paint := TSkPaint.Create ;
      try
        cl := TGIS_Color.FromARGB( RoundS( _transparency * 255 / 100 ),
                                   255, 255, 255 ) ;
        paint.ColorFilter := TSkColorFilter.MakeBlend( cl.ToARGB,
                                                       TSkBlendMode.DestIn ) ;
        TGIS_PaintSkia( _paint ).Canvas.DrawImageRect(
          T_SkiaUtils.GisBitmapToImage( TGIS_Bitmap( _cache ) ),
          RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
          paint
        ) ;
      finally
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerDrawProgressBitmap(
    const _bitmap : TObject ;
    const _paint  : TObject ;
    const _rect   : TRect
  ) ;
  begin
    ViewerBlendBitmaps( _bitmap, _paint, 100, True ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerDrawZoomingRect(
    const _canvas : TObject ;
    const _x      : Integer ;
    const _y      : Integer ;
    const _width  : Integer ;
    const _height : Integer ;
    const _color  : TGIS_Color
  ) ;
  var
    pnt : ISkPaint ;
  begin
    if not ( _canvas is TGIS_PaintSkia ) then exit ;
    pnt := TSkPaint.Create ;
    pnt.SetStyle( TSKPaintStyle.Stroke );
    pnt.StrokeWidth := 1 ;

    pnt.SetColor( _color.ARGB ) ;
    TGIS_PaintSkia(_canvas).Canvas.DrawRect(
      RectF( _x, _y, _x + _width, _y + _height ),
      pnt
    ) ;

    pnt.SetColor( TGIS_Color.Black.ARGB ) ;
    pnt.SetPathEffect( TSkPathEffect.MakeDash( [2*2,1*2], 0 ) ) ;
    TGIS_PaintSkia(_canvas).Canvas.DrawRect(
      RectF( _x, _y, _x + _width, _y + _height ),
      pnt
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.ViewerDrawDraggingTrack(
    const _canvas : TObject ;
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer ;
    const _color  : TGIS_Color
  ) ;
  var
    pnt : ISkPaint ;
    pt1 : TPointF ;
    pt2 : TPointF ;
  begin
    if not ( _canvas is TGIS_PaintSkia ) then exit ;
    pnt := TSkPaint.Create ;
    pnt.SetStyle( TSKPaintStyle.Stroke );
    pnt.StrokeWidth := 1 ;

    pt1 := PointF( _x1, _y1 ) ;
    pt2 := PointF( _x2, _y2 ) ;

    pnt.SetColor( _color.ARGB ) ;
    TGIS_PaintSkia(_canvas).Canvas.DrawLine( pt1, pt2, pnt ) ;

    pnt.SetColor( TGIS_Color.Black.ARGB ) ;
    pnt.SetPathEffect( TSkPathEffect.MakeDash( [2*2,1*2], 0 ) ) ;
    TGIS_PaintSkia(_canvas).Canvas.DrawLine( pt1, pt2, pnt ) ;
  end ;

  procedure TGIS_RendererVclSkia.ControlDrawTransparent(
    const _context : TObject ;
    const _bitmap  : TObject ;
    const _x       : Integer ;
    const _y       : Integer
  ) ;
  begin
    // _context is TGIS_PaintSkia
    // _bitmap i TGIS_Bitmap

    if not ( _context is TGIS_PaintSkia ) then exit ;
    if not assigned( TGIS_PaintSkia( _context ).Canvas ) then exit ;
    if not ( _bitmap is TGIS_Bitmap ) then exit ;
    if TGIS_Bitmap.IsNilOrEmpty( TGIS_Bitmap( _bitmap ) ) then exit ;

    TGIS_PaintSkia( _context ).Canvas.DrawImage(
      TSkImage( TGIS_Bitmap(_bitmap).GetData( BitmapFactorySkia ) ),
      _x, _y
    ) ;
  end ;

  function TGIS_RendererVclSkia.CanvasNative
    : TObject ;
  begin
    assert( assigned( FCanvas ) ) ;

    Result := TSkCanvas( T_CanvasEncapsulation(FCanvas).Canvas ) ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasFontMetrics(
    var   _break_char : Char    ;
    var   _height     : Integer ;
    var   _ascent     : Integer ;
    var   _true_type  : Boolean
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
  end;

  function TGIS_RendererVclSkia.CanvasTextBaseline(
    const _text : String
  ) : Single ;
  begin
    assert( assigned( FCanvas ) ) ;

    if IsStringEmpty( _text ) then begin
      result := 0 ;
      exit ;
    end ;

    prepareFont(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Font.Name,
      T_CanvasEncapsulation(FCanvas).Font.Size,
      T_CanvasEncapsulation(FCanvas).Font.Style,
      T_CanvasEncapsulation(FCanvas).Font.Color
    ) ;
    Result := T_CanvasEncapsulation(FCanvas).TextBaseline( _text ) ;
  end ;

  function TGIS_RendererVclSkia.CanvasTextExtent(
    const _text : String
  ) : TPoint ;
  begin
    if IsStringEmpty( _text ) then begin
      result := Point( 0, 0 ) ;
      exit ;
    end ;

    if ( _text = ' ' ) then begin
      Result := CanvasTextExtent( 'I I' ) ;
      Result.X := Result.X - CanvasTextExtent( 'II' ).X ;
      exit ;
    end ;

    assert( assigned( FCanvas ) ) ;
    if T_CanvasEncapsulation(FCanvas).Font.Size = 0 then begin
      Result := Point( 0, 0 ) ;
      exit ;
    end ;

    prepareFont(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Font.Name,
      T_CanvasEncapsulation(FCanvas).Font.Size,
      T_CanvasEncapsulation(FCanvas).Font.Style,
      T_CanvasEncapsulation(FCanvas).Font.Color
    ) ;
    Result := T_CanvasEncapsulation(FCanvas).MeasureText( _text ) ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawText(
    const _rect  : TRect   ;
    const _text  : String
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    if T_CanvasEncapsulation(FCanvas).Font.Size = 0 then exit ;
    if IsStringEmpty( _text ) then exit ;

    prepareFont(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Font.Name,
      T_CanvasEncapsulation(FCanvas).Font.Size,
      T_CanvasEncapsulation(FCanvas).Font.Style,
      T_CanvasEncapsulation(FCanvas).Font.Color
    ) ;
    T_CanvasEncapsulation(FCanvas).DrawText( _text, _rect.Left, _rect.Top ) ;
  end;

  procedure TGIS_RendererVclSkia.CanvasDrawLine(
    const _x1    : Integer ;
    const _y1    : Integer ;
    const _x2    : Integer ;
    const _y2    : Integer
  ) ;
  var
    afix : Double ;
  begin
    assert( assigned( FCanvas ) ) ;

    preparePen(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Pen.Color,
      T_CanvasEncapsulation(FCanvas).Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      Point( 0, 0 ),
      T_CanvasEncapsulation(FCanvas).Pen.LineCap,
      T_CanvasEncapsulation(FCanvas).Pen.LineJoin,
      T_CanvasEncapsulation(FCanvas).Pen.LineDash,
      Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
    ) ;
    T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;

    // avoid aliasing on 2, 4, 6 .. widths
    if T_CanvasEncapsulation(FCanvas).Pen.Width mod 2 = 0 then
      afix := 0
    else
      afix := 0.5 ;

    T_CanvasEncapsulation(FCanvas).DrawLine(
      _x1 + afix, _y1 + afix,
      _x2 + afix, _y2 + afix
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := length( _points ) ;
    CanvasDrawPolyLine( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := length( _points ) ;
    CanvasDrawPolyLine( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf ;
    const _count  : Integer
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := _count ;
    CanvasDrawPolyLine( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF ;
    const _count  : Integer
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := _count ;
    CanvasDrawPolyLine( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd   : ISkPathBuilder ;
    i    : Integer ;
    j    : Integer ;
    k    : Integer ;
    dfix : Double ;
  begin
    assert( assigned( FCanvas ) ) ;

    preparePen(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Pen.Color,
      T_CanvasEncapsulation(FCanvas).Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      Point( 0, 0 ),
      T_CanvasEncapsulation(FCanvas).Pen.LineCap,
      T_CanvasEncapsulation(FCanvas).Pen.LineJoin,
      Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
    ) ;
    T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;

    // avoid aliasing on 2, 4, 6 .. widths
    if T_CanvasEncapsulation(FCanvas).Pen.Width mod 2 = 0 then
      dfix := 0
    else
      dfix := 0.5 ;

    pd := TSkPathBuilder.Create ;
    try
      j := 0 ;
      for i := 0 to length( _parts ) - 1 do begin
        pd.MoveTo( _points[j].X + dfix,  _points[j].Y + dfix ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo( _points[j+k].X + dfix,  _points[j+k].Y + dfix ) ;
          inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;

      T_CanvasEncapsulation(FCanvas).DrawPath( pd.Detach ) ;
    finally
    end ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd : ISkPathBuilder ;
    i  : Integer ;
    j  : Integer ;
    k  : Integer ;
  begin
    assert( assigned( FCanvas ) ) ;

    preparePen(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Pen.Color,
      T_CanvasEncapsulation(FCanvas).Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      Point( 0, 0 ),
      T_CanvasEncapsulation(FCanvas).Pen.LineCap,
      T_CanvasEncapsulation(FCanvas).Pen.LineJoin,
      Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
    ) ;
    T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;

    pd := TSkPathBuilder.Create ;
    try
      j := 0 ;
      for i := 0 to length( _parts ) - 1 do begin
        pd.MoveTo( _points[j].X, _points[j].Y ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo( _points[j+k].X, _points[j+k].Y ) ;
          inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;

      T_CanvasEncapsulation(FCanvas).DrawPath( pd.Detach ) ;
    finally
    end ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawRectangle(
    const _rect : TRect
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;

    preparePen(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Pen.Color,
      T_CanvasEncapsulation(FCanvas).Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      Point( 0, 0 ),
      TGIS_LineCap.Square,
      TGIS_LineJoin.Miter,
      Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
    ) ;
    T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;
    CanvasDrawPolygon( [ Point( _rect.Left , _rect.Top    ),
                         Point( _rect.Right, _rect.Top    ),
                         Point( _rect.Right, _rect.Bottom ),
                         Point( _rect.Left , _rect.Bottom ),
                         Point( _rect.Left , _rect.Top    )
                       ]
                     ) ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawEllipse(
    const _x      : Integer ;
    const _y      : Integer ;
    const _width  : Integer ;
    const _height : Integer
  ) ;
  var
    w_outline : Integer ;
    usepen    : Boolean ;
  begin
    assert( assigned( FCanvas ) ) ;

    w_outline := Abs( T_CanvasEncapsulation(FCanvas).Pen.Width ) ;

    if ( T_CanvasEncapsulation(FCanvas).Pen.Style <> TGIS_PenStyle.Clear ) and
       ( Max( _width, _height ) < w_outline ) then begin
      usepen := T_CanvasEncapsulation(FCanvas).usepen ;
      T_CanvasEncapsulation(FCanvas).usepen := False ;
      try
        prepareBrush(
          FCanvas,
          T_CanvasEncapsulation(FCanvas).Brush.Color,
          nil,
          T_CanvasEncapsulation(FCanvas).Brush.Style,
          Point(0, 0)
        ) ;
        T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
          Self,
          T_CanvasEncapsulation(FCanvas),
          nil
        ) ;
        drawEllipse( _x, _y, _x + _width, _y + _height, FCanvas ) ;
        prepareBrush(
          FCanvas,
          T_CanvasEncapsulation(FCanvas).Pen.Color,
          nil,
          TGIS_BrushStyle.Solid,
          Point(0, 0)
        ) ;
        T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
          Self,
          T_CanvasEncapsulation(FCanvas),
          nil
        ) ;
        drawEllipse( _x - RoundS(w_outline / 2), _y - RoundS(w_outline / 2),
                     _x + _width + RoundS(w_outline / 2),
                     _y + _height + RoundS(w_outline / 2), FCanvas ) ;
      finally
        T_CanvasEncapsulation(FCanvas).usePen := usepen ;
      end ;
    end
    else begin
      prepareBrush(
        FCanvas,
        T_CanvasEncapsulation(FCanvas).Brush.Color,
        nil,
        T_CanvasEncapsulation(FCanvas).Brush.Style,
        Point(0, 0)
      ) ;
      T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
        Self,
        T_CanvasEncapsulation(FCanvas),
        nil
      ) ;
      preparePen(
        FCanvas,
        T_CanvasEncapsulation(FCanvas).Pen.Color,
        T_CanvasEncapsulation(FCanvas).Pen.Style,
        nil,
        TGIS_BrushStyle.Solid,
        Point( 0, 0 ),
        TGIS_LineCap.Round,
        TGIS_LineJoin.Round,
        Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
      ) ;
      T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
        Self,
        T_CanvasEncapsulation(FCanvas),
        nil
      ) ;
      drawEllipse( _x, _y, _x + _width, _y + _height, FCanvas ) ;
    end ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPie(
    const _angle_0  : Double  ;
    const _angle_1  : Double  ;
    const _radius   : Integer ;
    const _origin_x : Integer ;
    const _origin_y : Integer
  ) ;
  var
    points   : TGIS_DrawBufF  ;
    old_cap  : TGIS_LineCap ;
    old_join : TGIS_LineJoin   ;

    procedure stroke_arc( const _center_X  : Integer ;
                          const _center_Y  : Integer ;
                          const _radiusA   : Integer ;
                          const _radiusB   : Integer ;
                          const _start     : Double  ;
                          const _stop      : Double  ;
                          const _rotation  : Double  ;
                          const _segments  : Integer
                        ) ;
    var
      cnt      : Integer ;
      angle    : Double  ;
      arcangle : Double  ;
      step     : Double  ;
      steps    : Integer ;
      rsin     : Double  ;
      rcos     : Double  ;
      ptg      : TPointF ;

      function fmod( a, b : Double ) : Double ;
      var
        f : Integer ;
      begin
        f := TruncS( a/b ) ;
        Result := a - (b*f) ;
      end ;

    begin
      // rotation
      rsin := Sin( -_rotation ) ;
      rcos := Cos( -_rotation ) ;

      arcangle := fmod( (_stop-_start) +4*Pi, 2*Pi ) ;
      if arcangle = 0 then begin
        // full rotation
        if _stop <> _start then
          arcangle := 2*Pi ;
      end;

      // calculate number of segments - and minimize it
      steps := RoundS( Abs( arcangle ) / ( 2 * Pi ) * _segments ) ;
      steps := Max( 4, steps ) ;

      step := Abs( arcangle ) / steps ;

      // create vector of points
      SetLength( points, steps + 2 ) ;

      // calculate elliptical arc
      angle := _start ;
      for cnt := 0 to steps do begin
        ptg.Y := -_radiusB * Sin(angle) * rcos +
                  _radiusA * Cos(angle) * rsin + _center_Y ;
        ptg.X := -_radiusA * Cos(angle) * rcos -
                  _radiusB * Sin(angle) * rsin + _center_X ;
        points[cnt] := ptg ;

        angle := angle + step ;
      end ;
      points[steps+1] := PointF( _center_X, _center_Y ) ;
    end;

  begin
    assert( assigned( FCanvas ) ) ;

    try
      stroke_arc( _origin_x, _origin_y,
                  _radius div 2, _radius div 2,
                  _angle_0, _angle_1,
                  Pi/2, 30
                ) ;

      old_cap  := CanvasPen.LineCap ;
      old_join := CanvasPen.LineJoin   ;
      try
        CanvasPen.LineCap  := TGIS_LineCap.Round ;
        CanvasPen.LineJoin := TGIS_LineJoin.Round ;
        CanvasDrawPolygon( points ) ;
      finally
        CanvasPen.LineCap  := old_cap ;
        CanvasPen.LineJoin := old_join   ;
      end ;
    finally
      if assigned( points ) then
        points := nil ;
    end ;

  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawArc(
    const _x          : Integer ;
    const _y          : Integer ;
    const _radius     : Integer ;
    const _startAngle : Single ;
    const _sweepAngle : Single
  );
  begin
    assert( assigned( FCanvas ) ) ;

    preparePen(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Pen.Color,
      T_CanvasEncapsulation(FCanvas).Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      Point( 0, 0 ),
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
    ) ;
    T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;
    T_CanvasEncapsulation(FCanvas).DrawArc(
        RectF( _x - _radius, _y - _radius, _x + _radius, _y + _radius ),
        -_startAngle, -_sweepAngle, False
    ) ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawArc(
    const _x           : Integer ;
    const _y           : Integer ;
    const _width       : Integer ;
    const _height      : Integer ;
    const _startX      : Integer ;
    const _startY      : Integer ;
    const _endX        : Integer ;
    const _endY        : Integer
  );
  var
    start_angle : Single ;
    sweep_angle : Single ;
    center      : TPointF ;
    radius      : TPointF ;
    a1, a2      : Single ;
    dangle      : Single ;
  begin
    assert( assigned( FCanvas ) ) ;

    preparePen(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Pen.Color,
      T_CanvasEncapsulation(FCanvas).Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      Point( 0, 0 ),
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
    ) ;
    T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;

    center      := PointF(_x +_width/2, _y + _height/2);
    radius      := PointF(_width/2, _height/2);
    a1          := ArcTan2( _startY - center.Y, _startX - center.X );
    a2          := ArcTan2( _endY   - center.Y, _endX   - center.X );
    dangle      := (a2-a1);
    start_angle := RadToDeg( a1 ) ;
    sweep_angle := RadToDeg( dangle ) ;

    T_CanvasEncapsulation(FCanvas).DrawArc(
        RectF( center.X-radius.X, center.Y-radius.Y, center.X + radius.X, center.Y + radius.Y ),
        start_angle, sweep_angle, False
    ) ;
  end;

  procedure TGIS_RendererVclSkia.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := length( _points ) ;
    CanvasDrawPolygon( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := length( _points ) ;
    CanvasDrawPolygon( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf ;
    const _count  : Integer
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := _count ;
    CanvasDrawPolygon( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF ;
    const _count  : Integer
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := _count ;
    CanvasDrawPolygon( _points, parts );
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf  ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd   : ISkPathBuilder ;
    pp   : ISkPath ;
    i    : Integer ;
    j    : Integer ;
    k    : Integer ;
    dfix : Double ;
  begin
    assert( assigned( FCanvas ) ) ;

    prepareBrush(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Brush.Color,
      nil,
      T_CanvasEncapsulation(FCanvas).Brush.Style,
      Point(0, 0)
    ) ;
    T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;

    // avoid aliasing on 2, 4, 6 .. widths
    if T_CanvasEncapsulation(FCanvas).Pen.Width mod 2 = 0 then
      dfix := 0
    else
      dfix := 0.5 ;

    pd := TSkPathBuilder.Create ;
    try
      j := 0 ;
      for i := 0 to length( _parts ) - 1 do begin
        pd.MoveTo( _points[j].X + dfix,  _points[j].Y + dfix ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo( _points[j+k].X + dfix,  _points[j+k].Y + dfix ) ;
          inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;
      pd.Close ;
      pd.FillType := TSkPathFillType.EvenOdd ;
      pp := pd.Detach ;

      T_CanvasEncapsulation(FCanvas).FillPath( pp ) ;

      if T_CanvasEncapsulation(FCanvas).Pen.Width <> 0 then begin
        preparePen(
          FCanvas,
          T_CanvasEncapsulation(FCanvas).Pen.Color,
          T_CanvasEncapsulation(FCanvas).Pen.Style,
          nil,
          TGIS_BrushStyle.Solid,
          Point( 0, 0 ),
          TGIS_LineCap.Round,
          T_CanvasEncapsulation(FCanvas).Pen.LineJoin,
          Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
        ) ;
        T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
          Self,
          T_CanvasEncapsulation(FCanvas),
          nil
        ) ;
        T_CanvasEncapsulation(FCanvas).DrawPath( pp ) ;
      end ;
    finally
    end ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF  ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd : ISkPathBuilder ;
    pp : ISkPath ;
    i  : Integer ;
    j  : Integer ;
    k  : Integer ;
  begin
    assert( assigned( FCanvas ) ) ;

    prepareBrush(
      FCanvas,
      T_CanvasEncapsulation(FCanvas).Brush.Color,
      nil,
      T_CanvasEncapsulation(FCanvas).Brush.Style,
      Point(0, 0)
    ) ;
    T_Brush(T_CanvasEncapsulation(FCanvas).SkiaBrush).SelectBrush(
      Self,
      T_CanvasEncapsulation(FCanvas),
      nil
    ) ;

    pd := TSkPathBuilder.Create ;
    try
      j := 0 ;
      for i := 0 to length( _parts ) - 1 do begin
        pd.MoveTo( _points[j].X,  _points[j].Y ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo( _points[j+k].X,  _points[j+k].Y ) ;
          inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;
      pd.Close ;
      pd.FillType := TSkPathFillType.EvenOdd ;
      pp := pd.Detach ;

      T_CanvasEncapsulation(FCanvas).FillPath( pp ) ;

      if T_CanvasEncapsulation(FCanvas).Pen.Width <> 0 then begin
        preparePen(
          FCanvas,
          T_CanvasEncapsulation(FCanvas).Pen.Color,
          T_CanvasEncapsulation(FCanvas).Pen.Style,
          nil,
          TGIS_BrushStyle.Solid,
          Point( 0, 0 ),
          TGIS_LineCap.Round,
          T_CanvasEncapsulation(FCanvas).Pen.LineJoin,
          Abs( T_CanvasEncapsulation(FCanvas).Pen.Width )
        ) ;
        T_Pen(T_CanvasEncapsulation(FCanvas).SkiaPen).SelectPen(
          Self,
          T_CanvasEncapsulation(FCanvas),
          nil
        ) ;
        T_CanvasEncapsulation(FCanvas).DrawPath( pp ) ;
      end ;
    finally
    end ;
  end ;

  procedure TGIS_RendererVclSkia.CanvasDrawBitmap(
    const _bmp    : TGIS_Pixels ;
    const _size   : TPoint  ;
    const _dst    : TRect   ;
    const _format : TGIS_BitmapFormat ;
    const _order  : TGIS_BitmapLinesOrder
  ) ;
  begin
    RenderBitmap( nil, _bmp, _size, _dst, _format, _order );
  end;

  procedure TGIS_RendererVclSkia.CanvasDrawBitmap(
    const _bmp : TGIS_Bitmap ;
    const _dst : TRect
  ) ;
  begin
    RenderBitmap( nil, _bmp, _dst, True );
  end;

  procedure TGIS_RendererVclSkia.CanvasSetTransformation(
    const _angle    : Double  ;
    const _origin_x : Integer ;
    const _origin_y : Integer
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;

    if assigned( T_CanvasEncapsulation(FCanvas).Canvas ) then begin
      storedTransform := T_CanvasEncapsulation(FCanvas).Canvas.GetLocalToDeviceAs3x3 ;
      T_CanvasEncapsulation(FCanvas).Canvas.Translate( _origin_x, _origin_y ) ;
      if _angle <> 0 then
        T_CanvasEncapsulation(FCanvas).Canvas.RotateRadians( _angle ) ;
    end
    else
      assert( False ) ;
  end;

  procedure TGIS_RendererVclSkia.CanvasClearTransformation ;
  begin
    assert( assigned( FCanvas ) ) ;
    if assigned( T_CanvasEncapsulation(FCanvas).Canvas ) then
      T_CanvasEncapsulation(FCanvas).Canvas.SetMatrix( storedTransform )
    else
      assert( False ) ;
  end;

//==============================================================================
// TGIS_RendererVclSkiaCache
//==============================================================================

  procedure TGIS_RendererVclSkiaCache.doDestroy ;
  begin
    FreeObject( oBitmap ) ;
    inherited ;
  end ;

//uses
//  System.IO ;

//=============================================================================
// TGIS_BitmapSkia
//=============================================================================

  constructor TGIS_BitmapSkia.Create ;
  begin
    inherited ;

    FBitmap := nil ;
    FViewer := nil ;
  end;

  constructor TGIS_BitmapSkia.Create(
    const _width  : Integer ;
    const _height : Integer
  ) ;
  var
    surface : ISkSurface ;
  begin
    inherited Create;

    Premultiplied := True ;

    surface := TSkSurface.MakeRaster( _width, _height ) ;
    if surface <> nil then begin
      surface.Canvas.Clear( TAlphaColors.Null ) ;
      FBitmap := surface.MakeImageSnapshot.MakeRasterImage ;
      surface := nil ;
    end
    else
      FBitmap := nil ;

    FViewer := nil ;
  end;

  constructor TGIS_BitmapSkia.Create(
    const _image : ISkImage
  ) ;
  var
    surface : ISkSurface ;
  begin
    inherited Create;

    Premultiplied := True ;

    surface := TSkSurface.MakeRaster( _image.Width, _image.Height ) ;
    if surface <> nil then begin
      surface.Canvas.DrawImage( _image, 0, 0, nil ) ;
      FBitmap := surface.MakeImageSnapshot.MakeRasterImage ;
      surface := nil ;
    end
    else
      FBitmap := nil ;

    FViewer := nil ;
  end;

  class function TGIS_BitmapSkia.FromBitmap(
    const _bmp : TObject
  ) : TGIS_BitmapAbstract ;
  begin
    if _bmp = nil then
      Result := nil
    else
      Result := TGIS_BitmapSkia.Create( TSkImage( _bmp ) ) ;
  end;

  class function TGIS_BitmapSkia.FromFile(
    const _path : String
  ) : TGIS_BitmapAbstract ;
  var
    res : TGIS_BitmapSkia ;
  begin
    res := nil ;
    try
      res := TGIS_BitmapSkia.Create ;
      res.FBitmap := TSkImage.MakeFromEncodedFile( _path ).MakeRasterImage ;
      res.Premultiplied := True;
      Result := res ;
    except
      FreeObject( res ) ;
      raise ;
    end ;
  end ;

  class function TGIS_BitmapSkia.FromStream(
    const _stream : TObject
  ) : TGIS_BitmapAbstract ;
  var
    res : TGIS_BitmapSkia ;
  begin
    try
      res := TGIS_BitmapSkia.Create ;
      TStream( _stream ).Position := 0 ;
      res.FBitmap := TSkImage.MakeFromEncodedStream( TStream( _stream ) ).MakeRasterImage ;
      res.Premultiplied := True;
      Result := res ;
    except
      FreeObject( res ) ;
      raise;
    end ;
  end ;

  procedure TGIS_BitmapSkia.doDestroy ;
  begin
    FreeObject( FViewer ) ;
    FBitmap := nil ;
    inherited ;
  end;

  function TGIS_BitmapSkia.fget_Width
    : Integer ;
  begin
    Result := FBitmap.Width ;
  end;

  function TGIS_BitmapSkia.fget_Height
    : Integer ;
  begin
    Result := FBitmap.Height ;
  end;

  function TGIS_BitmapSkia.fget_PPI
    : Integer ;
  begin
    result := 96 ;
  end ;

  procedure TGIS_BitmapSkia.fset_PPI(
    const _value : Integer
  ) ;
  begin
  end ;

  function TGIS_BitmapSkia.fget_Data
    : TObject ;
  begin
    Result := TSkImage( FBitmap ) ;
  end;

  procedure TGIS_BitmapSkia.fset_Data(
    const _value : TObject
  ) ;
  begin
    if not ( _value is TSkImage ) then exit ;
    FBitmap := ISkImage( TSkImage( _value ) ).MakeRasterImage ;
  end;

  procedure TGIS_BitmapSkia.ToFile(
    const _path : String
  ) ;
  var
    ext : String ;
    bmp : TGIS_Bitmap ;
  begin
    ext := GetFileExt( _path ) ;
    if ( ext = '.jpg' ) or
       ( ext = '.jpeg' ) or
       ( ext = '.webp' ) or
       ( ext = '.png' ) then
      FBitmap.EncodeToFile( _path )
    else begin
      bmp := TGIS_Bitmap.Create ;
      try
        bmp.NativeBitmap := TBitmap.CreateFromSkImage( TSkImage( FBitmap ) ) ;
        bmp.SaveToFile( _path ) ;
      finally
        FreeObject( bmp ) ;
      end ;
    end ;
  end ;

  procedure TGIS_BitmapSkia.ToFile(
    const _path        : String ;
    const _format      : TGIS_PixelFormat ;
    const _subformat   : TGIS_PixelSubFormat ;
    const _compression : Integer
  ) ;
  begin
    ToFile( _path ) ;
  end ;

  procedure TGIS_BitmapSkia.ToStream(
    const _stream : TObject
  ) ;
  var
    bmp : TGIS_Bitmap ;
  begin
    try
      FBitmap.EncodeToStream( TStream( _stream) ) ;
    except
      bmp := TGIS_Bitmap.Create ;
      bmp.NativeBitmap := TBitmap.CreateFromSkImage( TSkImage( FBitmap ) ) ;
      bmp.SaveToStream( _stream ) ;
      FreeObject( bmp ) ;
    end ;
  end;

  procedure TGIS_BitmapSkia.ToStream(
    const _stream      : TObject ;
    const _format      : TGIS_PixelFormat ;
    const _subformat   : TGIS_PixelSubFormat ;
    const _compression : Integer
  ) ;
  begin
    ToStream( _stream ) ;
  end ;

  procedure TGIS_BitmapSkia.MakeTransparent ;
  var
    pixmap : ISkPixmap ;
    ptr    : PByte    ;
    x,y    : Integer  ;
    w1     : Integer  ;
    w,h    : Integer  ;
    co,cl  : Cardinal ;
  begin
    Assert( Assigned( FBitmap ) ) ;

    pixmap := FBitmap.PeekPixels ;
    if not assigned( pixmap ) then exit ;
    ptr := PByte( pixmap.Pixels ) ;
    if not assigned( ptr ) then exit ;

    co := PCardinal( ptr )^ ;

    h := FBitmap.Height ;
    w := FBitmap.Width  ;

    for y := 0 to h - 1 do begin
      w1 := y * pixmap.RowBytes ;
      for x := 0 to w - 1 do begin
        cl := PCardinal( ptr + w1 + x*4 )^ ;
        if cl = co then
          PCardinal( ptr + w1 + x*4 )^ := TGIS_Color.None.ARGB ;
      end ;
    end ;
  end ;

  procedure TGIS_BitmapSkia.Clear(
    const _color : TGIS_Color
  ) ;
  var
    ipixmap : ISkPixmap ;
  begin
    ipixmap := FBitmap.PeekPixels ;
    ipixmap.Erase( _color.ToARGB ) ;
  end ;

  procedure TGIS_BitmapSkia.LockPixels(
    var   _pixels   : TGIS_Pixels ;
    const _writable : Boolean     ;
    const _format   : TGIS_BitmapFormat ;
    const _order    : TGIS_BitmapLinesOrder
  ) ;
  var
    i,k       : Integer ;
    x,y       : Integer ;
    w,h       : Integer ;
    steph     : Integer ;
    revcolors : Boolean ;
    revlines  : Boolean ;
    pixmap    : ISkPixmap ;
    cl        : Int32   ;
    w1        : Integer ;
    a,r,g,b   : Byte    ;
  begin
    if ( length( FPixelData ) <> FBitmap.Width * FBitmap.Height )
       or
       ( _writable <> FWritable )
       or
       ( _format <> FFormat )
       or
       ( _order <> FLineOrder )
    then begin
      if      _format = TGIS_BitmapFormat.Native then
              revcolors := False
      else if _format = TGIS_BitmapFormat.ARGB then
              revcolors := False
      else    revcolors := True ;

      if      _order = TGIS_BitmapLinesOrder.Native then
              revlines := False
      else if _order = TGIS_BitmapLinesOrder.Down then
              revlines := False
      else    revlines := True ;

      UnlockPixels ;

      pixmap := FBitmap.PeekPixels ;
      if not assigned( pixmap ) then exit ;

      SetLength( FPixelData, FBitmap.Width * FBitmap.Height ) ;

      h := FBitmap.Height ;
      w := FBitmap.Width  ;

      if revlines then begin
        steph := -1 ;
        y     :=  h - 1 ;
      end
      else begin
        steph :=  1 ;
        y     :=  0 ;
      end;

      k := 0 ;

      if revcolors then begin
        for i := 0 to h - 1 do begin
          w1 := y * pixmap.RowBytes ;
          for x := 0 to w - 1 do begin
            cl := PInteger( PByte(pixmap.Pixels) + w1 + x*4 )^ ;

            a := ( cl shr 24 ) and $FF ;
            r := ( cl shr 16 ) and $FF ;
            g := ( cl shr  8 ) and $FF ;
            b := ( cl        ) and $FF ;

            if Premultiplied and ( a <> 255 ) and ( a <> 0 ) then begin
              r := Byte( r * 255 div a ) ;
              g := Byte( g * 255 div a ) ;
              b := Byte( b * 255 div a ) ;
            end ;

            FPixelData[k] := a shl 24 + b shl 16 + g shl 8 + r ;

            inc( k ) ;
          end;
          y := y + steph ;
        end;
      end
      else begin
        for i := 0 to h - 1 do begin
          w1 := y * pixmap.RowBytes ;
          for x := 0 to w - 1 do begin
            cl := PInteger( PByte(pixmap.Pixels) + w1 + x*4 )^ ;

            a := ( cl shr 24 ) and $FF ;
            r := ( cl shr 16 ) and $FF ;
            g := ( cl shr  8 ) and $FF ;
            b := ( cl        ) and $FF ;

            if Premultiplied and ( a <> 255 ) and ( a <> 0 ) then begin
              r := Byte( r * 255 div a ) ;
              g := Byte( g * 255 div a ) ;
              b := Byte( b * 255 div a ) ;
            end ;

            FPixelData[k] := a shl 24 + r shl 16 + g shl 8 + b ;

            inc( k ) ;
          end;
          y := y + steph ;
        end;
      end;

      FWritable  := _writable ;
      FFormat    := _format   ;
      FLineOrder := _order    ;
    end ;

    _pixels := FPixelData  ;
  end ;

  procedure TGIS_BitmapSkia.UnlockPixels ;
  var
    i,k       : Integer ;
    x,y       : Integer ;
    w,h       : Integer ;
    steph     : Integer ;
    revcolors : Boolean ;
    revlines  : Boolean ;
    pixmap    : ISkPixmap ;
    w1        : Integer ;
    cl        : Int32   ;
    a,r,g,b   : Byte    ;
  begin
    if Length( FPixelData ) = 0 then
      exit ;

    if FWritable then begin
      if      FFormat = TGIS_BitmapFormat.Native then
              revcolors := False
      else if FFormat = TGIS_BitmapFormat.ARGB then
              revcolors := False
      else    revcolors := True ;

      if      FLineOrder = TGIS_BitmapLinesOrder.Native then
              revlines := False
      else if FLineOrder = TGIS_BitmapLinesOrder.Down then
              revlines := False
      else    revlines := True ;

      pixmap := FBitmap.PeekPixels ;
      if not assigned( pixmap ) then exit ;

      h := FBitmap.Height ;
      w := FBitmap.Width  ;

      if revlines then begin
          steph := -1 ;
          y     :=  h - 1 ;
      end
      else begin
          steph :=  1 ;
          y    :=  0 ;
      end;

      k := 0 ;

      if revcolors then begin
        for i := 0 to h - 1 do begin
          w1 := y * pixmap.RowBytes ;
          for x := 0 to w - 1 do begin
            cl := FPixelData[k] ;

            a := ( cl shr 24 ) and $FF ;
            b := ( cl shr 16 ) and $FF ;
            g := ( cl shr  8 ) and $FF ;
            r := ( cl        ) and $FF ;

            if Premultiplied and ( a <> 255 ) then begin
              r := Byte( r * a div 255 ) ;
              g := Byte( g * a div 255 ) ;
              b := Byte( b * a div 255 ) ;
            end;

            PInteger( PByte(pixmap.Pixels) + w1 + x*4 )^ := a shl 24 + r shl 16 + g shl 8 + b ;

            inc( k ) ;
          end;
          y := y + steph ;
        end;
      end
      else begin
        for i := 0 to h - 1 do begin
          w1 := y * pixmap.RowBytes ;
          for x := 0 to w - 1 do begin
            cl := FPixelData[k] ;

            a := ( cl shr 24 ) and $FF ;
            r := ( cl shr 16 ) and $FF ;
            g := ( cl shr  8 ) and $FF ;
            b := ( cl        ) and $FF ;

            if Premultiplied and ( a <> 255 ) then begin
              r := Byte( r * a div 255 ) ;
              g := Byte( g * a div 255 ) ;
              b := Byte( b * a div 255 ) ;
            end;

            PInteger( PByte(pixmap.Pixels) + w1 + x*4 )^ := a shl 24 + r shl 16 + g shl 8 + b ;

            inc( k ) ;
          end;
          y := y + steph ;
        end;
      end;

    end ;

    SetLength( FPixelData, 0 ) ;
  end ;

  procedure TGIS_BitmapSkia.DrawShape(
    const _shape    :  TObject ;
    const _outline  :  Boolean ;
    var   _scale    :  Double  ;
    var   _offset   :  TPoint
  ) ;
  begin
    DrawShape( _shape, 0, _outline, _scale, _offset ) ;
  end;

  procedure TGIS_BitmapSkia.DrawShape(
    const _shape    :  TObject ;
    const _ppi      :  Integer ;
    const _outline  :  Boolean ;
    var   _scale    :  Double  ;
    var   _offset   :  TPoint
  ) ;
  begin
    DrawShape(  _shape, _ppi, _outline,
               TGIS_Color.LightGray, TGIS_Color.DimGray,
               _scale, _offset
             ) ;
  end;

  procedure TGIS_BitmapSkia.DrawShape(
    const _shape    :  TObject    ;
    const _ppi      :  Integer    ;
    const _outline  :  Boolean    ;
    const _areacolor:  TGIS_Color ;
    const _linecolor:  TGIS_Color ;
    var   _scale    :  Double     ;
    var   _offset   :  TPoint
  ) ;
  var
    lv      : TGIS_LayerVector ;
    shp     : TGIS_Shape       ;
    shp_tmp : TGIS_Shape       ;
    ext_tmp : TGIS_Extent      ;
    vwr     : TGIS_ViewerBmp   ;
    ptg1    : TGIS_Point       ;
    pt1     : TPoint           ;
    ptg2    : TGIS_Point       ;
    pt2     : TPoint           ;
    rnd     : TGIS_RendererAbstract ;
  begin
    shp := TGIS_Shape( _shape ) ;

    rnd := TGIS_RendererVclSkia.Create ;
    try
      vwr := TGIS_ViewerBmp.Create( self.Master, rnd ) ;
      try
        if _ppi > 0 then
          vwr.CustomPPI := _ppi ;

        vwr.Color := TGIS_Color.White ;
        lv := TGIS_LayerVector.Create ;

        if assigned( shp.Layer ) then
          lv.CS := shp.Layer.CS ;

        shp_tmp := lv.AddShape( shp ) ;

        vwr.Add( lv ) ;

        if shp.LockLevel < TGIS_Lock.Projection then begin
          if assigned( shp.Layer ) and assigned( shp.Layer.Viewer ) then
            vwr.CS := shp.Layer.Viewer.Ref.CS ;
        end;

        vwr.FullExtent ;

        lv.IgnoreShapeParams := True ;
        lv.Params.Line.Width := -3  ;
        lv.Params.Line.Color := _linecolor ;

        if _outline then begin
          lv.Params.Area.OutlineWidth := -3  ;
          lv.Params.Area.OutlineColor := _linecolor ;
        end
        else
          lv.Params.Area.OutlineWidth := 0  ;
        lv.Params.Area.Color := _areacolor ;

        vwr.Draw ;

        ext_tmp := shp_tmp.ProjectedExtent ;
        ptg1 := GisPoint( ext_tmp.XMin, ext_tmp.YMax ) ;
        pt1  := vwr.MapToScreen( ptg1 ) ;
        ptg2 := GisPoint( ext_tmp.XMax, ext_tmp.YMin ) ;
        pt2  := vwr.MapToScreen( ptg2 ) ;

        _scale  := Sqrt( Sqr( pt1.X  - pt2.X  ) + Sqr( pt1.Y  - pt2.Y  ) ) /
                   Sqrt( Sqr( ptg1.X - ptg2.X ) + Sqr( ptg1.Y - ptg2.Y ) ) ;
        _offset := pt1 ;
      finally
        FreeObject(  vwr ) ;
      end;
    finally
      FreeObject( rnd ) ;
    end ;
  end;

  procedure TGIS_BitmapSkia.DrawSymbol(
    const _name     :  String
  ) ;
  begin
    DrawSymbol( _name, 0 ) ;
  end;

  procedure TGIS_BitmapSkia.DrawSymbol(
    const _name     :  String ;
    const _ppi      :  Integer
  ) ;
  begin
    DrawSymbol( _name, _ppi, TGIS_Color.RenderColor, TGIS_Color.RenderColor ) ;
  end;

  procedure TGIS_BitmapSkia.DrawSymbol(
    const _name     :  String     ;
    const _ppi      :  Integer    ;
    const _areacolor:  TGIS_Color ;
    const _linecolor:  TGIS_Color
  ) ;
  var
    lv      : TGIS_LayerVector ;
    shp     : TGIS_Shape       ;
    vwr     : TGIS_ViewerBmp   ;
    old_cnt : Boolean          ;
    rnd     : TGIS_RendererAbstract ;
  begin
    rnd := TGIS_RendererVclSkia.Create ;
    try
      vwr := TGIS_ViewerBmp.Create( self.Master, rnd ) ;
      try
        if _ppi > 0 then
          vwr.CustomPPI := _ppi ;

        vwr.Color := TGIS_Color.None ;

        lv := TGIS_LayerVector.Create ;
        lv.Open ;

        shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
        shp.AddPart ;
        shp.AddPoint( GisPoint( 0, 0 ) ) ;
        shp.AddPart ;
        lv.Extent := GisExtent( -90, -90, 90, 90 ) ;

        lv.Params.Marker.Color := _areacolor ;
        lv.Params.Marker.Size := - RoundS( vwr.Width * 2 / 3 ) ;
        lv.Params.Marker.OutlineColor := _linecolor ;
        lv.Params.Marker.Symbol := SymbolList.Prepare( _name ) ;

        vwr.Add( lv ) ;

        old_cnt := lv.Params.Marker.Symbol.AutoCenter ;
        lv.Params.Marker.Symbol.AutoCenter := True ;

        vwr.FullExtent ;
        vwr.Draw ;

        lv.Params.Marker.Symbol.AutoCenter := old_cnt ;
      finally
        FreeObject( vwr ) ;
      end;
    finally
      FreeObject( rnd ) ;
    end ;
  end;

  procedure TGIS_BitmapSkia.DrawGlyph(
    const _symbol   :  TObject    ;
    const _ppi      :  Integer    ;
    const _color    :  TGIS_Color ;
    const _enabled  :  Boolean
  ) ;
  var
    lv      : TGIS_LayerVector ;
    shp     : TGIS_Shape       ;
    vwr     : TGIS_ViewerBmp   ;
    old_cnt : Boolean          ;
    rnd     : TGIS_RendererAbstract ;
  begin
    rnd := TGIS_RendererVclSkia.Create ;
    try
      vwr := TGIS_ViewerBmp.Create( self.Master, rnd ) ;
      try
        vwr.CustomPPI := _ppi ;

        vwr.Color := TGIS_Color.None ;

        lv := TGIS_LayerVector.Create ;
        lv.Open ;

        shp := lv.CreateShape( TGIS_ShapeType.Point ) ;
        shp.AddPart ;
        shp.AddPoint( GisPoint( 0, 0 ) ) ;
        shp.AddPart ;
        lv.Extent := GisExtent( -90, -90, 90, 90 ) ;

        lv.Params.Marker.Color := _color ;
        lv.Params.Marker.Size := -vwr.Height ;
        lv.Params.Marker.OutlineColor := _color ;
        lv.Params.Marker.Symbol := TGIS_SymbolAbstract( _symbol );

        vwr.Add( lv ) ;

        old_cnt := lv.Params.Marker.Symbol.AutoCenter ;
        lv.Params.Marker.Symbol.AutoCenter := True ;

        vwr.FullExtent ;
        vwr.Draw ;

        lv.Params.Marker.Symbol.AutoCenter := old_cnt ;
      finally
        FreeObject( vwr ) ;
      end ;
    finally
      FreeObject( rnd ) ;
    end;
  end;

  function TGIS_BitmapSkia.CreateViewer : IInterface ;
  begin
    FreeObject( FViewer ) ;

    FViewer := TGIS_ViewerBmp.Create( self.Master, nil ) ;
    Result := FViewer as IGIS_Viewer;
  end ;

//=============================================================================
// TGIS_BitmapFactorySkia
//=============================================================================

  function TGIS_BitmapFactorySkia.DoCreate(
    const _parent : TGIS_Bitmap ;
    const _width  : Integer ;
    const _height : Integer
  ) : TGIS_BitmapAbstract ;
  begin
    Result := TGIS_BitmapSkia.Create( _width, _height ) ;
    Result.Master := _parent ;
  end;

  function TGIS_BitmapFactorySkia.DoCreateFromBitmap(
    const _parent : TGIS_Bitmap ;
    const _bmp    : TObject
  ) : TGIS_BitmapAbstract ;
  begin
    Result := TGIS_BitmapSkia.FromBitmap( _bmp ) ;
    Result.Master := _parent ;
  end;

  function TGIS_BitmapFactorySkia.DoCreateFromFile(
    const _parent : TGIS_Bitmap ;
    const _path   : String
  ) : TGIS_BitmapAbstract ;
  begin
    Result := TGIS_BitmapSkia.FromFile( _path ) ;
    Result.Master := _parent ;
  end;

  function TGIS_BitmapFactorySkia.DoCreateFromStream(
    const _parent : TGIS_Bitmap ;
    const _stream : TObject
  ) : TGIS_BitmapAbstract ;
  begin
    Result := TGIS_BitmapSkia.FromStream( _stream ) ;
    Result.Master := _parent ;
  end;

   function TGIS_BitmapFactorySkia.DoCreateFromResource(
    const _parent : TGIS_Bitmap ;
    const _ref    : IntPtr ;
    const _name   : String
  ) : TGIS_BitmapAbstract ;
  (*var
    obj  : Object  ;
    name : String  ;
    rs   : String  ;
    asmb : &Assembly ; *)
  begin
    Result :=  nil ;
    //assert( False ) ;
    (*var position = _name.LastIndexOf('.');
    if position >= 0 then begin
      name := _name.Substring(position + 1) ;
      rs   := _name.Substring(0, position-1) ;
    end
    else begin
      name := _name ;
      rs   := 'TatukGIS.NDK.Properties.Resources';
    end;

    if _ref = 0 then
      asmb := self.GetType.Assembly
    else
      asmb := Object(_ref).GetType.Assembly ;

    using rm := new ResourceManager( rs, asmb )
    do begin
      obj := rm.GetObject(name) ;
    end ;

    if not assigned( obj ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                   _name,
                                   0
                                 ) ;

    Result := DoCreateFromBitmap( _parent, obj ) ;*)
  end;

  function TGIS_BitmapFactorySkia.NativeFormat
    : TGIS_BitmapFormat ;
  begin
    Result := TGIS_BitmapFormat.ARGB ;
  end;

  function TGIS_BitmapFactorySkia.NativeLineOrder
    : TGIS_BitmapLinesOrder ;
  begin
    Result := TGIS_BitmapLinesOrder.Down ;
  end;

  function TGIS_BitmapFactorySkia.BitmapType
    : TGIS_BitmapType ;
  begin
    Result := TGIS_BitmapType.Skia ;
  end;

{$REGION 'T_OpenGLSkiaConnection'}

  constructor T_OpenGLSkiaConnection.Create(
    const _handle : HWND
  ) ;
  var
    pfd : TPixelFormatDescriptor ;
    pf  : Integer ;
  begin
    inherited Create ;

    hWindow := _handle ;
    hDeviceContext := GetDC( _handle ) ;

    FillChar( pfd, SizeOf(pfd), 0 ) ;
    pfd.nSize := SizeOf(pfd) ;
    pfd.nVersion := 1 ;
    pfd.dwFlags := PFD_DRAW_TO_WINDOW or PFD_SUPPORT_OPENGL or PFD_DOUBLEBUFFER ;
    pfd.iPixelType := PFD_TYPE_RGBA ;
    pfd.cColorBits := 32 ;
    pfd.cDepthBits := 32 ;
    //pfd.cStencilBits := 8 ;
    pfd.cStencilBits := 0;
    pfd.iLayerType := PFD_MAIN_PLANE ;

    pf := ChoosePixelFormat( hDeviceContext, @pfd ) ;
    SetPixelFormat( hDeviceContext, pf, @pfd ) ;

    hGLRenderingContext := wglCreateContext(hDeviceContext);

    oInterface := nil ;
    oContext := nil ;

    if wglMakeCurrent( hDeviceContext, hGLRenderingContext ) then begin
      oInterface := TGrGlInterface.MakeNative ;
      if Assigned( oInterface ) then begin
        if ( not oInterface.Validate() ) then begin
          oInterface := nil ;
          exit ;
        end ;
        oContext := TGrDirectContext.MakeGl( oInterface ) ;
      end ;
    end;
  end ;

  procedure T_OpenGLSkiaConnection.doDestroy ;
  begin
    if hGLRenderingContext <> IntPtr(0) then begin
      wglMakeCurrent( hDeviceContext, hGLRenderingContext ) ;
      if Assigned( oContext ) then
        oContext.AbandonContext ;
      oContext := nil ;
      oInterface := nil ;
      wglDeleteContext( hGLRenderingContext ) ;
      hGLRenderingContext := IntPtr(0) ;
      ReleaseDC( hWindow, hDeviceContext ) ;
    end ;
    inherited ;
  end ;

  function T_OpenGLSkiaConnection.CreateCanvas(
    const _width  : Integer ;
    const _height : Integer
  ) : TObject ;
  const
    GL_RGBA8 = $8058 ;
  var
    fbInfo : TGrGlFramebufferInfo ;
  begin
    result := nil ;
    oBackendRT := nil ;
    oSurface := nil ;
    try
      try
        fbInfo.FBOID := 0 ;
        fbInfo.Format := GL_RGBA8 ;

        oBackendRT := TGrBackendRenderTarget.CreateGl( _width, _height,
                                                    (*sampelCount=*)1,
                                                    (*stencilBits=*)8,
                                                    fbInfo
                                                  ) ;

        oSurface := TSKSurface.MakeFromRenderTarget( oContext, oBackendRT,
                                       TGrSurfaceOrigin.BottomLeft,
                                       TSkColorType.Rgba8888,
                                       (*colorSpace=*)nil
                                       //(*surfaceProps=*)nil
                                     ) ;
        Result := TSkCanvas( oSurface.Canvas ) ;
      finally
        if not assigned( result ) then begin
          oSurface := nil ;
          oBackendRT := nil ;
        end ;
      end ;
    except
      oSurface := nil ;
      oBackendRT := nil ;
      Result := nil ;
    end ;
  end ;

  function T_OpenGLSkiaConnection.CreateRendererCanvas(
    const _width  : Integer ;
    const _height : Integer
  ) : ISkSurface ;
  var
    info : TSkImageInfo ;
  begin
    try
      try
        info := TSkImageInfo.Create( _width, _height,
                                     TSkColorType.Bgra8888,
                                     TSkAlphaType.Premul ) ;
        Result := TSkSurface.MakeRenderTarget( oContext, true, info ) ;
      finally
      end ;
    except
    end ;
  end ;

  procedure T_OpenGLSkiaConnection.FreeCanvas(
    var _canvas : TObject
  ) ;
  begin
    _canvas := nil ;
  end ;

  procedure T_OpenGLSkiaConnection.Flush ;
  begin
    oSurface.Flush();
    //oSurface.FlushAndSubmit();
    oContext.Flush();
    SwapBuffers( hDeviceContext ) ;
  end ;

  procedure T_OpenGLSkiaConnection.RendererFlush(
    const _surface : ISkSurface ;
    var   _image  : ISkImage
  ) ;
  var
    img : ISkImage ;
    surface : ISkSurface ;
  begin
    if assigned( _surface ) then
      surface := _surface
    else
      surface := oSurface ;
    surface.Flush();

    img := surface.MakeImageSnapshot ;
    (*if TGIS_Bitmap.BitmapType = TGIS_BitmapType.VCL then
      _image := img.MakeRasterImage
    else*)
    _image := img.MakeTextureImage( oContext ) ;

    oContext.Flush();
  end ;

{$ENDREGION}


{$REGION 'T_OpenGLConnectionFactory'}

type
  /// OpenGL factory
  T_OpenGLSkiaConnectionFactory = class( TGIS_OpenGLSkiaConnectionFactory )
    function CreateConnection( const _handle : THandle
                             ) : TGIS_OpenGLSkiaConnection; override;
  end;

function T_OpenGLSkiaConnectionFactory.CreateConnection(
  const _handle : THandle
) : TGIS_OpenGLSkiaConnection ;
begin
  Result := T_OpenGLSkiaConnection.Create( _handle ) ;
  if not Assigned( T_OpenGLSkiaConnection(Result).oInterface ) then begin
    FreeObjectNotNil( Result ) ;
    Result := nil ;
  end;
end;

{$ENDREGION}


initialization
  {$IFDEF GIS_SKIA_MODIFIED}
  if SkAvaiable then begin
  {$ENDIF}
    OpenGLSkiaConnectionFactory := T_OpenGLSkiaConnectionFactory.Create ;
    RegisterBitmapFactory( 'TGIS_BitmapFactorySkia', TGIS_BitmapFactorySkia ) ;
    RegisterRenderer( 'TGIS_RendererVclSkia', TGIS_RendererVclSkia ) ;
    BitmapFactorySkia := TGIS_BitmapFactorySkia.Create ;
  {$IFDEF GIS_SKIA_MODIFIED}
  end;
  {$ENDIF}
finalization
  FreeObject( BitmapFactorySkia ) ;
  FreeObject( OpenGLSkiaConnectionFactory) ;
  // clear to be sure that all picture symbols based on SkImage are freed
  SymbolList.Clear ;

//==================================== END ====================================
{$ENDIF}
end.


