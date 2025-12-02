//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
  Direct2D based renderer.
}

unit Lider.CG.GIS.VCL.GeoRendererDirect2D ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoRendererDirect2D"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  System.Types,
  System.UITypes,
  Winapi.Windows,
  Winapi.D2D1,
  Winapi.DxgiFormat,
  VCL.Graphics,
  VCL.Direct2D,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoRendererAbstract,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.VCL.GeoRenderer ;

type

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Encapsulates all objects associated with the Direct2D canvas.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  TGIS_CanvasInternal = class( TGIS_ObjectDisposable )
    private
      Canvas       : TCanvas ;

      bmp_hnd      : HBITMAP ;
      bmp_old      : HBITMAP ;
      bmp_dc       : HDC ;

      Font         : TGIS_Font  ;
      Pen          : TGIS_Pen   ;
      Brush        : TGIS_Brush ;

      D2DPen       : TObject    ;
      D2DBrush     : TObject    ;

      d2dCanvas      : TDirect2DCanvas ;
      ownsCanvas     : Boolean ;
      d2dPenBrush    : ID2D1Brush ;
      d2dPenStyle    : ID2D1StrokeStyle ;
      d2dBrushBrush  : ID2D1Brush ;
      d2dLockLevel   : Integer ;

      transparency : Integer    ;
      usePen       : Boolean    ;
      useBrush     : Boolean    ;

    private
      function  isToBeDrawn  : Boolean ;
      function  isToBeFilled : Boolean ;

      {$IFNDEF LEVEL_XE6_VCL}
        function  createBitmapFix     ( const _bitmap : TBitmap
                                      ) : ID2D1Bitmap;
        procedure stretchDrawFix      ( const _rect   : TRect;
                                        const _grph   : TGraphic
                                      ) ;
      {$ENDIF}

      function  prepareSolidColorBrush( const _color  : TGIS_Color ;
                                        const _d2dbp  : TD2D1BrushProperties
                                      ) : ID2D1SolidColorBrush;
      function  prepareBitmapBrush    ( const _bitmap : TBitmap ;
                                        const _d2dbp  : TD2D1BrushProperties
                                      ) : ID2D1BitmapBrush;
    protected
      procedure doDestroy ; override;

    public
      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to draw on
      /// </param>
      constructor Create ( const _bmp : TBitmap
                         ) ; overload;

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_target">
      ///   render target
      /// </param>
      constructor Create ( const _target    : ID2D1RenderTarget
                         ) ; overload;

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas object
      /// </param>
      constructor Create ( const _canvas    : TDirect2DCanvas
                         ) ; overload;

    public
      /// <summary>
      ///   Prepares the canvas for drawing.
      /// </summary>
      procedure BeginDraw ;
      /// <summary>
      ///   Finishes drawing.
      /// </summary>
      procedure EndDraw ;

  end ;

  /// <summary>
  ///   Renderer for VCL based on Direct2D.
  /// </summary>
  TGIS_RendererVclDirect2D = class( TGIS_RendererVclAbstract )
    private
      brushCache : TGIS_BrushCache ;
    private
      /// <summary>
      ///   Map canvas encapsulation.
      /// </summary>
      oCanvas            : TGIS_CanvasInternal ;

      /// <summary>
      ///   Selection canvas encapsulation.
      /// </summary>
      oSelectionCanvas   : TGIS_CanvasInternal ;

      /// <summary>
      ///   Charts canvas encapsulation.
      /// </summary>
      oChartsCanvas      : TGIS_CanvasInternal ;

      /// <summary>
      ///   Labels canvas encapsulation.
      /// </summary>
      oLabelsCanvas      : TGIS_CanvasInternal ;

      /// <summary>
      ///   Edit canvas encapsulation.
      /// </summary>
      oEditCanvas        : TGIS_CanvasInternal ;

      /// <summary>
      ///   Transparent canvas underlying bitmap.
      /// </summary>
      oTransparentBitmap : TBitmap ;

      /// <summary>
      ///   Transparent canvas encapsulation.
      /// </summary>
      oTransparentCanvas : TGIS_CanvasInternal ;

      /// <summary>
      ///   Selection color with transparency altered for topmost layers.
      /// </summary>
      colorSelection     : TGIS_Color ;

      /// <summary>
      ///   Reference to current canvas encapsulation.
      /// </summary>
      FCanvas            : TGIS_CanvasInternal ;

      /// <summary>
      ///   Stored Transformation.
      /// </summary>
      storedTransform : TD2D1Matrix3x2F ;
  private
      procedure prepareSelectionCanvas
                                 ( _transparently : Boolean ;
                                   _useBaseMap    : Boolean
                                 ) ;
      procedure prepareChartsCanvas ;
      procedure prepareLabelsCanvas ;

    private
      iTolerance      : Integer ;
      iToleranceSel   : Integer ;
      inEdit          : Boolean ;
      sourceShape     : TObject ;
      ignorePPI       : Boolean ;
      flashed         : Boolean ;
      pextra          : Boolean ;
      path_geometry   : ID2D1PathGeometry ;
      create_geometry : Boolean ;

    protected
      function  fget_CanvasPen   : TGIS_Pen ; override;
      procedure fset_CanvasPen   ( const _value : TGIS_Pen
                                 ) ; override;
      function  fget_CanvasBrush : TGIS_Brush ; override;
      procedure fset_CanvasBrush ( const _value : TGIS_Brush
                                 ) ; override;
      function  fget_CanvasFont  : TGIS_Font ; override;
      procedure fset_CanvasFont  ( const _value : TGIS_Font
                                 ) ; override;

      function  fget_Info        : String ; override;
    private
      /// <summary>
      ///   Get left-up corner of the original shape's projected extent.
      /// </summary>
      function  getShapeOrigin   : TPoint ;

      function  prepareBitmapFill( _bitmap : TBitmap
                                 ) : TBitmap ;

      /// <summary>
      ///   Prepare internal representation of pen.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas for which the pen is prepared
      /// </param>
      /// <param name="_color">
      ///   pen color
      /// </param>
      /// <param name="_style">
      ///   pen style
      /// </param>
      /// <param name="_bitmap">
      ///   pen bitmap
      /// </param>
      /// <param name="_pattern">
      ///   pen pattern
      /// </param>
      /// <param name="_origin">
      ///   origin for bitmaps and patterns
      /// </param>
      /// <param name="_cap">
      ///  line end cap style
      /// </param>
      /// <param name="_join">
      ///  line join style
      /// </param>
      /// <param name="_width">
      ///   pen width
      /// </param>
      procedure preparePen       ( const _canvas        : TGIS_CanvasInternal ;
                                   const _color         : TGIS_Color      ;
                                   const _style         : TGIS_PenStyle   ;
                                   const _bitmap        : TGIS_Bitmap     ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _origin        : TPoint          ;
                                   const _cap           : TGIS_LineCap    ;
                                   const _join          : TGIS_LineJoin   ;
                                   const _width         : Integer
                                 ) ;

      /// <summary>
      ///   Prepare internal representation of brush.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas for which the pen is prepared
      /// </param>
      /// <param name="_color">
      ///   brush color
      /// </param>
      /// <param name="_bitmap">
      ///   brush bitmap
      /// </param>
      /// <param name="_pattern">
      ///   brush pattern
      /// </param>
      /// <param name="_origin">
      ///   origin for bitmap brushes
      /// </param>
      procedure prepareBrush     ( const _canvas        : TGIS_CanvasInternal ;
                                   const _color         : TGIS_Color  ;
                                   const _bitmap        : TGIS_Bitmap ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _origin        : TPoint
                                 ) ;

      /// <summary>
      ///   Prepare internal representation of font.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas for which the pen is prepared
      /// </param>
      /// <param name="_name">
      ///   font name
      /// </param>
      /// <param name="_size">
      ///   font size
      /// </param>
      /// <param name="_style">
      ///   font style
      /// </param>
      /// <param name="_color">
      ///   font color
      /// </param>
      procedure prepareFont      ( const _canvas        : TGIS_CanvasInternal ;
                                   const _name          : String      ;
                                   const _size          : Integer     ;
                                   const _style         : TGIS_FontStyles ;
                                   const _color         : TGIS_Color
                                 ) ;

      /// <summary>
      ///   Draw a rectangle on the default canvas.
      /// </summary>
      /// <param name="_x1">
      ///   x coordinate of the upper-left corner of the rectangle
      /// </param>
      /// <param name="_y1">
      ///   y coordinate of the upper-left corner of the rectangle
      /// </param>
      /// <param name="_x2">
      ///   x coordinate of the bottom-right corner of the rectangle
      /// </param>
      /// <param name="_y2">
      ///   y coordinate of the bottom-right corner of the rectangle
      /// </param>
      procedure drawRectangle    ( const _x1            : Integer     ;
                                   const _y1            : Integer     ;
                                   const _x2            : Integer     ;
                                   const _y2            : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a rectangle on the given canvas.
      /// </summary>
      /// <param name="_x1">
      ///   x coordinate of the upper-left corner of the rectangle
      /// </param>
      /// <param name="_y1">
      ///   y coordinate of the upper-left corner of the rectangle
      /// </param>
      /// <param name="_x2">
      ///   x coordinate of the bottom-right corner of the rectangle
      /// </param>
      /// <param name="_y2">
      ///   y coordinate of the bottom-right corner of the rectangle
      /// </param>
      /// <param name="_canvas">
      ///   canvas to be drawn on
      /// </param>
      procedure drawRectangle    ( const _x1            : Integer     ;
                                   const _y1            : Integer     ;
                                   const _x2            : Integer     ;
                                   const _y2            : Integer     ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw an ellipse on the default canvas.
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
      procedure drawEllipse      ( const _x1            : Integer     ;
                                   const _y1            : Integer     ;
                                   const _x2            : Integer     ;
                                   const _y2            : Integer
                                 ) ; overload;

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
      procedure drawEllipse      ( const _x1            : Integer     ;
                                   const _y1            : Integer     ;
                                   const _x2            : Integer     ;
                                   const _y2            : Integer     ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      procedure drawArc          ( const _x1            : Integer ;
                                   const _y1            : Integer ;
                                   const _x2            : Integer ;
                                   const _y2            : Integer ;
                                   const _startX        : Integer ;
                                   const _startY        : Integer ;
                                   const _endX          : Integer ;
                                   const _endY          : Integer ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      procedure drawArc          ( const _x             : Integer ;
                                   const _y             : Integer ;
                                   const _radius        : Cardinal;
                                   const _startAngle    : Single ;
                                   const _sweepAngle    : Single ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw a polygon.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      procedure drawPolygon      ( const _points        : array of TPoint
                                 ) ;

      /// <summary>
      ///   Draw a polygon on the default canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_parts">
      ///   array of parts
      /// </param>
      /// <param name="_count">
      ///   number of parts
      /// </param>
      procedure drawPolyPolygon  ( const _points        : TGIS_DrawBuf      ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a polygon on a given canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_parts">
      ///   array of parts
      /// </param>
      /// <param name="_count">
      ///   number of parts
      /// </param>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      procedure drawPolyPolygon  ( const _points        : TGIS_DrawBuf      ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw a polygon on a given canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_parts">
      ///   array of parts
      /// </param>
      /// <param name="_count">
      ///   number of parts
      /// </param>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      procedure drawPolyPolygon  ( const _points        : TGIS_DrawBufF     ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw a line between two points on the given canvas.
      /// </summary>
      /// <param name="_x1">
      ///   x coordinate of the first point
      /// </param>
      /// <param name="_y1">
      ///   y coordinate of the first point
      /// </param>
      /// <param name="_x2">
      ///   x coordinate of the second point
      /// </param>
      /// <param name="_y2">
      ///   y coordinate of the second point
      /// </param>
      /// <param name="_canvas">
      ///   canvas to be drawn on
      /// </param>
      procedure drawLine         ( const _x1            : Integer     ;
                                   const _y1            : Integer     ;
                                   const _x2            : Integer     ;
                                   const _y2            : Integer     ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ;

      /// <summary>
      ///   Draw a line on the default canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_count">
      ///   number of points
      /// </param>
      procedure drawPolyline     ( const _points        : array of TPoint   ;
                                   const _count         : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a line on the given canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_count">
      ///   number of points
      /// </param>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      procedure drawPolyline     ( const _points        : array of TPoint   ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw a line on the default canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_parts">
      ///   array of parts
      /// </param>
      /// <param name="_count">
      ///   number of parts
      /// </param>
      procedure drawPolyPolyline ( const _points        : TGIS_DrawBuf      ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer
                                 ) ; overload;

      /// <summary>
      ///   Draw a line on a given canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_parts">
      ///   array of parts
      /// </param>
      /// <param name="_count">
      ///   number of parts
      /// </param>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      procedure drawPolyPolyline ( const _points        : TGIS_DrawBuf      ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw a line on a given canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_parts">
      ///   array of parts
      /// </param>
      /// <param name="_count">
      ///   number of parts
      /// </param>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      procedure drawPolyPolyline ( const _points        : TGIS_DrawBufF     ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      /// <summary>
      ///   Draw a string on a given canvas.
      /// </summary>
      /// <param name="_text">
      ///   text to draw
      /// </param>
      /// <param name="_x">
      ///   x origin
      /// </param>
      /// <param name="_y">
      ///   y origin
      /// </param>
      /// <param name="_canvas">
      ///   canvas to draw on
      /// </param>
      procedure drawText         ( const _text          : String      ;
                                   const _x             : Integer     ;
                                   const _y             : Integer     ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ;

      /// <summary>
      ///   Draw a marker.
      /// </summary>
      /// <param name="_style">
      ///   style of the marker
      /// </param>
      /// <param name="_size">
      ///   size of the marker
      /// </param>
      /// <param name="_pt">
      ///   central position of the marker
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True, only selection will be rendered
      /// </param>
      procedure drawMarker       ( const _style         : TGIS_MarkerStyle ;
                                   const _size          : Integer ;
                                   const _pt            : TPoint  ;
                                   const _selectionOnly : Boolean
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
      ///   if True only the shapeselection is rendered
      /// </param>
      /// <param name="_outlineMode">
      ///   outline drawing mode
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
      procedure doShapePolygon   ( const _shp           : TGIS_ShapePolygon ;
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
      ///   outline drawing mode
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

      procedure drawEditingEdgeLengths
                                 ( const _shp           : TGIS_Shape
                                 ) ;

      procedure drawEditingPoints( const _bitmap        : TBitmap     ;
                                   const _shp           : TGIS_Shape
                                 ) ;

    protected
      procedure doDestroy ; override ;

    public

      /// <inheritdoc/>
      constructor Create             ; override ;

      /// <summary>
      ///   Verifies if Direct2D rendering is supported on the current system.
      /// </summary>
      /// <returns>
      ///   True if Direct2D is supported
      /// </returns>
      class function  Supported      : Boolean ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Keeps D2DCanvas interface alive to avoid crashes on shutdown if
      ///   compiled as DLL.
      /// </summary>
      /// <remarks>
      ///   Only for internal use of TatukGIS.
      /// </remarks>
      procedure   KeepD2DCanvasAlive ;

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

      /// <summary>
      ///   Initiate context for the renderer( using D2D bitmaps only ).
      /// </summary>
      /// <param name="_parent">
      ///   viewer parented interface; for Control* like methods
      /// </param>
      /// <param name="_viewer">
      ///   viewer object
      /// </param>
      /// <param name="_context">
      ///   drawing context
      /// </param>
      /// <param name="_shift">
      ///   left and upper margins for the drawing surface
      /// </param>
      /// <param name="_width">
      ///   width of the drawing surface
      /// </param>
      /// <param name="_height">
      ///   height of the drawing surface
      /// </param>
      /// <param name="_ppi">
      ///   resolution for the drawing surface
      /// </param>
      /// <param name="_fontscale">
      ///   font scaling factor in percents
      /// </param>
      procedure CreateContextEx    ( const _parent    : IGIS_ViewerParent ;
                                     const _viewer    : IGIS_Viewer ;
                                     const _context   : TGIS_RendererContext ;
                                     const _shift     : TPoint  ;
                                     const _width     : Integer ;
                                     const _height    : Integer ;
                                     const _ppi       : Integer ;
                                     const _fontscale : Integer
                                   ) ;

      /// <inheritdoc/>
      procedure   RestoreContext     ; override;

      /// <summary>
      ///   Restore context using same data as used by CreateContext
      ///   ( using SharpDX bitmaps only ).
      /// </summary>
      procedure RestoreContextEx   ;

      /// <inheritdoc/>
      procedure   ReleaseContext     ; override;

      /// <inheritdoc/>
      procedure   PrepareHourglassContext ; override;

      /// <inheritdoc/>
      procedure   AfterDraw          ; override;

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
                                     ) ; override;

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
                                       const _antialias : Boolean
                                     ) ; override;

      /// <inheritdoc/>
      function    PrepareBitmapCache ( const _bmp      : TGIS_Pixels ;
                                       const _extent   : TGIS_Extent ;
                                       const _size     : TPoint      ;
                                       const _serial   : Integer     ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) : TGIS_RendererAbstractCache ;
                                     override;

      /// <inheritdoc/>
      procedure   RenderBitmapCache  ( const _handle   : TObject ;
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
                                     ) ; override;

      /// <inheritdoc from="IGIS_Viewer"/>
      procedure   Update             ; override;

      /// <inheritdoc/>
      procedure   Flush              ; override;

    // lo-level API
    public

      /// <inheritdoc/>
      /// <returns>
      ///   TDirect2DCanvas object.
      /// </returns>
      function    CanvasNative       : TObject ; override;

      /// <inheritdoc/>
      procedure   CanvasFontMetrics  ( var _break_char : Char    ;
                                       var _height     : Integer ;
                                       var _ascent     : Integer ;
                                       var _true_type  : Boolean
                                     ) ; override;

      /// <inheritdoc/>
      function    CanvasTextExtent   ( const _text     : String
                                     ) : TPoint ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawText     ( const _rect     : TRect   ;
                                       const _text     : String
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawLine     ( const _x1       : Integer ;
                                       const _y1       : Integer ;
                                       const _x2       : Integer ;
                                       const _y2       : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points   : TGIS_DrawBuf
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points   : TGIS_DrawBufF
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points   : TGIS_DrawBuf ;
                                       const _count    : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine ( const _points   : TGIS_DrawBufF ;
                                       const _count    : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine( const _points    : TGIS_DrawBuf ;
                                      const _parts     : TGIS_IntegerArray
                                    ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolyLine( const _points    : TGIS_DrawBufF ;
                                      const _parts     : TGIS_IntegerArray
                                    ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawRectangle( const _rect     : TRect
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawEllipse  ( const _x        : Integer ;
                                       const _y        : Integer ;
                                       const _width    : Integer ;
                                       const _height   : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPie      ( const _angle_0  : Double  ;
                                       const _angle_1  : Double  ;
                                       const _radius   : Integer ;
                                       const _origin_x : Integer ;
                                       const _origin_y : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure CanvasDrawArc       ( const _x          : Integer ;
                                      const _y          : Integer ;
                                      const _width      : Integer ;
                                      const _height     : Integer ;
                                      const _startX     : Integer ;
                                      const _startY     : Integer ;
                                      const _endX       : Integer ;
                                      const _endY       : Integer
                                    ) ; override;
      /// <inheritdoc/>
      procedure CanvasDrawArc       ( const _x          : Integer ;
                                      const _y          : Integer ;
                                      const _radius     : Integer ;
                                      const _startAngle : Single ;
                                      const _sweepAngle : Single
                                    ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBuf
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBufF
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBuf ;
                                       const _count    : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBufF ;
                                       const _count    : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBuf ;
                                       const _parts    : TGIS_IntegerArray
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBufF ;
                                       const _parts    : TGIS_IntegerArray
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
                                       const _angle    : Double  ;
                                       const _origin_x : Integer ;
                                       const _origin_y : Integer
                                     ) ; override;

      /// <inheritdoc/>
      procedure   CanvasClearTransformation ; override;
  end ;

  /// <summary>
  ///   Cached topmost pixel layer bitmap object to be used with
  ///   PrepareBitmapCache and RenderBitmapCache.
  /// </summary>
  TGIS_RendererVclDirect2DCache = class( TGIS_RendererAbstractCache )
    private
      oD2D1Bitmap : ID2D1Bitmap ;
    protected
      procedure doDestroy ; override ;
  end;

//##############################################################################
implementation

uses
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Generics.Collections,

  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.GeoHtmlLabel,
  Lider.CG.GIS.GeoArcLabel,
  Lider.CG.GIS.GeoChart,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.VCL.GeoFramework ;

const
  GIS_D2D1_PIXEL_SHIFT : Single = 0.5 ;

  // Maximum number of points for pline/polygon draws
  GDI_MAXPOINT_COUNT : Integer = 20000000 ;

var
  bD2DSupportCheck  : Integer = -1;

{$REGION 'T_Direct2DCanvasHack'}
type
  T_Direct2DCanvasHack = class( TCustomCanvas )
  //-- the following code MUST be the same as
  //-- in Vcl.Direct2D.TDirect2DCanvas
  //---- start --------------------------
  strict private
    class var
      FSupported: TUncertainState;
      {$IFDEF LEVEL_RX103_RTL}
      FDefaultDrawTextOption: Integer;
      {$ENDIF}
  public
    FPen: TDirect2DPen;
    FFont: TDirect2DFont;
    FBrush:  TDirect2DBrush;

    FD2DObjects: TObject;
    {$IFDEF LEVEL_RX103_RTL}
    FDrawTextOption: Integer;
    {$ENDIF}

    //Attached GDI Resources
    FDC: HDC;
    FHwnd: HWND;
    FSubRect: TRect;

    FRenderTarget: ID2D1RenderTarget;
  //---- end  -----------------------------
  end;
{$ENDREGION}

type
{$REGION 'T_Pen'}
  // Substitute of a pen object.
  T_Pen = class
    // properties internal values
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

      FPattern    : TGIS_BrushStyle ;

      FBitmap     : TGIS_Bitmap ;

      FOrigin     : TPoint ;

      // Set if any setting has changed.
      FChanged    : Boolean ;

    protected  // property access routines

      procedure fset_Color             ( const _value   : TGIS_Color
                                       ) ;
      procedure fset_Style             ( const _value   : TGIS_PenStyle
                                       ) ;
      procedure fset_LineCap           ( const _value   : TGIS_LineCap
                                       ) ;
      procedure fset_LineJoin          ( const _value   : TGIS_LineJoin
                                       ) ;
      procedure fset_Pattern           ( const _value   : TGIS_BrushStyle
                                       ) ;
      procedure fset_Bitmap            ( const _value   : TGIS_Bitmap
                                       ) ;
      procedure fset_Origin            ( const _origin  : TPoint
                                       ) ;

    public     // public methods

      // Create an instance.
      constructor Create             ;

      // Select the pen object according to earlier settings.
      procedure SelectPen            ( const _renderer : TGIS_RendererVclDirect2D ;
                                       const _canvas   : TGIS_CanvasInternal
                                     ) ;
    public     // public properties

      // Color of the pen.
      property Color                 : TGIS_Color
                                       read  FColor
                                       write fset_Color ;

      // Width of the pen in pixels.
      property Width                 : Integer
                                       read  FWidth
                                       write FWidth
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

      // Pattern for hatched pens.
      property Pattern               : TGIS_BrushStyle
                                       read  FPattern
                                       write fset_Pattern ;

     // External bitmap image that defines a pattern for the patterned pens.
      property Bitmap                : TGIS_Bitmap
                                       read  FBitmap
                                       write fset_Bitmap ;

      // Origin point used in the pattern brush.
      property Origin                : TPoint
                                       read  FOrigin
                                       write fset_Origin ;
  end ;
{$ENDREGION}

{$REGION 'T_Brush'}
  // Substitute of a brush object.
  T_Brush = class
    // properties internal values
    private

      // Color of the brush.
      FColor    : TGIS_Color ;

      // Pattern for the brush.
      FStyle    : TGIS_BrushStyle ;

      FBitmap   : TGIS_Bitmap ;

      FOrigin   : TPoint ;

      //Set if any setting has changed.
      FChanged  : Boolean ;

    protected   // property access routines

      procedure fset_Color           ( const _color       : TGIS_Color
                                     ) ;
      procedure fset_Style           ( const _style       : TGIS_BrushStyle
                                     ) ;
      procedure fset_Bitmap          ( const _bitmap      : TGIS_Bitmap
                                     ) ;
      procedure fset_Origin          ( const _origin      : TPoint
                                     ) ;
    public // public methods

      // Create an instance.
      constructor Create             ;

      // Select the brush object according to earlier settings.
      procedure SelectBrush          ( const _renderer : TGIS_RendererVclDirect2D ;
                                       const _canvas   : TGIS_CanvasInternal
                                     ) ;
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

      // Origin point used in the pattern brush.
      property Origin                : TPoint
                                       read  FOrigin
                                       write fset_Origin ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_CanvasInternal'}

constructor TGIS_CanvasInternal.Create(
  const _bmp : TBitmap
) ;
begin
  Canvas := _bmp.Canvas ;
  Canvas.Lock ;

  bmp_hnd := _bmp.Handle ;

  bmp_dc := CreateCompatibleDC( 0 ) ;
  bmp_old := SelectObject( bmp_dc, bmp_hnd ) ;

  d2dCanvas := TDirect2DCanvas.Create(
    bmp_dc, Rect( 0, 0, _bmp.Width, _bmp.Height )
  ) ;
  d2dCanvas.RenderTarget ;
  ownsCanvas := True ;

  Pen    := Lider.CG.GIS.GeoTypesUI.TGIS_Pen.Create ;
  Brush  := Lider.CG.GIS.GeoTypesUI.TGIS_Brush.Create ;
  Font   := Lider.CG.GIS.GeoTypesUI.TGIS_Font.Create ;

  D2DPen   := T_Pen.Create ;
  D2DBrush := T_Brush.Create ;

  d2dLockLevel := 0 ;
  BeginDraw ;

  transparency := 100  ;
  usePen       := True ;
  useBrush     := True ;
end ;

constructor TGIS_CanvasInternal.Create(
  const _target    : ID2D1RenderTarget
) ;

  procedure d2dCanvas_initialization ;
  begin
    T_Direct2DCanvasHack(d2dCanvas).FRenderTarget :=
      TD2DCanvas(d2dCanvas).CompatibleRenderTarget ;
    T_Direct2DCanvasHack(d2dCanvas).FD2DObjects :=
      TList<TDirect2DGraphicsObject>.Create();
    T_Direct2DCanvasHack(d2dCanvas).FPen   := TDirect2DPen.Create(d2dCanvas);
    T_Direct2DCanvasHack(d2dCanvas).FFont  := TDirect2DFont.Create(d2dCanvas);
    T_Direct2DCanvasHack(d2dCanvas).FBrush := TDirect2DBrush.Create(d2dCanvas);
    d2dCanvas.PenPos := Point(0,0);
    {$IFDEF LEVEL_RX103_RTL}
      d2dCanvas.DrawTextOption := D2D1_DRAW_TEXT_OPTIONS_NONE ;
    {$ENDIF}
  end;

begin
  Canvas := nil ;
  bmp_hnd := 0 ;
  bmp_dc := 0 ;

  d2dCanvas := TD2DCanvas.Create( _target ) ;
  d2dCanvas_initialization ;
  ownsCanvas := True ;

  Pen    := Lider.CG.GIS.GeoTypesUI.TGIS_Pen.Create ;
  Brush  := Lider.CG.GIS.GeoTypesUI.TGIS_Brush.Create ;
  Font   := Lider.CG.GIS.GeoTypesUI.TGIS_Font.Create ;

  D2DPen   := T_Pen.Create ;
  D2DBrush := T_Brush.Create ;

  d2dLockLevel := 0 ;
  BeginDraw ;

  transparency := 100  ;
  usePen       := True ;
  useBrush     := True ;
end ;

constructor TGIS_CanvasInternal.Create(
  const _canvas : TDirect2DCanvas
) ;
begin
  Canvas := nil ;
  bmp_hnd := 0 ;
  bmp_dc := 0 ;

  d2dCanvas := _canvas ;
  ownsCanvas := False ;

  Pen    := Lider.CG.GIS.GeoTypesUI.TGIS_Pen.Create ;
  Brush  := Lider.CG.GIS.GeoTypesUI.TGIS_Brush.Create ;
  Font   := Lider.CG.GIS.GeoTypesUI.TGIS_Font.Create ;

  D2DPen   := T_Pen.Create ;
  D2DBrush := T_Brush.Create ;

  d2dLockLevel := 0 ;

  transparency := 100  ;
  usePen       := True ;
  useBrush     := True ;
end ;

procedure TGIS_CanvasInternal.doDestroy ;
begin
  if Assigned( d2dCanvas ) then begin
    if ownsCanvas and
       ( d2dLockLevel > 0 ) then
      d2dCanvas.EndDraw ;
  end ;

  if assigned( Canvas ) then begin
    SelectObject( bmp_dc, bmp_old ) ;
    DeleteDC( bmp_dc ) ;
  end;

  if ownsCanvas then
    FreeObject( d2dCanvas ) ;

  FreeObject( D2DBrush ) ;
  FreeObject( D2DPen   ) ;

  FreeObject( Font  ) ;
  FreeObject( Brush ) ;
  FreeObject( Pen   ) ;

  if Assigned( Canvas ) then
    Canvas.Unlock ;

  inherited ;
end ;

function TGIS_CanvasInternal.isToBeDrawn : Boolean ;
begin
  Result := usePen and
            ( T_Pen(D2DPen).Style <> TGIS_PenStyle.Clear ) and
            ( T_Pen(D2DPen).Width <> 0 ) ;
end ;

function TGIS_CanvasInternal.isToBeFilled : Boolean ;
begin
  Result := useBrush and ( T_Brush(D2DBrush).Style <> TGIS_BrushStyle.Clear ) ;
end ;

{$IFNDEF LEVEL_XE6_VCL}
  // TDirect2DCanvas.CreateBitmap is buggy on version preior XE6
  function TGIS_CanvasInternal.createBitmapFix(
    const _bitmap : TBitmap
  ) : ID2D1Bitmap;
  var
    bi     : TBitmapInfo;
    buf    : array of Byte;
    d2prop : TD2D1BitmapProperties;
    hbmp   : HBitmap;
  begin
    FillChar(bi, SizeOf(bi), 0);
    bi.bmiHeader.biSize     := Sizeof(bi.bmiHeader);
    bi.bmiHeader.biHeight   := -_bitmap.Height;
    bi.bmiHeader.biWidth    :=  _bitmap.Width;
    bi.bmiHeader.biPlanes   := 1;
    bi.bmiHeader.biBitCount := 32;

    SetLength(buf, _bitmap.Height * _bitmap.Width * 4);
    hbmp := _bitmap.Handle;
    GetDIBits( _bitmap.Canvas.Handle, hbmp, 0, _bitmap.Height, @buf[0],
               bi,
               DIB_RGB_COLORS
              );

    d2prop.dpiX := 0;
    d2prop.dpiY := 0;
    d2prop.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
    if ( _bitmap.PixelFormat <> pf32bit   ) or
       ( _bitmap.AlphaFormat =  afIgnored )
    then
      d2prop.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE
    else
      d2prop.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

   d2dCanvas.RenderTarget.CreateBitmap(
     D2D1SizeU( _bitmap.Width, _bitmap.Height),
     @buf[0],
     4*_bitmap.Width,
     d2prop,
     Result
   )
  end;

  // TDirect2DCanvas.StretchDraw is buggy on version preior XE6
  procedure TGIS_CanvasInternal.stretchDrawFix(
    const _rect : TRect;
    const _grph : TGraphic
  ) ;
  var
    d2dbmp : ID2D1Bitmap ;
    d2drct : TD2DRectF   ;
    bmp    : TBitmap     ;
  begin
    bmp := TBitmap.Create;
    try
      bmp.Assign( _grph );

      d2dbmp := createBitmapFix( bmp );

      d2drct.Left   := _rect.Left   ;
      d2drct.Right  := _rect.Right  ;
      d2drct.Top    := _rect.Top    ;
      d2drct.Bottom := _rect.Bottom ;
      d2dCanvas.RenderTarget.DrawBitmap( d2dbmp, @d2drct, 1);
    finally
      FreeObject( bmp ) ;
    end;
  end;
{$ENDIF}

function TGIS_CanvasInternal.prepareBitmapBrush(
  const _bitmap : TBitmap ;
  const _d2dbp  : TD2D1BrushProperties
) : ID2D1BitmapBrush;
var
  d2dbbp  : TD2D1BitmapBrushProperties ;
  d2db    : ID2D1Bitmap ;

  {$IFNDEF LEVEL_XE6_VCL}
    // TDirect2DCanvas.CreateBitmap is buggy on version preior XE6
    function create_bitmap : ID2D1Bitmap;
    var
      bi     : TBitmapInfo;
      buf    : array of Byte;
      d2prop : TD2D1BitmapProperties;
      hbmp  : HBitmap;
    begin
      FillChar(bi, SizeOf(bi), 0);
      bi.bmiHeader.biSize := Sizeof(bi.bmiHeader);
      bi.bmiHeader.biHeight := -_bitmap.Height;
      bi.bmiHeader.biWidth := _bitmap.Width;
      bi.bmiHeader.biPlanes := 1;
      bi.bmiHeader.biBitCount := 32;

      SetLength(buf, _bitmap.Height * _bitmap.Width * 4);

      _bitmap.Canvas.Lock ;
      hbmp := _bitmap.Handle;
      GetDIBits( _bitmap.Canvas.Handle, hbmp, 0, _bitmap.Height, @buf[0],
                 bi,
                 DIB_RGB_COLORS
                );
      _bitmap.Canvas.Unlock ;

      d2prop.dpiX := 0;
      d2prop.dpiY := 0;
      d2prop.pixelFormat.format := DXGI_FORMAT_B8G8R8A8_UNORM;
      if ( _Bitmap.PixelFormat <> pf32bit   ) or
         ( _Bitmap.AlphaFormat =  afIgnored )
      then
        d2prop.pixelFormat.alphaMode := D2D1_ALPHA_MODE_IGNORE
      else
        d2prop.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;

     d2dCanvas.RenderTarget.CreateBitmap(
       D2D1SizeU( _bitmap.Width, _bitmap.Height),
       @buf[0],
       4*_bitmap.Width,
       d2prop,
       Result
     )
    end;
  {$ENDIF}

begin
  d2dbbp.extendModeX       := D2D1_EXTEND_MODE_WRAP ;
  d2dbbp.extendModeY       := D2D1_EXTEND_MODE_WRAP ;
  d2dbbp.interpolationMode := D2D1_BITMAP_INTERPOLATION_MODE_NEAREST_NEIGHBOR ;

  {$IFNDEF LEVEL_XE6_VCL}
    d2db := create_bitmap ;
  {$ELSE}
    d2db := d2dCanvas.CreateBitmap( _bitmap ) ;
  {$ENDIF}

  d2dCanvas.RenderTarget.CreateBitmapBrush(
    d2db,
    @d2dbbp,
    @_d2dbp,
    Result
  ) ;
end;

function TGIS_CanvasInternal.prepareSolidColorBrush(
  const _color : TGIS_Color;
  const _d2dbp : TD2D1BrushProperties
) : ID2D1SolidColorBrush;
var
  d2dc : TD2D1ColorF ;
begin
  d2dc.r := _color.R/255 ;
  d2dc.g := _color.G/255 ;
  d2dc.b := _color.B/255 ;
  d2dc.a := _color.A/255 ;

  d2dCanvas.RenderTarget.CreateSolidColorBrush(
    d2dc,
    @_d2dbp,
    Result
  ) ;
end;

procedure TGIS_CanvasInternal.BeginDraw ;
begin
  Inc( d2dLockLevel ) ;
  if assigned( d2dCanvas ) then begin
    d2dCanvas.RenderTarget ;
    if d2dCanvas is TD2DCanvas then
      TD2DCanvas( d2dCanvas ).BeginDraw
    else
      d2dCanvas.BeginDraw ;
  end ;
end ;

procedure TGIS_CanvasInternal.EndDraw ;
begin
  if assigned( d2dCanvas ) then
    d2dCanvas.EndDraw ;
  Dec( d2dLockLevel ) ;
end ;

{$ENDREGION}

{$REGION 'T_Pen'}

  constructor T_Pen.Create ;
  begin
    inherited ;

    FColor    := TGIS_Color.Black ;
    FWidth    := 1 ;
    FStyle    := TGIS_PenStyle.Solid ;
    FLineCap  := TGIS_LineCap.Round  ;
    FLineJoin := TGIS_LineJoin.Round ;
    FPattern  := TGIS_BrushStyle.Solid ;
    FBitmap   := nil ;
    FOrigin   := Point( 0, 0 ) ;
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

  procedure T_Pen.fset_Origin(
    const _origin : TPoint
  ) ;
  begin
    if ( FOrigin.X <> _origin.X ) or
       ( FOrigin.Y <> _origin.Y ) then begin
      FOrigin := _origin ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Pen.SelectPen(
    const _renderer : TGIS_RendererVclDirect2D ;
    const _canvas   : TGIS_CanvasInternal
  ) ;
    procedure create_pen ;
    var
      d2dssp : TD2D1StrokeStyleProperties ;
      d2dbp  : TD2D1BrushProperties ;
      b      : Boolean ;
      bmp    : TBitmap ;
      bb     : TBitmap ;
    begin
      d2dssp.miterLimit := 10 ;
      d2dssp.dashOffset := 0  ;

      case FLineCap of
        TGIS_LineCap.Flat   :
          begin
            d2dssp.startCap := D2D1_CAP_STYLE_FLAT ;
            d2dssp.endCap   := D2D1_CAP_STYLE_FLAT ;
          end ;
        TGIS_LineCap.Square :
          begin
            d2dssp.startCap := D2D1_CAP_STYLE_SQUARE ;
            d2dssp.endCap   := D2D1_CAP_STYLE_SQUARE ;
          end ;
        TGIS_LineCap.Round  :
          begin
            d2dssp.startCap := D2D1_CAP_STYLE_ROUND ;
            d2dssp.endCap   := D2D1_CAP_STYLE_ROUND ;
          end ;
      end ;
      d2dssp.dashCap := D2D1_CAP_STYLE_SQUARE ;

      case FLineJoin of
        // alternative: D2D1_LINE_JOIN_MITER_OR_BEVEL
        TGIS_LineJoin.Bevel :
          d2dssp.lineJoin := D2D1_LINE_JOIN_BEVEL ;
        TGIS_LineJoin.Miter :
          d2dssp.lineJoin := D2D1_LINE_JOIN_MITER ;
        TGIS_LineJoin.Round :
          d2dssp.lineJoin := D2D1_LINE_JOIN_ROUND ;
      end ;

      case FStyle of
        TGIS_PenStyle.Solid      :
          d2dssp.dashStyle := D2D1_DASH_STYLE_SOLID ;
        TGIS_PenStyle.Dash       :
          d2dssp.dashStyle := D2D1_DASH_STYLE_DASH ;
        TGIS_PenStyle.Dot        :
          d2dssp.dashStyle := D2D1_DASH_STYLE_DOT ;
        TGIS_PenStyle.DashDot    :
          d2dssp.dashStyle := D2D1_DASH_STYLE_DASH_DOT ;
        TGIS_PenStyle.DashDotDot :
          d2dssp.dashStyle := D2D1_DASH_STYLE_DASH_DOT_DOT ;
        // workaround - there's no D2D1_DASH_STYLE_CLEAR
        TGIS_PenStyle.Clear      : ;
      end ;

      D2DFactory.CreateStrokeStyle(
        d2dssp,
        nil,
        0,
        _canvas.d2dPenStyle
      ) ;

      d2dbp.opacity := 1.0 ;

      if assigned( FBitmap ) then begin
        d2dbp.transform := TD2D1Matrix3x2F.Translation( FOrigin.X, FOrigin.Y ) ;
        bb := _renderer.prepareBitmapFill( TBitmap( FBitmap.NativeBitmap ) ) ;
        _canvas.d2dPenBrush := _canvas.prepareBitmapBrush( bb, d2dbp ) ;
        if bb <> TBitmap( FBitmap.NativeBitmap ) then
          FreeObject( bb ) ;
      end
      else begin
        b := False ;
        case FPattern of
          TGIS_BrushStyle.Solid      : ;
          TGIS_BrushStyle.Clear      : ;
          TGIS_BrushStyle.Horizontal ,
          TGIS_BrushStyle.Vertical   ,
          TGIS_BrushStyle.FDiagonal  ,
          TGIS_BrushStyle.BDiagonal  ,
          TGIS_BrushStyle.Cross      ,
          TGIS_BrushStyle.DiagCross  :
            begin
              b := True ;
            end ;
        end ;

        if b then begin
          d2dbp.transform := TD2D1Matrix3x2F.Translation( FOrigin.X, FOrigin.Y ) ;
          bmp := _renderer.brushCache.GetBitmap( FPattern, FColor ) ;
          bb := _renderer.prepareBitmapFill( bmp ) ;
          _canvas.d2dPenBrush := _canvas.prepareBitmapBrush( bb, d2dbp ) ;
          if bmp <> bb then
            FreeObject( bb ) ;
        end
        else begin
          d2dbp.transform := TD2D1Matrix3x2F.Identity ;
          _canvas.d2dPenBrush := _canvas.prepareSolidColorBrush( FColor, d2dbp ) ;
        end ;

      end ;
    end ;

  begin
    if FChanged or not assigned( _canvas.d2dPenBrush ) then begin
      create_pen ;
      FChanged := False ;
    end ;
  end ;
{$ENDREGION}

{$REGION 'T_Brush'}

  constructor T_Brush.Create ;
  begin
    inherited ;

    FColor   := TGIS_Color.White ;
    FStyle   := TGIS_BrushStyle.Solid ;
    FBitmap  := nil ;
    FOrigin  := Point( 0, 0 ) ;
    FChanged := false ;
  end ;

  procedure T_Brush.fset_Color(
    const _color : TGIS_Color
  ) ;
  begin
    if FColor <> _color then begin
      FColor   := _color ;
      FChanged := true ;
    end ;
  end ;

  procedure T_Brush.fset_Style(
    const _style : TGIS_BrushStyle
  ) ;
  begin
    if FStyle <> _style then begin
      FStyle := _style ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Brush.fset_Bitmap(
    const _bitmap : TGIS_Bitmap
  ) ;
  begin
    if ( FBitmap <> _bitmap ) or
       ( assigned( FBitmap ) and
         not FBitmap.Equals( _bitmap )
       ) then
    begin
      FBitmap := _bitmap ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Brush.fset_Origin(
    const _origin : TPoint
  ) ;
  begin
    if ( FOrigin.X <> _origin.X ) or
       ( FOrigin.Y <> _origin.Y ) then begin
      FOrigin := _origin ;
      FChanged := True ;
    end ;
  end ;

  procedure T_Brush.SelectBrush(
    const _renderer : TGIS_RendererVclDirect2D ;
    const _canvas   : TGIS_CanvasInternal
  ) ;
    procedure create_brush ;
    var
      d2dbp  : TD2D1BrushProperties ;
      b   : Boolean ;
      bmp : TBitmap ;
      bb  : TBitmap ;
    begin
      d2dbp.opacity := 1.0 ;
      if assigned( FBitmap ) then begin
        assert( assigned( FBitmap.NativeBitmap ) ) ;
        d2dbp.transform := TD2D1Matrix3x2F.Translation( FOrigin.X, FOrigin.Y ) ;
        bb := _renderer.prepareBitmapFill( TBitmap( FBitmap.NativeBitmap ) ) ;
        _canvas.d2dBrushBrush := _canvas.prepareBitmapBrush( bb, d2dbp ) ;
        if bb <> TBitmap( FBitmap.NativeBitmap ) then
          FreeObject( bb ) ;
      end
      else
      begin
        b := False ;
        case FStyle of
          TGIS_BrushStyle.Solid      : ;
          TGIS_BrushStyle.Clear      : ;
          TGIS_BrushStyle.Horizontal ,
          TGIS_BrushStyle.Vertical   ,
          TGIS_BrushStyle.FDiagonal  ,
          TGIS_BrushStyle.BDiagonal  ,
          TGIS_BrushStyle.Cross      ,
          TGIS_BrushStyle.DiagCross  :
            begin
              b := True ;
            end ;
        end ;

        if b then begin
          d2dbp.transform := TD2D1Matrix3x2F.Translation( FOrigin.X, FOrigin.Y ) ;
          bmp := _renderer.brushCache.GetBitmap( FStyle, FColor ) ;
          bb := _renderer.prepareBitmapFill( bmp ) ;
          _canvas.d2dBrushBrush := _canvas.prepareBitmapBrush( bb, d2dbp ) ;
          if bb <> bmp then
            FreeObject( bb ) ;
        end
        else begin
          d2dbp.transform := TD2D1Matrix3x2F.Identity ;
          _canvas.d2dBrushBrush := _canvas.prepareSolidColorBrush( FColor, d2dbp ) ;
        end ;
      end;
    end ;

  begin
    if FChanged or not assigned( _canvas.d2dBrushBrush ) then begin
      create_brush ;
      FChanged := False ;
    end ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_RendererVclDirect2D'}

constructor TGIS_RendererVclDirect2D.Create ;
begin
  inherited ;

  brushCache := TGIS_brushCache.Create ;
  flashed := False ;
  pextra  := False ;
end ;

procedure TGIS_RendererVclDirect2D.doDestroy ;
begin
  FreeObject( brushCache ) ;

  FreeObject( oTransparentCanvas ) ;
  FreeObject( oTransparentBitmap ) ;

  inherited ;
end ;

procedure TGIS_RendererVclDirect2D.prepareSelectionCanvas(
  _transparently : Boolean ;
  _useBaseMap    : Boolean
) ;
var
  transp_sel : Integer ;
begin
  if Assigned( oSelectionCanvas ) then exit ;

  if Assigned( Viewer ) then begin
    colorSelection := Viewer.SelectionGisColor ;
    if _transparently then
      transp_sel := Viewer.SelectionTransparency
    else
      transp_sel := 100 ;
  end
  else begin
    colorSelection := TGIS_Color.Red ;
    transp_sel := 60 ;
  end ;

  if not _useBaseMap and Context.SelectionOnDemand then
    Context.AssignSelection(
      createTransparentBitmap( Width, Height ),
      True
    ) ;

  if not _useBaseMap and assigned( Context.Selection ) then begin
    oSelectionCanvas := TGIS_CanvasInternal.Create(
                          TBitmap( Context.Selection )
                        ) ;
  end
  else begin
    oSelectionCanvas := oCanvas ;
    colorSelection := TGIS_Color.FromARGB(
                      colorSelection.A * transp_sel div 100,
                      colorSelection.R,
                      colorSelection.G,
                      colorSelection.B
                     ) ;
  end;
end;

procedure TGIS_RendererVclDirect2D.prepareChartsCanvas ;
begin
  if Assigned( oChartsCanvas ) then exit ;

  if Context.ChartsOnDemand then
    Context.AssignCharts(
      createTransparentBitmap( Width, Height ),
      True
    ) ;

  if assigned( Context.Charts ) then begin
    oChartsCanvas  := TGIS_CanvasInternal.Create(
                        TBitmap( Context.Charts )
                      ) ;
  end
  else begin
    oChartsCanvas := oCanvas ;
  end;
end;

procedure TGIS_RendererVclDirect2D.prepareLabelsCanvas ;
begin
  if Assigned( oLabelsCanvas ) then exit ;

  if Context.LabelsOnDemand then
    Context.AssignLabels(
      createTransparentBitmap( Width, Height ),
      True
    ) ;

  if assigned( Context.Labels ) then begin
    oLabelsCanvas  := TGIS_CanvasInternal.Create(
                        TBitmap( Context.Labels )
                      ) ;
  end
  else begin
    oLabelsCanvas := oCanvas ;
  end;
end;

function TGIS_RendererVclDirect2D.fget_CanvasPen
 : Lider.CG.GIS.GeoTypesUI.TGIS_Pen ;
begin
  assert( assigned( FCanvas ) ) ;
  Result := FCanvas.Pen ;
end ;

procedure TGIS_RendererVclDirect2D.fset_CanvasPen(
  const _value : Lider.CG.GIS.GeoTypesUI.TGIS_Pen
) ;
begin
  assert( assigned( FCanvas ) ) ;
  FCanvas.Pen := _value ;
end ;

function TGIS_RendererVclDirect2D.fget_CanvasBrush
  : Lider.CG.GIS.GeoTypesUI.TGIS_Brush ;
begin
  assert( assigned( FCanvas ) ) ;
  Result := FCanvas.Brush ;
end ;

procedure TGIS_RendererVclDirect2D.fset_CanvasBrush(
  const _value : Lider.CG.GIS.GeoTypesUI.TGIS_Brush
) ;
begin
  assert( assigned( FCanvas ) ) ;
  FCanvas.Brush := _value ;
end ;

function  TGIS_RendererVclDirect2D.fget_CanvasFont
  : Lider.CG.GIS.GeoTypesUI.TGIS_Font ;
begin
  assert( assigned( FCanvas ) ) ;
  Result := FCanvas.Font ;
end ;

procedure TGIS_RendererVclDirect2D.fset_CanvasFont(
  const _value : Lider.CG.GIS.GeoTypesUI.TGIS_Font
) ;
begin
  assert( assigned( FCanvas ) ) ;
  FCanvas.Font := _value ;
end ;

function TGIS_RendererVclDirect2D.fget_Info
  : String ;
begin
  Result := '[' + ClassName + ':TDirect2DCanvas]' ;
end;

procedure TGIS_RendererVclDirect2D.KeepD2DCanvasAlive ;
var
  bmp : TBitmap ;
  cnv : TDirect2DCanvas ;
begin
  if TOSVersion.Major < 6 then exit ;
  if not ModuleIsLib then exit ;

  try
    bmp := TBitmap.Create ;
    try
      bmp.Canvas.Lock ;
      bmp.PixelFormat := pf32bit ;
      bmp.SetSize( 32, 32 ) ;
      cnv := TDirect2DCanvas.Create( bmp.Canvas, bmp.Canvas.ClipRect ) ;
      cnv.Supported ;
    finally
      FreeObject( cnv ) ;
      FreeObject( bmp ) ;
    end ;
  except

  end ;
end ;

class function TGIS_RendererVclDirect2D.Supported : Boolean ;
var
  bmp : TBitmap ;
  cnv : TDirect2DCanvas ;
begin
  Result := False ;
  if (TOSVersion.Major < 6) then exit ;

  if bD2DSupportCheck <> 0 then begin
    Result := bD2DSupportCheck = 1 ;
    exit ;
  end ;

  try
    bmp := TBitmap.Create ;
    try
      bmp.Canvas.Lock ;
      bmp.PixelFormat := pf32bit ;
      bmp.SetSize( 32, 32 ) ;
      cnv := TDirect2DCanvas.Create( bmp.Canvas, bmp.Canvas.ClipRect ) ;
      try
        Result := cnv.Supported ;
      finally
        FreeObject( cnv ) ;
      end;
    finally
      FreeObject( bmp ) ;
    end ;
    if Result then
      bD2DSupportCheck := 1
    else
      bD2DSupportCheck := -1 ;
  except

  end ;
end ;

function TGIS_RendererVclDirect2D.CreateInstance
  : TGIS_RendererAbstract ;
begin
  Result := TGIS_RendererVclDirect2D.Create ;
end ;

function TGIS_RendererVclDirect2D.getShapeOrigin : TPoint ;
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

    if ( drct_left > -1073741824 ) and  // avoid range errors
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

function TGIS_RendererVclDirect2D.prepareBitmapFill(
  _bitmap : TBitmap
) : TBitmap ;
var
  bmp : TBitmap ;
begin
  if ( not ignorePPI ) and ( PPI <> 96 ) then begin
    bmp := Vcl.Graphics.TBitmap(
             createTransparentBitmap( RoundS(_bitmap.Width * PPI / 96),
                                      RoundS(_bitmap.Height * PPI / 96) )
           ) ;
    bmp.Canvas.Lock ;
    bmp.AlphaFormat := TAlphaFormat.afPremultiplied ;
    bmp.Canvas.StretchDraw( Rect(0,0,bmp.Width,bmp.Height), _bitmap ) ;
    Result := bmp ;
  end else
    Result := _bitmap ;
end;

procedure TGIS_RendererVclDirect2D.preparePen(
  const _canvas  : TGIS_CanvasInternal ;
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
  T_Pen(_canvas.D2DPen).LineCap := _cap ;
  T_Pen(_canvas.D2DPen).LineJoin := _join ;
  T_Pen(_canvas.D2DPen).Style := _style ;
  if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then begin
    T_Pen(_canvas.D2DPen).Bitmap := _bitmap ;
    T_Pen(_canvas.D2DPen).Pattern := TGIS_BrushStyle.Solid ;
    T_Pen(_canvas.D2DPen).Origin := _origin ;
  end
  else begin
    T_Pen(_canvas.D2DPen).Bitmap := nil ;
    T_Pen(_canvas.D2DPen).Pattern := _pattern ;
    if ( _pattern = TGIS_BrushStyle.Clear ) or
       ( _pattern = TGIS_BrushStyle.Solid ) then
      T_Pen(_canvas.D2DPen).Origin := Point( 0, 0 )
    else
      T_Pen(_canvas.D2DPen).Origin := _origin ;
  end ;
  if _color.ARGB = _color.RenderColor.ARGB then
    T_Pen(_canvas.D2DPen).Color := TGIS_Color.None
  else
    T_Pen(_canvas.D2DPen).Color := _color ;
  T_Pen(_canvas.D2DPen).Width := _width ;
end ;

procedure TGIS_RendererVclDirect2D.prepareBrush(
  const _canvas  : TGIS_CanvasInternal ;
  const _color   : TGIS_Color  ;
  const _bitmap  : TGIS_Bitmap ;
  const _pattern : TGIS_BrushStyle ;
  const _origin  : TPoint
) ;
begin
  if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then begin
    T_Brush(_canvas.D2DBrush).Bitmap := _bitmap ;
    T_Brush(_canvas.D2DBrush).Style  := TGIS_BrushStyle.Solid ;
    T_Brush(_canvas.D2DBrush).Origin := _origin ;
  end
  else begin
    T_Brush(_canvas.D2DBrush).Bitmap := nil ;
    T_Brush(_canvas.D2DBrush).Style  := _pattern ;
    if ( _pattern = TGIS_BrushStyle.Solid ) or
       ( _pattern = TGIS_BrushStyle.Clear) then
      T_Brush(_canvas.D2DBrush).Origin := Point( 0, 0 )
    else
      T_Brush(_canvas.D2DBrush).Origin := _origin ;
  end ;
  if _color.ARGB = _color.RenderColor.ARGB then
    T_Brush(_canvas.D2DBrush).Color := TGIS_Color.None
  else
    T_Brush(_canvas.D2DBrush).Color := _color ;
end ;

procedure TGIS_RendererVclDirect2D.prepareFont(
  const _canvas : TGIS_CanvasInternal ;
  const _name   : String  ;
  const _size   : Integer ;
  const _style  : TGIS_FontStyles ;
  const _color  : TGIS_Color
) ;
begin
  _canvas.d2dCanvas.Font.Name := _name ;
  _canvas.d2dCanvas.Font.Size := Max(
                            1,
                            RoundS(
                              _size *
                              PPI/_canvas.d2dCanvas.Font.PixelsPerInch
                              * FontScale / 100
                            )
                          ) ;
  _canvas.d2dCanvas.Font.Style := VCLFontStyle( _style ) ;
  _canvas.d2dCanvas.Font.Color := VCLColor( _color ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawRectangle(
  const _x1 : Integer ;
  const _y1 : Integer ;
  const _x2 : Integer ;
  const _y2 : Integer
) ;
begin
  drawRectangle( _x1, _y1, _x2, _y2, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawRectangle(
  const _x1     : Integer ;
  const _y1     : Integer ;
  const _x2     : Integer ;
  const _y2     : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  rect : TD2D1RectF ;
  pt1 : TD2DPoint2f ;
  pt2 : TD2DPoint2f ;
begin
  rect.left   := _x1 + GIS_D2D1_PIXEL_SHIFT ;
  rect.top    := _y1 + GIS_D2D1_PIXEL_SHIFT ;
  rect.right  := _x2 + GIS_D2D1_PIXEL_SHIFT ;
  rect.bottom := _y2 + GIS_D2D1_PIXEL_SHIFT ;

  if ( _x2 - _x1 < 1.0 ) and ( _y2 - _y1 < 1.0 ) then begin
    rect.right := rect.left + 1.0 ;
    rect.bottom := rect.top + 1.0 ;
  end;

  if _canvas.isToBeFilled then begin
    T_Brush(_canvas.D2DBrush).SelectBrush( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.FillRectangle(
      rect,
      _canvas.d2dBrushBrush
    ) ;
  end ;

  if _canvas.isToBeDrawn then begin
    T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
    if ( _x2 - _x1 < 1.0 ) and ( _y2 - _y1 < 1.0 ) then begin
      _canvas.d2dCanvas.RenderTarget.DrawRectangle(
        rect,
        _canvas.d2dPenBrush,
        T_Pen(_canvas.D2DPen).Width,
        _canvas.d2dPenStyle
      ) ;
    end else if ( _x2 - _x1 < 1.0 ) or ( _y2 - _y1 < 1.0 ) then begin
      pt1.x := rect.left ;
      pt1.y := rect.top ;
      pt2.x := rect.right ;
      pt2.y := rect.bottom ;
      _canvas.d2dCanvas.RenderTarget.DrawLine(
        pt1, pt2,
        _canvas.d2dPenBrush,
        T_Pen(_canvas.D2DPen).Width,
        _canvas.d2dPenStyle
      ) ;
    end else
      _canvas.d2dCanvas.RenderTarget.DrawRectangle(
        rect,
        _canvas.d2dPenBrush,
        T_Pen(_canvas.D2DPen).Width,
        _canvas.d2dPenStyle
      ) ;

  end ;
end ;

procedure TGIS_RendererVclDirect2D.drawEllipse(
  const _x1 : Integer ;
  const _y1 : Integer ;
  const _x2 : Integer ;
  const _y2 : Integer
) ;
begin
  drawEllipse( _x1, _y1, _x2, _y2, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawEllipse(
  const _x1     : Integer ;
  const _y1     : Integer ;
  const _x2     : Integer ;
  const _y2     : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  pt   : TD2D1Point2F ;
  elli : TD2D1Ellipse ;
begin
  pt.x := ( _x1 + _x2 )/2 + GIS_D2D1_PIXEL_SHIFT ;
  pt.y := ( _y1 + _y2 )/2 + GIS_D2D1_PIXEL_SHIFT ;
  elli.point := pt ;
  elli.radiusX := _x2 - pt.x ;
  elli.radiusY := _y2 - pt.y ;

  if elli.radiusX < 1.0 then
    elli.radiusX := 1.0 ;
  if elli.radiusY < 1.0 then
    elli.radiusY := 1.0 ;

  if _canvas.isToBeFilled then begin
    T_Brush(_canvas.D2DBrush).SelectBrush( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.FillEllipse(
      elli,
      _canvas.d2dBrushBrush
    ) ;
  end ;

  if _canvas.isToBeDrawn then begin
    T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.DrawEllipse(
      elli,
      _canvas.d2dPenBrush,
      T_Pen(_canvas.D2DPen).Width,
      _canvas.d2dPenStyle
    ) ;
  end ;
end ;

//Helper function for Arc, Pie, Chord
function PointsToArc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
  out Center, A, B: TD2D1Point2F): TD2D1ArcSegment;
var
  TL, BR: TD2D1Point2F;
  A1, A2: Single;
  DAngle: Single;
  AA, BB: Single;
  Slope : Single;
  SavedExceptionMask: TArithmeticExceptionMask;
begin
  SavedExceptionMask := GetExceptionMask;
  SetExceptionMask(exAllArithmeticExceptions);
  try
    TL.x := Min(X1,x2);
    TL.y := Min(Y1,Y2);
    BR.x := Max(X1, X2);
    BR.y := Max(Y1, Y2);
    Center := D2D1PointF((BR.X +TL.X)/2, (BR.Y + TL.Y)/2);

    A1 := ArcTan2(Center.Y - Y3 - 0.5, Center.X - X3 - 0.5);
    A2 := ArcTan2(Center.Y - Y4 - 0.5, Center.X - X4 - 0.5);
    A1 := Pi - A1;
    A2 := Pi - A2;
    DAngle := A2-A1;
    if (DAngle < 0) and (-DAngle > 0.001) then
      DAngle := DAngle + 2 * PI;
    result.rotationAngle := 0;
    if DAngle > PI then
      result.arcSize := D2D1_ARC_SIZE_LARGE
    else
      result.arcSize := D2D1_ARC_SIZE_SMALL;
    result.sweepDirection := D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE;
    Result.size := D2D1SizeF((BR.X-TL.X-1)/2, (BR.Y - TL.Y-1)/2);

    AA := Result.size.width * Result.size.width;
    BB := Result.size.height * Result.size.height;
    Slope := ((Y3-center.y)*(Y3-center.y))/
             ((X3-center.x)*(X3-center.x));
    A.x := sqrt(AA*BB/(BB+AA*Slope));
    A.y := sqrt(BB*(1-A.x*A.x/AA));

    if (A1 < Pi/2) or (A1 > 3*PI/2) then
      A.x := center.x + A.x
    else
      A.x := center.x - A.x;

    if A1 > PI then
      A.y := center.y + A.y
    else
      A.y := center.y - A.y;
    Slope := ((Y4-center.y)*(Y4-center.y))/
             ((X4-center.x)*(X4-center.x));
    b.x := sqrt(AA*BB/(BB+AA*Slope));
    B.y := sqrt(BB*(1-B.x*B.x/AA));

    if (A2 < Pi/2) or (A2 > 3*PI/2) then
      B.x := center.x + b.x
    else
      B.x := center.x - B.x;

    if A2 > PI then
      B.y := center.y + B.y
    else
      B.y := center.y - B.y;

    result.point := B;
  finally
    SetExceptionMask(SavedExceptionMask);
  end;
end;

procedure TGIS_RendererVclDirect2D.drawArc(
  const _x1            : Integer ;
  const _y1            : Integer ;
  const _x2            : Integer ;
  const _y2            : Integer ;
  const _startX        : Integer ;
  const _startY        : Integer ;
  const _endX          : Integer ;
  const _endY          : Integer ;
  const _canvas        : TGIS_CanvasInternal
);
var
  A,B, Center : TD2D1Point2F ;
  Geometry    : ID2D1PathGeometry ;
  Sink        : ID2D1GeometrySink ;
  ArcSegment  : TD2D1ArcSegment ;
begin
  ArcSegment := PointsToArc(_X1,_Y1, _X2,_Y2, _startX, _startY, _endX, _endY, Center, A, B) ;

  D2DFactory.CreatePathGeometry(Geometry) ;
  Geometry.Open(Sink) ;
  try
    Sink.BeginFigure(A, D2D1_FIGURE_BEGIN_FILLED) ;
    try
      Sink.AddArc(ArcSegment) ;
    finally
      Sink.EndFigure(D2D1_FIGURE_END_OPEN) ;
    end ;
  finally
    Sink.Close ;
  end ;

  if _canvas.isToBeFilled then
    T_Brush(_canvas.D2DBrush).SelectBrush( Self, _canvas ) ;

  if _canvas.isToBeDrawn then begin
    T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.DrawGeometry(
      Geometry,
      FCanvas.d2dPenBrush,
      T_Pen(FCanvas.D2DPen).Width,
      FCanvas.d2dPenStyle
    ) ;
  end ;
end;

procedure TGIS_RendererVclDirect2D.drawArc(
  const _x             : Integer ;
  const _y             : Integer ;
  const _radius        : Cardinal;
  const _startAngle    : Single ;
  const _sweepAngle    : Single ;
  const _canvas        : TGIS_CanvasInternal
) ;
var
  A,B         : TD2D1Point2F ;
  Geometry    : ID2D1PathGeometry ;
  Sink        : ID2D1GeometrySink ;
  ArcSegment  : TD2D1ArcSegment ;
  LSin, LCos  : Single ;
begin
  ArcSegment.size := D2D1SizeF(_radius, _radius) ;
  ArcSegment.rotationAngle := _sweepAngle ;

  if Abs(_sweepAngle) > 180 then
    ArcSegment.arcSize := D2D1_ARC_SIZE_LARGE
  else
    ArcSegment.arcSize := D2D1_ARC_SIZE_SMALL ;

  SinCos(_startAngle * PI / 180, LSin, LCos) ;
  A := D2D1PointF(LCos * _radius + _x, _y - LSin * _radius) ;
  SinCos((_startAngle + _sweepAngle) * PI / 180, LSin, LCos) ;
  B := D2D1PointF(LCos * _radius + _x, _y - LSin * _radius) ;
  ArcSegment.point := B ;

  if _sweepAngle > 0 then
    ArcSegment.sweepDirection := D2D1_SWEEP_DIRECTION_COUNTER_CLOCKWISE
  else
    ArcSegment.sweepDirection := D2D1_SWEEP_DIRECTION_CLOCKWISE ;

  if _canvas.isToBeFilled then begin
    T_Brush(_canvas.D2DBrush).SelectBrush( Self, _canvas ) ;
  end ;

  D2DFactory.CreatePathGeometry(Geometry) ;
  Geometry.Open(Sink) ;
  try
    Sink.BeginFigure(A, D2D1_FIGURE_BEGIN_FILLED) ;
    try
      Sink.AddArc(ArcSegment) ;
    finally
      Sink.EndFigure(D2D1_FIGURE_END_OPEN) ;
    end;
  finally
    Sink.Close ;
  end ;

  if _canvas.isToBeDrawn then begin
    T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.DrawGeometry(
      Geometry,
      FCanvas.d2dPenBrush,
      T_Pen(FCanvas.D2DPen).Width,
      FCanvas.d2dPenStyle
    );
  end ;
end;


procedure TGIS_RendererVclDirect2D.drawPolygon(
  const _points : array of TPoint
) ;
var
  hr : HRESULT ;
  pg : ID2D1PathGeometry ;
  gs : ID2D1GeometrySink ;
  pt : TD2D1Point2F ;
  i  : Integer ;
begin
  hr := D2DFactory.CreatePathGeometry( pg ) ;
  hr := pg.Open( gs ) ;
  gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;
  gs.SetFillMode( D2D1_FILL_MODE_ALTERNATE ) ;

  pt.x := _points[0].X + GIS_D2D1_PIXEL_SHIFT ;
  pt.y := _points[0].Y + GIS_D2D1_PIXEL_SHIFT ;
  gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_FILLED ) ;
  for i := 1 to Length( _points ) - 1 do begin
    pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
    pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
    gs.AddLine( pt ) ;
  end ;
  gs.EndFigure( D2D1_FIGURE_END_CLOSED ) ;
  hr := gs.Close ;

  if FCanvas.isToBeFilled then begin
    T_Brush(FCanvas.D2DBrush).SelectBrush( Self, FCanvas ) ;
    FCanvas.d2dCanvas.RenderTarget.FillGeometry(
      pg,
      FCanvas.d2dBrushBrush
    ) ;
  end ;

  if FCanvas.isToBeDrawn then begin
    T_Pen(FCanvas.D2DPen).SelectPen( Self, FCanvas ) ;
    FCanvas.d2dCanvas.RenderTarget.DrawGeometry(
      pg,
      FCanvas.d2dPenBrush,
      T_Pen(FCanvas.D2DPen).Width,
      FCanvas.d2dPenStyle
    ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyPolygon(
  const _points : TGIS_DrawBuf ;
  const _parts  : TGIS_IntegerArray ;
  const _count  : Integer
) ;
begin
  drawPolyPolygon( _points, _parts, _count, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyPolygon(
  const _points : TGIS_DrawBuf ;
  const _parts  : TGIS_IntegerArray ;
  const _count  : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  hr  : HRESULT ;
  pg  : ID2D1PathGeometry ;
  gs  : ID2D1GeometrySink ;
  pt  : TD2D1Point2F ;
  i   : Integer ;
  k   : Integer ;
  l   : Integer ;
  npt : Integer ;
begin
  if create_geometry = True then begin
    hr := D2DFactory.CreatePathGeometry( pg ) ;
    hr := pg.Open( gs ) ;
    gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;
    gs.SetFillMode( D2D1_FILL_MODE_ALTERNATE ) ;

    i   := 0 ;
    k   := 0 ;
    npt := length( _points ) ;

    while (k < _count) and (i < npt) do begin
      pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
      pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
      l := i + _parts[k] ;
      Inc( i ) ;
      gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_FILLED ) ;
      while (i < l) and (i < npt) do begin
        pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
        pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
        gs.AddLine( pt ) ;
        Inc( i ) ;
      end ;
      gs.EndFigure( D2D1_FIGURE_END_CLOSED ) ;
      inc( k ) ;
    end ;

    hr := gs.Close ;

    path_geometry   := pg ;
    create_geometry := False ;
  end;

  if _canvas.isToBeFilled then begin
    T_Brush(_canvas.D2DBrush).SelectBrush( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.FillGeometry(
      path_geometry,
      _canvas.d2dBrushBrush
    ) ;
  end ;

  if _canvas.isToBeDrawn then begin
    T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.DrawGeometry(
      path_geometry,
      _canvas.d2dPenBrush,
      T_Pen(_canvas.D2DPen).Width,
      _canvas.d2dPenStyle
    ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyPolygon(
  const _points : TGIS_DrawBufF ;
  const _parts  : TGIS_IntegerArray ;
  const _count  : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  hr : HRESULT ;
  pg : ID2D1PathGeometry ;
  gs : ID2D1GeometrySink ;
  pt : TD2D1Point2F ;
  i  : Integer ;
  k  : Integer ;
  l  : Integer ;
begin
  hr := D2DFactory.CreatePathGeometry( pg ) ;
  hr := pg.Open( gs ) ;
  gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;
  gs.SetFillMode( D2D1_FILL_MODE_ALTERNATE ) ;

  i := 0 ;
  for k := 0 to _count - 1 do begin

    pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
    pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
    l := i + _parts[k] ;
    Inc( i ) ;
    gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_FILLED ) ;
    while i < l do begin
      pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
      pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
      gs.AddLine( pt ) ;
      Inc( i ) ;
    end ;
    gs.EndFigure( D2D1_FIGURE_END_CLOSED ) ;

  end ;

  hr := gs.Close ;

  if _canvas.isToBeFilled then begin
    T_Brush(_canvas.D2DBrush).SelectBrush( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.FillGeometry(
      pg,
      _canvas.d2dBrushBrush
    ) ;
  end ;

  if _canvas.isToBeDrawn then begin
    T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
    _canvas.d2dCanvas.RenderTarget.DrawGeometry(
      pg,
      _canvas.d2dPenBrush,
      T_Pen(_canvas.D2DPen).Width,
      _canvas.d2dPenStyle
    ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.drawLine(
  const _x1     : Integer ;
  const _y1     : Integer ;
  const _x2     : Integer ;
  const _y2     : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  pt1 : TD2DPoint2f ;
  pt2 : TD2DPoint2f ;
begin
  if not _canvas.isToBeDrawn then
    exit ;

  pt1.x := _x1 + GIS_D2D1_PIXEL_SHIFT ;
  pt1.y := _y1 + GIS_D2D1_PIXEL_SHIFT ;
  pt2.x := _x2 + GIS_D2D1_PIXEL_SHIFT ;
  pt2.y := _y2 + GIS_D2D1_PIXEL_SHIFT ;

  T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
  _canvas.d2dCanvas.RenderTarget.DrawLine(
    pt1, pt2,
    _canvas.d2dPenBrush,
    T_Pen(_canvas.D2DPen).Width,
    _canvas.d2dPenStyle
  ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyline(
  const _points : array of TPoint ;
  const _count  : Integer
) ;
begin
  drawPolyline( _points, _count, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyline(
  const _points : array of TPoint ;
  const _count  : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  hr : HRESULT ;
  pg : ID2D1PathGeometry ;
  gs : ID2D1GeometrySink ;
  pt : TD2D1Point2F ;
  i  : Integer ;
begin
  if not _canvas.isToBeDrawn then
    exit ;

  if _count < 2 then exit ;

  if create_geometry then begin
    hr := D2DFactory.CreatePathGeometry( pg ) ;
    hr := pg.Open( gs ) ;
    gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;

    pt.x := _points[0].X + GIS_D2D1_PIXEL_SHIFT ;
    pt.y := _points[0].Y + GIS_D2D1_PIXEL_SHIFT ;
    gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_HOLLOW ) ;
    for i := 1 to _count - 1 do begin
      pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
      pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
      gs.AddLine( pt ) ;
    end ;
    gs.EndFigure( D2D1_FIGURE_END_OPEN ) ;

    hr := gs.Close ;

    path_geometry := pg ;
    create_geometry := False ;
  end;

  T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
  _canvas.d2dCanvas.RenderTarget.DrawGeometry(
    path_geometry,
    _canvas.d2dPenBrush,
    T_Pen(_canvas.D2DPen).Width,
    _canvas.d2dPenStyle
  );
end ;

procedure TGIS_RendererVclDirect2D.drawPolyPolyline(
  const _points : TGIS_DrawBuf ;
  const _parts  : TGIS_IntegerArray ;
  const _count  : Integer
) ;
begin
  drawPolyPolyline(_points, _parts, _count, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyPolyline(
  const _points : TGIS_DrawBuf ;
  const _parts  : TGIS_IntegerArray ;
  const _count  : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  hr : HRESULT ;
  pg : ID2D1PathGeometry ;
  gs : ID2D1GeometrySink ;
  pt : TD2D1Point2F ;
  i  : Integer ;
  k  : Integer ;
  l  : Integer ;
begin
  if not _canvas.isToBeDrawn then
    exit ;

  if _count = 0 then exit ;

  hr := D2DFactory.CreatePathGeometry( pg ) ;
  hr := pg.Open( gs ) ;
  gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;

  i := 0 ;
  for k := 0 to _count - 1 do begin

    pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
    pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
    l := i + _parts[k] ;
    Inc( i ) ;
    gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_HOLLOW ) ;
    while i < l do begin
      pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
      pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
      gs.AddLine( pt ) ;
      Inc( i ) ;
    end ;
    gs.EndFigure( D2D1_FIGURE_END_OPEN ) ;

  end ;

  hr := gs.Close ;

  T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
  _canvas.d2dCanvas.RenderTarget.DrawGeometry(
    pg,
    _canvas.d2dPenBrush,
    T_Pen(_canvas.D2DPen).Width,
    _canvas.d2dPenStyle
  ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawPolyPolyline(
  const _points : TGIS_DrawBufF ;
  const _parts  : TGIS_IntegerArray ;
  const _count  : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
var
  hr : HRESULT ;
  pg : ID2D1PathGeometry ;
  gs : ID2D1GeometrySink ;
  pt : TD2D1Point2F ;
  i  : Integer ;
  k  : Integer ;
  l  : Integer ;
begin
  if not _canvas.isToBeDrawn then
    exit ;

  if _count = 0 then exit ;

  hr := D2DFactory.CreatePathGeometry( pg ) ;
  hr := pg.Open( gs ) ;
  gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;

  i := 0 ;
  for k := 0 to _count - 1 do begin

    pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
    pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
    l := i + _parts[k] ;
    Inc( i ) ;
    gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_HOLLOW ) ;
    while i < l do begin
      pt.x := _points[i].X + GIS_D2D1_PIXEL_SHIFT ;
      pt.y := _points[i].Y + GIS_D2D1_PIXEL_SHIFT ;
      gs.AddLine( pt ) ;
      Inc( i ) ;
    end ;
    gs.EndFigure( D2D1_FIGURE_END_OPEN ) ;

  end ;

  hr := gs.Close ;

  T_Pen(_canvas.D2DPen).SelectPen( Self, _canvas ) ;
  _canvas.d2dCanvas.RenderTarget.DrawGeometry(
    pg,
    _canvas.d2dPenBrush,
    T_Pen(_canvas.D2DPen).Width,
    _canvas.d2dPenStyle
  ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawText(
  const _text   : String  ;
  const _x      : Integer ;
  const _y      : Integer ;
  const _canvas : TGIS_CanvasInternal
) ;
begin
  _canvas.d2dCanvas.Brush.Style := TBrushStyle.bsClear ;
  _canvas.d2dCanvas.TextOut( _x, _y, _text ) ;
end ;

procedure TGIS_RendererVclDirect2D.drawMarker(
  const _style         : TGIS_MarkerStyle  ;
  const _size          : Integer ;
  const _pt            : TPoint  ;
  const _selectionOnly : Boolean
) ;
var
  isize    : Integer ;
  rct      : TRect   ;
  style    : TGIS_MarkerStyle ;

  function px( const x : Integer ) : Integer ;
  begin
    if x <> 0 then Result := _pt.X + isize * x div 128
              else Result := _pt.X ;
  end ;

  function py( const y : Integer ) : Integer ;
  begin
    if y <> 0 then Result := _pt.Y + isize * y div 128
              else Result := _pt.Y ;
  end ;

  function p( const x,y : Integer ) : TPoint ;
  var a, b : Integer ;
  begin
    if x <> 0 then a := _pt.X + isize * x div 128
              else a := _pt.X ;
    if y <> 0 then b := _pt.Y - isize * y div 128
              else b := _pt.Y ;
   Result := Point( a, b )
  end ;

  procedure do_draw ;
  begin
    case style of
      TGIS_MarkerStyle.Box           :
        drawRectangle(   px(-64), py(-64),
                         px( 64), py( 64)
                     ) ;
      TGIS_MarkerStyle.Circle        :
        drawEllipse  (   px(-64), py(-64),
                         px( 64), py( 64)
                     ) ;
      TGIS_MarkerStyle.Cross         :
        drawPolygon  ( [ p(-16, 64), p( 16, 64),
                         p( 16, 16), p( 64, 16),
                         p( 64,-16), p( 16,-16),
                         p( 16,-64), p(-16,-64),
                         p(-16,-16), p(-64,-16),
                         p(-64, 16), p(-16, 16)
                       ]
                     ) ;
      TGIS_MarkerStyle.DiagCross     :
        drawPolygon  ( [ p(-56, 34), p(-34, 56),
                         p(  0, 22), p( 34, 56),
                         p( 56, 34), p( 22,  0),
                         p( 56,-34), p( 34,-56),
                         p(  0,-22), p(-34,-56),
                         p(-56,-34), p(-22,  0)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleUp    :
        drawPolygon  ( [ p(-64,-64), p(  0, 64),
                         p( 64,-64)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleDown  :
        drawPolygon  ( [ p(  0,-64), p(-64, 64),
                         p( 64, 64)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleLeft  :
        drawPolygon  ( [ p(-64,  0), p( 64, 64),
                         p( 64,-64)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleRight :
        drawPolygon  ( [ p(-64, 64), p( 64,  0),
                         p(-64,-64)
                       ]
                     ) ;
    end ;
  end ;

  procedure do_draw_lowres ;
  begin
    case style of
      TGIS_MarkerStyle.Box           :
        drawRectangle(   px(-64), py(-64),
                         px( 64), py( 64)
                     ) ;
      TGIS_MarkerStyle.Circle        :
        drawEllipse  (   px(-64), py(-64),
                         px( 64), py( 64)
                     ) ;
      TGIS_MarkerStyle.Cross         :
        begin
          preparePen(
            FCanvas,
            T_Brush(FCanvas.D2DBrush).Color,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            Point(0, 0),
            TGIS_LineCap.Round,
            TGIS_LineJoin.Miter,
            1
          ) ;

          create_geometry := True ;
          drawPolyline  ( [ p(  0, 64), p(  0,-64)
                          ], 2
                        ) ;
          create_geometry := True ;
          drawPolyline  ( [ p( -64, 0), p( 64,  0)
                          ], 2
                        ) ;
        end;
      TGIS_MarkerStyle.DiagCross     :
        begin
          preparePen(
            FCanvas,
            T_Brush(FCanvas.D2DBrush).Color,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            Point(0, 0),
            TGIS_LineCap.Round,
            TGIS_LineJoin.Miter,
            1
          ) ;

          create_geometry := True ;
          drawPolyline  ( [ p(-56, 56), p( 56,-56)
                          ], 2
                        ) ;
          create_geometry := True ;
          drawPolyline  ( [ p(-56,-56), p( 56, 56)
                          ], 2
                        ) ;
        end;
      TGIS_MarkerStyle.TriangleUp    :
        drawPolygon  ( [ p(-64,-64), p(  0, 64),
                         p( 64,-64)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleDown  :
        drawPolygon  ( [ p(  0,-64), p(-64, 64),
                         p( 64, 64)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleLeft  :
        drawPolygon  ( [ p(-64,  0), p( 64, 64),
                         p( 64,-64)
                       ]
                     ) ;
      TGIS_MarkerStyle.TriangleRight :
        drawPolygon  ( [ p(-64, 64), p( 64,  0),
                         p(-64,-64)
                       ]
                     ) ;
    end ;
  end ;

begin
    if _size = 0 then exit ;
    isize := _size ;

    if not _selectionOnly then begin
      if isize < 3 then
        style := TGIS_MarkerStyle.Box
      else
        style := _style ;

      if ( isize < 8 ) and
         ( ( T_Pen(FCanvas.D2DPen).Width = 0 ) or
           ( T_Pen(FCanvas.D2DPen).Style = TGIS_PenStyle.Clear )
         )
      then
        do_draw_lowres
      else
        do_draw ;
    end ;
    if _selectionOnly then begin
      assert( assigned( oSelectionCanvas ) ) ;

      rct := Rect( _pt.X - isize div 2,
                   _pt.Y - isize div 2,
                   _pt.X + isize div 2,
                   _pt.Y + isize div 2
                 ) ;
      drawRectangle( rct.Left, rct.Top, rct.Right, rct.Bottom,
                     oSelectionCanvas ) ;
    end ;
end ;

procedure TGIS_RendererVclDirect2D.doShapePoint(
  const _shp           : TGIS_ShapePoint ;
  const _source        : TGIS_ShapePoint ;
  const _selectionOnly : Boolean
) ;
var
  params_marker : TGIS_ParamsMarker ;
  angle         : Double ;

  procedure draw_point ;
  var
    pt_x  : Integer ;
    pt_y  : Integer ;
    isize : Integer ;
    origin : TPoint ;
    offset : TPoint ;

    procedure preparePoint ;
    var
      dx, dy : Double ;
    begin
      dx := 0 ;
      dy := 0 ;
      // like TGIS_ShapePoint.GetPoint does
      _shp.GetPointEx( 0, 0, dx, dy ) ;

      // like MapToScreen does
      pt_x := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
      pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
    end ;

  begin
    FCanvas.usePen := True ;
    FCanvas.useBrush := True ;

    origin := GetShapeOrigin ;
    offset := getOffsetPoint( params_marker ) ;
    try
      preparePoint ;

      if params_marker.Size <> GIS_RENDER_SIZE then
        isize := TwipsToPixels( params_marker.Size )
      else
        isize := TwipsToPixels( Viewer.SelectionWidth ) ;


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

          params_marker.Symbol.Prepare( Viewer,
                                        params_marker.Size,
                                        params_marker.Color,
                                        params_marker.OutlineColor,
                                        angle,
                                        0,
                                        TGIS_LabelPosition.MiddleCenter,
                                        True,
                                        Self
                                      ) ;
          params_marker.Symbol.Draw( pt_x, pt_y ) ;
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
            TGIS_LineCap.Round,
            TGIS_LineJoin.Miter,
            TwipsToPixels( params_marker.OutlineWidth )
          ) ;
          drawMarker( params_marker.Style, isize,
                      Point( pt_x, pt_y ), False ) ;
        end ;
      end ;

      if _shp.IsSelected or _selectionOnly then
      begin
        prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                or pextra, pextra ) ;

        preparePen(
          oSelectionCanvas,
          colorSelection,
          TGIS_PenStyle.Solid,
          nil,
          TGIS_BrushStyle.Solid,
          Point(0, 0),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Miter,
          TwipsToPixels( Viewer.SelectionWidth )
        ) ;
        if Viewer.SelectionOutlineOnly then
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
        drawMarker( params_marker.Style, isize,
                    Point( pt_x, pt_y ), True ) ;
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
    end ;
  finally
    sourceShape := nil ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.doShapeMultiPoint(
  const _shp           : TGIS_ShapeMultiPoint ;
  const _source        : TGIS_ShapeMultiPoint ;
  const _selectionOnly : Boolean
) ;
var
  params_marker : TGIS_ParamsMarker ;
  angle         : Double ;

  procedure draw_multi_point ;
  var
    pt_x   : Integer ;
    pt_y   : Integer ;
    i      : Integer ;
    isize  : Integer ;
    origin : TPoint ;
    offset : TPoint ;

    procedure prepare_point( _idx : Integer ) ;
    var
      dx, dy : Double ;
    begin
      _shp.GetPointEx( 0, _idx, dx, dy ) ;
      // like MapToScreen does
      pt_x := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
      pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
    end ;

  begin
    FCanvas.usePen := True ;
    FCanvas.useBrush := True ;

    origin := GetShapeOrigin ;
    offset := getOffsetPoint( params_marker ) ;
    try
      if params_marker.Size <> GIS_RENDER_SIZE then
        isize := TwipsToPixels( params_marker.Size )
      else
        isize := TwipsToPixels( Viewer.SelectionWidth ) ;

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
          for i := 0 to _shp.GetNumPoints -1 do begin
            prepare_point( i ) ;
            params_marker.Symbol.Draw( pt_x, pt_y ) ;
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
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            TwipsToPixels( params_marker.OutlineWidth )
          ) ;
          for i := 0 to _shp.GetNumPoints -1 do begin
            prepare_point( i ) ;
            drawMarker( params_marker.Style, isize,
                        Point( pt_x, pt_y ), False ) ;
          end ;
        end ;
      end ;

      if _shp.IsSelected or _selectionOnly then
      begin
        prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                or pextra, pextra ) ;

        preparePen(
          oSelectionCanvas,
          colorSelection,
          TGIS_PenStyle.Solid,
          nil,
          TGIS_BrushStyle.Solid,
          Point(0, 0),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          TwipsToPixels( Viewer.SelectionWidth )
        ) ;
        if Viewer.SelectionOutlineOnly then
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
        for i := 0 to _shp.GetNumPoints -1 do begin
          prepare_point( i ) ;
          drawMarker( params_marker.Style, isize,
                      Point( pt_x, pt_y ), True ) ;
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
      draw_multi_point ;
    except
    end ;
  finally
    sourceShape := nil ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.doShapeLine(
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
    cur_size  : Integer ;
    draw_buf  : TGIS_DrawBuf ;
    selwidth  : Integer ;
    max_tlrnc : Integer ;
    origin    : TPoint  ;
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
      pt_x       : Integer ;
      pt_y       : Integer ;
      last_pt_x  : Integer ;
      last_pt_y  : Integer ;
      tmp_tlrnc  : Integer ;
      isize      : Integer ;
    begin
      Result := 0 ;
      part_size := _shp.GetPartSize( _part_no ) ;
      if part_size <= 0 then exit ;

      cur_tlrnc := _max_tlrnc ;
      last_pt_x := 0 ;
      last_pt_y := 0 ;
      repeat
        isize := 0 ;

        for point_no := 0 to part_size - 1 do begin
          // translate all points in part to screen coordinates
          _shp.GetPointEx( _part_no, point_no, dx, dy ) ;

          // like MapToScreen does
          pt_x :=  RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
          pt_y :=  RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;

          if point_no = 0 then begin
            draw_buf[isize] := Point( pt_x, pt_y );
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
              draw_buf[isize] := Point( pt_x, pt_y );
              last_pt_x := pt_x ;
              last_pt_y := pt_y ;
              inc( isize ) ;
            end ;
          end
          else begin
            draw_buf[isize] := Point( pt_x, pt_y );
            inc( isize ) ;
          end ;
        end ;
        cur_tlrnc := cur_tlrnc + 1 ;
        assert( isize < GDI_MAXPOINT_COUNT ) ;
      until isize < GDI_MAXPOINT_COUNT ;

      SetLength( draw_buf, isize ) ;
      Result := isize ;
    end ;

  begin
    FCanvas.usePen := True ;
    FCanvas.useBrush := True ;

    origin := GetShapeOrigin ;
    offset := getOffsetPoint( params_line ) ;
    try
      for part_no := 0 to _shp.GetNumParts - 1 do begin // all parts

        SetLength( draw_buf, _shp.GetPartSize(part_no) ) ;

        cur_size := prepare_drawbufpart( iTolerance, part_no ) ;
        if cur_size < 1 then continue ;
        create_geometry := True ;

        if not _selectionOnly then begin

          // first draw outline
          if ( Integer( _outlineMode )
               and
               Integer( TGIS_RendererMultipassMode.Outline )
             ) <> 0
          then begin
            if params_line.OutlineWidth <> 0 then begin
              if not assigned( params_line.Symbol ) then begin
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
                      + 2* TwipsToPixels( params_line.OutlineWidth )
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
                      + 2* TwipsToPixels( params_line.OutlineWidth )
                  ) ;

                drawPolyline( draw_buf, cur_size ) ;
              end ;
            end ;
          end ;

          // then line itself
          if ( Integer( _outlineMode )
               and
               Integer( TGIS_RendererMultipassMode.Line )
              ) <> 0
          then begin
            if assigned( params_line.Symbol ) then begin
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
              TGIS_SymbolLineHelper.DrawLine( Viewer, draw_buf,
                                              params_line.Symbol, cur_size ) ;
              params_line.Symbol.Unprepare ;
            end
            else begin
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
              drawPolyline( draw_buf, cur_size ) ;
            end ;
          end ;
        end;

        if _shp.IsSelected or _selectionOnly then begin
          prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                  or pextra, pextra ) ;

          // draw selected
          selwidth := TwipsToPixels( Viewer.SelectionWidth ) ;
          if not Viewer.SelectionOutlineOnly then begin
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

          SetLength( draw_buf, _shp.GetPartSize(part_no) ) ;

          cur_size := prepare_drawbufpart( max_tlrnc, part_no ) ;
          if cur_size < 0 then continue ;
          create_geometry := True ;

          preparePen(
            oSelectionCanvas,
            colorSelection,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            Point(0, 0),
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            selwidth
          ) ;
          drawPolyline( draw_buf, cur_size, oSelectionCanvas ) ;
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
    end ;
  finally
    sourceShape := nil ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.doShapePolygon(
  const _shp           : TGIS_ShapePolygon ;
  const _source        : TGIS_ShapePolygon ;
  const _selectionOnly : Boolean
) ;
var
  params_area : TGIS_ParamsArea ;

  procedure draw_polygon ;
  var
    num_parts : Integer ;
    draw_buf  : TGIS_DrawBuf      ;
    part_buf  : TGIS_IntegerArray ;
    hole_buf  : array of Byte     ;
    shp_size  : Integer ;
    max_tlrnc : Integer ;
    selwidth  : Integer ;
    tmp       : Integer ;
    i, j, k   : Integer ;
    pbuf      : TGIS_DrawBuf ;
    origin    : TPoint ;
    offset    : TPoint ;

    function prepare_drawbuf(
      const _max_tlrnc : Integer
    ) : Integer  ;
    var
      part_no    : Integer ;
      part_size  : Integer ;
      cur_tlrnc  : Integer ;
      cur_size   : Integer ;
      point_no   : Integer ;
      dx, dy     : Double  ;
      pt_x, pt_y : Integer ;
      tmp_tlrnc  : Integer ;
      last_pt_x  : Integer ;
      last_pt_y  : Integer ;
      first_pt_x : Integer ;
      first_pt_y : Integer ;
      iparts     : Integer ;
      isize      : Integer ;
      ilastsize  : Integer ;
    begin
      Result := 0 ;

      iparts    := 0 ;
      ilastsize := 0 ;
      isize     := 0 ;
      for part_no := 0 to _shp.GetNumParts -1 do begin
        part_size := _shp.GetPartSize( part_no ) ;

        if part_size <= 0 then continue ;

        first_pt_x := 0 ;
        first_pt_y := 0 ;
        last_pt_x  := 0 ;
        last_pt_y  := 0 ;
        cur_tlrnc := _max_tlrnc ;
        repeat
          isize := ilastsize ;
          cur_size := 0 ;

          for point_no := 0 to part_size - 1 do begin
            // translate all points in the part to screen coordinates
            _shp.GetPointEx( part_no, point_no, dx, dy ) ;

            // like MapToScreen does
            pt_x := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
            pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;

            if ( point_no = 0 ) then begin
              first_pt_x := pt_x ;
              first_pt_y := pt_y ;
              last_pt_x  := pt_x ;
              last_pt_y  := pt_y ;
            end else if ( point_no  < part_size - 1 ) then begin
              // basic simplifier
              tmp_tlrnc := Abs( pt_x - last_pt_x ) +
                           Abs( pt_y - last_pt_y ) ;
              if tmp_tlrnc > cur_tlrnc then begin
                if cur_size = 0 then begin
                  draw_buf[isize].X := first_pt_x ;
                  draw_buf[isize].Y := first_pt_y ;
                  inc( isize ) ;
                  inc( cur_size ) ;
                end;

                draw_buf[isize].X := pt_x ;
                draw_buf[isize].Y := pt_y ;
                last_pt_x := pt_x ;
                last_pt_y := pt_y ;
                inc( isize ) ;
                inc( cur_size ) ;
              end ;
            end else begin
              if isize = 0 then begin
                tmp_tlrnc := RoundS( Abs( pt_x - last_pt_x ) +
                                     Abs( pt_y - last_pt_y )
                                   ) ;
                if tmp_tlrnc > cur_tlrnc then  begin
                  draw_buf[isize] := Point( first_pt_x, first_pt_y );
                  inc( isize ) ;
                end ;
              end ;

              if cur_size > 0 then begin
                draw_buf[isize].X := pt_x ;
                draw_buf[isize].Y := pt_y ;
                inc( isize ) ;
                inc( cur_size ) ;
              end
              else begin
                num_Parts := 0 ;
              end;
            end ;
          end ;
          cur_tlrnc := cur_tlrnc + 1 ;
        until cur_size < GDI_MAXPOINT_COUNT ;

        ilastsize := isize ;

        if cur_size > 0 then begin
          part_buf[iparts] := cur_size ;
          hole_buf[iparts] := 1 ;
          inc( iparts ) ;
        end;
      end ;

      SetLength( draw_buf, isize ) ;
      Result := iparts ;
    end ;

    procedure draw_polygon_symbolfill ;
    var
      fill_gap  : TPoint ;
      fill_size : TPoint ;
      bmp  : TBitmap ;
      br   : ID2D1Brush ;
      pn   : ID2D1Brush ;
      st   : ID2D1StrokeStyle ;
      cnvp : TDirect2DCanvas ;
      tbmp : TGIS_Bitmap ;

      bmp_hnd      : HBITMAP ;
      bmp_old      : HBITMAP ;
      bmp_dc       : HDC ;
    begin
      bmp := nil ;
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
        fill_gap.X  := TwipsToPixels( params_area.SymbolGap ) ;
        fill_gap.Y  := TwipsToPixels( params_area.SymbolGap ) ;
        fill_size.X := params_area.Symbol.Width  + fill_gap.X ;
        fill_size.Y := params_area.Symbol.Height + fill_gap.Y ;
        // create 32-bit bitmap, fill out with transparent background
        bmp := Vcl.Graphics.TBitmap(
          createTransparentBitmap( fill_size.X, fill_size.Y )
        ) ;
        bmp.AlphaFormat := TAlphaFormat.afPremultiplied ;
        bmp_hnd := bmp.Handle ;
        bmp_dc := CreateCompatibleDC( 0 ) ;
        bmp_old := SelectObject( bmp_dc, bmp_hnd ) ;

        cnvp := FCanvas.d2dCanvas ;
        br   := FCanvas.d2dBrushBrush ;
        pn   := FCanvas.d2dPenBrush ;
        st   := FCanvas.d2dPenStyle ;
        FCanvas.d2dCanvas := TDirect2DCanvas.Create(
          bmp_dc, Rect( 0, 0, bmp.Width, bmp.Height )
        ) ;
        FCanvas.d2dBrushBrush := nil ;
        FCanvas.d2dPenBrush   := nil ;
        FCanvas.d2dPenStyle   := nil ;
        try
          FCanvas.BeginDraw ;
          params_area.Symbol.Draw( params_area.Symbol.Width  div 2,
                                   params_area.Symbol.Height div 2
                                 ) ;
          FCanvas.EndDraw ;
        finally
          SelectObject( bmp_dc, bmp_old ) ;
          DeleteDC( bmp_dc ) ;

          FreeObject( FCanvas.d2dCanvas ) ;
          FCanvas.d2dCanvas := cnvp ;
          FCanvas.d2dBrushBrush := br ;
          FCanvas.d2dPenBrush   := pn ;
          FCanvas.d2dPenStyle   := st ;
        end ;

        tbmp := TGIS_Bitmap.Create ;
        tbmp.NativeBitmap := bmp ;
        ignorePPI := True ;
        FCanvas.usePen := False ;
        try
          prepareBrush(
            FCanvas,
            params_area.Color,
            tbmp ,
            TGIS_BrushStyle.Solid,
            origin
          ) ;
          drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
        finally
          FCanvas.usePen := True ;
          ignorePPI := False ;
          FreeObject( tbmp ) ;
        end ;
      finally
        FreeObject( bmp ) ;
        params_area.Symbol.Unprepare ;
      end ;
    end ;

  begin
    SetLength( draw_buf, _shp.GetNumPoints ) ;
    SetLength( part_buf, _shp.GetNumParts  ) ;
    SetLength( hole_buf, _shp.GetNumParts  ) ;

    FCanvas.usePen := True ;
    FCanvas.useBrush := True ;

    origin := GetShapeOrigin ;
    offset := getOffsetPoint( params_area ) ;
    try
      if not _selectionOnly then begin

        num_parts := prepare_drawbuf( iTolerance ) ;
        if num_parts < 1 then exit ;
        create_geometry := True ;

        // inside first

        // symbol fill
        if assigned( params_area.Symbol ) then
        begin
          draw_polygon_symbolfill ;
        end

        // bitmap fill
        // pattern fill
        // standard fill
        else begin

          FCanvas.usePen := False ;
          try
            if ( not TGIS_Bitmap.IsNilOrEmpty( params_area.Bitmap ) ) and
               Assigned( _shp.Layer )                                 and
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
            drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
          finally
            FCanvas.usePen := True ;
          end ;

        end ;

        // line background for dashed lines
        if ( params_area.OutlineSymbol <> nil                 ) and
           ( params_area.OutlineStyle  <> TGIS_PenStyle.Solid ) and
           ( params_area.OutlineBackColor.ARGB  <> TGIS_Color.Crazy.ARGB ) then
        begin
          preparePen(
            FCanvas,
            params_area.OutlineBackColor,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            Point(0, 0),
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            TwipsToPixels( params_area.OutlineWidth )
          ) ;
          drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
        end ;

        // outline symbol fill
        if assigned( params_area.OutlineSymbol ) then begin
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
          // draw parts
          k := 0 ;
          for i := 0 to length(part_buf)-1 do begin
            SetLength( pbuf, part_buf[i] ) ;
            for j := 0 to part_buf[i] - 1 do
              pbuf[j] := draw_buf[k+j];
            TGIS_SymbolLineHelper.DrawLine(
              Viewer, pbuf,
              params_area.OutlineSymbol,
              part_buf[i] ) ;
            k := k + part_buf[i] ;
          end ;
          params_area.OutlineSymbol.Unprepare ;
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
          drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
        end

        // standard outline
        else begin

          FCanvas.useBrush := False ;
          try
            if ( ( params_area.OutlineWidth = 0 ) or
                 ( params_area.OutlineStyle = TGIS_PenStyle.Clear ) )
               and
               ( not Assigned(  params_area.Symbol ) ) and
               ( not Assigned(  params_area.Bitmap ) ) and
               ( params_area.Pattern = TGIS_BrushStyle.Solid )
            then begin
              if params_area.Color.A = 255 then begin
                // 1 pixel outline with the bitmap color
                preparePen(
                  FCanvas,
                  params_area.Color,
                  TGIS_PenStyle.Solid,
                  nil,
                  TGIS_BrushStyle.Solid,
                  origin,
                  TGIS_LineCap.Round,
                  TGIS_LineJoin.Round,
                  TwipsToPixels( 1 )
                ) ;
                drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
              end;
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
              drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
            end
            else begin
              // OutlineWidth = 0 and pattern fill - do nothing
            end ;
          finally
            FCanvas.useBrush := True ;
          end ;
        end ;

      end ;

      // draw selected
      if _shp.IsSelected or _selectionOnly then begin
        prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                 or pextra, pextra ) ;

        selwidth := TwipsToPixels( Viewer.SelectionWidth ) ;
        if not Viewer.SelectionOutlineOnly then begin
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

        num_parts := prepare_drawbuf( max_tlrnc ) ;
        if num_parts < 1 then exit ;
        create_geometry := True ;

        if params_area.OutlineWidth <> GIS_RENDER_SIZE then
          selwidth := selwidth +
                      TwipsToPixels( params_area.OutlineWidth ) ;
        preparePen(
          oSelectionCanvas,
          colorSelection,
          TGIS_PenStyle.Solid,
          nil,
          TGIS_BrushStyle.Solid,
          Point(0, 0),
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          selwidth
        ) ;
        if Viewer.SelectionOutlineOnly then
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
        drawPolyPolygon( draw_buf, part_buf, num_parts, oSelectionCanvas ) ;
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
    end ;
  finally
    sourceShape := nil ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.doShapeComplex(
  const _shp           : TGIS_ShapeComplex ;
  const _source        : TGIS_ShapeComplex ;
  const _selectionOnly : Boolean ;
  const _outlineMode   : TGIS_RendererMultipassMode
) ;
var
  i   : Integer ;
begin
  for i := 0 to _shp.ShapesCount-1 do
    RenderShape( _shp.GetShape(i), _source,
                 _selectionOnly, _outlineMode
               ) ;
end ;

procedure TGIS_RendererVclDirect2D.doLabelPoint(
  const _shp        : TGIS_Shape ;
  const _savePoints : Boolean ;
  var   _points     : TGIS_DrawBuf
) ;
var
  params_label    : TGIS_ParamsLabel ;
  txt             : String  ;
  params_marker   : TGIS_ParamsMarker ;
  label_alignment : TGIS_LabelAlignment ;
  label_positions : TGIS_LabelPositions ;
  rct             : TRect   ;
  cpoint          : TGIS_Point ;
  offset          : TPoint ;
  pt_origin_x     : Integer ;
  pt_origin_y     : Integer ;
  angle           : Double  ;
  rct_br          : TPoint  ;
  ssin            : Double  ;
  scos            : Double  ;
  gap             : Integer ;
  gap2            : Integer ;

  function text_extent(
    const _text      : String     ;
    const _rbp       : TPoint     ;
    const _alignment : TGIS_LabelAlignment
  ) : TPoint ;
  var
    lbl : TGIS_HtmlLabel ;
    rect : TRect ;
  begin
    rect := System.Types.Rect( 0, 0, _rbp.X, _rbp.Y ) ;
    lbl := TGIS_HtmlLabel.Create( self, _text, _alignment,
                                  rect.Right - rect.Left + 1,
                                  rect.Bottom - rect.Top + 1
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
    const _text      : String     ;
    const _rect      : TRect      ;
    const _shadow    : Boolean    ;
    const _alignment : TGIS_LabelAlignment ;
    const _angle     : Double     ;
    const _origin    : TPoint
  ) : TRect ;
  var
    shadow_width : Integer ;
    lbl : TGIS_HtmlLabel ;
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
      lbl.Draw( _rect, 0, _origin,
                shadow_width, _shp.Params.Labels.Color ) ;
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
    start           : TPoint  ;
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
    sym_size        : Double ;
    sym_scale       : Double ;
    sym_scale_x     : Double ;
    bshield         : Boolean ;
    bshadow         : Boolean ;
    txt_off_x       : Single ;
    txt_off_y       : Single ;
    sym_off_x       : Single ;
    sym_off_y       : Single ;
    dtmp            : Single ;
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
    if Assigned( sym ) then begin
      bshield := True ;

      sym_rct_label   := sym.ShieldLabel ;
      sym_rct_boundry := sym.ShieldBounds ;

      sym_scale := rct.Height / sym_rct_label.Height ;

      sym_scale_x := 1 ;

      // fit text in ShielLabel space
      if sym_scale * sym_rct_label.Width < rct.Width then begin
        sym_scale_x :=  rct.Width / ( sym_scale * sym_rct_label.Width ) ;
      end;

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

    if Assigned( _shp.Layer.LabelPosEvent ) then begin
      start := Point( start_x, start_y ) ;
      _shp.Layer.LabelPosEvent( TObject(Parent), _shp, _pos, start, rct ) ;
      start_x := start.X ;
      start_y := start.Y ;
    end ;

    rct_loc_left   := rct_tmp.Left   + start_x ;
    rct_loc_top    := rct_tmp.Top    + start_y ;
    rct_loc_right  := rct_tmp.Right  + start_x ;
    rct_loc_bottom := rct_tmp.Bottom + start_y ;

    if angle <> 0 then begin
      // like GisRotatePoint does
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
      labext_xmin :=    rct_loc_left   / FZoom - FExtentX  ;
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
            if Assigned( sym ) then
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
        if Assigned( sym ) then
          sym.Unprepare ;

        Result := False ;
        exit ;
      end ;
    end ;
    Result := True ;

    if ( not _shp.Params.Labels.Duplicates ) then
      Viewer.LabelsReg.AddDuplicated( txt )  ;

    if Assigned( sym ) then begin
      sym.Draw( rct_loc_left / 2 + rct_loc_right  / 2 + sym_off_x,
                rct_loc_top  / 2 + rct_loc_bottom / 2 + sym_off_y
              ) ;
      sym.Unprepare ;
    end;

    // draw background
    try
      if params_label.Pattern <> TGIS_BrushStyle.Clear then
      begin
        prepareBrush(
          oLabelsCanvas,
          params_label.Color,
          params_label.Bitmap,
          params_label.Pattern,
          Point(0, 0)
        ) ;
        oLabelsCanvas.useBrush := True ;
      end else
        oLabelsCanvas.useBrush := False ;
      if params_label.OutlineStyle <> TGIS_PenStyle.Clear then
      begin
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
        oLabelsCanvas.usePen := True ;
      end else
        oLabelsCanvas.usePen := False ;
      if oLabelsCanvas.useBrush or oLabelsCanvas.usePen then begin
        drawRectangle(
          RoundS( rct_txt.Left   + rct_loc_left ),
          RoundS( rct_txt.Top    + rct_loc_top  ),
          RoundS( rct_txt.Right  + rct_loc_left ),
          RoundS( rct_txt.Bottom + rct_loc_top  ),
          oLabelsCanvas
        ) ;
      end;
    finally
      oLabelsCanvas.usePen := True ;
      oLabelsCanvas.useBrush := True ;
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
  // cached values
  params_label := _shp.Params.Labels ;
  if not params_label.Visible then exit ;

  // prepare text for label
  txt := _shp.GetLabel ;
  if IsStringEmpty( txt ) then exit ;

  if ( not FTiled ) and ( not params_label.Duplicates ) and
    Viewer.LabelsReg.IsDuplicated( txt )
  then
    exit ;

  oLabelsCanvas.usePen   := True ;
  oLabelsCanvas.useBrush := True ;

  // rest of cached values
  params_marker   := _shp.Params.Marker ;
  label_alignment := params_label.Alignment ;
  label_positions := params_label.Position  ;

  // prepare rectangle for label
  rct := Rect( 0, 0,
               TwipsToPixels( params_label.Width,  0 ),
               TwipsToPixels( params_label.Height, 0 )
             ) ;

  if ( ( rct.Right  - rct.Left ) <= 0 ) or
     ( ( rct.Bottom - rct.Top  ) <= 0 )
  then exit ;

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
    CanvasSetTransformation( angle, pt_origin_x, pt_origin_Y );
    SinCos( angle, ssin, scos ) ;
  end ;

  try
    // calculate the size of the rectangle
    rct_br.X := rct.Right  ;
    rct_br.Y := rct.Bottom ;
    rct_br   := text_extent(
                  txt,
                  rct_br,
                  label_alignment
                ) ;
    rct := Rect( 0, 0,
                 rct_br.X, rct_br.Y
               ) ;

    if ( ( rct.Right  - rct.Left ) <= 0 ) or
       ( ( rct.Bottom - rct.Top  ) <= 0 )
    then exit ;

    if rct.Right  >  TwipsToPixels( params_label.Width,  0 ) then
      rct_br.X    := TwipsToPixels( params_label.Width,  0 ) ;
    if rct.Bottom >  TwipsToPixels( params_label.Height, 0 ) then
      rct_br.Y    := TwipsToPixels( params_label.Height, 0 ) ;
    rct := Rect( 0, 0, rct_br.X, rct_br.Y ) ;

    // calculate offset of rectangle
    if _shp.ShapeType in
       [TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint] then begin
      gap := TwipsToPixels( params_marker.Size ) div 2 ;
      gap := gap + TwipsToPixels( params_marker.OutlineWidth )  ;
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
      CanvasClearTransformation() ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.doLabelArc(
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
    GisDrawArcLabel( _shp, _shp, part_no, txt, FTiled, _savePoints, _points ) ;

end ;

procedure TGIS_RendererVclDirect2D.doChart(
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
         ( High( _shp.Params.Chart.ColorsInternal ) >= 0 )  then
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

procedure TGIS_RendererVclDirect2D.drawEditingLines(
  const _shp : TGIS_Shape
) ;
var
  shp         : TGIS_Shape ;
  draw_buf    : TGIS_DrawBuf ;
  part_no     : Integer ;
  part_size   : Integer ;

  function prepare_drawbufpart(
    const _max_tlrnc : Integer ;
    const _part_no   : Integer
  ) : Integer ;
  var
    part_size  : Integer ;
    cur_tlrnc  : Integer ;
    point_no   : Integer ;
    dx, dy     : Double  ;
    pt_x       : Integer ;
    pt_y       : Integer ;
    last_pt_x  : Integer ;
    last_pt_y  : Integer ;
    first_pt_x : Integer ;
    first_pt_y : Integer ;
    tmp_tlrnc  : Integer ;
    isize      : Integer ;
  begin
    Result := 0 ;
    part_size := _shp.GetPartSize( _part_no ) ;
    if part_size <= 0 then exit ;
    first_pt_x := 0 ;
    first_pt_y := 0 ;
    last_pt_x  := 0 ;
    last_pt_y  := 0 ;
    cur_tlrnc := _max_tlrnc ;
    repeat
      isize := 0 ;

      for point_no := 0 to part_size - 1 do begin
        // translate all points in part to screen coordinates
        _shp.GetPointEx( _part_no, point_no, dx, dy ) ;

        // like MapToScreen does
        pt_x :=  RoundS( (  dx + FExtentX ) * FZoom ) ;
        pt_y :=  RoundS( ( -dy + FExtentY ) * FZoom ) ;

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

            if isize = 0 then begin
              draw_buf[isize] := Point( first_pt_x, first_pt_y );
              inc( isize ) ;
            end ;

            draw_buf[isize] := Point( pt_x, pt_y );
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
              draw_buf[isize] := Point( first_pt_x, first_pt_y );
              inc( isize ) ;
            end ;
          end ;

          if isize > 0  then begin
            draw_buf[isize] := Point( pt_x, pt_y );
            inc( isize ) ;
          end;
        end ;
      end ;
      cur_tlrnc := cur_tlrnc + 1 ;
      assert( isize < GDI_MAXPOINT_COUNT ) ;
    until isize < GDI_MAXPOINT_COUNT ;

    SetLength( draw_buf, isize ) ;
    Result := isize ;
  end ;

begin
  shp := _shp ;
  if shp = nil then exit ;

  prepareBrush(
    oEditCanvas,
    Viewer.Editor.EditingLinesStyle.BrushColor,
    nil,
    Viewer.Editor.EditingLinesStyle.BrushStyle,
    Point(0, 0)
  ) ;
  preparePen(
    oEditCanvas,
    Viewer.Editor.EditingLinesStyle.PenColor,
    Viewer.Editor.EditingLinesStyle.PenStyle,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    TGIS_LineJoin.Round,
    Viewer.Editor.EditingLinesStyle.PenWidth
  ) ;

  for part_no := 0 to shp.GetNumParts - 1 do begin
    // draw_part

    SetLength( draw_buf, _shp.GetPartSize(part_no) ) ;

    part_size := prepare_drawbufpart( iTolerance, part_no ) ;
    if part_size < 1 then
      continue ;
    create_geometry := True ;

    drawPolyline( draw_buf, part_size, oEditCanvas ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.drawEditingPointMarkers(
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
  old_pt_txt_x : Integer ;
  old_pt_txt_y : Integer ;
  ptg3D        : TGIS_Point3D ;
  dx, dy   : Double  ;
  pt_x     : Integer ;
  pt_y     : Integer ;
  tw       : Integer ;

  procedure prepare_font ;
  begin
    oEditCanvas.Font.Name  := Viewer.Editor.EditingPointsStyle.PointsFont.Name ;
    oEditCanvas.Font.Size  := Viewer.Editor.EditingPointsStyle.PointsFont.Size ;
    oEditCanvas.Font.Style := Viewer.Editor.EditingPointsStyle.PointsFont.Style ;
    oEditCanvas.Font.Color := Viewer.Editor.EditingPointsStyle.PointsFont.Color ;
    prepareFont(
      oEditCanvas,
      oEditCanvas.Font.Name,
      oEditCanvas.Font.Size,
      oEditCanvas.Font.Style,
      oEditCanvas.Font.Color
    ) ;
  end ;

  procedure prepare_active ;
  begin
    prepareBrush(
      oEditCanvas,
      Viewer.Editor.EditingPointsStyle.ActivePoints.BrushColor,
      nil,
      Viewer.Editor.EditingPointsStyle.ActivePoints.BrushStyle,
      Point(0, 0)
    ) ;
    preparePen(
      oEditCanvas,
      Viewer.Editor.EditingPointsStyle.ActivePoints.PenColor,
      Viewer.Editor.EditingPointsStyle.ActivePoints.PenStyle,
      nil,
      TGIS_BrushStyle.Solid,
      Point(0, 0),
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Viewer.Editor.EditingPointsStyle.ActivePoints.PenWidth
    ) ;
  end ;

  procedure prepare_inactive ;
  begin
    prepareBrush(
      oEditCanvas,
      Viewer.Editor.EditingPointsStyle.InactivePoints.BrushColor,
      nil,
      Viewer.Editor.EditingPointsStyle.InactivePoints.BrushStyle,
      Point(0, 0)
    ) ;
    preparePen(
      oEditCanvas,
      Viewer.Editor.EditingPointsStyle.InactivePoints.PenColor,
      Viewer.Editor.EditingPointsStyle.InactivePoints.PenStyle,
      nil,
      TGIS_BrushStyle.Solid,
      Point(0, 0),
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Viewer.Editor.EditingPointsStyle.InactivePoints.PenWidth
    ) ;
  end ;

  procedure prepare_z(
    const _param : Boolean
  ) ;
  var
    cl : TColor ;
  begin
    if _param then cl := clOlive else cl := clRed ;
    prepareBrush(
        oEditCanvas,
        GISColor( cl ),
        nil,
        T_Brush(oEditCanvas.D2DBrush).Style,
        Point(0, 0)
    ) ;
  end ;

  procedure prepare_selected ;
  begin
    prepareBrush(
      oEditCanvas,
      Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushColor,
      nil,
      Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushStyle,
      Point(0, 0)
    ) ;
    preparePen(
      oEditCanvas,
      Viewer.Editor.EditingPointsStyle.SelectedPoints.PenColor,
      Viewer.Editor.EditingPointsStyle.SelectedPoints.PenStyle,
      nil,
      TGIS_BrushStyle.Solid,
      Point(0, 0),
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Viewer.Editor.EditingPointsStyle.SelectedPoints.PenWidth
    ) ;
  end ;

  procedure draw_marker(
    const _x1 : Integer ;
    const _y1 : Integer ;
    const _x2 : Integer ;
    const _y2 : Integer
  ) ;
  begin
    drawEllipse( _x1, _y1, _x2, _y2, oEditCanvas ) ;
  end ;

  function get_text_width(
    const _text : String
  ) : Integer ;
  var
    cnv  : TGIS_CanvasInternal ;
    size : TPoint ;
  begin
    cnv := FCanvas ;
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
    cnv : TGIS_CanvasInternal ;
    gap : Integer ;
  begin
      if Viewer.Editor.EditingPointsStyle.PointsFont.Color <>
         Viewer.Editor.EditingPointsStyle.PointsBackground then begin
        cnv := FCanvas ;
        try
          FCanvas := oEditCanvas ;
          pt := CanvasTextExtent( _text ) ;
        finally
          FCanvas := cnv ;
        end;
        // some space for better visibility
        gap := TwipsToPixels( 15 ) ;
        prepareBrush(
          oEditCanvas,
          Viewer.Editor.EditingPointsStyle.PointsBackground,
          nil,
          TGIS_BrushStyle.Solid,
          Point(0, 0)
        ) ;
        drawRectangle( _x - 2*gap, _y, _x + pt.X, _y + pt.Y - gap,
                       oEditCanvas
                     ) ;
      end ;
    drawText( _text, _x, _y, oEditCanvas  ) ;
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

        draw_marker( pt_x - tps, pt_y - tps, pt_x + tps, pt_y + tps ) ;

        if ( Abs( old_pt_txt_x - pt_x ) +
             Abs( pt_y - old_pt_txt_y )
           ) >=  2 * Abs( oEditCanvas.Font.Size )
        then begin
          old_pt_txt_x := pt_x ;
          old_pt_txt_y := pt_y ;

          if Viewer.Editor.ShowPointsNumbers then begin
            draw_text( pt_x + tps, pt_y + tps, IntToStr( point_no ) ) ;
            if Viewer.Editor.ShowPoints3D then begin
              // Show coord Z
              prepare_z ( Abs(ptg3D.Z) > eps ) ;
              tw := get_text_width( Format('%.2f',[ptg3D.Z] ) ) ;
              draw_text( pt_x - tps*2 - tw, pt_y - tps*2,
                         Format('%.2f',[ptg3D.Z] ) ) ;
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
      draw_marker( pt_x - tps, pt_y - tps, pt_x + tps, pt_y + tps ) ;
    end ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.drawEditingEdgeLengths(
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
    oEditCanvas.Font.Name  := Viewer.Editor.EditingEdgeLengthsStyle.Font.Name ;
    oEditCanvas.Font.Size  := Viewer.Editor.EditingEdgeLengthsStyle.Font.Size ;
    oEditCanvas.Font.Style := Viewer.Editor.EditingEdgeLengthsStyle.Font.Style ;
    oEditCanvas.Font.Color := Viewer.Editor.EditingEdgeLengthsStyle.Font.Color ;
    prepareFont(
      oEditCanvas,
      oEditCanvas.Font.Name,
      oEditCanvas.Font.Size,
      oEditCanvas.Font.Style,
      oEditCanvas.Font.Color
    ) ;

    prepareBrush(
      oEditCanvas,
      Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushColor,
      nil,
      Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushStyle,
      Point(0, 0)
    ) ;
    preparePen(
      oEditCanvas,
      Viewer.Editor.EditingEdgeLengthsStyle.Background.PenColor,
      Viewer.Editor.EditingEdgeLengthsStyle.Background.PenStyle,
      nil,
      TGIS_BrushStyle.Solid,
      Point(0, 0),
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Viewer.Editor.EditingEdgeLengthsStyle.Background.PenWidth
    ) ;
  end ;

  function get_text_width(
    const _text : String
  ) : Integer ;
  var
    cnv  : TGIS_CanvasInternal ;
    size : TPoint ;
  begin
    cnv := FCanvas ;
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
    cnv : TGIS_CanvasInternal ;
    gap : Integer ;
  begin
    if Viewer.Editor.EditingEdgeLengthsStyle.Font.Color <>
       Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushColor then begin
      cnv := FCanvas ;
      try
        FCanvas := oEditCanvas ;
        pt := CanvasTextExtent( _text ) ;
      finally
        FCanvas := cnv ;
      end;
      // some space for better visibility
      gap := TwipsToPixels( 15 ) ;
      drawRectangle( _x - gap, _y, _x + pt.X, _y + pt.Y - gap,
                     oEditCanvas
                   ) ;
    end ;
    drawText( _text, _x, _y, oEditCanvas  ) ;
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
    cnv : TGIS_CanvasInternal ;
  begin
    cnv := FCanvas ;
    try
      FCanvas := oEditCanvas ;
      CanvasSetTransformation( _angle, _origin_x, _origin_y ) ;
    finally
      FCanvas := cnv ;
    end ;
  end ;

  procedure clear_transformation ;
  var
    cnv : TGIS_CanvasInternal ;
  begin
    cnv := FCanvas ;
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
           ) >=  2 * Abs( oEditCanvas.Font.Size )
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

procedure TGIS_RendererVclDirect2D.drawEditingPoints(
  const _bitmap : TBitmap ;
  const _shp    : TGIS_Shape
) ;
begin
  if _shp.IsEmpty then exit ;
  oEditCanvas := TGIS_CanvasInternal.Create( _bitmap ) ;
  inEdit := True ;
  try
    if not Assigned( _shp.Layer.Renderer ) then
      _shp.Layer.Renderer := Self ;

    if ( TGIS_Shape( _shp ).ShapeType = TGIS_ShapeType.Arc ) or
       ( TGIS_Shape( _shp ).ShapeType = TGIS_ShapeType.Polygon ) then
      _shp.Draw ;
    drawEditingPointMarkers( _shp ) ;
    drawEditingEdgeLengths( _shp ) ;
  finally
    inEdit := False ;
    FreeObject( oEditCanvas ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.CreateContext(
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

procedure TGIS_RendererVclDirect2D.CreateContext(
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

procedure TGIS_RendererVclDirect2D.CreateContextEx(
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
  RestoreContextEx ;
end ;

procedure TGIS_RendererVclDirect2D.RestoreContext ;
begin
  Assert( not assigned( oCanvas ) ) ;
  if Assigned( oCanvas ) then exit ;

  if Context.BaseMapOnDemand then
    Context.AssignBaseMap(
      createTransparentBitmap( Width, Height ),
      True
    ) ;

  // sometimes (e.g. when there is only one layer and it is topmost)
  // oCanvas stays unassigned
  if assigned( Context.BaseMap ) then
    oCanvas := TGIS_CanvasInternal.Create(
                 TBitmap( Context.BaseMap )
               )
  else
    oCanvas := nil ;

  FCanvas := oCanvas ;
  oEditCanvas := nil ;
  oTransparentCanvas := nil ;
  sourceShape := nil ;

  iTolerance := 1 ;
  iToleranceSel := 2 ;
end ;

procedure TGIS_RendererVclDirect2D.RestoreContextEx ;
begin
  Assert( not assigned( oCanvas ) ) ;
  if Assigned( oCanvas ) then exit ;

  if not assigned( Context.NativeDrawContext ) then
    RestoreContext
  else
    oCanvas := TGIS_CanvasInternal.Create(
      TD2DCanvas(Context.NativeDrawContext)
    ) ;

  FCanvas := oCanvas ;
  oEditCanvas := nil ;
  oTransparentCanvas := nil ;
  sourceShape := nil ;

  iTolerance := 1 ;
  iToleranceSel := 2 ;
end;

procedure TGIS_RendererVclDirect2D.ReleaseContext ;
begin
  FreeObject( oCanvas ) ;

  inherited ;
end ;

procedure TGIS_RendererVclDirect2D.PrepareHourglassContext ;
begin
  // no action required
end ;

procedure TGIS_RendererVclDirect2D.AfterDraw ;
begin
  if assigned( Context ) then begin
    if assigned( Context.Selection ) then
      FreeObject( oSelectionCanvas )
    else
      oSelectionCanvas := nil ;

    if assigned( Context.Charts ) then
      FreeObject( oChartsCanvas )
    else
      oChartsCanvas := nil ;

    if assigned( Context.Labels ) then
      FreeObject( oLabelsCanvas )
    else
      oLabelsCanvas := nil ;
  end ;

  FreeObject( oCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.LockTransparent(
  const _transparency : Integer
) ;
var
  cl : D3DCOLORVALUE ;
begin
  assert( not assigned( oTransparentCanvas ) ) ;
  if _transparency = 100 then exit ;

  if Assigned( oTransparentBitmap ) then begin
    // delete if size changed
    if ( oTransparentBitmap.Width  <> Width  ) or
       ( oTransparentBitmap.Height <> Height )
    then
      FreeObject( oTransparentBitmap ) ;
  end;

  if not Assigned( oTransparentBitmap ) then begin
    oTransparentBitmap := VCL.Graphics.TBitmap(
                            createTransparentBitmap( Width, Height )
                          ) ;
    VCL.Graphics.TBitmap( oTransparentBitmap ).AlphaFormat
      := TAlphaFormat.afPremultiplied
  end;
  oTransparentCanvas := TGIS_CanvasInternal.Create( oTransparentBitmap ) ;
  cl.r := 0 ;
  cl.g := 0 ;
  cl.b := 0 ;
  cl.a := 0 ;
  oTransparentCanvas.d2dCanvas.RenderTarget.Clear(cl);

  oTransparentCanvas.transparency := _transparency ;

  FCanvas := oTransparentCanvas ;
end ;

procedure TGIS_RendererVclDirect2D.UnlockTransparent ;
var
  val : Integer ;
begin
  if assigned( oTransparentCanvas ) then begin
    val := oTransparentCanvas.transparency ;

    FreeObject( oTransparentCanvas ) ;
    if oCanvas.ownsCanvas = False then
      TD2DCanvas(oCanvas.d2dCanvas).DrawBitmap( oTransparentBitmap, val )
    else begin
      oCanvas.EndDraw ;
      blendBitmaps( oTransparentBitmap,
                    Context.BaseMap,
                    val,
                    True
                  ) ;
      oCanvas.BeginDraw ;
    end;
  end ;
  FCanvas := oCanvas ;
end ;

procedure TGIS_RendererVclDirect2D.RenderShape(
  const _shp           : TObject ;
  const _selectionOnly : Boolean ;
  const _outlineMode   : TGIS_RendererMultipassMode
) ;
begin
  RenderShape( _shp, nil, _selectionOnly ) ;
end ;

procedure TGIS_RendererVclDirect2D.RenderShape(
  const _shp           : TObject ;
  const _source        : TObject ;
  const _selectionOnly : Boolean ;
  const _outlineMode   : TGIS_RendererMultipassMode
) ;
var
  cnv : TGIS_CanvasInternal ;
begin
  if _shp = nil then exit ;
  if not inEdit and not assigned( oCanvas ) then exit ;

  cnv := FCanvas ;

  if assigned( oTransparentCanvas ) then
    FCanvas := oTransparentCanvas
  else
    FCanvas := oCanvas ;
  try
    case TGIS_Shape( _shp ).ShapeType of
      TGIS_ShapeType.Point       : doShapePoint(
                                     TGIS_ShapePoint( _shp    ),
                                     TGIS_ShapePoint( _source ),
                                     _selectionOnly
                                   ) ;
      TGIS_ShapeType.MultiPoint  : doShapeMultiPoint(
                                     TGIS_ShapeMultiPoint( _shp    ),
                                     TGIS_ShapeMultiPoint( _source ),
                                     _selectionOnly
                                   ) ;
      TGIS_ShapeType.Arc         : doShapeLine(
                                     TGIS_ShapeArc( _shp    ),
                                     TGIS_ShapeArc( _source ),
                                     _selectionOnly,
                                     _outlineMode
                                   ) ;
      TGIS_ShapeType.Polygon     : doShapePolygon(
                                     TGIS_ShapePolygon( _shp    ),
                                     TGIS_ShapePolygon( _source ),
                                     _selectionOnly
                                   ) ;
      TGIS_ShapeType.MultiPatch  : doShapePolygon(
                                     TGIS_ShapeMultiPatch( _shp    ),
                                     TGIS_ShapeMultiPatch( _source ),
                                     _selectionOnly
                                   ) ;
      TGIS_ShapeType.Complex     : doShapeComplex(
                                     TGIS_ShapeComplex( _shp    ),
                                     TGIS_ShapeComplex( _source ),
                                     _selectionOnly,
                                     _outlineMode
                                   ) ;
      else begin
        Assert( False, GIS_RS_ERR_UNTESTED ) ;
      end ;
    end ;
  finally
    FCanvas := cnv ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.RenderEditor(
  const _context : TObject
) ;
var
  tb   : TBitmap ;
  sz_f : D2D_SIZE_F ;
  sz_u : D2D_SIZE_U ;
begin
  if Assigned( Viewer ) and Viewer.Editor.InEdit then begin
    if assigned( Viewer.Editor.CurrentShape ) then begin
      // ensure proper renderer context
      if assigned( TGIS_Shape( Viewer.Editor.CurrentShape ).Layer ) then begin
        TGIS_Shape( Viewer.Editor.CurrentShape ).Layer.Renderer := Self ;

        if _context is TD2DCanvas then begin
          TD2DCanvas(_context).RenderTarget.GetSize( sz_f ) ;
          TD2DCanvas(_context).RenderTarget.GetPixelSize( sz_u ) ;

          tb := VCL.Graphics.TBitmap(
                  createTransparentBitmap( sz_u.width, sz_u.height )
                ) ;
          try
            drawEditingPoints( tb, TGIS_Shape( Viewer.Editor.CurrentShape ) ) ;
            TD2DCanvas(_context).StretchBitmapPremultiplied(
              Rect( 0, 0, Round( sz_f.width ), Round( sz_f.height ) ),
              tb
            ) ;
          finally
            FreeObject( tb )
          end ;
        end ;

        if _context is TBitmap then
          drawEditingPoints( TBitmap( _context ),
                             TGIS_Shape( Viewer.Editor.CurrentShape ) ) ;
      end;
    end ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.PaintExtra(
  const _sender  : TObject ;
  const _context : TObject ;
  const _event   : TGIS_RendererEvent
) ;
var
  ocnv : TGIS_CanvasInternal ;
  fcnv : TGIS_CanvasInternal ;
  osel : TGIS_CanvasInternal ;
  parent : IGIS_ViewerParent ;
  bmp  : TBitmap ;
  rd_parent : IGIS_ViewerParent ;
  rd_viewer : IGIS_Viewer ;
  rd_contxt : TGIS_RendererContext ;
  rd_shift  : TPoint ;
  rd_width  : Integer ;
  rd_height : Integer ;
  rd_ppi    : Integer ;
  rd_fscale : Integer ;

  procedure save_context ;
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
  end ;

  procedure prepare_context ;
  var
    fs : Integer ;
  begin
    if ( _context is TD2DCanvas ) then begin
      parent := TComponent(_sender) as IGIS_ViewerParent ;
      save_context ;

      fs := FontScale ;
      if fs = 0 then fs := 100 ;

      inherited CreateContext( parent, Viewer, Context, Point(0,0),
                               parent.ControlCanvasWidth,
                               parent.ControlCanvasHeight,
                               parent.ControlPPI, fs ) ;
      bmp := TBitmap( createTransparentBitmap( Width, Height ) ) ;
      FCanvas := TGIS_CanvasInternal.Create( bmp ) ;
    end else if ( _context is TBitmap ) then begin
      FCanvas := TGIS_CanvasInternal.Create( TBitmap( _context ) ) ;
    end ;
  end;

  procedure release_context ;
  begin
    if ( _context is TD2DCanvas ) then begin
      restore_context ;
      FreeObject( FCanvas ) ;
      TD2DCanvas(_context).DrawBitmapPremultiplied( 0, 0, bmp );
      FreeObject( bmp ) ;
    end else if ( _context is TBitmap ) then begin
      FreeObject( FCanvas ) ;
    end ;
  end;

begin
  if assigned( _event ) then begin
    ocnv := oCanvas ;
    fcnv := FCanvas ;
    osel := oSelectionCanvas ;
    prepare_context ;
    try
      pextra := True ;
      oCanvas := FCanvas ;
      oSelectionCanvas := nil ;
      _event( _sender, Self, TGIS_DrawMode.All ) ;
    finally
      pextra := False ;
      release_context ;
      oCanvas := ocnv ;
      FCanvas := fcnv ;
      oSelectionCanvas := osel ;
    end ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.RenderLabel(
  const _shp : TObject
) ;
var
  cnv : TGIS_CanvasInternal ;
  arr : TGIS_DrawBuf ;
begin
  prepareLabelsCanvas ;

  cnv := FCanvas ;
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
          Assert( False, GIS_RS_ERR_UNTESTED ) ;
    end ;
  finally
    FCanvas := cnv ;
  end ;
end;

procedure TGIS_RendererVclDirect2D.RenderLabel
( const _shp    : TObject ;
  var   _points : TGIS_DrawBuf
) ;
var
  cnv : TGIS_CanvasInternal ;
begin
  prepareLabelsCanvas ;

  cnv := FCanvas ;
  try
    FCanvas := oLabelsCanvas ;
    SetLength( _points, 0 ) ;
    case TGIS_Shape( _shp ).ShapeType of
      TGIS_ShapeType.Point       :
          doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
      TGIS_ShapeType.MultiPoint  :
          doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
      TGIS_ShapeType.Arc         :
          doLabelArc( TGIS_ShapeArc( _shp ), True, _points ) ;
      TGIS_ShapeType.Polygon     :
          doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
      TGIS_ShapeType.MultiPatch  :
          doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
      TGIS_ShapeType.Complex  :
          doLabelPoint( TGIS_Shape( _shp ), True, _points ) ;
      else
          Assert( False, GIS_RS_ERR_UNTESTED ) ;
    end ;
  finally
    FCanvas := cnv ;
  end ;
end ;


procedure TGIS_RendererVclDirect2D.RenderChart(
  const _shp : TObject
) ;
var
  cnv : TGIS_CanvasInternal ;
begin
  prepareChartsCanvas ;

  cnv := FCanvas ;
  try
    FCanvas := oChartsCanvas ;
    doChart( TGIS_Shape( _shp ) ) ;
  finally
    FCanvas := cnv ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.RenderShapeFlashed(
  const _shp : TObject
) ;
begin
  flashed := true ;
  try
    RenderShape( _shp, True ) ;
  finally
    flashed := false ;
  end;
end ;

function TGIS_RendererVclDirect2D.RenderBitmapBegin
  : TObject ;
begin
  Result := nil ;
end ;

procedure TGIS_RendererVclDirect2D.RenderBitmapEnd(
  const _handle : TObject
) ;
begin
end ;

procedure TGIS_RendererVclDirect2D.RenderBitmap(
  const _handle : TObject ;
  const _bmp    : TGIS_Pixels ;
  const _size   : TPoint  ;
  const _dst    : TRect   ;
  const _format : TGIS_BitmapFormat ;
  const _order  : TGIS_BitmapLinesOrder
) ;
var
  tbmp : TBitmap ;
  cnv  : TGIS_CanvasInternal ;
begin
  if assigned( oTransparentCanvas ) then begin
    tbmp := oTransparentBitmap ;
    cnv := oTransparentCanvas ;
  end
  else
  if Assigned( Context ) then begin
    tbmp := TBitmap( Context.BaseMap ) ;
    cnv := FCanvas ;
  end
  else
    exit ;

  if not assigned( tbmp ) then begin
    TD2DCanvas(cnv.d2dCanvas).StretchBitmap( _dst, _bmp, _size ) ;
  end else begin
    cnv.EndDraw ;
    renderBitmapInternal( _handle, tbmp, _bmp, _size, _dst, _format, _order, False ) ;
    cnv.BeginDraw ;
  end;
end ;

procedure TGIS_RendererVclDirect2D.RenderBitmap(
  const _handle    : TObject ;
  const _bmp       : TGIS_Bitmap ;
  const _dst       : TRect ;
  const _antialias : Boolean
) ;
var
  cnv : TGIS_CanvasInternal ;
begin
  if assigned( oTransparentCanvas ) then
    cnv := oTransparentCanvas
  else
    cnv := FCanvas ;

  TBitmap( _bmp.NativeBitmap ).AlphaFormat := TAlphaFormat.afPremultiplied ;
  {$IFNDEF LEVEL_XE6_VCL}
    cnv.stretchDrawFix( _dst, TBitmap( _bmp.NativeBitmap ) );
  {$ELSE}
    cnv.d2dCanvas.StretchDraw( _dst, TBitmap( _bmp.NativeBitmap ) ) ;
  {$ENDIF}
end ;

function TGIS_RendererVclDirect2D.PrepareBitmapCache(
  const _bmp      : TGIS_Pixels ;
  const _extent   : TGIS_Extent ;
  const _size     : TPoint      ;
  const _serial   : Integer     ;
  const _format   : TGIS_BitmapFormat ;
  const _order    : TGIS_BitmapLinesOrder
) : TGIS_RendererAbstractCache ;
var
  cnv : TGIS_CanvasInternal ;
  bmp : TGIS_Bitmap ;
begin
  Result := nil ;

  if assigned( oTransparentCanvas ) then
    cnv := oTransparentCanvas
  else
    cnv := FCanvas ;

  if not assigned( cnv ) then exit ;

  Result := TGIS_RendererVclDirect2DCache.Create( _extent, _size, _serial );

  bmp := TGIS_Bitmap.Create( _size.X, _size.Y ) ;

  VCL.Graphics.TBitmap( bmp.NativeBitmap ).AlphaFormat
    := TAlphaFormat.afPremultiplied ;
  renderBitmapInternal(
    nil,
    VCL.Graphics.TBitmap( bmp.NativeBitmap ),
    _bmp,
    _size,
    Rect( 0, 0, _size.X, _size.Y ),
    _format,
    _order,
    True
  ) ;

  {$IFNDEF LEVEL_XE6_VCL}
    TGIS_RendererVclDirect2DCache( Result ).oD2D1Bitmap
      := cnv.createBitmapFix( VCL.Graphics.TBitmap( bmp.NativeBitmap ) );
  {$ELSE}
    TGIS_RendererVclDirect2DCache( Result ).oD2D1Bitmap
      := cnv.d2dCanvas.CreateBitmap( VCL.Graphics.TBitmap( bmp.NativeBitmap ) );
  {$ENDIF}

  bmp.Free ;
end;

procedure TGIS_RendererVclDirect2D.RenderBitmapCache(
  const _handle   : TObject ;
  const _cache    : TGIS_RendererAbstractCache ;
  const _dst      : TRect
) ;
var
  cnv : TGIS_CanvasInternal ;
  rct : TD2DRectF;
begin
  if assigned( oTransparentCanvas ) then
    cnv := oTransparentCanvas
  else
    cnv := FCanvas ;

  rct.Left   := _dst.Left  ;
  rct.Right  := _dst.Right ;
  rct.Top    := _dst.Top;
  rct.Bottom := _dst.Bottom;

  cnv.d2dCanvas.RenderTarget.DrawBitmap(
    TGIS_RendererVclDirect2DCache( _cache ).oD2D1Bitmap,
    @rct,
    1
  );
end;

procedure TGIS_RendererVclDirect2D.Update ;
begin
end ;

procedure TGIS_RendererVclDirect2D.Flush ;
begin
  oCanvas.EndDraw ;
  oCanvas.BeginDraw ;
end ;

function TGIS_RendererVclDirect2D.CanvasNative
  : TObject ;
begin
  assert( assigned( FCanvas ) ) ;

  Result := FCanvas.d2dCanvas ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasFontMetrics(
  var _break_char : Char    ;
  var _height     : Integer ;
  var _ascent     : Integer ;
  var _true_type  : Boolean
) ;
begin
  assert( assigned( FCanvas ) ) ;
end ;

function TGIS_RendererVclDirect2D.CanvasTextExtent(
  const _text : String
) : TPoint ;
var
  siz : TSize ;
begin
  assert( assigned( FCanvas ) ) ;
  if FCanvas.Font.Size = 0 then begin
    Result := Point( 0, 0 ) ;
    exit ;
  end ;

  prepareFont(
    FCanvas,
    FCanvas.Font.Name,
    FCanvas.Font.Size,
    FCanvas.Font.Style,
    FCanvas.Font.Color
  ) ;
  siz := FCanvas.d2dCanvas.TextExtent( _text ) ;
  Result := TPoint.Create( siz.Width, siz.Height ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawText(
  const _rect : TRect ;
  const _text : String
) ;
begin
  assert( assigned( FCanvas ) ) ;
  if FCanvas.Font.Size = 0 then exit ;

  prepareFont(
    FCanvas,
    FCanvas.Font.Name,
    FCanvas.Font.Size,
    FCanvas.Font.Style,
    FCanvas.Font.Color
  ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Font.Color,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0)
  ) ;
  drawText( _text, _rect.Left, _rect.Top, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawLine(
  const _x1 : Integer ;
  const _y1 : Integer ;
  const _x2 : Integer ;
  const _y2 : Integer
) ;
begin
  assert( assigned( FCanvas ) ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    FCanvas.Pen.LineCap,
    FCanvas.Pen.LineJoin,
    FCanvas.Pen.Width
  ) ;

  drawLine( _x1, _y1, _x2, _y2, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolyLine(
  const _points : TGIS_DrawBuf
) ;
begin
  CanvasDrawPolyLine( _points, length( _points ) ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolyLine(
  const _points : TGIS_DrawBufF
) ;
begin
  CanvasDrawPolyLine( _points, length( _points ) ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolyLine(
  const _points : TGIS_DrawBuf ;
  const _count  : Integer
) ;
var
  ar : TGIS_IntegerArray ;
begin
  SetLength( ar, 1 ) ;
  ar[0] := _count ;
  CanvasDrawPolyLine( _points, ar ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolyLine(
  const _points : TGIS_DrawBufF ;
  const _count  : Integer
) ;
var
  ar : TGIS_IntegerArray ;
begin
  SetLength( ar, 1 ) ;
  ar[0] := _count ;
  CanvasDrawPolyLine( _points, ar ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolyLine(
  const _points : TGIS_DrawBuf ;
  const _parts  : TGIS_IntegerArray
) ;
begin
  assert( assigned( FCanvas ) ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    FCanvas.Pen.LineCap,
    FCanvas.Pen.LineJoin,
    FCanvas.Pen.Width
  ) ;
  drawPolyPolyline( _points, _parts, length(_parts), FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolyLine(
  const _points : TGIS_DrawBufF ;
  const _parts  : TGIS_IntegerArray
) ;
begin
  assert( assigned( FCanvas ) ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    FCanvas.Pen.LineCap,
    FCanvas.Pen.LineJoin,
    FCanvas.Pen.Width
  ) ;
  drawPolyPolyline( _points, _parts, length(_parts), FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawRectangle(
  const _rect : TRect
) ;
begin
  assert( assigned( FCanvas ) ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Square,
    TGIS_LineJoin.Miter,
    FCanvas.Pen.Width
  ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  drawRectangle( Min(_rect.Left, _rect.Right),
                 Min(_rect.Top, _rect.Bottom),
                 Max(_rect.Left, _rect.Right),
                 Max(_rect.Top, _rect.Bottom),
                 FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawEllipse(
  const _x      : Integer ;
  const _y      : Integer ;
  const _width  : Integer ;
  const _height : Integer
) ;
begin
  assert( assigned( FCanvas ) ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    TGIS_LineJoin.Round,
    FCanvas.Pen.Width
  ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  drawEllipse( _x, _y, _x + _width, _y + _height, FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawArc(
  const _x          : Integer ;
  const _y          : Integer ;
  const _width      : Integer ;
  const _height     : Integer ;
  const _startX     : Integer ;
  const _startY     : Integer ;
  const _endX       : Integer ;
  const _endY       : Integer
) ;
begin
  assert( assigned( FCanvas ) ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    TGIS_LineJoin.Round,
    FCanvas.Pen.Width
  ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  drawArc( _x, _y, _x + _width, _y + _height, _startX, _startY, _endX, _endY, FCanvas ) ;
end;

procedure TGIS_RendererVclDirect2D.CanvasDrawArc(
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
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    TGIS_LineJoin.Round,
    FCanvas.Pen.Width
  ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  drawArc( _x, _y, _radius, _startAngle, _sweepAngle, FCanvas ) ;
end;

procedure TGIS_RendererVclDirect2D.CanvasDrawPie(
  const _angle_0  : Double  ;
  const _angle_1  : Double  ;
  const _radius   : Integer ;
  const _origin_x : Integer ;
  const _origin_y : Integer
) ;
const
  LOCAL_CIRCLE_STEPS : Integer = 36 ;
var
  hr : HRESULT ;
  pg : ID2D1PathGeometry ;
  gs : ID2D1GeometrySink ;
  pt : TD2D1Point2F ;
  i  : Integer ;

  a0 : Double ;
  a1 : Double ;
  ai : Double ;
  si : Integer ;
  r  : Double ;
  rs : Double ;
  rc : Double ;
begin
  assert( assigned( FCanvas ) ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    TGIS_LineJoin.Round,
    FCanvas.Pen.Width
  ) ;

  a0 := _angle_0 ;
  a1 := _angle_1 ;
  ai := _angle_1 - _angle_0 ;
  si := Ceil( ai*LOCAL_CIRCLE_STEPS/( 2*PI ) ) ;
  ai := ai/si ;
  r  := _radius div 2 ;

  hr := D2DFactory.CreatePathGeometry( pg ) ;
  hr := pg.Open( gs ) ;
  gs.SetSegmentFlags( D2D1_PATH_SEGMENT_FORCE_ROUND_LINE_JOIN ) ;
  pt.x := _origin_x + GIS_D2D1_PIXEL_SHIFT ;
  pt.y := _origin_y + GIS_D2D1_PIXEL_SHIFT ;
  gs.BeginFigure( pt, D2D1_FIGURE_BEGIN_FILLED ) ;
  for i := 0 to si do begin
    a1 := a0 + i*ai ;
    SinCos( a1, rs, rc ) ;
    pt.x := _origin_x + GIS_D2D1_PIXEL_SHIFT + r*rs ;
    pt.y := _origin_Y + GIS_D2D1_PIXEL_SHIFT - r*rc ;
    gs.AddLine( pt ) ;
  end ;
  gs.EndFigure( D2D1_FIGURE_END_CLOSED ) ;
  hr := gs.Close ;

  if FCanvas.isToBeFilled then begin
    T_Brush(FCanvas.D2DBrush).SelectBrush( Self, FCanvas ) ;
    FCanvas.d2dCanvas.RenderTarget.FillGeometry(
      pg,
      FCanvas.d2dBrushBrush
    ) ;
  end ;

  if FCanvas.isToBeDrawn then begin
    T_Pen(FCanvas.D2DPen).SelectPen( Self, FCanvas ) ;
    FCanvas.d2dCanvas.RenderTarget.DrawGeometry(
      pg,
      FCanvas.d2dPenBrush,
      T_Pen(FCanvas.D2DPen).Width,
      FCanvas.d2dPenStyle
    ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolygon(
  const _points : TGIS_DrawBuf
) ;
begin
  CanvasDrawPolygon( _points, length(_points) );
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolygon(
  const _points : TGIS_DrawBufF
) ;
begin
  CanvasDrawPolygon( _points, length(_points) );
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolygon(
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

procedure TGIS_RendererVclDirect2D.CanvasDrawPolygon(
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

procedure TGIS_RendererVclDirect2D.CanvasDrawPolygon(
  const _points : TGIS_DrawBuf ;
  const _parts  : TGIS_IntegerArray
) ;
begin
  assert( assigned( FCanvas ) ) ;
  create_geometry := True ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    FCanvas.Pen.LineJoin,
    FCanvas.Pen.Width
  ) ;
  drawPolyPolygon( _points, _parts, length(_parts), FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawPolygon(
  const _points : TGIS_DrawBufF ;
  const _parts  : TGIS_IntegerArray
) ;
begin
  assert( assigned( FCanvas ) ) ;
  prepareBrush(
    FCanvas,
    FCanvas.Brush.Color,
    nil,
    FCanvas.Brush.Style,
    Point(0, 0)
  ) ;
  preparePen(
    FCanvas,
    FCanvas.Pen.Color,
    FCanvas.Pen.Style,
    nil,
    TGIS_BrushStyle.Solid,
    Point(0, 0),
    TGIS_LineCap.Round,
    FCanvas.Pen.LineJoin,
    FCanvas.Pen.Width
  ) ;
  drawPolyPolygon( _points, _parts, length(_parts), FCanvas ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawBitmap(
  const _bmp      : TGIS_Pixels ;
  const _size     : TPoint  ;
  const _dst      : TRect   ;
  const _format   : TGIS_BitmapFormat ;
  const _order    : TGIS_BitmapLinesOrder
) ;
var
  bmp : TGIS_Bitmap ;
  px  : TGIS_Pixels ;
begin
  bmp := TGIS_Bitmap.Create( _size.X, _size.Y ) ;
  try
    bmp.LockPixels( px, True, _format, _order ) ;
    try
      Move( _bmp[0], px[0], Length( _bmp ) * 4 ) ;
    finally
      bmp.UnlockPixels ;
    end;
    CanvasDrawBitmap( bmp, _dst ) ;
  finally
    FreeObject( bmp ) ;
  end ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasDrawBitmap(
  const _bmp       : TGIS_Bitmap       ;
  const _dst       : TRect
) ;
begin
  RenderBitmap( nil, _bmp, _dst, True ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasSetTransformation(
  const _angle    : Double  ;
  const _origin_x : Integer ;
  const _origin_y : Integer
) ;
var
  mtx  : TD2D1Matrix3x2F ;
  ssin : Double  ;
  scos : Double  ;
begin
  assert( assigned( FCanvas ) ) ;
  assert( assigned( FCanvas.d2dCanvas ) ) ;

  FCanvas.d2dCanvas.RenderTarget.GetTransform( storedTransform ) ;

  SinCos( _angle, ssin, scos ) ;

  mtx._11 :=  scos ;
  mtx._12 :=  ssin ;
  mtx._21 := -ssin ;
  mtx._22 :=  scos ;
  mtx._31 := _origin_x ;
  mtx._32 := _origin_y ;

  FCanvas.d2dCanvas.RenderTarget.SetTransform( storedTransform * mtx ) ;
end ;

procedure TGIS_RendererVclDirect2D.CanvasClearTransformation ;
begin
  assert( assigned( FCanvas ) ) ;
  assert( assigned( FCanvas.d2dCanvas ) ) ;

  FCanvas.d2dCanvas.RenderTarget.SetTransform( storedTransform ) ;
end ;

{$ENDREGION}

{$IFDEF LEVEL_RX103_DIRECT2DFIX}
  // Hack to fix RSP-21822 bug causing mutithreading problems.

  //this is the implementation of the new function
  var
    SingletonD2DFactoryHijack : ID2D1Factory;

  function D2DFactoryHijack(
    factoryType    : TD2D1FactoryType    = D2D1_FACTORY_TYPE_SINGLE_THREADED;
    factoryOptions : PD2D1FactoryOptions = nil
  ) : ID2D1Factory;
  var
    LD2DFactory: ID2D1Factory;
  begin
    if SingletonD2DFactoryHijack = nil then
    begin
      D2D1CreateFactory(factoryType, IID_ID2D1Factory, factoryOptions, LD2DFactory);
      if InterlockedCompareExchangePointer(
           Pointer(SingletonD2DFactoryHijack),
           Pointer(LD2DFactory),
           nil
         ) = nil
      then
        LD2DFactory._AddRef;
    end;
    Result := SingletonD2DFactoryHijack;
  end;

  var
    oD2DFactoryHijack : TGIS_Hijack = nil ;
{$ENDIF}

procedure TGIS_RendererVclDirect2DCache.doDestroy ;
begin
  ReleaseInterface( oD2D1Bitmap ) ;
  inherited ;
end ;

procedure doInitialization ;
begin
  bD2DSupportCheck := -1 ;

  if IsWine then
    exit ;
  try
    {$IFDEF LEVEL_RX103_DIRECT2DFIX}
      oD2DFactoryHijack := TGIS_Hijack.Create(
        @D2DFactory,
        @D2DFactoryHijack
      ) ;
      if (Win32MajorVersion >= 6) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        // use hijack name because hijack is still not active here
        D2DFactoryHijack(D2D1_FACTORY_TYPE_MULTI_THREADED, nil) ;
    {$ELSE}
      if (Win32MajorVersion >= 6) and (Win32Platform = VER_PLATFORM_WIN32_NT) then
        D2DFactory(D2D1_FACTORY_TYPE_MULTI_THREADED, nil) ;
    {$ENDIF}
    bD2DSupportCheck := 0 ;
  except
  end;
end;

procedure doFinalization ;
begin
  {$IFNDEF GIS_NODIRECT2D}
    {$IFDEF LEVEL_RX103_DIRECT2DFIX}
      FreeObject( oD2DFactoryHijack );
    {$ENDIF}
  {$ENDIF}
end;

initialization
  {$IFNDEF GIS_NODIRECT2D}
    doInitialization ;
    RegisterRenderer( 'TGIS_RendererVclDirect2D', TGIS_RendererVclDirect2D ) ;
  {$ENDIF}
finalization
  doFinalization ;


//==================================== END =====================================
end.

