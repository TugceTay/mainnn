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
  VCL renderer.
}

unit Lider.CG.GIS.VCL.GeoRendererGdi32;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoRendererGdi32"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.Graphics,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoRendererAbstract,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.VCL.GeoRenderer;

type

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Encapsulate all objects connected with the native GDI32 canvas.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  TGIS_CanvasInternal = class( TGIS_ObjectDisposable )
    private
      Canvas     : TCanvas    ;
      Font       : TGIS_Font  ;
      Pen        : TGIS_Pen   ;
      Brush      : TGIS_Brush ;

      VclPen     : TObject    ;
      VclBrush   : TObject    ;
      VclFont    : TObject    ;

      oldGraphicsMode
                 : Integer    ;
      oldXform   : TXForm     ;

      transparency : Integer  ;
      usePen       : Boolean  ;
      useBrush     : Boolean  ;

    protected
      procedure doDestroy ; override ;

    public

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_canvas">
      ///   existing VCL canvas object to which object will be associated
      /// </param>
      constructor Create( const _canvas : TCanvas ) ;
  end ;

  /// <summary>
  ///   Renderer for VCL Gdi32.
  /// </summary>
  TGIS_RendererVclGdi32 = class( TGIS_RendererVclAbstract )
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
      ///   Transparency bitmap context .
      /// </summary>
      oTransparentBitmap : TBitmap ;

      /// <summary>
      ///   Transparency canvas encapsulation.
      /// </summary>
      oTransparentCanvas : TGIS_CanvasInternal ;

      /// <summary>
      ///   Selection color with transparency altered for topmost layers.
      /// </summary>
      colorSelection     : TGIS_Color ;

      /// <summary>
      ///   Canvas bitmap, mainly for PaintExtra context.
      /// </summary>
      oCanvasBitmap  : TBitmap ;

      /// <summary>
      ///   Reference to the current canvas encapsulation.
      /// </summary>
      FCanvas            : TGIS_CanvasInternal ;

    private
      procedure prepareSelectionCanvas
                                 ( _transparently : Boolean ;
                                   _useBaseMap    : Boolean
                                 ) ;
      procedure prepareChartsCanvas ;
      procedure prepareLabelsCanvas ;

    private
      iTolerance    : Integer ;
      iToleranceSel : Integer ;
      inEdit        : Boolean ;
      flashed       : Boolean ;
      pextra        : Boolean ;

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

      function  prepareBitmapFill( const _bmp           : TBitmap
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
      /// <param name="_cap">
      ///   line end cap style
      /// </param>
      /// <param name="_join">
      ///   line jon style
      /// </param>
      /// <param name="_width">
      ///   pen width
      /// </param>
      procedure preparePen       ( const _canvas        : TGIS_CanvasInternal ;
                                   const _color         : TGIS_Color      ;
                                   const _style         : TGIS_PenStyle   ;
                                   const _bitmap        : TGIS_Bitmap     ;
                                   const _pattern       : TGIS_BrushStyle ;
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
      procedure prepareBrush     ( const _canvas        : TGIS_CanvasInternal ;
                                   const _color         : TGIS_Color  ;
                                   const _bitmap        : TGIS_Bitmap ;
                                   const _pattern       : TGIS_BrushStyle
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
      ///   Set brush origin to left-up corner of shape's projected extent.
      /// </summary>
      /// <param name="_shape">
      ///   source shape ( not truncated )
      /// </param>
      procedure setBrushOrigin   ( const _shape         : TObject
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
      /// <param name="_canvas">
      ///   canvas to be drawn on
      /// </param>
      procedure drawPolyPolygon  ( const _points        : TGIS_DrawBuf      ;
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
      ///   Draw a line on the default canvas.
      /// </summary>
      /// <param name="_points">
      ///   array of points to connect
      /// </param>
      /// <param name="_count">
      ///   number of points
      /// </param>
      /// <param name="_canvas">
      ///   canvas to be drawn on
      /// </param>
      procedure drawPolyline     ( const _points        : array of TPoint   ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ; overload;

      procedure drawPolyPolyline ( const _points        : array of TPoint   ;
                                   const _parts         : TGIS_IntegerArray ;
                                   const _count         : Integer           ;
                                   const _canvas        : TGIS_CanvasInternal
                                 ) ;

      procedure drawText         ( const _x             : Integer     ;
                                   const _y             : Integer     ;
                                   const _text          : String      ;
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
      /// <param name="_marker">
      ///   marker - to pass parematrers for really small markers
      /// </param>
      procedure drawMarker       ( const _style         : TGIS_MarkerStyle ;
                                   const _size          : Integer          ;
                                   const _pt            : TPoint           ;
                                   const _selectionOnly : Boolean          ;
                                   const _marker        : TGIS_ParamsMarker
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
      ///   if True only the shapeselection is rendered
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

      procedure drawEditingPoints( const _context       : TCanvas     ;
                                   const _shp           : TGIS_Shape
                                 ) ;

    protected
      procedure doDestroy ; override ;

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

      {#gendoc:hide:GENXDK}
      /// <inheritdoc/>
      procedure   CreatePrinterContext(
                                       const _canvas    : TObject ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer
                                     ) ; overload; override ;

      /// <inheritdoc/>
      procedure   RestoreContext     ; override;

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
                                     ) ; overload ; override;

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
      procedure   RenderEditor       ( const _context  : TObject
                                     ) ; override;

      /// <inheritdoc/>
      procedure   PaintExtra         ( const _sender   : TObject ;
                                       const _context  : TObject ;
                                       const _event    : TGIS_RendererEvent
                                     ) ; override;

      /// <inheritdoc/>
      procedure   Update             ; override;

      /// <inheritdoc/>
      procedure   Flush              ; override;

    // lo-level API
    public

      /// <inheritdoc/>
      /// <returns>
      ///   VCL.Graphics.TCanvas object.
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
      procedure CanvasDrawPolyLine  ( const _points    : TGIS_DrawBuf ;
                                      const _parts     : TGIS_IntegerArray
                                    ) ; override;

      /// <inheritdoc/>
      procedure CanvasDrawPolyLine  ( const _points    : TGIS_DrawBufF ;
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
      procedure   CanvasDrawPolygon  ( const _points   : TGIS_DrawBuf  ;
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

//##############################################################################
implementation

uses
  System.Math,
  {$IFDEF LEVEL_XE6_VCL}
    System.Math.Vectors,
  {$ENDIF}

  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoHtmlLabel,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.GeoChart,
  Lider.CG.GIS.GeoArcLabel,
  Lider.CG.GIS.GeoSymbol ;

const
  PenStyles : array[TPenStyle] of Word =
              ( PS_SOLID, PS_DASH, PS_DOT, PS_DASHDOT, PS_DASHDOTDOT,
                PS_NULL, PS_INSIDEFRAME, PS_USERSTYLE, PS_ALTERNATE
              ) ;

  PenModes  : array[TPenMode]  of Word =
              ( R2_BLACK,       R2_WHITE,
                R2_NOP,         R2_NOT,
                R2_COPYPEN,     R2_NOTCOPYPEN,
                R2_MERGEPENNOT, R2_MASKPENNOT,
                R2_MERGENOTPEN, R2_MASKNOTPEN,
                R2_MERGEPEN,    R2_NOTMERGEPEN,
                R2_MASKPEN,     R2_NOTMASKPEN,
                R2_XORPEN,      R2_NOTXORPEN
              ) ;

const
  // Maximum number of points for pline/polygon draws
  GDI_MAXPOINT_COUNT : Integer = 20000000 ;

type

  // Substitute of a pen object.
  T_Pen = class ( TGIS_ObjectDisposable )
    // properties internal values
    private

      // The Windows pen object handle.
      FHandle     : HPEN ;

      // Color of the pen.
      FColor      : TGIS_Color ;

      // Width of the pen in pixels.
      FWidth      : Integer ;

      // Style in which the pen draws.
      FStyle      : TGIS_PenStyle ;

      // Mode how the pen's color combines with the color on the background.
      FMode       : TPenMode ;

      // Cap style used at the end of a line.
      FLineCap    : TGIS_LineCap ;

      // Join style for the ends of two consecutive lines.
      FLineJoin   : TGIS_LineJoin ;

      FPattern    : TGIS_BrushStyle ;

      FBitmap     : TGIS_Bitmap ;

      FStretchedBitmap : TBitmap ;

      // Set if any setting has changed.
      FChanged    : Boolean ;

      // Set if the pen has been created by CreateExtPen function.
      FExSelected : Boolean ;

    protected  // property access routines

      procedure fset_Color             ( const _value   : TGIS_Color
                                       ) ;
      procedure fset_Width             ( const _value   : Integer
                                       ) ;
      function  fget_Style             : TGIS_PenStyle ;
      procedure fset_Style             ( const _value   : TGIS_PenStyle
                                       ) ;
      procedure fset_Mode              ( const _value   : TPenMode
                                       ) ;
      procedure fset_LineCap           ( const _value   : TGIS_LineCap
                                       ) ;
      procedure fset_LineJoin          ( const _value   : TGIS_LineJoin
                                       ) ;
      procedure fset_Pattern           ( const _value   : TGIS_BrushStyle
                                       ) ;
      procedure fset_Bitmap            ( const _value   : TGIS_Bitmap
                                       ) ;

    protected

      // Destroy an instance.
      procedure doDestroy            ; override;

    public     // public methods

      // Create an instance.
      constructor Create             ;

      // Select the pen object according to earlier settings.
      // Internally uses the Windows GDI CreatePenIndirect method.
      function SelectPen             ( const _dc          : HDC
                                     ) : HPEN ;

      // Select the pen object according to earlier settings.
      // Internally uses the Windows GDI ExtCreatePen method.
      function SelectPenEx           ( const _rnd         : TGIS_RendererVclGdi32 ;
                                       const _dc          : HDC
                                     ) : HPEN ;
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
                                       read  fget_Style
                                       write fset_Style
                                       default TGIS_PenStyle.Solid ;

      // Mode how the pen's color combines with the color on the background.
      property Mode                  : TPenMode
                                       read  FMode
                                       write fset_Mode
                                       default TPenMode.pmCopy ;

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
  end ;

  // Substitute of a brush object.
  T_Brush = class ( TGIS_ObjectDisposable )
    // properties internal values
    private

      // The Windows brush object handle.
      FHandle   : HBRUSH ;

      // Color of the brush.
      FColor    : TGIS_Color ;

      // Pattern for the brush.
      FStyle    : TGIS_BrushStyle ;

      FBitmap   : TGIS_Bitmap ;
      FStretchedBitmap : TBitmap ;

      //Set if any setting has changed.
      FChanged  : Boolean ;

    protected   // property access routines

      procedure fset_Color           ( const _color       : TGIS_Color
                                     ) ;
      function  fget_Style           : TGIS_BrushStyle ;
      procedure fset_Style           ( const _style       : TGIS_BrushStyle
                                     ) ;
      procedure fset_Bitmap          ( const _bitmap      : TGIS_Bitmap
                                     ) ;
    protected

      // Destroy an instance.
      procedure doDestroy            ; override;

    public // public methods

      // Create an instance.
      constructor Create             ;

      // Select the brush object according to earlier settings.
      // Internally uses the Windows GDI CreateBrushIndirect method.
      function SelectBrush           ( const _rnd         : TGIS_RendererVclGdi32 ;
                                       const _dc          : HDC
                                     ) : HBRUSH ;
    public // public properties

      // Color of the brush.
      property Color                 : TGIS_Color
                                       read  FColor
                                       write fset_Color  ;

      // Pattern for the brush.
      property Style                 : TGIS_BrushStyle
                                       read  fget_Style
                                       write fset_Style  ;

     // External bitmap image that defines a pattern for the brush.
      property Bitmap                : TGIS_Bitmap
                                       read  FBitmap
                                       write fset_Bitmap ;
  end ;

  // Substitute of a font object.
  T_Font = class ( TGIS_ObjectDisposable )
    // properties internal values
    private

      // The Windows font object handle
      FHandle         : HFONT ;

      // Typeface of the font.
      FName           : TFontName ;

      // Color of the text.
      FColor          : TGIS_Color ;

      // Height of the font.
      FHeight         : Integer ;

      // Specifies the angle, in tenths of degrees, between each character's
      // base line and the x-axis of the device.
      FOrientation    : Integer ;

      // Style o the font(normal, italic, bold, and so on).
      FStyle          : TFontStyles ;

      // Determines whether the characters
      // in the font all have the same width.
      FPitch          : TFontPitch ;

      // Conversion factor between logical inches
      // and the pixels of the device.
      FPixelsPerInch  : Integer ;

      // Set if any setting has changed.
      FChanged        : Boolean ;

      // Object for Vcl Font.
      FVcl            : TFont ;

    private     // private methods
      procedure do_init              ( const _pixelsPerInch : Integer ) ;

    protected
      st_Name              : TFontName ;
      st_Height            : Integer ;
      st_Orientation       : Integer ;
      st_Style             : TFontStyles ;
      st_CharSet           : TFontCharset ;
      st_PixelsPerInch     : Integer ;
      st_Pitch             : TFontPitch ;
      procedure st_store   ;
      function  st_changed : Boolean ;

    protected   // property access routines

      procedure fset_Name            ( const _name          : TFontName
                                     ) ;
      procedure fset_Color           ( const _color         : TGIS_Color
                                     ) ;
      procedure fset_Orientation     ( const _orientation   : Integer
                                     ) ;
      procedure fset_Style           ( const _style         : TFontStyles
                                     ) ;
      procedure fset_Pitch           ( const _pitch         : TFontPitch
                                     ) ;
      function  fget_Size            : Integer ;
      procedure fset_Size            ( const _size          : Integer
                                     ) ;
      procedure fset_PixelsPerInch   ( const _pixelsPerInch : Integer
                                     ) ;
      function  fget_Vcl             : TFont ;

    protected
      // Destroys an instance.
      procedure doDestroy            ; override;

    public      // public methods

      // Creates an instance.
      constructor Create             ( const _pixelsPerInch : Integer
                                     ) ;

      // Select the font object according to earlier settings.
      // Internally uses the Windows GDI CreateFontIndirect method.
      function SelectFont            ( const _dc            : HDC
                                     ) : HFONT ;

    public      // public properties

      // Typeface of the font.
      property Name                  : TFontName
                                       read  FName
                                       write fset_Name    ;

      // Color of the text.
      property Color                 : TGIS_Color
                                       read  FColor
                                       write fset_Color   ;

      // Height of the font.
      property Height                : Integer
                                       read  FHeight ;

      // Specifies the angle, in tenths of degrees, between each character's
      // base line and the x-axis of the device.
      property Orientation           : Integer
                                       read  FOrientation
                                       write fset_Orientation ;

      // Style oo the font(normal, italic, bold, and so on).
      property Style                 : TFontStyles
                                       read  FStyle
                                       write fset_Style   ;

      // Determines whether the characters
      // in the font all have the same width.
      property Pitch                 : TFontPitch
                                       read  FPitch
                                       write fset_Pitch   ;

      // Height of the font in points.
      property Size                  : Integer
                                       read  fget_Size
                                       write fset_Size    ;

      // Conversion factor between logical inches
      // and the pixels of the device.
      property PixelsPerInch         : Integer
                                       read  FPixelsPerInch
                                       write fset_PixelsPerInch ;

      // Vcl object for font.
      property Vcl                   : TFont
                                       read  fget_Vcl     ;
  end ;


//=============================================================================
// Utilities
//=============================================================================

  /// <summary>
  ///   Convert GDI32 bitmap to ARGB enabled by altering Alpha channel
  /// </summary>
  /// <param name="_bitmap">
  ///   bitmap to be fixed
  /// </param>
  procedure fix_bitmap( const _bitmap : TBitmap  ) ;
  var
    x, y          : Integer ;

    src_lin       : IntPtr ;

    cl            : Cardinal ;
  begin
    Assert( _bitmap.PixelFormat = pf32bit );

    for y := 0 to _bitmap.Height -1  do begin

      src_lin := IntPtr( _bitmap.ScanLine[ y ] ) ;

      for x := 0 to _bitmap.Width -1 do begin
        cl := PCardinal( src_lin )^ ;

        if (cl <> 0) and ( ( cl and $FF000000 ) = 0) then begin
          PCardinal( src_lin )^ :=  $FF000000 or cl ;  // fix for pure GDI32
        end ;

        src_lin := src_lin + 4 ;
      end;
    end;
  end;

//=============================================================================
// TGIS_CanvasInternal
//=============================================================================

  procedure TGIS_CanvasInternal.doDestroy ;
  begin
    FreeObject( VclFont  ) ;
    FreeObject( VclBrush ) ;
    FreeObject( VclPen   ) ;

    FreeObject( Font  ) ;
    FreeObject( Brush ) ;
    FreeObject( Pen   ) ;

    Canvas.Unlock ;

    inherited ;
  end ;

  constructor TGIS_CanvasInternal.Create(
    const _canvas : TCanvas
  ) ;
  begin
    Canvas := _canvas ;
    Canvas.Lock ;

    Pen    := Lider.CG.GIS.GeoTypesUI.TGIS_Pen.Create ;
    Brush  := Lider.CG.GIS.GeoTypesUI.TGIS_Brush.Create ;
    Font   := Lider.CG.GIS.GeoTypesUI.TGIS_Font.Create ;

    VclPen   := T_Pen.Create ;
    VclBrush := T_Brush.Create ;
    VclFont  := T_Font.Create( _canvas.Font.PixelsPerInch ) ;

    transparency := 100  ;
    usePen       := True ;
    useBrush     := True ;
  end ;

//=============================================================================
// TGIS_Pen
//=============================================================================

  constructor T_Pen.Create ;
  begin
    inherited ;

    FColor    := TGIS_Color.Black ;
    FWidth    := 1 ;
    FStyle    := TGIS_PenStyle.Solid ;
    FMode     := TPenMode.pmCopy ;
    FLineCap  := TGIS_LineCap.Round  ;
    FLineJoin := TGIS_LineJoin.Round ;
    FPattern  := TGIS_BrushStyle.Solid ;
    FBitmap   := nil ;
    FStretchedBitmap := nil ;

    FChanged    := False ;
    FExSelected := False ;
    FHandle     := 0 ;
  end ;

  procedure T_Pen.doDestroy ;
  begin
    if FHandle <> 0 then
      DeleteObject( FHandle ) ;
    FreeObject( FStretchedBitmap ) ;
    inherited ;
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

  function  T_Pen.fget_Style
    : TGIS_PenStyle ;
  begin
    Result := FStyle ;
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

  procedure T_Pen.fset_Mode(
    const _value : TPenMode
  ) ;
  begin
    if FMode <> _value then begin
      FMode    := _value ;
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

  function T_Pen.SelectPen( const _dc : HDC ) : HPEN ;
  var
    hpn : HPEN   ;

    function create_pen : HPEN ;
    var
      lp : TLogPen ;
    begin
      lp.lopnStyle   := Cardinal( FStyle ) ;
      lp.lopnWidth.X := FWidth ;
      lp.lopnColor   := VCLColor( FColor );

      Result := CreatePenIndirect( lp );
    end ;

  begin
    hpn := 0 ;
    if ( FHandle = 0 ) or FChanged or FExSelected then begin
      hpn := FHandle ;
      FHandle := create_pen ;
      // delete the old one
      if hpn <> 0 then
        DeleteObject( hpn ) ;
    end ;
    // select object
    SelectObject( _dc, FHandle ) ;
    // pen mode
    SetROP2( _dc, PenModes[FMode] ) ;
    FChanged    := False ;
    FExSelected := False ;
    Result := FHandle ;
  end ;

  function T_Pen.SelectPenEx(
    const _rnd : TGIS_RendererVclGdi32 ;
    const _dc  : HDC
  ) : HPEN ;
  var
    hpn   : HPEN   ;

    function create_pen : HPEN ;
    var
      lb      : TLogBrush ;
      pattern : array of Cardinal ;
      st      : Integer ;
      ftype   : Integer ;
      ecap    : Integer ;
      bmp     : TBitmap ;
    begin
      Result := 0 ;
      case FStyle of
        TGIS_PenStyle.Clear      : st   := PS_NULL       ;
        TGIS_PenStyle.Dash       : st   := PS_DASH       ;
        TGIS_PenStyle.Dot        : st   := PS_DOT        ;
        TGIS_PenStyle.DashDot    : st   := PS_DASHDOT    ;
        TGIS_PenStyle.DashDotDot : st   := PS_DASHDOTDOT ;
        else                       st   := PS_SOLID      ;
      end ;
      case FLineCap of
        TGIS_LineCap.Flat        : ecap := PS_ENDCAP_FLAT ;
        TGIS_LineCap.Square      : ecap := PS_ENDCAP_SQUARE ;
        else                       ecap := PS_ENDCAP_ROUND ;
      end ;
      if (FStyle = TGIS_PenStyle.Dot) and (FWidth <= 1) then
        ftype := PS_GEOMETRIC or PS_USERSTYLE or ecap or PS_JOIN_ROUND
      else
        ftype := PS_GEOMETRIC or st or ecap or PS_JOIN_ROUND ;
      pattern := nil ;
      if (ftype and PS_USERSTYLE) = PS_USERSTYLE then begin
        SetLength( pattern, 2 ) ;
        pattern[0] := 2 ;
        pattern[1] := 3 ;
      end ;

      if ( FStyle = TGIS_PenStyle.Clear ) or ( FWidth = 0 ) then
        Result := CreatePen( PS_NULL, 1, 0 )
      else if assigned( FBitmap ) then begin
        assert( assigned( FBitmap.NativeBitmap ) ) ;
        FreeObject( FStretchedBitmap ) ;
        lb.lbStyle := BS_PATTERN ;
        bmp := _rnd.prepareBitmapFill( TBitmap( FBitmap.NativeBitmap ) ) ;
        if bmp <> TBitmap( FBitmap.NativeBitmap ) then
          FStretchedBitmap := bmp ;
        lb.lbHatch := IntPtr(bmp.Handle) ;
        Result := ExtCreatePen( ftype, FWidth, lb, Length(pattern), pattern ) ;
      end else if ( FStyle = TGIS_PenStyle.Solid ) and ( FWidth = 1 ) then
        Result := CreatePen( PS_SOLID, 1, VCLColor( FColor ) )
      else begin
        case FPattern of
          TGIS_BrushStyle.Clear      : begin
                                         lb.lbStyle := BS_NULL ;
                                         lb.lbHatch := 0 ;
                                       end ;
          TGIS_BrushStyle.Horizontal : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_HORIZONTAL ;
                                       end ;
          TGIS_BrushStyle.Vertical   : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_VERTICAL ;
                                       end ;
          TGIS_BrushStyle.FDiagonal  : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_FDIAGONAL ;
                                       end ;
          TGIS_BrushStyle.BDiagonal  : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_BDIAGONAL ;
                                       end ;
          TGIS_BrushStyle.Cross      : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_CROSS ;
                                       end ;
          TGIS_BrushStyle.DiagCross  : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_DIAGCROSS ;
                                       end ;
          else                         begin
                                         // solid brush
                                         lb.lbStyle := BS_SOLID ;
                                         lb.lbHatch := 0 ;
                                       end ;
        end ;
        lb.lbColor := VCLColor( FColor ) ;
        Result := ExtCreatePen( ftype, FWidth, lb, Length(pattern), pattern ) ;
      end ;
    end ;

  begin

    hpn := 0 ;
    if ( FHandle = 0 ) or FChanged or not FExSelected then
    begin
      hpn := FHandle ;
      FHandle := create_pen ;
      // delete the old one
      if hpn <> 0 then
        DeleteObject( hpn ) ;
    end ;
    // select object
    SelectObject( _dc, FHandle );
    // pen mode
    SetROP2( _dc, PenModes[FMode] ) ;
    FChanged := False ;
    FExSelected := True ;
    Result := FHandle ;
  end ;

//=============================================================================
// TGIS_Brush
//=============================================================================

  constructor T_Brush.Create ;
  begin
    inherited ;

    FColor   := TGIS_Color.White ;
    FStyle   := TGIS_BrushStyle.Solid ;
    FBitmap  := nil ;
    FStretchedBitmap := nil ;
    FChanged := false ;
    FHandle  := 0   ;
  end ;

  procedure T_Brush.doDestroy ;
  begin
    if FHandle <> 0 then
      DeleteObject( FHandle ) ;
    FreeObject( FStretchedBitmap ) ;
    inherited ;
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

  function  T_Brush.fget_Style
    : TGIS_BrushStyle ;
  begin
    Result := FStyle ;
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

  function T_Brush.SelectBrush(
    const _rnd   : TGIS_RendererVclGdi32 ;
    const _dc    : HDC
  ) : HBRUSH ;
  var
    hbr : HBRUSH ;

    function create_brush : HBRUSH ;
    var
      lb : TLogBrush ;
      bmp : TBitmap ;
    begin
      if assigned( FBitmap ) then begin
        assert( assigned( FBitmap.NativeBitmap ) ) ;
        FreeObject( FStretchedBitmap ) ;
        lb.lbStyle := BS_PATTERN ;
        bmp := _rnd.prepareBitmapFill( TBitmap( FBitmap.NativeBitmap ) ) ;
        if bmp <> TBitmap( FBitmap.NativeBitmap ) then
          FStretchedBitmap := bmp ;
        lb.lbHatch := IntPtr(bmp.Handle) ;
        Result := Winapi.Windows.CreateBrushIndirect( lb ) ;
      end else begin
        case FStyle of
          TGIS_BrushStyle.Clear      : begin
                                         lb.lbStyle := BS_NULL ;
                                         lb.lbHatch := 0 ;
                                       end ;
          TGIS_BrushStyle.Horizontal : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_HORIZONTAL ;
                                       end ;
          TGIS_BrushStyle.Vertical   : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_VERTICAL ;
                                       end ;
          TGIS_BrushStyle.FDiagonal  : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_FDIAGONAL ;
                                       end ;
          TGIS_BrushStyle.BDiagonal  : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_BDIAGONAL ;
                                       end ;
          TGIS_BrushStyle.Cross      : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_CROSS ;
                                       end ;
          TGIS_BrushStyle.DiagCross  : begin
                                         lb.lbStyle := BS_HATCHED ;
                                         lb.lbHatch := HS_DIAGCROSS ;
                                       end ;
          else                         begin
                                         // solid brush
                                         lb.lbStyle := BS_SOLID ;
                                         lb.lbHatch := 0 ;
                                       end ;
        end ;
        lb.lbColor := VCLColor( FColor ) ;
        Result := Winapi.Windows.CreateBrushIndirect( lb ) ;
      end ;
    end ;

  begin
    hbr := 0 ;
    if ( FHandle = 0 ) or FChanged then begin
      hbr := FHandle ;
      FHandle := create_brush ;
      if hbr <> 0 then
        DeleteObject( hbr ) ;
    end ;
    SelectObject( _dc, FHandle ) ;
    FChanged := false ;
    Result := FHandle ;
  end ;

//=============================================================================
// TGIS_Font
//=============================================================================

  constructor T_Font.Create( const _pixelsPerInch : Integer ) ;
  begin
    inherited Create ;

    do_init( _pixelsPerInch ) ;
    FHandle := 0 ;
  end ;

  procedure T_Font.doDestroy ;
  begin
    FreeObject( FVcl ) ;

    if FHandle <> 0 then
      DeleteObject( FHandle ) ;

    inherited ;
  end ;

  procedure T_Font.do_init( const _pixelsPerInch : Integer ) ;
  begin
    FName           := 'MS Sans Serif' ;
    FColor          := TGIS_Color.Black ;
    FPixelsPerInch  := _pixelsPerInch ;
    FHeight         := -MulDiv(8, FPixelsPerInch, 72);
    FOrientation    := 0 ;
    FStyle          := [TFontStyle.fsBold] ;
    FPitch          := fpDefault ;
    FChanged        := false ;
    FVcl            := TFont.Create ;
    st_store ;
  end ;

  procedure T_Font.st_store ;
  begin
    st_Name          := FName ;
    st_Height        := FHeight ;
    st_Orientation   := FOrientation ;
    st_Style         := FStyle ;
    st_PixelsPerInch := FPixelsPerInch ;
    st_Pitch         := FPitch ;
  end ;

  function T_Font.st_changed : Boolean ;
  begin
    if ( st_Name          = FName          ) and
       ( st_Height        = FHeight        ) and
       ( st_Orientation   = FOrientation   ) and
       ( st_Style         = FStyle         ) and
       ( st_PixelsPerInch = FPixelsPerInch ) and
       ( st_Pitch         = FPitch         ) then
      Result := False
    else
      Result := True ;
  end ;

  procedure T_Font.fset_Name(
    const _name : TFontName
  ) ;
  begin
    if IsStringEmpty( _name ) then exit ;
    if FName <> _name then begin
      FName    := _name ;
      FChanged := true ;
    end ;
  end ;

  procedure T_Font.fset_Color(
    const _color : TGIS_Color
  ) ;
  begin
    if FColor <> _color then begin
      FColor := _color ;
      FChanged := true ;
    end ;
  end ;

  procedure T_Font.fset_Style(
    const _style : TFontStyles
  ) ;
  begin
    if FStyle <> _style then begin
      FStyle := _style ;
      FChanged := true ;
    end ;
  end ;

  function  T_Font.fget_Size
    : Integer ;
  begin
    Result := -MulDiv( FHeight, 72, FPixelsPerInch ) ;
  end ;

  procedure T_Font.fset_Size(
    const _size : Integer
  ) ;
  var
    newheight : Integer ;
  begin
    newheight := - MulDiv( _size, FPixelsPerInch, 72 ) ;
    if FHeight <> newheight then begin
      FHeight  := newheight ;
      FChanged := true ;
    end ;
  end ;

  procedure T_Font.fset_Orientation(
    const _orientation : Integer
  ) ;
  begin
    if FOrientation <> _orientation then begin
      FOrientation := _orientation ;
      FChanged     := true ;
    end ;
  end ;

  procedure T_Font.fset_Pitch(
    const _pitch : TFontPitch
  ) ;
  begin
    if FPitch <> _pitch then begin
      FPitch   := _pitch ;
      FChanged := true ;
    end ;
  end ;

  procedure T_Font.fset_PixelsPerInch(
    const _pixelsPerInch : Integer
  ) ;
  var
    s : Integer ;
  begin
    if FPixelsPerInch <> _pixelsPerInch then begin
      s := Size ;
      FPixelsPerInch := _pixelsPerInch ;
      FHeight := -MulDiv( s, FPixelsPerInch, 72 ) ;
      FChanged := true ;
    end ;
  end ;

  function T_Font.fget_Vcl
  : TFont ;
  begin
    FVcl.Name          := FName    ;
    FVcl.Size          := Size     ;
    FVcl.PixelsPerInch := FPixelsPerInch ;
    FVcl.Color         := VCLColor( FColor ) ;
    FVcl.Pitch         := FPitch   ;
    FVcl.Style         := FStyle   ;
    FVcl.Orientation   := FOrientation ;
    Result := FVcl ;
  end ;

  function enum_fonts(
    var _lf        : TLogFont    ;
    var _tm        : TTextMetric ;
        _font_type : DWORD       ;
        _data      : LPARAM
    ) : Integer ; stdcall ;
  begin
    TStrings(_data).Add( IntToStr( _lf.lfCharSet ) ) ;
    Result := 1 ;
  end ;

  function T_Font.SelectFont( const _dc : HDC ) : HFONT ;
  var
    hfnt : HFONT  ;
    lf   : TLogFont ;

    procedure setLogFont ;
    var
      i             : Integer ;
      found         : Boolean ;
      new_charset   : Integer ;
      found_charset : Integer ;
      enum_lst      : TStringList ;
    begin
      lf.lfHeight := FHeight;
      lf.lfWidth := 0;
      lf.lfEscapement  := FOrientation ;
      lf.lfOrientation := FOrientation ;
      if TFontStyle.fsBold in Style then lf.lfWeight := FW_BOLD
                         else lf.lfWeight := FW_NORMAL;
      lf.lfItalic    := Byte( TFontStyle.fsItalic    in Style ) ;
      lf.lfUnderline := Byte( TFontStyle.fsUnderline in Style ) ;
      lf.lfStrikeOut := Byte( TFontStyle.fsStrikeOut in Style ) ;
      if SameText( Name, 'Default' ) then StrPCopy( lf.lfFaceName,
                                                    String(DefFontData.Name)
                                                  )
                                     else StrPCopy( lf.lfFaceName, Name ) ;
      lf.lfQuality   := NONANTIALIASED_QUALITY ;
      if lf.lfOrientation <> 0 then lf.lfOutPrecision := OUT_TT_ONLY_PRECIS
                               else lf.lfOutPrecision := OUT_DEFAULT_PRECIS;
      lf.lfClipPrecision := CLIP_DEFAULT_PRECIS;
      case Pitch of
        fpVariable: lf.lfPitchAndFamily := VARIABLE_PITCH ;
        fpFixed:    lf.lfPitchAndFamily := FIXED_PITCH    ;
        else        lf.lfPitchAndFamily := DEFAULT_PITCH;
      end ;

      // check if the defined font charset is available
      // if not then set a possible charset
      new_charset := 1 ;
      enum_lst := TStringList.Create ;
      try
        enum_lst.BeginUpdate ;
        EnumFonts( _dc, lf.lfFaceName, @enum_fonts, NativeInt(enum_lst) );
        enum_lst.EndUpdate ;
        found := False ;
        for i := 0 to enum_lst.Count - 1 do begin
          found_charset := StrToInt( enum_lst.Strings[i] ) ;
          if found_charset = lf.lfCharSet then begin
            found := True ;
            break ;
          end
          else
            new_charset := found_charset ;
        end ;
        if not found then begin
          lf.lfCharSet := new_charset ;
        end ;
      finally
        FreeObject( enum_lst ) ;
      end ;
    end ;

    function create_font : HFONT ;
    begin
      st_store ;
      setLogFont ;
      Result := Winapi.Windows.CreateFontIndirect( lf ) ;
    end ;

  begin
    if _dc <> 0 then begin
      hfnt := 0 ;
      if FHandle = 0 then
        FHandle := create_font
      else if FChanged then
      begin
        hfnt := FHandle ;
        FHandle := create_font ;
      end ;
      SelectObject( _dc, FHandle ) ;
      SetTextColor( _dc, VCLColor( FColor ) ) ;
      if hfnt <> 0 then
        DeleteObject( hfnt ) ;
      FChanged := false ;
    end ;
    Result := FHandle ;
  end ;

//=============================================================================
// TGIS_RendererVclGd32
//=============================================================================

  procedure TGIS_RendererVclGdi32.prepareSelectionCanvas(
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
                            TBitmap( Context.Selection ).Canvas
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

  procedure TGIS_RendererVclGdi32.prepareChartsCanvas ;
  begin
    if Assigned( oChartsCanvas ) then exit ;

    if Context.ChartsOnDemand then
      Context.AssignCharts(
        createTransparentBitmap( Width, Height ),
        True
      ) ;

    if assigned( Context.Charts ) then begin
      oChartsCanvas  := TGIS_CanvasInternal.Create(
                          TBitmap( Context.Charts ).Canvas
                        ) ;
    end
    else begin
      oChartsCanvas := oCanvas ;
    end;
  end;

  procedure TGIS_RendererVclGdi32.prepareLabelsCanvas ;
  begin
    if Assigned( oLabelsCanvas ) then exit ;

    if Context.LabelsOnDemand then
      Context.AssignLabels(
        createTransparentBitmap( Width, Height ),
        True
      ) ;

    if assigned( Context.Labels ) then begin
      oLabelsCanvas  := TGIS_CanvasInternal.Create(
                          TBitmap( Context.Labels ).Canvas
                        ) ;
    end
    else begin
      oLabelsCanvas := oCanvas ;
    end;
  end;

  function TGIS_RendererVclGdi32.fget_CanvasPen
   : Lider.CG.GIS.GeoTypesUI.TGIS_Pen ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Pen ;
  end ;

  procedure TGIS_RendererVclGdi32.fset_CanvasPen(
    const _value : Lider.CG.GIS.GeoTypesUI.TGIS_Pen
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.Pen := _value ;
  end ;

  function TGIS_RendererVclGdi32.fget_CanvasBrush
    : Lider.CG.GIS.GeoTypesUI.TGIS_Brush ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Brush ;
  end ;

  procedure TGIS_RendererVclGdi32.fset_CanvasBrush(
    const _value : Lider.CG.GIS.GeoTypesUI.TGIS_Brush
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.Brush := _value ;
  end ;

  function  TGIS_RendererVclGdi32.fget_CanvasFont
    : Lider.CG.GIS.GeoTypesUI.TGIS_Font ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Font ;
  end ;

  procedure TGIS_RendererVclGdi32.fset_CanvasFont(
    const _value : Lider.CG.GIS.GeoTypesUI.TGIS_Font
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.Font := _value ;
  end ;

  function TGIS_RendererVclGdi32.fget_Info
    : String ;
  var
    bmp : TBitmap ;
    cnv : TGIS_CanvasInternal ;
  begin
    bmp := TBitmap( createTransparentBitmap(1,1) ) ;
    cnv := TGIS_CanvasInternal.Create( bmp.Canvas ) ;
    try
      Result := '[' + ClassName + ':' + cnv.Canvas.ClassName + ']' ;
    finally
      FreeObject( cnv ) ;
      FreeObject( bmp ) ;
    end;
  end;

  constructor TGIS_RendererVclGdi32.Create ;
  begin
    inherited ;

    brushCache := TGIS_brushCache.Create ;
    flashed := False ;
    pextra  := False ;
  end ;

  procedure TGIS_RendererVclGdi32.doDestroy ;
  begin
    FreeObject( brushCache ) ;

    inherited ;
  end ;

  function TGIS_RendererVclGdi32.CreateInstance
    : TGIS_RendererAbstract ;
  begin
    Result := TGIS_RendererVclGdi32.Create ;
  end ;

  procedure TGIS_RendererVclGdi32.CreateContext(
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

  procedure TGIS_RendererVclGdi32.CreateContext(
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

  procedure TGIS_RendererVclGdi32.CreatePrinterContext(
    const _canvas    : TObject ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _ppi       : Integer ;
    const _fontscale : Integer
  ) ;
  begin
    inherited CreatePrinterContext( _canvas,
                                    _width, _height, _ppi, _fontscale
                                  ) ;
    oCanvas := TGIS_CanvasInternal.Create( TCanvas( _canvas ) ) ;
    FCanvas := oCanvas ;
  end;

  procedure TGIS_RendererVclGdi32.RestoreContext ;
  begin
    Assert( not assigned( oCanvas ) ) ;
    if Assigned( oCanvas ) then exit ;

    if Context.BaseMapOnDemand then
      Context.AssignBaseMap(
        createTransparentBitmap( Width, Height ),
        True
      ) ;

    oCanvas := TGIS_CanvasInternal.Create(
                 TBitmap( Context.BaseMap ).Canvas
               ) ;

    FCanvas := oCanvas ;
    oEditCanvas := nil ;
    oTransparentBitmap := nil ;
    oTransparentCanvas := nil ;

    iTolerance := 1 ;
    iToleranceSel := 2 ;
  end ;

  procedure TGIS_RendererVclGdi32.ReleaseContext ;
  begin
    FreeObject( oSelectionCanvas ) ;

    FreeObject( oChartsCanvas    ) ;

    FreeObject( oLabelsCanvas    ) ;

    FreeObject( oCanvas ) ;

    inherited ;
  end ;

  procedure TGIS_RendererVclGdi32.PrepareHourglassContext ;
  begin
    // no action required
  end ;

  procedure TGIS_RendererVclGdi32.AfterDraw ;
  begin
    if assigned( Context.Selection ) and assigned( oSelectionCanvas ) then
    begin
      oSelectionCanvas.Canvas.Unlock ;
      FreeObject( oSelectionCanvas ) ;
    end
    else oSelectionCanvas := nil ;

    if assigned( Context.Charts ) and assigned( oChartsCanvas ) then
    begin
      oChartsCanvas.Canvas.Unlock ;
      FreeObject( oChartsCanvas ) ;
    end
    else oChartsCanvas := nil ;

    if assigned( Context.Labels ) and assigned( oLabelsCanvas ) then
    begin
      oLabelsCanvas.Canvas.Unlock ;
      FreeObject( oLabelsCanvas ) ;
    end
    else oLabelsCanvas := nil ;
  end ;

  procedure TGIS_RendererVclGdi32.LockTransparent(
    const _transparency : Integer
  ) ;
  begin
    assert( not assigned( oTransparentBitmap ) ) ;
    assert( not assigned( oTransparentCanvas ) ) ;
    if _transparency = 100 then exit ;

    oTransparentBitmap := VCL.Graphics.TBitmap(
                            createTransparentBitmap( Width, Height )
                          ) ;
    oTransparentCanvas := TGIS_CanvasInternal.Create(
                            oTransparentBitmap.Canvas ) ;
    oTransparentCanvas.transparency := _transparency ;
    oTransparentCanvas.Canvas.Lock ;

    FCanvas := oTransparentCanvas ;
  end ;

  procedure TGIS_RendererVclGdi32.UnlockTransparent ;
  begin
    if assigned( oTransparentBitmap ) then begin
      if assigned( oTransparentCanvas ) then begin
        oTransparentCanvas.Canvas.Unlock ;
        blendBitmaps( oTransparentBitmap,
                      Context.BaseMap,
                      oTransparentCanvas.transparency,
                      True
                    ) ;
        FreeObject( oTransparentCanvas ) ;
      end ;
      FreeObject( oTransparentBitmap ) ;
      FCanvas := oCanvas ;
    end ;
  end ;

  function TGIS_RendererVclGdi32.prepareBitmapFill(
    const _bmp : TBitmap
  ) : TBitmap ;
  var
    bmp : TBitmap ;
    i, k : Integer ;
    buf : PByteArray ;
  begin
    if PPI <> 96 then begin
      bmp := TBitmap.Create ;
      bmp.PixelFormat := pf32bit ;
      bmp.Width  := RoundS(_bmp.Width * PPI / 96) ;
      bmp.Height := RoundS(_bmp.Height * PPI / 96) ;
      bmp.AlphaFormat := TAlphaFormat.afPremultiplied ;

      for i := 0 to bmp.Height - 1 do begin
        buf := bmp.ScanLine[i] ;
        for k := 0 to bmp.Width - 1 do begin
          buf[4*k  ] := 0 ;
          buf[4*k+1] := 0 ;
          buf[4*k+2] := 0 ;
          buf[4*k+3] := 0 ;
        end ;
      end ;
      bmp.Canvas.StretchDraw( Rect(0,0,bmp.Width,bmp.Height), _bmp ) ;
      Result := bmp ;
    end else
      Result := _bmp ;
  end ;

  procedure TGIS_RendererVclGdi32.preparePen(
    const _canvas  : TGIS_CanvasInternal ;
    const _color   : TGIS_Color      ;
    const _style   : TGIS_PenStyle   ;
    const _bitmap  : TGIS_Bitmap     ;
    const _pattern : TGIS_BrushStyle ;
    const _cap     : TGIS_LineCap    ;
    const _join    : TGIS_LineJoin   ;
    const _width   : Integer
  ) ;
  var
    cl : TGIS_Color ;
    h, s, l, t : Double ;
  begin
    if _width = 0 then
      T_Pen(_canvas.VclPen).Style := TGIS_PenStyle.Clear
    else
      T_Pen(_canvas.VclPen).Style := _style ;

    if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then begin
      T_Pen(_canvas.VclPen).Bitmap := _bitmap ;
    end
    else begin
      T_Pen(_canvas.VclPen).Bitmap := nil ;
      T_Pen(_canvas.VclPen).Pattern := _pattern ;
    end ;
    T_Pen(_canvas.VclPen).Width    := _width ;
    T_Pen(_canvas.VclPen).LineCap  := _cap ;
    T_Pen(_canvas.VclPen).LineJoin := _join ;

    if _color.ARGB = _color.RenderColor.ARGB then
      cl := TGIS_Color.None
    else
      cl := _color ;
    // patch to make a pseudo transparent color
    if cl.A <> 255 then begin
      cl.ToAHSL( t, h, s, l ) ;

      l := l + (1- l)*(255-t*255)/255 ;

      T_Pen(_canvas.VclPen).Color := TGIS_Color.FromHSL( h, s, l ) ;
    end
    else
      T_Pen(_canvas.VclPen).Color := cl ;
  end ;

  procedure TGIS_RendererVclGdi32.prepareBrush(
    const _canvas  : TGIS_CanvasInternal ;
    const _color   : TGIS_Color  ;
    const _bitmap  : TGIS_Bitmap ;
    const _pattern : TGIS_BrushStyle
  ) ;
  var
    cl : TGIS_Color ;
    h, s, l, t : Double ;
  begin
    if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then begin
      T_Brush(_canvas.VclBrush).Bitmap := _bitmap ;
      T_Brush(_canvas.VclBrush).Style := TGIS_BrushStyle.Solid ;
    end
    else begin
      T_Brush(_canvas.VclBrush).Bitmap := nil ;
      T_Brush(_canvas.VclBrush).Style := _pattern ;
    end ;

    if _color.ARGB = _color.RenderColor.ARGB then
      cl := TGIS_Color.None
    else
      cl := _color ;
    // patch to make a pseudo transparent color
    if cl.A <> 255 then begin
      cl.ToAHSL( t, h, s, l ) ;

      l := l + (1- l)*(255-t*255)/255 ;

      T_Brush(_canvas.VclBrush).Color := TGIS_Color.FromHSL( h, s, l ) ;
    end
    else
      T_Brush(_canvas.VclBrush).Color := cl ;
  end ;

procedure TGIS_RendererVclGdi32.prepareFont(
    const _canvas : TGIS_CanvasInternal ;
    const _name   : String  ;
    const _size   : Integer ;
    const _style  : TGIS_FontStyles ;
    const _color  : TGIS_Color
  ) ;
  begin
    T_Font(_canvas.VclFont).Style := VCLFontStyle( _style ) ;
    T_Font(_canvas.VclFont).Name  := _name  ;
    T_Font(_canvas.VclFont).Size  := Max( 1,
                                          RoundS(
                                            _size *
                                            PPI /
                                            T_Font( _canvas.VclFont ).PixelsPerInch
                                            * FontScale / 100
                                          )
                                        ) ;
    T_Font(_canvas.VclFont).Color := _color ;
  end ;

  procedure TGIS_RendererVclGdi32.setBrushOrigin(
    const _shape : TObject
  ) ;
  var
    ext : TGIS_Extent ;
    rct_left  : Integer ;
    rct_top   : Integer ;
    drct_left : Double ;
    drct_top  : Double ;
    tp        : TPoint  ;
    pp        : PPoint  ;
  begin
    ext := TGIS_Shape(_shape).ProjectedExtent ;
    drct_left := (  ext.XMin + FExtentX ) * FZoom ;
    drct_top  := ( -ext.YMax + FExtentY ) * FZoom ;

    if ( drct_left > -1073741824 ) and  // avoid range errors
       ( drct_top  > -1073741824 )
    then begin
      rct_left := RoundS( drct_left ) ;
      rct_top  := RoundS( drct_top  ) ;
    end
    else begin
      rct_left := RoundS( Max( drct_Left, -1073741824 ))  ;
      rct_top  := RoundS( Max( drct_Top , -1073741824 ))  ;
    end ;

    pp := @tp ;
    Winapi.Windows.SetBrushOrgEx( FCanvas.Canvas.Handle,
                                  rct_left, rct_top, pp ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawRectangle(
    const _x1 : Integer ;
    const _y1 : Integer ;
    const _x2 : Integer ;
    const _y2 : Integer
  ) ;
  begin
    drawRectangle( _x1, _y1, _x2, _y2, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawRectangle(
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  var
    r : TRect ;
    add_pixel  : Integer ;
    old_bkmode : Integer  ;
  begin
    if T_Pen(_canvas.VclPen).Style = TGIS_PenStyle.Clear then
      add_pixel := 1
    else
      add_pixel := 0 ;
    r := Rect( _x1, _y1, _x2+add_pixel+1, _y2+add_pixel+1 ) ;
    if _canvas.useBrush and _canvas.usePen then
    begin
      T_Brush(_canvas.VclBrush).SelectBrush( Self, _canvas.Canvas.Handle ) ;
      T_Pen(_canvas.VclPen).SelectPenEx( Self, _canvas.Canvas.Handle ) ;
      old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT ) ;
      try
        _canvas.Canvas.Rectangle( r ) ;
      finally
        Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, old_bkmode ) ;
      end ;
    end
    else if _canvas.useBrush then
    begin
      _canvas.Canvas.Brush.Handle :=
        T_Brush(_canvas.VclBrush).SelectBrush( Self, _canvas.Canvas.Handle ) ;
      old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT ) ;
      try
        _canvas.Canvas.FillRect( r ) ;
      finally
        Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, old_bkmode ) ;
      end ;
    end
    else if _canvas.usePen then
    begin
      T_Pen(_canvas.VclPen).SelectPenEx( Self, _canvas.Canvas.Handle ) ;
      _canvas.Canvas.MoveTo( r.Left,  r.Top ) ;
      _canvas.Canvas.LineTo( r.Right, r.Top ) ;
      _canvas.Canvas.LineTo( r.Right, r.Bottom ) ;
      _canvas.Canvas.LineTo( r.Left,  r.Bottom ) ;
      _canvas.Canvas.LineTo( r.Left,  r.Top ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawEllipse(
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer
  ) ;
  begin
    drawEllipse( _x1, _y1, _x2, _y2, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawEllipse(
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  var
    old_bkmode  : Integer  ;
  begin
    T_Brush(_canvas.VclBrush).SelectBrush( Self, _canvas.Canvas.Handle ) ;
    T_Pen(_canvas.VclPen).SelectPenEx( Self, _canvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT ) ;
    try
      Winapi.Windows.Ellipse( _canvas.Canvas.Handle,
                              _x1, _y1, _x2, _y2
                            ) ;
    finally
      Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawPolygon(
    const _points : array of TPoint
  ) ;
  var
    old_bkmode  : Integer  ;
  begin
    T_Brush(FCanvas.VclBrush).SelectBrush( Self, FCanvas.Canvas.Handle ) ;
    T_Pen(FCanvas.VclPen).SelectPenEx( Self, FCanvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, TRANSPARENT ) ;
    try
      FCanvas.Canvas.Polygon( _points ) ;
    finally
      Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawPolyPolygon(
    const _points : TGIS_DrawBuf ;
    const _parts  : TGIS_IntegerArray ;
    const _count  : Integer
  ) ;
  begin
    drawPolyPolygon( _points, _parts, _count, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawPolyPolygon(
    const _points : TGIS_DrawBuf ;
    const _parts  : TGIS_IntegerArray ;
    const _count  : Integer ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  var
    old_bkmode  : Integer  ;
  begin
    if not _canvas.usePen then
      T_Pen(_canvas.VclPen).Style := TGIS_PenStyle.Clear ;
    T_Pen(_canvas.VclPen).SelectPenEx( Self, _canvas.Canvas.Handle ) ;
    if not _canvas.useBrush then
      T_Brush(_canvas.VclBrush).Style := TGIS_BrushStyle.Clear ;
    T_Brush(_canvas.VclBrush).SelectBrush( Self, _canvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT ) ;
    try
      Winapi.Windows.PolyPolygon( _canvas.Canvas.Handle,
                                  _points[0], _parts[0], _count
                                ) ;
    finally
      Winapi.Windows.SetBkMode ( _canvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawLine(
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  begin
    T_Pen(_canvas.VclPen).SelectPenEx( Self, _canvas.Canvas.Handle ) ;
    _canvas.Canvas.MoveTo( _x1, _y1 ) ;
    _canvas.Canvas.LineTo( _x2, _y2 ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawPolyline(
    const _points : array of TPoint ;
    const _count  : Integer
  ) ;
  begin
    drawPolyline( _points, _count, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawPolyline(
    const _points : array of TPoint ;
    const _count  : Integer ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  var
    old_bkmode  : Integer  ;
  begin
    T_Pen(_canvas.VclPen).SelectPenEx( Self, _canvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT ) ;
    try
      Winapi.Windows.Polyline( _canvas.Canvas.Handle,
                               _points[0], _count
                             ) ;
    finally
      Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawPolyPolyline(
    const _points : array of TPoint ;
    const _parts  : TGIS_IntegerArray ;
    const _count  : Integer ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  var
    old_bkmode  : Integer  ;
    old_bkcolor : COLORREF ;
  begin
    T_Pen(_canvas.VclPen).SelectPen( _canvas.Canvas.Handle ) ;
    T_Brush(_canvas.VclBrush).SelectBrush( Self, _canvas.Canvas.Handle ) ;
    if T_Brush(_canvas.VclBrush).Style = TGIS_BrushStyle.Clear then
      old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT )
    else
      old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, OPAQUE ) ;
    old_bkcolor := Winapi.Windows.SetBkColor( _canvas.Canvas.Handle,
                                              VCLColor(
                                                T_Brush(FCanvas.VclBrush).Color
                                              )
                                            ) ;
    try
      Winapi.Windows.PolyPolyline( _canvas.Canvas.Handle,
                                   _points[0], _parts[0], _count ) ;
    finally
      Winapi.Windows.SetBkColor( _canvas.Canvas.Handle, old_bkcolor ) ;
      Winapi.Windows.SetBkMode ( _canvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawText(
    const _x      : Integer ;
    const _y      : Integer ;
    const _text   : String  ;
    const _canvas : TGIS_CanvasInternal
  ) ;
  var
    old_bkmode : Integer ;
    old_bkcolor : COLORREF ;
  begin
    T_Font(_canvas.VclFont).SelectFont( _canvas.Canvas.Handle ) ;
    if T_Brush(_canvas.VclBrush).Style = TGIS_BrushStyle.Clear then
      old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, TRANSPARENT )
    else begin
      old_bkmode := Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, OPAQUE ) ;
      old_bkcolor := Winapi.Windows.SetBkColor( _canvas.Canvas.Handle,
                                 VCLColor( T_Brush(_canvas.VclBrush).Color )
                               ) ;
    end ;
    Winapi.Windows.TextOut( _canvas.Canvas.Handle,
                            _x, _y, PChar(_text), Length( _text ) ) ;
    Winapi.Windows.SetBkColor( _canvas.Canvas.Handle, old_bkcolor ) ;
    Winapi.Windows.SetBkMode( _canvas.Canvas.Handle, old_bkmode ) ;
  end ;

  procedure TGIS_RendererVclGdi32.drawMarker(
    const _style         : TGIS_MarkerStyle  ;
    const _size          : Integer ;
    const _pt            : TPoint  ;
    const _selectionOnly : Boolean ;
    const _marker        : TGIS_ParamsMarker
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
      if ( _marker.OutlineWidth = 0 ) or ( isize < 3 ) then
        preparePen(
          FCanvas,
          _marker.Color,
          TGIS_PenStyle.Solid,
          nil,
          _marker.Pattern,
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          TwipsToPixels( -1 )
       ) ;

      case style of
        TGIS_MarkerStyle.Box           :
          begin
            if isize < 2 then
              drawRectangle( _pt.X, _pt.Y, _pt.X+1, _pt.Y+1 )
            else
              drawRectangle(   px(-64), py(-64),
                               px( 64), py( 64)
                           ) ;
          end ;
        TGIS_MarkerStyle.Circle        :
          drawEllipse  (   px(-64), py(-64),
                           px( 64), py( 64)
                       ) ;
        TGIS_MarkerStyle.Cross         :
          begin
            drawPolyline  ( [ p(  0, 64), p(  0,-64)
                            ], 2
                          ) ;
            drawPolyline  ( [ p( -64, 0), p( 64,  0)
                            ], 2
                          ) ;
          end;
        TGIS_MarkerStyle.DiagCross     :
          begin
            drawPolyline  ( [ p(-56, 56), p( 56,-56)
                            ], 2
                          ) ;
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
      if ( isize < 3 ) then
        style := TGIS_MarkerStyle.Box
      else
        style := _style ;
      if ( isize < 8 )
      then
        do_draw_lowres
      else
        do_draw ;
    end ;
    if _selectionOnly then begin
      assert( Assigned( oSelectionCanvas ) ) ;

      rct := Rect( _pt.X - isize div 2,
                   _pt.Y - isize div 2,
                   _pt.X + isize div 2,
                   _pt.Y + isize div 2
                 ) ;
      drawRectangle( rct.Left, rct.Top, rct.Right, rct.Bottom,
                     oSelectionCanvas ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.doShapePoint(
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
      offset : TPoint ;

      procedure preparePoint ;
      var
        dx, dy : Double ;
      begin
        dx := 0 ;
        dy := 0 ;
        _shp.GetPointEx( 0, 0, dx, dy ) ;

        // like MapToScreen does
        pt_x := TruncS( (  dx + FExtentX ) * FZoom ) + offset.X ;
        pt_y := TruncS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
      end ;

    begin
      FCanvas.usePen := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_marker ) ;
      try
        preparePoint ;

        if params_marker.Size <> GIS_RENDER_SIZE then
          isize := TwipsToPixels( params_marker.Size )
        else
          isize := TwipsToPixels( Viewer.SelectionWidth ) ;

        if assigned( _source ) then setBrushOrigin( _source ) ;

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
              params_marker.Pattern
            ) ;
            preparePen(
              FCanvas,
              params_marker.OutlineColor,
              params_marker.OutlineStyle,
              params_marker.OutlineBitmap,
              params_marker.OutlinePattern,
              TGIS_LineCap.Round,
              TGIS_LineJoin.Miter,
              TwipsToPixels( params_marker.OutlineWidth )
            ) ;
            drawMarker( params_marker.Style, isize,
                        Point( pt_x, pt_y ), False, params_marker  ) ;
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
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            TwipsToPixels( Viewer.SelectionWidth )
          ) ;
          if Viewer.SelectionOutlineOnly then
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Clear
            )
          else
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Solid
            ) ;
          drawMarker( params_marker.Style, isize,
                      Point( pt_x, pt_y ), True, params_marker ) ;
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
    try
      draw_point ;
    except
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.doShapeMultiPoint(
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
      offset : TPoint ;

      procedure prepare_point( _idx : Integer ) ;
      var
        dx, dy : Double ;
      begin
        _shp.GetPointEx( 0, _idx, dx, dy ) ;
        // like MapToScreen does
        pt_x := TruncS( (  dx + FExtentX ) * FZoom ) + offset.X ;
        pt_y := TruncS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
      end ;

    begin
      FCanvas.usePen := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_marker ) ;
      try
        if params_marker.Size <> GIS_RENDER_SIZE then
          isize := TwipsToPixels( params_marker.Size )
        else
          isize := TwipsToPixels( Viewer.SelectionWidth ) ;

        if assigned( _source ) then setBrushOrigin( _source ) ;

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
              params_marker.Pattern
            ) ;
            preparePen(
              FCanvas,
              params_marker.OutlineColor,
              params_marker.OutlineStyle,
              params_marker.OutlineBitmap,
              params_marker.OutlinePattern,
              TGIS_LineCap.Round,
              TGIS_LineJoin.Miter,
              TwipsToPixels( params_marker.OutlineWidth )
            ) ;
            for i := 0 to _shp.GetNumPoints -1 do begin
              prepare_point( i ) ;
              drawMarker( params_marker.Style, isize,
                          Point( pt_x, pt_y ), False, params_marker ) ;
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
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            TwipsToPixels( Viewer.SelectionWidth )
          ) ;
          if Viewer.SelectionOutlineOnly then
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Clear
            )
          else
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Solid
            ) ;
          for i := 0 to _shp.GetNumPoints -1 do begin
            prepare_point( i ) ;
            drawMarker( params_marker.Style, isize,
                        Point( pt_x, pt_y ), True, params_marker ) ;
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
    try
      draw_multi_point ;
    except
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.doShapeLine(
    const _shp           : TGIS_ShapeArc ;
    const _source        : TGIS_ShapeArc ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  var
    params_line : TGIS_ParamsLine ;

    procedure draw_line ;
    var
      part_no     : Integer ;
      cur_size    : Integer ;
      draw_buf    : TGIS_DrawBuf ;
      max_tlrnc   : Integer ;
      selwidth    : Integer ;
      offset      : TPoint ;

      function prepare_drawbufpart(
        const _max_tlrnc : Integer ;
        const _part_no   : Integer
      ) : Integer  ;
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

        last_pt_x := 0 ;
        last_pt_y := 0 ;
        cur_tlrnc := _max_tlrnc ;
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

      offset := getOffsetPoint( params_line ) ;
      try
        if assigned( _source ) then setBrushOrigin( _source ) ;

        for part_no := 0 to _shp.GetNumParts - 1 do begin // all parts
          cur_size := 0 ;

          SetLength( draw_buf, _shp.GetPartSize(part_no) ) ;

          cur_size := prepare_drawbufpart( iTolerance, part_no ) ;
          if cur_size < 1 then continue ;

          if not _selectionOnly then begin

            // first draw outline
           if ( Integer( _outlineMode )
                and Integer( TGIS_RendererMultipassMode.Outline )
              ) <> 0 then
           begin
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
                  TGIS_LineCap.Round,
                  TGIS_LineJoin.Round,
                  TwipsToPixels( params_line.Width )
                ) ;
                drawPolyline( draw_buf, cur_size ) ;
              end ;
            end ;
          end ;

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

            preparePen(
              oSelectionCanvas,
              colorSelection,
              TGIS_PenStyle.Solid,
              nil,
              TGIS_BrushStyle.Solid,
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
    try
      draw_line ;
    except
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.doShapePolygon(
    const _shp           : TGIS_ShapePolygon ;
    const _source        : TGIS_ShapePolygon ;
    const _selectionOnly : Boolean
  ) ;
  var
    params_area : TGIS_ParamsArea ;
    rect_screen : TRect ;

    procedure draw_polygon ;
    var
      num_parts : Integer ;
      draw_buf  : TGIS_DrawBuf ;
      part_buf  : TGIS_IntegerArray ;
      hole_buf  : array of Byte ;
      act_part_no : Integer ;
      shp_size  : Integer ;
      max_tlrnc : Integer ;
      selwidth  : Integer ;
      tmp       : Integer ;
      i, j, k   : Integer ;
      pbuf      : TGIS_DrawBuf ;
      offset    : TPoint ;

      function prepare_drawbuf(
        const _max_tlrnc : Integer
      ) : Integer ;
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
        isize      := 0 ;
        iparts     := 0 ;
        ilastsize  := 0 ;
        last_pt_x  := 0 ;
        last_pt_y  := 0 ;
        first_pt_x := 0 ;
        first_pt_y := 0 ;
        for part_no := 0 to _shp.GetNumParts -1 do begin
          part_size := _shp.GetPartSize( part_no ) ;

          if part_size <= 0 then continue ;

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
                  inc(isize) ;
                  inc( cur_size ) ;
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

        if isize > 0 then begin
        Result := iparts ;
        end;

        Result := iparts ;
      end ;

      function select_polygon_using_regions : Boolean ;
      var
        isize   : Integer ;
        winding : Integer ;
        i       : Integer ;
        rgn     : HRGN    ;

        procedure draw_selection_polygon_ex(
          const _points  : TGIS_DrawBuf ;
          const _part    : Integer ;
          const _count   : Integer ;
          const _winding : Integer
        ) ;

          procedure selection_polygon_path_to_region(
            const _winding : Integer
          ) ;
          var
            tmp_rgn  : HRGN    ;
            box      : TRect   ;
            rgn_type : Integer ;
          begin
            tmp_rgn := Winapi.Windows.PathToRegion( FCanvas.Canvas.Handle ) ;
            try
              if tmp_rgn = HRGN( 0 ) then begin
                // should not happen - possible only for degenerated shapes
                box := Rect( 0, 0, 0, 0 )
              end
              else
                Winapi.Windows.GetRgnBox( tmp_rgn, box ) ;

              if ( box.Left = box.Right  ) and
                 ( box.Top  = box.Bottom ) then
                exit ;

              if rgn = HRGN( 0 ) then begin
                rgn := tmp_rgn ;
                tmp_rgn := HRGN( 0 ) ;
              end else begin
                if _winding = 1 then
                  // clockwise winding
                  rgn_type := Winapi.Windows.CombineRgn( rgn,
                                                         rgn,
                                                         tmp_rgn, RGN_OR
                                                       )
                else
                  // counterclockwise winding
                  rgn_type := Winapi.Windows.CombineRgn( rgn,
                                                         rgn,
                                                         tmp_rgn, RGN_XOR
                                                        ) ;
                if rgn_type = Winapi.Windows.ERROR then begin // just to avoid hint
                  Assert( rgn_type <> Winapi.Windows.ERROR ) ;
                end ;
              end ;
            finally
              if tmp_rgn <> HRGN( 0 ) then
                Winapi.Windows.DeleteObject( tmp_rgn ) ;
            end ;
          end ;

        begin
          if _count < 3 then exit ;
          Winapi.Windows.BeginPath( FCanvas.Canvas.Handle ) ;
          try
            Winapi.Windows.Polygon( FCanvas.Canvas.Handle,
                                    _points[_part], _count );
          finally
            Winapi.Windows.EndPath( FCanvas.Canvas.Handle ) ;
          end ;
          selection_polygon_path_to_region( _winding ) ;
        end ;

      begin
        Result := False ;
        rgn := HRGN( 0 ) ;

        isize := 0 ;
        for i := 0 to act_part_no - 1 do begin
          if hole_buf[i] = 0 then begin // ignore empty polygons
            isize := isize + part_buf[i] ;
            continue ;
          end ;

          {$IFDEF GIS_POLYPOLYGON_WINDED}
            if hole_buf[i] = 1 then
              winding := 1    // clockwise winding
            else
              winding := 0 ;  // counterclockwise winding
          {$ELSE}
              winding := 0 ;  // counterclockwise winding
          {$ENDIF}
          draw_selection_polygon_ex( draw_buf, isize, part_buf[i], winding ) ;
          isize := isize + part_buf[ i ] ;
        end ;

        if rgn <> HRGN( 0 ) then begin
          Winapi.Windows.SelectClipRgn( FCanvas.Canvas.Handle, rgn ) ;
          Winapi.Windows.IntersectClipRect( FCanvas.Canvas.Handle,
                                            rect_screen.Left,
                                            rect_screen.Top,
                                            rect_screen.Right,
                                            rect_screen.Bottom
                                          ) ;
          Result := True ;
        end ;

      end ;

      procedure draw_polygon_symbolfill ;
      var
        fill_gap : TPoint ;
        prjex : TGIS_Extent ;
        ptg_x : Double ;
        ptg_y : Double ;
        fill_offset : TPoint ;
        corner1_x : Double ;
        corner1_y : Double ;
        corner2_x : Double ;
        corner2_y : Double ;
        rvis : TRect ;
        rct : TRect ;
        rct_left : Integer ;
        rct_top : Integer ;
        rct_right : Integer ;
        rct_bottom : Integer ;
        x, y : Integer ;
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
        {$IFDEF GIS_POLYPOLYGON_NATIVE}
          Assert( False ) ;
        {$ENDIF}
        if select_polygon_using_regions then begin
          fill_gap.X := params_area.Symbol.Width  +
                        TwipsToPixels( params_area.SymbolGap ) ;
          fill_gap.Y := params_area.Symbol.Height +
                        TwipsToPixels( params_area.SymbolGap ) ;

          prjex := _shp.ProjectedExtent ;
          // like MapToScreenEx does
          ptg_x := (  prjex.XMin + FExtentX ) * FZoom ;
          ptg_y := ( -prjex.YMax + FExtentY ) * FZoom ;

          if ( params_area.SymbolSize <> 0 ) and
             ( fill_gap.X > 0 )              and
             ( fill_gap.Y > 0 )
          then begin
            fill_offset.X :=
              TruncS( ptg_x - Int( ptg_x / fill_gap.X ) * fill_gap.X ) ;
            fill_offset.Y :=
              TruncS( ptg_y - Int( ptg_y / fill_gap.Y ) * fill_gap.Y ) ;

            // like MapToScreenEx does
            corner1_x := (  prjex.XMin + FExtentX ) * FZoom ;
            corner1_y := ( -prjex.YMax + FExtentY ) * FZoom ;
            // like MapToScreenEx does
            corner2_x := (  prjex.XMax + FExtentX ) * FZoom ;
            corner2_y := ( -prjex.YMin + FExtentY ) * FZoom ;

            rvis := FCanvas.Canvas.ClipRect ;
            if corner1_x < 0
            then rct_Left   := fill_offset.X
            else rct_Left   := RoundS( corner1_x ) ;
            if corner2_x > rvis.Right
            then rct_Right  := rvis.Right
            else rct_Right  := RoundS( corner2_x ) ;
            if corner1_y < 0
            then rct_Top    := fill_offset.Y
            else rct_Top    := RoundS( corner1_y ) ;
            if corner2_y > rvis.Bottom
            then rct_Bottom := rvis.Bottom
            else rct_Bottom := RoundS( corner2_y ) ;
            rct := Rect( rct_Left, rct_Top,
                         rct_Right, rct_Bottom
                       ) ;

            y := rct.Top + params_area.Symbol.Height div 2 ;
            while y <= rct.Bottom + fill_gap.Y div 2 do begin
              x := rct.Left + params_area.Symbol.Width div 2;
              while x <= rct.Right + fill_gap.X div 2 do begin
                params_area.Symbol.Draw( x , y) ;
                x := x + fill_gap.X ;
              end ;
              y := y + fill_gap.Y ;
            end ;
          end ;
          Winapi.Windows.SelectClipRgn( FCanvas.Canvas.Handle, HRGN( 0 ) ) ;
          Winapi.Windows.IntersectClipRect( FCanvas.Canvas.Handle,
                                            rect_screen.Left,
                                            rect_screen.Top,
                                            rect_screen.Right,
                                            rect_screen.Bottom ) ;
        end ;
        params_area.Symbol.Unprepare ;
      end ;

      procedure draw_polygon_patternfill ;
      var
        bmp0 : TBitmap ;
        bmp : TBitmap ;
        prjex : TGIS_Extent ;
        ptg_x : Double ;
        ptg_y : Double ;
        fill_offset : TPoint ;
        corner1_x : Double ;
        corner1_y : Double ;
        corner2_x : Double ;
        corner2_y : Double ;
        rvis : TRect ;
        rct : TRect ;
        rct_left : Integer ;
        rct_top : Integer ;
        rct_right : Integer ;
        rct_bottom : Integer ;
        x, y : Integer ;
      begin

        {$IFDEF GIS_POLYPOLYGON_NATIVE}
          Assert( False ) ;
        {$ENDIF}
        bmp0 := brushCache.GetBitmap( params_area.Pattern,
                                      params_area.Color ) ;
        bmp := prepareBitmapFill( bmp0 ) ;
        try
          if select_polygon_using_regions then begin
            prjex := _shp.ProjectedExtent ;
            // like MapToScreenEx does
            ptg_x := (  prjex.XMin + FExtentX ) * FZoom ;
            ptg_y := ( -prjex.YMax + FExtentY ) * FZoom ;

            if ( bmp.Width > 0 ) and ( bmp.Height > 0 )
            then begin
              fill_offset.X :=
                TruncS( ptg_x - Int( ptg_x / bmp.Width ) * bmp.Width ) ;
              fill_offset.Y :=
                TruncS( ptg_y - Int( ptg_y / bmp.Height ) * bmp.Height ) ;

              // like MapToScreenEx does
              corner1_x := (  prjex.XMin + FExtentX ) * FZoom ;
              corner1_y := ( -prjex.YMax + FExtentY ) * FZoom ;
              // like MapToScreenEx does
              corner2_x := (  prjex.XMax + FExtentX ) * FZoom ;
              corner2_y := ( -prjex.YMin + FExtentY ) * FZoom ;

              rvis := FCanvas.Canvas.ClipRect ;
              if corner1_x < 0
              then rct_Left   := fill_offset.X
              else rct_Left   := RoundS( corner1_x ) ;
              if corner2_x > rvis.Right
              then rct_Right  := rvis.Right
              else rct_Right  := RoundS( corner2_x ) ;
              if corner1_y < 0
              then rct_Top    := fill_offset.Y
              else rct_Top    := RoundS( corner1_y ) ;
              if corner2_y > rvis.Bottom
              then rct_Bottom := rvis.Bottom
              else rct_Bottom := RoundS( corner2_y ) ;
              rct := Rect( rct_Left, rct_Top,
                           rct_Right, rct_Bottom
                         ) ;

              y := rct.Top ;
              while y <= rct.Bottom do begin
                x := rct.Left ;
                while x <= rct.Right do begin
                  FCanvas.Canvas.Draw( x, y, bmp ) ;
                  x := x + bmp.Width ;
                end ;
                y := y + bmp.Height ;
              end ;
            end ;
            Winapi.Windows.SelectClipRgn( FCanvas.Canvas.Handle, HRGN( 0 ) ) ;
            Winapi.Windows.IntersectClipRect( FCanvas.Canvas.Handle,
                                              rect_screen.Left,
                                              rect_screen.Top,
                                              rect_screen.Right,
                                              rect_screen.Bottom ) ;
          end ;
        finally
          if bmp <> bmp0 then
            FreeObject( bmp ) ;
        end;
      end ;

    begin
      SetLength( draw_buf, _shp.GetNumPoints ) ;
      SetLength( part_buf, _shp.GetNumParts  ) ;
      SetLength( hole_buf, _shp.GetNumParts  ) ;

      FCanvas.usePen := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_area ) ;
      try

        if assigned( _source ) then setBrushOrigin( _source ) ;

        if not _selectionOnly then begin

          num_parts := prepare_drawbuf( iTolerance ) ;
          if num_parts < 1 then exit ;

          act_part_no := length( part_buf ) ;

          // inside first

          // symbol fill
          if assigned( params_area.Symbol ) then
          begin
            draw_polygon_symbolfill ;
          end

          // pattern fill
          else if ( params_area.Pattern <> TGIS_BrushStyle.Clear ) and
                  ( params_area.Pattern <> TGIS_BrushStyle.Solid ) then
          begin
            draw_polygon_patternfill ;
          end

          // bitmap fill
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
                  params_area.Pattern
                )
              else
                prepareBrush(
                  FCanvas,
                  params_area.Color,
                  nil,
                  params_area.Pattern
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

            FCanvas.useBrush := False ;
            try
            preparePen(
              FCanvas,
              params_area.OutlineColor,
              params_area.OutlineStyle,
              params_area.OutlineBitmap,
              params_area.OutlinePattern,
              TGIS_LineCap.Round,
              TGIS_LineJoin.Round,
              TwipsToPixels( params_area.OutlineWidth )
            ) ;
            drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
            finally
              FCanvas.useBrush := True ;
            end ;
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
                    TGIS_LineCap.Round,
                    TGIS_LineJoin.Round,
                    TwipsToPixels( 1 )
                  ) ;
                  drawPolyPolygon( draw_buf, part_buf, num_parts ) ;
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

          if params_area.OutlineWidth <> GIS_RENDER_SIZE then
            selwidth := selwidth +
                        TwipsToPixels( params_area.OutlineWidth ) ;

          preparePen(
            oSelectionCanvas,
            colorSelection,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            selwidth
          ) ;
          if Viewer.SelectionOutlineOnly then
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Clear
            )
          else
            prepareBrush(
              oSelectionCanvas,
              colorSelection,
              nil,
              TGIS_BrushStyle.Solid
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
    rect_screen := Rect( 0, 0,
                         TBitmap(Context.BaseMap).Width,
                         TBitmap(Context.BaseMap).Height ) ;
    try
      draw_polygon ;
    except
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.doShapeComplex(
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

  procedure TGIS_RendererVclGdi32.RenderShape(
    const _shp           : TObject ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  begin
    RenderShape( _shp, nil, _selectionOnly, _outlineMode ) ;
  end ;

  procedure TGIS_RendererVclGdi32.RenderShape(
    const _shp           : TObject ;
    const _source        : TObject ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  var
    cnv : TGIS_CanvasInternal ;
  begin
    if _shp = nil then exit ;
    if not assigned( oCanvas ) then exit ;

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

  procedure TGIS_RendererVclGdi32.doLabelPoint(
    const _shp        : TGIS_Shape ;
    const _savePoints : Boolean ;
    var   _points     : TGIS_DrawBuf
  ) ;
  var
    angle           : Double       ;
    txt             : String       ;
    rct             : TRect        ;
    rct_br          : TPoint       ;
    pt_origin_x     : Integer      ;
    pt_origin_y     : Integer      ;
    cpoint          : TGIS_Point   ;
    offset          : TPoint ;
    gap             : Integer      ;
    gap2            : Integer      ;
    ssin            : Double       ;
    scos            : Double       ;
    params_marker   : TGIS_ParamsMarker    ;
    params_label    : TGIS_ParamsLabel     ;
    label_alignment : TGIS_LabelAlignment  ;
    label_positions : TGIS_LabelPositions  ;

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
       Result.X := rect.Right  - rect.Left + 1;
       Result.Y := rect.Bottom - rect.Top  + 1;
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
          //shadow_width := Max( 1, PPI div T_Font( FCanvas.VclFont ).PixelsPerInch ) ;
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
            params_label.Pattern
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
    params_label    := _shp.Params.Labels ;

    if not params_label.Visible then exit ;

    // prepare text for label
    txt := _shp.GetLabel ;
    if IsStringEmpty( txt ) then exit ;

    if ( not FTiled ) and ( not params_label.Duplicates ) and
      Viewer.LabelsReg.IsDuplicated( txt )
    then
      exit ;

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

    CanvasFont.Name   := params_label.FontName  ;
    CanvasFont.Size   := TwipsToPoints( params_label.FontSize ) ;
    CanvasFont.Style  := params_label.FontStyle ;
    if params_label.FontColor.ARGB = TGIS_Color.RenderColor.ARGB then
      CanvasFont.Color := TGIS_Color.None
    else
      CanvasFont.Color := params_label.FontColor ;

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
      rct_br := text_extent(
                  txt,
                  rct_br,
                  label_alignment
                ) ;
      rct := Rect( 0, 0,
                   rct_br.X-1, rct_br.Y-1
                 ) ;

      if ( ( rct.Right  - rct.Left ) <= 0 ) or
         ( ( rct.Bottom - rct.Top  ) <= 0 )
      then exit ;

      if rct.Right >  TwipsToPixels( params_label.Width,  0 ) then
         rct_br.X  := TwipsToPixels( params_label.Width,  0 ) ;
      if rct.Bottom > TwipsToPixels( params_label.Height, 0 ) then
         rct_br.Y :=  TwipsToPixels( params_label.Height, 0 ) ;

      // calculate offset of rectangle
      if _shp.ShapeType in [TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint] then
      begin
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
        CanvasClearTransformation();
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.doLabelArc(
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

  procedure TGIS_RendererVclGdi32.RenderLabel(
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
  end ;

  procedure TGIS_RendererVclGdi32.RenderLabel(
    const _shp    : TObject ;
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

  procedure TGIS_RendererVclGdi32.doChart(
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
      pt_x := TruncS( (  X + FExtentX ) * FZoom ) ;
      pt_y := TruncS( ( -Y + FExtentY ) * FZoom ) ;
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

  procedure TGIS_RendererVclGdi32.RenderChart(
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

  procedure TGIS_RendererVclGdi32.RenderShapeFlashed(
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

  function TGIS_RendererVclGdi32.RenderBitmapBegin
    : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_RendererVclGdi32.RenderBitmapEnd(
    const _handle   : TObject
  ) ;
  begin

  end ;

  procedure TGIS_RendererVclGdi32.RenderBitmap(
    const _handle : TObject ;
    const _bmp    : TGIS_Pixels ;
    const _size   : TPoint  ;
    const _dst    : TRect   ;
    const _format : TGIS_BitmapFormat ;
    const _order  : TGIS_BitmapLinesOrder
  ) ;
  var
    tbmp : TBitmap ;
  begin
    if assigned( oTransparentCanvas ) then
      tbmp := oTransparentBitmap
    else
    if Assigned( oCanvasBitmap ) then
      tbmp := oCanvasBitmap
    else
    if Assigned( Context ) then
      tbmp := TBitmap( Context.BaseMap )
    else
      exit ;

    renderBitmapInternal( _handle, tbmp, _bmp, _size, _dst, _format, _order, False );
  end ;

  procedure TGIS_RendererVclGdi32.RenderBitmap(
    const _handle    : TObject               ;
    const _bmp       : TGIS_Bitmap           ;
    const _dst       : TRect                 ;
    const _antialias : Boolean
  ) ;
  var
    tbmp  : TBitmap ;
    bmp   : TBitmap ;
  begin
    if assigned( oTransparentCanvas ) then
      tbmp := oTransparentBitmap
    else
    if Assigned( oCanvasBitmap ) then
      tbmp := oCanvasBitmap
    else
    if Assigned( Context ) then
      tbmp := TBitmap( Context.BaseMap )
    else
      exit ;

    bmp := VCL.Graphics.TBitmap(createTransparentBitmap( _dst.Width, _dst.Height )) ;
    try
      stretchBitmap( _bmp.NativeBitmap, bmp, Rect( 0, 0, bmp.Width, bmp.Height ), 100 ) ;
      TBitmap( bmp ).AlphaFormat := TAlphaFormat.afPremultiplied ;
      tbmp.Canvas.StretchDraw( _dst, bmp ) ;
    finally
      bmp.Free ;
    end;
  end ;

  procedure TGIS_RendererVclGdi32.drawEditingLines(
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
    ) : Integer  ;
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
      TGIS_BrushStyle.Clear
    ) ;
    preparePen(
      oEditCanvas,
      Viewer.Editor.EditingLinesStyle.PenColor,
      Viewer.Editor.EditingLinesStyle.PenStyle,
      nil,
      TGIS_BrushStyle.Solid,
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

      drawPolyline( draw_buf, part_size, oEditCanvas ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawEditingPointMarkers(
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
    dx, dy   : Double  ;
    pt_x     : Integer ;
    pt_y     : Integer ;
    ptg3D    : TGIS_Point3D ;
    tw       : Integer ;

    procedure prepare_font ;
    begin
      prepareFont(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.PointsFont.Name,
        Viewer.Editor.EditingPointsStyle.PointsFont.Size,
        Viewer.Editor.EditingPointsStyle.PointsFont.Style,
        Viewer.Editor.EditingPointsStyle.PointsFont.Color
      ) ;
      T_Font(oEditCanvas.VclFont).SelectFont( oEditCanvas.Canvas.Handle ) ;
    end ;

    procedure prepare_active ;
    begin
      prepareBrush(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.ActivePoints.BrushColor,
        nil,
        Viewer.Editor.EditingPointsStyle.ActivePoints.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.ActivePoints.PenColor,
        Viewer.Editor.EditingPointsStyle.ActivePoints.PenStyle,
        nil,
        TGIS_BrushStyle.Solid,
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
        Viewer.Editor.EditingPointsStyle.InactivePoints.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.InactivePoints.PenColor,
        Viewer.Editor.EditingPointsStyle.InactivePoints.PenStyle,
        nil,
        TGIS_BrushStyle.Solid,
        TGIS_LineCap.Round,
        TGIS_LineJoin.Round,
        Viewer.Editor.EditingPointsStyle.InactivePoints.PenWidth
      ) ;
    end ;

    procedure prepare_selected ;
    begin
      prepareBrush(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushColor,
        nil,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.PenColor,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.PenStyle,
        nil,
        TGIS_BrushStyle.Solid,
        TGIS_LineCap.Round,
        TGIS_LineJoin.Round,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.PenWidth
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
          T_Brush(oEditCanvas.VclBrush).Style
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

    procedure draw_text(
      const _x    : Integer ;
      const _y    : Integer ;
      const _text : String
    ) ;
    begin
      if Viewer.Editor.EditingPointsStyle.PointsFont.Color =
         Viewer.Editor.EditingPointsStyle.PointsBackground then
        prepareBrush(
            oEditCanvas,
            Viewer.Editor.EditingPointsStyle.PointsBackground,
            nil,
            TGIS_BrushStyle.Clear
        )
      else
        prepareBrush(
            oEditCanvas,
            Viewer.Editor.EditingPointsStyle.PointsBackground,
            nil,
            TGIS_BrushStyle.Solid
        ) ;
      drawText( _x, _y, _text, oEditCanvas ) ;
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
        pt_x := TruncS( (  dx + FExtentX ) * FZoom ) ;
        pt_y := TruncS( ( -dy + FExtentY ) * FZoom ) ;

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
             ) >=  2 * Abs( T_Font(oEditCanvas.VclFont).Height )
          then begin
            old_pt_txt_x := pt_x ;
            old_pt_txt_y := pt_y ;

            if Viewer.Editor.ShowPointsNumbers then begin
              draw_text( pt_x + tps, pt_y + tps, IntToStr( point_no ) ) ;
              if Viewer.Editor.ShowPoints3D then begin
                // Show coord Z
                prepare_z ( Abs(ptg3D.Z) > eps ) ;
                tw := oEditCanvas.Canvas.TextWidth( Format('%.2f',[ptg3D.Z] ) ) ;
                draw_text( pt_x - tps*2 - tw, pt_y - tps*2,
                           Format('%.2f',[ptg3D.Z] ) ) ;
              end ;
            end ;
          end ;
        end ;
      end ;
    end ;

    if ( Viewer.Editor.PointPos >= 0 ) and
       ( Viewer.Editor.PointPos < _shp.GetPartSize( Viewer.Editor.Part ) )
    then begin
      // active point
      prepare_selected ;
      _shp.GetPointEx( Viewer.Editor.Part, Viewer.Editor.PointPos, dx, dy ) ;
      if ( dx >= ext.XMin ) or
         ( dx <= ext.XMax ) or
         ( dy >= ext.YMin ) or
         ( dy <= ext.YMax )
      then begin
        // like MapToScreen does
        pt_x := TruncS( (  dx + FExtentX ) * FZoom ) ;
        pt_y := TruncS( ( -dy + FExtentY ) * FZoom ) ;
        draw_marker( pt_x - tps, pt_y - tps, pt_x + tps, pt_y + tps ) ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.drawEditingEdgeLengths(
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
        Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.PenColor,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.PenStyle,
        nil,
        TGIS_BrushStyle.Solid,
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
        drawRectangle( _x - 2*gap, _y, _x + pt.X, _y + pt.Y - gap,
                       oEditCanvas
                     ) ;
      end ;
      drawText( _x, _y, _text, oEditCanvas  ) ;
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

  procedure TGIS_RendererVclGdi32.drawEditingPoints(
    const _context : TCanvas ;
    const _shp     : TGIS_Shape
  ) ;
  begin
    if _shp.IsEmpty then exit ;
    oEditCanvas := TGIS_CanvasInternal.Create( _context ) ;
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

  procedure TGIS_RendererVclGdi32.RenderEditor(
    const _context : TObject
  ) ;
  begin
    if Assigned( Viewer ) and Viewer.Editor.InEdit then begin
      if assigned( Viewer.Editor.CurrentShape ) then begin
        // ensure proper renderer context
        if assigned( TGIS_Shape( Viewer.Editor.CurrentShape ).Layer ) then begin
          TGIS_Shape( Viewer.Editor.CurrentShape ).Layer.Renderer := self ;
          drawEditingPoints( TBitmap( _context ).Canvas,
                             TGIS_Shape( Viewer.Editor.CurrentShape ) ) ;

          fix_bitmap( TBitmap( _context ) ) ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.PaintExtra(
    const _sender  : TObject ;
    const _context : TObject ;
    const _event   : TGIS_RendererEvent
 );
  var
    ocnv : TGIS_CanvasInternal ;
    fcnv : TGIS_CanvasInternal ;
    osel : TGIS_CanvasInternal ;
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
      if ( _context is TCanvas ) then begin
        parent := TComponent(_sender) as IGIS_ViewerParent ;
        save_context ;

        fs := FontScale ;
        if fs = 0 then fs := 100 ;

        inherited CreateContext( parent, Viewer, Context, Point(0,0),
                                 parent.ControlCanvasWidth,
                                 parent.ControlCanvasHeight,
                                 parent.ControlPPI, fs ) ;
        FCanvas := TGIS_CanvasInternal.Create( TCanvas(_context) ) ;
      end else if _context is TBitmap then begin
        oCanvasBitmap := TBitmap( _context ) ;
        FCanvas := TGIS_CanvasInternal.Create( TBitmap(_context).Canvas ) ;
      end;
    end ;

    procedure do_extra ;
    begin
      if _context is TBitmap then begin
        fix_bitmap( TBitmap( _context ) ) ;
      end ;
    end;

    procedure release_context ;
    begin
      if ( _context is TCanvas ) then begin
        FreeObject( FCanvas ) ;
        restore_context ;
      end else if _context is TBitmap then begin
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
        _event( Self, Self, TGIS_DrawMode.All ) ;
        do_extra ;
      finally
        pextra := False ;
        release_context ;
        oCanvas := ocnv ;
        FCanvas := fcnv ;
        oSelectionCanvas := osel ;
        oCanvasBitmap := nil ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.Update ;
  begin
  end ;

  procedure TGIS_RendererVclGdi32.Flush ;
  begin
  end ;

  function TGIS_RendererVclGdi32.CanvasNative
    : TObject ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Canvas ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasFontMetrics(
    var _break_char : Char    ;
    var _height     : Integer ;
    var _ascent     : Integer ;
    var _true_type  : Boolean
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
  end ;

  function TGIS_RendererVclGdi32.CanvasTextExtent(
    const _text : String
  ) : TPoint ;
  var
    size : TSize ;
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
    T_Font(FCanvas.VclFont).SelectFont( FCanvas.Canvas.Handle ) ;
    Winapi.Windows.GetTextExtentPoint32( FCanvas.Canvas.Handle,
                                         PChar(_text),
                                         Length( _text ),
                                         size
                                       ) ;
    Result.X := size.cx  ;
    Result.Y := size.cy ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawText(
    const _rect : TRect   ;
    const _text : String
  ) ;
  var
    old_bkmode : Integer ;
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
    T_Font(FCanvas.VclFont).SelectFont( FCanvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetTextAlign( FCanvas.Canvas.Handle, 0 ) ;
    old_bkmode := Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, TRANSPARENT ) ;
    try
      Winapi.Windows.TextOut( FCanvas.Canvas.Handle,
                              _rect.Left, _rect.Top,
                              PChar(_text), Length( _text ) ) ;
    finally
      Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawLine(
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
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;

    drawLine( _x1, _y1, _x2, _y2, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawRectangle(
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
      TGIS_LineCap.Square,
      TGIS_LineJoin.Miter,
      FCanvas.Pen.Width
    ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    drawRectangle( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawEllipse(
    const _x      : Integer ;
    const _y      : Integer ;
    const _width  : Integer ;
    const _height : Integer
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      FCanvas.Pen.Width
    ) ;
    drawEllipse( _x, _y, _x + _width, _y + _height, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPie(
    const _angle_0  : Double  ;
    const _angle_1  : Double  ;
    const _radius   : Integer ;
    const _origin_x : Integer ;
    const _origin_y : Integer
  ) ;
  var
    points   : TGIS_DrawBufF ;
    old_cap  : TGIS_LineCap  ;
    old_join : TGIS_LineJoin ;

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
      asin     : Double  ;
      acos     : Double  ;
      ptg      : TGIS_Point ;

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

      arcangle := fmod( (_stop-_start) +4*Pi, 2*Pi ) ;
      if arcangle = 0 then begin
        // full rotation
        if _stop <> _start then
          arcangle := 2*Pi ;
      end ;

      // calculate number of segments - and minimize it
      steps := RoundS( Abs( arcAngle ) / (2*Pi) * _segments ) ;
      steps := Max( 4, steps ) ;

      step := Abs( arcangle ) / steps ;

      // create vector of points
      SetLength( points, steps + 2 ) ;

      // calculate elliptical arc
      angle := _start ;
      for cnt := 0 to steps do begin
        SinCos( angle, asin, acos ) ;
        ptg.Y := -_radiusB * asin * rcos +
                  _radiusA * acos * rsin + _center_Y ;
        ptg.X := -_radiusA * acos * rcos -
                  _radiusB * asin * rsin + _center_X ;
        points[cnt] := ptg ;

        angle := angle + step ;
      end ;
      points[steps+1] := GisPoint( _center_X, _center_Y ) ;
    end ;

  begin
    try
      stroke_arc( _origin_x, _origin_y,
                  _radius div 2, _radius div 2,
                  _angle_0, _angle_1,
                  Pi/2, 30
                ) ;
      old_cap  := CanvasPen.LineCap  ;
      old_join := CanvasPen.LineJoin ;
      try
        CanvasPen.LineCap  := TGIS_LineCap.Round ;
        CanvasPen.LineJoin := TGIS_LineJoin.Round ;
        CanvasDrawPolygon( points ) ;
      finally
        CanvasPen.LineCap  := old_cap  ;
        CanvasPen.LineJoin := old_join ;
      end ;
    finally
      if Assigned( points ) then
        points := nil ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawArc(
    const _x           : Integer ;
    const _y           : Integer ;
    const _width       : Integer ;
    const _height      : Integer ;
    const _startX      : Integer ;
    const _startY      : Integer ;
    const _endX        : Integer ;
    const _endY        : Integer
  ) ;
  var
    old_bkmode  : Integer  ;
  begin
    assert( assigned( FCanvas ) ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      FCanvas.Pen.Width
    ) ;

    T_Brush(FCanvas.VclBrush).SelectBrush( Self, FCanvas.Canvas.Handle ) ;
    T_Pen(FCanvas.VclPen).SelectPenEx( Self, FCanvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, TRANSPARENT ) ;
    try
      Winapi.Windows.Arc( FCanvas.Canvas.Handle, _x, _y, _x+_width, _y+_height,
                          _startX, _startY, _endX, _endY
                        ) ;
    finally
      Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawArc(
    const _x          : Integer ;
    const _y          : Integer ;
    const _radius     : Integer ;
    const _startAngle : Single ;
    const _sweepAngle : Single
  ) ;
  var
    old_bkmode      : Integer  ;
    rs, rc          : Single ;
    startX, startY,
    endX, endY      : Integer  ;
  begin
    assert( assigned( FCanvas ) ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      FCanvas.Pen.Width
    ) ;

    if _sweepAngle < 0 then begin
      SinCos( DegToRad(_startAngle-_sweepAngle), rs, rc ) ;
      startX := _X + Round( _radius*rc ) ;
      startY := _Y + Round( _radius*rs ) ;

      SinCos( DegToRad(_startAngle), rs, rc ) ;
      endX := _X + Round( _radius*rc ) ;
      endY := _Y + Round( _radius*rs ) ;
    end
    else begin
      SinCos( DegToRad(_startAngle), rs, rc ) ;
      startX := _X + Round( _radius*rc ) ;
      startY := _Y + Round( _radius*rs ) ;

      SinCos( DegToRad(_startAngle-_sweepAngle), rs, rc ) ;
      endX := _X + Round( _radius*rc ) ;
      endY := _Y + Round( _radius*rs ) ;
    end ;

    T_Brush(FCanvas.VclBrush).SelectBrush( Self, FCanvas.Canvas.Handle ) ;
    T_Pen(FCanvas.VclPen).SelectPenEx( Self, FCanvas.Canvas.Handle ) ;
    old_bkmode := Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, TRANSPARENT ) ;
    try
      Winapi.Windows.Arc( FCanvas.Canvas.Handle, _X-_radius, _Y-_radius, _X+_radius, _Y+_radius,
                          startX, startY, endX, endY ) ;
    finally
      Winapi.Windows.SetBkMode( FCanvas.Canvas.Handle, old_bkmode ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf
  ) ;
  begin
    CanvasDrawPolyLine( _points, Length( _points ) ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF
  ) ;
  begin
    CanvasDrawPolyLine( _points, Length( _points ) ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf ;
    const _count  : Integer
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    drawPolyline( _points, _count, FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF ;
    const _count  : Integer
  ) ;
  var
    i   : Integer ;
    buf : TGIS_DrawBuf ;
  begin
    assert( assigned( FCanvas ) ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;

    SetLength( buf, _count ) ;
    for i := 0 to _count -1 do begin
      buf[i] := Point( RoundS( _points[i].X ), RoundS( _points[i].Y ) );
    end ;

    drawPolyline( buf, _count, FCanvas ) ;
  end ;


  procedure TGIS_RendererVclGdi32.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf ;
    const _parts  : TGIS_IntegerArray
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      CanvasPen.Width
    ) ;
    drawPolyPolyline( _points, _parts, length(_parts), FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    i   : Integer ;
    buf : TGIS_DrawBuf ;
  begin
    assert( assigned( FCanvas ) ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      CanvasPen.Width
    ) ;

    SetLength( buf, Length( _points ) ) ;
    for i := 0 to Length( _points ) -1 do begin
      buf[i] := Point( RoundS( _points[i].X ), RoundS( _points[i].Y ) );
    end ;

    drawPolyPolyline( buf, _parts, length(_parts), FCanvas ) ;
  end ;


  procedure TGIS_RendererVclGdi32.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf
  ) ;
  begin
    CanvasDrawPolygon( _points, length(_points) ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF
  ) ;
  begin
    CanvasDrawPolygon( _points, length(_points) ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf ;
    const _count  : Integer
  ) ;
  var
    iarr : TGIS_IntegerArray ;
  begin
    SetLength( iarr, 1 ) ;
    iarr[0] := _count ;
    CanvasDrawPolygon( _points, iarr ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF ;
    const _count  : Integer
  ) ;
  var
    iarr : TGIS_IntegerArray ;
  begin
    SetLength( iarr, 1 ) ;
    iarr[0] := _count ;
    CanvasDrawPolygon( _points, iarr ) ;
  end ;


  procedure TGIS_RendererVclGdi32.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf ;
    const _parts  : TGIS_IntegerArray
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    drawPolyPolygon( _points, _parts, length(_parts), FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    i   : Integer ;
    buf : TGIS_DrawBuf ;
  begin
    assert( assigned( FCanvas ) ) ;
    preparePen(
      FCanvas,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;
    prepareBrush(
      FCanvas,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;

    SetLength( buf, Length( _points ) ) ;
    for i := 0 to Length( _points ) -1 do begin
      buf[i] := Point( RoundS( _points[i].X ), RoundS( _points[i].Y ) );
    end ;

    drawPolyPolygon( buf, _parts, length(_parts), FCanvas ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawBitmap(
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
      bmp.LockPixels( px, True, _format, _order );
      try
        Move( _bmp[0], px[0], Length( _bmp ) * 4 ) ;
      finally
        bmp.UnlockPixels ;
      end ;
      CanvasDrawBitmap( bmp, _dst );
    finally
      FreeObject( bmp ) ;
    end ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasDrawBitmap(
    const _bmp       : TGIS_Bitmap       ;
    const _dst       : TRect
  ) ;
  begin
    RenderBitmap( nil, _bmp, _dst, True );
  end ;

  procedure TGIS_RendererVclGdi32.CanvasSetTransformation(
    const _angle    : Double  ;
    const _origin_x : Integer ;
    const _origin_y : Integer
  ) ;
  var
    xform     : TXForm  ;
    ssin      : Double  ;
    scos      : Double  ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.oldGraphicsMode := Winapi.Windows.SetGraphicsMode(
                                 FCanvas.Canvas.Handle, GM_ADVANCED ) ;
    Winapi.Windows.GetWorldTransform( FCanvas.Canvas.Handle, FCanvas.oldXform ) ;
    SinCos( _angle, ssin, scos ) ;
    xform.eM11 :=  scos ;
    xform.eM22 :=  scos ;
    xform.eM12 :=  ssin ;
    xform.eM21 := -ssin ;
    xform.eDx  := _origin_x ;
    xform.eDy  := _origin_y ;
    Winapi.Windows.SetWorldTransform( FCanvas.Canvas.Handle, xform ) ;
  end ;

  procedure TGIS_RendererVclGdi32.CanvasClearTransformation ;
  begin
    assert( assigned( FCanvas ) ) ;
    Winapi.Windows.SetWorldTransform( FCanvas.Canvas.Handle, FCanvas.oldXform ) ;
    Winapi.Windows.SetGraphicsMode( FCanvas.Canvas.Handle,
                                    FCanvas.oldGraphicsMode ) ;
  end ;

initialization
  RegisterRenderer( 'TGIS_RendererVclGdi32', TGIS_RendererVclGdi32 ) ;

{==================================== END =====================================}
end.

