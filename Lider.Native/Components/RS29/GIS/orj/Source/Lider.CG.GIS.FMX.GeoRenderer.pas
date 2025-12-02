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
  Firemonkey renderer.
}

unit FMX.GisRenderer;
{$HPPEMIT '#pragma link "FMX.GisRenderer"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}

  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Filter.Effects,
  FMX.TextLayout,
  {$IFDEF LEVEL_XE5_FMX}
    FMX.Graphics,
  {$ENDIF}
  {$IFDEF LEVEL_XE6_FMX}
    System.Math.Vectors,
  {$ENDIF}

  System.Math,
  System.UIConsts,

  GisRtl,
  GisInterfaces,
  GisRendererAbstract,
  GisTypes,
  GisTypesUI,
  GisLayerVector;

type

  /// <summary>
  ///   Encapsulate all objects connected with the native FMX canvas.
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

      FmxPen     : TObject    ;
      FmxBrush   : TObject    ;
      FmxFont    : TObject    ;

      TextLayout : TTextLayout ;

      transparency : Integer ;
      usePen       : Boolean ;
      useBrush     : Boolean ;

    protected
      procedure   doDestroy ; override;

    public
      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_canvas">
      ///   existing FMX canvas object to which object will be associated
      /// </param>
      constructor Create      ( const _canvas : TCanvas ) ;

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap on which canvas will be created and associated
      /// </param>
      constructor CreateCanvas( const _bitmap : TBitmap ) ;
  end ;

  /// <summary>
  ///   Renderer for FMX.
  /// </summary>
  TGIS_RendererFmx = class( TGIS_RendererAbstract )
    private
      brushCache : TObject ;
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
      ///   Optimization variable for special canvas management
      ///   on GPU canvases
      /// </summary>
      bCanvasGpu         : Boolean ;

      /// <summary>
      ///   Transparency canvas encapsulation.
      /// </summary>
      oTransparentCanvas : TGIS_CanvasInternal ;

      /// <summary>
      ///   Selection color with transparency altered for topmost layers.
      /// </summary>
      colorSelection     : TGIS_Color ;

      /// <summary>
      ///   Transparency canvas encapsulation. Bitmap storage.
      /// </summary>
      oTransparentBitmap : {$IFDEF LEVEL_XE5_FMX}
                             FMX.Graphics.TBitmap ;
                           {$ELSE}
                             FMX.Types.TBitmap ;
                           {$ENDIF}

      /// <summary>
      ///   Reference to current canvas encapsulation.
      /// </summary>
      FCanvas            : TGIS_CanvasInternal ;

      /// <summary>
      ///   Stored canvas for PrapreDraw*
      /// </summary>
      prevCanvas         : TGIS_CanvasInternal ;

      /// <summary>
      ///   Stored stransformation.
      /// </summary>
      storedTransform    : TMatrix ;

      /// <summary>
      ///   Cached render informations.
      /// </summary>
      lastRenderer       : DWORD ;

      /// <summary>
      ///   Cached render informations.
      /// </summary>
      lastCanvasType     : String ;
    private
      procedure prepareSelectionCanvas
                                 ( _transparently : Boolean ;
                                   _useBaseMap    : Boolean ;
                                   _shp           : TGIS_Shape
                                 ) ;
      procedure prepareChartsCanvas ;
      procedure prepareLabelsCanvas ;

    private
      iTolerance    : Integer ;
      iToleranceSel : Integer ;
      inEdit        : Boolean ;
      sourceShape   : TObject ;
      ignorePPI     : Boolean ;
      flashed       : Boolean ;
      pextra        : Boolean ;

    protected
      function  fget_BitmapFactory : TGIS_BitmapFactory ; override;
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
      ///   Ensure that provided canvas is selected in drawing context.
      ///   Especially important on Mobile platform when only one canvas can
      ///   be in drawing state at time.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to be activated; other canvases will have EndScene called;
      ///   if _canvas=nil then drawing context on all canvases will be ended
      /// </param>
      /// <param name="_flush">
      ///   force canvas flushing (calling EndScene)
      /// </param>
      procedure verifyCanvas     ( const _canvas        : TCanvas ;
                                   const _flush         : Boolean = False
                                 ) ;
    private
      function  prepareBitmapFill( const _shp           : TGIS_Shape      ;
                                   const _bmp           : TBitmap
                                 ) : TBrushBitmap ;

      function  preparePatternFill
                                 ( const _shp           : TGIS_Shape      ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _color         : TGIS_Color
                                 ) : TBrushBitmap ;

      procedure preparePen       ( const _canvas        : TGIS_CanvasInternal ;
                                   const _shp           : TGIS_Shape      ;
                                   const _color         : TGIS_Color      ;
                                   const _style         : TGIS_PenStyle   ;
                                   const _bitmap        : TGIS_Bitmap     ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _cap           : TGIS_LineCap    ;
                                   const _join          : TGIS_LineJoin   ;
                                   const _width         : Integer
                                 ) ; overload ;
      procedure preparePen       ( const _canvas        : TGIS_CanvasInternal ;
                                   const _shp           : TGIS_Shape      ;
                                   const _color         : TGIS_Color      ;
                                   const _style         : TGIS_PenStyle   ;
                                   const _bitmap        : TGIS_Bitmap     ;
                                   const _pattern       : TGIS_BrushStyle ;
                                   const _cap           : TGIS_LineCap    ;
                                   const _join          : TGIS_LineJoin   ;
                                   const _dash          : TGIS_DashArray  ;
                                   const _width         : Integer
                                 ) ; overload ;
      procedure prepareBrush     ( const _canvas        : TGIS_CanvasInternal ;
                                   const _shp           : TGIS_Shape  ;
                                   const _color         : TGIS_Color  ;
                                   const _bitmap        : TGIS_Bitmap ;
                                   const _pattern       : TGIS_BrushStyle
                                 ) ;
      procedure prepareFont      ( const _canvas        : TGIS_CanvasInternal ;
                                   const _name          : String      ;
                                   const _size          : Integer     ;
                                   const _style         : TGIS_FontStyles
                                 ) ;
      procedure drawMarker       ( const _path          : TPathData   ;
                                   const _style         : TGIS_MarkerStyle  ;
                                   const _size          : Integer     ;
                                   const _pt            : TPoint      ;
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

      procedure drawEditingPoints( const _shp           : TGIS_Shape
                                 ) ;

      procedure drawEditingEdgeLengths
                                 ( const _shp           : TGIS_Shape
                                 ) ;

    protected
      procedure doDestroy ; override ;

    public

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
                                     ) ; override;

      /// <inheritdoc/>
      procedure   Update             ; override;

      /// <inheritdoc/>
      procedure   Flush              ; override;

      /// <inheritdoc/>
      function FriendlyName          : String ; override ;

    // lo-level API
    public
      /// <inheritdoc/>
      /// <returns>
      ///   FMX.Graphics.TCanvas object.
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
  TGIS_RendererFmxCache = class( TGIS_RendererAbstractCache )
    private
      oBitmap : TGIS_Bitmap ;
    protected
      procedure doDestroy ; override ;
  end;

//##############################################################################
implementation

uses
  System.Generics.Collections,
  FMX.Canvas.GPU,
  FMX.GisCanvasFix,

  GisResource,
  GisFunctions,
  FMX.GisFramework,
  GisSymbol,
  GisHtmlLabel,
  GisCsBase,
  GisCsSystems,
  GisArcLabel,
  GisChart,
  GisParams;

const
  // Maximum number of points for pline/polygon draws
  GDI_MAXPOINT_COUNT : Integer = 20000000 ;

type
  T_FMXBitmap = {$IFDEF LEVEL_XE5_FMX}
                  FMX.Graphics.TBitmap ;
                {$ELSE}
                  FMX.Types.TBitmap ;
                {$ENDIF}

type

  T_BrushCache = class ( TGIS_ObjectDisposable )
    private
      bmpHorizontal : TBitmap ;
      clrHorizontal : TGIS_Color ;
      bmpVertical   : TBitmap ;
      clrVertical   : TGIS_Color ;
      bmpFDiagonal  : TBitmap ;
      clrFDiagonal  : TGIS_Color ;
      bmpBDiagonal  : TBitmap ;
      clrBDiagonal  : TGIS_Color ;
      bmpCross      : TBitmap ;
      clrCross      : TGIS_Color ;
      bmpDiagCross  : TBitmap ;
      clrDiagCross  : TGIS_Color ;

    protected
      procedure doDestroy ; override ;

    public

      function  GetBitmap ( const _style : TGIS_BrushStyle ;
                            const _color : TGIS_Color
                          ) : TBitmap ;
  end ;

  // Substitute of a pen object.
  T_Pen = class ( TGIS_ObjectDisposable )
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

      // Dash array which the pen uses to draw dashed line.
      FLineDash   : TGIS_DashArray ;

      FPattern    : TGIS_BrushStyle ;

      FBitmap     : TGIS_Bitmap ;

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

    protected

      // Destroy an instance.
      procedure doDestroy            ; override;

    public     // public methods

      // Create an instance.
      constructor Create             ;

      // Select the pen object according to earlier settings.
      procedure SelectPen            ( const _rnd    : TGIS_RendererFmx ;
                                       const _canvas : TGIS_CanvasInternal ;
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
  end ;

  // Substitute of a brush object.
  T_Brush = class ( TGIS_ObjectDisposable )
    // properties internal values
    private

      // Color of the brush.
      FColor    : TGIS_Color ;

      // Pattern for the brush.
      FStyle    : TGIS_BrushStyle ;

      FBitmap   : TGIS_Bitmap ;

      // Set if any setting has changed.
      FChanged  : Boolean ;

    protected   // property access routines

      procedure fset_Color           ( const _color  : TGIS_Color
                                     ) ;
      procedure fset_Style           ( const _style  : TGIS_BrushStyle
                                     ) ;
      procedure fset_Bitmap          ( const _bitmap : TGIS_Bitmap
                                     ) ;
    protected

      // Destroy an instance.
      procedure doDestroy            ; override;

    public // public methods

      // Create an instance.
      constructor Create             ;

      // Select the brush object according to earlier settings.
      procedure SelectBrush          ( const _rnd    : TGIS_RendererFmx ;
                                       const _canvas : TGIS_CanvasInternal ;
                                       const _shp    : TGIS_Shape
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
  end ;


//=============================================================================
// TGIS_CanvasInternal
//=============================================================================

  procedure TGIS_CanvasInternal.doDestroy ;
  begin
    FreeObject( TextLayout ) ;

    FreeObject( FmxFont  ) ;
    FreeObject( FmxBrush ) ;
    FreeObject( FmxPen   ) ;

    FreeObject( Font  ) ;
    FreeObject( Brush ) ;
    FreeObject( Pen   ) ;

    inherited ;
  end;

  constructor TGIS_CanvasInternal.Create(
    const _canvas : TCanvas
  ) ;
  begin
    Canvas := _canvas ;
    Pen    := TGIS_Pen.Create ;
    Brush  := TGIS_Brush.Create ;
    Font   := TGIS_Font.Create ;

    FmxPen   := T_Pen.Create ;
    FmxBrush := T_Brush.Create ;
    FmxFont  := nil ;

    TextLayout := TTextLayoutManager.TextLayoutByCanvas(
                    _canvas.ClassType
                  ).Create( _canvas ) ;
    transparency := 100 ;
    usePen := True ;
    useBrush := True ;
  end;

  constructor TGIS_CanvasInternal.CreateCanvas(
    const _bitmap : TBitmap
  ) ;
  begin
    Create( _bitmap.Canvas ) ;
  end;

//=============================================================================
// T_BrushCache
//=============================================================================

procedure T_BrushCache.doDestroy ;
begin
  FreeObject( bmpHorizontal ) ;
  FreeObject( bmpVertical   ) ;
  FreeObject( bmpFDiagonal  ) ;
  FreeObject( bmpBDiagonal  ) ;
  FreeObject( bmpCross      ) ;
  FreeObject( bmpDiagCross  ) ;

  inherited ;
end ;

function T_BrushCache.GetBitmap(
  const _style : TGIS_BrushStyle ;
  const _color : TGIS_Color
) : TBitmap ;
var
  dim : Integer ;
  b   : Boolean ;
  i   : Integer ;
  k   : Integer ;
  color : TAlphaColor ;
  bdata : TBitmapData ;

  procedure init_bmp(
    var _bmp : TBitmap ;
        _dim : Integer
  ) ;
  begin
    b := False ;
    if not Assigned( _bmp ) then begin
      _bmp := TBitmap.Create( _dim, _dim ) ;
      _bmp.Clear( TAlphaColorRec.Null );
      b := True ;
    end ;
  end ;

begin
  case _style of
    TGIS_BrushStyle.Horizontal :
      begin
        dim := 6 ;
        init_bmp( bmpHorizontal, dim ) ;
        if b or ( _color <> clrHorizontal ) then begin
          clrHorizontal := _color ;
          color := FMXColor( _color ) ;
          if bmpHorizontal.Map( TMapAccess.ReadWrite, bdata ) then begin
            for i := 0 to dim-1 do begin
              for k := 0 to dim-1 do begin
                if ( k = 3 ) then
                  bdata.SetPixel( i, k, color ) ;
              end;
            end;
            bmpHorizontal.Unmap( bdata ) ;
          end ;
        end ;
        Result := bmpHorizontal ;
      end ;
    TGIS_BrushStyle.Vertical   :
      begin
        dim := 6 ;
        init_bmp( bmpVertical, dim ) ;
        if b or ( _color <> clrVertical ) then begin
          clrVertical := _color ;
          color := FMXColor( _color ) ;
          if bmpVertical.Map( TMapAccess.ReadWrite, bdata ) then begin
            for i := 0 to dim-1 do begin
              for k := 0 to dim-1 do begin
                if ( i = 3 ) then
                  bdata.SetPixel( i, k, color ) ;
              end;
            end;
            bmpVertical.Unmap( bdata ) ;
          end ;
        end ;
        Result := bmpVertical ;
      end ;
    TGIS_BrushStyle.FDiagonal  :
      begin
        dim := 8 ;
        init_bmp( bmpFDiagonal, dim ) ;
        if b or ( _color <> clrFDiagonal ) then begin
          clrFDiagonal := _color ;
          color := FMXColor( _color ) ;
          if bmpFDiagonal.Map( TMapAccess.ReadWrite, bdata ) then begin
            for i := 0 to dim-1 do begin
              for k := 0 to dim-1 do begin
                if ( i = k ) then
                  bdata.SetPixel( i, k, color ) ;
              end ;
            end ;
            bmpFDiagonal.Unmap( bdata ) ;
          end ;
        end ;
        Result := bmpFDiagonal ;
      end ;
    TGIS_BrushStyle.BDiagonal  :
      begin
        dim := 8 ;
        init_bmp( bmpBDiagonal, dim ) ;
        if b or ( _color <> clrBDiagonal ) then begin
          clrBDiagonal := _color ;
          color := FMXColor( _color ) ;
          if bmpBDiagonal.Map( TMapAccess.ReadWrite, bdata ) then begin
            for i := 0 to dim-1 do begin
              for k := 0 to dim-1 do begin
                if ( i + k = dim-1 ) then
                  bdata.SetPixel( i, k, color ) ;
              end ;
            end ;
            bmpBDiagonal.Unmap( bdata ) ;
          end ;
        end ;
        Result := bmpBDiagonal ;
      end ;
    TGIS_BrushStyle.Cross      :
      begin
        dim := 6 ;
        init_bmp( bmpCross, dim ) ;
        if b or ( _color <> clrCross ) then begin
          clrCross := _color ;
          color := FMXColor( _color ) ;
          if bmpCross.Map( TMapAccess.ReadWrite, bdata ) then begin
            for i := 0 to dim-1 do begin
              for k := 0 to dim-1 do begin
                if ( i = 3 ) or ( k = 3 ) then
                  bdata.SetPixel( i, k, color ) ;
              end ;
            end ;
            bmpCross.Unmap( bdata ) ;
          end ;
        end ;
        Result := bmpCross ;
      end ;
    TGIS_BrushStyle.DiagCross  :
      begin
        dim := 8 ;
        init_bmp( bmpDiagCross, dim ) ;
        if b or ( _color <> clrDiagCross ) then begin
          clrDiagCross := _color ;
          color := FMXColor( _color ) ;
          if bmpDiagCross.Map( TMapAccess.ReadWrite, bdata ) then begin
            for i := 0 to dim-1 do begin
              for k := 0 to dim-1 do begin
                if ( i = k ) or ( i + k = dim ) then
                  bdata.SetPixel( i, k, color ) ;
              end ;
            end ;
            bmpDiagCross.Unmap( bdata ) ;
          end ;
        end ;
        Result := bmpDiagCross ;
      end ;
    else
      begin
        Result := nil ;
        exit ;
      end ;
  end ;
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
    FLineCap  := TGIS_LineCap.Round ;
    FLineJoin := TGIS_LineJoin.Round   ;
    FPattern  := TGIS_BrushStyle.Solid ;
    FLineDash := [] ;
    FBitmap   := nil ;
    FChanged  := False ;
  end ;

  procedure T_Pen.doDestroy ;
  begin
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
    const _rnd    : TGIS_RendererFmx ;
    const _canvas : TGIS_CanvasInternal ;
    const _shp    : TGIS_Shape
  ) ;
  var
    canvas : TCanvas ;
    bmp    : TBrushBitmap ;
    arr    : TDashArray ;
    i, j   : Integer ;
  begin
    canvas := _canvas.Canvas ;

    if not TGIS_Bitmap.IsNilOrEmpty( FBitmap ) then begin
      if assigned( canvas.Stroke.Bitmap ) then begin
        canvas.Stroke.Bitmap.DisposeOf ;
        canvas.Stroke.Bitmap := nil ;
      end ;
      bmp := _rnd.prepareBitmapFill( _shp, TBitmap( FBitmap.NativeBitmap ) ) ;
      if assigned( bmp ) then begin
        canvas.Stroke.Bitmap := bmp ;
        canvas.Stroke.Kind := TBrushKind.Bitmap ;
      end
      else
        canvas.Stroke.Kind := TBrushKind.Solid ;
    end
    else if ( FPattern = TGIS_BrushStyle.Solid ) then begin
      canvas.Stroke.Kind := TBrushKind.Solid ;
    end
    else begin
      if assigned( canvas.Stroke.Bitmap ) then begin
        canvas.Stroke.Bitmap.DisposeOf ;
        canvas.Stroke.Bitmap := nil ;
      end ;
      bmp := _rnd.preparePatternFill( _shp, FPattern, FColor ) ;
      if assigned( bmp ) then begin
        canvas.Stroke.Bitmap := bmp ;
        canvas.Stroke.Kind := TBrushKind.Bitmap ;
      end
      else
        canvas.Stroke.Kind := TBrushKind.Solid ;
    end ;

    if length( FLineDash ) > 0 then begin
      canvas.Stroke.Dash := TStrokeDash.Custom ;

      j := 0 ;
      SetLength( arr, 2*length( FLineDash ) ) ;
      for i := 0 to length(FLineDash)-1 do
        if (i = 0) and (FLineDash[i] < 0) then begin
          arr[j] := 0 ;
          inc(j);
          arr[j] := abs(FLineDash[i]/FWidth) ;
          inc(j);
        end
        else begin
          arr[j] := abs(FLineDash[i]/FWidth) ;
          inc(j);
        end;

      if (j mod 2) = 1 then begin
        inc( j ) ;
      end ;

      SetLength( arr, j ) ;

      canvas.Stroke.SetCustomDash( arr, 0 ) ;
    end
    else begin
      case FStyle of
        TGIS_PenStyle.Solid      : canvas.Stroke.Dash  := TStrokeDash.Solid      ;
        TGIS_PenStyle.Dash       : canvas.Stroke.Dash  := TStrokeDash.Dash       ;
        TGIS_PenStyle.Dot        : canvas.Stroke.Dash  := TStrokeDash.Dot        ;
        TGIS_PenStyle.DashDot    : canvas.Stroke.Dash  := TStrokeDash.DashDot    ;
        TGIS_PenStyle.DashDotDot : canvas.Stroke.Dash  := TStrokeDash.DashDotDot ;
        TGIS_PenStyle.Clear      : canvas.Stroke.Kind  := TBrushKind.None        ;
        else                       canvas.Stroke.Dash  := TStrokeDash.Solid      ;
      end ;
    end;

    canvas.Stroke.Thickness := FWidth ;

    case FLineCap of
      TGIS_LineCap.Flat   : canvas.Stroke.Cap := TStrokeCap.Flat  ;
      TGIS_LineCap.Square : canvas.Stroke.Cap := TStrokeCap.Flat  ;
      TGIS_LineCap.Round  : canvas.Stroke.Cap := TStrokeCap.Round ;
      else                  canvas.Stroke.Cap := TStrokeCap.Round ;
    end ;
    case FLineJoin of
      TGIS_LineJoin.Bevel : canvas.Stroke.Join := TStrokeJoin.Bevel ;
      TGIS_LineJoin.Miter : canvas.Stroke.Join := TStrokeJoin.Miter ;
      TGIS_LineJoin.Round : canvas.Stroke.Join := TStrokeJoin.Round ;
      else                  canvas.Stroke.Join := TStrokeJoin.Round ;
    end ;
    canvas.Stroke.Color := FMXColor( FColor ) ;
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
    FChanged := False ;
  end ;

  procedure T_Brush.doDestroy ;
  begin
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

  procedure T_Brush.SelectBrush(
    const _rnd    : TGIS_RendererFmx ;
    const _canvas : TGIS_CanvasInternal ;
    const _shp    : TGIS_Shape
  ) ;
  var
    canvas : TCanvas ;
    bmp    : TBrushBitmap ;
  begin
    canvas := _canvas.Canvas ;
    if not TGIS_Bitmap.IsNilOrEmpty( FBitmap ) then begin
      if assigned( canvas.Fill.Bitmap ) then begin
        canvas.Fill.Bitmap.DisposeOf ;
        canvas.Fill.Bitmap := nil ;
      end ;
      canvas.Fill.Bitmap := _rnd.prepareBitmapFill(
                              _shp,
                              TBitmap( FBitmap.NativeBitmap )
                            ) ;
      canvas.Fill.Kind := TBrushKind.Bitmap ;
    end
    else if FStyle = TGIS_BrushStyle.Solid then begin
      canvas.Fill.Kind := TBrushKind.Solid ;
    end
    else if FStyle = TGIS_BrushStyle.Clear then begin
      canvas.Fill.Kind := TBrushKind.None ;
    end
    else begin
      if assigned( canvas.Fill.Bitmap ) then begin
        canvas.Fill.Bitmap.DisposeOf ;
        canvas.Fill.Bitmap := nil ;
      end ;

      bmp := _rnd.preparePatternFill( _shp, FStyle, FColor ) ;
      canvas.Fill.Bitmap := bmp ;
      if assigned( bmp ) then
        canvas.Fill.Kind := TBrushKind.Bitmap
      else
        canvas.Fill.Kind := TBrushKind.Solid ;
    end ;
    canvas.Stroke.Color := FMXColor( FColor ) ;
    canvas.Fill.Color   := FMXColor( FColor ) ;
  end ;

//=============================================================================
// TGIS_RendererFmx
//=============================================================================

  procedure TGIS_RendererFmx.prepareSelectionCanvas(
    _transparently : Boolean ;
    _useBaseMap    : Boolean ;
    _shp           : TGIS_Shape
  ) ;
  var
    transp_sel : Integer ;
  begin
    if Assigned( Viewer ) then begin
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

    if not _useBaseMap and Context.SelectionOnDemand then begin
      Context.AssignSelection(
        TBitmap.Create( Width, Height ),
        True
      ) ;
      TBitmap(Context.Selection).Clear( TAlphaColorRec.Null ) ;
    end;

    if not _useBaseMap and assigned( Context.Selection ) then begin
      oSelectionCanvas := TGIS_CanvasInternal.CreateCanvas(
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

  procedure TGIS_RendererFmx.prepareChartsCanvas ;
  begin
    if Assigned( oChartsCanvas ) then exit ;

    if Context.ChartsOnDemand then begin
      Context.AssignCharts(
        TBitmap.Create( Width, Height ),
        True
      ) ;
      TBitmap(Context.Charts).Clear( TAlphaColorRec.Null ) ;
    end ;

    if assigned( Context.Charts ) then begin
      oChartsCanvas  := TGIS_CanvasInternal.CreateCanvas(
                          TBitmap( Context.Charts )
                        ) ;
    end
    else begin
      oChartsCanvas := oCanvas ;
    end;
  end;

  procedure TGIS_RendererFmx.prepareLabelsCanvas ;
  begin
    if Assigned( oLabelsCanvas ) then exit ;

    if Context.LabelsOnDemand then begin
      Context.AssignLabels(
        TBitmap.Create( Width, Height ),
        True
      ) ;
      TBitmap(Context.Labels).Clear( TAlphaColorRec.Null ) ;
    end ;

    if assigned( Context.Labels ) then begin
      oLabelsCanvas  := TGIS_CanvasInternal.CreateCanvas(
                          TBitmap( Context.Labels )
                        ) ;
    end
    else begin
      oLabelsCanvas := oCanvas ;
    end;
  end;

  function TGIS_RendererFmx.fget_BitmapFactory
    : TGIS_BitmapFactory ;
  begin
    Result := nil ;
  end;

  function TGIS_RendererFmx.fget_CanvasPen
   : TGIS_Pen ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Pen ;
  end;

  procedure TGIS_RendererFmx.fset_CanvasPen(
    const _value : TGIS_Pen
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.Pen := _value ;
  end;

  function TGIS_RendererFmx.fget_CanvasBrush
    : TGIS_Brush ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Brush ;
  end;

  procedure TGIS_RendererFmx.fset_CanvasBrush(
    const _value : TGIS_Brush
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.Brush := _value ;
  end;

  function  TGIS_RendererFmx.fget_CanvasFont
    : TGIS_Font ;
  begin
    assert( assigned( FCanvas ) ) ;
    Result := FCanvas.Font ;
  end;

  procedure TGIS_RendererFmx.fset_CanvasFont(
    const _value : TGIS_Font
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    FCanvas.Font := _value ;
  end;

  function TGIS_RendererFmx.fget_Info
    : String ;
  var
    bmp : TBitmap ;
  begin
    bmp := TBitmap.Create( 1, 1 ) ;
    try
      Result := '[' + ClassName + ':' + bmp.Canvas.ClassName + ']' ;
    finally
      FreeObject( bmp ) ;
    end;
  end;

  constructor TGIS_RendererFmx.Create ;
  begin
    inherited ;

    brushCache := T_brushCache.Create ;
    flashed := false ;
    pextra := false ;

  end ;

  procedure TGIS_RendererFmx.doDestroy ;
  begin
    FreeObject( brushCache ) ;

    inherited ;
  end ;

  function TGIS_RendererFmx.CreateInstance
    : TGIS_RendererAbstract ;
  begin
    Result := TGIS_RendererFmx.Create ;
  end ;

  procedure TGIS_RendererFmx.CreateContext(
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

  procedure TGIS_RendererFmx.CreateContext(
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

  procedure TGIS_RendererFmx.CreatePrinterContext(
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


  procedure TGIS_RendererFmx.RestoreContext ;
  var
    stype : String ;
  begin
    Assert( not assigned( oCanvas ) ) ;
    if Assigned( oCanvas ) then exit ;

    if Context.BaseMapOnDemand then begin
      Context.AssignBaseMap(
        TBitmap.Create( Width, Height ),
        True
      ) ;
      TBitmap(Context.BaseMap).Clear( TAlphaColorRec.Null ) ;
    end;

    if Context.BaseMap is TGIS_Bitmap then
      oCanvas := TGIS_CanvasInternal.CreateCanvas( TBitmap( TGIS_Bitmap(Context.BaseMap).NativeBitmap ) )
    else
      oCanvas := TGIS_CanvasInternal.CreateCanvas( TBitmap( Context.BaseMap ) ) ;

    bCanvasGpu := oCanvas.Canvas is TCustomCanvasGpu ;

    FCanvas := oCanvas ;

    oEditCanvas := nil ;
    oTransparentBitmap := nil ;
    oTransparentCanvas := nil ;
    sourceShape := nil ;

    iTolerance := 1 ;
    iToleranceSel := 2 ;

    ActiveRenderer := 0 ;

    if oCanvas.Canvas is TCustomCanvasGpu then begin
      stype := TCustomCanvasGpu( oCanvas.Canvas ).Context.ClassName ;

      if stype = lastCanvasType then
        ActiveRenderer := lastRenderer
      else
      if stype = 'TContextAndroid' then
        ActiveRenderer := ActiveRenderer or _GLES
      else
      if stype = 'TContextIOS' then
        ActiveRenderer := ActiveRenderer or _GLES
      else
      if stype = 'TContextMetal' then
        ActiveRenderer := ActiveRenderer or _METAL
      else
      if stype = 'TContextMetal' then
        ActiveRenderer := ActiveRenderer or _METAL
      else
      if stype = 'TDX11Context' then
        ActiveRenderer := ActiveRenderer or _DirectX
      else
      if stype = 'TDX9Context' then
        ActiveRenderer := ActiveRenderer or _DirectX ;
    end
    else begin
      stype := oCanvas.Canvas.ClassName ;

      if stype = lastCanvasType then
        ActiveRenderer := lastRenderer
      else
      if stype = 'TCanvasD2D' then
        ActiveRenderer := ActiveRenderer or _Direct2D
      else
      if stype = 'TCanvasGdiPlus' then
        ActiveRenderer := ActiveRenderer or _GdiPlus
      else
      if stype = 'TCanvasQuartz' then
        ActiveRenderer := ActiveRenderer or _Quartz
      else
      if stype.StartsWith('TGrCanvas') then
        ActiveRenderer := ActiveRenderer or _Skia
      else
      if stype.StartsWith('TVkCanvas') then
        ActiveRenderer := ActiveRenderer or _Skia
      else
      if stype.StartsWith('TGlCanvas') then
        ActiveRenderer := ActiveRenderer or _Skia
      else
      if stype.StartsWith('TMtlCanvas') then
        ActiveRenderer := ActiveRenderer or _Skia
      else
      if stype.StartsWith('TSkRaster') then
        ActiveRenderer := ActiveRenderer or _Skia ;
    end;

    lastCanvasType := stype;
    lastRenderer := ActiveRenderer;

    {$IFDEF GIS_MOBILE}
      if ( ActiveRenderer and _Skia ) = 0 then
        if Assigned( Viewer ) then
          Viewer.IncrementalPaint := False
    {$ENDIF}
  end ;

  procedure TGIS_RendererFmx.ReleaseContext ;
  begin
    if Assigned( oCanvas ) then
      if oCanvas.Canvas.BeginSceneCount > 0 then
        oCanvas.Canvas.EndScene ;

    FreeObject( oCanvas ) ;

    FCanvas := nil ;
    inherited ;
  end ;

  procedure TGIS_RendererFmx.verifyCanvas(
    const _canvas : TCanvas ;
    const _flush  : Boolean
  ) ;
  begin
    if Assigned( _canvas ) and ( _canvas.BeginSceneCount > 0 ) then begin
      if _flush then
        _canvas.EndScene ;
      exit ;
    end;

    if Assigned( oCanvas ) and ( bCanvasGpu or _flush ) then begin
      if Assigned( oCanvas ) then
        if oCanvas.Canvas.BeginSceneCount > 0 then
          oCanvas.Canvas.EndScene ;
      if Assigned( oChartsCanvas ) then
        if oChartsCanvas.Canvas.BeginSceneCount > 0 then
          oChartsCanvas.Canvas.EndScene ;
      if Assigned( oLabelsCanvas ) then
        if oLabelsCanvas.Canvas.BeginSceneCount > 0 then
          oLabelsCanvas.Canvas.EndScene ;
      if Assigned( oSelectionCanvas ) then
        if oSelectionCanvas.Canvas.BeginSceneCount > 0 then
          oSelectionCanvas.Canvas.EndScene ;
      if Assigned( oEditCanvas ) then
        if oEditCanvas.Canvas.BeginSceneCount > 0 then
          oEditCanvas.Canvas.EndScene ;
      if Assigned( oTransparentCanvas ) then
        if oTransparentCanvas.Canvas.BeginSceneCount > 0 then
          oTransparentCanvas.Canvas.EndScene ;
    end ;

    if Assigned( _canvas ) then
      _canvas.BeginScene
  end;

  procedure TGIS_RendererFmx.PrepareHourGlassContext ;
  begin
    verifyCanvas( nil ) ;
  end;

  procedure TGIS_RendererFmx.AfterDraw ;
  begin
    if assigned( oSelectionCanvas ) then begin
      if assigned( Context.Selection ) then begin
        if oSelectionCanvas.Canvas.BeginSceneCount > 0 then
          oSelectionCanvas.Canvas.EndScene ;
        FreeObject( oSelectionCanvas ) ;
      end else
        oSelectionCanvas := nil ;
    end ;

    if assigned( oChartsCanvas ) then begin
      if assigned( Context.Charts ) then begin
        if oChartsCanvas.Canvas.BeginSceneCount > 0 then
          oChartsCanvas.Canvas.EndScene ;
        FreeObject( oChartsCanvas ) ;
      end else
        oChartsCanvas := nil ;
    end ;

    if assigned( oLabelsCanvas ) then begin
      if assigned( Context.Labels ) then begin
        if oLabelsCanvas.Canvas.BeginSceneCount > 0 then
          oLabelsCanvas.Canvas.EndScene ;
        FreeObject( oLabelsCanvas ) ;
      end else
        oLabelsCanvas := nil ;
    end ;

    if oCanvas.Canvas.BeginSceneCount > 0 then
      oCanvas.Canvas.EndScene ;
    FreeObject( oCanvas ) ;
  end;

  procedure TGIS_RendererFMX.PrepareDrawCharts ;
  begin
    prevCanvas := FCanvas;
    prepareChartsCanvas;
    FCanvas := oLabelsCanvas ;
  end;

  procedure TGIS_RendererFMX.AfterDrawCharts ;
  begin
    FCanvas := prevCanvas ;
  end;

  procedure TGIS_RendererFMX.PrepareDrawLabels ;
  begin
    prevCanvas := FCanvas;
    prepareLabelsCanvas;
    FCanvas := oLabelsCanvas ;
  end;

  procedure TGIS_RendererFMX.AfterDrawLabels ;
  begin
    FCanvas := prevCanvas ;
  end;

  procedure TGIS_RendererFMX.LockTransparent(
    const _transparency : Integer
  ) ;
  begin
    assert( not assigned( oTransparentBitmap ) ) ;
    assert( not assigned( oTransparentCanvas ) ) ;
    if _transparency = 100 then exit ;

    oTransparentBitmap := T_FMXBitmap.Create( Width, Height ) ;
    T_FMXBitmap(oTransparentBitmap).Clear( TAlphaColorRec.Null ) ;
    oTransparentCanvas := TGIS_CanvasInternal.CreateCanvas( oTransparentBitmap ) ;

    oTransparentCanvas.transparency := _transparency ;

    verifyCanvas( oTransparentCanvas.Canvas ) ;

    FCanvas := oTransparentCanvas ;
  end ;

  procedure TGIS_RendererFMX.UnlockTransparent ;
  begin
    if assigned( oTransparentBitmap ) then begin
      if assigned( oTransparentCanvas ) then begin
        if oTransparentCanvas.Canvas.BeginSceneCount > 0 then
          oTransparentCanvas.Canvas.EndScene ;
        verifyCanvas( oCanvas.Canvas ) ;
        oCanvas.Canvas.DrawBitmap(
          oTransparentBitmap,
          RectF( 0, 0,
                 oTransparentBitmap.Width,
                 oTransparentBitmap.Height),
          RectF( 0, 0,
                 Width,
                 oTransparentBitmap.Height/
                 oTransparentBitmap.Width * Width),
          oTransparentCanvas.transparency / 100, True
        ) ;
        FreeObject( oTransparentCanvas ) ;
      end ;
      FreeObject( oTransparentBitmap ) ;
      FCanvas := oCanvas ;
    end ;
  end ;

  function TGIS_RendererFmx.prepareBitmapFill(
    const _shp     : TGIS_Shape ;
    const _bmp     : TBitmap
  ) : TBrushBitmap ;
  var
    extshp : TGIS_Extent ;
    extsrc  : TGIS_Extent ;
    w    : Integer     ;
    h    : Integer     ;
    cx   : Double      ;
    cy   : Double      ;
    x    : Double      ;
    y    : Double      ;
    bmp  : TBitmap     ;
    bmp2 : TBitmap     ;

  begin
    w := _bmp.Width  ;
    h := _bmp.Height ;

    if ( not ignorePPI ) and ( PPI <> 96 ) then begin
      bmp := TBitmap.Create( RoundS(w * PPI / 96),
                             RoundS(h * PPI / 96) );
      bmp.Clear( TAlphaColorRec.Null );
      bmp.Canvas.BeginScene ;
      bmp.Canvas.DrawBitmap( _bmp, RectF( 0, 0, _bmp.Width, _bmp.Height ),
                                   RectF( 0, 0, bmp.Width,  bmp.Height ),
                             1//, True
                           ) ;
      bmp.Canvas.EndScene ;
      w := bmp.Width  ;
      h := bmp.Height ;
    end else
      bmp := _bmp ;

    bmp2 := TBitmap.Create( w, h );
    try
      bmp2.Clear( TAlphaColorRec.Null );

      bmp2.Canvas.BeginScene ;

      if Assigned( _shp ) then begin
        extsrc := TGIS_Shape( sourceShape ).ProjectedExtent ;

        // like MapToScreenEx does
//        cx := (  ext.XMin + FExtentX ) * FZoom ;
//        cy := ( -ext.YMin + FExtentY ) * FZoom ;

        if ( ( ActiveRenderer and _Direct2d ) <> 0 ) or
           ( ( ActiveRenderer and _GdiPlus  ) <> 0 )
        then begin
           cx := (  extsrc.XMin + FExtentX ) * FZoom ;
           cy := ( -extsrc.YMin + FExtentY ) * FZoom ;
        end
        else begin
          extshp := _shp.ProjectedExtent ;

          cx := (  extsrc.XMin - extshp.Xmin ) * FZoom ;
          cy := (  extshp.YMin - extsrc.Ymin ) * FZoom ;
        end ;
      end
      else begin
        cx := 0 ;
        cy := 0 ;
      end;

      x := cx - Int( cx / w ) * w ;
      y := cy - Int( cy / h ) * h ;

//      if GlobalUseDirect2D = False then begin
        x := RoundS( x ) ;
        y := RoundS( y ) ;
//      end ;

      if cx < 0 then x := x + w ;
      if cy < 0 then y := y + h ;

      // make a pattern properly aligned to shape position within window

      bmp2.Canvas.DrawBitmap( bmp, RectF( w-x, h-y, w  , h   ),
                                   RectF( 0  , 0  , x  , y   ),
                              1//, True
                            ) ;
      bmp2.Canvas.DrawBitmap( bmp, RectF( 0  , h-y, w-x, h   ),
                                   RectF( x  , 0  , w  , y   ),
                              1//, True
                            ) ;
      bmp2.Canvas.DrawBitmap( bmp, RectF( w-x, 0  , w  , h-y ),
                                   RectF( 0  , y  , x  , h   ),
                              1//, True
                            ) ;

      bmp2.Canvas.DrawBitmap( bmp, RectF( 0  , 0  , w-x, h-y ),
                                   RectF( x  , y  , w  , h   ),
                              1//, True
                            ) ;

      bmp2.Canvas.EndScene ;

      Result := TBrushBitmap.Create ;
      Result.Bitmap := bmp2 ;
    finally
      bmp2.Free ;
      if bmp <> _bmp then // PPI <> 96
        bmp.Free ;
    end;
  end ;

  function TGIS_RendererFmx.preparePatternFill(
    const _shp     : TGIS_Shape ;
    const _pattern : TGIS_BrushStyle ;
    const _color   : TGIS_Color
  ) : TBrushBitmap ;
  var
    bmp     : TBitmap ;
  begin
    Result := nil;
    bmp := T_BrushCache(brushCache).GetBitmap( _pattern, _color ) ;
    if assigned( bmp ) then
      Result := prepareBitmapFill( _shp, bmp ) ;
  end;

  procedure TGIS_RendererFmx.preparePen(
    const _canvas        : TGIS_CanvasInternal ;
    const _shp           : TGIS_Shape      ;
    const _color         : TGIS_Color      ;
    const _style         : TGIS_PenStyle   ;
    const _bitmap        : TGIS_Bitmap     ;
    const _pattern       : TGIS_BrushStyle ;
    const _cap           : TGIS_LineCap    ;
    const _join          : TGIS_LineJoin   ;
    const _width         : Integer
  ) ;
  begin
    preparePen( _canvas, _shp, _color, _style, _bitmap, _pattern, _cap, _join, nil, _width ) ;
  end;

  procedure TGIS_RendererFmx.preparePen(
    const _canvas        : TGIS_CanvasInternal ;
    const _shp           : TGIS_Shape      ;
    const _color         : TGIS_Color      ;
    const _style         : TGIS_PenStyle   ;
    const _bitmap        : TGIS_Bitmap     ;
    const _pattern       : TGIS_BrushStyle ;
    const _cap           : TGIS_LineCap    ;
    const _join          : TGIS_LineJoin   ;
    const _dash          : TGIS_DashArray  ;
    const _width         : Integer
  ) ;
  begin
    if _width = 0 then
      T_Pen(_canvas.FmxPen).Style := TGIS_PenStyle.Clear
    else
      T_Pen(_canvas.FmxPen).Style := _style ;
    if assigned( _shp ) then begin
      T_Pen(_canvas.FmxPen).Bitmap  := _bitmap ;
      T_Pen(_canvas.FmxPen).Pattern := _pattern ;
    end else begin
      T_Pen(_canvas.FmxPen).Bitmap  := nil ;
      T_Pen(_canvas.FmxPen).Pattern := TGIS_BrushStyle.Solid ;
    end ;
    T_Pen(_canvas.FmxPen).Width    := _width ;
    T_Pen(_canvas.FmxPen).Color    := _color ;
    T_Pen(_canvas.FmxPen).LineCap  := _cap   ;
    T_Pen(_canvas.FmxPen).LineJoin := _join  ;
    T_Pen(_canvas.FmxPen).LineDash := _dash  ;
    T_Pen(_canvas.FmxPen).SelectPen( Self, _canvas, _shp ) ;
  end ;

  procedure TGIS_RendererFmx.prepareBrush(
    const _canvas  : TGIS_CanvasInternal ;
    const _shp     : TGIS_Shape  ;
    const _color   : TGIS_Color  ;
    const _bitmap  : TGIS_Bitmap ;
    const _pattern : TGIS_BrushStyle
  ) ;
  begin
    if not TGIS_Bitmap.IsNilOrEmpty( _bitmap ) then begin
      T_Brush(_canvas.FmxBrush).Bitmap := _bitmap ;
      T_Brush(_canvas.FmxBrush).Style  := TGIS_BrushStyle.Solid ;
    end
    else begin
      T_Brush(_canvas.FmxBrush).Bitmap := nil ;
      T_Brush(_canvas.FmxBrush).Style  := _pattern ;
    end ;
    T_Brush(_canvas.FmxBrush).Color    := _color ;
    T_Brush(_canvas.FmxBrush).SelectBrush( Self, _canvas, _shp ) ;
  end ;

  procedure TGIS_RendererFmx.prepareFont(
    const _canvas : TGIS_CanvasInternal ;
    const _name   : String  ;
    const _size   : Integer ;
    const _style  : TGIS_FontStyles
  ) ;
  begin
    _canvas.Canvas.Font.Family := _name ;
    _canvas.Canvas.Font.Size   := Max(
                                    1,
                                    _size * ( PPI / 96 ) * ( FontScale / 100 )
                                    {$IFDEF MSWINDOWS}
                                      * 96 / 72
                                    {$ENDIF}
                                  ) ;
    _canvas.Canvas.Font.Style := FMXFontStyle( _style ) ;
  end ;

  procedure TGIS_RendererFmx.drawMarker(
    const _path       : TPathData ;
    const _style      : TGIS_MarkerStyle  ;
    const _size       : Integer ;
    const _pt         : TPoint  ;
    const _asselected : Boolean
  ) ;
  var
    isize      : Integer ;

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
     Result := PointF( a, b )
    end ;


    procedure do_draw ;
    begin
      case _style of
        TGIS_MarkerStyle.Box           :
          begin
            _path.AddRectangle(
              RectF( px(-64), py(-64),
                     px( 64), py( 64)
                   ),
              0, 0, []
            ) ;
          end ;
        TGIS_MarkerStyle.Circle        :
          begin
            _path.AddEllipse(
              RectF( px(-64), py(-64),
                     px( 64), py( 64)
                   )
            ) ;
          end ;
        TGIS_MarkerStyle.Cross         :
          begin
            _path.MoveTo( p(-16, 64) ) ;
            _path.LineTo( p( 16, 64) ) ;
            _path.LineTo( p( 16, 16) ) ;
            _path.LineTo( p( 64, 16) ) ;
            _path.LineTo( p( 64,-16) ) ;
            _path.LineTo( p( 16,-16) ) ;
            _path.LineTo( p( 16,-64) ) ;
            _path.LineTo( p(-16,-64) ) ;
            _path.LineTo( p(-16,-16) ) ;
            _path.LineTo( p(-64,-16) ) ;
            _path.LineTo( p(-64, 16) ) ;
            _path.LineTo( p(-16, 16) ) ;

            _path.ClosePath ;
          end ;
        TGIS_MarkerStyle.DiagCross     :
          begin
            _path.MoveTo( p(-56, 34) ) ;
            _path.LineTo( p(-34, 56) ) ;
            _path.LineTo( p(  0, 22) ) ;
            _path.LineTo( p( 34, 56) ) ;
            _path.LineTo( p( 56, 34) ) ;
            _path.LineTo( p( 22,  0) ) ;
            _path.LineTo( p( 56,-34) ) ;
            _path.LineTo( p( 34,-56) ) ;
            _path.LineTo( p(  0,-22) ) ;
            _path.LineTo( p(-34,-56) ) ;
            _path.LineTo( p(-56,-34) ) ;
            _path.LineTo( p(-22,  0) ) ;

            _path.ClosePath ;
          end ;
        TGIS_MarkerStyle.TriangleUp    :
          begin
            _path.MoveTo( p(-64,-64) ) ;
            _path.LineTo( p(  0, 64) ) ;
            _path.LineTo( p( 64,-64) ) ;

            _path.ClosePath ;
          end ;
        TGIS_MarkerStyle.TriangleDown  :
          begin
            _path.MoveTo( p( 0,-64) ) ;
            _path.LineTo( p(-64, 64) ) ;
            _path.LineTo( p( 64, 64) ) ;

            _path.ClosePath ;
          end ;
        TGIS_MarkerStyle.TriangleLeft  :
          begin
            _path.MoveTo( p(-64, 0) ) ;
            _path.LineTo( p( 64, 64) ) ;
            _path.LineTo( p( 64,-64) ) ;

            _path.ClosePath ;
          end ;
        TGIS_MarkerStyle.TriangleRight :
          begin
            _path.MoveTo( p(-64, 64) ) ;
            _path.LineTo( p( 64, 0) ) ;
            _path.LineTo( p(-64,-64) ) ;

            _path.ClosePath ;
          end ;
        end ;
      end ;
    begin
      if _size = 0 then exit ;

      isize := _size ;

      if not _asselected then begin
        do_draw ;
      end ;

      if _asselected then begin
        _path.AddRectangle(
          RectF( _pt.X - isize div 2, _pt.Y - isize div 2,
                 _pt.X + isize div 2, _pt.Y + isize div 2
               ),
          0, 0, []
        ) ;
      end ;
  end ;

  procedure TGIS_RendererFmx.doShapePoint(
    const _shp           : TGIS_ShapePoint ;
    const _source        : TGIS_ShapePoint ;
    const _selectionOnly : Boolean
  ) ;
  var
    params_marker : TGIS_ParamsMarker ;
    angle         : Double ;

    procedure draw_point ;
    var
      pt_x   : Integer ;
      pt_y   : Integer ;
      isize  : Integer ;
      mypath : TPathData ;
      offset : TPoint ;

      procedure preparePoint ;
      var
        dx, dy : Double ;
      begin
        _shp.GetPointEx( 0, 0, dx, dy ) ;

        // like MapToScreen does
        pt_x := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
        pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
      end ;

    begin
      FCanvas.usePen := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_marker ) ;
      mypath := TPathData.Create ;
      try
        preparePoint ;

        if params_marker.Size <> GIS_RENDER_SIZE then
          isize := twipsToPixels( params_marker.Size )
        else
          isize := twipsToPixels( _shp.Layer.SelectionWidth ) ;

        if not _selectionOnly then begin

          if Assigned( params_marker.Symbol ) then begin
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
            drawMarker( mypath, params_marker.Style, isize,
                        Point( pt_x, pt_y ), False ) ;
            prepareBrush(
              FCanvas,
              _shp,
              params_marker.Color,
              params_marker.Bitmap,
              params_marker.Pattern
            ) ;
            FCanvas.Canvas.FillPath( mypath, 1 ) ;
            preparePen(
              FCanvas,
              _shp,
              params_marker.OutlineColor,
              params_marker.OutlineStyle,
              params_marker.OutlineBitmap,
              params_marker.OutlinePattern,
              TGIS_LineCap.Square,
              TGIS_LineJoin.Miter,
              twipsToPixels( _shp.Params.Marker.OutlineWidth )
            ) ;
            FCanvas.Canvas.DrawPath( mypath, 1 ) ;
          end ;
        end ;

        if _shp.IsSelected or _selectionOnly then
        begin
          prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                  or pextra, pextra, _shp ) ;

          mypath.Clear ;
          verifyCanvas( oSelectionCanvas.Canvas ) ;
          drawMarker( mypath, params_marker.Style, isize,
                      Point( pt_x, pt_y ), True ) ;
          if _shp.Layer.SelectionOutlineOnly then
            prepareBrush(
              oSelectionCanvas,
              _shp,
              colorSelection,
              nil,
              TGIS_BrushStyle.Clear
            )
          else
            prepareBrush(
              oSelectionCanvas,
              _shp,
              colorSelection,
              nil,
              TGIS_BrushStyle.Solid
            ) ;
          oSelectionCanvas.Canvas.FillPath( mypath, 1 ) ;
          preparePen(
            oSelectionCanvas,
            _shp,
            colorSelection,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            TGIS_LineCap.Round,
            TGIS_LineJoin.Miter,
            twipsToPixels( _shp.Layer.SelectionWidth )
          ) ;
          oSelectionCanvas.Canvas.DrawPath( mypath, 1 ) ;
        end ;

      finally
        FreeObject( mypath ) ;
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

  procedure TGIS_RendererFmx.doShapeMultiPoint(
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
      mypath : TPathData ;
      offset : TPoint ;

      procedure prepare_point( _idx : Integer ) ;
      var
        dx, dy : Double ;
      begin
        _shp.GetPointEx( 0, _idx, dx, dy ) ;
        pt_x := RoundS( (  dx + FExtentX ) * FZoom ) + offset.X ;
        pt_y := RoundS( ( -dy + FExtentY ) * FZoom ) + offset.Y ;
      end ;

    begin
      FCanvas.usePen := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_marker ) ;
      mypath := TPathData.Create ;
      try
        if params_marker.Size <> GIS_RENDER_SIZE then
          isize := twipsToPixels( params_marker.Size )
        else
          isize := twipsToPixels( _shp.Layer.SelectionWidth ) ;

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
            for i := 0 to _shp.GetNumPoints -1 do begin
              myPath.Clear ;
              prepare_point( i ) ;
              drawMarker( mypath, params_marker.Style, isize,
                          Point( pt_x, pt_y ), False ) ;
              prepareBrush(
                FCanvas,
                _shp,
                params_marker.Color,
                params_marker.Bitmap,
                params_marker.Pattern
              ) ;
              FCanvas.Canvas.FillPath( mypath, 1 ) ;
              preparePen(
                FCanvas,
                _shp,
                params_marker.OutlineColor,
                params_marker.OutlineStyle,
                params_marker.OutlineBitmap,
                params_marker.OutlinePattern,
                TGIS_LineCap.Round,
                TGIS_LineJoin.Round,
                twipsToPixels( params_marker.OutlineWidth )
              ) ;
              FCanvas.Canvas.DrawPath( mypath, 1 ) ;
            end ;
          end ;
        end ;

        if _shp.IsSelected or _selectionOnly then begin
          prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                  or pextra, pextra, _shp ) ;

          mypath.Clear ;
          verifyCanvas( oSelectionCanvas.Canvas ) ;
          for i := 0 to _shp.GetNumPoints -1 do begin
            prepare_point( i ) ;
            drawMarker( myPath, params_marker.Style, isize,
                        Point( pt_x, pt_y ), True ) ;
          end ;
          if _shp.Layer.SelectionOutlineOnly then
            prepareBrush(
              oSelectionCanvas,
              _shp,
              colorSelection,
              nil,
              TGIS_BrushStyle.Clear
            )
          else
            prepareBrush(
              oSelectionCanvas,
              _shp,
              colorSelection,
              nil,
              TGIS_BrushStyle.Solid
            ) ;
          oSelectionCanvas.Canvas.FillPath( mypath, 1 ) ;
          preparePen(
            oSelectionCanvas,
            _shp,
            colorSelection,
            TGIS_PenStyle.Solid,
            nil,
            TGIS_BrushStyle.Solid,
            TGIS_LineCap.Round,
            TGIS_LineJoin.Round,
            twipsToPixels( _shp.Layer.SelectionWidth )
          ) ;
          oSelectionCanvas.Canvas.DrawPath( mypath, 1 ) ;
        end ;

      finally
        FreeObject( myPath ) ;
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

  procedure TGIS_RendererFmx.doShapeLine(
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
      mypath    : TPathData ;
      i         : Integer ;
      ar        : TGIS_DrawBufF ;
      selwidth  : Integer ;
      max_tlrnc : Integer ;
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
        pt_x       : Double ;
        pt_y       : Double ;
        last_pt_x  : Double ;
        last_pt_y  : Double ;
        tmp_tlrnc  : Integer ;
        isize      : Integer ;
      begin
        Result := 0 ;

        part_size := _shp.GetPartSize( _part_no ) ;
        if part_size <= 0 then exit ;

        cur_tlrnc := _max_tlrnc ;
        repeat
          isize := 0  ;

          mypath.Clear ;

          for point_no := 0 to part_size - 1 do begin
            _shp.GetPointEx( _part_no, point_no, dx, dy ) ;

            // like MapToScreen does
            pt_x :=  ( (  dx + FExtentX ) * FZoom ) + offset.X ;
            pt_y :=  ( ( -dy + FExtentY ) * FZoom ) + offset.Y ;

            if point_no = 0 then begin
              mypath.MoveTo( PointF( pt_x, pt_y ) );
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
                mypath.LineTo( PointF( pt_x, pt_y ) );
                last_pt_x := pt_x ;
                last_pt_y := pt_y ;
                inc( isize ) ;
              end ;
            end
            else begin
              mypath.LineTo( PointF( pt_x, pt_y ) );
              inc( isize ) ;
            end ;
          end ;

          cur_tlrnc := cur_tlrnc + 1 ;
        until isize < GDI_MAXPOINT_COUNT ;

        Result := isize ;
      end ;

    begin
      FCanvas.usePen := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_line ) ;
      for part_no := 0 to _shp.GetNumParts - 1 do
      begin
        mypath := TPathData.Create ;
        try
          if prepare_drawbufpart( iTolerance, part_no ) < 1 then exit ;

          if not _selectionOnly then begin

            // first draw outline
            if ( Integer(_outlineMode )
                 and
                 Integer( TGIS_RendererMultipassMode.Outline )
               ) <> 0
            then begin
              if params_line.OutlineWidth <> 0 then begin
                if params_line.Symbol = nil then begin
                  preparePen(
                    FCanvas,
                    _shp,
                    params_line.OutlineColor,
                    params_line.OutlineStyle,
                    params_line.OutlineBitmap,
                    params_line.OutlinePattern,
                    TGIS_LineCap.Round,
                    TGIS_LineJoin.Round,
                    twipsToPixels( params_line.Width )
                    + 2* twipsToPixels( params_line.OutlineWidth )
                  ) ;
                  if mypath.Count = 2 then
                    FCanvas.Canvas.DrawLine( mypath[0].Point, mypath[1].Point, 1 )
                  else
                    FCanvas.Canvas.DrawPath( mypath, 1 );
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
                // prepare an array of points
                SetLength( ar, mypath.Count ) ;
                for i:=0 to mypath.Count -1 do
                  ar[i] := mypath.Points[i].Point ;
                TGIS_SymbolLineHelper.DrawLine( Viewer, ar,
                                                params_line.Symbol, mypath.Count
                                              ) ;
                params_line.Symbol.Unprepare ;
              end
              else begin
                preparePen(
                  FCanvas,
                  _shp,
                  params_line.Color,
                  params_line.Style,
                  params_line.Bitmap,
                  params_line.Pattern,
                  TGIS_LineCap.Round,
                  TGIS_LineJoin.Round,
                  twipsToPixels( params_line.Width )
                ) ;
                if mypath.Count = 2 then
                  FCanvas.Canvas.DrawLine( mypath[0].Point, mypath[1].Point, 1 )
                else
                  FCanvas.Canvas.DrawPath( mypath, 1 ) ;
              end ;
            end ;
          end ;

          if _shp.IsSelected or _selectionOnly then begin
            prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                    or pextra, pextra, _shp ) ;

            // draw selected
            selwidth := twipsToPixels( _shp.Layer.SelectionWidth ) ;
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
              selwidth := selwidth + twipsToPixels( params_line.Width )
                          + 2* twipsToPixels( params_line.OutlineWidth ) ;
            end ;

            mypath.Clear ;
            if prepare_drawbufpart( max_tlrnc, part_no ) < 1 then exit ;

            verifyCanvas( oSelectionCanvas.Canvas ) ;
            oSelectionCanvas.Canvas.Stroke.Kind      := TBrushKind.Solid    ;
            oSelectionCanvas.Canvas.Stroke.Color     := colorSelection.ARGB ;
            oSelectionCanvas.Canvas.Stroke.Thickness := selwidth            ;
            oSelectionCanvas.Canvas.Stroke.Join      := TStrokeJoin.Round   ;
            oSelectionCanvas.Canvas.Stroke.Cap       := TStrokeCap.Round    ;

            if mypath.Count = 2 then
              oSelectionCanvas.Canvas.DrawLine( mypath[0].Point,
                                                mypath[1].Point, 1 )
            else
              oSelectionCanvas.Canvas.DrawPath( mypath, 1 ) ;
          end ;
        finally
          FreeObject( mypath );
        end ;
      end ;
    end ;

  begin
    assert( Assigned( _shp ) ) ;
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
  end;

  procedure TGIS_RendererFmx.doShapePolygon(
    const _shp           : TGIS_ShapePolygon ;
    const _source        : TGIS_ShapePolygon ;
    const _selectionOnly : Boolean ;
    const _part          : Integer
  ) ;
  var
    params_area : TGIS_ParamsArea ;
    rect_screen : TRect           ;

    procedure draw_polygon ;
    var
      mypath    : TPathData ;
      part_buf  : TGIS_IntegerArray ;
      tmp       : Integer ;
      i, j, k   : Integer ;
      ar        : TGIS_DrawBufF ;
      selwidth  : Integer ;
      max_tlrnc : Integer ;
      offset    : TPoint ;

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
        pt_x       : Double     ;
        pt_y       : Double     ;
        last_pt_x  : Double     ;
        last_pt_y  : Double     ;
        first_pt_x : Double     ;
        first_pt_y : Double     ;
        tmp_tlrnc  : Integer    ;
        iparts     : Integer    ;
        isize      : Integer    ;
        start      : Integer    ;
        stop       : Integer    ;
      begin
        Result := 0 ;

        cur_tlrnc := _max_tlrnc ;
        repeat
          iparts  := 0 ;
          isize   := 0; ;

          mypath.Clear ;

          if _part = -1 then begin
            start := 0 ;
            stop  := _shp.GetNumParts - 1 ;
          end
          else begin
            start := _part ;
            stop  := _part ;
          end ;

          for part_no := start to stop do begin
            part_size := _shp.GetPartSize( part_no ) ;

            cur_size := 0 ;
            for point_no := 0 to part_size - 1 do begin
              _shp.GetPointEx( part_no, point_no, dx, dy ) ;

              // like MapToScreen does
              pt_x :=  ( (  dx + FExtentX ) * FZoom ) + offset.X ;
              pt_y :=  ( ( -dy + FExtentY ) * FZoom ) + offset.Y ;

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
                    mypath.MoveTo( PointF( first_pt_x, first_pt_y ) );
                    inc( isize ) ;
                    inc( cur_size ) ;
                  end;

                  mypath.LineTo( PointF( pt_x, pt_y ) );
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
                  if tmp_tlrnc > cur_tlrnc then begin
                    mypath.MoveTo( PointF( first_pt_x, first_pt_y ) );
                    inc( isize ) ;
                    inc( cur_size ) ;
                  end
                  else begin
                    mypath.MoveTo( PointF(  pt_x  ,  pt_y   ) );
                    inc( isize ) ;
                    inc( cur_size ) ;
                    mypath.LineTo( PointF(  pt_x  ,  pt_y+1 ) );
                    inc( isize ) ;
                    inc( cur_size ) ;
                    mypath.LineTo( PointF(  pt_x+1,  pt_y+1  ) );
                    inc( isize ) ;
                    inc( cur_size ) ;
                    mypath.LineTo( PointF(  pt_x+1,  pt_y   ) );
                    inc( isize ) ;
                    inc( cur_size ) ;
                  end ;
                end;
                if cur_size > 0  then begin
                  mypath.LineTo( PointF(  pt_x,  pt_y ) );

                  inc( isize ) ;
                  inc( cur_size ) ;
                  mypath.ClosePath ;
                  inc( isize ) ;
                  inc( cur_size ) ;

                  part_buf[iparts] := cur_size ;
                  inc(iparts);
                end;
               end ;
            end ;
          end ;

          cur_tlrnc := cur_tlrnc + 1 ;
        until isize < GDI_MAXPOINT_COUNT ;

        Result := isize ;
      end ;

      procedure draw_polygon_symbolfill ;
      var
        fill_gap  : TPoint ;
        fill_size : TPoint ;
        bmp  : TBitmap ;
        cnv  : TCanvas ;
        tbmp : TGIS_Bitmap ;
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
          fill_gap.X  := twipsToPixels( params_area.SymbolGap ) ;
          fill_gap.Y  := twipsToPixels( params_area.SymbolGap ) ;
          fill_size.X := params_area.Symbol.Width  + fill_gap.X ;
          fill_size.Y := params_area.Symbol.Height + fill_gap.Y ;
          bmp := TBitmap.Create( fill_size.X, fill_size.Y );
          bmp.Clear( TAlphaColorRec.Null ) ;
          cnv := FCanvas.Canvas ;
          FCanvas.Canvas := bmp.Canvas ;
          FCanvas.Canvas.BeginScene ;
          try
            params_area.Symbol.Draw( params_area.Symbol.Width div 2,
                                     params_area.Symbol.Height div 2 ) ;
          finally
            FCanvas.Canvas.EndScene ;
            FCanvas.Canvas := cnv ;
          end;

          tbmp := TGIS_Bitmap.Create ;
          tbmp.NativeBitmap := bmp ;
          ignorePPI := True ;
          try
            prepareBrush(
              FCanvas,
              _shp,
              params_area.Color,
              tbmp ,
              TGIS_BrushStyle.Solid
            ) ;
            FCanvas.Canvas.FillPath( mypath, 1 );
          finally
            ignorePPI := False ;
            FreeObject( tbmp ) ;
          end ;
        finally
          params_area.Symbol.Unprepare ;
        end ;
      end ;

      procedure draw_polygon_hatchfill ;
      var
        fill_gap  : TPoint ;
        fill_size : TPoint ;
        bmp  : TBitmap ;
        cnv  : TCanvas ;
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
        rvis : TRect ;
        rct : TRect ;
        rct_left : Integer ;
        rct_top : Integer ;
        rct_right : Integer ;
        rct_bottom : Integer ;
        x, y : Integer ;
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

        rvis := Rect( 0, 0, FCanvas.Canvas.Width, FCanvas.Canvas.Height ) ;
        if corner1_x < 0 then
          rct_Left := fill_offset.X
        else
          rct_Left := RoundS( corner1_x ) ;
        if corner2_x > rvis.Right then
          rct_Right := rvis.Right
        else
          rct_Right := RoundS( corner2_x ) ;
        if corner1_y < 0 then
          rct_Top := fill_offset.Y
        else
          rct_Top := RoundS( corner1_y ) ;
        if corner2_y > rvis.Bottom then
          rct_Bottom := rvis.Bottom
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

          bmp := TBitmap.Create( rct.Width, rct.Height );
          bmp.Clear( TAlphaColorRec.Null ) ;
          cnv := FCanvas.Canvas ;
          FCanvas.Canvas := bmp.Canvas ;
          FCanvas.Canvas.BeginScene ;
          try
            params_area.Symbol.Draw( rct.Width, rct.Height ) ;
          finally
            FCanvas.Canvas.EndScene ;
            FCanvas.Canvas := cnv ;
          end;

          tbmp := TGIS_Bitmap.Create ;
          tbmp.NativeBitmap := bmp ;
          ignorePPI := True ;
          try
            prepareBrush(
              FCanvas,
              _shp,
              params_area.Color,
              tbmp ,
              TGIS_BrushStyle.Solid
            ) ;
            FCanvas.Canvas.FillPath( mypath, 1 );
          finally
            ignorePPI := False ;
            FreeObject( tbmp ) ;
          end ;
        params_area.Symbol.Unprepare ;
      end ;


    begin
      SetLength( part_buf, _shp.GetNumParts  ) ;

      FCanvas.usePen   := True ;
      FCanvas.useBrush := True ;

      offset := getOffsetPoint( params_area ) ;
      mypath := TPathData.Create ;
      try

        // prepare points (scale, etc)
        if  prepare_drawbuf( iTolerance ) < 1 then exit ;

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
            if ( not TGIS_Bitmap.IsNilOrEmpty( params_area.Bitmap ) ) and
               Assigned( _shp.Layer )                                 and
               ( _shp.Layer.View3D.Mode <> TGIS_3DLayerType.Dem )
            then
              prepareBrush(
                FCanvas,
                _shp,
                params_area.Color,
                params_area.Bitmap,
                params_area.Pattern
              )
            else
              prepareBrush(
                FCanvas,
                _shp,
                params_area.Color,
                nil,
                params_area.Pattern
              );
            FCanvas.Canvas.FillPath( mypath, 1 );
          end ;

          // line background for dashed lines
          if ( params_area.OutlineSymbol         <> nil                   ) and
             ( params_area.OutlineStyle          <> TGIS_PenStyle.Solid   ) and
             ( params_area.OutlineBackcolor.ARGB <> TGIS_Color.Crazy.ARGB ) then
          begin
            preparePen(
              FCanvas,
              _shp,
              params_area.OutlineBackcolor,
              TGIS_PenStyle.Solid,
              nil,
              TGIS_BrushStyle.Solid,
              TGIS_LineCap.Round,
              TGIS_LineJoin.Round,
              twipsToPixels( params_area.OutlineWidth )
            ) ;
            FCanvas.Canvas.DrawPath( mypath, 1 );
          end ;

          // outline symbol fill
          if assigned( params_area.OutlineSymbol ) then
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
            // draw parts
            k := 0 ;


            for i := 0 to length(part_buf) - 1 do begin
              // prepare an array of points
              SetLength( ar, part_buf[i] ) ;
              for j := 0 to part_buf[i] -1 do
                ar[j] := mypath.Points[k+j].Point ;
              TGIS_SymbolLineHelper.DrawLine( Viewer, ar,
                                              params_area.OutlineSymbol,
                                              part_buf[i] ) ;
              k := k + part_buf[i] ;
            end;

            params_area.OutlineSymbol.Unprepare ;
          end

          // outline pattern fill
          else if ( params_area.OutlinePattern <> TGIS_BrushStyle.Clear ) and
                  ( params_area.OutlinePattern <> TGIS_BrushStyle.Solid ) then
          begin
            preparePen(
              FCanvas,
              _shp,
              params_area.OutlineColor,
              params_area.OutlineStyle,
              params_area.OutlineBitmap,
              params_area.OutlinePattern,
              TGIS_LineCap.Round,
              TGIS_LineJoin.Round,
              twipsToPixels( params_area.OutlineWidth )
            ) ;
            FCanvas.Canvas.DrawPath( mypath, 1 );
          end

          // outline bitmap fill
          // standard outline
          else begin
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
                  _shp,
                  params_area.Color,
                  TGIS_PenStyle.Solid,
                  nil  ,
                  TGIS_BrushStyle.Solid,
                  TGIS_LineCap.Round,
                  TGIS_LineJoin.Round,
                  TwipsToPixels( 1 )
                ) ;
                FCanvas.Canvas.DrawPath( mypath, 1 );
              end ;
            end
            else
            if ( params_area.OutlineWidth <> 0 ) and
               ( params_area.OutlineStyle <> TGIS_PenStyle.Clear )
            then begin
              preparePen(
                FCanvas,
                _shp,
                params_area.OutlineColor,
                params_area.OutlineStyle,
                params_area.OutlineBitmap,
                params_area.OutlinePattern,
                TGIS_LineCap.Flat,
                TGIS_LineJoin.Round,
                TwipsToPixels( params_area.OutlineWidth )
              ) ;
              FCanvas.Canvas.DrawPath( mypath, 1 );
            end
            else begin
              // OutlineWidth = 0 and pattern fill - do nothing
            end ;
          end ;
        end ;

        // draw selected
        if _shp.IsSelected or _selectionOnly then begin
          prepareSelectionCanvas( ( not _shp.Layer.CachedPaint and not flashed )
                                  or pextra, pextra, _shp ) ;

          selwidth := twipsToPixels( _shp.Layer.SelectionWidth ) ;
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

          mypath.Clear ;
          if  prepare_drawbuf( iTolerance ) < 1 then exit ;

          verifyCanvas( oSelectionCanvas.Canvas ) ;

          if params_area.OutlineWidth <> GIS_RENDER_SIZE then
            selwidth := selwidth +
                        twipsToPixels( params_area.OutlineWidth ) ;

          oSelectionCanvas.Canvas.Fill.Kind     := TBrushKind.Solid    ;
          oSelectionCanvas.Canvas.Fill.Color    := colorSelection.ARGB ;

          oSelectionCanvas.Canvas.FillPath( mypath, 1 );

          oSelectionCanvas.Canvas.Stroke.Kind      := TBrushKind.Solid    ;
          oSelectionCanvas.Canvas.Stroke.Color     := colorSelection.ARGB ;
          oSelectionCanvas.Canvas.Stroke.Thickness := selwidth            ;
          oSelectionCanvas.Canvas.Stroke.Join      := TStrokeJoin.Round   ;
          oSelectionCanvas.Canvas.Stroke.Cap       := TStrokeCap.Round    ;

          oSelectionCanvas.Canvas.DrawPath( mypath, 1 );
        end ;
      finally
        FreeObject( mypath );
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
    rect_screen := Rect( 0, 0, TBitmap(Context.BaseMap).Width, TBitmap(Context.BaseMap).Height ) ;
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

  procedure TGIS_RendererFmx.doShapeMultiPatch(
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

  procedure TGIS_RendererFmx.doShapeComplex(
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
                   _selectionOnly, _outlineMode ) ;
  end ;

  procedure TGIS_RendererFmx.RenderShape(
    const _shp           : TObject ;
    const _selectionOnly : Boolean ;
    const _outlineMode   : TGIS_RendererMultipassMode
  ) ;
  begin
    RenderShape( _shp, nil, _selectionOnly, _outlineMode ) ;
  end ;

  procedure TGIS_RendererFmx.RenderShape(
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
      verifyCanvas( FCanvas.Canvas ) ;

      case TGIS_Shape( _shp ).ShapeType of
        TGIS_ShapeType.Point       :
            doShapePoint     ( TGIS_ShapePoint( _shp ),
                               TGIS_ShapePoint( _source ),
                               _selectionOnly
                             ) ;
        TGIS_ShapeType.MultiPoint  :
            doShapeMultiPoint( TGIS_ShapeMultiPoint( _shp ),
                               TGIS_ShapeMultiPoint( _source ),
                               _selectionOnly
                             ) ;
        TGIS_ShapeType.Arc         :
            doShapeLine      ( TGIS_ShapeArc( _shp ),
                               TGIS_ShapeArc( _source ),
                               _selectionOnly,
                               _outlineMode
                             ) ;
        TGIS_ShapeType.Polygon     :
            doShapePolygon   ( TGIS_ShapePolygon( _shp ),
                               TGIS_ShapePolygon( _source ),
                               _selectionOnly,
                               -1
                             ) ;
        TGIS_ShapeType.MultiPatch  :
            doShapeMultiPatch( TGIS_ShapeMultiPatch( _shp ),
                               TGIS_ShapeMultiPatch( _source ),
                               _selectionOnly
                             ) ;
        TGIS_ShapeType.Complex :
            doShapeComplex   ( TGIS_ShapeComplex( _shp ),
                               TGIS_ShapeComplex( _source ),
                               _selectionOnly,
                               _outlineMode
                             )
      else
            Assert( False, GIS_RS_ERR_UNTESTED ) ;
      end ;
    finally
      FCanvas := cnv ;
    end;
  end ;

  procedure TGIS_RendererFmx.doLabelPoint(
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
    offset          : TPoint       ;
    gap             : Integer      ;
    gap2            : Integer      ;
    transform       : TMatrix      ;
    ssin            : Double       ;
    scos            : Double       ;
    lbl             : TGIS_HtmlLabel ;
    params_marker   : TGIS_ParamsMarker    ;
    params_label    : TGIS_ParamsLabel     ;
    label_alignment : TGIS_LabelAlignment  ;
    label_positions : TGIS_LabelPositions  ;

    function text_extent
     : TPoint ;
    var
      rect : TRect ;
    begin
      rect := lbl.BoundingBox ;
      Result.X := rect.Right  - rect.Left + 1 ;
      Result.Y := rect.Bottom - rect.Top  + 1 ;
    end;

     function text_out(
       const _rect      : TRect   ;
       const _shadow    : Boolean ;
       const _angle     : Double  ;
       const _origin    : TPoint
     ) : TRect ;
     var
       shadow_width : Integer ;
     begin
       shadow_width := 0 ;
       if _shadow then begin
         // prepare size of outline in pixels
         // generally outline must be composed of 1 screen pixels (1/96 of inch)
         // for printer outline will must be a bit stronger then usual
         shadow_width := Max( 1, PPI div 96 ) ;
       end ;
       lbl.Draw( _rect, 0, _origin, shadow_width, _shp.Params.Labels.Color ) ;
    end;

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
        _shp.Layer.LabelPosEvent( TObject(Viewer), _shp, _pos, start, rct ) ;
        start_x := start.X ;
        start_y := start.Y ;
      end ;

      rct_loc_left   := rct_tmp.Left   + start_x ;
      rct_loc_top    := rct_tmp.Top    + start_y ;
      rct_loc_right  := rct_tmp.Right  + start_x ;
      rct_loc_bottom := rct_tmp.Bottom + start_y ;

      if angle <> 0 then begin
        // like GisRotatePoint does
        ssin := transform.m12 ;
        scos := transform.m11 ;

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
            _shp,
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
            _shp,
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
        if oLabelsCanvas.useBrush then begin
          oLabelsCanvas.Canvas.FillRect(
            RectF( rct_txt.Left   + rct_loc_left,
                   rct_txt.Top    + rct_loc_top ,
                   rct_txt.Right  + rct_loc_left,
                   rct_txt.Bottom + rct_loc_top
                 ),
            0, 0, [],
            1
          ) ;
        end;
        if oLabelsCanvas.usePen then begin
          oLabelsCanvas.Canvas.DrawRect(
            RectF( rct_txt.Left   + rct_loc_left,
                   rct_txt.Top    + rct_loc_top ,
                   rct_txt.Right  + rct_loc_left,
                   rct_txt.Bottom + rct_loc_top
                 ),
            0, 0, [],
            1
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
        Rect( RoundS( rct_txt.Left   + rct_loc_left + txt_off_x ),
              RoundS( rct_txt.Top    + rct_loc_top  + txt_off_y ),
              RoundS( rct_txt.Right  + rct_loc_left - txt_off_x ),
              RoundS( rct_txt.Bottom + rct_loc_top  - txt_off_y )
            ),
        bshadow,
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
                 twipsToPixels( params_label.Width,  0 ),
                 twipsToPixels( params_label.Height, 0 )
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
    end;

    if angle <> 0 then begin
      CanvasSetTransformation( angle, pt_origin_x, pt_origin_Y );
      {$IFDEF LEVEL_XE5_FMX}
        transform := TMatrix.Identity ;
      {$ELSE}
        transform := IdentityMatrix;
      {$ENDIF}
      transform.m31 := pt_origin_x;
      transform.m32 := pt_origin_y;
      {$IFDEF LEVEL_XE5_FMX}
        transform := TMatrix.CreateRotation(angle) * transform ;
      {$ELSE}
        transform := MatrixMultiply( CreateRotationMatrix(angle), transform );
      {$ENDIF}
      oLabelsCanvas.Canvas.SetMatrix( transform );
    end;

    lbl := nil ;
    try

      // calculate the size of the rectangle
      rct_br.X := rct.Right  ;
      rct_br.Y := rct.Bottom ;

      lbl := TGIS_HtmlLabel.Create( self, txt, label_alignment,
                                    rct_br.X,
                                    rct_br.Y
                                  ) ;
      rct_br := text_extent ;

      rct := Rect( 0, 0,
                   rct_br.X-1, rct_br.Y-1
                 ) ;

      if ( ( rct.Right  - rct.Left ) <= 0 ) or
         ( ( rct.Bottom - rct.Top  ) <= 0 )
      then exit ;

      if rct.Right >  twipsToPixels( params_label.Width,  0 ) then
         rct_br.X  := twipsToPixels( params_label.Width,  0 ) ;
      if rct.Bottom > twipsToPixels( params_label.Height, 0 ) then
         rct_br.Y :=  twipsToPixels( params_label.Height, 0 ) ;

      // calculate offset of rectangle
      if _shp.ShapeType in [TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint] then begin
        gap := twipsToPixels( params_marker.Size ) div 2 ;
        gap := gap + twipsToPixels( params_marker.OutlineWidth ) ;
      end
      else
        gap := 0 ;

      if ( _shp.Params.Chart.Size <> 0 ) and
         not IsStringEmpty( _shp.Params.Chart.Values ) then
      begin
        gap2 := twipsToPixels( _shp.Params.Chart.Size ) div 2 ;
        if gap2 > gap then gap := gap2 ;
      end ;

      gap := gap + twipsToPixels( params_label.OutlineWidth ) ;
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
      FreeObject ( lbl ) ;
      // The transformation should be rather reset in draw_label(),
      // but at exceptions it is a place for resetting.
      if angle <> 0 then begin
        {$IFDEF LEVEL_XE5_FMX}
          oLabelsCanvas.Canvas.SetMatrix( TMatrix.Identity ) ;
        {$ELSE}
          oLabelsCanvas.Canvas.SetMatrix( IdentityMatrix ) ;
        {$ENDIF}
      end ;
    end ;
  end;

  procedure TGIS_RendererFmx.doLabelArc(
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

  procedure TGIS_RendererFmx.RenderLabel(
    const _shp : TObject
  ) ;
  var
    cnv : TGIS_CanvasInternal ;
    arr : TGIS_DrawBuf ;
  begin
    prepareLabelsCanvas ;

    cnv := FCanvas ;
    sourceShape := _shp ;
    try
      FCanvas := oLabelsCanvas ;
      verifyCanvas( FCanvas.Canvas ) ;
      case TGIS_Shape( _shp ).ShapeType of
        TGIS_ShapeType.Point       :
            doLabelPoint( TGIS_ShapeArc( _shp ), False, arr ) ;
        TGIS_ShapeType.MultiPoint  :
            doLabelPoint( TGIS_ShapeArc( _shp ), False, arr ) ;
        TGIS_ShapeType.Arc         :
            doLabelArc( TGIS_ShapeArc( _shp ), False, arr ) ;
        TGIS_ShapeType.Polygon     :
            doLabelPoint( TGIS_ShapeArc( _shp ), False, arr ) ;
        TGIS_ShapeType.MultiPatch  :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        TGIS_ShapeType.Complex  :
            doLabelPoint( TGIS_Shape( _shp ), False, arr ) ;
        else
            Assert( False, GIS_RS_ERR_UNTESTED ) ;
      end;
    finally
      sourceShape := nil ;
      FCanvas := cnv ;
    end ;
  end ;

  procedure TGIS_RendererFmx.RenderLabel(
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
      verifyCanvas( FCanvas.Canvas ) ;
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
            Assert( False, GIS_RS_ERR_UNTESTED ) ;
      end;
    finally
      FCanvas := cnv ;
    end ;
  end ;

  procedure TGIS_RendererFmx.doChart(
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

  procedure TGIS_RendererFmx.RenderChart(
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

  procedure TGIS_RendererFmx.RenderShapeFlashed(
    const _shp : TObject
  ) ;
  begin
    flashed := true ;
    try
      RenderShape( _shp, True ) ;
    finally
      flashed := false ;
    end;
  end;

  function TGIS_RendererFmx.RenderBitmapBegin : TObject ;
  begin
    Result := nil ;
  end;

  procedure TGIS_RendererFmx.RenderBitmapEnd(
    const _handle   : TObject
  ) ;
  begin
    verifyCanvas( nil, True ) ;
  end;

  procedure TGIS_RendererFmx.RenderBitmap(
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
    bmp := TGIS_Bitmap.Create( _size.X, _size.Y, True ) ;
    try
      bmp.LockPixels( buf, True, _format, _order ) ;
      try
        Move( _bmp[0], buf[0],  _size.X * _size.Y * 4 )
      finally
        bmp.UnlockPixels ;
      end;
      RenderBitmap( _handle, bmp, _dst, False ) ;
    finally
      bmp.Free ;
    end;
  end ;

  procedure TGIS_RendererFmx.RenderBitmap(
    const _handle    : TObject           ;
    const _bmp       : TGIS_Bitmap       ;
    const _dst       : TRect             ;
    const _antialias : Boolean
  ) ;
  var
    cnv : TGIS_CanvasInternal ;
  begin
    if assigned( oTransparentCanvas ) then
      cnv := oTransparentCanvas
    else
      cnv := FCanvas ;

    verifyCanvas( cnv.Canvas ) ;

    cnv.Canvas.DrawBitmap(
      TBitmap( _bmp.NativeBitmap ),
      RectF( 0, 0, _bmp.Width, _bmp.Height ),
      RectF( _dst.Left, _dst.Top, _dst.Right, _dst.Bottom),
      1
    ) ;
  end ;

  procedure TGIS_RendererFmx.drawEditingLines(
    const _shp : TGIS_Shape
  ) ;
  var
    shp      : TGIS_Shape ;
    part_no  : Integer ;
    mypath   : TPathData ;

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
      repeat
        isize := 0  ;

        mypath.Clear ;

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
                mypath.MoveTo( PointF( first_pt_x, first_pt_y ) );
                inc( isize ) ;
              end ;

              mypath.LineTo( PointF( pt_x, pt_y ) );
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
                mypath.MoveTo( PointF( first_pt_x, first_pt_y ) );
                inc( isize ) ;
              end ;
            end ;

            if isize > 0  then begin
              mypath.LineTo( PointF( pt_x, pt_y ) );
              inc( isize ) ;
            end;

          end ;
        end ;

        cur_tlrnc := cur_tlrnc + 1 ;
      until isize < GDI_MAXPOINT_COUNT ;

      Result := isize ;
    end ;

    procedure real_draw ;
    begin
      prepareBrush(
        oEditCanvas,
        nil,
        Viewer.Editor.EditingLinesStyle.BrushColor,
        nil,
        Viewer.Editor.EditingLinesStyle.BrushStyle
      ) ;
      if Viewer.Editor.EditingLinesStyle.PenWidth > 1 then begin
        preparePen(
          oEditCanvas,
          nil,
          Viewer.Editor.EditingLinesStyle.BrushColor,
          TGIS_PenStyle.Solid,
          nil,
          TGIS_BrushStyle.Solid,
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          Viewer.Editor.EditingLinesStyle.PenWidth
        ) ;
        if mypath.Count = 2 then
          oEditCanvas.Canvas.DrawLine( mypath[0].Point, mypath[1].Point, 1 )
        else
          oEditCanvas.Canvas.DrawPath( mypath, 1 ) ;
        preparePen(
          oEditCanvas,
          nil,
          Viewer.Editor.EditingLinesStyle.PenColor,
          Viewer.Editor.EditingLinesStyle.PenStyle,
          nil,
          TGIS_BrushStyle.Solid,
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          Viewer.Editor.EditingLinesStyle.PenWidth
        ) ;
        if mypath.Count = 2 then
          oEditCanvas.Canvas.DrawLine( mypath[0].Point, mypath[1].Point, 1 )
        else
          oEditCanvas.Canvas.DrawPath( mypath, 1 ) ;
      end else begin
        preparePen(
          oEditCanvas,
          nil,
          Viewer.Editor.EditingLinesStyle.PenColor,
          Viewer.Editor.EditingLinesStyle.PenStyle,
          nil,
          TGIS_BrushStyle.Solid,
          TGIS_LineCap.Round,
          TGIS_LineJoin.Round,
          Viewer.Editor.EditingLinesStyle.PenWidth
        ) ;
        if mypath.Count = 2 then
          oEditCanvas.Canvas.DrawLine( mypath[0].Point, mypath[1].Point, 1 )
        else
          oEditCanvas.Canvas.DrawPath( mypath, 1 ) ;
        end ;
    end ;

  begin
    shp := _shp ;
    if shp = nil then exit ;

    verifyCanvas( oEditCanvas.Canvas );

    for part_no := 0 to shp.GetNumParts - 1 do begin
      // draw_part
      mypath := TPathData.Create ;
      try
        if prepare_drawbufpart( iTolerance, part_no ) < 1 then
          continue ;
        real_draw ;
      finally
        FreeObject( mypath ) ;
      end ;
    end ;
  end ;

  procedure TGIS_RendererFmx.drawEditingPointMarkers(
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
      prepareFont(
        oEditCanvas,
        Viewer.Editor.EditingPointsStyle.PointsFont.Name,
        Viewer.Editor.EditingPointsStyle.PointsFont.Size,
        Viewer.Editor.EditingPointsStyle.PointsFont.Style
      ) ;
      oEditCanvas.Font.Color :=
        Viewer.Editor.EditingPointsStyle.PointsFont.Color ;
    end ;

    procedure prepare_active ;
    begin
      prepareBrush(
        oEditCanvas,
        nil,
        Viewer.Editor.EditingPointsStyle.ActivePoints.BrushColor,
        nil,
        Viewer.Editor.EditingPointsStyle.ActivePoints.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        nil,
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
        nil,
        Viewer.Editor.EditingPointsStyle.InactivePoints.BrushColor,
        nil,
        Viewer.Editor.EditingPointsStyle.InactivePoints.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        nil,
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
        nil,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushColor,
        nil,
        Viewer.Editor.EditingPointsStyle.SelectedPoints.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        nil,
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
      cl : TGIS_Color ;
    begin
      if _param then cl := TGIS_Color.Olive else cl := TGIS_Color.Red ;
      prepareBrush(
        oEditCanvas,
        nil,
        cl,
        nil,
        T_Brush(oEditCanvas.FmxBrush).Style
      ) ;
    end ;

    procedure draw_marker(
      const _x1 : Integer ;
      const _y1 : Integer ;
      const _x2 : Integer ;
      const _y2 : Integer
    ) ;
    begin
      oEditCanvas.Canvas.FillEllipse( RectF( _x1, _y1, _x2, _y2 ), 1 ) ;
      oEditCanvas.Canvas.DrawEllipse( RectF( _x1, _y1, _x2, _y2 ), 1 ) ;
    end ;

    function text_extent( _text : String ) : TPoint ;
    var
      r : TRectF ;
    begin
      r := RectF( 0, 0, 32000, 32000 ) ;

      oEditCanvas.TextLayout.BeginUpdate;
      oEditCanvas.TextLayout.TopLeft := PointF( r.Left , r.Top );
      oEditCanvas.TextLayout.MaxSize := PointF( r.Width, r.Height);
      oEditCanvas.TextLayout.Text := _text ;
      oEditCanvas.TextLayout.WordWrap := False;
      oEditCanvas.TextLayout.Opacity := 1;
      oEditCanvas.TextLayout.HorizontalAlign := TTextAlign.Leading;
      oEditCanvas.TextLayout.VerticalAlign := TTextAlign.Leading;
      oEditCanvas.TextLayout.Font := oEditCanvas.Canvas.Font;
      oEditCanvas.TextLayout.Color :=  oEditCanvas.Canvas.Stroke.Color ;
      oEditCanvas.TextLayout.RightToLeft := False;
      oEditCanvas.TextLayout.EndUpdate;
      r := oEditCanvas.TextLayout.TextRect ;

      Result.X := RoundS( r.Right  ) ;
      Result.Y := RoundS( r.Bottom ) ;
    end ;

    procedure draw_text(
      const _x    : Integer ;
      const _y    : Integer ;
      const _text : String
    ) ;
    var
      sz : TPoint ;
      gap : Integer ;
    begin
      sz := text_extent( _text ) ;
      if Viewer.Editor.EditingPointsStyle.PointsFont.Color <>
         Viewer.Editor.EditingPointsStyle.PointsBackground then begin
        // some space for better visibility
        gap := TwipsToPixels( 15 ) ;
        prepareBrush(
          oEditCanvas,
          nil,
          Viewer.Editor.EditingPointsStyle.PointsBackground,
          nil,
          TGIS_BrushStyle.Solid
        ) ;
        oEditCanvas.Canvas.FillRect( RectF( _x - gap , _y, _x + sz.X + gap, _y + sz.Y ),
                                     0, 0, [], 1 ) ;
      end ;

      oEditCanvas.TextLayout.BeginUpdate;
      oEditCanvas.TextLayout.TopLeft := PointF( _x, _y ) ;
      oEditCanvas.TextLayout.MaxSize := PointF( sz.X, sz.Y ) ;
      oEditCanvas.TextLayout.Text := _text ;
      oEditCanvas.TextLayout.WordWrap := False ;
      oEditCanvas.TextLayout.Opacity := 1 ;
      oEditCanvas.TextLayout.HorizontalAlign := TTextAlign.Leading ;
      oEditCanvas.TextLayout.VerticalAlign := TTextAlign.Leading ;
      oEditCanvas.TextLayout.Font := oEditCanvas.Canvas.Font ;
      oEditCanvas.TextLayout.Color := Cardinal( oEditCanvas.Font.Color ) ;
      oEditCanvas.TextLayout.RightToLeft := False ;
      oEditCanvas.TextLayout.EndUpdate ;
      oEditCanvas.TextLayout.RenderLayout( oEditCanvas.Canvas ) ;
    end ;

  begin
    verifyCanvas( oEditCanvas.Canvas ) ;

    ext := Viewer.VisibleExtent ;
    tps := twipsToPixels( GIS_TRACKING_POINT_SIZE div 2 ) ;
    prepare_font ;

    for part_no := 0 to _shp.GetNumParts - 1 do begin
      if _shp.GetPartSize( part_no ) <= 0 then continue ;

      if _shp is TGIS_ShapePolygon
        then cnt := _shp.GetPartSize( part_no ) - 2
        else cnt := _shp.GetPartSize( part_no ) - 1 ;


      old_pt_x     := -1000 ;
      old_pt_y     := -1000 ;
      old_pt_txt_x := -1000 ;
      old_pt_txt_y := -1000 ;

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
             ) >=  3 * Abs( oEditCanvas.Font.Size )
          then begin
            old_pt_txt_x := pt_x ;
            old_pt_txt_y := pt_y ;

            if Viewer.Editor.ShowPointsNumbers then begin
              draw_text( pt_x + tps, pt_y + tps, IntToStr( point_no ) ) ;
              if Viewer.Editor.ShowPoints3D then begin
                prepare_z( Abs(ptg3D.Z) > eps ) ;
                tw := text_extent( Format('%.2f',[ptg3D.Z] ) ).X ;
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

  procedure TGIS_RendererFmx.drawEditingEdgeLengths(
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
        oEditCanvas.Font.Style
      ) ;

      prepareBrush(
        oEditCanvas,
        nil,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushColor,
        nil,
        Viewer.Editor.EditingEdgeLengthsStyle.Background.BrushStyle
      ) ;
      preparePen(
        oEditCanvas,
        nil,
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
        oEditCanvas.Canvas.FillRect( RectF( _x - gap , _y, _x + pt.X + gap, _y + pt.Y ),
                                     0, 0, [], 1 ) ;
      end ;

      oEditCanvas.TextLayout.BeginUpdate;
      oEditCanvas.TextLayout.TopLeft := PointF( _x, _y ) ;
      oEditCanvas.TextLayout.MaxSize := PointF( pt.X, pt.Y ) ;
      oEditCanvas.TextLayout.Text := _text ;
      oEditCanvas.TextLayout.WordWrap := False ;
      oEditCanvas.TextLayout.Opacity := 1 ;
      oEditCanvas.TextLayout.HorizontalAlign := TTextAlign.Leading ;
      oEditCanvas.TextLayout.VerticalAlign := TTextAlign.Leading ;
      oEditCanvas.TextLayout.Font := oEditCanvas.Canvas.Font ;
      oEditCanvas.TextLayout.Color := Cardinal( oEditCanvas.Font.Color ) ;
      oEditCanvas.TextLayout.RightToLeft := False ;
      oEditCanvas.TextLayout.EndUpdate ;
      oEditCanvas.TextLayout.RenderLayout( oEditCanvas.Canvas ) ;
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
    verifyCanvas( oEditCanvas.Canvas ) ;

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

  procedure TGIS_RendererFmx.drawEditingPoints(
    const _shp     : TGIS_Shape
  ) ;
  begin
    if _shp.IsEmpty then exit ;
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
    end;
  end ;

 function TGIS_RendererFmx.PrepareBitmapCache(
    const _bmp      : TGIS_Pixels ;
    const _extent   : TGIS_Extent ;
    const _size     : TPoint      ;
    const _serial   : Integer     ;
    const _format   : TGIS_BitmapFormat ;
    const _order    : TGIS_BitmapLinesOrder
  ) : TGIS_RendererAbstractCache ;
  var
    bmp : TGIS_Bitmap ;
    buf : TGIS_Pixels ;
  begin
    Result := TGIS_RendererFmxCache.Create( _extent, _size, _serial );

    bmp := TGIS_Bitmap.Create( _size.X, _size.Y, True ) ;
    bmp.LockPixels( buf, True, _format, _order ) ;
    try
      Move( _bmp[0], buf[0],  _size.X * _size.Y * 4 )
    finally
      bmp.UnlockPixels ;
    end;

    TGIS_RendererFmxCache( Result ).oBitmap := bmp ;
  end;

  procedure TGIS_RendererFmx.RenderBitmapCache(
    const _handle   : TObject ;
    const _cache    : TGIS_RendererAbstractCache ;
    const _dst      : TRect
  ) ;
  var
    cnv : TGIS_CanvasInternal ;
  begin
    if assigned( oTransparentCanvas ) then
      cnv := oTransparentCanvas
    else
      cnv := FCanvas ;

    verifyCanvas( cnv.Canvas ) ;

    cnv.Canvas.DrawBitmap(
      TBitmap( TGIS_RendererFmxCache( _cache ).oBitmap.NativeBitmap ),
      RectF( 0, 0, _dst.Width, _dst.Height ),
      RectF( _dst.Left, _dst.Top, _dst.Right, _dst.Bottom),
      1
    ) ;
  end ;

  procedure TGIS_RendererFmx.RenderEditor(
    const _context : TObject
  ) ;
  var
    ocnv : TGIS_CanvasInternal ;
    fcnv : TGIS_CanvasInternal ;
    m1 : TMatrix ;
    m2 : TMatrix ;
    sc : Single  ;
  begin
    if not Assigned( Viewer) then exit;

    if Viewer.Editor.InEdit and Assigned( Viewer.Editor.CurrentShape ) then begin

      Assert( _context is TCanvas ) ;
      TCanvas(_context).Flush ;

      ocnv := oCanvas ;
      fcnv := FCanvas ;
      m1 := TCanvas(_context).Matrix ;
      sc := TCanvas(_context).Scale  ;

      oEditCanvas := TGIS_CanvasInternal.Create( TCanvas(_context) ) ;
      m2 := TMatrix.CreateScaling( 1/sc, 1/sc ) *
            TMatrix.CreateTranslation( m1.m31, m1.m32 ) ;
      oEditCanvas.Canvas.SetMatrix(m2);
      oCanvas := oEditCanvas ;
      FCanvas := oEditCanvas ;
      try
        oEditCanvas.Canvas.BeginScene();
        // ensure proper renderer context
        if assigned( TGIS_Shape( Viewer.Editor.CurrentShape ).Layer ) then begin
          TGIS_Shape( Viewer.Editor.CurrentShape ).Layer.Renderer := self ;
          drawEditingPoints( TGIS_Shape( Viewer.Editor.CurrentShape ) ) ;
        end ;
      finally
        oEditCanvas.Canvas.EndScene();
        FreeObject( oEditCanvas ) ;

        TCanvas(_context).SetMatrix( m1 ) ;
        oCanvas := ocnv ;
        FCanvas := fcnv ;
      end;
    end ;
  end;

  procedure TGIS_RendererFmx.PaintExtra(
    const _sender  : TObject ;
    const _context : TObject ;
    const _event   : TGIS_RendererEvent
  ) ;
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
      sc : Single ;
    begin
      if ( _context is TCanvas ) then begin
        parent := TComponent(_sender) as IGIS_ViewerParent ;
        save_context ;

        sc := parent.ControlCanvasScale ;
        if TCanvas( _context ).Scale =1  then
          sc := 1 ;


        inherited CreateContext( parent, nil, nil, Point(0,0),
                                 RoundS( parent.ControlCanvasWidth / sc ),
                                 RoundS( parent.ControlCanvasHeight / sc ),
                                 RoundS( parent.ControlPPI / sc ),
                                 100 ) ;

        FCanvas := TGIS_CanvasInternal.Create( TCanvas(_context) ) ;
      end
      else
      if ( _context is TBitmap ) then begin
        FCanvas := TGIS_CanvasInternal.Create( TBitmap(_context).Canvas ) ;
      end;
    end;

    procedure release_context ;
    begin
      if ( _context is TCanvas ) then begin
        FreeObject( FCanvas ) ;
        restore_context ;
      end else if ( _context is TBitmap ) then begin
        FreeObject( FCanvas ) ;
      end;
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

  procedure TGIS_RendererFmx.Update ;
  begin
  end ;

  procedure TGIS_RendererFmx.Flush ;
  begin
    FCanvas.Canvas.Flush ;
  end ;

  function TGIS_RendererFmx.FriendlyName
    : String ;
  begin
    Result := 'Fmx' ;
  end ;

  function TGIS_RendererFmx.CanvasNative
    : TObject ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    Result := FCanvas.Canvas ;
  end ;

  procedure TGIS_RendererFmx.CanvasFontMetrics(
    var   _break_char : Char    ;
    var   _height     : Integer ;
    var   _ascent     : Integer ;
    var   _true_type  : Boolean
  ) ;
  begin
    verifyCanvas( FCanvas.Canvas ) ;
  end;

  function TGIS_RendererFmx.CanvasTextBaseline(
    const _text : String
  ) : Single ;
  begin
    Result := 0 ;
  end ;

  function TGIS_RendererFmx.CanvasTextExtent(
    const _text  : String
  ) : TPoint ;
  var
    r : TRectF;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;
    if FCanvas.Font.Size = 0 then begin
      Result := Point( 0, 0 ) ;
      exit ;
    end ;

    prepareFont(
      FCanvas,
      FCanvas.Font.Name,
      FCanvas.Font.Size,
      FCanvas.Font.Style
    ) ;

    r := RectF( 0, 0, 32000, 32000 ) ;

    FCanvas.TextLayout.BeginUpdate ;
    FCanvas.TextLayout.TopLeft := PointF( r.Left , r.Top );
    FCanvas.TextLayout.MaxSize := PointF( r.Width, r.Height);
    FCanvas.TextLayout.Text := _text ;
    FCanvas.TextLayout.WordWrap := False;
    FCanvas.TextLayout.Opacity := 1;
    FCanvas.TextLayout.HorizontalAlign := TTextAlign.Leading;
    FCanvas.TextLayout.VerticalAlign := TTextAlign.Leading;
    FCanvas.TextLayout.Font := FCanvas.Canvas.Font;
    FCanvas.TextLayout.Color :=  FCanvas.Canvas.Stroke.Color ;
    FCanvas.TextLayout.RightToLeft := False;
    FCanvas.TextLayout.EndUpdate;
    r := FCanvas.TextLayout.TextRect ;

    Result.X := RoundS( r.Right  ) ;
    Result.Y := RoundS( r.Bottom ) ;
  end;

  procedure TGIS_RendererFmx.CanvasDrawText(
    const _rect  : TRect   ;
    const _text  : String
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;
    if FCanvas.Font.Size = 0 then exit ;

    prepareFont(
      FCanvas,
      FCanvas.Font.Name,
      FCanvas.Font.Size,
      FCanvas.Font.Style
    ) ;
    prepareBrush(
      FCanvas,
      nil,
      FCanvas.Font.Color,
      nil,
      TGIS_BrushStyle.Solid
    ) ;

    FCanvas.TextLayout.BeginUpdate;

    FCanvas.TextLayout.TopLeft := PointF( _rect.Left , _rect.Top    ) ;
    FCanvas.TextLayout.MaxSize := PointF( _rect.Width, _rect.Height ) ;
    FCanvas.TextLayout.Text := _text ;
    FCanvas.TextLayout.WordWrap := False ;
    FCanvas.TextLayout.Opacity := 1 ;
    FCanvas.TextLayout.HorizontalAlign := TTextAlign.Leading ;
    FCanvas.TextLayout.VerticalAlign := TTextAlign.Leading ;
    FCanvas.TextLayout.Font := FCanvas.Canvas.Font ;
    FCanvas.TextLayout.Color := FCanvas.Canvas.Stroke.Color ;
    FCanvas.TextLayout.RightToLeft := False ;
    FCanvas.TextLayout.EndUpdate ;
    FCanvas.TextLayout.RenderLayout( FCanvas.Canvas ) ;
  end;

  procedure TGIS_RendererFmx.CanvasDrawLine(
    const _x1    : Integer ;
    const _y1    : Integer ;
    const _x2    : Integer ;
    const _y2    : Integer
  ) ;
  var
    afix : Double ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.LineDash,
      FCanvas.Pen.Width
    ) ;

    // avoid aliasing on 2, 4, 6 .. widths
    if FCanvas.Pen.Width mod 2 = 0 then
      afix := 0
    else
      afix := 0.5 ;

    FCanvas.Canvas.DrawLine(
      PointF( _x1 + afix, _y1 + afix ),
      PointF( _x2 + afix, _y2 + afix ),
      1
    ) ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := Length( _points ) ;
    CanvasDrawPolyLine( _points, parts );
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := Length( _points ) ;
    CanvasDrawPolyLine( _points, parts );
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolyLine(
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

  procedure TGIS_RendererFmx.CanvasDrawPolyLine(
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

  procedure TGIS_RendererFmx.CanvasDrawPolyLine(
    const _points : TGIS_DrawBuf ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd   : TPathData ;
    i    : Integer ;
    j    : Integer ;
    k    : Integer ;
    dfix : Double ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;

    // avoid aliasing on 2, 4, 6 .. widths
    if FCanvas.Pen.Width mod 2 = 0 then
      dfix := 0
    else
      dfix := 0.5 ;

    pd := TPathData.Create ;
    try
      j := 0 ;
      for i := 0 to Length( _parts ) - 1 do begin
        pd.MoveTo( PointF( _points[j].X + dfix,  _points[j].Y + dfix ) ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo(  PointF( _points[j+k].X + dfix,  _points[j+k].Y + dfix ) ) ;
          Inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;

      FCanvas.Canvas.DrawPath( pd, 1.0 ) ;
    finally
      FreeObject( pd ) ;
    end ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolyLine(
    const _points : TGIS_DrawBufF ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd : TPathData ;
    i  : Integer ;
    j  : Integer ;
    k  : Integer ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      FCanvas.Pen.LineCap,
      FCanvas.Pen.LineJoin,
      FCanvas.Pen.Width
    ) ;

    pd := TPathData.Create ;
    try
      j := 0 ;
      for i := 0 to Length( _parts ) - 1 do begin
        pd.MoveTo( PointF( _points[j].X, _points[j].Y ) ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo(  PointF( _points[j+k].X, _points[j+k].Y ) ) ;
          Inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;

      FCanvas.Canvas.DrawPath( pd, 1.0 ) ;
    finally
      FreeObject( pd ) ;
    end ;
  end ;


  procedure TGIS_RendererFmx.CanvasDrawRectangle(
    const _rect   : TRect
  ) ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Square,
      TGIS_LineJoin.Bevel,
      FCanvas.Pen.Width
    ) ;
    CanvasDrawPolygon( [ Point( _rect.Left , _rect.Top    ),
                         Point( _rect.Right, _rect.Top    ),
                         Point( _rect.Right, _rect.Bottom ),
                         Point( _rect.Left , _rect.Bottom ),
                         Point( _rect.Left , _rect.Top    )
                       ]
                     ) ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawEllipse(
    const _x      : Integer ;
    const _y      : Integer ;
    const _width  : Integer ;
    const _height : Integer
  ) ;
  var
    rct : TRectF ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    rct.Left   := _x ;
    rct.Top    := _y ;
    rct.Right  := _x + _width  ;
    rct.Bottom := _y + _height ;

    prepareBrush(
      FCanvas,
      nil,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;
    FCanvas.Canvas.FillEllipse( rct, 1.0 ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      Abs(FCanvas.Pen.Width)
    ) ;
    FCanvas.Canvas.DrawEllipse( rct, 1.0 ) ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPie(
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
      steps := RoundS( Abs( arcAngle ) / (2*Pi) * _segments ) ;
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
    verifyCanvas( FCanvas.Canvas ) ;

    try
      stroke_arc( _origin_x, _origin_y,
                  _radius div 2, _radius div 2,
                  _angle_0, _angle_1,
                  Pi/2, 30
                ) ;

      try
        old_cap  := CanvasPen.LineCap ;
        old_join := CanvasPen.LineJoin   ;
        CanvasPen.LineCap  := TGIS_LineCap.Round ;
        CanvasPen.LineJoin := TGIS_LineJoin.Round ;
        CanvasDrawPolygon( points ) ;
      finally
        CanvasPen.LineCap  := old_cap ;
        CanvasPen.LineJoin := old_join   ;
      end ;
    finally
      if Assigned( points ) then
        points := nil ;
    end ;

  end ;

  procedure TGIS_RendererFmx.CanvasDrawArc(
    const _x          : Integer ;
    const _y          : Integer ;
    const _radius     : Integer ;
    const _startAngle : Single ;
    const _sweepAngle : Single
  );
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      FCanvas.Pen.Width
    ) ;
    FCanvas.Canvas.DrawArc( PointF(_x,_y), PointF(_radius, _radius), _startAngle, -_sweepAngle, 1.0 ) ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawArc(
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
    verifyCanvas( FCanvas.Canvas ) ;

    preparePen(
      FCanvas,
      nil,
      FCanvas.Pen.Color,
      FCanvas.Pen.Style,
      nil,
      TGIS_BrushStyle.Solid,
      TGIS_LineCap.Round,
      TGIS_LineJoin.Round,
      FCanvas.Pen.Width
    ) ;

    center      := PointF(_x +_width/2, _y + _height/2);
    radius      := PointF(_width/2, _height/2);
    a1          := ArcTan2( _startY - Center.Y, _startX - Center.X );
    a2          := ArcTan2( _endY   - Center.Y, _endX   - Center.X );
    dangle      := (a2-a1);
    start_angle := RadToDeg( a1 ) ;
    sweep_angle := RadToDeg( dangle ) ;

    FCanvas.Canvas.DrawArc( center, radius, start_angle, sweep_angle, 1.0 ) ;
  end;

  procedure TGIS_RendererFmx.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := Length( _points ) ;
    CanvasDrawPolygon( _points, parts );
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF
  ) ;
  var
    parts : TGIS_IntegerArray ;
  begin
    SetLength( parts, 1 ) ;
    parts[0] := Length( _points ) ;
    CanvasDrawPolygon( _points, parts );
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolygon(
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

  procedure TGIS_RendererFmx.CanvasDrawPolygon(
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

  procedure TGIS_RendererFmx.CanvasDrawPolygon(
    const _points : TGIS_DrawBuf  ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd   : TPathData ;
    i    : Integer ;
    j    : Integer ;
    k    : Integer ;
    dfix : Double ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    prepareBrush(
      FCanvas,
      nil,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;

    // avoid aliasing on 2, 4, 6 .. widths
    if FCanvas.Pen.Width mod 2 = 0 then
      dfix := 0
    else
      dfix := 0.5 ;

    pd := TPathData.Create ;
    try
      j := 0 ;
      for i := 0 to Length( _parts ) - 1 do begin
        pd.MoveTo( PointF( _points[j].X + dfix,  _points[j].Y + dfix ) ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo(  PointF( _points[j+k].X + dfix,  _points[j+k].Y + dfix ) ) ;
          Inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;
      pd.ClosePath ;

      FCanvas.Canvas.FillPath( pd, 1.0 ) ;

      if FCanvas.Pen.Width > 0 then begin
        preparePen(
          FCanvas,
          nil,
          FCanvas.Pen.Color,
          FCanvas.Pen.Style,
          nil,
          TGIS_BrushStyle.Solid,
          TGIS_LineCap.Round,
          FCanvas.Pen.LineJoin,
          FCanvas.Pen.Width
        ) ;
        FCanvas.Canvas.DrawPath( pd, 1.0 ) ;
      end ;
    finally
      FreeObject( pd ) ;
    end ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawPolygon(
    const _points : TGIS_DrawBufF  ;
    const _parts  : TGIS_IntegerArray
  ) ;
  var
    pd : TPathData ;
    i  : Integer ;
    j  : Integer ;
    k  : Integer ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    prepareBrush(
      FCanvas,
      nil,
      FCanvas.Brush.Color,
      nil,
      FCanvas.Brush.Style
    ) ;

    pd := TPathData.Create ;
    try
      j := 0 ;
      for i := 0 to Length( _parts ) - 1 do begin
        pd.MoveTo( PointF( _points[j].X,  _points[j].Y ) ) ;
        k := 1 ;
        while j + k < j + _parts[i] do begin
          pd.LineTo(  PointF( _points[j+k].X,  _points[j+k].Y ) ) ;
          Inc( k ) ;
        end ;
        j := j + _parts[i] ;
      end ;
      pd.ClosePath ;

      FCanvas.Canvas.FillPath( pd, 1.0 ) ;

      if FCanvas.Pen.Width > 0 then begin
        preparePen(
          FCanvas,
          nil,
          FCanvas.Pen.Color,
          FCanvas.Pen.Style,
          nil,
          TGIS_BrushStyle.Solid,
          TGIS_LineCap.Round,
          FCanvas.Pen.LineJoin,
          FCanvas.Pen.Width
        ) ;
        FCanvas.Canvas.DrawPath( pd, 1.0 ) ;
      end ;
    finally
      FreeObject( pd ) ;
    end ;
  end ;

  procedure TGIS_RendererFmx.CanvasDrawBitmap(
    const _bmp      : TGIS_Pixels ;
    const _size     : TPoint  ;
    const _dst      : TRect   ;
    const _format   : TGIS_BitmapFormat ;
    const _order    : TGIS_BitmapLinesOrder
  ) ;
  begin
    RenderBitmap( nil, _bmp, _size, _dst, _format, _order );
  end;

  procedure TGIS_RendererFmx.CanvasDrawBitmap(
    const _bmp       : TGIS_Bitmap       ;
    const _dst       : TRect
  ) ;
  begin
    RenderBitmap( nil, _bmp, _dst, True );
  end;

  procedure TGIS_RendererFmx.CanvasSetTransformation(
    const _angle    : Double  ;
    const _origin_x : Integer ;
    const _origin_y : Integer
  ) ;
  var
    transform : TMatrix ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    storedTransform := FCanvas.Canvas.Matrix ;


    {$IFDEF LEVEL_XE5_FMX}
      transform := TMatrix.Identity;
    {$ELSE}
      transform := IdentityMatrix;
    {$ENDIF}
    transform.m31 := _origin_x ;
    transform.m32 := _origin_y ;
    {$IFDEF LEVEL_XE5_FMX}
      transform := TMatrix.CreateRotation(_angle) * transform ;
    {$ELSE}
      transform := MatrixMultiply( CreateRotationMatrix(_angle), transform );
    {$ENDIF}

    FCanvas.Canvas.MultiplyMatrix( transform ) ;
  end;

  procedure TGIS_RendererFmx.CanvasClearTransformation ;
  begin
    assert( assigned( FCanvas ) ) ;
    verifyCanvas( FCanvas.Canvas ) ;

    {$IFDEF LEVEL_XE5_FMX}
      FCanvas.Canvas.SetMatrix( storedTransform );
    {$ELSE}
      FCanvas.Canvas.SetMatrix( storedTransform );
    {$ENDIF}
  end;

  procedure TGIS_RendererFmxCache.doDestroy;
  begin
    FreeObject( oBitmap ) ;
    inherited ;
  end;
initialization
  RegisterRenderer( 'TGIS_RendererFmx', TGIS_RendererFmx ) ;

{==================================== END =====================================}
end.

