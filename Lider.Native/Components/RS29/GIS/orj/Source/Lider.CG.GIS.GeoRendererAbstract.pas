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
  Basic renderer class.
}

{$IFDEF DCC}
  unit GisRendererAbstract ;
  {$HPPEMIT '#pragma link "GisRendererAbstract"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisRtl,
    GisInterfaces,
    GisTypes,
    GisTypesUI;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type
  /// <summary>
  ///   Object to store all canvases on rendering context
  /// </summary>
  TGIS_RendererContext = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_BaseObjectDisposable )
    private
      oProgressiveHelper : TGIS_ViewerHelperRun ;

      // Canvas used to draw base map (common content).
      oBaseMap      : TObject ;
      bBaseMapOwn   : Boolean ;

      // Canvas used to draw selected objects.
      oSelection    : TObject ;
      bSelectionOwn : Boolean ;

      // Canvas used to draw charts.
      oCharts       : TObject ;
      bChartsOwn    : Boolean ;

      // Canvas used to draw labels.
      oLabels       : TObject ;
      bLabelsOwn    : Boolean ;

      // Native draw context.
      oNativeDrawContext  : TObject ;
      // Draw context factory.
      oDrawContextFactory : TObject ;
      // Source draw context.
      oSourceDrawContext  : TObject ;

      function  get_ProgressiveHelper : TObject ;
      procedure set_ProgressiveHelper( const _value : TObject ) ;

      function  fget_BaseMapOnDemand   : Boolean ;
      function  fget_SelectionOnDemand : Boolean ;
      function  fget_ChartsOnDemand    : Boolean ;
      function  fget_LabelsOnDemand    : Boolean ;

    protected

      /// <inheritdoc/>
      procedure  doDestroy ; override;

    public
      /// <summary>
      ///   Standard constructor
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Clear context status. Free all owned bitmaps.
      /// </summary>
      procedure   Clear ;

    public
      /// <summary>
      ///   Assign BaseMap bitmap. Free owned one if necessary.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to be assigned
      /// </param>
      /// <param name="_own">
      ///   True if context own bitmap (is responsible to free it)
      /// </param>
      procedure AssignBaseMap  ( const _bitmap : TObject ;
                                 const _own    : Boolean
                               ) ;

      /// <summary>
      ///   Assign Selection bitmap. Free existing one if necessary.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to be assigned
      /// </param>
      /// <param name="_own">
      ///   True if context own bitmap (is responsible to free it)
      /// </param>
      procedure AssignSelection( const _bitmap : TObject ;
                                 const _own    : Boolean
                               ) ;

      /// <summary>
      ///   Assign Charts bitmap. Free owned one if necessary.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to be assigned
      /// </param>
      /// <param name="_own">
      ///   True if context own bitmap (is responsible to free it)
      /// </param>
      procedure AssignCharts   ( const _bitmap : TObject ;
                                 const _own    : Boolean
                               ) ;

      /// <summary>
      ///   Assign Labels bitmap. Free owned one if necessary.
      /// </summary>
      /// <param name="_bitmap">
      ///   bitmap to be assigned
      /// </param>
      /// <param name="_own">
      ///   True if context own bitmap (is responsible to free it)
      /// </param>
      procedure AssignLabels   ( const _bitmap : TObject ;
                                 const _own    : Boolean
                               ) ;

      /// <summary>
      ///   If ProgressiveHelper is assigned then perform
      ///   TGIS_ViewerWndHelperRun.DoSynchronize( False ) in this context.
      /// </summary>
      procedure DoProgressiveUpdate ;

      /// <summary>
      ///   Assign draw context objects.
      /// </summary>
      /// <param name="_factory">
      ///   draw context factory
      /// </param>
      /// <param name="_nativeContext">
      ///   draw context to draw directly to
      /// </param>
      /// <param name="_sourceContext">
      ///   source draw context factory
      /// </param>
      procedure AssignDrawContext( const _factory       : TObject ;
                                   const _nativeContext : TObject ;
                                   const _sourceContext : TObject
                                 ) ;

      /// <summary>
      ///   Clear draw context.
      /// </summary>
      procedure ClearDrawContext ;

    public

      /// <summary>
      ///   Canvas used to draw base map (common content).
      /// </summary>
      property BaseMap   : TObject read oBaseMap   ;

      /// <summary>
      ///   Canvas used to draw selected objects.
      /// </summary>
      property Selection : TObject read oSelection ;

      /// <summary>
      ///   Canvas used to draw charts.
      /// </summary>
      property Charts    : TObject read oCharts    ;

      /// <summary>
      ///   Canvas used to draw labels.
      /// </summary>
      property Labels    : TObject read oLabels    ;

      /// <summary>
      ///   Helper for progressive, thread safely. updates if any.
      /// </summary>
      /// <value>
      ///   Extended is a value of active TGIS_ViewerWndHelperRun object.
      /// </value>
      /// <remarks>
      ///  Do not set it on non progressive context like rendering on bitmap.
      /// </remarks>
      property ProgressiveHelper : TObject read  get_ProgressiveHelper
                                           write set_ProgressiveHelper;


      /// <summary>
      ///   True if underlying bitmap should be created on-demand.
      /// </summary>
      property BaseMapOnDemand   : Boolean read fget_BaseMapOnDemand   ;

      /// <summary>
      ///   True if underlying bitmap should be created on-demand.
      /// </summary>
      property SelectionOnDemand : Boolean read fget_SelectionOnDemand ;

      /// <summary>
      ///   True if underlying bitmap should be created on-demand.
      /// </summary>
      property ChartsOnDemand    : Boolean read fget_ChartsOnDemand    ;

      /// <summary>
      ///   True if underlying bitmap should be created on-demand.
      /// </summary>
      property LabelsOnDemand    : Boolean read fget_LabelsOnDemand    ;

      /// <summary>
      ///   Native draw context.
      /// </summary>
      property NativeDrawContext  : TObject read  oNativeDrawContext ;

      /// <summary>
      ///   Draw context factory.
      /// </summary>
      property DrawContextFactory : TObject read  oDrawContextFactory ;

      /// <summary>
      ///   Source draw context.
      /// </summary>
      property SourceDrawContext  : TObject read  oSourceDrawContext ;
  end ;

  TGIS_RendererAbstract = class ;
  TGIS_RendererAbstractCache = class ;

  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      /// <summary>
      ///   Renderer event
      /// </summary>
      RenderEvent = public class ( java.util.EventObject )
        private
          FRenderer : TGIS_RendererAbstract ;
          FDrawMode : TGIS_DrawMode ;
        public
          /// <summary>
          ///   Create an object.
          /// </summary>
          /// <param name="_source">
          ///   who rises the event
          /// </param>
          /// <param name="_renderer">
          ///   renderer to be passed
          /// </param>
          /// <param name="_mode">
          ///   draw mode to be passed
          /// </param>
          constructor Create ( const _source    : Object ;
                               const _renderer  : TGIS_RendererAbstract ;
                               const _mode      : TGIS_DrawMode
                             ) ;
        public
          /// <summary>
          ///   Currently operating renderer. By assigning a new a different
          ///   rendering engine a rendering will be switched to a new engine .
          /// </summary>
          property Renderer : TGIS_RendererAbstract read FRenderer ;
          /// <summary>
          ///   Shows what scope of the layers must be drawn.
          /// </summary>
          property DrawMode : TGIS_DrawMode         read FDrawMode ;
      end ;

      /// <summary>
      ///   Render listener.
      /// </summary>
      RenderListener = public interface ( java.util.EventListener )
        /// <summary>
        ///   BeforeUpdate event. Will be fired before Update
        ///   operation.
        /// </summary>
        /// <param name="_evt">
        ///   render event
        /// </param>
        method BeforeUpdate( _evt : RenderEvent ) ;
        /// <summary>
        ///   Update event. Will be fired on Update
        ///   operation.
        /// </summary>
        /// <param name="_evt">
        ///   render event
        /// </param>
        /// <returns>
        ///   true if anything was drawn
        /// </returns>
        method Update      ( _evt : RenderEvent ) : Boolean ;
        /// <summary>
        ///   AfterUpdate event. Will be fired after Update
        ///   operation.
        /// </summary>
        /// <param name="_evt">
        ///   render event
        /// </param>
        method AfterUpdate ( _evt : RenderEvent ) ;
      end ;

      /// <summary>
      ///   Render adapter.
      /// </summary>
      RenderAdapter = public class( RenderListener )
        public
          /// <inheritdoc/>
          method BeforeUpdate ( _evt : RenderEvent ) ;
          /// <inheritdoc/>
          method Update       ( _evt : RenderEvent ) : Boolean ;
          /// <inheritdoc/>
          method AfterUpdate  ( _evt : RenderEvent ) ;
      end;
      /// <summary>
      ///   Handler for events using TGIS_Renderer object.
      /// </summary>
      /// <param name="_evt">
      ///   event renderer
      /// </param>
      TGIS_RendererEvent = public procedure(
        _evt : RenderEvent
      ) of object ;
    {$ENDIF}
    {$IFDEF CLR}

      /// <summary>
      ///   Provides data for TGIS_RendererEvent.
      /// </summary>
      TGIS_RendererEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                               class ( System.EventArgs )
        private
          FRenderer : TGIS_RendererAbstract ;
          FDrawMode : TGIS_DrawMode ;

        public

          /// <summary>
          ///   Create an object.
          /// </summary>
          /// <param name="_renderer">
          ///   renderer to be passed
          /// </param>
          /// <param name="_mode">
          ///   mode to be passed
          /// </param>
          constructor Create ( const _renderer : TGIS_RendererAbstract ;
                               const _mode     : TGIS_DrawMode
                             ) ;

        public
          /// <summary>
          ///   Currently operating renderer. By assigning a new a different
          ///   rendering engine a rendering will be switched to a new engine .
          /// </summary>
          property Renderer : TGIS_RendererAbstract read FRenderer ;
          /// <summary>
          ///   Shows what scope of the layers must be drawn.
          /// </summary>
          property DrawMode : TGIS_DrawMode         read FDrawMode ;
      end ;

      /// <summary>
      ///   Handler for events using TGIS_Renderer object.
      /// </summary>
      /// <param name="_sender">
      ///   event originator
      /// </param>
      /// <param name="_e">
      ///   event parameters
      /// </param>
      TGIS_RendererEvent = public procedure(
        _sender : System.Object ;
        _e      : TGIS_RendererEventArgs
      ) of object ;
    {$ENDIF}
    {$IFDEF ISLAND}
    /// <summary>
    ///   Handler for events using TGIS_Renderer object.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_renderer">
    ///   active renderer
    /// </param>
    /// <param name="_mode">
    ///   drawing mode
    /// </param>
    TGIS_RendererEvent = public procedure(
      _sender   : TObject ;
      _renderer : TGIS_RendererAbstract ;
      _mode     : TGIS_DrawMode
    ) of object ;
    {$ENDIF}
  {$ELSE}

    /// <summary>
    ///   Handler for events using TGIS_Renderer object.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_renderer">
    ///   active renderer
    /// </param>
    /// <param name="_mode">
    ///   drawing mode
    /// </param>
    TGIS_RendererEvent = procedure(
      {$IFDEF GENXDK}
        var _translated : Boolean ;
      {$ENDIF}
      _sender   : TObject ;
      _renderer : TGIS_RendererAbstract ;
      _mode     : TGIS_DrawMode
    ) of object ;

  {$ENDIF}

  /// <summary>
  ///   Basic renderer class to be used as a parent for customer map renderers
  ///   for VCL, FireMonkey, WPF etc.
  /// </summary>
  TGIS_RendererAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF} class ( TGIS_BaseObjectDisposable )

    private
      FPPI       : Integer ;
      FFontScale : Integer ;
      FShift     : TPoint  ;
      FWidth     : Integer ;
      FHeight    : Integer ;
      FCanvasExtent : TGIS_Extent ;
      FTileRect  : TRect   ;

      oMeasureBmp : TGIS_Bitmap ;
      oMeasureRnd : TGIS_RendererAbstract ;
      oMeasureCtx : TGIS_RendererContext ;

    protected
      /// <summary>
      ///   X coordinate of extent location.
      ///  </summary>
      FExtentX    : Double  ;

      /// <summary>
      ///   Y coordinate of extent location.
      ///  </summary>
      FExtentY    : Double  ;

      /// <summary>
      ///   Zoom from viewer.
      ///  </summary>
      FZoom       : Double  ;

      /// <summary>
      ///   True if drawing is tiled.
      ///  </summary>
      FTiled      : Boolean ;

    public
      /// <summary>
      ///   Viewer (parented interface) which owns renderer.
      ///   For Control* like operations
      ///  </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      Parent  : IGIS_ViewerParent ;

      /// <summary>
      ///   Viewer which owns renderer.
      ///  </summary>
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      Viewer  : IGIS_Viewer ;

      /// <summary>
      ///   Renderer context.
      ///  </summary>
      Context : TGIS_RendererContext ;

    protected
      function  fget_BitmapFactory   : TGIS_BitmapFactory ; virtual;abstract;
      function  fget_ViewRect        : TRect ;
      function  fget_CanvasFont      : TGIS_Font ; virtual;abstract;
      procedure fset_CanvasFont      ( const _value : TGIS_Font
                                     ) ; virtual;abstract;
      function  fget_CanvasPen       : TGIS_Pen ; virtual;abstract;
      procedure fset_CanvasPen       ( const _value : TGIS_Pen
                                     ) ; virtual;abstract;
      function  fget_CanvasBrush     : TGIS_Brush ; virtual;abstract;
      procedure fset_CanvasBrush     ( const _value : TGIS_Brush
                                     ) ; virtual;abstract;

      function  fget_Info            : String ; virtual; abstract;

    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure doDestroy            ; override;

    protected
      /// <summary>
      ///   Get offset point including position.
      /// </summary>
      /// <param name="_params">
      ///   params section
      /// </param>
      /// <returns>
      ///   Offset as point
      /// </returns>
      function getOffsetPoint( const _params : TObject
                             ) : TPoint ;
    public
      /// <summary>
      ///   Standard constructor. Creates bitmap 320x240.
      /// </summary>
      constructor Create             ; {$IFDEF DCC} virtual ; {$ENDIF}

    public
      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device dependent pixels. Used to make map device independent.
      /// </summary>
      /// <param name="_size">
      ///   size in twips; if negative then treated as pixels
      /// </param>
      /// <returns>
      ///   Size in twips.
      /// </returns>
      /// <remarks>
      ///   This implementation will calculate always on actual canvas PPI
      ///   while TGIS_Viewer version can provide improper results on
      ///   non-standard temporary devices like bitmap output on different PPI
      ///  then default.
      ///  Maximum value is 4096 * PPI / 96.
      /// </remarks>
      function TwipsToPixels         ( const _size : Integer
                                     )  : Integer ; overload ;

      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device dependent pixels. Used to make map device independent.
      /// </summary>
      /// <param name="_size">
      ///   size in twips; if negative then treated as pixels
      /// </param>
      /// <param name="_maxValue">
      ///   maximal value
      /// </param>
      /// <returns>
      ///   Size in twips.
      /// </returns>
      /// <remarks>
      ///   This implementation will calculate always on actual canvas PPI
      ///   while TGIS_Viewer version can provide improper results on
      ///   non-standard temporary devices like bitmap output on different PPI
      ///  then default.
      ///  Maximum value is _maxValue.
      ///  If _maxValue is set to 0, then there is no limit.
      /// </remarks>
      function TwipsToPixels         ( const _size     : Integer ;
                                       const _maxValue : Integer
                                     )  : Integer ; overload ;

      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device points. Used for advanced font sizing.
      /// </summary>
      /// <param name="_size">
      ///   size in twips; if negative then treated as pixels
      /// </param>
      /// <returns>
      ///   Size in points.
      /// </returns>
      /// <remarks>
      ///   This implementation will calculate always on actual canvas PPI
      ///   while TGIS_Viewer version can provide improper results on
      ///   non-standard temporary devices like bitmap output on different PPI
      ///  then default.
      /// </remarks>
      function TwipsToPoints        ( const _size : Integer
                                    ) : Integer ;

      /// <summary>
      ///   Convert size from device dependent pixels to device independent
      ///   Twips (1/1440 inch). Used to make map device independent.
      /// </summary>
      /// <param name="_size">
      ///   size in pixels; absolute value will be taken
      /// </param>
      /// <returns>
      ///   Size in twips.
      /// </returns>
      /// <remarks>
      ///   This implementation will calculate always on actual canvas PPI
      ///   while TGIS_Viewer version can provide improper results on
      ///   non-standard temporary devices like bitmap output on different PPI
      ///  then default.
      /// </remarks>
      function PixelsToTwips        ( const _size : Integer
                                    ) : Integer ;

    // hi-level api
    public
      /// <summary>
      ///   Create an instance of object of the same type as current one.
      /// </summary>
      /// <returns>
      ///   Newly created object or null if the renderer is not allowed.
      /// </returns>
      function    CreateInstance     : TGIS_RendererAbstract ; virtual; abstract;

      /// <summary>
      ///   Setup renderer if CreateContext are not in use.
      /// </summary>
      /// <param name="_parent">
      ///   viewer parented interface; for Control* like methods
      /// </param>
      /// <param name="_viewer">
      ///   viewer object
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
      procedure   Setup              ( const _parent    : IGIS_ViewerParent ;
                                       const _viewer    : IGIS_Viewer ;
                                       const _shift     : TPoint  ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer
                                     ) ; overload; virtual;


      /// <summary>
      ///   Initiate context for the renderer.
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
      procedure  CreateContext       ( const _parent    : IGIS_ViewerParent ;
                                       const _viewer    : IGIS_Viewer ;
                                       const _context   : TGIS_RendererContext ;
                                       const _shift     : TPoint  ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer
                                     ) ; overload; virtual;

      /// <summary>
      ///   Initiate context for the renderer.
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
      /// <param name="_tilerect">
      ///   rendering context location; used upon rendering tiles
      /// </param>
      procedure   CreateContext      ( const _parent    : IGIS_ViewerParent ;
                                       const _viewer    : IGIS_Viewer ;
                                       const _context   : TGIS_RendererContext ;
                                       const _shift     : TPoint  ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer ;
                                       const _tilerect  : TRect
                                     ) ; overload; virtual;

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Initiate context for the printer.
      /// </summary>
      /// <param name="_canvas">
      ///   drawing context; in most cases canvas object
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
      /// <remarks>
      ///   Implemented only on selected renderer becuase printer is sometimes
      ///   implemented only on selected canas types.
      /// </remarks>
      procedure   CreatePrinterContext(
                                       const _canvas    : TObject ;
                                       const _width     : Integer ;
                                       const _height    : Integer ;
                                       const _ppi       : Integer ;
                                       const _fontscale : Integer
                                     ) ; virtual;
      /// <summary>
      ///   Restore context using same data as used by CreateContext.
      /// </summary>
      procedure   RestoreContext     ; virtual;

      /// <summary>
      ///   Release an instance.
      /// </summary>
      procedure   ReleaseContext     ; virtual;


      /// <summary>
      ///   Allow busy indicator upon rendering process.
      /// </summary>
      /// <result>
      ///   True if process should be interrupted.
      /// </result>
      procedure   PrepareHourglassContext
                                     ; virtual; abstract;


      /// <summary>
      ///   Receive notification about pending renderer update.
      /// </summary>
      procedure   Update             ; virtual; abstract;

      /// <summary>
      ///   Render a single shape in a current context.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be rendered
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shape selection is rendered
      /// </param>
      /// <param name="_outlineMode">
      ///   outline drawing mode; valid only for lines; used
      ///   to drawing nice line junctions in multipass mode;
      /// </param>
      procedure   RenderShape        ( const _shp      : TObject ;
                                       const _selectionOnly
                                                       : Boolean = False ;
                                       const _outlineMode
                                                       : TGIS_RendererMultipassMode =
                                                         TGIS_RendererMultipassMode.Single
                                     ) ; overload; virtual; abstract;

      /// <summary>
      ///   Render a single shape in a current context.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be rendered
      /// </param>
      /// <param name="_source">
      ///   original shape for proper drawing fills when _shp is truncated
      /// </param>
      /// <param name="_selectionOnly">
      ///   if True only the shape selection is rendered
      /// </param>
      /// <param name="_outlineMode">
      ///   outline drawing mode; valid only for lines; used
      ///   to drawing nice line junctions in multipass mode;
      /// </param>
      procedure   RenderShape        ( const _shp      : TObject ;
                                       const _source   : TObject ;
                                       const _selectionOnly
                                                       : Boolean = False ;
                                       const _outlineMode
                                                       : TGIS_RendererMultipassMode =
                                                         TGIS_RendererMultipassMode.Single
                                     ) ; overload; virtual; abstract;

      /// <summary>
      ///   Render a label for a single shape in a current context.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be rendered
      /// </param>
      procedure   RenderLabel        ( const _shp      : TObject
                                     ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Render a label for a single shape in a current context.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be rendered
      /// </param>
      /// <param name="_points">
      ///   list of points delimiting label area
      /// </param>
      /// <para>
      ///   See TGIS_Shape.DrawLabel( TGIS_DrawBuf ) for more information.
      /// </para>
      procedure   RenderLabel        ( const _shp      : TObject ;
                                       var   _points   : TGIS_DrawBuf
                                     ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Render a chart for a single shape in a current context.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be rendered
      /// </param>
      procedure   RenderChart        ( const _shp      : TObject
                                     ) ; virtual; abstract;


      /// <summary>
      ///   Measure shield size
      /// </summary>
      /// <param name="_shp">
      ///   shape for which shield must be rendered
      /// </param>
      /// <param name="_size">
      ///   actual active size of shield
      /// </param>
      /// <returns>
      ///   Newly create label layout. Must be free directly or will be free
      ///   indirectly upon RenderShieldTexture call.
      /// </returns>
      function    MeasureShieldTexture( const _shp        : TObject ;
                                       var   _size       : TPoint
                                     ) : TObject; virtual;

      /// <summary>
      ///   Render texture shield (label + chart + whatever) for 3D viewer
      ///   purposes
      /// </summary>
      /// <param name="_shp">
      ///   shape for which shield must be rendered
      /// </param>
      /// <param name="_force_pow2">
      ///   If true size of resulting bitmaps is always power of 2 because some
      ///   3D engine requires it
      /// </param>
      /// <param name="_size">
      ///   actual active size of shield within bitmap which can be power 2
      ///   sized
      /// </param>
      /// <param name="_layout">
      ///   label layout allocated by MeasureShieldTexture; this method will
      ///   release this object
      /// </param>
      /// <returns>
      ///   Newly created bitmap or nil.
      /// </returns>
      function    RenderShieldTexture( const _shp        : TObject ;
                                       const _force_pow2 : Boolean ;
                                       const _size       : TPoint  ;
                                       const _layout     : TObject
                                     ) : TGIS_Bitmap; overload; virtual;

      /// <summary>
      ///   Render texture shield (label + chart + whatever) for 3D viewer
      ///   purposes
      /// </summary>
      /// <param name="_shp">
      ///   shape for which shield must be rendered
      /// </param>
      /// <param name="_force_pow2">
      ///   If true size of resulting bitmaps is always power of 2 because some
      ///   3D engine requires it
      /// </param>
      /// <param name="_size">
      ///   actual active size of shield within bitmap which can be power 2
      ///   sized
      /// </param>
      /// <returns>
      ///   Newly created bitmap or nil.
      /// </returns>
      function    RenderShieldTexture( const _shp        : TObject ;
                                       const _force_pow2 : Boolean ;
                                       var   _size       : TPoint
                                     ) : TGIS_Bitmap; overload; virtual;
      /// <summary>
      ///   Render shape in a flush state
      /// </summary>
      /// <param name="_shp">
      ///   shape to be rendered
      /// </param>
      procedure   RenderShapeFlashed ( const _shp      : TObject
                                     ) ; virtual; abstract;

      /// <summary>
      ///   Prepare custom rendering context for lengthy bitmap drawing. Use
      ///   for example to display progressive update of ECW files.
      /// </summary>
      /// <returns>
      ///   Handle to the context.
      /// </returns>
      function    RenderBitmapBegin  : TObject ; virtual; abstract;

      /// <summary>
      ///   Finalize custom rendering context.
      /// </summary>
      /// <param name="_handle">
      ///   handle to the context
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Must be paired with RenderBitmapBegin
      ///    </note>
      /// </remarks>
      procedure   RenderBitmapEnd    ( const _handle   : TObject
                                     ) ; virtual; abstract;

      /// <summary>
      ///   Render bitmap on a current canvas.
      /// </summary>
      /// <param name="_handle">
      ///   handle to the context obtained by RenderBitmapBegin;
      ///   cam be nil  for non progressive display
      /// </param>
      /// <param name="_bmp">
      ///   array of bitmap pixels
      /// </param>
      /// <param name="_size">
      ///   width/height of _bmp (in pixels)
      /// </param>
      /// <param name="_dst">
      ///   destination rectangle (in pixels); scaling will apply if required
      /// </param>
      /// <param name="_format">
      ///   pixel format of _bmp array
      /// </param>
      /// <param name="_order">
      ///   line order of _bmp array
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Must be paired with RenderBitmapBegin
      ///    </note>
      /// </remarks>
      procedure   RenderBitmap       ( const _handle   : TObject ;
                                       const _bmp      : TGIS_Pixels ;
                                       const _size     : TPoint  ;
                                       const _dst      : TRect   ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) ; overload; virtual; abstract;

      /// <summary>
      ///   Render bitmap on a current canvas with scaling.
      /// </summary>
      /// <param name="_handle">
      ///   handle to progressive layer context or nil
      /// </param>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_dst">
      ///   destination rectangle (to which bitmap should be rendered)
      /// </param>
      /// <param name="_antialias">
      ///   if True, rendering is anti-aliased if the framework gives such a possibility
      /// </param>
      procedure   RenderBitmap       ( const _handle   : TObject ;
                                       const _bmp      : TGIS_Bitmap ;
                                       const _dst      : TRect   ;
                                       const _antialias : Boolean
                                     ) ; overload; virtual; abstract;

      /// <summary>
      ///   Render an editor (if any).
      /// </summary>
      /// <param name="_context">
      ///   Rendering context (Canvas) of a final device.
      /// </param>
      procedure   RenderEditor       ( const _context  : TObject
                                     ) ; virtual; abstract;

      /// <summary>
      ///   Prepare renderer cache for topmost bitmap layers.
      /// </summary>
      /// <param name="_bmp">
      ///   array of bitmap pixels
      /// </param>
      /// <param name="_extent">
      ///   extent of _bmp
      /// </param>
      /// <param name="_size">
      ///   width/height of _bmp (in pixels)
      /// </param>
      /// <param name="_serial">
      ///   serial number of layer.Params; to keep track of layer rendering
      ///   parameters change
      /// </param>
      /// <param name="_format">
      ///   pixel format of _bmp array
      /// </param>
      /// <param name="_order">
      ///   line order of _bmp array
      /// </param>
      /// <returns>
      ///   returns cached object or nil, if cache is not supported by
      ///   renderer; time live of the object is maintained by TGIS_ViewerWnd
      /// </returns>
      function    PrepareBitmapCache ( const _bmp      : TGIS_Pixels ;
                                       const _extent   : TGIS_Extent ;
                                       const _size     : TPoint      ;
                                       const _serial   : Integer     ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) : TGIS_RendererAbstractCache ;
                                     virtual;

      /// <summary>
      ///   Render bitmap cache for topmost pixel layers.
      /// </summary>
      /// <param name="_handle">
      ///   handle to the context obtained by RenderBitmapBegin;
      ///   cam be nil  for non progressive display
      /// </param>
      /// <param name="_cache">
      ///   cache obtained via PrepareBitmapCache
      /// </param>
      /// <param name="_dst">
      ///   destination rectangle (in pixels); scaling will apply if required
      /// </param>
      procedure   RenderBitmapCache  ( const _handle   : TObject ;
                                       const _cache    : TGIS_RendererAbstractCache ;
                                       const _dst      : TRect
                                     ) ; virtual;

      /// <summary>
      ///   Preparation of the draw state.
      /// </summary>
      procedure   PrepareDraw        ; virtual;

      /// <summary>
      ///   Preparation of the draw state.
      /// </summary>
      /// <remarks>
      ///   <note type="caution">
      ///     Use PrepareDraw instead.
      ///   </note>
      /// </remarks>
      procedure   BeforeDraw        ; virtual;
                                     {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Receive notification about pending renderer update.
      /// </summary>
      procedure   AfterDraw          ; virtual; abstract;

      /// <summary>
      ///   Prepare context for PaintShapeChartEvent.
      /// </summary>
      procedure   PrepareDrawCharts  ; virtual; abstract;

      /// <summary>
      ///   Restore context after PaintShapeChartEvent.
      /// </summary>
      procedure   AfterDrawCharts    ; virtual; abstract;

      /// <summary>
      ///   Prepare context for PaintShapeLabelEvent.
      /// </summary>
      procedure   PrepareDrawLabels  ; virtual; abstract;

      /// <summary>
      ///   Restore context after PaintShapeLabelEvent.
      /// </summary>
      procedure   AfterDrawLabels    ; virtual; abstract;


      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   Paint an extra context on a top of a rendered map. Can be used for
      ///   example to drawing edited shape.
      /// </summary>
      /// <param name="_sender">
      ///   Sender to be passed to PainExtra event.
      /// </param>
      /// <param name="_context">
      ///   Rendering context (Canvas) of a final device.
      /// </param>
      /// <param name="_event">
      ///   Event to be fired.
      /// </param>
      procedure   PaintExtra         ( const _sender    : TObject ;
                                       const _context   : TObject ;
                                       const _event     : TGIS_RendererEvent
                                     ) ; overload; virtual; abstract;


      {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
      /// <summary>
      ///   Paint an extra context on a top of a rendered map. Can be used for
      ///   example to drawing edited shape.
      /// </summary>
      /// <param name="_sender">
      ///   Sender to be passed to PainExtra event.
      /// </param>
      /// <param name="_context">
      ///   Rendering context (Canvas) of a final device.
      /// </param>
      /// <param name="_event">
      ///   Event to be fired.
      /// </param>
      procedure   PaintExtra         ( const _sender    : TObject ;
                                       const _context   : TObject ;
                                       const _event     : TGIS_PaintEvent
                                     ) ; overload; virtual; abstract;


      /// <summary>
      ///   Begin of drawing in a layer transparency mode.
      /// </summary>
      /// <param name="_transparency">
      ///   0.100 where 0 means fully transparent and 100 solid
      /// </param>
      procedure   LockTransparent    ( const _transparency : Integer
                                     ) ; virtual; abstract;

      /// <summary>
      ///   End of drawing in a layer transparency mode.
      /// </summary>
      procedure   UnlockTransparent  ; virtual; abstract;

      /// <summary>
      ///   Update BaseMap of the context .
      /// </summary>
      procedure   Flush              ; virtual; abstract;

      /// <summary>
      ///   Check the content of bitmap cache and optimize it.
      /// </summary>
      procedure   OptimizeBitmapCache; virtual;

    {$IFNDEF JAVA}
    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_width">
    ///   paint width
    /// </param>
    /// <param name="_height">
    ///   paint height
    /// </param>
    /// <param name="_canvas">
    ///   source canvas
    /// </param>
    /// <returns>
    ///   paint object
    /// </returns>
    function  ViewerCreateWndPaint   ( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _canvas       : TObject
                                     ) : TObject ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   object to free
    /// </param>
    procedure ViewerFreeWndPaint     ( var   _paint        : TObject
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_localCanvas">
    ///   canvas to flush
    /// </param>
    /// <param name="_canvas">
    ///   canvas to draw on
    /// </param>
    procedure ViewerFlushToWndPaint  ( const _localCanvas  : TObject ;
                                       const _canvas       : TObject
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_canvas">
    ///   canvas to draw on
    /// </param>
    /// <param name="_width">
    ///   canvas width
    /// </param>
    /// <param name="_height">
    ///   canvas height
    /// </param>
    /// <param name="_color">
    ///   background color
    /// </param>
    procedure ViewerDrawBackground   ( const _canvas       : TObject ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _color        : TGIS_Color
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_bitmap">
    ///   bitmap to copy
    /// </param>
    /// <returns>
    ///   cache bitmap
    /// </returns>
    function ViewerPrepareCache      ( const _bitmap       : TGIS_Bitmap
                                     ) : TGIS_Bitmap ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_width">
    ///   paint width
    /// </param>
    /// <param name="_height">
    ///   paint height
    /// </param>
    /// <param name="_context">
    ///   source object
    /// </param>
    /// <returns>
    ///   temporary paint object
    /// </returns>
    function  ViewerCreateTemporaryPaint
                                     ( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _context      : TObject
                                     ) : TObject ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_width">
    ///   paint width
    /// </param>
    /// <param name="_height">
    ///   paint height
    /// </param>
    /// <param name="_context">
    ///   source object
    /// </param>
    /// <returns>
    ///   temporary paint object
    /// </returns>
    function  ViewerCreateTemporaryPaintEx
                                     ( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _context      : TObject
                                     ) : TObject ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_context">
    ///   source bitmap
    /// </param>
    /// <returns>
    ///   temporary paint object
    /// </returns>
    function  ViewerCreateTemporaryPaint
                                     ( const _context      : TGIS_Bitmap
                                     ) : TObject ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    procedure ViewerFreeTemporaryPaint
                                     ( var   _paint        : TObject
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_bitmap">
    ///   bitmp to draw on
    /// </param>
    procedure ViewerFreeTemporaryPaint
                                     ( var   _paint        : TObject ;
                                       const _bitmap       : TGIS_Bitmap
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    procedure ViewerClearTemporaryPaint
                                     ( const _paint        : TObject
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_canvas">
    ///   canvas to draw on
    /// </param>
    /// <param name="_width">
    ///   canvas width
    /// </param>
    /// <param name="_height">
    ///   canvas height
    /// </param>
    /// <param name="_paint">
    ///   paint to draw
    /// </param>
    /// <param name="_rect">
    ///   rectangle to draw from
    /// </param>
    procedure ViewerFlushTemporaryPaint
                                     ( const _canvas       : TObject ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_canvas">
    ///   canvas to draw on
    /// </param>
    /// <param name="_width">
    ///   canvas width
    /// </param>
    /// <param name="_height">
    ///   canvas height
    /// </param>
    /// <param name="_paint">
    ///   paint to draw
    /// </param>
    /// <param name="_fullcache">
    ///   paint to draw
    /// </param>
    /// <param name="_rect">
    ///   rectangle to draw from
    /// </param>
    procedure ViewerFlushTemporaryPaint
                                     ( const _canvas       : TObject ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _paint        : TObject ;
                                       const _fullcache    : TGIS_Bitmap ;
                                       const _rect         : TRect
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_color">
    ///   background color
    /// </param>
    procedure ViewerBeginDrawOnTemporaryPaint
                                     ( const _paint        : TObject ;
                                       const _color        : TGIS_Color
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    procedure ViewerEndDrawOnTemporaryPaint
                                     ( const _paint        : TObject
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_bitmap">
    ///   source bitmap
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_rect">
    ///   rectangle
    /// </param>
    procedure ViewerStretchBitmapFast( const _bitmap       : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_bitmap">
    ///   source bitmap
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_rect">
    ///   rectangle
    /// </param>
    /// <param name="_transparency">
    ///   bitmap transparency
    /// </param>
    procedure ViewerStretchBitmapFast( const _bitmap       : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect   ;
                                       const _transparency : Integer
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_bitmap">
    ///   source bitmap
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_transparency">
    ///   transparency factor
    /// </param>
    /// <param name="_merge_alpha">
    ///   merge alpha flag
    /// </param>
    procedure ViewerBlendBitmaps     ( const _bitmap       : TObject ;
                                       const _paint        : TObject ;
                                       const _transparency : Integer ;
                                       const _merge_alpha  : Boolean
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_bitmap">
    ///   source bitmap
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_merge_alpha">
    ///   merge alpha flag
    /// </param>
    procedure ViewerBlendLabelBitmaps( const _bitmap       : TObject ;
                                       const _paint        : TObject ;
                                       const _merge_alpha  : Boolean
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_width">
    ///   cache width
    /// </param>
    /// <param name="_height">
    ///   cache height
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_addObj">
    ///   additional object
    /// </param>
    /// <returns>
    ///   created cache
    /// </returns>
    function  ViewerCreateFullCache  ( const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _paint        : TObject ;
                                       const _addObj       : TObject
                                     ) : TGIS_Bitmap ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_paint">
    ///   paint to draw
    /// </param>
    /// <param name="_fullcache">
    ///   paint to draw
    /// </param>
    procedure ViewerFlushToFullCache ( var   _paint        : TObject ;
                                       const _fullcache    : TGIS_Bitmap
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_cache">
    ///   cache to draw
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_rect">
    ///   rectangle to draw in
    /// </param>
    procedure ViewerDrawCache        ( const _cache        : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_cache">
    ///   cache to draw
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_rect">
    ///   rectangle to draw in
    /// </param>
    /// <param name="_transparency">
    ///   transparency factor
    /// </param>
    procedure ViewerDrawCache        ( const _cache        : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect   ;
                                       const _transparency : Integer
                                     ) ; overload ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_bitmap">
    ///   bitmap to draw
    /// </param>
    /// <param name="_paint">
    ///   paint object
    /// </param>
    /// <param name="_rect">
    ///   rectangle
    /// </param>
    procedure ViewerDrawProgressBitmap
                                     ( const _bitmap       : TObject ;
                                       const _paint        : TObject ;
                                       const _rect         : TRect
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_canvas">
    ///   canvas to draw on
    /// </param>
    /// <param name="_x">
    ///   x-coordinate of the rectangle
    /// </param>
    /// <param name="_y">
    ///   y-coordinate of the rectangle
    /// </param>
    /// <param name="_width">
    ///   width of the rectangle
    /// </param>
    /// <param name="_height">
    ///   height of the rectangle
    /// </param>
    /// <param name="_color">
    ///   color of the rectangle
    /// </param>
    procedure ViewerDrawZoomingRect  ( const _canvas       : TObject ;
                                       const _x            : Integer ;
                                       const _y            : Integer ;
                                       const _width        : Integer ;
                                       const _height       : Integer ;
                                       const _color        : TGIS_Color
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Internal use only.
    /// </summary>
    /// <param name="_canvas">
    ///   canvas to draw on
    /// </param>
    /// <param name="_x1">
    ///   x-coordinate of the point 1
    /// </param>
    /// <param name="_y1">
    ///   y-coordinate of the point 1
    /// </param>
    /// <param name="_x2">
    ///   x-coordinate of the point 2
    /// </param>
    /// <param name="_y2">
    ///   y-coordinate of the point 2
    /// </param>
    /// <param name="_color">
    ///   color of the track
    /// </param>
    procedure ViewerDrawDraggingTrack( const _canvas       : TObject ;
                                       const _x1           : Integer ;
                                       const _y1           : Integer ;
                                       const _x2           : Integer ;
                                       const _y2           : Integer ;
                                       const _color        : TGIS_Color
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Draws control on the given context.
    ///   Internal use only.
    /// </summary>
    /// <param name="_context">
    ///   context to draw on
    /// </param>
    /// <param name="_bitmap">
    ///   bitmap to draw
    /// </param>
    /// <param name="_x">
    ///   x coordinate
    /// </param>
    /// <param name="_y">
    ///   y coordinate
    /// </param>
    procedure ControlDrawNonTransparent
                                     ( const _context   : TObject ;
                                       const _bitmap    : TGIS_Bitmap ;
                                       const _x         : Integer ;
                                       const _y         : Integer
                                     ) ; virtual ;

    {#gendoc:hide}
    /// <summary>
    ///   Draws transparent control on the given context.
    ///   Internal use only.
    /// </summary>
    /// <param name="_context">
    ///   context to draw on
    /// </param>
    /// <param name="_bitmap">
    ///   bitmap to draw
    /// </param>
    /// <param name="_x">
    ///   x coordinate
    /// </param>
    /// <param name="_y">
    ///   y coordinate
    /// </param>
    procedure ControlDrawTransparent ( const _context   : TObject ;
                                       const _bitmap    : TObject ;
                                       const _x         : Integer ;
                                       const _y         : Integer
                                     ) ; virtual ;
   {$ENDIF}

    public
      /// <summary>
      ///   Font of the Canvas object.
      /// </summary>
      property CanvasFont  : TGIS_Font  read  fget_CanvasFont
                                        write fset_CanvasFont  ;

      /// <summary>
      ///   Pen of the Canvas object.
      /// </summary>
      property CanvasPen   : TGIS_Pen   read  fget_CanvasPen
                                        write fset_CanvasPen   ;

      /// <summary>
      ///   Brush of the Canvas object.
      /// </summary>
      property CanvasBrush : TGIS_Brush read  fget_CanvasBrush
                                        write fset_CanvasBrush ;

      /// <summary>
      ///   Point-Per-Inch of the rendering context.
      /// </summary>
      property PPI         : Integer    read  FPPI   ;

      /// <summary>
      ///   Fonts scaling factor of the rendering context (in percents).
      /// </summary>
      property FontScale   : Integer    read  FFontScale ;

      /// <summary>
      ///   Offset of the rendering rectangle (in pixels).
      /// </summary>
      property Shift       : TPoint     read  FShift ;

      /// <summary>
      ///   Width of the rendering context (in pixels).
      /// </summary>
      property Width       : Integer    read  FWidth ;

      /// <summary>
      ///   Height of the rendering context (in pixels).
      /// </summary>
      property Height      : Integer    read  FHeight ;

      /// <summary>
      ///   Viewing rectangle of the rendering context (in pixels).
      ///   It is computed as Shift+Width and Shift+Height
      /// </summary>
      property ViewRect    : TRect      read  fget_ViewRect ;


      /// <summary>
      ///   Provide basic info about renderer like underlying technology used.
      /// </summary>
      property Info        : String     read  fget_Info ;

      /// <summary>
      ///   Return extent taken in account by label allocator.
      ///  </summary>
      property CanvasExtent : TGIS_Extent read FCanvasExtent ;

      /// <summary>
      ///   Rectangle of the rendering context (in pixels).
      /// </summary>
      /// <remarks>
      ///   For tiled drawing it is a location of the tile on the final output.
      ///   For non tiled drawing it is (0, 0, Width, Height).
      /// </remarks>
      property TileRect     : TRect      read  FTileRect ;

      /// <summary>
      ///   Returns a bitmap factory which produces bitmaps compatible with the renderer.
      /// </summary>
      property BitmapFactory : TGIS_BitmapFactory
                                         read  fget_BitmapFactory ;

    public

      /// <summary>
      ///   Get user-friendly name of the renderer.
      ///   Can be used to expose the renderer in the user interface.
      /// </summary>
      /// <returns>
      ///   User-friendly name of the renderer.
      /// </returns>
      function FriendlyName : String ; virtual ; abstract ;

    public

      /// <summary>
      ///   Return rendering context native to current renderer.
      /// </summary>
      /// <returns>
      ///   Canvas object specific for current renderer object.
      /// </returns>
      function CanvasNative         : TObject ; virtual; abstract;

      /// <summary>
      ///   Provide basing metric for a currently selected CanvasFont.
      /// </summary>
      /// <param name="_break_char">
      ///   character treated as a break character (space)
      /// </param>
      /// <param name="_height">
      ///   font height in pixels
      /// </param>
      /// <param name="_ascent">
      ///   font ascent size in pixels
      /// </param>
      /// <param name="_true_type">
      ///   True if font is TrueType or equivalent (scalable as vector)
      /// </param>
      procedure CanvasFontMetrics   ( var   _break_char  : Char    ;
                                      var   _height      : Integer ;
                                      var   _ascent      : Integer ;
                                      var   _true_type   : Boolean
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Provide a baseline for the passed text and a currently selected CanvasFont.
      /// </summary>
      /// <param name="_text">
      ///   text for which we set a baseline
      /// </param>
      /// <returns>
      ///   Baseline for text or -1 if the calculation is not possible
      /// </returns>
      function CanvasTextBaseline   ( const _text        : String
                                    ) : Single ; virtual; abstract;

      /// <summary>
      ///   Calculate text EM value for a currently selected CanvasFont.
      /// </summary>
      /// <param name="_text">
      ///   text for which extent will be calculated
      /// </param>
      /// <returns>
      ///   Text extent in pixels.
      /// </returns>
      function  CanvasTextExtent    ( const _text        : String
                                    ) : TPoint ; virtual; abstract;

      /// <summary>
      ///   Calculate text EM value for a currently selected CanvasFont.
      /// </summary>
      /// <param name="_em">
      ///   number of em
      /// </param>
      /// <returns>
      ///   Calculated values in pixels.
      /// </returns>
      function  CanvasTextEm        ( const _em          : Double
                                    ) : Integer ; virtual;

      /// <summary>
      ///   Draw a text on the Canvas object.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle (in pixels)
      /// </param>
      /// <param name="_text">
      ///   text to be drawn
      /// </param>
      procedure CanvasDrawText      ( const _rect        : TRect   ;
                                      const _text        : String
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Draw a line on the Canvas object.
      /// </summary>
      /// <param name="_x1">
      ///   x of staring point (in pixels)
      /// </param>
      /// <param name="_y1">
      ///   y of staring point (in pixels)
      /// </param>
      /// <param name="_x2">
      ///   x of ending point (in pixels)
      /// </param>
      /// <param name="_y2">
      ///   y of ending point (in pixels)
      /// </param>
      procedure CanvasDrawLine      ( const _x1          : Integer ;
                                      const _y1          : Integer ;
                                      const _x2          : Integer ;
                                      const _y2          : Integer
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a poly-line on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      procedure CanvasDrawPolyLine  ( const _points      : TGIS_DrawBuf
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a poly-line on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      procedure CanvasDrawPolyLine  ( const _points      : TGIS_DrawBufF
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a poly-line on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_count">
      ///   number of points to be drawn from the _points array
      /// </param>
      procedure CanvasDrawPolyLine  ( const _points      : TGIS_DrawBuf ;
                                      const _count       : Integer
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a poly-line on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_count">
      ///   number of points to be drawn from the _points array
      /// </param>
      procedure CanvasDrawPolyLine  ( const _points      : TGIS_DrawBufF ;
                                      const _count       : Integer
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a poly-line on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_parts">
      ///   array of all parts; each entry is a part index starting offset
      /// </param>
      procedure CanvasDrawPolyLine  ( const _points      : TGIS_DrawBuf ;
                                      const _parts       : TGIS_IntegerArray
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a poly-line on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_parts">
      ///   array of all parts; each entry is a part index starting offset
      /// </param>
      procedure CanvasDrawPolyLine  ( const _points      : TGIS_DrawBufF ;
                                      const _parts       : TGIS_IntegerArray
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a rectangle on the Canvas object.
      /// </summary>
      /// <param name="_rect">
      ///   rectangle (in pixels)
      /// </param>
      procedure CanvasDrawRectangle ( const _rect        : TRect
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Draw an ellipse on the Canvas object.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the upper-left corner of the bounding rectangle
      ///  (in pixels)
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the upper-left corner of the bounding rectangle
      ///  (in pixels)
      /// </param>
      /// <param name="_width">
      ///   ellipse width (in pixels)
      /// </param>
      /// <param name="_height">
      ///   ellipse height (in pixels)
      /// </param>
      procedure CanvasDrawEllipse   ( const _x           : Integer ;
                                      const _y           : Integer ;
                                      const _width       : Integer ;
                                      const _height      : Integer
                                    ) ; virtual; abstract;
      /// <summary>
      ///   Draw a pie on the Canvas object.
      /// </summary>
      /// <param name="_angle_0">
      ///   starting angle clockwise from the x-axis to the first side of
      ///   the pipe (in radians)
      /// </param>
      /// <param name="_angle_1">
      ///   ending angle from the first side to the second side of
      ///   the pipe (in radians)
      /// </param>
      /// <param name="_radius">
      ///   pie radius (in radians)
      /// </param>
      /// <param name="_origin_x">
      ///   x origin point of pie center (in pixels)
      /// </param>
      /// <param name="_origin_y">
      ///   y origin point of pie center (in pixels)
      /// </param>
      procedure CanvasDrawPie       ( const _angle_0     : Double  ;
                                      const _angle_1     : Double  ;
                                      const _radius      : Integer ;
                                      const _origin_x    : Integer ;
                                      const _origin_y    : Integer
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Draw an arc on the Canvas object.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the upper-left corner of the bounding rectangle
      ///  (in pixels)
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the upper-left corner of the bounding rectangle
      ///  (in pixels)
      /// </param>
      /// <param name="_width">
      ///   width of the ellipse that contains the arc (in pixels)
      /// </param>
      /// <param name="_height">
      ///   height of the ellipse that contains the arc (in pixels)
      /// </param>
      /// <param name="_startX">
      ///   x-coordinate of starting point defined by the intersection of the
      ///   ellipse and a line defined by the center (in pixels)
      /// </param>
      /// <param name="_startY">
      ///   y-coordinate of starting point defined by the intersection of the
      ///   ellipse and a line defined by the center (in pixels)
      /// </param>
      /// <param name="_endX">
      ///   x-coordinate of ending point defined by the intersection of the
      ///   ellipse and a line defined by the center (in pixels)
      /// </param>
      /// <param name="_endY">
      ///   y-coordinate of ending point defined by the intersection of the
      ///   ellipse and a line defined by the center (in pixels)
      /// </param>
      procedure CanvasDrawArc       ( const _x           : Integer ;
                                      const _y           : Integer ;
                                      const _width       : Integer ;
                                      const _height      : Integer ;
                                      const _startX      : Integer ;
                                      const _startY      : Integer ;
                                      const _endX        : Integer ;
                                      const _endY        : Integer
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw an arc on the Canvas object.
      /// </summary>
      /// <param name="_x">
      ///   x-coordinate of the center of the ellipse that contains the arc
      ///  (in pixels)
      /// </param>
      /// <param name="_y">
      ///   y-coordinate of the center of the ellipse that contains the arc
      ///  (in pixels)
      /// </param>
      /// <param name="_radius">
      ///   radius of the ellipse that contains the arc
      /// </param>
      /// <param name="_startAngle">
      ///   angle between the x-axis and the starting point of the arc
      /// </param>
      /// <param name="_sweepAngle">
      ///   angle between the starting and ending points of the arc
      /// </param>
      procedure CanvasDrawArc       ( const _x          : Integer ;
                                      const _y          : Integer ;
                                      const _radius     : Integer ;
                                      const _startAngle : Single ;
                                      const _sweepAngle : Single
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a polygon on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      procedure CanvasDrawPolygon   ( const _points      : TGIS_DrawBuf
                                    ) ; overload;virtual; abstract;

      /// <summary>
      ///   Draw a polygon on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      procedure CanvasDrawPolygon   ( const _points      : TGIS_DrawBufF
                                    ) ; overload;virtual; abstract;

      /// <summary>
      ///   Draw a polygon on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_count">
      ///   number of points to be drawn from the _points array
      /// </param>
      procedure CanvasDrawPolygon   ( const _points      : TGIS_DrawBuf ;
                                      const _count       : Integer
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a polygon on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_count">
      ///   number of points to be drawn from the _points array
      /// </param>
      procedure CanvasDrawPolygon   ( const _points      : TGIS_DrawBufF ;
                                      const _count       : Integer
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a polygon on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_parts">
      ///   array of all parts; each entry is a part index starting offset
      /// </param>
      procedure CanvasDrawPolygon   ( const _points      : TGIS_DrawBuf ;
                                      const _parts       : TGIS_IntegerArray
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw a polygon on the Canvas object.
      /// </summary>
      /// <param name="_points">
      ///   array of all points (in pixels)
      /// </param>
      /// <param name="_parts">
      ///   array of all parts; each entry is a part index starting offset
      /// </param>
      procedure CanvasDrawPolygon   ( const _points      : TGIS_DrawBufF ;
                                      const _parts       : TGIS_IntegerArray
                                    ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw bitmap on the Canvas object.
      /// </summary>
      /// <param name="_bmp">
      ///   array of bitmap pixels
      /// </param>
      /// <param name="_size">
      ///   width/height of _bmp (in pixels)
      /// </param>
      /// <param name="_dst">
      ///   destination rectangle (in pixels); scaling will apply if required
      /// </param>
      /// <param name="_format">
      ///   pixel format of _bmp array
      /// </param>
      /// <param name="_order">
      ///   line order of _bmp array
      /// </param>
      procedure   CanvasDrawBitmap   ( const _bmp      : TGIS_Pixels ;
                                       const _size     : TPoint  ;
                                       const _dst      : TRect   ;
                                       const _format   : TGIS_BitmapFormat ;
                                       const _order    : TGIS_BitmapLinesOrder
                                     ) ; overload; virtual; abstract;

      /// <summary>
      ///   Draw bitmap on the Canvas object.
      /// </summary>
      /// <param name="_bmp">
      ///   bitmap to be drawn
      /// </param>
      /// <param name="_dst">
      ///   destination rectangle (in pixels); scaling will apply if required
      /// </param>
      procedure   CanvasDrawBitmap   ( const _bmp       : TGIS_Bitmap       ;
                                       const _dst       : TRect
                                     ) ; overload; virtual; abstract;

      /// <summary>
      ///   Set a transformation on the Canvas object.
      /// </summary>
      /// <param name="_angle">
      ///   rotation angle in radians
      /// </param>
      /// <param name="_origin_x">
      ///   rotation angle x origin point (in pixels)
      /// </param>
      /// <param name="_origin_y">
      ///   rotation angle y origin point (in pixels)
      /// </param>
      procedure CanvasSetTransformation
                                    ( const _angle       : Double  ;
                                      const _origin_x    : Integer ;
                                      const _origin_y    : Integer
                                    ) ; virtual; abstract;

      /// <summary>
      ///   Reset any transformation on the Canvas object.
      /// </summary>
      procedure CanvasClearTransformation ; virtual; abstract;

  end ;

  /// <summary>
  ///   Cached topmost pixel layer bitmap object to be used with
  ///   PrepareBitmapCache and RenderBitmapCache.
  /// </summary>
  TGIS_RendererAbstractCache = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_ObjectDisposable )
    private
      FExtent : TGIS_Extent ;
      FSize   : TPoint      ;
      FSerial : Integer     ;
    public
      /// <summary>
      ///   Extent of the cached bitmap.
      /// </summary>
      property Extent : TGIS_Extent read FExtent ;

      /// <summary>
      ///   Size of of the cached bitmap.
      /// </summary>
      property Size   : TPoint      read FSize   ;

      /// <summary>
      ///   Layer.Params serial value.
      /// </summary>
      property Serial : Integer     read FSerial ;

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_extent">
      ///   extent of the cached bitmap
      /// </param>
      /// <param name="_size">
      ///   size of of the cached bitmap
      /// </param>
      /// <param name="_serial">
      ///   layer.Params serial value
      /// </param>
      constructor Create            ( const _extent      : TGIS_Extent ;
                                      const _size        : TPoint      ;
                                      const _serial      : Integer
                                    ) ;
    public
      /// <summary>
      ///   Reset serial number to force cache recreation.
      /// </summary>
      procedure ResetSerial ;
  end ;


  /// <summary>
  ///   Basic renderer class.
  /// </summary>
  TGIS_RendererAbstractClass = {$IFDEF OXYGENE} public {$ENDIF}
                               class of TGIS_RendererAbstract ;


  /// <summary>
  ///   Registration list of all renderers.
  /// </summary>
  TGIS_RendererManager = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      oItems : TDictionary< String, TGIS_RendererAbstractClass > ;
      oNames : TGIS_StringList ;
    public
      /// <summary>
      ///  Standard constructor
      /// </summary>
      constructor Create  ;

    protected
      procedure doDestroy ; override ;

    private
      function fget_Names : TGIS_StringList ;
    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Add renderer to the registrar.
      /// </summary>
      /// <param name="_name">
      ///   unique operation name; name is case insensitive
      /// </param>
      /// <param name="_type">
      ///   calls providing this operation
      /// </param>
      procedure Add       ( const _name : String ;
                            const _type : TGIS_RendererAbstractClass
                          ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Remove renderer from the registrar.
      /// </summary>
      /// <param name="_name">
      ///   unique operation name; name is case insensitive
      /// </param>
      procedure Remove    ( const _name : String
                          ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Get renderer type from the registrar.
      /// </summary>
      /// <param name="_name">
      ///   unique operation name; name is case insensitive
      /// </param>
      /// <returns>
      ///   renderer class type
      /// </returns>
      function  Get       ( const _name : String
                          ) : TGIS_RendererAbstractClass ;

      /// <summary>
      ///    Create new instance of renderer.
      /// </summary>
      /// <param name="_name">
      ///   name of the renderer; name is case insensitive
      /// </param>
      /// <returns>
      ///   new instance
      /// </returns>
      function  CreateInstance( const _name : String
                              ) : TGIS_RendererAbstract ;

      /// <summary>
      ///   Check if the renderer is registered.
      /// </summary>
      /// <param name="_name">
      ///   unique operation name; name is case insensitive
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Sometimes, however, only the first call to the constructor
      ///     ultimately determines whether the renderer can be used.
      ///   </para>
      /// </remarks>
      /// <returns>
      ///   value indicated if the renderer is registered
      /// </returns>
      function  IsRegistered  ( const _name : String
                              ) : Boolean ;

    published
      /// <summary>
      ///   List of all renderers names.
      /// </summary>
      property Names : TGIS_StringList read fget_Names ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Register a renderer
  /// </summary>
  /// <param name="_name">
  ///   unique name of the renderer; name is case insensitive
  /// </param>
  /// <param name="_class">
  ///   class implementing renderer
  /// </param>
  procedure RegisterRenderer(
    const _name  : String ;
    const _class : TGIS_RendererAbstractClass
  ) ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///    Manager to register and create new instance of renderer.
  /// </summary>
  /// <returns>
  ///   instance
  /// </returns>
  function RendererManager : TGIS_RendererManager ;

  {$IFDEF OXYGENE}
    procedure SelfRegisterRenderers ;
  {$ENDIF}

var
  /// <summary>
  ///   List of custom font files.
  /// </summary>
  CustomFontList : array of String ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Math,
    GisFunctions,
    GisParams,
    GisLayerVector,
    GisHtmlLabel ;
{$ENDIF}

var
  oRendererManager : TGIS_RendererManager ;
  wasSelfRegisterRenderers : Boolean = False ;

function RendererManager
  : TGIS_RendererManager ;
begin
  if not assigned( oRendererManager ) then
    oRendererManager := TGIS_RendererManager.Create ;
  Result := oRendererManager ;
end ;

{$IFDEF OXYGENE}
  procedure SelfRegisterRenderers ;
  begin
    if wasSelfRegisterRenderers then exit ;

    {$IFDEF JAVA}
      wasSelfRegisterRenderers := SelfInvokeClassMethod( 'tatukgis.jdk.Unit_',
                                                         'SelfRegisterRenderer' )  ;
    {$ENDIF}
    {$IFDEF CLR}
      wasSelfRegisterRenderers := SelfInvokeClassMethod( 'Unit_',
                                                         'SelfRegisterRenderer' )  ;
    {$ENDIF}
  end ;
{$ENDIF}

//=============================================================================
// TGIS_RendererContext
//=============================================================================

  constructor TGIS_RendererContext.Create ;
  begin
    ProgressiveHelper := nil ;

    oBaseMap      := nil ;
    bBaseMapOwn   := False ;

    oSelection    := nil ;
    bSelectionOwn := False ;

    oCharts       := nil ;
    bChartsOwn    := False ;

    oLabels       := nil ;
    bLabelsOwn    := False ;
  end ;

  procedure TGIS_RendererContext.doDestroy ;
  begin
    Clear ;
    inherited ;
  end;

  procedure TGIS_RendererContext.Clear ;
  begin
    if bBaseMapOwn   then begin
      FreeObject( oBaseMap   ) ;
      bBaseMapOwn := False ;
    end
    else oBaseMap      := nil ;

    if bSelectionOwn then begin
      FreeObject( oSelection ) ;
      bSelectionOwn := False ;
    end
    else oSelection    := nil ;

    if bChartsOwn    then begin
      FreeObject( oCharts    ) ;
      bChartsOwn := False ;
    end
    else oCharts       := nil ;

    if bLabelsOwn    then begin
      FreeObject( oLabels    ) ;
      bLabelsOwn := false ;
    end
    else oLabels       := nil ;
  end ;

  function TGIS_RendererContext.get_ProgressiveHelper
    : TObject ;
  begin
    Result := oProgressiveHelper ;
  end;

  procedure TGIS_RendererContext.set_ProgressiveHelper(
    const _value : TObject
  ) ;
  begin
    oProgressiveHelper := TGIS_ViewerHelperRun( _value ) ;
  end;

  function TGIS_RendererContext.fget_BaseMapOnDemand
    : Boolean ;
  begin
    Result := ( not assigned( oBaseMap ) ) and bBaseMapOwn ;
  end;

  function TGIS_RendererContext.fget_SelectionOnDemand
    : Boolean ;
  begin
    Result := ( not assigned( oSelection ) ) and bSelectionOwn ;
  end;

  function TGIS_RendererContext.fget_ChartsOnDemand
    : Boolean ;
  begin
    Result := ( not assigned( oCharts ) ) and bChartsOwn ;
  end;

  function TGIS_RendererContext.fget_LabelsOnDemand
    : Boolean ;
  begin
    Result := ( not assigned( oLabels ) ) and bLabelsOwn ;
  end;

  procedure TGIS_RendererContext.DoProgressiveUpdate  ;
  begin
    if assigned( oProgressiveHelper ) then
      oProgressiveHelper.DoSynchronize( False ) ;
  end;

  procedure TGIS_RendererContext.AssignDrawContext(
    const _factory       : TObject ;
    const _nativeContext : TObject ;
    const _sourceContext : TObject
  ) ;
  begin
    oDrawContextFactory := _factory ;
    oNativeDrawContext  := _nativeContext ;
    oSourceDrawContext  := _sourceContext ;
  end;

  procedure TGIS_RendererContext.ClearDrawContext ;
  begin
    oDrawContextFactory := nil ;
    oNativeDrawContext  := nil ;
    oSourceDrawContext  := nil ;
  end;

  procedure TGIS_RendererContext.AssignBaseMap(
    const _bitmap : TObject ;
    const _own    : Boolean
  ) ;
  begin
    if bBaseMapOwn then
      FreeObject( oBaseMap );

    oBaseMap    := _bitmap ;
    bBaseMapOwn := _own ;
  end;

  procedure TGIS_RendererContext.AssignSelection(
    const _bitmap : TObject ;
    const _own    : Boolean
  ) ;
  begin
    if bSelectionOwn then
      FreeObject( oSelection );

    oSelection    := _bitmap ;
    bSelectionOwn := _own ;
  end;

  procedure TGIS_RendererContext.AssignCharts(
    const _bitmap : TObject ;
    const _own    : Boolean
  ) ;
  begin
    if bChartsOwn then
      FreeObject( oCharts );

    oCharts    := _bitmap ;
    bChartsOwn := _own ;
  end;

  procedure TGIS_RendererContext.AssignLabels(
    const _bitmap : TObject ;
    const _own    : Boolean
  ) ;
  begin
    if bLabelsOwn then
      FreeObject( oLabels );

    oLabels    := _bitmap ;
    bLabelsOwn := _own ;
  end;


//=============================================================================
// RenderEvent
//=============================================================================

{$IFDEF OXYGENE}
  {$IFDEF JAVA}
    constructor RenderEvent.Create(
      const _source   : Object ;
      const _renderer : TGIS_RendererAbstract ;
      const _mode     : TGIS_DrawMode
    ) ;
    begin
      inherited Create( _source ) ;
      FRenderer := _renderer ;
      FDrawMode := _mode ;
    end ;
  {$ENDIF}
  {$IFDEF CLR}
    constructor TGIS_RendererEventArgs.Create(
      const _renderer : TGIS_RendererAbstract ;
      const _mode     : TGIS_DrawMode
    ) ;
    begin
      inherited Create ;
      FRenderer := _renderer ;
      FDrawMode := _mode ;
    end ;
  {$ENDIF}
{$ENDIF}

{$IFDEF JAVA}

  method RenderAdapter.BeforeUpdate(
    _evt : RenderEvent
  ) ;
  begin
    // do nothing
  end ;

  method RenderAdapter.Update(
    _evt : RenderEvent
  ) : Boolean ;
  begin
    // do nothing
    exit False ;
  end ;

  method RenderAdapter.AfterUpdate(
    _evt : RenderEvent
  ) ;
  begin
    // do nothing
  end ;

{$ENDIF}

//=============================================================================
// TGIS_RendererAbstract
//=============================================================================

  constructor TGIS_RendererAbstract.Create ;
  begin
    inherited ;
  end;

  function TGIS_RendererAbstract.TwipsToPixels(
    const _size : Integer
  ) : Integer ;
  begin
    Result := TwipsToPixels( _size, RoundS(4096*FPPI/96) ) ;
  end ;

  function TGIS_RendererAbstract.TwipsToPixels(
    const _size     : Integer ;
    const _maxValue : Integer
  ) : Integer ;
  var
    tmp   : Double  ;
  begin
    // 56.692913386  = mm2twips

    if       _size = GIS_RENDER_SIZE then
                             Result := 1
    else if _size < 0 then begin
                             if _size <= -GIS_AUTOSIZE_SIZE_MU then begin
                                // special case for value expressed in map units
                                tmp := 1.0
                                       * ( -_size mod GIS_AUTOSIZE_SIZE_MU )
                                       * Viewer.Zoom ;
                                Result := RoundS( tmp / 100 ) ;
                             end
                             else
                             if _size <= -GIS_AUTOSIZE_SIZE then begin
                                tmp := 1.0
                                       * ( -_size mod GIS_AUTOSIZE_SIZE )
                                       * Viewer.ScaleAsFloat * PPI ;
                                Result := RoundS( tmp /1440 * 56.692913386 ) ;
                             end
                             else
                               Result := -_size ; // minus so real pixels
                             if ( _maxValue > 0 ) and ( Result > _maxValue ) then
                               Result := _maxValue ;
                           end
    else if _size > 0 then begin
                             if _size >= GIS_AUTOSIZE_SIZE_MU then begin
                                // undefined - treat same as GIS_AUTOSIZE_MARKER
                                tmp := 1.0
                                       * ( _size mod GIS_AUTOSIZE_SIZE_MU )
                                       * Viewer.ScaleAsFloat ;
                                Result := RoundS( tmp / 100 ) ;
                             end
                             else
                             if _size >= GIS_AUTOSIZE_SIZE then begin
                                tmp := 1.0 * ( _size mod GIS_AUTOSIZE_SIZE ) *
                                       Viewer.ScaleAsFloat * PPI ;
                                Result := RoundS( tmp /1440 ) ;
                             end
                             else begin
                                tmp := 1.0 * _size * PPI ;
                                if tmp > 1440 then Result := RoundS( tmp /1440 )
                                              else Result := 1 ;
                             end;
                             if ( _maxValue > 0 ) and ( Result > _maxValue ) then
                               Result := _maxValue ;
                           end
    else                   Result := 0 ;

  end ;


  function TGIS_RendererAbstract.TwipsToPoints(
    const _size : Integer
  ) : Integer ;
  var
    tmp   : Double  ;
  begin
    // 20 = twips2points = 1440 / 72
    // 2.834645669291 = mm2point = mm2twips / twips2points = 56.692913386 / 20

    if       _size = GIS_RENDER_SIZE then
                             Result := 8
    else if  _size < 0 then begin
                             if _size <= -GIS_AUTOSIZE_SIZE_MU then begin
                                tmp := 1.0
                                       * ( -_size mod GIS_AUTOSIZE_SIZE_MU )
                                       * Viewer.Zoom ;
                                Result := RoundS( ( tmp / 100 ) *
                                                  ( 72 / PPI )
                                                ) ;
                             end
                             else
                             if _size <= -GIS_AUTOSIZE_SIZE then begin
                                tmp := 1.0
                                       * ( -_size mod GIS_AUTOSIZE_SIZE )
                                       * Viewer.ScaleAsFloat ;
                                Result := RoundS( tmp * 2.834645669291 ) ;
                             end
                             else
                               Result := RoundS( -_size * ( 72 / PPI )
                                               ) ; // minus so real pixels
                             if Result > 2048 then
                               Result := 2048 ;
                           end
    else if _size > 0 then begin
                             if _size >= GIS_AUTOSIZE_SIZE_MU then begin
                                // undefined - treat same as GIS_AUTOSIZE_SIZE
                                tmp := 1.0
                                       * ( _size mod GIS_AUTOSIZE_SIZE_MU )
                                       * Viewer.ScaleAsFloat ;
                                Result := RoundS( tmp / 20 ) ;
                             end
                             else
                             if _size >= GIS_AUTOSIZE_SIZE then begin
                                tmp := 1.0
                                       * ( _size mod GIS_AUTOSIZE_SIZE )
                                       * Viewer.ScaleAsFloat ;
                                Result := RoundS( tmp / 20 ) ;
                             end
                             else begin
                                tmp := 1.0 * _size ;
                                Result := RoundS( tmp / 20 ) ;
                             end;
                             if Result > 2048 then
                               Result := 2048 ;
                           end
    else                   Result := 0 ;
  end ;

  function TGIS_RendererAbstract.PixelsToTwips(
    const _size : Integer
  ) : Integer ;
  begin
    Result := RoundS( 1.0 * Abs( _size ) * 1440 / PPI ) ;
  end ;

  procedure TGIS_RendererAbstract.doDestroy ;
  begin
    ReleaseContext ;

    inherited ;
  end;

  procedure TGIS_RendererAbstract.Setup(
    const _parent    : IGIS_ViewerParent ;
    const _viewer    : IGIS_Viewer ;
    const _shift     : TPoint  ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _ppi       : Integer ;
    const _fontscale : Integer
  ) ;
  begin
    Parent  := _parent  ;
    Viewer  := _viewer  ;

    // keep context untouched
    // Context := nil ; 

    FShift     := _shift  ;
    FWidth     := _width  ;
    FHeight    := _height ;
    FPPI       := _ppi    ;
    FFontScale := _fontscale ;
    FTileRect  := Rect ( 0, 0, FWidth, FHeight ) ;
  end;


  procedure TGIS_RendererAbstract.CreateContext(
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
    Parent  := _parent  ;
    Viewer  := _viewer  ;
    Context := _context ;

    FShift     := _shift  ;
    FWidth     := _width  ;
    FHeight    := _height ;
    FPPI       := _ppi    ;
    FFontScale := _fontscale ;
    FTileRect  := Rect ( 0, 0, FWidth, FHeight ) ;
  end;

  procedure TGIS_RendererAbstract.CreateContext(
    const _parent        : IGIS_ViewerParent ;
    const _viewer        : IGIS_Viewer ;
    const _context       : TGIS_RendererContext ;
    const _shift         : TPoint  ;
    const _width         : Integer ;
    const _height        : Integer ;
    const _ppi           : Integer ;
    const _fontscale     : Integer ;
    const _tilerect      : TRect
  ) ;
  begin
    Parent  := _parent  ;
    Viewer  := _viewer  ;
    Context := _context ;

    FShift     := _shift  ;
    FWidth     := _width  ;
    FHeight    := _height ;
    FPPI       := _ppi    ;
    FFontScale := _fontscale ;
    FTileRect  := _tilerect ;
  end;

  procedure  TGIS_RendererAbstract.CreatePrinterContext(
    const _canvas    : TObject ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _ppi       : Integer ;
    const _fontscale : Integer
  ) ;
  begin
    Parent  := nil  ;
    Viewer  := nil  ;
    Context := nil ;

    FWidth     := _width  ;
    FHeight    := _height ;
    FPPI       := _ppi    ;
    FFontScale := _fontscale ;
  end;

  procedure TGIS_RendererAbstract.RestoreContext ;
  begin
    // do nothing
  end;

  procedure TGIS_RendererAbstract.ReleaseContext ;
  begin
    FreeObject( oMeasureBmp ) ;
    FreeObject( oMeasureRnd ) ;
    FreeObject( oMeasureCtx ) ;
  end;

  function TGIS_RendererAbstract.PrepareBitmapCache(
    const _bmp      : TGIS_Pixels ;
    const _extent   : TGIS_Extent ;
    const _size     : TPoint      ;
    const _serial   : Integer     ;
    const _format   : TGIS_BitmapFormat ;
    const _order    : TGIS_BitmapLinesOrder
  ) : TGIS_RendererAbstractCache ;
  begin
    // for safe inheritance only
    Result := nil ;
  end;

  procedure TGIS_RendererAbstract.RenderBitmapCache(
    const _handle   : TObject ;
    const _cache    : TGIS_RendererAbstractCache ;
    const _dst      : TRect
  ) ;
  begin
    // for safe inheritance only
  end;

  procedure TGIS_RendererAbstract.PrepareDraw ;
  begin
    if assigned( Viewer ) then begin
      FExtentX := - Viewer.Extent.XMin - Viewer.Viewport.X ;
      FExtentY :=   Viewer.Extent.YMax - Viewer.Viewport.Y ;
      FZoom := Viewer.Zoom ;
      FTiled := not GisIsNoWorld( Viewer.TemporaryVisibleExtent ) ;
      if FTiled then
        FCanvasExtent := _TGIS_Extent( Viewer.TemporaryVisibleExtent )
      else
        FCanvasExtent := _TGIS_Extent( Viewer.VisibleExtent ) ;
    end;
  end ;

  procedure TGIS_RendererAbstract.BeforeDraw ;
  begin
    PrepareDraw ;
  end ;

  procedure TGIS_RendererAbstract.OptimizeBitmapCache;
  begin
    // just for safe inheritance
  end;

  function TGIS_RendererAbstract.CanvasTextEm(
    const _em    : Double
  ) : Integer ;
  begin
    Result := RoundS( _em * PPI / 72 * FontScale / 100 * CanvasFont.Size / 6 ) ;
  end;

  function TGIS_RendererAbstract.fget_ViewRect : TRect ;
  begin
    // RectEx
    Result := Rect( FShift.X, FShift.Y, FShift.X+FWidth-1, FShift.Y+FHeight-1 ) ;
  end ;

  function TGIS_RendererAbstract.MeasureShieldTexture(
    const _shp        : TObject ;
    var   _size       : TPoint
  ) : TObject;
  var
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr          : IGIS_Viewer ;
    shp          : TGIS_Shape ;
    max_size     : TPoint ;
    label_params : TGIS_ParamsLabel ;
    label_text   : String ;
    xgap, ygap   : Integer ;
    out_w        : Integer ;
    lbl          : TGIS_HtmlLabel ;
  begin
    Result := nil ;
    _size.X := 0 ;
    _size.Y := 0 ;

    vwr := TGIS_Shape( _shp ).Layer.Viewer.Ref ;
    shp := TGIS_Shape( _shp ) ;

    label_params := shp.Params.Labels ;
    if not label_params.Visible then exit ;

    max_size := Point( vwr.TwipsToPixels( label_params.Width  ),
                       vwr.TwipsToPixels( label_params.Height )
                     ) ;

    if max_size.X <= 2 then exit ;
    if max_size.Y <= 2 then exit ;

    // prepare text for label
    label_text := shp.GetLabel ;

    if IsStringEmpty( label_text ) then exit ;

    if not label_params.Duplicates and
       vwr.LabelsReg.IsDuplicated( label_text )
    then
      exit ;


    if not assigned( oMeasureBmp ) then begin
      oMeasureBmp := TGIS_Bitmap.Create(
                        1,
                        1
                      ) ;
      {$IFDEF CLR}
        oMeasureRnd := RendererManager.CreateInstance('TGIS_RendererWinForms') ;
      {$ELSE}
        oMeasureRnd := self.CreateInstance ;
      {$ENDIF}

      oMeasureCtx := TGIS_RendererContext.Create ;
      oMeasureCtx.AssignBaseMap( oMeasureBmp, False ) ;

      oMeasureRnd.CreateContext( nil, vwr,
                                 oMeasureCtx, Point(0, 0),
                                 oMeasureBmp.Width, oMeasureBmp.Height,
                                 vwr.PPI, vwr.FontScale
                               ) ;
    end;


    oMeasureRnd.CanvasFont.Name  := label_params.FontName  ;
    oMeasureRnd.CanvasFont.Size  := vwr.TwipsToPoints( label_params.FontSize ) ;
    oMeasureRnd.CanvasFont.Style := label_params.FontStyle ;
    if label_params.FontColor.ARGB = TGIS_Color.RenderColor.ARGB then
      oMeasureRnd.CanvasFont.Color := TGIS_Color.None
    else
      oMeasureRnd.CanvasFont.Color := label_params.FontColor ;

    // measure label
    lbl := TGIS_HtmlLabel.Create( oMeasureRnd,
                                  label_text,
                                  label_params.Alignment,
                                  max_size.X, max_size.Y
                                ) ;

    _size.X := lbl.BoundingBox.Right  - lbl.BoundingBox.Left + 1 ;
    _size.Y := lbl.BoundingBox.Bottom - lbl.BoundingBox.Top  + 1 ;


    if _size.X > vwr.TwipsToPixels( label_params.Width  ) then
      _size.X := vwr.TwipsToPixels( label_params.Width  ) ;
    if _size.Y > vwr.TwipsToPixels( label_params.Height ) then
      _size.Y := vwr.TwipsToPixels( label_params.Height ) ;

    // draw label background
    if ( label_params.OutlineStyle = TGIS_PenStyle.Clear ) or
       ( label_params.OutlineWidth = 0 )
    then
      out_w := 0
    else
      out_w := Max( 1, vwr.TwipsToPixels( label_params.OutlineWidth ) ) ;

     if ( label_params.OutlineStyle <> TGIS_PenStyle.Clear   ) or
       ( label_params.Pattern      <> TGIS_BrushStyle.Clear )
    then begin
      xgap := oMeasureRnd.CanvasTextEm( 2 ) ;
      ygap := oMeasureRnd.CanvasTextEm( 1 ) ;

      _size    := Point( _size.X + 2*( 1 + out_w + xgap ),
                         _size.Y + 2*( 1 + out_w + ygap )
                       ) ;
    end ;

    Result := lbl ;
  end;

  function TGIS_RendererAbstract.RenderShieldTexture(
    const _shp        : TObject ;
    const _force_pow2 : Boolean ;
    const _size       : TPoint  ;
    const _layout     : TObject
  ) : TGIS_Bitmap;
  var
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr          : IGIS_Viewer ;
    shp          : TGIS_Shape ;
    label_params : TGIS_ParamsLabel ;
    rct_lbl      : TRect ;
    rct_lbl2     : TRect ;
    xgap, ygap   : Integer ;
    out_w        : Integer ;
    bmp          : TGIS_Bitmap ;
    ctx          : TGIS_RendererContext ;
    rnd          : TGIS_RendererAbstract ;
    lbl          : TGIS_HtmlLabel ;


    function sizepow2( const _sz : Integer ) : Integer ;
    begin
      if _force_pow2 then begin
        if      _sz > 1024 then Result := 2048
        else if _sz >  512 then Result := 1024
        else if _sz >  256 then Result :=  512
        else if _sz >  128 then Result :=  256
        else if _sz >   64 then Result :=  128
        else if _sz >   32 then Result :=   64
        else if _sz >   16 then Result :=   32
        else if _sz >    8 then Result :=   16
        else                    Result :=    8 ;
      end
      else begin
        Result := _sz
      end;
    end;

  begin
    Result := nil ;

    if not assigned( _layout ) then
      exit ;

    vwr := TGIS_Shape( _shp ).Layer.Viewer.Ref ;
    shp := TGIS_Shape( _shp ) ;

    label_params := shp.Params.Labels ;

    ctx := TGIS_RendererContext.Create ;
    try
      {$IFDEF CLR}
        rnd := RendererManager.CreateInstance('TGIS_RendererWinForms') ;
      {$ELSE}
        rnd := self.CreateInstance ;
      {$ENDIF}

      bmp := TGIS_Bitmap.Create(
               sizepow2( _size.X ),
               sizepow2( _size.Y )
             ) ;
      ctx.AssignBaseMap( bmp, False ) ;

      rnd.CreateContext( nil, vwr,
                         ctx, Point(0, 0),
                         bmp.Width, bmp.Height,
                         vwr.PPI, vwr.FontScale
                       ) ;

      rnd.PrepareDraw ;
      try
        rnd.CanvasFont.Name  := label_params.FontName  ;
        rnd.CanvasFont.Size  := vwr.TwipsToPoints( label_params.FontSize ) ;
        rnd.CanvasFont.Style := label_params.FontStyle ;
        if label_params.FontColor.ARGB = TGIS_Color.RenderColor.ARGB then
          rnd.CanvasFont.Color := TGIS_Color.None
        else
          rnd.CanvasFont.Color := label_params.FontColor ;

        // draw label background
        if ( label_params.OutlineStyle = TGIS_PenStyle.Clear ) or
           ( label_params.OutlineWidth = 0 )
        then
          out_w := 0
        else
          out_w := Max( 1, vwr.TwipsToPixels( label_params.OutlineWidth ) ) ;


        if ( label_params.OutlineStyle <> TGIS_PenStyle.Clear   ) or
           ( label_params.Pattern      <> TGIS_BrushStyle.Clear )
        then begin
          xgap := rnd.CanvasTextEm( 2 ) ;
          ygap := rnd.CanvasTextEm( 1 ) ;

          rct_lbl  := Rect(  1 + out_w + xgap,
                             1 + out_w + ygap,
                             _size.X + 1 + out_w + xgap,
                             _size.Y + 1 + out_w + ygap
                          ) ;
        end
        else begin
          rct_lbl  := Rect( 0, 0, _size.X -1, _size.Y -1 ) ;
        end ;

        rct_lbl2 := Rect( 1 + out_w div 2 ,
                          1 + out_w div 2,
                          _size.X - ( 1 + out_w div 2 ),
                          _size.Y - ( 1 + out_w div 2 )
                        ) ;

        // draw background
        rnd.CanvasBrush.Color := label_params.Color ;
        rnd.CanvasBrush.Style := label_params.Pattern ;

        rnd.CanvasPen.Color := label_params.OutlineColor ;
        rnd.CanvasPen.Style := label_params.OutlineStyle ;
        rnd.CanvasPen.Width := out_w ;
        rnd.CanvasDrawRectangle( rct_lbl2 )  ;

        lbl := TGIS_HtmlLabel.Create( rnd,
                                          shp.GetLabel,
                                          label_params.Alignment,
                                          _size.X, _size.Y
                                         ) ;


        // draw label
        if   ( label_params.Pattern = TGIS_BrushStyle.Clear )
             and
             TGIS_Bitmap.IsNilOrEmpty( label_params.Bitmap )
             and
             ( label_params.Color.ARGB <> label_params.FontColor.ARGB )
        then begin
          lbl.Draw( rct_lbl,
                    0,
                    Point( 0, 0 ),
                    Max( 1, PPI div 96 ),
                    label_params.Color
                  ) ;
        end
        else begin
          lbl.Draw( rct_lbl,
                    0,
                    Point( 0, 0 ),
                    0,
                    label_params.Color
                  ) ;
        end ;
      finally
        rnd.AfterDraw ;
      end ;
    finally
      FreeObjectNotNil( _layout ) ;
      FreeObject( lbl ) ;
      FreeObject( rnd ) ;
      FreeObject( ctx ) ;
    end;

    Result := bmp ;
  end;

  function TGIS_RendererAbstract.RenderShieldTexture(
    const _shp        : TObject ;
    const _force_pow2 : Boolean ;
    var   _size       : TPoint
  ) : TGIS_Bitmap;
  var
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr          : IGIS_Viewer ;
    shp          : TGIS_Shape ;
    label_params : TGIS_ParamsLabel ;
    label_text   : String ;
    max_size     : TPoint ;
    lbl_size     : TPoint ;
    rct_lbl      : TRect ;
    rct_lbl2     : TRect ;
    xgap, ygap   : Integer ;
    out_w        : Integer ;
    bmp          : TGIS_Bitmap ;
    ctx          : TGIS_RendererContext ;
    rnd          : TGIS_RendererAbstract ;
    lbl          : TGIS_HtmlLabel ;

    function sizepow2( const _sz : Integer ) : Integer ;
    begin
      if _force_pow2 then begin
        if      _sz > 1024 then Result := 2048
        else if _sz >  512 then Result := 1024
        else if _sz >  256 then Result :=  512
        else if _sz >  128 then Result :=  256
        else if _sz >   64 then Result :=  128
        else if _sz >   32 then Result :=   64
        else if _sz >   16 then Result :=   32
        else if _sz >    8 then Result :=   16
        else                    Result :=    8 ;
      end
      else begin
        Result := _sz
      end;
    end;

  begin
    _size.X := 0 ;
    _size.Y := 0 ;
    Result := nil ;

    bmp  := nil ;

    vwr := TGIS_Shape( _shp ).Layer.Viewer.Ref ;
    shp := TGIS_Shape( _shp ) ;

    label_params := shp.Params.Labels ;
    if not label_params.Visible then exit ;

    max_size := Point( vwr.TwipsToPixels( label_params.Width  ),
                       vwr.TwipsToPixels( label_params.Height )
                     ) ;

    if max_size.X <= 2 then exit ;
    if max_size.Y <= 2 then exit ;

    // prepare text for label
    label_text := shp.GetLabel ;

    if IsStringEmpty( label_text ) then exit ;

    if not label_params.Duplicates and
       vwr.LabelsReg.IsDuplicated( label_text )
    then
      exit ;


    lbl := nil ;
    rnd := nil ;
    ctx := nil ;

    ctx := TGIS_RendererContext.Create ;
    try
      {$IFDEF CLR}
        rnd := RendererManager.CreateInstance('TGIS_RendererWinForms') ;
      {$ELSE}
        rnd := self.CreateInstance ;
      {$ENDIF}

      bmp := TGIS_Bitmap.Create(
               sizepow2( max_size.X ),
               sizepow2( max_size.Y )
             ) ;
      ctx.AssignBaseMap( bmp, False ) ;

      rnd.CreateContext( nil, vwr,
                         ctx, Point(0, 0),
                         bmp.Width, bmp.Height,
                         vwr.PPI, vwr.FontScale
                       ) ;

      rnd.PrepareDraw ;
      try
        rnd.CanvasFont.Name  := label_params.FontName  ;
        rnd.CanvasFont.Size  := vwr.TwipsToPoints( label_params.FontSize ) ;
        rnd.CanvasFont.Style := label_params.FontStyle ;
        if label_params.FontColor.ARGB = TGIS_Color.RenderColor.ARGB then
          rnd.CanvasFont.Color := TGIS_Color.None
        else
          rnd.CanvasFont.Color := label_params.FontColor ;

        // measure label
        lbl := TGIS_HtmlLabel.Create( rnd,
                                      label_text,
                                      label_params.Alignment,
                                      max_size.X, max_size.Y
                                    ) ;

        lbl_size.X := lbl.BoundingBox.Right  - lbl.BoundingBox.Left + 1 ;
        lbl_size.Y := lbl.BoundingBox.Bottom - lbl.BoundingBox.Top  + 1 ;

        if lbl_size.X > vwr.TwipsToPixels( label_params.Width  ) then
          lbl_size.X := vwr.TwipsToPixels( label_params.Width  ) ;
        if lbl_size.Y > vwr.TwipsToPixels( label_params.Height ) then
          lbl_size.Y := vwr.TwipsToPixels( label_params.Height ) ;


        // draw label background
        if ( label_params.OutlineStyle = TGIS_PenStyle.Clear ) or
           ( label_params.OutlineWidth = 0 )
        then
          out_w := 0
        else
          out_w := Max( 1, vwr.TwipsToPixels( label_params.OutlineWidth ) ) ;


        if ( label_params.OutlineStyle <> TGIS_PenStyle.Clear   ) or
           ( label_params.Pattern      <> TGIS_BrushStyle.Clear )
        then begin
          xgap := rnd.CanvasTextEm( 2 ) ;
          ygap := rnd.CanvasTextEm( 1 ) ;

          _size    := Point( lbl_size.X + 2*( 1 + out_w + xgap ),
                             lbl_size.Y + 2*( 1 + out_w + ygap )
                           ) ;
          rct_lbl  := Rect(  1 + out_w + xgap,
                             1 + out_w + ygap,
                             lbl_size.X + 1 + out_w + xgap,
                             lbl_size.Y + 1 + out_w + ygap
                          ) ;
        end
        else begin
          _size    := Point( lbl_size.X, lbl_size.Y ) ;
          rct_lbl  := Rect( 0, 0, lbl_size.X -1, lbl_size.Y -1 ) ;
        end ;

        rct_lbl2 := Rect( 1 + out_w div 2 ,
                          1 + out_w div 2,
                          _size.X - ( 1 + out_w div 2 ),
                          _size.Y - ( 1 + out_w div 2 )
                        ) ;

        // draw background
        rnd.CanvasBrush.Color := label_params.Color ;
        rnd.CanvasBrush.Style := label_params.Pattern ;

        rnd.CanvasPen.Color := label_params.OutlineColor ;
        rnd.CanvasPen.Style := label_params.OutlineStyle ;
        rnd.CanvasPen.Width := out_w ;
        rnd.CanvasDrawRectangle( rct_lbl2 )  ;

        // draw label
        if   ( label_params.Pattern = TGIS_BrushStyle.Clear )
             and
             TGIS_Bitmap.IsNilOrEmpty( label_params.Bitmap )
             and
             ( label_params.Color.ARGB <> label_params.FontColor.ARGB )
        then begin
          lbl.Draw( rct_lbl, 0, Point( 0, 0 ),
                   Max( 1, PPI div 96 ), label_params.Color ) ;
        end
        else begin
          lbl.Draw( rct_lbl, 0, Point( 0, 0 ),
                    0, label_params.Color ) ;
        end ;
      finally
        rnd.AfterDraw ;
      end ;
    finally
      FreeObject( lbl ) ;
      FreeObject( rnd ) ;
      FreeObject( ctx ) ;
    end;

    Result := bmp ;
  end;

  function TGIS_RendererAbstract.getOffsetPoint(
    const _params : TObject
  ) : TPoint ;
  var
    px, py : Integer ;
    prm : TGIS_ParamsFeature ;
  begin
    prm := _params as TGIS_ParamsFeature ;
    px  := TwipsToPixels( prm.OffsetX ) ;
    py  := TwipsToPixels( prm.OffsetY ) ;

    if (px <> GIS_RENDER_SIZE) and (py <> GIS_RENDER_SIZE) then begin
      case prm.OffsetPosition of
        TGIS_OffsetPosition.UpLeft     :
          begin
            px := -px ;
            py := -py ;
          end;
        TGIS_OffsetPosition.UpRight    :
          begin
            py := -py ;
          end;
        TGIS_OffsetPosition.DownLeft   :
          begin
            px := -px ;
          end;
        TGIS_OffsetPosition.DownRight  :
          begin
          end;
      end;
    end ;
    Result := Point( px, py ) ;
  end;

{$IFNDEF JAVA}
  function TGIS_RendererAbstract.ViewerCreateWndPaint(
    const _width  : Integer ;
    const _height : Integer ;
    const _canvas : TObject
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_RendererAbstract.ViewerFreeWndPaint(
    var _paint : TObject
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerFlushToWndPaint(
    const _localCanvas : TObject ;
    const _canvas      : TObject
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerDrawBackground(
    const _canvas : TObject ;
    const _width  : Integer ;
    const _height : Integer ;
    const _color  : TGIS_Color
  ) ;
  begin

  end ;

  function TGIS_RendererAbstract.ViewerPrepareCache(
    const _bitmap : TGIS_Bitmap
  ) : TGIS_Bitmap ;
  begin
    Result := nil ;
  end ;

  function TGIS_RendererAbstract.ViewerCreateTemporaryPaint(
    const _width   : Integer ;
    const _height  : Integer ;
    const _context : TObject
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  function TGIS_RendererAbstract.ViewerCreateTemporaryPaintEx(
    const _width   : Integer ;
    const _height  : Integer ;
    const _context : TObject
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  function TGIS_RendererAbstract.ViewerCreateTemporaryPaint(
    const _context : TGIS_Bitmap
  ) : TObject ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_RendererAbstract.ViewerFreeTemporaryPaint(
    var   _paint : TObject
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerFreeTemporaryPaint(
    var   _paint  : TObject ;
    const _bitmap : TGIS_Bitmap
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerClearTemporaryPaint(
    const _paint : TObject
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerFlushTemporaryPaint(
    const _canvas : TObject ;
    const _width  : Integer ;
    const _height : Integer ;
    const _paint  : TObject ;
    const _rect   : TRect
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerFlushTemporaryPaint(
    const _canvas    : TObject ;
    const _width     : Integer ;
    const _height    : Integer ;
    const _paint     : TObject ;
    const _fullcache : TGIS_Bitmap ;
    const _rect      : TRect
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerBeginDrawOnTemporaryPaint(
    const _paint : TObject ;
    const _color : TGIS_Color
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerEndDrawOnTemporaryPaint(
    const _paint : TObject
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerStretchBitmapFast(
    const _bitmap : TObject ;
    const _paint  : TObject ;
    const _rect   : TRect
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerStretchBitmapFast(
    const _bitmap       : TObject ;
    const _paint        : TObject ;
    const _rect         : TRect   ;
    const _transparency : Integer
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerBlendBitmaps(
    const _bitmap       : TObject ;
    const _paint        : TObject ;
    const _transparency : Integer ;
    const _merge_alpha  : Boolean
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerBlendLabelBitmaps(
    const _bitmap      : TObject ;
    const _paint       : TObject ;
    const _merge_alpha : Boolean
  ) ;
  begin

  end ;

  function TGIS_RendererAbstract.ViewerCreateFullCache(
    const _width  : Integer ;
    const _height : Integer ;
    const _paint  : TObject ;
    const _addObj : TObject
  ) : TGIS_Bitmap ;
  begin
    Result := nil ;
  end ;

  procedure TGIS_RendererAbstract.ViewerFlushToFullCache(
    var   _paint     : TObject ;
    const _fullcache : TGIS_Bitmap
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerDrawCache(
    const _cache : TObject ;
    const _paint : TObject ;
    const _rect  : TRect
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerDrawCache(
    const _cache        : TObject ;
    const _paint        : TObject ;
    const _rect         : TRect   ;
    const _transparency : Integer
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerDrawProgressBitmap(
    const _bitmap : TObject ;
    const _paint  : TObject ;
    const _rect   : TRect
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerDrawZoomingRect(
    const _canvas : TObject ;
    const _x      : Integer ;
    const _y      : Integer ;
    const _width  : Integer ;
    const _height : Integer ;
    const _color  : TGIS_Color
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ViewerDrawDraggingTrack(
    const _canvas : TObject ;
    const _x1     : Integer ;
    const _y1     : Integer ;
    const _x2     : Integer ;
    const _y2     : Integer ;
    const _color  : TGIS_Color
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ControlDrawNonTransparent(
    const _context : TObject ;
    const _bitmap  : TGIS_Bitmap ;
    const _x       : Integer ;
    const _y       : Integer
  ) ;
  begin

  end ;

  procedure TGIS_RendererAbstract.ControlDrawTransparent(
    const _context : TObject ;
    const _bitmap  : TObject ;
    const _x       : Integer ;
    const _y       : Integer
  ) ;
  begin

  end ;
{$ENDIF}

  constructor TGIS_RendererAbstractCache.Create(
    const _extent      : TGIS_Extent ;
    const _size        : TPoint      ;
    const _serial      : Integer
  ) ;
  begin
    FExtent := _extent ;
    FSize   := _size   ;
    FSerial := _serial ;
  end;

  procedure TGIS_RendererAbstractCache.ResetSerial ;
  begin
    FSerial := 0 ;
  end;

  constructor TGIS_RendererManager.Create ;
  begin
    oItems := TDictionary<String,TGIS_RendererAbstractClass>.Create(
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
    oNames := TGIS_StringList.Create ;
  end;

  procedure TGIS_RendererManager.doDestroy ;
  begin
    FreeObject( oItems )  ;
    FreeObject( oNames )  ;
  end;

  function TGIS_RendererManager.fget_Names
    : TGIS_StringList ;
  {$IFDEF DCC}
    var
      elm : TPair< String, TGIS_RendererAbstractClass > ;
  {$ENDIF}
  begin
    if oNames.Count < 1 then begin
      try
        for elm in oItems do
          oNames.Add( elm.Key ) ;
      finally
        oNames.Sorted := True ;
      end ;
    end ;
    Result := oNames ;
  end ;

  procedure TGIS_RendererManager.Add(
    const _name : String ;
    const _type : TGIS_RendererAbstractClass
  ) ;
  var
    tmp : TGIS_RendererAbstractClass ;
  begin
    if oItems.TryGetValue( _name, tmp ) then begin
      // should never happens - only in a Delphi design mode
      // upon reinstalling packages
      oItems.Remove( _name  );
    end;

    oItems.Add( _name, _type );
    oNames.Clear ;
  end;

  procedure TGIS_RendererManager.Remove(
    const _name : String
  ) ;
  var
    tp : TGIS_RendererAbstractClass ;
  begin
    if not oItems.TryGetValue( _name, tp ) then exit ;
    oItems.Remove( _name  );
    oNames.Clear ;
  end ;

  function TGIS_RendererManager.Get(
    const _name : String
  ) : TGIS_RendererAbstractClass ;
  begin
    oItems.TryGetValue( _name, Result ) ;
  end ;

  function TGIS_RendererManager.CreateInstance(
    const _name : String
  ) : TGIS_RendererAbstract ;
  var
    tp : TGIS_RendererAbstractClass ;
  begin
    Result := nil ;
    if not oItems.TryGetValue( _name, tp ) then exit ;

    {$IFDEF DCC}
      Result := tp.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := TGIS_RendererAbstract( Activator.CreateInstance( tp.ActualType ) ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := TGIS_RendererAbstract( tp.ActualType.getConstructor([]).newInstance() ) ;
    {$ENDIF}
  end;

  function TGIS_RendererManager.IsRegistered(
    const _name : String
  ) : Boolean ;
  var
    tp : TGIS_RendererAbstractClass ;
  begin
    result := oItems.TryGetValue( _name, tp ) ;
  end ;

  procedure RegisterRenderer(
    const _name  : String ;
    const _class : TGIS_RendererAbstractClass
  ) ;
  begin
    RendererManager.Add( _name, _class ) ;
  end ;

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    //
  finalization
    FreeObject( oRendererManager ) ;
{$ENDIF}

{==================================== END =====================================}
end.

