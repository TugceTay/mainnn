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
  Printers unit of TatukGIS FMX.

  In this unit two printers were defined:
   * TGIS_Printer - used for normal printing
   * TGIS_PrinterPreview - used for print preview

  The reason why we implement our own TGIS_Printer class instead of using
  TPrinter is fairly easy: we can not subclass TPrinter. So a new TGIS_Printer
  class, which closely mimics TPrinter, was created.
}

unit FMX.GisPrintManager ;
{$HPPEMIT '#pragma link "FMX.GisPrintManager"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Types,
  System.UIConsts,
  System.UITypes,
  FMX.Printer,
  FMX.Forms,
  FMX.Graphics,

  GisInterfaces,
  GisTypes,
  GisTypesUI,
  GisFunctions,
  GisTemplatePrint,
  GisRendererAbstract,
  GisPrintManagerAbstract,
  FMX.GisPrinters ;

type

  /// <summary>
  ///   Event for OnBeforePrint and OnAfterPrint.
  /// </summary>
  /// <param name="_sender">
  ///   who rises the event
  /// </param>
  /// <param name="_printer">
  ///   printer object on which printing will occur
  /// </param>
  TGIS_PrintEvent = procedure(
    _sender   : TObject  ;
    _printer  : TPrinter
  ) of object ;

  TGIS_PrintManager = class ;

  /// <summary>
  ///   Event for OnPrintPage.
  /// </summary>
  /// <param name="_sender">
  ///   print manager object
  /// </param>
  /// <param name="_printmanager">
  ///   print manager object on which printing will occur
  /// </param>
  /// <param name="_lastpage">
  ///   set to True if it is the last page
  /// </param>
  TGIS_PrintPageEvent =  procedure(
        _sender       : TObject  ;
        _printmanager : TGIS_PrintManager ;
    var _lastpage     : Boolean
  ) of object ;

  /// <summary>
  ///   Event for OnBeforePrintMap and OnAfterPrintMap.
  /// </summary>
  /// <param name="_sender">
  ///   print manager object
  /// </param>
  /// <param name="_viewer">
  ///   map to print
  /// </param>
  /// <param name="_rect">
  ///   rectangle to be printed (area of print on paper)
  /// </param>
  /// <param name="_printer">
  ///   printer object on which printing will occur
  /// </param>
  TGIS_PrintMapEvent =  procedure(
    _sender  : TObject     ;
    _viewer  : IGIS_Viewer ;
    _rect    : TRect       ;
    _printer : TGIS_Printer
  ) of object ;

  /// <summary>
  ///   Print manager used for FMX;
  /// </summary>
  TGIS_PrintManager = class ( TGIS_PrintManagerAbstract, IGIS_ViewerParent )

    private
      oViewer   : IGIS_Viewer ;
      oViewerScale : Double ;
      oRenderer : TGIS_RendererAbstract ;
      oPrinter  : TGIS_Printer ;
      iDpi      : Integer ;

    private
      inRect     : Boolean ;
      rectSize   : TPoint  ;
      onPrintPage : Boolean ;

    private // properties - events

      /// <summary>
      ///   OnBeforePrint event. Will be fired on TGIS_ViewerWnd.Print.
      /// </summary>
      FOnBeforePrint : TGIS_PrintEvent ;

      /// <summary>
      ///   OnAfterPrint event. Will be fired upon completion of TGIS_ViewerWnd.Print.
      /// </summary>
      FOnAfterPrint  : TGIS_PrintEvent ;

      /// <summary>
      ///   OnPrintPage. Will be fired on TGIS_ViewerWnd.Print.
      /// </summary>
      FOnPrintPage   : TGIS_PrintPageEvent ;

      /// <summary>
      ///   BeforePrintMapEvent. Will be fired before printing a map.
      /// </summary>
      FOnBeforePrintMap : TGIS_PrintMapEvent ;

      /// <summary>
      ///   AfterPrintMapEvent. Will be fired after printing a map.
      /// </summary>
      FOnAfterPrintMap  : TGIS_PrintMapEvent ;

    private
      procedure printInternal          ;
      procedure print3DMapInRectangle  ( const _rect   : TRect ;
                                         const _viewer : IGIS_Viewer
                                       ) ;
      procedure printMapInRectangle    ( const _rect    : TRect ;
                                         const _viewer  : IGIS_Viewer ;
                                         const _extent  : TGIS_Extent ;
                                         const _background
                                                        : Boolean ;
                                         var   _scale   : Double ;
                                         var   _rextent : TGIS_Extent ;
                                               _print   : Boolean
                                       ) ;

    protected

      /// <inheritdoc/>
      procedure draw_box        ( const _rect       : TRect       ;
                                  const _color      : TGIS_Color  ;
                                  const _framecolor : TGIS_Color ;
                                  const _framewidth : Integer
                                ) ; override;

      /// <inheritdoc/>
      procedure draw_map        ( const _rect    : TRect       ;
                                  const _viewer  : IGIS_Viewer ;
                                  const _extent  : TGIS_Extent ;
                                  const _background
                                                 : Boolean     ;
                                  var   _scale   : Double      ;
                                  var   _rextent : TGIS_Extent ;
                                        _print   : Boolean
                                ) ; override;

      /// <inheritdoc/>
      procedure draw_control    ( const _rect    : TRect       ;
                                  const _control : IGIS_PrintableControl ;
                                  const _scale   : Double
                                ) ; override;

      /// <inheritdoc/>
      procedure draw_legend     ( const _rect        : TRect  ;
                                  const _control     : IGIS_PrintableControl ;
                                  const _scale       : Double ;
                                  const _compactView : String ;
                                  const _iconStyle   : String ;
                                  const _reverseOrder: String ;
                                  const _font        : String ;
                                  const _fontSize    : String ;
                                  const _fontColor   : String
                                ) ; override ;

      /// <inheritdoc/>
      procedure draw_scale      ( const _rect        : TRect  ;
                                  const _control     : IGIS_PrintableControl ;
                                  const _scale       : Double ;
                                  const _dividers    : String ;
                                  const _dividerColor1 : String ;
                                  const _dividerColor2 : String ;
                                  const _font        : String ;
                                  const _fontSize    : String ;
                                  const _fontColor   : String
                                ) ; override ;

      /// <inheritdoc/>
      procedure draw_northArrow ( const _rect        : TRect  ;
                                  const _control     : IGIS_PrintableControl ;
                                  const _scale       : Double ;
                                  const _style       : String ;
                                  const _color1      : String ;
                                  const _color2      : String ;
                                  const _path        : String
                                ) ; override ;

      /// <inheritdoc/>
      procedure draw_graphic    ( const _rect    : TRect       ;
                                  const _graphic : TGIS_TemplateGraphic ;
                                  const _path    : String
                                ) ; override ;

      /// <inheritdoc/>
      procedure draw_text       ( const _rect    : TRect       ;
                                  const _text    : String      ;
                                  const _name    : String      ;
                                  const _style   : TGIS_FontStyles ;
                                  const _size    : Integer     ;
                                  const _color   : TGIS_Color  ;
                                  const _align   : TGIS_LabelAlignment ;
                                  const _bgColor : TGIS_Color  ;
                                  const _bgWidth : Integer
                                ) ; override;

      /// <inheritdoc/>
      procedure draw_frame      ( const _rect   : TRect       ;
                                  const _color  : TGIS_Color  ;
                                  const _width  : Integer
                                ) ; override;

    public // IGIS_ViewerParent

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlClose           ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlDrawTexture     (       _bmp      : TObject     ;
                                         const _extent   : TGIS_Extent ;
                                         const _ppi      : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlDrawTexture     (       _bmp      : TObject     ;
                                         const _layer    : TGIS_LayerAbstract ;
                                         const _extent   : TGIS_Extent ;
                                         const _ppi      : Integer
                                       ) ; overload ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlRenderer        : TObject ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlFlash           ( const _times    : Integer ;
                                         const _delay    : Integer
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlSystemPPI       : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlPPI             : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlCanvasScale     : Single ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlCanvasHeight    : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlCanvasWidth     : Integer ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlRepaint         ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlProcessMessages ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function ControlUpdateSynchronize( const _interrupt : Boolean
                                       ) : Boolean ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateWholeMap  ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateProgressive  ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateTopmost   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateBasemap   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateSelection ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlUpdateEditor    ( const _final    : Boolean
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlHourglassShow   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlHourglassHide   ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  ControlHourglassShake  : Boolean ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlSet3DMode       ( const _mode     : TGIS_Viewer3DMode
                                       ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlRaiseEditorChangeEvent(
                                               _sender   : TObject
                                       ) ;
      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlAutoCenterViewport      ( const _dx, _dy : Double ) ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      procedure ControlExtentChanged ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  SetViewer ( const _viewer : TObject ) : TObject ;

      /// <inheritdoc from="IGIS_ViewerParent"/>
      function  GetViewer : TObject ;

    protected
      /// <summary>
      ///   Perform component cleanup.
      /// </summary>
      procedure   doDestroy      ; override;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create         ;

    public

      /// <inheritdoc/>
      procedure   Print        ( const _viewer  : IGIS_Viewer
                               ) ; override;


      /// <inheritdoc/>
      procedure   Print        ( const _viewer   : IGIS_Viewer ;
                                 const _printer  : TObject
                               ) ; override;

      /// <inheritdoc/>
      procedure   Print        ( const _viewer   : IGIS_Viewer ;
                                 const _printer  : TObject     ;
                                 var   _scale    : Double
                               ) ; override;

      /// <inheritdoc/>
      procedure   GetDrawingParams
                               ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect   ;
                                 var   _scale   : Double
                               ) ; override ;

      /// <inheritdoc/>
      procedure   DrawMap      ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect   ;
                                 var   _scale   : Double
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawMap      ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect   ;
                                 var   _scale   : Double  ;
                                 var   _rextent : TGIS_Extent
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawControl  ( const _control : IGIS_PrintableControl ;
                                 const _rect    : TRect
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawControl  ( const _control : IGIS_PrintableControl ;
                                 const _rect    : TRect ;
                                 const _scale   : Double
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawGraphic  ( const _path    : String ;
                                 const _rect    : TRect
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawGraphic  ( const _graphic : TGIS_TemplateGraphic ;
                                 const _rect    : TRect
                               ) ; override;
    public

      /// <summary>
      ///   Printer object.
      /// </summary>
      property Printer         : TGIS_Printer   read  oPrinter ;

    published // events

      /// <event/>
      /// <summary>
      ///   BeforePrint event. Will be fired on TGIS_ViewerWnd.Print.
      /// </summary>
      property BeforePrintEvent  : TGIS_PrintEvent
                                                read  FOnBeforePrint
                                                write FOnBeforePrint ;

      /// <event/>
      /// <summary>
      ///   AfterPrint event. Will be fired upon completion of TGIS_ViewerWnd.Print.
      /// </summary>
      property AfterPrintEvent   : TGIS_PrintEvent
                                                read FOnAfterPrint
                                                write FOnAfterPrint ;

      /// <event/>
      /// <summary>
      ///   PrintPage event. Will be fired on TGIS_ViewerWnd.Print. Override to add
      ///   custom printing.
      /// </summary>
      property PrintPageEvent    : TGIS_PrintPageEvent
                                                read  FOnPrintPage
                                                write FOnPrintPage ;

      /// <event/>
      /// <summary>
      ///   BeforePrintMap event.
      ///   Will be fired before printing a map after visible extent and scale
      ///   are already set.
      /// </summary>
      property BeforePrintMapEvent : TGIS_PrintMapEvent
                                                read  FOnBeforePrintMap
                                                write FOnBeforePrintMap ;

      /// <event/>
      /// <summary>
      ///   AfterPrintMap event.
      ///   Will be fired after printing a map.
      /// </summary>
      property AfterPrintMapEvent  : TGIS_PrintMapEvent
                                                read  FOnAfterPrintMap
                                                write FOnAfterPrintMap ;

  end ;

//##############################################################################
implementation

uses
  System.SysUtils,
  System.Math,
  FMX.Types,
  FMX.TextLayout,

  GisRtl,
  GisInternals,
  GisLayer,
  GisSymbol,
  GisViewer,
  FMX.GisFramework,
  FMX.GisRenderer,
  FMX.GisControlScale,
  FMX.GisControlNorthArrow,
  FMX.GisControlLegend,
  FMX.GisViewerWnd;

type
  T_FMXBitmap = {$IFDEF LEVEL_XE5_FMX}
                  FMX.Graphics.TBitmap ;
                {$ELSE}
                  FMX.Types.TBitmap ;
                {$ENDIF}

//==============================================================================
// IGIS_ViewerParent
//==============================================================================

  procedure TGIS_PrintManager.ControlClose ;
  begin
  end ;

  procedure TGIS_PrintManager.ControlDrawTexture(
          _bmp : TObject     ;
    const _extent : TGIS_Extent ;
    const _ppi : Integer
  ) ;
  begin
    ControlDrawTexture( _bmp, nil, _extent, _ppi ) ;
  end ;

  procedure TGIS_PrintManager.ControlDrawTexture(
          _bmp   : TObject     ;
    const _layer : TGIS_LayerAbstract ;
    const _extent: TGIS_Extent ;
    const _ppi   : Integer
  ) ;
  var
    ctx     : TGIS_RendererContext ;
    bmp     : T_FMXBitmap          ;
    old_ext : TGIS_Extent          ;
    old_res : Boolean              ;
    la      : TGIS_Layer           ;
  begin
    oViewer.Lock ;
    old_ext := oViewer.VisibleExtent ;
    old_res := oViewer.RestrictedDrag ;
    inRect := True ;
    try
      bmp := T_FMXBitmap( TGIS_Bitmap( _bmp ).NativeBitmap ) ;
      rectSize := Point( bmp.Width, bmp.Height ) ;
      oViewer.RestrictedDrag := False ;
      oViewer.VisibleExtent  := _extent ;
      if oViewer.Color.ARGB = TGIS_Color.None.ARGB then begin
      end
      else begin
        bmp.Clear( FMXColor( oViewer.Color ) ) ;
      end;

      if oViewer.IsEmpty then exit ;

      ctx := TGIS_RendererContext.Create ;
      try
        ctx.AssignBaseMap  ( bmp, False ) ;
        ctx.AssignSelection( nil, True  ) ;
        ctx.AssignCharts   ( nil, True  ) ;
        ctx.AssignLabels   ( nil, True  ) ;

        oRenderer.ReleaseContext ;
        oRenderer.CreateContext( self, self.oViewer, ctx, Point( 0, 0 ),
                                 RoundS( bmp.Width), RoundS( bmp.Height ),
                                 _ppi, oViewer.FontScale
                               ) ;
        try
          oViewer.LabelsReg.Reset ;
          oRenderer.PrepareDraw ;
          oViewer.BeginPaintInternal ;

          if Assigned( _layer ) then begin
            la := TGIS_Layer( _layer ) ;
            TGIS_RendererAbstract(oRenderer).LockTransparent( la.Transparency ) ;
            try
              try
                la.Renderer := oRenderer ;
                la.Paint ;
              except
                on e : Exception do begin
                  if not oViewer.NotifyPaintException( la.Name, e ) then
                    raise;
                end ;
              end ;
            finally
              TGIS_RendererAbstract(oRenderer).UnlockTransparent ;
            end ;
          end
          else
            oViewer.Draw( oRenderer, TGIS_DrawMode.AllExcept3D ) ;
        finally
          oViewer.EndPaintInternal ;
          oRenderer.AfterDraw ;

          bmp.Canvas.BeginScene ;
          try
            if Assigned( ctx.Selection ) then
              bmp.Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Selection),
                RectF( 0, 0, T_FMXBitmap(ctx.Selection).Width,
                             T_FMXBitmap(ctx.Selection).Height),
                RectF( 0, 0, bmp.Width,
                             T_FMXBitmap(ctx.Selection).Height
                             / T_FMXBitmap(ctx.Selection).Width * bmp.Width),
                oViewer.SelectionTransparency / 100, True
              ) ;
            if Assigned( ctx.Charts ) then
              bmp.Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Charts),
                RectF( 0, 0, T_FMXBitmap(ctx.Charts).Width,
                             T_FMXBitmap(ctx.Charts).Height),
                RectF( 0, 0, bmp.Width,
                             T_FMXBitmap(ctx.Charts).Height
                             / T_FMXBitmap(ctx.Charts).Width * bmp.Width),
                       1, False
              ) ;
            if Assigned( ctx.Labels ) then
              bmp.Canvas.DrawBitmap(
                T_FMXBitmap(ctx.Labels),
                RectF( 0, 0, T_FMXBitmap(ctx.Labels).Width,
                             T_FMXBitmap(ctx.Labels).Height),
                RectF( 0, 0, bmp.Width,
                             T_FMXBitmap(ctx.Labels).Height
                             / T_FMXBitmap(ctx.Labels).Width * bmp.Width),
                       1, False
              ) ;
          finally
            bmp.Canvas.EndScene ;
          end ;
        end ;
      finally
        oRenderer.ReleaseContext ;
        FreeObject( ctx )
      end ;
    finally
      inRect := False ;
      oViewer.VisibleExtent  := old_ext ;
      oViewer.RestrictedDrag := old_res ;
      oViewer.Unlock( False ) ;
    end;
  end ;

  function TGIS_PrintManager.ControlRenderer
    : TObject ;
  begin
    Result := oRenderer ;
  end ;

  procedure TGIS_PrintManager.ControlFlash(
    const _times : Integer ;
    const _delay : Integer
  ) ;
  begin
  end ;

  function TGIS_PrintManager.ControlSystemPPI
    : Integer ;
  begin
    if oPrinter is TGIS_PrinterPreview then
      Result := TGIS_PrinterPreview( oPrinter ).SystemPPI
    else
      Result := iDpi ;
  end ;

  function TGIS_PrintManager.ControlPPI
    : Integer ;
  begin
    Result := iDpi ;
  end ;

  function TGIS_PrintManager.ControlCanvasScale
    : Single ;
  begin
    if oPrinter is TGIS_PrinterPreview then
      Result := ControlPPI / ControlSystemPPI
    else
      Result := 1 ;
  end ;

  procedure TGIS_PrintManager.ControlAutoCenterViewport(
    const _dx, _dy: Double
  );
  begin

  end;

  procedure TGIS_PrintManager.ControlExtentChanged ;
  begin
    // do nothing
  end ;

  function TGIS_PrintManager.ControlCanvasHeight
    : Integer ;
  begin
    if inRect then
      Result := rectSize.Y
    else
      Result := oPrinter.PrintArea.Height ;
  end ;

  function TGIS_PrintManager.ControlCanvasWidth
    : Integer ;
  begin
    if inRect then
      Result := rectSize.X
    else
      Result := oPrinter.PrintArea.Width ;
  end ;

  procedure TGIS_PrintManager.ControlRepaint ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlProcessMessages ;
  begin
    // do nothing
  end ;

  function TGIS_PrintManager.ControlUpdateSynchronize(
    const _interrupt : Boolean
  ) : Boolean ;
  begin
    Result := True ; // always synchronic
  end;

  procedure TGIS_PrintManager.ControlUpdateWholeMap ;
  begin
  end ;

  procedure TGIS_PrintManager.ControlUpdateProgressive ;
  begin
    // do nothing
  end;

  procedure TGIS_PrintManager.ControlUpdateTopmost ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlUpdateBasemap ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlUpdateSelection ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlUpdateEditor(
    const _final : Boolean
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlHourglassShow ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlHourglassHide ;
  begin
    // do nothing
  end ;

  function  TGIS_PrintManager.ControlHourglassShake
    : Boolean ;
  begin
    Result := False ;
  end ;

  procedure TGIS_PrintManager.ControlSet3DMode(
    const _mode : TGIS_Viewer3DMode
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_PrintManager.ControlRaiseEditorChangeEvent(
    _sender : TObject
  ) ;
  begin
    // do nothing
  end ;

//==============================================================================
// constructor/destructor
//==============================================================================

  constructor TGIS_PrintManager.Create ;
  begin
    inherited ;
    FOnBeforePrint     := nil ;
    FOnAfterPrint      := nil ;
    FOnPrintPage       := nil ;
    FOnBeforePrintMap  := nil ;
    FOnAfterPrintMap   := nil ;
    inRect := False ;
    onPrintPage := false ;
  end ;

  procedure TGIS_PrintManager.doDestroy ;
  begin
    FOnBeforePrint     := nil ;
    FOnAfterPrint      := nil ;
    FOnPrintPage       := nil ;
    FOnBeforePrintMap  := nil ;
    FOnAfterPrintMap   := nil ;
    inherited ;
  end ;

  procedure TGIS_PrintManager.draw_box(
    const _rect       : TRect ;
    const _color      : TGIS_Color ;
    const _framecolor : TGIS_Color ;
    const _framewidth : Integer
  ) ;
  var
    path : TPathData ;
    r : TRectF ;
  begin
    if _color <> TGIS_Color.None then begin
      path := TPathData.Create ;
      try
        r.Left := _rect.Left ;
        r.Top := _rect.Top ;
        r.Right := _rect.Right ;
        r.Bottom := _rect.Bottom ;
        path.AddRectangle( r, 0, 0, [] ) ;

        oPrinter.Canvas.Fill.Bitmap := nil ;
        oPrinter.Canvas.Fill.Kind := TBrushKind.Solid ;
        oPrinter.Canvas.Fill.Color := FMXColor( _color ) ;
        oPrinter.Canvas.FillPath( path, 1 ) ;
      finally
        FreeObject( path ) ;
      end;
    end;
    draw_frame( _rect, _framecolor, _framewidth ) ;
  end ;

  procedure TGIS_PrintManager.draw_map(
    const _rect    : TRect       ;
    const _viewer  : IGIS_Viewer ;
    const _extent  : TGIS_Extent ;
    const _background
                   : Boolean     ;
    var   _scale   : Double      ;
    var   _rextent : TGIS_Extent ;
          _print   : Boolean
  ) ;
  var
    old_parent : IGIS_ViewerParent ;
    old_state  : TObject ;
    basemap_store : array of Boolean ;

    procedure store_basemap ;
    var
      i : Integer ;
    begin
      SetLength( basemap_store, _viewer.Items.Count ) ;
      for i := 0 to _viewer.Items.Count - 1 do begin
        basemap_store[i] := TGIS_Layer(_viewer.Items[i]).Basemap ;
        if basemap_store[i] then
          TGIS_Layer(_viewer.Items[i]).Basemap := False ;
      end;
    end;

    procedure restore_basemap ;
    var
      i : Integer ;
    begin
      for i := 0 to _viewer.Items.Count - 1 do begin
        if basemap_store[i] then
          TGIS_Layer(_viewer.Items[i]).Basemap := basemap_store[i] ;
      end;
    end ;

  begin
    if (_viewer is TGIS_ViewerWnd) and
       TGIS_ViewerWnd(_viewer).View3D then
      print3DMapInRectangle( _rect, _viewer )
    else begin
      old_state  := _viewer.StorePaintState ;
      try
        old_parent := _viewer.ReParent( Self ) ;
        try
          store_basemap ;
          printMapInRectangle( _rect, _viewer, _extent, _background, _scale, _rextent, _print ) ;
        finally
          restore_basemap ;
          _viewer.ReParent( old_parent ) ;
        end ;
      finally
        _viewer.RestorePaintState( old_state ) ;
      end;
    end ;
  end ;

  procedure TGIS_PrintManager.draw_control(
    const _rect    : TRect       ;
    const _control : IGIS_PrintableControl ;
    const _scale   : Double
  ) ;
  var
    bmp : TGIS_Bitmap ;
    old_parent : IGIS_ViewerParent ;
    sc_glow : Boolean ;
    na_glow : Boolean ;
  begin
    if not assigned( _control ) then exit ;
    if not ( _control is TGIS_ControlScale ) and
       not ( _control is TGIS_ControlNorthArrow ) and
       not ( _control is TGIS_ControlLegend ) then exit ;
    sc_glow := False ;
    na_glow := False ;
    if _control is TGIS_ControlScale then begin
      if not assigned( TGIS_ControlScale(_control).GIS_Viewer ) then exit ;
      old_parent := TGIS_ControlScale(_control).GIS_Viewer.ReParent( Self ) ;
      TGIS_ControlScale(_control).GIS_Viewer.TemporaryScaleInternal := _scale ;
      if metPrintDisableGlow then begin
        sc_glow := TGIS_ControlScale(_control).Glow ;
        TGIS_ControlScale(_control).Glow := False ;
      end ;
    end ;
    if _control is TGIS_ControlNorthArrow then begin
      if not assigned( TGIS_ControlNorthArrow(_control).GIS_Viewer ) then exit ;
      old_parent := TGIS_ControlNorthArrow(_control).GIS_Viewer.ReParent( Self ) ;
      TGIS_ControlNorthArrow(_control).GIS_Viewer.TemporaryScaleInternal := _scale ;
      if metPrintDisableGlow then begin
        na_glow := TGIS_ControlNorthArrow(_control).Glow ;
        TGIS_ControlNorthArrow(_control).Glow := False ;
      end ;
    end ;
    if _control is TGIS_ControlLegend then begin
      if not assigned( TGIS_ControlLegend(_control).GIS_Viewer ) then exit ;
      old_parent := TGIS_ControlLegend(_control).GIS_Viewer.ReParent( Self ) ;
      TGIS_ControlLegend(_control).GIS_Viewer.TemporaryScaleInternal := _scale ;
    end ;
    bmp := TGIS_Bitmap.Create( _rect.Width, _rect.Height) ;
    try
      _control.PrintBmp( bmp ) ;
      oPrinter.Canvas.BeginScene ;
      oPrinter.Canvas.DrawBitmap( T_FMXBitmap(bmp.NativeBitmap),
                                  RectF( 0, 0, bmp.Width, bmp.Height ),
                                  RectF( _rect.Left, _rect.Top,
                                         _rect.Right, _rect.Bottom ) ,
                                  1, False ) ;
      oPrinter.Canvas.EndScene ;
    finally
      FreeObject( bmp ) ;
      if _control is TGIS_ControlScale then begin
        if metPrintDisableGlow then
          TGIS_ControlScale(_control).Glow := sc_glow ;
        TGIS_ControlScale(_control).GIS_Viewer.TemporaryScaleInternal := 0 ;
        TGIS_ControlScale(_control).GIS_Viewer.ReParent( old_parent ) ;
      end ;
      if _control is TGIS_ControlNorthArrow then begin
        if metPrintDisableGlow then
          TGIS_ControlNorthArrow(_control).Glow := na_glow ;
        TGIS_ControlNorthArrow(_control).GIS_Viewer.TemporaryScaleInternal := 0 ;
        TGIS_ControlNorthArrow(_control).GIS_Viewer.ReParent( old_parent ) ;
      end ;
      if _control is TGIS_ControlLegend then begin
        TGIS_ControlLegend(_control).GIS_Viewer.TemporaryScaleInternal := 0 ;
        TGIS_ControlLegend(_control).GIS_Viewer.ReParent( old_parent ) ;
      end ;
    end ;
  end ;

  procedure TGIS_PrintManager.draw_legend(
    const _rect        : TRect  ;
    const _control     : IGIS_PrintableControl ;
    const _scale       : Double ;
    const _compactView : String ;
    const _iconStyle   : String ;
    const _reverseOrder: String ;
    const _font        : String ;
    const _fontSize    : String ;
    const _fontColor   : String
  ) ;
  begin
    draw_control( _rect, _control, _scale ) ;
  end ;

  procedure TGIS_PrintManager.draw_scale(
    const _rect        : TRect  ;
    const _control     : IGIS_PrintableControl ;
    const _scale       : Double ;
    const _dividers    : String ;
    const _dividerColor1 : String ;
    const _dividerColor2 : String ;
    const _font        : String ;
    const _fontSize    : String ;
    const _fontColor   : String
  ) ;
  begin
    draw_control( _rect, _control, _scale ) ;
  end ;

  procedure TGIS_PrintManager.draw_northArrow(
    const _rect    : TRect  ;
    const _control : IGIS_PrintableControl ;
    const _scale   : Double ;
    const _style   : String ;
    const _color1  : String ;
    const _color2  : String ;
    const _path    : String
  ) ;
  begin
    draw_control( _rect, _control, _scale ) ;
  end ;

  procedure TGIS_PrintManager.draw_graphic(
    const _rect    : TRect ;
    const _graphic : TGIS_TemplateGraphic ;
    const _path    : String
  ) ;
  var
    w, h : Integer ;
    sym  : TGIS_SymbolAbstract ;

    procedure draw_graphic_using_renderer ;
    var
      drawn : Boolean ;
      ctx   : TGIS_RendererContext ;
      cache : T_FMXBitmap ;
      rnd   : TGIS_RendererFMX ;
      f     : Double ;
      cnt   : Integer ;
    begin
      drawn := False ;
      cache := nil ;
      ctx := TGIS_RendererContext.Create ;
      try
        cache := T_FMXBitmap.Create( w, h );
        cache.Clear( TAlphaColorRec.Null ) ;

        ctx.AssignBaseMap( cache, True ) ;

        rnd := TGIS_RendererFMX.Create ;
        rnd.CreateContext( Self, oViewer, ctx, Point( 0, 0 ), w, h, iDpi, 100 ) ;
        try
          rnd.PrepareDraw ;

          if assigned( _graphic ) then
            // draw template graphic
            drawn := _graphic.Draw( rnd, Rect( 0, 0, w, h ) )
          else if assigned( sym ) then begin
            // draw symbol
            if ( sym.Width = 0 ) or ( sym.Height = 0 ) then exit ;
            f := Min( w/sym.Width, h/sym.Height ) ;
            sym.Prepare( oViewer,
                         -Max( RoundS(sym.Width*f), RoundS(sym.Height*f) ), // size in pixels
                         TGIS_Color.Black, // .Color,
                         TGIS_Color.Black, // .OutlineColor,
                         0,                // .SymbolRotate,
                         0,
                         TGIS_LabelPosition.MiddleCenter,
                         True,
                         rnd
                       ) ;
            sym.Draw( w div 2, h div 2 ) ;
            sym.Unprepare ;
            drawn := True ;
          end ;

        finally
          rnd.AfterDraw ;
          rnd.ReleaseContext ;
          FreeObject( rnd ) ;
        end ;
      finally
        if drawn then begin
          cnt := oPrinter.Canvas.BeginSceneCount ;
          if cnt = 0 then
            oPrinter.Canvas.BeginScene ;
          oPrinter.Canvas.DrawBitmap( cache,
                                      RectF( 0, 0, cache.Width, cache.Height ),
                                      RectF( _rect.Left, _rect.Top,
                                             _rect.Right, _rect.Bottom ) ,
                                      1, False ) ;
          if cnt = 0 then
            oPrinter.Canvas.EndScene ;
        end ;
        FreeObject( ctx ) ;
      end ;
     end ;

  begin
    try
      if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
      w := _rect.Width ;
      h := _rect.Height ;

      inRect := True ;
      try
        rectSize := Point( _rect.Width, _rect.Height ) ;
        if assigned( _graphic ) then begin
          draw_graphic_using_renderer ;
        end
        else begin
          sym := SymbolList.Prepare( _path ) ;
          if assigned( sym ) then begin
            try
              draw_graphic_using_renderer ;
            finally
              SymbolList.InternalDelete( _path ) ;
            end ;
          end ;
        end ;
      finally
        inRect := False ;
      end ;
    except
    end ;
  end ;

  procedure TGIS_PrintManager.draw_text(
    const _rect    : TRect ;
    const _text    : String ;
    const _name    : String ;
    const _style   : TGIS_FontStyles ;
    const _size    : Integer ;
    const _color   : TGIS_Color ;
    const _align   : TGIS_LabelAlignment ;
    const _bgColor : TGIS_Color  ;
    const _bgWidth : Integer
  ) ;
  var
    fnt  : TFont ;
    path : TPathData ;
    r    : TRectF ;

    procedure print_txt ;
    var
      tl : TTextLayout ;
    begin
      tl := TTextLayoutManager.TextLayoutByCanvas(
              oPrinter.Canvas.ClassType
            ).Create( oPrinter.Canvas ) ;
      try
        tl.BeginUpdate;
        tl.TopLeft := PointF( _rect.Left , _rect.Top    ) ;
        tl.MaxSize := PointF( _rect.Width, _rect.Height ) ;
        tl.Text := _text ;
        tl.WordWrap := False ;
        tl.Opacity := 1 ;
        if _align = TGIS_LabelAlignment.Center then
          tl.HorizontalAlign := TTextAlign.Center
        else if _align = TGIS_LabelAlignment.RightJustify then
          tl.HorizontalAlign := TTextAlign.Trailing
        else
          tl.HorizontalAlign := TTextAlign.Leading ;
        tl.VerticalAlign := TTextAlign.Leading ;
        tl.Font := oPrinter.Canvas.Font ;
        tl.Color := FMXColor(_color) ;
        tl.RightToLeft := False ;
        tl.EndUpdate ;
        tl.RenderLayout( oPrinter.Canvas ) ;
      finally
        FreeObject( tl ) ;
      end;
    end ;

  begin
    if IsStringEmpty( _text ) then exit ;

    fnt := TFont.Create ;
    fnt.Family := _name ;
    fnt.Size   := _size * iDpi div 72 ;
    fnt.Style  := FMXFontStyle( _style ) ;

    if _bgColor <> TGIS_Color.None then begin
      path := TPathData.Create ;
      try
        r.Left := _rect.Left ;
        r.Top := _rect.Top ;
        r.Right := _rect.Right ;
        r.Bottom := _rect.Bottom ;
        path.AddRectangle( r, 0, 0, [] ) ;

        oPrinter.Canvas.Fill.Bitmap := nil ;
        oPrinter.Canvas.Fill.Kind := TBrushKind.Solid ;
        oPrinter.Canvas.Fill.Color := FMXColor( _bgColor ) ;
        oPrinter.Canvas.FillPath( path, 1 ) ;
      finally
        FreeObject( path ) ;
      end;
    end ;

    if _bgWidth > 0 then begin
      path := TPathData.Create ;
      try
        r.Left := _rect.Left ;
        r.Top := _rect.Top ;
        r.Right := _rect.Right ;
        r.Bottom := _rect.Bottom ;
        path.AddRectangle( r, 0, 0, [] ) ;

        oPrinter.Canvas.Stroke.Bitmap := nil ;
        oPrinter.Canvas.Stroke.Kind := TBrushKind.Solid ;
        oPrinter.Canvas.Stroke.Dash  := TStrokeDash.Solid ;
        oPrinter.Canvas.Stroke.Join  := TStrokeJoin.Round ;
        oPrinter.Canvas.Stroke.Thickness := _bgWidth ;
        oPrinter.Canvas.Stroke.Cap := TStrokeCap.Flat ;
        oPrinter.Canvas.Stroke.Color := FMXColor( TGIS_Color.Black ) ;
        oPrinter.Canvas.DrawPath( path, 1 ) ;
      finally
        FreeObject( path ) ;
      end;
    end ;

    try
      oPrinter.Canvas.Font.Assign( fnt ) ;
      print_txt ;
    finally
      FreeObject( fnt ) ;
    end ;
  end ;

  function TGIS_PrintManager.GetViewer: TObject;
  begin
    Result := nil ;
  end;

  procedure TGIS_PrintManager.draw_frame(
    const _rect  : TRect ;
    const _color : TGIS_Color ;
    const _width : Integer
  ) ;
  var
    path : TPathData ;
    n1   : Integer ;
    n2   : Integer ;
    r    : TRectF ;
  begin
    if ( _color <> TGIS_Color.None ) and
       ( _width > 0 ) then begin
      path := TPathData.Create ;
      try
        // draw frame always inside rect
        n1 := _width div 2 ;
        n2 := ( _width - 1 ) div 2 ;
        r.Left := _rect.Left + n1 ;
        r.Top := _rect.Top + n1 ;
        r.Right := _rect.Right - n2 ;
        r.Bottom := _rect.Bottom - n2 ;
        path.AddRectangle( r, 0, 0, [] ) ;

        oPrinter.Canvas.Stroke.Bitmap := nil ;
        oPrinter.Canvas.Stroke.Kind := TBrushKind.Solid ;
        oPrinter.Canvas.Stroke.Dash  := TStrokeDash.Solid ;
        oPrinter.Canvas.Stroke.Join  := TStrokeJoin.Miter ;
        oPrinter.Canvas.Stroke.Thickness := _width ;
        oPrinter.Canvas.Stroke.Cap := TStrokeCap.Flat ;
        oPrinter.Canvas.Stroke.Color := FMXColor( _color ) ;
        oPrinter.Canvas.DrawPath( path, 1 ) ;
      finally
        FreeObject( path ) ;
      end;
    end ;
  end ;

  procedure TGIS_PrintManager.print3DMapInRectangle(
    const _rect   : TRect ;
    const _viewer : IGIS_Viewer
  ) ;
  var
    old_OverlappedExtentMargin : Integer ;
    old_parent : IGIS_ViewerParent ;
    wnd    : TGIS_ViewerWnd ;
    bmp    : TGIS_Bitmap ;
    w, h   : Integer ;
    f      : Single  ;
    rct    : TRect   ;
    tilesz : TPoint  ;

    procedure draw_on_canvas(
      _x : Integer ;
      _y : Integer
    ) ;
    var
      w1, h1 : Integer ;
    begin
      w1 := tilesz.X ;
      h1 := tilesz.Y ;

      if rct.Left + _x + w1 > rct.Right then
        w1 := w1 - ( ( rct.Left + _x + w1 ) - rct.Right  ) ;
      if rct.Top  + _y + h1  > rct.Bottom then
        h1 := h1 - ( ( rct.Top  + _y + h1 ) - rct.Bottom ) ;

      oPrinter.Canvas.BeginScene ;
      oPrinter.Canvas.DrawBitmap( T_FMXBitmap(bmp.NativeBitmap),
                                  RectF( 0, 0, w1, h1 ),
                                  RectF( rct.Left + _x,
                                         rct.Top  + _y,
                                         rct.Left + _x + w1,
                                         rct.Top  + _y + h1 ),
                                  1, False ) ;
      oPrinter.Canvas.EndScene ;
    end;

  begin
    wnd := _viewer as TGIS_ViewerWnd ;
    w := RoundS(wnd.Width) ;
    h := RoundS(wnd.Height) ;
    f := Min( _rect.Width/w, _rect.Height/h ) ;
    // compute real rectangle to print
    rct := Rect( RoundS(_rect.Left + (_rect.Width  - w * f) / 2),
                 RoundS(_rect.Top  + (_rect.Height - h * f) / 2),
                 RoundS(_rect.Left + (_rect.Width  + w * f) / 2),
                 RoundS(_rect.Top  + (_rect.Height + h * f) / 2)
               ) ;
    old_OverlappedExtentMargin := _viewer.OverlappedExtentMargin ;
    try
      // margin around a tile will be scanned while drawing for proper labels
      if FOverlappedExtentMargin < 0 then
        _viewer.OverlappedExtentMargin := _viewer.TwipsToPixels( 2 * 1440 )
      else
        _viewer.OverlappedExtentMargin := FOverlappedExtentMargin ;
      oRenderer := TGIS_RendererFMX.Create ;

      // printing

      old_parent := _viewer.ReParent( Self ) ;
      tilesz := wnd.Viewer3D.PrintBegin( rct.Width, rct.Height ) ;
      try
        w := 0 ;
        while w < rct.Width do begin
          h := 0 ;
          while h < rct.Height do begin
            bmp := TGIS_Bitmap.Create( tilesz.X, tilesz.y ) ;
            try
              wnd.Viewer3D.PrintTile( bmp, w, h ) ;
              draw_on_canvas( w, h ) ;
            finally
              FreeObject( bmp ) ;
            end ;
            Inc( h, tilesz.Y ) ;
          end ;
          Inc( w, tilesz.X ) ;
        end ;

      finally
        _viewer.ReParent( old_parent ) ;
        wnd.Viewer3D.PrintEnd ;
      end ;

    finally
      FreeObject( oRenderer ) ;
      _viewer.OverlappedExtentMargin := old_OverlappedExtentMargin ;
    end ;
  end;

  procedure TGIS_PrintManager.printMapInRectangle(
    const _rect    : TRect ;
    const _viewer  : IGIS_Viewer ;
    const _extent  : TGIS_Extent ;
    const _background
                   : Boolean ;
    var   _scale   : Double ;
    var   _rextent : TGIS_Extent ;
          _print   : Boolean
  ) ;
  var
    old_restricted : Boolean ;
    ext_page       : TGIS_Extent ;
    ext_tile       : TGIS_Extent ;
    xtiles, ytiles : Integer ;
    i, j           : Integer ;
    ext            : TGIS_Extent ;
    old_TemporaryVisibleExtent : TGIS_Extent ;
    metPaintTopmostLabelsOnTop : Boolean ;
    metPaintLabelsOnTop : Boolean ;
    cnv_scale     : Single ;

    procedure print_map(
      const _rect : TRect
    ) ;
    var
      w, h  : Integer ;
      ctx   : TGIS_RendererContext ;
      cache : T_FMXBitmap ;
      cnt   : Integer ;
      ctx_labels : T_FMXBitmap ;
      ctx_labels_topmost : T_FMXBitmap ;
      old_OverlappedExtentMargin : Integer ;
    begin
      if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
      w := _rect.Width ;
      h := _rect.Height ;

      cache := nil ;
      ctx_labels := nil ;
      ctx_labels_topmost := nil ;
      ctx := TGIS_RendererContext.Create ;
      try
        cache := T_FMXBitmap.Create( w, h ) ;
        if _background then
          cache.Clear( FMXColor( _viewer.Color ) )
        else
          cache.Clear( FMXColor( TGIS_Color.None ) ) ;

        ctx.AssignBaseMap  ( cache, True ) ;
        ctx.AssignSelection( nil  , True ) ;
        ctx.AssignCharts   ( nil  , True ) ;
        ctx.AssignLabels   ( nil  , True ) ;

        cache := T_FMXBitmap(ctx.BaseMap) ;

        oRenderer := TGIS_RendererFMX.Create ;

        oRenderer.CreateContext( Self, _viewer,
                                 ctx, Point( 0, 0 ), w, h, iDpi, 100, _rect
                               ) ;
        old_OverlappedExtentMargin := _viewer.OverlappedExtentMargin ;
        try
          // margin around a tile will be scanned while drawing
          if FOverlappedExtentMargin < 0 then
            _viewer.OverlappedExtentMargin := oRenderer.TwipsToPixels( 50 * 1440, 0 )
          else
            _viewer.OverlappedExtentMargin := FOverlappedExtentMargin ;
          oRenderer.PrepareDraw ;
          _viewer.BeginPaintInternal ;

          _viewer.Draw( oRenderer, TGIS_DrawMode.AllExceptTop ) ;

        finally
          _viewer.EndPaintInternal ;
          oRenderer.AfterDraw ;

          //blend bitmaps
          if cache.Canvas.BeginScene then begin
            try
              if Assigned( ctx.Selection ) then
                cache.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Selection),
                  RectF( 0, 0,
                         T_FMXBitmap(ctx.Selection).Width,
                         T_FMXBitmap(ctx.Selection).Height),
                  RectF( 0, 0,
                         w,
                         T_FMXBitmap(ctx.Selection).Height
                         / T_FMXBitmap(ctx.Selection).Width * w),
                 _viewer.SelectionTransparency / 100, False
               ) ;
              if Assigned( ctx.Charts ) then
                cache.Canvas.DrawBitmap(
                  T_FMXBitmap(ctx.Charts),
                  RectF( 0, 0,
                         T_FMXBitmap(ctx.Charts).Width,
                         T_FMXBitmap(ctx.Charts).Height),
                  RectF( 0, 0,
                         w,
                         T_FMXBitmap(ctx.Charts).Height
                         / T_FMXBitmap(ctx.Charts).Width * w),
                  1, False
                ) ;
              if Assigned( ctx.Labels ) then begin
                if not metPaintLabelsOnTop then
                  cache.Canvas.DrawBitmap(
                    T_FMXBitmap(ctx.Labels),
                    RectF( 0, 0,
                           T_FMXBitmap(ctx.Labels).Width,
                           T_FMXBitmap(ctx.Labels).Height),
                    RectF( 0, 0,
                           w,
                           T_FMXBitmap(ctx.Labels).Height
                           / T_FMXBitmap(ctx.Labels).Width * w),
                    1, False
                  )
                else begin
                  ctx_labels := T_FMXBitmap.Create( w, h ) ;
                  ctx_labels.Assign( T_FMXBitmap(ctx.Labels) ) ;
                end ;
              end ;
            finally
              cache.Canvas.EndScene ;
            end;
          end ;

          oRenderer.ReleaseContext ;

          if _viewer.IsTopmost then begin

            ctx.AssignSelection( nil, False ) ;
            ctx.AssignCharts   ( nil, False ) ;
            if metPaintTopmostLabelsOnTop then
              ctx.AssignLabels( nil, True )
            else
              ctx.AssignLabels( nil, False ) ;
            oRenderer.CreateContext( Self, _viewer, ctx,
                                     Point(0, 0), w, h, iDpi, 100 ) ;
            try
              oRenderer.PrepareDraw ;
              _viewer.BeginPaintInternal ;

              _viewer.Draw( oRenderer, TGIS_DrawMode.Top ) ;

            finally
              _viewer.EndPaintInternal ;
              oRenderer.AfterDraw ;
            end;

            if Assigned( ctx.Labels ) then
            begin
              if not metPaintLabelsOnTop then begin
                if cache.Canvas.BeginScene then begin
                  try
                    cache.Canvas.DrawBitmap(
                      T_FMXBitmap(ctx.Labels),
                      RectF( 0, 0,
                             T_FMXBitmap(ctx.Labels).Width,
                             T_FMXBitmap(ctx.Labels).Height),
                      RectF( 0, 0,
                             w,
                             T_FMXBitmap(ctx.Labels).Height
                             / T_FMXBitmap(ctx.Labels).Width * w),
                      1, False
                    ) ;
                  finally
                    cache.Canvas.EndScene ;
                  end;
                end;
              end else begin
                ctx_labels_topmost := T_FMXBitmap.Create( w, h ) ;
                ctx_labels_topmost.Assign( T_FMXBitmap(ctx.Labels) ) ;
              end;
            end;

            oRenderer.ReleaseContext ;

          end;

          if assigned( ctx_labels ) then begin
            // labels on top
            if cache.Canvas.BeginScene then begin
              try
                cache.Canvas.DrawBitmap(
                  ctx_labels,
                  RectF( 0, 0,
                         ctx_labels.Width,
                         ctx_labels.Height ),
                  RectF( 0, 0,
                         w,
                         ctx_labels.Height
                         / ctx_labels.Width * w),
                  1, False
                ) ;
              finally
                cache.Canvas.EndScene ;
              end;
            end ;
            FreeObject( ctx_labels ) ;
          end ;

          if assigned( ctx_labels_topmost ) then begin
            // topmost labels on top
            if cache.Canvas.BeginScene then begin
              try
                cache.Canvas.DrawBitmap(
                  ctx_labels_topmost,
                  RectF( 0, 0,
                         ctx_labels_topmost.Width,
                         ctx_labels_topmost.Height ),
                  RectF( 0, 0,
                         w,
                         ctx_labels_topmost.Height
                         / ctx_labels_topmost.Width * w),
                  1, False
                ) ;
              finally
                cache.Canvas.EndScene ;
              end;
            end ;
            FreeObject( ctx_labels_topmost ) ;
          end ;

          _viewer.OverlappedExtentMargin := old_OverlappedExtentMargin ;
          FreeObject( oRenderer ) ;
        end ;
      finally
        cnt := oPrinter.Canvas.BeginSceneCount ;
        if cnt = 0 then
          oPrinter.Canvas.BeginScene ;
        if assigned( cache ) then
          oPrinter.Canvas.DrawBitmap( cache,
                                      RectF( 0, 0, cache.Width, cache.Height ),
                                      RectF( _rect.Left, _rect.Top,
                                             _rect.Right, _rect.Bottom ) ,
                                      1, False ) ;
        if cnt = 0 then
          oPrinter.Canvas.EndScene ;

        FreeObject( ctx ) ;
      end ;
    end ;

  begin
    old_restricted := _viewer.RestrictedDrag ;
    old_TemporaryVisibleExtent := _viewer.TemporaryVisibleExtent ;
    inRect := True ;
    try
      // setting rectSize affects ControlCanvasWidth and ControlCanvasHeight
      rectSize := Point( _rect.Width, _rect.Height ) ;
      _viewer.RestrictedDrag := False ;
      _viewer.VisibleExtent := _extent ;
      _rextent := _viewer.VisibleExtent ;
      if _scale > 1e-10 then
        _viewer.ScaleAsFloat := _scale ;
      _scale := _viewer.ScaleAsFloat ;
      if not _print then exit ;

      ext_page := _viewer.ScreenToMapRect(
                    Rect(0, 0, rectSize.X, rectSize.Y) ) ;
      ext_tile := _viewer.ScreenToMapRect(
                    Rect(0, 0, FTileSize, FTileSize) ) ;
      cnv_scale := ControlCanvasScale ;
      if cnv_scale <> 1 then begin
        ext_page := _viewer.ScreenToMapRect(
                      Rect( 0, 0,
                            RoundS(rectSize.X/cnv_scale),
                            RoundS(rectSize.Y/cnv_scale) )
                    ) ;
        ext_tile := _viewer.ScreenToMapRect(
                      Rect( 0, 0,
                            RoundS(FTileSize/cnv_scale),
                            RoundS(FTileSize/cnv_scale) )
                    ) ;
      end ;

      if assigned( FOnBeforePrintMap ) then
        FOnBeforePrintMap( Self, _viewer, _rect, oPrinter ) ;

      // labels allocator common for all tiles
      _viewer.LabelsReg.Reset ;
      // extent taken in account by labels allocator
      if GisIsNoWorld( FTemporaryVisibleExtent ) then
        _viewer.TemporaryVisibleExtent := ext_page
      else
        _viewer.TemporaryVisibleExtent := FTemporaryVisibleExtent ;

      metPaintTopmostLabelsOnTop := GisMetadataAsBoolean(
         METADATA_PAINTTOPMOSTLABELSONTOP,
         False
      ) ;
      metPaintLabelsOnTop := GisMetadataAsBoolean(
         METADATA_PAINTLABELSONTOP,
         False
      ) ;

      xtiles := _rect.Width div FTileSize ;
      if ( _rect.Width mod FTileSize ) > 0 then xtiles := xtiles + 1 ;
      ytiles := _rect.Height div FTileSize ;
      if ( _rect.Height mod FTileSize ) > 0 then ytiles := ytiles + 1 ;
      for j := 0 to ytiles-1 do begin
        for i := 0 to xtiles-1 do begin
          // new viewer size
          rectSize := Point( Min( TileSize, _rect.Width  - i * TileSize ),
                             Min( TileSize, _rect.Height - j * TileSize ) ) ;
          // new viewer extent
          ext.XMin := ext_page.XMin + i * (ext_tile.XMax - ext_tile.XMin) ;
          ext.XMax := Min( ext.XMin + (ext_tile.XMax - ext_tile.XMin), ext_page.XMax ) ;
          ext.YMax := ext_page.YMax - j * (ext_tile.YMax - ext_tile.YMin) ;
          ext.YMin := Max( ext.YMax - (ext_tile.YMax - ext_tile.YMin), ext_page.YMin ) ;

          _viewer.VisibleExtent := ext ;
          // force the same scale for all tiles
          _viewer.TemporaryScaleInternal := _scale ;

          // print a tile on printer
          print_map( Rect( _rect.Left + i * FTileSize,
                           _rect.Top  + j * FTileSize,
                           Min( _rect.Right,
                                _rect.Left + ( i + 1 ) * FTileSize ),
                           Min( _rect.Bottom,
                                _rect.Top  + ( j + 1 ) * FTileSize )
                         ) ) ;
        end ;
      end ;
    finally
      inRect := False ;
      if _print then begin
        _viewer.TemporaryScaleInternal := 0 ;
        _viewer.TemporaryVisibleExtent := old_TemporaryVisibleExtent ;
        if assigned( FOnAfterPrintMap ) then
          FOnAfterPrintMap( Self, _viewer, _rect, oPrinter ) ;
        _viewer.RestrictedDrag := old_restricted ;
      end ;
    end ;
  end ;

  procedure TGIS_PrintManager.printInternal ;
  var
    old_extent      : TGIS_Extent ;
    old_state       : TObject ;
    old_incpnt      : Boolean ;
    started_locally : Boolean ;
    pageno          : Integer ;
    translated      : Boolean ;
    lastpage        : Boolean ;

    function measure_text(
      _text : String
    ) : TPoint ;
    var
      r : TRectF;
      tl : TTextLayout ;
    begin
      r := RectF( 0, 0, 32000, 32000 ) ;

      tl := TTextLayoutManager.TextLayoutByCanvas(
              oPrinter.Canvas.ClassType
            ).Create( oPrinter.Canvas ) ;
      try
        tl.TopLeft := PointF( r.Left , r.Top );
        tl.MaxSize := PointF( r.Width, r.Height);
        tl.Text := _text ;
        tl.WordWrap := False;
        tl.Opacity := 1;
        tl.HorizontalAlign := TTextAlign.Leading;
        tl.VerticalAlign := TTextAlign.Leading;
        tl.Font := oPrinter.Canvas.Font;
        tl.RightToLeft := False;
        tl.EndUpdate;
        r := tl.TextRect ;
        Result.X := RoundS( r.Right  ) ;
        Result.Y := RoundS( r.Bottom ) ;
      finally
        FreeObject( tl ) ;
      end ;
    end ;

    procedure print_txt(
      const _rect  : TRect ;
      const _txt   : String ;
      const _color : TAlphaColor
    ) ;
    var
      tl : TTextLayout ;
    begin
      tl := TTextLayoutManager.TextLayoutByCanvas(
              oPrinter.Canvas.ClassType
            ).Create( oPrinter.Canvas ) ;
      try
        tl.BeginUpdate;
        tl.TopLeft := PointF( _rect.Left , _rect.Top    ) ;
        tl.MaxSize := PointF( _rect.Width, _rect.Height ) ;
        tl.Text := _txt ;
        tl.WordWrap := False ;
        tl.Opacity := 1 ;
        tl.HorizontalAlign := TTextAlign.Leading ;
        tl.VerticalAlign := TTextAlign.Leading ;
        tl.Font := oPrinter.Canvas.Font ;
        tl.Color := _color ;
        tl.RightToLeft := False ;
        tl.EndUpdate ;
        tl.RenderLayout( oPrinter.Canvas ) ;
      finally
        FreeObject( tl ) ;
      end;
    end ;

    procedure process_standard_printing ;
    var
      t, h      : Integer ;
      left, top : Integer ;
      fnt       : TFont   ;
      ex        : TPoint  ;
      rtitle    : TRect   ;
      rsubtitle : TRect   ;
      rfooter   : TRect   ;
      rcontent  : TRect   ;
      rextent   : TGIS_Extent ;
    begin
      t := 0 ;
      if not IsStringEmpty( Title ) then begin
        fnt := FMXFont( TitleFont ) ;
        try
          fnt.Size := fnt.Size * iDpi / 72 ;
          oPrinter.Canvas.Font.Assign( fnt ) ;
          ex := measure_text( Title ) ;
          h := ex.Y * 110 div 100 ;
          rtitle := Rect( 0, t, oPrinter.PrintArea.Width, t+h ) ;
          left := rtitle.Left + ( (rtitle.Right -  rtitle.Left) - ex.X ) div 2 ;
          top  := rtitle.Top  + ( (rtitle.Bottom - rtitle.Top ) - ex.Y ) div 2 ;
          rtitle := Rect( left, top, left + ex.X, top + ex.Y ) ;
          print_txt( rtitle, Title, FMXColor( TitleFont.Color ) ) ;
        finally
          FreeObject( fnt ) ;
        end ;
      end else
        h := 0 ;

      t := t + h ;
      if not IsStringEmpty( Subtitle ) then begin
        fnt := FMXFont( SubtitleFont ) ;
        try
          fnt.Size := fnt.Size * iDpi / 72 ;
          oPrinter.Canvas.Font.Assign( fnt ) ;
          ex := measure_text( Subtitle ) ;
          h := ex.Y * 110 div 100 ;
          rsubtitle := Rect( 0, t, oPrinter.PrintArea.Width, t+h ) ;
          left := rsubtitle.Left + ( (rsubtitle.Right -  rsubtitle.Left) - ex.X ) div 2 ;
          top  := rsubtitle.Top  + ( (rsubtitle.Bottom - rsubtitle.Top ) - ex.Y ) div 2 ;
          rsubtitle := Rect( left, top, left + ex.X, top + ex.Y ) ;
          print_txt( rsubtitle, Subtitle, FMXColor( SubtitleFont.Color ) ) ;
        finally
          FreeObject( fnt ) ;
        end ;
      end else
        h := 0 ;

      t := t + h ;
      if not IsStringEmpty( Footer ) then begin
        fnt := FMXFont( FooterFont ) ;
        try
          fnt.Size := fnt.Size * iDpi / 72 ;
          oPrinter.Canvas.Font.Assign( fnt ) ;
          ex := measure_text( Footer ) ;
          h := ex.Y * 110 div 100 ;
          rfooter := Rect( 0, oPrinter.PrintArea.Height - h,
                           oPrinter.PrintArea.Width,
                           oPrinter.PrintArea.Height ) ;
          left := rfooter.Left + ( (rfooter.Right -  rfooter.Left) - ex.X ) div 2 ;
          top  := rfooter.Top  + ( (rfooter.Bottom - rfooter.Top ) - ex.Y ) div 2 ;
          rfooter := Rect( left, top, left + ex.X, top + ex.Y ) ;
          print_txt( rfooter, Footer, FMXColor( FooterFont.Color ) ) ;
        finally
          FreeObject( fnt ) ;
        end ;
      end else
        h := 0 ;

      rcontent := Rect( 0, t,
                        oPrinter.PrintArea.Width,
                        oPrinter.PrintArea.Height - h ) ;
      draw_map( rcontent, oViewer, old_extent, True, oViewerScale, rextent, True ) ;
    end ;

  begin

    pageno := oPrinter.PageNumber ;

    // store context
    old_extent := GisCommonExtent( oViewer.Extent, oViewer.VisibleExtent ) ;
    old_state  := oViewer.StorePaintState ;
    old_incpnt := oViewer.IncrementalPaint ;

    oViewer.IncrementalPaint := False ;

    try
      started_locally := False ;
      if not oPrinter.Printing then begin
        oPrinter.BeginDoc ;
        started_locally := True ;
      end ;
      oPrinter.PageNumber := pageno ;
      iDpi := oPrinter.Dpi ;
      try
        if not oPrinter.Printing then exit ;

        translated := False ;

        if Assigned( PrintPageEvent ) then begin
          translated := True ;
          while True do begin
            lastpage := True  ;
            onPrintPage := true ;
            oPrinter.Canvas.BeginScene ;
            PrintPageEvent( Self, Self, lastpage ) ;
            oPrinter.Canvas.EndScene ;
            onPrintPage := false ;
            if oPrinter.VirtualPrinter then begin
              if lastpage then oPrinter.PageNumber := 9999 ;
              break ;
            end ;
            if lastpage then break ;
            if oPrinter.Aborted then break ;
            oPrinter.NewPage ;
          end ;
        end ;

        if not translated then begin // standard printing

          oPrinter.Canvas.BeginScene ;
          try
            if not assigned( FTemplate ) then
              process_standard_printing
            else
              processTemplatePrinting( iDpi, oPrinter.PrintArea,
                                       oPrinter.PageWidth,
                                       oPrinter.PageHeight ) ;
          finally
            oPrinter.Canvas.EndScene ;
          end ;

          oPrinter.PageNumber := 9999 ;
        end ;
      finally
        if started_locally then begin
          if oPrinter.Printing then begin
            if not oPrinter.Aborted then
              oPrinter.EndDoc
          end ;
        end ;
      end ;
    finally
      // restore context
      oViewer.RestorePaintState( old_state ) ;
      oViewer.IncrementalPaint := old_incpnt ;
    end ;
 end ;

  procedure TGIS_PrintManager.Print(
    const _viewer  : IGIS_Viewer
  ) ;
  var
    scale : Double ;
  begin
    scale := 0 ;
    Print( _viewer, nil, scale ) ;
  end ;

  procedure TGIS_PrintManager.Print(
    const _viewer  : IGIS_Viewer ;
    const _printer : TObject
  ) ;
  var
    scale : Double ;
  begin
    scale := 0 ;
    Print( _viewer, _printer, scale ) ;
  end ;

  procedure TGIS_PrintManager.Print(
    const _viewer   : IGIS_Viewer ;
    const _printer  : TObject     ;
    var   _scale    : Double
  ) ;
  begin
    if assigned( _viewer ) then begin
      if GisIsNoWorld( _viewer.Extent ) then exit ;
      if _viewer.InPaint then exit ;
      _viewer.WaitForBackgroundProcesses ;
    end ;

    if _printer = nil then
      oPrinter := TGIS_Printer.Create( FMX.Printer.Printer )
    else begin
      if not( _printer is TGIS_Printer ) then exit ;
      oPrinter := TGIS_Printer( _printer ) ;
    end ;
    try
      if not oPrinter.VirtualPrinter and
         not assigned ( oPrinter.Printer ) then exit ;

      {$IFDEF MSWINDOWS}
        // assume that the printer can have an empty list
        // of available dpi
      {$ELSE}
        if assigned( oPrinter.Printer ) and
          ( oPrinter.Printer.ActivePrinter.DPICount > 0 ) then exit ;
      {$ENDIF}

      oViewer := _viewer ;
      if assigned( BeforePrintEvent ) then
        BeforePrintEvent( Self, oPrinter.Printer ) ;
      try

        oPrinter.Title := Title ;
        if not IsStringEmpty( Subtitle ) then
          oPrinter.Title :=
            oPrinter.Title + ' [' + Subtitle + ']';
        if IsStringEmpty( oPrinter.Title ) then
          oPrinter.Title := Application.Title ;
        if IsStringEmpty( oPrinter.Title ) then
          oPrinter.Title := GetFileName( Application.DefaultTitle ) ;

        oViewerScale := _scale ;
        printInternal ;
        _scale := oViewerScale ;

      finally
        if assigned( AfterPrintEvent ) then
          AfterPrintEvent( Self, oPrinter.Printer ) ;
      end ;

    finally
      oViewer := nil ;
      if not assigned( _printer ) then
        FreeObject( oPrinter )
      else
        oPrinter := nil ;
    end;
  end ;

  procedure TGIS_PrintManager.GetDrawingParams(
    const _viewer  : IGIS_Viewer ;
    const _extent  : TGIS_Extent ;
    const _rect    : TRect   ;
    var   _scale   : Double
  ) ;
  var
    rextent : TGIS_Extent ;
  begin
    if GisIsNoWorld( _extent ) then exit ;
    if not assigned( _viewer ) then exit ;
    if not onPrintPage then exit ;
    draw_map( _rect, _viewer, _extent, True, _scale, rextent, False ) ;
  end ;

  procedure TGIS_PrintManager.DrawMap(
    const _viewer : IGIS_Viewer ;
    const _extent : TGIS_Extent ;
    const _rect   : TRect ;
    var   _scale  : Double
  ) ;
  var
    rextent : TGIS_Extent ;
  begin
    DrawMap( _viewer, _extent, _rect, _scale, rextent ) ;
  end ;

  procedure TGIS_PrintManager.DrawMap(
    const _viewer  : IGIS_Viewer ;
    const _extent  : TGIS_Extent ;
    const _rect    : TRect ;
    var   _scale   : Double ;
    var   _rextent : TGIS_Extent
  ) ;
  begin
    if GisIsNoWorld( _extent ) then exit ;
    if not assigned( _viewer ) then exit ;
    if not onPrintPage then exit ;
    oPrinter.Canvas.BeginScene ;
    try
      draw_map( _rect, oViewer, _extent, True, _scale, _rextent, True ) ;
    finally
      oPrinter.Canvas.EndScene ;
    end ;
  end ;

  procedure TGIS_PrintManager.DrawControl(
    const _control : IGIS_PrintableControl ;
    const _rect    : TRect
  ) ;
  begin
    DrawControl( _control, _rect, 0 ) ;
  end ;

  procedure TGIS_PrintManager.DrawControl(
    const _control : IGIS_PrintableControl ;
    const _rect    : TRect ;
    const _scale   : Double
  ) ;
  begin
    if not assigned( _control ) then exit ;
    if not onPrintPage then exit ;
    oPrinter.Canvas.BeginScene ;
    try
      draw_control( _rect, _control, 0 ) ;
    finally
      oPrinter.Canvas.EndScene ;
    end ;
  end ;

  procedure TGIS_PrintManager.DrawGraphic(
    const _path : String ;
    const _rect : TRect
  ) ;
  begin
    if not onPrintPage then exit ;
    oPrinter.Canvas.BeginScene ;
    try
      draw_graphic( _rect, nil, _path ) ;
    finally
      oPrinter.Canvas.EndScene ;
    end ;
  end ;

  procedure TGIS_PrintManager.DrawGraphic(
    const _graphic : TGIS_TemplateGraphic ;
    const _rect    : TRect
  ) ;
  begin
    if not onPrintPage then exit ;
    oPrinter.Canvas.BeginScene ;
    try
      draw_graphic( _rect, _graphic, '' ) ;
    finally
      oPrinter.Canvas.EndScene ;
    end ;
  end ;

function TGIS_PrintManager.SetViewer(const _viewer: TObject): TObject;
begin
  Result := nil ;
end;

//==================================== END =====================================
end.


