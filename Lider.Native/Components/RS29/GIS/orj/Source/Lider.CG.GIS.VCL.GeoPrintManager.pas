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
  VCL print manager.
}

unit VCL.GisPrintManager ;
{$HPPEMIT '#pragma link "VCL.GisPrintManager"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Types,
  VCL.Printers,
  VCL.Forms,
  VCL.Graphics,

  GisInterfaces,
  GisTypes,
  GisTypesUI,
  GisFunctions,
  GisPrintManagerAbstract,
  GisRendererAbstract,
  GisTemplatePrint,
  VCL.GisPrinters;

type

  {$IFNDEF GENXDK}
    /// <summary>
    ///   Event for OnBeforePrint and OnAfterPrint.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_printer">
    ///   printer object on which printing will occur
    /// </param>
    TGIS_PrintEvent =  procedure(
      _sender   : TObject  ;
      _printer  : TPrinter
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Event for OnBeforePrint and OnAfterPrint.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    TGIS_PrintEvent =  procedure(
      _sender   : TObject  ;
    ) of object ;
  {$ENDIF}

  TGIS_PrintManager = class ;

  /// <summary>
  ///   Event for OnPrintPage, OnBeforePrintPage and OnAfterPrintPage.
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
    {$IFDEF GIS_XDK}
      var _translated : Boolean ;
    {$ENDIF}
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
  ///   Print manager used for VCL.
  /// </summary>
  TGIS_PrintManager = class ( TGIS_PrintManagerAbstract, IGIS_ViewerParent )

    private
      [weak]
      oViewer   : IGIS_Viewer ;
      oViewerRenderer : TObject ;
      oViewerScale : Double ;
      oRenderer : TGIS_RendererAbstract ;
      oPrinter  : TGIS_Printer ;
      iDpi      : Integer ;

    private
      inRect      : Boolean ;
      rectSize    : TPoint  ;
      inPrintPage : Boolean ;

    private // properties - events

      /// <summary>
      ///   BeforePrintEvent event. Will be fired on TGIS_ViewerWnd.Print.
      /// </summary>
      FOnBeforePrint : TGIS_PrintEvent ;

      /// <summary>
      ///   AfterPrintEvent event. Will be fired upon completion of TGIS_ViewerWnd.Print.
      /// </summary>
      FOnAfterPrint  : TGIS_PrintEvent ;

      /// <summary>
      ///   PrintPageEvent. Will be fired on TGIS_ViewerWnd.Print.
      /// </summary>
      FOnPrintPage       : TGIS_PrintPageEvent ;

      /// <summary>
      ///   BeforePrintPageEvent. Will be fired before printing each page.
      /// </summary>
      FOnBeforePrintPage : TGIS_PrintPageEvent ;

      /// <summary>
      ///   AfterPrintPageEvent. Will be fired after printing each page.
      /// </summary>
      FOnAfterPrintPage  : TGIS_PrintPageEvent ;

      /// <summary>
      ///   BeforePrintMapEvent. Will be fired before printing a map.
      /// </summary>
      FOnBeforePrintMap : TGIS_PrintMapEvent ;

      /// <summary>
      ///   AfterPrintMapEvent. Will be fired after printing a map.
      /// </summary>
      FOnAfterPrintMap  : TGIS_PrintMapEvent ;

    private
      procedure fix_alpha          ( const _bmp : VCL.Graphics.TBitmap
                                   ) ;
      procedure printInternal ;
      procedure print3DMapInRectangle
                                   ( const _rect   : TRect ;
                                     const _viewer : IGIS_Viewer
                                   ) ;
      procedure printMapInRectangle( const _rect       : TRect ;
                                     const _viewer     : IGIS_Viewer ;
                                     const _extent     : TGIS_Extent ;
                                     const _background : Boolean ;
                                     var   _scale      : Double ;
                                     var   _rextent    : TGIS_Extent ;
                                           _print      : Boolean
                                   ) ;

    protected

      /// <inheritdoc/>
      procedure draw_box           ( const _rect       : TRect      ;
                                     const _color      : TGIS_Color ;
                                     const _framecolor : TGIS_Color ;
                                     const _framewidth : Integer
                                   ) ; override;

      /// <inheritdoc/>
      procedure draw_map           ( const _rect       : TRect       ;
                                     const _viewer     : IGIS_Viewer ;
                                     const _extent     : TGIS_Extent ;
                                     const _background : Boolean     ;
                                     var   _scale      : Double      ;
                                     var   _rextent    : TGIS_Extent ;
                                           _print      : Boolean
                                   ) ; override;

      /// <inheritdoc/>
      procedure draw_control       ( const _rect    : TRect       ;
                                     const _control : IGIS_PrintableControl ;
                                     const _scale   : Double
                                   ) ; override;

      /// <inheritdoc/>
      procedure draw_legend        ( const _rect        : TRect  ;
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
      procedure draw_scale         ( const _rect        : TRect  ;
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
      procedure draw_northArrow    ( const _rect        : TRect  ;
                                     const _control     : IGIS_PrintableControl ;
                                     const _scale       : Double ;
                                     const _style       : String ;
                                     const _color1      : String ;
                                     const _color2      : String ;
                                     const _path        : String
                                   ) ; override ;

      /// <inheritdoc/>
      procedure draw_graphic       ( const _rect    : TRect      ;
                                     const _graphic : TGIS_TemplateGraphic ;
                                     const _path    : String
                                   ) ; override ;

      /// <inheritdoc/>
      procedure draw_text          ( const _rect    : TRect       ;
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
      procedure draw_frame         ( const _rect   : TRect      ;
                                     const _color  : TGIS_Color ;
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
      procedure ControlUpdateProgressive ;

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
      procedure doDestroy      ; override;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create       ;

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
                               ) ; override ;

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
                                 const _rect    : TRect       ;
                                 var   _scale   : Double
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawMap      ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect       ;
                                 var   _scale   : Double      ;
                                 var   _rextent : TGIS_Extent
                               ) ; override;

      /// <inheritdoc/>
      procedure   DrawControl  ( const _control : IGIS_PrintableControl ;
                                 const _rect    : TRect
                               ) ; overload ; override;
      /// <inheritdoc/>
      procedure   DrawControl  ( const _control : IGIS_PrintableControl ;
                                 const _rect    : TRect ;
                                 const _scale   : Double
                               ) ; overload ; override;


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
      property Printer           : TGIS_Printer read oPrinter ;

    published

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
                                                read  FOnAfterPrint
                                                write FOnAfterPrint ;

      /// <event/>
      /// <summary>
      ///   PrintPage event. Will be fired on TGIS_ViewerWnd.Print. Override to add
      ///   custom printing.
      /// </summary>
      /// <remarks>
      ///   Handling this event turns off default printing.
      /// </remarks>
      property PrintPageEvent       : TGIS_PrintPageEvent
                                                read  FOnPrintPage
                                                write FOnPrintPage ;

      /// <event/>
      /// <summary>
      ///   BeforePrintPage event. Will be fired before printing each page.
      /// </summary>
      /// <remarks>
      ///   Use this event to provide some extra printing or to modify
      ///   print templates upon printing on multiple pages.
      /// </remarks>
      property BeforePrintPageEvent : TGIS_PrintPageEvent
                                                read  FOnBeforePrintPage
                                                write FOnBeforePrintPage ;

      /// <event/>
      /// <summary>
      ///   AfterPrintPage event. Will be fired after printing each page.
      /// </summary>
      /// <remarks>
      ///   Use this event to provide some extra printing or to modify
      ///   multiple pages printing.
      /// </remarks>
      property AfterPrintPageEvent  : TGIS_PrintPageEvent
                                                read  FOnAfterPrintPage
                                                write FOnAfterPrintPage ;

      /// <event/>
      /// <summary>
      ///   BeforePrintMap event.
      ///   Will be fired before printing a map after visible extent and scale
      ///   are already set.
      /// </summary>
      property BeforePrintMapEvent  : TGIS_PrintMapEvent
                                                read  FOnBeforePrintMap
                                                write FOnBeforePrintMap ;

      /// <event/>
      /// <summary>
      ///   AfterPrintMap event.
      ///   Will be fired after printing a map.
      /// </summary>
      property AfterPrintMapEvent   : TGIS_PrintMapEvent
                                                read  FOnAfterPrintMap
                                                write FOnAfterPrintMap ;

  end ;

//##############################################################################
implementation

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.UITypes,
  System.SysUtils,
  System.Math,

  GisRtl,
  GisInternals,
  GisLayer,
  GisSymbol,
  GisParams,
  GisLayerVector,
  GisViewer,
  GisResource,
  VCL.GisFramework,
  VCL.GisRenderer,
  VCL.GisRendererGdi32,
  VCL.GisRendererGdiPlus,
  VCL.GisRendererDirect2D,
  VCL.GisControlScale,
  VCL.GisControlLegend,
  VCL.GisControlNorthArrow,
  VCL.GisGdiPlus,
  VCL.GisViewerWnd ;

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
    bmp     : VCL.Graphics.TBitmap ;
    old_ext : TGIS_Extent          ;
    old_res : Boolean              ;
    la      : TGIS_Layer           ;
  begin
    if not assigned( oViewer ) then exit ;
    oViewer.Lock ;
    old_ext := oViewer.VisibleExtent ;
    old_res := oViewer.RestrictedDrag ;
    inRect := True ;
    try
      bmp := VCL.Graphics.TBitmap( TGIS_Bitmap( _bmp ).NativeBitmap ) ;
      rectSize := Point( bmp.Width, bmp.Height ) ;
      oViewer.RestrictedDrag := False ;
      oViewer.VisibleExtent  := _extent ;
      try
        if oViewer.Color.ARGB = TGIS_Color.None.ARGB then begin
        end
        else begin
          bmp.Canvas.Brush.Color := oViewer.Color.ToBGR ;
          bmp.Canvas.FillRect( Rect( 0, 0, bmp.Width, bmp.Height ) ) ;
        end;
      finally
        bmp.Canvas.Unlock ;
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

          bmp.Canvas.Lock ;

          if Assigned( ctx.Selection ) then
            TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
              ctx.Selection,
              ctx.BaseMap,
              oViewer.SelectionTransparency,
              False
            ) ;
          if Assigned( ctx.Charts ) then
            TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
              ctx.Charts,
              ctx.BaseMap,
              100,
              False
            );
          if Assigned( ctx.Labels ) then
            TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
              ctx.Labels,
              ctx.BaseMap,
              100,
              False
            ) ;
          bmp.Canvas.UnLock ;
        end;
      finally
        oRenderer.ReleaseContext ;
        FreeObject( ctx ) ;
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
    Result := 1 ;
  end;

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
    // do nothing
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

  procedure TGIS_PrintManager.ControlAutoCenterViewport(
    const _dx, _dy : Double
  );
  begin
    // do nothing
  end;

  procedure TGIS_PrintManager.ControlExtentChanged ;
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
    FOnBeforePrintPage := nil ;
    FOnAfterPrintPage  := nil ;
    FOnBeforePrintMap  := nil ;
    FOnAfterPrintMap   := nil ;

    inRect := False ;
    inPrintPage := false ;
  end ;

  procedure TGIS_PrintManager.doDestroy ;
  begin
    FOnBeforePrint     := nil ;
    FOnAfterPrint      := nil ;
    FOnPrintPage       := nil ;
    FOnBeforePrintPage := nil ;
    FOnAfterPrintPage  := nil ;
    FOnBeforePrintMap  := nil ;
    FOnAfterPrintMap   := nil ;

    inherited ;
  end ;

  procedure TGIS_PrintManager.fix_alpha(
    const _bmp : VCL.Graphics.TBitmap
  ) ;
  var
    scanline : IntPtr  ;
    x        : Integer ;
    y        : Integer ;
  begin
    for y := 0 to _bmp.Height -1 do begin
      scanline := IntPtr( _bmp.ScanLine[y] ) ;
      for x := 0 to _bmp.Width -1 do begin
        PByte( scanline + 3 + x*4 )^ := $FF ;
      end;
    end;
  end;

  procedure TGIS_PrintManager.draw_box(
    const _rect       : TRect ;
    const _color      : TGIS_Color ;
    const _framecolor : TGIS_Color ;
    const _framewidth : Integer
  ) ;
  begin
    if _color <> TGIS_Color.None then begin
      oPrinter.Canvas.Brush.Style := bsSolid ;
      oPrinter.Canvas.Brush.Color := VCLColor( _color ) ;
      oPrinter.Canvas.Pen.Style := psSolid ;
      oPrinter.Canvas.Pen.Width := 1 ;
      oPrinter.Canvas.Pen.Color := VCLColor( _color ) ;
      oPrinter.Canvas.Rectangle( _rect ) ;
    end;
    draw_frame( _rect, _framecolor, _framewidth ) ;
  end ;

  procedure TGIS_PrintManager.draw_map(
    const _rect       : TRect       ;
    const _viewer     : IGIS_Viewer ;
    const _extent     : TGIS_Extent ;
    const _background : Boolean     ;
    var   _scale      : Double      ;
    var   _rextent    : TGIS_Extent ;
          _print      : Boolean
  ) ;
  var
    old_parent : IGIS_ViewerParent ;
    old_state  : TObject ;
    basemap_store : array of Boolean ;

    procedure store_basemap ;
    var
      i : Integer ;
    begin
      if not assigned( _viewer ) then exit ;

      SetLength( basemap_store, _viewer.Items.Count ) ;
      for i := 0 to _viewer.Items.Count - 1 do begin
        TGIS_Layer(_viewer.Items[i]).Lock ;
        try
          basemap_store[i] := TGIS_Layer(_viewer.Items[i]).Basemap ;
          if basemap_store[i] then begin
            TGIS_Layer(_viewer.Items[i]).Basemap := False ;
          end;
        finally
          TGIS_Layer(_viewer.Items[i]).UnLock ;
        end;
      end;
    end;

    procedure restore_basemap ;
    var
      i : Integer ;
    begin
      if not assigned( _viewer ) then exit ;
      for i := 0 to _viewer.Items.Count - 1 do begin
        TGIS_Layer(_viewer.Items[i]).Lock ;
        try
          if basemap_store[i] then
            TGIS_Layer(_viewer.Items[i]).Basemap := basemap_store[i] ;
        finally
          TGIS_Layer(_viewer.Items[i]).UnLock ;
        end;
      end;
      _viewer.UnLock ;
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

  procedure TGIS_PrintManager.draw_legend(
    const _rect         : TRect  ;
    const _control      : IGIS_PrintableControl ;
    const _scale        : Double ;
    const _compactView  : String ;
    const _iconStyle    : String ;
    const _reverseOrder : String ;
    const _font         : String ;
    const _fontSize     : String ;
    const _fontColor    : String
  ) ;
  var
    legend     : TGIS_ControlLegend ;
    old_parent : IGIS_ViewerParent ;
    bmp : TGIS_Bitmap ;
    lg_cvw : Boolean ;
    lg_ist : TGIS_LegendIconStyle ;
    lg_rvo : Boolean ;
    lg_fnm : String ;
    lg_fsz : Integer ;
    lg_fcl : TColor ;

    procedure draw_on_canvas ;
    var
      b : TGIS_GdipBitmap ;
    begin
      if assigned( oPrinter.Graphics ) then begin
        b := TGIS_GdipBitmap.Create( bmp ) ;
        try
          oPrinter.Graphics.DrawImage( b, _rect, 0, 0, _rect.Width, _rect.Height ) ;
        finally
          FreeObject( b ) ;
        end;
      end else
        VCLCanvasDrawBitmapEx( oPrinter.Canvas,
                               _rect,
                               VCL.Graphics.TBitmap(bmp.NativeBitmap) );
    end;

  begin
    if not assigned( _control ) then exit ;
    if _control is TGIS_ControlLegend then begin
      legend := TGIS_ControlLegend( _control ) ;
      if not assigned( legend.GIS_Viewer ) then exit ;
      lg_cvw := legend.CompactView ;
      lg_ist := legend.DrawIconStyle ;
      lg_rvo := legend.ReverseOrder ;
      lg_fnm := legend.Font.Name ;
      lg_fsz := legend.Font.Size ;
      lg_fcl := legend.Font.Color ;
      try
        legend.CompactView   := ParamBoolean( _compactView, legend.CompactView ) ;
        legend.DrawIconStyle := ParamLegendIconStyle( _iconStyle, legend.DrawIconStyle ) ;
        legend.ReverseOrder  := ParamBoolean( _reverseOrder, legend.ReverseOrder ) ;
        legend.Font.Name     := ParamString ( _font,        legend.Font.Name   ) ;
        legend.Font.Size     := ParamInteger( _fontSize,    legend.Font.Size   ) ;
        legend.Font.Color    := VCLColor( ParamColor( _fontColor, GisColor(legend.Font.Color) ) ) ;

        old_parent := legend.GIS_Viewer.ReParent( Self ) ;
        legend.GIS_Viewer.TemporaryScaleInternal := _scale ;
        bmp := TGIS_Bitmap.Create( _rect.Width, _rect.Height) ;
        try
          legend.PrintBmp( bmp ) ;
          draw_on_canvas ;
        finally
          FreeObject( bmp ) ;
          legend.GIS_Viewer.TemporaryScaleInternal := 0 ;
          legend.GIS_Viewer.ReParent( old_parent ) ;
        end ;
      finally
        legend.CompactView   := lg_cvw ;
        legend.DrawIconStyle := lg_ist ;
        legend.ReverseOrder  := lg_rvo ;
        legend.Font.Name     := lg_fnm ;
        legend.Font.Size     := lg_fsz ;
        legend.Font.Color    := lg_fcl ;
      end ;
    end ;
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
  var
    scale : TGIS_ControlScale ;
    old_parent : IGIS_ViewerParent ;
    bmp : TGIS_Bitmap ;
    sc_glow : Boolean ;
    sc_dvds : Integer ;
    sc_dcl1 : TColor ;
    sc_dcl2 : TColor ;
    sc_fnm  : String ;
    sc_fsz  : Integer ;
    sc_fcl  : TColor ;

    procedure draw_on_canvas ;
    var
      b : TGIS_GdipBitmap ;
    begin
      if assigned( oPrinter.Graphics ) then begin
        b := TGIS_GdipBitmap.Create( bmp ) ;
        try
          oPrinter.Graphics.DrawImage( b, _rect, 0, 0, _rect.Width, _rect.Height ) ;
        finally
          FreeObject( b ) ;
        end;
      end else begin
        if scale.Transparent then
          oPrinter.Canvas.Draw( _rect.Left, _rect.Top,
                                VCL.Graphics.TBitmap(bmp.NativeBitmap),
                                255 )
        else
          VCLCanvasDrawBitmapEx( oPrinter.Canvas,
                                 _rect,
                                 VCL.Graphics.TBitmap(bmp.NativeBitmap) );
      end ;
    end;

  begin
    if not assigned( _control ) then exit ;
    if not ( _control is TGIS_ControlScale ) then exit ;
    sc_glow := False ;
    scale   := TGIS_ControlScale( _control ) ;
    if not assigned( scale.GIS_Viewer ) then exit ;
    sc_dvds := scale.Dividers ;
    sc_dcl1 := scale.DividerColor1 ;
    sc_dcl2 := scale.DividerColor2 ;
    sc_fnm  := scale.Font.Name ;
    sc_fsz  := scale.Font.Size ;
    sc_fcl  := scale.Font.Color ;
    try
      scale.Dividers := ParamInteger( _dividers, scale.Dividers ) ;
      scale.DividerColor1 := VCLColor( ParamColor( _dividerColor1, GisColor(scale.DividerColor1) ) ) ;
      scale.DividerColor2 := VCLColor( ParamColor( _dividerColor2, GisColor(scale.DividerColor2) ) ) ;
      scale.Font.Name := ParamString( _font, scale.Font.Name ) ;
      scale.Font.Size := ParamInteger( _fontSize, scale.Font.Size ) ;
      scale.Font.Color := VCLColor( ParamColor( _fontColor, GisColor(scale.Font.Color) ) ) ;

      old_parent := scale.GIS_Viewer.ReParent( Self ) ;
      scale.GIS_Viewer.TemporaryScaleInternal := _scale ;
      if metPrintDisableGlow then begin
        sc_glow := scale.Glow ;
        scale.Glow := False ;
      end ;
      bmp := TGIS_Bitmap.Create( _rect.Width, _rect.Height ) ;
      try
        scale.PrintBmp( bmp ) ;
        draw_on_canvas ;
      finally
        FreeObject( bmp ) ;
        if metPrintDisableGlow then
          scale.Glow := sc_glow ;
        scale.GIS_Viewer.TemporaryScaleInternal := 0 ;
        scale.GIS_Viewer.ReParent( old_parent ) ;
      end ;
    finally
      scale.Dividers      := sc_dvds ;
      scale.DividerColor1 := sc_dcl1 ;
      scale.DividerColor2 := sc_dcl2 ;
      scale.Font.Name     := sc_fnm ;
      scale.Font.Size     := sc_fsz ;
      scale.Font.Color    := sc_fcl ;
    end ;
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
  var
    narrow : TGIS_ControlNorthArrow ;
    old_parent : IGIS_ViewerParent ;
    bmp : TGIS_Bitmap ;

    na_style : TGIS_ControlNorthArrowStyle ;
    na_cl1   : TColor ;
    na_cl2   : TColor ;
    na_path  : String ;
    na_glow  : Boolean ;

    procedure draw_on_canvas ;
    var
      b : TGIS_GdipBitmap ;
    begin
      if assigned( oPrinter.Graphics ) then begin
        b := TGIS_GdipBitmap.Create( bmp ) ;
        try
          oPrinter.Graphics.DrawImage( b, _rect, 0, 0, _rect.Width, _rect.Height ) ;
        finally
          FreeObject( b ) ;
        end;
      end else begin
        if _control is TGIS_ControlNorthArrow then begin
          if TGIS_ControlNorthArrow(_control).Transparent then
            oPrinter.Canvas.Draw( _rect.Left, _rect.Top,
                                  VCL.Graphics.TBitmap(bmp.NativeBitmap),
                                  255 )
          else
            VCLCanvasDrawBitmapEx( oPrinter.Canvas,
                                   _rect,
                                   VCL.Graphics.TBitmap(bmp.NativeBitmap) );
        end;
      end ;
    end;

  begin
    if not assigned( _control ) then exit ;
    if not ( _control is TGIS_ControlNorthArrow ) then exit ;

    narrow := TGIS_ControlNorthArrow( _control ) ;
    if not assigned( narrow.GIS_Viewer ) then exit ;
    na_style := narrow.Style ;
    na_cl1   := narrow.Color1 ;
    na_cl2   := narrow.Color2 ;
    na_path  := narrow.Path ;
    na_glow  := False ;
    try
      narrow.Style  := ParamNorthArrowStyle( _style, narrow.Style ) ;
      narrow.Color1 := VCLColor( ParamColor( _color1, GisColor( narrow.Color1 ) ) ) ;
      narrow.Color2 := VCLColor( ParamColor( _color2, GisColor( narrow.Color2 ) ) ) ;
      narrow.Path   := ParamString( _path, narrow.Path ) ;

      old_parent := narrow.GIS_Viewer.ReParent( Self ) ;
      narrow.GIS_Viewer.TemporaryScaleInternal := _scale ;
      if metPrintDisableGlow then begin
        na_glow := narrow.Glow ;
        narrow.Glow := False ;
      end ;
      bmp := TGIS_Bitmap.Create( _rect.Width, _rect.Height ) ;
      try
        narrow.PrintBmp( bmp ) ;
        draw_on_canvas ;
      finally
        FreeObject( bmp ) ;
        if metPrintDisableGlow then
          narrow.Glow := na_glow ;
        narrow.GIS_Viewer.TemporaryScaleInternal := 0 ;
        narrow.GIS_Viewer.ReParent( old_parent ) ;
      end ;
    finally
      narrow.Style  := na_style ;
      narrow.Color1 := na_cl1 ;
      narrow.Color2 := na_cl2 ;
      narrow.Path   := na_path ;
    end ;
  end ;

  procedure TGIS_PrintManager.draw_control(
    const _rect    : TRect ;
    const _control : IGIS_PrintableControl ;
    const _scale   : Double
  ) ;
  begin
    if not assigned( _control ) then exit ;
    if not ( _control is TGIS_ControlScale ) and
       not ( _control is TGIS_ControlNorthArrow ) and
       not ( _control is TGIS_ControlLegend )
    then exit ;
    if _control is TGIS_ControlScale then begin
      draw_scale( _rect, _control, _scale,
                  GIS_PARAM_NIL, GIS_PARAM_NIL,
                  GIS_PARAM_NIL, GIS_PARAM_NIL,
                  GIS_PARAM_NIL, GIS_PARAM_NIL ) ;
    end;
    if _control is TGIS_ControlNorthArrow then begin
      draw_northarrow( _rect, _control, _scale,
                       GIS_PARAM_NIL, GIS_PARAM_NIL,
                       GIS_PARAM_NIL, GIS_PARAM_NIL ) ;
    end;
    if _control is TGIS_ControlLegend then begin
      draw_legend( _rect, _control, _scale,
                   GIS_PARAM_NIL, GIS_PARAM_NIL,
                   GIS_PARAM_NIL, GIS_PARAM_NIL,
                   GIS_PARAM_NIL, GIS_PARAM_NIL ) ;
    end;
  end ;

  procedure TGIS_PrintManager.draw_graphic(
    const _rect    : TRect ;
    const _graphic : TGIS_TemplateGraphic ;
    const _path    : String
  ) ;
  var
    w, h  : Integer ;
    sym   : TGIS_SymbolAbstract ;
    grph  : TPicture ;
    ext   : String ;
    cache : TGIS_Bitmap ;

    procedure draw_on_canvas ;
    var
      bmp : TGIS_Bitmap ;
      b   : TGIS_GdipBitmap ;
    begin
      if assigned( oPrinter.Graphics ) then begin
        bmp := TGIS_Bitmap.Create ;
        bmp.LoadFromBitmap( cache.NativeBitmap, '' ) ;
        b := TGIS_GdipBitmap.Create( bmp ) ;
        try
          oPrinter.Graphics.DrawImage( b, _rect, 0, 0, _rect.Width, _rect.Height ) ;
        finally
          FreeObject( b ) ;
        end;
        FreeObject( bmp );
      end else
        oPrinter.Canvas.Draw(
          _rect.Left,
          _rect.Top,
          VCL.Graphics.TBitmap(cache.NativeBitmap),
          255
        ) ;
    end ;

    procedure draw_graphic_using_renderer ;
    var
      drawn : Boolean ;
      ctx   : TGIS_RendererContext ;
      rnd   : TGIS_RendererAbstract ;
      f     : Double ;
    begin
      drawn := false ;
      ctx := TGIS_RendererContext.Create ;
      try
        rnd := TGIS_RendererVclGdiPlus.Create ;

        cache := TGIS_Bitmap.Create( w, h ) ;
        ctx.AssignBaseMap( cache, True ) ;

        rnd.CreateContext( Self, oViewer, ctx, Point( 0, 0 ), w, h, iDpi, 100 ) ;
        try
          rnd.PrepareDraw ;

          if assigned( _graphic ) then
            //draw template graphic
            drawn := _graphic.Draw( rnd, Rect( 0, 0, w, h ) )
          else if assigned( sym ) then begin
            //draw symbol
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
        if drawn then
          draw_on_canvas ;
        FreeObject( ctx ) ;
      end ;
    end ;

  begin
    try
      if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
      w := _rect.Width ;
      h := _rect.Height ;

      if not assigned( _graphic ) then begin
        //special case
        ext := UpperCase( GetFileExt( _path ) ) ;
        if ( ext = '.WMF' ) or ( ext = '.EMF' ) then begin
          grph := TPicture.Create ;
          try
            grph.LoadFromFile( _path );
            oPrinter.Canvas.StretchDraw(
              Rect( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom),
                    grph.Graphic ) ;
          finally
            FreeObject( grph ) ;
          end ;
          exit ;
        end ;
      end ;

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
    const _rect     : TRect  ;
    const _text     : String ;
    const _name     : String ;
    const _style    : TGIS_FontStyles ;
    const _size     : Integer ;
    const _color    : TGIS_Color ;
    const _align    : TGIS_LabelAlignment ;
    const _bgColor  : TGIS_Color  ;
    const _bgWidth  : Integer
  ) ;
  var
    pt : TPoint ;
    old_bkmode : Integer ;
    txt : String ;
  begin
    oPrinter.Canvas.Brush.Style := bsClear ;

    oPrinter.Canvas.Font.Color := VCLColor( _color ) ;
    oPrinter.Canvas.Font.Name  := _name ;
    oPrinter.Canvas.Font.Size  := _size ;
    oPrinter.Canvas.Font.Style := VCLFontStyle( _style ) ;

    pt.Y := _rect.Top ;

    case _align of
      TGIS_LabelAlignment.Center :
        begin
          pt.X := _rect.Left + ( _rect.Right - _rect.Left ) div 2 ;
          Winapi.Windows.SetTextAlign( oPrinter.Canvas.Handle, TA_TOP or TA_CENTER ) ;
        end ;
      TGIS_LabelAlignment.RightJustify :
        begin
          pt.X := _rect.Right ;
          Winapi.Windows.SetTextAlign( oPrinter.Canvas.Handle, TA_TOP or TA_RIGHT ) ;
        end ;
      else
        begin
          pt.X := _rect.Left ;
          Winapi.Windows.SetTextAlign( oPrinter.Canvas.Handle, TA_TOP or TA_LEFT ) ;
        end ;
    end ;

    if _bgColor <> TGIS_Color.None then begin
      oPrinter.Canvas.Brush.Style := bsSolid ;
      oPrinter.Canvas.Brush.Color := VCLColor( _bgColor ) ;
      oPrinter.Canvas.Pen.Width := 1 ;
      oPrinter.Canvas.Pen.Style := psSolid ;
      oPrinter.Canvas.Pen.Color := VCLColor( _bgColor ) ;
      oPrinter.Canvas.Rectangle( _rect ) ;
    end ;

    if _bgWidth > 0 then begin
      oPrinter.Canvas.Pen.Width := _bgWidth ;
      oPrinter.Canvas.Pen.Style := psSolid ;
      oPrinter.Canvas.Rectangle( _rect ) ;
    end ;

    old_bkmode := Winapi.Windows.SetBkMode( oPrinter.Canvas.Handle, TRANSPARENT ) ;
    txt := GisExpandLabel( _text, FTemplate.GetField ) ;
    Winapi.Windows.ExtTextOut( oPrinter.Canvas.Handle, pt.X, pt.Y, 0, @_rect,
                               PChar(txt), Length( txt ), nil ) ;
    Winapi.Windows.SetBkMode( oPrinter.Canvas.Handle, old_bkmode ) ;
  end ;

  procedure TGIS_PrintManager.draw_frame(
    const _rect  : TRect ;
    const _color : TGIS_Color ;
    const _width : Integer
  ) ;
  var
    n1 : Integer ;
    n2 : Integer ;
    r  : TRect ;
  begin
    if ( _color <> TGIS_Color.None ) and
       ( _width > 0 ) then begin
      // draw frame always inside rect
      oPrinter.Canvas.Brush.Style := bsClear ;
      oPrinter.Canvas.Pen.Style := psSolid ;
      oPrinter.Canvas.Pen.Color := VCLColor(_color) ;
      oPrinter.Canvas.Pen.Width := _width ;
      n1 := _width div 2 ;
      n2 := ( _width - 1 ) div 2 ;
      r := Rect( _rect.Left + n1,
                 _rect.Top + n1,
                 _rect.Right - n2 + 1,
                 _rect.Bottom - n2 + 1
               ) ;
      oPrinter.Canvas.Rectangle( r ) ;
    end;
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
      b : TGIS_GdipBitmap ;
      r : TRect ;
      w1, h1 : Integer ;
    begin
      w1 := tilesz.X ;
      h1 := tilesz.y ;

      if rct.Left + _x + w1 > rct.Right then
        w1 := w1 - ( ( rct.Left + _x + w1 ) - rct.Right  ) ;
      if rct.Top  + _y + h1  > rct.Bottom then
        h1 := h1 - ( ( rct.Top  + _y + h1 ) - rct.Bottom ) ;

      r := Rect( rct.Left + _x,
                 rct.Top  + _y,
                 rct.Left + _x + w1,
                 rct.Top  + _y + h1 ) ;
      if assigned( oPrinter.Graphics ) then begin
        b := TGIS_GdipBitmap.Create( bmp ) ;
        try
          oPrinter.Graphics.DrawImage( b, r, 0, 0, w1, h1 ) ;
        finally
          FreeObject( b ) ;
        end;
      end else
        oPrinter.Canvas.CopyRect( r,
                                  VCL.Graphics.TBitmap(bmp.NativeBitmap).Canvas,
                                  Rect( 0, 0, w1, h1 ) );
    end;

  begin
    if not assigned( oViewer ) then exit ;
    wnd := _viewer as TGIS_ViewerWnd ;
    w := wnd.Width ;
    h := wnd.Height ;
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
      oRenderer := TGIS_RendererVclGdiPlus.Create ;
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
    const _rect       : TRect ;
    const _viewer     : IGIS_Viewer ;
    const _extent     : TGIS_Extent ;
    const _background : Boolean ;
    var   _scale      : Double ;
    var   _rextent    : TGIS_Extent ;
          _print      : Boolean
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

    procedure print_map(
      const _rect : TRect ;
      const _dest : TRect ;
      const _str  : String
    ) ;
    var
      w, h        : Integer ;
      ctx         : TGIS_RendererContext ;
      tcache      : TGIS_Bitmap  ;
      cacheDst    : VCL.Graphics.TBitmap ;
      transparent : Boolean ;
      b           : TGIS_GdipBitmap ;
      ctx_labels  : TGIS_Bitmap ;
      ctx_labels_topmost : TGIS_Bitmap ;
      old_OverlappedExtentMargin : Integer ;

      procedure prepareCacheDst ;
      begin
        if ( _rect.Width <> _dest.Width ) or
           ( _rect.Height <> _dest.Height ) then begin
          cacheDst := VCL.Graphics.TBitmap.Create ;
          cacheDst.Width  := _dest.Width ;
          cacheDst.Height := _dest.Height ;
          cacheDst.PixelFormat :=
            VCL.Graphics.TBitmap(TGIS_Bitmap(tcache).NativeBitmap).PixelFormat ;
          cacheDst.Canvas.Draw(
            0, 0,
            VCL.Graphics.TBitmap(TGIS_Bitmap(tcache).NativeBitmap)
          ) ;
        end ;
      end ;

    begin
      if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
      w := _rect.Width ;
      h := _rect.Height ;

      ctx_labels := nil ;
      ctx_labels_topmost := nil ;
      tcache := nil ;
      transparent := not _background ;
      ctx := TGIS_RendererContext.Create ;
      try
        try

          // always
          oRenderer := TGIS_RendererVclGdiPlus.Create ;

          tcache := TGIS_Bitmap.Create( w, h, oRenderer.BitmapFactory ) ;
          if not transparent then
            tcache.Clear( _viewer.Color ) ;

          ctx.AssignBaseMap  ( tcache, False ) ;

          ctx.AssignSelection( nil,    True ) ;
          ctx.AssignCharts   ( nil,    True ) ;
          ctx.AssignLabels   ( nil,    True ) ;

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
            VCL.Graphics.TBitmap(tcache.NativeBitmap).Canvas.Lock ;

            if Assigned( ctx.Selection ) then
              TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
                TGIS_Bitmap(ctx.Selection).NativeBitmap,
                TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                _viewer.SelectionTransparency,
                False
              ) ;
            if Assigned( ctx.Charts ) then
              if ( oPrinter is TGIS_PrinterPreview ) and transparent then begin
                TGIS_RendererVclAbstract( oRenderer ).stretchBitmapFast(
                  TGIS_Bitmap(ctx.Charts).NativeBitmap,
                  TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                  Rect( 0, 0, w, h )
                ) ;
              end else begin
                TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
                  TGIS_Bitmap(ctx.Charts).NativeBitmap,
                  TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                  100,
                  False
                ) ;
              end ;
            if Assigned( ctx.Labels ) then begin
              if not metPaintLabelsOnTop then begin
                if ( oPrinter is TGIS_PrinterPreview ) and transparent then begin
                  TGIS_RendererVclAbstract( oRenderer ).stretchBitmapFast(
                    TGIS_Bitmap(ctx.Labels).NativeBitmap,
                    TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                    Rect( 0, 0, w, h )
                  ) ;
                end else begin
                  TGIS_RendererVclAbstract( oRenderer ).blendBitmaps(
                    TGIS_Bitmap(ctx.Labels).NativeBitmap,
                    TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                    100,
                    False
                  ) ;
                end ;
              end else begin
                ctx_labels := TGIS_Bitmap.Create( w, h ) ;
                ctx_labels.Assign( TGIS_Bitmap( ctx.Labels ) ) ;
              end ;
            end ;

            VCL.Graphics.TBitmap(tcache.NativeBitmap).Canvas.UnLock ;

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

              if Assigned( ctx.Labels ) then begin
                if not metPaintLabelsOnTop then begin
                  //blend bitmaps
                  VCL.Graphics.TBitmap(tcache.NativeBitmap).Canvas.Lock ;

                  TGIS_RendererVclAbstract(oRenderer).blendBitmaps(
                    TGIS_Bitmap(ctx.Labels).NativeBitmap,
                    TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                    100,
                    False
                  );

                  VCL.Graphics.TBitmap(tcache.NativeBitmap).Canvas.UnLock ;
                end else begin
                  ctx_labels_topmost := TGIS_Bitmap.Create( w, h ) ;
                  ctx_labels_topmost.Assign( TGIS_Bitmap( ctx.Labels ) ) ;
                end ;
              end;

              oRenderer.ReleaseContext ;

            end;

            if assigned( ctx_labels ) then begin
              // labels on top
              if ( oPrinter is TGIS_PrinterPreview ) and transparent then begin
                TGIS_RendererVclAbstract( oRenderer ).stretchBitmapFast(
                  TGIS_Bitmap(ctx_labels).NativeBitmap,
                  TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                  Rect( 0, 0, w, h )
                ) ;
              end else begin
                TGIS_RendererVclAbstract( oRenderer ).blendBitmaps(
                  TGIS_Bitmap(ctx_labels).NativeBitmap,
                  TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                  100,
                  False
                ) ;
              end ;
              FreeObject( ctx_labels ) ;
            end ;

            if assigned( ctx_labels_topmost ) then begin
              // labels on top
              if ( oPrinter is TGIS_PrinterPreview ) and transparent then begin
                TGIS_RendererVclAbstract( oRenderer ).stretchBitmapFast(
                  TGIS_Bitmap(ctx_labels_topmost).NativeBitmap,
                  TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                  Rect( 0, 0, w, h )
                ) ;
              end else begin
                TGIS_RendererVclAbstract( oRenderer ).blendBitmaps(
                  TGIS_Bitmap(ctx_labels_topmost).NativeBitmap,
                  TGIS_Bitmap(ctx.BaseMap).NativeBitmap,
                  100,
                  False
                ) ;
              end ;
              FreeObject( ctx_labels_topmost ) ;
            end ;

            _viewer.OverlappedExtentMargin := old_OverlappedExtentMargin ;
          end ;
        finally
          cacheDst := VCL.Graphics.TBitmap(tcache.NativeBitmap) ;
          try
            if oPrinter is TGIS_PrinterPreview then begin
              prepareCacheDst ;
              if not transparent then
                fix_alpha( cacheDst ) ;
              TGIS_RendererVclAbstract( oRenderer ).stretchBitmapFast(
                cacheDst,
                TGIS_PrinterPreview( oPrinter ).Bitmap,
                _dest
              ) ;
            end else begin
              if transparent then begin
                if assigned( oPrinter.Graphics ) then begin
                  b := TGIS_GdipBitmap.Create( tcache ) ;
                  try
                    oPrinter.Graphics.DrawImage( b, _dest, 0, 0, _dest.Width, _dest.Height ) ;
                  finally
                    FreeObject( b ) ;
                  end;
                end else
                  oPrinter.Canvas.Draw( _rect.Left, _rect.Top,
                                        VCL.Graphics.TBitmap(cacheDst),
                                        255 ) ;
              end else begin
                prepareCacheDst ;
                cacheDst.PixelFormat := pf24bit ;
                VCLCanvasDrawBitmapEx( oPrinter.Canvas, _dest, cacheDst );
              end ;
            end ;
          finally
            if cacheDst <> tcache.NativeBitmap then
              FreeObject( cacheDst ) ;
          end;
          FreeObject( oRenderer ) ;
        end ;
      finally
        FreeObject( ctx ) ;
        FreeObject( tcache ) ;
      end;
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
          rectSize := Point( TileSize, TileSize ) ;
          // new viewer extent
          ext.XMin := ext_page.XMin + i * (ext_tile.XMax - ext_tile.XMin) ;
          ext.XMax := ext.XMin + (ext_tile.XMax - ext_tile.XMin) ;
          ext.YMax := ext_page.YMax - j * (ext_tile.YMax - ext_tile.YMin) ;
          ext.YMin := ext.YMax - (ext_tile.YMax - ext_tile.YMin) ;

          _viewer.VisibleExtent := ext ;
          // force the same scale for all tiles
          _viewer.TemporaryScaleInternal := _scale ;

          // print a tile on printer
          print_map( Rect( _rect.Left + i * FTileSize,
                           _rect.Top  + j * FTileSize,
                           _rect.Left + ( i + 1 ) * FTileSize,
                           _rect.Top  + ( j + 1 ) * FTileSize
                     ),
                     Rect( _rect.Left + i * FTileSize,
                           _rect.Top  + j * FTileSize,
                           Min( _rect.Right,
                                _rect.Left + ( i + 1 ) * FTileSize ),
                           Min( _rect.Bottom,
                                _rect.Top  + ( j + 1 ) * FTileSize )
                     ),
                     IntToStr(j)+IntToStr(i) ) ;
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
    init_locally    : Boolean ;
    pageno          : Integer ;
    lastpage        : Boolean ;
    translated      : Boolean ;
    {$IFDEF GIS_XDK}
      xdk_translated: Boolean ;
    {$ENDIF}

    procedure print_txt(
      const _rect : TRect ;
      const _txt  : String
    ) ;
    var
      x : Integer ;
      y : Integer ;
    begin
      with _rect do begin
        x := Left + ( (Right - Left) - oPrinter.Canvas.TextWidth ( _txt ) ) div 2 ;
        y := Top  + ( (Bottom - Top) - oPrinter.Canvas.TextHeight( _txt ) ) div 2 ;
      end ;
      oPrinter.Canvas.TextOut( x, y, _txt ) ;
    end ;

    procedure process_standard_printing ;
    var
      t, h : Integer ;
      fnt  : TFont   ;
      tw, th : Integer ;
      x : Integer ;
      y : Integer ;
      rtitle    : TRect   ;
      rsubtitle : TRect   ;
      rfooter   : TRect   ;
      rcontent  : TRect   ;
      rextent   : TGIS_Extent ;
    begin
      t := 0 ;
      if not IsStringEmpty( Title ) then begin
        fnt := VCLFont( TitleFont ) ;
        try
          oPrinter.Canvas.Font.Assign( fnt ) ;
        finally
          FreeObject( fnt ) ;
        end ;
        tw := oPrinter.Canvas.TextWidth ( Title ) ;
        th := oPrinter.Canvas.TextHeight( Title ) ;
        h := th * 110 div 100 ;
        rtitle := Rect( 0, t, oPrinter.PrintArea.Width, t+h ) ;
        with rtitle do begin
          x := Left + ( (Right - Left) - tw ) div 2 ;
          y := Top  + ( (Bottom - Top) - th ) div 2 ;
        end ;
        draw_text( Rect( x, y, x+tw, y+th ), Title,
                   TitleFont.Name, TitleFont.Style,
                   TitleFont.Size, TitleFont.Color,
                   TGIS_LabelAlignment.LeftJustify,
                   TGIS_Color.None, 0
                  ) ;
      end else
        h := 0 ;

      t := t + h ;
      if not IsStringEmpty( Subtitle ) then begin
        fnt := VCLFont( SubtitleFont ) ;
        try
          oPrinter.Canvas.Font.Assign( fnt ) ;
        finally
          FreeObject( fnt ) ;
        end ;
        tw := oPrinter.Canvas.TextWidth ( Subtitle ) ;
        th := oPrinter.Canvas.TextHeight( Subtitle ) ;
        h := th * 110 div 100 ;
        rsubtitle := Rect( 0, t, oPrinter.PrintArea.Width, t+h ) ;
        with rsubtitle do begin
          x := Left + ( (Right - Left) - tw ) div 2 ;
          y := Top  + ( (Bottom - Top) - th ) div 2 ;
        end ;
        draw_text( Rect( x, y, x+tw, y+th ), Subtitle,
                   SubtitleFont.Name, SubtitleFont.Style,
                   SubtitleFont.Size, SubtitleFont.Color,
                   TGIS_LabelAlignment.LeftJustify,
                   TGIS_Color.None, 0
                  ) ;
      end else
        h := 0 ;

      t := t + h ;
      if not IsStringEmpty( Footer ) then begin
        fnt := VCLFont( FooterFont ) ;
        try
          oPrinter.Canvas.Font.Assign( fnt ) ;
        finally
          FreeObject( fnt ) ;
        end ;
        tw := oPrinter.Canvas.TextWidth ( Footer ) ;
        th := oPrinter.Canvas.TextHeight( Footer ) ;
        h := th * 110 div 100 ;
        rfooter := Rect( 0, oPrinter.PrintArea.Height - h,
                         oPrinter.PrintArea.Width,
                         oPrinter.PrintArea.Height ) ;
        with rfooter do begin
          x := Left + ( (Right - Left) - tw ) div 2 ;
          y := Top  + ( (Bottom - Top) - th ) div 2 ;
        end ;
        draw_text( Rect( x, y, x+tw, y+th ), Footer,
                   FooterFont.Name, FooterFont.Style,
                   FooterFont.Size, FooterFont.Color,
                   TGIS_LabelAlignment.LeftJustify,
                   TGIS_Color.None, 0
                  ) ;
      end else
        h := 0 ;

      rcontent := Rect( 0, t,
                        oPrinter.PrintArea.Width,
                        oPrinter.PrintArea.Height - h ) ;
      if assigned( oViewer ) then
        draw_map( rcontent, oViewer, old_extent, True,
                  oViewerScale, rextent, True ) ;

    end ;

  begin
    old_incpnt := False ;
    pageno := oPrinter.PageNumber ;

    if assigned( oViewer ) then begin
      // store context
      old_extent := GisCommonExtent( oViewer.Extent, oViewer.VisibleExtent ) ;
      old_state  := oViewer.StorePaintState ;
      old_incpnt := oViewer.IncrementalPaint ;

      oViewer.IncrementalPaint := False ;
    end ;

    try
      started_locally := False ;
      init_locally := False ;
      if not oPrinter.Printing then begin
        oPrinter.BeginDoc ;
        started_locally := True ;
      end else
        init_locally := oPrinter.BeginDocEx ;
      oPrinter.PageNumber := pageno ;
      iDpi := oPrinter.Canvas.Font.PixelsPerInch ;
      try
        if not oPrinter.Printing then exit ;

        while true do begin
          translated := False ;

          lastpage := True  ;

          if Assigned( FOnBeforePrintPage ) then
            {$IFDEF GIS_XDK}
              FOnBeforePrintPage( xdk_translated, Self, Self, lastpage ) ;
            {$ELSE}
              FOnBeforePrintPage( Self, Self, lastpage ) ;
            {$ENDIF}

          if Assigned( FOnPrintPage ) then begin
            translated := True ;
            inPrintPage := true ;
            {$IFDEF GIS_XDK}
              FOnPrintPage( translated, Self, Self, lastpage ) ;
            {$ELSE}
              FOnPrintPage( Self, Self, lastpage ) ;
            {$ENDIF}

            inPrintPage := false ;
          end ;

          if not translated then begin
            if not assigned( FTemplate ) then
              process_standard_printing
            else
              processTemplatePrinting( iDpi, oPrinter.PrintArea,
                                       oPrinter.PageSize.X,
                                       oPrinter.PageSize.Y ) ;
          end;

          if Assigned( FOnAfterPrintPage ) then
            {$IFDEF GIS_XDK}
              FOnAfterPrintPage( xdk_translated, Self, Self, lastpage ) ;
            {$ELSE}
              FOnAfterPrintPage( Self, Self, lastpage ) ;
            {$ENDIF}

          if oPrinter.VirtualPrinter then begin
            if lastpage then oPrinter.PageNumber := 9999 ;
            break ;
          end ;

          if lastpage then break ;
          if oPrinter.Aborted then break ;

          oPrinter.NewPage ;
        end ;
      finally
        if started_locally then begin
          if oPrinter.Printing then begin
            if not oPrinter.Aborted
            then oPrinter.EndDoc
            else oPrinter.EndDocEx( True ) ;
          end ;
        end
        else oPrinter.EndDocEx( init_locally ) ;
        if oPrinter.VirtualPrinter then
          fix_alpha( TGIS_PrinterPreview( oPrinter ).Bitmap );
      end ;
    finally
      if assigned( oViewer ) then begin
        // restore context
        oViewer.RestorePaintState( old_state ) ;
        oViewer.IncrementalPaint := old_incpnt ;
      end ;
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
    const _viewer   : IGIS_Viewer ;
    const _printer  : TObject
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
  var
    old_printer : Boolean ;

  begin
    if assigned( _viewer ) then begin
      if GisIsNoWorld( _viewer.Extent ) then exit ;
      if _viewer.InPaint then exit ;
      _viewer.WaitForBackgroundProcesses ;
      _viewer.ReParentLock;
    end ;

    old_printer := False ;
    if _printer = nil then begin
      if not assigned( oPrinter ) then
        oPrinter := TGIS_Printer.Create( VCL.Printers.Printer )
      else
        old_printer := True ;
    end else begin
      if not ( _printer is TGIS_printer ) then exit ;
      oPrinter := TGIS_Printer( _printer ) ;
    end ;

    try
      if not oPrinter.VirtualPrinter and
         not assigned( oPrinter.Printer ) then exit ;

      if not assigned( _viewer ) then begin
        if _printer is TGIS_PrinterTemplate then begin
          // template designer does not set the viewer
          oViewer := nil ;
          oViewerRenderer := nil ;
        end else
          if not assigned( oViewer ) then exit ;
      end else begin
        oViewer := _viewer ;
        oViewerRenderer := _viewer.GetRenderContext ;
      end ;
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
          oPrinter.Title := GetFileName( Application.ExeName ) ;

        oViewerScale := _scale ;
        printInternal ;
        _scale := oViewerScale ;

      finally
        if assigned( AfterPrintEvent ) then
          AfterPrintEvent( Self, oPrinter.Printer ) ;
      end ;

    finally
      if assigned( _viewer ) then begin
        oViewer := nil ;
        oViewerRenderer := nil ;
        _viewer.ReParentUnlock;
      end ;
      if not assigned( _printer ) then begin
        if not old_printer then
          FreeObject( oPrinter ) ;
      end else
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
    if not inPrintPage then exit ;
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
  end;

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
    if not inPrintPage then exit ;
    draw_map( _rect, _viewer, _extent, True, _scale, _rextent, True ) ;
  end;

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
    if not inPrintPage then exit ;
    draw_control( _rect, _control, _scale ) ;
  end ;

  procedure TGIS_PrintManager.DrawGraphic(
    const _path : String ;
    const _rect : TRect
  ) ;
  begin
    if not inPrintPage then exit ;
    draw_graphic( _rect, nil, _path ) ;
  end;

  procedure TGIS_PrintManager.DrawGraphic(
    const _graphic : TGIS_TemplateGraphic ;
    const _rect    : TRect
  ) ;
  begin
    if not inPrintPage then exit ;
    draw_graphic( _rect, _graphic, '' ) ;
  end ;

  function TGIS_PrintManager.SetViewer(const _viewer: TObject): TObject;
  begin
    Result := nil ;
  end;

  function TGIS_PrintManager.GetViewer: TObject;
  begin
    Result := nil ;
  end;

//==================================== END =====================================
end.


