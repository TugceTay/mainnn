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
  Print Preview component. Form.
}

unit Lider.CG.GIS.FMX.GeoControlPrintPreview ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlPrintPreview"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.Types,
  System.SysUtils,
  System.Actions,
  System.Classes,
  System.UITypes,

  FMX.ActnList,
  FMX.Controls,
  FMX.Dialogs,
  FMX.Forms,
  FMX.Graphics,
  FMX.Printer,
  FMX.StdCtrls,
  FMX.Types,
  FMX.Objects,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoPrintManagerAbstract,
  Lider.CG.GIS.GeoPrintPreviewHelperAbstract,

  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.FMX.GeoPrinters,
  FMX.Controls.Presentation;

type

  /// <summary>
  ///   Helper class for managing print preview.
  /// </summary>
  TGIS_PrintPreviewFactoryFMX = class ( TGIS_PrintPreviewFactory )
    public

      /// <inheritdoc/>
      function  IsPrinter : Boolean ; override;

      /// <inheritdoc/>
      procedure Preview   ( const _viewer       : IGIS_Viewer ;
                            const _printManager : TGIS_PrintManagerAbstract ;
                            const _customPage   : String  ;
                            var   _scale        : Double  ;
                            const _caption      : String  ;
                            const _left         : Integer ;
                            const _top          : Integer ;
                            const _width        : Integer ;
                            const _height       : Integer ;
                            const _state        : TWindowState ;
                            const _help         : TGIS_HelpEvent
                          ) ; override;
  end ;

  /// <summary>
  ///   Control for managing print preview.
  /// </summary>
  [ComponentPlatformsAttribute( pfidAllDesktops )]
  TGIS_ControlPrintPreview = class( TRectangle )

    private // property internal values
      FGIS_Viewer : IGIS_Viewer ;
      FIsLastPage : Boolean ;

    private  // control components
      pnlPage     : TRectangle ;
      pnlShadow   : TRectangle ;
      imgPreview  : TImage ;

    private  // other private variables
      pageWidth      : Integer ;
      pageHeight     : Integer ;
      pageSize       : TPoint  ;
      pagePrintArea  : TRect   ;
      firstPaint     : Boolean ;

    private // property access routines

      function fget_IsPrinter : Boolean ;

    protected

      /// <summary>
      ///   Internal method for preview selected page.
      /// </summary>
      /// <param name="_pageNumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_printer">
      ///   virtual printer
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <returns>
      ///   Title string.
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function previewInternal( const _pageNumber   : Integer ;
                                const _printManager : TGIS_PrintManagerAbstract ;
                                const _printer      : TGIS_PrinterPreview ;
                                var   _scale        : Double
                              ) : String ;

    public

      /// <summary>
      ///   Create an instance.
      ///   See Embarcadero help for TRectangle for more details.
      /// </summary>
      /// <param name="_owner">
      ///   See Embarcadero help for TRectangle.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create    ( _owner : TComponent
                            ) ; override;

    protected

      /// <summary>
      ///   Standard Resize override.
      ///   See Embarcadero help for TRectangle for more details.
      /// </summary>
      procedure Resize    ; override;

      /// <summary>
      ///   Standard Paint override.
      ///   See Embarcadero help for TRectangle for more details.
      /// </summary>
      procedure Paint     ; override;

    public

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pagenumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <returns>
      ///   title string
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview     ( const _pagenumber   : Integer ;
                              const _printManager : TGIS_PrintManagerAbstract
                            ) : String ; overload ; virtual ;

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pagenumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <returns>
      ///   title string
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview     ( const _pagenumber   : Integer ;
                              const _printManager : TGIS_PrintManagerAbstract ;
                              var   _scale        : Double
                            ) : String ; overload ; virtual ;

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pagenumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_customPage">
      ///   default page orientation: "Landscape" or "Portrait"
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <returns>
      ///   title string
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview     ( const _pagenumber   : Integer ;
                              const _printManager : TGIS_PrintManagerAbstract ;
                              const _customPage   : String ;
                              var   _scale        : Double
                            ) : String ; overload ; virtual ;

      /// <summary>
      ///   Print document.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Print       ;

    public // properties new for this class

      /// <summary>
      ///   True, if last page in preview was reached.
      /// </summary>
      property IsLastPage : Boolean read FIsLastPage ;

      /// <summary>
      ///   Check if any printer exists.
      /// </summary>
      /// <returns>
      ///   True, if any printer exists.
      /// </returns>
      property IsPrinter : Boolean read fget_IsPrinter ;

    published // properties new for this class

      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer read FGIS_Viewer write FGIS_Viewer ;

    published // properties derived from base class

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property Action;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property Align ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property Anchors ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property Enabled ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property Hint ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property PopupMenu ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property TabStop  ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property TabOrder ;

      /// <summary>
      ///   See documentation for TRectangle in Delphi help.
      /// </summary>
      property Visible ;
  end ;

  /// <summary>
  ///   Print preview form. Only for internal use of TatukGIS.
  /// </summary>
  [ComponentPlatformsAttribute(
       pidWin32
    or pidWin64
    or pidOSX32
    {$IFDEF LEVEL_RX103_FMX}
      or pidOSX64
    {$ENDIF}
  )]
  TGIS_ControlPrintPreviewForm = class(TForm)
    dlgPrinterSetup: TPrinterSetupDialog;
    dlgPrint: TPrintDialog;
    stsBar: TStatusBar;
    tlbMain: TPanel;
    btnPrint: TSpeedButton;
    btnPrinterSetup: TSpeedButton;
    btnPrevPage: TSpeedButton;
    btnNextPage: TSpeedButton;
    btnHelp: TSpeedButton;
    stsLabel: TLabel;
    rctPageNumber: TRectangle;
    lblPageNumber: TLabel;
    imgPrint: TImage;
    imgPrinterSetup: TImage;
    imgPrevPage: TImage;
    imgNextPage: TImage;
    imgHelp: TImage;

      /// <summary>
      ///   Action "Print" handler.
      /// </summary>
      procedure btnPrintExecute       ( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Printer Setup" handler.
      /// </summary>
      procedure btnPrinterSetupExecute( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Previous Page" handler.
      /// </summary>
      procedure btnPrevPageExecute    ( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Next Page" handler.
      /// </summary>
      procedure btnNextPageExecute    ( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Help" handler.
      /// </summary>
      procedure btnHelpExecute        ( _sender      : TObject ) ;

      /// <summary>
      ///   Action list update.
      /// </summary>
      procedure actListUpdate         ;

      procedure FormCreate            ( _sender      : TObject ) ;
      procedure FormShow              ( _sender      : TObject ) ;

      /// <summary>
      ///   KeyDown handler.
      /// </summary>
      procedure FormKeyDown           (     _sender  : TObject ;
                                        var _key     : Word    ;
                                        var _keyChar : Char    ;
                                            _shift   : TShiftState
                                      ) ;

    private // property access routines
      function  fget_GIS_Viewer    : IGIS_Viewer ;
      procedure fset_GIS_Viewer    ( const _value : IGIS_Viewer ) ;
      procedure fset_HelpEvent     ( const _value : TGIS_HelpEvent ) ;

    private
      oPrintManager : TGIS_PrintManagerAbstract ;
      customPage    : String ;
      scale         : Double ;
      pnlPreview    : TGIS_ControlPrintPreview ;
      pageNumber    : Integer ;
      FOnHelp       : TGIS_HelpEvent ;

    private

      procedure initSelf ;
      procedure addResBmp( const _name   : String ;
                           const _parent : TControl
                         ) ;

    public

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TCustomForm in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create    ( _owner : TComponent
                            ) ; override;

    public

      /// <summary>
      ///   Show preview form. Start previewing on page number.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_customPage">
      ///   custom page format
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview ( const _printManager : TGIS_PrintManagerAbstract ;
                          const _customPage   : String ;
                          var   _scale        : Double
                        ) ;

    public // properties

      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer    read  fget_GIS_Viewer
                                           write fset_GIS_Viewer ;

    published // events

      /// <event/>
      /// <summary>
      ///   Event fired upon F1 key. Passed to any child form.
      /// </summary>
      property HelpEvent : TGIS_HelpEvent read  FOnHelp
                                          write fset_HelpEvent ;
  end;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

{$R Lider.CG.GIS.FMX.GeoControlPrintPreview_16x16.RES}


uses
  System.Math,
  System.UIConsts,
  Lider.CG.GIS.FMX.GeoPrintManager,
  Lider.CG.GIS.FMX.GeoViewerWnd;

//==============================================================================
// TGIS_PrintPreviewFactoryFMX
//==============================================================================

  function  TGIS_PrintPreviewFactoryFMX.IsPrinter
    : Boolean ;
  begin
    try
      Result := ( Printer.Count > 0 ) and assigned( Printer.ActivePrinter ) ;
    except
      Result := False ;
    end ;
  end ;

  procedure TGIS_PrintPreviewFactoryFMX.Preview(
    const _viewer       : IGIS_Viewer ;
    const _printManager : TGIS_PrintManagerAbstract ;
    const _customPage   : String  ;
    var   _scale        : Double  ;
    const _caption      : String  ;
    const _left         : Integer ;
    const _top          : Integer ;
    const _width        : Integer ;
    const _height       : Integer ;
    const _state        : TWindowState ;
    const _help         : TGIS_HelpEvent
  ) ;
  var
    frm : TGIS_ControlPrintPreviewForm ;
  begin
    frm := TGIS_ControlPrintPreviewForm.Create( nil ) ;
    try
      frm.Caption     := _caption ;
      frm.GIS_Viewer  := _viewer  ;
      frm.HelpEvent   := _help    ;
      frm.Left        := _left    ;
      frm.Top         := _top     ;
      frm.Width       := _width   ;
      frm.Height      := _height  ;
      frm.Position    := TFormPosition.Designed ;
      frm.WindowState := _state   ;

      if frm.Position = TFormPosition.Designed then begin
       frm.Left := RoundS( Screen.Width  - frm.Width  ) div 2 ;
       frm.Top  := RoundS( Screen.Height - frm.Height ) div 2 ;
      end ;
      frm.Preview( _printManager, _customPage, _scale ) ;
    finally
      frm.HelpEvent := nil ;
      FreeObject( frm ) ;
    end ;
  end ;

//==============================================================================
// TGIS_ControlPrintPreview
//==============================================================================

  constructor TGIS_ControlPrintPreview.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    Fill.Color       := TAlphaColorRec.Gray ;
    Width            := 100    ;
    Height           := 100    ;

    pnlShadow := TRectangle.Create( self ) ;
    with pnlShadow do begin
      Parent     := Self ;
      Fill.Color := TAlphaColorRec.Black ;
      Width      := 1    ;
      Height     := 1    ;
      Left       := -10  ;
    end ;

    pnlPage := TRectangle.Create( Self ) ;
    with pnlPage do begin
      Parent     := Self  ;
      Fill.Color := TAlphaColorRec.White ;
      Sides      := []    ;
      Width      := 1     ;
      Height     := 1     ;
      Left       := -10   ;
    end ;

    imgPreview := TImage.Create( pnlPage ) ;
    imgPreview.Parent := pnlPage ;

    pageSize := Point( 850, 1100 )  ;
    firstPaint := True ;
  end ;


  function TGIS_ControlPrintPreview.fget_IsPrinter : Boolean ;
  begin
    try
      Result := ( Printer.Count > 0 ) and assigned( Printer.ActivePrinter ) ;
    except
      // if Printer.PrinterIndex = -1 then Delphi will try to set a default printer;
      // if the setting fails an exception will be thrown;
      Result := False ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreview.Resize ;
  const
    SHADOW = 5          ;
    MARGIN = SHADOW + 8 ;
  var
    scale : Double ;
  begin
    if ( pageSize.X = 0 ) or ( pageSize.Y = 0 ) then exit ;

    scale := Min( ( Width  - 2* MARGIN ) / pageSize.X ,
                  ( Height - 2* MARGIN ) / pageSize.Y
                ) ;

    pnlPage.Width  := RoundS( pageSize.X * scale ) ;
    pnlPage.Height := RoundS( pageSize.Y * scale ) ;

    pnlPage.Position.X := ( Width  - pnlPage.Width  ) / 2 ;
    pnlPage.Position.Y := ( Height - pnlPage.Height ) / 2 ;

    pnlShadow.Width   := pnlPage.Width  ;
    pnlShadow.Height  := pnlPage.Height ;
    pnlShadow.Position.Y := pnlPage.Position.Y + SHADOW ;
    pnlShadow.Position.X := pnlPage.Position.X + SHADOW ;

    imgPreview.Width  := RoundS( pageWidth   * scale ) ;
    imgPreview.Height := RoundS( pageHeight  * scale ) ;

    imgPreview.Position.X := RoundS( pagePrintArea.Left * scale ) ;
    imgPreview.Position.Y := RoundS( pagePrintArea.Top  * scale ) ;
  end ;

  procedure TGIS_ControlPrintPreview.Paint ;
  begin
    if firstPaint then begin
      Resize ;
      firstPaint := False ;
    end ;
    inherited ;
  end ;

  function TGIS_ControlPrintPreview.previewInternal(
    const _pageNumber   : Integer ;
    const _printManager : TGIS_PrintManagerAbstract ;
    const _printer      : TGIS_PrinterPreview ;
    var   _scale        : Double
  ) : String ;
  var
    print_manager : TGIS_PrintManager ;
    ts : Integer ;
  begin
    pageWidth      := _printer.PageWidth  ;
    pageHeight     := _printer.PageHeight ;
    pageSize       := _printer.PageSize   ;
    pagePrintArea  := _printer.PrintArea  ;

    _printer.PageNumber := _pagenumber ;

    if assigned( _printManager ) then begin
      ts := _printManager.TileSize ;
      try
        _printManager.TileSize := Max( pageWidth, pageHeight ) ;
        _printManager.Print( FGIS_Viewer, _printer, _scale ) ;
      finally
        _printManager.TileSize := ts ;
      end;
    end else begin
      print_manager := TGIS_PrintManager.Create ;
      try
        print_manager.TileSize := Max( pageWidth, pageHeight ) ;
        print_manager.Print( FGIS_Viewer, _printer, _scale ) ;
      finally
        FreeObject( print_manager ) ;
      end ;
    end;

    Result := _printer.Title ;

    FIsLastPage := ( _printer.PageNumber = 9999 ) or FGIS_Viewer.IsEmpty ;
    imgPreview.Bitmap.Assign( _printer.Bitmap ) ;

    Resize ;
  end ;

  function TGIS_ControlPrintPreview.Preview(
    const _pagenumber   : Integer ;
    const _printManager : TGIS_PrintManagerAbstract
  ) : String ;
  var
    scale : Double ;
  begin
    scale := 0 ;
    Result := Preview( _pagenumber, _printManager, scale ) ;
  end ;

  function TGIS_ControlPrintPreview.Preview(
    const _pagenumber   : Integer ;
    const _printManager : TGIS_PrintManagerAbstract ;
    var   _scale        : Double
  ) : String ;
  var
    tprn : TPrinter ;
    prn  : TGIS_PrinterPreview ;
  begin
    if not Assigned( FGIS_Viewer ) then exit ;

    tprn := nil ;
    try
      if Printer.Count > 0 then
        if assigned( Printer.ActivePrinter ) then
          tprn := Printer ;
    except
      // do nothing
    end ;
    if FGIS_Viewer is TGIS_ViewerWnd then
      prn := TGIS_PrinterPreview.Create(
               tprn,
               TGIS_ViewerWnd( FGIS_Viewer ).ControlSystemPPI,
               TGIS_ViewerWnd( FGIS_Viewer ).ControlCanvasScale
             )
    else
      prn := TGIS_PrinterPreview.Create( tprn ) ;
    try
      Result := previewInternal( _pageNumber, _printManager, prn, _scale ) ;
    finally
      FreeObject( prn ) ;
    end ;
  end ;

  function TGIS_ControlPrintPreview.Preview(
    const _pagenumber   : Integer ;
    const _printManager : TGIS_PrintManagerAbstract ;
    const _customPage   : String ;
    var   _scale        : Double
  ) : String ;
  var
    tprn : TPrinter ;
    prn  : TGIS_PrinterPreview ;
  begin
    if not Assigned( FGIS_Viewer ) then exit ;

    tprn := nil ;
    try
      if Printer.Count > 0 then
        if assigned( Printer.ActivePrinter ) then
          tprn := Printer ;
    except
      // do nothing
    end ;
    if FGIS_Viewer is TGIS_ViewerWnd then
      prn := TGIS_CustomPrinterPreview.Create(
               tprn, _customPage,
               TGIS_ViewerWnd( FGIS_Viewer ).ControlSystemPPI,
               TGIS_ViewerWnd( FGIS_Viewer ).ControlCanvasScale
             )
    else
      prn := TGIS_CustomPrinterPreview.Create( tprn, _customPage ) ;
    try
      Result := previewInternal( _pageNumber, _printManager, prn, _scale ) ;
    finally
      FreeObject( prn ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreview.Print ;
  var
    print_manager : TGIS_PrintManager ;
  begin
    if not Assigned( FGIS_Viewer ) then exit ;

    print_manager := TGIS_PrintManager.Create ;
    try
      print_manager.TileSize := Max( pageWidth, pageHeight ) ;
      print_manager.Print( FGIS_Viewer, nil ) ;
    finally
      FreeObject( print_manager ) ;
    end ;
  end ;

//==============================================================================
// TGIS_ControlPrintPreviewForm
//==============================================================================

  constructor TGIS_ControlPrintPreviewForm.Create(
    _owner : TComponent
  ) ;
  begin
    inherited CreateNew( _owner ) ;
    initSelf ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.initSelf ;
  var
    anchors : TAnchors ;
  begin
    if _rsbidi then
      Self.BiDiMode := TBiDiMode.bdRightToLeft
    else
      Self.BiDiMode := TBiDiMode.bdLeftToRight ;
    Self.Left := 230 ;
    Self.Top := 200 ;
    Self.Caption := 'GIS_ControlPrintPreviewForm' ;
    Self.ClientHeight := 453 ;
    Self.ClientWidth := 613 ;
    Self.FormFactor.Width := 320 ;
    Self.FormFactor.Height := 480 ;
    Self.FormFactor.Devices := [TDeviceKind.Desktop, TDeviceKind.iPhone,
                                TDeviceKind.iPad];
    Self.OnCreate := FormCreate ;
    Self.OnKeyDown := FormKeyDown ;
    Self.OnShow := FormShow ;

    if Self.BiDiMode = TBiDiMode.bdRightToLeft then
      anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;

    stsBar := TStatusBar.Create( Self ) ;
    stsBar.Parent := Self ;
    stsBar.Position.Y := 431 ;
    stsBar.ShowSizeGrip := True ;
    stsBar.Size.Width := 613 ;
    stsBar.Size.Height := 22 ;
    stsBar.Size.PlatformDefault := False ;

    stsLabel := TLabel.Create( stsBar ) ;
    stsLabel.Parent := stsBar ;
    stsLabel.Position.X := 2 ;
    stsLabel.Position.Y := 1 ;
    stsLabel.Size.Width := 120 ;
    stsLabel.Size.Height := 17 ;
    stsLabel.Size.PlatformDefault := False ;

    tlbMain := TPanel.Create( Self ) ;
    tlbMain.Parent := Self ;
    tlbMain.Align := TAlignLayout.Top ;
    tlbMain.Size.Width := 613 ;
    tlbMain.Size.Height := 25 ;
    tlbMain.Size.PlatformDefault := False ;
    tlbMain.TabOrder := 1 ;

    btnPrint := TSpeedButton.Create( tlbMain ) ;
    btnPrint.Parent := tlbMain ;
    btnPrint.Anchors := anchors ;
    btnPrint.Enabled := True ;
    btnPrint.Position.X := 8 ;
    btnPrint.Position.Y := 1 ;
    btnPrint.Size.Width := 26 ;
    btnPrint.Size.Height := 22 ;
    btnPrint.Size.PlatformDefault := False ;
    btnPrint.Visible := True ;
    btnPrint.OnClick := btnPrintExecute ;

    btnPrinterSetup := TSpeedButton.Create( tlbMain ) ;
    btnPrinterSetup.Parent := tlbMain ;
    btnPrinterSetup.Anchors := anchors ;
    btnPrinterSetup.Enabled := True ;
    btnPrinterSetup.Position.X := 35 ;
    btnPrinterSetup.Position.Y := 1 ;
    btnPrinterSetup.Size.Width := 26 ;
    btnPrinterSetup.Size.Height := 22 ;
    btnPrinterSetup.Size.PlatformDefault := False ;
    btnPrinterSetup.Visible := True ;
    btnPrinterSetup.OnClick := btnPrinterSetupExecute ;

    btnPrevPage := TSpeedButton.Create( tlbMain ) ;
    btnPrevPage.Parent := tlbMain ;
    btnPrevPage.Anchors := anchors ;
    btnPrevPage.Enabled := True ;
    btnPrevPage.Position.X := 62 ;
    btnPrevPage.Position.Y := 1 ;
    btnPrevPage.Size.Width := 26 ;
    btnPrevPage.Size.Height := 22 ;
    btnPrevPage.Size.PlatformDefault := False ;
    btnPrevPage.Visible := True ;
    btnPrevPage.OnClick := btnPrevPageExecute ;

    btnNextPage := TSpeedButton.Create( tlbMain ) ;
    btnNextPage.Parent := tlbMain ;
    btnNextPage.Anchors := anchors ;
    btnNextPage.Enabled := True ;
    btnNextPage.Position.X := 112 ;
    btnNextPage.Position.Y := 1 ;
    btnNextPage.Size.Width := 26 ;
    btnNextPage.Size.Height := 22 ;
    btnNextPage.Size.PlatformDefault := False ;
    btnNextPage.Visible := True ;
    btnNextPage.OnClick := btnNextPageExecute ;

    btnHelp := TSpeedButton.Create( tlbMain ) ;
    btnHelp.Parent := tlbMain ;
    btnHelp.Anchors := anchors ;
    btnHelp.Enabled := True ;
    btnHelp.Position.X := 139 ;
    btnHelp.Position.Y := 1 ;
    btnHelp.Size.Width := 26 ;
    btnHelp.Size.Height := 22 ;
    btnHelp.Size.PlatformDefault := False ;
    btnHelp.Visible := True ;
    btnHelp.OnClick := btnHelpExecute ;

    rctPageNumber := TRectangle.Create( tlbMain ) ;
    rctPageNumber.Anchors := anchors ;
    rctPageNumber.Parent := tlbMain ;
    rctPageNumber.Fill.Color := TAlphaColorRec.Null ;
    rctPageNumber.Position.X := 89 ;
    rctPageNumber.Position.Y := 1 ;
    rctPageNumber.Sides := [] ;
    rctPageNumber.Size.Width := 22 ;
    rctPageNumber.Size.Height := 50 ;
    rctPageNumber.Size.PlatformDefault := False ;

    lblPageNumber := TLabel.Create( rctPageNumber ) ;
    lblPageNumber.Parent := rctPageNumber ;
    lblPageNumber.Position.X := 7 ;
    lblPageNumber.Position.Y := 3 ;
    lblPageNumber.Size.Width := 120 ;
    lblPageNumber.Size.Height := 17 ;
    lblPageNumber.Size.PlatformDefault := False ;
    lblPageNumber.Text := '1' ;

    dlgPrinterSetup := TPrinterSetupDialog.Create( Self ) ;
    dlgPrint := TPrintDialog.Create( Self ) ;

  end ;

  procedure TGIS_ControlPrintPreviewForm.Preview(
    const _printManager : TGIS_PrintManagerAbstract ;
    const _customPage   : String ;
    var   _scale        : Double
  ) ;
  var
    txt : String ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    oPrintManager := _printManager ;
    customPage := _customPage ;
    scale := _scale ;
    pageNumber := 1 ;
    actListUpdate ;
    ShowModal ;
  end;

  procedure TGIS_ControlPrintPreviewForm.FormKeyDown(
        _sender  : TObject;
    var _key     : Word ;
    var _keyChar : Char ;
        _shift   : TShiftState
  ) ;
  begin
    if      _key = vkEscape then ModalResult := mrCancel
    else if _key = vkF1     then btnHelpExecute( _sender ) ;
    actListUpdate ;
  end;

  procedure TGIS_ControlPrintPreviewForm.btnPrintExecute(
    _sender : TObject
  ) ;
  var
    print_manager : TGIS_PrintManagerAbstract ;
  begin
    if dlgPrint.Execute then begin
      if assigned( oPrintManager ) then
        oPrintManager.Print( pnlPreview.GIS_Viewer, nil )
      else begin
        print_manager := TGIS_PrintManager.Create ;
        try
          print_manager.Print( pnlPreview.GIS_Viewer, nil ) ;
        finally
          FreeObject( print_manager ) ;
        end ;
      end ;
      if not IsStringEmpty( customPage ) then
        stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, customPage, scale )
      else
        stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, scale ) ;
    end ;
    actListUpdate ;
  end;

  procedure TGIS_ControlPrintPreviewForm.btnPrinterSetupExecute(
    _sender : TObject
  ) ;
  begin
    if dlgPrinterSetup.Execute then begin
      if not IsStringEmpty( customPage ) then
        stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, customPage, scale )
      else
        stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, scale ) ;
    end ;
    actListUpdate ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.btnPrevPageExecute(
    _sender : TObject
  ) ;
  begin
    pageNumber := Min( 1, pageNumber - 1 ) ;
    if not IsStringEmpty( customPage ) then
      stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, customPage, scale )
    else
      stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, scale ) ;
    actListUpdate ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.btnNextPageExecute(
    _sender : TObject
  ) ;
  begin
    pageNumber := Min( 9999, pageNumber + 1 ) ;
    if not IsStringEmpty( customPage ) then
      stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, customPage, scale )
    else
      stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, scale ) ;
    actListUpdate ;
  end;

  procedure TGIS_ControlPrintPreviewForm.btnHelpExecute(
    _sender : TObject
  ) ;
  begin
    if Assigned( FOnHelp ) then FOnHelp( _sender, Name ) ;
    actListUpdate ;
  end;

  procedure TGIS_ControlPrintPreviewForm.actListUpdate ;
  begin
    lblPageNumber.Text      := IntToStr( pageNumber ) ;
    btnPrint.Enabled        := False ;
    btnPrinterSetup.Enabled := False ;
    try
      btnPrint.Enabled          := ( Printer.Count > 0 ) and
                                   assigned( Printer.ActivePrinter ) ;
      {$IFNDEF LINUX}
        btnPrinterSetup.Enabled := Printer.Count > 0 ;
      {$ENDIF}
    except
    end;
    btnPrevPage.Enabled     := pageNumber > 1 ;
    btnNextPage.Enabled     := not pnlPreview.IsLastPage ;
    rctPageNumber.Enabled   := btnPrevPage.Enabled or btnNextPage.Enabled ;
    btnPrevPage.Visible     := btnPrevPage.Enabled ;
    btnNextPage.Visible     := btnNextPage.Enabled ;
    rctPageNumber.Visible   := rctPageNumber.Enabled ;
  end;

  function TGIS_ControlPrintPreviewForm.fget_GIS_Viewer
    : IGIS_Viewer ;
  begin
    Result := pnlPreview.GIS_Viewer ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.fset_GIS_Viewer(
    const _value : IGIS_Viewer ) ;
  begin
    if pnlPreview.GIS_Viewer = _value then exit ;
    pnlPreview.GIS_Viewer := _value ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.fset_HelpEvent(
    const _value : TGIS_HelpEvent
  ) ;
  begin
    FOnHelp := _value ;
    btnHelp.Visible := Assigned( FOnHelp ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.addResBmp(
    const _name   : String ;
    const _parent : TControl
  ) ;
  var
    rstm  : TResourceStream ;
    bmp   : TBitmap ;
    img   : TImage ;
    dat   : TBitmapData ;
    x     : Integer ;
    y     : Integer ;
    w1    : Integer ;
    btn   : TCustomButton ;
    obj   : TFMXObject ;
    clr   : TAlphaColor ;
    blck  : TAlphaColor ;
  begin
    rstm := TResourceStream.Create(hInstance, _name, RT_RCDATA) ;
    try
      rstm.Position := 0 ;

      try
        if _parent is TSpeedButton then
          btn := TSpeedButton.Create( nil )
        else
          btn := TButton.Create( nil ) ;

        btn.Visible := False;
        btn.Parent := Self ;
        btn.ApplyStyleLookup ;

        obj := btn.FindStyleResource( 'text' ) ;
        if obj is TText then
          clr := TText(obj).TextSettings.FontColor
        else
          clr := TAlphaColorRec.Null ;
      finally
        FreeObject( btn ) ;
      end;

      bmp := TBitmap.Create ;
      try
        bmp.LoadFromStream( rstm ) ;

        if clr <> TAlphaColorRec.Null  then begin
          bmp.Map( TMapAccess.ReadWrite, dat );
          try
            if dat.PixelFormat = TPixelFormat.RGBA then begin
              blck := RGBtoBGR( TAlphaColorRec.Black  ) ;
              clr  := RGBtoBGR( clr ) ;
            end
            else
              blck := RGBtoBGR( TAlphaColorRec.Black ) ;

            for y := 0 to bmp.Height - 1 do begin
              w1 := y * dat.Pitch ;
              for x := 0 to bmp.Width - 1 do begin
                if PCardinal( IntPtr(dat.Data) + w1+x*4 )^ = blck
                then
                  PCardinal( IntPtr( dat.Data ) + w1+x*4 )^ := clr ;
              end;
            end;
          finally
            bmp.Unmap(dat);
          end;
        end;

        img := TImage.Create( _parent ) ;
        img.Parent := _parent ;
        img.Name := 'img' ;
        img.Margins.Left   := 5 ;
        img.Margins.Top    := 5 ;
        img.Margins.Right  := 5 ;
        img.Margins.Bottom := 5 ;
        img.Align := TAlignLayout.Client ;
        img.Bitmap.Assign( bmp );
        img.HitTest := False ;
        img.WrapMode := TImageWrapMode.Fit ;
      finally
        FreeObject( bmp ) ;
      end ;
    finally
      FreeObject( rstm ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.FormCreate(
    _sender : TObject
  ) ;
  begin
    Caption                := GIS_RS_PREVIEW_DLG          ;
    {$IFDEF LEVEL_RX10_FMX}
      btnPrint.Hint        := GIS_RS_PREVIEW_PRINT        ;
      btnPrinterSetup.Hint := GIS_RS_PREVIEW_PRINTERSETUP ;
      btnNextPage.Hint     := GIS_RS_PREVIEW_NEXTPAGE     ;
      btnPrevPage.Hint     := GIS_RS_PREVIEW_PREVPAGE     ;
      btnHelp.Hint         := GIS_RS_PREVIEW_HELP         ;
    {$ENDIF}

    Self.addResBmp( 'TGIS_PRINTPREVIEW_PRINT',      btnPrint        ) ;
    Self.addResBmp( 'TGIS_PRINTPREVIEW_PRINTSETUP', btnPrinterSetup ) ;
    Self.addResBmp( 'TGIS_PRINTPREVIEW_PREVPAGE',   btnPrevPage     ) ;
    Self.addResBmp( 'TGIS_PRINTPREVIEW_NEXTPAGE',   btnNextPage     ) ;
    Self.addResBmp( 'TGIS_PRINTPREVIEW_HELP',       btnHelp         ) ;

    oPrintManager := nil ;
    scale := 0 ;
    pnlPreview := TGIS_ControlPrintPreview.Create( self ) ;
    pnlPreview.Parent := self ;
    pnlPreview.Align  := TAlignLayout.Client ;
    pnlPreview.Resize ;
  end;

  procedure TGIS_ControlPrintPreviewForm.FormShow(
    _sender : TObject
  ) ;
  begin
    if not IsStringEmpty( customPage ) then
      stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, customPage, scale )
    else
      stsLabel.Text := pnlPreview.Preview( pageNumber, oPrintManager, scale ) ;
    actListUpdate ;
  end;

  procedure Register ;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ControlPrintPreview ] ) ;
  end ;
{==================================== END =====================================}
end.


