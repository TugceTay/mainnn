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
  Print Preview component. Form.
}

unit VCL.GisControlPrintPreview ;
{$HPPEMIT '#pragma link "VCL.GisControlPrintPreview"'}

{$INCLUDE GisInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Math,
  System.SysUtils,

  VCL.Graphics,
  VCL.Dialogs,
  VCL.Forms,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.ActnList,
  VCL.Buttons,
  VCL.ImgList,
  VCL.Printers,
  {$IFDEF LEVEL_XE8_VCL}
    System.ImageList,
  {$ENDIF}

  {$IFDEF GIS_XDK}
    XDK.Core,
  {$ENDIF}

  GisRtl,
  GisInterfaces,
  GisTypes,
  GisResource,
  GisPrintManagerAbstract,
  GisPrintPreviewHelperAbstract,
  VCL.GisPrinters,
  VCL.GisPrintManager;

type
  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Helper class for managing print preview.
  /// </summary>
  TGIS_PrintPreviewFactoryVCL = class ( TGIS_PrintPreviewFactory )
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
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_ControlPrintPreview = class( TCustomPanel, IGIS_Subscribe )

    private // property internal values
      [weak]
      FGIS_Viewer : IGIS_Viewer ;
      FIsLastPage : Boolean ;

    private  // control components
      pnlPage     : TPanel ;
      imgPreview  : TImage ;
      pnlShadow   : TPanel ;

    private  // other private variables
      pageWidth      : Integer ;
      pageHeight     : Integer ;
      pageSize       : TPoint  ;
      pagePrintArea  : TRect   ;
      firstPaint     : Boolean ;

    private // property access routines

      function fget_IsPrinter  : Boolean ;
      procedure fset_GIS_Viewer( const _value : IGIS_Viewer ) ;

    private // private methods

      procedure resize_control ( _scale       : Double  ) ;

    protected

      /// <summary>
      ///   Internal method for recreate page settings.
      /// </summary>
      /// <param name="_printer">
      ///   virtual printer
      /// </param>
      procedure recreateInternal( const _printer      : TGIS_PrinterPreview
                                ) ;

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
      function  previewInternal ( const _pageNumber   : Integer ;
                                  const _printManager : TGIS_PrintManagerAbstract ;
                                  const _printer      : TGIS_PrinterPreview ;
                                  var   _scale        : Double
                                ) : String ;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_owner">
      ///   parent element
      /// </param>
      constructor Create    ( _owner : TComponent
                            ) ; override;

      /// <summary>
      ///   Destroy dataset.
      /// </summary>
      destructor  Destroy               ; override;

     /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

    protected

      /// <summary>
      ///   Standard Resize override.
      ///   See Embarcadero help for TCustomControl for more details.
      /// </summary>
      procedure Resize    ; override;

      /// <summary>
      ///   Standard Paint override.
      ///   See Embarcadero help for TCustomControl for more details.
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
      ///   Title string.
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
      ///   Title string.
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
      ///   Title string.
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
      procedure Print       ; overload;

      /// <summary>
      ///   Print document.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Print       ( const _printManager : TGIS_PrintManager
                            ) ; overload;

    public // properties new for this class

      /// <summary>
      ///   True, if last page in preview was reached.
      /// </summary>
      property IsLastPage : Boolean read FIsLastPage ;

      /// <summary>
      ///   True, if any printer exists.
      /// </summary>
      property IsPrinter : Boolean read fget_IsPrinter ;

    published // properties new for this class

      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer read FGIS_Viewer write fset_GIS_Viewer ;

    published // properties derived from base class}

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Align ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Anchors ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BevelInner ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BevelOuter ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BevelWidth ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BorderStyle ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property BorderWidth ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Ctl3D ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Color default clWindow ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Enabled ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property HelpContext ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Hint ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentColor ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentCtl3D ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property ParentShowHint ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property PopupMenu ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property TabStop  ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property TabOrder ;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      property Visible;

    {$IFDEF GIS_XDK}
      public
        {#gendoc:hide:GENXDK}
        XDK : TGIS_ControlXDK ;
    {$ENDIF}

  end ;

  {#gendoc:hide}
  /// <summary>
  ///  Control for managing print preview projected especially for template designer.
  ///  For internal use only.
  /// </summary>
  TGIS_ControlPrintPreviewTemplateDesigner = class( TGIS_ControlPrintPreview )
    public

      /// <summary>
      ///   Set new page size.
      /// </summary>
      /// <remarks>
      ///   It is important to have a proper page size before the preview starts to be visible.
      /// </remarks>
      /// <param name="_printer">
      ///   new printer data
      /// </param>
      procedure Recreate    ( const _printer      : TGIS_PrinterTemplate
                            ) ;

      /// <summary>
      ///   Preview selected page.
      /// </summary>
      /// <param name="_pagenumber">
      ///   page number to be previewed (1 means the first page)
      /// </param>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_printer">
      ///   preview printer
      /// </param>
      /// <returns>
      ///   Title string.
      /// </returns>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      function  Preview     ( const _pagenumber   : Integer ;
                              const _printManager : TGIS_PrintManagerAbstract ;
                              const _printer      : TGIS_PrinterPreview
                            ) : String ; overload ; virtual ;
  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Print preview form. Only for internal use of TatukGIS.
  /// </summary>
  TGIS_ControlPrintPreviewForm = class(TForm)
      actList: TActionList;
      dlgPrinterSetup: TPrinterSetupDialog;
      dlgPrint: TPrintDialog;
      stsBar: TStatusBar;
      imgList: TImageList;
      tlbMain: TPanel;
      btnPrint: TSpeedButton;
      btnPrinterSetup: TSpeedButton;
      btnPrevPage: TSpeedButton;
      pnlPageNumber: TPanel;
      btnNextPage: TSpeedButton;
      btnHelp: TSpeedButton;

      /// <summary>
      ///   Action "Print" handler.
      /// </summary>
      procedure actPrintExecute       ( _sender      : TObject ) ;

      /// <summary>
      ///   Button "Print" handler.
      /// </summary>
      procedure btnPrintExecute       ( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Printer Setup" handler.
      /// </summary>
      procedure actPrinterSetupExecute( _sender      : TObject ) ;

      /// <summary>
      ///   Button "Printer Setup" handler.
      /// </summary>
      procedure btnPrinterSetupExecute( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Previous Page" handler.
      /// </summary>
      procedure actPrevPageExecute    ( _sender      : TObject ) ;

      /// <summary>
      ///   Button "Previous Page" handler.
      /// </summary>
      procedure btnPrevPageExecute    ( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Next Page" handler.
      /// </summary>
      procedure actNextPageExecute    ( _sender      : TObject ) ;

      /// <summary>
      ///   Button "Next Page" handler.
      /// </summary>
      procedure btnNextPageExecute    ( _sender      : TObject ) ;

      /// <summary>
      ///   Action "Help" handler.
      /// </summary>
      procedure actHelpExecute        ( _sender      : TObject ) ;

      /// <summary>
      ///   Button "Help" handler.
      /// </summary>
      procedure btnHelpExecute        ( _sender      : TObject ) ;

      /// <summary>
      ///   Action list update.
      /// </summary>
      procedure actListUpdate         ( _sender      : TObject ) ;

      procedure FormCreate            ( _sender      : TObject ) ;

      /// <summary>
      ///   KeyDown handler.
      /// </summary>
      procedure FormKeyDown           (     _sender  : TObject ;
                                        var _key     : Word    ;
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

    public

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TCustomForm in Delphi help.
      /// </param>
      constructor Create    ( _owner : TComponent
                            ) ; override;

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TCustomForm in Delphi help.
      /// </param>
      /// <param name="_dummy">
      ///   See documentation for TCustomForm in Delphi help.
      /// </param>
      constructor CreateNew ( _owner : TComponent  ;
                              _dummy : Integer = 0
                            ) ; override;

    public

      /// <summary>
      ///   Show preview form. Start previewing on page number 1.
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
                        ) ; overload ;

    public // properties

      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer read  fget_GIS_Viewer
                                        write fset_GIS_Viewer ;

    published //events
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

{$R GisControlPrintPreview_16x16.RES}

uses
  System.Types,
  VCL.GisFramework,
  VCL.GisControlHelper,
  VCL.GISRendererGdiPlus,
  VCL.GisRenderer;

const
  SHADOW = 5          ;
  MARGIN = SHADOW + 8 ;

//==============================================================================
// TGIS_PrintPreviewFactoryVCL
//==============================================================================

  function TGIS_PrintPreviewFactoryVCL.IsPrinter : Boolean ;
  begin
    try
      Result := ( Printer.Printers.Count > 0 ) and ( Printer.PrinterIndex >= 0 ) ;
    except
      // if Printer.PrinterIndex = -1 then Delphi will try to set a default printer;
      // if the setting fails an exception will be thrown;
      Result := False ;
    end ;
  end ;

  procedure TGIS_PrintPreviewFactoryVCL.Preview(
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
      frm.Caption     := _caption   ;
      frm.GIS_Viewer  := _viewer    ;
      frm.HelpEvent   := _help      ;
      frm.Left        := _left      ;
      frm.Top         := _top       ;
      frm.Width       := _width     ;
      frm.Height      := _height    ;
      frm.Position    := poDesigned ;
      frm.WindowState := _state     ;

      if frm.Position = poDesigned then begin
       frm.Left := ( Screen.Width  - frm.Width  ) div 2 ;
       frm.Top  := ( Screen.Height - frm.Height ) div 2 ;
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

    ControlStyle := ControlStyle + [csNeedsBorderPaint];

    Caption          := ''     ;
    Color            := clGray ;
    ParentBackground := False  ;
    Width            := 100    ;
    Height           := 100    ;
    BevelOuter       := bvNone ;
    BevelInner       := bvNone ;
    BorderStyle      := bsNone ;

    pnlShadow := TPanel.Create( self ) ;
    with pnlShadow do begin
      Caption            := ''       ;
      Color              := clBlack  ;
      ParentBackground   := False    ;
      BevelOuter         := bvNone   ;
      BevelInner         := bvNone   ;
      BorderStyle        := bsNone   ;
      Width              := 1        ;
      Height             := 1        ;
      Left               := -10      ;
    end ;

    pnlPage := TPanel.Create( Self ) ;
    with pnlPage do begin
      Caption            := ''       ;
      Color              := clWhite  ;
      ParentBackground   := False    ;
      BevelOuter         := bvNone   ;
      BevelInner         := bvNone   ;
      BorderStyle        := bsNone   ;
      Width              := 1        ;
      Height             := 1        ;
      Left               := -10      ;
    end ;

    imgPreview := TImage.Create( pnlPage ) ;
    with imgPreview do begin
      Stretch := True ;
    end ;

    InsertControl( pnlShadow ) ;
    InsertControl( pnlPage ) ;
    pnlPage.InsertControl( imgPreview ) ;

    pageSize := Point( 850, 1100 )  ;

    firstPaint := True ;
  end ;

  destructor TGIS_ControlPrintPreview.Destroy;
  begin
    if assigned( FGIS_Viewer ) then
      FGIS_Viewer.UnSubscribe( Self ) ;

    inherited;
  end ;

  function TGIS_ControlPrintPreview.fget_IsPrinter : Boolean ;
  begin
    try
      Result := ( Printer.Printers.Count > 0 ) and ( Printer.PrinterIndex >= 0 ) ;
    except
      // if Printer.PrinterIndex = -1 then Delphi will try to set a default printer;
      // if the setting fails an exception will be thrown;
      Result := False ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreview.fset_GIS_Viewer(
    const _value : IGIS_Viewer
  ) ;
  begin
    FGIS_Viewer := _value ;

    if assigned( FGIS_Viewer ) then
      FGIS_Viewer.Subscribe( Self ) ;
  end ;

  procedure TGIS_ControlPrintPreview.resize_control(
    _scale : Double
  ) ;
  var
    scale : Double ;
  begin
    if ( pageSize.X = 0 ) or ( pageSize.Y = 0 ) then exit ;

    if _scale = 0 then
      scale := Min( ( Width  - 2* MARGIN ) / pageSize.X ,
                    ( Height - 2* MARGIN ) / pageSize.Y
                  )
    else
      scale := _scale ;

    pnlPage.Width  := RoundS( pageSize.X * scale ) ;
    pnlPage.Height := RoundS( pageSize.Y * scale ) ;

    pnlPage.Left := ( Width  - pnlPage.Width  ) div 2 ;
    pnlPage.Top  := ( Height - pnlPage.Height ) div 2 ;

    pnlShadow.Width   := pnlPage.Width  ;
    pnlShadow.Height  := pnlPage.Height ;
    pnlShadow.Top     := pnlPage.Top    + SHADOW ;
    pnlShadow.Left    := pnlPage.Left   + SHADOW ;

    imgPreview.Width  := RoundS( pageWidth   * scale ) ;
    imgPreview.Height := RoundS( pageHeight  * scale ) ;

    imgPreview.Left := RoundS( pagePrintArea.Left * scale ) ;
    imgPreview.Top  := RoundS( pagePrintArea.Top  * scale ) ;
  end ;

  procedure TGIS_ControlPrintPreview.Resize ;
  begin
    resize_control( 0 ) ;
  end;

  procedure TGIS_ControlPrintPreview.Paint ;
  begin
    if firstPaint then begin
      Resize ;
      firstPaint := False ;
    end ;
    inherited ;
  end ;

  procedure TGIS_ControlPrintPreview.recreateInternal(
    const _printer : TGIS_PrinterPreview
  ) ;
  var
    scale : Double ;
  begin
    pageSize      := _printer.PageSize   ;
    scale := Min( ( ClientWidth  - 2* MARGIN ) / pageSize.X ,
                  ( ClientHeight - 2* MARGIN ) / pageSize.Y
                ) ;

    pageWidth     := _printer.PageWidth  ;
    pageHeight    := _printer.PageHeight ;
    pageSize      := _printer.PageSize   ;
    pagePrintArea := _printer.PrintArea  ;
    resize_control( scale ) ;
  end ;

  function TGIS_ControlPrintPreview.previewInternal(
    const _pageNumber   : Integer ;
    const _printManager : TGIS_PrintManagerAbstract ;
    const _printer      : TGIS_PrinterPreview ;
    var   _scale        : Double
  ) : String ;
  var
    scale : Double ;
    ts : Integer ;
    print_manager : TGIS_PrintManager ;
    rnd : TGIS_RendererVclAbstract ;
    bmp : TBitmap ;
  begin
    pageSize       := _printer.PageSize   ;
    scale := Min( ( ClientWidth  - 2* MARGIN ) / pageSize.X ,
                  ( ClientHeight - 2* MARGIN ) / pageSize.Y
                ) ;

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
    end ;

    Result := _printer.Title ;

    FIsLastPage := ( _printer.PageNumber = 9999 ) or
                   ( assigned( FGIS_Viewer ) and FGIS_Viewer.IsEmpty ) ;
    rnd := nil ;
    bmp := nil ;
    try
      rnd := TGIS_RendererVclGdiPlus.Create ;
      bmp := TBitmap.Create ;
      bmp.Width := RoundS( pageWidth * scale );
      bmp.Height := RoundS( pageHeight * scale );
      bmp.PixelFormat := pf32bit ;
      bmp.Canvas.Brush.Style := bsSolid ;
      bmp.Canvas.Brush.Color := clWhite ;
      bmp.Canvas.FillRect( Rect(0, 0, bmp.Width, bmp.Height ) );

      rnd.stretchBitmap( _printer.Bitmap, bmp, Rect(0, 0, bmp.Width, bmp.Height), 100 );
      imgPreview.Picture.Assign( bmp )
    finally
      FreeObject( bmp ) ;
      FreeObject( rnd ) ;
    end;
    resize_control( scale ) ;
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
    const _pageNumber   : Integer ;
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
      if ( Printer.PrinterIndex >= 0 ) then
        tprn := Printer ;
    except
      // do nothing
    end ;
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
      if ( Printer.PrinterIndex >= 0 ) then
        tprn := Printer ;
    except
      // do nothing
    end ;
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
      //?print_manager.TileSize := Max( pageWidth, pageHeight ) ;
      print_manager.Print( FGIS_Viewer, nil ) ;
    finally
      FreeObject( print_manager ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreview.Print(
    const _printManager : TGIS_PrintManager
  ) ;
  begin
    if not Assigned( FGIS_Viewer ) then exit ;
    //?_printManager.TileSize := Max( pageWidth, pageHeight ) ;
    _printManager.Print( FGIS_Viewer, nil ) ;
  end ;

  procedure TGIS_ControlPrintPreview.SubscribedEvent(
    _sender  : TObject ;
    _event   : Integer ;
    _context : TObject
  ) ;
  begin
    case _event of
      GIS_SUBSCRIBED_DESTROY :
        begin
          FGIS_Viewer := nil ;
        end ;
    end ;
  end ;

//==============================================================================
// TGIS_ControlPrintPreviewTemplateDesigner
//==============================================================================

  procedure TGIS_ControlPrintPreviewTemplateDesigner.Recreate(
    const _printer : TGIS_PrinterTemplate
  ) ;
  begin
    recreateInternal( _printer ) ;
  end ;

  function TGIS_ControlPrintPreviewTemplateDesigner.Preview(
    const _pagenumber   : Integer ;
    const _printManager : TGIS_PrintManagerAbstract ;
    const _printer      : TGIS_PrinterPreview
  ) : String ;
  var
    scale : Double ;
  begin
    scale := 0 ;
    Result := previewInternal( _pageNumber, _printManager, _printer, scale ) ;
  end ;

//==============================================================================
// TGIS_ControlPrintPreviewForm
//==============================================================================

  constructor TGIS_ControlPrintPreviewForm.Create(
    _owner : TComponent
  ) ;
  begin
    CreateNew( _owner ) ;
    DoubleBuffered := not IsWin11 ;
  end ;

  constructor TGIS_ControlPrintPreviewForm.CreateNew(
    _owner : TComponent  ;
    _dummy : Integer = 0
  ) ;
  begin
    inherited CreateNew( _owner, _dummy ) ;
    initSelf ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.initSelf ;
  var
    anchors : TAnchors ;
  begin
    {$IFDEF LEVEL_RX101_VCL}
      Self.ControlState := [csReadingState] ;
    {$ENDIF}
    if _rsbidi then
      Self.BiDiMode := TBiDiMode.bdRightToLeft
    else
      Self.BiDiMode := TBiDiMode.bdLeftToRight ;
    Self.Left := 230 ;
    Self.Top := 200 ;
    Self.Caption := 'GIS_ControlPrintPreviewForm' ;
    Self.ClientHeight := 453 ;
    Self.ClientWidth := 613 ;
    Self.Color := clBtnFace ;
    Font.Charset := DEFAULT_CHARSET ;
    Font.Color := clWindowText ;
    Font.Height := -11 ;
    Font.Name := 'Tahoma' ;
    Font.Style := [] ;
    Self.KeyPreview := True ;
    Self.OnCreate := FormCreate ;
    Self.OnKeyDown := FormKeyDown ;
    Self.PixelsPerInch := 96 ;
    Self.ControlState := [] ;

    if Self.BiDiMode = TBiDiMode.bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;

    stsBar := TStatusBar.Create( Self ) ;
    stsBar.Parent := Self ;
    stsBar.Left := 0 ;
    stsBar.Top := 434 ;
    stsBar.Width := 613 ;
    stsBar.Height := 19 ;
    stsBar.SimplePanel := True ;
    stsBar.ParentFont := True ;

    tlbMain := TPanel.Create( Self ) ;
    tlbMain.Parent := Self ;
    tlbMain.Height := 25 ;
    tlbMain.Align := alTop ;
    tlbMain.Anchors := [akLeft, akTop, akRight] ;
    tlbMain.ParentShowHint := False ;
    tlbMain.ShowHint := True ;
    tlbMain.TabOrder := 1 ;

    btnPrint := TSpeedButton.Create( tlbMain ) ;
    btnPrint.Parent := tlbMain ;
    btnPrint.Anchors := anchors ;
    btnPrint.Top := 1 ;
    btnPrint.Height := 23 ;
    PlaceControl( BiDiMode, nil, btnPrint, 8, 24 ) ;
    btnPrint.Flat := True ;
    btnPrint.OnClick := btnPrintExecute ;

    btnPrinterSetup := TSpeedButton.Create( tlbMain ) ;
    btnPrinterSetup.Parent := tlbMain ;
    btnPrinterSetup.Anchors := anchors ;
    btnPrinterSetup.Top := 1 ;
    btnPrinterSetup.Height := 23 ;
    PlaceControl( BiDiMode, nil, btnPrinterSetup, 39, 24 ) ;
    btnPrinterSetup.Flat := True ;
    btnPrinterSetup.OnClick := btnPrinterSetupExecute ;

    btnPrevPage := TSpeedButton.Create( tlbMain ) ;
    btnPrevPage.Parent := tlbMain ;
    btnPrevPage.Anchors := anchors ;
    btnPrevPage.Top := 1 ;
    btnPrevPage.Height := 23 ;
    PlaceControl( BiDiMode, nil, btnPrevPage, 70, 24 ) ;
    btnPrevPage.Flat := True ;
    btnPrevPage.OnClick := btnPrevPageExecute ;

    btnNextPage := TSpeedButton.Create( tlbMain ) ;
    btnNextPage.Parent := tlbMain ;
    btnNextPage.Anchors := anchors ;
    btnNextPage.Top := 1 ;
    btnNextPage.Height := 23 ;
    PlaceControl( BiDiMode, nil, btnNextPage, 120, 24 ) ;
    btnNextPage.Flat := True ;
    btnNextPage.OnClick := btnNextPageExecute ;

    btnHelp := TSpeedButton.Create( tlbMain ) ;
    btnHelp.Parent := tlbMain ;
    btnHelp.Anchors := anchors ;
    btnHelp.Top := 1 ;
    btnHelp.Height := 23 ;
    PlaceControl( BiDiMode, nil, btnHelp, 150, 24 ) ;
    btnHelp.Flat := True ;
    btnHelp.OnClick := btnHelpExecute ;

    pnlPageNumber := TPanel.Create( tlbMain ) ;
    pnlPageNumber.Parent := tlbMain ;
    pnlPageNumber.Anchors := anchors ;
    pnlPageNumber.Top := 1 ;
    pnlPageNumber.Height := 23 ;
    PlaceControl( BiDiMode, nil, pnlPageNumber, 92, 30 ) ;
    pnlPageNumber.BevelOuter := bvNone ;
    pnlPageNumber.Caption := '1' ;
    pnlPageNumber.TabOrder := 0 ;

    dlgPrinterSetup := TPrinterSetupDialog.Create( Self ) ;
    dlgPrint := TPrintDialog.Create( Self ) ;

  end ;

  procedure TGIS_ControlPrintPreviewForm.Preview(
    const _printManager : TGIS_PrintManagerAbstract ;
    const _customPage   : String ;
    var   _scale        : Double
  ) ;
  begin
    if not Assigned( GIS_Viewer ) then exit ;
    oPrintManager := _printManager ;
    customPage := _customPage ;
    scale := _scale ;
    pageNumber := 1 ;
    if IsStringEmpty( customPage ) then
      stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                               scale )
    else
      stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                               customPage, scale ) ;
    actListUpdate( Self ) ;

    {$IFDEF LEVEL_RX101_VCL}
      ScaleForCurrentDpi ;
    {$ELSE}
      Self.ScaleBy( Screen.PixelsPerInch, PixelsPerInch ) ;
    {$ENDIF}
    ShowModal ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.FormKeyDown(
        _sender : TObject ;
    var _key    : Word    ;
        _shift  : TShiftState
  );
  begin
    if      _key = VK_ESCAPE then ModalResult := mrCancel
    else if _key = VK_F1     then actHelpExecute( _sender ) ;
    actListUpdate( nil ) ;
  end;

  procedure TGIS_ControlPrintPreviewForm.actPrintExecute(
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
      if IsStringEmpty( customPage ) then
        stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                                 scale )
      else
        stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                                 customPage, scale ) ;
    end
  end;

  procedure TGIS_ControlPrintPreviewForm.btnPrintExecute(
    _sender : TObject
  ) ;
  begin
    actPrintExecute( _sender ) ;
    actListUpdate( _sender ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.actPrinterSetupExecute(
    _sender : TObject
  ) ;
  begin
    if dlgPrinterSetup.Execute then begin
      if IsStringEmpty( customPage ) then
        stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                                 scale )
      else
        stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                                 customPage, scale ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.btnPrinterSetupExecute(
    _sender : TObject
  ) ;
  begin
    actPrinterSetupExecute( _sender ) ;
    actListUpdate( _sender ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.actPrevPageExecute(
    _sender : TObject
  ) ;
  begin
    pageNumber := Min( 1, pageNumber - 1 ) ;
    if IsStringEmpty( customPage ) then
      stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                               scale )
    else
      stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                               customPage, scale ) ;
  end;

  procedure TGIS_ControlPrintPreviewForm.btnPrevPageExecute(
    _sender : TObject
  ) ;
  begin
    actPrevPageExecute( _sender ) ;
    actListUpdate( _sender ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.actNextPageExecute(
    _sender : TObject
  ) ;
  begin
    pageNumber := Min( 9999, pageNumber + 1 ) ;
    if IsStringEmpty( customPage ) then
      stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                               scale )
    else
      stsBar.SimpleText := pnlPreview.Preview( pageNumber, oPrintManager,
                                               customPage, scale ) ;
  end;

  procedure TGIS_ControlPrintPreviewForm.btnNextPageExecute(
    _sender : TObject
  ) ;
  begin
    actNextPageExecute( _sender ) ;
    actListUpdate( _sender ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.actHelpExecute(
    _sender : TObject
  ) ;
  begin
    if Assigned( FOnHelp ) then FOnHelp( _sender, Name ) ;
  end;

  procedure TGIS_ControlPrintPreviewForm.btnHelpExecute(
    _sender : TObject
  ) ;
  begin
    actHelpExecute( _sender ) ;
    actListUpdate( _sender ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.actListUpdate(
    _sender : TObject
  ) ;
  begin
    pnlPageNumber.Caption   := IntToStr( pageNumber ) ;
    btnPrint.Enabled        := False ;
    btnPrinterSetup.Enabled := False ;
    try
      btnPrint.Enabled        := ( Printer.Printers.Count > 0 ) and
                                 ( Printer.PrinterIndex >= 0 ) ;
      btnPrinterSetup.Enabled := Printer.Printers.Count > 0 ;
    except
    end;
    btnPrevPage.Enabled     := pageNumber > 1 ;
    btnNextPage.Enabled     := not pnlPreview.IsLastPage ;
    pnlPageNumber.Enabled   := btnPrevPage.Enabled or btnNextPage.Enabled ;
    btnPrevPage.Visible     := btnPrevPage.Enabled ;
    btnNextPage.Visible     := btnNextPage.Enabled ;
    pnlPageNumber.Visible   := pnlPageNumber.Enabled ;
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
    Update ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.fset_HelpEvent(
    const _value : TGIS_HelpEvent
  ) ;
  begin
    FOnHelp := _value ;
    btnHelp.Visible := Assigned( FOnHelp ) ;
  end ;

  procedure TGIS_ControlPrintPreviewForm.FormCreate(
    _sender : TObject
  ) ;
  var
    bmp : TBitmap ;

  procedure loadRes( const _name : String ) ;
  var
    rstm  : TResourceStream ;
  begin
    rstm := TResourceStream.Create(hInstance, _name, RT_RCDATA) ;
    try
      bmp.LoadFromStream( rstm ) ;
      imgList.AddMasked( bmp, bmp.TransparentColor ) ;
      imgList.DrawingStyle := TDrawingStyle.dsTransparent ;
    finally
      FreeObject( rstm ) ;
    end ;
  end ;

  begin
    Caption                 := GIS_RS_PREVIEW_DLG          ;
    btnPrint.Hint           := GIS_RS_PREVIEW_PRINT        ;
    btnPrinterSetup.Hint    := GIS_RS_PREVIEW_PRINTERSETUP ;
    btnNextPage.Hint        := GIS_RS_PREVIEW_NEXTPAGE     ;
    btnPrevPage.Hint        := GIS_RS_PREVIEW_PREVPAGE     ;
    btnHelp.Hint            := GIS_RS_PREVIEW_HELP         ;

    imgList := TImageList.Create( Self ) ;
    imgList.BkColor := TColor( $FF00FF ) ;

    bmp := TBitmap.Create ;
    try
      loadRes( 'TGIS_PRINTPREVIEW_PRINT'      ) ;
      loadRes( 'TGIS_PRINTPREVIEW_PRINTSETUP' ) ;
      loadRes( 'TGIS_PRINTPREVIEW_PREVPAGE'   ) ;
      loadRes( 'TGIS_PRINTPREVIEW_NEXTPAGE'   ) ;
      loadRes( 'TGIS_PRINTPREVIEW_HELP'       ) ;
    finally
      FreeObject( bmp ) ;
    end ;

    GisScaleImageList( imgList, imgList, Screen.PixelsPerInch, 96 );

    btnPrint.Glyph := nil ;
    imgList.GetBitmap( 0, btnPrint.Glyph ) ;
    btnPrinterSetup.Glyph := nil ;
    imgList.GetBitmap( 1, btnPrinterSetup.Glyph ) ;
    btnPrevPage.Glyph := nil ;
    imgList.GetBitmap( 2, btnPrevPage.Glyph ) ;
    btnNextPage.Glyph := nil ;
    imgList.GetBitmap( 3, btnNextPage.Glyph ) ;
    btnHelp.Glyph := nil ;
    imgList.GetBitmap( 4, btnHelp.Glyph ) ;

    oPrintManager := nil ;
    scale := 0 ;
    pnlPreview := TGIS_ControlPrintPreview.Create( self ) ;
    pnlPreview.Parent := self ;
    pnlPreview.Align  := alClient ;
    pnlPreview.Resize ;

    actListUpdate( _sender ) ;
  end;

  procedure Register ;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ControlPrintPreview ] ) ;
  end ;

{==================================== END =====================================}
end.



