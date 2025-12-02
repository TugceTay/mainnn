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
  Printers unit of TatukGIS VCL.

  In this unit two printers were defined:
   * TGIS_Printer - used for normal printing
   * TGIS_PrinterPreview - used for print preview

  The reason why we implement our own TGIS_Printer class instead of using
  TPrinter is fairly easy: we can not subclass TPrinter. So a new TGIS_Printer
  class, which closely mimics TPrinter, was created.
}

unit FMX.GisPrinters ;
{$HPPEMIT '#pragma link "FMX.GisPrinters"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Types,
  FMX.Graphics,
  FMX.Printer,

  GisRtl,
  GisTypes,
  GisClasses,
  GisPrintUtils,
  GisRendererAbstract,
  GisInterfaces ;

type

  /// <summary>
  ///   Encapsulation of standard printer. Creating this you must provide
  ///   existing TPrinter object (for example standard Printer).
  /// </summary>
  TGIS_Printer = class( TGIS_ObjectDisposable, IGIS_Printer )

    private
      FRenderer : TGIS_RendererAbstract ;

    protected // internal property values

      /// <summary>
      ///   Underlying TPrinter object.
      /// </summary>
      FPrinter : TPrinter ;

      /// <summary>
      ///   True, if is virtual driver (preview, pdf etc).
      /// </summary>
      FVirtualPrinter : Boolean ;

    protected // property access routines

      function  fget_Canvas            : TCanvas  ; virtual;
      function  fget_Renderer          : TGIS_RendererAbstract ; virtual;
      function  fget_Dpi               : Integer  ; virtual;
      function  fget_PageHeight        : Integer  ; virtual;
      procedure fset_PageHeight        ( const _value : Integer
                                       ) ; virtual;
      function  fget_PageWidth         : Integer  ; virtual;
      procedure fset_PageWidth         ( const _value : Integer
                                       ) ; virtual;
      function  fget_PageSize          : TPoint   ; virtual;
      procedure fset_PageSize          ( const _value : TPoint
                                       ) ; virtual;
      function  fget_PrintArea         : TRect ; virtual;
      function  fget_Aborted           : Boolean  ; virtual;
      function  fget_PageNumber        : Integer  ; virtual;

      procedure fset_PageNumber        ( const _value : Integer
                                       ) ; virtual;
      function  fget_Printing          : Boolean  ; virtual;
      function  fget_Title             : String ; virtual;
      procedure fset_Title             ( const _value : String
                                       ) ; virtual;

    protected

      /// <summary>
      ///   Destroy the instance.
      /// </summary>
      procedure doDestroy   ; override;

    public // API functions

      /// <summary>
      ///   Create a instance based on existing printer.
      /// </summary>
      /// <param name="_printer">
      ///   existing printer object
      /// </param>
      constructor Create    ( const _printer : TPrinter
                            ) ; virtual;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      procedure Abort       ; virtual;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      procedure BeginDoc    ; virtual;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      procedure EndDoc      ; virtual;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      procedure NewPage     ; virtual;

      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device dependent pixels (X-coordinate).
      /// </summary>
      /// <param name="_size">
      ///   size in twips
      /// </param>
      /// <returns>
      ///   converted value in pixels
      /// </returns>
      /// <remarks>
      ///   Used to make device independent positioning from the paper border.
      /// </remarks>
      function  TwipsToX ( const _size : Integer ) : Integer ; virtual;

      /// <summary>
      ///   Convert the size from to device independent Twips (1/1440 inch)
      ///   to device dependent pixels (Y-coordinate).
      /// </summary>
      /// <param name="_size">
      ///   size in twips
      /// </param>
      /// <returns>
      ///   converted value in pixels
      /// </returns>
      /// <remarks>
      ///   Used to make device independent positioning from the paper border.
      /// </remarks>
      function  TwipsToY ( const _size : Integer ) : Integer ; virtual;

    public // API properties
      /// <summary>
      ///   Underlying TPrinter object.
      /// </summary>
      property Printer : TPrinter read FPrinter ;

      /// <summary>
      ///   True, if is print virtual driver (not printer based: preview,
      ///   pdf etc.
      /// </summary>
      property VirtualPrinter : Boolean read FVirtualPrinter ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      property Canvas : TCanvas read fget_Canvas ;

      /// <summary>
      ///   Renderer object created on printer canvas.
      /// </summary>
      property Renderer : TGIS_RendererAbstract read fget_Renderer ;

      /// <summary>
      ///   Printer resolution.
      /// </summary>
      property Dpi    : Integer read fget_Dpi ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help. Value can be
      ///   assigned only for selected printers.
      /// </summary>
      property PageWidth  : Integer read fget_PageWidth write fset_PageWidth ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help. Value can be
      ///   assigned only for selected printers.
      /// </summary>
      property PageHeight : Integer read fget_PageHeight write fset_PageHeight ;

      /// <summary>
      ///   Physical page size. Value can be assigned only for selected
      ///   printers.
      /// </summary>
      property PageSize : TPoint read fget_PageSize write fset_PageSize ;

      /// <summary>
      ///   Physical print area.
      /// </summary>
      property PrintArea  : TRect read fget_PrintArea ;

      /// <summary>
      ///   If printing is interrupted.
      /// </summary>
      property Aborted    : Boolean read fget_Aborted ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      property PageNumber : Integer read fget_PageNumber write fset_PageNumber ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      property Printing : Boolean read fget_Printing ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      property Title : String read fget_Title write fset_Title ;

  end ;

  /// <summary>
  ///   Encapsulation of print preview printer. Creating this you can provide
  ///   existing TPrinter object (for example standard Printer). If not then
  ///   "virtual" printer will be created (based on LETTER page size).
  /// </summary>
  TGIS_PrinterPreview = class( TGIS_Printer )

    protected // internal property values

      /// <summary>
      ///   Bitmap created by print preview.
      /// </summary>
      FBitmap : TBitmap ;

      /// <summary>
      ///   Preview resolution.
      /// </summary>
      FPixelsPerInch : Integer ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      FPageNumber : Integer ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      FAborted : Boolean ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      FPrinting : Boolean ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      FTitle : String ;

      /// <summary>
      ///   System PPI; should be taken from GIS.ControlSystemPPI.
      /// </summary>
      FSystemPPI : Integer ;

      /// <summary>
      ///   Canvas scale; should be taken from GIS.ControlCanvasScale.
      /// </summary>
      FCanvasScale : Single ;

    private
      defaultPrinter : TGIS_PageSizeEntry ;

    protected // property access routines

      function  fget_Canvas     : TCanvas ; override;
      function  fget_Dpi        : Integer ; override;
      function  fget_PageHeight : Integer ; override;
      function  fget_PageWidth  : Integer ; override;
      function  fget_PageSize   : TPoint  ; override;
      function  fget_PrintArea  : TRect   ; override;
      function  fget_Aborted    : Boolean ; override;
      function  fget_PageNumber   : Integer ; override;
      procedure fset_PageNumber   ( const _value : Integer
                                  ) ; override;
      function  fget_Printing     : Boolean ; override;
      function  fget_Title        : String  ; override;
      procedure fset_Title        ( const _value : String
                                  ) ; override;

    protected

      /// <summary>
      ///   Destroy the instance.
      /// </summary>
      procedure doDestroy  ; override;

    private
      procedure doCreate   ( const _systemPPI   : Integer  ;
                             const _canvasScale : Single
                           ) ;

    public // API functions

      /// <inheritdoc/>
      constructor Create   ( const _printer     : TPrinter
                           ) ; overload ; override ;

      /// <summary>
      ///   Creates an object, takes into account non-standard screen parameters.
      /// </summary>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      /// <param name="_systemPPI">
      ///   system PPI; take it from GIS.ControlSystemPPI
      /// </param>
      /// <param name="_canvasScale">
      ///   canvas scale; take it from GIS.ControlCanvasScale
      /// </param>
      constructor Create   ( const _printer     : TPrinter ;
                             const _systemPPI   : Integer  ;
                             const _canvasScale : Single
                           ) ; overload ; virtual ;

      /// <inheritdoc/>
      procedure   Abort    ; override ;

      /// <inheritdoc/>
      procedure   BeginDoc ; override ;

      /// <inheritdoc/>
      procedure   EndDoc   ; override ;

      /// <inheritdoc/>
      procedure   NewPage  ; override ;

      /// <inheritdoc/>
      function  TwipsToX   ( const _size        : Integer
                           ) : Integer ; override ;

      /// <inheritdoc/>
      function  TwipsToY   ( const _size        : Integer
                           ) : Integer ; override ;

    public // API properties

      /// <summary>
      ///   Bitmap created by print preview.
      /// </summary>
      property Bitmap : TBitmap read FBitmap ;

      /// <summary>
      ///   Pixel per Inch of the preview device. Default is 96.
      /// </summary>
      property PPI  : Integer read FPixelsPerInch write FPixelsPerInch ;

      /// <summary>
      ///   Pixel per Inch of the preview device. Default is 96.
      /// </summary>
      property SystemPPI : Integer read FSystemPPI ;

      /// <summary>
      ///   Canvas scale. Default is 1.
      /// </summary>
      property CanvasScale : Single read FCanvasScale ;
  end;

  /// <summary>
  ///   Encapsulation of custom print preview printer used in preview.
  /// </summary>
  TGIS_CustomPrinterPreview = class( TGIS_PrinterPreview )
    private
      bChangedOrientation : Boolean ;

    private
      procedure applyCustomFormat( _format : String ) ;

    protected
      function  fget_PageHeight : Integer ; override ;
      function  fget_PageWidth  : Integer ; override ;
      function  fget_PageSize   : TPoint  ; override ;
      function  fget_PrintArea  : TRect   ; override ;

    public
      /// <summary>
      ///   Create a instance based on existing printer.
      /// </summary>
      /// <param name="_printer">
      ///   existing printer object
      /// </param>
      /// <param name="_customFormat">
      ///   custom page format
      /// </param>
      constructor Create   ( const _printer      : TPrinter ;
                             const _customFormat : String
                           ) ; overload ;

      /// <summary>
      ///   Creates an object, takes into account non-standard screen parameters.
      /// </summary>
      /// <param name="_printer">
      ///   printer object
      /// </param>
      /// <param name="_customFormat">
      ///   custom page format
      /// </param>
      /// <param name="_systemPPI">
      ///   system PPI; take it from GIS.ControlSystemPPI
      /// </param>
      /// <param name="_canvasScale">
      ///   canvas scale; take it from GIS.ControlCanvasScale
      /// </param>
      constructor Create   ( const _printer      : TPrinter ;
                             const _customFormat : String   ;
                             const _systemPPI    : Integer  ;
                             const _canvasScale  : Single
                           ) ; overload ;

  end ;

//##############################################################################
implementation

uses
  System.UITypes,
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
    FMX.Printer.Win,
  {$ENDIF}
  Math,
  FMX.GisRenderer;

//==============================================================================
// TGIS_Printer
//==============================================================================

  constructor TGIS_Printer.Create( const _printer : TPrinter ) ;
  begin
    inherited Create ;

    if Assigned( _printer ) and ( _printer.Count > 0 ) then
      FPrinter := _printer
    else
      FPrinter := nil ;

    FVirtualPrinter     := False ;

    FRenderer := nil ;
  end;

  procedure TGIS_Printer.doDestroy ;
  begin
    FreeObject( FRenderer ) ;
    inherited ;
  end ;

  function TGIS_Printer.fget_Canvas
    : TCanvas ;
  begin
    if assigned( FPrinter ) then
      Result := FPrinter.Canvas
    else
      Result := nil ;
  end;

  function TGIS_Printer.fget_Renderer
    : TGIS_RendererAbstract ;
  begin
    if not Assigned( FRenderer ) then begin
      FRenderer := TGIS_RendererFmx.Create ;
      TGIS_RendererFmx( FRenderer ).CreatePrinterContext(
        Canvas,
        PageWidth,
        PageHeight,
        Dpi,
        100
      ) ;
    end ;
    Result := FRenderer ;
  end;

  function TGIS_Printer.fget_Dpi
    : Integer ;
  begin
    {$IFDEF MSWINDOWS}
      if FPrinter.ActivePrinter.DPICount > 0 then
        Result := FPrinter.ActivePrinter.ActiveDPI.X
      else
        Result := GetDeviceCaps( TPrinterWin(FPrinter).Handle, LOGPIXELSX );
    {$ELSE}
      Result := FPrinter.ActivePrinter.ActiveDPI.X ;
    {$ENDIF}
  end ;

  function TGIS_Printer.fget_PageWidth
    : Integer ;
  begin
    Result := FPrinter.PageWidth ;
  end;

  procedure TGIS_Printer.fset_PageWidth(
    const _value : Integer
  ) ;
  begin
    // for save inheritance
  end;

  function TGIS_Printer.fget_PageHeight
    : Integer ;
  begin
    Result := FPrinter.PageHeight ;
  end;

  procedure TGIS_Printer.fset_PageHeight(
    const _value : Integer
  ) ;
  begin
    // for save inheritance
  end;

  function TGIS_Printer.fget_PageSize
    : TPoint ;
  begin
    Result.X := FPrinter.PageWidth ;
    Result.Y := FPrinter.PageHeight ;
  end;

  procedure TGIS_Printer.fset_PageSize(
    const _value : TPoint
  ) ;
  begin
    // for save inheritance
  end;

  function TGIS_Printer.fget_PrintArea
    : TRect;
  begin
    Result.Left   :=  0 ;
    Result.Top    :=  0 ;
    Result.Right  := FPrinter.PageWidth ;
    Result.Bottom := FPrinter.PageHeight ;
  end;

  function TGIS_Printer.fget_Aborted
    : Boolean ;
  begin
    Result := FPrinter.Aborted ;
  end ;

  function TGIS_Printer.fget_PageNumber
    : Integer ;
  begin
    Result := FPrinter.PageNumber ;
  end ;

  procedure TGIS_Printer.fset_PageNumber(
    const _value : Integer
  ) ;
  begin
    // only for safe inheritance
  end;

 function TGIS_Printer.fget_Printing
    : Boolean ;
  begin
    Result := FPrinter.Printing ;
  end ;

  function TGIS_Printer.fget_Title
    : String ;
  begin
    Result := FPrinter.Title ;
  end ;

  procedure TGIS_Printer.fset_Title(
    const _value : String
  ) ;
  begin
    FPrinter.Title := _value ;
  end ;

  procedure TGIS_Printer.Abort;
  begin
    FPrinter.Abort ;
  end ;

  procedure TGIS_Printer.BeginDoc;
  begin
    SetLastError( 0 ) ;

    {$IFDEF MSWINDOWS}
      if FPrinter.ActivePrinter.DPICount > 0 then
        FPrinter.ActivePrinter.ActiveDPIIndex := 0 ;
    {$ELSE}
      FPrinter.ActivePrinter.ActiveDPIIndex := 0 ;
    {$ENDIF}
    FPrinter.BeginDoc ;
  end;

  procedure TGIS_Printer.EndDoc ;
  begin
    FPrinter.EndDoc ;
  end;

  procedure TGIS_Printer.NewPage ;
  begin
    FPrinter.NewPage ;
  end;

  function TGIS_Printer.TwipsToX( const _size : Integer ) : Integer ;
  var
    tmp : Integer ;
  begin
    if      _size < 0 then Result := -_size // minus so real pixels
    else if _size > 0 then begin
                        if assigned( FPrinter ) and
                           assigned( FPrinter.ActivePrinter ) then begin
                          tmp := _size * Dpi ;
                          if tmp > 1440 then Result := RoundS( 1.0*tmp /1440 )
                                        else Result := 1 ;
                        end else Result := 0 ;
                      end
    else              Result := 0 ;
  end ;

  function TGIS_Printer.TwipsToY( const _size : Integer ) : Integer ;
  var
    tmp : Integer ;
  begin
    if      _size < 0 then Result := -_size // minus so real pixels
    else if _size > 0 then begin
                        if assigned( FPrinter ) and
                           assigned( FPrinter.ActivePrinter ) then begin
                          tmp := _size * Dpi ;
                          if tmp > 1440 then Result := RoundS( 1.0*tmp /1440 )
                                        else Result := 1 ;
                        end else Result := 0 ;
                      end
    else              Result := 0 ;
  end ;

//==============================================================================
// TGIS_PreviewPrinter
//==============================================================================

  procedure TGIS_PrinterPreview.doCreate(
    const _systemPPI   : Integer  ;
    const _canvasScale : Single
  ) ;
  var
    i : Integer ;
  begin
    FPixelsPerInch := _systemPPI ;
    FSystemPPI     := _systemPPI ;

    if assigned( FPrinter ) then
    begin
      {$IFDEF MSWINDOWS}
        if FPrinter.ActivePrinter.DPICount > 0 then
          FPrinter.ActivePrinter.ActiveDPIIndex := 0 ;
      {$ELSE}
        FPrinter.ActivePrinter.ActiveDPIIndex := 0 ;
      {$ENDIF}
    end ;

    FCanvasScale := _canvasScale ;
    FVirtualPrinter := True ;
    FAborted  := False ;
    FPrinting := False ;
    FTitle    := ''    ;
    FBitmap   := nil   ;
    i := TGIS_PageSizeTable.IndexOf( 'A4' ) ;
    if i >= 0 then
      TGIS_PageSizeTable.GetPage( i, False,
                                  defaultPrinter.Name, defaultPrinter.PageSize,
                                  defaultPrinter.PrintArea, defaultPrinter.PPI ) ;
  end ;

  constructor TGIS_PrinterPreview.Create(
    const _printer : TPrinter
  ) ;
  begin
    inherited Create( _printer ) ;
    doCreate( 96, 1 ) ;
  end ;

  constructor TGIS_PrinterPreview.Create(
    const _printer     : TPrinter ;
    const _systemPPI   : Integer  ;
    const _canvasScale : Single
  ) ;
  begin
    inherited Create( _printer ) ;
    doCreate( _systemPPI, _canvasScale ) ;
  end ;

  procedure TGIS_PrinterPreview.doDestroy ;
  begin
    FreeObject( FBitmap ) ;
    inherited;
  end;

  function TGIS_PrinterPreview.fget_Canvas
    : TCanvas;
  begin
    if not assigned( FBitmap ) then NewPage;
    Result := FBitmap.Canvas ;
  end;

  function TGIS_PrinterPreview.fget_Dpi : Integer ;
  begin
    Result := RoundS( PPI * FCanvasScale ) ;
  end ;

  function TGIS_PrinterPreview.fget_PageWidth
    : Integer;
  begin
    if assigned( FPrinter ) then begin
      Result := ( inherited fget_PageWidth * FPixelsPerInch ) div inherited fget_Dpi ;
    end
    else
      Result := RoundS( defaultPrinter.PrintArea.Width * FPixelsPerInch /
                        defaultPrinter.PPI ) ;
    if FCanvasScale <> 1 then
      Result := RoundS( Result * FCanvasScale ) ;
  end;

  function TGIS_PrinterPreview.fget_PageHeight
    : Integer;
  begin
    if assigned( FPrinter ) then begin
      Result := ( inherited fget_PageHeight * FPixelsPerInch ) div inherited fget_Dpi ;
    end
    else
      Result := RoundS( defaultPrinter.PrintArea.Height * FPixelsPerInch /
                        defaultPrinter.PPI ) ;
    if FCanvasScale <> 1 then
      Result := RoundS( Result * FCanvasScale ) ;
  end;

  function TGIS_PrinterPreview.fget_PageSize
    : TPoint ;
  begin
    if assigned( FPrinter ) then begin
      Result := inherited fget_PageSize ;
      Result.X := ( Result.X  * FPixelsPerInch ) div inherited fget_Dpi ;
      Result.Y := ( Result.Y  * FPixelsPerInch ) div inherited fget_Dpi ;
    end
    else begin
      Result.X := RoundS( defaultPrinter.PageSize.X * FPixelsPerInch /
                          defaultPrinter.PPI ) ;
      Result.Y := RoundS( defaultPrinter.PageSize.Y * FPixelsPerInch /
                          defaultPrinter.PPI ) ;
    end ;
    if FCanvasScale <> 1 then
      Result := Point( RoundS( Result.X * FCanvasScale ),
                       RoundS( Result.Y * FCanvasScale ) ) ;
  end;

  function TGIS_PrinterPreview.fget_PrintArea
    : TRect ;
  begin
    if assigned( FPrinter ) then begin
      Result := inherited fget_PrintArea ;
      Result.Left   := ( Result.Left   * FPixelsPerInch ) div inherited fget_Dpi ;
      Result.Right  := ( Result.Right  * FPixelsPerInch ) div inherited fget_Dpi ;
      Result.Top    := ( Result.Top    * FPixelsPerInch ) div inherited fget_Dpi ;
      Result.Bottom := ( Result.Bottom * FPixelsPerInch ) div inherited fget_Dpi ;
    end
    else begin
      Result.Left   := RoundS( defaultPrinter.PrintArea.Left * FPixelsPerInch /
                               defaultPrinter.PPI ) ;
      Result.Right  := RoundS( defaultPrinter.PrintArea.Right * FPixelsPerInch /
                               defaultPrinter.PPI ) ;
      Result.Top    := RoundS( defaultPrinter.PrintArea.Top * FPixelsPerInch /
                               defaultPrinter.PPI ) ;
      Result.Bottom := RoundS( defaultPrinter.PrintArea.Bottom * FPixelsPerInch /
                               defaultPrinter.PPI ) ;
    end ;
    if FCanvasScale <> 1 then
      Result := Rect( RoundS( Result.Left   * FCanvasScale ),
                      RoundS( Result.Top    * FCanvasScale ),
                      RoundS( Result.Right  * FCanvasScale ),
                      RoundS( Result.Bottom * FCanvasScale ) ) ;
  end;

  function TGIS_PrinterPreview.fget_Aborted
    : Boolean ;
  begin
    Result := FAborted ;
  end ;

  function TGIS_PrinterPreview.fget_PageNumber
    : Integer ;
  begin
    Result := FPageNumber ;
  end ;

  procedure TGIS_PrinterPreview.fset_PageNumber(
    const _value : Integer
  ) ;
  begin
    FPageNumber := Min( Max( 1, _value ), 9999 ) ;
  end;

  function TGIS_PrinterPreview.fget_Printing
    : Boolean ;
  begin
    Result := FPrinting ;
  end ;

  function TGIS_PrinterPreview.fget_Title
    : String ;
  begin
    Result := FTitle ;
  end ;

  procedure TGIS_PrinterPreview.fset_Title(
    const _value : String
  ) ;
  begin
    FTitle := _value ;
  end ;

  procedure TGIS_PrinterPreview.Abort;
  begin
    FAborted := True ;
    EndDoc;
  end;

  procedure TGIS_PrinterPreview.BeginDoc ;
  begin
    FPageNumber := 0 ;
    FAborted    := False ;
    FPrinting   := True  ;
    NewPage;
  end;

  procedure TGIS_PrinterPreview.EndDoc;
  begin
    FPrinting := False ;
  end;

  procedure TGIS_PrinterPreview.NewPage;
  var
    rct : TRect ;
  begin
    inc( FPageNumber ) ;

    FreeObject( FRenderer ) ;
    FreeObject( FBitmap ) ;
    FBitmap := {$IFDEF LEVEL_XE5_FMX}
                 FMX.Graphics.TBitmap.Create ;
               {$ELSE}
                 FMX.Types.TBitmap.Create ;
               {$ENDIF}
    rct := fget_PrintArea ;
    FBitmap.Width  := rct.Right - rct.Left ;
    FBitmap.Height := rct.Bottom - rct.Top ;
    // be sure that the bitmap is clear because it may not be
    FBitmap.Clear( TAlphaColorRec.Null ) ;
  end ;

  function TGIS_PrinterPreview.TwipsToX(
    const _size : Integer
  ) : Integer ;
  var
    tmp : Integer ;
  begin
    if not assigned( FPrinter ) and ( _size > 0 ) then begin
      tmp := _size * Dpi ;
      if tmp > 1440 then Result := RoundS( 1.0*tmp /1440 )
                    else Result := 1 ;
    end
    else
      Result := inherited TwipsToX( _size ) ;
  end ;

  function TGIS_PrinterPreview.TwipsToY(
    const _size : Integer
  ) : Integer ;
  var
    tmp : Integer ;
  begin
    if not assigned( FPrinter ) and ( _size > 0 ) then begin
      tmp := _size * Dpi ;
      if tmp > 1440 then Result := RoundS( 1.0*tmp /1440 )
                    else Result := 1 ;
    end
    else
      Result := inherited TwipsToY( _size )
  end ;

//==============================================================================
// TGIS_CustomPrinterPreview
//==============================================================================

  function TGIS_CustomPrinterPreview.fget_PageHeight
    : Integer ;
  begin
    if bChangedOrientation then
      Result := inherited fget_PageWidth
    else
      Result := inherited fget_PageHeight ;
  end ;

  function TGIS_CustomPrinterPreview.fget_PageWidth
    : Integer ;
  begin
    if bChangedOrientation then
      Result := inherited fget_PageHeight
    else
      Result := inherited fget_PageWidth ;
  end ;

  function TGIS_CustomPrinterPreview.fget_PageSize
    : TPoint  ;
  begin
    Result := inherited fget_PageSize ;
    if bChangedOrientation then
      Result := Point( Result.Y, Result.X ) ;
  end ;

  function TGIS_CustomPrinterPreview.fget_PrintArea
    : TRect ;
  var
    ps : TPoint ;
  begin
    Result := inherited fget_PrintArea ;
    if bChangedOrientation then
      Result := Rect( Result.Top, Result.Left,
                      Result.Bottom, Result.Right ) ;
  end ;

  procedure TGIS_CustomPrinterPreview.applyCustomFormat(
    _format : String
  ) ;
  var
    custom : TGIS_CustomPrinterSettings ;
    psize  : TPoint ;
  begin
    bChangedOrientation := False ;

    if IsStringEmpty( _format ) then exit ;
    custom := TGIS_PrintUtils.ResolveCustomPrinterSettings( _format ) ;
    if not assigned( custom ) then exit ;

    psize := inherited fget_PageSize ;
    if ( custom.Landscape and ( psize.X < psize.Y ) ) or
       ( custom.Portrait  and not ( psize.X < psize.Y ) ) then
      bChangedOrientation := True ;
    FreeObject( custom ) ;
  end ;

  constructor TGIS_CustomPrinterPreview.Create(
    const _printer      : TPrinter ;
    const _customFormat : String
  ) ;
  begin
    inherited Create( _printer ) ;
    applyCustomFormat( _customFormat ) ;
  end ;

  constructor TGIS_CustomPrinterPreview.Create(
    const _printer      : TPrinter ;
    const _customFormat : String   ;
    const _systemPPI    : Integer  ;
    const _canvasScale  : Single
  ) ;
  begin
    inherited Create( _printer, _systemPPI, _canvasScale ) ;
    applyCustomFormat( _customFormat ) ;
  end ;

{==================================== END =====================================}
end.

