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

unit VCL.GisPrinters ;
{$HPPEMIT '#pragma link "VCL.GisPrinters"'}

interface

{$INCLUDE GisInclude.inc}

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  System.Math,
  VCL.Graphics,
  VCL.Printers,
  VCL.Forms,

  GisRtl,
  GisInterfaces,
  GisTypes,
  GisClasses,
  GisPrintUtils,
  GisRendererAbstract,
  VCL.GisGdiPlus ;

type

  /// <summary>
  ///   Encapsulation of standard printer. Creating this you must provide
  ///   existing TPrinter object (for example standard Printer).
  /// </summary>
  TGIS_Printer = class( TGIS_ObjectDisposable, IGIS_Printer )

    private
      oParent      : TObject ;
      [weak]
      oViewer      : IGIS_Viewer ;
      oRenderer    : TGIS_RendererAbstract ;

    private
      /// <summary>
      ///   tracking printing state (to catch BeginDoc errors)
      /// </summary>
      inPrinting : Boolean ;

    private
      procedure init_graphics ;
      procedure free_graphics ;

    protected // internal property values
      /// <summary>
      ///   Underlying TPrinter object.
      /// </summary>
      FPrinter : TPrinter ;

      /// <summary>
      ///   True, if is virtual driver (preview, pdf etc).
      /// </summary>
      FVirtualPrinter : Boolean ;

      /// <summary>
      ///   Gdi32 canvas object.
      /// </summary>
      FCanvas : TCanvas ;

      /// <summary>
      ///   Gdi+ canvas object.
      /// </summary>
      FGraphics : TGIS_GdipGraphics ;

      /// <summary>
      ///   TRru if GDI Objecyt attached to Canvas.
      /// </summary>
      FGraphicsHasCanvas : Boolean ;

      /// <summary>
      ///   Renderer context of Printer canvas.
      /// </summary>
      FRenderer : TGIS_RendererAbstract ;

      /// <summary>
      ///   True, if printer output is preferred to be a bitmap.
      /// </summary>
      FPreferBitmapOutput : Boolean ;

    protected

      function  fget_Canvas            : TCanvas ; virtual;
      function  fget_Graphics          : TGIS_GdipGraphics ; virtual;
      function  fget_Renderer          : TGIS_RendererAbstract ; virtual;
      function  fget_PageHeight        : Integer ; virtual;
      procedure fset_PageHeight        ( const _value : Integer
                                       ) ; virtual;
      function  fget_PageWidth         : Integer ; virtual;
      procedure fset_PageWidth         ( const _value : Integer
                                       ) ; virtual;
      function  fget_PageSize          : TPoint  ; virtual;
      procedure fset_PageSize          ( const _value : TPoint
                                       ) ; virtual;
      function  fget_PrintArea         : TRect ; virtual;
      function  fget_Aborted           : Boolean ; virtual;
      function  fget_PageNumber        : Integer ; virtual;
      procedure fset_PageNumber        ( const _value : Integer
                                       ) ; virtual;
      function  fget_PPI               : Integer ; virtual;
      function  fget_Printing          : Boolean ; virtual;
      function  fget_Title             : String ; virtual;
      procedure fset_Title             ( const _value : String
                                       ) ; virtual;

      protected

        /// <summary>
        ///   Destroy the instance.
        /// </summary>
        procedure doDestroy   ; override;

      public // API functions

        {#gendoc:hide:GENXDK}
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
        ///   Initiate printing.
        ///   Use instead of BeginDoc when Printing is already set to True.
        /// </summary>
        /// <returns>
        ///   State of internal variables.
        ///   Pass it to EndDocEx when printing finishes.
        /// </returns>
        function  BeginDocEx  : Boolean ; virtual ;

        /// <summary>
        ///   Finish printing.
        ///   Use instead of EndDoc when printing was initiated with BeginDocEx.
        /// </summary>
        /// <param name="_init_locally">
        ///   True when printer internal variables were created locally.
        /// </param>
        procedure EndDocEx    ( const _init_locally : Boolean
                              ) ; virtual ;

        /// <summary>
        ///   See documentation for TPrinter in Delphi help.
        /// </summary>
        procedure NewPage     ; virtual;

        /// <summary>
        ///   Convert the size from device independent Twips (1/1440 inch)
        ///   to device dependent pixels (X-coordinate).
        /// </summary>
        /// <param name="_size">
        ///   given size in twips
        /// </param>
        /// <remarks>
        ///   Used to make device independent positioning from the paper border.
        /// </remarks>
        /// <returns>
        ///   Device dependent X-coordinate.
        /// </returns>
        function  TwipsToX ( const _size : Integer ) : Integer ; virtual;

        /// <summary>
        ///   Convert the size from device independent Twips (1/1440 inch)
        ///   to device dependent pixels (Y-coordinate).
        /// </summary>
        /// <param name="_size">
        ///   given size in twips
        /// </param>
        /// <remarks>
        ///   Used to make device independent positioning from the paper border.
        /// </remarks>
        /// <returns>
        ///   Device dependent X-coordinate.
        /// </returns>
        function  TwipsToY ( const _size : Integer ) : Integer ; virtual;

      public // API properties
        {#gendoc:hide:GENXDK}
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
        ///   True, if printer output is preferred to be a bitmap.
        /// </summary>
        /// <remarks>
        ///   <note type="note">
        ///     <list type="bullet">
        ///       <item>
        ///         For number of devices (for example selected PDF
        ///         printers) GDI is limited: regions are not fully
        ///         supported etc. In such case to properly render a
        ///         Bitmap output will be requited.
        ///       </item>
        ///       <item>
        ///         This property is giving a hint to application that
        ///         complicated things should be rendered as bitmaps.
        ///       </item>
        ///     </list>
        ///   </note>
        /// </remarks>
        property PreferBitmapOutput : Boolean read  FPreferBitmapOutput
                                              write FPreferBitmapOutput ;

        {#gendoc:hide:GENXDK}
        /// <summary>
        ///   See documentation for TPrinter in Delphi help.
        /// </summary>
        property Canvas : TCanvas read fget_Canvas ;

        {#gendoc:hide:GENXDK}
        /// <summary>
        ///   See documentation for TGPGraphics.
        /// </summary>
        property Graphics : TGIS_GdipGraphics read fget_Graphics ;

        {#gendoc:hide:GENXDK}
        /// <summary>
        ///   Renderer object created on printer canvas.
        /// </summary>
        property Renderer : TGIS_RendererAbstract read fget_Renderer ;

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
        ///   True if printing has been aborted.
        /// </summary>
        property Aborted    : Boolean read fget_Aborted ;

        /// <summary>
        ///   See documentation for TPrinter in Delphi help.
        /// </summary>
        property PageNumber : Integer read fget_PageNumber write fset_PageNumber ;

        /// <summary>
        ///   Pixel per Inch of printer device.
        /// </summary>
        property PPI  : Integer read fget_PPI ;

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
      ///   Printing resolution in pixels per inch.
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

    private
      defaultPrinter : TGIS_PageSizeEntry ;

    protected

      function  fget_Canvas     : TCanvas ; override;
      function  fget_PageHeight : Integer ; override;
      function  fget_PageWidth  : Integer ; override;
      function  fget_PageSize   : TPoint  ; override;
      function  fget_PrintArea  : TRect   ; override;
      function  fget_Aborted    : Boolean ; override;
      function  fget_PageNumber : Integer ; override;
      procedure fset_PageNumber ( const _value : Integer
                                ) ; override;
      function  fget_Printing   : Boolean ; override;
      function  fget_Title      : String  ; override;
      procedure fset_Title      ( const _value : String
                                ) ; override;
      function  fget_PPI        : Integer ; override;

    protected

      /// <summary>
      ///   Destroy the instance.
      /// </summary>
      procedure doDestroy  ; override;

    public // API functions

      {#gendoc:hide:GENXDK}
      /// <inheritdoc/>
      constructor Create     ( const _printer : TPrinter
                             ) ; override;

      /// <inheritdoc/>
      procedure   Abort      ; override;

      /// <inheritdoc/>
      procedure   BeginDoc   ; override;

      /// <inheritdoc/>
      procedure   EndDoc     ; override;

      /// <inheritdoc/>
      function    BeginDocEx : Boolean ; override ;

      /// <inheritdoc/>
      procedure   EndDocEx   ( const _init_locally : Boolean
                             ) ; override ;

      /// <inheritdoc/>
      procedure   NewPage    ; override;

    public // API properties

      /// <summary>
      ///   Bitmap created by print preview.
      /// </summary>
      property Bitmap : TBitmap read FBitmap ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Encapsulation of print preview printer used in template designer.
  ///   It is not bound to any existing TPrinter object.
  /// </summary>
  TGIS_PrinterTemplate = class( TGIS_PrinterPreview )
    protected // internal property values

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      FPageSize : TPoint ;

      /// <summary>
      ///   See documentation for TPrinter in Delphi help.
      /// </summary>
      FPrintArea : TRect ;

      /// <summary>
      ///   Resolution of virtual printer.
      /// </summary>
      FPPI : Integer ;

    protected

      function  fget_PageHeight : Integer ; override;
      function  fget_PageWidth  : Integer ; override;
      function  fget_PageSize   : TPoint  ; override;
      function  fget_PrintArea  : TRect   ; override;

    public // API functions

      /// <inheritdoc/>
      constructor Create     ( const _pageSize  : TPoint ;
                               const _printArea : TRect ;
                               const _ppi       : Integer
                             ) ;

  end ;

  {#gendoc:hide}
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
      {#gendoc:hide:GENXDK}
      constructor Create     ( const _printer : TPrinter ;
                               const _customFormat : String
                             ) ; overload ;
  end ;

//##############################################################################
implementation

uses
  System.Types,
  Winapi.GDIPAPI,
  Vcl.GisRendererGdi32,
  Vcl.GisRendererGdiPlus;

//==============================================================================
// TGIS_Printer
//==============================================================================

  constructor TGIS_Printer.Create( const _printer : TPrinter ) ;
  begin
    inherited Create ;

    if Assigned( _printer ) and ( _printer.Printers.Count > 0 ) then
      FPrinter := _printer
    else
      FPrinter := nil ;

    FCanvas             := nil ;
    FGraphics           := nil ;
    FRenderer           := nil ;
    FVirtualPrinter     := False ;
    FPreferBitmapOutput := False ;

    oParent   := nil ;
    oViewer   := nil ;
    oRenderer := nil ;
  end;

  procedure TGIS_Printer.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_Printer.fget_Canvas
    : TCanvas ;
  begin
    if assigned( FGraphics ) then begin
      if not assigned( FCanvas ) then begin
        FCanvas := TCanvas.Create;
        FCanvas.Handle := FGraphics.GetHDC ;
        FCanvas.Font.PixelsPerInch := FPrinter.Canvas.Font.PixelsPerInch ;
      end ;
      Result := FCanvas ;
    end else
      Result := nil ;
  end;

  function TGIS_Printer.fget_Graphics
    : TGIS_GdipGraphics ;
  begin
    if ( FGraphicsHasCanvas ) then begin
      FGraphics.ReleaseHDC( FCanvas.Handle ) ;
      FGraphicsHasCanvas := False ;
    end ;
    Result := FGraphics ;
  end;

  function TGIS_Printer.fget_Renderer
    : TGIS_RendererAbstract ;
  begin
    if not Assigned( FRenderer ) then begin
      if Canvas.ClassName = 'TBitmapCanvas' then begin
        FRenderer := TGIS_RendererVclGdiPlus.Create ;
        TGIS_RendererVclGdiPlus( FRenderer ).CreatePrinterContext(
          Canvas,
          PageWidth,
          PageHeight,
          Canvas.Font.PixelsPerInch,
          100
        ) ;
      end
      else begin
        FRenderer := TGIS_RendererVclGdi32.Create ;
        TGIS_RendererVclGdi32( FRenderer ).CreatePrinterContext(
          Canvas,
          PageWidth,
          PageHeight,
          Canvas.Font.PixelsPerInch,
          100
        ) ;
      end ;
    end ;
    Result := FRenderer ;
  end;

  function TGIS_Printer.fget_PageWidth
    : Integer ;
  begin
    Result := GetDeviceCaps( FPrinter.Handle, HORZRES ) ;
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
    Result := GetDeviceCaps( FPrinter.Handle, VERTRES ) ;
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
    Result.X := GetDeviceCaps( FPrinter.Handle, PHYSICALWIDTH ) ;
    Result.Y := GetDeviceCaps( FPrinter.Handle, PHYSICALHEIGHT ) ;
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
    Result.Left   := GetDeviceCaps( FPrinter.Handle, PHYSICALOFFSETX ) ;
    Result.Top    := GetDeviceCaps( FPrinter.Handle, PHYSICALOFFSETY ) ;
    Result.Right  := Result.Left +
                     GetDeviceCaps( FPrinter.Handle, HORZRES         ) ;
    Result.Bottom := Result.Top +
                     GetDeviceCaps( FPrinter.Handle, VERTRES         ) ;
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

  function TGIS_Printer.fget_PPI
    : Integer ;
  begin
    if assigned( FCanvas ) then
      Result := FCanvas.Font.PixelsPerInch
    else
      Result := GetDeviceCaps( FPrinter.Handle, LOGPIXELSX ) ;
  end ;

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

  procedure TGIS_Printer.Abort ;
  begin
    FPrinter.Abort ;
  end ;

  procedure TGIS_Printer.init_graphics ;
  begin
    FGraphics := TGIS_GdipGraphics.Create(FPrinter.Handle);
    FGraphicsHasCanvas := True ;
    FGraphics.SetPageUnit(UnitPixel);
    FGraphics.SetSmoothingMode(SmoothingModeDefault);
    FGraphics.SetPixelOffsetMode(PixelOffsetModeHalf);
    FGraphics.SetInterpolationMode(InterpolationModeHighQuality);
    FGraphics.SetTextContrast(4);
    FGraphics.SetCompositingMode(CompositingModeSourceOver);
  end ;

  procedure TGIS_Printer.free_graphics ;
  begin
    FreeObject( FRenderer ) ;
    if assigned( FCanvas ) then begin
      if FGraphicsHasCanvas then begin
        FGraphics.ReleaseHDC( FCanvas.Handle ) ;
        FGraphicsHasCanvas := False ;
      end;
      FreeObject( FCanvas ) ;
    end ;
    FreeObject( FGraphics ) ;
  end;

  procedure TGIS_Printer.BeginDoc;
  begin
    SetLastError( 0 ) ;

    FPrinter.BeginDoc ;
    init_graphics ;
  end;

  procedure TGIS_Printer.EndDoc ;
  begin
    FPrinter.EndDoc ;
    free_graphics ;
  end;

  function TGIS_Printer.BeginDocEx
    : Boolean ;
  begin
    Result := False ;
    if not assigned(FGraphics) then begin
      init_graphics ;
      Result := True ;
    end ;
  end;

  procedure TGIS_Printer.EndDocEx(
    const _init_locally : Boolean
  ) ;
  begin
    if _init_locally then
      free_graphics ;
  end ;

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
                           assigned( FPrinter.Canvas ) then begin
                          tmp := _size * Canvas.Font.PixelsPerInch ;
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
    Result := 0 ;
    if      _size < 0 then Result := -_size // minus so real pixels
    else if _size > 0 then begin
                        if assigned( FPrinter ) and
                           assigned( FPrinter.Canvas ) then begin
                          tmp := _size * Canvas.Font.PixelsPerInch ;
                          if tmp > 1440 then Result := RoundS( 1.0*tmp /1440 )
                                        else Result := 1 ;
                        end;
                      end
    else              Result := 0 ;
  end ;

//==============================================================================
// TGIS_PreviewPrinter
//==============================================================================

  constructor TGIS_PrinterPreview.Create( const _printer : TPrinter ) ;
  var
    i : Integer ;
  begin
    FPixelsPerInch := Screen.PixelsPerInch ;

    inherited Create( _printer ) ;

    FVirtualPrinter := True ;
    FAborted  := False ;
    FPrinting := False ;
    FTitle    := ''    ;
    FBitmap   := nil ;
    i := TGIS_PageSizeTable.IndexOf( 'A4' ) ;
    if i >= 0 then
      TGIS_PageSizeTable.GetPage( i, False,
                                  defaultPrinter.Name, defaultPrinter.PageSize,
                                  defaultPrinter.PrintArea, defaultPrinter.PPI ) ;
  end;

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
    Result.Font.PixelsPerInch := FPixelsPerInch ;
  end;

  function TGIS_PrinterPreview.fget_PageWidth
    : Integer;
  begin
    if assigned( FPrinter ) then begin
      Result := RoundS( inherited fget_PageWidth * FPixelsPerInch /
                        GetDeviceCaps( FPrinter.Handle, LOGPIXELSX ) ) ;
    end
    else
      Result := RoundS( defaultPrinter.PrintArea.Width * FPixelsPerInch /
                        defaultPrinter.PPI ) ;
  end;

  function TGIS_PrinterPreview.fget_PageHeight
    : Integer;
  begin
    if assigned( FPrinter ) then begin
      Result := RoundS( inherited fget_PageHeight * FPixelsPerInch /
                        GetDeviceCaps( FPrinter.Handle, LOGPIXELSY ) ) ;
    end
    else
      Result := RoundS( defaultPrinter.PrintArea.Height * FPixelsPerInch /
                        defaultPrinter.PPI ) ;
  end;

  function TGIS_PrinterPreview.fget_PageSize
    : TPoint ;
  begin
    if assigned( FPrinter ) then begin
      Result := inherited fget_PageSize ;
      Result.X := RoundS( Result.X  * FPixelsPerInch /
                          GetDeviceCaps( FPrinter.Handle, LOGPIXELSX ) ) ;
      Result.Y := RoundS( Result.Y  * FPixelsPerInch /
                          GetDeviceCaps( FPrinter.Handle, LOGPIXELSY ) ) ;
    end
    else begin
      Result.X := RoundS( defaultPrinter.PageSize.X * FPixelsPerInch /
                          defaultPrinter.PPI ) ;
      Result.Y := RoundS( defaultPrinter.PageSize.Y * FPixelsPerInch /
                          defaultPrinter.PPI ) ;
    end ;
  end;

  function TGIS_PrinterPreview.fget_PrintArea
    : TRect ;
  begin
    if assigned( FPrinter ) then begin
      Result := inherited fget_PrintArea ;
      Result.Left   := RoundS( Result.Left   * FPixelsPerInch /
                               GetDeviceCaps( FPrinter.Handle, LOGPIXELSX ) ) ;
      Result.Right  := RoundS( Result.Right  * FPixelsPerInch /
                               GetDeviceCaps( FPrinter.Handle, LOGPIXELSX ) ) ;
      Result.Top    := RoundS( Result.Top    * FPixelsPerInch /
                               GetDeviceCaps( FPrinter.Handle, LOGPIXELSY ) ) ;
      Result.Bottom := RoundS( Result.Bottom * FPixelsPerInch /
                               GetDeviceCaps( FPrinter.Handle, LOGPIXELSY ) ) ;
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

  function TGIS_PrinterPreview.fget_PPI
    : Integer ;
  begin
    Result := FPixelsPerInch ;
  end;

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
    NewPage ;
  end;

  procedure TGIS_PrinterPreview.EndDoc;
  begin
    FPrinting := False ;
  end;

  function TGIS_PrinterPreview.BeginDocEx ;
  begin
    Result := False ;
  end;

  procedure TGIS_PrinterPreview.EndDocEx(
    const _init_locally : Boolean
  ) ;
  begin
  end ;

  procedure TGIS_PrinterPreview.NewPage;
  var
    rct : TRect ;
  begin
    inc( FPageNumber ) ;

    FreeObject( FBitmap ) ;
    FBitmap := TBitmap.Create ;

    rct := fget_PrintArea ;
    FBitmap.PixelFormat := pf32bit ;
    FBitmap.Width  := rct.Right - rct.Left ;
    FBitmap.Height := rct.Bottom - rct.Top ;
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
    const _printer : TPrinter ;
    const _customFormat : String
  ) ;
  begin
    inherited Create( _printer ) ;
    applyCustomFormat( _customFormat ) ;
  end ;

//==============================================================================
// TGIS_PrinterTemplate
//==============================================================================

  constructor TGIS_PrinterTemplate.Create(
    const _pageSize  : TPoint ;
    const _printArea : TRect ;
    const _ppi       : Integer
  ) ;
  begin
    inherited Create( nil ) ;
    FPageSize := _pageSize ;
    FPrintArea := _printArea ;
    FPPI := _ppi ;
  end ;

  function TGIS_PrinterTemplate.fget_PageSize
    : TPoint  ;
  begin
    Result.X := RoundS( FPageSize.X  * FPixelsPerInch / FPPI ) ;
    Result.Y := RoundS( FPageSize.Y  * FPixelsPerInch / FPPI ) ;
  end ;

  function TGIS_PrinterTemplate.fget_PageHeight
    : Integer ;
  begin
    Result := RoundS( FPrintArea.Height  * FPixelsPerInch / FPPI ) ;
  end ;

  function TGIS_PrinterTemplate.fget_PageWidth
    : Integer ;
  begin
    Result := RoundS( FPrintArea.Width  * FPixelsPerInch / FPPI ) ;
  end ;

  function TGIS_PrinterTemplate.fget_PrintArea
    : TRect   ;
  begin
    Result.Left   := RoundS( FPrintArea.Left   * FPixelsPerInch / FPPI ) ;
    Result.Top    := RoundS( FPrintArea.Top    * FPixelsPerInch / FPPI ) ;
    Result.Right  := RoundS( FPrintArea.Right  * FPixelsPerInch / FPPI ) ;
    Result.Bottom := RoundS( FPrintArea.Bottom * FPixelsPerInch / FPPI ) ;
  end ;

{==================================== END =====================================}
end.

