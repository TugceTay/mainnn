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
  Basic print manager class.
}

{$IFDEF DCC}
  unit GisPrintManagerAbstract ;
  {$HPPEMIT '#pragma link "GisPrintManagerAbstract"'}
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
    System.ComponentModel,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,

    GisInterfaces,
    GisTypes,
    GisTypesUI,
    GisConfig,
    GisTemplatePrint,
    GisPrintBuilder ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}

const
  /// <summary>
  ///   Size of caching device for printing.
  /// </summary>
  PRINTER_TILE_SIZE = 9 * 300 ;

type

  /// <summary>
  ///   Basic print manager class to be used as a base class for print managers.
  /// </summary>
  TGIS_PrintManagerAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                              class ( TGIS_UncountedInterfacedObject )

    private
      /// <summary>
      ///   True if current print is in PDF mode.
      /// </summary>
      pdfPrint : Boolean ;

      pdfPageSize : TPoint ;

    private
      oConfig : TGIS_Config ;

    protected

      /// <summary>
      ///   If true then controls' Glow property is set to False upon printing.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Available only via metadata:
      ///     'TGIS_PrintManager.DisableGlow'.
      ///   </para>
      ///   <para>
      ///     Default value is False.
      ///   </para>
      /// </remarks>
      metPrintDisableGlow : Boolean ;

      /// <summary>
      ///   Width (in pixels) of the margin which be searched around every printed tile.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Available only via metadata:
      ///     'TGIS_PrintManager.OverlappedExtentMargin'.
      ///   </para>
      ///   <para>
      ///     Default value is -1.
      ///   </para>
      /// </remarks>
      metPrintOverlappedExtentMargin : Boolean ;

    protected

      /// <summary>
      ///   Draw a box defined in the .tpl or .ttktemplate by 'BOX'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the box
      /// </param>
      /// <param name="_color">
      ///   color of the box
      /// </param>
      /// <param name="_framecolor">
      ///   color of the box's frame
      /// </param>
      /// <param name="_framewidth">
      ///   width of the box's frame
      /// </param>
      procedure draw_box        ( const _rect       : TRect       ;
                                  const _color      : TGIS_Color  ;
                                  const _framecolor : TGIS_Color ;
                                  const _framewidth : Integer
                                ) ; virtual; abstract;

      /// <summary>
      ///   Draw a map defined in the .tpl or .ttktemplate by 'MAP'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the map
      /// </param>
      /// <param name="_viewer">
      ///   viewer object
      /// </param>
      /// <param name="_extent">
      ///   extent to be drawn
      /// </param>
      /// <param name="_background">
      ///   background color flag
      /// </param>
      /// <param name="_scale">
      ///   scale
      /// </param>
      /// <param name="_rextent">
      ///   real extent
      /// </param>
      /// <param name="_print">
      ///   if False, the procedure is used only to calculate map scale & extent
      /// </param>
      procedure draw_map        ( const _rect       : TRect       ;
                                  const _viewer     : IGIS_Viewer ;
                                  const _extent     : TGIS_Extent ;
                                  const _background : Boolean     ;
                                  var   _scale      : Double      ;
                                  var   _rextent    : TGIS_Extent ;
                                        _print      : Boolean
                                ) ; virtual; abstract;

      /// <summary>
      ///   Draw a control defined in the .tpl or .ttktemplate
      ///   by 'LEGEND', 'SCALE' or 'NORTHARROW'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the control
      /// </param>
      /// <param name="_control">
      ///   control object
      /// </param>
      /// <param name="_scale">
      ///   scale
      /// </param>
      procedure draw_control    ( const _rect    : TRect       ;
                                  const _control : IGIS_PrintableControl ;
                                  const _scale   : Double
                                ) ; virtual; abstract;

      /// <summary>
      ///   Draw a legend defined in the .tpl or .ttktemplate by 'LEGEND'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the control
      /// </param>
      /// <param name="_control">
      ///   control object
      /// </param>
      /// <param name="_scale">
      ///   scale
      /// </param>
      /// <param name="_compactView">
      ///   compact view flag
      /// </param>
      /// <param name="_iconStyle">
      ///   icon style
      /// </param>
      /// <param name="_reverseOrder">
      ///   reverse order flag
      /// </param>
      /// <param name="_font">
      ///   font name
      /// </param>
      /// <param name="_fontSize">
      ///   font size
      /// </param>
      /// <param name="_fontColor">
      ///   font color
      /// </param>
      procedure draw_legend     ( const _rect        : TRect  ;
                                  const _control     : IGIS_PrintableControl ;
                                  const _scale       : Double ;
                                  const _compactView : String ;
                                  const _iconStyle   : String ;
                                  const _reverseOrder: String ;
                                  const _font        : String ;
                                  const _fontSize    : String ;
                                  const _fontColor   : String
                                ) ; virtual ; abstract ;

      /// <summary>
      ///   Draw a scale defined in the .tpl or .ttktemplate by 'SCALE'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the control
      /// </param>
      /// <param name="_control">
      ///   control object
      /// </param>
      /// <param name="_scale">
      ///   scale
      /// </param>
      /// <param name="_dividers">
      ///   number of dividers
      /// </param>
      /// <param name="_dividerColor1">
      ///   divider color 1
      /// </param>
      /// <param name="_dividerColor2">
      ///   divider color 2
      /// </param>
      /// <param name="_font">
      ///   font name
      /// </param>
      /// <param name="_fontSize">
      ///   font size
      /// </param>
      /// <param name="_fontColor">
      ///   font color
      /// </param>
      procedure draw_scale      ( const _rect        : TRect  ;
                                  const _control     : IGIS_PrintableControl ;
                                  const _scale       : Double ;
                                  const _dividers    : String ;
                                  const _dividerColor1 : String ;
                                  const _dividerColor2 : String ;
                                  const _font        : String ;
                                  const _fontSize    : String ;
                                  const _fontColor   : String
                                ) ; virtual ; abstract ;

      /// <summary>
      ///   Draw a scale defined in the .tpl or .ttktemplate by 'SCALE'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the control
      /// </param>
      /// <param name="_control">
      ///   control object
      /// </param>
      /// <param name="_scale">
      ///   scale
      /// </param>
      /// <param name="_style">
      ///   north arrow style
      /// </param>
      /// <param name="_color1">
      ///   color 1
      /// </param>
      /// <param name="_color2">
      ///   color 2
      /// </param>
      /// <param name="_path">
      ///   path to a graphic
      /// </param>
      procedure draw_northArrow ( const _rect        : TRect  ;
                                  const _control     : IGIS_PrintableControl ;
                                  const _scale       : Double ;
                                  const _style       : String ;
                                  const _color1      : String ;
                                  const _color2      : String ;
                                  const _path        : String
                                ) ; virtual ; abstract ;

      /// <summary>
      ///   Draw an image defined in the .tpl or .ttktemplate by 'GRAPHIC'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the graphic
      /// </param>
      /// <param name="_graphics">
      ///   graphic object
      /// </param>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure draw_graphic    ( const _rect     : TRect   ;
                                  const _graphics : TGIS_TemplateGraphic ;
                                  const _path     : String
                                ) ; virtual ; abstract ;

      /// <summary>
      ///   Draw a text defined in the .tpl or .ttktemplate by 'TEXT'.
      /// </summary>
      /// <param name="_rect">
      ///   location of the text
      /// </param>
      /// <param name="_text">
      ///   text to be drawn
      /// </param>
      /// <param name="_name">
      ///   font name
      /// </param>
      /// <param name="_style">
      ///   font style
      /// </param>
      /// <param name="_size">
      ///   font size
      /// </param>
      /// <param name="_color">
      ///   color of the text
      /// </param>
      /// <param name="_align">
      ///   alignment
      /// </param>
      /// <param name="_bgColor">
      ///   background color, use None for transparent
      /// </param>
      /// <param name="_bgWidth">
      ///   background border width
      /// </param>
      procedure draw_text       ( const _rect     : TRect       ;
                                  const _text     : String      ;
                                  const _name     : String      ;
                                  const _style    : TGIS_FontStyles ;
                                  const _size     : Integer     ;
                                  const _color    : TGIS_Color  ;
                                  const _align    : TGIS_LabelAlignment ;
                                  const _bgColor  : TGIS_Color  ;
                                  const _bgWidth  : Integer
                                ) ; virtual; abstract;

      /// <summary>
      ///   Draw a frame defined in the .tpl by 'FRAME'
      ///   or in the .ttktemplate by 'BOX' with transparent interior .
      /// </summary>
      /// <param name="_rect">
      ///   location of the frame
      /// </param>
      /// <param name="_color">
      ///   color of the frame
      /// </param>
      /// <param name="_width">
      ///   width of the frame
      /// </param>
      procedure draw_frame      ( const _rect   : TRect       ;
                                  const _color  : TGIS_Color  ;
                                  const _width  : Integer
                                ) ; virtual; abstract;

      /// <summary>
      ///   Print a page according to defined template.
      /// </summary>
      /// <param name="_dpi">
      ///   printer resolution
      /// </param>
      /// <param name="_print_area">
      ///   print area
      /// </param>
      /// <param name="_page_width">
      ///   page width
      /// </param>
      /// <param name="_page_height">
      ///   page height
      /// </param>
      procedure processTemplatePrinting
                                ( _dpi          : Integer     ;
                                  _print_area   : TRect       ;
                                  _page_width   : Integer     ;
                                  _page_height  : Integer
                                ) ;

      /// <summary>
      ///   Init element by calling events.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_element">
      ///   element to init
      /// </param>
      procedure init_element    ( _sender       : TObject       ;
                                  _element      : TGIS_PrintLayoutElement
                                ) ;

      /// <summary>
      ///   Draw element by calling events.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_element">
      ///   element to draw
      /// </param>
      procedure draw_element    ( _sender       : TObject       ;
                                  _element      : TGIS_PrintLayoutElement
                                ) ;

      /// <summary>
      ///   Check element by calling events.
      /// </summary>
      /// <param name="_sender">
      ///   sender object
      /// </param>
      /// <param name="_element">
      ///   element to check
      /// </param>
      procedure check_element   ( _sender       : TObject       ;
                                  _element      : TGIS_PrintLayoutElement
                                ) ;

      /// <summary>
      ///   Return PAGESIZE value from the template file.
      /// </summary>
      /// <returns>
      ///   page size
      /// </returns>
      function GetPdfPageSize : TPoint ;
    protected

      /// <summary>
      ///   Force a draft printing at 100PPI
      /// </summary>
      FPrinterModeDraft : Boolean ;

      /// <summary>
      ///   Force a bitmap printing.
      /// </summary>
      FModeForceBitmap : Boolean ;

      /// <summary>
      ///   Size of tile (in pixels) used when printing semitransparent
      ///   layers. Default is optimized for A4.
      /// </summary>
      FTileSize : Integer ;

      /// <summary>
      ///   Text of title that will appear on a printed page.
      /// </summary>
      FTitle : String  ;

      /// <summary>
      ///   Font of title that will appear on a printed page.
      /// </summary>
      FTitleFont : TGIS_Font ;

      /// <summary>
      ///   Text of subtitle that will appear on a printed page.
      /// </summary>
      FSubtitle : String ;

      /// <summary>
      ///   Font of subtitle that will appear on a printed page.
      /// </summary>
      FSubtitleFont : TGIS_Font ;

      /// <summary>
      ///   Text of footer that will appear on a printed page.
      /// </summary>
      FFooter : String ;

      /// <summary>
      ///   Font for the header that will appear on a printed page.
      /// </summary>
      FFooterFont : TGIS_Font ;

      /// <summary>
      ///   Print template object.
      /// </summary>
      FTemplate : TGIS_TemplatePrint ;

      /// <summary>
      ///   Margin (in pixels) to scan around a printed tile.
      /// </summary>
      FOverlappedExtentMargin : Integer ;

      /// <summary>
      ///   Used to force rendering labels.
      /// </summary>
      FTemporaryVisibleExtent : TGIS_Extent ;

    protected

      procedure fset_TileSize         ( const _value : Integer
                                      ) ; virtual;
      procedure fset_Template         ( const _value : TGIS_TemplatePrint
                                      ) ; virtual;

    protected
      /// <summary>
      ///   Perform component cleanup
      /// </summary>
      {$IFNDEF MANAGED}
        procedure doDestroy      ; override ;
      {$ELSE}
        procedure doDestroy      ; virtual ;
      {$ENDIF}

    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create         ;

      {$IFNDEF OXYGENE}
        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        destructor  Destroy      ; override;
      {$ENDIF}

      /// <summary>
      ///   Print a map given by the viewer using default printer.
      /// </summary>
      /// <param name="_viewer">
      ///    viewer with a map to print
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when _viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure   Print          ( const _viewer  : IGIS_Viewer
                                 ) ; overload; virtual; abstract;

      /// <summary>
      ///   Print a map given by the viewer using passed _printer.
      /// </summary>
      /// <param name="_viewer">
      ///    viewer with a map to print
      /// </param>
      /// <param name="_printer">
      ///    printer to be used
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when _viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure   Print        ( const _viewer   : IGIS_Viewer ;
                                 const _printer  : TObject
                               ) ; overload; virtual; abstract;

      /// <summary>
      ///   Print a map given by the viewer using passed _printer.
      /// </summary>
      /// <param name="_viewer">
      ///    viewer with a map to print
      /// </param>
      /// <param name="_printer">
      ///    printer to be used
      /// </param>
      /// <param name="_scale">
      ///    scale factor used during print (for printing scale etc);
      ///    if scale=0 then scale will be calculated automatically to fit
      ///    the _extent
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when _viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure   Print        ( const _viewer   : IGIS_Viewer ;
                                 const _printer  : TObject     ;
                                 var   _scale    : Double
                               ) ; overload; virtual; abstract;

      /// <summary>
      ///   Get the actual scale of the map to be printed.
      ///   It can be used as an input parameter in DrawControl method.
      /// </summary>
      /// <param name="_viewer">
      ///   map to print
      /// </param>
      /// <param name="_extent">
      ///   map extent to be used
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      /// <param name="_scale">
      ///   calculated scale of the map
      ///   scale factor used during printing (for printing scale etc);
      ///   if scale=0 then scale will be calculated automatically to fit
      ///   the _extent
      /// </param>
      procedure   GetDrawingParams
                               ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect   ;
                                 var   _scale   : Double
                               ) ; virtual ; abstract ;

      /// <summary>
      ///   Print a map given by the viewer using passed _printer.
      /// </summary>
      /// <param name="_viewer">
      ///   map to print
      /// </param>
      /// <param name="_extent">
      ///   map extent to be used
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      /// <param name="_scale">
      ///   scale factor used during printing (for printing scale etc);
      ///   if scale=0 then scale will be calculated automatically to fit
      ///   the _extent
      /// </param>
      procedure   DrawMap      ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect   ;
                                 var   _scale   : Double
                               ) ; overload ; virtual ; abstract ;

      /// <summary>
      ///   Print a map given by the viewer.
      /// </summary>
      /// <param name="_viewer">
      ///   map to print
      /// </param>
      /// <param name="_extent">
      ///   map extent to be used
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      /// <param name="_scale">
      ///   scale factor used during printing (for printing scale etc);
      ///   if scale=0 then scale will be calculated automatically to fit
      ///   the _extent
      /// </param>
      /// <param name="_rextent">
      ///   real printed map extent
      /// </param>
      procedure   DrawMap      ( const _viewer  : IGIS_Viewer ;
                                 const _extent  : TGIS_Extent ;
                                 const _rect    : TRect   ;
                                 var   _scale   : Double  ;
                                 var   _rextent : TGIS_Extent
                               ) ; overload ; virtual ; abstract ;

      /// <summary>
      ///   Print a control.
      /// </summary>
      /// <param name="_control">
      ///   control to be printed
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      procedure   DrawControl  ( const _control : IGIS_PrintableControl ;
                                 const _rect    : TRect
                               ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Print a control.
      /// </summary>
      /// <param name="_control">
      ///   control to be printed
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      /// <param name="_scale">
      ///   scale factor used during printing;
      /// </param>
      procedure   DrawControl  ( const _control : IGIS_PrintableControl ;
                                 const _rect    : TRect ;
                                 const _scale   : Double
                               ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Print a graphic.
      /// </summary>
      /// <param name="_path">
      ///   path to the graphic
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      procedure   DrawGraphic  ( const _path    : String ;
                                 const _rect    : TRect
                               ) ; overload ; virtual; abstract;

      /// <summary>
      ///   Print a graphic.
      /// </summary>
      /// <param name="_graphic">
      ///   graphic to print
      /// </param>
      /// <param name="_rect">
      ///   rectangle to be printed (area of print on paper)
      /// </param>
      procedure   DrawGraphic  ( const _graphic : TGIS_TemplateGraphic ;
                                 const _rect    : TRect
                               ) ; overload ; virtual; abstract;

    public

      /// <summary>
      ///   <para>
      ///     Size of tile (in pixels) used when printing semitransparent
      ///     layers. Minimum=128; Maximum=8192; Default is optimized for A4
      ///     single pass print.
      ///   </para>
      ///   <para>
      ///     Increase the size if you want to speed-up the printing. But be
      ///     aware that bigger size can cause memory printer problems.
      ///   </para>
      /// </summary>
      {$IFDEF CLR}
        [DefaultValue(PRINTER_TILE_SIZE)]
      {$ENDIF}
      property TileSize         : Integer      read  FTileSize
                                               write fset_TileSize
                                               {$IFDEF OXYGENE}
                                                 ;
                                               {$ELSE}
                                               default PRINTER_TILE_SIZE  ;
                                               {$ENDIF}

      /// <summary>
      ///   Text for the footer that will appear on a printed page.
      /// </summary>
      property Footer           : String       read  FFooter
                                               write FFooter              ;

      /// <summary>
      ///   Font for the header that will appear on a printed page.
      ///   DefaultValue: Arial, 10pt, Black.
      /// </summary>
      property FooterFont       : TGIS_Font    read  FFooterFont
                                               write FFooterFont          ;

      /// <summary>
      ///   Text for the subtitle that will appear on a printed page.
      /// </summary>
      property Subtitle         : String       read  FSubtitle
                                               write FSubtitle            ;

      /// <summary>
      ///   Font for the subtitle that will appear on a printed page.
      ///   DefaultValue: Arial, 10pt, Bold, Black.
      /// </summary>
      property SubtitleFont     : TGIS_Font    read  FSubtitleFont
                                               write FSubtitleFont        ;

      /// <summary>
      ///   Text for the title that will appear on a printed page.
      /// </summary>
      property Title            : String       read  FTitle
                                               write FTitle               ;

      /// <summary>
      ///   Font for the title that will appear on a printed page.
      ///   DefaultValue: Arial, 18pt, Bold, Black.
      /// </summary>
      property TitleFont        : TGIS_Font    read  FTitleFont
                                               write FTitleFont  ;

      /// <summary>
      ///   Print template object.
      /// </summary>
      {#ownership:set:release}
      property Template         : TGIS_TemplatePrint
                                               read  FTemplate
                                               write fset_Template ;

      /// <summary>
      ///   Margin (in pixels) to scan around a printed tile.
      /// </summary>
      /// <remarks>
      ///   This is the number of pixels added to the edges of the printed tile
      ///   that increases its visible extent.
      ///   Used to draw shapes (and labels) placed at tiles' boundaries.
      ///   By default = -1, then a 50-inch margin is taken into account.
      /// </remarks>
      property OverlappedExtentMargin
                                : Integer      read  FOverlappedExtentMargin
                                               write FOverlappedExtentMargin ;

      /// <summary>
      ///   Area for rendered labels.
      /// </summary>
      /// <remarks>
      ///   Default value is GisNoWorld, it means that only labels
      ///   that fully fit inside the printed map extent will be rendered.
      ///   If the area exceeds the exported extent, then the label fragments
      ///   may appear on the outer edges.
      /// </remarks>
      property TemporaryVisibleExtent : TGIS_Extent
                                               read  FTemporaryVisibleExtent
                                               write FTemporaryVisibleExtent ;
  end ;

const
  {#gendoc:hide}
  METADATA_PRINTDISABLEGLOW
    = 'TGIS_PrintManager.DisableGlow';

  {#gendoc:hide}
  METADATA_PRINTOVERLAPPEDEXTENTMARGIN
    = 'TGIS_PrintManager.OverlappedExtentMargin';

//##############################################################################
implementation

{$IFDEF DCC}
uses
  System.SysUtils,
  System.Math,
  GisRtl,
  GisClasses,
  GisFunctions,
  GisConfigIni,
  GisConfigXml,
  GisXmlDoc,
  GisInternals,
  GisResource,
  GisParams ;
{$ENDIF}

const
  /// Draft mode printer PPI.
  PRINTER_PPI_DRAFT    = 100 ;
  /// Standard mode printer PPI (to be used to draw transparent layers etc).
  PRINTER_PPI_STANDARD = 300 ;

//=============================================================================
// TGIS_PrintManagerAbstract
//=============================================================================

  procedure TGIS_PrintManagerAbstract.fset_TileSize(
    const _value : Integer
  ) ;
  begin
    if      _value <  128 then FTileSize :=  128
    else if _value > 8192 then FTileSize := 8192
    else                       FTileSize := _value ;
  end ;

  procedure TGIS_PrintManagerAbstract.fset_Template(
    const _value : TGIS_TemplatePrint
  ) ;
  begin
    if FTemplate = _value then exit ;
    FreeObject( FTemplate ) ;
    FTemplate := _value ;
  end ;

  procedure TGIS_PrintManagerAbstract.init_element(
    _sender       : TObject       ;
    _element      : TGIS_PrintLayoutElement
  ) ;
  var
    elm_map : TGIS_PrintLayoutMap ;
    scale   : Double ;
    rextent : TGIS_Extent ;
  begin
    case _element.ElementType of
      TGIS_PrintLayoutElementType.Map :
        begin
          elm_map := TGIS_PrintLayoutMap( _element ) ;
          if assigned( elm_map.Viewer ) then begin
            scale := elm_map.Scale ;
            draw_map( elm_map.Location.Rectangle, elm_map.Viewer, elm_map.Extent,
                      elm_map.Background, scale, rextent, False ) ;
            elm_map.Scale := scale ;
            elm_map.RealExtent := rextent ;
          end;
        end ;
    end ;
  end ;

  procedure TGIS_PrintManagerAbstract.draw_element(
    _sender       : TObject       ;
    _element      : TGIS_PrintLayoutElement
  ) ;
  var
    elm_box : TGIS_PrintLayoutBox ;
    elm_map : TGIS_PrintLayoutMap ;
    elm_leg : TGIS_PrintLayoutLegend ;
    elm_scl : TGIS_PrintLayoutScale ;
    elm_nar : TGIS_PrintLayoutNorthArrow ;
    elm_grh : TGIS_PrintLayoutGraphic ;
    elm_txt : TGIS_PrintLayoutText ;
    elm_frm : TGIS_PrintLayoutFrame ;
    scale   : Double ;
    rextent : TGIS_Extent ;
  begin
    case _element.ElementType of
      TGIS_PrintLayoutElementType.Box        :
        begin
          elm_box := TGIS_PrintLayoutBox( _element ) ;
          draw_box( elm_box.Location.Rectangle, elm_box.Color,
                    elm_box.FrameColor, elm_box.FrameWidth.Width ) ;
        end ;
      TGIS_PrintLayoutElementType.Map        :
        begin
          elm_map := TGIS_PrintLayoutMap( _element ) ;
          if assigned( elm_map.Viewer ) then begin
            scale := elm_map.Scale ;
            draw_map( elm_map.Location.Rectangle, elm_map.Viewer, elm_map.Extent,
                      elm_map.Background, scale, rextent, True ) ;
            elm_map.Scale := scale ;
            elm_map.RealExtent := rextent ;
          end;
        end ;
      TGIS_PrintLayoutElementType.Legend     :
        begin
          elm_leg := TGIS_PrintLayoutLegend( _element ) ;
          if assigned( elm_leg ) then
            draw_legend( elm_leg.Location.Rectangle, elm_leg.Control, elm_leg.Scale,
                         elm_leg.CompactViewStr, elm_leg.DrawIconStyleStr,
                         elm_leg.ReverseOrderStr,
                         elm_leg.FontStr, elm_leg.FontSizeStr,
                         elm_leg.FontColorStr ) ;
        end ;
      TGIS_PrintLayoutElementType.Scale      :
        begin
          elm_scl := TGIS_PrintLayoutScale( _element ) ;
          if assigned( elm_scl ) then
            draw_scale( elm_scl.Location.Rectangle, elm_scl.Control, elm_scl.Scale,
                        elm_scl.DividersStr,
                        elm_scl.DividerColor1Str, elm_scl.DividerColor2Str,
                        elm_scl.FontStr, elm_scl.FontSizeStr,
                        elm_scl.FontColorStr ) ;
        end ;
      TGIS_PrintLayoutElementType.NorthArrow :
        begin
          elm_nar := TGIS_PrintLayoutNorthArrow( _element ) ;
          if assigned( elm_nar ) then
            draw_northArrow( elm_nar.Location.Rectangle, elm_nar.Control, elm_nar.Scale,
                             elm_nar.StyleStr, elm_nar.Color1Str, elm_nar.Color2Str,
                             elm_nar.PathStr ) ;
        end ;
      TGIS_PrintLayoutElementType.Graphic :
        begin
          elm_grh := TGIS_PrintLayoutGraphic( _element ) ;
          if _element.Index >= 0 then
            draw_graphic( elm_grh.Location.Rectangle, elm_grh.Graphic, elm_grh.Path ) ;
        end ;
      TGIS_PrintLayoutElementType.Text     :
        begin
          elm_txt := TGIS_PrintLayoutText( _element ) ;
          draw_text( elm_txt.Location.Rectangle, elm_txt.Text, elm_txt.Font, elm_txt.Style,
                     elm_txt.Size, elm_txt.Color, elm_txt.Align,
                     elm_txt.BackgroundColor, elm_txt.BackgroundWidth.Width
                   ) ;
        end ;
      TGIS_PrintLayoutElementType.Frame    :
        begin
          elm_frm := TGIS_PrintLayoutFrame( _element ) ;
          draw_frame( elm_frm.Location.Rectangle, elm_frm.Color, elm_frm.Width.Width ) ;
        end ;
    end ;
  end ;

  procedure TGIS_PrintManagerAbstract.check_element(
    _sender       : TObject       ;
    _element      : TGIS_PrintLayoutElement
  ) ;
  var
    elm_page : TGIS_PrintLayoutPage ;
  begin
    case _element.ElementType of
      TGIS_PrintLayoutElementType.Page      :
        begin
          elm_page := TGIS_PrintLayoutPage( _element ) ;
          pdfPageSize := Point( RoundS( elm_page.Width.Width  * 72/96 ),
                                RoundS( elm_page.Height.Width * 72/96 )
                               ) ;
        end ;
    end ;
  end ;

  procedure TGIS_PrintManagerAbstract.processTemplatePrinting(
    _dpi         : Integer ;
    _print_area  : TRect ;
    _page_width  : Integer ;
    _page_height : Integer
  ) ;
  var
    tpl_builder : TGIS_TemplatePrintBuilder ;
  begin
    tpl_builder := TGIS_TemplatePrintBuilder.Create( FTemplate ) ;
    try
      {$IFDEF OXYGENE}
        tpl_builder.InitElementEvent := @init_element ;
        tpl_builder.ElementEvent     := @draw_element ;
      {$ELSE}
        tpl_builder.InitElementEvent := init_element ;
        tpl_builder.ElementEvent     := draw_element ;
      {$ENDIF}

      tpl_builder.ProcessTemplate(
        _dpi,
        _print_area,
        _page_width,
        _page_height
      ) ;
    finally
      FreeObject( tpl_builder ) ;
    end ;
  end ;

  function TGIS_PrintManagerAbstract.GetPdfPageSize : TPoint ;
  var
    tpl_builder : TGIS_TemplatePrintBuilder ;
  begin
    Result := Point( 0, 0 ) ;

    if not assigned( FTemplate ) then exit ;

    tpl_builder := TGIS_TemplatePrintBuilder.Create( FTemplate ) ;
    try
      {$IFDEF OXYGENE}
        tpl_builder.ElementEvent   := @check_element ;
      {$ELSE}
        tpl_builder.ElementEvent   := check_element ;
      {$ENDIF}

      pdfPageSize := Point( 0, 0 ) ;
      tpl_builder.ProcessTemplate( 96, Rect( 0, 0, -1, -1 ), -1, -1 ) ;
    finally
      FreeObject( tpl_builder ) ;
    end ;

    Result := pdfPageSize ;

  end ;

  constructor TGIS_PrintManagerAbstract.Create ;
  begin
    inherited Create ;

    FPrinterModeDraft       := False ;
    FModeForceBitmap := False ;
    FTileSize        := PRINTER_TILE_SIZE ;

    FTitle := '' ;
    FTitleFont          := TGIS_Font.Create ;
    FTitleFont.Color    := TGIS_Color.Black ;
    FTitleFont.Name     := 'Arial' ;
    FTitleFont.Size     := 18 ;
    FTitleFont.Style    := GisGetFontStyle( TGIS_FontStyle.Bold ) ;
    FSubtitle := '' ;
    FSubtitleFont       := TGIS_Font.Create ;
    FSubtitleFont.Color := TGIS_Color.Black ;
    FSubtitleFont.Name  := 'Arial' ;
    FSubtitleFont.Size  := 10 ;
    FSubtitleFont.Style := GisGetFontStyle( TGIS_FontStyle.Bold ) ;
    FFooter := '' ;
    FFooterFont         := TGIS_Font.Create ;
    FFooterFont.Color   := TGIS_Color.Black ;
    FFooterFont.Name    := 'Arial' ;
    FFooterFont.Size    := 10 ;
    FFooterFont.Style   := GisGetEmptyFontStyle ;

    oConfig := nil ;
    FTemplate := nil ;

    metPrintDisableGlow := GisMetadataAsBoolean(
      METADATA_PRINTDISABLEGLOW,
      False
    ) ;

    FOverlappedExtentMargin := GisMetadataAsInteger(
      METADATA_PRINTOVERLAPPEDEXTENTMARGIN,
      -1
    ) ;

    FTemporaryVisibleExtent := GisNoWorld ;

  end ;

  procedure TGIS_PrintManagerAbstract.doDestroy ;
  begin
    FreeObject( FTitleFont    ) ;
    FreeObject( FSubtitleFont ) ;
    FreeObject( FFooterFont   ) ;

    FreeObject( oConfig ) ;
    FreeObject( FTemplate ) ;

    {$IFNDEF MANAGED}
      inherited ;
    {$ENDIF}
  end ;

  {$IFNDEF OXYGENE}
    destructor TGIS_PrintManagerAbstract.Destroy ;
    begin
      doDestroy ;
      inherited ;
    end;
  {$ENDIF}

{==================================== END =====================================}
end.

