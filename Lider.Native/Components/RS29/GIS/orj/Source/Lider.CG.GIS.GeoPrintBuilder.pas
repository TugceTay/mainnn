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
  Print builder and template layout classes.
}

{$IFDEF DCC}
  unit GisPrintBuilder ;
  {$HPPEMIT '#pragma link "GisPrintBuilder"'}
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
    System.Collections,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.Generics.Collections,
    GisInterfaces,
    GisRtl,
    GisXmlDoc,
    GisTypesUI,
    GisTypes,
    GisTemplatePrint ;
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

  {$REGION 'TGIS_PrintLayoutElement'}
type
  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout element types.
  /// </summary>
  TGIS_PrintLayoutElementType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   box element
    /// </summary>
    Box,
    /// <summary>
    ///   map element
    /// </summary>
    Map,
    /// <summary>
    ///   legend element
    /// </summary>
    Legend,
    /// <summary>
    ///   scale element
    /// </summary>
    Scale,
    /// <summary>
    ///   north arrow element
    /// </summary>
    NorthArrow,
    /// <summary>
    ///   graphic element
    /// </summary>
    Graphic,
    /// <summary>
    ///   text element
    /// </summary>
    Text,
    /// <summary>
    ///   frame element
    /// </summary>
    Frame,
    /// <summary>
    ///   page element
    /// </summary>
    Page
  ) ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout control types.
  /// </summary>
  TGIS_PrintLayoutControlType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   scale control
    /// </summary>
    Scale,
    /// <summary>
    ///   north arrow control
    /// </summary>
    NorthArrow,
    /// <summary>
    ///   legend control
    /// </summary>
    Legend
  ) ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout units.
  /// </summary>
  TGIS_PrintLayoutUnits  = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   cm unit
    /// </summary>
    uCm,
    /// <summary>
    ///   nn unit
    /// </summary>
    uMm,
    /// <summary>
    ///   in unit
    /// </summary>
    uIn,
    /// <summary>
    ///   pt unit
    /// </summary>
    uPt,
    /// <summary>
    ///   px unit
    /// </summary>
    uPx
  ) ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout anchor types.
  /// </summary>
  TGIS_PrintLayoutAnchor = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   left margin
    /// </summary>
    aLeftMargin   = $01,
    /// <summary>
    ///   top margin
    /// </summary>
    aTopMargin    = $02,
    /// <summary>
    ///   right margin
    /// </summary>
    aRightMargin  = $03,
    /// <summary>
    ///   bottom margin
    /// </summary>
    aBottomMargin = $04,
    /// <summary>
    ///   left border
    /// </summary>
    aLeftBorder   = $05,
    /// <summary>
    ///   top border
    /// </summary>
    aTopBorder    = $06,
    /// <summary>
    ///   right border
    /// </summary>
    aRightBorder  = $07,
    /// <summary>
    ///   bottom border
    /// </summary>
    aBottomBorder = $08
  ) ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Snap parameters encapsulation.
  /// </summary>
  TGIS_PrintLayoutSnap = {$IFDEF OXYGENE} public class {$ELSE} record {$ENDIF}
    public
      /// <summary>
      ///   snap value
      /// </summary>
      Value     : Double ;
      /// <summary>
      ///   snap unit
      /// </summary>
      Units     : TGIS_PrintLayoutUnits ;

    public

      /// <summary>
      ///   Get default location.
      /// </summary>
      /// <returns>
      ///   default location
      /// </returns>
      class function CreateDefault : TGIS_PrintLayoutSnap ; static ;

      /// <summary>
      ///   Get current snap as text.
      /// </summary>
      /// <returns>
      ///   snap as text
      /// </returns>
      function  AsText : String ;

      /// <summary>
      ///   Set new value.
      /// </summary>
      /// <param name="_values">
      ///   new values in text form
      /// </param>
      procedure SetValues( const _values   : String
                         ) ;

      /// <summary>
      ///   Snaps according to Value and Units.
      /// </summary>
      /// <param name="_v">
      ///   value to snap
      /// </param>
      /// <param name="_u">
      ///   unit of the value
      /// </param>
      /// <param name="_a">
      ///   layout anchor
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
      /// <param name="_dpi">
      ///   printer ppi
      /// </param>
      /// <returns>
      ///   snapped value
      /// </returns>
      function Round     ( const _v        : Single ;
                           const _u        : TGIS_PrintLayoutUnits ;
                           const _a        : TGIS_PrintLayoutAnchor ;
                           const _print_area
                                           : TRect ;
                           const _page_width
                                           : Integer ;
                           const _page_height
                                           : Integer ;
                           const _dpi      : Integer
                         ) : Double ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout element position type.
  /// </summary>
  TGIS_PrintLayoutPosition = {$IFDEF OXYGENE} public class {$ELSE} record {$ENDIF}
    public
      /// <summary>
      ///   position value
      /// </summary>
      Value     : Double ;
      /// <summary>
      ///   position unit
      /// </summary>
      Units     : TGIS_PrintLayoutUnits ;
      /// <summary>
      ///   position anchor
      /// </summary>
      Anchor    : TGIS_PrintLayoutAnchor ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_value">
      ///   position value
      /// </param>
      /// <param name="_units">
      ///   position unit
      /// </param>
      /// <param name="_anchor">
      ///   position anchor
      /// </param>
      constructor Create( const _value     : Double ;
                          const _units     : TGIS_PrintLayoutUnits ;
                          const _anchor    : TGIS_PrintLayoutAnchor
                        ) ;
      /// <summary>
      ///   Get position value as text.
      /// </summary>
      /// <returns>
      ///   position as text
      /// </returns>
      function  AsText : String ;

      /// <summary>
      ///   Get position value as text used in tpl file.
      /// </summary>
      /// <returns>
      ///   position as text
      /// </returns>
      function  AsTplText : String ;

      /// <summary>
      ///   Set position values.
      /// </summary>
      /// <param name="_value">
      ///   position value
      /// </param>
      /// <param name="_units">
      ///   position unit
      /// </param>
      /// <param name="_anchor">
      ///   position anchor
      /// </param>
      procedure SetValues( const _value     : Double ;
                           const _units     : String ;
                           const _anchor    : TGIS_PrintLayoutAnchor
                          ) ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout element width type.
  /// </summary>
  TGIS_PrintLayoutWidth = {$IFDEF OXYGENE} public class {$ELSE} record {$ENDIF}
    public
      /// <summary>
      ///   width value
      /// </summary>
      Value     : Double ;
      /// <summary>
      ///   width unit
      /// </summary>
      Units     : TGIS_PrintLayoutUnits ;
      /// <summary>
      ///   width in pixels
      /// </summary>
      Width     : Integer ; // for drawing
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_value">
      ///   width value
      /// </param>
      /// <param name="_units">
      ///   width unit
      /// </param>
      constructor Create( const _value     : Double ;
                          const _units     : TGIS_PrintLayoutUnits
                        ) ;
      /// <summary>
      ///   Get width value as text.
      /// </summary>
      /// <returns>
      ///   width as text
      /// </returns>
      function AsText : String ;

      /// <summary>
      ///   Set position values.
      /// </summary>
      /// <param name="_value">
      ///   position value
      /// </param>
      /// <param name="_units">
      ///   position unit
      /// </param>
      procedure SetValues( const _value     : Double ;
                           const _units     : String
                          ) ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout element location type.
  /// </summary>
  TGIS_PrintLayoutLocation = {$IFDEF OXYGENE} public class {$ELSE} record {$ENDIF}
    public
      /// <summary>
      ///   Position of left corner.
      /// </summary>
      Left      : TGIS_PrintLayoutPosition ;
      /// <summary>
      ///   Position of right corner.
      /// </summary>
      Right     : TGIS_PrintLayoutPosition ;
      /// <summary>
      ///   Position of top corner.
      /// </summary>
      Top       : TGIS_PrintLayoutPosition ;
      /// <summary>
      ///   Position of bottom corner.
      /// </summary>
      Bottom    : TGIS_PrintLayoutPosition ;
      /// <summary>
      ///   Rectangle in pixels.
      /// </summary>
      Rectangle : TRect ; // for drawing

    public

      /// <summary>
      ///   Get default location.
      /// </summary>
      /// <returns>
      ///   default location
      /// </returns>
      class function CreateDefault : TGIS_PrintLayoutLocation ; static ;

      /// <summary>
      ///   Update location from pixel bounds.
      /// </summary>
      /// <param name="_bounds">
      ///   new bounds
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
      /// <param name="_dpi">
      ///   dpi value
      /// </param>
      procedure UpdateToRectangle  ( const _bounds      : TRect ;
                                     const _print_area  : TRect ;
                                     const _page_width  : Integer ;
                                     const _page_height : Integer ;
                                     const _dpi         : Integer
                                   ) ;
                                   {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Update location from pixel bounds.
      /// </summary>
      /// <param name="_bounds">
      ///   new bounds
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
      /// <param name="_dpi">
      ///   dpi value
      /// </param>
      procedure UpdateToRectangleEx( const _bounds      : TRect ;
                                     const _print_area  : TRect ;
                                     const _page_width  : Integer ;
                                     const _page_height : Integer ;
                                     const _dpi         : Integer
                                   ) ; overload ;
                                   {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Calculate Rectangle property according to current coordinates.
      /// </summary>
      /// <param name="_print_area">
      ///   print area
      /// </param>
      /// <param name="_page_width">
      ///   page width
      /// </param>
      /// <param name="_page_height">
      ///   page height
      /// </param>
      /// <param name="_dpi">
      ///   dpi value
      /// </param>
      procedure UpdateRectangle    ( const _print_area  : TRect ;
                                     const _page_width  : Integer ;
                                     const _page_height : Integer ;
                                     const _dpi         : Integer
                                   ) ; overload ;

      /// <summary>
      ///   Update location from pixel bounds.
      /// </summary>
      /// <param name="_bounds">
      ///   new bounds
      /// </param>
      /// <param name="_left">
      ///   if change the left coordinate
      /// </param>
      /// <param name="_top">
      ///   if change the top coordinate
      /// </param>
      /// <param name="_right">
      ///   if change the right coordinate
      /// </param>
      /// <param name="_bottom">
      ///   if change the bottom coordinate
      /// </param>
      /// <param name="_snap">
      ///   snap value
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
      /// <param name="_dpi">
      ///   dpi value
      /// </param>
      procedure UpdateRectangle    ( const _bounds      : TRect ;
                                     const _left        : Boolean ;
                                     const _top         : Boolean ;
                                     const _right       : Boolean ;
                                     const _bottom      : Boolean ;
                                     const _snap        : TGIS_PrintLayoutSnap ;
                                     const _print_area  : TRect ;
                                     const _page_width  : Integer ;
                                     const _page_height : Integer ;
                                     const _dpi         : Integer
                                   ) ; overload ;

      /// <summary>
      ///   Update location from pixel bounds.
      /// </summary>
      /// <param name="_delta_left">
      ///   change of the left coordinate
      /// </param>
      /// <param name="_delta_top">
      ///   change of the top coordinate
      /// </param>
      /// <param name="_delta_right">
      ///   change of the right coordinate
      /// </param>
      /// <param name="_delta_bottom">
      ///   change of the bottom coordinate
      /// </param>
      /// <param name="_snap">
      ///   snap value
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
      /// <param name="_dpi">
      ///   dpi value
      /// </param>
      procedure UpdateRectangle    ( const _delta_left   : Double ;
                                     const _delta_top    : Double ;
                                     const _delta_right  : Double ;
                                     const _delta_bottom : Double ;
                                     const _snap         : TGIS_PrintLayoutSnap ;
                                     const _print_area   : TRect  ;
                                     const _page_width   : Integer ;
                                     const _page_height  : Integer ;
                                     const _dpi          : Integer
                                   ) ; overload ;

      /// <summary>
      ///   Update pixel rectangle after printer change.
      /// </summary>
      /// <param name="_print_area">
      ///   print area
      /// </param>
      /// <param name="_page_width">
      ///   page width
      /// </param>
      /// <param name="_page_height">
      ///   page height
      /// </param>
      /// <param name="_dpi">
      ///   dpi value
      /// </param>
      procedure UpdateRectangleEx  ( const _print_area  : TRect ;
                                     const _page_width  : Integer ;
                                     const _page_height : Integer ;
                                     const _dpi         : Integer
                                   ) ;
                                   {$IFNDEF GENDOC} deprecated ; {$ENDIF}

  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout base element type.
  /// </summary>
  TGIS_PrintLayoutElement = {$IFDEF OXYGENE} public {$ENDIF} class
    protected
      /// <summary>
      ///   element type
      /// </summary>
      FElementType : TGIS_PrintLayoutElementType ;
      /// <summary>
      ///   element location
      /// </summary>
      FLocation    : TGIS_PrintLayoutLocation ;
      /// <summary>
      ///   element name
      /// </summary>
      FName        : String ;
      /// <summary>
      ///   Element index
      /// </summary>
      FIndex       : Integer ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_elementType">
      ///   element type
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location     : TGIS_PrintLayoutLocation ;
                          const _elementType  : TGIS_PrintLayoutElementType ;
                          const _index        : Integer
                         ) ;
      /// <summary>
      ///   Create element with default parameters.
      /// </summary>
      /// <returns>
      ///   default element
      /// </returns>
      class function CreateDefault : TGIS_PrintLayoutElement ; virtual ; abstract ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <summary>
      ///   Write element as xml.
      /// </summary>
      /// <param name="_node">
      ///   parent node
      /// </param>
      procedure Write( const _node : IXMLNode ) ; virtual ;

      /// <summary>
      ///   Write index to xml node .
      /// </summary>
      /// <param name="_node">
      ///   parent node
      /// </param>
      /// <param name="_index">
      ///   index to write
      /// </param>
      procedure WriteIndex( const _node  : IXMLNode ;
                            const _index : Integer
                          ) ; virtual ;
    public
      /// <inheritdoc/>
      function ToString : String ; override ;
    public
      /// <summary>
      ///   element type
      /// </summary>
      property ElementType : TGIS_PrintLayoutElementType  read  FElementType ;
      /// <summary>
      ///   element location
      /// </summary>
      property Location    : TGIS_PrintLayoutLocation     read  FLocation
                                                          write FLocation ;
      /// <summary>
      ///   element name
      /// </summary>
      property Name        : String                       read  FName
                                                          write FName ;
      /// <summary>
      ///   Element index
      /// </summary>
      property &Index      : Integer                      read  FIndex
                                                          write FIndex ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout box element type.
  /// </summary>
  TGIS_PrintLayoutBox = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_PrintLayoutElement )
    private
      FColor      : TGIS_Color ;
      FFrameColor : TGIS_Color ;
      FFrameWidth : TGIS_PrintLayoutWidth ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_color">
      ///   element color
      /// </param>
      /// <param name="_frame">
      ///   frame color
      /// </param>
      /// <param name="_width">
      ///   frame width
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _color    : TGIS_Color ;
                          const _frame    : TGIS_Color ;
                          const _width    : TGIS_PrintLayoutWidth ;
                          const _index    : Integer
                         ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   Fill color.
      /// </summary>
      property Color : TGIS_Color read  FColor
                                  write FColor ;
      /// <summary>
      ///   Frame color.
      /// </summary>
      property FrameColor : TGIS_Color
                                  read  FFrameColor
                                  write FFrameColor ;
      /// <summary>
      ///   Frame width.
      /// </summary>
      property FrameWidth : TGIS_PrintLayoutWidth
                                  read  FFrameWidth
                                  write FFrameWidth ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout map element type.
  /// </summary>
  TGIS_PrintLayoutMap = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_PrintLayoutElement )
    private
      {$IFDEF DCC} [weak] {$ENDIF}
      FViewer     : IGIS_Viewer ;
      FExtent     : TGIS_Extent ;
      FBackground : Boolean ;
      FScale      : Double ;
      FRealExtent : TGIS_Extent ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_viewer">
      ///   control handle
      /// </param>
      /// <param name="_extent">
      ///   viewer extent
      /// </param>
      /// <param name="_background">
      ///   background flag
      /// </param>
      /// <param name="_scale">
      ///   viewer scale
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location   : TGIS_PrintLayoutLocation ;
                          const _viewer     : IGIS_Viewer ;
                          const _extent     : TGIS_Extent ;
                          const _background : Boolean ;
                          const _scale      : Double ;
                          const _index      : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   control handle.
      /// </summary>
      property Viewer     : IGIS_Viewer read  FViewer ;
      /// <summary>
      ///   viewer extent.
      /// </summary>
      property Extent     : TGIS_Extent read  FExtent
                                        write FExtent ;
      /// <summary>
      ///   background flag.
      /// </summary>
      property Background : Boolean     read  FBackground
                                        write FBackground ;
      /// <summary>
      ///   viewer scale.
      /// </summary>
      property Scale      : Double      read  FScale
                                        write FScale ;
      /// <summary>
      ///   real extent.
      /// </summary>
      property RealExtent : TGIS_Extent read  FRealExtent
                                        write FRealExtent ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout legend element type.
  /// </summary>
  TGIS_PrintLayoutLegend = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PrintLayoutElement )
    private
      FControl : IGIS_PrintableControl ;
      FScale   : Double ;
      FCType   : TGIS_PrintLayoutControlType ;
      FCompactView   : Boolean ;
      FDrawIconStyle : TGIS_LegendIconStyle ;
      FReverseOrder  : Boolean ;
      FFont          : String ;
      FFontSize      : Integer ;
      FFontColor     : TGIS_Color ;
      FCompactViewStr   : String ;
      FDrawIconStyleStr : String ;
      FReverseOrderStr  : String ;
      FFontStr          : String ;
      FFontSizeStr      : String ;
      FFontColorStr     : String ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_control">
      ///   control handle
      /// </param>
      /// <param name="_scale">
      ///   control scale
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _control  : IGIS_PrintableControl ;
                          const _scale    : Double ;
                          const _index    : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; overload ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   control handle.
      /// </summary>
      property Control : IGIS_PrintableControl        read  FControl ;
      /// <summary>
      ///   control scale.
      /// </summary>
      property Scale   : Double                       read  FScale
                                                      write FScale ;
      /// <summary>
      ///   control type.
      /// </summary>
      property CType   : TGIS_PrintLayoutControlType  read  FCType ;
      /// <summary>
      ///   compact view.
      /// </summary>
      property CompactView : Boolean                  read  FCompactView
                                                      write FCompactView ;
      /// <summary>
      ///   legend icon style
      /// </summary>
      property DrawIconStyle : TGIS_LegendIconStyle   read  FDrawIconStyle
                                                      write FDrawIconStyle ;
      /// <value></value>
      /// <summary>
      ///   reverse order.
      /// </summary>
      property ReverseOrder : Boolean                 read  FReverseOrder
                                                      write FReverseOrder ;
      /// <summary>
      ///   font name.
      /// </summary>
      property Font     : String                      read  FFont
                                                      write FFont ;
      /// <summary>
      ///   font size.
      /// </summary>
      property FontSize  : Integer                    read  FFontSize
                                                      write FFontSize ;
      /// <summary>
      ///   font color.
      /// </summary>
      property FontColor : TGIS_Color                 read  FFontColor
                                                      write FFontColor ;
      /// <summary>
      ///   compact view (string).
      /// </summary>
      property CompactViewStr : String                read  FCompactViewStr
                                                      write FCompactViewStr ;
      /// <summary>
      ///   legend icon style
      /// </summary>
      property DrawIconStyleStr : String              read  FDrawIconStyleStr
                                                      write FDrawIconStyleStr ;
      /// <value></value>
      /// <summary>
      ///   reverse order (string).
      /// </summary>
      property ReverseOrderStr : String               read  FReverseOrderStr
                                                      write FReverseOrderStr ;
      /// <summary>
      ///   font name (string).
      /// </summary>
      property FontStr   : String                     read  FFontStr
                                                      write FFontStr ;
      /// <summary>
      ///   font size (string).
      /// </summary>
      property FontSizeStr : String                   read  FFontSizeStr
                                                      write FFontSizeStr ;
      /// <summary>
      ///   font color (string).
      /// </summary>
      property FontColorStr : String                  read  FFontColorStr
                                                      write FFontColorStr ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout scale element type.
  /// </summary>
  TGIS_PrintLayoutScale = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_PrintLayoutElement )
    private
      FControl : IGIS_PrintableControl ;
      FScale   : Double ;
      FCType   : TGIS_PrintLayoutControlType ;
      FDividers : Integer ;
      FDividerColor1 : TGIS_Color ;
      FDividerColor2 : TGIS_Color ;
      FFont          : String     ;
      FFontSize      : Integer    ;
      FFontColor     : TGIS_Color ;
      FDividersStr      : String ;
      FDividerColor1Str : String ;
      FDividerColor2Str : String ;
      FFontStr          : String ;
      FFontSizeStr      : String ;
      FFontColorStr     : String ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_control">
      ///   control handle
      /// </param>
      /// <param name="_scale">
      ///   control scale
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _control  : IGIS_PrintableControl ;
                          const _scale    : Double ;
                          const _index    : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; overload ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   control handle.
      /// </summary>
      property Control : IGIS_PrintableControl        read  FControl ;
      /// <summary>
      ///   control scale.
      /// </summary>
      property Scale   : Double                       read  FScale
                                                      write FScale ;
      /// <summary>
      ///   control type.
      /// </summary>
      property CType   : TGIS_PrintLayoutControlType  read  FCType ;
      /// <summary>
      ///   dividers.
      /// </summary>
      property Dividers : Integer                     read  FDividers
                                                      write FDividers ;
      /// <summary>
      ///   divider color 1.
      /// </summary>
      property DividerColor1 : TGIS_Color             read  FDividerColor1
                                                      write FDividerColor1 ;
      /// <summary>
      ///   divider color 2.
      /// </summary>
      property DividerColor2 : TGIS_Color             read  FDividerColor2
                                                      write FDividerColor2 ;
      /// <summary>
      ///   font.
      /// </summary>
      property Font    : String                       read  FFont
                                                      write FFont ;
      /// <summary>
      ///   font size.
      /// </summary>
      property FontSize : Integer                     read  FFontSize
                                                      write FFontSize ;
      /// <summary>
      ///   font color.
      /// </summary>
      property FontColor : TGIS_Color                 read  FFontColor
                                                      write FFontColor ;
      /// <summary>
      ///   dividers (string).
      /// </summary>
      property DividersStr : String                   read  FDividersStr
                                                      write FDividersStr ;
      /// <summary>
      ///   divider color 1 (string).
      /// </summary>
      property DividerColor1Str : String              read  FDividerColor1Str
                                                      write FDividerColor1Str ;
      /// <summary>
      ///   divider color 2 (string).
      /// </summary>
      property DividerColor2Str : String              read  FDividerColor2Str
                                                      write FDividerColor2Str ;
      /// <summary>
      ///   font (string).
      /// </summary>
      property FontStr : String                       read  FFontStr
                                                      write FFontStr ;
      /// <summary>
      ///   font size (string).
      /// </summary>
      property FontSizeStr : String                   read  FFontSizeStr
                                                      write FFontSizeStr ;
      /// <summary>
      ///   font color (string).
      /// </summary>
      property FontColorStr : String                  read  FFontColorStr
                                                      write FFontColorStr ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout northarrow element type.
  /// </summary>
  TGIS_PrintLayoutNorthArrow = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_PrintLayoutElement )
    private
      FControl : IGIS_PrintableControl ;
      FScale   : Double ;
      FCType   : TGIS_PrintLayoutControlType ;
      FColor1  : TGIS_Color ;
      FColor2  : TGIS_Color ;
      FStyle   : TGIS_ControlNorthArrowStyle ;
      FPath    : String ;
      FStyleStr  : String ;
      FColor1Str : String ;
      FColor2Str : String ;
      FPathStr   : String ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_control">
      ///   control handle
      /// </param>
      /// <param name="_scale">
      ///   control scale
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _control  : IGIS_PrintableControl ;
                          const _scale    : Double ;
                          const _index    : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; overload ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   control handle.
      /// </summary>
      property Control : IGIS_PrintableControl        read  FControl ;
      /// <summary>
      ///   control scale.
      /// </summary>
      property Scale   : Double                       read  FScale
                                                      write FScale ;
      /// <summary>
      ///   control type.
      /// </summary>
      property CType   : TGIS_PrintLayoutControlType  read  FCType ;
      /// <summary>
      ///   fill color.
      /// </summary>
      property Color1  : TGIS_Color                   read  FColor1
                                                      write FColor1 ;
      /// <summary>
      ///   outline color.
      /// </summary>
      property Color2  : TGIS_Color                   read  FColor2
                                                      write FColor2 ;
      /// <summary>
      ///   symbol style, used if Path is empty.
      /// </summary>
      property Style   : TGIS_ControlNorthArrowStyle  read  FStyle
                                                      write FStyle ;
      /// <summary>
      ///   Symbol path. If empty then Style property is used.
      /// </summary>
      property Path    : String                       read  FPath
                                                      write FPath ;
      /// <summary>
      ///   fill color (string).
      /// </summary>
      property Color1Str : String                     read  FColor1Str
                                                      write FColor1Str ;
      /// <summary>
      ///   outline color (string).
      /// </summary>
      property Color2Str : String                     read  FColor2Str
                                                      write FColor2Str ;
      /// <summary>
      ///   symbol style, used if Path is empty (string).
      /// </summary>
      property StyleStr  : String                     read  FStyleStr
                                                      write FStyleStr ;
      /// <summary>
      ///   Symbol path. If empty then Style property is used (string).
      /// </summary>
      property PathStr   : String                     read  FPathStr
                                                      write FPathStr ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout graphic element type.
  /// </summary>
  TGIS_PrintLayoutGraphic = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_PrintLayoutElement )
    private
      FGraphic : TGIS_TemplateGraphic ;
      FPath    : String  ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_graphic">
      ///   graphic object
      /// </param>
      /// <param name="_path">
      ///   symbol path; relative to template
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _graphic  : TGIS_TemplateGraphic ;
                          const _path     : String ;
                          const _index    : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write     ( const _node  : IXMLNode ) ; override ;
      /// <inheritdoc/>
      procedure WriteIndex( const _node  : IXMLNode ;
                            const _index : Integer
                          ) ; override ;
    public
      /// <summary>
      ///   Graphic object.
      /// </summary>
      property Graphic  : TGIS_TemplateGraphic
                                  read  FGraphic
                                  write FGraphic ;
      /// <summary>
      ///   full path.
      /// </summary>
      property Path     : String read  FPath
                                 write FPath ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout text element type.
  /// </summary>
  TGIS_PrintLayoutText = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_PrintLayoutElement )
    private
      FText     : String      ;
      FFont     : String      ;
      FStyle    : TGIS_FontStyles ;
      FSize     : Integer     ;
      FColor    : TGIS_Color  ;
      FAlign    : TGIS_LabelAlignment ;
      FBgColor  : TGIS_Color ;
      FBgWidth  : TGIS_PrintLayoutWidth ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element type
      /// </param>
      /// <param name="_text">
      ///   element location
      /// </param>
      /// <param name="_font">
      ///   text font name
      /// </param>
      /// <param name="_style">
      ///   text style
      /// </param>
      /// <param name="_size">
      ///   text size
      /// </param>
      /// <param name="_color">
      ///   text color
      /// </param>
      /// <param name="_align">
      ///   text align
      /// </param>
      /// <param name="_bgColor">
      ///   background color, use None for transparent
      /// </param>
      /// <param name="_bgWidth">
      ///   background border width
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _text     : String      ;
                          const _font     : String      ;
                          const _style    : TGIS_FontStyles ;
                          const _size     : Integer     ;
                          const _color    : TGIS_Color  ;
                          const _align    : TGIS_LabelAlignment ;
                          const _bgColor  : TGIS_Color  ;
                          const _bgWidth  : TGIS_PrintLayoutWidth ;
                          const _index    : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
      /// <inheritdoc/>
      procedure WriteIndex( const _node  : IXMLNode ;
                            const _index : Integer
                          ) ; override ;
    public
      /// <summary>
      ///   text string.
      /// </summary>
      property Text     : String              read  FText
                                              write FText ;
      /// <summary>
      ///   text font name.
      /// </summary>
      property Font     : String              read  FFont
                                              write FFont ;
      /// <summary>
      ///   text style.
      /// </summary>
      property Style    : TGIS_FontStyles     read  FStyle
                                              write FStyle ;
      /// <summary>
      ///   text size.
      /// </summary>
      property Size     : Integer             read  FSize
                                              write FSize ;
      /// <summary>
      ///   text color.
      /// </summary>
      property Color    : TGIS_Color          read  FColor
                                              write FColor ;
      /// <summary>
      ///   text align.
      /// </summary>
      property Align    : TGIS_LabelAlignment read  FAlign
                                              write FAlign ;
      /// <summary>
      ///   text background color.
      /// </summary>
      property BackgroundColor  : TGIS_Color  read  FBgColor
                                              write FBgColor ;

      /// <summary>
      ///   text border width.
      /// </summary>
      property BackgroundWidth  : TGIS_PrintLayoutWidth
                                              read  FBgWidth
                                              write FBgWidth ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout frame element type.
  /// </summary>
  TGIS_PrintLayoutFrame = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_PrintLayoutElement )
    private
      FColor    : TGIS_Color  ;
      FWidth    : TGIS_PrintLayoutWidth ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_color">
      ///   element color
      /// </param>
      /// <param name="_width">
      ///   element width
      /// </param>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _color    : TGIS_Color  ;
                          const _width    : TGIS_PrintLayoutWidth ;
                          const _index    : Integer
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   frame color.
      /// </summary>
      property Color    : TGIS_Color            read  FColor
                                                write FColor ;
      /// <summary>
      ///   frame width.
      /// </summary>
      property Width    : TGIS_PrintLayoutWidth read  FWidth
                                                write FWidth ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Print layout page element type.
  /// </summary>
  TGIS_PrintLayoutPage = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_PrintLayoutElement )
    private
      FWidth    : TGIS_PrintLayoutWidth ;
      FHeight   : TGIS_PrintLayoutWidth ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_location">
      ///   element location
      /// </param>
      /// <param name="_width">
      ///   element width
      /// </param>
      /// <param name="_height">
      ///   element height
      /// </param>
      constructor Create( const _location : TGIS_PrintLayoutLocation ;
                          const _width    : TGIS_PrintLayoutWidth  ;
                          const _height   : TGIS_PrintLayoutWidth
                        ) ;
      /// <inheritdoc/>
      class function CreateDefault : TGIS_PrintLayoutElement ; override ;
    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <inheritdoc/>
      procedure Write( const _node : IXMLNode ) ; override ;
    public
      /// <summary>
      ///   Page width.
      /// </summary>
      property Width    : TGIS_PrintLayoutWidth read  FWidth
                                                write FWidth ;
      /// <summary>
      ///   Page height.
      /// </summary>
      property Height   : TGIS_PrintLayoutWidth read  FHeight
                                                write FHeight ;
  end ;

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Template event called for each found element.
  /// </summary>
  /// <param name="_sender">
  ///   event sender
  /// </param>
  /// <param name="_element">
  ///   element handle
  /// </param>
  {$IFDEF GENXDK}
    TGIS_TemplateElementEvent    = procedure( var _translated : Boolean ;
                                                  _sender     : TObject ;
                                                  _element    : TGIS_PrintLayoutElement
                                            ) of object ;
  {$ELSE}
    TGIS_TemplateElementEvent    = {$IFDEF OXYGENE} public {$ENDIF}
                                    procedure(     _sender   : TObject ;
                                                   _element  : TGIS_PrintLayoutElement
                                             ) of object ;
  {$ENDIF}

  {$ENDREGION}

  {$REGION 'TGIS_TemplatePrintBuilder'}
  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Encapsulation of print template builder.
  /// </summary>
  TGIS_TemplatePrintBuilder = {$IFDEF OXYGENE} public {$ENDIF} class
                               ( TGIS_Object )
    private
      // Print template object.
      FTemplate  : TGIS_TemplatePrint ;
      FElements  : TObjectList<TGIS_PrintLayoutElement> ;
      FPageElement : TGIS_PrintLayoutElement ;
      FRecreating  : Boolean ;

      // events
      FOnElement     : TGIS_TemplateElementEvent ;
      FOnInitElement : TGIS_TemplateElementEvent ;
    private
      print_area  : TRect ;
      page_width  : Integer ;
      page_height : Integer ;
      dpi         : Integer ;
    protected
      /// <summary>
      ///   Perform component cleanup
      /// </summary>
      procedure   doDestroy      ; override ;

    protected
      function fget_ElementsCount : Integer ;
      function fget_Element( _index : Integer ) : TGIS_PrintLayoutElement;

    protected
      /// <summary>
      ///   Create an instance. Only for internal use of TatukGIS.
      /// </summary>
      /// <param name="_template">
      ///   template print object
      /// </param>
      /// <param name="_recreate">
      ///   object is opened in recreate mode
      /// </param>
      constructor Create      ( const _template : TGIS_TemplatePrint ;
                                const _recreate : Boolean
                              ) ; overload ;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_template">
      ///   template print object
      /// </param>
      constructor Create      ( const _template : TGIS_TemplatePrint
                              ) ; overload ;

      /// <summary>
      ///   Process a page according to defined template.
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
      procedure ProcessTemplate ( const _dpi          : Integer     ;
                                  const _print_area   : TRect       ;
                                  const _page_width   : Integer     ;
                                  const _page_height  : Integer
                                ) ;
      /// <summary>
      ///   Update template processing parameters. Call after printer change.
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
      procedure UpdateTemplate  ( const _dpi          : Integer     ;
                                  const _print_area   : TRect       ;
                                  const _page_width   : Integer     ;
                                  const _page_height  : Integer
                                ) ;
      /// <summary>
      ///   Create new element.
      /// </summary>
      /// <param name="_type">
      ///   element type
      /// </param>
      /// <returns>
      ///   element object
      /// </returns>
      function  NewElement      ( const _type   : TGIS_PrintLayoutElementType
                                ) : TGIS_PrintLayoutElement ;
      /// <summary>
      ///   Add element to list.
      /// </summary>
      /// <param name="_elm">
      ///   element
      /// </param>
      procedure AddElement      ( const _elm    : TGIS_PrintLayoutElement
                                ) ;
      /// <summary>
      ///   Update element location.
      /// </summary>
      /// <param name="_elm">
      ///   element
      /// </param>
      /// <param name="_bounds">
      ///   new pixel bounds
      /// </param>
      procedure UpdateLocation  ( const _elm    : TGIS_PrintLayoutElement ;
                                  const _bounds : TRect
                                ) ;
                                {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Update element location.
      /// </summary>
      /// <param name="_elm">
      ///   element
      /// </param>
      /// <param name="_bounds">
      ///   new element bounds
      /// </param>
      /// <param name="_ratio">
      ///   element size ratio
      /// </param>
      /// <param name="_print_area">
      ///   print area pixel bounds
      /// </param>
      /// <param name="_page_width">
      ///   page width
      /// </param>
      /// <param name="_page_height">
      ///   page height
      /// </param>
      /// <param name="_dpi">
      ///   printer ppi
      /// </param>
      procedure UpdateLocationEx( const _elm         : TGIS_PrintLayoutElement ;
                                  const _bounds      : TRect ;
                                  const _ratio       : Double ;
                                  const _print_area  : TRect ;
                                  const _page_width  : Integer ;
                                  const _page_height : Integer ;
                                  const _dpi         : Integer
                                ) ;
                                {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Update element pixel rectangle in location.
      /// </summary>
      /// <param name="_elm">
      ///   element
      /// </param>
      procedure UpdateRectangle ( const _elm    : TGIS_PrintLayoutElement
                                ) ;
                                {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Update element pixel rectangle in location.
      /// </summary>
      /// <param name="_elm">
      ///   element
      /// </param>
      /// <param name="_print_area">
      ///   print area pixel bounds
      /// </param>
      /// <param name="_page_width">
      ///   page width
      /// </param>
      /// <param name="_page_height">
      ///   page height
      /// </param>
      /// <param name="_dpi">
      ///   printer ppi
      /// </param>
      procedure UpdateRectangleEx( const _elm   : TGIS_PrintLayoutElement ;
                                   const _print_area  : TRect ;
                                   const _page_width  : Integer ;
                                   const _page_height : Integer ;
                                   const _dpi         : Integer
                                 ) ;
                                {$IFNDEF GENDOC} deprecated ; {$ENDIF}

      /// <summary>
      ///   Remove element from list.
      /// </summary>
      /// <param name="_index">
      ///   element index
      /// </param>
      procedure RemoveElement   ( const _index  : Integer
                                ) ;
      /// <summary>
      ///   Move element on list.
      /// </summary>
      /// <param name="_index">
      ///   element index
      /// </param>
      /// <param name="_step">
      ///   offset to move relative to current position
      /// </param>
      procedure MoveElement     ( const _index  : Integer;
                                  const _step   : Integer
                                ) ;
      /// <summary>
      ///   Clear all element from list.
      /// </summary>
      procedure ClearElements ;

      {$IFNDEF GIS_XDK}
        /// <summary>
        ///   Get enumerator of elements list.
        /// </summary>
        /// <returns>
        ///   enumerator
        /// </returns>
        {#gendoc:hide:GENPDK}
        function  GetEnumerator : {$IFDEF CLR}
                                     IEnumerator
                                  {$ELSE}
                                    TEnumerator<TGIS_PrintLayoutElement>
                                  {$ENDIF} ;
      {$ENDIF}
      /// <summary>
      ///   Save template to file.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      procedure SaveToFile      ( const _path : String
                                ) ;
      /// <summary>
      ///   Copy a template file. Guarantee proper paths to files.
      /// </summary>
      /// <param name="_oldPath">
      ///   source path to file
      /// </param>
      /// <param name="_newPath">
      ///   destination path to file
      /// </param>
      /// <param name="_copyFiles">
      ///   if True, files are copied according to relative paths
      ///   if False, files are not copied; relative paths are properly changed
      /// </param>
      class procedure CopyTemplateFile
                                ( const _oldPath   : String ;
                                  const _newPath   : String ;
                                  const _copyFiles : Boolean
                                ) ;
    public
      /// <summary>
      ///   Print template object.
      /// </summary>
      property Template     : TGIS_TemplatePrint        read  FTemplate ;

      /// <event/>
      /// <summary>
      ///   Event called for each found element in template for initialization.
      /// </summary>
      property InitElementEvent : TGIS_TemplateElementEvent
                                                        read  FOnInitElement
                                                        write FOnInitElement ;

      /// <event/>
      /// <summary>
      ///   Event called for each found element in template.
      ///   If not assigned, all elements will be stored in the list.
      /// </summary>
      property ElementEvent : TGIS_TemplateElementEvent read  FOnElement
                                                        write FOnElement ;

      /// <summary>
      ///   Number of elements in template.
      /// </summary>
      property ElementsCount : Integer                  read  fget_ElementsCount ;

      /// <summary>
      ///   Element object accessed by index from internal list.
      /// </summary>
      /// <param name="_index">
      ///   element index
      /// </param>
      property Elements[_index : Integer] : TGIS_PrintLayoutElement
                                                        read  fget_Element; default;
  end ;
  {$ENDREGION}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
  System.SysUtils,
  System.Math,
  System.Variants,
  {$IFDEF LEVEL_XE2_RTL}
    System.IOUtils,
  {$ENDIF}

  GisClasses,
  GisConfigIni,
  GisConfigXml,
  GisInternals,
  GisResource,
  GisParams,
  GisConfig,
  GisFunctions,
  GisPrintUtils ;
{$ENDIF}

  {$REGION 'Consts'}
  const
    TPL_MAX_ELEMENT_COUNT = 999 ;
  {$ENDREGION}

  {$REGION 'TGIS_PrintLayoutElement'}

  class function TGIS_PrintLayoutSnap.CreateDefault : TGIS_PrintLayoutSnap ;
  begin
    {$IFDEF OXYGENE}
      result := TGIS_PrintLayoutSnap.create ;
    {$ENDIF}
    Result.Value := 0 ;
    Result.Units := TGIS_PrintLayoutUnits.uCm ;
  end ;

  function TGIS_PrintLayoutSnap.AsText
    : String ;
  begin
    Result := Value.ToString + TGIS_PrintUtils.UnitToText( Units ) ;
  end ;

  procedure TGIS_PrintLayoutSnap.SetValues(
    const _values : String
  ) ;
  var
    v : Double ;
    u : String ;

    procedure convert_size(
      const _txt   : String ;
        var _value : Double ;
        var _units : String
    ) ;  overload ;
    var
      j         : Integer ;
      c         : Char    ;
      txt       : String  ;
      state     : Integer ;
      sval      : String  ;
      dval      : Double  ;
      measure   : String  ;
    begin
      state   := 0  ;
      sval    := '' ;
      measure := '' ;
      txt     := _txt ;

      for j := StringFirst to length( txt )+StringFirst-1 do begin
        c := txt[ j ] ;

        case state of
          0 : begin
                state  := 1 ;
                sval   := c ;
              end ;
          1 : begin
                if c = ',' then
                  c := '.' ;
                if CharInSet( c, ['0','1','2','3','4','5','6','7','8','9', '.', ' ' ] ) then begin
                  sval := sval + c ;
                end
                else begin
                  state   := 2 ;
                  measure := c ;
                end ;
              end;
          2 : measure := measure + c ;
        end ;
      end ;

      sval := Trim( sval ) ;
      try
        if not IsStringEmpty( sval ) then
          dval := DotStrToFloat( sval )
        else
          dval := 0 ;
      except
        dval := 0 ;
      end ;

      _value := dval ;
      _units := measure ;
    end ;
  begin
    convert_size( _values, v, u ) ;
    Value := v ;
    if v = 0 then
      Units := TGIS_PrintLayoutUnits.uCm
    else
      Units := TGIS_PrintUtils.TextToUnit( u ) ;
  end ;

  function TGIS_PrintLayoutSnap.Round(
    const _v           : Single ;
    const _u           : TGIS_PrintLayoutUnits ;
    const _a           : TGIS_PrintLayoutAnchor ;
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) : Double ;
  var
    v : Double ;
    b : Double ;
    s : Integer ;
    n : Integer ;
    toProcess : Boolean ;
  begin
    Result := _v ;

    v := _v ;
    b := 0 ;
    case _a of
      TGIS_PrintLayoutAnchor.aRightMargin:
        begin
          b := TGIS_PrintUtils.FromPixels( _print_area.Right - _print_area.Left, _u, _dpi ) ;
          v := b - _v ;
        end ;
      TGIS_PrintLayoutAnchor.aBottomMargin:
        begin
          b := TGIS_PrintUtils.FromPixels( _print_area.Bottom - _print_area.Top, _u, _dpi ) ;
          v := b - _v ;
        end ;
      TGIS_PrintLayoutAnchor.aLeftBorder:
        begin
          b := TGIS_PrintUtils.FromPixels( _print_area.Left, _u, _dpi ) ;
          v := _v - b ;
        end ;
      TGIS_PrintLayoutAnchor.aTopBorder:
        begin
          b := TGIS_PrintUtils.FromPixels( _page_height - _print_area.Top, _u, _dpi ) ;
          v := b - _v ;
        end ;
      TGIS_PrintLayoutAnchor.aRightBorder:
        begin
          b := TGIS_PrintUtils.FromPixels( _page_width - _print_area.Left, _u, _dpi ) ;
          v := b - _v ;
        end ;
      TGIS_PrintLayoutAnchor.aBottomBorder:
        begin
          b := TGIS_PrintUtils.FromPixels( _page_height - _print_area.Top, _u, _dpi ) ;
          v := b - _v ;
        end ;
    end;

    toProcess := False ;
    case _u of
      TGIS_PrintLayoutUnits.uCm :
        begin
          case Units of
            TGIS_PrintLayoutUnits.uCm : begin
                                          n := RoundS( v / Value ) ;
                                          Result := n * Value ;
                                        end ;
            TGIS_PrintLayoutUnits.uMm : begin
                                          n := RoundS( v * 10 / Value ) ;
                                          Result := n * Value / 10 ;
                                        end ;
            else                        toProcess := True ;
          end ;
        end ;
      TGIS_PrintLayoutUnits.uMm :
        begin
          case Units of
            TGIS_PrintLayoutUnits.uCm : begin
                                          n := RoundS( v / ( Value * 10 ) ) ;
                                          Result := n * ( Value * 10 ) ;
                                        end ;
            TGIS_PrintLayoutUnits.uMm : begin
                                          n := RoundS( v / Value ) ;
                                          Result := n * Value ;
                                        end ;
            else                        toProcess := True ;
          end ;
        end ;
      TGIS_PrintLayoutUnits.uIn :
        begin
          case Units of
            TGIS_PrintLayoutUnits.uIn : begin
                                          n := RoundS( v / Value ) ;
                                          Result := n * Value ;
                                        end ;
            TGIS_PrintLayoutUnits.uPt : begin
                                          n := RoundS( v * 72 / Value ) ;
                                          Result := n * Value / 72 ;
                                        end ;
            else                        toProcess := True ;
          end ;
        end ;
      TGIS_PrintLayoutUnits.uPt :
        begin
          case Units of
            TGIS_PrintLayoutUnits.uIn : begin
                                          n := RoundS( v / ( Value * 72 ) ) ;
                                          Result := n * Value * 72 ;
                                        end ;
            TGIS_PrintLayoutUnits.uPt : begin
                                          n := RoundS( v / Value ) ;
                                          Result := n * Value ;
                                        end ;
            else                        toProcess := True ;
          end ;
        end ;
      TGIS_PrintLayoutUnits.uPx :
        begin
          toProcess := True ;
        end ;
    end ;

    if toProcess then begin
      s := TGIS_PrintUtils.ToPixels( Value, Units, _dpi ) ;
      n := RoundS( TGIS_PrintUtils.ToPixels( v, _u, _dpi ) / s ) ;
      Result := TGIS_PrintUtils.FromPixels( n * s, _u, _dpi ) ;
    end ;

    case _a of
      TGIS_PrintLayoutAnchor.aRightMargin:  Result := b - Result ;
      TGIS_PrintLayoutAnchor.aBottomMargin: Result := b - Result ;
      TGIS_PrintLayoutAnchor.aLeftBorder:   Result := Result + b ;
      TGIS_PrintLayoutAnchor.aTopBorder:    Result := Result + b ;
      TGIS_PrintLayoutAnchor.aRightBorder:  Result := b - Result ;
      TGIS_PrintLayoutAnchor.aBottomBorder: Result := b - Result ;
    end;
  end ;

  function TGIS_PrintLayoutPosition.AsTplText: String;
  var
    n : String ;
    m : String ;
    s : Integer ;
    u : String ;
  begin
    s := 1 ;
    if Value >= 0 then n := ''
                  else n := '!' ;

    case Anchor of
      TGIS_PrintLayoutAnchor.aLeftMargin  : m := '*' ;
      TGIS_PrintLayoutAnchor.aLeftBorder  : m := '' ;
      TGIS_PrintLayoutAnchor.aTopMargin   : m := '*' ;
      TGIS_PrintLayoutAnchor.aTopBorder   : m := '' ;
      TGIS_PrintLayoutAnchor.aRightMargin : m := '*' ;
      TGIS_PrintLayoutAnchor.aRightBorder : m := '' ;
      TGIS_PrintLayoutAnchor.aBottomMargin: m := '*' ;
      TGIS_PrintLayoutAnchor.aBottomBorder: m := ''
    end ;

    case Anchor of
      TGIS_PrintLayoutAnchor.aLeftMargin  : s :=  1 ;
      TGIS_PrintLayoutAnchor.aLeftBorder  : s :=  1 ;
      TGIS_PrintLayoutAnchor.aTopMargin   : s :=  1 ;
      TGIS_PrintLayoutAnchor.aTopBorder   : s :=  1 ;
      TGIS_PrintLayoutAnchor.aRightMargin : s := -1 ;
      TGIS_PrintLayoutAnchor.aRightBorder : s := -1 ;
      TGIS_PrintLayoutAnchor.aBottomMargin: s := -1 ;
      TGIS_PrintLayoutAnchor.aBottomBorder: s := -1
    end ;

    u := TGIS_PrintUtils.UnitToText( Units ) ;

    Result := Format('%s%s%s%s', [ n, m, DotFloatToStr(s*Abs(Value)), u ] ) ;
  end ;

  function TGIS_PrintLayoutPosition.AsText: String;
  var
    u : String ;
  begin
    u := TGIS_PrintUtils.UnitToText( Units ) ;
    Result := Format('%s %s%s', [ IntToStr( Integer(Anchor) ), DotFloatToStr(Value), u ] ) ;
  end ;

  procedure TGIS_PrintLayoutPosition.SetValues(
    const _value  : Double ;
    const _units  : String ;
    const _anchor : TGIS_PrintLayoutAnchor
  ) ;
  begin
    Value  := _value ;
    Units  := TGIS_PrintUtils.TextToUnit( _units ) ;
    Anchor := _anchor ;
  end ;

  constructor TGIS_PrintLayoutPosition.Create(
    const _value  : Double ;
    const _units  : TGIS_PrintLayoutUnits ;
    const _anchor : TGIS_PrintLayoutAnchor
  ) ;
  begin
    {$IFDEF OXYGENE}
      inherited Create ;
    {$ENDIF}
    Value   := _value ;
    Units   := _units ;
    Anchor  := _anchor ;
  end ;

  function TGIS_PrintLayoutWidth.AsText : String ;
  var
    u : String ;
  begin
    u := TGIS_PrintUtils.UnitToText( Units ) ;
    Result := Format('%s%s', [ DotFloatToStr(Value), u ] ) ;
  end;

  procedure TGIS_PrintLayoutWidth.SetValues(
    const _value : Double ;
    const _units : String
  ) ;
  begin
    Value := Abs(_value) ;
    Units := TGIS_PrintUtils.TextToUnit( _units ) ;
  end ;

  constructor TGIS_PrintLayoutWidth.Create(
    const _value : Double ;
    const _units : TGIS_PrintLayoutUnits
  ) ;
  begin
    {$IFDEF OXYGENE}
      inherited Create ;
    {$ENDIF}
    Value := _value ;
    Units := _units ;
    Width := 0 ;
  end ;

  class function TGIS_PrintLayoutLocation.CreateDefault : TGIS_PrintLayoutLocation;
  begin
    {$IFDEF OXYGENE}
      result := TGIS_PrintLayoutLocation.create ;
    {$ENDIF}
    Result.Left   := TGIS_PrintLayoutPosition.Create(
                        1, TGIS_PrintLayoutUnits.uCm, TGIS_PrintLayoutAnchor.aLeftMargin
                      ) ;
    Result.Right  := TGIS_PrintLayoutPosition.Create(
                        2, TGIS_PrintLayoutUnits.uCm, TGIS_PrintLayoutAnchor.aLeftMargin
                      ) ;
    Result.Top    := TGIS_PrintLayoutPosition.Create(
                        1, TGIS_PrintLayoutUnits.uCm, TGIS_PrintLayoutAnchor.aTopMargin
                      ) ;
    Result.Bottom := TGIS_PrintLayoutPosition.Create(
                        2, TGIS_PrintLayoutUnits.uCm, TGIS_PrintLayoutAnchor.aTopMargin
                      ) ;
    Result.Rectangle := Rect( 1, 1, 2, 2 ) ;
  end ;

  procedure TGIS_PrintLayoutLocation.UpdateToRectangle(
    const _bounds       : TRect ;
    const _print_area   : TRect ;
    const _page_width   : Integer ;
    const _page_height  : Integer ;
    const _dpi          : Integer
  ) ;
  begin
    UpdateRectangle( _bounds, True, True, True, True,
                     TGIS_PrintLayoutSnap.CreateDefault,
                     _print_area, _page_width, _page_height, _dpi ) ;
  end ;

  procedure TGIS_PrintLayoutLocation.UpdateToRectangleEx(
    const _bounds      : TRect ;
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) ;
  begin
    UpdateToRectangle( _bounds, _print_area, _page_width, _page_height, _dpi ) ;
  end ;

  procedure TGIS_PrintLayoutLocation.UpdateRectangle(
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) ;
  var
    x1, y1, x2, y2 : Integer ;

    function convert_to_pixels(
      const _v : Double ;
      const _a : TGIS_PrintLayoutAnchor ;
      const _u : TGIS_PrintLayoutUnits
    ) : Integer ;
    begin
      Result := TGIS_PrintUtils.ToPixels( _v, _u, _dpi ) ;
      case _a of
        TGIS_PrintLayoutAnchor.aLeftMargin :   ;
        TGIS_PrintLayoutAnchor.aTopMargin :    ;
        TGIS_PrintLayoutAnchor.aLeftBorder :   Result := Result - _print_area.Left ;
        TGIS_PrintLayoutAnchor.aTopBorder :    Result := Result - _print_area.Top ;
        TGIS_PrintLayoutAnchor.aRightMargin :  Result := _print_area.Right - _print_area.Left - Result ;
        TGIS_PrintLayoutAnchor.aBottomMargin : Result := _print_area.Bottom - _print_area.Top - Result ;
        TGIS_PrintLayoutAnchor.aRightBorder :  Result := _page_width - _print_area.Left - Result ;
        TGIS_PrintLayoutAnchor.aBottomBorder : Result := _page_height - _print_area.Top - Result ;
      end;
    end ;

  begin
    x1 := convert_to_pixels( Left.Value,   Left.Anchor,   Left.Units   ) ;
    y1 := convert_to_pixels( Top.Value,    Top.Anchor,    Top.Units    ) ;
    x2 := convert_to_pixels( Right.Value,  Right.Anchor,  Right.Units  ) ;
    y2 := convert_to_pixels( Bottom.Value, Bottom.Anchor, Bottom.Units ) ;

    Rectangle := Rect( Min( x1, x2 ),
                       Min( y1, y2 ),
                       Max( x1, x2 ),
                       Max( y1, y2 )
                     ) ;
  end ;

  procedure TGIS_PrintLayoutLocation.UpdateRectangle(
    const _bounds      : TRect ;
    const _left        : Boolean ;
    const _top         : Boolean ;
    const _right       : Boolean ;
    const _bottom      : Boolean ;
    const _snap        : TGIS_PrintLayoutSnap ;
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) ;

    function convert_size(
      const _px : Integer ;
      const _u  : TGIS_PrintLayoutUnits ;
      const _a  : TGIS_PrintLayoutAnchor
    ) : Double ;
    var
      px : Integer ;
      v  : Double ;
    begin
      Result := 0 ;
      px := 0 ;
      case _a of
        TGIS_PrintLayoutAnchor.aLeftMargin   : px := _px ;
        TGIS_PrintLayoutAnchor.aRightMargin  : px := _print_area.Right - _print_area.Left - _px ;
        TGIS_PrintLayoutAnchor.aLeftBorder   : px := _px - _print_area.Left ;
        TGIS_PrintLayoutAnchor.aRightBorder  : px := _page_width - _print_area.Left - _px ;
        TGIS_PrintLayoutAnchor.aTopMargin    : px := _px ;
        TGIS_PrintLayoutAnchor.aBottomMargin : px := _print_area.Bottom - _print_area.Top - _px ;
        TGIS_PrintLayoutAnchor.aTopBorder    : px := _px - _print_area.Top ;
        TGIS_PrintLayoutAnchor.aBottomBorder : px := _page_height - _print_area.Top - _px ;
      end;

      // pixels to units
      v := TGIS_PrintUtils.FromPixels( px, _u, _dpi ) ;

      if _snap.Value > 0 then
        // take snap into account
        v := _snap.Round( v, _u, _a, _print_area, _page_width, _page_height, _dpi ) ;

      case _u of
        TGIS_PrintLayoutUnits.uCm : Result := RoundTo( v, -2 ) ;
        TGIS_PrintLayoutUnits.uMm : Result := RoundTo( v, -1 ) ;
        TGIS_PrintLayoutUnits.uIn : Result := RoundTo( v, -2 ) ;
        TGIS_PrintLayoutUnits.uPt : Result := RoundTo( v, -2 ) ;
        TGIS_PrintLayoutUnits.uPx : Result := RoundTo( v, 0  ) ;
      end ;
    end ;

  begin
    // convert printer pixels to units according to anchors
    if _left then
      Left.Value  := convert_size( _bounds.Left, Left.Units, Left.Anchor ) ;
    if _right then
      Right.Value := convert_size( _bounds.Right, Right.Units, Right.Anchor ) ;

    if _top then
      Top.Value    := convert_size( _bounds.Top, Top.Units, Top.Anchor ) ;
    if _bottom then
      Bottom.Value := convert_size( _bounds.Bottom, Bottom.Units, Bottom.Anchor ) ;

    if _snap.Value = 0 then
      Rectangle := _bounds
    else
      UpdateRectangle( _print_area, _page_width, _page_height, _dpi ) ;
  end ;

  procedure TGIS_PrintLayoutLocation.UpdateRectangle(
    const _delta_left   : Double ;
    const _delta_top    : Double ;
    const _delta_right  : Double ;
    const _delta_bottom : Double ;
    const _snap         : TGIS_PrintLayoutSnap ;
    const _print_area   : TRect  ;
    const _page_width   : Integer ;
    const _page_height  : Integer ;
    const _dpi          : Integer
  ) ;

    function change_coordinate(
      _position : TGIS_PrintLayoutPosition ;
      _change   : Double
    ) : Double ;
    var
      toProcess : Boolean ;
      px : Integer ;
      v  : Single ;

      function sum( _s1, _s2 : Single ) : Single ; overload ;
      begin
        Result := _s1 ;
        if ( _position.Anchor = TGIS_PrintLayoutAnchor.aLeftMargin ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aLeftBorder ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aTopMargin  ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aTopBorder  ) then
          Result := _s1 + _s2 ;
        if ( _position.Anchor = TGIS_PrintLayoutAnchor.aRightMargin  ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aRightBorder  ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aBottomMargin ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aBottomBorder ) then
          Result := _s1 - _s2 ;
      end ;

      function sum( _s1, _s2 : Integer ) : Integer ; overload ;
      begin
        Result := _s1 ;
        if ( _position.Anchor = TGIS_PrintLayoutAnchor.aLeftMargin ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aLeftBorder ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aTopMargin  ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aTopBorder  ) then
          Result := _s1 + _s2 ;
        if ( _position.Anchor = TGIS_PrintLayoutAnchor.aRightMargin  ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aRightBorder  ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aBottomMargin ) or
           ( _position.Anchor = TGIS_PrintLayoutAnchor.aBottomBorder ) then
          Result := _s1 - _s2 ;
      end ;

    begin
      Result := _position.Value ;
      if _change = 0 then exit ;

      v := _position.Value ;
      toProcess := False ;
      case _position.Units of
        TGIS_PrintLayoutUnits.uCm :
          begin
            case _snap.Units of
              TGIS_PrintLayoutUnits.uCm : v := sum( v, _change ) ;
              TGIS_PrintLayoutUnits.uMm : v := sum( v, _change / 10 ) ;
              else                        toProcess := True ;
            end ;
          end ;
        TGIS_PrintLayoutUnits.uMm :
          begin
            case _snap.Units of
              TGIS_PrintLayoutUnits.uCm : v := sum( v, _change * 10 ) ;
              TGIS_PrintLayoutUnits.uMm : v := sum( v, _change ) ;
              else                        toProcess := True ;
            end ;
          end ;
        TGIS_PrintLayoutUnits.uIn :
          begin
            case _snap.Units of
              TGIS_PrintLayoutUnits.uIn : v := sum( v, _change ) ;
              TGIS_PrintLayoutUnits.uPt : v := sum( v, _change / 72 ) ;
              else                        toProcess := True ;
            end ;
          end ;
        TGIS_PrintLayoutUnits.uPt :
          begin
            case _snap.Units of
              TGIS_PrintLayoutUnits.uIn : v := sum( v, _change * 72 ) ;
              TGIS_PrintLayoutUnits.uPt : v := sum( v, _change ) ;
              else                        toProcess := True ;
            end ;
          end ;
        TGIS_PrintLayoutUnits.uPx :
          begin
            toProcess := True ;
          end ;
      end ;

      if toProcess then begin
        px := sum( TGIS_PrintUtils.ToPixels( v, _position.Units, _dpi ),
                   TGIS_PrintUtils.ToPixels( _change, _snap.Units, _dpi )
                 ) ;
        v := TGIS_PrintUtils.FromPixels( px, _position.Units, _dpi ) ;
      end ;

      // take snap into account
      v := _snap.Round( v, _position.Units, _position.Anchor,
                        _print_area, _page_width, _page_height, _dpi ) ;

      case _position.Units of
        TGIS_PrintLayoutUnits.uCm : Result := RoundTo( v, -2 ) ;
        TGIS_PrintLayoutUnits.uMm : Result := RoundTo( v, -1 ) ;
        TGIS_PrintLayoutUnits.uIn : Result := RoundTo( v, -2 ) ;
        TGIS_PrintLayoutUnits.uPt : Result := RoundTo( v, -2 ) ;
        TGIS_PrintLayoutUnits.uPx : Result := RoundTo( v, 0  ) ;
      end ;
    end ;

  begin
    Left.Value   := change_coordinate( Left,   _delta_left   * _snap.Value ) ;
    Right.Value  := change_coordinate( Right,  _delta_right  * _snap.Value ) ;

    Top.Value    := change_coordinate( Top,    _delta_top    * _snap.Value ) ;
    Bottom.Value := change_coordinate( Bottom, _delta_bottom * _snap.Value ) ;

    UpdateRectangle( _print_area, _page_width, _page_height, _dpi ) ;
  end ;

  procedure TGIS_PrintLayoutLocation.UpdateRectangleEx(
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) ;
  begin
    UpdateRectangle( _print_area, _page_width, _page_height, _dpi ) ;
  end ;

  constructor TGIS_PrintLayoutElement.Create(
    const _location     : TGIS_PrintLayoutLocation ;
    const _elementType  : TGIS_PrintLayoutElementType ;
    const _index        : Integer
  );
  begin
    inherited Create ;

    FLocation    := _location ;
    FElementType := _elementType ;
    FName        := '' ;
    FIndex       := _index ;
  end ;

  procedure TGIS_PrintLayoutElement.Write(
    const _node : IXMLNode
  ) ;
  begin
    _node.Attributes['name'] := Name ;

    WriteIndex( _node, &Index ) ;

    _node.Attributes['left']    := Location.Left.AsTplText   ;
    _node.Attributes['top']     := Location.Top.AsTplText    ;
    _node.Attributes['right']   := Location.Right.AsTplText  ;
    _node.Attributes['bottom']  := Location.Bottom.AsTplText ;
  end ;

  procedure TGIS_PrintLayoutElement.WriteIndex(
    const _node  : IXMLNode ;
    const _index : Integer
  ) ;
  begin
    if _index > 0 then
      _node.Attributes['index'] := _index ;
  end ;

  function TGIS_PrintLayoutElement.ToString
    : String ;
  begin
    result := Name ;
  end ;

  constructor TGIS_PrintLayoutBox.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _color    : TGIS_Color ;
    const _frame    : TGIS_Color ;
    const _width    : TGIS_PrintLayoutWidth ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Box, _index ) ;

    FColor := _color ;
    FFrameColor := _frame ;
    FFrameWidth := _width ;
    FName  := 'box' ;
  end ;

  class function TGIS_PrintLayoutBox.CreateDefault : TGIS_PrintLayoutElement ;
  begin
    Result := TGIS_PrintLayoutBox.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                TGIS_Color.None,
                TGIS_Color.Black,
                TGIS_PrintLayoutWidth.Create(1, TGIS_PrintLayoutUnits.uPx),
                0
              ) ;
  end ;

  procedure TGIS_PrintLayoutBox.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_BOX ) ;

    inherited Write( nd ) ;

    nd.Attributes['color']      := ConstructParamColor( Color ) ;
    nd.Attributes['frameColor'] := ConstructParamColor( FrameColor ) ;
    nd.Attributes['frameWidth'] := FrameWidth.AsText ;
  end ;

  constructor TGIS_PrintLayoutMap.Create(
    const _location   : TGIS_PrintLayoutLocation ;
    const _viewer     : IGIS_Viewer ;
    const _extent     : TGIS_Extent ;
    const _background : Boolean ;
    const _scale      : Double ;
    const _index      : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Map, _index ) ;

    FViewer := _viewer ;
    FExtent := _extent ;
    FBackground := _background ;
    FScale  := _scale ;
    FName   := 'map' ;
    FRealExtent := GisNoWorld ;
  end ;

  class function TGIS_PrintLayoutMap.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutMap.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                nil,
                GisNoWorld,
                True,
                1,
                1
              ) ;
  end ;

  procedure TGIS_PrintLayoutMap.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_MAP ) ;

    inherited Write( nd ) ;
  end ;

  constructor TGIS_PrintLayoutLegend.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _control  : IGIS_PrintableControl ;
    const _scale    : Double ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Legend, _index ) ;

    FControl := _control ;
    FScale   := _scale ;
    FName    := 'legend' ;
    FCompactView   := False ;
    FDrawIconStyle := TGIS_LegendIconStyle.Default ;
    FReverseOrder  := False ;
    FFont      := 'Arial' ;
    FFontSize  := 8 ;
    FFontColor := TGIS_Color.Black ;
    FCompactViewStr   := GIS_PARAM_NIL ;
    FDrawIconStyleStr := GIS_PARAM_NIL ;
    FReverseOrderStr  := GIS_PARAM_NIL ;
    FFontStr          := GIS_PARAM_NIL ;
    FFontSizeStr      := GIS_PARAM_NIL ;
    FFontColorStr     := GIS_PARAM_NIL ;
  end ;

  class function TGIS_PrintLayoutLegend.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutLegend.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                nil,
                1,
                1
              ) ;
  end ;

  procedure TGIS_PrintLayoutLegend.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_LEGEND ) ;
    inherited Write( nd ) ;

    nd.Attributes['compactView']   := ConstructParamBoolean( CompactView ) ;
    nd.Attributes['drawIconStyle'] := ConstructParamLegendIconStyle( DrawIconStyle ) ;
    nd.Attributes['reverseOrder']  := ConstructParamBoolean( ReverseOrder ) ;
    nd.Attributes['font']          := Font ;
    nd.Attributes['fontSize']      := FontSize ;
    nd.Attributes['fontColor']     := ConstructParamColor( FontColor ) ;
  end ;

  constructor TGIS_PrintLayoutScale.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _control  : IGIS_PrintableControl ;
    const _scale    : Double ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Scale, _index ) ;

    FControl := _control ;
    FScale   := _scale ;
    FName    := 'scale' ;
    FDividers      := 5 ;
    FDividerColor1 := TGIS_Color.Black ;
    FDividerColor2 := TGIS_Color.White ;
    FFont          := 'Arial' ;
    FFontSize      := 8 ;
    FFontColor     := TGIS_Color.Black ;
    FDividersStr      := GIS_PARAM_NIL ;
    FDividerColor1Str := GIS_PARAM_NIL ;
    FDividerColor2Str := GIS_PARAM_NIL ;
    FFontStr          := GIS_PARAM_NIL ;
    FFontSizeStr      := GIS_PARAM_NIL ;
    FFontColorStr     := GIS_PARAM_NIL ;
  end ;

  class function TGIS_PrintLayoutScale.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutScale.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                nil,
                1,
                1
              ) ;
  end ;

  procedure TGIS_PrintLayoutScale.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_SCALE ) ;
    inherited Write( nd ) ;

    nd.Attributes['dividers']      := ConstructParamInteger( Dividers ) ;
    nd.Attributes['dividerColor1'] := ConstructParamColor( DividerColor1 ) ;
    nd.Attributes['dividerColor2'] := ConstructParamColor( DividerColor2 ) ;
    nd.Attributes['font']          := Font ;
    nd.Attributes['fontSize']      := FontSize ;
    nd.Attributes['fontColor']     := ConstructParamColor( FontColor ) ;
  end ;

  constructor TGIS_PrintLayoutNorthArrow.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _control  : IGIS_PrintableControl ;
    const _scale    : Double ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.NorthArrow, _index ) ;

    FControl := _control ;
    FScale   := _scale ;
    FName    := 'northArrow' ;
    FColor1  := TGIS_Color.Black ;
    FColor2  := TGIS_Color.Black ;
    FStyle   := TGIS_ControlNorthArrowStyle.Arrow1 ;
    FPath    := '' ;
    FStyleStr  := GIS_PARAM_NIL ;
    FColor1Str := GIS_PARAM_NIL ;
    FColor2Str := GIS_PARAM_NIL ;
    FPathStr   := GIS_PARAM_NIL ;
  end ;

  class function TGIS_PrintLayoutNorthArrow.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutNorthArrow.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                nil,
                1,
                1
              ) ;
  end ;

  procedure TGIS_PrintLayoutNorthArrow.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_NORTHARROW ) ;
    inherited Write( nd ) ;

    nd.Attributes['style']  := ConstructParamNorthArrowStyle( Style ) ;
    nd.Attributes['color1'] := ConstructParamColor( Color1 ) ;
    nd.Attributes['color2'] := ConstructParamColor( Color2 ) ;
    if not IsStringEmpty( Path ) then
      nd.Attributes['path']   := Path ;
  end ;

  constructor TGIS_PrintLayoutGraphic.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _graphic  : TGIS_TemplateGraphic ;
    const _path     : String ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Graphic, _index ) ;

    FPath    := _path ;
    FGraphic := _graphic ;
    FName    := 'graphic' ;
  end ;

  class function TGIS_PrintLayoutGraphic.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutGraphic.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                nil,
                '',
                0
              ) ;
  end ;

  procedure TGIS_PrintLayoutGraphic.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_GRAPHIC ) ;

    inherited Write( nd ) ;

    nd.Attributes['path'] := Path ;
  end;

  procedure TGIS_PrintLayoutGraphic.WriteIndex(
    const _node  : IXMLNode ;
    const _index : Integer
  ) ;
  begin
    _node.Attributes['index'] := _index ;
  end ;

  constructor TGIS_PrintLayoutText.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _text     : String ;
    const _font     : String ;
    const _style    : TGIS_FontStyles ;
    const _size     : Integer ;
    const _color    : TGIS_Color ;
    const _align    : TGIS_LabelAlignment ;
    const _bgColor  : TGIS_Color  ;
    const _bgWidth  : TGIS_PrintLayoutWidth ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Text, _index ) ;

    FText     := _text ;
    FFont     := _font ;
    FStyle    := _style ;
    FSize     := _size ;
    FColor    := _color ;
    FAlign    := _align ;
    FName     := 'text' ;
    FBgColor  := _bgColor ;
    FBgWidth  := _bgWidth ;
  end ;

  class function TGIS_PrintLayoutText.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutText.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                'Text',
                'Arial',
                GisGetEmptyFontStyle,
                8,
                TGIS_Color.Black,
                TGIS_LabelAlignment.LeftJustify,
                TGIS_Color.None,
                TGIS_PrintLayoutWidth.Create(0, TGIS_PrintLayoutUnits.uPx),
                -1
              ) ;
  end ;

  procedure TGIS_PrintLayoutText.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_TEXT ) ;

    inherited Write( nd ) ;

    nd.Attributes['alignment'] := ConstructParamAlignment( Align ) ;
    nd.Attributes['color']     := ConstructParamColor( Color ) ;
    nd.Attributes['font']      := Font ;
    nd.Attributes['fontSize']  := Size ;
    nd.Attributes['fontStyle'] := ConstructParamFontStyle( Style ) ;
    nd.Attributes['text']      := Text ;
    nd.Attributes['bgColor']   := ConstructParamColor( BackgroundColor ) ;
    nd.Attributes['bgWidth']   := BackgroundWidth.AsText ;
  end ;

  procedure TGIS_PrintLayoutText.WriteIndex(
    const _node  : IXMLNode ;
    const _index : Integer
  ) ;
  begin
    _node.Attributes['index'] := _index ;
  end ;

  constructor TGIS_PrintLayoutFrame.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _color    : TGIS_Color ;
    const _width    : TGIS_PrintLayoutWidth ;
    const _index    : Integer
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Frame, _index ) ;

    FColor := _color ;
    FWidth := _width ;
    FName  := 'frame' ;
  end ;

  class function TGIS_PrintLayoutFrame.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutFrame.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                TGIS_Color.Black,
                TGIS_PrintLayoutWidth.Create(1, TGIS_PrintLayoutUnits.uPx),
                0
              ) ;
  end ;

  procedure TGIS_PrintLayoutFrame.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_FRAME ) ;

    inherited Write( nd ) ;

    nd.Attributes['color'] := ConstructParamColor( Color ) ;
    nd.Attributes['width'] := Width.AsText ;
  end ;

  constructor TGIS_PrintLayoutPage.Create(
    const _location : TGIS_PrintLayoutLocation ;
    const _width    : TGIS_PrintLayoutWidth  ;
    const _height   : TGIS_PrintLayoutWidth
  ) ;
  begin
    inherited Create( _location, TGIS_PrintLayoutElementType.Page, 1 ) ;

    FWidth  := _width ;
    FHeight := _height ;
    FName   := 'page' ;
  end ;

  class function TGIS_PrintLayoutPage.CreateDefault: TGIS_PrintLayoutElement;
  begin
    Result := TGIS_PrintLayoutPage.Create(
                TGIS_PrintLayoutLocation.CreateDefault,
                TGIS_PrintLayoutWidth.Create(1, TGIS_PrintLayoutUnits.uPx),
                TGIS_PrintLayoutWidth.Create(1, TGIS_PrintLayoutUnits.uPx)
              ) ;
  end ;

  procedure TGIS_PrintLayoutPage.Write(
    const _node : IXMLNode
  ) ;
  var
    nd : IXMLNode ;
  begin
    nd := _node.AddChild( GIS_TPL_PAGESIZE ) ;

    nd.Attributes['width' ] := Width.AsText ;
    nd.Attributes['height'] := Height.AsText ;
  end ;

{$ENDREGION}

  {$REGION 'TGIS_TemplatePrintBuilder'}
//=============================================================================
// TGIS_TemplatePrintBuilder
//=============================================================================

  constructor TGIS_TemplatePrintBuilder.Create(
    const _template : TGIS_TemplatePrint ;
    const _recreate : Boolean
  ) ;
  begin
    inherited Create ;

    FTemplate    := _template ;
    FElements    := TObjectList<TGIS_PrintLayoutElement>.Create( True ) ;
    FPageElement := nil ;
    FRecreating  := _recreate ;

    print_area  := Rect( 0, 0, 480, 600 ) ;
    page_width  := 480 ;
    page_height := 600 ;
    dpi         := 96 ;
  end ;

  constructor TGIS_TemplatePrintBuilder.Create(
    const _template : TGIS_TemplatePrint
  ) ;
  begin
    Create( _template, False ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.doDestroy ;
  begin
    FreeObject( FPageElement ) ;
    FreeObject( FElements ) ;
    inherited ;
  end ;

  function TGIS_TemplatePrintBuilder.fget_Element(
    _index: Integer
  ) : TGIS_PrintLayoutElement;
  begin
    if ( _index >= 0 ) and ( _index < FElements.Count ) then
      Result := FElements[_index]
    else
      Result := nil ;
  end;

  function TGIS_TemplatePrintBuilder.fget_ElementsCount: Integer;
  begin
    Result := FElements.Count ;
  end ;

  {$IFNDEF GIS_XDK}
    function TGIS_TemplatePrintBuilder.GetEnumerator :
      {$IFDEF CLR}
        IEnumerator
      {$ELSE}
        TEnumerator<TGIS_PrintLayoutElement>
      {$ENDIF} ;
    begin
      Result := FElements.GetEnumerator ;
    end ;
  {$ENDIF}

  procedure TGIS_TemplatePrintBuilder.AddElement(
    const _elm : TGIS_PrintLayoutElement
  ) ;
  begin
    FElements.Add( _elm ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.ClearElements ;
  begin
    FElements.Clear ;
    FreeObject( FPageElement ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.RemoveElement(
    const _index : Integer
  ) ;
  begin
    if ( _index >= 0 ) and ( _index < FElements.Count ) then
      {$IFDEF CLR}
        FElements.RemoveAt( _index ) ;
      {$ELSE}
        FElements.Delete( _index ) ;
      {$ENDIF}
  end ;

  procedure TGIS_TemplatePrintBuilder.MoveElement(
    const _index  : Integer;
    const _step   : Integer
  ) ;
  begin
    if ( _index >= 0 ) and ( _index < FElements.Count ) then
      {$IFDEF CLR}
        FElements.Move( _index, _index + _step ) ;
      {$ELSE}
        FElements.Move( _index, _index + _step ) ;
      {$ENDIF}
  end ;

  function TGIS_TemplatePrintBuilder.NewElement(
    const _type : TGIS_PrintLayoutElementType
  ) : TGIS_PrintLayoutElement ;
  var
    elm : TGIS_PrintLayoutElement ;
  begin
    elm := nil ;
    case _type of
      TGIS_PrintLayoutElementType.Box        : elm := TGIS_PrintLayoutBox.CreateDefault ;
      TGIS_PrintLayoutElementType.Map        : elm := TGIS_PrintLayoutMap.CreateDefault ;
      TGIS_PrintLayoutElementType.Legend     : elm := TGIS_PrintLayoutLegend.CreateDefault ;
      TGIS_PrintLayoutElementType.Scale      : elm := TGIS_PrintLayoutScale.CreateDefault ;
      TGIS_PrintLayoutElementType.NorthArrow : elm := TGIS_PrintLayoutNorthArrow.CreateDefault ;
      TGIS_PrintLayoutElementType.Graphic    : elm := TGIS_PrintLayoutGraphic.CreateDefault ;
      TGIS_PrintLayoutElementType.Text       : elm := TGIS_PrintLayoutText.CreateDefault ;
      TGIS_PrintLayoutElementType.Frame      : elm := TGIS_PrintLayoutFrame.CreateDefault
    else
      assert( True, 'Untested case' ) ;
    end ;

    FElements.Add( elm ) ;
    Result := elm ;
  end ;

  procedure TGIS_TemplatePrintBuilder.UpdateLocation(
    const _elm    : TGIS_PrintLayoutElement ;
    const _bounds : TRect
  ) ;
  begin
   if ( ( _elm.Location.Rectangle.Left   = _bounds.Left   ) and
        ( _elm.Location.Rectangle.Top    = _bounds.Top    ) and
        ( _elm.Location.Rectangle.Right  = _bounds.Right  ) and
        ( _elm.Location.Rectangle.Bottom = _bounds.Bottom )
      ) then exit ;

    _elm.Location.UpdateToRectangle(
      _bounds, print_area, page_width, page_height, dpi
    ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.UpdateLocationEx(
    const _elm         : TGIS_PrintLayoutElement ;
    const _bounds      : TRect ;
    const _ratio       : Double ;
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) ;
  begin
    if ( ( _elm.Location.Rectangle.Left   = _bounds.Left   ) and
         ( _elm.Location.Rectangle.Top    = _bounds.Top    ) and
         ( _elm.Location.Rectangle.Right  = _bounds.Right  ) and
         ( _elm.Location.Rectangle.Bottom = _bounds.Bottom )
       ) then exit ;

    _elm.Location.UpdateToRectangleEx(
      _bounds, _print_area, _page_width, _page_height, _dpi
    ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.UpdateRectangle(
    const _elm : TGIS_PrintLayoutElement
  ) ;
  begin
    _elm.Location.UpdateRectangle(
      print_area, page_width, page_height, dpi
    ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.UpdateRectangleEx(
    const _elm         : TGIS_PrintLayoutElement ;
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer ;
    const _dpi         : Integer
  ) ;
  begin
    _elm.Location.UpdateRectangleEx(
      _print_area, _page_width, _page_height, _dpi
    ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.ProcessTemplate(
    const _dpi         : Integer ;
    const _print_area  : TRect ;
    const _page_width  : Integer ;
    const _page_height : Integer
  ) ;
  var
    oConfig     : TGIS_Config ;
    tkn         : TGIS_Tokenizer ;
    scales      : array of Double ;
    extents     : array of TGIS_Extent ;
    node        : IXMLNode ;
    elm         : TGIS_PrintLayoutElement ;
    is_xml      : Boolean ;
    page_only   : Boolean ;

    {$REGION 'Template parser'}
    procedure set_scale( const _idx : Integer ; const _val : Double ) ;
    begin
      if _idx >= length( scales ) then
        SetLength( scales, _idx + 1 ) ;

      scales[ _idx ] := _val ;
    end ;

    function get_scale( const _idx : Integer ) : Double ;
    begin
      Result := 0 ;
      if ( _idx >= 1 ) and ( _idx < length( scales ) ) then
        Result := scales[ _idx ] ;
    end ;

    procedure set_extent( const _idx : Integer ; const _val : TGIS_Extent ) ;
    var
      len : Integer ;
      i   : Integer ;
    begin
      len := _idx - length( extents ) + 1 ;
      if _idx >= length( extents ) then begin
        SetLength( extents, _idx + 1 ) ;
        for i := len downto 1 do
          extents[ length(extents)-i ] := GisNoWorld ;
      end ;

      extents[ _idx ] := _val ;
    end ;

    function get_extent( const _idx : Integer ) : TGIS_Extent ;
    begin
      Result := GisNoWorld ;
      if ( _idx >= 1 ) and ( _idx < length( extents ) ) then
        Result := extents[ _idx ] ;
    end ;

    procedure return_real_extent ;
    var
      idx : Integer ;
      ext : TGIS_Extent ;
    begin
      for idx := 1 to FTemplate.GIS_ViewerCount do
        if assigned( FTemplate.GIS_Viewer[idx] ) then begin
          ext := get_extent( idx ) ;
          if not GisIsNoWorld( ext ) then
            FTemplate.GIS_ViewerExtent[idx] := ext ;
        end;
    end ;

    function convert_string( const _idx : Integer ) : String ; overload ;
    begin
      if tkn.Result.Count >= _idx then
        Result := tkn.Result[ _idx - 1 ]
      else
        Result := '' ;
    end ;

    function convert_string( const _name : String ) : String ; overload ;
    begin
      Result := VarToString( node.Attributes[ _name ] ) ;
    end ;

    function convert_color( const _idx : Integer ) : String ; overload ;
    begin
      Result := convert_string( _idx ) ;
      if IsStringEmpty( Result ) then
        Result := GIS_PARAM_NIL ;
    end ;

    function convert_color( const _name : String ) : String ; overload ;
    begin
      Result := convert_string( _name ) ;
      if IsStringEmpty( Result ) then
        Result := GIS_PARAM_NIL ;
    end ;

    function get_indexXml : Integer ;
    var
      str : String ;
    begin
      str := VarToString( node.Attributes[ 'index' ] ) ;
      if not TryStrToInt( str, Result ) then
        Result := 0 ;
    end ;

    function get_nameXml : String ;
    begin
      Result := VarToString( node.Attributes[ 'name' ] ) ;
    end ;

    procedure convert_size(
      const _txt        : String ;
      const _fix_minus  : Boolean ;
        var _sign       : Integer ;
        var _value      : Double ;
        var _margin     : Boolean ;
        var _units      : String
    ) ;  overload ;
    var
      j         : Integer ;
      c         : Char    ;
      txt       : String  ;
      state     : Integer ;
      sval      : String  ;
      dval      : Double  ;
      measure   : String  ;
    begin
      _sign   := 1 ;
      _margin := False ;

      state   := 0  ;
      sval    := '' ;
      measure := '' ;
      txt     := _txt ;

      for j := StringFirst to length( txt )+StringFirst-1 do begin
        c := txt[ j ] ;

        case state of
          0 : if c = '!' then begin
                _sign := -1 ;
              end
              else if c = '*' then begin
                _margin := True ;
              end
              else begin
                state  := 1 ;
                sval   := c ;
              end ;
          1 : if CharInSet( c, ['0','1','2','3','4','5','6','7','8','9', '.', '-', '+', ' ' ] ) then begin
                sval := sval + c ;
              end
              else begin
                state   := 2 ;
                measure := c ;
              end ;
          2 : measure := measure + c ;
        end ;
      end ;

      sval := Trim( sval ) ;
      try
        if not IsStringEmpty( sval ) then
          dval := DotStrToFloat( sval )
        else
          dval := 0 ;
      except
        dval := 0 ;
      end ;

      if _fix_minus then begin
        if length( sval ) > 0 then begin
          if sval[StringFirst] = '-' then
            dval := dval - 1e-7 ;
        end ;
      end ;

      _value := dval ;
      _units := measure ;
    end ;

    function txt_to_units( const _str : String ) : TGIS_PrintLayoutUnits ;
    begin
      Result := TGIS_PrintUtils.TextToUnit( _str ) ;
    end ;

    function convert_size(
      const _txt    : String ;
      var   _sign   : Integer ;
      var   _margin : Boolean
    ) : Double ;  overload ;
    var
      v : Double ;
      u : String ;

      function t( const _str : String ) : Boolean ;
      begin
        Result := CompareText( _str, u ) = 0 ;
      end ;

    begin
      convert_size( _txt, True, _sign, v, _margin, u ) ;

      if      t( TPL_SIZE_CM ) then Result := v * 1440 / 2.54
      else if t( TPL_SIZE_MM ) then Result := v * 1440 / 2.54 / 10
      else if t( TPL_SIZE_IN ) then Result := v * 1440
      else if t( TPL_SIZE_PT ) then Result := v * 1440 / 72
      else                          Result := v * 1440 / dpi ;
    end ;

    function convert_size(
      const _idx    : Integer ;
      var   _sign   : Integer ;
      var   _margin : Boolean
    ) : Double ; overload ;
    begin
      Result := convert_size( Trim( convert_string(_idx) ), _sign, _margin ) ;
    end ;

    function convert_width( const _idx : Integer ) : Integer ; overload ;
    var
      s : Integer ;
      m : Boolean ;
    begin
      Result := RoundS( convert_size( _idx, s, m ) ) ;
      Result := Result * dpi ;
      if Result > 1440 then Result := RoundS( 1.0*Result /1440 )
                       else Result := 1 ;
    end ;

    function convert_width( const _str : String ) : Integer ; overload ;
    var
      s : Integer ;
      m : Boolean ;
    begin
      Result := RoundS( convert_size( _str, s, m ) ) ;
      Result := Result * dpi ;
      if Result > 1440 then Result := RoundS( 1.0*Result /1440 )
                       else Result := 1 ;
    end ;

    function val_to_anchor(
      const _v          : Double ;
      const _margin     : Boolean ;
      const _left_right : Boolean
    ) : TGIS_PrintLayoutAnchor ;
    begin
      if _left_right then begin
        if _v >= 0 then begin
          if _margin then
            Result := TGIS_PrintLayoutAnchor.aLeftMargin
          else
            Result := TGIS_PrintLayoutAnchor.aLeftBorder
        end else begin
          if _margin then
            Result := TGIS_PrintLayoutAnchor.aRightMargin
          else
            Result := TGIS_PrintLayoutAnchor.aRightBorder ;
        end;
      end
      else begin
        if _v >= 0 then begin
          if _margin then
            Result := TGIS_PrintLayoutAnchor.aTopMargin
          else
            Result := TGIS_PrintLayoutAnchor.aTopBorder
        end else begin
          if _margin then
            Result := TGIS_PrintLayoutAnchor.aBottomMargin
          else
            Result := TGIS_PrintLayoutAnchor.aBottomBorder ;
        end;
      end ;
    end ;

    function get_rect : TGIS_PrintLayoutLocation ;
    var
      sign   : Integer ;
      val    : Double ;
      margin : Boolean ;
      units  : String ;
    begin
      {$IFDEF OXYGENE}
        result := TGIS_PrintLayoutLocation.create ;
      {$ENDIF}
      convert_size( convert_string(1), False, sign, val, margin, units ) ;
      Result.Left := TGIS_PrintLayoutPosition.Create( Abs(val),
                                                 txt_to_units( units ),
                                                 val_to_anchor( val, margin, True )
                                               ) ;
      convert_size( convert_string(3), False, sign, val, margin, units ) ;
      Result.Right := TGIS_PrintLayoutPosition.Create( Abs(val),
                                                 txt_to_units( units ),
                                                 val_to_anchor( val, margin, True )
                                               ) ;
      convert_size( convert_string(2), False, sign, val, margin, units ) ;
      Result.Top := TGIS_PrintLayoutPosition.Create( Abs(val),
                                                 txt_to_units( units ),
                                                 val_to_anchor( val, margin, False )
                                               ) ;
      convert_size( convert_string(4), False, sign, val, margin, units ) ;
      Result.Bottom := TGIS_PrintLayoutPosition.Create( Abs(val),
                                                 txt_to_units( units ),
                                                 val_to_anchor( val, margin, False )
                                               ) ;
      Result.UpdateRectangle( print_area, page_width, page_height, dpi ) ;
    end ;

    function get_rectXml : TGIS_PrintLayoutLocation ;
    var
      sign   : Integer ;
      val    : Double ;
      margin : Boolean ;
      units  : String ;
    begin
      {$IFDEF OXYGENE}
        result := TGIS_PrintLayoutLocation.create ;
      {$ENDIF}
      convert_size( convert_string('left'), False, sign, val, margin, units ) ;
      Result.Left := TGIS_PrintLayoutPosition.Create( sign * Abs(val),
                                                      txt_to_units( units ),
                                                      val_to_anchor( val, margin, True )
                                                    ) ;
      convert_size( convert_string('right'), False, sign, val, margin, units ) ;
      Result.Right := TGIS_PrintLayoutPosition.Create( sign * Abs(val),
                                                       txt_to_units( units ),
                                                       val_to_anchor( val, margin, True )
                                                     ) ;
      convert_size( convert_string('top'), False, sign, val, margin, units ) ;
      Result.Top := TGIS_PrintLayoutPosition.Create( sign * Abs(val),
                                                     txt_to_units( units ),
                                                     val_to_anchor( val, margin, False )
                                                   ) ;
      convert_size( convert_string('bottom'), False, sign, val, margin, units ) ;
      Result.Bottom := TGIS_PrintLayoutPosition.Create( sign * Abs(val),
                                                        txt_to_units( units ),
                                                        val_to_anchor( val, margin, False )
                                                      ) ;
      Result.UpdateRectangle( print_area, page_width, page_height, dpi ) ;
    end ;

    function get_width( const _idx : Integer ) : TGIS_PrintLayoutWidth ;
    var
      sign   : Integer ;
      val    : Double ;
      margin : Boolean ;
      units  : String ;
    begin
      convert_size( convert_string(_idx), False, sign, val, margin, units ) ;
      Result := TGIS_PrintLayoutWidth.Create( val, txt_to_units( units ) );
      if val > 0 then
        Result.Width := convert_width( _idx )
      else
        Result.Width := 0 ;
    end ;

    function get_widthXml( const _str : String ) : TGIS_PrintLayoutWidth ;
    var
      sign   : Integer ;
      val    : Double ;
      margin : Boolean ;
      units  : String ;
    begin
      convert_size( _str, False, sign, val, margin, units ) ;
      Result := TGIS_PrintLayoutWidth.Create( val, txt_to_units( units ) );
      if val > 0 then
        Result.Width := convert_width( _str )
      else
        Result.Width := 0 ;
    end ;

    procedure set_element_info ;
    begin
      if not is_xml then exit ;

      elm.Name      := get_nameXml ;
      //elm.Index     := get_indexXml ;
      elm.Location  := get_rectXml ;
    end ;

    procedure call_draw_box(
      const _index    : Integer ;
      const _location : TGIS_PrintLayoutLocation ;
      const _cl       : TGIS_Color ;
      const _framecl  : TGIS_Color ;
      const _frwidth  : TGIS_PrintLayoutWidth
    ) ;
    begin
      elm := TGIS_PrintLayoutBox.Create( _location, _cl, _framecl, _frwidth, _index ) ;
      set_element_info ;

      if assigned( FOnElement ) then begin
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else
        FElements.Add( elm ) ;
    end ;

    function do_boxInt( const _idx : Integer ) : Boolean ;
    var
      txt : String ;
      rc  : TGIS_PrintLayoutLocation ;
      cl  : TGIS_Color ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_BOX_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc := get_rect ;
      cl := ParamColor( convert_color( 5 ), TGIS_Color.Silver ) ;

      call_draw_box( _idx, rc, cl, TGIS_Color.Black,
                     TGIS_PrintLayoutWidth.Create(0, TGIS_PrintLayoutUnits.uPx) ) ;
    end ;

    procedure do_boxXml ;
    var
      rc   : TGIS_PrintLayoutLocation  ;
      cl   : TGIS_Color ;
      fcl  : TGIS_Color ;
      wdth : TGIS_PrintLayoutWidth ;
    begin
      rc   := get_rectXml ;
      cl   := ParamColor( convert_color( 'color' ), TGIS_Color.Silver ) ;
      fcl  := ParamColor( convert_color( 'frameColor' ), TGIS_Color.Black ) ;
      wdth := get_widthXml( convert_string('frameWidth') ) ;

      call_draw_box( 0, rc, cl, fcl, wdth ) ;
    end ;

    procedure call_draw_map(
      const _index      : Integer ;
      const _rc         : TGIS_PrintLayoutLocation ;
      const _viewer     : IGIS_Viewer ;
      const _extent     : TGIS_Extent ;
      const _background : Boolean ;
      var   _scale      : Double ;
      var   _rextent    : TGIS_Extent ;
            _init       : Boolean
    ) ;
    begin
      elm := TGIS_PrintLayoutMap.Create( _rc, _viewer, _extent, _background, _scale, _index ) ;
      set_element_info ;

      if _init then begin
        try
          if assigned( FOnInitElement ) then begin
            FOnInitElement( self, elm ) ;
            _scale := TGIS_PrintLayoutMap(elm).Scale ;
            _rextent := TGIS_PrintLayoutMap(elm).RealExtent ;
          end;
        finally
          FreeObject( elm ) ;
        end ;
      end else begin
        if assigned( FOnElement ) then begin
          try
            FOnElement( self, elm ) ;
            _scale := TGIS_PrintLayoutMap(elm).Scale ;
            _rextent := TGIS_PrintLayoutMap(elm).RealExtent ;
          finally
            FreeObject( elm ) ;
          end ;
        end
        else
          FElements.Add( elm ) ;
      end ;
    end ;

    function do_mapInt( const _idx : Integer ) : Boolean ;
    var
      txt   : String ;
      rc    : TGIS_PrintLayoutLocation  ;
      scale : Double ;
      rextent : TGIS_Extent ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_MAP_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc := get_rect ;

      if FRecreating or assigned( FTemplate.GIS_Viewer[ _idx ] ) then begin
        scale := FTemplate.GIS_ViewerScale[ _idx ] ;
        rextent := GisNoWorld ;
        call_draw_map( _idx, rc, FTemplate.GIS_Viewer[ _idx ],
                       FTemplate.GIS_ViewerExtent[ _idx ],
                       True, scale, rextent, False ) ;
        set_scale( _idx, scale ) ;
        set_extent( _idx, rextent ) ;
      end ;
    end ;

    procedure do_mapXml(
      _init : Boolean
    ) ;
    var
      rc    : TGIS_PrintLayoutLocation  ;
      scale : Double ;
      idx   : Integer ;
      rextent : TGIS_Extent ;
    begin
      idx := get_indexXml ;
      rc := get_rectXml ;

      if ( idx > 0 ) and ( idx <= FTemplate.GIS_ViewerCount ) then begin
        scale := FTemplate.GIS_ViewerScale[ idx ] ;
        rextent := FTemplate.GIS_ViewerExtent[ idx ] ;
        call_draw_map( idx, rc, FTemplate.GIS_Viewer[ idx ],
                       FTemplate.GIS_ViewerExtent[ idx ],
                       FTemplate.GIS_UseViewerColor[ idx ],
                       scale, rextent, _init ) ;
        set_scale( idx, scale ) ;
        set_extent( idx, rextent ) ;
      end else begin
        scale := 0 ;
        call_draw_map( idx, rc, nil, GisNoWorld, False, scale, rextent, _init ) ;
      end ;
    end ;

    procedure call_draw_control(
      const _index   : Integer ;
      const _rc      : TGIS_PrintLayoutLocation   ;
      const _control : IGIS_PrintableControl ;
      const _ctype   : TGIS_PrintLayoutControlType ;
      const _scale   : Double
    ) ;
    begin
      case _ctype of
        TGIS_PrintLayoutControlType.Legend :
          elm := TGIS_PrintLayoutLegend.Create( _rc, _control, _scale, _index ) ;
        TGIS_PrintLayoutControlType.Scale :
          elm := TGIS_PrintLayoutScale.Create( _rc, _control, _scale, _index ) ;
        TGIS_PrintLayoutControlType.NorthArrow :
          elm := TGIS_PrintLayoutNorthArrow.Create( _rc, _control, _scale, _index ) ;
      end;
      set_element_info ;

      if assigned( FOnElement ) then begin
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else
        FElements.Add( elm ) ;
    end ;

    procedure call_draw_legend(
      const _index     : Integer ;
      const _rc        : TGIS_PrintLayoutLocation ;
      const _control   : IGIS_PrintableControl ;
      const _scale     : Double ;
      const _compact   : String ;
      const _iconStyle : String ;
      const _reverse   : String ;
      const _fname     : String ;
      const _fsize     : String ;
      const _fcolor    : String
    ) ;
    begin
      elm := TGIS_PrintLayoutLegend.Create( _rc, _control, _scale, _index ) ;
      if not IsStringEmpty( _compact ) then
        TGIS_PrintLayoutLegend(elm).CompactViewStr := _compact ;
      if not IsStringEmpty( _iconStyle ) then
        TGIS_PrintLayoutLegend(elm).DrawIconStyleStr := _iconStyle ;
      if not IsStringEmpty( _reverse ) then
        TGIS_PrintLayoutLegend(elm).ReverseOrderStr := _reverse ;
      if not IsStringEmpty( _fname ) then
        TGIS_PrintLayoutLegend(elm).FontStr := _fname ;
      if not IsStringEmpty( _fsize ) then
        TGIS_PrintLayoutLegend(elm).FontSizeStr := _fsize ;
      if not IsStringEmpty( _fcolor ) then
        TGIS_PrintLayoutLegend(elm).FontColorStr := _fcolor ;
      set_element_info ;

      if assigned( FOnElement ) then begin
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else
        FElements.Add( elm ) ;
    end ;

    procedure call_draw_scale(
      const _index   : Integer ;
      const _rc      : TGIS_PrintLayoutLocation   ;
      const _control : IGIS_PrintableControl ;
      const _scale   : Double ;
      const _dividers      : String ;
      const _dividerColor1 : String ;
      const _dividerColor2 : String ;
      const _font          : String ;
      const _fontSize      : String ;
      const _fontColor     : String
    ) ;
    begin
      elm := TGIS_PrintLayoutScale.Create( _rc, _control, _scale, _index ) ;
      if not IsStringEmpty( _dividers ) then
        TGIS_PrintLayoutScale(elm).DividersStr  := _dividers ;
      if not IsStringEmpty( _dividerColor1 ) then
        TGIS_PrintLayoutScale(elm).DividerColor1Str := _dividerColor1 ;
      if not IsStringEmpty( _dividerColor2 ) then
        TGIS_PrintLayoutScale(elm).DividerColor2Str := _dividerColor2 ;
      if not IsStringEmpty( _font ) then
        TGIS_PrintLayoutScale(elm).FontStr      := _font ;
      if not IsStringEmpty( _fontSize ) then
        TGIS_PrintLayoutScale(elm).FontSizeStr  := _fontSize ;
      if not IsStringEmpty( _fontColor ) then
        TGIS_PrintLayoutScale(elm).FontColorStr := _fontColor ;
      set_element_info ;

      if assigned( FOnElement ) then begin
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else
        FElements.Add( elm ) ;
    end ;

    procedure call_draw_northarrow(
      const _index   : Integer ;
      const _rc      : TGIS_PrintLayoutLocation   ;
      const _control : IGIS_PrintableControl ;
      const _scale   : Double ;
      const _style   : String ;
      const _color1  : String ;
      const _color2  : String ;
      const _path    : String
    ) ;
    begin
      elm := TGIS_PrintLayoutNorthArrow.Create( _rc, _control, _scale, _index ) ;
      if not IsStringEmpty( _style ) then
        TGIS_PrintLayoutNorthArrow(elm).StyleStr  := _style  ;
      if not IsStringEmpty( _color1 ) then
        TGIS_PrintLayoutNorthArrow(elm).Color1Str := _color1 ;
      if not IsStringEmpty( _color2 ) then
        TGIS_PrintLayoutNorthArrow(elm).Color2Str := _color2 ;
      if not IsStringEmpty( _path ) then
        TGIS_PrintLayoutNorthArrow(elm).PathStr   := _path ;

      set_element_info ;

      if assigned( FOnElement ) then begin
        try
          if not IsStringEmpty( _path ) then
            TGIS_PrintLayoutNorthArrow(elm).PathStr :=
              GetPathAbsolute( GetFilePath(FTemplate.TemplatePath), _path ) ;
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else
        FElements.Add( elm ) ;
    end ;

    function do_legendInt( const _idx : Integer ) : Boolean ;
    var
      txt : String ;
      rc  : TGIS_PrintLayoutLocation  ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_LEGEND_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc := get_rect ;

      if FRecreating or assigned( FTemplate.GIS_Legend[ _idx ] ) then
        call_draw_control( _idx, rc, FTemplate.GIS_Legend[ _idx ],
                           TGIS_PrintLayoutControlType.Legend, get_scale(_idx)
                          ) ;
    end ;

    procedure do_legendXml ;
    var
      rc  : TGIS_PrintLayoutLocation  ;
      idx : Integer ;
      legend : IGIS_PrintableControl ;
      create_copy : Boolean ;
    begin
      idx := get_indexXml ;
      rc := get_rectXml ;

      if ( idx > 0 ) and ( idx <= FTemplate.GIS_LegendCount ) and
         assigned( FTemplate.GIS_Legend[idx] ) then begin

        create_copy := IsStringEmpty( FTemplate.GIS_Legend[idx].InternalName ) ;
        if create_copy then
          legend := FTemplate.GIS_Legend[idx].CreateCopy
        else
          legend := FTemplate.GIS_Legend[idx] ;
        try
          call_draw_legend( idx, rc, legend, get_scale(idx),
                            convert_string( 'compactView' ),
                            convert_string( 'drawIconStyle' ),
                            convert_string( 'reverseOrder' ),
                            convert_string( 'font' ),
                            convert_string( 'fontSize' ),
                            convert_string( 'fontColor' )
                          )
        finally
          if create_copy then
            FTemplate.GIS_Legend[idx].FreeCopy( legend ) ;
        end;
      end else
        call_draw_control( idx, rc, nil,
                           TGIS_PrintLayoutControlType.Legend, 0
                         ) ;
    end ;

    function do_scaleInt( const _idx : Integer ) : Boolean ;
    var
      txt  : String  ;
      rc   : TGIS_PrintLayoutLocation   ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_SCALE_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc := get_rect ;

      if FRecreating or assigned( FTemplate.GIS_Scale[ _idx ] ) then
        call_draw_control( _idx, rc, FTemplate.GIS_Scale[ _idx ],
                           TGIS_PrintLayoutControlType.Scale, get_scale(_idx)
                          ) ;
    end ;

    procedure do_scaleXml ;
    var
      rc    : TGIS_PrintLayoutLocation   ;
      idx   : Integer ;
      scale : IGIS_PrintableControl ;
      create_copy : Boolean ;
    begin
      idx := get_indexXml ;
      rc  := get_rectXml ;

      if ( idx > 0 ) and ( idx <= FTemplate.GIS_ScaleCount ) and
         assigned( FTemplate.GIS_Scale[idx] ) then begin

        create_copy := IsStringEmpty( FTemplate.GIS_Scale[idx].InternalName ) ;
        if create_copy then
          scale := FTemplate.GIS_Scale[idx].CreateCopy
        else
          scale := FTemplate.GIS_Scale[idx] ;
        try
          call_draw_scale( idx, rc, scale, get_scale(idx),
                           convert_string( 'dividers' ),
                           convert_string( 'dividerColor1' ),
                           convert_string( 'dividerColor2' ),
                           convert_string( 'font' ),
                           convert_string( 'fontSize' ),
                           convert_string( 'fontColor' )
                         ) ;
        finally
          if create_copy then
            FTemplate.GIS_Scale[idx].FreeCopy( scale ) ;
        end;
      end else
        call_draw_control( idx, rc, nil,
                           TGIS_PrintLayoutControlType.Scale, 0
                         )
    end ;

    function do_northarrowInt( const _idx : Integer ) : Boolean ;
    var
      txt : String  ;
      rc  : TGIS_PrintLayoutLocation   ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_NORTHARROW_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc := get_rect ;

      if FRecreating or assigned( FTemplate.GIS_NorthArrow[ _idx ] ) then
        call_draw_control( _idx, rc, FTemplate.GIS_NorthArrow[ _idx ],
                           TGIS_PrintLayoutControlType.NorthArrow, 0
                         ) ;
    end ;

    procedure do_northarrowXml ;
    var
      rc  : TGIS_PrintLayoutLocation   ;
      idx : Integer ;
      narrow : IGIS_PrintableControl ;
      create_copy : Boolean ;
    begin
      idx := get_indexXml ;
      rc  := get_rectXml ;

      if ( idx > 0 ) and ( idx <= FTemplate.GIS_NorthArrowCount ) and
         assigned( FTemplate.GIS_NorthArrow[idx] ) then begin

        create_copy := IsStringEmpty( FTemplate.GIS_NorthArrow[idx].InternalName ) ;
        if create_copy then
          narrow := FTemplate.GIS_NorthArrow[idx].CreateCopy
        else
          narrow := FTemplate.GIS_NorthArrow[idx] ;
        try
          call_draw_northarrow( idx, rc, narrow, 0,
                                convert_string( 'style' ),
                                convert_string( 'color1' ),
                                convert_string( 'color2' ),
                                convert_string( 'path' )
                              ) ;
        finally
          if create_copy then
            FTemplate.GIS_NorthArrow[idx].FreeCopy( narrow ) ;
        end ;
      end else
        call_draw_control( idx, rc, nil,
                           TGIS_PrintLayoutControlType.NorthArrow, 0
                         ) ;
    end ;

    procedure call_draw_graphic(
      const _index    : Integer ;
      const _rc       : TGIS_PrintLayoutLocation ;
      const _graphics : TGIS_TemplateGraphic ;
      const _path     : String
    ) ;
    var
      spath : String ;
    begin
      if assigned( FOnElement ) then
        spath := GetPathAbsolute( GetFilePath( GetPathAbsolute( '', FTemplate.TemplatePath ) ),
                                  _path
                                )
      else
        spath := _path ;

      elm := TGIS_PrintLayoutGraphic.Create( _rc, _graphics, spath, _index ) ;
      set_element_info ;

      if assigned( FOnElement ) then begin
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else
        FElements.Add( elm ) ;
    end ;

    function do_graphicInt( const _idx : Integer ) : Boolean ;
    var
      txt   : String ;
      rc    : TGIS_PrintLayoutLocation  ;
      spath : String ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_GRAPHIC_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc    := get_rect ;
      spath := convert_string( 5 ) ;

      if ( _idx > 0 ) and ( _idx <= FTemplate.GraphicCount ) and
         assigned( FTemplate.Graphic[_idx] ) then
        call_draw_graphic( _idx, rc, FTemplate.Graphic[_idx], spath )
      else
        call_draw_graphic( _idx, rc, nil, spath ) ;
    end ;

    procedure do_graphicXml ;
    var
      idx   : Integer ;
      rc    : TGIS_PrintLayoutLocation  ;
      spath : String ;
      name  : String ;
    begin
      idx   := get_indexXml ;
      rc    := get_rectXml ;
      spath := convert_string( 'path' ) ;
      name  := convert_string( 'name' ) ;

      if ( idx > 0 ) and ( idx <= FTemplate.GraphicCount ) and
         assigned( FTemplate.Graphic[idx] ) then
        call_draw_graphic( idx, rc, FTemplate.Graphic[idx], spath )
      else
        call_draw_graphic( idx, rc, nil, spath ) ;
    end ;

    procedure call_draw_text(
      const _index    : Integer     ;
      const _rc       : TGIS_PrintLayoutLocation       ;
      const _text     : String      ;
      const _font     : String      ;
      const _style    : TGIS_FontStyles ;
      const _size     : Integer     ;
      const _color    : TGIS_Color  ;
      const _align    : TGIS_LabelAlignment ;
      const _bgColor  : TGIS_Color  ;
      const _bgWidth  : TGIS_PrintLayoutWidth
    ) ;
    var
      txt : String ;
    begin
      if assigned( FOnElement ) then begin
        try
          if ( _index > 0 ) and
             not IsStringEmpty( FTemplate.Text[_index] ) then
            txt := FTemplate.Text[_index]
          else if ( _index = -1 ) then
            txt := ''
          else
            txt := _text ;
          elm := TGIS_PrintLayoutText.Create( _rc, txt, _font, _style, _size,
                                          _color, _align, _bgColor, _bgWidth,
                                          _index
                                        ) ;
          set_element_info ;
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else begin
        elm := TGIS_PrintLayoutText.Create( _rc, _text, _font, _style, _size,
                                            _color, _align, _bgColor, _bgWidth,
                                            _index
                                          ) ;
        set_element_info ;
        FElements.Add( elm ) ;
      end ;
    end ;

    function do_textInt( const _idx : Integer ) : Boolean ;
    var
      txt      : String  ;
      rc       : TGIS_PrintLayoutLocation   ;
      al       : TGIS_LabelAlignment ;
      cl       : TGIS_Color ;
      fname    : String  ;
      fsize    : Integer ;
      fstyle   : TGIS_FontStyles ;
      bgcl     : TGIS_Color ;
      bgwidth  : TGIS_PrintLayoutWidth ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_TEXT_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc       := get_rect ;
      al       := ParamAlignment  ( convert_string( 5 ) ,
                                    TGIS_LabelAlignment.LeftJustify
                                  ) ;
      cl       := ParamColor      ( convert_color( 6 ) ,
                                    TGIS_Color.Black
                                  ) ;
      fname    := ParamString     ( convert_string( 7 ) ,
                                    'Arial'
                                  ) ;
      fsize    := ParamInteger    ( convert_string( 8 ) ,
                                    14
                                  ) ;
      fstyle   := ParamFontStyle  ( convert_string( 9 ) ,
                                    GisGetEmptyFontStyle
                                  ) ;
      // old charset
                  ParamString     ( convert_string( 10 ),
                                    'DEFAULT'
                                  ) ;
      txt      := ParamString     ( convert_string( 11 ),
                                    ''
                                  ) ;
      bgcl     := ParamColor      ( convert_color( 12 ) ,
                                    TGIS_Color.None
                                  ) ;
      bgwidth  := get_width       ( 13
                                  ) ;

      call_draw_text( _idx, rc, txt, fname, fstyle, fsize, cl, al, bgcl, bgwidth ) ;
    end ;

    procedure do_textXml ;
    var
      v        : OleVariant ;
      txt      : String  ;
      rc       : TGIS_PrintLayoutLocation   ;
      al       : TGIS_LabelAlignment ;
      cl       : TGIS_Color ;
      fname    : String  ;
      fsize    : Integer ;
      fstyle   : TGIS_FontStyles ;
      idx      : Integer ;
      bgcl     : TGIS_Color ;
      bgwidth  : TGIS_PrintLayoutWidth ;
    begin
      idx      := get_indexXml ;
      rc       := get_rectXml ;
      al       := ParamAlignment  ( convert_string( 'alignment' ),
                                    TGIS_LabelAlignment.LeftJustify
                                  ) ;
      cl       := ParamColor      ( convert_color( 'color' ) ,
                                    TGIS_Color.Black
                                  ) ;
      fname    := ParamString     ( convert_string( 'font' ) ,
                                    'Arial'
                                  ) ;
      fsize    := ParamInteger    ( convert_string( 'fontSize' )  ,
                                    14
                                  ) ;
      fstyle   := ParamFontStyle  ( convert_string( 'fontStyle' ) ,
                                    GisGetEmptyFontStyle
                                  ) ;
      v := Unassigned ;
      v := node.Attributes[ 'text' ] ;
      if v <> Unassigned then
        txt    := ParamString     ( VarToString( v ),
                                    ''
                                  )
      else
        txt    := '' ;

      bgcl     := ParamColor      ( convert_color( 'bgColor' ),
                                    TGIS_Color.None ) ;
      bgwidth  := get_widthXml    ( convert_string( 'bgWidth' )
                                  ) ;

      call_draw_text( idx, rc, txt, fname, fstyle, fsize, cl, al, bgcl, bgwidth ) ;
    end ;

    procedure call_draw_frame(
      const _index  : Integer ;
      const _rc     : TGIS_PrintLayoutLocation ;
      const _color  : TGIS_Color ;
      const _width  : TGIS_PrintLayoutWidth
    ) ;
    begin
      if assigned( FOnElement ) then begin
        elm := TGIS_PrintLayoutFrame.Create( _rc, _color, _width, _index ) ;
        set_element_info ;
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else begin
        elm := TGIS_PrintLayoutBox.Create( _rc, TGIS_Color.None, _color, _width, _index ) ;
        set_element_info ;
        FElements.Add( elm ) ;
      end ;
    end ;

    function do_frameInt( const _idx : Integer ) : Boolean ;
    var
      txt  : String  ;
      rc   : TGIS_PrintLayoutLocation   ;
      cl   : TGIS_Color ;
      wdth : TGIS_PrintLayoutWidth ;
    begin
      txt := oConfig.ReadString( Format( GIS_TPL_FRAME_D, [_idx] ), '' ) ;
      if IsStringEmpty( txt ) then begin
        Result := False ;
        exit ;
      end
      else
        Result := True ;

      tkn.ExecuteEx( txt ) ;

      rc   := get_rect ;
      cl   := ParamColor( convert_color( 5 ), TGIS_Color.Black ) ;
      wdth := get_width( 6 ) ;
      if wdth.Width = 0 then
        wdth.Width := 1 ;

      call_draw_frame( _idx, rc, cl, wdth ) ;
    end ;

    procedure do_frameXml ;
    var
      rc   : TGIS_PrintLayoutLocation ;
      cl   : TGIS_Color ;
      wdth : TGIS_PrintLayoutWidth ;
    begin
      rc   := get_rectXml ;
      cl   := ParamColor( convert_color( 'color' ), TGIS_Color.Black ) ;
      wdth := get_widthXml( convert_string('width') ) ;
      if wdth.Width = 0 then
        wdth.Width := 1 ;

      call_draw_frame( 0, rc, cl, wdth ) ;
    end ;

    procedure call_do_page( const _rc     : TGIS_PrintLayoutLocation       ;
                            const _width  : TGIS_PrintLayoutWidth ;
                            const _height : TGIS_PrintLayoutWidth
                          ) ;
    begin
      elm := TGIS_PrintLayoutPage.Create( _rc, _width, _height ) ;
      if assigned( FOnElement ) then begin
        try
          FOnElement( self, elm ) ;
        finally
          FreeObject( elm ) ;
        end ;
      end
      else begin
        FreeObject( FPageElement ) ;
        FPageElement := elm ;
      end;
    end ;

    procedure do_PageInt ;
    var
      wdth : TGIS_PrintLayoutWidth ;
      hgth : TGIS_PrintLayoutWidth ;
      rc   : TGIS_PrintLayoutLocation   ;
      txt  : String ;
    begin
      txt := oConfig.ReadString( GIS_TPL_PAGESIZE, '' ) ;
      if IsStringEmpty( txt ) then exit ;

      tkn.ExecuteEx( txt ) ;

      rc   := get_rect ;
      wdth := get_width( 1 ) ;
      hgth := get_width( 2 ) ;

      call_do_page( rc, wdth, hgth ) ;
    end;

    procedure do_PageXml ;
    var
      wdth : TGIS_PrintLayoutWidth ;
      hgth : TGIS_PrintLayoutWidth ;
      rc   : TGIS_PrintLayoutLocation   ;
    begin
      rc   := get_rectXml ;
      wdth := get_widthXml( convert_string('width') ) ;
      hgth := get_widthXml( convert_string('height') ) ;

      call_do_page( rc, wdth, hgth ) ;
    end;

    procedure readTemplate ;
    var
      i   : Integer ;
      txt : String  ;
      s   : String  ;
    begin
      for i := 1 to TPL_MAX_ELEMENT_COUNT do begin
        txt := oConfig.ReadString( Format( GIS_TPL_TEXT_D, [i] ), '' ) ;
        if IsStringEmpty( txt ) then begin
          break ;
        end ;

        tkn.ExecuteEx( txt ) ;

        if tkn.Result.Count >= 11 then begin
          s := tkn.Result[ 11 - 1 ] ;
          FTemplate.Text[ i ] := s ;
        end ;
      end ;
    end ;
    {$ENDREGION}

    procedure processTemplateTextAsIni ;
    var
      i   : Integer ;
      lst : TStringList ;
    begin
      oConfig := TGIS_ConfigIni.Create( nil, '' ) ;
      try
        lst := TStringList.Create ;
        try
          lst.Text := FTemplate.TemplateText ;
          oConfig.SetStrings( lst );
        finally
          FreeObject( lst ) ;
        end ;

        oConfig.Section := GIS_TPL_GENERAL_HEADER ;

        do_PageInt ;
        if page_only then exit ;

        // draw all boxes
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_boxInt( i ) then break ;

        // draw all maps
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_mapInt( i ) then break ;

        // draw all legends
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_legendInt( i ) then break ;

        // draw all scales
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_scaleInt( i ) then break ;

        // draw all north arrows
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_northarrowInt( i ) then break ;

        // draw all graphics
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_graphicInt( i ) then break ;

        // draw all texts
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_textInt( i ) then break ;

        // draw all frames
          for i:=1 to TPL_MAX_ELEMENT_COUNT do
            if not do_frameInt( i ) then break ;
      finally
        FreeObject( oConfig ) ;
      end ;
    end ;

    function isXmlFormat : Boolean ;
    begin
      Result := Pos( '<?xml', FTemplate.TemplateText ) >= StringFirst ;
    end ;

    procedure processTemplateTextAsXml ;
    var
      i    : Integer ;
      lst  : TStringList ;
      root : IXMLNode ;
    begin
      oConfig := TGIS_ConfigXml.Create( nil, '' ) ;
      try
        lst := TStringList.Create ;
        try
          lst.Text := FTemplate.TemplateText ;
          oConfig.SetStrings( lst );
        finally
          FreeObject( lst ) ;
        end ;

        TGIS_ConfigXml(oConfig).IniObj.SetActiveNode( GIS_TPL_GENERAL );
        root := TGIS_ConfigXml(oConfig).IniObj.ActiveNode ;

        if page_only then begin
          for i := 0 to root.ChildNodes.Count-1 do begin
            node := root.ChildNodes[i] ;
            if node.NodeName = GIS_TPL_PAGESIZE then begin
              do_PageXml ;
              break ;
            end ;
          end ;
          exit ;
        end ;

        // first iteration to prepare map parameters
        for i := 0 to root.ChildNodes.Count-1 do begin
          node := root.ChildNodes[i] ;
          if node.NodeName = GIS_TPL_MAP         then
            do_mapXml( True ) ;
        end ;

        for i := 0 to root.ChildNodes.Count-1 do begin
          node := root.ChildNodes[i] ;

          if      node.NodeName = GIS_TPL_PAGESIZE    then
            do_PageXml
          else if node.NodeName = GIS_TPL_GRAPHIC     then
            do_graphicXml
          else if node.NodeName = GIS_TPL_TEXT        then
            do_textXml
          else if node.NodeName = GIS_TPL_BOX         then
            do_boxXml
          else if node.NodeName = GIS_TPL_FRAME       then
            do_frameXml
          else if node.NodeName = GIS_TPL_MAP         then
            do_mapXml( False )
          else if node.NodeName = GIS_TPL_LEGEND      then
            do_legendXml
          else if node.NodeName = GIS_TPL_SCALE       then
            do_scaleXml
          else if node.NodeName = GIS_TPL_NORTHARROW  then
            do_northarrowXml
        end ;
      finally
        FreeObject( oConfig ) ;
      end ;
    end ;

  begin
    FElements.Clear ;
    FreeObject( FPageElement ) ;

    tkn := TGIS_Tokenizer.Create ;
    try
      dpi         := _dpi ;
      print_area  := _print_area ;
      page_width  := _page_width ;
      page_height := _page_height ;

      page_only   := (page_width = -1) and (page_height = -1) ;

      is_xml := isXmlFormat ;
      if is_xml then
        processTemplateTextAsXml
      else
        processTemplateTextAsIni ;

      if page_only then exit ;

      // return real viewer extent
      return_real_extent ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_TemplatePrintBuilder.UpdateTemplate(
    const _dpi          : Integer     ;
    const _print_area   : TRect       ;
    const _page_width   : Integer     ;
    const _page_height  : Integer
  ) ;
  {$IFDEF DCC}
    var
      elm  : TGIS_PrintLayoutElement ;
  {$ENDIF}
  begin
    dpi         := _dpi ;
    print_area  := _print_area ;
    page_width  := _page_width ;
    page_height := _page_height ;

    for elm in FElements do
      UpdateRectangle( elm ) ;
  end ;

  procedure TGIS_TemplatePrintBuilder.SaveToFile(
    const _path : String
  ) ;
  var
    {$IFDEF DCC}
      elm : TGIS_PrintLayoutElement ;
    {$ENDIF}
    doc : TGIS_XMLDocument ;
    nd  : IXMLNode ;
  begin
    doc := TGIS_XMLDocument.Create ;
    try
      nd := doc.AddChild( GIS_COMMONFILES_TATUKGIS ) ;
      nd := nd.AddChild( GIS_TPL_GENERAL ) ;

      if assigned( FPageElement ) then
        FPageElement.Write( nd ) ;

      for elm in FElements do
        elm.Write( nd ) ;

      doc.SaveToFile( _path ) ;
    finally
      FreeObject( doc ) ;
    end ;
  end ;

  class procedure TGIS_TemplatePrintBuilder.CopyTemplateFile(
    const _oldPath   : String ;
    const _newPath   : String ;
    const _copyFiles : Boolean
  ) ;
  var
    template : TGIS_TemplatePrint ;
    builder  : TGIS_TemplatePrintBuilder ;
    graphic  : TGIS_PrintLayoutGraphic ;
    narrow   : TGIS_PrintLayoutNorthArrow ;
    path     : String ;
    i        : Integer ;
  begin
    template := TGIS_TemplatePrint.Create ;
    template.TemplatePath := _oldPath ;
    // open template builder in recreating mode
    builder := TGIS_TemplatePrintBuilder.Create( template, True ) ;
    // process template with dummy parameters
    builder.ProcessTemplate( 96, Rect( 0, 0, 100, 100 ), 100, 100 ) ;
    try
      // make relative paths to graphic files and north arrow files updated
      for i := 0 to builder.ElementsCount - 1  do begin
        if builder.Elements[i] is TGIS_PrintLayoutGraphic then begin
          if not _copyFiles then begin
            graphic := TGIS_PrintLayoutGraphic(builder.Elements[i]);
            path := GetPathAbsolute( GetFilePath( _oldPath ), graphic.Path ) ;
            if path <> graphic.Path then
              graphic.Path := GetPathRelative( GetFilePath( _newPath ), path ) ;
          end ;
        end
        else
        if builder.Elements[i] is TGIS_PrintLayoutNorthArrow then begin
          if not _copyFiles then begin
            narrow := TGIS_PrintLayoutNorthArrow(builder.Elements[i]);
            path := GetPathAbsolute( GetFilePath( _oldPath ), narrow.Path ) ;
            if path <> narrow.Path then
              narrow.Path := GetPathRelative( GetFilePath( _newPath ), path ) ;
          end ;
        end ;
      end ;
    finally
      builder.SaveToFile( _newPath ) ;
      FreeObject( builder ) ;
      FreeObject( template ) ;
    end;
  end ;

{$ENDREGION}

{==================================== END =====================================}


end.

