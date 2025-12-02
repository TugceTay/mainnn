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
  PVL Controls which helps us keep one code across the platforms.
}

{$IFDEF DCC}
  unit PVL.GisPvl;
  {$HPPEMIT '#pragma link "PVL.GisPvl"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

uses
  {$IFDEF CLR}
    System.Runtime.CompilerServices,
    System.Reflection,
    TatukGIS.NDK,
    TatukGIS.NDK.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    {$M+}
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Types,
    System.Generics.Collections,

    GisBaseObject,
    GisTypesUI,
    GisClasses,
    GisRtl,
    GisCSSystems,
    GisSymbol,
    GisInterfaces,
    GisPrintManagerAbstract,
    GisViewer,
    GisResource,
    GisRendererAbstract,
    GisTypes ;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    tatukgis.jdk.*,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.rtl ;
  {$ENDIF}

type
  {$IFNDEF DCC}
    /// <summary>
    ///   Alias for ordinary Object type.
    /// </summary>
    TObject = Object;
  {$ENDIF}

   /// <summary>
   ///   RGB value
   /// </summary>
  TGIS_PvlRGBVal = record

    /// <summary>
    ///   Red value
    /// </summary>
    R : Byte ;
    /// <summary>
    ///   Green value
    /// </summary>
    G : Byte ;
    /// <summary>
    ///   Blue value
    /// </summary>
    B : Byte ;
  end ;

  /// <summary>
  ///   Generic PVL event.
  /// </summary>
  /// <param name="_sender">
  ///   sender object
  /// </param>
  TGIS_PvlEvent          = procedure( _sender : TObject ) of object;

  /// <summary>
  ///   PVL key event.
  /// </summary>
  /// <param name="_sender">
  ///   Sender object
  /// </param>
  /// <param name="_key">
  ///   Key pressed
  /// </param>
  TGIS_PvlKeyEvent       = procedure( _sender : TObject; var _key : Word  ) of object;

  /// <summary>
  ///   PVL key press event.
  /// </summary>
  /// <param name="_sender">
  ///   Sender object
  /// </param>
  /// <param name="_char">
  ///   Character pressed
  /// </param>
  TGIS_PvlKeyPressEvent  = procedure( _sender : TObject; var _char : Char  ) of object;

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
  TGIS_PvlRendererEvent = procedure( _sender : TObject ; _renderer : TGIS_RendererAbstract ; _mode : TGIS_DrawMode ) of object ;


  /// <summary>
  ///   Mouse button.
  /// </summary>
  TGIS_PvlMouseButton = (

    /// <summary>
    ///   Left mouse button.
    /// </summary>
    Left,

    /// <summary>
    ///   Right mouse button.
    /// </summary>
    Right,

    /// <summary>
    ///   Middle mouse button.
    /// </summary>
    Middle
  );

  /// <summary>
  ///   Indicates the state of the modifier keys,
  ///   the mouse buttons, or the touch devices.
  /// </summary>
  TGIS_PvlShiftStateItem = (

    /// <summary>
    ///   The SHIFT key is held down.
    /// </summary>
    Shift,

    /// <summary>
    ///   The ALT key is held down.
    /// </summary>
    Alt,

    /// <summary>
    ///   The CTRL key is held down.
    /// </summary>
    Ctrl,

    /// <summary>
    ///   The left mouse button is held down.
    /// </summary>
    Left,

    /// <summary>
    ///   The right mouse button is held down.
    /// </summary>
    Right,

    /// <summary>
    ///   The wheel mouse button is held down.
    /// </summary>
    Middle,

    /// <summary>
    ///   The mouse button was double-clicked.
    /// </summary>
    Double,

    /// <summary>
    ///   The user is holding a finger on the touch surface.
    /// </summary>
    Touch,

    /// <summary>
    ///   The pen is touching the surface of a tablet.
    /// </summary>
    Pen,

    /// <summary>
    ///   The CMD key is held down (only on Mac).
    /// </summary>
    Command,

    /// <summary>
    ///   The user is moving a finger horizontally on the touch surface or is
    ///   rolling the mouse wheel to produce a horizontal displacement.
    /// </summary>
    Horizontal
  );

  /// <summary>
  ///   Set of TGIS_PvlShiftStateItem
  /// </summary>
  TGIS_PvlShiftState = set of TGIS_PvlShiftStateItem;

  /// <summary>
  ///   Mouse Event
  /// </summary>
  /// <param name="_sender">
  ///   Sender of the event
  /// </param>
  /// <param name="_button">
  ///   Mouse buttons pressed
  /// </param>
  /// <param name="_shift">
  ///   Shift state items
  /// </param>
  /// <param name="_x">
  ///   X location of the mouse
  /// </param>
  /// <param name="_y">
  ///   Y location of the mouse
  /// </param>
  TGIS_PvlMouseEvent = procedure( _sender : TObject ; _button: TGIS_PvlMouseButton; _shift: TGIS_PvlShiftState; _x, _y: Single ) of object ;

  /// <summary>
  ///   Mouse move event
  /// </summary>
  /// <param name="_sender">
  ///   Sender of the event
  /// </param>
  /// <param name="_shift">
  ///   Shift state items
  /// </param>
  /// <param name="_x">
  ///   X location of the mouse
  /// </param>
  /// <param name="_y">
  ///   Y location of the mouse
  /// </param>
  TGIS_PvlMouseMoveEvent = procedure( _sender : TObject ; _shift: TGIS_PvlShiftState; _x, _y: Single ) of object ;

  /// <summary>
  ///   Mouse wheel event
  /// </summary>
  /// <param name="_sender">
  ///   Sender of the event
  /// </param>
  /// <param name="_shift">
  ///   Shift state items
  /// </param>
  /// <param name="_delta">
  ///   The amount by which the mouse wheel is rotated
  /// </param>
  /// <param name="_handled">
  ///   Specifies whether the event is handled in the user code
  /// </param>
  TGIS_PvlMouseWheelEvent = procedure( _sender : TObject ; _shift: TGIS_PvlShiftState; _delta: Integer; var _handled: Boolean) of object ;

  /// <summary>
  ///   Layout anchors.
  /// </summary>
  TGIS_PvlAnchor         = (

    /// <summary>
    /// Top anchor.
    /// </summary>
    Top,

    /// <summary>
    /// Left anchor.
    /// </summary>
    Left,

    /// <summary>
    /// Bottom anchor.
    /// </summary>
    Bottom,

    /// <summary>
    /// Right anchor.
    /// </summary>
    Right
  ) ;

  /// <summary>
  ///   Set of TGIS_PvlAnchors.
  /// </summary>
  TGIS_PvlAnchors        = set of TGIS_PvlAnchor ;

  /// <summary>
  ///   Extension of the resource file.
  /// </summary>
  TGIS_PvlResourceExtension = {$IFDEF JAVA}enum{$ENDIF} (

    /// <summary>
    /// PNG file type.
    /// </summary>
    PNG {$IFDEF JAVA} = '.png' {$ENDIF},

    /// <summary>
    /// JPG file type.
    /// </summary>
    JPG {$IFDEF JAVA} = '.jpg' {$ENDIF},

    /// <summary>
    /// BMP file type.
    /// </summary>
    BMP {$IFDEF JAVA} = '.bmp' {$ENDIF},

    /// <summary>
    /// CGM file type.
    /// </summary>
    CGM {$IFDEF JAVA} = '.cgm' {$ENDIF},

    /// <summary>
    /// SVG file type.
    /// </summary>
    SVG {$IFDEF JAVA} = '.svg' {$ENDIF}
  ) {$IFDEF JAVA}of String{$ENDIF} ;

  /// <summary>
  ///   Layout align.
  /// </summary>
  TGIS_PvlAlign          = (

    /// <summary>
    /// None align.
    /// </summary>
    None,

    /// <summary>
    /// Client align.
    /// </summary>
    Client,

    /// <summary>
    /// Top align.
    /// </summary>
    Top,

    /// <summary>
    /// Left align.
    /// </summary>
    Left,

    /// <summary>
    /// Bottom align.
    /// </summary>
    Bottom,

    /// <summary>
    /// Right align.
    /// </summary>
    Right
  ) ;

  /// <summary>
  ///   Result type of TGIS_PvlInternalFormAbstract.
  /// </summary>
  TGIS_PvlModalResult    = {$IFDEF JAVA}enum{$ENDIF} (

    /// <summary>
    ///   None result of the ModalForm.
    /// </summary>
    None,

    /// <summary>
    ///   OK result of the ModalForm.
    /// </summary>
    OK,

    /// <summary>
    ///   Cancel result of the ModalForm.
    /// </summary>
    Cancel,

    /// <summary>
    ///   Abort result of the ModalForm.
    /// </summary>
    Abort,

    /// <summary>
    ///   No result of the ModalForm.
    /// </summary>
    No,

    /// <summary>
    ///   Yes result of the ModalForm.
    /// </summary>
    Yes
  ) {$IFDEF JAVA}of Integer{$ENDIF} ;

  /// <summary>
  ///   Item placement within Combo Box
  /// </summary>
  TGIS_PvlComboBoxHelperPosition = (
    /// <summary>
    ///   item to be placed at the top (for standard predefined values)
    /// </summary>
    Top,
    /// <summary>
    ///   item should be placed within LRU zone (below standard top items)
    /// </summary>
    Lru,
    /// <summary>
    ///   item (only one at a time) to be place above bottom items)
    /// </summary>
    Custom,
    /// <summary>
    ///   item should be placed at the bottom
    /// </summary>
    Bottom ) {$IFDEF JAVA}of Integer{$ENDIF} ;

  /// <summary>
  ///   Type of the custom bitmap combobox.
  /// </summary>
  TGIS_PvlCustomBitmapType = (
    /// <summary>
    ///   Fill style.
    /// </summary>
    Fill = 0,
    /// <summary>
    ///   Marker style.
    /// </summary>
    Marker = 1,
    /// <summary>
    ///   Outline style.
    /// </summary>
    Outline = 2,
    /// <summary>
    ///   Shield style.
    /// </summary>
    Shield = 3
  ) {$IFDEF JAVA}of Integer{$ENDIF} ;

  /// <summary>
  ///   Type of items on the TGIS_PvlItemList.
  /// </summary>
  TGIS_PvlItemListType   = (

    /// <summary>
    ///   Text type.
    /// </summary>
    Text,

    /// <summary>
    ///   SVG type.
    /// </summary>
    SVG
  ) ;

  /// <summary>
  ///   Alignment of the text in the label.
  /// </summary>
  TGIS_PvlLabelTextAlignment = (
    /// <summary>
    ///   Left alignment.
    /// </summary>
    Left = 0,
    /// <summary>
    ///   Center alignment.
    /// </summary>
    Center = 1,
    /// <summary>
    ///   Right alignment.
    /// </summary>
    Right = 2
  ) {$IFDEF JAVA}of Integer{$ENDIF} ;


  /// <summary>
  ///   Reference to function which is called upon ModalForm execute.
  /// </summary>
  TGIS_Proc =
    {$IFDEF GENPDK}
      procedure( _value : TGIS_PvlModalResult ) of object
    {$ELSE}
      {$IFDEF DCC}
        TProc<TGIS_PvlModalResult>
      {$ENDIF}
      {$IFDEF CLR}
        Action<TGIS_PvlModalResult>
      {$ENDIF}
      {$IFDEF JAVA}
       java.util.function.Consumer<TGIS_PvlModalResult>
      {$ENDIF}
    {$ENDIF}
   ;

  TGIS_PvlContext        = class ;
  TGIS_PvlPages          = class ;
  TGIS_PvlModalButton    = class ;
  TGIS_PvlControl        = class ;
  TGIS_PvlButton         = class ;
  TGIS_PvlRadioButton    = class ;
  TGIS_PvlTree           = class ;
  TGIS_PvlTreeNode       = class ;
  TGIS_PvlBase           = class ;

  /// <summary>
  /// List of PVL elements;
  /// </summary>
  {$IFNDEF GEN_WRAPPER}
    {$IFNDEF GIS_NOGENERICS}
      TGIS_PvlListOfElements = {$IFDEF OXYGENE} public {$ENDIF}
                           TList< TGIS_PvlBase > ;
    {$ELSE}
      TGIS_PvlListOfElements = class (
                             TList< TGIS_PvlBase >
                           ) ;
    {$ENDIF}
  {$ELSE}
    {#typehint:list:TGIS_PvlBase}
    TGIS_PvlListOfElements = class
    public
      /// <summary>
      ///   Clear the list.
      /// </summary>
      procedure Clear() ;

      /// <summary>
      ///   Add a new element to the list.
      /// </summary>
      /// <param name="_item">
      ///   element to be added.
      /// </param>
      procedure Add( const _item : TGIS_PvlBase ) ;

      /// <summary>
      ///   Delete and item for the list.
      /// </summary>
      /// <param name="_index">
      ///   index of the item to be deleted
      /// </param>
      procedure Delete( const _index : Integer ) ;

      /// <summary>
      ///   Number of elements on the list.
      /// </summary>
      property Count :  Integer read _dummy ;

      /// <summary>
      ///   Get item form the list.
      /// </summary>
      /// <param name="_index">
      ///   index of the item to be retrieved.
      /// </param>
      property Item[ const _index : Integer ] : TGIS_PvlBase read _dummy ;
    end;
  {$ENDIF}


  /// <summary>
  ///   Event argument for TGIS_ComboBoxHelperGetBitmapEvent.
  /// </summary>
  TGIS_PvlComboBoxHelperGetBitmapEventArgs = class
    private
      FValue      : String  ;
      FForeColor  : TGIS_Color   ;
      FFont       : TGIS_Font    ;
      FWidth      : Integer ;
      FHeight     : Integer ;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_value">
      ///   event identifier
      /// </param>
      /// <param name="_forecolor">
      ///   foreground color
      /// </param>
      /// <param name="_font">
      ///   font to be used
      /// </param>
      /// <param name="_width">
      ///  set bitmap width
      /// </param>
      /// <param name="_height">
      ///  set bitmap height
      /// </param>
      constructor Create( _value     : String  ;
                          _forecolor : TGIS_Color   ;
                          _font      : TGIS_Font    ;
                          _width     : Integer ;
                          _height    : Integer
                        ) ;
      /// <summary>
      ///  custom event identifier
      /// </summary>
      property Value      : String  read FValue ;
      /// <summary>
      ///  same as JComboBox.DrawItem
      /// </summary>
      property Font       : TGIS_Font    read FFont ;
      /// <summary>
      ///  same as JComboBox.DrawItem
      /// </summary>
      property ForeColor  : TGIS_Color   read FForeColor ;
      /// <summary>
      ///  bitmap width
      /// </summary>
      property Width      : Integer read FWidth ;
      /// <summary>
      ///  bitmap height
      /// </summary>
      property Height     : Integer read FHeight ;
  end ;

  /// <summary>
  /// Event to be fired upon selecting a bitmap.
  /// </summary>
  /// <param name="_sender">
  ///  sender object
  /// </param>
  /// <param name="_e">
  ///  Event argument for TGIS_ComboBoxHelperGetBitmapEvent.
  /// </param>
  /// <returns>
  ///  Bitmap.
  /// </returns>
  TGIS_PvlComboBoxHelperGetBitmapEvent = function(
    _sender : TObject ;
    _e      : TGIS_PvlComboBoxHelperGetBitmapEventArgs
  ) : TGIS_Bitmap of object ;


  /// <summary>
  ///   PVL Base control interface.
  /// </summary>
  IGIS_PvlBase = interface
    {$IFDEF DCC}
      ['{FDEDE3CD-7C37-4431-A675-6915FFE12492}']
    {$ENDIF}

    /// <summary>
    ///   Get native control
    /// </summary>
    /// <returns>
    ///   True if component is styled
    /// </returns>
    function  fget_NativeControl: TObject ;
  end;

  /// <summary>
  ///   PVL Obbject interface.
  /// </summary>
  IGIS_PvlObject = interface( IGIS_PvlBase )
    {$IFDEF DCC}
      ['{60DE22B9-DB82-4ECF-A3E0-AAD1150D5629}']
    {$ENDIF}
  end;

  /// <summary>
  ///   PVL Control interface.
  /// </summary>
  IGIS_PvlControl = interface( IGIS_PvlObject )
    {$IFDEF DCC}
      ['{8636BA6D-3B72-4AA9-8FB3-E68B994B09BE}']
    {$ENDIF}

    // IGIS_Control events access routines

    /// <summary>
    ///   Getter for OnClick property.
    /// </summary>
    /// <returns>
    ///   Current OnClick content .
    /// </returns>
    function  fget_OnClick      : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnClick property.
    /// </summary>
    /// <param name="_value">
    ///   New OnClick content.
    /// </param>
    procedure fset_OnClick          ( const _value      : TGIS_PvlEvent
                                    );

    /// <summary>
    ///   Getter for OnDblClick property.
    /// </summary>
    /// <returns>
    ///   Current OnDblClick content .
    /// </returns>
    function  fget_OnDblClick       :  TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnDblClick property.
    /// </summary>
    /// <param name="_value">
    ///   New OnDblClick content.
    /// </param>
    procedure fset_OnDblClick       ( const _value      : TGIS_PvlEvent
                                    );

    /// <summary>
    ///   Getter for OnMouseDown property.
    /// </summary>
    /// <returns>
    ///   Current OnMouseDown content .
    /// </returns>
    function  fget_OnMouseDown      :  TGIS_PvlMouseEvent;

    /// <summary>
    ///   Setter for OnMouseDown property.
    /// </summary>
    /// <param name="_value">
    ///   New OnMouseDown content.
    /// </param>
    procedure fset_OnMouseDown      ( const _value      : TGIS_PvlMouseEvent
                                    );

    /// <summary>
    ///   Getter for OnMouseMove property.
    /// </summary>
    /// <returns>
    ///   Current OnMouseMove content .
    /// </returns>
    function  fget_OnMouseMove      : TGIS_PvlMouseMoveEvent;

    /// <summary>
    ///   Setter for OnMouseMove property.
    /// </summary>
    /// <param name="_value">
    ///   New OnMouseMove content.
    /// </param>
    procedure fset_OnMouseMove      ( const _value      : TGIS_PvlMouseMoveEvent
                                    );

    /// <summary>
    ///   Getter for OnMouseUp property.
    /// </summary>
    /// <returns>
    ///   Current OnMouseUp content .
    /// </returns>
    function  fget_OnMouseUp        : TGIS_PvlMouseEvent;

    /// <summary>
    ///   Setter for OnMouseUp property.
    /// </summary>
    /// <param name="_value">
    ///   New OnMouseUp content.
    /// </param>
    procedure fset_OnMouseUp        ( const _value      : TGIS_PvlMouseEvent
                                    );

    /// <summary>
    ///   Getter for OnMouseWheel property.
    /// </summary>
    /// <returns>
    ///   Current OnMouseWheel content .
    /// </returns>
    function  fget_OnMouseWheel     : TGIS_PvlMouseWheelEvent;

    /// <summary>
    ///   Setter for OnMouseWheel property.
    /// </summary>
    /// <param name="_value">
    ///   New OnMouseWheel content.
    /// </param>
    procedure fset_OnMouseWheel     ( const _value      : TGIS_PvlMouseWheelEvent
                                    );

    /// <summary>
    ///   Getter for OnMouseEnter property.
    /// </summary>
    /// <returns>
    ///   Current OnMouseEnter content .
    /// </returns>
    function  fget_OnMouseEnter     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnMouseEnter property.
    /// </summary>
    /// <param name="_value">
    ///   New OnMouseEnter content.
    /// </param>
    procedure fset_OnMouseEnter     ( const _value      : TGIS_PvlEvent
                                    );

    /// <summary>
    ///   Getter for OnMouseLeave property.
    /// </summary>
    /// <returns>
    ///   Current OnMouseLeave content .
    /// </returns>
    function  fget_OnMouseLeave     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnMouseLeave property.
    /// </summary>
    /// <param name="_value">
    ///   New OnMouseLeave content.
    /// </param>
    procedure fset_OnMouseLeave     ( const _value      : TGIS_PvlEvent
                                    );

    /// <summary>
    ///   Setter for Context property.
    /// </summary>
    /// <param name="_value">
    ///   New Context content.
    /// </param>
    procedure fset_Context          ( const _value      : TGIS_PvlContext
                                    ) ;

    /// <summary>
    ///   Does component use styles for colors?
    /// </summary>
    /// <returns>
    ///   True if component is styled
    /// </returns>
    function  fget_IsStyled     : Boolean ;

    /// <summary>
    ///   Getter for Anchors property.
    /// </summary>
    /// <returns>
    ///   Anchors represented by TGIS_PvlAnchors
    /// </returns>
    function  fget_Anchors      : TGIS_PvlAnchors;

    /// <summary>
    ///   Setter for Anchors property.
    /// </summary>
    /// <param name="_value">
    ///   New anchors represented by TGIS_PvlAnchors
    /// </param>
    procedure fset_Anchors      ( const _value      : TGIS_PvlAnchors
                                );

    /// <summary>
    ///   Getter for Align property.
    /// </summary>
    /// <returns>
    ///   Alignment represented by TGIS_PvlAlign
    /// </returns>
    function  fget_Align        : TGIS_PvlAlign;

    /// <summary>
    ///   Setter for Align property.
    /// </summary>
    /// <param name="_value">
    ///   New alignment represented by TGIS_PvlAlign
    /// </param>
    procedure fset_Align        ( const _value      : TGIS_PvlAlign
                                );

    /// <summary>
    ///   Getter for Enabled property.
    /// </summary>
    /// <returns>
    ///   True if control is enabled
    /// </returns>
    function  fget_Enabled      : Boolean;

    /// <summary>
    ///   Setter for Enabled property.
    /// </summary>
    /// <param name="_value">
    ///   New value for the Enabled property
    /// </param>
    procedure fset_Enabled      ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for Width property.
    /// </summary>
    /// <returns>
    ///   Width of the control
    /// </returns>
    function  fget_Width        : Integer;

    /// <summary>
    ///   Setter for Width property.
    /// </summary>
    /// <param name="_value">
    ///   New width value
    /// </param>
    procedure fset_Width        ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Height property.
    /// </summary>
    /// <returns>
    ///   Height of the control
    /// </returns>
    function  fget_Height       : Integer;

    /// <summary>
    ///   Setter for Height property.
    /// </summary>
    /// <param name="_value">
    ///   New Height value
    /// </param>
    procedure fset_Height       ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Left property.
    /// </summary>
    /// <returns>
    ///   Left coordinate of the control
    /// </returns>
    function  fget_Left         : Integer;

    /// <summary>
    ///   Setter for Left property.
    /// </summary>
    /// <param name="_value">
    ///   New Left property value
    /// </param>
    procedure fset_Left         ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Top property.
    /// </summary>
    /// <returns>
    ///   Top coordinate of the control
    /// </returns>
    function  fget_Top          : Integer;

    /// <summary>
    ///   Setter for Top property.
    /// </summary>
    /// <param name="_value">
    ///   New Top property value
    /// </param>
    procedure fset_Top          ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for TabOrder property.
    /// </summary>
    /// <returns>
    ///   Index of TabOrder for the control
    /// </returns>
    function  fget_TabOrder     : Integer;

    /// <summary>
    ///   Setter for TabOrder property.
    /// </summary>
    /// <param name="_value">
    ///   New index of TabOrder for the control
    /// </param>
    procedure fset_TabOrder     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Visible property.
    /// </summary>
    /// <returns>
    ///   True if control is visible
    /// </returns>
    function  fget_Visible      : Boolean;

    /// <summary>
    ///   Setter for Visible property.
    /// </summary>
    /// <param name="_value">
    ///   New Visible value
    /// </param>
    procedure fset_Visible      ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for Hint property.
    /// </summary>
    /// <returns>
    ///   Hint contnent.
    /// </returns>
    function  fget_Hint         : String;

    /// <summary>
    ///   Setter for Hint property.
    /// </summary>
    /// <param name="_value">
    ///   New Hint value.
    /// </param>
    procedure fset_Hint         ( const _value      : String
                                );

    /// <summary>
    ///   Put focus on the control.
    /// </summary>
    procedure SetFocus;

    /// <summary>
    ///   Should be called by parent to enforce any specific Style updated or
    ///   PPI change
    /// </summary>
    procedure DoRedraw;

    /// <summary>
    ///   Interrupts the execution of an application so that it can process the message queue.
    /// </summary>
    procedure ProcessMessages;
  end;

  /// <summary>
  ///   PVL Label interface.
  /// </summary>
  IGIS_PvlLabel = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{0844DFD4-5BDD-4D07-96C4-7D64B6876D2B}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Caption property.
    /// </summary>
    /// <returns>
    ///   Caption of label
    /// </returns>
    function  fget_Caption      : String;

    /// <summary>
    ///   Setter for Caption property.
    /// </summary>
    /// <param name="_value">
    ///   New Caption value
    /// </param>
    procedure fset_Caption      ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FocusControl.
    /// </summary>
    /// <returns>
    ///   Control to be focused
    /// </returns>
    function  fget_FocusControl : TGIS_PvlControl;

    /// <summary>
    ///   Setter for FocusControl.
    /// </summary>
    /// <param name="_value">
    ///   New Control to be focused
    /// </param>
    procedure fset_FocusControl ( const _value      : TGIS_PvlControl
                                );
    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FontColor property.
    /// </summary>
    /// <returns>
    ///   Color of the font color
    /// </returns>
    function  fget_FontColor    : TGIS_Color;

    /// <summary>
    ///   Setter for FontColor property.
    /// </summary>
    /// <param name="_value">
    ///   New FontColor name value
    /// </param>
    procedure fset_FontColor    ( const _value      : TGIS_Color
                                );

    /// <summary>
    ///   Getter for Centered property.
    /// </summary>
    /// <returns>
    ///   True if centered
    /// </returns>
    function  fget_Alignment    : TGIS_PvlLabelTextAlignment;

    /// <summary>
    ///   Setter for Centered property.
    /// </summary>
    /// <param name="_value">
    ///   New Centered value
    /// </param>
    procedure fset_Alignment    ( const _value      : TGIS_PvlLabelTextAlignment
                                );
  end;

  /// <summary>
  ///   PVL Icon Button interface.
  /// </summary>
  IGIS_PvlIconButton = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{5DB0F254-6573-4D09-A186-2693EC493E3F}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Pushed property.
    /// </summary>
    /// <returns>
    ///   Current Pushed content .
    /// </returns>
    function  fget_Pushed       : Boolean;

    /// <summary>
    ///   Setter for Pushed property.
    /// </summary>
    /// <param name="_value">
    ///   New Pushed content.
    /// </param>
    procedure fset_Pushed       ( const _value      : Boolean
                                );
  end ;

  /// <summary>
  ///   PVL Button interface.
  /// </summary>
  IGIS_PvlButton = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{4D940540-4302-4AB7-8ED9-4E4F0FF469AD}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Caption property.
    /// </summary>
    /// <returns>
    ///   Button caption
    /// </returns>
    function  fget_Caption      : String;

    /// <summary>
    ///   Setter for Caption property.
    /// </summary>
    /// <param name="_value">
    ///   New caption value
    /// </param>
    procedure fset_Caption      ( const _value      : String
                                );

    /// <summary>
    ///   Getter for Default property.
    /// </summary>
    /// <returns>
    ///   True if the button is a default control
    /// </returns>
    function  fget_Default      : Boolean;

    /// <summary>
    ///   Setter for Default property.
    /// </summary>
    /// <param name="_value">
    ///   New property value
    /// </param>
    procedure fset_Default      ( const _value      : Boolean
                                );
  end ;

  /// <summary>
  ///   PVL Modal Button interface.
  /// </summary>
  IGIS_PvlModalButton = interface( IGIS_PvlButton )
    {$IFDEF DCC}
      ['{305F8F2D-7682-4424-A89A-CADFE0F68618}']
    {$ENDIF}
  end ;

  /// <summary>
  ///   PVL Edit interface.
  /// </summary>
  IGIS_PvlEdit = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{9F1DF359-4802-4F6C-9E4B-08370FE47DAB}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Text property.
    /// </summary>
    /// <returns>
    ///   Text in the EditBox
    /// </returns>
    function  fget_Text         : String;

    /// <summary>
    ///   Setter for Text property.
    /// </summary>
    /// <param name="_value">
    ///   New text to be placed in the EditBox
    /// </param>
    procedure fset_Text         ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );

    /// <summary>
    ///   Getter for Editable property.
    /// </summary>
    /// <returns>
    ///   True if Memo is editable.
    /// </returns>
    function  fget_Editable     : Boolean;

    /// <summary>
    ///   Setter for Editable property.
    /// </summary>
    /// <param name="_value">
    ///   True if Memo should be editable.
    /// </param>
    procedure fset_Editable     ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for SelectionStart property.
    /// </summary>
    /// <returns>
    ///   Index of the character in String to begin selection with
    /// </returns>
    function  fget_SelectionStart
                                : Integer;

    /// <summary>
    ///   Setter for SelectionStart property.
    /// </summary>
    /// <param name="_value">
    ///   New Index of the character in String to begin selection with
    /// </param>
    procedure fset_SelectionStart(
                                  const _value      : Integer
                                );

    /// <summary>
    ///   Getter for SelectionLength property.
    /// </summary>
    /// <returns>
    ///   Length of the selected text
    /// </returns>
    function  fget_SelectionLength
                                : Integer;

    /// <summary>
    ///   Setter for SelectionLength property.
    /// </summary>
    /// <param name="_value">
    ///   New length of the selection
    /// </param>
    procedure fset_SelectionLength(
                                  const _value      : Integer
                                );
    /// <summary>
    ///   Getter for SelectedtText property.
    /// </summary>
    /// <returns>
    ///   Selected text.
    /// </returns>
    function  fget_SelectedText : String;

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Getter for OnKeyDown property.
    /// </summary>
    /// <returns>
    ///   Current OnKeyDown content.
    /// </returns>
    function  fget_OnKeyDown    : TGIS_PvlKeyEvent;

    /// <summary>
    ///   Setter for OnKeyDown property.
    /// </summary>
    /// <param name="_value">
    ///   New OnKeyDown content.
    /// </param>
    procedure fset_OnKeyDown    ( const _value      : TGIS_PvlKeyEvent
                                );

    /// <summary>
    ///   Getter for OnKeyPress property.
    /// </summary>
    /// <returns>
    ///   Current OnKeyPress content .
    /// </returns>
    function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

    /// <summary>
    ///   Setter for OnKeyPress property.
    /// </summary>
    /// <param name="_value">
    ///   New OnKeyPress content.
    /// </param>
    procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                );

    /// <summary>
    ///   Set control style to hyperlink.
    /// </summary>
    procedure SetAsHyperlink      ;

    /// <summary>
    ///   Set font style when validation process fails.
    /// </summary>
    procedure SetFontAlarm      ;

    /// <summary>
    ///   Set default font style.
    /// </summary>
    procedure SetFontDefault    ;

    /// <summary>
    ///   Set background style when validation process fails.
    /// </summary>
    procedure SetBackgroundAlarm      ;

    /// <summary>
    ///   Set default background style.
    /// </summary>
    procedure SetBackgroundDefault    ;
  end ;

  /// <summary>
  ///   PVL Memo interface.
  /// </summary>
  IGIS_PvlMemo = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{A7815E20-4223-481B-9277-D088CB65D76A}']
    {$ENDIF}

    /// <summary>
    ///   Getter for CursorPos property.
    /// </summary>
    /// <returns>
    ///   Cursor position within EditBox.
    /// </returns>
    function  fget_CursorPos    : TPoint;

    /// <summary>
    ///   Setter for CursorPos property.
    /// </summary>
    /// <param name="_value">
    ///   New cursor position within EditBox.
    /// </param>
    procedure fset_CursorPos    ( const _value      : TPoint
                                );

    /// <summary>
    ///   Getter for Text property.
    /// </summary>
    /// <returns>
    ///   Text in the EditBox
    /// </returns>
    function  fget_Text         : String;

    /// <summary>
    ///   Setter for Text property.
    /// </summary>
    /// <param name="_value">
    ///   New text to be placed in the EditBox
    /// </param>
    procedure fset_Text         ( const _value      : String
                                );
    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );

    /// <summary>
    ///   Getter for SelectionStart property.
    /// </summary>
    /// <returns>
    ///   Index of the character in String to begin selection with
    /// </returns>
    function  fget_SelectionStart
                                : Integer;

    /// <summary>
    ///   Setter for SelectionStart property.
    /// </summary>
    /// <param name="_value">
    ///   New Index of the character in String to begin selection with
    /// </param>
    procedure fset_SelectionStart(
                                  const _value      : Integer
                                );

    /// <summary>
    ///   Getter for SelectionLength property.
    /// </summary>
    /// <returns>
    ///   Length of the selected text
    /// </returns>
    function  fget_SelectionLength
                                : Integer;

    /// <summary>
    ///   Setter for SelectionLength property.
    /// </summary>
    /// <param name="_value">
    ///   New length of the selection
    /// </param>
    procedure fset_SelectionLength(
                                  const _value      : Integer
                                );

    /// <summary>
    ///   Getter for SelectedtText property.
    /// </summary>
    /// <returns>
    ///   Selected text.
    /// </returns>
    function  fget_SelectedText : String;

    /// <summary>
    ///   Getter for WordWrap property.
    /// </summary>
    /// <returns>
    ///   True if Memo is word-wrapped.
    /// </returns>
    function  fget_WordWrap     : Boolean;

    /// <summary>
    ///   Setter for WordWrap property.
    /// </summary>
    /// <param name="_value">
    ///   True if Memo should be word-wrapped.
    /// </param>
    procedure fset_WordWrap     ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for Editable property.
    /// </summary>
    /// <returns>
    ///   True if Memo is editable.
    /// </returns>
    function  fget_Editable     : Boolean;

    /// <summary>
    ///   Setter for Editable property.
    /// </summary>
    /// <param name="_value">
    ///   True if Memo should be editable.
    /// </param>
    procedure fset_Editable     ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Getter for OnKeyDown property.
    /// </summary>
    /// <returns>
    ///   Current OnKeyDown content .
    /// </returns>
    function  fget_OnKeyDown    : TGIS_PvlKeyEvent;

    /// <summary>
    ///   Setter for OnKeyDown property.
    /// </summary>
    /// <param name="_value">
    ///   New OnKeyDown content.
    /// </param>
    procedure fset_OnKeyDown    ( const _value      : TGIS_PvlKeyEvent
                                );

    /// <summary>
    ///   Getter for OnKeyPress property.
    /// </summary>
    /// <returns>
    ///   Current OnKeyPress content.
    /// </returns>
    function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

    /// <summary>
    ///   Setter for OnKeyPress property.
    /// </summary>
    /// <param name="_value">
    ///   New OnKeyPress content.
    /// </param>
    procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                );

    /// <summary>
    ///   Set font style when validation process fails.
    /// </summary>
    procedure SetFontAlarm      ;

    /// <summary>
    ///   Set default font style.
    /// </summary>
    procedure SetFontDefault    ;

    /// <summary>
    ///   Clear memo content.
    /// </summary>
    procedure Clear             ;

    /// <summary>
    ///   Append text to the end of memo content.
    /// </summary>
    /// <param name="_value">
    ///   text to be added
    /// </param>
    procedure AppendText        ( const _value      : String
                                );

    /// <summary>
    ///   Append text to the end of memo content as a new line.
    /// </summary>
    /// <param name="_value">
    ///   text to be added
    /// </param>
    procedure AppendLine        ( const _value      : String
                                );
  end;

  /// <summary>
  ///   PVL Trackbar interface
  /// </summary>
  IGIS_PvlTrackBar = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{8BA3B38F-B73C-4164-9FE5-3153FF9D3FC5}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Minimum property.
    /// </summary>
    /// <returns>
    ///   Minimal value of the trackbar
    /// </returns>
    function  fget_Minimum
                                : Integer;

    /// <summary>
    ///   Setter for Minimum property.
    /// </summary>
    /// <param name="_value">
    ///   New minimal value of the trackbar
    /// </param>
    procedure fset_Minimum(
                                  const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Maximum property.
    /// </summary>
    /// <returns>
    ///   Maximum value of the trackbar
    /// </returns>
    function  fget_Maximum
                                : Integer;

    /// <summary>
    ///   Setter for Maximum property.
    /// </summary>
    /// <param name="_value">
    ///   New maximum value of the trackbar
    /// </param>
    procedure fset_Maximum(
                                  const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Position property.
    /// </summary>
    /// <returns>
    ///   Position value of the trackbar
    /// </returns>
    function  fget_Position
                                : Integer;

    /// <summary>
    ///   Setter for Position property.
    /// </summary>
    /// <param name="_value">
    ///   New position value of the trackbar
    /// </param>
    procedure fset_Position(
                                  const _value      : Integer
                                );

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );
  end;

  /// <summary>
  ///   PVL List Box interface.
  /// </summary>
  IGIS_PvlListBox = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{A7815E20-4223-481B-9277-D088CB65D76A}']
    {$ENDIF}

    /// <summary>
    ///   Getter for ItemList property.
    /// </summary>
    /// <returns>
    ///   List of items
    /// </returns>
    function  fget_ItemList     : TGIS_ListOfStrings;

    /// <summary>
    ///   Getter for the ItemsCount property.
    /// </summary>
    /// <returns>
    ///   Count of items on the list
    /// </returns>
    function  fget_ItemsCount   : Integer;

    /// <summary>
    ///   Get item with given index from the list.
    /// </summary>
    /// <param name="_idx">
    ///   Index of the item to get
    /// </param>
    /// <returns>
    ///   Item with given index
    /// </returns>
    function  fget_Item         ( const _idx        : Integer
                                ) : String;

    /// <summary>
    ///   Getter for ItemIndex property.
    /// </summary>
    /// <returns>
    ///   Index of the current item
    /// </returns>
    function  fget_ItemIndex    : Integer;

    /// <summary>
    ///   Setter for ItemIndex property.
    /// </summary>
    /// <param name="_value">
    ///   New index of the item
    /// </param>
    procedure fset_ItemIndex    ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Multiselect property.
    /// </summary>
    /// <returns>
    ///   Value of multiselect
    /// </returns>
    function  fget_Multiselect  : Boolean ;

    /// <summary>
    ///   Setter for Multiselect property.
    /// </summary>
    /// <param name="_value">
    ///   New value of the multiselect property
    /// </param>
    procedure fset_Multiselect  ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for SelectedItems property.
    /// </summary>
    /// <returns>
    ///   List of all selected items.
    /// </returns>
    function  fget_SelectedItems: TGIS_ListOfStrings ;

    /// <summary>
    ///   Getter for Selected property
    /// </summary>
    /// <param name="_index">
    ///   Index of the item
    /// </param>
    /// <returns>
    ///   True if selected
    /// </returns>
    function  fget_Selected     ( const _index      : Integer
                                ) : Boolean ;

    /// <summary>
    ///   Setter for Selected property.
    /// </summary>
    /// <param name="_index">
    ///   Index of the item
    /// </param>
    /// <param name="_value">
    ///   New value of the selected property on give index
    /// </param>
    procedure fset_Selected     ( const _index      : Integer ;
                                  const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for OnChange event property.
    /// </summary>
    /// <returns>
    ///   OnChange event
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange event property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange value
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Begin updating list.
    /// </summary>
    procedure BeginUpdate       ;

    /// <summary>
    ///   End updating list.
    /// </summary>
    procedure EndUpdate         ;

    /// <summary>
    ///   Clear all items from the list.
    /// </summary>
    procedure ItemsClear        ;

    /// <summary>
    ///   Add item to the list.
    /// </summary>
    /// <param name="_item">
    ///   Item to be added to the list
    /// </param>
    procedure ItemsAdd          ( const _item       : String
                                );
  end;

  /// <summary>
  ///   PVL SVG List interface.
  /// </summary>
  IGIS_PvlSVGList = interface( IGIS_PvlListBox )
    {$IFDEF DCC}
      ['{B8FDC5C0-D01F-4B99-BD74-0185F2202E56}']
    {$ENDIF}
  end;

  /// <summary>
  ///   PVL Color Preview interface.
  /// </summary>
  IGIS_PvlColorPreview = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{A7D081B7-A417-4C2A-BE4F-C9B01CFA0D62}']
    {$ENDIF}

    /// <summary>
    ///   Getter for the Color property
    /// </summary>
    /// <returns>
    ///   Color value of the selected color
    /// </returns>
    function  fget_Color        : TGIS_Color ;

    /// <summary>
    ///   Setter for the Color property
    /// </summary>
    /// <param name="_value">
    ///   New Color value
    /// </param>
    procedure fset_Color        (  const _value     : TGIS_Color
                                ) ;

    /// <summary>
    ///   Getter for Border property.
    /// </summary>
    /// <returns>
    ///   True if border is visible
    /// </returns>
    function  fget_Border       : Boolean;

    /// <summary>
    ///   Setter for Border property.
    /// </summary>
    /// <param name="_value">
    ///   New value for the property
    /// </param>
    procedure fset_Border       ( const _value      : Boolean
                                )  ;

  end;

  /// <summary>
  ///   PVL Color Wheel interface.
  /// </summary>
  IGIS_PvlColorWheel = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{BC60D8A5-BD59-4735-AAD3-3F454EB4DD70}']
    {$ENDIF}

    /// <summary>
    ///   Getter for the Color property
    /// </summary>
    /// <returns>
    ///   Color value of the selected color
    /// </returns>
    function  fget_Color        : TGIS_Color ;

    /// <summary>
    ///   Setter for the Color property
    /// </summary>
    /// <param name="_value">
    ///   New Color value
    /// </param>
    procedure fset_Color        ( const _value      : TGIS_Color
                                ) ;

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Converts Hue to RGB
    /// </summary>
    /// <param name="_hue">
    ///   Hue to be converted
    /// </param>
    /// <returns>
    ///   RGB value
    /// </returns>
    function  HueToRGB          ( const _hue        : Double
                                ) : TGIS_PvlRGBVal ; //? really here???
  end;

  /// <summary>
  ///   PVL Color Bar interface.
  /// </summary>
  IGIS_PvlColorBar = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{29A3E26E-EAA3-43BB-8606-381EBBD9970B}']
    {$ENDIF}

    /// <summary>
    ///   Getter for the Value property
    /// </summary>
    /// <returns>
    ///   Value of the slider
    /// </returns>
    function  fget_Value        : Double ;

    /// <summary>
    ///   Setter for the Value property
    /// </summary>
    /// <param name="_value">
    ///   New value
    /// </param>
    procedure fset_Value        ( const _value      : Double
                                ) ;
    /// <summary>
    ///   Getter for the Color property
    /// </summary>
    /// <returns>
    ///   Color value of the selected color
    /// </returns>
    function  fget_Color       : TGIS_Color ;

    /// <summary>
    ///   Setter for the Color property
    /// </summary>
    /// <param name="_value">
    ///   New Color value
    /// </param>
    procedure fset_Color        ( const _value      : TGIS_Color
                                ) ;

    /// <summary>
    ///   Getter for the Alpha property
    /// </summary>
    /// <returns>
    ///   True if alpha mode is enabled
    /// </returns>
    function  fget_Alpha        : Boolean ;

    /// <summary>
    ///   Setter for the Aplha property
    /// </summary>
    /// <param name="_value">
    ///   New Alpha value
    /// </param>
    procedure fset_Alpha        ( const _value      : Boolean
                                ) ;

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );
  end;

  /// <summary>
  ///   PVL ComboBox interface.
  /// </summary>
  IGIS_PvlComboBox = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{9917290B-7181-425F-8C7E-8183E3F4EF1C}']
    {$ENDIF}

    /// <summary>
    ///   Getter for the ItemsCount property.
    /// </summary>
    /// <returns>
    ///   Count of items on the list
    /// </returns>
    function  fget_ItemsCount   : Integer;

    /// <summary>
    ///   Get item with given index from the list.
    /// </summary>
    /// <param name="_idx">
    ///   Index of the item to get
    /// </param>
    /// <returns>
    ///   Item with given index
    /// </returns>
    function  fget_Item         ( const _idx        : Integer
                                ) : String;

    /// <summary>
    ///   Getter for ItemIndex property.
    /// </summary>
    /// <returns>
    ///   Index of the current item
    /// </returns>
    function  fget_ItemIndex    : Integer;

    /// <summary>
    ///   Setter for ItemIndex property.
    /// </summary>
    /// <param name="_value">
    ///   New index of the item
    /// </param>
    procedure fset_ItemIndex    ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Text property.
    /// </summary>
    /// <returns>
    ///   Text from the ComboBox
    /// </returns>
    function  fget_Text         : String;

    /// <summary>
    ///   Setter for Text property.
    /// </summary>
    /// <param name="_value">
    ///   New value for text in ComboBox
    /// </param>
    procedure fset_Text         ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );

    /// <summary>
    ///   Getter for Text property.
    /// </summary>
    /// <returns>
    ///   Text from the ComboBox
    /// </returns>
    function  fget_Tag          : NativeInt;

    /// <summary>
    ///   Setter for Tag property.
    /// </summary>
    /// <param name="_value">
    ///   New value for Tag
    /// </param>
    procedure fset_Tag          ( const _value      : NativeInt
                                );

    /// <summary>
    ///   Getter for Sorted property.
    /// </summary>
    /// <returns>
    ///   True if sorted
    /// </returns>
    function  fget_Sorted       : Boolean;

    /// <summary>
    ///   Setter for Sorted property.
    /// </summary>
    /// <param name="_value">
    ///   New value for Sorted property
    /// </param>
    procedure fset_Sorted       ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for DropDownCount property.
    /// </summary>
    /// <returns>
    ///   Number of rows showed in the combo popup
    /// </returns>
    function  fget_DropDownCount: Integer;


    /// <summary>
    ///   Setter for DropDownCount property.
    /// </summary>
    /// <param name="_value">
    ///   New number of rows showed in the combo popup
    /// </param>
    procedure fset_DropDownCount( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Begin updating list.
    /// </summary>
    procedure BeginUpdate       ;

    /// <summary>
    ///   End updating list.
    /// </summary>
    procedure EndUpdate         ;

    /// <summary>
    ///   Clear all items from the list.
    /// </summary>
    procedure ItemsClear        ;

    /// <summary>
    ///   Add item to the list.
    /// </summary>
    /// <param name="_item">
    ///   Item to be added to the list
    /// </param>
    procedure ItemsAdd          ( const _item       : String
                                );

    /// <summary>
    ///   Gets index of given item.
    /// </summary>
    /// <param name="_item">
    ///   Given item
    /// </param>
    /// <returns>
    ///   Index on the list
    /// </returns>
    function  IndexOf           ( const _item       : String
                                ) : Integer;
  end;

  /// <summary>
  ///   PVL ComboEdit interface.
  /// </summary>
  IGIS_PvlComboEdit = interface( IGIS_PvlComboBox )
    {$IFDEF DCC}
      ['{F83FC62F-863D-4588-B2FF-C0F68FD4550E}']
    {$ENDIF}

    /// <summary>
    ///   Getter for OnKeyPress property.
    /// </summary>
    /// <returns>
    ///   Current OnKeyPress content .
    /// </returns>
    function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

    /// <summary>
    ///   Setter for OnKeyPress property.
    /// </summary>
    /// <param name="_value">
    ///   New OnKeyPress content.
    /// </param>
    procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                );

    /// <summary>
    ///   Set font style when validation process fails.
    /// </summary>
    procedure SetFontAlarm      ;

    /// <summary>
    ///   Set default font style.
    /// </summary>
    procedure SetFontDefault    ;

    /// <summary>
    ///   Set background style when validation process fails.
    /// </summary>
    procedure SetBackgroundAlarm      ;

    /// <summary>
    ///   Set default background style.
    /// </summary>
    procedure SetBackgroundDefault    ;
  end;

  /// <summary>
  ///   PVL ComboBox interface.
  /// </summary>
  IGIS_PvlSearchBox = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{E5FB5CA3-92B8-4D72-BC4C-B5DD398C1BC4}']
    {$ENDIF}

    /// <summary>
    ///   Getter for the ItemsCount property.
    /// </summary>
    /// <returns>
    ///   Count of items on the list
    /// </returns>
    function  fget_ItemsCount   : Integer;

    /// <summary>
    ///   Get item with given index from the list.
    /// </summary>
    /// <param name="_idx">
    ///   Index of the item to get
    /// </param>
    /// <returns>
    ///   Item with given index
    /// </returns>
    function  fget_Item         ( const _idx        : Integer
                                ) : String;

    /// <summary>
    ///   Getter for ItemIndex property.
    /// </summary>
    /// <returns>
    ///   Index of the current item
    /// </returns>
    function  fget_ItemIndex    : Integer;

    /// <summary>
    ///   Setter for ItemIndex property.
    /// </summary>
    /// <param name="_value">
    ///   New index of the item
    /// </param>
    procedure fset_ItemIndex    ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for Text property.
    /// </summary>
    /// <returns>
    ///   Text from the ComboBox
    /// </returns>
    function  fget_Text         : String;

    /// <summary>
    ///   Setter for Text property.
    /// </summary>
    /// <param name="_value">
    ///   New value for text in ComboBox
    /// </param>
    procedure fset_Text         ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );

    /// <summary>
    ///   Getter for Sorted property.
    /// </summary>
    /// <returns>
    ///   True if sorted
    /// </returns>
    function  fget_Sorted       : Boolean;

    /// <summary>
    ///   Setter for Sorted property.
    /// </summary>
    /// <param name="_value">
    ///   New value for Sorted property
    /// </param>
    procedure fset_Sorted       ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for DropDownCount property.
    /// </summary>
    /// <returns>
    ///   Number of rows showed in the combo popup
    /// </returns>
    function  fget_DropDownCount: Integer;


    /// <summary>
    ///   Setter for DropDownCount property.
    /// </summary>
    /// <param name="_value">
    ///   New number of rows showed in the combo popup
    /// </param>
    procedure fset_DropDownCount( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Clear all items from the list.
    /// </summary>
    procedure ItemsClear        ;

    /// <summary>
    ///   Add item to the list.
    /// </summary>
    /// <param name="_item">
    ///   Item to be added to the list
    /// </param>
    procedure ItemsAdd          ( const _item       : String
                                );

    /// <summary>
    ///   Gets index of given item.
    /// </summary>
    /// <param name="_item">
    ///   Given item
    /// </param>
    /// <returns>
    ///   Index on the list
    /// </returns>
    function  IndexOf           ( const _item       : String
                                ) : Integer;
    /// <summary>
    ///   Getter for OnKeyPress property.
    /// </summary>
    /// <returns>
    ///   Current OnKeyPress content .
    /// </returns>
    function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

    /// <summary>
    ///   Setter for OnKeyPress property.
    /// </summary>
    /// <param name="_value">
    ///   New OnKeyPress content.
    /// </param>
    procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                );

    /// <summary>
    ///   Set font style when validation process fails.
    /// </summary>
    procedure SetFontAlarm      ;

    /// <summary>
    ///   Set default font style.
    /// </summary>
    procedure SetFontDefault    ;

    /// <summary>
    ///   Set background style when validation process fails.
    /// </summary>
    procedure SetBackgroundAlarm      ;

    /// <summary>
    ///   Set default background style.
    /// </summary>
    procedure SetBackgroundDefault    ;
  end;

  /// <summary>
  ///   PVL Custom ComboBox interface.
  /// </summary>
  IGIS_PvlCustomComboBox = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{ED900C1A-F855-467E-95B6-C9E0C51E2875}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Value property.
    /// </summary>
    /// <returns>
    ///   Value of the combobox that should be selected, if its not on the
    ///   list it is then added to it.
    /// </returns>
    function  fget_Value        : String ;

    /// <summary>
    ///   Setter for Value property.
    /// </summary>
    /// <param name="_value">
    ///   New value
    /// </param>
    procedure fset_Value        ( const _value      : String
                                ) ;

    /// <summary>
    ///   Getter for Field property.
    /// </summary>
    /// <returns>
    ///   Fields content.
    /// </returns>
    function  fget_Fields       : TStringList ;

    /// <summary>
    ///   Setter for Fields property.
    /// </summary>
    /// <param name="_value">
    ///   New value
    /// </param>
    procedure fset_Fields       ( const _value      : TStringList
                                ) ;

    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

  end;


  /// <summary>
  ///   PVL Size ComboBox interface.
  /// </summary>
  IGIS_PvlSizeComboBox = interface( IGIS_PvlCustomComboBox )
    {$IFDEF DCC}
      ['{61E6CFFC-9670-4C46-9A9F-9FFB23E40CCF}']
    {$ENDIF}

    /// <summary>
    ///   Fill the list.
    /// </summary>
    /// <param name="_forSymbol">
    ///   if units for symbol size
    /// </param>
    /// <param name="_forLine">
    ///   if units for symbol size
    /// </param>
    /// <param name="_field">
    ///   if values from fields
    /// </param>
    /// <param name="_renderer">
    ///   if renderer value
    /// </param>
    procedure Fill              ( const _forSymbol  : Boolean ;
                                  const _forLine    : Boolean ;
                                  const _field      : Boolean ;
                                  const _renderer   : Boolean
                                ) ;
    /// <summary>
    ///   Fill the list with measure units.
    /// </summary>
    /// <param name="_field">
    ///   if values from fields
    /// </param>
    procedure FillRealWorldUnits( const _field      : Boolean
                                ) ;
    /// <summary>
    ///   Fill the list with aggregation sizes.
    /// </summary>
    procedure FillAggregation   ;

  end;

  /// <summary>
  ///   PVL Color ComboBox interface.
  /// </summary>
  IGIS_PvlColorComboBox = interface( IGIS_PvlCustomComboBox )
    {$IFDEF DCC}
      ['{08DA827D-0705-48CD-91AC-30B3927F6D75}']
    {$ENDIF}

    /// <summary>
    ///   Populates the ComboBox with items.
    /// </summary>
    /// <param name="_field">
    ///   If values from fields
    /// </param>
    /// <param name="_renderer">
    ///   If renderer value
    /// </param>
    procedure Fill              ( const _field      : Boolean ;
                                  const _renderer   : Boolean
                                ) ; overload;

    /// <summary>
    ///   Populates the ComboBox with items from the list.
    ///   Place method has to be used first.
    /// </summary>
    /// <param name="_field">
    ///   If values from fields
    /// </param>
    /// <param name="_renderer">
    ///   If renderer value
    /// </param>
    /// <param name="_values">
    ///   List of colors to fill combobox ( eg. 'FFFFFFFF' ).
    /// </param>
    procedure Fill              ( const _field      : Boolean ;
                                  const _renderer   : Boolean ;
                                  const _values     : TStringList
                                ) ; overload;

  end;

  /// <summary>
  ///   PVL Custom Bitmap ComboBox interface.
  /// </summary>
  IGIS_PvlCustomBitmapComboBox = interface( IGIS_PvlCustomComboBox )
    {$IFDEF DCC}
      ['{E8FA01D8-8F03-4078-8E10-44A6BDE7C21E}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Type property.
    /// </summary>
    /// <returns>
    ///   Type value.
    /// </returns>
    function  fget_Type         : TGIS_PvlCustomBitmapType ;


    /// <summary>
    ///   Getter for BitmapEvent property.
    /// </summary>
    /// <returns>
    ///   Type value.
    /// </returns>
    function  fget_GetBitmapEvent
                                : TGIS_PvlComboBoxHelperGetBitmapEvent;

    /// <summary>
    ///   Setter for BitmapEvent property.
    /// </summary>
    /// <param name="_value">
    ///   New value
    /// </param>
    procedure fset_GetBitmapEvent(
                                  const _value      : TGIS_PvlComboBoxHelperGetBitmapEvent
                                ) ;

    /// <summary>
    ///   Populates the outline style ComboBox with items.
    /// </summary>
    /// <param name="_hasSymbol">
    ///   If symbols component can be invoked
    /// </param>
    procedure FillStyle         ( const _hasSymbol  : Boolean
                                ) ;

    /// <summary>
    ///   Populates the pattern fill ComboBox with items.
    /// </summary>
    /// <param name="_hasSymbol">
    ///   If symbols component can be invoked
    /// </param>
    procedure FillPattern       ( const _hasSymbol  : Boolean
                                ) ;

    /// <summary>
    ///   Populates the marker ComboBox with items.
    /// </summary>
    /// <param name="_hasSymbol">
    ///   If symbols component can be invoked
    /// </param>
    procedure FillMarker        ( const _hasSymbol  : Boolean
                                ) ;

    /// <summary>
    ///   Populates the shield ComboBox with items.
    /// </summary>
    procedure FillShield        ;

  end;

  /// <summary>
  ///   PVL Color Ramp ComboBox interface.
  /// </summary>
  IGIS_PvlColorRampComboBox = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{F5EF9616-8658-4425-9E87-6D0C766570E8}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Mode property.
    /// </summary>
    /// <returns>
    ///   Current Mode.
    /// </returns>
    function  fget_Mode         : TGIS_ColorMapMode;

    /// <summary>
    ///   Setter for Mode property.
    /// </summary>
    /// <param name="_value">
    ///   New Mode value
    /// </param>
    procedure fset_Mode         ( const _value      : TGIS_ColorMapMode
                                );

    /// <summary>
    ///   Setter for index property.
    /// </summary>
    /// <param name="_value">
    ///   New index value.
    /// </param>
    procedure fset_Index        ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for index property.
    /// </summary>
    /// <returns>
    ///   Current index.
    /// </returns>
    function  fget_Index        : Integer;

    /// <summary>
    ///   Get item with given index from the list.
    /// </summary>
    /// <param name="_idx">
    ///   Index of the item to get
    /// </param>
    /// <returns>
    ///   Item with given index
    /// </returns>
    function  fget_Item         ( const _idx        : Integer
                                ) : String;

    /// <summary>
    ///   Getter for ItemCount property.
    /// </summary>
    /// <returns>
    ///   Count of the items
    /// </returns>
    function  fget_ItemCount    : Integer;

    /// <summary>
    ///   Setter for ColorSchemas property.
    /// </summary>
    /// <returns>
    ///   Current ColorSchemas value.
    /// </returns>
    function fget_ColorSchemas  : TGIS_ColorSchemas;

    /// <summary>
    ///   Setter for ColorSchemas property.
    /// </summary>
    /// <param name="_value">
    ///   New ColorSchemas value
    /// </param>
    procedure fset_ColorSchemas ( const _value      : TGIS_ColorSchemas
                                );

    /// <summary>
    ///   Getter for Reverse property.
    /// </summary>
    /// <returns>
    ///   Current Reverse value.
    /// </returns>
    function  fget_Reverse     : Boolean ;

    /// <summary>
    ///   Setter for Reverse property.
    /// </summary>
    /// <param name="_value">
    ///   New Reverse value
    /// </param>
    procedure fset_Reverse      ( const _value      : Boolean
                                );

    /// <summary>
    ///   Geetter for ShowNames property.
    /// </summary>
    /// <returns>
    ///   Current ShowNames value.
    /// </returns>
    function  fget_ShowNames    : Boolean ;

    /// <summary>
    ///   Setter for ShowNames property.
    /// </summary>
    /// <param name="_value">
    ///   New ShowNames value
    /// </param>
    procedure fset_ShowNames    ( const _value      : Boolean
                                );


    /// <summary>
    ///   Getter for OnChange property.
    /// </summary>
    /// <returns>
    ///   Current OnChange content .
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange content.
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Color map value.
    /// </summary>
    /// <param name="_subClass">
    ///   get subclass of a ramp with specified number of colors, if available;
    ///   if 0, get default colormap;
    ///   if -1, get subclass with maximum number of colors;
    /// </param>
    /// <returns>
    ///   Array of colormap
    /// </returns>
    function Value              ( const _subClass   : Integer = -1
                                ) : TGIS_ColorMapArray;


    /// <summary>
    ///   Fill the control with data.
    /// </summary>
    procedure Fill              ;

    /// <summary>
    ///   Locks control from filling its values.
    /// </summary>
    procedure Lock              ;

    /// <summary>
    ///   Unlocks control from filling its values.
    /// </summary>
    procedure Unlock            ;
  end;

  /// <summary>
  ///   PVL CheckBox interface.
  /// </summary>
  IGIS_PvlCheckBox = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{753A4764-804A-4001-94BB-DF7866BCF198}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Checked property.
    /// </summary>
    /// <returns>
    ///   True if checked
    /// </returns>
    function  fget_Checked      : Boolean;

    /// <summary>
    ///   Setter for Checked property.
    /// </summary>
    /// <param name="_value">
    ///   New checked value
    /// </param>
    procedure fset_Checked      ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for OnChange event property.
    /// </summary>
    /// <returns>
    ///   OnChange event
    /// </returns>
    function  fget_OnChange     : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnChange event property.
    /// </summary>
    /// <param name="_value">
    ///   New OnChange value
    /// </param>
    procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Getter for Caption property.
    /// </summary>
    /// <returns>
    ///   Caption of the CheckBox
    /// </returns>
    function  fget_Caption      : String;

    /// <summary>
    ///   Setter for Caption property.
    /// </summary>
    /// <param name="_value">
    ///   New caption value
    /// </param>
    procedure fset_Caption      ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value     : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );
  end;

  /// <summary>
  ///   PVL Radio Button interface.
  /// </summary>
  IGIS_PvlRadioButton = interface( IGIS_PvlCheckBox )
    {$IFDEF DCC}
      ['{0CF39EB6-2322-4588-A838-008C41149481}']
    {$ENDIF}
    /// <summary>
    ///   Getter for Group property.
    /// </summary>
    /// <returns>
    ///   Group of the radio button
    /// </returns>
    function  fget_Group        : String;

    /// <summary>
    ///   Setter for Group property.
    /// </summary>
    /// <param name="_value">
    ///   New Group value
    /// </param>
    procedure fset_Group        ( const _value      : String
                                );

    /// <summary>
    ///   Un cjchek all memmebers of the same group as provideded button.
    /// </summary>
    /// <param name="_button">
    ///   reference radio button
    /// </param>
    procedure UncheckGroup      ( const _button     : TGIS_PvlRadioButton
                                ) ;
  end;

  /// <summary>
  ///   PVL Panel intraface
  /// </summary>
  IGIS_PvlPanel = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{3A1737FA-C05E-43E0-BA07-3F5B542CFFDE}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Scrollable property.
    /// </summary>
    /// <returns>
    ///   True if control is scrollable
    /// </returns>
    function  fget_Scrollable   : Boolean;

    /// <summary>
    ///   Setter for Scrollable property.
    /// </summary>
    /// <param name="_value">
    ///   New value for the property
    /// </param>
    procedure fset_Scrollable   ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for Border property.
    /// </summary>
    /// <returns>
    ///   True if border is visible
    /// </returns>
    function  fget_Border       : Boolean;

    /// <summary>
    ///   Setter for Border property.
    /// </summary>
    /// <param name="_value">
    ///   New value for the property
    /// </param>
    procedure fset_Border       ( const _value      : Boolean
                                );

    /// <summary>
    ///   Remove all components from the panel.
    /// </summary>
    procedure RemoveAllComponents;

  end;

  /// <summary>
  ///   PVL Canvas interface.
  /// </summary>
  IGIS_PvlCanvas = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{1FC29EF7-774F-4687-A04A-0BEEA6F5387F}']
    {$ENDIF}

    /// <summary>
    ///   Getter for OnRender property.
    /// </summary>
    /// <returns>
    ///   Current OnRender content .
    /// </returns>
    function  fget_OnRender     : TGIS_PvlRendererEvent;

    /// <summary>
    ///   Setter for OnRender property.
    /// </summary>
    /// <param name="_value">
    ///   New OnRender content.
    /// </param>
    procedure fset_OnRender     ( const _value      : TGIS_PvlRendererEvent
                                );
  end;

  /// <summary>
  ///   PVL Preview Panel interface.
  /// </summary>
  IGIS_PvlPreviewPanel = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{6E04BD21-F423-42B8-928A-6B1C02C4020E}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Caption property.
    /// </summary>
    /// <returns>
    ///   Caption of the control
    /// </returns>
    function  fget_Caption      : String;

    /// <summary>
    ///   Setter for Caption property.
    /// </summary>
    /// <param name="_value">
    ///   New Caption value
    /// </param>
    procedure fset_Caption      ( const _value      : String
                                );

    /// <summary>
    ///   Getter for FontSize property.
    /// </summary>
    /// <returns>
    ///   Size of the font used for the Caption
    /// </returns>
    function  fget_FontSize     : Integer;

    /// <summary>
    ///   Setter for FontSize property.
    /// </summary>
    /// <param name="_value">
    ///   New FontSize value
    /// </param>
    procedure fset_FontSize     ( const _value      : Integer
                                );

    /// <summary>
    ///   Getter for FontStyle property.
    /// </summary>
    /// <returns>
    ///   Set of TGIS_FontStyle
    /// </returns>
    function  fget_FontStyle    : TGIS_FontStyles;

    /// <summary>
    ///   Setter for FontStyle property.
    /// </summary>
    /// <param name="_value">
    ///   New set of TGIS_FontStyle
    /// </param>
    procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                );

    /// <summary>
    ///   Getter for FamilyFont property.
    /// </summary>
    /// <returns>
    ///   Name of the font family
    /// </returns>
    function  fget_FontFamily   : String;

    /// <summary>
    ///   Setter for FamilyFont property.
    /// </summary>
    /// <param name="_value">
    ///   New FontFamily name value
    /// </param>
    procedure fset_FontFamily   ( const _value      : String
                                );

    /// <summary>
    ///   Getter for Border property.
    /// </summary>
    /// <returns>
    ///   True if border is visible
    /// </returns>
    function  fget_Border       : Boolean;

    /// <summary>
    ///   Setter for Border property.
    /// </summary>
    /// <param name="_value">
    ///   New value for the property
    /// </param>
    procedure fset_Border       ( const _value      : Boolean
                                );

    /// <summary>
    ///   Getter for Bitmap property.
    /// </summary>
    /// <returns>
    ///   Object that need to be cast to platform native Bitmap
    /// </returns>
    function  fget_Bitmap       : TGIS_Bitmap;

    /// <summary>
    ///   Setter for Bitmap property.
    /// </summary>
    /// <param name="_value">
    ///   New value for the image
    /// </param>
    procedure fset_Bitmap       ( const _value      : TGIS_Bitmap
                                );

    /// <summary>
    ///   Getter for Color property.
    /// </summary>
    /// <returns>
    ///   Platform independent color
    /// </returns>
    function  fget_Color        : TGIS_Color;

    /// <summary>
    ///   Setter for Color property.
    /// </summary>
    /// <param name="_value">
    ///   New TGIS_Color value
    /// </param>
    procedure fset_Color        ( const _value      : TGIS_Color
                                );

    /// <summary>
    ///   Getter for Color property.
    /// </summary>
    /// <returns>
    ///   Platform independent color
    /// </returns>
    function  fget_StyledAreaColor
                                : TGIS_Color;
    /// <summary>
    ///   Invalidate preview panel.
    /// </summary>
    procedure Invalidate        ;

  end;

  /// <summary>
  ///   PVL Tree interface.
  /// </summary>
  IGIS_PvlTree = interface( IGIS_PvlControl )
    {$IFDEF DCC}
      ['{F34DB713-560C-453C-BBF8-23DB78BA34E9}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Root property.
    /// </summary>
    /// <returns>
    ///   Root property value
    /// </returns>
    function  fget_Root         : TGIS_PvlTreeNode;

    /// <summary>
    ///   Getter for Selected property.
    /// </summary>
    /// <returns>
    ///   Selected property value
    /// </returns>
    function  fget_Selected     : TGIS_PvlTreeNode;

    /// <summary>
    ///   Setter for Selected property.
    /// </summary>
    /// <param name="_value">
    ///   New Selected property value
    /// </param>
    procedure fset_Selected     ( const _value      : TGIS_PvlTreeNode
                                );

    /// <summary>
    ///   Getter for SelectedIndex property.
    /// </summary>
    /// <returns>
    ///   Selected index property value
    /// </returns>
    function  fget_SelectedIndex : Integer;

    /// <summary>
    ///   Getter for OnSelectChange property.
    /// </summary>
    /// <returns>
    ///   Current OnSelectChange content .
    /// </returns>
    function  fget_OnSelectChange
                                : TGIS_PvlEvent;

    /// <summary>
    ///   Setter for OnSelectChange property.
    /// </summary>
    /// <param name="_value">
    ///   New OnSelectChange content.
    /// </param>
    procedure fset_OnSelectChange
                                ( const _value      : TGIS_PvlEvent
                                );

    /// <summary>
    ///   Create a root even. Only for internal use/
    /// </summary>
    procedure CreateRoot        ;
  end;


  /// <summary>
  ///   PVL Tree Node interface.
  /// </summary>
  IGIS_PvlTreeNode = interface
    {$IFDEF DCC}
      ['{B1E9EFC5-4D4B-4BB2-81F5-E4F6806B606B}']
    {$ENDIF}

    /// <summary>
    ///   Getter for Caption property.
    /// </summary>
    /// <returns>
    ///   Caption property value
    /// </returns>
    function  fget_Caption      : String;

    /// <summary>
    ///   Setter for Caption property.
    /// </summary>
    /// <param name="_value">
    ///   New Caption value
    /// </param>
    procedure fset_Caption      ( const _value      : String
                                );
    /// <summary>
    ///   Getter for Expanded property.
    /// </summary>
    /// <returns>
    ///   True if node is expanded
    /// </returns>
    function  fget_Expanded     : Boolean;

    /// <summary>
    ///   Setter for Expanded property.
    /// </summary>
    /// <param name="_value">
    ///   New Expanded value
    /// </param>
    procedure fset_Expanded     ( const _value      : Boolean
                                );
    /// <summary>
    ///   Getter for NativeControl property.
    /// </summary>
    /// <returns>
    ///   NativeControl value
    /// </returns>
    function  fget_NativeControl
                                : TObject;

    /// <summary>
    ///   Setter for NativeControl property.
    /// </summary>
    /// <param name="_value">
    ///   New NativeControl property value
    /// </param>
    procedure fset_NativeControl( const _value      : TObject
                                );

    /// <summary>
    ///   Getter for Parent property.
    /// </summary>
    /// <returns>
    ///   Parent property value
    /// </returns>
    function  fget_Parent       : TGIS_PvlTreeNode;

    /// <summary>
    ///   Getter for Node property
    /// </summary>
    /// <param name="_index">
    ///   Index of node
    /// </param>
    /// <returns>
    ///   Node
    /// </returns>
    function  fget_Node         ( const _index      : Integer
                                ) : TGIS_PvlTreeNode;

    /// <summary>
    ///   Additional creation procedure
    /// </summary>
    /// <param name="_node">\
    ///   Created node
    /// </param>
    /// <param name="_caption">
    ///   Caption of created node
    /// </param>
    /// <param name="_index">
    ///   Index on which it should be placed
    /// </param>
    procedure doCreateNode      ( const _node       : TGIS_PvlTreeNode;
                                  const _caption    : String ;
                                  const _index      : Integer
                                ) ;

    /// <summary>
    ///   Additional removal procedure
    /// </summary>
    /// <param name="_node">
    ///   Node to be removed
    /// </param>
    procedure doRemoveNode      ( const _node       : TGIS_PvlTreeNode
                                ) ;

    /// <summary>
    ///   Additional deletation procedure
    /// </summary>
    /// <param name="_node">
    ///   Node to be removed
    /// </param>
    procedure doDeleteNode      ( const _node       : TGIS_PvlTreeNode
                                ) ;

    /// <summary>
    ///   Additional move procedure
    /// </summary>
    /// <param name="_node">
    ///   Node to be moved
    /// </param>
    /// <param name="_newParent">
    ///   New parent node
    /// </param>
    /// <param name="_index">
    ///   Index on which it should be placed
    /// </param>
    procedure doMoveNode        ( const _node       : TGIS_PvlTreeNode;
                                  const _newParent  : TGIS_PvlTreeNode ;
                                  const _index      : Integer
                                );
  end;

  /// <summary>
  ///   PVL Group Box interface.
  /// </summary>
  IGIS_PvlGroupBox = interface( IGIS_PvlControl )
   {$IFDEF DCC}
     ['{3F2A3D0E-BAAE-4C7C-8FB0-83FB24B0EEF9}']
   {$ENDIF}

   /// <summary>
   ///   Getter for Caption property.
   /// </summary>
   /// <returns>
   ///   Caption of the control
   /// </returns>
   function  fget_Caption      : String;

   /// <summary>
   ///   Setter for Caption property.
   /// </summary>
   /// <param name="_value">
   ///   New Caption value
   /// </param>
   procedure fset_Caption      ( const _value      : String
                               );
  end;

  /// <summary>
  ///   Base PVL class.
  /// </summary>
  TGIS_PvlBase = class({$IFDEF DCC}TGIS_UncountedInterfacedObject {$ELSE} TGIS_ObjectDisposable {$ENDIF}, IGIS_PvlBase )
    protected
      /// <summary>
      ///   Underlying platform specific control.
      /// </summary>
      oPlatform                 : IGIS_PvlBase;

    protected
      function fget_NativeControl
                                : TObject ;
                                virtual;
    private
      /// <summary>
      ///   For controls managing private context like Panel or GroupBox.
      /// </summary>
      oContextLocal               : TGIS_PvlContext ;

    public
      /// <summary>
      ///   Native control, need to be cast to platform control.
      /// </summary>
      property NativeControl    : TObject
                                  read  fget_NativeControl;
  end;

  /// <summary>
  ///   Base PVL class for system forms.
  /// </summary>
  TGIS_PvlSystemForm = {$IFDEF OXYGENE} abstract {$ENDIF}  class( TGIS_PvlBase )
    private
      ownFormInstance : Boolean   ;
    public

      /// <summary>
      ///   Create Modal form with parent.
      /// </summary>
      /// <param name="_context">
      ///   Context owner of the system Form
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create        (       _context     : TGIS_PvlContext
                                ); overload; virtual;

      /// <summary>
      ///   Create Modal form with parent.
      /// </summary>
      /// <param name="_context">
      ///   Context owner of the system Form
      /// </param>
      /// <param name="_self">
      ///   Reference to the TGIS_PvlSystemForm
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create        (       _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlSystemForm
                                ); overload; virtual;

    protected
      procedure doDestroy; override;
    public

      /// <summary>
      ///   Underlying platform specific form.
      /// </summary>
      oForm : TObject ;

    protected

      /// <summary>
      ///   Form instance.
      /// </summary>
      oFormInstance : TObject ;

      /// <summary>
      ///   Parent of the form.
      /// </summary>
      oParent : TObject;
  end;

  /// <summary>
  ///   Basic PVL Object.
  /// </summary>
  TGIS_PvlObject = class( TGIS_PvlBase, IGIS_PvlObject )
    public
      /// <summary>
      ///   Create a PVL object.
      /// </summary>
      /// <param name="_context">
      ///   Context
      /// </param>
      {#ownership:_context:ownif_empty}
      constructor Create         ( const _context : TGIS_PvlContext
                                 );
    protected
      /// <summary>
      ///   Context of the created control which helps creating
      ///   control depending on the platform its created on.
      /// </summary>
      oContext                  : TGIS_PvlContext;

    protected
      /// <summary>
      ///   Perform actuallow-velel construction.
      /// </summary>
      /// <param name="_context">
      ///   Context
      /// </param>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); virtual;
      procedure doDestroy; override;

    private
      procedure fset_Platform     ( const _value      : IGIS_PvlBase
                                  ) ;
      function  fget_PlatformControl
                                  : IGIS_PvlControl ;             virtual;
      property  PlatformControl   : IGIS_PvlControl
                                    read  fget_PlatformControl;   {$IFNDEF GENDOC}{$IFDEF OXYGENE}virtual;{$ENDIF}{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_NativeControl
                                : TObject ; override;
    public
      /// <summary>
      ///   Underlying platform specific control.
      /// </summary>
      property &Platform        : IGIS_PvlBase
                                  read  oPlatform
                                  write fset_Platform;

  end;

  /// <summary>
  ///   Basic PVL control.
  /// </summary>
  TGIS_PvlControl = class( TGIS_PvlObject, IGIS_PvlControl )
    public
      /// <summary>
      ///   Create a PVL control.
      /// </summary>
      /// <param name="_context">
      ///   Context
      /// </param>
      {#ownership:_context:ownif_empty}
      constructor Create         ( const _context : TGIS_PvlContext
                                 );
    protected
      /// <summary>
      ///   Initialize control.
      /// </summary>
      procedure initControl; virtual;

      /// <summary>
      ///   Add another control. USE WITH CAUTION AS SOME CONTROLS
      ///   CANT BE ADDED TO ANOTHERS.
      /// </summary>=
      /// <param name="_control">
      ///   Control to be added.
      /// </param>
      procedure &add              ( const _control : TObject
                                  ); virtual;
    protected
      /// <summary>
      ///   Tells us if object is visible BEFORE the constructor ends;
      /// </summary>
      bVisible                  : Boolean;

      /// <summary>
      ///   Is the control directly enabled/disabed
      /// </summary>
      bEnabled                  : Boolean ;

    protected
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Context    : TGIS_PvlContext ; virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Context    ( const _value: TGIS_PvlContext
                                );

    protected
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnClick          : TGIS_PvlEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnClick          ( const _value      : TGIS_PvlEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnDblClick       :  TGIS_PvlEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnDblClick       ( const _value      : TGIS_PvlEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnMouseDown      :  TGIS_PvlMouseEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnMouseDown      ( const _value      : TGIS_PvlMouseEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnMouseMove      : TGIS_PvlMouseMoveEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnMouseMove      ( const _value      : TGIS_PvlMouseMoveEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnMouseUp        : TGIS_PvlMouseEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnMouseUp        ( const _value      : TGIS_PvlMouseEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnMouseWheel     : TGIS_PvlMouseWheelEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnMouseWheel     ( const _value      : TGIS_PvlMouseWheelEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnMouseEnter     : TGIS_PvlEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnMouseEnter     ( const _value      : TGIS_PvlEvent
                                      );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_OnMouseLeave     : TGIS_PvlEvent;
      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_OnMouseLeave     ( const _value      : TGIS_PvlEvent
                                      );

    protected
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_IsStyled   : Boolean ; virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Anchors    : TGIS_PvlAnchors;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Anchors    ( const _value      : TGIS_PvlAnchors
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Align      : TGIS_PvlAlign;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Align      ( const _value      : TGIS_PvlAlign
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Enabled    : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Enabled    ( const _value      : Boolean
                                ); virtual ;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Width      : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Width      ( const _value      : Integer
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Height     : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Height     ( const _value      : Integer
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Left       : Integer;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_Left       ( const _value      : Integer
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Top        : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Top        ( const _value      : Integer
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_TabOrder   : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_TabOrder   ( const _value      : Integer
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Visible    : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Visible    ( const _value      : Boolean
                                );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Hint       : String;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Hint       ( const _value      : String
                                );
    public
      /// <summary>
      ///   Place control on the PVL form.
      /// </summary>
      /// <param name="_width">
      ///   Width of the control
      /// </param>
      /// <param name="_height">
      ///   Height of the control
      /// </param>
      /// <param name="_xsibling">
      ///   Horizontal sibling you want to relate in placement process
      /// </param>
      /// <param name="_xdistance">
      ///   Distance in x axis related to 0 point or sibling if defined
      /// </param>
      /// <param name="_ysibling">
      ///   Vertical sibling you want to relate in placement process
      /// </param>
      /// <param name="_ydistance">
      ///   Distance in y axis related to 0 point or sibling if defined.
      ///   If set to -1 then put control on the same y axis with sibling.
      ///   If set to 0 then put control below sibling.
      /// </param>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                ); virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure SetFocus; virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure DoRedraw; virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure ProcessMessages;
    public
      /// <summary>
      ///   Context of the control, platform related.
      /// </summary>
      property Context          : TGIS_PvlContext
                                  read  fget_Context
                                  write fset_Context ;

      /// <summary>
      ///   Does component use styles for colors?
      /// </summary>
      /// <remarks>
      ///   Styles is normally handled automatically. This property inform parent
      ///   that PVL requires custom action.
      /// </remarks>
      property IsStyled         : Boolean
                                  read  fget_IsStyled ;

      /// <summary>
      ///   Anchors of the control.
      /// </summary>
      property Anchors          : TGIS_PvlAnchors
                                  read  fget_Anchors
                                  write fset_Anchors;
      /// <summary>
      ///   Align of the control.
      /// </summary>
      property Align            : TGIS_PvlAlign
                                  read  fget_Align
                                  write fset_Align;
      /// <summary>
      ///   If control is enabled.
      /// </summary>
      property Enabled          : Boolean
                                  read  fget_Enabled
                                  write fset_Enabled;
      /// <summary>
      ///   Width of the control.
      /// </summary>
      property Width            : Integer
                                  read  fget_Width
                                  write fset_Width;
      /// <summary>
      ///   Height of the control.
      /// </summary>
      property Height           : Integer
                                  read  fget_Height
                                  write fset_Height;
      /// <summary>
      ///   Position of left margin.
      /// </summary>
      property Left             : Integer
                                  read  fget_Left
                                  write fset_Left;
      /// <summary>
      ///   Top of left margin.
      /// </summary>
      property Top              : Integer
                                  read  fget_Top
                                  write fset_Top;
      /// <summary>
      ///   Order of control when pressing Tab key.
      /// </summary>
      property TabOrder         : Integer
                                  read  fget_TabOrder
                                  write fset_TabOrder;
      /// <summary>
      ///   Visibility of the control.
      /// </summary>
      property Visible          : Boolean
                                  read  fget_Visible
                                  write fset_Visible;
      /// <summary>
      ///   Hint for the control.
      /// </summary>
      /// <remarks>
      ///   Implemented on VCL (all RAD versions) and FMX (RAD 10.1 and up)
      ///   only. Other platforms not supported yet.
      /// </remarks>
      property Hint             : String
                                  read  fget_Hint
                                  write fset_Hint;
    published
      /// <event/>
      /// <summary>
      ///   Occurs when the control is clicked.
      /// </summary>
      property OnClick           : TGIS_PvlEvent read  fget_OnClick
                                                 write fset_OnClick ;

      /// <event/>
      /// <summary>
      ///   Occurs when the left mouse button is
      ///   double-clicked with the pointer over the control.
      /// </summary>
      property OnDblClick        : TGIS_PvlEvent read  fget_OnDblClick
                                                 write fset_OnDblClick ;

      /// <event/>
      /// <summary>
      ///   Occurs when the user presses a mouse button
      ///   with the mouse pointer over a control.
      /// </summary>
      property OnMouseDown       : TGIS_PvlMouseEvent
                                                 read  fget_OnMouseDown
                                                 write fset_OnMouseDown ;

      /// <event/>
      /// <summary>
      ///   Occurs when the user moves the mouse pointer
      ///   while the mouse pointer is over a control.
      /// </summary>
      property OnMouseMove       : TGIS_PvlMouseMoveEvent
                                                 read  fget_OnMouseMove
                                                 write fset_OnMouseMove ;

      /// <event/>
      /// <summary>
      ///   Occurs when the user releases a mouse button
      ///   that was pressed with the mouse pointer over a component.
      /// </summary>
      property OnMouseUp         : TGIS_PvlMouseEvent
                                                 read  fget_OnMouseUp
                                                 write fset_OnMouseUp ;

      /// <event/>
      /// <summary>
      ///   Occurs when the mouse wheel moves while the control has focus.
      /// </summary>
      property OnMouseWheel      : TGIS_PvlMouseWheelEvent
                                                 read  fget_OnMouseWheel
                                                 write fset_OnMouseWheel ;

      /// <event/>
      /// <summary>
      ///   Occurs when the mouse cursor enters the area of the control.
      /// </summary>
      property OnMouseEnter       : TGIS_PvlEvent
                                                 read  fget_OnMouseEnter
                                                 write fset_OnMouseEnter ;

      /// <event/>
      /// <summary>
      ///   Occurs when the mouse cursor leaves the control area.
      /// </summary>
      property OnMouseLeave       : TGIS_PvlEvent
                                                 read  fget_OnMouseLeave
                                                 write fset_OnMouseLeave ;

  end;

  TGIS_PvlControlClass = class of TGIS_PvlControl ;

  /// <summary>
  ///   PVL symbol list to provide glyphs based on images abd SVG.
  /// </summary>
  TGIS_PvlIconsList = class( TGIS_PvlControl )
    private
      oSymbols                    : TGIS_SymbolList ;
      oList                       : TGIS_ObjectList ;
    private
      function  fget_Icon         ( const _index      : Integer
                                  ) :TGIS_SymbolAbstract;
    protected
      /// <inheritdoc/>
      procedure  doCreate         ( const _context   : TGIS_PvlContext
                                  ); override;

      /// <inheritdoc/>
      procedure initControl       ;                               override;

      /// <inheritdoc/>
      procedure doDestroy; override;

    public
      /// <summary>
      ///   Add file to the list of icons.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      /// <remarks>
      ///  .bmp, .jpg, .png, .svg, and .cgm files are accepted.
      /// </remarks>
      /// <returns>
      ///  Position on the list.
      /// </returns>
      function  Add             ( const _path       : String
                                ) : Integer; reintroduce; overload; virtual;

      /// <summary>
      ///   Add file to the list of icons.
      /// </summary>
      /// <param name="_name">
      ///   name of the file in resources
      /// </param>
      /// <param name="_extension">
      ///   extension of the file in resources
      /// </param>
      /// <remarks>
      ///  .bmp, .jpg, .png, .svg, and .cgm files are accepted.
      /// </remarks>
      /// <returns>
      ///  Position on the list.
      /// </returns>
      function  AddFromResource ( const _name       : String ;
                                  const _extension  : TGIS_PvlResourceExtension
                                ) : Integer; virtual;


    public
      /// <inheritdoc/>
      procedure SetFocus; override;

    public
      /// <summary>
      ///   Access icon symbol object.
      /// </summary>
      /// <param name="_index">
      ///   position of the icon; if outside the scope then nil returned
      /// </param>
      property Icon[ const _index : Integer ] : TGIS_SymbolAbstract
               read fget_Icon;

  end;

  /// <summary>
  ///   PVL panel control.
  /// </summary>
  TGIS_PvlPanel = class( TGIS_PvlControl, IGIS_PvlPanel )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

      /// <inheritdoc/>
      procedure doDestroy         ; override;

    public
      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlPanel ;               reintroduce;
      property PlatformControl    : IGIS_PvlPanel
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Context      : TGIS_PvlContext;              override;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Scrollable   : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Scrollable   ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Border       : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Border       ( const _value      : Boolean
                                  );

    public

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure RemoveAllComponents;

    public
      /// <summary>
      ///   If panel is scrollable.
      /// </summary>
      property Scrollable         : Boolean
                                    read  fget_Scrollable
                                    write fset_Scrollable;

      /// <summary>
      ///   If true then show border.
      /// </summary>
      property Border             : Boolean
                                    read  fget_Border
                                    write fset_Border ;
  end;


  /// <summary>
  ///   PVL Page
  /// </summary>
  TGIS_PvlPage = class
    private
      FOnEnter                    : TGIS_PvlEvent   ;
      FOnExit                     : TGIS_PvlEvent   ;
      FBeforeExit                 : TGIS_PvlEvent   ;
      FPage                       : TGIS_PvlPanel   ;
      FPageNumber                 : Integer         ;
      FNext                       : TGIS_PvlPage    ;
      FPrevious                   : TGIS_PvlPage    ;

    public
      /// <summary>
      ///   Constructor for the pages instance
      /// </summary>
      /// <param name="_context">
      ///   Context in which it should be created
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( const _context: TGIS_PvlContext
                                  ) ;

      /// <summary>
      ///   Activate page
      /// </summary>
      /// <param name="_sender">
      ///   Object that called the action
      /// </param>
      procedure Activate          ( const _sender     : TObject
                                  ) ;

      /// <summary>
      ///   Deactivate page
      /// </summary>
      /// <param name="_sender">
      ///   Object that called the action
      /// </param>
      procedure Deactivate        ( const _sender     : TObject
                                  ) ;

    private
      function  fget_Width      : Integer ;
      procedure fset_Width      ( const _value : Integer ) ;
      function  fget_Context    : TGIS_PvlContext ;
      function  fget_Height     : Integer ;
      procedure fset_Height     ( const _value : Integer ) ;
      function  fget_Left       : Integer ;
      procedure fset_Left       ( const _value : Integer ) ;
      function  fget_Top        : Integer ;
      procedure fset_Top        ( const _value : Integer ) ;
      procedure fset_Next       ( const _value : TGIS_PvlPage ) ;
      procedure fset_Previous   ( const _value : TGIS_PvlPage ) ;

    public

      /// <summary>
      ///   Number of a page
      /// </summary>
      property PageNumber         : Integer
                                  read  FPageNumber
                                  write FPageNumber ;

      /// <summary>
      ///   Underlying page accessor
      /// </summary>
      property Page               : TGIS_PvlPanel
                                  read  FPage
                                  write FPage ;

      /// <summary>
      ///   Height of the page
      /// </summary>
      property Height             : Integer
                                  read  fget_Height
                                  write fset_Height ;

      /// <summary>
      ///   Width of the page
      /// </summary>
      property Width              : Integer
                                  read  fget_Width
                                  write fset_Width ;

      /// <summary>
      ///   Context of the page
      /// </summary>
      property Context            : TGIS_PvlContext
                                  read fget_Context ;

      /// <summary>
      ///   Top of the page
      /// </summary>
      property Top                : Integer
                                  read  fget_Top
                                  write fset_Top ;

      /// <summary>
      ///   Left of the page
      /// </summary>
      property Left               : Integer
                                  read  fget_Left
                                  write fset_Left ;

      /// <summary>
      ///   Next page object.
      /// </summary>
      property Next               : TGIS_PvlPage
                                  read  FNext
                                  write fset_Next ;

      /// <summary>
      ///   Previous page object.
      /// </summary>
      property Previous           : TGIS_PvlPage
                                  read  FPrevious
                                  write fset_Previous ;

    published
      /// <event/>
      /// <summary>
      ///   OnEnter event accessor.
      /// </summary>
      property OnEnter            : TGIS_PvlEvent
                                  read  FOnEnter
                                  write FOnEnter;

      /// <event/>
      /// <summary>
      ///   OnExit event accessor.
      /// </summary>
      property OnExit             : TGIS_PvlEvent
                                  read  FOnExit
                                  write FOnExit;

      /// <event/>
      /// <summary>
      ///   OnExit event accessor.
      /// </summary>
      property BeforeExit         : TGIS_PvlEvent
                                  read  FBeforeExit
                                  write FBeforeExit;
  end;

  /// <summary>
  ///   PVL pages
  /// </summary>
  TGIS_PvlPages = class ( TGIS_PvlControl )
    private
      oPanel          : TGIS_PvlPanel         ;
      bAbort          : Boolean               ;
      FPages          : TList<TGIS_PvlPage>   ;
      FActive         : TGIS_PvlPage          ;
      FOnPageChange   : TGIS_PvlEvent         ;
      ctx             : TGIS_PvlContext       ;
      prevIsLast      : Boolean               ;
      prevIsFirst     : Boolean               ;
    protected

      /// <inheritdoc/>
      procedure  doCreate         ( const _context   : TGIS_PvlContext
                                  ); override;

      /// <inheritdoc/>
      procedure doDestroy; override;

    public
      /// <summary>
      ///   Add another page
      /// </summary>
      /// <returns>
      ///   New page
      /// </returns>
      function  AddPage             : TGIS_PvlPage ;

      /// <summary>
      ///   Move to the next page
      /// </summary>
      /// <param name="_sender">
      ///   Object that called the action
      /// </param>
      procedure Next                ( const _sender : TObject ) ;

      /// <summary>
      ///   Move to the previous page
      /// </summary>
      /// <param name="_sender">
      ///   Object that called the action
      /// </param>
      procedure Previous            ( const _sender : TObject ) ;

      /// <summary>
      ///   True if currently active page is the last one
      /// </summary>
      /// <returns>
      ///   True if active page is last
      /// </returns>
      function  IsLast              : Boolean      ;

      /// <summary>
      ///   True if currently active page is the first one
      /// </summary>
      /// <returns>
      ///   True if active page is first
      /// </returns>
      function  IsFirst             : Boolean      ;

      /// <summary>
      ///   Activate page with specific number
      /// </summary>
      /// <param name="_pageNr">
      ///   Index / Number of the page to be activated
      /// </param>
      /// <param name="_sender">
      ///   Object that called activation
      /// </param>
      procedure Activate          ( const _pageNr : Integer ;
                                    const _sender : TObject
                                  ) ;

      /// <inheritdoc/>
      procedure SetFocus          ; override;

      /// <summary>
      ///   Abort changing pages. Has to be
      ///   uses in pair with BeforeExit event.
      /// </summary>
      procedure Abort             ;

      /// <summary>
      ///   Provied active page.
      /// </summary>
      /// <returns>
      ///   Active page.
      /// </returns>
      function GetActivePageNumber  : Integer   ;

    private

      /// <summary>
      ///   Dectivate current page
      /// </summary>
      /// <param name="_sender">
      ///   Object that called deactivation
      /// </param>
      procedure Deactivate        ( const _sender : TObject
                                  ) ;

    published
      /// <event/>
      /// <summary>
      ///   OnPageChange event accessor.
      /// </summary>
      property OnPageChange      : TGIS_PvlEvent
                                  read  FOnPageChange
                                  write FOnPageChange;
  end;

  /// <summary>
  ///   PVL canvas.
  /// </summary>
  TGIS_PvlCanvas = class(TGIS_PvlControl, IGIS_PvlCanvas )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlCanvas ;        reintroduce;
      property PlatformControl    : IGIS_PvlCanvas
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    protected
      /// <inheritdoc from="IGIS_PvlCanvas"/>
      function  fget_OnRender     : TGIS_PvlRendererEvent ;

      /// <inheritdoc from="IGIS_PvlCanvas"/>
      procedure fset_OnRender     ( const _value : TGIS_PvlRendererEvent
                                  );


    public

      /// <event/>
      /// <summary>
      ///   Renderer on which we can draw.
      /// </summary>
      property OnRender           : TGIS_PvlRendererEvent
                                  read  fget_OnRender
                                  write fset_OnRender;
  end;

  /// <summary>
  ///   PVL preview panel control with ability to
  ///   show text, images or GIS elements.
  /// </summary>
  TGIS_PvlPreviewPanel = class( TGIS_PvlControl, IGIS_PvlPreviewPanel )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlPreviewPanel ;        reintroduce;
      property PlatformControl    : IGIS_PvlPreviewPanel
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    protected
      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_IsStyled     : Boolean ; override;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Caption      : String ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_FontSize     : Integer ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  ) ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_FontStyle    : TGIS_FontStyles ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  ) ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_FontFamily   : String ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_FontFamily   ( const _value      : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Border       : Boolean ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Border       ( const _value      : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Bitmap       : TGIS_Bitmap ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Bitmap       ( const _value      : TGIS_Bitmap
                                  ) ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Color        : TGIS_Color ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Color        ( const _value      : TGIS_Color
                                  ) ;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_StyledAreaColor
                                  : TGIS_Color ;
    public

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure Invalidate;

    public
      /// <summary>
      ///   Border of the control.
      /// </summary>
      property Border           : Boolean
                                  read  fget_Border
                                  write fset_Border;
      /// <summary>
      ///   Caption in the middle of a control.
      /// </summary>
      property Caption          : String
                                  read  fget_Caption
                                  write fset_Caption;
      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle        : TGIS_FontStyles
                                  read  fget_FontStyle
                                  write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      property FontSize         : Integer
                                  read  fget_FontSize
                                  write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily       : String
                                  read  fget_FontFamily
                                  write fset_FontFamily;
      /// <summary>
      ///   Image to be shown on the panel.
      /// </summary>
      property Bitmap           : TGIS_Bitmap
                                  read fget_Bitmap
                                  write fset_Bitmap;
      /// <summary>
      ///   Color of the background of the panel.
      /// </summary>
      property Color            : TGIS_Color
                                  read fget_Color
                                  write fset_Color;
      /// <summary>
      ///   Styled symbol color. Valid only if IsStyled is True.
      /// </summary>
      property StyledAreaColor  : TGIS_Color
                                  read fget_StyledAreaColor ;
  end;


  /// <summary>
  ///   PVL group box component.
  /// </summary>
  TGIS_PvlGroupBox = class( TGIS_PvlControl, IGIS_PvlGroupBox )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
      /// <inheritdoc/>
      procedure doDestroy         ; override;

    public
      /// <inheritdoc/>
      procedure Place           ( const _width      : Integer;
                                  const _height     : Integer;
                                  const _xsibling   : TGIS_PvlControl;
                                  const _xdistance  : Integer;
                                  const _ysibling   : TGIS_PvlControl;
                                  const _ydistance  : Integer
                                ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlGroupBox ;            reintroduce;
      property PlatformControl    : IGIS_PvlGroupBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlGroupBox"/>
      function  fget_Context      : TGIS_PvlContext; override ;

      /// <inheritdoc from="IGIS_PvlGroupBox"/>
      function  fget_Caption      : String ;

      /// <inheritdoc from="IGIS_PvlGroupBox"/>
      procedure fset_Caption      ( const _value      : String
                                  );

    public
      /// <summary>
      ///   Title of the group box.
      /// </summary>
      property  Caption         : String
                                  read  fget_Caption
                                  write fset_Caption ;
  end;

  /// <summary>
  ///   PVL trackbar component.
  /// </summary>
  TGIS_PvlTrackBar = class ( TGIS_PvlControl, IGIS_PvlTrackBar )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlTrackBar ;             reintroduce;
      property PlatformControl    : IGIS_PvlTrackBar
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}
    protected

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_Minimum      : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Minimum      ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_Maximum      : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Maximum      ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_Position     : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Position     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

    public

      /// <summary>
      ///   Minimum value of the trackbar
      /// </summary>
      property Minimum            : Integer
                                    read  fget_Minimum
                                    write fset_Minimum ;

      /// <summary>
      ///   Maximum value of the trackbar
      /// </summary>
      property Maximum            : Integer
                                    read  fget_Maximum
                                    write fset_Maximum ;

      /// <summary>
      ///   Current value of the trackbar
      /// </summary>
      property Position           : Integer
                                    read  fget_Position
                                    write fset_Position ;

    published
      /// <event/>
      /// <summary>
      ///   On change event of the trackbar
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                    read  fget_OnChange
                                    write fset_OnChange ;

  end ;

  /// <summary>
  ///   PVL list component.
  /// </summary>
  TGIS_PvlListBox = class( TGIS_PvlControl, IGIS_PvlListBox )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlListBox ;             reintroduce;
      property PlatformControl    : IGIS_PvlListBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_ItemList     : TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_ItemsCount   : Integer;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_ItemIndex    : Integer;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_SelectedItems
                                  : TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Selected     ( const _index      : Integer
                                  ) : Boolean ;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_Selected     ( const _index      : Integer ;
                                    const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Multiselect  : Boolean;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_Multiselect  ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_OnChange     : TGIS_PvlEvent ;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  ) ;

    public
      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure ItemsClear;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure ItemsAdd        ( const _item       : String
                                );
    public
      /// <summary>
      ///   List of the items in the component.
      /// </summary>
      property  ItemList        : TGIS_ListOfStrings
                                  read fget_ItemList;

      /// <summary>
      ///   Count of all items on the list.
      /// </summary>
      property ItemsCount       : Integer
                                  read  fget_ItemsCount;

      /// <summary>
      ///   Ability to select multiple items.
      /// </summary>
      property Multiselect      : Boolean
                                  read  fget_Multiselect
                                  write fset_Multiselect;

      /// <summary>
      ///   State of given by index item.
      /// </summary>
      /// <param name="_index">
      ///   index of the element
      /// </param>
      property Selected         [ const _index : Integer ]
                                : Boolean
                                  read  fget_Selected
                                  write fset_Selected ;

      /// <summary>
      ///   All selected items when using multiselect mode.
      /// </summary>
      property SelectedItems    : TGIS_ListOfStrings
                                  read fget_SelectedItems;

      /// <summary>
      ///   Item accessor.
      /// </summary>
      /// <param name="_idx">ID of the item you want to get</param>
      property Item[ const _idx : Integer ]
                                : String
                                  read  fget_Item;

      /// <summary>
      ///   Current index of item on the list.
      /// </summary>
      property ItemIndex        : Integer
                                  read  fget_ItemIndex
                                  write fset_ItemIndex;

    published
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange         : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
  end;

  /// <summary>
  ///   PVL svg list component.
  /// </summary>
  TGIS_PvlSVGList = class( TGIS_PvlControl, IGIS_PvlSVGList )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlSVGList ;             reintroduce;
      property PlatformControl    : IGIS_PvlSVGList
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_ItemList     : TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_ItemsCount   : Integer;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_SelectedItems
                                  : TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Selected     ( const _index      : Integer
                                  ) : Boolean ;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_Selected     ( const _index      : Integer ;
                                    const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Multiselect  : Boolean;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_Multiselect  ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_ItemIndex    : Integer;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_OnChange     : TGIS_PvlEvent ;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  ) ;
    public
      /// <summary>
      ///   List of the items in the component.
      /// </summary>
      property  ItemList        : TGIS_ListOfStrings
                                  read fget_ItemList;

      /// <summary>
      ///   Count of all items on the list.
      /// </summary>
      property ItemsCount       : Integer
                                  read  fget_ItemsCount;

      /// <summary>
      ///   Item accessor.
      /// </summary>
      /// <param name="_idx">ID of the item you want to get</param>
      property Item[ const _idx : Integer ]
                                : String
                                  read  fget_Item;

      /// <summary>
      ///   Current index of item on the list.
      /// </summary>
      property ItemIndex        : Integer
                                  read  fget_ItemIndex
                                  write fset_ItemIndex;

    public
      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure ItemsClear;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure ItemsAdd        ( const _item       : String
                                );

      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange         : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
  end;

  /// <summary>
  ///   PVL label control.
  /// </summary>
  TGIS_PvlLabel = class( TGIS_PvlControl, IGIS_PvlLabel )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlLabel ;               reintroduce;
      property PlatformControl    : IGIS_PvlLabel
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    private
      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FocusControl : TGIS_PvlControl;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FocusControl ( const _value      : TGIS_PvlControl
                                  );
      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontColor    : TGIS_Color;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontColor    ( const _value      : TGIS_Color
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_Alignment    : TGIS_PvlLabelTextAlignment;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_Alignment    ( const _value       : TGIS_PvlLabelTextAlignment
                                  );

    public
      /// <summary>
      ///   Caption of the label.
      /// </summary>
      property Caption            : String
                                  read  fget_Caption
                                  write fset_Caption;
      /// <summary>
      ///   Control to be focused upon selecting label.
      /// </summary>
      property FocusControl       : TGIS_PvlControl
                                  read  fget_FocusControl
                                  write fset_FocusControl;

      /// <summary>
      ///   Alignment of the text in the label.
      /// </summary>
      property Alignment         : TGIS_PvlLabelTextAlignment
                                  read  fget_Alignment
                                  write fset_Alignment ;

      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle          : TGIS_FontStyles
                                  read  fget_FontStyle
                                  write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      property FontSize           : Integer
                                  read  fget_FontSize
                                  write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily         : String
                                  read  fget_FontFamily
                                  write fset_FontFamily;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontColor          : TGIS_Color
                                  read  fget_FontColor
                                  write fset_FontColor;
  end;

  /// <summary>
  ///   PVL icon button control.
  /// </summary>
  TGIS_PvlIconButton = class ( TGIS_PvlControl, IGIS_PvlIconButton )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlIconButton ;          reintroduce;
      property PlatformControl    : IGIS_PvlIconButton
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    private
      oIconsList    : TGIS_PvlIconsList ;
      iIconIndex    : Integer ;
      iIconSize     : Integer ;
      FPush         : Boolean ;

    protected
      /// <summary>
      ///   Getter for IconsList property.
      /// </summary>
      /// <returns>
      ///   Current IconsList content.
      /// </returns>
      function  fget_IconsList    : TGIS_PvlIconsList; virtual;

      /// <summary>
      ///   Setter for IconsList property.
      /// </summary>
      /// <param name="_value">
      ///   New IconsList content.
      /// </param>
      procedure fset_IconsList    ( const _value      : TGIS_PvlIconsList
                                  ); virtual;

      /// <summary>
      ///   Getter for IconIndex property.
      /// </summary>
      /// <returns>
      ///   Current IconIndex content .
      /// </returns>
      function  fget_IconIndex    : Integer; virtual;

      /// <summary>
      ///   Setter for IconIndex property.
      /// </summary>
      /// <param name="_value">
      ///   New IconIndex content.
      /// </param>
      procedure fset_IconIndex    ( const _value      : Integer
                                  ); virtual;

      /// <summary>
      ///   Getter for IconSize property.
      /// </summary>
      /// <returns>
      ///   Current IconSize content .
      /// </returns>
      function  fget_IconSize     : Integer; virtual;

      /// <summary>
      ///   Setter for IconSize property.
      /// </summary>
      /// <param name="_value">
      ///   New IconSize content.
      /// </param>
      procedure fset_IconSize     ( const _value      : Integer
                                  ); virtual;

      /// <inheritdoc from="IconButton"/>
      function  fget_Pushed       : Boolean;

      /// <inheritdoc from="IconButton"/>
      procedure fset_Pushed       ( const _value      : Boolean
                                  );
    public
      /// <summary>
      ///   Icon drawn on the button. Icon has to be in the PNG format.
      /// </summary>
      property IconsList          : TGIS_PvlIconsList
                                  read  fget_IconsList
                                  write fset_IconsList;

      /// <summary>
      ///   Index withib IconsList.
      /// </summary>
      property IconIndex          : Integer
                                  read  fget_IconIndex
                                  write fset_IconIndex;
      /// <summary>
      ///   Size of icon. Default is 16.
      /// </summary>
      property IconSize           : Integer
                                  read  fget_IconSize
                                  write fset_IconSize;

      /// <summary>
      ///   Property that allows button to be pushed.
      /// </summary>
      property StayPressed        : Boolean
                                  read  FPush
                                  write FPush ;

      /// <summary>
      ///   True if button is pushed.
      /// </summary>
      property Pushed             : Boolean
                                  read  fget_Pushed
                                  write fset_Pushed ;
  end;

  /// <summary>
  ///   PVL button control.
  /// </summary>
  TGIS_PvlButton = class( TGIS_PvlControl, IGIS_PvlButton )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ; override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlButton ;              reintroduce;
      property PlatformControl    : IGIS_PvlButton
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    private
      /// <inheritdoc from="IGIS_PvlButton"/>
      function  fget_Caption      : String; virtual;

      /// <inheritdoc from="IGIS_PvlButton"/>
      procedure fset_Caption      ( const _value      : String
                                  ); virtual;


      /// <inheritdoc from="IGIS_PvlButton"/>
      function  fget_Default      : Boolean; virtual;

      /// <inheritdoc from="IGIS_PvlButton"/>
      procedure fset_Default      ( const _value      : Boolean
                                  ); virtual;

    public
      /// <summary>
      ///   Caption of the button.
      /// </summary>
      property Caption            : String
                                  read  fget_Caption
                                  write fset_Caption;

      /// <summary>
      ///   If upon clicking enter it is clicked too.
      /// </summary>
      property &Default           : Boolean
                                  read  fget_Default
                                  write fset_Default;
  end;

  /// <summary>
  ///   Another PVL button control but was defined
  ///   as another component due to some platform issues.
  ///   To be used specificaly for modal form buttons like OK, Cancel, Help.
  /// </summary>
  TGIS_PvlModalButton = class( TGIS_PvlControl, IGIS_PvlModalButton )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    protected

      /// <summary>
      ///   If default property.
      /// </summary>
      FDefaut                     : Boolean;

    private
      function fget_PlatformControl
                                  : IGIS_PvlModalButton ;         reintroduce;
      property PlatformControl    : IGIS_PvlModalButton
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlModalButton"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      function  fget_Default      : Boolean;

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      procedure fset_Default      ( const _value      : Boolean
                                  );

    public
      /// <summary>
      ///   Caption of the button.
      /// </summary>
      property Caption            : String
                                  read  fget_Caption
                                  write fset_Caption;
      /// <summary>
      ///   If upon clicking enter it is clicked too.
      /// </summary>
      property &Default           : Boolean
                                  read  fget_Default
                                  write fset_Default;
    public
      /// <inheritdoc/>
      procedure SetFocus          ; override;
  end;

  /// <summary>
  ///   PVL editbox control.
  /// </summary>
  TGIS_PvlEdit = class( TGIS_PvlControl, IGIS_PvlEdit )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlEdit ;                reintroduce;
      property PlatformControl    : IGIS_PvlEdit
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_Text         : String;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_Text         ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_Editable     : Boolean;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_Editable     ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_SelectionStart
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_SelectionStart(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_SelectionLength
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_SelectionLength(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_SelectedText
                                  : String;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnKeyDown    : TGIS_PvlKeyEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnKeyDown    ( const _value      : TGIS_PvlKeyEvent
                                  );
      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                  );

    public

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetAsHyperlink      ;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetFontAlarm      ;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetFontDefault    ;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetBackgroundAlarm      ;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetBackgroundDefault    ;

    public
      /// <summary>
      ///   Text in the EditBox.
      /// </summary>
      property Text               : String
                                  read  fget_Text
                                  write fset_Text;
      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle         : TGIS_FontStyles
                                   read  fget_FontStyle
                                   write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      property FontSize          : Integer
                                   read  fget_FontSize
                                   write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily        : String
                                   read  fget_FontFamily
                                   write fset_FontFamily;

      /// <summary>
      ///   Is EditBox editable?
      /// </summary>
      property Editable           : Boolean
                                  read  fget_Editable
                                  write fset_Editable;

      /// <summary>
      ///   Index of the first letter of the selection.
      /// </summary>
      property SelectionStart     : Integer
                                  read  fget_SelectionStart
                                  write fset_SelectionStart;
      /// <summary>
      ///   Indicates how many letters should be selected starting
      ///   with SelectionStart one.
      /// </summary>
      property SelectionLength    : Integer
                                  read  fget_SelectionLength
                                  write fset_SelectionLength;

      /// <summary>
      ///   Selected text content.
      /// </summary>
      property SelectedText       : String
                                  read  fget_SelectedText;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
      /// <event/>
      /// <summary>
      ///   OnKeyDown event accessor.
      /// </summary>
      property OnKeyDown          : TGIS_PvlKeyEvent
                                  read  fget_OnKeyDown
                                  write fset_OnKeyDown;
      /// <event/>
      /// <summary>
      ///   OnKeyPress event accessor.
      /// </summary>
      property OnKeyPress         : TGIS_PvlKeyPressEvent
                                  read  fget_OnKeyPress
                                  write fset_OnKeyPress;
  end;

  /// <summary>
  ///   PVL memo control.
  /// </summary>
  TGIS_PvlMemo = class( TGIS_PvlControl, IGIS_PvlMemo )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlMemo ;                reintroduce;
      property PlatformControl    : IGIS_PvlMemo
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}
    protected
      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_CursorPos    : TPoint;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_CursorPos    ( const _value      : TPoint
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_Text         : String;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_Text         ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_SelectionStart
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_SelectionStart(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_SelectionLength
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_SelectionLength(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_SelectedText
                                  : String;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_WordWrap     : Boolean;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_WordWrap     ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_Editable     : Boolean;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_Editable     ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnKeyDown    : TGIS_PvlKeyEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnKeyDown    ( const _value      : TGIS_PvlKeyEvent
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure SetFontAlarm      ;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure SetFontDefault    ;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure Clear             ;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure AppendText        ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure AppendLine        ( const _value      : String
                                  );
    public
      /// <summary>
      ///   Cursor position.
      /// </summary>
      property CursorPos          : TPoint
                                  read  fget_CursorPos
                                  write fset_CursorPos;
      /// <summary>
      ///   Text in the EditBox.
      /// </summary>
      property Text               : String
                                  read  fget_Text
                                  write fset_Text;

      /// <summary>
      ///   Index of the first letter of the selection.
      /// </summary>
      property SelectionStart     : Integer
                                  read  fget_SelectionStart
                                  write fset_SelectionStart;
      /// <summary>
      ///   Indicates how many letters should be selected starting
      ///   with SelectionStart one.
      /// </summary>
      property SelectionLength    : Integer
                                  read  fget_SelectionLength
                                  write fset_SelectionLength;
      /// <summary>
      ///   Selected text content.
      /// </summary>
      property SelectedText       : String
                                  read  fget_SelectedText;

      /// <summary>
      ///   Is Memo word-wrapped?
      /// </summary>
      property WordWrap           : Boolean
                                  read  fget_WordWrap
                                  write fset_WordWrap;

      /// <summary>
      ///   Is Memo editable?
      /// </summary>
      property Editable           : Boolean
                                  read  fget_Editable
                                  write fset_Editable;
    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
      /// <event/>
      /// <summary>
      ///   OnKeyDown event accessor.
      /// </summary>
      property OnKeyDown          : TGIS_PvlKeyEvent
                                  read  fget_OnKeyDown
                                  write fset_OnKeyDown;
      /// <event/>
      /// <summary>
      ///   OnKeyPress event accessor.
      /// </summary>
      property OnKeyPress         : TGIS_PvlKeyPressEvent
                                  read  fget_OnKeyPress
                                  write fset_OnKeyPress;
  end;

  /// <summary>
  ///   PVL combobox control.
  /// </summary>
  TGIS_PvlComboBox = class( TGIS_PvlControl, IGIS_PvlComboBox )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlComboBox ;            reintroduce;
      property PlatformControl    : IGIS_PvlComboBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_ItemsCount   : Integer; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_ItemIndex    : Integer; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  ); virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Text        : String; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Text        ( const _value       : String
                                 ); virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_FontSize     : Integer ;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  ) ;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_FontStyle    : TGIS_FontStyles ;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  ) ;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_FontFamily   : String ;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_FontFamily   ( const _value      : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Tag         : NativeInt; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Tag         ( const _value       : NativeInt
                                 ); virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Sorted      : Boolean; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Sorted      ( const _value       : Boolean
                                 ); virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_DropDownCount
                                 : Integer; virtual;


      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_DropDownCount(
                                    const _value      : Integer
                                 ); virtual;
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_OnChange    : TGIS_PvlEvent; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_OnChange    ( const _value       : TGIS_PvlEvent
                                 ); virtual;
    public
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure BeginUpdate       ; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure EndUpdate         ; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsClear        ; virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsAdd         ( const _item        : String
                                 ); virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  IndexOf          ( const _item        : String
                                 ) : Integer; virtual;

    public
      /// <summary>
      ///   Count of all items on the list.
      /// </summary>
      property ItemsCount        : Integer
                                   read  fget_ItemsCount;

      /// <summary>
      ///   Item accessor.
      /// </summary>
      /// <param name="_idx">
      ///   ID of the item you want to get
      /// </param>
      property Item[ const _idx : Integer ]
                                 : String
                                   read  fget_Item;

      /// <summary>
      ///   Current index of item on the list.
      /// </summary>
      property ItemIndex         : Integer
                                   read  fget_ItemIndex
                                   write fset_ItemIndex;
      /// <summary>
      ///   Text inside of the combobox.
      /// </summary>
      property Text              : String
                                   read  fget_Text
                                   write fset_Text;

      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle         : TGIS_FontStyles
                                   read  fget_FontStyle
                                   write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      /// <remarks>
      ///   If used it overrides Height property of the combobox.
      /// </remarks>
      property FontSize          : Integer
                                   read  fget_FontSize
                                   write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily        : String
                                   read  fget_FontFamily
                                   write fset_FontFamily;

      /// <summary>
      ///   Stores a NativeInt integral value as a part of a component.
      /// </summary>
      property Tag               : NativeInt
                                   read  fget_Tag
                                   write fset_Tag;

      /// <summary>
      ///   If combobox is sorted.
      /// </summary>
      property Sorted            : Boolean
                                   read  fget_Sorted
                                   write fset_Sorted;

      /// <summary>
      ///   Number of combo items showed in the dropdown.
      /// </summary>
      property DropDownCount     : Integer
                                   read  fget_DropDownCount
                                   write fset_DropDownCount;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange         : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
  end;

  /// <summary>
  ///   PVL comboedit control defined as separate component due some
  ///   platform limitations.
  /// </summary>
  TGIS_PvlComboEdit = class ( TGIS_PvlComboBox, IGIS_PvlComboEdit )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlComboEdit ;           reintroduce;
      property PlatformControl    : IGIS_PvlComboEdit
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_ItemsCount   : Integer; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_ItemIndex    : Integer; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Text        : String; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Text        ( const _value       : String
                                 ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Tag         : NativeInt; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Tag         ( const _value       : NativeInt
                                 ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Sorted      : Boolean; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Sorted      ( const _value       : Boolean
                                 ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_DropDownCount
                                 : Integer; override;


      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_DropDownCount(
                                    const _value      : Integer
                                 ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_OnChange    : TGIS_PvlEvent; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_OnChange    ( const _value       : TGIS_PvlEvent
                                 ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                  );
    public
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure BeginUpdate       ; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure EndUpdate         ; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsClear        ; override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsAdd         ( const _item        : String
                                 ); override;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  IndexOf          ( const _item        : String
                                 ) : Integer; override;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      procedure SetFontAlarm      ;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      procedure SetFontDefault    ;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      procedure SetBackgroundAlarm      ;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      procedure SetBackgroundDefault    ;

    public

      /// <event/>
      /// <summary>
      ///   OnKeyPress event accessor.
      /// </summary>
      property OnKeyPress         : TGIS_PvlKeyPressEvent
                                  read  fget_OnKeyPress
                                  write fset_OnKeyPress;
  end;

  /// <summary>
  ///   PVL searchbox control
  /// </summary>
  TGIS_PvlSearchBox = class( TGIS_PvlControl, IGIS_PvlSearchBox )

    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlSearchBox ;           reintroduce;
      property PlatformControl    : IGIS_PvlSearchBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_FontSize     : Integer ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  ) ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_FontStyle    : TGIS_FontStyles ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  ) ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_FontFamily   : String ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_FontFamily   ( const _value      : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_ItemsCount   : Integer;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_ItemIndex    : Integer;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_Text        : String;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_Text        ( const _value       : String
                                 );

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_Sorted      : Boolean;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_Sorted      ( const _value       : Boolean
                                 );

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_DropDownCount
                                 : Integer;


      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_DropDownCount(
                                    const _value      : Integer
                                 );

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_OnChange    : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_OnChange    ( const _value       : TGIS_PvlEvent
                                 );

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                  );
    public
      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure ItemsClear        ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure ItemsAdd         ( const _item        : String
                                 );

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      function  IndexOf          ( const _item        : String
                                 ) : Integer;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure SetFontAlarm      ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure SetFontDefault    ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure SetBackgroundAlarm      ;

      /// <inheritdoc from="IGIS_PvlSearchBox"/>
      procedure SetBackgroundDefault    ;

    public

      /// <summary>
      ///   Count of all items on the list.
      /// </summary>
      property ItemsCount        : Integer
                                   read  fget_ItemsCount;

      /// <summary>
      ///   Item accessor.
      /// </summary>
      /// <param name="_idx">
      ///   ID of the item you want to get
      /// </param>
      property Item[ const _idx : Integer ]
                                 : String
                                   read  fget_Item;

      /// <summary>
      ///   Current index of item on the list.
      /// </summary>
      property ItemIndex         : Integer
                                   read  fget_ItemIndex
                                   write fset_ItemIndex;
      /// <summary>
      ///   Text inside of the combobox.
      /// </summary>
      property Text              : String
                                   read  fget_Text
                                   write fset_Text;

      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle         : TGIS_FontStyles
                                   read  fget_FontStyle
                                   write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      /// <remarks>
      ///   If used it overrides Height property of the combobox.
      /// </remarks>
      property FontSize          : Integer
                                   read  fget_FontSize
                                   write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily        : String
                                   read  fget_FontFamily
                                   write fset_FontFamily;

      /// <summary>
      ///   If combobox is sorted.
      /// </summary>
      property Sorted            : Boolean
                                   read  fget_Sorted
                                   write fset_Sorted;

      /// <summary>
      ///   Number of combo items showed in the dropdown.
      /// </summary>
      property DropDownCount     : Integer
                                   read  fget_DropDownCount
                                   write fset_DropDownCount;

      /// <event/>
      /// <summary>
      ///   OnKeyPress event accessor.
      /// </summary>
      property OnKeyPress         : TGIS_PvlKeyPressEvent
                                  read  fget_OnKeyPress
                                  write fset_OnKeyPress;

      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange ;
  end;


  /// <summary>
  ///   PVL color preview panel.
  /// </summary>
  TGIS_PvlColorPreview = class ( TGIS_PvlControl, IGIS_PvlColorPreview )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlColorPreview ;        reintroduce;
      property PlatformControl    : IGIS_PvlColorPreview
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      function  fget_Color        : TGIS_Color ;

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      procedure fset_Color        (  const _value      : TGIS_Color
                                  ) ;

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      function  fget_Border       : Boolean;

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      procedure fset_Border       ( const _value      : Boolean
                                  );

    public
      /// <summary>
      ///   Selected color property
      /// </summary>
      /// <value>
      ///   Color selected on the wheel
      /// </value>
      property Color              : TGIS_Color
                                  read  fget_Color
                                  write fset_Color ;

      /// <summary>
      ///   If true then show border.
      /// </summary>
      property Border             : Boolean
                                  read  fget_Border
                                  write fset_Border ;

  end;

  /// <summary>
  ///   PVL color wheel picker.
  /// </summary>
  TGIS_PvlColorWheel = class ( TGIS_PvlControl, IGIS_PvlColorWheel )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlColorWheel ;          reintroduce;
      property PlatformControl    : IGIS_PvlColorWheel
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      function  fget_Color        : TGIS_Color ;

      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      procedure fset_Color        ( const _value     : TGIS_Color
                                  ) ;

      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      procedure fset_OnChange     ( const _value     : TGIS_PvlEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      function  HueToRGB          ( const _hue       : Double
                                  ) : TGIS_PvlRGBVal ;

    public
      /// <summary>
      ///   Selected color property
      /// </summary>
      /// <value>
      ///   Color selected on the wheel
      /// </value>
      property Color              : TGIS_Color
                                  read  fget_Color
                                  write fset_Color ;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange ;
  end;

  /// <summary>
  ///   PVL Color slider
  /// </summary>
  TGIS_PvlColorBar = class ( TGIS_PvlControl, IGIS_PvlColorBar )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlColorBar ;            reintroduce;
      property PlatformControl    : IGIS_PvlColorBar
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_Value        : Double ;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_Value        ( const _value      : Double
                                  ) ;
      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_Color        : TGIS_Color ;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_Color        ( const _value      : TGIS_Color
                                  ) ;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_Alpha        : Boolean ;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_Alpha        ( const _value      : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

    public

      /// <summary>
      ///   Position of the slider
      /// </summary>
      /// <value>
      ///   Position from 0...1.0
      /// </value>
      property Value              : Double
                                  read  fget_Value
                                  write fset_Value ;

      /// <summary>
      ///   Selected color property
      /// </summary>
      /// <value>
      ///   Color selected on the slider
      /// </value>
      property Color              : TGIS_Color
                                  read  fget_Color
                                  write fset_Color ;

      /// <summary>
      ///   If slider operates in alpha channel
      /// </summary>
      /// <value>
      ///   True if using alpha channel on slider
      /// </value>
      property Alpha              : Boolean
                                  read  fget_Alpha
                                  write fset_Alpha ;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange ;
  end;

  /// <summary>
  ///   PVL custom combobox control.
  /// </summary>
  TGIS_PvlCustomComboBox = class ( TGIS_PvlControl, IGIS_PvlCustomComboBox )
    private
      function fget_PlatformControl
                                  : IGIS_PvlCustomComboBox ;      reintroduce;
      property PlatformControl    : IGIS_PvlCustomComboBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      function  fget_Value        : String ;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_Value        ( const _value     : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      function  fget_Fields       : TStringList ;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_Fields       ( const _value      : TStringList
                                  ) ;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_OnChange     ( const _value     : TGIS_PvlEvent
                                  );

    public

      /// <summary>
      ///   Value of the ComboBox.
      /// </summary>
      property Value              : String
                                  read  fget_Value
                                  write fset_Value ;

      /// <summary>
      ///   Fields of the ComboBox.
      /// </summary>
      property Fields             : TStringList
                                  read  fget_Fields
                                  write fset_Fields ;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
  end;

  /// <summary>
  ///   PVL size combobox control.
  /// </summary>
  TGIS_PvlSizeComboBox = class ( TGIS_PvlCustomComboBox, IGIS_PvlSizeComboBox )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlSizeComboBox ;        reintroduce;
      property PlatformControl    : IGIS_PvlSizeComboBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    public
      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure Fill              ( const _forSymbol  : Boolean ;
                                    const _forLine    : Boolean ;
                                    const _field      : Boolean ;
                                    const _renderer   : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure FillRealWorldUnits( const _field      : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure FillAggregation   ;

    public
      /// <summary>
      ///   Property with fields to be used in field factor form.
      /// </summary>
      property Fields             : TStringList
                                    read  fget_Fields
                                    write fset_Fields ;
  end;

  /// <summary>
  ///   PVL color combobox control.
  /// </summary>
  TGIS_PvlColorComboBox = class( TGIS_PvlCustomComboBox, IGIS_PvlColorComboBox )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlColorComboBox ;       reintroduce;
      property PlatformControl    : IGIS_PvlColorComboBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    public

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure Fill              ( const _field      : Boolean ;
                                    const _renderer   : Boolean
                                  ) ; overload;

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure Fill              ( const _field      : Boolean ;
                                    const _renderer   : Boolean ;
                                    const _values     : TStringList
                                  ) ; overload;
    public
      /// <summary>
      ///   Property with fields to be used in field factor form.
      /// </summary>
      property Fields             : TStringList
                                    read  fget_Fields
                                    write fset_Fields ;
  end;


  /// <summary>
  ///   PVL custom bitmap combobox control.
  /// </summary>
  TGIS_PvlCustomBitmapComboBox = class( TGIS_PvlCustomComboBox,
                                        IGIS_PvlCustomBitmapComboBox
                                      )
    private
      function fget_PlatformControl
                                  : IGIS_PvlCustomBitmapComboBox ; reintroduce;
      property PlatformControl    : IGIS_PvlCustomBitmapComboBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      function  fget_Type         : TGIS_PvlCustomBitmapType ;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      function  fget_GetBitmapEvent
                                  : TGIS_PvlComboBoxHelperGetBitmapEvent;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure fset_GetBitmapEvent(
                                    const _value     : TGIS_PvlComboBoxHelperGetBitmapEvent
                                  ) ;


    public
      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillStyle         ( const _hasSymbol  : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillPattern       ( const _hasSymbol  : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillMarker        ( const _hasSymbol  : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillShield        ;

    public

      /// <summary>
      ///   Property with fields to be used in field factor form.
      /// </summary>
      property Fields             : TStringList
                                    read  fget_Fields
                                    write fset_Fields ;

      /// <summary>
      ///   Property which determins type of custom bitmap
      /// </summary>
      property &Type              : TGIS_PvlCustomBitmapType
                                    read fget_Type ;

    published
      /// <event/>
      /// <summary>
      ///   Event for getting bitmap
      /// </summary>
      property  GetBitmapEvent    : TGIS_PvlComboBoxHelperGetBitmapEvent
                                    read  fget_GetBitmapEvent
                                    write fset_GetBitmapEvent ;
  end;

  /// <summary>
  ///   PVL combobox control.
  /// </summary>
  TGIS_PvlCheckBox = class ( TGIS_PvlControl, IGIS_PvlCheckBox )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlCheckBox ;            reintroduce;
      property PlatformControl    : IGIS_PvlCheckBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc/>
      procedure initControl       ;                               override;

    protected
      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_Checked      : Boolean ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_Checked      ( const _value      : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_Caption      : String ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_Caption      ( const _value      : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_FontSize     : Integer ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  ) ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_FontStyle    : TGIS_FontStyles ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  ) ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_FontFamily   : String ;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_FontFamily   ( const _value      : String
                                  ) ;

    public
      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure SetFocus          ; override ;

    public
      /// <summary>
      ///   If checkbox is checked.
      /// </summary>
      property Checked            : Boolean
                                    read  fget_Checked
                                    write fset_Checked;
      /// <summary>
      ///   Caption of the checkbox.
      /// </summary>
      property Caption           : String
                                   read  fget_Caption
                                   write fset_Caption;
      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle         : TGIS_FontStyles
                                   read  fget_FontStyle
                                   write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      property FontSize          : Integer
                                   read  fget_FontSize
                                   write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily        : String
                                   read  fget_FontFamily
                                   write fset_FontFamily;
    published
      /// <summary>
      ///   On change event of the trackbar
      /// </summary>
      property OnChange           : TGIS_PvlEvent
                                    read  fget_OnChange
                                    write fset_OnChange ;

  end;

  /// <summary>
  ///   PVL radiobutton control.
  /// </summary>
  TGIS_PvlRadioButton = class ( TGIS_PvlControl, IGIS_PvlRadioButton {$IFDEF JAVA}, Comparable<TGIS_PvlRadioButton> {$ENDIF} )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;
    private
      function fget_PlatformControl
                                  : IGIS_PvlRadioButton ;         reintroduce;
      property PlatformControl    : IGIS_PvlRadioButton
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    private
      FGroup                      : String ;

    protected
      /// <inheritdoc/>
      procedure initControl       ; override;

    protected
      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_Checked      : Boolean ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_Checked      ( const _value      : Boolean
                                  ) ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_Group        : String ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_Group        ( const _value      : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_OnChange     : TGIS_PvlEvent ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  ) ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_Caption      : String ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_Caption      ( const _value      : String
                                  ) ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_FontSize     : Integer ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  ) ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_FontStyle    : TGIS_FontStyles ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  ) ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_FontFamily   : String ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_FontFamily   ( const _value      : String
                                  ) ;

    public
      {$IFDEF JAVA}
        /// <summary>
        ///   Comparator for java.
        /// </summary>
        /// <param name="_button">
        ///   Item to compare to.
        /// </param>
        /// <returns>
        ///   Returns 0 if items are equal, 1 if it is bigger, -1
        ///   if its smaller
        /// </returns>
        function compareTo        ( _button           : TGIS_PvlRadioButton
                                  ) : Integer ;
      {$ENDIF}
      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure SetFocus          ; override ;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure UncheckGroup      ( const _button     : TGIS_PvlRadioButton
                                  ) ;
    public
      /// <summary>
      ///   If checkbox is checked.
      /// </summary>
      property Checked            : Boolean
                                    read  fget_Checked
                                    write fset_Checked;
      /// <summary>
      ///   Group of the radio button.
      /// </summary>
      property &Group            : String
                                   read  fget_Group
                                   write fset_Group;
      /// <summary>
      ///   Caption of the checkbox.
      /// </summary>
      property Caption           : String
                                   read  fget_Caption
                                   write fset_Caption;
      /// <summary>
      ///   Font style to be used in the caption.
      /// </summary>
      property FontStyle         : TGIS_FontStyles
                                   read  fget_FontStyle
                                   write fset_FontStyle;
      /// <summary>
      ///   Font size to be used in the caption.
      /// </summary>
      property FontSize          : Integer
                                   read  fget_FontSize
                                   write fset_FontSize;
      /// <summary>
      ///   Font family to be used in the caption.
      /// </summary>
      property FontFamily        : String
                                   read  fget_FontFamily
                                   write fset_FontFamily;
    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange         : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;

  end;

  /// <summary>
  ///   PVL color ramp combo-box control.
  /// </summary>
  TGIS_PvlColorRampComboBox = class ( TGIS_PvlControl, IGIS_PvlColorRampComboBox  )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlColorRampComboBox ;   reintroduce;
      property PlatformControl    : IGIS_PvlColorRampComboBox
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Mode         : TGIS_ColorMapMode;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Mode         ( const _value      : TGIS_ColorMapMode
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Index        : Integer;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Index        ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_ItemCount    : Integer;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>

      function  fget_ColorSchemas : TGIS_ColorSchemas;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_ColorSchemas (
                                    const _value      : TGIS_ColorSchemas
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Reverse      : Boolean ;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Reverse      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_ShowNames    : Boolean ;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_ShowNames    ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

    public

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function Value            ( const _subClass   : Integer = -1
                                ) : TGIS_ColorMapArray;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure Fill;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure Lock;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure Unlock;

    public
      /// <summary>
      ///   ColorMap mode.
      /// </summary>
      property Mode             : TGIS_ColorMapMode
                                  read  fget_Mode
                                  write fset_Mode;

      /// <summary>
      ///   Index of the selected item.
      /// </summary>
      property &Index           : Integer
                                  read  fget_Index
                                  write fset_Index;

      /// <summary>
      ///   Ramp count.
      /// </summary>
      property ItemCount        : Integer
                                  read  fget_ItemCount;

      /// <summary>
      ///   Item accessor.
      /// </summary>
      /// <param name="_idx">
      ///   ID of the item you want to get
      /// </param>
      property Items[ const _idx : Integer ]
                                : String
                                  read  fget_Item;

      /// <summary>
      ///   Color schema filter.
      /// </summary>
      property ColorSchemas     : TGIS_ColorSchemas
                                  read  fget_ColorSchemas
                                  write fset_ColorSchemas;
      /// <summary>
      ///   Reverse colormap.
      /// </summary>
      property &Reverse         : Boolean
                                  read  fget_Reverse
                                  write fset_Reverse;
      /// <summary>
      ///   Show captions for ramps.
      /// </summary>
      property ShowNames        : Boolean
                                  read  fget_ShowNames
                                  write fset_ShowNames;

    published
      /// <event/>
      /// <summary>
      ///   OnChange event accessor.
      /// </summary>
      property OnChange         : TGIS_PvlEvent
                                  read  fget_OnChange
                                  write fset_OnChange;
  end;

  /// <summary>
  ///   PVL TreeNode control.
  /// </summary>
  TGIS_PvlTreeNode = class ( TGIS_PvlBase, IGIS_PvlTreeNode )
    public
      /// <summary>
      ///   Create Node.
      /// </summary>
      /// <param name="_parent">
      ///   parent of the node
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( const _parent     : TGIS_PvlTreeNode
                                  ); overload;
      /// <summary>
      ///   Create Node.
      /// </summary>
      /// <param name="_parent">
      ///   tree control
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( const _parent     : TGIS_PvlTree
                                  ); overload;
    protected
      /// <inherit/>
      procedure   doDestroy       ; override;

    protected
      /// <summary>
      ///   Underlying platform specific control.
      /// </summary>
      oPlatform                   : IGIS_PvlBase;

    private
      function fget_PlatformControl
                                  : IGIS_PvlTreeNode ;
      property PlatformControl    : IGIS_PvlTreeNode
                                    read  fget_PlatformControl;

    protected
      /// <summary>
      ///   Underlying platform specific control.
      /// </summary>
      oTreeControl                : TGIS_PvlTree;

      /// <summary>
      ///   List of child nodes
      /// </summary>
      lstNodes                    : TList< TGIS_PvlTreeNode >;

      /// <summary>
      ///   Parent node.
      /// </summary>
      oParent                     : TGIS_PvlTreeNode ;

    protected

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  fget_Expanded     : Boolean;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure fset_Expanded     ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  fget_NativeControl: TObject;
                                    override;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure fset_NativeControl( const _value       : TObject
                                  );

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  fget_Parent       : TGIS_PvlTreeNode;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  fget_Count        : Integer;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  fget_Node         ( const _index      : Integer
                                  ) : TGIS_PvlTreeNode;

    public

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure Clear             ;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  CreateNode        ( const _caption    : String
                                  ) : TGIS_PvlTreeNode; overload;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      function  CreateNode        ( const _caption    : String ;
                                    const _index      : Integer
                                  ) : TGIS_PvlTreeNode; overload;

      /// <summary>
      ///   Remove this node.
      ///   FOR INTERNAL USE ONLY !!!!
      /// </summary>
      procedure InternalRemove        ;

      /// <summary>
      ///   Delete this node from the parent.
      /// </summary>
      procedure DeleteNode        ;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure MoveNode          ( const _newParent  : TGIS_PvlTreeNode ;
                                    const _index      : Integer
                                  ); overload;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure MoveNode          ( const _newParent  : TGIS_PvlTreeNode
                                  ); overload;virtual;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure doCreateNode      ( const _node       : TGIS_PvlTreeNode;
                                    const _caption    : String ;
                                    const _index      : Integer
                                  ) ;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure doRemoveNode      ( const _node       : TGIS_PvlTreeNode
                                  ) ;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure doDeleteNode      ( const _node       : TGIS_PvlTreeNode
                                  ) ;

      /// <inheritdoc from="IGIS_PvlTreeNode"/>
      procedure doMoveNode        ( const _node       : TGIS_PvlTreeNode;
                                    const _newParent  : TGIS_PvlTreeNode ;
                                    const _index      : Integer
                                  );
    public
      /// <summary>
      ///   Node caption.
      /// </summary>
      property Caption            : String
                                    read  fget_Caption
                                    write fset_Caption;

      /// <summary>
      ///   Node parent.
      /// </summary>
      property Parent             : TGIS_PvlTreeNode
                                    read  fget_Parent;

      /// <summary>
      ///   If node is expanded or not.
      /// </summary>
      property Expanded           : Boolean
                                    read  fget_Expanded
                                    write fset_Expanded;

      /// <summary>
      ///   Native platform node object.
      /// </summary>
      property NativeControl      : TObject
                                    read  fget_NativeControl
                                    write fset_NativeControl ; {$IFDEF OXYGENE}reintroduce;{$ENDIF}

      /// <summary>
      ///   Tree control that owns a node.
      /// </summary>
      property TreeControl        : TGIS_PvlTree
                                    read  oTreeControl ;

      /// <summary>
      ///   Number of child nodes.
      /// </summary>
      property Count              : Integer
                                    read  fget_Count ;

      /// <summary>
      ///   Node at the index
      /// </summary>
      /// <param name="_index">
      ///   index of the node
      /// </param>
      /// <returns>
      ///   Node or nil if _index is out of the range.
      /// </returns>
      property Node[ const _index : Integer ]
                                  : TGIS_PvlTreeNode
                                    read  fget_Node ;

      /// <summary>
      ///   List of all children nodes
      /// </summary>
      /// <returns>
      ///   List of all children nodes
      /// </returns>
      property Nodes              : TList<TGIS_PvlTreeNode>
                                    read  lstNodes ;
  end;


  /// <summary>
  ///   PVL Tree control.
  /// </summary>
  TGIS_PvlTree = class ( TGIS_PvlControl, IGIS_PvlTree )
    protected
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  ); override;

    private
      function fget_PlatformControl
                                  : IGIS_PvlTree ;                reintroduce;
      property PlatformControl    : IGIS_PvlTree
                                    read  fget_PlatformControl;   {$IFDEF OXYGENE}override;{$ENDIF}

    protected
      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_Root         : TGIS_PvlTreeNode;

      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_Selected     : TGIS_PvlTreeNode;

      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure fset_Selected     ( const _value      : TGIS_PvlTreeNode
                                  );

      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_SelectedIndex: Integer;

      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_OnSelectChange
                                  : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure fset_OnSelectChange
                                  ( const _value      : TGIS_PvlEvent
                                  );


    public
      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure CreateRoot        ;

    public
      /// <summary>
      ///   Root node.
      /// </summary>
      property Root               : TGIS_PvlTreeNode
                                    read fget_Root;

      /// <summary>
      ///   Selected node.
      /// </summary>
      property Selected           : TGIS_PvlTreeNode
                                    read  fget_Selected
                                    write fset_Selected;

      /// <summary>
      ///   Index of the selected node
      ///   in the parent node.
      /// </summary>
      property SelectedIndex      : Integer
                                    read  fget_SelectedIndex;

    published
      /// <event/>
      /// <summary>
      ///   OnSelectChange event accessor.
      /// </summary>
      property OnSelectChange     : TGIS_PvlEvent
                                    read  fget_OnSelectChange
                                    write fset_OnSelectChange ;
  end;

  /// <summary>
  ///   PVL option dialog control.
  /// </summary>
  TGIS_PvlOptionDialog = class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;

      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute            : TGIS_PvlModalResult; virtual;

    protected
      /// <summary>
      ///   Getter for Text property.
      /// </summary>
      /// <returns>
      ///   Text in the option dialog
      /// </returns>
      function  fget_Text         : String; virtual;

      /// <summary>
      ///   Setter for Text property.
      /// </summary>
      /// <param name="_value">
      ///   New text to be placed in the option dialog
      /// </param>
      procedure fset_Text         ( const _value      : String
                                  ); virtual;
    public

      /// <summary>
      ///   Message of the dialog.
      /// </summary>
      property Text             : String
                                  read  fget_Text
                                  write fset_Text ;
  end;

  /// <summary>
  ///   PVL message dialog control.
  /// </summary>
  TGIS_PvlMessageDialog = class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;

      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      procedure Execute                   ; virtual;

    protected
      /// <summary>
      ///   Getter for Text property.
      /// </summary>
      /// <returns>
      ///   Text in the message dialog
      /// </returns>
      function  fget_Text         : String; virtual;

      /// <summary>
      ///   Setter for Text property.
      /// </summary>
      /// <param name="_value">
      ///   New text to be placed in the message dialog
      /// </param>
      procedure fset_Text         ( const _value      : String
                                  ); virtual;

      /// <summary>
      ///   Getter for Title property.
      /// </summary>
      /// <returns>
      ///   Title in the message dialog
      /// </returns>
      function  fget_Title        : String; virtual;

      /// <summary>
      ///   Setter for Title property.
      /// </summary>
      /// <param name="_value">
      ///   New Title to be placed in the message dialog
      /// </param>
      procedure fset_Title        ( const _value      : String
                                  ); virtual;
    public

      /// <summary>
      ///   Message of the dialog.
      /// </summary>
      property Text             : String
                                  read  fget_Text
                                  write fset_Text ;

      /// <summary>
      ///   Title of the dialog.
      /// </summary>
      property Title            : String
                                  read  fget_Title
                                  write fset_Title ;
  end;

  /// <summary>
  ///   PVL info dialog control.
  /// </summary>
  TGIS_PvlInfoDialog = class ( TGIS_PvlMessageDialog )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
  end;

  /// <summary>
  ///   PVL warning dialog control.
  /// </summary>
  TGIS_PvlWarningDialog = class ( TGIS_PvlMessageDialog )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
  end;

  /// <summary>
  ///   PVL error dialog control.
  /// </summary>
  TGIS_PvlErrorDialog = class ( TGIS_PvlMessageDialog )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
  end;

  /// <summary>
  ///   Class for static execution of the message dialogs in PVL
  /// </summary>
  TGIS_PvlMessages = class
    public

      /// <summary>
      ///   Static procedure to show option dialog.
      /// </summary>
      /// <param name="_message">
      ///   Message to be shown in the error dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      class function  ShowOption( const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                ) : TGIS_PvlModalResult ;  overload ;

      /// <summary>
      ///   Static procedure to show info dialog.
      /// </summary>
      /// <param name="_title">
      ///   Title of the info dialog
      /// </param>
      /// <param name="_message">
      ///   Message to be shown in the info dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     Deprecated. Use the overloaded version instead.
      ///   </note>
      /// </remarks>
      class procedure ShowInfo  ( const _title      : String ;
                                  const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                ); overload ;

      /// <summary>
      ///   Static procedure to show info dialog.
      /// </summary>
      /// <param name="_message">
      ///   Message to be shown in the info dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      class procedure ShowInfo  ( const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                ); overload ;

      /// <summary>
      ///   Static procedure to show error dialog.
      /// </summary>
      /// <param name="_title">
      ///   Title of the error dialog
      /// </param>
      /// <param name="_message">
      ///   Message to be shown in the error dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     Deprecated. Use the overloaded version instead.
      ///   </note>
      /// </remarks>
      class procedure ShowError ( const _title      : String ;
                                  const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                );  overload ;

      /// <summary>
      ///   Static procedure to show error dialog.
      /// </summary>
      /// <param name="_message">
      ///   Message to be shown in the error dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      class procedure ShowError ( const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                );  overload ;

      /// <summary>
      ///   Static procedure to show warning dialog.
      /// </summary>
      /// <param name="_title">
      ///   Title of the warning dialog
      /// </param>
      /// <param name="_message">
      ///   Message to be shown in the warning dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     Deprecated. Use the overloaded version instead.
      ///   </note>
      /// </remarks>
      class procedure ShowWarning
                                ( const _title      : String ;
                                  const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                );  overload ;

      /// <summary>
      ///   Static procedure to show warning dialog.
      /// </summary>
      /// <param name="_message">
      ///   Message to be shown in the warning dialog
      /// </param>
      /// <param name="_context">
      ///   Context on which dialog is supposed to be shown
      /// </param>
      class procedure ShowWarning
                                ( const _message    : String ;
                                  const _context    : TGIS_PvlContext
                                );  overload ;
  end;

  /// <summary>
  ///   PVL open dialog control.
  /// </summary>
  TGIS_PvlOpenSaveDialog = {$IFDEF OXYGENE} abstract {$ENDIF} class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
    protected

      /// <summary>
      ///   Getter for Filter property.
      /// </summary>
      /// <returns>
      ///   String representation of filter
      /// </returns>
      function  fget_Filter     : String; virtual;

      /// <summary>
      ///   Setter for Filter property.
      /// </summary>
      /// <param name="_value">
      ///   String representation of filter
      /// </param>
      procedure fset_Filter     ( const _value      : String
                                ); virtual;

      /// <summary>
      ///   Getter for FileName property.
      /// </summary>
      /// <returns>
      ///   Selected file name
      /// </returns>
      function  fget_FileName   : String; virtual;

      /// <summary>
      ///   Setter for FileName property.
      /// </summary>
      /// <param name="_value">
      ///   New value for file name
      /// </param>
      procedure fset_FileName   ( const _value      : String
                                ); virtual;

    public
      /// <summary>
      ///   String to filter out extenstions.
      /// </summary>
      property Filter           : String
                                  read  fget_Filter
                                  write fset_Filter;
      /// <summary>
      ///   Selected file name.
      /// </summary>
      property FileName         : String
                                  read  fget_FileName
                                  write fset_FileName;
    public
      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute          : TGIS_PvlModalResult; virtual;
  end;

  /// <summary>
  ///   PVL open dialog control.
  /// </summary>
  TGIS_PvlOpenDialog = class ( TGIS_PvlOpenSaveDialog ) ;

  /// <summary>
  ///   PVL save dialog control.
  /// </summary>
  TGIS_PvlSaveDialog = class ( TGIS_PvlOpenSaveDialog ) ;

  /// <summary>
  ///   PVL select folder dialog control.
  /// </summary>
  TGIS_PvlSelectFolderDialog = class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;

    protected
      /// <summary>
      ///   Getter for Directory property.
      /// </summary>
      /// <returns>
      ///   Selected directory path
      /// </returns>
      function  fget_Directory  : String; virtual;

      /// <summary>
      ///   Setter for Directory property.
      /// </summary>
      /// <param name="_value">
      ///   New value for Directory path
      /// </param>
      procedure fset_Directory  ( const _value      : String
                                ); virtual;

    public

      /// <summary>
      ///   String with directory path.
      ///   If set before execute the dialog
      ///   is going to start in that directory.
      /// </summary>
      property Directory        : String
                                read  fget_Directory
                                write fset_Directory;

    public
      /// <summary>
      ///   Invoke Dialog Form and execute it.
      /// </summary>
      /// <returns>
      ///   Returns result of the Execute operation
      /// </returns>
      function Execute          : TGIS_PvlModalResult; virtual;
  end;

  /// <summary>
  ///   PVL proxy for TGIS_ControlPrintPreview.
  /// </summary>
  TGIS_PvlControlPrintPreviewSimple = class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
    protected

      /// <summary>
      ///   Getter for GIS_Viewer property.
      /// </summary>
      /// <returns>
      ///   GIS viewer object
      /// </returns>
      function  fget_GIS_Viewer   : IGIS_Viewer ; virtual;

      /// <summary>
      ///   Setter for GIS_Viewer property.
      /// </summary>
      /// <param name="_GIS">
      ///   New GIS_Viewer value
      /// </param>
      procedure fset_GIS_Viewer   ( const _GIS : IGIS_Viewer
                                  ) ; virtual;

    public
      /// <summary>
      ///   Viewer to which Print Preview will be attached.
      /// </summary>
      property GIS_Viewer : IGIS_Viewer  read  fget_GIS_Viewer
                                         write fset_GIS_Viewer ;

    public

      /// <summary>
      ///   Execute print preview with default layout.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview       ; overload ; virtual;

      /// <summary>
      ///   Execute print preview with default layout.
      /// </summary>
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
      procedure Preview       ( var   _scale        : Double
                              ) ; overload ; virtual;

      /// <summary>
      ///   Execute print preview with layout defined by print manager object.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Printing fails when GIS_Viewer.InPaint is set to True.
      ///   </para>
      /// </remarks>
      procedure Preview       ( const _printManager : TGIS_PrintManagerAbstract
                              ) ; overload ; virtual;

      /// <summary>
      ///   Execute print preview with layout defined by print manager object.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
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
      procedure Preview       ( const _printManager : TGIS_PrintManagerAbstract ;
                                var   _scale        : Double
                              ) ; overload ; virtual;

      /// <summary>
      ///   Execute print preview with layout defined by print manager object.
      /// </summary>
      /// <param name="_printManager">
      ///   print manager object
      /// </param>
      /// <param name="_customPage">
      ///   custom page format;
      ///   see TGIS_ControlPrintPreview.Preview for more information
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
      procedure Preview       ( const _printManager : TGIS_PrintManagerAbstract ;
                                const _customPage   : String ;
                                var   _scale        : Double
                              ) ; overload ; virtual;
  end;

  /// <summary>
  ///   PVL proxy for TGIS_LineSymbolEditor.
  /// </summary>
  TGIS_PvlLineSymbolEditor = class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
    protected

      /// <summary>
      ///   Getter for Symbol property.
      /// </summary>
      /// <returns>
      ///   Symbol of the control
      /// </returns>
      function  fget_Symbol       : String ;                            virtual;

      /// <summary>
      ///   Setter for Symbol property.
      /// </summary>
      /// <param name="_symbol">
      ///   New Symbol value
      /// </param>
      procedure fset_Symbol       ( const _symbol     : String
                                  ) ;                                   virtual;
    public

      /// <summary>
      ///   Execute dialog on a line symbol given by path.
      /// </summary>
      /// <param name="_path">
      ///   path to the symbol.
      /// </param>
      /// <param name="_proc">
      ///   Action to be taken upon closing modal
      /// </param>
      /// <returns>
      ///   Dialog result on a given by path symbol
      /// </returns>
      function  Execute           ( const _path       : String ;
                                    const _proc       : TGIS_Proc
                                  ) : TGIS_PvlModalResult ;   overload; virtual;

      /// <summary>
      ///   Execute dialog on a line symbol given by path.
      /// </summary>
      /// <param name="_path">
      ///   path to the symbol.
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   Action to be taken upon closing modal
      /// </param>
      /// <returns>
      ///   Dialog result on a given by path symbol
      /// </returns>
      function  Execute           ( const _path       : String ;
                                    const _onhelp     : TGIS_HelpEvent ;
                                    const _proc       : TGIS_Proc
                                  ): TGIS_PvlModalResult ;    overload; virtual;

    public

      /// <summary>
      ///   Symbol defining string.
      /// </summary>
      property Symbol : String
                        read  fget_Symbol
                        write fset_Symbol ;
  end;

  /// <summary>
  ///   PVL proxy for TGIS_ControlCSSystem.
  /// </summary>
  TGIS_PvlControlCSSystem = class ( TGIS_PvlSystemForm )
    public

      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext
                                ); override;
      /// <inheritdoc/>
      constructor Create        (       _context    : TGIS_PvlContext ;
                                  const _self       : TGIS_PvlSystemForm
                                ); override;
    protected
      /// <summary>
      ///   Getter for CS property.
      /// </summary>
      /// <returns>
      ///   CS of the control
      /// </returns>
      function fget_CS           : TGIS_CSCoordinateSystem ; virtual ;

    public
      /// <summary>
      ///   Execute dialog on a given Coordinate System.
      /// </summary>
      /// <param name="_cs">
      ///   Coordinate System.
      /// </param>
      /// <param name="_proc">
      ///   Action to be taken upon closing modal
      /// </param>
      /// <returns>
      ///   Dialog result on a given coordinate system
      /// </returns>
      function Execute           ( const _cs         : TGIS_CSCoordinateSystem ;
                                   const _proc       : TGIS_Proc
                                 ) : TGIS_PvlModalResult ; overload ; virtual ;

      /// <summary>
      ///   Execute dialog on a given Coordinate System.
      /// </summary>
      /// <param name="_cs">
      ///   Coordinate System.
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   Action to be taken upon closing modal
      /// </param>
      /// <returns>
      ///   Dialog result on a given coordinate system
      /// </returns>
      function Execute           ( const _cs         : TGIS_CSCoordinateSystem ;
                                   const _onhelp     : TGIS_HelpEvent ;
                                   const _proc       : TGIS_Proc
                                 ) : TGIS_PvlModalResult ; overload ; virtual ;

      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_area_epsg">
      ///   epsg area specification;
      ///   <list type="bullet">
      ///     <item>
      ///       if _area_epsg=0 then systems matching _area_extent will be
      ///       presented, but also systems without specified area (AreaEPSG=0)
      ///     </item>
      ///     <item>
      ///       if _ara_epsg=1 then systems matching _area_extent will be
      ///       presented, but list will be limited to systems with specified
      ///       area (AreaEPSG>1)
      ///     </item>
      ///     <item>
      ///       if _area_epsg represents Area EPSG then only systems matching specified
      ///       area willl be presneted; _area_extent is not used in this case
      ///     </item>
      ///   </list>
      /// </param>
      /// <param name="_area_extent">
      ///   if _area_epsg is 0 or 1 then extent will be used to present systems with
      ///   matching area of use
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   Action to be taken upon closing modal
      /// </param>
      /// <returns>
      ///   Dialog result on a given coordinate system
      /// </returns>
      function Execute           ( const _cs         : TGIS_CSCoordinateSystem;
                                   const _area_epsg  : Integer ;
                                   const _area_extent: TGIS_Extent ;
                                   const _onhelp     : TGIS_HelpEvent ;
                                   const _proc       : TGIS_Proc
                                 ) : TGIS_PvlModalResult ; overload ; virtual ;

    public

      /// <summary>
      ///   Coordinate System assigned to the viewer.
      /// </summary>
      property CS                 : TGIS_CSCoordinateSystem
                                    read fget_CS ;
  end;

  /// <summary>
  ///   class of TGIS_PvlContextBase.
  /// </summary>
  TGIS_PvlContextBaseClass = class of TGIS_PvlContextBase;

  IGIS_PvlContext = interface
    {$IFDEF DCC}
      ['{ACA45901-4348-4CE2-B3E9-F9CA99A0B039}']
    {$ENDIF}

    /// <summary>
    ///   Create PVL Object.
    /// </summary>
    /// <param name="_parent">
    ///   Parent object.
    /// </param>
    /// <param name="_name">
    ///   Name of the class object.
    /// </param>
    /// <returns>
    ///   Created object.
    /// </returns>
    function CreateObject       ( const _parent     : TGIS_PvlBase;
                                  const _name       : String
                                ) : IGIS_PvlBase ; overload;

    /// <summary>
    ///   A safe way to free context.
    /// </summary>
    procedure FreeContext ;

    /// <summary>
    ///   Getter for Horizontal Margin.
    /// </summary>
    /// <returns>
    ///   Horizontal margin
    /// </returns>
    function  fget_HMargin      : Integer;

    /// <summary>
    ///   Setter for Horizontal Margin property.
    /// </summary>
    /// <param name="_value">
    ///   New Horizontal Margin value
    /// </param>
    procedure fset_HMargin      ( const _value : Integer
                                ) ;

    /// <summary>
    ///   Getter for Vertical Margin.
    /// </summary>
    /// <returns>
    ///   Vertical margin
    /// </returns>
    function  fget_VMargin      : Integer;

    /// <summary>
    ///   Setter for Vertical Margin property.
    /// </summary>
    /// <param name="_value">
    ///   New Vertical Margin value
    /// </param>
    procedure fset_VMargin      ( const _value : Integer
                                ) ;

    /// <summary>
    ///   Getter for Horizontal Space.
    /// </summary>
    /// <returns>
    ///   Horizontal space
    /// </returns>
    function  fget_HSpace       : Integer;

    /// <summary>
    ///   Getter for Vertical Space.
    /// </summary>
    /// <returns>
    ///   Vertical space
    /// </returns>
    function  fget_VSpace       : Integer;

    /// <summary>
    ///   Getter for Label Space.
    /// </summary>
    /// <returns>
    ///   Vertical space
    /// </returns>
    function  fget_LSpace       : Integer;

    /// <summary>
    ///   Getter for CanvasScale.
    /// </summary>
    /// <returns>
    ///   Canvas scale; 1 = 100%
    /// </returns>
    function  fget_CanvasScale  : Single;

    /// <summary>
    ///   Getter for PPIFix.
    /// </summary>
    /// <returns>
    ///   PPIFix mutilier
    /// </returns>
    function  fget_PPIFix       : Single;

    /// <summary>
    ///   Getter for PPI.
    /// </summary>
    /// <returns>
    ///   PPI value
    /// </returns>
    function  fget_PPI : Integer;

    /// <summary>
    ///   Gets the width of the client area.
    /// </summary>
    /// <returns>
    ///   Width of the client area
    /// </returns>
    function  ClientWidth      : Integer;

    /// <summary>
    ///   Function to get all available and supported fonts for the platform.
    /// </summary>
    /// <returns>
    ///   List of fonts names
    /// </returns>
    function  GetAllFonts     : TStringList;
  end;

  /// <summary>
  ///   Platform dependant context in which we operate.
  /// </summary>
  TGIS_PvlContextBase = class( {$IFDEF DCC}TGIS_UncountedInterfacedObject {$ELSE} TGIS_ObjectDisposable {$ENDIF} )
    public
      /// <summary>
      ///   Check if object is supported by current context platform.
      /// </summary>
      /// <param name="_parent">
      ///   Object to be checked
      /// </param>
      /// <returns>
      ///   Returns True if supported
      /// </returns>
      class function   Support  ( const _parent     : TObject
                                ) : Boolean;                  virtual;
      /// <summary>
      ///   Construct context.
      /// </summary>
      /// <param name="_parent">
      ///   Parent of the context
      /// </param>
      /// <param name="_ppi">
      ///   PPI parameter.
      /// </param>
      /// <param name="_ppifix">
      ///   Fix between HiRes and LoRes form size/pos calculations.
      /// </param>
      /// <param name="_canvasscale">
      ///   Canvas scale for proper rendering.
      /// </param>
      /// <param name="_righttoleft">
      ///   BiDi parameter to save RightToLeft pReferences to the context.
      /// </param>
      /// <param name="_context">
      ///   Context
      /// </param>
      constructor Create        ( const _parent     : TObject;
                                  const _ppi        : Integer;
                                  const _ppifix     : Single;
                                  const _canvasscale: Single;
                                  const _righttoleft: Boolean;
                                  const _context    : TGIS_PvlContext
                                );                            overload; virtual;

      /// <summary>
      ///   Construct context.
      /// </summary>
      /// <param name="_parent">
      ///   parent of the context; form or a parent control
      /// </param>
      /// <param name="_context">
      ///   Context
      /// </param>
      /// <param name="_context">
      ///   Context
      /// </param>
      constructor Create        ( const _parent     : TObject ;
                                  const _context    : TGIS_PvlContext
                                ) ;                           overload; virtual;

      /// <summary>
      ///   Construct context.
      /// </summary>
      /// <param name="_parent">
      ///   parent of the context; form or a parent control
      /// </param>
      /// <param name="_context">
      ///   Context
      /// </param>
      /// <param name="_refcontext">
      ///   Context used to create this one
      /// </param>
      constructor Create        ( const _parent     : TObject ;
                                  const _context    : TGIS_PvlContext;
                                  const _refcontext : TGIS_PvlContext
                                ) ;                           overload; virtual;

      /// <summary>
      ///   Create platform independent open dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created open dialog
      /// </returns>
      class function  CreateOpenDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlOpenDialog
                                ) : TGIS_PvlOpenDialog;                 virtual;

      /// <summary>
      ///   Create platform independent save dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created save dialog
      /// </returns>
      class function  CreateSaveDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlSaveDialog
                                ) : TGIS_PvlSaveDialog;                 virtual;

      /// <summary>
      ///   Create platform independent select folder dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created select folder dialog
      /// </returns>
      class function  CreateSelectFolderDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlSelectFolderDialog
                                ) : TGIS_PvlSelectFolderDialog;         virtual;

      /// <summary>
      ///   Create platform independent option dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created option dialog
      /// </returns>
      class function  CreateOptionDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlOptionDialog
                                ) : TGIS_PvlOptionDialog;               virtual;

      /// <summary>
      ///   Create platform independent info dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created info dialog
      /// </returns>
      class function  CreateInfoDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlInfoDialog
                                ) : TGIS_PvlInfoDialog;                 virtual;

      /// <summary>
      ///   Create platform independent warning dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created warning dialog
      /// </returns>
      class function  CreateWarningDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlWarningDialog
                                ) : TGIS_PvlWarningDialog;              virtual;

      /// <summary>
      ///   Create platform independent error dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created error dialog
      /// </returns>
      class function  CreateErrorDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlErrorDialog
                                ) : TGIS_PvlErrorDialog;                virtual;

      /// <summary>
      ///   Create platform independent CS dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created CS dialog
      /// </returns>
      class function  CreateCSDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlControlCSSystem
                                ) : TGIS_PvlControlCSSystem;            virtual;

      /// <summary>
      ///   Create platform independent Line Symbology dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created Line Symbology dialog
      /// </returns>
      class function  CreateLineSymbologyDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlLineSymbolEditor
                                ) : TGIS_PvlLineSymbolEditor;           virtual;

      /// <summary>
      ///   Create platform independent Line Symbology dialog.
      /// </summary>
      /// <param name="_context">
      ///   Parent context
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   Created Line Symbology dialog
      /// </returns>
      class function  CreatePrintPreviewSimpleDialog(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlControlPrintPreviewSimple
                                ) : TGIS_PvlControlPrintPreviewSimple;
                                                                        virtual;
  end;

  /// <summary>
  ///   Platform dependant context in which we operate.
  /// </summary>
  TGIS_PvlContext = class( TGIS_PvlContextBase, IGIS_PvlContext )
    public

      /// <summary>
      ///   Delete self if true.
      /// </summary>
      SelfDelete : Boolean ;

    protected

      /// <summary>
      ///   Platform context instance.
      /// </summary>
      oPlatform : IGIS_PvlContext ;

      /// <summary>
      ///   Master of the context class.
      /// </summary>
      oMaster : TGIS_PvlContext ;

      /// <summary>
      ///   List of the context controls.
      /// </summary>
      oList : TGIS_PvlListOfElements;

      /// <summary>
      ///   Parent of the context class.
      /// </summary>
      oNativeParent : TObject;

      /// <summary>
      ///   Parent of the context class.
      /// </summary>
      oParent : TGIS_PvlBase;

      /// <summary>
      ///   Fix to match design units to actual form PPI like required in VCL.
      /// </summary>
      dPPIFix : Single;

      /// <summary>
      ///   Canvas scale factor to create overrsized bitmaps like in FMX.
      /// </summary>
      dCanvasScale : Single ;

      /// <summary>
      ///   BiDi mode, Right to left if True.
      /// </summary>
      bRightToLeft : Boolean;

      /// <summary>
      ///   PPI value.
      /// </summary>
      iPPI  : Integer;

      /// <summary>
      ///   RadioButton groups within the context.
      /// </summary>
      oGroups                     : TDictionary< TGIS_PvlRadioButton, String >;


    protected
      /// <inheritdoc from="IGIS_PvlContext"/>
      function CreateObject       ( const _parent     : TGIS_PvlBase ;
                                    const _name       : String
                                  ) : IGIS_PvlBase ; overload; virtual;
    public
      /// <inheritdoc from="IGIS_PvlContext"/>
      procedure FreeContext ;

    public
      {#gendoc:hide}
      constructor Create        ( const _parent     : TObject;
                                  const _ppi        : Integer;
                                  const _ppifix     : Single;
                                  const _canvasscale: Single;
                                  const _righttoleft: Boolean
                                ); overload; virtual;

      {#gendoc:hide}
      constructor Create        ( const _parent     : TObject
                                ) ; overload; virtual;

      {#gendoc:hide}
      constructor Create        ( const _parent     : TObject ;
                                  const _context    : TGIS_PvlContext
                                ) ; overload; override;

      {#gendoc:hide}
      constructor Create        ; overload;

    protected
      procedure   doDestroy;    override;
    public
      /// <summary>
      ///   Refresh the context.
      /// </summary>
      /// <returns>
      ///   True if context has been refreshed.
      /// </returns>
      function    Refresh       : Boolean;
    public
      /// <summary>
      ///   Register context.
      /// </summary>
      /// <param name="_context">
      ///   Context class
      /// </param>
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      class procedure  &Register(
                                const _context      : TGIS_PvlContextBaseClass
                              );

      /// <summary>
      ///   Initialize ModalForm.
      /// </summary>
      /// <param name="_context">
      ///   Parent context for the modal form
      /// </param>
      /// <param name="_self">
      ///   Context
      /// </param>
      /// <returns>
      ///   New form
      /// </returns>
      class function  InitializeSystemForm(
                                  const _context    : TGIS_PvlContext;
                                  const _self       : TGIS_PvlSystemForm
                                ) : TGIS_PvlSystemForm;

      /// <inheritdoc from="IGIS_PvlContext"/>
      function  GetAllFonts     : TStringList; virtual;

      /// <summary>
      ///   Update PPI within this context
      /// </summary>
      /// <param name="_ppi">
      ///   New PPI.
      /// </param>
      /// <param name="_ppiFix">
      ///   New PPIFix.
      /// </param>
      /// <param name="_canvasScale">
      ///   New CanvasScale.
      /// </param>
      procedure UpdatePPI( const _ppi : Integer ;
                           const _ppiFix : Single ;
                           const _canvasScale : Single
                         ) ;

      /// <summary>
      ///   Update RightToLeft within this context
      /// </summary>
      /// <param name="_rightToLeft">
      ///   New RightToLeft.
      /// </param>
      procedure UpdateRightToLeft( const _rightToLeft : Boolean
                         ) ;

      /// <summary>
      ///   Redraws all elements which are created on this context.
      /// </summary>
      procedure Redraw ;

    protected
      /// <summary>
      ///   Gets helper component.
      /// </summary>
      /// <returns>
      ///   Helper component
      /// </returns>
      function  CreateHelper         : TGIS_PvlControl; virtual;

    protected

      /// <summary>
      ///   Getter for Horizontal Margin.
      /// </summary>
      /// <returns>
      ///   Horizontal margin
      /// </returns>
      function  fget_HMargin    : Integer;

      /// <summary>
      ///   Setter for Horizontal Margin property.
      /// </summary>
      /// <param name="_value">
      ///   New Horizontal Margin value
      /// </param>
      procedure fset_HMargin      ( const _value : Integer
                                  ) ;

      /// <summary>
      ///   Getter for Vertical Margin.
      /// </summary>
      /// <returns>
      ///   Vertical margin
      /// </returns>
      function  fget_VMargin      : Integer;

      /// <summary>
      ///   Setter for Vertical Margin property.
      /// </summary>
      /// <param name="_value">
      ///   New Vertical Margin value
      /// </param>
      procedure fset_VMargin      ( const _value : Integer
                                  ) ;

      /// <summary>
      ///   Getter for Horizontal Space.
      /// </summary>
      /// <returns>
      ///   Horizontal space
      /// </returns>
      function  fget_HSpace     : Integer;

      /// <summary>
      ///   Getter for Vertical Space.
      /// </summary>
      /// <returns>
      ///   Vertical space
      /// </returns>
      function  fget_VSpace     : Integer;

      /// <summary>
      ///   Getter for Label Space.
      /// </summary>
      /// <returns>
      ///   Vertical space
      /// </returns>
      function  fget_LSpace     : Integer;

      /// <summary>
      ///   Getter for CanvasScale.
      /// </summary>
      /// <returns>
      ///   Canvas scale; 1 = 100%
      /// </returns>
      function  fget_CanvasScale : Single;

      /// <summary>
      ///   Getter for PPIFix.
      /// </summary>
      /// <returns>
      ///   PPIFix mutilier
      /// </returns>
      function  fget_PPIFix : Single;

      /// <summary>
      ///   Getter for PPI.
      /// </summary>
      /// <returns>
      ///   PPI value
      /// </returns>
      function  fget_PPI : Integer;

      /// <summary>
      ///   Getter for Controls.
      /// </summary>
      /// <returns>
      ///   PVL controls.
      /// </returns>
      function  fget_Controls : TGIS_PvlListOfElements ;
    public
      /// <summary>
      ///   Parent of the context class.
      /// </summary>
      property NativeParent     : TObject
                                  read oNativeParent ;

      /// <summary>
      ///   Parent of the context class.
      /// </summary>
      property Parent           : TGIS_PvlBase
                                  read oParent ;

      /// <summary>
      ///   Underlying platform specific context.
      /// </summary>
      property &Platform        : IGIS_PvlContext
                                  read  oPlatform;

      /// <summary>
      ///   Canvas scale factor to create overrsized bitmaps like in FMX.
      /// </summary>
      property CanvasScale      : Single
                                  read fget_CanvasScale;

      /// <summary>
      ///   Fix to match design units to actual form PPI like required in VCL.
      /// </summary>
      property PPIFix           : Single
                                  read fget_PPIFix;

      /// <summary>
      ///   Context Pixels Per Inch.
      /// </summary>
      property PPI              : Integer
                                  read fget_PPI;

      /// <summary>
      ///   BiDi mode, Right to left if True.
      /// </summary>
      property RightToLeft      : Boolean
                                  read bRightToLeft;

      /// <summary>
      ///   Horizontal margin.
      /// </summary>
      property HMargin          : Integer
                                  read  fget_HMargin
                                  write fset_HMargin ;
      /// <summary>
      ///   Vertical margin.
      /// </summary>
      property VMargin          : Integer
                                  read  fget_VMargin
                                  write fset_VMargin ;
      /// <summary>
      ///   Horizontal space between controls.
      /// </summary>
      property HSpace           : Integer
                                  read fget_HSpace;
      /// <summary>
      ///   Vertical space between controls.
      /// </summary>
      property VSpace           : Integer
                                  read fget_VSpace;

      /// <summary>
      ///   Horizontal space between labels and controls.
      /// </summary>
      property LSpace           : Integer
                                  read fget_LSpace;

      /// <summary>
      ///   List of PVL controls which belongs to this context.
      /// </summary>
      property Controls         : TGIS_PvlListOfElements
                                  read fget_Controls;

      /// <inheritdoc from="IGIS_PvlContext"/>
      function  ClientWidth     : Integer;
  end ;



/// <summary>
///   Prepares text to be added and customly rendered combo box.
/// </summary>
/// <param name="_custom">
///   If custom parameter or plain color.
/// </param>
/// <param name="_class">
///   Class of the element.
/// </param>
/// <param name="_position">
///   Position of the element.
/// </param>
/// <param name="_caption">
///   Caption of the color
/// </param>
/// <param name="_value">
///   Value of the color.
/// </param>
/// <returns>
///   Newly constructed string.
/// </returns>
function PrepareComboBoxItem(
  const _custom   : Boolean                    ;
  const _class    : Char                       ;
  const _position : TGIS_PvlComboBoxHelperPosition ;
  const _caption  : String                     ;
  const _value    : String
) : String ;

/// <summary>
///   Prepare colors list for the comboboxes
/// </summary>
/// <returns>
///   List of colors
/// </returns>
function PrepareComboBoxColorsList
  : TStringList ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    {$IFDEF FRAMEWORK_VCL}
      VCL.AllPvl,
    {$ENDIF}
    {$IFDEF FRAMEWORK_FMX}
      FMX.AllPvl,
    {$ENDIF}
    PVL.GisPvlForms;
{$ENDIF}


const
  COMBO_HELPER_TYPE_SELECTABLE   =  '#' ;
  COMBO_HELPER_TYPE_CUSTOMACTION =  '@' ;
  COMBO_HELPER_POS_TOP           =  'T' ;
  COMBO_HELPER_POS_LRU           =  'L' ;
  COMBO_HELPER_POS_CUSTOM        =  'C' ;
  COMBO_HELPER_POS_BOTTOM        =  'B' ;

  GIS_COLOR_HUE_SIZE         : Integer = 1530 ;

  GIS_COLOR_WHEEL_MARGIN     : Integer = 14 ;
  GIS_COLOR_WHEEL_MARGIN2    : Integer = 7 ;

  GIS_COLOR_GRADIENT_MARGIN  : Integer = 8 ;
  GIS_COLOR_GRADIENT_MARGIN2 : Integer = 4 ;

var
  lstFrameworks : {$IFDEF DCC}
                    TList<TGIS_PvlContextBaseClass> ;
                  {$ELSEIF JAVA}
                    java.util.ArrayList<TGIS_PvlContextBaseClass> ;
                  {$ELSEIF CLR}
                    System.Collections.Generic.List<TGIS_PvlContextBaseClass>;
                  {$ENDIF}


function PrepareComboBoxItem(
  const _custom   : Boolean                    ;
  const _class    : Char                       ;
  const _position : TGIS_PvlComboBoxHelperPosition ;
  const _caption  : String                     ;
  const _value    : String
) : String ;
var
  bld : TStringBuilder ;
begin
  bld := TStringBuilder.Create ;
  try
    case _custom of
      True  : bld.Append( COMBO_HELPER_TYPE_CUSTOMACTION ) ;
      False : bld.Append( COMBO_HELPER_TYPE_SELECTABLE   ) ;
    end ;

    bld.Append( _class ) ;

    case _position of
      TGIS_PvlComboBoxHelperPosition.Top :
        bld.Append( COMBO_HELPER_POS_TOP    ) ;
      TGIS_PvlComboBoxHelperPosition.Lru     :
        bld.Append( COMBO_HELPER_POS_LRU    ) ;
      TGIS_PvlComboBoxHelperPosition.Custom  :
        bld.Append( COMBO_HELPER_POS_CUSTOM ) ;
      TGIS_PvlComboBoxHelperPosition.Bottom  :
        bld.Append( COMBO_HELPER_POS_BOTTOM ) ;
    end ;

    bld.Append( '|' ) ;
    bld.Append( _caption ) ;
    bld.Append( '|' ) ;
    bld.Append( _value ) ;

    Result := bld.ToString ;
  finally
    FreeObject( bld ) ;
  end ;
end ;

function PrepareComboBoxColorsList
  : TStringList ;
begin
  Result := TStringList.create ;
  Result.Add( 'FF000000' ) ;
  Result.Add( 'FF000080' ) ;
  Result.Add( 'FF0000FF' ) ;
  Result.Add( 'FF008000' ) ;
  Result.Add( 'FF00FF00' ) ;
  Result.Add( 'FF00FFFF' ) ;
  Result.Add( 'FF6BAED6' ) ;
  Result.Add( 'FF74C476' ) ;
  Result.Add( 'FF800000' ) ;
  Result.Add( 'FF808000' ) ;
  Result.Add( 'FF808080' ) ;
  Result.Add( 'FF9E9AC8' ) ;
  Result.Add( 'FFA4A0A0' ) ;
  Result.Add( 'FFA63603' ) ;
  Result.Add( 'FFB3CDE3' ) ;
  Result.Add( 'FFC0C0C0' ) ;
  Result.Add( 'FFC0C0FF' ) ;
  Result.Add( 'FFC0DCC0' ) ;
  Result.Add( 'FFC0FFC0' ) ;
  Result.Add( 'FFDF65B0' ) ;
  Result.Add( 'FFF0CAA6' ) ;
  Result.Add( 'FFFDBE85' ) ;
  Result.Add( 'FFFEEBE2' ) ;
  Result.Add( 'FFFF0000' ) ;
  Result.Add( 'FFFF00FF' ) ;
  Result.Add( 'FFFFC0C0' ) ;
  Result.Add( 'FFFFFF00' ) ;
  Result.Add( 'FFFFFFC0' ) ;
  Result.Add( 'FFFFFFFF' ) ;
end ;

{$REGION 'TGIS_PvlBase'}

  function TGIS_PvlBase.fget_NativeControl
    : TObject ;
  begin
    Result := oPlatform.fget_NativeControl ;
  end;

{$ENDREGION 'TGIS_PvlBase'}

{$REGION 'TGIS_PvlObject'}

constructor TGIS_PvlObject.Create(
  const _context : TGIS_PvlContext
)  ;
begin
  oContext := _context ;
  oContextLocal := nil ;

  doCreate( _context ) ;
end;

procedure TGIS_PvlObject.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  // for safe inheritance only
end ;

procedure TGIS_PvlObject.doDestroy ;
var
  tmp : TObject ;
begin
  tmp := oPlatform as TObject;
  FreeObject( tmp ) ;
  ReleaseInterface( oPlatform ) ;
  inherited ;
end;

procedure TGIS_PvlControl.initControl ;
begin
  // for safe inheritance only
end;

procedure TGIS_PvlObject.fset_Platform(
  const _value : IGIS_PvlBase
);
begin
  {$IFDEF GIS_PDK}
    if _value <> oPlatform then
      UnSubscribeAll ;
  {$ENDIF}

  oPlatform := _value ; ;
end;

function TGIS_PvlObject.fget_PlatformControl
  : IGIS_PvlControl ;
begin
  Result := oPlatform as IGIS_PvlControl ;
end;

function TGIS_PvlObject.fget_NativeControl
  : TObject ;
begin
  Result := PlatformControl.fget_NativeControl ;
end;
{$ENDREGION 'TGIS_PvlObject'}

{$REGION 'TGIS_PvlControl'}

constructor TGIS_PvlControl.Create(
  const _context : TGIS_PvlContext
)  ;
begin
  inherited Create( _context );

  _context.oList.Add( self ) ;

  bVisible := True ;
  bEnabled := True ;

  initControl ;
end;

function TGIS_PvlControl.fget_Context
  : TGIS_PvlContext ;
begin
  Result := oContext ;
end;

procedure TGIS_PvlControl.fset_Context(
  const _value: TGIS_PvlContext
) ;
begin
  oContext.Controls.Remove( Self ) ;
  oContext := _value ;
  oContext.Controls.Add( Self ) ;

  PlatformControl.fset_Context( _value ) ;
end;

function TGIS_PvlControl.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnClick ;
end;

procedure TGIS_PvlControl.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnClick( _value ) ;
end;

function TGIS_PvlControl.fget_OnDblClick
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnDblClick ;
end;

procedure TGIS_PvlControl.fset_OnDblClick(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnDblClick( _value ) ;
end;


function TGIS_PvlControl.fget_OnMouseDown
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_OnMouseDown ;
end;

procedure TGIS_PvlControl.fset_OnMouseDown(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_OnMouseDown( _value ) ;
end;

function TGIS_PvlControl.fget_OnMouseMove
  : TGIS_PvlMouseMoveEvent;
begin
  Result := PlatformControl.fget_OnMouseMove ;
end;

procedure TGIS_PvlControl.fset_OnMouseMove(
  const _value : TGIS_PvlMouseMoveEvent
);
begin
  PlatformControl.fset_OnMouseMove( _value ) ;
end;

function TGIS_PvlControl.fget_OnMouseUp
  : TGIS_PvlMouseEvent;
begin
  Result := PlatformControl.fget_OnMouseUp ;
end;

procedure TGIS_PvlControl.fset_OnMouseUp(
  const _value : TGIS_PvlMouseEvent
);
begin
  PlatformControl.fset_OnMouseUp( _value ) ;
end;

function TGIS_PvlControl.fget_OnMouseWheel
  : TGIS_PvlMouseWheelEvent;
begin
  Result := PlatformControl.fget_OnMouseWheel ;
end;

procedure TGIS_PvlControl.fset_OnMouseWheel(
  const _value : TGIS_PvlMouseWheelEvent
);
begin
  PlatformControl.fset_OnMouseWheel( _value ) ;
end;

function  TGIS_PvlControl.fget_OnMouseEnter
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnMouseEnter ;
end;

procedure TGIS_PvlControl.fset_OnMouseEnter(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnMouseEnter( _value ) ;
end;

function TGIS_PvlControl.fget_OnMouseLeave
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnMouseLeave ;
end;

procedure TGIS_PvlControl.fset_OnMouseLeave(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnMouseLeave( _value ) ;
end;

function TGIS_PvlControl.fget_IsStyled
  : Boolean ;
begin
  Result := False ;
end;

procedure TGIS_PvlControl.&add(
  const _control: TObject
) ;
begin
//?  oPlatform.add( _control ) ;
end;

function TGIS_PvlControl.fget_Anchors
  : TGIS_PvlAnchors ;
begin
  Result := PlatformControl.fget_Anchors ;
end;

procedure TGIS_PvlControl.fset_Anchors(
  const _value : TGIS_PvlAnchors
);
begin
  PlatformControl.fset_Anchors( _value ) ;
end ;

function TGIS_PvlControl.fget_Align
  : TGIS_PvlAlign ;
begin
  Result := PlatformControl.fget_Align ;
end;

procedure TGIS_PvlControl.fset_Align(
  const _value : TGIS_PvlAlign
);
begin
  PlatformControl.fset_Align( _value );
end ;

function  TGIS_PvlControl.fget_Enabled
  : Boolean ;
begin
  Result := bEnabled ;
end;

procedure TGIS_PvlControl.fset_Enabled(
  const _value : Boolean
);
var
  o : TObject ;
begin
  bEnabled := _value ;

  o :=  oContext.Parent;

  if o is TGIS_PvlControl then  // do not chnage if alredy disabled by parent
    if not TGIS_PvlControl(o).fget_Enabled then
      exit ;

  PlatformControl.fset_Enabled( _value );

  if assigned( oContextLocal ) then begin
    // subcontrols

    if _value then begin
      // enable subcontrols
      for o in oContextLocal.oList do begin
        if TGIS_PvlControl( o ).bEnabled then // not not direcely disabled
          TGIS_PvlControl( o ).PlatformControl.fset_Enabled( _value );
      end;
    end
    else begin
      // diables subcontrols
      for o in oContextLocal.oList do begin
          TGIS_PvlControl( o ).PlatformControl.fset_Enabled( _value );
      end;
    end;
  end;

end ;

function TGIS_PvlControl.fget_Width
  : Integer ;
begin
  Result := PlatformControl.fget_Width ;
end;

procedure TGIS_PvlControl.fset_Width(
  const _value : Integer
) ;
begin
  PlatformControl.fset_Width( _value );
end;

function TGIS_PvlControl.fget_Height
  : Integer ;
begin
  Result := PlatformControl.fget_Height ;
end;

procedure TGIS_PvlControl.fset_Height(
  const _value : Integer
) ;
begin
  PlatformControl.fset_Height( _value );
end;

function TGIS_PvlControl.fget_Left
  : Integer ;
begin
  Result := PlatformControl.fget_Left ;
end;

procedure TGIS_PvlControl.fset_Left(
  const _value : Integer
) ;
begin
  PlatformControl.fset_Left( _value );
end;

function TGIS_PvlControl.fget_Top
  : Integer ;
begin
  Result := PlatformControl.fget_Top ;
end;

procedure TGIS_PvlControl.fset_Top(
  const _value : Integer
) ;
begin
  PlatformControl.fset_Top( _value );
end;

function TGIS_PvlControl.fget_TabOrder
  : Integer ;
begin
  Result := PlatformControl.fget_TabOrder ;
end;

procedure TGIS_PvlControl.fset_TabOrder(
  const _value : Integer
) ;
begin
  PlatformControl.fset_TabOrder( _value );
end;

function TGIS_PvlControl.fget_Visible
  : Boolean ;
begin
  Result := bVisible ;
end;

procedure TGIS_PvlControl.fset_Visible(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Visible( _value );
  bVisible := _value ;
end;

function  TGIS_PvlControl.fget_Hint
  : String;
begin
  Result := PlatformControl.fget_Hint ;
end;

procedure TGIS_PvlControl.fset_Hint(
  const _value : String
);
begin
  PlatformControl.fset_Hint( _value ) ;
end;

procedure TGIS_PvlControl.Place(
  const _width     : Integer         ;
  const _height    : Integer         ;
  const _xsibling  : TGIS_PvlControl ;
  const _xdistance : Integer         ;
  const _ysibling  : TGIS_PvlControl ;
  const _ydistance : Integer
) ;
begin
  assert( ( _width >= 0 ) or ( _xdistance > 0 ) ) ;
  assert( ( _ydistance >= 0 ) or ( ( _ydistance < 0 ) and assigned( _ysibling ) ) ) ;

  Context.Refresh ;

  if _width > 0 then
    Width := _width ;

  if _height > 0 then
    Height := _height ;

  if _ydistance = -1 then
    Top := _ysibling.Top - ( Height - _ysibling.Height  ) div 2
  else
    if assigned( _ysibling ) then
      Top := _ysibling.Top + _ysibling.Height + _ydistance
    else
      Top := _ydistance ;

  if assigned( _ysibling ) and ( _ydistance = 0 ) then
    if _ysibling.bVisible then
      Top := _ysibling.Top + _ysibling.Height + oContext.VSpace
    else
      Top := _ysibling.Top ;

  if not Context.RightToLeft then begin
    if _xdistance < 0 then begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left - Width + _xdistance
      else
        Left := Context.ClientWidth - Width + _xdistance ;
    end
    else begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left + _xsibling.Width + _xdistance
      else
        Left := _xdistance ;

      if _width < 0 then
        Width := Context.ClientWidth - Left + _width ;
    end;
  end
  else begin
    if ( _xdistance = 0 ) and not assigned( _xsibling )  then
      Left := Context.ClientWidth - Width -  _xdistance
    else if _xdistance < 0 then begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left + _xsibling.Width - _xdistance
      else
        Left := - _xdistance ;
    end
    else begin
      if assigned( _xsibling ) then begin
        if _width < 0 then begin
          Left := - _width ;
          Width := _xsibling.Left - Left - _xdistance ;
        end else
          Left := _xsibling.Left - Width - _xdistance ;
      end
      else begin
        if _width < 0 then begin
          Left := - _width ;
          Width := Context.ClientWidth - Left - _xdistance ;
        end else
          if Context.Parent is TGIS_PvlGroupBox then
            Left := Context.ClientWidth - Width -  _xdistance
          else
            Left := Context.ClientWidth - Width -  _xdistance - Context.HMargin ;
      end ;
    end ;
  end ;
end ;

procedure TGIS_PvlControl.SetFocus ;
begin
  // For safe inheritance
end;

procedure TGIS_PvlControl.DoRedraw ;
begin
  Context.Refresh ;
  if not assigned( PlatformControl ) then
    exit ;
  PlatformControl.DoRedraw ;
end;

procedure TGIS_PvlControl.ProcessMessages ;
begin
  PlatformControl.ProcessMessages ;
end;

{$ENDREGION 'TGIS_PvlControl'}

{$REGION 'TGIS_PvlIconsList'}

function TGIS_PvlIconsList.fget_Icon(
  const _index      : Integer
) :TGIS_SymbolAbstract;
begin
  Result := nil ;
  if _index < 0 then
    exit ;
  if _index >= oList.Count then
    exit ;

  Result := TGIS_SymbolAbstract( oList[ _index ] ) ;
end;

procedure TGIS_PvlIconsList.doCreate(
  const _context   : TGIS_PvlContext
);
begin
  // do nothng
end ;

procedure TGIS_PvlIconsList.initControl ;
begin
  inherited ;
  oSymbols := TGIS_SymbolList.create ;
  oList := TGIS_ObjectList.create( False ) ;
end;

procedure TGIS_PvlIconsList.doDestroy ;
begin
  FreeObject( oSymbols ) ;
  FreeObject( oList ) ;
  inherited ;
end;

function TGIS_PvlIconsList.Add(
  const _path       : String
) : Integer;
begin
  oList.Add( oSymbols.Prepare( _path ) );
  Result := oList.Count-1 ;
end;

function TGIS_PvlIconsList.AddFromResource(
  const _name       : string ;
  const _extension  : TGIS_PvlResourceExtension
): Integer;
  {$IFDEF JAVA}
    var
      strm : TStream ;


    function prepareStream( const _name : String  ) : TGIS_MemoryStream ;
    var
      stream : java.io.InputStream ;
    begin
      Result := TGIS_MemoryStream.Create ;
      stream := getClass().getResourceAsStream( _name ) ;
      if assigned( stream ) then begin
        var data := new Byte[65535];
        var len := stream.read(data);

        while len > 0 do begin
          Result.Write( data, len ) ;
          len := stream.read( data ) ;
        end ;
      end ;
    end ;
  {$ENDIF}
  {$IFDEF DCC}
    var
      strm : TResourceStream ;
      smem : TMemoryStream ;
      bmp  : TGIS_Bitmap ;
  {$ENDIF}
  {$IFDEF CLR}
    var
      strm : System.IO.Stream ;
  {$ENDIF}
begin
{$IFDEF DCC}
  // make transparent
  bmp := TGIS_Bitmap.Create ;
  try
    strm := TResourceStream.Create(hInstance, _name, RT_RCDATA);
    try
      bmp.LoadFromStream( strm ) ;
      bmp.MakeTransparent ;
    finally
      FreeObject( strm ) ;
    end;

    smem := TMemoryStream.Create ;
    try
      bmp.SaveToStream( smem ) ;
      smem.Position := 0 ;
      oList.Add( oSymbols.Prepare( _name, smem ) );
      Result := oList.Count-1 ;
    finally
      FreeObject( smem ) ;
    end ;
  finally
    FreeObject( bmp ) ;
  end;
{$ELSE}
  {$IFDEF JAVA}
    strm := prepareStream( "/resources/" + _name + _extension.toString ) ;
    strm.Position := 0 ;
    oList.Add( oSymbols.Prepare( 'priv://' + _name + _extension.toString, strm ) );
    Result := oList.Count-1 ;
  {$ELSE}
    {$IFDEF CLR}
      var converter := System.Drawing.ImageConverter.create ;

      strm := System.IO.MemoryStream.Create( converter.ConvertTo( TatukGIS.NDK.Properties.Resources.ResourceManager.GetObject( _name ) as System.Drawing.Bitmap, typeOf( array of Byte ) ) as array of Byte ) ;
      oList.Add( oSymbols.Prepare( _name, strm ) ) ;
    {$ENDIF}
  {$ENDIF}
{$ENDIF}
end;

procedure TGIS_PvlIconsList.SetFocus ;
begin
  // do nothing
end;

{$ENDREGION 'TGIS_PvlIconsList'}

{$REGION 'TGIS_PvlPanel'}

procedure TGIS_PvlPanel.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oContextLocal := nil;
  oPlatform := _context.oPlatform.CreateObject(self, 'Panel');
end;

procedure TGIS_PvlPanel.doDestroy ;
begin
  FreeObject( oContextLocal ) ;
  inherited;
end;

procedure TGIS_PvlPanel.Place(
  const _width     : Integer         ;
  const _height    : Integer         ;
  const _xsibling  : TGIS_PvlControl ;
  const _xdistance : Integer         ;
  const _ysibling  : TGIS_PvlControl ;
  const _ydistance : Integer
) ;
begin
  assert( ( _width >= 0 ) or ( _xdistance > 0 ) ) ;
  assert( ( _ydistance >= 0 ) or ( ( _ydistance < 0 ) and assigned( _ysibling ) ) ) ;

  Context.Refresh ;

  if _width > 0 then
    Width := _width ;

  if _height > 0 then
    Height := _height ;

  if _ydistance = -1 then
    Top := _ysibling.Top - ( Height - _ysibling.Height  ) div 2
  else
    if assigned( _ysibling ) then
      Top := _ysibling.Top + _ysibling.Height + _ydistance
    else
      Top := _ydistance ;

  if assigned( _ysibling ) and ( _ydistance = 0 ) then
    if _ysibling.bVisible then
      Top := _ysibling.Top + _ysibling.Height + oContext.VSpace
    else
      Top := _ysibling.Top ;

  if not Context.RightToLeft then begin
    if _xdistance < 0 then begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left - Width + _xdistance
      else
        Left := oContext.ClientWidth - Width + _xdistance ;
    end
    else begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left + _xsibling.Width + _xdistance
      else
        Left := _xdistance ;

      if _width < 0 then
        Width := oContext.ClientWidth - Left + _width ;
    end;
  end
  else begin
    if _xdistance < 0 then begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left + _xsibling.Width - _xdistance
      else
        Left := - _xdistance ;
    end
    else begin
      if assigned( _xsibling ) then begin
        if _width < 0 then begin
          Left := - _width ;
          Width := _xsibling.Left - Left - _xdistance ;
        end else
          Left := _xsibling.Left - Width - _xdistance ;
      end
      else begin
        if _width < 0 then begin
          Left := - _width ;
          Width := oContext.ClientWidth - Left - _xdistance ;
        end else
          Left := oContext.ClientWidth - Width -  _xdistance - Context.HMargin ;
      end ;
    end ;
  end ;
end ;


function TGIS_PvlPanel.fget_PlatformControl
  : IGIS_PvlPanel ;
begin
  Result := oPlatform as IGIS_PvlPanel ;
end;

function TGIS_PvlPanel.fget_Context
  : TGIS_PvlContext;
begin
  if not assigned( oContextLocal ) then
    oContextLocal := TGIS_PvlContext.Create( self, inherited fget_Context ) ;
  Result := oContextLocal ;
end;

function TGIS_PvlPanel.fget_Scrollable
  : Boolean;
begin
  Result := PlatformControl.fget_Scrollable ;
end;

procedure TGIS_PvlPanel.fset_Scrollable(
  const _value : Boolean
);
begin
  PlatformControl.fset_Scrollable( _value ) ;
end;

function TGIS_PvlPanel.fget_Border
  : Boolean;
begin
  Result := PlatformControl.fget_Border ;
end;

procedure TGIS_PvlPanel.fset_Border(
  const _value : Boolean
);
begin
  PlatformControl.fset_Border( _value ) ;
end;

procedure TGIS_PvlPanel.RemoveAllComponents;
begin
  PlatformControl.RemoveAllComponents ;
end;

{$ENDREGION 'TGIS_PvlPanel'}

{$REGION 'TGIS_PvlPage'}

constructor TGIS_PvlPage.Create(
  const _context: TGIS_PvlContext
) ;
begin
  FPage := TGIS_PvlPanel.Create( _context ) ;
end;

procedure  TGIS_PvlPage.Activate(
  const _sender : TObject
) ;
begin
  FPage.Visible := True ;

  if assigned( FOnEnter ) then
    FOnEnter( _sender ) ;
end;

procedure TGIS_PvlPage.Deactivate(
  const _sender : TObject
) ;
begin
  FPage.Visible := False ;

  if assigned( FOnExit ) then
    FOnExit( _sender ) ;
end;

function TGIS_PvlPage.fget_Height
  : Integer;
begin
  Result := FPage.Height ;
end;

procedure TGIS_PvlPage.fset_Height(
  const _value : Integer
) ;
begin
  FPage.Height := _value ;
end;

function TGIS_PvlPage.fget_Width
  : Integer ;
begin
  Result := FPage.Width ;
end;

procedure TGIS_PvlPage.fset_Width(
  const _value : Integer
) ;
begin
  FPage.Width := _value ;
end;

function TGIS_PvlPage.fget_Context
  : TGIS_PvlContext ;
begin
  Result := FPage.Context ;
end;

function TGIS_PvlPage.fget_Left
  : Integer ;
begin
  Result := FPage.Left ;
end;

procedure TGIS_PvlPage.fset_Left(
  const _value : Integer
) ;
begin
  FPage.Left := _value ;
end;

function TGIS_PvlPage.fget_Top
  : Integer ;
begin
  Result := FPage.Top ;
end;

procedure TGIS_PvlPage.fset_Top(
  const _value : Integer
) ;
begin
  FPage.Top := _value ;
end;

procedure TGIS_PvlPage.fset_Next(
  const _value : TGIS_PvlPage
) ;
begin
  FNext := _value ;
end;

procedure TGIS_PvlPage.fset_Previous(
  const _value : TGIS_PvlPage
) ;
begin
  FPrevious := _value ;
end;

{$ENDREGION 'TGIS_PvlPage'}

{$REGION 'TGIS_PvlPages'}

procedure TGIS_PvlPages.doCreate(
  const _context: TGIS_PvlContext
) ;
begin
  oPanel := TGIS_PvlPanel.Create( _context ) ;
  oPlatform := oPanel.PlatformControl ;
  FPages := TList<TGIS_PvlPage>.Create ;
  bAbort := False ;
  ctx := oPanel.Context ;
end;

procedure TGIS_PvlPages.doDestroy ;
var
  itm : TGIS_PvlPage ;
begin
  for itm in FPages do begin
    FreeObjectNotNil( itm ) ;
  end;

  FreeObject( FPages ) ;
  oPlatform := nil ;

  inherited ;
end;

procedure TGIS_PvlPages.Activate(
  const _pageNr: Integer ;
  const _sender: TObject
);
begin
  if assigned( FActive ) then
    Deactivate( _sender ) ;

  if bAbort then
    exit ;

  FActive := FPages.{$IFDEF DCC}Items{$ELSE}Item{$ENDIF}[ _pageNr ] ;
  FActive.Activate( _sender ) ;

  if assigned( FOnPageChange ) then
    FOnPageChange( Self ) ;
end;

procedure TGIS_PvlPages.Deactivate(
  const _sender: TObject
) ;
begin
  if assigned( FActive ) then
  begin
    if assigned( FActive.BeforeExit ) then
      FActive.BeforeExit( _sender ) ;

    if bAbort then
      exit ;

    FActive.Deactivate( _sender ) ;
  end;
end;

procedure TGIS_PvlPages.Next(
  const _sender : TObject
) ;
var
  idx : Integer ;
begin
  Deactivate( _sender ) ;

  try
    if bAbort then
      exit;

    if not assigned ( FActive.Next ) then
      idx := FPages.IndexOf( FActive ) + 1
    else
      idx := FPages.IndexOf( FActive.Next ) ;

    //?FActive := nil ;

    Activate( idx, _sender ) ;

  finally
    if bAbort then
      bAbort := not bAbort ;
  end;
end;

procedure TGIS_PvlPages.Previous(
  const _sender : TObject
) ;
var
  idx : Integer ;
begin
  Deactivate( _sender ) ;

  if not assigned ( FActive.Previous ) then
    idx := FPages.IndexOf( FActive ) - 1
  else
    idx := FPages.IndexOf( FActive.Previous ) ;

  //?FActive := nil ;

  Activate( idx, _sender ) ;

  if bAbort then
    bAbort := not bAbort ;
end;

function TGIS_PvlPages.IsLast
  : Boolean ;
var
  idx : Integer ;
begin
  idx := FPages.IndexOf( FActive ) ;

  if not assigned( FActive ) then begin
    Result := prevIsLast ;
    exit ;
  end;

  if assigned( FActive.Next ) then
    Result := idx = ( FPages.Count - 1 )
  else
    Result := True ;

  prevIsLast := Result ;
end;

function TGIS_PvlPages.IsFirst
  : Boolean ;
var
  idx : Integer ;
begin
  idx := FPages.IndexOf( FActive ) ;

  if not assigned( FActive ) then begin
    Result := prevIsFirst ;
    exit ;
  end;

  if assigned( FActive.Previous ) then
    Result := idx = 0
  else
    Result := True ;

  prevIsFirst := Result ;
end;

procedure TGIS_PvlPages.SetFocus ;
begin
  // For safe inheritance
end;

procedure TGIS_PvlPages.Abort ;
begin
  bAbort := True ;
end;

function TGIS_PvlPages.GetActivePageNumber
  : Integer ;
begin
  Result := FPages.IndexOf( FActive ) ;
end;

function TGIS_PvlPages.AddPage
  : TGIS_PvlPage ;
begin
  Context.Refresh ;
  Result := TGIS_PvlPage.Create( ctx ) ;
  Result.Width := Width ;
  Result.Height := Height ;
  Result.Top := 0 ;
  Result.Left := 0 ;
  Result.Deactivate( Self ) ;
  FPages.Add( Result ) ;
  Result.PageNumber := FPages.IndexOf( Result ) ;
end;

{$ENDREGION 'TGIS_PvlPages'}

{$REGION 'TGIS_PvlCanvas'}

procedure TGIS_PvlCanvas.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'Canvas') ;
end;

function TGIS_PvlCanvas.fget_PlatformControl
  : IGIS_PvlCanvas ;
begin
  Result := oPlatform as IGIS_PvlCanvas ;
end;

procedure TGIS_PvlCanvas.initControl ;
begin
  inherited ;
end;

function TGIS_PvlCanvas.fget_OnRender
  : TGIS_PvlRendererEvent ;
begin
  Result := PlatformControl.fget_OnRender ;
end;

procedure TGIS_PvlCanvas.fset_OnRender(
  const _value: TGIS_PvlRendererEvent
) ;
begin
  PlatformControl.fset_OnRender( _value ) ;
end;

{$ENDREGION 'TGIS_PvlCanvas'}

{$REGION 'TGIS_PvlPreviewPanel'}

procedure TGIS_PvlPreviewPanel.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'PreviewPanel') ;
end;

function TGIS_PvlPreviewPanel.fget_PlatformControl
  : IGIS_PvlPreviewPanel ;
begin
  Result := oPlatform as IGIS_PvlPreviewPanel ;
end;

procedure TGIS_PvlPreviewPanel.initControl ;
begin
  inherited ;
  Caption := 'TGIS_PvlPreviewPanel' ;
end;

function TGIS_PvlPreviewPanel.fget_IsStyled
  : Boolean ;
begin
  Result := PlatformControl.fget_IsStyled ;
end;

function TGIS_PvlPreviewPanel.fget_Caption
  : String ;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlPreviewPanel.fset_Caption(
  const _value : String
) ;
begin
  PlatformControl.fset_Caption( _value ) ;
end;

function TGIS_PvlPreviewPanel.fget_FontSize
  : Integer ;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlPreviewPanel.fset_FontSize(
  const _value : Integer
) ;
begin
  PlatformControl.fset_FontSize( _value ) ;
end;

function  TGIS_PvlPreviewPanel.fget_FontStyle
  : TGIS_FontStyles ;
begin
  Result := PlatformControl.fget_FontStyle ;
end;

procedure TGIS_PvlPreviewPanel.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
begin
  PlatformControl.fset_FontStyle( _value ) ;
end;

function  TGIS_PvlPreviewPanel.fget_FontFamily
  : String ;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlPreviewPanel.fset_FontFamily(
  const _value : String
) ;
begin
  PlatformControl.fset_FontFamily( _value ) ;
end;

function  TGIS_PvlPreviewPanel.fget_Border
  : Boolean ;
begin
  Result := PlatformControl.fget_Border ;
end;

procedure TGIS_PvlPreviewPanel.fset_Border(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Border( _value ) ;
end;

function  TGIS_PvlPreviewPanel.fget_Bitmap
  : TGIS_Bitmap ;
begin
  Result := PlatformControl.fget_Bitmap ;
end;

procedure TGIS_PvlPreviewPanel.fset_Bitmap(
  const _value : TGIS_Bitmap
) ;
begin
  PlatformControl.fset_Bitmap( _value ) ;
end;

function  TGIS_PvlPreviewPanel.fget_Color
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_Color ;
end;

procedure TGIS_PvlPreviewPanel.fset_Color(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_Color( _value ) ;
end;

function TGIS_PvlPreviewPanel.fget_StyledAreaColor
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_StyledAreaColor ;
end;

procedure TGIS_PvlPreviewPanel.Invalidate;
begin
  PlatformControl.Invalidate ;
end;

{$ENDREGION 'TGIS_PvlPreviewPanel'}

{$REGION 'TGIS_PvlGroupBox'}

procedure TGIS_PvlGroupBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oContextLocal := nil;
  oPlatform := _context.oPlatform.CreateObject(self, 'GroupBox') ;
end;

procedure TGIS_PvlGroupBox.doDestroy ;
begin
  FreeObject( oContextLocal ) ;
  inherited ;
end;

procedure TGIS_PvlGroupBox.Place(
  const _width     : Integer         ;
  const _height    : Integer         ;
  const _xsibling  : TGIS_PvlControl ;
  const _xdistance : Integer         ;
  const _ysibling  : TGIS_PvlControl ;
  const _ydistance : Integer
) ;
begin
  assert( ( _width >= 0 ) or ( _xdistance > 0 ) ) ;
  assert( ( _ydistance >= 0 ) or ( ( _ydistance < 0 ) and assigned( _ysibling ) ) ) ;

  Context.Refresh ;

  if _width > 0 then
    Width := _width ;

  if _height > 0 then
    Height := _height ;

  if _ydistance = -1 then
    Top := _ysibling.Top - ( Height - _ysibling.Height  ) div 2
  else
    if assigned( _ysibling ) then
      Top := _ysibling.Top + _ysibling.Height + _ydistance
    else
      Top := _ydistance ;

  if assigned( _ysibling ) and ( _ydistance = 0 ) then
    if _ysibling.bVisible then
      Top := _ysibling.Top + _ysibling.Height + oContext.VSpace
    else
      Top := _ysibling.Top ;

  if not Context.RightToLeft then begin
    if _xdistance < 0 then begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left - Width + _xdistance
      else
        Left := oContext.ClientWidth - Width + _xdistance ;
    end
    else begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left + _xsibling.Width + _xdistance
      else
        Left := _xdistance ;

      if _width < 0 then
        Width := oContext.ClientWidth - Left + _width ;
    end;
  end
  else begin
    if _xdistance < 0 then begin
      if assigned( _xsibling ) then
        Left := _xsibling.Left + _xsibling.Width - _xdistance
      else
        Left := - _xdistance ;
    end
    else begin
      if assigned( _xsibling ) then begin
        if _width < 0 then begin
          Left := - _width ;
          Width := _xsibling.Left - Left - _xdistance ;
        end else
          Left := _xsibling.Left - Width - _xdistance ;
      end
      else begin
        if _width < 0 then begin
          Left := - _width ;
          Width := oContext.ClientWidth - Left - _xdistance ;
        end else
          Left := oContext.ClientWidth - Width -  _xdistance - Context.HMargin ;
      end ;
    end ;
  end ;
end ;

function TGIS_PvlGroupBox.fget_PlatformControl
  : IGIS_PvlGroupBox ;
begin
  Result := oPlatform as IGIS_PvlGroupBox ;
end;

function TGIS_PvlGroupBox.fget_Context
  : TGIS_PvlContext;
begin
  if not assigned( oContextLocal ) then begin
    oContextLocal := TGIS_PvlContext.Create( self, inherited fget_Context ) ;
    oContextLocal.VMargin := RoundS( 2 * oContext.VMargin ) ;
    oContextLocal.HMargin := oContext.HMargin ;
  end;

  Result := oContextLocal ;
end;

function TGIS_PvlGroupBox.fget_Caption
  : String ;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlGroupBox.fset_Caption(
  const _value : String
) ;
begin
  PlatformControl.fset_Caption( _value ) ;
end;

{$ENDREGION 'TGIS_PvlGroupBox'}

{$REGION 'TGIS_PvlTrackBar'}

procedure TGIS_PvlTrackBar.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject( self, 'TrackBar' ) ;
end;

function TGIS_PvlTrackBar.fget_PlatformControl
  : IGIS_PvlTrackBar ;
begin
  Result := oPlatform as IGIS_PvlTrackBar ;
end;

function TGIS_PvlTrackBar.fget_Minimum
  : Integer ;
begin
  Result := PlatformControl.fget_Minimum
end;

procedure TGIS_PvlTrackBar.fset_Minimum(
  const _value: Integer
) ;
begin
  PlatformControl.fset_Minimum( _value ) ;
end;

function TGIS_PvlTrackBar.fget_Maximum
  : Integer ;
begin
  Result := PlatformControl.fget_Maximum
end;

procedure TGIS_PvlTrackBar.fset_Maximum(
  const _value: Integer
) ;
begin
  PlatformControl.fset_Maximum( _value ) ;
end;

function TGIS_PvlTrackBar.fget_Position
  : Integer ;
begin
  Result := PlatformControl.fget_Position
end;

procedure TGIS_PvlTrackBar.fset_Position(
  const _value: Integer
) ;
begin
  PlatformControl.fset_Position( _value ) ;
end;

function TGIS_PvlTrackBar.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlTrackBar.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;


{$ENDREGION 'TGIS_PvlTrackBar'}

{$REGION 'TGIS_PvlListBox'}

procedure TGIS_PvlListBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ListBox') ;
end;

function TGIS_PvlListBox.fget_PlatformControl
  : IGIS_PvlListBox ;
begin
  Result := oPlatform as IGIS_PvlListBox ;
end;

function TGIS_PvlListBox.fget_ItemList
  : TGIS_ListOfStrings;
begin
  Result := PlatformControl.fget_ItemList ;
end;

function TGIS_PvlListBox.fget_ItemsCount
  : Integer;
begin
  Result := PlatformControl.fget_ItemsCount ;
end;

function TGIS_PvlListBox.fget_Item(
  const _idx : Integer
) : String;
begin
  Result := PlatformControl.fget_Item( _idx ) ;
end;

function TGIS_PvlListBox.fget_ItemIndex
  : Integer;
begin
  Result := PlatformControl.fget_ItemIndex ;
end;

procedure TGIS_PvlListBox.fset_ItemIndex(
  const _value : Integer
);
begin
  PlatformControl.fset_ItemIndex( _value ) ;
end;

function TGIS_PvlListBox.fget_SelectedItems
  : TGIS_ListOfStrings ;
begin
  Result := PlatformControl.fget_SelectedItems ;
end;

function TGIS_PvlListBox.fget_Selected(
  const _index: Integer
) : Boolean ;
begin
  Result := PlatformControl.fget_Selected( _index ) ;
end;

procedure TGIS_PvlListBox.fset_Selected(
  const _index: Integer;
  const _value: Boolean
) ;
begin
  PlatformControl.fset_Selected( _index, _value ) ;
end;

function TGIS_PvlListBox.fget_Multiselect
  : Boolean ;
begin
  Result := PlatformControl.fget_Multiselect ;
end;

procedure TGIS_PvlListBox.fset_Multiselect(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_Multiselect( _value ) ;
end;

function TGIS_PvlListBox.fget_OnChange
  : TGIS_PvlEvent ;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlListBox.fset_OnChange(
  const _value: TGIS_PvlEvent
) ;
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

procedure TGIS_PvlListBox.BeginUpdate;
begin
  PlatformControl.BeginUpdate ;
end;

procedure TGIS_PvlListBox.EndUpdate;
begin
  PlatformControl.EndUpdate ;
end;

procedure TGIS_PvlListBox.ItemsClear;
begin
  PlatformControl.ItemsClear ;
end;

procedure TGIS_PvlListBox.ItemsAdd(
  const _item : String
);
begin
  PlatformControl.ItemsAdd( _item ) ;
end;

{$ENDREGION 'TGIS_PvlListBox'}

{$REGION 'TGIS_PvlSVGList'}

procedure TGIS_PvlSVGList.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'SVGList') ;
end;

function TGIS_PvlSVGList.fget_PlatformControl
  : IGIS_PvlSVGList ;
begin
  Result := oPlatform as IGIS_PvlSVGList ;
end;

function TGIS_PvlSVGList.fget_ItemList
  : TGIS_ListOfStrings;
begin
  Result := PlatformControl.fget_ItemList ;
end;

function TGIS_PvlSVGList.fget_ItemsCount
  : Integer;
begin
  Result := PlatformControl.fget_ItemsCount ;
end;

function TGIS_PvlSVGList.fget_Item(
  const _idx : Integer
) : String;
begin
  Result := PlatformControl.fget_Item( _idx ) ;
end;

function TGIS_PvlSVGList.fget_ItemIndex
  : Integer;
begin
  Result := PlatformControl.fget_ItemIndex ;
end;

function TGIS_PvlSVGList.fget_Selected(
  const _index: Integer
) : Boolean ;
begin
  Result := PlatformControl.fget_Selected( _index ) ;
end;

procedure TGIS_PvlSVGList.fset_Selected(
  const _index: Integer;
  const _value: Boolean
) ;
begin
  PlatformControl.fset_Selected( _index, _value ) ;
end;

function TGIS_PvlSVGList.fget_SelectedItems
  : TGIS_ListOfStrings ;
begin
  Result := PlatformControl.fget_SelectedItems ;
end;

function TGIS_PvlSVGList.fget_Multiselect
  : Boolean ;
begin
  Result := PlatformControl.fget_Multiselect ;
end;

procedure TGIS_PvlSVGList.fset_Multiselect(
  const _value: Boolean
) ;
begin
  PlatformControl.fset_Multiselect( _value ) ;
end;

procedure TGIS_PvlSVGList.fset_ItemIndex(
  const _value : Integer
);
begin
  PlatformControl.fset_ItemIndex( _value ) ;
end;

function TGIS_PvlSVGList.fget_OnChange
  : TGIS_PvlEvent ;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlSVGList.fset_OnChange(
  const _value: TGIS_PvlEvent
) ;
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

procedure TGIS_PvlSVGList.BeginUpdate;
begin
  PlatformControl.BeginUpdate ;
end;

procedure TGIS_PvlSVGList.EndUpdate;
begin
  PlatformControl.EndUpdate ;
end;

procedure TGIS_PvlSVGList.ItemsClear;
begin
  PlatformControl.ItemsClear ;
end;

procedure TGIS_PvlSVGList.ItemsAdd(
  const _item : String
);
begin
  PlatformControl.ItemsAdd( _item ) ;
end;

{$ENDREGION 'TGIS_PvlListBox'}

{$REGION 'TGIS_PvlLabel'}

procedure TGIS_PvlLabel.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'Label') ;
end;

procedure TGIS_PvlLabel.initControl ;
begin
  Caption := 'TGIS_PvlLabel' ;
  inherited ;
end;

function TGIS_PvlLabel.fget_PlatformControl
  : IGIS_PvlLabel ;
begin
  Result := oPlatform as IGIS_PvlLabel ;
end;

function TGIS_PvlLabel.fget_Caption
  : String;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlLabel.fset_Caption(
  const _value : String
);
begin
  PlatformControl.fset_Caption( _value ) ;
end;

function TGIS_PvlLabel.fget_FocusControl
  : TGIS_PvlControl;
begin
  Result := PlatformControl.fget_FocusControl ;
end;

procedure TGIS_PvlLabel.fset_FocusControl(
  const _value : TGIS_PvlControl
);
begin
  PlatformControl.fset_FocusControl( _value ) ;
end;

function TGIS_PvlLabel.fget_FontSize
  : Integer;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlLabel.fset_FontSize(
  const _value : Integer
);
begin
  PlatformControl.fset_FontSize( _value ) ;
end;

function TGIS_PvlLabel.fget_FontStyle
  : TGIS_FontStyles;
begin
  Result := PlatformControl.fget_FontStyle ;

end;

procedure TGIS_PvlLabel.fset_FontStyle(
  const _value : TGIS_FontStyles
);
begin
  PlatformControl.fset_FontStyle( _value ) ;
end;

function TGIS_PvlLabel.fget_FontFamily
  : String;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlLabel.fset_FontFamily(
  const _value : String
);
begin
  PlatformControl.fset_FontFamily( _value ) ;
end;

function TGIS_PvlLabel.fget_FontColor
  : TGIS_Color;
begin
  Result := PlatformControl.fget_FontColor ;
end;

procedure TGIS_PvlLabel.fset_FontColor(
  const _value : TGIS_Color
);
begin
  PlatformControl.fset_FontColor( _value ) ;
end;

function TGIS_PvlLabel.fget_Alignment
  : TGIS_PvlLabelTextAlignment;
begin
  Result := PlatformControl.fget_Alignment ;
end;

procedure TGIS_PvlLabel.fset_Alignment(
  const _value : TGIS_PvlLabelTextAlignment
);
begin
  PlatformControl.fset_Alignment( _value ) ;
end;

{$ENDREGION 'TGIS_PvlLabel'}

{$REGION 'TGIS_PvlIconButton'}

procedure TGIS_PvlIconButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'IconButton') ;
end;

function TGIS_PvlIconButton.fget_PlatformControl
  : IGIS_PvlIconButton ;
begin
  Result := oPlatform as IGIS_PvlIconButton ;
end;

function TGIS_PvlIconButton.fget_IconsList
  : TGIS_PvlIconsList;
begin
  Result := oIconsList ;
end;

procedure TGIS_PvlIconButton.fset_IconsList(
  const _value : TGIS_PvlIconsList
);
begin
  oIconsList := _value;
  DoRedraw ;
end;

function  TGIS_PvlIconButton.fget_IconIndex
  : Integer;
begin
  Result := iIconIndex ;
end;

procedure TGIS_PvlIconButton.fset_IconIndex(
  const _value : Integer
);
begin
  iIconIndex := _value;
  DoRedraw ;
end;

function  TGIS_PvlIconButton.fget_IconSize
  : Integer;
begin
  if iIconSize = 0 then
    iIconSize := 16 ;
  Result := iIconSize ;
end;

procedure TGIS_PvlIconButton.fset_IconSize(
  const _value : Integer
);
begin
  iIconSize := _value;
  DoRedraw ;
end;

function TGIS_PvlIconButton.fget_Pushed
  : Boolean ;
begin
  Result := PlatformControl.fget_Pushed;
end;

procedure TGIS_PvlIconButton.fset_Pushed(
  const _value : Boolean
);
begin
  PlatformControl.fset_Pushed( _value );
end;

{$ENDREGION 'TGIS_PvlIconButton'}

{$REGION 'TGIS_PvlButton'}

procedure TGIS_PvlButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'Button') ;
end;

procedure TGIS_PvlButton.initControl ;
begin
  inherited ;
  Caption := 'TGIS_PvlButton' ;
end;

function TGIS_PvlButton.fget_PlatformControl
  : IGIS_PvlButton;
begin
  Result := oPlatform as IGIS_PvlButton ;
end;

function TGIS_PvlButton.fget_Caption
  : String;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlButton.fset_Caption(
  const _value : String
);
begin
  PlatformControl.fset_Caption( _value ) ;
end ;

function TGIS_PvlButton.fget_Default
  : Boolean;
begin
  Result := PlatformControl.fget_Default ;
end;

procedure TGIS_PvlButton.fset_Default(
  const _value : Boolean
);
begin
  PlatformControl.fset_Default( _value ) ;
end;

{$ENDREGION 'TGIS_PvlButton'}

{$REGION 'TGIS_PvlModalButton'}

procedure TGIS_PvlModalButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ModalButton') ;
end;

procedure TGIS_PvlModalButton.initControl ;
begin
  inherited ;
  Caption := 'TGIS_PvlModalButton' ;
end;

function TGIS_PvlModalButton.fget_PlatformControl
  : IGIS_PvlModalButton;
begin
  Result := oPlatform as IGIS_PvlModalButton ;
end;

function TGIS_PvlModalButton.fget_Caption
  : String;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlModalButton.fset_Caption(
  const _value : String
);
begin
  PlatformControl.fset_Caption( _value ) ;
end ;

function TGIS_PvlModalButton.fget_Default
  : Boolean;
begin
  Result := PlatformControl.fget_Default ;
end;

procedure TGIS_PvlModalButton.fset_Default(
  const _value : Boolean
);
begin
  PlatformControl.fset_Default( _value ) ;
end;

procedure TGIS_PvlModalButton.SetFocus ;
begin
  PlatformControl.SetFocus ;
end;

{$ENDREGION 'TGIS_PvlModalButton'}

{$REGION 'TGIS_PvlEdit'}

procedure TGIS_PvlEdit.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'Edit') ;
end;

procedure TGIS_PvlEdit.initControl ;
begin
  inherited ;
  Text := '' ;
end;

function TGIS_PvlEdit.fget_PlatformControl
  : IGIS_PvlEdit;
begin
  Result := oPlatform as IGIS_PvlEdit ;
end;

function TGIS_PvlEdit.fget_Text
  : String;
begin
  Result := PlatformControl.fget_Text;
end;

procedure TGIS_PvlEdit.fset_Text(
  const _value : String
);
begin
  PlatformControl.fset_Text( _value );
end;

function TGIS_PvlEdit.fget_FontSize
  : Integer ;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlEdit.fset_FontSize(
  const _value : Integer
) ;
begin
  PlatformControl.fset_FontSize( _value ) ;
end;

function  TGIS_PvlEdit.fget_FontStyle
  : TGIS_FontStyles ;
begin
  Result := PlatformControl.fget_FontStyle ;
end;

procedure TGIS_PvlEdit.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
begin
  PlatformControl.fset_FontStyle( _value ) ;
end;

function  TGIS_PvlEdit.fget_FontFamily
  : String ;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlEdit.fset_FontFamily(
  const _value : String
) ;
begin
  PlatformControl.fset_FontFamily( _value ) ;
end;

function TGIS_PvlEdit.fget_Editable
  : Boolean;
begin
  Result := PlatformControl.fget_Editable;
end;

procedure TGIS_PvlEdit.fset_Editable(
  const _value : Boolean
);
begin
  PlatformControl.fset_Editable( _value );
end;

function TGIS_PvlEdit.fget_SelectionStart
  : Integer;
begin
  Result := PlatformControl.fget_SelectionStart;
end;

procedure TGIS_PvlEdit.fset_SelectionStart(
  const _value : Integer
);
begin
  PlatformControl.fset_SelectionStart( _value );
end;

function TGIS_PvlEdit.fget_SelectionLength
  : Integer;
begin
  Result := PlatformControl.fget_SelectionLength;
end;

procedure TGIS_PvlEdit.fset_SelectionLength(
  const _value : Integer
);
begin
  PlatformControl.fset_SelectionLength( _value );
end;

function TGIS_PvlEdit.fget_SelectedText
  : String;
begin
  Result := PlatformControl.fget_SelectedText;
end;

function TGIS_PvlEdit.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlEdit.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlEdit.fget_OnKeyDown
  : TGIS_PvlKeyEvent;
begin
  Result := PlatformControl.fget_OnKeyDown ;
end;

procedure TGIS_PvlEdit.fset_OnKeyDown(
  const _value : TGIS_PvlKeyEvent
);
begin
  PlatformControl.fset_OnKeyDown( _value ) ;
end;

function TGIS_PvlEdit.fget_OnKeyPress
  : TGIS_PvlKeyPressEvent;
begin
  Result := PlatformControl.fget_OnKeyPress ;
end;

procedure TGIS_PvlEdit.fset_OnKeyPress(
  const _value : TGIS_PvlKeyPressEvent
);
begin
  PlatformControl.fset_OnKeyPress( _value ) ;
end;

procedure TGIS_PvlEdit.SetAsHyperlink;
begin
  PlatformControl.SetAsHyperlink ;
end;

procedure TGIS_PvlEdit.SetFontAlarm;
begin
  PlatformControl.SetFontAlarm ;
end;

procedure TGIS_PvlEdit.SetFontDefault;
begin
  PlatformControl.SetFontDefault
end;

procedure TGIS_PvlEdit.SetBackgroundAlarm;
begin
  PlatformControl.SetBackgroundAlarm ;
end;

procedure TGIS_PvlEdit.SetBackgroundDefault;
begin
  PlatformControl.SetBackgroundDefault
end;

{$ENDREGION 'TGIS_PvlEdit'}

{$REGION 'TGIS_PvlMemo'}

procedure TGIS_PvlMemo.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'Memo') ;
end;

procedure TGIS_PvlMemo.initControl ;
begin
  inherited ;
  Text := '' ;
end;

function TGIS_PvlMemo.fget_PlatformControl
  : IGIS_PvlMemo ;
begin
  Result := oPlatform as IGIS_PvlMemo ;
end;

function TGIS_PvlMemo.fget_CursorPos
  : TPoint;
begin
  Result := PlatformControl.fget_CursorPos ;
end;

procedure TGIS_PvlMemo.fset_CursorPos(
  const _value : TPoint
);
begin
  PlatformControl.fset_CursorPos( _value ) ;
end;

function TGIS_PvlMemo.fget_Text
  : String;
begin
  Result := PlatformControl.fget_Text ;
end;

procedure TGIS_PvlMemo.fset_Text(
  const _value : String
);
begin
  PlatformControl.fset_Text( _value ) ;
end;

function TGIS_PvlMemo.fget_FontSize
  : Integer;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlMemo.fset_FontSize(
  const _value : Integer
);
begin
  PlatformControl.fset_FontSize( _value ) ;
end;

function TGIS_PvlMemo.fget_FontStyle
  : TGIS_FontStyles;
begin
  Result := PlatformControl.fget_FontStyle ;

end;

procedure TGIS_PvlMemo.fset_FontStyle(
  const _value : TGIS_FontStyles
);
begin
  PlatformControl.fset_FontStyle( _value ) ;
end;

function TGIS_PvlMemo.fget_FontFamily
  : String;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlMemo.fset_FontFamily(
  const _value : String
);
begin
  PlatformControl.fset_FontFamily( _value ) ;
end;

function TGIS_PvlMemo.fget_SelectionStart
  : Integer;
begin
  Result := PlatformControl.fget_SelectionStart ;
end;

procedure TGIS_PvlMemo.fset_SelectionStart(
  const _value : Integer
);
begin
  PlatformControl.fset_SelectionStart( _value ) ;
end;

function TGIS_PvlMemo.fget_SelectionLength
  : Integer;
begin
  Result := PlatformControl.fget_SelectionLength ;
end;

procedure TGIS_PvlMemo.fset_SelectionLength(
  const _value : Integer
);
begin
  PlatformControl.fset_SelectionLength( _value ) ;
end;

function  TGIS_PvlMemo.fget_SelectedText
  : String;
begin
  Result := PlatformControl.fget_SelectedText ;
end;

function  TGIS_PvlMemo.fget_WordWrap
  : Boolean;
begin
  Result := PlatformControl.fget_WordWrap ;
end;

procedure TGIS_PvlMemo.fset_WordWrap(
  const _value : Boolean
);
begin
  PlatformControl.fset_WordWrap( _value ) ;
end;

function  TGIS_PvlMemo.fget_Editable
  : Boolean;
begin
  Result := PlatformControl.fget_Editable ;
end;

procedure TGIS_PvlMemo.fset_Editable(
  const _value : Boolean
);
begin
  PlatformControl.fset_Editable( _value ) ;
end;

function TGIS_PvlMemo.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlMemo.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlMemo.fget_OnKeyDown
  : TGIS_PvlKeyEvent;
begin
  Result := PlatformControl.fget_OnKeyDown ;
end;

procedure TGIS_PvlMemo.fset_OnKeyDown(
  const _value : TGIS_PvlKeyEvent
);
begin
  PlatformControl.fset_OnKeyDown( _value ) ;
end;

function TGIS_PvlMemo.fget_OnKeyPress
  : TGIS_PvlKeyPressEvent;
begin
  Result := PlatformControl.fget_OnKeyPress ;
end;

procedure TGIS_PvlMemo.fset_OnKeyPress(
  const _value : TGIS_PvlKeyPressEvent
);
begin
  PlatformControl.fset_OnKeyPress( _value ) ;
end;

procedure TGIS_PvlMemo.SetFontAlarm;
begin
  PlatformControl.SetFontAlarm ;
end;

procedure TGIS_PvlMemo.SetFontDefault;
begin
  PlatformControl.SetFontDefault ;
end;

procedure TGIS_PvlMemo.Clear ;
begin
  PlatformControl.Clear ;
end;

procedure TGIS_PvlMemo.AppendText(
  const _value : String
);
begin
  PlatformControl.AppendText( _value ) ;
end;

procedure TGIS_PvlMemo.AppendLine(
  const _value : String
);
begin
  PlatformControl.AppendLine( _value ) ;
end;

{$ENDREGION 'TGIS_PvlMemo'}

{$REGION 'TGIS_PvlComboBox'}

procedure TGIS_PvlComboBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ComboBox') ;
end;

function TGIS_PvlComboBox.fget_PlatformControl
  : IGIS_PvlComboBox ;
begin
  Result := oPlatform as IGIS_PvlComboBox ;
end;

function TGIS_PvlComboBox.fget_ItemsCount
  : Integer;
begin
  Result := PlatformControl.fget_ItemsCount ;
end;

function TGIS_PvlComboBox.fget_Item(
  const _idx : Integer
) : String;
begin
  Result := PlatformControl.fget_Item( _idx ) ;
end;

function  TGIS_PvlComboBox.fget_ItemIndex
  : Integer;
begin
  Result := PlatformControl.fget_ItemIndex ;
end;

procedure TGIS_PvlComboBox.fset_ItemIndex(
  const _value : Integer
);
begin
  PlatformControl.fset_ItemIndex( _value ) ;
end;

function TGIS_PvlComboBox.fget_Text
  : String;
begin
  Result := PlatformControl.fget_Text ;
end;

procedure TGIS_PvlComboBox.fset_Text(
  const _value : String
);
begin
  PlatformControl.fset_Text( _value ) ;
end;

function TGIS_PvlComboBox.fget_FontSize
  : Integer ;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlComboBox.fset_FontSize(
  const _value : Integer
) ;
begin
  PlatformControl.fset_FontSize( _value );
end;

function TGIS_PvlComboBox.fget_FontStyle
  : TGIS_FontStyles ;
begin
  Result := PlatformControl.fget_FontStyle ;
end;

procedure TGIS_PvlComboBox.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
begin
  PlatformControl.fset_FontStyle( _value );
end;

function TGIS_PvlComboBox.fget_FontFamily
  : String ;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlComboBox.fset_FontFamily(
  const _value : String
) ;
begin
  PlatformControl.fset_FontFamily( _value );
end;

function TGIS_PvlComboBox.fget_Tag
  : NativeInt;
begin
  Result := PlatformControl.fget_Tag ;
end;

procedure TGIS_PvlComboBox.fset_Tag(
  const _value : NativeInt
);
begin
  PlatformControl.fset_Tag( _value ) ;
end;

function TGIS_PvlComboBox.fget_Sorted
  : Boolean;
begin
  Result := PlatformControl.fget_Sorted ;
end;

procedure TGIS_PvlComboBox.fset_Sorted(
  const _value : Boolean
                                 );
begin
  PlatformControl.fset_Sorted( _value ) ;
end;

function TGIS_PvlComboBox.fget_DropDownCount
  : Integer;
begin
  Result := PlatformControl.fget_DropDownCount ;
end;

procedure TGIS_PvlComboBox.fset_DropDownCount(
  const _value : Integer
);
begin
  PlatformControl.fset_DropDownCount( _value ) ;
end;

function TGIS_PvlComboBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlComboBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

procedure TGIS_PvlComboBox.BeginUpdate;
begin
  PlatformControl.BeginUpdate ;
end;

procedure TGIS_PvlComboBox.EndUpdate;
begin
  PlatformControl.EndUpdate ;
end;

procedure TGIS_PvlComboBox.ItemsClear;
begin
  PlatformControl.ItemsClear ;
end;

procedure TGIS_PvlComboBox.ItemsAdd(
  const _item : String
);
begin
  PlatformControl.ItemsAdd( _item ) ;
end;

function  TGIS_PvlComboBox.IndexOf(
  const _item : String
) : Integer;
begin
  Result := PlatformControl.IndexOf( _item ) ;
end;

{$ENDREGION 'TGIS_PvlComboBox'}

{$REGION 'TGIS_PvlComboEdit'}

procedure TGIS_PvlComboEdit.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ComboEdit') ;
end;

function TGIS_PvlComboEdit.fget_PlatformControl
  : IGIS_PvlComboEdit;
begin
  Result := oPlatform as IGIS_PvlComboEdit ;
end;

function TGIS_PvlComboEdit.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlComboEdit.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlComboEdit.fget_OnKeyPress
  : TGIS_PvlKeyPressEvent;
begin
  Result := PlatformControl.fget_OnKeyPress ;
end;

procedure TGIS_PvlComboEdit.fset_OnKeyPress(
  const _value : TGIS_PvlKeyPressEvent
);
begin
  PlatformControl.fset_OnKeyPress( _value ) ;
end;

function TGIS_PvlComboEdit.fget_ItemsCount
  : Integer;
begin
  Result := PlatformControl.fget_ItemsCount ;
end;

function TGIS_PvlComboEdit.fget_Item(
  const _idx : Integer
) : String;
begin
  Result := PlatformControl.fget_Item( _idx ) ;
end;

function  TGIS_PvlComboEdit.fget_ItemIndex
  : Integer;
begin
  Result := PlatformControl.fget_ItemIndex ;
end;

procedure TGIS_PvlComboEdit.fset_ItemIndex(
  const _value : Integer
);
begin
  PlatformControl.fset_ItemIndex( _value ) ;
end;

function TGIS_PvlComboEdit.fget_Text
  : String;
begin
  Result := PlatformControl.fget_Text ;
end;

procedure TGIS_PvlComboEdit.fset_Text(
  const _value : String
);
begin
  PlatformControl.fset_Text( _value ) ;
end;

function TGIS_PvlComboEdit.fget_Tag
  : NativeInt;
begin
  Result := PlatformControl.fget_Tag ;
end;

procedure TGIS_PvlComboEdit.fset_Tag(
  const _value : NativeInt
);
begin
  PlatformControl.fset_Tag( _value ) ;
end;

function TGIS_PvlComboEdit.fget_Sorted
  : Boolean;
begin
  Result := PlatformControl.fget_Sorted ;
end;

procedure TGIS_PvlComboEdit.fset_Sorted(
  const _value : Boolean
                                 );
begin
  PlatformControl.fset_Sorted( _value ) ;
end;

function TGIS_PvlComboEdit.fget_DropDownCount
  : Integer;
begin
  Result := PlatformControl.fget_DropDownCount ;
end;

procedure TGIS_PvlComboEdit.fset_DropDownCount(
  const _value : Integer
);
begin
  PlatformControl.fset_DropDownCount( _value ) ;
end;

procedure TGIS_PvlComboEdit.BeginUpdate;
begin
  PlatformControl.BeginUpdate ;
end;

procedure TGIS_PvlComboEdit.EndUpdate;
begin
  PlatformControl.EndUpdate ;
end;

procedure TGIS_PvlComboEdit.ItemsClear;
begin
  PlatformControl.ItemsClear ;
end;

procedure TGIS_PvlComboEdit.ItemsAdd(
  const _item : String
);
begin
  PlatformControl.ItemsAdd( _item ) ;
end;

function  TGIS_PvlComboEdit.IndexOf(
  const _item : String
) : Integer;
begin
  Result := PlatformControl.IndexOf( _item ) ;
end;

procedure TGIS_PvlComboEdit.SetFontAlarm;
begin
  PlatformControl.SetFontAlarm ;
end;

procedure TGIS_PvlComboEdit.SetFontDefault;
begin
  PlatformControl.SetFontDefault
end;

procedure TGIS_PvlComboEdit.SetBackgroundAlarm;
begin
  PlatformControl.SetBackgroundAlarm ;
end;

procedure TGIS_PvlComboEdit.SetBackgroundDefault;
begin
  PlatformControl.SetBackgroundDefault
end;

{$ENDREGION 'TGIS_PvlComboEdit'}

{$REGION 'TGIS_PvlSearchBox'}

procedure TGIS_PvlSearchBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'SearchBox') ;
end;

function TGIS_PvlSearchBox.fget_PlatformControl
  : IGIS_PvlSearchBox;
begin
  Result := oPlatform as IGIS_PvlSearchBox ;
end;

function TGIS_PvlSearchBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlSearchBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlSearchBox.fget_OnKeyPress
  : TGIS_PvlKeyPressEvent;
begin
  Result := PlatformControl.fget_OnKeyPress ;
end;

procedure TGIS_PvlSearchBox.fset_OnKeyPress(
  const _value : TGIS_PvlKeyPressEvent
);
begin
  PlatformControl.fset_OnKeyPress( _value ) ;
end;

function TGIS_PvlSearchBox.fget_FontSize
  : Integer ;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlSearchBox.fset_FontSize(
  const _value : Integer
) ;
begin
  PlatformControl.fset_FontSize( _value );
end;

function TGIS_PvlSearchBox.fget_FontStyle
  : TGIS_FontStyles ;
begin
  Result := PlatformControl.fget_FontStyle ;
end;

procedure TGIS_PvlSearchBox.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
begin
  PlatformControl.fset_FontStyle( _value );
end;

function TGIS_PvlSearchBox.fget_FontFamily
  : String ;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlSearchBox.fset_FontFamily(
  const _value : String
) ;
begin
  PlatformControl.fset_FontFamily( _value );
end;

function TGIS_PvlSearchBox.fget_ItemsCount
  : Integer;
begin
  Result := PlatformControl.fget_ItemsCount ;
end;

function TGIS_PvlSearchBox.fget_Item(
  const _idx : Integer
) : String;
begin
  Result := PlatformControl.fget_Item( _idx ) ;
end;

function  TGIS_PvlSearchBox.fget_ItemIndex
  : Integer;
begin
  Result := PlatformControl.fget_ItemIndex ;
end;

procedure TGIS_PvlSearchBox.fset_ItemIndex(
  const _value : Integer
);
begin
  PlatformControl.fset_ItemIndex( _value ) ;
end;

function TGIS_PvlSearchBox.fget_Text
  : String;
begin
  Result := PlatformControl.fget_Text ;
end;

procedure TGIS_PvlSearchBox.fset_Text(
  const _value : String
);
begin
  PlatformControl.fset_Text( _value ) ;
end;

function TGIS_PvlSearchBox.fget_Sorted
  : Boolean;
begin
  Result := PlatformControl.fget_Sorted ;
end;

procedure TGIS_PvlSearchBox.fset_Sorted(
  const _value : Boolean
                                 );
begin
  PlatformControl.fset_Sorted( _value ) ;
end;

function TGIS_PvlSearchBox.fget_DropDownCount
  : Integer;
begin
  Result := PlatformControl.fget_DropDownCount ;
end;

procedure TGIS_PvlSearchBox.fset_DropDownCount(
  const _value : Integer
);
begin
  PlatformControl.fset_DropDownCount( _value ) ;
end;

procedure TGIS_PvlSearchBox.ItemsClear;
begin
  PlatformControl.ItemsClear ;
end;

procedure TGIS_PvlSearchBox.ItemsAdd(
  const _item : String
);
begin
  PlatformControl.ItemsAdd( _item ) ;
end;

function  TGIS_PvlSearchBox.IndexOf(
  const _item : String
) : Integer;
begin
  Result := PlatformControl.IndexOf( _item ) ;
end;

procedure TGIS_PvlSearchBox.SetFontAlarm;
begin
  PlatformControl.SetFontAlarm ;
end;

procedure TGIS_PvlSearchBox.SetFontDefault;
begin
  PlatformControl.SetFontDefault
end;

procedure TGIS_PvlSearchBox.SetBackgroundAlarm;
begin
  PlatformControl.SetBackgroundAlarm ;
end;

procedure TGIS_PvlSearchBox.SetBackgroundDefault;
begin
  PlatformControl.SetBackgroundDefault
end;
{$ENDREGION 'TGIS_PvlSearchBox'}

{$REGION 'TGIS_PvlTreeNode'}

constructor TGIS_PvlTreeNode.Create(
  const _parent : TGIS_PvlTreeNode
);
begin
  oParent := _parent ;
  lstNodes := TList< TGIS_PvlTreeNode >.Create ;
  oPlatform := _parent.TreeControl.Context.oPlatform.CreateObject(self, 'TreeNode') ;
  oTreeControl := _parent.TreeControl ;
end;

constructor TGIS_PvlTreeNode.Create(
  const _parent : TGIS_PvlTree
);
begin
  oParent := nil ;
  lstNodes := TList< TGIS_PvlTreeNode >.Create ;
  oPlatform := _parent.oContext.oPlatform.CreateObject(_parent, 'TreeNode') ;
  oTreeControl := _parent ;
end;

procedure TGIS_PvlTreeNode.doDestroy ;
var
  tmp : TObject ;
begin
  Clear ;
  FreeObject( lstNodes ) ;

  tmp := oPlatform as TObject ;
  FreeObject( tmp ) ;
  oPlatform := nil ;

  inherited;
end;

function TGIS_PvlTreeNode.fget_PlatformControl
  : IGIS_PvlTreeNode ;
begin
  Result := oPlatform as IGIS_PvlTreeNode ;
end;

function TGIS_PvlTreeNode.fget_Caption
  : String;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlTreeNode.fset_Caption(
  const _value : String
);
begin
  PlatformControl.fset_Caption( _value ) ;
end;

function TGIS_PvlTreeNode.fget_Expanded
  : Boolean;
begin
  Result := PlatformControl.fget_Expanded ;
end;

procedure TGIS_PvlTreeNode.fset_Expanded(
  const _value : Boolean
);
begin
  PlatformControl.fset_Expanded( _value ) ;
end;

function TGIS_PvlTreeNode.fget_Parent
  : TGIS_PvlTreeNode;
begin
  Result := oParent ;
end;

function TGIS_PvlTreeNode.fget_Count
  : Integer;
begin
  Result := lstNodes.Count ;
end;

function TGIS_PvlTreeNode.fget_NativeControl
  : TObject;
begin
  Result := PlatformControl.fget_NativeControl ;
end;

procedure TGIS_PvlTreeNode.fset_NativeControl(
  const _value : TObject
);
begin
  PlatformControl.fset_NativeControl( _value ) ;
end;

function TGIS_PvlTreeNode.fget_Node(
  const _index : Integer
) : TGIS_PvlTreeNode;
begin
  if ( _index < 0 )  or ( _index >= lstNodes.Count ) then
    Result := nil
  else
    Result := lstNodes[_index] ;
end;


function TGIS_PvlTreeNode.CreateNode(
  const _caption: String
) : TGIS_PvlTreeNode ;
begin
  Result := CreateNode( _caption, Count ) ;
end;

function TGIS_PvlTreeNode.CreateNode(
  const _caption    : String ;
  const _index      : Integer
) : TGIS_PvlTreeNode ;
begin
  Result := TGIS_PvlTreeNode.Create( TreeControl );

  doCreateNode( Result, _caption, _index ) ;

  Result.Caption := _caption ;
end;

procedure TGIS_PvlTreeNode.doCreateNode(
  const _node       : TGIS_PvlTreeNode ;
  const _caption    : String ;
  const _index      : Integer
) ;
begin
  _node.oParent := Self ;
  _node.oTreeControl := oTreeControl ;
  lstNodes.Add( _node );

  PlatformControl.doCreateNode( _node, _caption, _index);
end;

procedure TGIS_PvlTreeNode.doRemoveNode(
  const _node       : TGIS_PvlTreeNode
) ;
begin
  if Parent.lstNodes.Contains( _node ) then
    Parent.lstNodes.Remove( _node ) ;

  PlatformControl.doRemoveNode( _node );
end;

procedure TGIS_PvlTreeNode.doDeleteNode(
  const _node       : TGIS_PvlTreeNode
) ;
begin
  if Parent.lstNodes.Contains( _node ) then
    Parent.lstNodes.Remove( _node ) ;

  PlatformControl.doDeleteNode( _node );
end;

procedure TGIS_PvlTreeNode.doMoveNode(
  const _node       : TGIS_PvlTreeNode;
  const _newParent  : TGIS_PvlTreeNode ;
  const _index      : Integer
);
begin
  InternalRemove ;

  Self.oParent := _newParent ;

  _node.oTreeControl := oTreeControl ;
  Parent.lstNodes.Add( _node );

  PlatformControl.doMoveNode( _node, _newParent, _newParent.Count ) ;
end;

procedure TGIS_PvlTreeNode.InternalRemove ;
begin
  doRemoveNode( self );
end;

procedure TGIS_PvlTreeNode.DeleteNode ;
begin
  doDeleteNode( self );
end;

procedure TGIS_PvlTreeNode.MoveNode(
  const _newParent  : TGIS_PvlTreeNode ;
  const _index      : Integer
);
begin
  doMoveNode( Self, _newParent, _index ) ;
end;

procedure TGIS_PvlTreeNode.MoveNode(
  const _newParent: TGIS_PvlTreeNode
) ;
begin
  doMoveNode( Self, _newParent, _newParent.Count ) ;
end;

procedure TGIS_PvlTreeNode.Clear ;
var
  nd : TGIS_PvlTreeNode ;
begin
  for nd in lstNodes do begin
    FreeObjectNotNil( nd ) ;
  end;
  lstNodes.Clear ;
end;

{$ENDREGION 'TGIS_PvlTreeItem'}

{$REGION 'TGIS_PvlTree'}

procedure TGIS_PvlTree.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'Tree') ;
  CreateRoot ;
end;

function TGIS_PvlTree.fget_PlatformControl
  : IGIS_PvlTree ;
begin
  Result := oPlatform as IGIS_PvlTree ;
end;

function TGIS_PvlTree.fget_Root
  : TGIS_PvlTreeNode;
begin
  Result := PlatformControl.fget_Root ;
end;

function TGIS_PvlTree.fget_Selected
  : TGIS_PvlTreeNode;
begin
  Result := PlatformControl.fget_Selected ;
end;

procedure TGIS_PvlTree.fset_Selected(
  const _value : TGIS_PvlTreeNode
);
begin
  PlatformControl.fset_Selected( _value ) ;
end;

function TGIS_PvlTree.fget_SelectedIndex
  : Integer;
begin
  Result := PlatformControl.fget_SelectedIndex ;
end;

procedure TGIS_PvlTree.fset_OnSelectChange(
  const _value: TGIS_PvlEvent
) ;
begin
  PlatformControl.fset_OnSelectChange( _value ) ;
end;

function TGIS_PvlTree.fget_OnSelectChange
  : TGIS_PvlEvent ;
begin
  Result := PlatformControl.fget_OnSelectChange ;
end;

procedure TGIS_PvlTree.CreateRoot ;
begin
  PlatformControl.CreateRoot ;
  Root.oTreeControl := self;
end;

{$ENDREGION 'TGIS_PvlTree'}

{$REGION 'TGIS_PvlColorPreview'}

procedure TGIS_PvlColorPreview.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ColorPreview') ;
end;

function TGIS_PvlColorPreview.fget_PlatformControl
  : IGIS_PvlColorPreview ;
begin
  Result := oPlatform as IGIS_PvlColorPreview ;
end;

function TGIS_PvlColorPreview.fget_Color
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_Color ;
end;

procedure TGIS_PvlColorPreview.fset_Color(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_Color( _value ) ;
end;

function TGIS_PvlColorPreview.fget_Border
  : Boolean;
begin
  Result := PlatformControl.fget_Border ;
end;

procedure TGIS_PvlColorPreview.fset_Border(
  const _value : Boolean
);
begin
  PlatformControl.fset_Border( _value ) ;
end;

{$ENDREGION 'TGIS_PvlColorPreview'}

{$REGION 'TGIS_PvlColorWheel'}

procedure TGIS_PvlColorWheel.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ColorWheel') ;
end;

function TGIS_PvlColorWheel.fget_PlatformControl
  : IGIS_PvlColorWheel ;
begin
  Result := oPlatform as IGIS_PvlColorWheel ;
end;

function TGIS_PvlColorWheel.fget_Color
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_Color ;
end;

procedure TGIS_PvlColorWheel.fset_Color(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_Color( _value )
end;

function TGIS_PvlColorWheel.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlColorWheel.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlColorWheel.HueToRGB(
  const _hue : Double
) : TGIS_PvlRGBVal ;
begin
  Result := PlatformControl.HueToRGB( _hue ) ;
end;

{$ENDREGION 'TGIS_PvlColorWheel'}

{$REGION 'TGIS_PvlColorBar'}

procedure TGIS_PvlColorBar.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ColorBar') ;
end;

function TGIS_PvlColorBar.fget_PlatformControl
  : IGIS_PvlColorBar ;
begin
  Result := oPlatform as IGIS_PvlColorBar ;
end;

function TGIS_PvlColorBar.fget_Value
  : Double ;
begin
  Result := PlatformControl.fget_Value ;
end;

procedure TGIS_PvlColorBar.fset_Value(
  const _value : Double
) ;
begin
  PlatformControl.fset_Value( _value ) ;
end;

function TGIS_PvlColorBar.fget_Color
  : TGIS_Color ;
begin
  Result := PlatformControl.fget_Color ;
end;

procedure TGIS_PvlColorBar.fset_Color(
  const _value : TGIS_Color
) ;
begin
  PlatformControl.fset_Color( _value ) ;
end;

function  TGIS_PvlColorBar.fget_Alpha
  : Boolean ;
begin
  Result := PlatformControl.fget_Alpha ;
end;

procedure TGIS_PvlColorBar.fset_Alpha(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Alpha( _value ) ;
end;

function TGIS_PvlColorBar.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlColorBar.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

{$ENDREGION 'TGIS_PvlColorBar'}

{$REGION 'TGIS_PvlCustomComboBox'}

function TGIS_PvlCustomComboBox.fget_PlatformControl
  : IGIS_PvlCustomComboBox ;
begin
  Result := oPlatform as IGIS_PvlCustomComboBox ;
end;

function TGIS_PvlCustomComboBox.fget_Value
  : String;
begin
  Result := PlatformControl.fget_Value ;
end;

procedure TGIS_PvlCustomComboBox.fset_Value(
  const _value : String
);
begin
  PlatformControl.fset_Value( _value ) ;
end;

function TGIS_PvlCustomComboBox.fget_Fields
  : TStringList ;
begin
  Result := PlatformControl.fget_Fields ;
end;

procedure TGIS_PvlCustomComboBox.fset_Fields(
  const _value : TStringList
) ;
begin
  PlatformControl.fset_Fields( _value ) ;
end;

function TGIS_PvlCustomComboBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlCustomComboBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value ) ;
end;


{$ENDREGION 'TGIS_PvlCustomComboBox'}

{$REGION 'TGIS_PvlSizeComboBox'}

procedure TGIS_PvlSizeComboBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'SizeComboBox') ;
end;

function TGIS_PvlSizeComboBox.fget_PlatformControl
  : IGIS_PvlSizeComboBox ;
begin
  Result := oPlatform as IGIS_PvlSizeComboBox ;
end;

procedure TGIS_PvlSizeComboBox.Fill(
  const _forSymbol : Boolean ;
  const _forLine   : Boolean ;
  const _field     : Boolean ;
  const _renderer  : Boolean
) ;
begin
  PlatformControl.Fill( _forSymbol, _forLine, _field, _renderer ) ;
end;

procedure TGIS_PvlSizeComboBox.FillRealWorldUnits(
  const _field : Boolean
) ;
begin
  PlatformControl.FillRealWorldUnits( _field ) ;
end;

procedure TGIS_PvlSizeComboBox.FillAggregation ;
begin
  PlatformControl.FillAggregation ;
end;

{$ENDREGION 'TGIS_PvlSizeComboBox'}

{$REGION 'TGIS_PvlColorComboBox'}

procedure TGIS_PvlColorComboBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ColorComboBox') ;
end;

function TGIS_PvlColorComboBox.fget_PlatformControl
  : IGIS_PvlColorComboBox ;
begin
  Result := oPlatform as IGIS_PvlColorComboBox ;
end;

procedure TGIS_PvlColorComboBox.Fill(
  const _field    : Boolean ;
  const _renderer : Boolean
) ;
begin
  PlatformControl.Fill( _field, _renderer ) ;
end ;

procedure TGIS_PvlColorComboBox.Fill(
  const _field      : Boolean ;
  const _renderer   : Boolean ;
  const _values     : TStringList
) ;
begin
  PlatformControl.Fill( _field, _renderer, _values ) ;
end;

{$ENDREGION 'TGIS_PvlColorComboBox'}

{$REGION 'TGIS_PvlComboBoxHelperGetBitmapEventArgs'}
  constructor TGIS_PvlComboBoxHelperGetBitmapEventArgs.Create(
    _value     : String  ;
    _forecolor : TGIS_Color   ;
    _font      : TGIS_Font    ;
    _width     : Integer ;
    _height    : Integer
  ) ;
  begin
    inherited Create ;
    FValue     := _value      ;
    FForeColor := _forecolor  ;
    FFont      := _font       ;
    FWidth     := _width      ;
    FHeight    := _height     ;
  end ;
{$ENDREGION 'TGIS_PvlComboBoxHelperGetBitmapEventArgs'}

{$REGION 'TGIS_PvlCustomBitmapComboBox'}

function TGIS_PvlCustomBitmapComboBox.fget_PlatformControl
  : IGIS_PvlCustomBitmapComboBox ;
begin
  Result := oPlatform as IGIS_PvlCustomBitmapComboBox ;
end;


function TGIS_PvlCustomBitmapComboBox.fget_Type
  : TGIS_PvlCustomBitmapType ;
begin
  Result := PlatformControl.fget_Type ;
end;

function TGIS_PvlCustomBitmapComboBox.fget_GetBitmapEvent
  : TGIS_PvlComboBoxHelperGetBitmapEvent;
begin
  Result := PlatformControl.fget_GetBitmapEvent ;
end;

procedure TGIS_PvlCustomBitmapComboBox.fset_GetBitmapEvent(
  const _value : TGIS_PvlComboBoxHelperGetBitmapEvent
) ;
begin
  PlatformControl.fset_GetBitmapEvent( _value );
end;

procedure TGIS_PvlCustomBitmapComboBox.FillStyle(
  const _hasSymbol  : Boolean
) ;
begin
  PlatformControl.FillStyle( _hasSymbol );
end;

procedure TGIS_PvlCustomBitmapComboBox.FillPattern(
  const _hasSymbol : Boolean
) ;
begin
  PlatformControl.FillPattern( _hasSymbol );
end;

procedure TGIS_PvlCustomBitmapComboBox.FillMarker(
  const _hasSymbol  : Boolean
) ;
begin
  PlatformControl.FillMarker( _hasSymbol );
end;

procedure TGIS_PvlCustomBitmapComboBox.FillShield ;
begin
  PlatformControl.FillShield;
end;

{$ENDREGION 'TGIS_PvlCustomBitmapComboBox'}

{$REGION 'TGIS_PvlColorRampComboBox'}

procedure TGIS_PvlColorRampComboBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'ColorRampComboBox') ;
end;

function TGIS_PvlColorRampComboBox.fget_PlatformControl
  : IGIS_PvlColorRampComboBox ;
begin
  Result := oPlatform as IGIS_PvlColorRampComboBox ;
end;

function TGIS_PvlColorRampComboBox.fget_Mode
  : TGIS_ColorMapMode;
begin
  Result := PlatformControl.fget_Mode;
end;

procedure TGIS_PvlColorRampComboBox.fset_Mode(
  const _value : TGIS_ColorMapMode
);
begin
  PlatformControl.fset_Mode( _value );
end;

function TGIS_PvlColorRampComboBox.fget_Index
  : Integer;
begin
  Result := PlatformControl.fget_Index ;
end;

procedure TGIS_PvlColorRampComboBox.fset_Index(
  const _value : Integer
);
begin
  PlatformControl.fset_Index( _value );
end;

function TGIS_PvlColorRampComboBox.fget_Item(
  const _idx : Integer
) : String;
begin
  Result := PlatformControl.fget_Item( _idx ) ;
end;

function TGIS_PvlColorRampComboBox.fget_ItemCount
  : Integer;
begin
  Result := PlatformControl.fget_ItemCount;
end;

function TGIS_PvlColorRampComboBox.fget_ColorSchemas
  : TGIS_ColorSchemas;
begin
  Result := PlatformControl.fget_ColorSchemas ;
end;

procedure TGIS_PvlColorRampComboBox.fset_ColorSchemas (
  const _value : TGIS_ColorSchemas
);
begin
   PlatformControl.fset_ColorSchemas( _value )
end;

function TGIS_PvlColorRampComboBox.fget_Reverse
  : Boolean ;
begin
  Result := PlatformControl.fget_Reverse
end;

procedure TGIS_PvlColorRampComboBox.fset_Reverse(
  const _value : Boolean
);
begin
  PlatformControl.fset_Reverse( _value );
end;

function  TGIS_PvlColorRampComboBox.fget_ShowNames
  : Boolean ;
begin
  Result := PlatformControl.fget_ShowNames ;
end;

procedure TGIS_PvlColorRampComboBox.fset_ShowNames(
  const _value : Boolean
);
begin
  PlatformControl.fset_ShowNames( _value ) ;
end;


function  TGIS_PvlColorRampComboBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlColorRampComboBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  PlatformControl.fset_OnChange( _value )
end;

function TGIS_PvlColorRampComboBox.Value(
  const _subClass : Integer
) : TGIS_ColorMapArray;
begin
  Result := PlatformControl.Value( _subClass );
end;

procedure TGIS_PvlColorRampComboBox.Fill;
begin
  PlatformControl.Fill ;
end;

procedure TGIS_PvlColorRampComboBox.Lock;
begin
  PlatformControl.Lock ;
end;

procedure TGIS_PvlColorRampComboBox.Unlock;
begin
  PlatformControl.Unlock ;
end;

{$ENDREGION 'TGIS_PvlColorRampComboBox'}

{$REGION 'TGIS_PvlCheckBox'}

procedure TGIS_PvlCheckBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'CheckBox') ;
end;

function TGIS_PvlCheckBox.fget_PlatformControl
  : IGIS_PvlCheckBox ;
begin
  Result := oPlatform as IGIS_PvlCheckBox ;
end;

procedure TGIS_PvlCheckBox.initControl ;
begin
  inherited ;
end;

function TGIS_PvlCheckBox.fget_Checked
  : Boolean ;
begin
  Result := PlatformControl.fget_Checked
end;

procedure TGIS_PvlCheckBox.fset_Checked(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Checked( _value ) ;
end;

function TGIS_PvlCheckBox.fget_OnChange
  : TGIS_PvlEvent ;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlCheckBox.fset_OnChange(
  const _value: TGIS_PvlEvent
) ;
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlCheckBox.fget_Caption
  : String ;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlCheckBox.fset_Caption(
  const _value : String
) ;
begin
  PlatformControl.fset_Caption( _value );
end;

function TGIS_PvlCheckBox.fget_FontSize
  : Integer ;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlCheckBox.fset_FontSize(
  const _value : Integer
) ;
begin
  PlatformControl.fset_FontSize( _value );
end;

function TGIS_PvlCheckBox.fget_FontStyle
  : TGIS_FontStyles ;
begin
  Result := PlatformControl.fget_FontStyle ;
end;

procedure TGIS_PvlCheckBox.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
begin
  PlatformControl.fset_FontStyle( _value );
end;

function TGIS_PvlCheckBox.fget_FontFamily
  : String ;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlCheckBox.fset_FontFamily(
  const _value : String
) ;
begin
  PlatformControl.fset_FontFamily( _value );
end;

procedure TGIS_PvlCheckBox.SetFocus;
begin
  PlatformControl.SetFocus;
end;

{$ENDREGION 'TGIS_PvlCheckBox'}

{$REGION 'TGIS_PvlRadioButton'}

procedure TGIS_PvlRadioButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oPlatform := _context.oPlatform.CreateObject(self, 'RadioButton') ;
end;

function TGIS_PvlRadioButton.fget_PlatformControl
  : IGIS_PvlRadioButton ;
begin
  Result := oPlatform as IGIS_PvlRadioButton ;
end;

procedure TGIS_PvlRadioButton.initControl ;
begin
  inherited ;
  Caption := 'TGIS_PvlRadioButton' ;
end;

function TGIS_PvlRadioButton.fget_Checked
  : Boolean ;
begin
  Result := PlatformControl.fget_Checked
end;

procedure TGIS_PvlRadioButton.fset_Checked(
  const _value : Boolean
) ;
begin
  PlatformControl.fset_Checked( _value ) ;
end;

function TGIS_PvlRadioButton.fget_Group
  : String ;
begin
  Result := FGroup ;
  // any additional handling
  PlatformControl.fget_Group ;
end;

procedure TGIS_PvlRadioButton.fset_Group(
  const _value : String
) ;
var
  sgroup : String ;
begin
  FGroup := _value ;

  if oContext.oGroups.TryGetValue( self, sgroup ) then
    oContext.oGroups.Remove( self ) ;


  oContext.oGroups.Add( self, FGroup ) ;

  // any additional handling
  PlatformControl.fset_Group( _value );
end;

function TGIS_PvlRadioButton.fget_OnChange
  : TGIS_PvlEvent ;
begin
  Result := PlatformControl.fget_OnChange ;
end;

procedure TGIS_PvlRadioBUtton.fset_OnChange(
  const _value: TGIS_PvlEvent
) ;
begin
  PlatformControl.fset_OnChange( _value ) ;
end;

function TGIS_PvlRadioButton.fget_Caption
  : String ;
begin
  Result := PlatformControl.fget_Caption ;
end;

procedure TGIS_PvlRadioButton.fset_Caption(
  const _value : String
) ;
begin
  PlatformControl.fset_Caption( _value );
end;

function TGIS_PvlRadioButton.fget_FontSize
  : Integer ;
begin
  Result := PlatformControl.fget_FontSize ;
end;

procedure TGIS_PvlRadioButton.fset_FontSize(
  const _value : Integer
) ;
begin
  PlatformControl.fset_FontSize( _value );
end;

function TGIS_PvlRadioButton.fget_FontStyle
  : TGIS_FontStyles ;
begin
  Result := PlatformControl.fget_FontStyle ;
end;

procedure TGIS_PvlRadioButton.fset_FontStyle(
  const _value : TGIS_FontStyles
) ;
begin
  PlatformControl.fset_FontStyle( _value );
end;

function TGIS_PvlRadioButton.fget_FontFamily
  : String ;
begin
  Result := PlatformControl.fget_FontFamily ;
end;

procedure TGIS_PvlRadioButton.fset_FontFamily(
  const _value : String
) ;
begin
  PlatformControl.fset_FontFamily( _value );
end;

{$IFDEF JAVA}
  function TGIS_PvlRadioButton.compareTo(
    _button : TGIS_PvlRadioButton
  ) : Integer ;
  begin
    Result := CompareStr( Caption, _button.Caption ) ;
  end;
{$ENDIF}

procedure TGIS_PvlRadioButton.SetFocus;
begin
  PlatformControl.SetFocus;
end;

procedure TGIS_PvlRadioButton.UncheckGroup(
  const _button : TGIS_PvlRadioButton
) ;
var
  elm : TPair<TGIS_PvlRadioButton, String> ;
begin
  for elm in oContext.oGroups do begin
    if elm.Value = _button.Group then
      if elm.Key <> _button then
        TGIS_PvlRadioButton( elm.Key ).Checked := False
      else
        _button.Checked := True ;
  end;
end;

{$ENDREGION 'TGIS_PvlRadioButton'}

{$REGION 'TGIS_PvlSystemForm'}

constructor TGIS_PvlSystemForm.Create(
  _context: TGIS_PvlContext
) ;
begin
  oParent := _context.NativeParent ;
  oFormInstance := TGIS_PvlContext.InitializeSystemForm( _context, self ) ;

//  if _owner is TGIS_PvlContext then
    _context.oList.Add( self ) ;

  ownFormInstance := True ;
end;

constructor TGIS_PvlSystemForm.Create(
        _context: TGIS_PvlContext;
  const _self   : TGIS_PvlSystemForm
) ;
begin
  oFormInstance := _self ;
  ownFormInstance := False ;
end;

procedure TGIS_PvlSystemForm.doDestroy ;
begin
  if ownFormInstance then
    FreeObject( oFormInstance ) ;
  inherited ;
end;

{$ENDREGION 'TGIS_PvlSystemForm'}

{$REGION 'TGIS_PvlOptionDialog'}

constructor TGIS_PvlOptionDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlOptionDialog.Create(
        _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_PvlOptionDialog.fset_Text(
  const _value: string
) ;
begin
  TGIS_PvlOptionDialog( oFormInstance ).Text := _value ;
end;

function TGIS_PvlOptionDialog.fget_Text
  : string ;
begin
  Result := TGIS_PvlOptionDialog( oFormInstance ).Text ;
end;

function TGIS_PvlOptionDialog.Execute
  : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlOptionDialog( oFormInstance ).Execute ;
end;

{$ENDREGION 'TGIS_PvlOptionDialog'}

{$REGION 'TGIS_PvlMessageDialog'}

constructor TGIS_PvlMessageDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlMessageDialog.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_PvlMessageDialog.fset_Text(
  const _value: String
) ;
begin
  TGIS_PvlMessageDialog( oFormInstance ).Text := _value ;
end;

function TGIS_PvlMessageDialog.fget_Text
  : String ;
begin
  Result := TGIS_PvlMessageDialog( oFormInstance ).Text ;
end;

procedure TGIS_PvlMessageDialog.fset_Title(
  const _value: String
) ;
begin
  TGIS_PvlMessageDialog( oFormInstance ).Title := _value ;
end;

function TGIS_PvlMessageDialog.fget_Title
  : String ;
begin
  Result := TGIS_PvlMessageDialog( oFormInstance ).Title ;
end;

procedure TGIS_PvlMessageDialog.Execute
 ;
begin
  TGIS_PvlMessageDialog( oFormInstance ).Execute ;
end;

{$ENDREGION 'TGIS_PvlMessageDialog'}

{$REGION 'TGIS_PvlInfoDialog'}

constructor TGIS_PvlInfoDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
end;

constructor TGIS_PvlInfoDialog.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

{$ENDREGION 'TGIS_PvlInfoDialog'}

{$REGION 'TGIS_PvlWarningDialog'}

constructor TGIS_PvlWarningDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
end;

constructor TGIS_PvlWarningDialog.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

{$ENDREGION 'TGIS_PvlWarningDialog'}

{$REGION 'TGIS_PvlErrorDialog'}

constructor TGIS_PvlErrorDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
end;

constructor TGIS_PvlErrorDialog.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

{$ENDREGION 'TGIS_PvlErrorDialog'}

{$REGION 'TGIS_PvlMessages'}

class function TGIS_PvlMessages.ShowOption(
  const _message  : string;
  const _context  : TGIS_PvlContext
) : TGIS_PvlModalResult ;
var
  dlg : TGIS_PvlOptionDialog ;
begin
  dlg := TGIS_PvlOptionDialog.Create( _context ) ;
  dlg.Text := _message ;
  Result := dlg.Execute ;
end;

class procedure TGIS_PvlMessages.ShowInfo(
  const _title    : string ;
  const _message  : string ;
  const _context  : TGIS_PvlContext
) ;
var
  dlg : TGIS_PvlInfoDialog ;
begin
  dlg := TGIS_PvlInfoDialog.Create( _context ) ;
  dlg.Text := _message ;
  dlg.Title := _title ;
  dlg.Execute ;
end;

class procedure TGIS_PvlMessages.ShowInfo(
  const _message  : string;
  const _context  : TGIS_PvlContext
) ;
begin
  ShowInfo( 'Info', _message, _context ) ;
end;

class procedure TGIS_PvlMessages.ShowError(
  const _title    : string ;
  const _message  : string ;
  const _context  : TGIS_PvlContext
) ;
var
  dlg : TGIS_PvlErrorDialog ;
begin
  dlg := TGIS_PvlErrorDialog.Create( _context ) ;
  dlg.Text := _message ;
  dlg.Title := _title ;
  dlg.Execute ;
end;

class procedure TGIS_PvlMessages.ShowError(
  const _message  : string;
  const _context  : TGIS_PvlContext
) ;
begin
  ShowError( 'Error', _message, _context ) ;
end;

class procedure TGIS_PvlMessages.ShowWarning(
  const _title    : string ;
  const _message  : string ;
  const _context  : TGIS_PvlContext
) ;
var
  dlg : TGIS_PvlWarningDialog ;
begin
  dlg := TGIS_PvlWarningDialog.Create( _context ) ;
  dlg.Text := _message ;
  dlg.Title := _title ;
  dlg.Execute ;
end;

class procedure TGIS_PvlMessages.ShowWarning(
  const _message  : string;
  const _context  : TGIS_PvlContext
) ;
begin
  ShowWarning( 'Warning', _message, _context ) ;
end;

{$ENDREGION 'TGIS_PvlMessages'}

{$REGION 'TGIS_PvlOpenSaveDialog'}

constructor TGIS_PvlOpenSaveDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlOpenSaveDialog.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

procedure TGIS_PvlOpenSaveDialog.fset_Filter(
  const _value: string
) ;
begin
  TGIS_PvlOpenSaveDialog( oFormInstance ).Filter := _value ;
end;

function TGIS_PvlOpenSaveDialog.fget_Filter
  : string ;
begin
  Result := TGIS_PvlOpenSaveDialog( oFormInstance ).Filter ;
end;

procedure TGIS_PvlOpenSaveDialog.fset_FileName(
  const _value: string
) ;
begin
  TGIS_PvlOpenSaveDialog( oFormInstance ).FileName := _value ;
end;

function TGIS_PvlOpenSaveDialog.fget_FileName
  : string ;
begin
  Result := TGIS_PvlOpenSaveDialog( oFormInstance ).FileName ;
end;

function TGIS_PvlOpenSaveDialog.Execute
  : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlOpenSaveDialog( oFormInstance ).Execute ;
end;

{$ENDREGION 'TGIS_PvlOpenSaveDialog'}

{$REGION 'TGIS_PvlSelectFolderDialog'}
constructor TGIS_PvlSelectFolderDialog.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlSelectFolderDialog.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

function TGIS_PvlSelectFolderDialog.fget_Directory
  : string ;
begin
  Result := TGIS_PvlSelectFolderDialog( oFormInstance ).Directory ;
end;

procedure TGIS_PvlSelectFolderDialog.fset_Directory(
  const _value: string
) ;
begin
  TGIS_PvlSelectFolderDialog( oFormInstance ).Directory := _value ;
end;

function TGIS_PvlSelectFolderDialog.Execute
  : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlSelectFolderDialog( oFormInstance ).Execute ;
end;

{$ENDREGION 'TGIS_PvlSelectFolderDialog'}

{$REGION 'TGIS_PvlControlPrintPreviewSimple'}

constructor TGIS_PvlControlPrintPreviewSimple.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlControlPrintPreviewSimple.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

function TGIS_PvlControlPrintPreviewSimple.fget_GIS_Viewer
  : IGIS_Viewer;
begin
  Result := TGIS_PvlControlPrintPreviewSimple( oFormInstance ).fget_GIS_Viewer ;
end;

procedure TGIS_PvlControlPrintPreviewSimple.fset_GIS_Viewer(
  const _GIS: IGIS_Viewer
) ;
begin
  TGIS_PvlControlPrintPreviewSimple( oFormInstance ).fset_GIS_Viewer( _GIS ) ;
end;

procedure TGIS_PvlControlPrintPreviewSimple.Preview ;
var
  scale : Double ;
begin
  scale := 0 ;
  TGIS_PvlControlPrintPreviewSimple( oFormInstance ).Preview( nil, '', scale ) ;
end ;

procedure TGIS_PvlControlPrintPreviewSimple.Preview(
  var _scale : Double
) ;
begin
  TGIS_PvlControlPrintPreviewSimple( oFormInstance ).Preview( nil, '', _scale ) ;
end ;

procedure TGIS_PvlControlPrintPreviewSimple.Preview(
  const _printManager : TGIS_PrintManagerAbstract
) ;
var
  scale : Double ;
begin
  scale := 0 ;
  TGIS_PvlControlPrintPreviewSimple( oFormInstance ).Preview( _printManager, '', scale ) ;
end ;

procedure TGIS_PvlControlPrintPreviewSimple.Preview(
  const _printManager : TGIS_PrintManagerAbstract ;
  var   _scale        : Double
) ;
begin
  TGIS_PvlControlPrintPreviewSimple( oFormInstance ).Preview( _printManager, '', _scale ) ;
end ;

procedure TGIS_PvlControlPrintPreviewSimple.Preview(
  const _printManager : TGIS_PrintManagerAbstract ;
  const _customPage   : String ;
  var   _scale        : Double
) ;
begin
  TGIS_PvlControlPrintPreviewSimple( oFormInstance ).Preview( _printManager, _customPage, _scale ) ;
end ;

{$ENDREGION 'TGIS_PvlControlPrintPreviewSimple'}

{$REGION 'TGIS_PvlLineSymbolEditor'}

constructor TGIS_PvlLineSymbolEditor.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlLineSymbolEditor.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

function TGIS_PvlLineSymbolEditor.fget_Symbol
  : String ;
begin
  Result := TGIS_PvlLineSymbolEditor( oFormInstance ).fget_Symbol ;
end;

procedure TGIS_PvlLineSymbolEditor.fset_Symbol (
  const _symbol : String
) ;
begin
  TGIS_PvlLineSymbolEditor( oFormInstance ).fset_Symbol( _symbol ) ;
end;

function TGIS_PvlLineSymbolEditor.Execute(
  const _path   : String        ;
  const _onhelp : TGIS_HelpEvent;
  const _proc   : TGIS_Proc
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlLineSymbolEditor( oFormInstance ).Execute( _path, _onhelp, _proc ) ;
end;

function TGIS_PvlLineSymbolEditor.Execute(
  const _path   : String       ;
  const _proc   : TGIS_Proc
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlLineSymbolEditor( oFormInstance ).Execute( _path, _proc ) ;
end;

{$ENDREGION 'TGIS_PvLineSymbolEditor'}

{$REGION 'TGIS_PvlControlCSSystem'}

constructor TGIS_PvlControlCSSystem.Create(
  _context : TGIS_PvlContext
) ;
begin
  inherited ;
  oParent := _context.NativeParent ;
end;

constructor TGIS_PvlControlCSSystem.Create(
        _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) ;
begin
  inherited ;
end;

function TGIS_PvlControlCSSystem.fget_CS
  : TGIS_CSCoordinateSystem ;
begin
  Result := TGIS_PvlControlCSSystem( oFormInstance ).CS ;
end;

function TGIS_PvlControlCSSystem.Execute(
  const _cs     : TGIS_CSCoordinateSystem ;
  const _onhelp : TGIS_HelpEvent;
  const _proc   : TGIS_Proc
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlControlCSSystem( oFormInstance ).Execute(
    _cs,
    _onhelp,
    _proc
  ) ;
end;

function TGIS_PvlControlCSSystem.Execute(
  const _cs     : TGIS_CSCoordinateSystem ;
  const _proc   : TGIS_Proc
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlControlCSSystem( oFormInstance ).Execute(
    _cs,
    _proc
  ) ;
end;

function TGIS_PvlControlCSSystem.Execute(
  const _cs         : TGIS_CSCoordinateSystem;
  const _area_epsg  : Integer ;
  const _area_extent: TGIS_Extent ;
  const _onhelp     : TGIS_HelpEvent ;
  const _proc       : TGIS_Proc
) : TGIS_PvlModalResult ;
begin
  Result := TGIS_PvlControlCSSystem( oFormInstance ).Execute(
    _cs,
    _area_epsg,
    _area_extent,
    _onhelp,
    _proc
  ) ;
end;

{$ENDREGION 'TGIS_PvlControlCSSystem'}

{$REGION 'TGIS_PvlContextBase'}

constructor TGIS_PvlContextBase.Create(
  const _parent       : TObject;
  const _ppi          : Integer;
  const _ppifix       : Single;
  const _canvasscale  : Single;
  const _righttoleft  : Boolean;
  const _context      : TGIS_PvlContext
) ;
begin
  //For safe inheritance only
end;

constructor TGIS_PvlContextBase.Create(
  const _parent : TObject;
  const _context: TGIS_PvlContext
) ;
begin
  //For safe inheritance only
end;

constructor TGIS_PvlContextBase.Create(
  const _parent    : TObject;
  const _context   : TGIS_PvlContext;
  const _refcontext: TGIS_PvlContext
) ;
begin
  //For safe inheritance only
end;

class function TGIS_PvlContextBase.Support(
  const _parent: TObject
): Boolean ;
begin
  //For safe inheritance only
  Result := False ;
end;

class function TGIS_PvlContextBase.CreateOpenDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlOpenDialog
): TGIS_PvlOpenDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateSaveDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlSaveDialog
): TGIS_PvlSaveDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateSelectFolderDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlSelectFolderDialog
): TGIS_PvlSelectFolderDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateOptionDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlOptionDialog
): TGIS_PvlOptionDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateInfoDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlInfoDialog
): TGIS_PvlInfoDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateWarningDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlWarningDialog
): TGIS_PvlWarningDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateErrorDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlErrorDialog
): TGIS_PvlErrorDialog ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateCSDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlControlCSSystem
): TGIS_PvlControlCSSystem ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreateLineSymbologyDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlLineSymbolEditor
): TGIS_PvlLineSymbolEditor ;
begin
  //For safe inheritance only
  Result := nil ;
end;

class function TGIS_PvlContextBase.CreatePrintPreviewSimpleDialog(
  const _context: TGIS_PvlContext ;
  const _self   : TGIS_PvlControlPrintPreviewSimple
): TGIS_PvlControlPrintPreviewSimple ;
begin
  //For safe inheritance only
  Result := nil ;
end;

{$ENDREGION 'TGIS_PvlContextBase'}

{$REGION 'TGIS_PvlContext'}

function TGIS_PvlContext.CreateObject(
  const _parent : TGIS_PvlBase ;
  const _name   : String
) : IGIS_PvlBase ;
begin
  //For safe inheritance only
end ;

constructor TGIS_PvlContext.Create(
  const _parent     : TObject;
  const _ppi        : Integer;
  const _ppifix     : Single;
  const _canvasscale: Single;
  const _righttoleft: Boolean
);
var
  o : TGIS_PvlContextBaseClass ;
begin
  {$IFDEF OXYGENE}inherited( _parent, _ppi, _ppifix, _canvasscale, _righttoleft, nil ) ;{$ENDIF}

  iPPI         := _ppi ;
  dPPIFix      := _ppifix ;
  dCanvasScale := _canvasscale ;
  bRightToLeft := _righttoleft ;

  oPlatform := nil ;
  oList     := TGIS_PvlListOfElements.Create ;
  oGroups   := TDictionary<TGIS_PvlRadioButton, String>.Create ;
  assert( assigned( lstFrameworks ) ) ;
  for o in lstFrameworks do begin
    if o.Support( _parent ) then begin
      oPlatform := o.Create( _parent, _ppi, _ppifix, _canvasscale, _righttoleft, self ) as IGIS_PvlContext ;
      exit ;
    end;
  end ;

end;

procedure TGIS_PvlContext.FreeContext ;
begin
  oPlatform.FreeContext ;
end;

constructor TGIS_PvlContext.Create(
  const _parent : TObject
) ;
begin
  Create( _parent, TGIS_PvlContext(nil) ) ;
end ;

constructor TGIS_PvlContext.Create ;
begin
  Create( nil ) ;
end;

constructor TGIS_PvlContext.Create(
  const _parent  : TObject ;
  const _context : TGIS_PvlContext
) ;
var
  o : TGIS_PvlContextBaseClass ;
begin
  {$IFDEF OXYGENE}inherited( _parent, nil ) ;{$ENDIF}

  if assigned( _context ) then begin
    iPPI         := _context.PPI ;
    dPPIFix      := _context.PPIFix ;
    dCanvasScale := _context.CanvasScale ;
    bRightToLeft := _context.RightToLeft ;

  end else
  begin
    iPPI         := 96 ;
    dPPIFix      := 1 ;
    dCanvasScale := 1 ;
    bRightToLeft := False ;
  end;

  oList     := TGIS_PvlListOfElements.Create ;
  oGroups   := TDictionary<TGIS_PvlRadioButton, String>.Create ;
  oPlatform := nil ;

  for o in lstFrameworks do begin
    if o.Support( _parent ) or ( not assigned( _parent ) ) then begin
      if assigned( _context ) then
        oPlatform := o.Create( _parent, self, _context ) as IGIS_PvlContext
      else
        oPlatform := o.Create( _parent, self ) as IGIS_PvlContext ;
      if _parent is TGIS_PvlBase then begin
        oNativeParent := TGIS_PvlBase( _parent ).NativeControl ;
        oParent := TGIS_PvlBase( _parent )
      end
      else begin
        oNativeParent := _parent ;
        oParent := nil ;
      end;
      exit ;
    end;
  end ;
  oNativeParent := _parent ;
  oParent := nil ;
end;

procedure TGIS_PvlContext.doDestroy;
var
  itm : TGIS_PvlBase ;
begin
  if assigned( oList ) then
    for itm in oList do begin
      FreeObjectNotNil( itm ) ;
    end;
  FreeObject( oList ) ;
  FreeObject( oGroups );
  inherited;

  if assigned( Parent ) then
    Parent.oContextLocal := nil ;
 end;

function TGIS_PvlContext.fget_HMargin
  : Integer;
begin
  Result := oPlatform.fget_HMargin ;
end;

procedure TGIS_PvlContext.fset_HMargin(
  const _value: Integer
) ;
begin
  oPlatform.fset_HMargin( _value ) ;
end;

function TGIS_PvlContext.ClientWidth
  : Integer ;
begin
  Result := oPlatform.ClientWidth ;
end;

function TGIS_PvlContext.fget_VMargin
  : Integer;
begin
  Result := oPlatform.fget_VMargin ;
end;

procedure TGIS_PvlContext.fset_VMargin(
  const _value: Integer
) ;
begin
  oPlatform.fset_VMargin( _value ) ;
end;

function TGIS_PvlContext.fget_HSpace
  : Integer;
begin
  Result := oPlatform.fget_HSpace ;
end;

function TGIS_PvlContext.fget_VSpace
  : Integer;
begin
  Result := oPlatform.fget_VSpace ;
end;

function TGIS_PvlContext.fget_LSpace
  : Integer;
begin
  Result := oPlatform.fget_LSpace ;
end;

function TGIS_PvlContext.fget_Controls
  : TGIS_PvlListOfElements ;
begin
  Result := oList ;
end;

function TGIS_PvlContext.fget_CanvasScale
  : Single;
begin
  Result := dCanvasScale;
end;

function TGIS_PvlContext.fget_PPIFix
  : Single;
begin
  Result := dPPIFix;
end;

function TGIS_PvlContext.fget_PPI
  : Integer;
begin
  Result := iPPI ;
end;

function TGIS_PvlContext.Refresh
  : Boolean;
var
  oForm : IGIS_PvlForm ;
begin
  Result := False ;
  {$IFDEF OXYGENE}
    if oParent is IGIS_PvlForm then begin
      oForm := oParent as IGIS_PvlForm ;
  {$ELSE}
    if Supports( oParent, IGIS_PvlForm, oForm ) then begin
  {$ENDIF}
    if assigned( oForm ) then begin
      iPPI         := oForm.fget_PPI;
      dPPIFix      := oForm.fget_PPIFix;
      dCanvasScale := oForm.fget_CanvasScale;
      bRightToLeft := oForm.fget_RightToLeft ;
      Result := True ;
    end;
  end;
end;

class procedure TGIS_PvlContext.&Register(
  const _context : TGIS_PvlContextBaseClass
) ;
var
  o : TGIS_PvlContextBaseClass ;
begin
  if not assigned( lstFrameworks ) then
    lstFrameworks := {$IFDEF DCC}
                       TList<TGIS_PvlContextBaseClass>.Create ;
                     {$ELSEIF JAVA}
                        new java.util.ArrayList<TGIS_PvlContextBaseClass>();
                     {$ELSEIF CLR}
                        new System.Collections.Generic.List<TGIS_PvlContextBaseClass>();
                     {$ENDIF}

  for o in lstFrameworks do begin
    if o = _context then
      exit ;
  end;

  lstFrameworks.add( _context ) ;
end;

class function TGIS_PvlContext.InitializeSystemForm(
  const _context : TGIS_PvlContext ;
  const _self    : TGIS_PvlSystemForm
) : TGIS_PvlSystemForm ;
var
  o : TGIS_PvlContextBaseClass ;
begin
  Result := nil ;
  for o in lstFrameworks do begin
    if o.Support( _context ) then begin
      if _self is TGIS_PvlOpenDialog then
        Result := o.CreateOpenDialog( _context, TGIS_PvlOpenDialog( _self ) )
      else if _self is TGIS_PvlSaveDialog then
        Result := o.CreateSaveDialog( _context, TGIS_PvlSaveDialog( _self ) )
      else if _self is TGIS_PvlOptionDialog then
        Result := o.CreateOptionDialog( _context,TGIS_PvlOptionDialog(  _self ) )
      else if _self is TGIS_PvlInfoDialog then
        Result := o.CreateInfoDialog( _context,TGIS_PvlInfoDialog(  _self ) )
      else if _self is TGIS_PvlWarningDialog then
        Result := o.CreateWarningDialog( _context,TGIS_PvlWarningDialog(  _self ) )
      else if _self is TGIS_PvlErrorDialog then
        Result := o.CreateErrorDialog( _context,TGIS_PvlErrorDialog(  _self ) )
      else if _self is TGIS_PvlControlCSSystem then
        Result := o.CreateCSDialog( _context, TGIS_PvlControlCSSystem( _self ) )
      else if _self is TGIS_PvlLineSymbolEditor then
        Result := o.CreateLineSymbologyDialog( _context, TGIS_PvlLineSymbolEditor( _self ) )
      else if _self is TGIS_PvlControlPrintPreviewSimple then
        Result := o.CreatePrintPreviewSimpleDialog( _context, TGIS_PvlControlPrintPreviewSimple( _self ) )
      else if _self is TGIS_PvlSelectFolderDialog then
        Result := o.CreateSelectFolderDialog( _context, TGIS_PvlSelectFolderDialog( _self ) ) ;
      exit ;
    end;
  end ;
end ;

function TGIS_PvlContext.GetAllFonts
  : TStringList ;
begin
  Result := oPlatform.GetAllFonts ;
end ;

procedure TGIS_PvlContext.UpdatePPI(
  const _ppi : Integer ;
  const _ppiFix : Single ;
  const _canvasScale : Single
) ;
begin
  iPPI := _ppi ; ;
  dPPIFix := _ppiFix ; ;
  dCanvasScale := _canvasScale ;
end;

procedure TGIS_PvlContext.UpdateRightToLeft(
  const _rightToLeft : Boolean
) ;
begin
  bRightToLeft := _rightToLeft ;
end;

procedure TGIS_PvlContext.Redraw ;
var
  itm : TGIS_Object ;
begin
  for itm in oList do
    if itm is TGIS_PvlControl then begin
      TGIS_PvlControl( itm ).Context.UpdatePPI( PPI, PPIFix, CanvasScale );
      TGIS_PvlControl( itm ).DoRedraw ;
    end;
end ;

function TGIS_PvlContext.CreateHelper
  : TGIS_PvlControl ;
begin
  // For safe inheritance only
  Result := nil ;
end ;

{$ENDREGION 'TGIS_PvlContext'}

{$IFDEF DCC}

initialization
//  lstFrameworks := nil ;

finalization
  FreeObject( lstFrameworks ) ;
{$ENDIF}

//==================================== END =====================================
end.
