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
  Classes to help screen manipulation like zoom, drag etc.
}

unit FMX.GisControlColor ;
{$HPPEMIT '#pragma link "FMX.GisControlColor"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Types,
  System.SysUtils,
  System.UITypes,
  System.Classes,
  FMX.Forms,
  FMX.Objects,
  FMX.Types,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Graphics,
  FMX.ListBox,
  FMX.Layouts,
  FMX.Pickers,
  FMX.GisComboBoxHelper,
  FMX.GisControlVarious,
  FMX.GisModalForm,
  FMX.GisControlHelper,
  PVL.GisPvl,
  PVL.GisControlColor,
  GisTypes,
  GisTypesUI ;

type

  {$REGION 'TGIS_ColorPreview'}
  /// <summary>
  ///   Color preview box with support for alpha channel (transparency).
  /// </summary>
  TGIS_ColorPreview = class( TControl )
    private
      FCellSize : Integer ;
      FColor    : TGIS_Color ;
      FDialog   : Boolean ;
    private
      FOnDialogChange : TNotifyEvent ;
    private
      function  fget_CellSize : Integer ;
      procedure fset_CellSize ( const _size  : Integer
                              ) ;
      function  fget_Color    : TGIS_Color ;
      procedure fset_Color    ( const _color : TGIS_Color
                              ) ;
    protected

      /// <summary>
      ///   See documentation for TControl in Delphi help.
      /// </summary>
      /// <param name="Value">
      ///   See documentation for TControl in Delphi help.
      /// </param>
      procedure SetEnabled ( const Value : Boolean
                           ) ; override;

      /// <summary>
      ///   See documentation for TControl in Delphi help.
      /// </summary>
      procedure Click      ; override;

    protected

      /// <summary>
      ///   See documentation for TControl in Delphi help.
      /// </summary>
      procedure Paint ; override;

    public

      /// <summary>
      ///   See documentation for TControl in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TControl in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create ( _owner : TComponent
                         ) ; override;

      /// <summary>
      ///   See documentation for TControl in Delphi help.
      /// </summary>
      destructor Destroy ; override;

    public

      /// <summary>
      ///   Color to display.
      /// </summary>
      property Color    : TGIS_Color
                          read  fget_Color
                          write fset_Color ;

      /// <summary>
      ///   If true TGIS_ColorDialog will be called.
      /// </summary>
      property Dialog   : Boolean
                          read  FDialog
                          write FDialog ;


      /// <summary>
      ///   Cell size.
      /// </summary>
      property CellSize : Integer
                          read  fget_CellSize
                          write fset_CellSize ;

    public

      /// <event/>
      property OnDialogChange : TNotifyEvent
                        read  FOnDialogChange
                        write FOnDialogChange ;
  end ;

  {$ENDREGION}

  {$REGION 'TGIS_ColorComboBox'}
  /// <summary>
  ///   Combobox with marker styles.
  /// </summary>
  TGIS_ColorComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FOnChange       : TNotifyEvent ;
    private
      function  doValueColor    ( _sender    : TObject      ;
                                  _value     : String
                                ) : String ;
      function  doCustomColor   ( _sender    : TObject ;
                                  _value     : String
                                ) : String ;
      procedure doRenderColor   ( _item      : TListBoxItem ;
                                  _canvas    : TCanvas      ;
                                  _rect      : TRectF       ;
                                  _font      : TFont        ;
                                  _color     : TAlphaColor  ;
                                  _class     : Char         ;
                                  _caption   : String       ;
                                  _value     : String
                                ) ;
      function  fget_ColorValue : String ;
      procedure fset_ColorValue ( const _value : String
                                ) ;
      procedure fset_OnChange   ( _event : TNotifyEvent
                                ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   Fill the control with data.
      /// </summary>
      /// <param name="_field">
      ///   if values from fields
      /// </param>
      /// <param name="_renderer">
      ///   if renderer value
      /// </param>
      procedure Fill          ( const _field    : Boolean ;
                                const _renderer : Boolean
                              ) ;

    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

    public
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <summary>
      ///   Color value.
      /// </summary>
      property Value           : String
                                  read  fget_ColorValue
                                  write fset_ColorValue ;

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ColorDialog'}
  /// <summary>
  ///   Dialog box which allows to define or select a color.
  ///   Supports alpha channel (transparency).
  /// </summary>
  TGIS_ColorDialog = class( TGIS_ModalForm )
    private
      bLock      : Boolean ;
    private
      btnExpand  : TButton ;
      imgColor   : TControl ;
      scbValue   : TControl ;
      scbAlpha   : TControl ;
      lblColor   : TLabel ;
      cpvColor   : TGIS_ColorPreview ;
      lblHex     : TLabel ;
      edtHex     : TEdit ;
      lblAlpha   : TLabel ;
      edtAlpha   : TEdit ;
      lblRed     : TLabel ;
      edtRed     : TEdit ;
      lblGreen   : TLabel ;
      edtGreen   : TEdit ;
      lblBlue    : TLabel ;
      edtBlue    : TEdit ;
      lblHue     : TLabel ;
      edtHue     : TEdit ;
      lblSat     : TLabel ;
      edtSat     : TEdit ;
      lblLig     : TLabel ;
      edtLig     : TEdit ;
      gpbDefault : TGroupBox ;
      btnSimple  : TRadioButton ;
      btnComplex : TRadioButton ;
      imgDefault : TControl ;
      width_hidden   : Integer ;
      width_expanded : Integer ;
      height_always  : Integer ;

    private
      function  fget_AlphaColor : TGIS_Color ;
    private
      procedure lock         ;
      procedure unlock       ;
      function  isLocked     : Boolean ;
      procedure rgb2hsl      ;
      function  checkInteger ( const _str : String  ;
                               const _max : Integer ;
                               out   _val : Integer
                             ) : Boolean ;
      procedure assembleARGB ;
      function  assembleHSL  : Boolean ;
      procedure makeSimpleColors ;
      procedure makeComplexColors ;
    private
      procedure FormShow           ( _sender : TObject
                                   ) ;
      procedure doBtnExpandClick   ( _sender : TObject
                                   ) ;
      procedure doBtnSimpleClick   ( _sender : TObject
                                   ) ;
      procedure doBtnComplexClick  ( _sender : TObject
                                   ) ;
      procedure doImgColorChange   ( _sender : TObject
                                   ) ;
      procedure doScbValueChange   ( _sender : TObject
                                   ) ;
      procedure doScbAlphaChange   ( _sender : TObject
                                   ) ;
      procedure doEdtHexChange     ( _sender : TObject
                                   ) ;
      procedure doEdtAlphaChange   ( _sender : TObject
                                   ) ;
      procedure doEdtRedChange     ( _sender : TObject
                                   ) ;
      procedure doEdtGreenChange   ( _sender : TObject
                                   ) ;
      procedure doEdtBlueChange    ( _sender : TObject
                                   ) ;
      procedure doEdtHueChange     ( _sender : TObject
                                   ) ;
      procedure doEdtSatChange     ( _sender : TObject
                                   ) ;
      procedure doEdtLigChange     ( _sender : TObject
                                   ) ;
      procedure doImgDefaultClick  ( _sender : TObject
                                   ) ;
    protected

      /// <inheritdoc/>
      procedure initForm     ; override;

      /// <inheritdoc/>
      procedure initControls ; override;

    public

      /// <summary>
      ///   Shows the form.
      /// </summary>
      /// <param name="_color">
      ///   starting color value
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      procedure Execute     ( const _color  : TGIS_Color ;
                              const _onhelp : TGIS_HelpEvent ;
                              const _proc   : TProc<TModalResult>
                            ) ; overload;

      /// <summary>
      ///   Shows the form.
      /// </summary>
      /// <param name="_color">
      ///   starting color value
      /// </param>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      procedure Execute     ( const _color : TGIS_Color ;
                              const _proc  : TProc<TModalResult>
                            ) ; overload;
    public

      /// <summary>
      ///   Chosen color.
      /// </summary>
      property AlphaColor : TGIS_Color
                            read  fget_AlphaColor ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ColorRampComboBox'}
  /// <summary>
  ///   Combobox with bitmap and text.
  /// </summary>
  TGIS_ColorRampComboBox = class ( TGIS_ComboBoxAbstract )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      FOnChange       : TNotifyEvent ;
      bmpMap          : TGIS_ObjectList ;
      FMode           : TGIS_ColorMapMode ;
      FColorSchemas   : TGIS_ColorSchemas ;
      AIdx            : TGIS_IntegerArray ;
      FReverse        : Boolean ;
      iTextWidth      : Integer ;
      iTextGap        : Integer ;
  private
      procedure fset_Mode         ( const _mode : TGIS_ColorMapMode ) ;
      procedure fset_ColorSchemas ( const _type : TGIS_ColorSchemas ) ;
      procedure fset_Reverse      ( const _reverse : Boolean ) ;
    private
      function  doValueBitmap    ( _sender    : TObject      ;
                                  _value     : String
                                ) : String ;
      procedure doRenderBitmap   ( _item      : TListBoxItem ;
                                  _canvas    : TCanvas      ;
                                  _rect      : TRectF       ;
                                  _font      : TFont        ;
                                  _color     : TAlphaColor  ;
                                  _class     : Char         ;
                                  _caption   : String       ;
                                  _value     : String
                                ) ;
      function  fget_BitmapValue : String ;
      procedure fset_BitmapValue ( const _value : String
                                ) ;
      procedure fset_OnChange   ( _event : TNotifyEvent
                                ) ;
      procedure gradHorizontal    ( _canvas    : TCanvas ;
                                    _rect      : TRect ;
                                    _fromColor : TColor ;
                                    _toColor   : TColor ;
                                    _mode      : TGIS_ColorMapMode
                                  ) ;
    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create          ( _owner        : TComponent
                                  ) ; override;

      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      destructor  Destroy ; override ;

      /// <summary>
      ///   Fill the control with data.
      /// </summary>
      procedure Fill ;

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
      function Value            ( const _subClass   : Integer = -1
                                ) : TGIS_ColorMapArray ;
    public
      /// <inheritdoc/>
      procedure DelayedUpdate ( const _val : String
                              ) ; override;

    published //events
      /// <event/>
      property  CustomEvent    : TGIS_ComboBoxHelperCustomEvent
                                  read  FCustomEvent
                                  write FCustomEvent ;

      /// <event/>
      property  OnChange       : TNotifyEvent
                                  read  FOnChange
                                  write fset_OnChange ;
    public
      /// <summary>
      ///   ColorMap mode.
      /// </summary>
      property Mode             : TGIS_ColorMapMode
                                  read  FMode
                                  write fset_Mode ;
      /// <summary>
      ///   Color schema filter.
      /// </summary>
      property ColorSchemas     : TGIS_ColorSchemas
                                  read  FColorSchemas
                                  write fset_ColorSchemas ;
      /// <summary>
      ///   Reverse colormap.
      /// </summary>
      property Reverse          : Boolean
                                  read  FReverse
                                  write fset_Reverse ;
      /// <summary>
      ///   Color value.
      /// </summary>
      property ValueAsText           : String
                                 read  fget_BitmapValue
                                 write fset_BitmapValue ;
  end ;
  {$ENDREGION}

//##############################################################################
implementation

uses
  System.Generics.Collections,
  System.Math,
  FMX.Styles,

  GisRtl,
  GisXmlDoc,
  GisParams,
  GisResource,
  GisFunctions,
  FMX.GisFramework ;

{$REGION 'TGIS_ColorPreview'}
const
  GIS_COLOR_HUE_SIZE       : Integer = 1530 ;

  GIS_COLOR_WHEEL_MARGIN   : Integer = 14 ;
  GIS_COLOR_WHEEL_MARGIN2  : Integer = 7 ;

  GIS_COLOR_GRADIENT_MARGIN  : Integer = 8 ;
  GIS_COLOR_GRADIENT_MARGIN2 : Integer = 4 ;

const
  HGAP  = 16 ;
  HGAP2 = 82 ;
  HGAP3 = 52 ;
  LABEL_WIDTH = 64 ;
  LABEL2_WIDTH = 44 ;

var
  vScaleFactor : Single ;

type

  T_RGBVal = record
    R : Byte ;
    G : Byte ;
    B : Byte ;
  end ;


  T_colorWheel = class( TControl )
    private
      arrHue      : array of T_RGBVal ;
      bInit       : Boolean ;
      colorWheel  : TBitmap ;
      currState   : TBitmap ;
      currX       : Single ;
      currY       : Single ;
      bMouseDown  : Boolean ;
    private
      FColor     : TGIS_Color ;
    private
      FOnChange  : TNotifyEvent ;
    private
      function  fget_Color : TGIS_Color ;
      procedure fset_Color ( const _color : TGIS_Color
                           ) ;
    private
      procedure prepareHue     ;
      function  angleToRGB     ( const _phi : Double
                               ) : T_RGBVal ;
      function  checkMouse     ( const _x : Single ;
                                 const _y : Single
                               ) : Boolean ;
      procedure makeColorWheel ;
      procedure drawCrosshair  ;
      procedure raiseChange    ;
    protected
      procedure MouseDown ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
      procedure MouseMove ( _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
    protected
      procedure Paint ; override;
    public
      constructor Create  ( _owner : TComponent
                          ) ; override;
    public
      destructor Destroy  ; override;
    public
      function HueToRGB   ( const _hue : Double
                          ) : T_RGBVal ;
    public
      property Color    : TGIS_Color
                          read  fget_Color
                          write fset_Color ;
    public
      property OnChange : TNotifyEvent
                          read  FOnChange
                          write FOnChange ;
  end ;


  T_valueBar = class( TControl )
    protected
      bInit       : Boolean ;
      gradient    : TBitmap ;
      currState   : TBitmap ;
      currX       : Single ;
      bMouseDown  : Boolean ;
    protected
      FValue     : Double ;
      FColor     : TGIS_Color ;
      FAlpha     : Boolean ;
    protected
      FOnChange  : TNotifyEvent ;
    protected
      function  fget_Value : Double ;
      procedure fset_Value ( const _value : Double
                           ) ;
      function  fget_Color : TGIS_Color ;
      procedure fset_Color ( const _color : TGIS_Color
                           ) ;
      function  fget_Alpha : Boolean ;
      procedure fset_Alpha ( const _bool  : Boolean
                           ) ;
    protected
      function  checkMouse  ( const _x : Single ;
                              const _y : Single
                            ) : Boolean ;
      procedure updateValue ;
      procedure drawArrows  ;
      procedure raiseChange ;
    protected
      procedure makeGradient ; //virtual;
      procedure makeColor    ;
      procedure makeAlpha    ;
    protected
      procedure MouseDown ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
      procedure MouseMove ( _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
    protected
      procedure Paint ; override;
    public
      constructor Create ( _owner : TComponent
                         ) ; override;
    public
      destructor Destroy ; override;
    public
      property Value    : Double
                          read  fget_Value
                          write fset_Value ;
      property Color    : TGIS_Color
                          read  fget_Color
                          write fset_Color ;
      property Alpha    : Boolean
                          read  fget_Alpha
                          write fset_Alpha ;
    public
      property OnChange : TNotifyEvent
                          read  FOnChange
                          write FOnChange ;
  end ;


  T_defaultColors = class( TControl )
    private
      colorList   : TList<TGIS_Color> ;
      bInit       : Boolean ;
      colors      : TBitmap ;
      cWidth      : Integer ;
      cHeight     : Integer ;
    private
      FColor    : TGIS_Color ;
    private
      FOnColorClick : TNotifyEvent ;
    private
      function  fget_Color : TGIS_Color ;
    private
      function  checkMouse     ( const _x : Single ;
                                 const _y : Single
                               ) : Boolean ;
      procedure makeColorList  ;
    protected
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Single ;
                            _y      : Single
                          ) ; override;
    protected
      procedure Paint ; override;
    public
      constructor Create ( _owner : TComponent
                         ) ; override;
    public
      destructor Destroy ; override;
    public
      procedure SetCellSize ( const _w     : Integer ;
                              const _h     : Integer
                            ) ;
      procedure AddColor    ( const _color : TGIS_Color
                            ) ;
      procedure Clear       ;
    public
      property Color        : TGIS_Color
                              read  fget_Color ;
    public
      property OnColorClick : TNotifyEvent
                              read  FOnColorClick
                              write FOnColorClick ;
  end ;


  function local_HexStrToInt(
    const _str : String  ;
    out   _val : Integer
  ) : Boolean ;
  var
    res : Integer ;
    mul : Integer ;
    c   : Byte ;
    v   : Integer ;
    i   : Integer ;
    k   : Integer ;
  begin
    Result := True ;

    res := 0 ;
    v := 0 ;
    for i := StringLast( _str ) downto StringFirst do begin

      c := Byte( _str[i] ) ;
      if ( c >= 47 ) and ( c <= 57 ) then
        v := c - 48
      else
      if ( c >= 65 ) and ( c <= 70 ) then
        v := c - 55
      else
      if ( c >= 97 ) and ( c <= 102 ) then
        v := c - 87
      else begin
        _val := 0 ;
        Result := False ;
        exit ;
      end ;

      if i = StringLast( _str ) then
        mul := 1
      else begin
        mul := 16 ;
        for k := 1 to StringLast( _str ) - i - 1 do
          mul := mul * 16 ;
      end ;

      res := res + v*mul ;
    end ;

    _val := res ;
  end ;


  function local_HexStrToARGB(
    const _str : String  ;
    out   _a   : Byte ;
    out   _r   : Byte ;
    out   _g   : Byte ;
    out   _b   : Byte
  ) : Boolean ;
  var
    a : Integer ;
    r : Integer ;
    g : Integer ;
    b : Integer ;
  begin
    Result := False ;

    _a := 0 ;
    _r := 0 ;
    _g := 0 ;
    _b := 0 ;

    if Length( _str ) <> 8 then
      exit ;
    if not local_HexStrToInt( Copy( _str, 1, 2 ), a ) then
      exit ;
    if not local_HexStrToInt( Copy( _str, 3, 2 ), r ) then
      exit ;
    if not local_HexStrToInt( Copy( _str, 5, 2 ), g ) then
      exit ;
    if not local_HexStrToInt( Copy( _str, 7, 2 ), b ) then
      exit ;

    _a := Byte( a ) ;
    _r := Byte( r ) ;
    _g := Byte( g ) ;
    _b := Byte( b ) ;

    Result := True ;
  end ;


  // draw checkerboard color
  procedure draw_color(
    _canvas   : TCanvas    ;
    _color    : TGIS_Color ;
    _rect     : TRectF     ;
    _cellsize : Integer    ;
    _enabled  : Boolean
  ) ;
  var
    clr  : TGIS_Color ;
    i    : Integer ;
    imax : Integer ;
    k    : Integer ;
    kmax : Integer ;
    blst : Boolean ;
    bkgm : Double ;
    fggm : Double ;
    r    : Byte ;
    g    : Byte ;
    b    : Byte ;
    rb   : Byte ;
    gb   : Byte ;
    bb   : Byte ;
    rw   : Byte ;
    gw   : Byte ;
    bw   : Byte ;
    bmix : Cardinal ;
    wmix : Cardinal ;
  begin
    if _enabled then begin
      clr := _color ;
      rb := $FF ;
      gb := $FF ;
      bb := $FF ;
      rw := $C0 ;
      gw := $C0 ;
      bw := $C0 ;
    end
    else begin
      clr := TGIS_Color.FromARGB( $00000000 ) ;
      rb := $C0 ;
      gb := $C0 ;
      bb := $C0 ;
      rw := $88 ;
      gw := $88 ;
      bw := $88 ;
    end ;

    imax := RoundS( _rect.Width  / _cellsize ) ;
    kmax := RoundS( _rect.Height / _cellsize ) ;

    fggm := clr.A / 255 ;
    bkgm := 1.0 - fggm ;

    r := clr.R ;
    g := clr.G ;
    b := clr.B ;

    bmix := ( RoundS( bkgm * rb + fggm * r ) shl 16 ) +
            ( RoundS( bkgm * gb + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bb + fggm * b )        ) ;
    wmix := ( RoundS( bkgm * rw + fggm * r ) shl 16 ) +
            ( RoundS( bkgm * gw + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bw + fggm * b )        ) ;

    _canvas.Fill.Kind := TBrushKind.Solid ;
    _canvas.Stroke.Thickness := 1 ;

    for k := 0 to kmax do begin

      blst := ( k mod 2 ) = 0 ;

      for i := 0 to imax do begin

        if blst then begin
          _canvas.Stroke.Color := bmix or $FF000000 ;
          _canvas.Fill.Color   := bmix or $FF000000;
        end
        else begin
          _canvas.Stroke.Color := wmix or $FF000000;
          _canvas.Fill.Color   := wmix or $FF000000;
        end ;

        // additional 1 added to Right
        // because of problems with bdRightToLeft mode
        _canvas.FillRect(
           RectF( Min( _rect.Right , _rect.Left + i * _cellsize - 0.5 ),
                  Min( _rect.Bottom, _rect.Top  + k * _cellsize - 0.5 ),
                  Min( _rect.Right ,
                    _rect.Left + ( i + 1 ) * _cellsize + 1 + 1 - 0.5 ),
                  Min( _rect.Bottom,
                    _rect.Top  + ( k + 1 ) * _cellsize + 1 + 1 - 0.5 )
                ),
           0, 0, [], 1
        ) ;

        blst := not blst ;
      end ;

    end ;
  end ;

//==============================================================================
// T_colorWheel
//==============================================================================

  constructor T_colorWheel.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    FColor := TGIS_Color.White ;

    currX := RoundS(  Width/2 ) ;
    currY := RoundS( Height/2 ) ;

    prepareHue ;

    bInit := False ;
    bMouseDown := False ;
  end ;


  destructor T_colorWheel.Destroy ;
  begin
    FreeObject( colorWheel ) ;
    FreeObject( currState ) ;

    inherited ;
  end ;


  function T_colorWheel.fget_Color : TGIS_Color ;
  var
    px  : T_RGBVal ;
    w2  : Double ;
    h2  : Double ;
    x   : Double ;
    y   : Double ;
    s   : Double ;
  begin
    w2 := ( Width  - GIS_COLOR_WHEEL_MARGIN )/2 ;
    h2 := ( Height - GIS_COLOR_WHEEL_MARGIN )/2 ;

    y := h2 - currY + GIS_COLOR_WHEEL_MARGIN2 ;
    x := currX - w2 - GIS_COLOR_WHEEL_MARGIN2 ;

    px := angleToRGB( ArcTan2( y, x ) ) ;

    s := Sqrt( x*x + y*y )/w2 ;
    if s > 1.0 then
      s := 1.0 ;

    Result := TGIS_Color.FromRGB(
                RoundS( 255 - s*px.R ),
                RoundS( 255 - s*px.G ),
                RoundS( 255 - s*px.B )
              ) ;
  end ;


  procedure T_colorWheel.fset_Color(
    const _color : TGIS_Color
  ) ;
  var
    h   : Double ;
    s   : Double ;
    v   : Double ;
    rad : Double ;
    w2  : Double ;
    x   : Double ;
    y   : Double ;
  begin
    _color.ToHSV( h, s, v ) ;

    rad := 2*PI*h ;

    y := s*Sin( rad ) ;
    x := s*Cos( rad ) ;
    w2 := ( Width  - GIS_COLOR_WHEEL_MARGIN )/2 ;

    currX := RoundS( x*w2 + Width/2 ) ;
    currY := RoundS( Height/2 - y*w2 ) ;

    Repaint ;
  end ;


  procedure T_colorWheel.prepareHue ;
  var
    seq : Integer ;
    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
    i   : Integer ;

    procedure do_w_0 ;
    begin
      Inc( g ) ;
      if g = 255 then
        Inc( seq ) ;
    end ;

    procedure do_w_1 ;
    begin
      Dec( r ) ;
      if r = 0 then
        Inc( seq ) ;
    end ;

    procedure do_w_2 ;
    begin
      Inc( b ) ;
      if b = 255 then
        Inc( seq ) ;
    end ;

    procedure do_w_3 ;
    begin
      Dec( g ) ;
      if g = 0 then
        Inc( seq ) ;
    end ;

    procedure do_w_4 ;
    begin
      Inc( r ) ;
      if r = 255 then
        Inc( seq ) ;
    end ;

    procedure do_w_5 ;
    begin
      Dec( b ) ;
      if b = 0 then
        Inc( seq ) ;
    end ;

  begin
    SetLength( arrHue, GIS_COLOR_HUE_SIZE ) ;

    seq := 0 ;

    seq := 0 ;
    r := 255 ;
    g := 0 ;
    b := 0 ;

    arrHue[0].R := r ;
    arrHue[0].G := g ;
    arrHue[0].B := b ;

    for i := 1 to GIS_COLOR_HUE_SIZE - 1 do begin

      case seq of
        0 : do_w_0 ;
        1 : do_w_1 ;
        2 : do_w_2 ;
        3 : do_w_3 ;
        4 : do_w_4 ;
        5 : do_w_5 ;
      end ;

      arrHue[i].R := r ;
      arrHue[i].G := g ;
      arrHue[i].B := b ;

    end ;
  end ;


  function T_colorWheel.angleToRGB(
    const _phi : Double
  ) : T_RGBVal ;
  var
    i : Integer ;
  begin
    i := FloorS( ( ( PI + _phi )/( 2 * PI ) ) * GIS_COLOR_HUE_SIZE ) ;

    if i < GIS_COLOR_HUE_SIZE then
      Result := arrHue[i]
    else
      Result := arrHue[0] ;
  end ;


  function T_colorWheel.HueToRGB(
    const _hue : Double
  ) : T_RGBVal ;
  var
    hue : Double ;
  begin
    if _hue = 360.0 then
      hue := 0.0
    else
      hue := _hue ;

    Result := angleToRGB( 2*PI*hue/360 - PI ) ;
  end ;


  function T_colorWheel.checkMouse(
    const _x : Single ;
    const _y : Single
  ) : Boolean ;
  var
    w2 : Integer ;
    h2 : Integer ;
    x  : Single ;
    y  : Single ;
    r  : Double ;
  begin
    Result := True ;

    w2 := RoundS( Width/2.0 ) ;
    h2 := RoundS( Height/2.0 ) ;

    y := h2 - _y ;
    x := _x - w2 ;

    r := Sqrt( x*x + y*y ) ;
    if r > w2 - GIS_COLOR_WHEEL_MARGIN2/vScaleFactor then
      Result := False ;
  end ;


  procedure T_colorWheel.makeColorWheel ;
  var
    w2  : Integer ;
    h2  : Integer ;
    x   : Integer ;
    y   : Integer ;
    r   : Double ;
    a   : Double ;
    hue : T_RGBVal ;
    i   : Integer ;
    k   : Integer ;
    bmpd: TBitmapData ;
    ptr : Pointer ;
  begin
    colorWheel := TBitmap.Create ;
    colorWheel.SetSize(
      RoundS( vScaleFactor*Width  ),
      RoundS( vScaleFactor*Height )
    ) ;
    colorWheel.Clear( 0 ) ;

    colorWheel.Canvas.BeginScene ;
    w2 := RoundS( ( vScaleFactor*Width  - GIS_COLOR_WHEEL_MARGIN )/2.0 ) ;
    h2 := RoundS( ( vScaleFactor*Height - GIS_COLOR_WHEEL_MARGIN )/2.0 ) ;

    colorWheel.Map( TMapAccess.Write, bmpd ) ;
    for i := GIS_COLOR_WHEEL_MARGIN2
        to colorWheel.Height - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

      ptr := bmpd.GetScanline(i) ;
      y := h2 - i + GIS_COLOR_WHEEL_MARGIN2 ;

      for k := GIS_COLOR_WHEEL_MARGIN2
          to colorWheel.Width - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

        x := k - w2 - GIS_COLOR_WHEEL_MARGIN2 ;

        r := Sqrt( x*x + y*y )/w2 ;
        if r > 1.0 then
          continue ;

        a := ArcTan2( y, x ) ;

        hue := angleToRGB( a ) ;

        PCardinal( NativeInt(ptr) + 4*k)^ := Cardinal(
                 255 shl 24 +
                (255 - RoundS( r*hue.R )) shl 16 +
                (255 - RoundS( r*hue.G )) shl 8 +
                (255 - RoundS( r*hue.B )) shl 0 ) ;
      end ;

    end ;
    colorWheel.UnMap( bmpd ) ;

    colorWheel.Canvas.EndScene ;

    currState := TBitmap.Create ;
    currState.SetSize(
      RoundS( vScaleFactor*Width  ),
      RoundS( vScaleFactor*Height )
    ) ;
    currState.Clear( 0 ) ;

    bInit := True ;
  end ;


  procedure T_colorWheel.drawCrosshair ;
  var
    path : TPathData ;
    scl  : Single ;
    cx   : Single ;
    cy   : Single ;
    c1, c2, c3, c6 ,c7 : Single ;
//    frm : TGIS_ModalForm ;
  begin
    currState.Canvas.BeginScene ;
    currState.Canvas.Clear(0);
    currState.Canvas.DrawBitmap(
      colorWheel,
      RectF(0, 0, colorWheel.Width, colorWheel.Height),
      RectF(0, 0, colorWheel.Width, colorWheel.Height),
      1
    ) ;

    currState.Canvas.Stroke.Kind := TBrushKind.Solid ;
    currState.Canvas.Stroke.Thickness := vScaleFactor ;

    path := TPathData.Create();

    cx := currX ;
    cy := currY ;

    currX := vScaleFactor*currX ;
    currY := vScaleFactor*currY ;

    c1 := vScaleFactor*1.0 ;
    c2 := vScaleFactor*2.0 ;
    c3 := vScaleFactor*3.0 ;
    c6 := vScaleFactor*6.0 ;
    c7 := vScaleFactor*7.0 ;

    currState.Canvas.Stroke.Color := $FF000000 ;
    path.MoveTo( PointF( currX - c1 - 0.5, currY - c2 - 0.5 ) ) ;
    path.LineTo( PointF( currX - c1 - 0.5, currY - c7 - 0.5 ) ) ;
    path.MoveTo( PointF( currX      - 0.5, currY - c2 - 0.5 ) ) ;
    path.LineTo( PointF( currX      - 0.5, currY - c7 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := $FFFFFFFF  ;
    path.MoveTo( PointF( currX      - 0.5, currY - c3 - 0.5 ) ) ;
    path.LineTo( PointF( currX      - 0.5, currY - c6 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := $FF000000  ;
    path.MoveTo( PointF( currX + c1 - 0.5, currY - c2 - 0.5 ) ) ;
    path.LineTo( PointF( currX + c1 - 0.5, currY - c7 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;

    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FF000000 ) ;
    path.MoveTo( PointF( currX - c1 - 0.5, currY + c2 - 0.5 ) ) ;
    path.LineTo( PointF( currX - c1 - 0.5, currY + c7 - 0.5 ) ) ;
    path.MoveTo( PointF( currX      - 0.5, currY + c2 - 0.5 ) ) ;
    path.LineTo( PointF( currX      - 0.5, currY + c7 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FFFFFFFF ) ;
    path.MoveTo( PointF( currX      - 0.5, currY + c3 - 0.5 ) ) ;
    path.LineTo( PointF( currX      - 0.5, currY + c6 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FF000000 ) ;
    path.MoveTo( PointF( currX + c1 - 0.5, currY + c2 - 0.5 ) ) ;
    path.LineTo( PointF( currX + c1 - 0.5, currY + c7 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;

    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FF000000 ) ;
    path.MoveTo( PointF( currX - c2 - 0.5, currY - c1 - 0.5 ) ) ;
    path.LineTo( PointF( currX - c7 - 0.5, currY - c1 - 0.5 ) ) ;
    path.MoveTo( PointF( currX - c2 - 0.5, currY      - 0.5 ) ) ;
    path.LineTo( PointF( currX - c7 - 0.5, currY      - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FFFFFFFF ) ;
    path.MoveTo( PointF( currX - c3 - 0.5, currY      - 0.5 ) ) ;
    path.LineTo( PointF( currX - c6 - 0.5, currY      - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FF000000 ) ;
    path.MoveTo( PointF( currX - c2 - 0.5, currY + c1 - 0.5 ) ) ;
    path.LineTo( PointF( currX - c7 - 0.5, currY + c1 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;

    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FF000000 ) ;
    path.MoveTo( PointF( currX + c2 - 0.5, currY - c1 - 0.5 ) ) ;
    path.LineTo( PointF( currX + c7 - 0.5, currY - c1 - 0.5 ) ) ;
    path.MoveTo( PointF( currX + c2 - 0.5, currY      - 0.5 ) ) ;
    path.LineTo( PointF( currX + c7 - 0.5, currY      - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FFFFFFFF ) ;
    path.MoveTo( PointF( currX + c3 - 0.5, currY      - 0.5 ) ) ;
    path.LineTo( PointF( currX + c6 - 0.5, currY      - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    path.Clear ;
    currState.Canvas.Stroke.Color := ( $FF000000 ) ;
    path.MoveTo( PointF( currX + c2 - 0.5, currY + c1 - 0.5 ) ) ;
    path.LineTo( PointF( currX + c7 - 0.5, currY + c1 - 0.5 ) ) ;
    currState.Canvas.DrawPath( path, 1 ) ;
    FreeObject( path ) ;

    currX := cx ;
    currY := cy ;

    currState.Canvas.EndScene ;
  end ;


  procedure T_colorWheel.raiseChange ;
  begin
    if Assigned( FOnChange ) then
      FOnChange( Self ) ;
  end ;


  procedure T_colorWheel.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  begin
    if not checkMouse( _x, _y ) then
      exit ;

    currX := _x ;
    currY := _y ;

    Repaint ;

    raiseChange ;

    bMouseDown := True ;
  end ;


  procedure T_colorWheel.MouseMove(
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  begin
    if not bMouseDown then
      exit ;

    if not checkMouse( _x, _y ) then
      exit ;

    currX := _x ;
    currY := _y ;

    Repaint ;

    raiseChange ;
  end ;


  procedure T_colorWheel.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  begin
    if not bMouseDown then
      exit ;

    if not checkMouse( _x, _y ) then begin
      bMouseDown := False ;
      exit ;
    end ;

    currX := _x ;
    currY := _y ;

    Repaint ;

    bMouseDown := False ;

    raiseChange ;
  end ;


  procedure T_colorWheel.Paint ;
  begin
    if not bInit then
      makeColorWheel ;

    drawCrosshair ;

    Canvas.DrawBitmap(
      currState,
      RectF(0, 0, currState.Width, currState.Height ),
      RectF(0, 0, currState.Width/vScaleFactor, currState.Height/vScaleFactor ),
      1
    ) ;
  end ;


//==============================================================================
// T_valueBar
//==============================================================================

  constructor T_valueBar.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    currX := GIS_COLOR_GRADIENT_MARGIN2 ;

    FColor := TGIS_Color.White ;
    FAlpha := False ;

    bInit := False ;
  end ;


  destructor T_valueBar.Destroy ;
  begin
    FreeObject( gradient ) ;
    FreeObject( currState ) ;

    inherited ;
  end ;


  function T_valueBar.fget_Value : Double ;
  begin
    Result := FValue ;
  end ;


  procedure T_valueBar.fset_Value(
    const _value : Double
  ) ;
  begin
    if _value > 1.0 then
      FValue := 1.0
    else
    if FValue < 0.0 then
      FValue := 0
    else
      FValue := _value ;

    currX := RoundS(
               FValue * ( Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) +
               GIS_COLOR_GRADIENT_MARGIN2
             ) ;

    Repaint ;
  end ;


  function T_valueBar.fget_Color : TGIS_Color ;
  begin
    Result := FColor ;
  end ;


  procedure T_valueBar.fset_Color(
    const _color : TGIS_Color
  ) ;
  begin
    FColor := _color ;

    bInit := False ;

    Repaint ;
  end ;


  function T_valueBar.fget_Alpha : Boolean ;
  begin
    Result := FAlpha ;
  end ;


  procedure T_valueBar.fset_Alpha(
    const _bool : Boolean
  ) ;
  begin
    FAlpha := _bool ;
  end ;


  function T_valueBar.checkMouse(
    const _x : Single ;
    const _y : Single
  ) : Boolean ;
  begin
    Result := True ;

    if ( _x < GIS_COLOR_GRADIENT_MARGIN2/vScaleFactor + vScaleFactor ) or
       ( _x > Width - GIS_COLOR_GRADIENT_MARGIN2/vScaleFactor - vScaleFactor ) then
      Result := False ;
  end ;


  procedure T_valueBar.updateValue ;
  begin
    FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;
  end ;


  procedure T_valueBar.makeGradient ;
  begin
    FreeObject( gradient ) ;

    gradient := TBitmap.Create ;
    gradient.SetSize(
      RoundS( vScaleFactor*Width  ),
      RoundS( vScaleFactor*Height )
    ) ;
    gradient.Clear( 0 ) ;
    gradient.Canvas.BeginScene ;
    gradient.Canvas.Stroke.Color := TAlphaColorRec.Black ;
    gradient.Canvas.Stroke.Kind := TBrushKind.Solid ;
    gradient.Canvas.Fill.Color := 0 ;
    gradient.Canvas.Fill.Kind := TBrushKind.Solid ;
    gradient.Canvas.FillRect(
      RectF( 0, 0, vScaleFactor*Width, vScaleFactor*Height ), 0, 0, [], 1
    ) ;

    if Alpha then
      makeAlpha
    else
      makeColor ;

    gradient.Canvas.EndScene ;

    FreeObject( currState ) ;

    currState := TBitmap.Create ;
    currState.SetSize(
      RoundS( vScaleFactor*Width  ),
      RoundS( vScaleFactor*Height )
    ) ;
    currState.Clear( 0 ) ;

    bInit := True ;
  end ;


  procedure T_valueBar.makeColor ;
  var
    bmpd: TBitmapData ;
    ptr : PByteArray ;
    i   : Integer ;
    k   : Integer ;
  begin
    inherited ;

    gradient.Map( TMapAccess.Write, bmpd ) ;

    for i := GIS_COLOR_GRADIENT_MARGIN2
      to RoundS( vScaleFactor*Height - GIS_COLOR_GRADIENT_MARGIN2 - 1) do begin

      ptr := bmpd.GetScanline(i) ;

      for k := GIS_COLOR_GRADIENT_MARGIN2
        to RoundS( vScaleFactor*Width - GIS_COLOR_GRADIENT_MARGIN2 - 1) do begin

        ptr[4*k  ] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.B
          ) ;
        ptr[4*k+1] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.G
          ) ;
        ptr[4*k+2] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.R
          ) ;
        ptr[4*k+3  ] := 255 ;

      end ;

    end ;

    gradient.Unmap(bmpd);

    gradient.Canvas.Stroke.Color := $FF000000  ;
    gradient.Canvas.Fill.Kind := TBrushKind.None ;
    gradient.Canvas.Stroke.Thickness := vScaleFactor ;
    gradient.Canvas.DrawRect(
      RectF(
        GIS_COLOR_GRADIENT_MARGIN2 - 0.5 ,
        GIS_COLOR_GRADIENT_MARGIN2 - 0.5,
        vScaleFactor*Width  - GIS_COLOR_GRADIENT_MARGIN2 + 0.5,
        vScaleFactor*Height - GIS_COLOR_GRADIENT_MARGIN2 + 0.5
      ),
      0, 0, [], 1
    ) ;
  end ;


  procedure T_valueBar.makeAlpha ;
  var
    bmpd: TBitmapData ;
    ptr : PByteArray ;
    mul : Double ;
    i   : Integer ;
    k   : Integer ;
    rb  : Boolean ;
    cb  : Boolean ;

    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
  begin
    inherited ;

    r := FColor.R ;
    g := FColor.G ;
    b := FColor.B ;

    cb := False ;
    gradient.Map( TMapAccess.Write, bmpd ) ;

    for i := GIS_COLOR_GRADIENT_MARGIN2
      to RoundS( vScaleFactor*Height - GIS_COLOR_GRADIENT_MARGIN2 - 1 ) do begin

      if ( ( i - GIS_COLOR_GRADIENT_MARGIN2 ) mod RoundS( vScaleFactor*8 )  ) = 0 then
        cb := not cb ;
      rb := cb ;

      ptr := bmpd.GetScanline(i) ;

      for k := GIS_COLOR_GRADIENT_MARGIN2
        to RoundS( vScaleFactor*Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 ) do begin

        if ( ( k - GIS_COLOR_GRADIENT_MARGIN2 ) mod RoundS( vScaleFactor*8 ) ) = 0 then
          rb := not rb ;

        mul := ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
               ( vScaleFactor*Width - GIS_COLOR_GRADIENT_MARGIN ) ;

        if rb then begin
          ptr[4*k  ] := RoundS( mul * b + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+1] := RoundS( mul * g + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+2] := RoundS( mul * r + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+3] := 255 ;
        end
        else begin
          ptr[4*k  ] := RoundS( mul * b + ( 1 - mul ) * $FF ) ;
          ptr[4*k+1] := RoundS( mul * g + ( 1 - mul ) * $FF ) ;
          ptr[4*k+2] := RoundS( mul * r + ( 1 - mul ) * $FF ) ;
          ptr[4*k+3] := 255 ;
        end ;

      end ;

    end ;
    gradient.Unmap(bmpd);

     gradient.Canvas.Stroke.Color := $FF000000 ;
     gradient.Canvas.Fill.Kind := TBrushKind.None ;
     gradient.Canvas.Stroke.Thickness := vScaleFactor ;
     gradient.Canvas.DrawRect(
       RectF(
         GIS_COLOR_GRADIENT_MARGIN2 - 0.5,
         GIS_COLOR_GRADIENT_MARGIN2 - 0.5,
         vScaleFactor*Width  - GIS_COLOR_GRADIENT_MARGIN2 + 0.5,
         vScaleFactor*Height - GIS_COLOR_GRADIENT_MARGIN2 + 0.5
       ), 0, 0, [], 1
     ) ;
  end ;


  procedure T_valueBar.drawArrows ;
  var
    path : TPathData ;
    cx   : Single ;
    cy   : Single ;
  begin
    currState.Canvas.BeginScene ;
    currState.Canvas.Clear(0);
    currState.Canvas.DrawBitmap( gradient,
      RectF(0, 0, gradient.Width, gradient.Height),
      RectF(0, 0, gradient.Width, gradient.Height),
      1
    ) ;

    cx := currX ;
    currX := vScaleFactor*currX ;

    currState.Canvas.Stroke.Color := TAlphaColorRec.Black ;
    currState.Canvas.Stroke.Thickness := vScaleFactor ;
    currState.Canvas.Fill.Color := TAlphaColorRec.White ;
    currState.Canvas.Fill.Kind := TBrushKind.Solid ;
    currState.Canvas.FillRect(
      RectF( currX - vScaleFactor*2.5, 0, currX + vScaleFactor*2.5, vScaleFactor*Height ),
      2, 2, [TCorner.TopLeft, TCorner.TopRight,
        TCorner.BottomLeft, TCorner.BottomRight ], 1
    ) ;
    currState.Canvas.DrawRect(
      RectF( currX - vScaleFactor*2.5, 0, currX + vScaleFactor*2.5, vScaleFactor*Height ),
      2, 2, [TCorner.TopLeft, TCorner.TopRight,
        TCorner.BottomLeft, TCorner.BottomRight ], 1
    ) ;

    currX := cx ;

//    currState.Canvas.Stroke.Color := TAlphaColorRec.Lightgray ;
//    path := TPathData.Create();
//    path.MoveTo( PointF( currX - 2, 0 ) ) ;
//    path.LineTo( PointF( currX - 2, Height ) ) ;
//    currState.Canvas.DrawPath( path, 0.5);
//
//    currState.Canvas.Stroke.Color := TAlphaColorRec.Gray ;
//    path.Clear ;
//    path.MoveTo( PointF( currX - 1, 0 ) ) ;
//    path.LineTo( PointF( currX - 1, Height ) ) ;
//    currState.Canvas.DrawPath( path, 0.5);
//    path.Clear ;
//    path.MoveTo( PointF( currX, 0 ) ) ;
//    path.LineTo( PointF( currX, Height ) ) ;
//    currState.Canvas.DrawPath( path, 1);
//    path.Clear ;
//    path.MoveTo( PointF( currX + 1, 0 ) ) ;
//    path.LineTo( PointF( currX + 1, Height ) ) ;
//    currState.Canvas.DrawPath( path, 0.5);
//
//    currState.Canvas.Stroke.Color := TAlphaColorRec.Darkgray ;
//    path.Clear ;
//    path.MoveTo( PointF( currX + 2, 0 ) ) ;
//    path.LineTo( PointF( currX + 2, Height ) ) ;
//    currState.Canvas.DrawPath( path, 0.5);

    currState.Canvas.EndScene ;
  end ;


  procedure T_valueBar.raiseChange ;
  begin
    if Assigned( FOnChange ) then
      FOnChange( Self ) ;
  end ;


  procedure T_valueBar.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  begin
    if not checkMouse( _x, _y ) then
      exit ;

    currX := _x ;

    updateValue ;

    Repaint ;

    raiseChange ;

    bMouseDown := True ;
  end ;


  procedure T_valueBar.MouseMove(
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  begin
    if not bMouseDown then
      exit ;

    if not checkMouse( _x, _y ) then
      exit ;

    currX := _x ;

    updateValue ;

    Repaint ;

    raiseChange ;
  end ;


  procedure T_valueBar.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  begin
    if not bMouseDown then
      exit ;

    if not checkMouse( _x, _y ) then begin
      bMouseDown := False ;
      exit ;
    end ;

    currX := _x ;

    updateValue ;

    Repaint ;

    raiseChange ;

    bMouseDown := False ;
  end ;


  procedure T_valueBar.Paint ;
  begin
    if not bInit then
      makeGradient ;

    drawArrows ;

    Canvas.BeginScene ;
    Canvas.DrawBitmap( currState,
      RectF(0, 0, currState.Width, currState.Height),
      RectF(0, 0, currState.Width/vScaleFactor, currState.Height/vScaleFactor),
      1
    ) ;
    Canvas.EndScene ;
  end ;


//==============================================================================
// T_defaultColors
//==============================================================================

  constructor T_defaultColors.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    cWidth  := RoundS( vScaleFactor*16 ) ;
    cHeight := RoundS( vScaleFactor*16 ) ;

    colorList := TList<TGIS_Color>.Create ;
  end ;


  destructor T_defaultColors.Destroy ;
  begin
    FreeObject( colors );
    FreeObject( colorList ) ;

    inherited ;
  end ;


  function T_defaultColors.fget_Color : TGIS_Color ;
  begin
    Result := FColor ;
  end ;


  function T_defaultColors.checkMouse(
    const _x : Single ;
    const _y : Single
  ) : Boolean ;
  var
    clr : TAlphaColor ;
    vbmpdata : TBitmapData;
  begin
    Result := True ;

    if colors.Map( TMapAccess.Read, vbmpdata ) then begin
      try
        clr := vbmpdata.GetPixel(
          TruncS( vScaleFactor*_x ),
          TruncS( vScaleFactor*_y )
        ) ;
        if clr = 0 then
          Result := False  ;
      finally
        colors.Unmap( vbmpdata ) ;
      end;
    end ;
  end ;


  procedure T_defaultColors.makeColorList  ;
  var
    w : Integer ;
    h : Integer ;
    r : Byte ;
    g : Byte ;
    b : Byte ;
    i : Integer ;
    j : Integer ;
  begin
    if not Assigned( colors ) then
      colors := TBitmap.Create ;

    colors.SetSize(
      RoundS( vScaleFactor*Width  ),
      RoundS( vScaleFactor*Height )
    ) ;
    colors.Clear( 0 ) ;

    colors.Canvas.BeginScene ;
    colors.Canvas.Stroke.Thickness := 1 ;
    colors.Canvas.Stroke.Color := TAlphaColorRec.Lightgray ;
    colors.Canvas.Stroke.Kind := TBrushKind.Solid ;
    colors.Canvas.Fill.Color := 0 ;
    colors.Canvas.Fill.Kind := TBrushKind.Solid ;
    colors.Canvas.FillRect(
      RectF( 0, 0, vScaleFactor*Width, vScaleFactor*Height), 0, 0, [], 1
    ) ;

    w := RoundS( vScaleFactor ) ;
    j := 0 ;
    for i := 0 to colorList.Count - 1 do begin
      h := RoundS( j*( cHeight + vScaleFactor*4 ) + vScaleFactor ) ;

      if h + cHeight > vScaleFactor*Height then begin
        w := RoundS( w + cWidth + vScaleFactor*4 ) ;
        j := 0 ;
        h := RoundS( vScaleFactor ) ;
      end ;

      colors.Canvas.Stroke.Color := TAlphaColorRec.Black ;
      colors.Canvas.Stroke.Thickness := vScaleFactor ;
      colors.Canvas.Fill.Color := colorList[i].ARGB ;
      if TForm(Parent).BiDiMode = bdRightToLeft then begin
        colors.Canvas.DrawRect(
          RectF( vScaleFactor*Width - w - cWidth, h,
                 vScaleFactor*Width - w, h + cHeight
          ), 0, 0, [], 1
        ) ;
        colors.Canvas.FillRect(
          RectF( vScaleFactor*Width - w - cWidth, h,
                 vScaleFactor*Width - w, h + cHeight
          ), 0, 0, [], 1
        ) ;
      end
      else begin
        colors.Canvas.DrawRect(
          RectF(w, h, w + cWidth, h + cHeight ), 0, 0, [], 1 ) ;
        colors.Canvas.FillRect(
          RectF(w, h, w + cWidth, h + cHeight ), 0, 0, [], 1 ) ;
      end;
      Inc( j ) ;
    end ;
    colors.Canvas.EndScene ;

    bInit := True ;
  end ;


  procedure T_defaultColors.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Single ;
    _y      : Single
  ) ;
  var
    clr : TAlphaColor ;
    vbmpdata : TBitmapData;
    r,g,b : Byte ;
  begin
    if not checkMouse( _x, _y ) then
      exit ;

    if colors.Map( TMapAccess.Read, vbmpdata ) then begin
      try
        clr := vbmpdata.GetPixel(
          TruncS( vScaleFactor*_x ),
          TruncS( vScaleFactor*_y )
        ) ;
        if clr = 0 then exit ;
        FColor := TGIS_Color.FromRGB( clr ) ;
      finally
        colors.Unmap( vbmpdata ) ;
      end;
    end ;

    if Assigned( FOnColorClick ) then
      FOnColorClick( Self ) ;
  end ;


  procedure T_defaultColors.Paint ;
  begin
    if not bInit then
      makeColorList ;

    Canvas.DrawBitmap(
      colors,
      RectF(0, 0, colors.Width, colors.Height),
      RectF(0, 0, colors.Width/vScaleFactor, colors.Height/vScaleFactor),
      1
    ) ;
  end ;


  procedure T_defaultColors.SetCellSize(
    const _w : Integer ;
    const _h : Integer
  ) ;
  begin
    cWidth  := _w ;
    cHeight := _h ;
  end ;


  procedure T_defaultColors.AddColor(
    const _color : TGIS_Color
  ) ;
  begin
    colorList.Add( _color ) ;

    bInit := False ;
  end ;


  procedure T_defaultColors.Clear ;
  begin
    colorList.Clear ;
  end ;


//==============================================================================
// TGIS_ColorPreview
//==============================================================================

  constructor TGIS_ColorPreview.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    FCellSize := 8 ;
    FColor := TGIS_Color.White ;
    FDialog := True ;

    ClipChildren := True ;
  end ;


  destructor TGIS_ColorPreview.Destroy ;
  begin

    inherited ;
  end ;


  function TGIS_ColorPreview.fget_CellSize : Integer ;
  begin
    Result := FCellSize ;
  end ;


  procedure TGIS_ColorPreview.fset_CellSize(
    const _size : Integer
  ) ;
  begin
    if _size < 2 then
      exit ;

    FCellSize := _size ;

    Repaint ;
  end ;


  function TGIS_ColorPreview.fget_Color : TGIS_Color ;
  begin
    Result := FColor ;
  end ;


  procedure TGIS_ColorPreview.fset_Color(
    const _color : TGIS_Color
  ) ;
  begin
    FColor := _color ;

    Repaint ;
  end ;


  procedure TGIS_ColorPreview.SetEnabled(
    const Value : Boolean
  ) ;
  begin
    inherited ;

    Repaint ;
  end ;


  procedure TGIS_ColorPreview.Click ;
  var
    dlg   : TGIS_ControlColor ;
    proc  : TGIS_Proc ;
  begin
    if not Dialog then
      exit ;

    dlg := TGIS_ControlColor.Create( Self ) ;

    proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result = TGIS_PvlModalResult.OK then begin
          Color := dlg.Color ;
          if Assigned( FOnDialogChange ) then
            FOnDialogChange( Self ) ;
        end ;
      end
    {$IFDEF OXYGENE}){$ENDIF};

    dlg.Execute( Color, proc ) ;
  end ;


  procedure TGIS_ColorPreview.Paint ;
  var
    i    : Integer ;
    imax : Integer ;
    k    : Integer ;
    kmax : Integer ;
    blst : Boolean ;
    bkgm : Double ;
    fggm : Double ;
    r    : Byte ;
    g    : Byte ;
    b    : byte ;
    rb   : Byte ;
    gb   : Byte ;
    bb   : Byte ;
    rw   : Byte ;
    gw   : Byte ;
    bw   : Byte ;
    bmix : Cardinal ;
    wmix : Cardinal ;
    clr  : TGIS_Color ;
  begin
    Canvas.BeginScene ;

    draw_color( Canvas,
                FColor,
                RectF( 0.5, 0.5, Width - 0.5, Height - 0.5 ),
                FCellSize,
                Enabled
              ) ;

    Canvas.Stroke.Kind := TBrushKind.Solid ;
    if Enabled then
      Canvas.Stroke.Color := $FF000000
    else
      Canvas.Stroke.Color :=  $FF888888 ;
    Canvas.Fill.Kind := TBrushKind.None ;

    Canvas.DrawRect( RectF( 0.5, 0.5, Width - 0.5, Height - 0.5 ), 0, 0, [], 1 ) ;
    Canvas.EndScene ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ColorDialog'}
//==============================================================================
// TGIS_ColorDialog
//==============================================================================

  function TGIS_ColorDialog.fget_AlphaColor : TGIS_Color ;
  begin
    Result := cpvColor.Color ;
  end ;


  procedure TGIS_ColorDialog.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_COLOR_CONTROL_COLOR ) ;
    Self.Name := 'TGIS_ColorDialog' ;
    Self.OnShow := FormShow ;
  end ;

  procedure TGIS_ColorDialog.initControls ;
  var
    w : Single ;
    anchors   : TAnchors ;
    anchorsR  : TAnchors ;
    anchorsB  : TAnchors ;
    anchorsRB : TAnchors ;
    text_align : TTextAlign ;
  begin
    w := 36 ;

    vScaleFactor := GUIScale*Self.PPI/Self.SystemPPI ;

    if BiDiMode = bdRightToLeft then begin
      anchors   := [TAnchorKind.akRight, TAnchorKind.akTop] ;
      anchorsR  := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      anchorsB  := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
      anchorsRB := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
    end else begin
      anchors   := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      anchorsR  := [TAnchorKind.akRight, TAnchorKind.akTop] ;
      anchorsB  := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
      anchorsRB := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
    end ;

    imgColor := T_colorWheel.Create( oMainForm ) ;
    imgColor.Parent := oMainForm ;
    imgColor.Anchors := anchors ;
    imgColor.Position.Y := 8 ;
    imgColor.Height := 202 ;
    PlaceControl( BiDiMode, nil, imgColor, 8, 202 ) ;
    T_colorWheel( imgColor ).OnChange := doImgColorChange ;

    scbValue := T_valueBar.Create( oMainForm ) ;
    scbValue.Parent := oMainForm ;
    scbValue.Anchors := anchors ;
    scbValue.Position.Y := imgColor.Position.Y + imgColor.Height + 8 ;
    scbValue.Height := 24 ;
    PlaceControl( BiDiMode, nil, scbValue, 8, imgColor.Width ) ;
    T_valueBar( scbValue ).OnChange := doScbValueChange ;

    scbAlpha := T_valueBar.Create( oMainForm ) ;
    scbAlpha.Parent := oMainForm ;
    scbAlpha.Anchors := anchors ;
    scbAlpha.Position.Y := scbValue.Position.Y + scbValue.Height + 8 ;
    scbAlpha.Height := 24 ;
    PlaceControl( BiDiMode, nil, scbAlpha, 8, imgColor.Width ) ;
    T_valueBar( scbAlpha ).Alpha := True ;
    T_valueBar( scbAlpha ).OnChange := doScbAlphaChange ;

    lblColor := TLabel.Create( oMainForm ) ;
    lblColor.Parent := oMainForm ;
    lblColor.Anchors := anchors ;
    lblColor.Position.Y := imgColor.Position.Y + 10 ;
    PlaceControl( BiDiMode, imgColor, lblColor, HGAP, 150 ) ;
    lblColor.Text := _rsrcna( GIS_RS_COLOR_CONTROL_PREVIEW ) ;
    lblColor.FixSize ;

    cpvColor := TGIS_ColorPreview.Create( oMainForm ) ;
    cpvColor.Parent := oMainForm ;
    cpvColor.Anchors := anchors ;
    cpvColor.Position.Y := lblColor.Position.Y + lblColor.Height ;
    cpvColor.Height := 50 ;
    PlaceControl( BiDiMode, imgColor, cpvColor, HGAP, 150 ) ;
    cpvColor.ClipChildren := True ;
    cpvColor.Dialog := False ;

    if BiDiMode = bdRightToLeft then
      text_align := TTextAlign.Leading
    else
      text_align := TTextAlign.Trailing ;

    edtAlpha := TEdit.Create( oMainForm ) ;
    edtAlpha.Parent := oMainForm ;
    edtAlpha.Anchors := anchors ;
    edtAlpha.Height := 24 ;
    edtAlpha.Position.Y := cpvColor.Position.Y + cpvColor.Height + 8 ;
    PlaceControl( BiDiMode, imgColor, edtAlpha, HGAP2, w ) ;
    edtAlpha.FixSize ;
    edtAlpha.OnChange := doEdtAlphaChange ;
    edtAlpha.KillFocusByReturn := True ;

    lblAlpha := TLabel.Create( oMainForm ) ;
    lblAlpha.Parent := oMainForm ;
    lblAlpha.Anchors := anchors ;
    lblAlpha.TextAlign := text_align ;
    lblAlpha.Text := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) ;
    lblAlpha.Height := 24 ;
    lblAlpha.Width := LABEL_WIDTH ;
    lblAlpha.FixSize ;
    lblAlpha.AlignVertically( edtAlpha ) ;
    PlaceControl( BiDiMode, imgColor, lblAlpha, HGAP, lblAlpha.Width ) ;

    edtRed := TEdit.Create( oMainForm ) ;
    edtRed.Parent := oMainForm ;
    edtRed.Anchors := anchors ;
    edtRed.Height := 24 ;
    edtRed.Position.Y := edtAlpha.Position.Y + edtAlpha.Height + 6 ;
    PlaceControl( BiDiMode, imgColor, edtRed, HGAP2, w ) ;
    edtRed.FixSize ;
    edtRed.OnChange := doEdtRedChange ;
    edtRed.KillFocusByReturn := True ;

    lblRed := TLabel.Create( oMainForm ) ;
    lblRed.Parent := oMainForm ;
    lblRed.Anchors := anchors ;
    lblRed.TextAlign := text_align ;
    lblRed.Text := _rsrcna( GIS_RS_COLOR_CONTROL_RED ) ;
    lblRed.Height := 24 ;
    lblRed.Width := LABEL_WIDTH ;
    lblRed.FixSize ;
    lblRed.AlignVertically( edtRed ) ;
    PlaceControl( BiDiMode, imgColor, lblRed, HGAP, lblRed.Width ) ;

    edtGreen := TEdit.Create( oMainForm ) ;
    edtGreen.Parent := oMainForm ;
    edtGreen.Anchors := anchors ;
    edtGreen.Height := 24 ;
    edtGreen.Position.Y := edtRed.Position.Y + edtRed.Height + 6 ;
    PlaceControl( BiDiMode, imgColor, edtGreen, HGAP2, w ) ;
    edtGreen.FixSize ;
    edtGreen.OnChange := doEdtGreenChange ;
    edtGreen.KillFocusByReturn := True ;

    lblGreen := TLabel.Create( oMainForm ) ;
    lblGreen.Parent := oMainForm ;
    lblGreen.Anchors := anchors ;
    lblGreen.TextAlign := text_align ;
    lblGreen.Text := _rsrcna( GIS_RS_COLOR_CONTROL_GREEN ) ;
    lblGreen.Height := 24 ;
    lblGreen.Width := LABEL_WIDTH ;
    lblGreen.FixSize ;
    lblGreen.AlignVertically( edtGreen ) ;
    PlaceControl( BiDiMode, imgColor, lblGreen, HGAP, lblGreen.Width ) ;

    edtBlue := TEdit.Create( oMainForm ) ;
    edtBlue.Parent := oMainForm ;
    edtBlue.Anchors := anchors ;
    edtBlue.Height := 24 ;
    edtBlue.Position.Y := edtGreen.Position.Y + edtGreen.Height + 6 ;
    PlaceControl( BiDiMode, imgColor, edtBlue, HGAP2, w ) ;
    edtBlue.FixSize ;
    edtBlue.OnChange := doEdtBlueChange ;
    edtBlue.KillFocusByReturn := True ;

    lblBlue := TLabel.Create( oMainForm ) ;
    lblBlue.Parent := oMainForm ;
    lblBlue.Anchors := anchors ;
    lblBlue.TextAlign := text_align ;
    lblBlue.Text := _rsrcna( GIS_RS_COLOR_CONTROL_BLUE ) ;
    lblBlue.Height := 24 ;
    lblBlue.Width := LABEL_WIDTH ;
    lblBlue.FixSize ;
    lblBlue.AlignVertically( edtBlue ) ;
    PlaceControl( BiDiMode, imgColor, lblBlue, HGAP, lblBlue.Width ) ;

    edtHue := TEdit.Create( oMainForm ) ;
    edtHue.Parent := oMainForm ;
    edtHue.Anchors := anchors ;
    edtHue.Height := 24 ;
    edtHue.Position.Y := edtAlpha.Position.Y + edtAlpha.Height + 6 ;
    PlaceControl( BiDiMode, edtRed, edtHue, HGAP3, w ) ;
    edtHue.FixSize ;
    edtHue.OnChange := doEdtHueChange ;
    edtHue.KillFocusByReturn := True ;

    lblHue := TLabel.Create( oMainForm ) ;
    lblHue.Parent := oMainForm ;
    lblHue.Anchors := anchors ;
    lblHue.TextAlign := text_align ;
    lblHue.Text := _rsrcna( GIS_RS_COLOR_CONTROL_HUE ) ;
    lblHue.Height := 24 ;
    lblHue.Width := LABEL_WIDTH ;
    lblHue.FixSize ;
    lblHue.AlignVertically( edtHue ) ;
    PlaceControl( BiDiMode, edtRed, lblHue, HGAP/3, LABEL2_WIDTH ) ;

    edtSat := TEdit.Create( oMainForm ) ;
    edtSat.Parent := oMainForm ;
    edtSat.Anchors := anchors ;
    edtSat.Height := 24 ;
    edtSat.Position.Y := edtRed.Position.Y + edtRed.Height + 6 ;
    PlaceControl( BiDiMode, edtRed, edtSat, HGAP3, w ) ;
    edtSat.FixSize ;
    edtSat.OnChange := doEdtSatChange ;
    edtSat.KillFocusByReturn := True ;

    lblSat := TLabel.Create( oMainForm ) ;
    lblSat.Parent := oMainForm ;
    lblSat.Anchors := anchors ;
    lblSat.TextAlign := text_align ;
    lblSat.Text := _rsrcna( GIS_RS_COLOR_CONTROL_SAT ) ;
    lblSat.Height := 24 ;
    lblSat.Width := LABEL_WIDTH ;
    lblSat.FixSize ;
    lblSat.AlignVertically( edtSat ) ;
    PlaceControl( BiDiMode, edtRed, lblSat, HGAP/3, LABEL2_WIDTH ) ;

    edtLig := TEdit.Create( oMainForm ) ;
    edtLig.Parent := oMainForm ;
    edtLig.Anchors := anchors ;
    edtLig.Height := 24 ;
    edtLig.Position.Y := edtGreen.Position.Y + edtGreen.Height + 6 ;
    PlaceControl( BiDiMode, edtRed, edtLig, HGAP3, w ) ;
    edtLig.FixSize ;
    edtLig.OnChange := doEdtLigChange ;
    edtLig.KillFocusByReturn := True ;

    lblLig := TLabel.Create( oMainForm ) ;
    lblLig.Parent := oMainForm ;
    lblLig.Anchors := anchors ;
    lblLig.TextAlign := text_align ;
    lblLig.Text := _rsrcna( GIS_RS_COLOR_CONTROL_LUM ) ;
    lblLig.Height := 24 ;
    lblLig.Width := LABEL_WIDTH ;
    lblLig.FixSize ;
    lblLig.AlignVertically( edtLig ) ;
    PlaceControl( BiDiMode, edtRed, lblLig, HGAP/3, LABEL2_WIDTH ) ;

    edtHex := TEdit.Create( oMainForm ) ;
    edtHex.Parent := oMainForm ;
    edtHex.Anchors := anchors ;
    edtHex.Position.Y := edtLig.Position.Y + edtLig.Height + 6 ;
    edtHex.Height := 24 ;
    PlaceControl( BiDiMode, imgColor, edtHex, HGAP2,
                  edtLig.Position.X + edtLig.Width - edtBlue.Position.X ) ;
    edtHex.FixSize ;
    edtHex.OnChange := doEdtHexChange ;
    edtHex.KillFocusByReturn := True ;

    lblHex := TLabel.Create( oMainForm ) ;
    lblHex.Parent := oMainForm ;
    lblHex.Anchors := anchors ;
    lblHex.TextAlign := text_align ;
    lblHex.Text := _rsrcna( GIS_RS_COLOR_CONTROL_HEX ) ;
    lblHex.Height := 24 ;
    lblHex.Width := LABEL_WIDTH ;
    lblHex.FixSize ;
    lblHex.AlignVertically( edtHex ) ;
    PlaceControl( BiDiMode, imgColor, lblHex, HGAP, lblHex.Width ) ;

    btnSimple := TRadioButton.Create( oMainForm ) ;
    btnSimple.GroupName := oMainForm.ClassName ;
    btnSimple.Parent := oMainForm ;
    btnSimple.Position.Y := lblColor.Position.Y ;
    btnSimple.Height := 18 ;
    btnSimple.Width := 30 ;
    btnSimple.FixSize ;
    PlaceControl( BiDiMode, cpvColor, btnSimple, HGAP, btnSimple.Width ) ;
    btnSimple.Text := '1' ;
    btnSimple.IsChecked := True ;
    btnSimple.OnClick := doBtnSimpleClick ;
    btnSimple.Visible := False ;

    btnComplex := TRadioButton.Create( oMainForm ) ;
    btnComplex.GroupName := oMainForm.ClassName ;
    btnComplex.Parent := oMainForm ;
    btnComplex.Position.Y := btnSimple.Position.Y ;
    btnComplex.Height := 18 ;
    btnComplex.Width := 30 ;
    btnComplex.FixSize ;
    PlaceControl( BiDiMode, btnSimple, btnComplex, 1, btnComplex.Width ) ;
    btnComplex.Text := '2' ;
    btnComplex.IsChecked := False ;
    btnComplex.OnClick := doBtnComplexClick ;
    btnComplex.Visible := False ;

    imgDefault := T_defaultColors.Create( oMainForm ) ;
    imgDefault.Parent := oMainForm ;
    imgDefault.Position.Y := btnSimple.Position.Y + btnSimple.Height + 4 ;
    imgDefault.Height := scbAlpha.Position.Y + scbAlpha.Height - imgDefault.Position.Y ;
    PlaceControl( BiDiMode, cpvColor, imgDefault, HGAP, 167 ) ;
    T_defaultColors( imgDefault ).SetCellSize(
      RoundS( vScaleFactor*18 ), RoundS( vScaleFactor*18 )
    ) ;
    T_defaultColors( imgDefault ).OnColorClick := doImgDefaultClick ;
    imgDefault.Visible := False ;
    makeSimpleColors ;

    btnExpand := TButton.Create( oMainForm ) ;
    btnExpand.Parent := oMainForm ;
    btnExpand.Height := 24 ;
    btnExpand.Position.Y := Self.ClientHeight - btnExpand.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnExpand, -8, 24 ) ;
    if BidiMode = bdRightToLeft then
      btnExpand.Text := '<'
    else
      btnExpand.Text := '>' ;
    btnExpand.TabOrder := 3 ;
    btnExpand.OnClick := doBtnExpandClick ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      btnExpand.Visible := False ;
      btnComplex.Visible := False ;
      btnSimple.Visible := False ;
    {$ELSE}
      btnHelp.Visible := assigned( pOnHelp ) ;
      btnHelp.Position.Y  := Self.ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
      btnHelp.TabOrder := 0 ;

      btnCancel.Position.Y  := Self.ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, btnExpand, btnCancel, -8, 80 ) ;
      btnCancel.TabOrder := 2 ;

      btnOK.Position.Y  := Self.ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
      btnOK.TabOrder := 1 ;
    {$ENDIF}

    width_hidden   := 8 + Round(imgColor.Width) + HGAP2 +
                      Round(edtBlue.Width) + 3*HGAP +
                      Round(edtLig.Width) + HGAP ;
    width_expanded := width_hidden + Round(imgDefault.Width) + 8 ;
    height_always  := Round(scbAlpha.Position.Y + scbAlpha.Height + 8 +
                            btnExpand.Height + 8) ;

    PlaceControl( BiDiMode, imgColor, lblColor, HGAP, 150 ) ;
    btnSimple.Anchors := anchors ;
    btnComplex.Anchors := anchors ;
    imgDefault.Anchors := anchors ;
    btnExpand.Anchors := anchorsRB ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Anchors := anchorsB ;
      btnCancel.Anchors := anchorsRB ;
      btnOK.Anchors := anchorsRB ;
    {$ENDIF}

    Self.ClientWidth  := width_hidden ;
    Self.ClientHeight := height_always ;

    width_expanded := RoundS( width_expanded * vScaleFactor ) ;
    width_hidden   := RoundS( width_hidden   * vScaleFactor ) ;
  end ;


  procedure TGIS_ColorDialog.lock ;
  begin
    bLock := True ;
  end ;


  procedure TGIS_ColorDialog.unlock ;
  begin
    bLock := False ;
  end ;


  function TGIS_ColorDialog.isLocked : Boolean ;
  begin
    Result := bLock ;
  end ;


  procedure TGIS_ColorDialog.rgb2hsl ;
  var
    r : Byte ;
    g : Byte ;
    b : Byte ;
    clr : TGIS_Color ;
    h : Double ;
    s : Double ;
    l : Double ;
    v : Integer ;
  begin
    if not ( checkInteger( edtRed.Text  , 255, v ) and
             checkInteger( edtGreen.Text, 255, v ) and
             checkInteger( edtBlue.Text , 255, v )
           ) then
      exit ;
    r := StrToInt( edtRed.Text   ) ;
    g := StrToInt( edtGreen.Text ) ;
    b := StrToInt( edtBlue.Text  ) ;

    clr := TGIS_Color.FromRGB( r, g, b ) ;
    clr.ToHSL( h, s, l ) ;

    edtHue.Text := IntToStr( RoundS( 360*h ) ) ;
    edtSat.Text := IntToStr( RoundS( 100*s ) ) ;
    edtLig.Text := IntToStr( RoundS( 100*l ) ) ;
  end ;


  function TGIS_ColorDialog.checkInteger(
    const _str : String  ;
    const _max : Integer ;
    out   _val : Integer
  ) : Boolean ;
  var
    v : Integer ;
  begin
    Result := False ;

    _val := 0 ;

    if not TryStrToInt( _str, v ) then
      exit ;

    if ( v < 0 ) or ( v > _max ) then
      exit ;

    _val := v ;

    Result := True ;
  end ;


  procedure TGIS_ColorDialog.assembleARGB ;
  var
    a : Integer ;
    r : Integer ;
    g : Integer ;
    b : Integer ;
    clr : TGIS_Color ;
    h : Double ;
    s : Double ;
    v : Double ;
  begin
    if not ( checkInteger( edtAlpha.Text, 255, a ) and
             checkInteger( edtRed.Text  , 255, r ) and
             checkInteger( edtGreen.Text, 255, g ) and
             checkInteger( edtBlue.Text , 255, b )
           ) then
      exit ;

    cpvColor.Color := TGIS_Color.FromARGB(
      Byte( a ), Byte( r ), Byte( g ), Byte( b )
    ) ;
    cpvColor.Color.ToHSV( h, s, v ) ;

    T_colorWheel( imgColor ).Color := cpvColor.Color ;
    T_valueBar( scbValue ).Color := T_colorWheel( imgColor ).Color ;
    T_valueBar( scbValue ).Value := v ;
    T_valueBar( scbAlpha ).Color := cpvColor.Color ;
    T_valueBar( scbAlpha ).Value := a/255 ;
  end ;


  function TGIS_ColorDialog.assembleHSL : Boolean ;
  var
    clr : T_RGBVal ;
    hi : Integer ;
    si : Integer ;
    li : Integer ;
    s : Double ;
    l : Double ;
    c : Double ;
    m : Double ;
    r : Double ;
    g : Double ;
    b : Double ;
  begin
    Result := False ;

    if not ( checkInteger( edtHue.Text, 360, hi ) and
             checkInteger( edtSat.Text, 100, si ) and
             checkInteger( edtLig.Text, 100, li )
           ) then
      exit ;

    clr := T_colorWheel( imgColor ).HueToRGB( 1.0*hi ) ;
    s := si/100.0 ;
    l := li/100.0 ;

    c := ( 1 - Abs( 2*l - 1 ) )*s ;
    m := l - c/2 ;

    r := c*clr.R/255 ;
    g := c*clr.G/255 ;
    b := c*clr.B/255 ;

    edtRed.Text   := IntToStr( RoundS( 255*( r + m ) ) ) ;
    edtGreen.Text := IntToStr( RoundS( 255*( g + m ) ) ) ;
    edtBlue.Text  := IntToStr( RoundS( 255*( b + m ) ) ) ;

    assembleARGB ;

    Result := True ;
  end ;


  procedure TGIS_ColorDialog.makeSimpleColors ;
  var
    def : T_defaultColors ;
  begin
    def := T_defaultColors( imgDefault ) ;

    def.Clear ;

    // shades of red
    def.AddColor( TGIS_Color.FromRGB( $FFC0C0 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF9090 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF6060 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF3030 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF0000 ) ) ;

    // ColorBrewer "Single hue" 1
    def.AddColor( TGIS_Color.FromRGB( $eff3ff ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $bdd7e7 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $6baed6 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $3182bd ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $08519c ) ) ;

    // shades of green
    def.AddColor( TGIS_Color.FromRGB( $C0FFC0 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $90FF90 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $60FF60 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $30FF30 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $00FF00 ) ) ;

    // ColorBrewer "Single hue" 2
    def.AddColor( TGIS_Color.FromRGB( $edf8e9 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $bae4b3 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $74c476 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $31a354 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $006d2c ) ) ;

    // shades of blue
    def.AddColor( TGIS_Color.FromRGB( $C0C0FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $9090FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $6060FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $3030FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $0000FF ) ) ;

    // ColorBrewer "Single hue" 3
    def.AddColor( TGIS_Color.FromRGB( $f7f7f7 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $cccccc ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $969696 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $636363 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $252525 ) ) ;

    // shades of yellow
    def.AddColor( TGIS_Color.FromRGB( $FFFFC0 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FFFF90 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FFFF60 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FFFF30 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FFFF00 ) ) ;

    // ColorBrewer "Single hue" 4
    def.AddColor( TGIS_Color.FromRGB( $feedde ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fdbe85 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fd8d3c ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $e6550d ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $a63603 ) ) ;

    // shades of cyan
    def.AddColor( TGIS_Color.FromRGB( $C0FFFF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $90FFFF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $60FFFF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $30FFFF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $00FFFF ) ) ;

    // ColorBrewer "Single hue" 5
    def.AddColor( TGIS_Color.FromRGB( $f2f0f7 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $cbc9e2 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $9e9ac8 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $756bb1 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $54278f ) ) ;

    // shades of fuchsia
    def.AddColor( TGIS_Color.FromRGB( $FFC0FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF90FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF60FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF30FF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $FF00FF ) ) ;

    // ColorBrewer "Single hue" 6
    def.AddColor( TGIS_Color.FromRGB( $fee5d9 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fcae91 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fb6a4a ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $de2d26 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $a50f15 ) ) ;

    // shades of grey
    def.AddColor( TGIS_Color.FromRGB( $FFFFFF ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $B0B0B0 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $808080 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $404040 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $000000 ) ) ;

    def.Repaint ;
  end ;


  procedure TGIS_ColorDialog.makeComplexColors ;
  var
    def : T_defaultColors ;
  begin
    def := T_defaultColors( imgDefault ) ;

    def.Clear ;

    // ColorBrewer "Multi-hue" 1
    def.AddColor( TGIS_Color.FromRGB( $edf8fb ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $b2e2e2 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $66c2a4 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $2ca25f ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $006d2c ) ) ;

    // ColorBrewer "Multi-hue" 2
    def.AddColor( TGIS_Color.FromRGB( $f1eef6 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $d7b5d8 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $df65b0 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $dd1c77 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $980043 ) ) ;

    // ColorBrewer "Multi-hue" 3
    def.AddColor( TGIS_Color.FromRGB( $edf8fb ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $b3cde3 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $8c96c6 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $8856a7 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $810f7c ) ) ;

    // ColorBrewer "Multi-hue" 4
    def.AddColor( TGIS_Color.FromRGB( $feebe2 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fbb4b9 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $f768a1 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $c51b8a ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $7a0177 ) ) ;

    // ColorBrewer "Multi-hue" 5
    def.AddColor( TGIS_Color.FromRGB( $f0f9e8 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $bae4bc ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $7bccc4 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $43a2ca ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $0868ac ) ) ;

    // ColorBrewer "Multi-hue" 6
    def.AddColor( TGIS_Color.FromRGB( $ffffcc ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $c2e699 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $78c679 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $31a354 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $006837 ) ) ;

    // ColorBrewer "Multi-hue" 7
    def.AddColor( TGIS_Color.FromRGB( $fef0d9 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fdcc8a ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fc8d59 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $e34a33 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $b30000 ) ) ;

    // ColorBrewer "Multi-hue" 8
    def.AddColor( TGIS_Color.FromRGB( $ffffcc ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $a1dab4 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $41b6c4 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $2c7fb8 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $253494 ) ) ;

    // ColorBrewer "Multi-hue" 9
    def.AddColor( TGIS_Color.FromRGB( $f1eef6 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $bdc9e1 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $74a9cf ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $2b8cbe ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $045a8d ) ) ;

    // ColorBrewer "Multi-hue" 10
    def.AddColor( TGIS_Color.FromRGB( $ffffd4 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fed98e ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fe9929 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $d95f0e ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $993404 ) ) ;

    // ColorBrewer "Multi-hue" 11
    def.AddColor( TGIS_Color.FromRGB( $f6eff7 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $bdc9e1 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $67a9cf ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $1c9099 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $016c59 ) ) ;

    // ColorBrewer "Multi-hue" 12
    def.AddColor( TGIS_Color.FromRGB( $ffffb2 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fecc5c ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $fd8d3c ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $f03b20 ) ) ;
    def.AddColor( TGIS_Color.FromRGB( $bd0026 ) ) ;

    def.Repaint ;
  end ;

  procedure TGIS_ColorDialog.FormShow(
    _sender : TObject
  ) ;
  var
    off : Integer ;
    len : Integer ;
    mul : Integer ;
  begin
    PlaceControl( BiDiMode, imgColor, lblColor, HGAP,  lblColor.Width ) ;
    PlaceControl( BiDiMode, imgColor, cpvColor, HGAP,  cpvColor.Width ) ;

    {$IFDEF GIS_MOBILE}
      if BiDiMode = bdRightToLeft then
        len := 5
      else
        len := 12 ;
      off := 10 ;
      mul := 0 ;
    {$ELSE}
      off := 0 ;
      len := 0 ;
      mul := 1 ;
    {$ENDIF}

    PlaceControl( BiDiMode, imgColor, lblAlpha, HGAP-off, lblAlpha.Width+len ) ;
    PlaceControl( BiDiMode, imgColor, edtAlpha, HGAP2,    edtAlpha.Width     ) ;
    PlaceControl( BiDiMode, imgColor, lblRed,   HGAP-off, lblRed.Width+len   ) ;
    PlaceControl( BiDiMode, imgColor, edtRed,   HGAP2,    edtRed.Width       ) ;
    PlaceControl( BiDiMode, imgColor, lblGreen, HGAP-off, lblGreen.Width+len ) ;
    PlaceControl( BiDiMode, imgColor, edtGreen, HGAP2,    edtGreen.Width     ) ;
    PlaceControl( BiDiMode, imgColor, lblBlue,  HGAP-off, lblBlue.Width+len  ) ;
    PlaceControl( BiDiMode, imgColor, edtBlue,  HGAP2,    edtBlue.Width      ) ;

    PlaceControl( BiDiMode, edtRed, lblHue, HGAP/3 * mul, lblHue.Width+len/2 ) ;
    PlaceControl( BiDiMode, edtRed, edtHue, HGAP3, edtHue.Width ) ;
    PlaceControl( BiDiMode, edtRed, lblSat, HGAP/3 * mul, lblSat.Width+len/2 ) ;
    PlaceControl( BiDiMode, edtRed, edtSat, HGAP3, edtSat.Width ) ;
    PlaceControl( BiDiMode, edtRed, lblLig, HGAP/3 * mul, lblLig.Width+len/2 ) ;
    PlaceControl( BiDiMode, edtRed, edtLig, HGAP3, edtLig.Width ) ;

    PlaceControl( BiDiMode, imgColor, lblHex, HGAP-off, lblHex.Width+len ) ;
    if BiDiMode = bdRightToLeft then begin
      PlaceControl( BiDiMode, imgColor, edtHex, HGAP2,
                    edtBlue.RightBottomCorner.X - edtLig.Position.X ) ;
      lblColor.Width := lblHex.RightBottomCorner.X - edtHex.Position.X ;
      lblColor.Position.X := edtHex.Position.X ;
      cpvColor.Width := lblHex.RightBottomCorner.X - edtHex.Position.X ;
      cpvColor.Position.X := lblColor.Position.X ;
    end else begin
      PlaceControl( BiDiMode, imgColor, edtHex, HGAP2,
                    edtLig.Position.X + edtLig.Width - edtBlue.Position.X ) ;
      lblColor.Width := edtHex.RightBottomCorner.X - lblHex.Position.X ;
      lblColor.Position.X := lblHex.Position.X ;
      cpvColor.Width := edtHex.RightBottomCorner.X - lblHex.Position.X ;
      cpvColor.Position.X := lblColor.Position.X ;
    end;

    PlaceControl( BiDiMode, cpvColor, btnSimple, HGAP, btnSimple.Width ) ;
    PlaceControl( BiDiMode, btnSimple, btnComplex, 1, btnComplex.Width ) ;
    PlaceControl( BiDiMode, cpvColor, imgDefault, HGAP, imgDefault.Width ) ;

  end ;

  procedure TGIS_ColorDialog.doBtnExpandClick(
    _sender : TObject
  ) ;
  begin
    if imgDefault.Visible then begin
      btnSimple.Visible := False ;
      btnComplex.Visible := False ;
      imgDefault.Visible := False ;
      if BiDiMode = bdRightToLeft then
        Self.Left := Self.Left + ( Self.ClientWidth - width_hidden ) ;
      Self.ClientWidth := width_hidden ;
      if BidiMode = bdRightToLeft then
        btnExpand.Text := '<'
      else
        btnExpand.Text := '>' ;
      end
    else begin
      btnSimple.Visible := True ;
      btnComplex.Visible := True ;
      imgDefault.Visible := True ;
      if BiDiMode = bdRightToLeft then
        Self.Left := Self.Left - ( width_expanded - Self.ClientWidth ) ;
      Self.ClientWidth := width_expanded ;
      if BidiMode = bdRightToLeft then
        btnExpand.Text := '>'
      else
        btnExpand.Text := '<' ;
    end ;
  end ;


  procedure TGIS_ColorDialog.doBtnSimpleClick(
    _sender : TObject
  ) ;
  begin
    makeSimpleColors ;
  end ;

  procedure TGIS_ColorDialog.doBtnComplexClick(
    _sender : TObject
  ) ;
  begin
    makeComplexColors ;
  end ;


  procedure TGIS_ColorDialog.doImgColorChange(
    _sender : TObject
  ) ;
  var
    clr : TGIS_Color ;
    mul : Double ;
    a   : Byte ;
    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
  begin
    lock ;

    clr := T_colorWheel( imgColor ).Color ;
    mul := T_valueBar( scbValue ).Value ;

    T_valueBar( scbValue ).Color := clr ;

    r := FloorS( mul*clr.R ) ;
    g := FloorS( mul*clr.G ) ;
    b := FloorS( mul*clr.B ) ;

    clr := TGIS_color.FromRGB( r, g, b ) ;

    T_valueBar( scbAlpha ).Color := clr ;

    mul := T_valueBar( scbAlpha ).Value ;
    a := RoundS( 255*mul ) ;
    cpvColor.Color := TGIS_Color.FromARGB( a, r, g, b ) ;

    edtHex.Text   := Format( '%.8x', [cpvColor.Color.ARGB] ) ;
    edtAlpha.Text := IntToStr( a ) ;
    edtRed.Text   := IntToStr( r ) ;
    edtGreen.Text := IntToStr( g ) ;
    edtBlue.Text  := IntToStr( b ) ;

    rgb2hsl ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doScbValueChange(
    _sender : TObject
  ) ;
  var
    clr : TGIS_Color ;
    mul : Double ;
    a   : Byte ;
    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
  begin
    lock ;

    clr := T_colorWheel( imgColor ).Color ;
    mul := T_valueBar( scbValue ).Value ;

    r := FloorS( mul*clr.R ) ;
    g := FloorS( mul*clr.G ) ;
    b := FloorS( mul*clr.B ) ;

    clr := TGIS_Color.FromRGB( r, g, b ) ;

    T_valueBar( scbAlpha ).Color := clr ;

    mul := T_valueBar( scbAlpha ).Value ;
    a := RoundS( 255*mul ) ;
    cpvColor.Color := TGIS_Color.FromARGB( a, r, g, b ) ;

    edtHex.Text   := Format( '%.8x', [cpvColor.Color.ARGB] ) ;
    edtAlpha.Text := IntToStr( a ) ;
    edtRed.Text   := IntToStr( r ) ;
    edtGreen.Text := IntToStr( g ) ;
    edtBlue.Text  := IntToStr( b ) ;

    rgb2hsl ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doScbAlphaChange(
    _sender : TObject
  ) ;
  var
    clr : TGIS_Color ;
    a   : Byte ;
  begin
    lock ;

    a := RoundS( 255*T_valueBar( scbAlpha ).Value ) ;
    clr := T_valueBar( scbAlpha ).Color ;
    cpvColor.Color := TGIS_Color.FromARGB( a, clr.R, clr.G, clr.B ) ;

    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;
    edtAlpha.Text := IntToStr( a ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtHexChange(
    _sender : TObject
  ) ;
  var
    a : Byte ;
    r : Byte ;
    g : Byte ;
    b : Byte ;
  begin
    if isLocked then
      exit ;

    if not local_HexStrToARGB( edtHex.Text, a, r, g, b ) then begin
      edtHex.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtHex.FontColor := TColor( $000000 ) ;

    edtAlpha.Text := IntToStr( a ) ;
    edtRed.Text   := IntToStr( r ) ;
    edtGreen.Text := IntToStr( g ) ;
    edtBlue.Text  := IntToStr( b ) ;

    assembleARGB ;
    rgb2hsl ;
    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtAlphaChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtAlpha.Text, 255, v ) then begin
      edtAlpha.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtAlpha.FontColor := TColor( $000000 ) ;

    T_valueBar( scbAlpha ).Value := StrToInt( edtAlpha.Text )/255 ;

    assembleARGB ;
    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtRedChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtRed.Text, 255, v ) then begin
      edtRed.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtRed.FontColor := TColor( $000000 ) ;

    assembleARGB ;
    rgb2hsl ;
    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtGreenChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtGreen.Text, 255, v ) then begin
      edtGreen.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtGreen.FontColor := TColor( $000000 ) ;

    assembleARGB ;
    rgb2hsl ;
    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtBlueChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtBlue.Text, 255, v ) then begin
      edtBlue.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtBlue.FontColor := TColor( $000000 ) ;

    assembleARGB ;
    rgb2hsl ;
    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtHueChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtHue.Text, 360, v ) then begin
      edtHue.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtHue.FontColor := TColor( $000000 ) ;

    assembleHSL ;

    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtSatChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtSat.Text, 100, v ) then begin
      edtSat.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtSat.FontColor := TColor( $000000 ) ;

    assembleHSL ;

    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doEdtLigChange(
    _sender : TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtLig.Text, 100, v ) then begin
      edtLig.FontColor := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtLig.FontColor := TColor( $000000 ) ;

    assembleHSL ;

    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.doImgDefaultClick(
    _sender : TObject
  ) ;
  var
    clr : TGIS_Color ;
    v   : Integer ;
    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
  begin
    clr := T_defaultColors( imgDefault ).Color ;

    lock ;

    if not checkInteger( edtAlpha.Text, 255, v ) then begin
      T_valueBar( scbAlpha ).Value := 1.0 ;
      edtAlpha.Text := IntToStr( 255 ) ;
    end ;

    edtRed.Text   := IntToStr( clr.R ) ;
    edtGreen.Text := IntToStr( clr.G ) ;
    edtBlue.Text  := IntToStr( clr.B ) ;

    assembleARGB ;
    rgb2hsl ;
    edtHex.Text := Format( '%.8x', [cpvColor.Color.ARGB] ) ;

    unlock ;
  end ;


  procedure TGIS_ColorDialog.Execute(
    const _color  : TGIS_Color ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TProc<TModalResult>
  ) ;
  var
    r : Byte ;
    g : Byte ;
    b : Byte ;
    h : Double ;
    s : Double ;
    v : Double ;
  begin
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
    btnHelp.Visible := assigned( pOnHelp ) ;
    {$ENDIF}

    lock ;

    _color.ToHSV( h, s, v ) ;

    T_colorWheel( imgColor ).Color := _color ;
    T_valueBar( scbValue ).Color := T_colorWheel( imgColor ).Color ;
    T_valueBar( scbValue ).Value := v ;
    T_valueBar( scbAlpha ).Color := _color ;
    T_valueBar( scbAlpha ).Value := _color.A/255 ;
    cpvColor.Color := _color ;

    edtHex.Text   := Format( '%.8x', [_color.ARGB] ) ;

    edtAlpha.Text := IntToStr( _color.A ) ;
    edtRed.Text   := IntToStr( _color.R ) ;
    edtGreen.Text := IntToStr( _color.G ) ;
    edtBlue.Text  := IntToStr( _color.B ) ;

    rgb2hsl ;

    unlock ;

    ShowModalEx( _proc ) ;
  end ;

  procedure TGIS_ColorDialog.Execute(
    const _color : TGIS_Color ;
    const _proc  : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _color, hlp, _proc ) ;
  end;
{$ENDREGION}

{$REGION 'TGIS_ColorComboBox'}
{ TGIS_ColorComboBox }

  constructor TGIS_ColorComboBox.Create(_owner: TComponent);
  begin
    inherited;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Color', 3 ) ;
    comboExt.ValueEvent  := doValueColor ;
    comboExt.RenderEvent := doRenderColor ;
    comboExt.CustomEvent := doCustomColor ;
    icomboExt := comboExt ;

    Height := 28 ;
  end;

  procedure TGIS_ColorComboBox.Fill(
    const _field    : Boolean ;
    const _renderer : Boolean
  ) ;

    procedure addItem( const _value : String ) ;
    begin
      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 'c', TGIS_ComboBoxHelperPosition.Top,
          _value, ConstructParamAsText( GIS_PARAMTXT_TYPE_ARGB, _value, '' )
        )
      ) ;
    end ;

  begin
    comboExt.BeginFill ;
    try
      Items.Clear ;

      addItem( 'FF000000' ) ;
      addItem( 'FF000080' ) ;
      addItem( 'FF0000FF' ) ;
      addItem( 'FF008000' ) ;
      addItem( 'FF00FF00' ) ;
      addItem( 'FF00FFFF' ) ;
      addItem( 'FF6BAED6' ) ;
      addItem( 'FF74C476' ) ;
      addItem( 'FF800000' ) ;
      addItem( 'FF808000' ) ;
      addItem( 'FF808080' ) ;
      addItem( 'FF9E9AC8' ) ;
      addItem( 'FFA4A0A0' ) ;
      addItem( 'FFA63603' ) ;
      addItem( 'FFB3CDE3' ) ;
      addItem( 'FFC0C0C0' ) ;
      addItem( 'FFC0C0FF' ) ;
      addItem( 'FFC0DCC0' ) ;
      addItem( 'FFC0FFC0' ) ;
      addItem( 'FFDF65B0' ) ;
      addItem( 'FFF0CAA6' ) ;
      addItem( 'FFFDBE85' ) ;
      addItem( 'FFFEEBE2' ) ;
      addItem( 'FFFF0000' ) ;
      addItem( 'FFFF00FF' ) ;
      addItem( 'FFFFC0C0' ) ;
      addItem( 'FFFFFF00' ) ;
      addItem( 'FFFFFFC0' ) ;
      addItem( 'FFFFFFFF' ) ;

      if _renderer then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            False, 'r', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_GENERAL_BYRENDERER, GIS_PARAMTXT_TYPE_RENDERER
          )
        ) ;

      comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
          GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
        )
      ) ;

      if _field then
        comboExt.SetItem(
          TGIS_ComboBoxHelper.PrepareItem(
            True, 'C', TGIS_ComboBoxHelperPosition.Bottom,
            GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
          )
        ) ;
    finally
      comboExt.EndFill ;
    end ;
  end ;


  function TGIS_ColorComboBox.doValueColor(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );

      if s1 = GIS_PARAMTXT_TYPE_RENDERER then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'r',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_ARGB then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'c',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_FIELD then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'f',
                    TGIS_ComboBoxHelperPosition.Lru,
                    _value,
                    _value
                  ) ;

    except
      Result := '' ;
    end;
  end ;


  function TGIS_ColorComboBox.doCustomColor(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    res : String ;
    s1, s2, s3 : String ;
  begin
    if assigned( FCustomEvent ) then begin
      res := FCustomEvent( Self, _value ) ;
      SplitParamAsText( res, s1, s2, s3 );

      if s1 = GIS_PARAMTXT_TYPE_ARGB then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'c',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
      if s1 = GIS_PARAMTXT_TYPE_FIELD then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'f',
                    TGIS_ComboBoxHelperPosition.Lru,
                    res,
                    res
                  )
      else
        Result := '' ;
    end ;
  end ;


  procedure TGIS_ColorComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
    s1, s2, s3 : String ;
  begin
    if IsStringEmpty( _val ) then
      exit ;

    SplitParamAsText( _val, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_ARGB then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               'c',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               'f',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             ) ;

    comboExt.SetItem( val, True ) ;
  end ;


  procedure TGIS_ColorComboBox.doRenderColor(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  );
  var
    r  : TRectF ;
    r1 : TRectF ;
    str : String ;
    s1, s2, s3 : String ;

    function convert_color(
      const _color : String
    ) : TGIS_Color ;
    var
      s1, s2, s3 : String ;
    begin
      SplitParamAsText( _color, s1, s2, s3 );

      Assert( s1 = GIS_PARAMTXT_TYPE_ARGB ) ;

      Result := TGIS_Color.FromARGB( StrToUInt64( '$'+s2 ) ) ;
    end ;

  begin
    str := '' ;

    r := _rect ;

    case _class of
      'c' : begin
              r1 := _rect ;
              r1.Width  := r1.Height ;
              r1.Left   := r1.Left   + 1.5  ;
              r1.Top    := r1.Top    + 1.5  ;
              r1.Bottom := r1.Bottom - 1.5  ;
              r1.Right  := r1.Right  - 1.5  ;

              _canvas.Fill.Kind := TBrushKind.Solid ;
              _canvas.Fill.Color := TAlphaColorRec.White ;

              _canvas.FillRect( r1, 0, 0, [], _item.AbsoluteOpacity ) ;

              draw_color( _canvas, convert_color(_value), r1, 4, True ) ;

              _canvas.Stroke.Color := _color ;
              _canvas.Stroke.Kind := TBrushKind.Solid ;
              _canvas.Stroke.Thickness := 1 ;
              _canvas.DrawRect( r1, 0, 0, [], _item.AbsoluteOpacity ) ;

              r.Left := r1.Right + r1.Left ;
              str := IntToHex( _canvas.Fill.Color and $FFFFFF, 6 );
            end ;
      'f' : begin
              SplitParamAsText(_caption, s1, s2, s3 ) ;
              if IsStringEmpty(s2) then
                s2 := s1 ;
              str  := s2 ;
            end ;
      else  begin
              str  := _caption ;
            end ;
    end;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( r, str, False,
                      _item.AbsoluteOpacity, [], TTextAlign.Leading
                    );
  end ;

  function TGIS_ColorComboBox.fget_ColorValue : String ;
  begin
    Result := comboExt.Value ;
  end;

  procedure TGIS_ColorComboBox.fset_ColorValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;

  procedure TGIS_ColorComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ColorRampComboBox'}
{ TGIS_ColorRampComboBox }

  constructor TGIS_ColorRampComboBox.Create(_owner: TComponent);
  begin
    inherited;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Bitmap', 3 ) ;
    comboExt.ValueEvent  := doValueBitmap ;
    comboExt.RenderEvent := doRenderBitmap ;
    icomboExt := comboExt ;

    AIdx     := nil ;
    FMode    := TGIS_ColorMapMode.Continuous ;
    FColorSchemas := [ TGIS_ColorSchema.Diverging,
                       TGIS_ColorSchema.Miscellaneous,
                       TGIS_ColorSchema.Qualitative,
                       TGIS_ColorSchema.Sequential   ] ;
    iTextWidth := 0 ;
    iTextGap   := 0 ;
  end;

  destructor TGIS_ColorRampComboBox.Destroy ;
  begin
    FreeObject( bmpMap ) ;

    inherited ;
  end ;

  procedure TGIS_ColorRampComboBox.fset_Mode(
    const _mode : TGIS_ColorMapMode
  ) ;
  begin
    if _mode <> FMode then begin
      FMode := _mode ;
      Fill ;
    end ;
  end ;

  procedure TGIS_ColorRampComboBox.fset_ColorSchemas(
    const _type : TGIS_ColorSchemas
  ) ;
  begin
    if _type <> FColorSchemas then begin
      FColorSchemas := _type ;
      Fill ;
    end ;
  end ;

  procedure TGIS_ColorRampComboBox.fset_Reverse(
    const _reverse : Boolean
  ) ;
  begin
    if _reverse <> FReverse then begin
      FReverse := _reverse ;
      Fill ;
    end ;
  end ;

  procedure TGIS_ColorRampComboBox.gradHorizontal(
    _canvas    : TCanvas ;
    _rect      : TRect ;
    _fromColor : TColor ;
    _toColor   : TColor ;
    _mode      : TGIS_ColorMapMode
  ) ;
   var
     x        : Integer ;
     dr,dg,db : Double ;
     c1,c2    : TColor ;
     r1,r2,
     g1,g2,
     b1,b2    : Byte ;
     r,g,b    : Byte ;
     ir,ig,ib : Integer ;
     cnt      : Integer ;
     w        : Integer ;

      function GetRValue( const _color : Integer ) : Byte ;
      begin
        Result := Byte(( _color and $ff0000 ) shr 16) ;
      end;

      function GetGValue( const _color : Integer ) : Byte ;
      begin
        Result := Byte(( _color and $0000ff00 ) shr 8) ;
      end;

      function GetBValue( const _color : Integer ) : Byte ;
      begin
        Result := Byte( _color and $000000ff ) ;
      end;

   begin
     c1 := _fromColor;
     r1 := GetRValue(c1) ;
     g1 := GetGValue(c1) ;
     b1 := GetBValue(c1) ;

     c2 := _toColor;
     r2 := GetRValue(c2) ;
     g2 := GetGValue(c2) ;
     b2 := GetBValue(c2) ;

     w  := _rect.Right - _rect.Left ;
     if w = 0 then
      w := 1 ;
     dr := (r2-r1) / w ;
     dg := (g2-g1) / w ;
     db := (b2-b1) / w ;

     if _mode = TGIS_ColorMapMode.Continuous then begin
       cnt := 0 ;
       for X := _rect.Left to _rect.Right-1 do begin
         ir := r1 + Ceil(dr*cnt) ;
         if ir > 255 then
           r := 255
         else if ir < 0 then
           r := 0
         else
          r := ir ;
         ig := g1 + Ceil(dg*cnt) ;
         if ig > 255 then
           g := 255
         else if ig < 0 then
           g := 0
         else
          g := ig ;
         ib := b1 + Ceil(db*cnt) ;
         if ib > 255 then
           b := 255
         else if ib < 0 then
           b := 0
         else
          b := ib ;

         _canvas.Stroke.Thickness := 1 ;
         _canvas.Stroke.Color := TGIS_Color.FromRGB(r,g,b).ARGB ;
         _canvas.DrawLine( PointF(X, _rect.Top), PointF(X, _rect.Bottom),1 ) ;
         inc( cnt ) ;
       end ;
     end
     else begin
       _canvas.Fill.Color := TGIS_Color.FromRGB(r1,g1,b1).ARGB ;
       _canvas.FillRect( RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
                         0, 0, [], 1
                       ) ;
     end;
   end ;

  procedure TGIS_ColorRampComboBox.Fill ;
  var
    i_ramp               : Integer ;
    i_color        : Integer ;
    bmp            : TBitmap ;
    rect           : TRect ;
    ramps_count    : Integer ;
    col_map_arr    : TGIS_ColorMapArray ;
    left_colormap        : TGIS_ColorMap ;
    right_colormap       : TGIS_ColorMap ;

    function VCLColor( const _color : TGIS_Color ) : TColor ;
    begin
      Result := _color.ARGB and $00FFFFFF ;
      if Result = $00000000 then
        Result := $00010101 ; // to avoid bad interpretation of NULL
    end ;

    function addItem( const _value : String ) : Integer ;
    begin
      Result := comboExt.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False, 'c', TGIS_ComboBoxHelperPosition.Top,
          _value, ConstructParamAsText( GIS_PARAMTXT_TYPE_TEXTURE, _value, '' )
        )
      ) ;
    end ;

  begin
    FreeObject( bmpMap ) ;

    ItemIndex := -1 ;

    ramps_count := GisColorRampList.Count ;
    if ramps_count = 0 then
      GisColorRampList.Init ;

    bmpMap := TGIS_ObjectList.Create( True ) ;
    rect.Top    := 0 ;
    rect.Bottom := 1 ;

    for i_ramp := 0 to ramps_count - 1 do begin
      if not ( GisColorRampList[i_ramp].MapType in FColorSchemas ) then
        continue ;

      bmp := TBitmap.Create ;
      bmp.Width  := RoundS(Width) ;
      bmp.Height := 1 ;
      bmp.Canvas.BeginScene ;

      col_map_arr := GisColorRampList[i_ramp].RealizeColorMap( Mode, 0, Reverse ) ;

      for i_color := 0 to high( col_map_arr ) - 1 do begin
        left_colormap := col_map_arr[i_color] ;
        right_colormap := col_map_arr[i_color+1] ;

        rect.Left  := RoundS( Width * left_colormap.Index / 100 ) ;
        rect.Right := RoundS( Width * right_colormap.Index / 100 ) ;

        gradHorizontal(
          bmp.Canvas,
          rect,
          VCLColor( left_colormap.RGB ),
          VCLColor( right_colormap.RGB),
          FMode
        ) ;
      end ;

      bmp.Canvas.EndScene ;
      bmpMap.Add( bmp ) ;

      if ( i_ramp mod 20 ) = 0 then
        Application.ProcessMessages ;
    end ;

    SetLength( AIdx, bmpMap.Count ) ;
    comboExt.BeginFill ;
    try
      Items.Clear ;
      for i_ramp := 0 to ramps_count - 1 do begin
        if not (GisColorRampList[i_ramp].MapType in FColorSchemas) then continue ;
        i_color := addItem( GisColorRampList[i_ramp].Name ) ;
        AIdx[i_color] := i_ramp ;
      end ;
    finally
      comboExt.EndFill ;
    end ;
  end ;

  function TGIS_ColorRampComboBox.doValueBitmap(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    s1, s2, s3 : String ;
  begin
    try
      SplitParamAsText( _value, s1, s2, s3 );

      if s1 = GIS_PARAMTXT_TYPE_TEXTURE then
        Result := TGIS_ComboBoxHelper.PrepareItem(
                    False ,
                    'c',
                    TGIS_ComboBoxHelperPosition.Top,
                    _value,
                    _value
                  )
    except
      Result := '' ;
    end;
  end ;

  procedure TGIS_ColorRampComboBox.DelayedUpdate(
    const _val : String
  ) ;
  var
    val : String ;
    s1, s2, s3 : String ;
  begin
    if IsStringEmpty( _val ) then exit ;

    SplitParamAsText( _val, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_TEXTURE then
      val := TGIS_ComboBoxHelper.PrepareItem(
               False ,
               'c',
               TGIS_ComboBoxHelperPosition.Lru,
               _val,
               _val
             ) ;

    comboExt.SetItem( val, True ) ;
  end ;

  procedure TGIS_ColorRampComboBox.doRenderBitmap(
    _item      : TListBoxItem ;
    _canvas    : TCanvas      ;
    _rect      : TRectF       ;
    _font      : TFont        ;
    _color     : TAlphaColor  ;
    _class     : Char         ;
    _caption   : String       ;
    _value     : String
  );
  var
    r   : TRectF ;
    r1  : TRectF ;
    str : String ;
    s1,
    s2,
    s3  : String ;
    bmp : TBitmap ;
    idx : Integer ;
    i   : Integer ;

  begin
    str := '' ;

                SplitParamAsText( _value, s1, s2, s3 );

    idx := AIdx[_item.Index] ;
    if length( GisColorRampList[idx].Map ) = 0 then exit ;


    if iTextGap = 0 then
      iTextGap := RounDS( _canvas.TextWidth( ' ' ) ) ;

    if iTextWidth = 0 then
      for i := 0 to Items.Count -1 do
        iTextWidth := Max( iTextWidth,
                           RoundS( _canvas.TextWidth( GisColorRampList[i].Name ) )
                         ) ;


    case _class of
      'c' : begin
              r1 := _rect ;
              r1.Left   := r1.Left   + 0.5  ;
              r1.Top    := r1.Top    + 0.5  ;
              r1.Bottom := r1.Bottom - 0.5  ;
              r1.Right  := r1.Right  - 0.5 - iTextWidth - 2*iTextGap ;

              bmp := TBitmap(bmpMap[_item.Index]) ;
              _canvas.DrawBitmap( bmp, RectF(0, 0, bmp.Width, bmp.Height),
                                  r1, 1
                                 ) ;
              str := s2;

              r := _rect ;
              r.Left    := _rect.Right - iTextWidth - iTextGap;
              r.Right   := _rect.Right - iTextGap;
            end ;
      else  begin
              r := _rect ;
              r.Left    := iTextGap;
              r.Right   := _rect.Right - iTextGap ;
              str  := _caption ;
            end ;
    end;

    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( r, str, False,
                      1, [], TTextAlign.Leading
                    );
  end ;

  function TGIS_ColorRampComboBox.fget_BitmapValue : String ;
  begin
    Result := comboExt.Value ;
  end;

  procedure TGIS_ColorRampComboBox.fset_BitmapValue(
    const _value : String
  ) ;
  begin
    comboExt.Value := _value ;
  end ;

  procedure TGIS_ColorRampComboBox.fset_OnChange(
    _event : TNotifyEvent
  ) ;
  begin
    FOnChange := _event ;
    comboExt.SetOnChange( FOnChange ) ;
  end ;

  function TGIS_ColorRampComboBox.Value(
    const _subClass   : Integer = -1
  ) : TGIS_ColorMapArray ;
  var
    idx : Integer ;
  begin
    if (ItemIndex >=0) and (ItemIndex < GisColorRampList.Count) then begin
      idx := AIdx[ItemIndex] ;
      Result := GisColorRampList[idx].RealizeColorMap( Mode, _subClass, Reverse ) ;
    end
    else
      Result := nil ;
  end ;

{$ENDREGION}

//==================================== END =====================================
end.

