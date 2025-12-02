// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Controls to manipulate color preview.
}

unit Lider.CG.GIS.VCL.GeoControlColor ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlColor"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  System.Classes,
  System.Types,
  System.UITypes,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  VCL.Graphics,

  Lider.CG.GIS.VCL.GeoModalForm,
  Lider.CG.GIS.VCL.GeoComboBoxHelper,

  Lider.CG.GIS.PVL.GeoPvl,
  Lider.CG.GIS.PVL.GeoControlColor,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI ;

type

  {$REGION 'TGIS_ColorPreview'}
  /// <summary>
  ///   Combobox with colors.
  /// </summary>
  TGIS_ColorPreview = class( TCustomControl )
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
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      /// <param name="_value">
      ///   See documentation for TCustomControl in Delphi help.
      /// </param>
      procedure SetEnabled ( _value : Boolean
                           ) ; override;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      procedure Click      ; override;

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      procedure Paint ; override;

    public

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TCustomControl in Delphi help.
      /// </param>
      constructor Create ( _owner : TComponent
                         ) ; override;

    public

      /// <summary>
      ///   See documentation for TCustomControl in Delphi help.
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
  TGIS_ColorComboBox = class ( TComboBox )
    private
      comboExt        : TGIS_ComboBoxHelper ;
      icomboExt       : IInterface ;
      FCustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
    private
      function  doValueColor    ( _sender  : TObject ;
                                  _value   : String
                                ) : String ;
      function  doCustomColor   ( _sender  : TObject ;
                                  _value   : String
                                ) : String ;
      procedure doRenderColor   ( _control : TComboBox       ;
                                  _rect    : TRect           ;
                                  _state   : TOwnerDrawState ;
                                  _class   : Char            ;
                                  _caption : String          ;
                                  _value   : String
                                ) ;
      function  fget_ColorValue : String ;
      procedure fset_ColorValue ( const _value : String
                                ) ;

    public
      /// <summary>
      ///   See documentation for TComponent in Delphi help.
      /// </summary>
      /// <param name="_owner">
      ///   See documentation for TComponent in Delphi help.
      /// </param>
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
      procedure Fill              ( const _field    : Boolean ;
                                    const _renderer : Boolean
                                  ) ;

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
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_ColorDialog'}
  /// <summary>
  ///   Color dialog called from TGIS_ColorPreview.
  /// </summary>
  TGIS_ColorDialog = class( TGIS_ModalForm )
    private
      bLock      : Boolean ;
    private
      btnExpand  : TButton ;
      imgColor   : TCustomControl ;
      scbValue   : TCustomControl ;
      scbAlpha   : TCustomControl ;
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
      btnSimple  : TRadioButton ;
      btnComplex : TRadioButton ;
      imgDefault : TCustomControl ;
      width_hidden   : Integer ;
      width_expanded : Integer ;
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
      procedure initForm         ; override;

      /// <inheritdoc/>
      procedure initControls     ; override;

      /// <inheritdoc/>
      procedure showForm         ; override;

      /// <inheritdoc/>
      procedure afterPPIChanged  ; override;

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      procedure Resize ; override;

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
      /// <returns>
      ///   Modal value.
      /// </returns>
      function Execute      ( const _color  : TGIS_Color ;
                              const _onhelp : TGIS_HelpEvent
                            ) : Integer ; overload;

      /// <summary>
      ///   Shows the form.
      /// </summary>
      /// <param name="_color">
      ///   starting color value
      /// </param>
      /// <returns>
      ///   Modal value.
      /// </returns>
      function Execute      ( const _color : TGIS_Color
                            ) : Integer ; overload;

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
  ///   Combobox with color ramp.
  /// </summary>
  TGIS_ColorRampComboBox = class ( TComboBox )
    private
      bmpMap        : TGIS_ObjectList ;
      FColorSchemas : TGIS_ColorSchemas ;
      FMode         : TGIS_ColorMapMode ;
      FReverse      : Boolean ;
      AIdx          : TGIS_IntegerArray ;
      iTextWidth    : Integer ;
      iTextGap      : Integer ;
    private
      procedure fset_Mode         ( const _mode : TGIS_ColorMapMode ) ;
      procedure fset_Reverse      ( const _reverse : Boolean ) ;
      procedure fset_ColorSchemas ( const _type : TGIS_ColorSchemas ) ;
    private
      procedure doRampListDrawItem( Control : TWinControl ;
                                    Index   : Integer ;
                                    Rect    : TRect ;
                                    State   : TOwnerDrawState
                                  ) ;
      function  toGisColor        ( const _color : Integer
                                  ) : TGIS_Color ;
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
      ///   Colormap value.
      /// </summary>
      /// <param name="_subClass">
      ///   get subclass of a ramp with specified number of colors, if available;
      ///   if 0, get default colormap;
      ///   if -1, get subclass with maximum number of colors;
      /// </param>
      /// <returns>
      ///   Array of colormap
      /// </returns>
      function Value            ( const _subClass : Integer = 0
                                ) : TGIS_ColorMapArray ;

      /// <summary>
      ///   Color schema filter.
      /// </summary>
      property ColorSchemas     : TGIS_ColorSchemas
                                  read  FColorSchemas
                                  write fset_ColorSchemas ;

      /// <summary>
      ///   Colormap mode.
      /// </summary>
      property Mode             : TGIS_ColorMapMode
                                  read  FMode
                                  write fset_Mode ;

      /// <summary>
      ///   Reverse colormap.
      /// </summary>
      property Reverse          : Boolean
                                  read  FReverse
                                  write fset_Reverse ;

  end ;
  {$ENDREGION}


//##############################################################################
implementation

uses
  System.SysUtils,
  System.Generics.Collections,
  System.Math,
  Vcl.Styles,
  Vcl.Themes,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoXmlDoc,
  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.VCL.GeoControlHelper ;

{$REGION 'TGIS_ColorPreview'}

const
  GIS_COLOR_HUE_SIZE       : Integer = 1530 ;

  GIS_COLOR_WHEEL_MARGIN   : Integer = 14 ;
  GIS_COLOR_WHEEL_MARGIN2  : Integer = 7 ;

  GIS_COLOR_GRADIENT_MARGIN  : Integer = 8 ;
  GIS_COLOR_GRADIENT_MARGIN2 : Integer = 4 ;

const
  HGAP  : Integer = 16 ;
  HGAP2 : Integer = 82 ;
  HGAP3 : Integer = 50 ;
  LABEL_WIDTH : Integer = 64 ;
  LABEL2_WIDTH : Integer = 44 ;

type

  T_RGBVal = record
    R : Byte ;
    G : Byte ;
    B : Byte ;
  end ;


  T_colorWheel = class( TCustomControl )
    private
      arrHue     : array of T_RGBVal ;
      bInit      : Boolean ;
      colorWheel : TBitmap ;
      currState  : TBitmap ;
      currX      : Integer ;
      currY      : Integer ;
      bMouseDown : Boolean ;
      iPPI       : Integer ;
    private
      FColor     : TGIS_Color ;
    private
      FOnChange  : TNotifyEvent ;
    private
      function  fget_Color : TGIS_Color ;
      procedure fset_Color ( const _color : TGIS_Color
                           ) ;
    private
      function  readPPI        : Integer ;
      function  ppiFix         ( const _value : Integer
                               ) : Integer ;
      procedure prepareHue     ;
      function  angleToRGB     ( const _phi : Double
                               ) : T_RGBVal ;
      function  checkMouse     ( const _x : Integer ;
                                 const _y : Integer
                               ) : Boolean ;
      procedure makeColorWheel ;
      procedure drawCrosshair  ;
      procedure raiseChange    ;
    protected
      procedure MouseDown ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
                          ) ; override;
      procedure MouseMove ( _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
                          ) ; override;
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
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


  T_valueBar = class( TCustomControl )
    protected
      bInit      : Boolean ;
      gradient   : TBitmap ;
      currState  : TBitmap ;
      currX      : Integer ;
      bMouseDown : Boolean ;
      iPPI       : Integer ;
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
      function  readPPI     : Integer ;
      function  ppiFix       ( const _value : Integer
                             ) : Integer ;
      function  checkMouse  ( const _x : Integer ;
                              const _y : Integer
                            ) : Boolean ;
      procedure drawArrows  ;
      procedure raiseChange ;
    protected
      procedure makeGradient ; //virtual;
      procedure makeColor    ;
      procedure makeAlpha    ;
    protected
      procedure MouseDown ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
                          ) ; override;
      procedure MouseMove ( _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
                          ) ; override;
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
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


  T_defaultColors = class( TCustomControl )
    private
      colorList : TList<TGIS_Color> ;
      bInit     : Boolean ;
      colors    : TBitmap ;
      cWidth    : Integer ;
      cHeight   : Integer ;
      iPPI      : Integer ;
    private
      FColor    : TGIS_Color ;
    private
      FOnColorClick : TNotifyEvent ;
    private
      function  fget_Color : TGIS_Color ;
    private
      function  readPPI        : Integer ;
      function  ppiFix         ( const _value : Integer
                               ) : Integer ;
      function  checkMouse     ( const _x : Integer ;
                                 const _y : Integer
                               ) : Boolean ;
      procedure makeColorList  ;
    protected
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x      : Integer ;
                            _y      : Integer
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
    for i := Length( _str ) downto 1 do begin

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

      if i = Length( _str ) then
        mul := 1
      else begin
        mul := 16 ;
        for k := 1 to Length( _str ) - i - 1 do
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
    _rect     : TRect      ;
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

    imax := RoundS( _rect.Width  / _cellsize ) + 1 ;
    kmax := RoundS( _rect.Height / _cellsize ) + 1 ;

    fggm := clr.A / 255 ;
    bkgm := 1.0 - fggm ;

    r := clr.R ;
    g := clr.G ;
    b := clr.B ;

    bmix := ( RoundS( bkgm * rb + fggm * b ) shl 16 ) +
            ( RoundS( bkgm * gb + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bb + fggm * r )        ) ;
    wmix := ( RoundS( bkgm * rw + fggm * b ) shl 16 ) +
            ( RoundS( bkgm * gw + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bw + fggm * r )        ) ;

    _canvas.Brush.Style := TBrushStyle.bsSolid ;
    for k := 0 to kmax do begin

      blst := ( k mod 2 ) = 0 ;

      for i := 0 to imax do begin

        if blst then begin
          _canvas.Pen.Color   := TColor( bmix ) ;
          _canvas.Brush.Color := TColor( bmix ) ;
        end
        else begin
          _canvas.Pen.Color   := TColor( wmix ) ;
          _canvas.Brush.Color := TColor( wmix ) ;
        end ;

        _canvas.Rectangle(
          Min( _rect.Right , _rect.Left + i * _cellsize + 1 ),
          Min( _rect.Bottom, _rect.Top  + k * _cellsize + 1 ),
          Min( _rect.Right , _rect.Left + ( i + 1 ) * _cellsize + 1 ),
          Min( _rect.Bottom, _rect.Top  + ( k + 1 ) * _cellsize + 1 )
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

    DoubleBuffered := not IsWin11 ;

    FColor := TGIS_Color.White ;

    iPPI := readPPI ;

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
    rs  : Double ;
    rc  : Double ;
  begin
    _color.ToHSV( h, s, v ) ;

    rad := 2*PI*h ;

    SinCos( rad, rs, rc ) ;
    y := s*rs ;
    x := s*rc ;
    w2 := ( Width  - GIS_COLOR_WHEEL_MARGIN )/2 ;

    currX := RoundS( x*w2 + Width/2 ) ;
    currY := RoundS( Height/2 - y*w2 ) ;

    Repaint ;
  end ;

  function T_colorWheel.readPPI : Integer ;
  begin
    Result := TGIS_ModalForm(Owner).CurrentPPI ;
  end;

  function T_colorWheel.ppiFix(
    const _value : Integer
  ) : Integer ;
  begin
    Result := MulDiv( _value, iPPI, 96 ) ;
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
    const _x : Integer ;
    const _y : Integer
  ) : Boolean ;
  var
    w2 : Integer ;
    h2 : Integer ;
    x  : Integer ;
    y  : Integer ;
    r  : Double ;
  begin
    Result := True ;

    w2 := RoundS( ( Width  - GIS_COLOR_WHEEL_MARGIN )/2 ) ;
    h2 := RoundS( ( Height - GIS_COLOR_WHEEL_MARGIN )/2 ) ;

    y := h2 - _y + GIS_COLOR_WHEEL_MARGIN2 ;
    x := _x - w2 - GIS_COLOR_WHEEL_MARGIN2 ;

    r := Sqrt( x*x + y*y )/w2 ;
    if r > 1.0 then
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
    ptr : pByteArray ;
  begin
    if not assigned( colorWheel ) then
      colorWheel := TBitmap.Create ;
    colorWheel.PixelFormat := TPixelFormat.pf24bit ;
    colorWheel.SetSize( Width, Height ) ;

    colorWheel.Canvas.Pen.Color := StyleServices.GetSystemColor( clBtnFace ) ;
    colorWheel.Canvas.Pen.Style := TPenStyle.psSolid ;
    colorWheel.Canvas.Brush.Color := StyleServices.GetSystemColor( clBtnFace ) ;
    colorWheel.Canvas.Brush.Style := TBrushStyle.bsSolid ;
    colorWheel.Canvas.Rectangle( 0, 0, Width, Height ) ;

    w2 := RoundS( ( Width  - GIS_COLOR_WHEEL_MARGIN )/2 ) ;
    h2 := RoundS( ( Height - GIS_COLOR_WHEEL_MARGIN )/2 ) ;

    for i := GIS_COLOR_WHEEL_MARGIN2
        to colorWheel.Height - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

      ptr := colorWheel.ScanLine[i] ;
      y := h2 - i + GIS_COLOR_WHEEL_MARGIN2 ;

      for k := GIS_COLOR_WHEEL_MARGIN2
          to colorWheel.Width - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

        x := k - w2 - GIS_COLOR_WHEEL_MARGIN2 ;

        r := Sqrt( x*x + y*y )/w2 ;
        if r > 1.0 then
          continue ;

        a := ArcTan2( y, x ) ;

        hue := angleToRGB( a ) ;

        ptr[3*k  ] := 255 - RoundS( r*hue.B ) ;
        ptr[3*k+1] := 255 - RoundS( r*hue.G ) ;
        ptr[3*k+2] := 255 - RoundS( r*hue.R ) ;
      end ;

    end ;

    if not assigned( currState ) then
      currState := TBitmap.Create ;
    currState.PixelFormat := TPixelFormat.pf24bit ;
    currState.SetSize( Width, Height ) ;

    bInit := True ;
  end ;


  procedure T_colorWheel.drawCrosshair ;
  begin
    currState.Canvas.Draw( 0, 0, colorWheel ) ;

    currState.Canvas.Pen.Style := TPenStyle.psSolid ;
    currState.Canvas.Pen.Width := ppiFix(1) ;

    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX - ppiFix(1), currY - ppiFix(2) ) ;
    currState.Canvas.LineTo( currX - ppiFix(1), currY - ppiFix(7) ) ;
    currState.Canvas.MoveTo( currX            , currY - ppiFix(2) ) ;
    currState.Canvas.LineTo( currX            , currY - ppiFix(7) ) ;
    currState.Canvas.Pen.Color := TColor( $FFFFFF ) ;
    currState.Canvas.MoveTo( currX            , currY - ppiFix(3) ) ;
    currState.Canvas.LineTo( currX            , currY - ppiFix(6) ) ;
    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX + ppiFix(1), currY - ppiFix(2) ) ;
    currState.Canvas.LineTo( currX + ppiFix(1), currY - ppiFix(7) ) ;

    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX - ppiFix(1), currY + ppiFix(2) ) ;
    currState.Canvas.LineTo( currX - ppiFix(1), currY + ppiFix(7) ) ;
    currState.Canvas.MoveTo( currX            , currY + ppiFix(2) ) ;
    currState.Canvas.LineTo( currX            , currY + ppiFix(7) ) ;
    currState.Canvas.Pen.Color := TColor( $FFFFFF ) ;
    currState.Canvas.MoveTo( currX            , currY + ppiFix(3) ) ;
    currState.Canvas.LineTo( currX            , currY + ppiFix(6) ) ;
    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX + ppiFix(1), currY + ppiFix(2) ) ;
    currState.Canvas.LineTo( currX + ppiFix(1), currY + ppiFix(7) ) ;

    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX - ppiFix(2), currY - ppiFix(1) ) ;
    currState.Canvas.LineTo( currX - ppiFix(7), currY - ppiFix(1) ) ;
    currState.Canvas.MoveTo( currX - ppiFix(2), currY             ) ;
    currState.Canvas.LineTo( currX - ppiFix(7), currY             ) ;
    currState.Canvas.Pen.Color := TColor( $FFFFFF ) ;
    currState.Canvas.MoveTo( currX - ppiFix(3), currY             ) ;
    currState.Canvas.LineTo( currX - ppiFix(6), currY             ) ;
    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX - ppiFix(2), currY + ppiFix(1) ) ;
    currState.Canvas.LineTo( currX - ppiFix(7), currY + ppiFix(1) ) ;

    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX + ppiFix(2), currY - ppiFix(1) ) ;
    currState.Canvas.LineTo( currX + ppiFix(7), currY - ppiFix(1) ) ;
    currState.Canvas.MoveTo( currX + ppiFix(2), currY             ) ;
    currState.Canvas.LineTo( currX + ppiFix(7), currY             ) ;
    currState.Canvas.Pen.Color := TColor( $FFFFFF ) ;
    currState.Canvas.MoveTo( currX + ppiFix(3), currY             ) ;
    currState.Canvas.LineTo( currX + ppiFix(6), currY             ) ;
    currState.Canvas.Pen.Color := TColor( $000000 ) ;
    currState.Canvas.MoveTo( currX + ppiFix(2), currY + ppiFix(1) ) ;
    currState.Canvas.LineTo( currX + ppiFix(7), currY + ppiFix(1) ) ;
  end ;


  procedure T_colorWheel.raiseChange ;
  begin
    if Assigned( FOnChange ) then
      FOnChange( Self ) ;
  end ;


  procedure T_colorWheel.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
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
    _x      : Integer ;
    _y      : Integer
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
    _x      : Integer ;
    _y      : Integer
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
  var
    ppi : Integer ;
  begin
    ppi := readPPI ;
    if ( not bInit ) or ( ppi <> iPPI ) then begin
      currX := currX * ppi div iPPI ;
      currY := currY * ppi div iPPI ;
      iPPI := ppi ;
      makeColorWheel ;
    end ;

    drawCrosshair ;

    Canvas.Draw( 0, 0, currState ) ;
  end ;


//==============================================================================
// T_valueBar
//==============================================================================

  constructor T_valueBar.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    DoubleBuffered := not IsWin11 ;

    iPPI := readPPI ;

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

  function T_valueBar.readPPI : Integer ;
  begin
    Result := TGIS_ModalForm(Owner).CurrentPPI ;
  end;

  function T_valueBar.ppiFix(
    const _value : Integer
  ) : Integer ;
  begin
    Result := MulDiv( _value, iPPI, 96 ) ;
  end ;

  function T_valueBar.checkMouse(
    const _x : Integer ;
    const _y : Integer
  ) : Boolean ;
  begin
    Result := True ;

    if ( _x < GIS_COLOR_GRADIENT_MARGIN2             ) or
       ( _x > Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 ) then
      Result := False ;
  end ;


  procedure T_valueBar.makeGradient ;
  begin
    FreeObject( gradient ) ;

    gradient := TBitmap.Create ;
    gradient.PixelFormat := TPixelFormat.pf24bit ;
    gradient.SetSize( Width, Height ) ;

    gradient.Canvas.Pen.Color := StyleServices.GetSystemColor( clBtnFace ) ;
    gradient.Canvas.Pen.Style := TPenStyle.psSolid ;
    gradient.Canvas.Brush.Color := StyleServices.GetSystemColor( clBtnFace ) ;
    gradient.Canvas.Brush.Style := TBrushStyle.bsSolid ;
    gradient.Canvas.Rectangle( 0, 0, Width, Height ) ;

    if Alpha then
      makeAlpha
    else
      makeColor ;

    FreeObject( currState ) ;

    currState := TBitmap.Create ;
    currState.PixelFormat := TPixelFormat.pf24bit ;
    currState.SetSize( Width, Height ) ;

    bInit := True ;
  end ;


  procedure T_valueBar.makeColor ;
  var
    ptr : pByteArray ;
    i   : Integer ;
    k   : Integer ;
  begin
    inherited ;

    for i := GIS_COLOR_GRADIENT_MARGIN2
        to Height - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

      ptr := gradient.ScanLine[i] ;

      for k := GIS_COLOR_GRADIENT_MARGIN2
          to Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

        ptr[3*k  ] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.B
          ) ;
        ptr[3*k+1] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.G
          ) ;
        ptr[3*k+2] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.R
          ) ;

      end ;

    end ;

    gradient.Canvas.Pen.Color := TColor( $000000 ) ;
    gradient.Canvas.Brush.Style := TBrushStyle.bsClear ;
    gradient.Canvas.Rectangle(
      GIS_COLOR_GRADIENT_MARGIN2 - 1 ,
      GIS_COLOR_GRADIENT_MARGIN2 - 1,
      Width  - GIS_COLOR_GRADIENT_MARGIN2 + 1,
      Height - GIS_COLOR_GRADIENT_MARGIN2 + 1
    ) ;
  end ;


  procedure T_valueBar.makeAlpha ;
  var
    ptr : pByteArray ;
    mul : Double ;
    i   : Integer ;
    k   : Integer ;
    rb  : Boolean ;
    cb  : Boolean ;

    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
    sz  : Integer ;
  begin
    inherited ;

    r := FColor.R ;
    g := FColor.G ;
    b := FColor.B ;

    sz := MulDiv( 8, iPPI, 96 ) ;
    cb := False ;

    for i := GIS_COLOR_GRADIENT_MARGIN2
        to Height - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

      if ( ( i - GIS_COLOR_GRADIENT_MARGIN2 ) mod sz ) = 0 then
        cb := not cb ;
      rb := cb ;

      ptr := gradient.ScanLine[i] ;

      for k := GIS_COLOR_GRADIENT_MARGIN2
          to Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

        if ( ( k - GIS_COLOR_GRADIENT_MARGIN2 ) mod sz ) = 0 then
          rb := not rb ;

        mul := ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
               ( Width - GIS_COLOR_GRADIENT_MARGIN ) ;

        if rb then begin
          ptr[3*k  ] := RoundS( mul * b + ( 1 - mul ) * $C0 ) ;
          ptr[3*k+1] := RoundS( mul * g + ( 1 - mul ) * $C0 ) ;
          ptr[3*k+2] := RoundS( mul * r + ( 1 - mul ) * $C0 ) ;
        end
        else begin
          ptr[3*k  ] := RoundS( mul * b + ( 1 - mul ) * $FF ) ;
          ptr[3*k+1] := RoundS( mul * g + ( 1 - mul ) * $FF ) ;
          ptr[3*k+2] := RoundS( mul * r + ( 1 - mul ) * $FF ) ;
        end ;

      end ;

    end ;

    gradient.Canvas.Pen.Color := TColor( $000000 ) ;
    gradient.Canvas.Brush.Style := TBrushStyle.bsClear ;
    gradient.Canvas.Rectangle(
      GIS_COLOR_GRADIENT_MARGIN2 - 1 ,
      GIS_COLOR_GRADIENT_MARGIN2 - 1,
      Width  - GIS_COLOR_GRADIENT_MARGIN2 + 1,
      Height - GIS_COLOR_GRADIENT_MARGIN2 + 1
    ) ;
  end ;


  procedure T_valueBar.drawArrows ;
  begin
    currState.Canvas.Draw( 0, 0, gradient ) ;

    currState.Canvas.Brush.Color := clWhite ;
    currState.Canvas.Pen.Color   := clBlack ;
    currState.Canvas.Polygon( [
      Point( currX - 1 , 0          ),
      Point( currX - 2 , 1          ),
      Point( currX - 2 , Height - 2 ),
      Point( currX - 1 , Height - 1 ),
      Point( currX + 1 , Height - 1 ),
      Point( currX + 2 , Height - 2 ),
      Point( currX + 2 , 1          ),
      Point( currX + 1 , 0          ),
      Point( currX - 1 , 0          )
    ] );
  end ;


  procedure T_valueBar.raiseChange ;
  begin
    if Assigned( FOnChange ) then
      FOnChange( Self ) ;
  end ;


  procedure T_valueBar.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  begin
    if not checkMouse( _x, _y ) then
      exit ;

    currX := _x ;

    FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;

    Repaint ;

    raiseChange ;

    bMouseDown := True ;
  end ;


  procedure T_valueBar.MouseMove(
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  begin
    if not bMouseDown then
      exit ;

    if not checkMouse( _x, _y ) then
      exit ;

    currX := _x ;

    FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;

    Repaint ;

    raiseChange ;
  end ;


  procedure T_valueBar.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  begin
    if not bMouseDown then
      exit ;

    if not checkMouse( _x, _y ) then begin
      bMouseDown := False ;
      exit ;
    end ;

    currX := _x ;

    FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;

    Repaint ;

    raiseChange ;

    bMouseDown := False ;
  end ;


  procedure T_valueBar.Paint ;
  var
    ppi : Integer ;
  begin
    ppi := readPPI ;
    if ( not bInit ) or ( ppi <> iPPI ) then begin
      currX := currX * ppi div iPPI ;
      iPPI := ppi ;
      makeGradient ;
    end ;

    drawArrows ;

    Canvas.Draw( 0, 0, currState ) ;
  end ;


//==============================================================================
// T_defaultColors
//==============================================================================

  constructor T_defaultColors.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    DoubleBuffered := not IsWin11 ;

    iPPI := readPPI ;
    cWidth  := ppiFix( 16 ) ;
    cHeight := ppiFix( 16 ) ;

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

  function T_defaultColors.readPPI : Integer ;
  begin
    Result := TGIS_ModalForm(Owner).CurrentPPI ;
  end ;

  function T_defaultColors.ppiFix(
    const _value : Integer
  ) : Integer ;
  begin
    Result := MulDiv( _value, iPPI, 96 ) ;
  end ;

  function T_defaultColors.checkMouse(
    const _x : Integer ;
    const _y : Integer
  ) : Boolean ;
  begin
    Result := True ;

    if colors.Canvas.Pixels[_x,_y] = clBtnFace then
      Result := False ;
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
    gap : Integer ;
  begin
    if not Assigned( colors ) then
      colors := TBitmap.Create ;

    colors.PixelFormat := TPixelFormat.pf24bit ;
    colors.SetSize( Width, Height ) ;

    colors.Canvas.Pen.Color := StyleServices.GetSystemColor( clBtnFace ) ;
    colors.Canvas.Pen.Style := TPenStyle.psSolid ;
    colors.Canvas.Brush.Color := StyleServices.GetSystemColor( clBtnFace ) ;
    colors.Canvas.Brush.Style := TBrushStyle.bsSolid ;
    colors.Canvas.Rectangle( 0, 0, Width, Height ) ;

    gap := ppiFix( 4 ) ;

    w := 0 ;
    j := 0 ;
    for i := 0 to colorList.Count - 1 do begin
      h := j*( cHeight + gap ) ;

      if i mod 10 = 0 then begin
        if i <> 0 then
          w := w + cWidth + gap ;
        j := 0 ;
        h := 0 ;
      end ;

      colors.Canvas.Pen.Color := TColor( $000000 ) ;
      colors.Canvas.Brush.Color := colorList[i].ToBGR ;
      if Parent.BiDiMode = bdRightToLeft then
        colors.Canvas.Rectangle( Width - w - cWidth, h, Width - w, h + cHeight )
      else
        colors.Canvas.Rectangle( w, h, w + cWidth, h + cHeight ) ;

      Inc( j ) ;
    end ;

    bInit := True ;
  end ;


  procedure T_defaultColors.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x      : Integer ;
    _y      : Integer
  ) ;
  var
    clr : TColor ;
  begin
    if not checkMouse( _x, _y ) then
      exit ;

    clr := colors.Canvas.Pixels[_x,_y] ;
    if clr = StyleServices.GetSystemColor( clBtnFace ) then
      exit ;

    FColor := TGIS_Color.FromBGR( clr ) ;

    if Assigned( FOnColorClick ) then
      FOnColorClick( Self ) ;
  end ;


  procedure T_defaultColors.Paint ;
  var
    ppi : Integer ;
  begin
    ppi := readPPI ;
    if ( not bInit ) or ( ppi <> iPPI ) then begin
      iPPI := ppi ;
      makeColorList ;
    end;

    Canvas.Draw( 0, 0, colors ) ;
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

    DoubleBuffered := not IsWin11 ;

    FCellSize := 8 ;
    FColor := TGIS_Color.White ;
    FDialog := True ;
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
    _value : Boolean
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
  begin
    draw_color( Canvas, FColor, Canvas.ClipRect, FCellSize, Enabled ) ;

    if Enabled then
      Canvas.Pen.Color := TColor( $000000 )
    else
      Canvas.Pen.Color := TColor( $888888 ) ;
    Canvas.Brush.Style := TBrushStyle.bsClear ;

    Canvas.Rectangle( 0, 0, Width, Height ) ;
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
    Self.ClientWidth  := 500 ;
    Self.ClientHeight := 320 ;
    Self.Name := 'TGIS_ColorDialog' ;
  end;

  procedure TGIS_ColorDialog.initControls ;
  var
    w : Integer ;
    anchors   : TAnchors ;
    anchorsR  : TAnchors ;
    anchorsB  : TAnchors ;
    anchorsRB : TAnchors ;
  begin
    w := 30 ;

    if BiDiMode = bdRightToLeft then begin
      anchors := [akRight, akTop] ;
      anchorsR := [akLeft, akTop] ;
      anchorsB  := [akRight, akBottom] ;
      anchorsRB := [akLeft, akBottom] ;
    end else begin
      anchors   := [akLeft, akTop] ;
      anchorsR  := [akRight, akTop] ;
      anchorsB  := [akLeft, akBottom] ;
      anchorsRB := [akRight, akBottom] ;
    end ;

    btnExpand := TButton.Create( Self ) ;
    btnExpand.Parent := Self ;
    btnExpand.Anchors := anchorsRB ;
    btnExpand.Height := 25 ;
    btnExpand.Top  := Self.ClientHeight - btnExpand.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnExpand, -8, 25 ) ;
    btnExpand.Caption := '>' ;
    btnExpand.TabOrder := 3 ;
    btnExpand.OnClick := doBtnExpandClick ;

    PlaceControl( BiDiMode, btnExpand, btnCancel, -8, btnCancel.Width ) ;
    btnCancel.TabOrder := 2 ;

    PlaceControl( BiDiMode, btnCancel, btnOK, -8, btnOK.Width ) ;
    btnOK.TabOrder := 1 ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 0 ;

    imgColor := T_colorWheel.Create( Self ) ;
    imgColor.Parent := Self ;
    imgColor.Anchors := anchors ;
    imgColor.Top := 8 ;
    imgColor.Height := 202 ;
    PlaceControl( BiDiMode, nil, imgColor, 8, 202 ) ;
    T_colorWheel( imgColor ).OnChange := doImgColorChange ;

    scbValue := T_valueBar.Create( Self ) ;
    scbValue.Parent := Self ;
    scbValue.Top := imgColor.Top + imgColor.Height + 8 ;
    scbValue.Anchors := anchors ;
    scbValue.Height := 24 ;
    PlaceControl( BiDiMode, nil, scbValue, 8, imgColor.Width ) ;
    T_valueBar( scbValue ).OnChange := doScbValueChange ;

    scbAlpha := T_valueBar.Create( Self ) ;
    scbAlpha.Parent := Self ;
    scbAlpha.Anchors := anchors ;
    scbAlpha.Top := scbValue.Top + scbValue.Height + 8 ;
    scbAlpha.Height := 24 ;
    PlaceControl( BiDiMode, nil, scbAlpha, 8, imgColor.Width ) ;
    T_valueBar( scbAlpha ).Alpha := True ;
    T_valueBar( scbAlpha ).OnChange := doScbAlphaChange ;

    lblColor := TLabel.Create( Self ) ;
    lblColor.Parent := Self ;
    lblColor.Anchors := anchors ;
    lblColor.Top := imgColor.Top + 10 ;
    PlaceControl( BiDiMode, imgColor, lblColor, HGAP, 150 ) ;
    lblColor.AutoSize := False ;
    lblColor.Caption := _rsrc( GIS_RS_COLOR_CONTROL_PREVIEW ) ;

    cpvColor := TGIS_ColorPreview.Create( Self ) ;
    cpvColor.Parent := Self ;
    cpvColor.Anchors := anchors ;
    cpvColor.Top := lblColor.Top + lblColor.Height + 2 ;
    cpvColor.Height := 50 ;
    PlaceControl( BiDiMode, imgColor, cpvColor, HGAP, 150 ) ;
    cpvColor.Dialog := False ;

    edtAlpha := TEdit.Create( Self ) ;
    edtAlpha.Parent := Self ;
    edtAlpha.Anchors := anchors ;
    edtAlpha.Top := cpvColor.Top + cpvColor.Height + 8 ;
    PlaceControl( BiDiMode, imgColor, edtAlpha, HGAP2, w ) ;
    edtAlpha.AutoSize := False ;
    edtAlpha.TabOrder := 4 ;
    edtAlpha.OnChange := doEdtAlphaChange ;

    lblAlpha := TLabel.Create( Self ) ;
    lblAlpha.Parent := Self ;
    lblAlpha.Anchors := anchors ;
    lblAlpha.Alignment := taRightJustify ;
    lblAlpha.Caption := _rsrc( GIS_RS_COLOR_CONTROL_ALPHA ) ;
    PlaceControl( BiDiMode, imgColor, lblAlpha, HGAP, LABEL_WIDTH ) ;
    lblAlpha.AutoSize := False ;

    edtRed := TEdit.Create( Self ) ;
    edtRed.Parent := Self ;
    edtRed.Anchors := anchors ;
    edtRed.Top := edtAlpha.Top + edtAlpha.Height + 6 ;
    PlaceControl( BiDiMode, imgColor, edtRed, HGAP2, w ) ;
    edtRed.AutoSize := False ;
    edtRed.TabOrder := 5 ;
    edtRed.OnChange := doEdtRedChange ;

    lblRed := TLabel.Create( Self ) ;
    lblRed.Parent := Self ;
    lblRed.Anchors := anchors ;
    lblRed.Alignment := taRightJustify ;
    lblRed.Caption := _rsrc( GIS_RS_COLOR_CONTROL_RED ) ;
    PlaceControl( BiDiMode, imgColor, lblRed, HGAP, LABEL_WIDTH ) ;
    lblRed.AutoSize := False ;

    edtGreen := TEdit.Create( Self ) ;
    edtGreen.Parent := Self ;
    edtGreen.Anchors := anchors ;
    edtGreen.Top := edtRed.Top + edtRed.Height + 6 ;
    PlaceControl( BiDiMode, imgColor, edtGreen, HGAP2, w ) ;
    edtGreen.AutoSize := False ;
    edtGreen.TabOrder := 6 ;
    edtGreen.OnChange := doEdtGreenChange ;

    lblGreen := TLabel.Create( Self ) ;
    lblGreen.Parent := Self ;
    lblGreen.Anchors := anchors ;
    lblGreen.Alignment := taRightJustify ;
    lblGreen.Caption := _rsrc( GIS_RS_COLOR_CONTROL_GREEN ) ;
    PlaceControl( BiDiMode, imgColor, lblGreen, HGAP, LABEL_WIDTH ) ;
    lblGreen.AutoSize := False ;

    edtBlue := TEdit.Create( Self ) ;
    edtBlue.Parent := Self ;
    edtBlue.Anchors := anchors ;
    edtBlue.Top := edtGreen.Top + edtGreen.Height + 6 ;
    PlaceControl( BiDiMode, imgColor, edtBlue, HGAP2, w ) ;
    edtBlue.AutoSize := False ;
    edtBlue.TabOrder := 7 ;
    edtBlue.OnChange := doEdtBlueChange ;

    lblBlue := TLabel.Create( Self ) ;
    lblBlue.Parent := Self ;
    lblBlue.Anchors := anchors ;
    lblBlue.Alignment := taRightJustify ;
    lblBlue.Caption := _rsrc( GIS_RS_COLOR_CONTROL_BLUE ) ;
    PlaceControl( BiDiMode, imgColor, lblBlue, HGAP, LABEL_WIDTH ) ;
    lblBlue.AutoSize := False ;

    edtHue := TEdit.Create( Self ) ;
    edtHue.Parent := Self ;
    edtHue.Anchors := anchors ;
    edtHue.Top := edtAlpha.Top + edtAlpha.Height + 6 ;
    PlaceControl( BiDiMode, edtRed, edtHue, HGAP3, w ) ;
    edtHue.AutoSize := False ;
    edtHue.TabOrder := 8 ;
    edtHue.OnChange := doEdtHueChange ;

    lblHue := TLabel.Create( Self ) ;
    lblHue.Parent := Self ;
    lblHue.Anchors := anchors ;
    lblHue.Alignment := taRightJustify ;
    lblHue.Caption := _rsrc( GIS_RS_COLOR_CONTROL_HUE ) ;
    PlaceControl( BiDiMode, edtRed, lblHue, HGAP div 3 , LABEL2_WIDTH ) ;
    lblHue.AutoSize := False ;

    edtSat := TEdit.Create( Self ) ;
    edtSat.Parent := Self ;
    edtSat.Anchors := anchors ;
    edtSat.Top := edtRed.Top + edtRed.Height + 6 ;
    PlaceControl( BiDiMode, edtRed, edtSat, HGAP3, w ) ;
    edtSat.AutoSize := False ;
    edtSat.TabOrder := 9 ;
    edtSat.OnChange := doEdtSatChange ;

    lblSat := TLabel.Create( Self ) ;
    lblSat.Parent := Self ;
    lblSat.Anchors := anchors ;
    lblSat.Alignment := taRightJustify ;
    lblSat.Caption := _rsrc( GIS_RS_COLOR_CONTROL_SAT ) ;
    PlaceControl( BiDiMode, edtRed, lblSat, HGAP div 3, LABEL2_WIDTH ) ;
    lblSat.AutoSize := False ;

    edtLig := TEdit.Create( Self ) ;
    edtLig.Parent := Self ;
    edtLig.Anchors := anchors ;
    edtLig.Top := edtGreen.Top + edtGreen.Height + 6 ;
    PlaceControl( BiDiMode, edtRed, edtLig, HGAP3, w ) ;
    edtLig.AutoSize := False ;
    edtLig.TabOrder := 10 ;
    edtLig.OnChange := doEdtLigChange ;

    lblLig := TLabel.Create( Self ) ;
    lblLig.Parent := Self ;
    lblLig.Anchors := anchors ;
    lblLig.Alignment := taRightJustify ;
    lblLig.Caption := _rsrc( GIS_RS_COLOR_CONTROL_LUM ) ;
    PlaceControl( BiDiMode, edtRed, lblLig, HGAP div 3, LABEL2_WIDTH ) ;
    lblLig.AutoSize := False ;

    edtHex := TEdit.Create( Self ) ;
    edtHex.Parent := Self ;
    edtHex.Anchors := anchors ;
    edtHex.Top := edtLig.Top + edtLig.Height + 6 ;
    if BiDiMode = bdRightToLeft then
      PlaceControl( BiDiMode, imgColor, edtHex, HGAP2,
                    RightBottomCorner( edtBlue ).X - edtLig.Left )
    else
      PlaceControl( BiDiMode, imgColor, edtHex, HGAP2,
                    RightBottomCorner( edtLig ).X - edtBlue.Left ) ;
    edtHex.AutoSize := False ;
    edtHex.TabOrder := 11 ;
    edtHex.OnChange := doEdtHexChange ;

    lblHex := TLabel.Create( Self ) ;
    lblHex.Parent := Self ;
    lblHex.Anchors := anchors ;
    lblHex.Alignment := taRightJustify ;
    lblHex.Caption := _rsrc( GIS_RS_COLOR_CONTROL_HEX ) ;
    PlaceControl( BiDiMode, imgColor, lblHex, HGAP, LABEL_WIDTH ) ;
    lblHex.AutoSize := False ;

    if BiDiMode = bdRightToLeft then begin
      lblColor.Width := RightBottomCorner( lblHex ).X - edtHex.Left ;
      lblColor.Left := edtHex.Left ;
      cpvColor.Width := RightBottomCorner( lblHex ).X - edtHex.Left ;
      cpvColor.Left := lblColor.Left ;
    end else begin
      lblColor.Width := RightBottomCorner( edtHex ).X - lblHex.Left ;
      lblColor.Left := lblHex.Left ;
      cpvColor.Width := RightBottomCorner( edtHex ).X - lblHex.Left ;
      cpvColor.Left := lblColor.Left ;
    end;

    btnSimple := TRadioButton.Create( Self ) ;
    btnSimple.Parent := Self ;
    btnSimple.Anchors := anchors ;
    btnSimple.Top := lblColor.Top ;
    btnSimple.Height := 18 ;
    PlaceControl( BiDiMode, cpvColor, btnSimple, HGAP, 30 ) ;
    btnSimple.Caption := '1' ;
    btnSimple.Checked := True ;
    btnSimple.TabOrder := 12 ;
    btnSimple.OnClick := doBtnSimpleClick ;

    btnComplex := TRadioButton.Create( Self ) ;
    btnComplex.Parent := Self ;
    btnComplex.Anchors := anchors ;
    btnComplex.Top := btnSimple.Top ;
    btnComplex.Height := 18 ;
    PlaceControl( BiDiMode, btnSimple, btnComplex, 1, 30 ) ;
    btnComplex.Caption := '2' ;
    btnComplex.Checked := False ;
    btnComplex.TabOrder := 13 ;
    btnComplex.OnClick := doBtnComplexClick ;

    imgDefault := T_defaultColors.Create( Self ) ;
    imgDefault.Parent := Self ;
    imgDefault.Anchors := anchors ;
    imgDefault.Top := btnSimple.Top + btnSimple.Height + 4 ;
    imgDefault.Height := scbAlpha.Top + scbAlpha.Height - imgDefault.Top ;
    PlaceControl( BiDiMode, cpvColor, imgDefault, HGAP, 156 ) ;
    T_defaultColors( imgDefault ).OnColorClick := doImgDefaultClick ;
    imgDefault.Visible := False ;
    makeSimpleColors ;

  end ;

  procedure TGIS_ColorDialog.showForm ;
  begin
    width_hidden   := ppiFix(8) + imgColor.Width + ppiFix(HGAP2) +
                      edtBlue.Width + 3*ppiFix(HGAP) +
                      edtLig.Width + ppiFix(HGAP) ;
    width_expanded := width_hidden + imgDefault.Width + ppiFix(8) ;
    ClientWidth  := width_hidden ;
    ClientHeight := scbAlpha.Top + scbAlpha.Height + ppiFix(8) +
                    btnOK.Height + ppiFix(8) ;

    cpvColor.CellSize := ppiFix(8) ;
    AlignVertically( lblAlpha, edtAlpha ) ;
    AlignVertically( lblRed,   edtRed   ) ;
    AlignVertically( lblGreen, edtGreen ) ;
    AlignVertically( lblBlue,  edtBlue  ) ;
    AlignVertically( lblHue,   edtHue   ) ;
    AlignVertically( lblSat,   edtSat   ) ;
    AlignVertically( lblLig,   edtLig   ) ;
    AlignVertically( lblHex,   edtHex   ) ;
    T_defaultColors( imgDefault ).SetCellSize(
      ppiFix( 18 ), ppiFix( 18 )
    ) ;
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
    clr.ToHSL( h, s, l );

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

  procedure TGIS_ColorDialog.doBtnExpandClick(
    _sender : TObject
  ) ;
  begin
    if imgDefault.Visible then begin
      imgDefault.Visible := False ;
      if BiDiMode = bdRightToLeft then
        Self.Left := Self.Left + ( Self.ClientWidth - width_hidden ) ;
      Self.ClientWidth := width_hidden ;
      btnExpand.Caption := '>' ;
    end
    else begin
      imgDefault.Visible := True ;
      if BiDiMode = bdRightToLeft then
        Self.Left := Self.Left - ( width_expanded - Self.ClientWidth ) ;
      Self.ClientWidth := width_expanded ;
      btnExpand.Caption := '<' ;
    end ;
  end ;

  procedure TGIS_ColorDialog.doBtnSimpleClick(
    _sender : TObject
  ) ;
  begin
    makeSimpleColors ;
  end ;

  procedure TGIS_ColorDialog.afterPPIChanged ;
  begin
    width_hidden   := ppiFix(8) + imgColor.Width + ppiFix(HGAP2) +
                      edtHex.Width + ppiFix(HGAP) ;
    width_expanded := width_hidden + imgDefault.Width + ppiFix(8) ;

    cpvColor.CellSize := ppiFix(8) ;
    AlignVertically( lblAlpha, edtAlpha ) ;
    AlignVertically( lblRed,   edtRed   ) ;
    AlignVertically( lblGreen, edtGreen ) ;
    AlignVertically( lblBlue,  edtBlue  ) ;
    AlignVertically( lblHue,   edtHue   ) ;
    AlignVertically( lblSat,   edtSat   ) ;
    AlignVertically( lblLig,   edtLig   ) ;
    AlignVertically( lblHex,   edtHex   ) ;
    T_defaultColors( imgDefault ).SetCellSize(
      ppiFix( 18 ), ppiFix( 18 )
    ) ;
  end;

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
    clr :=  T_valueBar( scbAlpha ).Color ;
    cpvColor.Color := TGIS_Color.FromARGB( a, clr.R, clr.G, clr.B ) ;

    edtHex.Text   := Format( '%.8x', [cpvColor.Color.ARGB] ) ;
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
      edtHex.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtHex.Font.Color := TColor( $000000 ) ;

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
      edtAlpha.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtAlpha.Font.Color := TColor( $000000 ) ;

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
      edtRed.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtRed.Font.Color := TColor( $000000 ) ;

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
      edtGreen.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtGreen.Font.Color := TColor( $000000 ) ;

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
      edtBlue.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtBlue.Font.Color := TColor( $000000 ) ;

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
      edtHue.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtHue.Font.Color := TColor( $000000 ) ;

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
      edtSat.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtSat.Font.Color := TColor( $000000 ) ;

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
      edtLig.Font.Color := TColor( $0000FF ) ;
      exit ;
    end ;

    lock ;

    edtLig.Font.Color := TColor( $000000 ) ;

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

  procedure TGIS_ColorDialog.Resize ;
  begin
    inherited ;

    if not assigned( imgColor ) then exit ;

  end ;

  function TGIS_ColorDialog.Execute(
    const _color  : TGIS_Color ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    h : Double ;
    s : Double ;
    v : Double ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := assigned( pOnHelp ) ;

    lock ;
    try
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
    finally
      unlock ;
    end;

    Result := ShowModal ;
  end ;

  function TGIS_ColorDialog.Execute(
    const _color  : TGIS_Color
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _color, hlp ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ColorComboBox'}
  constructor TGIS_ColorComboBox.Create(_owner: TComponent);
  begin
    inherited;
    ItemHeight := 24 ;

    comboExt := TGIS_ComboBoxHelper.Create( Self, Self.Name + '_Color', 3 ) ;
    comboExt.ValueEvent  := doValueColor ;
    comboExt.RenderEvent := doRenderColor ;
    comboExt.CustomEvent := doCustomColor ;
    icomboExt := comboExt ;

    ShowHint := True ;
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
          _value, ConstructParamAstext( GIS_PARAMTXT_TYPE_ARGB, _value, '' )
        )
      ) ;
    end ;

  begin
    comboExt.BeginFill ;
    try
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
      res := FCustomEvent( self, _value ) ;
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

  procedure TGIS_ColorComboBox.doRenderColor(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  );
  var
    rect     : TRect;
    a,r,g,b  : Byte ;
    bckg     : TColor;
    s1,s2,s3 : String ;
    tf       : TTextFormat ;
    s       : string;

    procedure convert_color(
      const _color : String ;
      out   _a     : Byte ;
      out   _r     : Byte ;
      out   _g     : Byte ;
      out   _b     : Byte
    ) ;
    var
      cl  : TGIS_Color ;
      s1, s2, s3 : String ;
    begin
      SplitParamAsText( _color, s1, s2, s3 );

      Assert( s1 = GIS_PARAMTXT_TYPE_ARGB ) ;

      cl := TGIS_Color.FromARGB( Cardinal(StrToInt64( '$'+s2)) ) ;
      _a := cl.A ;
      _r := cl.R ;
      _g := cl.G ;
      _b := cl.B ;
    end ;

  begin
    if BiDiMode = bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    with Canvas do begin
      FillRect(_rect);

      bckg := Brush.Color;
      case _class of
        'c' : begin
                rect := _rect;
                InflateRect( rect, -1, -1 );
                InflateRect( rect, -1, -1 );
                if BiDiMode = bdRightToLeft then
                  rect.Left := rect.Right - ( rect.Bottom - rect.Top )
                else
                  rect.Right := rect.Bottom - rect.Top + rect.Left;
                convert_color( _value, a, r, g ,b );

                if odDisabled in _state then begin
                  Brush.Color := clGray ;
                  FillRect( rect );
                  Brush.Color := clGray;
                  FrameRect( rect );
                end
                else begin
                  draw_color( Canvas, TGIS_Color.FromARGB( a, r, g, b ), rect, 4, True ) ;
                  Brush.Color := _control.Canvas.Font.Color ;
                  FrameRect( rect );
                end ;

                Brush.Color := bckg ;
                if BiDiMode = bdRightToLeft then
                  _rect.Right := rect.Left - 9
                else
                  _rect.Left := rect.Right + 9 ;
                _rect.Top := _rect.Top + (_rect.Bottom - _rect.Top - TextHeight('$')) div 2 ;

                s := IntToHex( $FFFFFF and TGIS_Color.FromRGB( r, g, b ).ARGB, 6 ) ;
                TextRect( _rect, s, tf ) ;
              end;
        'f' : begin
                SplitParamAsText(_caption, s1, s2, s3 ) ;
                if IsStringEmpty(s2) then
                  s2 := s1 ;
                _rect.Right := _rect.Right - 4 ;
                _rect.Left := _rect.Left + 4 ;
                _rect.Top := _rect.Top + (_rect.Bottom - _rect.Top - TextHeight(s2)) div 2 ;
                TextRect( _rect, s2, tf ) ;
              end ;
        else begin
          _rect.Right := _rect.Right - 4 ;
          _rect.Left := _rect.Left + 4 ;
          _rect.Top := _rect.Top + (_rect.Bottom - _rect.Top - TextHeight(_caption)) div 2 ;
          TextRect( _rect, _caption, tf ) ;
        end ;
      end;
    end;
    Hint := _caption ;
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
{$ENDREGION}

{$REGION 'TGIS_ColorRampComboBox'}
  constructor TGIS_ColorRampComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    Style := csOwnerDrawFixed ;
    DoubleBuffered := not IsWin11 ;
    ParentDoubleBuffered := False ;
    OnDrawItem := doRampListDrawItem ;

    ShowHint := True ;
    bmpMap   := nil ;
    AIdx     := nil ;
    FMode    := TGIS_ColorMapMode.Continuous ;
    FColorSchemas := [ TGIS_ColorSchema.Diverging,
                       TGIS_ColorSchema.Miscellaneous,
                       TGIS_ColorSchema.Qualitative,
                       TGIS_ColorSchema.Sequential   ] ;
    iTextWidth := 0 ;
    iTextGap   := 0 ;
  end ;

  destructor TGIS_ColorRampComboBox.Destroy ;
  begin
    FreeObject( bmpMap ) ;

    inherited ;
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

  procedure TGIS_ColorRampComboBox.fset_Mode(
    const _mode : TGIS_ColorMapMode
  ) ;
  begin
    if _mode <> FMode then begin
      FMode := _mode ;
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

  procedure TGIS_ColorRampComboBox.doRampListDrawItem(
    Control : TWinControl ;
    Index   : Integer ;
    Rect    : TRect ;
    State   : TOwnerDrawState
  );
  var
    cnt  : Integer ;
    s    : TSize   ;
    r    : TRect   ;
    bmp  : TBitmap ;
    idx  : Integer ;
    str  : String  ;
    tf   : TTextFormat ;
    i    : Integer ;
  begin
    idx := AIdx[Index] ;
    cnt := length( GisColorRampList[idx].Map ) ;

    if ( cnt = 0 ) then exit  ;

    with (Control as TComboBox) do begin
      Canvas.FillRect( Rect ) ;

      if iTextGap = 0 then
        iTextGap := Canvas.TextWidth( ' ' ) ;

      if iTextWidth = 0 then
        for i := 0 to Items.Count -1 do
          iTextWidth := Max( iTextWidth,
                             Canvas.TextWidth( GisColorRampList[i].Name )
                           ) ;

      r := Rect ;
      r.Bottom := r.Bottom - 1 ;
      r.Top    := r.Top + 1 ;
      r.Right  := r.Right - iTextWidth - 2*iTextGap ;
      bmp := TBitmap( bmpMap[Index] ) ;
      Canvas.StretchDraw( r, bmp ) ;
      Canvas.Brush.Style := bsClear ;

      if BIDiMode = bdRightToLeft then begin
        tf := [tfRtlReading,tfRight] ;
      end
      else begin
        tf := [] ;
      end ;

      r := Rect ;
      r.Left  := r.Right - iTextWidth - iTextGap ;
      r.Top   := r.Top + ( Rect.Height - Canvas.TextHeight( 'Ay' ) ) div 2 ;
      r.Right := r.Right - iTextGap ;
      str := GisColorRampList[idx].Name ;
      Canvas.TextRect( r, str, tf ) ;

      if odFocused in State then
        Canvas.DrawFocusRect( Rect ) ;
    end ;
  end ;

  function TGIS_ColorRampComboBox.toGisColor(
    const _color : Integer
  ) : TGIS_Color ;
  var
    br,bg,bb : Byte ;
  begin
    br := GetRValue(_color) ;
    bg := GetGValue(_color) ;
    bb := GetBValue(_color) ;
    Result := TGIS_Color.FromRGB( br, bg, bb ) ;
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

         _canvas.Pen.Color := RGB( r,g,b ) ;
         _canvas.MoveTo( X, _rect.Top ) ;
         _canvas.LineTo( X, _rect.Bottom ) ;
         inc( cnt ) ;
       end ;
     end
     else begin
       _canvas.Brush.Color := _fromColor ;
       _canvas.FillRect( _rect ) ;
     end;
   end ;

  procedure TGIS_ColorRampComboBox.Fill ;
  var
    i_ramp         : Integer ;
    i_color        : Integer ;
    bmp            : TBitmap ;
    rect           : TRect ;
    ramps_count    : Integer ;
    col_map_arr    : TGIS_ColorMapArray ;
    left_colormap  : TGIS_ColorMap ;
    right_colormap : TGIS_ColorMap ;
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
      bmp.Width  := Width ;
      bmp.Height := 1 ;

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

      bmpMap.Add( bmp ) ;

      if ( i_ramp mod 20 ) = 0 then
        Application.ProcessMessages ;
    end ;

    SetLength( AIdx, bmpMap.Count ) ;
    Items.BeginUpdate ;
    try
      Items.Clear ;
      for i_ramp := 0 to ramps_count - 1 do begin
        if not (GisColorRampList[i_ramp].MapType in FColorSchemas) then continue ;
        i_color := Items.Add( GisColorRampList[i_ramp].Name ) ;
        AIdx[i_color] := i_ramp ;
      end ;
    finally
      Items.EndUpdate ;
    end ;
  end ;

  function TGIS_ColorRampComboBox.Value(
    const _subClass : Integer = 0
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


