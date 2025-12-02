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
  Legend component. Vector dialog - bitmap selector.
}

{$IFDEF DCC}
  unit PVL.GisControlColor ;
  {$HPPEMIT '#pragma link "PVL.GisControlColor"'}
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
    TatukGIS.NDK,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Classes,
    System.SysUtils,
    System.StrUtils,
    System.Math,

    GisClasses,
    GisTypes,
    GisTypesUI,
    GisParams,
    PVL.GisPvl,
    PVL.GisPvlForms;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    java.awt.*,
    javax.swing.*,
    java.beans.*,
    tatukgis.jdk.*,
    tatukgis.rtl ;
  {$ENDIF}

type
  /// <summary>
  ///   Visual form for managing bitmaps.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlColor = class( TGIS_PvlModalForm )
  const
    PREVIEW_SIZE : Integer = {$IFNDEF GIS_MOBILE_DIALOGS}17{$ELSE}22{$ENDIF} ;
  private
    bLock           : Boolean ;
    prevType        : String ;
    arrPalette      : array of array of TGIS_PvlColorPreview ;
    cColorWheel     : TGIS_PvlColorWheel ;
    lblColorBar     : TGIS_PvlLabel ;
    cColorBar       : TGIS_PvlColorBar ;
    lblAlphaBar     : TGIS_PvlLabel ;
    cAlphaBar       : TGIS_PvlColorBar ;
    lblPrevNewColor : TGIS_PvlLabel ;
    prevNewColor    : TGIS_PvlColorPreview ;
    lblPrevCurrColor: TGIS_PvlLabel ;
    prevCurrColor   : TGIS_PvlColorPreview ;
    lblHex          : TGIS_PvlLabel ;
    edtHex          : TGIS_PvlEdit ;
    lblCmbType      : TGIS_PvlLabel ;
    cmbType         : TGIS_PvlComboBox ;
    pRgb            : TGIS_PvlPanel ;
    lblAlpha        : TGIS_PvlLabel ;
    edtAlpha        : TGIS_PvlEdit ;
    lblRed          : TGIS_PvlLabel ;
    edtRed          : TGIS_PvlEdit ;
    lblBlue         : TGIS_PvlLabel ;
    edtBlue         : TGIS_PvlEdit ;
    lblGreen        : TGIS_PvlLabel ;
    edtGreen        : TGIS_PvlEdit ;
    lblHue          : TGIS_PvlLabel ;
    edtHue          : TGIS_PvlEdit ;
    lblSat          : TGIS_PvlLabel ;
    edtSat          : TGIS_PvlEdit ;
    pHsl            : TGIS_PvlPanel ;
    lblLig          : TGIS_PvlLabel ;
    edtLig          : TGIS_PvlEdit ;
    pHsv            : TGIS_PvlPanel ;
    lblVal          : TGIS_PvlLabel ;
    edtVal          : TGIS_PvlEdit ;
    pCmyk           : TGIS_PvlPanel ;
    lblCyan         : TGIS_PvlLabel ;
    edtCyan         : TGIS_PvlEdit ;
    lblMagenta      : TGIS_PvlLabel ;
    edtMagenta      : TGIS_PvlEdit ;
    lblYellow       : TGIS_PvlLabel ;
    edtYellow       : TGIS_PvlEdit ;
    lblKey          : TGIS_PvlLabel ;
    edtKey          : TGIS_PvlEdit ;
    pHcl            : TGIS_PvlPanel ;
    lblChroma       : TGIS_PvlLabel ;
    edtChroma       : TGIS_PvlEdit ;
    lblLum          : TGIS_PvlLabel ;
    edtLum          : TGIS_PvlEdit ;
    cmbPalette      : TGIS_PvlComboBox ;
    pPalette        : TGIS_PvlPanel ;
    iHeight         : Integer ;
    iMaxHeight      : Integer ;
    iWidth          : Integer ;
    iMaxWidth       : Integer ;

  public
    /// <inheritdoc/>
    procedure DoAfterCreate      ; override;

    /// <inheritdoc/>
    procedure DoInitForm         ; override;

    /// <inheritdoc/>
    procedure DoInitControls     ; override;

    /// <inheritdoc/>
    procedure DoRedraw           ; override;

  private
    procedure fset_Color          ( const _value : TGIS_Color
                                  ) ;
    function  fget_Color          : TGIS_Color ;
    procedure lock                ;
    procedure unlock              ;
    function  isLocked            : Boolean ;
    procedure rgb2hsl             ;
    procedure rgb2hsv             ;
    procedure rgb2cmyk            ;
    procedure rgb2hcl             ;
    function  checkInteger        ( const _str : String  ;
                                    const _max : Integer ;
                                    out   _val : Integer
                                  ) : Boolean ;
    function  checkDouble         ( const _str : String  ;
                                    const _max : Double ;
                                    out   _val : Double
                                  ) : Boolean ;
    procedure calculateColors     ;
    procedure assembleARGB        ;
    procedure assembleHSL         ;
    procedure assembleHSV         ;
    procedure assembleHCL         ;
    procedure assembleCMYK        ;
    procedure createPalette       ( const _columns : Integer ;
                                    const _rows    : Integer
                                  ) ;
    procedure createStandard      ;
    procedure createSequential_1  ;
    procedure createSequential_2  ;
    procedure createSequential_3  ;
    procedure createDiverging     ;
    procedure createQualitative   ;
    procedure createColorBlindFriendly;

  private
    procedure typeChange         ( sender : TObject ) ;
    procedure paletteChange      ( sender : TObject ) ;
    procedure alphaChange        ( sender : TObject ) ;
    procedure rgbChange          ( sender : TObject ) ;
    procedure wheelChange        ( sender : TObject ) ;
    procedure colorBarChange     ( sender : TObject ) ;
    procedure alphaBarChange     ( sender : TObject ) ;
    procedure hueChange          ( sender : TObject ) ;
    procedure satChange          ( sender : TObject ) ;
    procedure ligChange          ( sender : TObject ) ;
    procedure valChange          ( sender : TObject ) ;
    procedure chromaChange       ( sender : TObject ) ;
    procedure lumChange          ( sender : TObject ) ;
    procedure hexChange          ( sender : TObject ) ;
    procedure previewClick       ( sender : TObject ) ;
    procedure cmykChange         ( sender : TObject ) ;

  public
    /// <summary>
    ///   Chosen color.
    /// </summary>
    property Color     : TGIS_Color
                         read  fget_Color
                         write fset_Color ;

  public
    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute   : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute   ( const _proc   : TGIS_Proc
                       ) : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_color">
    ///   Color property
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result,
    /// </returns>
    function Execute   ( const _color  : TGIS_Color ;
                         const _proc   : TGIS_Proc
                       ) : TGIS_PvlModalResult; overload;


    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_color">
    ///   Color property
    /// </param>
    /// <returns>
    ///   Modal result,
    /// </returns>
    function Execute   ( const _color  : TGIS_Color
                       ) : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog on a bitmap given by path.
    /// </summary>
    /// <param name="_color">
    ///   Color property
    /// </param>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result
    /// </returns>
    function Execute   ( const _color  : TGIS_Color     ;
                         const _onhelp : TGIS_HelpEvent ;
                         const _proc   : TGIS_Proc
                       ) : TGIS_PvlModalResult; overload;

  protected
    /// <inheritdoc/>
    procedure doDestroy ; override ;
  end;

//##############################################################################
implementation
{$IFDEF DCC}
  uses
    GisRtl,
    GisResource;
{$ENDIF}

//==============================================================================
// Private events & methods
//==============================================================================

  procedure TGIS_ControlColor.rgb2hsl ;
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

  procedure TGIS_ControlColor.rgb2hsv ;
  var
    r : Byte ;
    g : Byte ;
    b : Byte ;
    clr : TGIS_Color ;
    h : Double ;
    s : Double ;
    v2: Double ;
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
    clr.ToHSV( h, s, v2 );

    edtHue.Text := IntToStr( RoundS( 360*h ) ) ;
    edtSat.Text := IntToStr( RoundS( 100*s ) ) ;
    edtVal.Text := IntToStr( RoundS( 100*v2 ) ) ;
  end ;

  procedure TGIS_ControlColor.rgb2cmyk ;
  var
    r : Byte ;
    g : Byte ;
    b : Byte ;
    clr : TGIS_Color ;
    c : Double ;
    m : Double ;
    y : Double ;
    k : Double ;
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
    clr.ToCMYK( c, m, y, k ) ;

    edtCyan.Text := IntToStr( RoundS( c * 100 ) ) ;
    edtMagenta.Text := IntToStr( RoundS( m * 100 ) ) ;
    edtYellow.Text := IntToStr( RoundS( y * 100 ) ) ;
    edtKey.Text := IntToStr( RoundS( k * 100 ) ) ;
  end;

  procedure TGIS_ControlColor.rgb2hcl ;
  var
    r : Byte ;
    g : Byte ;
    b : Byte ;
    clr : TGIS_Color ;
    h : Double ;
    c : Double ;
    l: Double ;
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
    clr.ToHCL( h, c, l ) ;

    edtHue.Text := IntToStr( RoundS( h * 360 ) ) ;
    edtChroma.Text := IntToStr( RoundS( c * 100 ) ) ;
    edtLum.Text := IntToStr( RoundS( l * 100 ) ) ;
  end;

  function TGIS_ControlColor.checkInteger(
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

  function TGIS_ControlColor.checkDouble(
    const _str : String  ;
    const _max : Double  ;
    out   _val : Double
  ) : Boolean ;
  var
    v : Double ;
    s : String ;
  begin
    Result := False ;

    _val := 0 ;
    s :=  DotFloatToStr( DotStrToFloat( _str ) ) ;

    if not TryStrToFloat( s, v ) then
      exit ;

    if ( v < 0 ) or ( v > _max ) then
      exit ;

    _val := v ;

    Result := True ;
  end ;

  procedure TGIS_ControlColor.calculateColors ;
  begin
    rgb2hsl ;
    rgb2hsv ;
    rgb2cmyk ;
    rgb2hcl ;
  end;

  procedure TGIS_ControlColor.assembleARGB ;
  var
    a : Integer ;
    r : Integer ;
    g : Integer ;
    b : Integer ;
    h : Double ;
    s : Double ;
    v : Double ;
    v1 : Integer ;
    v2 : Double ;
  begin
    a := 255 ;

    if not ( checkInteger( edtRed.Text  , 255, r ) and
             checkInteger( edtGreen.Text, 255, g ) and
             checkInteger( edtBlue.Text , 255, b )
           ) then
      exit ;

    case cmbType.ItemIndex of
      // RGB
      0 : begin
        if not checkInteger( edtAlpha.Text, 255, v1 ) then
          exit ;
        a := v1 ;
      end;
      // HSL or HSV or CMYK or HCL
      1, 2, 3, 4 : begin
        if not checkDouble( edtAlpha.Text, 100, v2 ) then
          exit ;
        a := RoundS( v2 * 255 / 100 ) ;
      end;
    end;

    prevNewColor.Color := TGIS_Color.FromARGB(
      Byte( a ), Byte( r ), Byte( g ), Byte( b )
    ) ;
    prevNewColor.Color.ToHSV( h, s, v ) ;

    cColorWheel.Color := prevNewColor.Color ;
    cColorBar.Color := cColorWheel.Color ;
    cColorBar.Value := v ;
    cAlphaBar.Color := prevNewColor.Color ;
    cAlphaBar.Value := a/255 ;
  end ;

  procedure TGIS_ControlColor.assembleHSL ;
  var
    clr : TGIS_PvlRGBVal ;
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
    if not ( checkInteger( edtHue.Text, 360, hi ) and
             checkInteger( edtSat.Text, 100, si ) and
             checkInteger( edtLig.Text, 100, li )
           ) then
      exit ;

    clr := cColorWheel.HueToRGB( 1.0*hi ) ;
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
  end ;

  procedure TGIS_ControlColor.assembleHSV ;
  var
    clr : TGIS_PvlRGBVal ;
    hi : Integer ;
    si : Integer ;
    va : Integer ;
    v : Double ;
    r : Double ;
    g : Double ;
    b : Double ;
  begin
    if not ( checkInteger( edtHue.Text, 360, hi ) and
             checkInteger( edtSat.Text, 100, si ) and
             checkInteger( edtVal.Text, 100, va )
           ) then
      exit ;

    clr := cColorWheel.HueToRGB( 1.0*hi ) ;
    v := va/100.0 ;

    r := v*clr.R ;
    g := v*clr.G ;
    b := v*clr.B ;

    edtRed.Text   := IntToStr( RoundS( r ) ) ;
    edtGreen.Text := IntToStr( RoundS( g ) ) ;
    edtBlue.Text  := IntToStr( RoundS( b ) ) ;

    assembleARGB ;
  end ;

  procedure TGIS_ControlColor.assembleHCL;
  var
    clr : TGIS_Color ;
    hi : Integer ;
    ci : Integer ;
    li : Integer ;
  begin
    if not ( checkInteger( edtHue.Text, 360, hi ) and
             checkInteger( edtChroma.Text, 134, ci ) and
             checkInteger( edtLum.Text, 100, li ) ) then
      exit ;

    clr := TGIS_Color.FromHCL( hi/360, ci/100, li/100 ) ;

    edtRed.Text := IntToStr( clr.R ) ;
    edtGreen.Text := IntToStr( clr.G ) ;
    edtBlue.Text := IntToStr( clr.B ) ;

    assembleARGB ;
  end;

  procedure TGIS_ControlColor.assembleCMYK ;
  var
    clr : TGIS_Color ;
    ci : Integer ;
    mi : Integer ;
    yi : Integer ;
    ki : Integer ;
    c, m, y, k  : Single ;
    r, g, b : Integer ;
  begin
    if not ( checkInteger( edtCyan.Text, 100, ci ) and
             checkInteger( edtMagenta.Text, 100, mi ) and
             checkInteger( edtYellow.Text, 100, yi ) and
             checkInteger( edtKey.Text, 100, ki )
           ) then
      exit ;

    c := ci / 100.0 ;
    m := mi / 100.0 ;
    y := yi / 100.0 ;
    k := ki / 100.0 ;

    clr := TGIS_Color.FromCMYK( c, m, y, k ) ;

    r := clr.R ;
    g := clr.G ;
    b := clr.B ;

    edtRed.Text   := IntToStr( RoundS( r ) ) ;
    edtGreen.Text := IntToStr( RoundS( g ) ) ;
    edtBlue.Text  := IntToStr( RoundS( b ) ) ;

    assembleARGB ;
  end ;

  procedure TGIS_ControlColor.createPalette(
    const _columns: Integer;
    const _rows: Integer
  ) ;
  var
    i, j : Integer ;
  begin
    LockWindow ;
    try
      pPalette.RemoveAllComponents ;

      SetLength( arrPalette, _columns, _rows ) ;

      for i := 0 to _columns - 1 do begin
        for j := 0 to _rows - 1 do begin
          arrPalette[i][j] := TGIS_PvlColorPreview.Create( pPalette.Context )  ;
          arrPalette[i][j].Border := True ;
          arrPalette[i][j].Color := TGIS_Color.None ;

          {$IFDEF DCC}
          arrPalette[i][j].OnClick := previewClick ;
          {$ELSE}
          arrPalette[i][j].OnClick := @previewClick ;
          {$ENDIF}

          if ( i = 0 ) and ( j = 0 ) then
            arrPalette[i][j].Place( PREVIEW_SIZE, PREVIEW_SIZE, nil, 0, nil, 0 )
          else if i = 0 then
            arrPalette[i][j].Place( PREVIEW_SIZE, PREVIEW_SIZE, nil, 0, arrPalette[i, j-1], 0 )
          else if j = 0 then
            arrPalette[i][j].Place( PREVIEW_SIZE, PREVIEW_SIZE, arrPalette[i-1, j], RoundS( pPalette.Context.VMargin / 2 / pPalette.Context.PPIFix ), nil, 0 )
          else
            arrPalette[i][j].Place( PREVIEW_SIZE, PREVIEW_SIZE, arrPalette[i-1, j], RoundS( pPalette.Context.VMargin / 2 / pPalette.Context.PPIFix ), arrPalette[i, j-1], 0 ) ;
        end;
      end;
    finally
      UnlockWindow ;
    end;
  end;

  procedure TGIS_ControlColor.createStandard ;
  begin
    createPalette( 11, 9 ) ;

    // Red
    arrPalette[0][0].Color := TGIS_Color.FromRGB( $FFCDD2 ) ;
    arrPalette[0][1].Color := TGIS_Color.FromRGB( $EF9A9A ) ;
    arrPalette[0][2].Color := TGIS_Color.FromRGB( $E57373 ) ;
    arrPalette[0][3].Color := TGIS_Color.FromRGB( $EF5350 ) ;
    arrPalette[0][4].Color := TGIS_Color.FromRGB( $F44336 ) ;
    arrPalette[0][5].Color := TGIS_Color.FromRGB( $E53935 ) ;
    arrPalette[0][6].Color := TGIS_Color.FromRGB( $D32F2F ) ;
    arrPalette[0][7].Color := TGIS_Color.FromRGB( $C62828 ) ;
    arrPalette[0][8].Color := TGIS_Color.FromRGB( $B71C1C ) ;

    // Purple
    arrPalette[1][0].Color := TGIS_Color.FromRGB( $E1BEE7 ) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB( $CE93D8 ) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB( $BA68C8 ) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB( $AB47BC ) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB( $9C27B0 ) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB( $8E24AA ) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB( $7B1FA2 ) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB( $6A1B9A ) ;
    arrPalette[1][8].Color := TGIS_Color.FromRGB( $4A148C ) ;

    // Indigo
    arrPalette[2][0].Color := TGIS_Color.FromRGB( $C5CAE9 ) ;
    arrPalette[2][1].Color := TGIS_Color.FromRGB( $9FA8DA ) ;
    arrPalette[2][2].Color := TGIS_Color.FromRGB( $7986CB ) ;
    arrPalette[2][3].Color := TGIS_Color.FromRGB( $5C6BC0 ) ;
    arrPalette[2][4].Color := TGIS_Color.FromRGB( $3F51B5 ) ;
    arrPalette[2][5].Color := TGIS_Color.FromRGB( $3949AB ) ;
    arrPalette[2][6].Color := TGIS_Color.FromRGB( $303F9F ) ;
    arrPalette[2][7].Color := TGIS_Color.FromRGB( $283593 ) ;
    arrPalette[2][8].Color := TGIS_Color.FromRGB( $1A237E ) ;

    // Blue
    arrPalette[3][0].Color := TGIS_Color.FromRGB( $BBDEFB ) ;
    arrPalette[3][1].Color := TGIS_Color.FromRGB( $90CAF9 ) ;
    arrPalette[3][2].Color := TGIS_Color.FromRGB( $64B5F6 ) ;
    arrPalette[3][3].Color := TGIS_Color.FromRGB( $42A5F5 ) ;
    arrPalette[3][4].Color := TGIS_Color.FromRGB( $2196F3 ) ;
    arrPalette[3][5].Color := TGIS_Color.FromRGB( $1E88E5 ) ;
    arrPalette[3][6].Color := TGIS_Color.FromRGB( $1976D2 ) ;
    arrPalette[3][7].Color := TGIS_Color.FromRGB( $1565C0 ) ;
    arrPalette[3][8].Color := TGIS_Color.FromRGB( $0D47A1 ) ;

    // Cyan
    arrPalette[4][0].Color := TGIS_Color.FromRGB( $B2EBF2 ) ;
    arrPalette[4][1].Color := TGIS_Color.FromRGB( $80DEEA ) ;
    arrPalette[4][2].Color := TGIS_Color.FromRGB( $4DD0E1 ) ;
    arrPalette[4][3].Color := TGIS_Color.FromRGB( $26C6DA ) ;
    arrPalette[4][4].Color := TGIS_Color.FromRGB( $00BCD4 ) ;
    arrPalette[4][5].Color := TGIS_Color.FromRGB( $00ACC1 ) ;
    arrPalette[4][6].Color := TGIS_Color.FromRGB( $0097A7 ) ;
    arrPalette[4][7].Color := TGIS_Color.FromRGB( $00838F ) ;
    arrPalette[4][8].Color := TGIS_Color.FromRGB( $006064 ) ;

    // Green
    arrPalette[5][0].Color := TGIS_Color.FromRGB( $C8E6C9 ) ;
    arrPalette[5][1].Color := TGIS_Color.FromRGB( $A5D6A7 ) ;
    arrPalette[5][2].Color := TGIS_Color.FromRGB( $81C784 ) ;
    arrPalette[5][3].Color := TGIS_Color.FromRGB( $66BB6A ) ;
    arrPalette[5][4].Color := TGIS_Color.FromRGB( $4CAF50 ) ;
    arrPalette[5][5].Color := TGIS_Color.FromRGB( $43A047 ) ;
    arrPalette[5][6].Color := TGIS_Color.FromRGB( $388E3C ) ;
    arrPalette[5][7].Color := TGIS_Color.FromRGB( $2E7D32 ) ;
    arrPalette[5][8].Color := TGIS_Color.FromRGB( $1B5E20 ) ;

    // Light Green
    arrPalette[6][0].Color := TGIS_Color.FromRGB( $DCEDC8 ) ;
    arrPalette[6][1].Color := TGIS_Color.FromRGB( $C5E1A5 ) ;
    arrPalette[6][2].Color := TGIS_Color.FromRGB( $AED581 ) ;
    arrPalette[6][3].Color := TGIS_Color.FromRGB( $9CCC65 ) ;
    arrPalette[6][4].Color := TGIS_Color.FromRGB( $8BC34A ) ;
    arrPalette[6][5].Color := TGIS_Color.FromRGB( $7CB342 ) ;
    arrPalette[6][6].Color := TGIS_Color.FromRGB( $689F38 ) ;
    arrPalette[6][7].Color := TGIS_Color.FromRGB( $558B2F ) ;
    arrPalette[6][8].Color := TGIS_Color.FromRGB( $33691E ) ;

    // Lime
    arrPalette[7][0].Color := TGIS_Color.FromRGB( $F0F4C3 ) ;
    arrPalette[7][1].Color := TGIS_Color.FromRGB( $E6EE9C ) ;
    arrPalette[7][2].Color := TGIS_Color.FromRGB( $DCE775 ) ;
    arrPalette[7][3].Color := TGIS_Color.FromRGB( $D4E157 ) ;
    arrPalette[7][4].Color := TGIS_Color.FromRGB( $CDDC39 ) ;
    arrPalette[7][5].Color := TGIS_Color.FromRGB( $C0CA33 ) ;
    arrPalette[7][6].Color := TGIS_Color.FromRGB( $AFB42B ) ;
    arrPalette[7][7].Color := TGIS_Color.FromRGB( $9E9D24 ) ;
    arrPalette[7][8].Color := TGIS_Color.FromRGB( $827717 ) ;

    // Yellow
    arrPalette[8][0].Color := TGIS_Color.FromRGB( $FFF9C4 ) ;
    arrPalette[8][1].Color := TGIS_Color.FromRGB( $FFF59D ) ;
    arrPalette[8][2].Color := TGIS_Color.FromRGB( $FFF176 ) ;
    arrPalette[8][3].Color := TGIS_Color.FromRGB( $FFEE58 ) ;
    arrPalette[8][4].Color := TGIS_Color.FromRGB( $FFEB3B ) ;
    arrPalette[8][5].Color := TGIS_Color.FromRGB( $FDD835 ) ;
    arrPalette[8][6].Color := TGIS_Color.FromRGB( $FBC02D ) ;
    arrPalette[8][7].Color := TGIS_Color.FromRGB( $F9A825 ) ;
    arrPalette[8][8].Color := TGIS_Color.FromRGB( $F57F17 ) ;

    // Amber
    arrPalette[9][0].Color := TGIS_Color.FromRGB( $FFECB3 ) ;
    arrPalette[9][1].Color := TGIS_Color.FromRGB( $FFE082 ) ;
    arrPalette[9][2].Color := TGIS_Color.FromRGB( $FFD54F ) ;
    arrPalette[9][3].Color := TGIS_Color.FromRGB( $FFCA28 ) ;
    arrPalette[9][4].Color := TGIS_Color.FromRGB( $FFC107 ) ;
    arrPalette[9][5].Color := TGIS_Color.FromRGB( $FFB300 ) ;
    arrPalette[9][6].Color := TGIS_Color.FromRGB( $FFA000 ) ;
    arrPalette[9][7].Color := TGIS_Color.FromRGB( $FF8F00 ) ;
    arrPalette[9][8].Color := TGIS_Color.FromRGB( $FF6F00 ) ;

    // Black
    arrPalette[10][0].Color := TGIS_Color.FromRGB( $F5F5F5 ) ;
    arrPalette[10][1].Color := TGIS_Color.FromRGB( $EEEEEE ) ;
    arrPalette[10][2].Color := TGIS_Color.FromRGB( $E0E0E0 ) ;
    arrPalette[10][3].Color := TGIS_Color.FromRGB( $BDBDBD ) ;
    arrPalette[10][4].Color := TGIS_Color.FromRGB( $9E9E9E ) ;
    arrPalette[10][5].Color := TGIS_Color.FromRGB( $757575 ) ;
    arrPalette[10][6].Color := TGIS_Color.FromRGB( $616161 ) ;
    arrPalette[10][7].Color := TGIS_Color.FromRGB( $424242 ) ;
    arrPalette[10][8].Color := TGIS_Color.FromRGB( $212121 ) ;
  end;

  procedure TGIS_ControlColor.createSequential_1 ;
  begin
    createPalette( 6, 9 ) ;

    // Blues
    arrPalette[0][0].Color := TGIS_Color.FromRGB(247,251,255) ;
    arrPalette[0][1].Color := TGIS_Color.FromRGB(222,235,247) ;
    arrPalette[0][2].Color := TGIS_Color.FromRGB(198,219,239) ;
    arrPalette[0][3].Color := TGIS_Color.FromRGB(158,202,225) ;
    arrPalette[0][4].Color := TGIS_Color.FromRGB(107,174,214) ;
    arrPalette[0][5].Color := TGIS_Color.FromRGB(66,146,198) ;
    arrPalette[0][6].Color := TGIS_Color.FromRGB(33,113,181) ;
    arrPalette[0][7].Color := TGIS_Color.FromRGB(8,81,156) ;
    arrPalette[0][8].Color := TGIS_Color.FromRGB(8,48,107) ;

    // Greens
    arrPalette[1][0].Color := TGIS_Color.FromRGB(247,252,245) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB(229,245,224) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB(199,233,192) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB(161,217,155) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB(116,196,118) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB(65,171,93) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB(35,139,69) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB(0,109,44) ;
    arrPalette[1][8].Color := TGIS_Color.FromRGB(0,68,27) ;

    // Greys
    arrPalette[2][0].Color := TGIS_Color.FromRGB(255,255,255) ;
    arrPalette[2][1].Color := TGIS_Color.FromRGB(240,240,240) ;
    arrPalette[2][2].Color := TGIS_Color.FromRGB(217,217,217) ;
    arrPalette[2][3].Color := TGIS_Color.FromRGB(189,189,189) ;
    arrPalette[2][4].Color := TGIS_Color.FromRGB(150,150,150) ;
    arrPalette[2][5].Color := TGIS_Color.FromRGB(115,115,115) ;
    arrPalette[2][6].Color := TGIS_Color.FromRGB(82,82,82) ;
    arrPalette[2][7].Color := TGIS_Color.FromRGB(37,37,37) ;
    arrPalette[2][8].Color := TGIS_Color.FromRGB(0,0,0) ;

    // Oranges
    arrPalette[3][0].Color := TGIS_Color.FromRGB(255,245,235) ;
    arrPalette[3][1].Color := TGIS_Color.FromRGB(254,230,206) ;
    arrPalette[3][2].Color := TGIS_Color.FromRGB(253,208,162) ;
    arrPalette[3][3].Color := TGIS_Color.FromRGB(253,174,107) ;
    arrPalette[3][4].Color := TGIS_Color.FromRGB(253,141,60) ;
    arrPalette[3][5].Color := TGIS_Color.FromRGB(241,105,19) ;
    arrPalette[3][6].Color := TGIS_Color.FromRGB(217,72,1) ;
    arrPalette[3][7].Color := TGIS_Color.FromRGB(166,54,3) ;
    arrPalette[3][8].Color := TGIS_Color.FromRGB(127,39,4) ;

    // Purples
    arrPalette[4][0].Color := TGIS_Color.FromRGB(252,251,253) ;
    arrPalette[4][1].Color := TGIS_Color.FromRGB(239,237,245) ;
    arrPalette[4][2].Color := TGIS_Color.FromRGB(218,218,235) ;
    arrPalette[4][3].Color := TGIS_Color.FromRGB(188,189,220) ;
    arrPalette[4][4].Color := TGIS_Color.FromRGB(158,154,200) ;
    arrPalette[4][5].Color := TGIS_Color.FromRGB(128,125,186) ;
    arrPalette[4][6].Color := TGIS_Color.FromRGB(106,81,163) ;
    arrPalette[4][7].Color := TGIS_Color.FromRGB(84,39,143) ;
    arrPalette[4][8].Color := TGIS_Color.FromRGB(63,0,125) ;

    // Reds
    arrPalette[5][0].Color := TGIS_Color.FromRGB(255,245,240) ;
    arrPalette[5][1].Color := TGIS_Color.FromRGB(254,224,210) ;
    arrPalette[5][2].Color := TGIS_Color.FromRGB(252,187,161) ;
    arrPalette[5][3].Color := TGIS_Color.FromRGB(252,146,114) ;
    arrPalette[5][4].Color := TGIS_Color.FromRGB(251,106,74) ;
    arrPalette[5][5].Color := TGIS_Color.FromRGB(239,59,44) ;
    arrPalette[5][6].Color := TGIS_Color.FromRGB(203,24,29) ;
    arrPalette[5][7].Color := TGIS_Color.FromRGB(165,15,21) ;
    arrPalette[5][8].Color := TGIS_Color.FromRGB(103,0,13) ;
  end;

  procedure TGIS_ControlColor.createSequential_2 ;
  begin
    createPalette( 8, 9 ) ;

    // BuGn
    arrPalette[0][0].Color := TGIS_Color.FromRGB(247,252,253) ;
    arrPalette[0][1].Color := TGIS_Color.FromRGB(229,245,249) ;
    arrPalette[0][2].Color := TGIS_Color.FromRGB(204,236,230) ;
    arrPalette[0][3].Color := TGIS_Color.FromRGB(153,216,201) ;
    arrPalette[0][4].Color := TGIS_Color.FromRGB(102,194,164) ;
    arrPalette[0][5].Color := TGIS_Color.FromRGB(65,174,118) ;
    arrPalette[0][6].Color := TGIS_Color.FromRGB(35,139,69) ;
    arrPalette[0][7].Color := TGIS_Color.FromRGB(0,109,44) ;
    arrPalette[0][8].Color := TGIS_Color.FromRGB(0,68,27) ;

    // BuPu
    arrPalette[1][0].Color := TGIS_Color.FromRGB(247,252,253) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB(224,236,244) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB(191,211,230) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB(158,188,218) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB(140,150,198) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB(140,107,177) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB(136,65,157) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB(129,15,124) ;
    arrPalette[1][8].Color := TGIS_Color.FromRGB(77,0,75) ;

    // GnBu
    arrPalette[2][0].Color := TGIS_Color.FromRGB(247,252,240) ;
    arrPalette[2][1].Color := TGIS_Color.FromRGB(224,243,219) ;
    arrPalette[2][2].Color := TGIS_Color.FromRGB(204,235,197) ;
    arrPalette[2][3].Color := TGIS_Color.FromRGB(168,221,181) ;
    arrPalette[2][4].Color := TGIS_Color.FromRGB(123,204,196) ;
    arrPalette[2][5].Color := TGIS_Color.FromRGB(78,179,211) ;
    arrPalette[2][6].Color := TGIS_Color.FromRGB(43,140,190) ;
    arrPalette[2][7].Color := TGIS_Color.FromRGB(8,104,172) ;
    arrPalette[2][8].Color := TGIS_Color.FromRGB(8,64,129) ;

    // OrRd
    arrPalette[3][0].Color := TGIS_Color.FromRGB(255,247,236) ;
    arrPalette[3][1].Color := TGIS_Color.FromRGB(254,232,200) ;
    arrPalette[3][2].Color := TGIS_Color.FromRGB(253,212,158) ;
    arrPalette[3][3].Color := TGIS_Color.FromRGB(253,187,132) ;
    arrPalette[3][4].Color := TGIS_Color.FromRGB(252,141,89) ;
    arrPalette[3][5].Color := TGIS_Color.FromRGB(239,101,72) ;
    arrPalette[3][6].Color := TGIS_Color.FromRGB(215,48,31) ;
    arrPalette[3][7].Color := TGIS_Color.FromRGB(179,0,0) ;
    arrPalette[3][8].Color := TGIS_Color.FromRGB(127,0,0) ;

    // PuBu
    arrPalette[4][0].Color := TGIS_Color.FromRGB(255,247,251) ;
    arrPalette[4][1].Color := TGIS_Color.FromRGB(236,231,242) ;
    arrPalette[4][2].Color := TGIS_Color.FromRGB(208,209,230) ;
    arrPalette[4][3].Color := TGIS_Color.FromRGB(166,189,219) ;
    arrPalette[4][4].Color := TGIS_Color.FromRGB(116,169,207) ;
    arrPalette[4][5].Color := TGIS_Color.FromRGB(54,144,192) ;
    arrPalette[4][6].Color := TGIS_Color.FromRGB(5,112,176) ;
    arrPalette[4][7].Color := TGIS_Color.FromRGB(4,90,141) ;
    arrPalette[4][8].Color := TGIS_Color.FromRGB(2,56,88) ;

    // PuRd
    arrPalette[5][0].Color := TGIS_Color.FromRGB(247,244,249) ;
    arrPalette[5][1].Color := TGIS_Color.FromRGB(231,225,239) ;
    arrPalette[5][2].Color := TGIS_Color.FromRGB(212,185,218) ;
    arrPalette[5][3].Color := TGIS_Color.FromRGB(201,148,199) ;
    arrPalette[5][4].Color := TGIS_Color.FromRGB(223,101,176) ;
    arrPalette[5][5].Color := TGIS_Color.FromRGB(231,41,138) ;
    arrPalette[5][6].Color := TGIS_Color.FromRGB(206,18,86) ;
    arrPalette[5][7].Color := TGIS_Color.FromRGB(152,0,67) ;
    arrPalette[5][8].Color := TGIS_Color.FromRGB(103,0,31) ;

    // RdPu
    arrPalette[6][0].Color := TGIS_Color.FromRGB(255,247,243) ;
    arrPalette[6][1].Color := TGIS_Color.FromRGB(253,224,221) ;
    arrPalette[6][2].Color := TGIS_Color.FromRGB(252,197,192) ;
    arrPalette[6][3].Color := TGIS_Color.FromRGB(250,159,181) ;
    arrPalette[6][4].Color := TGIS_Color.FromRGB(247,104,161) ;
    arrPalette[6][5].Color := TGIS_Color.FromRGB(221,52,151) ;
    arrPalette[6][6].Color := TGIS_Color.FromRGB(174,1,126) ;
    arrPalette[6][7].Color := TGIS_Color.FromRGB(122,1,119) ;
    arrPalette[6][8].Color := TGIS_Color.FromRGB(73,0,106) ;

    // YlGn
    arrPalette[7][0].Color := TGIS_Color.FromRGB(255,255,229) ;
    arrPalette[7][1].Color := TGIS_Color.FromRGB(247,252,185) ;
    arrPalette[7][2].Color := TGIS_Color.FromRGB(217,240,163) ;
    arrPalette[7][3].Color := TGIS_Color.FromRGB(173,221,142) ;
    arrPalette[7][4].Color := TGIS_Color.FromRGB(120,198,121) ;
    arrPalette[7][5].Color := TGIS_Color.FromRGB(65,171,93) ;
    arrPalette[7][6].Color := TGIS_Color.FromRGB(35,132,67) ;
    arrPalette[7][7].Color := TGIS_Color.FromRGB(0,104,55) ;
    arrPalette[7][8].Color := TGIS_Color.FromRGB(0,69,41) ;
  end;

  procedure TGIS_ControlColor.createSequential_3 ;
  begin
    createPalette( 4, 9 ) ;

    // YlGnBu
    arrPalette[0][0].Color := TGIS_Color.FromRGB(255,255,217) ;
    arrPalette[0][1].Color := TGIS_Color.FromRGB(237,248,177) ;
    arrPalette[0][2].Color := TGIS_Color.FromRGB(199,233,180) ;
    arrPalette[0][3].Color := TGIS_Color.FromRGB(127,205,187) ;
    arrPalette[0][4].Color := TGIS_Color.FromRGB(65,182,196) ;
    arrPalette[0][5].Color := TGIS_Color.FromRGB(29,145,192) ;
    arrPalette[0][6].Color := TGIS_Color.FromRGB(34,94,168) ;
    arrPalette[0][7].Color := TGIS_Color.FromRGB(37,52,148) ;
    arrPalette[0][8].Color := TGIS_Color.FromRGB(8,29,88) ;

    // YlOrBr
    arrPalette[1][0].Color := TGIS_Color.FromRGB(255,255,229) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB(255,247,188) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB(254,227,145) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB(254,196,79) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB(254,153,41) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB(236,112,20) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB(204,76,2) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB(153,52,4) ;
    arrPalette[1][8].Color := TGIS_Color.FromRGB(102,37,6) ;

    // YlOrRd
    arrPalette[2][0].Color := TGIS_Color.FromRGB(255,255,204) ;
    arrPalette[2][1].Color := TGIS_Color.FromRGB(255,237,160) ;
    arrPalette[2][2].Color := TGIS_Color.FromRGB(254,217,118) ;
    arrPalette[2][3].Color := TGIS_Color.FromRGB(254,178,76) ;
    arrPalette[2][4].Color := TGIS_Color.FromRGB(253,141,60) ;
    arrPalette[2][5].Color := TGIS_Color.FromRGB(252,78,42) ;
    arrPalette[2][6].Color := TGIS_Color.FromRGB(227,26,28) ;
    arrPalette[2][7].Color := TGIS_Color.FromRGB(189,0,38) ;
    arrPalette[2][8].Color := TGIS_Color.FromRGB(128,0,38) ;

    // PuBuGn
    arrPalette[3][0].Color := TGIS_Color.FromRGB(255,247,251) ;
    arrPalette[3][1].Color := TGIS_Color.FromRGB(236,226,240) ;
    arrPalette[3][2].Color := TGIS_Color.FromRGB(208,209,230) ;
    arrPalette[3][3].Color := TGIS_Color.FromRGB(166,189,219) ;
    arrPalette[3][4].Color := TGIS_Color.FromRGB(103,169,207) ;
    arrPalette[3][5].Color := TGIS_Color.FromRGB(54,144,192) ;
    arrPalette[3][6].Color := TGIS_Color.FromRGB(2,129,138) ;
    arrPalette[3][7].Color := TGIS_Color.FromRGB(1,108,89) ;
    arrPalette[3][8].Color := TGIS_Color.FromRGB(1,70,54) ;
  end;

  procedure TGIS_ControlColor.createDiverging ;
  begin
    createPalette( 9, 9 ) ;

    // BrBG
    arrPalette[0][0].Color := TGIS_Color.FromRGB(140,81,10) ;
    arrPalette[0][1].Color := TGIS_Color.FromRGB(191,129,45) ;
    arrPalette[0][2].Color := TGIS_Color.FromRGB(223,194,125) ;
    arrPalette[0][3].Color := TGIS_Color.FromRGB(246,232,195) ;
    arrPalette[0][4].Color := TGIS_Color.FromRGB(245,245,245) ;
    arrPalette[0][5].Color := TGIS_Color.FromRGB(199,234,229) ;
    arrPalette[0][6].Color := TGIS_Color.FromRGB(128,205,193) ;
    arrPalette[0][7].Color := TGIS_Color.FromRGB(53,151,143) ;
    arrPalette[0][8].Color := TGIS_Color.FromRGB(1,102,94) ;

    // PiYG
    arrPalette[1][0].Color := TGIS_Color.FromRGB(197,27,125) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB(222,119,174) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB(241,182,218) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB(253,224,239) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB(247,247,247) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB(230,245,208) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB(184,225,134) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB(127,188,65) ;
    arrPalette[1][8].Color := TGIS_Color.FromRGB(77,146,33) ;

    // PRGn
    arrPalette[2][0].Color := TGIS_Color.FromRGB(118,42,131) ;
    arrPalette[2][1].Color := TGIS_Color.FromRGB(153,112,171) ;
    arrPalette[2][2].Color := TGIS_Color.FromRGB(194,165,207) ;
    arrPalette[2][3].Color := TGIS_Color.FromRGB(231,212,232) ;
    arrPalette[2][4].Color := TGIS_Color.FromRGB(247,247,247) ;
    arrPalette[2][5].Color := TGIS_Color.FromRGB(217,240,211) ;
    arrPalette[2][6].Color := TGIS_Color.FromRGB(166,219,160) ;
    arrPalette[2][7].Color := TGIS_Color.FromRGB(90,174,97) ;
    arrPalette[2][8].Color := TGIS_Color.FromRGB(27,120,55) ;

    // PuOr
    arrPalette[3][0].Color := TGIS_Color.FromRGB(179,88,6) ;
    arrPalette[3][1].Color := TGIS_Color.FromRGB(224,130,20) ;
    arrPalette[3][2].Color := TGIS_Color.FromRGB(253,184,99) ;
    arrPalette[3][3].Color := TGIS_Color.FromRGB(254,224,182) ;
    arrPalette[3][4].Color := TGIS_Color.FromRGB(247,247,247) ;
    arrPalette[3][5].Color := TGIS_Color.FromRGB(216,218,235) ;
    arrPalette[3][6].Color := TGIS_Color.FromRGB(178,171,210) ;
    arrPalette[3][7].Color := TGIS_Color.FromRGB(128,115,172) ;
    arrPalette[3][8].Color := TGIS_Color.FromRGB(84,39,136) ;

    // RdBu
    arrPalette[4][0].Color := TGIS_Color.FromRGB(178,24,43) ;
    arrPalette[4][1].Color := TGIS_Color.FromRGB(214,96,77) ;
    arrPalette[4][2].Color := TGIS_Color.FromRGB(244,165,130) ;
    arrPalette[4][3].Color := TGIS_Color.FromRGB(253,219,199) ;
    arrPalette[4][4].Color := TGIS_Color.FromRGB(247,247,247) ;
    arrPalette[4][5].Color := TGIS_Color.FromRGB(209,229,240) ;
    arrPalette[4][6].Color := TGIS_Color.FromRGB(146,197,222) ;
    arrPalette[4][7].Color := TGIS_Color.FromRGB(67,147,195) ;
    arrPalette[4][8].Color := TGIS_Color.FromRGB(33,102,172) ;

    // RdGy
    arrPalette[5][0].Color := TGIS_Color.FromRGB(178,24,43) ;
    arrPalette[5][1].Color := TGIS_Color.FromRGB(214,96,77) ;
    arrPalette[5][2].Color := TGIS_Color.FromRGB(244,165,130) ;
    arrPalette[5][3].Color := TGIS_Color.FromRGB(253,219,199) ;
    arrPalette[5][4].Color := TGIS_Color.FromRGB(255,255,255) ;
    arrPalette[5][5].Color := TGIS_Color.FromRGB(224,224,224) ;
    arrPalette[5][6].Color := TGIS_Color.FromRGB(186,186,186) ;
    arrPalette[5][7].Color := TGIS_Color.FromRGB(135,135,135) ;
    arrPalette[5][8].Color := TGIS_Color.FromRGB(77,77,77) ;

    // RdYlBu
    arrPalette[6][0].Color := TGIS_Color.FromRGB(215,48,39) ;
    arrPalette[6][1].Color := TGIS_Color.FromRGB(244,109,67) ;
    arrPalette[6][2].Color := TGIS_Color.FromRGB(253,174,97) ;
    arrPalette[6][3].Color := TGIS_Color.FromRGB(254,224,144) ;
    arrPalette[6][4].Color := TGIS_Color.FromRGB(255,255,191) ;
    arrPalette[6][5].Color := TGIS_Color.FromRGB(224,243,248) ;
    arrPalette[6][6].Color := TGIS_Color.FromRGB(171,217,233) ;
    arrPalette[6][7].Color := TGIS_Color.FromRGB(116,173,209) ;
    arrPalette[6][8].Color := TGIS_Color.FromRGB(69,117,180) ;

    // RdYlGn
    arrPalette[7][0].Color := TGIS_Color.FromRGB(215,48,39) ;
    arrPalette[7][1].Color := TGIS_Color.FromRGB(244,109,67) ;
    arrPalette[7][2].Color := TGIS_Color.FromRGB(253,174,97) ;
    arrPalette[7][3].Color := TGIS_Color.FromRGB(254,224,139) ;
    arrPalette[7][4].Color := TGIS_Color.FromRGB(255,255,191) ;
    arrPalette[7][5].Color := TGIS_Color.FromRGB(217,239,139) ;
    arrPalette[7][6].Color := TGIS_Color.FromRGB(166,217,106) ;
    arrPalette[7][7].Color := TGIS_Color.FromRGB(102,189,99) ;
    arrPalette[7][8].Color := TGIS_Color.FromRGB(26,152,80) ;

    // Spectral
    arrPalette[8][0].Color := TGIS_Color.FromRGB(213,62,79) ;
    arrPalette[8][1].Color := TGIS_Color.FromRGB(244,109,67) ;
    arrPalette[8][2].Color := TGIS_Color.FromRGB(253,174,97) ;
    arrPalette[8][3].Color := TGIS_Color.FromRGB(254,224,139) ;
    arrPalette[8][4].Color := TGIS_Color.FromRGB(255,255,191) ;
    arrPalette[8][5].Color := TGIS_Color.FromRGB(230,245,152) ;
    arrPalette[8][6].Color := TGIS_Color.FromRGB(171,221,164) ;
    arrPalette[8][7].Color := TGIS_Color.FromRGB(102,194,165) ;
    arrPalette[8][8].Color := TGIS_Color.FromRGB(50,136,189) ;
  end;

  procedure TGIS_ControlColor.createQualitative ;
  begin
    createPalette( 8, 9 ) ;

    // Accent
    arrPalette[0][0].Color := TGIS_Color.FromRGB(127,201,127) ;
    arrPalette[0][1].Color := TGIS_Color.FromRGB(190,174,212) ;
    arrPalette[0][2].Color := TGIS_Color.FromRGB(253,192,134) ;
    arrPalette[0][3].Color := TGIS_Color.FromRGB(255,255,153) ;
    arrPalette[0][4].Color := TGIS_Color.FromRGB(56,108,176) ;
    arrPalette[0][5].Color := TGIS_Color.FromRGB(240,2,127) ;
    arrPalette[0][6].Color := TGIS_Color.FromRGB(191,91,23) ;
    arrPalette[0][7].Color := TGIS_Color.FromRGB(102,102,102) ;

    // Dark2
    arrPalette[1][0].Color := TGIS_Color.FromRGB(27,158,119) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB(217,95,2) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB(117,112,179) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB(231,41,138) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB(102,166,30) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB(230,171,2) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB(166,118,29) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB(102,102,102) ;

    // Paired
    arrPalette[2][0].Color := TGIS_Color.FromRGB(166,206,227) ;
    arrPalette[2][1].Color := TGIS_Color.FromRGB(31,120,180) ;
    arrPalette[2][2].Color := TGIS_Color.FromRGB(178,223,138) ;
    arrPalette[2][3].Color := TGIS_Color.FromRGB(51,160,44) ;
    arrPalette[2][4].Color := TGIS_Color.FromRGB(251,154,153) ;
    arrPalette[2][5].Color := TGIS_Color.FromRGB(227,26,28) ;
    arrPalette[2][6].Color := TGIS_Color.FromRGB(253,191,111) ;
    arrPalette[2][7].Color := TGIS_Color.FromRGB(255,127,0) ;
    arrPalette[2][8].Color := TGIS_Color.FromRGB(202,178,214) ;

    // Pastel-1
    arrPalette[3][0].Color := TGIS_Color.FromRGB(251,180,174) ;
    arrPalette[3][1].Color := TGIS_Color.FromRGB(179,205,227) ;
    arrPalette[3][2].Color := TGIS_Color.FromRGB(204,235,197) ;
    arrPalette[3][3].Color := TGIS_Color.FromRGB(222,203,228) ;
    arrPalette[3][4].Color := TGIS_Color.FromRGB(254,217,166) ;
    arrPalette[3][5].Color := TGIS_Color.FromRGB(255,255,204) ;
    arrPalette[3][6].Color := TGIS_Color.FromRGB(229,216,189) ;
    arrPalette[3][7].Color := TGIS_Color.FromRGB(253,218,236) ;
    arrPalette[3][8].Color := TGIS_Color.FromRGB(242,242,242) ;

    // Pastel-2
    arrPalette[4][0].Color := TGIS_Color.FromRGB(179,226,205) ;
    arrPalette[4][1].Color := TGIS_Color.FromRGB(253,205,172) ;
    arrPalette[4][2].Color := TGIS_Color.FromRGB(203,213,232) ;
    arrPalette[4][3].Color := TGIS_Color.FromRGB(244,202,228) ;
    arrPalette[4][4].Color := TGIS_Color.FromRGB(230,245,201) ;
    arrPalette[4][5].Color := TGIS_Color.FromRGB(255,242,174) ;
    arrPalette[4][6].Color := TGIS_Color.FromRGB(241,226,204) ;
    arrPalette[4][7].Color := TGIS_Color.FromRGB(204,204,204) ;

    // Set-1
    arrPalette[5][0].Color := TGIS_Color.FromRGB(228,26,28) ;
    arrPalette[5][1].Color := TGIS_Color.FromRGB(55,126,184) ;
    arrPalette[5][2].Color := TGIS_Color.FromRGB(77,175,74) ;
    arrPalette[5][3].Color := TGIS_Color.FromRGB(152,78,163) ;
    arrPalette[5][4].Color := TGIS_Color.FromRGB(255,127,0) ;
    arrPalette[5][5].Color := TGIS_Color.FromRGB(255,255,51) ;
    arrPalette[5][6].Color := TGIS_Color.FromRGB(166,86,40) ;
    arrPalette[5][7].Color := TGIS_Color.FromRGB(247,129,191) ;

    // Set-2
    arrPalette[6][0].Color := TGIS_Color.FromRGB(102,194,165) ;
    arrPalette[6][1].Color := TGIS_Color.FromRGB(252,141,98) ;
    arrPalette[6][2].Color := TGIS_Color.FromRGB(141,160,203) ;
    arrPalette[6][3].Color := TGIS_Color.FromRGB(231,138,195) ;
    arrPalette[6][4].Color := TGIS_Color.FromRGB(166,216,84) ;
    arrPalette[6][5].Color := TGIS_Color.FromRGB(255,217,47) ;
    arrPalette[6][6].Color := TGIS_Color.FromRGB(229,196,148) ;
    arrPalette[6][7].Color := TGIS_Color.FromRGB(179,179,179) ;
    arrPalette[6][8].Color := TGIS_Color.FromRGB(153,153,153) ;

    // Set-3
    arrPalette[7][0].Color := TGIS_Color.FromRGB(141,211,199) ;
    arrPalette[7][1].Color := TGIS_Color.FromRGB(255,255,179) ;
    arrPalette[7][2].Color := TGIS_Color.FromRGB(190,186,218) ;
    arrPalette[7][3].Color := TGIS_Color.FromRGB(251,128,114) ;
    arrPalette[7][4].Color := TGIS_Color.FromRGB(128,177,211) ;
    arrPalette[7][5].Color := TGIS_Color.FromRGB(253,180,98) ;
    arrPalette[7][6].Color := TGIS_Color.FromRGB(179,222,105) ;
    arrPalette[7][7].Color := TGIS_Color.FromRGB(252,205,229) ;
    arrPalette[7][8].Color := TGIS_Color.FromRGB(217,217,217) ;
  end;

  procedure TGIS_ControlColor.createColorBlindFriendly ;
  begin
    createPalette( 11, 10 ) ;

    // IBM
    arrPalette[0][0].Color := TGIS_Color.FromARGB( $FF648FFF ) ;
    arrPalette[0][1].Color := TGIS_Color.FromARGB( $FF785EF0 ) ;
    arrPalette[0][2].Color := TGIS_Color.FromARGB( $FFDC267F ) ;
    arrPalette[0][3].Color := TGIS_Color.FromARGB( $FFFE6100 ) ;
    arrPalette[0][4].Color := TGIS_Color.FromARGB( $FFFFB000 ) ;

    // Colorblind barrier-free color pallet (Okabe, Ito)
    arrPalette[1][0].Color := TGIS_Color.FromRGB(230,159,0) ;
    arrPalette[1][1].Color := TGIS_Color.FromRGB(86,180,233) ;
    arrPalette[1][2].Color := TGIS_Color.FromRGB(0,158,115) ;
    arrPalette[1][3].Color := TGIS_Color.FromRGB(240,228,66) ;
    arrPalette[1][4].Color := TGIS_Color.FromRGB(0,114,178) ;
    arrPalette[1][5].Color := TGIS_Color.FromRGB(213,94,0) ;
    arrPalette[1][6].Color := TGIS_Color.FromRGB(204,121,167) ;
    arrPalette[1][7].Color := TGIS_Color.FromRGB(0,0,0) ;

    // Martin Krzywinski
    arrPalette[2][0].Color := TGIS_Color.FromARGB( $FF000000 ) ;
    arrPalette[2][1].Color := TGIS_Color.FromARGB( $FF2271B2 ) ;
    arrPalette[2][2].Color := TGIS_Color.FromARGB( $FF3DB7E9 ) ;
    arrPalette[2][3].Color := TGIS_Color.FromARGB( $FFF748A5 ) ;
    arrPalette[2][4].Color := TGIS_Color.FromARGB( $FF359B73 ) ;
    arrPalette[2][5].Color := TGIS_Color.FromARGB( $FFd55e00 ) ;
    arrPalette[2][6].Color := TGIS_Color.FromARGB( $FFe69f00 ) ;
    arrPalette[2][7].Color := TGIS_Color.FromARGB( $FFf0e442 ) ;

    // Paul Tol - Bright
    arrPalette[3][0].Color := TGIS_Color.FromARGB( $FF4477AA ) ;
    arrPalette[3][1].Color := TGIS_Color.FromARGB( $FFEE6677 ) ;
    arrPalette[3][2].Color := TGIS_Color.FromARGB( $FF228833 ) ;
    arrPalette[3][3].Color := TGIS_Color.FromARGB( $FFCCBB44 ) ;
    arrPalette[3][4].Color := TGIS_Color.FromARGB( $FF66CCEE ) ;
    arrPalette[3][5].Color := TGIS_Color.FromARGB( $FFAA3377 ) ;
    arrPalette[3][6].Color := TGIS_Color.FromARGB( $FFBBBBBB ) ;

    // Paul Tol - Vibrant
    arrPalette[4][0].Color := TGIS_Color.FromARGB( $FFEE7733 ) ;
    arrPalette[4][1].Color := TGIS_Color.FromARGB( $FF0077BB ) ;
    arrPalette[4][2].Color := TGIS_Color.FromARGB( $FF33BBEE ) ;
    arrPalette[4][3].Color := TGIS_Color.FromARGB( $FFEE3377 ) ;
    arrPalette[4][4].Color := TGIS_Color.FromARGB( $FFCC3311 ) ;
    arrPalette[4][5].Color := TGIS_Color.FromARGB( $FF009988 ) ;
    arrPalette[4][6].Color := TGIS_Color.FromARGB( $FFBBBBBB ) ;

    // Paul Tol - Muted
    arrPalette[5][0].Color := TGIS_Color.FromARGB( $FFCC6677 ) ;
    arrPalette[5][1].Color := TGIS_Color.FromARGB( $FF332288 ) ;
    arrPalette[5][2].Color := TGIS_Color.FromARGB( $FFDDCC77 ) ;
    arrPalette[5][3].Color := TGIS_Color.FromARGB( $FF117733 ) ;
    arrPalette[5][4].Color := TGIS_Color.FromARGB( $FF88CCEE ) ;
    arrPalette[5][5].Color := TGIS_Color.FromARGB( $FF882255 ) ;
    arrPalette[5][6].Color := TGIS_Color.FromARGB( $FF44AA99 ) ;
    arrPalette[5][7].Color := TGIS_Color.FromARGB( $FF999933 ) ;
    arrPalette[5][8].Color := TGIS_Color.FromARGB( $FFAA4499 ) ;

    // Paul Tol - MediumContrast
    arrPalette[6][0].Color := TGIS_Color.FromARGB( $FF6699CC ) ;
    arrPalette[6][1].Color := TGIS_Color.FromARGB( $FF004488 ) ;
    arrPalette[6][2].Color := TGIS_Color.FromARGB( $FFEECC66 ) ;
    arrPalette[6][3].Color := TGIS_Color.FromARGB( $FF994455 ) ;
    arrPalette[6][4].Color := TGIS_Color.FromARGB( $FF997700 ) ;
    arrPalette[6][5].Color := TGIS_Color.FromARGB( $FFEE99AA ) ;

    // Paul Tol - Light
    arrPalette[7][0].Color := TGIS_Color.FromARGB( $FF77AADD ) ;
    arrPalette[7][1].Color := TGIS_Color.FromARGB( $FFEE8866 ) ;
    arrPalette[7][2].Color := TGIS_Color.FromARGB( $FFEEDD88 ) ;
    arrPalette[7][3].Color := TGIS_Color.FromARGB( $FFFFAABB ) ;
    arrPalette[7][4].Color := TGIS_Color.FromARGB( $FF99DDFF ) ;
    arrPalette[7][5].Color := TGIS_Color.FromARGB( $FF44BB99 ) ;
    arrPalette[7][6].Color := TGIS_Color.FromARGB( $FFBBCC33 ) ;
    arrPalette[7][7].Color := TGIS_Color.FromARGB( $FFAAAA00 ) ;
    arrPalette[7][8].Color := TGIS_Color.FromARGB( $FFDDDDDD ) ;

    // Paul Tol - Pale
    arrPalette[8][0].Color := TGIS_Color.FromARGB( $FFBBCCEE ) ;
    arrPalette[8][1].Color := TGIS_Color.FromARGB( $FFCCEEFF ) ;
    arrPalette[8][2].Color := TGIS_Color.FromARGB( $FFCCDDAA ) ;
    arrPalette[8][3].Color := TGIS_Color.FromARGB( $FFEEEEBB ) ;
    arrPalette[8][4].Color := TGIS_Color.FromARGB( $FFFFCCCC ) ;
    arrPalette[8][5].Color := TGIS_Color.FromARGB( $FFDDDDDD ) ;

    // Paul Tol - Dark
    arrPalette[9][0].Color := TGIS_Color.FromARGB( $FF222255 ) ;
    arrPalette[9][1].Color := TGIS_Color.FromARGB( $FF225555 ) ;
    arrPalette[9][2].Color := TGIS_Color.FromARGB( $FF225522 ) ;
    arrPalette[9][3].Color := TGIS_Color.FromARGB( $FF666633 ) ;
    arrPalette[9][4].Color := TGIS_Color.FromARGB( $FF663333 ) ;
    arrPalette[9][5].Color := TGIS_Color.FromARGB( $FF555555 ) ;

    // Paul Tol - RainbowDiscrete
    arrPalette[10][0].Color := TGIS_Color.FromARGB( $FF882E72 ) ;
    arrPalette[10][1].Color := TGIS_Color.FromARGB( $FF1965B0 ) ;
    arrPalette[10][2].Color := TGIS_Color.FromARGB( $FF7BAFDE ) ;
    arrPalette[10][3].Color := TGIS_Color.FromARGB( $FF4EB265 ) ;
    arrPalette[10][4].Color := TGIS_Color.FromARGB( $FFCAE0AB ) ;
    arrPalette[10][5].Color := TGIS_Color.FromARGB( $FFF7F056 ) ;
    arrPalette[10][6].Color := TGIS_Color.FromARGB( $FFF4A736 ) ;
    arrPalette[10][7].Color := TGIS_Color.FromARGB( $FFE8601C ) ;
    arrPalette[10][8].Color := TGIS_Color.FromARGB( $FFDC050C ) ;
    arrPalette[10][9].Color := TGIS_Color.FromARGB( $FF72190E ) ;
  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlColor.typeChange(
    sender: TObject
  ) ;
  var
    a : Integer ;
  begin
    if cmbType.Item[ cmbType.ItemIndex ] = prevType then
      exit ;
    if not IsStringEmpty( edtAlpha.Text ) then
    begin
      case cmbType.ItemIndex of
        // RGB
        0 : begin
          pRgb.Visible := True ;
          pHsl.Visible := False ;
          pHsv.Visible := False ;
          pCmyk.Visible := False ;
          pHcl.Visible := False ;

          lblAlpha.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) ;

          if not TryStrToInt( edtAlpha.Text, a ) then
            exit ;

          edtAlpha.Text := IntToStr( Min( RoundS( a * 255 / 100 ), 255 ) ) ;
        end;
        // HSL
        1 : begin
          pRgb.Visible := False ;
          pHsl.Visible := True ;
          pHsv.Visible := False ;
          pCmyk.Visible := False ;
          pHcl.Visible := False ;

          // Adding components here to reuse it as both HSL and HSV uses Hue and Saturation
          lblHue.Context := pHsl.Context ;
          edtHue.Context := pHsl.Context ;
          lblSat.Context := pHsl.Context ;
          edtSat.Context := pHsl.Context ;

          if not ( ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HSV ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_CMYK ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HCL ) ) ) then begin
            lblAlpha.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) + ' (%)' ;

            if not TryStrToInt( edtAlpha.Text, a ) then
              exit ;

            edtAlpha.Text := IntToStr( RoundS( Min( a / 255 * 100, 100 ) ) ) ;
          end;
        end;
        // HSV
        2 : begin
          pRgb.Visible := False ;
          pHsl.Visible := False ;
          pHsv.Visible := True ;
          pCmyk.Visible := False ;
          pHcl.Visible := False ;

          // Adding components here to reuse it as both HSL and HSV uses Hue and Saturation
          lblHue.Context := pHsv.Context ;
          edtHue.Context := pHsv.Context ;
          lblSat.Context := pHsv.Context ;
          edtSat.Context := pHsv.Context ;

          if not ( ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HSL ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_CMYK ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HCL ) ) ) then begin
            lblAlpha.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) + ' (%)' ;

            if not TryStrToInt( edtAlpha.Text, a ) then
              exit ;

            edtAlpha.Text := IntToStr( RoundS( Min( a / 255 * 100, 100 ) ) ) ;
          end;
        end;
        // CMYK
        3 : begin
          pRgb.Visible := False ;
          pHsl.Visible := False ;
          pHsv.Visible := False ;
          pCmyk.Visible := True ;
          pHcl.Visible := False ;

          if not ( ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HSL ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HSV ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HCL ) ) )  then begin
            lblAlpha.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) + ' (%)' ;

            if not TryStrToInt( edtAlpha.Text, a ) then
              exit ;

            edtAlpha.Text := IntToStr( RoundS( Min( a / 255 * 100, 100 ) ) ) ;
          end;
        end;
        // HCL
        4 : begin
          pRgb.Visible := False ;
          pHsl.Visible := False ;
          pHsv.Visible := False ;
          pCmyk.Visible := False ;
          pHcl.Visible := True ;

          // Adding components here to reuse it as both HSL and HCL uses Hue
          lblHue.Context := pHcl.Context ;
          edtHue.Context := pHcl.Context ;

          if not ( ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HSL ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_HSV ) ) or ( prevType = _rsrcna( GIS_RS_COLOR_CONTROL_CMYK ) ) ) then begin
            lblAlpha.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) + ' (%)' ;

            if not TryStrToInt( edtAlpha.Text, a ) then
              exit ;

            edtAlpha.Text := IntToStr( RoundS( Min( a / 255 * 100, 100 ) ) ) ;
          end;
        end;
      end;
    end;

    prevType := cmbType.Item[ cmbType.ItemIndex ] ;
  end;

  procedure TGIS_ControlColor.paletteChange(
    sender: TObject
  ) ;
  begin
    LockWindow ;
    case cmbPalette.ItemIndex of
      0 : begin
        createStandard ;
      end;
      1 : begin
        createSequential_1 ;
      end;
      2 : begin
        createSequential_2 ;
      end;
      3 : begin
        createSequential_3 ;
      end;
      4 : begin
        createDiverging ;
      end;
      5 : begin
        createQualitative ;
      end;
      6 : begin
        createColorBlindFriendly ;
      end;
    end;
    UnlockWindow ;
  end;

  procedure TGIS_ControlColor.alphaChange(
    sender: TObject
  ) ;
  var
    v1 : Integer ;
    v2 : Double ;
  begin
    if isLocked then
      exit ;

    if IsStringEmpty( edtAlpha.Text ) then
      exit ;

    lock ;
    try
      case cmbType.ItemIndex of
        // RGB
        0 : begin
          if checkInteger( edtAlpha.Text, 255, v1 ) then begin
            cAlphaBar.Value := v1 / 255 ;
            edtAlpha.SetFontDefault ;
          end else begin
            edtAlpha.SetFontAlarm ;
            exit ;
          end;
        end;
        // HSL or HSV or CMYK or HCL
        1, 2, 3, 4 : begin
          if checkDouble( edtAlpha.Text, 100, v2 ) then begin
            cAlphaBar.Value := v2 / 100 ;
            edtAlpha.SetFontDefault ;
          end else begin
            edtAlpha.SetFontAlarm ;
            exit ;
          end;
        end;
      end;

      assembleARGB ;

      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.rgbChange(
    sender: TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not ( sender is TGIS_PvlEdit ) then
      exit ;

    if IsStringEmpty( TGIS_PvlEdit( sender ).Text ) then
      exit ;

    if not checkInteger( TGIS_PvlEdit( sender ).Text, 255, v ) then begin
      TGIS_PvlEdit( sender ).SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      TGIS_PvlEdit( sender ).SetFontDefault ;
      assembleARGB ;
      calculateColors ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.hueChange(
    sender: TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtHue.Text, 360, v ) then begin
      edtHue.SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      edtHue.SetFontDefault ;
      assembleHSL ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.satChange(
    sender: TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtSat.Text, 100, v ) then begin
      edtSat.SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      edtSat.SetFontDefault ;
      assembleHSL ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.ligChange(
    sender: TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtLig.Text, 100, v ) then begin
      edtLig.SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      edtLig.SetFontDefault ;
      assembleHSL ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.valChange(
    sender: TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtVal.Text, 100, v ) then begin
      edtVal.SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      edtVal.SetFontDefault ;
      assembleHSV ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.chromaChange(
    sender: TObject 
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtChroma.Text, 134, v ) then begin
      edtChroma.SetFontAlarm ;
      exit ;
    end;

    lock ;
    try
      edtChroma.SetFontDefault ;
      assembleHCL ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.lumChange(
    sender: TObject
  );
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not checkInteger( edtLum.Text, 100, v ) then begin
      edtLum.SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      edtLum.SetFontDefault ;
      assembleHCL ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.hexChange(
    sender: TObject
  ) ;
  var
    clr : TGIS_Color ;
  begin
    if isLocked then
      exit ;

    if not TGIS_Color.TryFromString( edtHex.Text, clr) then begin
      edtHex.SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      edtHex.SetFontDefault ;

      case cmbType.ItemIndex of
        // RGB
        0 : begin
          edtAlpha.Text := IntToStr( clr.A ) ;
        end;
        // HSL or HSV or CMYK
        1, 2, 3 : begin
          edtAlpha.Text := IntToStr( RoundS( clr.A / 255 * 100 ) ) ;
        end;
      end;
      edtRed.Text   := IntToStr( clr.R ) ;
      edtGreen.Text := IntToStr( clr.G ) ;
      edtBlue.Text  := IntToStr( clr.B ) ;

      assembleARGB ;

      calculateColors ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.cmykChange(
    sender: TObject
  ) ;
  var
    v : Integer ;
  begin
    if isLocked then
      exit ;

    if not ( sender is TGIS_PvlEdit ) then
      exit ;

    if IsStringEmpty( TGIS_PvlEdit( sender ).Text ) then
      exit ;

    if not checkInteger( TGIS_PvlEdit( sender ).Text, 100, v ) then begin
      TGIS_PvlEdit( sender ).SetFontAlarm ;
      exit ;
    end ;

    lock ;
    try
      TGIS_PvlEdit( sender ).SetFontDefault ;
      assembleCMYK ;
      edtHex.Text := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.wheelChange(
    sender: TObject
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
    try
      clr := cColorWheel.Color ;
      mul := cColorBar.Value ;

      cColorBar.Color := clr ;

      r := FloorS( mul*clr.R ) ;
      g := FloorS( mul*clr.G ) ;
      b := FloorS( mul*clr.B ) ;

      clr := TGIS_Color.FromRGB( r, g, b ) ;

      cAlphaBar.Color := clr ;

      mul := cAlphaBar.Value ;
      a := RoundS( 255*mul ) ;
      prevNewColor.Color := TGIS_Color.FromARGB( a, r, g, b ) ;

      edtHex.Text   := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;

      cAlphaBar.Value := DotStrToFloat( IntToStr( a ) ) / 255 ;

      edtRed.Text   := IntToStr( r ) ;
      edtGreen.Text := IntToStr( g ) ;
      edtBlue.Text  := IntToStr( b ) ;

      calculateColors ;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.colorBarChange(
    sender : TObject
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
    try
      clr := cColorWheel.Color ;
      mul := cColorBar.Value ;

      r := FloorS( mul*clr.R ) ;
      g := FloorS( mul*clr.G ) ;
      b := FloorS( mul*clr.B ) ;

      clr := TGIS_Color.FromRGB( r, g, b ) ;

      cAlphaBar.Color := clr ;

      mul := cAlphaBar.Value ;
      a := RoundS( 255*mul ) ;
      prevNewColor.Color := TGIS_Color.FromARGB( a, r, g, b ) ;

      edtHex.Text   := Format( '%.8x', [ Integer( prevNewColor.Color.ARGB )] ) ;

      cAlphaBar.Value := DotStrToFloat( IntToStr( a ) ) / 255  ;

      edtRed.Text   := IntToStr( r ) ;
      edtGreen.Text := IntToStr( g ) ;
      edtBlue.Text  := IntToStr( b ) ;

      calculateColors ;
    finally
      unlock ;
    end;
  end ;

  procedure TGIS_ControlColor.alphaBarChange(
    sender : TObject
  ) ;
  var
    clr : TGIS_Color ;
    a   : Byte ;
  begin
    lock ;
    try
      a := RoundS( 255 * cAlphaBar.Value ) ;
      clr :=  cAlphaBar.Color ;
      prevNewColor.Color := TGIS_Color.FromARGB( a, clr.R, clr.G, clr.B ) ;

      edtHex.Text   := Format( '%.8x', [ Integer( prevNewColor.Color.ARGB ) ] ) ;

      case cmbType.ItemIndex of
        // RGB
        0 : begin
          edtAlpha.Text := DotFloatToStr( a ) ;
        end;
        // HSL or HSV or CMYK or HCL
        1, 2, 3, 4: begin
          edtAlpha.Text := DotFloatToStrPrec( cAlphaBar.Value * 100, 0 ) ;
        end;
      end;
    finally
      unlock ;
    end;
  end;

  procedure TGIS_ControlColor.previewClick(
    sender: TObject
  ) ;
  var
    clr : TGIS_Color ;
    r   : Byte ;
    g   : Byte ;
    b   : Byte ;
  begin
    lock ;
    try
      if not ( sender is TGIS_PvlColorPreview ) then
        exit ;

      clr := TGIS_PvlColorPreview( sender ).Color ;

      if clr = TGIS_Color.None then
        exit ;

      prevNewColor.Color := clr ;

      r := clr.R;
      g := clr.G;
      b := clr.B;

      edtHex.Text   := Format( '%.8x', [Integer( prevNewColor.Color.ARGB )] ) ;


      case cmbType.ItemIndex of
        // RGB
        0 : begin
          edtAlpha.Text := IntToStr( RoundS( cAlphaBar.Value * 255 ) ) ;
        end;
        // HSL or HSV or CMYK
        1, 2, 3: begin
          edtAlpha.Text := DotFloatToStrPrec( cAlphaBar.Value * 100, 0 ) ;
        end;
      end;
      edtRed.Text   := IntToStr( r ) ;
      edtGreen.Text := IntToStr( g ) ;
      edtBlue.Text  := IntToStr( b ) ;

      assembleARGB ;

      calculateColors ;
    finally
      unlock ;
    end;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlColor.DoAfterCreate;
  begin
    inherited ;
    Color := TGIS_Color.FromRGB( $FFC0C0C0 ) ;
  end;

  procedure TGIS_ControlColor.DoInitForm ;
  begin
    inherited ;

    iMaxHeight := 280 ;
    iMaxWidth  := 380 ;

    Self.Caption := _rsrc( GIS_RS_COLOR_CONTROL_COLOR ) ;
    Self.ClientHeight := iMaxHeight ;
    Self.ClientWidth := iMaxWidth ;
    Self.Name := 'TGIS_ControlColor' ;
  end ;

  procedure TGIS_ControlColor.DoInitControls ;
  const
    COLOR_WHEEL_SIZE = {$IFDEF GIS_MOBILE_DIALOGS}1.5 * {$ENDIF}140 ;
    SECOND_COLUMN_WIDTH = {$IFDEF GIS_MOBILE_DIALOGS} 30 + {$ENDIF}125 ;
  var
    iDiff     : Integer ;
    base      : TGIS_PvlBase ;
  begin
    inherited ;

    cColorWheel := TGIS_PvlColorWheel.Create( Context ) ;
    cColorWheel.Place( RoundS( COLOR_WHEEL_SIZE ), RoundS( COLOR_WHEEL_SIZE ), nil, Context.HMargin, nil, 0 ) ;

    {$IFDEF DCC}
      cColorWheel.OnChange := wheelChange ;
    {$ELSE}
      cColorWheel.OnChange := @wheelChange ;
    {$ENDIF}

    lblColorBar := TGIS_PvlLabel.Create( Context ) ;
    lblColorBar.Place( cColorWheel.Width, 0, nil, Context.HMargin, cColorWheel, 0 ) ;
    lblColorBar.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_VALUE ) ;

    cColorBar := TGIS_PvlColorBar.Create( Context ) ;
    cColorBar.Place( cColorWheel.Width, RoundS( {$IFDEF GIS_MOBILE_DIALOGS}1.25 * {$ENDIF} 20 ), nil,Context.HMargin, lblColorBar, 0 ) ;

    {$IFDEF DCC}
      cColorBar.OnChange := colorBarChange ;
    {$ELSE}
      cColorBar.OnChange := @colorBarChange ;
    {$ENDIF}

    lblAlphaBar := TGIS_PvlLabel.Create( Context ) ;
    lblAlphaBar.Place( cColorWheel.Width, 0, nil, Context.HMargin, cColorBar, 0 ) ;
    lblAlphaBar.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) ;

    cAlphaBar := TGIS_PvlColorBar.Create( Context ) ;
    cAlphaBar.Place( cColorWheel.Width, RoundS( {$IFDEF GIS_MOBILE_DIALOGS}1.25 * {$ENDIF} 20 ), nil,Context.HMargin, lblAlphaBar, 0 ) ;
    cAlphaBar.Alpha := True;

    {$IFDEF DCC}
      cAlphaBar.OnChange := alphaBarChange ;
    {$ELSE}
      cAlphaBar.OnChange := @alphaBarChange ;
    {$ENDIF}

    lblPrevNewColor := TGIS_PvlLabel.Create( Context ) ;
    lblPrevNewColor.Place( {$IFDEF GIS_MOBILE_DIALOGS} 10 + {$ENDIF}60, 0, cColorWheel, 5 * Context.VSpace, nil, cColorWheel.Top + Context.HMargin ) ;
    lblPrevNewColor.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_PREVIEW ) ;

    prevNewColor := TGIS_PvlColorPreview.Create( Context ) ;
    prevNewColor.Place( SECOND_COLUMN_WIDTH - lblPrevNewColor.Width, 22, lblPrevNewColor, 0, nil, cColorWheel.Top + Context.HMargin  ) ;
    prevNewColor.Border := true ;

    lblPrevNewColor.Height := prevNewColor.Height ;

    lblPrevCurrColor := TGIS_PvlLabel.Create( Context ) ;
    lblPrevCurrColor.Place( lblPrevNewColor.Width, 0, cColorWheel, 5 * Context.VSpace, nil, lblPrevNewColor.Top + lblPrevNewColor.Height ) ;
    lblPrevCurrColor.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_CURRENT ) ;

    prevCurrColor := TGIS_PvlColorPreview.Create( Context ) ;
    prevCurrColor.Place( prevNewColor.Width, 22, lblPrevCurrColor, 0, nil, lblPrevNewColor.Top + lblPrevNewColor.Height  ) ;
    prevCurrColor.Border := true ;

    lblPrevCurrColor.Height := prevCurrColor.Height ;

    lblHex := TGIS_PvlLabel.Create( Context ) ;
    lblHex.Place( lblPrevNewColor.Width, 0,cColorWheel, 5 * Context.VSpace, prevCurrColor, 0 ) ;
    lblHex.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_HEX ) ;

    edtHex := TGIS_PvlEdit.Create( Context ) ;
    edtHex.Place( prevCurrColor.Width + lblPrevCurrColor.Width - lblHex.Width, 0, lblHex, 0, prevCurrColor, 0 ) ;

    {$IFDEF DCC}
      edtHex.OnChange := hexChange ;
    {$ELSE}
      edtHex.OnChange := @hexChange ;
    {$ENDIF}

    lblHex.Height := edtHex.Height ;

    lblCmbType := TGIS_PvlLabel.Create( Context ) ;
    lblCmbType.Place( SECOND_COLUMN_WIDTH, 0, cColorWheel, 5 * Context.VSpace, edtHex, 0 ) ;
    lblCmbType.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_TYPE ) ;

    cmbType := TGIS_PvlComboBox.Create( Context ) ;
    cmbType.Place( lblCmbType.Width, 0, cColorWheel, 5 * Context.VSpace, lblCmbType, 0 ) ;
    cmbType.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_RGB ) );
    cmbType.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_HSL ) );
    cmbType.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_HSV ) );
    cmbType.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_CMYK ) ) ;
    cmbType.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_HCL ) ) ;

    {$IFDEF DCC}
      cmbType.OnChange := typeChange ;
    {$ELSE}
      cmbType.OnChange := @typeChange ;
    {$ENDIF}

    lblAlpha := TGIS_PvlLabel.Create( Context ) ;
    lblAlpha.Place( {$IFDEF GIS_MOBILE_DIALOGS}10 +{$ENDIF}80, 0, cColorWheel, 5 * Context.VSpace, cmbType, 0 ) ;
    lblAlpha.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_ALPHA ) ;

    edtAlpha := TGIS_PvlEdit.Create( Context ) ;
    edtAlpha.Place( prevCurrColor.Width + lblPrevCurrColor.Width - lblAlpha.Width - Context.VSpace, 0, lblAlpha, Context.VSpace, cmbType, 0 ) ;

    {$IFDEF DCC}
      edtAlpha.OnChange := alphaChange ;
    {$ELSE}
      edtAlpha.OnChange := @alphaChange ;
    {$ENDIF}

    lblAlpha.Height := edtAlpha.Height ;

    pRgb := TGIS_PvlPanel.Create( Context ) ;
    pRgb.Place( cmbType.Width, 75, cColorWheel, 5 * Context.VSpace, edtAlpha, 0 ) ;

    pHsl := TGIS_PvlPanel.Create( Context ) ;
    pHsl.Place( cmbType.Width, pRgb.Height, cColorWheel, 5 * Context.VSpace, edtAlpha, 0 ) ;
    pHsl.Visible := false ;

    pHsv := TGIS_PvlPanel.Create( Context ) ;
    pHsv.Place( cmbType.Width, pRgb.Height, cColorWheel, 5 * Context.VSpace, edtAlpha, 0 ) ;
    pHsv.Visible := false ;

    pCmyk := TGIS_PvlPanel.Create( Context ) ;
    pCmyk.Place( cmbType.Width, pRgb.Height, cColorWheel, 5 * Context.VSpace, edtAlpha, 0 ) ;
    pCmyk.Visible := false ;

    pHcl := TGIS_PvlPanel.Create( Context ) ;
    pHcl.Place( cmbType.Width, pRgb.Height, cColorWheel, 5 * Context.VSpace, edtAlpha, 0 ) ;
    pHcl.Visible := false ;

    {$REGION 'RGB panel'}
      lblRed := TGIS_PvlLabel.Create( pRgb.Context ) ;
      lblRed.Place( lblAlpha.Width, 0, nil, 0, nil, 0 ) ;
      lblRed.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_RED ) ;

      edtRed := TGIS_PvlEdit.Create( pRgb.Context ) ;
      edtRed.Place( cmbType.Width - lblRed.Width - pRgb.Context.VSpace, 0, lblRed, pRgb.Context.VSpace, nil, 0 ) ;

      {$IFDEF DCC}
        edtRed.OnChange := rgbChange ;
      {$ELSE}
        edtRed.OnChange := @rgbChange ;
      {$ENDIF}

      lblRed.Height := edtRed.Height ;

      lblGreen := TGIS_PvlLabel.Create( pRgb.Context ) ;
      lblGreen.Place( lblAlpha.Width, 0, nil, 0, edtRed, 0 ) ;
      lblGreen.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_GREEN ) ;

      edtGreen := TGIS_PvlEdit.Create( pRgb.Context ) ;
      edtGreen.Place( cmbType.Width - lblGreen.Width - pRgb.Context.VSpace, 0, lblGreen, pRgb.Context.VSpace, edtRed, 0 ) ;

      {$IFDEF DCC}
        edtGreen.OnChange := rgbChange ;
      {$ELSE}
        edtGreen.OnChange := @rgbChange ;
      {$ENDIF}

      lblGreen.Height := edtGreen.Height ;

      lblBlue := TGIS_PvlLabel.Create( pRgb.Context ) ;
      lblBlue.Place( lblAlpha.Width, 0, nil, 0, edtGreen, 0 ) ;
      lblBlue.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_BLUE ) ;

      edtBlue := TGIS_PvlEdit.Create( pRgb.Context ) ;
      edtBlue.Place( cmbType.Width - lblBlue.Width - pRgb.Context.VSpace, 0, lblBlue, pRgb.Context.VSpace, edtGreen, 0 ) ;

      {$IFDEF DCC}
        edtBlue.OnChange := rgbChange ;
      {$ELSE}
        edtBlue.OnChange := @rgbChange ;
      {$ENDIF}

      lblBlue.Height := edtBlue.Height ;

      pRgb.Height := edtBlue.Top + edtBlue.Height + Context.VMargin ;

    {$ENDREGION 'RGB panel'}

    {$REGION 'HSV and HSL ( + HCL hue ) panel'}
      lblHue := TGIS_PvlLabel.Create( pHsv.Context ) ;
      lblHue.Place( lblAlpha.Width, 0, nil, 0, nil, 0 ) ;
      lblHue.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_HUE ) + ' (°)' ;

      edtHue := TGIS_PvlEdit.Create( pHsv.Context ) ;
      edtHue.Place( cmbType.Width - lblHue.Width - pHsv.Context.VSpace, 0, lblHue, pHsv.Context.VSpace, nil, 0 ) ;

      {$IFDEF DCC}
        edtHue.OnChange := hueChange ;
      {$ELSE}
        edtHue.OnChange := @hueChange ;
      {$ENDIF}

      lblHue.Height := edtHue.Height ;

      lblSat := TGIS_PvlLabel.Create( pHsv.Context ) ;
      lblSat.Place( lblAlpha.Width, 0, nil, 0, edtHue, 0 ) ;
      lblSat.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_SAT ) + ' (%)' ;

      edtSat := TGIS_PvlEdit.Create( pHsv.Context ) ;
      edtSat.Place( cmbType.Width - lblHue.Width - pHsv.Context.VSpace, 0, lblSat, pHsv.Context.VSpace, edtHue, 0 ) ;

      {$IFDEF DCC}
        edtSat.OnChange := satChange ;
      {$ELSE}
        edtSat.OnChange := @satChange ;
      {$ENDIF}

      lblSat.Height := edtSat.Height ;
    {$ENDREGION 'HSV and HSL'}

    {$REGION 'HSL panel'}
      lblLig := TGIS_PvlLabel.Create( pHsl.Context ) ;
      lblLig.Place( lblAlpha.Width, 0, nil, 0, edtSat, 0 ) ;
      lblLig.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_LIGHTNESS ) + ' (%)' ;

      edtLig := TGIS_PvlEdit.Create( pHsl.Context ) ;
      edtLig.Place( cmbType.Width - lblHue.Width - pHsl.Context.VSpace, 0, lblLig, pHsl.Context.VSpace, edtSat, 0 ) ;

      {$IFDEF DCC}
        edtLig.OnChange := ligChange ;
      {$ELSE}
        edtLig.OnChange := @ligChange ;
      {$ENDIF}

      lblLig.Height := edtLig.Height ;

      pHsl.Height := edtLig.Top + edtLig.Height + pHsl.Context.VMargin ;

    {$ENDREGION 'HSL panel'}

    {$REGION 'HSV panel'}
      lblVal := TGIS_PvlLabel.Create( pHsv.Context ) ;
      lblVal.Place( lblAlpha.Width, 0, nil, 0, edtSat, 0 ) ;
      lblVal.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_VALUE ) + ' (%)' ;

      edtVal := TGIS_PvlEdit.Create( pHsv.Context ) ;
      edtVal.Place( cmbType.Width - lblHue.Width - pHsv.Context.VSpace, 0, lblLig, pHsv.Context.VSpace, edtSat, 0 ) ;

      {$IFDEF DCC}
        edtVal.OnChange := valChange ;
      {$ELSE}
        edtVal.OnChange := @valChange ;
      {$ENDIF}

      lblVal.Height := edtVal.Height ;

      pHsv.Height := edtVal.Top + edtVal.Height + pHsv.Context.VMargin ;

    {$ENDREGION 'HSV panel'}

    {$REGION 'CMYK panel'}

      lblCyan := TGIS_PvlLabel.Create( pCmyk.Context ) ;
      lblCyan.Place( lblAlpha.Width, 0, nil, 0, nil, 0 ) ;
      lblCyan.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_CYAN ) + ' (%)' ;

      edtCyan := TGIS_PvlEdit.Create( pCmyk.Context ) ;
      edtCyan.Place( cmbType.Width - lblCyan.Width - pCmyk.Context.VSpace, 0, lblCyan, pCmyk.Context.VSpace, nil, 0 ) ;

      {$IFDEF DCC}
        edtCyan.OnChange := cmykChange ;
      {$ELSE}
        edtCyan.OnChange := @cmykChange ;
      {$ENDIF}

      lblCyan.Height := edtCyan.Height ;

      lblMagenta := TGIS_PvlLabel.Create( pCmyk.Context ) ;
      lblMagenta.Place( lblAlpha.Width, 0, nil, 0, edtCyan, 0 ) ;
      lblMagenta.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_MAGENTA ) + ' (%)' ;

      edtMagenta := TGIS_PvlEdit.Create( pCmyk.Context ) ;
      edtMagenta.Place( cmbType.Width - lblCyan.Width - pCmyk.Context.VSpace, 0, lblMagenta, pCmyk.Context.VSpace, edtCyan, 0 ) ;

      {$IFDEF DCC}
        edtMagenta.OnChange := cmykChange ;
      {$ELSE}
        edtMagenta.OnChange := @cmykChange ;
      {$ENDIF}

      lblMagenta.Height := edtMagenta.Height ;

      lblYellow := TGIS_PvlLabel.Create( pCmyk.Context ) ;
      lblYellow.Place( lblAlpha.Width, 0, nil, 0, edtMagenta, 0 ) ;
      lblYellow.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_YELLOW ) + ' (%)' ;

      edtYellow := TGIS_PvlEdit.Create( pCmyk.Context ) ;
      edtYellow.Place( cmbType.Width - lblCyan.Width - pCmyk.Context.VSpace, 0, lblYellow, pCmyk.Context.VSpace, edtMagenta, 0 ) ;

      {$IFDEF DCC}
        edtYellow.OnChange := cmykChange ;
      {$ELSE}
        edtYellow.OnChange := @cmykChange ;
      {$ENDIF}

      lblYellow.Height := edtYellow.Height ;

      lblKey := TGIS_PvlLabel.Create( pCmyk.Context ) ;
      lblKey.Place( lblAlpha.Width, 0, nil, 0, edtYellow, 0 ) ;
      lblKey.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_KEY ) + ' (%)' ;

      edtKey := TGIS_PvlEdit.Create( pCmyk.Context ) ;
      edtKey.Place( cmbType.Width - lblCyan.Width - pCmyk.Context.VSpace, 0, lblKey, pCmyk.Context.VSpace, edtYellow, 0 ) ;

      {$IFDEF DCC}
        edtKey.OnChange := cmykChange ;
      {$ELSE}
        edtKey.OnChange := @cmykChange ;
      {$ENDIF}

      lblKey.Height := edtKey.Height ;

      pCmyk.Height := edtKey.Top + edtKey.Height + pCmyk.Context.VMargin ;

    {$ENDREGION 'CMYK panel'}

    {$REGION 'HCL panel'}
      lblChroma := TGIS_PvlLabel.Create( pHcl.Context ) ;
      lblChroma.Place( lblAlpha.Width, 0, nil, 0, edtHue, 0 ) ;
      lblChroma.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_CHROMA + ' (°)'  ) ;

      edtChroma := TGIS_PvlEdit.Create( pHcl.Context ) ;
      edtChroma.Place( cmbType.Width - lblChroma.Width - pHcl.Context.VSpace, 0, lblHue, pHcl.Context.HSpace, edtHue, 0 ) ;

      lblChroma.Height := edtChroma.Height ;

      {$IFDEF DCC}
        edtChroma.OnChange := ChromaChange ;
      {$ELSE}
        edtChroma.OnChange := @chromaChange ;
      {$ENDIF}

      lblLum := TGIS_PvlLabel.Create( pHcl.Context ) ;
      lblLum.Place( lblChroma.Width, 0, nil, 0, edtChroma, 0 ) ;
      lblLum.Caption := _rsrcna( GIS_RS_COLOR_CONTROL_LUM + ' (%)' ) ;

      edtLum := TGIS_PvlEdit.Create( pHcl.Context ) ;
      edtLum.Place( edtChroma.Width, 0, lblLum, pHcl.Context.HSpace, edtChroma, 0 ) ;

      lblLum.Height := edtLum.Height ;

      {$IFDEF DCC}
        edtLum.OnChange := LumChange ;
      {$ELSE}
        edtLum.OnChange := @lumChange ;
      {$ENDIF}

    {$ENDREGION 'HCL panel'}

    {$IFDEF DCC}
      cmbType.OnChange := typeChange ;
    {$ELSE}
      cmbType.OnChange := @typeChange ;
    {$ENDIF}

    cmbPalette := TGIS_PvlComboBox.Create( Context ) ;
    cmbPalette.Place( RoundS( 11 * ( PREVIEW_SIZE + Context.HMargin / 2 ) ), 0, prevNewColor, 5 * Context.VSpace, nil, Context.HMargin ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_STANDARD ) ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_SEQUENTIAL_1 ) ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_SEQUENTIAL_2 ) ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_SEQUENTIAL_3 ) ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_DIVERGING ) ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_QUALITATIVE ) ) ;
    cmbPalette.ItemsAdd( _rsrcna( GIS_RS_COLOR_CONTROL_PALETTE_COLORBLIND ) ) ;

    pPalette := TGIS_PvlPanel.Create( Context ) ;
    pPalette.Place( cmbPalette.Width + RoundS( Context.HMargin / 2 ) {$IFDEF GIS_MOBILE_DIALOGS} + RoundS( Context.HMargin ) {$ENDIF}, RoundS( 11 * ( PREVIEW_SIZE + Context.VMargin / 2 ) ), prevNewColor, 5 * Context.VSpace, cmbPalette, 0 ) ;

    {$IFDEF DCC}
      cmbPalette.OnChange := paletteChange ;
    {$ELSE}
      cmbPalette.OnChange := @paletteChange ;
    {$ENDIF}

    cmbType.ItemIndex := 0 ;
    cmbPalette.ItemIndex := 0 ;
    prevType := cmbType.Item[ cmbType.ItemIndex ] ;

    createStandard ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;
    BtnHelp.TabOrder := 2 ;
    BtnCancel.TabOrder := 4 ;
    BtnOK.TabOrder := 3 ;

    iHeight := pCmyk.Top + pCmyk.Height + {$IFNDEF GIS_MOBILE_DIALOGS} Context.VSpace + BtnOK.Height + {$ENDIF} Context.VMargin ;

    if iMaxHeight < iHeight  then
      iMaxHeight := iHeight ;

    iHeight := pPalette.Top + pPalette.Height + {$IFNDEF GIS_MOBILE_DIALOGS} Context.VSpace + BtnOK.Height + {$ENDIF} Context.VMargin ;

    if iMaxHeight < iHeight  then
      iMaxHeight := iHeight ;

    //Ensure correct controls placment in case of RightToLeft set to true.
    if not Context.RightToLeft then begin
      iWidth := pPalette.Left + pPalette.Width + Context.HMargin
    end else begin
      iWidth := - pPalette.Left + cColorWheel.Left + cColorWheel.Width + Context.HMargin + 2 * Context.HSpace ;
      if iMaxWidth < iWidth then
      begin
        iDiff := iWidth - iMaxWidth ;
        for base in Context.Controls do begin
          if base is TGIS_PvlControl then
            TGIS_PvlControl( base ).Left := TGIS_PvlControl( base ).Left + iDiff ;
        end;
      end;
    end;

    if iMaxWidth < iWidth then
      iMaxWidth := iWidth ;

    Self.ClientHeight := iMaxHeight ;
    Self.ClientWidth  := iMaxWidth ;

    //Ensure that we correct buttons placement !
    doPlaceButtons ;

    pPalette.Height := iMaxHeight - ( cmbPalette.Top + cmbPalette.Height + Context.HSpace + Context.VMargin + 30 ) ;
  end ;

  procedure TGIS_ControlColor.DoRedraw ;
  begin
    inherited ;
  end;

  procedure TGIS_ControlColor.fset_Color(
    const _value : TGIS_Color
  ) ;
  begin
    prevNewColor.Color := _value ;
  end;

  function TGIS_ControlColor.fget_Color
    : TGIS_Color ;
  begin
    Result := prevNewColor.Color ;
  end;

  procedure TGIS_ControlColor.lock ;
  begin
    bLock := True ;
  end ;

  procedure TGIS_ControlColor.unlock ;
  begin
    bLock := False ;
  end ;

  function TGIS_ControlColor.isLocked : Boolean ;
  begin
    Result := bLock ;
  end ;

  function TGIS_ControlColor.Execute
   : TGIS_PvlModalResult;
  begin
    Result := Execute( TGIS_Proc( nil ) ) ;
  end ;

  function TGIS_ControlColor.Execute(
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( Color, TGIS_HelpEvent(nil), _proc ) ;
  end ;

  function TGIS_ControlColor.Execute(
    const _color  : TGIS_Color ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( _color, TGIS_HelpEvent(nil), _proc ) ;
  end ;

  function TGIS_ControlColor.Execute(
    const _color  : TGIS_Color
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( _color, TGIS_HelpEvent(nil), nil ) ;
  end ;

  function TGIS_ControlColor.Execute(
    const _color  : TGIS_Color ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult;
  var
    h : Double ;
    s : Double ;
    v : Double ;
  begin
    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    lock ;

    _color.ToHSV( h, s, v ) ;

    cColorWheel.Color := _color ;
    cColorBar.Color := _color ;
    cColorBar.Value := v ;
    cAlphaBar.Color := _color ;
    cAlphaBar.Value := _color.A/255 ;

    prevCurrColor.Color := _color ;
    prevNewColor.Color := _color ;
    prevCurrColor.Color := _color ;

    edtHex.Text   := Format( '%.8x', [ Integer( _color.ARGB )] ) ;

    edtAlpha.Text := IntToStr( _color.A ) ;
    edtRed.Text   := IntToStr( _color.R ) ;
    edtGreen.Text := IntToStr( _color.G ) ;
    edtBlue.Text  := IntToStr( _color.B ) ;

    calculateColors ;

    unlock ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

  procedure TGIS_ControlColor.doDestroy;
  begin
    inherited ;
  end;

//==================================== END =====================================
end.
