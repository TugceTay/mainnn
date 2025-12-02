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
  Legend component. Vector dialog - font symbol selector.
}

unit Lider.CG.GIS.FMX.GeoControlLegendFNT ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendFNT"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Actions,
  System.Classes,
  System.Types,
  System.UITypes,
  System.Math,
  FMX.Forms,
  FMX.ActnList,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.Layouts,
  FMX.Graphics,
  FMX.Objects,
  FMX.ComboEdit,
  FMX.ListBox,
  FMX.Types,
  FMX.Controls,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.FMX.GeoFramework, FMX.Controls.Presentation,
  Lider.CG.GIS.FMX.GeoModalForm ;

type

  /// <summary>
  ///   Visual form for managing font symbols.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlLegendFNT = class( TGIS_ModalForm )
    lblFonts: TLabel;
    cmbFonts: TComboBox;
    chkStrikeout: TCheckBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    lblChar : TLabel;
    edtChar : TEdit;
    lblCode : TLabel;
    lblUnicode : TLabel;
    edtUnicode : TEdit;
    pnlPreview : TRectangle;
    lblPreview: TLabel;
    procedure FormShow(Sender: TObject);
    procedure cmbFontsChange(Sender: TObject);
    procedure edtCharKeyDown(Sender: TObject; var Key: Word;
                             var KeyChar: Char; Shift: TShiftState);
    procedure edtCharTyping(Sender: TObject);
    procedure edtCharChange(Sender: TObject);
    procedure edtUnicodeEnter(Sender: TObject);
    procedure edtUnicodeKeyDown(Sender: TObject; var Key: Word;
                             var KeyChar: Char; Shift: TShiftState);
    procedure edtUnicodeTyping(Sender: TObject);
    procedure actStyleExecute(Sender: TObject);
    procedure actListUpdate(Sender: TObject);
    procedure btnOKClick(Sender: TObject); override;
  private
    FSymbol : TGIS_SymbolFont ;
    FLetter : Char ;
    pModalProc : TProc<TModalResult> ;
    tmrAfterShow : TTimer ; // to bypass FMX iOS bug
  private
    const frmClientHeight = 282 ;
    const frmClientWidth  = 298 ;
  private
    procedure doTimerAfterShow( _sender : TObject ) ;
    procedure previewChar( const _c : Char ) ;
    procedure doModalResult( _modal_result : TModalResult ) ;

  protected

    /// <inheritdoc/>
    procedure initForm     ; override;

    /// <inheritdoc/>
    procedure initControls ; override;

  public
    /// <summary>
    ///   Execute dialog on a given symbol.
    /// </summary>
    /// <param name="_symbol">
    ///   base symbol.
    /// </param>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button  will be
    ///   visible and help support will be enabled;
    /// </param>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute      ( const _symbol : TGIS_SymbolFont ;
                             const _onhelp : TGIS_HelpEvent  ;
                             const _proc   : TProc<TModalResult>
                           ) ; overload;

    /// <summary>
    ///   Execute dialog on a given symbol.
    /// </summary>
    /// <param name="_symbol">
    ///   base symbol.
    /// </param>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute      ( const _symbol : TGIS_SymbolFont ;
                             const _proc   : TProc<TModalResult>
                           ) ; overload;

  published

    /// <summary>
    ///   Chosen font symbol.
    /// </summary>
    property Symbol : TGIS_SymbolFont read FSymbol ;
  end ;


//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.FMX.GeoControlHelper ;

//==============================================================================
// Private events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.doTimerAfterShow(
    _sender : TObject
  ) ;
  begin
    tmrAfterShow.Enabled := False ;
    edtChar.Text := FLetter ;
  end;

  procedure TGIS_ControlLegendFNT.previewChar(
    const _c : Char
  ) ;
  var
    str : String ;
  begin
    lblPreview.Text := _c ;

    {$IFNDEF LEVEL_RX102_FMX}
      if _c = '&' then
        lblPreview.Text := lblPreview.Text + _c ;
    {$ENDIF}

    {$IFDEF GIS_MOBILE_DIALOGS}
      ShowOK := Ord( FLetter ) >= 32 ;
    {$ELSE}
      btnOK.Enabled := Ord( FLetter ) >= 32  ;
    {$ENDIF}
  end ;


  procedure TGIS_ControlLegendFNT.doModalResult(
    _modal_result : TModalResult
  ) ;
  var
    stmp  : String  ;
    schar : String  ;
  begin
    if _modal_result = mrOK then begin
      stmp := ConstructParamFontStyle( TGIS_FontStyles( lblPreview.Font.Style ) ) ;

      if FLetter = '|' then
        schar := IntToStr( Ord( FLetter ) )
      else
        schar := FLetter ;

      if not IsStringEmpty( stmp ) then
        stmp := lblPreview.Font.Family + ':' + schar + ':' + stmp
      else
        stmp := lblPreview.Font.Family + ':' + schar ;

      FSymbol := TGIS_SymbolFont( SymbolList.Prepare( stmp ) ) ;
    end ;

    if Assigned( pModalProc ) then
      pModalProc( _modal_result ) ;

  end ;


//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.FormShow(Sender: TObject);
  begin
    {$IFNDEF GIS_MOBILE_DIALOGS}
      edtChar.SetFocus ;
      edtChar.CaretPosition := 1 ;
      edtChar.Caret.Visible := False ;
      edtChar.ResetSelection ;
    {$ENDIF}
  end ;

//==============================================================================
// Controls events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.cmbFontsChange(Sender: TObject);
  begin
    if cmbFonts.ItemIndex < 0 then exit ;
    lblPreview.Font.Family := cmbFonts.Items[ cmbFonts.ItemIndex ] ;
    lblPreview.Font.Size := 80 ;
  end;

  procedure TGIS_ControlLegendFNT.edtCharKeyDown(
    Sender      : TObject ;
    var Key     : Word ;
    var KeyChar : Char ;
    Shift       : TShiftState
  ) ;
  begin
    if KeyChar < Char(32) then begin
      Key := 0 ;
      exit ;
    end;
    edtChar.Text := '' ;
  end;

  procedure TGIS_ControlLegendFNT.edtCharTyping(
    Sender : TObject
  ) ;
  var
    s : String ;
  begin
    s := edtChar.Text;
    if s.Length = 0 then exit ;
    edtChar.Text := s[s.Length];
  end;

  procedure TGIS_ControlLegendFNT.edtCharChange(Sender: TObject);
  var
    i : Integer ;
    bytes : TBytes ;
    event : TNotifyEvent ;
  begin
    if Length( edtChar.Text ) > 0 then
      FLetter := edtChar.Text[1]
    else
      FLetter := Char( 0 ) ;
    previewChar( FLetter ) ;

    event := edtUnicode.OnChangeTracking ;
    edtUnicode.OnChangeTracking := nil ;
    try
      if Length( edtChar.Text ) > 0 then begin
        bytes := TEncoding.Unicode.GetBytes( edtChar.Text ) ;
        edtUnicode.Text := Format( '%.2x', [ bytes[1] ] ) +
                           Format( '%.2x', [ bytes[0] ] ) ;
      end else
        edtUnicode.Text := Char(0) ;
    finally
      edtUnicode.OnChangeTracking := event ;
    end;
  end ;

  procedure TGIS_ControlLegendFNT.edtUnicodeEnter(Sender: TObject);
  begin
    {$IFNDEF ANDROID}
      // cursor at the end
      edtUnicode.CaretPosition := Length(edtUnicode.Text);
      edtUnicode.ResetSelection ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendFNT.edtUnicodeKeyDown(Sender: TObject; var Key: Word;
                             var KeyChar: Char; Shift: TShiftState);
  begin
    if KeyChar < Char(32) then exit ;
    if not ( ( ( KeyChar >= '0' ) and ( KeyChar <= '9' ) ) or
             ( ( KeyChar >= 'a' ) and ( KeyChar <= 'f' ) ) or
             ( ( KeyChar >= 'A' ) and ( KeyChar <= 'F' ) ) ) then
      KeyChar := Char(0) ;
    {$IFDEF ANDROID}
      if ( Length( edtUnicode.Text ) >= 5 ) and
    {$ELSE}
      if ( Length( edtUnicode.Text ) >= 4 ) and
    {$ENDIF}
       ( edtUnicode.SelLength = 0 ) then
      KeyChar := Char(0) ;
  end ;

  procedure TGIS_ControlLegendFNT.edtUnicodeTyping(Sender: TObject);
  var
    bytes : array[0..1] of Byte ;
    chars : TCharArray ;
    i : Integer ;
    txt : String ;
    s : String ;
  begin
    bytes[0] := 0 ;
    bytes[1] := 0 ;
    txt := edtUnicode.Text ;
    for i := 3 downto Length(txt) do begin
      txt := '0' + txt ;
    end;
    bytes[0] := StrToInt( '$' + txt[3] + txt[4] ) ;
    bytes[1] := StrToInt( '$' + txt[1] + txt[2] ) ;

    chars := TEncoding.Unicode.GetChars( bytes ) ;

    edtChar.OnChangeTracking := nil ;
    edtChar.Text := chars[0] ;
    edtChar.OnChangeTracking := edtCharChange ;

    if Length( edtChar.Text ) > 0 then
      FLetter := edtChar.Text[1]
    else
      FLetter := Char( 0 ) ;
    previewChar( FLetter ) ;
  end ;

  procedure TGIS_ControlLegendFNT.btnOKClick(Sender: TObject);
  begin
    if btnOK.Enabled then
      ModalResult := mrOK ;
  end ;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT ) ;
    Self.ClientHeight := frmClientHeight ;
    Self.ClientWidth := frmClientWidth ;

    Self.Name := 'TGIS_ControlLegendFNT' ;
    OnShow := FormShow ;
  end ;

  procedure TGIS_ControlLegendFNT.initControls ;
  var
    i    : Integer ;
    lst  : TSTringList ;
    x, y : Single ;
    h    : Single ;
  const
    {$IFDEF GIS_MOBILE_DIALOGS}
      CHECKBOX_WIDTH = 100 ;
    {$ELSE}
      CHECKBOX_WIDTH = 75 ;
    {$ENDIF}
  begin
    if _rsbidi then
      Self.BiDiMode := TBiDiMode.bdRightToLeft
    else
      Self.BiDiMode := TBiDiMode.bdLeftToRight ;

    lblFonts := TLabel.Create( oMainForm ) ;
    lblFonts.Parent := oMainForm ;
    lblFonts.Size.PlatformDefault := False ;
    lblFonts.TextSettings.WordWrap := False ;
    lblFonts.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_LBLFONT ) ;
    lblFonts.TabOrder := 0 ;
    lblFonts.FixSize ;

    cmbFonts := TComboBox.Create( oMainForm ) ;
    cmbFonts.Parent := oMainForm ;
    cmbFonts.Touch.InteractiveGestures := [TInteractiveGesture.LongTap, TInteractiveGesture.DoubleTap] ;
    cmbFonts.TabOrder := 0 ;
    cmbFonts.ItemIndex := -1 ;
    cmbFonts.ListBoxResource := 'combolistboxstyle' ;
    cmbFonts.Size.PlatformDefault := False ;
    cmbFonts.TabOrder := 1 ;
    cmbFonts.OnChange := cmbFontsChange ;

    chkBold := TCheckBox.Create( oMainForm ) ;
    chkBold.Parent := oMainForm ;
    chkBold.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_CHKBOLD ) ;
    chkBold.Font.Style := [TFontStyle.fsBold] ;
    chkBold.StyledSettings := chkBold.StyledSettings - [TStyledSetting.Style] ;
    chkBold.Size.PlatformDefault := False ;
    chkBold.TabOrder := 2 ;
    chkBold.OnChange := actStyleExecute ;

    chkItalic := TCheckBox.Create( oMainForm ) ;
    chkItalic.Parent := oMainForm ;
    chkItalic.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_CHKITALIC ) ;
    chkItalic.Font.Style := [TFontStyle.fsItalic] ;
    chkItalic.StyledSettings := chkItalic.StyledSettings - [TStyledSetting.Style] ;
    chkItalic.Size.PlatformDefault := False ;
    chkItalic.TabOrder := 3 ;
    chkItalic.OnChange := actStyleExecute ;

    chkStrikeout := TCheckBox.Create( oMainForm ) ;
    chkStrikeout.Parent := oMainForm ;
    chkStrikeout.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_STRIKEOUT ) ;
    chkStrikeout.Font.Style := [TFontStyle.fsStrikeOut] ;
    chkStrikeout.StyledSettings := chkStrikeout.StyledSettings - [TStyledSetting.Style] ;
    chkStrikeout.Size.PlatformDefault := False ;
    chkStrikeout.TabOrder := 4 ;
    chkStrikeout.OnChange := actStyleExecute ;

    chkUnderline := TCheckBox.Create( oMainForm ) ;
    chkUnderline.Parent := oMainForm ;
    chkUnderline.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_CHKUNDERLINE ) ;
    chkUnderline.Font.Style := [TFontStyle.fsUnderline] ;
    chkUnderline.StyledSettings := chkUnderline.StyledSettings - [TStyledSetting.Style] ;
    chkUnderline.Size.PlatformDefault := False ;
    chkUnderline.TabOrder := 5 ;
    chkUnderline.OnChange := actStyleExecute ;

    lblChar := TLabel.Create( oMainForm ) ;
    lblChar.Parent := oMainForm ;
    lblChar.Trimming := TTextTrimming.None ;
    lblChar.WordWrap := False ;
    lblChar.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_LETTER ) ;
    lblChar.TabOrder := 6 ;
    lblChar.FixSize ;

    edtChar := TEdit.Create( oMainForm ) ;
    edtChar.Parent := oMainForm ;
    edtChar.TabOrder := 7 ;
    edtChar.KillFocusByReturn := True ;
    edtChar.OnChangeTracking := edtCharChange ;
    edtChar.OnTyping := edtCharTyping ;
    edtChar.KillFocusByReturn := True ;

    lblCode := TLabel.Create( oMainForm ) ;
    lblCode.Parent := oMainForm ;
    lblCode.Trimming := TTextTrimming.None ;
    lblCode.WordWrap := False ;
    lblCode.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_CODE ) ;
    lblCode.TabOrder := 8 ;
    lblCode.FixSize ;

    edtUnicode := TEdit.Create( oMainForm ) ;
    edtUnicode.Parent := oMainForm ;
    edtUnicode.TabOrder := 10 ;
    edtUnicode.KillFocusByReturn := True ;
    edtUnicode.OnEnter := edtUnicodeEnter ;
    edtUnicode.OnKeyDown := edtUnicodeKeyDown ;
    edtUnicode.OnTyping := edtUnicodeTyping ;
    edtUnicode.KillFocusByReturn := True ;
    {$IFDEF ANDROID}
      edtUnicode.Hook ;
    {$ENDIF}

    lblUnicode := TLabel.Create( oMainForm ) ;
    lblUnicode.Parent := oMainForm ;
    lblUnicode.Trimming := TTextTrimming.None ;
    lblUnicode.WordWrap := False ;
    lblUnicode.TextAlign := TTextAlign.Trailing ;
    lblUnicode.Text := 'U+' ;
    lblUnicode.TabOrder := 9 ;
    lblUnicode.FixSize ;

    pnlPreview := TRectangle.Create( oMainForm ) ;
    pnlPreview.Parent := oMainForm ;
    pnlPreview.ClipChildren := True ;
    pnlPreview.Fill.Kind := TBrushKind.None ;
    pnlPreview.Stroke.Color := TAlphaColorRec.Darkgrey ;
    pnlPreview.TabOrder := 11 ;

    lblPreview := TLabel.Create( pnlPreview ) ;
    lblPreview.Parent := pnlPreview ;
    {$IFDEF LEVEL_RX102_FMX}
      lblPreview.PrefixStyle := TPrefixStyle.HidePrefix ;
    {$ENDIF}
    lblPreview.Trimming := TTextTrimming.None ;
    lblPreview.WordWrap := False ;
    lblPreview.StyledSettings := chkItalic.StyledSettings -
                                 [TStyledSetting.Style, TStyledSetting.Size,
                                  TStyledSetting.Family] ;
    lblPreview.Align := TAlignLayout.Client ;
    lblPreview.TextAlign := TTextAlign.Center ;
    lblPreview.VertTextAlign := TTextAlign.Center ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := assigned( pOnHelp ) ;
      btnHelp.TabOrder := 12 ;
      btnCancel.TabOrder := 14 ;
      btnOK.TabOrder := 13 ;
    {$ENDIF}

    y := 8 ;

    lblFonts.Position.Y := y ;
    PlaceControl( BiDiMode, nil, lblFonts, 8, -1 ) ;

    y := y + lblFonts.Height ;

    cmbFonts.Position.Y := y ;
    cmbFonts.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbFonts, 8, -1 ) ;

    x := 8 ;
    y := y + cmbFonts.Height + 4 ;

    h := cmbFonts.Height ;

    chkBold.Position.Y := y ;
    chkBold.Size.Height := h ;
    PlaceControl( BiDiMode, nil, chkBold, x, CHECKBOX_WIDTH ) ;

    x := x + chkBold.Width + 4 ;

    chkItalic.Position.Y := y ;
    chkItalic.Size.Height := h ;
    PlaceControl( BiDiMode, nil, chkItalic, x, CHECKBOX_WIDTH ) ;

    x := x + chkItalic.Width + 4 ;

    chkStrikeout.Position.Y := y ;
    chkStrikeout.Size.Height := h ;
    PlaceControl( BiDiMode, nil, chkStrikeout, x, CHECKBOX_WIDTH ) ;

    x := x + chkStrikeout.Width + 4 ;

    chkUnderline.Position.Y := y ;
    chkUnderline.Size.Height := h ;
    PlaceControl( BiDiMode, nil, chkUnderline, x, CHECKBOX_WIDTH ) ;

    y := y + chkUnderline.Height +4 ;

    pnlPreview.Position.Y := y ;
    pnlPreview.Size.Height := 153 ;
    PlaceControl( BiDiMode, nil, pnlPreview, -8, 153 ) ;

    x := x + chkUnderline.Width + 8 ;

    y := y + 20  ;
    lblChar.Position.Y := y ;
    PlaceControl( BiDiMode, nil, lblChar, 8, 60) ;

    edtChar.Position.Y := y + ( lblChar.Height - edtChar.Height ) / 2  ;
    PlaceControl( BiDiMode, nil, edtChar, 80, 50 ) ;

    lblChar.AlignVertically( edtChar  );

    y := y + edtChar.Height + 4 ;

    lblCode.Position.Y := y ;
    PlaceControl( BiDiMode, nil, lblCode, 8, 50 ) ;

    edtUnicode.Position.Y := y + ( lblCode.Height - edtUnicode.Height ) / 2 ;
    PlaceControl( BiDiMode, nil, edtUnicode, 80, 50 ) ;

    lblUnicode.Position.Y := y ;
    PlaceControl( BiDiMode, edtUnicode, lblUnicode, -1, 25 ) ;

    lblCode.AlignVertically( edtUnicode );
    lblUnicode.AlignVertically( edtUnicode );

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Size.Height := 24 ;
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
    {$ENDIF}

    y := pnlPreview.Position.Y + pnlPreview.Height ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      y := y + 8 ;
    {$ELSE}
      y := y + 16 ;
    {$ENDIF}

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Size.Height := 24 ;
      btnHelp.Position.Y := y ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;

      btnCancel.Size.Height := 24 ;
      btnCancel.Position.Y := y ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;

      btnOK.Size.Height := 24 ;
      btnOK.Position.Y := y ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
    {$ENDIF}

    {$IFNDEF GIS_MOBILE_DIALOGS}
      y := y + btnOK.Height + 8 ;
    {$ENDIF}

    ClientWidth  := 4 * CHECKBOX_WIDTH + 3 * 4 + 2 * 8 ;
    ClientHeight := RoundS( y ) ;

    lst := TStringList.Create ;
    try
      GetFontList( lst ) ;
      cmbFonts.Items.Assign( lst ) ;
    finally
      FreeObject( lst ) ;
    end;

    tmrAfterShow := TTimer.Create( oMainForm );
    tmrAfterShow.Interval := 100 ;
    tmrAfterShow.OnTimer := doTimerAfterShow ;
  end ;

  procedure TGIS_ControlLegendFNT.Execute(
    const _symbol : TGIS_SymbolFont ;
    const _onhelp : TGIS_HelpEvent  ;
    const _proc   : TProc<TModalResult>
  ) ;
  var
    i : Integer ;
  begin
    pModalProc := _proc ;
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    FSymbol := _symbol ;
    if Assigned( Symbol ) then begin
      cmbFonts.ItemIndex := -1 ;
      for i:=0 to cmbFonts.Items.Count - 1 do
        if CompareText( cmbFonts.Items[i], Symbol.Font.Name ) = 0 then begin
          cmbFonts.ItemIndex := i ;
          break ;
        end ;

      FLetter := Symbol.Char ;

      chkBold.IsChecked       := TGIS_FontStyle.Bold      in Symbol.Font.Style ;
      chkItalic.IsChecked     := TGIS_FontStyle.Italic    in Symbol.Font.Style ;
      chkUnderline.IsChecked  := TGIS_FontStyle.Underline in Symbol.Font.Style ;
      chkStrikeout.IsChecked  := TGIS_FontStyle.Strikeout in Symbol.Font.Style ;
    end else begin
      cmbFonts.ItemIndex := -1 ;
      for i:=0 to cmbFonts.Items.Count - 1 do
        if CompareText( cmbFonts.Items[i], 'Arial' ) = 0 then begin
          cmbFonts.ItemIndex := i ;
          break ;
        end ;

      FLetter := 'A' ;
    end ;

    if cmbFonts.ItemIndex < 0 then  cmbFonts.ItemIndex := 0 ;
    cmbFontsChange( nil ) ;
    actStyleExecute( nil ) ;

//    edtChar.Text := FLetter ;
    actListUpdate( nil ) ;

    ShowModalEx( doModalResult ) ;
  end ;

  procedure TGIS_ControlLegendFNT.Execute(
    const _symbol : TGIS_SymbolFont ;
    const _proc   : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _symbol, hlp, _proc ) ;
  end;

//==============================================================================
// Actions
//==============================================================================

  procedure TGIS_ControlLegendFNT.actStyleExecute(Sender: TObject);
  begin
    with lblPreview.Font do begin
      if chkBold.IsChecked      then Style := Style + [TFontStyle.fsBold     ]
                                else Style := Style - [TFontStyle.fsBold     ] ;
      if chkItalic.IsChecked    then Style := Style + [TFontStyle.fsItalic   ]
                                else Style := Style - [TFontStyle.fsItalic   ] ;
      if chkUnderline.IsChecked then Style := Style + [TFontStyle.fsUnderline]
                                else Style := Style - [TFontStyle.fsUnderline] ;
      if chkStrikeout.IsChecked then Style := Style + [TFontStyle.fsStrikeout]
                                else Style := Style - [TFontStyle.fsStrikeout] ;
    end ;
  end;

  procedure TGIS_ControlLegendFNT.actListUpdate(Sender: TObject);
  begin
    {$IFDEF GIS_MOBILE_DIALOGS}
      ShowOk := not IsStringEmpty( FLetter ) ;
    {$ELSE}
      btnOK.Enabled := not IsStringEmpty( FLetter ) ;
    {$ENDIF}
  end;


//==================================== END =====================================
end.

