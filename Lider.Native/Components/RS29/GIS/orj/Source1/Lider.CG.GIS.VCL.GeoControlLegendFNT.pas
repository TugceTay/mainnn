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

unit Lider.CG.GIS.VCL.GeoControlLegendFNT ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlLegendFNT"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  System.SysUtils,
  VCL.Graphics,
  VCL.Forms,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Controls,
  VCL.ActnList,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.VCL.GeoModalForm;

type

  /// <summary>
  ///   Control for internal use only.
  /// </summary>
  TGIS_LetterEdit = class(TEdit)
    private
      procedure set_focus ( var msg : TWMSetFocus ); message WM_SetFocus ;
  end ;

  /// <summary>
  ///   Visual form for managing font symbols.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlLegendFNT = class( TGIS_ModalForm )
    lblFonts: TLabel;
    cmbFonts: TComboBox;
    lblChar: TLabel;
    edtChar: TGIS_LetterEdit;
    lblCode: TLabel;
    lblUnicode: TLabel;
    edtUnicode: TEdit;
    pnlPreview: TPanel;
    chkStrikeout: TCheckBox;
    chkBold: TCheckBox;
    chkItalic: TCheckBox;
    chkUnderline: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure cmbFontsChange(Sender: TObject);
    procedure edtCharChange(Sender: TObject);
    procedure edtCharKeyPress(Sender: TObject; var Key: Char);
    procedure edtUnicodeKeyPress(Sender: TObject; var Key: Char);
    procedure edtUnicodeEnter(Sender: TObject);
    procedure edtUnicodeChange(Sender: TObject);
    procedure actStyleExecute(Sender: TObject);
    procedure actListUpdate(Sender: TObject);
  private
    FSymbol : TGIS_SymbolFont ;
    FLetter : Char ;
    procedure previewChar( const _c : Char ) ;

  protected

    /// <inheritdoc/>
    procedure initForm        ; override;

    /// <inheritdoc/>
    procedure initControls    ; override;

    /// <inheritdoc/>
    procedure showForm        ; override;

    /// <inheritdoc/>
    procedure afterPPIChanged ; override;

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
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute           ( const _symbol : TGIS_SymbolFont ;
                                 const _onhelp : TGIS_HelpEvent
                               ) : Integer ; overload;

    /// <summary>
    ///   Execute dialog on a given symbol.
    /// </summary>
    /// <param name="_symbol">
    ///   base symbol.
    /// </param>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute           ( const _symbol : TGIS_SymbolFont
                               ) : Integer ; overload;
  published

    /// <summary>
    ///   Selected Symbol or nil.
    /// </summary>
    property Symbol : TGIS_SymbolFont read FSymbol ;
  end;

//##############################################################################
implementation

uses
  {$IFDEF LEVEL_XE2_RTL}
    System.UITypes,
  {$ENDIF}

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.VCL.GeoControlHelper ;

//==============================================================================
// callbacks
//==============================================================================

  function enum_fonts(
     var _lf        : TLogFont    ;
     var _tm        : TTextMetric ;
         _font_type : Integer     ;
         _data      : Pointer
  ) : Integer ; stdcall;
  begin
      with TStrings( _data) do begin
        if _font_type and TRUETYPE_FONTTYPE > 0 then
          AddObject(  StrPas( _lf.lfFaceName ), Pointer( _lf.lfCharSet ) ) ;
        Result:= 1;
      end ;
  end;

  procedure TGIS_LetterEdit.set_focus ( var msg : TWMSetFocus );
  begin
    inherited ;
    HideCaret( Handle ) ;
  end ;

//==============================================================================
// Private events & methods
//==============================================================================

 procedure TGIS_ControlLegendFNT.previewChar( const _c : Char ) ;
 begin
    pnlPreview.Caption := _c ;

    if _c = '&' then
      pnlPreview.Caption := pnlPreview.Caption + _c ;
 end ;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.FormCreate(Sender: TObject);
  var
    i  : Integer ;
    lst : TStringList ;
  begin
    lst := TStringList.Create ;
    try
      GetFontList( lst ) ;
      cmbFonts.Items.Assign( lst );
      if lst.Count > 0 then
        cmbFonts.ItemIndex := 0;
    finally
      FreeObject( lst ) ;
    end;
  end;

//==============================================================================
// Controls events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.cmbFontsChange(Sender: TObject);
  begin
    if cmbFonts.ItemIndex < 0 then exit ;
    pnlPreview.Font.Name := cmbFonts.Items[ cmbFonts.ItemIndex ] ;
    pnlPreview.Font.Size := 60 *2 ;
  end;

  procedure TGIS_ControlLegendFNT.edtCharChange(Sender: TObject);
  var
    i : Integer ;
    bytes : TBytes ;
  begin
    if Length( edtChar.Text ) > 0 then
      FLetter := edtChar.Text[1]
    else
      FLetter := Char( 0 ) ;
    previewChar( FLetter ) ;

    edtUnicode.OnChange := nil ;
    if Length( edtChar.Text ) > 0 then begin
      bytes := TEncoding.Unicode.GetBytes( edtChar.Text ) ;
      edtUnicode.Text := Format( '%.2x', [ bytes[1] ] ) +
                         Format( '%.2x', [ bytes[0] ] ) ;
    end else
      edtUnicode.Text := Char(0) ;
    edtUnicode.OnChange := edtUnicodeChange ;
  end;

  procedure TGIS_ControlLegendFNT.edtCharKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key < Char(32) then exit ;
    edtChar.Text := '';
  end;

  procedure TGIS_ControlLegendFNT.edtUnicodeEnter(Sender: TObject);
  begin
    // cursor at the end
    edtUnicode.SelStart := Length(edtUnicode.Text);
    edtUnicode.SelLength := 0 ;
  end ;

  procedure TGIS_ControlLegendFNT.edtUnicodeKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key < Char(32) then exit ;
    if not ( ( ( Key >= '0' ) and ( Key <= '9' ) ) or
             ( ( Key >= 'a' ) and ( Key <= 'f' ) ) or
             ( ( Key >= 'A' ) and ( Key <= 'F' ) ) ) then
      Key := Char(0) ;
    if ( Length( edtUnicode.Text ) = 4 ) and
       ( edtUnicode.SelLength = 0 ) then
      Key := Char(0) ;
  end ;

  procedure TGIS_ControlLegendFNT.edtUnicodeChange(Sender: TObject);
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

    edtChar.OnChange := nil ;
    edtChar.Text := chars[0] ;
    edtChar.OnChange := edtCharChange ;

    if Length( edtChar.Text ) > 0 then
      FLetter := edtChar.Text[1]
    else
      FLetter := Char( 0 ) ;
    previewChar( FLetter ) ;
  end ;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendFNT.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT ) ;
    Self.ClientHeight := 282 ;
    Self.ClientWidth := 282 ;
    Self.Name := 'TGIS_ControlLegendFNT' ;
    Self.OnCreate := FormCreate ;
  end ;

  procedure TGIS_ControlLegendFNT.initControls ;
  var
    anchors   : TAnchors ;
    anchorsR  : TAnchors ;
    anchorsLR : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then begin
      anchors  := [akRight, akTop] ;
      anchorsR := [akLeft, akTop] ;
    end else begin
      anchors  := [akLeft, akTop] ;
      anchorsR := [akRight, akTop] ;
    end ;
    anchorsLR  := [akLeft, akTop, akRight] ;

    lblFonts := TLabel.Create( Self ) ;
    lblFonts.Parent := Self ;
    lblFonts.Top := 8 ;
    lblFonts.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblFonts, 8, -1 ) ;
    lblFonts.Anchors := anchorsLR ;
    lblFonts.AutoSize := False ;
    lblFonts.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_LBLFONT ) ;
    lblFonts.FocusControl := cmbFonts ;

    cmbFonts := TComboBox.Create( Self ) ;
    cmbFonts.Parent := Self ;
    cmbFonts.Anchors := anchorsLR ;
    cmbFonts.Top := 24 ;
    cmbFonts.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbFonts, 8, -1 ) ;
    cmbFonts.Sorted := True ;
    cmbFonts.Style := csDropDownList ;
    cmbFonts.TabOrder := 0 ;
    cmbFonts.Text := 'cmbFonts' ;
    cmbFonts.OnChange := cmbFontsChange ;

    chkBold := TCheckBox.Create( Self ) ;
    chkBold.Parent := Self ;
    chkBold.Anchors := anchors ;
    chkBold.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CHKBOLD ) ;
    chkBold.Font.Style := [TFontStyle.fsBold] ;
    chkBold.Top := 53 ;
    chkBold.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkBold, 8, 70 ) ;
    chkBold.TabOrder := 1 ;
    chkBold.OnClick := actStyleExecute ;

    chkItalic := TCheckBox.Create( Self ) ;
    chkItalic.Parent := Self ;
    chkItalic.Anchors := anchors ;
    chkItalic.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CHKITALIC ) ;
    chkItalic.Font.Style := [TFontStyle.fsItalic] ;
    chkItalic.Top := 53 ;
    chkItalic.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkItalic, 73, 70 ) ;
    chkItalic.TabOrder := 2 ;
    chkItalic.OnClick := actStyleExecute ;

    chkStrikeout := TCheckBox.Create( Self ) ;
    chkStrikeout.Parent := Self ;
    chkStrikeout.Anchors := anchors ;
    chkStrikeout.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_STRIKEOUT ) ;
    chkStrikeout.Font.Style := [TFontStyle.fsStrikeout];
    chkStrikeout.Top := 53 ;
    chkStrikeout.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStrikeout, 138, 70 ) ;
    chkStrikeout.TabOrder := 3 ;
    chkStrikeout.OnClick := actStyleExecute ;

    chkUnderline := TCheckBox.Create( Self ) ;
    chkUnderline.Parent := Self ;
    chkUnderline.Anchors := anchors ;
    chkUnderline.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CHKUNDERLINE ) ;
    chkUnderline.Font.Style := [TFontStyle.fsUnderline] ;
    chkUnderline.Top := 53 ;
    chkUnderline.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkUnderline, 203, 70 ) ;
    chkUnderline.TabOrder := 4 ;
    chkUnderline.OnClick := actStyleExecute ;

    lblChar := TLabel.Create( Self ) ;
    lblChar.Parent := Self ;
    lblChar.Anchors := anchors ;
    lblChar.Top := 101 ;
    lblChar.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblChar, 8, 45 ) ;
    lblChar.AutoSize := False ;
    lblChar.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_LETTER ) ;
    lblChar.FocusControl := edtChar ;

    edtChar := TGIS_LetterEdit.Create( Self ) ;
    edtChar.Parent := Self ;
    edtChar.Anchors := anchors ;
    edtChar.AutoSelect := False ;
    edtChar.Top := 98 ;
    edtChar.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtChar, 58, 40 ) ;
    edtChar.AutoSize := False ;
    edtChar.TabOrder := 5 ;
    edtChar.OnChange := edtCharChange ;
    edtChar.OnKeyPress := edtCharKeyPress ;

    lblCode := TLabel.Create( Self ) ;
    lblCode.Parent := Self ;
    lblCode.Anchors := anchors ;
    lblCode.Top := 126 ;
    lblCode.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblCode, 8, 30 ) ;
    lblCode.AutoSize := False ;
    lblCode.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CODE ) ;
    lblCode.FocusControl := edtUnicode ;

    edtUnicode := TEdit.Create( Self ) ;
    edtUnicode.Parent := Self ;
    edtUnicode.Anchors := anchors ;
    edtUnicode.AutoSelect := False ;
    edtUnicode.Top := 123 ;
    edtUnicode.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtUnicode, 58, 40 ) ;
    edtUnicode.AutoSize := False ;
    edtUnicode.TabOrder := 6 ;
    edtUnicode.OnChange := edtUnicodeChange ;
    edtUnicode.OnEnter := edtUnicodeEnter ;
    edtUnicode.OnKeyPress := edtUnicodeKeyPress ;

    lblUnicode := TLabel.Create( Self ) ;
    lblUnicode.Parent := Self ;
    lblUnicode.Anchors := anchors ;
    lblUnicode.Caption := 'U+' ;
    lblUnicode.Top := 126 ;
    lblUnicode.Height := 13 ;
    PlaceControl( BiDiMode, edtUnicode, lblUnicode, -2, lblUnicode.Width ) ;
    lblUnicode.AutoSize := False ;
    lblUnicode.FocusControl := edtUnicode ;

    pnlPreview := TPanel.Create( Self ) ;
    pnlPreview.Parent := Self ;
    pnlPreview.Anchors := anchorsR ;
    pnlPreview.Top := 78 ;
    pnlPreview.Height := 153 ;
    PlaceControl( BiDiMode, nil, pnlPreview, -8, 162 ) ;
    pnlPreview.BevelInner := bvRaised ;
    pnlPreview.BevelOuter := bvLowered ;
    pnlPreview.Font.Charset := DEFAULT_CHARSET ;
    pnlPreview.Font.Color := clWindowText ;
    pnlPreview.Font.Height := -11 ;
    pnlPreview.Font.Name := 'Tahoma' ;
    pnlPreview.Font.Style := [] ;
    pnlPreview.ParentFont := False ;
    pnlPreview.TabOrder := 7 ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 8 ;
    btnCancel.TabOrder := 10 ;
    btnOK.TabOrder := 9 ;
  end ;

  procedure TGIS_ControlLegendFNT.showForm ;
  begin
    edtChar.SetFocus ;
  end ;

  procedure TGIS_ControlLegendFNT.afterPPIChanged ;
  begin

  end ;

  function TGIS_ControlLegendFNT.Execute(
    const _symbol : TGIS_SymbolFont ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    i     : Integer ;
    stmp  : String  ;
    schar : String  ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    FSymbol := _symbol ;
    if Assigned( Symbol ) then begin
      cmbFonts.ItemIndex := -1 ;
      for i:=0 to cmbFonts.Items.Count - 1 do
        if CompareText( cmbFonts.Items[i], Symbol.Font.Name ) = 0 then begin
          cmbFonts.ItemIndex := i ;
          break ;
        end ;

      FLetter := Symbol.Char ;

      chkBold.Checked       := TGIS_FontStyle.Bold      in Symbol.Font.Style ;
      chkItalic.Checked     := TGIS_FontStyle.Italic    in Symbol.Font.Style ;
      chkUnderline.Checked  := TGIS_FontStyle.Underline in Symbol.Font.Style ;
      chkStrikeout.Checked  := TGIS_FontStyle.Strikeout in Symbol.Font.Style ;
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
    edtChar.Text := FLetter ;
    actListUpdate( nil ) ;

    Result := ShowModal ;

    if Result = mrOK then begin
      stmp := ConstructParamFontStyle( TGIS_FontStyles( pnlPreview.Font.Style ) ) ;

      if FLetter = '|' then
        schar := IntToStr( Ord( FLetter ) )
      else
        schar := FLetter ;

      if not IsStringEmpty( stmp ) then
        stmp := pnlPreview.Font.Name + ':' + schar + ':' + stmp
      else
        stmp := pnlPreview.Font.Name + ':' + schar ;

      FSymbol := TGIS_SymbolFont( SymbolList.Prepare( stmp ) ) ;
    end ;
  end ;

  function TGIS_ControlLegendFNT.Execute(
    const _symbol : TGIS_SymbolFont
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _symbol, hlp ) ;
  end;

//==============================================================================
// Actions
//==============================================================================

  procedure TGIS_ControlLegendFNT.actStyleExecute(Sender: TObject);
  begin
      with pnlPreview.Font do begin
        if chkBold.Checked      then Style := Style + [TFontStyle.fsBold     ]
                                else Style := Style - [TFontStyle.fsBold     ] ;
        if chkItalic.Checked    then Style := Style + [TFontStyle.fsItalic   ]
                                else Style := Style - [TFontStyle.fsItalic   ] ;
        if chkUnderline.Checked then Style := Style + [TFontStyle.fsUnderline]
                                else Style := Style - [TFontStyle.fsUnderline] ;
        if chkStrikeout.Checked then Style := Style + [TFontStyle.fsStrikeout]
                                else Style := Style - [TFontStyle.fsStrikeout] ;
      end ;
  end;

  procedure TGIS_ControlLegendFNT.actListUpdate(Sender: TObject);
  begin
    btnOK.Enabled := not IsStringEmpty( FLetter ) ;
  end;

//==================================== END =====================================
end.

