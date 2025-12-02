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

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlFont ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlFont"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK.PVL ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk.pvl ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface
  uses
  {$IFDEF CLR}
    TatukGIS.ndk,
    System.Drawing.Text,
    System.Drawing,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Classes,
    System.SysUtils,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.PVL.GeoPvl,
    Lider.CG.GIS.PVL.GeoPvlForms;
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
  ///   Visual form for managing font symbols.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlFont = class( TGIS_PvlModalForm )
  private
    lblFonts          : TGIS_PvlLabel         ;
    cmbFonts          : TGIS_PvlComboBox      ;
    lblChar           : TGIS_PvlLabel         ;
    edtChar           : TGIS_PvlEdit          ;
    lblCode           : TGIS_PvlLabel         ;
    lblUnicode        : TGIS_PvlLabel         ;
    edtUnicode        : TGIS_PvlEdit          ;
    pnlPreview        : TGIS_PvlPreviewPanel  ;
    chkStrikeout      : TGIS_PvlCheckBox      ;
    chkBold           : TGIS_PvlCheckBox      ;
    chkItalic         : TGIS_PvlCheckBox      ;
    chkUnderline      : TGIS_PvlCheckBox      ;
    mProc             : TGIS_Proc             ;
  private
    FSymbol           : TGIS_SymbolFont       ;
    FLetter           : Char                  ;
  private
    procedure cmbFontsChange      ( Sender: TObject                     ) ;
    procedure edtCharChange       ( Sender: TObject                     ) ;
    procedure edtCharKeyPress     (     Sender  : TObject ;
                                    var Key     : Char
                                  );
    procedure edtUnicodeKeyPress  (     Sender  : TObject;
                                    var Key     : Char
                                  ) ;
    procedure edtUnicodeChange    ( Sender: TObject                     ) ;
    procedure actStyleExecute     ( Sender: TObject                     ) ;
    procedure actListUpdate       ( Sender: TObject                     ) ;
    procedure initFontCombo                                               ;
    procedure previewChar         ( const _c : Char                     ) ;
    procedure doModalResult       ( _modal_result : TGIS_PvlModalResult ) ;
  public

    /// <inheritdoc/>
    procedure DoInitForm          ; override;

    /// <inheritdoc/>
    procedure DoInitControls      ; override;

  public

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute              : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute              ( const _proc   : TGIS_Proc
                                  ) : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog on a given symbol.
    /// </summary>
    /// <param name="_symbol">
    ///   base symbol.
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute              ( const _symbol : TGIS_SymbolFont ;
                                    const _proc   : TGIS_Proc
                                  ) : TGIS_PvlModalResult; overload;

    /// <summary>
    ///   Execute dialog on a given symbol.
    /// </summary>
    /// <param name="_font">
    ///   base font.
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute              ( const _font   : TGIS_Font ;
                                    const _proc   : TGIS_Proc
                                  ) : TGIS_PvlModalResult; overload;

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
    ///   Action to be performed after closing modal form or nil.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute              ( const _symbol : TGIS_SymbolFont ;
                                    const _onhelp : TGIS_HelpEvent ;
                                    const _proc   : TGIS_Proc
                                  ) : TGIS_PvlModalResult; overload;
    protected
      /// <inheritdoc/>
      procedure doDestroy ; override ;

  published
    /// <summary>
    ///   Selected Symbol or nil.
    /// </summary>
    property Symbol : TGIS_SymbolFont
                                  read  FSymbol ;
  end;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    {$IFDEF LEVEL_XE2_RTL}
      System.UITypes,
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoParams;
{$ENDIF}

//==============================================================================
// Private events & methods
//==============================================================================

 procedure TGIS_ControlFont.previewChar( const _c : Char ) ;
 begin
    pnlPreview.Caption := _c ;

    if _c = '&' then
      pnlPreview.Caption := pnlPreview.Caption + _c ;
 end ;

//==============================================================================
// Controls events & methods
//==============================================================================

  procedure TGIS_ControlFont.initFontCombo ;
  var
    lst : TStringList ;
    itm : String ;
    i   : Integer ;
  begin
    try
      lst := Context.GetAllFonts ;
      for i := 0 to lst.Count - 1 do begin
        itm := lst.Strings[i] ;
        cmbFonts.ItemsAdd( itm ) ;
      end;
    finally
      FreeObject( lst ) ;
    end;
  end;

  procedure TGIS_ControlFont.cmbFontsChange(Sender: TObject);
  begin
    if cmbFonts.ItemIndex < 0 then exit ;
    pnlPreview.FontFamily := cmbFonts.Item[ cmbFonts.ItemIndex ] ;
    pnlPreview.FontSize := 50 * 2 ;
  end;

  procedure TGIS_ControlFont.edtCharChange(Sender: TObject);
  var
    bytes : TBytes ;
  begin
    if length( edtChar.Text ) > 0 then
      FLetter := edtChar.Text[StringFirst]
    else
      FLetter := Char( 0 ) ;
    previewChar( FLetter ) ;

    edtUnicode.OnChange := nil ;
    if length( edtChar.Text ) > 0 then begin
      {$IFNDEF JAVA}
        bytes := TEncoding.Unicode.GetBytes( edtChar.Text ) ;
      {$ELSE}
        bytes := TEncoding.UTF16LE.GetBytes( edtChar.Text ) ;
      {$ENDIF}
      edtUnicode.Text := Format( '%.2x', [ bytes[1] ] ) +
                         Format( '%.2x', [ bytes[0] ] ) ;
    end else
      edtUnicode.Text := Char(0) ;
    {$IFDEF DCC}
      edtUnicode.OnChange := edtUnicodeChange ;
    {$ELSE}
      edtUnicode.OnChange := @edtUnicodeChange ;
    {$ENDIF}
  end;

  procedure TGIS_ControlFont.edtCharKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key < Char(32) then begin
      Key := Char(0) ;
      exit ;
    end;
    edtChar.Text := '';
  end;

  procedure TGIS_ControlFont.edtUnicodeKeyPress(Sender: TObject; var Key: Char);
  begin
    if Key < Char(32) then begin
      Key := Char(0) ;
      exit ;
    end;

    if not ( ( ( Key >= '0' ) and ( Key <= '9' ) ) or
             ( ( Key >= 'a' ) and ( Key <= 'f' ) ) or
             ( ( Key >= 'A' ) and ( Key <= 'F' ) ) ) then
      Key := Char(0) ;
    if ( length( edtUnicode.Text ) = 4 ) and
       ( edtUnicode.SelectionLength = 0 ) then
      Key := Char(0) ;
  end ;

  procedure TGIS_ControlFont.edtUnicodeChange(Sender: TObject);
  var
    bytes : array[0..1] of Byte ;
    {$IFDEF DCC}
    chars : TCharArray ;
    {$ELSE}
    chars : array of Char ;
    {$ENDIF}
    i : Integer ;
    txt : String ;
  begin
    bytes[0] := 0 ;
    bytes[1] := 0 ;
    txt := edtUnicode.Text ;

    for i := 3 downto length(txt) do begin
      txt := '0' + txt ;
    end;

    bytes[0] := StrToInt( '$' + txt[ StringFirst + 2 ] + txt[ StringFirst + 3 ] ) ;
    bytes[1] := StrToInt( '$' + txt[ StringFirst ] + txt[ StringFirst + 1 ] ) ;

    {$IFNDEF JAVA}
      chars := TEncoding.Unicode.GetChars( bytes ) ;
    {$ELSE}
      chars := TEncoding.UTF16LE.GetString( bytes ).ToCharArray ;
    {$ENDIF}

    edtChar.OnChange := nil ;
    edtChar.Text := chars[0] ;
    {$IFDEF DCC}
      edtChar.OnChange := edtCharChange ;
    {$ELSE}
      edtChar.OnChange := @edtCharChange ;
    {$ENDIF}

    if length( edtChar.Text ) > 0 then
      FLetter := edtChar.Text[StringFirst]
    else
      FLetter := Char( 0 ) ;
    previewChar( FLetter ) ;
  end ;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlFont.DoInitForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT ) ;
    Self.ClientHeight := 262 ;
    Self.ClientWidth := 282 ;
    Self.Name := 'TGIS_ControlFont' ;
  end ;

  procedure TGIS_ControlFont.DoInitControls ;
  var
    ytop : Integer ;
  begin
    FSymbol := TGIS_SymbolFont( SymbolList.Prepare( 'Arial:g' ) );

    ytop := Context.VMargin ;

    lblFonts := TGIS_PvlLabel.Create( Context ) ;
    lblFonts.Place( -1, 0, nil, Context.HMargin, nil, ytop ) ;
    lblFonts.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_LBLFONT ) ;

    ytop := ytop + lblFonts.Height + Context.LSpace ;

    cmbFonts := TGIS_PvlComboBox.Create( Context ) ;
    cmbFonts.Place( -Context.HMargin, 0, nil, Context.HMargin, nil, ytop ) ;
    cmbFonts.TabOrder := 0 ;
    cmbFonts.Sorted := True ;
    cmbFonts.Text := 'cmbFonts' ;
    {$IFDEF DCC}
      cmbFonts.OnChange := cmbFontsChange ;
    {$ELSE}
      cmbFonts.OnChange := @cmbFontsChange ;
    {$ENDIF}

    pnlPreview := TGIS_PvlPreviewPanel.Create( Context ) ;
    pnlPreview.Place( 150, 150, nil, -Context.HMargin, nil, cmbFonts.Top + cmbFonts.Height + BtnOK.Height div 2) ;
    pnlPreview.Border := True ;
    pnlPreview.TabOrder := 7 ;
    pnlPreview.FontFamily := 'Tahoma' ;
    pnlPreview.FontSize := 24 ;

    chkBold := TGIS_PvlCheckBox.Create( Context ) ;
    chkBold.Place( 100, 18, nil, Context.HMargin, nil, pnlPreview.Top ) ;
    chkBold.FontStyle := GisAddFontStyle( chkBold.FontStyle, TGIS_FontStyle.Bold ) ;
    chkBold.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CHKBOLD ) ;
    chkBold.TabOrder := 1 ;
    {$IFDEF DCC}
      chkBold.OnClick := actStyleExecute ;
    {$ELSE}
      chkBold.OnClick := @actStyleExecute ;
    {$ENDIF}

    chkItalic := TGIS_PvlCheckBox.Create( Context ) ;
    chkItalic.Place( 100, 18, nil, Context.HMargin, chkBold, 0 ) ;
    chkItalic.FontStyle := GisAddFontStyle( chkItalic.FontStyle, TGIS_FontStyle.Italic ) ;
    chkItalic.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CHKITALIC ) ;
    chkItalic.TabOrder := 2 ;
    {$IFDEF DCC}
      chkItalic.OnClick := actStyleExecute ;
    {$ELSE}
      chkItalic.OnClick := @actStyleExecute ;
    {$ENDIF}

    chkStrikeout := TGIS_PvlCheckBox.Create( Context ) ;
    chkStrikeout.Place( 100, 18, nil, Context.HMargin, chkItalic, 0 ) ;
    chkStrikeout.FontStyle := GisAddFontStyle( chkStrikeout.FontStyle, TGIS_FontStyle.StrikeOut ) ;
    chkStrikeout.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_STRIKEOUT ) ;
    chkStrikeout.TabOrder := 3 ;
    {$IFDEF DCC}
      chkStrikeout.OnClick := actStyleExecute ;
    {$ELSE}
      chkStrikeout.OnClick := @actStyleExecute ;
    {$ENDIF}

    chkUnderline := TGIS_PvlCheckBox.Create( Context ) ;
    chkUnderline.Place( 100, 18, nil, Context.HMargin, chkStrikeout, 0 ) ;
    chkUnderline.FontStyle := GisAddFontStyle( chkUnderline.FontStyle, TGIS_FontStyle.Underline ) ;
    chkUnderline.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_CHKUNDERLINE ) ;
    chkUnderline.TabOrder := 4 ;
    {$IFDEF DCC}
      chkUnderline.OnClick := actStyleExecute ;
    {$ELSE}
      chkUnderline.OnClick := @actStyleExecute ;
    {$ENDIF}

    lblCode := TGIS_PvlLabel.Create( Context ) ;
    lblCode.Place( 45, 20, nil, Context.HMargin, nil, pnlPreview.Top + pnlPreview.Height - 20 ) ;
    lblCode.Caption :=  _rsrc( GIS_RS_LEGEND_DLGFONT_CODE ) ;

    edtUnicode := TGIS_PvlEdit.Create( Context ) ;
    edtUnicode.Place( 40, 20, pnlPreview, -Context.HSpace, nil, lblCode.Top ) ;
    edtUnicode.TabOrder := 6 ;

    lblUnicode := TGIS_PvlLabel.Create( Context ) ;
    lblUnicode.Place( 20, 20, edtUnicode, -Context.HSpace, nil, lblCode.Top ) ;
    lblUnicode.Caption := 'U+' ;

    lblChar := TGIS_PvlLabel.Create( Context ) ;
    lblChar.Place( 45, 20, nil, Context.HMargin, nil, lblCode.Top -lblCode.Height - Context.VSpace ) ;
    lblChar.Caption := _rsrc( GIS_RS_LEGEND_DLGFONT_LETTER ) ;

    edtChar := TGIS_PvlEdit.Create( Context ) ;
    edtChar.Place( 40, 20, pnlPreview, -Context.HSpace, nil, lblChar.Top ) ;
    edtChar.TabOrder := 5 ;
    {$IFDEF DCC}
      edtChar.OnChange := edtCharChange ;
      edtChar.OnKeyPress := edtCharKeyPress ;
    {$ELSE}
      edtChar.OnChange := @edtCharChange ;
      edtChar.OnKeyPress := @edtCharKeyPress ;
    {$ENDIF}

    {$IFDEF DCC}
      edtUnicode.OnChange := edtUnicodeChange ;
      //?edtUnicode.OnEnter := edtUnicodeEnter ;
      edtUnicode.OnKeyPress := edtUnicodeKeyPress ;
    {$ELSE}
      edtUnicode.OnChange := @edtUnicodeChange ;
      //?edtUnicode.OnEnter := @edtUnicodeEnter ;
      edtUnicode.OnKeyPress := @edtUnicodeKeyPress ;
    {$ENDIF}



    lblFonts.FocusControl := cmbFonts ;
    lblChar.FocusControl := edtChar ;
    lblCode.FocusControl := edtUnicode ;
    lblUnicode.FocusControl := edtUnicode ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;
    BtnHelp.TabOrder := 8 ;
    BtnCancel.TabOrder := 10 ;
    BtnOK.TabOrder := 9 ;

    initFontCombo ;
  end ;

  function TGIS_ControlFont.Execute(
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( TGIS_SymbolFont( nil ), TGIS_HelpEvent(nil), nil ) ;
  end;

  function TGIS_ControlFont.Execute(
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( TGIS_SymbolFont( nil ), TGIS_HelpEvent(nil), _proc ) ;
  end;

  function TGIS_ControlFont.Execute(
    const _symbol : TGIS_SymbolFont ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( _symbol, TGIS_HelpEvent(nil), _proc ) ;
  end;

  function TGIS_ControlFont.Execute(
    const _font : TGIS_Font ;
    const _proc : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    stmp  : String  ;
    schar : String  ;
  begin
      stmp := ConstructParamFontStyle( TGIS_FontStyles( _font.Style ) ) ;

      if FLetter = '|' then
        schar := IntToStr( ord( FLetter ) )
      else
        schar := FLetter ;

      if not IsStringEmpty( stmp ) then
        stmp := _font.Name + ':' + schar + ':' + stmp
      else
        stmp := _font.Name + ':' + schar ;

    Result := Execute( TGIS_SymbolFont( SymbolList.Prepare( stmp ) ), _proc ) ;
  end;

  function TGIS_ControlFont.Execute(
    const _symbol : TGIS_SymbolFont ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult;
  var
    i       : Integer ;
    tmpProc : TGIS_Proc ;
  begin
    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    FSymbol := _symbol ;
    if assigned( Symbol ) then begin
      cmbFonts.ItemIndex := -1 ;
      for i:=0 to cmbFonts.ItemsCount - 1 do
        if CompareText( cmbFonts.Item[i], Symbol.Font.Name ) = 0 then begin
          cmbFonts.ItemIndex := i ;
          break ;
        end ;

      FLetter := Symbol.Char ;

      chkBold.Checked       := GisTestFontStyle( TGIS_FontStyle.Bold, Symbol.Font.Style ) ;
      chkItalic.Checked     := GisTestFontStyle( TGIS_FontStyle.Italic, Symbol.Font.Style ) ;
      chkUnderline.Checked  := GisTestFontStyle( TGIS_FontStyle.Underline, Symbol.Font.Style ) ;
      chkStrikeout.Checked  := GisTestFontStyle( TGIS_FontStyle.StrikeOut, Symbol.Font.Style ) ;
    end else begin
      cmbFonts.ItemIndex := -1 ;
      for i:=0 to cmbFonts.ItemsCount - 1 do
        if CompareText( cmbFonts.Item[i], 'Arial' ) = 0 then begin
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

    mProc := _proc ;

    tmpProc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      begin
        doModalResult( _modal_result ) ;
      end
    {$IFDEF OXYGENE}){$ENDIF};

    Result := ShowModal( tmpProc, assigned( _proc ) ) ;
  end ;

  procedure TGIS_ControlFont.doDestroy ;
  begin
    inherited ;
  end;

  procedure TGIS_ControlFont.doModalResult(
    _modal_result : TGIS_PvlModalResult
  ) ;
  var
    stmp  : String  ;
    schar : String  ;
  begin
    if _modal_result = TGIS_PvlModalResult.OK then begin
      stmp := ConstructParamFontStyle( TGIS_FontStyles( pnlPreview.FontStyle ) ) ;

      if FLetter = '|' then
        schar := IntToStr( ord( FLetter ) )
      else
        schar := FLetter ;

      if not IsStringEmpty( stmp ) then
        stmp := pnlPreview.FontFamily + ':' + schar + ':' + stmp
      else
        stmp := pnlPreview.FontFamily + ':' + schar ;

      FSymbol := TGIS_SymbolFont( SymbolList.Prepare( stmp ) ) ;
    end ;
    if assigned( mProc ) then
      mProc( _modal_result ) ;
  end ;

//==============================================================================
// Actions
//==============================================================================

  procedure TGIS_ControlFont.actStyleExecute(Sender: TObject);
  begin
      with pnlPreview do begin
        if chkBold.Checked then
          FontStyle := GisAddFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.Bold )
        else
          FontStyle := GisRemoveFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.Bold ) ;

        if chkItalic.Checked then
          FontStyle := GisAddFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.Italic )
        else
          FontStyle := GisRemoveFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.Italic ) ;

        if chkUnderline.Checked then
          FontStyle := GisAddFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.Underline )
        else
          FontStyle := GisRemoveFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.Underline ) ;

        if chkStrikeout.Checked then
          FontStyle := GisAddFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.StrikeOut )
        else
          FontStyle := GisRemoveFontStyle( pnlPreview.FontStyle, TGIS_FontStyle.StrikeOut ) ;
      end ;

    pnlPreview.Invalidate ;
  end;

  procedure TGIS_ControlFont.actListUpdate(Sender: TObject);
  begin
    BtnOK.Enabled := not IsStringEmpty( FLetter ) ;
  end;

//==================================== END =====================================
end.

