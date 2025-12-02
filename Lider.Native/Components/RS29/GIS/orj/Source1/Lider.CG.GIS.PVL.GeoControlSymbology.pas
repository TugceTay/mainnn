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
  Legend component. Vector dialog - symbol selector.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlSymbology ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlSymbology"'}
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
    TatukGIS.ndk.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Classes,
    System.Math,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoParams,
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
  ///   Visual form for managing symbols.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    To be use only from: TGIS_ControlLegend.
  ///    </note>
  /// </remarks>
  TGIS_ControlSymbology = class( TGIS_PvlModalForm )
    private
      btnSelectFont     : TGIS_PvlButton        ;
      btnSelectFile     : TGIS_PvlButton        ;
      pnlMain           : TGIS_PvlPreviewPanel  ;
      pnlMainWidth      : Integer               ;
      pnlMainHeight     : Integer               ;
      chkTransparent    : TGIS_PvlCheckBox      ;
      dlgOpen           : TGIS_PvlOpenDialog    ;
      btnSelectLibrary  : TGIS_PvlButton        ;
    private
      procedure actSelectFontExecute    ( Sender: TObject ) ;
      procedure actSelectFileExecute    ( Sender: TObject ) ;
      procedure actTransparentExecute   ( Sender: TObject ) ;
      procedure actSelectLibraryExecute ( Sender: TObject ) ;
    private
      { Private declarations }
      FSymbol       : TGIS_SymbolAbstract ;
      FBitmap       : TGIS_Bitmap ;
      FOnlySVG      : Boolean ;
      FOnlyCategory : String ;
    private
      /// <summary>
      ///   Setter for Symbol property.
      /// </summary>
      /// <param name="_symbol">
      ///   Symbol to be set.
      /// </param>
      procedure fset_Symbol        ( _symbol : TGIS_SymbolAbstract
                                 ) ;

      /// <summary>
      ///   Remove any ?TRUE fragment i a path.
      /// </summary>
      /// <param name="_name">
      ///   Symbol name.
      /// </param>
      function  trimQuestionmark ( const _name   : String
                                 ) : String ;

      /// <summary>
      ///   Render given shape on the control.
      /// </summary>
      procedure renderShape      ;

      /// <summary>
      ///   Redraw file based symbol applying transparency when required
      /// </summary>
      /// <param name="_name">
      ///   symbol name
      /// </param>
      procedure redrawFileSymbol ( const _name   : String
                                 ) ;

    public

      /// <inheritdoc/>
      procedure DoInitForm         ; override;

      /// <inheritdoc/>
      procedure DoInitControls     ; override;

      /// <inheritdoc/>
      procedure DoRedraw           ; override;

      /// <inheritdoc/>
      procedure DoAfterCreate      ; override;

    public

      /// <summary>
      ///   Execute dialog on a symbol given by name.
      /// </summary>
      /// <param name="_name">
      ///   name (path) of the symbol.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute            ( const _name   : String
                                  ) : TGIS_PvlModalResult ; overload;

      /// <summary>
      ///   Execute dialog on a symbol given by name.
      /// </summary>
      /// <param name="_name">
      ///   name (path) of the symbol.
      /// </param>
      /// <param name="_proc">
      ///   Action to be performed after closing modal form.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute            ( const _name   : String ;
                                    const _proc   : TGIS_Proc
                                  ) : TGIS_PvlModalResult ; overload;

      /// <summary>
      ///   Execute dialog on a symbol given by name.
      /// </summary>
      /// <param name="_name">
      ///   name (path) of the symbol.
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   Action to be performed after closing modal form.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute           ( const _name   : String         ;
                                   const _onhelp : TGIS_HelpEvent ;
                                   const _proc   : TGIS_Proc
                                 ) : TGIS_PvlModalResult ; overload;

    protected
      /// <inheritdoc/>
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Active symbol.
      /// </summary>
      property Symbol : TGIS_SymbolAbstract
               read  FSymbol
               write fset_Symbol ;

      /// <summary>
      ///   If true then dialog will offer only Libaray and SVG Fiel selection.
      /// </summary>
      property OnlySVG : Boolean
               read  FOnlySVG
               write FOnlySVG ;

      /// <summary>
      ///   If not empty then Libary form will present only symbols matching
      ///   categeory.
      /// </summary>
      property OnlyCategory : String
               read  FOnlyCategory
               write FOnlyCategory ;
  end;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.PVL.GeoControlFont,
    Lider.CG.GIS.PVL.GeoControlSymbologyLibrary;
{$ENDIF}


//==============================================================================
// Internal methods
//==============================================================================

  procedure TGIS_ControlSymbology.fset_Symbol(
    _symbol : TGIS_SymbolAbstract
  ) ;
  begin
    FSymbol := _symbol ;

    chkTransparent.Enabled := assigned( FSymbol ) and
                              ( FSymbol is TGIS_SymbolPicture ) ;
    if not chkTransparent.Enabled then chkTransparent.Checked := False ;
  end ;

  function TGIS_ControlSymbology.trimQuestionmark(
    const _name : String
  ) : String ;
  var
    k : Integer ;
  begin
    // truncate any ?TRUE fragment in path
    k := Pos( '?', _name ) ;
    if k > StringFirst then Result := Copy( _name, StringFirst, k - StringFirst )
             else Result := _name ;
  end;

  procedure TGIS_ControlSymbology.renderShape ;
  var
    iw : Integer ;
    ih : Integer ;
  begin
    if assigned( FSymbol ) then begin

      {$IFDEF GIS_MOBILE_DIALOGS}
        iw := RoundS( pnlMainWidth  * CanvasScale * PPIFix ) ;
        ih := RoundS( pnlMainHeight * CanvasScale * PPIFix ) ;
      {$ELSE}
        iw := RoundS( pnlMainWidth  * CanvasScale * GUIScale * PPIFix ) ;
        ih := RoundS( pnlMainHeight * CanvasScale * GUIScale * PPIFix  ) ;
      {$ENDIF}

      if assigned( FBitmap ) then
        FreeObject( FBitmap ) ;

      FBitmap := TGIS_Bitmap.Create( iw, ih ) ;
      if pnlMain.IsStyled then
        FBitmap.DrawSymbol( FSymbol.Name, PPI,
                            pnlMain.StyledAreaColor, TGIS_Color.Silver
                          )
      else
        FBitmap.DrawSymbol( FSymbol.Name, PPI ) ;
      pnlMain.Bitmap := FBitmap ;
      pnlMain.Invalidate ;
    end;
  end;

  procedure TGIS_ControlSymbology.redrawFileSymbol(
    const _name : String
  ) ;
  var
    tmp : String  ;
  begin
    tmp := trimQuestionmark( _name ) ;

    if chkTransparent.Checked then
      fset_Symbol( SymbolList.Prepare( tmp + '?TRUE' ) )
    else
      fset_Symbol( SymbolList.Prepare( tmp ) ) ;

    renderShape ;
  end ;


//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlSymbology.actSelectFontExecute(
    Sender: TObject
  ) ;
  var
    frm  : TGIS_ControlFont ;
    sym  : TGIS_SymbolFont ;
    proc : TGIS_Proc ;
  begin
    frm := TGIS_ControlFont.create( Self ) ;
    if FSymbol is TGIS_SymbolFont then sym := TGIS_SymbolFont( FSymbol )
                                  else sym := nil     ;


    proc := {$IFDEF OXYGENE}new TGIS_Proc({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result = TGIS_PvlModalResult.OK then begin
          fset_Symbol( frm.Symbol ) ;
          renderShape ;
        end;
      end
    {$IFDEF OXYGENE}){$ENDIF};

    frm.Execute( sym, OnHelpEvent, proc ) ;
  end;

  procedure TGIS_ControlSymbology.actSelectLibraryExecute(
    Sender: TObject
  ) ;
  var
    frm : TGIS_ControlSymbologyLibrary ;
    proc : TGIS_Proc ;
  begin
    frm := TGIS_ControlSymbologyLibrary.Create( Self ) ;

    proc := {$IFDEF OXYGENE}new TGIS_Proc({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result = TGIS_PvlModalResult.OK then begin
          fset_Symbol( frm.Symbol ) ;
          renderShape ;
        end;
      end
    {$IFDEF OXYGENE}){$ENDIF};

    frm.OnlyCategory := FOnlyCategory ;
    frm.Execute( proc ) ;
  end;

  procedure TGIS_ControlSymbology.actSelectFileExecute(
    Sender: TObject
  ) ;
  begin
    if ( not assigned( FSymbol ) ) or ( FSymbol is TGIS_SymbolFont )
      then dlgOpen.FileName := ''
      else dlgOpen.FileName := trimQuestionmark( FSymbol.Name ) ;

    if dlgOpen.Execute = TGIS_PvlModalResult.OK then
      redrawFileSymbol( dlgOpen.FileName ) ;
  end;

  procedure TGIS_ControlSymbology.actTransparentExecute(
    Sender: TObject
  ) ;
  begin
    redrawFileSymbol( FSymbol.Name ) ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlSymbology.DoInitForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL ) ;
    Self.ClientHeight := 231 ;
    Self.ClientWidth := 260 ;
    Self.Name := 'TGIS_ControlSymbology' ;
  end ;

  procedure TGIS_ControlSymbology.DoInitControls ;
  var
    ytop : Integer ;
  begin
    ytop := Context.VMargin ;

    btnSelectLibrary := TGIS_PvlButton.Create( Context ) ;
    btnSelectLibrary.Place( 75, 25, nil, Context.HMargin, nil, ytop ) ;
    btnSelectLibrary.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_LIB ) ;
    {$IFDEF DCC}
      btnSelectLibrary.OnClick := actSelectLibraryExecute ;
    {$ELSE}
      btnSelectLibrary.OnClick := @actSelectLibraryExecute ;
    {$ENDIF}
    btnSelectLibrary.TabOrder := 0 ;

    ytop := ytop + btnSelectLibrary.Height + Context.VSpace ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnSelectFile := TGIS_PvlButton.Create( Context ) ;
      btnSelectFile.Place( 75, 25, nil, Context.HMargin, nil, ytop ) ;
      btnSelectFile.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILE ) ;
      {$IFDEF DCC}
        btnSelectFile.OnClick := actSelectFileExecute ;
      {$ELSE}
        btnSelectFile.OnClick := @actSelectFileExecute ;
      {$ENDIF}
      btnSelectFile.TabOrder := 1 ;

      ytop := ytop + btnSelectFile.Height + Context.VSpace ;
    {$ENDIF}

    btnSelectFont := TGIS_PvlButton.Create( Context ) ;
    btnSelectFont.Place( 75, 25, nil, Context.HMargin, nil, ytop ) ;
    btnSelectFont.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FONT ) ;
    {$IFDEF DCC}
      btnSelectFont.OnClick := actSelectFontExecute ;
    {$ELSE}
      btnSelectFont.OnClick := @actSelectFontExecute ;
    {$ENDIF}
    btnSelectFont.TabOrder := 2 ;

    ytop := Context.VMargin ;

    pnlMain := TGIS_PvlPreviewPanel.Create( Context ) ;
    pnlMain.Place( 150, 150, nil, -Context.HMargin, nil, ytop ) ;
    pnlMain.Border := True ;
    pnlMain.Caption := _rsrc( GIS_RS_LEGEND_NONE ) ;
    pnlMain.TabOrder := 3 ;

    ytop := ytop + pnlMain.Height + Context.VSpace ;

    chkTransparent := TGIS_PvlCheckBox.Create( Context ) ;
    chkTransparent.Place(  150, 17, nil, -Context.HMargin, nil, ytop ) ;
    chkTransparent.Caption :=  _rsrc( GIS_RS_LEGEND_DLGSYMBOL_TRANSPARENT ) ;
    {$IFDEF DCC}
      chkTransparent.OnClick := actTransparentExecute ;
    {$ELSE}
      chkTransparent.OnClick := @actTransparentExecute ;
    {$ENDIF}
    chkTransparent.TabOrder := 4 ;

    BtnHelp.Visible := assigned( OnHelpEvent ) ;
    BtnHelp.TabOrder := 5 ;
    BtnCancel.TabOrder := 7 ;
    BtnOK.TabOrder := 6 ;

    dlgOpen := TGIS_PvlOpenDialog.Create( Context ) ;

    pnlMainWidth  := pnlMain.Width ;
    pnlMainHeight := pnlMain.Height ;
  end ;

  procedure TGIS_ControlSymbology.DoRedraw ;
  begin
    pnlMain.DoRedraw ;
    renderShape ;
    inherited ;
  end;

  procedure TGIS_ControlSymbology.DoAfterCreate ;
  begin
    FOnlySVG := False ;
    FOnlyCategory := '' ;
    inherited ;
  end;

  function TGIS_ControlSymbology.Execute(
    const _name : String
  ) : TGIS_PvlModalResult ;
  begin
    Result := Execute( _name, nil ) ;
  end;

  function TGIS_ControlSymbology.Execute(
    const _name   : String ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult  ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _name, hlp, _proc ) ;
  end;

  function TGIS_ControlSymbology.Execute(
    const _name   : String         ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    k   : Integer ;
  begin
    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    fset_Symbol( SymbolList.Prepare( _name ) ) ;

    renderShape ;

    if FSymbol is TGIS_SymbolPicture then k := Pos( '?', _name )
                                     else k := 0 ;
    chkTransparent.Checked := k > 0 ;

    if FOnlySVG then
      dlgOpen.Filter := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILTER_SVG )
    else
      dlgOpen.Filter := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILTER ) ;

    btnSelectFont.Visible := not FOnlySVG ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

  procedure TGIS_ControlSymbology.doDestroy ;
  begin
    FreeObject( FBitmap ) ;
    inherited ;
  end;

//==================================== END =====================================
end.
