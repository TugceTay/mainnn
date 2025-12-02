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

unit Lider.CG.GIS.VCL.GeoControlLegendSYM ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlLegendSYM"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,

  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.ActnList,
  VCL.ExtDlgs,
  VCL.Graphics,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.VCL.GeoModalForm,
  VCL.Lider.CG.GIS.GeoViewerWnd;

type

  /// <summary>
  ///   Visual form for managing symbols.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    To be use only from: TGIS_ControlLegend.
  ///    </note>
  /// </remarks>
  TGIS_ControlLegendSYM = class( TGIS_ModalForm )
    btnSelectFont: TButton;
    btnSelectFile: TButton;
    pnlMain: TPanel;
    chkTransparent: TCheckBox;
    dlgOpen: TOpenPictureDialog;
    btnSelectLibrary: TButton;
    procedure FormCreate(Sender: TObject);
    procedure actSelectFontExecute(Sender: TObject);
    procedure actSelectFileExecute(Sender: TObject);
    procedure actTransparentExecute(Sender: TObject);
    procedure actSelectLibraryExecute(Sender: TObject);
  private
    { Private declarations }
    FSymbol : TGIS_SymbolAbstract ;
    GIS     : TGIS_ViewerWnd ;

  private

    procedure setSymbol        ( _symbol : TGIS_SymbolAbstract
                               ) ;
    /// <summary>
    ///   Remove any ?TRUE fragment i a path.
    /// </summary>
    /// <param name="_name">
    ///   symbol name
    /// </param>
    function  trimQuestionmark ( const _name   : String
                               ) : String ;
    procedure renderShape      ;

    /// <summary>
    ///   Redraw file based symbol applying transparency when required
    /// </summary>
    /// <param name="_name">
    ///   symbol name
    /// </param>
    procedure redrawFileSymbol ( const _name   : String
                               ) ;

  protected

    /// <inheritdoc/>
    procedure initForm         ; override;

    /// <inheritdoc/>
    procedure initControls     ; override;

  public

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
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute           ( const _name   : String         ;
                                 const _onhelp : TGIS_HelpEvent
                               ) : Integer ; overload;

    /// <summary>
    ///   Execute dialog on a symbol given by name.
    /// </summary>
    /// <param name="_name">
    ///   name (path) of the symbol.
    /// </param>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute           ( const _name   : String
                               ) : Integer ; overload;
  public
    /// <summary>
    ///   Active symbol.
    /// </summary>
    property Symbol : TGIS_SymbolAbstract read  FSymbol ;

  end;


//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.VCL.GeoControlHelper,
  Lider.CG.GIS.VCL.GeoControlLegendFNT,
  Lider.CG.GIS.VCL.GeoControlLegendLIB;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.FormCreate(
    Sender: TObject
  ) ;
  begin
    GIS := TGIS_ViewerWnd.Create( self ) ;
    GIS.Align   := alClient ;
    GIS.Parent  := pnlMain ;
    GIS.Width   := pnlMain.Width ;
    GIS.Height  := pnlMain.Height ;
    GIS.Color   := clBtnFace ;
    GIS.Enabled := False ;
    GIS.Visible := False ;

    renderShape ;
  end;

//==============================================================================
// Internal methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.setSymbol(
    _symbol : TGIS_SymbolAbstract
  ) ;
  begin
    FSymbol := _symbol ;
    GIS.Visible := assigned( FSymbol ) ;
    chkTransparent.Enabled := assigned( FSymbol ) and
                              ( FSymbol is TGIS_SymbolPicture ) ;
    if not chkTransparent.Enabled then chkTransparent.Checked := False ;
  end ;

  function TGIS_ControlLegendSYM.trimQuestionmark(
    const _name : String
  ) : String ;
  var
    k : Integer ;
  begin
    // truncate any ?TRUE fragment in path
    k := Pos( '?', _name ) ;
    if k > 0 then Result := Copy( _name, 1, k-1 )
             else Result := _name ;
  end;

  procedure TGIS_ControlLegendSYM.renderShape ;
  var
    ll  : TGIS_LayerVector ;
    shp : TGIS_Shape       ;
  begin
    GIS.Close ;

    ll := TGIS_LayerVector.Create ;
    GIS.Add( ll ) ;
    ll.Extent := GisExtent( -1, -1, 1, 1 ) ;

    if ( FSymbol is TGIS_SymbolLine   ) or
       ( FSymbol is TGIS_SymbolLineEx ) then
    begin
      shp := ll.CreateShape( TGIS_ShapeType.Arc ) ;
      shp.Lock( TGIS_Lock.Extent );
      shp.AddPart ;
      shp.AddPoint( GisPoint( -1, 0 ) ) ;
      shp.AddPoint( GisPoint(  1, 0 ) ) ;
      shp.Unlock ;
      shp.Params.Line.Symbol := FSymbol ;
      shp.Params.Line.Width := -3 ;
      shp.Params.Line.Color := TGIS_Color.Black ;
      shp.Params.Line.OutlineColor := TGIS_Color.Black ;
    end
    else begin
      shp := ll.CreateShape( TGIS_ShapeType.Point ) ;
      shp.Lock( TGIS_Lock.Extent );
      shp.AddPart ;
      shp.AddPoint( GisPoint( 0, 0 ) ) ;
      shp.Unlock ;
      shp.Params.Marker.Symbol := FSymbol ;
      shp.Params.Marker.Size := - GIS.Width div 2 ;
      if FSymbol is TGIS_SymbolFont then
      begin
        shp.Params.Marker.Color := TGIS_Color.Black ;
        shp.Params.Marker.OutlineColor := TGIS_Color.Black ;
      end
      else begin
        shp.Params.Marker.Color := TGIS_Color.RenderColor ;
        shp.Params.Marker.OutlineColor := TGIS_Color.RenderColor ;
      end ;
    end;
    GIS.FullExtent ;
  end;

  procedure TGIS_ControlLegendSYM.redrawFileSymbol(
    const _name : String
  ) ;
  var
    tmp : String  ;
  begin
    tmp := trimQuestionmark( _name ) ;

    if chkTransparent.Checked then
      setSymbol( SymbolList.Prepare( tmp + '?TRUE' ) )
    else
      setSymbol( SymbolList.Prepare( tmp ) ) ;

    renderShape ;
  end ;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.actSelectFontExecute(
    Sender: TObject
  ) ;
  var
    frm : TGIS_ControlLegendFNT ;
    sym : TGIS_SymbolFont ;
  begin
    frm := TGIS_ControlLegendFNT.Create( Self ) ;
    try
      if FSymbol is TGIS_SymbolFont then sym := TGIS_SymbolFont( FSymbol )
                                    else sym := nil     ;

      if frm.Execute( sym, pOnHelp ) = mrOK then begin
        setSymbol( frm.Symbol ) ;
        renderShape ;
      end ;
    finally
      frm.Free ;
    end ;
  end;

  procedure TGIS_ControlLegendSYM.actSelectLibraryExecute(
    Sender: TObject
  ) ;
  var
    frm : TGIS_ControlLegendLIB ;
  begin
    frm := TGIS_ControlLegendLIB.Create( Self ) ;
    try

      if frm.Execute = mrOK then begin
        setSymbol( frm.Symbol ) ;
        renderShape ;
      end ;
    finally
      frm.Free ;
    end ;
  end;

  procedure TGIS_ControlLegendSYM.actSelectFileExecute(
    Sender: TObject
  ) ;
  begin
    if ( not Assigned( FSymbol ) ) or ( FSymbol is TGIS_SymbolFont )
      then dlgOpen.FileName := ''
      else dlgOpen.FileName := trimQuestionmark( FSymbol.Name ) ;

    if dlgOpen.Execute then
      redrawFileSymbol( dlgOpen.FileName ) ;
  end;

  procedure TGIS_ControlLegendSYM.actTransparentExecute(
    Sender: TObject
  ) ;
  begin
    redrawFileSymbol( FSymbol.Name ) ;
  end;


//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL ) ;
    Self.ClientHeight := 231 ;
    Self.ClientWidth := 260 ;
    Self.Name := 'TGIS_ControlLegendSYM' ;
    Self.OnCreate := FormCreate ;
  end ;

  procedure TGIS_ControlLegendSYM.initControls ;
  var
    anchors : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;

    btnSelectFile := TButton.Create( Self ) ;
    btnSelectFile.Parent := Self ;
    btnSelectFile.Anchors := anchors ;
    btnSelectFile.Top := 8 ;
    btnSelectFile.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnSelectFile, 8, 75 ) ;
    btnSelectFile.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILE ) ;
    btnSelectFile.TabOrder := 0 ;
    btnSelectFile.OnClick := actSelectFileExecute ;

    btnSelectFont := TButton.Create( Self ) ;
    btnSelectFont.Parent := Self ;
    btnSelectFont.Anchors := anchors ;
    btnSelectFont.Top := 39 ;
    btnSelectFont.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnSelectFont, 8, 75 ) ;
    btnSelectFont.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FONT ) ;
    btnSelectFont.TabOrder := 1 ;
    btnSelectFont.OnClick := actSelectFontExecute ;

    btnSelectLibrary := TButton.Create( Self ) ;
    btnSelectLibrary.Parent := Self ;
    btnSelectLibrary.Anchors := anchors ;
    btnSelectLibrary.Top := 70 ;
    btnSelectLibrary.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnSelectLibrary, 8, 75 ) ;
    btnSelectLibrary.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_LIB ) ;
    btnSelectLibrary.TabOrder := 2 ;
    btnSelectLibrary.OnClick := actSelectLibraryExecute ;

    pnlMain := TPanel.Create( Self ) ;
    pnlMain.Parent := Self ;
    pnlMain.Anchors := anchors ;
    pnlMain.Top := 8 ;
    pnlMain.Height := 153 ;
    PlaceControl( BiDiMode, nil, pnlMain, 90, 162 ) ;
    pnlMain.BevelInner := bvRaised ;
    pnlMain.BevelOuter := bvLowered ;
    pnlMain.Caption := _rsrc( GIS_RS_LEGEND_NONE ) ;
    pnlMain.TabOrder := 3 ;

    chkTransparent := TCheckBox.Create( Self ) ;
    chkTransparent.Parent := Self ;
    chkTransparent.Anchors := anchors ;
    chkTransparent.Top := 168 ;
    chkTransparent.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkTransparent, 90, 153 ) ;
    chkTransparent.Caption := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_TRANSPARENT ) ;
    chkTransparent.TabOrder := 4 ;
    chkTransparent.OnClick := actTransparentExecute ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 5 ;
    btnCancel.TabOrder := 7 ;
    btnOK.TabOrder := 6 ;

    dlgOpen := TOpenPictureDialog.Create( Self ) ;
    dlgOpen.Filter := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILTER ) ;
  end ;

  function TGIS_ControlLegendSYM.Execute(
    const _name   : String         ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    k : Integer ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    setSymbol( SymbolList.Prepare( _name ) ) ;

    renderShape ;

    if FSymbol is TGIS_SymbolPicture then k := Pos( '?', _name )
                                     else k := 0 ;
    chkTransparent.Checked := k > 0 ;

    Result := ShowModal ;
  end ;

  function TGIS_ControlLegendSYM.Execute(
    const _name   : String
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := self.Execute( _name, hlp ) ;
  end;

//==================================== END =====================================
end.

