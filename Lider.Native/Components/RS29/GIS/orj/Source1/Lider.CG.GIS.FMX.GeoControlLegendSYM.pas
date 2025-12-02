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

unit Lider.CG.GIS.FMX.GeoControlLegendSYM ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendSYM"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Actions,
  FMX.Types,
  FMX.Objects,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.TabControl,
  FMX.ActnList,
  Fmx.StdCtrls,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.FMX.GeoViewerWnd,
  FMX.Controls.Presentation,
  Lider.CG.GIS.FMX.GeoModalForm ;

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
    pnlMain: TRectangle;
    lblMain: TLabel;
    dlgOpen: TOpenDialog;
    chkTransparent: TCheckBox;
    btnSelectLibrary: TButton;
    procedure FormShow(Sender: TObject);
    procedure btnSelectFontClick(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure chkTransparentChange(Sender: TObject);
    procedure btnSelectLibraryClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject); override;
  private
    { Private declarations }
    FSymbol : TGIS_SymbolAbstract ;
    GIS     : TGIS_ViewerWnd ;
  private
    const frmClientHeight = 234 ;
    const frmClientWidth  = 260 ;
  private
    /// <summary>
    ///   Remove any ?TRUE fragment i a path.
    /// </summary>
    /// <param name="_name">
    ///   symbol name
    /// </param>
    function  trimQuestionmark ( const _name   : String
                               ) : String ;
    procedure setSymbol        ( _symbol : TGIS_SymbolAbstract
                               ) ;
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
    procedure initForm     ; override;

    /// <inheritdoc/>
    procedure initControls ; override;

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
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute       ( const _name   : String         ;
                              const _onhelp : TGIS_HelpEvent ;
                              const _proc   : TProc<TModalResult>
                            ) ; overload;

    /// <summary>
    ///   Execute dialog on a symbol given by name.
    /// </summary>
    /// <param name="_name">
    ///   name (path) of the symbol.
    /// </param>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute       ( const _name   : String         ;
                              const _proc   : TProc<TModalResult>
                            ) ; overload;

  public
    /// <summary>
    ///   Selected Symbol or nil.
    /// </summary>
    property Symbol : TGIS_SymbolAbstract read  FSymbol ;

  end;


//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.FMX.GeoControlHelper,
  Lider.CG.GIS.FMX.GeoControlLegendFNT,
  Lider.CG.GIS.FMX.GeoControlLegendLIB;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.FormShow(
    Sender: TObject
  ) ;
  begin
    renderShape ;
  end;

//==============================================================================
// Internal methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.setSymbol(
    _symbol : TGIS_SymbolAbstract
  ) ;
  var
    is_symbol : Boolean ;
  begin
    FSymbol := _symbol ;
    is_symbol := assigned( FSymbol ) ;
    lblMain.Visible := not is_symbol ;
    GIS.Visible     := is_symbol ;
    if assigned( btnOK ) then
      btnOK.Enabled := is_symbol ;
    chkTransparent.Enabled := is_symbol and
                              ( FSymbol is TGIS_SymbolPicture ) ;
    if not chkTransparent.Enabled then chkTransparent.IsChecked := False ;
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
      shp.Params.Marker.Size := -TruncS(GIS.Width/2 * GIS.Canvas.Scale) ;
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

    if chkTransparent.IsChecked then
      setSymbol( SymbolList.Prepare( tmp + '?TRUE' ) )
    else
      setSymbol( SymbolList.Prepare( tmp ) ) ;
    GIS.Visible := Assigned( FSymbol ) ;

    renderShape ;
  end ;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.btnSelectFontClick(
    Sender: TObject
  ) ;
  var
    frm : TGIS_ControlLegendFNT ;
    sym : TGIS_SymbolFont ;
  begin
    frm := TGIS_ControlLegendFNT.Create( oMainForm ) ;
    if FSymbol is TGIS_SymbolFont then sym := TGIS_SymbolFont( FSymbol )
                                  else sym := nil     ;

    frm.Execute(
      sym, pOnHelp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result <> mrOK then
          exit ;

        setSymbol( frm.Symbol ) ;
        GIS.Visible := Assigned( FSymbol ) ;

        renderShape ;
      end
    ) ;
  end ;

  procedure TGIS_ControlLegendSYM.btnSelectLibraryClick(Sender: TObject);
  var
    frm : TGIS_ControlLegendLIB ;
  begin
    frm := TGIS_ControlLegendLIB.Create( oMainForm ) ;
    frm.Execute(
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result <> mrOK then
          exit ;

        setSymbol( frm.Symbol ) ;
        GIS.Visible := Assigned( FSymbol ) ;

        renderShape ;
      end
    ) ;
  end ;

  procedure TGIS_ControlLegendSYM.btnSelectFileClick(
    Sender: TObject
  ) ;
  begin
    if ( not Assigned( FSymbol ) ) or ( FSymbol is TGIS_SymbolFont )
      then dlgOpen.FileName := ''
      else dlgOpen.FileName := trimQuestionmark( FSymbol.Name ) ;

    if dlgOpen.Execute then begin
      redrawFileSymbol( dlgOpen.FileName ) ;
    end ;
  end;

  procedure TGIS_ControlLegendSYM.chkTransparentChange(
    Sender: TObject
  ) ;
  begin
    redrawFileSymbol( FSymbol.Name ) ;
  end;

  procedure TGIS_ControlLegendSYM.btnOKClick(
    Sender : TObject
  ) ;
  begin
    if btnOK.Enabled then
      ModalResult := mrOK ;
  end ;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendSYM.initForm ;
  begin
    Self.Caption := _rsrc  ( GIS_RS_LEGEND_DLGSYMBOL ) ;
    Self.ClientHeight := frmClientHeight ;
    Self.ClientWidth := frmClientWidth ;
    Self.Name := 'TGIS_ControlLegendSYM' ;
    Self.OnShow := FormShow ;
  end ;

  procedure TGIS_ControlLegendSYM.initControls ;
  begin
    btnSelectFile := TButton.Create( oMainForm ) ;
    btnSelectFile.Parent := oMainForm ;
    btnSelectFile.Size.PlatformDefault := False ;
    btnSelectFile.TabOrder := 0 ;
    btnSelectFile.Text := _rsrcna( GIS_RS_LEGEND_DLGSYMBOL_FILE ) ;
    btnSelectFile.OnClick := btnSelectFileClick ;

    btnSelectFont := TButton.Create( oMainForm ) ;
    btnSelectFont.Parent := oMainForm ;
    btnSelectFont.Size.PlatformDefault := False ;
    btnSelectFont.TabOrder := 1 ;
    btnSelectFont.Text := _rsrcna( GIS_RS_LEGEND_DLGSYMBOL_FONT ) ;
    btnSelectFont.OnClick := btnSelectFontClick ;

    btnSelectLibrary := TButton.Create( oMainForm ) ;
    btnSelectLibrary.Parent := oMainForm ;
    btnSelectLibrary.Size.PlatformDefault := False ;
    btnSelectLibrary.TabOrder := 2 ;
    btnSelectLibrary.Text := _rsrcna( GIS_RS_LEGEND_DLGSYMBOL_LIB ) ;
    btnSelectLibrary.OnClick := btnSelectLibraryClick ;

    pnlMain := TRectangle.Create( oMainForm ) ;
    pnlMain.Parent := oMainForm ;
    pnlMain.Fill.Kind := TBrushKind.None ;
    pnlMain.Stroke.Color := TAlphaColorRec.Darkgrey ;
    pnlMain.TabOrder := 3 ;

    lblMain := TLabel.Create( pnlMain ) ;
    lblMain.Parent := pnlMain ;
    lblMain.Size.PlatformDefault := False ;
    lblMain.Text := _rsrcna( GIS_RS_LEGEND_NONE ) ;
    lblMain.Align := TAlignLayout.Client ;
    lblMain.TextAlign := TTextAlign.Center ;
    lblMain.VertTextAlign := TTextAlign.Center ;
    lblMain.FixSize ;

    chkTransparent := TCheckBox.Create( oMainForm ) ;
    chkTransparent.Parent := oMainForm ;
    chkTransparent.Size.PlatformDefault := False ;
    chkTransparent.TabOrder := 4 ;
    chkTransparent.OnChange := chkTransparentChange ;
    chkTransparent.Text := _rsrcna( GIS_RS_LEGEND_DLGSYMBOL_TRANSPARENT ) ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := assigned( pOnHelp ) ;
      btnHelp.TabOrder := 5 ;
      btnCancel.TabOrder := 7 ;
      btnOK.TabOrder := 6 ;
    {$ENDIF}

    dlgOpen := TOpenDialog.Create( oMainForm ) ;
    dlgOpen.Filter := _rsrc( GIS_RS_LEGEND_DLGSYMBOL_FILTER ) ;

    GIS := TGIS_ViewerWnd.Create( oMainForm ) ;
    GIS.BackgroundColor := TColorRec.Null ;

    GIS.Align   := TAlignLayout.Client ;
    GIS.Parent  := pnlMain ;
    GIS.Width   := pnlMain.Width ;
    GIS.Height  := pnlMain.Height ;
    GIS.HiRes   := True  ;
    GIS.Enabled := False ;
    GIS.Visible := False ;

    btnSelectFile.Position.Y := 8 ;
    btnSelectFile.Size.Height := 24 ;
    PlaceControl( BiDiMode, nil, btnSelectFile, 8, 75 ) ;

    btnSelectFont.Position.Y := 39 ;
    btnSelectFont.Size.Height := 24 ;
    PlaceControl( BiDiMode, nil, btnSelectFont, 8, 75 ) ;

    btnSelectLibrary.Position.Y := 70 ;
    btnSelectLibrary.Size.Height := 24 ;
    PlaceControl( BiDiMode, nil, btnSelectLibrary, 8, 75 ) ;

    pnlMain.Position.Y := 8 ;
    pnlMain.Size.Height := 153 ;
    PlaceControl( BiDiMode, nil, pnlMain, -8, 153 ) ;

    chkTransparent.Position.Y := 168 ;
    chkTransparent.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, chkTransparent, 90, 162 ) ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Size.Height := 24 ;
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 75 ) ;

      btnCancel.Size.Height := 24 ;
      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 75 ) ;

      btnOK.Size.Height := 24 ;
      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 75 ) ;
    {$ENDIF}

    {$IFDEF GIS_MOBILE_DIALOGS}
      btnSelectFile.Visible := False ;
      chkTransparent.Visible := False ;
      ClientHeight := Rounds( pnlMain.Position.Y + pnlMain.Height + 8 ) ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendSYM.Execute(
    const _name   : String         ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TProc<TModalResult>
  ) ;
  var
    k : Integer ;
  begin
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    setSymbol( SymbolList.Prepare( _name ) ) ;

    renderShape ;

    if FSymbol is TGIS_SymbolPicture then k := Pos( '?', _name )
                                     else k := 0 ;
    chkTransparent.IsChecked := k > 0 ;

    ShowModalEx( _proc ) ;
  end ;

  procedure TGIS_ControlLegendSYM.Execute(
    const _name : String ;
    const _proc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _name, hlp, _proc ) ;
  end ;

//==================================== END =====================================
end.

