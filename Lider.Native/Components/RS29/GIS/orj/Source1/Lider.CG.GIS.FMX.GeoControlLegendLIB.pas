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
  Legend component. Vector dialog - bitmap selector.
}

unit Lider.CG.GIS.FMX.GeoControlLegendLIB ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendLIB"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  FMX.Forms,
  FMX.Graphics,
  FMX.ComboEdit,
  FMX.ListView.Types,
  FMX.ListBox,
  FMX.StdCtrls,
  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  FMX.Objects,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.GeoLibrarySVG,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.FMX.GeoModalForm, FMX.Controls.Presentation ;

type

  /// <summary>
  ///   Visual form for managing bitmaps.
  /// </summary>
  /// <remarks>
  ///   To be use only from: TGIS_ControlLegend.
  /// </remarks>
  TGIS_ControlLegendLIB = class( TGIS_ModalForm )
    pnlMain: TPanel;
    rctMain: TRectangle;
    lstItems: TVertScrollBox;
    cmbFilter: TComboEdit;
    procedure btnOKClick(Sender: TObject); override;
    procedure cmbFilterChange(Sender: TObject);
    procedure lstClick(Sender: TObject);
    procedure lstMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private // properties internal value
    FSymbol : TGIS_SymbolAbstract ;
    oLib : TGIS_SymbolLibrarySVG ;
  private
    const FRM_CLIENT_HEIGHT = 289 ;
    const FRM_CLIENT_WIDTH  = 369 ;
    const MAX_COLUMNS       = 3 ;

  protected

    /// <inheritdoc/>
    procedure initForm       ; override ;

    /// <inheritdoc/>
    procedure initControls   ; override ;

    /// <inheritdoc/>
    procedure btnCancelClick ( _sender : TObject
                             ) ; override ;

  public
    /// <summary>
    ///   Execute dialog.
    /// </summary>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    /// <param name="_proc">
    ///   to be executed after the form was closed
    /// </param>
    procedure Execute ( const _proc : TProc<TModalResult>
                      ) ;
  public

    /// <summary>
    ///   Selected Symbol or nil.
    /// </summary>
    property Symbol : TGIS_SymbolAbstract read FSymbol ;
  end;

//##############################################################################
implementation

uses
  System.StrUtils,
  Lider.CG.GIS.FMX.GeoControlHelper,
  FMX.Pickers,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource;

type
  T_Item = class( TLayout )
    private
      oRect    : TRectangle ;
      oImage   : TImage ;
      oCaption : TLabel ;
      oName    : String ;
      bSelected : Boolean ;
    public
      constructor Create( _owner: TComponent) ; override;
    private
      procedure fset_Selected( const _value : Boolean ) ;
    public

      property Selected : Boolean read bSelected write fset_Selected ;
  end;


  constructor T_Item.Create( _owner: TComponent) ;
  begin
    inherited Create( _owner );

    Width          := 110 ;
    Padding.Left   := 5 ;
    Padding.Top    := 5 ;
    Padding.Bottom := 5 ;
    Padding.Right  := 5 ;

    oRect := TRectangle.Create( Self );
    oRect.Parent := Self ;

    oImage := TImage.Create( Self );
    oImage.Parent := Self ;
    oImage.Width  := 64 ;
    oImage.Height := 64 ;
    oImage.Align := TAlignLayout.Top ;
    oImage.HitTest := False ;

    oCaption := TLabel.Create( Self );
    oCaption.Parent := Self ;
    oCaption.TextSettings.HorzAlign := TTextAlign.Center ;
    oCaption.TextSettings.VertAlign := TTextAlign.Leading ;
    oCaption.Align := TAlignLayout.Bottom ;
    oCaption.HitTest := False ;
    oCaption.StyledSettings := oCaption.StyledSettings - [TStyledSetting.Size] ;
    oCaption.Font.Size := 11 ; //? check for mobile
    oCaption.Height := 18 ;

    Height := oImage.Height + oCaption.Height + 10 ;

    // oRect continuation
    oRect.Width := Width - 2 ;
    oRect.Height := Height - 2 ;
    oRect.Position.X := 1 ;
    oRect.Position.Y := 1 ;
    oRect.Stroke.Thickness := 0 ;
    oRect.Fill.Color := $FFCDE8FF ;
    oRect.Visible := False ;
    oRect.HitTest := False ;

    HitTest := True ;
  end;

  procedure T_Item.fset_Selected( const _value : Boolean ) ;
  begin
    if _value = bSelected then Exit ;

    oRect.Visible := _value ;
    bSelected := _value ;
  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendLIB.btnOKClick(Sender: TObject);
  begin
    inherited ;
  end ;

  procedure TGIS_ControlLegendLIB.cmbFilterChange(Sender: TObject);
  var
    symbol_name : String ;
    itm         : T_Item ;
    idx         : Integer ;
    row         : Integer ;
    col         : Integer ;
    o           : TFmxObject ;
    i           : Integer ;
    caption     : String ;
    filter      : String ;
    loop_filter : String ;
    search_str  : Boolean ;
  begin
    if Assigned( lstItems.Content.Children ) then begin
      for i:= lstItems.Content.Children.Count -1 downto 0 do
        lstItems.Content.RemoveObject( lstItems.Content.Children.Items[i] );
    end;

    idx := 0 ;
    row := 0 ;
    col := 0 ;

    if Length( cmbFilter.Text ) = 0 then
      cmbFilter.ItemIndex := 1 ;

    filter := cmbFilter.Text ;
    if Length( filter ) < 2 then
      exit ;

    if oLib.Categories.IndexOf( filter ) > -1 then begin
      loop_filter := filter ; // filter is category name
      search_str := False ;
    end
    else begin
      loop_filter := '*' ;   // filter is search string
      search_str := True ;
    end ;

    for symbol_name in oLib.Loop( loop_filter ) do begin
      caption := oLib.GetCaption( symbol_name ) ;

      if search_str and ( not ContainsText( caption, filter ) ) then
        continue ;

      itm := T_Item.Create( lstItems ) ;

      {$IFNDEF GIS_MOBILE_DIALOGS}
        itm.OnClick := lstClick ;
      {$ENDIF}
      itm.OnMouseDown := lstMouseDown ;

      itm.oName := symbol_name ;
      itm.oCaption.Text := caption ;

      itm.oImage.Bitmap.Assign( TBitmap( oLib.GetPreview(symbol_name).NativeBitmap ) ) ;

      itm.Position.X := col * itm.Width ;
      itm.Position.Y := row * itm.Height ;

      lstItems.AddObject( itm ) ;

      inc( col ) ;
      if col >= MAX_COLUMNS then begin
        inc( row ) ;
        col := 0 ;
      end ;

      inc( idx ) ;
    end ;
  end;

  procedure TGIS_ControlLegendLIB.lstClick(Sender: TObject);
  var
    itm : T_Item ;
  begin
    itm := T_Item( Sender ) ;
    Application.ProcessMessages ;
    FSymbol := SymbolList.Prepare( itm.oName ) ;
  end;

  procedure TGIS_ControlLegendLIB.lstMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  var
    itm : T_Item ;
    i : Integer ;
  begin
    itm := T_Item( Sender ) ;

    if Assigned( lstItems.Content.Children ) then begin
      for i:= lstItems.Content.Children.Count -1 downto 0 do
        T_Item( lstItems.Content.Children.Items[i] ).Selected := False ;
    end;

    itm.Selected := True ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      FSymbol := SymbolList.Prepare( itm.oName ) ;
      ShowOK := True ;
    {$ENDIF}
    Application.ProcessMessages ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendLIB.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_DLGLIB ) ;
    Self.ClientHeight := FRM_CLIENT_HEIGHT ;
    Self.ClientWidth := FRM_CLIENT_WIDTH ;
  end ;

  procedure TGIS_ControlLegendLIB.initControls ;
  begin
    pnlMain := TPanel.Create( oMainForm );
    pnlMain.Parent := oMainForm ;
    pnlMain.Size.PlatformDefault := False ;
    pnlMain.TabOrder := 1 ;

    rctMain := TRectangle.Create( pnlMain );
    rctMain.Parent := pnlMain ;
    rctMain.Align := TAlignLayout.Client ;
    rctMain.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    rctMain.Fill.Color := TAlphaColorRec.White ;
    rctMain.Padding.Left := 1 ;
    rctMain.Padding.Top := 1 ;
    rctMain.Padding.Right := 1 ;
    rctMain.Padding.Bottom := 1 ;
    rctMain.Size.Height := pnlMain.Height ;
    rctMain.Size.Width := pnlMain.Width ;
    rctMain.Size.PlatformDefault := False ;

    lstItems := TVertScrollBox.Create( rctMain ) ;
    lstItems.Parent := rctMain ;
    lstItems.Align := TAlignLayout.Client ;
    lstItems.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    lstItems.Size.PlatformDefault := False ;
    lstItems.TabOrder := 1 ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnCancel.Visible := False ;
      btnHelp.Visible := False ;
      btnOK.TabOrder := 2 ;
    {$ENDIF}

    cmbFilter := TComboEdit.Create( oMainForm ) ;

    cmbFilter.Parent := oMainForm ;
    cmbFilter.Size.PlatformDefault := False ;
    cmbFilter.TabOrder := 0 ;
    cmbFilter.OnTyping := cmbFilterChange ;
    cmbFilter.OnChange := cmbFilterChange ;

    oLib := TGIS_SymbolLibrarySVG.Handle ;
    oLib.OpenForBrowsing ;

    cmbFilter.Items.Assign( oLib.Categories );
    cmbFilter.ItemIndex := 1 ;
    cmbFilterChange( oMainForm ) ;

    if _rsbidi then
      Self.BiDiMode := TBiDiMode.bdRightToLeft
    else
      Self.BiDiMode := TBiDiMode.bdLeftToRight ;

    pnlMain.Position.Y := 8 ;
    pnlMain.Size.Height := 241 ;
    PlaceControl( BiDiMode, nil, pnlMain, 8, -1 ) ;

    cmbFilter.Position.Y := pnlMain.Position.Y + pnlMain.Height + 8 ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      PlaceControl( BiDiMode, nil, cmbFilter, 8, -1 ) ;
    {$ELSE}
      btnOk.Position.Y := cmbFilter.Position.Y ;
      btnOk.Height := cmbFilter.Height ;
      PlaceControl( BiDiMode, nil, cmbFilter, 8, 265 ) ;
      PlaceControl( BiDiMode, nil, btnOk, 280, 80 ) ;
    {$ENDIF}

    Self.ClientHeight := RoundS( cmbFilter.Position.Y ) +
                         RoundS( cmbFilter.Height ) + 8 ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      ShowOK := False ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendLIB.btnCancelClick ;
  begin
    ModalResult := mrCancel ;
  end;

  procedure TGIS_ControlLegendLIB.Execute(
    const _proc : TProc<TModalResult>
  ) ;
  begin
    ShowModalEx( _proc ) ;
  end ;

//==================================== END =====================================
end.

