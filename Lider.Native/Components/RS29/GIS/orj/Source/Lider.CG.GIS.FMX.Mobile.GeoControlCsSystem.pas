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
  Coordinate System. Dialog box to select from predefined list.
}

unit FMX.Mobile.GisControlCsSystem;
{$HPPEMIT '#pragma link "FMX.Mobile.GisControlCsSystem"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,

  FMX.Controls,
  FMX.Forms,
  FMX.Types,
  FMX.Edit,
  FMX.ComboEdit,
  FMX.StdCtrls,
  FMX.Memo,
  FMX.ListBox,
  FMX.SearchBox,
  FMX.Layouts,
  FMX.Controls.Presentation,

  GisRtl,
  GisFunctions,
  GisResource,
  GisTypes,
  GisCsBase,
  GisCsSystems,
  GisCsFactory,

  FMX.GisModalForm ;

type
  /// <summary>
  ///   Dialog box to select coordinate system from the lists of existing
  ///   (predefined) Coordinate Systems.
  /// </summary>
  TGIS_ControlCSSystem = class(TGIS_ModalForm)
    // designed controls
      lstSystems: TListBox;
      edtSearch: TSearchBox;
      btnGEOGCS: TSpeedButton;
      btnPROJCS: TSpeedButton;
      btnUNKNOWN: TSpeedButton;
      btnWKT: TButton;
      memWKT: TMemo;
      btnOK: TButton;
      btnCancel: TButton;
      procedure edtSearchTyping(Sender: TObject);
      procedure btnPROJCSClick(Sender: TObject);
      procedure btnUNKNOWNClick(Sender: TObject);
      procedure btnGEOGCSClick(Sender: TObject);
      procedure memWKTChangeTracking(Sender: TObject);
      procedure btnWKTClick(Sender: TObject);
      procedure btnOKClick(Sender: TObject);
      procedure btnCancelClick(Sender: TObject);
      procedure lstSystemsItemClick(const Sender: TCustomListBox;
        const Item: TListBoxItem);
      procedure FormCreate(Sender: TObject);
    private
      FCS : TGIS_CSCoordinateSystem ;
      oCS : TGIS_CSCoordinateSystem ;

      pModalProc : TProc<TModalResult> ;

      procedure init ;
      procedure doModalResult( _modal_result : TModalResult ) ;

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

    public

      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_onhelp">
      ///   not supported on mobile
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute  ( const _cs     : TGIS_CSCoordinateSystem;
                           const _onhelp : TGIS_HelpEvent         ;
                           const _proc   : TProc<TModalResult>
                         ) ; overload;

      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute ( const _cs   : TGIS_CSCoordinateSystem ;
                          const _proc : TProc<TModalResult>
                        ) ; overload;

      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_area_epsg">
      ///   epsg area specification;
      ///   <list type="bullet">
      ///     <item>
      ///       if _area_epsg=0 then systems matching _area_extent will be
      ///       presented, but also systems without specified area (AreaEPSG=0)
      ///     </item>
      ///     <item>
      ///       if _ara_epsg=1 then systems matching _area_extent will be
      ///       presented, but list will be limited to systems with specified
      ///       area (AreaEPSG>1)
      ///     </item>
      ///     <item>
      ///       if _area_epsg represents Area EPSG then only systems matching specified
      ///       area willl be presneted; _area_extent is not used in this case
      ///     </item>
      ///   </list>
      /// </param>
      /// <param name="_area_extent">
      ///   if _area_epsg is 0 or 1 then extent will be used to present systems with
      ///   matching area of use
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute ( const _cs          : TGIS_CSCoordinateSystem;
                          const _area_epsg   : Integer                ;
                          const _area_extent : TGIS_Extent            ;
                          const _onhelp      : TGIS_HelpEvent         ;
                          const _proc        : TProc<TModalResult>
                        ) ; overload;

    public
      /// <summary>
      ///  Coordinate system.
      /// </summary>
      property CS : TGIS_CSCoordinateSystem read FCS ;
  end;

var
  GIS_ControlCSSystem: TGIS_ControlCSSystem;

//##############################################################################
implementation

type
  T_CsMapper = class
    private
      FEPSG : Integer ;
    public
      constructor Create( const _epsg : Integer ) ;

      property EPSG : Integer read FEPSG ;
  end ;

{ T_CsMapper }

constructor T_CsMapper.Create(const _epsg: Integer);
begin
  inherited Create ;

  FEPSG := _epsg ;
end;

procedure TGIS_ControlCSSystem.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel ;
end;

procedure TGIS_ControlCSSystem.btnGEOGCSClick(Sender: TObject);
begin
  btnGEOGCS.IsPressed := True ;

  oCS := TGIS_CSFactory.ByEPSG( 0 ) ;

  lstSystems.Visible := True ;
  memWKT.Visible := False ;
  btnWKT.Visible := True ;
  edtSearchTyping( Sender );
end;

procedure TGIS_ControlCSSystem.btnPROJCSClick(Sender: TObject);
begin
  btnPROJCS.IsPressed := True ;

  oCS := TGIS_CSFactory.ByEPSG( 0 ) ;

  lstSystems.Visible := True ;
  memWKT.Visible := False ;
  btnWKT.Visible := True ;
  edtSearchTyping( Sender );
end;

procedure TGIS_ControlCSSystem.btnUNKNOWNClick(Sender: TObject);
begin
  btnUnknown.IsPressed := True ;

  oCS := TGIS_CSFactory.ByEPSG( 0 ) ;

  lstSystems.Visible := False ;
  memWKT.Visible := False ;
  btnWKT.Visible := False ;
  edtSearchTyping( Sender );
end;

function cs_from_friendlytext(
  const _txt : String
) : TGIS_CSCoordinateSystem ;
var
  k : Integer ;
  i : Integer ;
  tmp : String ;
begin
  k := Pos( 'alias:', _txt ) ;
  if k > 0 then k := k + 6 ;
  if k < 1 then begin
   k := Pos( 'epsg:', _txt ) ;
   k := k + 5 ;
  end;

  for i := k to k+10 do begin
    if CharInSet( _txt[i], [ '0'..'9' ] ) then
      tmp := tmp + _txt[i]
    else break ;
  end;

  if tmp <> '' then
    Result := TGIS_CsFactory.ByEPSG( StrToInt( tmp ) )
  else
    Result := nil ;
end;

procedure TGIS_ControlCSSystem.btnWKTClick(Sender: TObject);
var
  cs  : TGIS_CSCoordinateSystem ;
  obj : T_CsMapper ;
begin
  memWKT.Visible := True ;
  lstSystems.Visible := False ;

  if lstSystems.ItemIndex >= 0 then begin
    obj := T_CsMapper( lstSystems.Items.Objects[lstSystems.ItemIndex] ) ;
    cs := TGIS_CSFactory.ByEPSG( obj.EPSG ) ;

    if assigned( cs ) then
      memWKT.Text := cs.PrettyWKT
    else
      memWKT.Text := 'EMPTY' ;
  end
  else
    memWKT.Text := 'EMPTY' ;

  btnWKT.Visible := False ;
end;

procedure TGIS_ControlCSSystem.btnOKClick(Sender: TObject);
begin
  ModalResult := mrOK ;
end;

procedure TGIS_ControlCSSystem.edtSearchTyping(Sender: TObject);
var
  i : Integer ;
  cs : TGIS_CSAbstract ;

  function do_compare : Boolean ;
  begin
    if Assigned( oCS ) and ( cs.EPSG = oCS.EPSG ) then begin
      Result := False ;
      exit ;
    end;

    if edtSearch.Text = '' then begin
      Result := True ;
      exit ;
    end;

    Result := Pos( UpperCase( edtSearch.Text ), UpperCase( cs.FriendlyName )
                 ) >= StringFirst ;
  end;
begin
  lstSystems.Clear ;

  if btnGEOGCS.IsPressed then begin
    for i:=0 to CSGeographicCoordinateSystemList.Count-1 do begin
      if lstSystems.Count > 25 then break ;

      cs := CSGeographicCoordinateSystemList[i] ;
      if do_compare then
        lstSystems.Items.AddObject( cs.FriendlyName, T_CsMapper.Create( cs.EPSG ) ) ;
    end;

    if oCS is TGIS_CSGeographicCoordinateSystem then
      lstSystems.Items.InsertObject(0, oCS.FriendlyName, T_CsMapper.Create( oCS.EPSG ) );
  end
  else
  if btnPROJCS.IsPressed then begin
    for i:=0 to CSProjectedCoordinateSystemList.Count-1 do begin
      if lstSystems.Count > 25 then break ;

      cs := CSProjectedCoordinateSystemList[i] ;
      if do_compare then
        lstSystems.Items.AddObject( cs.FriendlyName, T_CsMapper.Create( cs.EPSG ) );
    end;

    if oCS is TGIS_CSProjectedCoordinateSystem then
      lstSystems.Items.InsertObject(0, oCS.FriendlyName, T_CsMapper.Create( oCS.EPSG ) );
  end ;

  if lstSystems.Count > 0 then
    lstSystems.ItemIndex := 0 ;

  if lstSystems.Count > 25 then
    lstSystems.Items.Add( _rsrcna( GIS_RS_CS_HELPER_MORE ) );
end;

procedure TGIS_ControlCSSystem.memWKTChangeTracking(Sender: TObject);
var
  cs : TGIS_CSCoordinateSystem ;
begin
  cs := TGIS_CSFactory.ByWKT( memWKT.Text ) ;

  if ( not Assigned( cs ) ) or ( cs is TGIS_CSUnknownCoordinateSystem ) then begin
    memWKT.TextSettings.FontColor := TAlphaColorRec.Red ;
    oCS := nil ;
  end
  else begin
    memWKT.TextSettings.FontColor := TAlphaColorRec.Black ;
    oCS := cs ;
  end;

  if oCS is TGIS_CSGeographicCoordinateSystem then
    btnGEOGCS.IsPressed := True
  else
  if oCS is TGIS_CSProjectedCoordinateSystem then
    btnPROJCS.IsPressed := True
  else
    btnUNKNOWN.IsPressed := True
end;

constructor TGIS_ControlCSSystem.Create(
  _owner : TComponent
) ;
begin
  inherited CreateNew( _owner, 0 ) ;
  init ;
end;

procedure TGIS_ControlCSSystem.init ;
var
  t : Single ;
begin
  if _rsbidi then
    Self.BiDiMode := bdRightToLeft
  else
    Self.BiDiMode := bdLeftToRight;

  Self.Left := 0 ;
  Self.Top := 0 ;
  Self.ClientHeight := 298 ;
  Self.ClientWidth := 306 ;

  Self.Caption := 'Form144' ;
  Self.BorderIcons := [TBorderIcon.biSystemMenu] ;
  Self.BorderStyle := TFmxFormBorderStyle.Single ;
  Self.Position := TFormPosition.OwnerFormCenter ;
  Self.FormFactor.Devices := [TDeviceKind.Desktop] ;

  Self.FormFactor.Width := 320 ;
  Self.FormFactor.Height := 480 ;
  Self.OnCreate := FormCreate ;
  Self.DesignerMasterStyle := 2 ;

  t := 8 ;

  btnGEOGCS := TSpeedButton.Create( Self ) ;
  btnGEOGCS.Parent := Self ;
  btnGEOGCS.StaysPressed := True ;
  btnGEOGCS.GroupName := 'CSSelector' ;
  btnGEOGCS.Position.X := 8 ;
  btnGEOGCS.Position.Y := t ;
  btnGEOGCS.Size.Width := 97 ;
  btnGEOGCS.Size.Height := 29 ;
  btnGEOGCS.Size.PlatformDefault := False ;
  btnGEOGCS.StyleLookup := 'toolbuttonleft' ;
  btnGEOGCS.Text := 'btnGEOGCS' ;
  btnGEOGCS.OnClick := btnGEOGCSClick ;

  btnPROJCS := TSpeedButton.Create( Self ) ;
  btnPROJCS.Parent := Self ;
  btnPROJCS.StaysPressed := True ;
  btnPROJCS.GroupName := 'CSSelector' ;
  btnPROJCS.Position.X := 104 ;
  btnPROJCS.Position.Y := t ;
  btnPROJCS.Size.Width := 97 ;
  btnPROJCS.Size.Height := 29 ;
  btnPROJCS.Size.PlatformDefault := False ;
  btnPROJCS.StyleLookup := 'toolbuttonmiddle' ;
  btnPROJCS.Text := 'btnPROJCS' ;
  btnPROJCS.OnClick := btnPROJCSClick ;

  btnUNKNOWN := TSpeedButton.Create( Self ) ;
  btnUNKNOWN.Parent := Self ;
  btnUNKNOWN.StaysPressed := True ;
  btnUNKNOWN.GroupName := 'CSSelector' ;
  btnUNKNOWN.Position.X := 200 ;
  btnUNKNOWN.Position.Y := t ;
  btnUNKNOWN.Size.Width := 97 ;
  btnUNKNOWN.Size.Height := 29 ;
  btnUNKNOWN.Size.PlatformDefault := False ;
  btnUNKNOWN.StyleLookup := 'toolbuttonright' ;
  btnUNKNOWN.Text := 'btnUNKNOWN' ;
  btnUNKNOWN.OnClick := btnUNKNOWNClick ;

  t := t + btnGEOGCS.Height + 3 ;

  lstSystems := TListBox.Create( Self ) ;
  lstSystems.Parent := Self ;
  lstSystems.Position.X := 8 ;
  lstSystems.Position.Y := t ;
  lstSystems.Size.Width := 289 ;
  lstSystems.Size.Height := 209 ;
  lstSystems.Size.PlatformDefault := False ;
  lstSystems.TabOrder := 0 ;
  lstSystems.DisableFocusEffect := True ;
  lstSystems.DefaultItemStyles.ItemStyle := 'listboxitemstyle' ;
  lstSystems.DefaultItemStyles.GroupHeaderStyle := '' ;
  lstSystems.DefaultItemStyles.GroupFooterStyle := '' ;
  lstSystems.OnItemClick := lstSystemsItemClick ;

  edtSearch := TSearchBox.Create( lstSystems ) ;
  edtSearch.Parent := lstSystems ;
  edtSearch.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                          TInteractiveGesture.DoubleTap] ;
  edtSearch.Align := TAlignLayout.Top ;
  edtSearch.TabOrder := 0 ;
  edtSearch.Size.Width := 289 ;
  edtSearch.Size.Height := 44 ;
  edtSearch.Size.PlatformDefault := False ;
  edtSearch.OnTyping := edtSearchTyping ;
  edtSearch.KillFocusByReturn := True ;

  t := t + lstSystems.Height + 7 ;

  btnWKT := TButton.Create( Self ) ;
  btnWKT.Parent := Self ;
  btnWKT.Position.X := 8 ;
  btnWKT.Position.Y := t ;
  btnWKT.Size.Width := 81 ;
  btnWKT.Size.Height := 25 ;
  btnWKT.Size.PlatformDefault := False ;
  btnWKT.TabOrder := 4 ;
  btnWKT.Text := 'btnWKT' ;
  btnWKT.OnClick := btnWKTClick ;

  memWKT := TMemo.Create( Self ) ;
  memWKT.Parent := Self ;
  memWKT.Touch.InteractiveGestures := [TInteractiveGesture.Pan,
                                       TInteractiveGesture.LongTap,
                                       TInteractiveGesture.DoubleTap] ;
  memWKT.StyledSettings := [TStyledSetting.Family, TStyledSetting.Size,
                            TStyledSetting.Style] ;
  memWKT.TextSettings.WordWrap := True ;
  memWKT.OnChangeTracking := memWKTChangeTracking ;
  memWKT.Position.X := 8 ;
  memWKT.Position.Y := lstSystems.Position.Y ;
  memWKT.Size.Width := 289 ;
  memWKT.Size.Height := 209 ;
  memWKT.Size.PlatformDefault := False ;
  memWKT.TabOrder := 5 ;
  memWKT.Visible := False ;

  btnOK := TButton.Create( Self ) ;
  btnOK.Parent := Self ;
  btnOK.Position.X := 216 ;
  btnOK.Position.Y := t ;
  btnOK.Size.Width := 81 ;
  btnOK.Size.Height := 25 ;
  btnOK.Size.PlatformDefault := False ;
  btnOK.TabOrder := 7 ;
  btnOK.Text := 'btnOK' ;
  btnOK.OnClick := btnOKClick ;

  btnCancel := TButton.Create( Self ) ;
  btnCancel.Parent := Self ;
  btnCancel.Position.X := 128 ;
  btnCancel.Position.Y := t ;
  btnCancel.Size.Width := 81 ;
  btnCancel.Size.Height := 25 ;
  btnCancel.Size.PlatformDefault := False ;
  btnCancel.TabOrder := 6 ;
  btnCancel.Text := 'btnCancel' ;
  btnCancel.OnClick := btnCancelClick ;

end ;

procedure TGIS_ControlCSSystem.Execute(
  const _cs     : TGIS_CSCoordinateSystem ;
  const _onhelp : TGIS_HelpEvent          ;
  const _proc   : TProc<TModalResult>
)  ;
var
  gcs : TGIS_CSGeographicCoordinateSystem ;
  pcs : TGIS_CSProjectedCoordinateSystem ;
  r : TModalResult ;
begin
  pModalProc := _proc ;

  FCS := _cs ;

  oCS := FCS ;
  if oCS is TGIS_CSGeographicCoordinateSystem then begin
    btnGEOGCSClick( self ) ;
  end
  else if oCS is TGIS_CSProjectedCoordinateSystem then begin
    btnPROJCSClick( self ) ;
  end
  else begin
    btnUNKNOWNClick( self ) ;
  end;

  oCS := FCS ;
  edtSearchTyping( self );

  ShowModalEx( doModalResult )
end;

procedure TGIS_ControlCSSystem.Execute(
  const _cs   : TGIS_CSCoordinateSystem ;
  const _proc : TProc<TModalResult>
) ;
var
  hlp : TGIS_HelpEvent ;
begin
  hlp := nil ;
  Execute( _cs, hlp, _proc ) ;
end;

procedure TGIS_ControlCSSystem.Execute(
  const _cs          : TGIS_CSCoordinateSystem ;
  const _area_epsg   : Integer                 ;
  const _area_extent : TGIS_Extent             ;
  const _onhelp      : TGIS_HelpEvent          ;
  const _proc        : TProc<TModalResult>
) ;
begin
  Execute( _cs, _onhelp, _proc ) ; // area is ignored
end;

procedure TGIS_ControlCSSystem.FormCreate(Sender: TObject);
begin
  Caption                := _rsrc  ( GIS_RS_CS_DLG            ) ;
  btnOK.Text             := _rsrcna( GIS_RS_BTN_OK            ) ;
  btnCancel.Text         := _rsrcna( GIS_RS_BTN_CANCEL        ) ;
  btnWKT.Text            := _rsrcna( GIS_RS_CS_ASWKT_MOBILE   ) ;
  btnGEOGCS.Text         := _rsrcna( GIS_RS_CS_GEOGCS_MOBILE  ) ;
  btnPROJCS.Text         := _rsrcna( GIS_RS_CS_PROJCS_MOBILE  ) ;
  btnUNKNOWN.Text        := _rsrcna( GIS_RS_CS_UNKNOWN_MOBILE ) ;

  {$IFDEF GIS_MOBILE_DIALOGS}
    btnOK.Visible     := False ;
    btnCancel.Visible := False ;
  {$ENDIF}

  ClientHeight := RoundS( btnWKT.Position.Y + btnWKT.Height + btnWKT.Position.X ) ;
  ClientWidth  := RoundS( memWKT.Position.X + memWKT.Width  + btnWKT.Position.X ) ;
end;

procedure TGIS_ControlCSSystem.lstSystemsItemClick(
 const Sender: TCustomListBox;
 const Item: TListBoxItem
) ;
var
  cs  : TGIS_CSCoordinateSystem ;
  obj : T_CsMapper ;
begin
  cs := nil ;
  if lstSystems.ItemIndex >= 0 then begin
    obj := T_CsMapper( lstSystems.Items.Objects[lstSystems.ItemIndex] ) ;
    cs := TGIS_CSFactory.ByEPSG( obj.EPSG ) ;
  end ;

  if Assigned( cs ) then
    oCS := cs ;
end;

procedure TGIS_ControlCSSystem.doModalResult(
  _modal_result : TModalResult
) ;
begin
  if _modal_result = mrOk then begin
    FCS := oCS ;
  end ;

  // bug on Mobiles upon releasing empty TEdit which was previously not empty
  edtSearch.Caret.Visible := False ;

  if Assigned( pModalProc ) then
    pModalProc( _modal_result ) ;
end ;

{==================================== END =====================================}


end.

