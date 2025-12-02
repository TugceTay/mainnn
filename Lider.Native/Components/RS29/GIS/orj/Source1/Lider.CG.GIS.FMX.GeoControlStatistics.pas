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
  Statistics. Dialog box for performing statistics calculation needed for classification.
}

unit Lider.CG.GIS.FMX.GeoControlStatistics ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlStatistics"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,

  FMX.Controls,
  FMX.Forms,
  FMX.Types,
  FMX.StdCtrls,
  FMX.Controls.Presentation,

  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoTypes,

  Lider.CG.GIS.FMX.GeoModalForm,
  Lider.CG.GIS.FMX.GeoControlHelper;

type

  /// <summary>
  ///   Dialog box to calculate statistics.
  /// </summary>
  TGIS_ControlStatistics = class( TGIS_ModalForm )

    private
      lblProgress : TLabel ;
      rdbFastScan : TRadioButton ;
      rdbFullScan : TRadioButton ;

    private
      procedure btnOKClick    (Sender: TObject) ; override ;
      procedure btnCancelClick(Sender: TObject) ; override ;

      procedure doBusyEvent(     _sender : TObject;
                                 _pos    : Integer ;
                                 _end    : Integer ;
                             var _abort  : Boolean
                           ) ;
    private
      objLayer : TGIS_Layer ;
      bAbort   : Boolean ;

    protected

      /// <inheritdoc/>
      procedure initForm         ; override ;

      /// <inheritdoc/>
      procedure initControls     ; override ;

    public
      /// <summary>
      ///   Execute dialog on a given layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be analyzed
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled
      /// </param>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      procedure Execute( const _layer  : TGIS_Layer ;
                         const _onhelp : TGIS_HelpEvent ;
                         const _proc   : TProc<TModalResult>
                       ) ; overload ;

      /// <summary>
      ///   Execute dialog on a given layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be analyzed
      /// </param>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      procedure Execute( const _layer  : TGIS_Layer ;
                         const _proc   : TProc<TModalResult>
                       ) ; overload ;

  end;

implementation

uses
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoStatistics;

procedure TGIS_ControlStatistics.doBusyEvent(
      _sender: TObject ;
      _pos   : Integer ;
      _end   : Integer ;
  var _abort : Boolean
) ;
begin
  // process progress
  if ( _pos <> -1 ) then
    lblProgress.Text := Format( '%d%%', [_pos] ) ;

  _abort := bAbort ;
  Application.ProcessMessages ;
end;

procedure TGIS_ControlStatistics.btnCancelClick(Sender: TObject);
begin
  bAbort := True ;
  inherited ;
end;

procedure TGIS_ControlStatistics.btnOKClick(Sender: TObject);
begin
  // update visibility of controls during the calculation
  btnOK.Enabled := False ;
  rdbFastScan.Visible := False ;
  rdbFullScan.Visible := False ;
  lblProgress.Visible := True ;

  bAbort := False ;

  TGIS_StatisticsLayer( objLayer.Statistics ).BusyEvent := doBusyEvent ;
  try
    objLayer.Statistics.Calculate(
      objLayer.Extent,
      nil,
      '',
      rdbFastScan.IsChecked
    ) ;
  finally
    TGIS_StatisticsLayer( objLayer.Statistics ).BusyEvent := nil ;
  end;

  // if calculation was aborted return mrCancel
  if bAbort then
    ModalResult := mrCancel
  else
    inherited ;
end;

procedure TGIS_ControlStatistics.initControls;
begin
  inherited;

  lblProgress := TLabel.Create( oMainForm ) ;
  lblProgress.Parent := oMainForm ;
  lblProgress.Position.Y := 48 ;
  lblProgress.Size.Height := 25 ;
  lblProgress.Size.Width:= 120 ;
  lblProgress.Size.PlatformDefault := False ;
  PlaceControl( BiDiMode, nil, lblProgress, 0, -1 ) ;
  lblProgress.StyledSettings := [TStyledSetting.Family, TStyledSetting.Style, TStyledSetting.FontColor] ;
  lblProgress.TextSettings.Font.Size := 24 ;
  lblProgress.TextSettings.HorzAlign := TTextAlign.Center ;
  lblProgress.Text := '' ;
  lblProgress.Visible := False ;
  lblProgress.FixSize ;

  rdbFastScan := TRadioButton.Create( oMainForm ) ;
  rdbFastScan.Parent := oMainForm ;
  rdbFastScan.Position.Y := 40 ;
  rdbFastScan.Size.Height := 17 ;
  PlaceControl( BiDiMode, nil, rdbFastScan, 80, 200 ) ;
  rdbFastScan.Size.PlatformDefault := False ;
  rdbFastScan.TabOrder := 0 ;
  rdbFastScan.Text := _rsrcna( GIS_RS_STATISTICS_DLG_FAST_SCAN ) ;
  rdbFastScan.FixSize ;
  rdbFastScan.IsChecked := True ;

  rdbFullScan := TRadioButton.Create( oMainForm ) ;
  rdbFullScan.Parent := oMainForm ;
  rdbFullScan.Position.Y := 64 ;
  rdbFullScan.Size.Height := 17 ;
  PlaceControl( BiDiMode, nil, rdbFullScan, 80, 200 ) ;
  rdbFullScan.Size.PlatformDefault := False ;
  rdbFullScan.TabOrder := 1 ;
  rdbFullScan.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBFULLSCAN ) ;
  rdbFullScan.FixSize ;
  rdbFullScan.IsChecked := False ;

  {$IFNDEF GIS_MOBILE_DIALOGS}
    btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
    btnHelp.TabOrder := 2 ;

    btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;
    btnCancel.TabOrder := 4 ;

    btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
    PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
    btnOK.TabOrder := 3 ;
  {$ENDIF}
end;

procedure TGIS_ControlStatistics.initForm;
begin
  inherited;

  Self.Name := 'TGIS_ControlStatistics' ;
  Self.Caption := _rsrcna( GIS_RS_STATISTICS_DLG_FORM_NAME ) ;
  Self.ClientHeight := 150 ;
  Self.ClientWidth := 350 ;
end;

procedure TGIS_ControlStatistics.Execute(
  const _layer  : TGIS_Layer ;
  const _onhelp : TGIS_HelpEvent ;
  const _proc   : TProc<TModalResult>
) ;
begin
  Assert( Assigned( _layer ) ) ;
  objLayer := _layer ;

  pOnHelp := _onhelp ;
  {$IFNDEF GIS_MOBILE_DIALOGS}
    btnHelp.Visible := assigned( pOnHelp ) ;
  {$ENDIF}

  ShowModalEx( _proc ) ;
end;

procedure TGIS_ControlStatistics.Execute(
  const _layer  : TGIS_Layer ;
  const _proc : TProc<TModalResult>
) ;
var
  hlp : TGIS_HelpEvent ;
begin
  hlp := nil ;
  Execute( _layer, hlp, _proc ) ;
end ;

end.

