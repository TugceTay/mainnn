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

unit Lider.CG.GIS.VCL.GeoControlStatistics ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlStatistics"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Classes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.StdCtrls,
  VCL.Buttons,

  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.VCL.GeoModalForm,
  Lider.CG.GIS.VCL.GeoControlHelper;
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
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute( const _layer  : TGIS_Layer ;
                      const _onhelp : TGIS_HelpEvent
                    ) : Integer ; overload ;

    /// <summary>
    ///   Execute dialog on a given layer.
    /// </summary>
    /// <param name="_layer">
    ///   layer to be analyzed
    /// </param>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute( const _layer  : TGIS_Layer
                    ) : Integer ; overload ;
  end;

implementation

uses
  System.SysUtils,

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
    lblProgress.Caption := Format( '%d%%', [_pos] ) ;

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
      rdbFastScan.Checked
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

  if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
  else
    anchors := [akLeft, akTop] ;

  lblProgress := TLabel.Create( Self ) ;
  lblProgress.Parent := Self ;
  lblProgress.Anchors := Anchors ;
  lblProgress.AlignWithMargins := True ;
  lblProgress.Align := alTop ;
  lblProgress.Alignment := taCenter ;
  lblProgress.Margins.Top := 48 ;
  PlaceControl( BiDiMode, nil, lblProgress, 16, -1 ) ;
  lblProgress.Caption := '' ;
  // multiply default font size (to work properly on HDPI)
  lblProgress.Font.Size := lblProgress.Font.Size * 3 ;
  lblProgress.Visible := False ;

  rdbFastScan := TRadioButton.Create( Self ) ;
  rdbFastScan.Parent := Self ;
  rdbFastScan.Anchors := Anchors ;
  rdbFastScan.Top := 42 ;
  rdbFastScan.Height := 17 ;
  PlaceControl( BiDiMode, nil, rdbFastScan, 80, 200 ) ;
  rdbFastScan.Caption := _rsrc( GIS_RS_STATISTICS_DLG_FAST_SCAN ) ;
  rdbFastScan.TabOrder := 0 ;
  rdbFastScan.Checked := True ;

  rdbFullScan := TRadioButton.Create( Self ) ;
  rdbFullScan.Parent := Self ;
  rdbFullScan.Anchors := Anchors ;
  rdbFullScan.Top := 65 ;
  rdbFullScan.Height := 17 ;
  PlaceControl( BiDiMode, nil, rdbFullScan, 80, 200 ) ;
  rdbFullScan.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBFULLSCAN ) ;
  rdbFullScan.TabOrder := 1 ;
  rdbFullScan.Checked := True ;

  btnHelp.TabOrder := 2 ;
  btnOK.TabOrder := 3 ;
  btnCancel.TabOrder := 4 ;
end;

procedure TGIS_ControlStatistics.initForm;
begin
  inherited;

  Self.Name := 'TGIS_ControlStatistics' ;
  Self.Caption := _rsrc( GIS_RS_STATISTICS_DLG_FORM_NAME ) ;
  Self.ClientHeight := 150 ;
  Self.ClientWidth := 350 ;
end;

function TGIS_ControlStatistics.Execute(
  const _layer  : TGIS_Layer ;
  const _onhelp : TGIS_HelpEvent
) : Integer ;
begin
  Assert( Assigned( _layer ) ) ;
  objLayer := _layer ;

  pOnHelp := _onhelp ;
  btnHelp.Visible := Assigned( pOnHelp ) ;

  Result := ShowModal ;

  if ( Result = mrCancel ) then
    objLayer.Statistics.ResetModified ;
end;

function TGIS_ControlStatistics.Execute(
  const _layer  : TGIS_Layer
) : Integer ;
begin
  pOnHelp := nil ;
  Execute( _layer, pOnHelp ) ;
end;

end.

