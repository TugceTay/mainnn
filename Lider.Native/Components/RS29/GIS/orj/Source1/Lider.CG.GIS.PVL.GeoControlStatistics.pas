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
  Visual control for custom values.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlStatistics ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlStatistics"'}
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
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Generics.Collections,
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoStatistics,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoTypesUI,
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
  ///   Dialog box to calculate statistics.
  /// </summary>
  TGIS_ControlStatistics = class( TGIS_PvlModalForm )
    private
      {$IFDEF JAVA}
        tCalculations   : Thread                    ;
      {$ENDIF}
      objLayer    : TGIS_Layer                ;
      bAbort      : Boolean                   ;
      lblProgress : TGIS_PvlLabel             ;
      btnExecute  : TGIS_PvlButton            ;
      grpScan     : String                    ;
      rdbFastScan : TGIS_PvlRadioButton       ;
      rdbFullScan : TGIS_PvlRadioButton       ;

    public
      /// <inheritdoc/>
      procedure BtnOKClick(_sender: TObject); override;
      /// <inheritdoc/>
      procedure BtnCancelClick(_sender: TObject); override;

      {$IFDEF DCC}
        /// <inheritdoc/>
        procedure doBusyEvent(     _sender : TObject;
                                   _pos    : Integer ;
                                   _end    : Integer ;
                               var _abort  : Boolean
                             ) ;
      {$ELSE}
        /// <inheritdoc/>
        procedure doBusyEvent(     _sender : TObject ;
                                   _args   : TGIS_BusyEventArgs
                             ) ;

      {$ENDIF}

    private
      procedure onExecuteClick      ( _sender : TObject ) ;

    protected

      /// <inheritdoc/>
      procedure doDestroy ; override ;
    public

      /// <inheritdoc/>
      procedure DoInitForm; override;

      /// <inheritdoc/>
      procedure DoInitControls     ; override ;

    public

    /// <summary>
    ///   Execute dialog on a given layer.
    /// </summary>
    /// <param name="_layer">
    ///   layer to be analyzed
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute ( const _layer  : TGIS_Layer
                     ) : TGIS_PvlModalResult ; overload ;
    /// <summary>
    ///   Execute dialog on a given layer.
    /// </summary>
    /// <param name="_layer">
    ///   layer to be analyzed
    /// </param>
    /// <param name="_proc">
    ///   Action to be performed after closing modal form.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  Execute( const _layer  : TGIS_Layer;
                       const _proc   : TGIS_Proc
                     ) : TGIS_PvlModalResult ; overload ;

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
    ///   Action to be performed after closing modal form.
    /// </param>
    /// <returns>
    ///   Modal result.
    /// </returns>
    function  Execute( const _layer  : TGIS_Layer ;
                       const _onhelp : TGIS_HelpEvent ;
                       const _proc   : TGIS_Proc
                     ) : TGIS_PvlModalResult ; overload ;

  end;

//##############################################################################
implementation

  {$IFDEF DCC}
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
      if ( _pos = 100 ) then
        lblProgress.Caption := _rsrcna( GIS_RS_BTN_DONE ) ;

      _abort := bAbort ;
      DoUpdateGUI;
    end;
  {$ELSE}
    procedure TGIS_ControlStatistics.doBusyEvent(
      _sender : TObject ;
      _args   : TGIS_BusyEventArgs
    ) ;
    begin
      // process progress
      if ( _args.Pos <> -1 ) then
        lblProgress.Caption := Format( '%d%%', [_args.Pos] ) ;
      if ( _args.Pos = 100 ) then
        lblProgress.Caption := _rsrcna( GIS_RS_BTN_DONE ) ;
      if ( ( _args.Pos = -1 ) and ( _args.EndPos = -1 ) ) then
        BtnCancel.Enabled := False ;
      _args.Abort := bAbort ;

      doUpdateGUI ;
    end;
  {$ENDIF}

  procedure TGIS_ControlStatistics.btnCancelClick(_sender: TObject);
  begin
    bAbort := True ;
    objLayer.Statistics.ResetModified ;

    inherited ;

    {$IFDEF JAVA}
      if assigned( tCalculations ) then
        tCalculations.interrupt ;
    {$ENDIF}
  end;

  procedure TGIS_ControlStatistics.btnOKClick(_sender: TObject);
  begin
    bAbort := False ;
    inherited ;
  end;

  procedure TGIS_ControlStatistics.onExecuteClick(
    _sender: TObject
  ) ;
  begin
    // update visibility of controls during the calculation
    rdbFastScan.Visible := False ;
    rdbFullScan.Visible := False ;
    BtnOK.Visible := True ;
    BtnOK.Enabled := False ;
    btnExecute.Visible := False ;
    lblProgress.Visible := True ;

    bAbort := False ;

    {$IFDEF DCC}
      TGIS_StatisticsLayer( objLayer.Statistics ).BusyEvent := doBusyEvent ;
    {$ELSE}
      {$IFDEF JAVA}
        TGIS_StatisticsLayer( objLayer.Statistics ).BusyEvent := @doBusyEvent ;
      {$ELSE}
        TGIS_StatisticsLayer( objLayer.Statistics ).BusyEvent += new TGIS_BusyEvent( doBusyEvent ) ;
      {$ENDIF}
    {$ENDIF}

    {$IFDEF JAVA}
      //  new thread for code in order to leave gui thread free for updates
      tCalculations := Thread.Create(
        new class Runnable (
          run := method  begin
            objLayer.Statistics.Calculate(
              objLayer.Extent,
              nil,
              '',
              rdbFastScan.Checked
            ) ;

            BtnOK.Enabled := True ;
          end
        )
      ) ;
      tCalculations.start();
    {$ELSE}
      try
        objLayer.Statistics.Calculate(
          objLayer.Extent,
          nil,
          '',
          rdbFastScan.Checked
        ) ;
      finally
        {$IFDEF DCC}
          TGIS_StatisticsLayer( objLayer.Statistics ).BusyEvent := nil ;
        {$ENDIF}
      end;

      BtnOK.Enabled := True ;
    {$ENDIF}
  end;

  procedure TGIS_ControlStatistics.DoInitControls;
  begin
    BtnOK.Visible := False ;

    grpScan := 'grpScan' ;

    lblProgress := TGIS_PvlLabel.Create( Context ) ;
    lblProgress.Visible := False ;
    lblProgress.Place( ClientWidth- 2*Context.HMargin, 30, nil, Context.HMargin, nil, 20 ) ;
    lblProgress.Alignment := TGIS_PvlLabelTextAlignment.Center ;
    lblProgress.FontStyle := [ TGIS_FontStyle.Bold ];

    rdbFastScan := TGIS_PvlRadioButton.Create( Context ) ;
    rdbFastScan.Group := grpScan ;
    rdbFastScan.Place( 200, 0, nil, 80, nil, Context.VMargin ) ;
    rdbFastScan.Caption := _rsrc( GIS_RS_STATISTICS_DLG_FAST_SCAN ) ;

    rdbFullScan := TGIS_PvlRadioButton.Create( Context ) ;
    rdbFullScan.Group := grpScan ;
    rdbFullScan.Place( 200, 0, nil, 80, rdbFastScan, 0 ) ;
    rdbFullScan.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBFULLSCAN ) ;

    btnExecute := TGIS_PvlButton.Create( Context ) ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      btnExecute.Place( BtnOK.Width, BtnOK.Height, nil, -Context.HSpace, nil, Self.ClientHeight - BtnCancel.Height - Context.VMargin ) ;
    {$ELSE}
      btnExecute.Place( BtnOK.Width, BtnOK.Height, BtnCancel, -Context.HSpace, nil, BtnCancel.Top ) ;
    {$ENDIF}
    btnExecute.Caption := BtnOK.Caption ;

    {$IFDEF DCC}
      btnExecute.OnClick := onExecuteClick ;
    {$ELSE}
      btnExecute.OnClick := @onExecuteClick ;
    {$ENDIF}
  end;

  procedure TGIS_ControlStatistics.doDestroy ;
  begin
    inherited ;
  end;

  procedure TGIS_ControlStatistics.DoInitForm;
  begin
    Self.Caption := _rsrcna( GIS_RS_STATISTICS_DLG_FORM_NAME ) ;
    Self.ClientHeight := 100 ;
    Self.ClientWidth := 350 ;
    Self.Name := 'TGIS_ControlStatistics' ;
  end;

  function TGIS_ControlStatistics.Execute(
    const _layer : TGIS_Layer
  ) : TGIS_PvlModalResult;
  begin
    Result := Execute( _layer, nil ) ;
  end ;

  function TGIS_ControlStatistics.Execute(
    const _layer  : TGIS_Layer;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, hlp, _proc ) ;
  end ;

  function TGIS_ControlStatistics.Execute(
    const _layer  : TGIS_Layer ;
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  begin
    assert( assigned( _layer ) ) ;
    objLayer := _layer ;

    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    rdbFastScan.Checked := True ;

    rdbFullScan.Checked:= not rdbFastScan.Checked ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end;

//==================================== END =====================================
end.
