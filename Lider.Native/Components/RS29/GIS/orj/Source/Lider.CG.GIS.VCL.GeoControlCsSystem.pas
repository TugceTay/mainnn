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

unit VCL.GisControlCsSystem ;
{$HPPEMIT '#pragma link "VCL.GisControlCsSystem"'}

interface

{$INCLUDE GisInclude.inc}

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

  GisRtl,
  GisTypes,

  GisCsSystems,
  VCL.GisModalForm,
  VCL.GisControlCsHelper;

type

  /// <summary>
  ///   Dialog box to select coordinate system from the lists of existing
  ///   (predefined) Coordinate Systems.
  /// </summary>
  TGIS_ControlCSSystem = class( TGIS_ModalForm )
    // form
      rdbGEOGCS: TRadioButton;
      rdbPROJCS: TRadioButton;
      rdbUNKNOWN: TRadioButton;
      cmbGEOGCS: TComboBox;
      btnGEOGCS: TSpeedButton;
      cmbPROJCS: TComboBox;
      btnPROJCS: TSpeedButton;

    // form
      procedure frmCreate(Sender: TObject);
      procedure frmDestroy(Sender: TObject);
      procedure rdbGEOGCSClick(Sender: TObject);
      procedure btnPROJCSClick(Sender: TObject);
      procedure btnGEOGCSClick(Sender: TObject);
      procedure rdbPROJCSClick(Sender: TObject);
      procedure rdbUNKNOWNClick(Sender: TObject);

    private
      FCS : TGIS_CSCoordinateSystem ;

      hlpGEOGCS : TGIS_CSAbstractListHelper ;
      hlpPROJCS : TGIS_CSAbstractListHelper ;

    protected

      /// <inheritdoc/>
      procedure initForm         ; override;

      /// <inheritdoc/>
      procedure initControls     ; override;

      /// <inheritdoc/>
      procedure showForm         ; override;

      /// <inheritdoc/>
      procedure afterPPIChanged  ; override;

    public

      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute  ( const _cs          : TGIS_CSCoordinateSystem;
                          const _onhelp      : TGIS_HelpEvent
                        ) : Integer ; overload;


      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute  ( const _cs     : TGIS_CSCoordinateSystem
                        ) : Integer ; overload;

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
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute  ( const _cs          : TGIS_CSCoordinateSystem;
                          const _area_epsg   : Integer ;
                          const _area_extent : TGIS_Extent;
                          const _onhelp      : TGIS_HelpEvent
                        ) : Integer ; overload;
    public
      /// <summary>
      /// Coordinate system.
      /// </summary>
      property CS : TGIS_CSCoordinateSystem read FCS ;
  end;

//##############################################################################
implementation

uses
  GisResource,
  GisFunctions,
  GisCsFactory,
  VCL.GisControlHelper,
  VCL.GisControlCsSystemSetup ;

//==============================================================================
// GEOGCS events
//==============================================================================

  procedure TGIS_ControlCSSystem.rdbGEOGCSClick(Sender: TObject);
  begin
    cmbGEOGCS.Enabled  := True  ;
    btnGEOGCS.Enabled  := True  ;

    cmbPROJCS.Enabled  := False ;
    btnPROJCS.Enabled  := False ;

    ActiveControl := cmbGEOGCS ;
  end;

  procedure TGIS_ControlCSSystem.btnGEOGCSClick(
    Sender: TObject
  );
  var
    dlg : TGIS_ControlCSSystemSetup ;
  begin
      if rdbUNKNOWN.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( 0 ) ;
      end
      else if rdbGEOGCS.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpGEOGCS.SelectedObject.EPSG ) ;
      end
      else if rdbPROJCS.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpPROJCS.SelectedObject.EPSG ) ;
      end;
    dlg := TGIS_ControlCSSystemSetup.Create( Self );
    try
      dlg.Execute( FCS, pOnHelp ) ;
      FCS := dlg.CS ;
    finally
      dlg.Free ;
    end;

    if not Assigned( FCS ) then
      FCS := CSUnknownCoordinateSystem
    else
      hlpGEOGCS.ByEPSG( FCS.EPSG ) ;
  end;

//==============================================================================
// PROJCS events
//==============================================================================

  procedure TGIS_ControlCSSystem.rdbPROJCSClick(
    Sender: TObject
  ) ;
  begin
    cmbGEOGCS.Enabled  := False ;
    btnGEOGCS.Enabled  := False ;

    cmbPROJCS.Enabled  := True  ;
    btnPROJCS.Enabled  := True  ;

    ActiveControl := cmbPROJCS ;
  end;

  procedure TGIS_ControlCSSystem.btnPROJCSClick(
    Sender: TObject
  );
  var
    dlg : TGIS_ControlCSSystemSetup ;
  begin
      if rdbUNKNOWN.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( 0 ) ;
      end
      else if rdbGEOGCS.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpGEOGCS.SelectedObject.EPSG ) ;
      end
      else if rdbPROJCS.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpPROJCS.SelectedObject.EPSG ) ;
      end;
    dlg := TGIS_ControlCSSystemSetup.Create( Self );
    try
      dlg.Execute( FCS, pOnHelp ) ;
      FCS := dlg.CS ;
    finally
      dlg.Free ;
    end;
    if FCS.EPSG > 0 then
      hlpPROJCS.ByEPSG( FCS.EPSG )
    else
      rdbUNKNOWN.Checked := True ;
  end;

//==============================================================================
// UNKNOWN events
//==============================================================================

  procedure TGIS_ControlCSSystem.rdbUNKNOWNClick(Sender: TObject);
  begin
    cmbGEOGCS.Enabled  := False ;
    btnGEOGCS.Enabled  := False ;

    cmbPROJCS.Enabled := False ;
    btnPROJCS.Enabled := False ;
  end;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlCSSystem.frmCreate(
    Sender: TObject
  );
  begin
    hlpGEOGCS  :=  TGIS_CSAbstractListHelper.Create(
                    cmbGEOGCS,
                    CSGeographicCoordinateSystemList
                  ) ;
    hlpPROJCS :=  TGIS_CSAbstractListHelper.Create(
                    cmbPROJCS,
                    CSProjectedCoordinateSystemList
                  ) ;

    rdbPROJCS.Checked := True ;
  end;

  procedure TGIS_ControlCSSystem.frmDestroy(
    Sender: TObject
  );
  begin
    FreeObject( hlpGEOGCS ) ;
    FreeObject( hlpPROJCS ) ;
  end;

//==============================================================================
// Various
//==============================================================================

  procedure TGIS_ControlCSSystem.initForm  ;
  begin
    Self.Caption := _rsrc( GIS_RS_CS_DLG ) ;
    Self.ClientHeight := 158 ;
    Self.ClientWidth := 426 ;
    Self.OnCreate  := frmCreate  ;
    Self.OnDestroy := frmDestroy ;
    Self.Name := 'TGIS_ControlCSSystem' ;
  end ;

  procedure TGIS_ControlCSSystem.initControls ;
  var
    anchors : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;

    rdbPROJCS := TRadioButton.Create( Self ) ;
    rdbPROJCS.Parent := Self ;
    rdbPROJCS.Anchors := anchors ;
    rdbPROJCS.Top := 8 ;
    rdbPROJCS.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbPROJCS, 8, 376 ) ;
    rdbPROJCS.Caption := _rsrc( GIS_RS_CS_PROJCS ) ;
    rdbPROJCS.TabOrder := 0 ;
    rdbPROJCS.OnClick := rdbPROJCSClick ;

    cmbPROJCS := TComboBox.Create( Self ) ;
    cmbPROJCS.Parent := Self ;
    cmbPROJCS.Anchors := anchors ;
    cmbPROJCS.Top := rdbPROJCS.Top + rdbPROJCS.Height ;
    PlaceControl( BiDiMode, nil, cmbPROJCS, 24, 360 ) ;
    cmbPROJCS.TabOrder := 1 ;
    cmbPROJCS.Text := 'cmbPROJCS' ;

    btnPROJCS := TSpeedButton.Create( Self ) ;
    btnPROJCS.Parent := Self ;
    btnPROJCS.Anchors := anchors ;
    btnPROJCS.Top := cmbPROJCS.Top ;
    PlaceControl( BiDiMode, cmbPROJCS, btnPROJCS, 6, cmbPROJCS.Height ) ;
    btnPROJCS.Caption := '...' ;
    btnPROJCS.OnClick := btnPROJCSClick ;

    rdbGEOGCS := TRadioButton.Create( Self ) ;
    rdbGEOGCS.Parent := Self ;
    rdbGEOGCS.Anchors := anchors ;
    rdbGEOGCS.Top := cmbPROJCS.Top + cmbPROJCS.Height + 6 ;
    rdbGEOGCS.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbGEOGCS, 8, 376 ) ;
    rdbGEOGCS.Caption := _rsrc( GIS_RS_CS_GEOGCS ) ;
    rdbGEOGCS.TabOrder := 2 ;
    rdbGEOGCS.OnClick := rdbGEOGCSClick ;

    cmbGEOGCS := TComboBox.Create( Self ) ;
    cmbGEOGCS.Parent := Self ;
    cmbGEOGCS.Anchors := anchors ;
    cmbGEOGCS.Top := rdbGEOGCS.Top + rdbGEOGCS.Height ;
    PlaceControl( BiDiMode, nil, cmbGEOGCS, 24, 360 ) ;
    cmbGEOGCS.TabOrder := 3 ;
    cmbGEOGCS.Text := 'cmbGEOGCS' ;

    btnGEOGCS := TSpeedButton.Create( Self ) ;
    btnGEOGCS.Parent := Self ;
    btnGEOGCS.Anchors := anchors ;
    btnGEOGCS.Top := cmbGEOGCS.Top ;
    PlaceControl( BiDiMode, cmbGEOGCS, btnGEOGCS, 6, cmbGEOGCS.Height ) ;
    btnGEOGCS.Caption := '...' ;
    btnGEOGCS.OnClick := btnGEOGCSClick ;

    rdbUNKNOWN := TRadioButton.Create( Self ) ;
    rdbUNKNOWN.Parent := Self ;
    rdbUNKNOWN.Anchors := anchors ;
    rdbUNKNOWN.Top := cmbGEOGCS.Top + cmbGEOGCS.Height + 6 ;
    PlaceControl( BiDiMode, nil, rdbUNKNOWN, 8, 376 ) ;
    rdbUNKNOWN.Caption := _rsrc( GIS_RS_CS_UNKNOWN ) ;
    rdbUNKNOWN.TabOrder := 4 ;
    rdbUNKNOWN.OnClick := rdbUNKNOWNClick ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 5 ;
    btnCancel.TabOrder := 7 ;
    btnOK.TabOrder := 6 ;

  end ;

  procedure TGIS_ControlCSSystem.showForm ;
  begin
    afterPPIChanged ;
  end ;

  procedure TGIS_ControlCSSystem.afterPPIChanged ;
  begin
    btnPROJCS.Top := cmbPROJCS.Top ;
    btnPROJCS.Height := cmbPROJCS.Height ;
    PlaceControl( BiDiMode, cmbPROJCS, btnPROJCS, ppiFix(6), cmbPROJCS.Height ) ;
    btnGEOGCS.Top := cmbGEOGCS.Top ;
    btnGEOGCS.Height := cmbGEOGCS.Height ;
    PlaceControl( BiDiMode, cmbGEOGCS, btnGEOGCS, ppiFix(6), cmbGEOGCS.Height ) ;
    ClientWidth  := ppiFix(24) + cmbPROJCS.Width + ppiFix(6) +
                    btnPROJCS.Width + ppiFix(8) ;
    ClientHeight := rdbUNKNOWN.Top + rdbUNKNOWN.Height +
                    ppiFix(16) + btnOK.Height + ppiFix(8) ;
  end ;

  function TGIS_ControlCSSystem.Execute(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  begin
    Result := Execute( _cs, 0, GisNoWorld(), _onhelp );
  end ;

  function TGIS_ControlCSSystem.Execute(
    const _cs : TGIS_CSCoordinateSystem
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _cs, hlp ) ;
  end;

  function TGIS_ControlCSSystem.Execute(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _area_epsg : Integer ;
    const _area_extent : TGIS_Extent;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    gcs : TGIS_CSGeographicCoordinateSystem ;
    pcs : TGIS_CSProjectedCoordinateSystem ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    FCS := _cs ;

    hlpGEOGCS.AreaEPSG   := _area_epsg;
    hlpGEOGCS.AreaExtent := _area_extent ;

    hlpPROJCS.AreaEPSG   := _area_epsg;
    hlpPROJCS.AreaExtent := _area_extent ;

    hlpGEOGCS.ByEpsg ( GIS_EPSG_WGS84 ) ;
    hlpPROJCS.ByEpsg( 3786 ) ;

    if FCS is TGIS_CSGeographicCoordinateSystem then begin
      rdbGEOGCS.Checked := True  ;

      gcs := _cs as TGIS_CSGeographicCoordinateSystem ;
      hlpGEOGCS.ByEpsg( gcs.EPSG ) ;
    end
    else if _cs is TGIS_CSProjectedCoordinateSystem then begin
      rdbPROJCS.Checked := True  ;

      pcs := _cs as TGIS_CSProjectedCoordinateSystem ;
      hlpGEOGCS.ByEpsg( pcs.Geocs.EPSG ) ;
      hlpPROJCS.ByEpsg( pcs.EPSG ) ;
    end
    else begin
      rdbUNKNOWN.Checked := True  ;
    end;

    Result := ShowModal ;

    if Result = mrOk then begin
      if rdbUNKNOWN.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( 0 ) ;
      end
      else if rdbGEOGCS.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpGEOGCS.SelectedObject.EPSG ) ;
      end
      else if rdbPROJCS.Checked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpPROJCS.SelectedObject.EPSG ) ;
      end;
    end;
  end;


{==================================== END =====================================}
end.

