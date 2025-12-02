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
  Coordinate System. Dialog box to select from predefined list.
}

unit Lider.CG.GIS.FMX.GeoControlCsSystem ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlCsSystem"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_MOBILE}
  {$ERROR 'On Mobile platforms use Lider.CG.GIS.FMX.Mobile.GeoControlCsSystem' }
{$ENDIF}

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
  FMX.Controls.Presentation,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoCsSystems,

  Lider.CG.GIS.FMX.GeoModalForm,
  Lider.CG.GIS.FMX.GeoControlCsHelper;

type
  /// <summary>
  ///   Dialog box to select coordinate system from the lists of existing
  ///   (predefined) Coordinate Systems.
  /// </summary>
  TGIS_ControlCSSystem = class(TGIS_ModalForm)
    // form
    rdbGEOGCS: TRadioButton;
    rdbPROJCS: TRadioButton;
    rdbUNKNOWN: TRadioButton;
    cmbGEOGCS: Lider.CG.GIS.FMX.GeoControlCsHelper.TComboEdit;
    cmbPROJCS: Lider.CG.GIS.FMX.GeoControlCsHelper.TComboEdit;
    btnGEOGCS: TCornerButton;
    btnPROJCS: TCornerButton;

    // form
    procedure frmCreate       (     _sender  : TObject );
    procedure frmDestroy      (     _sender  : TObject
                              );
    procedure frmShow         (     _sender  : TObject
                              );
    procedure btnPROJCSClick  (     _sender  : TObject );
    procedure btnGEOGCSClick  (     _sender  : TObject );
    procedure rdbPROJCSChange (     _sender  : TObject );
    procedure rdbGEOGCSChange (     _sender  : TObject );
    procedure rdbUNKNOWNChange(     _sender  : TObject );

    private
      FCS : TGIS_CSCoordinateSystem ;

      pModalProc : TProc<TModalResult> ;

      hlpGEOGCS : TGIS_CSAbstractListHelper ;
      hlpPROJCS : TGIS_CSAbstractListHelper ;

      procedure doModalResult(
                               _modal_result : TModalResult
                             ) ;

      procedure doExecute    ( const _cs     : TGIS_CSCoordinateSystem;
                               const _onhelp : TGIS_HelpEvent         ;
                               const _proc   : TProc<TModalResult>    ;
                               const _free   : Boolean
                             ) ;
    protected

      /// <inheritdoc/>
      procedure initForm     ; override;

      /// <inheritdoc/>
      procedure initControls ; override;


    public

      {#gendoc:hide:GENPDK}
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
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute      ( const _cs     : TGIS_CSCoordinateSystem;
                               const _onhelp : TGIS_HelpEvent         ;
                               const _proc   : TProc<TModalResult>
                             ) ; overload;

      {#gendoc:hide:GENPDK}
      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute      ( const _cs   : TGIS_CSCoordinateSystem ;
                               const _proc : TProc<TModalResult>
                             ) ; overload;

      {$IFDEF GIS_PDK}
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
        /// <param name="_proc">
        ///   procedure to be executed upon modal result
        /// </param>
        function Execute     ( const _cs     : TGIS_CSCoordinateSystem;
                               const _onhelp : TGIS_HelpEvent
                             ) : TModalResult; overload;

        /// <summary>
        ///   Execute dialog on a given coordinate system.
        /// </summary>
        /// <param name="_cs">
        ///   coordinate system
        /// </param>
        /// <returns>
        ///   Modal result.
        /// </returns>
        function Execute     ( const _cs   : TGIS_CSCoordinateSystem
                             ) : TModalResult; overload;
      {$ENDIF}
    public
      /// <summary>
      ///  Coordinate system.
      /// </summary>
      property CS : TGIS_CSCoordinateSystem read FCS ;
  end;

//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoCsFactory,

  Lider.CG.GIS.FMX.GeoControlCsSystemSetup,
  Lider.CG.GIS.FMX.GeoControlHelper;

//==============================================================================
// GEOGCS events
//==============================================================================

  procedure TGIS_ControlCSSystem.rdbGEOGCSChange(
    _sender : TObject
  );
  begin
    cmbGEOGCS.Enabled     := True  ;
    btnGEOGCS.Enabled     := True  ;

    cmbPROJCS.Enabled     := False ;
    cmbPROJCS.DroppedDown := False ;
    btnPROJCS.Enabled     := False ;

    ActiveControl := cmbGEOGCS ;
    cmbGEOGCS.SetFocus ; // WIN32 Bug
  end;

  procedure TGIS_ControlCSSystem.btnGEOGCSClick(
    _sender: TObject
  );
  var
    dlg : TGIS_ControlCSSystemSetup ;
  begin
    if rdbUNKNOWN.IsChecked then begin
      FCS := TGIS_CSFactory.ByEPSG( 0 ) ;
    end
    else if rdbGEOGCS.IsChecked then begin
      FCS := TGIS_CSFactory.ByEPSG( hlpGEOGCS.SelectedObject.EPSG ) ;
    end
    else if rdbPROJCS.IsChecked then begin
      FCS := TGIS_CSFactory.ByEPSG( hlpPROJCS.SelectedObject.EPSG ) ;
    end;

    ActiveControl := btnGEOGCS ;
    dlg := TGIS_ControlCSSystemSetup.Create( oMainForm );
    dlg.Execute(
      FCS,
      pOnHelp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result = mrOk then
          ActiveControl := btnOK
        else
          ActiveControl := cmbGEOGCS ;

        FCS := dlg.CS ;

        if not Assigned( FCS ) then
          FCS := CSUnknownCoordinateSystem
        else
          hlpGEOGCS.ByEPSG( FCS.EPSG ) ;
      end
    ) ;
 end;

//==============================================================================
// PROJCS events
//==============================================================================

  procedure TGIS_ControlCSSystem.rdbPROJCSChange(
    _sender : TObject
  );
  begin
    cmbGEOGCS.Enabled     := False ;
    cmbGEOGCS.DroppedDown := False ;
    btnGEOGCS.Enabled     := False ;

    cmbPROJCS.Enabled     := True  ;
    btnPROJCS.Enabled     := True  ;

    ActiveControl := cmbPROJCS ;
    cmbPROJCS.SetFocus ; // WIN32 Bug
  end;

  procedure TGIS_ControlCSSystem.btnPROJCSClick(
    _sender: TObject
  );
  var
    dlg : TGIS_ControlCSSystemSetup ;
  begin
    if rdbUNKNOWN.IsChecked then begin
      FCS := TGIS_CSFactory.ByEPSG( 0 ) ;
    end
    else if rdbGEOGCS.IsChecked then begin
      FCS := TGIS_CSFactory.ByEPSG( hlpGEOGCS.SelectedObject.EPSG ) ;
    end
    else if rdbPROJCS.IsChecked then begin
      FCS := TGIS_CSFactory.ByEPSG( hlpPROJCS.SelectedObject.EPSG ) ;
    end;

    ActiveControl := btnPROJCS ;
    dlg := TGIS_ControlCSSystemSetup.Create( oMainForm );
    dlg.Execute(
      FCS,
      pOnHelp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result = mrOk then
          ActiveControl := btnOK
        else
          ActiveControl := cmbPROJCS ;

        FCS := dlg.CS ;

        if FCS.EPSG > 0 then
          hlpPROJCS.ByEPSG( FCS.EPSG )
        else begin
          rdbUNKNOWN.IsChecked := True ;
          rdbUNKNOWNChange( oMainForm ) ;
        end;
      end
    ) ;
  end;

//==============================================================================
// UNKNOWN events
//==============================================================================

  procedure TGIS_ControlCSSystem.rdbUNKNOWNChange(
    _sender : TObject
  );
  begin
    cmbGEOGCS.Enabled     := False ;
    cmbGEOGCS.DroppedDown := False ;
    btnGEOGCS.Enabled     := False ;

    cmbPROJCS.Enabled     := False ;
    cmbPROJCS.DroppedDown := False ;
    btnPROJCS.Enabled     := False ;
  end;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlCSSystem.frmCreate(
    _sender: TObject
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
  end;

  procedure TGIS_ControlCSSystem.frmDestroy(
    _sender: TObject
  );
  begin
    FreeObject( hlpGEOGCS ) ;
    FreeObject( hlpPROJCS ) ;
  end;

  procedure TGIS_ControlCSSystem.frmShow(
    _sender: TObject
  );
  begin
    // bypass FMX problem with a proper showing Caret on compound buttons
    // by forcing focus again
    if FCS is TGIS_CSGeographicCoordinateSystem then begin
      btnGEOGCS.SetFocus ;
      cmbGEOGCS.SetFocus ;
    end
    else if FCS is TGIS_CSProjectedCoordinateSystem then begin
      btnPROJCS.SetFocus ;
      cmbPROJCS.SetFocus ;
    end;
  end;

//==============================================================================
// Various
//==============================================================================

  procedure TGIS_ControlCSSystem.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_CS_DLG ) ;
    Self.ClientHeight := 158 ;
    Self.ClientWidth := 426 ;
    Self.Name := 'TGIS_ControlCSSystem' ;
    Self.OnCreate := frmCreate ;
    Self.OnDestroy := frmDestroy ;
    Self.OnShow := frmShow ;
  end ;

  procedure TGIS_ControlCSSystem.initControls ;
  begin
    rdbPROJCS := TRadioButton.Create( oMainForm ) ;
    rdbPROJCS.Parent := oMainForm ;
    rdbPROJCS.Position.Y := 8 ;
    rdbPROJCS.Size.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbPROJCS, 8, 376 ) ;
    rdbPROJCS.Size.PlatformDefault := False ;
    rdbPROJCS.TabOrder := 0 ;
    rdbPROJCS.Text := _rsrcna( GIS_RS_CS_PROJCS  ) ;
    rdbPROJCS.GroupName := 'TGIS_ControlCSSystem' ;
    rdbPROJCS.OnChange := rdbPROJCSChange ;
    rdbPROJCS.FixSize ;

    cmbPROJCS := TComboEdit.Create( oMainForm ) ;
    cmbPROJCS.Parent := oMainForm ;
    cmbPROJCS.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                            TInteractiveGesture.DoubleTap] ;
    cmbPROJCS.TabOrder := 0 ;
    cmbPROJCS.ItemHeight := 19 ;
    cmbPROJCS.ItemIndex := -1 ;
    cmbPROJCS.ListBoxResource := 'combolistboxstyle' ;
    cmbPROJCS.Text := 'cmbPROJCS' ;
    cmbPROJCS.Position.Y := 25 ;
    cmbPROJCS.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbPROJCS, 30, 354 ) ;
    cmbPROJCS.Size.PlatformDefault := False ;
    cmbPROJCS.TabOrder := 1 ;

    btnPROJCS := TCornerButton.Create( oMainForm ) ;
    btnPROJCS.Parent := oMainForm ;
    if BiDiMode = TBiDiMode.bdRightToLeft  then
      btnPROJCS.Corners := [TCorner.TopLeft, TCorner.BottomLeft]
    else
      btnPROJCS.Corners := [TCorner.TopRight, TCorner.BottomRight] ;
    btnPROJCS.Cursor := crArrow ;
    btnPROJCS.Sides := [TSide.Top, TSide.Left, TSide.Bottom, TSide.Right] ;
    btnPROJCS.Size.Height := 22 ;
    PlaceControl( BiDiMode, cmbPROJCS, btnPROJCS, 6, 30 ) ;
    btnPROJCS.Size.PlatformDefault := False ;
    btnPROJCS.TabOrder := 2 ;
    if BiDiMode = TBiDiMode.bdRightToLeft  then
      btnPROJCS.Text := '<'
    else
      btnPROJCS.Text := '>' ;
    btnPROJCS.XRadius := 10 ;
    btnPROJCS.YRadius := 10 ;
    btnPROJCS.OnClick := btnPROJCSClick ;
    btnPROJCS.AlignVertically(cmbPROJCS);

    rdbGEOGCS := TRadioButton.Create( oMainForm ) ;
    rdbGEOGCS.Parent := oMainForm ;
    rdbGEOGCS.Position.Y := 51 ;
    rdbGEOGCS.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbGEOGCS, 8, 376 ) ;
    rdbGEOGCS.Size.PlatformDefault := False ;
    rdbGEOGCS.TabOrder := 3 ;
    rdbGEOGCS.Text := _rsrcna( GIS_RS_CS_GEOGCS  ) ;
    rdbGEOGCS.GroupName := 'TGIS_ControlCSSystem' ;
    rdbGEOGCS.OnChange := rdbGEOGCSChange ;
    rdbGEOGCS.FixSize ;

    cmbGEOGCS := TComboEdit.Create( oMainForm ) ;
    cmbGEOGCS.Parent := oMainForm ;
    cmbGEOGCS.Touch.InteractiveGestures := [TInteractiveGesture.LongTap, TInteractiveGesture.DoubleTap] ;
    cmbGEOGCS.TabOrder := 0 ;
    cmbGEOGCS.ItemHeight := 19 ;
    cmbGEOGCS.ItemIndex := -1 ;
    cmbGEOGCS.ListBoxResource := 'combolistboxstyle' ;
    cmbGEOGCS.Text := 'cmbGEOGCS' ;
    cmbGEOGCS.Position.Y := 68 ;
    cmbGEOGCS.Size.Height := 22 ;
    PlaceControl( BiDiMode, nil, cmbGEOGCS, 30, 354 ) ;
    cmbGEOGCS.Size.PlatformDefault := False ;
    cmbGEOGCS.TabOrder := 4 ;

    btnGEOGCS := TCornerButton.Create( oMainForm ) ;
    btnGEOGCS.Parent := oMainForm ;
    if BiDiMode = TBiDiMode.bdRightToLeft  then
      btnGEOGCS.Corners := [TCorner.TopLeft, TCorner.BottomLeft]
    else
      btnGEOGCS.Corners := [TCorner.TopRight, TCorner.BottomRight] ;
    btnGEOGCS.Cursor := crArrow ;
    btnGEOGCS.Sides := [TSide.Top, TSide.Left, TSide.Bottom, TSide.Right] ;
    btnGEOGCS.Size.Height := 22 ;
    PlaceControl( BiDiMode, cmbGEOGCS, btnGEOGCS, 6, 30 ) ;
    btnGEOGCS.Size.PlatformDefault := False ;
    btnGEOGCS.TabOrder := 5 ;
    if BiDiMode = TBiDiMode.bdRightToLeft  then
      btnGEOGCS.Text := '<'
    else
      btnGEOGCS.Text := '>' ;
    btnGEOGCS.XRadius := 10 ;
    btnGEOGCS.YRadius := 10 ;
    btnGEOGCS.OnClick := btnGEOGCSClick ;
    btnGEOGCS.AlignVertically(cmbGEOGCS);

    rdbUNKNOWN := TRadioButton.Create( oMainForm ) ;
    rdbUNKNOWN.Parent := oMainForm ;
    rdbUNKNOWN.Position.Y := 94 ;
    rdbUNKNOWN.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbUNKNOWN, 8, 376 ) ;
    rdbUNKNOWN.Size.PlatformDefault := False ;
    rdbUNKNOWN.TabOrder := 6 ;
    rdbUNKNOWN.Text := _rsrcna( GIS_RS_CS_UNKNOWN ) ;
    rdbUNKNOWN.GroupName := 'TGIS_ControlCSSystem' ;
    rdbUNKNOWN.OnChange := rdbUNKNOWNChange ;
    rdbUNKNOWN.FixSize ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
      btnHelp.TabOrder := 7 ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;
      btnCancel.TabOrder := 9 ;

      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
      btnOK.TabOrder := 8 ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlCSSystem.Execute(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _onhelp : TGIS_HelpEvent          ;
    const _proc   : TProc<TModalResult>
  )  ;
  begin
    doExecute( _cs, _onhelp, _proc, True );
  end;

  procedure TGIS_ControlCSSystem.Execute(
    const _cs   : TGIS_CSCoordinateSystem ;
    const _proc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    doExecute( _cs, hlp, _proc, True );
  end;

{$IFDEF GIS_PDK}
  function TGIS_ControlCSSystem.Execute(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _onhelp : TGIS_HelpEvent
  ) : TModalResult ;
  var
    res : TModalResult ;
  begin
    doExecute(
      _cs,
      _onhelp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result = mrOk then begin
          res := _modal_result ;
        end ;
      end,
      False
    ) ;
    Result := res ;
  end;

  function TGIS_ControlCSSystem.Execute(
    const _cs   : TGIS_CSCoordinateSystem
  ) : TModalResult ;
  var
    hlp : TGIS_HelpEvent ;
    res : TModalResult ;
  begin
    hlp := nil ;
    doExecute(
      _cs,
      hlp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result = mrOk then begin
          res := _modal_result ;
        end ;
      end,
      False
    ) ;
    Result := res ;
  end;
{$ENDIF}

  procedure TGIS_ControlCSSystem.doModalResult(
    _modal_result : TModalResult
  ) ;
  begin
    if _modal_result = mrOk then begin
      if rdbUNKNOWN.IsChecked then begin
        FCS := TGIS_CSFactory.ByEPSG( 0 ) ;
      end
      else if rdbGEOGCS.IsChecked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpGEOGCS.SelectedObject.EPSG ) ;
      end
      else if rdbPROJCS.IsChecked then begin
        FCS := TGIS_CSFactory.ByEPSG( hlpPROJCS.SelectedObject.EPSG ) ;
      end;
    end ;

    if Assigned( pModalProc ) then
      pModalProc( _modal_result ) ;
  end ;

  procedure TGIS_ControlCSSystem.doExecute(
    const _cs     : TGIS_CSCoordinateSystem;
    const _onhelp : TGIS_HelpEvent         ;
    const _proc   : TProc<TModalResult>    ;
    const _free   : Boolean
  ) ;
  var
    gcs : TGIS_CSGeographicCoordinateSystem ;
    pcs : TGIS_CSProjectedCoordinateSystem ;
    r   : TModalResult ;
  begin
    pModalProc := _proc ;
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    FCS := _cs ;

    hlpGEOGCS.ByEpsg ( FCS.EPSG ) ;

    if FCS is TGIS_CSGeographicCoordinateSystem then begin
      rdbGEOGCS.IsChecked := True ;
      rdbGEOGCSChange( oMainForm ) ;
      btnGEOGCS.SetFocus ;
      cmbGEOGCS.SetFocus ;

      gcs := FCS as TGIS_CSGeographicCoordinateSystem ;
      hlpGEOGCS.ByEpsg( gcs.EPSG ) ;
    end
    else if FCS is TGIS_CSProjectedCoordinateSystem then begin
      rdbPROJCS.IsChecked := True ;
      rdbPROJCSChange( oMainForm ) ;
      btnPROJCS.SetFocus ;
      cmbPROJCS.SetFocus ;

      pcs := FCS as TGIS_CSProjectedCoordinateSystem ;
      hlpGEOGCS.ByEpsg( pcs.Geocs.EPSG ) ;
      hlpPROJCS.ByEpsg( pcs.EPSG ) ;
    end
    else begin
      rdbUNKNOWN.IsChecked := True  ;
      rdbUNKNOWNChange( oMainForm ) ;
    end;

    ShowModalEx( doModalResult, _free ) ;
  end;


{==================================== END =====================================}
end.

