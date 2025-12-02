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
  Coordinate System. Dialog box to view predefined projection.
}

unit FMX.GisControlCsSystemSetup ;
{$HPPEMIT '#pragma link "FMX.GisControlCsSystemSetup"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.SysUtils,
  System.UITypes,

  FMX.Controls,
  FMX.Dialogs,
  FMX.Edit,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Forms,
  FMX.Memo,
  FMX.Layouts,
  FMX.TabControl,
  FMX.Types,
  FMX.ComboEdit,
  FMX.Controls.Presentation,

  GisRtl,
  GisFunctions,
  GisCsSystems,
  GisCsProjections,
  GisCsFactory,
  GisTypes,

  FMX.GisModalForm,
  FMX.GisControlCsHelper;

type

  /// <summary>
  ///   Dialog box to alter existing Coordinate System.
  /// </summary>
  TGIS_ControlCSSystemSetup = class(TGIS_ModalForm)
    // form
      pagMain: TTabControl;
      tabGeocs: TTabItem;
      tabProjcs: TTabItem;
      tabWkt: TTabItem;
      edtProjcsName: TEdit;
      lblGeocsDatum: TLabel;
      lblGeocsPrimem: TLabel;
      lblGeocsUnit: TLabel;
      cmbGeocsUnit: TComboEdit;
      cmbGeocsDatum: TComboEdit;
      cmbGeocsPrimem: TComboEdit;
      edtGeocsName: TEdit;
      lblProjcsUnit: TLabel;
      cmbProjcsUnit: TComboEdit;
      lblProjcsProj: TLabel;
      cmbProjcsProj: TComboEdit;
      ScrollBox1: TScrollBox;
      lblProjcsCentralMeridian: TLabel;
      lblProjcsLatitudeOfOrigin: TLabel;
      lblProjcsFalseEasting: TLabel;
      lblProjcsFalseNorthing: TLabel;
      lblProjcsStandardParallel_1: TLabel;
      lblProjcsStandardParallel_2: TLabel;
      lblProjcsPseudoStandardParallel_1: TLabel;
      lblProjcsZone: TLabel;
      lblProjcsScaleFactor: TLabel;
      lblProjcsLongitudeOfCenter: TLabel;
      lblProjcsLatitudeOfCenter: TLabel;
      lblProjcsAzimuth: TLabel;
      edtProjcsLatitudeOfOrigin: TEdit;
      edtProjcsFalseNorthing: TEdit;
      edtProjcsAzimuth: TEdit;
      edtProjcsZone: TEdit;
      edtProjcsStandardParallel_2: TEdit;
      edtProjcsScaleFactor: TEdit;
      edtProjcsLatitudeOfCenter: TEdit;
      edtProjcsCentralMeridian: TEdit;
      edtProjcsFalseEasting: TEdit;
      edtProjcsStandardParallel_1: TEdit;
      edtProjcsPseudoStandardParallel_1: TEdit;
      edtProjcsLongitudeOfCenter: TEdit;
      lblProjcsLongitudeOfPoint_1: TLabel;
      lblProjcsLatitudeOfPoint_1: TLabel;
      edtProjcsLongitudeOfPoint_1: TEdit;
      edtProjcsLatitudeOfPoint_1: TEdit;
      lblProjcsLongitudeOfPoint_2: TLabel;
      lblProjcsLatitudeOfPoint_2: TLabel;
      edtProjcsLongitudeOfPoint_2: TEdit;
      edtProjcsLatitudeOfPoint_2: TEdit;
      lblProjcsYScale: TLabel;
      lblProjcsXScale: TLabel;
      edtProjcsYScale: TEdit;
      edtProjcsXScale: TEdit;
      lblProjcsXYPlaneRotation: TLabel;
      lblProjcsCentralParallel: TLabel;
      edtProjcsXYPlaneRotation: TEdit;
      edtProjcsCentralParallel: TEdit;
      lblGeocs: TLabel;
      cmbGeocs: TComboEdit;
      memWkt: TMemo;
      btnOpenWkt: TButton;
      btnSaveWkt: TButton;
      dlgOpenWkt: TOpenDialog;
      dlgSaveWkt: TSaveDialog;
      chkPretty: TCheckBox;
      btnGeocs: TCornerButton;

      procedure frmCreate          (     _sender : TObject );
      procedure frmDestroy         (     _sender : TObject );
      procedure frmShow            (     _sender : TObject );
      procedure cmbProjcsProjChange(     _sender : TObject );
      procedure btnOKClick         (     _sender : TObject ); override;
      procedure doProjcsVerify     (     _sender : TObject );
      procedure btnOpenWktClick    (     _sender : TObject );
      procedure btnSaveWktClick    (     _sender : TObject );
      procedure tabWktShow         (     _sender : TObject );
      procedure memWktChange       (     _sender : TObject );
      procedure btnGeocsClick      (     _sender : TObject );
      procedure doGeocsChange      (     _sender : TObject );
      procedure doProjcsChange     (     _sender : TObject );
      procedure chkPrettyChange    (     _sender : TObject );
    private
      FCS : TGIS_CSCoordinateSystem ;

      hlpGeocs        : TGIS_CSAbstractListHelper ;

      hlpGeocsDatum   : TGIS_CSAbstractListHelper ;
      hlpGeocsPrimem  : TGIS_CSAbstractListHelper ;
      hlpGeocsUnit    : TGIS_CSAbstractListHelper ;

      hlpProjcsUnit   : TGIS_CSAbstractListHelper ;
      hlpProjcsProj   : TGIS_CSAbstractListHelper ;

    private
      procedure setupCs    ;

      /// <summary>
      ///   Read default parameters from the projection.
      /// </summary>
      /// <param name="_proj">
      ///   projection to be read
      /// </param>
      /// <param name="_gcs">
      ///   system to obtain prime meridian
      /// </param>
      procedure readProjParams( const _proj : TGIS_CSProjAbstract ;
                                const _gcs  : TGIS_CSGeographicCoordinateSystem
                              ) ;

    protected

      /// <inheritdoc/>
      procedure initForm     ; override;

      /// <inheritdoc/>
      procedure initControls ; override;

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
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute  ( const _cs     : TGIS_CSCoordinateSystem ;
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
      procedure Execute  ( const _cs     : TGIS_CSCoordinateSystem ;
                           const _proc   : TProc<TModalResult>
                         ) ; overload;
    public
      /// <summary>
      /// Coordinate system.
      /// </summary>
      property CS : TGIS_CSCoordinateSystem read FCS ;
  end;

//##############################################################################
implementation

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
    FMX.Platform.Win,
  {$ENDIF}

  Math,
  GisCsBase,
  GisResource,
  {$IFDEF LEVEL_XE8_FMX}
    FMX.Scrollbox,
  {$ENDIF}
  FMX.GisControlHelper ;

//==============================================================================
// Private functions
//==============================================================================

  { Prepare dialog based on CS.
  }
  procedure TGIS_ControlCSSystemSetup.setupCs ;
  var
    gcs : TGIS_CSGeographicCoordinateSystem ;
    pcs : TGIS_CSProjectedCoordinateSystem ;
  begin
    if CS is TGIS_CSGeographicCoordinateSystem then begin
      tabGeocs.Visible  := True  ;
      tabProjcs.Visible := False ;

      gcs := CS as TGIS_CSGeographicCoordinateSystem ;
      hlpGeocsDatum.ByEpsg( gcs.Datum.EPSG ) ;
      hlpGeocsPrimem.ByEpsg( gcs.PrimeMeridian.EPSG ) ;
      hlpGeocsUnit.ByEpsg( gcs.Units.EPSG ) ;

      edtGeocsName.Text := gcs.WKT ;
      edtGeocsName.Enabled := False ;
    end
    else if CS is TGIS_CSProjectedCoordinateSystem then begin
      tabGeocs.Visible  := False ;
      tabProjcs.Visible := True  ;

      pcs := CS as TGIS_CSProjectedCoordinateSystem ;

      gcs := pcs.Geocs ;
      hlpGeocs.ByEPSG( gcs.EPSG );
      hlpProjcsUnit.ByEpsg( pcs.Units.EPSG ) ;
      hlpProjcsProj.ByEpsg( pcs.Projection.EPSG ) ;

      readProjParams(
        pcs.Projection,
        TGIS_CSGeographicCoordinateSystem( hlpGeocs.SelectedObject )
      ) ;

      edtProjcsName.Text := pcs.WKT ;
      edtProjcsName.Enabled := False ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.readProjParams(
    const _proj : TGIS_CSProjAbstract ;
    const _gcs  : TGIS_CSGeographicCoordinateSystem
  ) ;
  var
    primem : Double ;
  begin
    primem := _gcs.PrimeMeridian.Longitude ;

    if TGIS_CSProjParameter.CentralMeridian in _proj.ParametersSet then
      edtProjcsCentralMeridian.Text :=
        GisLongitudeToStr( _proj.CentralMeridian - primem ) ;

    if TGIS_CSProjParameter.LatitudeOfOrigin in _proj.ParametersSet then
      edtProjcsLatitudeOfOrigin.Text :=
        GisLatitudeToStr( _proj.LatitudeOfOrigin ) ;

    if TGIS_CSProjParameter.FalseEasting in _proj.ParametersSet then
      edtProjcsFalseEasting.Text :=
        DotFloatToStr( _proj.FalseEasting ) ;

    if TGIS_CSProjParameter.FalseNorthing in _proj.ParametersSet then
      edtProjcsFalseNorthing.Text :=
        DotFloatToStr( _proj.FalseNorthing ) ;

    if TGIS_CSProjParameter.StandardParallel_1 in _proj.ParametersSet then
      edtProjcsStandardParallel_1.Text :=
        GisLatitudeToStr( _proj.StandardParallel_1 ) ;

    if TGIS_CSProjParameter.StandardParallel_2 in _proj.ParametersSet then
      edtProjcsStandardParallel_2.Text :=
        GisLatitudeToStr( _proj.StandardParallel_2 ) ;

    if TGIS_CSProjParameter.PseudoStandardParallel_1 in _proj.ParametersSet then
      edtProjcsPseudoStandardParallel_1.Text :=
        GisLatitudeToStr( _proj.PseudoStandardParallel_1 ) ;

    if TGIS_CSProjParameter.Zone in _proj.ParametersSet then
      edtProjcsZone.Text :=
        IntToStr( _proj.Zone ) ;

    if TGIS_CSProjParameter.ScaleFactor in _proj.ParametersSet then
      edtProjcsScaleFactor.Text :=
        DotFloatToStr( _proj.Scalefactor ) ;

    if TGIS_CSProjParameter.LongitudeOfCenter in _proj.ParametersSet then
      edtProjcsLongitudeOfCenter.Text :=
        GisLongitudeToStr( _proj.LongitudeOfCenter - primem ) ;

    if TGIS_CSProjParameter.LatitudeOfCenter in _proj.ParametersSet then
      edtProjcsLatitudeOfCenter.Text :=
        GisLatitudeToStr( _proj.LatitudeOfCenter ) ;

    if TGIS_CSProjParameter.Azimuth in _proj.ParametersSet then
      edtProjcsAzimuth.Text :=
        GisAngleToStr( _proj.Azimuth ) ;

    if TGIS_CSProjParameter.LongitudeOfPoint_1 in _proj.ParametersSet then
      edtProjcsLongitudeOfPoint_1.Text :=
        GisLongitudeToStr( _proj.LongitudeOfPoint_1 - primem ) ;

    if TGIS_CSProjParameter.LatitudeOfPoint_1 in _proj.ParametersSet then
      edtProjcsLatitudeOfPoint_1.Text :=
        GisLatitudeToStr( _proj.LatitudeOfPoint_1 ) ;

    if TGIS_CSProjParameter.LongitudeOfPoint_2 in _proj.ParametersSet then
      edtProjcsLongitudeOfPoint_2.Text :=
        GisLongitudeToStr( _proj.LongitudeOfPoint_2 - primem ) ;

    if TGIS_CSProjParameter.LatitudeOfPoint_2 in _proj.ParametersSet then
      edtProjcsLatitudeOfPoint_2.Text :=
        GisLatitudeToStr( _proj.LatitudeOfPoint_2 ) ;

    if TGIS_CSProjParameter.XScale in _proj.ParametersSet then
      edtProjcsXScale.Text :=
        DotFloatToStr( _proj.XScale ) ;

    if TGIS_CSProjParameter.YScale in _proj.ParametersSet then
      edtProjcsYScale.Text :=
        DotFloatToStr( _proj.YScale ) ;

    if TGIS_CSProjParameter.XYPlaneRotation in _proj.ParametersSet then
      edtProjcsXYPlaneRotation.Text :=
        GisAngleToStr( _proj.XYPlaneRotation ) ;
  end;

//==============================================================================
// Form events
//==============================================================================

  procedure TGIS_ControlCSSystemSetup.frmCreate(
    _sender: TObject
  );
  begin
    {$IFDEF MSWINDOWS}
      // Avoid cursor flickering on XE4; Needs modified IFMXCursorService
      // elsewhere to properly set mouse cursor
      { TODO -creevaluate : XE4 Cursor flickering problem }
      if TFmxObject( oMainForm ) is TCustomForm then begin
        SetClassLong( FmxHandleToHWND( TCustomForm(oMainForm).Handle), GCL_HCURSOR, 0 ) ;
        oMainForm.Cursor := crArrow ;
      end ;
    {$ENDIF}

    hlpGeocs           :=  TGIS_CSAbstractListHelper.Create(
                            cmbGeocs,
                            CSGeographicCoordinateSystemList
                          ) ;
    hlpGeocsDatum      := TGIS_CSAbstractListHelper.Create(
                            cmbGeocsDatum,
                            CSDatumList
                          ) ;

    hlpGeocsPrimem     := TGIS_CSAbstractListHelper.Create(
                            cmbGeocsPrimem,
                            CSPrimeMeridianList
                          ) ;

    hlpGeocsUnit       := TGIS_CSAbstractListHelper.Create(
                            cmbGeocsUnit,
                            CSUnitsList
                          ) ;
    hlpGeocsUnit.ShowUnitsLinear := False ;

    hlpProjcsUnit      := TGIS_CSAbstractListHelper.Create(
                            cmbProjcsUnit,
                            CSUnitsList
                          ) ;
    hlpProjcsUnit.ShowUnitsAngular := False ;

    hlpProjcsProj      := TGIS_CSAbstractListHelper.Create(
                            cmbProjcsProj,
                            CSProjList
                          ) ;
    hlpProjcsProj.ShowProjRevertible := True  ;
    hlpProjcsProj.ShowProjStandard   := True  ;

    // make page active for a while to avoid OSX rendering problem of the
    // TAB caption
    pagMain.ActiveTab := tabWKT ;
  end;

  procedure TGIS_ControlCSSystemSetup.frmDestroy(
    _sender : TObject
  );
  begin
    FreeObject( hlpGeocs       ) ;
    FreeObject( hlpGeocsDatum  ) ;
    FreeObject( hlpGeocsPrimem ) ;
    FreeObject( hlpGeocsUnit   ) ;

    FreeObject( hlpProjcsUnit  ) ;
    FreeObject( hlpProjcsProj  ) ;
  end;

  procedure TGIS_ControlCSSystemSetup.frmShow(
    _sender : TObject
  ) ;
  begin
    //
  end ;

  procedure TGIS_ControlCSSystemSetup.tabWktShow(
    _sender : TObject
  );
  var
    proj_param : TGIS_CSProjParameters             ;

    procedure init_proj_params ;
    var
      prj    : TGIS_CSProjAbstract ;
    begin
      prj := TGIS_CSProjAbstract( hlpProjcsProj.SelectedObject ) ;

      proj_param.CentralMeridian          := NaN  ;
      proj_param.LatitudeOfOrigin         := NaN  ;
      proj_param.FalseEasting             := NaN  ;
      proj_param.FalseNorthing            := NaN  ;
      proj_param.StandardParallel_1       := NaN  ;
      proj_param.StandardParallel_2       := NaN  ;
      proj_param.PseudoStandardParallel_1 := NaN  ;
      proj_param.Zone                     := 0    ;
      proj_param.ScaleFactor              := NaN  ;
      proj_param.LongitudeOfCenter        := NaN  ;
      proj_param.LatitudeOfCenter         := NaN  ;
      proj_param.Azimuth                  := NaN  ;
      proj_param.LongitudeOfPoint_1       := NaN  ;
      proj_param.LatitudeOfPoint_1        := NaN  ;
      proj_param.LongitudeOfPoint_2       := NaN  ;
      proj_param.LatitudeOfPoint_2        := NaN  ;
      proj_param.XScale                   := NaN  ;
      proj_param.YScale                   := NaN  ;
      proj_param.XYPlaneRotation          := NaN  ;

      if TGIS_CSProjParameter.CentralMeridian in prj.ParametersSet then
        proj_param.CentralMeridian :=
          GisStrToLongitude( edtProjcsCentralMeridian.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfOrigin in prj.ParametersSet then
        proj_param.LatitudeOfOrigin :=
          GisStrToLatitude( edtProjcsLatitudeOfOrigin.Text ) ;

      if TGIS_CSProjParameter.FalseEasting in prj.ParametersSet then
        proj_param.FalseEasting :=
          DotStrToFloat( edtProjcsFalseEasting.Text ) ;

      if TGIS_CSProjParameter.FalseNorthing in prj.ParametersSet then
        proj_param.FalseNorthing :=
          DotStrToFloat( edtProjcsFalseNorthing.Text ) ;

      if TGIS_CSProjParameter.StandardParallel_1 in prj.ParametersSet then
        proj_param.StandardParallel_1 :=
          GisStrToLatitude( edtProjcsStandardParallel_1.Text ) ;

      if TGIS_CSProjParameter.StandardParallel_2 in prj.ParametersSet then
        proj_param.StandardParallel_2 :=
          GisStrToLatitude( edtProjcsStandardParallel_2.Text ) ;

      if TGIS_CSProjParameter.PseudoStandardParallel_1 in prj.ParametersSet then
        proj_param.PseudoStandardParallel_1 :=
          GisStrToLatitude( edtProjcsPseudoStandardParallel_1.Text ) ;

      if TGIS_CSProjParameter.Zone in prj.ParametersSet then
        proj_param.Zone := StrToInt( edtProjcsZone.Text ) ;

      if TGIS_CSProjParameter.ScaleFactor in prj.ParametersSet then
        proj_param.Scalefactor :=
          DotStrToFloat( edtProjcsScaleFactor.Text ) ;

      if TGIS_CSProjParameter.LongitudeOfCenter in prj.ParametersSet then
        proj_param.LongitudeOfCenter :=
          GisStrToLongitude( edtProjcsLongitudeOfCenter.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfCenter in prj.ParametersSet then
        proj_param.LatitudeOfCenter :=
          GisStrToLatitude( edtProjcsLatitudeOfCenter.Text ) ;

      if TGIS_CSProjParameter.Azimuth in prj.ParametersSet then
        proj_param.Azimuth :=
          GisStrToAngle( edtProjcsAzimuth.Text ) ;

      if TGIS_CSProjParameter.LongitudeOfPoint_1 in prj.ParametersSet then
        proj_param.LongitudeOfPoint_1 :=
          GisStrToLongitude( edtProjcsLongitudeOfPoint_1.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfPoint_1 in prj.ParametersSet then
        proj_param.LatitudeOfPoint_1 :=
          GisStrToLatitude( edtProjcsLatitudeOfPoint_1.Text ) ;

      if TGIS_CSProjParameter.LongitudeOfPoint_2 in prj.ParametersSet then
        proj_param.LongitudeOfPoint_2 :=
          GisStrToLongitude( edtProjcsLongitudeOfPoint_2.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfPoint_2 in prj.ParametersSet then
        proj_param.LatitudeOfPoint_2 :=
          GisStrToLatitude( edtProjcsLatitudeOfPoint_2.Text ) ;

      if TGIS_CSProjParameter.XScale in prj.ParametersSet then
        proj_param.XScale :=
          DotStrToFloat( edtProjcsXScale.Text ) ;

      if TGIS_CSProjParameter.YScale in prj.ParametersSet then
        proj_param.YScale :=
          DotStrToFloat( edtProjcsYScale.Text ) ;

      if TGIS_CSProjParameter.XYPlaneRotation in prj.ParametersSet then
        proj_param.XYPlaneRotation :=
          GisStrToAngle( edtProjcsXYPlaneRotation.Text ) ;
    end;
  begin
    if FCS is TGIS_CSGeographicCoordinateSystem then begin
      FCS := CSGeographicCoordinateSystemList.Prepare(
               -1, edtGeocsName.Text,
               hlpGeocsDatum.SelectedObject.EPSG,
               hlpGeocsPrimem.SelectedObject.EPSG,
               hlpGeocsUnit.SelectedObject.EPSG
             ) ;
    end
    else if FCS is TGIS_CSProjectedCoordinateSystem then begin
      init_proj_params ;

      FCS := CSProjectedCoordinateSystemList.Prepare(
               -1, edtProjcsName.Text,
               hlpGeocs.SelectedObject.EPSG,
               hlpProjcsUnit.SelectedObject.EPSG,
               hlpProjcsProj.SelectedObject.EPSG,
               proj_param
             ) ;
    end ;

    if chkPretty.IsChecked then begin
      memWkt.WordWrap := False ;
      memWkt.Text := CS.PrettyWKT ;
    end
    else begin
      memWkt.WordWrap := True ;
      memWkt.Text := CS.FullWKT ;
    end ;
  end;

  procedure TGIS_ControlCSSystemSetup.btnOpenWktClick(
    _sender : TObject
  ) ;
  begin
    if dlgOpenWkt.Execute() then begin
      memWkt.Lines.LoadFromFile( dlgOpenWkt.FileName );
      FCS := TGIS_CSFactory.ByWKT( memWkt.Text ) ;
      setupCs ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.btnGeocsClick(
    _sender : TObject
  );
  var
    dlg : TGIS_ControlCSSystemSetup ;
    gcs : TGIS_CSCoordinateSystem ;
  begin
    gcs := TGIS_CSCoordinateSystem( hlpGeocs.SelectedObject ) ;
    dlg := TGIS_ControlCSSystemSetup.Create( oMainForm );
    ActiveControl := btnGeocs ;

    dlg.Execute(
      gcs,
      pOnHelp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result = mrOk then begin
          gcs := dlg.CS ;
          if gcs.EPSG > 0 then
          hlpGeocs.ByEPSG( gcs.EPSG ) ;

          ActiveControl := btnOK ;
        end
        else
          ActiveControl := cmbGeocs ;
      end
    ) ;
  end;

  procedure TGIS_ControlCSSystemSetup.btnSaveWktClick(
    _sender: TObject
  ) ;
  begin
    if dlgSaveWkt.Execute() then begin
      memWkt.Lines.SaveToFile( dlgSaveWkt.FileName );
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.btnOKClick(
    _sender : TObject
  ) ;
  var
    proj_param : TGIS_CSProjParameters ;

    procedure init_proj_params ;
    var
      prj    : TGIS_CSProjAbstract ;
    begin
      prj := TGIS_CSProjAbstract( hlpProjcsProj.SelectedObject ) ;

      proj_param.CentralMeridian          := NaN  ;
      proj_param.LatitudeOfOrigin         := NaN  ;
      proj_param.FalseEasting             := NaN  ;
      proj_param.FalseNorthing            := NaN  ;
      proj_param.StandardParallel_1       := NaN  ;
      proj_param.StandardParallel_2       := NaN  ;
      proj_param.PseudoStandardParallel_1 := NaN  ;
      proj_param.Zone                     := 0    ;
      proj_param.ScaleFactor              := NaN  ;
      proj_param.LongitudeOfCenter        := NaN  ;
      proj_param.LatitudeOfCenter         := NaN  ;
      proj_param.Azimuth                  := NaN  ;
      proj_param.LongitudeOfPoint_1       := NaN  ;
      proj_param.LatitudeOfPoint_1        := NaN  ;
      proj_param.LongitudeOfPoint_2       := NaN  ;
      proj_param.LatitudeOfPoint_2        := NaN  ;
      proj_param.XScale                   := NaN  ;
      proj_param.YScale                   := NaN  ;
      proj_param.XYPlaneRotation          := NaN  ;

      if TGIS_CSProjParameter.CentralMeridian in prj.ParametersSet then
        proj_param.CentralMeridian :=
          GisStrToLongitude( edtProjcsCentralMeridian.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfOrigin in prj.ParametersSet then
        proj_param.LatitudeOfOrigin :=
          GisStrToLatitude( edtProjcsLatitudeOfOrigin.Text ) ;

      if TGIS_CSProjParameter.FalseEasting in prj.ParametersSet then
        proj_param.FalseEasting :=
          DotStrToFloat( edtProjcsFalseEasting.Text ) ;

      if TGIS_CSProjParameter.FalseNorthing in prj.ParametersSet then
        proj_param.FalseNorthing :=
          DotStrToFloat( edtProjcsFalseNorthing.Text ) ;

      if TGIS_CSProjParameter.StandardParallel_1 in prj.ParametersSet then
        proj_param.StandardParallel_1 :=
          GisStrToLatitude( edtProjcsStandardParallel_1.Text ) ;

      if TGIS_CSProjParameter.StandardParallel_2 in prj.ParametersSet then
        proj_param.StandardParallel_2 :=
          GisStrToLatitude( edtProjcsStandardParallel_2.Text ) ;

      if TGIS_CSProjParameter.PseudoStandardParallel_1 in prj.ParametersSet then
        proj_param.PseudoStandardParallel_1 :=
          GisStrToLatitude( edtProjcsPseudoStandardParallel_1.Text ) ;

      if TGIS_CSProjParameter.Zone in prj.ParametersSet then
        proj_param.Zone := StrToInt( edtProjcsZone.Text ) ;

      if TGIS_CSProjParameter.ScaleFactor in prj.ParametersSet then
        proj_param.Scalefactor :=
          DotStrToFloat( edtProjcsScaleFactor.Text ) ;

      if TGIS_CSProjParameter.LongitudeOfCenter in prj.ParametersSet then
        proj_param.LongitudeOfCenter :=
          GisStrToLongitude( edtProjcsLongitudeOfCenter.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfCenter in prj.ParametersSet then
        proj_param.LatitudeOfCenter :=
          GisStrToLatitude( edtProjcsLatitudeOfCenter.Text ) ;

      if TGIS_CSProjParameter.Azimuth in prj.ParametersSet then
        proj_param.Azimuth :=
          GisStrToAngle( edtProjcsAzimuth.Text ) ;

      if TGIS_CSProjParameter.LongitudeOfPoint_1 in prj.ParametersSet then
        proj_param.LongitudeOfPoint_1 :=
          GisStrToLongitude( edtProjcsLongitudeOfPoint_1.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfPoint_1 in prj.ParametersSet then
        proj_param.LatitudeOfPoint_1 :=
          GisStrToLatitude( edtProjcsLatitudeOfPoint_1.Text ) ;

      if TGIS_CSProjParameter.LongitudeOfPoint_2 in prj.ParametersSet then
        proj_param.LongitudeOfPoint_2 :=
          GisStrToLongitude( edtProjcsLongitudeOfPoint_2.Text ) ;

      if TGIS_CSProjParameter.LatitudeOfPoint_2 in prj.ParametersSet then
        proj_param.LatitudeOfPoint_2 :=
          GisStrToLatitude( edtProjcsLatitudeOfPoint_2.Text ) ;

      if TGIS_CSProjParameter.XScale in prj.ParametersSet then
        proj_param.XScale :=
          DotStrToFloat( edtProjcsXScale.Text ) ;

      if TGIS_CSProjParameter.YScale in prj.ParametersSet then
        proj_param.YScale :=
          DotStrToFloat( edtProjcsYScale.Text ) ;

      if TGIS_CSProjParameter.XYPlaneRotation in prj.ParametersSet then
        proj_param.XYPlaneRotation :=
          GisStrToAngle( edtProjcsXYPlaneRotation.Text ) ;
    end;

  begin
    if FCS is TGIS_CSGeographicCoordinateSystem then begin
      FCS := CSGeographicCoordinateSystemList.Prepare(
               -1, edtGeocsName.Text,
               hlpGeocsDatum.SelectedObject.EPSG,
               hlpGeocsPrimem.SelectedObject.EPSG,
               hlpGeocsUnit.SelectedObject.EPSG
             ) ;
    end
    else if FCS is TGIS_CSProjectedCoordinateSystem then begin
      init_proj_params ;

      FCS := CSProjectedCoordinateSystemList.Prepare(
               -1, edtProjcsName.Text,
               hlpGeocs.SelectedObject.EPSG,
               hlpProjcsUnit.SelectedObject.EPSG,
               hlpProjcsProj.SelectedObject.EPSG,
               proj_param
             ) ;
    end ;

    ModalResult := mrOk ;
  end;

  procedure TGIS_ControlCSSystemSetup.cmbProjcsProjChange(
    _sender: TObject
  ) ;
  var
    obj   : TGIS_CSProjAbstract ;
    itop  : Double ;
    bline : Boolean ;

    hlbl : Double ;
    hlin : Double ;
  begin
    doProjcsChange( _sender ) ;

    obj := nil ;
    if not Assigned( hlpProjcsProj ) then begin
      if _sender is TGIS_CSAbstractListHelper then
        obj := TGIS_CSProjAbstract(
                 ( _sender as TGIS_CSAbstractListHelper ).SelectedObject
               ) ;
    end
    else
      obj := TGIS_CSProjAbstract( hlpProjcsProj.SelectedObject ) ;

    hlbl := lblProjcsCentralMeridian.Height + 2 ;
    hlin := edtProjcsCentralMeridian.Height + hlbl + 6 ;

    itop := 5 ;

    // line 1
    bline := False ;
    if TGIS_CSProjParameter.CentralMeridian in obj.ParametersSet then begin
      lblProjcsCentralMeridian.Visible := True  ;
      edtProjcsCentralMeridian.Visible := True  ;
      lblProjcsCentralMeridian.Position.Y := itop  ;
      edtProjcsCentralMeridian.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsCentralMeridian.Visible := False ;
      edtProjcsCentralMeridian.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfOrigin in obj.ParametersSet then begin
      lblProjcsLatitudeOfOrigin.Visible := True  ;
      edtProjcsLatitudeOfOrigin.Visible := True  ;
      lblProjcsLatitudeOfOrigin.Position.Y := itop  ;
      edtProjcsLatitudeOfOrigin.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLatitudeOfOrigin.Visible := False ;
      edtProjcsLatitudeOfOrigin.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 2
    bline := False ;
    if TGIS_CSProjParameter.FalseEasting in obj.ParametersSet then begin
      lblProjcsFalseEasting.Visible := True  ;
      edtProjcsFalseEasting.Visible := True  ;
      lblProjcsFalseEasting.Position.Y := itop  ;
      edtProjcsFalseEasting.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsFalseEasting.Visible := False ;
      edtProjcsFalseEasting.Visible := False ;
    end ;

    if TGIS_CSProjParameter.FalseNorthing in obj.ParametersSet then begin
      lblProjcsFalseNorthing.Visible := True  ;
      edtProjcsFalseNorthing.Visible := True  ;
      lblProjcsFalseNorthing.Position.Y := itop  ;
      edtProjcsFalseNorthing.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsFalseNorthing.Visible := False ;
      edtProjcsFalseNorthing.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 3
    bline := False ;
    if TGIS_CSProjParameter.StandardParallel_1 in obj.ParametersSet then begin
      lblProjcsStandardParallel_1.Visible := True  ;
      edtProjcsStandardParallel_1.Visible := True  ;
      lblProjcsStandardParallel_1.Position.Y := itop  ;
      edtProjcsStandardParallel_1.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsStandardParallel_1.Visible := False ;
      edtProjcsStandardParallel_1.Visible := False ;
    end ;

    if TGIS_CSProjParameter.StandardParallel_2 in obj.ParametersSet then begin
      lblProjcsStandardParallel_2.Visible := True  ;
      edtProjcsStandardParallel_2.Visible := True  ;
      lblProjcsStandardParallel_2.Position.Y := itop  ;
      edtProjcsStandardParallel_2.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsStandardParallel_2.Visible := False ;
      edtProjcsStandardParallel_2.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 4
    bline := False ;
    if TGIS_CSProjParameter.PseudoStandardParallel_1 in obj.ParametersSet then begin
      lblProjcsPseudoStandardParallel_1.Visible := True  ;
      edtProjcsPseudoStandardParallel_1.Visible := True  ;
      lblProjcsPseudoStandardParallel_1.Position.Y := itop  ;
      edtProjcsPseudoStandardParallel_1.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsPseudoStandardParallel_1.Visible := False ;
      edtProjcsPseudoStandardParallel_1.Visible := False ;
    end ;

    if TGIS_CSProjParameter.Zone in obj.ParametersSet then begin
      lblProjcsZone.Visible := True  ;
      edtProjcsZone.Visible := True  ;
      lblProjcsZone.Position.Y := itop  ;
      edtProjcsZone.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsZone.Visible := False ;
      edtProjcsZone.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 5
    bline := False ;
    if TGIS_CSProjParameter.ScaleFactor in obj.ParametersSet then begin
      lblProjcsScaleFactor.Visible := True  ;
      edtProjcsScaleFactor.Visible := True  ;
      lblProjcsScaleFactor.Position.Y := itop  ;
      edtProjcsScaleFactor.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsScaleFactor.Visible := False ;
      edtProjcsScaleFactor.Visible := False ;
    end ;

    if TGIS_CSProjParameter.Azimuth in obj.ParametersSet then begin
      lblProjcsAzimuth.Visible := True  ;
      edtProjcsAzimuth.Visible := True  ;
      lblProjcsAzimuth.Position.Y := itop  ;
      edtProjcsAzimuth.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsAzimuth.Visible := False ;
      edtProjcsAzimuth.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 6
    bline := False ;
    if TGIS_CSProjParameter.LongitudeOfCenter in obj.ParametersSet then begin
      lblProjcsLongitudeOfCenter.Visible := True  ;
      edtProjcsLongitudeOfCenter.Visible := True  ;
      lblProjcsLongitudeOfCenter.Position.Y := itop  ;
      edtProjcsLongitudeOfCenter.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLongitudeOfCenter.Visible := False ;
      edtProjcsLongitudeOfCenter.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfCenter in obj.ParametersSet then begin
      lblProjcsLatitudeOfCenter.Visible := True  ;
      edtProjcsLatitudeOfCenter.Visible := True  ;
      lblProjcsLatitudeOfCenter.Position.Y := itop  ;
      edtProjcsLatitudeOfCenter.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLatitudeOfCenter.Visible := False ;
      edtProjcsLatitudeOfCenter.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 7
    bline := False ;
    if TGIS_CSProjParameter.LongitudeOfPoint_1 in obj.ParametersSet then begin
      lblProjcsLongitudeOfPoint_1.Visible := True  ;
      edtProjcsLongitudeOfPoint_1.Visible := True  ;
      lblProjcsLongitudeOfPoint_1.Position.Y := itop  ;
      edtProjcsLongitudeOfPoint_1.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLongitudeOfPoint_1.Visible := False ;
      edtProjcsLongitudeOfPoint_1.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfPoint_1 in obj.ParametersSet then begin
      lblProjcsLatitudeOfPoint_1.Visible := True  ;
      edtProjcsLatitudeOfPoint_1.Visible := True  ;
      lblProjcsLatitudeOfPoint_1.Position.Y := itop  ;
      edtProjcsLatitudeOfPoint_1.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLatitudeOfPoint_1.Visible := False ;
      edtProjcsLatitudeOfPoint_1.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 8
    bline := False ;
    if TGIS_CSProjParameter.LongitudeOfPoint_2 in obj.ParametersSet then begin
      lblProjcsLongitudeOfPoint_2.Visible := True  ;
      edtProjcsLongitudeOfPoint_2.Visible := True  ;
      lblProjcsLongitudeOfPoint_2.Position.Y := itop  ;
      edtProjcsLongitudeOfPoint_2.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLongitudeOfPoint_2.Visible := False ;
      edtProjcsLongitudeOfPoint_2.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfPoint_2 in obj.ParametersSet then begin
      lblProjcsLatitudeOfPoint_2.Visible := True  ;
      edtProjcsLatitudeOfPoint_2.Visible := True  ;
      lblProjcsLatitudeOfPoint_2.Position.Y := itop  ;
      edtProjcsLatitudeOfPoint_2.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLatitudeOfPoint_2.Visible := False ;
      edtProjcsLatitudeOfPoint_2.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 9
    bline := False ;
    if TGIS_CSProjParameter.XScale in obj.ParametersSet then begin
      lblProjcsXScale.Visible := True  ;
      edtProjcsXScale.Visible := True  ;
      lblProjcsXScale.Position.Y := itop  ;
      edtProjcsXScale.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsXScale.Visible := False ;
      edtProjcsXScale.Visible := False ;
    end ;

    if TGIS_CSProjParameter.YScale in obj.ParametersSet then begin
      lblProjcsYScale.Visible := True  ;
      edtProjcsYScale.Visible := True  ;
      lblProjcsYScale.Position.Y := itop  ;
      edtProjcsYScale.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsYScale.Visible := False ;
      edtProjcsYScale.Visible := False ;
    end;

    if bline then begin
      itop := itop + hlin ;
    end ;

    // line 10
    bline := False ;
    if TGIS_CSProjParameter.XYPlaneRotation in obj.ParametersSet then begin
      lblProjcsXYPlaneRotation.Visible := True  ;
      edtProjcsXYPlaneRotation.Visible := True  ;
      lblProjcsXYPlaneRotation.Position.Y := itop  ;
      edtProjcsXYPlaneRotation.Position.Y := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsXYPlaneRotation.Visible := False ;
      edtProjcsXYPlaneRotation.Visible := False ;
    end ;

    if bline then begin
      itop := itop + hlin ;
    end ;

    lblProjcsCentralParallel.Visible    := False ;
    edtProjcsCentralParallel.Visible    := False ;

    ScrollBox1.ShowScrollBars := False ;
    ScrollBox1.ShowScrollBars := True ;

    readProjParams(
      obj,
      TGIS_CSGeographicCoordinateSystem( hlpGeocs.SelectedObject )
    ) ;
  end;

  procedure TGIS_ControlCSSystemSetup.chkPrettyChange(
    _sender : TObject
  );
  begin
    if Assigned( CS ) then begin
      memWktChange( _sender );
      if chkPretty.IsChecked then begin
        memWkt.WordWrap := False ;
        memWkt.Text := CS.PrettyWKT ;
        memWkt.ReadOnly := True ;
      end
      else begin
        memWkt.WordWrap := True ;
        memWkt.Text := CS.FullWKT ;
        memWkt.ReadOnly := False ;
      end ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.doProjcsVerify(
    _sender: TObject
  );
  var
    res : Boolean ;
    obj : TGIS_CSProjAbstract ;

    function verify_longitude( const _edt : TEdit ) : Boolean ;
    begin
      try
        GisStrToLongitude( _edt.Text ) ;
        _edt.StyleLookup := '' ;
        Result := False ;
      except
        _edt.StyleLookup := 'editstyle_error' ;
        Result := True ;
      end;
    end ;

    function verify_latitude( const _edt : TEdit ) : Boolean ;
    begin
      try
        GisStrToLatitude( _edt.Text ) ;
        _edt.StyleLookup := '' ;
        Result := False ;
      except
        _edt.StyleLookup := 'editstyle_error' ;
        Result := True  ;
      end;
    end ;

    function verify_angle( const _edt : TEdit ) : Boolean ;
    begin
      try
        GisStrToAngle( _edt.Text ) ;
        _edt.StyleLookup := '' ;
        Result := False ;
      except
        _edt.StyleLookup := 'editstyle_error' ;
        Result := True  ;
      end;
    end ;

    function verify_float( const _edt : TEdit ) : Boolean ;
    begin
      try
        DotStrToFloat( _edt.Text ) ;
        _edt.StyleLookup := '' ;
        Result := False ;
      except
        _edt.StyleLookup := 'editstyle_error' ;
        Result := True  ;
      end;
    end ;

    function verify_int( const _edt : TEdit ) : Boolean ;
    begin
      try
        StrToInt( _edt.Text ) ;
        _edt.StyleLookup := '' ;
        Result := False ;
      except
        _edt.StyleLookup := 'editstyle_error' ;
        Result := True  ;
      end;
    end ;

  begin
    res := False;

    obj := TGIS_CSProjAbstract( hlpProjcsProj.SelectedObject ) ;

    if TGIS_CSProjParameter.CentralMeridian in obj.ParametersSet then
      res := verify_longitude( edtProjcsCentralMeridian ) or res ;

    if TGIS_CSProjParameter.LatitudeOfOrigin in obj.ParametersSet then
      res := verify_latitude( edtProjcsLatitudeOfOrigin ) or res  ;

    if TGIS_CSProjParameter.FalseEasting in obj.ParametersSet then
      res := verify_float( edtProjcsFalseEasting ) or res ;

    if TGIS_CSProjParameter.FalseNorthing in obj.ParametersSet then
      res := verify_float( edtProjcsFalseNorthing ) or res ;

    if TGIS_CSProjParameter.StandardParallel_1 in obj.ParametersSet then
      res := verify_latitude( edtProjcsStandardParallel_1 ) or res  ;

    if TGIS_CSProjParameter.StandardParallel_2 in obj.ParametersSet then
      res := verify_latitude( edtProjcsStandardParallel_2 ) or res  ;

    if TGIS_CSProjParameter.PseudoStandardParallel_1 in obj.ParametersSet then
      res := verify_latitude( edtProjcsPseudoStandardParallel_1 ) or res  ;

    if TGIS_CSProjParameter.Zone in obj.ParametersSet then
      res := verify_int( edtProjcsZone ) or res ;

    if TGIS_CSProjParameter.ScaleFactor in obj.ParametersSet then
      res := verify_float( edtProjcsScaleFactor ) or res ;

    if TGIS_CSProjParameter.LongitudeOfCenter in obj.ParametersSet then
      res := verify_longitude( edtProjcsLongitudeOfCenter ) or res ;

    if TGIS_CSProjParameter.LatitudeOfCenter in obj.ParametersSet then
      res := verify_latitude( edtProjcsLatitudeOfCenter ) or res ;

    if TGIS_CSProjParameter.Azimuth in obj.ParametersSet then
      res := verify_angle( edtProjcsAzimuth ) or res ;

    if TGIS_CSProjParameter.LongitudeOfPoint_1 in obj.ParametersSet then
      res := verify_longitude( edtProjcsLongitudeOfPoint_1 ) or res ;

    if TGIS_CSProjParameter.LatitudeOfPoint_1 in obj.ParametersSet then
      res := verify_latitude( edtProjcsLatitudeOfPoint_1 ) or res ;

    if TGIS_CSProjParameter.LongitudeOfPoint_2 in obj.ParametersSet then
      res := verify_longitude( edtProjcsLongitudeOfPoint_2 ) or res ;

    if TGIS_CSProjParameter.LatitudeOfPoint_2 in obj.ParametersSet then
      res := verify_latitude( edtProjcsLatitudeOfPoint_2 ) or res ;

    if TGIS_CSProjParameter.XScale in obj.ParametersSet then
      res := verify_float( edtProjcsXScale ) or res ;

    if TGIS_CSProjParameter.YScale in obj.ParametersSet then
      res := verify_float( edtProjcsYScale ) or res ;

    if TGIS_CSProjParameter.XYPlaneRotation in obj.ParametersSet then
      res := verify_angle( edtProjcsXYPlaneRotation ) or res ;

    btnOK.Enabled  := not res ;
    tabWkt.Enabled := not res ;

    doProjcsChange( _sender );

  end;

  procedure TGIS_ControlCSSystemSetup.memWktChange(
    _sender: TObject
  ) ;
  var
    tmp : TGIS_CSCoordinateSystem ;
  begin
    tmp := TGIS_CSFactory.ByWKT( memWkt.Text ) ;
    if ( ( CS  is TGIS_CSGeographicCoordinateSystem ) and
         ( tmp is TGIS_CSGeographicCoordinateSystem )
       )
       or
       ( ( CS  is TGIS_CSProjectedCoordinateSystem  ) and
         ( tmp is TGIS_CSProjectedCoordinateSystem  )
       )
    then begin
      FCS := tmp ;
      setupCs ;
      memWkt.StyleLookup := '' ;
      chkPretty.Enabled := True ;
      btnOK.Enabled := True ;
      btnSaveWkt.Enabled := True ;
    end
    else begin
      memWkt.StyleLookup := 'memostyle_error' ;
      chkPretty.Enabled := False ;
      btnOK.Enabled := False ;
      btnSaveWkt.Enabled := False ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.doGeocsChange(
    _sender : TObject
  ) ;
  begin
     edtGeocsName.Text := 'Custom GCS' ;
     edtGeocsName.Enabled := True  ;
  end;

  procedure TGIS_ControlCSSystemSetup.doProjcsChange(
    _sender : TObject
  );
  begin
     edtProjcsName.Text := 'Custom PCS' ;
     edtProjcsName.Enabled := True  ;
  end;

//==============================================================================
// Various
//==============================================================================

  procedure TGIS_ControlCSSystemSetup.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_CSSETUP_DLG ) ;
    Self.ClientHeight := 402 ;
    Self.ClientWidth := 385 ;
    Self.Name := 'TGIS_ControlCSSystemSetup' ;
    Self.OnCreate := frmCreate ;
    Self.OnDestroy := frmDestroy ;
    Self.OnShow := frmShow ;
  end;

  procedure TGIS_ControlCSSystemSetup.initControls ;
  var
    anchors : TAnchors ;
  begin
    pagMain := TTabControl.Create( oMainForm ) ;
    pagMain.Parent := oMainForm ;
    pagMain.Position.X := 8 ;
    pagMain.Position.Y := 8 ;
    pagMain.Size.Width := 369 ;
    pagMain.Size.Height := 353 ;
    pagMain.Size.PlatformDefault := False ;
    pagMain.TabHeight := 20 ;
    pagMain.TabIndex := 0 ;
    pagMain.TabOrder := 0 ;
    pagMain.TabPosition := TTabPosition.Top ;

    tabGeocs := TTabItem.Create( pagMain ) ;
    tabGeocs.Parent := pagMain ;
    tabGeocs.IsSelected := True ;
    tabGeocs.Size.Width := 70 ;
    tabGeocs.Size.Height := 20 ;
    tabGeocs.Size.PlatformDefault := False ;
    tabGeocs.StyleLookup := '' ;
    tabGeocs.TabOrder := 0 ;
    tabGeocs.Text := _rsrcna( GIS_RS_CSSETUP_GEOGCS ) ;

    edtGeocsName := TEdit.Create( tabGeocs ) ;
    edtGeocsName.Parent := tabGeocs ;
    edtGeocsName.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                               TInteractiveGesture.DoubleTap] ;
    edtGeocsName.TabOrder := 0 ;
    edtGeocsName.Text := 'edtGeocsName' ;
    edtGeocsName.Position.Y := 6 ;
    edtGeocsName.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtGeocsName, 6, -1 ) ;
    edtGeocsName.Size.PlatformDefault := False ;
    edtGeocsName.KillFocusByReturn := True ;

    lblGeocsDatum := TLabel.Create( tabGeocs ) ;
    lblGeocsDatum.Parent := tabGeocs ;
    lblGeocsDatum.AutoSize := True ;
    lblGeocsDatum.Position.Y := 35 ;
    lblGeocsDatum.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblGeocsDatum, 6, -1 ) ;
    lblGeocsDatum.Size.PlatformDefault := False ;
    lblGeocsDatum.TextSettings.WordWrap := False ;
    lblGeocsDatum.Text := _rsrcna( GIS_RS_CSSETUP_DATUM ) ;
    lblGeocsDatum.FixSize ;

    cmbGeocsDatum := TComboEdit.Create( tabGeocs ) ;
    cmbGeocsDatum.Parent := tabGeocs ;
    cmbGeocsDatum.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    cmbGeocsDatum.TabOrder := 1 ;
    cmbGeocsDatum.ItemHeight := 19 ;
    cmbGeocsDatum.ItemIndex := -1 ;
    cmbGeocsDatum.ListBoxResource := 'combolistboxstyle' ;
    cmbGeocsDatum.Position.Y := 52 ;
    cmbGeocsDatum.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocsDatum, 6, -1 ) ;
    cmbGeocsDatum.Size.PlatformDefault := False ;
    cmbGeocsDatum.OnChange := doGeocsChange ;

    lblGeocsPrimem := TLabel.Create( tabGeocs ) ;
    lblGeocsPrimem.Parent := tabGeocs ;
    lblGeocsPrimem.AutoSize := True ;
    lblGeocsPrimem.Position.Y := 89 ;
    lblGeocsPrimem.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblGeocsPrimem, 6, 164 ) ;
    lblGeocsPrimem.Size.PlatformDefault := False ;
    lblGeocsPrimem.TextSettings.WordWrap := False ;
    lblGeocsPrimem.Text := _rsrcna( GIS_RS_CSSETUP_PRIME_MERIDIAN ) ;
    lblGeocsPrimem.FixSize ;

    cmbGeocsPrimem := TComboEdit.Create( tabGeocs ) ;
    cmbGeocsPrimem.Parent := tabGeocs ;
    cmbGeocsPrimem.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                 TInteractiveGesture.DoubleTap] ;
    cmbGeocsPrimem.TabOrder := 2 ;
    cmbGeocsPrimem.ItemHeight := 19 ;
    cmbGeocsPrimem.ItemIndex := -1 ;
    cmbGeocsPrimem.ListBoxResource := 'combolistboxstyle' ;
    cmbGeocsPrimem.Position.Y := 106 ;
    cmbGeocsPrimem.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocsPrimem, 6, 164 ) ;
    cmbGeocsPrimem.Size.PlatformDefault := False ;
    cmbGeocsPrimem.OnChange := doGeocsChange ;

    lblGeocsUnit := TLabel.Create( tabGeocs ) ;
    lblGeocsUnit.Parent := tabGeocs ;
    lblGeocsUnit.AutoSize := True ;
    lblGeocsUnit.Position.Y := 143 ;
    lblGeocsUnit.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblGeocsUnit, 6, 164 ) ;
    lblGeocsUnit.Size.PlatformDefault := False ;
    lblGeocsUnit.TextSettings.WordWrap := False ;
    lblGeocsUnit.Text := _rsrcna( GIS_RS_CSSETUP_UNIT ) ;
    lblGeocsUnit.FixSize ;

    cmbGeocsUnit := TComboEdit.Create( tabGeocs ) ;
    cmbGeocsUnit.Parent := tabGeocs ;
    cmbGeocsUnit.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                               TInteractiveGesture.DoubleTap] ;
    cmbGeocsUnit.TabOrder := 3 ;
    cmbGeocsUnit.ItemHeight := 19 ;
    cmbGeocsUnit.ItemIndex := -1 ;
    cmbGeocsUnit.ListBoxResource := 'combolistboxstyle' ;
    cmbGeocsUnit.Position.Y := 160 ;
    cmbGeocsUnit.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocsUnit, 6, 164 ) ;
    cmbGeocsUnit.Size.PlatformDefault := False ;
    cmbGeocsUnit.OnChange := doGeocsChange ;

    tabProjcs := TTabItem.Create( pagMain ) ;
    tabProjcs.Parent := pagMain ;
    tabProjcs.IsSelected := False ;
    tabProjcs.Size.Width := 69 ;
    tabProjcs.Size.Height := 20 ;
    tabProjcs.Size.PlatformDefault := False ;
    tabProjcs.StyleLookup := '' ;
    tabProjcs.TabOrder := 1 ;
    tabProjcs.Text := _rsrcna( GIS_RS_CSSETUP_PROJCS ) ;

    edtProjcsName := TEdit.Create( tabProjcs ) ;
    edtProjcsName.Parent := tabProjcs ;
    edtProjcsName.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    edtProjcsName.TabOrder := 0 ;
    edtProjcsName.Cursor := crDefault ;
    edtProjcsName.Text := 'edtProjcsName' ;
    edtProjcsName.Position.Y := 6 ;
    edtProjcsName.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtProjcsName, 6, -1 ) ;
    edtProjcsName.Size.PlatformDefault := False ;
    edtProjcsName.FixSize ;
    edtProjcsName.KillFocusByReturn := True ;

    lblProjcsProj := TLabel.Create( tabProjcs ) ;
    lblProjcsProj.Parent := tabProjcs ;
    lblProjcsProj.AutoSize := True ;
    lblProjcsProj.Position.Y := 35 ;
    lblProjcsProj.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsProj, 6, -1 ) ;
    lblProjcsProj.Size.PlatformDefault := False ;
    lblProjcsProj.TextSettings.WordWrap := False ;
    lblProjcsProj.Text := _rsrcna( GIS_RS_CSSETUP_PROJECTION ) ;
    lblProjcsProj.FixSize ;

    cmbProjcsProj := TComboEdit.Create( tabProjcs ) ;
    cmbProjcsProj.Parent := tabProjcs ;
    cmbProjcsProj.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    cmbProjcsProj.TabOrder := 1 ;
    cmbProjcsProj.ItemHeight := 19 ;
    cmbProjcsProj.ItemIndex := -1 ;
    cmbProjcsProj.ListBoxResource := 'combolistboxstyle' ;
    cmbProjcsProj.Position.Y := 52 ;
    cmbProjcsProj.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbProjcsProj, 6, -1 ) ;
    cmbProjcsProj.Size.PlatformDefault := False ;
    cmbProjcsProj.OnChange := cmbProjcsProjChange ;

    ScrollBox1 := TScrollBox.Create( tabProjcs ) ;
    ScrollBox1.Parent := tabProjcs ;
    ScrollBox1.Position.Y := 80 ;
    ScrollBox1.Size.Height := 145 ;
    PlaceControl( BiDiMode, nil, ScrollBox1, 6, -1 ) ;
    ScrollBox1.Size.PlatformDefault := False ;
    ScrollBox1.TabOrder := 2 ;
    ScrollBox1.OnClick := tabWktShow ;

    if BiDiMode = bdRightToLeft then
      anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;

    lblProjcsCentralMeridian := TLabel.Create( ScrollBox1 ) ;
    lblProjcsCentralMeridian.Parent := ScrollBox1 ;
    lblProjcsCentralMeridian.Anchors := anchors ;
    lblProjcsCentralMeridian.AutoSize := True ;
    lblProjcsCentralMeridian.Position.Y := 5 ;
    lblProjcsCentralMeridian.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsCentralMeridian, 10, 150 ) ;
    lblProjcsCentralMeridian.Size.PlatformDefault := False ;
    lblProjcsCentralMeridian.TextSettings.WordWrap := False ;
    lblProjcsCentralMeridian.Text := _rsrcna( GIS_RS_CSSETUP_CENTRAL_MERIDIAN ) ;
    lblProjcsCentralMeridian.FixSize ;

    lblProjcsLatitudeOfOrigin := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfOrigin.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfOrigin.Anchors := anchors ;
    lblProjcsLatitudeOfOrigin.AutoSize := True ;
    lblProjcsLatitudeOfOrigin.Position.Y := 5 ;
    lblProjcsLatitudeOfOrigin.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfOrigin, 170, 150 ) ;
    lblProjcsLatitudeOfOrigin.Size.PlatformDefault := False ;
    lblProjcsLatitudeOfOrigin.TextSettings.WordWrap := False ;
    lblProjcsLatitudeOfOrigin.Text := _rsrcna( GIS_RS_CSSETUP_LATITUDE_OF_ORIGIN ) ;
    lblProjcsLatitudeOfOrigin.FixSize ;

    lblProjcsFalseEasting := TLabel.Create( ScrollBox1 ) ;
    lblProjcsFalseEasting.Parent := ScrollBox1 ;
    lblProjcsFalseEasting.Anchors := anchors ;
    lblProjcsFalseEasting.AutoSize := True ;
    lblProjcsFalseEasting.Position.Y := 45 ;
    lblProjcsFalseEasting.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsFalseEasting, 10, 150 ) ;
    lblProjcsFalseEasting.Size.PlatformDefault := False ;
    lblProjcsFalseEasting.TextSettings.WordWrap := False ;
    lblProjcsFalseEasting.Text := _rsrcna( GIS_RS_CSSETUP_FALSE_EASTING ) ;
    lblProjcsFalseEasting.FixSize ;

    lblProjcsFalseNorthing := TLabel.Create( ScrollBox1 ) ;
    lblProjcsFalseNorthing.Parent := ScrollBox1 ;
    lblProjcsFalseNorthing.Anchors := anchors ;
    lblProjcsFalseNorthing.AutoSize := True ;
    lblProjcsFalseNorthing.Position.Y := 45 ;
    lblProjcsFalseNorthing.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsFalseNorthing, 170, 150 ) ;
    lblProjcsFalseNorthing.Size.PlatformDefault := False ;
    lblProjcsFalseNorthing.TextSettings.WordWrap := False ;
    lblProjcsFalseNorthing.Text := _rsrcna( GIS_RS_CSSETUP_FALSE_NORTHING ) ;
    lblProjcsFalseNorthing.FixSize ;

    lblProjcsStandardParallel_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsStandardParallel_1.Parent := ScrollBox1 ;
    lblProjcsStandardParallel_1.Anchors := anchors ;
    lblProjcsStandardParallel_1.AutoSize := True ;
    lblProjcsStandardParallel_1.Position.Y := 85 ;
    lblProjcsStandardParallel_1.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsStandardParallel_1, 10, 150 ) ;
    lblProjcsStandardParallel_1.Size.PlatformDefault := False ;
    lblProjcsStandardParallel_1.TextSettings.WordWrap := False ;
    lblProjcsStandardParallel_1.Text := _rsrcna( GIS_RS_CSSETUP_STANDARD_PARELLEL_1 ) ;
    lblProjcsStandardParallel_1.FixSize ;

    lblProjcsStandardParallel_2 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsStandardParallel_2.Parent := ScrollBox1 ;
    lblProjcsStandardParallel_2.Anchors := anchors ;
    lblProjcsStandardParallel_2.AutoSize := True ;
    lblProjcsStandardParallel_2.Position.Y := 85 ;
    lblProjcsStandardParallel_2.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsStandardParallel_2, 170, 150 ) ;
    lblProjcsStandardParallel_2.Size.PlatformDefault := False ;
    lblProjcsStandardParallel_2.TextSettings.WordWrap := False ;
    lblProjcsStandardParallel_2.Text := _rsrcna( GIS_RS_CSSETUP_STANDARD_PARALLEL_2 ) ;
    lblProjcsStandardParallel_2.FixSize ;

    lblProjcsPseudoStandardParallel_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsPseudoStandardParallel_1.Parent := ScrollBox1 ;
    lblProjcsPseudoStandardParallel_1.Anchors := anchors ;
    lblProjcsPseudoStandardParallel_1.AutoSize := True ;
    lblProjcsPseudoStandardParallel_1.Position.Y := 125 ;
    lblProjcsPseudoStandardParallel_1.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsPseudoStandardParallel_1, 10, 150 ) ;
    lblProjcsPseudoStandardParallel_1.Size.PlatformDefault := False ;
    lblProjcsPseudoStandardParallel_1.TextSettings.WordWrap := False ;
    lblProjcsPseudoStandardParallel_1.Text := _rsrcna( GIS_RS_CSSETUP_PSEUDO_STANDARD_PARALLEL_1 ) ;
    lblProjcsPseudoStandardParallel_1.FixSize ;

    lblProjcsZone := TLabel.Create( ScrollBox1 ) ;
    lblProjcsZone.Parent := ScrollBox1 ;
    lblProjcsZone.Anchors := anchors ;
    lblProjcsZone.AutoSize := True ;
    lblProjcsZone.Position.Y := 125 ;
    lblProjcsZone.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsZone, 170, 150 ) ;
    lblProjcsZone.Size.PlatformDefault := False ;
    lblProjcsZone.TextSettings.WordWrap := False ;
    lblProjcsZone.Text := _rsrcna( GIS_RS_CSSETUP_ZONE ) ;
    lblProjcsZone.FixSize ;

    lblProjcsScaleFactor := TLabel.Create( ScrollBox1 ) ;
    lblProjcsScaleFactor.Parent := ScrollBox1 ;
    lblProjcsScaleFactor.Anchors := anchors ;
    lblProjcsScaleFactor.AutoSize := True ;
    lblProjcsScaleFactor.Position.Y := 165 ;
    lblProjcsScaleFactor.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsScaleFactor, 10, 150 ) ;
    lblProjcsScaleFactor.Size.PlatformDefault := False ;
    lblProjcsScaleFactor.TextSettings.WordWrap := False ;
    lblProjcsScaleFactor.Text := _rsrcna( GIS_RS_CSSETUP_SCALE_FACTOR ) ;
    lblProjcsScaleFactor.FixSize ;

    lblProjcsLongitudeOfCenter := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLongitudeOfCenter.Parent := ScrollBox1 ;
    lblProjcsLongitudeOfCenter.Anchors := anchors ;
    lblProjcsLongitudeOfCenter.AutoSize := True ;
    lblProjcsLongitudeOfCenter.Position.Y := 205 ;
    lblProjcsLongitudeOfCenter.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLongitudeOfCenter, 10, 150 ) ;
    lblProjcsLongitudeOfCenter.Size.PlatformDefault := False ;
    lblProjcsLongitudeOfCenter.TextSettings.WordWrap := False ;
    lblProjcsLongitudeOfCenter.Text := _rsrcna( GIS_RS_CSSETUP_LONGITUDE_OF_CENTER ) ;
    lblProjcsLongitudeOfCenter.FixSize ;

    lblProjcsLatitudeOfCenter := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfCenter.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfCenter.Anchors := anchors ;
    lblProjcsLatitudeOfCenter.AutoSize := True ;
    lblProjcsLatitudeOfCenter.Position.Y := 205 ;
    lblProjcsLatitudeOfCenter.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfCenter, 170, 150 ) ;
    lblProjcsLatitudeOfCenter.Size.PlatformDefault := False ;
    lblProjcsLatitudeOfCenter.TextSettings.WordWrap := False ;
    lblProjcsLatitudeOfCenter.Text := _rsrcna( GIS_RS_CSSETUP_LATITUDE_OF_CENTER ) ;
    lblProjcsLatitudeOfCenter.FixSize ;

    lblProjcsAzimuth := TLabel.Create( ScrollBox1 ) ;
    lblProjcsAzimuth.Parent := ScrollBox1 ;
    lblProjcsAzimuth.Anchors := anchors ;
    lblProjcsAzimuth.AutoSize := True ;
    lblProjcsAzimuth.Position.Y := 165 ;
    lblProjcsAzimuth.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsAzimuth, 170, 150 ) ;
    lblProjcsAzimuth.Size.PlatformDefault := False ;
    lblProjcsAzimuth.TextSettings.WordWrap := False ;
    lblProjcsAzimuth.Text := _rsrcna( GIS_RS_CSSETUP_AZIMUTH ) ;
    lblProjcsAzimuth.FixSize ;

    lblProjcsLongitudeOfPoint_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLongitudeOfPoint_1.Parent := ScrollBox1 ;
    lblProjcsLongitudeOfPoint_1.Anchors := anchors ;
    lblProjcsLongitudeOfPoint_1.AutoSize := True ;
    lblProjcsLongitudeOfPoint_1.Position.Y := 245 ;
    lblProjcsLongitudeOfPoint_1.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLongitudeOfPoint_1, 10, 150 ) ;
    lblProjcsLongitudeOfPoint_1.Size.PlatformDefault := False ;
    lblProjcsLongitudeOfPoint_1.TextSettings.WordWrap := False ;
    lblProjcsLongitudeOfPoint_1.Text := _rsrcna( GIS_RS_CSSETUP_LONGITUDE_OF_POINT_1 ) ;
    lblProjcsLongitudeOfPoint_1.FixSize ;

    lblProjcsLatitudeOfPoint_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfPoint_1.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfPoint_1.Anchors := anchors ;
    lblProjcsLatitudeOfPoint_1.AutoSize := True ;
    lblProjcsLatitudeOfPoint_1.Position.Y := 245 ;
    lblProjcsLatitudeOfPoint_1.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfPoint_1, 170, 150 ) ;
    lblProjcsLatitudeOfPoint_1.Size.PlatformDefault := False ;
    lblProjcsLatitudeOfPoint_1.TextSettings.WordWrap := False ;
    lblProjcsLatitudeOfPoint_1.Text := _rsrcna( GIS_RS_CSSETUP_LATITUDE_OF_POINT_1 ) ;
    lblProjcsLatitudeOfPoint_1.FixSize ;

    lblProjcsLongitudeOfPoint_2 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLongitudeOfPoint_2.Parent := ScrollBox1 ;
    lblProjcsLongitudeOfPoint_2.Anchors := anchors ;
    lblProjcsLongitudeOfPoint_2.AutoSize := True ;
    lblProjcsLongitudeOfPoint_2.Position.Y := 285 ;
    lblProjcsLongitudeOfPoint_2.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLongitudeOfPoint_2, 10, 150 ) ;
    lblProjcsLongitudeOfPoint_2.Size.PlatformDefault := False ;
    lblProjcsLongitudeOfPoint_2.TextSettings.WordWrap := False ;
    lblProjcsLongitudeOfPoint_2.Text := _rsrcna( GIS_RS_CSSETUP_LONGITUDE_OF_POINT_2 ) ;
    lblProjcsLongitudeOfPoint_2.FixSize ;

    lblProjcsLatitudeOfPoint_2 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfPoint_2.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfPoint_2.Anchors := anchors ;
    lblProjcsLatitudeOfPoint_2.AutoSize := True ;
    lblProjcsLatitudeOfPoint_2.Position.Y := 285 ;
    lblProjcsLatitudeOfPoint_2.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfPoint_2, 170, 150 ) ;
    lblProjcsLatitudeOfPoint_2.Size.PlatformDefault := False ;
    lblProjcsLatitudeOfPoint_2.TextSettings.WordWrap := False ;
    lblProjcsLatitudeOfPoint_2.Text := _rsrcna( GIS_RS_CSSETUP_LATITUDE_OF_POINT_2 ) ;
    lblProjcsLatitudeOfPoint_2.FixSize ;

    lblProjcsYScale := TLabel.Create( ScrollBox1 ) ;
    lblProjcsYScale.Parent := ScrollBox1 ;
    lblProjcsYScale.Anchors := anchors ;
    lblProjcsYScale.AutoSize := True ;
    lblProjcsYScale.Position.Y := 325 ;
    lblProjcsYScale.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsYScale, 170, 150 ) ;
    lblProjcsYScale.Size.PlatformDefault := False ;
    lblProjcsYScale.TextSettings.WordWrap := False ;
    lblProjcsYScale.Text := _rsrcna( GIS_RS_CSSETUP_Y_SCALE ) ;
    lblProjcsYScale.FixSize ;

    lblProjcsXScale := TLabel.Create( ScrollBox1 ) ;
    lblProjcsXScale.Parent := ScrollBox1 ;
    lblProjcsXScale.Anchors := anchors ;
    lblProjcsXScale.AutoSize := True ;
    lblProjcsXScale.Position.Y := 325 ;
    lblProjcsXScale.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsXScale, 10, 150 ) ;
    lblProjcsXScale.Size.PlatformDefault := False ;
    lblProjcsXScale.TextSettings.WordWrap := False ;
    lblProjcsXScale.Text := _rsrcna( GIS_RS_CSSETUP_X_SCALE ) ;
    lblProjcsXScale.FixSize ;

    lblProjcsXYPlaneRotation := TLabel.Create( ScrollBox1 ) ;
    lblProjcsXYPlaneRotation.Parent := ScrollBox1 ;
    lblProjcsXYPlaneRotation.Anchors := anchors ;
    lblProjcsXYPlaneRotation.AutoSize := True ;
    lblProjcsXYPlaneRotation.Position.Y := 365 ;
    lblProjcsXYPlaneRotation.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsXYPlaneRotation, 10, 150 ) ;
    lblProjcsXYPlaneRotation.Size.PlatformDefault := False ;
    lblProjcsXYPlaneRotation.TextSettings.WordWrap := False ;
    lblProjcsXYPlaneRotation.Text := _rsrcna( GIS_RS_CSSETUP_XY_PLANE_ROTATION ) ;
    lblProjcsXYPlaneRotation.FixSize ;

    lblProjcsCentralParallel := TLabel.Create( ScrollBox1 ) ;
    lblProjcsCentralParallel.Parent := ScrollBox1 ;
    lblProjcsCentralParallel.Anchors := anchors ;
    lblProjcsCentralParallel.AutoSize := True ;
    lblProjcsCentralParallel.Position.Y := 405 ;
    lblProjcsCentralParallel.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsCentralParallel, 10, 150 ) ;
    lblProjcsCentralParallel.Size.PlatformDefault := False ;
    lblProjcsCentralParallel.TextSettings.WordWrap := False ;
    lblProjcsCentralParallel.Text := _rsrcna( GIS_RS_CSSETUP_CENTRAL_PARALLEL ) ;
    lblProjcsCentralParallel.FixSize ;

    edtProjcsCentralMeridian := TEdit.Create( ScrollBox1 ) ;
    edtProjcsCentralMeridian.Parent := ScrollBox1 ;
    edtProjcsCentralMeridian.Anchors := anchors ;
    edtProjcsCentralMeridian.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                           TInteractiveGesture.DoubleTap] ;
    edtProjcsCentralMeridian.TabOrder := 0 ;
    edtProjcsCentralMeridian.Text := 'edtProjcsCentralMeridian' ;
    edtProjcsCentralMeridian.Position.Y := 20 ;
    edtProjcsCentralMeridian.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsCentralMeridian, 10, 150 ) ;
    edtProjcsCentralMeridian.Size.PlatformDefault := False ;
    edtProjcsCentralMeridian.FixSize ;
    edtProjcsCentralMeridian.KillFocusByReturn := True ;
    edtProjcsCentralMeridian.Text := '0' ;
    edtProjcsCentralMeridian.OnChangeTracking := doProjcsVerify ;

    edtProjcsLatitudeOfOrigin := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfOrigin.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfOrigin.Anchors := anchors ;
    edtProjcsLatitudeOfOrigin.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                            TInteractiveGesture.DoubleTap] ;
    edtProjcsLatitudeOfOrigin.TabOrder := 1 ;
    edtProjcsLatitudeOfOrigin.Text := 'edtProjcsLatitudeOfOrigin' ;
    edtProjcsLatitudeOfOrigin.Position.Y := 20 ;
    edtProjcsLatitudeOfOrigin.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfOrigin, 170, 150 ) ;
    edtProjcsLatitudeOfOrigin.Size.PlatformDefault := False ;
    edtProjcsLatitudeOfOrigin.FixSize ;
    edtProjcsLatitudeOfOrigin.KillFocusByReturn := True ;
    edtProjcsLatitudeOfOrigin.Text := '0' ;
    edtProjcsLatitudeOfOrigin.OnChangeTracking := doProjcsVerify ;

    edtProjcsFalseEasting := TEdit.Create( ScrollBox1 ) ;
    edtProjcsFalseEasting.Parent := ScrollBox1 ;
    edtProjcsFalseEasting.Anchors := anchors ;
    edtProjcsFalseEasting.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                        TInteractiveGesture.DoubleTap] ;
    edtProjcsFalseEasting.TabOrder := 2 ;
    edtProjcsFalseEasting.Text := 'edtProjcsFalseEasting' ;
    edtProjcsFalseEasting.Position.Y := 60 ;
    edtProjcsFalseEasting.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsFalseEasting, 10, 150 ) ;
    edtProjcsFalseEasting.Size.PlatformDefault := False ;
    edtProjcsFalseEasting.FixSize ;
    edtProjcsFalseEasting.KillFocusByReturn := True ;
    edtProjcsFalseEasting.Text := '0' ;
    edtProjcsFalseEasting.OnChangeTracking := doProjcsVerify ;

    edtProjcsFalseNorthing := TEdit.Create( ScrollBox1 ) ;
    edtProjcsFalseNorthing.Parent := ScrollBox1 ;
    edtProjcsFalseNorthing.Anchors := anchors ;
    edtProjcsFalseNorthing.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                         TInteractiveGesture.DoubleTap] ;
    edtProjcsFalseNorthing.TabOrder := 3 ;
    edtProjcsFalseNorthing.Text := 'edtProjcsFalseNorthing' ;
    edtProjcsFalseNorthing.Position.Y := 60 ;
    edtProjcsFalseNorthing.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsFalseNorthing, 170, 150 ) ;
    edtProjcsFalseNorthing.Size.PlatformDefault := False ;
    edtProjcsFalseNorthing.FixSize ;
    edtProjcsFalseNorthing.KillFocusByReturn := True ;
    edtProjcsFalseNorthing.Text := '0' ;
    edtProjcsFalseNorthing.OnChangeTracking := doProjcsVerify ;

    edtProjcsStandardParallel_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsStandardParallel_1.Parent := ScrollBox1 ;
    edtProjcsStandardParallel_1.Anchors := anchors ;
    edtProjcsStandardParallel_1.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                              TInteractiveGesture.DoubleTap] ;
    edtProjcsStandardParallel_1.TabOrder := 4 ;
    edtProjcsStandardParallel_1.Text := 'edtProjcsStandardParallel_1' ;
    edtProjcsStandardParallel_1.Position.Y := 100 ;
    edtProjcsStandardParallel_1.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsStandardParallel_1, 10, 150 ) ;
    edtProjcsStandardParallel_1.Size.PlatformDefault := False ;
    edtProjcsStandardParallel_1.FixSize ;
    edtProjcsStandardParallel_1.KillFocusByReturn := True ;
    edtProjcsStandardParallel_1.Text := '0' ;
    edtProjcsStandardParallel_1.OnChangeTracking := doProjcsVerify ;

    edtProjcsStandardParallel_2 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsStandardParallel_2.Parent := ScrollBox1 ;
    edtProjcsStandardParallel_2.Anchors := anchors ;
    edtProjcsStandardParallel_2.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                              TInteractiveGesture.DoubleTap] ;
    edtProjcsStandardParallel_2.TabOrder := 5 ;
    edtProjcsStandardParallel_2.Text := 'edtProjcsStandardParallel_2' ;
    edtProjcsStandardParallel_2.Position.Y := 100 ;
    edtProjcsStandardParallel_2.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsStandardParallel_2, 170, 150 ) ;
    edtProjcsStandardParallel_2.Size.PlatformDefault := False ;
    edtProjcsStandardParallel_2.FixSize ;
    edtProjcsStandardParallel_2.KillFocusByReturn := True ;
    edtProjcsStandardParallel_2.Text := '0' ;
    edtProjcsStandardParallel_2.OnChangeTracking := doProjcsVerify ;

    edtProjcsPseudoStandardParallel_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsPseudoStandardParallel_1.Parent := ScrollBox1 ;
    edtProjcsPseudoStandardParallel_1.Anchors := anchors ;
    edtProjcsPseudoStandardParallel_1.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                                    TInteractiveGesture.DoubleTap] ;
    edtProjcsPseudoStandardParallel_1.TabOrder := 6 ;
    edtProjcsPseudoStandardParallel_1.Text := 'edtProjcsPseudoStandardParallel_1' ;
    edtProjcsPseudoStandardParallel_1.Position.Y := 140 ;
    edtProjcsPseudoStandardParallel_1.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsPseudoStandardParallel_1, 10, 150 ) ;
    edtProjcsPseudoStandardParallel_1.Size.PlatformDefault := False ;
    edtProjcsPseudoStandardParallel_1.FixSize ;
    edtProjcsPseudoStandardParallel_1.KillFocusByReturn := True ;
    edtProjcsPseudoStandardParallel_1.Text := '0' ;
    edtProjcsPseudoStandardParallel_1.OnChangeTracking := doProjcsVerify ;

    edtProjcsZone := TEdit.Create( ScrollBox1 ) ;
    edtProjcsZone.Parent := ScrollBox1 ;
    edtProjcsZone.Anchors := anchors ;
    edtProjcsZone.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    edtProjcsZone.TabOrder := 7 ;
    edtProjcsZone.Text := 'edtProjcsZone' ;
    edtProjcsZone.Position.Y := 140 ;
    edtProjcsZone.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsZone, 170, 150 ) ;
    edtProjcsZone.Size.PlatformDefault := False ;
    edtProjcsZone.FixSize ;
    edtProjcsZone.KillFocusByReturn := True ;
    edtProjcsZone.Text := '0' ;
    edtProjcsZone.OnChangeTracking := doProjcsVerify ;

    edtProjcsScaleFactor := TEdit.Create( ScrollBox1 ) ;
    edtProjcsScaleFactor.Parent := ScrollBox1 ;
    edtProjcsScaleFactor.Anchors := anchors ;
    edtProjcsScaleFactor.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                       TInteractiveGesture.DoubleTap] ;
    edtProjcsScaleFactor.TabOrder := 8 ;
    edtProjcsScaleFactor.Text := 'edtProjcsScaleFactor' ;
    edtProjcsScaleFactor.Position.Y := 180 ;
    edtProjcsScaleFactor.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsScaleFactor, 10, 150 ) ;
    edtProjcsScaleFactor.Size.PlatformDefault := False ;
    edtProjcsScaleFactor.FixSize ;
    edtProjcsScaleFactor.KillFocusByReturn := True ;
    edtProjcsScaleFactor.Text := '0' ;
    edtProjcsScaleFactor.OnChangeTracking := doProjcsVerify ;

    edtProjcsAzimuth := TEdit.Create( ScrollBox1 ) ;
    edtProjcsAzimuth.Parent := ScrollBox1 ;
    edtProjcsAzimuth.Anchors := anchors ;
    edtProjcsAzimuth.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                   TInteractiveGesture.DoubleTap] ;
    edtProjcsAzimuth.TabOrder := 9 ;
    edtProjcsAzimuth.Text := 'edtProjcsAzimuth' ;
    edtProjcsAzimuth.Position.Y := 180 ;
    edtProjcsAzimuth.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsAzimuth, 170, 150 ) ;
    edtProjcsAzimuth.Size.PlatformDefault := False ;
    edtProjcsAzimuth.FixSize ;
    edtProjcsAzimuth.KillFocusByReturn := True ;
    edtProjcsAzimuth.Text := '0' ;
    edtProjcsAzimuth.OnChangeTracking := doProjcsVerify ;

    edtProjcsLongitudeOfCenter := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLongitudeOfCenter.Parent := ScrollBox1 ;
    edtProjcsLongitudeOfCenter.Anchors := anchors ;
    edtProjcsLongitudeOfCenter.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                             TInteractiveGesture.DoubleTap] ;
    edtProjcsLongitudeOfCenter.TabOrder := 10 ;
    edtProjcsLongitudeOfCenter.Text := 'edtProjcsLongitudeOfCenter' ;
    edtProjcsLongitudeOfCenter.Position.Y := 220 ;
    edtProjcsLongitudeOfCenter.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLongitudeOfCenter, 10, 150 ) ;
    edtProjcsLongitudeOfCenter.Size.PlatformDefault := False ;
    edtProjcsLongitudeOfCenter.FixSize ;
    edtProjcsLongitudeOfCenter.KillFocusByReturn := True ;
    edtProjcsLongitudeOfCenter.Text := '0' ;
    edtProjcsLongitudeOfCenter.OnChangeTracking := doProjcsVerify ;

    edtProjcsLatitudeOfCenter := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfCenter.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfCenter.Anchors := anchors ;
    edtProjcsLatitudeOfCenter.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                            TInteractiveGesture.DoubleTap] ;
    edtProjcsLatitudeOfCenter.TabOrder := 11 ;
    edtProjcsLatitudeOfCenter.Text := 'edtProjcsLatitudeOfCenter' ;
    edtProjcsLatitudeOfCenter.Position.Y := 220 ;
    edtProjcsLatitudeOfCenter.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfCenter, 170, 150 ) ;
    edtProjcsLatitudeOfCenter.Size.PlatformDefault := False ;
    edtProjcsLatitudeOfCenter.FixSize ;
    edtProjcsLatitudeOfCenter.KillFocusByReturn := True ;
    edtProjcsLatitudeOfCenter.Text := '0' ;
    edtProjcsLatitudeOfCenter.OnChangeTracking := doProjcsVerify ;

    edtProjcsLongitudeOfPoint_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLongitudeOfPoint_1.Parent := ScrollBox1 ;
    edtProjcsLongitudeOfPoint_1.Anchors := anchors ;
    edtProjcsLongitudeOfPoint_1.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                              TInteractiveGesture.DoubleTap] ;
    edtProjcsLongitudeOfPoint_1.TabOrder := 12 ;
    edtProjcsLongitudeOfPoint_1.Text := 'edtProjcsLongitudeOfPoint_1' ;
    edtProjcsLongitudeOfPoint_1.Position.Y := 260 ;
    edtProjcsLongitudeOfPoint_1.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLongitudeOfPoint_1, 10, 150 ) ;
    edtProjcsLongitudeOfPoint_1.Size.PlatformDefault := False ;
    edtProjcsLongitudeOfPoint_1.FixSize ;
    edtProjcsLongitudeOfPoint_1.KillFocusByReturn := True ;
    edtProjcsLongitudeOfPoint_1.Text := '0' ;
    edtProjcsLongitudeOfPoint_1.OnChangeTracking := doProjcsVerify ;

    edtProjcsLatitudeOfPoint_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfPoint_1.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfPoint_1.Anchors := anchors ;
    edtProjcsLatitudeOfPoint_1.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                             TInteractiveGesture.DoubleTap] ;
    edtProjcsLatitudeOfPoint_1.TabOrder := 13 ;
    edtProjcsLatitudeOfPoint_1.Text := 'edtProjcsLatitudeOfPoint_1' ;
    edtProjcsLatitudeOfPoint_1.Position.Y := 260 ;
    edtProjcsLatitudeOfPoint_1.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfPoint_1, 170, 150 ) ;
    edtProjcsLatitudeOfPoint_1.Size.PlatformDefault := False ;
    edtProjcsLatitudeOfPoint_1.FixSize ;
    edtProjcsLatitudeOfPoint_1.KillFocusByReturn := True ;
    edtProjcsLatitudeOfPoint_1.Text := '0' ;
    edtProjcsLatitudeOfPoint_1.OnChangeTracking := doProjcsVerify ;

    edtProjcsLongitudeOfPoint_2 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLongitudeOfPoint_2.Parent := ScrollBox1 ;
    edtProjcsLongitudeOfPoint_2.Anchors := anchors ;
    edtProjcsLongitudeOfPoint_2.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                              TInteractiveGesture.DoubleTap] ;
    edtProjcsLongitudeOfPoint_2.TabOrder := 14 ;
    edtProjcsLongitudeOfPoint_2.Text := 'edtProjcsLongitudeOfPoint_2' ;
    edtProjcsLongitudeOfPoint_2.Position.Y := 300 ;
    edtProjcsLongitudeOfPoint_2.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLongitudeOfPoint_2, 10, 150 ) ;
    edtProjcsLongitudeOfPoint_2.Size.PlatformDefault := False ;
    edtProjcsLongitudeOfPoint_2.FixSize ;
    edtProjcsLongitudeOfPoint_2.KillFocusByReturn := True ;
    edtProjcsLongitudeOfPoint_2.Text := '0' ;
    edtProjcsLongitudeOfPoint_2.OnChangeTracking := doProjcsVerify ;

    edtProjcsLatitudeOfPoint_2 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfPoint_2.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfPoint_2.Anchors := anchors ;
    edtProjcsLatitudeOfPoint_2.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                             TInteractiveGesture.DoubleTap] ;
    edtProjcsLatitudeOfPoint_2.TabOrder := 15 ;
    edtProjcsLatitudeOfPoint_2.Text := 'edtProjcsLatitudeOfPoint_2' ;
    edtProjcsLatitudeOfPoint_2.Position.Y := 300 ;
    edtProjcsLatitudeOfPoint_2.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfPoint_2, 170, 150 ) ;
    edtProjcsLatitudeOfPoint_2.Size.PlatformDefault := False ;
    edtProjcsLatitudeOfPoint_2.FixSize ;
    edtProjcsLatitudeOfPoint_2.KillFocusByReturn := True ;
    edtProjcsLatitudeOfPoint_2.Text := '0' ;
    edtProjcsLatitudeOfPoint_2.OnChangeTracking := doProjcsVerify ;

    edtProjcsXScale :=  TEdit.Create( ScrollBox1 ) ;
    edtProjcsXScale.Parent := ScrollBox1 ;
    edtProjcsXScale.Anchors := anchors ;
    edtProjcsXScale.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                  TInteractiveGesture.DoubleTap] ;
    edtProjcsXScale.TabOrder := 16 ;
    edtProjcsXScale.Text := 'edtProjcsXScale' ;
    edtProjcsXScale.Position.Y := 340 ;
    edtProjcsXScale.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsXScale, 10, 150 ) ;
    edtProjcsXScale.Size.PlatformDefault := False ;
    edtProjcsXScale.FixSize ;
    edtProjcsXScale.KillFocusByReturn := True ;
    edtProjcsXScale.Text := '0' ;
    edtProjcsXScale.OnChangeTracking := doProjcsVerify ;

    edtProjcsYScale := TEdit.Create( ScrollBox1 ) ;
    edtProjcsYScale.Parent := ScrollBox1 ;
    edtProjcsYScale.Anchors := anchors ;
    edtProjcsYScale.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                  TInteractiveGesture.DoubleTap] ;
    edtProjcsYScale.TabOrder := 17 ;
    edtProjcsYScale.Text := 'edtProjcsYScale' ;
    edtProjcsYScale.Position.Y := 340 ;
    edtProjcsYScale.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsYScale, 170, 150 ) ;
    edtProjcsYScale.Size.PlatformDefault := False ;
    edtProjcsYScale.FixSize ;
    edtProjcsYScale.KillFocusByReturn := True ;
    edtProjcsYScale.Text := '0' ;
    edtProjcsYScale.OnChangeTracking := doProjcsVerify ;

    edtProjcsXYPlaneRotation := TEdit.Create( ScrollBox1 ) ;
    edtProjcsXYPlaneRotation.Parent := ScrollBox1 ;
    edtProjcsXYPlaneRotation.Anchors := anchors ;
    edtProjcsXYPlaneRotation.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                           TInteractiveGesture.DoubleTap] ;
    edtProjcsXYPlaneRotation.TabOrder := 18 ;
    edtProjcsXYPlaneRotation.Text := 'edtProjcsXYPlaneRotation' ;
    edtProjcsXYPlaneRotation.Position.Y := 380 ;
    edtProjcsXYPlaneRotation.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsXYPlaneRotation, 10, 150 ) ;
    edtProjcsXYPlaneRotation.Size.PlatformDefault := False ;
    edtProjcsXYPlaneRotation.FixSize ;
    edtProjcsXYPlaneRotation.KillFocusByReturn := True ;
    edtProjcsXYPlaneRotation.Text := '0' ;
    edtProjcsXYPlaneRotation.OnChangeTracking := doProjcsVerify ;

    edtProjcsCentralParallel := TEdit.Create( ScrollBox1 ) ;
    edtProjcsCentralParallel.Parent := ScrollBox1 ;
    edtProjcsCentralParallel.Anchors := anchors ;
    edtProjcsCentralParallel.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                           TInteractiveGesture.DoubleTap] ;
    edtProjcsCentralParallel.TabOrder := 19 ;
    edtProjcsCentralParallel.Text := 'edtProjcsCentralParallel' ;
    edtProjcsCentralParallel.Position.Y := 420 ;
    edtProjcsCentralParallel.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsCentralParallel, 10, 150 ) ;
    edtProjcsCentralParallel.Size.PlatformDefault := False ;
    edtProjcsCentralParallel.FixSize ;
    edtProjcsCentralParallel.KillFocusByReturn := True ;
    edtProjcsCentralParallel.Text := '0' ;
    edtProjcsCentralParallel.OnChangeTracking := doProjcsVerify ;

    cmbProjcsUnit := TComboEdit.Create( tabProjcs ) ;
    cmbProjcsUnit.Parent := tabProjcs ;
    cmbProjcsUnit.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    cmbProjcsUnit.TabOrder := 3 ;
    cmbProjcsUnit.ItemHeight := 19 ;
    cmbProjcsUnit.ItemIndex := -1 ;
    cmbProjcsUnit.ListBoxResource := 'combolistboxstyle' ;
    cmbProjcsUnit.Position.Y := 250 ;
    cmbProjcsUnit.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbProjcsUnit, 6, 325 ) ;
    cmbProjcsUnit.Size.PlatformDefault := False ;
    cmbProjcsUnit.OnChange := doProjcsChange ;

    lblProjcsUnit := TLabel.Create( tabProjcs ) ;
    lblProjcsUnit.Parent := tabProjcs ;
    edtProjcsCentralParallel.Anchors := anchors ;
    lblProjcsUnit.AutoSize := True ;
    lblProjcsUnit.Position.Y := 233 ;
    lblProjcsUnit.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblProjcsUnit, 6, 325 ) ;
    lblProjcsUnit.Size.PlatformDefault := False ;
    lblProjcsUnit.TextSettings.WordWrap := False ;
    lblProjcsUnit.Text := _rsrcna( GIS_RS_CSSETUP_UNIT ) ;
    lblProjcsUnit.FixSize ;

    lblGeocs := TLabel.Create( tabProjcs ) ;
    lblGeocs.Parent := tabProjcs ;
    lblGeocs.AutoSize := True ;
    lblGeocs.Position.Y := 283 ;
    lblGeocs.Size.Height := 16 ;
    PlaceControl( BiDiMode, nil, lblGeocs, 6, 325 ) ;
    lblGeocs.Size.PlatformDefault := False ;
    lblGeocs.TextSettings.WordWrap := False ;
    lblGeocs.Text := _rsrcna( GIS_RS_CSSETUP_GEOGCS ) ;
    lblGeocs.FixSize ;

    cmbGeocs := TComboEdit.Create( tabProjcs ) ;
    cmbGeocs.Parent := tabProjcs ;
    cmbGeocs.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                           TInteractiveGesture.DoubleTap] ;
    cmbGeocs.TabOrder := 4 ;
    cmbGeocs.ItemHeight := 19 ;
    cmbGeocs.ItemIndex := -1 ;
    cmbGeocs.ListBoxResource := 'combolistboxstyle' ;
    cmbGeocs.Position.Y := 300 ;
    cmbGeocs.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocs, 6, 325 ) ;
    cmbGeocs.Size.PlatformDefault := False ;
    cmbGeocs.OnChange := doProjcsChange ;

    btnGeocs := TCornerButton.Create( cmbGeocs ) ;
    btnGeocs.Parent := cmbGeocs ;
    if BiDiMode = TBiDiMode.bdRightToLeft  then
      btnGeocs.Corners := [TCorner.TopLeft, TCorner.BottomLeft]
    else
      btnGeocs.Corners := [TCorner.TopRight, TCorner.BottomRight] ;
    btnGeocs.Cursor := crArrow ;
    btnGeocs.Sides := [TSide.Top, TSide.Left, TSide.Bottom, TSide.Right] ;
    btnGeocs.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, btnGeocs, 330, 24 ) ;
    btnGeocs.Size.PlatformDefault := False ;
    btnGeocs.TabOrder := 5 ;
    if BiDiMode = TBiDiMode.bdRightToLeft  then
      btnGeocs.Text := '<'
    else
      btnGeocs.Text := '>' ;
    btnGeocs.XRadius := 10 ;
    btnGeocs.YRadius := 10 ;
    btnGeocs.OnClick := btnGeocsClick ;

    tabWkt := TTabItem.Create( pagMain ) ;
    tabWkt.Parent := pagMain ;
    tabWkt.IsSelected := False ;
    tabWkt.Size.Width := 59 ;
    tabWkt.Size.Height := 20 ;
    tabWkt.Size.PlatformDefault := False ;
    tabWkt.StyleLookup := '' ;
    tabWkt.TabOrder := 2 ;
    tabWkt.Text := _rsrcna( GIS_RS_CSSETUP_PROJCS_WKT ) ;
    tabWkt.OnClick := tabWktShow ;

    chkPretty := TCheckBox.Create( tabWkt ) ;
    chkPretty.Parent := tabWkt ;
    chkPretty.Position.Y := 10 ;
    chkPretty.Size.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkPretty, 6, 182 ) ;
    chkPretty.Size.PlatformDefault := False ;
    chkPretty.TabOrder := 0 ;
    chkPretty.Text := _rsrcna( GIS_RS_CSSETUP_PRETTY_WKT ) ;
    chkPretty.OnChange := chkPrettyChange ;

    btnSaveWkt := TButton.Create( tabWkt ) ;
    btnSaveWkt.Parent := tabWkt ;
    btnSaveWkt.Position.Y := 7 ;
    btnSaveWkt.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, btnSaveWkt, -6, 80 ) ;
    btnSaveWkt.Size.PlatformDefault := False ;
    btnSaveWkt.TabOrder := 2 ;
    btnSaveWkt.Text := _rsrcna( GIS_RS_CSSETUP_EXPORT ) ;
    btnSaveWkt.OnClick := btnSaveWktClick ;

    btnOpenWkt := TButton.Create( tabWkt ) ;
    btnOpenWkt.Parent := tabWkt ;
    btnOpenWkt.Position.Y := 7 ;
    btnOpenWkt.Size.Height := 21 ;
    PlaceControl( BiDiMode, btnSaveWkt, btnOpenWkt, -4, 80 ) ;
    btnOpenWkt.Size.PlatformDefault := False ;
    btnOpenWkt.TabOrder := 1 ;
    btnOpenWkt.Text := _rsrcna( GIS_RS_CSSETUP_IMPORT ) ;
    btnOpenWkt.OnClick := btnOpenWktClick ;

    memWkt := TMemo.Create( tabWkt ) ;
    memWkt.Parent := tabWkt ;
    memWkt.Touch.InteractiveGestures := [TInteractiveGesture.Pan,
                                         TInteractiveGesture.LongTap,
                                         TInteractiveGesture.DoubleTap] ;
    {$IFDEF LEVEL_XE8_FMX}
      memWkt.DataDetectorTypes := [] ;
    {$ENDIF}
    memWkt.OnChange := memWktChange ;
    memWkt.OnChangeTracking := memWktChange ;
    memWkt.Position.Y := 35 ;
    memWkt.Size.Height := 291 ;
    PlaceControl( BiDiMode, nil, memWkt, 6, -1 ) ;
    memWkt.Size.PlatformDefault := False ;
    {$IFDEF LEVEL_XE8_FMX}
      memWkt.ScrollDirections := TScrollDirections.Vertical ;
    {$ENDIF}
    memWkt.TabOrder := 3 ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
      btnHelp.TabOrder := 1 ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;
      btnCancel.TabOrder := 3 ;

      btnOK.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
      btnOK.TabOrder := 2 ;
    {$ENDIF}

    dlgOpenWkt := TOpenDialog.Create( oMainForm ) ;
    dlgOpenWkt.Parent := oMainForm ;
    dlgOpenWkt.DefaultExt := 'prj' ;
    dlgOpenWkt.Filter := _rsrcna( GIS_RS_CSSETUP_WKT_FILTER ) ;

    dlgSaveWkt := TSaveDialog.Create( oMainForm ) ;
    dlgSaveWkt.Parent := oMainForm ;
    dlgSaveWkt.DefaultExt := 'prj' ;
    dlgSaveWkt.Filter := _rsrcna( GIS_RS_CSSETUP_WKT_FILTER ) ;
    dlgSaveWkt.Options := [TOpenOption.ofOverwritePrompt,
                           TOpenOption.ofHideReadOnly,
                           TOpenOption.ofExtensionDifferent,
                           TOpenOption.ofEnableSizing] ;
  end ;

  procedure TGIS_ControlCSSystemSetup.Execute(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _onhelp : TGIS_HelpEvent          ;
    const _proc   : TProc<TModalResult>
  ) ;
  begin
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    FCS := _cs ;

    if FCS is TGIS_CSProjectedCoordinateSystem then
      pagMain.ActiveTab := tabProjcs
    else
      pagMain.ActiveTab := tabGeocs ;

    setupCs ;

    ShowModalEx( _proc ) ;
  end ;

  procedure TGIS_ControlCSSystemSetup.Execute(
    const _cs   : TGIS_CSCoordinateSystem ;
    const _proc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _cs, hlp, _proc ) ;
  end;

{==================================== END =====================================}
end.

