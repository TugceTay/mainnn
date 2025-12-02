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
  Coordinate System. Dialog box to view predefined projection.
}

unit Lider.CG.GIS.VCL.GeoControlCsSystemSetup ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlCsSystemSetup"'}

interface

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.Buttons,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.GeoCsProjections,
  Lider.CG.GIS.VCL.GeoModalForm,
  Lider.CG.GIS.VCL.GeoControlCsHelper;

type

  /// <summary>
  ///   Dialog box to alter existing Coordinate System.
  /// </summary>
  TGIS_ControlCSSystemSetup = class( TGIS_ModalForm )
    // form
      pagMain: TPageControl;
      tabGeocs: TTabSheet;
      tabProjcs: TTabSheet;
      tabWkt: TTabSheet;
      edtProjcsName: TEdit;
      lblGeocsDatum: TLabel;
      lblGeocsPrimem: TLabel;
      lblGeocsUnit: TLabel;
      cmbGeocsUnit: TComboBox;
      cmbGeocsDatum: TComboBox;
      cmbGeocsPrimem: TComboBox;
      edtGeocsName: TEdit;
      lblProjcsUnit: TLabel;
      cmbProjcsUnit: TComboBox;
      lblProjcsProj: TLabel;
      cmbProjcsProj: TComboBox;
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
      cmbGeocs: TComboBox;
      btnGeocs: TSpeedButton;
      memWkt: TMemo;
      btnOpenWkt: TButton;
      btnSaveWkt: TButton;
      dlgOpenWkt: TOpenDialog;
      dlgSaveWkt: TSaveDialog;
      chkPretty: TCheckBox;
      procedure frmCreate(Sender: TObject);
      procedure frmDestroy(Sender: TObject);
      procedure btnOKClick(Sender: TObject); override;
      procedure cmbProjcsProjChange(Sender: TObject);
      procedure doProjcsVerify(Sender: TObject);
      procedure btnOpenWktClick(Sender: TObject);
      procedure btnSaveWktClick(Sender: TObject);
      procedure tabWktShow(Sender: TObject);
      procedure memWktChange(Sender: TObject);
      procedure chkPrettyClick(Sender: TObject);
      procedure btnGeocsClick(Sender: TObject);
      procedure doGeocsChange(Sender: TObject);
      procedure doProjcsChange(Sender: TObject);

    private
      FCS : TGIS_CSCoordinateSystem ;

      hlpGeocs        : TGIS_CSAbstractListHelper ;

      hlpGeocsDatum   : TGIS_CSAbstractListHelper ;
      hlpGeocsPrimem  : TGIS_CSAbstractListHelper ;
      hlpGeocsUnit    : TGIS_CSAbstractListHelper ;

      hlpProjcsUnit   : TGIS_CSAbstractListHelper ;
      hlpProjcsProj   : TGIS_CSAbstractListHelper ;

    private

      /// <summary>
      ///   Prepare dialog based on CS.
      /// </summary>
      procedure setupCs ;

      /// <summary>
      ///   Read default parameters from the projection.
      /// </summary>
      /// <param name="_proj">
      ///    projection to be read
      /// </param>
      /// <param name="_gcs">
      ///    system to obtain primemeridian
      /// </param>
      procedure readProjParams( const _proj : TGIS_CSProjAbstract ;
                                const _gcs  : TGIS_CSGeographicCoordinateSystem
                              ) ;

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
      ///  Modal result.
      /// </returns>
      function Execute  ( const _cs     : TGIS_CSCoordinateSystem ;
                          const _onhelp : TGIS_HelpEvent
                        ) : Integer ; overload;

      /// <summary>
      ///   Execute dialog on a given coordinate system.
      /// </summary>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <returns>
      ///  Modal result.
      /// </returns>
      function Execute  ( const _cs     : TGIS_CSCoordinateSystem
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
  System.Math,

  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.GeoCsFactory,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.VCL.GeoControlHelper ;

//==============================================================================
// Private functions
//==============================================================================

  procedure TGIS_ControlCSSystemSetup.setupCs ;
  var
    gcs : TGIS_CSGeographicCoordinateSystem ;
    pcs : TGIS_CSProjectedCoordinateSystem ;
  begin
    if CS is TGIS_CSGeographicCoordinateSystem then begin
      tabGeocs.TabVisible  := True  ;
      tabProjcs.TabVisible := False ;

      gcs := CS as TGIS_CSGeographicCoordinateSystem ;
      hlpGeocsDatum.ByEpsg( gcs.Datum.EPSG ) ;
      hlpGeocsPrimem.ByEpsg( gcs.PrimeMeridian.EPSG ) ;
      hlpGeocsUnit.ByEpsg( gcs.Units.EPSG ) ;

      edtGeocsName.Text := gcs.WKT ;
      edtGeocsName.Enabled := False ;
    end
    else if CS is TGIS_CSProjectedCoordinateSystem then begin
      tabGeocs.TabVisible  := False ;
      tabProjcs.TabVisible := True  ;

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
    Sender: TObject
  );
  begin
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
  end;

  procedure TGIS_ControlCSSystemSetup.frmDestroy(Sender: TObject);
  begin
    FreeObject( hlpGeocs       ) ;
    FreeObject( hlpGeocsDatum  ) ;
    FreeObject( hlpGeocsPrimem ) ;
    FreeObject( hlpGeocsUnit   ) ;

    FreeObject( hlpProjcsUnit  ) ;
    FreeObject( hlpProjcsProj  ) ;
  end;

  procedure TGIS_ControlCSSystemSetup.tabWktShow(Sender: TObject);
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

    if chkPretty.Checked then begin
      memWkt.ScrollBars := ssBoth ;
      memWkt.WordWrap := False ;
      memWkt.Text := CS.PrettyWKT ;
    end
    else begin
      memWkt.ScrollBars := ssVertical ;
      memWkt.WordWrap := True ;
      memWkt.Text := CS.FullWKT ;
    end ;
  end;

  procedure TGIS_ControlCSSystemSetup.btnOpenWktClick(
    Sender: TObject
  ) ;
  begin
    if dlgOpenWkt.Execute() then begin
      memWkt.Lines.LoadFromFile( dlgOpenWkt.FileName );
      FCS := TGIS_CSFactory.ByWKT( memWkt.Text ) ;
      setupCs ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.btnGeocsClick(Sender: TObject);
  var
    dlg : TGIS_ControlCSSystemSetup ;
    gcs : TGIS_CSCoordinateSystem ;
  begin
    gcs := TGIS_CSCoordinateSystem( hlpGeocs.SelectedObject ) ;
    dlg := TGIS_ControlCSSystemSetup.Create( Self );
    try
      if dlg.Execute( gcs, pOnHelp ) = mrOk then begin
        gcs := dlg.CS ;
        if gcs.EPSG > 0 then
          hlpGeocs.ByEPSG( gcs.EPSG ) ;
      end ;
    finally
      dlg.Free ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.btnSaveWktClick(
    Sender: TObject
  ) ;
  begin
    if dlgSaveWkt.Execute() then begin
      memWkt.Lines.SaveToFile( dlgSaveWkt.FileName );
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.btnOKClick(
    Sender : TObject
  ) ;
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
    inherited ;
  end;

procedure TGIS_ControlCSSystemSetup.cmbProjcsProjChange(
    Sender: TObject
  ) ;
  var
    obj : TGIS_CSProjAbstract ;
    itop : Integer ;
    bline : Boolean ;

    hlbl : Integer ;
    hlin : Integer ;
  begin
    doProjcsChange( Sender ) ;

    obj := nil ;
    if not Assigned( hlpProjcsProj ) then begin
      if Sender is TGIS_CSAbstractListHelper then
        obj := TGIS_CSProjAbstract( (Sender as TGIS_CSAbstractListHelper).SelectedObject ) ;
    end
    else
      obj := TGIS_CSProjAbstract( hlpProjcsProj.SelectedObject ) ;

    hlbl := lblProjcsCentralMeridian.Height + 2 ;
    hlin := edtProjcsCentralMeridian.Height + hlbl + 6 ;

    ScrollBox1.VertScrollBar.Position := 0 ;

    itop := 5 ;

    // line 1
    bline := False ;
    if TGIS_CSProjParameter.CentralMeridian in obj.ParametersSet then begin
      lblProjcsCentralMeridian.Visible := True  ;
      edtProjcsCentralMeridian.Visible := True  ;
      lblProjcsCentralMeridian.Top     := itop  ;
      edtProjcsCentralMeridian.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsCentralMeridian.Visible := False ;
      edtProjcsCentralMeridian.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfOrigin in obj.ParametersSet then begin
      lblProjcsLatitudeOfOrigin.Visible := True  ;
      edtProjcsLatitudeOfOrigin.Visible := True  ;
      lblProjcsLatitudeOfOrigin.Top     := itop  ;
      edtProjcsLatitudeOfOrigin.Top     := itop + hlbl ;
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
      lblProjcsFalseEasting.Top     := itop  ;
      edtProjcsFalseEasting.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsFalseEasting.Visible := False ;
      edtProjcsFalseEasting.Visible := False ;
    end ;

    if TGIS_CSProjParameter.FalseNorthing in obj.ParametersSet then begin
      lblProjcsFalseNorthing.Visible := True  ;
      edtProjcsFalseNorthing.Visible := True  ;
      lblProjcsFalseNorthing.Top     := itop  ;
      edtProjcsFalseNorthing.Top     := itop + hlbl ;
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
      lblProjcsStandardParallel_1.Top     := itop  ;
      edtProjcsStandardParallel_1.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsStandardParallel_1.Visible := False ;
      edtProjcsStandardParallel_1.Visible := False ;
    end ;

    if TGIS_CSProjParameter.StandardParallel_2 in obj.ParametersSet then begin
      lblProjcsStandardParallel_2.Visible := True  ;
      edtProjcsStandardParallel_2.Visible := True  ;
      lblProjcsStandardParallel_2.Top     := itop  ;
      edtProjcsStandardParallel_2.Top     := itop + hlbl ;
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
      lblProjcsPseudoStandardParallel_1.Top     := itop  ;
      edtProjcsPseudoStandardParallel_1.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsPseudoStandardParallel_1.Visible := False ;
      edtProjcsPseudoStandardParallel_1.Visible := False ;
    end ;

    if TGIS_CSProjParameter.Zone in obj.ParametersSet then begin
      lblProjcsZone.Visible := True  ;
      edtProjcsZone.Visible := True  ;
      lblProjcsZone.Top     := itop  ;
      edtProjcsZone.Top     := itop + hlbl ;
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
      lblProjcsScaleFactor.Top     := itop  ;
      edtProjcsScaleFactor.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsScaleFactor.Visible := False ;
      edtProjcsScaleFactor.Visible := False ;
    end ;

    if TGIS_CSProjParameter.Azimuth in obj.ParametersSet then begin
      lblProjcsAzimuth.Visible := True  ;
      edtProjcsAzimuth.Visible := True  ;
      lblProjcsAzimuth.Top     := itop  ;
      edtProjcsAzimuth.Top     := itop + hlbl ;
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
      lblProjcsLongitudeOfCenter.Top     := itop  ;
      edtProjcsLongitudeOfCenter.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLongitudeOfCenter.Visible := False ;
      edtProjcsLongitudeOfCenter.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfCenter in obj.ParametersSet then begin
      lblProjcsLatitudeOfCenter.Visible := True  ;
      edtProjcsLatitudeOfCenter.Visible := True  ;
      lblProjcsLatitudeOfCenter.Top     := itop  ;
      edtProjcsLatitudeOfCenter.Top     := itop + hlbl ;
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
      lblProjcsLongitudeOfPoint_1.Top     := itop  ;
      edtProjcsLongitudeOfPoint_1.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLongitudeOfPoint_1.Visible := False ;
      edtProjcsLongitudeOfPoint_1.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfPoint_1 in obj.ParametersSet then begin
      lblProjcsLatitudeOfPoint_1.Visible := True  ;
      edtProjcsLatitudeOfPoint_1.Visible := True  ;
      lblProjcsLatitudeOfPoint_1.Top     := itop  ;
      edtProjcsLatitudeOfPoint_1.Top     := itop + hlbl ;
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
      lblProjcsLongitudeOfPoint_2.Top     := itop  ;
      edtProjcsLongitudeOfPoint_2.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsLongitudeOfPoint_2.Visible := False ;
      edtProjcsLongitudeOfPoint_2.Visible := False ;
    end ;

    if TGIS_CSProjParameter.LatitudeOfPoint_2 in obj.ParametersSet then begin
      lblProjcsLatitudeOfPoint_2.Visible := True  ;
      edtProjcsLatitudeOfPoint_2.Visible := True  ;
      lblProjcsLatitudeOfPoint_2.Top     := itop  ;
      edtProjcsLatitudeOfPoint_2.Top     := itop + hlbl ;
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
      lblProjcsXScale.Top     := itop  ;
      edtProjcsXScale.Top     := itop + hlbl ;
      bline := bline or True ;
    end
    else begin
      lblProjcsXScale.Visible := False ;
      edtProjcsXScale.Visible := False ;
    end ;

    if TGIS_CSProjParameter.YScale in obj.ParametersSet then begin
      lblProjcsYScale.Visible := True  ;
      edtProjcsYScale.Visible := True  ;
      lblProjcsYScale.Top     := itop  ;
      edtProjcsYScale.Top     := itop + hlbl ;
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
      lblProjcsXYPlaneRotation.Top     := itop  ;
      edtProjcsXYPlaneRotation.Top     := itop + hlbl ;
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

    if itop  + 5 > ScrollBox1.Height then
      ScrollBox1.VertScrollBar.Range := itop + 5
    else
      ScrollBox1.VertScrollBar.Range := itop ;

    readProjParams(
      obj,
      TGIS_CSGeographicCoordinateSystem( hlpGeocs.SelectedObject )
    ) ;
  end;

  procedure TGIS_ControlCSSystemSetup.chkPrettyClick(
    Sender: TObject
  ) ;
  begin
    if Assigned( CS ) then begin
      memWktChange( Sender );
      if chkPretty.Checked then begin
        memWkt.ScrollBars := ssBoth ;
        memWkt.WordWrap := False ;
        memWkt.Text := CS.PrettyWKT ;
      end
      else begin
        memWkt.ScrollBars := ssVertical ;
        memWkt.WordWrap := True ;
        memWkt.Text := CS.FullWKT ;
      end ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.doProjcsVerify(
    Sender: TObject);
  var
    res : Boolean ;
    obj : TGIS_CSProjAbstract ;

    function verify_longitude( const _edt : TEdit ) : Boolean ;
    begin
      try
        GisStrToLongitude( _edt.Text ) ;
        _edt.Color := clWindow ;
        Result := False ;
      except
        _edt.Color := clRed ;
        Result := False ;
      end;
    end ;

    function verify_latitude( const _edt : TEdit ) : Boolean ;
    begin
      try
        GisStrToLatitude( _edt.Text ) ;
        _edt.Color := clWindow ;
        Result := False ;
      except
        _edt.Color := clRed ;
        Result := True  ;
      end;
    end ;

    function verify_angle( const _edt : TEdit ) : Boolean ;
    begin
      try
        GisStrToAngle( _edt.Text ) ;
        _edt.Color := clWindow ;
        Result := False ;
      except
        _edt.Color := clRed ;
        Result := True  ;
      end;
    end ;

    function verify_float( const _edt : TEdit ) : Boolean ;
    begin
      try
        DotStrToFloat( _edt.Text ) ;
        _edt.Color := clWindow ;
        Result := False ;
      except
        _edt.Color := clRed ;
        Result := True  ;
      end;
    end ;

    function verify_int( const _edt : TEdit ) : Boolean ;
    begin
      try
        StrToInt( _edt.Text ) ;
        _edt.Color := clWindow ;
        Result := False ;
      except
        _edt.Color := clRed ;
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

    btnOK.Enabled := not res ;

    doProjcsChange( Sender );

  end;

  procedure TGIS_ControlCSSystemSetup.memWktChange(
    Sender: TObject
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
      memWkt.Font.Color := clBlack ;
      chkPretty.Enabled  := True ;
      btnOK.Enabled      := True ;
      btnSaveWkt.Enabled := True ;
    end
    else begin
      memWkt.Font.Color := clRed ;
      chkPretty.Enabled  := False ;
      btnOK.Enabled      := False ;
      btnSaveWkt.Enabled := False ;
    end;
  end;

  procedure TGIS_ControlCSSystemSetup.doGeocsChange(Sender: TObject);
  begin
     edtGeocsName.Text := 'Custom GCS' ;
     edtGeocsName.Enabled := True  ;
  end;

  procedure TGIS_ControlCSSystemSetup.doProjcsChange(Sender: TObject);
  begin
     edtProjcsName.Text := 'Custom PCS' ;
     edtProjcsName.Enabled := True  ;
  end;

//==============================================================================
// Various
//==============================================================================

  procedure TGIS_ControlCSSystemSetup.initForm  ;
  begin
    Self.Caption := _rsrc( GIS_RS_CSSETUP_DLG ) ;
    Self.ClientHeight := 402 ;
    Self.ClientWidth := 385 ;
    Self.OnCreate := frmCreate ;
    Self.OnDestroy := frmDestroy ;
    Self.Name := 'TGIS_ControlCSSystemSetup' ;
  end ;

  procedure TGIS_ControlCSSystemSetup.initControls ;
  var
    anchors   : TAnchors ;
  begin

    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;

    pagMain := TPageControl.Create( Self ) ;
    pagMain.Parent := Self ;
    pagMain.Left := 8 ;
    pagMain.Top := 8 ;
    pagMain.Width := 369 ;
    pagMain.Height := 353 ;
    pagMain.ActivePage := tabGeocs ;
    pagMain.TabOrder := 0 ;

    tabGeocs := TTabSheet.Create( pagMain ) ;
    tabGeocs.Parent := pagMain ;
    tabGeocs.Caption := _rsrc( GIS_RS_CSSETUP_GEOGCS ) ;
    tabGeocs.PageControl := pagMain ;

    edtGeocsName := TEdit.Create( tabGeocs ) ;
    edtGeocsName.Parent := tabGeocs ;
    edtGeocsName.Top := 3 ;
    edtGeocsName.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtGeocsName, 3, -1 ) ;
    edtGeocsName.TabOrder := 0 ;
    edtGeocsName.Text := 'edtGeocsName' ;

    lblGeocsDatum := TLabel.Create( tabGeocs ) ;
    lblGeocsDatum.Parent := tabGeocs ;
    lblGeocsDatum.Top := 35 ;
    lblGeocsDatum.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblGeocsDatum, 3, 70 ) ;
    lblGeocsDatum.Caption := _rsrc( GIS_RS_CSSETUP_DATUM ) ;
    lblGeocsDatum.FocusControl := cmbGeocsDatum ;

    cmbGeocsDatum := TComboBox.Create( tabGeocs ) ;
    cmbGeocsDatum.Parent := tabGeocs ;
    cmbGeocsDatum.Top := 52 ;
    cmbGeocsDatum.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocsDatum, 3, -1 ) ;
    cmbGeocsDatum.TabOrder := 1 ;
    cmbGeocsDatum.Text := 'cmbGeocsDatum' ;
    cmbGeocsDatum.OnChange := doGeocsChange ;

    lblGeocsPrimem := TLabel.Create( tabGeocs ) ;
    lblGeocsPrimem.Parent := tabGeocs ;
    lblGeocsPrimem.Top := 89 ;
    lblGeocsPrimem.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblGeocsPrimem, 3, 73 ) ;
    lblGeocsPrimem.Caption := _rsrc( GIS_RS_CSSETUP_PRIME_MERIDIAN ) ;
    lblGeocsPrimem.FocusControl := cmbGeocsPrimem ;

    cmbGeocsPrimem := TComboBox.Create( tabGeocs ) ;
    cmbGeocsPrimem.Parent := tabGeocs ;
    cmbGeocsPrimem.Top := 106 ;
    cmbGeocsPrimem.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocsPrimem, 3, 164 ) ;
    cmbGeocsPrimem.TabOrder := 2 ;
    cmbGeocsPrimem.Text := 'cmbGeocsPrimem' ;
    cmbGeocsPrimem.OnChange := doGeocsChange ;

    lblGeocsUnit := TLabel.Create( tabGeocs ) ;
    lblGeocsUnit.Parent := tabGeocs ;
    lblGeocsUnit.Top := 143 ;
    lblGeocsUnit.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblGeocsUnit, 3, 58 ) ;
    lblGeocsUnit.Caption := _rsrc( GIS_RS_CSSETUP_UNIT ) ;
    lblGeocsUnit.FocusControl := cmbGeocsUnit ;

    cmbGeocsUnit := TComboBox.Create( tabGeocs ) ;
    cmbGeocsUnit.Parent := tabGeocs ;
    cmbGeocsUnit.Top := 160 ;
    cmbGeocsUnit.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocsUnit, 3, 164 ) ;
    cmbGeocsUnit.TabOrder := 3 ;
    cmbGeocsUnit.Text := 'cmbGeocsUnit' ;
    cmbGeocsUnit.OnChange := doGeocsChange ;

    tabProjcs := TTabSheet.Create( pagMain ) ;
    tabProjcs.Parent := pagMain ;
    tabProjcs.Caption := _rsrc( GIS_RS_CSSETUP_PROJCS ) ;
    tabProjcs.ImageIndex := 1 ;
    tabProjcs.PageControl := pagMain ;

    edtProjcsName := TEdit.Create( tabProjcs ) ;
    edtProjcsName.Parent := tabProjcs ;
    edtProjcsName.Top := 3 ;
    edtProjcsName.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtProjcsName, 3, -1 ) ;
    edtProjcsName.TabOrder := 0 ;
    edtProjcsName.Text := 'edtProjcsName' ;

    lblProjcsProj := TLabel.Create( tabProjcs ) ;
    lblProjcsProj.Parent := tabProjcs ;
    lblProjcsProj.Top := 35 ;
    lblProjcsProj.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsProj, 3, 58 ) ;
    lblProjcsProj.Caption := _rsrc( GIS_RS_CSSETUP_PROJECTION ) ;
    lblProjcsProj.FocusControl := cmbProjcsProj ;

    cmbProjcsProj := TComboBox.Create( tabProjcs ) ;
    cmbProjcsProj.Parent := tabProjcs ;
    cmbProjcsProj.Top := 52 ;
    cmbProjcsProj.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbProjcsProj, 3, -1 ) ;
    cmbProjcsProj.TabOrder := 1 ;
    cmbProjcsProj.Text := 'cmbProjcsProj' ;
    cmbProjcsProj.OnChange := cmbProjcsProjChange ;

    ScrollBox1 := TScrollBox.Create( tabProjcs ) ;
    ScrollBox1.Parent := tabProjcs ;
    ScrollBox1.Top := 79 ;
    ScrollBox1.Height := 143 ;
    PlaceControl( BiDiMode, nil, ScrollBox1, 3, -1 ) ;
    ScrollBox1.BevelInner := bvNone ;
    ScrollBox1.BevelOuter := bvNone ;
    ScrollBox1.BevelWidth := 2 ;
    ScrollBox1.Ctl3D := False ;
    ScrollBox1.ParentBackground := True ;
    ScrollBox1.ParentCtl3D := False ;
    ScrollBox1.TabOrder := 2 ;

    lblProjcsCentralMeridian := TLabel.Create( ScrollBox1 ) ;
    lblProjcsCentralMeridian.Parent := ScrollBox1 ;
    lblProjcsCentralMeridian.Anchors := anchors ;
    lblProjcsCentralMeridian.Top := 5 ;
    lblProjcsCentralMeridian.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsCentralMeridian, 10, 114 ) ;
    lblProjcsCentralMeridian.Caption := _rsrc( GIS_RS_CSSETUP_CENTRAL_MERIDIAN ) ;
    lblProjcsCentralMeridian.FocusControl := edtProjcsCentralMeridian ;

    lblProjcsLatitudeOfOrigin := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfOrigin.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfOrigin.Anchors := anchors ;
    lblProjcsLatitudeOfOrigin.Top := 5 ;
    lblProjcsLatitudeOfOrigin.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfOrigin, 170, 118 ) ;
    lblProjcsLatitudeOfOrigin.Caption := _rsrc( GIS_RS_CSSETUP_LATITUDE_OF_ORIGIN ) ;
    lblProjcsLatitudeOfOrigin.FocusControl := edtProjcsLatitudeOfOrigin ;

    lblProjcsFalseEasting := TLabel.Create( ScrollBox1 ) ;
    lblProjcsFalseEasting.Parent := ScrollBox1 ;
    lblProjcsFalseEasting.Anchors := anchors ;
    lblProjcsFalseEasting.Top := 45 ;
    lblProjcsFalseEasting.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsFalseEasting, 10, 99 ) ;
    lblProjcsFalseEasting.Caption := _rsrc( GIS_RS_CSSETUP_FALSE_EASTING ) ;
    lblProjcsFalseEasting.FocusControl := edtProjcsFalseEasting ;

    lblProjcsFalseNorthing := TLabel.Create( ScrollBox1 ) ;
    lblProjcsFalseNorthing.Parent := ScrollBox1 ;
    lblProjcsFalseNorthing.Anchors := anchors ;
    lblProjcsFalseNorthing.Top := 45 ;
    lblProjcsFalseNorthing.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsFalseNorthing, 170, 105 ) ;
    lblProjcsFalseNorthing.Caption := _rsrc( GIS_RS_CSSETUP_FALSE_NORTHING ) ;
    lblProjcsFalseNorthing.FocusControl := edtProjcsFalseNorthing ;

    lblProjcsStandardParallel_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsStandardParallel_1.Parent := ScrollBox1 ;
    lblProjcsStandardParallel_1.Anchors := anchors ;
    lblProjcsStandardParallel_1.Top := 85 ;
    lblProjcsStandardParallel_1.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsStandardParallel_1, 10, 129 ) ;
    lblProjcsStandardParallel_1.Caption := _rsrc( GIS_RS_CSSETUP_STANDARD_PARELLEL_1 ) ;
    lblProjcsStandardParallel_1.FocusControl := edtProjcsStandardParallel_1 ;

    lblProjcsStandardParallel_2 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsStandardParallel_2.Parent := ScrollBox1 ;
    lblProjcsStandardParallel_2.Anchors := anchors ;
    lblProjcsStandardParallel_2.Top := 85 ;
    lblProjcsStandardParallel_2.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsStandardParallel_2, 170, 129 ) ;
    lblProjcsStandardParallel_2.Caption := _rsrc( GIS_RS_CSSETUP_STANDARD_PARALLEL_2 ) ;
    lblProjcsStandardParallel_2.FocusControl := edtProjcsStandardParallel_2 ;

    lblProjcsPseudoStandardParallel_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsPseudoStandardParallel_1.Parent := ScrollBox1 ;
    lblProjcsPseudoStandardParallel_1.Anchors := anchors ;
    lblProjcsPseudoStandardParallel_1.Top := 125 ;
    lblProjcsPseudoStandardParallel_1.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsPseudoStandardParallel_1, 10, 164 ) ;
    lblProjcsPseudoStandardParallel_1.Caption := _rsrc( GIS_RS_CSSETUP_PSEUDO_STANDARD_PARALLEL_1 ) ;
    lblProjcsPseudoStandardParallel_1.FocusControl := edtProjcsPseudoStandardParallel_1 ;

    lblProjcsZone := TLabel.Create( ScrollBox1 ) ;
    lblProjcsZone.Parent := ScrollBox1 ;
    lblProjcsZone.Anchors := anchors ;
    lblProjcsZone.Top := 123 ;
    lblProjcsZone.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsZone, 170, 63 ) ;
    lblProjcsZone.Caption := _rsrc( GIS_RS_CSSETUP_ZONE ) ;
    lblProjcsZone.FocusControl := edtProjcsZone ;

    lblProjcsScaleFactor := TLabel.Create( ScrollBox1 ) ;
    lblProjcsScaleFactor.Parent := ScrollBox1 ;
    lblProjcsScaleFactor.Anchors := anchors ;
    lblProjcsScaleFactor.Top := 165 ;
    lblProjcsScaleFactor.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsScaleFactor, 10, 95 ) ;
    lblProjcsScaleFactor.Caption := _rsrc( GIS_RS_CSSETUP_SCALE_FACTOR ) ;
    lblProjcsScaleFactor.FocusControl := edtProjcsScaleFactor ;

    lblProjcsLongitudeOfCenter := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLongitudeOfCenter.Parent := ScrollBox1 ;
    lblProjcsLongitudeOfCenter.Anchors := anchors ;
    lblProjcsLongitudeOfCenter.Top := 205 ;
    lblProjcsLongitudeOfCenter.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLongitudeOfCenter, 10, 131 ) ;
    lblProjcsLongitudeOfCenter.Caption := _rsrc( GIS_RS_CSSETUP_LONGITUDE_OF_CENTER ) ;
    lblProjcsLongitudeOfCenter.FocusControl := edtProjcsLongitudeOfCenter ;

    lblProjcsLatitudeOfCenter := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfCenter.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfCenter.Anchors := anchors ;
    lblProjcsLatitudeOfCenter.Top := 205 ;
    lblProjcsLatitudeOfCenter.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfCenter, 170, 123 ) ;
    lblProjcsLatitudeOfCenter.Caption := _rsrc( GIS_RS_CSSETUP_LATITUDE_OF_CENTER ) ;
    lblProjcsLatitudeOfCenter.FocusControl := edtProjcsLatitudeOfCenter ;

    lblProjcsAzimuth := TLabel.Create( ScrollBox1 ) ;
    lblProjcsAzimuth.Parent := ScrollBox1 ;
    lblProjcsAzimuth.Anchors := anchors ;
    lblProjcsAzimuth.Top := 165 ;
    lblProjcsAzimuth.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsAzimuth, 170, 77 ) ;
    lblProjcsAzimuth.Caption := _rsrc( GIS_RS_CSSETUP_AZIMUTH ) ;
    lblProjcsAzimuth.FocusControl := edtProjcsAzimuth ;

    lblProjcsLongitudeOfPoint_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLongitudeOfPoint_1.Parent := ScrollBox1 ;
    lblProjcsLongitudeOfPoint_1.Anchors := anchors ;
    lblProjcsLongitudeOfPoint_1.Top := 245 ;
    lblProjcsLongitudeOfPoint_1.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLongitudeOfPoint_1, 10, 134 ) ;
    lblProjcsLongitudeOfPoint_1.Caption := _rsrc( GIS_RS_CSSETUP_LONGITUDE_OF_POINT_1 ) ;
    lblProjcsLongitudeOfPoint_1.FocusControl := edtProjcsLongitudeOfPoint_1 ;

    lblProjcsLatitudeOfPoint_1 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfPoint_1.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfPoint_1.Anchors := anchors ;
    lblProjcsLatitudeOfPoint_1.Top := 245 ;
    lblProjcsLatitudeOfPoint_1.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfPoint_1, 170, 126 ) ;
    lblProjcsLatitudeOfPoint_1.Caption := _rsrc( GIS_RS_CSSETUP_LATITUDE_OF_POINT_1 ) ;
    lblProjcsLatitudeOfPoint_1.FocusControl := edtProjcsLatitudeOfPoint_1 ;

    lblProjcsLongitudeOfPoint_2 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLongitudeOfPoint_2.Parent := ScrollBox1 ;
    lblProjcsLongitudeOfPoint_2.Anchors := anchors ;
    lblProjcsLongitudeOfPoint_2.Top := 286 ;
    lblProjcsLongitudeOfPoint_2.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLongitudeOfPoint_2, 10, 134 ) ;
    lblProjcsLongitudeOfPoint_2.Caption := _rsrc( GIS_RS_CSSETUP_LONGITUDE_OF_POINT_2 ) ;
    lblProjcsLongitudeOfPoint_2.FocusControl := edtProjcsLongitudeOfPoint_2 ;

    lblProjcsLatitudeOfPoint_2 := TLabel.Create( ScrollBox1 ) ;
    lblProjcsLatitudeOfPoint_2.Parent := ScrollBox1 ;
    lblProjcsLatitudeOfPoint_2.Anchors := anchors ;
    lblProjcsLatitudeOfPoint_2.Top := 285 ;
    lblProjcsLatitudeOfPoint_2.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsLatitudeOfPoint_2, 170, 126 ) ;
    lblProjcsLatitudeOfPoint_2.Caption := _rsrc( GIS_RS_CSSETUP_LATITUDE_OF_POINT_2 ) ;
    lblProjcsLatitudeOfPoint_2.FocusControl := edtProjcsLatitudeOfPoint_2 ;

    lblProjcsYScale := TLabel.Create( ScrollBox1 ) ;
    lblProjcsYScale.Parent := ScrollBox1 ;
    lblProjcsYScale.Anchors := anchors ;
    lblProjcsYScale.Top := 325 ;
    lblProjcsYScale.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsYScale, 170, 70 ) ;
    lblProjcsYScale.Caption := _rsrc( GIS_RS_CSSETUP_Y_SCALE ) ;
    lblProjcsYScale.FocusControl := edtProjcsYScale ;

    lblProjcsXScale := TLabel.Create( ScrollBox1 ) ;
    lblProjcsXScale.Parent := ScrollBox1 ;
    lblProjcsXScale.Anchors := anchors ;
    lblProjcsXScale.Top := 325 ;
    lblProjcsXScale.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsXScale, 10, 70 ) ;
    lblProjcsXScale.Caption := _rsrc( GIS_RS_CSSETUP_X_SCALE ) ;
    lblProjcsXScale.FocusControl := edtProjcsXScale ;

    lblProjcsXYPlaneRotation := TLabel.Create( ScrollBox1 ) ;
    lblProjcsXYPlaneRotation.Parent := ScrollBox1 ;
    lblProjcsXYPlaneRotation.Anchors := anchors ;
    lblProjcsXYPlaneRotation.Top := 365 ;
    lblProjcsXYPlaneRotation.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsXYPlaneRotation, 10, 118 ) ;
    lblProjcsXYPlaneRotation.Caption := _rsrc( GIS_RS_CSSETUP_XY_PLANE_ROTATION ) ;
    lblProjcsXYPlaneRotation.FocusControl := edtProjcsXYPlaneRotation ;

    lblProjcsCentralParallel := TLabel.Create( ScrollBox1 ) ;
    lblProjcsCentralParallel.Parent := ScrollBox1 ;
    lblProjcsCentralParallel.Anchors := anchors ;
    lblProjcsCentralParallel.Top := 405 ;
    lblProjcsCentralParallel.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsCentralParallel, 10, 108 ) ;
    lblProjcsCentralParallel.Caption := _rsrc( GIS_RS_CSSETUP_CENTRAL_PARALLEL ) ;
    lblProjcsCentralParallel.FocusControl := edtProjcsCentralParallel ;

    edtProjcsCentralMeridian := TEdit.Create( ScrollBox1 ) ;
    edtProjcsCentralMeridian.Parent := ScrollBox1 ;
    edtProjcsCentralMeridian.Anchors := anchors ;
    edtProjcsCentralMeridian.Top := 20 ;
    edtProjcsCentralMeridian.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsCentralMeridian, 10, 150 ) ;
    edtProjcsCentralMeridian.TabOrder := 0 ;
    edtProjcsCentralMeridian.Text := '0' ;
    edtProjcsCentralMeridian.OnChange := doProjcsVerify ;

    edtProjcsLatitudeOfOrigin := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfOrigin.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfOrigin.Anchors := anchors ;
    edtProjcsLatitudeOfOrigin.Top := 20 ;
    edtProjcsLatitudeOfOrigin.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfOrigin, 170, 150 ) ;
    edtProjcsLatitudeOfOrigin.TabOrder := 1 ;
    edtProjcsLatitudeOfOrigin.Text := '0' ;
    edtProjcsLatitudeOfOrigin.OnChange := doProjcsVerify ;

    edtProjcsFalseEasting := TEdit.Create( ScrollBox1 ) ;
    edtProjcsFalseEasting.Parent := ScrollBox1 ;
    edtProjcsFalseEasting.Anchors := anchors ;
    edtProjcsFalseEasting.Top := 60 ;
    edtProjcsFalseEasting.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsFalseEasting, 10, 150 ) ;
    edtProjcsFalseEasting.TabOrder := 2 ;
    edtProjcsFalseEasting.Text := '0' ;
    edtProjcsFalseEasting.OnChange := doProjcsVerify ;

    edtProjcsFalseNorthing := TEdit.Create( ScrollBox1 ) ;
    edtProjcsFalseNorthing.Parent := ScrollBox1 ;
    edtProjcsFalseNorthing.Anchors := anchors ;
    edtProjcsFalseNorthing.Top := 60 ;
    edtProjcsFalseNorthing.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsFalseNorthing, 170, 150 ) ;
    edtProjcsFalseNorthing.TabOrder := 3 ;
    edtProjcsFalseNorthing.Text := '0' ;
    edtProjcsFalseNorthing.OnChange := doProjcsVerify ;

    edtProjcsStandardParallel_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsStandardParallel_1.Parent := ScrollBox1 ;
    edtProjcsStandardParallel_1.Anchors := anchors ;
    edtProjcsStandardParallel_1.Top := 100 ;
    edtProjcsStandardParallel_1.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsStandardParallel_1, 10, 150 ) ;
    edtProjcsStandardParallel_1.TabOrder := 4 ;
    edtProjcsStandardParallel_1.Text := '0' ;
    edtProjcsStandardParallel_1.OnChange := doProjcsVerify ;

    edtProjcsStandardParallel_2 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsStandardParallel_2.Parent := ScrollBox1 ;
    edtProjcsStandardParallel_2.Anchors := anchors ;
    edtProjcsStandardParallel_2.Top := 100 ;
    edtProjcsStandardParallel_2.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsStandardParallel_2, 170, 150 ) ;
    edtProjcsStandardParallel_2.TabOrder := 5 ;
    edtProjcsStandardParallel_2.Text := '0' ;
    edtProjcsStandardParallel_2.OnChange := doProjcsVerify ;

    edtProjcsPseudoStandardParallel_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsPseudoStandardParallel_1.Parent := ScrollBox1 ;
    edtProjcsPseudoStandardParallel_1.Anchors := anchors ;
    edtProjcsPseudoStandardParallel_1.Top := 140 ;
    edtProjcsPseudoStandardParallel_1.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsPseudoStandardParallel_1, 10, 150 ) ;
    edtProjcsPseudoStandardParallel_1.TabOrder := 6 ;
    edtProjcsPseudoStandardParallel_1.Text := '0' ;
    edtProjcsPseudoStandardParallel_1.OnChange := doProjcsVerify ;

    edtProjcsZone := TEdit.Create( ScrollBox1 ) ;
    edtProjcsZone.Parent := ScrollBox1 ;
    edtProjcsZone.Anchors := anchors ;
    edtProjcsZone.Top := 140 ;
    edtProjcsZone.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsZone, 170, 150 ) ;
    edtProjcsZone.TabOrder := 7 ;
    edtProjcsZone.Text := '0' ;
    edtProjcsZone.OnChange := doProjcsVerify ;

    edtProjcsScaleFactor := TEdit.Create( ScrollBox1 ) ;
    edtProjcsScaleFactor.Parent := ScrollBox1 ;
    edtProjcsScaleFactor.Anchors := anchors ;
    edtProjcsScaleFactor.Top := 180 ;
    edtProjcsScaleFactor.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsScaleFactor, 10, 150 ) ;
    edtProjcsScaleFactor.TabOrder := 8 ;
    edtProjcsScaleFactor.Text := '0' ;
    edtProjcsScaleFactor.OnChange := doProjcsVerify ;

    edtProjcsAzimuth := TEdit.Create( ScrollBox1 ) ;
    edtProjcsAzimuth.Parent := ScrollBox1 ;
    edtProjcsAzimuth.Anchors := anchors ;
    edtProjcsAzimuth.Top := 180 ;
    edtProjcsAzimuth.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsAzimuth, 170, 150 ) ;
    edtProjcsAzimuth.TabOrder := 9 ;
    edtProjcsAzimuth.Text := '0' ;
    edtProjcsAzimuth.OnChange := doProjcsVerify ;

    edtProjcsLongitudeOfCenter := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLongitudeOfCenter.Parent := ScrollBox1 ;
    edtProjcsLongitudeOfCenter.Anchors := anchors ;
    edtProjcsLongitudeOfCenter.Top := 220 ;
    edtProjcsLongitudeOfCenter.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLongitudeOfCenter, 10, 150 ) ;
    edtProjcsLongitudeOfCenter.TabOrder := 10 ;
    edtProjcsLongitudeOfCenter.Text := '0' ;
    edtProjcsLongitudeOfCenter.OnChange := doProjcsVerify ;

    edtProjcsLatitudeOfCenter := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfCenter.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfCenter.Anchors := anchors ;
    edtProjcsLatitudeOfCenter.Top := 220 ;
    edtProjcsLatitudeOfCenter.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfCenter, 170, 150 ) ;
    edtProjcsLatitudeOfCenter.TabOrder := 11 ;
    edtProjcsLatitudeOfCenter.Text := '0' ;
    edtProjcsLatitudeOfCenter.OnChange := doProjcsVerify ;

    edtProjcsLongitudeOfPoint_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLongitudeOfPoint_1.Parent := ScrollBox1 ;
    edtProjcsLongitudeOfPoint_1.Anchors := anchors ;
    edtProjcsLongitudeOfPoint_1.Top := 260 ;
    edtProjcsLongitudeOfPoint_1.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLongitudeOfPoint_1, 10, 150 ) ;
    edtProjcsLongitudeOfPoint_1.TabOrder := 12 ;
    edtProjcsLongitudeOfPoint_1.Text := '0' ;
    edtProjcsLongitudeOfPoint_1.OnChange := doProjcsVerify ;

    edtProjcsLatitudeOfPoint_1 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfPoint_1.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfPoint_1.Anchors := anchors ;
    edtProjcsLatitudeOfPoint_1.Top := 260 ;
    edtProjcsLatitudeOfPoint_1.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfPoint_1, 170, 150 ) ;
    edtProjcsLatitudeOfPoint_1.TabOrder := 13 ;
    edtProjcsLatitudeOfPoint_1.Text := '0' ;
    edtProjcsLatitudeOfPoint_1.OnChange := doProjcsVerify ;

    edtProjcsLongitudeOfPoint_2 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLongitudeOfPoint_2.Parent := ScrollBox1 ;
    edtProjcsLongitudeOfPoint_2.Anchors := anchors ;
    edtProjcsLongitudeOfPoint_2.Top := 300 ;
    edtProjcsLongitudeOfPoint_2.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLongitudeOfPoint_2, 10, 150 ) ;
    edtProjcsLongitudeOfPoint_2.TabOrder := 14 ;
    edtProjcsLongitudeOfPoint_2.Text := '0' ;
    edtProjcsLongitudeOfPoint_2.OnChange := doProjcsVerify ;

    edtProjcsLatitudeOfPoint_2 := TEdit.Create( ScrollBox1 ) ;
    edtProjcsLatitudeOfPoint_2.Parent := ScrollBox1 ;
    edtProjcsLatitudeOfPoint_2.Anchors := anchors ;
    edtProjcsLatitudeOfPoint_2.Top := 300 ;
    edtProjcsLatitudeOfPoint_2.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsLatitudeOfPoint_2, 170, 150 ) ;
    edtProjcsLatitudeOfPoint_2.TabOrder := 15 ;
    edtProjcsLatitudeOfPoint_2.Text := '0' ;
    edtProjcsLatitudeOfPoint_2.OnChange := doProjcsVerify ;

    edtProjcsXScale := TEdit.Create( ScrollBox1 ) ;
    edtProjcsXScale.Parent := ScrollBox1 ;
    edtProjcsXScale.Anchors := anchors ;
    edtProjcsXScale.Top := 340 ;
    edtProjcsXScale.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsXScale, 10, 150 ) ;
    edtProjcsXScale.TabOrder := 16 ;
    edtProjcsXScale.Text := '0' ;
    edtProjcsXScale.OnChange := doProjcsVerify ;

    edtProjcsYScale := TEdit.Create( ScrollBox1 ) ;
    edtProjcsYScale.Parent := ScrollBox1 ;
    edtProjcsYScale.Anchors := anchors ;
    edtProjcsYScale.Top := 340 ;
    edtProjcsYScale.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsYScale, 170, 150 ) ;
    edtProjcsYScale.TabOrder := 17 ;
    edtProjcsYScale.Text := '0' ;
    edtProjcsYScale.OnChange := doProjcsVerify ;

    edtProjcsXYPlaneRotation := TEdit.Create( ScrollBox1 ) ;
    edtProjcsXYPlaneRotation.Parent := ScrollBox1 ;
    edtProjcsXYPlaneRotation.Anchors := anchors ;
    edtProjcsXYPlaneRotation.Top := 380 ;
    edtProjcsXYPlaneRotation.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsXYPlaneRotation, 10, 150 ) ;
    edtProjcsXYPlaneRotation.TabOrder := 18 ;
    edtProjcsXYPlaneRotation.Text := '0' ;
    edtProjcsXYPlaneRotation.OnChange := doProjcsVerify ;

    edtProjcsCentralParallel := TEdit.Create( ScrollBox1 ) ;
    edtProjcsCentralParallel.Parent := ScrollBox1 ;
    edtProjcsCentralParallel.Anchors := anchors ;
    edtProjcsCentralParallel.Top := 420 ;
    edtProjcsCentralParallel.Height := 19 ;
    PlaceControl( BiDiMode, nil, edtProjcsCentralParallel, 10, 150 ) ;
    edtProjcsCentralParallel.TabOrder := 19 ;
    edtProjcsCentralParallel.Text := '0' ;
    edtProjcsCentralParallel.OnChange := doProjcsVerify ;

    lblProjcsUnit := TLabel.Create( tabProjcs ) ;
    lblProjcsUnit.Parent := tabProjcs ;
    lblProjcsUnit.Top := 233 ;
    lblProjcsUnit.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblProjcsUnit, 3, 58 ) ;
    lblProjcsUnit.Caption := _rsrc( GIS_RS_CSSETUP_UNIT ) ;
    lblProjcsUnit.FocusControl := cmbProjcsUnit ;

    cmbProjcsUnit := TComboBox.Create( tabProjcs ) ;
    cmbProjcsUnit.Parent := tabProjcs ;
    cmbProjcsUnit.Top := 250 ;
    cmbProjcsUnit.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbProjcsUnit, 3, 325 ) ;
    cmbProjcsUnit.TabOrder := 3 ;
    cmbProjcsUnit.Text := 'cmbProjcsUnit' ;
    cmbProjcsUnit.OnChange := doProjcsChange ;

    lblGeocs := TLabel.Create( tabProjcs ) ;
    lblGeocs.Parent := tabProjcs ;
    lblGeocs.Top := 283 ;
    lblGeocs.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblGeocs, 3, 39 ) ;
    lblGeocs.Caption := _rsrc( GIS_RS_CSSETUP_GEOGCS ) ;
    lblGeocs.FocusControl := cmbGeocs ;

    cmbGeocs := TComboBox.Create( tabProjcs ) ;
    cmbGeocs.Parent := tabProjcs ;
    cmbGeocs.Top := 300 ;
    cmbGeocs.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbGeocs, 3, 325 ) ;
    cmbGeocs.TabOrder := 4 ;
    cmbGeocs.Text := 'cmbGeocs' ;
    cmbGeocs.OnChange := doProjcsChange ;

    btnGeocs := TSpeedButton.Create( tabProjcs ) ;
    btnGeocs.Parent := tabProjcs ;
    btnGeocs.Top := 300 ;
    btnGeocs.Height := 21 ;
    PlaceControl( BiDiMode, cmbGeocs, btnGeocs, 6, 24 ) ;
    btnGeocs.Caption := '...' ;
    btnGeocs.OnClick := btnGeocsClick ;

    tabWkt := TTabSheet.Create( pagMain ) ;
    tabWkt.Parent := pagMain ;
    tabWkt.Caption := _rsrc( GIS_RS_CSSETUP_PROJCS_WKT ) ;
    tabWkt.ImageIndex := 2 ;
    tabWkt.PageControl := pagMain ;
    tabWkt.OnShow := tabWktShow ;

    chkPretty := TCheckBox.Create( tabWkt ) ;
    chkPretty.Parent := tabWkt ;
    chkPretty.Top := 8 ;
    chkPretty.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkPretty, 3, 182 ) ;
    chkPretty.Caption := _rsrc( GIS_RS_CSSETUP_PRETTY_WKT ) ;
    chkPretty.TabOrder := 0 ;
    chkPretty.OnClick := chkPrettyClick ;

    btnSaveWkt := TButton.Create( tabWkt ) ;
    btnSaveWkt.Parent := tabWkt ;
    btnSaveWkt.Top := 5 ;
    btnSaveWkt.Height := 21 ;
    PlaceControl( BiDiMode, nil, btnSaveWkt, -3, 75 ) ;
    btnSaveWkt.Caption := _rsrc( GIS_RS_CSSETUP_EXPORT ) ;
    btnSaveWkt.TabOrder := 2 ;
    btnSaveWkt.OnClick := btnSaveWktClick ;

    btnOpenWkt := TButton.Create( tabWkt ) ;
    btnOpenWkt.Parent := tabWkt ;
    btnOpenWkt.Top := 5 ;
    btnOpenWkt.Height := 21 ;
    PlaceControl( BiDiMode, btnSaveWkt, btnOpenWkt, -3, 75 ) ;
    btnOpenWkt.Caption := _rsrc( GIS_RS_CSSETUP_IMPORT ) ;
    btnOpenWkt.TabOrder := 1 ;
    btnOpenWkt.OnClick := btnOpenWktClick ;

    memWkt := TMemo.Create( tabWkt ) ;
    memWkt.Parent := tabWkt ;
    memWkt.Top := 34 ;
    memWkt.Height := 286 ;
    PlaceControl( BiDiMode, nil, memWkt, 3, -1 ) ;
    memWkt.ScrollBars := ssVertical ;
    memWkt.TabOrder := 3 ;
    memWkt.OnChange := memWktChange ;

    btnHelp.TabOrder := 1 ;
    btnCancel.TabOrder := 3 ;
    btnOK.TabOrder := 2 ;

    dlgOpenWkt := TOpenDialog.Create( Self ) ;
    dlgOpenWkt.DefaultExt := 'prj' ;
    dlgOpenWkt.Filter := _rsrc( GIS_RS_CSSETUP_WKT_FILTER ) ;

    dlgSaveWkt := TSaveDialog.Create( Self ) ;
    dlgSaveWkt.DefaultExt := 'prj' ;
    dlgSaveWkt.Filter := _rsrc( GIS_RS_CSSETUP_WKT_FILTER ) ;
    dlgSaveWkt.Options := [ofOverwritePrompt, ofHideReadOnly, ofExtensionDifferent, ofEnableSizing] ;
  end;

  procedure TGIS_ControlCSSystemSetup.showForm ;
  begin
    afterPPIChanged ;
  end ;

  procedure TGIS_ControlCSSystemSetup.afterPPIChanged ;
  begin
    btnGeocs.Top := cmbGeocs.Top ;
    btnGeocs.Height := cmbGeocs.Height ;
    PlaceControl( BiDiMode, cmbGeocs, btnGeocs, ppiFix(6), btnGeocs.Height + ppiFix(3) ) ;
  end ;

  function TGIS_ControlCSSystemSetup.Execute(
    const _cs     : TGIS_CSCoordinateSystem ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  begin
    pagMain.ActivePageIndex := 0 ;

    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    FCS := _cs ;
    setupCs ;

    Result := ShowModal ;
  end ;

  function TGIS_ControlCSSystemSetup.Execute(
    const _cs : TGIS_CSCoordinateSystem
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _cs, hlp ) ;
  end;

{==================================== END =====================================}
end.

