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
  Legend component. Grid wizard.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlLegendGridWiz ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlLegendGridWiz"'}
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
    TatukGIS.NDK.WinForms,
    TatukGIS.RTL ;
  {$ENDIF}

  {$IFDEF DCC}
    System.Classes,
    System.SysUtils,
    System.Math,
    System.Variants,
    Data.DB,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoClassification,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoUtils,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoStatistics,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoSqlQuery,
    Lider.CG.GIS.PVL.GeoControlStatistics,
    Lider.CG.GIS.PVL.GeoValueValidatorHelper,
    Lider.CG.GIS.PVL.GeoPvl,
    Lider.CG.GIS.PVL.GeoPvlWidgets,
    Lider.CG.GIS.PVL.GeoPvlForms;
  {$ENDIF}

  {$IFDEF JAVA}
    java.util,
    tatukgis.jdk.*,
    tatukgis.rtl ;
  {$ENDIF}

type

  /// <summary>
  ///   Visual form for managing vector rendering wizard.
  /// </summary>
  TGIS_PvlControlLegendGridWiz = class( TGIS_PvlModalWizard )
    private
      // simple/advanced classification choice
      pnlChoose           : TGIS_PvlPage              ;
      rdbChooseSimple     : TGIS_PvlRadioButton       ;
      rdbChooseAdvanced   : TGIS_PvlRadioButton       ;
      grpChoose           : String                    ;

      // simple -> unique/continuous choice
      pnlAnalyze          : TGIS_PvlPage              ;
      rdbAnalyzeUnique    : TGIS_PvlRadioButton       ;
      lblAnalyzeUnique    : TGIS_PvlLabel             ;
      pnlAnalyzeUnique    : TGIS_PvlPanel             ;
      memAnalyzeUnique    : TGIS_PvlListBox           ;
      rdbAnalyzeContinous : TGIS_PvlRadioButton       ;
      grpAnalyze          : String                    ;

      lblAnalyzeMin       : TGIS_PvlLabel             ;
      edtAnalyzeMin       : TGIS_PvlEdit              ;
      lblAnalyzeMax       : TGIS_PvlLabel             ;
      edtAnalyzeMax       : TGIS_PvlEdit              ;
      chkAnalyzeAvg       : TGIS_PvlCheckBox          ;
      edtAnalyzeAvg       : TGIS_PvlEdit              ;

      // simple -> colors/ramps choice
      pnlStyleParams      : TGIS_PvlPage              ;
      rdbStyleColorRange  : TGIS_PvlRadioButton       ;
      lblStyleColorStart  : TGIS_PvlLabel             ;
      cmbStyleColorStart  : TGIS_PvlColorComboBox     ;
      lblStyleColorMid    : TGIS_PvlLabel             ;
      cmbStyleColorMid    : TGIS_PvlColorComboBox     ;
      lblStyleColorEnd    : TGIS_PvlLabel             ;
      cmbStyleColorEnd    : TGIS_PvlColorComboBox     ;
      chkStyleColorRampAdd: TGIS_PvlCheckBox          ;
      chkStyleColorHSL    : TGIS_PvlCheckBox          ;
      rdbStyleColorRamp   : TGIS_PvlRadioButton       ;
      cmbStyleRampList    : TGIS_PvlColorRampWidget   ;
      grpStyle            : String                    ;

      // simple -> min/mid/max/interval values
      pnlRamp             : TGIS_PvlPage              ;
      lblRampStep         : TGIS_PvlLabel             ;
      edtRampStep         : TGIS_PvlEdit              ;
      lblRampLegend       : TGIS_PvlLabel             ;
      edtRampLegend       : TGIS_PvlEdit              ;

      // advanced
      pnlFormulaAdvanced  : TGIS_PvlPage              ;
      lblMethodAdvanced   : TGIS_PvlLabel             ;
      cmbMethodAdvanced   : TGIS_PvlComboBox          ;
      lblClassesAdvanced  : TGIS_PvlLabel             ;
      cmbClassesAdvanced  : TGIS_PvlComboBox          ;
      lblIntervalAdvanced : TGIS_PvlLabel             ;
      cmbIntervalAdvanced : TGIS_PvlComboEdit         ;
      lblBandAdvanced     : TGIS_PvlLabel             ;
      cmbBandAdvanced     : TGIS_PvlComboBox          ;

    private
      objLayer            : TGIS_LayerPixel           ;
      objParams           : TGIS_ParamsSectionPixel   ;
      vvedtMin            : IGIS_ValueValidator       ;
      vvedtStep           : IGIS_ValueValidator       ;
      vvedtAvg            : IGIS_ValueValidator       ;
      vvedtLegend         : IGIS_ValueValidator       ;
      vvedtMax            : IGIS_ValueValidator       ;
      iMode               : Integer                   ;
      bLimitExceeded      : Boolean                   ;
      afterExecute        : Boolean                   ;

    public
      /// <inheritdoc/>
      procedure BtnOKClick              ( _sender: TObject ) ; override;

    private
      procedure createAdvancedFormula   ;
      procedure createChoose            ;
      procedure createAnalyze           ;
      procedure createRamp              ;
      procedure createColors            ;
      procedure createOrder             ;

    private
      procedure onPageChange            ( _sender : TObject ) ;
      procedure onChooseExit            ( _sender : TObject ) ;
      procedure onAnalyzeEnter          ( _sender : TObject ) ;
      procedure onFormulaAdvancedEnter  ( _sender : TObject ) ;
      procedure onFormulaAdvancedExit   ( _sender : TObject ) ;
      procedure onAnalyzeExit           ( _sender : TObject ) ;
      procedure onStyleEnter            ( _sender : TObject ) ;
      procedure onRdbAnalyzeChange      ( _sender : TObject ) ;
      procedure onRdbStyleChange        ( _sender : TObject ) ;
      procedure onMethodAdvancedChange  ( _sender : TObject ) ;

    private
      // Because of how FMX mobile works in order to keep
      // code multiplatform we have to use helping procedures
      // so we can emulate proper behaviour on mobile devices.
      procedure doCheckUniquesWork ( band           : String ;
                                     unique_params  : TGIS_VariantArray
                                   ) ;
      procedure doClassifyWork     ( classifier     : TGIS_ClassificationPixel ;
                                     prm            : TGIS_ParamsList
                                   ) ;
    private
      function  check_uniques      : Boolean ;
      procedure check_method       ;
      procedure check_style        ;
      function  doClassify         : Boolean ;
      function  getRamp            ( const _numClasses : Integer
                                   ) : TGIS_ColorMapArray ;
    protected
      /// <inheritdodc/>
      procedure doDestroy          ; override;
    public
      /// <inheritdoc/>
      procedure DoInitForm        ; override;

      /// <inheritdoc/>
      procedure DoInitControls    ; override;

    public
      /// <summary>
      ///   Execute dialog on a given layer and parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be analyzed
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled
      /// </param>
      /// <param name="_proc">
      ///   action to be taken upon execute
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute       ( const _layer  : TGIS_LayerPixel         ;
                               const _params : TGIS_ParamsSectionPixel ;
                               const _onhelp : TGIS_HelpEvent          ;
                               const _proc   : TGIS_Proc
                             ) : TGIS_PvlModalResult ; overload;

      /// <summary>
      ///   Execute dialog on a given layer and parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be analyzed
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered
      /// </param>
      /// <param name="_proc">
      ///   action to be taken upon execute
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute       ( const _layer  : TGIS_LayerPixel         ;
                               const _params : TGIS_ParamsSectionPixel ;
                               const _proc   : TGIS_Proc
                             ) : TGIS_PvlModalResult ; overload;

      /// <summary>
      ///   Execute dialog on a given layer and parameters.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be analyzed
      /// </param>
      /// <param name="_params">
      ///   parameters to be altered
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute       ( const _layer  : TGIS_LayerPixel         ;
                               const _params : TGIS_ParamsSectionPixel
                             ) : TGIS_PvlModalResult ; overload;
   end ;

var
  /// <summary>
  ///   Global value of type TGIS_PvlControlLegendGridWiz.
  /// </summary>
  GIS_ControlLegendGridWiz : TGIS_PvlControlLegendGridWiz ;

//##############################################################################
implementation

const
  // Maximum number of unique values.
  UNIQUE_LIMIT_GRID = 256 ;

//==============================================================================
// Private events & methods
//==============================================================================

procedure TGIS_PvlControlLegendGridWiz.doCheckUniquesWork(
  band : String ;
  unique_params : TGIS_VariantArray
) ;
var
  i             : Integer ;
  unique_count  : Integer ;
  unique_string : String ;
  stats_unique  : TGIS_StatisticsItemVariantList ;
begin
    stats_unique := objLayer.Statistics.Get( band ).Unique ;
    stats_unique.Params := unique_params ;

    unique_count := stats_unique.Values.Count ;
    if ( unique_count < UNIQUE_LIMIT_GRID ) then begin
      bLimitExceeded := False ;
      unique_string := GIS_RS_LEGEND_WIZARD_VALUES ;
    end
    else begin
      bLimitExceeded := True ;
      unique_string := GIS_RS_LEGEND_WIZARD_VALLIMIT ;
    end ;

    memAnalyzeUnique.BeginUpdate ;
    try
      memAnalyzeUnique.ItemsAdd(
        Format( unique_string, [unique_count] )
      ) ;

      for i := 0 to stats_unique.Values.Count-1 do
        memAnalyzeUnique.ItemsAdd( VarToString( stats_unique.Values[i] ) ) ;
    finally
      memAnalyzeUnique.EndUpdate ;
    end ;
end;

procedure TGIS_PvlControlLegendGridWiz.doClassifyWork(
  classifier : TGIS_ClassificationPixel ;
  prm        : TGIS_ParamsList
) ;
var
  sp : TGIS_ParamsSectionPixel ;
begin
  try
    classifier.EstimateNumClasses ;

    // set color ramp
    if rdbAnalyzeUnique.Checked then
      classifier.ColorRamp := getRamp( classifier.NumClasses )
    else if rdbStyleColorRamp.Checked then
      classifier.ColorRamp := getRamp( 0 )
    else
      classifier.ColorRamp := nil ;

    classifier.Classify( prm ) ;


    sp := TGIS_ParamsSectionPixel(prm[0]) ;
    sp.Pixel.GridBand    := objLayer.Params.Pixel.GridBand    ;
    sp.Pixel.GridNoValue := objLayer.Params.Pixel.GridNoValue ;
    sp.Pixel.GridShadow  := objLayer.Params.Pixel.GridShadow ;
    sp.FalseZAsText      := objLayer.Params.FalseZAsText ;
    sp.ScaleZ            := objLayer.Params.ScaleZ ;
    sp.NormalizedZ       := objLayer.Params.NormalizedZ ;

    objParams.Assign( prm[0] );
  finally
    FreeObject( classifier ) ;
    FreeObject( prm ) ;
  end;
end;

function TGIS_PvlControlLegendGridWiz.check_uniques : Boolean ;
var
  band          : String ;
  unique_params : TGIS_VariantArray ;
  stats_funs    : TGIS_StatisticalFunctions ;
  frm_stats     : TGIS_ControlStatistics ;
  proc          : TGIS_Proc ;
begin
  Result := False ;

  if ( memAnalyzeUnique.ItemsCount <= 0 ) then begin
    // determine and add uniques
    stats_funs := TGIS_StatisticalFunctions.EmptyStatistics ;
    stats_funs.Unique := True ;

    // min & max will be needed later
    stats_funs.Min := True ;
    stats_funs.Max := True ;
    band := IntToStr( objLayer.Params.Pixel.GridBand ) ;
    objLayer.Statistics.Add( band, stats_funs ) ;
    SetLength( unique_params, 1 ) ;
    unique_params[0] := UNIQUE_LIMIT_GRID ;

    if objLayer.MustCalculateStatistics then begin
      frm_stats := TGIS_ControlStatistics.Create( Self ) ;
      proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
        procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then begin
              objLayer.Statistics.ResetModified ;
              rdbAnalyzeUnique.Checked := False ;
              rdbAnalyzeContinous.Checked := True ;

              exit ;
            end else begin
              doCheckUniquesWork( band, unique_params ) ;
            end ;
        end
      {$IFDEF OXYGENE}){$ENDIF};

      frm_stats.Execute( objLayer, OnHelpEvent, proc ) ;
    end else begin
      doCheckUniquesWork( band, unique_params ) ;
    end;
  end ;

  Result := True ;
end;

procedure TGIS_PvlControlLegendGridWiz.check_method ;
var
  &method : String ;
  tag     : Double ;
begin
  &method := cmbMethodAdvanced.Item[cmbMethodAdvanced.ItemIndex] ;

  // firstly reset controls if not STDEV
  if not ( ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
            ( &method = GIS_RS_CLASSIFY_METHOD_SDC ) ) then
  begin
    if Pos( 'STDEV', cmbIntervalAdvanced.Text ) > StringFirst then begin
      cmbIntervalAdvanced.ItemsClear ;
      cmbIntervalAdvanced.Text := DotFloatToStrPrec(
        cmbIntervalAdvanced.Tag / 100.0, 2
      ) ;
    end ;
  end ;

  // set method-specific control properties
  // DefinedInterval
  if &method = GIS_RS_CLASSIFY_METHOD_DI then begin
    cmbIntervalAdvanced.Enabled := True ;
    cmbClassesAdvanced.Enabled := False ;
  end
  // Quartile
  else if &method = GIS_RS_CLASSIFY_METHOD_QR then begin
    cmbIntervalAdvanced.Enabled := False ;
    cmbClassesAdvanced.Enabled := False ;
  end
  // StandardDeviation(s)
  else if ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
          ( &method = GIS_RS_CLASSIFY_METHOD_SDC ) then
  begin
    // use tag to store last interval value
    if TryStrToFloat( cmbIntervalAdvanced.Text, tag ) then
      cmbIntervalAdvanced.Tag := RoundS( tag * 100 ) ;

    if cmbIntervalAdvanced.ItemsCount = 0 then begin
      cmbIntervalAdvanced.ItemsAdd( '1 STDEV' ) ;
      cmbIntervalAdvanced.ItemsAdd( '1/2 STDEV' ) ;
      cmbIntervalAdvanced.ItemsAdd( '1/3 STDEV' ) ;
      cmbIntervalAdvanced.ItemsAdd( '1/4 STDEV' ) ;
      cmbIntervalAdvanced.ItemIndex := 0 ;
    end ;
    cmbIntervalAdvanced.Enabled := True ;
    cmbClassesAdvanced.Enabled := False ;
  end
  else begin
    cmbIntervalAdvanced.Enabled := False ;
    cmbClassesAdvanced.Enabled := True ;
  end ;
end ;

procedure TGIS_PvlControlLegendGridWiz.check_style ;
begin
  if rdbStyleColorRange.Checked then begin
    lblStyleColorStart.Enabled     := True ;
    lblStyleColorEnd.Enabled       := True ;
    cmbStyleColorStart.Enabled     := True ;
    cmbStyleColorEnd.Enabled       := True ;
    cmbStyleRampList.Enabled       := False ;

    if iMode = 0 then begin
      lblStyleColorMid.Enabled     := chkAnalyzeAvg.Checked ;
      cmbStyleColorMid.Enabled     := chkAnalyzeAvg.Checked;
      chkStyleColorRampAdd.Enabled := True ;
      chkStyleColorHSL.Enabled     := True ;
    end
    else begin
      lblStyleColorMid.Enabled     := False ;
      cmbStyleColorMid.Enabled     := False ;
      chkStyleColorRampAdd.Enabled := False ;
      chkStyleColorHSL.Enabled     := False ;
    end ;
  end
  else if rdbStyleColorRamp.Checked then begin

    lblStyleColorStart.Enabled     := False ;
    lblStyleColorMid.Enabled       := False ;
    lblStyleColorEnd.Enabled       := False ;
    cmbStyleColorStart.Enabled     := False ;
    cmbStyleColorMid.Enabled       := False ;
    cmbStyleColorEnd.Enabled       := False ;
    cmbStyleRampList.Enabled       := True ;
    chkStyleColorRampAdd.Enabled   := False ;
    chkStyleColorHSL.Enabled       := False ;
  end;

  if ( rdbStyleColorRamp.Checked ) or ( iMode = 1 ) then begin
    pnlStyleParams.Next := nil ;
    BtnOK.Visible := True ;
    BtnNext.Visible := False ;
  end
  else begin
    pnlStyleParams.Next := pnlRamp ;
    BtnOK.Visible := False ;
    BtnNext.Visible := True ;
  end;
end ;

  procedure TGIS_PvlControlLegendGridWiz.onPageChange(
    _sender : TObject
  ) ;
  var
    state : Integer ;
  begin
    if afterExecute then begin

      if iMode = 0 then begin
        if rdbStyleColorRange.Checked then begin
          if chkStyleColorRampAdd.Checked then
            state := 1
          else
            state := 0 ;
        end
        else
          state := 2 ;
        case state of
          0, 1 : begin
            lblAnalyzeMin.Enabled := True ;
    //??        chkAnalyzeAvg.Checked := True ;
            chkAnalyzeAvg.Enabled := True ;
            lblAnalyzeMax.Enabled := True ;
            lblRampStep.Enabled   := True ;
            lblRampLegend.Enabled := True ;
            edtAnalyzeMin.Enabled := True ;
            edtRampStep.Enabled   := True ;
            edtAnalyzeAvg.Enabled := True ;
            edtRampLegend.Enabled := True ;
            edtAnalyzeMax.Enabled := True ;
          end ;
          2 : begin
            lblAnalyzeMin.Enabled := True ;
   //??         chkAnalyzeAvg.Checked := False ;
            chkAnalyzeAvg.Enabled := False ;
            lblAnalyzeMax.Enabled := True ;
            lblRampStep.Enabled   := False ;
            lblRampLegend.Enabled := False ;
            edtAnalyzeMin.Enabled := True ;
            edtRampStep.Enabled   := False ;
            edtAnalyzeAvg.Enabled := False ;
            edtRampLegend.Enabled := False ;
            edtAnalyzeMax.Enabled := True ;
          end ;
        end ;
      end ;
    end;
  end ;

  function TGIS_PvlControlLegendGridWiz.doClassify : Boolean ;
  var
    &method    : String ;
    cmin, cmax : TGIS_Color ;
    prm        : TGIS_ParamsList ;
    sec        : TGIS_ParamsRender ;
    classifier : TGIS_ClassificationPixel ;
    frm_stats  : TGIS_ControlStatistics ;
    proc       : TGIS_Proc ;
  begin
    Result := False ;

    prm := TGIS_ParamsList.Create ;
    prm.Assign( objLayer.ParamsList ) ;

    classifier := TGIS_ClassificationPixel.Create( objLayer ) ;
    // set properties
    // simple classification / unique
    if rdbAnalyzeUnique.Checked then begin
      classifier.Band := IntToStr( objParams.Pixel.GridBand ) ;
      classifier.Method := TGIS_ClassificationMethod.Unique ;
    end
    // advanced classification
    else begin
      sec := TGIS_ParamsRender.Create ;
      try
        sec.StartColorAsText := cmbStyleColorStart.Value ;
        cmin := sec.StartColor ;
        sec.EndColorAsText   := cmbStyleColorEnd.Value ;
        cmax := sec.EndColor ;
      finally
        FreeObject( sec ) ;
      end ;

      classifier.Band       := cmbBandAdvanced.Item[ cmbBandAdvanced.ItemIndex ] ;
      classifier.NumClasses := cmbClassesAdvanced.ItemIndex + 1 ;
      classifier.StartColor := cmin ;
      classifier.EndColor   := cmax ;
      classifier.ShowLegend := True ;

      // set method
      &method := cmbMethodAdvanced.Item[cmbMethodAdvanced.ItemIndex] ;
      if &method = GIS_RS_CLASSIFY_METHOD_DI then begin
        classifier.Method   := TGIS_ClassificationMethod.DefinedInterval ;
        classifier.Interval := DotStrToFloat( cmbIntervalAdvanced.Text ) ;
      end
      else if &method = GIS_RS_CLASSIFY_METHOD_EI then
        classifier.Method := TGIS_ClassificationMethod.EqualInterval
      else if &method = GIS_RS_CLASSIFY_METHOD_GI then
        classifier.Method := TGIS_ClassificationMethod.GeometricalInterval
      else if &method = GIS_RS_CLASSIFY_METHOD_KM then
        classifier.Method := TGIS_ClassificationMethod.KMeans
      else if &method = GIS_RS_CLASSIFY_METHOD_NB then
        classifier.Method := TGIS_ClassificationMethod.NaturalBreaks
      else if &method = GIS_RS_CLASSIFY_METHOD_QN then
        classifier.Method := TGIS_ClassificationMethod.Quantile
      else if &method = GIS_RS_CLASSIFY_METHOD_QR then
        classifier.Method := TGIS_ClassificationMethod.Quartile
      else if &method = GIS_RS_CLASSIFY_METHOD_SD then
        classifier.Method := TGIS_ClassificationMethod.StandardDeviation
      else if &method = GIS_RS_CLASSIFY_METHOD_SDC then
        classifier.Method := TGIS_ClassificationMethod.StandardDeviationWithCentral
      else
        classifier.Method := TGIS_ClassificationMethod.NaturalBreaks ;

      // set interval for Standard Deviation
      if ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
         ( &method = GIS_RS_CLASSIFY_METHOD_SDC ) then begin
        case cmbIntervalAdvanced.ItemIndex of
            0 : classifier.Interval := 1.0 ;
            1 : classifier.Interval := 1.0 / 2 ;
            2 : classifier.Interval := 1.0 / 3 ;
            3 : classifier.Interval := 1.0 / 4 ;
        else    classifier.Interval := 1.0 ;
        end ;
      end ;
    end ;

    // run classifier
    if classifier.MustCalculateStatistics then begin
      frm_stats := TGIS_ControlStatistics.Create( Self ) ;
      proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
        procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit
          else
            doClassifyWork( classifier, prm ) ;
        end
      {$IFDEF OXYGENE}){$ENDIF};

      frm_stats.Execute( objLayer, OnHelpEvent, proc ) ;
    end else begin
      doClassifyWork( classifier, prm ) ;
    end;

    Result := True ;
  end ;

  function TGIS_PvlControlLegendGridWiz.getRamp(
    const _numClasses : Integer
  ) : TGIS_ColorMapArray ;
  var
    ci       : Integer ;
    &method  : String ;
    interval : Double ;
  begin
    // default colormap
    ci := _numClasses ;

    if rdbChooseAdvanced.Checked then begin
      &method := cmbMethodAdvanced.Item[cmbMethodAdvanced.ItemIndex] ;

      // for StandardDeviation use default colormap
      if ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
         ( &method = GIS_RS_CLASSIFY_METHOD_SDC ) then
        ci := 0
      // Quartile always returns 4 class breaks
      else if ( &method = GIS_RS_CLASSIFY_METHOD_QR ) then
        ci := 4
      // for DefinedInterval it's easy to calculate number of classes
      else if ( &method = GIS_RS_CLASSIFY_METHOD_DI ) and
              TryStrToFloat( cmbIntervalAdvanced.Text, interval ) then
        ci := CeilS( ( objLayer.MaxHeight - objLayer.MinHeight ) / interval )
      else
        ci := cmbClassesAdvanced.ItemIndex + 1 ;
    end ;

    Result := cmbStyleRampList.Value( ci ) ;
  end ;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_PvlControlLegendGridWiz.btnOKClick(
    _sender: TObject
  );
  var
    prm              : TGIS_ParamsMarker ;
    cmin, cmax, cmid : TGIS_Color ;
    cs               : TGIS_ColorInterpolationMode ;
  begin
    // simple classification
    if iMode = 0 then begin

      // unique classification
      if rdbAnalyzeUnique.Checked then begin
        if not doClassify then
          exit ;
        objParams.Pixel.Antialias := False ;
        objParams.Pixel.GridShadow := False ;
      end
      // continuous classification / colors
      else if rdbStyleColorRange.Checked then begin
        prm := TGIS_ParamsMarker.Create ;
        try
          prm.ColorAsText := cmbStyleColorStart.Value ;
          cmin := prm.Color ;
          prm.ColorAsText := cmbStyleColorMid.Value ;
          cmid := prm.Color ;
          prm.ColorAsText := cmbStyleColorEnd.Value ;
          cmax := prm.Color ;
        finally
          FreeObject( prm ) ;
        end ;

        if chkStyleColorHSL.Checked then
          cs := TGIS_ColorInterpolationMode.HSL
        else
          cs := TGIS_ColorInterpolationMode.RGB ;

        objLayer.GenerateRamp( cmin,
                               cmid,
                               cmax,
                               vvedtMin.Value,
                               vvedtAvg.Value,
                               vvedtMax.Value,
                               chkAnalyzeAvg.Checked,
                               vvedtStep.Value,
                               vvedtLegend.Value,
                               objParams,
                               not chkStyleColorRampAdd.Checked,
                               cs
                              )
      end
      // continuous classification / color ramp
      else begin
        if cmbStyleRampList.Discrete then
          cs := TGIS_ColorInterpolationMode.None
        else
          cs := TGIS_ColorInterpolationMode.RGB ;

        objLayer.GenerateRampEx( vvedtMin.Value,
                                 vvedtMax.Value,
                                 getRamp( 0 ),
                                 objParams,
                                 cs
                                ) ;
      end ;
    end
    // advanced classification
    else begin
      if not doClassify then
       exit ;
      // the classification result can also be visualized
      // with the SmoothColors option enabled
      //? check if it works
      objParams.Pixel.GridSmoothColors := not cmbStyleRampList.Discrete ;
    end ;
    ModalResult := TGIS_PvlModalResult.OK ;

  end;

  procedure TGIS_PvlControlLegendGridWiz.createChoose ;
  begin
    pnlChoose := Self.Pages.AddPage ;

    grpChoose := 'grpChoose' ;

    rdbChooseSimple := TGIS_PvlRadioButton.Create( Context ) ;
    rdbChooseSimple.Group := grpChoose ;
    rdbChooseSimple.Place( 200, 0, nil, RoundS( Pages.Width / 2 - 100 ), nil, RoundS( Pages.Height / 2 - 2 * Context.HMargin - BtnHelp.Height ) ) ;
    rdbChooseSimple.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_SIMPLE ) ;
    rdbChooseSimple.Checked := True ;

    pnlChoose.AddComponent( rdbChooseSimple, 'Choose_RadioButton_Simple' ) ;

    rdbChooseAdvanced :=  TGIS_PvlRadioButton.Create( Context ) ;
    rdbChooseAdvanced.Group := grpChoose ;
    rdbChooseAdvanced.Place( 200, 0, nil, RoundS( Pages.Width / 2 - 100 ), rdbChooseSimple, 0 ) ;
    rdbChooseAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_ADVANCED ) ;

    pnlChoose.AddComponent( rdbChooseAdvanced, 'Choose_RadioButton_Advance' ) ;

    {$IFDEF DCC}
      pnlChoose.OnExit := onChooseExit ;
    {$ELSE}
      pnlChoose.OnExit := @onChooseExit ;
    {$ENDIF}
  end;

  procedure TGIS_PvlControlLegendGridWiz.createAdvancedFormula ;
  var
    COLUMN_WIDTH : Integer ;
    i            : Integer ;
  begin
    COLUMN_WIDTH := RoundS( Self.ClientWidth / 4 ) - ( 2 * Context.HMargin ) ;

    pnlFormulaAdvanced := Self.Pages.AddPage ;

    lblMethodAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblMethodAdvanced.Place( COLUMN_WIDTH * 2, 0, nil, Context.HMargin, nil, 50 ) ;
    lblMethodAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CLASSIFY_METHOD ) ;

    pnlFormulaAdvanced.AddComponent( lblMethodAdvanced, 'Advanced_Label_Method' ) ;

    cmbMethodAdvanced := TGIS_PvlComboBox.Create( Context ) ;
    cmbMethodAdvanced.Place( lblMethodAdvanced.Width, 0, nil, Context.HMargin, lblMethodAdvanced, 0 ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_DI ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_EI ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_GI ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_NB ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_KM ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_QN ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_QR ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_SD ) ) ;
    cmbMethodAdvanced.ItemsAdd( _rsrc( GIS_RS_CLASSIFY_METHOD_SDC ) ) ;
    cmbMethodAdvanced.ItemIndex := 3 ;

    {$IFDEF DCC}
      cmbMethodAdvanced.OnChange := onMethodAdvancedChange ;
    {$ELSE}
      cmbMethodAdvanced.OnChange := @onMethodAdvancedChange ;
    {$ENDIF}

    pnlFormulaAdvanced.AddComponent( cmbMethodAdvanced, 'Advanced_ComboBox_Method' ) ;

    lblClassesAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblClassesAdvanced.Place( COLUMN_WIDTH, 0, lblMethodAdvanced, Context.HMargin, nil, lblMethodAdvanced.Top ) ;
    lblClassesAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CLASSIFY_CLASSES ) ;

    pnlFormulaAdvanced.AddComponent( lblClassesAdvanced, 'Advanced_Label_Classes' ) ;

    cmbClassesAdvanced := TGIS_PvlComboBox.Create( Context ) ;
    cmbClassesAdvanced.Place( lblClassesAdvanced.Width, 0, cmbMethodAdvanced, Context.HMargin, lblClassesAdvanced, 0 ) ;

    for i := 1 to 9 do
      cmbClassesAdvanced.ItemsAdd( IntToStr( i ) ) ;
    cmbClassesAdvanced.ItemIndex := 4 ;

    pnlFormulaAdvanced.AddComponent( cmbClassesAdvanced, 'Advanced_ComboBox_Classes' ) ;

    lblIntervalAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblIntervalAdvanced.Place( COLUMN_WIDTH, 0, lblClassesAdvanced, Context.HMargin, nil, lblMethodAdvanced.Top ) ;
    lblIntervalAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CLASSIFY_INTERVAL ) ;

    pnlFormulaAdvanced.AddComponent( lblIntervalAdvanced, 'Advanced_Label_Interval' ) ;

    cmbIntervalAdvanced := TGIS_PvlComboEdit.Create( Context ) ;
    cmbIntervalAdvanced.Place( lblIntervalAdvanced.Width, 0, cmbClassesAdvanced, Context.HMargin, lblIntervalAdvanced, 0 ) ;

    pnlFormulaAdvanced.AddComponent( cmbIntervalAdvanced, 'Advanced_ComboEdit_Interval' ) ;

    lblBandAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblBandAdvanced.Place( COLUMN_WIDTH, 0, nil, Context.HMargin, cmbMethodAdvanced, 0 ) ;
    lblBandAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_PRM_BAND ) ;

    pnlFormulaAdvanced.AddComponent( lblBandAdvanced, 'Advanced_Label_Band' ) ;

    cmbBandAdvanced := TGIS_PvlComboBox.Create( Context ) ;
    cmbBandAdvanced.Place( lblBandAdvanced.Width, 0, nil, Context.HMargin, lblBandAdvanced, 0 ) ;

    pnlFormulaAdvanced.AddComponent( cmbBandAdvanced, 'Advanced_ComboBox_Band' ) ;

    {$IFDEF DCC}
      pnlFormulaAdvanced.OnEnter := onFormulaAdvancedEnter ;
    {$ELSE}
      pnlFormulaAdvanced.OnEnter := @onFormulaAdvancedEnter ;
    {$ENDIF}

    {$IFDEF DCC}
      pnlFormulaAdvanced.OnExit := onFormulaAdvancedExit ;
    {$ELSE}
      pnlFormulaAdvanced.OnExit := @onFormulaAdvancedExit ;
    {$ENDIF}

  end;

  procedure TGIS_PvlControlLegendGridWiz.createAnalyze ;
  begin
    pnlAnalyze := Self.Pages.AddPage ;

    grpAnalyze := 'grpAnalyze' ;

    // Left side

    rdbAnalyzeUnique := TGIS_PvlRadioButton.Create( Context ) ;
    rdbAnalyzeUnique.Group := grpAnalyze ;
    rdbAnalyzeUnique.Place( RoundS( ( pnlAnalyze.Width -  Context.HMargin ) / 2 - Context.HSpace ), 0, nil, Context.HMargin, nil, Context.VMargin ) ;
    rdbAnalyzeUnique.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBUNIQUE ) ;

    {$IFDEF DCC}
      rdbAnalyzeUnique.OnClick := onRdbAnalyzeChange ;
    {$ELSE}
      rdbAnalyzeUnique.OnClick := @onRdbAnalyzeChange ;
    {$ENDIF}

    pnlAnalyze.AddComponent( rdbAnalyzeUnique, 'Analyze_RadioButton_Unique' ) ;

    lblAnalyzeUnique := TGIS_PvlLabel.Create( Context ) ;
    lblAnalyzeUnique.Place( rdbAnalyzeUnique.Width, 0, nil, Context.HMargin, rdbAnalyzeUnique, 0 ) ;
    lblAnalyzeUnique.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLUNIQUE ) ;

    pnlAnalyze.AddComponent( lblAnalyzeUnique, 'Analyze_Label_Unique' ) ;

    pnlAnalyzeUnique := TGIS_PvlPanel.Create( Context ) ;

    pnlAnalyze.AddComponent( pnlAnalyzeUnique, 'Analyze_Panel_Unique' ) ;

    memAnalyzeUnique := TGIS_PvlListBox.Create( Context ) ;

    // Right side

    rdbAnalyzeContinous := TGIS_PvlRadioButton.Create( Context ) ;
    rdbAnalyzeContinous.Group := grpAnalyze ;
    rdbAnalyzeContinous.Place( rdbAnalyzeUnique.Width, 0, rdbAnalyzeUnique, Context.HSpace, nil, Context.VMargin ) ;
    rdbAnalyzeContinous.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBCONTINOUS ) ;

    {$IFDEF DCC}
      rdbAnalyzeContinous.OnClick := onRdbAnalyzeChange ;
    {$ELSE}
      rdbAnalyzeContinous.OnClick := @onRdbAnalyzeChange ;
    {$ENDIF}

    pnlAnalyze.AddComponent( rdbAnalyzeContinous, 'Analyze_RadioButton_Continous' ) ;

    lblAnalyzeMin := TGIS_PvlLabel.Create( Context ) ;
    lblAnalyzeMin.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, rdbAnalyzeContinous, 0 ) ;
    lblAnalyzeMin.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLMIN ) ;

    pnlAnalyze.AddComponent( lblAnalyzeMin, 'Analyze_Label_Minimum' ) ;

    edtAnalyzeMin := TGIS_PvlEdit.Create( Context ) ;
    edtAnalyzeMin.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, lblAnalyzeMin, 0 ) ;

    pnlAnalyze.AddComponent( edtAnalyzeMin, 'Analyze_Edit_Minimum' ) ;

    lblAnalyzeMax := TGIS_PvlLabel.Create( Context ) ;
    lblAnalyzeMax.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, edtAnalyzeMin, 0 ) ;
    lblAnalyzeMax.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLMAX ) ;

    pnlAnalyze.AddComponent( lblAnalyzeMax, 'Analyze_Label_Maximum' ) ;

    edtAnalyzeMax := TGIS_PvlEdit.Create( Context ) ;
    edtAnalyzeMax.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, lblAnalyzeMax, 0 ) ;

    pnlAnalyze.AddComponent( edtAnalyzeMax, 'Analyze_Edit_Maximum' ) ;

    chkAnalyzeAvg := TGIS_PvlCheckBox.Create( Context ) ;
    chkAnalyzeAvg.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, edtAnalyzeMax, 0 ) ;
    chkAnalyzeAvg.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CHKAVG ) ;
    chkAnalyzeAvg.Checked := True ;

    {$IFDEF DCC}
      chkAnalyzeAvg.OnClick := onRdbAnalyzeChange ;
    {$ELSE}
      chkAnalyzeAvg.OnClick := @onRdbAnalyzeChange ;
    {$ENDIF}

    pnlAnalyze.AddComponent( chkAnalyzeAvg, 'Analyze_CheckBox_Average' ) ;

    edtAnalyzeAvg := TGIS_PvlEdit.Create( Context ) ;
    edtAnalyzeAvg.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, chkAnalyzeAvg, 0 ) ;

    pnlAnalyze.AddComponent( edtAnalyzeAvg, 'Analyze_Edit_Average' ) ;

    vvedtMax := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMax ) ;
    vvedtMax.Precision := 15 ;
    vvedtMax.MinVal := -GIS_MAX_SINGLE ;
    vvedtMax.MaxVal := GIS_MAX_SINGLE ;

    vvedtMin := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMin ) ;
    vvedtMin.Precision := 15 ;
    vvedtMin.MinVal := -GIS_MAX_SINGLE ;
    vvedtMin.MaxVal := GIS_MAX_SINGLE ;

    vvedtAvg := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeAvg ) ;
    vvedtAvg.Precision := 15 ;
    vvedtAvg.MinVal := -GIS_MAX_SINGLE ;
    vvedtAvg.MaxVal := GIS_MAX_SINGLE ;

    {$IFDEF DCC}
      pnlAnalyze.OnEnter := onAnalyzeEnter ;
    {$ELSE}
      pnlAnalyze.OnEnter := @onAnalyzeEnter ;
    {$ENDIF}

    {$IFDEF DCC}
      pnlAnalyze.OnExit := onAnalyzeExit ;
    {$ELSE}
      pnlAnalyze.OnExit := @onAnalyzeExit ;
    {$ENDIF}

    pnlAnalyzeUnique.Place( rdbAnalyzeUnique.Width - Context.VMargin, edtAnalyzeAvg.Top + edtAnalyzeAvg.Height - ( lblAnalyzeUnique.Top + lblAnalyzeUnique.Height + Context.VSpace ), nil, Context.HMargin, lblAnalyzeUnique, 0 ) ;
    pnlAnalyzeUnique.Scrollable := True ;

    pnlAnalyzeUnique.AddComponent( memAnalyzeUnique ) ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.createRamp ;
  begin
    pnlRamp := Self.Pages.AddPage ;

    lblRampStep := TGIS_PvlLabel.Create( Context ) ;
    lblRampStep.Place( RoundS( pnlRamp.Width - 2 * Context.HMargin ), 0, nil, Context.HMargin, nil, Context.VMargin ) ;
    lblRampStep.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLSTEP ) ;

    pnlRamp.AddComponent( lblRampStep, 'Ramp_Label_Step' ) ;

    edtRampStep := TGIS_PvlEdit.Create( Context ) ;
    edtRampStep.Place( lblRampStep.Width, 0, nil, Context.HMargin, lblRampStep, 0 ) ;

    pnlRamp.AddComponent( edtRampStep, 'Ramp_Edit_Step' ) ;

    lblRampLegend := TGIS_PvlLabel.Create( Context ) ;
    lblRampLegend.Place( lblRampStep.Width, 0, nil, Context.HMargin, edtRampStep, Context.VSpace * 2 ) ;
    lblRampLegend.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLLEGSTEP ) ;

    pnlRamp.AddComponent( lblRampLegend, 'Ramp_Label_Legend' ) ;

    edtRampLegend := TGIS_PvlEdit.Create( Context ) ;
    edtRampLegend.Place( lblRampStep.Width, 0, nil, Context.HMargin, lblRampLegend, 0 ) ;

    pnlRamp.AddComponent( edtRampLegend, 'Ramp_Edit_Legend' ) ;


    vvedtStep := TGIS_ValueValidatorEditHelper.Create( edtRampStep ) ;
    vvedtStep.Precision := 15 ;
    vvedtStep.MinVal := -GIS_MAX_SINGLE ;
    vvedtStep.MaxVal := GIS_MAX_SINGLE ;

    vvedtLegend := TGIS_ValueValidatorEditHelper.Create( edtRampLegend ) ;
    vvedtLegend.Precision := 15 ;
    vvedtLegend.MinVal := -GIS_MAX_SINGLE ;
    vvedtLegend.MaxVal := GIS_MAX_SINGLE ;

//    {$IFDEF DCC}
//      pnlRender.OnEnter := onRenderEnter ;
//    {$ELSE}
//      pnlRender.OnEnter := @onRenderEnter ;
//    {$ENDIF}
  end;

  procedure TGIS_PvlControlLegendGridWiz.createColors ;
  var
    COLUMN_WIDTH  : Integer ;
  begin
    COLUMN_WIDTH := RoundS( Self.ClientWidth / 3 ) - ( 2 * Context.HMargin ) - RoundS( ( Context.HMargin + 3 * Context.HSpace ) / 3 ) ;

    pnlStyleParams := Self.Pages.AddPage ;

    grpStyle := 'grpStyle' ;

    rdbStyleColorRange := TGIS_PvlRadioButton.Create( Context ) ;
    rdbStyleColorRange.Group := grpStyle ;
    rdbStyleColorRange.Place( 3 * COLUMN_WIDTH , 0, nil, Context.HMargin, nil, Context.VMargin ) ;
    rdbStyleColorRange.Caption := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
    rdbStyleColorRange.Checked := True ;

    {$IFDEF DCC}
      rdbStyleColorRange.OnClick := onRdbStyleChange ;
    {$ELSE}
      rdbStyleColorRange.OnClick := @onRdbStyleChange ;
    {$ENDIF}

    pnlStyleParams.AddComponent( rdbStyleColorRange, 'StyleParams_RadioButton_Range' ) ;

    lblStyleColorStart := TGIS_PvlLabel.Create( Context ) ;
    lblStyleColorStart.Place( COLUMN_WIDTH, 0, nil, Context.HMargin + 3 * Context.HSpace, rdbStyleColorRange, 0 ) ;
    lblStyleColorStart.Caption := _rsrcna( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;

    pnlStyleParams.AddComponent( lblStyleColorStart, 'StyleParams_Label_ColorStart' ) ;

    cmbStyleColorStart := TGIS_PvlColorComboBox.Create( Context ) ;
    cmbStyleColorStart.Place( COLUMN_WIDTH, 24, nil, Context.HMargin + 3 * Context.HSpace, lblStyleColorStart, 0 ) ;
    cmbStyleColorStart.Fill( False, False ) ;

    pnlStyleParams.AddComponent( cmbStyleColorStart, 'StyleParams_ComboBox_ColorStart' ) ;

    lblStyleColorMid := TGIS_PvlLabel.Create( Context ) ;
    lblStyleColorMid.Place( COLUMN_WIDTH, 0, lblStyleColorStart, Context.HSpace, rdbStyleColorRange, 0 ) ;
    lblStyleColorMid.Caption := _rsrcna( GIS_RS_LEGEND_PRM_MIDDLECOLOR ) ;

    pnlStyleParams.AddComponent( lblStyleColorMid, 'StyleParams_Label_ColorMid' ) ;

    cmbStyleColorMid := TGIS_PvlColorComboBox.Create( Context ) ;
    cmbStyleColorMid.Place( COLUMN_WIDTH, 24, cmbStyleColorStart, Context.HSpace, lblStyleColorMid, 0 ) ;
    cmbStyleColorMid.Fill( False, False ) ;

    pnlStyleParams.AddComponent( cmbStyleColorMid, 'StyleParams_ComboBox_ColorMid' ) ;

    lblStyleColorEnd := TGIS_PvlLabel.Create( Context ) ;
    lblStyleColorEnd.Place( COLUMN_WIDTH, 0, lblStyleColorMid, Context.HSpace, rdbStyleColorRange, 0 ) ;
    lblStyleColorEnd.Caption := _rsrcna( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;

    pnlStyleParams.AddComponent( lblStyleColorEnd, 'StyleParams_Label_ColorEnd' ) ;

    cmbStyleColorEnd := TGIS_PvlColorComboBox.Create( Context ) ;
    cmbStyleColorEnd.Place( COLUMN_WIDTH, 24, cmbStyleColorMid, Context.HSpace, lblStyleColorEnd, 0 ) ;
    cmbStyleColorEnd.Fill( False, False ) ;

    pnlStyleParams.AddComponent( cmbStyleColorEnd, 'StyleParams_ComboBox_ColorEnd' ) ;

    chkStyleColorRampAdd := TGIS_PvlCheckBox.Create( Context ) ;
    chkStyleColorRampAdd.Place( COLUMN_WIDTH * 2 + Context.HSpace, 0, nil, Context.HMargin + 3 * Context.HSpace, cmbStyleColorStart, 0 ) ;
    chkStyleColorRampAdd.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RAMP_ADD ) ;

    pnlStyleParams.AddComponent( chkStyleColorRampAdd, 'StyleParams_CheckBox_RampAdd' ) ;

    chkStyleColorHSL := TGIS_PvlCheckBox.Create( Context ) ;
    chkStyleColorHSL.Place( COLUMN_WIDTH, 0, chkStyleColorRampAdd, Context.HSpace, cmbStyleColorEnd, 0 ) ;
    chkStyleColorHSL.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_USEHSL ) ;
    chkStyleColorHSL.Checked := True ;

    pnlStyleParams.AddComponent( chkStyleColorHSL, 'StyleParams_CheckBox_HSL' ) ;

    rdbStyleColorRamp := TGIS_PvlRadioButton.Create( Context ) ;
    rdbStyleColorRamp.Group := grpStyle ;
    rdbStyleColorRamp.Place( rdbStyleColorRange.Width, 0, nil, Context.HMargin, chkStyleColorHSL, 2 * Context.VSpace ) ;
    rdbStyleColorRamp.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RAMPS ) ;

    {$IFDEF DCC}
      rdbStyleColorRamp.OnClick := onRdbStyleChange ;
    {$ELSE}
      rdbStyleColorRamp.OnClick := @onRdbStyleChange ;
    {$ENDIF}

    pnlStyleParams.AddComponent( rdbStyleColorRamp, 'StyleParams_RadioButton_Ramp' ) ;

    cmbStyleRampList := TGIS_PvlColorRampWidget.Create( Context ) ;
    cmbStyleRampList.TwoColumns := True ;
    cmbStyleRampList.ComboHeight := 24 ;
    cmbStyleRampList.Place( COLUMN_WIDTH * 3 + 2 * Context.HSpace, 0, nil, Context.HMargin + 3 * Context.HSpace, rdbStyleColorRamp, 0 ) ;
    cmbStyleRampList.Fill ;


    pnlStyleParams.AddComponent( cmbStyleRampList, 'StyleParams_RampWidget_Ramp' ) ;

    {$IFDEF DCC}
      pnlStyleParams.OnEnter := onStyleEnter ;
    {$ELSE}
      pnlStyleParams.OnEnter := @onStyleEnter ;
    {$ENDIF}
  end;

  procedure TGIS_PvlControlLegendGridWiz.createOrder ;
  begin
    pnlChoose.Next := pnlAnalyze ;
    pnlAnalyze.Next := pnlStyleParams ;
    pnlFormulaAdvanced.Next := pnlStyleParams ;
    pnlStyleParams.Next := pnlRamp ;

    pnlStyleParams.Previous:= pnlAnalyze ;
    pnlRamp.Previous := pnlStyleParams ;
    pnlAnalyze.Previous := pnlChoose ;
    pnlFormulaAdvanced.Previous := pnlChoose ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onChooseExit(
    _sender: TObject
  ) ;
  begin
    if _sender = Self.BtnNext then begin
      if rdbChooseSimple.Checked then
        iMode := 0
      else if rdbChooseAdvanced.Checked then
        iMode := 1
      else
        iMode := -1 ;
    end;

    case iMode of
      0 : begin
        pnlChoose.Next := pnlAnalyze ;
        rdbAnalyzeContinous.Checked := True ;
      end;
      1 : begin
        pnlChoose.Next := pnlFormulaAdvanced ;
      end;
      -1: pnlChoose.Next := nil ;
    end;

    pnlChoose.Previous := nil ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onAnalyzeEnter(
    _sender: TObject
  ) ;
  var
    bl : Boolean ;
  begin
    bl := rdbAnalyzeUnique.Enabled ;
    lblAnalyzeUnique.Visible := bl ;
    memAnalyzeUnique.Visible := bl ;

    bl := rdbAnalyzeUnique.Checked ;

    // calculate statistics on demand if needed
    if bl and not check_uniques then
      exit ;

    lblAnalyzeUnique.Enabled := bl ;
    memAnalyzeUnique.Enabled := bl ;

    bl := rdbAnalyzeContinous.Enabled ;
    lblAnalyzeMin.Visible := bl ;
    edtAnalyzeMin.Visible := bl ;
    lblAnalyzeMax.Visible := bl ;
    edtAnalyzeMax.Visible := bl ;
    chkAnalyzeAvg.Visible := bl ;
    edtAnalyzeAvg.Visible := bl ;

    bl := rdbAnalyzeContinous.Checked ;
    lblAnalyzeMin.Enabled := bl ;
    edtAnalyzeMin.Enabled := bl ;
    lblAnalyzeMax.Enabled := bl ;
    edtAnalyzeMax.Enabled := bl ;
    chkAnalyzeAvg.Enabled := bl ;
    edtAnalyzeAvg.Enabled := bl and chkAnalyzeAvg.Checked ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onFormulaAdvancedEnter(
    _sender: TObject
  ) ;
  begin
    check_method ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onFormulaAdvancedExit(
    _sender: TObject
  ) ;
  var
    &method : String ;
  begin
    cmbStyleRampList.Lock ;
    // set ramp stuff to default
    cmbStyleRampList.Reverse := False ;
    cmbStyleRampList.ShowAll := False ;

    cmbStyleColorStart.Value    := 'ARGB:FFEFF3FF' ;
    cmbStyleColorMid.Value      := 'ARGB:FF6BAED6' ;
    cmbStyleColorEnd.Value      := 'ARGB:FF08519C' ;

    cmbStyleRampList.Discrete := False ;  // continuous by default

    &method := cmbMethodAdvanced.Item[cmbMethodAdvanced.ItemIndex] ;
    if ( &method = GIS_RS_CLASSIFY_METHOD_QR ) or
        ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
        ( &method = GIS_RS_CLASSIFY_METHOD_SDC ) then
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Diverging]
    else
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
    cmbStyleRampList.ItemIndex := 0 ;

    pnlStyleParams.Next := nil ;
    pnlStyleParams.Previous := pnlFormulaAdvanced ;

    cmbStyleRampList.Unlock;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onAnalyzeExit(
    _sender: TObject
  ) ;
  var
    optionDialog  : TGIS_PvlOptionDialog  ;
  begin
    cmbStyleRampList.Lock;
    // set ramp stuff to default
    cmbStyleRampList.Reverse := False ;
    cmbStyleRampList.ShowAll := False ;

    // too many unique values warning
    if rdbAnalyzeUnique.Checked and bLimitExceeded then begin
      if _sender = BtnNext then begin
        optionDialog := TGIS_PvlOptionDialog.Create( Context ) ;
        optionDialog.Text := Format( _rsrc( GIS_RS_LEGEND_TOO_MANY_UNIQUE ), [UNIQUE_LIMIT_GRID] ) ;

        if optionDialog.Execute = TGIS_PvlModalResult.No then begin
          Pages.Abort;
          Pages.Activate( pnlAnalyze.PageNumber, Self ) ;
          exit ;
        end;
      end;
    end;

    cmbStyleColorStart.Value := 'ARGB:FF0000FF' ;
    cmbStyleColorMid.Value   := 'ARGB:FF0FF000' ;
    cmbStyleColorEnd.Value   := 'ARGB:FFFF0000' ;

    if rdbAnalyzeUnique.Checked then begin
      rdbStyleColorRange.Enabled := False ;
      rdbStyleColorRamp.Checked := True ;
      cmbStyleRampList.Discrete := True ;  // set discrete for Unique
    end
    else begin
      rdbStyleColorRange.Enabled := True ;
      rdbStyleColorRange.Checked := True ;
      rdbStyleColorRamp.Enabled := True ;

      cmbStyleRampList.Discrete := False ;  // continuous by default
    end ;

    if not rdbAnalyzeUnique.Checked then
      pnlStyleParams.Next := pnlRamp
    else
      pnlStyleParams.Next := nil ;

    pnlStyleParams.Previous := pnlAnalyze ;


    if rdbAnalyzeUnique.Checked then begin
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
      cmbStyleRampList.Unlock;
      cmbStyleRampList.ItemIndex := cmbStyleRampList.ItemCount - 4 ;
    end else begin
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
      cmbStyleRampList.Unlock;
      cmbStyleRampList.ItemIndex := 0 ;
    end;

  end;

  procedure TGIS_PvlControlLegendGridWiz.onStyleEnter(
    _sender: TObject
  ) ;
  begin
    check_style ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onRdbAnalyzeChange(
    _sender: TObject
  ) ;
  var
    bl : Boolean ;
  begin
    bl := rdbAnalyzeUnique.Enabled ;
    lblAnalyzeUnique.Visible := bl ;
    memAnalyzeUnique.Visible := bl ;

    bl := rdbAnalyzeUnique.Checked ;

    // calculate statistics on demand if needed
    if bl and not check_uniques then
      exit ;

    lblAnalyzeUnique.Enabled := bl ;
    memAnalyzeUnique.Enabled := bl ;

    bl := rdbAnalyzeContinous.Enabled ;
    lblAnalyzeMin.Visible := bl ;
    edtAnalyzeMin.Visible := bl ;
    lblAnalyzeMax.Visible := bl ;
    edtAnalyzeMax.Visible := bl ;
    chkAnalyzeAvg.Visible := bl ;
    edtAnalyzeAvg.Visible := bl ;

    bl := rdbAnalyzeContinous.Checked ;
    lblAnalyzeMin.Enabled := bl ;
    edtAnalyzeMin.Enabled := bl ;
    lblAnalyzeMax.Enabled := bl ;
    edtAnalyzeMax.Enabled := bl ;
    chkAnalyzeAvg.Enabled := bl ;
    edtAnalyzeAvg.Enabled := bl and chkAnalyzeAvg.Checked ;
  end ;

  procedure TGIS_PvlControlLegendGridWiz.onRdbStyleChange(
    _sender: TObject
  ) ;
  begin
    check_style ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.onMethodAdvancedChange(
    _sender: TObject
  ) ;
  begin
    check_method ;
  end;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_PvlControlLegendGridWiz.doDestroy ;
  begin
    inherited ;
  end;

  procedure TGIS_PvlControlLegendGridWiz.DoInitForm ;
  begin
    Self.Caption      := _rsrc( GIS_RS_LEGEND_WIZARD_GRID ) ;
    Self.ClientHeight := 270 ;
    Self.ClientWidth  := 420 ;
    Self.Name         := 'TGIS_PvlControlLegendGridWiz' ;
  end ;

  procedure TGIS_PvlControlLegendGridWiz.DoInitControls ;
  begin
    // Create pages
    createChoose ;
    createAdvancedFormula ;
    createAnalyze ;
    createColors ;
    createRamp ;

    // Give pages order
    createOrder ;

    {$IFDEF DCC}
      Pages.OnPageChange := onPageChange ;
    {$ELSE}
      Pages.OnPageChange := @onPageChange ;
    {$ENDIF}

    BtnOK.Visible := False ;
  end ;

  function TGIS_PvlControlLegendGridWiz.Execute(
    const _layer  : TGIS_LayerPixel         ;
    const _params : TGIS_ParamsSectionPixel ;
    const _onhelp : TGIS_HelpEvent          ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    i : Integer ;
  begin
    Pages.Activate( 0, Self ) ;

    assert( assigned( _layer ) ) ;
    objLayer  := _layer  ;

    assert( assigned( _params ) ) ;
    objParams := _params ;

    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    vvedtMin.Value    := _layer.MinHeight ;
    vvedtAvg.Value    := ( _layer.MaxHeight - _layer.MinHeight ) / 2 +
                         _layer.MinHeight ;
    vvedtMax.Value    := _layer.MaxHeight ;
    vvedtStep.Value   := ( _layer.MaxHeight - _layer.MinHeight ) / 100 ;
    vvedtLegend.Value := ( _layer.MaxHeight - _layer.MinHeight ) / 10 ;
    if vvedtStep.Value = 0 then
      vvedtStep.Value := vvedtLegend.Value ;

    for i := 1 to _layer.BandsCount do
      cmbBandAdvanced.ItemsAdd( IntToStr( i ) ) ;
    cmbBandAdvanced.ItemIndex := 0 ;

    cmbIntervalAdvanced.Text := DotFloatToStr( vvedtLegend.Value * 2 ) ;

    afterExecute := True ;

    Result := ShowModal( _proc, assigned( _proc ) ) ; ;
  end ;

  function TGIS_PvlControlLegendGridWiz.Execute(
    const _layer  : TGIS_LayerPixel         ;
    const _params : TGIS_ParamsSectionPixel ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, _params, hlp, _proc ) ;
  end;

  function TGIS_PvlControlLegendGridWiz.Execute(
    const _layer  : TGIS_LayerPixel         ;
    const _params : TGIS_ParamsSectionPixel
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, _params, hlp, nil ) ;
  end;

//==================================== END =====================================
end.

