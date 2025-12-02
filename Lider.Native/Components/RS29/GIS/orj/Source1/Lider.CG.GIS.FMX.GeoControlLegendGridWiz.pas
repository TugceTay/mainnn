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

unit Lider.CG.GIS.FMX.GeoControlLegendGridWiz ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendGridWiz"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.Actions,
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Variants,
  FMX.ActnList,
  FMX.ComboEdit,
  FMX.Controls,
  FMX.Dialogs,
  FMX.Edit,
  FMX.Forms,
  FMX.Graphics,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Memo,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.Types,

  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.FMX.GeoControlColor,
  Lider.CG.GIS.FMX.GeoValueValidatorHelper,
  FMX.Controls.Presentation,
  Lider.CG.GIS.FMX.GeoModalForm ;

type

  /// <summary>
  ///   Visual form for managing vector rendering wizard.
  /// </summary>
  TGIS_ControlLegendGridWiz = class( TGIS_ModalForm )
    // buttons
    btnNextPage: TButton;
    btnPreviousPage: TButton;

    // simple/advanced classification choice
    pnlChoose: TLayout;
    rdbChooseSimple: TRadioButton;
    rdbChooseAdvanced: TRadioButton;

    // simple -> unique/continuous choice
    pnlAnalyze: TLayout;
    rdbAnalyzeUnique: TRadioButton;
    lblAnalyzeUnique: TLabel;
    memAnalyzeUnique: TMemo;
    rdbAnalyzeContinous: TRadioButton;

    lblAnalyzeMin: TLabel;
    edtAnalyzeMin: TEdit;
    chkAnalyzeMid: TCheckBox;
    edtAnalyzeMid: TEdit;
    lblAnalyzeMax: TLabel;
    edtAnalyzeMax: TEdit;

    // simple -> colors/ramps choice
    pnlStyleParams: TLayout;
    rdbStyleColorRange: TRadioButton;
    lblStyleColorStart: TLabel;
    cmbStyleColorStart: TGIS_ColorComboBox;
    lblStyleColorMid: TLabel;
    cmbStyleColorMid: TGIS_ColorComboBox;
    lblStyleColorEnd: TLabel;
    cmbStyleColorEnd: TGIS_ColorComboBox;
    rdbStyleColorRamp: TRadioButton;
    chkStyleColorRampAdd: TCheckBox;
    cmbStyleRampList: TGIS_ColorRampComboBox;
    chkStyleColorHSL: TCheckBox;
    chkStyleDiscrete: TCheckBox;
    chkShowAllRamps: TCheckBox;
    
    // simple -> min/mid/max/interval values
    pnlRamp: TLayout;
    lblRampStep: TLabel;
    edtRampStep: TEdit;
    edtRampLegend: TEdit;
    lblRampLegend: TLabel;

    // advanced
    pnlFormulaAdvanced: TLayout;
    lblMethodAdvanced: TLabel;
    cmbMethodAdvanced: TComboBox;
    lblClassesAdvanced: TLabel;
    cmbClassesAdvanced: TComboBox;
    lblIntervalAdvanced: TLabel;
    cmbIntervalAdvanced: TComboEdit;
    lblBandAdvanced: TLabel;
    cmbBandAdvanced: TComboBox;
    chkReverseRamp: TCheckBox;
    procedure actListUpdate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actNextPageExecute(Sender: TObject);
    procedure actPreviousPageExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject); override;
    procedure btnNextPageClick(Sender: TObject);
    procedure btnPreviousPageClick(Sender: TObject);
    procedure actDiscreteClick(Sender: TObject);
    procedure actReverseClick(Sender: TObject);
    procedure actShowAllClick(Sender: TObject);
  private
    objLayer         : TGIS_LayerPixel  ;
    objParams        : TGIS_ParamsSectionPixel ;
    vvedtMin         : IGIS_ValueValidator ;
    vvedtStep        : IGIS_ValueValidator ;
    vvedtMid         : IGIS_ValueValidator ;
    vvedtLegend      : IGIS_ValueValidator ;
    vvedtMax         : IGIS_ValueValidator ;
    iPage            : Integer ;
    iMode            : Integer ;
    bLimitExceeded   : Boolean ;
    lastColorSchemas : TGIS_ColorSchemas ;
  private
    procedure updateControlsState( const _mode  : Integer ) ;
    function  doCustomColor      ( _sender : TObject ;
                                   _value  : String
                                 ) : String ;
    function  doClassify         : Boolean ;
    function  getRamp            ( const _numClasses : Integer
                                 ) : TGIS_ColorMapArray ;
  protected
    /// <inheritdoc/>
    procedure initForm     ; override;

    /// <inheritdoc/>
    procedure initControls ; override;
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
    /// <param name="_resProc">
    ///   parameters to be altered
    /// </param>
    procedure Execute      ( const _layer   : TGIS_LayerPixel         ;
                             const _params  : TGIS_ParamsSectionPixel ;
                             const _onhelp  : TGIS_HelpEvent ;
                             const _resProc : TProc<TModalResult>
                           ) ; overload;

    /// <summary>
    ///   Execute dialog on a given layer and parameters.
    /// </summary>
    /// <param name="_layer">
    ///   layer to be analyzed
    /// </param>
    /// <param name="_params">
    ///   parameters to be altered
    /// </param>
    /// <param name="_resProc">
    ///   parameters to be altered
    /// </param>
    procedure Execute      ( const _layer   : TGIS_LayerPixel         ;
                             const _params  : TGIS_ParamsSectionPixel ;
                             const _resProc : TProc<TModalResult>
                           ) ; overload;
  end ;

var
  /// <summary>
  ///   Global value of type TGIS_ControlLegendGridWiz.
  /// </summary>
  GIS_ControlLegendGridWiz : TGIS_ControlLegendGridWiz ;


//##############################################################################
implementation

uses
  {$IFDEF LEVELVCLXE2_RTL}
    System.UITypes,
  {$ENDIF}
  System.Math,
  FMX.Pickers,

  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoClassification,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoStatistics,
  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.FMX.GeoControlVarious,
  Lider.CG.GIS.FMX.GeoComboBoxHelper,
  Lider.CG.GIS.FMX.GeoControlHelper,
  Lider.CG.GIS.FMX.GeoControlStatistics ;

const
  // Maximum number of unique values.
  UNIQUE_LIMIT = 256 ;

//==============================================================================
// Private events & methods
//==============================================================================

  procedure TGIS_ControlLegendGridWiz.updateControlsState(
    const _mode : Integer
  ) ;
  begin
    case _mode of
      0, 1 : begin
        lblAnalyzeMin.Enabled   := True ;
        chkAnalyzeMid.IsChecked := True ;
        chkAnalyzeMid.Enabled   := True ;
        lblAnalyzeMax.Enabled   := True ;
        lblRampStep.Enabled     := True ;
        lblRampLegend.Enabled   := True ;
        edtAnalyzeMin.Enabled   := True ;
        edtRampStep.Enabled     := True ;
        edtAnalyzeMid.Enabled   := True ;
        edtRampLegend.Enabled   := True ;
        edtAnalyzeMax.Enabled   := True ;
      end ;
      2 : begin
        lblAnalyzeMin.Enabled   := True ;
        chkAnalyzeMid.IsChecked := False ;
        chkAnalyzeMid.Enabled   := False ;
        lblAnalyzeMax.Enabled   := True ;
        lblRampStep.Enabled     := False ;
        lblRampLegend.Enabled   := False ;
        edtAnalyzeMin.Enabled   := True ;
        edtRampStep.Enabled     := False ;
        edtAnalyzeMid.Enabled   := False ;
        edtRampLegend.Enabled   := False ;
        edtAnalyzeMax.Enabled   := True ;
      end ;
    end ;

  end ;

  function TGIS_ControlLegendGridWiz.doClassify : Boolean ;
  var
    method     : String ;
    cmin, cmax : TGIS_Color ;
    prm        : TGIS_ParamsList ;
    sec        : TGIS_ParamsRender ;
    sp         : TGIS_ParamsSectionPixel ;
    classifier : TGIS_ClassificationPixel ;
    frm_stats  : TGIS_ControlStatistics ;
    modal_res  : Integer ;
  begin
    Result := False ;

    prm := TGIS_ParamsList.Create ;
    try
      prm.Assign( objLayer.ParamsList ) ;

      classifier := TGIS_ClassificationPixel.Create( objLayer ) ;
      try
        // set properties
        // simple classification / unique
        if rdbAnalyzeUnique.IsChecked then begin
          classifier.Band := '1' ;
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

          classifier.Band       := cmbBandAdvanced.Items[ cmbBandAdvanced.ItemIndex ] ;
          classifier.NumClasses := cmbClassesAdvanced.ItemIndex + 1 ;
          classifier.StartColor := cmin ;
          classifier.EndColor   := cmax ;
          classifier.ShowLegend := True ;

          // set method
          method := cmbMethodAdvanced.Items[cmbMethodAdvanced.ItemIndex] ;
          if method = GIS_RS_CLASSIFY_METHOD_DI then begin
            classifier.Method   := TGIS_ClassificationMethod.DefinedInterval ;
            classifier.Interval := DotStrToFloat( cmbIntervalAdvanced.Text ) ;
          end
          else if method = GIS_RS_CLASSIFY_METHOD_EI then
            classifier.Method := TGIS_ClassificationMethod.EqualInterval
          else if method = GIS_RS_CLASSIFY_METHOD_GI then
            classifier.Method := TGIS_ClassificationMethod.GeometricalInterval
          else if method = GIS_RS_CLASSIFY_METHOD_KM then
            classifier.Method := TGIS_ClassificationMethod.KMeans
          else if method = GIS_RS_CLASSIFY_METHOD_NB then
            classifier.Method := TGIS_ClassificationMethod.NaturalBreaks
          else if method = GIS_RS_CLASSIFY_METHOD_QN then
            classifier.Method := TGIS_ClassificationMethod.Quantile
          else if method = GIS_RS_CLASSIFY_METHOD_QR then
            classifier.Method := TGIS_ClassificationMethod.Quartile
          else if method = GIS_RS_CLASSIFY_METHOD_SD then
            classifier.Method := TGIS_ClassificationMethod.StandardDeviation
          else if method = GIS_RS_CLASSIFY_METHOD_SDC then
            classifier.Method := TGIS_ClassificationMethod.StandardDeviationWithCentral
          else
            classifier.Method := TGIS_ClassificationMethod.NaturalBreaks ;

          // set interval for Standard Deviation
          if ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
             ( method = GIS_RS_CLASSIFY_METHOD_SDC ) then begin
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
          frm_stats := TGIS_ControlStatistics.Create( oMainForm ) ;
          frm_stats.Execute(
            objLayer,
            pOnHelp,
            procedure( _modal_result : TModalResult )
            begin
              modal_res := _modal_result ;
              if ( modal_res = mrCancel ) then
                objLayer.Statistics.ResetModified ;
                rdbAnalyzeUnique.IsChecked := False ;
                rdbAnalyzeContinous.IsChecked := True ;
            end
          ) ;
          if ( modal_res = mrCancel ) then
            exit ;
        end ;

        classifier.EstimateNumClasses ;

        // set color ramp
        if rdbAnalyzeUnique.IsChecked then
          classifier.ColorRamp := getRamp( classifier.NumClasses )
        else if rdbStyleColorRamp.IsChecked then
          classifier.ColorRamp := getRamp( 0 )
        else
          classifier.ColorRamp := nil ;

        classifier.Classify( prm ) ;
      finally
        FreeObject( classifier ) ;
      end ;

      sp := TGIS_ParamsSectionPixel(prm[0]) ;
      sp.Pixel.GridBand    := objLayer.Params.Pixel.GridBand    ;
      sp.Pixel.GridNoValue := objLayer.Params.Pixel.GridNoValue ;
      sp.Pixel.GridShadow  := objLayer.Params.Pixel.GridShadow ;
      sp.FalseZAsText      := objLayer.Params.FalseZAsText ;
      sp.ScaleZ            := objLayer.Params.ScaleZ ;
      sp.NormalizedZ       := objLayer.Params.NormalizedZ ;

      objParams.Assign( prm[0] );
    finally
      prm.Free ;
    end ;

    Result := True ;
  end ;

  function TGIS_ControlLegendGridWiz.doCustomColor(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    val : String ;
    dlg : TGIS_ColorDialog ;
  begin
    Result := '' ;

    if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      // Pass oParentWindow as the owner for positioning
      // the form in the center of LegendForm
      dlg := TGIS_ColorDialog.Create( oMainForm ) ;
      dlg.Execute(
        TGIS_Color.White,
        pOnHelp,
        procedure( _modal_result : TModalResult )
        begin
          if _modal_result <> mrOK then
            exit ;

            val := ConstructParamAsText(
                        GIS_PARAMTXT_TYPE_ARGB,
                        IntToHex( dlg.AlphaColor.ARGB, 2 ),
                        ''
                      ) ;
          TGIS_ComboBoxAbstract( _sender ).DelayedUpdate( val ) ;
        end
      ) ;
    end ;

  end ;

  function TGIS_ControlLegendGridWiz.getRamp(
    const _numClasses : Integer
  ) : TGIS_ColorMapArray ;
  var
    ci       : Integer ;
    method   : String ;
    interval : Double ;
  begin
    // default colormap
    ci := _numClasses ;

    if rdbChooseAdvanced.IsChecked then begin
      method := cmbMethodAdvanced.Items[cmbMethodAdvanced.ItemIndex] ;

      // for StandardDeviation use default colormap
      if ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
         ( method = GIS_RS_CLASSIFY_METHOD_SDC ) then
        ci := 0
      // Quartile always returns 4 class breaks
      else if ( method = GIS_RS_CLASSIFY_METHOD_QR ) then
        ci := 4
      // for DefinedInterval it's easy to calculate number of classes
      else if ( method = GIS_RS_CLASSIFY_METHOD_DI ) and
              TryStrToFloat( cmbIntervalAdvanced.Text, interval ) then
        ci := CeilS( ( objLayer.MaxHeight - objLayer.MinHeight ) / interval )
      else
        ci := cmbClassesAdvanced.ItemIndex + 1 ;
    end ;

    Result := cmbStyleRampList.Value( ci ) ;
  end ;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendGridWiz.FormShow(Sender: TObject);
  begin
    actListUpdate( nil ) ;
  end ;

  procedure TGIS_ControlLegendGridWiz.actDiscreteClick(Sender: TObject);
  var
    idx : Integer ;
  begin
    idx := cmbStyleRampList.ItemIndex ;
    if chkStyleDiscrete.IsChecked then
      cmbStyleRampList.Mode := TGIS_ColorMapMode.Discrete
    else
      cmbStyleRampList.Mode := TGIS_ColorMapMode.Continuous ;

    cmbStyleRampList.ItemIndex := idx ;
  end ;

  procedure TGIS_ControlLegendGridWiz.actShowAllClick(Sender: TObject);
  begin
    if chkShowAllRamps.IsChecked then begin
      lastColorSchemas := cmbStyleRampList.ColorSchemas ;
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Diverging,
                                        TGIS_ColorSchema.Miscellaneous,
                                        TGIS_ColorSchema.Qualitative,
                                        TGIS_ColorSchema.Sequential] ;
    end
    else begin
      if lastColorSchemas = [] then
        cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Diverging,
                                          TGIS_ColorSchema.Miscellaneous,
                                          TGIS_ColorSchema.Qualitative,
                                          TGIS_ColorSchema.Sequential]
      else
        cmbStyleRampList.ColorSchemas := lastColorSchemas ;
    end ;
    cmbStyleRampList.ItemIndex := 0 ;
  end ;

  procedure TGIS_ControlLegendGridWiz.actReverseClick(Sender: TObject);
  var
    idx : Integer ;
  begin
    idx := cmbStyleRampList.ItemIndex ;
    cmbStyleRampList.Reverse := chkReverseRamp.IsChecked ;
    cmbStyleRampList.ItemIndex := idx ;
  end ;

  procedure TGIS_ControlLegendGridWiz.FormDestroy(Sender: TObject);
  begin

  end ;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendGridWiz.actListUpdate(
    Sender: TObject
  ) ;
  var
    bl : Boolean ;
    modal_res : Integer ;

    function check_uniques : Boolean ;
    var
      i             : Integer ;
      unique_count  : Integer ;
      unique_string : String ;
      memo_lines    : TStrings ;
      unique_params : TGIS_VariantArray ;
      stats_funs    : TGIS_StatisticalFunctions ;
      stats_unique  : TGIS_StatisticsItemVariantList ;
      frm_stats     : TGIS_ControlStatistics ;
    begin
      Result := False ;

      if ( memAnalyzeUnique.Lines.Count <= 0 ) then begin
        // determine and add uniques
        stats_funs := TGIS_StatisticalFunctions.EmptyStatistics ;
        stats_funs.Unique := True ;

        // min & max will be needed later
        stats_funs.Min := True ;
        stats_funs.Max := True ;
        objLayer.Statistics.Add( '1', stats_funs ) ;
        SetLength( unique_params, 1 ) ;
        unique_params[0] := UNIQUE_LIMIT ;
        stats_unique := objLayer.Statistics.Get( '1' ).Unique ;
        stats_unique.Params := unique_params ;

        if objLayer.MustCalculateStatistics then begin
          frm_stats := TGIS_ControlStatistics.Create( oMainForm ) ;
          frm_stats.Execute(
            objLayer,
            pOnHelp,
            procedure( _modal_result : TModalResult )
            begin
              modal_res := _modal_result ;
              if ( modal_res = mrCancel ) then begin
                objLayer.Statistics.ResetModified ;
                rdbAnalyzeUnique.IsChecked := False ;
                rdbAnalyzeContinous.IsChecked := True ;
              end ;
            end
          ) ;
          if ( modal_res = mrCancel ) then
            exit ;
        end ;

        unique_count := stats_unique.Values.Count ;
        if ( unique_count < UNIQUE_LIMIT ) then begin
          bLimitExceeded := False ;
          unique_string := GIS_RS_LEGEND_WIZARD_VALUES ;
        end
        else begin
          bLimitExceeded := True ;
          unique_string := GIS_RS_LEGEND_WIZARD_VALLIMIT ;
        end ;

        memo_lines := memAnalyzeUnique.Lines ;
        memo_lines.BeginUpdate ;
        try
          memo_lines.Clear ;
          memo_lines.Add(
            Format( unique_string, [unique_count] )
          ) ;

          for i := 0 to stats_unique.Values.Count-1 do
            memo_lines.Add( stats_unique.Values[i] ) ;
        finally
          memo_lines.EndUpdate ;
        end ;
      end ;

      Result := True ;
    end;

    procedure check_method ;
    var
      method : String ;
      tag    : Double ;
    begin
      method := cmbMethodAdvanced.Items[cmbMethodAdvanced.ItemIndex] ;

      // firstly reset controls if not STDEV
      if not ( ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
               ( method = GIS_RS_CLASSIFY_METHOD_SDC ) ) then
      begin
//        cmbIntervalAdvanced.Style := csDropDown ;
        if Pos( 'STDEV', cmbIntervalAdvanced.Text ) > StringFirst then begin
          cmbIntervalAdvanced.Items.Clear ;
          cmbIntervalAdvanced.Text := DotFloatToStrPrec(
            cmbIntervalAdvanced.Tag / 100.0, 2
          ) ;
        end ;
      end ;

      // set method-specific control properties
      // DefinedInterval
      if method = GIS_RS_CLASSIFY_METHOD_DI then begin
        cmbIntervalAdvanced.Enabled := True ;
        cmbClassesAdvanced.Enabled := False ;
      end
      // Quartile
      else if method = GIS_RS_CLASSIFY_METHOD_QR then begin
        cmbIntervalAdvanced.Enabled := False ;
        cmbClassesAdvanced.Enabled := False ;
      end
      // StandardDeviation(s)
      else if ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
              ( method = GIS_RS_CLASSIFY_METHOD_SDC ) then
      begin
        // use tag to store last interval value
        if TryStrToFloat( cmbIntervalAdvanced.Text, tag ) then
          cmbIntervalAdvanced.Tag := RoundS( tag * 100 ) ;

//        cmbIntervalAdvanced.Style := csDropDownList ;
        if cmbIntervalAdvanced.Items.Count = 0 then begin
          cmbIntervalAdvanced.Items.Add( '1 STDEV' ) ;
          cmbIntervalAdvanced.Items.Add( '1/2 STDEV' ) ;
          cmbIntervalAdvanced.Items.Add( '1/3 STDEV' ) ;
          cmbIntervalAdvanced.Items.Add( '1/4 STDEV' ) ;
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

    procedure check_style ;
    begin
      if rdbStyleColorRange.IsChecked then begin
        lblStyleColorStart.Enabled := True ;
        lblStyleColorEnd.Enabled   := True ;
        cmbStyleColorStart.Enabled := True ;
        cmbStyleColorEnd.Enabled   := True ;
        cmbStyleRampList.Enabled   := False ;
        chkReverseRamp.Enabled     := False ;
        chkStyleDiscrete.Enabled   := False ;
        chkShowAllRamps.Enabled    := False ;

        if iMode = 0 then begin
          btnNextPage.Visible := True ;
          btnOK.Visible := False  ;

          lblStyleColorMid.Enabled     := chkAnalyzeMid.IsChecked ;
          cmbStyleColorMid.Enabled     := chkAnalyzeMid.IsChecked ;
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
      else if rdbStyleColorRamp.IsChecked then begin
        if iMode = 0 then begin
          btnNextPage.Visible := False ;
          btnOK.Visible := True ;
        end ;

        lblStyleColorStart.Enabled   := False ;
        lblStyleColorMid.Enabled     := False ;
        lblStyleColorEnd.Enabled     := False ;
        cmbStyleColorStart.Enabled   := False ;
        cmbStyleColorMid.Enabled     := False ;
        cmbStyleColorEnd.Enabled     := False ;
        cmbStyleRampList.Enabled     := True ;
        chkReverseRamp.Enabled       := True ;
        chkStyleDiscrete.Enabled     := True ;
        chkShowAllRamps.Enabled      := True ;
        chkStyleColorRampAdd.Enabled := False ;
        chkStyleColorHSL.Enabled     := False ;
      end
    end ;

  begin
    case iPage of
      0 :
          begin
            if iMode = 0 then begin
              bl := rdbAnalyzeUnique.Enabled ;
              lblAnalyzeUnique.Visible := bl ;
              memAnalyzeUnique.Visible := bl ;

              bl := rdbAnalyzeUnique.IsChecked ;

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
              chkAnalyzeMid.Visible := bl ;
              edtAnalyzeMid.Visible := bl ;

              bl := rdbAnalyzeContinous.IsChecked ;
              lblAnalyzeMin.Enabled := bl ;
              edtAnalyzeMin.Enabled := bl ;
              lblAnalyzeMax.Enabled := bl ;
              edtAnalyzeMax.Enabled := bl ;
              chkAnalyzeMid.Enabled := bl ;
              edtAnalyzeMid.Enabled := bl and chkAnalyzeMid.IsChecked ;
            end
            else
              check_method ;
          end ;
      1 : check_style ;
      2 :
          begin
            if iMode = 0 then begin
              if rdbStyleColorRange.IsChecked then begin
                if chkStyleColorRampAdd.IsChecked then
                  updateControlsState( 1 )
                else
                  updateControlsState( 0 ) ;
              end
              else
                updateControlsState( 2 ) ;
            end
            else begin
              check_style
            end ;
          end ;
    end ;

    btnPreviousPage.Enabled := iPage >= 0 ;
  end;

  procedure TGIS_ControlLegendGridWiz.actNextPageExecute(Sender: TObject);
  var
    method : String ;
  begin
    case iPage of
      -1 :
          begin
            if rdbChooseSimple.IsChecked then begin
              iMode := 0 ;
              pnlAnalyze.Visible := True ;
              pnlRamp.Visible := False ;
              pnlStyleParams.Visible  := False ;

              rdbAnalyzeContinous.IsChecked := True ;
            end
            else begin
              iMode := 1 ;
              pnlFormulaAdvanced.Visible := True ;
              pnlStyleParams.Visible  := False  ;
            end ;
            pnlChoose.Visible := False ;
          end ;
      0 :
          begin
            // set ramp stuff to default
            chkReverseRamp.IsChecked := False ;
            chkShowAllRamps.IsChecked := False ;
            
            if iMode = 0 then begin
              // too many unique values warning
              if rdbAnalyzeUnique.IsChecked and bLimitExceeded then
                if MessageDlg(
                     Format( _rsrc( GIS_RS_LEGEND_TOO_MANY_UNIQUE ), [UNIQUE_LIMIT] ),
                     TMsgDlgType.mtConfirmation,
                     [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                     0,
                     TMsgDlgBtn.mbNo
                   ) = mrNo
                then
                  exit ;

              pnlAnalyze.Visible := False ;
              pnlStyleParams.Visible := True  ;

              cmbStyleColorStart.Value    := 'ARGB:FF0000FF' ;
              cmbStyleColorMid.Value      := 'ARGB:FF0FF000' ;
              cmbStyleColorEnd.Value      := 'ARGB:FFFF0000' ;

              if rdbAnalyzeUnique.IsChecked then begin
                rdbStyleColorRange.Enabled := False ;
                rdbStyleColorRamp.IsChecked := True ;

                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
                cmbStyleRampList.ItemIndex := cmbStyleRampList.Count - 4 ;
                chkStyleDiscrete.IsChecked := True ;  // set discrete for Unique
              end
              else begin
                rdbStyleColorRange.Enabled := True ;
                rdbStyleColorRange.IsChecked := True ;
                rdbStyleColorRamp.Enabled := True ;

                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
                cmbStyleRampList.ItemIndex := 0 ;
                chkStyleDiscrete.IsChecked := False ;  // continuous by default

              end ;
            end
            else begin
              cmbStyleColorStart.Value    := 'ARGB:FFEFF3FF' ;
              cmbStyleColorMid.Value      := 'ARGB:FF6BAED6' ;
              cmbStyleColorEnd.Value      := 'ARGB:FF08519C' ;

              chkStyleDiscrete.IsChecked := False ;  // continuous by default

              pnlStyleParams.Visible  := True  ;
              pnlFormulaAdvanced.Visible := False ;
              //cmbStyleRampList.Mode := 0 ;
              method := cmbMethodAdvanced.Items[cmbMethodAdvanced.ItemIndex] ;
              if ( method = GIS_RS_CLASSIFY_METHOD_QR ) or
                 ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
                 ( method = GIS_RS_CLASSIFY_METHOD_SDC ) then
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Diverging]
              else
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
              cmbStyleRampList.ItemIndex := 0 ;
              btnNextPage.Visible := False ;
              btnOK.Visible       := True  ;
            end ;
          end ;
      1 :
          begin
            if iMode = 0 then begin
              pnlStyleParams.Visible := False ;
              pnlRamp.Visible := True ;

              btnNextPage.Visible := False ;
              btnOK.Visible := True  ;
            end ;
          end ;
    end ;
    Inc( iPage ) ;
  end;

  procedure TGIS_ControlLegendGridWiz.actPreviousPageExecute(Sender: TObject);
  begin
    case iPage of
      -1 :
          begin
            exit ;
          end ;
      0 :
          begin
            Dec( iPage ) ;
            pnlChoose.Visible          := True  ;
            pnlAnalyze.Visible         := False ;
            pnlRamp.Visible            := False ;
            pnlStyleParams.Visible     := False ;
            pnlFormulaAdvanced.Visible := False ;
          end ;
      1 :
          begin
            Dec( iPage ) ;
            if iMode = 0 then begin
              pnlAnalyze.Visible     := True ;
              pnlStyleParams.Visible := False ;
            end
            else begin
              pnlFormulaAdvanced.Visible := True ;
              pnlStyleParams.Visible     := False ;
            end ;

            btnNextPage.Visible := True ;
            btnOK.Visible       := False  ;
          end ;
      2 :
          begin
            Dec( iPage ) ;
            if iMode = 0 then begin
              pnlStyleParams.Visible := True ;
              pnlRamp.Visible        := False ;

              btnNextPage.Visible    := True ;
              btnOK.Visible          := False  ;
            end
          end ;
    end ;
  end;

  procedure TGIS_ControlLegendGridWiz.btnOKClick(
    Sender: TObject
  );
  var
    prm : TGIS_ParamsMarker ;
    cmin, cmax, cmid : TGIS_Color ;
    cs  : TGIS_ColorInterpolationMode ;
  begin
    // simple classification
    if iMode = 0 then begin

      // unique classification
      if rdbAnalyzeUnique.IsChecked then begin
        if not doClassify then
          exit ;
        objParams.Pixel.Antialias := False ;
        objParams.Pixel.GridShadow := False ;
      end
      // continuous classification / colors
      else if rdbStyleColorRange.IsChecked then begin
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

        if chkStyleColorHSL.IsChecked then
          cs := TGIS_ColorInterpolationMode.HSL
        else
          cs := TGIS_ColorInterpolationMode.RGB ;

        objLayer.GenerateRamp( cmin,
                               cmid,
                               cmax,
                               vvedtMin.Value,
                               vvedtMid.Value,
                               vvedtMax.Value,
                               chkAnalyzeMid.IsChecked,
                               vvedtStep.Value,
                               vvedtLegend.Value,
                               objParams,
                               not chkStyleColorRampAdd.IsChecked,
                               cs
                             )
      end
      // continuous classification / color ramp
      else if ( cmbStyleRampList.ItemIndex >= 0 ) then begin
        if chkStyleDiscrete.IsChecked then
          cs := TGIS_ColorInterpolationMode.None
        else
          cs := TGIS_ColorInterpolationMode.RGB ;

        objLayer.GenerateRampEx( vvedtMin.Value,
                                 vvedtMax.Value,
                                 getRamp(0),
                                 objParams,
                                 cs
                                ) ;
      end;
    end
    // advanced classification
    else begin
      if not doClassify then
       exit ;
      // the classification result can also be visualized
      // with the SmoothColors option enabled
      objParams.Pixel.GridSmoothColors := not chkStyleDiscrete.IsChecked ;
    end ;

    ModalResult := mrOK ;
    {$IFDEF NEXTGEN}
      Hide ;
    {$ENDIF}
  end;

  procedure TGIS_ControlLegendGridWiz.btnNextPageClick(Sender: TObject);
  begin
    actNextPageExecute(Sender);
    actListUpdate(Sender);
  end ;

  procedure TGIS_ControlLegendGridWiz.btnPreviousPageClick(Sender: TObject);
  begin
    actPreviousPageExecute(Sender);
    actListUpdate(Sender);
  end ;

//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendGridWiz.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_GRID ) ;
    ClientWidth  := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      ClientHeight := 285 ;
    {$ELSE}
      ClientHeight := 185 ;
    {$ENDIF}
    Self.Name         := 'TGIS_ControlLegendGridWiz' ;
    Self.OnDestroy    := FormDestroy ;
    Self.OnShow       := FormShow ;
  end ;

  procedure TGIS_ControlLegendGridWiz.initControls ;
  var
    i : Integer ;
    t : Single ;
  begin
    // panel Ramp
    pnlRamp := TLayout.Create( oMainForm ) ;
    pnlRamp.Parent := oMainForm ;
    pnlRamp.Position.Y := 20 ;
    pnlRamp.Position.X := 33 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlRamp.Size.Height := 276 ;
    {$ELSE}
      pnlRamp.Size.Height := 176 ;
    {$ENDIF}
    pnlRamp.Width := 353 ;
    pnlRamp.Size.PlatformDefault := False ;
    pnlRamp.TabOrder := 3 ;

    t := 8 ;

    lblRampStep := TLabel.Create( pnlRamp ) ;
    lblRampStep.Parent := pnlRamp ;
    lblRampStep.AutoSize := True ;
    lblRampStep.Position.Y := t ;
    lblRampStep.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblRampStep, 8, 111 ) ;
    lblRampStep.Size.PlatformDefault := False ;
    lblRampStep.TextSettings.WordWrap := False ;
    lblRampStep.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLSTEP ) ;
    lblRampStep.FixSize ;

    t := t + lblRampStep.Height + 5 ;

    edtRampStep := TEdit.Create( pnlRamp ) ;
    edtRampStep.Parent := pnlRamp ;
    edtRampStep.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                          TInteractiveGesture.DoubleTap] ;
    edtRampStep.TabOrder := 0 ;
    edtRampStep.Position.Y := t ;
    edtRampStep.ClipChildren := True ;
    edtRampStep.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtRampStep, 8, 111 ) ;
    edtRampStep.Size.PlatformDefault := False ;
    edtRampStep.FixSize ;
    edtRampStep.KillFocusByReturn := True ;

    t := t + edtRampStep.Height + 5 ;

    lblRampLegend := TLabel.Create( pnlRamp ) ;
    lblRampLegend.Parent := pnlRamp ;
    lblRampLegend.AutoSize := True ;
    lblRampLegend.Position.Y := t ;
    lblRampLegend.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblRampLegend, 8, 111 ) ;
    lblRampLegend.Size.PlatformDefault := False ;
    lblRampLegend.TextSettings.WordWrap := False ;
    lblRampLegend.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLLEGSTEP ) ;
    lblRampLegend.FixSize ;

    t := t + lblRampLegend.Height + 5 ;

    edtRampLegend := TEdit.Create( pnlRamp ) ;
    edtRampLegend.Parent := pnlRamp ;
    edtRampLegend.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                            TInteractiveGesture.DoubleTap] ;
    edtRampLegend.TabOrder := 4 ;
    edtRampLegend.Position.Y := t ;
    edtRampLegend.ClipChildren := True ;
    edtRampLegend.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtRampLegend, 8, 111 ) ;
    edtRampLegend.Size.PlatformDefault := False ;
    edtRampLegend.FixSize ;
    edtRampLegend.KillFocusByReturn := True ;

    pnlChoose := TLayout.Create( oMainForm ) ;
    pnlChoose.Parent := oMainForm ;
    pnlChoose.Position.Y := 8 ;
    pnlChoose.Position.X := 8 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlChoose.Size.Height := 276 ;
    {$ELSE}
      pnlChoose.Size.Height := 193 ;
    {$ENDIF}
    pnlChoose.Width := 353 ;
    pnlChoose.Size.PlatformDefault := False ;
    pnlChoose.TabOrder := 3 ;

    rdbChooseSimple := TRadioButton.Create( pnlChoose ) ;
    rdbChooseSimple.Parent := pnlChoose;
    rdbChooseSimple.GroupName := 'TGIS_ControlLegendGridWiz' ;
    rdbChooseSimple.Position.Y := 70 ;
    rdbChooseSimple.Size.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbChooseSimple, 90, -1 ) ;
    rdbChooseSimple.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_SIMPLE ) ;
    rdbChooseSimple.TabOrder := 1 ;
    rdbChooseSimple.IsChecked := True ;
    rdbChooseSimple.FixSize ;

    rdbChooseAdvanced := TRadioButton.Create( pnlChoose ) ;
    rdbChooseAdvanced.Parent := pnlChoose;
    rdbChooseAdvanced.GroupName := 'TGIS_ControlLegendGridWiz' ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      rdbChooseAdvanced.Position.Y := 95 ;
    {$ELSE}
      rdbChooseAdvanced.Position.Y := 100 ;
    {$ENDIF}
    rdbChooseAdvanced.Size.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbChooseAdvanced, 90, -1 ) ;
    rdbChooseAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_ADVANCED ) ;
    rdbChooseAdvanced.TabOrder := 1 ;
    rdbChooseAdvanced.IsChecked := False ;
    rdbChooseAdvanced.FixSize ;

    // panel Analyze
    pnlAnalyze := TLayout.Create( oMainForm ) ;
    pnlAnalyze.Parent := oMainForm ;
    pnlAnalyze.Position.Y := 8 ;
    pnlAnalyze.Position.X := 8 ;
    pnlAnalyze.Size.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlAnalyze.Size.Height := 276 ;
    {$ELSE}
      pnlAnalyze.Size.Height := 193 ;
    {$ENDIF}
    pnlAnalyze.Size.PlatformDefault := False ;
    pnlAnalyze.TabOrder := 3 ;

    t := 8 ;

    rdbAnalyzeUnique := TRadioButton.Create( pnlAnalyze ) ;
    rdbAnalyzeUnique.Parent := pnlAnalyze ;
    rdbAnalyzeUnique.GroupName := 'TGIS_ControlLegendGridWiz_mode' ;
    rdbAnalyzeUnique.Position.Y := t ;
    PlaceControl( BiDiMode, nil, rdbAnalyzeUnique, 16, 135 ) ;
    rdbAnalyzeUnique.Size.PlatformDefault := False ;
    rdbAnalyzeUnique.TabOrder := 0 ;
    rdbAnalyzeUnique.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBUNIQUE ) ;
    rdbAnalyzeUnique.OnChange := actListUpdate ;
    rdbAnalyzeUnique.FixSize ;

    t := t + rdbAnalyzeUnique.Height + 5 ;

    lblAnalyzeUnique := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeUnique.Parent := pnlAnalyze ;
    lblAnalyzeUnique.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLUNIQUE ) ;
    lblAnalyzeUnique.AutoSize := True ;
    lblAnalyzeUnique.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblAnalyzeUnique, 16, 135 ) ;
    lblAnalyzeUnique.Size.PlatformDefault := False ;
    lblAnalyzeUnique.FixSize ;

    t := t + lblAnalyzeUnique.Height ;

    memAnalyzeUnique := TMemo.Create( pnlAnalyze ) ;
    memAnalyzeUnique.Parent := pnlAnalyze ;
    memAnalyzeUnique.Touch.InteractiveGestures := [TInteractiveGesture.Pan,
                                                   TInteractiveGesture.LongTap,
                                                   TInteractiveGesture.DoubleTap] ;
    {$IFDEF LEVEL_XE8_FMX}
      memAnalyzeUnique.DataDetectorTypes := [] ;
    {$ENDIF}
    memAnalyzeUnique.ReadOnly := True ;
    memAnalyzeUnique.Position.Y := t ;
    memAnalyzeUnique.Size.Height := 113 ;
    PlaceControl( BiDiMode, nil, memAnalyzeUnique, 16, 135 ) ;
    memAnalyzeUnique.Size.PlatformDefault := False ;
    memAnalyzeUnique.TabOrder := 1 ;

    rdbAnalyzeContinous := TRadioButton.Create( pnlAnalyze ) ;
    rdbAnalyzeContinous.Parent := pnlAnalyze ;
    rdbAnalyzeContinous.GroupName := 'TGIS_ControlLegendGridWiz_mode' ;
    rdbAnalyzeContinous.Position.Y := rdbAnalyzeUnique.Position.Y ; ;
    PlaceControl( BiDiMode, nil, rdbAnalyzeContinous, 184, 135 ) ;
    rdbAnalyzeContinous.Size.PlatformDefault := False ;
    rdbAnalyzeContinous.TabOrder := 2 ;
    rdbAnalyzeContinous.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBCONTINOUS ) ;
    rdbAnalyzeContinous.OnChange := actListUpdate ;
    rdbAnalyzeContinous.FixSize ;

    t := lblAnalyzeUnique.Position.Y ;

    lblAnalyzeMin := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeMin.Parent := pnlAnalyze ;
    lblAnalyzeMin.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLMIN ) ;
    lblAnalyzeMin.AutoSize := True ;
    lblAnalyzeMin.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblAnalyzeMin, 184, 135 ) ;
    lblAnalyzeMin.FixSize ;

    t := t + lblAnalyzeMin.Height ;

    edtAnalyzeMin := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMin.Parent := pnlAnalyze ;
    edtAnalyzeMin.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    edtAnalyzeMin.TabOrder := 3 ;
    edtAnalyzeMin.TextSettings.FontColor := TAlphaColorRec.Red ;
    edtAnalyzeMin.Position.Y := t ;
    edtAnalyzeMin.ClipChildren := True ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMin, 184, 135 ) ;
    edtAnalyzeMin.Size.PlatformDefault := False ;
    edtAnalyzeMin.FixSize ;
    edtAnalyzeMin.KillFocusByReturn := True ;

    t := t + edtAnalyzeMin.Height + 5 ;

    lblAnalyzeMax := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeMax.Parent := pnlAnalyze ;
    lblAnalyzeMax.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_LBLMAX ) ;
    lblAnalyzeMax.AutoSize := True ;
    lblAnalyzeMax.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblAnalyzeMax, 184, 135 ) ;
    lblAnalyzeMax.FixSize ;

    t := t + lblAnalyzeMax.Height ;

    edtAnalyzeMax := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMax.Parent := pnlAnalyze ;
    edtAnalyzeMax.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    edtAnalyzeMax.TabOrder := 4 ;
    edtAnalyzeMax.TextSettings.FontColor := TAlphaColorRec.Red ;
    edtAnalyzeMax.Position.Y := t ;
    edtAnalyzeMax.ClipChildren := True ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMax, 184, 135 ) ;
    edtAnalyzeMax.Size.PlatformDefault := False ;
    edtAnalyzeMax.FixSize ;
    edtAnalyzeMax.KillFocusByReturn := True ;

    t := t + edtAnalyzeMax.Height + 4 ;

    chkAnalyzeMid := TCheckBox.Create( pnlAnalyze ) ;
    chkAnalyzeMid.Parent := pnlAnalyze ;
    chkAnalyzeMid.Position.Y := t ;
    PlaceControl( BiDiMode, nil, chkAnalyzeMid, 184, 135 ) ;
    chkAnalyzeMid.Size.PlatformDefault := False ;
    chkAnalyzeMid.TabOrder := 5 ;
    chkAnalyzeMid.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_CHKAVG ) ;
    chkAnalyzeMid.OnChange := actListUpdate ;

    t := t + chkAnalyzeMid.Height ;

    edtAnalyzeMid := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMid.Parent := pnlAnalyze ;
    edtAnalyzeMid.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    edtAnalyzeMid.TabOrder := 6 ;
    edtAnalyzeMid.TextSettings.FontColor := TAlphaColorRec.Red ;
    edtAnalyzeMid.Position.Y := t ;
    edtAnalyzeMid.ClipChildren := True ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMid, 184, 135 ) ;
    edtAnalyzeMid.Size.PlatformDefault := False ;
    edtAnalyzeMid.FixSize ;
    edtAnalyzeMid.KillFocusByReturn := True ;

    // panel Style Params
    pnlStyleParams := TLayout.Create( oMainForm ) ;
    pnlStyleParams.Parent := oMainForm ;
    pnlStyleParams.Position.Y := 8 ;
    pnlStyleParams.Position.X := 8 ;
    pnlStyleParams.Size.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlStyleParams.Size.Height := 276 ;
    {$ELSE}
      pnlStyleParams.Size.Height := 193 ;
    {$ENDIF}
    pnlStyleParams.Size.PlatformDefault := False ;
    pnlStyleParams.TabOrder := 3 ;

    rdbStyleColorRange := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRange.Parent := pnlStyleParams;
    rdbStyleColorRange.Position.Y := 10 ;
    rdbStyleColorRange.Size.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRange, 8, 90 ) ;
    rdbStyleColorRange.Text := _rsrc( GIS_RS_LEGEND_PRM_COLOR ) ;
    rdbStyleColorRange.TabOrder := 1 ;
    rdbStyleColorRange.IsChecked := True ;
    rdbStyleColorRange.OnChange := actListUpdate ;
    rdbStyleColorRange.FixSize ;

    t := 35 ;
    lblStyleColorStart := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorStart.Parent := pnlStyleParams ;
    lblStyleColorStart.Position.Y := t ;
    lblStyleColorStart.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorStart, 16, 121 ) ;
    lblStyleColorStart.Text := _rsrc( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
    lblStyleColorStart.FixSize ;

    cmbStyleColorStart := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorStart.Parent := pnlStyleParams ;
    cmbStyleColorStart.Position.Y := t + 16 ;
    cmbStyleColorStart.Size.Height := 28 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorStart, 16, 100 ) ;
    cmbStyleColorStart.Fill( False, False ) ;
    cmbStyleColorStart.CustomEvent := doCustomColor ;

    lblStyleColorMid := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorMid.Parent := pnlStyleParams ;
    lblStyleColorMid.Position.Y := t ;
    lblStyleColorMid.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorMid, 130, 121 ) ;
    lblStyleColorMid.Text := _rsrc( GIS_RS_LEGEND_PRM_MIDDLECOLOR ) ;
    lblStyleColorMid.FixSize ;

    cmbStyleColorMid := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorMid.Parent := pnlStyleParams ;
    cmbStyleColorMid.Position.Y := t + 16 ;
    cmbStyleColorMid.Size.Height := 28 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorMid, 130, 100 ) ;
    cmbStyleColorMid.Fill( False, False ) ;
    cmbStyleColorMid.CustomEvent := doCustomColor ;

    lblStyleColorEnd := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorEnd.Parent := pnlStyleParams ;
    lblStyleColorEnd.Position.Y := t ;
    lblStyleColorEnd.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorEnd, 244, 121 ) ;
    lblStyleColorEnd.Text := _rsrc( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
    lblStyleColorEnd.FixSize ;

    cmbStyleColorEnd := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorEnd.Parent := pnlStyleParams ;
    cmbStyleColorEnd.Position.Y := t + 16 ;
    cmbStyleColorEnd.Size.Height := 28 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorEnd, 244, 100 ) ;
    cmbStyleColorEnd.Fill( False, False ) ;
    cmbStyleColorEnd.CustomEvent := doCustomColor ;

    chkStyleColorRampAdd := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleColorRampAdd.Parent := pnlStyleParams ;
    chkStyleColorRampAdd.Position.Y := t + 8 + 21 + 20 ;
    chkStyleColorRampAdd.Size.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleColorRampAdd, 16, 211 ) ;
    chkStyleColorRampAdd.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_ADD ) ;
    chkStyleColorRampAdd.TabOrder := 5 ;

    chkStyleColorHSL := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleColorHSL.Parent := pnlStyleParams ;
    chkStyleColorHSL.Position.Y := t + 8 + 21 + 20 ;
    chkStyleColorHSL.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleColorHSL, 244, 100 ) ;
    chkStyleColorHSL.Text := _rsrc( GIS_RS_LEGEND_WIZARD_USEHSL ) ;
    chkStyleColorHSL.TabOrder := 6 ;
    chkStyleColorHSL.IsChecked := True ;

    rdbStyleColorRamp := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRamp.Parent := pnlStyleParams;
    rdbStyleColorRamp.Position.Y := 112 ;
    rdbStyleColorRamp.Size.Height := 22 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRamp, 8, 90 ) ;
    rdbStyleColorRamp.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMPS ) ;
    rdbStyleColorRamp.TabOrder := 1 ;
    rdbStyleColorRamp.OnChange := actListUpdate ;
    rdbStyleColorRamp.IsChecked := False ;
    rdbStyleColorRamp.FixSize ;

    cmbStyleRampList := TGIS_ColorRampComboBox.Create( pnlStyleParams ) ;
    cmbStyleRampList.Parent := pnlStyleParams ;
    cmbStyleRampList.Position.Y := 139 ;
    cmbStyleRampList.Size.Height := 22 ;
    PlaceControl( BiDiMode, nil, cmbStyleRampList, 16, 330 ) ;
    cmbStyleRampList.TabOrder := 5 ;
    cmbStyleRampList.Fill ;
    cmbStyleRampList.ItemIndex := 0 ;

    chkReverseRamp := TCheckBox.Create( pnlStyleParams ) ;
    chkReverseRamp.Parent := pnlStyleParams ;
    chkReverseRamp.Position.Y := 165 ;
    chkReverseRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkReverseRamp, 20, 90 ) ;
    chkReverseRamp.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_REVERSE ) ;
    chkReverseRamp.IsChecked := False ;
    chkReverseRamp.TabOrder := 7 ;
    chkReverseRamp.OnChange := actReverseClick ;

    chkStyleDiscrete := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleDiscrete.Parent := pnlStyleParams ;
    chkStyleDiscrete.Position.Y := 165 ;
    chkStyleDiscrete.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleDiscrete, 130, 100 ) ;
    chkStyleDiscrete.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_DISCRETE ) ;
    chkStyleDiscrete.OnChange := actDiscreteClick ;
    chkStyleDiscrete.TabOrder := 8 ;

    chkShowAllRamps := TCheckBox.Create( pnlStyleParams ) ;
    chkShowAllRamps.Parent := pnlStyleParams ;
    chkShowAllRamps.Position.Y := 165 ;
    chkShowAllRamps.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkShowAllRamps, 240, 100 ) ;
    chkShowAllRamps.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_SHOWALL ) ;
    chkShowAllRamps.OnChange := actShowAllClick ;
    chkShowAllRamps.TabOrder := 8 ;

    // panel Formula Advanced
    pnlFormulaAdvanced := TLayout.Create( oMainForm ) ;
    pnlFormulaAdvanced.Parent := oMainForm ;
    pnlFormulaAdvanced.Position.Y := 8 ;
    pnlFormulaAdvanced.Position.X := 8 ;
    pnlFormulaAdvanced.Size.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlFormulaAdvanced.Size.Height := 276 ;
    {$ELSE}
      pnlFormulaAdvanced.Size.Height := 176 ;
    {$ENDIF}
    pnlFormulaAdvanced.Size.PlatformDefault := False ;
    pnlFormulaAdvanced.TabOrder := 3 ;

    lblMethodAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblMethodAdvanced.Parent := pnlFormulaAdvanced ;
    lblMethodAdvanced.Position.Y := 52 ;
    lblMethodAdvanced.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblMethodAdvanced, 8, -1 ) ;
    lblMethodAdvanced.AutoSize := False ;
    lblMethodAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_METHOD ) ;
    lblMethodAdvanced.FixSize ;

    cmbMethodAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbMethodAdvanced.Parent := pnlFormulaAdvanced ;
    cmbMethodAdvanced.Position.Y := 68 ;
    cmbMethodAdvanced.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbMethodAdvanced, 8, 160 ) ;
    cmbMethodAdvanced.TabOrder := 0 ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_DI ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_EI ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_GI ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_NB ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_KM ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_QN ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_QR ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_SD ) ) ;
    cmbMethodAdvanced.Items.Add( _rsrc( GIS_RS_CLASSIFY_METHOD_SDC ) ) ;
    cmbMethodAdvanced.ItemIndex := 3 ;
    cmbMethodAdvanced.OnChange := actListUpdate ;

    lblClassesAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblClassesAdvanced.Parent := pnlFormulaAdvanced ;
    lblClassesAdvanced.Position.Y := 52 ;
    lblClassesAdvanced.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblClassesAdvanced, 182, 80 ) ;
    lblClassesAdvanced.AutoSize := False ;
    lblClassesAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_CLASSES ) ;
    lblClassesAdvanced.FixSize ;

    cmbClassesAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbClassesAdvanced.Parent := pnlFormulaAdvanced ;
    cmbClassesAdvanced.Position.Y := 68 ;
    cmbClassesAdvanced.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbClassesAdvanced, 182, 75 ) ;
    cmbClassesAdvanced.TabOrder := 0 ;
    for i := 1 to 9 do
      cmbClassesAdvanced.Items.Add( IntToStr( i ) ) ;
    cmbClassesAdvanced.ItemIndex := 4 ;
    cmbClassesAdvanced.OnChange := actListUpdate ;

    lblIntervalAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    lblIntervalAdvanced.Position.Y := 52 ;
    lblIntervalAdvanced.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblIntervalAdvanced, 270, 75 ) ;
    lblIntervalAdvanced.AutoSize := False ;
    lblIntervalAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_INTERVAL ) ;
    lblIntervalAdvanced.FixSize ;

    cmbIntervalAdvanced := TComboEdit.Create( pnlFormulaAdvanced ) ;
    cmbIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    cmbIntervalAdvanced.Position.Y := 68 ;
    cmbIntervalAdvanced.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbIntervalAdvanced, 270, 75 ) ;
    cmbIntervalAdvanced.TabOrder := 0 ;
    cmbIntervalAdvanced.Text := '100' ;
    cmbIntervalAdvanced.OnChange := actListUpdate ;

    lblBandAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblBandAdvanced.Parent := pnlFormulaAdvanced ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      lblBandAdvanced.Position.Y := 107 ;
    {$ELSE}
      lblBandAdvanced.Position.Y := 97 ;
    {$ENDIF}
    lblBandAdvanced.Size.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblBandAdvanced, 8, 80 ) ;
    lblBandAdvanced.AutoSize := False ;
    lblBandAdvanced.Text := _rsrc( GIS_RS_LEGEND_PRM_BAND ) ;
    lblBandAdvanced.FixSize ;

    cmbBandAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbBandAdvanced.Parent := pnlFormulaAdvanced ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      cmbBandAdvanced.Position.Y := 124 ;
    {$ELSE}
      cmbBandAdvanced.Position.Y := 114 ;
    {$ENDIF}
    cmbBandAdvanced.Size.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbBandAdvanced, 8, 75 ) ;
    cmbBandAdvanced.TabOrder := 0 ;
    cmbBandAdvanced.OnChange := actListUpdate ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
      btnHelp.TabOrder := 3 ;

      btnCancel.Position.Y := ClientHeight - btnCancel.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;
      btnCancel.TabOrder := 7 ;
    {$ENDIF}

    btnNextPage := TButton.Create( oMainForm ) ;
    btnNextPage.Parent := oMainForm ;
    btnNextPage.Enabled := True ;
    btnNextPage.Size.Height := 24 ;
    btnNextPage.Position.Y := ClientHeight - btnNextPage.Height - 8 ;
    PlaceControl( BiDiMode, btnCancel, btnNextPage, -8, 80 ) ;
    btnNextPage.Size.PlatformDefault := False ;
    btnNextPage.TabOrder := 5 ;
    btnNextPage.Text := _rsrcna( GIS_RS_BTN_NEXT ) ;
    btnNextPage.OnClick := btnNextPageClick ;

    btnPreviousPage := TButton.Create( oMainForm ) ;
    btnPreviousPage.Parent := oMainForm ;
    btnPreviousPage.Enabled := True ;
    btnPreviousPage.Size.Height := 24 ;
    btnPreviousPage.Position.Y := ClientHeight - btnPreviousPage.Height - 8 ;
    PlaceControl( BiDiMode, btnNextPage, btnPreviousPage, -8, 80 ) ;
    btnPreviousPage.Size.PlatformDefault := False ;
    btnPreviousPage.TabOrder := 4 ;
    btnPreviousPage.Text := _rsrcna( GIS_RS_BTN_PREVIOUS ) ;
    btnPreviousPage.OnClick := btnPreviousPageClick ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnOK.Position.Y := ClientHeight - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnPreviousPage, btnOK, -8, 80 ) ;
      btnOK.TabOrder := 6 ;
      btnCancel.Position.Y := btnNextPage.Position.Y - btnOK.Height ;
    {$ENDIF}

    if BIDiMode = bdRightToLeft then begin
      {$IFNDEF GIS_MOBILE_DIALOGS}
        btnHelp.Anchors       := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
        btnCancel.Anchors     := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
        btnOK.Anchors         := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
      {$ENDIF}
      btnNextPage.Anchors     := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
      btnPreviousPage.Anchors := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
    end else begin
      {$IFNDEF GIS_MOBILE_DIALOGS}
        btnHelp.Anchors       := [TAnchorKind.akLeft, TAnchorKind.akBottom] ;
        btnCancel.Anchors     := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
        btnOK.Anchors         := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
      {$ENDIF}
      btnNextPage.Anchors     := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
      btnPreviousPage.Anchors := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
    end ;
    iPage := -1 ;
    iMode := 0 ;

    lblAnalyzeMin.FixSize    ;
    lblAnalyzeMax.FixSize    ;
    lblRampStep.FixSize   ;
    lblRampLegend.FixSize ;
    
    vvedtMin := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMin    ) ;
    vvedtMin.Precision := 15 ;
    vvedtMin.MinVal := -GIS_MAX_SINGLE ;
    vvedtMin.MaxVal := GIS_MAX_SINGLE ;

    vvedtStep := TGIS_ValueValidatorEditHelper.Create( edtRampStep   ) ;
    vvedtStep.Precision := 15 ;
    vvedtStep.MinVal := -GIS_MAX_SINGLE ;
    vvedtStep.MaxVal := GIS_MAX_SINGLE ;

    vvedtMid := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMid    ) ;
    vvedtMid.Precision := 15 ;
    vvedtMid.MinVal := -GIS_MAX_SINGLE ;
    vvedtMid.MaxVal := GIS_MAX_SINGLE ;

    vvedtLegend := TGIS_ValueValidatorEditHelper.Create( edtRampLegend ) ;
    vvedtLegend.Precision := 15 ;
    vvedtLegend.MinVal := -GIS_MAX_SINGLE ;
    vvedtLegend.MaxVal := GIS_MAX_SINGLE ;

    vvedtMax := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMax    ) ;
    vvedtMax.Precision := 15 ;
    vvedtMax.MinVal := -GIS_MAX_SINGLE ;
    vvedtMax.MaxVal := GIS_MAX_SINGLE ;

    rdbChooseSimple.FixSize ;
    rdbChooseSimple.IsChecked := True ;
    rdbChooseAdvanced.FixSize ;

    pnlChoose.Visible             := True ;

    pnlAnalyze.Visible            := False ;
    pnlAnalyze.Position.X         := pnlChoose.Position.X ;
    pnlAnalyze.Position.Y         := pnlChoose.Position.Y ;

    pnlRamp.Visible               := False ;
    pnlRamp.Position.X            := pnlChoose.Position.X ;
    pnlRamp.Position.Y            := pnlChoose.Position.Y ;

    pnlStyleParams.Visible        := False ;
    pnlStyleParams.Position.X     := pnlChoose.Position.X ;
    pnlStyleParams.Position.Y     := pnlChoose.Position.Y ;

    pnlFormulaAdvanced.Visible    := False ;
    pnlFormulaAdvanced.Position.X := pnlChoose.Position.X ;
    pnlFormulaAdvanced.Position.Y := pnlChoose.Position.Y ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnOK.Visible := False ;
      btnOK.Position.X := btnNextPage.Position.X ;
      btnOK.Position.Y := btnNextPage.Position.Y ;
      btnCancel.Position.Y := btnNextPage.Position.Y  ;
    {$ENDIF}

    ClientWidth  := TruncS(
      pnlChoose.Position.X + pnlChoose.Width + pnlChoose.Position.X ) ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      ClientHeight := TruncS(
        pnlChoose.Position.Y + pnlChoose.Height + pnlChoose.Position.Y ) ;
    {$ELSE}
      ClientHeight := TruncS( pnlChoose.Position.Y + pnlChoose.Height + 8 +
                              btnCancel.Height + 8 ) ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlLegendGridWiz.Execute(
    const _layer   : TGIS_LayerPixel         ;
    const _params  : TGIS_ParamsSectionPixel ;
    const _onhelp  : TGIS_HelpEvent ;
    const _resProc : TProc<TModalResult>
  ) ;
  var
    i : Integer ;
  begin
    Assert( Assigned( _layer ) ) ;
    objLayer  := _layer  ;

    Assert( Assigned( _params ) ) ;
    objParams := _params ;

    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    vvedtMin.Value    := _layer.MinHeight ;
    vvedtMid.Value    := ( _layer.MaxHeight - _layer.MinHeight ) / 2 +
                         _layer.MinHeight ;
    vvedtMax.Value    := _layer.MaxHeight ;
    vvedtStep.Value   := ( _layer.MaxHeight - _layer.MinHeight ) / 100 ;
    vvedtLegend.Value := ( _layer.MaxHeight - _layer.MinHeight ) / 10 ;

    if vvedtStep.Value = 0 then
      vvedtStep.Value := vvedtLegend.Value ;

    for i := 1 to _layer.BandsCount do
      cmbBandAdvanced.Items.Add( IntToStr( i ) ) ;
    cmbBandAdvanced.ItemIndex := 0 ;

    cmbIntervalAdvanced.Text := DotFloatToStr( vvedtLegend.Value * 2 ) ;

    actListUpdate( nil ) ;

    ShowModalEx( _resProc ) ;
  end ;

  procedure TGIS_ControlLegendGridWiz.Execute(
    const _layer   : TGIS_LayerPixel         ;
    const _params  : TGIS_ParamsSectionPixel ;
    const _resProc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _layer, _params, hlp, _resProc ) ;
  end;

//==================================== END =====================================
end.



