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

unit Lider.CG.GIS.VCL.GeoControlLegendGridWiz ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlLegendGridWiz"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Variants,
  System.SysUtils,

  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ActnList,
  VCL.Buttons,
  VCL.ExtCtrls,

  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.VCL.GeoModalForm,
  Lider.CG.GIS.VCL.GeoValueValidatorHelper,
  Lider.CG.GIS.VCL.GeoControlColor ;

type

  /// <summary>
  ///   Visual form for managing vector rendering wizard.
  /// </summary>
  TGIS_ControlLegendGridWiz = class( TGIS_ModalForm )
    // buttons
    btnNextPage: TButton;
    btnPreviousPage: TButton;

    // simple/advanced classification choice
    pnlChoose: TPanel;
    rdbChooseSimple: TRadioButton;
    rdbChooseAdvanced: TRadioButton;

    // simple -> unique/continuous choice
    pnlAnalyze: TPanel;
    rdbAnalyzeUnique: TRadioButton;
    lblAnalyzeUnique: TLabel;
    memAnalyzeUnique: TMemo;
    rdbAnalyzeContinous: TRadioButton;

    lblAnalyzeMin: TLabel;
    edtAnalyzeMin: TEdit;
    lblAnalyzeMax: TLabel;
    edtAnalyzeMax: TEdit;
    chkAnalyzeMid: TCheckBox;
    edAnalyzetMid: TEdit;

    // simple -> colors/ramps choice
    pnlStyleParams: TPanel;
    rdbStyleColorRange: TRadioButton;
    lblStyleColorStart: TLabel;
    cmbStyleColorStart: TGIS_ColorComboBox ;
    lblStyleColorMid: TLabel;
    cmbStyleColorMid: TGIS_ColorComboBox ;
    lblStyleColorEnd: TLabel;
    cmbStyleColorEnd: TGIS_ColorComboBox ;
    chkStyleColorRampAdd: TCheckBox;
    chkStyleColorHSL: TCheckBox;
    rdbStyleColorRamp: TRadioButton;
    cmbStyleRampList: TGIS_ColorRampComboBox;
    chkReverseRamp: TCheckBox;
    chkStyleDiscrete: TCheckBox;
    chkShowAllRamps: TCheckBox;

    // simple -> min/mid/max/interval values
    pnlRamp: TPanel;
    lblRampStep: TLabel;
    edtRampStep: TEdit;
    lblRampLegend: TLabel;
    edtRampLegend: TEdit;

    // advanced
    pnlFormulaAdvanced: TPanel;
    lblMethodAdvanced: TLabel;
    cmbMethodAdvanced: TComboBox;
    lblClassesAdvanced: TLabel;
    cmbClassesAdvanced: TComboBox;
    lblIntervalAdvanced: TLabel;
    cmbIntervalAdvanced: TComboBox;
    lblBandAdvanced: TLabel;
    cmbBandAdvanced: TComboBox;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure actListUpdate(Sender: TObject);
    procedure actNextPageExecute(Sender: TObject);
    procedure actPreviousPageExecute(Sender: TObject);
    procedure btnOKClick(Sender: TObject); override;
    procedure btnNextPageClick(Sender: TObject);
    procedure btnPreviousPageClick(Sender: TObject);
    procedure actDiscreteClick(Sender: TObject);
    procedure actReverseClick(Sender: TObject);
    procedure actShowAllClick(Sender: TObject);
  private
    objLayer         : TGIS_LayerPixel   ;
    objParams        : TGIS_ParamsSectionPixel  ;
    vvedtMin         : IGIS_ValueValidator ;
    vvedtStep        : IGIS_ValueValidator ;
    vvedtMid         : IGIS_ValueValidator ;
    vvedtLegend      : IGIS_ValueValidator ;
    vvedtMax         : IGIS_ValueValidator ;
    iPage            : Integer          ;
    iMode            : Integer          ;
    bLimitExceeded   : Boolean ;
    lastColorSchemas : TGIS_ColorSchemas ;
  private
    procedure updateControlsState( const _mode : Integer ) ;
    function  doClassify         : Boolean ;
    function  doCustomColor      ( _sender : TObject ;
                                   _value  : String
                                 ) : String ;
    function  getRamp            ( const _numClasses : Integer
                                 ) : TGIS_ColorMapArray ;
  protected
    /// <inheritdoc/>
    procedure initForm        ; override;

    /// <inheritdoc/>
    procedure initControls    ; override;

    /// <inheritdoc/>
    procedure showForm        ; override;

    /// <inheritdoc/>
    procedure afterPPIChanged ; override;
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
    /// <returns>
    ///   Modal result.
    /// </returns>
    function Execute       ( const _layer  : TGIS_LayerPixel         ;
                             const _params : TGIS_ParamsSectionPixel ;
                             const _onhelp : TGIS_HelpEvent
                           ) : Integer ; overload;

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
                           ) : Integer ; overload;
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
  Lider.CG.GIS.GeoXmlDoc,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoClassification,
  Lider.CG.GIS.GeoStatistics,
  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.VCL.GeoControlHelper,
  Lider.CG.GIS.VCL.GeoControlStatistics ;

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
        lblAnalyzeMin.Enabled := True ;
        chkAnalyzeMid.Checked := True ;
        chkAnalyzeMid.Enabled := True ;
        lblAnalyzeMax.Enabled := True ;
        lblRampStep.Enabled   := True ;
        lblRampLegend.Enabled := True ;
        edtAnalyzeMin.Enabled := True ;
        edtRampStep.Enabled   := True ;
        edAnalyzetMid.Enabled := True ;
        edtRampLegend.Enabled := True ;
        edtAnalyzeMax.Enabled := True ;
      end ;
      2 : begin
        lblAnalyzeMin.Enabled := True ;
        chkAnalyzeMid.Checked := False ;
        chkAnalyzeMid.Enabled := False ;
        lblAnalyzeMax.Enabled := True ;
        lblRampStep.Enabled   := False ;
        lblRampLegend.Enabled := False ;
        edtAnalyzeMin.Enabled := True ;
        edtRampStep.Enabled   := False ;
        edAnalyzetMid.Enabled := False ;
        edtRampLegend.Enabled := False ;
        edtAnalyzeMax.Enabled := True ;
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
  begin
    Result := False ;

    prm := TGIS_ParamsList.Create ;
    try
      prm.Assign( objLayer.ParamsList ) ;

      classifier := TGIS_ClassificationPixel.Create( objLayer ) ;
      try
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
          frm_stats := TGIS_ControlStatistics.Create( Self ) ;
          try
            if ( frm_stats.Execute( objLayer, pOnHelp ) = mrCancel ) then
              exit ;
          finally
            FreeObject( frm_stats ) ;
          end ;
        end ;

        classifier.EstimateNumClasses ;

        // set color ramp
        if rdbAnalyzeUnique.Checked then
          classifier.ColorRamp := getRamp( classifier.NumClasses )
        else if rdbStyleColorRamp.Checked then
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
    dlg : TGIS_ColorDialog ;
  begin
    Result := '' ;

    if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      // Pass oParentWindow as the owner for positioning
      // the form in the center of LegendForm
      dlg := TGIS_ColorDialog.Create( self ) ;
      try
        if dlg.Execute( TGIS_Color.White, pOnHelp  ) = mrOK then begin
          Result := ConstructParamAsText(
                      GIS_PARAMTXT_TYPE_ARGB,
                      IntToHex( dlg.AlphaColor.ARGB, 2 ),
                      ''
                    ) ;
        end ;
      finally
        FreeObject( dlg ) ;
      end ;
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

    if rdbChooseAdvanced.Checked then begin
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

  procedure TGIS_ControlLegendGridWiz.FormCreate(Sender: TObject);
  begin
    iPage := -1 ;
    iMode := 0 ;
  end;

  procedure TGIS_ControlLegendGridWiz.FormDestroy(Sender: TObject);
  begin

  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendGridWiz.actListUpdate(
    Sender: TObject
  );
  var
    bl : Boolean ;

    function check_uniques : Boolean ;
    var
      i             : Integer ;
      band          : String ;
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
        band := IntToStr( objLayer.Params.Pixel.GridBand ) ;
        objLayer.Statistics.Add( band, stats_funs ) ;
        SetLength( unique_params, 1 ) ;
        unique_params[0] := UNIQUE_LIMIT ;
        stats_unique := objLayer.Statistics.Get( band ).Unique ;
        stats_unique.Params := unique_params ;

        if objLayer.MustCalculateStatistics then begin
          frm_stats := TGIS_ControlStatistics.Create( Self ) ;
          try
            if ( frm_stats.Execute( objLayer, pOnHelp ) = mrCancel ) then begin
              objLayer.Statistics.ResetModified ;
              rdbAnalyzeUnique.Checked := False ;
              rdbAnalyzeContinous.Checked := True ;

              exit ;
            end ;
          finally
            FreeObject( frm_stats ) ;
          end ;
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
        cmbIntervalAdvanced.Style := csDropDown ;
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

        cmbIntervalAdvanced.Style := csDropDownList ;
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
      if rdbStyleColorRange.Checked then begin
        lblStyleColorStart.Enabled     := True ;
        lblStyleColorEnd.Enabled       := True ;
        cmbStyleColorStart.Enabled     := True ;
        cmbStyleColorEnd.Enabled       := True ;
        cmbStyleRampList.Enabled       := False ;
        chkReverseRamp.Enabled         := False ;
        chkStyleDiscrete.Enabled       := False ;
        chkShowAllRamps.Enabled        := False ;

        if iMode = 0 then begin
          btnNextPage.Visible := True ;
          btnOK.Visible := False  ;

          lblStyleColorMid.Enabled     := chkAnalyzeMid.Checked ;
          cmbStyleColorMid.Enabled     := chkAnalyzeMid.Checked;
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
        if iMode = 0 then begin
          btnNextPage.Visible := False ;
          btnOK.Visible := True ;
        end ;

        lblStyleColorStart.Enabled     := False ;
        lblStyleColorMid.Enabled       := False ;
        lblStyleColorEnd.Enabled       := False ;
        cmbStyleColorStart.Enabled     := False ;
        cmbStyleColorMid.Enabled       := False ;
        cmbStyleColorEnd.Enabled       := False ;
        cmbStyleRampList.Enabled       := True ;
        chkReverseRamp.Enabled         := True ;
        chkStyleDiscrete.Enabled       := True ;
        chkShowAllRamps.Enabled        := True ;
        chkStyleColorRampAdd.Enabled   := False ;
        chkStyleColorHSL.Enabled       := False ;
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
              chkAnalyzeMid.Visible := bl ;
              edAnalyzetMid.Visible := bl ;

              bl := rdbAnalyzeContinous.Checked ;
              lblAnalyzeMin.Enabled := bl ;
              edtAnalyzeMin.Enabled := bl ;
              lblAnalyzeMax.Enabled := bl ;
              edtAnalyzeMax.Enabled := bl ;
              chkAnalyzeMid.Enabled := bl ;
              edAnalyzetMid.Enabled := bl and chkAnalyzeMid.Checked ;
            end
            else
              check_method ;
          end ;
      1 : check_style ;
      2 :
          begin
            if iMode = 0 then begin
              if rdbStyleColorRange.Checked then begin
                if chkStyleColorRampAdd.Checked then
                  updateControlsState( 1 )
                else
                  updateControlsState( 0 ) ;
              end
              else
                updateControlsState( 2 ) ;
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
            if rdbChooseSimple.Checked then begin
              iMode := 0 ;
              pnlAnalyze.Visible := True ;
              pnlRamp.Visible := False ;
              pnlStyleParams.Visible  := False ;

              rdbAnalyzeContinous.Checked := True ;
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
            chkReverseRamp.Checked := False ;
            chkShowAllRamps.Checked := False ;
            
            if iMode = 0 then begin
              // too many unique values warning
              if rdbAnalyzeUnique.Checked and bLimitExceeded then
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

              cmbStyleColorStart.Value := 'ARGB:FF0000FF' ;
              cmbStyleColorMid.Value   := 'ARGB:FF0FF000' ;
              cmbStyleColorEnd.Value   := 'ARGB:FFFF0000' ;

              if rdbAnalyzeUnique.Checked then begin
                rdbStyleColorRange.Enabled := False ;
                rdbStyleColorRamp.Checked := True ;

                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
                cmbStyleRampList.ItemIndex := cmbStyleRampList.GetCount - 4 ;
                chkStyleDiscrete.Checked := True ;  // set discrete for Unique
              end
              else begin
                rdbStyleColorRange.Enabled := True ;
                rdbStyleColorRange.Checked := True ;
                rdbStyleColorRamp.Enabled := True ;

                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
                cmbStyleRampList.ItemIndex := 0 ;
                chkStyleDiscrete.Checked := False ;  // continuous by default
              end ;
            end
            else begin
              cmbStyleColorStart.Value    := 'ARGB:FFEFF3FF' ;
              cmbStyleColorMid.Value      := 'ARGB:FF6BAED6' ;
              cmbStyleColorEnd.Value      := 'ARGB:FF08519C' ;

              chkStyleDiscrete.Checked := False ;  // continuous by default

              pnlStyleParams.Visible  := True  ;
              pnlFormulaAdvanced.Visible := False ;
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
    prm              : TGIS_ParamsMarker ;
    cmin, cmax, cmid : TGIS_Color ;
    cs               : TGIS_ColorInterpolationMode ;
    classifier       : TGIS_ClassificationPixel ;
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
                               vvedtMid.Value,
                               vvedtMax.Value,
                               chkAnalyzeMid.Checked,
                               vvedtStep.Value,
                               vvedtLegend.Value,
                               objParams,
                               not chkStyleColorRampAdd.Checked,
                               cs
                              )
      end
      // continuous classification / color ramp
      else begin
        if chkStyleDiscrete.Checked then
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
      objParams.Pixel.GridSmoothColors := not chkStyleDiscrete.Checked ;
    end ;

    ModalResult := mrOK ;
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
    Self.ActiveControl := btnNextPage ;
    Self.Caption       := _rsrc( GIS_RS_LEGEND_WIZARD_GRID ) ;
    Self.ClientHeight  := 440 ;
    Self.ClientWidth   := 734 ;
    Self.Name          := 'TGIS_ControlLegendGridWiz' ;
    Self.OnCreate      := FormCreate ;
    Self.OnDestroy     := FormDestroy ;
  end ;

  procedure TGIS_ControlLegendGridWiz.initControls ;
  var
    anchors   : TAnchors ;
    anchorsLR : TAnchors ;
    t, i      : Integer ;
  begin
    if BiDiMode = bdRightToLeft then
      anchors := [akRight, akTop]
    else
      anchors := [akLeft, akTop] ;

    anchorsLR := [akLeft, akTop, akRight] ;

    pnlRamp := TPanel.Create( Self ) ;
    pnlRamp.Parent := Self ;
    pnlRamp.Anchors := anchorsLR ;
    pnlRamp.Top := 20 ;
    pnlRamp.Height := 139 ;
    PlaceControl( BiDiMode, nil, pnlRamp, 33, -1 ) ;
    pnlRamp.BevelOuter := bvNone ;
    pnlRamp.TabOrder := 3 ;

    lblRampStep := TLabel.Create( pnlRamp ) ;
    lblRampStep.Parent := pnlRamp ;
    lblRampStep.Anchors := anchors ;
    lblRampStep.Top := 0 ;
    lblRampStep.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblRampStep, 16, 32 ) ;
    lblRampStep.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLSTEP ) ;
    lblRampStep.FocusControl := edtRampStep ;

    edtRampStep := TEdit.Create( pnlRamp ) ;
    edtRampStep.Parent := pnlRamp ;
    edtRampStep.Anchors := anchors ;
    edtRampStep.Top := 16 ;
    edtRampStep.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtRampStep, 16, 111 ) ;
    edtRampStep.Font.Charset := DEFAULT_CHARSET ;
    edtRampStep.Font.Color := clWindowText ;
    edtRampStep.Font.Height := -11 ;
    edtRampStep.Font.Name := 'Tahoma' ;
    edtRampStep.Font.Style := [] ;
    edtRampStep.ParentFont := False ;
    edtRampStep.TabOrder := 2 ;

    vvedtStep := TGIS_ValueValidatorEditHelper.Create( edtRampStep ) ;
    vvedtStep.Precision := 15 ;
    vvedtStep.MinVal := -GIS_MAX_SINGLE ;
    vvedtStep.MaxVal := GIS_MAX_SINGLE ;

    lblRampLegend := TLabel.Create( pnlRamp ) ;
    lblRampLegend.Parent := pnlRamp ;
    lblRampLegend.Anchors := anchors ;
    lblRampLegend.Top := 48 ;
    lblRampLegend.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblRampLegend, 16, 45 ) ;
    lblRampLegend.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLLEGSTEP ) ;
    lblRampLegend.FocusControl := edtRampLegend ;

    edtRampLegend := TEdit.Create( pnlRamp ) ;
    edtRampLegend.Parent := pnlRamp ;
    edtRampLegend.Anchors := anchors ;
    edtRampLegend.Top := 64 ;
    edtRampLegend.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtRampLegend, 16, 111 ) ;
    edtRampLegend.Font.Charset := DEFAULT_CHARSET ;
    edtRampLegend.Font.Color := clWindowText ;
    edtRampLegend.Font.Height := -11 ;
    edtRampLegend.Font.Name := 'Tahoma' ;
    edtRampLegend.Font.Style := [] ;
    edtRampLegend.ParentFont := False ;
    edtRampLegend.TabOrder := 6 ;

    vvedtLegend := TGIS_ValueValidatorEditHelper.Create( edtRampLegend ) ;
    vvedtLegend.Precision := 15 ;
    vvedtLegend.MinVal := -GIS_MAX_SINGLE ;
    vvedtLegend.MaxVal := GIS_MAX_SINGLE ;

    pnlChoose := TPanel.Create( Self ) ;
    pnlChoose.Parent := Self ;
    pnlChoose.BevelOuter := bvNone ;
    pnlChoose.Left := 8 ;
    pnlChoose.Top := 8 ;
    pnlChoose.Width := 353 ;
    pnlChoose.Height := 193 ;

    rdbChooseSimple := TRadioButton.Create( pnlChoose ) ;
    rdbChooseSimple.Parent := pnlChoose;
    rdbChooseSimple.Top := 70 ;
    rdbChooseSimple.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbChooseSimple, 100, -1 ) ;
    rdbChooseSimple.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_SIMPLE ) ;
    rdbChooseSimple.TabOrder := 1 ;
    rdbChooseSimple.Checked := True ;

    rdbChooseAdvanced := TRadioButton.Create( pnlChoose ) ;
    rdbChooseAdvanced.Parent := pnlChoose;
    rdbChooseAdvanced.Top := 95 ;
    rdbChooseAdvanced.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbChooseAdvanced, 100, -1 ) ;
    rdbChooseAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_ADVANCED ) ;
    rdbChooseAdvanced.TabOrder := 2 ;
    rdbChooseAdvanced.Checked := False ;

    pnlAnalyze := TPanel.Create( Self ) ;
    pnlAnalyze.Parent := Self ;
    pnlAnalyze.BevelOuter := bvNone ;
    pnlAnalyze.Left := 8 ;
    pnlAnalyze.Top := 0 ;
    pnlAnalyze.Width := 353 ;
    pnlAnalyze.Height := 193 ;

    lblAnalyzeMin := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeMin.Parent := pnlAnalyze ;
    lblAnalyzeMin.Anchors := anchors ;
    lblAnalyzeMin.Top := 32 ;
    lblAnalyzeMin.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblAnalyzeMin, 184, 64 ) ;
    lblAnalyzeMin.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLMIN ) ;
    lblAnalyzeMin.FocusControl := edtAnalyzeMin ;

    edtAnalyzeMin := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMin.Parent := pnlAnalyze ;
    edtAnalyzeMin.Anchors := anchors ;
    edtAnalyzeMin.Top := 48 ;
    edtAnalyzeMin.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMin, 184, 121 ) ;
    edtAnalyzeMin.TabOrder := 3 ;

    lblAnalyzeMax := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeMax.Parent := pnlAnalyze ;
    lblAnalyzeMax.Anchors := anchors ;
    lblAnalyzeMax.Top := 72 ;
    lblAnalyzeMax.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblAnalyzeMax, 184, 68 ) ;
    lblAnalyzeMax.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLMAX ) ;
    lblAnalyzeMax.FocusControl := edtAnalyzeMax ;

    edtAnalyzeMax := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMax.Parent := pnlAnalyze ;
    edtAnalyzeMax.Anchors := anchors ;
    edtAnalyzeMax.Top := 88 ;
    edtAnalyzeMax.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMax, 184, 121 ) ;
    edtAnalyzeMax.TabOrder := 4 ;

    chkAnalyzeMid := TCheckBox.Create( pnlAnalyze ) ;
    chkAnalyzeMid.Parent := pnlAnalyze ;
    chkAnalyzeMid.Anchors := anchors ;
    chkAnalyzeMid.Top := 112 ;
    chkAnalyzeMid.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkAnalyzeMid, 184, 137 ) ;
    chkAnalyzeMid.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CHKAVG ) ;
    chkAnalyzeMid.TabOrder := 5 ;
    chkAnalyzeMid.Checked := True ;
    chkAnalyzeMid.State := cbChecked ;
    chkAnalyzeMid.OnClick := actListUpdate ;

    edAnalyzetMid := TEdit.Create( pnlAnalyze ) ;
    edAnalyzetMid.Parent := pnlAnalyze ;
    edAnalyzetMid.Anchors := anchors ;
    edAnalyzetMid.Top := 128 ;
    edAnalyzetMid.Height := 21 ;
    PlaceControl( BiDiMode, nil, edAnalyzetMid, 184, 121 ) ;
    edAnalyzetMid.TabOrder := 6 ;

    vvedtMax := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMax ) ;
    vvedtMax.Precision := 15 ;
    vvedtMax.MinVal := -GIS_MAX_SINGLE ;
    vvedtMax.MaxVal := GIS_MAX_SINGLE ;

    vvedtMin := TGIS_ValueValidatorEditHelper.Create( edtAnalyzeMin ) ;
    vvedtMin.Precision := 15 ;
    vvedtMin.MinVal := -GIS_MAX_SINGLE ;
    vvedtMin.MaxVal := GIS_MAX_SINGLE ;

    vvedtMid := TGIS_ValueValidatorEditHelper.Create( edAnalyzetMid ) ;
    vvedtMid.Precision := 15 ;
    vvedtMid.MinVal := -GIS_MAX_SINGLE ;
    vvedtMid.MaxVal := GIS_MAX_SINGLE ;

    rdbAnalyzeUnique := TRadioButton.Create( pnlAnalyze ) ;
    rdbAnalyzeUnique.Parent := pnlAnalyze;
    rdbAnalyzeUnique.Top := 8 ;
    rdbAnalyzeUnique.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbAnalyzeUnique, 16, 161 ) ;
    rdbAnalyzeUnique.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBUNIQUE ) ;
    rdbAnalyzeUnique.TabOrder := 0 ;
    rdbAnalyzeUnique.OnClick := actListUpdate ;

    lblAnalyzeUnique := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeUnique.Parent := pnlAnalyze ;
    lblAnalyzeUnique.Top := 32 ;
    lblAnalyzeUnique.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblAnalyzeUnique, 16, 81 ) ;
    lblAnalyzeUnique.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLUNIQUE ) ;
    lblAnalyzeUnique.FocusControl := memAnalyzeUnique ;

    memAnalyzeUnique := TMemo.Create( pnlAnalyze ) ;
    memAnalyzeUnique.Parent := pnlAnalyze ;
    memAnalyzeUnique.Top := 48 ;
    memAnalyzeUnique.Height := 113 ;
    PlaceControl( BiDiMode, nil, memAnalyzeUnique, 16, 137 ) ;
    memAnalyzeUnique.ParentColor := True ;
    memAnalyzeUnique.ReadOnly := True ;
    memAnalyzeUnique.ScrollBars := ssVertical ;
    memAnalyzeUnique.TabOrder := 1 ;
    memAnalyzeUnique.WordWrap := False ;

    rdbAnalyzeContinous := TRadioButton.Create( pnlAnalyze ) ;
    rdbAnalyzeContinous.Parent := pnlAnalyze ;
    rdbAnalyzeContinous.Top := 8 ;
    rdbAnalyzeContinous.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbAnalyzeContinous, 184, 161 ) ;
    rdbAnalyzeContinous.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBCONTINOUS ) ;
    rdbAnalyzeContinous.TabOrder := 2 ;
    rdbAnalyzeContinous.OnClick := actListUpdate ;

    pnlStyleParams := TPanel.Create( Self ) ;
    pnlStyleParams.Parent := Self ;
    pnlStyleParams.BevelOuter := bvNone ;
    pnlStyleParams.Left := 8 ;
    pnlStyleParams.Top := 0 ;
    pnlStyleParams.Width := 353 ;
    pnlStyleParams.Height := 193 ;

    rdbStyleColorRange := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRange.Parent := pnlStyleParams;
    rdbStyleColorRange.Top := 10 ;
    rdbStyleColorRange.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRange, 8, 90 ) ;
    rdbStyleColorRange.Caption := _rsrc( GIS_RS_LEGEND_PRM_COLOR ) ;
    rdbStyleColorRange.TabOrder := 1 ;
    rdbStyleColorRange.Checked := True ;
    rdbStyleColorRange.OnClick := actListUpdate ;

    t := 35 ;
    lblStyleColorStart := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorStart.Parent := pnlStyleParams ;
    lblStyleColorStart.Top := t ;
    lblStyleColorStart.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorStart, 16, 121 ) ;
    lblStyleColorStart.Caption := _rsrc( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
    lblStyleColorStart.FocusControl := cmbStyleColorStart ;

    cmbStyleColorStart := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorStart.Parent := pnlStyleParams ;
    cmbStyleColorStart.Top := t + 16 ;
    cmbStyleColorStart.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorStart, 16, 100 ) ;
    cmbStyleColorStart.Fill( False, False ) ;
    cmbStyleColorStart.CustomEvent := doCustomColor ;

    lblStyleColorMid := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorMid.Parent := pnlStyleParams ;
    lblStyleColorMid.Top := t ;
    lblStyleColorMid.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorMid, 130, 121 ) ;
    lblStyleColorMid.Caption := _rsrc( GIS_RS_LEGEND_PRM_MIDDLECOLOR ) ;
    lblStyleColorMid.FocusControl := cmbStyleColorMid ;

    cmbStyleColorMid := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorMid.Parent := pnlStyleParams ;
    cmbStyleColorMid.Top := t + 16 ;
    cmbStyleColorMid.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorMid, 130, 100 ) ;
    cmbStyleColorMid.Fill( False, False ) ;
    cmbStyleColorMid.CustomEvent := doCustomColor ;

    lblStyleColorEnd := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorEnd.Parent := pnlStyleParams ;
    lblStyleColorEnd.Top := t ;
    lblStyleColorEnd.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorEnd, 244, 121 ) ;
    lblStyleColorEnd.Caption := _rsrc( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
    lblStyleColorEnd.FocusControl := cmbStyleColorEnd ;

    cmbStyleColorEnd := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorEnd.Parent := pnlStyleParams ;
    cmbStyleColorEnd.Top := t + 16 ;
    cmbStyleColorEnd.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorEnd, 244, 100 ) ;
    cmbStyleColorEnd.Fill( False, False ) ;
    cmbStyleColorEnd.CustomEvent := doCustomColor ;

    chkStyleColorRampAdd := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleColorRampAdd.Parent := pnlStyleParams ;
    chkStyleColorRampAdd.Anchors := anchors ;
    chkStyleColorRampAdd.Top := t + 8 + 21 + 20 ;
    chkStyleColorRampAdd.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleColorRampAdd, 16, 211 ) ;
    chkStyleColorRampAdd.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_ADD ) ;
    chkStyleColorRampAdd.TabOrder := 5 ;

    chkStyleColorHSL := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleColorHSL.Parent := pnlStyleParams ;
    chkStyleColorHSL.Anchors := anchors ;
    chkStyleColorHSL.Top := t + 8 + 21 + 20 ;
    chkStyleColorHSL.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleColorHSL, 244, 100 ) ;
    chkStyleColorHSL.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_USEHSL ) ;
    chkStyleColorHSL.TabOrder := 6 ;
    chkStyleColorHSL.Checked := True ;

    rdbStyleColorRamp := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRamp.Parent := pnlStyleParams;
    rdbStyleColorRamp.Top := 112 ;
    rdbStyleColorRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRamp, 8, 90 ) ;
    rdbStyleColorRamp.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMPS ) ;
    rdbStyleColorRamp.TabOrder := 1 ;
    rdbStyleColorRamp.OnClick := actListUpdate ;
    rdbStyleColorRamp.Checked := False ;

    cmbStyleRampList := TGIS_ColorRampComboBox.Create( pnlStyleParams ) ;
    cmbStyleRampList.Parent := pnlStyleParams ;
    cmbStyleRampList.Anchors := [akLeft, akTop, akRight] ;
    cmbStyleRampList.Top := 139 ;
    cmbStyleRampList.Height := 22 ;
    PlaceControl( BiDiMode, nil, cmbStyleRampList, 16, 330 ) ;
    cmbStyleRampList.TabOrder := 5 ;
    cmbStyleRampList.Fill ;
    cmbStyleRampList.ItemIndex := 0 ;

    chkReverseRamp := TCheckBox.Create( pnlStyleParams ) ;
    chkReverseRamp.Parent := pnlStyleParams ;
    chkReverseRamp.Anchors := anchors ;
    chkReverseRamp.Top := 165 ;
    chkReverseRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkReverseRamp, 20, 90 ) ;
    chkReverseRamp.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_REVERSE ) ;
    chkReverseRamp.Checked := False ;
    chkReverseRamp.State := cbUnChecked ;
    chkReverseRamp.TabOrder := 7 ;
    chkReverseRamp.OnClick := actReverseClick ;

    chkStyleDiscrete := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleDiscrete.Parent := pnlStyleParams ;
    chkStyleDiscrete.Anchors := anchors ;
    chkStyleDiscrete.Top := 165 ;
    chkStyleDiscrete.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleDiscrete, 130, 100 ) ;
    chkStyleDiscrete.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_DISCRETE ) ;
    chkStyleDiscrete.OnClick := actDiscreteClick ;
    chkStyleDiscrete.TabOrder := 8 ;

    chkShowAllRamps := TCheckBox.Create( pnlStyleParams ) ;
    chkShowAllRamps.Parent := pnlStyleParams ;
    chkShowAllRamps.Anchors := anchors ;
    chkShowAllRamps.Top := 165 ;
    chkShowAllRamps.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkShowAllRamps, 240, 100 ) ;
    chkShowAllRamps.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_SHOWALL ) ;
    chkShowAllRamps.OnClick := actShowAllClick ;
    chkShowAllRamps.TabOrder := 8 ;

    pnlFormulaAdvanced := TPanel.Create( Self ) ;
    pnlFormulaAdvanced.Parent := Self ;
    pnlFormulaAdvanced.BevelOuter := bvNone ;
    pnlFormulaAdvanced.Left := 8 ;
    pnlFormulaAdvanced.Top := 8 ;
    pnlFormulaAdvanced.Width := 353 ;
    pnlFormulaAdvanced.Height := 176 ;

    lblMethodAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblMethodAdvanced.Parent := pnlFormulaAdvanced ;
    lblMethodAdvanced.Top := 52 ;
    lblMethodAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblMethodAdvanced, 8, -1 ) ;
    lblMethodAdvanced.AutoSize := False ;
    lblMethodAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_METHOD ) ;
    lblMethodAdvanced.FocusControl := cmbMethodAdvanced ;

    cmbMethodAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbMethodAdvanced.Parent := pnlFormulaAdvanced ;
    cmbMethodAdvanced.Top := 68 ;
    cmbMethodAdvanced.Height := 21 ;
    cmbMethodAdvanced.Style := TComboBoxStyle.csDropDownList ;
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
    lblClassesAdvanced.Top := 52 ;
    lblClassesAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblClassesAdvanced, 182, 80 ) ;
    lblClassesAdvanced.AutoSize := False ;
    lblClassesAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_CLASSES ) ;
    lblClassesAdvanced.FocusControl := cmbClassesAdvanced ;

    cmbClassesAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbClassesAdvanced.Parent := pnlFormulaAdvanced ;
    cmbClassesAdvanced.Top := 68 ;
    cmbClassesAdvanced.Height := 21 ;
    cmbClassesAdvanced.Style := TComboBoxStyle.csDropDownList ;
    PlaceControl( BiDiMode, nil, cmbClassesAdvanced, 182, 75 ) ;
    cmbClassesAdvanced.TabOrder := 0 ;
    for i := 1 to 9 do
      cmbClassesAdvanced.Items.Add( IntToStr( i ) ) ;
    cmbClassesAdvanced.ItemIndex := 4 ;
    cmbClassesAdvanced.OnChange := actListUpdate ;

    lblIntervalAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    lblIntervalAdvanced.Top := 52 ;
    lblIntervalAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblIntervalAdvanced, 270, 75 ) ;
    lblIntervalAdvanced.AutoSize := False ;
    lblIntervalAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_INTERVAL ) ;
    lblIntervalAdvanced.FocusControl := cmbIntervalAdvanced ;

    cmbIntervalAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    cmbIntervalAdvanced.Top := 68 ;
    cmbIntervalAdvanced.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbIntervalAdvanced, 270, 75 ) ;
    cmbIntervalAdvanced.TabOrder := 0 ;
    cmbIntervalAdvanced.Text := '100' ;
    cmbIntervalAdvanced.OnChange := actListUpdate ;

    lblBandAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblBandAdvanced.Parent := pnlFormulaAdvanced ;
    lblBandAdvanced.Top := 97 ;
    lblBandAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblBandAdvanced, 8, 80 ) ;
    lblBandAdvanced.AutoSize := False ;
    lblBandAdvanced.Caption := _rsrc( GIS_RS_LEGEND_PRM_BAND ) ;
    lblBandAdvanced.FocusControl := cmbBandAdvanced ;

    cmbBandAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbBandAdvanced.Parent := pnlFormulaAdvanced ;
    cmbBandAdvanced.Top := 114 ;
    cmbBandAdvanced.Height := 21 ;
    cmbBandAdvanced.Style := TComboBoxStyle.csDropDownList ;
    PlaceControl( BiDiMode, nil, cmbBandAdvanced, 8, 75 ) ;
    cmbBandAdvanced.TabOrder := 0 ;
    cmbBandAdvanced.OnChange := actListUpdate ;

    btnNextPage := TButton.Create( Self ) ;
    btnNextPage.Parent := Self ;
    if BIDiMode = bdRightToLeft then
      btnNextPage.Anchors := [akLeft, akBottom]
    else
      btnNextPage.Anchors := [akRight, akBottom] ;
    btnNextPage.Height := 25 ;
    btnNextPage.Top := Self.ClientHeight - btnNextPage.Height - 8 ;
    PlaceControl( BiDiMode, btnCancel, btnNextPage, -8, btnCancel.Width ) ;
    btnNextPage.Caption := _rsrc( GIS_RS_BTN_NEXT ) ;
    btnNextPage.OnClick := btnNextPageClick ;

    btnPreviousPage := TButton.Create( Self ) ;
    btnPreviousPage.Parent := Self ;
    if BIDiMode = bdRightToLeft then
      btnPreviousPage.Anchors := [akLeft, akBottom]
    else
      btnPreviousPage.Anchors := [akRight, akBottom] ;
    btnPreviousPage.Height := 25 ;
    btnPreviousPage.Top := Self.ClientHeight - btnPreviousPage.Height - 8 ;
    PlaceControl( BiDiMode, btnNextPage, btnPreviousPage, -8, btnNextPage.Width ) ;
    btnPreviousPage.Caption := _rsrc( GIS_RS_BTN_PREVIOUS ) ;
    btnPreviousPage.OnClick := btnPreviousPageClick ;

    btnOK.Top := btnNextPage.Top - btnOK.Height ;
    btnOK.Left := btnNextPage.Left ;
    btnOK.OnClick := btnOKClick ;
    btnCancel.Top := btnNextPage.Top - btnOK.Height ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    btnHelp.TabOrder := 0 ;
    btnOK.TabOrder := 3 ;
    btnCancel.TabOrder := 8 ;
    btnPreviousPage.TabOrder := 1 ;
    btnNextPage.TabOrder := 2 ;
  end ;

  procedure TGIS_ControlLegendGridWiz.showForm ;
  begin
    pnlChoose.Visible  := True ;

    pnlAnalyze.Visible  := False ;
    pnlAnalyze.Left     := pnlChoose.Left  ;
    pnlAnalyze.Top      := pnlChoose.Top   ;

    pnlRamp.Visible  := False ;
    pnlRamp.Left     := pnlChoose.Left  ;
    pnlRamp.Top      := pnlChoose.Top   ;

    pnlStyleParams.Visible  := False ;
    pnlStyleParams.Left     := pnlChoose.Left  ;
    pnlStyleParams.Top      := pnlChoose.Top   ;

    pnlFormulaAdvanced.Visible := False ;
    pnlFormulaAdvanced.Left    := pnlChoose.Left  ;
    pnlFormulaAdvanced.Top     := pnlChoose.Top   ;

    btnOK.Visible      := False            ;
    btnOK.Left         := btnNextPage.Left ;
    btnOK.Top          := btnNextPage.Top  ;
    btnCancel.Top      := btnNextPage.Top  ;

    ClientWidth  := pnlChoose.Left + pnlChoose.Width + 8 ;
    ClientHeight := pnlChoose.Top + pnlChoose.Height + 8 +
                    btnCancel.Height + 8 ;

    actListUpdate( nil ) ;
  end;

  procedure TGIS_ControlLegendGridWiz.afterPPIChanged ;
  begin

  end;

  procedure TGIS_ControlLegendGridWiz.actDiscreteClick(Sender: TObject);
  var
    idx : Integer ;
  begin
    idx := cmbStyleRampList.ItemIndex ;
    if chkStyleDiscrete.Checked then
      cmbStyleRampList.Mode := TGIS_ColorMapMode.Discrete
    else
      cmbStyleRampList.Mode := TGIS_ColorMapMode.Continuous ;

    cmbStyleRampList.ItemIndex := idx ;
  end ;

  procedure TGIS_ControlLegendGridWiz.actShowAllClick(Sender: TObject);
  begin
    if chkShowAllRamps.Checked then begin
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
    cmbStyleRampList.Reverse := chkReverseRamp.Checked ;
    cmbStyleRampList.ItemIndex := idx ;
  end ;

  function TGIS_ControlLegendGridWiz.Execute(
    const _layer  : TGIS_LayerPixel         ;
    const _params : TGIS_ParamsSectionPixel ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    i : Integer ;
  begin
    Assert( Assigned( _layer ) ) ;
    objLayer  := _layer  ;

    Assert( Assigned( _params ) ) ;
    objParams := _params ;

    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    btnOK.Visible  := False ;

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

    Result := ShowModal ;
  end ;

  function TGIS_ControlLegendGridWiz.Execute(
    const _layer  : TGIS_LayerPixel         ;
    const _params : TGIS_ParamsSectionPixel
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, _params, hlp ) ;
  end;


//==================================== END =====================================
end.

