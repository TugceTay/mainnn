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
  Legend component. Vector wizard.
}

unit Lider.CG.GIS.VCL.GeoControlLegendVectorWiz ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlLegendVectorWiz"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  System.SysUtils,
  System.Math,
  System.Classes,
  System.Variants,

  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ActnList,
  VCL.ExtCtrls,
  {$IFNDEF GIS_NODB}
     Data.DB,
  {$ENDIF}

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoLayerPixel,
  Lider.CG.GIS.VCL.GeoControlColor,
  Lider.CG.GIS.VCL.GeoControlVarious,
  Lider.CG.GIS.VCL.GeoModalForm;

type

  /// <summary>
  ///   Visual form for managing vector rendering wizard.
  /// </summary>
  TGIS_ControlLegendVectorWiz = class( TGIS_ModalForm )
    // buttons
    btnNextPage: TButton;
    btnPreviousPage: TButton;

    // simple/advanced classification choice
    pnlChoose: TPanel;
    rdbChooseSimple: TRadioButton;
    rdbChooseAdvanced: TRadioButton;

    // simple -> formula and limited/full scan choice
    pnlFormula: TPanel;
    lblFormula: TLabel;
    cmbFormula: TComboBox;
    rdbLimitedScan: TRadioButton;
    rdbFullScan: TRadioButton;

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
    chkAnalyzeAvg: TCheckBox;
    edtAnalyzeAvg: TEdit;
    chkAnalyzeLog: TCheckBox;

    // features and render type choice
    pnlRender: TPanel;
    grpRenderFeature: TGroupBox;
    rdbRenderMarker: TRadioButton;
    rdbRenderLine: TRadioButton;
    rdbRenderArea: TRadioButton;
    grpRenderValue: TGroupBox;
    rdbRenderSize: TRadioButton;
    rdbRenderOutlineWidth: TRadioButton;
    rdbRenderColor: TRadioButton;
    rdbRenderOutlineColor: TRadioButton;
    chkStyleShowLegend: TCheckBox;

    // colors/ramps choice
    pnlStyleParams: TPanel;
    rdbStyleSize: TRadioButton;
    lblStyleSizeStart: TLabel;
    cmbStyleSizeStart: TGIS_SizeComboBox;
    lblStyleSizeEnd: TLabel;
    cmbStyleSizeEnd: TGIS_SizeComboBox;
    rdbStyleColorRange: TRadioButton;
    lblStyleColorStart: TLabel;
    cmbStyleColorStart: TGIS_ColorComboBox ;
    rdbStyleColorRamp: TRadioButton;
    lblStyleColorEnd: TLabel;
    cmbStyleColorEnd: TGIS_ColorComboBox ;
    cmbStyleRampList: TGIS_ColorRampComboBox;
    chkReverseRamp: TCheckBox;
    chkStyleDiscrete: TCheckBox;
    chkShowAllRamps: TCheckBox;

    // adcanced
    pnlFormulaAdvanced: TPanel;
    lblFormulaAdvanced: TLabel;
    cmbFormulaAdvanced: TComboBox;
    lblMethodAdvanced: TLabel;
    cmbMethodAdvanced: TComboBox;
    lblClassesAdvanced: TLabel;
    cmbClassesAdvanced: TComboBox;
    lblIntervalAdvanced: TLabel;
    cmbIntervalAdvanced: TComboBox;

    procedure actListUpdate(Sender: TObject);
    procedure actNextPageExecute(Sender: TObject);
    procedure actPreviousPageExecute(Sender: TObject);
    procedure actDiscreteClick(Sender: TObject);
    procedure actReverseClick(Sender: TObject);
    procedure actShowAllClick(Sender: TObject);
    procedure btnNextPageClick(Sender: TObject);
    procedure btnPreviousPageClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject); override;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    objLayer            : TGIS_LayerVector ;
    objParams           : TGIS_ParamsList  ;
    iPage               : Integer          ;
    valMin              : Variant          ;
    valMax              : Variant          ;
    valCnt              : Integer          ;
    bStrings            : Boolean          ;
    bDates              : Boolean          ;
    lstUnique           : TGIS_VariantList ;
    overUnique          : Boolean          ;
    overRecords         : Boolean          ;
    oType               : TGIS_ShapeType   ;
    iUniqueLimit        : Integer          ;
    iUniqueSearchLimit  : Integer          ;
    iMode               : Integer          ;
    lastColorSchemas    : TGIS_ColorSchemas ;
    function doCustomSize           ( _sender : TObject ;
                                      _value  : String
                                    ) : String ;
    function doCustomColor          ( _sender : TObject ;
                                      _value  : String
                                    ) : String ;
    procedure doClassifySimple ;
    function  doClassifyAdvanced : Boolean ;
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
    ///   Execute dialog on a given layer and parameters.
    /// </summary>
    /// <param name="_layer">
    ///   layer to be analyzed
    /// </param>
    /// <param name="_params">
    ///   parameters to be altered
    /// </param>
    /// <param name="_type">
    ///   type of expected render feature or TGIS_ShapeType.Unknown
    /// </param>
    /// <param name="_onhelp">
    ///   help notification function; if assigned the help button will be
    ///   visible and help support will be enabled
    /// </param>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute  ( const _layer  : TGIS_LayerVector ;
                        const _type   : TGIS_ShapeType   ;
                        const _params : TGIS_ParamsList  ;
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
    /// <param name="_type">
    ///   type of expected render feature or TGIS_ShapeType.Unknown
    /// </param>
    /// <returns>
    ///   Show modal results: mrCancel or mrOK.
    /// </returns>
    function Execute  ( const _layer  : TGIS_LayerVector ;
                        const _type   : TGIS_ShapeType   ;
                        const _params : TGIS_ParamsList
                      ) : Integer ; overload;
  public

    /// <summary>
    ///   Limit for unique values.
    /// </summary>
    property UniqueLimit        : Integer read  iUniqueLimit
                                          write iUniqueLimit ;

    /// <summary>
    ///   Limit for unique values.
    /// </summary>
    property UniqueSearchLimit  : Integer read  iUniqueSearchLimit
                                          write iUniqueSearchLimit ;
  end ;

var
  /// <summary>
  ///   Global value of type TGIS_ControlLegendVectorWiz.
  /// </summary>
  GIS_ControlLegendVectorWiz: TGIS_ControlLegendVectorWiz;


//##############################################################################
implementation

uses
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoSqlQuery,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoXmlDoc,
  Lider.CG.GIS.GeoClassification,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.PVL.GeoPvl,
  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.PVL.GeoControlFieldFactor,
  Lider.CG.GIS.VCL.GeoControlStatistics,
  Lider.CG.GIS.VCL.GeoControlHelper,
  Lider.CG.GIS.PVL.GeoControlSizeForm;

const
  // Maximum number of unique values.
  UNIQUE_LIMIT = 256 ;

  // Maximum number of records searched for unique values.
  UNIQUE_LIMIT_MAX_RECORDS = 16384 ;

//==============================================================================
// Form events & methods
//==============================================================================

  procedure TGIS_ControlLegendVectorWiz.FormCreate(Sender: TObject);
  begin
    lstUnique := TGIS_VariantList.Create ;
    iUniqueLimit       := UNIQUE_LIMIT ;
    iUniqueSearchLimit := UNIQUE_LIMIT_MAX_RECORDS ;

    iPage     := -1 ;
    iMode     := 0 ;

    bDates   := False ;
    bStrings := False ;
  end;

  procedure TGIS_ControlLegendVectorWiz.FormDestroy(Sender: TObject);
  begin
    FreeObject( lstUnique ) ;
  end;

//==============================================================================
// Control events & methods
//==============================================================================

  procedure TGIS_ControlLegendVectorWiz.actListUpdate(
    Sender : TObject
  ) ;
  var
    bl  : Boolean ;
    tag : Double ;

    procedure check_render ;
    begin
      rdbRenderMarker.Enabled :=
         ( TGIS_ShapeType.Point in objLayer.SupportedShapesAll ) or
         ( TGIS_ShapeType.MultiPoint in objLayer.SupportedShapesAll ) ;
      rdbRenderLine.Enabled :=
         TGIS_ShapeType.Arc in objLayer.SupportedShapesAll ;
      rdbRenderArea.Enabled :=
         TGIS_ShapeType.Polygon in objLayer.SupportedShapesAll ;
      rdbRenderSize.Enabled :=
         rdbRenderMarker.Checked or rdbRenderLine.Checked ;
    end ;

    procedure check_method ;
    var
      method : String ;
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

  begin
    case iPage of
      0 : // Formula page
          begin
            if iMode = 0 then
              btnNextPage.Enabled := not IsStringEmpty( Trim( cmbFormula.Text ) )
            else begin
              btnNextPage.Enabled := ( cmbFormulaAdvanced.ItemIndex >= 0 ) and
                                     ( cmbMethodAdvanced.ItemIndex >= 0 ) ;
              check_method ;
            end ;
          end ;
      1 : // Analyze page
          begin
            if iMode = 0 then begin
              bl := rdbAnalyzeUnique.Enabled ;
                lblAnalyzeUnique.Visible := bl ;
                memAnalyzeUnique.Visible := bl ;

              bl := rdbAnalyzeUnique.Checked ;
                lblAnalyzeUnique.Enabled := bl ;
                memAnalyzeUnique.Enabled := bl ;

              bl := rdbAnalyzeContinous.Enabled ;
                lblAnalyzeMin.Visible    := bl ;
                edtAnalyzeMin.Visible    := bl ;
                lblAnalyzeMax.Visible    := bl ;
                edtAnalyzeMax.Visible    := bl ;
                chkAnalyzeAvg.Visible    := bl ;
                edtAnalyzeAvg.Visible    := bl ;
                chkAnalyzeLog.Visible    := bl ;

              bl := rdbAnalyzeContinous.Checked ;
                lblAnalyzeMin.Enabled    := bl ;
                edtAnalyzeMin.Enabled    := bl ;
                lblAnalyzeMax.Enabled    := bl ;
                edtAnalyzeMax.Enabled    := bl ;
                chkAnalyzeAvg.Enabled    := bl ;
                edtAnalyzeAvg.Enabled    := bl and chkAnalyzeAvg.Checked ;
                chkAnalyzeLog.Enabled    := bl ;
            end
            else begin
              check_render
            end ;
          end ;
      2 : // Render page
          begin
            if iMode = 0 then
              check_render
            else begin
              if rdbStyleColorRange.Checked then begin
                lblStyleSizeStart.Enabled  := False ;
                lblStyleSizeEnd.Enabled    := False ;
                cmbStyleSizeStart.Enabled  := False ;
                cmbStyleSizeEnd.Enabled    := False ;
                lblStyleColorStart.Enabled := True ;
                lblStyleColorEnd.Enabled   := True ;
                cmbStyleColorStart.Enabled := True ;
                cmbStyleColorEnd.Enabled   := True ;
                cmbStyleRampList.Enabled   := False ;
                chkReverseRamp.Enabled     := False ;
                chkStyleDiscrete.Enabled   := False ;
                chkShowAllRamps.Enabled    := False ;
              end
              else if rdbStyleColorRamp.Checked then begin
                lblStyleSizeStart.Enabled  := False ;
                lblStyleSizeEnd.Enabled    := False ;
                cmbStyleSizeStart.Enabled  := False ;
                cmbStyleSizeEnd.Enabled    := False ;
                lblStyleColorStart.Enabled := False ;
                lblStyleColorEnd.Enabled   := False ;
                cmbStyleColorStart.Enabled := False ;
                cmbStyleColorEnd.Enabled   := False ;
                cmbStyleRampList.Enabled   := True ;
                chkReverseRamp.Enabled     := True ;
                chkStyleDiscrete.Enabled   := True ;
                chkShowAllRamps.Enabled    := True ;
              end
              else if rdbStyleSize.Checked then begin
                lblStyleSizeStart.Enabled  := True ;
                lblStyleSizeEnd.Enabled    := True ;
                cmbStyleSizeStart.Enabled  := True ;
                cmbStyleSizeEnd.Enabled    := True ;
                lblStyleColorStart.Enabled := False ;
                lblStyleColorEnd.Enabled   := False ;
                cmbStyleColorStart.Enabled := False ;
                cmbStyleColorEnd.Enabled   := False ;
                cmbStyleRampList.Enabled   := False ;
                chkReverseRamp.Enabled     := False ;
                chkStyleDiscrete.Enabled   := False ;
                chkShowAllRamps.Enabled    := False ;
              end
            end ;
          end ;
    end ;

    btnPreviousPage.Enabled := iPage >= 0 ;
  end;

  procedure TGIS_ControlLegendVectorWiz.actNextPageExecute(Sender: TObject);
  var
    lst       : TStringList ;
    first_run : Boolean     ;
    limit     : Integer     ;
    ext       : TGIS_Extent ;
    method    : String ;
    frm_stats : TGIS_ControlStatistics ;

    // calculate formula
    function calc_formula : Boolean ;
    var
      i    : Integer       ;
      shp  : TGIS_Shape    ;
      sql  : TGIS_SqlQuery ;
      oval : Variant       ;

      function add_unique( const _val : Variant ) : Integer ;
      var
        k    : Integer ;
        stmp : String  ;
      begin
        stmp := _val ;
        if not lst.Find( _val, k ) then begin
          if lst.Count < iUniqueLimit then begin
            lst.Add( stmp ) ;
            lstUnique.Add( _val ) ;
          end
          else begin
            overUnique := True ;
          end ;
        end ;
        Result := lstUnique.Count ;
      end ;

    begin
      Result := False ;
      valMin      :=  GIS_MAX_DOUBLE ;
      valMax      := -GIS_MAX_DOUBLE ;
      valCnt      := 0 ;
      overUnique  := False ;
      overRecords := False ;

      lst := TStringList.Create ;
      lst.Sorted := True ;
      lstUnique.Clear ;
      try
        sql := TGIS_SqlQuery.Create ;
        try
          sql.Prepare( cmbFormula.Text ) ;

          first_run := True ;
          if rdbLimitedScan.Checked then begin
            limit := iUniqueSearchLimit;
            ext   := objLayer.Viewer.Ref.VisibleExtent ;
          end
          else begin
            limit := GIS_MAX_INTEGER ;
            ext := objLayer.Viewer.Ref.Extent ;
          end ;

          shp := objLayer.FindFirst(
                   ext,
                   TGIS_ParamsSectionVector( objParams.Items[0] ).Query
                 ) ;

          if Assigned( shp ) then begin
            sql.Parse( shp, 0 ) ;
            if objLayer.MustCalculateStatistics then begin
              frm_stats := TGIS_ControlStatistics.Create( Self ) ;
              try
                if ( frm_stats.Execute( objLayer, pOnHelp ) = mrCancel ) then
                  exit ;
              finally
                FreeObject( frm_stats ) ;
              end ;
            end ;
          end ;

          while Assigned( shp ) and ( not overRecords ) do
          begin
            Dec( limit ) ;
            if limit <= 0 then
              overRecords := True ;

            oval := sql.Parse( shp, 0 ) ;

            if ( not VarIsNull( oval ) ) and ( not VarIsEmpty( oval ) ) then
            begin
              Inc( valCnt ) ;
              if IsVariantString( oval ) then begin
                add_unique( oval ) ;
                if first_run then
                  bStrings := True ;
              end
              else if GetVariantType( oval ) = TGIS_VariantType.DateTime then begin
                if first_run then
                  bDates := True ;
                add_unique( oval ) ;
                oval := Double(oval) ;
                if oval < valMin then valMin := oval ;
                if oval > valMax then valMax := oval ;
              end
              else begin
                if first_run then
                  bStrings := False ;
                add_unique( oval ) ;
                oval := Double(oval) ;
                if oval < valMin then valMin := oval ;
                if oval > valMax then valMax := oval ;
              end ;

              first_run := False ;
            end ;

            shp := objLayer.FindNext ;
          end ;
        finally
          sql.Free ;
        end ;
      finally
        lst.Free ;
      end ;

      lstUnique.Sort ;

      if bDates then begin
        edtAnalyzeMin.Text := DateTimeToStr( TDateTime(valMin) ) ;
        edtAnalyzeAvg.Text := DateTimeToStr( TDateTime((valMax + valMin) / 2)  ) ;
        edtAnalyzeMax.Text := DateTimeToStr( TDateTime(valMax) ) ;
      end
      else begin
        edtAnalyzeMin.Text := FloatToStr( valMin ) ;
        edtAnalyzeAvg.Text := FloatToStr( (valMax + valMin) / 2  ) ;
        edtAnalyzeMax.Text := FloatToStr( valMax ) ;
      end ;

      memAnalyzeUnique.Lines.BeginUpdate ;
      try
        memAnalyzeUnique.Lines.Clear ;
        if overUnique then
          memAnalyzeUnique.Lines.Add( Format( GIS_RS_LEGEND_WIZARD_VALLIMIT,
                                              [ iUniqueLimit ]
                                            )
                                    ) ;
        if overRecords then
          memAnalyzeUnique.Lines.Add( Format( GIS_RS_LEGEND_WIZARD_RECLIMIT,
                                              [  iUniqueSearchLimit ]
                                            )
                                    )
        else
          memAnalyzeUnique.Lines.Add( Format( GIS_RS_LEGEND_WIZARD_RECLIMIT,
                                              [  valCnt ]
                                            )
                                    ) ;

        for i := 0 to lstUnique.Count -1  do
          memAnalyzeUnique.Lines.Add( lstUnique[i] ) ;
      finally
        memAnalyzeUnique.Lines.EndUpdate ;
      end ;

      rdbAnalyzeUnique.Checked    := False ;
      rdbAnalyzeContinous.Checked := False ;
      rdbAnalyzeContinous.Enabled := not bStrings  ;

      if bStrings or ( not ( overUnique or overRecords ) ) then
        rdbAnalyzeUnique.Checked := True
      else
        rdbAnalyzeContinous.Checked := True ;

      Result := True ;
    end ;

    procedure set_render_type ;
    begin
      case oType of
        TGIS_ShapeType.Point,
        TGIS_ShapeType.MultiPoint : rdbRenderMarker.Checked := True ;
        TGIS_ShapeType.Arc        : rdbRenderLine.Checked   := True ;
        TGIS_ShapeType.Polygon,
        TGIS_ShapeType.MultiPatch : rdbRenderArea.Checked   := True ;
        else                        begin
                                      Assert( False, GIS_RS_ERR_UNTESTED ) ;
                                      rdbRenderMarker.Checked := True ;
                                    end ;
      end ;
    end ;

    procedure set_render_style ;
    begin
      if rdbRenderSize.Checked or rdbRenderOutlineWidth.Checked then begin
        rdbStyleSize.Checked        := True ;
        rdbStyleSize.Enabled        := True ;
        rdbStyleColorRange.Enabled  := False ;
        rdbStyleColorRamp.Enabled   := False ;
        cmbStyleSizeStart.Enabled   := True ;
        cmbStyleSizeEnd.Enabled     := True ;
        cmbStyleColorStart.Enabled  := False ;
        cmbStyleColorEnd.Enabled    := False ;
        cmbStyleRampList.Enabled    := False ;
        chkReverseRamp.Enabled      := False ;
        chkStyleDiscrete.Enabled    := False ;
        chkShowAllRamps.Enabled     := False ;
        cmbStyleSizeStart.Value     := 'SIZE:1 twips' ;
        cmbStyleSizeEnd.Value       := 'SIZE:24 pt' ;
      end
      else if rdbRenderColor.Checked or rdbRenderOutlineColor.Checked then begin
        rdbStyleColorRange.Enabled  := True ;
        rdbStyleColorRamp.Enabled   := True ;
        rdbStyleSize.Enabled        := False ;
        rdbStyleColorRange.Checked  := True ;
        cmbStyleSizeStart.Enabled   := False ;
        cmbStyleSizeEnd.Enabled     := False ;
        cmbStyleColorStart.Enabled  := True ;
        cmbStyleColorEnd.Enabled    := True ;
        cmbStyleRampList.Enabled    := True ;
        chkReverseRamp.Enabled      := True ;
        chkStyleDiscrete.Enabled    := True ;
        chkShowAllRamps.Enabled     := True ;
        cmbStyleColorStart.Value    := 'ARGB:FFEDF8E9' ;
        cmbStyleColorEnd.Value      := 'ARGB:FF008000' ;

        if iMode = 0 then begin
          if rdbAnalyzeUnique.Checked then begin
            rdbStyleColorRange.Enabled := False ;
            rdbStyleColorRamp.Checked  := True ;
            cmbStyleColorStart.Enabled := False ;
            cmbStyleColorEnd.Enabled   := False ;
          end
          else begin
            rdbStyleColorRamp.Enabled  := False ;
            rdbStyleColorRange.Checked := True ;
            cmbStyleRampList.Enabled   := False ;
            chkReverseRamp.Enabled     := False ;
            chkStyleDiscrete.Enabled   := False ;
            chkShowAllRamps.Enabled    := False ;
          end ;
        end ;
      end ;
    end ;

  begin
    case iPage of
      -1 : // Choose page
          begin
            if rdbChooseSimple.Checked then begin
              iMode := 0 ;
              pnlFormulaAdvanced.Visible := False ;
              pnlFormula.Visible := True  ;
              pnlAnalyze.Visible := False  ;
            end
            else begin
              iMode := 1 ;
              pnlFormulaAdvanced.Visible := True ;
              pnlFormula.Visible := False  ;
              pnlAnalyze.Visible := False  ;
            end ;
            pnlChoose.Visible := False ;

            Inc( iPage ) ;
          end ;
      0 : // Formula page
          begin
            if iMode = 0 then begin
              if not calc_formula then
                exit ;

              pnlFormula.Visible := False ;
              pnlAnalyze.Visible := True  ;
            end
            else begin
              pnlFormulaAdvanced.Visible := False ;
              pnlRender.Visible   := True  ;
              set_render_type ;

              // reset ramp params to default
              chkReverseRamp.Checked := False ;
              chkStyleDiscrete.Checked := False ;
              chkShowAllRamps.Checked := False ;

              method := cmbMethodAdvanced.Items[cmbMethodAdvanced.ItemIndex] ;
              if ( method = GIS_RS_CLASSIFY_METHOD_QR ) or
                 ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
                 ( method = GIS_RS_CLASSIFY_METHOD_SDC )
              then
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Diverging]
              else
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;

              cmbStyleRampList.ItemIndex := 0 ;
            end ;
            Inc( iPage ) ;
          end ;
      1 : // Analyze page
          begin
            if iMode = 0 then begin

              // too many uniques warning
              if rdbAnalyzeUnique.Checked and overUnique then
                if MessageDlg(
                     Format( _rsrc( GIS_RS_LEGEND_TOO_MANY_UNIQUE ), [iUniqueLimit] ),
                     TMsgDlgType.mtConfirmation,
                     [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo],
                     0,
                     TMsgDlgBtn.mbNo
                   ) = mrNo
                then
                  exit ;

              if IsStringEmpty( edtAnalyzeMin.Text ) then
                exit ;
              if IsStringEmpty( edtAnalyzeMax.Text )  then
                exit ;
              if chkAnalyzeAvg.Checked and IsStringEmpty( edtAnalyzeAvg.Text ) then
                exit ;

              chkReverseRamp.Checked := False ;
              chkShowAllRamps.Checked := False ;

              if rdbAnalyzeUnique.Checked then begin
                chkStyleDiscrete.Checked := True ;
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
                // default is 'Unique'
                cmbStyleRampList.ItemIndex := cmbStyleRampList.GetCount - 4 ;
              end
              else begin
                // will be used in future (color ramps in renderer)
                chkStyleDiscrete.Checked := False ;
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
              end ;

              Inc( iPage ) ;
              pnlAnalyze.Visible  := False ;
              pnlRender.Visible   := True  ;
              btnNextPage.Visible := True ;
              btnOK.Visible       := False  ;

              set_render_type ;
            end
            else begin
              Inc( iPage ) ;
              pnlRender.Visible      := False ;
              pnlStyleParams.Visible := True  ;
              btnNextPage.Visible    := False ;
              btnOK.Visible          := True  ;

              set_render_style ;
            end ;
          end ;
      2 : //
          begin
            if iMode = 0 then begin
              Inc( iPage ) ;
              pnlRender.Visible      := False ;
              pnlStyleParams.Visible := True  ;
              btnNextPage.Visible    := False ;
              btnOK.Visible          := True  ;

              set_render_style ;
            end ;
          end ;
    end ;
  end;

  procedure TGIS_ControlLegendVectorWiz.btnNextPageClick(Sender: TObject);
  begin
    actNextPageExecute(Sender);
    actListUpdate(Sender);
  end ;

  procedure TGIS_ControlLegendVectorWiz.actPreviousPageExecute(Sender: TObject);
  begin
    case iPage of
      -1 : // Formula page
          begin
            exit ; // we are already on a first page
          end ;
      0 : // Formula page
          begin
            Dec( iPage ) ;
            pnlChoose.Visible   := True  ;
            pnlAnalyze.Visible  := False ;
            pnlFormula.Visible  := False ;
            pnlFormulaAdvanced.Visible  := False ;
          end ;
      1 : // Analyze page
          begin
            Dec( iPage ) ;
            if iMode = 0 then begin
              pnlAnalyze.Visible  := False ;
              pnlFormula.Visible  := True  ;
            end
            else begin
              pnlRender.Visible  := False ;
              pnlFormulaAdvanced.Visible  := True  ;
            end;
          end ;
      2 : // Render page
          begin
            Dec( iPage ) ;
            if iMode = 0 then begin
              pnlRender.Visible   := False ;
              pnlAnalyze.Visible  := True  ;
              btnOK.Visible       := False ;
              btnNextPage.Visible := True  ;
            end
            else begin
              pnlRender.Visible       := True ;
              pnlStyleParams.Visible  := False ;
              btnOK.Visible           := False ;
              btnNextPage.Visible     := True  ;
            end ;
          end ;
      3 : // Render page
          begin
            Dec( iPage ) ;
            if iMode = 0 then begin
              pnlRender.Visible       := True ;
              pnlStyleParams.Visible  := False ;
              btnOK.Visible           := False ;
              btnNextPage.Visible     := True  ;
            end
          end ;
    end ;
  end;

  procedure TGIS_ControlLegendVectorWiz.actDiscreteClick(Sender: TObject);
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

  procedure TGIS_ControlLegendVectorWiz.actReverseClick(Sender: TObject);
  var
    idx : Integer ;
  begin
    idx := cmbStyleRampList.ItemIndex ;
    cmbStyleRampList.Reverse := chkReverseRamp.Checked ;
    cmbStyleRampList.ItemIndex := idx ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.actShowAllClick(Sender: TObject);
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

  procedure TGIS_ControlLegendVectorWiz.btnPreviousPageClick(Sender: TObject);
  begin
    actPreviousPageExecute(Sender);
    actListUpdate(Sender);
  end ;

  procedure TGIS_ControlLegendVectorWiz.doClassifySimple ;
  var
    prm          : TGIS_ParamsList          ;
    sec          : TGIS_ParamsSectionVector ;
    i            : Integer                  ;
    size         : Integer                  ;
    color        : TGIS_Color               ;
    colormap_arr : TGIS_ColorMapArray       ;
    color_id     : Integer                  ;
    colors_count : Integer                  ;
    tmp_query    : String                   ;
  begin
    prm := TGIS_ParamsList.Create ;
    try
      prm.Assign( objParams ) ;

      for i:= prm.Count - 1 downto 1 do begin
        prm.Selected := i ;
        prm.Delete ;
      end ;

      if iMode = 0 then begin
        // Simple / Unique values
        if rdbAnalyzeUnique.Checked then begin
          sec := TGIS_ParamsSectionVector( prm.Items[0] ) ;
          sec.Visible := True ;
          sec.Render.Expression := '' ;

          // use color ramps instead generating random colors
          colormap_arr := cmbStyleRampList.Value( lstUnique.Count ) ;
          colors_count := length( colormap_arr ) ;

          for i := 0 to lstUnique.Count - 1 do begin
            tmp_query := '' ;

            if bStrings then
              tmp_query := Format(
                '%s=%s',
                [ cmbFormula.Text, AnsiQuotedStr( lstUnique[i], '''') ]
              )
            else begin
              try
                case VarType( lstUnique[i] )  of
                  varBoolean  ,
                  varWord     ,
                  varLongWord ,
                  varByte     ,
                  varShortInt ,
                  varSmallInt ,
                  varInteger  ,
                  varInt64    :
                    tmp_query := Format( '%s=%s',
                                   [ cmbFormula.Text, String( lstUnique[i] ) ]
                                 ) ;
                  varDate     :
                    tmp_query := Format( '%s=%s',
                                   [ cmbFormula.Text, DecodeDateTimeToSqlMacro( lstUnique[i] ) ]
                                 ) ;
                  else
                    tmp_query := Format( '%s=%s',
                                   [ cmbFormula.Text, DotFloatToStr( lstUnique[i] ) ]
                                 ) ;
                end;
              except
                continue ;
              end;
            end;

            prm.Add ;
            sec := TGIS_ParamsSectionVector( prm.Items[prm.Count-1] ) ;
            sec.Visible := True ;
            sec.Render.Expression := '' ;

            size  := Max( 1, i * 250 div lstUnique.Count ) ;

            // discrete color ramps have doubled colors definition,
            // so take every second value (div 2),
            // and do not exceed color count in ramp (mod)
            if ( cmbStyleRampList.Mode = TGIS_ColorMapMode.Discrete ) then
              color_id := 2 * ( i mod ( colors_count div 2 ) )
            else
              color_id := i mod colors_count ;

            color := colormap_arr[color_id].RGB ;

            if      rdbRenderMarker.Checked then begin
                    if      rdbRenderSize.Checked then
                            sec.Marker.Size := size + 15
                    else if rdbRenderColor.Checked then
                            sec.Marker.Color := color
                    else if rdbRenderOutlineWidth.Checked then
                            sec.Marker.OutlineWidth := size
                    else if rdbRenderOutlineColor.Checked then
                            sec.Marker.OutlineColor := color
                    else begin
                      Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end ;
                    sec.Marker.ShowLegend := chkStyleShowLegend.Checked ;
            end
            else if rdbRenderLine.Checked then begin
                    if      rdbRenderSize.Checked then
                            sec.Line.Width := size
                    else if rdbRenderColor.Checked then
                            sec.Line.Color := color
                    else if rdbRenderOutlineWidth.Checked then
                            sec.Line.OutlineWidth := size
                    else if rdbRenderOutlineColor.Checked then
                            sec.Line.OutlineColor := color
                    else begin
                      Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end;
                    sec.Line.ShowLegend := chkStyleShowLegend.Checked ;
            end
            else if rdbRenderArea.Checked then begin
                    if      rdbRenderColor.Checked then
                            sec.Area.Color := color
                    else if rdbRenderOutlineWidth.Checked then
                            sec.Area.OutlineWidth := size
                    else if rdbRenderOutlineColor.Checked then
                            sec.Area.OutlineColor := color
                    else begin
                      Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end ;
                    sec.Area.ShowLegend := chkStyleShowLegend.Checked ;
            end
            else begin
              Assert( False, GIS_RS_ERR_UNTESTED  ) ;
            end ;
            sec.Legend := lstUnique[i] ;

            if ( not IsStringEmpty( sec.Query ) ) and
               ( not IsStringEmpty( tmp_query ) ) then
              sec.Query := Format( '(%s) AND (%s)', [sec.Query, tmp_query] )
            else
              sec.Query := tmp_query ;
          end ;

          sec := TGIS_ParamsSectionVector( prm.Items[0] ) ;
          if      rdbRenderMarker.Checked then begin
                  if      rdbRenderSize.Checked then
                          sec.Marker.Size := 150
                  else if rdbRenderColor.Checked then
                          sec.Marker.Color := TGIS_Color.Red
                  else if rdbRenderOutlineWidth.Checked then
                          sec.Marker.OutlineWidth := 0
                  else if rdbRenderOutlineColor.Checked then
                          sec.Marker.OutlineColor := TGIS_Color.Black
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Marker.ShowLegend := (overUnique or overRecords) and
                                            chkStyleShowLegend.Checked ;
          end
          else if rdbRenderLine.Checked then begin
                  if      rdbRenderSize.Checked then
                          sec.Line.Width := 1
                  else if rdbRenderColor.Checked then
                          sec.Line.Color := TGIS_Color.Black
                  else if rdbRenderOutlineWidth.Checked then
                          sec.Line.OutlineWidth := 0
                  else if rdbRenderOutlineColor.Checked then
                          sec.Line.OutlineColor := TGIS_Color.Black
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end;
                  sec.Line.ShowLegend := (overUnique or overRecords) and
                                          chkStyleShowLegend.Checked ;
          end
          else if rdbRenderArea.Checked then begin
                  if      rdbRenderColor.Checked then
                          sec.Area.Color := TGIS_Color.Gray
                  else if rdbRenderOutlineWidth.Checked then
                          sec.Area.OutlineWidth := 1
                  else if rdbRenderOutlineColor.Checked then
                          sec.Area.OutlineColor := TGIS_Color.Black
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Area.ShowLegend := (overUnique or overRecords) and
                                          chkStyleShowLegend.Checked ;
          end
          else begin
            Assert( False, GIS_RS_ERR_UNTESTED  ) ;
          end ;
          sec.Legend := GIS_RS_LEGEND_TEXT_OTHER ;
        end
        // Simple / Continuous values
        else begin
          sec := TGIS_ParamsSectionVector( prm.Items[prm.Count-1] ) ;
          sec.Visible := True ;
          sec.Render.Expression := '' ;
          sec.Render.Expression := cmbFormula.Text ;
          sec.Render.StartColorAsText := cmbStyleColorStart.Value ;
          sec.Render.EndColorAsText := cmbStyleColorEnd.Value ;
          sec.Render.StartSizeAsText := cmbStyleSizeStart.Value ;
          sec.Render.EndSizeAsText := cmbStyleSizeEnd.Value ;

          if chkAnalyzeAvg.Checked then begin
            if bDates then begin
              sec.Render.MinVal   := Double(StrToDateTime( edtAnalyzeAvg.Text )) ;
              sec.Render.MaxVal   := Double(StrToDateTime( edtAnalyzeMax.Text )) ;
            end
            else begin
              sec.Render.MinVal   := DotStrToFloat( edtAnalyzeAvg.Text ) ;
              sec.Render.MaxVal   := DotStrToFloat( edtAnalyzeMax.Text ) ;
            end ;
            if chkAnalyzeLog.Checked then sec.Render.Zones := -5
                                     else sec.Render.Zones :=  5 ;

            if bDates then begin
              sec.Render.MinValEx := Double(StrToDateTime( edtAnalyzeMin.Text )) ;
              sec.Render.MaxValEx := Double(StrToDateTime( edtAnalyzeAvg.Text )) ;
            end
            else begin
              sec.Render.MinValEx := DotStrToFloat( edtAnalyzeMin.Text ) ;
              sec.Render.MaxValEx := DotStrToFloat( edtAnalyzeAvg.Text ) ;
            end ;
            if chkAnalyzeLog.Checked then sec.Render.ZonesEx := -5
                                     else sec.Render.ZonesEx :=  5 ;
          end
          else begin
            if bDates then begin
              sec.Render.MinVal := Double(StrToDateTime( edtAnalyzeMin.Text )) ;
              sec.Render.MaxVal := Double(StrToDateTime( edtAnalyzeMax.Text )) ;
            end
            else begin
              sec.Render.MinVal := DotStrToFloat( edtAnalyzeMin.Text ) ;
              sec.Render.MaxVal := DotStrToFloat( edtAnalyzeMax.Text ) ;
            end ;
            if chkAnalyzeLog.Checked then sec.Render.Zones := -5
                                     else sec.Render.Zones :=  5 ;
            sec.Render.MinValEx := 0 ;
            sec.Render.MaxValEx := 0 ;
            sec.Render.ZonesEx  := 0 ;
          end ;

          size  := GIS_RENDER_SIZE  ;
          color := TGIS_Color.RenderColor ;

          if      rdbRenderMarker.Checked then begin
                  if      rdbRenderSize.Checked then
                          sec.Marker.Size := size
                  else if rdbRenderColor.Checked then
                          sec.Marker.Color := color
                  else if rdbRenderOutlineWidth.Checked then
                          sec.Marker.OutlineWidth := size
                  else if rdbRenderOutlineColor.Checked then
                          sec.Marker.OutlineColor := color
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Marker.ShowLegend := chkStyleShowLegend.Checked ;
          end
          else if rdbRenderLine.Checked then begin
                  if      rdbRenderSize.Checked then
                          sec.Line.Width := size
                  else if rdbRenderColor.Checked then
                          sec.Line.Color := color
                  else if rdbRenderOutlineWidth.Checked then
                          sec.Line.OutlineWidth := size
                  else if rdbRenderOutlineColor.Checked then
                          sec.Line.OutlineColor := color
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Line.ShowLegend := chkStyleShowLegend.Checked ;
          end
          else if rdbRenderArea.Checked then begin
                  if      rdbRenderColor.Checked then
                          sec.Area.Color := color
                  else if rdbRenderOutlineWidth.Checked then
                          sec.Area.OutlineWidth := size
                  else if rdbRenderOutlineColor.Checked then
                          sec.Area.OutlineColor := color
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Area.ShowLegend := chkStyleShowLegend.Checked ;
          end
          else begin
            Assert( False, GIS_RS_ERR_UNTESTED  ) ;
          end
        end ;
      end
      else begin

      end ;

      objParams.Assign( prm );
    finally
      prm.Free ;
    end ;
  end ;

  function TGIS_ControlLegendVectorWiz.doClassifyAdvanced : Boolean ;
  var
    i          : Integer ;
    ci         : Integer ;
    fname      : String ;
    method     : String ;
    prm        : TGIS_ParamsList ;
    sec        : TGIS_ParamsSectionVector ;
    classifier : TGIS_ClassificationVector ;
    frm_stats  : TGIS_ControlStatistics ;
  begin
    Result := False ;

    prm := TGIS_ParamsList.Create ;
    try
      prm.Assign( objParams ) ;

      for i := prm.Count - 1 downto 1 do begin
        prm.Selected := i ;
        prm.Delete ;
      end ;

      sec := TGIS_ParamsSectionVector( prm.Items[0] ) ;
      sec.Visible := True ;
      sec.Render.Expression := '' ;

      classifier := TGIS_ClassificationVector.Create( objLayer ) ;
      try
        // set properties
        fname := cmbFormulaAdvanced.Items[ cmbFormulaAdvanced.ItemIndex ] ;
        classifier.Field        := GisDeNormalizedSQLName( fname ) ;
        classifier.NumClasses   := cmbClassesAdvanced.ItemIndex + 1 ;

        sec.Render.StartColorAsText := cmbStyleColorStart.Value ;
        classifier.StartColor       := sec.Render.StartColor ;
        sec.Render.EndColorAsText   := cmbStyleColorEnd.Value ;
        classifier.EndColor         := sec.Render.EndColor ;
        sec.Render.StartSizeAsText  := cmbStyleSizeStart.Value ;
        classifier.StartSize        := sec.Render.StartSize ;
        sec.Render.EndSizeAsText    := cmbStyleSizeEnd.Value ;
        classifier.EndSize          := sec.Render.EndSize ;
        classifier.ShowLegend       := chkStyleShowLegend.Checked ;

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

        // set color ramp
        if rdbStyleColorRamp.Checked then begin
          if ( method = GIS_RS_CLASSIFY_METHOD_QR ) or
             ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
             ( method = GIS_RS_CLASSIFY_METHOD_SDC ) or
             ( method = GIS_RS_CLASSIFY_METHOD_DI ) then
            ci := classifier.NumClasses
          else
            ci := cmbClassesAdvanced.ItemIndex + 1 ;

          classifier.ColorRamp := cmbStyleRampList.Value( ci )
        end
        else
          classifier.ColorRamp := nil ;

        // set render type
        if rdbRenderSize.Checked then
          classifier.RenderType := TGIS_ClassificationRenderType.Size
        else if rdbRenderColor.Checked then
          classifier.RenderType := TGIS_ClassificationRenderType.Color
        else if rdbRenderOutlineWidth.Checked then
          classifier.RenderType := TGIS_ClassificationRenderType.OutlineWidth
        else if rdbRenderOutlineColor.Checked then
          classifier.RenderType := TGIS_ClassificationRenderType.OutlineColor
        else
          classifier.RenderType := TGIS_ClassificationRenderType.Color ;

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

        classifier.Classify( prm ) ;
      finally
        FreeObject( classifier ) ;
      end ;

      objParams.Assign( prm );
    finally
      prm.Free ;
    end ;

    Result := True ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.btnOKClick(Sender: TObject);
  begin
    if btnNextPage.Visible = True then begin
      btnNextPageClick( Sender ) ;
      exit ;
    end;

    if iMode = 0 then
      doClassifySimple
    else
      if not doClassifyAdvanced then
        exit ;

    ModalResult := mrOK ;
  end;

  function TGIS_ControlLegendVectorWiz.doCustomSize(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    dlg  : TGIS_ControlSizeForm ;
    frm  : TGIS_ControlFieldFactor ;
    val  : String ;
    proc : TGIS_Proc ;
  begin
    Result := '' ;

    if _value = GIS_PARAMTXT_TYPE_FIELD then begin
      frm := TGIS_ControlFieldFactor.Create( Self ) ;
      frm.FillFields( cmbFormulaAdvanced.Items ) ;
      frm.FillUnits( TGIS_FieldFactorUnitsType.Size ) ;

      proc := procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit ;

          val := ConstructNumberAsText(
                   frm.cmbFields.Text,
                   frm.spnFactor.Text,
                   frm.cmbUnits.Text
                 ) ;
          //oComboBox.DelayedUpdate( val ) ;
        end ;

      frm.Execute(
        pOnHelp,
        proc
      );
      Result := val ;
    end
    else
    if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      dlg := TGIS_ControlSizeForm.Create( Self ) ;
      dlg.FillUnits( False ) ;
      proc := procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit ;
          if dlg.isRotation then
            val := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                      dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text
          else
            val := GIS_PARAMTXT_TYPE_SIZE + ':' +
                      dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text
        end ;

      dlg.Execute( pOnHelp, proc ) ;
      Result := val ;
    end;
  end ;

  function TGIS_ControlLegendVectorWiz.doCustomColor(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    dlg  : TGIS_ColorDialog ;
    frm  : TGIS_ControlFieldFactor ;
    proc : TGIS_Proc ;
    val  : String ;
  begin
    Result := '' ;

    if _value = GIS_PARAMTXT_TYPE_FIELD then begin
      frm := TGIS_ControlFieldFactor.Create( Self ) ;
      frm.FillFields( cmbFormula.Items ) ;
      frm.FillUnits( TGIS_FieldFactorUnitsType.NoScale ) ;

      proc := procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit ;

          val := ConstructParamAsText(
                    GIS_PARAMTXT_TYPE_FIELD,
                    frm.cmbFields.Text, ''
                  ) ;
          //oComboBox.DelayedUpdate( val ) ;
        end ;

      frm.Execute(
        pOnHelp,
        proc
      );

      Result := val ;
    end
    else
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


//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendVectorWiz.initForm ;
  begin
    Self.ActiveControl := btnNextPage ;
    Self.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RENDER ) ;
    Self.ClientHeight := 411 ;
    Self.ClientWidth := 734 ;
    Self.Name := 'TGIS_ControlLegendVectorWiz' ;
    Self.OnCreate := FormCreate ;
    Self.OnDestroy := FormDestroy ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.initControls ;
  var
    i : Integer ;
  begin
    pnlFormula := TPanel.Create( Self ) ;
    pnlFormula.Parent := Self ;
    pnlFormula.BevelOuter := bvNone ;
    pnlFormula.Left := 8 ;
    pnlFormula.Top := 8 ;
    pnlFormula.Width := 353 ;
    pnlFormula.Height := 176 ;

    lblFormula := TLabel.Create( pnlFormula ) ;
    lblFormula.Parent := pnlFormula ;
    lblFormula.Top := 16 ;
    lblFormula.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblFormula, 8, -1 ) ;
    lblFormula.AutoSize := False ;
    lblFormula.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_FORMULA ) ;
    lblFormula.FocusControl := cmbFormula ;

    cmbFormula := TComboBox.Create( pnlFormula ) ;
    cmbFormula.Parent := pnlFormula ;
    cmbFormula.Top := 32 ;
    cmbFormula.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbFormula, 8, -1 ) ;
    cmbFormula.TabOrder := 0 ;
    cmbFormula.OnChange := actListUpdate ;

    rdbLimitedScan := TRadioButton.Create( pnlFormula ) ;
    rdbLimitedScan.Parent := pnlFormula;
    rdbLimitedScan.Top := 70 ;
    rdbLimitedScan.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbLimitedScan, 8, -1 ) ;
    rdbLimitedScan.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBLIMITSCAN ) ;
    rdbLimitedScan.TabOrder := 1 ;
    rdbLimitedScan.Checked := True ;

    rdbFullScan := TRadioButton.Create( pnlFormula ) ;
    rdbFullScan.Parent := pnlFormula;
    rdbFullScan.Top := 95 ;
    rdbFullScan.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbFullScan, 8, -1 ) ;
    rdbFullScan.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBFULLSCAN ) ;
    rdbFullScan.TabOrder := 2 ;

    pnlAnalyze := TPanel.Create( Self ) ;
    pnlAnalyze.Parent := Self ;
    pnlAnalyze.BevelOuter := bvNone ;
    pnlAnalyze.Left := 370 ;
    pnlAnalyze.Top := 8 ;
    pnlAnalyze.Width := 353 ;
    pnlAnalyze.Height := 176 ;

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

    rdbAnalyzeContinous := TRadioButton.Create( pnlAnalyze ) ;
    rdbAnalyzeContinous.Parent := pnlAnalyze ;
    rdbAnalyzeContinous.Top := 8 ;
    rdbAnalyzeContinous.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbAnalyzeContinous, 184, 161 ) ;
    rdbAnalyzeContinous.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBCONTINOUS ) ;
    rdbAnalyzeContinous.TabOrder := 2 ;
    rdbAnalyzeContinous.OnClick := actListUpdate ;

    lblAnalyzeMin := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeMin.Parent := pnlAnalyze ;
    lblAnalyzeMin.Top := 32 ;
    lblAnalyzeMin.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblAnalyzeMin, 184, 64 ) ;
    lblAnalyzeMin.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLMIN ) ;
    lblAnalyzeMin.FocusControl := edtAnalyzeMin ;

    edtAnalyzeMin := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMin.Parent := pnlAnalyze ;
    edtAnalyzeMin.Top := 48 ;
    edtAnalyzeMin.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMin, 184, 121 ) ;
    edtAnalyzeMin.TabOrder := 3 ;

    lblAnalyzeMax := TLabel.Create( pnlAnalyze ) ;
    lblAnalyzeMax.Parent := pnlAnalyze ;
    lblAnalyzeMax.Top := 72 ;
    lblAnalyzeMax.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblAnalyzeMax, 184, 68 ) ;
    lblAnalyzeMax.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_LBLMAX ) ;
    lblAnalyzeMax.FocusControl := edtAnalyzeMax ;

    edtAnalyzeMax := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeMax.Parent := pnlAnalyze ;
    edtAnalyzeMax.Top := 88 ;
    edtAnalyzeMax.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtAnalyzeMax, 184, 121 ) ;
    edtAnalyzeMax.TabOrder := 4 ;

    chkAnalyzeAvg := TCheckBox.Create( pnlAnalyze ) ;
    chkAnalyzeAvg.Parent := pnlAnalyze ;
    chkAnalyzeAvg.Top := 112 ;
    chkAnalyzeAvg.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkAnalyzeAvg, 184, 137 ) ;
    chkAnalyzeAvg.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CHKAVG ) ;
    chkAnalyzeAvg.TabOrder := 5 ;
    chkAnalyzeAvg.OnClick := actListUpdate ;

    edtAnalyzeAvg := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeAvg.Parent := pnlAnalyze ;
    edtAnalyzeAvg.Top := 128 ;
    edtAnalyzeAvg.Height := 21 ;
    PlaceControl( BiDiMode, nil, edtAnalyzeAvg, 184, 121 ) ;
    edtAnalyzeAvg.TabOrder := 6 ;

    chkAnalyzeLog := TCheckBox.Create( pnlAnalyze ) ;
    chkAnalyzeLog.Parent := pnlAnalyze ;
    chkAnalyzeLog.Top := 152 ;
    chkAnalyzeLog.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkAnalyzeLog, 184, 137 ) ;
    chkAnalyzeLog.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CHKLOG ) ;
    chkAnalyzeLog.TabOrder := 7 ;
    chkAnalyzeLog.OnClick := actListUpdate ;

    pnlRender := TPanel.Create( Self ) ;
    pnlRender.Parent := Self ;
    pnlRender.BevelOuter := bvNone ;
    pnlRender.Top := 191 ;
    pnlRender.Height := 176 ;
    pnlRender.Left := 370 ;
    pnlRender.Width := 353 ;

    grpRenderFeature := TGroupBox.Create( pnlRender ) ;
    grpRenderFeature.Parent := pnlRender ;
    grpRenderFeature.Top := 8 ;
    grpRenderFeature.Height := 121 ;
    PlaceControl( BiDiMode, nil, grpRenderFeature, 16, 153 ) ;
    grpRenderFeature.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_GRPSHAPE ) ;
    grpRenderFeature.TabOrder := 0 ;

    rdbRenderMarker := TRadioButton.Create( grpRenderFeature ) ;
    rdbRenderMarker.Parent := grpRenderFeature ;
    rdbRenderMarker.Top := 24 ;
    rdbRenderMarker.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderMarker, 8, 137 ) ;
    rdbRenderMarker.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBMARKER ) ;
    rdbRenderMarker.TabOrder := 0 ;
    rdbRenderMarker.OnClick := actListUpdate ;

    rdbRenderLine := TRadioButton.Create( grpRenderFeature ) ;
    rdbRenderLine.Parent := grpRenderFeature ;
    rdbRenderLine.Top := 48 ;
    rdbRenderLine.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderLine, 8, 137 ) ;
    rdbRenderLine.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBLINE ) ;
    rdbRenderLine.TabOrder := 1 ;
    rdbRenderLine.OnClick := actListUpdate ;

    rdbRenderArea := TRadioButton.Create( grpRenderFeature ) ;
    rdbRenderArea.Parent := grpRenderFeature ;
    rdbRenderArea.Top := 72 ;
    rdbRenderArea.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderArea, 8, 137 ) ;
    rdbRenderArea.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBAREA ) ;
    rdbRenderArea.TabOrder := 2 ;
    rdbRenderArea.OnClick := actListUpdate ;

    grpRenderValue := TGroupBox.Create( pnlRender ) ;
    grpRenderValue.Parent := pnlRender ;
    grpRenderValue.Top := 8 ;
    grpRenderValue.Height := 121 ;
    PlaceControl( BiDiMode, nil, grpRenderValue, 184, 153 ) ;
    grpRenderValue.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_GRPVALUE ) ;
    grpRenderValue.TabOrder := 1 ;

    rdbRenderSize := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderSize.Parent := grpRenderValue ;
    rdbRenderSize.Top := 24 ;
    rdbRenderSize.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderSize, 8, 137 ) ;
    rdbRenderSize.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RBDSIZE ) ;
    rdbRenderSize.TabOrder := 0 ;
    rdbRenderSize.OnClick := actListUpdate ;

    rdbRenderColor := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderColor.Parent := grpRenderValue ;
    rdbRenderColor.Top := 48 ;
    rdbRenderColor.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderColor, 8, 137 ) ;
    rdbRenderColor.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RBDCOLOR ) ;
    rdbRenderColor.Checked := True ;
    rdbRenderColor.TabOrder := 1 ;
    rdbRenderColor.TabStop := True ;
    rdbRenderColor.OnClick := actListUpdate ;

    rdbRenderOutlineWidth := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderOutlineWidth.Parent := grpRenderValue ;
    rdbRenderOutlineWidth.Top := 72 ;
    rdbRenderOutlineWidth.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderOutlineWidth, 8, 137 ) ;
    rdbRenderOutlineWidth.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_OUTLINEWIDTH ) ;
    rdbRenderOutlineWidth.TabOrder := 2 ;
    rdbRenderOutlineWidth.OnClick := actListUpdate ;

    rdbRenderOutlineColor := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderOutlineColor.Parent := grpRenderValue ;
    rdbRenderOutlineColor.Top := 96 ;
    rdbRenderOutlineColor.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbRenderOutlineColor, 8, 137 ) ;
    rdbRenderOutlineColor.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_OUTLINECOLOR ) ;
    rdbRenderOutlineColor.TabOrder := 3 ;
    rdbRenderOutlineColor.OnClick := actListUpdate ;

    chkStyleShowLegend := TCheckBox.Create( pnlRender ) ;
    chkStyleShowLegend.Parent := pnlRender ;
    chkStyleShowLegend.Top := 140 ;
    chkStyleShowLegend.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleShowLegend, 16, -1 ) ;
    chkStyleShowLegend.Caption := _rsrc( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
    chkStyleShowLegend.TabOrder := 5 ;
    chkStyleShowLegend.Checked := True ;
    chkStyleShowLegend.OnClick := actListUpdate ;

    pnlChoose := TPanel.Create( Self ) ;
    pnlChoose.Parent := Self ;
    pnlChoose.BevelOuter := bvNone ;
    pnlChoose.Left := 8 ;
    pnlChoose.Top := 8 ;
    pnlChoose.Width := 353 ;
    pnlChoose.Height := 176 ;

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
    rdbChooseAdvanced.TabOrder := 1 ;
    rdbChooseAdvanced.Checked := False ;

    pnlFormulaAdvanced := TPanel.Create( Self ) ;
    pnlFormulaAdvanced.Parent := Self ;
    pnlFormulaAdvanced.BevelOuter := bvNone ;
    pnlFormulaAdvanced.Left := 8 ;
    pnlFormulaAdvanced.Top := 8 ;
    pnlFormulaAdvanced.Width := 353 ;
    pnlFormulaAdvanced.Height := 176 ;

    lblFormulaAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblFormulaAdvanced.Parent := pnlFormulaAdvanced ;
    lblFormulaAdvanced.Top := 16 ;
    lblFormulaAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblFormulaAdvanced, 8, -1 ) ;
    lblFormulaAdvanced.AutoSize := False ;
    lblFormulaAdvanced.Caption := _rsrc( GIS_RS_FIELDFACTOR_FIELD ) ;
    lblFormulaAdvanced.FocusControl := cmbFormulaAdvanced ;

    cmbFormulaAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbFormulaAdvanced.Parent := pnlFormulaAdvanced ;
    cmbFormulaAdvanced.Top := 32 ;
    cmbFormulaAdvanced.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbFormulaAdvanced, 8, -1 ) ;
    cmbFormulaAdvanced.TabOrder := 0 ;
    cmbFormulaAdvanced.Style := csDropDownList ;
    cmbFormulaAdvanced.OnChange := actListUpdate ;

    lblMethodAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblMethodAdvanced.Parent := pnlFormulaAdvanced ;
    lblMethodAdvanced.Top := 62 ;
    lblMethodAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblMethodAdvanced, 8, -1 ) ;
    lblMethodAdvanced.AutoSize := False ;
    lblMethodAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_METHOD ) ;
    lblMethodAdvanced.FocusControl := cmbMethodAdvanced ;

    cmbMethodAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbMethodAdvanced.Parent := pnlFormulaAdvanced ;
    cmbMethodAdvanced.Top := 78 ;
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
    lblClassesAdvanced.Top := 62 ;
    lblClassesAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblClassesAdvanced, 182, 80 ) ;
    lblClassesAdvanced.AutoSize := False ;
    lblClassesAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_CLASSES ) ;
    lblClassesAdvanced.FocusControl := cmbClassesAdvanced ;

    cmbClassesAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbClassesAdvanced.Parent := pnlFormulaAdvanced ;
    cmbClassesAdvanced.Top := 78 ;
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
    lblIntervalAdvanced.Top := 62 ;
    lblIntervalAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblIntervalAdvanced, 270, 75 ) ;
    lblIntervalAdvanced.AutoSize := False ;
    lblIntervalAdvanced.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_INTERVAL ) ;
    lblIntervalAdvanced.FocusControl := cmbIntervalAdvanced ;

    cmbIntervalAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    cmbIntervalAdvanced.Top := 78 ;
    cmbIntervalAdvanced.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbIntervalAdvanced, 270, 75 ) ;
    cmbIntervalAdvanced.TabOrder := 0 ;
    cmbIntervalAdvanced.Text := '100' ;
    cmbIntervalAdvanced.OnChange := actListUpdate ;

    pnlStyleParams := TPanel.Create( Self ) ;
    pnlStyleParams.Parent := Self ;
    pnlStyleParams.BevelOuter := bvNone ;
    pnlStyleParams.Left := 8 ;
    pnlStyleParams.Top := 0 ;
    pnlStyleParams.Width := 353 ;
    pnlStyleParams.Height := 176 ;

    rdbStyleSize := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleSize.Parent := pnlStyleParams;
    rdbStyleSize.Top := 33 ;
    rdbStyleSize.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleSize, 8, 90 ) ;
    rdbStyleSize.Caption := _rsrc( GIS_RS_LEGEND_PRM_SIZE ) ;
    rdbStyleSize.TabOrder := 1 ;
    rdbStyleSize.Checked := True ;

    lblStyleSizeStart := TLabel.Create( pnlStyleParams ) ;
    lblStyleSizeStart.Parent := pnlStyleParams ;
    lblStyleSizeStart.Top := 16 ;
    lblStyleSizeStart.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleSizeStart, 100, 121 ) ;
    lblStyleSizeStart.Caption := _rsrc( GIS_RS_LEGEND_PRM_STARTSIZE ) ;
    lblStyleSizeStart.FocusControl := cmbStyleSizeStart ;

    cmbStyleSizeStart := TGIS_SizeComboBox.Create( pnlStyleParams ) ;
    cmbStyleSizeStart.Parent := pnlStyleParams ;
    cmbStyleSizeStart.Top := 33 ;
    PlaceControl( BiDiMode, nil, cmbStyleSizeStart, 100, 100 ) ;
    cmbStyleSizeStart.Fill( True, False, True, True ) ;
    cmbStyleSizeStart.CustomEvent := doCustomSize ;

    lblStyleSizeEnd := TLabel.Create( pnlStyleParams ) ;
    lblStyleSizeEnd.Parent := pnlStyleParams ;
    lblStyleSizeEnd.Top := 16 ;
    lblStyleSizeEnd.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleSizeEnd, 220, 121 ) ;
    lblStyleSizeEnd.Caption := _rsrc( GIS_RS_LEGEND_PRM_ENDSIZE ) ;
    lblStyleSizeEnd.FocusControl := cmbStyleSizeEnd ;

    cmbStyleSizeEnd := TGIS_SizeComboBox.Create( pnlStyleParams ) ;
    cmbStyleSizeEnd.Parent := pnlStyleParams ;
    cmbStyleSizeEnd.Top := 33 ;
    PlaceControl( BiDiMode, nil, cmbStyleSizeEnd, 220, 100 ) ;
    cmbStyleSizeEnd.Fill( True, False, True, True ) ;
    cmbStyleSizeEnd.CustomEvent := doCustomSize ;

    rdbStyleColorRange := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRange.Parent := pnlStyleParams;
    rdbStyleColorRange.Top := 76 ;
    rdbStyleColorRange.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRange, 8, 90 ) ;
    rdbStyleColorRange.Caption := _rsrc( GIS_RS_LEGEND_PRM_COLOR ) ;
    rdbStyleColorRange.TabOrder := 1 ;
    rdbStyleColorRange.OnClick := actListUpdate ;

    lblStyleColorStart := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorStart.Parent := pnlStyleParams ;
    lblStyleColorStart.Top := 60 ;
    lblStyleColorStart.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorStart, 100, 121 ) ;
    lblStyleColorStart.Caption := _rsrc( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
    lblStyleColorStart.FocusControl := cmbStyleSizeStart ;

    cmbStyleColorStart := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorStart.Parent := pnlStyleParams ;
    cmbStyleColorStart.Top := 76 ;
    cmbStyleColorStart.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorStart, 100, 100 ) ;
    cmbStyleColorStart.Fill( True, True ) ;
    cmbStyleColorStart.CustomEvent := doCustomColor ;

    lblStyleColorEnd := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorEnd.Parent := pnlStyleParams ;
    lblStyleColorEnd.Top := 60 ;
    lblStyleColorEnd.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorEnd, 220, 121 ) ;
    lblStyleColorEnd.Caption := _rsrc( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
    lblStyleColorEnd.FocusControl := cmbStyleSizeEnd ;

    cmbStyleColorEnd := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorEnd.Parent := pnlStyleParams ;
    cmbStyleColorEnd.Top := 76 ;
    cmbStyleColorEnd.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorEnd, 220, 100 ) ;
    cmbStyleColorEnd.Fill( True, True ) ;
    cmbStyleColorEnd.CustomEvent := doCustomColor ;

    rdbStyleColorRamp := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRamp.Parent := pnlStyleParams;
    rdbStyleColorRamp.Top := 119 ;
    rdbStyleColorRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRamp, 8, 90 ) ;
    rdbStyleColorRamp.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMPS ) ;
    rdbStyleColorRamp.TabOrder := 1 ;
    rdbStyleColorRamp.OnClick := actListUpdate ;
    rdbStyleColorRamp.Checked := False ;

    cmbStyleRampList := TGIS_ColorRampComboBox.Create( pnlStyleParams ) ;
    cmbStyleRampList.Parent := pnlStyleParams ;
    cmbStyleRampList.Anchors := [akLeft, akTop, akRight] ;
    cmbStyleRampList.Top := 119 ;
    cmbStyleRampList.Height := 22 ;
    PlaceControl( BiDiMode, nil, cmbStyleRampList, 100, 221 ) ;
    cmbStyleRampList.TabOrder := 5 ;

    chkReverseRamp := TCheckBox.Create( pnlStyleParams ) ;
    chkReverseRamp.Parent := pnlStyleParams ;
    chkReverseRamp.Anchors := anchors ;
    chkReverseRamp.Top := 145 ;
    chkReverseRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkReverseRamp, 100, 90 ) ;
    chkReverseRamp.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_REVERSE ) ;
    chkReverseRamp.Checked := False ;
    chkReverseRamp.State := cbUnChecked ;
    chkReverseRamp.TabOrder := 7 ;
    chkReverseRamp.OnClick := actReverseClick ;

    chkStyleDiscrete := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleDiscrete.Parent := pnlStyleParams ;
    chkStyleDiscrete.Anchors := anchors ;
    chkStyleDiscrete.Top := 145 ;
    chkStyleDiscrete.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleDiscrete, 195, 100 ) ;
    chkStyleDiscrete.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_DISCRETE ) ;
    chkStyleDiscrete.OnClick := actDiscreteClick ;
    chkStyleDiscrete.TabOrder := 8 ;

    chkShowAllRamps := TCheckBox.Create( pnlStyleParams ) ;
    chkShowAllRamps.Parent := pnlStyleParams ;
    chkShowAllRamps.Anchors := anchors ;
    chkShowAllRamps.Top := 145 ;
    chkShowAllRamps.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkShowAllRamps, 290, 100 ) ;
    chkShowAllRamps.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_SHOWALL ) ;
    chkShowAllRamps.OnClick := actShowAllClick ;
    chkShowAllRamps.TabOrder := 8 ;

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

    btnHelp.Visible := assigned( pOnHelp ) ;

    btnOK.Top := btnNextPage.Top - btnOK.Height ;
    btnOK.Left := btnNextPage.Left ;
    btnOK.OnClick := btnOKClick ;

    btnHelp.TabOrder := 0 ;
    btnPreviousPage.TabOrder := 1 ;
    btnNextPage.TabOrder := 2 ;
    btnOK.TabOrder := 3 ;
    btnCancel.TabOrder := 4 ;
    pnlFormula.TabOrder := 5 ;
    pnlAnalyze.TabOrder := 6 ;
    pnlRender.TabOrder := 7 ;
    pnlChoose.TabOrder := 8 ;
    pnlFormulaAdvanced.TabOrder := 9 ;

  end ;

  procedure TGIS_ControlLegendVectorWiz.showForm ;
  begin
    pnlChoose.Visible  := True ;
    pnlChoose.Left     := pnlChoose.Left  ;
    pnlChoose.Top      := pnlChoose.Top   ;

    pnlFormula.Visible := False ;
    pnlFormula.Left    := pnlFormula.Left  ;
    pnlFormula.Top     := pnlFormula.Top   ;

    pnlFormulaAdvanced.Visible := False ;
    pnlFormulaAdvanced.Left    := pnlFormula.Left  ;
    pnlFormulaAdvanced.Top     := pnlFormula.Top   ;

    pnlAnalyze.Visible := False ;
    pnlAnalyze.Left    := pnlFormula.Left  ;
    pnlAnalyze.Top     := pnlFormula.Top   ;

    pnlRender.Visible  := False ;
    pnlRender.Left     := pnlFormula.Left  ;
    pnlRender.Top      := pnlFormula.Top   ;

    pnlStyleParams.Visible  := False ;
    pnlStyleParams.Left     := pnlFormula.Left  ;
    pnlStyleParams.Top      := pnlFormula.Top   ;

    btnOK.Visible      := False            ;
    btnOK.Left         := btnNextPage.Left ;
    btnOK.Top          := btnNextPage.Top  ;

    ClientWidth  := pnlFormula.Left + pnlFormula.Width + 8 ;
    ClientHeight := pnlFormula.Top + pnlFormula.Height + 8 +
                    btnCancel.Height + 8 ;
    actListUpdate( nil ) ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.afterPPIChanged ;
  begin
    //ClientWidth  := pnlFormula.Left + pnlFormula.Width + 8 ;
    //ClientHeight := pnlFormula.Top + pnlFormula.Height + 8 +
    //                btnCancel.Height + 8 ;
  end ;

  function TGIS_ControlLegendVectorWiz.Execute(
    const _layer  : TGIS_LayerVector ;
    const _type   : TGIS_ShapeType   ;
    const _params : TGIS_ParamsList  ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  var
    i           : Integer     ;
    wasjoin     : Boolean     ;
    lstFields   : TStringList ;
    lstFieldsN  : TStringList ;
    {$IFNDEF GIS_NOADO}
    field_type  : TFieldType ;
    field_size  : Integer ;
    {$ENDIF}
  begin
    Assert( Assigned( _layer ) ) ;
    objLayer := _layer ;

    if _type = TGIS_ShapeType.Unknown then oType := objLayer.DefaultShapeType
                                      else oType := _type ;

    Assert( Assigned( _params ) ) ;
    objParams := _params ;

    pOnHelp := _onhelp ;
    btnHelp.Visible := Assigned( pOnHelp ) ;

    lstFields  := TStringList.Create ;
    lstFieldsN := TStringList.Create ;
    try
      lstFields.Text := GIS_FIELDS_PREDEFINED ;

      if (oType = TGIS_ShapeType.Polygon) or (oType = TGIS_ShapeType.MultiPatch) then
      begin
        lstFieldsN.Add( GIS_FIELD_AREA    ) ;
        lstFieldsN.Add( GIS_FIELD_LENGTH  ) ;
      end
      else if (oType = TGIS_ShapeType.Arc ) then begin
        lstFieldsN.Add( GIS_FIELD_LENGTH  ) ;
      end ;
      if objLayer.IsVector3D then begin
        lstFieldsN.Add( GIS_FIELD_COORD_Z ) ;
        lstFieldsN.Add( GIS_FIELD_COORD_M ) ;
      end ;
      lstFieldsN.Add( GIS_FIELD_MIN_X ) ;
      lstFieldsN.Add( GIS_FIELD_MIN_Y ) ;
      if objLayer.IsVector3D then begin
        lstFieldsN.Add( GIS_FIELD_MIN_Z ) ;
        lstFieldsN.Add( GIS_FIELD_MIN_M ) ;
      end;
      lstFieldsN.Add( GIS_FIELD_MAX_X ) ;
      lstFieldsN.Add( GIS_FIELD_MAX_Y ) ;
      if objLayer.IsVector3D then begin
        lstFieldsN.Add( GIS_FIELD_MAX_Z ) ;
        lstFieldsN.Add( GIS_FIELD_MAX_M ) ;
      end;
      lstFieldsN.Add( GIS_FIELD_CENTER_X ) ;
      lstFieldsN.Add( GIS_FIELD_CENTER_Y ) ;
      if objLayer.IsVector3D then begin
        lstFieldsN.Add( GIS_FIELD_CENTER_Z ) ;
        lstFieldsN.Add( GIS_FIELD_CENTER_M ) ;
      end;
      lstFieldsN.Add( GIS_FIELD_CENTROID_X ) ;
      lstFieldsN.Add( GIS_FIELD_CENTROID_Y ) ;
      lstFieldsN.Add( GIS_FIELD_NUM_PARTS ) ;
      lstFieldsN.Add( GIS_FIELD_NUM_POINTS ) ;
      lstFieldsN.Add( GIS_FIELD_AGGREGATED_COUNT ) ;
      lstFieldsN.Add( GIS_FIELD_AGGREGATED_VALUE ) ;

      with objLayer as TGIS_LayerVector do begin
        for i:=0 to Fields.Count - 1 do begin
          if not (TGIS_FieldFlags.Visible in FieldInfo( i ).Flags) then continue ;

          lstFields.Add( TGIS_Utils.GisNormalizedSQLName( FieldInfo( i ).NewName ) ) ;
          if FieldInfo( i ).FieldType in [TGIS_FieldType.Number,TGIS_FieldType.Float] then
            lstFieldsN.Add( TGIS_Utils.GisNormalizedSQLName( FieldInfo( i ).NewName ) ) ;
        end ;

        wasjoin := False ;

        {$IFNDEF GIS_NODB}
          if ( not wasjoin ) and Assigned( JoinDB ) then begin
            wasjoin := True ;
            for i:=0 to JoinDB.FieldCount-1 do begin
              JoinFieldInfo( JoinDB.Fields[i], field_type, field_size ) ;
              if field_type <> ftUnknown then begin
                lstFields.Add( ToJoinFieldName( JoinDB.Fields[i].DisplayName ) ) ;
                if field_type in
                  [ftWord,ftSmallint,ftInteger,ftLargeint,ftFloat,ftCurrency,ftBCD,ftFMTBcd]
                then
                  lstFieldsN.Add( ToJoinFieldName( JoinDB.Fields[i].DisplayName ) ) ;
              end ;
            end ;
          end ;
        {$ENDIF}

        {$IFNDEF GIS_NOADO_JOIN}
          if ( not wasjoin ) and Assigned( JoinADO ) then begin
            //wasjoin := True ;
            for i:=0 to JoinADO.Fields.Count-1 do begin
              JoinFieldInfo( JoinADO.Fields[i], field_type, field_size ) ;
              if field_type <> ftUnknown then begin
                lstFields.Add( ToJoinFieldName( JoinADO.Fields.Item[i].Name ) ) ;
                if field_type in
                  [ftWord,ftSmallint,ftInteger,ftLargeint,ftFloat,ftCurrency,ftBCD,ftFMTBcd]
                then
                  lstFieldsN.Add( ToJoinFieldName( JoinADO.Fields.Item[i].Name ) ) ;
              end ;
              end ;
          end ;
        {$ENDIF}
      end ;

      cmbFormula.Items.Assign( lstFields ) ;

      if cmbFormula.Items.Count > 0 then
        cmbFormula.ItemIndex := Min( 0, cmbFormula.Items.Count - 1 ) ;

      cmbFormulaAdvanced.Items.Assign( lstFieldsN ) ;

      if cmbFormulaAdvanced.Items.Count > 0 then
        cmbFormulaAdvanced.ItemIndex := Min( 0, cmbFormulaAdvanced.Items.Count - 1 )
    finally
      lstFields.Free ;
      lstFieldsN.Free ;
    end ;

    Result := ShowModal ;
  end ;

  function TGIS_ControlLegendVectorWiz.Execute(
    const _layer  : TGIS_LayerVector ;
    const _type   : TGIS_ShapeType   ;
    const _params : TGIS_ParamsList
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, _type, _params, hlp ) ;
  end;


//==================================== END =====================================
end.

