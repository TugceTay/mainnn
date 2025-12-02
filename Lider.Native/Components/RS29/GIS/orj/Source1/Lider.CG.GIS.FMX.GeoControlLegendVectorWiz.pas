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

unit Lider.CG.GIS.FMX.GeoControlLegendVectorWiz ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendVectorWiz"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  Data.DB,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.Menus,
  FMX.Grid,
  FMX.Memo,
  FMX.Edit,
  FMX.Platform,
  System.Actions,
  FMX.ActnList,
  FMX.StdCtrls,
  FMX.Graphics,
  FMX.ComboEdit,
  FMX.ListBox,
  FMX.Layouts,

  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoClasses,
  FMX.Controls.Presentation,
  Lider.CG.GIS.FMX.GeoControlVarious,
  Lider.CG.GIS.FMX.GeoControlColor,
  Lider.CG.GIS.FMX.GeoModalForm ;


type

  /// <summary>
  ///   Visual form for managing vector rendering wizard.
  /// </summary>
  TGIS_ControlLegendVectorWiz = class( TGIS_ModalForm )
    pnlFormula: TLayout;
    lblFormula: TLabel;
    pnlAnalyze: TLayout;
    lblAnalyzeUnique: TLabel;
    lblAnalyzeMin: TLabel;
    lblAnalyzeMax: TLabel;
    rdbAnalyzeUnique: TRadioButton;
    rdbAnalyzeContinous: TRadioButton;
    memAnalyzeUnique: TMemo;
    btnNextPage: TButton;
    btnPreviousPage: TButton;
    pnlRender: TLayout;
    grpRenderFeature: TGroupBox;
    rdbRenderMarker: TRadioButton;
    rdbRenderLine: TRadioButton;
    rdbRenderArea: TRadioButton;
    grpRenderValue: TGroupBox;
    rdbRenderSize: TRadioButton;
    rdbRenderOutlineWidth: TRadioButton;
    rdbRenderColor: TRadioButton;
    rdbRenderOutlineColor: TRadioButton;
    chkAnalyzeAvg: TCheckBox;
    chkAnalyzeLog: TCheckBox;
    edtAnalyzeAvg: TEdit;
    edtAnalyzeMin: TEdit;
    edtAnalyzeMax: TEdit;
    cmbFormula: TComboEdit;
    rdbLimitedScan: TRadioButton;
    rdbFullScan: TRadioButton;
    pnlChoose: TLayout;
    rdbChooseSimple: TRadioButton;
    rdbChooseAdvanced: TRadioButton;
    pnlFormulaAdvanced: TLayout;
    lblFormulaAdvanced: TLabel;
    cmbFormulaAdvanced: TComboBox;
    lblMethodAdvanced: TLabel;
    cmbMethodAdvanced: TComboBox;
    lblClassesAdvanced: TLabel;
    cmbClassesAdvanced: TComboBox;
    lblIntervalAdvanced: TLabel;
    cmbIntervalAdvanced: TComboEdit;
    pnlStyleParams: TLayout;
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
    chkStyleShowLegend: TCheckBox;
    cmbStyleRampList: TGIS_ColorRampComboBox;
    chkReverseRamp: TCheckBox;
    chkStyleDiscrete: TCheckBox;
    chkShowAllRamps: TCheckBox;

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
    procedure initForm     ; override;

    /// <inheritdoc/>
    procedure initControls ; override;

    /// <inheritdoc/>
    procedure showForm     ; override;

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
    /// <param name="_resProc">
    ///   parameters to be altered
    /// </param>
    procedure Execute      ( const _layer   : TGIS_LayerVector ;
                             const _type    : TGIS_ShapeType   ;
                             const _params  : TGIS_ParamsList  ;
                             const _onhelp  : TGIS_HelpEvent   ;
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
    /// <param name="_type">
    ///   type of expected render feature or TGIS_ShapeType.Unknown
    /// </param>
    /// <param name="_resProc">
    ///   parameters to be altered
    /// </param>
    procedure Execute      ( const _layer   : TGIS_LayerVector ;
                             const _type    : TGIS_ShapeType   ;
                             const _params  : TGIS_ParamsList  ;
                             const _resProc : TProc<TModalResult>
                           ) ; overload;

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
  Math,
  Lider.CG.GIS.GeoSqlQuery,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoUtils,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoXmlDoc,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoClassification,
  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.FMX.GeoControlSizeForm,
  Lider.CG.GIS.FMX.GeoControlFieldFactor,
  Lider.CG.GIS.FMX.GeoControlStatistics,
  Lider.CG.GIS.FMX.GeoControlHelper ;

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

  procedure TGIS_ControlLegendVectorWiz.actListUpdate(Sender: TObject);
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
         rdbRenderMarker.IsChecked or rdbRenderLine.IsChecked ;
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

            bl := rdbAnalyzeUnique.IsChecked ;
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

            bl := rdbAnalyzeContinous.IsChecked ;
              lblAnalyzeMin.Enabled    := bl ;
              edtAnalyzeMin.Enabled    := bl ;
              lblAnalyzeMax.Enabled    := bl ;
              edtAnalyzeMax.Enabled    := bl ;
              chkAnalyzeAvg.Enabled    := bl ;
              edtAnalyzeAvg.Enabled    := bl and chkAnalyzeAvg.IsChecked ;
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
              if rdbStyleColorRange.IsChecked then begin
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
              else if rdbStyleColorRamp.IsChecked then begin
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
              else if rdbStyleSize.IsChecked then begin
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
    modal_res : Integer ;

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
          if rdbLimitedScan.IsChecked then begin
            limit := iUniqueSearchLimit;
            ext   := objLayer.Viewer.Ref.VisibleExtent ;
          end
          else begin
            limit := GIS_MAX_INTEGER ;
            ext := objLayer.Viewer.Ref.Extent ;
          end ;

          shp := objLayer.FindFirst( ext, '' ) ;

          if Assigned( shp ) then begin
            sql.Parse( shp, 0 ) ;
            if objLayer.MustCalculateStatistics then begin
              frm_stats := TGIS_ControlStatistics.Create( oMainForm ) ;
              frm_stats.Execute(
                objLayer,
                pOnHelp,
                procedure( _modal_result : TModalResult )
                begin
                  modal_res := _modal_result ;
                  if ( modal_res = mrCancel ) then
                    objLayer.Statistics.ResetModified ;
                end
              ) ;
              if ( modal_res = mrCancel ) then
                exit( False ) ;
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

      rdbAnalyzeUnique.IsChecked    := False ;
      rdbAnalyzeContinous.IsChecked := False ;
      rdbAnalyzeContinous.Enabled := not bStrings  ;

      if bStrings or ( not ( overUnique or overRecords ) ) then
        rdbAnalyzeUnique.IsChecked := True
      else
        rdbAnalyzeContinous.IsChecked := True ;

      Result := True ;
    end ;

    procedure set_render_type ;
    begin
      case oType of
        TGIS_ShapeType.Point,
        TGIS_ShapeType.MultiPoint : rdbRenderMarker.IsChecked := True ;
        TGIS_ShapeType.Arc        : rdbRenderLine.IsChecked   := True ;
        TGIS_ShapeType.Polygon,
        TGIS_ShapeType.MultiPatch : rdbRenderArea.IsChecked   := True ;
        else                        begin
                                      Assert( False, GIS_RS_ERR_UNTESTED ) ;
                                      rdbRenderMarker.IsChecked := True ;
                                    end ;
      end ;
    end ;

    procedure set_render_style ;
    begin
      if rdbRenderSize.IsChecked or rdbRenderOutlineWidth.IsChecked then begin
        rdbStyleSize.IsChecked       := True ;
        rdbStyleSize.Enabled         := True ;
        rdbStyleColorRange.Enabled   := False ;
        rdbStyleColorRamp.Enabled    := False ;
        cmbStyleSizeStart.Enabled    := True ;
        cmbStyleSizeEnd.Enabled      := True ;
        cmbStyleColorStart.Enabled   := False ;
        cmbStyleColorEnd.Enabled     := False ;
        cmbStyleRampList.Enabled     := False ;
        chkReverseRamp.Enabled       := False ;
        chkStyleDiscrete.Enabled     := False ;
        chkShowAllRamps.Enabled      := False ;
        cmbStyleSizeStart.Value      := 'SIZE:1 twips' ;
        cmbStyleSizeEnd.Value        := 'SIZE:24 pt' ;
      end
      else if rdbRenderColor.IsChecked or rdbRenderOutlineColor.IsChecked then begin
        rdbStyleColorRange.Enabled   := True ;
        rdbStyleColorRamp.Enabled    := True ;
        rdbStyleSize.Enabled         := False ;
        rdbStyleColorRange.IsChecked := True ;
        cmbStyleSizeStart.Enabled    := False ;
        cmbStyleSizeEnd.Enabled      := False ;
        cmbStyleColorStart.Enabled   := True ;
        cmbStyleColorEnd.Enabled     := True ;
        cmbStyleRampList.Enabled     := True ;
        chkReverseRamp.Enabled       := True ;
        chkStyleDiscrete.Enabled     := True ;
        chkShowAllRamps.Enabled      := True ;
        cmbStyleColorStart.Value     := 'ARGB:FFEDF8E9' ;
        cmbStyleColorEnd.Value       := 'ARGB:FF008000' ;

        if iMode = 0 then begin
          if rdbAnalyzeUnique.IsChecked then begin
            rdbStyleColorRange.Enabled   := False ;
            rdbStyleColorRamp.IsChecked  := True ;
            cmbStyleColorStart.Enabled   := False ;
            cmbStyleColorEnd.Enabled     := False ;
          end
          else begin
            rdbStyleColorRamp.Enabled    := False ;
            rdbStyleColorRange.IsChecked := True ;
            cmbStyleRampList.Enabled     := False ;
            chkReverseRamp.Enabled       := False ;
            chkStyleDiscrete.Enabled     := False ;
            chkShowAllRamps.Enabled      := False ;
          end ;
        end ;
      end ;
    end ;

  begin
    case iPage of
      -1 : // Choose page
          begin
            if rdbChooseSimple.IsChecked then begin
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
              chkReverseRamp.IsChecked := False ;
              chkStyleDiscrete.IsChecked := False ;
              chkShowAllRamps.IsChecked := False ;

              method := cmbMethodAdvanced.Items[cmbMethodAdvanced.ItemIndex] ;
              if ( method = GIS_RS_CLASSIFY_METHOD_QR ) or
                 ( method = GIS_RS_CLASSIFY_METHOD_SD ) or
                 ( method = GIS_RS_CLASSIFY_METHOD_SDC ) then
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
              if rdbAnalyzeUnique.IsChecked and overUnique then
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
              if chkAnalyzeAvg.IsChecked and IsStringEmpty( edtAnalyzeAvg.Text ) then
                exit ;

              chkReverseRamp.IsChecked := False ;
              chkShowAllRamps.IsChecked := False ;

              if rdbAnalyzeUnique.IsChecked then begin
                chkStyleDiscrete.IsChecked := True ;
                cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
                // default is 'Unique'
                cmbStyleRampList.ItemIndex := cmbStyleRampList.Count - 4 ;
              end
              else begin
                // will be used in future (color ramps in renderer)
                chkStyleDiscrete.IsChecked := False ;
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
              pnlRender.Visible       := False ;
              pnlStyleParams.Visible  := True  ;
              btnNextPage.Visible     := False ;
              btnOK.Visible           := True  ;

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

  procedure TGIS_ControlLegendVectorWiz.actDiscreteClick(Sender: TObject);
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

  procedure TGIS_ControlLegendVectorWiz.actReverseClick(Sender: TObject);
  var
    idx : Integer ;
  begin
    idx := cmbStyleRampList.ItemIndex ;
    cmbStyleRampList.Reverse := chkReverseRamp.IsChecked ;
    cmbStyleRampList.ItemIndex := idx ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.actShowAllClick(Sender: TObject);
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
        if rdbAnalyzeUnique.IsChecked then begin
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

            if      rdbRenderMarker.IsChecked then begin
                    if      rdbRenderSize.IsChecked then
                            sec.Marker.Size := size + 15
                    else if rdbRenderColor.IsChecked then
                            sec.Marker.Color := color
                    else if rdbRenderOutlineWidth.IsChecked then
                            sec.Marker.OutlineWidth := size
                    else if rdbRenderOutlineColor.IsChecked then
                            sec.Marker.OutlineColor := color
                    else begin
                      Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end ;
                      sec.Marker.ShowLegend := chkStyleShowLegend.IsChecked ;
            end
            else if rdbRenderLine.IsChecked then begin
                    if      rdbRenderSize.IsChecked then
                            sec.Line.Width := size
                    else if rdbRenderColor.IsChecked then
                            sec.Line.Color := color
                    else if rdbRenderOutlineWidth.IsChecked then
                            sec.Line.OutlineWidth := size
                    else if rdbRenderOutlineColor.IsChecked then
                            sec.Line.OutlineColor := color
                    else begin
                      Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end;
                      sec.Line.ShowLegend := chkStyleShowLegend.IsChecked ;
            end
            else if rdbRenderArea.IsChecked then begin
                    if      rdbRenderColor.IsChecked then
                            sec.Area.Color := color
                    else if rdbRenderOutlineWidth.IsChecked then
                            sec.Area.OutlineWidth := size
                    else if rdbRenderOutlineColor.IsChecked then
                            sec.Area.OutlineColor := color
                    else begin
                      Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end ;
                      sec.Area.ShowLegend := chkStyleShowLegend.IsChecked ;
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
          if      rdbRenderMarker.IsChecked then begin
                  if      rdbRenderSize.IsChecked then
                          sec.Marker.Size := 150
                  else if rdbRenderColor.IsChecked then
                          sec.Marker.Color := TGIS_Color.Red
                  else if rdbRenderOutlineWidth.IsChecked then
                          sec.Marker.OutlineWidth := 0
                  else if rdbRenderOutlineColor.IsChecked then
                          sec.Marker.OutlineColor := TGIS_Color.Black
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                    sec.Marker.ShowLegend := (overUnique or overRecords) and
                                              chkStyleShowLegend.IsChecked ;
          end
          else if rdbRenderLine.IsChecked then begin
                  if      rdbRenderSize.IsChecked then
                          sec.Line.Width := 1
                  else if rdbRenderColor.IsChecked then
                          sec.Line.Color := TGIS_Color.Black
                  else if rdbRenderOutlineWidth.IsChecked then
                          sec.Line.OutlineWidth := 0
                  else if rdbRenderOutlineColor.IsChecked then
                          sec.Line.OutlineColor := TGIS_Color.Black
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end;
                    sec.Line.ShowLegend := (overUnique or overRecords) and
                                            chkStyleShowLegend.IsChecked ;
          end
          else if rdbRenderArea.IsChecked then begin
                  if      rdbRenderColor.IsChecked then
                          sec.Area.Color := TGIS_Color.Gray
                  else if rdbRenderOutlineWidth.IsChecked then
                          sec.Area.OutlineWidth := 1
                  else if rdbRenderOutlineColor.IsChecked then
                          sec.Area.OutlineColor := TGIS_Color.Black
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                    sec.Area.ShowLegend := (overUnique or overRecords) and
                                            chkStyleShowLegend.IsChecked ;
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

          if chkAnalyzeAvg.IsChecked then begin
            if bDates then begin
              sec.Render.MinVal   := Double(StrToDateTime( edtAnalyzeAvg.Text )) ;
              sec.Render.MaxVal   := Double(StrToDateTime( edtAnalyzeMax.Text )) ;
            end
            else begin
              sec.Render.MinVal   := DotStrToFloat( edtAnalyzeAvg.Text ) ;
              sec.Render.MaxVal   := DotStrToFloat( edtAnalyzeMax.Text ) ;
            end ;
            if chkAnalyzeLog.IsChecked then sec.Render.Zones := -5
                                       else sec.Render.Zones :=  5 ;

            if bDates then begin
              sec.Render.MinValEx := Double(StrToDateTime( edtAnalyzeMin.Text )) ;
              sec.Render.MaxValEx := Double(StrToDateTime( edtAnalyzeAvg.Text )) ;
            end
            else begin
              sec.Render.MinValEx := DotStrToFloat( edtAnalyzeMin.Text ) ;
              sec.Render.MaxValEx := DotStrToFloat( edtAnalyzeAvg.Text ) ;
            end;
            if chkAnalyzeLog.IsChecked then sec.Render.ZonesEx := -5
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
            if chkAnalyzeLog.IsChecked then sec.Render.Zones := -5
                                       else sec.Render.Zones :=  5 ;
            sec.Render.MinValEx := 0 ;
            sec.Render.MaxValEx := 0 ;
            sec.Render.ZonesEx  := 0 ;
          end ;

          size  := GIS_RENDER_SIZE  ;
          color := TGIS_Color.RenderColor ;

          if      rdbRenderMarker.IsChecked then begin
                  if      rdbRenderSize.IsChecked then
                          sec.Marker.Size := size
                  else if rdbRenderColor.IsChecked then
                          sec.Marker.Color := color
                  else if rdbRenderOutlineWidth.IsChecked then
                          sec.Marker.OutlineWidth := size
                  else if rdbRenderOutlineColor.IsChecked then
                          sec.Marker.OutlineColor := color
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                    sec.Marker.ShowLegend := chkStyleShowLegend.IsChecked ;
          end
          else if rdbRenderLine.IsChecked then begin
                  if      rdbRenderSize.IsChecked then
                          sec.Line.Width := size
                  else if rdbRenderColor.IsChecked then
                          sec.Line.Color := color
                  else if rdbRenderOutlineWidth.IsChecked then
                          sec.Line.OutlineWidth := size
                  else if rdbRenderOutlineColor.IsChecked then
                          sec.Line.OutlineColor := color
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                    sec.Line.ShowLegend := chkStyleShowLegend.IsChecked ;
          end
          else if rdbRenderArea.IsChecked then begin
                  if      rdbRenderColor.IsChecked then
                          sec.Area.Color := color
                  else if rdbRenderOutlineWidth.IsChecked then
                          sec.Area.OutlineWidth := size
                  else if rdbRenderOutlineColor.IsChecked then
                          sec.Area.OutlineColor := color
                  else begin
                    Assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                    sec.Area.ShowLegend := chkStyleShowLegend.IsChecked ;
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
    i           : Integer ;
    ci          : Integer ;
    fname       : String ;
    method      : String ;
    prm         : TGIS_ParamsList ;
    sec         : TGIS_ParamsSectionVector ;
    classifier  : TGIS_ClassificationVector ;
    frm_stats   : TGIS_ControlStatistics ;
    modal_res   : Integer ;
  begin
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
        classifier.ShowLegend       := chkStyleShowLegend.IsChecked ;

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
        if rdbStyleColorRamp.IsChecked then begin
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
        if rdbRenderSize.IsChecked then
          classifier.RenderType := TGIS_ClassificationRenderType.Size
        else if rdbRenderColor.IsChecked then
          classifier.RenderType := TGIS_ClassificationRenderType.Color
        else if rdbRenderOutlineWidth.IsChecked then
          classifier.RenderType := TGIS_ClassificationRenderType.OutlineWidth
        else if rdbRenderOutlineColor.IsChecked then
          classifier.RenderType := TGIS_ClassificationRenderType.OutlineColor
        else
          classifier.RenderType := TGIS_ClassificationRenderType.Color ;

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
            end
          ) ;
          if ( modal_res = mrCancel ) then
            exit( False ) ;
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
    dlg : TGIS_ControlSizeForm ;
    frm : TGIS_ControlFieldFactor ;
    val : String ;
  begin
    Result := '' ;

    if _value = GIS_PARAMTXT_TYPE_FIELD then begin
      frm := TGIS_ControlFieldFactor.Create( Self ) ;
      frm.FillFields( cmbFormulaAdvanced.Items ) ;
      frm.FillUnits( TGIS_FieldFactorUnitsType.Size ) ;

      frm.Execute(
        pOnHelp,
        procedure( _modal_result : TModalResult )
        begin
          if _modal_result <> mrOK then
            exit ;

            val := ConstructNumberAsText(
                     frm.cmbFields.Items[frm.cmbFields.ItemIndex],
                     frm.spnFactor.Text,
                     frm.cmbUnits.Items[frm.cmbUnits.ItemIndex]
                  ) ;
          TGIS_ComboBoxAbstract( _sender ).DelayedUpdate( val ) ;
        end
      ) ;
    end
    else
    if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      dlg := TGIS_ControlSizeForm.Create( Self ) ;
      dlg.FillUnits( False ) ;
      dlg.Execute(
        pOnHelp,
        procedure( _modal_result : TModalResult )
          begin
            if _modal_result <> mrOK then
              exit ;

            TGIS_ComboBoxAbstract( _sender ).DelayedUpdate( dlg.CustomSize ) ;
          end
      ) ;
    end;
  end ;

  function TGIS_ControlLegendVectorWiz.doCustomColor(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    dlg : TGIS_ColorDialog ;
    frm : TGIS_ControlFieldFactor ;
    val : String ;
  begin
    Result := '' ;

    if _value = GIS_PARAMTXT_TYPE_FIELD then begin
      frm := TGIS_ControlFieldFactor.Create( Self ) ;
      frm.FillFields( cmbFormula.Items ) ;
      frm.FillUnits( TGIS_FieldFactorUnitsType.NoScale ) ;

      frm.Execute(
        pOnHelp,
        procedure( _modal_result : TModalResult )
        begin
          if _modal_result <> mrOK then
            exit ;

          val := ConstructNumberAsText(
                   frm.cmbFields.Items[frm.cmbFields.ItemIndex],
                   frm.spnFactor.Text,
                   frm.cmbUnits.Items[frm.cmbUnits.ItemIndex]
                 ) ;
          TGIS_ComboBoxAbstract( _sender ).DelayedUpdate( val ) ;
        end
      ) ;
    end
    else
    if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      // Pass oParentWindow as the owner for positioning
      // the form in the center of LegendForm
      dlg := TGIS_ColorDialog.Create( self ) ;
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


//==============================================================================
// Public events & methods
//==============================================================================

  procedure TGIS_ControlLegendVectorWiz.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RENDER ) ;
    Self.ClientHeight := 411 ;
    Self.ClientWidth := 734 ;
    Self.Name := 'TGIS_ControlLegendVectorWiz' ;
    Self.OnCreate := FormCreate ;
    Self.OnDestroy := FormDestroy ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.initControls ;
  var
    t, l : Single ;
    i : Integer ;
  begin
    pnlFormula := TLayout.Create( oMainForm ) ;
    pnlFormula.Parent := oMainForm ;
    pnlFormula.Position.X := 8 ;
    pnlFormula.Position.Y := 8 ;
    pnlFormula.Size.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlFormula.Size.Height := 285 ;
    {$ELSE}
      pnlFormula.Size.Height := 185 ;
    {$ENDIF}
    pnlFormula.Size.PlatformDefault := False ;
    pnlFormula.TabOrder := 0 ;

    t := 16 ;

    lblFormula := TLabel.Create( pnlFormula ) ;
    lblFormula.Parent := pnlFormula ;
    lblFormula.Position.Y := t ;
    PlaceControl( BiDiMode, nil, lblFormula, 8, -1 ) ;
    lblFormula.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_FORMULA ) ;
    lblFormula.FixSize ;

    t := t + lblFormula.Height ;

    cmbFormula := TComboEdit.Create( pnlFormula ) ;
    cmbFormula.Parent := pnlFormula ;
    cmbFormula.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                             TInteractiveGesture.DoubleTap] ;
    cmbFormula.TabOrder := 0 ;
    cmbFormula.ItemIndex := -1 ;
    cmbFormula.Position.Y := t ;
    PlaceControl( BiDiMode, nil, cmbFormula, 8, -1 ) ;
    cmbFormula.Size.PlatformDefault := False ;
    cmbFormula.OnChange := actListUpdate ;
    cmbFormula.KillFocusByReturn := True ;

    t := t + cmbFormula.Height + 16 ;

    rdbLimitedScan := TRadioButton.Create( pnlFormula ) ;
    rdbLimitedScan.GroupName := Self.ClassName  ;
    rdbLimitedScan.Parent := pnlFormula;
    rdbLimitedScan.Position.Y := t ;
    rdbLimitedScan.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbLimitedScan, 8, -1 ) ;
    rdbLimitedScan.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBLIMITSCAN ) ;
    rdbLimitedScan.TabOrder := 1 ;
    rdbLimitedScan.IsChecked := True ;
    rdbLimitedScan.FixSize ;

    t := t + rdbLimitedScan.Height + 8 ;

    rdbFullScan := TRadioButton.Create( pnlFormula ) ;
    rdbFullScan.GroupName := Self.ClassName  ;
    rdbFullScan.Parent := pnlFormula;
    rdbFullScan.Position.Y := t ;
    rdbFullScan.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbFullScan, 8, -1 ) ;
    rdbFullScan.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBFULLSCAN ) ;
    rdbFullScan.TabOrder := 2 ;
    rdbFullScan.FixSize ;

    pnlAnalyze := TLayout.Create( oMainForm ) ;
    pnlAnalyze.Parent := oMainForm ;
    pnlAnalyze.Position.X := 370 ;
    pnlAnalyze.Position.Y := 8 ;
    pnlAnalyze.Size.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlFormula.Size.Height := 285 ;
    {$ELSE}
      pnlFormula.Size.Height := 185 ;
    {$ENDIF}
    pnlAnalyze.Size.PlatformDefault := False ;
    pnlAnalyze.TabOrder := 1 ;

    t := 8 ;

    rdbAnalyzeUnique := TRadioButton.Create( pnlAnalyze ) ;
    rdbAnalyzeUnique.Parent := pnlAnalyze ;
    rdbAnalyzeUnique.GroupName := 'TGIS_ControlLegendVectorWiz_mode' ;
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
    rdbAnalyzeContinous.GroupName := 'TGIS_ControlLegendVectorWiz_mode' ;
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

    chkAnalyzeAvg := TCheckBox.Create( pnlAnalyze ) ;
    chkAnalyzeAvg.Parent := pnlAnalyze ;
    chkAnalyzeAvg.Position.Y := t ;
    PlaceControl( BiDiMode, nil, chkAnalyzeAvg, 184, 135 ) ;
    chkAnalyzeAvg.Size.PlatformDefault := False ;
    chkAnalyzeAvg.TabOrder := 5 ;
    chkAnalyzeAvg.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_CHKAVG ) ;
    chkAnalyzeAvg.OnChange := actListUpdate ;

    t := t + chkAnalyzeAvg.Height ;

    edtAnalyzeAvg := TEdit.Create( pnlAnalyze ) ;
    edtAnalyzeAvg.Parent := pnlAnalyze ;
    edtAnalyzeAvg.Touch.InteractiveGestures := [TInteractiveGesture.LongTap,
                                                TInteractiveGesture.DoubleTap] ;
    edtAnalyzeAvg.TabOrder := 6 ;
    edtAnalyzeAvg.TextSettings.FontColor := TAlphaColorRec.Red ;
    edtAnalyzeAvg.Position.Y := t ;
    edtAnalyzeAvg.ClipChildren := True ;
    PlaceControl( BiDiMode, nil, edtAnalyzeAvg, 184, 135 ) ;
    edtAnalyzeAvg.Size.PlatformDefault := False ;
    edtAnalyzeAvg.FixSize ;
    edtAnalyzeAvg.KillFocusByReturn := True ;

    t := t + edtAnalyzeAvg.Height + 4 ;

    chkAnalyzeLog := TCheckBox.Create( pnlAnalyze ) ;
    chkAnalyzeLog.Parent := pnlAnalyze ;
    chkAnalyzeLog.Position.Y := t ;
    PlaceControl( BiDiMode, nil, chkAnalyzeLog, 184, 135 ) ;
    chkAnalyzeLog.Size.PlatformDefault := False ;
    chkAnalyzeLog.TabOrder := 7 ;
    chkAnalyzeLog.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_CHKLOG ) ;
    chkAnalyzeLog.OnChange := actListUpdate ;

    pnlRender := TLayout.Create( oMainForm ) ;
    pnlRender.Parent := oMainForm ;
    pnlRender.Position.X := 370 ;
    pnlRender.Position.Y := 191 ;
    pnlRender.Size.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlFormula.Size.Height := 285 ;
    {$ELSE}
      pnlFormula.Size.Height := 185 ;
    {$ENDIF}
    pnlRender.Size.PlatformDefault := False ;
    pnlRender.TabOrder := 2 ;

    grpRenderFeature := TGroupBox.Create( pnlRender ) ;
    grpRenderFeature.Parent := pnlRender ;
    grpRenderFeature.Position.Y := 8 ;
    grpRenderFeature.Size.Height := 121 ;
    PlaceControl( BiDiMode, nil, grpRenderFeature, 16, 153 ) ;
    grpRenderFeature.Size.PlatformDefault := False ;
    grpRenderFeature.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_GRPSHAPE ) ;
    grpRenderFeature.TabOrder := 0 ;

    rdbRenderMarker := TRadioButton.Create( grpRenderFeature ) ;
    rdbRenderMarker.Parent := grpRenderFeature ;
    rdbRenderMarker.GroupName := 'TGIS_ControlLegendVectorWiz_feature' ;
    rdbRenderMarker.IsChecked := True ;
    rdbRenderMarker.Position.Y := 24 ;
    rdbRenderMarker.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderMarker, 8, 137 ) ;
    rdbRenderMarker.Size.PlatformDefault := False ;
    rdbRenderMarker.TabOrder := 0 ;
    rdbRenderMarker.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBMARKER ) ;
    rdbRenderMarker.OnChange := actListUpdate ;
    rdbRenderMarker.FixSize ;

    rdbRenderLine := TRadioButton.Create( grpRenderFeature ) ;
    rdbRenderLine.Parent := grpRenderFeature ;
    rdbRenderLine.GroupName := 'TGIS_ControlLegendVectorWiz_feature' ;
    rdbRenderLine.Position.Y := 48 ;
    rdbRenderLine.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderLine, 8, 137 ) ;
    rdbRenderLine.Size.PlatformDefault := False ;
    rdbRenderLine.TabOrder := 1 ;
    rdbRenderLine.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBLINE ) ;
    rdbRenderLine.OnChange := actListUpdate ;
    rdbRenderLine.FixSize ;

    rdbRenderArea := TRadioButton.Create( grpRenderFeature ) ;
    rdbRenderArea.Parent := grpRenderFeature ;
    rdbRenderArea.GroupName := 'TGIS_ControlLegendVectorWiz_feature' ;
    rdbRenderArea.Position.Y := 72 ;
    rdbRenderArea.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderArea, 8, 137 ) ;
    rdbRenderArea.Size.PlatformDefault := False ;
    rdbRenderArea.TabOrder := 2 ;
    rdbRenderArea.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBAREA ) ;
    rdbRenderArea.OnChange := actListUpdate ;
    rdbRenderArea.FixSize ;

    grpRenderValue := TGroupBox.Create( pnlRender ) ;
    grpRenderValue.Parent := pnlRender ;
    grpRenderValue.Position.Y := 8 ;
    grpRenderValue.Size.Height := 121 ;
    PlaceControl( BiDiMode, nil, grpRenderValue, 184, 153 ) ;
    grpRenderValue.Size.PlatformDefault := False ;
    grpRenderValue.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_GRPVALUE ) ;
    grpRenderValue.TabOrder := 1 ;

    rdbRenderSize := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderSize.Parent := grpRenderValue ;
    rdbRenderSize.GroupName := 'TGIS_ControlLegendVectorWiz_value' ;
    rdbRenderSize.Position.Y := 24 ;
    rdbRenderSize.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderSize, 8, 137 ) ;
    rdbRenderSize.Size.PlatformDefault := False ;
    rdbRenderSize.TabOrder := 0 ;
    rdbRenderSize.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RBDSIZE ) ;
    rdbRenderSize.OnChange := actListUpdate ;
    rdbRenderSize.FixSize ;

    rdbRenderColor := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderColor.Parent := grpRenderValue ;
    rdbRenderColor.GroupName := 'TGIS_ControlLegendVectorWiz_value' ;
    rdbRenderColor.IsChecked := True ;
    rdbRenderColor.Position.Y := 48 ;
    rdbRenderColor.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderColor, 8, 137 ) ;
    rdbRenderColor.Size.PlatformDefault := False ;
    rdbRenderColor.TabOrder := 1 ;
    rdbRenderColor.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_RBDCOLOR ) ;
    rdbRenderColor.OnChange := actListUpdate ;
    rdbRenderColor.FixSize ;

    rdbRenderOutlineWidth := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderOutlineWidth.Parent := grpRenderValue ;
    rdbRenderOutlineWidth.GroupName := 'TGIS_ControlLegendVectorWiz_value' ;
    rdbRenderOutlineWidth.Position.Y := 72 ;
    rdbRenderOutlineWidth.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderOutlineWidth, 8, 137 ) ;
    rdbRenderOutlineWidth.Size.PlatformDefault := False ;
    rdbRenderOutlineWidth.TabOrder := 2 ;
    rdbRenderOutlineWidth.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_OUTLINEWIDTH ) ;
    rdbRenderOutlineWidth.OnChange := actListUpdate ;
    rdbRenderOutlineWidth.FixSize ;

    rdbRenderOutlineColor := TRadioButton.Create( grpRenderValue ) ;
    rdbRenderOutlineColor.Parent := grpRenderValue ;
    rdbRenderOutlineColor.GroupName := 'TGIS_ControlLegendVectorWiz_value' ;
    rdbRenderOutlineColor.Position.Y := 96 ;
    rdbRenderOutlineColor.Size.Height := 19 ;
    PlaceControl( BiDiMode, nil, rdbRenderOutlineColor, 8, 137 ) ;
    rdbRenderOutlineColor.Size.PlatformDefault := False ;
    rdbRenderOutlineColor.TabOrder := 3 ;
    rdbRenderOutlineColor.Text := _rsrcna( GIS_RS_LEGEND_WIZARD_OUTLINECOLOR ) ;
    rdbRenderOutlineColor.OnChange := actListUpdate ;
    rdbRenderOutlineColor.FixSize ;

    chkStyleShowLegend := TCheckBox.Create( pnlRender ) ;
    chkStyleShowLegend.Parent := pnlRender ;
    chkStyleShowLegend.Position.Y := 140 ;
    chkStyleShowLegend.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleShowLegend, 16, -1 ) ;
    chkStyleShowLegend.Text := _rsrc( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
    chkStyleShowLegend.TabOrder := 5 ;
    chkStyleShowLegend.IsChecked := True ;
    chkStyleShowLegend.OnClick := actListUpdate ;

    pnlChoose := TLayout.Create( oMainForm ) ;
    pnlChoose.Parent := oMainForm ;
    pnlChoose.Position.X := 8 ;
    pnlChoose.Position.Y := 8 ;
    pnlChoose.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlChoose.Size.Height := 285 ;
    {$ELSE}
      pnlChoose.Size.Height := 185 ;
    {$ENDIF}
    pnlChoose.Size.PlatformDefault := False ;

    rdbChooseSimple := TRadioButton.Create( pnlChoose ) ;
    rdbChooseSimple.Parent := pnlChoose;
    rdbChooseSimple.Position.Y := 70 ;
    rdbChooseSimple.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbChooseSimple, 90, -1 ) ;
    rdbChooseSimple.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_SIMPLE ) ;
    rdbChooseSimple.TabOrder := 1 ;
    rdbChooseSimple.IsChecked := True ;
    rdbChooseSimple.FixSize ;

    rdbChooseAdvanced := TRadioButton.Create( pnlChoose ) ;
    rdbChooseAdvanced.Parent := pnlChoose;
    {$IFDEF GIS_MOBILE_DIALOGS}
      rdbChooseAdvanced.Position.Y := 95 ;
    {$ELSE}
      rdbChooseAdvanced.Position.Y := 100 ;
    {$ENDIF}
    rdbChooseAdvanced.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbChooseAdvanced, 90, -1 ) ;
    rdbChooseAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_ADVANCED ) ;
    rdbChooseAdvanced.TabOrder := 2 ;
    rdbChooseAdvanced.FixSize ;

    pnlFormulaAdvanced := TLayout.Create( oMainForm ) ;
    pnlFormulaAdvanced.Parent := oMainForm ;
    pnlFormulaAdvanced.Position.X := 8 ;
    pnlFormulaAdvanced.Position.Y := 8 ;
    pnlFormulaAdvanced.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlFormulaAdvanced.Size.Height := 285 ;
    {$ELSE}
      pnlFormulaAdvanced.Size.Height := 185 ;
    {$ENDIF}
    pnlFormulaAdvanced.Size.PlatformDefault := False ;

    lblFormulaAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblFormulaAdvanced.Parent := pnlFormulaAdvanced ;
    lblFormulaAdvanced.Position.Y := 16 ;
    lblFormulaAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblFormulaAdvanced, 8, -1 ) ;
    lblFormulaAdvanced.AutoSize := False ;
    lblFormulaAdvanced.Text := _rsrc( GIS_RS_FIELDFACTOR_FIELD ) ;
    lblFormulaAdvanced.FixSize ;

    cmbFormulaAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbFormulaAdvanced.Parent := pnlFormulaAdvanced ;
    cmbFormulaAdvanced.Position.Y := 32 ;
    cmbFormulaAdvanced.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbFormulaAdvanced, 8, -1 ) ;
    cmbFormulaAdvanced.TabOrder := 0 ;
    cmbFormulaAdvanced.OnChange := actListUpdate ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      t := 64 ;
    {$ELSE}
      t := 70 ;
    {$ENDIF}
    lblMethodAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblMethodAdvanced.Parent := pnlFormulaAdvanced ;
    lblMethodAdvanced.Position.Y := t ;
    lblMethodAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblMethodAdvanced, 8, -1 ) ;
    lblMethodAdvanced.AutoSize := False ;
    lblMethodAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_METHOD ) ;
    lblMethodAdvanced.FixSize ;

    cmbMethodAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbMethodAdvanced.Parent := pnlFormulaAdvanced ;
    cmbMethodAdvanced.Position.Y := t+16 ;
    cmbMethodAdvanced.Height := 21 ;
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
    lblClassesAdvanced.Position.Y := t ;
    lblClassesAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblClassesAdvanced, 182, 80 ) ;
    lblClassesAdvanced.AutoSize := False ;
    lblClassesAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_CLASSES ) ;
    lblClassesAdvanced.FixSize ;

    cmbClassesAdvanced := TComboBox.Create( pnlFormulaAdvanced ) ;
    cmbClassesAdvanced.Parent := pnlFormulaAdvanced ;
    cmbClassesAdvanced.Position.Y := t + 16 ;
    cmbClassesAdvanced.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbClassesAdvanced, 182, 75 ) ;
    cmbClassesAdvanced.TabOrder := 0 ;
    for i := 1 to 9 do
      cmbClassesAdvanced.Items.Add( IntToStr( i ) ) ;
    cmbClassesAdvanced.ItemIndex := 4 ;
    cmbClassesAdvanced.OnChange := actListUpdate ;

    lblIntervalAdvanced := TLabel.Create( pnlFormulaAdvanced ) ;
    lblIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    lblIntervalAdvanced.Position.Y := t ;
    lblIntervalAdvanced.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblIntervalAdvanced, 270, 75 ) ;
    lblIntervalAdvanced.AutoSize := False ;
    lblIntervalAdvanced.Text := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_INTERVAL ) ;
    lblIntervalAdvanced.FixSize ;

    cmbIntervalAdvanced := TComboEdit.Create( pnlFormulaAdvanced ) ;
    cmbIntervalAdvanced.Parent := pnlFormulaAdvanced ;
    cmbIntervalAdvanced.Position.Y := t + 16 ;
    cmbIntervalAdvanced.Height := 21 ;
    PlaceControl( BiDiMode, nil, cmbIntervalAdvanced, 270, 75 ) ;
    cmbIntervalAdvanced.TabOrder := 0 ;
    cmbIntervalAdvanced.Text := '100' ;
    cmbIntervalAdvanced.OnChange := actListUpdate ;

    pnlStyleParams := TLayout.Create( oMainForm ) ;
    pnlStyleParams.Parent := oMainForm ;
    pnlStyleParams.Position.X := 8 ;
    pnlStyleParams.Position.Y := 0 ;
    pnlStyleParams.Width := 353 ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlStyleParams.Size.Height := 285 ;
    {$ELSE}
      pnlStyleParams.Size.Height := 185 ;
    {$ENDIF}
    pnlRender.Size.PlatformDefault := False ;

    rdbStyleSize := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleSize.Parent := pnlStyleParams;
    rdbStyleSize.Position.Y := 33 ;
    rdbStyleSize.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleSize, 8, 90 ) ;
    rdbStyleSize.Text := _rsrc( GIS_RS_LEGEND_PRM_SIZE ) ;
    rdbStyleSize.TabOrder := 1 ;
    rdbStyleSize.IsChecked := True ;
    rdbStyleSize.FixSize ;

    lblStyleSizeStart := TLabel.Create( pnlStyleParams ) ;
    lblStyleSizeStart.Parent := pnlStyleParams ;
    lblStyleSizeStart.Position.Y := 16 ;
    lblStyleSizeStart.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleSizeStart, 100, 121 ) ;
    lblStyleSizeStart.Text := _rsrc( GIS_RS_LEGEND_PRM_STARTSIZE ) ;
    lblStyleSizeStart.FixSize ;

    cmbStyleSizeStart := TGIS_SizeComboBox.Create( pnlStyleParams ) ;
    cmbStyleSizeStart.Parent := pnlStyleParams ;
    cmbStyleSizeStart.Position.Y := 33 ;
    PlaceControl( BiDiMode, nil, cmbStyleSizeStart, 100, 100 ) ;
    cmbStyleSizeStart.Fill( True, False, True, True ) ;
    cmbStyleSizeStart.CustomEvent := doCustomSize ;

    lblStyleSizeEnd := TLabel.Create( pnlStyleParams ) ;
    lblStyleSizeEnd.Parent := pnlStyleParams ;
    lblStyleSizeEnd.Position.Y := 16 ;
    lblStyleSizeEnd.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleSizeEnd, 220, 121 ) ;
    lblStyleSizeEnd.Text := _rsrc( GIS_RS_LEGEND_PRM_ENDSIZE ) ;
    lblStyleSizeEnd.FixSize ;

    cmbStyleSizeEnd := TGIS_SizeComboBox.Create( pnlStyleParams ) ;
    cmbStyleSizeEnd.Parent := pnlStyleParams ;
    cmbStyleSizeEnd.Position.Y := 33 ;
    PlaceControl( BiDiMode, nil, cmbStyleSizeEnd, 220, 100 ) ;
    cmbStyleSizeEnd.Fill( True, False, True, True ) ;
    cmbStyleSizeEnd.CustomEvent := doCustomSize ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      t := 78 ;
      l := 62 ;
    {$ELSE}
      t := 84 ;
      l := 68 ;
    {$ENDIF}
    rdbStyleColorRange := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRange.Parent := pnlStyleParams;
    rdbStyleColorRange.Position.Y := t ;
    rdbStyleColorRange.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRange, 8, 90 ) ;
    rdbStyleColorRange.Text := _rsrc( GIS_RS_LEGEND_PRM_COLOR ) ;
    rdbStyleColorRange.TabOrder := 1 ;
    rdbStyleColorRange.OnChange := actListUpdate ;
    rdbStyleColorRange.FixSize ;

    lblStyleColorStart := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorStart.Parent := pnlStyleParams ;
    lblStyleColorStart.Position.Y := l ;
    lblStyleColorStart.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorStart, 100, 121 ) ;
    lblStyleColorStart.Text := _rsrc( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
    lblStyleColorStart.FixSize ;

    cmbStyleColorStart := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorStart.Parent := pnlStyleParams ;
    cmbStyleColorStart.Position.Y := t ;
    cmbStyleColorStart.Height := 28 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorStart, 100, 100 ) ;
    cmbStyleColorStart.Fill( True, True ) ;
    cmbStyleColorStart.CustomEvent := doCustomColor ;

    lblStyleColorEnd := TLabel.Create( pnlStyleParams ) ;
    lblStyleColorEnd.Parent := pnlStyleParams ;
    lblStyleColorEnd.Position.Y := l ;
    lblStyleColorEnd.Height := 13 ;
    PlaceControl( BiDiMode, nil, lblStyleColorEnd, 220, 121 ) ;
    lblStyleColorEnd.Text := _rsrc( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
    lblStyleColorEnd.FixSize ;

    cmbStyleColorEnd := TGIS_ColorComboBox.Create( pnlStyleParams ) ;
    cmbStyleColorEnd.Parent := pnlStyleParams ;
    cmbStyleColorEnd.Position.Y := t ;
    cmbStyleColorEnd.Height := 28 ;
    PlaceControl( BiDiMode, nil, cmbStyleColorEnd, 220, 100 ) ;
    cmbStyleColorEnd.Fill( True, True ) ;
    cmbStyleColorEnd.CustomEvent := doCustomColor ;

    rdbStyleColorRamp := TRadioButton.Create( pnlStyleParams ) ;
    rdbStyleColorRamp.Parent := pnlStyleParams;
    rdbStyleColorRamp.Position.Y := 125 ;
    rdbStyleColorRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, rdbStyleColorRamp, 8, 90 ) ;
    rdbStyleColorRamp.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMPS ) ;
    rdbStyleColorRamp.TabOrder := 1 ;
    rdbStyleColorRamp.OnChange := actListUpdate ;
    rdbStyleColorRamp.IsChecked := False ;
    rdbStyleColorRamp.FixSize ;

    cmbStyleRampList := TGIS_ColorRampComboBox.Create( pnlStyleParams ) ;
    cmbStyleRampList.Parent := pnlStyleParams ;
    cmbStyleRampList.Position.Y := 125 ;
    cmbStyleRampList.Height := 22 ;
    PlaceControl( BiDiMode, nil, cmbStyleRampList, 100, 221 ) ;
    cmbStyleRampList.TabOrder := 5 ;
    cmbStyleRampList.ItemIndex := 0 ;

    chkReverseRamp := TCheckBox.Create( pnlStyleParams ) ;
    chkReverseRamp.Parent := pnlStyleParams ;
    chkReverseRamp.Position.Y := 151 ;
    chkReverseRamp.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkReverseRamp, 100, 90 ) ;
    chkReverseRamp.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_REVERSE ) ;
    chkReverseRamp.IsChecked := False ;
    chkReverseRamp.TabOrder := 7 ;
    chkReverseRamp.OnChange := actReverseClick ;

    chkStyleDiscrete := TCheckBox.Create( pnlStyleParams ) ;
    chkStyleDiscrete.Parent := pnlStyleParams ;
    chkStyleDiscrete.Position.Y := 151 ;
    chkStyleDiscrete.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkStyleDiscrete, 195, 100 ) ;
    chkStyleDiscrete.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_DISCRETE ) ;
    chkStyleDiscrete.OnChange := actDiscreteClick ;
    chkStyleDiscrete.TabOrder := 8 ;

    chkShowAllRamps := TCheckBox.Create( pnlStyleParams ) ;
    chkShowAllRamps.Parent := pnlStyleParams ;
    chkShowAllRamps.Position.Y := 151 ;
    chkShowAllRamps.Height := 17 ;
    PlaceControl( BiDiMode, nil, chkShowAllRamps, 290, 100 ) ;
    chkShowAllRamps.Text := _rsrc( GIS_RS_LEGEND_WIZARD_RAMP_SHOWALL ) ;
    chkShowAllRamps.OnChange := actShowAllClick ;
    chkShowAllRamps.TabOrder := 9 ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Position.Y := ClientHeight - btnHelp.Height - 8 ;
      PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;
      btnHelp.TabOrder := 3 ;

      btnCancel.Position.Y := 100 - btnCancel.Height - 8 ;
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
      btnOK.Position.Y := 100 - btnOK.Height - 8 ;
      PlaceControl( BiDiMode, btnPreviousPage, btnOK, -8, 80 ) ;
      btnOK.TabOrder := 6 ;
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

    ClientWidth  := TruncS(
      pnlFormula.Position.X + pnlFormula.Width + pnlFormula.Position.X ) ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      ClientHeight := TruncS(
        pnlFormula.Position.Y + pnlFormula.Height + pnlFormula.Position.Y ) ;
    {$ELSE}
      ClientHeight := TruncS( pnlFormula.Position.Y + pnlFormula.Height + 8 +
                              btnCancel.Height + 8 ) ;
    {$ENDIF}

  end ;

  procedure TGIS_ControlLegendVectorWiz.showForm;
  begin
    lblFormula.FixSize ;
    lblAnalyzeUnique.FixSize ;
    lblAnalyzeMin.FixSize ;
    lblAnalyzeMax.FixSize ;
    lblFormulaAdvanced.FixSize ;
    rdbLimitedScan.IsChecked := True ;
    rdbChooseSimple.FixSize ;
    rdbChooseSimple.IsChecked := True ;
    rdbChooseAdvanced.FixSize ;

    pnlChoose.Visible     := True ;
    pnlChoose.Position.X  := pnlChoose.Position.X  ;
    pnlChoose.Position.Y  := pnlChoose.Position.Y   ;

    pnlFormula.Visible      := False ;
    pnlFormula.Position.X   := pnlFormula.Position.X  ;
    pnlFormula.Position.Y   := pnlFormula.Position.Y   ;

    pnlFormulaAdvanced.Visible    := False ;
    pnlFormulaAdvanced.Position.X := pnlFormula.Position.X  ;
    pnlFormulaAdvanced.Position.Y := pnlFormula.Position.Y   ;

    pnlAnalyze.Visible      := False ;
    pnlAnalyze.Position.X   := pnlFormula.Position.X  ;
    pnlAnalyze.Position.Y   := pnlFormula.Position.Y   ;

    pnlRender.Visible     := False ;
    pnlRender.Position.X  := pnlFormula.Position.X  ;
    pnlRender.Position.Y  := pnlFormula.Position.Y   ;

    pnlStyleParams.Visible    := False ;
    pnlStyleParams.Position.X := pnlFormula.Position.X  ;
    pnlStyleParams.Position.Y := pnlFormula.Position.Y   ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnOK.Visible := False ;
      btnOK.Position.X := btnNextPage.Position.X ;
      btnOK.Position.Y := btnNextPage.Position.Y ;
    {$ENDIF}

    actListUpdate( nil ) ;
  end;

  procedure TGIS_ControlLegendVectorWiz.Execute(
    const _layer   : TGIS_LayerVector ;
    const _type    : TGIS_ShapeType   ;
    const _params  : TGIS_ParamsList  ;
    const _onhelp  : TGIS_HelpEvent   ;
    const _resProc : TProc<TModalResult>
  ) ;
  var
    lst     : TStringList ;
    i       : Integer     ;
    wasjoin : Boolean     ;
    lstFields   : TStringList ;
    lstFieldsN  : TStringList ;
    {$IFNDEF GIS_NODB}
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
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

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
                  [ftWord,ftSmallint,ftInteger,ftLargeint,ftCurrency,ftBCD,ftFMTBcd]
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
                  [ftWord,ftSmallint,ftInteger,ftLargeint, ftCurrency,ftBCD,ftFMTBcd]
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

    ShowModalEx( _resProc ) ;
  end ;

  procedure TGIS_ControlLegendVectorWiz.Execute(
    const _layer   : TGIS_LayerVector ;
    const _type    : TGIS_ShapeType   ;
    const _params  : TGIS_ParamsList  ;
    const _resProc : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( _layer, _type, _params, hlp, _resProc ) ;
  end;

//==================================== END =====================================
end.

