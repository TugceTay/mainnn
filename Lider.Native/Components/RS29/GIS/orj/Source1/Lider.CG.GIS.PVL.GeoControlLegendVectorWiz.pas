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

{$IFDEF DCC}
  unit Lider.CG.GIS.PVL.GeoControlLegendVectorWiz ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.PVL.GeoControlLegendVectorWiz"'}
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
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoSqlQuery,
    Lider.CG.GIS.PVL.GeoControlStatistics,
    Lider.CG.GIS.PVL.GeoPvl,
    Lider.CG.GIS.PVL.GeoPvlForms,
    Lider.CG.GIS.PVL.GeoPvlWidgets ;
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
  TGIS_PvlControlLegendVectorWiz = class( TGIS_PvlModalWizard )
    private
      // simple/advanced classification choice
      pnlChoose             : TGIS_PvlPage              ;
      grpChoose             : String                    ;
      rdbChooseSimple       : TGIS_PvlRadioButton       ;
      rdbChooseAdvanced     : TGIS_PvlRadioButton       ;

      // simple -> formula and limited/full scan choice
      pnlFormula            : TGIS_PvlPage              ;
      lblFormula            : TGIS_PvlLabel             ;
      cmbFormula            : TGIS_PvlComboEdit         ;
      grpFormula            : String                    ;
      rdbLimitedScan        : TGIS_PvlRadioButton       ;
      rdbFullScan           : TGIS_PvlRadioButton       ;

      // simple -> unique/continuous choice
      pnlAnalyze            : TGIS_PvlPage              ;
      grpAnalyze            : String                    ;
      rdbAnalyzeUnique      : TGIS_PvlRadioButton       ;
      lblAnalyzeUnique      : TGIS_PvlLabel             ;
      pnlAnalyzeUnique      : TGIS_PvlPanel             ;
      memAnalyzeUnique      : TGIS_PvlListBox           ;
      rdbAnalyzeContinous   : TGIS_PvlRadioButton       ;
      lblAnalyzeMin         : TGIS_PvlLabel             ;
      edtAnalyzeMin         : TGIS_PvlEdit              ;
      lblAnalyzeMax         : TGIS_PvlLabel             ;
      edtAnalyzeMax         : TGIS_PvlEdit              ;
      chkAnalyzeAvg         : TGIS_PvlCheckBox          ;
      edtAnalyzeAvg         : TGIS_PvlEdit              ;
      chkAnalyzeLog         : TGIS_PvlCheckBox          ;

      // features and render type choice
      pnlRender             : TGIS_PvlPage              ;
      grpbxRenderFeature    : TGIS_PvlGroupBox          ;
      grpRenderFeature      : String                    ;
      rdbRenderMarker       : TGIS_PvlRadioButton       ;
      rdbRenderLine         : TGIS_PvlRadioButton       ;
      rdbRenderArea         : TGIS_PvlRadioButton       ;
      grpbxRenderValue      : TGIS_PvlGroupBox          ;
      grpRenderValue        : String                    ;
      rdbRenderSize         : TGIS_PvlRadioButton       ;
      rdbRenderOutlineWidth : TGIS_PvlRadioButton       ;
      rdbRenderColor        : TGIS_PvlRadioButton       ;
      rdbRenderOutlineColor : TGIS_PvlRadioButton       ;
      chkStyleShowLegend    : TGIS_PvlCheckBox          ;

      // colors/ramps choice
      pnlStyleParams        : TGIS_PvlPage              ;
      grpStyle              : String                    ;
      rdbStyleSize          : TGIS_PvlRadioButton       ;
      lblStyleSizeStart     : TGIS_PvlLabel             ;
      cmbStyleSizeStart     : TGIS_PvlSizeComboBox      ;
      lblStyleSizeEnd       : TGIS_PvlLabel             ;
      cmbStyleSizeEnd       : TGIS_PvlSizeComboBox      ;
      rdbStyleColorRange    : TGIS_PvlRadioButton       ;
      lblStyleColorStart    : TGIS_PvlLabel             ;
      cmbStyleColorStart    : TGIS_PvlColorComboBox     ;
      rdbStyleColorRamp     : TGIS_PvlRadioButton       ;
      lblStyleColorEnd      : TGIS_PvlLabel             ;
      cmbStyleColorEnd      : TGIS_PvlColorComboBox     ;
      cmbStyleRampList      : TGIS_PvlColorRampWidget   ;

      // advanced
      pnlFormulaAdvanced    : TGIS_PvlPage              ;
      lblFormulaAdvanced    : TGIS_PvlLabel             ;
      cmbFormulaAdvanced    : TGIS_PvlComboBox          ;
      lblMethodAdvanced     : TGIS_PvlLabel             ;
      cmbMethodAdvanced     : TGIS_PvlComboBox          ;
      lblClassesAdvanced    : TGIS_PvlLabel             ;
      cmbClassesAdvanced    : TGIS_PvlComboBox          ;
      lblIntervalAdvanced   : TGIS_PvlLabel             ;
      cmbIntervalAdvanced   : TGIS_PvlComboEdit         ;

    private
      objLayer              : TGIS_LayerVector          ;
      objParams             : TGIS_ParamsList           ;
      valMin                : Variant                   ;
      valMax                : Variant                   ;
      valCnt                : Integer                   ;
      bStrings              : Boolean                   ;
      bDates                : Boolean                   ;
      lstUnique             : TGIS_VariantList          ;
      overUnique            : Boolean                   ;
      overRecords           : Boolean                   ;
      oType                 : TGIS_ShapeType            ;
      iUniqueLimit          : Integer                   ;
      iUniqueSearchLimit    : Integer                   ;
      iMode                 : Integer                   ;
      lstFields             : TStringList               ;
      lstFieldsN            : TStringList               ;

    public    
      /// <inheritdoc/>
      procedure BtnOKClick(_sender: TObject); override;

    private
      procedure createFormula         ;
      procedure createAdvancedFormula ;
      procedure createChoose          ;
      procedure createAnalyze         ;
      procedure createFeatures        ;
      procedure createColors          ;
      procedure createOrder           ;
    private
      procedure set_render_type       ;
      procedure set_render_style      ;
      procedure set_fields            ;
      procedure check_render          ;
      procedure doClassifySimple      ;
      function  doClassifyAdvanced    : Boolean ;
    private
      // Because of how FMX mobile works in order to keep
      // code multiplatform we have to use helping procedures
      // so we can emulate proper behaviour on mobile devices.
      procedure doOnAnalyzeEnterWork  ( shp : TGIS_Shape ;
                                        sql : TGIS_SqlQuery ;
                                        limit : Integer ;
                                        first_run : Boolean ;
                                        lst : TStringList )    ;
      procedure doClassifyWork        ( classifier     : TGIS_ClassificationVector ;
                                        prm            : TGIS_ParamsList
                                      ) ;
    private
      procedure onChooseExit            ( _sender : TObject ) ;
      procedure onFormulaEnter          ( _sender : TObject ) ;
      procedure onFormulaExit           ( _sender : TObject ) ;
      procedure onRenderEnter           ( _sender : TObject ) ;
      procedure onRenderExit            ( _sender : TObject ) ;
      procedure onAnalyzeEnter          ( _sender : TObject ) ;
      procedure onAnalyzeExit           ( _sender : TObject ) ;
      procedure onFormulaAdvancedExit   ( _sender : TObject ) ;
      procedure onMethodAdvancedChange  ( _sender : TObject ) ;
      procedure onParamStyleParamsEnter ( _sender : TObject ) ;
      procedure rdbAnalyzeChange        ( _sender : TObject ) ;
      procedure rdbStyleChange          ( _sender : TObject ) ;
      procedure cmbFormulaChange        ( _sender : TObject ) ;

    protected
      /// <inheritdodc/>
      procedure doDestroy          ; override;

    public
      /// <inheritdoc/>
      procedure DoInitForm         ; override;

      /// <inheritdoc/>
      procedure DoInitControls     ; override;

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
      /// <param name="_proc">
      ///   Action to be performed after closing modal form.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute  ( const _layer  : TGIS_LayerVector ;
                           const _type   : TGIS_ShapeType   ;
                           const _params : TGIS_ParamsList  ;
                           const _onhelp : TGIS_HelpEvent   ;
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
      /// <param name="_type">
      ///   type of expected render feature or TGIS_ShapeType.Unknown
      /// </param>
      /// <param name="_proc">
      ///   Action to be performed after closing modal form.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute  ( const _layer  : TGIS_LayerVector ;
                           const _type   : TGIS_ShapeType   ;
                           const _params : TGIS_ParamsList  ;
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
      /// <param name="_type">
      ///   type of expected render feature or TGIS_ShapeType.Unknown
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute   ( const _layer  : TGIS_LayerVector ;
                           const _type   : TGIS_ShapeType   ;
                           const _params : TGIS_ParamsList 
                         ) : TGIS_PvlModalResult ; overload;
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

//##############################################################################
implementation

const
  // Maximum number of unique values.
  UNIQUE_LIMIT_VECTOR = 256 ;

  // Maximum number of records searched for unique values.
  UNIQUE_LIMIT_VECTOR_MAX_RECORDS = 16384 ;

  procedure TGIS_PvlControlLegendVectorWiz.doDestroy ;
  begin
    FreeObject( lstUnique ) ;
    FreeObject( lstFields ) ;
    FreeObject( lstFieldsN ) ;

    inherited ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.DoInitForm ;
  begin
    Self.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RENDER ) ;
    Self.ClientHeight := 250 ;
    Self.ClientWidth := 390 ;
    Self.Name := 'TGIS_ControlLegendVectorWiz' ;

    lstUnique          := TGIS_VariantList.Create ;
    iUniqueLimit       := UNIQUE_LIMIT_VECTOR ;
    iUniqueSearchLimit := UNIQUE_LIMIT_VECTOR_MAX_RECORDS ;
  end ;

  procedure TGIS_PvlControlLegendVectorWiz.set_render_type ;
  begin
    case oType of
      TGIS_ShapeType.Point,
      TGIS_ShapeType.MultiPoint : rdbRenderMarker.Checked := True ;
      TGIS_ShapeType.Arc        : rdbRenderLine.Checked   := True ;
      TGIS_ShapeType.Polygon,
      TGIS_ShapeType.MultiPatch : rdbRenderArea.Checked   := True ;
      else                        begin
                                    assert( False, GIS_RS_ERR_UNTESTED ) ;
                                    rdbRenderMarker.Checked := True ;
                                  end ;
    end ;
  end ;

  procedure TGIS_PvlControlLegendVectorWiz.set_render_style ;
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
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_PvlControlLegendVectorWiz.set_fields ;
  begin
    if iMode = 0 then begin
      cmbStyleSizeStart.Fields.Assign( lstFields ) ;
      cmbStyleSizeEnd.Fields.Assign( lstFields ) ;
      cmbStyleColorStart.Fields.Assign( lstFields ) ;
      cmbStyleColorEnd.Fields.Assign( lstFields ) ;
    end else begin
      cmbStyleSizeStart.Fields.Assign( lstFieldsN ) ;
      cmbStyleSizeEnd.Fields.Assign( lstFieldsN ) ;
      cmbStyleColorStart.Fields.Assign( lstFieldsN ) ;
      cmbStyleColorEnd.Fields.Assign( lstFieldsN ) ;
    end;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.check_render ;
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

  procedure TGIS_PvlControlLegendVectorWiz.doClassifySimple ;
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

  function quoteString(
    _str : String ;
    _char: Char
  ) : String ;
  begin
    {$IFDEF DCC}
      Result := AnsiQuotedStr( _str, _char ) ;
    {$ELSE}
      Result := QuotedStr( _str, _char ) ;
    {$ENDIF}
  end;

  function StringDateToDouble( 
    _date : String 
  ) : Double ;
  begin
    {$IFDEF JAVA}
      Result := TDateTime.ToOADate(StrToDateTime( _date )) ;
    {$ELSE}
      {$IFDEF CLR}
        Result := StrToDateTime( _date ).ToOADate ;
      {$ELSE}
        Result := Double(StrToDateTime( _date )) ;
      {$ENDIF}
    {$ENDIF}
  end;
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
                [ cmbFormula.Text, quoteString( VarToString( lstUnique[i] ), '''') ]
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
                                   [ cmbFormula.Text, VarToString( lstUnique[i] ) ]
                                 ) ;
                  varDate     :
                    tmp_query := Format( '%s=%s',
                                   [ cmbFormula.Text, DecodeDateTimeToSqlMacro( lstUnique[i] ) ]
                                 ) ;
                  else
                    tmp_query := Format( '%s=%s',
                                   [ cmbFormula.Text, DotFloatToStr( VarToDouble( lstUnique[i] ) ) ]
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
                      assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
                      assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
                      assert( False, GIS_RS_ERR_UNTESTED  ) ;
                    end ;
                    sec.Area.ShowLegend := chkStyleShowLegend.Checked ;
            end
            else begin
              assert( False, GIS_RS_ERR_UNTESTED  ) ;
            end ;
            sec.Legend := VarToString( lstUnique[i] ) ;

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
                    assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
                    assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
                    assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Area.ShowLegend := (overUnique or overRecords) and
                                          chkStyleShowLegend.Checked ;
          end
          else begin
            assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
              sec.Render.MinVal   := StringDateToDouble( edtAnalyzeAvg.Text ) ;
              sec.Render.MaxVal   := StringDateToDouble( edtAnalyzeMax.Text ) ;
            end
            else begin
              sec.Render.MinVal   := DotStrToFloat( edtAnalyzeAvg.Text ) ;
              sec.Render.MaxVal   := DotStrToFloat( edtAnalyzeMax.Text ) ;
            end ;
            if chkAnalyzeLog.Checked then sec.Render.Zones := -5
                                     else sec.Render.Zones :=  5 ;

            if bDates then begin
              sec.Render.MinValEx := StringDateToDouble( edtAnalyzeMin.Text ) ;
              sec.Render.MaxValEx := StringDateToDouble( edtAnalyzeAvg.Text ) ;
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
              sec.Render.MinVal := StringDateToDouble( edtAnalyzeMin.Text ) ;
              sec.Render.MaxVal := StringDateToDouble( edtAnalyzeMax.Text ) ;
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
                    assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
                    assert( False, GIS_RS_ERR_UNTESTED  ) ;
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
                    assert( False, GIS_RS_ERR_UNTESTED  ) ;
                  end ;
                  sec.Area.ShowLegend := chkStyleShowLegend.Checked ;
          end
          else begin
            assert( False, GIS_RS_ERR_UNTESTED  ) ;
          end
        end ;
      end
      else begin

      end ;

      objParams.Assign( prm );
    finally
      FreeObject( prm ) ;
    end ;
  end ;

  procedure TGIS_PvlControlLegendVectorWiz.doOnAnalyzeEnterWork(
    shp : TGIS_Shape ;
    sql : TGIS_SqlQuery ;
    limit : Integer ;
    first_run : Boolean ;
    lst : TStringList
  ) ;

    function StringDate( const _val : Double ) : String ;
    begin
      {$IFDEF JAVA}
        var dt := VarToDateTime( TDateTime.FromOADate( _val ) ) ;
        if (dt.getHour=0) and (dt.getMinute=0) and (dt.getSecond=0) then
          Result := new java.text.SimpleDateFormat("dd.MM.yyyy").format( dt )
        else
          Result := new java.text.SimpleDateFormat("dd.MM.yyyy hh:mm:ss").format( dt ) ;
      {$ELSE}
        {$IFDEF CLR}
          var dt := VarToDateTime( TDateTime.FromOADate( _val ) ) ;
          if (dt.Hour=0) and (dt.Minute=0) and (dt.Second=0) then
            Result := dt.ToShortDateString
          else
            Result := dt.ToString ;
        {$ELSE}
          Result := DateTimeToStr( TDateTime( _val ) ) ;
        {$ENDIF}
      {$ENDIF}
    end;

    function add_unique( const _val : Variant ) : Integer ;
    var
      k    : Integer ;
      stmp : String  ;
    begin
      stmp := VarToString( _val ) ;
      if not lst.Find( stmp, k ) then begin
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

  var
    oval  : Variant ;
    i     : Integer ;

  begin
    while assigned( shp ) and ( not overRecords ) do
    begin
      dec( limit ) ;
      if limit <= 0 then
        overRecords := True ;

      oval := sql.Parse( shp, 0 ) ;

      if ( not VarIsNull( oval ) ) and ( not VarIsEmpty( oval ) ) then
      begin
        inc( valCnt ) ;
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
          if VarToDouble( oval ) < VarToDouble( valMin ) then valMin := oval ;
          if VarToDouble( oval ) > VarToDouble( valMax ) then valMax := oval ;
        end
        else begin
          if first_run then
            bStrings := False ;
          add_unique( oval ) ;
          oval := VarToDouble(oval) ;
          if VarToDouble( oval ) < VarToDouble( valMin ) then valMin := oval ;
          if VarToDouble( oval ) > VarToDouble( valMax ) then valMax := oval ;
        end ;

        first_run := False ;
      end ;

      shp := objLayer.FindNext ;
    end ;

    FreeObject( sql ) ;
    FreeObject( lst ) ;


    lstUnique.Sort ;

    if bDates then begin
      edtAnalyzeMin.Text := StringDate( VarToDouble( valMin ) ) ;
      edtAnalyzeAvg.Text := StringDate( ( VarToDouble( valMax ) + VarToDouble( valMin ) ) / 2 ) ;
      edtAnalyzeMax.Text := StringDate( VarToDouble( valMax ) ) ;
    end
    else begin
      edtAnalyzeMin.Text := FloatToStr( VarToDouble( valMin ) ) ;
      edtAnalyzeAvg.Text := FloatToStr( (VarToDouble( valMax ) + VarToDouble( valMin ) ) / 2  ) ;
      edtAnalyzeMax.Text := FloatToStr( VarToDouble( valMax ) ) ;
    end ;

    memAnalyzeUnique.BeginUpdate ;
    try
      memAnalyzeUnique.ItemsClear ;
      if overUnique then
        memAnalyzeUnique.ItemsAdd( Format( GIS_RS_LEGEND_WIZARD_VALLIMIT,
                                            [ iUniqueLimit ]
                                          )
                                  ) ;
      if overRecords then
        memAnalyzeUnique.ItemsAdd( Format( GIS_RS_LEGEND_WIZARD_RECLIMIT,
                                            [  iUniqueSearchLimit ]
                                          )
                                  )
      else
        memAnalyzeUnique.ItemsAdd( Format( GIS_RS_LEGEND_WIZARD_RECLIMIT,
                                            [  valCnt ]
                                          )
                                  ) ;

      for i := 0 to lstUnique.Count -1  do
        memAnalyzeUnique.ItemsAdd( VarToString( lstUnique[i] ) ) ;
    finally
      memAnalyzeUnique.EndUpdate ;
    end ;

    rdbAnalyzeUnique.Checked    := False ;
    rdbAnalyzeContinous.Checked := False ;
    rdbAnalyzeContinous.Enabled := not bStrings  ;

    if bStrings or ( not ( overUnique or overRecords ) ) then
      rdbAnalyzeUnique.Checked := True
    else
      rdbAnalyzeContinous.Checked := True ;

    rdbAnalyzeChange( Self ) ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.doClassifyWork(
    classifier  : TGIS_ClassificationVector;
    prm         : TGIS_ParamsList
  ) ;
  begin

    classifier.Classify( prm ) ;
    FreeObject( classifier ) ;

    objParams.Assign( prm );
    FreeObject( prm ) ;
  end;

  function TGIS_PvlControlLegendVectorWiz.doClassifyAdvanced : Boolean ;
  var
    i          : Integer ;
    ci         : Integer ;
    fname      : String ;
    &method    : String ;
    prm        : TGIS_ParamsList ;
    sec        : TGIS_ParamsSectionVector ;
    classifier : TGIS_ClassificationVector ;
    frm_stats  : TGIS_ControlStatistics ;
    proc       : TGIS_Proc ;
  begin
    Result := False ;

    prm := TGIS_ParamsList.Create ;

    prm.Assign( objParams ) ;

    for i := prm.Count - 1 downto 1 do begin
      prm.Selected := i ;
      prm.Delete ;
    end ;

    sec := TGIS_ParamsSectionVector( prm.Items[0] ) ;
    sec.Visible := True ;
    sec.Render.Expression := '' ;

    classifier := TGIS_ClassificationVector.Create( objLayer ) ;

    // set properties
    fname := cmbFormulaAdvanced.Item[ cmbFormulaAdvanced.ItemIndex ] ;
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

    // set color ramp
    if rdbStyleColorRamp.Checked then begin
      if ( &method = GIS_RS_CLASSIFY_METHOD_QR ) or
         ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
         ( &method = GIS_RS_CLASSIFY_METHOD_SDC ) or
         ( &method = GIS_RS_CLASSIFY_METHOD_DI ) then
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

  procedure TGIS_PvlControlLegendVectorWiz.onChooseExit(
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
      0 : pnlChoose.Next := pnlFormula ;
      1 : pnlChoose.Next := pnlFormulaAdvanced ;
      -1: pnlChoose.Next := nil ;
    end;

    pnlChoose.Previous := nil ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onFormulaEnter(
    _sender: TObject
  ) ;
  begin
    Self.BtnNext.Enabled := not IsStringEmpty( cmbFormula.Text ) ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onFormulaExit(
    _sender: TObject
  ) ;
  begin
    BtnNext.Enabled := True ;

    if _sender = Self.BtnNext then begin
      pnlFormula.Next := pnlAnalyze ;
    end;

    if _sender = Self.BtnPrevious then begin
      pnlFormula.Previous := pnlChoose ;
    end;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onRenderEnter(
    _sender: TObject
  ) ;
  begin
    check_render ;

    set_render_type ;

    rdbRenderColor.Checked := True ;

    if iMode = 0 then
      pnlRender.Previous := pnlAnalyze 
    else if iMode = 1 then
      pnlRender.Previous := pnlFormulaAdvanced ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onRenderExit(
    _sender: TObject
  ) ;
  begin
    if iMode = 0 then
      check_render
    else begin
      rdbStyleChange( Self ) ;
    end;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onAnalyzeEnter(
    _sender: TObject
  ) ;
  var
    lst       : TGIS_StringList         ;
    first_run : Boolean                 ;
    limit     : Integer                 ;
    ext       : TGIS_Extent             ;
    frm_stats : TGIS_ControlStatistics  ;
    shp       : TGIS_Shape              ;
    sql       : TGIS_SqlQuery           ;
    proc      : TGIS_Proc               ;
  begin
    valMin      :=  GIS_MAX_DOUBLE ;
    valMax      := -GIS_MAX_DOUBLE ;
    valCnt      := 0 ;
    overUnique  := False ;
    overRecords := False ;

    lst := TGIS_StringList.Create ;
    lst.Sorted := True ;
    lstUnique.Clear ;
    sql := TGIS_SqlQuery.Create ;
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

    if assigned( shp ) then begin
      sql.Parse( shp, 0 ) ;
      if objLayer.MustCalculateStatistics then begin
        frm_stats := TGIS_ControlStatistics.Create( Self ) ;
        proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
          procedure( _modal_result : TGIS_PvlModalResult )
          begin
            if _modal_result <> TGIS_PvlModalResult.OK then
              exit
            else
              doOnAnalyzeEnterWork( shp, sql, limit, first_run, lst ) ;
          end
        {$IFDEF OXYGENE}){$ENDIF};

        frm_stats.Execute( objLayer, OnHelpEvent, proc ) ;
      end else begin
        doOnAnalyzeEnterWork( shp, sql, limit, first_run, lst ) ;
      end ;
    end ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onAnalyzeExit(
    _sender : TObject 
  ) ;
  var
    optionDialog : TGIS_PvlOptionDialog ;
  begin
    cmbStyleRampList.Lock ;

    if iMode = 0 then begin
      // too many uniques warning
      if rdbAnalyzeUnique.Checked and overUnique then
        if _sender = BtnNext then begin
          optionDialog := TGIS_PvlOptionDialog.Create( Context ) ;
          optionDialog.Text := Format( _rsrc( GIS_RS_LEGEND_TOO_MANY_UNIQUE ), [iUniqueLimit] ) ;

          if optionDialog.Execute = TGIS_PvlModalResult.No
            then begin
              Pages.Abort;
              Pages.Activate( pnlAnalyze.PageNumber, Self ) ;
              exit ;
            end;
        end;

      if IsStringEmpty( edtAnalyzeMin.Text ) then
        exit ;
      if IsStringEmpty( edtAnalyzeMax.Text )  then
        exit ;
      if chkAnalyzeAvg.Checked and IsStringEmpty( edtAnalyzeAvg.Text ) then
        exit ;

      cmbStyleRampList.Reverse := False ;
      cmbStyleRampList.ShowAll := False ;

      if rdbAnalyzeUnique.Checked then begin
        cmbStyleRampList.Discrete := True ;
        cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
        // default is 'Unique'
        cmbStyleRampList.ItemIndex := cmbStyleRampList.ItemCount - 4 ;
      end
      else begin
        // will be used in future (color ramps in renderer)
        cmbStyleRampList.Discrete := False ;
        cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
      end ;

      set_render_type ;
    end
    else begin
      set_render_style ;
    end ;

    if rdbAnalyzeUnique.Checked then begin
      cmbStyleRampList.Discrete := True ;
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Qualitative] ;
      // default is 'Unique'
      cmbStyleRampList.Unlock;
      cmbStyleRampList.ItemIndex := cmbStyleRampList.ItemCount - 4 ;
    end
    else begin
      // will be used in future (color ramps in renderer)
      cmbStyleRampList.Discrete := False ;
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;
      cmbStyleRampList.Unlock;
      cmbStyleRampList.ItemIndex := 0 ;
    end ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onFormulaAdvancedExit(
    _sender: TObject
  ) ;
  var
    &method : String ;
  begin
    set_render_type ;

    cmbStyleRampList.Lock;
    // reset ramp params to default
    cmbStyleRampList.Reverse := False ;
    cmbStyleRampList.Discrete := False ;
    cmbStyleRampList.ShowAll := False ;

    &method := cmbMethodAdvanced.Item[cmbMethodAdvanced.ItemIndex] ;
    if ( &method = GIS_RS_CLASSIFY_METHOD_QR ) or
        ( &method = GIS_RS_CLASSIFY_METHOD_SD ) or
        ( &method = GIS_RS_CLASSIFY_METHOD_SDC )
    then
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Diverging]
    else
      cmbStyleRampList.ColorSchemas := [TGIS_ColorSchema.Sequential] ;

    cmbStyleRampList.Unlock ;
    cmbStyleRampList.ItemIndex := 0 ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.onMethodAdvancedChange(
    _sender: TObject
  ) ;
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

  procedure TGIS_PvlControlLegendVectorWiz.onParamStyleParamsEnter(
    _sender: TObject
  ) ;
  begin
    set_fields ;
    set_render_style ;
    rdbStyleChange( Self ) ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.rdbAnalyzeChange(
    _sender: TObject
  ) ;
  var
    bl : Boolean ;
  begin
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
  end;

  procedure TGIS_PvlControlLegendVectorWiz.rdbStyleChange(
    _sender: TObject
  ) ;
  begin
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
    end
  end;

  procedure TGIS_PvlControlLegendVectorWiz.cmbFormulaChange( _sender: TObject );
  begin
    if not ( _sender is TGIS_PvlComboEdit ) then
      exit ;

    BtnNext.Enabled := not IsStringEmpty( TGIS_PvlComboEdit( _sender ).Text )
  end;

  procedure TGIS_PvlControlLegendVectorWiz.createChoose ;
  begin
    pnlChoose := Self.Pages.AddPage ;

    grpChoose := 'grpChoose' ;

    rdbChooseSimple := TGIS_PvlRadioButton.Create( Context ) ;
    rdbChooseSimple.Group := grpChoose ;
    rdbChooseSimple.Place( 200, 0, nil, RoundS( Pages.Width / 2 - 100 ), nil, RoundS( Pages.Height / 2 - 2 * Context.HMargin - BtnHelp.Height ) ) ;
    rdbChooseSimple.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_CLASSIFY_SIMPLE ) ;
    rdbChooseSimple.Checked := True ;

    pnlChoose.AddComponent( rdbChooseSimple, 'Choose_RadioButton_Simple' ) ;

    rdbChooseAdvanced := TGIS_PvlRadioButton.Create( Context ) ;
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

  procedure TGIS_PvlControlLegendVectorWiz.createFormula ;
  begin
    pnlFormula := Self.Pages.AddPage ;

    grpFormula := 'grpFormula' ;

    lblFormula := TGIS_PvlLabel.Create( Context ) ;
    lblFormula.Place( pnlFormula.Width - 2 * Context.HMargin, 0, nil, Context.HMargin, nil, Context.VMargin ) ;
    lblFormula.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_FORMULA ) ;

    pnlFormula.AddComponent( lblFormula, 'Formula_Label_Formula' ) ;

    cmbFormula := TGIS_PvlComboEdit.Create( Context ) ;
    cmbFormula.FilteredSearch := True ;
    cmbFormula.Place( pnlFormula.Width - 2 * Context.HMargin, 0, nil, Context.HMargin, lblFormula, 0 ) ;

    {$IFDEF DCC}
      cmbFormula.OnChange := cmbFormulaChange ;
    {$ELSE}
      cmbFormula.OnChange := @cmbFormulaChange ;
    {$ENDIF}

    pnlFormula.AddComponent( cmbFormula, 'Formula_ComboEdit_Formula' ) ;

    rdbLimitedScan := TGIS_PvlRadioButton.Create( Context ) ;
    rdbLimitedScan.Group := grpFormula ;
    rdbLimitedScan.Place( pnlFormula.Width - 2 * Context.HMargin, 0, nil, Context.HMargin, cmbFormula, 0 ) ;
    rdbLimitedScan.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBLIMITSCAN ) ;
    rdbLimitedScan.Checked := True ;

    pnlFormula.AddComponent( rdbLimitedScan, 'Formula_RadioButton_LimitedScan' ) ;

    rdbFullScan := TGIS_PvlRadioButton.Create( Context ) ;
    rdbFullScan.Group := grpFormula ;
    rdbFullScan.Place( pnlFormula.Width - 2 * Context.HMargin, 0, nil, Context.HMargin, rdbLimitedScan, 0 ) ;
    rdbFullScan.Caption := _rsrc( GIS_RS_LEGEND_WIZARD_RDBFULLSCAN ) ;

    pnlFormula.AddComponent( rdbFullScan, 'Formula_RadioButton_FullScan' ) ;

    {$IFDEF DCC}
      pnlFormula.OnExit := onFormulaExit ;
    {$ELSE}
      pnlFormula.OnExit := @onFormulaExit ;
    {$ENDIF}

    {$IFDEF DCC}
      pnlFormula.OnEnter := onFormulaEnter ;
    {$ELSE}
      pnlFormula.OnEnter := @onFormulaEnter ;
    {$ENDIF}

  end;

  procedure TGIS_PvlControlLegendVectorWiz.createAdvancedFormula ;
  var
    i : Integer ;
  begin
    pnlFormulaAdvanced := Self.Pages.AddPage ;

    lblFormulaAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblFormulaAdvanced.Place( pnlFormulaAdvanced.Width - 2 * Context.HMargin, 0, nil, Context.HMargin, nil, Context.VMargin ) ;
    lblFormulaAdvanced.Caption := _rsrcna( GIS_RS_FIELDFACTOR_FIELD ) ;

    pnlFormulaAdvanced.AddComponent( lblFormulaAdvanced, 'FormulaAdvanced_Label_Formula' ) ;

    cmbFormulaAdvanced := TGIS_PvlComboBox.Create( Context ) ;
    cmbFormulaAdvanced.Place( lblFormulaAdvanced.Width, 0, nil, Context.HMargin, lblFormulaAdvanced, 0 ) ;

    pnlFormulaAdvanced.AddComponent( cmbFormulaAdvanced, 'FormulaAdvanced_ComboBox_Formula' ) ;

    lblMethodAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblMethodAdvanced.Place( RoundS( cmbFormula.Width / 2 - Context.HMargin ), 0, nil, Context.HMargin, cmbFormulaAdvanced, 0 ) ;
    lblMethodAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CLASSIFY_METHOD ) ;

    pnlFormulaAdvanced.AddComponent( lblMethodAdvanced, 'FormulaAdvanced_Label_Method' ) ;

    cmbMethodAdvanced := TGIS_PvlComboBox.Create( Context ) ;
    cmbMethodAdvanced.Place( lblMethodAdvanced.Width, 0, nil, Context.HMargin, lblMethodAdvanced, 0 ) ;

    {$IFDEF DCC}
      cmbMethodAdvanced.OnChange := onMethodAdvancedChange ;
    {$ELSE}
      cmbMethodAdvanced.OnChange := @onMethodAdvancedChange ;
    {$ENDIF}

    pnlFormulaAdvanced.AddComponent( cmbMethodAdvanced, 'FormulaAdvanced_ComboBox_Method' ) ;

    lblClassesAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblClassesAdvanced.Place( RoundS( lblMethodAdvanced.Width / 2 ), 0, lblMethodAdvanced, Context.HMargin, cmbFormulaAdvanced, 0 ) ;
    lblClassesAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CLASSIFY_CLASSES ) ;

    pnlFormulaAdvanced.AddComponent( lblClassesAdvanced, 'FormulaAdvanced_Label_Classes' ) ;

    cmbClassesAdvanced := TGIS_PvlComboBox.Create( Context ) ;
    cmbClassesAdvanced.Place( lblClassesAdvanced.Width, 0, cmbMethodAdvanced, Context.HMargin, lblClassesAdvanced, 0 ) ;

    pnlFormulaAdvanced.AddComponent( cmbClassesAdvanced, 'FormulaAdvanced_ComboBox_Classes' ) ;

    lblIntervalAdvanced := TGIS_PvlLabel.Create( Context ) ;
    lblIntervalAdvanced.Place( lblClassesAdvanced.Width, 0, lblClassesAdvanced, Context.HMargin, cmbFormulaAdvanced, 0 ) ;
    lblIntervalAdvanced.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CLASSIFY_INTERVAL ) ;

    pnlFormulaAdvanced.AddComponent( lblIntervalAdvanced, 'FormulaAdvanced_Label_Interval' ) ;

    cmbIntervalAdvanced := TGIS_PvlComboEdit.Create( Context ) ;
    cmbIntervalAdvanced.Place( lblIntervalAdvanced.Width, 0, cmbClassesAdvanced, Context.HMargin, lblIntervalAdvanced, 0 ) ;
    cmbIntervalAdvanced.Text := '100' ;

    pnlFormulaAdvanced.AddComponent( cmbIntervalAdvanced, 'FormulaAdvanced_ComboBox_Interval' ) ;

    for i := 1 to 9 do
      cmbClassesAdvanced.ItemsAdd( IntToStr( i ) ) ;
    cmbClassesAdvanced.ItemIndex := 4 ;

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
      pnlFormulaAdvanced.OnExit := onFormulaAdvancedExit ;
    {$ELSE}
      pnlFormulaAdvanced.OnExit := @onFormulaAdvancedExit ;
    {$ENDIF}
  end;

  procedure TGIS_PvlControlLegendVectorWiz.createAnalyze ;
  begin
    pnlAnalyze := Self.Pages.AddPage ;

    grpAnalyze := 'grpAnalyze' ;

    // Left side

    rdbAnalyzeUnique := TGIS_PvlRadioButton.Create( Context ) ;
    rdbAnalyzeUnique.Group := grpAnalyze ;
    rdbAnalyzeUnique.Place( RoundS( ( pnlAnalyze.Width -  Context.HMargin ) / 2 - Context.HSpace ), 0, nil, Context.HMargin, nil, Context.VMargin ) ;
    rdbAnalyzeUnique.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBUNIQUE ) ;
    rdbAnalyzeUnique.Checked := True ;

    {$IFDEF DCC}
      rdbAnalyzeUnique.OnClick := rdbAnalyzeChange ;
    {$ELSE}
      rdbAnalyzeUnique.OnClick := @rdbAnalyzeChange ;
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
      rdbAnalyzeContinous.OnClick := rdbAnalyzeChange ;
    {$ELSE}
      rdbAnalyzeContinous.OnClick := @rdbAnalyzeChange ;
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

    {$IFDEF DCC}
      chkAnalyzeAvg.OnClick := rdbAnalyzeChange ;
    {$ELSE}
      chkAnalyzeAvg.OnClick := @rdbAnalyzeChange ;
    {$ENDIF}

    pnlAnalyze.AddComponent( chkAnalyzeAvg, 'Analyze_CheckBox_Average' ) ;

    edtAnalyzeAvg := TGIS_PvlEdit.Create( Context ) ;
    edtAnalyzeAvg.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, chkAnalyzeAvg, 0 ) ;

    pnlAnalyze.AddComponent( edtAnalyzeAvg, 'Analyze_Edit_Average' ) ;

    chkAnalyzeLog := TGIS_PvlCheckBox.Create( Context ) ;
    chkAnalyzeLog.Place( rdbAnalyzeContinous.Width, 0, rdbAnalyzeUnique, Context.HSpace, edtAnalyzeAvg, 0 ) ;
    chkAnalyzeLog.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_CHKLOG ) ;

    pnlAnalyze.AddComponent( chkAnalyzeLog, 'Analyze_CheckBox_Logarytmic' ) ;

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

  procedure TGIS_PvlControlLegendVectorWiz.createFeatures ;
  begin
    pnlRender := Self.Pages.AddPage ;

    grpRenderValue := 'grpRenderValue' ;
    grpRenderFeature := 'grpRenderFeature' ;

    // Left side

    grpbxRenderValue := TGIS_PvlGroupBox.Create( Context ) ;
    grpbxRenderValue.Place( RoundS( ( pnlRender.Width -  Context.HMargin ) / 2 - Context.HSpace ), 110, nil, Context.HMargin, nil, Context.VMargin ) ;
    grpbxRenderValue.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_GRPVALUE ) ;

    pnlRender.AddComponent( grpbxRenderValue, 'Render_GroupBox_Value' ) ;

    rdbRenderMarker := TGIS_PvlRadioButton.Create( grpbxRenderValue.Context ) ;
    rdbRenderMarker.Group := grpRenderValue ;
    rdbRenderMarker.Place( grpbxRenderValue.Width - 2 * Context.HMargin, 0, nil, grpbxRenderValue.Context.HMargin, nil, grpbxRenderValue.Context.VMargin ) ;
    rdbRenderMarker.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBMARKER ) ;

    rdbRenderLine := TGIS_PvlRadioButton.Create( grpbxRenderValue.Context ) ;
    rdbRenderLine.Group := grpRenderValue ;
    rdbRenderLine.Place( rdbRenderMarker.Width, 0, nil, grpbxRenderValue.Context.HMargin, rdbRenderMarker, 0 ) ;
    rdbRenderLine.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBLINE ) ;

    rdbRenderArea := TGIS_PvlRadioButton.Create( grpbxRenderValue.Context ) ;
    rdbRenderArea.Group := grpRenderValue ;
    rdbRenderArea.Place( rdbRenderMarker.Width, 0, nil, grpbxRenderValue.Context.HMargin, rdbRenderLine, 0 ) ;
    rdbRenderArea.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RDBAREA ) ;

    chkStyleShowLegend := TGIS_PvlCheckBox.Create( Context ) ;
    chkStyleShowLegend.Caption := _rsrcna( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND );
    chkStyleShowLegend.Checked := true ;

    pnlRender.AddComponent( chkStyleShowLegend, 'Render_CheckBox_ShowInLegend' ) ;

    // Right side

    grpbxRenderFeature := TGIS_PvlGroupBox.Create( Context ) ;
    grpbxRenderFeature.Place( grpbxRenderValue.Width, 110, grpbxRenderValue, Context.HSpace, nil, Context.VMargin ) ;
    grpbxRenderFeature.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_GRPSHAPE ) ;

    pnlRender.AddComponent( grpbxRenderFeature, 'Render_GroupBox_Feature' ) ;

    rdbRenderSize := TGIS_PvlRadioButton.Create( grpbxRenderFeature.Context ) ;
    rdbRenderSize.Group := grpRenderFeature ;
    rdbRenderSize.Place( grpbxRenderFeature.Width - 2 * grpbxRenderValue.Context.HMargin, 0, nil, grpbxRenderValue.Context.HMargin, nil, grpbxRenderValue.Context.VMargin ) ;
    rdbRenderSize.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_RBDSIZE ) ;

    rdbRenderColor := TGIS_PvlRadioButton.Create( grpbxRenderFeature.Context ) ;
    rdbRenderColor.Group := grpRenderFeature ;
    rdbRenderColor.Place( rdbRenderSize.Width, 0, nil, grpbxRenderValue.Context.HMargin, rdbRenderSize, 0 ) ;
    rdbRenderColor.Caption := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;

    rdbRenderOutlineWidth := TGIS_PvlRadioButton.Create( grpbxRenderFeature.Context ) ;
    rdbRenderOutlineWidth.Group := grpRenderFeature ;
    rdbRenderOutlineWidth.Place( rdbRenderSize.Width, 0, nil, grpbxRenderValue.Context.HMargin, rdbRenderColor, 0 ) ;
    rdbRenderOutlineWidth.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_OUTLINEWIDTH ) ;

    rdbRenderOutlineColor := TGIS_PvlRadioButton.Create( grpbxRenderFeature.Context ) ;
    rdbRenderOutlineColor.Group := grpRenderFeature ;
    rdbRenderOutlineColor.Place( rdbRenderSize.Width, 0, nil, grpbxRenderValue.Context.HMargin, rdbRenderOutlineWidth, 0 ) ;
    rdbRenderOutlineColor.Caption := _rsrcna( GIS_RS_LEGEND_WIZARD_OUTLINECOLOR ) ;

    grpbxRenderFeature.Height := rdbRenderOutlineColor.Top + rdbRenderOutlineColor.Height + Context.HMargin ;
    grpbxRenderValue.Height := grpbxRenderFeature.Height ;

    chkStyleShowLegend.Place( grpbxRenderValue.Width, 0, nil, grpbxRenderValue.Context.HMargin, grpbxRenderValue, 0 ) ;

    {$IFDEF DCC}
      pnlRender.OnEnter := onRenderEnter ;
    {$ELSE}
      pnlRender.OnEnter := @onRenderEnter ;
    {$ENDIF}

    {$IFDEF DCC}
      pnlRender.OnExit := onRenderExit ;
    {$ELSE}
      pnlRender.OnExit := @onRenderExit ;
    {$ENDIF}
  end;

  procedure TGIS_PvlControlLegendVectorWiz.createColors ;
  var
    FIRST_COLUMN_WIDTH  : Integer ;
    SECOND_COLUMN_WIDTH : Integer ;
    THIRD_COLUMN_WIDTH  : Integer ;
  begin
    FIRST_COLUMN_WIDTH  := 80 ;
    SECOND_COLUMN_WIDTH := RoundS( ( Self.ClientWidth - FIRST_COLUMN_WIDTH ) / 2 ) - ( 2 * Context.HMargin ) - ( 2 * Context.HSpace ) ;
    THIRD_COLUMN_WIDTH  := SECOND_COLUMN_WIDTH ;

    pnlStyleParams := Self.Pages.AddPage ;

    grpStyle := 'grpStyle' ;

    lblStyleSizeStart := TGIS_PvlLabel.Create( Context ) ;
    lblStyleSizeStart.Place( SECOND_COLUMN_WIDTH, 0, nil, FIRST_COLUMN_WIDTH + Context.HMargin + Context.HSpace, nil, Context.VMargin ) ;
    lblStyleSizeStart.Caption := _rsrcna( GIS_RS_LEGEND_PRM_STARTSIZE ) ;
    lblStyleSizeStart.Alignment := TGIS_PvlLabelTextAlignment.Left ;

    pnlStyleParams.AddComponent( lblStyleSizeStart, 'StyleParams_Label_SizeStart' ) ;

    lblStyleSizeEnd := TGIS_PvlLabel.Create( Context ) ;
    lblStyleSizeEnd.Place( THIRD_COLUMN_WIDTH, 0, lblStyleSizeStart, Context.HSpace, nil, Context.VMargin ) ;
    lblStyleSizeEnd.Caption := _rsrcna( GIS_RS_LEGEND_PRM_ENDSIZE ) ;
    lblStyleSizeEnd.Alignment := TGIS_PvlLabelTextAlignment.Left ;

    pnlStyleParams.AddComponent( lblStyleSizeEnd, 'StyleParams_Label_SizeEnd' ) ;

    rdbStyleSize := TGIS_PvlRadioButton.Create( Context ) ;
    rdbStyleSize.Group := grpStyle ;
    rdbStyleSize.Place( FIRST_COLUMN_WIDTH, 0, nil, Context.HMargin, lblStyleSizeStart, 0 ) ;
    rdbStyleSize.Caption := _rsrcna( GIS_RS_LEGEND_PRM_SIZE ) ;

    {$IFDEF DCC}
      rdbStyleSize.OnClick := rdbStyleChange ;
    {$ELSE}
      rdbStyleSize.OnClick := @rdbStyleChange ;
    {$ENDIF}

    pnlStyleParams.AddComponent( rdbStyleSize, 'StyleParams_RadioButton_Size' ) ;

    cmbStyleSizeStart := TGIS_PvlSizeComboBox.Create( Context ) ;
    cmbStyleSizeStart.Place( SECOND_COLUMN_WIDTH, 0, rdbStyleSize, Context.HSpace, lblStyleSizeStart, 0 ) ;
    cmbStyleSizeStart.Fill( True, False, True, True ) ;

    pnlStyleParams.AddComponent( cmbStyleSizeStart, 'StyleParams_ComboBox_SizeStart' ) ;

    cmbStyleSizeEnd := TGIS_PvlSizeComboBox.Create( Context ) ;
    cmbStyleSizeEnd.Place( THIRD_COLUMN_WIDTH, 0, cmbStyleSizeStart, Context.HSpace, lblStyleSizeEnd, 0 ) ;
    cmbStyleSizeEnd.Fill( True, False, True, True ) ;

    pnlStyleParams.AddComponent( cmbStyleSizeEnd, 'StyleParams_ComboBox_SizeEnd' ) ;

    lblStyleColorStart := TGIS_PvlLabel.Create( Context ) ;
    lblStyleColorStart.Place( SECOND_COLUMN_WIDTH, 0, nil, FIRST_COLUMN_WIDTH + Context.HMargin + Context.HSpace, cmbStyleSizeStart,2 * Context.VSpace ) ;
    lblStyleColorStart.Caption := _rsrcna( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
    lblStyleColorStart.Alignment := TGIS_PvlLabelTextAlignment.Left ;

    pnlStyleParams.AddComponent( lblStyleColorStart, 'StyleParams_Label_ColorStart' ) ;

    lblStyleColorEnd := TGIS_PvlLabel.Create( Context ) ;
    lblStyleColorEnd.Place( THIRD_COLUMN_WIDTH, 0, lblStyleColorStart, Context.HSpace, cmbStyleSizeStart, 2 * Context.VSpace ) ;
    lblStyleColorEnd.Caption := _rsrcna( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
    lblStyleColorEnd.Alignment := TGIS_PvlLabelTextAlignment.Left ;

    pnlStyleParams.AddComponent( lblStyleColorEnd, 'StyleParams_Label_ColorEnd' ) ;

    rdbStyleColorRange := TGIS_PvlRadioButton.Create( Context ) ;
    rdbStyleColorRange.Group := grpStyle ;
    rdbStyleColorRange.Place( FIRST_COLUMN_WIDTH, 0, nil, Context.HMargin, lblStyleColorStart, 0 ) ;
    rdbStyleColorRange.Caption := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;

    {$IFDEF DCC}
      rdbStyleColorRange.OnClick := rdbStyleChange ;
    {$ELSE}
      rdbStyleColorRange.OnClick := @rdbStyleChange ;
    {$ENDIF}

    pnlStyleParams.AddComponent( rdbStyleColorRange, 'StyleParams_RadioButton_Color' ) ;

    cmbStyleColorStart := TGIS_PvlColorComboBox.Create( Context ) ;
    cmbStyleColorStart.Place( SECOND_COLUMN_WIDTH, 24, rdbStyleColorRange, Context.HSpace, lblStyleColorStart, 0 ) ;
    cmbStyleColorStart.Fill( true, true ) ;

    pnlStyleParams.AddComponent( cmbStyleColorStart, 'StyleParams_ComboBox_ColorStart' ) ;

    cmbStyleColorEnd := TGIS_PvlColorComboBox.Create( Context ) ;
    cmbStyleColorEnd.Place( THIRD_COLUMN_WIDTH, 24, cmbStyleColorStart, Context.HSpace, lblStyleColorStart, 0 ) ;
    cmbStyleColorEnd.Fill( true, true ) ;

    pnlStyleParams.AddComponent( cmbStyleColorEnd, 'StyleParams_ComboBox_ColorEnd' ) ;

    rdbStyleColorRamp := TGIS_PvlRadioButton.Create( Context ) ;
    rdbStyleColorRamp.Group := grpStyle ;
    rdbStyleColorRamp.Place( FIRST_COLUMN_WIDTH, 0, nil, Context.HMargin, cmbStyleColorStart, 2 * Context.VSpace ) ;
    rdbStyleColorRamp.Caption := _rsrcna( GIS_RS_LEGEND_PAG_RAMP ) ;

    {$IFDEF DCC}
      rdbStyleColorRamp.OnClick := rdbStyleChange ;
    {$ELSE}
      rdbStyleColorRamp.OnClick := @rdbStyleChange ;
    {$ENDIF}

    pnlStyleParams.AddComponent( rdbStyleColorRamp, 'StyleParams_RadioButton_Ramp' ) ;

    cmbStyleRampList := TGIS_PvlColorRampWidget.Create( Context ) ;
    cmbStyleRampList.TwoColumns := True ;
    cmbStyleRampList.ComboHeight := 24 ;
    cmbStyleRampList.Place( SECOND_COLUMN_WIDTH + Context.HSpace + THIRD_COLUMN_WIDTH, 0, rdbStyleColorRamp, Context.HSpace, cmbStyleColorStart, 2 * Context.VSpace ) ;
    cmbStyleRampList.Fill ;

    pnlStyleParams.AddComponent( cmbStyleRampList, 'StyleParams_RampWidget_Ramp' ) ;

    {$IFDEF DCC}
      pnlStyleParams.OnEnter := onParamStyleParamsEnter ;
    {$ELSE}
      pnlStyleParams.OnEnter := @onParamStyleParamsEnter ;
    {$ENDIF}

  end;

  procedure TGIS_PvlControlLegendVectorWiz.createOrder ;
  begin
    pnlChoose.Next := pnlFormula ;
    pnlFormula.Next := pnlAnalyze ;
    pnlAnalyze.Next := pnlRender ;
    pnlFormulaAdvanced.Next := pnlRender ;
    pnlRender.Next := pnlStyleParams ;
     

    pnlFormula.Previous := pnlChoose ;
    pnlFormulaAdvanced.Previous := pnlChoose ;
    pnlAnalyze.Previous := pnlFormula ;
    pnlRender.Previous := pnlAnalyze ;
    pnlStyleParams.Previous := pnlRender ;
  end;

  procedure TGIS_PvlControlLegendVectorWiz.DoInitControls ;
  begin
    // Create pages
    createChoose ;
    createFormula ;
    createAdvancedFormula ;
    createAnalyze ;
    createFeatures ;
    createColors ;

    // Give pages order
    createOrder ;

    BtnOK.Visible := False ;
  end ;

  procedure TGIS_PvlControlLegendVectorWiz.btnOKClick(
      _sender: TObject
  ) ;
  begin
     if iMode = 0 then
      doClassifySimple
    else
      if not doClassifyAdvanced then
        exit ;

    inherited ;
  end;

  function TGIS_PvlControlLegendVectorWiz.Execute(
    const _layer  : TGIS_LayerVector ;
    const _type   : TGIS_ShapeType   ;
    const _params : TGIS_ParamsList  ;
    const _onhelp : TGIS_HelpEvent   ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    i           : Integer     ;
    wasjoin     : Boolean     ;
    {$IFNDEF GIS_NODB}
    field_type  : TFieldType ;
    field_size  : Integer ;
    {$ENDIF}
  begin
    Pages.Activate( 0, Self ) ;

    OnHelpEvent := _onhelp ;
    BtnHelp.Visible := assigned( OnHelpEvent ) ;

    assert( assigned( _layer ) ) ;
    objLayer := _layer ;

    if _type = TGIS_ShapeType.Unknown then oType := objLayer.DefaultShapeType
                                      else oType := _type ;

    assert( assigned( _params ) ) ;
    objParams := _params ;


    lstFields  := TStringList.Create ;
    lstFieldsN := TStringList.Create ;

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
        if ( not wasjoin ) and assigned( JoinDB ) then begin
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
        if ( not wasjoin ) and assigned( JoinADO ) then begin
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

    for i := 0 to lstFields.Count - 1 do
      cmbFormula.ItemsAdd( lstFields.Strings[ i ] ) ;

    cmbFormula.Text := '' ;

    for i := 0 to lstFieldsN.Count - 1 do
      cmbFormulaAdvanced.ItemsAdd( lstFieldsN.Strings[ i ] ) ;

    if cmbFormulaAdvanced.ItemsCount > 0 then
      cmbFormulaAdvanced.ItemIndex := Min( 0, cmbFormulaAdvanced.ItemsCount - 1 ) ;

    Result := ShowModal( _proc, assigned( _proc ) ) ;
  end ;

  function TGIS_PvlControlLegendVectorWiz.Execute(
    const _layer  : TGIS_LayerVector ;
    const _type   : TGIS_ShapeType   ;
    const _params : TGIS_ParamsList  ;
    const _proc   : TGIS_Proc
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, _type, _params, hlp, _proc ) ;
  end ;

  function TGIS_PvlControlLegendVectorWiz.Execute(
    const _layer  : TGIS_LayerVector ;
    const _type   : TGIS_ShapeType   ;
    const _params : TGIS_ParamsList 
  ) : TGIS_PvlModalResult ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _layer, _type, _params, hlp, nil ) ;
  end ;

//==================================== END =====================================
end.
