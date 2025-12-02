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
  Encapsulation of S57 layer.
}

{$IFDEF DCC}
  unit GisLayerS57 ;
  {$HPPEMIT '#pragma link "GisLayerS57"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Variants,
    GisTypes,
    GisTypesUI,    
    GisLayerVector,
    GisLayerCompound ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerS57 = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of S57 layer.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    It doesn't support versioning, reads only .000 file.
  ///    </note>
  /// </remarks>
  TGIS_LayerS57 = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerCompoundVector )
    private

      /// <summary>
      ///   File handle.
      /// </summary>
      FileS57     : TObject ;

      /// <summary>
      ///   Current shape.
      /// </summary>
      oShape      : TGIS_Shape ;

      /// <summary>
      ///   Current layer.
      /// </summary>
      oLayer      : TGIS_LayerVector ;

      /// <summary>
      ///   Visual flags.
      /// </summary>
      FUseS57Draw : Boolean ;

      /// <summary>
      ///   Layer filter.
      /// </summary>
      FLayerName  : String ;

    private

      /// <summary>
      ///   Prepare sublayer.
      /// </summary>
      /// <param name="_name">
      ///   sublayer name
      /// </param>
      procedure prepareSubLayer  ( const _name     : String ;
                                   const _caption  : String ;
                                   const _gtype    : Integer
                                 ) ;

      /// <summary>
      ///   Apply style.
      /// </summary>
      /// <param name="_layerName">
      ///   sublayer name
      /// </param>
      /// <param name="_lv">
      ///   sublayer handle
      /// </param>
      procedure addStyle1        ( const _layerName : String ;
                                   const _lv        : TGIS_LayerVector
                                 ) ;

      /// <summary>
      ///   Apply style.
      /// </summary>
      /// <param name="_layerName">
      ///   sublayer name
      /// </param>
      /// <param name="_lv">
      ///   sublayer handle
      /// </param>
      procedure addStyle2        ( const _layerName : String ;
                                   const _lv        : TGIS_LayerVector
                                 ) ;

      /// <summary>
      ///   Apply style.
      /// </summary>
      /// <param name="_layerName">
      ///   sublayer name
      /// </param>
      /// <param name="_lv">
      ///   sublayer handle
      /// </param>
      procedure addStyle3        ( const _layerName : String ;
                                   const _lv        : TGIS_LayerVector
                                 ) ;

      /// <summary>
      ///   End shape building.
      /// </summary>
      procedure OnEndShapeEvent  ;

      /// <summary>
      ///   Add part to shape.
      /// </summary>
      procedure OnAddPartEvent   ;

      /// <summary>
      ///   Begin shape building.
      /// </summary>
      /// <param name="_shpType">
      ///   shape type to create
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      procedure OnBeginShapeEvent( const _shpType  : TGIS_ShapeType ;
                                   const _dim      : TGIS_DimensionType
                                 ) ;

      /// <summary>
      ///   Add coordinates to shape.
      /// </summary>
      /// <param name="_ptg">
      ///   point to add
      /// </param>
      procedure OnAddCoordEvent  ( const _ptg      : TGIS_Point3D
                                 ) ;

      /// <summary>
      ///   Add and set attributes to shape.
      /// </summary>
      procedure OnAddAttrEvent   ( const _name     : String ;
                                   const _val      : String ;
                                   const _tp       : Char
                                 ) ;

      /// <summary>
      ///   Set shape sublayer.
      /// </summary>
      /// <param name="_name">
      ///   sublayer name
      /// </param>
      procedure OnSetLayerEvent  ( const _name     : String ;
                                   const _caption  : String ;
                                   const _gtype    : Integer
                                 ) ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      procedure setUp           ; override;
    protected
      // destructor

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;
    public

      /// <summary>
      ///   Use S52 style rendering.
      /// </summary>      
      property UseS57Draw  : Boolean read FUseS57Draw write FUseS57Draw ;

      /// <summary>
      ///   S57 format layer name to load. Only shapes that belong to this
      ///   layer will be created.
      /// </summary>
      property LayerName   : String  read FLayerName  write FLayerName  ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    GisRtl,
    GisInternals,
    GisClasses,
    GisSymbol,
    GisFileS57,
    GisResource,
    GisParams,
    GisRegistredLayers,
    GisGeometryFactory ;
{$ENDIF}

  {$DEFINE GIS_S57_SYMBOLOGY}

//==============================================================================
// TGIS_LayerS57
//==============================================================================

  constructor TGIS_LayerS57.Create;
  begin
    inherited;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                           ] ;
    FUseS57Draw := True ;
  end ;

  procedure TGIS_LayerS57.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerS57.OnAddAttrEvent(
    const _name : String ;
    const _val  : String ;
    const _tp   : Char
  ) ;
  begin
    if assigned( oLayer ) and ( oLayer.FindField( _name ) < 0 ) then
      oLayer.AddFieldInternal( _name, TGIS_FieldType.String, 1 , 0 ) ;

    if assigned( oShape ) and not IsStringEmpty( _val )  then
      oShape.SetField( _name, _val ) ;
  end ;

  procedure TGIS_LayerS57.OnAddCoordEvent(
    const _ptg : TGIS_Point3D
  ) ;
  begin
    oShape.AddPoint3D( _ptg );
  end ;

  procedure TGIS_LayerS57.OnAddPartEvent ;
  begin
    oShape.AddPart;
  end ;

  procedure TGIS_LayerS57.OnBeginShapeEvent(
    const _shpType : TGIS_ShapeType ;
    const _dim     : TGIS_DimensionType
  ) ;
  begin
    oShape := oLayer.CreateShape( _shpType ) ;
    oShape.Lock( TGIS_Lock.Projection );
    oShape.AddPart ;
  end ;

  procedure TGIS_LayerS57.OnEndShapeEvent ;
  var
    eList : TGIS_ObjectList ;
    arc   : TGIS_ShapeArc ;
    poly  : TGIS_Shape ;
    i, j  : Integer ;
  begin
    oShape.Unlock ;

    if oShape.ShapeType = TGIS_ShapeType.Polygon then begin
      eList := TGIS_ObjectList.Create( True ) ;
      try
        for i := 0 to oShape.GetNumParts - 1 do begin

          arc := TGIS_ShapeArc.Create ;
          arc.AddPart ;

          for j := 0 to oShape.GetPartSize( i ) - 2 do
            arc.AddPoint3D( oShape.GetPoint3D( i, j ) ) ;

          eList.Add( arc ) ;
        end ;

        poly := TGIS_GeometryFactory.GisBuildShapeFromEdges(
                  eList, TGIS_ShapeType.Polygon, 0, nil, nil, False, -1, nil, False
                ) ;
        try
          if assigned( poly ) then
            oShape.CopyGeometry( poly ) ;
        finally
          FreeObject( poly ) ;
        end ;
      finally
        FreeObject( eList ) ;
      end ;

    end ;
  end ;

  procedure TGIS_LayerS57.OnSetLayerEvent(
    const _name    : String ;
    const _caption : String ;
    const _gtype   : Integer
  ) ;
  begin
    prepareSubLayer( _name, _caption, _gtype );
  end ;

  procedure TGIS_LayerS57.addStyle1(
    const _layerName : String ;
    const _lv        : TGIS_LayerVector
   ) ;
  var
    lv  : TGIS_LayerVector ;
    c   : TGIS_Color ;
    def : TGIS_LabelPositions ;
  begin
    lv := _lv ;
    if _layerName='M_SDAT' then begin
      lv.Active := False ;
      lv.Caption := 'Sounding datum' ;
      lv.Name := 'M_SDAT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DEPARE' then begin
      lv.Active := True ;
      lv.Caption := 'Depth area' ;
      lv.Name := 'DEPARE' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '186:189:164:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1 LIKE ''-%'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '84:173:216:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=0)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=2)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '147:203:230:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=5)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=10)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '185:220:238:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=20)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '204:228:242:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=30)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '221:237:247:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=40)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '235:244:250:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=50)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=1)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=3)' ;
      lv.Params.Visible := True ;
      
      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=4)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := '(DRVAL1=17)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '186:189:164:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1 LIKE ''-%'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '84:173:216:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=0)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=2)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '147:203:230:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=5)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=10)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '185:220:238:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=20)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '204:228:242:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=30)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '221:237:247:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=40)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '235:244:250:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=50)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=1)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=3)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=4)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := '(DRVAL1=17)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '186:189:164:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1 LIKE ''-%'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '84:173:216:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=0)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=2)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '147:203:230:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=5)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=10)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '185:220:238:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=20)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '204:228:242:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=30)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '221:237:247:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=40)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '235:244:250:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=50)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=1)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=3)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=4)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=17)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '186:189:164:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1 LIKE ''-%'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '84:173:216:255', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=0)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '129:195:226:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=2)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '147:203:230:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=5)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=10)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '185:220:238:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=20)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '204:228:242:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=30)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '221:237:247:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=40)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '235:244:250:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=50)' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '166:211:234:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := '(DRVAL1=17)' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LNDARE' then begin
      lv.Active := True ;
      lv.Caption := 'Land area' ;
      lv.Name := 'LNDARE' ;
      lv.Params.Area.Color := ParamColor( '201:185:122:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '201:185:122:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DRGARE' then begin
      lv.Active := True ;
      lv.Caption := 'Dredged area' ;
      lv.Name := 'DRGARE' ;
      lv.Params.Area.Color := ParamColor( '128:128:64:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '128:128:64:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:RASTER01EXT') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      lv.Params.Marker.Size := -17 ;
      lv.Params.Area.SymbolGap := -15 ;
      lv.Params.Area.SymbolSize := -5 ;
      {$ENDIF}
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( '243:243:243:255', c ) ;
      lv.Params.Labels.Field := 'DRVAL1' ;
      lv.Params.Labels.Font.Color := ParamColor( 'OLIVE', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := '{DRVAL1}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_COVR' then begin
      lv.Active := False ;
      lv.Caption := 'Coverage' ;
      lv.Name := 'M_COVR' ;
      lv.Params.Area.Color := ParamColor( '211:211:211:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '211:211:211:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LAKARE' then begin
      lv.Active := True ;
      lv.Caption := 'Lake' ;
      lv.Name := 'LAKARE' ;
      lv.Params.Area.Color := ParamColor( '115:182:239:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RIVERS' then begin
      lv.Active := True ;
      lv.Caption := 'River' ;
      lv.Name := 'RIVERS' ;
      lv.Params.Area.Color := ParamColor( '115:182:239:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '115:182:239:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CANALS' then begin
      lv.Active := True ;
      lv.Caption := 'Canal' ;
      lv.Name := 'CANALS' ;
      lv.Params.Area.Color := ParamColor( '115:182:239:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '80:80:80:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Solid ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '115:182:239:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Line.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ADMARE' then begin
      lv.Active := False ;
      lv.Caption := 'Administration area (Named)' ;
      lv.Name := 'ADMARE' ;
      lv.Params.Area.Color := ParamColor( '163:180:183:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TS_PAD' then begin
      lv.Active := True ;
      lv.Caption := 'Tidal stream panel data' ;
      lv.Name := 'TS_PAD' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Color := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BUAARE' then begin
      lv.Active := True ;
      lv.Caption := 'Built-up area' ;
      lv.Name := 'BUAARE' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SEAARE' then begin
      lv.Active := True ;
      lv.Caption := 'Sea area / named water area' ;
      lv.Name := 'SEAARE' ;
      lv.Params.Area.Color := ParamColor( '235:244:250:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SBDARE' then begin
      lv.Active := True ;
      lv.Caption := 'Seabed area' ;
      lv.Name := 'SBDARE' ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -20 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Color := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -1 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=1' ;
      lv.Params.Labels.Value := 'mud' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=2' ;
      lv.Params.Labels.Value := 'clay' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=3' ;
      lv.Params.Labels.Value := 'silt' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=4' ;
      lv.Params.Labels.Value := 'sand' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=5' ;
      lv.Params.Labels.Value := 'stone' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=6' ;
      lv.Params.Labels.Value := 'gravel' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=7' ;
      lv.Params.Labels.Value := 'pebbles' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=8' ;
      lv.Params.Labels.Value := 'cobbles' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=9' ;
      lv.Params.Labels.Value := 'rock' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=11' ;
      lv.Params.Labels.Value := 'lava' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=14' ;
      lv.Params.Labels.Value := 'coral' ;
      
      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=17' ;
      lv.Params.Labels.Value := 'shells' ;

      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR=18' ;
      lv.Params.Labels.Value := 'boulder' ;

      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR IN (''5'',''18'')' ;
      lv.Params.Labels.Value := 'stone, boulder' ;

      lv.ParamsList.Add ;
      lv.Params.Query := 'NATSUR IN (''5'',''8'')' ;
      lv.Params.Labels.Value := 'stone, cobbles' ;
    end
    else if _layerName='LNDRGN' then begin
      lv.Active := True ;
      lv.Caption := 'Land region' ;
      lv.Name := 'LNDRGN' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RADRNG' then begin
      lv.Active := True ;
      lv.Caption := 'Radar range' ;
      lv.Name := 'RADRNG' ;
      lv.Params.Area.Color := ParamColor( '255:204:204:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '255:128:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '255:128:255:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -6 ;
      lv.Params.Marker.Color := ParamColor( '255:204:204:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '255:204:204:255', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -1 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Cross ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ACHARE' then begin
      lv.Active := True ;
      lv.Caption := 'Anchorage area' ;
      lv.Name := 'ACHARE' ;
      lv.Params.Area.Color := ParamColor( '245:245:245:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '201:86:201:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.OutlineSymbol := SymbolList.Prepare('LIBSVG:S52:CLIFFINDICATOR01ext') ;
      assert( lv.Params.Area.OutlineSymbol <> nil ) ;
      lv.Params.Area.OutlineWidth := -11 ;
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:ACHARE02') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Area.SymbolGap := -20 ;
      lv.Params.Area.SymbolSize := -13 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '193:0:193:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.DiagCross ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CTNARE' then begin
      lv.Active := True ;
      lv.Caption := 'Caution area' ;
      lv.Name := 'CTNARE' ;
      lv.Params.Area.Color := ParamColor( '211:166:233:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FERYRT' then begin
      lv.Active := True ;
      lv.Caption := 'Ferry route' ;
      lv.Name := 'FERYRT' ;
      lv.Params.Area.Color := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Line.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PIPARE' then begin
      lv.Active := True ;
      lv.Caption := 'Pipeline area' ;
      lv.Name := 'PIPARE' ;
      lv.Params.Area.Color := ParamColor( '211:166:233:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CBLARE' then begin
      lv.Active := True ;
      lv.Caption := 'Cable area' ;
      lv.Name := 'CBLARE' ;
      lv.Params.Area.Color := ParamColor( '201:86:201:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '201:86:201:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.OutlineSymbol := SymbolList.Prepare('LIBSVG:S52:CLIFFINDICATOR01ext') ;
      assert( lv.Params.Area.OutlineSymbol <> nil ) ;
      lv.Params.Area.OutlineWidth := -11 ;
      {$ENDIF}
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '252:222:254:255', c ) ;
      lv.Params.Labels.Font.Color := ParamColor( 'PURPLE', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := 'cable area' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else 
      addStyle3( _layerName, _lv ) ;
  end ;

  procedure TGIS_LayerS57.addStyle3(
    const _layerName : String ;
    const _lv        : TGIS_LayerVector
   ) ;
  var
    lv  : TGIS_LayerVector ;
    c   : TGIS_Color ;
    def : TGIS_LabelPositions ;
  begin  
    lv := _lv ;
    if _layerName='DMPGRD' then begin
      lv.Active := True ;
      lv.Caption := 'Dumping ground' ;
      lv.Name := 'DMPGRD' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.OutlineSymbol := SymbolList.Prepare('LIBSVG:S52:CLIFFINDICATOR01ext') ;
      assert( lv.Params.Area.OutlineSymbol <> nil ) ;
      lv.Params.Area.OutlineWidth := -11 ;
      {$ENDIF}
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Allocator := False ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -50 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := 'dumping ground (not defined)' ;
      lv.Params.Labels.Width := -80 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -1 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'chemical waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG = ''2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'nuclear waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'explosives dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'spoil ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG = ''5''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'vessel dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -80 ;
      lv.Params.Labels.Value := 'chemical waste and explosives dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'CATDPG IN ( ''2,4'', ''4,2'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'chemical waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG = ''2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'nuclear waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'explosives dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'spoil ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG = ''5''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'vessel dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -80 ;
      lv.Params.Labels.Value := 'chemical waste and explosives dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'CATDPG IN ( ''2,4'', ''4,2'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'chemical waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'nuclear waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'explosives dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'spoil ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''5''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'vessel dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -80 ;
      lv.Params.Labels.Value := 'chemical waste and explosives dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG IN ( ''2,4'', ''4,2'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'chemical waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'nuclear waste dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'explosives dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'spoil ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''5''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := 'vessel dumping ground' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -80 ;
      lv.Params.Labels.Value := 'chemical waste and explosives dumping ground' ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'CATDPG IN ( ''2,4'', ''4,2'')' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='VEGATN' then begin
      lv.Active := True ;
      lv.Caption := 'Vegetation' ;
      lv.Name := 'VEGATN' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DYKCON' then begin
      lv.Active := True ;
      lv.Caption := 'Dyke' ;
      lv.Name := 'DYKCON' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='OBSTRN' then begin
      lv.Active := True ;
      lv.Caption := 'Obstruction' ;
      lv.Name := 'OBSTRN' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( '198:77:187:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '198:77:187:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -13 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:OBSTRN01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='WRECKS' then begin
      lv.Active := True ;
      lv.Caption := 'Wreck' ;
      lv.Name := 'WRECKS' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -15 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:WRECK01EXT') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LNDMRK' then begin
      lv.Active := True ;
      lv.Caption := 'Landmark' ;
      lv.Name := 'LNDMRK' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'INFORM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{INFORM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '202:0:219:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -2 ;
      lv.Params.Marker.Color := ParamColor( '41:41:41:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '41:41:41:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LNDMRK01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 2E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='NAVLNE' then begin
      lv.Active := True ;
      lv.Caption := 'Navigation line' ;
      lv.Name := 'NAVLNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='HRBFAC' then begin
      lv.Active := True ;
      lv.Caption := 'Harbour facility' ;
      lv.Name := 'HRBFAC' ;
      lv.Params.Area.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:HRBFAC01ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      lv.Params.Area.SymbolGap := -10 ;
      lv.Params.Area.SymbolSize := -13 ;
      {$ENDIF}
      lv.Params.Labels.Color := ParamColor( '245:245:245:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:MIDDLERIGHT:FLOW',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'BLACK', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:HRBFAC01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TOPMAR' then begin
      lv.Active := True ;
      lv.Caption := 'Top mark' ;
      lv.Name := 'TOPMAR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PRDARE' then begin
      lv.Active := True ;
      lv.Caption := 'Production / storage area' ;
      lv.Name := 'PRDARE' ;
      lv.Params.Area.Color := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ROADWY' then begin
      lv.Active := True ;
      lv.Caption := 'Road' ;
      lv.Name := 'ROADWY' ;
      lv.Params.Area.Color := ParamColor( '201:185:122:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LNDELV' then begin
      lv.Active := True ;
      lv.Caption := 'Land elevation' ;
      lv.Name := 'LNDELV' ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Follow ;
      lv.Params.Labels.Color := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Labels.Field := 'ELEVAT' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Color := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Line.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Marker.Color := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RAILWY' then begin
      lv.Active := True ;
      lv.Caption := 'Railway' ;
      lv.Name := 'RAILWY' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '139:102:31:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BUISGL' then begin
      lv.Active := True ;
      lv.Caption := 'Building, single' ;
      lv.Name := 'BUISGL' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PILBOP' then begin
      lv.Active := True ;
      lv.Caption := 'Pilot boarding place' ;
      lv.Name := 'PILBOP' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( '198:77:187:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '198:77:187:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:PILBOP02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 2E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='UWTROC' then begin
      lv.Active := True ;
      lv.Caption := 'Underwater rock / awash rock' ;
      lv.Name := 'UWTROC' ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'VALSOU' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -20 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER',def) ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Marker.Size := -15 ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.OutlineWidth := -1 ;
      lv.Params.Marker.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Visible := True ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
    end
    else if _layerName='SILTNK' then begin
      lv.Active := True ;
      lv.Caption := 'Silo / tank' ;
      lv.Name := 'SILTNK' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DRYDOC' then begin
      lv.Active := True ;
      lv.Caption := 'Dry dock' ;
      lv.Name := 'DRYDOC' ;
      lv.Params.Area.Color := ParamColor( '201:185:122:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CRANES' then begin
      lv.Active := True ;
      lv.Caption := 'Crane' ;
      lv.Name := 'CRANES' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_QUAL' then begin
      lv.Active := False ;
      lv.Caption := 'Quality of data' ;
      lv.Name := 'M_QUAL' ;
      lv.Params.Area.Color := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='MAGVAR' then begin
      lv.Active := False ;
      lv.Caption := 'Magnetic variation' ;
      lv.Name := 'MAGVAR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '211:166:233:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_NSYS' then begin
      lv.Active := False ;
      lv.Caption := 'Navigational system of marks' ;
      lv.Name := 'M_NSYS' ;
      lv.Params.Area.Color := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RESARE' then begin
      lv.Active := True ;
      lv.Caption := 'Restricted area' ;
      lv.Name := 'RESARE' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '202:0:219:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:OSPARE65ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      lv.Params.Area.SymbolGap := -20 ;
      lv.Params.Area.SymbolSize := -13 ;
      {$ELSE}
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -3 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := 'RESTRN=1' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:OSPARE56ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := 'RESTRN=3' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:BRTHNO01') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := 'RESTRN=7' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := 'RESTRN=1' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:OSPARE56ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := 'RESTRN=3' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:BRTHNO01') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Query := 'RESTRN=7' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'RESTRN=1' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:OSPARE56ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'RESTRN=3' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:BRTHNO01') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'RESTRN=7' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PILPNT' then begin
      lv.Active := True ;
      lv.Caption := 'Pile' ;
      lv.Name := 'PILPNT' ;
      lv.Params.Area.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'BLACK', c ) ;
      lv.Params.Marker.Size := -7 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 2.5E-5 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DEPCNT' then begin
      lv.Active := True ;
      lv.Caption := 'Depth contour' ;
      lv.Name := 'DEPCNT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '133:213:224:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SOUNDG' then begin
      lv.Active := True ;
      lv.Caption := 'Sounding' ;
      lv.Name := 'SOUNDG' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'SOUNDING' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -20 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER',def) ;
      lv.Params.Labels.Value := '{SOUNDING}' ;
      lv.Params.Labels.Width := -50 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -1 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RDOCAL' then begin
      lv.Active := True ;
      lv.Caption := 'Radio calling-in point' ;
      lv.Name := 'RDOCAL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Color := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='MORFAC' then begin
      lv.Active := True ;
      lv.Caption := 'Mooring/warping facility' ;
      lv.Name := 'MORFAC' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '7:7:7:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FOGSIG' then begin
      lv.Active := True ;
      lv.Caption := 'Fog signal' ;
      lv.Name := 'FOGSIG' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '211:166:233:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PIPSOL' then begin
      lv.Active := True ;
      lv.Caption := 'Pipeline, submarine/on land' ;
      lv.Name := 'PIPSOL' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Dash ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.DiagCross ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RADLNE' then begin
      lv.Active := True ;
      lv.Caption := 'Radar line' ;
      lv.Name := 'RADLNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RECTRC' then begin
      lv.Active := True ;
      lv.Caption := 'Recommended track' ;
      lv.Name := 'RECTRC' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Line.Symbol := SymbolList.Prepare('LIBSVG:S52:RECTRC92ext') ;
      assert( lv.Params.Line.Symbol <> nil ) ;
      lv.Params.Line.SymbolGap := -4 ;
      lv.Params.Line.Width := -17 ;
      {$ENDIF}
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DAMCON' then begin
      lv.Active := True ;
      lv.Caption := 'Dam' ;
      lv.Name := 'DAMCON' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Line.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CBLSUB' then begin
      lv.Active := True ;
      lv.Caption := 'Cable, submarine' ;
      lv.Name := 'CBLSUB' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '202:0:219:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Dash ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.DiagCross ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DISMAR' then begin
      lv.Active := True ;
      lv.Caption := 'Distance mark' ;
      lv.Name := 'DISMAR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SLCONS' then begin
      lv.Active := True ;
      lv.Caption := 'Shoreline Construction' ;
      lv.Name := 'SLCONS' ;
      lv.Params.Area.Color := ParamColor( '201:185:122:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '105:105:105:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Line.Style := TGIS_PenStyle.Dash ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='COALNE' then begin
      lv.Active := True ;
      lv.Caption := 'Coastline' ;
      lv.Name := 'COALNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SISTAW' then begin
      lv.Active := True ;
      lv.Caption := 'Signal station, warning' ;
      lv.Name := 'SISTAW' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '212:234:238:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SISTAT' then begin
      lv.Active := True ;
      lv.Caption := 'Signal station, traffic' ;
      lv.Name := 'SISTAT' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( '198:77:187:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '198:77:187:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:SISTAT02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TIDEWY' then begin
      lv.Active := True ;
      lv.Caption := 'Tideway' ;
      lv.Name := 'TIDEWY' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BCNLAT' then begin
      lv.Active := True ;
      lv.Caption := 'Beacon, lateral' ;
      lv.Name := 'BCNLAT' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Line.OutlineWidth := -1 ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'SILVER', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '64:64:64:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '55:55:55:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT15') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '55:55:55:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT16') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.OutlineColor := ParamColor( '55:55:55:255', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''MISSING''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '55:55:55:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT15') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '55:55:55:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT16') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.OutlineColor := ParamColor( '55:55:55:255', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''MISSING''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.OutlineWidth := 0 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineWidth := 0 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT15') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineWidth := 0 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT16') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.OutlineWidth := 0 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''MISSING''' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BRIDGE' then begin
      lv.Active := True ;
      lv.Caption := 'Bridge' ;
      lv.Name := 'BRIDGE' ;
      lv.Params.Area.Color := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '197:69:195:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Line.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BOYSPP' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy, special purpose/general' ;
      lv.Name := 'BOYSPP' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Line.OutlineWidth := -1 ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '41:198:178:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '105:32:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSAW12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'BOYSHP IN (''1'', ''3'', ''6'', ''7'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '148:243:12:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '155:37:96:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSPP35') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'BOYSHP IN (''2'', ''4'', ''5'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '41:198:178:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '105:32:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSAW12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'BOYSHP IN (''1'', ''3'', ''6'', ''7'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '148:243:12:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '155:37:96:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSPP35') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'BOYSHP IN (''2'', ''4'', ''5'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '41:198:178:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '105:32:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSAW12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'BOYSHP IN (''1'',''3'',''6'',''7'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '148:243:12:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '155:37:96:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSPP35') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'BOYSHP IN (''2'',''4'',''5'')' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BOYLAT' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy, lateral' ;
      lv.Name := 'BOYLAT' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Line.OutlineWidth := -1 ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '236:3:87:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '243:242:242:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '142:73:203:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT24') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR IN (''3'', ''3,4,3'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '245:35:189:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '243:242:242:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '24:181:178:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR IN (''4'',  ''4,3,4'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '236:3:87:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '239:236:236:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '142:73:203:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT24') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR IN (''3'', ''3,4,3'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '245:35:189:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '239:236:236:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '24:181:178:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR IN (''4'',  ''4,3,4'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '236:3:87:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '239:236:236:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '142:73:203:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT24') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR IN (''3'', ''3,4,3'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '245:35:189:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '239:236:236:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '24:181:178:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR IN (''4'', ''4,3,4'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '236:3:87:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '239:236:236:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '142:73:203:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT24') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR IN (''3'', ''3,4,3'')' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '245:35:189:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '239:236:236:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '24:181:178:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYLAT13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR IN (''4'', ''4,3,4'')' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BCNSPP' then begin
      lv.Active := True ;
      lv.Caption := 'Beacon, special purpose/general' ;
      lv.Name := 'BCNSPP' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '202:0:219:255', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -2 ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNLAT13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BOYCAR' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy, cardinal' ;
      lv.Name := 'BOYCAR' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Line.OutlineWidth := -1 ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT:FLOW',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT:FLOW',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT:FLOW',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT:FLOW',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '244:244:244:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Bold] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;
    end
    else 
      addStyle2( _layerName, _lv ) ;
  end ;

  procedure TGIS_LayerS57.addStyle2(
    const _layerName : String ;
    const _lv        : TGIS_LayerVector
   ) ;
  var
    lv  : TGIS_LayerVector ;
    c   : TGIS_Color ;
    def : TGIS_LabelPositions ;
  begin
    lv := _lv ;
    if _layerName='BCNCAR' then begin
      lv.Active := True ;
      lv.Caption := 'Beacon, cardinal' ;
      lv.Name := 'BCNCAR' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Line.OutlineWidth := -1 ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '25:97:89:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '198:38:47:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '209:139:50:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '189:59:219:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR02') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''2,6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '18:152:22:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '36:221:28:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR03') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '9:169:51:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '69:5:177:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '95:95:95:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BCNCAR04') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6,2,6''' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LIGHTS' then begin
      lv.Active := True ;
      lv.Caption := 'Light' ;
      lv.Name := 'LIGHTS' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineWidth := -1 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Line.OutlineWidth := -1 ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '95:218:216:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '215:211:146:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( 'WHITE', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '227:72:222:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '34:60:98:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS11') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '29:138:98:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '243:242:208:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '87:193:37:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '3:92:166:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.25E-5 ;
      lv.Params.Query := 'COLOUR = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '95:218:216:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '215:211:146:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( 'WHITE', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '227:72:222:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '34:60:98:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS11') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '29:138:98:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '243:242:208:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '87:193:37:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '3:92:166:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 5E-6 ;
      lv.Params.Query := 'COLOUR = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '95:218:216:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '215:211:146:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( 'WHITE', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '227:72:222:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '34:60:98:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS11') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '29:138:98:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '243:242:208:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '87:193:37:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '3:92:166:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''6''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := False ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '95:218:216:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '215:211:146:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( 'WHITE', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''1''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '227:72:222:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '34:60:98:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS11') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''3''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '29:138:98:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '243:242:208:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '124:240:91:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''4''' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Color := ParamColor( '87:193:37:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.OutlineWidth := 1 ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '3:92:166:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( 'GRAY', c ) ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.OutlineWidth := 0 ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '69:69:69:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:LIGHTS13') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'COLOUR = ''6''' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='GATCON' then begin
      lv.Active := True ;
      lv.Caption := 'Gate' ;
      lv.Name := 'GATCON' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Line.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Line.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CBLOHD' then begin
      lv.Active := True ;
      lv.Caption := 'Cable, overhead' ;
      lv.Name := 'CBLOHD' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Line.Color := ParamColor( '125:137:140:255', c ) ;
      lv.Params.Line.Style := TGIS_PenStyle.Dot ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PYLONS' then begin
      lv.Active := True ;
      lv.Caption := 'Pylon/bridge support' ;
      lv.Name := 'PYLONS' ;
      lv.Params.Area.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Color := ParamColor( '177:145:57:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '82:90:92:255', c ) ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TSSCRS' then begin
      lv.Active := True ;
      lv.Caption := 'Traffic Separation Scheme Crossing' ;
      lv.Name := 'TSSCRS' ;
      lv.Params.Area.Color := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.BDiagonal ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -3 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='MARCUL' then begin
      lv.Active := True ;
      lv.Caption := 'Marine farm/culture' ;
      lv.Name := 'MARCUL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BCNSAW' then begin
      lv.Active := True ;
      lv.Caption := 'Beacon, safe water' ;
      lv.Name := 'BCNSAW' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='lkbspt' then begin
      lv.Active := True ;
      lv.Caption := 'Lock basin part' ;
      lv.Name := 'lkbspt' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='brgare' then begin
      lv.Active := True ;
      lv.Caption := 'Bridge area' ;
      lv.Name := 'brgare' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='STSLNE' then begin
      lv.Active := True ;
      lv.Caption := 'Straight territorial sea baseline' ;
      lv.Name := 'STSLNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SMCFAC' then begin
      lv.Active := True ;
      lv.Caption := 'Small craft facility' ;
      lv.Name := 'SMCFAC' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TWRTPT' then begin
      lv.Active := True ;
      lv.Caption := 'Two-way route  part' ;
      lv.Name := 'TWRTPT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RIVBNK' then begin
      lv.Active := True ;
      lv.Caption := 'River bank' ;
      lv.Name := 'RIVBNK' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PONTON' then begin
      lv.Active := True ;
      lv.Caption := 'Pontoon' ;
      lv.Name := 'PONTON' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FNCLNE' then begin
      lv.Active := True ;
      lv.Caption := 'Fence/wall' ;
      lv.Name := 'FNCLNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LOKBSN' then begin
      lv.Active := True ;
      lv.Caption := 'Lock basin' ;
      lv.Name := 'LOKBSN' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='MIPARE' then begin
      lv.Active := True ;
      lv.Caption := 'Military practice area' ;
      lv.Name := 'MIPARE' ;
      lv.Params.Area.Color := ParamColor( '242:242:242:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '201:86:201:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.OutlineSymbol := SymbolList.Prepare('LIBSVG:S52:CLIFFINDICATOR01ext') ;
      assert( lv.Params.Area.OutlineSymbol <> nil ) ;
      lv.Params.Area.OutlineWidth := -11 ;
      {$ENDIF}
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( '255:232:255:255', c ) ;
      lv.Params.Labels.Font.Color := ParamColor( 'PURPLE', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := 'military practice' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.Color := ParamColor( '202:0:219:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.DiagCross ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='$CSYMB' then begin
      lv.Active := True ;
      lv.Caption := 'Cartographic symbol' ;
      lv.Name := '$CSYMB' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SLOTOP' then begin
      lv.Active := True ;
      lv.Caption := 'Slope topline' ;
      lv.Name := 'SLOTOP' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CURENT' then begin
      lv.Active := True ;
      lv.Caption := 'Current - non - gravitational' ;
      lv.Name := 'CURENT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='WATFAL' then begin
      lv.Active := True ;
      lv.Caption := 'Waterfall' ;
      lv.Name := 'WATFAL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CUSZNE' then begin
      lv.Active := True ;
      lv.Caption := 'Custom zone' ;
      lv.Name := 'CUSZNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='OFSPLF' then begin
      lv.Active := True ;
      lv.Caption := 'Offshore platform' ;
      lv.Name := 'OFSPLF' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '245:245:245:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'BLACK', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:OFSPLF01') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='wtware' then begin
      lv.Active := True ;
      lv.Caption := 'Waterway area' ;
      lv.Name := 'wtware' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RCTLPT' then begin
      lv.Active := True ;
      lv.Caption := 'Recommended Traffic Lane Part' ;
      lv.Name := 'RCTLPT' ;
      lv.Params.Area.Color := ParamColor( '215:0:215:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( '242:240:242:255', c ) ;
      lv.Params.Labels.Font.Color := ParamColor( 'FUCHSIA', c ) ;
      lv.Params.Labels.Font.Size := 22 ;
      lv.Params.Labels.FontSize := 440 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.RotateAsText := 'FIELD:ORIENT:0.000349065850398866' ;
      lv.Params.Labels.Value := #$21EA ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LOGPON' then begin
      lv.Active := True ;
      lv.Caption := 'Log pond' ;
      lv.Name := 'LOGPON' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CTSARE' then begin
      lv.Active := True ;
      lv.Caption := 'Cargo transshipment area' ;
      lv.Name := 'CTSARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RUNWAY' then begin
      lv.Active := True ;
      lv.Caption := 'Runway' ;
      lv.Name := 'RUNWAY' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='vehtrf' then begin
      lv.Active := True ;
      lv.Caption := 'Vehicle transfer' ;
      lv.Name := 'vehtrf' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='lokare' then begin
      lv.Active := True ;
      lv.Caption := 'Lock area' ;
      lv.Name := 'lokare' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DWRTPT' then begin
      lv.Active := True ;
      lv.Caption := 'Deep water route part' ;
      lv.Name := 'DWRTPT' ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='EXEZNE' then begin
      lv.Active := False ;
      lv.Caption := 'Exclusive Economic Zone' ;
      lv.Name := 'EXEZNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_UNIT' then begin
      lv.Active := False ;
      lv.Caption := 'Units of measurement of data' ;
      lv.Name := 'M_UNIT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TUNNEL' then begin
      lv.Active := True ;
      lv.Caption := 'Tunnel' ;
      lv.Name := 'TUNNEL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_CSCL' then begin
      lv.Active := False ;
      lv.Caption := 'Compilation scale of data' ;
      lv.Name := 'M_CSCL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='WEDKLP' then begin
      lv.Active := True ;
      lv.Caption := 'Weed/Kelp' ;
      lv.Name := 'WEDKLP' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='rtplpt' then begin
      lv.Active := True ;
      lv.Caption := 'Route planning point' ;
      lv.Name := 'rtplpt' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='C_ASSO' then begin
      lv.Active := True ;
      lv.Caption := 'Association' ;
      lv.Name := 'C_ASSO' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CONZNE' then begin
      lv.Active := False ;
      lv.Caption := 'Contiguous zone' ;
      lv.Name := 'CONZNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CANBNK' then begin
      lv.Active := True ;
      lv.Caption := 'Canal bank' ;
      lv.Name := 'CANBNK' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LITVES' then begin
      lv.Active := True ;
      lv.Caption := 'Light vessel' ;
      lv.Name := 'LITVES' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='boywtw' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy water-way' ;
      lv.Name := 'boywtw' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_VDAT' then begin
      lv.Active := False ;
      lv.Caption := 'Vertical datum of data' ;
      lv.Name := 'M_VDAT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ZEMCNT' then begin
      lv.Active := True ;
      lv.Caption := 'Zero meter contour' ;
      lv.Name := 'ZEMCNT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BCNISD' then begin
      lv.Active := True ;
      lv.Caption := 'Beacon, isolated danger' ;
      lv.Name := 'BCNISD' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( '243:229:77:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSPP11') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CTRPNT' then begin
      lv.Active := True ;
      lv.Caption := 'Control point' ;
      lv.Name := 'CTRPNT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='T_TIMS' then begin
      lv.Active := True ;
      lv.Caption := 'Tidal stream - time series' ;
      lv.Name := 'T_TIMS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SNDWAV' then begin
      lv.Active := True ;
      lv.Caption := 'Sand waves' ;
      lv.Name := 'SNDWAV' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TSSRON' then begin
      lv.Active := True ;
      lv.Caption := 'Traffic Separation Scheme  Roundabout' ;
      lv.Name := 'TSSRON' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TS_FEB' then begin
      lv.Active := True ;
      lv.Caption := 'Tidal stream - flood/ebb' ;
      lv.Name := 'TS_FEB' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='MONUMT' then begin
      lv.Active := True ;
      lv.Caption := 'Monument' ;
      lv.Name := 'MONUMT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TS_PRH' then begin
      lv.Active := True ;
      lv.Caption := 'Tidal stream - harmonic prediction' ;
      lv.Name := 'TS_PRH' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='WATTUR' then begin
      lv.Active := True ;
      lv.Caption := 'Water turbulence' ;
      lv.Name := 'WATTUR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SQUARE' then begin
      lv.Active := True ;
      lv.Caption := 'Square' ;
      lv.Name := 'SQUARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RADSTA' then begin
      lv.Active := True ;
      lv.Caption := 'Radar station' ;
      lv.Name := 'RADSTA' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TOWERS' then begin
      lv.Active := True ;
      lv.Caption := 'Tower' ;
      lv.Name := 'TOWERS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RADRFL' then begin
      lv.Active := True ;
      lv.Caption := 'Radar reflector' ;
      lv.Name := 'RADRFL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FLODOC' then begin
      lv.Active := True ;
      lv.Caption := 'Floating dock' ;
      lv.Name := 'FLODOC' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='C_AGGR' then begin
      lv.Active := True ;
      lv.Caption := 'Aggregation' ;
      lv.Name := 'C_AGGR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='T_HMON' then begin
      lv.Active := True ;
      lv.Caption := 'Tide - harmonic prediction' ;
      lv.Name := 'T_HMON' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FSHZNE' then begin
      lv.Active := False ;
      lv.Caption := 'Fishery zone' ;
      lv.Name := 'FSHZNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BOYSAW' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy, safe water' ;
      lv.Name := 'BOYSAW' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( '238:90:108:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '62:62:62:255', c ) ;
      lv.Params.Marker.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -20 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BOYSAW12') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TSSBND' then begin
      lv.Active := True ;
      lv.Caption := 'Traffic Separation Scheme  Boundary' ;
      lv.Name := 'TSSBND' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Dash ;
      lv.Params.Line.Width := -3 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TSELNE' then begin
      lv.Active := True ;
      lv.Caption := 'Traffic Separation Line' ;
      lv.Name := 'TSELNE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='AIRARE' then begin
      lv.Active := True ;
      lv.Caption := 'Airport / airfield' ;
      lv.Name := 'AIRARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RETRFL' then begin
      lv.Active := True ;
      lv.Caption := 'Retro-reflector' ;
      lv.Name := 'RETRFL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DOCARE' then begin
      lv.Active := True ;
      lv.Caption := 'Dock area' ;
      lv.Name := 'DOCARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FAIRWY' then begin
      lv.Active := True ;
      lv.Caption := 'Fairway' ;
      lv.Name := 'FAIRWY' ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='$TEXTS' then begin
      lv.Active := True ;
      lv.Caption := 'Text' ;
      lv.Name := '$TEXTS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='wtwaxs' then begin
      lv.Active := True ;
      lv.Caption := 'waterway axis' ;
      lv.Name := 'wtwaxs' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ICNARE' then begin
      lv.Active := True ;
      lv.Caption := 'Incineration area' ;
      lv.Name := 'ICNARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TS_TIS' then begin
      lv.Active := True ;
      lv.Caption := 'Tidal stream - time series' ;
      lv.Name := 'TS_TIS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='termnl' then begin
      lv.Active := True ;
      lv.Caption := 'terminal' ;
      lv.Name := 'termnl' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CONVYR' then begin
      lv.Active := True ;
      lv.Caption := 'Conveyor' ;
      lv.Name := 'CONVYR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_SREL' then begin
      lv.Active := False ;
      lv.Caption := 'Survey reliability' ;
      lv.Name := 'M_SREL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SLOGRD' then begin
      lv.Active := True ;
      lv.Caption := 'Sloping ground' ;
      lv.Name := 'SLOGRD' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ISTZNE' then begin
      lv.Active := True ;
      lv.Caption := 'Inshore traffic zone' ;
      lv.Name := 'ISTZNE' ;
      lv.Params.Area.Color := ParamColor( '198:77:187:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '198:77:187:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.OutlineSymbol := SymbolList.Prepare('LIBSVG:S52:CLIF01ext') ;
      lv.Params.Area.OutlineWidth := -11 ;
      assert( lv.Params.Area.OutlineSymbol <> nil ) ;
      {$ENDIF}
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( '255:213:255:255', c ) ;
      lv.Params.Labels.Font.Color := ParamColor( 'PURPLE', c ) ;
      lv.Params.Labels.Font.Size := 9 ;
      lv.Params.Labels.FontSize := 180 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.Value := 'inshore traffic zone' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -17 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='$AREAS' then begin
      lv.Active := True ;
      lv.Caption := 'Cartographic area' ;
      lv.Name := '$AREAS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RTPBCN' then begin
      lv.Active := True ;
      lv.Caption := 'Radar transponder beacon' ;
      lv.Name := 'RTPBCN' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CHNWIR' then begin
      lv.Active := True ;
      lv.Caption := 'Chain/Wire' ;
      lv.Name := 'CHNWIR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ICEARE' then begin
      lv.Active := True ;
      lv.Caption := 'Ice area' ;
      lv.Name := 'ICEARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='wtwprf' then begin
      lv.Active := True ;
      lv.Caption := 'waterway profile' ;
      lv.Name := 'wtwprf' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LITFLT' then begin
      lv.Active := True ;
      lv.Caption := 'Light float' ;
      lv.Name := 'LITFLT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='T_NHMN' then begin
      lv.Active := True ;
      lv.Caption := 'Tide - non-harmonic prediction' ;
      lv.Name := 'T_NHMN' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PIPOHD' then begin
      lv.Active := True ;
      lv.Caption := 'Pipeline, overhead' ;
      lv.Name := 'PIPOHD' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='bcnwtw' then begin
      lv.Active := True ;
      lv.Caption := 'Beacon water-way' ;
      lv.Name := 'bcnwtw' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='lg_vsp' then begin
      lv.Active := True ;
      lv.Caption := 'Maximum permitted vessel speed' ;
      lv.Name := 'lg_vsp' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='trnbsn' then begin
      lv.Active := True ;
      lv.Caption := 'turning basin' ;
      lv.Name := 'trnbsn' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_HDAT' then begin
      lv.Active := False ;
      lv.Caption := 'Horizontal datum of data' ;
      lv.Name := 'M_HDAT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='UNSARE' then begin
      lv.Active := True ;
      lv.Caption := 'Unsurveyed area' ;
      lv.Name := 'UNSARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FRPARE' then begin
      lv.Active := True ;
      lv.Caption := 'Free port area' ;
      lv.Name := 'FRPARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='wtwgag' then begin
      lv.Active := True ;
      lv.Caption := 'Waterway gauge' ;
      lv.Name := 'wtwgag' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BOYINB' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy, installation' ;
      lv.Name := 'BOYINB' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='OILBAR' then begin
      lv.Active := True ;
      lv.Caption := 'Oil barrier' ;
      lv.Name := 'OILBAR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TSEZNE' then begin
      lv.Active := True ;
      lv.Caption := 'Traffic Separation Zone' ;
      lv.Name := 'TSEZNE' ;
      lv.Params.Area.Color := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:RASTER02ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      lv.Params.Area.SymbolGap := 0 ;
      lv.Params.Area.SymbolSize := -15 ;
      {$ELSE}
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Area.OutlineColor := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Solid ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -17 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='excnst' then begin
      lv.Active := True ;
      lv.Caption := 'Exceptional navigation strcuture' ;
      lv.Name := 'excnst' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DWRTCL' then begin
      lv.Active := True ;
      lv.Caption := 'Deep water route centerline' ;
      lv.Name := 'DWRTCL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ACHBRT' then begin
      lv.Active := True ;
      lv.Caption := 'Anchor berth' ;
      lv.Name := 'ACHBRT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SPRING' then begin
      lv.Active := True ;
      lv.Caption := 'Spring' ;
      lv.Name := 'SPRING' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TSSLPT' then begin
      lv.Active := True ;
      lv.Caption := 'Traffic Separation Scheme  Lane part' ;
      lv.Name := 'TSSLPT' ;
      lv.Params.Area.Color := ParamColor( '215:0:215:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Alignment := TGIS_LabelAlignment.Center ;
      lv.Params.Labels.Color := ParamColor( '245:245:245:255', c ) ;
      lv.Params.Labels.Font.Color := ParamColor( 'FUCHSIA', c ) ;
      lv.Params.Labels.Font.Size := 22 ;
      lv.Params.Labels.FontSize := 440 ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLECENTER:FLOW',def) ;
      lv.Params.Labels.RotateAsText := 'FIELD:ORIENT:0.000349065850398866' ;
      lv.Params.Labels.Value := #$21D1 ;
      lv.Params.Labels.Width := -100 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1.000000001E-9 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='bunsta' then begin
      lv.Active := True ;
      lv.Caption := 'Bunker station' ;
      lv.Name := 'bunsta' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BOYISD' then begin
      lv.Active := True ;
      lv.Caption := 'Buoy, isolated danger' ;
      lv.Name := 'BOYISD' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='comare' then begin
      lv.Active := True ;
      lv.Caption := 'Communication area' ;
      lv.Name := 'comare' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CHKPNT' then begin
      lv.Active := True ;
      lv.Caption := 'Checkpoint' ;
      lv.Name := 'CHKPNT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='HULKES' then begin
      lv.Active := True ;
      lv.Caption := 'Hulk' ;
      lv.Name := 'HULKES' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RAPIDS' then begin
      lv.Active := True ;
      lv.Caption := 'Rapids' ;
      lv.Name := 'RAPIDS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BERTHS' then begin
      lv.Active := True ;
      lv.Caption := 'Berth' ;
      lv.Name := 'BERTHS' ;
      lv.Params.Area.Color := ParamColor( 'GRAY', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( '255:234:255:255', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Color := ParamColor( 'PURPLE', c ) ;
      lv.Params.Labels.Font.Name := 'Arial Narrow' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Font.Style := [TGIS_FontStyle.Italic] ;
      lv.Params.Labels.Height := -100 ;
      lv.Params.Labels.Position := ParamPosition( 'MIDDLERIGHT',def) ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Labels.Width := -600 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.Color := ParamColor( '185:0:185:255', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( '185:0:185:255', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:BRTHNO01ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 2.5E-5 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SWPARE' then begin
      lv.Active := True ;
      lv.Caption := 'Swept Area' ;
      lv.Name := 'SWPARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_PROD' then begin
      lv.Active := False ;
      lv.Caption := 'Production information' ;
      lv.Name := 'M_PROD' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='hrbbsn' then begin
      lv.Active := True ;
      lv.Caption := 'Harbour basin' ;
      lv.Name := 'hrbbsn' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='C_STAC' then begin
      lv.Active := True ;
      lv.Caption := 'Stacked on/stacked under' ;
      lv.Name := 'C_STAC' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='BUIREL' then begin
      lv.Active := True ;
      lv.Caption := 'Building, religous' ;
      lv.Name := 'BUIREL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RSCSTA' then begin
      lv.Active := True ;
      lv.Caption := 'Rescue station' ;
      lv.Name := 'RSCSTA' ;
      lv.Params.Area.Color := ParamColor( 'RED', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -1 ;
      lv.Params.Marker.OutlineColor := ParamColor( 'RED', c ) ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Marker.Size := -25 ;
      lv.Params.Marker.Symbol := SymbolList.Prepare('LIBSVG:S52:RSCSTA68ext') ;
      assert( lv.Params.Marker.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 2E-6 ;
      lv.Params.Visible := True ;
    end
    else if _layerName='ACHPNT' then begin
      lv.Active := True ;
      lv.Caption := 'Anchor' ;
      lv.Name := 'ACHPNT' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='lg_sdm' then begin
      lv.Active := True ;
      lv.Caption := 'Maximum permitted ship dimensions' ;
      lv.Name := 'lg_sdm' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='$LINES' then begin
      lv.Active := True ;
      lv.Caption := 'Cartographic line' ;
      lv.Name := '$LINES' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='HRBARE' then begin
      lv.Active := True ;
      lv.Caption := 'Harbour area (administrative)' ;
      lv.Name := 'HRBARE' ;
      lv.Params.Area.Color := ParamColor( '163:180:183:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '165:166:148:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dot ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
      lv.Params.Visible := True ;
    end
    else if _layerName='tisdge' then begin
      lv.Active := True ;
      lv.Caption := 'Time Schedule - in general' ;
      lv.Name := 'tisdge' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='notmrk' then begin
      lv.Active := True ;
      lv.Caption := 'Notice mark' ;
      lv.Name := 'notmrk' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='COSARE' then begin
      lv.Active := True ;
      lv.Caption := 'Continental shelf area' ;
      lv.Name := 'COSARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LOCMAG' then begin
      lv.Active := False ;
      lv.Caption := 'Local magnetic anomaly' ;
      lv.Name := 'LOCMAG' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='prtare' then begin
      lv.Active := True ;
      lv.Caption := 'port area' ;
      lv.Name := 'prtare' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='refdmp' then begin
      lv.Active := True ;
      lv.Caption := 'refuse dump' ;
      lv.Name := 'refdmp' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_NPUB' then begin
      lv.Active := False ;
      lv.Caption := 'Nautical publication information' ;
      lv.Name := 'M_NPUB' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RCRTCL' then begin
      lv.Active := True ;
      lv.Caption := 'Recommended route centerline' ;
      lv.Name := 'RCRTCL' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TS_PNH' then begin
      lv.Active := True ;
      lv.Caption := 'Tidal stream - non-harmonic prediction' ;
      lv.Name := 'TS_PNH' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='OSPARE' then begin
      lv.Active := True ;
      lv.Caption := 'Offshore production area' ;
      lv.Name := 'OSPARE' ;
      lv.Params.Area.Color := ParamColor( '255:210:255:255', c ) ;
      lv.Params.Area.OutlineColor := ParamColor( '202:0:219:255', c ) ;
      lv.Params.Area.OutlineStyle := TGIS_PenStyle.Dash ;
      lv.Params.Area.OutlineWidth := -1 ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:OSPARE65ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      lv.Params.Area.SymbolGap := -20 ;
      lv.Params.Area.SymbolSize := -13 ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Line.Color := ParamColor( '183:0:183:255', c ) ;
      lv.Params.Line.OutlinePattern := TGIS_BrushStyle.Clear ;
      lv.Params.Line.OutlineStyle := TGIS_PenStyle.Clear ;
      lv.Params.Line.Style := TGIS_PenStyle.Clear ;
      lv.Params.Line.Width := -3 ;
      lv.Params.Marker.Color := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.OutlineColor := ParamColor( 'LIME', c ) ;
      lv.Params.Marker.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Marker.Size := -20 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'RESTRN=1' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:OSPARE56ext') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'RESTRN=3' ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Area.Symbol := nil ;
      lv.Params.Area.SymbolGap := 120 ;
      lv.Params.Area.SymbolSize := 120 ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Visible := True ;

      lv.ParamsList.Add ;
      {$IFDEF GIS_S57_SYMBOLOGY}
      lv.Params.Area.Symbol := SymbolList.Prepare('LIBSVG:S52:BRTHNO01') ;
      assert( lv.Params.Area.Symbol <> nil ) ;
      {$ENDIF}
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.MaxScale := 1 ;
      lv.Params.MinScale := 1E-6 ;
      lv.Params.Query := 'RESTRN=7' ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FORSTC' then begin
      lv.Active := True ;
      lv.Caption := 'Fortified structure' ;
      lv.Name := 'FORSTC' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CAUSWY' then begin
      lv.Active := True ;
      lv.Caption := 'Causeway' ;
      lv.Name := 'CAUSWY' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='$COMPS' then begin
      lv.Active := True ;
      lv.Caption := 'Compass' ;
      lv.Name := '$COMPS' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SPLARE' then begin
      lv.Active := True ;
      lv.Caption := 'Sea-plane landing area' ;
      lv.Name := 'SPLARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='SUBTLN' then begin
      lv.Active := True ;
      lv.Caption := 'Submarine transit lane' ;
      lv.Name := 'SUBTLN' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_ACCY' then begin
      lv.Active := False ;
      lv.Caption := 'Accuracy of data' ;
      lv.Name := 'M_ACCY' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='GRIDRN' then begin
      lv.Active := True ;
      lv.Caption := 'Gridiron' ;
      lv.Name := 'GRIDRN' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='_extgn' then begin
      lv.Active := True ;
      lv.Caption := 'Light, extinguished' ;
      lv.Name := '_extgn' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='CGUSTA' then begin
      lv.Active := True ;
      lv.Caption := 'Coastguard station' ;
      lv.Name := 'CGUSTA' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='LAKSHR' then begin
      lv.Active := True ;
      lv.Caption := 'Lake shore' ;
      lv.Name := 'LAKSHR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FSHFAC' then begin
      lv.Active := True ;
      lv.Caption := 'Fishing facility' ;
      lv.Name := 'FSHFAC' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='M_HOPA' then begin
      lv.Active := False ;
      lv.Caption := 'Horizontal datum shift parameters' ;
      lv.Name := 'M_HOPA' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='RDOSTA' then begin
      lv.Active := True ;
      lv.Caption := 'Radio station' ;
      lv.Name := 'RDOSTA' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='DAYMAR' then begin
      lv.Active := True ;
      lv.Caption := 'Daymark' ;
      lv.Name := 'DAYMAR' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='TESARE' then begin
      lv.Active := False ;
      lv.Caption := 'Territorial sea area' ;
      lv.Name := 'TESARE' ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='PRCARE' then begin
      lv.Active := True ;
      lv.Caption := 'Precautionary area' ;
      lv.Name := 'PRCARE' ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.BDiagonal ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else if _layerName='FSHGRD' then begin
      lv.Active := True ;
      lv.Caption := 'Fishing ground' ;
      lv.Name := 'FSHGRD' ;
      lv.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      lv.Params.Labels.Color := ParamColor( 'BLACK', c ) ;
      lv.Params.Labels.Field := 'OBJNAM' ;
      lv.Params.Labels.Font.Size := 8 ;
      lv.Params.Labels.Value := '{OBJNAM}' ;
      lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
      lv.Params.Visible := True ;
    end
    else
      assert( False, _layerName + ' style not found' ) ;
  end ;

  procedure TGIS_LayerS57.prepareSubLayer(
    const _name    : String  ;
    const _caption : String  ;
    const _gtype   : Integer
  ) ;
  var
    lv : TGIS_LayerVector ;
  begin
    oLayer := TGIS_LayerVector( Get( _name ) ) ;
    if oLayer <> nil then exit ;

    lv := TGIS_LayerVector.Create ;
    lv.Name     := _name ;
    lv.Caption  := _caption ;
    lv.CS := CS ;
    case _gtype of
      1 : lv.DefaultShapeType := TGIS_ShapeType.Point ;
      2 : lv.DefaultShapeType := TGIS_ShapeType.Arc ;
      3 : lv.DefaultShapeType := TGIS_ShapeType.Polygon ;
    end ;
    Add( lv ) ;
    lv.SupportedDimensions := GisGetDimensionType( TGIS_DimensionType.XY ) ;

    if UseS57Draw then begin
      lv.Params.Area.OutlineColor := TGIS_Color.Gray ;
      lv.Params.Labels.Color      := TGIS_Color.Black  ;
    end ;

    if _name = 'SOUNDG' then begin
      lv.Params.Labels.Field := S57_SOUNDING_FIELD ;
      lv.Params.Labels.Color := TGIS_Color.Gray  ;
      lv.Params.Labels.FontSize := 7 * 20 ;
      lv.Params.Labels.FontColor := TGIS_Color.Gray ;
    end
    else
      lv.Params.Labels.Field := 'OBJNAM' ;

    if ( _name = 'M_COVR' ) or ( _name = 'M_NPUB' ) or ( _name = 'M_NSYS' ) or
       ( _name = 'M_QUAL' ) or ( _name = 'M_ACCY' ) or ( _name = 'LOCMAG' ) or
       ( _name = 'M_PROD' ) or ( _name = 'M_SDAT' ) or ( _name = 'M_SREL' ) or
       ( _name = 'M_UNIT' ) or ( _name = 'M_VDAT' ) or ( _name = 'MAGVAR' ) or
       ( _name = 'M_CSCL' ) or ( _name = 'M_HDAT' ) or ( _name = 'M_HOPA' ) or
       ( _name = 'TESARE' ) or ( _name = 'FSHZNE' ) or
       ( _name = 'ADMARE' ) or ( _name = 'CONZNE' ) then
      lv.Active := False;

    lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;

    oLayer := lv ;

    addStyle1( _name, oLayer ) ;
  end ;

  procedure TGIS_LayerS57.setUp ;
  begin
    inherited;

    RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
    Lock ;
    try
      FileS57 := TGIS_FileS57.Create( Path ) ;

      // events
      {$IFNDEF OXYGENE}
        TGIS_FileS57( FileS57 ).NewShapeEvent := OnBeginShapeEvent ;
        TGIS_FileS57( FileS57 ).EndShapeEvent := OnEndShapeEvent   ;
        TGIS_FileS57( FileS57 ).AddPartEvent  := OnAddPartEvent    ;
        TGIS_FileS57( FileS57 ).AddCoordEvent := OnAddCoordEvent   ;
        TGIS_FileS57( FileS57 ).AddAttrEvent  := OnAddAttrEvent    ;
        TGIS_FileS57( FileS57 ).SetLayerEvent := OnSetLayerEvent   ;
      {$ELSE}
        TGIS_FileS57( FileS57 ).NewShapeEvent := @OnBeginShapeEvent ;
        TGIS_FileS57( FileS57 ).EndShapeEvent := @OnEndShapeEvent   ;
        TGIS_FileS57( FileS57 ).AddPartEvent  := @OnAddPartEvent    ;
        TGIS_FileS57( FileS57 ).AddCoordEvent := @OnAddCoordEvent   ;
        TGIS_FileS57( FileS57 ).AddAttrEvent  := @OnAddAttrEvent    ;
        TGIS_FileS57( FileS57 ).SetLayerEvent := @OnSetLayerEvent   ;
      {$ENDIF}

      // settings
      TGIS_FileS57( FileS57 ).UseS57Draw := UseS57Draw      ;
      TGIS_FileS57( FileS57 ).LayerName  := LayerName       ;

      SetCSByEPSG( GIS_EPSG_WGS84 ) ;
      try
        TGIS_FileS57( FileS57 ).ParseS57 ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 0 ) ;
      end ;

      if SafeFileExists( Path ) then
         FAge := GisFileAge( Path ) ;

      FFileInfo := 'IHO S-57 Native format' ;

      ReadConfig ;

    finally
      Unlock ;
      RaiseBusyRelease( Self ) ;

      FreeObject( FileS57 ) ;
      FIsModified := False ;
    end ;
  end ;
  
  
//==============================================================================
// Unit_GisLayerS57
//==============================================================================  

  class procedure Unit_GisLayerS57.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-S57', 'IHO S-57 ENC', TGIS_LayerS57, '.000',
                   TGIS_RegisteredLayerType.Vector, 
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisLayerS57.SelfRegisterLayer() ;
{$ENDIF}


//==================================== END =====================================
end.

