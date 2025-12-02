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
  Common pipeline operations.
}
{$IFDEF DCC}
  unit GisPipelineOperations ;
  {$HPPEMIT '#pragma link "GisPipelineOperations"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type
  {#gendoc:hide}
  // Initialization section handler
  Unit_GisPipelineOperations = class
    public
      class procedure SelfRegisterPipeline() ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Math,
    System.Sysutils,

    GisCsSystems,
    GisFunctions,
    GisInterfaces,
    GisInterpolation,
    GisLayer,
    GisLayerPixel,
    GisLayerVector,
    GisPipeline,
    GisRegistredLayers,
    GisResource,
    GisRtl,
    GisTypes,
    GisTypesUI,
    GisViewer;
{$ENDIF}

{$REGION 'Common pipeline operations'}
type
  /// <summary>
  ///   Implementation of the 'GIS.Close' command.
  /// </summary>
  T_Pipeline_GisClose = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'GIS.Update' command.
  /// </summary>
  T_Pipeline_GisUpdate = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'GIS.FullExtent' command.
  /// </summary>
  T_Pipeline_GisFullExtent = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'GIS.ZoomToLayer' command.
  /// </summary>
  T_Pipeline_GisZoomToLayer = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.Open' command.
  /// </summary>
  T_Pipeline_LayerOpen = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.Close' command.
  /// </summary>
  T_Pipeline_LayerClose = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.Get' command.
  /// </summary>
  T_Pipeline_LayerGet = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.Save' command.
  /// </summary>
  T_Pipeline_LayerSave = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the abstract class for 'Layer.CreateGrid' and
  ///   'Layer.CreatePixel' commands.
  /// </summary>
  T_Pipeline_LayerCreatePixelAbstract =
    {$IFDEF OXYGENE} abstract {$ENDIF} class( TGIS_PipelineOperationAbstract )
    const
      PARAM_WIDTH       = 'Width' ;
      PARAM_HEIGHT      = 'Height' ;
      PARAM_PIXELSIZE   = 'PixelSize' ;
      DEFAULT_WIDTH     = 1024 ;
      DEFAULT_HEIGHT    = 0 ;
      DEFAULT_PIXELSIZE = 0.0 ;

    protected
      function isGrid : Boolean ; virtual ; abstract ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.CreateGrid' command.
  /// </summary>
  T_Pipeline_LayerCreateGrid = class( T_Pipeline_LayerCreatePixelAbstract )
    const
      IS_GRID = True ;

    protected
      function isGrid : Boolean ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.CreatePixel' command.
  /// </summary>
  T_Pipeline_LayerCreatePixel = class( T_Pipeline_LayerCreatePixelAbstract )
    const
      IS_GRID = False ;

    protected
      function isGrid : Boolean ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.CreateVector' command.
  /// </summary>
  T_Pipeline_LayerCreateVector = class( TGIS_PipelineOperationAbstract )
    const
      PARAM_SHAPE_TYPE     = 'ShapeType' ;
      PARAM_DIMENSION_TYPE = 'DimensionType' ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Layer.AddField' command.
  /// </summary>
  T_Pipeline_LayerAddField = class( TGIS_PipelineOperationAbstract )
    const
      PARAM_TYPE            = 'Type' ;
      PARAM_WIDTH           = 'Width' ;
      PARAM_DECIMAL         = 'Decimal' ;
      DEFAULT_FIELD_NAME    = 'NewField' ;
      DEFAULT_FIELD_WIDTH   = 6 ;
      DEFAULT_FIELD_DECIMAL = 0 ;
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Grid.GenerateRamp' command.
  /// </summary>
  T_Pipeline_GridGenerateRamp = class( TGIS_PipelineOperationAbstract )
    const
      PARAM_START_COLOR     = 'StartColor' ;
      PARAM_MID_COLOR       = 'MidColor' ;
      PARAM_END_COLOR       = 'EndColor' ;
      PARAM_MIN_VAL         = 'MinValue' ;
      PARAM_MID_VAL         = 'MidValue' ;
      PARAM_MAX_VAL         = 'MaxValue' ;
      PARAM_USE_MIDDLE      = 'UseMiddle' ;
      DEFAULT_USE_MIDDLE    = True ;
      PARAM_RAMP_INTERVAL   = 'RampInterval' ;
      PARAM_LEGEND_INTERVAL = 'LegendInterval' ;
      PARAM_COLOR_SPACE     = 'ColorSpace' ;
      COLOR_SPACE_RGB       = 'RGB' ;
      COLOR_SPACE_HSL       = 'HSL' ;
      COLOR_SPACE_HSL360    = 'HSL360' ;
      DEFAULT_COLOR_SPACE   = COLOR_SPACE_HSL ;

    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end;

class procedure Unit_GisPipelineOperations.SelfRegisterPipeline() ;
begin
  RegisterPipeline( 'GIS.Close',          T_Pipeline_GisClose ) ;
  RegisterPipeline( 'GIS.FullExtent',     T_Pipeline_GisFullExtent ) ;
  RegisterPipeline( 'GIS.Update',         T_Pipeline_GisUpdate ) ;
  RegisterPipeline( 'GIS.ZoomToLayer',    T_Pipeline_GisZoomToLayer ) ;

  RegisterPipeline( 'Layer.Get',          T_Pipeline_LayerGet ) ;
  RegisterPipeline( 'Layer.Open',         T_Pipeline_LayerOpen ) ;
  RegisterPipeline( 'Layer.Close',        T_Pipeline_LayerClose ) ;
  RegisterPipeline( 'Layer.Save',         T_Pipeline_LayerSave ) ;
  RegisterPipeline( 'Layer.CreateGrid',   T_Pipeline_LayerCreateGrid ) ;
  RegisterPipeline( 'Layer.CreatePixel',  T_Pipeline_LayerCreatePixel ) ;
  RegisterPipeline( 'Layer.CreateVector', T_Pipeline_LayerCreateVector ) ;
  RegisterPipeline( 'Layer.AddField',     T_Pipeline_LayerAddField ) ;

  RegisterPipeline( 'Grid.GenerateRamp',  T_Pipeline_GridGenerateRamp ) ;
end ;

{$ENDREGION 'Common pipeline operations'}

{$REGION 'T_Pipeline_GisClose'}
procedure T_Pipeline_GisClose.Initialize ;
begin
  // no parameters
end;

procedure T_Pipeline_GisClose.Execute ;
var
  i     : Integer ;
  keys  : TGIS_StringList ;
  {$IFDEF DCC}
    elm : TPair< String, TObject > ;
  {$ENDIF}
begin
  keys := TGIS_StringList.Create ;

  try
    // find keys to set nil
    for elm in Parent.Variables do begin
      if elm.Value is TGIS_Layer then
        keys.Add( elm.Key ) ;
    end ;

    for i := 0 to keys.Count - 1 do
      Parent.SetVar( keys[i] , nil ) ;

    Parent.GIS.Close ;
  finally
    FreeObject( keys ) ;
  end;
end;
{$ENDREGION 'T_Pipeline_GisClose'}

{$REGION 'T_Pipeline_GisUpdate'}
procedure T_Pipeline_GisUpdate.Initialize ;
begin
  // no parameters
end;

procedure T_Pipeline_GisUpdate.Execute ;
begin
  Parent.GIS.InvalidateWholeMap ;
end;
{$ENDREGION 'T_Pipeline_GisUpdate'}

{$REGION 'T_Pipeline_GisFullExtent'}
procedure T_Pipeline_GisFullExtent.Initialize ;
begin
  // no parameters
end;

procedure T_Pipeline_GisFullExtent.Execute ;
begin
  Parent.GIS.FullExtent ;
end;
{$ENDREGION 'T_Pipeline_GisFullExtent'}

{$REGION 'T_Pipeline_GisZoomToLayer'}
procedure T_Pipeline_GisZoomToLayer.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_LAYER,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  ) ;
end;

procedure T_Pipeline_GisZoomToLayer.Execute ;
var
  param : String ;
  obj   : TObject;
begin
  param := ParamAsString( GIS_PIPELINE_PARAM_LAYER ) ;
  obj := Parent.GetVar( param ) ;
  if obj is TGIS_Layer then
    Parent.GIS.VisibleExtent := TGIS_Layer( obj ).Extent ;
end;
{$ENDREGION 'T_Pipeline_GisZoomToLayer'}

{$REGION 'T_Pipeline_LayerOpen'}
procedure T_Pipeline_LayerOpen.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_RESULT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_NAME,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_PATH,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_PATH,
    ''
  );
end;

procedure T_Pipeline_LayerOpen.Execute ;
var
  param : String ;
  path  : String ;
  name  : String ;
  layer : TGIS_Layer ;
begin
  path := ParamAsString( GIS_PIPELINE_PARAM_PATH ) ;
  name := ParamAsString( GIS_PIPELINE_PARAM_NAME, '' ) ;

  layer := GisCreateLayer( name, path ) ;
  Parent.GIS.Add( layer ) ;

  // Result
  param := ParamAsString( GIS_PIPELINE_PARAM_RESULT ) ;
  Parent.SetVar( param, layer ) ;
end;
{$ENDREGION 'T_Pipeline_LayerOpen'}

{$REGION 'T_Pipeline_LayerClose'}
procedure T_Pipeline_LayerClose.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_LAYER,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  ) ;
end;

procedure T_Pipeline_LayerClose.Execute ;
var
  param : String ;
  obj   : TObject ;
  lst : TList<String> ;
  {$IFDEF DCC}
    str : String ;
    elm : TPair< String, TObject > ;
  {$ENDIF}
begin

  param := ParamAsString( GIS_PIPELINE_PARAM_LAYER ) ;

  obj := Parent.GetVar( param ) ;
  if obj is TGIS_Layer then
    param := TGIS_Layer( obj ).Name ;

  if Parent.GIS.Get( param ) <> nil then begin

    lst := TList<String>.Create ;
    try
      // find key to set nil (we don't remove vars, only set nil to var)
      for elm in Parent.Variables do begin
        if ( elm.Value is TGIS_Layer ) and
           ( TGIS_Layer( elm.Value ).Name = param ) then
        begin
          lst.Add( elm.Key ) ;
        end;
      end ;

      for str in lst do
        Parent.SetVar( str, nil ) ;

    finally
      FreeObject( lst ) ;
    end;

    Parent.GIS.Delete( param ) ;
  end ;

  Parent.GIS.InvalidateWholeMap ;
end ;
{$ENDREGION '_Pipeline_LayerClose'}

{$REGION 'T_Pipeline_LayerGet'}
procedure T_Pipeline_LayerGet.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_RESULT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_NAME,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  );
end;

procedure T_Pipeline_LayerGet.Execute ;
var
  param : String ;
  obj   : TObject;
  layer : TGIS_Layer ;
begin
  // Name or Layer
  param := ParamAsString( GIS_PIPELINE_PARAM_NAME ) ;
  obj := Parent.GetVar( param ) ;

  if obj is TGIS_Layer then
    param := TGIS_Layer( obj ).Name ;

  layer := TGIS_Layer( Parent.GIS.Get( param ) ) ;

  // Result
  param := ParamAsString( GIS_PIPELINE_PARAM_RESULT ) ;
  Parent.SetVar( param, layer ) ;
end ;
{$ENDREGION 'T_Pipeline_LayerGet'}

{$REGION 'T_Pipeline_LayerSave'}
procedure T_Pipeline_LayerSave.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_LAYER,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  );
end;

procedure T_Pipeline_LayerSave.Execute ;
var
  param : String ;
  obj   : TObject;
begin
  param := ParamAsString( GIS_PIPELINE_PARAM_LAYER ) ;
  obj := Parent.GetVar( param ) ;
  if obj is TGIS_Layer then
    TGIS_Layer( obj ).SaveAll ;

end ;
{$ENDREGION 'T_Pipeline_LayerSave'}

{$REGION 'T_Pipeline_LayerCreatePixelAbstract'}
procedure T_Pipeline_LayerCreatePixelAbstract.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_RESULT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_NAME,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_PATH,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    GIS_PIPELINE_DEFAULT_PATH,
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_CS,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    GIS_PIPELINE_DEFAULT_CS,
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_EXTENT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    GIS_PIPELINE_DEFAULT_EXTENT,
    ''
  );
  defineParam(
    PARAM_WIDTH,
    TGIS_PipelineParameterType.Int,
    64,
    80000,
    False,
    IntToStr( DEFAULT_WIDTH ),
    ''
  );
  defineParam(
    PARAM_HEIGHT,
    TGIS_PipelineParameterType.Int,
    64,
    80000,
    False,
    IntToStr( DEFAULT_HEIGHT ),
    ''
  );
  defineParam(
    PARAM_PIXELSIZE,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    False,
    DotFloatToStr( DEFAULT_PIXELSIZE ),
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_SAVE,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr(GIS_PIPELINE_DEFAULT_SAVE),
    ''
  ) ;
end;

procedure T_Pipeline_LayerCreatePixelAbstract.Execute ;
var
  param      : String ;
  cs         : TGIS_CSCoordinateSystem ;
  ext        : TGIS_Extent ;
  name       : String ;
  path       : String ;
  width      : Integer ;
  height     : Integer ;
  pixel_size : Double ;
  obj        : TObject;
  lp         : TGIS_LayerPixel ;
begin
  name := ParamAsString( GIS_PIPELINE_PARAM_NAME, '' ) ;
  path := ParamAsString( GIS_PIPELINE_PARAM_PATH, '') ;
  cs := ParamAsCS( GIS_PIPELINE_PARAM_CS, CSUnknownCoordinateSystem ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, GisNoWorld ) ;

  // Width
  param := ParamAsString( PARAM_WIDTH ) ;
  obj := Parent.GetVar( param ) ;
  if obj is TGIS_LayerPixel then
    width := TGIS_LayerPixel( obj ).BitWidth
  else
    width := ParamAsInt( PARAM_WIDTH, DEFAULT_WIDTH ) ;

  // Height
  param := ParamAsString( PARAM_HEIGHT ) ;
  obj := Parent.GetVar( param ) ;
  if obj is TGIS_LayerPixel then
    height := TGIS_LayerPixel( obj ).BitHeight
  else
    height := ParamAsInt( PARAM_HEIGHT, DEFAULT_HEIGHT ) ;

  // PixelSize
  pixel_size := ParamAsFloat( PARAM_PIXELSIZE, DEFAULT_PIXELSIZE ) ;

  if IsStringEmpty( path ) then
    lp := TGIS_LayerPixel.Create
  else
    lp := GisCreateLayer( name, path ) as TGIS_LayerPixel ;

  lp.Name := name ;
  if GisIsSameValue( pixel_size , 0.0, GIS_DOUBLE_RESOLUTION ) then
    lp.Build( path, isGrid, cs, ext, width, height, lp.DefaultSubFormat )
  else
    lp.Build( path, isGrid, cs, ext, pixel_size, lp.DefaultSubFormat ) ;

  lp.Params.Pixel.GridShadow := False ;

  if ParamAsBool( GIS_PIPELINE_PARAM_SAVE, GIS_PIPELINE_DEFAULT_SAVE ) then
    lp.SaveAll ;

  Parent.GIS.Add( lp );

  // Result
  param := ParamAsString( GIS_PIPELINE_PARAM_RESULT ) ;
  Parent.SetVar( param, lp ) ;
end;
{$ENDREGION 'T_Pipeline_LayerCreateGrid'}

{$REGION 'T_Pipeline_LayerCreateGrid'}
function T_Pipeline_LayerCreateGrid.isGrid : Boolean ;
begin
  Result := IS_GRID ;
end;
{$ENDREGION 'T_Pipeline_LayerCreateGrid'}

{$REGION 'T_Pipeline_LayerCreatePixel'}
function T_Pipeline_LayerCreatePixel.isGrid : Boolean ;
begin
  Result := IS_GRID ;
end;
{$ENDREGION 'T_Pipeline_LayerCreatePixel'}

{$REGION 'T_Pipeline_LayerCreateVector'}
procedure T_Pipeline_LayerCreateVector.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_RESULT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_PARAM_LAYER,
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_NAME,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_PATH,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    GIS_PIPELINE_DEFAULT_PATH,
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_CS,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    GIS_PIPELINE_DEFAULT_CS,
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_EXTENT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    GIS_PIPELINE_DEFAULT_EXTENT,
    ''
  ) ;
  defineParam(
    PARAM_SHAPE_TYPE,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    'Unknown',
    '!Unknown|Point|MultiPoint|Arc|Polygon'
  ) ;
  defineParam(
    PARAM_DIMENSION_TYPE,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    'XY',
    '!Unknown|XY|XYZ|XYM|XYZM'
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_SAVE,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr(GIS_PIPELINE_DEFAULT_SAVE),
    ''
  ) ;
end ;

procedure T_Pipeline_LayerCreateVector.Execute ;
var
  param          : String ;
  cs             : TGIS_CSCoordinateSystem ;
  ext            : TGIS_Extent ;
  name           : String ;
  path           : String ;
  lv             : TGIS_LayerVector ;
  shape_type     : TGIS_ShapeType ;
  dimension_type : TGIS_DimensionType ;
begin
  name := ParamAsString( GIS_PIPELINE_PARAM_NAME, '' ) ;
  path := ParamAsString( GIS_PIPELINE_PARAM_PATH, '' ) ;
  cs := ParamAsCS( GIS_PIPELINE_PARAM_CS, CSUnknownCoordinateSystem ) ;
  ext := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, GisNoWorld ) ;

  // ShapeType
  param := ParamAsString( PARAM_SHAPE_TYPE ) ;
  if CompareText( param, 'Unknown' ) = 0 then
    shape_type := TGIS_ShapeType.Unknown
  else if CompareText( param, 'Point' ) = 0 then
    shape_type := TGIS_ShapeType.Point
  else if CompareText( param, 'MultiPoint' ) = 0 then
    shape_type := TGIS_ShapeType.MultiPoint
  else if CompareText( param, 'Arc' ) = 0 then
    shape_type := TGIS_ShapeType.Arc
  else if CompareText( param, 'Polygon' ) = 0 then
    shape_type := TGIS_ShapeType.Polygon
  else
    shape_type := TGIS_ShapeType.Unknown ;

  // DimensionType
  param := ParamAsString( PARAM_DIMENSION_TYPE ) ;
  if CompareText( param, 'Unknown' ) = 0 then
    dimension_type := TGIS_DimensionType.Unknown
  else if CompareText( param, 'XY' ) = 0 then
    dimension_type := TGIS_DimensionType.XY
  else if CompareText( param, 'XYZ' ) = 0 then
    dimension_type := TGIS_DimensionType.XYZ
  else if CompareText( param, 'XYM' ) = 0 then
    dimension_type := TGIS_DimensionType.XYM
  else if CompareText( param, 'XYZM' ) = 0 then
    dimension_type := TGIS_DimensionType.XYZM
  else
    dimension_type := TGIS_DimensionType.Unknown ;

  if IsStringEmpty( path ) then
    lv := TGIS_LayerVector.Create
  else
    lv := GisCreateLayer( name, path ) as TGIS_LayerVector ;

  lv.Name := name ;
  lv.CS := cs ;
  lv.Build( path, ext, shape_type, dimension_type );

  if ParamAsBool( GIS_PIPELINE_PARAM_SAVE, GIS_PIPELINE_DEFAULT_SAVE ) then
    lv.SaveAll ;

  Parent.GIS.Add( lv );

  // Result
  param := ParamAsString( GIS_PIPELINE_PARAM_RESULT ) ;
  Parent.SetVar( param, lv ) ;
end;
{$ENDREGION 'T_Pipeline_LayerCreateVector'}

{$REGION 'T_Pipeline_LayerAddField'}
procedure T_Pipeline_LayerAddField.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_LAYER,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  );
  defineParam(
    GIS_PIPELINE_PARAM_NAME,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    DEFAULT_FIELD_NAME,
    ''
  );
  defineParam(
    PARAM_TYPE,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    'Number',
    '!String|Number|Float|Boolean|Date'
  );
  defineParam(
    PARAM_WIDTH,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    IntToStr( DEFAULT_FIELD_WIDTH ),
    ''
  );
  defineParam(
    PARAM_DECIMAL,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    IntToStr( DEFAULT_FIELD_DECIMAL ),
    ''
  );
end;

procedure T_Pipeline_LayerAddField.Execute ;
var
  param       : String ;
  lv          : TGIS_LayerVector ;
  fld_type    : TGIS_FieldType ;
  fld_name    : String ;
  fld_width   : Integer ;
  fld_decimal : Integer ;
begin
  fld_name := ParamAsString( GIS_PIPELINE_PARAM_NAME, DEFAULT_FIELD_NAME ) ;
  fld_width := ParamAsInt( PARAM_WIDTH, DEFAULT_FIELD_WIDTH ) ;
  fld_decimal := ParamAsInt( PARAM_DECIMAL, DEFAULT_FIELD_DECIMAL ) ;
  lv := ParamAsLayerVector( GIS_PIPELINE_PARAM_LAYER ) ;

  // Type
  param := ParamAsString( PARAM_TYPE ) ;
  if CompareText( param, 'String' ) = 0 then
    fld_type := TGIS_FieldType.String
  else if CompareText( param, 'Number' ) = 0 then
    fld_type := TGIS_FieldType.Number
  else if CompareText( param, 'Float' ) = 0 then
    fld_type := TGIS_FieldType.Float
  else if CompareText( param, 'Boolean' ) = 0 then
    fld_type := TGIS_FieldType.Boolean
  else if CompareText( param, 'Date' ) = 0 then
    fld_type := TGIS_FieldType.Date
  else
    fld_type := TGIS_FieldType.Number ;

  lv.AddField( fld_name, fld_type, fld_width, fld_decimal ) ;
end;
{$ENDREGION 'T_Pipeline_LayerAddField'}

{$REGION 'T_Pipeline_GridGenerateRamp'}
procedure T_Pipeline_GridGenerateRamp.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_LAYER,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_LAYER,
    ''
  );
  defineParam(
    PARAM_START_COLOR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_MID_COLOR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_END_COLOR,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_MIN_VAL,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_MID_VAL,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_MAX_VAL,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_USE_MIDDLE,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr( DEFAULT_USE_MIDDLE ),
    ''
  );

  defineParam(
    PARAM_RAMP_INTERVAL,
    TGIS_PipelineParameterType.Float,
    0,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_LEGEND_INTERVAL,
    TGIS_PipelineParameterType.Float,
    0,
    NaN,
    False,
    '',
    ''
  );
  defineParam(
    PARAM_COLOR_SPACE,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    DEFAULT_COLOR_SPACE,
    COLOR_SPACE_HSL + '|' + COLOR_SPACE_HSL360 + '|' + COLOR_SPACE_RGB
  );
end ;

procedure T_Pipeline_GridGenerateRamp.Execute ;
var
  slayer          : String ;
  param           : String ;
  obj             : TObject;
  dst             : TGIS_LayerPixel ;
  start_color     : TGIS_Color ;
  mid_color       : TGIS_Color ;
  end_color       : TGIS_Color ;
  color_space     : TGIS_ColorInterpolationMode ;
  min_val         : Single ;
  max_val         : Single ;
  mid_val         : Single ;
  ramp_interval   : Single ;
  legend_interval : Single ;
  use_middle      : Boolean ;
begin
  dst := nil ;
  Parent.GIS.BusyPrepare( self, Name );

  // layer
  slayer := ParamAsString( GIS_PIPELINE_PARAM_LAYER ) ;
  obj := Parent.GetVar( slayer ) ;

  if not assigned( obj ) then
    obj := Parent.GIS.Get( slayer ) ;

  if obj is TGIS_LayerPixel then
    dst := TGIS_LayerPixel( obj )
  else
    LogError( Format( 'Layer "%s" does not exists or is wrong type', [ slayer ] ) );

  // colors
  start_color := ParamAsColor( PARAM_START_COLOR, TGIS_Color.Blue ) ;
  mid_color := ParamAsColor( PARAM_MID_COLOR, TGIS_Color.Lime ) ;
  end_color := ParamAsColor( PARAM_END_COLOR, TGIS_Color.Red ) ;

  param := ParamAsString( PARAM_COLOR_SPACE ) ;
  if CompareText( param, COLOR_SPACE_RGB ) = 0 then
    color_space := TGIS_ColorInterpolationMode.RGB
  else if CompareText( param, COLOR_SPACE_HSL ) = 0 then
    color_space := TGIS_ColorInterpolationMode.HSL
  else if CompareText( param, COLOR_SPACE_HSL360 ) = 0 then
    color_space := TGIS_ColorInterpolationMode.HSL360
  else
    color_space := TGIS_ColorInterpolationMode.HSL ;

  // values
  min_val := ParamAsFloat( PARAM_MIN_VAL, dst.MinHeight ) ;
  max_val := ParamAsFloat( PARAM_MAX_VAL, dst.MaxHeight ) ;
  mid_val := ParamAsFloat( PARAM_MID_VAL, ( min_val + max_val ) / 2 ) ;

  use_middle := ParamAsBool( PARAM_USE_MIDDLE, DEFAULT_USE_MIDDLE ) ;

  // intervals
  ramp_interval := ParamAsFloat( PARAM_RAMP_INTERVAL, ( max_val - min_val ) / 100 ) ;
  legend_interval := ParamAsFloat( PARAM_LEGEND_INTERVAL, ( max_val - min_val ) / 10 ) ;

  dst.BusyEvent := Parent.GIS.AssignedBusyEvent;
  dst.GenerateRamp(
    start_color,
    mid_color,
    end_color,
    min_val,
    mid_val,
    max_val,
    use_middle,
    ramp_interval,
    legend_interval,
    nil,
    True,
    color_space
  ) ;

  Parent.GIS.BusyRelease( self );
end;
{$ENDREGION 'T_Pipeline_Kriging'}

{$IFDEF DCC}
  initialization
    Unit_GisPipelineOperations.SelfRegisterPipeline() ;
{$ENDIF}

{==================================== END =====================================}
end.
