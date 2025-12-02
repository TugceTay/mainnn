//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
  Slope Map analysis tool.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoSlopeMap ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoSlopeMap"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Runtime.InteropServices,
    TatukGIS.RTL,
    TatukGIS.NDK,
    TatukGIS.NDK.OpenCL;
{$ENDIF}
{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes ;
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
  GisSlopeMap = class
    public
      class procedure SelfRegisterPipeline ;
  end ;

  /// <summary>
  ///   Unit in which the slope is expressed.
  /// </summary>
  TGIS_SlopeUnit = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   The slope is expressed in degrees.
    /// </summary>
    Degree,
    /// <summary>
    ///   The slope is expressed in percents.
    /// </summary>
    Percent
  ) ;

  /// <summary>
  ///   Slope map generator for digital terrain models.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     This class is OpenCL-enabled, it can utilize OpenCL for increased
  ///     performance. Use TGIS_OpenCLEngine.Enabled to enable OpenCL
  ///     computation mode.
  ///   </note>
  /// </remarks>
  /// <exception cref="EGIS_Exception">
  ///  GIS_RS_ERR_BADPARAM
  /// </exception>
  TGIS_SlopeMap = {$IFDEF OXYGENE} public {$ENDIF}
                  class( TGIS_BaseObjectDisposable )
    private
      FSlopeUnit : TGIS_SlopeUnit ;
      FDem       : TGIS_LayerPixel ;
      FExtent    : TGIS_Extent ;
      FSLope     : TGIS_LayerPixel ;
      FApplyRamp : Boolean ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;

    private
      procedure doApplyRamp ;

      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Default constructor.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Generates slope map based on a terrain model.
      /// </summary>
      /// <param name="_dem">
      ///   terrain model as a grid layer; if the layer has no CS then it is
      ///   assumed that the extent is expressed in meters
      /// </param>
      /// <param name="_extent">
      ///   extent to be processed
      /// </param>
      /// <param name="_slope_map">
      ///   output slope map as a grid layer; this layer must have the same
      ///   extent, CS, and resolution as _dem
      /// </param>
      /// <param name="_ramp">
      ///   if True then the standard 0-5-10-15-21-31-90 degrees color ramp
      ///   is applied to the slope map
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ( const _dem       : TGIS_LayerPixel ;
                           const _extent    : TGIS_Extent ;
                           const _slope_map : TGIS_LayerPixel ;
                           const _ramp      : Boolean
                         ) ; overload ;
      /// <summary>
      ///   Generates slope map based on a terrain model.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ; overload ;

    public
      /// <summary>
      ///   Unit used to express slope values in the output grid.
      /// </summary>
      property SlopeUnit     : TGIS_SlopeUnit
                               read  FSlopeUnit
                               write FSlopeUnit ;
      /// <summary>
      ///   Terrain model as a grid layer; if the layer has no CS then it is
      ///   assumed that the extent is expressed in meters.
      /// </summary>
      property SourceLayer   : TGIS_LayerPixel
                               read  FDem
                               write FDem ;
      /// <summary>
      ///   Extent to be processed (in source layer units).
      /// </summary>
      property Extent        : TGIS_Extent
                               read  FExtent
                               write FExtent ;
      /// <summary>
      ///   Output slope map as a grid layer; this layer must have the same
      ///   extent, CS, and resolution as the source layer.
      /// </summary>
      property OutputLayer   : TGIS_LayerPixel
                               read  FSLope
                               write FSLope ;
      /// <summary>
      ///   If True then the standard 0-5-10-15-21-31-90 degrees color ramp
      ///   is applied to the slope map layer.
      /// </summary>
      property ApplyRamp     : Boolean
                               read  FApplyRamp
                               write FApplyRamp ;

    published // events
      /// <summary>
      ///   Event fired upon progress of the generation process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent    : TGIS_BusyEvent
                             add fadd_BusyEvent
                             remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent : TGIS_BusyEvent
                             read  fget_BusyEvent
                             write fset_BusyEvent ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,
    System.Types,

    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoOpenCL,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoPipeline,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoTypesUI ;
{$ENDIF}

{$REGION 'GIS_OCL_SLOPEMAP_SOURCE'}
{$IFNDEF JAVA}
const
  GIS_OCL_SLOPEMAP_SOURCE : String =
    '#pragma OPENCL EXTENSION cl_khr_fp64 : enable' + #13#10 +
    #13#10 +
    '/* KERNEL */' + #13#10 +
    '__kernel void slopemap(' + #13#10 +
    '  __global float * input,' + #13#10 +
    '  __global float * output,' + #13#10 +
    '  float noval_i,' + #13#10 +
    '  float noval_o,' + #13#10 +
    '  double dist_v,' + #13#10 +
    '  double dist_h,' + #13#10 +
    '  int bdeg' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  size_t w = get_global_size(0);' + #13#10 +
    '  size_t h = get_global_size(1);' + #13#10 +
    #13#10 +
    '  int ix = get_global_id(0);' + #13#10 +
    '  int iy = get_global_id(1);' + #13#10 +
    #13#10 +
    '  int i = iy * w + ix;' + #13#10 +
    '  float val = input[i];' + #13#10 +
    #13#10 +
    '  if (val == noval_i)' + #13#10 +
    '  {' + #13#10 +
    '    output[i] = noval_o;' + #13#10 +
    '    return;' + #13#10 +
    '  }' + #13#10 +
    '  float window[8];' + #13#10 +
    '  for (int k = 0; k < 8; k++)' + #13#10 +
    '    window[k] = val;' + #13#10 +
    '  int cnt = 0;' + #13#10 +
    '  for (int y = -1; y <= 1; y++)' + #13#10 +
    '  {' + #13#10 +
    '    if (iy + y < 0 || iy + y > h)' + #13#10 +
    '    {' + #13#10 +
    '      cnt += 3;' + #13#10 +
    '      continue;' + #13#10 +
    '    }' + #13#10 +
    '    for (int x = -1; x <= 1; x++)' + #13#10 +
    '    {' + #13#10 +
    '      if (y == 0 && x == 0)' + #13#10 +
    '        continue;' + #13#10 +
    '      if (ix + x < 0 || ix + x > w)' + #13#10 +
    '      {' + #13#10 +
    '        cnt++;' + #13#10 +
    '        continue;' + #13#10 +
    '      }' + #13#10 +
    '      val = input[i + y * w + x];' + #13#10 +
    '      if (val == noval_i)' + #13#10 +
    '        continue;' + #13#10 +
    '      window[cnt] = val;' + #13#10 +
    '      cnt++;' + #13#10 +
    '    }' + #13#10 +
    '  }' + #13#10 +
    '  float dzdx = ((window[2] + 2*window[4] + window[7]) -' + #13#10 +
    '                (window[0] + 2*window[3] + window[5])) / (8.0 * dist_h);' + #13#10 +
    '  float dzdy = ((window[5] + 2*window[6] + window[7]) -' + #13#10 +
    '                (window[0] + 2*window[1] + window[2])) / (8.0 * dist_v);' + #13#10 +
    '  float slope = atan(sqrt(dzdx*dzdx + dzdy*dzdy));' + #13#10 +
    '  slope = slope * 180.0 / M_PI;' + #13#10 +
    '  if(bdeg == 0)' + #13#10 +
    '    slope = slope * 100.0 / 45.0 ;' + #13#10 +
    #13#10 +
    '  output[i] = slope;' + #13#10 +
    '}' ;
  GIS_OCL_SLOPEMAP_KERNEL : String = 'slopemap' ;
{$ENDIF}
{$ENDREGION}

{$REGION 'T_Pipeline_SlopeMap'}
type
  T_Pipeline_SlopeMap = class( TGIS_PipelineOperationExtendedAbstract )
    const
      PARAM_APPLY_RAMP   = 'ApplyRamp' ;
      PARAM_SLOPE_UNIT   = 'SlopeUnit' ;
      DEFAULT_APPLY_RAMP = True ;
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

procedure T_Pipeline_SlopeMap.Initialize ;
begin
  inherited ;

  defineParam(
    GIS_PIPELINE_PARAM_EXTENT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  ) ;
  defineParam(
    PARAM_APPLY_RAMP,
    TGIS_PipelineParameterType.Boolean,
    NaN,
    NaN,
    False,
    BoolToStr( DEFAULT_APPLY_RAMP ),
    ''
  ) ;
  defineParam(
    PARAM_SLOPE_UNIT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    'Degree',
    '!Degree|Percent'
  ) ;
end ;

procedure T_Pipeline_SlopeMap.Execute;
var
  param      : String ;
  src        : TGIS_LayerPixel ;
  dst        : TGIS_LayerPixel ;
  slope_unit : TGIS_SlopeUnit ;
  slope      : TGIS_SlopeMap ;

begin
  src := ParamAsLayerPixel( GIS_PIPELINE_PARAM_SOURCE ) ;
  dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;

  // SlopeUnit
  param := ParamAsString( PARAM_SLOPE_UNIT ) ;
  if CompareText( param, 'Degree' ) = 0 then
    slope_unit := TGIS_SlopeUnit.Degree
  else if CompareText( param, 'Percent' ) = 0 then
    slope_unit := TGIS_SlopeUnit.Percent
  else
    slope_unit := TGIS_SlopeUnit.Degree ;

  // execute contour generator
 slope := TGIS_SlopeMap.Create ;
  try
    slope.SourceLayer := src ;
    slope.OutputLayer := dst ;
    slope.Extent := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src.Extent ) ;
    slope.ApplyRamp := ParamAsBool( PARAM_APPLY_RAMP, DEFAULT_APPLY_RAMP ) ;
    slope.SlopeUnit := slope_unit ;
    slope.busyEventManager.UseProgressThreshold := False ;
    {$IFDEF CLR}
      slope.BusyEvent += DoBusyEvent ;
    {$ELSE}
      slope.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
    {$ENDIF}
    slope.Generate ;
  finally
    FreeObject( slope ) ;
  end ;

  inherited ;
end ;

class procedure GisSlopeMap.SelfRegisterPipeline ;
begin
  RegisterPipeline( 'SlopeMap', T_Pipeline_SlopeMap ) ;
end ;
{$ENDREGION}

{$REGION 'TSlopeMap'}
type
  TSlopeMap = {$IFDEF OXYGENE} abstract {$ENDIF} class
  {$IFDEF OXYGENE} assembly or {$ENDIF} protected
    FDem       : TGIS_LayerPixel ;
    FSlope     : TGIS_LayerPixel ;
    FExtent    : TGIS_Extent ;
    FSlopeUnit : TGIS_SlopeUnit ;
    FBusy      : TGIS_BusyEventManager;
    FMin       : Single ;
    FMax       : Single ;

  private
    function runSlopeAlgorithm( const _srcLock   : TGIS_LayerPixelLock ;
                                const _dstLock   : TGIS_LayerPixelLock ;
                                const _srcNoData : Single ;
                                const _dstNoData : Single ;
                                const _pxSizeY   : Double ;
                                const _pxSizeX   : Double ;
                                const _useDegree : Boolean
                              ) : TArray<Single> ; virtual ; abstract ;
  public
    constructor Create ;
    procedure Generate( const _sourceLayer      : TGIS_LayerPixel ;
                        const _outputLayer      : TGIS_LayerPixel ;
                        const _extent           : TGIS_Extent ;
                        const _slopeUnit        : TGIS_SlopeUnit ;
                        const _busyEventManager : TGIS_BusyEventManager
                      ) ;
  end;

  TSlopeMapCPU = class(TSlopeMap)
  private
    function runSlopeAlgorithm( const _srcLock   : TGIS_LayerPixelLock ;
                                const _dstLock   : TGIS_LayerPixelLock ;
                                const _srcNoData : Single ;
                                const _dstNoData : Single ;
                                const _pxSizeY   : Double ;
                                const _pxSizeX   : Double ;
                                const _useDegree : Boolean
                              ) : TArray<Single> ; override ;
  end ;
{$ENDREGION}

{$IFNDEF JAVA OR ISLAND}
  TSlopeMapOCL = class(TSlopeMap)
  private
    function runSlopeAlgorithm( const _srcLock   : TGIS_LayerPixelLock ;
                                const _dstLock   : TGIS_LayerPixelLock ;
                                const _srcNoData : Single ;
                                const _dstNoData : Single ;
                                const _pxSizeY   : Double ;
                                const _pxSizeX   : Double ;
                                const _useDegree : Boolean
                              ) : TArray<Single> ; override ;
  end;
{$ENDIF}

{$REGION 'TSlopeMap'}
constructor TSlopeMap.Create;
begin
  inherited ;
  FMin := GIS_MAX_SINGLE ;
  FMax := -GIS_MAX_SINGLE ;
end;

procedure TSlopeMap.Generate(
  const _sourceLayer : TGIS_LayerPixel ;
  const _outputLayer : TGIS_LayerPixel ;
  const _extent      : TGIS_Extent ;
  const _slopeUnit   : TGIS_SlopeUnit ;
  const _busyEventManager : TGIS_BusyEventManager
) ;
const
  LOCAL_ARR_DEGREES : array[0..6] of String =
    ( '0', '5', '10', '15', '21', '31', '90' ) ;
  LOCAL_ARR_PERCENTS : array[0..6] of String =
    ( '0', '9', '18', '27', '38', '60', '1000' ) ;
  LOCAL_ARR_BGR : array[0..5] of Cardinal =
    ( 32768, 47360, 62720, 61695, 41471, 255 ) ;
  LOCAL_CHAR_DEGREE  : Char   = #176 ;
  LOCAL_CHAR_PERCENT : Char   = '%' ;
  LOCAL_PARAM_SRC    : String = '_dem' ;
  LOCAL_PARAM_EXT    : String = '_extent' ;
  LOCAL_PARAM_DST    : String = '_slope_map' ;
var
  ext           : TGIS_Extent ;
  lock_dem      : TGIS_LayerPixelLock ;
  lock_slope    : TGIS_LayerPixelLock ;
  pt            : TPoint ;
  ptg_0         : TGIS_Point ;
  ptg_1         : TGIS_Point ;
  px_size_y     : Double ;
  px_size_x     : Double ;
  slope_min_max : TArray<Single> ;
begin
  FDem := _sourceLayer ;
  FSlope := _outputLayer ;
  FExtent := _extent ;
  FSlopeUnit := _slopeUnit ;
  FBusy := _busyEventManager ; ;

  if not assigned( FDem ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SRC, 1
    ) ;

  if not FDem.IsGrid then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SRC, 2
    ) ;

  if not assigned( FSlope ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 1
    ) ;

  if not FSlope.IsGrid then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 2
    ) ;

  ext := FDem.CS.ExtentToCS( FSlope.CS, FDem.Extent ) ;
  ext := GisCommonExtent( ext, FSlope.Extent ) ;

  if GisIsEmptyExtent( ext ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 3
    ) ;

  if ( FDem.BitWidth  <> FSlope.BitWidth  ) or
     ( FDem.BitHeight <> FSlope.BitHeight ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 4
    ) ;

  ext := GisCommonExtent(
    ext, FDem.CS.ExtentToCS( FSlope.CS, FExtent )
  ) ;

  if GisIsEmptyExtent( ext ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_EXT, 1
    ) ;

  FDem.Open ;
  FSlope.Open ;

  lock_dem := FDem.LockPixels( ext, FSlope.CS, False ) ;
  lock_slope := FSlope.LockPixels( ext, FSlope.CS, True ) ;
  try
    // for Geographic CS we asssume that elevation is in meters!
    if FDem.CS is TGIS_CSGeographicCoordinateSystem then begin
      // center pixel and coordinates
      ptg_0 := GisPoint( ( ext.XMin + ext.XMax ) / 2.0,
                      ( ext.YMin + ext.YMax ) / 2.0 ) ;
      pt := lock_dem.MapToRaster( ptg_0, FSlope.CS ) ;
      ptg_0 := lock_dem.RasterToMap( pt, FSlope.CS ) ;

      // horizontal pixel size
      ptg_1 := lock_dem.RasterToMap( Point( pt.X + 1, pt.Y ), FSlope.CS ) ;
      px_size_x := FSlope.CS.Distance( ptg_0, ptg_1 ) ;

      // vertical pixel size
      ptg_1 := lock_dem.RasterToMap( Point( pt.X, pt.Y+1 ), FSlope.CS ) ;
      px_size_y := FSlope.CS.Distance( ptg_0, ptg_1 ) ;

    end
    // otherwise, elevation is in the same unit as XY
    else begin
      px_size_x := ( FDem.Extent.XMax - FDem.Extent.XMin ) / FDem.BitWidth ;
      px_size_y := ( FDem.Extent.YMax - FDem.Extent.YMin ) / FDem.BitHeight ;
    end ;

    slope_min_max := runSlopeAlgorithm(
      lock_dem, lock_slope,
      FDem.NoDataValue, FSlope.NoDataValue,
      px_size_y, px_size_x,
      FSlopeUnit = TGIS_SlopeUnit.Degree
    ) ;  // change xy order

    FSlope.MinHeight := slope_min_max[0] ;
    FSlope.MaxHeight := slope_min_max[1] ;
  finally
    FDem.UnlockPixels( lock_dem ) ;
    FSlope.UnlockPixels( lock_slope ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'TSlopeMapCPU'}
function TSlopeMapCPU.runSlopeAlgorithm(
  const _srcLock   : TGIS_LayerPixelLock ;
  const _dstLock   : TGIS_LayerPixelLock ;
  const _srcNoData : Single ;
  const _dstNoData : Single ;
  const _pxSizeY   : Double ;
  const _pxSizeX   : Double ;
  const _useDegree : Boolean
) : TArray<Single> ;
var
  src_val    : Double ;
  window     : TGIS_DoubleArray ;
  w          : Integer ;
  h          : Integer ;
  busy_total : Integer ;

  procedure proc_pixel( const _w : Integer ; const _h : Integer ) ;
  var
    slope_val : Double ;
    dzdx      : Double ;
    dzdy      : Double ;
    ww        : Integer ;
    hh        : Integer ;
    ii        : Integer ;

  begin;
    src_val := _srcLock.Grid[_h, _w] ;
    if SameValue(src_val, _srcNoData) then
      exit ;

    for ii := 0 to 7 do
      window[ii] := src_val ;

    ii := 0 ;
    for hh := -1 to 1 do begin
      for ww := -1 to 1 do begin
        if ( hh = 0 ) and ( ww = 0 ) then
          continue ;

        src_val := _srcLock.Grid[_h + hh, _w + ww] ;
        if SameValue(src_val, _srcNoData) then
          continue ;

        window[ii] := src_val ;
        inc( ii ) ;
      end ;
    end ;

    dzdx := ( ( window[2] + 2*window[4] + window[7] ) -
              ( window[0] + 2*window[3] + window[5] ) ) / ( 8.0 * _pxSizeX ) ;
    dzdy := ( ( window[5] + 2*window[6] + window[7] ) -
              ( window[0] + 2*window[1] + window[2] ) ) / ( 8.0 * _pxSizeY ) ;
    slope_val := ArcTan( Sqrt( dzdx*dzdx + dzdy*dzdy ) ) ;

    slope_val := RadToDeg( slope_val ) ;
    if not _useDegree then
      slope_val := 100.0*slope_val/45.0 ;

    _dstLock.Grid[_h, _w] := slope_val ;

    FMin := Min( slope_val, FMin ) ;
    FMax := Max( slope_val, FMax ) ;
  end ;

begin
  SetLength( Result, 2 ) ;

  busy_total := ( _srcLock.Bounds.Height + 1 ) * ( _srcLock.Bounds.Width + 1 ) ;
  FBusy.StartEvent(
    _rsrc( GIS_RS_BUSY_EXPORT ), busy_total
  ) ;
  try
    SetLength( window, 8 ) ;

    // everything except borders
    for h := _srcLock.Bounds.Top + 1 to _srcLock.Bounds.Bottom - 1 do begin
      for w := _srcLock.Bounds.Left + 1 to _srcLock.Bounds.Right - 1 do begin
        proc_pixel( w, h ) ;
        if FBusy.PushEvent then
          exit ;
      end ;
    end ;

    // top border except corners
    h := _srcLock.Bounds.Top ;
    for w := _srcLock.Bounds.Left + 1 to _srcLock.Bounds.Right - 1 do begin
      _dstLock.Grid[h, w] := _dstNoData;
      if FBusy.PushEvent then
        exit ;
    end ;

    // bottom border except corners
    h := _srcLock.Bounds.Bottom ;
    for w := _srcLock.Bounds.Left + 1 to _srcLock.Bounds.Right - 1 do begin
      _dstLock.Grid[h, w] := _dstNoData;
      if FBusy.PushEvent then
        exit ;
    end ;

    // left border except corners
    w := _srcLock.Bounds.Left ;
    for h := _srcLock.Bounds.Top + 1 to _srcLock.Bounds.Bottom - 1 do begin
      _dstLock.Grid[h, w] := _dstNoData;
      if FBusy.PushEvent then
        exit ;
    end ;

    // right border except corners
    w := _srcLock.Bounds.Right ;
    for h := _srcLock.Bounds.Top + 1 to _srcLock.Bounds.Bottom - 1 do begin
      _dstLock.Grid[h, w] := _dstNoData;
      if FBusy.PushEvent then
        exit ;
    end ;

    //top-left corner
    _dstLock.Grid[_srcLock.Bounds.Top, _srcLock.Bounds.Left] := _dstNoData;
    FBusy.PushEvent ;

    //top-right corner
    _dstLock.Grid[_srcLock.Bounds.Top, _srcLock.Bounds.Right] := _dstNoData;
    FBusy.PushEvent ;

    //bottom-left corner
    _dstLock.Grid[_srcLock.Bounds.Bottom, _srcLock.Bounds.Left] := _dstNoData;
    FBusy.PushEvent ;

    //bottom-right corner
    _dstLock.Grid[_srcLock.Bounds.Bottom, _srcLock.Bounds.Right] := _dstNoData;
    FBusy.PushEvent ;
  finally
    FBusy.EndEvent ;
    Result[0] := FMin ;
    Result[1] := FMax ;
  end ;
end;
{$ENDREGION}

{$REGION 'TSlopeMapOCL'}

{$IFNDEF JAVA OR ISLAND}
function TSlopeMapOCL.runSlopeAlgorithm(
  const _srcLock   : TGIS_LayerPixelLock ;
  const _dstLock   : TGIS_LayerPixelLock ;
  const _srcNoData : Single ;
  const _dstNoData : Single ;
  const _pxSizeY   : Double ;
  const _pxSizeX   : Double ;
  const _useDegree : Boolean
) : TArray<Single> ;
var
  ocl       : TGIS_OpenCLProgram ;
  ibuf      : array of Single ;
  obuf      : array of Single ;
  xsiz      : Integer ;
  ysiz      : Integer ;
  siz       : Integer ;
  top       : Integer ;
  bottom    : Integer ;
  left      : Integer ;
  right     : Integer ;
  ideg      : Integer ;
  i         : Integer ;
  w         : Integer ;
  h         : Integer ;
  slope_val : Single ;

begin
  SetLength( Result, 2 ) ;

  xsiz := _srcLock.Bounds.Width + 1 ;
  ysiz := _srcLock.Bounds.Height + 1 ;
  siz := xsiz * ysiz * sizeOf( Single ) ;
  SetLength( ibuf, xsiz*ysiz ) ;

  i := 0 ;
  for h := _srcLock.Bounds.Top to _srcLock.Bounds.Bottom do begin
    for w := _srcLock.Bounds.Left to _srcLock.Bounds.Right do begin
      ibuf[i] := _srcLock.Grid[h,w] ;
      inc( i ) ;
    end ;
  end ;

  ocl := TGIS_OpenCLProgram.Create ;
  try
    if not ocl.LoadFromString( GIS_OCL_SLOPEMAP_SOURCE,
              GIS_OCL_SLOPEMAP_KERNEL ) then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_OPENCL_LOAD ), '', ocl.ErrorCode
            ) ;
    if FSlopeUnit = TGIS_SlopeUnit.Degree then
      ideg := 1
    else
      ideg := 0 ;

    ocl.WorkDimension := 2 ;
    ocl.GlobalWorkSizes[0] := xsiz ;
    ocl.GlobalWorkSizes[1] := ysiz ;

    {$IFDEF DCC}
      ocl.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], siz, @ibuf[0] ) ;
      ocl.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], siz, nil ) ;
      ocl.SetArgument( 2, sizeOf( Single ), @_srcNoData ) ;
      ocl.SetArgument( 3, sizeOf( Single ), @_dstNoData ) ;
      ocl.SetArgument( 4, sizeOf( Double ), @_pxSizeY ) ;
      ocl.SetArgument( 5, sizeOf( Double ), @_pxSizeX ) ;
      ocl.SetArgument( 6, sizeOf( Integer ), @ideg ) ;
    {$ENDIF}
    {$IFDEF CLR}
      ocl.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], siz, ibuf ) ;
      ocl.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], siz, nil ) ;
      ocl.SetArgument( 2, sizeOf( Single ), _srcNoData ) ;
      ocl.SetArgument( 3, sizeOf( Single ), _dstNoData ) ;
      ocl.SetArgument( 4, sizeOf( Double ), _pxSizeY ) ;
      ocl.SetArgument( 5, sizeOf( Double ), _pxSizeX ) ;
      ocl.SetArgument( 6, sizeOf( Integer ), ideg ) ;
    {$ENDIF}

    if not ocl.Execute then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_OPENCL_EXECUTION ), '', ocl.ErrorCode
            ) ;

    SetLength( obuf, xsiz*ysiz ) ;
    {$IFDEF DCC}
      ocl.ReadBuffer( 1, siz, @obuf[0] ) ;
    {$ENDIF}
    {$IFDEF CLR}
      ocl.ReadBuffer( 1, siz, obuf ) ;
    {$ENDIF}
  finally
    FreeObject( ocl ) ;
  end ;

  top := _dstLock.Bounds.Top;
  bottom := _dstLock.Bounds.Bottom;
  left := _dstLock.Bounds.Left;
  right := _dstLock.Bounds.Right;

  i := -1 ;
  for h := top to bottom do begin
    for w := left to right do begin

      inc( i ) ;
      slope_val := obuf[i] ;

      // nodata on the edges
      if (h = top) or (h = bottom) or (w = left) or (w = right) then begin
        _dstLock.Grid[h, w] := _dstNoData ;
        continue;
      end
      else
        _dstLock.Grid[h, w] := slope_val ;

      if SameValue(slope_val, _dstNoData) then
        continue ;

      FMin := Min( slope_val, FMin ) ;
      FMax := Max( slope_val, FMax ) ;
    end ;
  end ;

  Result[0] := FMin ;
  Result[1] := FMax ;
end ;
{$ENDIF}
{$ENDREGION}

{$REGION 'TGIS_SlopeMap'}
constructor TGIS_SlopeMap.Create ;
begin
  inherited ;

  FSlopeUnit := TGIS_SlopeUnit.Degree ;
  FApplyRamp := True ;
  busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
end ;

procedure TGIS_SlopeMap.doDestroy ;
begin
  FreeObject( busyEventManager ) ;
  inherited ;
end ;

{$IFDEF CLR}
  procedure TGIS_SlopeMap.fadd_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent += _value ;
  end ;

  procedure TGIS_SlopeMap.fremove_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent -= _value ;
  end ;
{$ELSE}
  procedure TGIS_SlopeMap.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent := _value ;
  end ;

  function TGIS_SlopeMap.fget_BusyEvent : TGIS_BusyEvent ;
  begin
    Result := busyEventManager.BusyEvent ;
  end ;
{$ENDIF}

procedure TGIS_SlopeMap.doApplyRamp ;
const
  LOCAL_ARR_DEGREES : array[0..6] of String =
    ( '0', '5', '10', '15', '21', '31', '90' ) ;
  LOCAL_ARR_PERCENTS : array[0..6] of String =
    ( '0', '9', '18', '27', '38', '60', '1000' ) ;
  LOCAL_ARR_BGR : array[0..5] of Cardinal =
    ( 32768, 47360, 62720, 61695, 41471, 255 ) ;
  LOCAL_CHAR_DEGREE : Char = #176 ;
  LOCAL_CHAR_PERCENT : Char = '%' ;
var
  lgnd   : String ;
  i      : Integer ;
begin
  FSLope.Params.Pixel.GridShadow := False ;

  if FSlopeUnit = TGIS_SlopeUnit.Degree then begin
    for i := 0 to 5 do begin
      if i = 0 then
        lgnd := '< ' + LOCAL_ARR_DEGREES[i+1] + LOCAL_CHAR_DEGREE
      else
      if i = 5 then
        lgnd := '> ' + LOCAL_ARR_DEGREES[i] + LOCAL_CHAR_DEGREE
      else
        lgnd := LOCAL_ARR_DEGREES[i] + LOCAL_CHAR_DEGREE + ' - ' +
                LOCAL_ARR_DEGREES[i+1] + LOCAL_CHAR_DEGREE ;

      FSLope.Params.Pixel.AltitudeMapZones.Add(
        LOCAL_ARR_DEGREES[i] + ',' + LOCAL_ARR_DEGREES[i+1] + ',' +
        ConstructParamColor( TGIS_Color.FromBGR( LOCAL_ARR_BGR[i] ) ) +
        ',' + lgnd
      ) ;
    end ;
  end
  else begin
    for i := 0 to 5 do begin
      if i = 0 then
        lgnd := '< ' + LOCAL_ARR_PERCENTS[i+1] + LOCAL_CHAR_PERCENT
      else
      if i = 5 then
        lgnd := '> ' + LOCAL_ARR_PERCENTS[i] + LOCAL_CHAR_PERCENT
      else
        lgnd := LOCAL_ARR_PERCENTS[i] + LOCAL_CHAR_PERCENT + ' - ' +
                LOCAL_ARR_PERCENTS[i+1] + LOCAL_CHAR_PERCENT ;

      FSLope.Params.Pixel.AltitudeMapZones.Add(
        LOCAL_ARR_PERCENTS[i] + ',' + LOCAL_ARR_PERCENTS[i+1] + ',' +
        ConstructParamColor( TGIS_Color.FromBGR( LOCAL_ARR_BGR[i] ) ) +
        ',' + lgnd
      ) ;
    end ;
  end ;
end ;

procedure TGIS_SlopeMap.Generate(
  const _dem       : TGIS_LayerPixel ;
  const _extent    : TGIS_Extent ;
  const _slope_map : TGIS_LayerPixel ;
  const _ramp      : Boolean
) ;
begin
  FDem := _dem ;
  FExtent := _extent ;
  FSLope := _slope_map ;
  FApplyRamp := _ramp ;

  Generate ;
end ;

procedure TGIS_SlopeMap.Generate ;
var
  slopeAlgorithm : TSlopeMap;
begin
  {$IFDEF JAVA OR ISLAND}
    slopeAlgorithm := TSlopeMapCPU.Create ;
  {$ELSE}
    if GisOpenCLEngine.Available and GisOpenCLEngine.Enabled then
      slopeAlgorithm := TSlopeMapOCL.Create
    else
      slopeAlgorithm := TSlopeMapCPU.Create ;
  {$ENDIF}
  try
    slopeAlgorithm.Generate( FDem, FSLope, FExtent, FSlopeUnit, busyEventManager ) ;
  finally
    FreeObject( slopeAlgorithm ) ;
  end;

if FApplyRamp then
  doApplyRamp ;

end ;
{$ENDREGION}

{$IFDEF DCC}
initialization
  GisSlopeMap.SelfRegisterPipeline ;
{$ENDIF}
end.
