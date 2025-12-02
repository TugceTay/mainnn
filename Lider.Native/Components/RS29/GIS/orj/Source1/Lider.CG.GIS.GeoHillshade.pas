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
  Hillshade analysis tool.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoHillshade ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoHillshade"'}
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
  GisHillshade = class
    public
      class procedure SelfRegisterPipeline ;
  end ;

  /// <summary>
  ///   Hillshade generator for digital terrain models.
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
  TGIS_Hillshade = {$IFDEF OXYGENE} public {$ENDIF}
                  class( TGIS_BaseObjectDisposable )
    private
      FSourceLayer  : TGIS_LayerPixel ;
      FExtent       : TGIS_Extent ;
      FOutputLayer  : TGIS_LayerPixel ;
      FZFactor      : Double ;
      FAzimuth      : Double ;
      FAltitude     : Double ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;

    private
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

      procedure generateCPU ;
      {$IFNDEF JAVA OR ISLAND}
        procedure generateOCL ;
      {$ENDIF}

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Generates hillshade based on a terrain model.
      /// </summary>
      /// <param name="_dem">
      ///   terrain model as a grid layer; if the layer has no CS then it is
      ///   assumed that the extent is expressed in meters
      /// </param>
      /// <param name="_extent">
      ///   extent to be processed
      /// </param>
      /// <param name="_hillshade">
      ///   output hillshade as a grid layer; this layer must have the same
      ///   extent, CS, and resolution as _dem
      /// </param>
      /// <param name="_zfactor">
      ///   factor used to amplify or weaken the hillshade effect
      /// </param>
      /// <param name="_azimuth">
      ///   direction of the light source in degrees
      /// </param>
      /// <param name="_altitude">
      ///   angular altitude of the light source in degrees
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ( const _dem       : TGIS_LayerPixel ;
                           const _extent    : TGIS_Extent ;
                           const _hillshade : TGIS_LayerPixel ;
                           const _zfactor   : Double ;
                           const _azimuth   : Double ;
                           const _altitude  : Double
                         ) ; overload ;
      /// <summary>
      ///   Generates hillshade based on a terrain model.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ; overload ;

    public
      /// <summary>
      ///   Terrain model as a grid layer; if the layer has no CS then it is
      ///   assumed that the extent is expressed in meters.
      /// </summary>
      property SourceLayer   : TGIS_LayerPixel
                               read  FSourceLayer
                               write FSourceLayer ;
      /// <summary>
      ///   Extent to be processed (in source layer units).
      /// </summary>
      property Extent        : TGIS_Extent
                               read  FExtent
                               write FExtent ;
      /// <summary>
      ///   Output hillshade as a grid layer; this layer must have the same
      ///   extent, CS, and resolution as the source layer.
      /// </summary>
      property OutputLayer   : TGIS_LayerPixel
                               read  FOutputLayer
                               write FOutputLayer ;
      /// <summary>
      ///   Factor used to amplify (value &gt; 1) or weaken (0 &lt; value
      ///   &lt; 1) the hillshade effect; default value is 1.
      /// </summary>
      property ZFactor       : Double
                               read  FZFactor
                               write FZFactor ;
      /// <summary>
      ///   Direction of the light source in degrees; default value is 315
      ///   degrees (northwest).
      /// </summary>
      property Azimuth       : Double
                               read  FAzimuth
                               write FAzimuth ;
      /// <summary>
      ///   Angular altitude of the light source in degrees; default value is
      ///   45 degrees.
      /// </summary>
      property Altitude      : Double
                               read  FAltitude
                               write FAltitude ;

    published // events
      /// <summary>
      ///   Event fired upon progress of the generation process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent      : TGIS_BusyEvent
                               add fadd_BusyEvent
                               remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent   : TGIS_BusyEvent
                               read  fget_BusyEvent
                               write fset_BusyEvent ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Types,
    System.Math,
    System.SysUtils,

    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoPipeline,
    Lider.CG.GIS.GeoOpenCL ;
{$ENDIF}

{$REGION 'GIS_OCL_HILLSHADE_SOURCE'}
{$IFNDEF JAVA}
const
  GIS_OCL_HILLSHADE_SOURCE : String =
    '#pragma OPENCL EXTENSION cl_khr_fp64 : enable' + #13#10 +
    #13#10 +
    '/* KERNEL */' + #13#10 +
    '__kernel void hillshade(' + #13#10 +
    '  __global float * input,' + #13#10 +
    '  __global float * output,' + #13#10 +
    '  float noval_i,' + #13#10 +
    '  float noval_o,' + #13#10 +
    '  double dist_v,' + #13#10 +
    '  double dist_h,' + #13#10 +
    '  double zfctr,' + #13#10 +
    '  double azmth,' + #13#10 +
    '  double alt' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  size_t w = get_global_size(0);' + #13#10 +
    '  size_t h = get_global_size(1);' + #13#10 +
    #13#10 +
    '  int ix = get_global_id(0);' + #13#10 +
    '  int iy = get_global_id(1);' + #13#10 +
    #13#10 +
    '  int i = iy*w+ix;' + #13#10 +
    '  float val = input[i];' + #13#10 +
    '  ' + #13#10 +
    '  if (val==noval_i)' + #13#10 +
    '  {' + #13#10 +
    '    output[i] = noval_o;' + #13#10 +
    '    return ;' + #13#10 +
    '  }' + #13#10 +
    ' ' + #13#10 +
    '  float dzdx, dzdy, slope, aspct, hlshd ;' + #13#10 +
    '  ' + #13#10 +
    '  if((ix>0)&&(ix<w-1)&&(iy>0)&&(iy<h-1))' + #13#10 +
    '  {' + #13#10 +
    '    dzdx = ( ( input[i-w+1] + 2*input[i+1] + input[i+w+1] ) -' + #13#10 +
    '             ( input[i-w-1] + 2*input[i-1] + input[i+w-1] ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '    dzdy = ( ( input[i+w-1] + 2*input[i+w] + input[i+w+1] ) -' + #13#10 +
    '             ( input[i-w-1] + 2*input[i-w] + input[i-w+1] ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  {' + #13#10 +
    '    if(ix==0)' + #13#10 +
    '    {' + #13#10 +
    '      if(iy==0)' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( val + 2*input[i+1] + input[i+w+1] ) - ( 4*val ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( val + 2*input[i+w] + input[i+w+1] ) - ( 4*val ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '      else' + #13#10 +
    '      if(iy==h-1)' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( input[i-w+1] + 2*input[i+1] + val ) - ( 4*val ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( 4*val ) - ( val + 2*input[i-w] + input[i-w+1] ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '      else' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( input[i-w+1] + 2*input[i+1] + input[i+w+1] ) - ( 4*val ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( val + 2*input[i+w] + input[i+w+1] ) -' + #13#10 +
    '                 ( val + 2*input[i-w] + input[i-w+1] ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '    }' + #13#10 +
    '    else ' + #13#10 +
    '    if(ix==w-1)' + #13#10 +
    '    {' + #13#10 +
    '      if(iy==0)' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( 4*val ) - ( val + 2*input[i-1] + input[i+w-1] ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( input[i+w-1] + 2*input[i+w] + val ) - ( 4*val ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '      else' + #13#10 +
    '      if(iy==h-1)' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( 4*val ) - ( input[i-w-1] + 2*input[i-1] + val ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( 4*val ) - ( input[i-w-1] + 2*input[i-w] + val ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '      else' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( 4*val ) - ( input[i-w-1] + 2*input[i-1] + input[i+w-1] ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( input[i+w-1] + 2*input[i+w] + val ) -' + #13#10 +
    '                 ( input[i-w-1] + 2*input[i-w] + val ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '    }' + #13#10 +
    '    else' + #13#10 +
    '    {' + #13#10 +
    '      if(iy==0)' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( val + 2*input[i+1] + input[i+w+1] ) -' + #13#10 +
    '                 ( val + 2*input[i-1] + input[i+w-1] ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( input[i+w-1] + 2*input[i+w] + input[i+w+1] ) - ( 4*val ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }' + #13#10 +
    '      else if(iy==h-1)' + #13#10 +
    '      {' + #13#10 +
    '        dzdx = ( ( input[i-w+1] + 2*input[i+1] + val ) -' + #13#10 +
    '                 ( input[i-w-1] + 2*input[i-1] + val ) ) / ( 8.0*dist_h ) ;' + #13#10 +
    '        dzdy = ( ( 4*val ) - ( input[i-w-1] + 2*input[i-w] + input[i-w+1] ) ) / ( 8.0*dist_v ) ;' + #13#10 +
    '      }    ' + #13#10 +
    '    }' + #13#10 +
    '  }' + #13#10 +
    '  ' + #13#10 +
    '  slope = atan( zfctr*sqrt( dzdx*dzdx + dzdy*dzdy ) ) ;' + #13#10 +
    '  aspct = atan2( dzdy, -dzdx ) ;' + #13#10 +
    '  ' + #13#10 +
    '  hlshd = 255.0 * ( ( cos( alt ) * cos( slope ) ) + ' + #13#10 +
    '          ( sin( alt ) * sin( slope ) * cos( azmth - aspct ) ) ) ;' + #13#10 +
    '  if(hlshd<0.0)' + #13#10 +
    '  {' + #13#10 +
    '    hlshd = 0.0 ;' + #13#10 +
    '  }' + #13#10 +
    #13#10 +
    '  output[i] = hlshd ;' + #13#10 +
    '}' ;
  GIS_OCL_HILLSHADE_KERNEL : String = 'hillshade' ;
{$ENDIF}
{$ENDREGION}

//==============================================================================
// T_Pipeline_Hillshade
//==============================================================================
type
  T_Pipeline_Hillshade = class( TGIS_PipelineOperationExtendedAbstract )
    const
      PARAM_ZFACTOR  = 'ZFactor' ;
      PARAM_AZIMUTH  = 'Azimuth' ;
      PARAM_ALTITUDE = 'Altitude' ;
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  class procedure GisHillshade.SelfRegisterPipeline ;
  begin
    RegisterPipeline( 'Hillshade', T_Pipeline_Hillshade ) ;
  end ;

  procedure T_Pipeline_Hillshade.Initialize ;
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
      PARAM_ZFACTOR,
      TGIS_PipelineParameterType.Float,
      0,
      NaN,
      False,
      '1.0',
      ''
    ) ;
    defineParam(
      PARAM_AZIMUTH,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      '315.0',
      ''
    ) ;
    defineParam(
      PARAM_ALTITUDE,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      '45.0',
      ''
    ) ;
  end ;

  procedure T_Pipeline_Hillshade.Execute;
  var
    src        : TGIS_LayerPixel ;
    dst        : TGIS_LayerPixel ;
    hillshade  : TGIS_Hillshade ;
  begin
    src := ParamAsLayerPixel( GIS_PIPELINE_PARAM_SOURCE ) ;
    dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;

    // execute contour generator
    hillshade := TGIS_Hillshade.Create ;
    try
      hillshade.SourceLayer := src ;
      hillshade.OutputLayer := dst ;
      hillshade.ZFactor := ParamAsFloat( PARAM_ZFACTOR, 1.0 ) ;
      hillshade.Azimuth := ParamAsFloat( PARAM_AZIMUTH, 315.0 ) ;
      hillshade.Altitude := ParamAsFloat( PARAM_ALTITUDE, 45.0 ) ;
      hillshade.Extent := ParamAsExtent( GIS_PIPELINE_PARAM_EXTENT, src.Extent ) ;
      hillshade.busyEventManager.UseProgressThreshold := False ;
      {$IFDEF CLR}
        hillshade.BusyEvent += DoBusyEvent ;
      {$ELSE}
        hillshade.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
      {$ENDIF}
      hillshade.Generate ;
    finally
      FreeObject( hillshade ) ;
    end ;

    inherited ;
  end ;

//==============================================================================
// TGIS_Hillshade
//==============================================================================

  constructor TGIS_Hillshade.Create ;
  begin
   inherited ;

   FZFactor := 1.0 ;
   FAzimuth := 315.0 ;
   FAltitude := 45.0 ;

   busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
  end ;

  procedure TGIS_Hillshade.doDestroy ;
  begin
    FreeObject( busyEventManager );

    inherited ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_Hillshade.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_Hillshade.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_Hillshade.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_Hillshade.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}

  procedure TGIS_Hillshade.generateCPU ;
  const
    LOCAL_PARAM_SRC : String = 'SourceLayer' ;
    LOCAL_PARAM_EXT : String = 'Extent' ;
    LOCAL_PARAM_DST : String = 'OutputLayer' ;
  var
    dem    : TGIS_LayerPixel ;
    smap   : TGIS_LayerPixel ;

    ext    : TGIS_Extent ;
    pl_src : TGIS_LayerPixelLock ;
    pl_dst : TGIS_LayerPixelLock ;
    p      : TPoint ;
    p0     : TGIS_Point ;
    p1     : TGIS_Point ;
    dist_v : Double ;
    dist_h : Double ;
    vmin   : Single ;
    vmax   : Single ;
    w      : Integer ;
    ws     : Integer ;
    we     : Integer ;
    h      : Integer ;
    hs     : Integer ;
    he     : Integer ;
    val    : Single ;
    pmax   : Int64 ;

    wndw   : TGIS_DoubleArray ;
    zfctr  : Double ;
    azmth  : Double ;
    alt    : Double ;
    slope  : Double ;
    aspct  : Double ;
    hlshd  : Double ;
    dzdx   : Double ;
    dzdy   : Double ;

    procedure proc_pixel( const _w : Integer ; const _h : Integer ) ;
    var
      ww : Integer ;
      hh : Integer ;
      ii : Integer ;
    begin
      val := pl_src.Grid[_h,_w] ;
      if val = dem.NoDataValue then
        exit ;

      for ii := 0 to 8 do
        wndw[ii] := val ;

      ii := 0 ;
      if hs = 0 then
        ii := 3 ;
      for hh := _h+hs to _h+he do begin
        if ws = 0 then
          inc( ii ) ;
        for ww := _w+ws to _w+we do begin
          if ( hh = _h ) and ( ww = _w ) then begin
            inc( ii ) ;
            continue ;
          end ;

          val := pl_src.Grid[hh,ww] ;
          if val = dem.NoDataValue then
            continue ;

          wndw[ii] := val ;
          inc( ii ) ;
        end ;
        if we = 0 then
          inc( ii ) ;
      end ;

      dzdx := ( ( wndw[2] + 2*wndw[5] + wndw[8] ) -
                ( wndw[0] + 2*wndw[3] + wndw[6] ) ) / ( 8.0*dist_h ) ;
      dzdy := ( ( wndw[6] + 2*wndw[7] + wndw[8] ) -
                ( wndw[0] + 2*wndw[1] + wndw[2] ) ) / ( 8.0*dist_v ) ;
      slope := ArcTan( zfctr * Sqrt( dzdx*dzdx + dzdy*dzdy ) ) ;
      aspct := ArcTan2( dzdy, -dzdx ) ;

      hlshd := 255.0 * ( ( Cos( alt ) * Cos( slope ) ) +
        ( Sin( alt ) * Sin( slope ) * Cos( azmth - aspct ) ) ) ;
      if hlshd < 0.0 then
        hlshd := 0.0 ;

      pl_dst.Grid[_h,_w] := hlshd ;
      vmin := Min( hlshd, vmin ) ;
      vmax := Max( hlshd, vmax ) ;
    end ;

  begin
    dem  := FSourceLayer ;
    smap := FOutputLayer ;

    if not assigned( dem ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SRC, 1
      ) ;

    if not dem.IsGrid then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SRC, 2
      ) ;

    if not assigned( smap ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 1
      ) ;

    if not smap.IsGrid then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 2
      ) ;

    ext := dem.CS.ExtentToCS( smap.CS, dem.Extent ) ;
    ext := GisCommonExtent( ext, smap.Extent ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 3
      ) ;

    if ( dem.BitWidth  <> smap.BitWidth  ) or
       ( dem.BitHeight <> smap.BitHeight ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 4
      ) ;

    ext := GisCommonExtent(
      ext, dem.CS.ExtentToCS( smap.CS, FExtent )
    ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_EXT, 1
      ) ;

    dem.Open ;
    smap.Open ;

    vmin := GIS_MAX_SINGLE ;
    vmax := 0.0 ;

    zfctr := FZFactor ;
    azmth := 360.0 - FAzimuth + 90.0 ;
    if azmth > 360.0 then
      azmth := azmth - 360.0 ;
    azmth := DegToRad( azmth ) ;
    alt := DegToRad( 90.0 - FAltitude ) ;
    SetLength( wndw, 9 ) ;

    pl_src := dem.LockPixels( ext, smap.CS, False ) ;
    pl_dst := smap.LockPixels( ext, smap.CS, True ) ;
    try
      if smap.CS.EPSG <> 0 then begin
        // center pixel and coordinates
        p0 := GisPoint( ( ext.XMin + ext.XMax )/2.0,
                        ( ext.YMin + ext.YMax )/2.0
                      ) ;
        p := pl_src.MapToRaster( p0, smap.CS ) ;
        p0 := pl_src.RasterToMap( p, smap.CS ) ;

        // vertical distance
        p1 := pl_src.RasterToMap( Point( p.X, p.Y+1 ), smap.CS ) ;
        dist_v := smap.CS.Distance( p0, p1 ) ;

        // horizontal distance
        p1 := pl_src.RasterToMap( Point( p.X+1, p.Y ), smap.CS ) ;
        dist_h := smap.CS.Distance( p0, p1 ) ;
      end
      else begin
        dist_v := ( dem.Extent.YMax - dem.Extent.YMin )/dem.BitHeight ;
        dist_h := ( dem.Extent.XMax - dem.Extent.XMin )/dem.BitWidth ;
      end ;

      pmax := ( pl_src.Bounds.Height + 1 ) * ( pl_src.Bounds.Width + 1 ) ;
      busyEventManager.StartEvent( GIS_RS_BUSY_HILLSHADE_GENERATE, pmax ) ;
      try
        // everything except borders
        hs := -1 ;
        he :=  1 ;
        ws := -1 ;
        we :=  1 ;
        for h := pl_src.Bounds.Top + 1 to pl_src.Bounds.Bottom - 1 do begin
          for w := pl_src.Bounds.Left + 1 to pl_src.Bounds.Right - 1 do begin
            proc_pixel( w, h ) ;
            if busyEventManager.PushEvent then
              exit ;
          end ;
        end ;

        // top border except corners
        hs :=  0 ;
        he :=  1 ;
        ws := -1 ;
        we :=  1 ;
        h := pl_src.Bounds.Top ;
        for w := pl_src.Bounds.Left + 1 to pl_src.Bounds.Right - 1 do begin
          proc_pixel( w, h ) ;
          if busyEventManager.PushEvent then
            exit ;
        end ;

        // bottom border except corners
        hs := -1 ;
        he :=  0 ;
        ws := -1 ;
        we :=  1 ;
        h := pl_src.Bounds.Bottom ;
        for w := pl_src.Bounds.Left + 1 to pl_src.Bounds.Right - 1 do begin
          proc_pixel( w, h ) ;
          if busyEventManager.PushEvent then
            exit ;
        end ;

        // left border except corners
        hs := -1 ;
        he :=  1 ;
        ws :=  0 ;
        we :=  1 ;
        w := pl_src.Bounds.Left ;
        for h := pl_src.Bounds.Top + 1 to pl_src.Bounds.Bottom - 1 do begin
          proc_pixel( w, h ) ;
          if busyEventManager.PushEvent then
            exit ;
        end ;

        // right border except corners
        hs := -1 ;
        he :=  1 ;
        ws := -1 ;
        we :=  0 ;
        w := pl_src.Bounds.Right ;
        for h := pl_src.Bounds.Top + 1 to pl_src.Bounds.Bottom - 1 do begin
          proc_pixel( w, h ) ;
          if busyEventManager.PushEvent then
            exit ;
        end ;

        //top-left corner
        hs :=  0 ;
        he :=  1 ;
        ws :=  0 ;
        we :=  1 ;
        proc_pixel( pl_src.Bounds.Left, pl_src.Bounds.Top ) ;

        //top-right corner
        hs :=  0 ;
        he :=  1 ;
        ws := -1 ;
        we :=  0 ;
        proc_pixel( pl_src.Bounds.Right, pl_src.Bounds.Top ) ;

        //bottom-left corner
        hs := -1 ;
        he :=  0 ;
        ws :=  0 ;
        we :=  1 ;
        proc_pixel( pl_src.Bounds.Left, pl_src.Bounds.Bottom ) ;

        //bottom-right corner
        hs := -1 ;
        he :=  0 ;
        ws := -1 ;
        we :=  0 ;
        proc_pixel( pl_src.Bounds.Right, pl_src.Bounds.Bottom ) ;

      finally
        busyEventManager.EndEvent ;
      end ;
    finally
      smap.MinHeight := vmin ;
      smap.MaxHeight := vmax ;
      dem.UnlockPixels( pl_src ) ;
      smap.UnlockPixels( pl_dst ) ;
    end ;
  end ;


{$IFNDEF JAVA OR ISLAND}
  procedure TGIS_Hillshade.generateOCL ;
  const
    LOCAL_PARAM_SRC : String = '_dem' ;
    LOCAL_PARAM_EXT : String = '_extent' ;
    LOCAL_PARAM_DST : String = '_hillshade' ;
  var
    dem    : TGIS_LayerPixel ;
    smap   : TGIS_LayerPixel ;

    ext    : TGIS_Extent ;
    pl_src : TGIS_LayerPixelLock ;
    pl_dst : TGIS_LayerPixelLock ;
    p      : TPoint ;
    p0     : TGIS_Point ;
    p1     : TGIS_Point ;
    dist_v : Double ;
    dist_h : Double ;
    vmin   : Single ;
    vmax   : Single ;
    slope  : Double ;
    w      : Integer ;
    h      : Integer ;
    i      : Integer ;

    zfctr  : Double ;
    azmth  : Double ;
    alt    : Double ;

    // OpenCL
    ocl  : TGIS_OpenCLProgram ;

    ibuf : array of Single ;
    obuf : array of Single ;
    xsiz : Integer ;
    ysiz : Integer ;
    siz  : Integer ;
  begin
    dem  := FSourceLayer ;
    smap := FOutputLayer ;

    if not assigned( dem ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SRC, 1
      ) ;

    if not dem.IsGrid then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_SRC, 2
      ) ;

    if not assigned( smap ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 1
      ) ;

    if not smap.IsGrid then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 2
      ) ;

    ext := dem.CS.ExtentToCS( smap.CS, dem.Extent ) ;
    ext := GisCommonExtent( ext, smap.Extent ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 3
      ) ;

    if ( dem.BitWidth  <> smap.BitWidth  ) or
       ( dem.BitHeight <> smap.BitHeight ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DST, 4
      ) ;

    ext := GisCommonExtent(
      ext, dem.CS.ExtentToCS( smap.CS, FExtent )
    ) ;

    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_EXT, 1
      ) ;

    dem.Open ;
    smap.Open ;

    vmin := GIS_MAX_SINGLE ;
    vmax := 0.0 ;

    zfctr := FZFactor ;
    azmth := 360.0 - FAzimuth + 90.0 ;
    if azmth > 360.0 then
      azmth := azmth - 360.0 ;
    azmth := DegToRad( azmth ) ;
    alt := DegToRad( 90.0 - FAltitude ) ;

    pl_src := dem.LockPixels( ext, smap.CS, False ) ;
    pl_dst := smap.LockPixels( ext, smap.CS, True ) ;
    try

      if smap.CS.EPSG <> 0 then begin
        // center pixel and coordinates
        p0 := GisPoint( ( ext.XMin + ext.XMax )/2.0,
                        ( ext.YMin + ext.YMax )/2.0
                      ) ;
        p := pl_src.MapToRaster( p0, smap.CS ) ;
        p0 := pl_src.RasterToMap( p, smap.CS ) ;

        // vertical distance
        p1 := pl_src.RasterToMap( Point( p.X, p.Y+1 ), smap.CS ) ;
        dist_v := smap.CS.Distance( p0, p1 ) ;

        // horizontal distance
        p1 := pl_src.RasterToMap( Point( p.X+1, p.Y ), smap.CS ) ;
        dist_h := smap.CS.Distance( p0, p1 ) ;
      end
      else begin
        dist_v := ( dem.Extent.YMax - dem.Extent.YMin )/dem.BitHeight ;
        dist_h := ( dem.Extent.XMax - dem.Extent.XMin )/dem.BitWidth ;
      end ;

      xsiz := pl_src.Bounds.Width + 1 ;
      ysiz := pl_src.Bounds.Height + 1 ;
      siz := xsiz*ysiz*sizeOf( Single ) ;
      SetLength( ibuf, xsiz*ysiz ) ;

      i := 0 ;
      for h := pl_src.Bounds.Top to pl_src.Bounds.Bottom do begin
        for w := pl_src.Bounds.Left to pl_src.Bounds.Right do begin
          ibuf[i] := pl_src.Grid[h,w] ;
          inc( i ) ;
        end ;
      end ;

      ocl := TGIS_OpenCLProgram.Create ;
      try
        if not ocl.LoadFromString( GIS_OCL_HILLSHADE_SOURCE,
                 GIS_OCL_HILLSHADE_KERNEL ) then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_OPENCL_LOAD ), '', ocl.ErrorCode
                ) ;

        ocl.WorkDimension := 2 ;
        ocl.GlobalWorkSizes[0] := xsiz ;
        ocl.GlobalWorkSizes[1] := ysiz ;

        {$IFDEF DCC}
          ocl.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], siz, @ibuf[0] ) ;
          ocl.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], siz, nil ) ;
          ocl.SetArgument( 2, sizeOf( Single ), @dem.NoDataValue ) ;
          ocl.SetArgument( 3, sizeOf( Single ), @smap.NoDataValue ) ;
          ocl.SetArgument( 4, sizeOf( Double ), @dist_v ) ;
          ocl.SetArgument( 5, sizeOf( Double ), @dist_h ) ;
          ocl.SetArgument( 6, sizeOf( Double ), @zfctr ) ;
          ocl.SetArgument( 7, sizeOf( Double ), @azmth ) ;
          ocl.SetArgument( 8, sizeOf( Double ), @alt ) ;
        {$ENDIF}
        {$IFDEF CLR}
          ocl.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], siz, ibuf ) ;
          ocl.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], siz, nil ) ;
          ocl.SetArgument( 2, sizeOf( Single ), dem.NoDataValue ) ;
          ocl.SetArgument( 3, sizeOf( Single ), smap.NoDataValue ) ;
          ocl.SetArgument( 4, sizeOf( Double ), dist_v ) ;
          ocl.SetArgument( 5, sizeOf( Double ), dist_h ) ;
          ocl.SetArgument( 6, sizeOf( Double ), zfctr ) ;
          ocl.SetArgument( 7, sizeOf( Double ), azmth ) ;
          ocl.SetArgument( 8, sizeOf( Double ), alt ) ;
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

      i := -1 ;
      for h := pl_dst.Bounds.Top to pl_dst.Bounds.Bottom do begin
        for w := pl_dst.Bounds.Left to pl_dst.Bounds.Right do begin

          inc( i ) ;
          slope := obuf[i] ;

          if slope < 0.0 then
            continue ;

          pl_dst.Grid[h,w] := slope ;
          vmin := Min( slope, vmin ) ;
          vmax := Max( slope, vmax ) ;
        end ;
      end ;

    finally
      dem.UnlockPixels( pl_src ) ;
      smap.UnlockPixels( pl_dst ) ;
    end ;

    smap.MinHeight := vmin ;
    smap.MaxHeight := vmax ;
  end ;
{$ENDIF}


  procedure TGIS_Hillshade.Generate(
    const _dem       : TGIS_LayerPixel ;
    const _extent    : TGIS_Extent ;
    const _hillshade : TGIS_LayerPixel ;
    const _zfactor   : Double ;
    const _azimuth   : Double ;
    const _altitude  : Double
  ) ;
  begin
    FSourceLayer := _dem ;
    FExtent := _extent ;
    FOutputLayer := _hillshade ;
    FZFactor := _zfactor ;
    FAzimuth := _azimuth ;
    FAltitude := _altitude ;

    Generate ;
  end ;


  procedure TGIS_Hillshade.Generate ;
  begin
    {$IFDEF JAVA OR ISLAND}
      generateCPU ;
    {$ELSE}
      if GisOpenCLEngine.Available and GisOpenCLEngine.Enabled then
        generateOCL
      else
        generateCPU ;
    {$ENDIF}
  end ;

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
  GisHillshade.SelfRegisterPipeline ;
{$ENDIF}
//==================================== END =====================================
end.
