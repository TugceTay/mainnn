//==============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//==============================================================================
{
  Viewshed and Fresnel zones analysis tool.
}

{$IFDEF DCC}
  unit GisViewshed ;
  {$HPPEMIT '#pragma link "GisViewshed"'}
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
    System.Collections.Generic,
    System.Runtime.InteropServices,
    TatukGIS.RTL,
    TatukGIS.NDK,
    TatukGIS.NDK.OpenCL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Generics.Collections,

    GisBaseObject,
    GisClasses,
    GisCsSystems,
    GisLayerVector,
    GisLayerPixel,
    GisOpenCL,
    GisRtl,
    GisTypes ;
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
  /// <summary>
  ///   Specifies what is used as elevation of the observers in the viewshed or
  ///   Fresnel zone clearance analysis. Elevation can be modified with
  ///   an offset attribute field/value.
  /// </summary>
  TGIS_ViewshedObserverElevation = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Elevation is the Z coordinate.
    /// </summary>
    Z,

    /// <summary>
    ///   Elevation is the M coordinate.
    /// </summary>
    M,

    /// <summary>
    ///   Elevation is the sum of the Z and the M coordinate.
    /// </summary>
    ZM,

    /// <summary>
    ///   Elevation is read from the underlying Digital Elevation Model (DEM).
    /// </summary>
    OnDem,

    /// <summary>
    ///   Elevation is read from the underlying Digital Elevation Model (DEM)
    ///   plus the M coordinate.
    /// </summary>
    OnDemPlusM,

    /// <summary>
    ///   Elevation is zero; useful if elevation should be offset-based.
    /// </summary>
    Zero
  ) ;

  /// <summary>
  ///   Specifies output values of the viewshed analysis.
  /// </summary>
  TGIS_ViewshedOutput = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Values are: 0 or no data - not visible, 1 - visible.
    /// </summary>
    Visibility,

    /// <summary>
    ///   Values are: 0 or no data - not visible, greater than 0 - number of
    ///   visible observers.
    /// </summary>
    NumberOfObservers
  ) ;

  /// <summary>
  ///   Viewshed, above-ground-level (AGL), and Fresnel zone clearance
  ///   generator.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     This class is OpenCL-enabled, so it can utilize OpenCL for increased
  ///     performance. Use TGIS_OpenCLEngine.Enabled to enable OpenCL
  ///     computation mode.
  ///   </note>
  ///   <note type="note">
  ///     Currently OpenCL computation mode can be used to generate viewshed
  ///     only. Above Ground Level and Fresnel zone clearance are always
  ///     computed using CPU. For best computation performance (e.g. for
  ///     continuous generation) set ViewshedOutput to Visibility and don't
  ///     set AboveGroundLevelLayer.
  ///   </note>
  /// </remarks>
  /// <exception cref="EGIS_Exception">
  ///  GIS_RS_ERR_BADPARAM,
  ///  GIS_RS_ERR_WRONGFIELD
  /// </exception>
  TGIS_Viewshed = {$IFDEF OXYGENE} public {$ENDIF}
                  class ( TGIS_BaseObjectDisposable )
    private
      FRadius                : Integer ;
      FRefractivity          : Double ;
      FCurvedEarth           : Boolean ;
      FObserverElevation     : TGIS_ViewshedObserverElevation ;
      FViewshedOutput        : TGIS_ViewshedOutput ;
      FFresnelZoneNumber     : Integer ;
      FFrequency             : Double ;
      FFillWithZeros         : Boolean ;
      FTerrainLayer          : TGIS_LayerPixel ;
      FObserversLayer        : TGIS_LayerVector ;
      FOutputLayer           : TGIS_LayerPixel ;
      FAboveGroundLevelLayer : TGIS_LayerPixel ;
      FTerrainOffset         : Single ;
      FObserversOffsetField  : String ;
      FObserversOffset       : Single ;

    private
      {$IFNDEF JAVA OR ISLAND} // OpenCL
        oProgram             : TGIS_OpenCLProgram ;
        iOCLxsiz             : Integer ;
        iOCLysiz             : Integer ;
      {$ENDIF}
      busyEventManager       : TGIS_BusyEventManager ;

    private
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}
      function  fget_Radius         : Integer ;
      procedure fset_Radius         ( const _val : Integer
                                    ) ;
      function  fget_Refractivity   : Double ;
      procedure fset_Refractivity   ( const _val : Double
                                    ) ;
      function  fget_Frequency      : Double ;
      procedure fset_Frequency      ( const _val : Double
                                    ) ;
      function  fget_TerrainLayer      : TGIS_LayerPixel ;
      procedure fset_TerrainLayer   ( const _val : TGIS_LayerPixel
                                    ) ;
    private
      procedure generateCPU ;
      {$IFNDEF JAVA OR ISLAND}
        procedure generateOCL_number ;
        procedure generateOCL_visibility ;
      {$ENDIF}
      procedure generateFresnelCPU  ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   <para>
      ///     Generates viewshed (and, optionally, above-ground-level (AGL)
      ///     raster) as a grid/Digital Elevation Model (DEM).
      ///   </para>
      ///   <para>
      ///     The values on the viewshed layer indicate the number of
      ///     observers which can see each cell. The values on the
      ///     above-ground-level layer indicate the minimum height that must
      ///     be added to a nonvisible cell to make it visible by at least
      ///     one observer.
      ///   </para>
      /// </summary>
      /// <param name="_dem">
      ///   terrain model as a grid layer
      /// </param>
      /// <param name="_observers">
      ///   layer containing observers; layer is expected to contain one or
      ///   more points; only first 50 points will be used.
      /// </param>
      /// <param name="_viewshed">
      ///   output layer containing the viewshed analysis; values are set
      ///   according to the ViewshedOutput property; the layer is
      ///   initially filled according to the FillWithZeros property;
      ///   the layer must be constructed before calling this method
      /// </param>
      /// <param name="_above_ground_level">
      ///   output layer containing values of how many meters should be added
      ///   to a particular point to be visible from at least one observer;
      ///   this layer must have the same extent, CS, and resolution as
      ///   _viewshed; if NULL this analysis will be skipped; layer is
      ///   initially filled according to FillWithZeros property
      /// </param>
      /// <param name="_offset_dem">
      ///   fixed terrain (_dem) elevation offset in meters to e.g. add forest
      ///   level
      /// </param>
      /// <param name="_offset_field">
      ///   attribute field defining observers elevation offset; field value
      ///   must be expressed in meters
      /// </param>
      /// <param name="_offset_observer">
      ///   fixed observers elevation offset in meters
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate   ( const _dem                : TGIS_LayerPixel ;
                             const _observers          : TGIS_LayerVector ;
                             const _viewshed           : TGIS_LayerPixel ;
                             const _above_ground_level : TGIS_LayerPixel ;
                             const _offset_dem         : Single ;
                             const _offset_field       : String ;
                             const _offset_observer    : Single
                           ) ; overload ;
      /// <summary>
      ///   <para>
      ///     Generates viewshed as a grid/Digital Elevation Model (DEM).
      ///   </para>
      ///   <para>
      ///     The values on the viewshed layer indicate the number of
      ///     observers which can see each cell.
      ///   </para>
      /// </summary>
      /// <param name="_dem">
      ///   terrain model as a grid layer
      /// </param>
      /// <param name="_observers">
      ///   layer containing observer locations; layer is expected to contain
      ///   one or more points; only first 50 points will be used.
      /// </param>
      /// <param name="_viewshed">
      ///   output layer containing the viewshed analysis; values are set
      ///   according to the ViewshedOutput property; the layer is
      ///   initially filled according to the FillWithZeros property;
      ///   the layer must be constructed before calling this method
      /// </param>
      /// <param name="_offset_dem">
      ///   fixed terrain (_dem) elevation offset in meters to e.g. add forest
      ///   level
      /// </param>
      /// <param name="_offset_field">
      ///   attribute field defining observers elevation offset; field value
      ///   must be expressed in meters
      /// </param>
      /// <param name="_offset_observer">
      ///   fixed observers elevation offset in meters
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate   ( const _dem                : TGIS_LayerPixel ;
                             const _observers          : TGIS_LayerVector ;
                             const _viewshed           : TGIS_LayerPixel ;
                             const _offset_dem         : Single ;
                             const _offset_field       : String ;
                             const _offset_observer    : Single
                           ) ; overload ;
      /// <summary>
      ///   <para>
      ///     Generates viewshed (and, optionally, above-ground-level (AGL)
      ///     raster) as a grid/Digital Elevation Model (DEM).
      ///   </para>
      ///   <para>
      ///     The values on the viewshed layer indicate the number of
      ///     observers which can see each cell. The values on the
      ///     above-ground-level layer indicate the minimum height that must
      ///     be added to a nonvisible cell to make it visible by at least
      ///     one observer.
      ///   </para>
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ; overload ;
      /// <summary>
      ///   <para>
      ///     Generates n-th Fresnel Zone clearance map as a grid/Digital
      ///     Elevation Model (DEM).
      ///   </para>
      ///   <para>
      ///     The values of the clearance map represent the biggest (based on
      ///     the signal from all the transmitters) among the smallest
      ///     percentages of unobstructed cross section of the Fresnel Zone
      ///     along the line of sight between the transmitter and the
      ///     receiver where 0 means that the Fresnel Zone is completely
      ///     blocked and 100 that it is free of obstructions.
      ///   </para>
      /// </summary>
      /// <param name="_dem">
      ///   terrain model as a grid layer
      /// </param>
      /// <param name="_transmitters">
      ///   layer containing transmitters; layer is expected to contain one or
      ///   more points; only first 50 points will be used.
      /// </param>
      /// <param name="_clearance">
      ///   output layer containing the clearance map; the values
      ///   vary from 0 (zone is completely obstructed) to 100 (zone is
      ///   unobstructed); the layer is initially filled according to
      ///   FillWithZeros property; the layer must be constructed before
      ///   calling this method
      /// </param>
      /// <param name="_zone_number">
      ///   number of the Fresnel Zone to calculate
      /// </param>
      /// <param name="_offset_dem">
      ///   fixed terrain (_dem) elevation offset in meters to e.g. add forest
      ///   level
      /// </param>
      /// <param name="_offset_field">
      ///   attribute field defining transmitters elevation offset; field value
      ///   must be expressed in meters
      /// </param>
      /// <param name="_offset_transmitter">
      ///   fixed transmitters elevation offset in meters
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure GenerateFresnel(
                             const _dem                : TGIS_LayerPixel ;
                             const _transmitters       : TGIS_LayerVector ;
                             const _clearance          : TGIS_LayerPixel ;
                             const _zone_number        : Integer ;
                             const _offset_dem         : Single ;
                             const _offset_field       : String ;
                             const _offset_transmitter : Single
                           ) ; overload ;
      /// <summary>
      ///   <para>
      ///     Generates n-th Fresnel Zone clearance map as a grid/Digital
      ///     Elevation Model (DEM).
      ///   </para>
      ///   <para>
      ///     The values of the clearance map represent the biggest (based on
      ///     the signal from all the transmitters) among the smallest
      ///     percentages of unobstructed cross section of the Fresnel Zone
      ///     along the line of sight between the transmitter and the
      ///     receiver where 0 means that the Fresnel Zone is completely
      ///     blocked and 100 that it is free of obstructions.
      ///   </para>
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure GenerateFresnel ; overload ;
    public
      /// <summary>
      ///   Specifies the maximum distance (in meters) between the observer and
      ///   the cell for the analysis to be performed; greater distance implies
      ///   no visibility; default is 20 km.
      /// </summary>
      property Radius            : Integer
                                   read  fget_Radius
                                   write fset_Radius ;
      /// <summary>
      ///   Specifies the refractivity coefficient of light; default is 0.13.
      /// </summary>
      property Refractivity      : Double
                                   read  fget_Refractivity
                                   write fset_Refractivity ;
      /// <summary>
      ///   If True then the elevation of input data will be corrected for
      ///   Earth curvature and refraction.
      /// </summary>
      property CurvedEarth           : Boolean
                                       read  FCurvedEarth
                                       write FCurvedEarth ;
      /// <summary>
      ///   Specifies coordinate used as the elevation of the observers;
      ///   default is DEM.
      /// </summary>
      property ObserverElevation     : TGIS_ViewshedObserverElevation
                                       read  FObserverElevation
                                       write FObserverElevation ;
      /// <summary>
      ///   Specifies output values of the viewshed analysis.
      /// </summary>
      property ViewshedOutput        : TGIS_ViewshedOutput
                                       read  FViewshedOutput
                                       write FViewshedOutput ;
      /// <summary>
      ///   Number of the Fresnel zone for clearance calculation.
      /// </summary>
      property FresnelZoneNumber     : Integer
                                       read  FFresnelZoneNumber
                                       write FFresnelZoneNumber ;
      /// <summary>
      ///   Radio frequency for Fresnel Zone analysis; default is 100 MHz.
      /// </summary>
      property Frequency             : Double
                                       read  fget_Frequency
                                       write fset_Frequency ;
      /// <summary>
      ///   If True then output is initially filled with zeros instead of
      ///   NoData (default).
      /// </summary>
      property FillWithZeros         : Boolean
                                       read  FFillWithZeros
                                       write FFillWithZeros ;
      /// <summary>
      ///   Terrain model as a grid layer.
      /// </summary>
      property TerrainLayer          : TGIS_LayerPixel
                                       read  fget_TerrainLayer
                                       write fset_TerrainLayer ;
      /// <summary>
      ///   Layer containing observer locations; layer is expected to contain
      ///   one or more points; only first 50 points will be used.
      /// </summary>
      property ObserversLayer        : TGIS_LayerVector
                                       read  FObserversLayer
                                       write FObserversLayer ;
      /// <summary>
      ///   Output layer containing the viewshed analysis; values are set
      ///   according to the ViewshedOutput property; the layer is
      ///   initially filled according to the FillWithZeros property.
      /// </summary>
      property OutputLayer           : TGIS_LayerPixel
                                       read  FOutputLayer
                                       write FOutputLayer ;
      /// <summary>
      ///   Output layer containing values of how many meters should be added
      ///   to a particular point to be visible from at least one observer;
      ///   this layer must have the same extent, CS, and resolution as
      ///   the OutputLayer; if NULL, this analysis will be skipped; layer is
      ///   initially filled according to the FillWithZeros property.
      /// </summary>
      property AboveGroundLevelLayer : TGIS_LayerPixel
                                       read  FAboveGroundLevelLayer
                                       write FAboveGroundLevelLayer ;
      /// <summary>
      ///   Fixed terrain elevation offset in meters to for example add forest
      ///   level.
      /// </summary>
      property TerrainOffset         : Single
                                       read  FTerrainOffset
                                       write FTerrainOffset ;
      /// <summary>
      ///   Attribute field defining observers elevation offset in meters.
      /// </summary>
      property ObserversOffsetField  : String
                                       read  FObserversOffsetField
                                       write FObserversOffsetField ;
      /// <summary>
      ///   Fixed observers elevation offset in meters.
      /// </summary>
      property ObserversOffset       : Single
                                       read  FObserversOffset
                                       write FObserversOffset ;
    published // events
      /// <summary>
      ///   Event fired upon progress of the interpolation process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent             : TGIS_BusyEvent
                                      add fadd_BusyEvent
                                      remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent          : TGIS_BusyEvent
                                      read  fget_BusyEvent
                                      write fset_BusyEvent ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Variants,
    System.Math,

    GisFunctions,
    GisResource,
    GisTransform,
    GisTypesUI ;
{$ENDIF}

{$IFNDEF JAVA}
const
  GIS_OCL_VIEWSHED_SOURCE : String =
    '#pragma OPENCL EXTENSION cl_khr_fp64 : enable' + #13#10 +
    #13#10 +
    '#define EARTHRAD 6378137' + #13#10 +
    #13#10 +
    '/* distance between two planar points */' + #13#10 +
    'double _distance(' + #13#10 +
    '  double xtoy, ' + #13#10 +
    '  double p0x,' + #13#10 +
    '  double p0y,' + #13#10 +
    '  double p1x,' + #13#10 +
    '  double p1y' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  double difx = xtoy*(p0x-p1x);' + #13#10 +
    '  double dify = p0y-p1y;' + #13#10 +
    #13#10 +
    '  return sqrt(difx*difx+dify*dify);' + #13#10 +
    '}' + #13#10 +
    #13#10 +
    '/* KERNEL */' + #13#10 +
    '__kernel void viewshed(' + #13#10 +
    '  __global float * input,' + #13#10 +
    '  __global char * output,' + #13#10 +
    '  __global double * points,' + #13#10 +
    '  float ndv,' + #13#10 +
    '  int curv,' + #13#10 +
    '  int xsiz,' + #13#10 +
    '  int ysiz,' + #13#10 +
    '  double xtoy,' + #13#10 +
    '  double rad,' + #13#10 +
    '  double ref,' + #13#10 +
    '  double off,' + #13#10 +
    '  int adem' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  int p = get_global_id(1);' + #13#10 +
    '  int i = get_global_id(0);' + #13#10 +
    #13#10 +
    '  int k,ix,iy,xx,yy,tmax,rx,ry;' + #13#10 +
    '  double cref,ax,bx,ay,by,dist,dd,di,rr,slp,smax,z,zz;' + #13#10 +
    '  double px,py,pz;' + #13#10 +
    #13#10 +
    '  px = points[3*p];' + #13#10 +
    '  py = points[3*p+1];' + #13#10 +
    '  pz = points[3*p+2];' + #13#10 +
    #13#10 +
    '  if (i>=xsiz)' + #13#10 +
    '  {' + #13#10 +
    '    i = i-xsiz+2;' + #13#10 +
    '    if (i>=ysiz)' + #13#10 +
    '    {' + #13#10 +
    '      i = i-ysiz;' + #13#10 +
    '      if (i>=xsiz)' + #13#10 +
    '      {' + #13#10 +
    '        i = i-xsiz+2;' + #13#10 +
    #13#10 +
    '        ix = 0 ;' + #13#10 +
    '        iy = ysiz-i;' + #13#10 +
    '        i = (ysiz-i)*xsiz;' + #13#10 +
    '      }' + #13#10 +
    '      else' + #13#10 +
    '      {' + #13#10 +
    '        ix = xsiz-1-i;' + #13#10 +
    '        iy = ysiz-1;' + #13#10 +
    '        i = ysiz*xsiz-1-i;' + #13#10 +
    '      }' + #13#10 +
    '    }' + #13#10 +
    '    else' + #13#10 +
    '    {' + #13#10 +
    '      ix = xsiz-1;' + #13#10 +
    '      iy = i-1;' + #13#10 +
    '      i = i*xsiz-1;' + #13#10 +
    '    }' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  {' + #13#10 +
    '    ix = i;' + #13#10 +
    '    iy = 0;' + #13#10 +
    '  }' + #13#10 +
    '' + #13#10 +
    '  cref = 1.0-ref;' + #13#10 +
    #13#10 +
    '  xx = (fabs(px-ix));' + #13#10 +
    '  yy = (fabs(py-iy));' + #13#10 +
    '/*  tmax = fmax(xx,yy)+1;*/' + #13#10 +
    '  if (xx>=yy)' + #13#10 +
    '  {' + #13#10 +
    '    tmax = xx+1;' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  {' + #13#10 +
    '    tmax = yy+1;' + #13#10 +
    '  }' + #13#10 +
    #13#10 +
    '  if (tmax<2) return;' + #13#10 +
    #13#10 +
    '  ax = (1.0*ix-px)/tmax;' + #13#10 +
    '  bx = px;' + #13#10 +
    '  ay = (1.0*iy-py)/tmax;' + #13#10 +
    '  by = py;' + #13#10 +
    #13#10 +
    '  if (adem==0)' + #13#10 +
    '  {' + #13#10 +
    '    k = 0;' + #13#10 +
    '    rx = (ax*k+bx);' + #13#10 +
    '    ry = (ay*k+by);' + #13#10 +
    '    i = (ry*xsiz+rx);' + #13#10 +
    '    pz = pz+input[i];' + #13#10 +
    '  }' + #13#10 +
    #13#10 +
    '  dist = _distance(xtoy,px,py,1.0*ix,1.0*iy);' + #13#10 +
    '  dd = dist/tmax;' + #13#10 +
    #13#10 +
    '  rx = (ax*0.1+bx);' + #13#10 +
    '  ry = (ay*0.1+by);' + #13#10 +
    '  i = (ry*xsiz+rx);' + #13#10 +
    '  output[i] = 1;' + #13#10 +
    #13#10 +
    '  smax = -DBL_MAX;' + #13#10 +
    '  for (k=1;k<=tmax;k++)' + #13#10 +
    '  {' + #13#10 +
    '    di = k*dd;' + #13#10 +
    '    if (di>rad) break;' + #13#10 +
    #13#10 +
    '    rx = (ax*k+bx);' + #13#10 +
    '    ry = (ay*k+by);' + #13#10 +
    '    i = (ry*xsiz+rx);' + #13#10 +
    '    z = input[i];' + #13#10 +
    '    if (z==ndv) continue;' + #13#10 +
    '    z = z+off;' + #13#10 +
    '    if (curv==0)' + #13#10 +
    '    {' + #13#10 +
    '      z = z-cref*(EARTHRAD/cos(di/EARTHRAD)-EARTHRAD);' + #13#10 +
    '    }' + #13#10 +
    '    zz = z-pz;' + #13#10 +
    '    rr = _distance(xtoy,rx,ry,px,py);' + #13#10 +
    '    slp = zz/rr;' + #13#10 +
    #13#10 +
    '    if (slp>=smax)' + #13#10 +
    '    {' + #13#10 +
    '      smax = slp;' + #13#10 +
    '      output[i] = 1;' + #13#10 +
    '    }' + #13#10 +
    '  }' + #13#10 +
    '}' ;
  GIS_OCL_VIEWSHED_KERNEL : String = 'viewshed' ;
{$ENDIF}

type

  T_byteGridLine = array of Byte ;

  T_byteGrid = array of T_byteGridLine ;

  T_byteGridLock = class
    private
      aMain   : T_byteGrid ;
      aAux    : TGIS_GridArray ;
      iWidth  : Integer ;
      iHeight : Integer ;
      iW0     : Integer ;
      iH0     : Integer ;
    public
      constructor Create ( const _lock : TGIS_LayerPixelLock ;
                           const _bagl : Boolean
                         ) ;
    {$IFNDEF MANAGED}
      public
        destructor Destroy ; override ;
    {$ENDIF}
    public
      procedure Reset    ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure Mark     ( const _w : Integer ;
                           const _h : Integer
                         ) ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  IsMarked ( const _w : Integer ;
                           const _h : Integer
                         ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  GetElev  ( const _w : Integer ;
                           const _h : Integer
                         ) : Single ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure SetElev  ( const _w : Integer ;
                           const _h : Integer ;
                           const _v : Single
                         ) ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  end ;

  T_singleGridLock = class
    private
      aMain   : TGIS_GridArray ;
      iWidth  : Integer ;
      iHeight : Integer ;
      iW0     : Integer ;
      iH0     : Integer ;
    public
      constructor Create ( const _lock : TGIS_LayerPixelLock
                         ) ;
    {$IFNDEF MANAGED}
      public
        destructor Destroy ; override ;
    {$ENDIF}
    public
      procedure Reset    ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      procedure Mark     ( const _w : Integer ;
                           const _h : Integer ;
                           const _v : Single
                         ) ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  Value    ( const _w : Integer ;
                           const _h : Integer
                         ) : Single ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  end ;

{$REGION 'T_byteGridLock'}
  constructor T_byteGridLock.Create(
    const _lock : TGIS_LayerPixelLock ;
    const _bagl : Boolean
  ) ;
  begin
    inherited Create ;

    iWidth := _lock.Bounds.Width + 1 ;
    iHeight := _lock.Bounds.Height + 1 ;
    iW0 := _lock.Bounds.Left ;
    iH0 := _lock.Bounds.Top ;

    SetLength( aMain, iHeight, iWidth ) ;
    if _bagl then
      SetLength( aAux, iHeight, iWidth ) ;

    Reset ;
  end ;

  {$IFNDEF MANAGED}
    destructor T_byteGridLock.Destroy ;
    begin

     inherited ;
    end ;
  {$ENDIF}

  procedure T_byteGridLock.Reset ;
  var
    w : Integer ;
    h : Integer ;
  begin
    for h := 0 to iHeight - 1 do begin
      for w := 0 to iWidth - 1 do begin
        aMain[h,w] := 0 ;
      end ;
    end ;
  end ;

  procedure T_byteGridLock.Mark(
    const _w : Integer ;
    const _h : Integer
  ) ;
  begin
    aMain[_h-iH0,_w-iW0] := 1 ;
  end ;

  function T_byteGridLock.IsMarked(
    const _w : Integer ;
    const _h : Integer
  ) : Boolean ;
  begin
    if aMain[_h-iH0,_w-iW0] = 0 then
      Result := False
    else
      Result := True ;
  end ;

  function T_byteGridLock.GetElev(
    const _w : Integer ;
    const _h : Integer
  ) : Single ;
  begin
    Result := aAux[_h-iH0,_w-iW0] ;
  end ;

  procedure T_byteGridLock.SetElev(
    const _w : Integer ;
    const _h : Integer ;
    const _v : Single
  ) ;
  begin
    aAux[_h-iH0,_w-iW0] := _v ;
  end ;
{$ENDREGION}

{$REGION 'T_singleGridLock'}
  constructor T_singleGridLock.Create(
    const _lock : TGIS_LayerPixelLock
  ) ;
  begin
    inherited Create ;

    iWidth := _lock.Bounds.Width + 1 ;
    iHeight := _lock.Bounds.Height + 1 ;
    iW0 := _lock.Bounds.Left ;
    iH0 := _lock.Bounds.Top ;

    SetLength( aMain, iHeight, iWidth ) ;

    Reset ;
  end ;

  {$IFNDEF MANAGED}
    destructor T_singleGridLock.Destroy ;
    begin

     inherited ;
    end ;
  {$ENDIF}

  procedure T_singleGridLock.Reset ;
  var
    w : Integer ;
    h : Integer ;
  begin
    for h := 0 to iHeight - 1 do begin
      for w := 0 to iWidth - 1 do begin
        aMain[h,w] := GIS_GRID_NOVALUE ;
      end ;
    end ;
  end ;

  procedure T_singleGridLock.Mark(
    const _w : Integer ;
    const _h : Integer ;
    const _v : Single
  ) ;
  begin
    aMain[_h-iH0,_w-iW0] := _v ;
  end ;

  function T_singleGridLock.Value(
    const _w : Integer ;
    const _h : Integer
  ) : Single ;
  begin
    Result := aMain[_h-iH0,_w-iW0] ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_Viewshed'}
  constructor TGIS_Viewshed.Create ;
  begin
    inherited ;

    FRadius := 20000 ;
    FRefractivity := 0.13 ;
    FCurvedEarth := False ;
    FObserverElevation := TGIS_ViewshedObserverElevation.OnDem ;
    FViewshedOutput := TGIS_ViewshedOutput.NumberOfObservers ;
    FFresnelZoneNumber := 1 ;
    FFrequency := 100000000 ;
    FFillWithZeros := False ;
    FTerrainOffset := 0.0 ;
    FObserversOffsetField := '' ;
    FObserversOffset := 0.0 ;

    busyEventManager := TGIS_BusyEventManager.Create( Self ) ;

    {$IFNDEF JAVA OR ISLAND}
      if not ( GisOpenCLEngine.Available and GisOpenCLEngine.Enabled ) then
        exit ;

      oProgram := TGIS_OpenCLProgram.Create ;
      if not oProgram.LoadFromString( GIS_OCL_VIEWSHED_SOURCE,
               GIS_OCL_VIEWSHED_KERNEL ) then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_OPENCL_LOAD ), '', oProgram.ErrorCode
              ) ;
      oProgram.WorkDimension := 2 ;
    {$ENDIF}
  end ;

  procedure TGIS_Viewshed.doDestroy ;
  begin
    FreeObject( busyEventManager ) ;
    {$IFNDEF JAVA OR ISLAND}
      FreeObject( oProgram ) ;
    {$ENDIF}

    inherited ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_Viewshed.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_Viewshed.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_Viewshed.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_Viewshed.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}

  function TGIS_Viewshed.fget_Radius : Integer ;
  begin
    Result := FRadius ;
  end ;

  procedure TGIS_Viewshed.fset_Radius(
    const _val : Integer
  ) ;
  begin
    if ( _val < 0 ) then
      FRadius := 0
    else
      FRadius := _val ;
  end ;

  function TGIS_Viewshed.fget_Refractivity : Double ;
  begin
    Result := FRefractivity ;
  end ;

  procedure TGIS_Viewshed.fset_Refractivity(
    const _val : Double
  ) ;
  begin
    if ( _val < 0.0 ) then
      FRefractivity := 0.0
    else
    if ( _val > 1.0 ) then
      FRefractivity := 1.0
    else
      FRefractivity := _val ;
  end ;

  function TGIS_Viewshed.fget_Frequency : Double ;
  begin
    Result := FFrequency ;
  end ;

  procedure TGIS_Viewshed.fset_Frequency(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FFrequency := _val ;
  end ;

  function TGIS_Viewshed.fget_TerrainLayer : TGIS_LayerPixel ;
  begin
    Result := FTerrainLayer ;
  end ;

  procedure TGIS_Viewshed.fset_TerrainLayer(
    const _val : TGIS_LayerPixel
  ) ;
  {$IFNDEF JAVA OR ISLAND}
  var
    lpl  : TGIS_LayerPixelLock ;
    ibuf : array of Single ;
    xsiz : Integer ;
    ysiz : Integer ;
    isiz : Integer ;
    h    : Integer ;
    w    : Integer ;
    i    : Integer ;
  {$ENDIF}
  begin
    FTerrainLayer := _val ;

    {$IFNDEF JAVA OR ISLAND}
      if not assigned( FTerrainLayer ) then
        exit ;

      if not assigned( oProgram ) then
        exit ;

      FTerrainLayer.Open ;

      lpl := FTerrainLayer.LockPixels(
               FTerrainLayer.Extent, FTerrainLayer.CS, False ) ;
      try
        xsiz := lpl.Bounds.Width + 1 ;
        ysiz := lpl.Bounds.Height + 1 ;
        isiz := xsiz*ysiz*sizeOf( Single ) ;
        SetLength( ibuf, xsiz*ysiz ) ;

        i := 0 ;
        for h := lpl.Bounds.Top to lpl.Bounds.Bottom do begin
          for w := lpl.Bounds.Left to lpl.Bounds.Right do begin
            ibuf[i] := lpl.Grid[h,w] ;
            inc( i ) ;
          end ;
        end ;

      finally
        FTerrainLayer.UnlockPixels( lpl ) ;
      end ;

      iOCLxsiz := xsiz ;
      iOCLysiz := ysiz ;

      {$IFDEF DCC}
        oProgram.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, @ibuf[0] ) ;
        oProgram.SetArgument( 3, sizeOf( Single ), @FTerrainLayer.NoDataValue ) ;
        oProgram.SetArgument( 5, sizeOf( Integer ), @xsiz ) ;
        oProgram.SetArgument( 6, sizeOf( Integer ), @ysiz ) ;
      {$ENDIF}
      {$IFDEF CLR}
        oProgram.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, ibuf ) ;
        oProgram.SetArgument( 3, sizeOf( Single ), FTerrainLayer.NoDataValue ) ;
        oProgram.SetArgument( 5, sizeOf( Integer ), xsiz ) ;
        oProgram.SetArgument( 6, sizeOf( Integer ), ysiz ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure TGIS_Viewshed.generateCPU ;
  const
    LOCAL_PARAM_DEM       : String  = 'TerrainLayer' ;
    LOCAL_PARAM_OBSERVERS : String  = 'ObserversLayer' ;
    LOCAL_PARAM_VIEWSHED  : String  = 'ViewshedLayer' ;
    LOCAL_PARAM_AGL       : String  = 'AboveGroundLevelLayer' ;
    LOCAL_PARAM_FIELD     : String  = 'ObserversOffsetField' ;
    LOCAL_EARTH_RAD       : Double  = 6378137 ;
    LOCAL_EARTH_RAD_RF    : Double  = 8504000 ;
    LOCAL_LIGHT_SPEED     : Double  = 299792458 ;
    LOCAL_MAX_OBSERVERS   : Integer = 50 ;
  var
    dem     : TGIS_LayerPixel ;
    obs     : TGIS_LayerVector ;
    vws     : TGIS_LayerPixel ;
    agl     : TGIS_LayerPixel ;
    dem_off : Single ;
    obs_fld : String ;
    obs_off : Single ;

    cs      : TGIS_CSCoordinateSystem ;
    {$IFDEF DCC}
      shp   : TGIS_Shape ;
    {$ENDIF}
    bagl    : Boolean ;
    ext     : TGIS_Extent ;
    pl_dem  : TGIS_LayerPixelLock ;
    pl_vshd : TGIS_LayerPixelLock ;
    pl_agl  : TGIS_LayerPixelLock ;
    bgl     : T_byteGridLock ;
    p       : TPoint ;
    z       : Double ;
    p0      : TGIS_Point3D ;
    p1      : TGIS_Point ;
    val     : Single ;
    vmax    : Single ;
    elv     : Single ;
    emax    : Single ;
    w       : Integer ;
    h       : Integer ;
    pr      : TGIS_Point ;
    ax      : Double ;
    bx      : Double ;
    ay      : Double ;
    by      : Double ;
    tmax    : Integer ;
    bfld    : Boolean ;
    bcs     : Boolean ;
    wdth    : Double ;
    hght    : Double ;
    rad     : Double ;
    radx    : Double ;
    ang     : Double ;
    // trace ray
    slp     : Double ;
    smax    : Double ;
    dist    : Double ;
    dd      : Double ;
    di      : Double ;
    cref    : Double ;
    // progress
    poi     : Integer ;
    cnt     : Integer ;
    total   : Int64 ;
    abrt    : Boolean ;
    id      : TGIS_Uid ;

    function check_field : Boolean ;
    var
      ff : TGIS_FieldInfo ;
      ii : Integer ;
    begin
      Result := True ;

      if IsStringEmpty( obs_fld ) then
        exit ;

      for ii := 0 to obs.Fields.Count - 1 do begin
        ff := obs.Fields[ii] ;
        if ff.Name = obs_fld then begin
          if ( ff.FieldType <> TGIS_FieldType.Number ) and
             ( ff.FieldType <> TGIS_FieldType.Float  ) then
            exit
          else
            break ;
        end ;
      end ;

      Result := False ;
    end ;

    function g_voff( const _s : TGIS_Shape ) : Single ;
    begin
      if bfld then
        Result := obs_off + VarToSingle( _s.GetField( obs_fld ) )
      else
        Result := obs_off ;
    end ;

    procedure prep_radx ;
    var
      ww : Double ;
    begin
      ww := cs.Distance(
              GisPoint( p0.X, p0.Y ),
              GisPoint( p0.X + rad, p0.Y )
            ) ;
      radx := rad*FRadius/ww ;
    end ;

    function prep_coeff : Boolean ;
    var
      xx : Double ;
      yy : Double ;
    begin
      Result := False ;

      xx := Abs( p0.X - p1.X ) ;
      yy := Abs( p0.Y - p1.Y ) ;

      if xx >= yy then
        tmax := RoundS( xx*pl_dem.Bounds.Width/wdth )
      else
        tmax := RoundS( yy*pl_dem.Bounds.Height/hght ) ;

      // correction for rounding problems
      inc( tmax ) ;

      if tmax < 2 then
        exit ;

      ax := ( p1.X - p0.X )/tmax ;
      bx := p0.X ;
      ay := ( p1.Y - p0.Y )/tmax ;
      by := p0.Y ;

      Result := True ;
    end ;

    function trace_ray( const _w : Integer ; const _h : Integer ) : Boolean ;
    var
      ii : Integer ;
      zz : Double ;
      rr : Double ;
    begin
      Result := False ;

      abrt := busyEventManager.PushEvent ;
      if abrt then begin
        vws.MinHeight := 0.0 ;
        vws.MaxHeight := vmax ;
        if bagl then begin
          agl.MinHeight := 0.0 ;
          agl.MaxHeight := emax ;
        end ;
        exit ;
      end ;

      p1 := pl_vshd.RasterToMap( Point( _w, _h ), vws.CS ) ;
      dist := vws.CS.Distance( GisPoint( p0.X, p0.Y ), p1 ) ;

      if not prep_coeff then
        exit ;

      dd := dist/tmax ;

      pr := GisPoint( 0.1*ax + bx, 0.1*ay + by ) ;
      p := pl_vshd.MapToRaster( pr, vws.CS ) ;
      bgl.Mark( p.X, p.Y ) ;

      smax := -GIS_MAX_DOUBLE ;
      for ii := 1 to tmax do begin
        di := ii*dd ;
        if di > FRadius then
          break ;
        pr := GisPoint( ax*ii + bx, ay*ii + by ) ;
        p := pl_dem.MapToRaster( pr, vws.CS ) ;
        if ( p.X < pl_dem.Bounds.Left ) or ( p.X > pl_dem.Bounds.Right ) or
           ( p.Y < pl_dem.Bounds.Top ) or ( p.Y > pl_dem.Bounds.Bottom ) then
          continue ;

        z := pl_dem.Grid[p.Y,p.X] ;
        if z = GIS_GRID_NOVALUE then
          continue ;
        z := z + dem_off ;
        if bcs and CurvedEarth then
          z := z - cref*( LOCAL_EARTH_RAD/Cos( di/LOCAL_EARTH_RAD ) -
                          LOCAL_EARTH_RAD ) ;

        zz := z - p0.Z ;
        rr := Sqrt( Sqr( pr.X - p0.X ) + Sqr( pr.Y - p0.Y ) ) ;
        slp := zz/rr ;

        if slp >= smax then begin
          smax := slp ;
          p := pl_vshd.MapToRaster( pr, vws.CS ) ;
          if ( p.X < pl_vshd.Bounds.Left ) or ( p.X > pl_vshd.Bounds.Right ) or
             ( p.Y < pl_vshd.Bounds.Top ) or ( p.Y > pl_vshd.Bounds.Bottom ) then
            continue ;
          bgl.Mark( p.X, p.Y ) ;
          if bagl then
            ang := ArcTan2( zz, rr ) ;
        end
        else begin
          if bagl then begin
            zz := Abs( zz - rr*Tan( ang ) ) ;
            p := pl_vshd.MapToRaster( pr, vws.CS ) ;
            if ( p.X < pl_vshd.Bounds.Left ) or ( p.X > pl_vshd.Bounds.Right ) or
               ( p.Y < pl_vshd.Bounds.Top ) or ( p.Y > pl_vshd.Bounds.Bottom ) then
              continue ;
            bgl.SetElev( p.X, p.Y, zz ) ;
          end ;
        end ;
      end ;

      Result := True ;
    end ;

  begin
    dem     := FTerrainLayer ;
    obs     := FObserversLayer ;
    vws     := FOutputLayer ;
    agl     := FAboveGroundLevelLayer ;
    dem_off := FTerrainOffset ;
    obs_fld := FObserversOffsetField ;
    obs_off := FObserversOffset ;

    if not assigned( dem ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DEM, 1
      ) ;

    if not assigned( obs ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_OBSERVERS, 1
      ) ;

    if not assigned( vws ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_VIEWSHED, 1
      ) ;

    if GisIsEmptyExtent( GisCommonExtent( dem.Extent, vws.Extent ) ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_VIEWSHED, 2
      ) ;

    if not check_field then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_FIELD, 1
      ) ;

    bagl := assigned( agl ) ;
    if bagl then begin
      if agl.CS.EPSG <> vws.CS.EPSG then
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_AGL, 1
        ) ;

      if not GisIsSameExtent( agl.Extent, vws.Extent ) then
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_AGL, 2
        ) ;

      if ( agl.BitWidth  <> vws.BitWidth  ) or
         ( agl.BitHeight <> vws.BitHeight )
      then
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_AGL, 3
        ) ;
    end ;

    dem.Open ;
    obs.Open ;
    vws.Open ;
    if bagl then
      agl.Open ;

    bagl := assigned( agl ) ;

    abrt := False ;

    vmax := 0.0 ;
    emax := 0.0 ;
    cref := 1.0 - FRefractivity ;

    bfld := not IsStringEmpty( obs_fld ) ;
    bcs := vws.CS.EPSG <> 0 ;

    if assigned( obs.Viewer ) then
      cs := obs.Viewer.Ref.CS
    else
      cs := obs.CS ;

    if cs is TGIS_CSProjectedCoordinateSystem then
      rad := TGIS_CSProjectedCoordinateSystem(
               cs ).Units.FromBase( FRadius )
    else
    if cs is TGIS_CSGeographicCoordinateSystem then
      rad := RadToDeg( FRadius/LOCAL_EARTH_RAD )
    else
      rad := FRadius ;

    if rad <= 0.0 then
      exit ;

    cnt := 0 ;
    for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in obs.Loop(
                  GisCommonExtent( obs.Extent, dem.Extent )
               )
    do begin
      id := shp.Uid ;
      inc( cnt ) ;
      if cnt >= LOCAL_MAX_OBSERVERS then
        break ;
    end ;

    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_VIEWSHED ), cnt ) ;
    try
      poi := -1 ;
      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in obs.Loop(
                    GisCommonExtent( obs.Extent, dem.Extent )
                 )
      do begin
        inc( poi ) ;
        if poi > LOCAL_MAX_OBSERVERS then
          break ;

        p0 := cs.ToCS3D( vws.CS, shp.GetPoint3D( 0, 0 ) ) ;

        if vws.CS is TGIS_CSGeographicCoordinateSystem then begin
          prep_radx ;
          ext := GisExtent( p0.X - radx, p0.Y - rad, p0.X + radx, p0.Y + rad ) ;
        end
        else begin
          ext := GisExtent( p0.X - rad, p0.Y - rad, p0.X + rad, p0.Y + rad ) ;
        end ;
        ext := GisCommonExtent( ext, vws.Extent ) ;
        wdth := ext.XMax - ext.XMin ;
        hght := ext.YMax - ext.YMin ;

        pl_dem := dem.LockPixels( ext, vws.CS, False ) ;
        pl_vshd := vws.LockPixels( ext, vws.CS, True ) ;
        if bagl then
          pl_agl := agl.LockPixels( ext, vws.CS, True ) ;

        bgl := T_byteGridLock.Create( pl_vshd, bagl ) ;

        total := 2 * ( pl_vshd.Bounds.Width + pl_vshd.Bounds.Height ) ;
        busyEventManager.StartEvent(
          _rsrc( GIS_RS_BUSY_VIEWSHED_TRACERAY ), total
        ) ;
        try
          case ObserverElevation of
            TGIS_ViewshedObserverElevation.Z    :
              p0.Z := p0.Z + g_voff( shp ) ;
            TGIS_ViewshedObserverElevation.M    :
              p0.Z := p0.M + g_voff( shp ) ;
            TGIS_ViewshedObserverElevation.ZM   :
              p0.Z := p0.Z + p0.M + g_voff( shp ) ;
            TGIS_ViewshedObserverElevation.OnDem :
              begin
                p := pl_dem.MapToRaster( GisPoint( p0.X, p0.Y ), vws.CS ) ;
                z := pl_dem.Grid[p.Y,p.X] ;
                p0.Z := z + g_voff( shp ) ;
              end ;
            TGIS_ViewshedObserverElevation.OnDemPlusM :
              begin
                p := pl_dem.MapToRaster( GisPoint( p0.X, p0.Y ), vws.CS ) ;
                z := pl_dem.Grid[p.Y,p.X] ;
                p0.Z := z + p0.M + g_voff( shp ) ;
              end ;
            TGIS_ViewshedObserverElevation.Zero :
              p0.Z := g_voff( shp ) ;
          end ;

          // mark the cell right under the observer
          pr := GisPoint( p0.X, p0.Y ) ;
          p := pl_vshd.MapToRaster( pr, vws.CS ) ;
          bgl.Mark( p.X, p.Y ) ;

          // trace rays to the left extent border
          w := pl_vshd.Bounds.Left ;
          for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          // trace rays to the right extent border
          w := pl_vshd.Bounds.Right ;
          for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          // trace rays to the top extent border
          h := pl_vshd.Bounds.Top ;
          for w := pl_vshd.Bounds.Left + 1 to pl_vshd.Bounds.Right - 1 do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          // trace rays to the bottom extent border
          h := pl_vshd.Bounds.Bottom ;
          for w := pl_vshd.Bounds.Left + 1 to pl_vshd.Bounds.Right - 1 do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
            for w := pl_vshd.Bounds.Left to pl_vshd.Bounds.Right do begin
              if bgl.IsMarked( w, h ) then begin
                val := pl_vshd.Grid[h,w] ;
                if val = GIS_GRID_NOVALUE then
                  val := 0.0 ;
                if ViewshedOutput = TGIS_ViewshedOutput.NumberOfObservers then
                  val := val + 1.0
                else
                  val := 1.0 ;
                pl_vshd.Grid[h,w] := val ;
                vmax := Max( vmax, val ) ;
                if bagl then
                  pl_agl.Grid[h,w] := 0.0 ;
              end
              else begin
                if bagl then begin
                  val := pl_vshd.Grid[h,w] ;
                  if val = GIS_GRID_NOVALUE then begin
                    val := pl_agl.Grid[h,w] ;
                    elv := bgl.GetElev( w, h ) ;
                    if val = GIS_GRID_NOVALUE then begin
                      if elv = 0.0 then
                        continue ;
                      pl_agl.Grid[h,w] := elv ;
                      emax := Max( emax, elv ) ;
                    end
                    else
                    if elv < val then
                      pl_agl.Grid[h,w] := elv ;
                  end ;
                end ;
              end ;
            end ;
          end ;

        finally
          dem.UnlockPixels( pl_dem ) ;
          vws.UnlockPixels( pl_vshd ) ;
          if bagl then
            agl.UnlockPixels( pl_agl ) ;
          FreeObject( bgl ) ;
          busyEventManager.EndEvent ;
        end ;
      end ;

      if FFillWithZeros then begin
        pl_vshd := vws.LockPixels( vws.Extent, vws.CS, True ) ;
        try
          for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
            for w := pl_vshd.Bounds.Left to pl_vshd.Bounds.Right do begin
              if pl_vshd.Grid[h,w] = vws.NoDataValue then
                pl_vshd.Grid[h,w] := 0.0 ;
            end ;
          end ;
        finally
          vws.UnlockPixels( pl_vshd ) ;
        end ;
      end ;

      vws.MinHeight := 0.0 ;
      vws.MaxHeight := vmax ;

      if bagl then begin
        emax := 0.0 ;
        pl_agl := agl.LockPixels( agl.Extent, agl.CS, True ) ;
        try
          for h := pl_agl.Bounds.Top to pl_agl.Bounds.Bottom do begin
            for w := pl_agl.Bounds.Left to pl_agl.Bounds.Right do begin
              val := pl_agl.Grid[h,w] ;
              if ( not FFillWithZeros ) and ( val = 0.0 ) then
                pl_agl.Grid[h,w] := agl.NoDataValue ;
              emax := Max( emax, val ) ;
            end ;
          end ;
        finally
          agl.UnlockPixels( pl_agl ) ;
        end ;
        agl.MinHeight := 0.0 ;
        agl.MaxHeight := emax ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;
  end ;

{$IFNDEF JAVA OR ISLAND}
  procedure TGIS_Viewshed.generateOCL_number ;
  const
    LOCAL_PARAM_DEM : String = 'TerrainLayer' ;
    LOCAL_PARAM_OBSERVERS : String = 'ObserversLayer' ;
    LOCAL_PARAM_VIEWSHED : String = 'OutputLayer' ;
    LOCAL_PARAM_FIELD : String = 'ObserversOffsetField' ;
  const
    LOCAL_EARTH_RAD : Double = 6378137 ;
    LOCAL_LIGHT_SPEED : Double = 299792458 ;
    LOCAL_LOCK_PROGRESS : Integer = 100 ;
    LOCAL_MAX_OBSERVERS : Integer = 50 ;
  var
    dem     : TGIS_LayerPixel ;
    obs     : TGIS_LayerVector ;
    vws     : TGIS_LayerPixel ;
    dem_off : Single ;
    obs_fld : String ;
    obs_off : Single ;

    cs      : TGIS_CSCoordinateSystem ;
    {$IFDEF DCC}
      shp   : TGIS_Shape ;
    {$ENDIF}
    ext     : TGIS_Extent ;
    pl_dem  : TGIS_LayerPixelLock ;
    pl_vshd : TGIS_LayerPixelLock ;
    p0      : TGIS_Point3D ;
    val     : Single ;
    vmax    : Single ;
    w       : Integer ;
    h       : Integer ;
    bfld    : Boolean ;
    bcs     : Boolean ;
    wdth    : Double ;
    hght    : Double ;
    rad     : Double ;
    radx    : Double ;
    cref    : Double ;

    ibuf : array of Single ;
    obuf : array of Byte ;
    pbuf : array of Double ;
    xsiz : Integer ;
    ysiz : Integer ;
    isiz : Integer ;
    osiz : Integer ;
    psiz : Integer ;

    wdst : Double ;
    prad : Double ;
    vbuf : Single ;
    curv : Integer ;
    xtoy : Double ;
    adem : Integer ;
    i    : Integer ;

    function check_field : Boolean ;
    var
      ff : TGIS_FieldInfo ;
      ii : Integer ;
    begin
      Result := True ;

      if IsStringEmpty( obs_fld ) then
        exit ;

      for ii := 0 to obs.Fields.Count - 1 do begin
        ff := obs.Fields[ii] ;
        if ff.Name = obs_fld then begin
          if ( ff.FieldType <> TGIS_FieldType.Number ) and
             ( ff.FieldType <> TGIS_FieldType.Float  ) then
            exit
          else
            break ;
        end ;
      end ;

      Result := False ;
    end ;

    function g_voff( const _s : TGIS_Shape ) : Single ;
    var
      val : Single ;
    begin
      val := VarToSingle( _s.GetField( obs_fld ) ) ;

      if bfld then
        Result := obs_off + val
      else
        Result := obs_off ;
    end ;

    procedure prep_radx ;
    var
      ww : Double ;
    begin
      ww := cs.Distance(
              GisPoint( p0.X, p0.Y ),
              GisPoint( p0.X + rad, p0.Y )
            ) ;
      radx := rad*FRadius/ww ;
    end ;

  begin
    dem     := FTerrainLayer ;
    obs     := FObserversLayer ;
    vws     := FOutputLayer ;
    dem_off := FTerrainOffset ;
    obs_fld := FObserversOffsetField ;
    obs_off := FObserversOffset ;

    if not assigned( dem ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DEM, 1
      ) ;

    if not assigned( obs ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_OBSERVERS, 1
      ) ;

    if not assigned( vws ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_VIEWSHED, 1
      ) ;

    if GisIsEmptyExtent( GisCommonExtent( dem.Extent, vws.Extent ) ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_VIEWSHED, 2
      ) ;

    if not check_field then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_FIELD, 1
      ) ;

    dem.Open ;
    obs.Open ;
    vws.Open ;

    vmax := 0.0 ;
    cref := 1.0 - FRefractivity ;

    bfld := not IsStringEmpty( obs_fld );
    bcs := vws.CS.EPSG <> 0 ;

    if bcs and CurvedEarth then
      curv := 0
    else
      curv := 1 ;

    if assigned( obs.Viewer ) then
      cs := obs.Viewer.Ref.CS
    else
      cs := obs.CS ;

    if cs is TGIS_CSProjectedCoordinateSystem then
      rad := TGIS_CSProjectedCoordinateSystem(
               cs ).Units.FromBase( FRadius )
    else
    if cs is TGIS_CSGeographicCoordinateSystem then
      rad := RadToDeg( FRadius/LOCAL_EARTH_RAD )
    else
      rad := FRadius ;

    if rad <= 0.0 then
      exit ;

    adem := 1 ;
    case ObserverElevation of
      TGIS_ViewshedObserverElevation.OnDem,
      TGIS_ViewshedObserverElevation.OnDemPlusM : adem := 0 ;
    end ;

    if dem.CS.EPSG <> 0 then
      wdst := dem.CS.Distance(
        GisPoint( dem.Extent.XMin, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 ),
        GisPoint( dem.Extent.XMax, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 )
      )
    else
      wdst := GisPoint2Point(
        GisPoint( dem.Extent.XMin, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 ),
        GisPoint( dem.Extent.XMax, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 )
      ) ;

    ext := dem.Extent ;
    p0 := GisPoint3D(
      ( ext.XMin + ext.XMax )/2.0,
      ( ext.YMin + ext.YMax )/2.0,
      0.0
    ) ;
    if vws.CS is TGIS_CSGeographicCoordinateSystem then begin
      prep_radx ;
      ext := GisExtent( p0.X - radx, p0.Y - rad, p0.X + radx, p0.Y + rad ) ;
    end
    else begin
      radx := rad ;
      ext := GisExtent( p0.X - rad, p0.Y - rad, p0.X + rad, p0.Y + rad ) ;
    end ;
    wdth := ext.XMax - ext.XMin ;
    hght := ext.YMax - ext.YMin ;
    xtoy := rad/radx ;
    prad := xtoy*FRadius*dem.BitWidth/wdst ;

    ext := dem.Extent ;
    wdth := ext.XMax - ext.XMin ;
    hght := ext.YMax - ext.YMin ;

    vmax := 0.0 ;

    if not assigned( oProgram ) then begin
      oProgram := TGIS_OpenCLProgram.Create ;
      if not oProgram.LoadFromString( GIS_OCL_VIEWSHED_SOURCE,
               GIS_OCL_VIEWSHED_KERNEL ) then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_OPENCL_LOAD ), '', oProgram.ErrorCode
              ) ;
      oProgram.WorkDimension := 2 ;
    end ;

    if not oProgram.CheckArgument( 0 ) then begin

      pl_dem := dem.LockPixels( ext, vws.CS, False ) ;
      try
        xsiz := pl_dem.Bounds.Width + 1 ;
        ysiz := pl_dem.Bounds.Height + 1 ;
        isiz := xsiz*ysiz*sizeOf( Single ) ;
        osiz := xsiz*ysiz*sizeOf( Byte ) ;
        SetLength( ibuf, xsiz*ysiz ) ;

        i := 0 ;
        for h := pl_dem.Bounds.Top to pl_dem.Bounds.Bottom do begin
          for w := pl_dem.Bounds.Left to pl_dem.Bounds.Right do begin
            ibuf[i] := pl_dem.Grid[h,w] ;
            inc( i ) ;
          end ;
        end ;

      finally
        dem.UnlockPixels( pl_dem ) ;
      end ;

      {$IFDEF DCC}
        oProgram.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, @ibuf[0] ) ;
        oProgram.SetArgument( 3, sizeOf( Single ), @dem.NoDataValue ) ;
        oProgram.SetArgument( 5, sizeOf( Integer ), @xsiz ) ;
        oProgram.SetArgument( 6, sizeOf( Integer ), @ysiz ) ;
      {$ENDIF}
      {$IFDEF CLR}
        oProgram.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, ibuf ) ;
        oProgram.SetArgument( 3, sizeOf( Single ), dem.NoDataValue ) ;
        oProgram.SetArgument( 5, sizeOf( Integer ), xsiz ) ;
        oProgram.SetArgument( 6, sizeOf( Integer ), ysiz ) ;
      {$ENDIF}
    end
    else begin
      xsiz := iOCLxsiz ;
      ysiz := iOCLysiz ;
      osiz := xsiz*ysiz*sizeOf( Byte ) ;
    end ;

    pl_vshd := vws.LockPixels( ext, vws.CS, True ) ;
    try

      for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
        for w := pl_vshd.Bounds.Left to pl_vshd.Bounds.Right do begin
          pl_vshd.Grid[h,w] := vws.NoDataValue ;
        end ;
      end ;

      oProgram.GlobalWorkSizes[0] := 2*xsiz + 2*ysiz - 4 ;
      oProgram.GlobalWorkSizes[1] := 1 ;

      {$IFDEF DCC}
        oProgram.SetArgument(  4, sizeOf( Integer ), @curv ) ;
        oProgram.SetArgument(  7, sizeOf( Double  ), @xtoy ) ;
        oProgram.SetArgument(  8, sizeOf( Double  ), @prad ) ;
        oProgram.SetArgument(  9, sizeOf( Double  ), @FRefractivity ) ;
        oProgram.SetArgument( 10, sizeOf( Double  ), @dem_off ) ;
        oProgram.SetArgument( 11, sizeOf( Integer ), @adem ) ;
      {$ENDIF}
      {$IFDEF CLR}
        oProgram.SetArgument(  4, sizeOf( Integer ), curv ) ;
        oProgram.SetArgument(  7, sizeOf( Double  ), xtoy ) ;
        oProgram.SetArgument(  8, sizeOf( Double  ), prad ) ;
        oProgram.SetArgument(  9, sizeOf( Double  ), FRefractivity ) ;
        oProgram.SetArgument( 10, sizeOf( Double  ), dem_off ) ;
        oProgram.SetArgument( 11, sizeOf( Integer ), adem ) ;
      {$ENDIF}

      SetLength( obuf, xsiz*ysiz ) ;

      SetLength( pbuf, 3 ) ;
      psiz := 3*sizeOf( Double ) ;
      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
        obs.Loop( ext )
      do begin

      p0 := cs.ToCS3D( vws.CS, shp.GetPoint3D( 0, 0 ) ) ;

        case ObserverElevation of
          TGIS_ViewshedObserverElevation.Z    :
            p0.Z := p0.Z + g_voff( shp ) ;
          TGIS_ViewshedObserverElevation.M    :
            p0.Z := p0.M + g_voff( shp ) ;
          TGIS_ViewshedObserverElevation.ZM   :
            p0.Z := p0.Z + p0.M + g_voff( shp ) ;
          TGIS_ViewshedObserverElevation.OnDem :
            p0.Z := g_voff( shp ) ;
          TGIS_ViewshedObserverElevation.OnDemPlusM :
            p0.Z := p0.M + g_voff( shp ) ;
          TGIS_ViewshedObserverElevation.Zero :
            p0.Z := g_voff( shp ) ;
        end ;

        pbuf[0] := xsiz*( p0.X - ext.XMin )/wdth ;
        pbuf[1] := ysiz*( ext.YMax - p0.Y )/hght ;
        pbuf[2] := p0.Z ;

        {$IFDEF DCC}
          oProgram.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], osiz, nil ) ;
          oProgram.SetArgument( 2, [TGIS_OpenCLMemoryFlag.ReadOnly ], psiz, @pbuf[0] ) ;
        {$ENDIF}
        {$IFDEF CLR}
          oProgram.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], osiz, nil ) ;
          oProgram.SetArgument( 2, [TGIS_OpenCLMemoryFlag.ReadOnly ], psiz, pbuf ) ;
        {$ENDIF}

        if not oProgram.Execute then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_OPENCL_EXECUTION ), '', oProgram.ErrorCode
                ) ;

        {$IFDEF DCC}
          oProgram.ReadBuffer( 1, osiz, @obuf[0] ) ;
        {$ENDIF}
        {$IFDEF CLR}
          oProgram.ReadBuffer( 1, osiz, obuf ) ;
        {$ENDIF}

        i := -1 ;
        for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
          for w := pl_vshd.Bounds.Left to pl_vshd.Bounds.Right do begin
            inc( i ) ;
            vbuf := obuf[i] ;
            if vbuf > 0 then begin
              val := pl_vshd.Grid[h,w] ;
              if val = vws.NoDataValue then
                val := 0.0 ;
              val := val + 1.0 ;
              pl_vshd.Grid[h,w] := val ;
              vmax := Max( vmax, val ) ;
            end ;
          end ;
        end ;

      end ;

    finally
      vws.UnlockPixels( pl_vshd ) ;
    end ;

    if FFillWithZeros then begin
      pl_vshd := vws.LockPixels( vws.Extent, vws.CS, True ) ;
      try
        for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
          for w := pl_vshd.Bounds.Left to pl_vshd.Bounds.Right do begin
            if pl_vshd.Grid[h,w] = vws.NoDataValue then
              pl_vshd.Grid[h,w] := 0.0 ;
          end ;
        end ;
      finally
        vws.UnlockPixels( pl_vshd ) ;
      end ;
    end ;

    vws.MinHeight := 0.0 ;
    vws.MaxHeight := vmax ;
  end ;

  procedure TGIS_Viewshed.generateOCL_visibility ;
  const
    LOCAL_PARAM_DEM : String = 'TerrainLayer' ;
    LOCAL_PARAM_OBSERVERS : String = 'ObserversLayer' ;
    LOCAL_PARAM_VIEWSHED : String = 'OutputLayer' ;
    LOCAL_PARAM_FIELD : String = 'ObserversOffsetField' ;
  const
    LOCAL_EARTH_RAD : Double = 6378137 ;
    LOCAL_LIGHT_SPEED : Double = 299792458 ;
    LOCAL_LOCK_PROGRESS : Integer = 100 ;
    LOCAL_MAX_OBSERVERS : Integer = 50 ;
  var
    dem     : TGIS_LayerPixel ;
    obs     : TGIS_LayerVector ;
    vws     : TGIS_LayerPixel ;
    dem_off : Single ;
    obs_fld : String ;
    obs_off : Single ;

    cs      : TGIS_CSCoordinateSystem ;
    {$IFDEF DCC}
      shp   : TGIS_Shape ;
    {$ENDIF}
    ext     : TGIS_Extent ;
    pl_dem  : TGIS_LayerPixelLock ;
    pl_vshd : TGIS_LayerPixelLock ;
    p0      : TGIS_Point3D ;
    vmax    : Single ;
    w       : Integer ;
    h       : Integer ;
    bfld    : Boolean ;
    bcs     : Boolean ;
    wdth    : Double ;
    hght    : Double ;
    rad     : Double ;
    radx    : Double ;
    cref    : Double ;

    ibuf : array of Single ;
    obuf : array of Byte ;
    pbuf : array of Double ;
    xsiz : Integer ;
    ysiz : Integer ;
    isiz : Integer ;
    osiz : Integer ;
    psiz : Integer ;
    cnt  : Integer ;

    wdst : Double ;
    prad : Double ;
    vbuf : Single ;
    curv : Integer ;
    xtoy : Double ;
    adem : Integer ;
    i    : Integer ;
    id   : TGIS_Uid ;

    function check_field : Boolean ;
    var
      ff : TGIS_FieldInfo ;
      ii : Integer ;
    begin
      Result := True ;

      if IsStringEmpty( obs_fld ) then
        exit ;

      for ii := 0 to obs.Fields.Count - 1 do begin
        ff := obs.Fields[ii] ;
        if ff.Name = obs_fld then begin
          if ( ff.FieldType <> TGIS_FieldType.Number ) and
             ( ff.FieldType <> TGIS_FieldType.Float  ) then
            exit
          else
            break ;
        end ;
      end ;

      Result := False ;
    end ;

    function g_voff( const _s : TGIS_Shape ) : Single ;
    var
      val : Single ;
    begin
      val := VarToSingle( _s.GetField( obs_fld ) ) ;

      if bfld then
        Result := obs_off + val
      else
        Result := obs_off ;
    end ;

    procedure prep_radx ;
    var
      ww : Double ;
    begin
      ww := cs.Distance(
              GisPoint( p0.X, p0.Y ),
              GisPoint( p0.X + rad, p0.Y )
            ) ;
      radx := rad*FRadius/ww ;
    end ;

  begin
    dem     := FTerrainLayer ;
    obs     := FObserversLayer ;
    vws     := FOutputLayer ;
    dem_off := FTerrainOffset ;
    obs_fld := FObserversOffsetField ;
    obs_off := FObserversOffset ;

    if not assigned( dem ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DEM, 1
      ) ;

    if not assigned( obs ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_OBSERVERS, 1
      ) ;

    if not assigned( vws ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_VIEWSHED, 1
      ) ;

    if GisIsEmptyExtent( GisCommonExtent( dem.Extent, vws.Extent ) ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_VIEWSHED, 2
      ) ;

    if not check_field then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_FIELD, 1
      ) ;

    dem.Open ;
    obs.Open ;
    vws.Open ;

    vmax := 0.0 ;
    cref := 1.0 - FRefractivity ;

    bfld := not IsStringEmpty( obs_fld );
    bcs := vws.CS.EPSG <> 0 ;

    if bcs and CurvedEarth then
      curv := 0
    else
      curv := 1 ;

    if assigned( obs.Viewer ) then
      cs := obs.Viewer.Ref.CS
    else
      cs := obs.CS ;

    if cs is TGIS_CSProjectedCoordinateSystem then
      rad := TGIS_CSProjectedCoordinateSystem(
               cs ).Units.FromBase( FRadius )
    else
    if cs is TGIS_CSGeographicCoordinateSystem then
      rad := RadToDeg( FRadius/LOCAL_EARTH_RAD )
    else
      rad := FRadius ;

    if rad <= 0.0 then
      exit ;

    adem := 1 ;
    case ObserverElevation of
      TGIS_ViewshedObserverElevation.OnDem,
      TGIS_ViewshedObserverElevation.OnDemPlusM : adem := 0 ;
    end ;

    if dem.CS.EPSG <> 0 then
      wdst := dem.CS.Distance(
        GisPoint( dem.Extent.XMin, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 ),
        GisPoint( dem.Extent.XMax, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 )
      )
    else
      wdst := GisPoint2Point(
        GisPoint( dem.Extent.XMin, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 ),
        GisPoint( dem.Extent.XMax, ( dem.Extent.YMin + dem.Extent.YMax )/2.0 )
      ) ;

    ext := dem.Extent ;
    p0 := GisPoint3D(
      ( ext.XMin + ext.XMax )/2.0,
      ( ext.YMin + ext.YMax )/2.0,
      0.0
    ) ;
    if vws.CS is TGIS_CSGeographicCoordinateSystem then begin
      prep_radx ;
      ext := GisExtent( p0.X - radx, p0.Y - rad, p0.X + radx, p0.Y + rad ) ;
    end
    else begin
      radx := rad ;
      ext := GisExtent( p0.X - rad, p0.Y - rad, p0.X + rad, p0.Y + rad ) ;
    end ;
    wdth := ext.XMax - ext.XMin ;
    hght := ext.YMax - ext.YMin ;
    xtoy := rad/radx ;
    prad := xtoy*FRadius*dem.BitWidth/wdst ;

    ext := dem.Extent ;
    wdth := ext.XMax - ext.XMin ;
    hght := ext.YMax - ext.YMin ;

    if not assigned( oProgram ) then begin
      oProgram := TGIS_OpenCLProgram.Create ;
      if not oProgram.LoadFromString( GIS_OCL_VIEWSHED_SOURCE,
               GIS_OCL_VIEWSHED_KERNEL ) then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_OPENCL_LOAD ), '', oProgram.ErrorCode
              ) ;
      oProgram.WorkDimension := 2 ;
    end ;

    if not oProgram.CheckArgument( 0 ) then begin

      pl_dem := dem.LockPixels( ext, vws.CS, False ) ;
      try
        xsiz := pl_dem.Bounds.Width + 1 ;
        ysiz := pl_dem.Bounds.Height + 1 ;
        isiz := xsiz*ysiz*sizeOf( Single ) ;
        osiz := xsiz*ysiz*sizeOf( Byte ) ;
        SetLength( ibuf, xsiz*ysiz ) ;

        i := 0 ;
        for h := pl_dem.Bounds.Top to pl_dem.Bounds.Bottom do begin
          for w := pl_dem.Bounds.Left to pl_dem.Bounds.Right do begin
            ibuf[i] := pl_dem.Grid[h,w] ;
            inc( i ) ;
          end ;
        end ;
      finally
        dem.UnlockPixels( pl_dem ) ;
      end ;

      {$IFDEF DCC}
        oProgram.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, @ibuf[0] ) ;
        oProgram.SetArgument( 3, sizeOf( Single ), @dem.NoDataValue ) ;
        oProgram.SetArgument( 5, sizeOf( Integer ), @xsiz ) ;
        oProgram.SetArgument( 6, sizeOf( Integer ), @ysiz ) ;
      {$ENDIF}
      {$IFDEF CLR}
        oProgram.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, ibuf ) ;
        oProgram.SetArgument( 3, sizeOf( Single ), dem.NoDataValue ) ;
        oProgram.SetArgument( 5, sizeOf( Integer ), xsiz ) ;
        oProgram.SetArgument( 6, sizeOf( Integer ), ysiz ) ;
      {$ENDIF}
    end
    else begin
      xsiz := iOCLxsiz ;
      ysiz := iOCLysiz ;
      osiz := xsiz*ysiz*sizeOf( Byte ) ;
    end ;

    cnt := 0 ;
    for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in obs.Loop( ext )
    do begin
      inc( cnt ) ;
      id := shp.Uid ;
    end ;

    SetLength( pbuf, 3*cnt ) ;
    psiz := 3*cnt*sizeOf( Double ) ;

    i := 0 ;
    for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
      obs.Loop( ext )
    do begin

      p0 := cs.ToCS3D( vws.CS, shp.GetPoint3D( 0, 0 ) ) ;

      case ObserverElevation of
        TGIS_ViewshedObserverElevation.Z    :
          p0.Z := p0.Z + g_voff( shp ) ;
        TGIS_ViewshedObserverElevation.M    :
          p0.Z := p0.M + g_voff( shp ) ;
        TGIS_ViewshedObserverElevation.ZM   :
          p0.Z := p0.Z + p0.M + g_voff( shp ) ;
        TGIS_ViewshedObserverElevation.OnDem :
          p0.Z := g_voff( shp ) ;
        TGIS_ViewshedObserverElevation.OnDemPlusM :
          p0.Z := p0.M + g_voff( shp ) ;
        TGIS_ViewshedObserverElevation.Zero :
          p0.Z := g_voff( shp ) ;
      end ;

      pbuf[3*i  ] := xsiz*( p0.X - ext.XMin )/wdth ;
      pbuf[3*i+1] := ysiz*( ext.YMax - p0.Y )/hght ;
      pbuf[3*i+2] := p0.Z ;

      inc( i ) ;
    end ;

    oProgram.GlobalWorkSizes[0] := 2*xsiz + 2*ysiz - 4 ;
    oProgram.GlobalWorkSizes[1] := cnt ;

    {$IFDEF DCC}
      oProgram.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], osiz, nil ) ;
      oProgram.SetArgument( 2, [TGIS_OpenCLMemoryFlag.ReadOnly ], psiz, @pbuf[0] ) ;
      oProgram.SetArgument( 4, sizeOf( Integer ), @curv ) ;
      oProgram.SetArgument( 7, sizeOf( Double ), @xtoy ) ;
      oProgram.SetArgument( 8, sizeOf( Double ), @prad ) ;
      oProgram.SetArgument( 9, sizeOf( Double ), @FRefractivity ) ;
      oProgram.SetArgument( 10, sizeOf( Double ), @dem_off ) ;
      oProgram.SetArgument( 11, sizeOf( Integer ), @adem ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oProgram.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], osiz, nil ) ;
      oProgram.SetArgument( 2, [TGIS_OpenCLMemoryFlag.ReadOnly ], psiz, pbuf ) ;
      oProgram.SetArgument( 4, sizeOf( Integer ), curv ) ;
      oProgram.SetArgument( 7, sizeOf( Double ), xtoy ) ;
      oProgram.SetArgument( 8, sizeOf( Double ), prad ) ;
      oProgram.SetArgument( 9, sizeOf( Double ), FRefractivity ) ;
      oProgram.SetArgument( 10, sizeOf( Double ), dem_off ) ;
      oProgram.SetArgument( 11, sizeOf( Integer ), adem ) ;
    {$ENDIF}

    if not oProgram.Execute then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_OPENCL_EXECUTION ), '', oProgram.ErrorCode
            ) ;

    SetLength( obuf, xsiz*ysiz ) ;
    {$IFDEF DCC}
      oProgram.ReadBuffer( 1, osiz, @obuf[0] ) ;
    {$ENDIF}
    {$IFDEF CLR}
      oProgram.ReadBuffer( 1, osiz, obuf ) ;
    {$ENDIF}

    pl_vshd := vws.LockPixels( ext, vws.CS, True ) ;
    try
      i := -1 ;
      for h := pl_vshd.Bounds.Top to pl_vshd.Bounds.Bottom do begin
        for w := pl_vshd.Bounds.Left to pl_vshd.Bounds.Right do begin
          inc( i ) ;
          vbuf := obuf[i] ;
          if FFillWithZeros then
            pl_vshd.Grid[h,w] := 0.0
          else
            pl_vshd.Grid[h,w] := vws.NoDataValue ;
          if vbuf > 0 then
            pl_vshd.Grid[h,w] := 1.0 ;
        end ;
      end ;
    finally
      vws.UnlockPixels( pl_vshd ) ;
    end ;

    vws.MinHeight := 0.0 ;
    vws.MaxHeight := 1.0 ;
  end ;
{$ENDIF}

  procedure TGIS_Viewshed.generateFresnelCPU ;
  const
    LOCAL_PARAM_DEM          : String  = 'TerrainLayer' ;
    LOCAL_PARAM_TRANSMITTERS : String  = 'ObserversLayer' ;
    LOCAL_PARAM_CLEARANCE    : String  = 'OutputLayer' ;
    LOCAL_PARAM_ZONE_NUMBER  : String  = 'FresnelZoneNumber' ;
    LOCAL_PARAM_FIELD        : String  = 'ObserversOffsetField' ;
    LOCAL_EARTH_RAD          : Double  = 6378137 ;
    LOCAL_EARTH_RAD_RF       : Double  = 8504000 ;
    LOCAL_LIGHT_SPEED        : Double  = 299792458 ;
    LOCAL_LOCK_PROGRESS      : Integer = 100 ;
    LOCAL_MAX_OBSERVERS      : Integer = 50 ;
  var
    dem     : TGIS_LayerPixel ;
    trs     : TGIS_LayerVector ;
    clr     : TGIS_LayerPixel ;
    dem_off : Single ;
    trs_fld : String ;
    trs_off : Single ;

    cs      : TGIS_CSCoordinateSystem ;
    {$IFDEF DCC}
      shp   : TGIS_Shape ;
    {$ENDIF}
    zone    : Integer ;
    ext     : TGIS_Extent ;
    pl_dem  : TGIS_LayerPixelLock ;
    pl_clrc : TGIS_LayerPixelLock ;
    sgl     : T_singleGridLock ;
    p       : TPoint ;
    z       : Double ;
    p0      : TGIS_Point3D ;
    p1      : TGIS_Point ;
    val     : Single ;
    prv     : Single ;
    w       : Integer ;
    h       : Integer ;
    pr      : TGIS_Point ;
    ax      : Double ;
    bx      : Double ;
    ay      : Double ;
    by      : Double ;
    tmax    : Integer ;
    bfld    : Boolean ;
    bcs     : Boolean ;
    wdth    : Double ;
    hght    : Double ;
    rad     : Double ;
    radx    : Double ;
    // trace ray
    dist    : Double ;
    dd      : Double ;
    di      : Double ;
    dk      : Double ;
    ix      : array of Integer ;
    iy      : array of Integer ;
    vz      : array of Double ;
    az      : array of Double ;
    bz      : array of Double ;
    ca      : array of Double ;
    clrnc   : array of Single ;
    wvl     : Double ;
    fzr     : Double ;
    tmp     : Double ;
    // progress
    poi     : Integer ;
    cnt     : Integer ;
    total   : Int64 ;
    abrt    : Boolean ;
    id      : TGIS_Uid ;

    function check_field : Boolean ;
    var
      ff : TGIS_FieldInfo ;
      ii : Integer ;
    begin
      Result := True ;

      if IsStringEmpty( trs_fld ) then
        exit ;

      for ii := 0 to trs.Fields.Count - 1 do begin
        ff := trs.Fields[ii] ;
        if ff.Name = trs_fld then begin
          if ( ff.FieldType <> TGIS_FieldType.Number ) and
             ( ff.FieldType <> TGIS_FieldType.Float  ) then
            exit
          else
            break ;
        end ;
      end ;

      Result := False ;
    end ;

    function g_voff( const _s : TGIS_Shape ) : Single ;
    var
      val : Single ;
    begin
      val := VarToSingle( _s.GetField( trs_fld ) ) ;

      if bfld then
        Result := trs_off + val
      else
        Result := trs_off ;
    end ;

    procedure prep_radx ;
    var
      ww : Double ;
    begin
      ww := clr.CS.Distance(
              GisPoint( p0.X, p0.Y ),
              GisPoint( p0.X + rad, p0.Y )
            ) ;
      radx := rad*FRadius/ww ;
    end ;

    function prep_coeff : Boolean ;
    var
      xx : Double ;
      yy : Double ;
    begin
      Result := False ;

      xx := Abs( p0.X - p1.X ) ;
      yy := Abs( p0.Y - p1.Y ) ;

      if xx >= yy then
        tmax := RoundS( xx*pl_dem.Bounds.Width/wdth )
      else
        tmax := RoundS( yy*pl_dem.Bounds.Height/hght ) ;

      // correction for rounding problems
      inc( tmax ) ;

      if tmax < 2 then
        exit ;

      ax := ( p1.X - p0.X )/tmax ;
      bx := p0.X ;
      ay := ( p1.Y - p0.Y )/tmax ;
      by := p0.Y ;

      Result := True ;
    end ;

    function trace_ray( const _w : Integer ; const _h : Integer ) : Boolean ;
    var
      ii : Integer ;
      kk : Integer ;
      dt : Double ;
    begin
      Result := False ;

      abrt := busyEventManager.PushEvent ;
      if abrt then begin
        clr.MinHeight := 0.0 ;
        clr.MaxHeight := 100.0 ;
        exit ;
      end ;

      p1 := pl_clrc.RasterToMap( Point( _w, _h ), clr.CS ) ;
      dist := clr.CS.Distance( GisPoint( p0.X, p0.Y ), p1 ) ;

      if not prep_coeff then
        exit ;

      dd := dist/tmax ;

      pr := GisPoint( 0.1*ax + bx, 0.1*ay + by ) ;
      p := pl_clrc.MapToRaster( pr, clr.CS ) ;
      sgl.Mark( p.X, p.Y, 0.0 ) ;

      SetLength( ix, tmax + 1 ) ;
      SetLength( iy, tmax + 1 ) ;
      SetLength( vz, tmax + 1 ) ;
      SetLength( az, tmax + 1 ) ;
      SetLength( bz, tmax + 1 ) ;
      SetLength( ca, tmax + 1 ) ;
      for ii := 1 to tmax do begin
        di := ii*dd ;
        if di > FRadius then begin
          tmax := ii - 1 ;
          break ;
        end ;
        pr := GisPoint( ax*ii + bx, ay*ii + by ) ;
        p := pl_dem.MapToRaster( pr, clr.CS ) ;
        if ( p.X < pl_dem.Bounds.Left ) or ( p.X > pl_dem.Bounds.Right ) or
           ( p.Y < pl_dem.Bounds.Top ) or ( p.Y > pl_dem.Bounds.Bottom ) then
          continue ;
        z := pl_dem.Grid[p.Y,p.X] ;
        if z = dem.NoDataValue then begin
          vz[ii] := GIS_GRID_NOVALUE ;
          continue ;
        end ;
        z := z + dem_off ;
        if bcs and CurvedEarth then
          z := z - ( LOCAL_EARTH_RAD_RF/Cos( di/LOCAL_EARTH_RAD_RF ) -
                     LOCAL_EARTH_RAD_RF ) ;

        p := pl_clrc.MapToRaster( pr, clr.CS ) ;
        if ( p.X < pl_clrc.Bounds.Left ) or ( p.X > pl_clrc.Bounds.Right ) or
           ( p.Y < pl_clrc.Bounds.Top ) or ( p.Y > pl_clrc.Bounds.Bottom ) then
          continue ;
        ix[ii] := p.X ;
        iy[ii] := p.Y ;
        vz[ii] := z ;
        az[ii] := ( z - p0.Z )/ii ;
        bz[ii] := p0.Z ;
        ca[ii] := 1.0/Cos( ArcTan2( p0.Z - z, di ) ) ;
      end ;

      SetLength( clrnc, tmax + 1 ) ;
      clrnc[1] := 100.0 ;
      for ii := 2 to tmax do
        clrnc[ii] := GIS_MAX_SINGLE ;

      for ii := 1 to tmax do begin

        if vz[ii] = GIS_GRID_NOVALUE then
          continue ;

        di := ii*dd ;
        if di > FRadius then
          break ;

        for kk := ii to tmax do begin

          if kk = ii then
            continue ;

          if clrnc[kk] = GIS_GRID_NOVALUE then
            continue ;

          z := az[kk]*ii + bz[kk] ;

          dk := kk*dd ;

          dt := di*ca[kk] ;
          dk := dk*ca[kk] ;

          fzr := wvl*Sqrt( dt*( dk - dt )/dk ) ;

          if z + fzr < vz[ii] then begin
            clrnc[kk] := GIS_GRID_NOVALUE ;
            continue ;
          end ;

          if fzr = 0.0 then
            tmp := 100.0
          else begin
            tmp := 100.0*( vz[ii] - z + fzr )/( 2.0*fzr ) ;
            if tmp > 0.0 then
              tmp := 100.0 - tmp
            else
              tmp := 100.0 ;
          end ;

          if tmp < clrnc[kk] then
            clrnc[kk] := tmp ;

        end ;

      end ;

      for ii := 1 to tmax do begin
        if vz[ii] = GIS_GRID_NOVALUE then
          continue ;
        if sgl.Value( ix[ii], iy[ii] ) < clrnc[ii] then
          sgl.Mark( ix[ii], iy[ii], clrnc[ii] ) ;
      end ;

      Result := True ;
    end ;

  begin
    dem     := FTerrainLayer ;
    trs     := FObserversLayer ;
    clr     := FOutputLayer ;
    zone    := FFresnelZoneNumber ;
    dem_off := FTerrainOffset ;
    trs_fld := FObserversOffsetField ;
    trs_off := FObserversOffset ;

    if not assigned( dem ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_DEM, 1
      ) ;

    if not assigned( trs ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_TRANSMITTERS, 1
      ) ;

    if not assigned( clr ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_CLEARANCE, 1
      ) ;

    if GisIsEmptyExtent(
         GisCommonExtent( dem.Extent, clr.Extent )
       ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_CLEARANCE, 2
      ) ;

    if FFresnelZoneNumber < 1 then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_ZONE_NUMBER, 1
      ) ;

    if not check_field then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), LOCAL_PARAM_FIELD, 1
      ) ;

    dem.Open ;
    trs.Open ;
    clr.Open ;

    zone := FFresnelZoneNumber ;

    abrt := False ;

    bfld := not IsStringEmpty( trs_fld );
    bcs := clr.CS.EPSG <> 0 ;

    if assigned( trs.Viewer ) then
      cs := trs.Viewer.Ref.CS
    else
      cs := trs.CS ;

    if clr.CS is TGIS_CSProjectedCoordinateSystem then
      rad := TGIS_CSProjectedCoordinateSystem(
               clr.CS ).Units.FromBase( FRadius )
    else
    if clr.CS is TGIS_CSGeographicCoordinateSystem then
      rad := RadToDeg( FRadius/LOCAL_EARTH_RAD )
    else
      rad := FRadius ;

    if rad <= 0.0 then
      exit ;

    wvl := Sqrt( zone*LOCAL_LIGHT_SPEED/FFrequency ) ;

    cnt := 0 ;
    for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in trs.Loop(
                  GisCommonExtent( trs.Extent, dem.Extent )
               )
    do begin
      inc( cnt ) ;
      id := shp.Uid ;
      if cnt >= LOCAL_MAX_OBSERVERS then
        break ;
    end ;

    busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_VIEWSHED_FRESNEL ), cnt ) ;
    try
      poi := -1 ;
      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in trs.Loop(
                    GisCommonExtent( trs.Extent, dem.Extent )
                 )
      do begin
        inc( poi ) ;
        if poi > LOCAL_MAX_OBSERVERS then
          break ;

        p0 := cs.ToCS3D( clr.CS, shp.GetPoint3D( 0, 0 ) ) ;

        if clr.CS is TGIS_CSGeographicCoordinateSystem then begin
          prep_radx ;
          ext := GisExtent( p0.X - radx, p0.Y - rad, p0.X + radx, p0.Y + rad ) ;
        end
        else
          ext := GisExtent( p0.X - rad, p0.Y - rad, p0.X + rad, p0.Y + rad ) ;
        ext := GisCommonExtent( ext, clr.Extent ) ;
        wdth := ext.XMax - ext.XMin ;
        hght := ext.YMax - ext.YMin ;

        pl_dem := dem.LockPixels( ext, clr.CS, False ) ;
        pl_clrc := clr.LockPixels( ext, clr.CS, True ) ;
        sgl := T_singleGridLock.Create( pl_clrc ) ;
        total := 2 * ( pl_clrc.Bounds.Width + pl_clrc.Bounds.Height ) ;
        busyEventManager.StartEvent(
          _rsrc( GIS_RS_BUSY_VIEWSHED_TRACERAY ), total
        ) ;
        try

          case ObserverElevation of
            TGIS_ViewshedObserverElevation.Z    :
              p0.Z := p0.Z + g_voff( shp ) ;
            TGIS_ViewshedObserverElevation.M    :
              p0.Z := p0.M + g_voff( shp ) ;
            TGIS_ViewshedObserverElevation.ZM   :
              p0.Z := p0.Z + p0.M + g_voff( shp ) ;
            TGIS_ViewshedObserverElevation.OnDem :
              begin
                p := pl_dem.MapToRaster( GisPoint( p0.X, p0.Y ), clr.CS ) ;
                z := pl_dem.Grid[p.Y,p.X] ;
                p0.Z := z + g_voff( shp ) ;
              end ;
            TGIS_ViewshedObserverElevation.OnDemPlusM :
              begin
                p := pl_dem.MapToRaster( GisPoint( p0.X, p0.Y ), clr.CS ) ;
                z := pl_dem.Grid[p.Y,p.X] ;
                p0.Z := z + p0.M + g_voff( shp ) ;
              end ;
            TGIS_ViewshedObserverElevation.Zero :
              p0.Z := g_voff( shp ) ;
          end ;

          // mark the cell right under the observer
          pr := GisPoint( p0.X, p0.Y ) ;
          p := pl_clrc.MapToRaster( pr, clr.CS ) ;
          sgl.Mark( p.X, p.Y, 100.0 ) ;

          // trace rays to the left extent border
          w := pl_clrc.Bounds.Left ;
          for h := pl_clrc.Bounds.Top to pl_clrc.Bounds.Bottom do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          // trace rays to the right extent border
          w := pl_clrc.Bounds.Right ;
          for h := pl_clrc.Bounds.Top to pl_clrc.Bounds.Bottom do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          // trace rays to the top extent border
          h := pl_clrc.Bounds.Top ;
          for w := pl_clrc.Bounds.Left + 1 to pl_clrc.Bounds.Right - 1 do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          // trace rays to the bottom extent border
          h := pl_clrc.Bounds.Bottom ;
          for w := pl_clrc.Bounds.Left + 1 to pl_clrc.Bounds.Right - 1 do begin
            if not trace_ray( w, h ) then
              continue ;
          end ;

          for h := pl_clrc.Bounds.Top to pl_clrc.Bounds.Bottom do begin
            for w := pl_clrc.Bounds.Left to pl_clrc.Bounds.Right do begin
              val := sgl.Value( w, h ) ;
              if val <> GIS_GRID_NOVALUE then begin
                prv := pl_clrc.Grid[h,w] ;
                if val > prv then
                  pl_clrc.Grid[h,w] := val ;
              end ;
            end ;
          end ;

        finally
          dem.UnlockPixels( pl_dem ) ;
          clr.UnlockPixels( pl_clrc ) ;
          FreeObject( sgl ) ;
          busyEventManager.EndEvent ;
        end ;
      end ;

      if FFillWithZeros then begin
        pl_clrc := clr.LockPixels(
          clr.Extent, clr.CS, True
        ) ;
        try
          for h := pl_clrc.Bounds.Top to pl_clrc.Bounds.Bottom do begin
            for w := pl_clrc.Bounds.Left to pl_clrc.Bounds.Right do begin
              if pl_clrc.Grid[h,w] = GIS_GRID_NOVALUE then
                pl_clrc.Grid[h,w] := 0.0 ;
            end ;
          end ;
        finally
          clr.UnlockPixels( pl_clrc ) ;
        end ;
      end ;

      clr.MinHeight := 0.0 ;
      clr.MaxHeight := 100.0 ;

    finally
      busyEventManager.EndEvent ;
    end ;
  end ;

  procedure TGIS_Viewshed.Generate(
    const _dem                : TGIS_LayerPixel ;
    const _observers          : TGIS_LayerVector ;
    const _viewshed           : TGIS_LayerPixel ;
    const _above_ground_level : TGIS_LayerPixel ;
    const _offset_dem         : Single ;
    const _offset_field       : String ;
    const _offset_observer    : Single
  ) ;
  begin
    FTerrainLayer := _dem ;
    FObserversLayer := _observers ;
    FOutputLayer := _viewshed ;
    FAboveGroundLevelLayer := _above_ground_level ;
    FTerrainOffset := _offset_dem ;
    FObserversOffsetField := _offset_field ;
    FObserversOffset := _offset_observer ;

    if not assigned( FAboveGroundLevelLayer ) then
      Generate
    else
      generateCPU ;
  end ;

  procedure TGIS_Viewshed.Generate(
    const _dem                : TGIS_LayerPixel ;
    const _observers          : TGIS_LayerVector ;
    const _viewshed           : TGIS_LayerPixel ;
    const _offset_dem         : Single ;
    const _offset_field       : String ;
    const _offset_observer    : Single
  ) ;
  begin
    Generate(
      _dem, _observers, _viewshed, nil,
      _offset_dem, _offset_field, _offset_observer
    ) ;
  end ;

  procedure TGIS_Viewshed.Generate ;
  begin
    {$IFDEF JAVA OR ISLAND}
      generateCPU ;
    {$ELSE}
      if GisOpenCLEngine.Available and GisOpenCLEngine.Enabled and
         ( not assigned( FAboveGroundLevelLayer ) ) then begin
        if ViewshedOutput = TGIS_ViewshedOutput.Visibility then
          generateOCL_visibility
        else
          generateOCL_number ;
      end
      else
        generateCPU ;
    {$ENDIF}
  end ;

  procedure TGIS_Viewshed.GenerateFresnel(
    const _dem                : TGIS_LayerPixel ;
    const _transmitters       : TGIS_LayerVector ;
    const _clearance          : TGIS_LayerPixel ;
    const _zone_number        : Integer ;
    const _offset_dem         : Single ;
    const _offset_field       : String ;
    const _offset_transmitter : Single
  ) ;
  begin
    FTerrainLayer := _dem ;
    FObserversLayer := _transmitters ;
    FOutputLayer := _clearance ;
    FFresnelZoneNumber := _zone_number ;
    FTerrainOffset := _offset_dem ;
    FObserversOffsetField := _offset_field ;
    FObserversOffset := _offset_transmitter ;

    GenerateFresnel ;
  end ;

  procedure TGIS_Viewshed.GenerateFresnel ;
  begin
    generateFresnelCPU ;
  end ;
{$ENDREGION}

end.
