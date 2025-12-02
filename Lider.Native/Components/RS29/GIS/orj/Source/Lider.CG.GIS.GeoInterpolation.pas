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
  Library of interpolation methods.
}

{$IFDEF DCC}
  unit GisInterpolation ;
  {$HPPEMIT '#pragma link "GisInterpolation"'}
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
    System.Types,
    System.Classes,
    System.Generics.Collections,

    GisClasses,
    GisLayerVector,
    GisLayerPixel,
    GisMatrix ,
    GisRtl,
    GisTypes,
    GisUtils ;
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
  Unit_GisInterpolation = class
    public
      class procedure SelfRegisterPipeline ;
  end ;

  /// <summary>
  ///   Base class for methods for path interpolation.
  /// </summary>
  TGIS_InterpolatedPath = {$IFDEF OXYGENE} public abstract {$ENDIF}
                          class( TGIS_Object )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      xLineA           : Double ;
      xLineB           : Double ;
      yLineA           : Double ;
      yLineB           : Double ;
      zLineA           : Double ;
      zLineB           : Double ;
      rotMul           : Integer ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FAllowDuplicates : Boolean ;
      FClosed          : Boolean ;
      FSpan            : Double ;
      FLooped          : Boolean ;
      FEqualizeInt     : Boolean ;
      FLevelInt        : Boolean ;
      FRotation        : Boolean ;
      FTolerance       : Double ;
    protected
      /// <summary>
      ///   Storage of interpolation points.
      /// </summary>
      points     : TList<TGIS_Point3D> ;
      /// <summary>
      ///   Storage of intervals between interpolation points.
      /// </summary>
      intervals  : TList<Double> ;
      /// <summary>
      ///   Storage of indexes of duplicated points.
      /// </summary>
      duplicates : TList<Integer> ;
      /// <summary>
      ///   Sum of all the distances between subsequent interpolation points.
      /// </summary>
      length       : Double ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function  fget_Count        : Integer ;
      function  fget_Closed       : Boolean ;
      function  fget_Span         : Double ;
      procedure fset_Span         ( const _val : Double
                                  ) ;
      function  fget_Looped       : Boolean ;
      procedure fset_Looped       ( const _val : Boolean
                                  ) ;
      function  fget_LastInterval : Double ;
      procedure fset_LastInterval
                                  ( const _val : Double
                                  ) ;
      function  fget_EqualizeInt  : Boolean ;
      procedure fset_EqualizeInt  ( const _val : Boolean
                                  ) ;
      function  fget_LevelInt     : Boolean ;
      procedure fset_LevelInt     ( const _val : Boolean
                                  ) ;
      function  fget_Rotation     : Boolean ;
      procedure fset_Rotation     ( const _val : Boolean
                                  ) ;
      function  fget_Tolerance    : Double ;
      procedure fset_Tolerance    ( const _val : Double
                                  ) ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Prepares interpolation parameters.
      /// </summary>
      procedure prepare         ; virtual ; abstract ;
      /// <summary>
      ///   Interpolates a new point.
      /// </summary>
      /// <param name="_t">
      ///   argument of the parametrization; should be between 0 and length
      /// </param>
      /// <returns>
      ///   interpolated point
      /// </returns>
      function  interpolate     ( const _t    : Double
                                ) : TGIS_Point3D ; virtual ; abstract ;
      /// <summary>
      ///   Returns the index of the next interpolation point for a
      ///   distance from the beginning of the path.
      /// </summary>
      /// <param name="_t">
      ///   distance from the beginning of the path
      /// </param>
      /// <param name="_idx">
      ///   index of the next interpolation point
      /// </param>
      /// <param name="_pdst">
      ///   distance to the previous interpolation point
      /// </param>
      /// <param name="_ndst">
      ///   distance to the next interpolation point
      /// </param>
      procedure nextPointByDist ( const _t    : Double ;
                                    out _idx  : Integer ;
                                    out _pdst : Double ;
                                    out _ndst : Double
                                ) ;
    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Adds an interpolation point.
      /// </summary>
      /// <param name="_pt">
      ///   point to be added
      /// </param>
      /// <returns>
      ///   True if AllowDuplicates is True and the previous point is identical
      /// </returns>
      function  AddPoint     ( const _pt  : TGIS_Point3D
                             ) : Boolean ;
      /// <summary>
      ///   Indicates that all the interpolation points were added and prepares
      ///   the interpolation function; no changes can be made after Close.
      /// </summary>
      procedure Close        ; overload ;
      /// <summary>
      ///   Indicates that all the interpolation points were added and prepares
      ///   the interpolation function; no changes can be made after Close.
      /// </summary>
      /// <param name="_sync">
      ///   path to synchronize with
      /// </param>
      /// <remarks>
      ///   <note type="caution">
      ///     Only for internal use of TatukGIS.
      ///   </note>
      /// </remarks>
      procedure Close        ( const _sync : TGIS_InterpolatedPath
                             ) ; overload ;
      /// <summary>
      ///   Returns the index of the next interpolation point for an
      ///   argument of the parametrization.
      /// </summary>
      /// <param name="_t">
      ///   argument of the parametrization; should be between 0 and Span
      /// </param>
      /// <returns>
      ///   index of the next interpolation point
      /// </returns>
      function  NextPoint    ( const _t   : Double
                             ) : Integer ;
      /// <summary>
      ///   Interpolates a point.
      /// </summary>
      /// <param name="_t">
      ///   argument of the parametrization; should be between 0 and Span
      /// </param>
      /// <returns>
      ///   interpolated point
      /// </returns>
      function  Calculate    ( const _t   : Double
                             ) : TGIS_Point3D ;
      /// <summary>
      ///   Deletes the last added point.
      /// </summary>
      procedure DeleteLast   ;
      /// <summary>
      ///   Clears the list of points.
      /// </summary>
      procedure Clear        ;
      /// <summary>
      ///   Gets the intervals and the total length of the path.
      /// </summary>
      /// <param name="_list">
      ///   list of intervals
      /// </param>
      /// <param name="_len">
      ///   total length
      /// </param>
      /// <remarks>
      ///   <note type="caution">
      ///     Only for internal use of TatukGIS.
      ///   </note>
      /// </remarks>
      procedure GetIntervals ( var _list : TGIS_ListOfDoubles ;
                               var _len  : Double
                              ) ;

    public
      /// <summary>
      ///   Allow subsequent points to be identical.
      /// </summary>
      property  AllowDuplicates : Boolean
                                  read  FAllowDuplicates
                                  write FAllowDuplicates ;
      /// <summary>
      ///   The total number of sample points.
      /// </summary>
      property  Count           : Integer
                                  read  fget_Count ;
      /// <summary>
      ///   If True then interpolation was performed
      ///   and no more points can be added.
      /// </summary>
      property  Closed          : Boolean
                                  read  fget_Closed ;
      /// <summary>
      ///   The span of parametrization argument.
      /// </summary>
      property  Span            : Double
                                  read  fget_Span
                                  write fset_Span ;
      /// <summary>
      ///   True if the path should be a loop (closed path).
      /// </summary>
      property  Looped          : Boolean
                                  read  fget_Looped
                                  write fset_Looped ;
      /// <summary>
      ///   The last interval.
      /// </summary>
      property  LastInterval    : Double
                                  read  fget_LastInterval
                                  write fset_LastInterval ;
      /// <summary>
      ///   True if the distances between the sample points should be equal.
      /// </summary>
      property  EqualizeIntervals : Boolean
                                    read  fget_EqualizeInt
                                    write fset_EqualizeInt ;
      /// <summary>
      ///   True if the distances between the sample points should optimized
      ///   (leveled).
      /// </summary>
      property  LevelIntervals  : Boolean
                                  read  fget_LevelInt
                                  write fset_LevelInt ;
      /// <summary>
      ///   True if the sample points represent rotation.
      /// </summary>
      /// <remarks>
      ///   <note type="caution">
      ///     Only for internal use of TatukGIS.
      ///   </note>
      /// </remarks>
      property  Rotation        : Boolean
                                  read  fget_Rotation
                                  write fset_Rotation ;
      /// <summary>
      ///   Minimum distance between sample points to be treated as separate
      ///   points.
      /// </summary>
      property  Tolerance       : Double
                                  read  fget_Tolerance
                                  write fset_Tolerance ;
  end ;


  /// <summary>
  ///   Implementation of linear interpolation for path generation in 3D.
  /// </summary>
  TGIS_LinearPath = {$IFDEF OXYGENE} public {$ENDIF}
                    class( TGIS_InterpolatedPath )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      xParamsA  : TGIS_Vector ;
      xParamsB  : TGIS_Vector ;
      yParamsA  : TGIS_Vector ;
      yParamsB  : TGIS_Vector ;
      zParamsA  : TGIS_Vector ;
      zParamsB  : TGIS_Vector ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      procedure prepare     ; override ;
      /// <inheritdoc/>
      function  interpolate ( const _t : Double
                            ) : TGIS_Point3D ; override ;
  end ;


  /// <summary>
  ///   Implementation of cubic splines for path generation in 3D.
  /// </summary>
  TGIS_CubicSplines = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_InterpolatedPath )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      xParamsM  : TGIS_Vector ;
      xParamsA  : TGIS_Vector ;
      xParamsB  : TGIS_Vector ;
      yParamsM  : TGIS_Vector ;
      yParamsA  : TGIS_Vector ;
      yParamsB  : TGIS_Vector ;
      zParamsM  : TGIS_Vector ;
      zParamsA  : TGIS_Vector ;
      zParamsB  : TGIS_Vector ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      procedure prepare     ; override ;
      /// <inheritdoc/>
      function  interpolate ( const _t : Double
                            ) : TGIS_Point3D ; override ;
  end ;

  /// <summary>
  ///   Specifies coordinate used as data input for interpolation process.
  /// </summary>
  TGIS_VectorToGridCoordinate = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Forces value of 1.0; for the purpose of concentration maps.
    /// </summary>
    None,
    /// <summary>
    ///   The Z coordinate.
    /// </summary>
    Z,
    /// <summary>
    ///   The M coordinate.
    /// </summary>
    M,
    /// <summary>
    ///   The sum of the Z and the M coordinate.
    /// </summary>
    ZM
  ) ;

  /// <summary>
  ///   Abstract class for all classes which implement grid generation from
  ///   vector data.
  /// </summary>
  TGIS_VectorToGridAbstract = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_BaseObjectDisposable )
    private
      FCoordinate      : TGIS_VectorToGridCoordinate ;
      FUseDefVal       : Boolean ;
      FDefVal          : Single ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;

    private
      function  isNumericField      ( const _fld : String ;
                                      const _lv  : TGIS_LayerVector
                                    ) : Boolean ;
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
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   Populates a pixel (grid) layer with values resulting from
      ///   computations based on the set of sample points from the vector
      ///   layer.
      /// </summary>
      /// <param name="_src">
      ///   source layer which contains the sample points
      /// </param>
      /// <param name="_srcext">
      ///   extent of interest on the source layer
      /// </param>
      /// <param name="_srcfld">
      ///   the name of the numeric field which contains the values to be used
      ///   for interpolation; if empty then the coordinate defined by the
      ///   Coordinate property will be used for interpolation
      /// </param>
      /// <param name="_dst">
      ///   destination grid layer
      /// </param>
      /// <param name="_dstext">
      ///   extent on the destination layer to be populated with
      ///   interpolated data
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ( const _src    : TGIS_LayerVector ;
                           const _srcext : TGIS_Extent ;
                           const _srcfld : String ;
                           const _dst    : TGIS_LayerPixel ;
                           const _dstext : TGIS_Extent
                         ) ; virtual ;
    public
      /// <summary>
      ///   Defines which coordinate is taken as interpolation value if the
      ///   interpolation is not based on an attribute field; default is Z.
      /// </summary>
      property Coordinate : TGIS_VectorToGridCoordinate
                            read  FCoordinate
                            write FCoordinate ;
      /// <summary>
      ///   If true, then each grid cell for which the interpolated value
      ///   cannot be computed will be set to DefaultValue.
      /// </summary>
      property UseDefaultValue : Boolean
                                 read  FUseDefVal
                                 write FUseDefVal ;
      /// <summary>
      ///   If UseDefaultValue is true, then this value will be set for each
      ///   grid cell for which the interpolated value cannot be computed.
      /// </summary>
      property DefaultValue    : Single
                                 read  FDefVal
                                 write FDefVal ;
    published // events
      /// <summary>
      ///   Event fired upon progress of the interpolation process.
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


  /// <summary>
  ///   Abstract stub class for internal implementation of structured shape
  ///   list used in vector-to-grid computations.
  /// </summary>
  TGIS_ShapeListStub = {$IFDEF OXYGENE} public abstract {$ENDIF}
                       class( TGIS_Object )
    protected
      function  fget_Count : Integer ; virtual ; abstract ;
      function  fget_Point ( const _idx : Integer
                           ) : TGIS_Point ; virtual ; abstract ;
      function  fget_Value ( const _idx : Integer
                           ) : Double ; virtual ; abstract ;
    public
      /// <summary>
      ///   Number of points in the list.
      /// </summary>
      property Count : Integer
                       read  fget_Count ;
      /// <summary>
      ///   Access to a specific shape.
      /// </summary>
      /// <param name="_idx">
      ///   index of the shape in the list
      /// </param>
      property Point[const _idx : Integer] : TGIS_Point
                       read fget_Point ; default ;
      /// <summary>
      ///   Access to a sample value of a specific shape.
      /// </summary>
      /// <param name="_idx">
      ///   index of the shape in the list
      /// </param>
      property Value[const _idx : Integer] : Double
                       read fget_Value ;
  end ;


  /// <summary>
  ///   Abstract class for all semivariance models.
  /// </summary>
  TGIS_SemivarianceAbstract = {$IFDEF OXYGENE}public abstract {$ENDIF}
                              class( TGIS_BaseObjectDisposable )

    private
      FNugget : Double ;
    private
      function  fget_Nugget : Double ;
    protected
      procedure fset_Nugget ( const _val : Double
                            ) ; virtual ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    public
      /// <summary>
      ///   If necessary, performs preliminary computations; is called before
      ///   Calculate for each point set.
      /// </summary>
      /// <param name="_list">
      ///   list of shapes necessary to perform computations
      /// </param>
      /// <returns>
      ///   True if the computations succeeded; False otherwise
      /// </returns>
      function  Prepare   ( const _list : TGIS_ShapeListStub
                          ) : Boolean ; virtual ;
      /// <summary>
      ///   Calculates the value of semivariance for a given distance.
      /// </summary>
      /// <param name="_dist">
      ///   distance between the sample and the data point
      /// </param>
      /// <returns>
      ///   value of semivariance for a given distance
      /// </returns>
      function  Calculate ( const _dist : Double
                          ) : Double ; virtual ; abstract ;
    public
      /// <summary>
      ///   The nugget of semivariance.
      /// </summary>
      property Nugget : Double
                        read  fget_Nugget
                        write fset_Nugget ;
  end ;

  /// <summary>
  ///   Represents the power-law semivariance model.
  /// </summary>
  TGIS_SemivariancePowerLaw = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_SemivarianceAbstract )
    private
      dAlpha    : Double ;
    private
      FExponent : Double ;
    private
      function  fget_Exponent : Double ;
      procedure fset_Exponent ( const _val : Double
                              ) ;
    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      function  Prepare   ( const _list : TGIS_ShapeListStub
                          ) : Boolean ; override ;
      /// <inheritdoc/>
      function  Calculate ( const _dist : Double
                          ) : Double ; override ;
    public
      /// <summary>
      ///   Power-law exponent; should be greater than or equal to 1 and less
      ///   than 2; default is 1.5.
      /// </summary>
      property Exponent : Double
                          read  fget_Exponent
                          write fset_Exponent ;
  end ;

  /// <summary>
  ///   Abstract class for all semivariance models with range.
  /// </summary>
  TGIS_SemivarianceWithRange = {$IFDEF OXYGENE} public abstract {$ENDIF}
                               class( TGIS_SemivarianceAbstract )
    private
      FRange : Double ;
    private
      function  fget_Range  : Double ;
      procedure fset_Range  ( const _val : Double
                            ) ;
    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Range of the semivariance formula.
      /// </summary>
      property Range : Double
                       read  fget_Range
                       write fset_Range ;
  end ;

  /// <summary>
  ///   Represents the exponential semivariance model.
  /// </summary>
  TGIS_SemivarianceExponential = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_SemivarianceWithRange )

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      function  Calculate ( const _dist : Double
                          ) : Double ; override ;
  end ;

  /// <summary>
  ///   Represents the gaussian semivariance model.
  /// </summary>
  TGIS_SemivarianceGaussian = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_SemivarianceWithRange )

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      function  Calculate ( const _dist : Double
                          ) : Double ; override ;
  end ;

  /// <summary>
  ///   Abstract class for all semivariance models with range and sill.
  /// </summary>
  TGIS_SemivarianceWithSill = {$IFDEF OXYGENE}public abstract {$ENDIF}
                              class( TGIS_SemivarianceWithRange )
    protected
      /// <summary>
      ///   Actual sill (reduced by the nugget) used in computations.
      /// </summary>
      relativeSill : Double ;
    private
      FSill  : Double ;
    private
      function  fget_Sill   : Double ;
      procedure fset_Sill   ( const _val : Double
                            ) ;
    protected
      procedure fset_Nugget ( const _val : Double
                            ) ; override ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   The sill of semivariance.
      /// </summary>
      property Sill : Double
                      read  fget_Sill
                      write fset_Sill ;
  end ;

  /// <summary>
  ///   Represents the spherical semivariance model.
  /// </summary>
  TGIS_SemivarianceSpherical = {$IFDEF OXYGENE} public {$ENDIF}
                               class( TGIS_SemivarianceWithSill )
    public
      /// <inheritdoc/>
      function  Calculate ( const _dist : Double
                          ) : Double ; override ;
  end ;

  /// <summary>
  ///   Represents the circular semivariance model.
  /// </summary>
  TGIS_SemivarianceCircular = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_SemivarianceWithSill )
    public
      /// <inheritdoc/>
      function  Calculate ( const _dist : Double
                          ) : Double ; override ;
  end ;

  /// <summary>
  ///   Represents the linear semivariance model.
  /// </summary>
  TGIS_SemivarianceLinear = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_SemivarianceWithSill )
    public
      /// <inheritdoc/>
      function  Calculate ( const _dist : Double
                          ) : Double ; override ;
  end ;

  /// <summary>
  ///   Implementation of the ordinary Kriging interpolation method.
  /// </summary>
  TGIS_InterpolationKriging = {$IFDEF OXYGENE} public {$ENDIF}
                              class( TGIS_VectorToGridAbstract )
    private
      FWindowed     : Boolean ;
      FMinPoints    : Integer ;
      FMaxPoints    : Integer ;
      FRadius       : Double ;
      FSemivariance : TGIS_SemivarianceAbstract ;
    private
      function  fget_MinPoints    : Integer ;
      procedure fset_MinPoints    ( const _val : Integer
                                  ) ;
      function  fget_MaxPoints    : Integer ;
      procedure fset_MaxPoints    ( const _val : Integer
                                  ) ;
      function  fget_Radius       : Double ;
      procedure fset_Radius       ( const _val : Double
                                  ) ;
      function  fget_Semivariance : TGIS_SemivarianceAbstract ;
      procedure fset_Semivariance ( const _obj : TGIS_SemivarianceAbstract
                                  ) ;
    protected
      procedure doDestroy ; override ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      procedure Generate ( const _src    : TGIS_LayerVector ;
                           const _srcext : TGIS_Extent ;
                           const _srcfld : String ;
                           const _dst    : TGIS_LayerPixel ;
                           const _dstext : TGIS_Extent
                         ) ; override ;
    public
      /// <summary>
      ///   If True then the windowed version of the algorithm is used; false
      ///   by default.
      /// </summary>
      property Windowed        : Boolean
                                 read  FWindowed
                                 write FWindowed ;
      /// <summary>
      ///   The minimum number of input data points used to interpolate at each
      ///   grid cell; default is 4.
      /// </summary>
      property MinPoints       : Integer
                                 read  fget_MinPoints
                                 write fset_MinPoints ;
      /// <summary>
      ///   The maximum number of input data points used to interpolate at each
      ///   grid cell; default is 4.
      /// </summary>
      property MaxPoints       : Integer
                                 read  fget_MaxPoints
                                 write fset_MaxPoints ;
      /// <summary>
      ///   Defines the distance of search for input data points for a grid
      ///   cell. Only the data points within the distance may be taken to
      ///   interpolate the value.
      /// </summary>
      property Radius          : Double
                                 read  fget_Radius
                                 write fset_Radius ;
      /// <summary>
      ///   Model used for the semivariance calculation; default is power-law.
      /// </summary>
      {#ownership:set:release}
      property Semivariance    : TGIS_SemivarianceAbstract
                                 read  fget_Semivariance
                                 write fset_Semivariance ;
  end ;

  /// <summary>
  ///   Implementation of the Inverse Distance Weighting (IDW)
  ///   interpolation method.
  /// </summary>
  TGIS_InterpolationIDW = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_VectorToGridAbstract )
    private
      FWindowed : Boolean ;
      FRadius   : Double ;
      FExponent : Double ;
    private
      function  fget_Radius       : Double ;
      procedure fset_Radius       ( const _val : Double
                                  ) ;
      function  fget_Exponent     : Double ;
      procedure fset_Exponent     ( const _val : Double
                                  ) ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      procedure Generate ( const _src    : TGIS_LayerVector ;
                           const _srcext : TGIS_Extent ;
                           const _srcfld : String ;
                           const _dst    : TGIS_LayerPixel ;
                           const _dstext : TGIS_Extent
                         ) ; override ;
    public
      /// <summary>
      ///   If True then the windowed version of the algorithm is used; true by
      ///   default.
      /// </summary>
      property Windowed : Boolean
                          read  FWindowed
                          write FWindowed ;
      /// <summary>
      ///   Applies only to the windowed method (Windowed = True).
      ///   Defines the distance of search for input data points for a grid
      ///   cell. Only the data points within the distance may be taken to
      ///   interpolate the value.
      /// </summary>
      property Radius   : Double
                          read  fget_Radius
                          write fset_Radius ;
      /// <summary>
      ///   Exponent in the formula for weight calculation; default value is 2.
      /// </summary>
      property Exponent : Double
                          read  fget_Exponent
                          write fset_Exponent ;
  end ;

  /// <summary>
  ///   Fast heatmap generator, creates superposition of normal (Gaussian)
  ///   distribution for each data point.
  /// </summary>
  TGIS_GaussianHeatmap = {$IFDEF OXYGENE} public {$ENDIF}
                         class( TGIS_VectorToGridAbstract )
    private
      FRadius : Double ;
    private
      function  fget_Radius       : Double ;
      procedure fset_Radius       ( const _val : Double
                                  ) ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      procedure Generate ( const _src    : TGIS_LayerVector ;
                           const _srcext : TGIS_Extent ;
                           const _srcfld : String ;
                           const _dst    : TGIS_LayerPixel ;
                           const _dstext : TGIS_Extent
                         ) ; override ;
      /// <summary>
      ///   Calculates a "good choice" of the 3-sigma ("radius") of the
      ///   Gaussian distribution. Overwrites the Radius property value.
      /// </summary>
      /// <param name="_src">
      ///   source layer which contains the sample points
      /// </param>
      /// <param name="_srcext">
      ///   extent of interest on the source layer
      /// </param>
      /// <param name="_dst">
      ///   destination grid layer
      /// </param>
      procedure EstimateRadius
                       ( const _src    : TGIS_LayerVector ;
                         const _srcext : TGIS_Extent ;
                         const _dst    : TGIS_LayerPixel
                       ) ;
    public
      /// <summary>
      ///   3-sigma ("radius") of the Gaussian distribution.
      /// </summary>
      property Radius : Double
                        read  fget_Radius
                        write fset_Radius ;
  end ;

  /// <summary>
  ///   Implementation of the Completely Regularized Splines (CRS)
  ///   interpolation method.
  /// </summary>
  TGIS_InterpolationSplines = {$IFDEF OXYGENE} public {$ENDIF}
                              class ( TGIS_VectorToGridAbstract )
    private
      const LOCAL_EULER_CONSTANT : Double = 0.5772156649 ;
    private
      dApproxC1  : Double ;
      dApproxC2  : Double ;
    private
      FWindowed  : Boolean ;
      FMinPoints : Integer ;
      FMaxPoints : Integer ;
      FRadius    : Double ;
      FTension   : Double ;
    private
      function  fget_MinPoints    : Integer ;
      procedure fset_MinPoints    ( const _val : Integer
                                  ) ;
      function  fget_MaxPoints    : Integer ;
      procedure fset_MaxPoints    ( const _val : Integer
                                  ) ;
      function  fget_Radius       : Double ;
      procedure fset_Radius       ( const _val : Double
                                  ) ;
      function  fget_Tension      : Double ;
      procedure fset_Tension      ( const _val : Double
                                  ) ;
    private
      function approxExpIntE1     ( const _arg : Double
                                  ) : Double ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      procedure Generate ( const _src    : TGIS_LayerVector ;
                           const _srcext : TGIS_Extent ;
                           const _srcfld : String ;
                           const _dst    : TGIS_LayerPixel ;
                           const _dstext : TGIS_Extent
                         ) ; override ;
    public
      /// <summary>
      ///   If True then the windowed version of the algorithm is used; true by
      ///   default.
      /// </summary>
      property Windowed : Boolean
                          read  FWindowed
                          write FWindowed ;
      /// <summary>
      ///   The minimum number of input data points used to interpolate at each
      ///   grid cell; default is 4.
      /// </summary>
      property MinPoints       : Integer
                                 read  fget_MinPoints
                                 write fset_MinPoints ;
      /// <summary>
      ///   The maximum number of input data points used to interpolate at each
      ///   grid cell; default is 4.
      /// </summary>
      property MaxPoints       : Integer
                                 read  fget_MaxPoints
                                 write fset_MaxPoints ;
      /// <summary>
      ///   Applies only to the windowed method (Windowed = True).
      ///   Defines the distance of search for input data points for a grid
      ///   cell. Only the data points within the distance may be taken to
      ///   interpolate the value.
      /// </summary>
      property Radius   : Double
                          read  fget_Radius
                          write fset_Radius ;
      /// <summary>
      ///   The tension (smoothing) factor.
      /// </summary>
      property Tension  : Double
                          read  fget_Tension
                          write fset_Tension ;
  end ;

  /// <summary>
  ///   Provides the means to convert a Triangulated Irregular Network (TIN)
  ///   vector layer to a digital elevation model (grid layer). The algorithm
  ///   is a simplified Inverse Distance Weighting (IDW) interpolation method.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///     This class is OpenCL-enabled, it can utilize OpenCL for increased
  ///     performance. Use TGIS_OpenCLEngine.Enabled to enable OpenCL
  ///     computation mode.
  ///   </note>
  /// </remarks>
  TGIS_TinToGrid = {$IFDEF OXYGENE} public {$ENDIF}
                                    class( TGIS_Object )
    private
      FExponent        : Double ;
      FSourceLayer     : TGIS_LayerVector ;
      FOutputLayer     : TGIS_LayerPixel ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      busyEventManager : TGIS_BusyEventManager ;

    private
      function  fget_Exponent       : Double ;
      procedure fset_Exponent       ( const _val : Double
                                    ) ;
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

    private
      function  generateCPU         : Boolean ;
      {$IFNDEF JAVA OR ISLAND}
        function  generateOCL       : Boolean ;
      {$ENDIF}

    protected
      procedure doDestroy ; override ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <summary>
      ///   Generates a grid layer based on a Triangulated Irregular Network
      ///   (TIN) layer.
      /// </summary>
      /// <param name="_src">
      ///   source TIN layer
      /// </param>
      /// <param name="_dst">
      ///   destination grid layer; this layer must have the same CS and extent
      ///   as _src
      /// </param>
      /// <returns>
      ///   True if the operation was completed successfully; False otherwise
      /// </returns>
      function  Generate   ( const _src : TGIS_LayerVector ;
                             const _dst : TGIS_LayerPixel
                           ) : Boolean ; overload ;

      /// <summary>
      ///   Generates a grid layer based on a Triangulated Irregular Network
      ///   (TIN) layer.
      /// </summary>
      /// <returns>
      ///   True if the operation was completed successfully; False otherwise
      /// </returns>
      function  Generate   : Boolean ; overload ;

    public
      /// <summary>
      ///   Exponent in the formula for weight calculation; default value is 2.
      /// </summary>
      property Exponent    : Double
                             read  fget_Exponent
                             write fset_Exponent ;
      /// <summary>
      ///   Source layer, Triangulated Irregular Network (TIN).
      /// </summary>
      property SourceLayer : TGIS_LayerVector
                             read  FSourceLayer
                             write FSourceLayer ;
      /// <summary>
      ///   Destination grid layer; this layer must have the same CS and extent
      ///   as the source layer.
      /// </summary>
      property OutputLayer : TGIS_LayerPixel
                             read  FOutputLayer
                             write FOutputLayer ;

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

  /// <summary>
  ///   Specifies cell assignment method for point cloud to grid conversion.
  /// </summary>
  TGIS_PointCloudAssignment = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Number of points in cell.
    /// </summary>
    Count,
    /// <summary>
    ///   First value found in cell.
    /// </summary>
    First,
    /// <summary>
    ///   Last value found in cell.
    /// </summary>
    Last,
    /// <summary>
    ///   Minimum value found in cell.
    /// </summary>
    Min,
    /// <summary>
    ///   Maximum value found in cell.
    /// </summary>
    Max,
    /// <summary>
    ///   Mean value comoputed from points within cell;
    /// </summary>
    Mean,
    /// <summary>
    ///   Range of point values in cell.
    /// </summary>
    Range,
    /// <summary>
    ///   Uses Inverse Distance Weighted interpolation for points within cell.
    /// </summary>
    IDW,
    /// <summary>
    ///   Value from the nearest point to cell center.
    /// </summary>
    NearestNeighbor
  ) ;

  /// <summary>
  ///   Specifies cell assignment method for NoData filling.
  /// </summary>
  TGIS_PointCloudFormula = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   For each grid cell with NoData, DefaultValue will be assigned.
    /// </summary>
    DefaultValue,
    /// <summary>
    ///   Computes mean value from grid cells within search window.
    /// </summary>
    Mean,
    /// <summary>
    ///   Uses Inverse Distance Weighted interpolation for grid cells within
    ///   search window.
    /// </summary>
    IDW,
    /// <summary>
    ///   Searches for nearest grid cell within search window.
    /// </summary>
    NearestNeighbor,
    /// <summary>
    ///   Uses Triangulation interpolation to determine value for empty grid
    ///   cells. WindowSize parameter is omitted. It takes longer to execute.
    ///   Recommended for data with local holes.
    /// </summary>
    Triangulation
  ) ;

  /// <summary>
  ///   Provides the means to convert a point cloud vector layer such as LIDAR
  ///   to a digital elevation model (grid layer).
  /// </summary>
  TGIS_PointCloudToGrid = {$IFDEF OXYGENE} public {$ENDIF}
                                           class( TGIS_VectorToGridAbstract )
    private
      FAssignment    : TGIS_PointCloudAssignment ;
      FDefValFormula : TGIS_PointCloudFormula ;
      FWindowSize    : Integer ;
      FExponent      : Double ;

    private
      sourceLayer    : TGIS_LayerVector ;
      outputLayer    : TGIS_LayerPixel ;
      sourceExtent   : TGIS_Extent ;
      outputExtent   : TGIS_Extent ;
      sourceField    : String ;
      minVal         : Single ;
      maxVal         : Single ;
      pixelSize      : Double ;

      // storing indexes of nulls in list is much faster than loop whole pixels
      // for Mean, Range, IDW methods this loop is necessary so we use this fact
      emptyCells          : TList<TPoint> ;

    private
      function  fget_WindowSize : Integer ;
      procedure fset_WindowSize ( const _val : Integer ) ;

      function  fget_Exponent   : Double ;
      procedure fset_Exponent   ( const _val : Double ) ;

      function  getPixelSize    : Double ;
      procedure doAssignment ;
      procedure fillNoData ;
      function  findNoData      : Boolean ;

    protected
      procedure doDestroy ; override ;

    public
      /// <inheritdoc/>
      constructor Create ;

    public
      /// <inheritdoc/>
      procedure Generate ( const _src    : TGIS_LayerVector ;
                           const _srcext : TGIS_Extent ;
                           const _srcfld : String ;
                           const _dst    : TGIS_LayerPixel ;
                           const _dstext : TGIS_Extent
                         ) ; overload ; override ;

      /// <summary>
      ///   Generates a grid layer based on a vector layer with point cloud.
      ///   All points within source layer extent will be processed.
      ///   Gridding cell size same as destination layer's pixel size.
      ///   Coordinate property is omitted and elevation will be assigned from
      ///   Z coordinates.
      /// </summary>
      /// <param name="_src">
      ///   source layer, point cloud such as LIDAR; must be 3D
      /// </param>
      /// <param name="_dst">
      ///   destination grid layer; this layer must have the same CS as the
      ///   source layer
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      procedure Generate ( const _src     : TGIS_LayerVector ;
                           const _dst     : TGIS_LayerPixel
                         ) ; overload ;

      /// <summary>
      ///   Defines method of assigning point values within each pixel;
      ///   default is Mean.
      /// </summary>
      property Assignment : TGIS_PointCloudAssignment
                            read  FAssignment
                            write FAssignment ;

      /// <summary>
      ///   Defines cell assignment method for NoData filling;
      ///   default is DefaultValue.
      /// </summary>
      property DefaultValueFormula : TGIS_PointCloudFormula
                                     read  FDefValFormula
                                     write FDefValFormula ;

      /// <summary>
      ///   Defines window size for filling NoData process;
      ///   odd value required, even values will be rounded up;
      ///   default value is 3.
      /// </summary>
      property WindowSize : Integer
                            read  fget_WindowSize
                            write fset_WindowSize ;
      /// <summary>
      ///   Exponent in the formula for weight calculation; default value is 2.
      /// </summary>
      property Exponent : Double
                          read  fget_Exponent
                          write fset_Exponent ;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Variants,
    System.SysUtils,
    GisResource,
    GisFunctions,
    GisCsSystems,
    GisOpenCL,
    GisTriangulation,
    GisPipeline ;
{$ENDIF}

type T_INTERPOLATION = class
  public const
    PARAM_SRC     = '_src' ;
    PARAM_SRCEXT  = '_srcext' ;
    PARAM_SRCFLD  = '_srcfld' ;
    PARAM_DST     = '_dst' ;
    PARAM_DSTEXT  = '_dstext' ;

    USE_DEFVAL    = False ;
    DEFVAL        = 0.0 ;
    EXPONENT      = 2.0 ;
    PROGRESS_STEP = 0.01 ;
    PIXEL_SIZE    = 1.0 ;
    WINDOW_SIZE   = 3 ;
end;

{$REGION 'GIS_OCL_TINTOGRID_SOURCE'}
{$IFNDEF JAVA}
const
  GIS_OCL_TINTOGRID_SOURCE =
    '#pragma OPENCL EXTENSION cl_khr_fp64 : enable' + #13#10 +
    #13#10 +
    '/* squared distance between two planar points */' + #13#10 +
    'double distsqr(' + #13#10 +
    '  double xtoy,' + #13#10 +
    '  double p0x,' + #13#10 +
    '  double p0y,' + #13#10 +
    '  double p1x,' + #13#10 +
    '  double p1y' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  double difx = xtoy*(p0x-p1x);' + #13#10 +
    '  double dify = p0y-p1y;' + #13#10 +
    '' + #13#10 +
    '  return difx*difx+dify*dify;' + #13#10 +
    '}' + #13#10 +
    '' + #13#10 +
    '/* checks if point lays within a triangle */' + #13#10 +
    'bool is3(' + #13#10 +
    '  double px,' + #13#10 +
    '  double py,' + #13#10 +
    '  double p0x,' + #13#10 +
    '  double p0y,' + #13#10 +
    '  double p1x,' + #13#10 +
    '  double p1y,' + #13#10 +
    '  double p2x,' + #13#10 +
    '  double p2y' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  double vx = px-p0x;' + #13#10 +
    '  double vy = py-p0y;' + #13#10 +
    '  double v1x = p1x-p0x;' + #13#10 +
    '  double v1y = p1y-p0y;' + #13#10 +
    '  double v2x = p2x-p0x;' + #13#10 +
    '  double v2y = p2y-p0y;' + #13#10 +
    '  double d12 = v1x*v2y-v1y*v2x;' + #13#10 +
    '' + #13#10 +
    '  double u = (vx*v2y-vy*v2x)/d12;' + #13#10 +
    '  double v = -(vx*v1y-vy*v1x)/d12;' + #13#10 +
    '' + #13#10 +
    '  return (u>=0.0)&&(v>=0.0)&&(u+v<=1.0);' + #13#10 +
    '}' + #13#10 +
    '' + #13#10 +
    '/* returns IDW (Inverse Distance Squared) interpolated value */' + #13#10 +
    'float interpolate(' + #13#10 +
    '  double exp,' + #13#10 +
    '  double xtoy,' + #13#10 +
    '  double px,' + #13#10 +
    '  double py,' + #13#10 +
    '  double p0x,' + #13#10 +
    '  double p0y,' + #13#10 +
    '  double p0z,' + #13#10 +
    '  double p1x,' + #13#10 +
    '  double p1y,' + #13#10 +
    '  double p1z,' + #13#10 +
    '  double p2x,' + #13#10 +
    '  double p2y,' + #13#10 +
    '  double p2z' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  float val=0.0;' + #13#10 +
    '  double w0 = distsqr(xtoy,px,py,p0x,p0y);' + #13#10 +
    '  double w1 = distsqr(xtoy,px,py,p1x,p1y);' + #13#10 +
    '  double w2 = distsqr(xtoy,px,py,p2x,p2y);' + #13#10 +
    '  ' + #13#10 +
    '  if (w0==0.0)' + #13#10 +
    '  {' + #13#10 +
    '    val = p0z;' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  if (w1==0.0)' + #13#10 +
    '  {' + #13#10 +
    '    val = p1z;' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  if (w2==0.0)' + #13#10 +
    '  {' + #13#10 +
    '    val = p2z;' + #13#10 +
    '  }' + #13#10 +
    '  else' + #13#10 +
    '  {' + #13#10 +
    '    if (exp==2.0)' + #13#10 +
    '    {' + #13#10 +
    '      w0 = 1.0/w0;' + #13#10 +
    '      w1 = 1.0/w1;' + #13#10 +
    '      w2 = 1.0/w2;' + #13#10 +
    '    }' + #13#10 +
    '    else' + #13#10 +
    '    {' + #13#10 +
    '      w0 = 1.0/pow(w0,exp/2.0);' + #13#10 +
    '      w1 = 1.0/pow(w1,exp/2.0);' + #13#10 +
    '      w2 = 1.0/pow(w2,exp/2.0);' + #13#10 +
    '    }' + #13#10 +
    '    val = (w0*p0z+w1*p1z+w2*p2z)/(w0+w1+w2);' + #13#10 +
    '  }' + #13#10 +
    '' + #13#10 +
    '  return val;' + #13#10 +
    '}' + #13#10 +
    '' + #13#10 +
    '/* KERNEL */' + #13#10 +
    '__kernel void tin2grid(' + #13#10 +
    '  __global double * input,' + #13#10 +
    '  __global float * output,' + #13#10 +
    '  double exp,' + #13#10 +
    '  int siz,' + #13#10 +
    '  double xtoy' + #13#10 +
    ')' + #13#10 +
    '{' + #13#10 +
    '  int i = 9*get_global_id(0);' + #13#10 +
    '  int ix,iy;' + #13#10 +
    '' + #13#10 +
    '  int xmin = round(fmin(input[i  ],fmin(input[i+3],input[i+6])));' +
    #13#10 +
    '  int ymin = round(fmin(input[i+1],fmin(input[i+4],input[i+7])));' +
    #13#10 +
    '  int xmax = round(fmax(input[i  ],fmax(input[i+3],input[i+6])));' +
    #13#10 +
    '  int ymax = round(fmax(input[i+1],fmax(input[i+4],input[i+7])));' +
    #13#10 +
    '  ' + #13#10 +
    '  for (iy=ymin;iy<=ymax;iy++)' + #13#10 +
    '  {' + #13#10 +
    '    for (ix=xmin;ix<=xmax;ix++)' + #13#10 +
    '    {' + #13#10 +
    '      if (is3(ix,iy,input[i],input[i+1],input[i+3],' +
    'input[i+4],input[i+6],input[i+7]))' + #13#10 +
    '      {' + #13#10 +
    '        output[iy*siz+ix]=interpolate(exp,xtoy,ix,iy,' +
    'input[i],input[i+1],input[i+2],input[i+3],input[i+4],' +
    'input[i+5],input[i+6],input[i+7],input[i+8]);' + #13#10 +
    '      }' + #13#10 +
    '    }' + #13#10 +
    '  }' + #13#10 +
    '}' ;
  GIS_OCL_TINTOGRID_KERNEL = 'tin2grid' ;
{$ENDIF}
{$ENDREGION}

type

  /// <summary>
  ///   Implementation of the 'PointCloudToGrid' pipeline command.
  /// </summary>
  T_Pipeline_PointCloudToGrid = class( TGIS_PipelineOperationExtendedAbstract )
    const
      PARAM_ASSIGNMENT     = 'Assignment' ;
      PARAM_COORDINATE     = 'Coordinate' ;
      PARAM_DEFVAL         = 'DefaultValue' ;
      PARAM_DEFVAL_FORMULA = 'DefaultValueFormula' ;
      PARAM_EXPONENT       = 'Exponent' ;
      PARAM_USE_DEFVAL     = 'UseDefaultValue' ;
      PARAM_WINDOW_SIZE    = 'WindowSize' ;
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  /// <summary>
  ///   Implementation of the 'Kriging' pipeline command.
  /// </summary>
  T_Pipeline_InterpolationKriging = class( TGIS_PipelineOperationExtendedAbstract)
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;

  T_pointValue = class
    public
      Uid   : Int64 ;
      Point : TGIS_Point ;
      Value : Double ;
      Dist  : Double ;
  end ;

  T_pointList = class( TGIS_ShapeListStub )
    private
      {$IFDEF DCC}
        lBase    : TObjectList<T_pointValue> ;
        lSorted  : TObjectList<T_pointValue> ;
      {$ELSE}
        lBase    : TList<T_pointValue> ;
        lSorted  : TList<T_pointValue> ;
      {$ENDIF}
      eCoord     : TGIS_VectorToGridCoordinate ;
      bCoord     : Boolean ;
      sField     : String ;
    private
      FThreshold : Integer ;
      FSorted    : Boolean ;
    protected
      function  fget_Count : Integer ; override ;
      function  fget_Point ( const _idx : Integer
                           ) : TGIS_Point ; override ;
      function  fget_Value ( const _idx : Integer
                           ) : Double ; override ;
      function  fget_Uid   ( const _idx : Integer
                           ) : Int64 ;

    protected
      procedure doDestroy ; override ;
    public
      constructor Create ( const _coord : TGIS_VectorToGridCoordinate ;
                           const _field : String
                         ) ;

    public
      procedure Prepare ( const _src       : TGIS_LayerVector ;
                          const _dst       : TGIS_LayerPixel ;
                          const _extent    : TGIS_Extent
                        ) ;
      procedure Narrow  ( const _point     : TGIS_Point ;
                          const _radius    : Double
                        ) ;
      procedure Sort    ( const _point     : TGIS_Point ;
                          const _radius    : Double
                        ) ;
      function  Equal   ( const _list      : T_pointList
                        ) : Boolean ;
    public
      property Threshold : Integer
                           read  FThreshold
                           write FThreshold ;
      property Sorted    : Boolean
                           read FSorted ;
      property Uid[const _idx : Integer]
                         : Int64
                           read  fget_Uid ;
  end ;

  procedure assignMinMax (
      var _min   : Single ;
      var _max   : Single ;
      const _val : Single ) ;
  begin
    _min := Min( _min, _val ) ;
    _max := Max( _max, _val ) ;
  end ;

  function isNoData(
    const _val     : Single ;
    const _novalue : Single
    ) : Boolean ; overload ;
  begin
    Result := GisIsSameValue( _val, _novalue, GIS_SINGLE_RESOLUTION )
  end ;

  function isNoData(
    const _val : Single
    ) : Boolean ; overload;
  begin
    Result := GisIsSameValue( _val, GIS_GRID_NOVALUE, GIS_SINGLE_RESOLUTION )
  end ;

//==============================================================================
// T_pointList
//==============================================================================

  constructor T_pointList.Create(
    const _coord : TGIS_VectorToGridCoordinate ;
    const _field : String
  ) ;
  begin
    inherited Create ;

    {$IFDEF DCC}
      lBase := TObjectList<T_pointValue>.Create ;
      lSorted := TObjectList<T_pointValue>.Create ;
      lsorted.OwnsObjects := False ;
    {$ELSE}
      lBase := TList<T_pointValue>.Create ;
      lSorted := TList<T_pointValue>.Create ;
    {$ENDIF}

    eCoord := _coord ;
    bCoord := length( _field ) = 0 ;
    sField := _field ;

    FThreshold := MaxInt ;
    FSorted := False ;
  end ;


  procedure T_pointList.doDestroy ;
  begin
    FreeObject( lBase ) ;
    FreeObject( lSorted ) ;

    inherited ;
  end ;


  function T_pointList.fget_Count : Integer ;
  begin
    if Sorted then
      Result := lSorted.Count
    else
      Result := lBase.Count ;
  end ;


  function T_pointList.fget_Point(
    const _idx : Integer
  ) : TGIS_Point ;
  begin
    if Sorted then
      Result := lSorted[_idx].Point
    else
      Result := lBase[_idx].Point ;
  end ;


  function T_pointList.fget_Value(
    const _idx : Integer
  ) : Double ;
  var
    lst : TList<T_pointValue> ;
  begin
    if bCoord and ( eCoord = TGIS_VectorToGridCoordinate.None ) then begin
      Result := 1.0 ;
      exit ;
    end ;

    if Sorted then
      lst := lSorted
    else
      lst := lBase ;

    if _idx = lst.Count then
      Result := 0.0
    else
      Result := lst[_idx].Value ;
  end ;


  function T_pointList.fget_Uid(
    const _idx : Integer
  ) : Int64 ;
  begin
    if Sorted then
      Result := lSorted[_idx].Uid
    else
      Result := lBase[_idx].Uid ;
  end ;


  procedure T_pointList.Prepare(
    const _src    : TGIS_LayerVector ;
    const _dst    : TGIS_LayerPixel ;
    const _extent : TGIS_Extent
  ) ;
  var
    cs  : TGIS_CSCoordinateSystem ;
    pv  : T_pointValue ;
    p3d : TGIS_Point3D ;
    ext : TGIS_Extent ;
    en  : TGIS_LayerVectorEnumerator ;
    shp : TGIS_Shape ;
    p   : TGIS_Point ;
    b   : Boolean ;
    i   : Integer ;
    j   : Integer ;
    k   : Integer ;
  begin
    lBase.Clear ;

    FSorted := False ;

    if assigned( _src.Viewer ) then
      cs := _src.Viewer.Ref.CS
    else
      cs := _src.CS ;

    ext := _src.CS.ExtentFromCS( _dst.CS, _extent ) ;
    en := _src.Loop( ext ).GetEnumerator ;
    try
      while en.MoveNext do begin
        shp := en.GetCurrent ;

        for j := 0 to shp.GetNumParts - 1 do begin
          for k := 0 to shp.GetPartSize( j ) - 1 do begin

            p3d := shp.GetPoint3D( j, k ) ;
            p := GisPoint( p3d.X, p3d.Y ) ;

            b := False ;
            for i := 0 to lBase.Count - 1 do begin
              b := GisIsSamePoint( lBase[i].Point, p ) ;
              if b then break ;
            end ;
            if b then continue ;

            pv := T_pointValue.Create ;
            pv.Uid := shp.Uid ;
            pv.Point := _dst.CS.FromCS( cs, p ) ;
            if bCoord then begin
              case eCoord of
                TGIS_VectorToGridCoordinate.Z  : pv.Value := p3d.Z ;
                TGIS_VectorToGridCoordinate.M  : pv.Value := p3d.M ;
                TGIS_VectorToGridCoordinate.ZM : pv.Value := p3d.Z + p3d.M ;
              end ;
            end
            else
              pv.Value := VarToDouble( shp.GetField( sField ) ) ;

            lBase.Add( pv ) ;

          end ;
        end ;

      end ;
    finally
      FreeObject( en ) ;
    end ;
  end ;


  procedure T_pointList.Narrow(
    const _point  : TGIS_Point ;
    const _radius : Double
  ) ;
  var
    pt  : TGIS_Point ;
    rsq : Double ;
    dst : Double ;
    i   : Integer ;

    function g_dist( const _p : TGIS_Point ) : Double ;
    begin
      Result := Sqr( _point.X - _p.X ) + Sqr( _point.Y - _p.Y ) ;
    end ;

  begin
    rsq := Sqr( _radius ) ;

    for i := 0 to lBase.Count - 1 do begin
      pt := _TGIS_Point(lBase[i].Point) ;
      dst := g_dist( pt ) ;
      if dst > rsq then
        {$IFDEF OXYGENE}
          lBase.RemoveAt( i ) ;
        {$ELSE}
          lBase.Delete( i ) ;
        {$ENDIF}
    end ;
  end ;


  procedure T_pointList.Sort(
    const _point  : TGIS_Point ;
    const _radius : Double
  ) ;
  var
    pt  : TGIS_Point ;
    cnt : Integer ;
    rsq : Double ;
    dst : Double ;
    i   : Integer ;
    {$IFDEF OXYGENE}
      pv : T_pointValue ;
    {$ENDIF}

    function g_dist( const _p : TGIS_Point ) : Double ;
    begin
      Result := Sqr( _point.X - _p.X ) + Sqr( _point.Y - _p.Y ) ;
    end ;

  begin
    lSorted.Clear ;

    rsq := Sqr( _radius ) ;

    for i := 0 to lBase.Count - 1 do begin
      pt := _TGIS_Point(lBase[i].Point) ;
      dst := g_dist( pt ) ;
      if dst <= rsq then begin
        lBase[i].Dist := dst ;
        lSorted.Add( lBase[i] ) ;
      end ;
    end ;

    while True do begin
      cnt := 0 ;
      for i := 0 to lSorted.Count - 2 do begin
        if lSorted[i].Dist > lSorted[i+1].Dist then begin
          {$IFDEF OXYGENE}
            pv := lSorted.Item[i] ;
            lSorted.RemoveAt( i ) ;
            lSorted.Insert( i+1, pv ) ;
          {$ELSE}
            lSorted.Exchange( i, i+1 ) ;
          {$ENDIF}
          inc( cnt ) ;
        end ;
      end ;
      if cnt = 0 then
        break ;
    end ;

    FSorted := True ;

    if Count > Threshold then
      {$IFDEF OXYGENE}
        lSorted.RemoveRange( Threshold, Count - Threshold ) ;
      {$ELSE}
        lSorted.DeleteRange( Threshold, Count - Threshold ) ;
      {$ENDIF}
  end ;


  function T_pointList.Equal(
    const _list : T_pointList
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := False ;

    if Count <> _list.Count then
      exit ;

    if Sorted <> _list.Sorted then
      exit ;

    if Sorted then begin
      for i := 0 to Count - 1 do begin
        if lSorted[i].Uid <> _list.Uid[i] then
          exit ;
      end ;
    end
    else begin
      for i := 0 to Count - 1 do begin
        if lBase[i].Uid <> _list.Uid[i] then
          exit ;
      end ;
    end ;

    Result := True ;
  end ;


//==============================================================================
// TGIS_InterpolatedPath
//==============================================================================

  constructor TGIS_InterpolatedPath.Create ;
  begin
    inherited ;

    points := TList<TGIS_Point3D>.Create ;
    intervals := TList<Double>.Create ;
    duplicates := TList<Integer>.Create ;

    intervals.Add( 0.0 ) ;

    FAllowDuplicates := False ;
    FClosed          := False ;
    FSpan            := 100.0 ;
    FLooped          := False ;
    FEqualizeInt     := False ;
    FLevelInt        := False ;
    FRotation        := False ;

    rotMul := 0 ;
  end ;


  procedure TGIS_InterpolatedPath.doDestroy ;
  begin
    FreeObject( points ) ;
    FreeObject( intervals ) ;
    FreeObject( duplicates ) ;

    inherited ;
  end ;


  function TGIS_InterpolatedPath.fget_Count : Integer ;
  begin
    if Closed and Looped and ( points.Count > 1 ) then
      Result := points.Count - 1
    else
      Result := points.Count ;
  end ;


  function TGIS_InterpolatedPath.fget_Closed : Boolean ;
  begin
    Result := FClosed ;
  end ;


  function TGIS_InterpolatedPath.fget_Span : Double ;
  begin
    Result := FSpan ;
  end ;


  procedure TGIS_InterpolatedPath.fset_Span(
    const _val : Double
  ) ;
  begin
    if Closed then
      exit ;

    if _val <= 0.0 then
      exit ;

    FSpan := _val ;
  end ;


  function TGIS_InterpolatedPath.fget_Looped : Boolean ;
  begin
    Result := FLooped ;
  end ;


  procedure TGIS_InterpolatedPath.fset_Looped(
    const _val : Boolean
  ) ;
  begin
    if Closed then
      exit ;

    FLooped := _val ;
  end ;


  function TGIS_InterpolatedPath.fget_LastInterval : Double ;
  begin
    Result := intervals[points.Count-1] ;
  end ;


  procedure TGIS_InterpolatedPath.fset_LastInterval(
    const _val : Double
  ) ;
  begin
    if Closed then
      exit ;

    if _val <= 0 then
      exit ;

    length := length - intervals[points.Count-1] ;
    intervals[points.Count-1] := _val ;
    length := length + intervals[points.Count-1] ;
  end ;


  function TGIS_InterpolatedPath.fget_EqualizeInt : Boolean ;
  begin
    Result := FEqualizeInt ;
  end ;


  procedure TGIS_InterpolatedPath.fset_EqualizeInt(
    const _val : Boolean
  ) ;
  begin
    if Closed then
      exit ;

    if _val then
      FLevelInt := False ;

    FEqualizeInt := _val ;
  end ;


  function TGIS_InterpolatedPath.fget_LevelInt : Boolean ;
  begin
    Result := FLevelInt ;
  end ;


  procedure TGIS_InterpolatedPath.fset_LevelInt(
    const _val : Boolean
  ) ;
  begin
    if Closed then
      exit ;

    if _val then
      FEqualizeInt := False ;

    FLevelInt := _val ;
  end ;


  function TGIS_InterpolatedPath.fget_Rotation : Boolean ;
  begin
    Result := FRotation ;
  end ;


  procedure TGIS_InterpolatedPath.fset_Rotation(
    const _val : Boolean
  ) ;
  begin
    if Closed then
      exit ;

    FRotation := _val ;
  end ;


  function TGIS_InterpolatedPath.fget_Tolerance : Double ;
  begin
    Result := FTolerance ;
  end ;


  procedure TGIS_InterpolatedPath.fset_Tolerance(
    const _val : Double
  ) ;
  begin
    FTolerance := Abs( _val ) ;
  end ;


  procedure TGIS_InterpolatedPath.nextPointByDist(
    const _t    : Double ;
      out _idx  : Integer ;
      out _pdst : Double ;
      out _ndst : Double
  ) ;
  var
    l : Double ;
    d : Double ;
    i : Integer ;
  begin
    if points.Count < 2 then begin
      _idx := -1 ;
      _pdst := 0.0 ;
      _ndst := 0.0 ;
      exit ;
    end ;

    l := 0.0 ;
    d := 0.0 ;
    for i := 1 to points.Count - 1 do begin
      d := l ;
      l := l + intervals[i] ;
      if _t <= l then
        break ;
    end ;

    _idx := i ;
    _pdst := d ;
    _ndst := l ;
  end ;


  function TGIS_InterpolatedPath.AddPoint(
    const _pt : TGIS_Point3D
  ) : Boolean ;
  var
    pt  : TGIS_Point3D ;
    tmp : Double ;
    res : Boolean ;
    i   : Integer ;

    function dist : Double ;
    var
      pp0 : TGIS_Point3D ;
      pp1 : TGIS_Point3D ;
    begin
      pp0 := _TGIS_Point3D(points[points.Count-2]) ;
      pp1 := _TGIS_Point3D(points[points.Count-1]) ;
      Result := GisPoint2Point3D( pp1, pp0 ) ;
    end ;

  begin
    Result := False ;
    if Closed then
      exit ;

    pt := _TGIS_Point3D(_pt) ;

    res := False ;
    if Count > 0 then begin
      i := Count - 1 ;
      res := ( Abs( pt.X - points[i].X ) <= Tolerance ) and
             ( Abs( pt.Y - points[i].Y ) <= Tolerance ) and
             ( Abs( pt.Z - points[i].Z ) <= Tolerance ) ;
    end ;

    if Rotation and ( Count > 0 ) then begin
      i := Count - 1 ;
      tmp := pt.Z + rotMul*2*Pi ;
      if tmp - points[i].Z < -Pi then
        inc( rotMul )
      else
      if tmp - points[i].Z >  Pi then
        dec( rotMul ) ;
      pt.Z := pt.Z + rotMul*2*Pi ;
    end ;

    if AllowDuplicates then
      points.Add( pt )
    else begin
      if res then begin
        Result := res ;
        exit ;
      end
      else
        points.Add( pt )
    end ;

    if res then
      intervals.Add( 0.0 )
    else
    if points.Count > 1 then begin
      intervals.Add( dist ) ;
      length := length + intervals[points.Count-1] ;
    end ;

    if AllowDuplicates then begin
      if res then
        duplicates.Add( points.Count-1 ) ;
    end ;

    Result := res ;
  end ;


  procedure TGIS_InterpolatedPath.Close ;
  begin
    Close( nil ) ;
  end ;


  procedure TGIS_InterpolatedPath.Close(
    const _sync : TGIS_InterpolatedPath
  ) ;
  var
    d : Double ;
    i : Integer ;

    procedure normalize ;
    var
      dd : Double ;
      ii : Integer ;
    begin
      dd := intervals[0] ;
      for ii := 1 to points.Count - 1 do
        dd := Max( dd, intervals[ii] ) ;
      if dd = 0 then
        exit ;
      dd := dd/10 ;
      length := 0.0 ;
      for ii := 1 to points.Count - 1 do begin
        intervals[ii] := intervals[ii]/dd ;
        length := length + intervals[ii] ;
      end ;
    end ;

    procedure equalize ;
    var
      ii : Integer ;
    begin
      length := 0.0 ;
      for ii := 1 to points.Count - 1 do begin
        intervals[ii] := 1.0 ;
        length := length + 1.0 ;
      end ;
    end ;

    procedure level ;
    var
      av : Double ;
      ii : Integer ;
      mn : Double ;
      mx : Double ;
      dv : Double ;
    begin
      mx := intervals[0] ;
      for ii := 1 to points.Count - 1 do
        mx := Max( mx, intervals[ii] ) ;
      if mx = 0 then
        exit ;

      mn := mx ;
      for ii := 1 to points.Count - 1 do
        mn := Min( mn, intervals[ii] ) ;
      if mn = 0 then
        exit ;

      dv := CeilS( mx/mn ) ;
      dv := Log2( dv ) + 1.0 ;

      av := length/( points.Count - 1 ) ;
      length := 0.0 ;
      for ii := 1 to points.Count - 1 do begin
        intervals[ii] := ( intervals[ii] - av )/dv + av ;
        length := length + intervals[ii] ;
      end ;
    end ;

    procedure prep_line ;
    begin
      xLineA := ( points[1].X - points[0].X )/FSpan ;
      xLineB := points[0].X ;

      yLineA := ( points[1].Y - points[0].Y )/FSpan ;
      yLineB := points[0].Y ;

      zLineA := ( points[1].Z - points[0].Z )/FSpan ;
      zLineB := points[0].Z ;
    end ;

  begin
    if Closed then
      exit ;

    if points.Count > 1 then begin
      if Looped then
        AddPoint( points[0] ) ;

      if AllowDuplicates then begin
        if length = 0 then begin
          for i := 1 to intervals.Count - 1 do
            intervals[i] := 10.0 ;
          length := 10.0*( intervals.Count - 1 ) ;
        end
        else
        if duplicates.Count > 0 then begin
          d := length/( points.Count - 1 - duplicates.Count ) ;
          for i := 0 to duplicates.Count - 1 do begin
            intervals[duplicates[i]] := d ;
            length := length + d ;
          end ;
        end ;
      end ;

      if EqualizeIntervals then
        equalize
      else begin
        normalize ;
        if LevelIntervals then
          level ;
      end ;
    end ;

    if assigned( _sync ) then begin
      if _sync.points.Count = points.Count then begin
        intervals.Clear ;
        intervals.AddRange( _sync.intervals ) ;
        length := _sync.length ;
      end ;
    end ;

    FClosed := True ;

    case points.Count of
      0 : exit ;
      1 : exit ;
      2 : prep_line ;
      else
        prepare ;
    end ;
  end ;


  function TGIS_InterpolatedPath.NextPoint(
    const _t : Double
  ) : Integer ;
  var
    t  : Double ;
    pd : Double ;
    nd : Double ;
    i  : Integer ;
  begin
    t := _t*length/FSpan ;

    nextPointByDist( t, i, pd, nd ) ;

    if Looped and ( i = points.Count - 1 ) then
      i := 0 ;

    Result := i ;
  end ;


  function TGIS_InterpolatedPath.Calculate(
    const _t : Double
  ) : TGIS_Point3D ;
  var
    res : TGIS_Point3D ;
    t   : Double ;

    function out_of_span : Boolean ;
    var
      rr : Boolean ;
    begin
      rr := False ;

      t := _t*length/FSpan ;

      if _t <= 0 then begin
        res := _TGIS_Point3D(points[0]) ;
        rr := True ;
      end
      else
      if _t >= FSpan then begin
        res := _TGIS_Point3D(points[points.Count-1]) ;
        rr := True ;
      end ;

      Result := rr ;
    end ;

    procedure calc_line ;
    begin
      res.X := xLineA*_t + xLineB ;
      res.Y := yLineA*_t + yLineB ;
      res.Z := zLineA*_t + zLineB ;
    end ;

  begin
    if not Closed then begin
      Result := GisPoint3D( 0.0, 0.0, 0.0 ) ;
      exit ;
    end ;

    case points.Count of
      0 : res := GisPoint3D( 0.0, 0.0, 0.0 ) ;
      1 : res := _TGIS_Point3D(points[0]) ;
      2 :
        begin
          if not out_of_span then
            calc_line ;
        end ;
      else
        begin
          if not out_of_span then
            res := interpolate( t ) ;
        end ;
    end ;

    Result := res ;
  end ;


  procedure TGIS_InterpolatedPath.DeleteLast ;
  begin
    if Closed then
      exit ;

    length := length - intervals[Count-1] ;

    if AllowDuplicates then begin
      if duplicates[Count-1] = Count - 1 then
        {$IFDEF DCC}
          intervals.Delete( Count - 1 ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          intervals.RemoveAt( Count - 1 ) ;
        {$ENDIF}
    end ;

    {$IFDEF DCC}
      points.Delete( Count - 1 ) ;
      intervals.Delete( Count - 1 ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      points.RemoveAt( Count - 1 ) ;
      intervals.RemoveAt( Count - 1 ) ;
    {$ENDIF}

    if Count = 0 then
      intervals.Add( 0.0 ) ;
  end ;


  procedure TGIS_InterpolatedPath.Clear ;
  begin
    if Closed then
      exit ;

    length := 0.0 ;

    points.Clear ;
    intervals.Clear ;
    if AllowDuplicates then
      duplicates.Clear ;
    intervals.Add( 0.0 ) ;
  end ;


  procedure TGIS_InterpolatedPath.GetIntervals(
    var _list : TGIS_ListOfDoubles ;
    var _len  : Double
  ) ;
  begin
    _list.Clear ;
    _list.AddRange( intervals ) ;
    _len := length ;
  end ;


//==============================================================================
// TGIS_LinearPath
//==============================================================================

  procedure TGIS_LinearPath.prepare ;
  var
    i : Integer ;
  begin
    SetLength( xParamsA, points.Count ) ;
    SetLength( xParamsB, points.Count ) ;
    SetLength( yParamsA, points.Count ) ;
    SetLength( yParamsB, points.Count ) ;
    SetLength( zParamsA, points.Count ) ;
    SetLength( zParamsB, points.Count ) ;

    for i := 1 to points.Count - 1 do begin
      xParamsA[i] := ( points[i].X - points[i-1].X )/intervals[i] ;
      xParamsB[i] := points[i].X - xParamsA[i]*intervals[i] ;

      yParamsA[i] := ( points[i].Y - points[i-1].Y )/intervals[i] ;
      yParamsB[i] := points[i].Y - yParamsA[i]*intervals[i] ;

      zParamsA[i] := ( points[i].Z - points[i-1].Z )/intervals[i] ;
      zParamsB[i] := points[i].Z - zParamsA[i]*intervals[i] ;
    end ;

  end ;


  function TGIS_LinearPath.interpolate(
    const _t : Double
  ) : TGIS_Point3D ;
  var
    pd : Double ;
    nd : Double ;
    i  : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point3D.Create ;
    {$ENDIF}
    nextPointByDist( _t, i, pd, nd ) ;

    Result.X := xParamsA[i]*( _t - pd ) + xParamsB[i] ;
    Result.Y := yParamsA[i]*( _t - pd ) + yParamsB[i] ;
    Result.Z := zParamsA[i]*( _t - pd ) + zParamsB[i] ;
  end ;


//==============================================================================
// TGIS_CubicSplines
//==============================================================================

  procedure TGIS_CubicSplines.prepare ;
  var
    mtr : TGIS_SquareMatrix ;
    rhs : TGIS_Vector ;
    i   : Integer ;
    d   : Integer ;
    u   : Double ;
    a   : Double ;
    b   : Double ;
  begin
    SetLength( xParamsA, points.Count ) ;
    SetLength( xParamsB, points.Count ) ;
    SetLength( yParamsA, points.Count ) ;
    SetLength( yParamsB, points.Count ) ;
    SetLength( zParamsA, points.Count ) ;
    SetLength( zParamsB, points.Count ) ;

    SetLength( rhs, points.Count ) ;
    mtr := TGIS_SquareMatrix.Create( points.Count ) ;
    try

      mtr.MakeIdentity ;
      d := mtr.Dimension - 1 ;
      mtr[0,0] := 2.0 ;
      mtr[0,1] := 1.0 ;
      for i := 1 to d - 1 do begin
        u := intervals[i+1]/( intervals[i] + intervals[i+1] ) ;
        mtr[i,i-1] := 1.0 - u ;
        mtr[i,i]   := 2.0 ;
        mtr[i,i+1] := u ;
      end ;
      mtr[d,d-1] := 1.0 ;
      mtr[d,d]   := 2.0 ;

      // X
      a := ( points[1].X - points[0].X )/intervals[1] ;
      rhs[0] := ( 6.0/intervals[1] )*
                ( ( ( points[1].X - points[0].X )/intervals[1] ) - a ) ;
      for i := 1 to d - 1 do begin
        rhs[i] := ( 6.0/( intervals[i] + intervals[i+1] ) )*
                   (
                     ( ( points[i+1].X - points[i].X )/intervals[i+1] ) -
                     ( ( points[i].X - points[i-1].X )/intervals[i] )
                   ) ;
      end ;
      if Looped then
        b := a
      else
        b := ( points[d].X - points[d-1].X )/intervals[d] ;
      rhs[d] := ( 6.0/intervals[d] )*
        ( b - ( ( points[d].X - points[d-1].X )/intervals[d] ) ) ;


      xParamsM := mtr.Solve( rhs ) ;

      for i := 1 to d do begin
        xParamsA[i] :=
          ( ( points[i].X - points[i-1].X )/intervals[i] ) -
          ( intervals[i]/6.0 )*( xParamsM[i] - xParamsM[i-1] ) ;
        xParamsB[i] :=
          points[i-1].X - xParamsM[i-1]*Sqr( intervals[i] )/6.0 ;
      end ;

      // Y
      a := ( points[1].Y - points[0].Y )/intervals[1] ;
      rhs[0] := ( 6.0/intervals[1] )*
                ( ( ( points[1].Y - points[0].Y )/intervals[1] ) - a ) ;
      for i := 1 to d - 1 do begin
        rhs[i] := ( 6.0/( intervals[i] + intervals[i+1] ) )*
                   (
                     ( ( points[i+1].Y - points[i].Y )/intervals[i+1] ) -
                     ( ( points[i].Y - points[i-1].Y )/intervals[i] )
                   ) ;
      end ;
      if Looped then
        b := a
      else
        b := ( points[d].Y - points[d-1].Y )/intervals[d] ;
      rhs[d] := ( 6.0/intervals[d] )*
        ( b - ( ( points[d].Y - points[d-1].Y )/intervals[d] ) ) ;


      yParamsM := mtr.Solve( rhs ) ;

      for i := 1 to d do begin
        yParamsA[i] :=
          ( ( points[i].Y - points[i-1].Y )/intervals[i] ) -
          ( intervals[i]/6.0 )*( yParamsM[i] - yParamsM[i-1] ) ;
        yParamsB[i] :=
          points[i-1].Y - yParamsM[i-1]*Sqr( intervals[i] )/6.0 ;
      end ;

      // Z
      a := ( points[1].Z - points[0].Z )/intervals[1] ;
      rhs[0] := ( 6.0/intervals[1] )*
                ( ( ( points[1].Z - points[0].Z )/intervals[1] ) - a ) ;
      for i := 1 to d - 1 do begin
        rhs[i] := ( 6.0/( intervals[i] + intervals[i+1] ) )*
                   (
                     ( ( points[i+1].Z - points[i].Z )/intervals[i+1] ) -
                     ( ( points[i].Z - points[i-1].Z )/intervals[i] )
                   ) ;
      end ;
      if Looped then
        b := a
      else
        b := ( points[d].Z - points[d-1].Z )/intervals[d] ;
      rhs[d] := ( 6.0/intervals[d] )*
        ( b - ( ( points[d].Z - points[d-1].Z )/intervals[d] ) ) ;

      zParamsM := mtr.Solve( rhs ) ;

      for i := 1 to d do begin
        zParamsA[i] :=
          ( ( points[i].Z - points[i-1].Z )/intervals[i] ) -
          ( intervals[i]/6.0 )*( zParamsM[i] - zParamsM[i-1] ) ;
        zParamsB[i] :=
          points[i-1].Z - zParamsM[i-1]*Sqr( intervals[i] )/6.0 ;
      end ;

    finally
      FreeObject( mtr ) ;
    end ;
  end ;


  function TGIS_CubicSplines.interpolate(
    const _t : Double
  ) : TGIS_Point3D ;
  var
    pd : Double ;
    nd : Double ;
    i  : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point3D.Create ;
    {$ENDIF}
    nextPointByDist( _t, i, pd, nd ) ;

    Result.X := xParamsM[i-1]*Power( nd - _t, 3 )/( 6*intervals[i] ) +
                xParamsM[i]*Power( _t - pd, 3 )/( 6*intervals[i] ) +
                xParamsA[i]*( _t - pd ) + xParamsB[i] ;

    Result.Y := yParamsM[i-1]*Power( nd - _t, 3 )/( 6*intervals[i] ) +
                yParamsM[i]*Power( _t - pd, 3 )/( 6*intervals[i] ) +
                yParamsA[i]*( _t - pd ) + yParamsB[i] ;

    Result.Z := zParamsM[i-1]*Power( nd - _t, 3 )/( 6*intervals[i] ) +
                zParamsM[i]*Power( _t - pd, 3 )/( 6*intervals[i] ) +
                zParamsA[i]*( _t - pd ) + zParamsB[i] ;
  end ;


//==============================================================================
// TGIS_SemivarianceAbstract
//==============================================================================

  constructor TGIS_SemivarianceAbstract.Create ;
  begin
    inherited ;

    Nugget := 0.0 ;
  end ;


  procedure TGIS_SemivarianceAbstract.doDestroy ;
  begin
    // nothing to free

    inherited ;
  end ;


  function TGIS_SemivarianceAbstract.fget_Nugget : Double ;
  begin
    Result := FNugget ;
  end ;


  procedure TGIS_SemivarianceAbstract.fset_Nugget(
    const _val : Double
  ) ;
  begin
    if _val >= 0.0 then
      FNugget := _val ;
  end ;


  function TGIS_SemivarianceAbstract.Prepare(
    const _list : TGIS_ShapeListStub
  ) : Boolean ;
  begin
    Result := True ;
  end ;


//==============================================================================
// TGIS_SemivariancePowerLaw
//==============================================================================

  constructor TGIS_SemivariancePowerLaw.Create ;
  begin
    inherited ;

    dAlpha := 1.0 ;
    Exponent := 1.5 ;
  end ;


  function TGIS_SemivariancePowerLaw.fget_Exponent : Double ;
  begin
    Result := FExponent ;
  end ;


  procedure TGIS_SemivariancePowerLaw.fset_Exponent(
    const _val : Double
  ) ;
  begin
    if ( _val >= 1.0 ) and ( _val < 2.0 ) then
      FExponent := _val ;
  end ;


  function TGIS_SemivariancePowerLaw.Prepare(
    const _list : TGIS_ShapeListStub
  ) : Boolean ;
  var
    num : Double ;
    den : Double ;
    dst : Double ;
    val : Double ;
    i   : Integer ;
    k   : Integer ;

    function g_dist(
      const _i : Integer ;
      const _k : Integer
    ) : Double ;
    var
      p1 : TGIS_Point ;
      p2 : TGIS_Point ;
    begin
      p1 := _TGIS_Point(_list[_i]) ;
      p2 := _TGIS_Point(_list[_k]) ;

      Result := Power(
        ( p1.X - p2.X )*( p1.X - p2.X ) +
        ( p1.Y - p2.Y )*( p1.Y - p2.Y ),
        0.75 // 0.5 * BETA, which in this case is 1.5 as a most universal value
      ) ;
    end ;

  begin
    Result := False ;

    num := 0.0 ;
    den := 0.0 ;
    for i := 0 to _list.Count - 1 do begin
      for k := i + 1 to _list.Count - 1 do begin
        dst := g_dist( i, k ) ;
        val := _list.Value[i] ;
        val := val -_list.Value[k] ;
        num := num + dst*( 0.5*val*val ) ;
        den := den + dst*dst ;
      end ;
    end ;

    if den = 0 then
      exit ;

    dAlpha := num/den ;

    Result := True ;
  end ;


  function TGIS_SemivariancePowerLaw.Calculate(
    const _dist : Double
  ) : Double ;
  begin
    if _dist < 1e-12 then
      Result := 0.0
    else
      Result := Nugget + dAlpha*Power( _dist, Exponent ) ;
  end ;


//==============================================================================
// TGIS_SemivarianceWithRange
//==============================================================================

  constructor TGIS_SemivarianceWithRange.Create ;
  begin
    inherited ;

    Range := 1.0 ;
  end ;


  function TGIS_SemivarianceWithRange.fget_Range  : Double ;
  begin
    Result := FRange ;
  end ;


  procedure TGIS_SemivarianceWithRange.fset_Range(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FRange := _val ;
  end ;


//==============================================================================
// TGIS_SemivarianceExponential
//==============================================================================

  constructor TGIS_SemivarianceExponential.Create ;
  begin
    inherited ;

  end ;


  function TGIS_SemivarianceExponential.Calculate(
    const _dist : Double
  ) : Double ;
  begin
    if _dist < 1e-12 then
      Result := 0.0
    else
      Result := Nugget + ( 1 - Exp( -_dist/Range ) ) ;
  end ;


//==============================================================================
// TGIS_SemivarianceGaussian
//==============================================================================

  constructor TGIS_SemivarianceGaussian.Create ;
  begin
    inherited ;

  end ;


  function TGIS_SemivarianceGaussian.Calculate(
    const _dist : Double
  ) : Double ;
  begin
    if _dist < 1e-12 then
      Result := 0.0
    else
      Result := Nugget + ( 1 - Exp( -( _dist*_dist )/( Range*Range ) ) ) ;
  end ;


//==============================================================================
// TGIS_SemivarianceWithSill
//==============================================================================

  constructor TGIS_SemivarianceWithSill.Create ;
  begin
    inherited ;

    Sill := 1.0 ;
    relativeSill := 1.0 ;
  end ;


  procedure TGIS_SemivarianceWithSill.fset_Nugget(
    const _val : Double
  ) ;
  begin
    inherited ;

    if Nugget > Sill then
      Nugget := Sill ;

    relativeSill := Sill - Nugget ;
  end ;


  function TGIS_SemivarianceWithSill.fget_Sill : Double ;
  begin
    Result := FSill ;
  end ;


  procedure TGIS_SemivarianceWithSill.fset_Sill(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FSill := _val ;

    if Sill < Nugget then
      FSill := Nugget ;

    relativeSill := Sill - Nugget ;
  end ;


//==============================================================================
// TGIS_SemivarianceSpherical
//==============================================================================

  function TGIS_SemivarianceSpherical.Calculate(
    const _dist : Double
  ) : Double ;
  var
    rat : Double ;
  begin
    if _dist < 1e-12 then
      Result := 0.0
    else
    if _dist <= Range then begin
      rat := _dist/Range ;
      Result := Nugget + relativeSill*( 1.5*rat - 0.5*Power( rat, 3 ) ) ;
    end
    else
      Result := Sill ;
  end ;


//==============================================================================
// TGIS_SemivarianceCircular
//==============================================================================

  function TGIS_SemivarianceCircular.Calculate(
    const _dist : Double
  ) : Double ;
  var
    rat : Double ;
  begin
    if _dist < 1e-12 then
      Result := 0.0
    else
    if _dist <= Range then begin
      rat := _dist/Range ;
      Result := Nugget +
        ( 2.0*relativeSill/Pi )*( rat*Sqrt( 1.0 - rat*rat ) + ArcSin( rat ) ) ;
    end
    else
      Result := Sill ;
  end ;


//==============================================================================
// TGIS_SemivarianceLinear
//==============================================================================

  function TGIS_SemivarianceLinear.Calculate(
    const _dist : Double
  ) : Double ;
  begin
    if _dist < 1e-12 then
      Result := 0.0
    else
    if _dist <= Range then
      Result := Nugget + relativeSill*_dist/Range
    else
      Result := Sill ;
  end ;

  {$REGION 'TGIS_VectorToGridAbstract'}
  constructor TGIS_VectorToGridAbstract.Create ;
  begin
    inherited ;

    Coordinate := TGIS_VectorToGridCoordinate.Z ;
    UseDefaultValue := T_INTERPOLATION.USE_DEFVAL ;
    DefaultValue := T_INTERPOLATION.DEFVAL ;

    busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
  end ;

  procedure TGIS_VectorToGridAbstract.doDestroy ;
  begin
    FreeObject( busyEventManager ) ;

    inherited ;
  end ;

  procedure TGIS_VectorToGridAbstract.Generate(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _srcfld : String ;
    const _dst    : TGIS_LayerPixel ;
    const _dstext : TGIS_Extent
  ) ;
  var
    cs  : TGIS_CSCoordinateSystem ;
    ext : TGIS_Extent ;
  begin
    if not assigned( _src ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_SRC, 1
      ) ;

    if not assigned( _dst ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 1
      ) ;

    if not _dst.IsGridImage then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 2
      ) ;

    ext := GisCommonExtent( _src.Extent, _srcext ) ;
    if GisIsEmptyExtent( ext ) then
      // for source with only one point common extent is empty
      if not ( GisIsSameValue( ext.XMin, ext.XMax ) and
         GisIsSameValue( ext.YMin, ext.YMax ) )
      then
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_SRCEXT, 1
        ) ;

    ext := GisCommonExtent( _dst.Extent, _dstext ) ;
    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DSTEXT, 1
      ) ;

    if assigned( _src.Viewer ) then
      cs := _src.Viewer.Ref.CS
    else
      cs := _src.CS ;

    ext := GisCommonExtent(
      cs.ExtentFromCS( _src.CS, _srcext ),
      cs.ExtentFromCS( _dst.CS, _dstext )
    ) ;
    if GisIsEmptyExtent( ext ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DSTEXT, 2
      ) ;

    if length( _srcfld ) > 0 then begin
      if ( _src.FindField( _srcfld ) < 0 ) or
         ( not isNumericField( _srcfld, _src ) ) then
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_SRCFLD, 1
        ) ;
    end ;

    _src.Open ;
    _dst.Open ;
  end ;

  function TGIS_VectorToGridAbstract.isNumericField(
    const _fld : String ;
    const _lv  : TGIS_LayerVector
  ) : Boolean ;
  var
    fld_info : TGIS_FieldInfo ;
    fld_indx : Integer ;
  begin
    Result := False ;

    fld_indx := _lv.FindField( _fld ) ;
    if fld_indx < 0 then
      exit ;

    fld_info := _lv.FieldInfo( fld_indx ) ;
    case fld_info.FieldType of
      TGIS_FieldType.Number,
      TGIS_FieldType.Float  : Result := True ;
    end ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_VectorToGridAbstract.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_VectorToGridAbstract.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_VectorToGridAbstract.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_VectorToGridAbstract.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}
  {$ENDREGION}

  {$REGION 'TGIS_InterpolationKriging'}
  constructor TGIS_InterpolationKriging.Create ;
  begin
    inherited ;

    FWindowed := False ;
    FMinPoints := 4 ;
    FMaxPoints := 4 ;
    FRadius := 1.0 ;

    Semivariance := TGIS_SemivariancePowerLaw.Create ;
  end ;

  procedure TGIS_InterpolationKriging.doDestroy ;
  begin
    FreeObject( FSemivariance ) ;

    inherited ;
  end ;

  function TGIS_InterpolationKriging.fget_MinPoints : Integer ;
  begin
    Result := FMinPoints ;
  end ;

  procedure TGIS_InterpolationKriging.fset_MinPoints(
    const _val : Integer
  ) ;
  begin
    if _val < 2 then
      exit ;

    FMinPoints := _val ;

    if FMaxPoints < _val then
      FMaxPoints := _val ;
  end ;

  function TGIS_InterpolationKriging.fget_MaxPoints : Integer ;
  begin
    Result := FMaxPoints ;
  end ;

  procedure TGIS_InterpolationKriging.fset_MaxPoints(
    const _val : Integer
  ) ;
  begin
    if _val < 2 then
      exit ;

    FMaxPoints := _val ;

    if FMinPoints > _val then
      FMinPoints := _val ;
  end ;

  function TGIS_InterpolationKriging.fget_Radius : Double ;
  begin
    Result := FRadius ;
  end ;

  procedure TGIS_InterpolationKriging.fset_Radius(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FRadius := _val ;
  end ;

  function TGIS_InterpolationKriging.fget_Semivariance
    : TGIS_SemivarianceAbstract ;
  begin
    Result := FSemivariance ;
  end ;

  procedure TGIS_InterpolationKriging.fset_Semivariance(
    const _obj : TGIS_SemivarianceAbstract
  ) ;
  begin
    if assigned( _obj ) then begin
      FreeObject( FSemivariance ) ;
      FSemivariance := _obj ;
    end ;
  end ;

  procedure TGIS_InterpolationKriging.Generate(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _srcfld : String ;
    const _dst    : TGIS_LayerPixel ;
    const _dstext : TGIS_Extent
  ) ;
  var
    ext  : TGIS_Extent ;
    pix  : TGIS_LayerPixelLock ;
    lst  : T_pointList ;
    tmp  : T_pointList ;
    ats  : Boolean ;
    pt   : TGIS_Point ;
    mvg  : TGIS_SquareMatrix ;
    vvg  : TGIS_Vector ;
    vev  : TGIS_Vector ;
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    w    : Integer ;
    h    : Integer ;
    i    : Integer ;
    pmax : Int64 ;
    abrt : Boolean ;

    function g_variogram(
      const _i : Integer ;
      const _k : Integer
    ) : Double ;
    var
      pp1 : TGIS_Point ;
      pp2 : TGIS_Point ;
      dd  : Double ;
    begin
      if ( _i = lst.Count ) or ( _k = lst.Count ) then begin
        if _i = _k then
          Result := 0.0
        else
          Result := 1.0 ;
      end
      else begin
        if _i = _k then
          Result := 0.0
        else begin

          if _i = -1 then
            pp1 := pt
          else
            pp1 := _TGIS_Point(lst[_i]) ;

          if _k = -1 then
            pp2 := _TGIS_Point(pt)
          else
            pp2 := _TGIS_Point(lst[_k]) ;

          dd := GisPoint2Point( pp1 ,pp2 ) ;
          Result := Semivariance.Calculate( dd ) ;
        end ;
      end ;
    end ;

    function g_no_value : Single ;
    begin
      if UseDefaultValue then
        Result := DefaultValue
      else
        Result := _dst.NoDataValue ;
    end ;

    procedure prep_matrix ;
    var
      ii : Integer ;
      kk : Integer ;
    begin
      SetLength( vvg, lst.Count + 1 ) ;
      SetLength( vev, lst.Count + 1 ) ;
      mvg := TGIS_SquareMatrix.Create( lst.Count + 1 ) ;

      for ii := 0 to lst.Count do begin
        for kk := ii to lst.Count do begin
          mvg[ii,kk] := g_variogram( ii, kk ) ;
          if kk <> ii then
            mvg[kk,ii] := mvg[ii,kk] ;
        end ;
        vev[ii] := lst.Value[ii] ;
      end ;
    end ;

  begin
    inherited Generate( _src, _srcext, _srcfld, _dst, _dstext ) ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    pix := _dst.LockPixels( _dstext, _dst.CS, True ) ;
    lst := T_pointList.Create( Coordinate, _srcfld ) ;
    pmax := ( pix.Bounds.Bottom - pix.Bounds.Top + 1 ) *
      ( pix.Bounds.Right - pix.Bounds.Left + 1)  ;
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_INTERPOLATION_KRIGING ), pmax
    ) ;
    try
      if not Windowed then begin
        lst.Prepare( _src, _dst, _dstext ) ;
        if lst.Count = 0 then
          exit ;

        if not Semivariance.Prepare( lst ) then
          exit ;

        prep_matrix ;
        if not mvg.Invert then
          exit ;
        vev := mvg.Multiply( vev ) ;
      end
      else
        mvg := nil ;

      for h := pix.Bounds.Top to pix.Bounds.Bottom do begin
        for w := pix.Bounds.Left to pix.Bounds.Right do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          pt := pix.RasterToMap( Point( w, h ), _dst.CS ) ;

          if Windowed then begin

            ext := GisExtent(
                     pt.X - Radius, pt.Y - Radius,
                     pt.X + Radius, pt.Y + Radius
                   ) ;

            tmp := T_pointList.Create( Coordinate, _srcfld ) ;
            tmp.Threshold := MaxPoints ;
            tmp.Prepare( _src, _dst, ext ) ;
            tmp.Sort( pt, Radius ) ;
            ats := lst.Equal( tmp ) ;
            if ats then
              FreeObject( tmp )
            else begin
              FreeObject( lst ) ;
              lst := tmp ;
            end ;

            if lst.Count < MinPoints then begin
              pix.Grid[h,w] := g_no_value ;
              continue ;
            end ;

            if not ats then
              FreeObject( mvg ) ;

            if not ats then begin

              if not Semivariance.Prepare( lst ) then begin
                pix.Grid[h,w] := g_no_value ;
                continue ;
              end ;

              prep_matrix ;
              if not mvg.Invert then begin
                pix.Grid[h,w] := g_no_value ;
                continue ;
              end ;
              vev := mvg.Multiply( vev ) ;

            end ;
          end ;

          for i := 0 to lst.Count do
            vvg[i] := g_variogram( -1, i ) ;

          val := GisDotProduct( vvg, vev ) ;

          pix.Grid[h,w] := val ;
          vmin := Min( vmin, val ) ;
          vmax := Max( vmax, val ) ;
        end ;
      end ;
    finally
      _dst.MinHeight := vmin ;
      _dst.MaxHeight := vmax ;
      busyEventManager.EndEvent ;
      FreeObject( mvg ) ;
      FreeObject( lst ) ;
      _dst.UnlockPixels( pix ) ;
    end ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_InterpolationIDW'}
  constructor TGIS_InterpolationIDW.Create ;
  begin
    inherited ;

    FWindowed := False ;
    FRadius := 1.0 ;
    FExponent := T_INTERPOLATION.EXPONENT ;
  end ;

  function TGIS_InterpolationIDW.fget_Radius : Double ;
  begin
    Result := FRadius ;
  end ;

  procedure TGIS_InterpolationIDW.fset_Radius(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FRadius := _val ;
  end ;

  function TGIS_InterpolationIDW.fget_Exponent : Double ;
  begin
    Result := FExponent ;
  end ;

  procedure TGIS_InterpolationIDW.fset_Exponent(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FExponent := _val
    else
      FExponent := T_INTERPOLATION.EXPONENT ;
  end ;

  procedure TGIS_InterpolationIDW.Generate(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _srcfld : String ;
    const _dst    : TGIS_LayerPixel ;
    const _dstext : TGIS_Extent
  ) ;
  var
    ext  : TGIS_Extent ;
    pix  : TGIS_LayerPixelLock ;
    lst  : T_pointList ;
    pt   : TGIS_Point ;
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    w    : Integer ;
    h    : Integer ;
    i    : Integer ;
    dist : Double ;
    wght : Double ;
    num  : Double ;
    den  : Double ;
    pmax : Int64 ;
    abrt : Boolean ;

    function g_dist(
      const _i : Integer
    ) : Double ;
    var
      pp : TGIS_Point ;
      dd : Double ;
    begin
      pp := _TGIS_Point(lst[_i]) ;

      dd := ( pt.X - pp.X )*( pt.X - pp.X ) +
            ( pt.Y - pp.Y )*( pt.Y - pp.Y ) ;
      if Windowed then
        Result := Sqrt( dd )
      else begin
        if Exponent = 2.0 then
          Result := dd
        else
          Result := Power( dd, Exponent/2.0 ) ;
      end ;
    end ;

    function g_weight : Double ;
    begin
      if Windowed then
        Result := Power( Max( 0, Radius - dist )/( Radius*dist ), Exponent )
      else
        Result := 1.0/dist ;
    end ;

    function g_no_value : Single ;
    begin
      if UseDefaultValue then
        Result := DefaultValue
      else
        Result := _dst.NoDataValue ;
    end ;

  begin
    inherited Generate( _src, _srcext, _srcfld, _dst, _dstext ) ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    pix := _dst.LockPixels( _dstext, _dst.CS, True ) ;
    lst := T_pointList.Create( Coordinate, _srcfld ) ;
    pmax := ( pix.Bounds.Height + 1 ) * ( pix.Bounds.Width + 1 ) ;
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_INTERPOLATION_IDW ), pmax
    ) ;
    try
      if not Windowed then begin
        lst.Prepare( _src, _dst, _dstext ) ;
        if lst.Count = 0 then
          exit ;
      end ;

      for h := pix.Bounds.Top to pix.Bounds.Bottom do begin
        for w := pix.Bounds.Left to pix.Bounds.Right do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          pt := pix.RasterToMap( Point( w, h ), _dst.CS ) ;

          if Windowed then begin
            ext := GisExtent(
                     pt.X - Radius, pt.Y - Radius,
                     pt.X + Radius, pt.Y + Radius
                   ) ;

            lst.Prepare( _src, _dst, ext ) ;

            if lst.Count = 0 then begin
              pix.Grid[h,w] := g_no_value ;
              continue ;
            end ;
          end ;

          num := 0.0 ;
          den := 0.0 ;
          for i := 0 to lst.Count - 1 do begin
            dist := g_dist( i ) ;
            if dist < 1e-12 then begin
              num := lst.Value[i] ;
              den := 1.0 ;
              break ;
            end ;
            wght := g_weight ;
            num := num + wght*lst.Value[i] ;
            den := den + wght ;
          end ;

          if den = 0 then begin
            pix.Grid[h,w] := g_no_value ;
            continue ;
          end ;

          if den = 1.0 then
            val := num
          else
            val := num/den ;

          pix.Grid[h,w] := val ;
          vmin := Min( vmin, val ) ;
          vmax := Max( vmax, val ) ;
        end ;
      end ;
    finally
      _dst.MinHeight := vmin ;
      _dst.MaxHeight := vmax ;
      busyEventManager.EndEvent ;
      FreeObject( lst ) ;
      _dst.UnlockPixels( pix ) ;
    end ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_GaussianHeatmap'}
  constructor TGIS_GaussianHeatmap.Create ;
  begin
    inherited ;

    Coordinate := TGIS_VectorToGridCoordinate.None ;
    FRadius := 1.0 ;
  end ;

  function TGIS_GaussianHeatmap.fget_Radius : Double ;
  begin
    Result := FRadius ;
  end ;

  procedure TGIS_GaussianHeatmap.fset_Radius(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FRadius := _val ;
  end ;

  procedure TGIS_GaussianHeatmap.Generate(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _srcfld : String ;
    const _dst    : TGIS_LayerPixel ;
    const _dstext : TGIS_Extent
  ) ;
  var
    ext  : TGIS_Extent ;
    pix  : TGIS_LayerPixelLock ;
    lst  : T_pointList ;
    dx   : Double ;
    dy   : Double ;
    rad2 : Double ;
    pt   : TGIS_Point ;
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    w    : Integer ;
    h    : Integer ;
    i    : Integer ;
    dist : Double ;
    ws   : Integer ;
    hs   : Integer ;
    wc   : Integer ;
    hc   : Integer ;
    hr   : Integer ;
    ps   : TGIS_Point ;
    val0 : Double ;
    tmin : Single ;
    tmax : Single ;
    la   : Double ;
    lb   : Double ;
    abrt : Boolean ;

    function g_no_value : Single ;
    begin
      if UseDefaultValue then
        Result := DefaultValue
      else
        Result := _dst.NoDataValue ;
    end ;

  begin
    inherited Generate( _src, _srcext, _srcfld, _dst, _dstext ) ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    tmin :=  GIS_MAX_SINGLE ;
    tmax := -GIS_MAX_SINGLE ;

    pix := _dst.LockPixels( _dstext, _dst.CS, True ) ;
    lst := T_pointList.Create( Coordinate, _srcfld ) ;
    try
      lst.Prepare( _src, _dst, _dstext ) ;
      if lst.Count = 0 then
        exit ;

      busyEventManager.StartEvent(
        _rsrc( GIS_RS_BUSY_INTERPOLATION_HEATMAP ), lst.Count
      ) ;
      try
        rad2 := Radius*Radius ;

        ext := _TGIS_Extent(_dstext) ;

        dx := Abs( ext.XMax - ext.XMin )/( pix.Bounds.Width  + 1 ) ;
        dy := Abs( ext.YMax - ext.YMin )/( pix.Bounds.Height + 1 ) ;

        wc := RoundS( Radius/dx ) ;
        hc := RoundS( Radius/dy ) ;

        for h := pix.Bounds.Top to pix.Bounds.Bottom do begin
          for w := pix.Bounds.Left to pix.Bounds.Right do
            pix.Grid[h,w] := g_no_value ;
        end ;

        for i := 0 to lst.Count - 1 do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          pt := _TGIS_Point(lst[i]) ;
          val0 := lst.Value[i] ;

          ws := RoundS( (  pt.X - ext.XMin )/dx ) - wc ;
          hs := RoundS( (  pt.Y - ext.YMin )/dy ) - hc ;

          for h := hs to hs + 2*hc do begin

            if ( h < pix.Bounds.Top ) or ( h > pix.Bounds.Bottom ) then
              continue ;

            for w := ws to ws + 2*wc do begin

              if ( w < pix.Bounds.Left ) or ( w > pix.Bounds.Right ) then
                continue ;

              ps := GisPoint( ext.XMin+w*dx, ext.YMin+h*dy ) ;
              dist := Sqr( pt.X - ps.X ) + Sqr( pt.Y - ps.Y ) ;
              if dist > rad2 then
                continue ;

              val := val0*Exp( -9*dist/( 2*rad2 ) ) ;

              hr := pix.Bounds.Height -h ;
              if pix.Grid[hr,w] <> GIS_GRID_NOVALUE then
                val := val + pix.Grid[hr,w] ;

              pix.Grid[hr,w] := val ;

              vmin := Min( vmin, val0 ) ;
              vmax := Max( vmax, val0 ) ;

              tmin := Min( tmin, val ) ;
              tmax := Max( tmax, val ) ;
            end ;
          end ;
        end ;

        if Coordinate <> TGIS_VectorToGridCoordinate.None then begin

          la := ( vmax - vmin )/( tmax - tmin ) ;
          lb := vmin - la*tmin ;

          vmin :=  GIS_MAX_SINGLE ;
          vmax := -GIS_MAX_SINGLE ;

          for h := 0 to pix.Bounds.Height do begin
            for w := 0 to pix.Bounds.Width do begin

              if pix.Grid[h,w] = GIS_GRID_NOVALUE then
                continue ;

              val := pix.Grid[h,w] ;
              val := la*val + lb ;
              pix.Grid[h,w] := val ;

              vmin := Min( vmin, val ) ;
              vmax := Max( vmax, val ) ;
            end ;
          end ;

        end
        else begin
          vmin := tmin ;
          vmax := tmax ;
        end ;
      finally
        busyEventManager.EndEvent ;
      end;
    finally
      _dst.MinHeight := vmin ;
      _dst.MaxHeight := vmax ;
      _dst.UnlockPixels( pix ) ;
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_GaussianHeatmap.EstimateRadius(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _dst    : TGIS_LayerPixel
  ) ;
  var
    cs    : TGIS_CSCoordinateSystem ;
    ext   : TGIS_Extent ;
    lst   : TList<TGIS_Point> ;
    en    : TGIS_LayerVectorEnumerator ;
    shp   : TGIS_Shape ;
    srad  : Double ;
    rad   : Double ;
    avg   : Double ;
    i     : Integer ;
    k     : Integer ;
  begin
    if assigned( _src.Viewer ) then
      cs := _src.Viewer.Ref.CS
    else
      cs := _src.CS ;

    lst := TList<TGIS_Point>.Create ;
    try

      en := _src.Loop( _srcext ).GetEnumerator ;
      try
        while en.MoveNext do begin
          shp := en.GetCurrent ;
          lst.Add( _dst.CS.FromCS( cs, shp.Centroid ) ) ;
        end ;
      finally
        FreeObject( en ) ;
      end ;

      if lst.Count < 2 then
        exit ;

      ext := _dst.CS.ExtentFromCS( _src.CS, _srcext ) ;
      srad := Sqrt(
        Sqr( ext.XMax - ext.XMin ) +
        Sqr( ext.YMax - ext.YMin )
      ) ;

      avg := 0.0 ;
      for i := 0 to lst.Count - 1 do begin
        rad := srad ;
        for k := 0 to lst.Count - 1 do begin
          if k = i then
            continue ;
          rad := Min( rad, GisPoint2Point( lst[i], lst[k] ) ) ;
        end ;
        avg := avg + rad ;
      end ;

      Radius := 3.0*avg/lst.Count ;

    finally
      FreeObject( lst ) ;
    end ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_InterpolationSplines'}
  constructor TGIS_InterpolationSplines.Create ;
  begin
    inherited ;

    FWindowed := False ;
    FMinPoints := 4 ;
    FMaxPoints := 4 ;
    FRadius := 1.0 ;
    FTension := 1.0 ;

    dApproxC1 := ( 1 - Exp( -LOCAL_EULER_CONSTANT ) )*10.8 ;
    dApproxC2 := 5.874 + 5/3 ;
  end ;

  function TGIS_InterpolationSplines.fget_MinPoints : Integer ;
  begin
    Result := FMinPoints ;
  end ;

  procedure TGIS_InterpolationSplines.fset_MinPoints(
    const _val : Integer
  ) ;
  begin
    if _val < 2 then
      exit ;

    FMinPoints := _val ;

    if FMaxPoints < _val then
      FMaxPoints := _val ;
  end ;

  function TGIS_InterpolationSplines.fget_MaxPoints : Integer ;
  begin
    Result := FMaxPoints ;
  end ;

  procedure TGIS_InterpolationSplines.fset_MaxPoints(
    const _val : Integer
  ) ;
  begin
    if _val < 2 then
      exit ;

    FMaxPoints := _val ;

    if FMinPoints > _val then
      FMinPoints := _val ;
  end ;

  function TGIS_InterpolationSplines.fget_Radius : Double ;
  begin
    Result := FRadius ;
  end ;

  procedure TGIS_InterpolationSplines.fset_Radius(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FRadius := _val ;
  end ;

  function TGIS_InterpolationSplines.fget_Tension : Double ;
  begin
    Result := FTension ;
  end ;

  procedure TGIS_InterpolationSplines.fset_Tension(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FTension := _val
    else
      FTension := 1.0 ;
  end ;

  function TGIS_InterpolationSplines.approxExpIntE1(
    const _arg : Double
  ) : Double ;
  var
    x   : Double ;
    xx  : Double ;
    ox  : Double ;
    fx  : Double ;
    tmp : Double ;
  begin
    x := _arg ;
    xx := x*x ;
    ox := 1.0/x ;

    fx := ( xx + 5.874*x + dApproxC1 )/( xx + dApproxC2*x + 10.8 ) ;
    tmp := 1.0 + ox - ( ox - Ln( 1.0 + ox ) )*fx ;

    Result := Exp( -x )*Ln( tmp ) ;
  end ;

  procedure TGIS_InterpolationSplines.Generate(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _srcfld : String ;
    const _dst    : TGIS_LayerPixel ;
    const _dstext : TGIS_Extent
  ) ;
  var
    ext  : TGIS_Extent ;
    pix  : TGIS_LayerPixelLock ;
    lst  : T_pointList ;
    tmp  : T_pointList ;
    ats  : Boolean ;
    pt   : TGIS_Point ;
    mcf  : TGIS_SquareMatrix ;
    vcf  : TGIS_Vector ;
    vvl  : TGIS_Vector ;
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    w    : Integer ;
    h    : Integer ;
    i    : Integer ;
    pmax : Integer ;
    abrt : Boolean ;

    function g_no_value : Single ;
    begin
      if UseDefaultValue then
        Result := DefaultValue
      else
        Result := _dst.NoDataValue ;
    end ;

    function g_crs( const _p1 : TGIS_Point ;
                    const _p2 : TGIS_Point ) : Double ;
    var
     dd : Double ;
     aa : Double ;
    begin
      dd := GisPoint2Point( _p1, _p2 ) ;
      if dd < 1e-12 then
        Result := 0.0
      else begin
        aa := Tension*dd*dd/4 ;
        Result := -( Tension/( 4*Pi ) )*(
                    Ln( aa ) +
                    approxExpIntE1( aa ) +
                    LOCAL_EULER_CONSTANT
                  ) ;
      end ;
    end ;

    procedure calc_coeff ;
    var
      ii : Integer ;
      kk : Integer ;
    begin
      SetLength( vcf, lst.Count + 1 ) ;
      SetLength( vvl, lst.Count + 1 ) ;
      mcf := TGIS_SquareMatrix.Create( lst.Count + 1 ) ;

      for ii := 0 to lst.Count - 1 do begin
        vvl[ii] := lst.Value[ii] ;
        mcf[ii,0] := 1.0 ;
        mcf[lst.Count,ii+1] := 1.0 ;
      end ;
      vvl[lst.Count] := 0.0 ;
      mcf[lst.Count,0] := 0.0 ;

      for ii := 0 to lst.Count - 1 do begin
        for kk := ii to lst.Count - 1 do begin
          mcf[ii,kk+1] := g_crs( lst[ii], lst[kk] ) ;
          if kk <> ii then
            mcf[kk,ii+1] := mcf[ii,kk+1] ;
        end ;
      end ;

      vcf := mcf.Solve( vvl ) ;

      FreeObject( mcf ) ;
    end ;

  begin
    inherited Generate( _src, _srcext, _srcfld, _dst, _dstext ) ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    pix := _dst.LockPixels( _dstext, _dst.CS, True ) ;
    lst := T_pointList.Create( Coordinate, _srcfld ) ;
    pmax := ( pix.Bounds.Height + 1 )*( pix.Bounds.Width + 1 ) ;
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_INTERPOLATION_SPLINES ), pmax
    ) ;
    try
      if not Windowed then begin
        lst.Prepare( _src, _dst, _dstext ) ;
        if lst.Count = 0 then
          exit ;

        calc_coeff ;

        if length( vcf ) = 0 then
          exit ;
      end ;

      for h := pix.Bounds.Top to pix.Bounds.Bottom do begin
        for w := pix.Bounds.Left to pix.Bounds.Right do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          pt := pix.RasterToMap( Point( w, h ), _dst.CS ) ;

          if Windowed then begin
            ext := GisExtent(
                     pt.X - Radius, pt.Y - Radius,
                     pt.X + Radius, pt.Y + Radius
                   ) ;

            tmp := T_pointList.Create( Coordinate, _srcfld ) ;
            tmp.Threshold := MaxPoints ;
            tmp.Prepare( _src, _dst, ext ) ;
            tmp.Sort( pt, Radius ) ;
            ats := lst.Equal( tmp ) ;
            if ats then
              FreeObject( tmp )
            else begin
              FreeObject( lst ) ;
              lst := tmp ;
            end ;

            if lst.Count < MinPoints then begin
              pix.Grid[h,w] := g_no_value ;
              continue ;
            end ;

            if not ats then
              calc_coeff ;

            if length( vcf ) = 0 then begin
              pix.Grid[h,w] := g_no_value ;
              continue ;
            end ;
          end ;

          val := vcf[0] ;
          for i := 0 to lst.Count - 1 do
            val := val + vcf[i+1]*g_crs( pt, lst[i] ) ;

          pix.Grid[h,w] := val ;
          vmin := Min( vmin, val ) ;
          vmax := Max( vmax, val ) ;
        end ;
      end ;
    finally
      _dst.MinHeight := vmin ;
      _dst.MaxHeight := vmax ;
      _dst.UnlockPixels( pix ) ;
      FreeObject( lst ) ;
      busyEventManager.EndEvent ;
    end ;
  end ;
  {$ENDREGION}

  {$REGION 'TGIS_TinToGrid'}
  constructor TGIS_TinToGrid.Create ;
  begin
    inherited ;

    FExponent := T_INTERPOLATION.EXPONENT ;
    busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
  end ;

  procedure TGIS_TinToGrid.doDestroy ;
  begin
    FreeObject( busyEventManager ) ;

    inherited ;
  end ;

  function TGIS_TinToGrid.fget_Exponent : Double ;
  begin
    Result := FExponent ;
  end ;

  procedure TGIS_TinToGrid.fset_Exponent(
    const _val : Double
  ) ;
  begin
    if _val > 0.0 then
      FExponent := _val
    else
      FExponent := T_INTERPOLATION.EXPONENT ;
  end ;

    {$IFDEF CLR}
    procedure TGIS_TinToGrid.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_TinToGrid.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_TinToGrid.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_TinToGrid.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}

  function TGIS_TinToGrid.generateCPU : Boolean ;
  var
    src  : TGIS_LayerVector ;
    dst  : TGIS_LayerPixel ;

    ext  : TGIS_Extent ;
    exts : TGIS_Extent ;
    xsiz : Integer ;
    ysiz : Integer ;
    psiz : TGIS_Point ;
    ws   : Integer ;
    we   : Integer ;
    hs   : Integer ;
    he   : Integer ;
    prt  : Integer ;
    vp   : TGIS_Point ;
    v1   : TGIS_Point ;
    v2   : TGIS_Point ;
    d12  : Double ;
    u    : Double ;
    v    : Double ;
    cs   : TGIS_CSCoordinateSystem ;
    {$IFDEF DCC}
      obj : TGIS_Shape ;
    {$ENDIF}
    shp  : TGIS_Shape ;
    pix  : TGIS_LayerPixelLock ;
    pt   : TGIS_Point ;
    p0   : TGIS_Point3D ;
    p1   : TGIS_Point3D ;
    p2   : TGIS_Point3D ;
    w0   : Double ;
    w1   : Double ;
    w2   : Double ;
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    w    : Integer ;
    h    : Integer ;
    // progress
    pcnt : Int64 ;
    abrt : Boolean ;

    function dist_sqr(
      const _p0 : TGIS_Point ;
      const _p1 : TGIS_Point
    ) : Double ;
    begin
      Result := Sqr( _p0.X - _p1.X ) + Sqr( _p0.Y - _p1.Y ) ;
    end ;

    function is3 : Boolean ;
    begin
      vp := GisPoint( pt.X - p0.X, pt.Y - p0.Y ) ;
      v1 := GisPoint( p1.X - p0.X, p1.Y - p0.Y ) ;
      v2 := GisPoint( p2.X - p0.X, p2.Y - p0.Y ) ;
      d12 := v1.X*v2.Y - v1.Y*v2.X ;

      u := vp.X*v2.Y - vp.Y*v2.X ;
      v := vp.X*v1.Y - vp.Y*v1.X ;
      u := u/d12 ;
      v := -v/d12 ;

      Result := ( u >= 0.0 ) and ( v >= 0.0 ) and ( u + v <= 1.0 ) ;
    end ;

  begin
    Result := False ;

    src := FSourceLayer ;
    dst := FOutputLayer ;

    if not assigned( src ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_SRC, 1
      ) ;

    if not assigned( dst ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 1
      ) ;

    if not dst.IsGridImage then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 2
      ) ;

    if src.CS.EPSG <> dst.CS.EPSG then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 3
      ) ;

    if not GisIsSameExtent( src.Extent, dst.Extent ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 4
      ) ;

    src.Open ;
    dst.Open ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    if assigned( src.Viewer ) then
      cs := src.Viewer.Ref.CS
    else
      cs := src.CS ;

    ext := GisCommonExtent(
      src.CS.ExtentToCS( dst.CS, src.Extent ),
      dst.Extent
    ) ;
    exts := cs.ExtentFromCS( dst.CS, ext ) ;

    pcnt := 0 ;
    for obj in src.Loop( exts ) do begin
      {$IFDEF DCC}
        shp := obj ;
      {$ELSE}
        shp := TGIS_Shape( obj ) ;
      {$ENDIF}
      inc( pcnt, shp.GetNumParts ) ;
    end ;

    pix := dst.LockPixels( ext, dst.CS, True ) ;
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_INTERPOLATION_TINTOGRID ), pcnt
    ) ;
    try
      xsiz := pix.Bounds.Width+1 ;
      ysiz := pix.Bounds.Height+1 ;
      psiz := GisPoint(
        ( ext.XMax - ext.XMin )/xsiz,
        ( ext.YMax - ext.YMin )/ysiz
      ) ;

      for obj in src.Loop( ext ) do begin
        {$IFDEF DCC}
          shp := obj ;
        {$ELSE}
          shp := TGIS_Shape( obj ) ;
        {$ENDIF}

        ws := pix.Bounds.Left +
              RoundS( ( shp.Extent.XMin - ext.XMin )/psiz.X ) ;
        we := pix.Bounds.Left +
              RoundS( ( shp.Extent.XMax - ext.XMin )/psiz.X ) - 1 ;
        he := pix.Bounds.Top -
              RoundS( ( shp.Extent.YMin - ext.YMax )/psiz.Y ) - 1 ;
        hs := pix.Bounds.Top -
              RoundS( ( shp.Extent.YMax - ext.YMax )/psiz.Y ) ;

        for prt := 0 to shp.GetNumParts - 1 do begin

          abrt := busyEventManager.PushEvent ;
          if abrt then
            exit ;

          p0 := shp.GetPoint3D( prt, 0 ) ;
          p1 := shp.GetPoint3D( prt, 1 ) ;
          p2 := shp.GetPoint3D( prt, 2 ) ;

          for h := hs to he do begin
            for w := ws to we do begin

              pt := pix.RasterToMap( Point( w, h ), cs ) ;

              if not is3 then
                continue ;

              w0 := dist_sqr( pt, GisPoint( p0.X, p0.Y ) ) ;
              w1 := dist_sqr( pt, GisPoint( p1.X, p1.Y ) ) ;
              w2 := dist_sqr( pt, GisPoint( p2.X, p2.Y ) ) ;

              if w0 = 0.0 then
                val := p0.Z
              else
              if w1 = 0.0 then
                val := p1.Z
              else
              if w2 = 0.0 then
                val := p2.Z
              else begin
                if FExponent = 2.0 then begin
                  w0 := 1.0/w0 ;
                  w1 := 1.0/w1 ;
                  w2 := 1.0/w2 ;
                end
                else begin
                  w0 := 1.0/Power( w0, FExponent/2.0 ) ;
                  w1 := 1.0/Power( w1, FExponent/2.0 ) ;
                  w2 := 1.0/Power( w2, FExponent/2.0 ) ;
                end ;
                val := ( w0*p0.Z + w1*p1.Z + w2*p2.Z )/( w0 + w1 + w2 );
              end ;

              pix.Grid[h,w] := val ;
              vmin := Min( vmin, val ) ;
              vmax := Max( vmax, val ) ;
            end ;
          end ;
        end ;
      end ;
    finally
      dst.MinHeight := vmin ;
      dst.MaxHeight := vmax ;
      busyEventManager.EndEvent ;
      dst.UnlockPixels( pix ) ;
    end ;

    Result := True ;
  end ;

{$IFNDEF JAVA OR ISLAND}
  function TGIS_TinToGrid.generateOCL : Boolean ;
  var
    src  : TGIS_LayerVector ;
    dst  : TGIS_LayerPixel ;

    ext  : TGIS_Extent ;
    exts : TGIS_Extent ;
    xsiz : Integer ;
    ysiz : Integer ;
    psiz : TGIS_Point ;
    prt  : Integer ;
    cs   : TGIS_CSCoordinateSystem ;
    {$IFDEF DCC}
      obj : TGIS_Shape ;
    {$ENDIF}
    shp  : TGIS_Shape ;
    pix  : TGIS_LayerPixelLock ;
    pt   : TGIS_Point ;
    p0   : TGIS_Point3D ;
    p1   : TGIS_Point3D ;
    p2   : TGIS_Point3D ;
    val  : Single ;
    vmin : Single ;
    vmax : Single ;
    w    : Integer ;
    h    : Integer ;
    dx   : Double ;
    dy   : Double ;
    xtoy : Double ;

    // OpenCL
    ocl  : TGIS_OpenCLProgram ;

    ibuf : array of Double ;
    obuf : array of Single ;
    tcnt : Integer ;
    i    : Integer ;
    isiz : Integer ;
    osiz : Integer ;

    function dist_sqr(
      const _p0 : TGIS_Point ;
      const _p1 : TGIS_Point
    ) : Double ;
    begin
      Result := Sqr( _p0.X - _p1.X ) + Sqr( _p0.Y - _p1.Y ) ;
    end ;

  begin
    Result := False ;

    src := FSourceLayer ;
    dst := FOutputLayer ;

    if not assigned( src ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_SRC, 1
      ) ;

    if not assigned( dst ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 1
      ) ;

    if not dst.IsGridImage then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 2
      ) ;

    if src.CS.EPSG <> dst.CS.EPSG then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 3
      ) ;

    if not GisIsSameExtent( src.Extent, dst.Extent ) then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 4
      ) ;

    src.Open ;
    dst.Open ;

    vmin :=  GIS_MAX_SINGLE ;
    vmax := -GIS_MAX_SINGLE ;

    if assigned( src.Viewer ) then
      cs := src.Viewer.Ref.CS
    else
      cs := src.CS ;

    ext := GisCommonExtent(
      src.CS.ExtentToCS( dst.CS, src.Extent ),
      dst.Extent
    ) ;
    exts := cs.ExtentFromCS( dst.CS, ext ) ;

    tcnt := 0 ;
    for obj in src.Loop( exts ) do begin
      {$IFDEF DCC}
        shp := obj ;
      {$ELSE}
        shp := TGIS_Shape( obj ) ;
      {$ENDIF}
      inc( tcnt, shp.GetNumParts ) ;
    end ;
    SetLength( ibuf, 9*tcnt ) ;
    isiz := 9*tcnt*sizeOf( Double ) ;

    pix := dst.LockPixels( ext, dst.CS, True ) ;
    try
      xsiz := pix.Bounds.Width + 1 ;
      ysiz := pix.Bounds.Height + 1 ;
      psiz := GisPoint(
        ( exts.XMax - exts.XMin )/xsiz,
        ( exts.YMax - exts.YMin )/ysiz
      ) ;
      osiz := xsiz*ysiz*sizeOf( Single ) ;

      i := 0 ;
      for obj in src.Loop( ext ) do begin
        {$IFDEF DCC}
          shp := obj ;
        {$ELSE}
          shp := TGIS_Shape( obj ) ;
        {$ENDIF}
        for prt := 0 to shp.GetNumParts - 1 do begin
          p0 := shp.GetPoint3D( prt, 0 ) ;
          p1 := shp.GetPoint3D( prt, 1 ) ;
          p2 := shp.GetPoint3D( prt, 2 ) ;
          ibuf[i  ] := ( p0.X - exts.XMin )/psiz.X - 0.5 ;
          ibuf[i+1] := (-p0.Y + exts.YMax )/psiz.Y - 0.5 ;
          ibuf[i+2] := p0.Z ;
          inc( i, 3 ) ;
          ibuf[i  ] := ( p1.X - exts.XMin )/psiz.X - 0.5 ;
          ibuf[i+1] := (-p1.Y + exts.YMax )/psiz.Y - 0.5 ;
          ibuf[i+2] := p1.Z ;
          inc( i, 3 ) ;
          ibuf[i  ] := ( p2.X - exts.XMin )/psiz.X - 0.5 ;
          ibuf[i+1] := (-p2.Y + exts.YMax )/psiz.Y - 0.5 ;
          ibuf[i+2] := p2.Z ;
          inc( i, 3 ) ;
        end ;
      end ;

      SetLength( obuf, xsiz*ysiz ) ;
      for i := 0 to xsiz*ysiz - 1 do
        obuf[i] := dst.NoDataValue ;

      pt := pix.RasterToMap( Point( pix.Bounds.Left, pix.Bounds.Top ), cs ) ;
      dx := GisPoint2Point(
        pt, pix.RasterToMap( Point( pix.Bounds.Left+1, pix.Bounds.Top ), cs )
      ) ;
      dy := GisPoint2Point(
        pt, pix.RasterToMap( Point( pix.Bounds.Left, pix.Bounds.Top+1 ), cs )
      ) ;
      xtoy := dx/dy ;

      ocl := TGIS_OpenCLProgram.Create ;
      try
        if not ocl.LoadFromString( GIS_OCL_TINTOGRID_SOURCE,
                 GIS_OCL_TINTOGRID_KERNEL ) then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_OPENCL_LOAD ), '', ocl.ErrorCode
                ) ;

        ocl.WorkDimension := 1 ;
        ocl.GlobalWorkSizes[0] := tcnt ;

        {$IFDEF DCC}
          ocl.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, @ibuf[0] ) ;
          ocl.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], osiz, @obuf[0] ) ;
          ocl.SetArgument( 2, sizeOf( Double  ), @FExponent ) ;
          ocl.SetArgument( 3, sizeOf( Integer ), @xsiz ) ;
          ocl.SetArgument( 4, sizeOf( Double  ), @xtoy ) ;
        {$ENDIF}
        {$IFDEF CLR}
          ocl.SetArgument( 0, [TGIS_OpenCLMemoryFlag.ReadOnly ], isiz, ibuf ) ;
          ocl.SetArgument( 1, [TGIS_OpenCLMemoryFlag.WriteOnly], osiz, obuf ) ;
          ocl.SetArgument( 2, sizeOf( Double  ), FExponent ) ;
          ocl.SetArgument( 3, sizeOf( Integer ), xsiz ) ;
          ocl.SetArgument( 4, sizeOf( Double  ), xtoy ) ;
        {$ENDIF}

        if not ocl.Execute then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_OPENCL_EXECUTION ), '', ocl.ErrorCode
                ) ;

        {$IFDEF DCC}
          ocl.ReadBuffer( 1, osiz, @obuf[0] ) ;
        {$ENDIF}
        {$IFDEF CLR}
          ocl.ReadBuffer( 1, osiz, obuf ) ;
        {$ENDIF}
      finally
        FreeObject( ocl ) ;
      end ;

      i := -1 ;
      for h := pix.Bounds.Left to pix.Bounds.Right do begin
        for w := pix.Bounds.Top to pix.Bounds.Bottom do begin
          inc( i ) ;
          val := obuf[i] ;
          pix.Grid[h,w] := val ;
          if val <> dst.NoDataValue then begin
            vmin := Min( val, vmin ) ;
            vmax := Max( val, vmax ) ;
          end ;
        end ;
      end ;

    finally
      dst.UnlockPixels( pix ) ;
    end ;

    dst.MinHeight := vmin ;
    dst.MaxHeight := vmax ;

    Result := True ;
  end ;
{$ENDIF}

  function TGIS_TinToGrid.Generate(
    const _src : TGIS_LayerVector ;
    const _dst : TGIS_LayerPixel
  ) : Boolean ;
  begin
    FSourceLayer := _src ;
    FOutputLayer := _dst ;

    Result := Generate ;
  end ;

  function TGIS_TinToGrid.Generate : Boolean ;
  begin
    {$IFDEF JAVA OR ISLAND}
      Result := generateCPU ;
    {$ELSE}
      if GisOpenCLEngine.Available and GisOpenCLEngine.Enabled then
        Result := generateOCL
      else
        Result := generateCPU ;
    {$ENDIF}
  end ;
{$ENDREGION}

{$REGION 'TGIS_PointCloudToGrid'}
  constructor TGIS_PointCloudToGrid.Create ;
  begin
    inherited ;

    FAssignment := TGIS_PointCloudAssignment.Mean ;
    FExponent   := T_INTERPOLATION.EXPONENT ;
    FWindowSize := T_INTERPOLATION.WINDOW_SIZE ;
    emptyCells  := TList<TPoint>.Create() ;
    minVal      := GIS_MAX_SINGLE ;
    maxVal      := -GIS_MAX_SINGLE ;

  end ;

  procedure TGIS_PointCloudToGrid.doDestroy ;
  begin
    FreeObject( emptyCells ) ;

    inherited ;
  end ;

  function TGIS_PointCloudToGrid.fget_Exponent : Double ;
  begin
    Result := FExponent ;
  end ;

  procedure TGIS_PointCloudToGrid.fset_Exponent(
    const _val : Double
  ) ;
  begin
    if _val > GIS_DOUBLE_RESOLUTION then
      FExponent := _val
    else
      FExponent := T_INTERPOLATION.EXPONENT ;
  end ;

    function TGIS_PointCloudToGrid.fget_WindowSize : Integer ;
  begin
    Result := FWindowSize ;
  end ;

  procedure TGIS_PointCloudToGrid.fset_WindowSize(
    const _val : Integer
  ) ;
  begin
    if _val > 0 then
      FWindowSize := _val
    else
      FWindowSize := T_INTERPOLATION.WINDOW_SIZE ;
  end ;

  procedure TGIS_PointCloudToGrid.Generate(
    const _src : TGIS_LayerVector ;
    const _dst : TGIS_LayerPixel
  ) ;
  begin
    Generate( _src, _src.Extent, '', _dst, _dst.Extent ) ;
  end ;


  procedure TGIS_PointCloudToGrid.Generate(
    const _src    : TGIS_LayerVector ;
    const _srcext : TGIS_Extent ;
    const _srcfld : String ;
    const _dst    : TGIS_LayerPixel ;
    const _dstext : TGIS_Extent
  ) ;
  begin
    inherited Generate( _src, _srcext, _srcfld, _dst, _dstext ) ;

    sourceLayer  := _src ;
    sourceExtent := _srcext ;
    sourceField  := _srcfld ;
    outputLayer  := _dst ;
    outputExtent := _dstext ;

    //? TODO: LayerPixel's PixelSize property needed again!
    pixelSize := getPixelSize ;

    doAssignment ;

    if UseDefaultValue then
      fillNoData ;
  end ;

  function  TGIS_PointCloudToGrid.getPixelSize : Double ;
  var
    ext    : TGIS_Extent ;
    width  : Integer ;
    height : Integer ;
    dx     : Double ;
    dy     : Double ;
  begin
    ext := outputLayer.Extent ;
    width := outputLayer.BitWidth ;
    height := outputLayer.BitHeight ;

    dx := ( ext.XMax - ext.XMin ) / width ;
    dy := ( ext.YMax - ext.YMin ) / height ;

    if GisIsSameValue( dx, dy, GIS_DOUBLE_RESOLUTION ) then
      pixelSize := dx
    else
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ), T_INTERPOLATION.PARAM_DST, 1
      ) ;
    Result := pixelSize ;
  end ;

  procedure TGIS_PointCloudToGrid.doAssignment ;
  var
    {$IFDEF DCC}
      obj      : TGIS_Shape ;
    {$ENDIF}
    shp        : TGIS_Shape ;
    i_prt      : Integer ;
    i_pnt      : Integer ;
    x          : Integer ;
    y          : Integer ;
    val        : Single ;
    dist       : Double ;
    idw_wght   : Double ;
    idw_exacts : TList<TPoint> ;
    p3d        : TGIS_Point3D ;
    lp_lock    : TGIS_LayerPixelLock ;
    arr        : Array of Array of Double ;
    pmax       : Int64 ;
    abrt       : Boolean ;

    function get_points_count : Int64 ;
    var
      {$IFDEF DCC}
        obj : TGIS_Shape ;
      {$ENDIF}
      shp  : TGIS_Shape ;
    begin
      Result := 0 ;
      for obj in sourceLayer.Loop( sourceExtent ) do begin
        {$IFDEF DCC}
          shp := obj ;
        {$ELSE}
          shp := TGIS_Shape( obj ) ;
        {$ENDIF}
        inc( Result, shp.GetNumPoints ) ;
      end ;
    end ;

    function assign_val : Single ;
    var
      out_val : Single ;
    begin
      out_val := 0 ;
      if length( sourceField ) = 0 then begin
        case Coordinate of
          TGIS_VectorToGridCoordinate.Z  : out_val := p3d.Z ;
          TGIS_VectorToGridCoordinate.M  : out_val := p3d.M ;
          TGIS_VectorToGridCoordinate.ZM : out_val := p3d.Z + p3d.M ;
        end ;
      end
      else
        out_val := VarToDouble( shp.GetField( sourceField ) ) ;

      Result := out_val ;
    end ;

    function is_idw_match( const _x, _y : Integer ) : Boolean;
    {$IFNDEF OXYGENE}
      var
        pt : TPoint ;
    {$ENDIF}
    begin
      Result := False ;
      for pt in idw_exacts do begin
        if ( _x = pt.X ) and ( _y = pt.Y ) then
          Result := True ;
      end ;
    end ;

    procedure assign_count ;
    begin
      if isNoData( lp_lock.Grid[y, x] ) then
        lp_lock.Grid[y, x] := 1
      else
        lp_lock.Grid[y, x] := lp_lock.Grid[y, x] + 1 ;

      assignMinMax ( minVal, maxVal, lp_lock.Grid[y, x] ) ;
    end ;

    procedure assign_first ;
    begin
      // assign only if pixel is NoData
      if isNoData( lp_lock.Grid[y, x] ) then begin
        lp_lock.Grid[y, x] := val ;
        assignMinMax ( minVal, maxVal, val ) ;
      end ;
    end ;

    procedure assign_last ;
    begin
      // assign all - last remains
      lp_lock.Grid[y, x] := val ;
      assignMinMax ( minVal, maxVal, val ) ;
    end ;

    procedure assign_min ;
    begin
      // can't compare to NoData!
      if isNoData( lp_lock.Grid[y, x] )
      or ( val < lp_lock.Grid[y, x] ) then begin
        lp_lock.Grid[y, x] := val ;
        assignMinMax ( minVal, maxVal, val ) ;
      end ;
    end ;

    procedure assign_max ;
    begin
      // can't compare to NoData!
      if isNoData( lp_lock.Grid[y, x] )
        or ( val > lp_lock.Grid[y, x] ) then
      begin
        lp_lock.Grid[y, x] := val ;
        assignMinMax ( minVal, maxVal, val ) ;
      end ;
    end ;

    procedure assign_mean ;
    begin
      // array stores number of points (count); grid store sums
      if isNoData( lp_lock.Grid[y, x] ) then begin
        arr[y, x] := 1 ;
        lp_lock.Grid[y, x] := val ;
      end
      else begin
        arr[y, x] := arr[y, x] + 1 ;
        lp_lock.Grid[y, x] := lp_lock.Grid[y, x] + val ;
      end ;
    end ;

    procedure assign_range ;
    begin
      // array stores mins; grid store maxs
      if isNoData( lp_lock.Grid[y, x] ) then begin
        arr[y, x] := val ;
        lp_lock.Grid[y, x] := val ;
      end
      else begin
        if ( val < arr[y, x] ) then
          arr[y, x] := val ;
        if ( val > lp_lock.Grid[y, x] ) then
          lp_lock.Grid[y, x] := val ;
      end ;
    end ;

    procedure assign_IDW ;
    begin
      // check if there is no exact matches
      if is_idw_match(x, y)  then begin
        Exit ;
      end;

      dist := GisPoint2Point(
        GisPoint2DFrom3D( p3d ),
        lp_lock.RasterToMap( Point( x, y ), outputLayer.CS )
        ) ;
      if dist > GIS_DOUBLE_RESOLUTION then begin
        idw_wght := 1 / Power( dist , FExponent ) ;

        // array stores sum of weights; grid store sum of vals & weights
        if isNoData( lp_lock.Grid[y, x] ) then begin
          arr[y, x] := idw_wght ;
          lp_lock.Grid[y, x] := val * idw_wght ;
        end
        else begin
          arr[y, x] := arr[y, x] + idw_wght ;
          lp_lock.Grid[y, x] := lp_lock.Grid[y, x] + val * idw_wght ;
        end;
      end
      else begin
        lp_lock.Grid[y, x] := val ;
        idw_exacts.Add( Point( x, y ) ) ;
      end ;
    end ;

    procedure assign_nearest ;
    begin
      dist := GisPoint2Point(
        GisPoint2DFrom3D( p3d ),
        lp_lock.RasterToMap( Point( x, y ), outputLayer.CS )
        ) ;

      // array stores NN distance; grid store val
      if isNoData( lp_lock.Grid[y, x] ) then begin
        arr[y, x] := dist ;
        lp_lock.Grid[y, x] := val ;
        assignMinMax ( minVal, maxVal, val ) ;
      end
      else begin
        if arr[y, x] > dist then begin
          arr[y, x] := dist ;
          lp_lock.Grid[y, x] := val ;
          assignMinMax ( minVal, maxVal, val ) ;
        end ;
      end ;
    end ;

    procedure assign_count_finish ;  // Count - fill NoData with zeros
    var
      x, y : Integer ;
    begin
      for y := lp_lock.Bounds.Top to lp_lock.Bounds.Bottom do begin
        for x := lp_lock.Bounds.Left to lp_lock.Bounds.Right do begin
          if isNoData( lp_lock.Grid[y, x] ) then begin
            lp_lock.Grid[y, x] := 0 ;
            assignMinMax ( minVal, maxVal, 0 ) ;
          end ;
        end ;
      end ;
    end ;

    procedure assign_mean_finish ;  // divide sum by count
    var
      x, y : Integer ;
    begin
      for y := lp_lock.Bounds.Top to lp_lock.Bounds.Bottom do begin
        for x := lp_lock.Bounds.Left to lp_lock.Bounds.Right do begin
          if not isNoData( lp_lock.Grid[y, x] ) then begin
            lp_lock.Grid[y, x] := lp_lock.Grid[y, x] / arr[y, x] ;
            assignMinMax ( minVal, maxVal, lp_lock.Grid[y, x] ) ;
          end
          else begin
            emptyCells.Add(Point( x, y ) ) ;
          end ;
        end ;
      end ;
    end ;

    procedure assign_range_finish ;  // max and min difference
    var
      x, y : Integer ;
    begin
      for y := lp_lock.Bounds.Top to lp_lock.Bounds.Bottom do begin
        for x := lp_lock.Bounds.Left to lp_lock.Bounds.Right do begin
          if not isNoData( lp_lock.Grid[y, x] ) then begin
            lp_lock.Grid[y, x] := lp_lock.Grid[y, x] - arr[y, x] ;
            assignMinMax ( minVal, maxVal, lp_lock.Grid[y, x] ) ;
          end
          else begin
            emptyCells.Add(Point( x, y ) ) ;
          end ;
        end ;
      end ;
    end ;

    procedure assign_IDW_finish ;  // divide sum(vals & weights) by sum(weights)
    var
      x, y : Integer ;
    begin
      for y := lp_lock.Bounds.Top to lp_lock.Bounds.Bottom do begin
        for x := lp_lock.Bounds.Left to lp_lock.Bounds.Right do begin
          // we don't want to reassign exact IDW matches
          if is_idw_match( x, y ) then
            Continue ;

          if not isNoData( lp_lock.Grid[y, x] ) then begin
              lp_lock.Grid[y, x] := lp_lock.Grid[y, x] / arr[y, x] ;
              assignMinMax ( minVal, maxVal, lp_lock.Grid[y, x] ) ;
          end
          else
            emptyCells.Add(Point( x, y ) ) ;
        end ;
      end ;
    end ;

  begin
    // additional array required for Mean, Range, IDW process
    if ( FAssignment = TGIS_PointCloudAssignment.Mean ) or
       ( FAssignment = TGIS_PointCloudAssignment.Range ) or
       ( FAssignment = TGIS_PointCloudAssignment.IDW ) or
       ( FAssignment = TGIS_PointCloudAssignment.NearestNeighbor ) then
    begin
      SetLength( arr, outputLayer.BitHeight, outputLayer.BitWidth ) ;
    end ;

    // list of pixel indexes with exact matches in IDW process
    if ( FAssignment = TGIS_PointCloudAssignment.IDW ) then
    begin
      idw_exacts := TList<TPoint>.Create() ;
    end ;

    pmax := get_points_count ;
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_INTERPOLATION_PCTOGRID ), pmax
    ) ;
    lp_lock := outputLayer.LockPixels( outputExtent, outputLayer.CS, True ) ;
    try
      for obj in sourceLayer.Loop( sourceExtent ) do begin
        {$IFDEF DCC}
          shp := obj ;
        {$ELSE}
          shp := TGIS_Shape( obj ) ;
        {$ENDIF}

        for i_prt := 0 to shp.GetNumParts - 1 do begin
          for i_pnt := 0 to shp.GetPartSize( i_prt ) - 1 do begin

            abrt := busyEventManager.PushEvent ;
            if abrt then
              exit ;

            p3d := shp.GetPoint3D( i_prt, i_pnt ) ;
            x := FloorS( ( p3d.X - sourceExtent.XMin ) / pixelSize ) ;
            y := lp_lock.Bounds.Bottom
              - ( FloorS(( p3d.Y - sourceExtent.YMin ) / pixelSize )) ;

            // if x is on the right bound or y on the top
            // (inverted y-axis - not bottom!) pixel index is shifted back
            if x > lp_lock.Bounds.Right then
              x := lp_lock.Bounds.Right ;
            if y < 0 then
              y := 0 ;

            // assignment methods NOT requiring additional array:
            // Count / First / Last / Min / Max
            if ( FAssignment = TGIS_PointCloudAssignment.Count ) then
              assign_count
            else begin
              val := assign_val ;

              case FAssignment of
                TGIS_PointCloudAssignment.First           : assign_first ;
                TGIS_PointCloudAssignment.Last            : assign_last ;
                TGIS_PointCloudAssignment.Min             : assign_min ;
                TGIS_PointCloudAssignment.Max             : assign_max ;
                // assignment methods requiring additional array:
                // Mean / Range / IDW
                TGIS_PointCloudAssignment.Mean            : assign_mean ;
                TGIS_PointCloudAssignment.Range           : assign_range ;
                TGIS_PointCloudAssignment.IDW             : assign_IDW ;
                TGIS_PointCloudAssignment.NearestNeighbor : assign_nearest ;
              end ;
            end ;
          end ;
        end ;
      end ;

      // finishing assignments
      case FAssignment of
        TGIS_PointCloudAssignment.Count : assign_count_finish ;
        TGIS_PointCloudAssignment.Mean  : assign_mean_finish ;
        TGIS_PointCloudAssignment.Range : assign_range_finish ;
        TGIS_PointCloudAssignment.IDW   : assign_IDW_finish ;
      end ;

    finally
      outputLayer.UnlockPixels( lp_lock);
      outputLayer.MinHeight := minVal ;
      outputLayer.MaxHeight := maxVal ;
      if ( FAssignment = TGIS_PointCloudAssignment.IDW ) then
        FreeObject( idw_exacts ) ;
      busyEventManager.EndEvent ;
    end ;
  end ;

  procedure TGIS_PointCloudToGrid.fillNoData ;
  var
    shp        : TGIS_Shape ;
    i          : Integer ;
    r          : Integer ;
    null_step  : Integer ;
    sum        : Double ;
    x          : Integer ;
    y          : Integer ;
    x_null     : Integer ;
    y_null     : Integer ;
    val        : Single ;
    dist       : Double ;
    idw_wght   : Double ;
    p3d        : TGIS_Point3D ;
    lp_lock    : TGIS_LayerPixelLock ;
    pts_lv     : TGIS_LayerVector ;
    delaunay   : TGIS_LayerDelaunay ;

    procedure fill_default_value ;  // Simple filling with DefaultValue
    var
      i : Integer ;
    begin
      for i := 0 to emptyCells.Count - 1 do begin
        x_null := emptyCells[i].X ;
        y_null := emptyCells[i].Y ;
        lp_lock.Grid[y_null, x_null] := DefaultValue ;
      end ;

      assignMinMax ( minVal, maxVal, DefaultValue ) ;
    end ;

    procedure fill_triangulation ;
    var
      x, y : Integer ;
      {$IFNDEF OXYGENE}
        empty_cell : TPoint ;
      {$ENDIF}
    begin
      pts_lv := TGIS_LayerVector.Create ;
      pts_lv.Name := 'points_for_triangulation' ;
      pts_lv.CS := outputLayer.CS ;
      pts_lv.Extent := outputLayer.Extent ;
      pts_lv.Open ;
      pts_lv.DefaultDimension := TGIS_DimensionType.XYZ ;

      shp := pts_lv.CreateShape( TGIS_ShapeType.MultiPoint ) ;
      shp.Lock( TGIS_Lock.Extent ) ;
      shp.AddPart ;

      // collecting points for triangulation
      for empty_cell in emptyCells do begin
        for y := empty_cell.Y - 1 to empty_cell.Y + 1 do begin
          for x := empty_cell.X - 1 to empty_cell.X + 1 do begin
            if ( x < 0 ) or ( x > lp_lock.Bounds.Right  ) or
               ( y < 0 ) or ( y > lp_lock.Bounds.Bottom ) or
               ( x = empty_cell.X ) and ( y = empty_cell.Y ) then
              Continue ;

            if not isNoData( lp_lock.Grid[y, x] ) then begin
              p3d := GisPoint3DFrom2D(
                lp_lock.RasterToMap( Point( x, y ), outputLayer.CS )
                );
              p3d.Z := lp_lock.Grid[y, x] ;
              shp.AddPoint3D( p3d ) ;
            end ;
          end
        end ;
      end ;

      shp.Unlock ;

      delaunay := TGIS_LayerDelaunay.Create ;
      try
        delaunay.Name := 'delaunay_TIN' ;
        delaunay.CS := outputLayer.CS ;
        delaunay.ImportLayer(
          pts_lv,
          pts_lv.Extent,
          TGIS_ShapeType.MultiPoint,
          '',
          True ) ;

        // interpolate value for empty cells
        for empty_cell in emptyCells do begin
          val := delaunay.PointAltitude(
            lp_lock.RasterToMap( Point( empty_cell.X, empty_cell.Y ), outputLayer.CS )) ;
          if not isNoData( val ) then begin
            lp_lock.Grid[empty_cell.Y, empty_cell.X] := val ;
            assignMinMax ( minVal, maxVal, val ) ;
          end;
        end ;

      finally
        FreeObject( delaunay ) ;
      end;
    end ;

    procedure fill_mean ;
    begin
      // array stores number of points (count); grid store sums
      if not isNoData( lp_lock.Grid[y, x] ) then begin
        sum := sum + 1 ;
        if isNoData( lp_lock.Grid[y_null, x_null] ) then
          lp_lock.Grid[y_null, x_null] := val
        else
          lp_lock.Grid[y_null, x_null] := lp_lock.Grid[y_null, x_null] + val ;
      end ;
    end ;

    procedure fill_nearest ;
    begin
      dist := GisPoint2Point(
        GisPoint( x_null, y_null ),
        GisPoint( x, y )
        ) ;
      assert( dist > GIS_DOUBLE_RESOLUTION ) ;  // it's not possible in this context

      // sum stores nearest distance
      if not  isNoData( lp_lock.Grid[y, x] ) then begin
        if isNoData( lp_lock.Grid[y_null, x_null] ) then begin
          sum := dist ;
          lp_lock.Grid[y_null, x_null] := val ;
        end
        else begin
          if sum > dist then begin
            sum := dist ;
            lp_lock.Grid[y_null, x_null] := val ;
          end;
        end ;
      end ;
    end ;

    procedure fill_IDW ;
    begin
      dist := GisPoint2Point(
        GisPoint( x_null, y_null ),
        GisPoint( x, y )
        ) ;
      assert( dist > GIS_DOUBLE_RESOLUTION ) ;  // it's not possible in this context
      idw_wght := 1 / Power( dist , FExponent ) ;

      // sum stores sum of weights; grid store sum of vals & weights
      if not isNoData( lp_lock.Grid[y, x] ) then begin
        sum := sum +  idw_wght ;
        if isNoData( lp_lock.Grid[y_null, x_null] ) then
          lp_lock.Grid[y_null, x_null] := val * idw_wght
        else
          lp_lock.Grid[y_null, x_null] := lp_lock.Grid[y_null, x_null]
            + val * idw_wght ;
      end ;
    end ;

  begin
    if not findNoData then
      Exit ;

    lp_lock := outputLayer.LockPixels( outputExtent, outputLayer.CS, True ) ;
    try
      case FDefValFormula of
        TGIS_PointCloudFormula.DefaultValue  : fill_default_value ;
        TGIS_PointCloudFormula.Triangulation : fill_triangulation ;
        else begin  // Use windowing
          i := 0 ;
          null_step := 1 ;
          r := RoundS( FWindowSize / 2 ) ;

          // NoData filling process requires 2 loops: from 0 to nulls.Count-1
          // and inversely from nulls.Count-1 to 0, because in some situaion
          // first loop may not fill all of the null cells
          // To avoid we use while loop with sweeping (increasing and decreasing) index
          while emptyCells.Count <> 0 do begin
            sum := 0.0 ;
            x_null := emptyCells[i].X ;
            y_null := emptyCells[i].Y ;

            for y := y_null - r to y_null + r do begin
              for x := x_null - r to x_null + r do begin
                // Out of grid or temp cell is current null cell
                if ( x < 0 ) or ( x > lp_lock.Bounds.Right  ) or
                   ( y < 0 ) or ( y > lp_lock.Bounds.Bottom ) or
                   ( x = x_null ) and ( y = y_null ) then
                  Continue ;

                val := lp_lock.Grid[y, x] ;

                case FDefValFormula of
                  TGIS_PointCloudFormula.Mean            : fill_mean ;
                  TGIS_PointCloudFormula.IDW             : fill_IDW ;
                  TGIS_PointCloudFormula.NearestNeighbor : fill_nearest ;
                end ;
              end ;
            end ;

            if i = emptyCells.Count - 1 then
              null_step := -1 ;

            // it is possible that in window there are no assigned cells
            if isNoData( lp_lock.Grid[y_null, x_null] ) then begin
              i := i + null_step ;
            end
            else begin
              if ( FDefValFormula = TGIS_PointCloudFormula.Mean ) or
                 ( FDefValFormula = TGIS_PointCloudFormula.IDW ) then
              begin
                lp_lock.Grid[y_null, x_null]
                  := lp_lock.Grid[y_null, x_null] / sum ;
              end ;
              assignMinMax ( minVal, maxVal, lp_lock.Grid[y_null, x_null] ) ;

              {$IFDEF OXYGENE}
                emptyCells.RemoveAt( i ) ;
              {$ELSE}
                emptyCells.Delete( i ) ;
              {$ENDIF}

              // for return loop remember to decrease index
              if null_step = - 1 then
                i := i + null_step ;
            end ;
          end ;
        end ;
      end ;
    finally
      outputLayer.UnlockPixels( lp_lock );
      outputLayer.MinHeight := minVal ;
      outputLayer.MaxHeight := maxVal ;
    end ;
  end ;

  function TGIS_PointCloudToGrid.findNoData: Boolean ;
  var
    x    : Integer ;
    y    : Integer ;
    lp_lock : TGIS_LayerPixelLock ;
  begin
    lp_lock := outputLayer.LockPixels(
      outputLayer.Extent, outputLayer.CS, True ) ;
    try
      // NoData cell indexes stored in 'nulls' list
      // Mean, Range, IDW - NoData cells caught in Assignment part
      // Fist, Last. Min, Max - need to be checked
      if not ( ( FAssignment = TGIS_PointCloudAssignment.Mean ) or
               ( FAssignment = TGIS_PointCloudAssignment.Range ) or
               ( FAssignment = TGIS_PointCloudAssignment.IDW ) ) then begin
        for y := lp_lock.Bounds.Top to lp_lock.Bounds.Bottom do begin
          for x := lp_lock.Bounds.Left to lp_lock.Bounds.Right do begin
            if isNoData( lp_lock.Grid[y, x] ) then begin
              emptyCells.Add(Point( x, y ) ) ;
            end ;
          end ;
        end ;
      end ;
    finally
      outputLayer.UnlockPixels( lp_lock ) ;
    end;

    Result := emptyCells.Count <> 0 ;
  end ;
{$ENDREGION}

{$REGION 'T_Pipeline_PointCloudToGrid'}
  class procedure Unit_GisInterpolation.SelfRegisterPipeline ;
  begin
    RegisterPipeline(
      'Interpolation.PointCloudToGrid',
      T_Pipeline_PointCloudToGrid
    ) ;

    RegisterPipeline(
      'Interpolation.Kriging',
      T_Pipeline_InterpolationKriging
    ) ;

    //? TODO: Add more interpolation operations
  end ;

  procedure T_Pipeline_PointCloudToGrid.Initialize ;
  begin
    inherited ;

    defineParam(
      GIS_PIPELINE_PARAM_FIELD,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '',
      ''
    ) ;
    defineParam(
      PARAM_ASSIGNMENT,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Mean',
      '!Count|First|Last|Min|Max|Mean|Range|IDW|NearestNeighbor'
    ) ;
    defineParam(
      PARAM_COORDINATE,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'Z',
      '!None|Z|M|ZM'
    ) ;
    defineParam(
      PARAM_DEFVAL,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      FloatToStr( T_INTERPOLATION.DEFVAL ),
      ''
    ) ;
    defineParam(
      PARAM_DEFVAL_FORMULA,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      'DefaultValue',
      '!DefaultValue|Mean|IDW|NearestNeighbor|Triangulation'
    ) ;
    defineParam(
      PARAM_EXPONENT,
      TGIS_PipelineParameterType.Float,
      NaN,
      NaN,
      False,
      FloatToStr( T_INTERPOLATION.EXPONENT ),
      ''
    ) ;
    defineParam(
      PARAM_USE_DEFVAL,
      TGIS_PipelineParameterType.Boolean,
      NaN,
      NaN,
      False,
      BoolToStr( T_INTERPOLATION.USE_DEFVAL ),
      ''
    ) ;
    defineParam(
      PARAM_WINDOW_SIZE,
      TGIS_PipelineParameterType.Int,
      NaN,
      NaN,
      False,
      IntToStr( T_INTERPOLATION.WINDOW_SIZE ),
      '3|5|7|9'
    ) ;
  end;

  procedure T_Pipeline_PointCloudToGrid.Execute;
  var
    param      : String ;
    src        : TGIS_LayerVector ;
    dst        : TGIS_LayerPixel ;
    src_fld    : String ;
    pc_to_grid : TGIS_PointCloudToGrid ;
    assignment : TGIS_PointCloudAssignment ;
    coordinate : TGIS_VectorToGridCoordinate ;
    formula    : TGIS_PointCloudFormula ;
  begin
    src := ParamAsLayerVector( GIS_PIPELINE_PARAM_SOURCE ) ;
    dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
    src_fld := ParamAsField( src, GIS_PIPELINE_PARAM_FIELD, '') ;

    // Assignment
    param := ParamAsString( PARAM_ASSIGNMENT ) ;
    if CompareText( param, 'Count' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Count
    else if CompareText( param, 'Count' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Count
    else if CompareText( param, 'First' ) = 0 then
      assignment := TGIS_PointCloudAssignment.First
    else if CompareText( param, 'Last' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Last
    else if CompareText( param, 'Min' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Min
    else if CompareText( param, 'Max' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Max
    else if CompareText( param, 'Mean' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Mean
    else if CompareText( param, 'Range' ) = 0 then
      assignment := TGIS_PointCloudAssignment.Range
    else if CompareText( param, 'IDW' ) = 0 then
      assignment := TGIS_PointCloudAssignment.IDW
    else if CompareText( param, 'NearestNeighbor' ) = 0 then
      assignment := TGIS_PointCloudAssignment.NearestNeighbor
    else
      assignment := TGIS_PointCloudAssignment.Mean ;

    // Coordinate
    param := ParamAsString( PARAM_COORDINATE ) ;
    if CompareText( param, 'None' ) = 0 then
      coordinate := TGIS_VectorToGridCoordinate.None
    else if CompareText( param, 'Z' ) = 0 then
      coordinate := TGIS_VectorToGridCoordinate.Z
    else if CompareText( param, 'M' ) = 0 then
      coordinate := TGIS_VectorToGridCoordinate.M
    else if CompareText( param, 'ZM' ) = 0 then
      coordinate := TGIS_VectorToGridCoordinate.ZM
    else
      coordinate := TGIS_VectorToGridCoordinate.Z ;

    //   DefaultValueFormula
    param := ParamAsString( PARAM_DEFVAL_FORMULA ) ;
    if CompareText( param, 'DefaultValue' ) = 0 then
      formula := TGIS_PointCloudFormula.DefaultValue
    else if CompareText( param, 'Mean' ) = 0 then
      formula := TGIS_PointCloudFormula.Mean
    else if CompareText( param, 'IDW' ) = 0 then
      formula := TGIS_PointCloudFormula.IDW
    else if CompareText( param, 'NearestNeighbor' ) = 0 then
      formula := TGIS_PointCloudFormula.NearestNeighbor
    else if CompareText( param, 'Triangulation' ) = 0 then
      formula := TGIS_PointCloudFormula.Triangulation
    else
      formula := TGIS_PointCloudFormula.DefaultValue ;

    // execute point cloud gridding
    pc_to_grid := TGIS_PointCloudToGrid.Create ;
    try
      pc_to_grid.Assignment := assignment ;
      pc_to_grid.Coordinate := coordinate ;
      pc_to_grid.DefaultValue := ParamAsFloat( PARAM_DEFVAL, T_INTERPOLATION.DEFVAL ) ;
      pc_to_grid.DefaultValueFormula := formula ;
      pc_to_grid.Exponent := ParamAsFloat( PARAM_EXPONENT, T_INTERPOLATION.EXPONENT ) ;
      pc_to_grid.UseDefaultValue := ParamAsBool( PARAM_USE_DEFVAL, T_INTERPOLATION.USE_DEFVAL ) ;
      pc_to_grid.WindowSize := ParamAsInt( PARAM_WINDOW_SIZE, T_INTERPOLATION.WINDOW_SIZE ) ;
      pc_to_grid.busyEventManager.UseProgressThreshold := False ;
      {$IFDEF CLR}
        pc_to_grid.BusyEvent += DoBusyEvent ;
      {$ELSE}
        pc_to_grid.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
      {$ENDIF}
      pc_to_grid.Generate( src, src.Extent, src_fld, dst, dst.Extent ) ;
    finally
      FreeObject( pc_to_grid ) ;
    end ;

    inherited ;
  end ;
{$ENDREGION}

{$REGION 'T_Pipeline_InterpolationKriging'}
  procedure T_Pipeline_InterpolationKriging.Initialize ;
  begin
    inherited ;

    defineParam(
      GIS_PIPELINE_PARAM_FIELD,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      False,
      '',
      ''
    ) ;
  end ;

  procedure T_Pipeline_InterpolationKriging.Execute ;
  var
    src     : TGIS_LayerVector ;
    src_fld : String ;
    dst     : TGIS_LayerPixel ;
    vtg     : TGIS_InterpolationKriging ;
  begin
    src := ParamAsLayerVector( GIS_PIPELINE_PARAM_SOURCE ) ;
    dst := ParamAsLayerPixel( GIS_PIPELINE_PARAM_DESTINATION ) ;
    src_fld := ParamAsField( src, GIS_PIPELINE_PARAM_FIELD ) ;

    vtg := TGIS_InterpolationKriging.Create ;
    try
      // for windowed version of this method you need to set Windowed=True
      // and at least the Radius, e.g.
      // vtg.Windowed := True ;
      // vtg.Radius := ( ext.XMax - ext.XMin )/5.0 ;

      // generate the Kriging interpolation grid
      vtg.Semivariance := TGIS_SemivarianceGaussian.Create ;
      // Pipeline event manager use progress threshold,
      // so this should not, to avoid using trigger twice
      vtg.busyEventManager.UseProgressThreshold := False ;
      {$IFDEF CLR}
        vtg.BusyEvent += DoBusyEvent ;
      {$ELSE}
        vtg.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
      {$ENDIF}
      vtg.Generate( src, src.Extent, src_fld, dst, dst.Extent ) ;

    finally
      FreeObject( vtg );
    end ;

    inherited ;
  end;
{$ENDREGION}

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
  Unit_GisInterpolation.SelfRegisterPipeline ;
{$ENDIF}

//==================================== END =====================================
end.
