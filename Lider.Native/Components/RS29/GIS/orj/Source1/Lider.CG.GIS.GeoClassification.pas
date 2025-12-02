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
  Provides methods to statistically subdivide a set of numeric values into classes.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoClassification;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoClassification"'}
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

uses
  {$IFDEF CLR}
    System.Collections.Generic,
    TatukGIS.RTL ,
    TatukGIS.RTL.XML ;
  {$ENDIF}
  {$IFDEF DCC}
    System.Generics.Collections,
    System.SysUtils,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoMatrix,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoStatistics,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI;
  {$ENDIF}
  {$IFDEF JAVA}
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
  {$ENDIF}
  {$IFDEF ISLAND}
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
  {$ENDIF}

type
  /// <summary>
  ///   Enumeration of visual properties which can be set for the classification
  ///   process. Available only for vector layers.
  /// </summary>
  TGIS_ClassificationRenderType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Color.
    /// </summary>
    Color,

    /// <summary>
    ///   Size for markers, width for lines; not available for polygons.
    /// </summary>
    Size,

    /// <summary>
    ///   Outline width.
    /// </summary>
    OutlineWidth,

    /// <summary>
    ///   Outline color.
    /// </summary>
    OutlineColor
  ) ;

  /// <summary>
  ///   Enumeration of classification methods.
  /// </summary>
  TGIS_ClassificationMethod = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   The interval between class breaks is defined by the user
    ///   through the Interval property.
    ///   Number of classes is determined automatically and can be gotten
    ///   from the NumClasses property.
    /// </summary>
    DefinedInterval,

    /// <summary>
    ///   The range of attribute values is partitioned into equal-sized subranges
    ///   by defining the number of classes through the NumClasses property.
    /// </summary>
    EqualInterval,

    /// <summary>
    ///   Class breaks are based on class intervals that have a geometrical series.
    ///   User specifies the number of class intervals through the NumClasses property.
    /// </summary>
    GeometricalInterval,

    /// <summary>
    ///   Class breaks are determined using the K-Means Clustering Algorithm.
    ///   All values from an attribute are divided into N=NumClasses clusters
    ///   so the within-cluster sum of squares is minimized.
    /// </summary>
    KMeans,

    /// <summary>
    ///   K-Means algorithm is used to partition data into N=NumClasses spatial
    ///   clusters based on objects centroids.
    ///   Available only for vector layers.
    ///   New field named 'KMEANS_ID' will be added or updated if it exists.
    /// </summary>
    KMeansSpatial,

    /// <summary>
    ///   Class breaks are determined using the Fisher's Exact Optimization algorithm.
    ///   Dataset is divided into N=NumClasses clusters so that
    ///   the between-cluster sum of squares is maximized. This is default method.
    /// </summary>
    NaturalBreaks,

    /// <summary>
    ///   Class intervals are created to have an equal number of observations.
    ///   Number of classes can be defined through the NumClasses property.
    ///   This method is also known as Equal Count.
    /// </summary>
    Quantile,

    /// <summary>
    ///   Quantile method is applied with fixed property NumClasses=4.
    ///   This method creates class breaks at the first, second, and third
    ///   quartiles i.e. at 25-th, 50-th, and 75-th percentiles respectively.
    /// </summary>
    Quartile,

    /// <summary>
    ///   Class breaks are set above and below the mean of the attribute values
    ///   at intervals, for example at 1, 1/2, 1/3, or 1/4 standard deviations
    ///   until all the data values are contained within these ranges.
    ///   Interval is specified through the Interval property.
    ///   This method produces even number of classes.
    /// </summary>
    StandardDeviation,

    /// <summary>
    ///   Same as StandardDeviation but class breaks are shifted
    ///   by a half of the interval to get central class in range
    ///   [Mean-Interval/2..Mean+Interval/2].
    ///   For example, if Interval equals 1 Std Dev, central class is limited by
    ///   breaks -0.5 Std Dev and 0.5 Std Dev.
    ///   This method produces odd number of classes.
    /// </summary>
    StandardDeviationWithCentral,

    /// <summary>
    ///   Class breaks are set for unique values.
    ///   Number of classes can be limited through the NumClasses property.
    /// </summary>
    Unique,

    /// <summary>
    ///   Class breaks are set manually by the user through the AddClassBreak method.
    ///   This method is used when classes are meaningful.
    /// </summary>
    Manual

  ) {$IFDEF JAVA} of Integer {$ENDIF} ;

  /// <summary>
  ///   General class that enables performing advanced data classification
  ///   in the process of creating choropleth (thematic) maps. Must be derived.
  /// </summary>
  TGIS_ClassificationAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                                 class( TGIS_Object )
    private const
      MAX_KMEANS_ITERATION = 10 ;

      // constructor defaults
      DEFAULT_METHOD       = TGIS_ClassificationMethod.NaturalBreaks ;
      DEFAULT_NUM_CLASSES  = 5 ;
      DEFAULT_INTERVAL     = 1 ;
      DEFAULT_SHOW_LEGEND  = True ;

      QUARTILE_NUM_CLASSES = 4 ;
      ARITHMETIC_SEQUENCE  = 0 ;
      GEOMETRIC_SEQUENCE   = 1 ;

    private
      FMethod              : TGIS_ClassificationMethod ;
      FInterval            : Double ;
      FStartColor          : TGIS_Color ;
      FEndColor            : TGIS_Color ;
      FColorRamp           : TGIS_ColorMapArray ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FNumClasses          : Integer ;
      FShowLegend          : Boolean ;
      FLayer               : TGIS_Layer ;

    {$IFNDEF OXYGENE} protected {$ELSE} unit {$ENDIF}
      {#gendoc:hide}
      FTarget              : String ;

    private
      minValue             : Double ;
      maxValue             : Double ;
      classIntervalsLst    : TObjectList< TObject > ;
      manualBreaksLst      : TList< Double > ;

    // getters & setters
    private
      function  fget_ClassBreaks            ( const _index  : Integer
                                            ) : Variant ;

      procedure fset_Interval               ( const _value  : Double
                                            ) ;

      procedure fset_Method                 ( const _value : TGIS_ClassificationMethod
                                            ) ;

      function  fget_NumClasses             : Integer ;

      procedure fset_NumClasses             ( const _value  : Integer
                                            ) ;


    // internal methods
    private
      procedure determineMinMax ;

      procedure classifyDefinedInterval     ( const _onlyNumClasses : Boolean = false
                                            ) ;

      procedure classifyEqualInterval       ;

      procedure classifyGeometricalInterval ;

      procedure classifyInterval            ( const _sequenceType  : Integer ;
                                              const _offset         : Double
                                            ) ;

      procedure classifyKMeansField         ;

      procedure classifyManual              ;

      procedure classifyNaturalBreaksFisher ;

      procedure classifyQuantile            ;

      procedure classifyQuartile            ;

      procedure classifyStandardDeviation   ( const _withCentral    : Boolean ;
                                              const _onlyNumClasses : Boolean  = false
                                            ) ;

      procedure classifyUnique              ( const _onlyNumClasses : Boolean = false
                                            ) ;

      function  colorRampIsDiscrete         : Boolean ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure addClassBreakUnique         ( const _uniqueValue  : Variant
                                            ) ;

      procedure addClassBreakInternal       ( const _startValue   : Double ;
                                              const _endValue     : Double ;
                                              const _index        : Double = 0.0 ;
                                              const _indexEx      : Double = 0.0
                                            ) ;


      function  applyClassification         ( const _params       : TGIS_ParamsList ;
                                              const _discreteRamp : Boolean
                                            ) : Boolean ; virtual ; abstract ;

      function  calcInterpolationRatio      ( const _i : Integer ;
                                              const _n : Integer
                                            ) : Double ;

      function  classifyKMeans              ( const _data         : TGIS_Matrix ;
                                              var   _clusters     : TArray< Integer >
                                            ) : Boolean ;

      procedure classifyKMeansSpatial       ; virtual ; abstract ;

      function  generateLegend              ( const _classBreak   : TObject ;
                                              const _firstClass   : Boolean ;
                                              const _lastClass    : Boolean
                                            ) : String ;

      function  getMaxNumClasses            : Integer ; virtual ; abstract ;

      function  interpolateColor            ( const _iBreak       : Integer ;
                                              const _breaksCount  : Integer ;
                                              const _discreteRamp : Boolean
                                            ) : TGIS_Color ;

      function classBreakIsEmpty            ( const _classBreak   : TObject
                                            ) : Boolean ;

    protected
      procedure doDestroy ; override ;

      /// <summary>
      ///   Internal constructor.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be classified
      /// </param>
      procedure doCreate ( const _layer : TGIS_Layer
                         ) ;

    public
      /// <summary>
      ///   Adds a new class break.
      /// </summary>
      /// <param name="_breakValue">
      ///   A value of the new class break.
      /// </param>
      /// <remarks>
      ///   Class breaks do not need to be added in ascending order.
      ///   Classifier sorts it.
      /// </remarks>
      procedure AddClassBreak          ( const _breakValue : Double
                                       ) ; overload ;

      /// <summary>
      ///   Performs the classification.
      /// </summary>
      /// <returns>
      ///   True if classification is successful, False otherwise
      ///   (e.g. if layer's statistics are not calculated or are out-of-date).
      /// </returns>
      /// <remarks>
      ///   <note type="caution">
      ///     Layer's ParamsList will be cleaned,
      ///     so user style sections will be erased.
      ///   </note>
      /// </remarks>
      function Classify                : Boolean ; overload ;

      /// <summary>
      ///   Performs the classification and applies to defined ParamsList.
      /// </summary>
      /// <param name="_params">
      ///   ParamsList to apply a classification style
      /// </param>
      /// <returns>
      ///   True if classification is successful, False otherwise
      ///   (e.g. if layer's statistics are not calculated or are out-of-date).
      /// </returns>
      /// <remarks>
      ///   <note type="caution">
      ///     For unassigned _params, a layer's ParamsList will be used.
      ///   </note>
      /// </remarks>
      function Classify                ( const _params : TGIS_ParamsList
                                       ) : Boolean ; overload ;

      /// <summary>
      ///   Performs the pre-classification and sets NumClasses property.
      ///   Only works for methods with an unknown number of classes at the start,
      ///   such as: DefinedInterval, StandardDeviation,
      ///   StandardDeviationWithCentral, and Unique.
      /// </summary>
      procedure EstimateNumClasses ;

      /// <summary>
      ///   If True, statistics from layer assigned to classification
      ///   need to be calculated.
      /// </summary>
      /// <returns>
      ///   True if layer's statistics need to be calculated; False otherwise.
      /// </returns>
      /// <remarks>
      ///   It is the user responsibility to (re)calculate statistics.
      ///   This is performed simply by invoking Layer.Statistics.Calculate
      ///   method.
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDNOEXIST
      /// </exception>
      function MustCalculateStatistics : Boolean ;

    public
      /// <summary>
      ///   Computed class breaks.
      ///   Available after completing the classification.
      /// </summary>
      /// <param name="_index">
      ///   position in the list; from 0 to NumClasses
      /// </param>
      /// <remarks>
      ///   Number of generated class breaks equals NumClasses + 1, so
      ///   when using 'for' loops, iterate in range from 0 to NumClasses:
      ///   <list type="bullet">
      ///     <item>
      ///       _index=0 - minimum class break value,
      ///     </item>
      ///     <item>
      ///       _index from 1 to NumClasses-1 - intermediate class break values,
      ///     </item>
      ///     <item>
      ///       _index=NumClasses - maximum class break value.
      ///     </item>
      ///   </list>
      /// </remarks>
      property ClassBreaks             [ const _index : Integer ]
                                       : Variant
                                         read  fget_ClassBreaks ;

      /// <summary>
      ///   The color ramp used to generate colors for class breaks.
      ///   If not provided, StartColor and EndColor will be used.
      /// </summary>
      property ColorRamp               : TGIS_ColorMapArray
                                         read  FColorRamp
                                         write FColorRamp ;

      /// <summary>
      ///   Class interval size; default is 1.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Available only for methods: DefinedInterval and StandardDeviation.
      ///   </note>
      /// </remarks>
      property Interval                : Double
                                         read  FInterval
                                         write fset_Interval ;

      /// <summary>
      ///   Layer assigned to object.
      /// </summary>
      property Layer                   : TGIS_Layer
                                         read  FLayer ;

      /// <summary>
      ///   Classification method; NaturalBreaks by default.
      /// </summary>
      property &Method                 : TGIS_ClassificationMethod
                                         read  FMethod
                                         write fset_Method ;

      /// <summary>
      ///   Number of classes; default is 5, max is 30.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     This property is disabled for methods: DefinedInterval,
      ///     Quartile and StandardDeviation.
      ///     Value of NumClasses property is automatically calculated every
      ///     Interval, Method or Target properties change.
      ///   </note>
      /// </remarks>
      property NumClasses              : Integer
                                         read  fget_NumClasses
                                         write fset_NumClasses ;

      /// <summary>
      ///   Starting color used to generate ramp for class breaks.
      /// </summary>
      /// <remarks>
      ///   Works only for RenderType property values:
      ///   TGIS_ClassificationRenderType.Color and
      ///   TGIS_ClassificationRenderType.OutlineColor
      /// </remarks>
      property StartColor              : TGIS_Color
                                         read  FStartColor
                                         write FStartColor ;

      /// <summary>
      ///   Ending color used to generate ramp for class breaks.
      /// </summary>
      /// <remarks>
      ///   Works only for RenderType values:
      ///   TGIS_ClassificationRenderType.Color and
      ///   TGIS_ClassificationRenderType.OutlineColor.
      /// </remarks>
      property EndColor                : TGIS_Color
                                         read  FEndColor
                                         write FEndColor ;

      /// <summary>
      ///   If True, features will be shown in legend panel. Default is True.
      /// </summary>
      /// <remarks>
      ///   False for KMeansSpatial method.
      /// </remarks>
      property ShowLegend              : Boolean
                                         read  FShowLegend
                                         write FShowLegend ;

      /// <summary>
      ///   Indicates the data to be used for classification.
      /// </summary>
      /// <remarks>
      ///   Numeric field for a vector layer or band number for a pixel layer,
      ///   e.g.: '1', '2', '3', etc
      /// </remarks>
      property Target                  : String
                                         read  FTarget
                                         write FTarget ;
  end;

  /// <summary>
  ///   Class that enables performing advanced vector data classification
  ///   in the process of creating choropleth (thematic) maps.
  /// </summary>
  TGIS_ClassificationVector = {$IFDEF OXYGENE} public  {$ENDIF}
                                               class( TGIS_ClassificationAbstract )
    private const  // constructor defaults
      MAX_NUM_CLASSES    = 256 ;
      DEFAULT_START_SIZE = 1 ;
      DEFAULT_END_SIZE   = 480 ;  // value from TGIS_ParamsRender

    private
      FStartSize    : Integer ;
      FEndSize      : Integer ;
      FClassIdField : String ;
      FRenderType   : TGIS_ClassificationRenderType ;

    private
      procedure do_fill_field             ( const _classQuery   : String ;
                                            const _iBreak       : Integer
                                          ) ;

      procedure setParams                 ( const _params       : TGIS_ParamsList ;
                                            const _shapeType    : TGIS_ShapeType ;
                                            const _query        : String ;
                                            const _legend       : String ;
                                            const _color        : TGIS_Color ;
                                            const _width        : Integer
                                          ) ;

      function  interpolateWidth          ( const _iBreak       : Integer ;
                                            const _breaksCount  : Integer
                                          ) : Integer ;

      function  generateQuery             ( const _classBreak   : TObject ;
                                            const _lastClass    : Boolean
                                          ) : String ;

      function fieldIsString              ( const _field        : String
                                          ) : Boolean ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function  applyClassification       ( const _params       : TGIS_ParamsList ;
                                            const _discreteRamp : Boolean
                                          ) : Boolean ; override ;

      procedure classifyKMeansSpatial     ; override ;

      function  getMaxNumClasses          : Integer ; override ;

    public
      /// <inheritdoc/>
      constructor Create                  ( const _layer : TGIS_LayerVector
                                          ) ;

    public
      /// <summary>
      ///   If not empty, the attribute field of the specified name will be
      ///   populated with the class id values. Empty by default.
      ///   Ignored for 'KMeansSpatial' method.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Field must exist.
      ///   </note>
      /// </remarks>
      property ClassIdField : String
                              read  FClassIdField
                              write FClassIdField ;

      /// <summary>
      ///   Numeric field with values to be used in classification.
      ///   For 'KMeansSpatial' method, property is ignored.
      /// </summary>
      property Field        : String
                              read  FTarget
                              write FTarget ;

      /// <summary>
      ///   Starting size or width for generating Params section;
      ///   default is white.
      /// </summary>
      /// <remarks>
      ///   Works only for RenderType values:
      ///   TGIS_ClassificationRenderType.Size and
      ///   TGIS_ClassificationRenderType.OutlineWidth.
      /// </remarks>
      property StartSize    : Integer
                              read  FStartSize
                              write FStartSize ;

      /// <summary>
      ///   Ending size or width for generating Params section;
      ///   default is black.
      /// </summary>
      /// <remarks>
      ///   Works only for RenderType values:
      ///   TGIS_ClassificationRenderType.Size and
      ///   TGIS_ClassificationRenderType.OutlineWidth.
      /// </remarks>
      property EndSize      : Integer
                              read  FEndSize
                              write FEndSize ;

      /// <summary>
      ///   Visual property which will be affected during classification.
      /// </summary>
      property RenderType   : TGIS_ClassificationRenderType
                              read FRenderType
                              write FRenderType ;
  end;

  /// <summary>
  ///   Class that enables performing advanced pixel data classification
  ///   in the process of creating choropleth (thematic) maps.
  /// </summary>
  TGIS_ClassificationPixel = {$IFDEF OXYGENE} public  {$ENDIF}
                                              class( TGIS_ClassificationAbstract )
    private const  // constructor defaults
      MAX_NUM_CLASSES = 256 ;
      DEFAULT_BAND    = '1' ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function  applyClassification       ( const _params       : TGIS_ParamsList ;
                                            const _discreteRamp : Boolean
                                          ) : Boolean ; override ;

      procedure classifyKMeansSpatial     ; override ;

      function  getMaxNumClasses          : Integer ; override ;

    public
      /// <summary>
      ///   Creates an object. Always related to the layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be classified
      /// </param>
      constructor Create ( const _layer : TGIS_LayerPixel
                         ) ;

    public
      /// <summary>
      ///   Band from which values are used in classification.
      /// </summary>
      /// <remarks>
      ///   Band number, starting from 1, e.g.: '1', '2', '3', etc
      /// </remarks>
      property Band      : String
                           read  FTarget
                           write FTarget ;

  end;

  /// <summary>
  ///   Class for creating classification objects depending on the layer's type.
  /// </summary>
  TGIS_ClassificationFactory = {$IFDEF OXYGENE} public static {$ENDIF} class
    public
      /// <summary>
      ///   Creates an object that inherits from TGIS_ClassificationAbstract class
      ///   based on the layer's type.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be assigned to classifier
      /// </param>
      /// <returns>
      ///   new instance of class that inherits from TGIS_ClassificationAbstract
      ///   class or nil
      /// </returns>
      class function CreateClassifier ( const _layer : TGIS_Layer
                                      ) : TGIS_ClassificationAbstract ; static ;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Generics.Defaults,
    System.Variants,

    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource;
{$ENDIF}

{$REGION 'T_CLassBreak'}
type T_ClassBreak = class
  private
    FUnique : Variant;
    FClassBreak: TGIS_Point3D;

  public
    constructor Create  ( const _var : Variant
                        ) ; overload ;

    constructor Create  ( const _start    : Double ;
                          const _end      : Double ;
                          const _index    : Double ;
                          const _index_ex : Double
                        ) ; overload ;

  public
    property Unique     : Variant
                          read FUnique
                          write FUnique ;

    property Interval   : TGIS_Point3D
                          read FClassBreak
                          write FClassBreak ;
end;

constructor T_ClassBreak.Create(
  const _var : Variant
) ;
begin
  FUnique := _var ;
end;

constructor T_ClassBreak.Create( const _start, _end, _index, _index_ex: Double ) ;
begin
  FClassBreak := GisPoint3D( _start, _end, _index, _index_ex ) ;
end;
{$ENDREGION}

{$REGION 'T_DistComparer<T>'}
// Comparer with possibility to pass additional parameter used for
// sorting elements from dataset by distance to overall mean.
{$IFDEF DCC}
type
  T_DistComparer< T > = class( TComparer< T > )
    private
      class var FAverage       : TArray< Double > ;
      class var FData          : TGIS_Matrix ;

    public
      class property Average   : TArray< Double >   read FAverage ;
      class property Data      : TGIS_Matrix        read FData ;
      class function Construct ( const _comparison : TComparison< T > ;
                                 const _data       : TGIS_Matrix ;
                                 const _avg        : TArray< Double >
                               ) : IComparer< T > ;
  end;

  class function T_DistComparer< T >.Construct(
    const _comparison : TComparison< T > ;
    const _data       : TGIS_Matrix ;
    const _avg        : TArray< Double >
  ) : IComparer< T > ;
  begin
    FAverage := _avg ;
    FData := _data ;
    Result := inherited Construct( _comparison ) ;
  end;
{$ENDIF}
{$IFDEF CLR}
type
  T_DistComparer< T > = class( IComparer< T > )
    private
      class var FAverage       : TArray< Double > ;
      class var FData          : TGIS_Matrix ;
      class var FFnc           : Comparison<T> ;
    public
      class property Average   : TArray< Double >   read FAverage ;
      class property Data      : TGIS_Matrix        read FData ;
      constructor Create       ( const _comparison : Comparison< T > ;
                                 const _data       : TGIS_Matrix ;
                                 const _avg        : TArray< Double >
                               ) ;
      method Compare(x: T; y: T): Integer;
  end ;

  constructor T_DistComparer< T >.Create(
    const _comparison : Comparison< T > ;
    const _data       : TGIS_Matrix ;
    const _avg        : TArray< Double >
  ) ;
  begin
    inherited Create ;
    FAverage := _avg ;
    FData    := _data ;
    FFnc     := _comparison ;
  end;

  method T_DistComparer< T >.Compare(x: T; y: T): Integer;
  begin
    exit FFnc( x, y ) ;
  end ;
{$ENDIF}
{$IFDEF JAVA}
type
  T_DistCompare = function (
    _item1 : Integer ;
    _item2 : Integer
  ) : Integer ;

  T_DistComparer< T > = class( java.util.Comparator<Integer> )
    private
      class var FAverage       : TArray< Double > ;
      class var FData          : TGIS_Matrix ;
      class var FFnc           : T_DistCompare ;
    public
      class property Average   : TArray< Double >   read FAverage ;
      class property Data      : TGIS_Matrix        read FData ;
      constructor Create      (  const _comparison : T_DistCompare ;
                                 const _data       : TGIS_Matrix ;
                                 const _avg        : TArray< Double >
                               ) ;
      method compare(x: nullable Integer; y: nullable Integer): Integer;
  end ;

  constructor T_DistComparer< T >.Create(
    const _comparison : T_DistCompare ;
    const _data       : TGIS_Matrix ;
    const _avg        : TArray< Double >
  ) ;
  begin
    inherited Create ;
    FAverage := _avg ;
    FData    := _data ;
    FFnc     := _comparison ;
  end;

  method T_DistComparer< T >.compare(x: nullable Integer;  y: nullable Integer): Integer;
  begin
    exit FFnc( x, y ) ;
  end ;
{$ENDIF}
{$ENDREGION}

{$REGION 'T_KMeans'}
// Inspired by Hartigan's K-Means Clustering Algorithm
type
  T_KMeans = class
    private
      a : TGIS_Matrix ;         // data matrix, in original A(M,N)
      m : Integer ;             // number of points
      n  : Integer ;            // the number of dimensions
      c : TGIS_Matrix ;         // matrix of initial cluster centers

      k : Integer ;             // number of clusters
      ic1 : TArray< Integer > ; // cluster each point belongs to
      ic2 : TArray< Integer > ; // cluster which each point is most likely
                                // to be transferred to at each step

      nc :TArray< Integer > ;   // number of points in each cluster
      an1 :TArray< Double > ;
      an2 : TArray< Double > ;
      ncp : TArray< Integer > ;
      d : TArray< Double > ;

      itran : TArray< Boolean > ;
      live : TArray< Integer > ;
      iter : Integer ;          // maximum number of iterations allowed
      wss : TArray< Double > ;  // within-cluster sum of squares of each cluster
      ifault : Integer ;        // fault diagnostics

      index : Integer ;

      big : Double ;            // very large positive number

    private
      procedure centresInitialization ;
      procedure kmns ;
      procedure optra ;
      procedure qtran ;

    public
      property Clusters       : TArray<Integer> read ic1 ;
      property ClusterSizes   : TArray<Integer> read nc ;
      property ClusterCenters : TGIS_Matrix read c ;
      property Iterations     : Integer read iter ;

      /// <summary>
      ///   Fault diagnostics; see remarks.
      /// </summary>
      /// <remarks>
      ///   0 - No fault
      ///   1 - At least one cluster is empty after the initial assignment
      ///       (A better set of initial cluster centers is called for)
      ///   2 - The allowed maximum number of iterations is exceeded
      ///   3 - K is less than or equal to 1 or grater than or equal to M
      /// </remarks>
      property FaultCode : Integer read ifault ;

    public
      /// <summary>
      ///   Creates an object.
      /// </summary>
      constructor Create ( const _dataMatrix : TGIS_Matrix
                         ) ;
      /// <summary>
      ///   Run K-Means algorithm.
      /// </summary>
      /// <param name="_numClusters">
      ///   the number of clusters
      /// </param>
      /// <param name="_maxIter">
      ///   the maximum number of iterations allowed
      /// </param>
      /// <returns>
      ///   If True, operation succeed.
      /// </returns>
      function Run  ( const _numClusters : Integer ;
                      const _maxIter     : Integer
                    ) : Boolean ;

  end;

  constructor T_KMeans.Create (
    const _dataMatrix : TGIS_Matrix
  ) ;
  begin
    a := _dataMatrix ;
    m := length( a ) ;
    if m > 0 then
      n := length( a[0] )
    else
      n := 0 ;
  end;

  function T_KMeans.Run(
    const _numClusters : Integer ;
    const _maxIter     : Integer
  ) : Boolean ;
  begin
    Result := False ;
    if m * n = 0  then
      exit ;

    k := _numClusters ;
    iter := _maxIter ;

    centresInitialization ;

    kmns ;

    Result := ifault = 0 ;
  end ;

  function doCompareDist( const _left, _right : Integer ) : Integer ;
  var
    avg        : TArray< Double > ;
    data       : TGIS_Matrix ;
    dist_left  : Double ;
    dist_right : Double ;
    dist_diff  : Double ;
    val        : Int64 ;

    function calc_dist( const _i : Integer ) : Double ;
    var
      j : Integer ;
      delta : Double ;
    begin
      Result := 0.0 ;

      for j := 0 to length( data[0] ) - 1 do begin
        delta := data[_i, j] - avg[j] ;
        Result := Result + delta * delta ;
      end ;
    end ;

  begin
    avg := T_DistComparer< Integer >.Average ;
    data := T_DistComparer< Integer >.Data ;

    dist_left := calc_dist( _left ) ;
    dist_right := calc_dist( _right ) ;
    dist_diff := dist_left - dist_right ;

    // Round returns Int64, so for very big numbers,
    // it can throw floating-point invalid operation exception
    if dist_diff > 1000 then
      val := 1
    else if dist_diff < -1000 then
      val := -1
    else
      val := RoundS( dist_diff ) ;

    if val > 0 then
      Result := 1
    else if val < 0 then
      Result := -1
    else
      Result := 0 ;
  end ;

  procedure T_KMeans.centresInitialization ;
  var
    i, j, l      : Integer ;
    idx          : Integer ;
    avg          : TArray< Double > ;
    index_sorted : TArray< Integer > ;

    function calc_mean(
      const _j : Integer  // j-th dimension
    ) : Double ;
    var
      ii           : Integer ;
      online_avg   : Double ;
      online_delta : Double ;
    begin
      // online averaging method
      online_avg := 0 ;

      for ii := 0 to m-1 do begin
        online_delta := a[ii, _j] - online_avg ;
        online_avg := online_avg + online_delta / ( ii + 1 ) ;
      end ;
      Result := online_avg ;
    end ;

  begin
    SetLength( c, k, n ) ;
    SetLength( avg, n ) ;
    SetLength( index_sorted, m ) ;

    // index array for sorting purposes in next steps
    for i := 0 to m-1 do
      index_sorted[i] := i ;

    // procedure of obtaining the initial cluster centers:
    // 1. calculate overall mean of dataset
    for j := 0 to n-1 do
      avg[j] := calc_mean( j ) ;

    // 2. sort points by their distance to the mean;
    // values from dataset are sorted by their distances
    // to overall mean of the sample in ascending order
    // using custom T_DistComparer<T>
    {$IFDEF DCC}
      TArray.Sort<Integer>( index_sorted, T_DistComparer< Integer >.Construct( doCompareDist, a, avg ) ) ;
    {$ELSE}
      {$IFDEF JAVA}
        java.util.Arrays.sort( index_sorted, new T_DistComparer<Integer>( @doCompareDist, a, avg ) ) ;
      {$ELSE}
        TArray<Integer>.Sort( index_sorted, new T_DistComparer<Integer>( @doCompareDist, a, avg ) ) ;
      {$ENDIF}
    {$ENDIF}

    // 3. for L cluster select the (L * M / K)-th element
    for j := 0 to n-1 do begin
      for l := 0 to k-1 do begin
        idx := RoundS( l * m / k ) ;
        idx := index_sorted[idx] ;
        c[l, j] := a[idx, j] ;
      end ;
    end ;
  end ;

  procedure T_KMeans.kmns ;
  var
    i      : Integer ;                        // i-th point
    j      : Integer ;                        // j-th dimension

    l      : Integer ;                        // l-th cluster
    ij     : Integer ;                        // ij-th iteration
    il     : Integer ;                        // il-th nearest point

    ii     : Integer ;
    aa     : Double ;
    da     : Double ;
    dc     : Double ;
    db     : Double ;
    temp   : Double ;
    dt     : TArray< Double > ;
  begin
    SetLength( ic1, m ) ;
    SetLength( ic2, m ) ;
    SetLength( d, m ) ;

    SetLength( nc, k ) ;
    SetLength( an1, k ) ;
    SetLength( an2, k ) ;
    SetLength( ncp, k ) ;

    SetLength( itran, k ) ;
    SetLength( live, k ) ;
    SetLength( wss, k ) ;
    SetLength( dt, 2 ) ;

    // Define BIG to be a very large positive number.
    big := GIS_MAX_DOUBLE ;

    ifault := 3 ;
    if ( k <= 1 ) or ( k >= m ) then
      exit ;

    // For each point i, find its two closest centers,
    // IC1(I) and IC2(I). Assign it to IC1(I).
    for i := 0 to m-1 do begin                // (50) I=1,M
      ic1[i] := 0 ;                           // =1
      ic2[i] := 1 ;                           // =2

      for il := 0 to 1 do begin               // (10) IL=1,2
        dt[il] := 0.0 ;
        for j := 0 to n-1 do begin            // (10)
          da := a[i, j] - c[il, j] ;
          dt[il] := dt[il] + da * da ;
        end;
      end ;

      if dt[0] > dt[1] then begin             // inverted condition
        ic1[i] := 1 ;                         // =2
        ic2[i] := 0 ;                         // =1
        temp := dt[0] ;                       // DT[1]
        dt[0] := dt[1] ;                      // DT[1]=DT[2]
        dt[1] := temp ;                       // DT[2]
      end ;

      for l := 2 to k-1 do begin              // (20) L=3,K
        db := 0.0 ;
        for j := 0 to n-1 do begin            // (30)
          dc := a[i, j] - c[l, j] ;
          db := db + dc * dc ;
        end ;

        if db < dt[1] then begin              // inverted condition (goto 50)
                                              // (30)
          if db >= dt[0] then begin           // inverted condition (goto 40)
            dt[1] := db ;                     // DT[2]
            ic2[i] := l ;
          end
          else begin
            dt[1] := dt[0] ;                  // DT[2]=DT[1]
            ic2[i] := ic1[i] ;
            dt[0] := db ;                     // DT[1]
            ic1[i] := l ;
          end ;
        end ;
      end ;
    end ;                                     // (50)

    // Update cluster centers to be the average
    // of points contained within them.
    for l := 0 to k-1 do begin                // (70)
      nc[l] := 0 ;
      for j := 0 to n-1 do                    // (60)
        c[l, j] := 0.0 ;
    end ;

    // sum distances to centre in each cluster
    for i := 0 to m-1 do begin                // (90)
      l := ic1[i] ;
      nc[l] := nc[l] + 1 ;
      for j := 0 to n-1 do                    // (80)
        c[l, j] := c[l, j] + a[i, j] ;
    end ;

    // Check to see if there is any empty cluster at this stages
    ifault := 1 ;
    for l := 0 to k-1 do                      // (100)
      if nc[l] = 0 then
        exit ;

    ifault := 0 ;
    for l := 0 to k-1 do begin                // (120)
      aa := nc[l] ;                           // number of points in cluster L
      for j := 0 to n-1 do                    // (80)
        c[l, j] := c[l, j] / aa ;             // new cluster center -> avg=sum/n

      // Initialize AN1, AN2, ITRAN and NCP
      // AN1(L) is equal to NC(L) / (NC(L) - 1)
      // AN2(L) is equal to NC(L) / (NC(L) + 1)
      // ITRAN(L)=1 (True) if cluster is updated in the quick-transfer stage
      // ITRAN(L)=0 (False) otherwise
      // In the optimal-transfer stage, NCP(L) indicates the step at
      // which cluster L is last updated.
      // In the quick-transfer stage, NCP(L) is equal to the step at
      // which cluster L is last updated plus M.
      an2[l] := aa / ( aa + 1.0 ) ;
      an1[l] := big ;
      if aa > 1 then
        an1[l] := aa / ( aa - 1.0 ) ;
      itran[l] := True ;
      ncp[l] := -1 ;
    end ;

    // Since the specified number of iterations is exceeded
    // IFAULT is set to be equal to 2.
    // This may indicate unforeseen looping.
    ifault := 2 ;

    index := 0 ;
    for ij := 0 to iter-1 do begin            // (140)
      // In this stage, there is only one pass through the data.
      // Each point is reallocated, if necessary, to the cluster
      // that will include the maximum reduction in within-cluster
      // sum of squares.
      optra ;

      // Stop if no transfer took place in the last M
      // optimal-transfer steps.
      if index = m-1 then begin               // m-1 -> indexing from 0!
        ifault := 0 ;                         // (goto 150)
        break ;
      end ;

      // Each point is tested in turn to see if it should be
      // reallocated to the cluster which it is most likely to
      // be transferred to (IC2(I)) from its present cluster (IC1(I)).
      // Loop through the data until no further change is to take place.
      qtran ;

      // If there are only two clusters,
      // no need to re-enter optimal-transfer stage.
      if k = 2 then begin                     // (goto 150)
        ifault := 0 ;
        break ;
      end ;

      for l := 0 to k-1 do                    // (130)
        ncp[l] := 0 ;
    end ;
                                              // (150)
    // Compute within cluster sum of squares for each cluster.
    for l := 0 to k-1 do begin                // (160)
      wss[l] := 0.0 ;
      for j := 0 to n-1 do                    // (160)
        c[l,j] := 0.0 ;
    end ;

    for i := 0 to m-1 do begin                // (170)
      ii := ic1[i] ;
      for j := 0 to n-1 do                    // (170)
        c[ii, j] := c[ii, j] + a[i, j] ;
    end ;

    for j := 0 to n-1 do begin                // (190)
      for l := 0 to k-1 do                    // (180)
        c[l, j] := c[l, j] / nc[l] ;
      for i := 0 to m-1 do begin              // (190)
        ii := ic1[i] ;
        da := a[i, j] - c[ii, j] ;
        wss[ii] := wss[ii] + da * da ;
      end ;
    end ;
  end ;

  //  This is optimal-transfer stage.
  //  Each point is reallocated, if necessary, to the
  //  cluster that will induce a maximum reduction in
  //  the within-cluster sum of squares.
  procedure T_KMeans.optra ;
  var
    i                      : Integer ;        // i-th point
    j                      : Integer ;        // j-th dimension
    l                      : Integer ;        // l-th cluster
    l1, l2, ll             : Integer ;
    da, db, dc, de, df, dd : Double ;
    r2, rr                 : Double ;
    al1, al2, alt, alw     : Double ;
  begin
    // If cluster L is updated in the last quick-transfer stage,
    // it belongs to the live set throughout this stage.
    // Otherwise, at each step, it is not in the live set if it
    // has not been updated in the last M optimal-transfer steps.
    for l := 0 to k-1 do                      // (10)
      if itran[l] = True then
        live[l] := m + 1 ;

    for i := 0 to m-1 do begin                // (100)
      inc( index ) ;
      // 1-st and 2-nd nearest clusters for i-th point
      l1 := ic1[i] ;
      l2 := ic2[i] ;
      ll := l2 ;

      // If point I is the only member of cluster L1, no transfer.
      if nc[l1] <> 1 then begin               // inverted condition (goto 90)

        // If L1 has not yet been updated in this stage
        // no need to recompute D(I).
        if ncp[l1] <> 0 then begin            // inverted condition (goto 30)
          de := 0.0 ;
          for j := 0 to n-1 do begin          // (20)
            df := a[i, j] - c[l1, j] ;
            de := de + df * df ;
          end ;
          d[i] := de * an1[l1] ;
        end ;

        // Find the cluster with minimum R2
        da := 0.0 ;
        for j := 0 to n-1 do begin            // (40)
          db := a[i, j] - c[l2, j] ;
          da := da + db * db ;
        end ;
        r2 := da * an2[l2] ;

        for l := 0 to k-1 do begin            // (60)
          // If I is greater than or equal to live(L1), then L1 is
          // not in the live set. If this is true, we only need to
          // consider clusters that are in the live set for possible
          // transfer or point I. Otherwise, we need to consider
          // all possible clusters.
          if not (
            ( i >= live[l1] ) and
            ( i >= live[l] ) or
            ( l = l1 ) or
            ( l = ll )
          ) then begin                        // inverted condition (goto 60)
            rr := r2 / an2[l] ;
            dc := 0.0 ;
            for j := 0 to n-1 do begin        // (50)
              dd := a[i, j] - c[l, j] ;
              dc := dc + dd * dd ;
            end ;

            if dc < rr then begin             // inverted condition (goto 60)
              r2 := dc * an2[l] ;
              l2 := l ;
            end ;
          end ;
        end ;                                 // (60)

        if r2 >= d[i] then begin              // inverted condition (goto 70)
          // If no transfer is necessary, L2 is the new IC2(I)
          ic2[i] := l2 ;
        end
        else begin
          index := 0;
          live[l1] := m + i ;
          live[l2] := m + i ;
          ncp[l1] := i ;
          ncp[l2] := i ;
          al1 := nc[l1] ;
          alw := al1 - 1.0 ;
          al2 := nc[l2] ;
          alt := al2 + 1.0;
          for j := 0 to n-1 do begin          // (80)
            c[l1, j] := ( c[l1, j] * al1 - a[i, j] ) / alw ;
            c[l2, j] := ( c[l2, j] * al2 + a[i, j] ) / alt ;
          end ;
          // changing count for clusters L1 and L2
          nc[l1] := nc[l1] - 1 ;
          nc[l2] := nc[l2] + 1 ;
          an2[l1] := alw / al1 ;
          an1[l1] := big ;
          if alw > 1.0 then
            an1[l1] := alw / ( alw - 1.0 ) ;
          an1[l2] := alt / al2 ;
          an2[l2] := alt / ( alt + 1.0 ) ;
          ic1[i] := l2 ;
          ic2[i] := l1 ;
        end ;                                 // (90)
        if index = m-1 then                   // m-1 -> indexing from 0!
          exit ;
      end ;
    end ;                                     // (100)

    for l := 0 to k-1 do begin                // (110)
      // ITRAN[L] is set to zero (False) before entering QTRAN.
      // Also, LIVE[L] has to be decreased by M before
      // re-entering OPTRA.
      itran[l] := False ;
      live[l] := live[l] - m ;
    end ;
  end ;

  // This is the quick-transfer stage.
  // IC1(I) is the cluster which point I belongs to.
  // IC2(I) is the cluster which point I is most
  // likely to be transferred to.
  // For each point I, IC1(I) and IC2(I) are switched, if
  // necessary, to reduce within cluster sum of squares.
  // The cluster centers are updated after each step.
  procedure T_KMeans.qtran ;
  var
    l1, l2              : Integer ;
    i, j, icoun, istep  : Integer ;           // i-th point
    da, db, dd, de, r2  : Double ;
    al1, al2, alt, alw  : Double ;

  begin
    // In the optimal-transfer stage, NCP(L) indicates the
    // step at which cluster L is last updated.
    // In the quick-transfer stage, NCP(L) is equal to the
    // step at which cluster L is last updated plus M.
    icoun := 0;
    istep := 0 ;
    while True do begin
      for i := 0 to m-1 do begin              // (70)
        inc( icoun ) ;
        inc( istep ) ;
        // 1-st and 2-nd nearest clusters for i-th point
        l1 := ic1[i] ;
        l2 := ic2[i] ;

        // If point I is the only member of cluster L1, no transfer.
        if nc[l1] <> 1 then begin             // inverted condition (GOTO 60)

          // If ISTEP is greater than NCP(L1), no need to recompute
          // distance from point I to cluster L1.
          // Note that if cluster L1 is last updated exactly M steps
          // ago we still need to compute the distance from point I
          // to cluster L1.
          if istep <= ncp[l1] then begin      // inverted condition (GOTO 30)
            da := 0.0 ;
            for j := 0 to n-1 do begin        // (20)
              db := a[i, j] - c[l1, j] ;
              da := da + db * db ;
            end ;
            d[i] := da * an1[l1] ;
          end ;

          // If ISTEP is greater than equal to both NCP[L1] and
          // NCP[L2] there will be no transfer of point I at this step.
          if ( istep < ncp[l1] ) or           // (30)
             ( istep < ncp[l2] )
          then begin                          // inverted condition (GOTO 60)
            r2 := d[i] / an2[l2] ;
            dd := 0.0 ;
            for j := 0 to n-1 do begin        // (40)
              de := a[i, j] - c[l2, j] ;
              dd := dd + de * de ;
            end ;

            if dd < r2 then begin             // inverted condition (GOTO 60)

              // Update cluster centers, NCP, NC, ITRAN, AN1 and AN2
              // for clusters L1 and L2. Also, update IC1(I) and IC2(I).
              // Note that if any updating occurs in this stage,
              // index is set back to 0.
              icoun := 0 ;
              index := 0 ;
              itran[l1] := True ;
              itran[l2] := True ;
              ncp[l1] := istep + m ;
              ncp[l2] := istep + m ;
              al1 := nc[l1] ;
              alw := al1 - 1.0 ;
              al2 := nc[l2] ;
              alt:= al2 + 1.0 ;
              for j := 0 to n-1 do begin      // (50)
                c[l1, j] := ( c[l1, j] * al1 - a[i, j] ) / alw ;
                c[l2, j] := ( c[l2, j] * al2 + a[i, j] ) / alt ;
              end ;
              nc[l1] := nc[l1] - 1 ;
              nc[l2] := nc[l2] + 1 ;
              an2[l1] := alw / al1 ;
              an1[l1] := big ;
              if alw > 1.0 then
                an1[l1] := alw / ( alw - 1.0 ) ;
              an1[l2] := alt / al2 ;
              an2[l2] := alt / ( alt + 1.0 ) ;
              ic1[i] := l2;
              ic2[i] := l1;
            end ;
          end ;
        end ;

        // If no reallocation took place in the last M steps, returns.
        if icoun = m then                     // (60)
          exit ;
      end ;                                   // (70)
    end ;                                     // (10)
  end ;
{$ENDREGION}

{$REGION 'TGIS_ClassificationAbstract'}

// getters & setters
function TGIS_ClassificationAbstract.fget_ClassBreaks(
  const _index : Integer
) : Variant ;
var
  is_unique : Boolean ;
begin
  is_unique := ( FMethod = TGIS_ClassificationMethod.Unique ) ;

  if is_unique then
    Result := T_ClassBreak( classIntervalsLst[_index] ).Unique
  else
    if _index = NumClasses then
      Result := T_ClassBreak( classIntervalsLst[_index-1] ).Interval.Y
    else
      Result := T_ClassBreak( classIntervalsLst[_index] ).Interval.X ;
end;

function TGIS_ClassificationAbstract.fget_NumClasses : Integer ;
begin
  case FMethod of
    TGIS_ClassificationMethod.Manual : begin
      FNumClasses := classIntervalsLst.Count;
      if FNumClasses < 0 then
        FNumClasses := 0 ;
    end ;
    // Quartile is Quantile with 4 classes
    TGIS_ClassificationMethod.Quartile : begin
      FNumClasses := QUARTILE_NUM_CLASSES ;
    end ;
  end;

  Result := FNumClasses ;
end ;

procedure TGIS_ClassificationAbstract.fset_NumClasses(
  const _value : Integer
) ;
begin
  // can't set value for following methods:
  case FMethod of
    TGIS_ClassificationMethod.DefinedInterval,
    TGIS_ClassificationMethod.Manual,
    TGIS_ClassificationMethod.Quartile,
    TGIS_ClassificationMethod.StandardDeviation,
    TGIS_ClassificationMethod.StandardDeviationWithCentral,
    TGIS_ClassificationMethod.Unique
     : exit ;
  end ;

  if _value < 1 then
    FNumClasses := 1
  else if _value > getMaxNumClasses then
    FNumClasses := getMaxNumClasses
  else
    FNumClasses := _value ;
end ;

procedure TGIS_ClassificationAbstract.fset_Interval(
  const _value : Double
) ;
begin
  if ( _value <= 0 ) then
    FInterval := DEFAULT_INTERVAL
  else
    FInterval := _value ;
end ;

procedure TGIS_ClassificationAbstract.fset_Method(
  const _value : TGIS_ClassificationMethod
) ;
begin
  FMethod := _value ;
end;

// constructor & destructor
procedure TGIS_ClassificationAbstract.doCreate(
  const _layer : TGIS_Layer
) ;
begin
  FLayer := _layer ;
  FTarget := '' ;
  FColorRamp := nil ;
  FMethod := DEFAULT_METHOD ;
  FNumClasses := DEFAULT_NUM_CLASSES ;
  FInterval := DEFAULT_INTERVAL ;
  FStartColor := TGIS_Color.White ;
  FEndColor := TGIS_Color.Black ;
  FShowLegend := DEFAULT_SHOW_LEGEND ;

  classIntervalsLst := TObjectList< TObject >.Create ;
  manualBreaksLst := TList<Double>.Create ;
end;

procedure TGIS_ClassificationAbstract.doDestroy ;
begin
  FreeObject( classIntervalsLst ) ;
  FreeObject( manualBreaksLst ) ;
end;

// public methods
function TGIS_ClassificationAbstract.MustCalculateStatistics : Boolean ;
  procedure ensure_statistics(
    const _name : String;
    const _fun  : TGIS_StatisticalFunction
  ) ;
  begin
    Layer.Statistics.Add( _name, TGIS_StatisticalFunctions.Create( _fun ) ) ;
  end ;

procedure ensure_average ;
  begin
    ensure_statistics( FTarget, TGIS_StatisticalFunction.Average ) ;
  end ;

  procedure ensure_count ;
  begin
    ensure_statistics( FTarget, TGIS_StatisticalFunction.Count ) ;
  end ;

  procedure ensure_max ;
  begin
    ensure_statistics( FTarget, TGIS_StatisticalFunction.Max ) ;
  end ;

  procedure ensure_min ;
  begin
    ensure_statistics( FTarget, TGIS_StatisticalFunction.Min ) ;
  end ;

  procedure ensure_percentile ;
  var
    i           : Integer ;
    rank        : Double ;
    percentages : TGIS_VariantArray ;
  begin
    rank := 100 / NumClasses ;
    SetLength( percentages, NumClasses - 1) ;
    for i := 1 to NumClasses - 1 do
      percentages[i-1] := i * rank ;

    Layer.Statistics.Add(
      FTarget,
      TGIS_StatisticalFunction.Percentile,
      percentages,
      NullVar
    ) ;
  end ;

  procedure ensure_sample ;
  begin
    // use default Sample params (Size=1000, Replacement=False)
    ensure_statistics( FTarget, TGIS_StatisticalFunction.Sample ) ;
  end ;

  procedure ensure_stddev ;
  begin
    ensure_statistics( FTarget, TGIS_StatisticalFunction.StandardDeviation ) ;
  end ;

  procedure ensure_unique ;
  var
    params : TGIS_VariantArray ;
  begin
    ensure_statistics( FTarget, TGIS_StatisticalFunction.Unique ) ;
    SetLength( params, 1 ) ;
    params[0] := getMaxNumClasses ;
    Layer.Statistics.Get( FTarget ).Unique.Params := params ;
  end ;

begin
  Result := False ;
  if FMethod = TGIS_ClassificationMethod.Manual then
    exit;

  if IsStringEmpty( FTarget ) then begin
    // KMeansSpatial does not need a Target (Field)
    if ( FMethod = TGIS_ClassificationMethod.KMeansSpatial ) then
      exit
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FIELDNOEXIST ), FTarget, 0 ) ;
  end ;

  try
    case FMethod of
      TGIS_ClassificationMethod.KMeans,
      TGIS_ClassificationMethod.NaturalBreaks : begin
        ensure_sample ;
      end ;
      TGIS_ClassificationMethod.Quartile : begin
        ensure_percentile ;
      end ;
      TGIS_ClassificationMethod.Quantile : begin
        if NumClasses > 1 then
          ensure_percentile ;
      end ;
      TGIS_ClassificationMethod.StandardDeviation,
      TGIS_ClassificationMethod.StandardDeviationWithCentral : begin
        ensure_average ;
        ensure_stddev ;
      end ;
      TGIS_ClassificationMethod.Unique : begin
        // Unique does not need min & max
        ensure_unique ;
      end ;
    end ;
    ensure_min ;
    ensure_max ;
  finally
    Result := FLayer.MustCalculateStatistics or FLayer.Statistics.Obsolete ;
  end ;
end;

procedure TGIS_ClassificationAbstract.EstimateNumClasses ;
begin
  // calclulate NumClasses only for the following methods
  if not ( FMethod in [TGIS_ClassificationMethod.DefinedInterval,
                      TGIS_ClassificationMethod.StandardDeviation,
                      TGIS_ClassificationMethod.StandardDeviationWithCentral,
                      TGIS_ClassificationMethod.Unique] )
  then exit ;

  // the Target may not yet be set
  try
    if MustCalculateStatistics then
      FLayer.Statistics.Calculate ;
  except
    exit ;
  end ;

  determineMinMax ;

  // determine the number of classes
  case FMethod of
    TGIS_ClassificationMethod.DefinedInterval              : classifyDefinedInterval( true ) ;
    TGIS_ClassificationMethod.StandardDeviation            : classifyStandardDeviation( false, true ) ;
    TGIS_ClassificationMethod.StandardDeviationWithCentral : classifyStandardDeviation( true, true ) ;
    TGIS_ClassificationMethod.Unique                       : classifyUnique( true ) ;
  end ;
end;

function TGIS_ClassificationAbstract.Classify : Boolean ;
begin
  Result := Classify( FLayer.ParamsList ) ;
end ;

function TGIS_ClassificationAbstract.Classify(
  const _params : TGIS_ParamsList
)  : Boolean ;
begin
  Result := False ;

  if MustCalculateStatistics then
    exit ;

  classIntervalsLst.Clear ;

  if FMethod = TGIS_ClassificationMethod.KMeansSpatial then begin
    // KMeansSpatial works only for vector yet
    if not ( FLayer is TGIS_LayerVector ) then
      exit ;

    minValue := 0 ;
    maxValue := NumClasses ;
  end
  else begin
    if IsStringEmpty( FTarget ) then
      exit ;

    determineMinMax ;
  end ;

  if ( FMethod <> TGIS_ClassificationMethod.Unique ) and
     ( FMethod <> TGIS_ClassificationMethod.Manual ) and
     GisIsSameValue( minValue, maxValue, GIS_DOUBLE_RESOLUTION ) then
  begin
    FNumClasses := 1 ;
    addClassBreakInternal( minValue, maxValue ) ;
  end
  else begin
    case FMethod of
      TGIS_ClassificationMethod.DefinedInterval              : classifyDefinedInterval ;
      TGIS_ClassificationMethod.EqualInterval                : classifyEqualInterval ;
      TGIS_ClassificationMethod.GeometricalInterval          : classifyGeometricalInterval ;
      TGIS_ClassificationMethod.KMeans                       : classifyKMeansField ;
      TGIS_ClassificationMethod.KMeansSpatial                : classifyKMeansSpatial ;
      TGIS_ClassificationMethod.Manual                       : classifyManual ;
      TGIS_ClassificationMethod.NaturalBreaks                : classifyNaturalBreaksFisher ;
      TGIS_ClassificationMethod.Quantile                     : classifyQuantile ;
      TGIS_ClassificationMethod.Quartile                     : classifyQuartile ;
      TGIS_ClassificationMethod.StandardDeviation            : classifyStandardDeviation( False ) ;
      TGIS_ClassificationMethod.StandardDeviationWithCentral : classifyStandardDeviation( True ) ;
      TGIS_ClassificationMethod.Unique                       : classifyUnique ;
    end ;
  end ;

  Result := applyClassification( _params, colorRampIsDiscrete ) ;
end;

// classify methods
procedure TGIS_ClassificationAbstract.classifyDefinedInterval(
  const _onlyNumClasses : Boolean
) ;
begin
  // - (minus) GIS_DOUBLE_RESOLUTION to obtain a proper number of class brakes due to use rounding
  FNumClasses := Min(
    getMaxNumClasses,
    TruncS( ( maxValue - minValue ) / FInterval - GIS_DOUBLE_RESOLUTION ) + 1
  ) ;
  if ( NumClasses = 0 ) then
    FNumClasses := 1 ;

  if _onlyNumClasses then
    exit ;

  classifyInterval( ARITHMETIC_SEQUENCE, 0 ) ;
end;

procedure TGIS_ClassificationAbstract.classifyEqualInterval ;
begin
  FInterval := ( maxValue - minValue ) / NumClasses ;

  classifyInterval( ARITHMETIC_SEQUENCE, 0 ) ;
end;

procedure TGIS_ClassificationAbstract.classifyGeometricalInterval ;
var
  offset : Double ;
begin
  offset := 0.0 ;

  // for values <= 0 we use offset to work only on positives
  if ( minValue <=  GIS_DOUBLE_RESOLUTION ) then begin
    offset := Abs( minValue - 1 ) ;
    FInterval := Power(
      ( maxValue + offset ) / ( minValue + offset ), 1 / NumClasses
    ) ;
  end
  else begin
    FInterval := Power( maxValue / minValue, 1 / NumClasses ) ;
  end ;

  classifyInterval( GEOMETRIC_SEQUENCE, offset ) ;
end;

procedure TGIS_ClassificationAbstract.classifyInterval(
  const _sequenceType : Integer ;
  const _offset        : Double
) ;
var
  i        : Integer;
  curr_max : Double ;
  curr_min : Double ;
begin
  curr_max := minValue + _offset;

  if GisIsSameValue( FInterval, 0, GIS_DOUBLE_RESOLUTION ) then
    FNumClasses := 1 ;

  for i := 0 to NumClasses - 1 do begin
    curr_min := curr_max ;

    if ( _sequenceType = ARITHMETIC_SEQUENCE ) then
      curr_max := curr_min + FInterval
    else if ( _sequenceType = GEOMETRIC_SEQUENCE ) then
      curr_max := curr_min * FInterval ;

    if ( i = NumClasses - 1 ) then
      curr_max := maxValue + _offset ;

    addClassBreakInternal( curr_min - _offset, curr_max - _offset, i ) ;
  end ;
end;

procedure TGIS_ClassificationAbstract.classifyQuantile ;
var
  i, z         : Integer ;
  curr_max     : Double ;
  curr_min     : Double ;
  percentiles  : TList< Variant > ;
  stats_result : TGIS_StatisticsResult ;
begin
  stats_result := FLayer.Statistics.Get( FTarget ) ;
  if not stats_result.Percentile.Calculated then
    exit ;
  percentiles := stats_result.Percentile.Values ;

  curr_max := minValue ;

  z := 0 ;
  if NumClasses > 1 then begin
    for i := 0 to percentiles.Count - 1 do begin
      curr_min := curr_max ;
      curr_max := VarToDouble( percentiles[i] ) ;

      addClassBreakInternal( curr_min, curr_max, i ) ;
      z := i + 1 ;
    end ;
  end ;

  // percentiles are calculated only inside
  // so first and last value is taken directly
  addClassBreakInternal( curr_max, maxValue, z ) ;
end;

procedure TGIS_ClassificationAbstract.classifyQuartile ;
begin
  classifyQuantile ;
end;

procedure TGIS_ClassificationAbstract.classifyKMeansField ;
var
  data     : TGIS_Matrix ;
  clusters : TArray<Integer> ;

  function prepare_data : TGIS_Matrix ;
  var
    i            : Integer ;
    stats_result : TGIS_StatisticsResult ;
    sample       : TList< Variant > ;
    count        : Integer ;
  begin
    stats_result := FLayer.Statistics.Get( FTarget ) ;
    if not stats_result.Sample.Calculated then
      exit ;
    sample := stats_result.Sample.Values ;
    count := sample.Count ;

    // prepare data matrix M by N (M - count, N - dimensions)
    SetLength( Result, count, 1 ) ;
    for i := 0 to count-1 do
      Result[i, 0] := VarToDouble( sample[i] ) ;
  end ;

  procedure add_breaks_from_clusters(
    const _clusters : TArray<Integer>
  ) ;
  var
    i, l         : Integer ;  // i-th point; l-th cluster
    curr_max     : Double ;
    curr_min     : Double ;
    clusters_min : TArray< Double > ;
  begin
    // prepare array of max values in clusters
    SetLength( clusters_min, NumClasses ) ;
    for l := 0 to NumClasses - 1 do
      clusters_min[l] := GIS_MAX_DOUBLE ;

    // find max value in each cluster and update clusters_max
    for i := low( data ) to high( data ) do begin
      l := _clusters[i] ;
      if data[i, 0] < clusters_min[l] then
        clusters_min[l] := data[i, 0] ;
    end ;
    curr_max := 0 ;
    // add breaks
    {$IFDEF DCC}
      TArray.Sort<Double>( clusters_min ) ;
    {$ELSE}
      {$IFDEF JAVA}
        java.util.Arrays.sort( clusters_min ) ;
      {$ELSE}
        TArray<Double>.Sort( clusters_min ) ;
      {$ENDIF}
    {$ENDIF}
    for l := 0 to NumClasses-1 do begin
      if l < NumClasses-1 then begin
        curr_min := clusters_min[l] ;
        curr_max := clusters_min[l + 1] ;
      end
      else begin
        curr_min := curr_max ;
        curr_max := maxValue ;
      end ;
      addClassBreakInternal( curr_min, curr_max, l ) ;
    end ;
  end ;

begin
  data := prepare_data ;

  if classifyKMeans( data, clusters ) then
    add_breaks_from_clusters( clusters )
  else  // if failed create 1 class
   addClassBreakInternal( minValue, maxValue, 0 ) ;
end;

function TGIS_ClassificationAbstract.classifyKMeans(
  const _data     : TGIS_Matrix ;
  var   _clusters : TArray< Integer >
) : Boolean ;
  var
    i_num_classes : Integer ;
    kmns          : T_KMeans ;
begin
  Result := False ;

  kmns := T_KMeans.Create( _data ) ;
  try
    // KMeans will try classify data for smaller number of classes if necessary
    for i_num_classes := NumClasses downto 2 do begin
      if kmns.Run( i_num_classes, MAX_KMEANS_ITERATION ) then begin
        FNumClasses := i_num_classes ;
        _clusters := kmns.Clusters ;
        Result := True ;
        exit ;
      end ;
    end ;
  finally
    FreeObject( kmns ) ;
  end ;
end;

{$IFDEF JAVA OR ISLAND}
  function x_compare_fun( const Item1, Item2 : Variant ): Integer ;
  begin
    Result := VarCompareInt( Item1, Item2 ) ;
  end ;
{$ENDIF}

procedure TGIS_ClassificationAbstract.classifyManual ;
var
  i        : Integer;
  curr_min : Double ;
  curr_max : Double ;
begin
  {$IFDEF JAVA}
    java.util.Collections.sort( manualBreaksLst ) ;
  {$ELSE}
    {$IFDEF ISLAND}
      manualBreaksLst.Sort( (a, b) -> begin
        if a > b then
          Result := 1
        else if a < b then
          Result :=-1
        else
          Result := 0 ;
      end
      ) ;
    {$ELSE}
    manualBreaksLst.Sort ;
    {$ENDIF}
  {$ENDIF}

  for i := 0 to manualBreaksLst.Count-2 do begin
    curr_min := manualBreaksLst[i] ;
    curr_max := manualBreaksLst[i+1] ;
    addClassBreakInternal( curr_min, curr_max, i ) ;
  end;

  manualBreaksLst.Clear;
end;

//? should work on whole dataset or only samples
procedure TGIS_ClassificationAbstract.classifyNaturalBreaksFisher ;
var
  i, i2, i3    : Integer ;
  j, j2        : Integer ;
  l, l2        : Integer ;
  il, iu       : Integer ;
  k, m         : Integer ;
  sn, ik       : Integer ;
  s, ss        : Double ;
  variance     : Double ;
  val          : Double ;
  work         : array of array of Double ;
  iwork        : array of array of Integer ;
  stats_result : TGIS_StatisticsResult ;
  x            : TList< Variant > ;
  curr_max     : Double ;
  curr_min     : Double ;
begin
  stats_result := FLayer.Statistics.Get( FTarget ) ;
  if not stats_result.Sample.Calculated then
    exit ;
  x := TList< Variant >.Create ;
  try
    x.AddRange( stats_result.Sample.Values ) ;
    {$IFDEF JAVA OR ISLAND}
      x.Sort( @x_compare_fun ) ;
    {$ELSE}
      x.Sort ;
    {$ENDIF}

    k := NumClasses ;
    m := x.Count ;

    SetLength( iwork, m + 1, k + 1) ;
    SetLength( work, m + 1, k + 1) ;

    // inspired by Hartigan's implementation of Fisher's algorithm
    // Initialize and output data
    for j := 1 to k do begin
      iwork[1, j] := 1 ;
      work[1, j] := 0.0 ;
      for i := 1 to m do
        work[i, j] := GIS_MAX_DOUBLE ;
    end ;
    variance := 0 ;
    // Compute WORK and IWORK iteratively
    for i := 1 to m do begin
      ss := 0.0 ;
      s := 0.0 ;
      for i2 := 1 to i do begin
        i3 := i - i2 + 1 ;
        val := VarToDouble( x[i3-1] ) ;  // line doesn't exist in FORTRAN; -1 (indexing from 0)
        ss := ss + Sqr( val ) ;
        s:= s + val ;
        sn := i2 ;
        variance := ss - Sqr( s ) / sn ;
        ik := i3 - 1;
        if ik <> 0 then begin
          for j := 2 to k do begin
            if ( work[i,j] >= variance + work[ik, j-1] ) then begin
              iwork[i, j] := i3 ;
              work[i, j] := variance + work[ik, j-1] ;
            end ;
          end ;
        end ;
      end ;

      work[i, 1] := variance ;
      iwork[i, 1] := 1 ;
    end ;

    curr_max := maxValue ;

    // Print results
    j := 1 ;  // loop in FORTRAN; we take only last result from multivariate case
    j2 := k - j + 1 ;

    il := m + 1 ;
    for l := 1 to j2 do begin
      l2 := j2 - l + 1 ;
      iu := il - 1 ;
      il := iwork[iu, l2] ;  // iwork = MG in PFISH subroutine

      curr_min := VarToDouble( x[il-1] ) ;  // -1 because indexing from 0
      addClassBreakInternal( curr_min, curr_max, l2 ) ;

      curr_max := curr_min ;
    end;
  finally
    FreeObject( x ) ;
  end ;

  {$IFDEF JAVA}
    java.util.Collections.reverse( classIntervalsLst ) ;
  {$ELSE}
    classIntervalsLst.Reverse ;
  {$ENDIF}
end;

procedure TGIS_ClassificationAbstract.classifyStandardDeviation(
  const _withCentral     : Boolean ;
  const _onlyNumClasses : Boolean
) ;
var
  stats_result : TGIS_StatisticsResult ;
  avg          : Double ;
  std_dev      : Double ;
  std_interval : Double ;
  std_offset   : Double ;
  lower_count  : Integer ;
  upper_count  : Integer ;
  i            : Integer ;
  curr_min     : Double ;
  curr_max     : Double ;
  index_offset : Double ;
begin
  stats_result := FLayer.Statistics.Get( FTarget ) ;
  if not stats_result.Average.Calculated and
     not stats_result.StandardDeviation.Calculated
  then
    exit ;

  avg := stats_result.Average.Value ;
  std_dev := stats_result.StandardDeviation.Value ;
  std_interval := FInterval * std_dev ;

  // shift class breaks by half of the Interval
  if _withCentral then
    index_offset := 0.5
  else
    index_offset := 0 ;

  std_offset := std_interval * index_offset ;

  // no variability in data
  if GisIsSameValue( std_dev, 0, GIS_DOUBLE_RESOLUTION ) then begin
    lower_count := 0 ;
    upper_count := 0 ;
  end
  else begin
    // we can not exceed MAX_NUM_CLASSES
    lower_count := Min(
      getMaxNumClasses div 2,
      FloorS( ( avg - std_offset - minValue ) / std_interval - 0.0001 ) + 1
    ) ;
    upper_count := Min(
      getMaxNumClasses div 2,
      FloorS( ( maxValue - avg + std_offset ) / std_interval - 0.0001 ) + 1
    ) ;
  end ;

  // align number of classes down to avoid class breaks out of min-max range
  lower_count := Min( lower_count, upper_count ) ;

  // ensure the minimal number of class breaks
  if lower_count < 2 then
    lower_count := 2 ;

  // ensure that the last class break is multiple of 1 standard deviation
  if ( not _withCentral ) and
     ( not GisIsSameValue( FInterval, 1, GIS_DOUBLE_RESOLUTION ) )
  then
    lower_count := RoundS( FloorS( lower_count * FInterval ) / FInterval ) + 1 ;

  upper_count := lower_count ;

  if _withCentral then
    inc( upper_count ) ;

  FNumClasses := lower_count + upper_count ;

  if _onlyNumClasses then
    exit ;

  // class breaks are symetrical about the average value
  for i := -1 * lower_count to upper_count - 1 do begin
    curr_min := avg - std_offset + i * std_interval ;
    curr_max := curr_min + std_interval ;

    if ( i = -1 * lower_count) then
      curr_min := minValue ;
    if ( i = upper_count - 1 ) then
      curr_max := maxValue ;

    addClassBreakInternal(
      curr_min,
      curr_max,
      ( i - index_offset ) * FInterval ,
      ( i + 1 - index_offset ) * FInterval
    ) ;
  end ;
end;

procedure TGIS_ClassificationAbstract.classifyUnique(
  const _onlyNumClasses : Boolean = false
) ;
var
  stats_result : TGIS_StatisticsResult ;
  i            : Integer ;
begin
  stats_result := FLayer.Statistics.Get( FTarget ) ;
  if not stats_result.Unique.Calculated then
    exit ;

  FNumClasses := stats_result.Unique.Values.Count ;

  if _onlyNumClasses then
    exit ;

  for i := 0 to stats_result.Unique.Values.Count-1 do
    addClassBreakUnique( stats_result.Unique.Values[i] ) ;
end;

// other private methods
procedure TGIS_ClassificationAbstract.determineMinMax ;
var
  stats_result : TGIS_StatisticsResult ;
begin
  stats_result := FLayer.Statistics.Get( FTarget ) ;
  if not ( assigned( stats_result ) and
           stats_result.Min.Calculated and
           stats_result.Max.Calculated )
  then exit ;

  minValue := stats_result.Min.Value ;
  maxValue := stats_result.Max.Value ;
end;

function TGIS_ClassificationAbstract.generateLegend(
  const _classBreak : TObject  ;
  const _firstClass : Boolean ;
  const _lastClass  : Boolean
) : String ;
var
  closure      : String ;
  use_brackets : Boolean ;
  class_break  : T_ClassBreak ;
begin
  Result := '' ;

  class_break := T_ClassBreak( _classBreak ) ;

  use_brackets := False ;

  if _lastClass then
    closure := ']'
  else
    closure := ')' ;

  case FMethod of
    TGIS_ClassificationMethod.Quartile : begin
        if NumClasses = 1 then
          Result := 'Min - Max'
        else if class_break.Interval.Z = 0 then
          Result := 'Min - Q1'
        else if class_break.Interval.Z = 1 then
          Result := 'Q1 - Median'
        else if class_break.Interval.Z = 2 then
          Result := 'Median - Q3'
        else if class_break.Interval.Z = 3 then
          Result := 'Q3 - Max' ;
      end ;
    TGIS_ClassificationMethod.StandardDeviationWithCentral : begin
        if NumClasses = 1 then
          Result := 'Std Dev = 0'
        else if NumClasses = 1 then
          Result := Format(
            '%s - %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.Z, 2 ), DotFloatToStrPrec( class_break.Interval.M, 2 )]
          )
        else if _firstClass then
          Result := Format(
            '< %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.M, 2 )]
          )
        else if _lastClass then
          Result := Format(
            '>= %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.Z, 2 )]
          )
        else
          Result := Format(
            '%s - %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.Z, 2 ), DotFloatToStrPrec( class_break.Interval.M, 2 )]
          ) ;
      end ;
    TGIS_ClassificationMethod.StandardDeviation : begin
        if NumClasses = 1 then
          Result := 'Std Dev = 0'
        else if _firstClass then
          Result := Format(
            '< %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.M, 2 )]
          )
        else if _lastClass then
          Result := Format(
            '>= %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.Z, 2 )]
          )
        else
          Result := Format(
            '%s - %s Std Dev',
            [DotFloatToStrPrec( class_break.Interval.Z, 2 ), DotFloatToStrPrec( class_break.Interval.M, 2 )]
          ) ;
      end ;

    TGIS_ClassificationMethod.Unique :begin
        if IsVariantString( class_break.Unique ) then
          Result := VarToString( class_break.Unique )
        else
          Result := DotFloatToStr( VarToDouble( class_break.Unique ) ) ;
      end ;
    else
      begin
        if use_brackets then
          Result := Format(
            '[%s; %s%s',
            [DotFloatToStrPrec( class_break.Interval.X, 2 ),
             DotFloatToStrPrec( class_break.Interval.Y, 2 ),
             closure]
          )
        else
          Result := Format(
            '%s - %s',
            [DotFloatToStrPrec( class_break.Interval.X, 2 ), DotFloatToStrPrec( class_break.Interval.Y, 2 )]
          ) ;
      end ;
  end ;
end;

function TGIS_ClassificationAbstract.interpolateColor(
  const _iBreak       : Integer ;
  const _breaksCount  : Integer ;
  const _discreteRamp : Boolean
) : TGIS_Color ;
var
  i              : Integer ;
  color_ramp_len : Integer ;
  start_color    : TGIS_Color ;
  end_color      : TGIS_Color ;
  start_index    : Double ;
  end_index      : Double ;
  ratio          : Double ;
  ratio_100      : Double  ;
begin
  // default or if using color ramp failed
  start_color := FStartColor ;
  end_color := FEndColor ;

  ratio := calcInterpolationRatio( _iBreak, _breaksCount ) ;

  // use color ramp
  if assigned( FColorRamp )then begin
    ratio_100 := ratio * 100 ;
    color_ramp_len  := length( FColorRamp ) ;

    if _discreteRamp and ( FMethod=TGIS_ClassificationMethod.Unique ) then begin
      // in discrete ramps colors are doubled, so we select every second
      i := _iBreak mod ( color_ramp_len div 2 );
      Result := FColorRamp[2*i].RGB ;
      exit ;
    end ;

    // use exact colors, colormap has adequate realization
    if ( _breaksCount = color_ramp_len ) then begin
      Result := FColorRamp[_iBreak].RGB ;
      exit ;
    end ;

    // if STDEV=0 try get middle color from color ramp
    if ( ( FMethod = TGIS_ClassificationMethod.StandardDeviation ) or
         ( FMethod = TGIS_ClassificationMethod.StandardDeviationWithCentral ) )
       and ( _breaksCount = 1 )
    then begin
      Result := FColorRamp[color_ramp_len div 2].RGB ;
      exit ;
    end ;

    if ratio_100 <= GIS_DOUBLE_RESOLUTION then begin
      Result := FColorRamp[0].RGB ;
      exit ;
    end
    else if ratio_100 >= ( 100 - GIS_DOUBLE_RESOLUTION ) then begin
      Result := FColorRamp[color_ramp_len-1].RGB ;
      exit ;
    end
    else begin
      for i := 0 to color_ramp_len - 2 do begin
        start_index := FColorRamp[i].Index ;
        end_index := FColorRamp[i+1].Index ;
        if ( start_index < ratio_100 ) and
           ( ratio_100 <= end_index ) then
        begin
          start_color := FColorRamp[i].RGB ;
          end_color := FColorRamp[i+1].RGB ;
          ratio := ( ratio_100 - start_index ) / ( end_index - start_index ) ;
          break ;
        end ;
      end ;
    end ;
  end ;

  Result := GradientColor(
    start_color,
    end_color,
    0,
    1,
    ratio,
    TGIS_ColorInterpolationMode.RGB
  ) ;
end;

function TGIS_ClassificationAbstract.colorRampIsDiscrete : Boolean ;
var
  i : Integer ;
begin
  Result := False ;
  for i := 0 to ( length( FColorRamp ) div 2 ) - 1 do
    Result := True and ( FColorRamp[2*i].RGB = FColorRamp[2*i+1].RGB) ;
    if not Result then
      exit ;
end;

function TGIS_ClassificationAbstract.calcInterpolationRatio(
  const _i : Integer ;
  const _n : Integer
) : Double ;
begin
  if _n = 1 then
    Result := 0.0
  else
    Result := Min( 1.0, _i / ( _n - 1 ) ) ;
end ;

procedure TGIS_ClassificationAbstract.AddClassBreak( const _breakValue: Double ) ;
begin
  manualBreaksLst.Add( _breakValue ) ;
end;

procedure TGIS_ClassificationAbstract.addClassBreakInternal(
  const _startValue : Double ;
  const _endValue   : Double ;
  const _index      : Double ;
  const _indexEx    : Double
) ;
begin
  classIntervalsLst.Add( T_ClassBreak.Create(
    _startValue, _endValue, _index, _indexEx )
  ) ;
end;

procedure TGIS_ClassificationAbstract.addClassBreakUnique(
  const _uniqueValue: Variant
) ;
begin
  classIntervalsLst.Add( T_ClassBreak.Create( _uniqueValue ) ) ;
end;

function TGIS_ClassificationAbstract.classBreakIsEmpty(
  const _classBreak : TObject
) : Boolean ;
var
  class_break : T_ClassBreak ;
begin
  class_break := T_ClassBreak( _classBreak ) ;

  if ( FMethod = TGIS_ClassificationMethod.Unique ) then
    Result := VarIsEmpty( class_break.Unique )
  else
    Result := IsNan( class_break.Interval.X ) or IsNan( class_break.Interval.Y ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_ClassificationVector'}
 constructor TGIS_ClassificationVector.Create(
  const _layer : TGIS_LayerVector
) ;
begin
  doCreate( TGIS_Layer ( _layer ) );
  FClassIdField := '' ;
  FStartSize := DEFAULT_START_SIZE ;
  FEndSize := DEFAULT_END_SIZE ;
  FRenderType := TGIS_ClassificationRenderType.Color ;
end;

procedure TGIS_ClassificationVector.classifyKMeansSpatial ;
var
  lv       : TGIS_LayerVector ;
  data     : TGIS_Matrix ;
  clusters : TArray<Integer> ;

  function prepare_data : TGIS_Matrix ;
  var
    i     : Integer ;
    count : Integer ;
    is_3d : Boolean ;
    pnt   : TGIS_Point ;
    shp   : TGIS_Shape ;
    {$IFDEF DCC}
      obj : TGIS_Shape ;
    {$ENDIF}
  begin
    count := lv.GetLastUid ;
    is_3d := not IsStringEmpty( FTarget ) ;

    if is_3d then
      SetLength( Result, count , 3 )
    else
      SetLength( Result, count , 2 ) ;

    i := 0 ;
    for obj in lv.Loop do begin
      {$IFDEF DCC}
        shp := obj ;
      {$ELSE}
        shp := TGIS_Shape( obj ) ;
      {$ENDIF}

      pnt := shp.Centroid ;

      Result[i, 0] := pnt.X ;
      Result[i, 1] := pnt.Y ;

      if is_3d then begin
        Result[i, 2] := VarToDouble( shp.GetField( FTarget ) ) ;
      end ;

      inc( i ) ;
    end ;
  end ;

  procedure add_breaks_from_clusters(
    const _clusters : TArray<Integer>
  ) ;
  var
    i : Integer ;
    shp : TGIS_Shape ;
    {$IFDEF DCC}
      obj : TGIS_Shape ;
    {$ENDIF}
  begin
    if lv.FindField( FTarget ) < 0 then
      lv.AddField( FTarget, TGIS_FieldType.Number, 2, 0 ) ;

    i := 0 ;
    for obj in lv.Loop do begin
      {$IFDEF DCC}
        shp := obj ;
      {$ELSE}
        shp := TGIS_Shape( obj ) ;
      {$ENDIF}
      shp.MakeEditable.SetField( FTarget, _clusters[i] ) ;
      inc( i ) ;
    end ;

    for i:= 0 to NumClasses - 1 do
      addClassBreakInternal( i, i + 1, i ) ;
  end ;

begin
  FShowLegend := False ;
  lv := TGIS_LayerVector( FLayer ) ;
  FTarget := GIS_CLASSIFY_KMEANS_ID ;
  FClassIdField := '' ;

  data := prepare_data ;
  if classifyKMeans( data, clusters ) then
    add_breaks_from_clusters( clusters )
  else
    addClassBreakInternal( NaN, NaN ) ;
end;

function TGIS_ClassificationVector.applyClassification(
  const _params       : TGIS_ParamsList ;
  const _discreteRamp : Boolean
) : Boolean ;
var
  i_break      : Integer ;
  breaks_count : Integer ;
  fill_field   : Boolean ;
  first_class  : Boolean ;
  last_class   : Boolean ;
  lv           : TGIS_LayerVector ;
  class_break  : TObject ;
  class_query  : String ;
  class_legend : String ;
  class_color  : TGIS_Color ;
  class_width  : Integer ;
begin
  Result := False ;

  lv := TGIS_LayerVector( FLayer ) ;
  fill_field := IsStringEmpty( FClassIdField ) and
                ( lv.FindField( FClassIdField ) > 0 ) ;

  first_class := True ;
  _params.Clear ;

  // Unique method need additional "Other" section at the beginning
  if ( FMethod = TGIS_ClassificationMethod.Unique ) then begin
    // use defaults for "Other" section
    class_query := '' ;
    class_legend := GIS_RS_LEGEND_OTHER ;
    class_color := TGIS_Color.None ;
    class_width := 0 ;

    setParams(
      _params,
      lv.DefaultShapeType,
      class_query,
      class_legend,
      class_color,
      class_width
    ) ;
    first_class := False ;
  end ;

  breaks_count := classIntervalsLst.Count ;
  if breaks_count = 0 then
    exit ;

  for i_break := 0 to breaks_count-1 do begin
    last_class := ( i_break = breaks_count - 1 ) ;

    if not first_class then
      _params.Add ;

    class_break := classIntervalsLst[i_break] ;

    if classBreakIsEmpty( class_break ) then
      continue ;

    class_query := generateQuery( class_break, last_class ) ;
    class_legend := generateLegend( class_break, first_class, last_class ) ;
    class_color := interpolateColor( i_break, breaks_count, _discreteRamp ) ;
    class_width := interpolateWidth( i_break, breaks_count ) ;

    setParams(
      _params,
      lv.DefaultShapeType,
      class_query,
      class_legend,
      class_color,
      class_width
    ) ;

    if first_class then
      first_class := False ;

    if fill_field then
      do_fill_field( class_query, i_break ) ;
  end ;

  Result := True ;
end;

function TGIS_ClassificationVector.generateQuery(
  const _classBreak : TObject ;
  const _lastClass  : Boolean
) : String ;
var
  closure     : String ;
  fname       : String ;
  query_str   : String ;
  unique_str  : String ;
  class_break : T_ClassBreak ;
begin
  class_break := T_ClassBreak( _classBreak ) ;
  fname := GisNormalizedSQLName( FTarget ) ;

  if ( FMethod = TGIS_ClassificationMethod.Unique ) then begin
    if fieldIsString( FTarget ) then begin
      query_str := '%s = ''%s''' ;
      unique_str := VarToString( class_break.Unique ) ;
    end
    else begin
      query_str := '%s = %s' ;
      unique_str := DotFloatToStr( VarToDouble( class_break.Unique ) ) ;
    end ;

    Result := Format( query_str, [fname, unique_str] ) ;
  end
  else begin
    query_str := '%s >= %s AND %s %s %s' ;

    if _lastClass then
      closure := '<='
    else
      closure := '<' ;

    Result := Format(
      query_str,
      [fname, DotFloatToStr( class_break.Interval.X ),
       fname, closure, DotFloatToStr( class_break.Interval.Y )]
    ) ;
  end ;
end;

function TGIS_ClassificationVector.fieldIsString(
  const _field : String
) : Boolean ;
var
  lv       : TGIS_LayerVector ;
  field_id : Integer ;
begin
  lv := TGIS_LayerVector( FLayer ) ;
  field_id := lv.FindField( FTarget ) ;

  if field_id < GIS_FIELD_ID_UID then
    Result := ( lv.Fields[field_id].FieldType = TGIS_FieldType.String )
  // virtual fields
  else
    Result := ( lv.FieldsVirtual[field_id-GIS_FIELD_ID_UID].FieldType = TGIS_FieldType.String ) ;
end;

function TGIS_ClassificationVector.getMaxNumClasses : Integer ;
begin
  Result := MAX_NUM_CLASSES ;
end;

procedure TGIS_ClassificationVector.do_fill_field(
  const _classQuery : String ;
  const _iBreak     : Integer
) ;
var
  shp   : TGIS_Shape ;
  {$IFDEF DCC}
    obj : TGIS_Shape ;
  {$ENDIF}
begin
  for obj in TGIS_LayerVector( FLayer ).Loop( GisWholeWorld, _classQuery ) do begin
    {$IFDEF DCC}
      shp := obj ;
    {$ELSE}
      shp := TGIS_Shape( obj ) ;
    {$ENDIF}
    shp.MakeEditable.SetField( FClassIdField, _iBreak ) ;
  end ;
end;

procedure TGIS_ClassificationVector.setParams(
  const _params    : TGIS_ParamsList ;
  const _shapeType : TGIS_ShapeType ;
  const _query     : String ;
  const _legend    : String ;
  const _color     : TGIS_Color ;
  const _width     : Integer
) ;
var
  use_default_color : Boolean ;
  use_default_width : Boolean ;
  marker            : TGIS_ParamsMarker ;
  line              : TGIS_ParamsLine ;
  area              : TGIS_ParamsArea ;
  section           : TGIS_ParamsSectionVector ;
begin
  section := TGIS_ParamsSectionVector( _params.Items[_params.Count-1] ) ;
  section.Query := _query ;
  section.Legend := _legend ;

  use_default_color := ( _color = TGIS_Color.None ) ;
  use_default_width := ( _width = 0 ) ;

  case _shapeType of
    TGIS_ShapeType.Point,
    TGIS_ShapeType.MultiPoint : begin  // points
      marker := section.Marker ;
      case FRenderType of
        TGIS_ClassificationRenderType.Size :
          if not use_default_width then marker.Size := _width ;
        TGIS_ClassificationRenderType.Color :
          if not use_default_color then marker.Color := _color ;
        TGIS_ClassificationRenderType.OutlineWidth :
          if not use_default_width then marker.OutlineWidth := _width ;
        TGIS_ClassificationRenderType.OutlineColor :
          if not use_default_color then marker.OutlineColor := _color ;
      end ;
    section.Marker.ShowLegend := ShowLegend ;
    end ;
    TGIS_ShapeType.Arc : begin         // lines
      line := section.Line ;
      case FRenderType of
        TGIS_ClassificationRenderType.Size :
          if not use_default_width then line.Width := _width ;
        TGIS_ClassificationRenderType.Color :
          if not use_default_color then line.Color := _color ;
        TGIS_ClassificationRenderType.OutlineWidth :
          if not use_default_width then line.OutlineWidth := _width ;
        TGIS_ClassificationRenderType.OutlineColor :
          if not use_default_color then line.OutlineColor := _color ;
      end ;
    section.Line.ShowLegend := ShowLegend ;
    end ;
    TGIS_ShapeType.Polygon,
    TGIS_ShapeType.MultiPatch : begin  // polygons
      area := section.Area ;
      case FRenderType of
        TGIS_ClassificationRenderType.Size :
          exit ;  // render type not allowed for polygons
        TGIS_ClassificationRenderType.Color :
          if not use_default_color then area.Color := _color ;
        TGIS_ClassificationRenderType.OutlineWidth :
          if not use_default_width then area.OutlineWidth := _width ;
        TGIS_ClassificationRenderType.OutlineColor :
          if not use_default_color then area.OutlineColor := _color ;
      end ;
      section.Area.ShowLegend := ShowLegend ;
    end ;
  end ;
end ;

function TGIS_ClassificationVector.interpolateWidth(
  const _iBreak      : Integer ;
  const _breaksCount : Integer
) : Integer ;
var
  ratio : Double ;
begin
  ratio := calcInterpolationRatio( _iBreak, _breaksCount ) ;
  Result := RoundS( ( 1 - ratio ) * FStartSize + ratio * FEndSize ) ;
end;

{$ENDREGION}

{$REGION 'TGIS_ClassificationPixel'}
constructor TGIS_ClassificationPixel.Create(
  const _layer : TGIS_LayerPixel
) ;
begin
  doCreate( TGIS_Layer ( _layer ) );
  FTarget := DEFAULT_BAND ;
end;

procedure TGIS_ClassificationPixel.classifyKMeansSpatial ;
begin
  // no implementation for pixel layers
end;

function TGIS_ClassificationPixel.getMaxNumClasses : Integer ;
begin
  Result := MAX_NUM_CLASSES ;
end;

function TGIS_ClassificationPixel.applyClassification(
  const _params       : TGIS_ParamsList ;
  const _discreteRamp : Boolean
) : Boolean ;
var
  lp           : TGIS_LayerPixel ;
  i_break      : Integer;
  band_int     : Integer ;
  breaks_count : Integer ;
  first_class  : Boolean ;
  last_class   : Boolean ;
  zone_def     : String ;
  class_legend : String ;
  class_color  : TGIS_Color ;
  class_break  : T_ClassBreak;
  prm          : TGIS_ParamsSectionPixel ;
  start_val    : Double ;
  end_val      : Double ;
begin
  Result := False ;

  lp := TGIS_LayerPixel( Layer ) ;

  // Fast statistics are calculated by default,
  // so min/max could be inaccurate for big pixel layers
  minValue := Min( minValue, lp.MinHeight ) ;
  maxValue := Max( maxValue, lp.MaxHeight ) ;
  end_val := maxValue ;

  breaks_count := classIntervalsLst.Count ;
  if breaks_count = 0 then
    exit ;

  prm := TGIS_ParamsSectionPixel.Create ;
  try
    prm.Pixel.Antialias := lp.Params.Pixel.Antialias ;
    prm.Pixel.GridShadow := lp.Params.Pixel.GridShadow ;
    if TryStrToInt( Band, band_int ) then
      prm.Pixel.GridBand := band_int
    else
      prm.Pixel.GridBand := StrToInt( GIS_BAND_DEFAULT ) ;

    for i_break := 0 to breaks_count-1 do begin
      first_class := ( i_break = 0 ) ;
      last_class := ( i_break = breaks_count - 1 ) ;

      class_break := T_ClassBreak ( classIntervalsLst[i_break] ) ;
      class_color := interpolateColor( i_break, breaks_count, _discreteRamp ) ;
      class_legend := generateLegend( class_break, first_class, last_class ) ;

      if ( FMethod = TGIS_ClassificationMethod.Unique ) then begin
        start_val := VarToDouble( class_break.Unique ) ;
        end_val := start_val ;

        // add first zone for other values
        if first_class and ( ( minValue - start_val ) < -GIS_DOUBLE_RESOLUTION )
        then begin
          zone_def := Format(
            '%s,%s,%s,%s',
            [DotFloatToStr( minValue ),
             DotFloatToStr( start_val ),
             ConstructParamColor( TGIS_Color.LightGray ),
             GIS_RS_LEGEND_OTHER]
          ) ;

          prm.Pixel.AltitudeMapZones.Add( zone_def ) ;
        end ;
      end
      else begin
        start_val := class_break.Interval.X ;
        end_val := class_break.Interval.Y ;
      end;

      zone_def := Format(
        '%s,%s,%s,%s',
        [DotFloatToStr( start_val ),
         DotFloatToStr( end_val ),
         ConstructParamColor( class_color ),
         class_legend]
      ) ;

      prm.Pixel.AltitudeMapZones.Add( zone_def ) ;
    end ;

    // add last zone for other values
    if ( FMethod = TGIS_ClassificationMethod.Unique ) and
       ( ( maxValue - end_val ) > GIS_DOUBLE_RESOLUTION )
    then begin
      zone_def := Format(
        '%s,%s,%s,%s',
        [DotFloatToStr( end_val ),
         DotFloatToStr( maxValue ),
         ConstructParamColor( TGIS_Color.LightGray ),
         GIS_RS_LEGEND_OTHER]
      ) ;

      prm.Pixel.AltitudeMapZones.Add( zone_def ) ;
    end ;

    prm.Pixel.ShowLegend := FShowLegend ;

    _params.Clear ;
    _params[0].Assign( prm ) ;

    Result := True ;
  finally
    FreeObject( prm ) ;
  end;
end;
{$ENDREGION}

{$REGION 'TGIS_ClassificationFactory'}
class function TGIS_ClassificationFactory.CreateClassifier(
  const _layer : TGIS_Layer
) : TGIS_ClassificationAbstract ;
begin
  if _layer is TGIS_LayerVector then
    Result := TGIS_ClassificationVector.Create( TGIS_LayerVector( _layer ) )
  else if _layer is TGIS_LayerPixel then
    Result := TGIS_ClassificationPixel.Create( TGIS_LayerPixel( _layer ) )
  else
    Result := nil ;
end;
{$ENDREGION}

end.
