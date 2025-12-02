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
  Provides statistics for layers.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoStatistics;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoStatistics"'}
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
    TatukGIS.RTL.XML
  {$ENDIF}
  {$IFDEF DCC}
    System.Generics.Collections,
    System.Variants,

    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes
  {$ENDIF}
  {$IFDEF JAVA}
    remobjects.elements.rtl.*,
    tatukgis.rtl ,
    tatukgis.rtl.xml
  {$ENDIF}
  {$IFDEF ISLAND}
    remobjects.elements.rtl.*,
    TatukGIS.RTL ,
    TatukGIS.RTL.XML
  {$ENDIF}
  // uses for unit tests
  {$IFDEF GIS_UNIT_TESTS}
    {$IFDEF CLR}
      ,
      ttkNDUnit,
      Defines,
      NUnit.Framework,
      TatukGIS.NDK.Common;
    {$ENDIF}
    {$IFDEF DCC}
      ,
      ttkNDUnit,
      Defines;
    {$ENDIF}
    {$IFDEF JAVA}
      ,
      tatukgis.junit;
    {$ENDIF}
  {$ELSE}
    ;
  {$ENDIF}

type
  {$IFDEF GIS_UNIT_TESTS}
    // Unit tests of the implementation section functionality
    [TestFixture]
    TestUnitStatistics = {$IFDEF OXYGENE} public {$ENDIF} class( TttkNDUnit )
      public
        [Test] procedure TestRandomSampling ;
    end;
  {$ENDIF}

  /// <summary>
  ///   Available statistical functions
  /// </summary>
  TGIS_StatisticalFunction ={$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   The average value (arithmetic mean) of dataset.
    /// </summary>
    Average,

    /// <summary>
    ///   Number of valid elements in dataset (not empty, not null).
    /// </summary>
    Count,

    /// <summary>
    ///   Number of empty or null elements in dataset.
    /// </summary>
    CountMissings,

    /// <summary>
    ///   The maximum value in dataset.
    /// </summary>
    Max,

    /// <summary>
    ///   The value that occurs most often in dataset.
    /// </summary>
    Majority,

    /// <summary>
    ///   The median (middle value) of dataset.
    /// </summary>
    Median,

    /// <summary>
    ///   The minimum value in dataset.
    /// </summary>
    Min,

    /// <summary>
    ///   The value that occurs least often in dataset.
    /// </summary>
    Minority,

    /// <summary>
    ///   The value below which a percentage of data falls.
    ///   Not available for strings.
    /// </summary>
    /// <remarks>
    ///   Percentages can be passed by
    ///   '_params=[p1, p2, ..., pN]', where:
    ///   p (Double) - the requested percentile value in range 0..100, inclusive.
    ///   By default 25th (first quartile), 50th (median)
    ///   and 75th (third quartile) percentiles are calculated.
    /// </remarks>
    Percentile,

    /// <summary>
    ///   The difference between maximum and minimum values in dataset.
    /// </summary>
    Range,

    /// <summary>
    ///   The standard deviation of dataset.
    /// </summary>
    StandardDeviation,

    /// <summary>
    ///   The sum of values in dataset.
    /// </summary>
    Sum,

    /// <summary>
    ///   The variance of dataset.
    /// </summary>
    Variance,

    /// <summary>
    ///   The number of unique values in dataset.
    /// </summary>
    Variety,

    /// <summary>
    ///   Extracting unique values from dataset.
    /// </summary>
    /// <remarks>
    ///   Parameters for this function can be passed by
    ///   "_params=[Limit]', where:
    ///   Limit (Integer) - number of elements to extract; default is 65536.
    /// </remarks>
    Unique,

    /// <summary>
    ///   Extracting sample values from dataset.
    /// </summary>
    /// <remarks>
    ///   Parameters for this function can be passed by
    ///   "_params=[Size, Replacement]', where:
    ///   Size (Integer) - number of elements to extract; default is 256.
    ///   Replacement (Boolean) - if True, samples will be taken with replacement;
    ///   default is False.
    /// </remarks>
    Sample
  ) ;

  /// <summary>
  ///   Record with all statistical functions.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     The record is used to activate requested statistical functions for
  ///     calculation by setting True for specific.
  ///   </para>
  ///   <para>
  ///     Predefined records can be used: EmptyStatistics, BasicStatistics,
  ///     StandardStatistics and AllStatistics.
  ///   </para>
  /// </remarks>
  TGIS_StatisticalFunctions = {$IFDEF OXYGENE} public {$ENDIF}
                              {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}

    public
      /// <summary>
      ///   The average value (arithmetic mean) of dataset.
      /// </summary>
      Average           : Boolean ;

      /// <summary>
      ///   Number of valid elements in dataset (not empty, not null).
      /// </summary>
      Count             : Boolean ;

      /// <summary>
      ///   Number of empty or null elements in dataset.
      /// </summary>
      CountMissings     : Boolean ;

      /// <summary>
      ///   The maximum value in dataset.
      /// </summary>
      Max               : Boolean ;

      /// <summary>
      ///   The value that occurs most often in dataset.
      /// </summary>
      Majority          : Boolean ;

      /// <summary>
      ///   The median (middle value) of dataset.
      /// </summary>
      Median            : Boolean ;

      /// <summary>
      ///   The minimum value in dataset.
      /// </summary>
      Min               : Boolean ;

      /// <summary>
      ///   The value that occurs least often in dataset.
      /// </summary>
      Minority          : Boolean ;

      /// <summary>
      ///   The value below which a percentage of data falls.
      ///   Not available for strings.
      /// </summary>
      /// <remarks>
      ///   Percentages can be passed by
      ///   '_params=[p1, p2, ..., pN]', where:
      ///   p (Double) - the requested percentile value in range 0..100, inclusive.
      ///   By default 25th (first quartile), 50th (median)
      ///   and 75th (third quartile) percentiles are calculated.
      /// </remarks>
      Percentile        : Boolean ;

      /// <summary>
      ///   The difference between maximum and minimum values in dataset.
      /// </summary>
      Range             : Boolean ;

      /// <summary>
      ///   Extracting sample values from dataset.
      /// </summary>
      /// <remarks>
      ///   Parameters for this function can be passed by
      ///   '_params=[Size, Replacement]', where:
      ///   Size (Integer) - number of elements to extract; default is 1000.
      ///   Replacement (Boolean) - if True, samples will be taken with replacement;
      ///   default is False.
      /// </remarks>
      Sample            : Boolean ;

      /// <summary>
      ///   The standard deviation of dataset.
      /// </summary>
      StandardDeviation : Boolean ;

      /// <summary>
      ///   The sum of values in dataset.
      /// </summary>
      Sum               : Boolean ;

      /// <summary>
      ///   The variance of dataset.
      /// </summary>
      Variance          : Boolean ;

      /// <summary>
      ///   The number of unique values in dataset.
      /// </summary>
      Variety           : Boolean ;

      /// <summary>
      ///   Extracting unique values from dataset.
      /// </summary>
      /// <remarks>
      ///   Parameters for this function can be passed by
      ///   "_params=[Limit]', where:
      ///   Limit (Integer) - number of elements to extract; default is 65536.
      /// </remarks>
      Unique            : Boolean ;

    public
      /// <summary>
      ///   Prepare a record with active basic statistical functions.
      /// </summary>
      /// <returns>
      ///   predefined record with active basic statistical functions
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Active functions: Average, Count, Max, Min, Sum.
      ///   </note>
      /// </remarks>
      class function BasicStatistics    : TGIS_StatisticalFunctions ; static ;

      /// <summary>
      ///   Prepare a record with active most common statistical functions.
      /// </summary>
      /// <returns>
      ///   predefined record with active most common statistical functions
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Active functions: Average, Count, CountMissings, Max, Median, Min,
      ///     Range, StandardDeviation, Sum, Variance.
      ///   </note>
      /// </remarks>
      class function StandardStatistics : TGIS_StatisticalFunctions ; static ;

      /// <summary>
      ///   Prepare a record with active all available statistical functions.
      /// </summary>
      /// <returns>
      ///   predefined record with active all available statistical functions
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     Active functions: Average, Count, CountMissings, Max, Majority,
      ///     Median, Min, Minority, Percentile (25th, 50th and 75th), Range,
      ///     Sample (Size=256, Replacement=False), StandardDeviation, Sum,
      ///     Variance, Variety, Unique.
      ///   </note>
      /// </remarks>
      class function AllStatistics      : TGIS_StatisticalFunctions ; static ;

      /// <summary>
      ///   Prepare a record with deactivate all statistical functions.
      /// </summary>
      /// <returns>
      ///   record with deactivate all statistical functions
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///     All statistical functions set to False;
      ///     use it to create custom record.
      ///   </note>
      /// </remarks>
      class function EmptyStatistics    : TGIS_StatisticalFunctions ; static ;

    public
      /// <summary>
      ///   Create a record and set all statistical functions.
      /// </summary>
      /// <param name="_calculate">
      ///   boolean value to be set for all statistical functions
      /// </param>
      constructor Create ( const _calculate : Boolean
                         ) ; overload ;

      /// <summary>
      ///   Create a record with active one statistical function.
      /// </summary>
      /// <param name="_function">
      ///   statistical function to be set to True in record.
      /// </param>
      constructor Create ( const _function  : TGIS_StatisticalFunction
                         ) ; overload ;

  end ;

  /// <summary>
  ///  General class for storing single statistical value. Must be derived.
  /// </summary>
  TGIS_StatisticsItem = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                         class ( TGIS_Object )
    private const
      // default params
      /// <summary>
      ///   25-th percentile
      /// </summary>
      PERCENTILE_25      = 25 ;

      /// <summary>
      ///   50-th percentile
      /// </summary>
      PERCENTILE_50      = 50 ;

      /// <summary>
      ///   75-th percentile
      /// </summary>
      PERCENTILE_75      = 75 ;

      /// <summary>
      ///   Default method for Sample statistics
      /// </summary>
      SAMPLE_REPLACEMENT = False ;

      /// <summary>
      ///   Default size for Sample statistics
      /// </summary>
      SAMPLE_SIZE        = 256 ;    // 2^8

    private
      FName                     : String ;
      FParams                   : TGIS_VariantArray ;

    private {$IFDEF OXYGENE} unit {$ENDIF}
      FRequested                : Boolean ;
      FCalculated               : Boolean ;

    private
      statFunction              : TGIS_StatisticalFunction ;

    private
      procedure fset_Params    ( const _value : TGIS_VariantArray ) ;
      function  getFunctionName( const _function : TGIS_StatisticalFunction
                               ) : String ;
      // works only for functions with params
      function  getDefaultParams( const _function : TGIS_StatisticalFunction
                                ) : TGIS_VariantArray ;

      procedure setDefaultParams ;

    private {$IFDEF OXYGENE} unit {$ENDIF}
      function asVariant       : Variant ; virtual ; abstract ;
      function variantToString ( const _variant : Variant
                               ) : String ;

    public
      /// <summary>
      ///   Create statistics item.
      /// </summary>
      /// <param name="_function">
      ///    statistical function stored in item
      /// </param>
      /// <param name="_requested">
      ///   True if statistical value should be calculated; False otherwise
      /// </param>
      /// <remarks>
      ///   <note type="caution">
      ///     Only for internal use of TatukGIS.
      ///   </note>
      /// </remarks>
      constructor Create  ( const _function  : TGIS_StatisticalFunction ;
                            const _requested : Boolean
                          ) ; virtual ;

    public
      /// <summary>
      ///   Prepare string representation of statistical value.
      /// </summary>
      /// <returns>
      ///   string
      /// </returns>
      function ToString   : String ; reintroduce ; virtual ; abstract ;

    public
      /// <summary>
      ///  True if statistical value is calculated; False otherwise.
      /// </summary>
      property Calculated : Boolean
                            read FCalculated ;

      /// <summary>
      ///  Name of statistical value stored in item.
      /// </summary>
      property Name       : String
                            read FName ;

      /// <summary>
      ///  Function parameters used in calculations.
      /// </summary>
      property Params     : TGIS_VariantArray
                            read FParams
                            write fset_Params ;

      /// <summary>
      ///  True if statistical value is requested; False otherwise.
      /// </summary>
      property Requested  : Boolean
                            read FRequested ;

  end ;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  /// Alias for generic list of TGIS_StatisticsItem;
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_StatisticsItemList = {$IFDEF OXYGENE} public {$ENDIF}
                              TList< TGIS_StatisticsItem > ;
  {$ELSE}
    TGIS_StatisticsItemList = class (
                                TList< TGIS_StatisticsItem >
                              ) ;
  {$ENDIF}

  /// <summary>
  ///   Class for storing integer statistical value.
  /// </summary>
  TGIS_StatisticsItemInteger = {$IFDEF OXYGENE} public {$ENDIF}
                                                class ( TGIS_StatisticsItem )
    private {$IFDEF OXYGENE} unit {$ENDIF}
      FValue             : Integer ;

    private {$IFDEF OXYGENE} unit {$ENDIF}
      function asVariant : Variant ; override ;

    public
      /// <summary>
      ///   Integer statistical value.
      /// </summary>
      property Value     : Integer
                           read FValue ;

    public
      /// <inheritdoc/>
      constructor Create ( const _function  : TGIS_StatisticalFunction ;
                           const _requested : Boolean
                         ) ; override ;

    public
      /// <inheritdoc/>
      function ToString  : String ; override ;

  end ;

  /// <summary>
  ///   Class for storing double-precision statistical value.
  /// </summary>
  TGIS_StatisticsItemDouble = {$IFDEF OXYGENE} public {$ENDIF}
                                               class ( TGIS_StatisticsItem )
    private {$IFDEF OXYGENE} unit {$ENDIF}
      FValue             : Double ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function asVariant : Variant ; override ;

    public
      /// <summary>
      ///   Double-precision statistical value.
      /// </summary>
      property Value     : Double
                           read FValue ;

    public
      /// <inheritdoc/>
      constructor Create ( const _function  : TGIS_StatisticalFunction ;
                           const _requested : Boolean
                         ) ; override ;

    public
      /// <inheritdoc/>
      function ToString  : String ; override ;

  end ;

  /// <summary>
  ///   Class for storing variant statistical value.
  /// </summary>
  TGIS_StatisticsItemVariant = {$IFDEF OXYGENE} public {$ENDIF}
                                                class ( TGIS_StatisticsItem )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FValue              : Variant ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function asVariant  : Variant ; override ;

    public
      /// <summary>
      ///   Variant statistical result.
      /// </summary>
      property Value     : Variant
                           read FValue ;

    public
      /// <inheritdoc/>
      constructor Create ( const _function  : TGIS_StatisticalFunction ;
                           const _requested : Boolean
                         ) ; override ;

    public
      /// <inheritdoc/>
      function ToString  : String ; override ;

  end ;

  /// <summary>
  ///   Class for storing statistical result with list of variants.
  /// </summary>
  TGIS_StatisticsItemVariantList = {$IFDEF OXYGENE} public {$ENDIF}
                                               class ( TGIS_StatisticsItem )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FValues             : TGIS_ListOfVariants ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function asVariant  : Variant ; override ;

    protected
      procedure doDestroy ; override ;

    public
      /// <inheritdoc/>
      constructor Create ( const _function  : TGIS_StatisticalFunction ;
                           const _requested : Boolean
                         ) ; override ;

    public
      /// <inheritdoc/>
      function ToString   : String ; override ;

    public
      /// <summary>
      ///   List of variants.
      /// </summary>
      property Values     : TGIS_ListOfVariants
                            read FValues ;

  end ;

  /// <summary>
  ///   Container for statistics results.
  /// </summary>
  TGIS_StatisticsResult = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )

    private
      FUseBesselCorrection  : Boolean ;
      FDatasetType          : TGIS_FieldType ;
      FIgnoreValue          : Variant ;
      FName                 : String ;

      FAverage              : TGIS_StatisticsItemDouble ;
      FCount                : TGIS_StatisticsItemInteger ;
      FCountMissings        : TGIS_StatisticsItemInteger ;
      FMajority             : TGIS_StatisticsItemVariant ;
      FMax                  : TGIS_StatisticsItemDouble ;
      FMedian               : TGIS_StatisticsItemDouble ;
      FMin                  : TGIS_StatisticsItemDouble ;
      FMinority             : TGIS_StatisticsItemVariant ;
      FPercentile           : TGIS_StatisticsItemVariantList ;
      FRange                : TGIS_StatisticsItemDouble ;
      FSample               : TGIS_StatisticsItemVariantList ;
      FStandardDeviation    : TGIS_StatisticsItemDouble ;
      FSum                  : TGIS_StatisticsItemDouble ;
      FVariance             : TGIS_StatisticsItemDouble ;
      FVariety              : TGIS_StatisticsItemInteger ;
      FUnique               : TGIS_StatisticsItemVariantList ;

    private
      // internal information about needed statistics
      needAverage           : Boolean ;
      needTotalVariance     : Boolean ;
      needMin               : Boolean ;
      needMax               : Boolean ;
      needValues            : Boolean ;
      needStrings           : Boolean ;

      // internal statistics
      internalAverage       : Double ;
      internalCount         : Integer ;
      internalCountMissings : Integer ;
      internalMax           : Double ;
      internalMin           : Double ;
      internalSum           : Double ;
      valuesSorted          : Boolean ;
      valuesLst             : TList< Double > ;
      stringsDct            : TDictionary< String, Integer > ;
      resampleRatio         : Double ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      valuesLimit           : Integer ;

      // variables for Welford's method
      deltaOld              : Double ;
      deltaNew              : Double ;
      totalVariance         : Double ;

      // handling ignored value
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      isIgnoreValue         : Boolean ;
    private
      boolIgnoreValue       : Boolean ;
      doubleIgnoreValue     : Double ;
      stringIgnoreValue     : String ;

      // handling raising busy events
      busyEventManager      : TGIS_BusyEventManager ;

    private
      function  fget_AvailableStatistics : TGIS_StatisticsItemList ;
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

      // reset internal statistics
      procedure initializeInternalStatistics ;

      // find needed statistics
      function  checkIfNeedAverage       : Boolean ;
      function  checkIfNeedTotalVariance : Boolean ;
      function  checkIfNeedMin           : Boolean ;
      function  checkIfNeedMax           : Boolean ;
      function  checkIfNeedValues        : Boolean ;
      function  checkIfNeedStrings       : Boolean ;

      // online algorithm
      // add value to engine and update temporary statistics
      procedure updateValues             ( const _value       : Double ) ;

      // add string to engine's dictionary
      procedure updateStrings            ( const _string      : String ) ;

      // processing min & max
      procedure assignMin                ( const _value       : Double ) ;
      procedure assignMax                ( const _value       : Double ) ;

      // calculation methods
      function  calcMedian               : Double ;
      procedure calcPercentiles ;
      function  calcPercentile           ( const _percentage  : Double
                                         ) : Double ;
      function  calcRange                : Double ;
      function  calcStandardDeviation    : Double ;
      function  calcVariance             : Double ;

      /// <summary>
      ///   Generates samples.
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  generateSamples          : Boolean;

      /// <summary>
      ///   Using one-pass algorithm for calculate Majority, Minority, Unique & Variety.
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  processOccurrences       : Boolean ;
      function  processValueOccurrences  ( const _uniqueLimit : Integer
                                         ) : Boolean ;
      function  processStringOccurrences ( const _uniqueLimit : Integer
                                         ) : Boolean ;

      // sort 'valuesLst' list
      procedure sortValues ;

      // increment internal 'count_missings_'
      procedure incrementCountMissings ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // getter & setter
      function  fget_UseBesselCorrection : Boolean ; virtual ;
      procedure fset_UseBesselCorrection ( const _value : Boolean ) ; virtual ;

      /// <summary>
      ///   Finalize and compute all requested statistics.
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  doComplete          : Boolean ;

      // clear 'valuesLst' and 'stringsDct'
      procedure cleanUp ;

      // set params: 'needAverage', 'needMin', 'needMax' and so on
      procedure determineNeededVariables ;

      // for 'Fast Statistics' use factor to approximate Count, CountMissings & Sum
      procedure setResampleRatio    ( const _ratio  : Double ) ;

      // use to add more requested statistical functions
      procedure updateFunctions     ( const _functions : TGIS_StatisticalFunctions ) ;

    protected
      procedure doDestroy           ; override ;

    public
      /// <summary>
      ///   Create an object.
      /// </summary>
      /// <param name="_dataset_type">
      ///   dataset type
      /// </param>
      /// <param name="_name">
      ///   name of statistics result
      /// </param>
      /// <param name="_functions">
      ///   record with defined statistical functions
      /// </param>
      /// <param name="_ignore_value">
      ///   value from dataset which will be ignored in statistics
      /// </param>
      constructor Create ( const _dataset_type      : TGIS_FieldType ;
                           const _name              : String ;
                           const _functions         : TGIS_StatisticalFunctions ;
                           const _ignore_value      : Variant
                         ) ; overload ; virtual ;

    public
      /// <summary>
      ///   Adds value to dataset from which statistics will be computed.
      /// </summary>
      /// <param name="_value">
      ///   value that will be added
      /// </param>
      procedure Update   ( const _value : Variant ) ;

      /// <summary>
      ///   Finalize and compute all requested statistics.
      /// </summary>
      procedure Complete ;

    public
      /// <summary>
      ///   List of calculated statistics (only requested).
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Remember to free this list.
      ///   </note>
      /// </remarks>
      property AvailableStatistics   : TGIS_StatisticsItemList
                                       read fget_AvailableStatistics ;

      /// <summary>
      ///   Dataset type; read-only.
      /// </summary>
      property DatasetType           : TGIS_FieldType
                                       read FDatasetType ;

      /// <summary>
      ///   Value from dataset which is ignored in statistics; read-only.
      /// </summary>
      property IgnoreValue           : Variant
                                       read FIgnoreValue ;

      /// <summary>
      ///   Name of statistics result; read-only.
      /// </summary>
      property Name                  : String
                                       read FName ;

      /// <summary>
      ///   True if statistics class uses Bessel's correction.
      ///   If layer is assigned to statistics result, property is read-only.
      /// </summary>
      /// <remarks>
      ///   Read more at TGIS_StatisticsAbstract.UseBesselCorrection.
      /// </remarks>
      property UseBesselCorrection   : Boolean
                                       read  fget_UseBesselCorrection
                                       write fset_UseBesselCorrection ;

      /// <summary>
      ///   The average value (arithmetic mean) of dataset.
      /// </summary>
      property Average               : TGIS_StatisticsItemDouble
                                       read FAverage ;

      /// <summary>
      ///   Number of valid elements in dataset (not empty, not null).
      /// </summary>
      property Count                 : TGIS_StatisticsItemInteger
                                       read FCount ;

      /// <summary>
      ///   Number of empty or null elements in dataset.
      /// </summary>
      property CountMissings         : TGIS_StatisticsItemInteger
                                       read FCountMissings ;

      /// <summary>
      ///   The maximum value in dataset.
      /// </summary>
      property Max                   : TGIS_StatisticsItemDouble
                                       read FMax ;

      /// <summary>
      ///   The value that occurs most often in dataset.
      /// </summary>
      property Majority              : TGIS_StatisticsItemVariant
                                       read FMajority ;

      /// <summary>
      ///   The median (middle value) of dataset.
      /// </summary>
      property Median                : TGIS_StatisticsItemDouble
                                       read FMedian ;

      /// <summary>
      ///   The minimum value in dataset.
      /// </summary>
      property Min                   : TGIS_StatisticsItemDouble
                                       read FMin ;

      /// <summary>
      ///   The value that occurs least often in dataset.
      /// </summary>
      property Minority              : TGIS_StatisticsItemVariant
                                       read FMinority ;

      /// <summary>
      ///   Percentiles calculated for requested percentages.
      /// </summary>
      property Percentile            : TGIS_StatisticsItemVariantList
                                       read FPercentile ;

      /// <summary>
      ///   The difference between maximum and minimum values in dataset.
      /// </summary>
      property Range                 : TGIS_StatisticsItemDouble
                                       read FRange ;

      /// <summary>
      ///   Sample values from dataset.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     Default sample size is 256.
      ///   </note>
      /// </remarks>
      property Sample                : TGIS_StatisticsItemVariantList
                                       read FSample ;

      /// <summary>
      ///   The standard deviation of dataset.
      /// </summary>
      property StandardDeviation     : TGIS_StatisticsItemDouble
                                       read FStandardDeviation ;

      /// <summary>
      ///   The sum of values in dataset.
      /// </summary>
      property Sum                   : TGIS_StatisticsItemDouble
                                       read FSum ;

      /// <summary>
      ///   The variance of dataset.
      /// </summary>
      property Variance              : TGIS_StatisticsItemDouble
                                       read FVariance ;

      /// <summary>
      ///   The number of unique values in dataset.
      /// </summary>
      property Variety               : TGIS_StatisticsItemInteger
                                       read FVariety ;

      /// <summary>
      ///   Unique values from dataset.
      /// </summary>
      /// <remarks>
      ///   <note type="caution">
      ///     Number of unique elements is limited to 65 536.
      ///   </note>
      /// </remarks>
      property Unique                : TGIS_StatisticsItemVariantList
                                       read FUnique ;

    published // events
      /// <summary>
      ///   Event fired upon progress of statistics calculations.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent              : TGIS_BusyEvent
                                       add fadd_BusyEvent
                                       remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent           : TGIS_BusyEvent
                                       read  fget_BusyEvent
                                       write fset_BusyEvent ;
      {$ENDIF}

  end ;

  /// <summary>
  ///   Container for layer's statistics result.
  /// </summary>
  TGIS_StatisticsLayerResult = {$IFDEF OXYGENE} public {$ENDIF}
                                                class( TGIS_StatisticsResult )
    private
      FLayer : TGIS_LayerAbstract ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      // getter & setter
      function  fget_UseBesselCorrection : Boolean ; override ;
      procedure fset_UseBesselCorrection ( const _value : Boolean ) ; override ;

    public
      /// <summary>
      ///   Create an object.
      /// </summary>
      /// <param name="_layer">
      ///   layer assigned to statistics results
      /// </param>
      /// <param name="_dataset_type">
      ///   dataset type
      /// </param>
      /// <param name="_name">
      ///   name of statistics result
      /// </param>
      /// <param name="_functions">
      ///   record with defined statistical functions
      /// </param>
      /// <param name="_ignore_value">
      ///   value from dataset which will be ignored in statistics;
      ///   use Null to omit
      /// </param>
      constructor Create ( const _layer        : TGIS_LayerAbstract ;
                           const _dataset_type : TGIS_FieldType ;
                           const _name         : String ;
                           const _functions    : TGIS_StatisticalFunctions ;
                           const _ignore_value : Variant
                         ) ; overload ;
    public
      /// <summary>
      ///   Layer assigned to statistics result; read-only.
      /// </summary>
      property Layer     : TGIS_LayerAbstract
                           read FLayer ;

  end ;

  /// <summary>
  ///   General statistics class. Must be derived.
  /// </summary>
  TGIS_StatisticsAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                             class( TGIS_BaseObjectDisposable )

    private
      FAvailableResults    : TGIS_ListOfStrings ;
      FDefinedResults      : TGIS_ListOfStrings ;
      FUseBesselCorrection : Boolean ;

    private
      busyEventManager     : TGIS_BusyEventManager ;

    {$IFNDEF OXYGENE} protected {$ELSE} unit {$ENDIF}
      {#gendoc:hide}
      FLayer               : TGIS_LayerAbstract ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FAge                 : {$IFDEF CLR} DateTime ; {$ELSE} TDateTime ; {$ENDIF}
      FFastStatistics      : Boolean ;

      paramExtent          : TGIS_Extent ;
      paramShape           : TGIS_ShapeAbstract ;
      paramDe9im           : String ;
      useInternalParams    : Boolean ;
      FOnBusy              : TGIS_BusyEvent ;

      // dictionary which store name and statistics result pair.
      resultsDct           : TDictionary< String, TGIS_StatisticsLayerResult > ;

    private
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Prepares and raises the BusyEvent during completing results in
        ///   instances of TGIS_StatisticsLayerResult.
        /// </summary>
        /// <param name="_sender">
        ///   event originator
        /// </param>
        /// <param name="_args">
        ///   An instance of TGIS_BusyEventArgs
        ///   that provides data for the busy event.
        /// </param>
        procedure completingBusyEvent ( _sender : TObject ;
                                        _args   : TGIS_BusyEventArgs
                                      ) ;
      {$ELSE}
        /// <summary>
        ///   Prepares and raises the BusyEvent during completing results in
        ///   instances of TGIS_StatisticsLayerResult.
        /// </summary>
        /// <param name="_sender">
        ///   event originator
        /// </param>
        /// <param name="_pos">
        ///   current position; -1 at the end of the run
        /// </param>
        /// <param name="_end">
        ///   max position; -1 at the end of the run
        /// </param>
        /// <returns>
        ///   if set to True inside message handler then an abort request
        /// </returns>
        procedure completingBusyEvent(     _sender : TObject ;
                                           _pos    : Integer ;
                                           _end    : Integer ;
                                       var _abort  : Boolean
                                     ) ;
      {$ENDIF}

      procedure completingBusyEventEx(     _sender : TObject ;
                                           _pos    : Integer ;
                                           _end    : Integer ;
                                       var _abort  : Boolean
                                     ) ;

    private
      /// <summary>
      ///   Executes Complete method on all statistics definitions
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  completeResults        : Boolean ;

      // Obsolete property getter
      function  fget_Obsolete          : Boolean ;

      // Modified property getter
      function  fget_Modified          : Boolean ;

      // free individual results stored in dictionary
      procedure freeStatisticResults ;

     // set valuesLimit for all statistics definitions
      procedure setValuesLimit ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      /// <summary>
      ///   Collects values from dataset
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  collectValuesFromDataset  : Boolean ; virtual ; abstract ;

      // get default ignored value specific for layer type
      function  getDefaultIgnoreValue     : Variant ; virtual ; abstract ;

      // get layer specific values limnit for 'Fast Statistics' mode
      function  getValuesLimit            : Integer ; virtual ; abstract ;

      // determine dataset type and throw exception if failed
      function  determineDatasetType      ( const _name  : String
                                          ) : TGIS_FieldType ; virtual ; abstract ;

      // try to determine dataset type without exception
      function  determineDatasetTypeNoExc ( const _name  : String ;
                                            var   _type  : TGIS_FieldType
                                          ) : Boolean ; virtual ; abstract ;

      procedure makeDefinedResultsAvailable ;

      // remove existing statistics result, indicated by '_name'
      procedure remove                    ( const _name  : String ) ;

      procedure setResampleRatio          ( const _ratio : Double ) ;

    protected
      procedure doDestroy                 ; override ;

      /// <summary>
      ///   "private constructor"
      /// </summary>
      /// <param name="_layer">
      ///   layer to be assigned to statistics object
      /// </param>
      procedure doCreate                  ( const _layer : TGIS_LayerAbstract
                                          ) ;

    public
      /// <summary>
      ///   <para>
      ///     Adds statistics result definition.
      ///   </para>
      ///   <para>
      ///     If definition exists, requested functions will be updated.
      ///   </para>
      /// </summary>
      /// <param name="_name">
      ///   name of statistics result;
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     Statistical functions from
      ///     TGIS_StatisticsFunctions.StandardStatistics will be calculated.
      ///   </note>
      ///   Valid strings which can be set for _name are listed below.
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Layer type</term>
      ///       <description>Valid strings</description>
      ///     </listheader>
      ///     <item>
      ///       <term>vector</term>
      ///       <description>existing fields names</description>
      ///     </item>
      ///     <item>
      ///       <term>grid</term>
      ///       <description>'Value'</description>
      ///     </item>
      ///     <item>
      ///       <term>image</term>
      ///       <description>'A', 'R', 'G', 'B', 'H', 'S', 'L'</description>
      ///     </item>
      ///     <item>
      ///       <term>grid or pixel</term>
      ///       <description>
      ///         band number starting from 0: '0', '1', '2', '3', etc.,
      ///         where 0 is the default band
      ///       </description>
      ///     </item>
      ///   </list>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDNOEXIST
      ///   GIS_RS_ERR_BADPIXELBAND
      /// </exception>
      procedure Add                ( const _name         : String
                                   ) ; overload ;

      /// <summary>
      ///   <para>
      ///     Adds statistics result definition.
      ///   </para>
      ///   <para>
      ///     If definition exists, requested functions will be updated.
      ///   </para>
      /// </summary>
      /// <param name="_name">
      ///   name of statistics result;
      /// </param>
      /// <param name="_functions">
      ///   record with requested statistical functions
      /// </param>
      /// <remarks>
      ///   Valid strings which can be set for _name are listed below.
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Layer type</term>
      ///       <description>Valid strings</description>
      ///     </listheader>
      ///     <item>
      ///       <term>vector</term>
      ///       <description>existing fields names</description>
      ///     </item>
      ///     <item>
      ///       <term>grid</term>
      ///       <description>'Value'</description>
      ///     </item>
      ///     <item>
      ///       <term>image</term>
      ///       <description>'A', 'R', 'G', 'B', 'H', 'S', 'L'</description>
      ///     </item>
      ///     <item>
      ///       <term>grid or pixel</term>
      ///       <description>
      ///         band number starting from 0: '0', '1', '2', '3', etc.,
      ///         where 0 is the default band
      ///       </description>
      ///     </item>
      ///   </list>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDNOEXIST
      ///   GIS_RS_ERR_BADPIXELBAND
      /// </exception>
      procedure Add                ( const _name         : String ;
                                     const _functions    : TGIS_StatisticalFunctions
                                   ) ; overload ;

      /// <summary>
      ///   <para>
      ///     Adds statistics result definition.
      ///   </para>
      ///   <para>
      ///     If definition exists, requested functions will be updated.
      ///   </para>
      /// </summary>
      /// <param name="_name">
      ///   name of statistics result;
      /// </param>
      /// <param name="_functions">
      ///   record with requested statistical functions
      /// </param>
      /// <param name="_ignore_value">
      ///   value from dataset which will be ignored in statistics;
      ///   use Null to omit
      /// </param>
      /// <remarks>
      ///   Valid strings which can be set for _name are listed below.
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Layer type</term>
      ///       <description>Valid strings</description>
      ///     </listheader>
      ///     <item>
      ///       <term>vector</term>
      ///       <description>existing fields names</description>
      ///     </item>
      ///     <item>
      ///       <term>grid</term>
      ///       <description>'Value'</description>
      ///     </item>
      ///     <item>
      ///       <term>image</term>
      ///       <description>'A', 'R', 'G', 'B', 'H', 'S', 'L'</description>
      ///     </item>
      ///     <item>
      ///       <term>grid or pixel</term>
      ///       <description>
      ///         band number starting from 0: '0', '1', '2', '3', etc.,
      ///         where 0 is the default band
      ///       </description>
      ///     </item>
      ///   </list>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDNOEXIST
      ///   GIS_RS_ERR_BADPIXELBAND
      /// </exception>
      procedure Add                ( const _name         : String ;
                                     const _functions    : TGIS_StatisticalFunctions ;
                                     const _ignore_value : Variant
                                   ) ; overload ;

      /// <summary>
      ///   <para>
      ///     Adds statistics result definition.
      ///   </para>
      ///   <para>
      ///     If definition exists, requested functions will be updated.
      ///   </para>
      /// </summary>
      /// <param name="_name">
      ///   name of statistics result;
      /// </param>
      /// <param name="_function">
      ///   requested statistical function
      /// </param>
      /// <param name="_params">
      ///   optional parameters; if empty, assign default parameters;
      ///   check the documentation of a specific statistical function
      /// </param>
      /// <param name="_ignore_value">
      ///   value from dataset which will be ignored in statistics;
      ///   use Null to omit
      /// </param>
      /// <remarks>
      ///   Valid strings which can be set for _name are listed below.
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Layer type</term>
      ///       <description>Valid strings</description>
      ///     </listheader>
      ///     <item>
      ///       <term>vector</term>
      ///       <description>existing fields names</description>
      ///     </item>
      ///     <item>
      ///       <term>grid</term>
      ///       <description>'Value'</description>
      ///     </item>
      ///     <item>
      ///       <term>image</term>
      ///       <description>'A', 'R', 'G', 'B', 'H', 'S', 'L'</description>
      ///     </item>
      ///     <item>
      ///       <term>grid or pixel</term>
      ///       <description>
      ///         band number starting from 0: '0', '1', '2', '3', etc.,
      ///         where 0 is the default band
      ///       </description>
      ///     </item>
      ///   </list>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FIELDNOEXIST
      ///   GIS_RS_ERR_BADPIXELBAND
      /// </exception>
      procedure Add                ( const _name         : String ;
                                     const _function     : TGIS_StatisticalFunction ;
                                     const _params       : TGIS_VariantArray ;
                                     const _ignore_value : Variant
                                   ) ; overload ;

      /// <summary>
      ///   Adds statistics result definitions for all available datasets.
      ///   Resets previous definitions.
      /// </summary>
      /// <remarks>
      ///   Adds all bands names for pixel layer.
      ///   Adds all non-virtual fields names for vector layers.
      ///   <note type="note">
      ///     Use Add method to calclulte statistics for virtual fields, such as
      ///     'GIS_UID', 'GIS_AREA', 'GIS_LENGTH', etc.
      ///   </note>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADFORMAT
      /// </exception>
      procedure AddAll             ; overload ; virtual ; abstract ;

      /// <summary>
      ///   Adds statistics result definitions for all available datasets.
      ///   Resets previous definitions.
      /// </summary>
      /// <param name="_functions">
      ///   record with defined statistical functions
      /// </param>
      /// <remarks>
      ///   Adds all bands names for pixel layer.
      ///   Adds all non-virtual fields names for vector layers.
      ///   <note type="note">
      ///     Use Add method to calclulte statistics for virtual fields, such as
      ///     'GIS_UID', 'GIS_AREA', 'GIS_LENGTH', etc.
      ///   </note>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADFORMAT
      /// </exception>
      procedure AddAll             ( const _functions    : TGIS_StatisticalFunctions
                                   ) ; overload ; virtual ; abstract ;

      /// <summary>
      ///   Calculate requested statistics from the entire layer.
      /// </summary>
      procedure Calculate          ; overload ; virtual ;

      /// <summary>
      ///   Calculate requested statistics from filtered elements of the layer.
      /// </summary>
      /// <param name="_extent">
      ///   extent on which statistics will be calculated
      /// </param>
      /// <param name="_shape">
      ///  if not nil, statistics will be calculated on area which matches
      ///   _de9im matrix with _shape
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_fast_statistics">
      ///    if True, 'Fast Statistics' will be computed
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     'Fast Statistics' means that statistics class uses resampling for
      ///     pixel layers greater than 2048 x 2048 cells .
      ///     In this case results are approximate with high accuracy.
      ///     Set False to compute fully accurate statistics.
      ///     'Fast Statistics' has no effects for vector layers.
      ///   </note>
      /// </remarks>
      procedure Calculate          ( const _extent          : TGIS_Extent ;
                                     const _shape           : TGIS_ShapeAbstract ;
                                     const _de9im           : String ;
                                     const _fast_statistics : Boolean
                                   ) ; overload ; virtual ;

      /// <summary>
      ///   Get statistics result by name.
      /// </summary>
      /// <param name="_name">
      ///   name of statistics result
      /// </param>
      /// <returns>
      ///   container with statistics result or nil if result does not exist
      /// </returns>
      /// <remarks>
      ///   Valid strings which can be set for _name are listed below.
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Layer type</term>
      ///       <description>Valid strings</description>
      ///     </listheader>
      ///     <item>
      ///       <term>vector</term>
      ///       <description>existing fields names</description>
      ///     </item>
      ///     <item>
      ///       <term>grid</term>
      ///       <description>'Value'</description>
      ///     </item>
      ///     <item>
      ///       <term>image</term>
      ///       <description>'A', 'R', 'G', 'B', 'H', 'S', 'L'</description>
      ///     </item>
      ///     <item>
      ///       <term>grid or pixel</term>
      ///       <description>
      ///         band number starting from 0: '0', '1', '2', '3', etc.,
      ///         where 0 is the default band
      ///       </description>
      ///     </item>
      ///   </list>
      /// </remarks>
      function  Get                ( const _name         : String
                                   ) : TGIS_StatisticsLayerResult ;

      /// <summary>
      ///   Parse statistics file and load all information into parent layer.
      /// </summary>
      /// <returns>
      ///   True if statisticsfile was properly interpreted.
      /// </returns>
      /// <remarks>
      ///   Method will try to load file with the same path as parent layer
      ///   with '.ttkstats' extension.
      /// </remarks>
      function LoadFromFile        : Boolean ; overload ; virtual ;

      /// <summary>
      ///   Parse statistics file and load all information into parent layer.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      /// <returns>
      ///   True if file with '.ttkstats' was properly interpreted.
      /// </returns>
      function LoadFromFile        ( const _path : String
                                   ) : Boolean ; overload ; virtual ;

      /// <summary>
      ///   Resets statistics object to initial state.
      /// </summary>
      /// <remarks>
      ///   Resets all statistics definitions and results.
      /// </remarks>
      procedure Reset ;

      /// <summary>
      ///   <para>
      ///     Undo changes in requested statistics definitions.
      ///   </para>
      ///   <para>
      ///     All requested but not calculated statistics definitions will be erased.
      ///   </para>
      /// </summary>
      procedure ResetModified ;

      /// <summary>
      ///   Save layer statistics into auxiliary file.
      /// </summary>
      /// <remarks>
      ///   File will be saved in the same folder as parent layer
      ///   and with the same name with '.ttkstats' extension.
      /// </remarks>
      procedure SaveToFile         ; overload ;

      /// <summary>
      ///   Save layer statistics into auxiliary file.
      /// </summary>
      /// <param name="_path">
      ///   path to '.ttkstats' file
      /// </param>
      procedure SaveToFile         ( const _path : String
                                   ) ; overload ;

    public
      /// <summary>
      ///   Age of the statistics.
      /// </summary>
      property Age                 : {$IFDEF CLR} DateTime {$ELSE} TDateTime {$ENDIF}
                                     read FAge ;
      /// <summary>
      ///   List of available results names; read-only.
      /// </summary>
      property AvailableResults    : TGIS_ListOfStrings
                                     read  FAvailableResults ;

      /// <summary>
      ///   List of already defined results names; read-only.
      /// </summary>
      property DefinedResults      : TGIS_ListOfStrings
                                     read FDefinedResults ;

      /// <summary>
      ///   Determines the state if 'Fast Statistics' was computed; read-only.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///     'Fast Statistics' means that statistics class uses resampling for
      ///     pixel layers greater than 2048 x 2048 cells and search limit for
      ///     vector layers with more than 10 000 shapes. In this case, results
      ///     are approximate with high accuracy. This is default.
      ///     Use overloaded 'Calculate' method and set parameter '_fast_statistics'
      ///     to False in order to compute fully accurate statistics.
      ///   </note>
      /// </remarks>
      property FastStatistics      : Boolean
                                     read FFastStatistics ;

      /// <summary>
      ///   If True, statistics class will use Bessel's correction.
      /// </summary>
      /// <remarks>
      ///   True means that sample variance and standard deviation is calculated
      ///   (divider equals 'N-1'); False means that population variance and
      ///   standard deviation is calculated. (divider equals 'N').
      /// </remarks>
      property UseBesselCorrection : Boolean
                                     read  FUseBesselCorrection
                                     write FUseBesselCorrection ;

      /// <summary>
      ///   True if requested statistics have changed.
      /// </summary>
      /// <remarks>
      ///   Modified statistics are not recalculated automatically.
      ///   It is the user responsibility to recalculate it.
      /// </remarks>
      property Modified            : Boolean
                                     read fget_Modified ;

      /// <summary>
      ///   Check validity of statistics.
      /// </summary>
      /// <returns>
      ///   True if statistics are out-of-date; False otherwise.
      /// </returns>
      /// <remarks>
      ///   Outdated statistics are not recalculated automatically.
      ///   It is the user responsibility to recalculate it.
      /// </remarks>
      property Obsolete            : Boolean
                                     read fget_Obsolete ;

  end ;

  /// <summary>
  ///   Statistics class for vector layers.
  /// </summary>
  TGIS_StatisticsLayer = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                          class( TGIS_StatisticsAbstract )
    private
      {$IFDEF CLR}
        procedure fadd_BusyEvent    ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent    : TGIS_BusyEvent ;
        procedure fset_BusyEvent    ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

    public
      /// <summary>
      ///   Create an object. Always related to layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be assigned to statistics object
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADFORMAT
      ///   GIS_RS_ERR_BADPARAM
      /// </exception>
      constructor Create            ( const _layer : TGIS_LayerAbstract
                                    ) ; virtual ;

    public
      /// <summary>
      ///   Layer assigned to statistics; read-only.
      /// </summary>
      property Layer                : TGIS_LayerAbstract
                                      read  FLayer ;

    published //events
      /// <summary>
      ///   The event that is fired during the computation of statistics.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent             : TGIS_BusyEvent
                                      add    fadd_BusyEvent
                                      remove fremove_BusyEvent ;
      {$ELSE}
        /// <event/>
        property BusyEvent          : TGIS_BusyEvent
                                      read  fget_BusyEvent
                                      write fset_BusyEvent ;
      {$ENDIF}
  end;

  /// <summary>
  ///   Statistics class for vector layers.
  /// </summary>
  TGIS_StatisticsVector = {$IFDEF OXYGENE} public {$ENDIF}
                                           class( TGIS_StatisticsLayer )
    private const
      USE_SELECTED                 = False ;
      FAST_STATISTICS_VECTOR_LIMIT = 10000 ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      paramQuery       : String ;
      paramUseSelected : Boolean ;

      /// <summary>
      ///   Filters shapes which match requested criteria.
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  collectValuesFromDataset  : Boolean ; override ;

      // default ignored value for vector layers is Null
      function  getDefaultIgnoreValue     : Variant ; override ;

      // default values limit for vector is 10 000
      function  getValuesLimit            : Integer ; override ;

      // determine dataset type and throw exception if failed
      function  determineDatasetType      ( const _name   : String
                                          ) : TGIS_FieldType ; override ;

      // try to determine dataset type without exception
      function  determineDatasetTypeNoExc ( const _name : String ;
                                            var   _type : TGIS_FieldType
                                          ) : Boolean ; override ;

    public
      /// <inheritdoc/>
      constructor Create      ( const _layer : TGIS_LayerAbstract
                              ) ; override ;

    public
      /// <inheritdoc/>
      procedure AddAll        ; override ;

      /// <inheritdoc/>
      procedure AddAll        ( const _functions    : TGIS_StatisticalFunctions
                              ) ; override ;

      /// <summary>
      ///   Adds statistics result definitions for all non-virtual fields
      ///   from vector layer.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Statistical functions active in
      ///     TGIS_StatisticsFunctions.StandardStatistics will be calculated.
      ///   </para>
      ///   <para>
      ///     Default ignored value for vector layers is Null.
      ///   </para>
      ///   <note type="note">
      ///     Use Add method to calclulte statistics for virtual fields, such as
      ///     'GIS_UID', 'GIS_AREA', 'GIS_LENGTH', etc.
      ///   </note>
      /// </remarks>
      procedure AddAllFields  ; overload ;

      /// <summary>
      ///   Adds statistics result definitions for all non-virtual fields
      ///   from vector layer.
      /// </summary>
      /// <param name="_functions">
      ///   record with defined statistical functions
      /// </param>
      /// <remarks>
      ///   Default ignored value for vector layers is Null.
      ///   <note type="note">
      ///     Use Add method to calclulte statistics for virtual fields, such as
      ///     'GIS_UID', 'GIS_AREA', 'GIS_LENGTH', etc.
      ///   </note>
      /// </remarks>
      procedure AddAllFields  ( const _functions : TGIS_StatisticalFunctions
                              ) ; overload ;

      /// <summary>
      ///   Calculate requested statistics from filtered elements of the layer.
      /// </summary>
      /// <param name="_extent">
      ///   extent on which statistics will be calculated
      /// </param>
      /// <param name="_shape">
      ///  if not nil, statistics will be calculated on area which matches
      ///   _de9im matrix with _shape
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_fast_statistics">
      ///    if True, 'Fast Statistics' will be computed
      /// </param>
      /// <param name="_query">
      ///    query to filter shapes from vector layer to be included in statistics
      /// </param>
      /// <param name="_use_selected">
      ///   if True, only selected shapes will be included in statistics
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///     'Fast Statistics' means that statistics class uses resampling for
      ///     pixel layers greater than 2048 x 2048 cells .
      ///     In this case results are approximate with high accuracy.
      ///     Set False to compute fully accurate statistics.
      ///     'Fast Statistics' has no effects for vector layers.
      ///   </note>
      /// </remarks>
      procedure Calculate          ( const _extent          : TGIS_Extent ;
                                     const _shape           : TGIS_ShapeAbstract ;
                                     const _de9im           : String ;
                                     const _fast_statistics : Boolean ;
                                     const _query           : String ;
                                     const _use_selected    : Boolean
                                   ) ; overload ;  virtual ;

  end ;

  /// <summary>
  ///   Statistics class for raster layers.
  /// </summary>
  TGIS_StatisticsPixel = {$IFDEF OXYGENE} public {$ENDIF}
                                          class( TGIS_StatisticsLayer )

    private const
      // max width/height of processed pixel layer
      MAX_PIXEL_DIMENSION         = 2048 ;

      // max number of elements in 'valuesLst' list in 'Fast Statistics' mode
      FAST_STATISTICS_PIXEL_LIMIT = MAX_PIXEL_DIMENSION * MAX_PIXEL_DIMENSION ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      /// <summary>
      ///   Filters pixels which match requested criteria.
      /// </summary>
      /// <returns>
      ///   True if the process has been aborted
      /// </returns>
      function  collectValuesFromDataset  : Boolean ; override ;

      // default ignored value for raster layers is NoDataValue
      function  getDefaultIgnoreValue     : Variant ; override ;

      // default values limit for pixel is 2048^2
      function  getValuesLimit            : Integer ; override ;

      // determine dataset type and throw exception if failed
      function  determineDatasetType      ( const _name   : String
                                          ) : TGIS_FieldType ; override ;

      // try to determine dataset type without exception
      function  determineDatasetTypeNoExc ( const _name : String ;
                                            var   _type : TGIS_FieldType
                                          ) : Boolean ; override ;

    public
      /// <inheritdoc/>
      constructor Create      ( const _layer : TGIS_LayerAbstract
                              ) ; override ;

    public
      /// <inheritdoc/>
      procedure AddAll        ; override ;

      /// <inheritdoc/>
      procedure AddAll        ( const _functions    : TGIS_StatisticalFunctions
                              ) ; override ;
      /// <summary>
      ///   <para>
      ///     Adds statistics result definitions for all bands from pixel layer;
      ///   </para>
      ///   <para>
      ///     Generate definition named 'Value' for grid layer.
      ///     Generate definitions named 'A', 'R', 'G' and 'B' for pixel layer.
      ///   </para>
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     Statistical functions active in
      ///     TGIS_StatisticsFunctions.StandardStatistics will be calculated.
      ///   </para>
      ///   <para>
      ///     Default ignored value is raster's NoDataValue.
      ///   </para>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADFORMAT
      /// </exception>
      procedure AddAllBands   ; overload ;

      /// <summary>
      ///   <para>
      ///     Adds statistics result definitions for all bands from pixel layer;
      ///   </para>
      ///   <para>
      ///     Generate definition named 'Value' for grid layer.
      ///     Generate definitions named 'A', 'R', 'G' and 'B' for pixel layer.
      ///   </para>
      /// </summary>
      /// <param name="_functions">
      ///   record with defined statistical functions
      /// </param>
      /// <remarks>
      ///   <para>
      ///     Default ignored value is raster's NoDataValue.
      ///   </para>
      /// </remarks>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERBADFORMAT
      /// </exception>
      procedure AddAllBands   ( const _functions : TGIS_StatisticalFunctions
                              ) ; overload ;

      /// <inheritdoc/>
      /// <remarks>
      ///   GDAL's files with extension '.aux.xml' are also possible.
      /// </remarks>
      function LoadFromFile        : Boolean ; override ;

      /// <inheritdoc/>
      /// <remarks>
      ///   GDAL's files with extension '.aux.xml' are also possible.
      /// </remarks>
      function LoadFromFile        ( const _path : String
                                   ) : Boolean ; override ;

  end ;

  /// <summary>
  ///   Class for creating statistics objects depending on a layer type.
  /// </summary>
  TGIS_StatisticsFactory = {$IFDEF OXYGENE} public static {$ENDIF} class
    public
      /// <summary>
      ///   Create a TGIS_StatisticsAbstract object based on a layer type.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be assigned to statistics object
      /// </param>
      /// <returns>
      ///   new instance of statistics class or nil
      /// </returns>
      class function CreateStatistics ( const _layer : TGIS_LayerAbstract
                                      ) : TGIS_StatisticsAbstract ; static ;

  end ;

//##############################################################################

implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.SysUtils,
    System.StrUtils,
    System.Types,

    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoLayerSql,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoXmlDoc;
{$ENDIF}

const
  // separator used in ToString method
  VALUE_SEPARATOR       = '|' ;

  // the limit for unique elements (2^16)
  UNIQUE_STAT_LIMIT     = 65536 ;

  // maximum number of elements to process (2^24)
  MAX_VALUES_LIMIT      = 16777216 ;

  // 'population' standard deviation and variance by default; 'sample' otherwise
  USE_BESSEL_CORRECTION = False ;

  // fast statistics calculated by default
  FAST_STATISTICS       = True ;

{$REGION 'T_FUNCTIONS'}
type
  T_FUNCTIONS = class
    const
      AVERAGE            = 'Average' ;
      COUNT              = 'Count' ;
      COUNT_MISSINGS     = 'CountMissings' ;
      MAJORITY           = 'Majority' ;
      MAX                = 'Max' ;
      MEDIAN             = 'Median' ;
      MIN                = 'Min' ;
      MINORITY           = 'Minority' ;
      PERCENTILE         = 'Percentile' ;
      RANGE              = 'Range' ;
      SAMPLE             = 'Sample' ;
      STANDARD_DEVIATION = 'StandardDeviation' ;
      SUM                = 'Sum' ;
      VARIANCE           = 'Variance' ;
      VARIETY            = 'Variety' ;
      UNIQUE             = 'Unique' ;
  end;
{$ENDREGION}

{$REGION 'T_Sampling'}
type
  T_RandomSamplingIterator = class
    private
      FCurrentItem : Integer ;
      FReplacement : Boolean ;
      u            : Double ;
      m, t         : Integer ;
      n_sample     : Integer ;
      N_population : Integer ;

    public
      constructor Create   ( const _sampleSize     : Integer ;
                             const _populationSize : Integer ;
                             const _replacement    : Boolean
                           ) ;
      procedure Reset ;
      function Next        : Boolean ;
      property CurrentItem : Integer read FCurrentItem ;
      property Replacement : Boolean read FReplacement ;
  end;

constructor T_RandomSamplingIterator.Create(
  const _sampleSize     : Integer ;
  const _populationSize : Integer ;
  const _replacement    : Boolean
) ;
begin
  N_population := _populationSize ;

  // sample size can not be larger than population size
  n_sample := Min( _sampleSize, N_population ) ;

  FReplacement := _replacement ;

  Reset ;
end;

procedure T_RandomSamplingIterator.Reset ;
begin
  // initialize internal fields
  m := 0 ;  // number of records selected so far
  t := 0 ;  // total number of input records that we have deal with
end;

function T_RandomSamplingIterator.Next : Boolean ;
begin
  Result := ( m < n_sample ) ;

  while Result do begin
    // *** random sampling with replacement ***
    if Replacement then begin
      FCurrentItem := GetRandom( N_population ) ;
      inc( m ) ;
      break ;
    end
    // *** random sampling without replacement ***
    // Knuth D. E. (1969) Art of Computer Programming,
    // Volume 2, Seminumerical Algorithms, Section 3.4.2, Algorithm S
    else begin
      u := GetRandom ;
      if ( ( N_population - t ) * u ) >= ( n_sample - m ) then begin
        inc( t ) ;
        continue ;
      end
      else begin
        FCurrentItem := t ;
        inc( m ) ;
        inc( t ) ;
        break ;
      end ;
    end ;
  end ;
end ;
{$ENDREGION}

{$REGION 'T_StatisticsIO'}
  type
  /// <summary>
  ///   Contains methods for saving and loading statistics.
  /// </summary>
  T_StatisticsIO = class
    const
      FIELD_TYPE_NUMBER               = 'Number' ;
      FIELD_TYPE_FLOAT                = 'Float' ;
      FIELD_TYPE_DATE                 = 'Date' ;
      FIELD_TYPE_STRING               = 'String' ;
      FIELD_TYPE_BOOLEAN              = 'Boolean' ;

      NODE_TATUKGIS                   = 'LicadGIS' ; // ilker degistirme
      NODE_STATISTICS                 = 'Statistics' ;
      NODE_STATISTICS_RESULT          = 'StatisticsResult' ;
      NODE_STI                        = 'STI' ;  // Statistic Item

      ATTRIBUTE_KEY                   = 'key' ;
      ATTRIBUTE_NAME                  = 'name' ;
      ATTRIBUTE_DATE                  = 'date' ;
      ATTRIBUTE_PARAMS                = 'params' ;
      ATTRIBUTE_DATASET_TYPE          = 'dataset_type' ;
      ATTRIBUTE_IGNORE_VALUE          = 'ignore_value' ;
      ATTRIBUTE_USE_BESSEL_CORRECTION = 'use_bessel_correction' ;
      ATTRIBUTE_FAST_STATISTICS       = 'fast_statistics' ;

      // gdal metadata constants
      PAM_NODE_DATASET                = 'PAMDataset' ;
      PAM_NODE_RASTER_BAND            = 'PAMRasterBand' ;
      PAM_NODE_METADATA               = 'Metadata' ;
      PAM_NODE_MDI                    = 'MDI' ;

      PAM_ATTRIBUTE_BAND              = 'band' ;
      PAM_ATTRIBUTE_DOMAIN            = 'domain' ;

      PAM_STATISTICS_APPROXIMATE      = 'STATISTICS_APPROXIMATE' ;
      PAM_STATISTICS_MAXIMUM          = 'STATISTICS_MAXIMUM' ;
      PAM_STATISTICS_MEAN             = 'STATISTICS_MEAN' ;
      PAM_STATISTICS_MINIMUM          = 'STATISTICS_MINIMUM' ;
      PAM_STATISTICS_STDDEV           = 'STATISTICS_STDDEV' ;
      PAM_STATISTICS_VALID_PERCENT    = 'STATISTICS_VALID_PERCENT' ;

    private
      class function parseDouble  ( const _value : OleVariant
                                  ) : Double ;

      class function validatePath ( const _path                : String ;
                                    const _ext                 : String ;
                                    const _include_file_exists : Boolean
                                  ) : Boolean ;

    public
      /// <summary>
      ///   Parse TTKSTATS file and load all information into _layer
      /// </summary>
      /// <param name="_path">
      ///   path to a TTKSTATS file
      /// </param>
      /// <param name="_layer">
      ///   layer to which statistics are to be loaded
      /// </param>
      /// <returns>
      ///   True if TTKSTATS file was properly interpreted
      /// </returns>
      class function  ReadTtkStats( const _path  : String ;
                                    const _layer : TGIS_Layer
                                  ) : Boolean ;

      /// <summary>
      ///   Save layer statistics into TTKSTATS file
      /// </summary>
      /// <param name="_path">
      ///   path to a TTKSTATS file
      /// </param>
      /// <param name="_layer">
      ///   layer from which statistics are to be readed
      /// </param>
      class procedure SaveTtkStats( const _path  : String ;
                                    const _layer : TGIS_Layer
                                  ) ;

      /// <summary>
      ///   Parse GDAL's .AUX.XML file and load all information into _layer
      /// </summary>
      /// <param name="_path">
      ///   path to a .AUX.XML file
      /// </param>
      /// <param name="_layer">
      ///   pixel layer to which statistics are to be loaded
      /// </param>
      /// <returns>
      ///   True if .AUX.XML file was properly interpreted
      /// </returns>
      /// <remarks>
      ///   Use only with pixel layers.
      /// </remarks>
      class function  ReadAuxXml  ( const _path  : String ;
                                    const _layer : TGIS_LayerPixel
                                  ) : Boolean ;

      /// <summary>
      ///   Save layer statistics into GDAL's.AUX.XML file
      /// </summary>
      /// <param name="_path">
      ///   path to a .AUX.XML file
      /// </param>
      /// <param name="_layer">
      ///   pixel layer from which statistics are to be readed
      /// </param>
      /// <remarks>
      ///   Use only with pixel layers.
      /// </remarks>
      class procedure SaveAuxXml  ( const _path  : String ;
                                    const _layer : TGIS_LayerPixel
                                  ) ;

  end;
{$ENDREGION}

{$REGION 'TGIS_StatisticalFunctions'}
constructor TGIS_StatisticalFunctions.Create(
  const _calculate : Boolean
) ;
begin
  Average := _calculate ;
  Count := _calculate ;
  CountMissings := _calculate ;
  Max := _calculate ;
  Majority := _calculate ;
  Median := _calculate ;
  Min := _calculate ;
  Minority := _calculate ;
  Percentile := _calculate ;
  Range := _calculate ;
  Sample := _calculate ;
  StandardDeviation := _calculate ;
  Sum := _calculate ;
  Variance := _calculate ;
  Variety := _calculate ;
  Unique := _calculate ;
end;

constructor TGIS_StatisticalFunctions.Create(
  const _function  : TGIS_StatisticalFunction
) ;
begin
  Create ( False ) ;

  if _function = TGIS_StatisticalFunction.Average then
    Average := True
  else if _function = TGIS_StatisticalFunction.Count then
    Count := True
  else if _function = TGIS_StatisticalFunction.CountMissings then
    CountMissings := True
  else if _function = TGIS_StatisticalFunction.Max then
    Max := True
  else if _function = TGIS_StatisticalFunction.Majority then
    Majority := True
  else if _function = TGIS_StatisticalFunction.Median then
    Median := True
  else if _function = TGIS_StatisticalFunction.Min then
    Min := True
  else if _function = TGIS_StatisticalFunction.Minority then
    Minority := True
  else if _function = TGIS_StatisticalFunction.Percentile then
    Percentile := True
  else if _function = TGIS_StatisticalFunction.Range then
    Range := True
  else if _function = TGIS_StatisticalFunction.Sample then
    Sample := True
  else if _function = TGIS_StatisticalFunction.StandardDeviation then
    StandardDeviation := True
  else if _function = TGIS_StatisticalFunction.Sum then
    Sum := True
  else if _function = TGIS_StatisticalFunction.Variance then
    Variance := True
  else if _function = TGIS_StatisticalFunction.Variety then
    Variety := True
  else if _function = TGIS_StatisticalFunction.Unique then
    Unique := True ;
end;

class function TGIS_StatisticalFunctions.EmptyStatistics : TGIS_StatisticalFunctions ;
begin
  Result := TGIS_StatisticalFunctions.Create( False ) ;
end;

class function TGIS_StatisticalFunctions.BasicStatistics : TGIS_StatisticalFunctions ;
begin
  Result := TGIS_StatisticalFunctions.EmptyStatistics ;
  Result.Average := True ;
  Result.Count := True ;
  Result.Max := True ;
  Result.Min := True ;
  Result.Sum:= True ;
end;

class function TGIS_StatisticalFunctions.StandardStatistics : TGIS_StatisticalFunctions ;
begin
  Result := TGIS_StatisticalFunctions.BasicStatistics ;
  Result.CountMissings := True ;
  Result.Median := True ;
  Result.Range := True ;
  Result.StandardDeviation := True ;
  Result.Variance := True ;
end;

class function TGIS_StatisticalFunctions.AllStatistics : TGIS_StatisticalFunctions ;
begin
  Result := TGIS_StatisticalFunctions.Create( True ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsLayer'}
constructor TGIS_StatisticsLayer.Create(
  const _layer : TGIS_LayerAbstract
) ;
begin
  doCreate( _layer ) ;
end;
{$IFDEF CLR}
  procedure TGIS_StatisticsLayer.fadd_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent += _value ;
  end ;

  procedure TGIS_StatisticsLayer.fremove_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent -= _value ;
  end ;
{$ELSE}
  procedure TGIS_StatisticsLayer.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent := _value ;
  end ;

  function TGIS_StatisticsLayer.fget_BusyEvent : TGIS_BusyEvent ;
  begin
    Result := busyEventManager.BusyEvent ;
  end ;
{$ENDIF}

{$ENDREGION}

{$REGION 'TGIS_StatisticsAbstract'}
procedure TGIS_StatisticsAbstract.doCreate(
  const _layer : TGIS_LayerAbstract
) ;
begin
  if not assigned( _layer ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ), '_layer', 0 ) ;

  FAge := EncodeDate( 1899, 12, 30 ) ;
  FLayer := _layer ;
  FFastStatistics := FAST_STATISTICS ;
  FUseBesselCorrection := USE_BESSEL_CORRECTION ;
  FAvailableResults := TGIS_ListOfStrings.Create ;
  FDefinedResults := TGIS_ListOfStrings.Create ;

  busyEventManager := TGIS_BusyEventManager.Create( FLayer ) ;

  // True for Calculate() method
  // False for Calcualate(with arguments)
  useInternalParams := True ;

  resultsDct := TDictionary< String, TGIS_StatisticsLayerResult>.Create ;
end;

procedure TGIS_StatisticsAbstract.doDestroy ;
begin
  freeStatisticResults ;

  FreeObject( resultsDct ) ;
  FreeObject( FAvailableResults ) ;
  FreeObject( FDefinedResults ) ;

  FreeObject( busyEventManager ) ;
  inherited ;
end;

procedure TGIS_StatisticsAbstract.freeStatisticResults ;
var
  stats_result : TGIS_StatisticsLayerResult ;
  {$IFDEF DCC}
    stats_name : String ;
  {$ENDIF}
begin
  for stats_name in FDefinedResults do
    if resultsDct.TryGetValue( stats_name, stats_result ) then begin
      {$IFDEF CLR}
        stats_result.BusyEvent -= completingBusyEvent ;
      {$ENDIF}
      FreeObject( stats_result ) ;
    end ;
end;

procedure TGIS_StatisticsAbstract.remove(
  const _name : String
) ;
var
  stats_result : TGIS_StatisticsLayerResult ;
begin
  if resultsDct.TryGetValue( _name, stats_result ) then
    FreeObject( stats_result ) ;

  resultsDct.Remove( _name ) ;
  FDefinedResults.Remove( _name ) ;
  FAvailableResults.Remove( _name ) ;
end ;

procedure TGIS_StatisticsAbstract.setResampleRatio(
  const _ratio : Double
) ;
{$IFDEF DCC}
  var
    stats : TGIS_StatisticsResult ;
{$ENDIF}
begin
  for stats in resultsDct.Values do
    stats.setResampleRatio( _ratio ) ;
end ;

{$IFDEF OXYGENE}
  procedure TGIS_StatisticsAbstract.completingBusyEvent(
        _sender : TObject ;
        _args   : TGIS_BusyEventArgs
  ) ;
  var
    abrt : Boolean ;
  begin
    abrt := _args.Abort ;
    completingBusyEventEx( _sender, _args.Pos, _args.EndPos, abrt ) ;
    _args.Abort := abrt ;
  end ;

{$ELSE}
  procedure TGIS_StatisticsAbstract.completingBusyEvent(
        _sender : TObject ;
        _pos    : Integer ;
        _end    : Integer ;
    var _abort  : Boolean
  ) ;
  begin
    completingBusyEventEx( _sender, _pos, _end, _abort ) ;
  end ;
{$ENDIF}

procedure TGIS_StatisticsAbstract.completingBusyEventEx(
      _sender : TObject ;
      _pos    : Integer ;
      _end    : Integer ;
  var _abort  : Boolean
) ;
var
  event_manager : TGIS_BusyEventManager ;
  stage_id      : Integer ;
begin
  event_manager := TGIS_BusyEventManager( _sender ) ;
  stage_id := event_manager.Count - 1 ;
  case event_manager.Position[stage_id] of
    -1 : begin
      busyEventManager.EndEvent ;
    end ;
    0  : begin
      busyEventManager.StartEvent(
        event_manager.Name[stage_id],
        event_manager.EndValue[stage_id],
        event_manager.Sender[stage_id]
      ) ;
    end
    else
      busyEventManager.PushEvent ;
  end ;
end;

procedure TGIS_StatisticsAbstract.Add(
  const _name      : String
) ;
begin
  Add( _name, TGIS_StatisticalFunctions.StandardStatistics ) ;
end;

procedure TGIS_StatisticsAbstract.Add(
  const _name         : String ;
  const _function     : TGIS_StatisticalFunction ;
  const _params       : TGIS_VariantArray ;
  const _ignore_value : Variant
) ;
var
  functions    : TGIS_StatisticalFunctions ;
  stats_result : TGIS_StatisticsResult ;
begin
  // use existing API - at first, add particular function...
  functions := TGIS_StatisticalFunctions.Create( _function ) ;
  Add( _name, functions, _ignore_value ) ;

  // ... and then set params
  stats_result := Get( _name ) ;
  case _function of
    TGIS_StatisticalFunction.Percentile :
      stats_result.Percentile.Params := _params ;
    TGIS_StatisticalFunction.Sample :
      stats_result.Sample.Params := _params ;
    TGIS_StatisticalFunction.Unique :
      stats_result.Unique.Params := _params ;
  end ;
end;

procedure TGIS_StatisticsAbstract.Add(
  const _name      : String ;
  const _functions : TGIS_StatisticalFunctions
) ;
var
  default_ignore_value : Variant ;
begin
  default_ignore_value := getDefaultIgnoreValue ;
  Add( _name, _functions, default_ignore_value ) ;
end;

procedure TGIS_StatisticsAbstract.Add(
  const _name         : String ;
  const _functions    : TGIS_StatisticalFunctions ;
  const _ignore_value : Variant
) ;
var
  ignore_value : Variant ;
  dataset_type : TGIS_FieldType ;
  stats_result : TGIS_StatisticsLayerResult ;
begin
  if resultsDct.ContainsKey( _name ) then begin
    stats_result := resultsDct[_name] ;
    stats_result.updateFunctions( _functions ) ;
  end
  else begin
    dataset_type := determineDatasetType( _name ) ;

    if VarIsNull( _ignore_value ) then
      ignore_value := getDefaultIgnoreValue
    else
      ignore_value := _ignore_value ;

    stats_result := TGIS_StatisticsLayerResult.Create(
      FLayer,
      dataset_type,
      _name,
      _functions,
      ignore_value
    ) ;

    {$IFDEF CLR}
      stats_result.BusyEvent += completingBusyEvent ;
    {$ELSE}
      stats_result.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}completingBusyEvent ;
    {$ENDIF}

    resultsDct.Add( _name, stats_result ) ;

    FDefinedResults.Add( _name ) ;
  end ;
end;

function TGIS_StatisticsAbstract.Get(
  const _name : String
) : TGIS_StatisticsLayerResult ;
begin
  Result := nil ;
  resultsDct.TryGetValue( _name, Result ) ;
end;

procedure TGIS_StatisticsAbstract.Calculate ;
var
  lyr : TGIS_Layer ;
begin
  lyr := TGIS_Layer( FLayer ) ;

  FAvailableResults.Clear ;

  if resultsDct.Count = 0 then
    exit ;

  if useInternalParams then begin
    paramExtent := GisWholeWorld ;
    paramShape := nil ;
    paramDe9im := '' ;
    useInternalParams := True ;
  end ;

  if GisIsCommonExtent( paramExtent, lyr.Extent ) then
    paramExtent := GisCommonExtent( paramExtent, lyr.Extent )
  else
    exit ;

  setValuesLimit ;

  busyEventManager.StartEvent( _rsrc( GIS_RS_BUSY_STATISTICS_CALCULATE ), 2 ) ;
  try
    if collectValuesFromDataset then
      exit ;

    if completeResults then
      exit ;

    FAge := Now ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure TGIS_StatisticsAbstract.Calculate(
  const _extent          : TGIS_Extent ;
  const _shape           : TGIS_ShapeAbstract ;
  const _de9im           : String ;
  const _fast_statistics : Boolean
) ;
begin
  FFastStatistics := _fast_statistics ;
  paramExtent := _extent ;
  paramShape := _shape ;
  paramDe9im := _de9im ;
  useInternalParams := False ;
  Calculate ;
end;

procedure TGIS_StatisticsAbstract.Reset ;
begin
  freeStatisticResults ;
  resultsDct.Clear ;
  FAvailableResults.Clear ;
  FDefinedResults.Clear ;
end;

procedure TGIS_StatisticsAbstract.ResetModified ;
var
  o               : TGIS_StatisticsLayerResult ;
  names_to_remove : TList< String > ;
  {$IFDEF DCC}
    results_name  : String ;
  {$ENDIF}
begin
  if not Modified then exit ;

  names_to_remove := TList< String >.Create ;
  try
    for results_name in FDefinedResults do begin
      if not resultsDct.TryGetValue( results_name, o ) then continue ;

      if FAvailableResults.Contains( results_name ) then begin
        if not o.Average.Calculated           and o.Average.Requested then
          o.Average.FRequested := False ;
        if not o.Count.Calculated             and o.Count.Requested then
          o.Count.FRequested := False ;
        if not o.CountMissings.Calculated     and o.CountMissings.Requested then
          o.CountMissings.FRequested := False ;
        if not o.Majority.Calculated          and o.Majority.Requested then
          o.Majority.FRequested := False ;
        if not o.Max.Calculated               and o.Max.Requested then
          o.Max.FRequested := False ;
        if not o.Median.Calculated            and o.Median.Requested then
          o.Median.FRequested := False ;
        if not o.Min.Calculated               and o.Min.Requested then
          o.Min.FRequested := False ;
        if not o.Minority.Calculated          and o.Minority.Requested then
          o.Minority.FRequested := False ;
        if not o.Percentile.Calculated        and o.Percentile.Requested then
          o.Percentile.FRequested := False ;
        if not o.Range.Calculated             and o.Range.Requested then
          o.Range.FRequested := False ;
        if not o.Sample.Calculated             and o.Sample.Requested then
          o.Sample.FRequested := False ;
        if not o.StandardDeviation.Calculated and o.StandardDeviation.Requested then
          o.StandardDeviation.FRequested := False ;
        if not o.Sum.Calculated               and o.Sum.Requested then
          o.Sum.FRequested := False ;
        if not o.Unique.Calculated            and o.Unique.Requested then
          o.Unique.FRequested := False ;
        if not o.Variance.Calculated          and o.Variance.Requested then
          o.Variance.FRequested := False ;
        if not o.Variety.Calculated           and o.Variety.Requested then
          o.Variety.FRequested := False ;
      end
      else begin
        FreeObject( o ) ;
        resultsDct.Remove( results_name ) ;
        names_to_remove.Add( results_name ) ;
      end ;
    end ;

    for results_name in names_to_remove do
      FDefinedResults.Remove( results_name ) ;

  finally
    FreeObject( names_to_remove ) ;
  end;
end;

function TGIS_StatisticsAbstract.LoadFromFile : Boolean ;
var
  ll   : TGIS_Layer ;
  path : String ;
begin
  Reset;
  ll := TGIS_Layer( FLayer ) ;
  path := ll.Path + GIS_TTKSTATS_EXT ;
  Result := LoadFromFile( path ) ;
end;

function TGIS_StatisticsAbstract.LoadFromFile(
  const _path : String
) : Boolean ;
begin
  Reset;
  Result := T_StatisticsIO.ReadTtkStats( _path, TGIS_Layer( FLayer ) )
end;

procedure TGIS_StatisticsAbstract.SaveToFile ;
var
  ll   : TGIS_Layer ;
  path : String ;
begin
  ll := TGIS_Layer( FLayer ) ;
  path := TGIS_Layer( FLayer ).Path + GIS_TTKSTATS_EXT ;
  SaveToFile( path ) ;
end;

procedure TGIS_StatisticsAbstract.SaveToFile(
  const _path : String
) ;
begin
  if IsServerPath( _path ) then exit ;
  T_StatisticsIO.SaveTtkStats( _path, TGIS_Layer( FLayer ) ) ;
end;

function TGIS_StatisticsAbstract.fget_Modified : Boolean ;
var
  o              : TGIS_StatisticsResult ;
  {$IFDEF DCC}
    results_name : String ;
  {$ENDIF}
begin
  Result := False ;

  for results_name in FDefinedResults do begin

    if FAvailableResults.Contains( results_name ) then begin
      o := resultsDct[ results_name ] ;

      Result := Result or
        ( o.Average.Calculated           <> o.Average.Requested           ) or
        ( o.Count.Calculated             <> o.Count.Requested             ) or
        ( o.CountMissings.Calculated     <> o.CountMissings.Requested     ) or
        ( o.Majority.Calculated          <> o.Majority.Requested          ) or
        ( o.Max.Calculated               <> o.Max.Requested               ) or
        ( o.Median.Calculated            <> o.Median.Requested            ) or
        ( o.Min.Calculated               <> o.Min.Requested               ) or
        ( o.Minority.Calculated          <> o.Minority.Requested          ) or
        ( o.Percentile.Calculated        <> o.Percentile.Requested        ) or
        ( o.Range.Calculated             <> o.Range.Requested             ) or
        ( o.Sample.Calculated            <> o.Sample.Requested             ) or
        ( o.StandardDeviation.Calculated <> o.StandardDeviation.Requested ) or
        ( o.Sum.Calculated               <> o.Sum.Requested               ) or
        ( o.Unique.Calculated            <> o.Unique.Requested            ) or
        ( o.Variance.Calculated          <> o.Variance.Requested          ) or
        ( o.Variety.Calculated           <> o.Variety.Requested           ) ;
    end
    else begin
      Result := True ;
    end ;
  end ;
end;

function TGIS_StatisticsAbstract.fget_Obsolete : Boolean ;
begin
  {$IFDEF ISLAND}
    Result := False ;
  {$ELSE}
    {$IFNDEF ANDROID}
      if FLayer is TGIS_LayerSqlAbstract then
        Result := False ;
    {$ENDIF}

    Result := CompareDateTime( FAge, TGIS_Layer( FLayer ).Age ) < 0;
  {$ENDIF}
end;

procedure TGIS_StatisticsAbstract.setValuesLimit ;
{$IFDEF DCC}
var
  stats : TGIS_StatisticsResult ;
{$ENDIF}
begin
  for stats in resultsDct.Values do begin
    if FastStatistics then
      stats.valuesLimit := getValuesLimit
    else
      stats.valuesLimit := MAX_VALUES_LIMIT ;
  end ;
end;

function TGIS_StatisticsAbstract.completeResults : Boolean ;
var
  {$IFDEF DCC}
    result_name : String ;
  {$ENDIF}
  stats         : TGIS_StatisticsLayerResult ;
begin
  Result := False ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_STATISTICS_COMPLETE ), FDefinedResults.Count
  ) ;
  try
    for result_name in FDefinedResults do begin
      stats := resultsDct[result_name] ;
      // to avoid using trigger twice incoming (TGIS_StatisticsResult) busy
      // event manager don't use progress threshold,
      // this (TGIS_StatisticsAbstract) do
      stats.busyEventManager.UseProgressThreshold := False ;

      if stats.doComplete then begin
        FAvailableResults.Clear ;
        Result := True ;
        exit ;
      end ;

      FAvailableResults.Add( result_name  ) ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end;
end;

procedure TGIS_StatisticsAbstract.makeDefinedResultsAvailable ;
{$IFDEF DCC}
  var
    stats_name : String ;
{$ENDIF}
begin
  for stats_name in FDefinedResults do
    FAvailableResults.Add( stats_name ) ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsVector'}
constructor TGIS_StatisticsVector.Create(
  const _layer : TGIS_LayerAbstract
) ;
begin
  inherited ;

  if not( _layer is TGIS_LayerVector ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_LAYERBADFORMAT ), TGIS_Layer( _layer ).Name, 0
    ) ;

  paramQuery := '' ;
  paramUseSelected := USE_SELECTED ;
end;

procedure TGIS_StatisticsVector.AddAll ;
begin
  AddAllFields ;
end;

procedure TGIS_StatisticsVector.AddAll(
  const _functions : TGIS_StatisticalFunctions
) ;
begin
  AddAllFields( _functions ) ;
end;

procedure TGIS_StatisticsVector.AddAllFields ;
begin
  AddAllFields( TGIS_StatisticalFunctions.StandardStatistics ) ;
end;

procedure TGIS_StatisticsVector.AddAllFields(
  const _functions : TGIS_StatisticalFunctions
) ;
var
  i          : Integer ;
  lv         : TGIS_LayerVector ;
  field_info : TGIS_FieldInfo ;
begin
  Reset ;
  lv := TGIS_LayerVector( Layer ) ;
  for i := 0 to lv.Fields.Count - 1 do begin
    field_info := lv.Fields[i] ;
    Add( field_info.NewName, _functions ) ;
  end ;
end ;

procedure TGIS_StatisticsVector.Calculate(
  const _extent          : TGIS_Extent ;
  const _shape           : TGIS_ShapeAbstract ;
  const _de9im           : String ;
  const _fast_statistics : Boolean ;
  const _query           : String ;
  const _use_selected    : Boolean
) ;
begin
  paramQuery := _query ;
  paramUseSelected := _use_selected ;
  inherited Calculate( _extent, _shape, _de9im, _fast_statistics ) ;
end;

function TGIS_StatisticsVector.getDefaultIgnoreValue : Variant ;
begin
  Result := NullVar ;
end;

function TGIS_StatisticsVector.getValuesLimit : Integer ;
begin
  Result := FAST_STATISTICS_VECTOR_LIMIT ;
end;

function TGIS_StatisticsVector.collectValuesFromDataset : Boolean ;
var
  lv                : TGIS_LayerVector ;
  shp               : TGIS_Shape ;
  field_var         : Variant ;
  i_shape           : Integer ;
  n_shapes          : Integer ;
  all_shapes        : Integer ;
  use_sampling      : Boolean ;
  resample_ratio    : Double ;
  sampler_iter           : T_RandomSamplingIterator ;
  stats             : TGIS_StatisticsResult ;
  binded_field      : Integer ;
  binded_fields_dct : TDictionary<String, Integer> ;
  enumerator        : TGIS_LayerVectorEnumerator ;
  {$IFDEF DCC}
    pair            : TPair< String, TGIS_StatisticsLayerResult > ;
  {$ENDIF}
begin
  Result := False ;

  lv := TGIS_LayerVector( Layer ) ;

  //? improve the determination of n_shapes
  all_shapes := lv.GetLastUid ;
  if all_shapes = 0 then
    exit ;

  // the number of shapes exceeds the limit for Fast Statistics
  if FastStatistics and ( all_shapes > FAST_STATISTICS_VECTOR_LIMIT ) then begin
    n_shapes := FAST_STATISTICS_VECTOR_LIMIT ;
    resample_ratio := ( 1.0 * n_shapes ) / all_shapes ;
    use_sampling := True ;
  end
  else begin
    n_shapes := all_shapes ;
    resample_ratio := 1.0 ;
    use_sampling := False ;
    // turn off 'FastStatistics' mode, because the number off all elements is below the limit
    FFastStatistics := False ;
  end ;

  setResampleRatio( resample_ratio ) ;

  stats := nil ;

  enumerator := lv.Loop(
    paramExtent,
    paramQuery,
    TGIS_Shape( paramShape ),
    paramDe9im
  ).GetEnumerator ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_STATISTICS_COLLECT ), n_shapes
  ) ;
  binded_fields_dct := TDictionary<String, Integer>.Create ;
  sampler_iter := T_RandomSamplingIterator.Create( n_shapes, all_shapes, False ) ;
  try
    if use_sampling and ( not sampler_iter.Next ) then
      exit ;

    for pair in resultsDct do
      binded_fields_dct.Add( pair.Key, enumerator.BindField( pair.Key ) ) ;

    i_shape := -1 ;
    while enumerator.MoveNext do begin
      inc( i_shape ) ;

      if busyEventManager.PushEvent then begin
        Result := True ;
        if assigned( stats ) then
          stats.cleanUp ;
        exit ;
      end ;

      // randomly select 10 000 shapes from dataset
      if use_sampling and ( i_shape <> sampler_iter.CurrentItem ) then
        continue ;

      {$IFDEF DCC}
        shp := enumerator.GetCurrent ;
      {$ELSE}
        shp := TGIS_Shape( enumerator.GetCurrent ) ;
      {$ENDIF}

      if paramUseSelected and ( not shp.IsSelected ) then
        continue ;

      for pair in resultsDct do begin
        binded_field := binded_fields_dct[pair.Key] ;
        field_var := enumerator.GetField( binded_field ) ;
        stats := pair.Value ;
        stats.Update( field_var ) ;
      end ;

      if use_sampling and ( not sampler_iter.Next ) then
        break ;
    end ;
  finally
    FreeObject( sampler_iter ) ;
    FreeObject( enumerator ) ;
    FreeObject( binded_fields_dct ) ;
    busyEventManager.EndEvent ;
  end ;
end;

function TGIS_StatisticsVector.determineDatasetType(
  const _name : String
) : TGIS_FieldType ;
begin
  if not determineDatasetTypeNoExc( _name, Result ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FIELDNOEXIST ), _name, 0 ) ;
end;

function TGIS_StatisticsVector.determineDatasetTypeNoExc(
  const _name : String ;
  var   _type : TGIS_FieldType
) : Boolean ;
var
  lv          : TGIS_LayerVector ;
  field_index : Integer ;

  function findJoinField( var _subtype  : TGIS_FieldType ) : Boolean ;
  var
    wasjoin : Boolean ;
    {$IFDEF CLR}
      dt    : System.Data.DataTable ;
    {$ENDIF}
    i       : Integer ;
    fname   : String ;
  begin
    fname := FromJoinFieldName( _name ) ;
    wasjoin := False ;
    Result := False ;

    i := 0 ; // to avoid hints ;

    {$IFNDEF GIS_NODB}
      if ( not wasjoin ) and Assigned( lv.JoinDB ) then begin
        wasjoin := True ;
        for i := 0 to lv.JoinDB.FieldCount-1 do begin
          if CompareText( lv.JoinDB.Fields[i].FieldName, fname ) = 0 then begin
            Result := JoinFieldType( lv.JoinDB.Fields[i], _subtype ) ;
            break ;
          end ;
        end ;
      end ;
    {$ENDIF}

    {$IFNDEF GIS_NOADO_JOIN}
      if ( not wasjoin ) and Assigned( lv.JoinADO ) then begin
        wasjoin := True ;
        for i := 0 to lv.JoinADO.Fields.Count-1 do begin
          if CompareText( lv.JoinADO.Fields[i].Name, fname ) = 0 then begin
            Result := JoinFieldType( lv.JoinADO.Fields[i], _subtype ) ;
            break ;
          end ;
        end ;
      end ;
    {$ENDIF}

    {$IFDEF CLR}
      dt := nil ; // to avoid hints ;
      {$IFNDEF GIS_NOADO}
       if ( not wasjoin ) and assigned( lv.JoinNET ) then begin
         wasjoin := True ;
         dt := lv.JoinNET as System.Data.DataTable ;
          for i := 0 to dt.Columns.Count -1 do
            if CompareText( dt.Columns[i].ColumnName, fname ) = 0 then begin
              Result := JoinFieldType( dt.Columns[i].DataType, _subtype ) ;
              break ;
            end ;
       end ;
      {$ENDIF}
    {$ENDIF}

    {$IFDEF JAVA}
      {$IFNDEF GIS_NOJDBC}
      if ( not wasjoin ) and assigned( lv.JoinJDBC ) then begin
        wasjoin := True ;
        for i := 1 to java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnCount do begin
          if CompareText( java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnName[i], fname ) = 0 then begin
            Result := JoinFieldType( java.sql.ResultSet(lv.JoinJDBC).getMetaData.ColumnType[i], _subtype ) ;
            break ;
          end ;
        end ;
      end ;
      {$ENDIF}
    {$ENDIF}

  end ;

begin
  Result := True ;

  lv := TGIS_LayerVector( FLayer ) ;
  field_index := lv.FindField( _name ) ;

  if field_index >= 0 then
    _type := lv.FieldInfo(field_index).FieldType
  else if not findJoinField( _type ) then
    Result := False ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsPixel'}
constructor TGIS_StatisticsPixel.Create(
  const _layer : TGIS_LayerAbstract
) ;
begin
  inherited ;

  if not( _layer is TGIS_LayerPixel ) then
    raise EGIS_Exception.Create(
      _rsrc( GIS_RS_ERR_LAYERBADFORMAT ), TGIS_Layer( _layer ).Name, 0
    ) ;
end;

procedure TGIS_StatisticsPixel.AddAll ;
begin
  AddAllBands ;
end;

procedure TGIS_StatisticsPixel.AddAll(
  const _functions : TGIS_StatisticalFunctions
) ;
begin
  AddAllBands( _functions ) ;
end;

procedure TGIS_StatisticsPixel.AddAllBands ;
begin
  AddAllBands( TGIS_StatisticalFunctions.StandardStatistics ) ;
end;

procedure TGIS_StatisticsPixel.AddAllBands(
  const _functions : TGIS_StatisticalFunctions
) ;
var
  lp : TGIS_LayerPixel ;
begin
  Reset ;
  lp := TGIS_LayerPixel( Layer ) ;
  if lp.IsGrid then begin
    Add( GIS_BAND_GRID, _functions, lp.NoDataValue ) ;
  end
  else if lp.IsPixel then begin
    Add( GIS_BAND_A, _functions, lp.NoDataValue ) ;
    Add( GIS_BAND_R, _functions, lp.NoDataValue ) ;
    Add( GIS_BAND_G, _functions, lp.NoDataValue ) ;
    Add( GIS_BAND_B, _functions, lp.NoDataValue ) ;
  end
  else begin
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERBADFORMAT ), TGIS_Layer( Layer ).Name , 0 ) ;
  end ;
end;

function TGIS_StatisticsPixel.LoadFromFile : Boolean ;
var
  path : String ;

  function get_aux_xml_path( const _path : String ) : String ;
  begin
    Result := _path + GIS_AUX_XML_EXT ;
  end;

begin
  // if .ttkstats failed try .aux.xml
  Result := inherited ;
  if not Result then begin ;
    path := get_aux_xml_path( TGIS_Layer( FLayer ).Path ) ;
    Result := LoadFromFile( path ) ;
  end ;
end;

function TGIS_StatisticsPixel.LoadFromFile(
  const _path : String
) : Boolean ;
begin
  Result := inherited ;
  if not Result then begin
    Result := T_StatisticsIO.ReadAuxXml( _path, TGIS_LayerPixel( FLayer ) )
  end ;
end;

function TGIS_StatisticsPixel.getDefaultIgnoreValue : Variant ;
begin
  Result := TGIS_LayerPixel( Layer ).NoDataValue ;
end;

function TGIS_StatisticsPixel.collectValuesFromDataset : Boolean ;
var
  lp               : TGIS_LayerPixel ;
  lp_px_size       : TGIS_Point ;

  n_cells          : Int64 ;
  n_cells_fast     : Int64 ;
  i_tile           : Integer ;
  resample_ratio   : Double ;

  px_size_loop     : Double ;
  ext_loop         : TGIS_Extent ;

  common_ext       : TGIS_Extent ;
  common_ext_range : TGIS_Point ;

  tiles_count      : Integer ;  // number of tiles
  tiles_no         : TPoint ;   // number of horizontal (X) and vertical (Y) tiles
  tile_index       : TPoint ;
  tile_size        : TGIS_Point ;

  need_mapped      : Boolean ;
  need_native      : Boolean ;
  binded_bands_dct : TDictionary< String, Integer > ;

  function calc_extent_range(
    const _ext : TGIS_Extent
  ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point.Create ;
    {$ENDIF}

    Result.X := Abs( _ext.XMax - _ext.XMin ) ;
    Result.Y := Abs( _ext.YMax - _ext.YMin ) ;
  end ;

  function calc_pixel_size(
    const _lp : TGIS_LayerPixel
  ) : TGIS_Point ;
  var
    lp_ext_range : TGIS_Point ;
  begin
    // calc native pixel resolution
    lp_ext_range := calc_extent_range ( lp.Extent ) ;

    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point.Create ;
    {$ENDIF}

    Result.X := lp_ext_range.X / lp.BitWidth ;
    Result.Y := lp_ext_range.Y / lp.BitHeight ;
  end ;

  function get_num_cells(
    const _ext_range : TGIS_Point ;
    const _px_size_X : Double ;
    const _px_size_Y : Double
  ) : Int64 ;
  var
    delta_X : Double ;
    delta_Y : Double ;
    cells_X : Int64 ;
    cells_Y : Int64 ;
  begin
    delta_X := _ext_range.X / _px_size_X ;
    delta_Y := _ext_range.Y / _px_size_Y ;

    cells_X := TruncS( delta_X) ;
    if not GisIsSameValue( delta_X, cells_X ) then
      inc( cells_X ) ;

    cells_Y := TruncS( delta_Y ) ;
    if not GisIsSameValue( delta_Y, cells_Y ) then
      inc( cells_Y ) ;

    Result := cells_X * cells_Y ;
  end ;

  function calc_tile_size(
    const _px_size      : TGIS_Point ;
    const _px_dimension : Integer
  ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := TGIS_Point.Create ;
    {$ENDIF}

    Result.X := lp_px_size.X * MAX_PIXEL_DIMENSION ;
    Result.Y := lp_px_size.Y * MAX_PIXEL_DIMENSION ;
  end ;

  function get_num_XY_tiles(
    const _ext       : TGIS_Extent ;
    const _tile_size : TGIS_Point
  ) : TPoint ;
  var
    x, y      : Integer ;
    ext_range : TGIS_Point ;
  begin
    ext_range := calc_extent_range( _ext ) ;

    x := FloorS( ext_range.X / _tile_size.X ) + 1 ;
    if GisIsSameValue( ext_range.X, x * _tile_size.X ) then
      dec( x ) ;

    y := FloorS( ext_range.Y / _tile_size.Y ) + 1 ;
    if GisIsSameValue( ext_range.Y, y * _tile_size.Y ) then
      dec( y ) ;

    Result := TPoint.Create( x, y ) ;
  end ;

  function get_tile_index(
    const _i        : Integer ;
    const _tiles_no : TPoint
  ) : TPoint ;
  var
    x, y      : Integer ;
  begin
    x := _i mod _tiles_no.X ;
    y := _i div _tiles_no.X ;  // X is OK here!

    Result := TPoint.Create( x, y ) ;
  end ;

  function get_tile_extent(
    const _tile_index : TPoint ;
    const _ext        : TGIS_Extent ;
    const _tile_size  : TGIS_Point ;
    const _px_size    : TGIS_Point
  ) : TGIS_Extent ;
  var
    min_X, min_Y : Double ;
    max_X, max_Y : Double ;
  begin
    min_X := _ext.XMin + _tile_index.X * _tile_size.X + 1E-3*_px_size.X ;
    max_X := min_X + _tile_size.X - 0.5*_px_size.X ;
    min_Y := _ext.YMin + _tile_index.Y * _tile_size.Y + 1E-3*_px_size.Y ;
    max_Y := min_Y + _tile_size.Y - 0.5*_px_size.Y ;

    Result := GisCommonExtent( paramExtent, GisExtent( min_X, min_Y, max_X, max_Y )) ;
  end ;

  // determine which bands are requested
  function determine_requested_bands_and_bind : TDictionary< String, Integer > ;
  const
    NO_BIND_BAND_ID = -1 ;
  var
    band_id     : Integer ;
    {$IFDEF DCC}
      band_name : String ;
    {$ENDIF}
  begin
    Result := TDictionary< String, Integer >.Create ;

    need_mapped := False ;
    need_native := False ;

    for band_name in resultsDct.Keys do begin
      if ( band_name = GIS_BAND_GRID ) or
         ( band_name = GIS_BAND_DEFAULT ) or
         ( band_name = GIS_BAND_A ) or
         ( band_name = GIS_BAND_R ) or
         ( band_name = GIS_BAND_G ) or
         ( band_name = GIS_BAND_B ) or
         ( band_name = GIS_BAND_H ) or
         ( band_name = GIS_BAND_S ) or
         ( band_name = GIS_BAND_L )
      then begin
        need_mapped := True ;
        Result.Add( band_name, NO_BIND_BAND_ID ) ;
      end
      else if TryStrToInt( band_name, band_id ) and
              ( band_id <= lp.BandsCount ) and
              ( band_id > 0 )
      then begin
        need_native := True ;
        Result.Add( band_name, band_id - 1 ) ;
      end
      else begin
        Result.Add( band_name, NO_BIND_BAND_ID  ) ;
      end ;
    end ;
  end;

  // returns True if the process has been aborted
  function loop_through_layer(
    const _band_param : Integer
  ) : Boolean ;
  var
    pixel_item : TGIS_PixelItem ;
    stats      : TGIS_StatisticsResult ;
    name       : String ;
    pixel_val  : Double ;
    band_id    : Integer ;
    {$IFDEF DCC}
      obj      : TGIS_PixelItem ;
      pair     : TPair< String, TGIS_StatisticsLayerResult > ;
    {$ENDIF}
  begin
    Result := False ;
    stats := nil ;

    for obj in lp.Loop(
      ext_loop,
      px_size_loop,
      False,
      TGIS_Shape( paramShape ),
      paramDe9im,
      _band_param,
      False
    ) do begin
      if busyEventManager.PushEvent then begin
        Result := True ;
        if assigned( stats ) then
          stats.cleanUp ;
        exit ;
      end ;

      {$IFDEF DCC}
        pixel_item := obj ;
      {$ELSE}
        pixel_item := TGIS_PixelItem( obj ) ;
      {$ENDIF}
      stats := nil ;
      for pair in resultsDct do begin
        stats := pair.Value ;
        name := pair.Key ;

        case _band_param of
          -1 :  // bands
          begin
            band_id := binded_bands_dct[name] ;
            if ( band_id > -1 ) then
              pixel_val := pixel_item.Bands[band_id]
            else
              continue ;
          end ;
          0 :  // colors
          begin
            if ( name = GIS_BAND_GRID ) then
              pixel_val := pixel_item.Value
            else if ( name = GIS_BAND_DEFAULT ) then
              pixel_val := pixel_item.Value
            else if ( name = GIS_BAND_A ) then
              pixel_val := pixel_item.Color.A
            else if ( name = GIS_BAND_R ) then
              pixel_val := pixel_item.Color.R
            else if ( name = GIS_BAND_G ) then
              pixel_val := pixel_item.Color.G
            else if ( name = GIS_BAND_B ) then
              pixel_val := pixel_item.Color.B
            else if ( name = GIS_BAND_H ) then
              pixel_val := pixel_item.Color.H
            else if ( name = GIS_BAND_S ) then
              pixel_val := pixel_item.Color.S
            else if ( name = GIS_BAND_L ) then
              pixel_val := pixel_item.Color.L
            else
              continue ;
          end
        else
          pixel_val := NaN ;
        end ;

        stats.Update( pixel_val ) ;
      end ;
    end ;
  end;

  // loop through the layer pixel and gather data from proper band
  // returns True if the process has been aborted
  function collect_data_from_layer(
    const _need_mapped  : Boolean ;
    const _need_native  : Boolean
  ) : Boolean ;  // if False then exit
  var
    band_param : Integer ;
  begin
    Result := False ;

    if _need_mapped then begin
      band_param := 0 ;
      Result := loop_through_layer( band_param ) ;
      if Result then
        exit ;
    end ;

    if _need_native then begin
      band_param := -1 ;
      Result := loop_through_layer( band_param ) ;
      if Result then
        exit ;
    end ;
  end;

begin
  Result := False ;

  lp := TGIS_LayerPixel( Layer ) ;

  // calc native pixel resolution
  lp_px_size := calc_pixel_size( lp ) ;

  // approximated number of cells in common extent
  common_ext := GisCommonExtent( paramExtent , lp.Extent ) ;
  common_ext_range := calc_extent_range ( common_ext ) ;
  n_cells := get_num_cells( common_ext_range, lp_px_size.X, lp_px_size.Y ) ;

  if n_cells = 0 then
    exit ;

  // default params for 'Loop'
  ext_loop := paramExtent ;
  px_size_loop := 0.0 ;

  resample_ratio := 1.0 ;
  tiles_no := TPoint.Create( 1, 1 ) ;
  tile_size := calc_tile_size( lp_px_size, MAX_PIXEL_DIMENSION ) ;

  if n_cells > Power( MAX_PIXEL_DIMENSION, 2 ) then begin  // number of cells exceed max
    // resampling version
    if FastStatistics then begin
      px_size_loop := common_ext_range.X / MAX_PIXEL_DIMENSION ;
      n_cells_fast := get_num_cells( common_ext_range, px_size_loop, px_size_loop ) ;
      resample_ratio := ( 1.0 * n_cells_fast ) / n_cells ;
      n_cells := n_cells_fast ;
    end
    // windowing version
    else begin
      // find number of vertical and horizontal tiles in common extent
      tiles_no := get_num_XY_tiles( common_ext, tile_size ) ;
    end ;
  end
  else begin
    FFastStatistics := False ;
  end ;

  setResampleRatio( resample_ratio ) ;

  binded_bands_dct := determine_requested_bands_and_bind ;
  if need_native and need_mapped then
    n_cells := 2 * n_cells ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_STATISTICS_COLLECT ), n_cells
  ) ;
  try
    tiles_count := tiles_no.X * tiles_no.Y ;

    for i_tile := 0 to tiles_count - 1 do begin
      if tiles_count > 1 then begin
        tile_index := get_tile_index( i_tile, tiles_no ) ;
        ext_loop := get_tile_extent( tile_index, common_ext, tile_size, lp_px_size ) ;
      end ;

      if collect_data_from_layer( need_mapped, need_native ) then begin
        Result := True ;
        exit ;
      end ;
    end ;
  finally
    FreeObject( binded_bands_dct ) ;
    busyEventManager.EndEvent ;
  end ;
end;

function TGIS_StatisticsPixel.determineDatasetType(
  const _name : String
) : TGIS_FieldType ;
begin
  if not determineDatasetTypeNoExc( _name, Result ) then
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELBAND ), _name, 0 ) ;
end;

function TGIS_StatisticsPixel.getValuesLimit : Integer ;
begin
  Result := FAST_STATISTICS_PIXEL_LIMIT ;
end;

function TGIS_StatisticsPixel.determineDatasetTypeNoExc(
  const _name : String ;
  var   _type : TGIS_FieldType
) : Boolean ;
var
  lp          : TGIS_LayerPixel ;

  function is_band_number_valid(
    const _band_name : String
  ) : Boolean ;
  var
    band : Integer ;
  begin
    band := -1 ;
    TryStrToInt( _band_name, band ) ;
    Result := ( band <= lp.BandsCount ) and ( band > 0 ) ;
  end ;

  function is_grid_band_name_valid(
    const _band_name : String
  ) : Boolean ;
  begin
    Result := lp.IsGrid and
    ( ( _band_name = GIS_BAND_GRID ) or
      ( _band_name = GIS_BAND_DEFAULT ) ) ;
  end ;

  function is_pixel_band_name_valid(
    const _band_name : String
  ) : Boolean ;
  begin
    Result :=  lp.IsPixel and (
      ( _band_name = GIS_BAND_A ) or
      ( _band_name = GIS_BAND_R ) or
      ( _band_name = GIS_BAND_G ) or
      ( _band_name = GIS_BAND_B ) or
      ( _band_name = GIS_BAND_H) or
      ( _band_name = GIS_BAND_S ) or
      ( _band_name = GIS_BAND_L )
    ) ;
  end ;

begin
  Result := True ;
  _type := TGIS_FieldType.Float ;

  lp := TGIS_LayerPixel( FLayer ) ;
  if not ( is_band_number_valid( _name ) or
           is_grid_band_name_valid( _name ) or
           is_pixel_band_name_valid( _name ) )
  then
    Result := False ;
end ;
{$ENDREGION}

{$REGION 'TGIS_StatisticsResult'}
function  TGIS_StatisticsResult.fget_UseBesselCorrection : Boolean ;
begin
  Result := FUseBesselCorrection ;
end;

procedure TGIS_StatisticsResult.fset_UseBesselCorrection(
  const _value : Boolean
) ;
begin
  FUseBesselCorrection := _value ;
end;

constructor TGIS_StatisticsResult.Create(
  const _dataset_type : TGIS_FieldType ;
  const _name         : String ;
  const _functions    : TGIS_StatisticalFunctions ;
  const _ignore_value : Variant
) ;
begin
  inherited Create ;

  // property fields
  FName := _name ;
  FDatasetType := _dataset_type ;
  FIgnoreValue := _ignore_value ;
  FUseBesselCorrection := USE_BESSEL_CORRECTION ;

  valuesLimit := MAX_VALUES_LIMIT ;

  busyEventManager := TGIS_BusyEventManager.Create( Self ) ;

  // ignore value
  isIgnoreValue := not VarIsNull( FIgnoreValue ) ;
  boolIgnoreValue :=  False ;
  stringIgnoreValue := '' ;
  doubleIgnoreValue := NaN ;

  if isIgnoreValue then begin
    case FDatasetType of
      TGIS_FieldType.Number,
      TGIS_FieldType.Float,
      TGIS_FieldType.Date :
      begin
        doubleIgnoreValue := VarToDouble( FIgnoreValue ) ;
        if IsNan( doubleIgnoreValue ) then
          isIgnoreValue := False ;
      end ;
      TGIS_FieldType.String  : stringIgnoreValue := VarToString( FIgnoreValue ) ;
      TGIS_FieldType.Boolean : boolIgnoreValue := VarToBoolean( FIgnoreValue ) ;
    end ;
  end ;

  // internal statistics
  initializeInternalStatistics ;

  valuesSorted := False ;
  valuesLst := TList< Double >.Create ;
  stringsDct := TDictionary< String, Integer >.Create ;

  // Average
  if _functions.Average then
    FAverage := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Average, True )
  else
    FAverage := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Average, False ) ;

  // Count
  if _functions.Count then
    FCount := TGIS_StatisticsItemInteger.Create( TGIS_StatisticalFunction.Count, True )
  else
    FCount := TGIS_StatisticsItemInteger.Create( TGIS_StatisticalFunction.Count, False ) ;

  // CountMissings
  if _functions.CountMissings then
    FCountMissings := TGIS_StatisticsItemInteger.Create( TGIS_StatisticalFunction.CountMissings, True )
  else
    FCountMissings := TGIS_StatisticsItemInteger.Create( TGIS_StatisticalFunction.CountMissings, False ) ;

  // Majority
  if _functions.Majority then
    FMajority := TGIS_StatisticsItemVariant.Create( TGIS_StatisticalFunction.Majority, True )
  else
    FMajority := TGIS_StatisticsItemVariant.Create( TGIS_StatisticalFunction.Majority, False ) ;

  // Max
  if _functions.Max then
    FMax := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Max, True )
  else
    FMax := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Max, False ) ;

  // Median
  if _functions.Median then
    FMedian := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Median, True )
  else
    FMedian := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Median, False ) ;

  // Min
  if _functions.Min then
    FMin := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Min, True )
  else
    FMin := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Min, False ) ;

  // Minority
  if _functions.Minority then
    FMinority := TGIS_StatisticsItemVariant.Create( TGIS_StatisticalFunction.Minority, True )
  else
    FMinority := TGIS_StatisticsItemVariant.Create( TGIS_StatisticalFunction.Minority, False ) ;

  // Percentile
  if _functions.Percentile then
    FPercentile := TGIS_StatisticsItemVariantList.Create( TGIS_StatisticalFunction.Percentile, True )
  else
    FPercentile := TGIS_StatisticsItemVariantList.Create( TGIS_StatisticalFunction.Percentile, False ) ;

  // Range
  if _functions.Range then
    FRange := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Range, True )
  else
    FRange := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Range, False ) ;

  // Sample
  if _functions.Sample then
    FSample := TGIS_StatisticsItemVariantList.Create( TGIS_StatisticalFunction.Sample, True )
  else
    FSample:= TGIS_StatisticsItemVariantList.Create( TGIS_StatisticalFunction.Sample, False ) ;

  // StandardDeviation
  if _functions.StandardDeviation then
    FStandardDeviation := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.StandardDeviation, True )
  else
    FStandardDeviation := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.StandardDeviation, False ) ;

  // Sum
  if _functions.Sum then
    FSum := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Sum,True )
  else
    FSum := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Sum, False ) ;

  // Variance
  if _functions.Variance then
    FVariance := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Variance, True )
  else
    FVariance := TGIS_StatisticsItemDouble.Create( TGIS_StatisticalFunction.Variance, False ) ;

  // Variety
  if _functions.Variety then
    FVariety := TGIS_StatisticsItemInteger.Create( TGIS_StatisticalFunction.Variety, True )
  else
    FVariety := TGIS_StatisticsItemInteger.Create( TGIS_StatisticalFunction.Variety, False ) ;

  // Unique
  if _functions.Unique then
    FUnique := TGIS_StatisticsItemVariantList.Create( TGIS_StatisticalFunction.Unique, True )
  else
    FUnique := TGIS_StatisticsItemVariantList.Create( TGIS_StatisticalFunction.Unique, False ) ;

  // find needed values
  determineNeededVariables ;
end;

procedure TGIS_StatisticsResult.determineNeededVariables ;
begin
  needAverage := checkIfNeedAverage ;
  needTotalVariance  := checkIfNeedTotalVariance ;
  needMin := checkIfNeedMin ;
  needMax := checkIfNeedMax ;
  needValues := checkIfNeedValues ;
  needStrings := checkIfNeedStrings ;
end;

procedure TGIS_StatisticsResult.initializeInternalStatistics ;
begin
  internalAverage := 0 ;
  internalCount := 0 ;
  internalCountMissings := 0 ;
  internalMax := -GIS_MAX_DOUBLE ;
  internalMin := GIS_MAX_DOUBLE ;
  internalSum := 0.0 ;
  totalVariance := 0.0 ;
  resampleRatio := 1.0 ;
end;

procedure TGIS_StatisticsResult.doDestroy;
begin
  FreeObject( FAverage ) ;
  FreeObject( FCount ) ;
  FreeObject( FCountMissings ) ;
  FreeObject( FMajority ) ;
  FreeObject( FMax ) ;
  FreeObject( FMedian ) ;
  FreeObject( FMin ) ;
  FreeObject( FMinority ) ;
  FreeObject( FPercentile ) ;
  FreeObject( FRange ) ;
  FreeObject( FSample ) ;
  FreeObject( FStandardDeviation ) ;
  FreeObject( FSum ) ;
  FreeObject( FVariance ) ;
  FreeObject( FVariety ) ;
  FreeObject( FUnique ) ;

  FreeObject( valuesLst ) ;
  FreeObject( stringsDct ) ;

  FreeObject( busyEventManager ) ;

  inherited ;
end;

procedure TGIS_StatisticsResult.setResampleRatio ( const _ratio : Double ) ;
begin
  resampleRatio := _ratio ;
end;

procedure TGIS_StatisticsResult.incrementCountMissings ;
begin
  inc( internalCountMissings ) ;
end;

procedure TGIS_StatisticsResult.Update(
  const _value : Variant
) ;
var
  variant_bool            : Boolean ;
  variant_value           : Double ;
  variant_string          : String ;
  force_increment_missing : Boolean ;
begin
  force_increment_missing := False ;
  variant_value := 0 ;

   if VarIsEmpty( _value ) or VarIsNull( _value ) then begin
    force_increment_missing := True ;
  end
  else begin
    // extract variant value
    case  FDatasetType of
      // case: numeric (including date) fields
      TGIS_FieldType.Float,
      TGIS_FieldType.Number,
      TGIS_FieldType.Date : begin
        variant_value := VarToDouble( _value ) ;

        // check ignored value
        if IsNan( variant_value ) or
           ( isIgnoreValue and GisIsSameValue( variant_value, doubleIgnoreValue ) )
        then
          force_increment_missing := True ;
      end ;

      // case: string fields
      TGIS_FieldType.String : begin
        variant_string := VarToString( _value ) ;

        if isIgnoreValue and
           ( variant_string = stringIgnoreValue ) then begin
          force_increment_missing := True ;
        end
        else begin
          updateStrings( variant_string ) ;
          variant_value := length( variant_string ) ;
        end ;
      end ;

      // case: boolean fields
      TGIS_FieldType.Boolean : begin
        variant_bool := VarToBoolean( _value ) ;

        if isIgnoreValue and ( variant_bool = boolIgnoreValue ) then
          force_increment_missing := True
        else
          variant_value := ord( variant_bool ) ;
      end ;
    else
      force_increment_missing := True ;
    end ;
  end ;

  if force_increment_missing then
    incrementCountMissings
  else
    updateValues( variant_value ) ;
end;

procedure TGIS_StatisticsResult.updateValues(
  const _value : Double
) ;
begin
  inc( internalCount ) ;

  internalSum := internalSum + _value ;

  // Welford's method to avoid catastrophic cancellation
  if needAverage then begin
    deltaOld := _value - internalAverage ;
    internalAverage := internalAverage + deltaOld / internalCount ;
  end ;

  if needTotalVariance then begin
    deltaNew := _value - internalAverage ;
    totalVariance := totalVariance + deltaOld * deltaNew ;
  end ;

  if needMin then
    assignMin( _value ) ;

  if needMax then
    assignMax( _value ) ;

  if needValues then begin
    // limit the number of processed values in FastStatistics mode
    if ( valuesLst.Count > valuesLimit ) then
      exit ;

    valuesLst.Add( _value ) ;
  end ;
end;

procedure TGIS_StatisticsResult.updateStrings(
  const _string : String
) ;
var
  n_occurrences : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
begin
  if needStrings then begin
    if stringsDct.TryGetValue( _string, n_occurrences ) then
      stringsDct.AddOrSetValue( _string, n_occurrences + 1 )
    else
      stringsDct.Add( _string, 1 ) ;
  end ;
end;

function TGIS_StatisticsResult.checkIfNeedAverage : Boolean ;
begin
  Result := Average.Requested or
            StandardDeviation.Requested or
            Variance.Requested ;
end;

function TGIS_StatisticsResult.checkIfNeedTotalVariance : Boolean ;
begin
  Result := StandardDeviation.Requested or
            Variance.Requested ;
end;

function TGIS_StatisticsResult.checkIfNeedMin : Boolean ;
begin
  Result := Min.Requested or
            Range.Requested ;
end;

function TGIS_StatisticsResult.checkIfNeedMax : Boolean ;
begin
  Result := Max.Requested or
            Range.Requested ;
end;

function TGIS_StatisticsResult.checkIfNeedValues : Boolean ;
begin
  Result := ( DatasetType <> TGIS_FieldType.String ) and
            ( Majority.Requested or
              Minority.Requested or
              Median.Requested or
              Percentile.Requested or
              Sample.Requested or
              Unique.Requested or
              Variety.Requested ) ;
end;

function TGIS_StatisticsResult.checkIfNeedStrings : Boolean ;
begin
  Result := ( DatasetType = TGIS_FieldType.String ) and
            ( Unique.Requested or
              Variety.Requested or
              Sample.Requested ) ;
end;

procedure TGIS_StatisticsResult.assignMin(
  const _value : Double
) ;
begin
  if _value < internalMin then
    internalMin := _value ;
end;

procedure TGIS_StatisticsResult.assignMax(
  const _value : Double
) ;
begin
  if _value > internalMax then
    internalMax := _value ;
end;

procedure TGIS_StatisticsResult.Complete ;
begin
  doComplete ;
end;

function TGIS_StatisticsResult.doComplete : Boolean ;
const
  // 1.Count, 2.CountMissings, 3.Sum, 4.Average, 5.Variance, 6.Std.dev.
  // 7.Min, 8.Max, 9.Range, 10.Sample, 11.Median, 12.Percentile, 13.Variety,
  // 14.internal loop (Unique, Minority, Majority), 15.Minority, 16.Majority
  N_STEPS = 16 ;
var
  abrt : Boolean ;
begin
  Result := False ;
  valuesSorted := False ;

  busyEventManager.StartEvent( Name, N_STEPS ) ;
  try
    abrt := busyEventManager.PushEvent ;
    if Count.Requested then begin
      Count.FValue := RoundS( internalCount / resampleRatio ) ;
      Count.FCalculated := True ;
    end ;

    abrt := busyEventManager.PushEvent ;
    if CountMissings.Requested then begin
      CountMissings.FValue := RoundS( internalCountMissings / resampleRatio ) ;
      CountMissings.FCalculated := True ;
    end ;

    // if there is no valid values don't fill next variables
    if internalCount = 0 then
      Exit ;

    abrt := busyEventManager.PushEvent ;
    if Sum.Requested then begin
      Sum.FValue := internalSum / resampleRatio ;
      Sum.FCalculated := True ;
    end ;

    abrt := busyEventManager.PushEvent ;
    if Average.Requested then begin
      Average.FValue := internalAverage ;
      Average.FCalculated := True ;
    end ;

    abrt := busyEventManager.PushEvent ;
    if Variance.Requested then begin
      Variance.FValue := calcVariance ;
      Variance.FCalculated := True ;
    end ;

    // dependent on Variance
    abrt := busyEventManager.PushEvent ;
    if StandardDeviation.Requested then begin
      StandardDeviation.FValue := calcStandardDeviation ;
      StandardDeviation.FCalculated := True ;
    end ;

    abrt := busyEventManager.PushEvent ;
    if Max.Requested then begin
      Max.FValue := internalMax ;
      Max.FCalculated := True ;
    end ;

    abrt := busyEventManager.PushEvent ;
    if Min.Requested then begin
      Min.FValue := internalMin ;
      Min.FCalculated := True ;
    end ;

    // dependent on Min & Max
    abrt := busyEventManager.PushEvent ;
    if Range.Requested then begin
      Range.FValue := calcRange ;
      Range.FCalculated := True ;
    end ;

    // busy event inside function
    // generate samples before sorting values
    if Sample.Requested then begin
      abrt := generateSamples ;
      Sample.FCalculated := True ;
    end
    else
      abrt := busyEventManager.PushEvent ;

    abrt := busyEventManager.PushEvent ;
    if Median.Requested then begin
      Median.FValue := calcMedian ;
      Median.FCalculated := True ;
    end ;

    abrt := busyEventManager.PushEvent ;
    if Percentile.Requested then begin
      calcPercentiles ;
      Percentile.FCalculated := True ;
    end ;

    // busy events inside function
    if Unique.Requested or
       Variety.Requested or
       Majority.Requested or
       Minority.Requested
    then
      abrt := processOccurrences ;

    if abrt then begin
      Result := True ;
      exit ;
    end ;
  finally
    cleanUp ;
    busyEventManager.EndEvent ;
  end ;
end;

function TGIS_StatisticsResult.calcVariance : Double ;
begin
  if UseBesselCorrection then  // sample variance
    Result := totalVariance / ( internalCount - 1 )
  else  // population variance
    Result := totalVariance / internalCount ;
end;

function TGIS_StatisticsResult.calcStandardDeviation : Double ;
begin
  if IsNan( Variance.Value ) then
    Result := Sqrt( calcVariance )
  else
    Result := Sqrt( Variance.Value ) ;
end;

function TGIS_StatisticsResult.calcRange : Double ;
begin
  Result := internalMax - internalMin ;
end;

//? TODO: Consider implementing O(n) algorithm in future
// (quick search or median of medians; duplicates must be permitted!)
function TGIS_StatisticsResult.calcMedian : Double ;
var
  middleIndex  : Integer ;
  values_count : Integer ;
begin
  Result := NaN ;

  // Median does not exist for strings
  if ( DatasetType = TGIS_FieldType.String ) then
    exit ;

  // Median will be determined from limited number of elements
  values_count := valuesLst.Count ;

  sortValues ;

  middleIndex := values_count div 2 ;

  if Odd( values_count ) then
    Result := valuesLst[middleIndex]
  else
    Result := ( valuesLst[middleIndex-1] + valuesLst[middleIndex] ) / 2 ;
end;

procedure TGIS_StatisticsResult.calcPercentiles ;
var
  i           : Integer ;
  percentages : TArray< Double > ;

  function assign_params : TArray< Double > ;
  var
    do_raise   : Boolean ;
    i_param    : Integer ;
    param      : Variant ;
    percentage : Double ;
  begin
    do_raise := False ;

    SetLength( Result, length( Percentile.Params ) ) ;
    for i_param := 0 to length( Percentile.Params )-1 do begin
      param := Percentile.Params[i_param] ;

      case GetVariantType( param ) of
        TGIS_VariantType.Float,
        TGIS_VariantType.Int,
        TGIS_VariantType.UInt : begin
          percentage := VarToDouble( param ) ;
          if ( percentage < 0 ) or ( percentage > 100 ) then
            do_raise := True
          else
            Result[i_param] := percentage ;
        end
        else
          do_raise := True ;
          raise EGIS_Exception.Create(
            _rsrc( GIS_RS_ERR_BADPARAM ),
            Format( '[%s] Percentile.Params', [Name] ),
            0
          ) ;
      end ;

      if do_raise then begin
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_BADPARAM ),
          Format( '[%s] Percentile.Params', [Name] ),
          0
        ) ;
      end;
    end ;
  end ;
begin
  FPercentile.FValues.Clear ;

  percentages := assign_params ;

  // Percentile does not exist for strings
  if ( DatasetType = TGIS_FieldType.String ) then
    exit ;

  sortValues ;
  for i := 0 to length( percentages )-1 do
    FPercentile.FValues.Add( calcPercentile( percentages[i] ) ) ;
end;

// method from Hyndman and Fan (1996) article in an American Statistician
// used by U.S. National Institute of Standards and Technology and MS Excel
function TGIS_StatisticsResult.calcPercentile(
  const _percentage : Double
) : Double ;
var
  i            : Integer ;
  values_count : Integer ;
  f            : Double ;
  delta        : Double ;
  rank         : Double ;
begin
  // Median will be determined from limited number of elements
  values_count := valuesLst.Count ;

  // only values from range 0..1 are permitted
  f := GisDMax( 0, _percentage / 100 ) ;
  f := GisDMin( 1, f ) ;

  rank := ( values_count-1 ) * f ;
  i := FloorS( rank ) ;
  delta := rank - i ;

  if ( i = values_count-1 ) then
    Result := valuesLst[i]
  else
    Result := ( 1 - delta ) * valuesLst[i] + delta * valuesLst[i+1] ;
end;

function TGIS_StatisticsResult.generateSamples : Boolean ;
var
  size           : Integer ;
  elements_count : Integer ;
  replacement    : Boolean ;
  strings_sample : Boolean ;
  strings_arr    : TArray< String > ;
  sampler_iter        : T_RandomSamplingIterator ;

  // validate and assign 'Sample' params
  procedure assign_params(
    const _size        : Integer ;
    const _replacement : Boolean
  ) ;
  var
    do_raise : Boolean ;
  begin
    do_raise := False ;

    try
      size := VarToInt32( Sample.Params[0] ) ;
      replacement := VarToBoolean( Sample.Params[1] ) ;
      if ( size < 0 ) then
        do_raise := True ;
    except
      do_raise := True ;
    end ;

    if do_raise then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ),
        Format( '[%s] Sample.Params', [Name]),
        0
      ) ;
  end ;

  // stringsDct -> array of strings
  procedure prepare_strings_array( const _count : Integer ) ;
  var
    i, e     : Integer ;
    key      : String ;
    {$IFDEF DCC}
      elem   : TPair< String, Integer > ;
    {$ENDIF}
  begin
    i := 0 ;
    SetLength( strings_arr, _count ) ;
    for elem in stringsDct do begin
      key := elem.Key ;
      for e := 0 to elem.Value - 1 do begin
        strings_arr[i] := key ;
        inc( i ) ;
      end ;
    end ;
  end ;

begin
  Result := False ;
  {$IFDEF DCC}
    Randomize ;
  {$ENDIF}

  assign_params( size, replacement ) ;

  // Sample will be determined only from collected elements
  // valuesLst or stringsDct may not contains all elements for very big datasets
  // (internalCount <> valuesLst.Count or stringsDct.Count)
  elements_count := internalCount ;

  if DatasetType = TGIS_FieldType.String then begin
    strings_sample := True ;
    prepare_strings_array( elements_count ) ;
    //? test big string dataset
  end
  else begin
    strings_sample := False ;
    elements_count := {$IFDEF DCC}System.{$ENDIF}Math.Min( elements_count, valuesLst.Count ) ;
  end ;

  Sample.FValues.Clear ;

  busyEventManager.StartEvent( T_FUNCTIONS.SAMPLE, size ) ;
  try
    sampler_iter := T_RandomSamplingIterator.Create( size, elements_count, replacement ) ;
    try
      while sampler_iter.Next do begin
        if busyEventManager.PushEvent then begin
          Sample.FValues.Clear ;
          Result := True ;
          exit ;
        end ;

        if strings_sample then
          Sample.FValues.Add( strings_arr[sampler_iter.CurrentItem] )
        else
          Sample.FValues.Add( valuesLst[sampler_iter.CurrentItem] ) ;
      end ;
    finally
      FreeObject( sampler_iter ) ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end ;
end;

procedure TGIS_StatisticsResult.sortValues ;
begin
  if not valuesSorted then begin
    {$IFDEF JAVA}
      java.util.Collections.sort( valuesLst ) ;
    {$ELSE}
      {$IFDEF ISLAND}
        valuesLst.Sort( (_p1,_p2)-> begin
          if      _p1 > _p2 then Result := 1
          else if _p1 < _p2 then Result :=-1
          else                   Result := 0 ;
        end
        ) ;
      {$ELSE}
      valuesLst.Sort ;
      {$ENDIF}
    {$ENDIF}
    valuesSorted := True ;
  end ;
end;

function TGIS_StatisticsResult.processOccurrences : Boolean ;
var
  unique_limit : Integer ;
  // validate and assign 'Unique' params
  function assign_params : Integer ;
  var
    do_raise : Boolean ;
  begin
    Result := UNIQUE_STAT_LIMIT ;
    do_raise := False ;

    try
      Result := VarToInt32( Unique.Params[0] ) ;
      if ( Result < 0 ) then
        do_raise := True ;
    except
      do_raise := True ;
    end ;

    if do_raise then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_BADPARAM ),
        Format( '[%s] Unique.Params', [Name]),
        0
      ) ;
  end ;

begin
  unique_limit := assign_params ;

  case DatasetType of
    TGIS_FieldType.String : Result := processStringOccurrences( unique_limit ) ;
  else
    Result := processValueOccurrences( unique_limit ) ;
  end ;
end;

function TGIS_StatisticsResult.processStringOccurrences(
 const _uniqueLimit : Integer
) : Boolean ;
var
  current_string       : String ;
  majority_string      : String ;
  minority_string      : String ;
  string_count         : Integer ;
  majority_count       : Integer ;
  minority_count       : Integer ;
  no_clear_majority    : Boolean ;
  no_clear_minority    : Boolean ;
  {$IFDEF DCC}
    pair : TPair< String, Integer > ;
  {$ENDIF}
begin
  Result := False ;

  if FUnique.FRequested then
    FUnique.FValues.Clear ;

  majority_count := 0 ;
  minority_count := GIS_MAX_INTEGER ;
  no_clear_majority := True ;
  no_clear_minority := True ;

  busyEventManager.StartEvent(
    _rsrc( GIS_RS_BUSY_STATISTICS_OCCURRENCES ), stringsDct.Count
  ) ;
  try
    for pair in stringsDct do begin
      if busyEventManager.PushEvent then begin
        Result := True ;
        exit ;
      end ;

      current_string := pair.Key ;
      string_count := pair.Value ;

      // Unique part
      if FUnique.FRequested and ( FUnique.FValues.Count < _uniqueLimit ) then
        FUnique.FValues.Add( current_string ) ;

      // Majority part
      if Majority.Requested then begin
        if string_count > majority_count then begin
          majority_count := string_count ;
          majority_string := current_string ;
          no_clear_majority := False ;
        end
        else if string_count = majority_count then begin
          no_clear_majority := True ;
        end ;
      end ;

      // Minority part
      if Minority.Requested then begin
        if string_count < minority_count then begin
          minority_count := string_count ;
          minority_string := current_string ;
          no_clear_minority := False ;
        end
        else if string_count = minority_count then begin
          no_clear_minority := True ;
        end ;
      end ;
    end ;
  finally
    busyEventManager.EndEvent ;
  end;

  if FUnique.Requested then
    FUnique.FCalculated := True ;

  busyEventManager.PushEvent ;
  if FVariety.FRequested then begin
    FVariety.FValue := stringsDct.Count ;
    FVariety.FCalculated := True ;
  end ;

  busyEventManager.PushEvent ;
  if FMajority.FRequested then begin
    FMajority.FCalculated := True ;
    if no_clear_majority then
      FMajority.FValue := NullVar
    else
      FMajority.FValue := majority_string ;
  end ;

  busyEventManager.PushEvent ;
  if FMinority.FRequested then begin
    FMinority.FCalculated := True ;
    if no_clear_minority then
      FMinority.FValue := NullVar
    else
      FMinority.FValue := minority_string ;
  end ;
end;

function TGIS_StatisticsResult.processValueOccurrences(
 const _uniqueLimit : Integer
) : Boolean ;
var
  i, idx         : Integer ;
  val            : Double ;
  minority_value : Double ;
  majority_value : Double ;
  variety_value  : Integer ;
  minority_count : Integer ;
  majority_count : Integer ;
  current_count  : Integer ;
  values_count   : Integer ;
begin
  Result := False ;

  majority_value := NaN ;
  minority_value := NaN ;

  variety_value := 0 ;

  if FUnique.FRequested then
    FUnique.FValues.Clear ;

  values_count := valuesLst.Count ;

  // Majority, Minority, Unique, and Variety can be determined
  // only if valuesLst contains some elements
  if ( values_count > 0 ) then begin
    sortValues ;

    val := valuesLst[0] ;

    if FUnique.FRequested and ( FUnique.FValues.Count < _uniqueLimit ) then
      FUnique.FValues.Add( val ) ;

    if FVariety.FRequested  then
      inc( variety_value ) ;

    current_count := 1 ;
    minority_count := GIS_MAX_INTEGER ;
    majority_count := 0 ;
    minority_value := val ;
    majority_value := val ;

    // start from 2nd element
    // not Count-1 -> possible last two values may be majority
    busyEventManager.StartEvent(
      _rsrc( GIS_RS_BUSY_STATISTICS_OCCURRENCES ), values_count-1
    ) ;
    try
      for i := 1 to values_count do begin
        if busyEventManager.PushEvent then begin
          Result := True ;
          exit ;
        end ;

        if i = values_count then
          idx := 0
        else
          idx := i ;

        if not GisIsSameValue( valuesLst[idx], val, GIS_DOUBLE_RESOLUTION ) then begin

          // Majority part
          if FMajority.Requested then begin
            if current_count > majority_count then begin
              majority_count := current_count ;
              majority_value := val ;
            end
            else if current_count = majority_count then begin
              majority_value := NaN ;
            end ;
          end ;

          // Minority part
          if FMinority.Requested then begin
            if current_count < minority_count then begin
              minority_count := current_count ;
              minority_value := val ;
            end
            else if current_count = minority_count then begin
              minority_value := NaN ;
            end ;
          end ;

          if idx = 0 then
            break ;

          val := valuesLst[idx] ;
          current_count := 1 ;  // reset current count (no of occurrences)

          // Unique part
          if FUnique.FRequested and ( FUnique.FValues.Count < _uniqueLimit ) then
            FUnique.FValues.Add( val ) ;

          // Variety part
          if FVariety.FRequested  then
            inc( variety_value ) ;
        end
        else begin
          inc( current_count ) ;
        end ;
      end ;
    finally
      busyEventManager.EndEvent ;
    end ;
  end ;

  if FUnique.FRequested then
    FUnique.FCalculated := True ;

  busyEventManager.PushEvent ;
  if FVariety.FRequested then begin
    FVariety.FValue := variety_value ;
    FVariety.FCalculated := True ;
    end ;

  busyEventManager.PushEvent ;
  if FMajority.FRequested then begin
    FMajority.FValue := majority_value ;
    FMajority.FCalculated := True ;
  end ;

  busyEventManager.PushEvent ;
  if FMinority.FRequested then begin
    FMinority.FValue := minority_value ;
    FMinority.FCalculated := True ;
  end ;
end;

function TGIS_StatisticsResult.fget_AvailableStatistics
  : TGIS_StatisticsItemList ;
begin
  Result := TGIS_StatisticsItemList.Create ;

  if Average.Calculated then
    Result.Add( Average ) ;

  if Count.Calculated then
    Result.Add( Count ) ;

  if CountMissings.Calculated then
    Result.Add( CountMissings ) ;

  if Majority.Calculated then
    Result.Add( Majority ) ;

  if Max.Calculated then
    Result.Add( Max ) ;

  if Median.Calculated then
    Result.Add( Median ) ;

  if Min.Calculated then
    Result.Add( Min ) ;

  if Minority.Calculated then
    Result.Add( Minority ) ;

  if Percentile.Calculated then
    Result.Add( Percentile ) ;

  if Range.Calculated then
    Result.Add( Range ) ;

  if Sample.Calculated then
    Result.Add( Sample ) ;

  if StandardDeviation.Calculated then
    Result.Add( StandardDeviation ) ;

  if Sum.Calculated then
    Result.Add( Sum ) ;

  if Variance.Calculated then
    Result.Add( Variance ) ;

  if Variety.Calculated then
    Result.Add( Variety ) ;

  if Unique.Calculated then
    Result.Add( Unique ) ;
end;

{$IFDEF CLR}
  procedure TGIS_StatisticsResult.fadd_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent += _value ;
  end ;

  procedure TGIS_StatisticsResult.fremove_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent -= _value ;
  end ;
{$ELSE}
  procedure TGIS_StatisticsResult.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent := _value ;
  end ;

  function TGIS_StatisticsResult.fget_BusyEvent : TGIS_BusyEvent ;
  begin
    Result := busyEventManager.BusyEvent ;
  end ;
{$ENDIF}

procedure TGIS_StatisticsResult.cleanUp ;
begin
  initializeInternalStatistics ;
  valuesLst.Clear ;
  stringsDct.Clear ;
end;

procedure TGIS_StatisticsResult.updateFunctions (
  const _functions : TGIS_StatisticalFunctions
) ;
begin
  // Average
  if not FAverage.Calculated then
    FAverage.FRequested := FAverage.FRequested or _functions.Average ;
  // Count
  if not FCount.Calculated then
    FCount.FRequested := FCount.FRequested or _functions.Count ;
  // CountMissings
  if not FCountMissings.Calculated then
    FCountMissings.FRequested := FCountMissings.FRequested or _functions.CountMissings ;
  // Majority
  if not FMajority.Calculated then
    FMajority.FRequested := FMajority.FRequested or _functions.Majority ;
  // Max
  if not FMax.Calculated then
    FMax.FRequested := FMax.FRequested or _functions.Max ;
  // Median
  if not FMedian.Calculated then
    FMedian.FRequested := FMedian.FRequested  or _functions.Median ;
  // Min
  if not FMin.Calculated then
    FMin.FRequested := FMin.FRequested or _functions.Min ;
  // Minority
  if not FMinority.Calculated then
    FMinority.FRequested := FMinority.FRequested  or _functions.Minority ;
  // Percentile
  if not FPercentile.Calculated then
    FPercentile.FRequested := FPercentile.FRequested or _functions.Percentile ;
  // Range
  if not FRange.Calculated then
    FRange.FRequested := FRange.FRequested or _functions.Range ;
  // Sample
  if not FSample.Calculated then
    FSample.FRequested := FSample.FRequested or _functions.Sample ;
  // StandardDeviation
  if not FStandardDeviation.Calculated then
    FStandardDeviation.FRequested := FStandardDeviation.FRequested  or _functions.StandardDeviation ;
  // Sum
  if not FSum.Calculated then
    FSum.FRequested := FSum.FRequested or _functions.Sum ;
  // Unique
  if not FUnique.Calculated then
    FUnique.FRequested := FUnique.FRequested or _functions.Unique ;
  // Variance
  if not FVariance.Calculated then
    FVariance.FRequested := FVariance.FRequested or _functions.Variance ;
  // Variety
  if not FVariety.Calculated then
    FVariety.FRequested := FVariety.FRequested or _functions.Variety ;

  determineNeededVariables ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsLayerResult'}
function  TGIS_StatisticsLayerResult.fget_UseBesselCorrection : Boolean ;
var
  lr : TGIS_Layer ;
begin
  lr := TGIS_Layer( FLayer ) ;

  if assigned( lr ) and assigned( lr.Statistics ) then
    Result := lr.Statistics.UseBesselCorrection
  else
    Result := inherited ;
end;

procedure TGIS_StatisticsLayerResult.fset_UseBesselCorrection(
  const _value : Boolean
) ;
var
  lr : TGIS_Layer ;
begin
  lr := TGIS_Layer( FLayer ) ;

  // if layer is assigned, property is read-only
  if assigned( lr ) and assigned( lr.Statistics ) then
    exit ;

  inherited ;
end;

constructor TGIS_StatisticsLayerResult.Create(
  const _layer        : TGIS_LayerAbstract ;
  const _dataset_type : TGIS_FieldType ;
  const _name         : String ;
  const _functions    : TGIS_StatisticalFunctions ;
  const _ignore_value : Variant
) ;
begin
  inherited Create( _dataset_type, _name, _functions, _ignore_value ) ;
  FLayer := _layer ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsItem'}
constructor TGIS_StatisticsItem.Create (
  const _function  : TGIS_StatisticalFunction ;
  const _requested : Boolean
) ;
begin
  FCalculated := False ;
  FRequested := _requested ;
  FName := getFunctionName( _function ) ;
  FParams := getDefaultParams( _function ) ;
  statFunction := _function ;
end;

function TGIS_StatisticsItem.getFunctionName(
  const _function : TGIS_StatisticalFunction
) : String ;
begin
  case _function of
    TGIS_StatisticalFunction.Average :
      Result := T_FUNCTIONS.AVERAGE ;
    TGIS_StatisticalFunction.Count :
      Result := T_FUNCTIONS.COUNT ;
    TGIS_StatisticalFunction.CountMissings :
      Result := T_FUNCTIONS.COUNT_MISSINGS ;
    TGIS_StatisticalFunction.Max :
      Result := T_FUNCTIONS.MAX ;
    TGIS_StatisticalFunction.Majority :
      Result := T_FUNCTIONS.MAJORITY ;
    TGIS_StatisticalFunction.Median :
      Result := T_FUNCTIONS.MEDIAN ;
    TGIS_StatisticalFunction.Min :
      Result := T_FUNCTIONS.MIN ;
    TGIS_StatisticalFunction.Minority :
      Result := T_FUNCTIONS.MINORITY ;
    TGIS_StatisticalFunction.Percentile :
      Result := T_FUNCTIONS.PERCENTILE ;
    TGIS_StatisticalFunction.Range :
      Result := T_FUNCTIONS.RANGE ;
    TGIS_StatisticalFunction.Sample :
      Result := T_FUNCTIONS.SAMPLE ;
    TGIS_StatisticalFunction.StandardDeviation :
      Result := T_FUNCTIONS.STANDARD_DEVIATION ;
    TGIS_StatisticalFunction.Sum :
      Result := T_FUNCTIONS.SUM ;
    TGIS_StatisticalFunction.Variance :
      Result := T_FUNCTIONS.VARIANCE ;
    TGIS_StatisticalFunction.Variety :
      Result := T_FUNCTIONS.VARIETY ;
    TGIS_StatisticalFunction.Unique :
      Result := T_FUNCTIONS.UNIQUE ;
    else
      Result := '' ;
  end ;
end;

function TGIS_StatisticsItem.getDefaultParams(
  const _function : TGIS_StatisticalFunction
) : TGIS_VariantArray ;
begin
  case _function of
    TGIS_StatisticalFunction.Percentile : begin
      SetLength( Result, 3 ) ;
      Result[0] := PERCENTILE_25 ;
      Result[1] := PERCENTILE_50 ;
      Result[2] := PERCENTILE_75 ;
    end ;
    TGIS_StatisticalFunction.Sample : begin
      SetLength( Result, 2 ) ;
      Result[0] := SAMPLE_SIZE ;
      Result[1] := SAMPLE_REPLACEMENT ;
    end ;
    TGIS_StatisticalFunction.Unique : begin
      SetLength( Result, 1 ) ;
      Result[0] := UNIQUE_STAT_LIMIT ;
    end ;
  end ;
end ;

procedure TGIS_StatisticsItem.setDefaultParams ;
begin
  Params := getDefaultParams( statFunction ) ;
end;

function TGIS_StatisticsItem.variantToString(
  const _variant : Variant
) : String ;
var
  dbl : Double ;
begin
  if VarIsNull( _variant ) then begin
    Result := 'NAN' ;
  end
  else begin
    case GetVariantType( _variant ) of
      TGIS_VariantType.Float      : begin
        dbl := VarToDouble( _variant ) ;
        if not IsNan( dbl ) then Result := DotFloatToStr( dbl ) else Result := 'NAN' ;
      end ;
      TGIS_VariantType.WideString,
      TGIS_VariantType.AnsiString : Result := VarToString( _variant ) ;
      TGIS_VariantType.Int        : Result := IntToStr( VarToInt32( _variant ) );
      TGIS_VariantType.DateTime   : DateTimeToString(
                                      Result, 'ddddd', VarToDateTime ( _variant )
                                    ) ;
    else
      Result := '' ;
    end ;
  end ;
end;

procedure TGIS_StatisticsItem.fset_Params(
  const _value : TGIS_VariantArray
) ;
var
  i                          : Integer ;
  variants_are_equal         : Boolean ;
  var_type_new, var_type_old : TGIS_VariantType ;
begin
  variants_are_equal := False ;
  // comparing arrays of variants to decide if statistics request changed
  try
    variants_are_equal := length( _value ) = length( FParams ) ;
    if not variants_are_equal then
      exit ;

    for i := low( _value ) to high( _value ) do begin
      variants_are_equal := False ;

      var_type_new := GetVariantType( _value[i] ) ;
      var_type_old := GetVariantType( FParams[i] ) ;

      // variants must have the same type
      if ( ( var_type_new = TGIS_VariantType.Int ) or
           ( var_type_new = TGIS_VariantType.UInt ) ) and
         ( ( var_type_old = TGIS_VariantType.Int ) or
           ( var_type_old = TGIS_VariantType.UInt ) ) then
      begin
        // Int and UInt treated as the same type
      end
      else if ( ( var_type_new = TGIS_VariantType.Int64 ) or
                ( var_type_new = TGIS_VariantType.UInt64 ) ) and
              ( ( var_type_old = TGIS_VariantType.Int64 ) or
                ( var_type_old = TGIS_VariantType.UInt64 ) ) then
      begin
        // Int64 and UInt64 treated as the same type
      end
      else if ( ( var_type_new = TGIS_VariantType.AnsiString ) or
                ( var_type_new = TGIS_VariantType.WideString) ) and
              ( ( var_type_old = TGIS_VariantType.AnsiString ) or
                ( var_type_old = TGIS_VariantType.WideString ) ) then
      begin
        // AnsiString and WideString treated as the same type
      end
      else if ( ( var_type_new = TGIS_VariantType.Float ) or
                ( var_type_new = TGIS_VariantType.Fixed) ) and
              ( ( var_type_old = TGIS_VariantType.Float) or
                ( var_type_old = TGIS_VariantType.Fixed) ) then
      begin
        // Float and Fixed treated as the same type
      end
      else if GetVariantType( _value[i] ) <> GetVariantType( FParams[i] ) then
        exit ;

      // comparing values converted to their default type
      case GetVariantType( _value[i] ) of
        TGIS_VariantType.Boolean :
          variants_are_equal := ( VarToBoolean( FParams[i] ) = VarToBoolean( _value[i] ) ) ;
        TGIS_VariantType.DateTime :
          variants_are_equal := ( VarToDateTime( FParams[i] ) = VarToDateTime( _value[i] ) ) ;
        TGIS_VariantType.Int,
        TGIS_VariantType.UInt :
          variants_are_equal := ( VarToInt32( FParams[i] ) = VarToInt32( _value[i] ) ) ;
        TGIS_VariantType.Int64,
        TGIS_VariantType.UInt64 :
          variants_are_equal := ( VarToInt64( FParams[i] ) = VarToInt64( _value[i] ) ) ;
        TGIS_VariantType.Float,
        TGIS_VariantType.Fixed :
          variants_are_equal := ( VarToDouble( FParams[i] ) = VarToDouble( _value[i] ) ) ;
        TGIS_VariantType.AnsiString,
        TGIS_VariantType.WideString :
          variants_are_equal := ( VarToString( FParams[i] ) = VarToString( _value[i] ) ) ;
        else
          variants_are_equal := False ;
      end ;
      if not variants_are_equal then
        exit ;
    end ;
  finally
    if not variants_are_equal then begin
      FCalculated := False ;
      if length( _value ) = 0 then
        FParams := getDefaultParams( statFunction )
      else begin
        SetLength( FParams, length( _value ) ) ;
        for i := low( _value ) to high( _value ) do
          FParams[i] := _value[i] ;
      end ;
    end ;
  end ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsItemDouble'}
constructor TGIS_StatisticsItemDouble.Create (
  const _function  : TGIS_StatisticalFunction ;
  const _requested : Boolean
) ;
begin
  inherited ;
  FValue := NaN ;
end;

function TGIS_StatisticsItemDouble.ToString : String ;
begin
  Result := DotFloatToStr( FValue ) ;
end;

function TGIS_StatisticsItemDouble.asVariant : Variant ;
begin
  Result := FValue ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsItemInteger'}
constructor TGIS_StatisticsItemInteger.Create (
  const _function  : TGIS_StatisticalFunction ;
  const _requested : Boolean
) ;
begin
  inherited ;
  FValue := 0 ;
end;

function TGIS_StatisticsItemInteger.ToString : String ;
begin
  Result := IntToStr( FValue ) ;
end;

function TGIS_StatisticsItemInteger.asVariant : Variant ;
begin
  Result := FValue ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsItemVariant'}
constructor TGIS_StatisticsItemVariant.Create (
  const _function  : TGIS_StatisticalFunction ;
  const _requested : Boolean
) ;
begin
  inherited ;
  FValue := NullVar ;
end;

function TGIS_StatisticsItemVariant.ToString : String ;
begin
  Result := variantToString( FValue ) ;
end;

function TGIS_StatisticsItemVariant.asVariant : Variant ;
begin
  Result := FValue ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsItemVariantList'}
constructor TGIS_StatisticsItemVariantList.Create (
  const _function  : TGIS_StatisticalFunction ;
  const _requested : Boolean
) ;
begin
  inherited ;
  FValues := TGIS_ListOfVariants.Create ;
end;

procedure TGIS_StatisticsItemVariantList.doDestroy ;
begin
  FreeObject( FValues ) ;
  inherited ;
end;

function TGIS_StatisticsItemVariantList.ToString : String ;
var
  builder         : TStringBuilder ;
  {$IFDEF DCC}
  value           : Variant ;
  {$ENDIF}
  list_has_values : Boolean ;
begin
  if FValues.Count = 0 then begin
    Result := 'NAN' ;
    exit ;
  end ;

  list_has_values := False ;
  builder := TStringBuilder.Create ;
  try
    for value in FValues do begin
//      list_has_values := True ;
      builder.Append( variantToString( value ) + VALUE_SEPARATOR ) ;
    end ;

    if builder.Length > 0 then begin
      {$IFDEF JAVA}
        builder.delete( builder.Length-1, 1 ) ;
      {$ELSE}
        builder.Remove( builder.Length-1, 1 ) ;
      {$ENDIF}
    end ;

    Result := builder.ToString ;
  finally
    FreeObject( builder ) ;
  end ;
end;

function TGIS_StatisticsItemVariantList.asVariant : Variant ;
begin
  Result := ToString ;
end;
{$ENDREGION}

{$REGION 'TGIS_StatisticsFactory'}
class function TGIS_StatisticsFactory.CreateStatistics(
  const _layer : TGIS_LayerAbstract
) : TGIS_StatisticsAbstract ;
begin
  if _layer is TGIS_LayerVector then
    Result := TGIS_StatisticsVector.Create( _layer )
  else if _layer is TGIS_LayerPixel then
    Result := TGIS_StatisticsPixel.Create( _layer )
  else
    Result := nil ;
end;
{$ENDREGION}

{$REGION 'T_StatisticsIO'}
class function T_StatisticsIO.parseDouble(
  const _value : OleVariant
) : Double ;
var
  val_str : String ;
begin
  val_str := VarToString( _value ) ;

  if IsStringEmpty( val_str ) or
     ( UpperCase( val_str ) = 'NAN' ) then
    Result := NaN
  else
    Result := DotStrToFloat( val_str ) ;
end;

class function T_StatisticsIO.validatePath(
  const _path                : String ;
  const _ext                 : String ;
  const _include_file_exists : Boolean
) : Boolean ;
var
  validate_ext : Boolean ;
begin
  {$IFDEF OXYGENE}
    validate_ext := _path.EndsWith( _ext ) ;
  {$ELSE}
    validate_ext := EndsStr( _ext, _path ) ;
  {$ENDIF}

  Result := validate_ext and ( not IsServerPath( _path ) ) ;
  if _include_file_exists then
    Result := Result and SafeFileExists( _path ) ;
end ;

//------------------------------------------------------------------------------
// ReadTtkStatistics
//------------------------------------------------------------------------------
class function T_StatisticsIO.ReadTtkStats(
  const _path  : String ;
  const _layer : TGIS_Layer
) : Boolean ;
var
  new_stats : TGIS_StatisticsAbstract ;
  stats     : TGIS_StatisticsAbstract ;
  xml       : TGIS_XMLDocument ;
  node      : IXMLNode ;
  file_err  : Boolean ;

  function parseParams(
    const _node : IXMLNode ;
    const _fun  : TGIS_StatisticalFunction
  ) : TGIS_VariantArray ;
  var
    i            : Integer ;
    token        : TGIS_Tokenizer ;
    params_str   : String ;
    params_count : Integer ;
  begin
    if _node.HasAttribute( ATTRIBUTE_PARAMS ) then
      params_str := VarToString( _node.Attributes[ATTRIBUTE_PARAMS] )
    else
      exit ;

    token := TGIS_Tokenizer.Create ;
    try
      token.ExecuteEx( params_str, VALUE_SEPARATOR ) ;

      params_count := token.Result.Count ;
      if params_count > 0 then begin

        SetLength( Result, params_count ) ;
        case _fun of
          TGIS_StatisticalFunction.Sample : begin
            Result[0] := StrToInt( token.Result[0] ) ;
            if params_count > 1 then
              Result[1] := StrToBool( token.Result[1] ) ;
          end ;
          TGIS_StatisticalFunction.Percentile : begin
            for i := 0 to params_count - 1 do
              Result[i] := DotStrToFloat( token.Result[i] ) ;
          end ;
        end ;
      end ;
    finally
      FreeObject( token ) ;
    end ;
  end ;

  function parseIgnoreValue(
    const _ignore_value : OleVariant ;
    const _dataset_type : TGIS_FieldType
  ) : Variant ;
  begin
    case _dataset_type of
      TGIS_FieldType.String  : Result := VarToString( _ignore_value ) ;
      TGIS_FieldType.Boolean : Result := VarToBoolean( _ignore_value ) ;
    else
      Result := parseDouble( _ignore_value ) ;
    end ;
  end;

  procedure parseVariantList(
    const _node_value   : OleVariant ;
    const _values       : TList< Variant > ;
    const _dataset_type : TGIS_FieldType
  ) ;
  var
    i            : Integer ;
    item         : String ;
    variants_str : String ;
    value        : Variant ;
    token        : TGIS_Tokenizer ;
  begin
    _values.Clear ;

    variants_str := VarToString( _node_value ) ;

    if UpperCase( variants_str ) = 'NAN' then
      exit;

    token := TGIS_Tokenizer.Create ;
    try
      token.ExecuteEx( variants_str, VALUE_SEPARATOR ) ;
      for i := 0 to token.Result.Count - 1 do begin
        item := token.Result[i] ;
        if IsStringEmpty( item ) or ( UpperCase( item ) = 'NAN' ) then begin
          _values.Clear ;
          exit ;
        end ;

        case _dataset_type of
          TGIS_FieldType.Number,
          TGIS_FieldType.Float,
          TGIS_FieldType.Date    : value := DotStrToFloat( item ) ;
          TGIS_FieldType.String  : value := item ;
          TGIS_FieldType.Boolean : value := StrToBool( item ) ;
        end ;

        _values.Add( value ) ;
      end ;
    finally
      FreeObject( token ) ;
    end ;
  end;

  function parseVariant(
    const _value : OleVariant
  ) : Variant ;
  var
    value_str : String ;
  begin
    value_str := VarToString( _value ) ;

    if IsStringEmpty( value_str ) then
      Result := NullVar
    else if UpperCase( value_str ) = 'NAN' then
      Result := NaN
    else
      Result := _value ;
  end;

  function parse_statistic_item(
    const _node         : IXMLNode ;
    const _key          : String ;
    const _stats_result : TGIS_StatisticsLayerResult
  ) : Boolean ;
  begin
    Result := True ;

    if ( _key = T_FUNCTIONS.AVERAGE ) then begin
      _stats_result.Average.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Average.FCalculated := True ;
      _stats_result.Average.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.COUNT ) then begin
      _stats_result.Count.FValue := VarToInt32( _node.NodeValue ) ;
      _stats_result.Count.FCalculated := True ;
      _stats_result.Count.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.COUNT_MISSINGS ) then begin
      _stats_result.CountMissings.FValue := VarToInt32( _node.NodeValue ) ;
      _stats_result.CountMissings.FCalculated := True ;
      _stats_result.CountMissings.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.MAJORITY ) then begin
      _stats_result.Majority.FValue := parseVariant( _node.NodeValue ) ;
      _stats_result.Majority.FCalculated := True ;
      _stats_result.Majority.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.MAX ) then begin
      _stats_result.Max.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Max.FCalculated := True ;
      _stats_result.Max.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.MEDIAN ) then begin
      _stats_result.Median.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Median.FCalculated := True ;
      _stats_result.Median.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.MIN ) then begin
      _stats_result.Min.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Min.FCalculated := True ;
      _stats_result.Min.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.MINORITY ) then begin
      _stats_result.Minority.FValue := parseVariant( _node.NodeValue ) ;
      _stats_result.Minority.FCalculated := True ;
      _stats_result.Minority.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.RANGE ) then begin
      _stats_result.Range.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Range.FCalculated := True ;
      _stats_result.Range.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.STANDARD_DEVIATION ) then begin
      _stats_result.StandardDeviation.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.StandardDeviation.FCalculated := True ;
      _stats_result.StandardDeviation.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.SUM ) then begin
      _stats_result.Sum.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Sum.FCalculated := True ;
      _stats_result.Sum.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.VARIANCE ) then begin
      _stats_result.Variance.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Variance.FCalculated := True ;
      _stats_result.Variance.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.VARIETY ) then begin
      _stats_result.Variety.FValue := VarToInt32( _node.NodeValue ) ;
      _stats_result.Variety.FCalculated := True ;
      _stats_result.Variety.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.UNIQUE ) then begin
      parseVariantList(
        _node.NodeValue,
        _stats_result.Unique.FValues,
        _stats_result.DatasetType
      ) ;
      _stats_result.Unique.FCalculated := True ;
      _stats_result.Unique.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.PERCENTILE ) then begin
      parseVariantList(
        _node.NodeValue,
        _stats_result.Percentile.FValues,
        _stats_result.DatasetType
      ) ;
      _stats_result.Percentile.Params := parseParams(
        _node,
        TGIS_StatisticalFunction.Percentile
      ) ;
      _stats_result.Percentile.FCalculated := True ;
      _stats_result.Percentile.FRequested := True ;
    end
    else if ( _key = T_FUNCTIONS.SAMPLE ) then begin
      parseVariantList(
        _node.NodeValue,
        _stats_result.Sample.FValues,
        _stats_result.DatasetType
      ) ;
      _stats_result.Sample.Params := parseParams(
        _node,
        TGIS_StatisticalFunction.Sample
      ) ;
      _stats_result.Sample.FCalculated := True ;
      _stats_result.Sample.FRequested := True ;
    end
    else
      Result := False ;
  end;

  function parse_statistic_result(
    const _node         : IXMLNode ;
    const _stats_result : TGIS_StatisticsLayerResult
  ) : Boolean ;
  var
    i           : Integer ;
    key         : String ;
    node_item   : IXMLNode     ;
    nodes_lst   : IXMLNodeList ;
    temp_result : Boolean ;
  begin
    Result := False ;

    nodes_lst := _node.ChildNodes ;
    for i := 0 to nodes_lst.Count - 1 do begin
      node_item := nodes_lst[i] ;

      if ( node_item.NodeName <> NODE_STI ) then begin
        file_err := True ;
        exit ;
      end ;

      if node_item.HasAttribute( ATTRIBUTE_KEY ) then
        key := VarToString( node_item.Attributes[ATTRIBUTE_KEY] )
      else begin
        file_err := True ;
        exit ;
      end ;

      temp_result := parse_statistic_item( node_item, key, _stats_result ) ;

      Result := Result or temp_result ;
      if not temp_result then begin
        if file_err then
          exit
        else
          continue ;
      end ;
    end ;

    // set internal params
    _stats_result.determineNeededVariables ;
  end;

  function parse_statistics(
    const _node  : IXMLNode
  ) : Boolean;
  var
    stats_result : TGIS_StatisticsLayerResult ;
    node_result  : IXMLNode     ;
    nodes_lst    : IXMLNodeList ;
    dataset_type : TGIS_FieldType ;
    stats_name   : String ;
    ignore_value : Variant ;
    i            : Integer ;
    temp_result  : Boolean ;
  begin
    Result := False ;

    nodes_lst := _node.ChildNodes ;
    for i := 0 to nodes_lst.Count - 1 do begin
      node_result := nodes_lst[i] ;

      if node_result.NodeName <> NODE_STATISTICS_RESULT then
        exit ;

      // attribute 'name' is obligatory
      if node_result.HasAttribute( ATTRIBUTE_NAME ) then
        stats_name := VarToString( node_result.Attributes[ATTRIBUTE_NAME] )
      else begin
        file_err := True ;
        exit ;
      end ;

      // field may not exist in layer, so we continue rather than exit
      if not new_stats.determineDatasetTypeNoExc( stats_name, dataset_type ) then
        continue ;

      // attribute 'ignore_value' is optional
      if node_result.HasAttribute( ATTRIBUTE_IGNORE_VALUE ) then
        ignore_value := parseIgnoreValue(
          node_result.Attributes[ATTRIBUTE_IGNORE_VALUE],
          dataset_type
        )
      else
        ignore_value := new_stats.getDefaultIgnoreValue ;

      new_stats.Add( stats_name, TGIS_StatisticalFunctions.EmptyStatistics, ignore_value ) ;
      stats_result := new_stats.Get( stats_name ) ;

      temp_result := parse_statistic_result( node_result, stats_result ) ;
      Result := Result or temp_result ;
      if not temp_result then begin
        new_stats.remove( stats_name ) ;
        if file_err then
          exit
        else
          continue ;
      end ;
    end ;

    new_stats.makeDefinedResultsAvailable ;
  end ;

  function parse_tatukgis(
    const _node : IXMLNode
  ) : Boolean ;
  var
    nodes_lst  : IXMLNodeList ;
    node_stats : IXMLNode ;
  begin
    Result := False ;

    nodes_lst := _node.ChildNodes ;
    if nodes_lst.Count > 0 then begin
      node_stats := nodes_lst[0] ;
      if ( node_stats.NodeName = NODE_STATISTICS ) then begin
        // time stamp
        if node_stats.HasAttribute( ATTRIBUTE_DATE ) then
          new_stats.FAge := XMLStringToDateTime(
            VarToString( node_stats[ATTRIBUTE_DATE] )
          )
        else
          new_stats.FAge := GisFileAge( _path ) ;

        // attribute 'use_bessel_correction' is optional
        if node_stats.HasAttribute( ATTRIBUTE_USE_BESSEL_CORRECTION ) then
          new_stats.UseBesselCorrection := VarToBoolean(
            node_stats.Attributes[ATTRIBUTE_USE_BESSEL_CORRECTION]
          )
        else
          new_stats.UseBesselCorrection := stats.UseBesselCorrection ;

        Result := parse_statistics( node_stats ) ;
      end ;
    end ;
  end;

begin
  Result := False ;
  try
    file_err := False ;

    if not validatePath( _path, GIS_TTKSTATS_EXT, True ) then
      exit ;

    stats := _layer.Statistics ;
    new_stats := TGIS_StatisticsFactory.CreateStatistics( _layer ) ;
    try
      xml := TGIS_XMLDocument.Create ;
      try
        xml.LoadFromFile( _path ) ;

        node := xml.DocumentElement ;

        if not assigned( node ) then
          exit ;

        if ( node.NodeName = NODE_TATUKGIS ) then
          Result := parse_tatukgis( node ) ;
      finally
        FreeObject( xml ) ;
      end ;
    finally
      if Result then
        _layer.Statistics := new_stats
      else
        FreeObject( new_stats ) ;
    end ;
  except
    Result := False ;
  end ;
end;

//------------------------------------------------------------------------------
// SaveTtkStatistics
//------------------------------------------------------------------------------
class procedure T_StatisticsIO.SaveTtkStats(
  const _path  : String ;
  const _layer : TGIS_Layer
) ;
var
  stats : TGIS_StatisticsAbstract ;
  xml   : TGIS_XMLDocument ;
  node  : IXMLNode ;

  function parse_dataset_type(
    const _dataset_type : TGIS_FieldType
  ) : String  ;
  begin
    case _dataset_type of
      TGIS_FieldType.Number  : Result := FIELD_TYPE_NUMBER ;
      TGIS_FieldType.Float   : Result := FIELD_TYPE_FLOAT ;
      TGIS_FieldType.Date    : Result := FIELD_TYPE_DATE ;
      TGIS_FieldType.String  : Result := FIELD_TYPE_STRING ;
      TGIS_FieldType.Boolean : Result := FIELD_TYPE_BOOLEAN ;
    else
      Result := '' ;
    end ;
  end ;

  function parse_params(
    const _node   : IXMLNode ;
    const _params : TGIS_VariantArray
  ) : Boolean ;
  var
    i       : Integer ;
    builder : TStringBuilder ;
  begin
    Result := False ;

    if length( _params ) = 0 then begin
      Result := True ;
      exit ;
    end ;

    builder := TStringBuilder.Create ;
    try
      for i := 0 to length( _params ) - 1  do begin
        builder.Append( VarToString ( _params[i] ) + VALUE_SEPARATOR ) ;
      end ;

      // remove last separator '|'
      if builder.Length > 0 then begin
      {$IFDEF JAVA}
        builder.delete( builder.Length-1, 1 ) ;
      {$ELSE}
        builder.Remove( builder.Length-1, 1 ) ;
      {$ENDIF}

      _node.Attributes[ATTRIBUTE_PARAMS] := builder.ToString ;
      Result := True ;
    end ;
    finally
      FreeObject( builder ) ;
    end ;
  end;

  function parse_statistics_result(
    const _node       : IXMLNode ;
    const _stats_name : String
  ) : Boolean ;
  var
    node_result     : IXMLNode ;
    node_item       : IXMLNode ;
    stats_result    : TGIS_StatisticsLayerResult ;
    available_stats : TList< TGIS_StatisticsItem > ;
    {$IFDEF DCC}
      stats_item      : TGIS_StatisticsItem ;
    {$ENDIF}

  begin
    Result := False ;
    stats_result := stats.Get( _stats_name ) ;

    node_result := _node.AddChild( NODE_STATISTICS_RESULT ) ;
    node_result.Attributes[ATTRIBUTE_NAME] := _stats_name ;

    if stats_result.isIgnoreValue then
      node_result.Attributes[ATTRIBUTE_IGNORE_VALUE] := stats_result.IgnoreValue ;

    available_stats := stats_result.AvailableStatistics ;
    try
      for stats_item in available_stats do begin
        node_item := node_result.AddChild( NODE_STI ) ;
        node_item.Attributes[ATTRIBUTE_KEY] := stats_item.Name ;
        node_item.NodeValue := stats_item.asVariant ;

        Result := parse_params( node_item, stats_item.Params ) or Result ;
        if not Result then
          break ;
      end ;
    finally
      FreeObject( available_stats );
    end ;
  end ;

  function parse_statistics(
    const _node : IXMLNode
  ) : Boolean ;
  var
    node_stats   : IXMLNode ;
    {$IFDEF DCC}
      stats_name : String ;
    {$ENDIF}
  begin
    Result := False ;
    node_stats := _node.AddChild( NODE_STATISTICS ) ;

    // time stamp
    node_stats.Attributes[ATTRIBUTE_DATE] := DateTimeToXMLString( stats.Age, 2, True ) ;

    // use bessel correction
    if stats.UseBesselCorrection <> USE_BESSEL_CORRECTION then
      node_stats.Attributes[ATTRIBUTE_USE_BESSEL_CORRECTION] := not USE_BESSEL_CORRECTION ;

    // fast statistics
    if ( stats is TGIS_StatisticsPixel ) and
       ( TGIS_StatisticsPixel( stats ).FastStatistics <> FAST_STATISTICS ) then
      node_stats.Attributes[ATTRIBUTE_FAST_STATISTICS] := not FAST_STATISTICS ;

    for stats_name in stats.AvailableResults do begin
      Result := parse_statistics_result( node_stats, stats_name ) or Result ;
      if not Result then
        break ;
    end ;
  end ;

begin
  if not validatePath( _path, GIS_TTKSTATS_EXT, False ) then
    exit ;

  stats := _layer.Statistics ;
  xml := TGIS_XMLDocument.Create ;
  try
    xml.Active := True;
    node := xml.AddChild( NODE_TATUKGIS ) ;

    if parse_statistics( node ) then begin
      xml.SaveToFile( _path )
    end
    else begin
      if SafeFileExists( _path ) then
        DeleteFile( _path ) ;
    end ;
  finally
    FreeObject( xml ) ;
  end ;
end;

//------------------------------------------------------------------------------
// ReadAuxXml
//------------------------------------------------------------------------------
class function T_StatisticsIO.ReadAuxXml(
  const _path  : String ;
  const _layer : TGIS_LayerPixel
) : Boolean ;
var
  new_stats : TGIS_StatisticsAbstract ;
  stats     : TGIS_StatisticsAbstract ;
  xml       : TGIS_XMLDocument ;
  node      : IXMLNode ;

  function parse_mdi(
    const _node         : IXMLNode ;
    const _key          : String ;
    const _stats_result : TGIS_StatisticsLayerResult
  ) : Boolean ;
  begin
    Result := True ;
    if ( _key = PAM_STATISTICS_MAXIMUM ) then begin
      _stats_result.Max.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Max.FCalculated := True ;
      _stats_result.Max.FRequested := True ;
    end
    else if ( _key = PAM_STATISTICS_MEAN ) then begin
      _stats_result.Average.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Average.FCalculated := True ;
      _stats_result.Average.FRequested := True ;
    end
    else if ( _key = PAM_STATISTICS_MINIMUM ) then begin
      _stats_result.Min.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.Min.FCalculated := True ;
      _stats_result.Min.FRequested := True ;
    end
    else if ( _key = PAM_STATISTICS_STDDEV ) then begin
      _stats_result.StandardDeviation.FValue := parseDouble( _node.NodeValue ) ;
      _stats_result.StandardDeviation.FCalculated := True ;
      _stats_result.StandardDeviation.FRequested := True ;
    end
    else
      Result := False ;
  end;

  function parse_metadata(
    const _node         : IXMLNode ;
    const _stats_result : TGIS_StatisticsLayerResult
  ) : Boolean ;
  var
    i             : Integer ;
    key           : String ;
    node_item     : IXMLNode     ;
    nodes_lst     : IXMLNodeList ;
    num_stat_keys : Integer ;
  begin
    Result := False ;

    num_stat_keys := 0 ;
    nodes_lst := _node.ChildNodes ;
    for i := 0 to nodes_lst.Count - 1 do begin
      node_item := nodes_lst[i] ;

      if ( node_item.NodeName <> PAM_NODE_MDI ) then
        continue ;

      if node_item.HasAttribute( ATTRIBUTE_KEY ) then
        key := VarToString( node_item.Attributes[ATTRIBUTE_KEY] )
      else
        continue ;

      if parse_mdi( node_item, key, _stats_result ) then
        inc( num_stat_keys ) ;
    end ;

    if num_stat_keys > 0 then
      Result := True ;
  end;

  function parse_rasterband(
    const _node         : IXMLNode ;
    const _stats_result : TGIS_StatisticsLayerResult
  ) : Boolean ;
  var
    i             : Integer ;
    is_metadata   : Boolean ;
    node_metadata : IXMLNode     ;
    nodes_lst     : IXMLNodeList ;
  begin
    Result := False ;
    is_metadata := False ;

    nodes_lst := _node.ChildNodes ;
    for i := 0 to nodes_lst.Count - 1 do begin
      node_metadata := nodes_lst[i] ;

      if node_metadata.NodeName = PAM_NODE_METADATA then begin
        // skip if attribute 'domain' exist
        if node_metadata.HasAttribute( PAM_ATTRIBUTE_DOMAIN ) then
          continue ;

        is_metadata := True ;
        Result := parse_metadata( node_metadata, _stats_result ) ;
        if not Result then
          exit ;
      end ;
    end ;

    if not is_metadata then
      Result := False ;
  end ;

  function parse_dataset(
    const _node : IXMLNode
  ) : Boolean ;
  var
    i                : Integer ;
    stats_name       : String ;
    stats_result     : TGIS_StatisticsLayerResult ;
    nodes_lst        : IXMLNodeList ;
    node_raster_band : IXMLNode ;
    temp_result      : Boolean ;
  begin
    Result := False ;

    nodes_lst := _node.ChildNodes ;
    for i := 0 to nodes_lst.Count - 1 do  begin
      node_raster_band := nodes_lst[i] ;

      if node_raster_band.NodeName = PAM_NODE_RASTER_BAND then begin
        // band
        if node_raster_band.HasAttribute( PAM_ATTRIBUTE_BAND ) then
          stats_name := VarToString( node_raster_band[PAM_ATTRIBUTE_BAND] )
        else
          continue ;

        new_stats.Add( stats_name, TGIS_StatisticalFunctions.EmptyStatistics ) ;
        stats_result := new_stats.Get( stats_name ) ;

        temp_result := parse_rasterband( node_raster_band, stats_result ) ;
        Result := Result or temp_result ;
        if not temp_result then begin
          new_stats.remove( stats_name ) ;
          continue ;
        end ;

        // set internal params
        stats_result.determineNeededVariables ;
      end ;
    end ;

    new_stats.makeDefinedResultsAvailable ;
  end;

begin
  Result := False ;
  try
    if not validatePath( _path, GIS_AUX_XML_EXT, True ) then
      exit ;

    stats := _layer.Statistics ;
    new_stats := TGIS_StatisticsPixel.Create( _layer ) ;
    try
      new_stats.FAge := GisFileAge( _path ) ;

      xml := TGIS_XMLDocument.Create ;
      try
        xml.LoadFromFile( _path ) ;
        node := xml.DocumentElement ;

        if not assigned( node ) then
          exit ;

        if ( node.NodeName = PAM_NODE_DATASET ) then
          Result := parse_dataset( node ) ;
      finally
        FreeObject( xml ) ;
      end ;
    finally
      if Result then
        _layer.Statistics := new_stats
      else
        FreeObject( new_stats ) ;
    end ;
  except
    Result := False ;
  end ;
end;

//------------------------------------------------------------------------------
// SaveAuxXml
//------------------------------------------------------------------------------
class procedure T_StatisticsIO.SaveAuxXml(
  const _path  : String ;
  const _layer : TGIS_LayerPixel
) ;
var
  stats : TGIS_StatisticsAbstract ;
  xml   : TGIS_XMLDocument ;
  node  : IXMLNode ;

  function parse_mdi(
    const _stats_name : String
  ) : String ;
  begin
    if _stats_name = T_FUNCTIONS.MAX then
      Result := PAM_STATISTICS_MAXIMUM
    else if _stats_name = T_FUNCTIONS.AVERAGE then
      Result := PAM_STATISTICS_MEAN
    else if _stats_name = T_FUNCTIONS.MIN then
      Result := PAM_STATISTICS_MINIMUM
    else if _stats_name = T_FUNCTIONS.STANDARD_DEVIATION then
      Result := PAM_STATISTICS_STDDEV
    else
      Result := '' ;
  end;

  function parse_metadata(
    const _node       : IXMLNode ;
    const _stats_name : String
  ) : Boolean ;
  var
    i               : Integer ;
    key             : String ;
    node_metadata   : IXMLNode ;
    node_mdi        : IXMLNode ;
    stats_result    : TGIS_StatisticsLayerResult ;
    available_stats : TList< TGIS_StatisticsItem > ;
    {$IFDEF DCC}
      stats_item    : TGIS_StatisticsItem ;
    {$ENDIF}

  begin
    Result := False ;
    stats_result := stats.Get( _stats_name ) ;

    node_metadata := _node.AddChild( PAM_NODE_METADATA ) ;

    node_mdi := node_metadata.AddChild( PAM_NODE_MDI ) ;
    node_mdi.Attributes[ATTRIBUTE_KEY] := PAM_STATISTICS_APPROXIMATE ;
    node_mdi.NodeValue := GIS_INI_PARAM_BOOLEAN_YES ;

    available_stats := stats_result.AvailableStatistics ;
    try
      i := 0 ;
      for stats_item in available_stats do begin
        key := parse_mdi( stats_item.Name ) ;
        if IsStringEmpty( key ) then
          continue ;

        node_mdi := node_metadata.AddChild( PAM_NODE_MDI ) ;
        node_mdi.Attributes[ATTRIBUTE_KEY] := key ;
        node_mdi.NodeValue := stats_item.asVariant ;
        inc( i ) ;
      end ;

      if i = 0 then
        Result := False
      else
        Result := True ;
    finally
      FreeObject( available_stats );
    end ;
  end ;

  function parse_raster_band(
    const _node : IXMLNode
  ) : Boolean ;
  var
    i                : Integer ;
    node_raster_band : IXMLNode ;
    stats_name       : String ;
  begin
    Result := False ;

    for i := 0 to stats.AvailableResults.Count - 1 do begin
      stats_name := stats.AvailableResults[i] ;

      node_raster_band := _node.AddChild( PAM_NODE_RASTER_BAND ) ;
      node_raster_band.Attributes[PAM_ATTRIBUTE_BAND] := i + 1 ;

      Result := parse_metadata( node_raster_band, stats_name ) or Result ;

      if not Result then
        break ;
    end ;
  end ;

begin
  if not validatePath( _path, GIS_TTKSTATS_EXT, False ) then
    exit ;

  stats := _layer.Statistics ;
  xml := TGIS_XMLDocument.Create ;
  try
    xml.Active := True;
    node := xml.AddChild( PAM_NODE_DATASET ) ;

    if parse_raster_band( node ) then begin
      xml.SaveToFile( _path )
    end
    else begin
      if SafeFileExists( _path ) then
        DeleteFile( _path ) ;
    end ;
  finally
    FreeObject( xml ) ;
  end ;
end;
{$ENDREGION}

{$REGION 'UnitTests'}
{$IFDEF GIS_UNIT_TESTS}
  procedure TestUnitStatistics.TestRandomSampling ;
  const
    N_POPULATION = 100 ;
    N_SAMPLE     = 10 ;
  var
    samples_lst : TList< Integer > ;

    procedure generate_samples_and_test(
      const _nSamples    : Integer ;
      const _nPopulation : Integer ;
      const _replacement : Boolean ;
      const _testId      : String
    ) ;
    var
      i            : Integer ;
      sampler_iter : T_RandomSamplingIterator ;
      sample_val   : Integer ;
    begin
      samples_lst.Clear ;

      sampler_iter := T_RandomSamplingIterator.Create( _nSamples, _nPopulation, _replacement ) ;
      try
        while sampler_iter.Next do
          samples_lst.Add( sampler_iter.CurrentItem ) ;

        // sampler should not generate more samples than dataset size
        Assert.AreEqual(
          Min( _nSamples, _nPopulation ),
          samples_lst.Count,
          Format( '#00010-%s - Incorrect number of drawn values', [_testId] )
        ) ;

        for i := 0 to samples_lst.Count - 1 do begin
          sample_val := samples_lst[i] ;
          Assert.IsTrue(
            ( ( 0 <= sample_val ) and ( sample_val < _nPopulation ) ),
            Format(
              '#00020-%s - The drawn value is out of range (0<=%d<%d)',
              [_testId, sample_val, _nPopulation]
          ) ) ;
        end ;
      finally
        FreeObject( sampler_iter ) ;
      end ;
    end;

  begin
    SET_TEST_NAME( 'TestUnitStatistics.TestRandomSampling' ) ;

    {$IFDEF DCC}
      Randomize ;
    {$ENDIF}

    samples_lst := TList<Integer>.Create ;
    try
      // 1.   Test random sampling with replacement
      // 1.1. Sample size < population size
      generate_samples_and_test( N_SAMPLE, N_POPULATION, True, '1.1' ) ;

      // 1.2. Sample size > population size
      generate_samples_and_test( N_POPULATION, N_SAMPLE, True, '1.2' ) ;

      // 1.3. Sample size = population size
      generate_samples_and_test( N_SAMPLE, N_SAMPLE, True, '1.3' ) ;

      // 2.   Test random sampling without replacement
      // 2.1. Sample size < population size
      generate_samples_and_test( N_SAMPLE, N_POPULATION, False, '2.1' ) ;

      // 2.2. Sample size > population size
      generate_samples_and_test( N_POPULATION, N_SAMPLE, False, '2.2' ) ;

      // 2.3. Sample size = population size
      generate_samples_and_test( N_SAMPLE, N_SAMPLE, False, '2.3' ) ;
    finally
      FreeObject( samples_lst ) ;
    end ;
  end;
{$ENDIF}
{$ENDREGION}

{$IFNDEF OXYGENE}
  {$IFDEF GIS_UNIT_TESTS}
  initialization
    TestUnitStatistics.Register ;
  {$ENDIF}
{$ENDIF}

end.
