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
  Raster algebra.
}

{$IFDEF DCC}
  unit GisRasterAlgebra ;
  {$HPPEMIT '#pragma link "GisRasterAlgebra"'}
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
  uses
    System.Types,
    System.Variants,
    System.Generics.Collections,

    GisClasses,
    GisInterfaces,
    GisLayer,
    GisLayerPixel,
    GisLayerVector,
    GisRtl,
    GisStatistics,
    GisTypes,
    GisTypesUI ;
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
  Unit_GisRasterAlgebra = class
    public
      class procedure SelfRegisterPipeline ;
  end ;


  /// <summary>
  ///   Engine for manipulating raster layers using algebraic expressions.
  /// </summary>
  TGIS_RasterAlgebra = {$IFDEF OXYGENE} public {$ENDIF}
                       class( TGIS_Object )
    private
      oLayers          : TList<TGIS_Layer> ;
      oAliases         : TList<String> ;

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

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure addLayerAlias ( const _layer : TGIS_Layer ;
                                const _alias : String
                              ) ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy ; override ;

    public
      /// <summary>
      ///   Registers the layer for use in the engine.
      /// </summary>
      /// <param name="_layer">
      ///   layer object
      /// </param>
      procedure AddLayer ( const _layer : TGIS_Layer
                         ) ;
      /// <summary>
      ///   Calculates the result of the algebraic expression.
      /// </summary>
      /// <param name="_expression">
      ///   algebraic expression as a string
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_RALGEBRA_LAYERS,
      ///   GIS_RS_ERR_RALGEBRA_EXPRESSION,
      ///   GIS_RS_ERR_RALGEBRA_EXTENT
      /// </exception>
      procedure Execute  ( const _expression : String
                         ) ;
    published // events
      /// <summary>
      ///   Event fired upon progress of the calculation process.
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
    System.SysUtils,
    System.Math,

    GisCsFactory,
    GisCSSystems,
    GisFunctions,
    GisPipeline,
    GisResource ;
{$ENDIF}

const
  GIS_RA_BAND_ERROR : Integer = -GIS_MAX_INTEGER ;

type

  T_Pipeline_RasterAlgebra = class( TGIS_PipelineOperationAbstract )
    const
      PARAM_LAYER   = 'Layer' ;
      PARAM_FORMULA = 'Formula' ;
    public
      procedure Initialize ; override ;
      procedure Execute ; override ;
  end ;


type

  T_raItemType = (
    raOperator, raNumber, raVector, raPixel, raFunction, raStatistic
  ) ;

  T_raOperatorType = (
    raNULL, raNotNULL, raError,
    raLess, raLessEqual, raEqual, raGreaterEqual, raGreater, raNotEqual,
    raAdd, raSubtract,
    raMultiply, raDivide, raDivInt, raModulo,
    raPower,
    raNOT, raAND, raOR, raXOR,
    raNoData,
    raLBracket, raRBracket, raComma,
    raSign
  ) ;

  T_raOperatorAssociativity = (
    raAssociative, raLeftAssociative, raRightAssociative,
    raNotAssociative
  ) ;


  T_raItem = record
    &Type : T_raItemType ;
    Value : Variant ;
  end ;


  T_raVariable = record
    IsVector : Boolean ;
    Vector   : TGIS_LayerVector ;
    Field    : String ;
    Pixel    : TGIS_LayerPixel ;
    Band     : Integer ;
  end ;


  T_raStatistic = record
    IsVector : Boolean ;
    Vector   : TGIS_LayerVector ;
    Field    : String ;
    Pixel    : TGIS_LayerPixel ;
    Band     : Integer ;
    Func     : TGIS_StatisticalFunction ;
  end ;


  T_raParser = class( TGIS_Object )
    private
      oRPN        : TList<T_raItem> ;
      oOperators  : TGIS_Stack<T_raItem> ;
      oLayers     : TList<TGIS_Layer> ;
      oAliases    : TList<String> ;
      bAliases    : Boolean ;
      bReady      : Boolean ;
      oStack      : TGIS_Stack<T_raItem> ;
    private
      function  findLayer    ( const _name : String
                             ) : TGIS_Layer ;
      procedure prepStats    ;
    public
      constructor Create ( const _layers  : TList<TGIS_Layer> ;
                           const _aliases : TList<String>
                         ) ;
    protected
      procedure doDestroy ; override ;
    public
      function  Parse    ( const _expression : String
                         ) : Integer ;
      function  Pop      : Single ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      function  Evaluate : Single ;
    public
      Variables   : TList<T_raVariable> ;
      Statistics  : TList<T_raStatistic> ;
      Values      : TGIS_SingleArray ;
      StatValues  : TGIS_SingleArray ;
  end ;


  T_raFunction = {$IFDEF OXYGENE} abstract {$ENDIF} class( TGIS_Object )
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      FSymbol       : String ;
      FNoData       : Boolean ;
    public
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; virtual ; abstract ;
    public
      property Symbol       : String
                              read  FSymbol ;
      property NoData       : Boolean
                              read  FNoData ;
  end ;


  T_raFunctions = class( TGIS_Object )
    private
      {$IFDEF DCC}
        oList : TObjectList<T_raFunction> ;
      {$ENDIF}
      {$IFDEF CLR}
        oList : TList<T_raFunction> ;
      {$ENDIF}
      {$IFDEF JAVA}
        oList : TList<T_raFunction> ;
      {$ENDIF}
      {$IFDEF ISLAND}
        oList : TList<T_raFunction> ;
      {$ENDIF}
    private
      function fget_Items ( const _idx : Integer
                          ) : T_raFunction ;
    public
      constructor Create ;
    protected
      procedure doDestroy ; override ;
    public
      procedure Add  ( const _function : T_raFunction
                     ) ;
      function  Find ( const _symbol   : String
                     ) : Integer ;
    public
      property  Items [const _idx : Integer] : T_raFunction
                                               read fget_Items ;
  end ;


  T_raFunctionNoData = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionAbs = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionMin = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionMax = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionRad2Deg = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionDeg2Rad = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionFloor = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCeil = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionRound = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionIf = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionSqr = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionPower = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionSqrt = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionRoot = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionExp = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionLn = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionLog2 = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionLog10 = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionLog = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionSin = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCos = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionTan = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCot = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionSec = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCsc = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcSin = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcCos = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcTan = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcTan2 = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcCot = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcSec = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcCsc = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionSinh = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCosh = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionTanh = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCoth = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionSech = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionCsch = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcSinh = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcCosh = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcTanh = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcCoth = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcSech = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionArcCsch = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionARGB = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionRGB = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionAHSL = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionHSL = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionAHSV = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raFunctionHSV = class( T_raFunction )
    public
      constructor Create ;
    public
      function Evaluate ( const _parser : T_raParser
                        ) : Single ; override ;
  end ;


  T_raLock = record
    Layer : TGIS_LayerPixel ;
    Band  : Integer ;
    Lock  : TGIS_LayerPixelLock ;
  end ;


var
  oraFunctions : T_raFunctions ;


//==============================================================================
// T_Pipeline_RasterAlgebra
//==============================================================================

  class procedure Unit_GisRasterAlgebra.SelfRegisterPipeline ;
  begin
    RegisterPipeline( 'Algebra', T_Pipeline_RasterAlgebra ) ;
  end ;


  procedure T_Pipeline_RasterAlgebra.Initialize ;
  var
    i : Integer ;
  begin
    inherited ;

    defineParam(
      GIS_PIPELINE_PARAM_RESULT,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      True,
      '',
      ''
    ) ;

    for i := 0 to 9 do
      defineParam(
        PARAM_LAYER + IntToStr( i ),
        TGIS_PipelineParameterType.String,
        NaN,
        NaN,
        False,
        '',
        ''
      ) ;

    defineParam(
      PARAM_FORMULA,
      TGIS_PipelineParameterType.String,
      NaN,
      NaN,
      True,
      '',
      ''
    ) ;
  end ;


  procedure T_Pipeline_RasterAlgebra.Execute ;
  var
    layer  : TGIS_Layer ;
    res    : TGIS_LayerPixel ;
    expr   : String ;
    ra     : TGIS_RasterAlgebra ;
    i      : Integer ;
  begin
    res := ParamAsLayerPixel( GIS_PIPELINE_PARAM_RESULT ) ;
    expr := ParamAsString( PARAM_FORMULA ) ;

    ra := TGIS_RasterAlgebra.Create ;
    try
      ra.addLayerAlias( res, GIS_PIPELINE_PARAM_RESULT ) ;

      for i := 0 to 9 do begin
        if TryParamAsLayer( PARAM_LAYER + IntToStr( i ), layer ) then
          ra.addLayerAlias( layer, PARAM_LAYER + IntToStr( i ) ) ;
      end ;
      ra.busyEventManager.UseProgressThreshold := False ;
      {$IFDEF CLR}
        ra.BusyEvent += DoBusyEvent ;
      {$ELSE}
        ra.BusyEvent := {$IFDEF OXYGENE}@{$ENDIF}DoBusyEvent ;
      {$ENDIF}
      ra.Execute( expr ) ;
    finally
      FreeObject( ra ) ;
    end ;

     inherited ;
  end ;

//==============================================================================
// utilities
//==============================================================================

  function raFunctions
    : T_raFunctions ;
  begin
    if not assigned( oraFunctions ) then begin
      oraFunctions := T_raFunctions.Create ;
      oraFunctions.Add( T_raFunctionNoData.Create ) ;
      oraFunctions.Add( T_raFunctionAbs.Create ) ;
      oraFunctions.Add( T_raFunctionMin.Create ) ;
      oraFunctions.Add( T_raFunctionMax.Create ) ;
      oraFunctions.Add( T_raFunctionRad2Deg.Create ) ;
      oraFunctions.Add( T_raFunctionDeg2Rad.Create ) ;
      oraFunctions.Add( T_raFunctionFloor.Create ) ;
      oraFunctions.Add( T_raFunctionCeil.Create ) ;
      oraFunctions.Add( T_raFunctionRound.Create ) ;
      oraFunctions.Add( T_raFunctionIf.Create ) ;
      oraFunctions.Add( T_raFunctionSqr.Create ) ;
      oraFunctions.Add( T_raFunctionPower.Create ) ;
      oraFunctions.Add( T_raFunctionSqrt.Create ) ;
      oraFunctions.Add( T_raFunctionRoot.Create ) ;
      oraFunctions.Add( T_raFunctionExp.Create ) ;
      oraFunctions.Add( T_raFunctionLn.Create ) ;
      oraFunctions.Add( T_raFunctionLog2.Create ) ;
      oraFunctions.Add( T_raFunctionLog10.Create ) ;
      oraFunctions.Add( T_raFunctionLog.Create ) ;
      oraFunctions.Add( T_raFunctionSin.Create ) ;
      oraFunctions.Add( T_raFunctionCos.Create ) ;
      oraFunctions.Add( T_raFunctionTan.Create ) ;
      oraFunctions.Add( T_raFunctionCot.Create ) ;
      oraFunctions.Add( T_raFunctionSec.Create ) ;
      oraFunctions.Add( T_raFunctionCsc.Create ) ;
      oraFunctions.Add( T_raFunctionArcSin.Create ) ;
      oraFunctions.Add( T_raFunctionArcCos.Create ) ;
      oraFunctions.Add( T_raFunctionArcTan.Create ) ;
      oraFunctions.Add( T_raFunctionArcTan2.Create ) ;
      oraFunctions.Add( T_raFunctionArcCot.Create ) ;
      oraFunctions.Add( T_raFunctionArcSec.Create ) ;
      oraFunctions.Add( T_raFunctionArcCsc.Create ) ;
      oraFunctions.Add( T_raFunctionSinh.Create ) ;
      oraFunctions.Add( T_raFunctionCosh.Create ) ;
      oraFunctions.Add( T_raFunctionTanh.Create ) ;
      oraFunctions.Add( T_raFunctionCoth.Create ) ;
      oraFunctions.Add( T_raFunctionSech.Create ) ;
      oraFunctions.Add( T_raFunctionCsch.Create ) ;
      oraFunctions.Add( T_raFunctionArcSinh.Create ) ;
      oraFunctions.Add( T_raFunctionArcCosh.Create ) ;
      oraFunctions.Add( T_raFunctionArcTanh.Create ) ;
      oraFunctions.Add( T_raFunctionArcCoth.Create ) ;
      oraFunctions.Add( T_raFunctionArcSech.Create ) ;
      oraFunctions.Add( T_raFunctionArcCsch.Create ) ;
      oraFunctions.Add( T_raFunctionARGB.Create ) ;
      oraFunctions.Add( T_raFunctionRGB.Create ) ;
      oraFunctions.Add( T_raFunctionAHSL.Create ) ;
      oraFunctions.Add( T_raFunctionHSL.Create ) ;
      oraFunctions.Add( T_raFunctionAHSV.Create ) ;
      oraFunctions.Add( T_raFunctionHSV.Create ) ;
    end ;

    Result := oraFunctions ;
  end ;

//==============================================================================
// T_raFunction
//==============================================================================

  constructor T_raFunction.Create ;
  begin
    inherited ;

    FNoData := False ;
  end ;


  procedure T_raFunction.doDestroy ;
  begin

    inherited ;
  end ;


//==============================================================================
// T_raFunctions
//==============================================================================

  constructor T_raFunctions.Create ;
  begin
    inherited ;

    {$IFDEF DCC}
      oList := TObjectList<T_raFunction>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      oList := new TList<T_raFunction> ;
    {$ENDIF}
    {$IFDEF JAVA}
      oList := new TList<T_raFunction> ;
    {$ENDIF}
    {$IFDEF ISLAND}
      oList := new TList<T_raFunction> ;
    {$ENDIF}
  end ;


  procedure T_raFunctions.doDestroy ;
  begin
    FreeObject( oList ) ;

    inherited ;
  end ;


  function T_raFunctions.fget_Items(
    const _idx : Integer
  ) : T_raFunction ;
  begin
    Result := oList[_idx] ;
  end ;


  procedure T_raFunctions.Add(
    const _function : T_raFunction
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to oList.Count - 1 do begin
      if CompareText( _function.Symbol, oList[i].Symbol ) = 0 then
        exit ;
    end ;

    oList.Add( _function ) ;
  end ;


  function T_raFunctions.Find(
    const _symbol : String
  ) : Integer ;
  var
    res : Integer ;
    i   : Integer ;
  begin
    res := -1 ;

    for i := 0 to oList.Count - 1 do begin
      if CompareText( _symbol, oList[i].Symbol ) = 0 then begin
        res := i ;
        break ;
      end ;
    end ;

    Result := res ;
  end ;


//==============================================================================
// T_raFunctionNoData
//==============================================================================

  constructor T_raFunctionNoData.Create ;
  begin
    inherited ;

    FSymbol := 'NODATA' ;
    FNoData := True ;
  end ;


  function T_raFunctionNoData.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  begin
    if IsNan( _parser.Pop ) then
      Result := 1.0
    else
      Result := 0.0 ;
  end ;


//==============================================================================
// T_raFunctionAbs
//==============================================================================

  constructor T_raFunctionAbs.Create ;
  begin
    inherited ;

    FSymbol := 'ABS' ;
  end ;


  function T_raFunctionAbs.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := Abs( v ) ;
  end ;


//==============================================================================
// T_raFunctionMin
//==============================================================================

  constructor T_raFunctionMin.Create ;
  begin
    inherited ;

    FSymbol := 'MIN' ;
  end ;


  function T_raFunctionMin.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
  begin
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) then
      Result := v1
    else
    if IsNan( v1 ) then
      Result := v0
    else
      Result := Min( v0, v1 ) ;
  end ;


//==============================================================================
// T_raFunctionMax
//==============================================================================

  constructor T_raFunctionMax.Create ;
  begin
    inherited ;

    FSymbol := 'MAX' ;
  end ;


  function T_raFunctionMax.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
  begin
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) then
      Result := v1
    else
    if IsNan( v1 ) then
      Result := v0
    else
      Result := Max( v0, v1 ) ;
  end ;


//==============================================================================
// T_raFunctionRad2Deg
//==============================================================================

  constructor T_raFunctionRad2Deg.Create ;
  begin
    inherited ;

    FSymbol := 'RAD2DEG' ;
  end ;


  function T_raFunctionRad2Deg.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := RadToDeg( v ) ;
  end ;


//==============================================================================
// T_raFunctionDeg2Rad
//==============================================================================

  constructor T_raFunctionDeg2Rad.Create ;
  begin
    inherited ;

    FSymbol := 'DEG2RAD' ;
  end ;


  function T_raFunctionDeg2Rad.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := DegToRad( v ) ;
  end ;


//==============================================================================
// T_raFunctionFloor
//==============================================================================

  constructor T_raFunctionFloor.Create ;
  begin
    inherited ;

    FSymbol := 'FLOOR' ;
  end ;


  function T_raFunctionFloor.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := 1.0*FloorS( v ) ;
  end ;


//==============================================================================
// T_raFunctionCeil
//==============================================================================

  constructor T_raFunctionCeil.Create ;
  begin
    inherited ;

    FSymbol := 'CEIL' ;
  end ;


  function T_raFunctionCeil.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := 1.0*CeilS( v ) ;
  end ;


//==============================================================================
// T_raFunctionRound
//==============================================================================

  constructor T_raFunctionRound.Create ;
  begin
    inherited ;

    FSymbol := 'ROUND' ;
  end ;


  function T_raFunctionRound.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := 1.0*RoundS( v ) ;
  end ;


//==============================================================================
// T_raFunctionIf
//==============================================================================

  constructor T_raFunctionIf.Create ;
  begin
    inherited ;

    FSymbol := 'IF' ;
  end ;


  function T_raFunctionIf.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
  begin
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) then
      Result := NaN
    else
    if v0 <> 0.0 then
      Result := v1
    else
      Result := v2 ;
  end ;


//==============================================================================
// T_raFunctionSqr
//==============================================================================

  constructor T_raFunctionSqr.Create ;
  begin
    inherited ;

    FSymbol := 'SQR' ;
  end ;


  function T_raFunctionSqr.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := v*v ;
  end ;


//==============================================================================
// T_raFunctionPower
//==============================================================================

  constructor T_raFunctionPower.Create ;
  begin
    inherited ;

    FSymbol := 'POW' ;
  end ;


  function T_raFunctionPower.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
  begin
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) then
      Result := NaN
    else begin
      try
        Result := Power( v0, v1 ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionSqrt
//==============================================================================

  constructor T_raFunctionSqrt.Create ;
  begin
    inherited ;

    FSymbol := 'SQRT' ;
  end ;


  function T_raFunctionSqrt.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Sqrt( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionRoot
//==============================================================================

  constructor T_raFunctionRoot.Create ;
  begin
    inherited ;

    FSymbol := 'ROOT' ;
  end ;


  function T_raFunctionRoot.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
  begin
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) then
      Result := NaN
    else begin
      try
        Result := Power( v0, 1.0/v1 ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionExp
//==============================================================================

  constructor T_raFunctionExp.Create ;
  begin
    inherited ;

    FSymbol := 'EXP' ;
  end ;


  function T_raFunctionExp.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Exp( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionLn
//==============================================================================

  constructor T_raFunctionLn.Create ;
  begin
    inherited ;

    FSymbol := 'LN' ;
  end ;


  function T_raFunctionLn.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Ln( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionLog2
//==============================================================================

  constructor T_raFunctionLog2.Create ;
  begin
    inherited ;

    FSymbol := 'LOG2' ;
  end ;


  function T_raFunctionLog2.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Log2( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionLog10
//==============================================================================

  constructor T_raFunctionLog10.Create ;
  begin
    inherited ;

    FSymbol := 'Log10' ;
  end ;


  function T_raFunctionLog10.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Log10( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionLog
//==============================================================================

  constructor T_raFunctionLog.Create ;
  begin
    inherited ;

    FSymbol := 'LOG' ;
  end ;


  function T_raFunctionLog.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
  begin
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) then
      Result := NaN
    else begin
      try
        Result := LogN( v0, v1 ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionSin
//==============================================================================

  constructor T_raFunctionSin.Create ;
  begin
    inherited ;

    FSymbol := 'SIN' ;
  end ;


  function T_raFunctionSin.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := Sin( v ) ;
  end ;


//==============================================================================
// T_raFunctionCos
//==============================================================================

  constructor T_raFunctionCos.Create ;
  begin
    inherited ;

    FSymbol := 'COS' ;
  end ;


  function T_raFunctionCos.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := Cos( v ) ;
  end ;


//==============================================================================
// T_raFunctionTan
//==============================================================================

  constructor T_raFunctionTan.Create ;
  begin
    inherited ;

    FSymbol := 'TAN' ;
  end ;


  function T_raFunctionTan.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Tan( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionCot
//==============================================================================

  constructor T_raFunctionCot.Create ;
  begin
    inherited ;

    FSymbol := 'COT' ;
  end ;


  function T_raFunctionCot.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Cot( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionSec
//==============================================================================

  constructor T_raFunctionSec.Create ;
  begin
    inherited ;

    FSymbol := 'SEC' ;
  end ;


  function T_raFunctionSec.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Sec( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionCsc
//==============================================================================

  constructor T_raFunctionCsc.Create ;
  begin
    inherited ;

    FSymbol := 'CSC' ;
  end ;


  function T_raFunctionCsc.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Csc( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcSin
//==============================================================================

  constructor T_raFunctionArcSin.Create ;
  begin
    inherited ;

    FSymbol := 'ARCSIN' ;
  end ;


  function T_raFunctionArcSin.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcSin( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcCos
//==============================================================================

  constructor T_raFunctionArcCos.Create ;
  begin
    inherited ;

    FSymbol := 'ARCCOS' ;
  end ;


  function T_raFunctionArcCos.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcCos( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcTan
//==============================================================================

  constructor T_raFunctionArcTan.Create ;
  begin
    inherited ;

    FSymbol := 'ARCTAN' ;
  end ;


  function T_raFunctionArcTan.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := ArcTan( v ) ;
  end ;


//==============================================================================
// T_raFunctionArcTan2
//==============================================================================

  constructor T_raFunctionArcTan2.Create ;
  begin
    inherited ;

    FSymbol := 'ARCTAN2' ;
  end ;


  function T_raFunctionArcTan2.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
  begin
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) then
      Result := NaN
    else
      Result := ArcTan2( v0, v1 ) ;
  end ;


//==============================================================================
// T_raFunctionArcCot
//==============================================================================

  constructor T_raFunctionArcCot.Create ;
  begin
    inherited ;

    FSymbol := 'ARCCOT' ;
  end ;


  function T_raFunctionArcCot.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else
      Result := ArcCot( v ) ;
  end ;


//==============================================================================
// T_raFunctionArcSec
//==============================================================================

  constructor T_raFunctionArcSec.Create ;
  begin
    inherited ;

    FSymbol := 'ARCSEC' ;
  end ;


  function T_raFunctionArcSec.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcSec( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcCsc
//==============================================================================

  constructor T_raFunctionArcCsc.Create ;
  begin
    inherited ;

    FSymbol := 'ARCCSC' ;
  end ;


  function T_raFunctionArcCsc.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcCsc( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionSinh
//==============================================================================

  constructor T_raFunctionSinh.Create ;
  begin
    inherited ;

    FSymbol := 'SINH' ;
  end ;


  function T_raFunctionSinh.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Sinh( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionCosh
//==============================================================================

  constructor T_raFunctionCosh.Create ;
  begin
    inherited ;

    FSymbol := 'COSH' ;
  end ;


  function T_raFunctionCosh.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Cosh( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionTanh
//==============================================================================

  constructor T_raFunctionTanh.Create ;
  begin
    inherited ;

    FSymbol := 'TANH' ;
  end ;


  function T_raFunctionTanh.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Tanh( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionCoth
//==============================================================================

  constructor T_raFunctionCoth.Create ;
  begin
    inherited ;

    FSymbol := 'COTH' ;
  end ;


  function T_raFunctionCoth.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Coth( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionSech
//==============================================================================

  constructor T_raFunctionSech.Create ;
  begin
    inherited ;

    FSymbol := 'SECH' ;
  end ;


  function T_raFunctionSech.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Sech( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionCsch
//==============================================================================

  constructor T_raFunctionCsch.Create ;
  begin
    inherited ;

    FSymbol := 'CSCH' ;
  end ;


  function T_raFunctionCsch.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := Csch( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcSinh
//==============================================================================

  constructor T_raFunctionArcSinh.Create ;
  begin
    inherited ;

    FSymbol := 'ARCSINH' ;
  end ;


  function T_raFunctionArcSinh.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcSinh( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcCosh
//==============================================================================

  constructor T_raFunctionArcCosh.Create ;
  begin
    inherited ;

    FSymbol := 'ARCCOSH' ;
  end ;


  function T_raFunctionArcCosh.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcCosh( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcTanh
//==============================================================================

  constructor T_raFunctionArcTanh.Create ;
  begin
    inherited ;

    FSymbol := 'ARCTANH' ;
  end ;


  function T_raFunctionArcTanh.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcTanh( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcCoth
//==============================================================================

  constructor T_raFunctionArcCoth.Create ;
  begin
    inherited ;

    FSymbol := 'ARCCOTH' ;
  end ;


  function T_raFunctionArcCoth.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcCoth( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcSech
//==============================================================================

  constructor T_raFunctionArcSech.Create ;
  begin
    inherited ;

    FSymbol := 'ARCSECH' ;
  end ;


  function T_raFunctionArcSech.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcSech( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionArcCsch
//==============================================================================

  constructor T_raFunctionArcCsch.Create ;
  begin
    inherited ;

    FSymbol := 'ARCCSCH' ;
  end ;


  function T_raFunctionArcCsch.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v : Single ;
  begin
    v := _parser.Pop ;

    if IsNan( v ) then
      Result := NaN
    else begin
      try
        Result := ArcCsch( v ) ;
      except
        Result := NaN ;
      end ;
    end ;
  end ;


//==============================================================================
// T_raFunctionARGB
//==============================================================================

  constructor T_raFunctionARGB.Create ;
  begin
    inherited ;

    FSymbol := 'ARGB' ;
  end ;


  function T_raFunctionARGB.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
    v3 : Single ;
    a, r, g, b : Byte ;
  begin
    v3 := _parser.Pop ;
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) or IsNan( v2 ) or IsNan( v3 ) then begin
      Result := NaN ;
      exit ;
    end ;

    a := Byte( RoundS( v0 ) ) ;
    r := Byte( RoundS( v1 ) ) ;
    g := Byte( RoundS( v2 ) ) ;
    b := Byte( RoundS( v3 ) ) ;

    Result := 1.0*Integer( TGIS_Color.FromARGB( a, r, g, b ).ToARGB ) ;
  end ;


//==============================================================================
// T_raFunctionRGB
//==============================================================================

  constructor T_raFunctionRGB.Create ;
  begin
    inherited ;

    FSymbol := 'RGB' ;
  end ;


  function T_raFunctionRGB.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
    r, g, b : Byte ;
  begin
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) or IsNan( v2 ) then begin
      Result := NaN ;
      exit ;
    end ;

    r := Byte( RoundS( v0 ) ) ;
    g := Byte( RoundS( v1 ) ) ;
    b := Byte( RoundS( v2 ) ) ;

    Result := 1.0*Integer( TGIS_Color.FromRGB( r, g, b ).ToARGB ) ;
  end ;


//==============================================================================
// T_raFunctionAHSL
//==============================================================================

  constructor T_raFunctionAHSL.Create ;
  begin
    inherited ;

    FSymbol := 'AHSL' ;
  end ;


  function T_raFunctionAHSL.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
    v3 : Single ;
  begin
    v3 := _parser.Pop ;
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) or IsNan( v2 ) or IsNan( v3 ) then begin
      Result := NaN ;
      exit ;
    end ;

    Result := 1.0*Integer( TGIS_Color.FromAHSL( v0, v1, v2, v3 ).ToARGB ) ;
  end ;


//==============================================================================
// T_raFunctionHSL
//==============================================================================

  constructor T_raFunctionHSL.Create ;
  begin
    inherited ;

    FSymbol := 'HSL' ;
  end ;


  function T_raFunctionHSL.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
  begin
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) or IsNan( v2 ) then begin
      Result := NaN ;
      exit ;
    end ;

    Result := 1.0*Integer( TGIS_Color.FromHSL( v0, v1, v2 ).ToARGB ) ;
  end ;


//==============================================================================
// T_raFunctionAHSV
//==============================================================================

  constructor T_raFunctionAHSV.Create ;
  begin
    inherited ;

    FSymbol := 'AHSV' ;
  end ;


  function T_raFunctionAHSV.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
    v3 : Single ;
  begin
    v3 := _parser.Pop ;
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) or IsNan( v2 ) or IsNan( v3 ) then begin
      Result := NaN ;
      exit ;
    end ;

    Result := 1.0*Integer( TGIS_Color.FromAHSV( v0, v1, v2, v3 ).ToARGB ) ;
  end ;


//==============================================================================
// T_raFunctionHSV
//==============================================================================

  constructor T_raFunctionHSV.Create ;
  begin
    inherited ;

    FSymbol := 'HSV' ;
  end ;


  function T_raFunctionHSV.Evaluate(
    const _parser : T_raParser
  ) : Single ;
  var
    v0 : Single ;
    v1 : Single ;
    v2 : Single ;
  begin
    v2 := _parser.Pop ;
    v1 := _parser.Pop ;
    v0 := _parser.Pop ;

    if IsNan( v0 ) or IsNan( v1 ) or IsNan( v2 ) then begin
      Result := NaN ;
      exit ;
    end ;

    Result := 1.0*Integer( TGIS_Color.FromHSV( v0, v1, v2 ).ToARGB ) ;
  end ;


//==============================================================================
// T_raParser
//==============================================================================

  constructor T_raParser.Create(
    const _layers  : TList<TGIS_Layer> ;
    const _aliases : TList<String>
  ) ;
  var
    i : Integer ;
  begin
    inherited Create ;

    oLayers := _layers ;
    oAliases := _aliases ;

    bAliases := False ;
    for i := 0 to oAliases.Count - 1 do begin
      if IsStringEmpty( oAliases[i] ) then
        continue ;
      bAliases := True ;
      break ;
    end ;

    oRPN := TList<T_raItem>.Create ;
    oOperators := TGIS_Stack<T_raItem>.Create ;

    Variables := TList<T_raVariable>.Create ;
    Statistics := TList<T_raStatistic>.Create ;

    bReady := False ;
  end ;


  procedure T_raParser.doDestroy ;
  begin
    FreeObject( oRPN        ) ;
    FreeObject( oOperators  ) ;
    FreeObject( Variables   ) ;
    FreeObject( Statistics  ) ;

    inherited ;
  end ;


  function T_raParser.findLayer(
    const _name : String
  ) : TGIS_Layer ;
  var
    res : TGIS_Layer ;
    str : String ;
    i   : Integer ;
  begin
    res := nil ;

    for i := 0 to oLayers.Count - 1 do begin
      if bAliases then
        str := oAliases[i]
      else
        str := oLayers[i].Name ;

      if CompareText( str, _name ) = 0 then begin
        res := oLayers[i] ;
        break ;
      end ;
    end ;

    Result := res ;
  end ;


  procedure T_raParser.prepStats ;
  var
    stat : T_raStatistic ;
    llst : TList<TGIS_Layer> ;
    res  : TGIS_StatisticsLayerResult ;
    i    : Integer ;

    function layer_on_list(
      const _l : TGIS_Layer
    ) : Boolean ;
    var
      ii : Integer ;
    begin
      Result := False ;

      if llst.Count = 0 then
        exit ;

      for ii := 0 to llst.Count - 1 do begin
        if CompareText( _l.Name, llst[ii].Name ) = 0 then begin
          Result := True ;
          break ;
        end ;
      end ;
    end ;

    function get_band(
      const _i : Integer ;
      const _b : Boolean
    ) : String ;
    var
      rr : String ;
    begin
      if _b then
        rr := IntToStr( _i )
      else begin
        case _i of
           1 : rr := GIS_BAND_R ;
           2 : rr := GIS_BAND_G ;
           3 : rr := GIS_BAND_B ;
           4 : rr := GIS_BAND_A ;
          else rr := IntToStr( _i ) ;
        end ;
      end ;

      Result := rr ;
    end ;

    procedure add_statistic(
      const _s : T_raStatistic
    ) ;
    var
      arr : TGIS_VariantArray ;
    begin
      SetLength( arr, 0 ) ;
      if _s.IsVector then
        _s.Vector.Statistics.Add(
          _s.Field, _s.Func, arr, NaN
        )
      else
        _s.Pixel.Statistics.Add(
          get_band( _s.Band, stat.Pixel.IsGrid ),
          _s.Func, arr, stat.Pixel.NoDataValue
        ) ;
    end ;

  begin
    if Statistics.Count = 0 then
      exit ;

    SetLength( StatValues, Statistics.Count ) ;

    llst := TList<TGIS_Layer>.Create ;
    try
      for i := 0 to Statistics.Count - 1 do begin
        stat := Statistics[i] ;
        if stat.IsVector then begin
          if not layer_on_list( stat.Vector ) then
            llst.Add( stat.Vector ) ;
        end
        else begin
          if not layer_on_list( stat.Pixel ) then
            llst.Add( stat.Pixel ) ;
        end ;
        add_statistic( stat ) ;
      end ;

      for i := 0 to llst.Count - 1 do
        llst[i].Statistics.Calculate ;
    finally
      FreeObject( llst ) ;
    end ;

    for i := 0 to Statistics.Count - 1 do begin
      stat := Statistics[i] ;
      if stat.IsVector then
        res := stat.Vector.Statistics.Get( stat.Field )
      else
        res := stat.Pixel.Statistics.Get(
                 get_band( stat.Band, stat.Pixel.IsGrid ) ) ;
      case stat.Func of
        TGIS_StatisticalFunction.Average :
          StatValues[i] := res.Average.Value ;
        TGIS_StatisticalFunction.Count :
          StatValues[i] := 1.0*res.Count.Value ;
        TGIS_StatisticalFunction.CountMissings :
          StatValues[i] := 1.0*res.CountMissings.Value ;
        TGIS_StatisticalFunction.Max :
          StatValues[i] := res.Max.Value ;
        TGIS_StatisticalFunction.Majority :
          StatValues[i] := VarToSingle( res.Majority.Value ) ;
        TGIS_StatisticalFunction.Median :
          StatValues[i] := res.Median.Value ;
        TGIS_StatisticalFunction.Min :
          StatValues[i] := res.Min.Value ;
        TGIS_StatisticalFunction.Minority :
          StatValues[i] := VarToSingle( res.Minority.Value ) ;
        TGIS_StatisticalFunction.Range :
          StatValues[i] := res.Range.Value ;
        TGIS_StatisticalFunction.StandardDeviation :
          StatValues[i] := res.StandardDeviation.Value ;
        TGIS_StatisticalFunction.Sum :
          StatValues[i] := res.Sum.Value ;
        TGIS_StatisticalFunction.Variance :
          StatValues[i] := res.Variance.Value ;
        TGIS_StatisticalFunction.Variety :
          StatValues[i] := 1.0*res.Variety.Value ;
      end ;
    end ;

  end ;


  function T_raParser.Parse(
    const _expression : String
  ) : Integer ;
  var
    sb    : TStringBuilder ;
    stack : TGIS_Stack<T_raItem> ;
    state : Integer ;
    lstr  : String ;
    rstr  : String ;
    opr   : T_raOperatorType ;
    opr1  : T_raOperatorType ;
    prt2  : Boolean ;
    len   : Integer ;
    last  : T_raItemType ;
    skip  : Boolean ;
    sfunc : TGIS_StatisticalFunction ;
    quot1 : Boolean ;
    quot2 : Boolean ;
    c     : Char ;
    i     : Integer ;
    j     : Integer ;

    function is_whitespace
      : Boolean ;
    begin
      if ( ( c >= #9 ) and ( c <= #13 ) ) or ( c = #32 ) then
        Result := True
      else
        Result := False ;
    end ;

    function quoted
      : Boolean ;
    begin
      Result := quot1 or quot2 ;
    end ;

    function is_quote
      : Boolean ;
    begin
      if ( c = '''' ) and ( not quot2 ) then begin
        quot1 := not quot1 ;
        Result := True ;
      end
      else
      if ( c = '"' ) and ( not quot1 ) then begin
        quot2 := not quot2 ;
        Result := True ;
      end
      else
        Result := False ;
    end ;

    function is_alphanumeric
      : Boolean ;
    begin
      if ( ( c >= #48 ) and ( c <= #57  ) ) or
         ( ( c >= #65 ) and ( c <= #90  ) ) or
         ( ( c >= #97 ) and ( c <= #122 ) ) then
        Result := True
      else
        Result := False ;
    end ;

    function is_equality
      : Boolean ;
    begin
      if c = #61 then
        Result := True
      else
        Result := False ;
    end ;

    function get_operator
      : T_raOperatorType ;
    var
      rr : T_raOperatorType ;
    begin
      rr := T_raOperatorType.raNULL ;
      if opr1 = T_raOperatorType.raNULL then begin
        case c of
          '+' : rr := T_raOperatorType.raAdd ;
          '-' : rr := T_raOperatorType.raSubtract ;
          '*' : rr := T_raOperatorType.raMultiply ;
          '/' : rr := T_raOperatorType.raDivide ;
          '%' : rr := T_raOperatorType.raModulo ;
          '^' : rr := T_raOperatorType.raPower ;
          '?' : rr := T_raOperatorType.raNoData ;
          '(' : rr := T_raOperatorType.raLBracket ;
          ')' : rr := T_raOperatorType.raRBracket ;
          ',' : rr := T_raOperatorType.raComma ;
          '<' :
            begin
              rr  := T_raOperatorType.raNotNULL ;
              opr1 := T_raOperatorType.raLess ;
            end ;
          '=' :
            begin
              rr  := T_raOperatorType.raNotNULL ;
              opr1 := T_raOperatorType.raEqual ;
            end ;
          '>' :
            begin
              rr  := T_raOperatorType.raNotNULL ;
              opr1 := T_raOperatorType.raGreater ;
            end ;
          '!' :
            begin
              rr  := T_raOperatorType.raNotNULL ;
              opr1 := T_raOperatorType.raNotEqual ;
            end ;
        end ;
      end
      else begin
        if c = '=' then begin
          case opr1 of
            T_raOperatorType.raLess :
              rr := T_raOperatorType.raLessEqual ;
            T_raOperatorType.raEqual :
              rr := T_raOperatorType.raEqual ;
            T_raOperatorType.raGreater :
              rr := T_raOperatorType.raGreaterEqual ;
            T_raOperatorType.raNotEqual :
              rr := T_raOperatorType.raNotEqual ;
          end ;
        end
        else
        if c = '>' then begin
          case opr1 of
            T_raOperatorType.raLess :
              rr := T_raOperatorType.raNotEqual ;
            T_raOperatorType.raEqual,
            T_raOperatorType.raGreater,
            T_raOperatorType.raNotEqual :
              rr := T_raOperatorType.raError ;
          end ;
        end
        else begin
          case opr1 of
            T_raOperatorType.raLess :
              rr := T_raOperatorType.raLess ;
            T_raOperatorType.raEqual :
              rr := T_raOperatorType.raError ;
            T_raOperatorType.raGreater :
              rr := T_raOperatorType.raGreater ;
            T_raOperatorType.raNotEqual :
              rr := T_raOperatorType.raError ;
          end ;
        end ;
        opr1 := T_raOperatorType.raNULL ;
      end ;
      Result := rr ;
    end ;

    function top_operator
      : T_raOperatorType ;
    begin
      Result := T_raOperatorType( stack.Top.Value ) ;
    end ;

    function operator_order(
      const _t : T_raOperatorType
    ) : Integer ;
    var
      rr : Integer ;
    begin
      rr := -1 ;
      case _t of
        T_raOperatorType.raLBracket :
          rr := 0 ;
        T_raOperatorType.raLess,
        T_raOperatorType.raLessEqual,
        T_raOperatorType.raEqual,
        T_raOperatorType.raGreaterEqual,
        T_raOperatorType.raGreater,
        T_raOperatorType.raNotEqual :
          rr := 1 ;
        T_raOperatorType.raAdd,
        T_raOperatorType.raSubtract :
          rr := 2 ;
        T_raOperatorType.raMultiply,
        T_raOperatorType.raDivide,
        T_raOperatorType.raDivInt,
        T_raOperatorType.raModulo :
          rr := 3 ;
        T_raOperatorType.raPower :
          rr := 4 ;
        T_raOperatorType.raNOT,
        T_raOperatorType.raAND,
        T_raOperatorType.raOR,
        T_raOperatorType.raXOR :
          rr := 3 ;
        T_raOperatorType.raNoData :
          rr := 5 ;
        T_raOperatorType.raSign :
          rr := 9 ;
      end ;

      Result := rr ;
    end ;

    function operator_associativity(
      const _t : T_raOperatorType
    ) : T_raOperatorAssociativity ;
    var
      rr : T_raOperatorAssociativity ;
    begin
      case _t of
        T_raOperatorType.raLBracket :
          rr := T_raOperatorAssociativity.raNotAssociative ;
        T_raOperatorType.raLess,
        T_raOperatorType.raLessEqual,
        T_raOperatorType.raEqual,
        T_raOperatorType.raGreaterEqual,
        T_raOperatorType.raGreater,
        T_raOperatorType.raNotEqual :
          rr := T_raOperatorAssociativity.raNotAssociative ;
        T_raOperatorType.raAdd,
        T_raOperatorType.raMultiply,
        T_raOperatorType.raSign :
          rr := T_raOperatorAssociativity.raAssociative ;
        T_raOperatorType.raSubtract,
        T_raOperatorType.raDivide,
        T_raOperatorType.raDivInt,
        T_raOperatorType.raModulo :
          rr := T_raOperatorAssociativity.raLeftAssociative ;
        T_raOperatorType.raPower :
          rr := T_raOperatorAssociativity.raRightAssociative ;
        T_raOperatorType.raNOT,
        T_raOperatorType.raAND,
        T_raOperatorType.raOR,
        T_raOperatorType.raXOR :
          rr := T_raOperatorAssociativity.raNotAssociative ;
        T_raOperatorType.raNoData :
          rr := T_raOperatorAssociativity.raNotAssociative
        else rr := T_raOperatorAssociativity.raAssociative ;
      end ;

      Result := rr ;
    end ;

    function pop_item
      : Boolean ;
    var
      oo : T_raItem ;
    begin
      Result := False ;

      assert( not stack.IsEmpty );
      if stack.IsEmpty then
        exit ;

      oo := stack.Pop ;
      if ( oo.&Type <> T_raItemType.raOperator ) or
         ( T_raOperatorType( oo.Value ) <> T_raOperatorType.raLBracket ) then
        oRPN.Add( oo ) ;

      Result := True ;
    end ;

    function operator_left(
      const _t : T_raOperatorType
    ) : Boolean ;
    var
      oa : T_raOperatorAssociativity ;
    begin
      oa := operator_associativity( _t ) ;
      if ( oa = T_raOperatorAssociativity.raAssociative ) or
         ( oa = T_raOperatorAssociativity.raLeftAssociative ) then
        Result := True
      else
        Result := False ;
    end ;

    function operator_right(
      const _t : T_raOperatorType
    ) : Boolean ;
    var
      oa : T_raOperatorAssociativity ;
    begin
      oa := operator_associativity( _t ) ;
      if ( oa = T_raOperatorAssociativity.raAssociative ) or
         ( oa = T_raOperatorAssociativity.raRightAssociative ) then
        Result := True
      else
        Result := False ;
    end ;

    function operator_sign(
      const _t : T_raOperatorType
    ) : Boolean ;
    var
      vv : Single ;
      oo : T_raItem ;
      rr : Boolean ;
    begin
      Result := False ;
      vv := 0 ;

      if last <> T_raItemType.raOperator then
        exit ;

      if sb.Length > 0 then
        exit ;

      if _t = T_raOperatorType.raAdd then begin
        vv := 1.0 ;
        rr := True ;
      end
      else
      if _t = T_raOperatorType.raSubtract then begin
        vv := -1.0 ;
        rr := True ;
      end
      else
        rr := False ;

      if not rr then
        exit ;

      {$IFDEF GIS_NORECORDS}
        oo := new T_raItem ;
      {$ENDIF}
      oo.&Type := T_raItemType.raNumber ;
      oo.Value := vv ;
      oRPN.Add( oo ) ;

      {$IFDEF GIS_NORECORDS}
        oo := new T_raItem ;
      {$ENDIF}
      oo.&Type := T_raItemType.raOperator ;
      oo.Value := T_raOperatorType.raSign ;
      stack.Push( oo ) ;

      Result := True ;
    end ;

    function push_operator(
      const _t : T_raOperatorType
    ) : Boolean ;
    var
      oo : T_raItem ;
      ll : Boolean ;
      rr : Boolean ;
      ii : Integer ;
      jj : Integer ;
    begin
      Result := False ;

      if last = T_raItemType.raOperator then begin
        if _t <> T_raOperatorType.raNOT then begin
          if _t <> T_raOperatorType.raLBracket then
            exit ;
        end ;
      end ;

      {$IFDEF GIS_NORECORDS}
        oo := new T_raItem ;
      {$ENDIF}
      oo.&Type := T_raItemType.raOperator ;
      oo.Value := _t ;

      case _t of
        T_raOperatorType.raLBracket :
          begin
            stack.Push( oo ) ;
          end ;
        T_raOperatorType.raRBracket :
          begin
            if stack.IsEmpty then
              exit ;

            while top_operator <> T_raOperatorType.raLBracket do begin
              if not pop_item then
                exit ;
            end ;

            pop_item ;

            if ( not stack.IsEmpty ) and
               ( stack.Top.&Type = T_raItemType.raFunction ) then
              pop_item ;
          end ;
        T_raOperatorType.raComma :
          begin
            if stack.IsEmpty then
              exit ;

            while top_operator <> T_raOperatorType.raLBracket do begin
              if not pop_item then
                exit ;
            end ;
          end ;
        else
          begin
            ll := operator_left( _t ) ;
            rr := operator_right( _t ) ;
            ii := operator_order( _t ) ;
            while not stack.IsEmpty do begin
              jj := operator_order( top_operator ) ;
              if ll and ( ii <= jj ) then
                pop_item
              else
              if rr and ( ii < jj ) then
                pop_item
              else
                break ;
            end ;
            stack.Push( oo ) ;
          end ;
      end ;

      if _t = T_raOperatorType.raRBracket then
        last := T_raItemType.raNumber
      else
        last := T_raItemType.raOperator ;

      Result := True ;
    end ;


    function push_function(
      const _f : Integer
    ) : Boolean ;
    var
      ff : T_raFunction ;
      oo : T_raItem ;
    begin
      Result := False ;

      assert( last = T_raItemType.raOperator ) ;
      if last <> T_raItemType.raOperator then
        exit ;

      if c <> '(' then begin
        ff := oraFunctions.Items[_f] ;
        if ff.NoData then begin
          {$IFDEF GIS_NORECORDS}
            oo := new T_raItem ;
          {$ENDIF}
          oo.&Type := T_raItemType.raNumber ;
          oo.Value := NaN ;
          oRPN.Add( oo ) ;
          last := T_raItemType.raNumber ;

          Result := True ;
          exit ;
        end
        else
          exit ;
      end ;

      {$IFDEF GIS_NORECORDS}
        oo := new T_raItem ;
      {$ENDIF}
      oo.&Type := T_raItemType.raFunction ;
      oo.Value := _f ;

      stack.Push( oo ) ;
      last := T_raItemType.raFunction ;

      Result := True ;
    end ;

    procedure add_number(
      const _v : Single
    ) ;
    var
      oo : T_raItem ;
    begin
      if last <> T_raItemType.raOperator then
        exit ;

      {$IFDEF GIS_NORECORDS}
        oo := new T_raItem ;
      {$ENDIF}
      oo.&Type := T_raItemType.raNumber ;
      oo.Value := _v ;

      oRPN.Add( oo ) ;
      last := T_raItemType.raNumber ;
    end ;

    function find_variable(
      const _v : T_raVariable
    ) : Integer ;
    var
      vv : T_raVariable ;
      ii : Integer ;
      rr : Integer ;
    begin
      rr := -1 ;

      if _v.IsVector then begin

        for ii := 0 to Variables.Count - 1 do begin
          vv := Variables[ii] ;
          if not vv.IsVector then
            continue ;
          if ( CompareText( _v.Vector.Name, vv.Vector.Name ) = 0 ) and
             ( CompareText( _v.Field, vv.Field ) = 0 ) then begin
            rr := ii ;
            break ;
          end ;
        end ;

      end
      else begin

        for ii := 0 to Variables.Count - 1 do begin
          vv := Variables[ii] ;
          if vv.IsVector then
            continue ;
          if ( CompareText( _v.Pixel.Name, vv.Pixel.Name ) = 0 ) and
             ( _v.Band = vv.Band ) then begin
            rr := ii ;
            break ;
          end ;
        end ;

      end ;

      Result := rr ;
    end ;

    function get_band(
      const _s : String ;
      const _b : Boolean
    ) : Integer ;
    var
      ii : Integer ;
    begin
      if length( _s ) > 0 then begin
        if _b then begin // grid
          if TryStrToInt( _s, ii ) then begin
            if ii < 1 then
              ii := GIS_RA_BAND_ERROR ;
          end
          else
            ii := GIS_RA_BAND_ERROR ;
        end
        else begin // pixel
          case _s[StringFirst] of
            'H'  : ii := -1 ;
            'S'  :
              begin
                if length( _s ) < 2 then
                  ii := GIS_RA_BAND_ERROR
                else begin
                  case _s[StringFirst+1] of
                    'L' : ii := -2 ;
                    'V' : ii := -3 ;
                  end ;
                end ;
              end ;
            'L'  : ii := -4 ;
            'V'  : ii := -5 ;
            'R'  : ii := 1 ;
            'G'  : ii := 2 ;
            'B'  : ii := 3 ;
            'A'  : ii := 4 ;
            else begin
              if TryStrToInt( _s, ii ) then begin
                if ii < 1 then
                  ii := GIS_RA_BAND_ERROR ;
              end
              else
                ii := GIS_RA_BAND_ERROR ;
            end ;
          end ;
        end ;
      end
      else
        ii := 0 ;

      Result := ii ;
    end ;

    function add_variable(
      const _l : String ;
      const _r : String
    ) : Boolean ;
    var
      vv  : T_raVariable ;
      oo  : T_raItem ;
      ll  : TGIS_Layer ;
      llv : TGIS_LayerVector ;
      llp : TGIS_LayerPixel ;
      ff  : TGIS_FieldInfo ;
      ii  : Integer ;
    begin
      Result := False ;

      if last <> T_raItemType.raOperator then
        exit ;

      ll := findLayer( _l ) ;
      if not assigned( ll ) then
        exit ;

      if ll.IsVector then begin

        if Variables.Count = 0 then
          exit ;

        llv := TGIS_LayerVector( ll ) ;

        if IsStringEmpty( _r ) then
          exit ;

        ii := llv.FindField( _r ) ;
        if ii < 0 then
          exit ;

        ff := llv.FieldInfo( ii ) ;
        if ( ff.FieldType <> TGIS_FieldType.Number ) and
           ( ff.FieldType <> TGIS_FieldType.Float  ) then
          exit ;

        {$IFDEF GIS_NORECORDS}
          vv := new T_raVariable ;
        {$ENDIF}
        vv.IsVector := True ;
        vv.Vector := llv ;
        vv.Pixel := nil ;
        vv.Field := _r ;

        ii := find_variable( vv ) ;
        if ii < 0 then
          {$IFDEF OXYGENE}
            ii := Variables.Count ;
            Variables.Add( vv ) ;
          {$ELSE}
            ii := Variables.Add( vv ) ;
          {$ENDIF}

        {$IFDEF GIS_NORECORDS}
          oo := new T_raItem ;
        {$ENDIF}
        oo.&Type := T_raItemType.raVector ;
        oo.Value := ii ;

        oRPN.Add( oo ) ;
        last := T_raItemType.raVector ;

      end
      else begin

        llp := TGIS_LayerPixel( ll ) ;

        ii := get_band( _r, ll.IsGrid ) ;

        if ii = GIS_RA_BAND_ERROR then
          exit ;

        if llp.IsGrid and ( ii < 0 ) then
          exit ;

        if ii > llp.BandsCount then
          exit ;

        {$IFDEF GIS_NORECORDS}
          vv := new T_raVariable ;
        {$ENDIF}
        vv.IsVector := False ;
        vv.Pixel := llp ;
        vv.Vector := nil ;
        if ii = 0 then begin
          if llp.IsGrid then
            vv.Band := 1
          else
            vv.Band := 0 ;
        end
        else
          vv.Band := ii ;

        ii := find_variable( vv ) ;
        if ii < 0 then begin
          {$IFDEF OXYGENE}
            ii := Variables.Count ;
            Variables.Add( vv ) ;
          {$ELSE}
            ii := Variables.Add( vv ) ;
          {$ENDIF}
        end ;

        if Variables.Count > 1 then begin
          {$IFDEF GIS_NORECORDS}
            oo := new T_raItem ;
          {$ENDIF}
          oo.&Type := T_raItemType.raPixel ;
          oo.Value := ii ;

          oRPN.Add( oo ) ;
          last := T_raItemType.raPixel ;
        end ;

      end ;

      Result := True ;
    end ;

    function find_statistic(
      const _v : T_raStatistic
    ) : Integer ;
    var
      vv : T_raStatistic ;
      ii : Integer ;
      rr : Integer ;
    begin
      rr := -1 ;

      if _v.IsVector then begin

        for ii := 0 to Statistics.Count - 1 do begin
          vv := Statistics[ii] ;
          if not vv.IsVector then
            continue ;
          if ( CompareText( _v.Vector.Name, vv.Vector.Name ) = 0 ) and
             ( CompareText( _v.Field, vv.Field ) = 0 ) and
             ( _v.Func = vv.Func ) then begin
            rr := ii ;
            break ;
          end ;
        end ;

      end
      else begin

        for ii := 0 to Statistics.Count - 1 do begin
          vv := Statistics[ii] ;
          if vv.IsVector then
            continue ;
          if ( CompareText( _v.Pixel.Name, vv.Pixel.Name ) = 0 ) and
             ( _v.Band = vv.Band ) and
             ( _v.Func = vv.Func ) then begin
            rr := ii ;
            break ;
          end ;
        end ;

      end ;

      Result := rr ;
    end ;

    function find_stat_func(
      const _s : String ;
        out _f : TGIS_StatisticalFunction
    ) : Boolean ;
    begin
      Result := True ;

      if CompareText( _s, 'AVG' ) = 0 then
        _f := TGIS_StatisticalFunction.Average
      else
      if CompareText( _s, 'COUNT' ) = 0 then
        _f := TGIS_StatisticalFunction.Count
      else
      if CompareText( _s, 'COUNTNULL' ) = 0 then
        _f := TGIS_StatisticalFunction.CountMissings
      else
      if CompareText( _s, 'MAX' ) = 0 then
        _f := TGIS_StatisticalFunction.Max
      else
      if CompareText( _s, 'MAJORITY' ) = 0 then
        _f := TGIS_StatisticalFunction.Majority
      else
      if CompareText( _s, 'MEDIAN' ) = 0 then
        _f := TGIS_StatisticalFunction.Median
      else
      if CompareText( _s, 'MIN' ) = 0 then
        _f := TGIS_StatisticalFunction.Min
      else
      if CompareText( _s, 'MINORITY' ) = 0 then
        _f := TGIS_StatisticalFunction.Minority
      else
      if CompareText( _s, 'RANGE' ) = 0 then
        _f := TGIS_StatisticalFunction.Range
      else
      if CompareText( _s, 'STDEV' ) = 0 then
        _f := TGIS_StatisticalFunction.StandardDeviation
      else
      if CompareText( _s, 'SUM' ) = 0 then
        _f := TGIS_StatisticalFunction.Sum
      else
      if CompareText( _s, 'VARIANCE' ) = 0 then
        _f := TGIS_StatisticalFunction.Variance
      else
      if CompareText( _s, 'VARIETY' ) = 0 then
        _f := TGIS_StatisticalFunction.Variety
      else
        Result := False ;
    end ;

    function add_statistic(
      const _l : String ;
      const _r : String ;
      const _f : TGIS_StatisticalFunction
    ) : Boolean ;
    var
      vv  : T_raStatistic ;
      oo  : T_raItem ;
      ll  : TGIS_Layer ;
      llv : TGIS_LayerVector ;
      llp : TGIS_LayerPixel ;
      ff  : TGIS_FieldInfo ;
      ii  : Integer ;
    begin
      Result := False ;

      if last <> T_raItemType.raOperator then
        exit ;

      ll := findLayer( _l ) ;
      if not assigned( ll ) then
        exit ;

      if ll.IsVector then begin

        llv := TGIS_LayerVector( ll ) ;

        if IsStringEmpty( _r ) then
          exit ;

        ii := llv.FindField( _r ) ;
        if ii < 0 then
          exit ;

        ff := llv.FieldInfo( ii ) ;
        if ( ff.FieldType <> TGIS_FieldType.Number ) and
           ( ff.FieldType <> TGIS_FieldType.Float  ) then
          exit ;

        {$IFDEF GIS_NORECORDS}
          vv := new T_raStatistic ;
        {$ENDIF}
        vv.IsVector := True ;
        vv.Vector := llv ;
        vv.Field := _r ;
        vv.Func := _f ;

        ii := find_statistic( vv ) ;
        if ii < 0 then
          {$IFDEF OXYGENE}
            ii := Statistics.Count ;
            Statistics.Add( vv ) ;
          {$ELSE}
            ii := Statistics.Add( vv ) ;
          {$ENDIF}

        {$IFDEF GIS_NORECORDS}
          oo := new T_raItem ;
        {$ENDIF}
        oo.&Type := T_raItemType.raStatistic ;
        oo.Value := ii ;
        oRPN.Add( oo ) ;
        last := T_raItemType.raStatistic ;

      end
      else begin

        llp := TGIS_LayerPixel( ll ) ;

        ii := get_band( _r, ll.IsGrid ) ;

        if ii = GIS_RA_BAND_ERROR then
          exit ;

        if llp.IsGrid and ( ii < 0 ) then
          exit ;

        if ii > llp.BandsCount then
          exit ;

        {$IFDEF GIS_NORECORDS}
          vv := new T_raStatistic ;
        {$ENDIF}
        vv.IsVector := False ;
        vv.Pixel := llp ;
        vv.Func := _f ;
        if ii = 0 then begin
          if llp.IsGrid then
            vv.Band := 1
          else
            exit ;
        end
        else
          vv.Band := ii ;

        ii := find_statistic( vv ) ;
        if ii < 0 then
          {$IFDEF OXYGENE}
            ii := Statistics.Count ;
            Statistics.Add( vv ) ;
          {$ELSE}
            ii := Statistics.Add( vv ) ;
          {$ENDIF}

        {$IFDEF GIS_NORECORDS}
          oo := new T_raItem ;
        {$ENDIF}
        oo.&Type := T_raItemType.raStatistic ;
        oo.Value := ii ;
        oRPN.Add( oo ) ;
        last := T_raItemType.raStatistic ;

      end ;

      Result := True ;
    end ;

    function proc_string(
      const _l : String ;
      const _r : String
    ) : Boolean ;
    var
      ff : Integer ;
      ss : Single ;
      ii : Integer ;
      bb : Boolean ;
    begin
      Result := True ;

      if TryStrToInt( _l, ii ) then begin
        if length( _r ) = 0 then begin
          ss := DotStrToFloat( _l ) ;
          add_number( ss ) ;
          exit ;
        end
        else
        if TryStrToInt( _r, ii ) then begin
          ss := DotStrToFloat( _l + '.' + _r ) ;
          add_number( ss ) ;
          exit ;
        end
        else begin
          Result := False ;
          exit ;
        end ;
      end
      else begin
        bb := True ;
        if CompareText( _l, 'PI' ) = 0 then
          add_number( Pi )
        else
        if CompareText( _l, 'E' ) = 0 then
          add_number( Exp( 1 ) )
        else
        if CompareText( _l, 'DIV' ) = 0 then
          push_operator( T_raOperatorType.raDivInt )
        else
        if CompareText( _l, 'MOD' ) = 0 then
          push_operator( T_raOperatorType.raModulo )
        else
        if CompareText( _l, 'NOT' ) = 0 then
          push_operator( T_raOperatorType.raNOT )
        else
        if CompareText( _l, 'AND' ) = 0 then
          push_operator( T_raOperatorType.raAND )
        else
        if CompareText( _l, 'OR' ) = 0 then
          push_operator( T_raOperatorType.raOR )
        else
        if CompareText( _l, 'XOR' ) = 0 then
          push_operator( T_raOperatorType.raXOR )
        else begin
          if skip then
            skip := False
          else begin
            if find_stat_func( _l, sfunc ) then begin
              state := 6 ;
              exit ;
            end ;
          end ;

          ff := raFunctions.Find( _l ) ;
          if ff >= 0 then begin
            if not push_function( ff ) then begin
              Result := False ;
              exit ;
            end ;
          end
          else
            bb := False ;
        end ;

        if bb then
          exit ;
      end ;

      if not add_variable( _l, _r ) then
        Result := False ;
    end ;

  begin
    Result := -1 ;

    oRPN.Clear ;
    Variables.Clear ;

    bReady := False ;
    i := 0 ;
    j := 0 ;
    state := 0 ;
    prt2 := False ;
    sb := TStringBuilder.Create ;
    stack := TGIS_Stack<T_raItem>.Create ;
    len := StringLast( _expression ) + 1 ;
    opr1 := T_raOperatorType.raNULL ;
    last := T_raItemType.raOperator ;
    skip := False ;
    quot1 := False ;
    quot2 := False ;
    try
      i := StringFirst - 1 ;
      while i < len do begin
        inc( i ) ;

        if i < len then
          c := _expression[i]
        else
          c := ' ' ;

        if last = T_raItemType.raFunction then begin
          if c <> '(' then
            exit ;
        end ;

        case state of
          0 : // whitespaces before result
            begin
              if is_whitespace then
                continue
              else
              if is_quote then begin
                state := 1 ;
                continue ;
              end
              else begin
                if not is_alphanumeric then
                  exit ;
                sb.Append( c ) ;
                state := 1 ;
              end ;
            end ;
          1 : // result name
            begin
              if quoted then begin
                if not is_quote then
                  sb.Append( c ) ;
              end
              else
              if is_whitespace then begin
                rstr := sb.ToString ;
                sb.Clear ;
                if not add_variable( rstr, '' ) then
                  exit ;
                state := 3 ;
              end
              else begin
                case c of
                  '.' :
                    begin
                      lstr := sb.ToString ;
                      sb.Clear ;
                      state := 2 ;
                    end ;
                  '=' :
                    begin
                      rstr := sb.ToString ;
                      sb.Clear ;
                      if not add_variable( rstr, '' ) then
                        exit ;
                      state := 4 ;
                    end ;
                  else begin
                    if not is_alphanumeric then
                      exit ;
                    sb.Append( c ) ;
                  end ;
                end ;
              end ;
            end ;
          2 : // result band
            begin
              if is_whitespace then begin
                rstr := sb.ToString ;
                sb.Clear ;
                if not add_variable( lstr, rstr ) then
                  exit ;
                state := 3 ;
              end
              else begin
                case c of
                  '=' :
                    begin
                      rstr := sb.ToString ;
                      sb.Clear ;
                      if not add_variable( lstr, rstr ) then
                        exit ;
                      state := 4 ;
                    end ;
                  else begin
                    if not is_alphanumeric then
                      exit ;
                    sb.Append( c ) ;
                  end ;
                end ;
              end ;
            end ;
          3 : // whitespaces before =
            begin
              if is_whitespace then
                continue
              else begin
                if not is_equality then
                  exit ;
                state := 4 ;
                last := T_raItemType.raOperator ;
              end ;
            end ;
          4 : // whitespaces after =
            begin
              if quoted then begin
                if not is_quote then
                  sb.Append( c )
                else
                  state := 5 ;
              end
              else
              if is_quote then
                continue
              else
              if is_whitespace then
                continue
              else begin
                opr := get_operator ;
                if opr = T_raOperatorType.raError then
                  exit
                else
                if opr = T_raOperatorType.raNotNULL then begin
                  if last = T_raItemType.raOperator then
                    exit
                  else
                    continue ;
                end
                else
                if opr <> T_raOperatorType.raNULL then begin
                  if operator_sign( opr ) then
                    continue
                  else
                  if not push_operator( opr ) then
                    exit ;
                  case opr of
                    T_raOperatorType.raLess,
                    T_raOperatorType.raGreater :
                      dec( i ) ;
                  end ;
                end
                else begin
                  sb.Append( c ) ;
                  state := 5 ;
                end ;
              end ;
            end ;
          5 : // RHS of formula
            begin
              opr := get_operator ;
              if operator_sign( opr ) then
                continue
              else
              if is_whitespace or
                ( opr <> T_raOperatorType.raNULL ) then begin
                if prt2 then begin
                  rstr := sb.ToString ;
                  if not proc_string( lstr, rstr ) then
                    exit ;
                end
                else begin
                  lstr := sb.ToString ;
                  if not proc_string( lstr, '' ) then
                    exit ;
                end ;

                sb.Clear ;
                prt2 := False ;

                if state = 6 then begin
                  if opr <> T_raOperatorType.raLBracket then
                    exit ;
                  j := i - 4 ;
                  continue ;
                end ;

                state := 4 ;

                if opr = T_raOperatorType.raNotNULL then
                  continue
                else
                if opr <> T_raOperatorType.raNULL then begin
                  if not push_operator( opr ) then
                    exit ;
                end ;

                continue ;
              end ;

              case c of
                '.' :
                  begin
                    if prt2 then
                      exit ;
                    lstr := sb.ToString ;
                    sb.Clear ;
                    prt2 := True ;
                  end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
          6 : // argument of a statistical function
            begin
              case c of
                ',' :
                  begin
                    sb.Clear ;
                    prt2 := False ;
                    state := 4 ;
                    skip := True ;
                    i := j ;
                  end ;
                '.' :
                  begin
                    lstr := TrimLeft( sb.ToString ) ;
                    sb.Clear ;
                    prt2 := True ;
                  end ;
                ')' :
                  begin
                    if prt2 then begin
                      rstr := TrimRight( sb.ToString ) ;
                      if not add_statistic( lstr, rstr, sfunc ) then
                        exit ;
                    end
                    else begin
                      lstr := Trim( sb.ToString ) ;
                      if not add_statistic( lstr, '', sfunc ) then
                        exit ;
                    end ;

                    sb.Clear ;
                    prt2 := False ;
                    state := 4 ;
                  end ;
                else
                  sb.Append( c ) ;
              end ;
            end ;
        end ;
      end ;

      if last = T_raItemType.raOperator then
        exit ;

      while not stack.IsEmpty do begin
        if ( stack.Top.&Type = T_raItemType.raOperator ) and
           ( T_raOperatorType( stack.Top.Value )
               = T_raOperatorType.raLBracket ) then
          exit ;

        pop_item ;
      end ;

      if ( state < 4 ) or ( oRPN.Count = 0 ) then
        exit ;

      bReady := True ;
    finally
      if not bReady then
        Result := i ;
      FreeObject( stack ) ;
      FreeObject( sb ) ;
    end ;

    SetLength( Values, Variables.Count ) ;
    prepStats ;
  end ;


  function T_raParser.Pop
    : Single ;
  var
    tmp : T_raItem ;
  begin
    Result := 0 ;
    tmp := oStack.Pop ;
    case tmp.&Type of
      T_raItemType.raNumber :
        Result := VarToSingle( tmp.Value ) ;
      T_raItemType.raVector,
      T_raItemType.raPixel :
        Result := Values[VarToInt32( tmp.Value )] ;
      T_raItemType.raStatistic :
        Result := StatValues[VarToInt32( tmp.Value )] ;
    end ;
  end ;


  function T_raParser.Evaluate
    : Single ;
  var
    itm   : T_raItem ;
    val   : T_raItem ;
    vals  : TGIS_SingleArray ;
    v0    : Single ;
    v1    : Single ;
    b0    : Byte ;
    b1    : Byte ;
    res   : Single ;
    i     : Integer ;

    procedure eval_function ;
    var
      ff : T_raFunction ;
    begin
      ff := oraFunctions.Items[VarToInt32( itm.Value )] ;
      val.&Type := T_raItemType.raNumber ;
      val.Value := ff.Evaluate( Self ) ;
      oStack.Push( val ) ;
    end ;

  begin
    if not bReady then begin
      Result := NaN ;
      exit ;
    end ;

    res := NaN ;
    v1 := 0 ;
    SetLength( vals, 4 ) ;
    oStack := TGIS_Stack<T_raItem>.Create ;
    try
      for i := 0 to oRPN.Count - 1 do begin
        itm := oRPN[i] ;
        {$IFDEF GIS_NORECORDS}
          val := new T_raItem ;
        {$ENDIF}
        case itm.&Type of
          T_raItemType.raNumber,
          T_raItemType.raVector,
          T_raItemType.raPixel,
          T_raItemType.raStatistic :
            begin
              {$IFDEF GIS_NORECORDS}
                val.&Type := itm.&Type ;
                val.Value := itm.Value ;
                oStack.Push( val ) ;
              {$ELSE}
                oStack.Push( itm ) ;
              {$ENDIF}
            end ;
          T_raItemType.raOperator :
            begin
              if T_raOperatorType( itm.Value ) <> T_raOperatorType.raNOT then
                v1 := Pop ;
              v0 := Pop ;

              case T_raOperatorType( itm.Value ) of
                T_raOperatorType.raAdd :
                  begin
                    if IsNan( v0 ) then
                      val.Value := v1
                    else
                    if IsNan( v1 ) then
                      val.Value := v0
                    else
                      val.Value := v0 + v1 ;
                  end ;
                T_raOperatorType.raSubtract :
                  begin
                    if IsNan( v0 ) then
                      val.Value := NaN
                    else
                    if IsNan( v1 ) then
                      val.Value := v0
                    else
                      val.Value := v0 - v1 ;
                  end ;
                T_raOperatorType.raMultiply :
                  begin
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := NaN
                    else
                      val.Value := v0 * v1 ;
                  end ;
                T_raOperatorType.raDivide :
                  begin
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := NaN
                    else
                    if v1 = 0.0 then
                      val.Value := NaN
                    else
                      val.Value := v0/v1 ;
                  end ;
                T_raOperatorType.raDivInt :
                  begin
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := NaN
                    else
                    if v1 = 0.0 then
                      val.Value := NaN
                    else
                      val.Value := 1.0*TruncS( v0/v1 ) ;
                  end ;
                T_raOperatorType.raModulo :
                  begin
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := NaN
                    else
                    if v1 = 0.0 then
                      val.Value := NaN
                    else begin
                      val.Value := 1.0*TruncS( v0 / v1 ) ;
                      val.Value := v0 - v1*VarToSingle( val.Value ) ;
                    end ;
                  end ;
                T_raOperatorType.raPower :
                  begin
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := NaN
                    else
                    if ( v0 = 0.0 ) and ( v1 < 0.0 ) then
                      val.Value := NaN
                    else
                      val.Value := Power( v0, v1 ) ;
                  end ;
                T_raOperatorType.raNOT :
                  begin
                    if IsNan( v0 ) then
                      val.Value := NaN
                    else
                    if v0 = 0.0 then
                      val.Value := 1.0
                    else
                      val.Value := 0.0 ;
                  end ;
                T_raOperatorType.raAND :
                  begin
                    if IsNan( v0 ) then v0 := 0.0 ;
                    if IsNan( v1 ) then v1 := 0.0 ;

                    if v0 = 0.0 then b0 := 0 else b0 := 1 ;
                    if v1 = 0.0 then b1 := 0 else b1 := 1 ;

                    val.Value := b0 and b1 ;
                  end ;
                T_raOperatorType.raOR :
                  begin
                    if IsNan( v0 ) then v0 := 0.0 ;
                    if IsNan( v1 ) then v1 := 0.0 ;

                    if v0 = 0.0 then b0 := 0 else b0 := 1 ;
                    if v1 = 0.0 then b1 := 0 else b1 := 1 ;

                    val.Value := b0 or b1 ;
                  end ;
                T_raOperatorType.raXOR :
                  begin
                    if IsNan( v0 ) then v0 := 0.0 ;
                    if IsNan( v1 ) then v1 := 0.0 ;

                    if v0 = 0.0 then b0 := 0 else b0 := 1 ;
                    if v1 = 0.0 then b1 := 0 else b1 := 1 ;

                    val.Value := b0 xor b1 ;
                  end ;
                T_raOperatorType.raLess :
                  begin
                    if IsNan( v1 ) then begin
                      if IsNan( v0 ) then
                        val.Value := NaN
                      else
                        val.Value := 0.0 ;
                    end
                    else
                    if IsNan( v0 ) or ( v0 < v1 ) then
                      val.Value := 1.0
                    else
                      val.Value := 0.0 ;
                  end ;
                T_raOperatorType.raLessEqual :
                  begin
                    if IsNan( v1 ) then begin
                      if IsNan( v0 ) then
                        val.Value := NaN
                      else
                        val.Value := 0.0 ;
                    end
                    else
                    if IsNan( v0 ) or ( v0 <= v1 ) then
                      val.Value := 1.0
                    else
                      val.Value := 0.0 ;
                  end ;
                T_raOperatorType.raEqual :
                  begin
                    if IsNan( v0 ) and IsNan( v1 ) then
                      val.Value := NaN
                    else
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := 0.0
                    else
                    if v0 = v1 then
                      val.Value := 1.0
                    else
                      val.Value := 0.0 ;
                  end ;
                T_raOperatorType.raGreaterEqual :
                  begin
                    if IsNan( v0 ) then begin
                      if IsNan( v1 ) then
                        val.Value := NaN
                      else
                        val.Value := 0.0 ;
                    end
                    else
                    if IsNan( v1 ) or ( v0 >= v1 ) then
                      val.Value := 1.0
                    else
                      val.Value := 0.0 ;
                  end ;
                T_raOperatorType.raGreater :
                  begin
                    if IsNan( v0 ) then begin
                      if IsNan( v1 ) then
                        val.Value := NaN
                      else
                        val.Value := 0.0 ;
                    end
                    else
                    if IsNan( v1 ) or ( v0 > v1 ) then
                      val.Value := 1.0
                    else
                      val.Value := 0.0 ;
                  end ;
                T_raOperatorType.raNotEqual :
                  begin
                    if IsNan( v0 ) and IsNan( v1 ) then
                      val.Value := NaN
                    else
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := 1.0
                    else
                    if v0 = v1 then
                      val.Value := 0.0
                    else
                      val.Value := 1.0 ;
                  end ;
                T_raOperatorType.raNoData :
                  begin
                    if IsNan( v0 ) then
                      val.Value := v1
                    else
                      val.Value := v0 ;
                  end ;
                T_raOperatorType.raSign :
                  begin
                    if IsNan( v0 ) or IsNan( v1 ) then
                      val.Value := NaN
                    else
                      val.Value := v0 * v1 ;
                  end ;
              end ;
              val.&Type := T_raItemType.raNumber ;
              oStack.Push( val ) ;
            end ;
          T_raItemType.raFunction :
            begin
              eval_function ;
            end ;
        end ;
      end ;

      assert( oStack.Count = 1 ) ;
      itm := oStack.Pop ;
      case itm.&Type of
        T_raItemType.raNumber    : res := VarToSingle( itm.Value ) ;
        T_raItemType.raVector,
        T_raItemType.raPixel     : res := Values[VarToInt32( itm.Value )] ;
        T_raItemType.raStatistic : res := StatValues[VarToInt32( itm.Value )] ;
      end ;
    finally
      FreeObject( oStack ) ;
    end ;

    Result := res ;
  end ;


//==============================================================================
// TGIS_RasterAlgebra
//==============================================================================

  constructor TGIS_RasterAlgebra.Create ;
  begin
    inherited ;

    oLayers := TList<TGIS_Layer>.Create ;
    oAliases := TList<String>.Create ;

    busyEventManager := TGIS_BusyEventManager.Create( Self ) ;
  end ;

  procedure TGIS_RasterAlgebra.doDestroy ;
  begin
    FreeObject( oLayers ) ;
    FreeObject( oAliases ) ;
    FreeObject( busyEventManager ) ;

    inherited ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_RasterAlgebra.fadd_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent += _value ;
    end ;

    procedure TGIS_RasterAlgebra.fremove_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent -= _value ;
    end ;
  {$ELSE}
    procedure TGIS_RasterAlgebra.fset_BusyEvent(
      const _value : TGIS_BusyEvent
    ) ;
    begin
      busyEventManager.BusyEvent := _value ;
    end ;

    function TGIS_RasterAlgebra.fget_BusyEvent : TGIS_BusyEvent ;
    begin
      Result := busyEventManager.BusyEvent ;
    end ;
  {$ENDIF}

  procedure TGIS_RasterAlgebra.addLayerAlias(
    const _layer : TGIS_Layer ;
    const _alias : String
  ) ;
  begin
    oLayers.Add( _layer ) ;
    oAliases.Add( _alias ) ;
  end ;

  procedure TGIS_RasterAlgebra.AddLayer(
    const _layer : TGIS_Layer
  ) ;
  begin
    addLayerAlias( _layer, '' ) ;
  end ;

  procedure TGIS_RasterAlgebra.Execute(
    const _expression : String
  ) ;
  var
    prsr  : T_raParser ;
    bpix  : Boolean ;
    src   : TGIS_Layer ;
    dst   : TGIS_LayerPixel ;
    ext   : TGIS_Extent ;
    psiz  : Double ;
    pt    : TGIS_Point ;
    shp   : TGIS_Shape ;
    alck  : array of T_raLock ;
    aidx  : array of Integer ;
    cnt   : Integer ;
    lock  : array of TGIS_LayerPixelLock ;
    bnds  : array of TRect ;
    wdth  : Integer ;
    hght  : Integer ;
    ptw   : Double ;
    pth   : Double ;
    rad   : Double ;
    vrt   : Variant ;
    val   : Single ;
    band  : Integer ;
    vmin  : Single ;
    vmax  : Single ;
    ndv   : Single ;
    w     : Integer ;
    h     : Integer ;
    i     : Integer ;
    k     : Integer ;
    err   : Integer ;
    abrt  : Boolean ;

    function find_lock : Integer ;
    var
      ll : TGIS_LayerPixel ;
      bb : Integer ;
      vv : T_raVariable ;
      ii : Integer ;
    begin
      Result := -1 ;

      vv := prsr.Variables[i] ;
      for ii := 0 to cnt - 1 do begin
        ll := alck[ii].Layer ;
        bb := alck[ii].Band ;
        if vv.IsVector then
          continue ;
        if ( CompareText( vv.Pixel.Name, ll.Name ) = 0 ) and
           ( ( vv.Band = bb ) or
             ( ( vv.Band < 1 ) and ( bb < 1 ) )
           ) then begin
          Result := ii ;
          break ;
        end ;
      end ;
    end ;

    function make_lock(
      const _i : Integer ;
      const _s : Double ;
      const _w : Boolean
    ) : TGIS_LayerPixelLock ;
    var
      vv  : T_raVariable ;
      ll  : TGIS_LayerPixel ;
      lck : TGIS_LayerPixelLock ;
    begin
      vv := prsr.Variables[_i] ;

      ll := vv.Pixel ;

      if _s = 0.0 then
        band := prsr.Variables[0].Band ;

      if ll.IsGrid then begin
        if vv.Band <= 1 then
          lck := ll.LockPixels( ext, dst.CS, _s, _w )
        else
          lck := ll.LockPixels( ext, dst.CS, _s, vv.Band, _w ) ;
      end
      else begin
        if vv.Band <= 0 then
          lck := ll.LockPixels( ext, dst.CS, _s, _w )
        else
          lck := ll.LockPixels( ext, dst.CS, _s, vv.Band, _w ) ;
      end ;

      Result := lck ;
    end ;

    function get_hslv(
      const _c : Integer ;
      const _b : Integer
    ) : Single ;
    var
      hh, ss, ll, vv : Double ;
      cc : TGIS_Color ;
    begin
      Result := 0 ;
      cc := TGIS_Color.FromARGB( Cardinal( _c ) ) ;
      case _b of
        -1 :
          begin
            cc.ToHSL( hh, ss, ll ) ;
            Result := hh ;
          end ;
        -2 :
          begin
            cc.ToHSL( hh, ss, ll ) ;
            Result := ss ;
          end ;
        -3 :
          begin
            cc.ToHSV( hh, ss, vv ) ;
            Result := ss ;
          end ;
        -4 :
          begin
            cc.ToHSL( hh, ss, ll ) ;
            Result := ll ;
          end ;
        -5 :
          begin
            cc.ToHSV( hh, ss, vv ) ;
            Result := vv ;
          end ;
      end ;
    end ;

    function in_bounds(
      const _i : Integer
    ) : Boolean ;
    begin
      if ( h >= bnds[_i].Top  ) and ( h <= bnds[_i].Bottom ) and
         ( w >= bnds[_i].Left ) and ( w <= bnds[_i].Right  ) then
        Result := True
      else
        Result := False ;
    end ;

    function get_value(
      const _i : Integer
    ) : Single ;
    var
      bb : Integer ;
      pp : Integer ;
      rr : TRect ;
    begin
      if not in_bounds( _i ) then begin
        Result := NaN ;
        exit ;
      end ;

      bb := prsr.Variables[_i].Band ;
      rr := bnds[_i] ;
      if prsr.Variables[_i].Pixel.IsGrid then begin
        if bb <= 1 then
          Result := lock[_i].Grid[h-rr.Top,w-rr.Left]
        else
          Result := lock[_i].Band[h-rr.Top,w-rr.Left] ;
      end
      else begin
        if bb > 0 then
          Result := lock[_i].Band[h-rr.Top,w-rr.Left]
        else begin
          pp := lock[_i].BitmapPos(w-rr.Left,h-rr.Top) ;
          if bb = 0 then
            Result := lock[_i].Bitmap[pp]
          else
            Result := get_hslv( lock[_i].Bitmap[pp], bb ) ;
        end ;
      end ;
    end ;

    function calc_hslv(
      const _c : Integer
    ) : Integer ;
    var
      aa, hh, ss, ll, vv : Double ;
      cc : TGIS_Color ;
    begin
      Result := 0 ;
      cc := TGIS_Color.FromARGB( Cardinal( _c ) ) ;
      case band of
        -1 :
          begin
            cc.ToAHSL( aa, hh, ss, ll ) ;
            Result := Integer( TGIS_Color.FromAHSL( aa, val, ss, ll ).ToARGB ) ;
          end ;
        -2 :
          begin
            cc.ToAHSL( aa, hh, ss, ll ) ;
            Result := Integer( TGIS_Color.FromAHSL( aa, hh, val, ll ).ToARGB ) ;
          end ;
        -3 :
          begin
            cc.ToAHSV( aa, hh, ss, vv ) ;
            Result := Integer( TGIS_Color.FromAHSV( aa, hh, val, vv ).ToARGB ) ;
          end ;
        -4 :
          begin
            cc.ToAHSL( aa, hh, ss, ll ) ;
            Result := Integer( TGIS_Color.FromAHSL( aa, hh, ss, val ).ToARGB ) ;
          end ;
        -5 :
          begin
            cc.ToAHSV( aa, hh, ss, vv ) ;
            Result := Integer( TGIS_Color.FromAHSV( aa, hh, ss, val ).ToARGB ) ;
          end ;
      end ;
    end ;

    procedure set_value ;
    var
      pp : Integer ;
    begin
      if dst.IsGrid then begin
        if band <= 1 then
          lock[0].Grid[h,w] := val
        else
          lock[0].Band[h,w] := val ;
      end
      else begin
        if band > 0 then
          lock[0].Band[h,w] := val
        else begin
          pp := lock[0].BitmapPos(w,h) ;
          if band = 0 then
            lock[0].Bitmap[pp] := Integer( RoundS( val ) )
          else
            lock[0].Bitmap[pp] := calc_hslv( lock[0].Bitmap[pp] ) ;
        end ;
      end ;
    end ;

  begin
    wdth  := 0 ;
    hght  := 0 ;

    if oLayers.Count = 0 then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_RALGEBRA_NO_LAYERS ), '', 0
      ) ;

    bpix := False ;
    for i := 0 to oLayers.Count - 1 do
      bpix := bpix or ( not oLayers[i].IsVector ) ;

    if not bpix then
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_RALGEBRA_NO_PIXEL ), '', 0
      ) ;

    for i := 0 to oLayers.Count - 1 do
      oLayers[i].Open ;

    prsr := T_raParser.Create( oLayers, oAliases ) ;
    try
      err := prsr.Parse( _expression ) ;
      if err >= 0 then
        raise EGIS_Exception.Create(
          _rsrc( GIS_RS_ERR_RALGEBRA_EXPRESSION ), '', err
        ) ;

      dst := prsr.Variables[0].Pixel ;
      ext := dst.Extent ;

      if dst.IsGrid then
        ndv := dst.NoDataValue
      else
        ndv := 0 ;

      SetLength( alck, prsr.Variables.Count ) ;
      SetLength( aidx, prsr.Variables.Count ) ;
      SetLength( bnds, prsr.Variables.Count ) ;
      cnt := 1 ;
      for i := 0 to prsr.Variables.Count - 1 do begin
        if prsr.Variables[i].IsVector then
          continue ;
        if i = 0 then begin
          aidx[0] := 0 ;
          {$IFDEF GIS_NORECORDS}
            alck[0] := new T_raLock ;
          {$ENDIF}
          alck[0].Layer := prsr.Variables[0].Pixel ;
          alck[0].Band := prsr.Variables[0].Band ;
          alck[0].Lock := make_lock( 0, 0.0, True ) ;
          wdth := alck[0].Lock.Bounds.Width ;
          hght := alck[0].Lock.Bounds.Height ;
          psiz := alck[0].Lock.PixelSize.X ;
        end
        else begin
          k := find_lock ;
          if k >= 0 then
            aidx[i] := k
          else begin
            aidx[i] := cnt ;
            {$IFDEF GIS_NORECORDS}
              alck[cnt] := new T_raLock ;
            {$ENDIF}
            alck[cnt].Layer := prsr.Variables[i].Pixel ;
            alck[cnt].Band := prsr.Variables[i].Band ;
            alck[cnt].Lock := make_lock( i, psiz, False ) ;
            inc( cnt ) ;
          end ;
        end ;
        src := prsr.Variables[i].Pixel ;
        bnds[i] := Rect(
                     CeilS(  wdth*( src.Extent.XMin - dst.Extent.XMin )/
                                  ( dst.Extent.XMax - dst.Extent.XMin )
                     ),
                     hght -
                     FloorS( hght*( src.Extent.YMax - dst.Extent.YMin )/
                                  ( dst.Extent.YMax - dst.Extent.YMin )
                     ),
                     FloorS( wdth*( src.Extent.XMax - dst.Extent.XMin )/
                                  ( dst.Extent.XMax - dst.Extent.XMin )
                     ),
                     hght -
                     CeilS(  hght*( src.Extent.YMin - dst.Extent.YMin )/
                                  ( dst.Extent.YMax - dst.Extent.YMin )
                     )
                   ) ;

      end ;

      SetLength( lock, prsr.Variables.Count ) ;
      for i := 0 to prsr.Variables.Count - 1 do
        lock[i] := alck[aidx[i]].Lock ;

      inc( wdth ) ;
      inc( hght ) ;
      ptw := ( ext.XMax - ext.XMin )/wdth ;
      pth := ( ext.YMax - ext.YMin )/hght ;
      rad := 0.5*Sqrt(  ptw*ptw + pth*pth ) ;

      busyEventManager.StartEvent(
        _rsrc( GIS_RS_BUSY_ALGEBRA ), hght * wdth
      ) ;
      try
        for h := 0 to hght - 1 do begin
          for w := 0 to wdth - 1 do begin

            abrt := busyEventManager.PushEvent ;
            if abrt then
              break ;

            for i := 1 to prsr.Variables.Count - 1 do begin
              {$IFDEF OXYGENE}
                pt := TGIS_Utils.GisPoint(
                                ext.XMin + w*ptw,
                                ext.YMin + ( hght - h - 1 )*pth
                              ) ;
              {$ELSE}
                pt := GisPoint( ext.XMin + w*ptw,
                                ext.YMin + ( hght - h - 1 )*pth
                              ) ;
              {$ENDIF}
              if prsr.Variables[i].IsVector then begin
                shp := prsr.Variables[i].Vector.Locate( pt, rad ) ;
                if not assigned( shp ) then
                  prsr.Values[i] := NaN
                else begin
                  vrt := shp.GetField( prsr.Variables[i].Field ) ;
                  if vrt = Unassigned then
                    prsr.Values[i] := NaN
                  else
                    prsr.Values[i] := VarToSingle( vrt ) ;
                end ;
              end
              else begin
                prsr.Values[i] := get_value( i ) ;
                if IsNan( prsr.Values[i] ) or
                   ( prsr.Values[i] = prsr.Variables[i].Pixel.NoDataValue ) then
                  prsr.Values[i] := NaN
              end ;
            end ;

            val := prsr.Evaluate ;
            if IsNan( val ) then
              val := ndv ;
            set_value ;

          end ;
        end ;

        // set min/max for a grid result
        if dst.IsGrid then begin
          vmin :=  GIS_MAX_SINGLE ;
          vmax := -GIS_MAX_SINGLE ;
          ndv := dst.NoDataValue ;
          for h := 0 to hght - 1 do begin
            for w := 0 to wdth - 1 do begin
              val := get_value( 0 ) ;
              if val = ndv then
                continue ;
              vmin := Min( val, vmin ) ;
              vmax := Max( val, vmax ) ;
            end ;
          end ;
          dst.MinHeight := vmin ;
          dst.MaxHeight := vmax ;
        end ;
      finally
        for i := 0 to cnt - 1 do
          alck[i].Layer.UnlockPixels( alck[i].Lock ) ;
        busyEventManager.EndEvent ;
      end ;
    finally
      FreeObject( prsr ) ;
    end ;
  end ;

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
initialization
  Unit_GisRasterAlgebra.SelfRegisterPipeline ;

finalization
  FreeObject( oraFunctions ) ;
{$ENDIF}

//==================================== END =====================================
end.
