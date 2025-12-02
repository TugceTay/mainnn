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
  Pipeline operations.
}

{$IFDEF DCC}
  unit GisPipeline ;
  {$HPPEMIT '#pragma link "GisPipeline"'}
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
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Math,
    System.SysUtils,

    GisClasses,
    GisCsSystems,
    GisInterfaces,
    GisLayer,
    GisLayerPixel,
    GisLayerVector,
    GisResource,
    GisRtl,
    GisTypes,
    GisTypesUI;
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
  Unit_GisPipeline = class
    public
      class procedure SelfRegisterPipeline() ;
  end ;

  TGIS_Pipeline = class ;

  /// <summary>
  ///   Types of pipeline parameters
  /// </summary>
  TGIS_PipelineParameterType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   string value
    /// </summary>
    &String,
    /// <summary>
    ///   integer values
    /// </summary>
    &Int,
    /// <summary>
    ///   float value
    /// </summary>
    &Float,
    /// <summary>
    ///   boolean value
    /// </summary>
    &Boolean ) ;

  /// <summary>
  ///   Pipeline parameter  encapsulation.
  /// </summary>
  TGIS_PipelineParameter = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FName        : String ;
      FType        : TGIS_PipelineParameterType ;
      FMin         : Double ;
      FMax         : Double ;
      FRequired    : Boolean ;
      FDefault     : String ;
      FPredefined  : String ;
      FValue       : String ;

    public
      /// <summary>
      ///   Define pipeline parameter.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_type">
      ///   type of the parameter
      /// </param>
      /// <param name="_min">
      ///   minimum value of the parameter; NaN if undefined
      /// </param>
      /// <param name="_max">
      ///   maximum value of the parameter; NaN if undefined
      /// </param>
      /// <param name="_required">
      ///   True if parameter is obligatory
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter used, empty string if not defined
      /// </param>
      /// <param name="_predefined">
      ///   list of predefined values to be selected from, separated by '|';
      ///   if the list starts from '!' character then list is strict - only
      ///   only provided values are supported
      /// </param>
      constructor Create( const _name        : String  ;
                          const _type        : TGIS_PipelineParameterType ;
                          const _min         : Double  ;
                          const _max         : Double  ;
                          const _required    : Boolean ;
                          const _default     : String  ;
                          const _predefined  : String
                        ) ;

    public

      /// <summary>
      ///   Parameter name.
      /// </summary>
      property Name        : String
                             read  FName ;

      /// <summary>
      ///   Parameter type.
      /// </summary>
      property &Type       : TGIS_PipelineParameterType
                             read  FType ;

      /// <summary>
      ///   Minimum allowed value for numeric parameters. If NaN hen undefined.
      /// </summary>
      property Min         : Double
                             read  FMin ;

      /// <summary>
      ///   Maximum allowed value for numeric parameters. If NaN hen undefined.
      /// </summary>
      property Max         : Double
                             read  FMax ;

      /// <summary>
      ///   If true, then parameter is required.
      /// </summary>
      property Required    : Boolean
                             read  FRequired ;

      /// <summary>
      ///   Default value for the parameter.
      /// </summary>
      property &Default    : String
                             read  FDefault ;

      /// <summary>
      ///   Value of the parameter.
      /// </summary>
      property Value       : String
                             read  FValue
                             write FValue ;

      /// <summary>
      ///   Value of the parameter.
      /// </summary>
      property Predefined  : String
                             read  FPredefined ;
  end;

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Parameters list for Items.
  /// </summary>
  {$IFNDEF GIS_NOGENERICS}
    TGIS_PipelineParameterList = {$IFDEF OXYGENE} public {$ENDIF}
                                 TObjectList<TGIS_PipelineParameter> ;
  {$ELSE}
    TGIS_PipelineParameterList = class( TObjectList<TGIS_PipelineParameter> );
  {$ENDIF}

  /// <summary>
  ///   Single pipeline operation.
  /// </summary>
  TGIS_PipelineOperationAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                   class( TGIS_Object )
    private
      FName     : String ;
      FLine     : Integer ;
      FPipeline : TGIS_Pipeline ;
      FResult   : Boolean ;

    private
      lstParams : TGIS_PipelineParameterList ;
      dctParams : TDictionary<String,TGIS_PipelineParameter> ;

    private
      procedure doBusyEventEx(     _sender : TObject ;
                                   _pos    : Integer ;
                                   _end    : Integer ;
                               var _abort  : Boolean
                              ) ;

    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <summary>
      ///   Define pipeline parameter.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_type">
      ///   type of the parameter
      /// </param>
      /// <param name="_min">
      ///   minimum value of the parameter; NaN if undefined
      /// </param>
      /// <param name="_max">
      ///   maximum value of the parameter; NaN if undefined
      /// </param>
      /// <param name="_required">
      ///   True if parameter is obligatory
      /// </param>
      /// <param name="_default">
      ///   default value of the parameter used, empty string if not defined
      /// </param>
      /// <param name="_predefined">
      ///   list of predefined values to be selected from, separated by '|';
      ///   if the list starts from '!' character then list is strict - only
      ///   only provided values are supported
      /// </param>
      procedure defineParam( const _name        : String  ;
                             const _type        : TGIS_PipelineParameterType ;
                             const _min         : Double  ;
                             const _max         : Double  ;
                             const _required    : Boolean ;
                             const _default     : String  ;
                             const _predefined  : String
                           ) ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Procedure that connects external busy events with pipeline's.
        /// </summary>
        /// <param name="_sender">
        ///   event originator
        /// </param>
        /// <param name="_args">
        ///   An instance of TGIS_BusyEventArgs
        ///   that provides data for the busy event.
        /// </param>
        procedure DoBusyEvent(     _sender : TObject ;
                                   _args   : TGIS_BusyEventArgs
                             ) ;
      {$ELSE}
        /// <summary>
        ///   Procedure that connects external busy events with pipeline's.
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
        /// <param name="_abort">
        ///   if set to True inside message handler then an abort request
        /// </param>
        procedure DoBusyEvent(       _sender      : TObject ;
                                     _pos         : Integer ;
                                     _end         : Integer ;
                              var    _abort       : Boolean
                             ) ;
      {$ENDIF}

    protected
      procedure doDestroy  ; override ;

    public
      /// <summary>
      ///   Standard constructor
      /// </summary>
      /// <param name="_pipeline">
      ///   pipeline engine on which operation will be performed
      /// </param>
      /// <param name="_name">
      ///   unique name of the operation
      /// </param>
      /// <param name="_line">
      ///   source code line number
      /// </param>
      constructor Create     ( const _pipeline    : TGIS_Pipeline ;
                               const _name        : String;
                               const _line        : Integer
                             ) ;

    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Perform operation initialization.
      /// </summary>
      procedure Initialize   ; virtual ; abstract ;

      /// <summary>
      ///   Execute operation.
      /// </summary>
      procedure Execute      ; virtual ; abstract ;

      /// <summary>
      ///   Parse command
      /// </summary>
      /// <param name="_command">
      ///   command line including operation name and parameters
      /// </param>
      procedure Parse        ( const _command : String
                             ) ;

      /// <summary>
      ///   Lunch UI form with all parameters to allow interactive operation
      ///   definition.
      /// </summary>
      procedure ShowForm     ; virtual ;

      /// <summary>
      ///   Show operation as a command line text (with all parameters).
      /// </summary>
      /// <returns>
      ///   Complete command line with parameters.
      /// </returns>
      function AsText        : String ; virtual ;

    public
      /// <summary>
      ///   Attached pipeline engine
      /// </summary>
      property Parent  : TGIS_Pipeline
                         read FPipeline ;

      /// <summary>
      ///   Name of operation.
      /// </summary>
      property Name    : String
                         read FName ;

      /// <summary>
      ///  Executing line in TGIS_Pipeline
      /// </summary>
      property Line     : Integer
                          read FLine ;

      /// <summary>
      ///   List of all parameters.
      /// </summary>
      property Params : TGIS_PipelineParameterList
                        read lstParams ;

      /// <summary>
      ///  True if execution was successful.
      /// </summary>
      property &Result  : Boolean
                          read FResult
                          write FResult ;

    private

      /// <summary>
      ///   Try to interpret variable.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function tryParam            ( const _name    : String ;
                                     var  _value   : String
                                   ) : Boolean ;

    protected

      /// <summary>
      ///   Try to interpret variable as a boolean value.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsBool      ( const _name    : String ;
                                     var   _value   : Boolean
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as a integer value.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsInt       ( const _name    : String ;
                                     var   _value   : Integer
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as a double value.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsFloat     ( const _name    : String ;
                                     var   _value   : Double
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as a string.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsString    ( const _name    : String ;
                                     var   _value   : String
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as a color.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsColor     ( const _name    : String ;
                                     var   _value   : TGIS_Color
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as a vector layer's field.
      /// </summary>
      /// <param name="_lv">
      ///   vector layer which contains required field
      /// </param>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsField     ( const _lv      : TGIS_LayerVector ;
                                     const _name    : String ;
                                     var   _value   : String
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as an instance of TGIS_CSCoordinateSystem.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsCS        ( const _name    : String ;
                                     var   _value   : TGIS_CSCoordinateSystem
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as an instance of TGIS_Extent.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsExtent    ( const _name    : String ;
                                     var   _value   : TGIS_Extent
                                   ) : Boolean ;

      /// <summary>
      ///   Try to interpret variable as an instance of TGIS_Layer.
      /// </summary>
      /// <param name="_name">
      ///   name of the variable
      /// </param>
      /// <param name="_value">
      ///   interpreted value
      /// </param>
      /// <returns>
      ///   True if variable can be interpreted
      /// </returns>
      function TryParamAsLayer     ( const _name    : String ;
                                     var   _value   : TGIS_Layer
                                   ) : Boolean ;

      /// <summary>
      ///   Get parameter as a boolean value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists then error is reported.
      /// </remarks>
      function  ParamAsBool        ( const _name    : String
                                   ) : Boolean ; overload ;

      /// <summary>
      ///   Get parameter as a boolean value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  ParamAsBool        ( const _name    : String ;
                                     const _default : Boolean
                                   ) : Boolean ; overload ;

      /// <summary>
      ///   Get parameter as an integer value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists then error is reported.
      /// </remarks>
      function  ParamAsInt         ( const _name    : String
                                   ) : Integer ; overload ;

      /// <summary>
      ///   Get parameter as an integer value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  ParamAsInt         ( const _name    : String ;
                                     const _default : Integer
                                   ) : Integer ; overload ;

      /// <summary>
      ///   Get parameter as a double value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists then error is reported.
      /// </remarks>
      function  ParamAsFloat       ( const _name    : String
                                   ) : Double ; overload ;

      /// <summary>
      ///   Get parameter as a double value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  ParamAsFloat       ( const _name    : String ;
                                     const _default : Double
                                   ) : Double ; overload ;

      /// <summary>
      ///   Get parameter as a string value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  ParamAsString      ( const _name    : String
                                   ) : String ; overload ;

      /// <summary>
      ///   Get parameter as a string value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      ///  <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  ParamAsString      ( const _name    : String ;
                                     const _default : String
                                   ) : String ; overload ;

      /// <summary>
      ///   Get parameter as a color value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   TGIS_Color
      /// </returns>
      function  ParamAsColor       ( const _name    : String
                                   ) : TGIS_Color ; overload ;

      /// <summary>
      ///   Get parameter as a color value.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      ///  <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   TGIS_Color
      /// </returns>
      function  ParamAsColor       ( const _name    : String ;
                                     const _default : TGIS_Color
                                   ) : TGIS_Color ; overload ;

      /// <summary>
      ///   Get parameter as a string value.
      /// </summary>
      /// <param name="_lv">
      ///   vector layer which contains required field
      /// </param>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
     function ParamAsField         ( const _lv      : TGIS_LayerVector ;
                                     const _name    : String
                                   ) : String ; overload ;

      /// <summary>
      ///   Get parameter as a string value.
      /// </summary>
      /// <param name="_lv">
      ///   vector layer which contains required field
      /// </param>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      ///  <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function ParamAsField        ( const _lv      : TGIS_LayerVector ;
                                     const _name    : String ;
                                     const _default : String
                                   ) : String ; overload ;

      /// <summary>
      ///   Get parameter as an instance of TGIS_CSCoordinateSystem.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   instance of TGIS_CSCoordinateSystem
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists then error is reported.
      /// </remarks>
      function  ParamAsCS          ( const _name    : String
                                   ) : TGIS_CSCoordinateSystem ; overload ;

      /// <summary>
      ///   Get parameter as an instance of TGIS_CSCoordinateSystem.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   instance of TGIS_CSCoordinateSystem
      /// </returns>
      function  ParamAsCS          ( const _name    : String ;
                                     const _default : TGIS_CSCoordinateSystem
                                   ) : TGIS_CSCoordinateSystem ; overload ;

      /// <summary>
      ///   Get parameter as an instance of TGIS_Extent.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   instance of TGIS_Extent
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists then error is reported.
      /// </remarks>
      function  ParamAsExtent      ( const _name    : String
                                   ) : TGIS_Extent ; overload ;

      /// <summary>
      ///   Get parameter as an instance of TGIS_Extent.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <param name="_default">
      ///   default value to be used if parameter is empty
      /// </param>
      /// <returns>
      ///   instance of TGIS_Extent
      /// </returns>
      function  ParamAsExtent      ( const _name    : String ;
                                     const _default : TGIS_Extent
                                   ) : TGIS_Extent ; overload ;

      /// <summary>
      ///   Get parameter as instance of TGIS_Layer.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   instance of TGIS_Layer
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists or is not a TGIS_Layer then error is
      ///   reported.
      /// </remarks>
      function  ParamAsLayer       ( const _name    : String
                                   ) : TGIS_Layer ;

      /// <summary>
      ///   Get parameter as instance of TGIS_LayerVector.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   instance of TGIS_LayerVector
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists or is not a TGIS_LayerVector then error
      ///   is reported.
      /// </remarks>
      function  ParamAsLayerVector ( const _name    : String
                                   ) : TGIS_LayerVector ;

      /// <summary>
      ///   Get parameter as instance of TGIS_LayerPixel.
      /// </summary>
      /// <param name="_name">
      ///   name of the parameter
      /// </param>
      /// <returns>
      ///   instance of TGIS_LayerPixel
      /// </returns>
      /// <remarks>
      ///   If parameter does not exists or is not a TGIS_LayerPixel then
      ///   error is reported.
      /// </remarks>
      function  ParamAsLayerPixel  ( const _name    : String
                                   ) : TGIS_LayerPixel ;

      /// <summary>
      ///   Log error message.
      /// </summary>
      /// <param name="_msg">
      ///   message to be logged
      /// </param>
      procedure LogError           ( const _msg     : String
                                   ) ;

      /// <summary>
      ///   Log warning message.
      /// </summary>
      /// <param name="_msg">
      ///   message to be logged
      /// </param>
      procedure LogWarning         ( const _msg     : String
                                   ) ;
  end ;

  TGIS_PipelineOperationAbstractClass = {$IFDEF OXYGENE} public {$ENDIF}
                                      class of TGIS_PipelineOperationAbstract ;

  /// <summary>
  ///   Pipeline base process operation that provides:
  ///   * defining 3 params: "Source", "Destination" and "Save"
  ///   * destination layer saving if needed.
  /// </summary>
  TGIS_PipelineOperationExtendedAbstract = {$IFDEF OXYGENE} public {$ENDIF}
                                         class( TGIS_PipelineOperationAbstract )
    public
      /// <summary>
      ///   Defines 3 params: "Source", "Destination" and "Save".
      /// </summary>
      procedure Initialize ; override ;

      /// <summary>
      ///   Performs destination layer saving if needed.
      /// </summary>
      procedure Execute ; override ;

  end;

  /// <summary>
  ///   Registration list of all pipeline operations.
  /// </summary>
  TGIS_PipelineOperations = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      oBlock : TDictionary< String, TGIS_PipelineOperationAbstractClass > ;
      oNames : TStringList ;

    public
      /// <summary>
      ///  Standard constructor
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy ; override ;

    private
      function fget_Names : TStringList ;

    public
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Add operation to the registrar.
      /// </summary>
      /// <param name="_name">
      ///   unique operation name; name is case insensitive
      /// </param>
      /// <param name="_type">
      ///   calls providing this operation
      /// </param>
      procedure Add     ( const _name : String;
                          const _type : TGIS_PipelineOperationAbstractClass
                        ) ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Find operation in registrar.
      /// </summary>
      /// <param name="_name">
      ///   name of the operation; name is case insensitive
      /// </param>
      /// <returns>
      ///   calls providing operation or nil, if name does not exists in a
      ///   registrar
      /// </returns>
      function  Get     ( const _name : String
                        ) : TGIS_PipelineOperationAbstractClass ;

    published
      /// <summary>
      ///   List of all pipeline operations names.
      /// </summary>
      property Names : TStringList read fget_Names ;
  end ;

  /// <summary>
  ///   Event to be fired on text message, warning, or error events.
  /// </summary>
  /// <param name="_message">
  ///   text of the message
  /// </param>
  TGIS_PipelineMessageEvent = {$IFDEF OXYGENE} public {$ENDIF}
                        procedure( const _message   : String
                                 ) of object ;

  /// <summary>
  ///   Event to be fired on operation basis.
  /// </summary>
  /// <param name="_operation">
  ///   operation object
  /// </param>
  TGIS_PipelineOperationEvent = {$IFDEF OXYGENE} public {$ENDIF}
                        procedure( _operation : TGIS_PipelineOperationAbstract
                                 ) of object ;

  /// <summary>
  ///   Pipeline engine.
  /// </summary>
  TGIS_Pipeline = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      FShowMessageEvent : TGIS_PipelineMessageEvent ;
      FLogWarningEvent  : TGIS_PipelineMessageEvent ;
      FLogErrorEvent    : TGIS_PipelineMessageEvent ;
      FShowFormEvent    : TGIS_PipelineOperationEvent ;
      FOnBusy           : TGIS_BusyEvent ;
      {$IFDEF DCC} [weak] {$ENDIF}
      FViewer : IGIS_Viewer ;

    private
      oVar              : TDictionary< String, TObject > ;
      lstSourceCode     : TGIS_StringList ;
      busyEventManager  : TGIS_BusyEventManager ;

    private
      procedure fset_GIS              ( const _value : IGIS_Viewer ) ;
      function  fget_SourceCode       : String ;
      procedure fset_SourceCode       ( const _value : String ) ;
      function  fget_ShowMessageEvent : TGIS_PipelineMessageEvent ;
      procedure fset_ShowMessageEvent ( const _value : TGIS_PipelineMessageEvent ) ;
      function  fget_LogWarningEvent  : TGIS_PipelineMessageEvent;
      procedure fset_LogWarningEvent  ( const _value : TGIS_PipelineMessageEvent ) ;
      function  fget_LogErrorEvent    : TGIS_PipelineMessageEvent ;
      procedure fset_LogErrorEvent    ( const _value : TGIS_PipelineMessageEvent ) ;
      function  fget_ShowFormEvent    : TGIS_PipelineOperationEvent ;
      procedure fset_ShowFormEvent    ( const _value : TGIS_PipelineOperationEvent ) ;
      {$IFDEF CLR}
        procedure fadd_BusyEvent      ( const _value : TGIS_BusyEvent  ) ;
        procedure fremove_BusyEvent   ( const _value : TGIS_BusyEvent  ) ;
      {$ELSE}
        function  fget_BusyEvent      : TGIS_BusyEvent ;
        procedure fset_BusyEvent      ( const _value : TGIS_BusyEvent ) ;
      {$ENDIF}

   {$IFDEF OXYGENE} assembly or {$ENDIF} protected
      /// <summary>
      ///   raises ShowMessageEvent
      /// </summary>
      /// <param name="_text">
      ///   text of the message
      /// </param>
      procedure doShowMessage( const _text      : String ) ;

      /// <summary>
      ///   raises LogWarningEvent
      /// </summary>
      /// <param name="_msg">
      ///   text of the warning message
      /// </param>
      procedure doLogWarning ( const _msg       : String ) ;

      /// <summary>
      ///   raises LogErrorEvent
      /// </summary>
      /// <param name="_msg">
      ///   text of the error message
      /// </param>
      procedure doLogError   ( const _msg       : String ) ;

      /// <summary>
      ///   raises ShowFormEvent
      /// </summary>
      /// <param name="_operation">
      ///   operation object
      /// </param>
      procedure doShowForm   ( const _operation : TGIS_PipelineOperationAbstract
                             ) ;
    public
      /// <summary>
      ///  Standard constructor
      /// </summary>
      constructor Create ;

    protected
      procedure doDestroy    ; override ;

    public

      /// <summary>
      ///   Set a global variable
      /// </summary>
      /// <param name="_name">
      ///   _name of the variable (case insensitive)
      /// </param>
      /// <param name="_obj">
      ///   value of the variable
      /// </param>
      procedure SetVar       ( const _name      : String;
                               const _obj       : TObject
                             ) ;

      /// <summary>
      ///   Get global variable
      /// </summary>
      /// <param name="_name">
      ///   _name of the variable (case insensitive)
      /// </param>
      /// <returns>
      ///   value of the variable or nil if variable does not exists
      /// </returns>
      function  GetVar       ( const _name      : String
                             ) : TObject ;

      /// <summary>
      ///   Parse text command and create an operation.
      /// </summary>
      /// <param name="_command">
      ///   full command line for the pipeline operation
      /// </param>
      /// <param name="_line">
      ///   line number in a source code; used for reference purposes
      /// </param>
      /// <returns>
      ///   Create pipeline operation object or nil.
      /// </returns>
      function Parse         ( const _command   : String ;
                               const _line      : Integer
                             ) :  TGIS_PipelineOperationAbstract ;

      /// <summary>
      ///   Execute pipeline operations.
      /// </summary>
      /// <returns>
      ///   True if pipeline execution was completed successfully;
      ///   False otherwise.
      /// </returns>
      function  Execute      : Boolean ; overload ;

      /// <summary>
      ///   Execute a single pipeline operation.
      /// </summary>
      /// <param name="_command">
      ///   full command line for the pipeline operation
      /// </param>
      /// <param name="_line">
      ///   line number in a source code; used for reference purposes
      /// </param>
      /// <returns>
      ///   True if pipeline execution was completed successfully;
      ///   False otherwise/
      /// </returns>
      function  Execute      ( const _command   : String;
                               const _line      : Integer
                             ) : Boolean ; overload ;

      /// <summary>
      ///   Lunch UI form with all parameters to allow interactive operation
      ///   definition.
      /// </summary>
      /// <param name="_line">
      ///   line number in a source code for which form must be presented
      /// </param>
      procedure ShowForm     ( const _line    : Integer
                             ) ; overload ;

      /// <summary>
      ///   Lunch UI form with all parameters to allow interactive operation
      ///   definition.
      /// </summary>
      /// <param name="_command">
      ///   full command line for the pipeline operation
      /// </param>
      /// <param name="_line">
      ///   line number in a source code; used for reference purposes
      /// </param>
      procedure ShowForm     ( const _command   : String;
                               const _line      : Integer
                             ) ; overload ;

    public

      /// <summary>
      ///   GIS Viewer on which pipeline operations will be performed.
      /// </summary>
      property GIS              : IGIS_Viewer
                                  read FViewer
                                  write fset_GIS ;

      /// <summary>
      ///   Source code of pipeline operations separated by CR LF.
      /// </summary>
      /// <remarks>
      ///   Line can be commend out with ';' character
      /// </remarks>
      property SourceCode       : String
                                  read fget_SourceCode
                                  write fset_SourceCode ;

      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Dictionary with Pipeline variables; read-only.
      /// </summary>
      property Variables        : TDictionary<String,TObject>
                                  read oVar ;

    published //events
      /// <event/>
      /// <summary>
      ///   Event to be raised upon 'Say' like operations.
      /// </summary>
      property ShowMessageEvent : TGIS_PipelineMessageEvent
                                  read  fget_ShowMessageEvent
                                  write fset_ShowMessageEvent ;

      /// <event/>
      /// <summary>
      ///   Event to be raised upon warning.
      /// </summary>
      property LogWarningEvent  : TGIS_PipelineMessageEvent
                                  read  fget_LogWarningEvent
                                  write fset_LogWarningEvent ;

      /// <event/>
      /// <summary>
      ///   Event to be raised upon error.
      /// </summary>
      property LogErrorEvent    : TGIS_PipelineMessageEvent
                                  read  fget_LogErrorEvent
                                  write fset_LogErrorEvent ;

      /// <event/>
      /// <summary>
      ///   Event to be raised upon ShowForm execution.
      /// </summary>
      property ShowFormEvent    : TGIS_PipelineOperationEvent
                                  read  fget_ShowFormEvent
                                  write fset_ShowFormEvent ;

      {$IFNDEF CLR}
        /// <event/>
        /// <summary>
        ///   (Deprecated) Event to be raised upon execution to report which line is currently
        ///   executed.
        /// </summary>
        property ProgressEvent    : TGIS_BusyEvent
                                    read fget_BusyEvent
                                    write fset_BusyEvent ;
      {$ENDIF}

    published //events
      /// <summary>
      ///   Event to be raised upon execution to report which line is currently
      ///   executed.
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

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Register single pipeline operation
  /// </summary>
  /// <param name="_name">
  ///   unique name of the operation; name is case insensitive
  /// </param>
  /// <param name="_class">
  ///   class implementing operation
  /// </param>
  procedure RegisterPipeline(
    const _name  : String ;
    const _class : TGIS_PipelineOperationAbstractClass
  ) ;

 /// <summary>
 ///   Returns the list of all pipeline's operations defined.
 /// </summary>
 /// <returns>
 ///   Global list of all defined pipeline's operations.
 /// </returns>
 function PipelineOperations
  : TGIS_PipelineOperations ;

//##############################################################################

implementation

{$IFDEF DCC}
  uses
    GisCsFactory,
    GisFunctions,
    GisParams;
{$ENDIF}

var
  oPipelineOperations : TGIS_PipelineOperations ;
  wasSelfRegisterPipeline : Boolean = False ;

function PipelineOperations
  : TGIS_PipelineOperations ;
begin
  if not assigned( oPipelineOperations ) then
    oPipelineOperations := TGIS_PipelineOperations.Create ;
  Result := oPipelineOperations ;
end ;


//type
//? TK
//  TGIS_VariantClass = class
//    private
//      Value : Variant ;
//  end ;

{$REGION 'T_Extent'}
type
  T_Extent = class
    public
      XMin : Double ;
      YMin : Double ;
      XMax : Double ;
      YMax : Double ;
    public
      constructor Create( _ext : TGIS_Extent ) ; overload ;
      constructor Create( _xmin, _ymin, _xmax, _ymax : Double ) ; overload ;
  end ;

constructor T_Extent.Create( _ext : TGIS_Extent ) ;
begin
  self.XMin := _ext.XMin ;
  self.YMin := _ext.YMin ;
  self.XMax := _ext.XMax ;
  self.YMax := _ext.YMax ;
end ;

constructor T_Extent.Create( _xmin, _ymin, _xmax, _ymax : Double ) ;
begin
  self.XMin := _xmin ;
  self.YMin := _ymin ;
  self.XMax := _xmax ;
  self.YMax := _ymax ;
end ;
{$ENDREGION 'T_Extent'}

{$REGION 'T_Literal'}
type
  T_Literal = class
    public
      Value : String ;
    public
      constructor Create( _value : String ) ; overload ;
  end ;

constructor T_Literal.Create( _value : String ) ;
begin
  self.Value := _value ;
end ;
{$ENDREGION 'T_Literal'}

{$REGION 'TGIS_Pipeline'}
function  TGIS_Pipeline.fget_SourceCode
  : String ;
begin
  Result := lstSourceCode.Text ;
end;

procedure TGIS_Pipeline.fset_SourceCode(
  const _value : String
) ;
begin
  lstSourceCode.Text := _value ;
end;

procedure TGIS_Pipeline.fset_GIS(
  const _value : IGIS_Viewer
) ;
begin
  FViewer := _value ;
  oVar.Add( GIS_PIPELINE_VAR_GIS, TObject( GIS ) ) ;
end;

function TGIS_Pipeline.fget_ShowMessageEvent
  : TGIS_PipelineMessageEvent ;
begin
  Result := FShowMessageEvent ;
end;

procedure TGIS_Pipeline.fset_ShowMessageEvent(
  const _value : TGIS_PipelineMessageEvent
) ;
begin
  FShowMessageEvent := _value ;
end;

function TGIS_Pipeline.fget_LogWarningEvent
  : TGIS_PipelineMessageEvent;
begin
  Result := FLogWarningEvent ;
end;

procedure TGIS_Pipeline.fset_LogWarningEvent(
  const _value : TGIS_PipelineMessageEvent
) ;
begin
  FLogWarningEvent := _value ;
end;

function TGIS_Pipeline.fget_LogErrorEvent
  : TGIS_PipelineMessageEvent ;
begin
  Result := FLogErrorEvent ;
end;

procedure TGIS_Pipeline.fset_LogErrorEvent(
  const _value : TGIS_PipelineMessageEvent
) ;
begin
  FLogErrorEvent := _value ;
end;

function TGIS_Pipeline.fget_ShowFormEvent
  : TGIS_PipelineOperationEvent ;
begin
  Result := FShowFormEvent ;
end;

procedure TGIS_Pipeline.fset_ShowFormEvent(
  const _value : TGIS_PipelineOperationEvent
) ;
begin
  FShowFormEvent := _value ;
end;

{$IFDEF CLR}
  procedure TGIS_Pipeline.fadd_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent += _value ;
  end ;

  procedure TGIS_Pipeline.fremove_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent -= _value ;
  end ;
{$ELSE}
  procedure TGIS_Pipeline.fset_BusyEvent(
    const _value : TGIS_BusyEvent
  ) ;
  begin
    busyEventManager.BusyEvent := _value ;
  end ;

  function TGIS_Pipeline.fget_BusyEvent : TGIS_BusyEvent ;
  begin
    Result := busyEventManager.BusyEvent ;
  end ;
{$ENDIF}

procedure TGIS_Pipeline.doShowMessage(
  const _text : String
) ;
begin
  if assigned( FShowMessageEvent ) then
    FShowMessageEvent( _text ) ;
end ;

procedure TGIS_Pipeline.doLogWarning(
  const _msg  : String
) ;
begin
  if assigned( FLogWarningEvent ) then
    FLogWarningEvent( _msg ) ;
end ;

procedure TGIS_Pipeline.doLogError(
  const _msg  : String
) ;
begin
  if assigned( FLogErrorEvent ) then
    FLogErrorEvent( _msg ) ;
  Abort ;
end ;

procedure TGIS_Pipeline.doShowForm(
  const _operation : TGIS_PipelineOperationAbstract
) ;
begin
  if assigned( FShowFormEvent ) then
    FShowFormEvent( _operation ) ;
end ;

{$IFDEF OXYGENE}
  procedure SelfRegisterPipeline ;
  begin
    if wasSelfRegisterPipeline then exit ;

    {$IFDEF JAVA}
      wasSelfRegisterPipeline := SelfInvokeClassMethod( 'tatukgis.jdk.Unit_', 'SelfRegisterPipeline' )  ;
    {$ENDIF}
    {$IFDEF CLR}
      wasSelfRegisterPipeline := SelfInvokeClassMethod( 'Unit_', 'SelfRegisterPipeline' )  ;
    {$ENDIF}
  end ;
{$ENDIF}

constructor TGIS_Pipeline.Create ;
begin
  oVar := TDictionary< String, TObject >.Create(
            {$IFDEF OXYGENE}
              {$IFDEF JAVA}
                java.lang.String.CASE_INSENSITIVE_ORDER
              {$ENDIF}
              {$IFDEF CLR}
                StringComparer.OrdinalIgnoreCase
              {$ENDIF}
            {$ELSE}
              TIStringComparer.Ordinal
            {$ENDIF}
          ) ;
  lstSourceCode := TGIS_StringList.Create ;

  FShowMessageEvent := nil ;
  FLogWarningEvent  := nil ;
  FLogErrorEvent    := nil ;
  FShowFormEvent    := nil ;
  FOnBusy           := nil ;
  busyEventManager  := TGIS_BusyEventManager.Create( Self ) ;

  {$IFDEF OXYGENE}
    SelfRegisterPipeline ;
  {$ENDIF}
end;

procedure TGIS_Pipeline.doDestroy ;
begin
  FreeObject( oVar ) ;
  FreeObject( lstSourceCode ) ;
  FreeObject( busyEventManager ) ;
end;

procedure TGIS_Pipeline.SetVar( const _name : String; const _obj : TObject ) ;
begin
  oVar.AddOrSetValue( _name, _obj );
end;

function TGIS_Pipeline.GetVar( const _name : String )  : TObject ;
begin
  if not oVar.TryGetValue( _name, Result ) then
    Result := nil ;
end;

function TGIS_Pipeline.Parse(
  const _command : String ;
  const _line    : Integer
) :  TGIS_PipelineOperationAbstract ;
var
  tp  : TGIS_PipelineOperationAbstractClass ;
  obj : TGIS_PipelineOperationAbstract ;
  sid : String ;
begin
  Result := nil ;
  try
    {$IFDEF JAVA OR ISLAND}
      sid := _command.Split( ' ' )[0] ;
    {$ELSE}
      sid := _command.Split( [' '] )[0] ;
    {$ENDIF}

    tp := oPipelineOperations.Get( sid ) ;
    if not assigned( tp ) then begin
      doLogError( Format( 'Line %d: Unknown "%s" operation', [_line, sid] ) ) ;
      exit ;
    end ;

    {$IFDEF DCC}
      obj := tp.Create( self, sid, _line ) ;
    {$ENDIF}
    {$IFDEF CLR}
      obj := TGIS_PipelineOperationAbstract(
        Activator.CreateInstance( tp.ActualType, [self, sid, _line] )
      );
    {$ENDIF}
    {$IFDEF JAVA}
      var prm := new &Class[3] ;
      prm[0] := typeOf(TGIS_Pipeline);
      prm[1] := typeOf(String);
      prm[2] := typeOf(Integer);

      obj := TGIS_PipelineOperationAbstract(
        tp.ActualType.getConstructor(prm).newInstance([self, sid, _line] )
      ) ;
    {$ENDIF}

    obj.Parse( _command ) ;

    Result := obj ;
  except
    on ex : Exception do
      doLogError( Format( 'Line %d: %s %s', [ _line, sid, ex.Message ] ) ) ;
  end ;
end ;

function TGIS_Pipeline.Execute : Boolean ;
var
  i      : Integer ;
  cmd    : String  ;
begin
  Result := False ;

  busyEventManager.StartEvent( 'Executing pipeline...', lstSourceCode.Count ) ;
  try
    for i := 1 to lstSourceCode.Count do begin

      cmd := Trim( lstSourceCode[i-1] ) ;
      if IsStringEmpty( cmd ) then
        continue ;
      if cmd[ StringFirst ] = ';' then
        continue ;

      if busyEventManager.PushEvent(
      {$IFDEF JAVA OR ISLAND}
        cmd.Split( ' ', True )[0] ) then
      {$ELSE}
        cmd.Split( [' '], 1 )[0] ) then
      {$ENDIF}
        exit ;

      if not Execute( cmd, i ) then
        exit ;
    end;
  finally
    busyEventManager.EndEvent ;
  end ;

  Result := True ;
end ;

function TGIS_Pipeline.Execute(
  const _command : String ;
  const _line    : Integer
) : Boolean ;
var
  obj : TGIS_PipelineOperationAbstract ;
begin
  Result := False ;

  obj := Parse( _command, _line ) ;
  if assigned( obj ) then begin
    try
      try
        obj.Result := True ;
        obj.Execute ;
        Result := obj.Result ;
      except
        on ex : Exception do
          doLogError( Format( 'Line %d: %s %s', [ _line, obj.Name, ex.Message ] ) ) ;
      end ;
    finally
      FreeObject( obj ) ;
    end;
  end ;
end ;

procedure TGIS_Pipeline.ShowForm(
  const _line : Integer
) ;
var
  cmd : String ;
begin
  cmd := Trim( lstSourceCode[_line-1] ) ;
  if IsStringEmpty( cmd ) then
    exit ;
  if cmd[ StringFirst ] = ';' then
    exit ;

  ShowForm( cmd, _line ) ;
end;

procedure TGIS_Pipeline.ShowForm(
  const _command : String ;
  const _line : Integer
) ;
var
  obj : TGIS_PipelineOperationAbstract ;
begin
  obj := Parse( _command, _line ) ;
  if assigned( obj ) then begin
    try
      obj.ShowForm ;
    finally
      FreeObject( obj ) ;
    end;
  end ;
end;
{$ENDREGION 'TGIS_Pipeline'}

{$REGION 'TGIS_PipelineOperations'}
constructor TGIS_PipelineOperations.Create ;
begin
  oBlock := TDictionary<String,TGIS_PipelineOperationAbstractClass>.Create(
              {$IFDEF OXYGENE}
                {$IFDEF JAVA}
                  java.lang.String.CASE_INSENSITIVE_ORDER
                {$ENDIF}
                {$IFDEF CLR}
                  StringComparer.OrdinalIgnoreCase
                {$ENDIF}
              {$ELSE}
                TIStringComparer.Ordinal
              {$ENDIF}
            ) ;
  oNames := TStringList.Create ;
end;

procedure TGIS_PipelineOperations.doDestroy ;
begin
  FreeObject( oBlock )  ;
  FreeObject( oNames )  ;
end;

function TGIS_PipelineOperations.fget_Names
  : TStringList ;
{$IFDEF DCC}
  var
    elm : TPair< String, TGIS_PipelineOperationAbstractClass > ;
{$ENDIF}
begin
  if oNames.Count < 1 then begin
    try
      for elm in oBlock do
        oNames.Add( elm.Key ) ;
    finally
      oNames.Sorted := True ;
    end ;
  end ;
  Result := oNames ;
end ;

procedure TGIS_PipelineOperations.Add(
  const _name : String ;
  const _type : TGIS_PipelineOperationAbstractClass
) ;
begin
  oBlock.Add( _name, _type );
  oNames.Clear ;
end;

function TGIS_PipelineOperations.Get(
  const _name : String
) : TGIS_PipelineOperationAbstractClass ;
begin
  Result := nil ;
  oBlock.TryGetValue( _name, Result ) ;
end;

{$ENDREGION 'TGIS_PipelineOperations'}

{$REGION 'TGIS_PipelineParameter'}
constructor TGIS_PipelineParameter.Create(
  const _name        : String  ;
  const _type        : TGIS_PipelineParameterType ;
  const _min         : Double  ;
  const _max         : Double  ;
  const _required    : Boolean ;
  const _default     : String  ;
  const _predefined  : String
) ;
begin
  FName        := _name        ;
  FType        := _type        ;
  FMin         := _min         ;
  FMax         := _max         ;
  FRequired    := _required    ;
  FDefault     := _default     ;
  FPredefined  := _predefined  ;
  FValue       := ''           ;
end ;
{$ENDREGION 'TGIS_PipelineParameter'}

{$REGION 'TGIS_PipelineOperationAbstract'}
procedure TGIS_PipelineOperationAbstract.defineParam(
  const _name        : String  ;
  const _type        : TGIS_PipelineParameterType ;
  const _min         : Double  ;
  const _max         : Double  ;
  const _required    : Boolean ;
  const _default     : String  ;
  const _predefined  : String
) ;
var
  o : TGIS_PipelineParameter ;
begin
  o := TGIS_PipelineParameter.Create(
          _name        ,
          _type        ,
          _min         ,
          _max         ,
          _required    ,
          _default     ,
          _predefined
        ) ;

  dctParams.Add( _name, o ) ;
  lstParams.Add( o ) ;
end;

constructor TGIS_PipelineOperationAbstract.Create(
  const _pipeline : TGIS_Pipeline ;
  const _name     : String ;
  const _line     : Integer
) ;
begin
  FPipeline := _pipeline ;
  FName := _name ;
  FLine := _line ;
  lstParams := TGIS_PipelineParameterList.Create ;
  dctParams := TDictionary<String,TGIS_PipelineParameter>.Create(
                 {$IFDEF OXYGENE}
                   {$IFDEF JAVA}
                     java.lang.String.CASE_INSENSITIVE_ORDER
                   {$ENDIF}
                   {$IFDEF CLR}
                     StringComparer.OrdinalIgnoreCase
                   {$ENDIF}
                 {$ELSE}
                   TIStringComparer.Ordinal
                 {$ENDIF}
               ) ;

  Initialize ;
end;

procedure TGIS_PipelineOperationAbstract.doDestroy ;
begin
  FreeObject( dctParams );
  FreeObject( lstParams);
end;

{$IFDEF OXYGENE}
  procedure TGIS_PipelineOperationAbstract.DoBusyEvent(
        _sender : TObject ;
        _args   : TGIS_BusyEventArgs
  ) ;
  var
    abrt : Boolean ;
  begin
    abrt := _args.Abort ;
    doBusyEventEx( _sender, _args.Pos, _args.EndPos, abrt ) ;
    _args.Abort := abrt ;
  end ;

{$ELSE}
  procedure TGIS_PipelineOperationAbstract.DoBusyEvent(
        _sender : TObject ;
        _pos    : Integer ;
        _end    : Integer ;
    var _abort  : Boolean
  ) ;
  begin
    doBusyEventEx( _sender, _pos, _end, _abort ) ;
  end ;
{$ENDIF}

procedure TGIS_PipelineOperationAbstract.doBusyEventEx(
      _sender : TObject ;
      _pos    : Integer ;
      _end    : Integer ;
  var _abort  : Boolean
) ;
var
  event_manager : TGIS_BusyEventManager ;
  stage_pos     : Integer ;
  stage_end_val : Int64 ;
  stage_name    : String ;
begin
  if _sender is TGIS_BusyEventManager then begin
    event_manager := TGIS_BusyEventManager( _sender ) ;
    stage_pos := event_manager.Position[event_manager.Count-1] ;

    case stage_pos of
      -1 : begin
        FPipeline.busyEventManager.EndEvent ;
      end ;
      0  : begin
        stage_name := event_manager.Name[event_manager.Count-1] ;
        stage_end_val := event_manager.EndValue[event_manager.Count-1] ;

        FPipeline.busyEventManager.StartEvent(
          stage_name, stage_end_val, event_manager.Sender[event_manager.Count-1]
        ) ;
      end
      else
        FPipeline.busyEventManager.PushEvent
    end ;
  end
  else begin
    case _pos of
      -1 : FPipeline.busyEventManager.EndEvent ;
      0  : begin
        FPipeline.busyEventManager.StartEvent( 'Executing...', _end, _sender ) ;
      end
      else
        FPipeline.busyEventManager.PushEvent ;
    end ;
  end ;
end;

procedure TGIS_PipelineOperationAbstract.Parse(
  const _command : String
) ;
var
  tkn  : TGIS_Tokenizer ;
  tkn2 : TGIS_Tokenizer ;
  str  : String ;
  i    : Integer ;
  prm  : TGIS_PipelineParameter ;
begin
  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.Execute( _command, [' '], True );
    for i := 1 to tkn.Result.Count - 1 do begin
      tkn2 := TGIS_Tokenizer.Create ;
      try
        tkn2.Execute( tkn.Result[i], ['='], True );
        str := tkn2.Result[0] ; //? report error

        if dctParams.TryGetValue( str, prm ) then begin
          prm.Value := tkn2.Result[1] ;
        end
        else begin
          if ( tkn2.Result.Count = 1 ) and ( lstParams.Count = 1 ) then
            lstParams[0].Value := tkn2.Result[0]
        end;

      finally
        FreeObject( tkn2 ) ;
      end;
    end;
  finally
    FreeObject( tkn ) ;
  end;
end;

procedure TGIS_PipelineOperationAbstract.ShowForm ;
begin
  Parent.doShowForm( self );
end;

function TGIS_PipelineOperationAbstract.AsText
  : String ;
var
  bld    : TStringBuilder ;
  o      : TGIS_PipelineParameter ;
  i      : Integer ;
  tmp    : String ;
  bquote : Boolean ;
begin
  bld := TStringBuilder.Create ;
  try
    bld.Append( FName ) ;

    for i := 0 to lstParams.Count -1 do begin

      o := lstParams[i] ;

      if ( o.Value = o.Default ) and ( not o.Required ) then
        continue ;

      bquote := ( Pos( ',', o.Value ) >= StringFirst )
                or
                ( Pos( '"', o.Value ) >= StringFirst )
                or
                ( Pos( ' ', o.Value ) >= StringFirst )
                or
                ( o.Value = '' ) ;

      if bquote then
        {$IFDEF DCC}
          tmp := AnsiQuotedStr( o.Value, '"' )
        {$ELSE}
          tmp := QuotedStr( o.Value, '"' )
        {$ENDIF}
      else
        tmp := o.Value ;

      bld.Append( ' ' ) ;
      bld.Append( o.Name ) ;
      bld.Append( '=' ) ;
      bld.Append( tmp ) ;
    end;

    Result := bld.ToString ;
  finally
    FreeObject( bld ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.tryParam(
  const _name  : String;
  var   _value : String
) : Boolean ;
var
  prm : TGIS_PipelineParameter ;
  obj : TObject ;
begin
  _value := '' ;
  Result := dctParams.TryGetValue( _name, prm ) ;
  if Result then
    _value := prm.Value ;

  if _value <> '' then begin
    obj := Parent.GetVar( _value ) ;
    if obj is T_Literal then begin
      _value := T_Literal( obj ).Value ;
    end ;
  end ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsBool(
  const _name  : String;
  var   _value : Boolean
) : Boolean ;
var
  value : String ;
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then
    _value := StrToBool( value )
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsInt(
  const _name  : String;
  var   _value : Integer
) : Boolean ;
var
  value : String ;
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then
    _value := StrToInt( value )
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsFloat(
  const _name  : String;
  var   _value : Double
) : Boolean ;
var
  value : String ;
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then
    _value := DotStrToFloat( value )
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsString(
  const _name  : String ;
  var   _value : String
) : Boolean ;
begin
  Result := tryParam( _name, _value ) ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsColor(
  const _name  : String ;
  var   _value : TGIS_Color
) : Boolean ;
var
  value : String ;

  function color_as_text_to_color(
    const _color_txt : String
  ) : TGIS_Color ;
  var
    sec : TGIS_ParamsRender ;
  begin
    sec := TGIS_ParamsRender.Create ;
    try
      sec.StartColorAsText := _color_txt ;
      Result := sec.StartColor ;
    finally
      FreeObject( sec ) ;
    end ;
  end;

  function t( const _txt : String ) : Boolean ;
  begin
    Result := CompareText( _txt, value ) = 0 ;
  end ;

begin
  Result := tryParam( _name, value ) ;
  if Result and ( length( value ) <> 0 ) then begin
    if      t( GIS_INI_PARAM_COLOR_AQUA    ) then _value := TGIS_Color.Aqua
    else if t( GIS_INI_PARAM_COLOR_GRAY    ) then _value := TGIS_Color.Gray
    else if t( GIS_INI_PARAM_COLOR_NAVY    ) then _value := TGIS_Color.Navy
    else if t( GIS_INI_PARAM_COLOR_SILVER  ) then _value := TGIS_Color.Silver
    else if t( GIS_INI_PARAM_COLOR_BLACK   ) then _value := TGIS_Color.Black
    else if t( GIS_INI_PARAM_COLOR_GREEN   ) then _value := TGIS_Color.Green
    else if t( GIS_INI_PARAM_COLOR_OLIVE   ) then _value := TGIS_Color.Olive
    else if t( GIS_INI_PARAM_COLOR_TEAL    ) then _value := TGIS_Color.Teal
    else if t( GIS_INI_PARAM_COLOR_BLUE    ) then _value := TGIS_Color.Blue
    else if t( GIS_INI_PARAM_COLOR_LIME    ) then _value := TGIS_Color.Lime
    else if t( GIS_INI_PARAM_COLOR_PURPLE  ) then _value := TGIS_Color.Purple
    else if t( GIS_INI_PARAM_COLOR_WHITE   ) then _value := TGIS_Color.White
    else if t( GIS_INI_PARAM_COLOR_FUCHSIA ) then _value := TGIS_Color.Fuchsia
    else if t( GIS_INI_PARAM_COLOR_MAROON  ) then _value := TGIS_Color.Maroon
    else if t( GIS_INI_PARAM_COLOR_RED     ) then _value := TGIS_Color.Red
    else if t( GIS_INI_PARAM_COLOR_YELLOW  ) then _value := TGIS_Color.Yellow
    else if t( GIS_INI_PARAM_RENDER        ) then _value := TGIS_Color.RenderColor
    else _value := color_as_text_to_color( value ) ;
  end
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsField(
  const _lv    : TGIS_LayerVector ;
  const _name  : String ;
  var   _value : String
) : Boolean ;
var
  value : String ;
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then begin
    if _lv.FindField( value ) >= 0 then
    _value := value ;
  end
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsCS(
  const _name  : String;
  var   _value : TGIS_CSCoordinateSystem
) : Boolean ;
var
  value  : String ;
  obj    : TObject ;
  {$IFDEF DCC}
    igis : IGIS_Viewer ;
  {$ENDIF}
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then begin
    obj := Parent.GetVar( value ) ;

    {$IFDEF DCC}
      if ( obj is TObject ) and obj.GetInterface( IGIS_Viewer, igis ) then
       _value := igis.CS
    {$ELSE}
      if obj is IGIS_Viewer then
        _value := IGIS_Viewer( obj ).CS
    {$ENDIF}
    else if obj is TGIS_Layer then
      _value := TGIS_Layer( obj ).CS
    else if  obj is TGIS_CSCoordinateSystem then
      _value := TGIS_CSCoordinateSystem( obj )
    else
      Result := False ;
  end
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsExtent(
  const _name  : String;
  var   _value : TGIS_Extent
) : Boolean ;
var
  ext    : T_Extent ;
  value  : String   ;
  obj    : TObject  ;
  {$IFDEF DCC}
    igis : IGIS_Viewer ;
  {$ENDIF}
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then begin
    obj := Parent.GetVar( value ) ;

    {$IFDEF DCC}
      if ( obj is TObject ) and obj.GetInterface( IGIS_Viewer, igis ) then
       _value := igis.Extent
    {$ELSE}
      if obj is IGIS_Viewer then
        _value := IGIS_Viewer( obj ).Extent
    {$ENDIF}
    else if obj is TGIS_Layer then
      _value := TGIS_Layer( obj ).Extent
    else if  obj is T_Extent then begin
      ext := T_Extent( obj ) ;
      _value := GisExtent( ext.XMin, ext.YMin, ext.XMax, ext.YMax ) ;
    end
    else
      Result := False ;
  end
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.TryParamAsLayer(
  const _name  : String;
  var   _value : TGIS_Layer
) : Boolean ;
var
  value : String   ;
  obj   : TObject  ;
begin
  Result := tryParam( _name, value ) ;
  if Result and ( value <> '' ) then begin
    obj := Parent.GetVar( value ) ;

    if obj is TGIS_Layer then
      _value := TGIS_Layer( obj )
    else
      Result := False ;
  end
  else
    Result := False ;
end;

function TGIS_PipelineOperationAbstract.ParamAsBool(
  const _name    : String
) : Boolean ;
begin
  if not TryParamAsBool( _name, Result ) then begin
    LogError( Format( 'Paramter "%s" must be defined', [_name] ) ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsBool(
  const _name    : String ;
  const _default : Boolean
) : Boolean ;
begin
  if not TryParamAsBool( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsInt(
  const _name    : String
) : Integer ;
begin
  if not TryParamAsInt( _name, Result ) then begin
    LogError( Format( 'Paramter "%s" must be defined', [_name] ) ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsInt(
  const _name    : String ;
  const _default : Integer
) : Integer ;
begin
  if not TryParamAsInt( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsFloat(
  const _name    : String
) : Double ;
begin
  if not TryParamAsFloat( _name, Result ) then begin
    LogError( Format( 'Paramter "%s" must be defined', [_name] ) ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsFloat(
  const _name    : String ;
  const _default : Double
) : Double ;
begin
  if not TryParamAsFloat( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsString(
  const _name  : String
) : String ;
begin
  if not TryParamAsString( _name, Result ) then begin
    raise Exception.Create(_name) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsString(
  const _name    : String ;
  const _default : String
) : String ;
begin
  if not TryParamAsString( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsColor(
  const _name  : String
) : TGIS_Color ;
begin
  if not TryParamAsColor( _name, Result ) then begin
    raise Exception.Create(_name) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsColor(
  const _name    : String ;
  const _default : TGIS_Color
) : TGIS_Color ;
begin
  if not TryParamAsColor( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsCS(
  const _name    : String
) : TGIS_CSCoordinateSystem ;
begin
  if not TryParamAsCS( _name, Result ) then begin
    LogError( Format( 'Paramter "%s" must be defined', [_name] ) ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsCS(
  const _name    : String ;
  const _default : TGIS_CSCoordinateSystem
) : TGIS_CSCoordinateSystem  ;
begin
  if not TryParamAsCS( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsExtent(
  const _name    : String
) : TGIS_Extent ;
begin
  if not TryParamAsExtent( _name, Result ) then begin
    LogError( Format( 'Paramter "%s" must be defined', [_name] ) ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsExtent(
  const _name    : String ;
  const _default : TGIS_Extent
) : TGIS_Extent  ;
begin
  if not TryParamAsExtent( _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsField(
  const _lv   : TGIS_LayerVector ;
  const _name : String
) : String ;
begin
  if not TryParamAsField( _lv, _name, Result ) then begin
    LogError( Format(
      'Paramter "%s" does not exist or is not a field in layer "%s" ',
      [_name, _lv]
    ) ) ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsField(
  const _lv      : TGIS_LayerVector ;
  const _name    : String ;
  const _default : String
) : String ;
begin
  if not TryParamAsField( _lv, _name, Result ) then begin
    Result := _default ;
  end;
end;

function TGIS_PipelineOperationAbstract.ParamAsLayer(
  const _name : String
) : TGIS_Layer ;
var
  prm : TGIS_PipelineParameter ;
  obj : TObject ;
begin
  Result := nil ;
  if dctParams.TryGetValue( _name, prm ) then begin
    obj := Parent.GetVar( prm.Value ) ;
    if obj is TGIS_Layer then begin
      Result := TGIS_Layer ( obj ) ;
      Exit ;
    end ;
  end ;

  LogError( Format( 'Layer "%s" does not exists or is wrong type', [_name] ) ) ;
end;

function TGIS_PipelineOperationAbstract.ParamAsLayerVector(
  const _name : String
) : TGIS_LayerVector ;
var
  prm : TGIS_PipelineParameter ;
  obj : TObject ;
begin
  Result := nil ;
  if dctParams.TryGetValue( _name, prm ) then begin
    obj := Parent.GetVar( prm.Value ) ;
    if obj is TGIS_LayerVector then begin
      Result := TGIS_LayerVector ( obj ) ;
      Exit ;
    end ;
  end ;

  LogError( Format( 'Layer "%s" does not exists or is wrong type', [_name] ) ) ;
end;

function TGIS_PipelineOperationAbstract.ParamAsLayerPixel(
  const _name : String
) : TGIS_LayerPixel ;
var
  prm : TGIS_PipelineParameter ;
  obj : TObject ;
begin
  Result := nil ;
  if dctParams.TryGetValue( _name, prm ) then begin
    obj := Parent.GetVar( prm.Value ) ;
    if obj is TGIS_LayerPixel then begin
      Result := TGIS_LayerPixel ( obj ) ;
      Exit ;
    end ;
  end ;

  LogError( Format( 'Layer "%s" does not exists or is wrong type', [_name] ) ) ;
end;

procedure TGIS_PipelineOperationAbstract.LogError(
  const _msg : String
) ;
begin
  Parent.doLogError( Format( 'Line %d: %s %s', [ FLine, FName, _msg ] ) );
end;

procedure TGIS_PipelineOperationAbstract.LogWarning(
  const _msg : String
) ;
begin
  Parent.doLogWarning( Format( 'Line %d: %s %s', [ FLine, FName, _msg ] ) );
end;
{$ENDREGION 'TGIS_PipelineOperationAbstract'}

{$REGION 'TGIS_PipelineOperationExtendedAbstract'}
procedure TGIS_PipelineOperationExtendedAbstract.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_SOURCE,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
  ) ;
  defineParam(
    GIS_PIPELINE_PARAM_DESTINATION,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '',
    ''
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
end;

procedure TGIS_PipelineOperationExtendedAbstract.Execute ;
var
  dst : TGIS_Layer ;
begin
  dst := ParamAsLayer( GIS_PIPELINE_PARAM_DESTINATION ) ;
  if ParamAsBool( GIS_PIPELINE_PARAM_SAVE, GIS_PIPELINE_DEFAULT_SAVE ) then
    dst.SaveAll ;
end;
{$ENDREGION 'TGIS_PipelineOperationExtendedAbstract'}

{$REGION 'Pipeline basic operations'}
type
  /// <summary>
  ///   Implementation of the 'Base.Say' command.
  /// </summary>
  T_Pipeline_BaseSay = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Base.SetCS' command.
  /// </summary>
  T_Pipeline_BaseSetCS = class( TGIS_PipelineOperationAbstract )
    const
      PARAM_EPSG = 'EPSG' ;
      PARAM_WKT  = 'WKT' ;
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Base.SetExtent' command.
  /// </summary>
  T_Pipeline_BaseSetExtent = class( TGIS_PipelineOperationAbstract )
    const
      PARAM_XMIN = 'XMin' ;
      PARAM_XMAX = 'XMax' ;
      PARAM_YMIN = 'YMin' ;
      PARAM_YMAX = 'YMax' ;
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Implementation of the 'Base.SetVars' command.
  /// </summary>
  T_Pipeline_BaseSetVars = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

  /// <summary>
  ///   Helper operation to shows 'Base.SetVars' form.
  /// </summary>
  T_Pipeline_BaseSetVarsForm = class( TGIS_PipelineOperationAbstract )
    public
      procedure Initialize ; override ;
      procedure Execute    ; override ;
  end;

class procedure Unit_GisPipeline.SelfRegisterPipeline() ;
begin
  RegisterPipeline( 'Base.Say',       T_Pipeline_BaseSay       ) ;
  RegisterPipeline( 'Base.SetCS',     T_Pipeline_BaseSetCS     ) ;
  RegisterPipeline( 'Base.SetExtent', T_Pipeline_BaseSetExtent ) ;
  RegisterPipeline( 'Base.SetVars',   T_Pipeline_BaseSetVars   ) ;
end ;
{$ENDREGION 'Pipeline operations'}

{$REGION 'T_Pipeline_BaseSay'}
procedure T_Pipeline_BaseSay.Initialize ;
begin
  defineParam(
    'Text',
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    'Say something',
    ''
  ) ;
end;

procedure T_Pipeline_BaseSay.Execute ;
var
  param : String ;
  obj   : TObject;
  sText : String ;
begin
  param := ParamAsString( 'Text' ) ;

  obj := Parent.GetVar( param ) ;
  if not assigned( obj ) then
    sText := param
  else if obj is TGIS_Layer then
    sText := TGIS_Layer( obj ).Name ;

  Parent.doShowMessage( sText ) ;
end;
{$ENDREGION 'T_Pipeline_BaseSay'}

{$REGION 'T_Pipeline_BaseSetCS'}
procedure T_Pipeline_BaseSetCS.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_RESULT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    GIS_PIPELINE_DEFAULT_CS,
    ''
  ) ;
  defineParam(
    PARAM_EPSG,
    TGIS_PipelineParameterType.Int,
    0,
    9999999,
    False,
    '4326',
    ''
  ) ;
  defineParam(
    PARAM_WKT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    False,
    '',
    ''
  ) ;
end;

procedure T_Pipeline_BaseSetCS.Execute ;
var
  epsgFound : Boolean ;
  wktFound  : Boolean ;
  epsg      : Integer ;
  wkt       : String  ;
  param     : String ;
  cs        : TGIS_CSCoordinateSystem ;
begin
  epsgFound := False ;
  wktFound := False ;
  cs := CSUnknownCoordinateSystem ;

  if TryParamAsInt( PARAM_EPSG, epsg ) then begin
    epsgFound := True ;
    cs := TGIS_CSFactory.ByEPSG( epsg ) ;
  end ;

  if TryParamAsString( PARAM_WKT, wkt ) then begin
    if not IsStringEmpty( wkt) then begin

      if epsgFound then begin
        LogWarning( 'Two parameters are given, EPSG will be used.' ) ;
      end
      else begin
        wktFound := True ;
        cs := TGIS_CSFactory.ByWKT( wkt ) ;
      end ;
    end ;
  end ;

  if not ( epsgFound or wktFound ) then begin
    LogWarning( 'No parameters are given. UnknownCoordinateSystem will be applied.' ) ;
    cs := CSUnknownCoordinateSystem ;
  end;

  param := ParamAsString( GIS_PIPELINE_PARAM_RESULT ) ;
  Parent.SetVar( param, cs ) ;
end;
{$ENDREGION 'T_Pipeline_BaseSetCS'}

{$REGION 'T_Pipeline_BaseSetExtent'}
procedure T_Pipeline_BaseSetExtent.Initialize ;
begin
  defineParam(
    GIS_PIPELINE_PARAM_RESULT,
    TGIS_PipelineParameterType.String,
    NaN,
    NaN,
    True,
    '$extent1',
    ''
  );
  defineParam(
    PARAM_XMIN,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    True,
    '0',
    ''
  );
  defineParam(
    PARAM_YMIN,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    True,
    '0',
    ''
  );
  defineParam(
    PARAM_XMAX,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    True,
    '0',
    ''
  );
  defineParam(
    PARAM_YMAX,
    TGIS_PipelineParameterType.Float,
    NaN,
    NaN,
    True,
    '0',
    ''
  );
end;

procedure T_Pipeline_BaseSetExtent.Execute ;
var
  xmin : Double ;
  ymin : Double ;
  xmax : Double ;
  ymax : Double ;
  res  : String ;
  oext : T_Extent ;
begin
  xmin := ParamAsFloat( PARAM_XMIN ) ;
  ymin := ParamAsFloat( PARAM_YMIN) ;
  xmax := ParamAsFloat( PARAM_XMAX ) ;
  ymax := ParamAsFloat( PARAM_YMAX) ;

  if ( xmin >= xmax ) or
     ( ymin >= ymax) then
    LogError('Minimum values can''t exceed maximum values.') ;

  res := ParamAsString( GIS_PIPELINE_PARAM_RESULT ) ;
  oext := T_Extent.Create( xmin, ymin, xmax, ymax ) ;

  Parent.SetVar( res, oext ) ;
end;
{$ENDREGION 'T_Pipeline_BaseSetExtent'}

{$REGION 'T_Pipeline_BaseSetVars'}
procedure T_Pipeline_BaseSetVars.Initialize ;
var
  i : Integer ;
begin
  for i:=1 to 5  do begin
    if i = 1 then
      defineParam(
        'Var' + IntToStr(i),
        TGIS_PipelineParameterType.String,
        NaN,
        NaN,
        True,
        '$var1=SampleValue',
        ''
      )
    else
      defineParam(
        'Var' + IntToStr(i),
        TGIS_PipelineParameterType.String,
        NaN,
        NaN,
        False,
        '',
        ''
      );
  end ;
end ;

procedure T_Pipeline_BaseSetVars.Execute ;
var
  i   : Integer ;
  res : String ;
  tkn : TGIS_Tokenizer ;
  lst : TStringList ;
  op  : T_Pipeline_BaseSetVarsForm ;
begin
  lst := TStringList.Create ;
  op  := T_Pipeline_BaseSetVarsForm.Create( Parent, 'SetVars', 1 ) ;
  try
    for i := 1 to Params.Count do begin
      res := ParamAsString( Params[i-1].Name ) ;
      if res <> '' then begin
        tkn := TGIS_Tokenizer.Create ;
        try
          tkn.Execute( res, ['='] );
          if tkn.Result.Count <> 2 then begin
            //? report error
          end ;
          if not assigned( Parent.GetVar( tkn.Result[0] ) ) then begin
            op.defineParam(
              tkn.Result[1],
              TGIS_PipelineParameterType.String,
              NaN,
              NaN,
              True,
              '',
              ''
            );
            lst.Add( tkn.Result[0] ) ;
          end;
        finally
          FreeObject( tkn ) ;
        end;
      end ;
    end;

    if lst.Count > 0 then begin
      op.ShowForm ;

      if op.Result then begin
        for i := 0 to lst.Count -1  do begin
          Parent.SetVar( lst[i], T_Literal.Create( op.Params[i].Value ) );
        end;
      end;
      self.Result := op.Result ;
    end;

  finally
    FreeObject( op  ) ;
    FreeObject( lst ) ;
  end;
end;

procedure T_Pipeline_BaseSetVarsForm.Initialize ;
begin
  // do nothing
end ;

procedure T_Pipeline_BaseSetVarsForm.Execute ;
begin
  // do nothing
end;
{$ENDREGION 'T_Pipeline_BaseSetVars'}

procedure RegisterPipeline(
  const _name  : String ;
  const _class : TGIS_PipelineOperationAbstractClass
) ;
begin
  PipelineOperations.Add( _name, _class ) ;
end;

//==============================================================================
// initialization / finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisPipeline.SelfRegisterPipeline() ;
  finalization
    FreeObject( oPipelineOperations );
{$ENDIF}

{==================================== END =====================================}
end.
