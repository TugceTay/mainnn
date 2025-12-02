//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Encapsulation of a JSON layer.

  This unit is inspired from the JSON-C - a JSON implementation in C.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileJSON ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileJSON"'}
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
    System.Text,
    System.Collections.Generic,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Variants,

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

const
  {#GENDOC:HIDE}
  GIS_JSON_MAX_DEPTH    = sizeOf( LongInt ) * 8 ;

type
  {$IFDEF DCC}
    {#GENDOC:HIDE}
    TGIS_JSONBitArray   = set of 0..GIS_JSON_MAX_DEPTH - 1 ;
  {$ENDIF}
  {$IFDEF OXYGENE}
    {#GENDOC:HIDE}
    TGIS_JSONBitArray = array [0..31] of Byte ;
  {$ENDIF}

  TGIS_JSONObject       = class ;
  TGIS_JSONList         = class ;
  TGIS_JSONIterator     = class ;

  {#GENDOC:HIDE}
  TGIS_JSONType = {$IFDEF OXYGENE} public {$ENDIF}
  (
     Null,
     Boolean,
     Double,
     Int,
     &String,
     &Object,
     &Array
  ) ;

  TGIS_JSONEnumerator = class ;
  TGIS_JSONPairEnumerator = class ;

  {$IFDEF OXYGENE}
     T_FObj_JSON nested in TGIS_JSONObject = record
        public
          asBool      : Boolean ;
          asDouble    : Double ;
          asInt       : Integer ;
          asObj       : TGIS_JSONList ;
          asArray     : TGIS_JSONArray ;
          asString    : String ;
      end  ;
  {$ENDIF}

  /// <summary>
  ///   JSON pair implementation.
  /// </summary>
  TGIS_JSONPair = {$IFDEF OXYGENE} public {$ENDIF} record
    public
      /// <summary>
      ///   Key value
      /// </summary>
      Key : String ;
      /// <summary>
      ///   Object value
      /// </summary>
      Val : TGIS_JSONObject ;
    public
      /// <summary>
      ///   Get enumerator of elements list.
      /// </summary>
      /// <returns>
      ///   enumerator
      /// </returns>
      function  GetEnumerator : TGIS_JSONPairEnumerator ;
  end ;

  /// <summary>
  ///   JSON array implementation.
  /// </summary>
  TGIS_JSONArray = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable  )
  private
    FElements : TGIS_ObjectList ;
  private
    function fget_Count : Integer ;
    function fget_Value( const _idx : Integer ) : TGIS_JSONObject ;
  protected
    /// <summary>
    ///   Destructor.
    /// </summary>
    procedure doDestroy ; override;
  public
    /// <summary>
    ///   Constructor.
    /// </summary>
    constructor Create ;
    /// <summary>
    ///   Add object to array.
    /// </summary>
    /// <param name="_obj">
    ///   object
    /// </param>
    procedure Add( const _obj : TGIS_JSONObject ) ;
    /// <summary>
    ///   Clear array.
    /// </summary>
    procedure Clear ;
  public
    /// <summary>
    ///   Items count in array.
    /// </summary>
    property Count : Integer read fget_Count;
    /// <summary>
    ///   Items of array.
    /// </summary>
    /// <param name="_idx">
    ///   index in array
    /// </param>
    property Items[const _idx : Integer] : TGIS_JSONObject read fget_Value; default;
  end ;


  /// <summary>
  ///   JSON object implementation.
  /// </summary>
  TGIS_JSONObject = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable  )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FDataType   : TGIS_JSONType ;
      {$IFDEF OXYGENE}
        FObj      : T_FObj_JSON ;
      {$ELSE}
        FObj          : record
          asBool      : Boolean ;
          asDouble    : Double ;
          asInt       : Integer ;
          asObj       : TGIS_JSONList ;
          asArray     : TGIS_JSONArray ;
          asString    : String ;
        end ;
      {$ENDIF}
      FInUse      : Boolean ;
      FWriter     : TStringBuilder ;
    private
      function fget_Obj( const _path : Variant ) : TGIS_JSONObject ;
      function fget_Pair( const _path : Variant ) : TGIS_JSONPair ;
    protected

      /// <summary>
      ///   Private constructor for all objects.
      /// </summary>
      procedure doCreate ; overload;

      /// <summary>
      ///   Private constructor for all objects.
      /// </summary>
      /// <param name="_jt">
      ///   json type
      /// </param>
      procedure doCreate( const _jt : TGIS_JSONType
                         ) ; overload;

    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Constructor for json integer object.
      /// </summary>
      constructor Create ; overload;

      /// <summary>
      ///   Constructor for json object type.
      /// </summary>
      /// <param name="_jt">
      ///   json type
      /// </param>
      constructor Create( const _jt : TGIS_JSONType
                         ) ; overload;

      /// <summary>
      ///   Constructor for json boolean object.
      /// </summary>
      /// <param name="_bool">
      ///   boolean value
      /// </param>
      constructor Create( const _bool : Boolean
                         ) ; overload;

      /// <summary>
      ///   Constructor for json integer object.
      /// </summary>
      /// <param name="_int">
      ///   integer value
      /// </param>
      constructor Create( const _int : Integer
                         ) ; overload;

      /// <summary>
      ///   Constructor for json double object.
      /// </summary>
      /// <param name="_dbl">
      ///   double value
      /// </param>
      constructor Create( const _dbl : Double
                         ) ; overload;

      /// <summary>
      ///   Constructor for json string object.
      /// </summary>
      /// <param name="_str">
      ///   string value
      /// </param>
      constructor Create( const _str : String
                         ) ; overload;

      /// <summary>
      ///   Get json object as boolean value.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  AsBoolean  : Boolean ;

      /// <summary>
      ///   Get json object as integer value.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  AsInteger  : Integer ;

      /// <summary>
      ///   Get json object as double value.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  AsDouble   : Double ;

      /// <summary>
      ///   Get json object as string value.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  AsString   : String ;

      /// <summary>
      ///   Clear object values.
      /// </summary>
      /// <param name="_all">
      ///   if True, clear all values and subvalues
      /// </param>
      procedure Clear   ( const _all : Boolean
                         ) ;

      /// <summary>
      ///   Get enumerator of elements list.
      /// </summary>
      /// <returns>
      ///   enumerator
      /// </returns>
      function  GetEnumerator : TGIS_JSONEnumerator ;

      {$IFDEF JAVA}
        /// <summary>
        ///   Get enumerator of elements list.
        /// </summary>
        /// <returns>
        ///   enumerator
        /// </returns>
        function &Iterator : java.util.Iterator<TGIS_JSONObject> ;
      {$ENDIF}

      /// <summary>
      ///   Parse json data.
      /// </summary>
      /// <param name="_data">
      ///   json data
      /// </param>
      /// <returns>
      ///   root object
      /// </returns>
      class function ParseJSON( const _data : String
                              ) : TGIS_JSONObject ;
      /// <summary>
      ///   Get json value from object.
      /// </summary>
      /// <param name="_path">
      ///   json path
      /// </param>
      /// <returns>
      ///   value
      /// </returns>
      function  GetValue<T>( const _path : String ) : T ;

      /// <summary>
      ///   Get json value from object.
      /// </summary>
      /// <param name="_path">
      ///   json path
      /// </param>
      /// <param name="_value">
      ///   object value
      /// </param>
      /// <returns>
      ///   True if object was found
      /// </returns>
      function  TryGetValue<T>( const _path : String ;
                                  out _value : T
                              ) : Boolean ;

      /// <summary>
      ///   Find object by path.
      /// </summary>
      /// <param name="_path">
      ///   json path
      /// </param>
      /// <returns>
      ///   object if found
      /// </returns>
      function  FindObject( const _path : String
                          ) : TGIS_JSONObject ;

      /// <summary>
      ///   Get object string value.
      /// </summary>
      /// <returns>
      ///   value
      /// </returns>
      function  Value : String ;

      /// <summary>
      ///   Is instance of json array type.
      /// </summary>
      /// <returns>
      ///   True if is object json array
      /// </returns>
      function  IsArray : Boolean ;

      /// <summary>
      ///   Is instance of json object type.
      /// </summary>
      /// <returns>
      ///   True if is object json object
      /// </returns>
      function  IsObject : Boolean ;

      /// <summary>
      ///   Add pair to object.
      /// </summary>
      /// <param name="_key">
      ///   key name
      /// </param>
      /// <param name="_val">
      ///   json object
      /// </param>
      procedure AddPair( const _key : String ;
                         const _val : TGIS_JSONObject
                       ) ;
      /// <summary>
      ///   Clone object.
      /// </summary>
      /// <returns>
      ///   New object.
      /// </returns>
      function Clone : TGIS_JSONObject ;
    public
      /// <summary>
      ///   Object data type.
      /// </summary>
      property DataType   : TGIS_JSONType   read  FDataType ;

      /// <summary>
      ///   JSON object as object type.
      /// </summary>
      property AsObject   : TGIS_JSONList        read  FObj.asObj ;

      /// <summary>
      ///   JSON object as array type.
      /// </summary>
      property AsArray  : TGIS_JSONArray        read  FObj.asArray ;

      /// <summary>
      ///   Is JSON object in use.
      /// </summary>
      property InUse      : Boolean              read  FInUse
                                                 write FInUse ;

      /// <summary>
      ///   JSON objects.
      /// </summary>
      /// <param name="_path">
      ///   Path to evaluate. Can be an index of array or object name (separated with .)
      /// </param>
      property Objects[const _path : Variant] : TGIS_JSONObject read fget_Obj ; default ;

      /// <summary>
      ///   JSON pairs (key and val).
      /// </summary>
      /// <param name="_path">
      ///   Path to evaluate. Can be an index of array or object name (separated with .)
      /// </param>
      property Pairs[const _path : Variant] : TGIS_JSONPair read fget_Pair ;
  end ;


  /// <summary>
  ///   JSON tokener states.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_JSONTokenerState = {$IFDEF OXYGENE} public {$ENDIF}
                                ( tsEatws,
                                  tsStart,
                                  tsFinish,
                                  tsNull,
                                  tsCommentStart,
                                  tsComment,
                                  tsCommentEol,
                                  tsCommentEnd,
                                  tsString,
                                  tsStringEscape,
                                  tsEscapeUnicode,
                                  tsEscapeHexadecimal,
                                  tsBoolean,
                                  tsNumber,
                                  tsArray,
                                  tsArrayAdd,
                                  tsArraySep,
                                  tsObjectFieldStart,
                                  tsObjectField,
                                  tsObjectUnquotedField,
                                  tsObjectFieldEnd,
                                  tsObjectValue,
                                  tsObjectValueAdd,
                                  tsObjectSep
                                ) ;

  /// <summary>
  ///   JSON token.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_JSONToken = record
    curState    : TGIS_JSONTokenerState ;
    lastState   : TGIS_JSONTokenerState ;
    obj         : TGIS_JSONObject ;
    currentObj  : TGIS_JSONObject ;
    fieldName   : String ;
    parentObj   : TGIS_JSONObject ;
  end ;

  /// <summary>
  ///   JSON progress event.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_JSONProgressEvent = procedure(
        _sender : TObject ;
        _pos    : Int64   ;
        _end    : Int64   ;
    var _abort  : Boolean
  ) of object ;

  /// <summary>
  ///   JSON tokenizer.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_JSONTokenizer = class( TGIS_ObjectDisposable )
    private
      writer      : TStringBuilder ;
      lvl         : Integer ;
      isDouble    : Integer ;
      stPos       : Integer ;
      charOffset  : Integer ;
      quoteChar   : Char    ;
      stk         : array [0..40] of TGIS_JSONToken ;
      FOnProgress : TGIS_JSONProgressEvent ;
    protected

      /// <summary>
      ///   Reset level.
      /// </summary>
      /// <param name="_depth">
      ///   level value
      /// </param>
      procedure ResetLevel( const _depth : Integer ) ;

    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Parse JSON text.
      /// </summary>
      /// <param name="_str">
      ///   json string
      /// </param>
      function Parse( const _str : String
                     ) : TGIS_JSONObject;

      /// <summary>
      ///   Reset tokenizer.
      /// </summary>
      procedure Reset ;

   public //events
      /// <event/>
      /// <summary>
      ///   Progress event.
      /// </summary>
      property ProgressEvent : TGIS_JSONProgressEvent
                               read  FOnProgress
                               write FOnProgress ;
  end ;

  /// <summary>
  ///   JSON list item.
  /// </summary>
  TGIS_JSONItem = class( TGIS_Object )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FGt   : TGIS_JSONItem ;
      FLt   : TGIS_JSONItem ;
      FBf   : Integer ;
      FHash : Cardinal ;
      FName : String ;
      FObj  : TGIS_JSONObject ;
    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_name">
      ///   object name
      /// </param>
      /// <param name="_obj">
      ///   object handle
      /// </param>
      constructor Create ( const _name : String ;
                           const _obj  : TGIS_JSONObject
                         ) ;

      /// <summary>
      ///   Calculate hash for a key string.
      /// </summary>
      /// <param name="_key">
      ///   key value
      /// </param>
      /// <returns>
      ///    hash value
      /// </returns>
      class function Hash( const _key : String
                         ) : Cardinal ;
                         {$IFDEF GIS_STATIC} static ; {$ENDIF}
    public

        /// <summary>
        ///   JSON item name.
        /// </summary>
        property Name    : String          read FName ;

        /// <summary>
        ///   JSON item pointer.
        /// </summary>
        property Ptr     : TGIS_JSONObject read FObj ;

        /// <summary>
        ///   JSON item value.
        /// </summary>
        property Value   : TGIS_JSONObject read FObj write FObj ;
  end ;

  /// <summary>
  ///   JSON list.
  /// </summary>
  TGIS_JSONList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FRoot   : TGIS_JSONItem ;
      FCount  : Integer ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

      /// <summary>
      ///   Balanse a list.
      /// </summary>
      /// <param name="_bal">
      ///   list item
      /// </param>
      function  balance        ( var _bal     : TGIS_JSONItem
                               ) : TGIS_JSONItem ;

      /// <summary>
      ///   Delete object.
      /// </summary>
      /// <param name="_obj">
      ///   object handle
      /// </param>
      /// <param name="_all">
      ///   if True, free all objects
      /// </param>
      procedure doDeleteObj    (   var _obj   : TGIS_JSONItem ;
                                 const _all   : Boolean
                               ) ;

      /// <summary>
      ///   Compare node to node.
      /// </summary>
      /// <param name="_node1">
      ///   item
      /// </param>
      /// <param name="_node2">
      ///   item
      /// </param>
      function  compareNodeNode( const _node1 : TGIS_JSONItem ;
                                 const _node2 : TGIS_JSONItem
                               ) : Integer ;

      /// <summary>
      ///   Compare key node.
      /// </summary>
      /// <param name="_key">
      ///   key
      /// </param>
      /// <param name="_itm">
      ///   item
      /// </param>
      function  compareKeyNode ( const _key   : String ;
                                 const _itm   : TGIS_JSONItem
                               ) : Integer ;

      /// <summary>
      ///   Insert item.
      /// </summary>
      /// <param name="_itm">
      ///   item
      /// </param>
      function  insert         (   var _itm   : TGIS_JSONItem
                               ) : TGIS_JSONItem ;
    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create       ;

      /// <summary>
      ///   Clear items from the list.
      /// </summary>
      procedure Clear          ; overload;

      /// <summary>
      ///   Clear items from the list.
      /// </summary>
      /// <param name="_all">
      ///   if True, clear all objects
      /// </param>
      procedure Clear          ( const _all : Boolean
                               ) ; overload;

      /// <summary>
      ///   Add an item.
      /// </summary>
      /// <param name="_key">
      ///   key value
      /// </param>
      /// <param name="_obj">
      ///   object handle
      /// </param>
      /// <returns>
      ///    object
      /// </returns>
      function Add             ( const _key : String ;
                                 const _obj : TGIS_JSONObject
                               ) : TGIS_JSONObject ;
    public

        /// <summary>
        ///   JSON list items count.
        /// </summary>
        property Count : Integer read FCount ;
  end ;

  /// <summary>
  ///   JSON iterator.
  /// </summary>
  {#typehint:iterator:TGIS_JSONItem}
  TGIS_JSONIterator = class
    private
      FList   : TGIS_JSONList ;
      FBranch : TGIS_JSONBitArray ;
      FDepth  : LongInt ;
      FPath   : array[0..GIS_JSON_MAX_DEPTH - 2] of TGIS_JSONItem ;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_list">
      ///   list of objects
      /// </param>
      constructor Create( const _list : TGIS_JSONList
                         ) ;

      /// <summary>
      ///   Search an item based on the key.
      /// </summary>
      /// <param name="_key">
      ///   key value
      /// </param>
      procedure Search( const _key  : String
                       ) ;

      /// <summary>
      ///   Move iterator first.
      /// </summary>
      procedure First ;

      /// <summary>
      ///   Get iterator.
      /// </summary>
      /// <returns>
      ///    item object
      /// </returns>
      function  GetIter : TGIS_JSONItem ;

      /// <summary>
      ///   Move iterator next.
      /// </summary>
      procedure Next ;
    public

      /// <summary>
      ///   JSON iterator current object.
      /// </summary>
      property CurrentObj : TGIS_JSONItem read GetIter ;
  end ;

  /// <summary>
  ///   JSON iter.
  /// </summary>
  TGIS_JSONIter = {$IFDEF OXYGENE} public {$ENDIF} record
    /// <summary>
    ///   Key value.
    /// </summary>
    key  : String            ;
    /// <summary>
    ///   Value.
    /// </summary>
    val  : TGIS_JSONObject   ;
    /// <summary>
    ///   JSON iter.
    /// </summary>
    Iter : TGIS_JSONIterator ;
  end ;

  /// <summary>
  ///   JSON enumerator.
  /// </summary>
  TGIS_JSONEnumerator = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TObject
                            {$IFDEF CLR} , IEnumerator<Object> {$ENDIF}
                            {$IFDEF JAVA}, java.util.Iterator<TGIS_JSONObject> {$ENDIF}
                          )
  private
    currItr : TGIS_JSONIter ;
    currBof : Boolean       ;
    currObj : TGIS_JSONObject ;
    currArr : TGIS_JSONArray ;
    currPos : Integer ;
  private
    {$IFDEF CLR}
      function   fget_current_obj : Object ;
    {$ENDIF}
  public
    /// <summary>
    ///   Constructor for enumerator.
    /// </summary>
    /// <param name="_obj">
    ///   json object
    /// </param>
    constructor Create( const _obj : TGIS_JSONObject ) ;

    {$IFNDEF MANAGED}
      /// <summary>
      ///   Destroy an object.
      /// </summary>
      destructor Destroy       ; override;
    {$ELSE}
      /// <summary>
      ///   Destroy an object.
      /// </summary>
      procedure  Dispose       ;
    {$ENDIF}

    /// <summary>
    ///   Reset enumerator.
    /// </summary>
    procedure  Reset           ;

    /// <summary>
    ///   Move to next record
    /// </summary>
    /// <returns>
    ///   If false then there is no more shapes.
    /// </returns>
    function   MoveNext        : Boolean ;

  {$IFDEF GENXDK}
    protected
  {$ENDIF}

    /// <summary>
    ///   Get current enumerator value.
    /// </summary>
    /// <returns>
    ///   Shape itself or nil.
    /// </returns>
    function   GetCurrent      : TGIS_JSONObject ;
  public
    {$IFDEF JAVA}
      /// <summary>
      ///   Java specific enumerator support method.
      /// </summary>
      /// <returns>
      ///   Java specific enumerator return value.
      /// </returns>
      method hasNext : Boolean ;

      /// <summary>
      ///   Java specific enumerator support method.
      /// </summary>
      /// <returns>
      ///   Java specific enumerator return value.
      /// </returns>
      method next : TGIS_JSONObject ;

      /// <summary>
      ///   Java specific enumerator support method.
      /// </summary>
      /// <returns>
      ///   Java specific enumerator return value.
      /// </returns>
      method &remove ;
    {$ENDIF}
  public
    /// <summary>
    ///   Current enumerator value.
    /// </summary>
    {$IFNDEF CLR}
      property  Current : TGIS_JSONObject  read GetCurrent      ;
    {$ELSE}
      property  Current : Object  read fget_current_obj ;
    {$ENDIF}
  end ;

  /// <summary>
  ///   JSON pair enumerator.
  /// </summary>
  TGIS_JSONPairEnumerator = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TObject
                            {$IFDEF CLR} , IEnumerator<TObject> {$ENDIF}
                            {$IFDEF JAVA}, java.util.Iterator<TGIS_JSONPair> {$ENDIF}
                          )
  private
    currItr : TGIS_JSONIter ;
    currBof : Boolean       ;
    currObj : TGIS_JSONObject ;
    currArr : TGIS_JSONArray ;
    currPos : Integer ;
  private
    {$IFDEF CLR}
      function   fget_current_obj : Object ;
    {$ENDIF}
  public
    /// <summary>
    ///   Constructor for enumerator.
    /// </summary>
    /// <param name="_obj">
    ///   json object
    /// </param>
    constructor Create( const _obj : TGIS_JSONObject ) ;

    {$IFNDEF MANAGED}
      /// <summary>
      ///   Destroy an object.
      /// </summary>
      destructor Destroy       ; override;
    {$ELSE}
      /// <summary>
      ///   Destroy an object.
      /// </summary>
      procedure  Dispose       ;
    {$ENDIF}

    /// <summary>
    ///   Reset enumerator.
    /// </summary>
    procedure  Reset           ;

    /// <summary>
    ///   Move to next record
    /// </summary>
    /// <returns>
    ///   If false then there is no more shapes.
    /// </returns>
    function   MoveNext        : Boolean ;

  {$IFDEF GENXDK}
    protected
  {$ENDIF}

    /// <summary>
    ///   Get current enumerator value.
    /// </summary>
    /// <returns>
    ///   Shape itself or nil.
    /// </returns>
    function   GetCurrent      : TGIS_JSONPair ;
  public
    {$IFDEF JAVA}
      /// <summary>
      ///   Java specific enumerator support method.
      /// </summary>
      /// <returns>
      ///   Java specific enumerator return value.
      /// </returns>
      method hasNext : Boolean ;

      /// <summary>
      ///   Java specific enumerator support method.
      /// </summary>
      /// <returns>
      ///   Java specific enumerator return value.
      /// </returns>
      method next : TGIS_JSONPair ;

      /// <summary>
      ///   Java specific enumerator support method.
      /// </summary>
      /// <returns>
      ///   Java specific enumerator return value.
      /// </returns>
      method &remove ;
    {$ENDIF}
  public
    /// <summary>
    ///   Current enumerator value.
    /// </summary>
    {$IFNDEF CLR}
      property  Current : TGIS_JSONPair  read GetCurrent ;
    {$ELSE}
      property  Current : Object  read fget_current_obj ;
    {$ENDIF}
  end ;


  /// <summary>
  ///   <para>
  ///     Find first JSON object.
  ///   </para>
  /// </summary>
  /// <param name="_obj">
  ///   json object to search
  /// </param>
  /// <param name="_iter">
  ///   returned iterator
  /// </param>
  /// <returns>
  ///    True if object if found
  /// </returns>
  function  JSONObjectFindFirst ( const _obj  : TGIS_JSONObject ;
                                  var   _iter : TGIS_JSONIter
                                ) : Boolean ; overload;

  /// <summary>
  ///   <para>
  ///     Find first JSON object based on a key.
  ///   </para>
  /// </summary>
  /// <param name="_key">
  ///   key to find
  /// </param>
  /// <param name="_obj">
  ///   json object to search
  /// </param>
  /// <param name="_iter">
  ///   returned iterator
  /// </param>
  /// <returns>
  ///    True if object if found
  /// </returns>
  function  JSONObjectFindFirst ( const _key  : String ;
                                  const _obj  : TGIS_JSONObject ;
                                  var   _iter : TGIS_JSONIter
                                ) : Boolean ; overload;

  /// <summary>
  ///   <para>
  ///     Find JSON object based on a key.
  ///   </para>
  /// </summary>
  /// <param name="_key">
  ///   key to find
  /// </param>
  /// <param name="_root">
  ///   json object
  /// </param>
  /// <returns>
  ///    object if found
  /// </returns>
  function  JSONObjectFind      ( const _key  : String ;
                                  const _root : TGIS_JSONObject
                                ) : TGIS_JSONObject ;

  /// <summary>
  ///   <para>
  ///     Find JSON object based on a key.
  ///   </para>
  /// </summary>
  /// <param name="_key">
  ///   key to find
  /// </param>
  /// <param name="_root">
  ///   json object
  /// </param>
  /// <returns>
  ///    object if found
  /// </returns>
  function  JSONObjectFindPair  ( const _key  : String ;
                                  const _root : TGIS_JSONObject
                                ) : TGIS_JSONPair ; overload;

  /// <summary>
  ///   <para>
  ///     Find JSON object value based on a key.
  ///   </para>
  /// </summary>
  /// <param name="_root">
  ///   json object
  /// </param>
  /// <param name="_key">
  ///   key to find
  /// </param>
  /// <returns>
  ///    object if found
  /// </returns>
  function  JSONObjectGetValue ( const _root      : TGIS_JSONObject ;
                                 const _key       : String
                                ) : Variant ;

  /// <summary>
  ///   <para>
  ///     Find first JSON object.
  ///   </para>
  /// </summary>
  /// <param name="_iter">
  ///   iterator
  /// </param>
  /// <returns>
  ///    True if object if found
  /// </returns>
  function  JSONObjectFindNext  ( var   _iter : TGIS_JSONIter
                                ) : Boolean;

  /// <summary>
  ///   <para>
  ///     Close JSON object finding iterator.
  ///   </para>
  /// </summary>
  /// <param name="_iter">
  ///   iterator
  /// </param>
  procedure JSONObjectFindClose ( var   _iter : TGIS_JSONIter
                                ) ;

  /// <summary>
  ///   <para>
  ///     Get JSON object type.
  ///   </para>
  /// </summary>
  /// <param name="_obj">
  ///   json object
  /// </param>
  /// <returns>
  ///    object type
  /// </returns>
  function  JSONObjectGetType   ( const _obj : TGIS_JSONObject
                                ) : TGIS_JSONType;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisGeoJSONTypeNull      = TGIS_JSONType.Null    ;
      gisGeoJSONTypeBoolean   = TGIS_JSONType.Boolean ;
      gisGeoJSONTypeDouble    = TGIS_JSONType.Double  ;
      gisGeoJSONTypeInt       = TGIS_JSONType.Int     ;
      gisGeoJSONTypeString    = TGIS_JSONType.String  ;
      gisGeoJSONTypeObject    = TGIS_JSONType.Object  ;
      gisGeoJSONTypeArray     = TGIS_JSONType.Array   ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Rtti,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}

//=============================================================================
// TGIS_JSONTokenizer
//=============================================================================

  constructor TGIS_JSONTokenizer.Create ;
  begin
    inherited ;

    writer := TStringBuilder.Create ;
    Reset ;
  end ;

  procedure TGIS_JSONTokenizer.doDestroy ;
  begin
    Reset ;
    FreeObject( writer ) ;

    inherited ;
  end ;

  procedure TGIS_JSONTokenizer.Reset ;
  var
    i : Integer ;
  begin
    for i := lvl downto 0 do
      ResetLevel( i ) ;

    lvl := 0 ;
  end ;

  procedure TGIS_JSONTokenizer.ResetLevel(
    const _depth : Integer
  ) ;
  begin
    stk[ _depth ].curState    := TGIS_JSONTokenerState.tsEatws ;
    stk[ _depth ].lastState   := TGIS_JSONTokenerState.tsStart ;
    stk[ _depth ].currentObj  := nil ;
    stk[ _depth ].fieldName   := '' ;
    stk[ _depth ].obj         := nil ;
    stk[ _depth ].parentObj   := nil ;
  end ;

  function TGIS_JSONTokenizer.Parse(
    const _str : String
  ) : TGIS_JSONObject ;
  var
    obj     : TGIS_JSONObject ;
    c       : Char     ;
    str     : Integer  ;
    strLen  : Integer  ;
    abort   : Boolean  ;

    procedure parseChar ;
    const
      {$IFNDEF OXYGENE}
        spaces   = [#32,#8,#9,#10,#12,#13] ;
        alphanum = ['-','_','a'..'z','A'..'Z','0'..'9'] ;
      {$ELSE}
        spaces   : TSysCharSet = [#32,#8,#9,#10,#12,#13] ;
        alphanum : TSysCharSet = ['-','_','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z',
                                  'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T','U','V','W','X','Y','Z',
                                  '0','1','2','3','4','5','6','7','8','9'] ;
      {$ENDIF}
    var
      numi    : Integer ;
      numd    : Double;
      code    : Integer ;
      {$IFDEF JAVA}
        v     : Variant ;
      {$ENDIF}
      {$IFDEF ISLAND}
        v     : Variant ;
      {$ENDIF}
    begin
      case stk[ lvl ].curState of
        TGIS_JSONTokenerState.tsEatws :
          begin
            if CharInSet( c, spaces ) then
            else if ( c = '/' ) then begin
              writer.Length := 0;
              writer.Append( c ) ;
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsCommentStart ;
            end
            else begin
              stk[ lvl ].curState := stk[ lvl ].lastState ;
              parseChar ;
            end
          end ;
        TGIS_JSONTokenerState.tsStart :
           case c of
             '"',
             '''':
                begin
                  stk[ lvl ].curState := TGIS_JSONTokenerState.tsString ;
                  writer.Length := 0;
                  quoteChar := c ;
                end ;
             '-' :
                begin
                  stk[ lvl ].curState := TGIS_JSONTokenerState.tsNumber ;
                  writer.Length := 0;
                  isDouble   := 0 ;
                  parseChar ;
                end ;
             '0'..'9':
                begin
                  stk[ lvl ].curState := TGIS_JSONTokenerState.tsNumber ;
                  writer.Length := 0;
                  isDouble   := 0 ;
                  parseChar ;
                end ;
             '{':
                begin
                  stk[ lvl ].curState   := TGIS_JSONTokenerState.tsEatws ;
                  stk[ lvl ].lastState  := TGIS_JSONTokenerState.tsObjectFieldStart ;
                  stk[ lvl ].currentObj := TGIS_JSONObject.Create(TGIS_JSONType.Object) ;
                end ;
             '[':
                begin
                  stk[ lvl ].curState   := TGIS_JSONTokenerState.tsEatws ;
                  stk[ lvl ].lastState  := TGIS_JSONTokenerState.tsArray ;
                  stk[ lvl ].currentObj := TGIS_JSONObject.Create(TGIS_JSONType.Array) ;
                end ;
             'N',
             'n':
                begin
                  stk[ lvl ].curState := TGIS_JSONTokenerState.tsNull ;
                  writer.Length := 0;
                  stPos := 0 ;
                  parseChar ;
                end ;
             'T',
             't',
             'F',
             'f':
                begin
                  stk[ lvl ].curState := TGIS_JSONTokenerState.tsBoolean ;
                  writer.Length := 0;
                  stPos := 0 ;
                  parseChar ;
                end ;
            else
                exit;
            end ; //case
        TGIS_JSONTokenerState.tsFinish:
          begin
            if ( lvl = 0 ) then exit ;
              obj := stk[ lvl ].currentObj ;
              ResetLevel( lvl ) ;
              dec( lvl ) ;
              parseChar ;
            end ;
        TGIS_JSONTokenerState.tsNull:
          begin
            writer.Append( c ) ;
            inc( stPos ) ;
            if ( writer.Length >= 4 )                      and
               ( 'n' = writer[0] ) and ( 'u' = writer[1] ) and
               ( 'l' = writer[2] ) and ( 'l' = writer[3] ) then
            begin
              if (stPos = 4) then begin
                stk[ lvl ].currentObj := nil ;
                stk[ lvl ].lastState  := TGIS_JSONTokenerState.tsFinish ;
                stk[ lvl ].curState   := TGIS_JSONTokenerState.tsEatws ;
                parseChar ;
              end ;
            end
            else
              exit ;
          end ;
        TGIS_JSONTokenerState.tsCommentStart:
          begin
            if ( c = '*' ) then
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsComment
            else if ( c = '/' ) then
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsCommentEol
            else
              exit ;
            writer.Append( c ) ;
          end ;
        TGIS_JSONTokenerState.tsComment:
          begin
            if ( c = '*' ) then
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsCommentEnd ;
              writer.Append( c ) ;
            end ;
        TGIS_JSONTokenerState.tsCommentEol:
          begin
            if ( c = #10 ) then
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsEatws
            else
              writer.Append( c ) ;
          end ;
        TGIS_JSONTokenerState.tsCommentEnd:
          begin
            writer.Append( c ) ;
            if ( c = '/' ) then
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsEatws
            else
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsComment ;
          end ;
        TGIS_JSONTokenerState.tsString:
          begin
            if (c = quoteChar) then begin
              stk[ lvl ].currentObj := TGIS_JSONObject.Create(writer.ToString) ;
              stk[ lvl ].lastState  := TGIS_JSONTokenerState.tsFinish ;
              stk[ lvl ].curState   := TGIS_JSONTokenerState.tsEatws ;
            end
            else if (c = '\') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsString ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsStringEscape ;
            end
            else
              writer.Append( c ) ;
          end ;
        TGIS_JSONTokenerState.tsStringEscape:
          case c of
            '"',
            '\',
            '/':
              begin
                writer.Append( c ) ;
                stk[ lvl ].curState := stk[ lvl ].lastState ;
              end ;
            'b',
            'n',
            'r',
            't',
            'f':
              begin
                if ( c = 'b' ) then
                  writer.Append( Char(  #8) )
                else if ( c = 'n' ) then
                  writer.Append( Char( #10) )
                else if ( c = 'r' ) then
                  writer.Append( Char( #13) )
                else if ( c = 't' ) then
                  writer.Append( Char(  #9) )
                else if ( c = 'f' ) then
                  writer.Append( Char( #12) ) ;
                stk[ lvl ].curState := stk[ lvl ].lastState ;
              end ;
            else
              exit;
          end ;
        TGIS_JSONTokenerState.tsBoolean:
          begin
            writer.Append( c ) ;
            inc( stPos ) ;
            if ( writer.Length >= 4 )                      and
               ( 't' = writer[0] ) and ( 'r' = writer[1] ) and
               ( 'u' = writer[2] ) and ( 'e' = writer[3] ) then
            begin
              if (stPos = 4) then begin
                stk[ lvl ].currentObj:= TGIS_JSONObject.Create(True) ;
                stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish ;
                stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
                parseChar ;
              end
            end
            else if ( writer.Length >= 5 )                      and
                    ( 'f' = writer[0] ) and ( 'a' = writer[1] ) and
                    ( 'l' = writer[2] ) and ( 's' = writer[3] ) and
                    ( 'e' = writer[4] ) then
            begin
              if (stPos = 5) then begin
                stk[ lvl ].currentObj:= TGIS_JSONObject.Create(False);
                stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish ;
                stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
                parseChar ;
              end
            end
            else
              exit ;
          end ;
        TGIS_JSONTokenerState.tsNumber:
          begin
            if CharInSet( c, ['0','1','2','3','4','5','6','7','8','9','.','+','-','e','E'] ) then  begin
              writer.Append( c ) ;
              if CharInSet( c, ['.','e','E'] ) then
                isDouble := 1 ;
            end
            else begin
              if (isDouble = 0) then begin
                {$IFDEF ISLAND}
                  v := numi ;
                  Val( writer.ToString, v, code );
                  numi := VarToInt32(v) ;
                {$ELSE}
                  {$IFDEF JAVA}
                    v := numi ;
                    Val( writer.ToString, v, code );
                    numi := VarToInt32(v) ;
                  {$ELSE}
                    Val( writer.ToString, numi, code );
                  {$ENDIF}
                {$ENDIF}
                stk[ lvl ].currentObj := TGIS_JSONObject.Create(numi) ;
              end
              else if ( isDouble <> 0 ) then begin
                {$IFDEF ISLAND}
                  v := numd;
                  Val( writer.ToString, v, code );
                  numd := VarToDouble(v) ;
                {$ELSE}
                  {$IFDEF JAVA}
                    v := numd;
                    Val( writer.ToString, v, code );
                    numd := VarToDouble(v) ;
                  {$ELSE}
                    Val( writer.ToString, numd, code ) ;
                  {$ENDIF}
                {$ENDIF}
                stk[ lvl ].currentObj := TGIS_JSONObject.Create(numd) ;
              end
              else
                exit ;

              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
              parseChar ;
            end
          end ;
        TGIS_JSONTokenerState.tsArray:
          begin
            if (c = ']') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            end
            else begin
              if (lvl >= 40) then
                exit ;
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsArrayAdd ;
              inc( lvl ) ;
              ResetLevel( lvl ) ;
              parseChar ;
            end
          end ;
        TGIS_JSONTokenerState.tsArrayAdd:
          begin
            stk[ lvl ].currentObj.AsArray.Add(obj) ;
            stk[ lvl ].lastState := TGIS_JSONTokenerState.tsArraySep ;
            stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            parseChar ;
          end ;
        TGIS_JSONTokenerState.tsArraySep:
          begin
            if (c = ']') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            end
            else if (c = ',') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsArray ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            end
            else
              exit ;
          end ;
        TGIS_JSONTokenerState.tsObjectFieldStart:
          begin
            if (c = '}') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            end
            else if CharInSet( c, ['"', ''''] ) then begin
              quoteChar := c ;
              writer.Length := 0;
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsObjectField ;
            end
            else
              exit ;
          end ;
        TGIS_JSONTokenerState.tsObjectField:
          begin
            if (c = quoteChar) then begin
              stk[ lvl ].fieldName := writer.ToString ;
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectFieldEnd ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            end
            else if (c = '\') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectField ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsStringEscape ;
            end
            else
              writer.Append( c ) ;
          end ;
        TGIS_JSONTokenerState.tsObjectUnquotedField:
          begin
            if CharInSet( c, [':',#0] ) then begin
              stk[ lvl ].fieldName := writer.ToString ;
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectFieldEnd ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
              parseChar ;
            end
            else if (c = '\') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectUnquotedField ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsStringEscape ;
            end
            else
              writer.Append( c ) ;
          end ;
        TGIS_JSONTokenerState.tsObjectFieldEnd:
          begin
            if (c = ':') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectValue ;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            end
            else
              exit;
          end ;
        TGIS_JSONTokenerState.tsObjectValue:
          begin
            if (lvl >= 40) then exit ;
              stk[ lvl ].curState := TGIS_JSONTokenerState.tsObjectValueAdd ;
              inc( lvl ) ;
              ResetLevel( lvl ) ;
              parseChar ;
            end ;
        TGIS_JSONTokenerState.tsObjectValueAdd:
          begin
            stk[ lvl ].currentObj.AsObject.Add(stk[ lvl ].fieldName, obj) ;
            stk[ lvl ].fieldName := '' ;
            stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectSep ;
            stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws ;
            parseChar ;
          end ;
        TGIS_JSONTokenerState.tsObjectSep:
          begin
            if (c = '}') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsFinish;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws;
            end
            else if (c = ',') then begin
              stk[ lvl ].lastState := TGIS_JSONTokenerState.tsObjectFieldStart;
              stk[ lvl ].curState  := TGIS_JSONTokenerState.tsEatws;
            end
            else
              exit;
          end ;
      end ; // case
    end ;

  begin
    obj := nil;

    if length( _str ) = 0 then begin
      Result := obj ;
      exit ;
    end ;

    try
      abort      := False ;
      charOffset := 0;
      str        := StringFirst ;
      strLen     := StringLast( _str )  ;
      try
        repeat
          if ( charOffset = -1 ) then break ;
          c := _str[ str ] ;
          parseChar ;
          inc( str ) ;
          inc( charOffset ) ;

          if str mod 100 = 0 then
            if assigned( FOnProgress ) then
              FOnProgress( Self, str, strLen, abort ) ;

        until ( str > strLen ) or abort ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTED_PARSING ), '', str ) ;
      end ;
    finally
      Result := stk[ lvl ].currentObj ;
    end ;
  end ;

//=============================================================================
// TGIS_JSONArray
//=============================================================================

  constructor TGIS_JSONArray.Create ;
  begin
    inherited ;

    FElements := TGIS_ObjectList.Create(True) ;
  end ;

  procedure TGIS_JSONArray.doDestroy ;
  begin
    FreeObject( FElements ) ;

    inherited ;
  end ;

  function TGIS_JSONArray.fget_Count : Integer ;
  begin
    Result := FElements.Count ;
  end ;

  function TGIS_JSONArray.fget_Value( const _idx : Integer ) : TGIS_JSONObject ;
  begin
    Result := FElements[_idx] as TGIS_JSONObject ;
  end ;

  procedure TGIS_JSONArray.Add( const _obj : TGIS_JSONObject ) ;
  begin
    FElements.Add( _obj ) ;
  end ;

  procedure TGIS_JSONArray.Clear ;
  begin
    FElements.Clear ;
  end ;

//=============================================================================
// TGIS_JSONObject
//=============================================================================

  procedure TGIS_JSONObject.doCreate ;
  begin
    doCreate( TGIS_JSONType.Object ) ;
  end ;

  procedure TGIS_JSONObject.doCreate(
    const _jt : TGIS_JSONType
  ) ;
  begin
    FDataType := _jt ;
    FInUse    := False ;

    case FDataType of
      TGIS_JSONType.Object  : FObj.asObj    := TGIS_JSONList.Create ;
      TGIS_JSONType.Array   : FObj.asArray  := TGIS_JSONArray.Create ;
      else                    FObj.asObj    := nil ;
    end ;

  end ;

  constructor TGIS_JSONObject.Create ;
  begin
   Create( TGIS_JSONType.Object ) ;
  end ;

  constructor TGIS_JSONObject.Create(
    const _int : Integer
  ) ;
  begin
    inherited Create ;

    doCreate( TGIS_JSONType.Int ) ;
    FObj.asInt := _int ;
  end ;

  constructor TGIS_JSONObject.Create(
    const _bool : Boolean
  ) ;
  begin
    inherited Create ;

    doCreate( TGIS_JSONType.Boolean ) ;
    FObj.asBool := _bool;
  end ;

  constructor TGIS_JSONObject.Create(
    const _jt : TGIS_JSONType
  ) ;
  begin
    inherited Create ;

    doCreate( _jt ) ;
  end ;

  constructor TGIS_JSONObject.Create(
    const _dbl : Double
  ) ;
  begin
    inherited Create ;

    doCreate( TGIS_JSONType.Double ) ;
    FObj.asDouble := _dbl ;
  end ;

  constructor TGIS_JSONObject.Create(
    const _str : String
  ) ;
  begin
    inherited Create ;

    doCreate( TGIS_JSONType.String ) ;
    FObj.asString := _str ;
  end ;

  procedure TGIS_JSONObject.doDestroy ;
  begin
    case FDataType of
      TGIS_JSONType.Object  : FreeObject( FObj.asObj )   ;
      TGIS_JSONType.String  : FObj.asString := ''        ;
      TGIS_JSONType.Array   : FreeObject( FObj.asArray ) ;
    end ;

    if assigned( FWriter ) then
      FreeObject( FWriter ) ;

    inherited ;
  end ;

  function TGIS_JSONObject.fget_Obj(
    const _path : Variant
  ) : TGIS_JSONObject ;
  var
    tkn : TGIS_Tokenizer ;
    obj : TGIS_JSONObject ;
    i   : Integer ;
  begin
    obj := Self ;

    if IsVariantString( _path ) then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( VarToString(_path), '.' ) ;
        for i := 0 to tkn.Result.Count-1 do
          obj := JSONObjectFind( tkn.Result[i], obj ) ;
      finally
        FreeObject( tkn ) ;
      end ;
    end
    else begin
      i := VarToInt32( _path ) ;
      if Self.FDataType = TGIS_JSONType.&Array then begin
        if (i >= 0) and (i < Self.AsArray.Count ) then
          obj := Self.AsArray[i] as TGIS_JSONObject;
      end;
    end ;
    Result := obj ;
  end ;

  function TGIS_JSONObject.fget_Pair(
    const _path : Variant
  ) : TGIS_JSONPair ;
  var
    tkn : TGIS_Tokenizer ;
    obj : TGIS_JSONPair ;
    i   : Integer ;
  begin
    obj.Key := VarToString(_path) ;
    obj.Val := Self ;

    if IsVariantString( _path ) then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( VarToString(_path), '.' ) ;
        for i := 0 to tkn.Result.Count-1 do
          obj := JSONObjectFindPair( tkn.Result[i], obj.Val ) ;
      finally
        FreeObject( tkn ) ;
      end ;
    end
    else begin
      i := VarToInt32( _path ) ;
      if Self.FDataType = TGIS_JSONType.&Array then begin
        if (i >= 0) and (i < Self.AsArray.Count ) then begin
          obj.Val := Self.AsArray[i] as TGIS_JSONObject;
          obj.Key := VarToString(_path) ;
        end ;
      end ;
    end ;
    Result := obj ;
  end ;

  function TGIS_JSONObject.GetEnumerator : TGIS_JSONEnumerator ;
  begin
    Result := TGIS_JSONEnumerator.Create( self ) ;
  end ;

  {$IFDEF JAVA}
    function TGIS_JSONObject.&Iterator : java.util.Iterator<TGIS_JSONObject> ;
    begin
      Result := TGIS_JSONEnumerator.Create( self ) ;
    end ;
  {$ENDIF}

  function TGIS_JSONObject.AsBoolean : Boolean ;
  begin
    case FDataType of
      TGIS_JSONType.Boolean : Result := FObj.asBool ;
      TGIS_JSONType.Int     : Result := ( FObj.asInt <> 0 ) ;
      TGIS_JSONType.Double  : Result := ( FObj.asDouble <> 0 ) ;
      TGIS_JSONType.String  : Result := ( length( FObj.asString ) <> 0 ) ;
      else                    Result := True ;
    end ;
  end ;

  function TGIS_JSONObject.AsDouble : Double ;
  var
    code    : Integer ;
    cdouble : Double ;
    {$IFDEF JAVA}
      v : Variant ;
    {$ENDIF}
    {$IFDEF ISLAND}
      v : Variant ;
    {$ENDIF}
  begin
    case FDataType of
      TGIS_JSONType.Double  : Result := FObj.asDouble ;
      TGIS_JSONType.Int     : Result := FObj.asInt ;
      TGIS_JSONType.Boolean : Result := ord( FObj.asBool ) ;
      TGIS_JSONType.String  : begin
                              {$IFDEF ISLAND}
                                  v := cdouble ;
                                  Val( FObj.asString, v, code );
                                  cdouble := VarToDouble(v) ;
                                {$ELSE}
                                  {$IFDEF JAVA}
                                    v := cdouble ;
                                    Val( FObj.asString, v, code );
                                    cdouble := VarToDouble(v) ;
                                  {$ELSE}
                                    Val( FObj.asString, cdouble, code ) ;
                                  {$ENDIF}
                                {$ENDIF}
                                if code = 0 then
                                  Result := cdouble
                                else
                                  Result := 0.0 ;
                              end ;
      else                    Result := 0.0 ;
    end ;
  end ;

  function TGIS_JSONObject.AsInteger : Integer ;
  var
    code  : Integer ;
    cint  : Integer ;
    {$IFDEF JAVA}
      v : Variant ;
    {$ENDIF}
    {$IFDEF ISLAND}
      v : Variant ;
    {$ENDIF}
  begin
    case FDataType of
      TGIS_JSONType.Int     : Result := FObj.asInt ;
      TGIS_JSONType.Double  : Result := RoundS( FObj.asDouble ) ;
      TGIS_JSONType.Boolean : Result := ord( FObj.asBool ) ;
      TGIS_JSONType.String  : begin
                                {$IFDEF ISLAND}
                                  v := cint ;
                                  Val( FObj.asString, v, code );
                                  cint := VarToInt32(v) ;
                                {$ELSE}
                                  {$IFDEF JAVA}
                                    v := cint ;
                                    Val( FObj.asString, v, code );
                                    cint := VarToInt32(v) ;
                                  {$ELSE}
                                    Val( FObj.asString, cint, code ) ;
                                  {$ENDIF}
                                {$ENDIF}
                                if code = 0 then
                                  Result := cint
                                else
                                  Result := 0 ;
                              end ;
      else                    Result := 0 ;
    end ;
  end ;

  function TGIS_JSONObject.AsString : String ;
  begin
    case FDataType of
      TGIS_JSONType.Int     : Result := IntToStr( FObj.asInt ) ;
      TGIS_JSONType.Double  : Result := DotFloatToStr( FObj.asDouble ) ;
      TGIS_JSONType.Boolean : Result := BoolToStr( FObj.asBool, True ) ;
      TGIS_JSONType.String  : Result := FObj.asString
      else                    Result := '' ;
    end ;
  end ;

  procedure TGIS_JSONObject.Clear(
    const _all : Boolean
  ) ;
  begin
    if FInUse then exit;

    FInUse := True ;
    try
      case FDataType of
        TGIS_JSONType.Boolean : FObj.asBool   := False ;
        TGIS_JSONType.Double  : FObj.asDouble := 0.0 ;
        TGIS_JSONType.Int     : FObj.asInt    := 0 ;
        TGIS_JSONType.String  : FObj.asString := '' ;
        TGIS_JSONType.Object  : FObj.asObj.Clear( _all ) ;
        TGIS_JSONType.Array   : FObj.asArray.Clear ;
      end ;
    finally
      FInUse := False ;
    end ;
  end ;

  class function TGIS_JSONObject.ParseJSON(
    const _data : String
  ) : TGIS_JSONObject ;
  var
    tkn : TGIS_JSONTokenizer ;
  begin
    tkn := TGIS_JSONTokenizer.Create ;
    try
      Result := tkn.Parse( _data ) ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  function TGIS_JSONObject.GetValue<T>( const _path : String ) : T ;
  var
    val : T ;
  begin
    if TryGetValue<T>( _path, val ) then
      Result := val
    else
      Result := default(T) ;
  end ;

  function TGIS_JSONObject.TryGetValue<T>(
    const _path  : String ;
      out _value : T
  ) : Boolean ;
  var
    itr : TGIS_JSONIter ;
    {$IFDEF DCC}
    v   : TValue ;
    {$ENDIF}

  begin
    try
      if JSONObjectFindFirst( _path, self, itr ) then begin
        if assigned( itr.val ) then begin
          case itr.val.FDataType of
            TGIS_JSONType.Null    : begin
                                      {$IFDEF DCC}
                                      _value := TValue.FromVariant( NullVar ).AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF OXYGENE}
                                      _value := T(NullVar);
                                      {$ENDIF}
                                    end ;
            TGIS_JSONType.Boolean : begin
                                      {$IFDEF DCC}
                                      _value := TValue.FromVariant( itr.val.AsBoolean ).AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF OXYGENE}
                                      _value := T(itr.val.AsBoolean);
                                      {$ENDIF}
                                    end ;
            TGIS_JSONType.Double  : begin
                                      {$IFDEF DCC}
                                      _value := TValue.FromVariant( itr.val.AsDouble ).AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF OXYGENE}
                                      _value := T(itr.val.AsDouble);
                                      {$ENDIF}
                                    end ;
            TGIS_JSONType.Int     : begin
                                      {$IFDEF DCC}
                                      _value := TValue.FromVariant( itr.val.AsInteger ).AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF CLR}
                                      _value := T(Convert.ChangeType(itr.val.AsInteger, typeOf(T)));
                                      {$ENDIF}
                                      {$IFDEF JAVA}
                                      CastVal<T>(itr.val.AsInteger, _value);
                                      {$ENDIF}
                                    end ;
            TGIS_JSONType.String  : begin
                                      {$IFDEF DCC}
                                      _value := TValue.FromVariant( itr.val.AsString ).AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF OXYGENE}
                                      _value := T(itr.val.AsString);
                                      {$ENDIF}
                                    end ;
            TGIS_JSONType.Object  : begin
                                      {$IFDEF DCC}
                                      TValue.Make( @itr.val.AsObject, System.TypeInfo(TGIS_JSONList), v ) ;
                                      _value := v.AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF OXYGENE}
                                      _value := T(itr.val.AsObject);
                                      {$ENDIF}
                                    end ;
            TGIS_JSONType.Array   : begin
                                      {$IFDEF DCC}
                                      TValue.Make( @itr.val.AsArray, System.TypeInfo(TGIS_JSONArray), v ) ;
                                      _value := v.AsType<T>;
                                      {$ENDIF}
                                      {$IFDEF OXYGENE}
                                      _value := T(itr.val.AsArray);
                                      {$ENDIF}
                                    end ;
            else  begin
              {$IFDEF DCC}
              _value := TValue.FromVariant( Unassigned).AsType<T>;
              {$ENDIF}
              {$IFDEF CLR}
              _value := T(Unassigned);
              {$ENDIF}
            end ;
          end ;

          Result := True ;
        end
        else
          Result := False ;
      end
      else
        Result := False ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function TGIS_JSONObject.FindObject(
    const _path : String
  ) : TGIS_JSONObject ;
  begin
    Result := JSONObjectFind( _path, Self ) ;
  end ;

  function TGIS_JSONObject.Value : String ;
  begin
    Result := AsString ;
  end;

  function TGIS_JSONObject.IsArray : Boolean ;
  begin
    Result := FDataType = TGIS_JSONType.&Array ;
  end ;

  function TGIS_JSONObject.IsObject : Boolean ;
  begin
    Result := FDataType = TGIS_JSONType.&Object ;
  end ;

  procedure TGIS_JSONObject.AddPair(
    const _key : String ;
    const _val : TGIS_JSONObject
  ) ;
  begin
    FObj.asObj.Add( _key, _val ) ;
  end ;

  function TGIS_JSONObject.Clone : TGIS_JSONObject ;
  var
    i : Integer ;
  begin
    case FDataType of
      TGIS_JSONType.Null    : Result := TGIS_JSONObject.Create( TGIS_JSONType.Null ) ;
      TGIS_JSONType.Boolean : Result := TGIS_JSONObject.Create( FObj.asBool ) ;
      TGIS_JSONType.Double  : Result := TGIS_JSONObject.Create( FObj.asDouble ) ;
      TGIS_JSONType.Int     : Result := TGIS_JSONObject.Create( FObj.asInt ) ;
      TGIS_JSONType.String  : Result := TGIS_JSONObject.Create( FObj.asString ) ;
      TGIS_JSONType.Object  :
        begin
          Result := TGIS_JSONObject.Create( TGIS_JSONType.&Object ) ;
          // TODO
        end ;
      TGIS_JSONType.Array   :
        begin
          Result := TGIS_JSONObject.Create( TGIS_JSONType.&Array ) ;
          for i := 0 to FObj.asArray.Count-1 do
            Result.AsArray.Add( FObj.asArray[i].Clone ) ;
        end
    else
      Result := nil ;
    end ;
  end ;

//=============================================================================
// TGIS_JSONItem
//=============================================================================

  constructor TGIS_JSONItem.Create(
    const _name : String ;
    const _obj   : TGIS_JSONObject
   ) ;
  begin
    inherited Create  ;

    FName := _name ;
    FObj  := _obj ;
    FHash := Hash( FName ) ;
  end ;

  procedure TGIS_JSONItem.doDestroy ;
  begin
    inherited ;
  end ;

  class function TGIS_JSONItem.Hash(
    const _key : String
  ) : Cardinal ;
  var
    h   : Cardinal ;
    i   : Integer  ;
  begin
    h := 0 ;
    if not IsStringEmpty( _key ) then begin
      for i := StringFirst to StringLast( _key ) do
        h := h + ( ( h xor Byte( _key[i] ) ) and $FF ) ;
    end ;
    Result := h ;
  end ;

//=============================================================================
// TGIS_JSONList
//=============================================================================

  constructor TGIS_JSONList.Create ;
  begin
    inherited Create ;

    FRoot   := nil ;
    FCount  := 0 ;
  end ;

  procedure TGIS_JSONList.doDestroy ;
  begin
    Clear ;
    inherited ;
  end ;

  function TGIS_JSONList.balance(
    var _bal : TGIS_JSONItem
  ) : TGIS_JSONItem ;
  var
    deep,
    iold : TGIS_JSONItem ;
    bf   : Integer ;
  begin
    if _bal.FBf > 0 then begin
      deep := _bal.FGt ;

      if deep.FBf < 0 then begin
        iold     := _bal ;
        _bal     := deep.FLt ;
        iold.FGt := _bal.FLt ;
        deep.FLt := _bal.FGt ;
        _bal.FLt := iold ;
        _bal.FGt := deep ;
        bf       := _bal.FBf ;

        if bf <> 0 then begin
          if bf > 0 then begin
            iold.FBf := -1 ;
            deep.FBf := 0 ;
          end
          else begin
            deep.FBf := 1 ;
            iold.FBf := 0 ;
          end ;
          _bal.FBf := 0 ;
        end
        else begin
          iold.FBf := 0 ;
          deep.FBf := 0 ;
        end ;
      end
      else begin
        _bal.FGt := deep.FLt ;
        deep.FLt := _bal ;
        if deep.FBf = 0 then begin
          deep.FBf := -1 ;
          _bal.FBf := 1 ;
        end
        else begin
          deep.FBf := 0 ;
          _bal.FBf := 0 ;
        end ;
        _bal := deep ;
      end ;
    end
    else begin
      deep := _bal.FLt ;
      if deep.FBf > 0 then begin
        iold     := _bal ;
        _bal     := deep.FGt ;
        iold.FLt := _bal.FGt ;
        deep.FGt := _bal.FLt ;
        _bal.FGt := iold ;
        _bal.FLt := deep ;
        bf       := _bal.FBf ;

        if bf <> 0 then begin
          if bf < 0 then begin
            iold.FBf := 1 ;
            deep.FBf := 0 ;
          end
          else begin
            deep.FBf := -1 ;
            iold.FBf := 0 ;
          end ;
          _bal.FBf := 0 ;
        end
        else begin
          iold.FBf := 0 ;
          deep.FBf := 0 ;
        end ;
      end
      else begin
        _bal.FLt := deep.FGt ;
        deep.FGt := _bal ;

        if deep.FBf = 0 then begin
          deep.FBf := 1 ;
          _bal.FBf := -1 ;
        end
        else begin
          deep.FBf := 0 ;
          _bal.FBf := 0 ;
        end ;

        _bal := deep ;
      end ;
    end ;

    Result := _bal ;
  end ;

  function TGIS_JSONList.insert(
    var _itm : TGIS_JSONItem
  ) : TGIS_JSONItem ;
  var
    u, pu,
    hh, pObj     : TGIS_JSONItem ;
    lvl, uDepth  : LongInt ;
    cmp          : Integer ;
    uBf          : Integer ;
    branch       : TGIS_JSONBitArray ;
    p            : TGIS_JSONObject ;
    {$IFDEF OXYGENE}
      i         : Integer ;
    {$ENDIF}
  begin
    inc( FCount ) ;

    _itm.FLt := nil ;
    _itm.FGt := nil ;
    _itm.FBf := 0 ;
    {$IFDEF OXYGENE}
      for i := 0 to 31 do
        branch[i] := 0 ;
    {$ELSE}
      branch := [] ;
    {$ENDIF}

    if (FRoot = nil) then
      FRoot := _itm
    else begin
      u       := nil ;
      pu      := nil ;
      lvl     := 0 ;
      uDepth  := 0 ;
      hh      := FRoot ;
      pObj    := nil ;

      repeat
        if (hh.FBf <> 0) then begin
          u       := hh ;
          pu      := pObj ;
          uDepth  := lvl ;
        end ;

        if hh.FHash <> _itm.FHash then begin
          if hh.FHash < _itm.FHash then
            cmp := -1
          else if hh.FHash > _itm.FHash then
            cmp := 1
          else
            cmp := 0 ;
        end
        else
          cmp := compareNodeNode( _itm, hh ) ;

        if cmp = 0 then begin
          Result  := hh ;
          p       := hh.Ptr ;
          hh.FObj := _itm.Ptr ;
          _itm.FObj := p ;

          doDeleteObj( _itm, false ) ;
          dec( FCount ) ;
          exit ;
        end ;

        pObj := hh ;
        if cmp > 0 then begin
          hh := hh.FGt ;
          {$IFDEF DCC}
            Include( branch, lvl ) ;
          {$ENDIF}
          {$IFDEF OXYGENE}
            branch[lvl] := 1 ;
          {$ENDIF}
        end
        else begin
          hh := hh.FLt ;
          {$IFDEF DCC}
            Exclude( branch, lvl ) ;
          {$ENDIF}
          {$IFDEF OXYGENE}
            branch[lvl] := 0 ;
          {$ENDIF}
        end ;
        inc( lvl ) ;
      until ( hh = nil ) ;

      if cmp < 0 then
        pObj.FLt := _itm
      else
        pObj.FGt := _itm ;

      lvl := uDepth ;

      if (u = nil) then
        hh := FRoot
      else begin
        {$IFDEF OXYGENE}
        if branch[lvl] = 1 then
        {$ELSE}
        if lvl in branch then
        {$ENDIF}
          cmp := 1
        else
          cmp := -1 ;
        inc(lvl) ;
        uBf := u.FBf ;
        if (cmp < 0) then
          dec(uBf)
        else
          inc(uBf) ;
        if cmp < 0 then
          hh := u.FLt
        else
          hh := u.FGt ;
        if ((uBf <> -2) and (uBf <> 2)) then begin
          u.FBf := uBf ;
          u := nil ;
        end ;
      end ;

      if (hh <> nil) then
        while (_itm <> hh) do begin
          {$IFDEF OXYGENE}
          if branch[lvl] = 1 then
          {$ELSE}
          if lvl in branch then
          {$ENDIF}
            cmp := 1
          else
            cmp := -1 ;
          inc(lvl) ;
          if (cmp < 0) then begin
            hh.FBf := -1 ;
            hh := hh.FLt ;
          end
          else begin
            hh.FBf := 1 ;
            hh     := hh.FGt ;
          end ;
        end ;

      if (u <> nil) then begin
        u := balance(u) ;
        if (pu = nil) then
          FRoot := u
        else begin
          lvl := uDepth - 1 ;
          {$IFDEF OXYGENE}
          if branch[lvl] = 1 then
          {$ELSE}
          if lvl in branch then
          {$ENDIF}
            cmp := 1
          else
            cmp := -1 ;
          if (cmp < 0) then
            pu.FLt := u
          else
            pu.FGt := u ;
        end ;
      end ;
    end ;
    Result := _itm ;
  end ;

  procedure TGIS_JSONList.doDeleteObj(
    var _obj : TGIS_JSONItem ;
    const _all : Boolean
  ) ;
  var
    jobj : TGIS_JSONObject ;
  begin
    if _obj.Ptr <> nil then begin
      if _all then
        _obj.Value.Clear( True ) ;
      jobj := _obj.Value ;
      FreeObject( jobj ) ;
    end ;

    FreeObject( _obj ) ;
  end ;

  procedure TGIS_JSONList.Clear;
  begin
    Clear( False ) ;
  end ;

  procedure TGIS_JSONList.Clear(
    const _all : Boolean
  ) ;
  var
    node1, node2 : TGIS_JSONItem ;
  begin
    node1 := FRoot ;

    while node1 <> nil do begin
      if (node1.FLt = nil) then begin
        node2 := node1.FGt ;
        doDeleteObj( node1, _all ) ;
      end
      else begin
        node2     := node1.FLt ;
        node1.FLt := node2.FGt ;
        node2.FGt := node1 ;
      end ;
      node1 := node2 ;
    end ;
    FRoot  := nil ;
    FCount := 0 ;
  end ;

  function TGIS_JSONList.compareKeyNode(
    const _key : String ;
    const _itm : TGIS_JSONItem
  ) : Integer ;
  begin
    Result := CompareStr( _key, _itm.FName ) ;
  end ;

  function TGIS_JSONList.compareNodeNode(
    const _node1 : TGIS_JSONItem ;
    const _node2 : TGIS_JSONItem
  ) : Integer ;
  begin
    Result := CompareStr( _node1.FName, _node2.FName ) ;
  end ;

  function TGIS_JSONList.Add(
    const _key : String ;
    const _obj : TGIS_JSONObject
  ) : TGIS_JSONObject ;
  var
    newObj : TGIS_JSONItem ;
    itm    : TGIS_JSONItem ;
  begin
    itm    := TGIS_JSONItem.Create( _key, _obj ) ;
    newObj := insert( itm ) ;

    if newObj.FObj <> nil then
      Result := newObj.Value
    else
      Result := nil ;
  end ;


//=============================================================================
// TGIS_JSONIterator
//=============================================================================

  constructor TGIS_JSONIterator.Create(
    const _list : TGIS_JSONList
  ) ;
  begin
    inherited Create  ;

    FDepth := -1 ;
    FList  := _list ;
  end ;

  procedure TGIS_JSONIterator.Search(
    const _key  : String
  ) ;
  const
    GIS_JSON_MASK_HIGH_BIT  = not ((not LongWord(0)) shr 1) ;
  var
    h         : TGIS_JSONItem ;
    d         : LongInt ;
    cmp,
    targetCmp : Integer ;
    ha        : Cardinal ;
  begin
    FDepth := -1 ;
    if not assigned( FList ) then exit ;

    ha     := TGIS_JSONItem.Hash(_key) ;
    h      := FList.FRoot ;
    d      := 0 ;

    if (h = nil) then exit ;

    targetCmp := 0 ;

    while true do begin
      if h.FHash < ha then
        cmp := -1
      else if h.FHash > ha then
        cmp := 1
      else
        cmp := 0 ;

      if cmp = 0 then
        cmp := FList.compareKeyNode( _key, h ) ;

      if cmp = 0 then begin
        FDepth := d ;
        break ;
      end
      else if targetCmp <> 0 then
        if ( (cmp xor targetCmp) and GIS_JSON_MASK_HIGH_BIT ) = 0 then
          FDepth := d ;

      if cmp < 0 then
        h := h.FLt
      else
        h := h.FGt ;

      if h = nil then break ;

      if cmp > 0 then
        {$IFDEF DCC}
          Include( FBranch, d )
        {$ENDIF}
        {$IFDEF OXYGENE}
          FBranch[d] := 1
        {$ENDIF}
      else
        {$IFDEF DCC}
          Exclude( FBranch, d ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          FBranch[d] := 0 ;
        {$ENDIF}

      FPath[d] := h ;
      inc( d ) ;
    end ;
  end ;

  procedure TGIS_JSONIterator.First ;
  var
    h : TGIS_JSONItem ;
    {$IFDEF OXYGENE}
      i : Integer ;
    {$ENDIF}
  begin
    FDepth  := -1 ;
    if not assigned( FList ) then exit ;

    h := FList.FRoot ;
    {$IFDEF OXYGENE}
      for i := 0 to 31 do
        FBranch[i] := 0 ;
    {$ELSE}
      FBranch := [] ;
    {$ENDIF}

    while h <> nil do begin
      if ( FDepth <> -1 ) then
        FPath[FDepth] := h ;

      inc( FDepth ) ;
      h := h.FLt ;
    end ;
  end ;

  function TGIS_JSONIterator.GetIter : TGIS_JSONItem ;
  begin
    if ( FDepth = -1 ) then begin
      Result := nil ;
      exit ;
    end ;

    if ( FDepth = 0 ) and assigned( FList ) then
      Result := FList.FRoot
    else
      Result := FPath[FDepth - 1] ;
  end ;

  procedure TGIS_JSONIterator.Next ;
  var
    h : TGIS_JSONItem ;
  begin
    if ( FDepth <> -1 ) and assigned( FList ) then begin
      if FDepth = 0 then
        h := FList.FRoot.FGt
      else
        h := FPath[FDepth - 1].FGt ;

      if ( h = nil ) then begin
        repeat
          if ( FDepth = 0 ) then begin
            FDepth := -1 ;
            break ;
          end ;
          dec( FDepth ) ;
        {$IFDEF OXYGENE}
        until (not (FBranch[FDepth]=1 ))
        {$ELSE}
        until (not (FDepth in FBranch))
        {$ENDIF}
      end
      else begin
        {$IFDEF DCC}
          Include( FBranch, FDepth ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          FBranch[FDepth] := 1 ;
        {$ENDIF}
        FPath[FDepth] := h ;
        inc( FDepth ) ;
        while True do begin
          h := h.FLt ;
          if ( h = nil ) then break ;
          {$IFDEF DCC}
            Exclude( FBranch, FDepth ) ;
          {$ENDIF}
          {$IFDEF OXYGENE}
            FBranch[FDepth] := 0 ;
          {$ENDIF}
          FPath[FDepth] := h ;
          inc( FDepth ) ;
        end ;
      end ;
    end ;
  end ;

//=============================================================================
// Global functions
//=============================================================================

  function JSONObjectFindFirst(
    const _obj  : TGIS_JSONObject;
    var _iter : TGIS_JSONIter
  ) : Boolean;
  var
    i : TGIS_JSONItem ;
  begin
    _iter.Iter := TGIS_JSONIterator.Create( _obj.AsObject ) ;
    _iter.Iter.First ;
    i := _iter.Iter.GetIter ;

    if i <> nil then begin
      _iter.key := i.Name;
      _iter.val := i.Value;
      Result    := true;
    end
    else
      Result := False;
  end ;

  function JSONObjectFindFirst(
    const _key : String ;
    const _obj : TGIS_JSONObject;
      var _iter: TGIS_JSONIter
  ) : Boolean;
  var
    i : TGIS_JSONItem ;
  begin
    if assigned( _obj ) then begin
      _iter.Iter := TGIS_JSONIterator.Create( _obj.AsObject ) ;
      _iter.Iter.Search( _key ) ;
      i := _iter.Iter.CurrentObj;

      if i <> nil then begin
        Result    := _iter.Iter.CurrentObj.Name = _key ;
        _iter.key := _iter.Iter.CurrentObj.FName ;
        _iter.val := _iter.Iter.CurrentObj.Value ;
      end
      else
        Result := False ;
    end
    else begin
      _iter.Iter := nil ;
      Result := False ;
    end;
  end ;

  function JSONObjectFindNext(
    var _iter : TGIS_JSONIter
  ) : Boolean;
  var
    i : TGIS_JSONItem ;
  begin
    _iter.Iter.Next;
    i := _iter.Iter.GetIter;

    if i <> nil then begin
      _iter.key := i.FName ;
      _iter.val := i.Value ;
      Result    := True ;
    end
    else
      Result := False;
  end ;

  procedure JSONObjectFindClose(
    var _iter : TGIS_JSONIter
  ) ;
  var
    jiter : TGIS_JSONIterator ;
  begin
    jiter := _iter.Iter ;
    FreeObject( jiter ) ;
    _iter.val := nil ;
  end ;

  function  JSONObjectFind(
    const _key  : String ;
    const _root : TGIS_JSONObject
  ) : TGIS_JSONObject ;
  var
    itr : TGIS_JSONIter ;
  begin
    try
      if JSONObjectFindFirst( _key, _root, itr ) then
        Result := itr.val
      else
        Result := nil ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function  JSONObjectFindPair(
    const _key  : String ;
    const _root : TGIS_JSONObject
  ) : TGIS_JSONPair ;
  var
    itr : TGIS_JSONIter ;
  begin
    try
      if JSONObjectFindFirst( _key, _root, itr ) then begin
        Result.Val := itr.val ;
        Result.Key := itr.key ;
      end
      else begin
        Result.Val := nil ;
        Result.Key := '' ;
      end ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function  JSONObjectGetValue(
    const _root      : TGIS_JSONObject ;
    const _key       : String
  ) : Variant ;
  var
    itr : TGIS_JSONIter ;
  begin
    try
      if JSONObjectFindFirst( _key, _root, itr ) then begin
        if assigned( itr.val ) then begin
          case itr.val.FDataType of
            TGIS_JSONType.Null    : Result := NullVar ;
            TGIS_JSONType.Boolean : Result := itr.val.AsBoolean ;
            TGIS_JSONType.Double  : Result := itr.val.AsDouble ;
            TGIS_JSONType.Int     : Result := itr.val.AsInteger ;
            TGIS_JSONType.String  : Result := itr.val.AsString ;
            TGIS_JSONType.Object  : Result := Unassigned ;
            TGIS_JSONType.Array   : Result := Unassigned ;
          end
        end
        else
          Result := Unassigned ;
      end
      else
        Result := Unassigned ;
    finally
      JSONObjectFindClose( itr ) ;
    end ;
  end ;

  function JSONObjectGetType(
    const _obj : TGIS_JSONObject
  ) : TGIS_JSONType ;
  begin
    if _obj <> nil then
      Result := _obj.FDataType
    else
      Result := TGIS_JSONType.Null ;
  end ;

//=============================================================================
// TGIS_JSONEnumerator
//=============================================================================

  constructor TGIS_JSONEnumerator.Create(
    const _obj : TGIS_JSONObject
  ) ;
  begin
    inherited Create ;

    currObj := _obj ;
    Reset ;
  end ;

  {$IFNDEF MANAGED}
    destructor TGIS_JSONEnumerator.Destroy ;
    begin
      JSONObjectFindClose( currItr ) ;

      inherited ;
    end ;
  {$ELSE}
    procedure TGIS_JSONEnumerator.Dispose ;
    begin
      JSONObjectFindClose( currItr ) ;

      {$IFNDEF OXYGENE}
        inherited ;
      {$ENDIF}
    end ;
  {$ENDIF}

  procedure TGIS_JSONEnumerator.Reset ;
  var
    i : TGIS_JSONItem ;
  begin
    if currObj.FDataType = TGIS_JSONType.&Object then begin
      currItr.Iter := TGIS_JSONIterator.Create( currObj.AsObject ) ;
      currItr.Iter.First ;
      i := currItr.Iter.GetIter ;

      if i <> nil then begin
        currItr.key := i.Name ;
        currItr.val := i.Value ;
        currBof := True ;
      end
      else
        currBof := False ;
    end
    else if currObj.FDataType = TGIS_JSONType.&Array then begin
      currArr := currObj.AsArray ;
      if currArr.Count > 0 then begin
        currPos := 0 ;
        currBof := True ;
      end
      else begin
        currPos := 0 ;
        currBof := False ;
      end ;
    end ;
  end ;

  function TGIS_JSONEnumerator.MoveNext : Boolean ;
  var
    i : TGIS_JSONItem ;
  begin
    if currBof then begin
      Result := True ;
      currBof := False ;
      exit ;
    end;

    if currObj.FDataType = TGIS_JSONType.&Object then begin
      currItr.Iter.Next ;
      i := currItr.Iter.GetIter ;

      if i <> nil then begin
        currItr.key := i.FName ;
        currItr.val := i.Value ;
        Result      := True ;
      end
      else
        Result := False ;
    end
    else if currObj.FDataType = TGIS_JSONType.&Array then begin
      inc( currPos ) ;
      Result := currPos < currArr.Count ;
    end
    else
      Result := False ;
  end ;

  function TGIS_JSONEnumerator.GetCurrent : TGIS_JSONObject ;
  begin
    if currObj.FDataType = TGIS_JSONType.&Object then
      Result := currItr.val
    else if currObj.FDataType = TGIS_JSONType.&Array then
      Result := TGIS_JSONObject( currArr[currPos] )
    else
      Result := nil ;
  end ;

  {$IFDEF CLR}
    function TGIS_JSONEnumerator.fget_current_obj : Object ;
    begin
      Result := GetCurrent ;
    end ;
  {$ENDIF}

  {$IFDEF JAVA}
    method TGIS_JSONEnumerator.hasNext : Boolean ;
    begin
      Result := MoveNext ;
    end ;

    method TGIS_JSONEnumerator.next : TGIS_JSONObject ;
    begin
      Result := GetCurrent ;
    end ;

    method TGIS_JSONEnumerator.&remove ;
    begin

    end ;
  {$ENDIF}

//=============================================================================
// TGIS_JSONPairEnumerator
//=============================================================================

  function TGIS_JSONPair.GetEnumerator : TGIS_JSONPairEnumerator ;
  begin
    Result := TGIS_JSONPairEnumerator.Create( Val ) ;
  end ;


  constructor TGIS_JSONPairEnumerator.Create(
    const _obj : TGIS_JSONObject
  ) ;
  begin
    inherited Create ;

    currObj := _obj ;
    Reset ;
  end ;

  {$IFNDEF MANAGED}
    destructor TGIS_JSONPairEnumerator.Destroy ;
    begin
      JSONObjectFindClose( currItr ) ;

      inherited ;
    end ;
  {$ELSE}
    procedure TGIS_JSONPairEnumerator.Dispose ;
    begin
      JSONObjectFindClose( currItr ) ;

      {$IFNDEF OXYGENE}
        inherited ;
      {$ENDIF}
    end ;
  {$ENDIF}

  procedure TGIS_JSONPairEnumerator.Reset ;
  var
    i : TGIS_JSONItem ;
  begin
    if currObj.FDataType = TGIS_JSONType.&Object then begin
      currItr.Iter := TGIS_JSONIterator.Create( currObj.AsObject ) ;
      currItr.Iter.First ;
      i := currItr.Iter.GetIter ;

      if i <> nil then begin
        currItr.key := i.Name ;
        currItr.val := i.Value ;
        currBof := True ;
      end
      else
        currBof := False ;
    end
    else if currObj.FDataType = TGIS_JSONType.&Array then begin
      currArr := currObj.AsArray ;
      if currArr.Count > 0 then begin
        currPos := 0 ;
        currBof := True ;
      end
      else begin
        currPos := 0 ;
        currBof := False ;
      end ;
    end ;
  end ;

  function TGIS_JSONPairEnumerator.MoveNext : Boolean ;
  var
    i : TGIS_JSONItem ;
  begin
    if currBof then begin
      Result := True ;
      currBof := False ;
      exit ;
    end;

    if currObj.FDataType = TGIS_JSONType.&Object then begin
      currItr.Iter.Next ;
      i := currItr.Iter.GetIter ;

      if i <> nil then begin
        currItr.key := i.FName ;
        currItr.val := i.Value ;
        Result      := True ;
      end
      else
        Result := False ;
    end
    else if currObj.FDataType = TGIS_JSONType.&Array then begin
      inc( currPos ) ;
      Result := currPos < currArr.Count ;
    end
    else
      Result := False ;
  end ;

  function TGIS_JSONPairEnumerator.GetCurrent : TGIS_JSONPair ;
  begin
    if currObj.FDataType = TGIS_JSONType.&Object then begin
      Result.Val := currItr.val ;
      Result.Key := currItr.key ;
    end
    else if currObj.FDataType = TGIS_JSONType.&Array then begin
      Result.Val := TGIS_JSONObject( currArr[currPos] ) ;
      Result.Key := IntToStr( currPos ) ;
    end
    else begin
      Result.Val := nil ;
      Result.Key := '' ;
    end ;
  end ;

  {$IFDEF CLR}
    function TGIS_JSONPairEnumerator.fget_current_obj : Object ;
    begin
      Result := GetCurrent ;
    end ;
  {$ENDIF}

  {$IFDEF JAVA}
    method TGIS_JSONPairEnumerator.hasNext : Boolean ;
    begin
      Result := MoveNext ;
    end ;

    method TGIS_JSONPairEnumerator.next : TGIS_JSONPair ;
    begin
      Result := GetCurrent ;
    end ;

    method TGIS_JSONPairEnumerator.&remove ;
    begin

    end ;
  {$ENDIF}



//==================================== END =====================================
end.

