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
  SQL Evaluator closely mimics WHERE clause syntax.
  For internal use of TGIS_LayerShp /and any future similar classes/

  Internal representation was built on a stack machine. Stack machine has two
  stacks, one for values and one for operators.
}

{$IFDEF DCC}
  unit GisSqlQuery ;
  {$HPPEMIT '#pragma link "GisSqlQuery"'}
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
    System.SysUtils,
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Variants,

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
  // call-back function

     /// <summary>
     ///   Call-back function, must return a binding value of the field given
     ///   by field.
     /// </summary>
     /// <param name="_field">
     ///   field name
     /// </param>
     /// <param name="_cursor">
     ///   cursor id
     /// </param>
     TGIS_SqlQueryBindFun = {$IFDEF OXYGENE} public {$ENDIF}
                            function( const _field   : String ;
                                      const _cursor  : Integer
                                    ) : Integer of object ;

     /// <summary>
     ///   Call-back function, must return a value of the field given by
     ///   _field. The Parse function is called once for each record.
     /// </summary>
     /// <param name="_shape">
     ///   shape instance
     /// </param>
     /// <param name="_field">
     ///   field name
     /// </param>
     /// <param name="_cursor">
     ///   cursor id
     /// </param>
     TGIS_SqlQueryFieldBindFun = {$IFDEF OXYGENE} public {$ENDIF}
                                 function( const _shape   : TObject ;
                                           const _field   : Integer ;
                                           const _cursor  : Integer
                                         ) : Variant of object ;

     /// <summary>
     ///   Call-back function, must return a value of the field given by _field.
     /// </summary>
     /// <param name="_uid">
     ///   UID value
     /// </param>
     /// <param name="_field">
     ///   field name
     /// </param>
     TGIS_SqlQueryFieldFun = {$IFDEF OXYGENE} public {$ENDIF}
                             function( const _uid   : TGIS_Uid ;
                                       const _field : String
                                     ) : Variant of object ;
     /// <summary>
     ///   Call-back function, must return a value of the field given by _field.
     /// </summary>
     /// <param name="_field">
     ///   field name
     /// </param>
     /// <param name="_function">
     ///   statistic function name
     /// </param>
     TGIS_SqlQueryStatFun = {$IFDEF OXYGENE} public {$ENDIF}
                             function( const _field    : String  ;
                                       const _function : String
                                     ) : Variant of object ;

     /// <summary>
     ///   Calculates a logical value of WHERE-like statements for non
     ///   SQL-based tables.
     /// </summary>
     TGIS_SqlQuery = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
       private
         /// <summary>
         ///   Reference to Parser class.
         /// </summary>
         classPtr    : TObject ;

         /// <summary>
         ///   Query to be parsed.
         /// </summary>
         queryStr    : String ;

         /// <summary>
         ///   Currently bound layer.
         /// </summary>
         boundLayer : TObject ;

       protected

         /// <summary>
         ///   Destroy parser.
         /// </summary>
         procedure   doDestroy     ; override;

       public

         /// <summary>
         ///   Create parser.
         /// </summary>
         constructor  Create       ; overload ;

         {#gendoc:hide:GENXDK}
         {#gendoc:hide:GENPDK}
         {#gendoc:hide:GENSCR}
         /// <summary>
         ///   A callback to be executed form TGIS_LayerVector upon binding
         ///   layer to the SQL query object (connecting field access methods)
         /// </summary>
         /// <param name="_bind_fun">
         ///   binding filed method
         /// </param>
         /// <param name="_field_bind_fun">
         ///   bind field access
         /// </param>
         /// <param name="_stat_fun">
         ///   statistics access method
         /// </param>
         /// <remarks>
         ///   Only for internal use of TatukGIS.
         /// </remarks>
         procedure BindSqlQueryCallBack(
                                    _bind_fun       : TGIS_SqlQueryBindFun      ;
                                    _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
                                    _stat_fun       : TGIS_SqlQueryStatFun
                                   ) ;

         /// <summary>
         ///   Calling once per scope. Prepares a statement for future
         ///   record-by-record comparisons.
         /// </summary>
         /// <param name="_query">
         ///   query text - closely mimics SQL WHERE part of SELECT statement
         /// </param>
         /// <exception cref="EGIS_Exception">
         ///   GIS_RS_ERR_SQLQUERY
         /// </exception>
         /// <returns>
         ///    -1 if failed, positive number of parsed position
         /// </returns>
         /// <remarks>
         ///   <note type="note">
         ///    Only forObsoleted! Use overloaded method instead.
         ///    </note>
         /// </remarks>
         function    Prepare       ( const _query : String
                                   ) : Integer ;

         /// <summary>
         ///   RePrepare precompiled a query. Will refresh any field bindings.
         /// </summary>
         procedure   RePrepare     ;

         {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
         /// <summary>
         ///   Parse each record. Because the resulting value for the call-back
         ///   function is always a string, then the kind of operation /string
         ///   or numerical/ will be discovered based on right operand in the
         ///   query given by Prepare() .
         /// </summary>
         /// <param name="_field_fun">
         ///   obsoleted - has no meaning anymore; used only upon unit tests
         /// </param>
         /// <param name="_bind_fun">
         ///   obsoleted - has no meaning anymore
         /// </param>
         /// <param name="_field_bind_fun">
         ///   obsoleted - has no meaning anymore
         /// </param>
         /// <param name="_shape">
         ///   shape from which attribute will be parsed; if nil then
         ///   attributes from the current shape will be used
         /// </param>
         /// <param name="_cursor">
         ///   cursor id
         /// </param>
         /// <exception cref="EGIS_Exception">
         ///   GIS_RS_ERR_SQLQUERY
         /// </exception>
         /// <returns>
         ///    parsed result value
         /// </returns>
         /// <remarks>
         ///   <note type="note">
         ///    Obsoleted! Use overloaded method instead.
         ///    </note>
         /// </remarks>
         function    Parse(
                       _field_fun      : TGIS_SqlQueryFieldFun     ;
                       _bind_fun       : TGIS_SqlQueryBindFun      ;
                       _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
                       _shape          : TObject                   ;
                       const _cursor   : Integer
                     ) : Variant ; overload ;

         /// <summary>
         ///   Parse each record. Because the resulting value for the call-back
         ///   function is always a string, then the kind of operation /string
         ///   or numerical/ will be discovered based on right operand in the
         ///   query given by Prepare() .
         /// </summary>
         /// <param name="_shape">
         ///   shape from which attribute will be parsed; if nil then
         ///   attributes from the current shape will be used
         /// </param>
         /// <param name="_cursor">
         ///   cursor id
         /// </param>
         /// <exception cref="EGIS_Exception">
         ///   GIS_RS_ERR_SQLQUERY
         /// </exception>
         /// <returns>
         ///    parsed result value
         /// </returns>
         function    Parse(
                       _shape          : TObject                   ;
                       const _cursor   : Integer
                     ) : Variant ; overload ;

         {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
         /// <summary>
         ///   Parse each record and cast result as boolean. Because the
         ///   resulting value for the call-back function is always a string,
         ///   then kind of operation /string or numerical/ will be discovered
         ///   based on right operand in the query given by Prepare() .
         /// </summary>
         /// <param name="_field_fun">
         ///   obsoleted - has no meaning anymore; used only upon unit tests
         /// </param>
         /// <param name="_bind_fun">
         ///   obsoleted - has no meaning anymore
         /// </param>
         /// <param name="_field_bind_fun">
         ///   obsoleted - has no meaning anymore
         /// </param>
         /// <param name="_shape">
         ///   shape from which attribute will be parsed; if nil then
         ///   attributes from the current shape will be used
         /// </param>
         /// <param name="_default">
         ///   default value if expression can not be represented as boolean
         /// </param>
         /// <param name="_cursor">
         ///   cursor id
         /// </param>
         /// <returns>
         ///    parsed result value as boolean True or False
         /// </returns>
         /// <remarks>
         ///   <note type="note">
         ///    Obsoleted! Use overloaded method instead.
         ///    </note>
         /// </remarks>
         function    ParseAsBoolean(
                       _field_fun      : TGIS_SqlQueryFieldFun     ;
                       _bind_fun       : TGIS_SqlQueryBindFun      ;
                       _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
                       _shape          : TObject                   ;
                       const _default  : Boolean                   ;
                       const _cursor   : Integer
                     ) : Boolean ; overload ;

         {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
         /// <summary>
         ///   Parse each record and cast result as boolean. Because the
         ///   resulting value for the call-back function is always a string,
         ///   then kind of operation /string or numerical/ will be discovered
         ///   based on right operand in the query given by Prepare() .
         /// </summary>
         /// <param name="_shape">
         ///   shape from which attribute will be parsed; if nil then
         ///   attributes from the current shape will be used
         /// </param>
         /// <param name="_default">
         ///   default value if expression can not be represented as boolean
         /// </param>
         /// <param name="_cursor">
         ///   cursor id
         /// </param>
         /// <returns>
         ///    parsed result value as boolean True or False
         /// </returns>
         function    ParseAsBoolean(
                       _shape          : TObject                   ;
                       const _default  : Boolean                   ;
                       const _cursor   : Integer
                     ) : Boolean ; overload ;

         {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
         /// <summary>
         ///   Parse each record and cast result as float. Because the
         ///   resulting value for the call-back function is always a string,
         ///   then kind of operation /string or numerical/ will be discovered
         ///   based on right operand in the query given by Prepare() .
         /// </summary>
         /// <param name="_field_fun">
         ///   obsoleted - has no meaning anymore; used only upon unit tests
         /// </param>
         /// <param name="_bind_fun">
         ///   obsoleted - has no meaning anymore
         /// </param>
         /// <param name="_field_bind_fun">
         ///   obsoleted - has no meaning anymore
         /// </param>
         /// <param name="_shape">
         ///   shape from which attribute will be parsed; if nil then
         ///   attributes from the current shape will be used
         /// </param>
         /// <param name="_default">
         ///   default value if expression can not be represented as float
         /// </param>
         /// <param name="_cursor">
         ///   cursor id
         /// </param>
         /// <returns>
         ///    parsed result value as float
         /// </returns>
         /// <remarks>
         ///   <note type="note">
         ///    Obsoleted! Use overloaded method instead.
         ///    </note>
         /// </remarks>
         function    ParseAsFloat(
                       _field_fun      : TGIS_SqlQueryFieldFun     ;
                       _bind_fun       : TGIS_SqlQueryBindFun      ;
                       _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
                       _shape          : TObject                   ;
                       const _default  : Double                    ;
                       const _cursor   : Integer
                    ) : Double  ; overload ;

         {#gendoc:hide:GENPDK} { TODO -cPDK : Verify }
         /// <summary>
         ///   Parse each record and cast result as float. Because the
         ///   resulting value for the call-back function is always a string,
         ///   then kind of operation /string or numerical/ will be discovered
         ///   based on right operand in the query given by Prepare() .
         /// </summary>
         /// <param name="_shape">
         ///   shape from which attribute will be parsed; if nil then
         ///   attributes from the current shape will be used
         /// </param>
         /// <param name="_default">
         ///   default value if expression can not be represented as float
         /// </param>
         /// <param name="_cursor">
         ///   cursor id
         /// </param>
         /// <returns>
         ///    parsed result value as float
         /// </returns>
         function ParseAsFloat(
                       _shape          : TObject                   ;
                       const _default  : Double                    ;
                       const _cursor   : Integer
                    ) : Double  ; overload ;

         /// <summary>
         ///   Current query text.
         /// </summary>
         property Query : String read queryStr ;
     end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.DateUtils,

    GisInternals,
    GisResource,
    GisClasses,
    GisLayer,
    GisLayerSublayer,
    GisLayerVector;
{$ENDIF}

const
  MAX_STACK = 512 ; // maximum size of various stacks
  MAX_ARGS  =  10 ; // maximum number of function args

type

  // Support parsing of LIKE statements with support for '_' and '%'.
  //
  T_LikeParser = class
    private
      charVect : array[0..65535] of Cardinal ;
      stateVal : Cardinal ;
      wildMask : Cardinal ;
      endMask  : Cardinal ;
    public

      //  Prepare (precompile) a pattern for text matching.
      //  Done for optimization. Done for optimization.
      //  Once prepared - several times parsed.
      //  _pattern  pattern to be compiled
      procedure Prepare( const _pattern : String  ) ;

      // Parse a given string using pattern sets in the Prepared procedure.
      // _text   text to be parsed
      // return True if matching text ; False, if no matching text
      function  Parse  ( const _text    : Variant ) : Boolean ;
  end ;

  {$IFDEF OXYGENE}
    T_FunctionDelegate = delegate             ( const _arg    : TGIS_VariantArray ;
                                                const _count  : Integer
                                              ) : Variant ;
  {$ELSE}
    T_FunctionDelegate = reference to function( const _arg    : TGIS_VariantArray ;
                                                const _count  : Integer
                                              ) : Variant ;
  {$ENDIF}

  T_FunctionParser = class
    private
      fncName   : String ;
      fncMethod : T_FunctionDelegate ;
    public
      procedure Prepare ( const _name   : String ;
                          const _method : T_FunctionDelegate
                        ) ;
      function Calculate( const _arg    : TGIS_VariantArray ;
                          const _count  : Integer
                        ) : Variant ;
  end ;

  // Supported type of items.
  T_SqlItemType = (

      // unknown item
      itUnknown,

      // field name
      itName,

      // statistical result
      itStat,

      // function name
      itFunction,

      // value (literal. number etc.)
      itValue,

      // numerical value
      itNumber,

      // like operand
      itLike,

      // operator type
      itOperator
  ) ;

  // Supported type of operators. Don't change the order!
  T_SqlOperator = (

      // unknown type
      opUNKNOWN,

      // beginning of query
      opBEG,

      // end of query
      opEND,

      // parenthesis close
      opCLS,

      // parenthesis open
      opOPN,

      // compare BETWEEN
      opBETWEEN,

      // logical AND
      opAND,

      // logical OR
      opOR,

      // logical NOT
      opNOT,

      // literal/numerical equal
      opEQ,

      // literal/numerical non equal
      opNE,

      // literal/numerical less then
      opLT,

      // literal/numerical less equal
      opLE,

      // literal/numerical greater then
      opGT,

      // literal/numerical greater equal
      opGE,

      // literal like
      opLIKE,

      // numerical unary add
      opUADD,

      // numerical subtract
      opUSUB,

      // numerical add
      opADD,

      // numerical unary add
      opSUB,

      // numerical multiply
      opMUL,

      // numerical divide
      opDIV,

      // numerical modulo
      opMOD,

      // compare IN
      opIN,

      // NULL comparison operator (temporary)
      opISTmp,

      // NULL comparison operator
      opISNULL,

      // NULL comparison operator
      opISNOTNULL,

      // function operator
      opFUNCTION
  ) ;

  function Ord_SqlOperator(
    const _op : T_SqlOperator
  ) : Integer ;
    {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
  begin
    Result := Integer(_op) ;
  end ;

{$IFDEF DCC}
  {$DEFINE SQLITEM_RECORDS}
{$ENDIF}

type
  // Stack item.
  T_SqlItem = {$IFDEF SQLITEM_RECORDS} record
              {$ELSE}                  class
              {$ENDIF}
    public
      // Type of item.
      ItemType : T_SqlItemType ;

      // Representation of value.
      Value : Variant ;

      // Internal value.
      Tag : Integer ;
    public
      {$IFDEF OXYGENE}
      constructor Create      ;
      {$ENDIF}
      constructor Create      ( const _val : T_SqlItem
                              ) ; overload;
      constructor Create      ( const _itemtype : T_SqlItemType ;
                                const _value    : Variant
                              ) ; overload;
      constructor Create      ( const _tag      : Integer
                              ) ; overload;
      constructor Create      ( const _val : T_SqlOperator
                              ) ; overload;
  end ;

  // List of precompiled items.
  T_CompiledList = class( TGIS_Object )
    private
      objList : TList< T_SqlItem > ;
      function fget_Item( _index : Integer ) : T_SqlItem ;
      function fget_Count                    : Integer   ;

    protected
      // Destroy instance and free all items.
      procedure doDestroy   ; override;

    public
      constructor  Create   ;

      //Clear list and free all items.
      procedure Clear       ;

      // Test is listis empty
      // return True if list is empty
      function  IsEmpty     : Boolean ;

      // Add an item to a compiled list.
      // _item item to be added
      procedure Add         ( const _item : T_SqlItem     ) ;

      // Add an operator item to a compiled list.
      // _item operator to be added
      procedure AddOperator ( const _val  : T_SqlOperator ) ;

      // Get the last element from the list.
      // return  last element or nil
      function  Last        : T_SqlItem ;

      // Delete the last element from the list.
      procedure DeleteLast  ;
    public
      property Items[_index: Integer]: T_SqlItem read fget_Item ; default ;
      property Count : Integer read fget_Count ;
  end ;

  // List of fields.
  T_FieldList = class( TGIS_Object )
    private
      nameList : TGIS_StringList ;
      objList : TList< T_SqlItem > ;

      function  fget_Item ( _index : Integer ) : T_SqlItem ;
      function  fget_Name ( _index : Integer ) : String    ;
      function  fget_Count                     : Integer   ;
      procedure fset_Tag  ( _index : Integer; const _value : Integer ) ;
      procedure fset_Value( _index : Integer; const _value : Variant ) ;

    public
      constructor Create ;

    protected

      // Destroy instance and free all items.
      procedure doDestroy   ; override;

    public
      // Clear list and free all items.
      procedure Clear ;

      // Add a field to the field list.
      // _name field name
      function  Add   ( const _name : String;
                        const _bind : TGIS_SqlQueryBindFun
                      ) : Integer ;
    public
      property Items [_index: Integer]: T_SqlItem read  fget_Item  ; default ;
      property Names [_index: Integer]: String    read  fget_Name  ;
      property Tags  [_index: Integer]: Integer   write fset_Tag   ;
      property Values[_index: Integer]: Variant   write fset_Value ;
      property Count : Integer read fget_Count ;
  end ;

  // List of stats.
  T_StatList = class( TGIS_Object )
    private
      nameList  : TGIS_StringList ;
      fieldList : TGIS_StringList ;
      objList : TList< T_SqlItem > ;

      function  fget_Item ( _index : Integer ) : T_SqlItem ;
      function  fget_Name ( _index : Integer ) : String    ;
      function  fget_Field( _index : Integer ) : String    ;
      function  fget_Count                     : Integer   ;
      procedure fset_Value( _index : Integer; const _value : Variant ) ;

    public
      constructor Create ;

    protected

      // Destroy instance and free all items.
      procedure doDestroy   ; override;

    public
      // Clear list and free all items.
      procedure Clear ;

      // Add a statistic to to the statistics list.
      // _name field name
      function  Add   ( const _name  : String;
                        const _field : String
                      ) : Integer ;
    public
      property Items [_index: Integer]: T_SqlItem read  fget_Item  ; default ;
      property Names [_index: Integer]: String    read  fget_Name  ;
      property Fields[_index: Integer]: String    read  fget_Field ;
      property Values[_index: Integer]: Variant   write fset_Value ;
      property Count : Integer read fget_Count ;
  end ;
  // List of fields.
  T_LikeList = class( TGIS_ObjectList )
    private

      // Items property access function.
      // _index  index position
      // return  pointed item
      function fget_Item  ( _index : Integer
                          ) : T_LikeParser ; reintroduce ;

    public
      {$IFNDEF OXYGENE}
        constructor Create ;
      {$ENDIF}

      // Add an item to the precompiled list, e.g. statements list.
      // _text text of statement
      function  Add   ( const _name : String ) : Integer ;
      property  Items[_index: Integer]: T_LikeParser
                                        read fget_Item ;
                                        default ;
                                        {$IFDEF OXYGENE} reintroduce ; {$ENDIF}
  end ;

  // List of functions.
  T_FunctionList = class( TGIS_ObjectList )
    private
      oFuncNames : TDictionary<String, T_FunctionDelegate> ;
    private

      // Items property access function.
      // _index  index position
      // return  pointed item
      function fget_Item ( _index : Integer
                         ) : T_FunctionParser ; reintroduce ;

    public
      constructor Create ;
      {$IFNDEF OXYGENE}
        destructor Destroy ; override;
      {$ENDIF}

      // Add an item to the precompiled list, e.g. statements list.
      // _text text of statement
      function  Add       ( const _name : String ) : Integer ;
      function  IsFunction( const _name : String ) : Boolean ;
      property  Items[_index: Integer]: T_FunctionParser
                                        read fget_Item ;
                                        default ;
                                        {$IFDEF OXYGENE} reintroduce ; {$ENDIF}
  end ;


  // Stack for items (names|literals|numbers). Operators have a different stack
  //  see: TOperatorStack.
  T_ItemStack = class
    private
      itemStore : array of T_SqlItem ;
      itemCount : Integer ;
    public
      constructor Create     ;

    public
      // Clear list and free all items.
      procedure  Clear       ;

      // Test is stack is empty
      // return True if stack is empty
      function   IsEmpty     : Boolean ;

      // Push an item onto the stack.
      // _item item to be pushed
      procedure  Push        ( const _item : T_SqlItem     ) ;

      // Push a value item onto the stack.
      // _val value to be pushed
      procedure  PushValue   ( const _val  : Variant       ) ;

      // Push an operator item onto the stack.
      // _op operator to be pushed
      procedure  PushOperator( const _op   : T_SqlOperator ) ;

      // Return a pointer to the top item onto the stack
      // return pointer to the top item
      function   Top         : T_SqlItem ;

      // Pop an item from the stack.
      // return pointer to item which was just removed from stack
      function   Pop         : T_SqlItem ;

    public
      property  Count : Integer read itemCount ;
  end ;

  // Stack for operators. Items (names|literals|numbers) have a different stack.
  //  See: TItemStack.
  T_OperatorStack = class
    private
      itemStore : array of T_SqlOperator ;
      itemCount : Integer ;
    public
      constructor Create ;

      // Clear list and free all items.
      procedure Clear   ;

      // Test is stack is empty
      // return True if stack is empty
      function  IsEmpty : Boolean ;

      // Push an item onto the stack.
      // _item item to be pushed
      procedure Push    ( const _item : T_SqlOperator ) ;

      // Return a pointer to the top item onto the stack
      // return pointer to the top item
      function  Top     : T_SqlOperator ;

      // Pop an item from the stack.
      // return pointer to item which was just removed from stack
      function  Pop     : T_SqlOperator ;
    public
      property  Count : Integer read itemCount ;
  end ;

  // SQL parser itself.
  T_SqlParser = class( TGIS_Object )
     private
       // Not empty, if last parsed element was a statistical function.
       wasStat : String ;

       // Not empty, if last parsed element was a function.
       wasFunction : String ;

       // True if last parsed element was LIKE.
       wasLike : Boolean ;

       // True if last parsed element was an operator, delimityer or
       // anything that can raise unary operator
       unaryMarker : Boolean ;

       // Query to be parsed.
       queryText : String ;

       // Current position in query.
       queryPos : Integer ;

       // Precompiled list.
       compiledList : T_CompiledList ;

       // Prefetched fields.
       fieldList : T_FieldList ;

       // Prefetched stats.
       statList : T_StatList ;

       // Functions computations.
       functionList : T_FunctionList ;

       // Prepared like computations.
       likeList : T_LikeList ;

       // Stack for values.
       itemStack : T_ItemStack ;

       // Stack for operators.
       operatorStack : T_OperatorStack ;

       // Stack for IN values.
       inStack : T_ItemStack ;

       // Array for function args.
       fncArgs  : TGIS_VariantArray ;
     private
     {$IFDEF OXYGENE} unit {$ENDIF}
       // Function for retrieving fields value.
       getFieldFun : TGIS_SqlQueryFieldFun     ;

       // Function for retrieving fields binding.
       getBindFun : TGIS_SqlQueryBindFun ;

       // Function for retrieving fields values.
       getFieldBindFun : TGIS_SqlQueryFieldBindFun ;

       //Function for retrieving statitical values. }
       getStatFun : TGIS_SqlQueryStatFun ;

     private
       // Create a stack item of Like type.
       // _val  name of item
       function makeLike       ( const _val : String
                               ) : T_SqlItem ;

       // Create a stack item of Name type.
       // _val  name of item
       function makeName       ( const _val : String
                               ) : T_SqlItem ;

       // Create a stack item of statistical value.
       // _val  name of item
       function makeStat       ( const _name : String ;
                                 const _val  : String
                               ) : T_SqlItem ;

       // Create a stack item of Function type.
       // _val  name of item
       function makeFunction   ( const _val : String
                               ) : T_SqlItem ;

       // Create a stack item of Value type.
       // _val  literal value of item
       function makeValue      ( const _val : Variant
                               ) : T_SqlItem ;

       // Access a character from query text.
       // return current character
       function  peekChar      : Char ;

       // Access next character from query text.
       // return next character
       function  getChar       : Char ;

       // Is query text reached?
       // return True - no more characters in query text;
       //        False - still few characters in query text
       function  eofChar       : Boolean ;

       // Parse pre-recognized Name item.
       procedure parseName     ;

       // Parse pre-recognized Macro item.
       procedure parseMacro    ;

       // Parse Macro parameters.
       procedure parseMacroParams(
                                 var   _ar  : TGIS_VariantArray
                               )  ;
       // Parse pre-recognized Number item.
       procedure parseNumber   ;

       // Parse pre-recognized Literal item.
       procedure parseLiteral  ;

       // Parse pre-recognized Operator item.
       procedure parseOperator ;

       // Parse pre-recognized parenthesis item.
       procedure parseParenthesis ;

       // Do unary calculation. Calculation will be done in the following manner:
       //     Right   := Stack.Pop
       //     Result  := operation Right
       //     Stack.Push( Result )
       // _op operator (make calculations with it).
       procedure doUnary       ( const _op : T_SqlOperator
                               ) ;

       // Do calculation. Calculation will be done in the following manner:
       //     Right   := Stack.Pop
       //     Left    := Stack.Pop
       //     Result  := Left operation Right
       //     Stack.Push( Result )
       // _op operator (make calculations with it).
       procedure doArithmetic  ( const _op : T_SqlOperator
                               ) ;

       // Do literal and numerical comparison of te two elements that are at the
       // top  of the stack. Result will be pushed onto the stack.
       // _op operator (make comparison with it).
       procedure doCompare     ( const _op : T_SqlOperator
                               ) ;

       // Do logical AND|OR comparison of two elements that are at the top of
       // the stack. Result will be pushed onto the stack.
       // _op operator (make comparison with it).
       procedure doLogical     ( const _op : T_SqlOperator
                               ) ;

       // Do logical NOT comparison of two elements that are at the top of the
       // stack. Result will be pushed onto the stack.
       // _op operator (make comparison with it).
       procedure doLogicalNot  ( const _op : T_SqlOperator
                               ) ;

       // Do logical NOT comparison of two elements that are at the top of the
       // stack. Result will be pushed onto the stack.
       //  _op operator (make comparison with it).
       procedure doNullable    ( const _op : T_SqlOperator
                               ) ;

       // Do literal LIKE comparison of two elements that are at the top of the
       // stack. Result will be pushed onto the stack.
       // _op operator (make comparison with it).
       procedure doLike        ( const _op : T_SqlOperator
                               ) ;

       // Do FUNCTION calculation on element that is at the top of the
       // stack. Result will be pushed onto the stack.
       procedure doFunction    ( const _op : T_SqlOperator
                               ) ;

       // Do literal LIKE comparison of two elements that are at the top of the
       // stack.  Result will be pushed onto the stack.
       procedure doIn          ;

       // Resolve a given item against a database field.
       // _val    item to be resolved
       // return  resolved item
       function  resolve       ( const _val : T_SqlItem
                               ) : Variant ;

       // Store item onto the stack.
       // _item item to be stored
       procedure storeItem     ( const _item : T_SqlItem
                               ) ;
       // Interpret operator. To initialize calculation, call with enD.
       // To reprieve final  result call with enD.
       // _item operator
       procedure doOperator    ( const _item : T_SqlOperator
                               ) ;


       // True if _name represents statiscical function.
       // _name element name
       function  isStatistical ( const _name : String
                               ) : Boolean ;



     protected
       procedure doDestroy  ; override;

     public
       // constructors / destructor
       constructor  Create  ;

       // Prepare (precompile) a query. Error of compilation will be reported as
       // an exception.
       // _text   query text
       // return  position on which syntax error occurs or -1
       function    Prepare( const _text : String
                          ) : Integer ;

       // RePrepare precompiled a query. Will refresh any field binding.
       procedure   RePrepare ;

       // Parse a query, one record per call.
       // _shape          shape from which attribute will be parsed; if nil
       //                 then attributes from the current shape will be used
       // _cursor         cursor id
       // return          parsing result
       function    Parse  ( const _shape          : TObject                   ;
                            const _cursor         : Integer
                          ) : Variant ;
  end ;

//=============================================================================
// T_SqlItem
//=============================================================================

  {$IFDEF OXYGENE}
  constructor T_SqlItem.Create ;
  begin
    {$IFNDEF SQLITEM_RECORDS}
      inherited Create ;
    {$ENDIF}
    ItemType := T_SqlItemType.itUnknown ;
    Value    := Unassigned ;
    Tag      := 0         ;
  end ;
  {$ENDIF}

  constructor T_SqlItem.Create(
    const _val : T_SqlItem
  ) ;
  begin
    {$IFNDEF SQLITEM_RECORDS}
      inherited Create ;
    {$ENDIF}
    ItemType := _val.ItemType ;
    Value    := _val.Value    ;
    Tag      := _val.Tag      ;
  end ;

  constructor T_SqlItem.Create(
    const _itemtype : T_SqlItemType ;
    const _value    : Variant
  ) ;
  begin
    {$IFNDEF SQLITEM_RECORDS}
      inherited Create ;
    {$ENDIF}
    ItemType := _itemtype ;
    Value    := _value    ;
    Tag      := 0         ;
  end ;

  constructor T_SqlItem.Create(
    const _tag : Integer
  ) ;
  begin
    {$IFNDEF SQLITEM_RECORDS}
      inherited Create ;
    {$ENDIF}
    ItemType := T_SqlItemType.itUnknown ;
    Value    := Unassigned ;
    Tag      := _tag       ;
  end ;

  constructor T_SqlItem.Create(
    const _val : T_SqlOperator
  ) ;
  begin
    {$IFNDEF SQLITEM_RECORDS}
      inherited Create ;
    {$ENDIF}
    ItemType := T_SqlItemType.itOperator ;
    Value    := Ord_SqlOperator( _val ) ;
  end ;

//=============================================================================
// TLikeParser
//=============================================================================

  procedure T_LikeParser.Prepare(
    const _pattern : String
  ) ;
  var
    i        : Integer ;
    len      : Integer ;
    any_mask : Cardinal ;

    function bit_mask( const _bit : Integer ) : Cardinal ;
    begin
      Result := not( $80000000 shr _bit ) ;
    end ;

  begin
    // set defaults
       for i:= 0 to high( charVect ) do
         charVect[i] := $ffffffff ;

       any_mask := $ffffffff ;
       wildMask := $ffffffff ;

    // add dummy first char to the character vector ;
       charVect[ ord( '_' ) ] := charVect[ ord( '_' ) ] and bit_mask(0) ;

    // compile parsing pattern
       len := 1 ;
       i := StringFirst - 1 ;
       while i < StringLast( _pattern ) do begin
         inc( i ) ;
         case _pattern[i] of
           '_' : begin
                   // any single character
                   any_mask := any_mask and bit_mask( len ) ;
                 end ;
           '%' : begin
                   // any substring
                   wildMask := wildMask and bit_mask( len-1 ) ;
                   continue ;
                 end
           else  begin
                   // strict match
                   charVect[ ord( _pattern[i] ) ] := charVect[ ord( _pattern[i] ) ]
                                                   and bit_mask( len ) ;
                 end ;
         end ;
         inc( len ) ;
       end ;
       endMask := not bit_mask( len - 1 ) ;

    // set '_' flag
       if any_mask <> $ffffffff then begin
         for i:=0 to high( charVect ) do
           charVect[i] := charVect[i] and any_mask ;
         end ;

  end ;

  function T_LikeParser.Parse(
    const _text : Variant
  ) : Boolean ;
  var
    i    : Integer  ;
    a    : Cardinal ;
    b    : Cardinal ;
    stmp : String   ;
    o    : Integer  ;
    cv   : Cardinal ;
  begin
    Result := False ;

    if VarIsNull( _text ) then exit ;

    {$IFDEF OXYGENE}
      stmp := UpperCase( VarToString( _text ) ) ;
    {$ELSE}
      stmp := AnsiUpperCase( _text ) ;
    {$ENDIF}

    stateVal := $7fffffff ; // bit 0
    for i := StringFirst to StringLast( stmp ) do begin
      o := ord( stmp[i] ) ;
      if o >= 0 then
        cv := charVect[ o ]
      else 
        cv := $ffffffff ;

      a := ( stateVal shr 1 ) or cv ;         
      b := stateVal or wildMask ;
      stateVal := a and b ;
      if stateVal = $FFFFFFFF then exit ;
      if ( stateVal and endMask ) = 0 then begin
        if i=StringLast( stmp ) then begin
          Result := True ;
          exit ;
        end ;
      end ;
    end ;

  end ;

//=============================================================================
// TCompiledList
//=============================================================================

  constructor T_CompiledList.Create ;
  begin
    inherited ;
    objList := TList< T_SqlItem>.Create ;
  end ;

  procedure T_CompiledList.doDestroy ;
  begin
    Clear ;
    FreeObject( objList ) ;
    inherited ;
  end ;

  procedure T_CompiledList.Clear ;
  begin
    objList.Clear ;
  end ;

  function T_CompiledList.IsEmpty : Boolean ;
  begin
    Result := Count <= 0 ;
  end ;

  function T_CompiledList.fget_Item(
    _index : Integer
  ) : T_SqlItem ;
  begin
    Result := T_SqlItem( objList[_index] ) ;
  end ;

  function T_CompiledList.fget_Count
    : Integer ;
  begin
    Result := objList.Count ;
  end ;

  procedure T_CompiledList.Add(
    const _item : T_SqlItem
  ) ;
  begin
    objList.Add( _item ) ;
  end ;

  procedure T_CompiledList.AddOperator(
    const _val : T_SqlOperator
  ) ;
  begin
    objList.Add( T_SqlItem.Create( _val ) ) ;
  end ;

  function T_CompiledList.Last : T_SqlItem ;
  begin
    Result := Items[ Count-1 ] ;
  end ;

  procedure T_CompiledList.DeleteLast ;
  begin
    {$IFDEF OXYGENE}
      objList.RemoveAt( Count - 1 ) ;
    {$ELSE}
      objList.Delete( Count - 1 ) ;
    {$ENDIF}
  end ;

//=============================================================================
// TFieldList
//=============================================================================

  constructor T_FieldList.Create ;
  begin
    inherited ;
    nameList := TGIS_StringList.Create ;
    objList := TList< T_SqlItem>.Create ;
  end ;

  procedure T_FieldList.doDestroy ;
  begin
    FreeObject( nameList ) ;
    FreeObject( objList  ) ;
    inherited ;
  end ;

  procedure T_FieldList.Clear ;
  begin
    nameList.Clear ;
    objList.Clear  ;
  end ;

  function T_FieldList.fget_Item(
    _index : Integer
  ) : T_SqlItem ;
  begin
    Result := objList[_index ] ;
  end ;

  function T_FieldList.fget_Name(
    _index : Integer
  ) : String    ;
  begin
    Result := nameList[_index ] ;
  end ;

  procedure T_FieldList.fset_Tag(
          _index : Integer ;
    const _value : Integer
  ) ;
  {$IFDEF SQLITEM_RECORDS}
    var
      itm : T_SqlItem ;
    begin
      itm := objList[ _index ] ;
      itm.Tag := _value ;
      objList[ _index ] := itm ;
    end ;
  {$ELSE}
    begin
      objList[ _index ].Tag := _value ;
    end ;
  {$ENDIF}

  procedure T_FieldList.fset_Value(
          _index : Integer ;
    const _value : Variant
  ) ;
  {$IFDEF SQLITEM_RECORDS}
    var
      itm : T_SqlItem ;
    begin
      itm := objList[ _index ] ;
      itm.Value := _value ;
      objList[ _index ] := itm ;
    end ;
  {$ELSE}
    begin
      objList[ _index ].Value := _value ;
    end ;
  {$ENDIF}

  function T_FieldList.fget_Count
    : Integer ;
  begin
     Result := objList.Count ;
  end ;

  function T_FieldList.Add(
    const _name : String;
    const _bind : TGIS_SqlQueryBindFun
  ) : Integer ;
  begin
    Result := nameList.IndexOf( _name ) ;
    if Result < 0 then begin
      Result := nameList.Add( _name ) ;
      objList.Add( T_SqlItem.Create( -1 ) ) ;
    end ;
  end ;

//=============================================================================
// TStatsdList
//=============================================================================

  constructor T_StatList.Create ;
  begin
    inherited ;
    nameList  := TGIS_StringList.Create ;
    fieldList := TGIS_StringList.Create ;
    objList := TList< T_SqlItem>.Create ;
  end ;

  procedure T_StatList.doDestroy ;
  begin
    FreeObject( nameList  ) ;
    FreeObject( fieldList  ) ;
    FreeObject( objList  ) ;
    inherited ;
  end ;

  procedure T_StatList.Clear ;
  begin
    nameList.Clear ;
    fieldList.Clear ;
    objList.Clear  ;
  end ;

  function T_StatList.fget_Item(
    _index : Integer
  ) : T_SqlItem ;
  begin
    Result := objList[_index ] ;
  end ;

  function T_StatList.fget_Name(
    _index : Integer
  ) : String    ;
  begin
    Result := nameList[_index ] ;
  end ;

  function T_StatList.fget_Field(
    _index : Integer
  ) : String    ;
  begin
    Result := fieldList[_index ] ;
  end ;

  procedure T_StatList.fset_Value(
          _index : Integer ;
    const _value : Variant
  ) ;
  {$IFDEF SQLITEM_RECORDS}
    var
      itm : T_SqlItem ;
    begin
      itm := objList[ _index ] ;
      itm.Value := _value ;
      objList[ _index ] := itm ;
    end ;
  {$ELSE}
    begin
      objList[ _index ].Value := _value ;
    end ;
  {$ENDIF}

  function T_StatList.fget_Count
    : Integer ;
  begin
     Result := objList.Count ;
  end ;

  function T_StatList.Add(
    const _name  : String;
    const _field : String
  ) : Integer ;
  begin
    Result := nameList.IndexOf( _name ) ;
    if Result < 0 then begin
      Result := nameList.Add( _name ) ;
      Result := fieldList.Add( _field ) ;
      objList.Add( T_SqlItem.Create( -1 ) ) ;
    end ;
  end ;

//=============================================================================
// TLikeList
//=============================================================================

  {$IFNDEF OXYGENE}
    constructor T_LikeList.Create ;
    begin
      inherited ;
      OwnsObjects := True ;
    end ;
  {$ENDIF}

  function T_LikeList.fget_Item(
    _index : Integer
  ) : T_LikeParser ;
  begin
    Result := T_LikeParser( inherited Items[_index] ) ;
  end ;

  function T_LikeList.Add(
    const _name : String
  ) : Integer ;
  var
    obj : T_LikeParser ;
  begin
    obj := T_LikeParser.Create ;
    {$IFDEF OXYGENE}
      obj.Prepare( UpperCase( _name ) ) ;
    {$ELSE}
      obj.Prepare( AnsiUpperCase( _name ) ) ;
    {$ENDIF}
    Result := inherited Add( obj ) ;
  end ;

//=============================================================================
// T_FunctionList
//=============================================================================

  constructor T_FunctionList.Create ;
  var
    fnc : T_FunctionDelegate ;
  begin
    inherited ;
    OwnsObjects := True ;

    oFuncNames := TDictionary<String, T_FunctionDelegate>.Create(
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

    // Math
    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Abs( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'ABS', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := ArcSin( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'ASIN', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := ArcCos( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'ACOS', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := ArcTan( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'ATAN', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := CeilS( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'CEIL', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Cos( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'COS', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := RadToDeg( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'DEGREES', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Exp( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'EXP', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := FloorS( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'FLOOR', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Ln( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'LN', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 1 then
        Result := LogN( VarToDouble(_arg[1]), VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'LOG', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Log10( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'LOG10', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      Result := Pi ;
    end ;
    oFuncNames.Add( 'PI', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 1 then
        Result := Power( VarToDouble(_arg[1]), VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'POWER', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := DegToRad( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'RADIANS', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count = 1 then
        Result := RoundS( VarToDouble(_arg[0]) )
      else if _count > 1 then
        Result := RoundTo( VarToDouble(_arg[1]), -VarToInt32(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'ROUND', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Sin( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'SIN', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Sqrt( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'SQRT', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Tan( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'TAN', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := TruncS( VarToDouble(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'TRUNC', fnc ) ;

    // Strings
    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := length( VarToString(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'LENGTH', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := UpperCase( VarToString(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'UPPER', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := LowerCase( VarToString(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'LOWER', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 1 then
        Result := Pos( VarToString(_arg[0]), VarToString(_arg[1]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'STRPOS', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 2 then
        Result := Copy( VarToString(_arg[2]), VarToInt32(_arg[1]), VarToInt32(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'SUBSTR', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 1 then
        Result := Copy( VarToString(_arg[1]), StringFirst, VarToInt32(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'LEFT', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 1 then
        Result := Copy( VarToString(_arg[1]),
                        length(VarToString(_arg[1]))-VarToInt32(_arg[0])+StringFirst,
                        VarToInt32(_arg[0])
                        )
                      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'RIGHT', fnc ) ;

    fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
    begin
      if _count > 0 then
        Result := Trim( VarToString(_arg[0]) )
      else
        Result := Unassigned ;
    end ;
    oFuncNames.Add( 'TRIM', fnc ) ;
  end ;

  {$IFNDEF OXYGENE}

    destructor T_FunctionList.Destroy ;
    begin
      FreeObject( oFuncNames ) ;

      inherited ;
    end ;

  {$ENDIF}

  function T_FunctionList.fget_Item(
    _index : Integer
  ) : T_FunctionParser ;
  begin
    Result := T_FunctionParser( inherited Items[_index] ) ;
  end ;

  function T_FunctionList.IsFunction(
    const _name : String
  ) : Boolean ;
  begin
    Result := oFuncNames.ContainsKey( _name ) ;
  end ;

  function T_FunctionList.Add(
    const _name : String
  ) : Integer ;
  var
    obj : T_FunctionParser ;
    fnc : T_FunctionDelegate ;
  begin
    obj := T_FunctionParser.Create ;
    if oFuncNames.TryGetValue( _name, fnc ) then
      obj.Prepare( _name, fnc )
    else begin
      fnc := function( const _arg : TGIS_VariantArray; const _count : Integer ) : Variant
             begin
               Result := Unassigned ;
             end ;
      obj.Prepare( _name, fnc ) ;
    end ;

    Result := inherited Add( obj ) ;
  end ;

//=============================================================================
// T_FunctionParser
//=============================================================================

  procedure T_FunctionParser.Prepare(
    const _name   : String ;
    const _method : T_FunctionDelegate
  ) ;
  begin
    fncName := _name ;
    fncMethod := _method ;
  end ;

  function T_FunctionParser.Calculate(
    const _arg    : TGIS_VariantArray ;
    const _count  : Integer
  ) : Variant ;
  begin
    if not VarIsNull( _arg[0] ) then
      Result := fncMethod( _arg, _count )
    else
      Result := NaN ;
  end ;

//=============================================================================
// TItemStack
//=============================================================================

  constructor T_ItemStack.Create ;
  begin
    SetLength( itemStore, MAX_STACK ) ;
    Clear ;
  end ;

  procedure T_ItemStack.Clear ;
  begin
    itemCount := 0 ;
  end ;

  function T_ItemStack.IsEmpty : Boolean ;
  begin
    Result := itemCount <= 0 ;
  end ;

  procedure T_ItemStack.Push(
    const _item : T_SqlItem
  ) ;
  begin
    inc( itemCount ) ;

    if itemCount > MAX_STACK then begin
      itemCount := MAX_STACK ;
      Abort ;
    end ;

    itemStore[ itemCount - 1 ] := T_SqlItem.Create( _item.ItemType,
                                                    _item.Value
                                                  ) ;
  end ;

  procedure T_ItemStack.PushValue(
    const _val : Variant
  ) ;
  begin
    inc( itemCount ) ;

    if itemCount > MAX_STACK then begin
      itemCount := MAX_STACK ;
      Abort ;
    end ;

    itemStore[ itemCount - 1 ] := T_SqlItem.Create( T_SqlItemType.itValue,
                                                    _val
                                                  ) ;
  end ;

  procedure T_ItemStack.PushOperator(
    const _op : T_SqlOperator
  ) ;
  begin
    inc( itemCount ) ;

    if itemCount > MAX_STACK then begin
      itemCount := MAX_STACK ;
      Abort ;
    end ;

    itemStore[ itemCount - 1 ] := T_SqlItem.Create( T_SqlItemType.itOperator,
                                                    Ord_SqlOperator( _op )
                                                  ) ;
  end ;

  function  T_ItemStack.Top : T_SqlItem ;
  begin
    if itemCount < 1 then Abort ;

    Result := itemStore[ itemCount - 1 ] ;
  end ;

  function  T_ItemStack.Pop : T_SqlItem ;
  begin
    if itemCount < 1 then Abort ;

    dec( itemCount ) ;
    Result := itemStore[ itemCount ] ;

    // free not required: DCC uses records; other plaforms know what to do
  end ;

//=============================================================================
// T_OperatorStack
//=============================================================================

  constructor T_OperatorStack.Create ;
  begin
    SetLength( itemStore, MAX_STACK ) ;
    Clear ;
  end ;

  procedure T_OperatorStack.Clear ;
  begin
    itemCount := 0  ;
  end ;

  function T_OperatorStack.IsEmpty : Boolean ;
  begin
    Result := itemCount <= 0 ;
  end ;

  procedure T_OperatorStack.Push(
    const _item : T_SqlOperator
  ) ;
  begin
    inc( itemCount ) ;

    if itemCount > MAX_STACK then begin
      itemCount := MAX_STACK ;
      Abort ;
    end ;

    itemStore[ itemCount - 1 ] := _item ;
  end ;

  function  T_OperatorStack.Top : T_SqlOperator ;
  begin
    Result := itemStore[ itemCount - 1 ] ;
  end ;

  function  T_OperatorStack.Pop : T_SqlOperator ;
  begin
    dec( itemCount ) ;
    if itemCount >= 0 then
      Result := itemStore[ itemCount ]
    else
      Result := T_SqlOperator.opUNKNOWN ;
  end ;

//=============================================================================
// TSqlParser
//=============================================================================

  constructor T_SqlParser.Create ;
  begin
    inherited Create ;

    getFieldFun     := nil ;
    getBindFun      := nil ;
    getFieldBindFun := nil ;
    getStatFun      := nil ;

    compiledList  := T_CompiledList.Create  ;
    fieldList     := T_FieldList.Create     ;
    statList      := T_StatList.Create      ;
    functionList  := T_FunctionList.Create  ;
    likeList      := T_LikeList.Create      ;
    itemStack     := T_ItemStack.Create     ;
    operatorStack := T_OperatorStack.Create ;
    inStack       := T_ItemStack.Create     ;

    SetLength( fncArgs, MAX_ARGS ) ;
  end ;

  procedure T_SqlParser.doDestroy ;
  begin
    FreeObject( compiledList  ) ;
    FreeObject( fieldList     ) ;
    FreeObject( statList      ) ;
    FreeObject( functionList  ) ;
    FreeObject( likeList      ) ;
    FreeObject( itemStack     ) ;
    FreeObject( operatorStack ) ;
    FreeObject( inStack       ) ;

    inherited ;
  end ;

  function T_SqlParser.makeLike(
    const _val : String
  ) : T_SqlItem ;
  begin
    Result := T_SqlItem.Create( T_SqlItemType.itLike,
                                likeList.Add( _val )
                              ) ;
  end ;

  function T_SqlParser.makeName(
    const _val : String
  ) : T_SqlItem ;
  begin
    Result := T_SqlItem.Create( T_SqlItemType.itName,
                                fieldList.Add( _val, getBindFun )
                              ) ;
  end ;

  function T_SqlParser.makeStat(
    const _name : String ;
    const _val  : String
  ) : T_SqlItem ;
  begin
    Result := T_SqlItem.Create( T_SqlItemType.itStat,
                                statList.Add( _name, _val )
                              ) ;
  end ;

  function T_SqlParser.makeFunction(
    const _val : String
  ) : T_SqlItem ;
  begin
    Result := T_SqlItem.Create( T_SqlItemType.itFunction,
                                functionList.Add( _val )
                              ) ;
  end ;

  function T_SqlParser.makeValue(
    const _val : Variant
  ) : T_SqlItem ;
  begin
    Result := T_SqlItem.Create( T_SqlItemType.itValue,
                                _val
                              ) ;
  end ;

  function T_SqlParser.peekChar : Char ;
  begin
    Result := queryText[ queryPos ] ;
  end ;

  function T_SqlParser.getChar : Char ;
  begin
    Result := peekChar ;
    inc( queryPos ) ;
  end ;

  function T_SqlParser.eofChar : Boolean ;
  begin
    Result := queryPos > StringLast( queryText ) ;
  end ;

  procedure T_SqlParser.parseName ;
  var
    c      : Char           ;
    str    : TStringBuilder ;
    tmp    : String         ;
    oelm1  : T_SqlItem ;
    belm1  : Boolean   ;
    oelm2  : T_SqlItem ;
    belm2  : Boolean   ;
    wasnot : Boolean   ;
    first  : Boolean   ;
  begin
    str := TStringBuilder.Create ;
    try

      unaryMarker := False ;

      if peekChar = '"' then begin       // "xxx" syntax
        getChar ;
        {$IFDEF JAVA}
          str.setLength(0) ;
        {$ELSE}
          str.Length := 0 ;
        {$ENDIF}
        c := #0 ;
        while not eofChar do begin
           c := getChar ;
           if c = '"' then break ;
           str.Append( c ) ;
        end ;
        if ( c <> '"' ) or ( str.Length <= 0 ) then
          Abort ;

        tmp := str.ToString ;
        compiledList.Add( makeName( tmp ) ) ;
      end
      else if peekChar = '[' then begin  // [xxx] syntax
        getChar ;
        {$IFDEF JAVA}
          str.setLength(0) ;
        {$ELSE}
          str.Length := 0 ;
        {$ENDIF}
        c := #0 ;
        while not eofChar do begin
           c := getChar ;
           if c = ']' then break ;
           str.Append( c ) ;
        end ;
        if ( c <> ']' ) or ( str.Length <= 0 ) then
          Abort ;

        tmp := str.ToString ;
        compiledList.Add( makeName( tmp ) ) ;
      end
      else begin                         // xxx syntax /without ""/
        {$IFDEF JAVA}
          str.setLength(0) ;
        {$ELSE}
          str.Length := 0 ;
        {$ENDIF}
        first := True ;
        while not eofChar do begin
          if ( not first )
             and
             ( ord( peekChar ) < 128 )
             and
             ( not ( AnsiChar(peekChar) in
                     [ '.','A'..'Z','a'..'z','0'..'9','_','@','$','#']
                   )
             )
          then
            break ;

          str.Append( getChar ) ;
          first := False ;
        end ;
        tmp := str.ToString ;

        // if field name is the same as a function name
        if not IsStringEmpty( wasFunction ) then begin
           compiledList.Add( makeName( wasFunction ) ) ;
           wasFunction := '' ;
         end ;

        // after all it can be an operator
        if      CompareText( tmp, 'AND'     ) = 0 then begin
                  compiledList.AddOperator( T_SqlOperator.opAND ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'OR'      ) = 0 then begin
                compiledList.AddOperator( T_SqlOperator.opOR ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'NOT'     ) = 0 then begin
                  wasnot := False ;
                  if not compiledList.IsEmpty then begin
                    // eliminate any redundant unary operator
                    oelm1 := compiledList.Last ;
                    if oelm1.ItemType = T_SqlItemType.itOperator then begin
                      {$IFDEF DCC}
                        if oelm1.Value = T_SqlOperator.opNOT
                      {$ELSE}
                        if VarEqual( oelm1.Value, Ord_SqlOperator( T_SqlOperator.opNOT ) )
                      {$ENDIF}
                      then
                        wasnot := True ;
                    end ;
                  end ;
                  if wasnot then compiledList.DeleteLast
                            else compiledList.AddOperator( T_SqlOperator.opNOT  ) ;

                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'FALSE'   ) = 0 then begin
                  compiledList.Add( makeValue( False ) ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'TRUE'    ) = 0 then begin
                  compiledList.Add( makeValue( True ) ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'LIKE'    ) = 0 then begin
                  compiledList.AddOperator( T_SqlOperator.opLIKE ) ;
                  wasLike := True ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'BETWEEN' ) = 0 then begin
                  compiledList.AddOperator( T_SqlOperator.opBETWEEN ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'IN'      ) = 0 then begin
                  compiledList.AddOperator( T_SqlOperator.opIN ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'IS'      ) = 0 then begin
                  compiledList.AddOperator( T_SqlOperator.opISTmp ) ;
                  unaryMarker := True ;
                end
        else if CompareText( tmp, 'NULL'    ) = 0 then begin
                  // test for IS NO
                  belm2 := False ;
                  belm1 := False ;
                  if      compiledList.Count >= 2 then begin
                            oelm2 := compiledList[ compiledList.Count-2] ;
                            belm2 := True ;
                            oelm1 := compiledList[ compiledList.Count-1] ;
                            belm1 := True ;
                          end
                  else if compiledList.Count >= 1 then begin
                            oelm1 := compiledList[ compiledList.Count-1] ;
                            belm1 := True
                          end ;

                  if      belm2                                             and
                          ( oelm2.ItemType = T_SqlItemType.itOperator )     and
                          VarEqual(
                            oelm2.Value,
                            Ord_SqlOperator( T_SqlOperator.opISTmp )
                          )                                                 and

                          belm1                                             and
                          ( oelm1.ItemType = T_SqlItemType.itOperator )     and
                          VarEqual(
                            oelm1.Value,
                            Ord_SqlOperator( T_SqlOperator.opNOT )
                          ) then
                          begin // IS NOT
                            compiledList.DeleteLast ;
                            compiledList.DeleteLast ;
                            compiledList.AddOperator( T_SqlOperator.opISNOTNULL ) ;
                          end
                  else if belm1                                              and
                          ( oelm1.ItemType = T_SqlItemType.itOperator )      and
                          VarEqual(
                            oelm1.Value,
                            Ord_SqlOperator( T_SqlOperator.opISTmp )
                          ) then
                          begin // IS
                            compiledList.DeleteLast ;
                            compiledList.AddOperator( T_SqlOperator.opISNULL ) ;
                          end
                  else    compiledList.Add( makeValue( NullVar ) ) ;
                  unaryMarker := True ;
                end
        else if functionList.IsFunction( tmp ) then begin
                  wasFunction := tmp ;
                  unaryMarker := True ;
                end
        else if IsStringEmpty( wasStat ) and isStatistical( tmp ) then begin
                  wasStat := tmp ;
                  unaryMarker := True ;
                end
        else    begin
                  if IsStringEmpty( wasStat ) then begin
                    compiledList.Add( makeName( tmp ) ) ;
                  end
                  else begin
                    compiledList.Add( makeStat( wasStat, tmp ) ) ;
                    unaryMarker := True ;
                  end;

                  wasStat := '' ;
                end;
      end ;

    finally
      FreeObject( str ) ;
    end ;
  end ;

  procedure T_SqlParser.parseMacro ;
  var
    str    : TStringBuilder    ;
    tmp    : String            ;
    c      : Char              ;
    param  : TGIS_VariantArray ;

    year        : Integer ;
    month       : Integer ;
    day         : Integer ;
    hour        : Integer ;
    minutes     : Integer ;
    seconds     : Integer ;
    miliseconds : Integer ;

    function get_today : TDateTime;
    begin
      {$IFDEF CLR}
        Result :=  System.DateTime.Now.Date ;
      {$ELSE}
        Result := Date ;
      {$ENDIF}
    end ;

    function get_now: TDateTime;
    begin
      {$IFDEF CLR}
        Result :=  System.DateTime.Now ;
      {$ELSE}
        Result := Now ;
      {$ENDIF}
    end ;

  begin
    str := TStringBuilder.Create ;
    try

      getChar ;
      {$IFDEF JAVA}
        str.setLength(0) ;
      {$ELSE}
        str.Length := 0 ;
      {$ENDIF}
      while not eofChar do
      begin
        c := getChar ;
        str.Append( c ) ;
        if ( not eofChar ) and ( ( peekChar = '(' ) or ( peekChar = ' ' ) ) then
        begin
          break ;
        end ;
      end ;

      tmp := str.ToString ;

      if tmp = 'date' then begin
        SetLength( param, 0 ) ;

        if not eofChar then
          parseMacroParams( param ) ;

        if length( param ) < 1 then
          Abort ;

        year        := 0 ;
        month       := 1 ;
        day         := 1 ;

        if length( param ) > 0 then
           year        := VarToInt32( param[0] ) ;
        if length( param ) > 1 then
           month       := VarToInt32( param[1] ) ;
        if length( param ) > 2 then
           day         := VarToInt32( param[2] ) ;
        if length( param ) > 3 then
          Abort ;

        compiledList.Add(
                       makeValue(
                         EncodeDate(
                           year        ,
                           month       ,
                           day
                         )
                       )
                     ) ;
      end
      else if tmp = 'time' then begin
        SetLength( param, 0 ) ;

        if not eofChar then
          parseMacroParams( param ) ;

        if length( param ) < 1 then
          Abort ;

        hour        := 0 ;
        minutes     := 0 ;
        seconds     := 0 ;
        miliseconds := 0 ;

        if length( param ) > 0 then
           hour        := VarToInt32( param[0] ) ;
        if length( param ) > 1 then
           minutes     := VarToInt32( param[1] ) ;
        if length( param ) > 2 then
           seconds     := VarToInt32( param[2] ) ;
        if length( param ) > 3 then
           miliseconds := VarToInt32( param[3] ) ;
        if length( param ) > 4 then
          Abort ;

        compiledList.Add(
                       makeValue(
                         EncodeTime(
                           hour        ,
                           minutes     ,
                           seconds     ,
                           miliseconds
                         )
                       )
                     ) ;
      end
      else if tmp = 'datetime' then begin
        SetLength( param, 0 ) ;

        if not eofChar then
          parseMacroParams( param ) ;

        if length( param ) < 1 then
          Abort ;

        year        := 0 ;
        month       := 1 ;
        day         := 1 ;
        hour        := 0 ;
        minutes     := 0 ;
        seconds     := 0 ;
        miliseconds := 0 ;

        if length( param ) > 0 then
           year        := VarToInt32( param[0] ) ;
        if length( param ) > 1 then
           month       := VarToInt32( param[1] ) ;
        if length( param ) > 2 then
           day         := VarToInt32( param[2] ) ;
        if length( param ) > 3 then
           hour        := VarToInt32( param[3] ) ;
        if length( param ) > 4 then
           minutes     := VarToInt32( param[4] ) ;
        if length( param ) > 5 then
           seconds     := VarToInt32( param[5] ) ;
        if length( param ) > 6 then
           miliseconds := VarToInt32( param[6] ) ;
        if length( param ) > 7 then
          Abort ;

        compiledList.Add(
                       makeValue(
                         EncodeDateTime(
                           year        ,
                           month       ,
                           day         ,
                           hour        ,
                           minutes     ,
                           seconds     ,
                           miliseconds
                         )
                       )
                     ) ;
      end
      else if tmp = 'now' then begin
        compiledList.Add(
                      makeValue(
                        get_now
                      )
                    ) ;
      end
      else if tmp = 'today' then begin
        compiledList.Add(
                       makeValue(
                         get_today
                       )
                     ) ;
      end
      else Abort ;

    finally
      FreeObject( str ) ;
    end ;
  end ;

  procedure T_SqlParser.parseMacroParams(
    var _ar : TGIS_VariantArray
  )  ;
  var
    str : TStringBuilder ;
    tmp : String         ;
    c   : Char           ;
    tkn : TGIS_Tokenizer ;
    i   : Integer        ;
  begin
    str := TStringBuilder.Create ;
    try

      c := getChar ;

      if c <> '(' then
        Abort ;

      {$IFDEF JAVA}
        str.setLength(0) ;
      {$ELSE}
        str.Length := 0 ;
      {$ENDIF}
      while not eofChar do
      begin
        c := getChar ;
        str.Append( c ) ;
        if ( not eofChar ) and ( peekChar = ')' ) then
        begin
          getChar ;
          break ;
        end ;
      end ;

      tmp := str.ToString ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( tmp, ',', '"' );
        SetLength( _ar, tkn.Result.Count ) ;

        for i:=0 to tkn.Result.Count-1 do begin
          _ar[i] := Trim( tkn.Result[i] ) ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;

    finally
      FreeObject( str ) ;
    end ;
  end ;

  procedure T_SqlParser.parseNumber ;
  var
    str    : TStringBuilder ;
    tmp    : String         ;
    bfloat : Boolean        ;
    c      : Char           ;
    state  : Integer        ;
    elm    : T_SqlItem      ;
  begin
    str := TStringBuilder.Create ;
    try

      unaryMarker := False ;

      bfloat := false ;
      {$IFDEF JAVA}
        str.setLength(0) ;
      {$ELSE}
        str.Length := 0 ;
      {$ENDIF}
      try
        state := 0 ;
        while not eofChar do begin
          c := peekChar ;

          case state of
            0 :  begin
                   if ( c >= '0' ) and  ( c <= '9' ) then begin
                     // OK
                   end
                   else
                   if ( c = '.' ) {or ( c = ',' )} then begin
                     // OK
                     bfloat := True ;
                     state := 1 ;
                   end
                   else
                   if ( c = 'E' ) or ( c = 'e' ) then begin
                     // OK
                     bfloat := True ;
                     state := 2 ;
                   end
                   else
                     break ;
                 end ;
            1 :  begin
                   if ( c >= '0' ) and  ( c <= '9' ) then begin
                     // OK
                   end
                   else
                   if ( c = 'E' ) or ( c = 'e' ) then begin
                     bfloat := True ;
                     state := 2 ;
                   end
                   else
                     break ;
                 end;
            2 :  begin
                   if ( c = '+' ) or
                      ( c = '-' )
                      or
                      ( ( c >= '0' ) and  ( c <= '9' ) )
                   then begin
                     state := 3 ;
                   end
                   else
                     break ;
                 end;
            3 :  begin
                   if ( c >= '0' ) and  ( c <= '9' ) then begin
                     // OK
                   end
                   else
                     break ;
                 end;
            else begin
                   assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) );
                 end ;
          end;

          str.Append( getChar ) ;
        end ;

      finally
        tmp := str.ToString ;

            if not compiledList.IsEmpty then begin
              elm := compiledList.Last ;
              if elm.ItemType = T_SqlItemType.itOperator then
              begin
                {$IFDEF DCC}
                  if elm.Value = T_SqlOperator.opUADD
                {$ELSE}
                  if VarEqual(
                       elm.Value,
                       Ord_SqlOperator( T_SqlOperator.opUADD )
                     )
                {$ENDIF}
                  then begin
                    compiledList.DeleteLast ;
                  end
                  else if
                   {$IFDEF DCC}
                      elm.Value = T_SqlOperator.opUSUB
                   {$ELSE}
                      VarEqual(
                        elm.Value,
                        Ord_SqlOperator( T_SqlOperator.opUSUB )
                      )
                   {$ENDIF}
                   then begin
                     compiledList.DeleteLast ;
                     tmp := '-' + tmp ;
                   end ;
                end
            end ;

        if bfloat then
          compiledList.Add( makeValue( DotStrToFloat( tmp ) ) )
        else
          compiledList.Add( makeValue( StrToInt64( tmp ) ) ) ;
      end ;

    finally
      FreeObject( str ) ;
    end ;
  end ;

  procedure T_SqlParser.parseLiteral ;
  var
    c      : Char           ;
    str    : TStringBuilder ;
    tmp    : String         ;
    state  : Integer        ;
  begin
    str := TStringBuilder.Create ;
    try

      unaryMarker := False ;

      getChar ;
      {$IFDEF JAVA}
        str.setLength(0) ;
      {$ELSE}
        str.Length := 0 ;
      {$ENDIF}
      state := 0 ;
      while not eofChar do begin
        c := peekChar ;

        case state of
          0 :  if c = '''' then begin
                 state := 1 ;
                 getChar ;
               end
               else begin
                 str.Append( c ) ;
                 getChar ;
               end ;
          1 :  if c = '''' then begin
                 str.Append( c ) ;
                 getChar ;
                 state := 0 ;
               end
               else begin
                 break ;
               end
          else begin
                 assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) );
               end ;
        end ;
      end ;
      if state <> 1  then
        Abort ;

      tmp := str.ToString ;
      if wasLike then
        compiledList.Add( makeLike ( tmp ) )
      else
        compiledList.Add( makeValue( tmp ) ) ;

    finally
      FreeObject( str ) ;
    end ;
  end ;

  procedure T_SqlParser.parseOperator ;
  var
    str    : TStringBuilder ;
    tmp    : String         ;
    elm    : T_SqlItem      ;
    wassub : Boolean        ;
    c      : Char           ;
  begin
    str := TStringBuilder.Create ;
    try

      try
        c := getChar ;
        str.Append( c ) ;
        if not eofChar then begin
          if ( ord( c ) < 128 ) and InCharSet( c, ['<', '>'] ) then
          begin
            c := peekChar ;
            if InCharSet( c, [ '=', '>' ] ) then begin
              str.Append( getChar ) ;
            end ;
          end ;
        end ;
      finally
         tmp := str.ToString ;

         // if field name is the same as a function name
         if not IsStringEmpty( wasFunction ) then begin
            compiledList.Add( makeName( wasFunction ) ) ;
            wasFunction := '' ;
          end ;

         if not IsStringEmpty( wasStat ) then begin
            compiledList.Add( makeName( wasStat ) ) ;
            wasStat := '' ;
          end ;

        if      tmp = '='  then compiledList.AddOperator( T_SqlOperator.opEQ  )
        else if tmp = '<>' then compiledList.AddOperator( T_SqlOperator.opNE  )
        else if tmp = '<'  then compiledList.AddOperator( T_SqlOperator.opLT  )
        else if tmp = '<=' then compiledList.AddOperator( T_SqlOperator.opLE  )
        else if tmp = '>'  then compiledList.AddOperator( T_SqlOperator.opGT  )
        else if tmp = '>=' then compiledList.AddOperator( T_SqlOperator.opGE  )
        else if tmp = '+'  then begin
          if unaryMarker then begin
            // nothing - '+' is not significant
          end
          else
            compiledList.AddOperator( T_SqlOperator.opADD ) ;
        end
        else if tmp = '-'  then begin
          wassub := False ;
          if unaryMarker then begin
            // eliminate any rendunant operator
            if not compiledList.IsEmpty then begin
              elm := compiledList.Last ;
              if elm.ItemType = T_SqlItemType.itOperator then
              begin
                {$IFDEF DLCC}
                  if elm.Value = T_SqlOperator.opUADD
                {$ELSE}
                  if VarEqual(
                       elm.Value,
                       Ord_SqlOperator( T_SqlOperator.opUADD )
                     )
                {$ENDIF}
                  then begin
                    compiledList.DeleteLast ;
                  end
                  else if
                   {$IFDEF DCC}
                      elm.Value = T_SqlOperator.opUSUB
                   {$ELSE}
                      VarEqual(
                        elm.Value,
                        Ord_SqlOperator( T_SqlOperator.opUSUB )
                      )
                   {$ENDIF}
                   then begin
                     wassub := True  ;
                     compiledList.DeleteLast ;
                   end ;
                end
            end ;
            if not wassub then
              compiledList.AddOperator( T_SqlOperator.opUSUB ) ;
          end
          else
            compiledList.AddOperator( T_SqlOperator.opSUB ) ;
        end
        else if tmp = '*'  then compiledList.AddOperator( T_SqlOperator.opMUL )
        else if tmp = '/'  then compiledList.AddOperator( T_SqlOperator.opDIV )
        else if tmp = '%'  then compiledList.AddOperator( T_SqlOperator.opMOD )
        else    Abort ;

        unaryMarker := True ;
      end

    finally
      FreeObject( str ) ;
    end ;
  end ;

  procedure T_SqlParser.parseParenthesis ;
  var
    c    : Char ;
  begin
    c := getChar ;
    if      c = '('  then begin
      // make function here (case when single field name is function name)
      if not IsStringEmpty( wasFunction ) then begin
        compiledList.Add( makeFunction( wasFunction ) ) ;
        wasFunction := '' ;
      end ;
      compiledList.AddOperator( T_SqlOperator.opOPN ) ;
      unaryMarker := True ;
    end
    else if c = ')'  then begin
      compiledList.AddOperator( T_SqlOperator.opCLS ) ;
      unaryMarker := False ;
    end
    else begin
      assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
      Abort ;
    end ;
  end ;

  procedure T_SqlParser.doUnary(
    const _op : T_SqlOperator
  ) ;
  begin
    with itemStack do begin

      case _op of
        T_SqlOperator.opUSUB : PushValue( - VarToDouble( resolve( Pop ) ) ) ;
        else begin
          assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          Abort ;
        end
      end ;
    end ;
  end ;

  procedure T_SqlParser.doArithmetic(
    const _op : T_SqlOperator
  ) ;
  var
    left  : Variant ;
    right : Variant ;
    otmp  : T_SqlItem ;
    btmp  : Boolean ;
  begin
    with itemStack do begin

      right := resolve( Pop ) ;

      if IsEmpty then begin
        btmp := False ;
      end
      else begin
        otmp := Top  ;
        btmp := True ;
      end ;

      if btmp then begin
        if otmp.ItemType  in [T_SqlItemType.itName, T_SqlItemType.itStat, T_SqlItemType.itValue] then
          left := resolve( Pop )
        else
          left := 0 ;
      end
      else
        left := 0 ;

      case _op of
        T_SqlOperator.opADD : if IsVariantString( left ) then
          // string concatenation
          PushValue( VarToString( left ) + VarToString( right ) )
        else
          PushValue( VarToDouble( left ) + VarToDouble( right ) ) ;
        T_SqlOperator.opSUB : PushValue( VarToDouble( left ) - VarToDouble( right ) ) ;
        T_SqlOperator.opMUL : PushValue( VarToDouble( left ) * VarToDouble( right ) ) ;
        T_SqlOperator.opDIV : PushValue( VarToDouble( left ) / VarToDouble( right ) ) ;
        T_SqlOperator.opMOD : PushValue( VarToInt32( left ) mod VarToInt32( right ) )
        else    begin
                   assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                   Abort ;
                 end
      end ;
    end ;
  end ;

  procedure T_SqlParser.doCompare(
    const _op : T_SqlOperator
  ) ;
  var
    left   : Variant  ;
    right  : Variant  ;
    sl, sr : Double   ;
    vt_l   : TVarType ;
    vt_r   : TVarType ;

    function same_float( const _a, _b : Double ) : Boolean ;
    var
      eps : Double ;
    const
      RES = 1E-15 * 1000 ;
    begin

      eps := {$IFDEF DCC}System.{$ENDIF}Math.Max(
               {$IFDEF DCC}System.{$ENDIF}Math.Min( Abs(_a), Abs(_b) ) * RES, RES
             ) ;
      if _a > _b then Result := (_a - _b) <= eps
                 else Result := (_b - _a) <= eps ;
      end ;

    function compare_float( const _a, _b : Double ) : Integer ;
    begin
      if      same_float( _a, _b ) then Result :=  0
      else if _a < _b              then Result := -1
      else if _a > _b              then Result :=  1
      else                              Result := -2  ; // unexpected value
    end ;

  begin
    with itemStack do begin
      right := resolve( Pop ) ;
      left  := resolve( Pop ) ;

      vt_l := VarType( left  ) ;
      vt_r := VarType( right ) ;
      if ( vt_l = varNull ) or ( vt_r = varNull )
      then begin
        case _op of
          T_SqlOperator.opEQ : PushValue( vt_l  = vt_r ) ;
          T_SqlOperator.opNE : PushValue( vt_l <> vt_r ) ;
          T_SqlOperator.opLT : PushValue( False        ) ;
          T_SqlOperator.opLE : PushValue( False        ) ;
          T_SqlOperator.opGT : PushValue( False        ) ;
          T_SqlOperator.opGE : PushValue( False        ) ;
          else   begin
                   assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                   Abort ;
                 end ;
        end ;

      end
      else begin
        if vt_l <> vt_r then begin
          if IsVariantString( right ) then begin
            try
              if not IsStringEmpty( String(right) ) then
                right := VarAsType( right, vt_l )
              else begin
                case vt_l of
                  {$IFNDEF GIS_NOPOINTERS}
                    varOleStr,
                    varStrArg,
                  {$ENDIF}
                  {$IFDEF DCC}
                    varUString,
                  {$ENDIF}
                  varString:
                    right := VarAsType( right, vt_l )
                  else begin
                    PushValue( False ) ;
                    exit ;
                  end ;
                end ;
              end ;
            except
              PushValue( False ) ;
              exit ;
            end ;
          end
          else if IsVariantString( left ) then begin
            try
              if not IsStringEmpty( String(left) ) then
                left := VarAsType( left, vt_l )
              else begin
                case vt_l of
                  {$IFNDEF GIS_NOPOINTERS}
                    varOleStr,
                    varStrArg,
                  {$ENDIF}
                  {$IFDEF DCC}
                    varUString,
                  {$ENDIF}
                  varString:
                    left := VarAsType( left, vt_l )
                  else begin
                    PushValue( False ) ;
                    exit ;
                  end ;
                end ;
              end ;
            except
              PushValue( False ) ;
              exit ;
            end ;
          end
        end ;

        case VarTypeEx( left ) of
          varExInt,
          varExUInt,
          varExInt64,
          varExUInt64,
          varExFixed,
          varExDateTime,
          varExBoolean :
        {$IFNDEF OXYGENE}
          case _op of
            T_SqlOperator.opEQ : PushValue( left =  right ) ;
            T_SqlOperator.opNE : PushValue( left <> right ) ;
            T_SqlOperator.opLT : PushValue( left <  right ) ;
            T_SqlOperator.opLE : PushValue( left <= right ) ;
            T_SqlOperator.opGT : PushValue( left >  right ) ;
            T_SqlOperator.opGE : PushValue( left >= right ) ;
            else   begin
              assert( False, GIS_RS_ERR_UNTESTED ) ;
              Abort ;
            end ;
          end ;
          {$ELSE}
            case _op of
              T_SqlOperator.opEQ : PushValue(   VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrEqual
                                            ) ;
              T_SqlOperator.opNE : PushValue(   VarCompareRel( left, right )
                                                <>
                                                TVariantRelationship.vrEqual
                                            ) ;
              T_SqlOperator.opLT : PushValue(   VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrLessThan
                                            ) ;
              T_SqlOperator.opLE : PushValue( (
                                                VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrLessThan
                                              )
                                              or
                                              (
                                                VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrEqual
                                              )
                                            ) ;
              T_SqlOperator.opGT : PushValue(   VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrGreaterThan
                                            ) ;
              T_SqlOperator.opGE : PushValue( (
                                                VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrGreaterThan
                                              )
                                              or
                                              (
                                                VarCompareRel( left, right )
                                                =
                                                TVariantRelationship.vrEqual
                                              )
                                            ) ;
              else   begin
                       assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                       Abort ;
                     end ;
            end ;
          {$ENDIF}

          varExFloat :
            begin
              sl  := VarToDouble( left  ) ;
              sr  := VarToDouble( right ) ;
              case _op of
                T_SqlOperator.opEQ : PushValue( compare_float( sl, sr ) =  0 ) ;
                T_SqlOperator.opNE : PushValue( compare_float( sl, sr ) <> 0 ) ;
                T_SqlOperator.opLT : PushValue( compare_float( sl, sr ) <  0 ) ;
                T_SqlOperator.opLE : PushValue( compare_float( sl, sr ) <= 0 ) ;
                T_SqlOperator.opGT : PushValue( compare_float( sl, sr ) >  0 ) ;
                T_SqlOperator.opGE : PushValue( compare_float( sl, sr ) >= 0 ) ;
                else   begin
                         assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                         Abort ;
                       end ;
              end ;
            end ;

          varExAnsiString,
          varExWideString :
            case _op of
              T_SqlOperator.opEQ :
                {$IFDEF JAVA OR ISLAND}
                  PushValue( VarToString( left  ).CompareToIgnoreCase(
                               VarToString( right )
                             ) = 0
                           ) ;  { TODO -cReview : utility function }
                {$ELSE}
                  {$IFDEF CLR}
                    PushValue( System.String.Compare( VarToString( left  ),
                                                      VarToString( right ),
                                                      True
                                                    ) =  0
                             ) ;
                  {$ELSE}
                    PushValue( AnsiCompareText( left, right ) =  0 ) ;
                  {$ENDIF}
                {$ENDIF}
              T_SqlOperator.opNE :
                {$IFDEF JAVA OR ISLAND}
                  PushValue( VarToString( left  ).CompareToIgnoreCase(
                               VarToString( right )
                             ) <> 0
                           ) ;  { TODO -cReview : utility function }
                {$ELSE}
                  {$IFDEF CLR}
                    PushValue( System.String.Compare( VarToString( left  ),
                                                      VarToString( right ),
                                                      True
                                                    ) <> 0
                             ) ;
                  {$ELSE}
                    PushValue( AnsiCompareText( left, right ) <> 0 ) ;
                  {$ENDIF}
                {$ENDIF}
              T_SqlOperator.opLT :
                {$IFDEF JAVA OR ISLAND}
                  PushValue( VarToString( left  ).CompareToIgnoreCase(
                               VarToString( right )
                             ) <  0
                           ) ;  { TODO -cReview : utility function }
                {$ELSE}
                  {$IFDEF CLR}
                    PushValue( System.String.Compare( VarToString( left  ),
                                                      VarToString( right ),
                                                      True
                                                    ) <  0
                             ) ;
                  {$ELSE}
                    PushValue( AnsiCompareText( left, right ) <  0 ) ;
                  {$ENDIF}
                {$ENDIF}
              T_SqlOperator.opLE :
                {$IFDEF JAVA OR ISLAND}
                  PushValue( VarToString( left  ).CompareToIgnoreCase(
                               VarToString( right )
                             ) <= 0
                           ) ;  { TODO -cReview : utility function }
                {$ELSE}
                  {$IFDEF CLR}
                    PushValue( System.String.Compare( VarToString( left  ),
                                                      VarToString( right ),
                                                      True
                                                    ) <= 0
                             ) ;
                  {$ELSE}
                    PushValue( AnsiCompareText( left, right ) <= 0 ) ;
                  {$ENDIF}
                {$ENDIF}
              T_SqlOperator.opGT :
                {$IFDEF JAVA OR ISLAND}
                  PushValue( VarToString( left  ).CompareToIgnoreCase(
                               VarToString( right )
                             ) >  0
                           ) ; { TODO -cReview : utility function }
                {$ELSE}
                  {$IFDEF CLR}
                    PushValue( System.String.Compare( VarToString( left  ),
                                                      VarToString( right ),
                                                      True
                                                    ) >  0
                             ) ;
                  {$ELSE}
                    PushValue( AnsiCompareText( left, right ) >  0 ) ;
                  {$ENDIF}
                {$ENDIF}
              T_SqlOperator.opGE :
                {$IFDEF JAVA OR ISLAND}
                  PushValue( VarToString( left  ).CompareToIgnoreCase(
                               VarToString( right )
                             ) >= 0
                           ) ; { TODO -cReview : utility function }
                {$ELSE}
                  {$IFDEF CLR}
                    PushValue( System.String.Compare( VarToString( left  ),
                                                      VarToString( right ),
                                                      True
                                                    ) >= 0
                             ) ;
                  {$ELSE}
                    PushValue( AnsiCompareText( left, right ) >= 0 ) ;
                  {$ENDIF}
                {$ENDIF}
              else
                begin
                  assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                  Abort ;
                end ;
            end ;
          else
            Abort ;
        end ;
      end ;
    end ;
  end ;

  procedure T_SqlParser.doLogical(
    const _op : T_SqlOperator
  ) ;
  var
    left  : Variant ;
    right : Variant ;
    base  : Variant ;
    {$IFDEF OXYGENE}
      lvr : TVariantRelationship ;
      rvr : TVariantRelationship ;
    {$ENDIF}
  begin
    with itemStack do begin
      right := resolve( Pop ) ;
      left  := resolve( Pop ) ;

      case _op of
        T_SqlOperator.opAND: begin
                 if operatorStack.Top <> T_SqlOperator.opBETWEEN then
                   // plain logical
                   PushValue( VarToBoolean( left ) and VarToBoolean( right ) )
                 else begin // AND belongs to BETWEEN
                   operatorStack.Pop ;
                   base := resolve( Pop ) ;

                   try
                     if IsVariantString( left ) then
                        left  := VarAsType( left,  VarType( base ) ) ;
                     if IsVariantString( right ) then
                        right := VarAsType( right, VarType( base ) ) ;
                   except
                     PushValue( False ) ;
                     exit ;
                   end ;

                   {$IFNDEF OXYGENE}
                     PushValue( ( left <= base ) and ( base <= right ) ) ;
                   {$ELSE}
                     lvr := VarCompareRel( left, base  ) ;
                     rvr := VarCompareRel( base, right ) ;
                     PushValue( (
                                  ( lvr = TVariantRelationship.vrLessThan )
                                  or
                                  ( lvr = TVariantRelationship.vrEqual )
                                )
                                and
                                (
                                   ( rvr = TVariantRelationship.vrLessThan )
                                   or
                                   ( rvr = TVariantRelationship.vrEqual )
                                )
                              ) ;
                   {$ENDIF}
                 end ;
               end ;
        T_SqlOperator.opOR : PushValue( VarToBoolean( left ) or VarToBoolean( right ) ) ;
        else   begin
                 assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                 Abort ;
               end ;
      end ;
    end ;
  end ;

  procedure T_SqlParser.doLogicalNot(
    const _op : T_SqlOperator
  ) ;
  var
    right : Variant ;
  begin
    with itemStack do begin
      right := resolve( Pop ) ;

      case _op of
        T_SqlOperator.opNOT: PushValue( not VarToBoolean( right ) ) ;
        else   begin
                 assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                 Abort ;
               end ;
      end ;
    end ;
  end ;

  procedure T_SqlParser.doNullable(
    const _op : T_SqlOperator
  ) ;
  var
    right : Variant ;
  begin
    with itemStack do begin
      right := resolve( Pop ) ;

      case _op of
        T_SqlOperator.opISNULL    : PushValue(     VarIsNull( right ) ) ;
        T_SqlOperator.opISNOTNULL : PushValue( not VarIsNull( right ) ) ;
        else          begin
                        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                        Abort ;
                      end ;
      end ;
    end ;
  end ;

  procedure T_SqlParser.doLike(
    const _op : T_SqlOperator
  ) ;
  var
    left  : Variant      ;
    right : T_SqlItem    ;
    obj   : T_LikeParser ;
  begin
    with itemStack do begin
      right := Pop ;
      left  := resolve( Pop );
      if right.ItemType = T_SqlItemType.itLike then
        obj := likeList[ Integer( right.Value ) ]
      else begin
        obj := nil ;
        Abort ;
      end ;

      case _op of
        T_SqlOperator.opLIKE : PushValue( obj.Parse( left ) ) ;
        else     begin
                   assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                   Abort ;
                 end ;
      end ;
    end ;
  end ;

  procedure T_SqlParser.doFunction(
    const _op : T_SqlOperator
  ) ;
  var
    itm   : T_SqlItem    ;
    fnc   : T_FunctionParser ;
    p     : Integer ;
  begin
    with itemStack do begin
      itm := Pop ;
      for p := 0 to MAX_ARGS-1 do
        fncArgs[p] := Unassigned ;

      p := 0 ;
      if itm.ItemType <> T_SqlItemType.itFunction then begin
        // fnc, arg1,..,argN
        fncArgs[p] := resolve( itm ) ;
        inc( p ) ;

        itm := Pop ;
        // collect all args for function
        while not IsEmpty and ( itm.ItemType <> T_SqlItemType.itFunction ) do begin
          if p < MAX_ARGS then
            fncArgs[p] := resolve( itm ) ;
          inc( p ) ;
          itm := Pop ;
        end ;
      end ;

      if itm.ItemType = T_SqlItemType.itFunction then
        fnc := functionList[ Integer( itm.Value ) ]
      else begin
        fnc := nil ;
        Abort ;
      end ;

      case _op of
        T_SqlOperator.opFUNCTION : PushValue( fnc.Calculate( fncArgs, p ) ) ;
        else     begin
                   assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                   Abort ;
                 end ;
      end ;
    end ;
  end ;

  procedure T_SqlParser.doIn ;
  var
    tmp    : T_SqlItem    ;
    left   : Variant      ;
    right  : Variant      ;
    res    : Boolean      ;
  begin
    inStack.Clear ;

    with itemStack do begin
      while not itemStack.IsEmpty do begin
        tmp := Pop ;
        if tmp.ItemType  = T_SqlItemType.itOperator then break ;

        inStack.Push( tmp ) ;
      end ;
      left  := resolve( Pop ) ;

      res := False ;
      while not inStack.IsEmpty do begin
        try
          right := resolve( inStack.Pop ) ;

          if      IsVariantString( right ) then
                  right := VarAsType( right, VarType( left ) )
          else if IsVariantString( left  ) then
                  left :=  VarAsType( left,  VarType( right ) ) ;

          if VarEqual( left, right ) then begin
            res := True ;
            break ;
          end ;

        except
          PushValue( False ) ;
          exit ;
        end ;
      end ;

      PushValue( res ) ;
    end ;
  end ;

  function T_SqlParser.resolve(
    const _val : T_SqlItem
  ) : Variant ;
  begin
    if _val.ItemType = T_SqlItemType.itName then
      Result := fieldList[ Integer( _val.Value ) ].Value
    else
    if _val.ItemType = T_SqlItemType.itStat then
      Result := statList[ Integer( _val.Value ) ].Value
    else
      Result := _val.Value ;
  end ;

  procedure T_SqlParser.storeItem(
    const _item : T_SqlItem
  ) ;
  begin
    itemStack.Push( _item ) ;
  end ;

  procedure T_SqlParser.doOperator(
    const _item : T_SqlOperator
  ) ;
  var
   op : T_SqlOperator ;
  begin

    with operatorStack do begin
      if ( _item = T_SqlOperator.opBEG ) or ( _item = T_SqlOperator.opOPN ) then
      begin
        // open parenthesis and initialization must always be pushed
        Push( _item ) ;
        exit ;
      end ;
      if ( Ord_SqlOperator( Top ) <  Ord_SqlOperator( _item ) ) and
         ( _item                  <> T_SqlOperator.opEND      ) then
      begin
        // push an operator more important than the last one
        Push( _item ) ;
        if _item = T_SqlOperator.opIN then
          itemStack.PushOperator( _item ) ;
        exit ;
      end ;

      // do calculations, until operator which less important than the current
      // one is found
      while ( Ord_SqlOperator( Top ) >= Ord_SqlOperator( _item ) ) or
            ( _item                   = T_SqlOperator.opEND      ) do
      begin
        op := Pop ;
        case op of
          T_SqlOperator.opBEG  : exit  ; // bottom of stack was reached

          T_SqlOperator.opOPN  : break ; // parenthesis open - no action simple pop out
          T_SqlOperator.opAND,
          T_SqlOperator.opOR   : doLogical( op ) ;

          T_SqlOperator.opNOT  : doLogicalNot( op ) ;

          T_SqlOperator.opISNULL,
          T_SqlOperator.opISNOTNULL
                 : doNullable( op ) ;

          T_SqlOperator.opEQ,
          T_SqlOperator.opNE,
          T_SqlOperator.opLT,
          T_SqlOperator.opLE,
          T_SqlOperator.opGT,
          T_SqlOperator.opGE   : doCompare( op ) ;

          T_SqlOperator.opLIKE : doLike( op ) ;

          T_SqlOperator.opIN   : doIn ;

          T_SqlOperator.opUADD,
          T_SqlOperator.opUSUB : doUnary( op ) ;
          T_SqlOperator.opADD,
          T_SqlOperator.opSUB,
          T_SqlOperator.opMUL,
          T_SqlOperator.opMOD,
          T_SqlOperator.opDIV  : doArithmetic( op ) ;
          T_SqlOperator.opFUNCTION  : doFunction( op ) ;
          else     Abort
        end ;
      end ;

      // pending operation was cleared from the stack, so now we can
      // push a current operator ( with the exception, of course, a closed
      // parenthesis because its role is over )
      if (_item <> T_SqlOperator.opCLS) and ( _item <> T_SqlOperator.opOPN )
      then
        Push( _item ) ;

      if ( _item = T_SqlOperator.opEND ) and
         ( not operatorStack.IsEmpty   ) then Abort ;

    end ;
  end ;

  function T_SqlParser.isStatistical(
    const _name : String
  ) : Boolean ;
  begin
    Result := ( CompareText( _name, 'COUNT'     ) = 0 ) or
              ( CompareText( _name, 'AVG'       ) = 0 ) or
              ( CompareText( _name, 'COUNTNULL' ) = 0 ) or
              ( CompareText( _name, 'MAX'       ) = 0 ) or
              ( CompareText( _name, 'MAJORITY'  ) = 0 ) or
              ( CompareText( _name, 'MEDIAN'    ) = 0 ) or
              ( CompareText( _name, 'MIN'       ) = 0 ) or
              ( CompareText( _name, 'MINORITY'  ) = 0 ) or
              ( CompareText( _name, 'RANGE'     ) = 0 ) or
              ( CompareText( _name, 'STDEV'     ) = 0 ) or
              ( CompareText( _name, 'SUM'       ) = 0 ) or
              ( CompareText( _name, 'VARIANCE'  ) = 0 ) or
              ( CompareText( _name, 'VARIETY'   ) = 0 ) ;
  end;

  function T_SqlParser.Prepare(
    const _text : String
  ) : Integer ;
  var
    c    : Char ;
    chs1 : TCharSet ;
    chs2 : TCharSet ;
    chs3 : TCharSet ;
    chs4 : TCharSet ;
    chs5 : TCharSet ;
    chs6 : TCharSet ;
    chs7 : TCharSet ;
  begin
    Result := -1 ;

    // be sure that stacks are empty
    // it can be something after the last unsuccessful parsing
    itemStack.Clear ;
    operatorStack.Clear ;
    compiledList.Clear ;
    fieldList.Clear ;
    statList.Clear ;
    likeList.Clear ;

    // initialize operation
    queryPos    := StringFirst ;
    queryText   := _text ;
    wasStat     := ''    ;
    wasFunction := ''    ;
    wasLike     := False ;
    unaryMarker := True  ;

    if IsStringEmpty( queryText ) then exit ;

    compiledList.AddOperator( T_SqlOperator.opBEG ) ;

    try
      // pre-recognize items by first char
      chs1 := PrepareCharSet( ['AZ', 'az', '"', '[' ] ) ;
      chs2 := PrepareCharSet( ['$'] ) ;
      chs3 := PrepareCharSet( ['09', '.'] ) ;
      chs4 := PrepareCharSet( [''''] ) ;
      chs5 := PrepareCharSet( ['<','>','=','+','-','*','/','%'] ) ;
      chs6 := PrepareCharSet( ['(',')'] ) ;
      chs7 := PrepareCharSet( [','] ) ;

      while not eofChar do
      begin
        c := peekChar ;
        if c = ' ' then begin
          getChar ;
          Continue ;
        end
        else if ord( c ) > 127            then parseName
        else if InCharSet( c, chs1 )      then parseName
        else if InCharSet( c, chs2 )      then parseMacro
        else if InCharSet( c, chs3 )      then parseNumber
        else if InCharSet( c, chs4 )      then begin
                                            parseLiteral ;
                                            wasLike := False ;
                                          end
        else if InCharSet( c, chs5 )      then parseOperator
        else if InCharSet( c, chs6 )      then parseParenthesis
        else if InCharSet( c, chs7 )      then begin
                                            // paramter separtor ','
                                            getChar ;
                                            unaryMarker := True ;
                                          end
        else begin
          Abort ;
        end ;
      end ;

      // if field name is the same as a function name (Renderer case)
      if not IsStringEmpty( wasFunction ) then begin
         compiledList.Add( makeName( wasFunction ) ) ;
         wasFunction := '' ;
      end ;
      if not IsStringEmpty( wasStat ) then begin
         compiledList.Add( makeName( wasStat ) ) ;
         wasStat := '' ;
      end ;
    except
      Result := queryPos ;
    end ;

  end ;

  procedure T_SqlParser.RePrepare ;
  var
    i : Integer ;
  begin
    for i:=0 to fieldList.Count - 1 do
      fieldList.Tags[i] := -1 ;
  end ;

  function T_SqlParser.Parse(
    const _shape          : TObject                   ;
    const _cursor         : Integer
  ) : Variant ;
  var
    i      : Integer   ;
    oitem  : T_SqlItem ;
    bitem  : Boolean   ;
    otmp   : T_SqlItem ;
    btmp   : Boolean   ;
    oval   : Variant   ;
  begin
    Result := Unassigned ;

    // be sure that stacks are empty
    // these can be something after the last unsuccessful parsing
    itemStack.Clear ;
    operatorStack.Clear ;

    // set a call-back - field-retrieving routine


    // compute fields
    for i:= 0 to fieldList.Count - 1 do begin
      oval := NullVar ;

      if assigned( getFieldFun ) then begin
        if assigned( _shape ) then
          oval := getFieldFun(
                    TGIS_Shape(_shape).Uid,
                    fieldList.Names[ i ]
                  )
        else
          oval := getFieldFun(
                    -1, fieldList.Names[ i ]
                  )
      end
      else begin
        if fieldList[ i ].Tag < 0 then
          if assigned( getBindFun ) then
            fieldList.Tags[ i ] := getBindFun( fieldList.Names[ i ], _cursor ) ;
        if assigned( getFieldBindFun ) then
          oval := getFieldBindFun( _shape, fieldList[ i ].Tag, _cursor ) ;
      end;

      // empty and null should be treated equally
      if VarIsEmpty( oval ) then
        exit;

      // exit if bind method reset field list
      if fieldList.Count=0 then exit ;
        fieldList.Values[ i ] := oval ;
    end ;

    // compute stats
    for i:= 0 to statList.Count - 1 do begin
      oval := NullVar ;

      if assigned( getStatFun ) then
        oval := getStatFun( statList.Fields[ i ], statList.Names[ i ] ) ;

      // empty and null should be treated equally
      if VarIsEmpty( oval ) then
        exit;

      // exit if bind method reset field list
      if statList.Count=0 then exit ;
        statList.Values[ i ] := oval ;
    end ;

    // compute expressions
    for i:=0 to compiledList.Count - 1 do begin
      oitem := compiledList.Items[i] ;
      if oitem.ItemType  = T_SqlItemType.itOperator then begin
        if operatorStack.Count = 1 then begin
          {$IFDEF DCC}
            if      oitem.Value = T_SqlOperator.opAND then begin
          {$ELSE}
            if      VarEqual( oitem.Value,
                              Ord_SqlOperator( T_SqlOperator.opAND )
                    ) then begin
          {$ENDIF}
                    btmp := not itemStack.IsEmpty ;
                    if btmp then
                      otmp := itemStack.Top ;
                    if btmp                                               and
                       ( otmp.ItemType          = T_SqlItemType.itValue ) and
                       ( VarType( otmp.Value )  = varBoolean            ) and
                       {$IFDEF DCC}
                         ( otmp.Value           = False                 )
                       {$ELSE}
                         VarEqual( otmp.Value, False )
                       {$ENDIF}
                    then begin
                      Result := False ;
                      exit ;
                    end ;
                  end
          {$IFDEF DCC}
            else if oitem.Value = T_SqlOperator.opOR then begin
          {$ELSE}
            else if VarEqual( oitem.Value,
                              Ord_SqlOperator( T_SqlOperator.opOR )
                            ) then begin
          {$ENDIF}
                    btmp := not itemStack.IsEmpty ;
                    if btmp then
                      otmp := itemStack.Top ;
                    if btmp                                               and
                       ( otmp.ItemType          = T_SqlItemType.itValue ) and
                       ( VarType( otmp.Value )  = varBoolean            ) and
                       {$IFDEF DCC}
                         ( otmp.Value           = True                  )
                       {$ELSE}
                         VarEqual( otmp.Value, True )
                       {$ENDIF}
                    then begin
                      Result := True ;
                      exit ;
                    end ;
            end ;
        end ;

        {$IFDEF DCC}
          doOperator( oitem.Value ) ;
        {$ELSE}
          doOperator( T_SqlOperator( Integer(oitem.Value) )  ) ;
        {$ENDIF}
      end
      else if oitem.ItemType  = T_SqlItemType.itFunction then begin
        doOperator( T_SqlOperator.opFUNCTION ) ;
        storeItem( oitem ) ;
      end
      else begin
        storeItem( oitem ) ;
      end ;
    end ;

    doOperator( T_SqlOperator.opEND ) ;

    bitem := not itemStack.IsEmpty ;
    if bitem then
       Result := resolve( itemStack.Top ) ;
  end ;

//=============================================================================
// TGIS_SqlQuery
//=============================================================================

  constructor TGIS_SqlQuery.Create ;
  begin
    inherited ;
    boundLayer := nil ;
    classPtr := T_SqlParser.Create ;
  end ;

  procedure TGIS_SqlQuery.doDestroy ;
  begin
    FreeObject( classPtr ) ;
    inherited ;
  end ;

  procedure TGIS_SqlQuery.BindSqlQueryCallBack(
    _bind_fun       : TGIS_SqlQueryBindFun      ;
    _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
    _stat_fun       : TGIS_SqlQueryStatFun
  ) ;
  begin
    T_SqlParser( classPtr ).getFieldFun     := nil             ;
    T_SqlParser( classPtr ).getBindFun      := _bind_fun       ;
    T_SqlParser( classPtr ).getFieldBindFun := _field_bind_fun ;
    T_SqlParser( classPtr ).getStatFun      := _stat_fun       ;
  end;

  function TGIS_SqlQuery.Prepare(
    const _query : String
  ) : Integer ;
  begin
    if queryStr <> _query then queryStr := _query ;
    try
      Result := T_SqlParser( classPtr ).Prepare( queryStr ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SQLQUERY ), queryStr, 0 ) ;
    end ;
  end ;

  procedure TGIS_SqlQuery.RePrepare ;
  begin
    T_SqlParser( classPtr ).RePrepare ;
  end ;

  function TGIS_SqlQuery.Parse(
    _field_fun      : TGIS_SqlQueryFieldFun     ;
    _bind_fun       : TGIS_SqlQueryBindFun      ;
    _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
    _shape          : TObject                   ;
    const _cursor   : Integer
  ) : Variant ;
  begin
    if assigned( _field_fun ) then
      T_SqlParser( classPtr ).getFieldFun := _field_fun ;

    Result := Parse( _shape, _cursor ) ;
  end ;

  function TGIS_SqlQuery.Parse(
    _shape          : TObject                   ;
    const _cursor   : Integer
  ) : Variant ;
  var
    ll : TGIS_Layer ;
  begin
    try
      if assigned( _shape ) then begin
        ll := TGIS_Shape( _shape ).Layer ;

        if assigned( ll ) and ( boundLayer <> ll ) then begin
          TGIS_Shape( _shape ).Layer.BindSqlQueryInternal( self ) ;
          boundLayer := TGIS_Shape( _shape ).Layer ;
        end;
      end ;

      Result := T_SqlParser( classPtr ).Parse( _shape,
                                               _cursor
                                             ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_SQLQUERY ), queryStr, 0 ) ;
    end ;
  end ;

  function TGIS_SqlQuery.ParseAsBoolean(
    _field_fun      : TGIS_SqlQueryFieldFun     ;
    _bind_fun       : TGIS_SqlQueryBindFun      ;
    _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
    _shape          : TObject                   ;
    const _default  : Boolean                   ;
    const _cursor   : Integer
  ) : Boolean ;
  begin
    if assigned( _field_fun ) then
      T_SqlParser( classPtr ).getFieldFun := _field_fun ;

    Result := ParseAsBoolean( _shape, _default, _cursor ) ;
  end ;

  function TGIS_SqlQuery.ParseAsBoolean(
    _shape          : TObject ;
    const _default  : Boolean ;
    const _cursor   : Integer
  ) : Boolean ;
  var
    ll  : TGIS_Layer ;
    val : Variant ;
  begin
    try
      if assigned( _shape ) then begin
        ll := TGIS_Shape( _shape ).Layer ;

        if assigned( ll ) and ( boundLayer <> ll ) then begin
          TGIS_Shape( _shape ).Layer.BindSqlQueryInternal( self ) ;
          boundLayer := TGIS_Shape( _shape ).Layer ;
        end;
      end ;

      val := T_SqlParser( classPtr ).Parse( _shape,
                                            _cursor
                                          ) ;

      if VarIsEmpty( val ) or VarIsNull( val ) then Result := _default
                                               else Result := VarToBoolean( val ) ;

    except
      Result := _default ;
    end ;
  end;

  function TGIS_SqlQuery.ParseAsFloat(
    _field_fun      : TGIS_SqlQueryFieldFun     ;
    _bind_fun       : TGIS_SqlQueryBindFun      ;
    _field_bind_fun : TGIS_SqlQueryFieldBindFun ;
    _shape          : TObject                   ;
    const _default  : Double                    ;
    const _cursor   : Integer
  ) : Double ;
  begin
    if assigned( _field_fun ) then
      T_SqlParser( classPtr ).getFieldFun := _field_fun ;

    Result := ParseAsFloat( _shape, _default, _cursor ) ;
  end ;

  function TGIS_SqlQuery.ParseAsFloat(
    _shape          : TObject                   ;
    const _default  : Double                    ;
    const _cursor   : Integer
  ) : Double ;
  var
    ll  : TGIS_Layer ;
    val : Variant ;
  begin
    try
      if assigned( _shape ) then begin
        ll := TGIS_Shape( _shape ).Layer ;

        if assigned( ll ) and ( boundLayer <> ll ) then begin
          TGIS_Shape( _shape ).Layer.BindSqlQueryInternal( self ) ;
          boundLayer := TGIS_Shape( _shape ).Layer ;
        end;
      end ;

      val := T_SqlParser( classPtr ).Parse( _shape,
                                            _cursor
                                          ) ;

      if VarIsNull( val ) then Result := _default
                          else Result := VarToDouble( val ) ;
    except
      Result := _default ;
    end ;
  end ;

{==================================== END =====================================}
end.
