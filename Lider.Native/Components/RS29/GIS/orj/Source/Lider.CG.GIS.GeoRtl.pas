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
  RTL interface implementation. The purpose of this unit is to mimic Delphi RTL
  interface on particular compiler/framework to maintain single source compilation.
}
unit GisRtl;
{$HPPEMIT '#pragma link "GisRtl"}

{$INCLUDE GisInclude.inc}

interface

uses
  System.SysUtils,
  System.Generics.Defaults,
  System.Classes,
  System.Types,
  System.Variants,
  GisBaseObject;

type
  /// <summary>
  ///   Base object for all TGIS_ classes. Mother of all classes.
  /// </summary>
  TGIS_Object = GisBaseObject.TGIS_Object ;

  /// <summary>
  ///   Base object for all TGIS_ classes. Mother of all classes.
  ///   Used when object must be disposed (like stream etc.)
  /// </summary>
  TGIS_ObjectDisposable = GisBaseObject.TGIS_ObjectDisposable ;

  /// <summary>
  ///   Base object for all TGIS_ classes. Mother of all classes.
  ///   Used as a base class for all layers.
  /// </summary>
  TGIS_BaseObjectDisposable = GisBaseObject.TGIS_BaseObjectDisposable ;

{$REGION 'Free handlers'}

/// <summary>
///   Free the object and set pointer to nil. Safe for unassigned objects.
/// </summary>
/// <param name="_obj">
///   object to be freed
/// </param>
procedure FreeObject          ( var _obj ) ;

/// <summary>
///   Release the interface and set pointer to nil.
/// </summary>
/// <param name="_obj">
///   interface to be released
/// </param>
procedure ReleaseInterface    ( var _obj ) ;


/// <summary>
///   Free the object. Safe for unassigned objects.
/// </summary>
/// <param name="_obj">
///   object to be freed
/// </param>
procedure FreeObjectNotNil    ( _obj : Pointer ) ;
{$ENDREGION}

{$REGION 'String handlers'}
/// <summary>
///   Indicates if the specified string is empty or consists only of white-space characters.
/// </summary>
/// <param name="_str">
///   string to test
/// </param>
/// <returns>
///   True, if string is empty contains only white-space characters.
/// </returns>
function IsStringEmptyOrWhiteSpace( const _str : String ) : Boolean ; inline ;

/// <summary>
///   Indicates if the specified string contains any white-space character.
/// </summary>
/// <param name="_str">
///   string to test
/// </param>
/// <returns>
///   True, if string contains any white-space character.
/// </returns>
function HasStringWhiteSpace( const _str : String ) : Boolean ; inline ;

/// <summary>
///   Indicates if the specified string is empty or nil.
/// </summary>
/// <param name="_str">
///   string to test
/// </param>
/// <returns>
///   True, if string is empty has not content.
/// </returns>
function IsStringEmpty( const _str : String ) : Boolean ; inline ;

/// <summary>
///   Allocate a string variable.
/// </summary>
/// <param name="_str">
///   variable to allocate
/// </param>
/// <param name="_len">
///   length
/// </param>
procedure SetLengthStr( var _str : String; _len : Integer ) ; inline ;

const
  /// <summary>
  ///   Index of first character in a string.
  /// </summary>
  StringFirst  = 1 ;

/// <summary>
///   Returns the last character index of the specified string.
/// </summary>
/// <param name="_str">
///   string to be tested
/// </param>
/// <returns>
///   Index of the last character in the string.
/// </returns>
/// <remarks>
///   <note type="note">
///    For VCL/.NET independent String handling.
///    </note>
/// </remarks>
function StringLast   ( const _str : String
                      ) : Integer ; inline ;
{$ENDREGION}

{$REGION 'String utilities'}
/// <summary>
///   Platform independent string replace method.
/// </summary>
/// <param name="_str">
///   base string
/// </param>
/// <param name="_subs">
///   string to be replaced
/// </param>
/// <param name="_repl">
///   string replacement
/// </param>
/// <returns>
///   String with all replacement done.
/// </returns>
function StringReplaceAll( const _str, _subs, _repl : String ) : String ;

/// <summary>
///   Compute hash value for provided string
/// </summary>
/// <param name="_str">
///   string for which hash will be computed
/// </param>
/// <returns>
///   Hash value.
/// </returns>
function StringHash( const _str : String ) : Int64 ;
{$ENDREGION}

{$REGION 'Convert(..)' }

  /// <summary>
  ///   Convert A String into ANSI TBytes representation.
  /// </summary>
  /// <param name="_str">
  ///   String
  /// </param>
  /// <returns>
  ///   String converted to TBytes.
  /// </returns>
  function  ConvertAnsiString   ( const _str         : String
                                ) : TBytes ; overload;

  /// <summary>
  ///   Convert array of bytes as ANSI String into String representation.
  /// </summary>
  /// <param name="_bytes">
  ///   TBytes buffer
  /// </param>
  /// <returns>
  ///   Converted string.
  /// </returns>
  function  ConvertAnsiString   ( const _bytes       : array of byte
                                ) : String ; overload;

  /// <summary>
  ///   Convert TBytes as ANSI String into String representation.
  /// </summary>
  /// <param name="_bytes">
  ///   TBytes buffer
  /// </param>
  /// <returns>
  ///   Converted string.
  /// </returns>
  function  ConvertAnsiString   ( const _bytes       : TBytes
                                ) : String ; overload;

  /// <summary>
  ///   Convert TBytes as ANSI String into String representation.
  /// </summary>
  /// <param name="_bytes">
  ///   TBytes buffer
  /// </param>
  /// <param name="_count">
  ///   number of bytes to be converted
  /// </param>
  /// <returns>
  ///   Converted string.
  /// </returns>
  function  ConvertAnsiString   ( const _bytes       : TBytes  ;
                                  const _count       : Integer
                                ) : String ; overload;

  /// <summary>
  ///   Convert TBytes as ANSI String into String representation with codepage
  ///   manipulation.
  /// </summary>
  /// <param name="_bytes">
  ///   TBytes buffer
  /// </param>
  /// <param name="_count">
  ///   number of bytes to be converted
  /// </param>
  /// <param name="_codepage">
  ///   Code Page of _bytes
  /// </param>
  /// <param name="_outcodepage">
  ///   Code Page of returning String (meaningless if String type is Unicode -
  ///   CLR platforms and Delphi 2009 and up)
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    it is rather slow function!
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Converted string.
  /// </returns>
  function  ConvertAnsiString   ( const _bytes       : TBytes  ;
                                  const _count       : Integer ;
                                  const _codepage    : Integer ;
                                  const _outcodepage : Integer
                                ) : String ; overload;

  /// <summary>
  ///   Convert TBytes as ANSI String into String representation with codepage
  ///   manipulation.
  /// </summary>
  /// <param name="_bytes">
  ///   TBytes buffer
  /// </param>
  /// <param name="_codepage">
  ///   Code Page of _bytes
  /// </param>
  /// <param name="_outcodepage">
  ///   Code Page of returning String (meaningless if String type is Unicode -
  ///   CLR platforms and Delphi 2009 and up)
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    it is rather slow function!
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Converted string.
  /// </returns>
  function  ConvertAnsiString   ( const _bytes       : TBytes  ;
                                  const _codepage    : Integer ;
                                  const _outcodepage : Integer
                                ) : String ; overload;

  /// <summary>
  ///   Convert TBytes as ANSI String into String representation with codepage
  ///   manipulation.
  /// </summary>
  /// <param name="_bytes">
  ///   TBytes buffer
  /// </param>
  /// <param name="_offset">
  ///   offset in buffer
  /// </param>
  /// <param name="_count">
  ///   number of bytes to be converted
  /// </param>
  /// <param name="_codepage">
  ///   Code Page of _bytes
  /// </param>
  /// <param name="_outcodepage">
  ///   Code Page of returning String (meaningless if String type is Unicode -
  ///   CLR platforms and Delphi 2009 and up)
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    it is rather slow function!
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Converted string.
  /// </returns>
  function  ConvertAnsiString   ( const _bytes       : TBytes  ;
                                  const _offset      : Integer ;
                                  const _count       : Integer ;
                                  const _codepage    : Integer ;
                                  const _outcodepage : Integer
                                ) : String ; overload;
  {$IFNDEF NEXTGEN}
    /// <summary>
    ///   Perform code page conversion.
    /// </summary>
    /// <param name="_source">
    ///   String to be converted
    /// </param>
    /// <param name="_sourceCP">
    ///   code page of source String (converted from)
    /// </param>
    /// <param name="_destCP">
    ///   code page of destination (converted to)
    /// </param>
    /// <returns>
    ///   Converted string.
    /// </returns>
    function  ConvertStr2StrCP  ( const _source      : AnsiString ;
                                  const _sourceCP    : Integer    ;
                                  const _destCP      : Integer
                                ) : AnsiString ;

    /// <summary>
    ///   Perform code page conversion.
    /// </summary>
    /// <param name="_source">
    ///   String to be converted
    /// </param>
    /// <param name="_sourceCP">
    ///   code page of source String (converted from)
    /// </param>
    /// <returns>
    ///   Converted string.
    /// </returns>
    function  ConvertStr2WStrCP ( const _source      : AnsiString ;
                                  const _sourceCP    : Integer
                                ) : WideString ;

    /// <summary>
    ///   Perform code page conversion.
    /// </summary>
    /// <param name="_source">
    ///   String to be converted
    /// </param>
    /// <param name="_destCP">
    ///   code page of destination (converted to)
    /// </param>
    /// <returns>
    ///   Converted string.
    /// </returns>
    function  ConvertWStr2StrCP ( const _source      : WideString ;
                                  const _destCP      : Integer
                                ) : AnsiString ;

    /// <summary>
    ///   Perform code page conversion (only if _source is String).
    /// </summary>
    /// <param name="_source">
    ///   variant to be converted
    /// </param>
    /// <param name="_sourceCP">
    ///   code page of source String (converted from)
    /// </param>
    /// <param name="_destCP">
    ///   code page of destination (converted to)
    /// </param>
    /// <returns>
    ///   Converted string.
    /// </returns>
    function  ConvertVar2StrCP  ( const _source      : Variant    ;
                                  const _sourceCP    : Integer    ;
                                  const _destCP      : Integer
                                ) : AnsiString ;

    /// <summary>
    ///   Perform code page conversion (only if _source is String).
    /// </summary>
    /// <param name="_source">
    ///   variant to be converted
    /// </param>
    /// <param name="_sourceCP">
    ///   code page of source String (converted from)
    /// </param>
    /// <returns>
    ///   Converted string.
    /// </returns>
    function  ConvertVar2WStrCP ( const _source      : Variant    ;
                                  const _sourceCP    : Integer
                                ) : WideString ;
  {$ENDIF}
{$ENDREGION}

{$REGION 'Float converters'}
  /// <summary>
  ///   Convert a float to string. Decimal separator will be a dot.
  /// </summary>
  /// <param name="_val">
  ///   value to be converted
  /// </param>
  /// <returns>
  ///   Converted value.
  /// </returns>
  function DotFloatToStr( const _val : Double ) : String ;

  /// <summary>
  ///   Convert a float to string with precision. Decimal separator will be a dot.
  /// </summary>
  /// <param name="_val">
  ///   value to be converted
  /// </param>
  /// <param name="_prec">
  ///   number of decimal digits to round to
  /// </param>
  /// <returns>
  ///   Converted value.
  /// </returns>
  ///   If the precision is positive, values are rounded to precision significant
  ///   digits after the decimal point.
  ///   If the precision is negative, values are rounded to precision significant
  ///   digits before the decimal point,
  ///   e.g. for a precision of -1 num is rounded to tens,
  ///   for a precision of -2 to hundreds, etc.
  function  DotFloatToStrPrec  ( const _val  : Double ;
                                 const _prec : Integer
                               ) : String ;

  /// <summary>
  ///   Convert a string to float.
  /// </summary>
  /// <param name="_val">
  ///   value to be converted
  /// </param>
  /// <returns>
  ///   Converted value.
  /// </returns>
  function DotStrToFloat( const _val : String ) : Double ;
{$ENDREGION}

{$REGION 'Date converters'}
  /// <summary>
  ///   Convert a date/time form local time to UTC.
  /// </summary>
  /// <param name="_val">
  ///   value to be converted
  /// </param>
  /// <returns>
  ///   Converted value.
  /// </returns>
  function  DateTimeToUTC  ( const _val : TDateTime
                           ) : TDateTime ;

  /// <summary>
  ///   Convert a date/time form UTC to local time.
  /// </summary>
  /// <param name="_val">
  ///   value to be converted
  /// </param>
  /// <returns>
  ///   Converted value.
  /// </returns>
  function  DateTimeToLocal( const _val : TDateTime
                           ) : TDateTime ;
{$ENDREGION}

{$REGION 'Variant handlers'}

const
  /// <summary>
  ///   Variant of unsupported type.
  /// </summary>
  varExUnsupported =  0 ;

  /// <summary>
  ///   Variant of nothing value.
  /// </summary>
  varExNothing     =  1 ;

  /// <summary>
  ///   Variant of ANSI string value.
  /// </summary>
  varExAnsiString  =  2 ;

  /// <summary>
  ///   Variant of wide string value.
  /// </summary>
  varExWideString  =  3 ;

  /// <summary>
  ///   Variant of date-time value.
  /// </summary>
  varExDateTime    =  4 ;

  /// <summary>
  ///   Variant of boolean value.
  /// </summary>
  varExBoolean     =  5 ;

  /// <summary>
  ///   Variant of integer value.
  /// </summary>
  varExInt         =  6 ;

  /// <summary>
  ///   Variant of unsigned integer value.
  /// </summary>
  varExUInt        =  7 ;

  /// <summary>
  ///   Variant of Int64 value.
  /// </summary>
  varExInt64       =  8 ;

  /// <summary>
  ///   Variant of unsigned In64 value.
  /// </summary>
  varExUInt64      =  9 ;

  /// <summary>
  ///   Variant of float value.
  /// </summary>
  varExFloat       = 10 ;

  /// <summary>
  ///   Variant of fixed value.
  /// </summary>
  varExFixed       = 11 ;

  /// <summary>
  ///   Check variant type.
  /// </summary>
  /// <param name="_var">
  ///   variant to be evaluated
  /// </param>
  /// <returns>
  ///   Extended variant type like varExFloat, varExInt64 etc.
  /// </returns>
  function  VarTypeEx         ( const _var : Variant
                              ) : Integer ; inline;

  /// <summary>
  ///   Check variant is any of string types.
  /// </summary>
  /// <param name="_var">
  ///   variant to be evaluated
  /// </param>
  /// <returns>
  ///   True if variant can be treated as a string.
  /// </returns>
  function  IsVariantString   ( const _var : Variant
                              ) : Boolean ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToBoolean      ( const _val : Variant
                              ) : Boolean ;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToDecimal      ( const _val : Variant
                              ) : Currency ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToByte         ( const _val : Variant
                              ) : Byte ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToUInt16       ( const _val : Variant
                              ) : Word ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToUInt32       ( const _val : Variant
                              ) : LongWord ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  function  VarToUInt64       ( const _val : Variant
                              ) : Int64 ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToSByte        ( const _val : Variant
                              ) : ShortInt ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToInt16        ( const _val : Variant
                              ) : SmallInt ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToInt32        ( const _val : Variant
                              ) : Integer ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToInt64        ( const _val : Variant
                              ) : Int64 ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToSingle       ( const _val : Variant
                              ) : Single ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToDouble       ( const _val : Variant
                              ) : Double ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToChar         ( const _val : Variant
                              ) : String ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToString       ( const _val : Variant
                              ) : String ; inline;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val">
  ///   variant to be converted
  /// </param>
  /// <returns>
  ///   Converted variant value.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarToDateTime     ( const _val : Variant
                              ) : TDateTime ; inline;

  /// <summary>
  ///   Universal null variant definition (across Native &amp; CLR)
  /// </summary>
  /// <returns>
  ///   Null variant value.
  /// </returns>
  function  NullVar             : Variant ;

  /// <summary>
  ///   The purpose of this function is to mimic Delphi RTL interface on to
  ///   maintain single source compilation.
  /// </summary>
  /// <param name="_val1">
  ///   variant to be evaluated
  /// </param>
  /// <param name="_val2">
  ///   variant to be evaluated
  /// </param>
  /// <returns>
  ///   True if vales are equal.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    Only for Internal use of TatukGIS
  ///    </note>
  /// </remarks>
  function  VarEqual          ( const _val1     : Variant ;
                                const _val2     : Variant
                              ) : Boolean ;

  /// <summary>
  ///   Copy bytes from source pointer to variant object.
  /// </summary>
  /// <param name="_dest">
  ///   destination variant
  /// </param>
  /// <param name="_source">
  ///   source pointer
  /// </param>
  /// <param name="_size">
  ///   source size
  /// </param>
  procedure VarFromPtr        ( const _dest     : Variant ;
                                const _source   : IntPtr ;
                                const _size     : Integer
                               ) ;
{$ENDREGION}

{$REGION 'TGIS_BaseStream'}
type
  /// <summary>
  ///   Equivalent to TStream. Exists only for unified streams inheritance in
  ///   all DK versions.
  /// </summary>
  TGIS_BaseStream = class( TStream )
    protected
      procedure doDestroy ; virtual;
    public
      /// <summary>
      ///   Standard destructor.
      /// </summary>
      destructor Destroy ; override;

      /// <summary>
      ///   Read byte.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadByte         (   var _buffer   : Byte
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read byte.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadByte         (   var _buffer   : Byte ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read short integer.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadShortInt     (   var _buffer   : ShortInt
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read short integer.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadShortInt     (   var _buffer   : ShortInt ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read smallint.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadSmallInt     (   var _buffer   : SmallInt
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read smallint.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadSmallInt     (   var _buffer   : SmallInt ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read word.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadWord         (   var _buffer   : Word
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read word.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadWord         (   var _buffer   : Word ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read integer.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadInteger      (   var _buffer   : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read integer.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadInteger      (   var _buffer   : Integer ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read cardinal.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadCardinal     (   var _buffer   : Cardinal
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read cardinal.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadCardinal     (   var _buffer   : Cardinal ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read int64.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadInt64        (   var _buffer   : Int64
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read int64.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadInt64        (   var _buffer   : Int64 ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read single.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadSingle       (   var _buffer   : Single
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read single.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadSingle       (   var _buffer   : Single ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read short double.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadDouble       (   var _buffer   : Double
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Read short double.
      /// </summary>
      /// <param name="_buffer">
      ///   read buffer.
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function ReadDouble       (   var _buffer   : Double ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;

      /// <summary>
      ///   Write byte.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteByte        ( const _buffer   : Byte
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write byte.
      /// </summary>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteByte        ( const _buffer   : Byte    ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write short integer.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteShortInt    ( const _buffer   : ShortInt
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write short integer.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteShortInt    ( const _buffer   : ShortInt ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write word.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteWord        ( const _buffer   : Word
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write word.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteWord        ( const _buffer   : Word ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write integer.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteInteger     ( const _buffer   : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write integer.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteInteger     ( const _buffer   : Integer ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write cardinal.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteCardinal    ( const _buffer   : Cardinal
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write cardinal.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteCardinal    ( const _buffer   : Cardinal    ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write Single.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteSingle      ( const _buffer   : Single
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write Single.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteSingle      ( const _buffer   : Single ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write double.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteDouble      ( const _buffer   : Double
                                ) : Integer ; overload; inline ;
      /// <summary>
      ///   Write double.
      /// </summary>
      /// <param name="_buffer">
      ///   write buffer
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function WriteDouble      ( const _buffer   : Double ;
                                  const _count    : Integer
                                ) : Integer ; overload; inline ;

  end ;

{$ENDREGION}

{$REGION 'TGIS_Timer'}
type
  /// <summary>
  ///   Platform dependent Timer implementation.
  /// </summary>
  TGIS_TimerAbstract = class( TGIS_ObjectDisposable )
    protected
      function  fget_NativeTimer : TObject ;
                                 virtual; abstract;
      function  fget_Enabled     : Boolean ;
                                 virtual; abstract;
      procedure fset_Enabled     ( const _value : Boolean      ) ;
                                 virtual; abstract;
      function  fget_Interval    : Cardinal ; virtual; abstract;
      procedure fset_Interval    ( const _value : Cardinal     ) ;
                                 virtual; abstract;
      function  fget_OnTimer     : TNotifyEvent ;
                                 virtual; abstract;
      procedure fset_OnTimer     ( const _value : TNotifyEvent ) ;
                                 virtual; abstract;

    public

      /// <summary>
      ///   Same as Timer.
      /// </summary>
      property  NativeTimer : TObject      read  fget_NativeTimer ;

      /// <summary>
      ///   Same as Timer.
      /// </summary>
      property  Enabled     : Boolean      read  fget_Enabled
                                           write fset_Enabled  ;

      /// <summary>
      ///   Same as Timer.
      /// </summary>
      property  Interval    : Cardinal     read  fget_Interval
                                           write fset_Interval ;

      /// <event/>
      /// <summary>
      ///   Same as Timer.
      /// </summary>
      property  OnTimer     : TNotifyEvent read  fget_OnTimer
                                           write fset_OnTimer  ;
  end;

  /// <summary>
  ///   Platform independent factory for platform dependent TGIS_Timer object.
  /// </summary>
  TGIS_TimerFactory = class
    public
      /// <summary>
      ///   Construct a platform dependent Timer.
      /// </summary>
      /// <returns>
      ///   Created timer/
      /// </returns>
      function DoCreate          : TGIS_TimerAbstract ;
                                   virtual; abstract;
  end;

  /// <summary>
  ///   Platform independent Timer (single class for both FMX and VCL).
  /// </summary>
  TGIS_Timer = class( TGIS_ObjectDisposable )
    private
      FTimer :  TGIS_TimerAbstract ;
    private
      function  fget_NativeTimer : TObject ;
      function  fget_Enabled     : Boolean ;
      procedure fset_Enabled     ( const _value : Boolean      ) ;
      function  fget_Interval    : Cardinal ;
      procedure fset_Interval    ( const _value : Cardinal     ) ;
      function  fget_OnTimer     : TNotifyEvent ;
      procedure fset_OnTimer     ( const _value : TNotifyEvent ) ;
    protected
      procedure doDestroy     ; override;

    public
      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor Create      ;

    public

      /// <summary>
      ///   Same as Timer.
      /// </summary>
      property  NativeTimer : TObject      read  fget_NativeTimer ;

      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  Enabled     : Boolean      read  fget_Enabled
                                           write fset_Enabled  ;
      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  Interval    : Cardinal     read  fget_Interval
                                           write fset_Interval ;

      /// <event/>
      /// <summary>
      ///   Same as TTimer.
      /// </summary>
      property  OnTimer     : TNotifyEvent read  fget_OnTimer
                                           write fset_OnTimer  ;
  end;
{$ENDREGION}

{$REGION 'TGIS_Http'}
  /// <summary>
  ///   Common http connection settings passed to HttpFetchStream method.
  /// </summary>
  TGIS_HttpSettings  = {$IFDEF OXYGENE} public {$ENDIF} record
    public
      /// <summary>
      ///  Cache usage.
      /// </summary>
      UseCache    : Boolean  ;
      /// <summary>
      ///  Timeout in ms.
      /// </summary>
      Timeout     : Integer  ;
      /// <summary>
      ///   Agent name.
      /// </summary>
      Agent       : String   ;
      /// <summary>
      ///   Referer address.
      /// </summary>
      Referer     : String   ;
      /// <summary>
      ///   User name.
      /// </summary>
      User        : String   ;
      /// <summary>
      ///   User password.
      /// </summary>
      Pass        : String   ;
      /// <summary>
      ///   Proxy Server address.
      /// </summary>
      ProxyServer : String ;
      /// <summary>
      ///   Proxy Server port.
      /// </summary>
      ProxyPort   : Integer ;
      /// <summary>
      ///   Proxy User name.
      /// </summary>
      ProxyUser   : String ;
      /// <summary>
      ///   Proxy User password.
      /// </summary>
      ProxyPass   : String ;
      /// <summary>
      ///   Proxy Server domain.
      /// </summary>
      ProxyDomain : String ;
    public
      /// <summary>
      ///   Create default values for http settings.
      /// </summary>
      /// <returns>
      ///   default settings
      /// </returns>
      class function CreateDefault : TGIS_HttpSettings ; static ;
  end ;

  /// <summary>
  ///   Fetch url into stream
  /// </summary>
  /// <param name="_url">
  ///   URL to be fetched
  /// </param>
  /// <param name="_options">
  ///   http connection options
  /// </param>
  /// <param name="_responseStatus">
  ///   response code (like 200, 404)
  /// </param>
  /// <param name="_responseContentLength">
  ///   response length
  /// </param>
  /// <param name="_responseContentType">
  ///   response content type
  /// </param>
  /// <param name="_responseContentExpires">
  ///   content expires time in UTC; 01-01-0001 for undefined
  /// </param>
  /// <param name="_responseHeaders">
  ///   all response headers CRLF separated
  /// </param>
  /// <param name="_responseStream">
  ///   response stream; if not assigned then newly created memory stream
  /// </param>
  procedure HttpFetchStream(
      const _url                    : String    ;
      const _options                : TGIS_HttpSettings ;
        var _responseStatus         : Integer   ;
        var _responseContentLength  : Int64     ;
        var _responseContentType    : String    ;
        var _responseContentExpires : TDateTime ;
        var _responseHeaders        : String    ;
        var _responseStream         : TStream
  ) ;
{$ENDREGION}

{$REGION 'TGIS_Hijack'}
{$IFDEF MSWINDOWS}
  type
    TGIS_Hijack = class
      private
        type
          TCode = packed record
            Jump   : Byte    ;
            Offset : Integer ;
          end;
          PCode = ^TCode;

          TAbsoluteIndirectJmp = packed record
            OpCode : Word  ;
            Addr   : DWORD ;
          end;
          PAbsoluteIndirectJmp = ^TAbsoluteIndirectJmp;
      private
        oldCode  : TCode   ;
        oldProc  : Pointer ;
      private
        function getActualAddr( const _proc    : Pointer
                              ) : Pointer;
      public
        constructor Create   (  const _oldproc : Pointer ;
                                const _newproc : Pointer
                             ) ;
        destructor  Destroy  ; override;
      end;
{$ENDIF}
{$ENDREGION}

{$REGION TRGB Records}
{$IFNDEF MSWINDOWS}
  type
    {#gendoc:hide}
    TRGBTriple = record
      public
        rgbtBlue  : Byte ;
        rgbtGreen : Byte ;
        rgbtRed   : Byte ;
    end ;

    {#gendoc:hide}
    TRGBQuad = record
      public
        rgbBlue     : Byte ;
        rgbGreen    : Byte ;
        rgbRed      : Byte ;
        rgbReserved : Byte ;
    end ;
{$ENDIF}
{$ENDREGION}

{$REGION 'Math methods speedy version'}

const
  {$IFDEF ANDROID}
    libname_posix = '/system/lib/libm.so'; // Android
  {$ENDIF}

  {$IF defined(IOS) or defined(OSX)}
    libname_posix = '/usr/lib/libSystem.dylib'; // iOS device
  {$IFEND}

  {$IFDEF UNDERSCOREIMPORTNAME}
    underscore_posix = '_';
  {$ELSE}
    underscore_posix = '';
  {$ENDIF}

  {$IF not defined(CPUARM) and defined(IOS)}
    function llrint_posix(_x: Double): Int64; cdecl;
      external libname_posix name underscore_posix + 'llrint';
  {$IFEND}

  {$IF defined(IOS) or defined(ANDROID)}
    function trunc_posix(_x: Double): Double; cdecl;
      external libname_posix name underscore_posix + 'trunc';
  {$IFEND}

  {$IF defined(IOS) or defined(OSX) or defined(ANDROID)}
    function floor_posix(_x: Double): Double; cdecl;
      external libname_posix name underscore_posix + 'floor';
  {$IFEND}

  {$IF defined(IOS) or defined(OSX) or defined(ANDROID)}
    function ceil_posix(_x: Double): Double; cdecl;
      external libname_posix name underscore_posix + 'ceil';
  {$IFEND}

  /// <summary>
  ///   Faster version of Round() for iOS Simulator.
  ///   On other methods just same as built-in method.
  /// </summary>
  /// <param name="_val">
  ///   same as Round()
  /// </param>
  /// <returns>
  ///   Same as Round().
  /// </returns>
  function RoundS ( const _val : Extended
                  ) : Int64; inline;

  /// <summary>
  ///   Faster version of Trunc() for iOS Simulator, iOS and Android.
  ///   On other methods just same as built-in method.
  /// </summary>
  /// <param name="_val">
  ///   same as Trunc()
  /// </param>
  /// <returns>
  ///   Same as Trunc().
  /// </returns>
  function TruncS ( const _val : Double
                  ) : Int64; inline;

  /// <summary>
  ///   Faster version of Floor() for iOS Simulator, iOS, Android and OSX.
  ///   On other methods just same as built-in method.
  /// </summary>
  /// <param name="_val">
  ///   same as Floor()
  /// </param>
  /// <returns>
  ///   Same as Floor().
  /// </returns>
  function FloorS ( const _val : Double
                  ) : Integer; inline;

  /// <summary>
  ///   Faster version of Ceil() for iOS Simulator, iOS, Android and OSX.
  ///   On other methods just same as built-in method.
  /// </summary>
  /// <param name="_val">
  ///   same as Ceil()
  /// </param>
  /// <returns>
  ///   Same as Ceil().
  /// </returns>
  function CeilS ( const _val : Double
                  ) : Integer; inline;

  /// <summary>
  ///   Gives information if OS is Windows 11 or not.
  /// </summary>
  /// <returns>
  ///   True for Windows 11
  /// </returns>
  function IsWin11 : Boolean ; inline ;

{$ENDREGION}

function IsMetricSystem : Boolean ;

function GetTickCount   : Cardinal ;

function GetRandom      ( const _range : Integer
                        ) : Integer ; overload ; inline ;
function GetRandom      : Double ; overload ; inline ;

function CompareDateTime(
                          const _d1 : TDateTime ;
                          const _d2 : TDateTime
                        ) : Integer ;

{$IFDEF MSWINDOWS}
  function IsWine      : Boolean;
{$ENDIF}

var
  TimerHelper : TGIS_TimerFactory ;

{$IFNDEF GIS_NORECORDS}
  type
    _TPoint = TPoint ;
    _TPointF = TPointF ;
{$ENDIF}

type
  {#gendoc:hide}
  /// <summary>
  ///   Type for Variant casting. Provided for .NET source compatibility with
  ///   ConvertVar() functions.
  /// </summary>
  ConvertVar = Variant ;

const

  {$IFNDEF LEVEL_XE3_RTL}
    pidiOSSimulator = $0008;
    pidAndroid      = $0010;
    pidLinux32      = $0020;
    pidiOSDevice    = $0040;
    pidiOSDevice32  = pidiOSDevice;
    pidLinux64      = $0080;

    pidWinNX32      = $0100;
    pidWinIoT32     = $0200; // Embedded IoT (Internet of Things) Windows w/ Intel Galileo
    pidiOSDevice64  = $0400;
    pidWinARM       = $0800;
  {$ENDIF}

  {$IFNDEF LEVEL_RX103_RTL}
    pidAllPlatforms =
         pidWin32
      or pidWin64
      or pidOSX32
      {$IFDEF LEVEL_RX103_RTL}
        or pidOSX64
      {$ENDIF}
      {$IFDEF LEVEL_XE8_RTL}
        or pidiOSDevice32
        or pidiOSDevice64
      {$ELSE}
        or pidiOSDevice
      {$ENDIF}
      or pidiOSSimulator
      or pidAndroid
      ;

    { Platform family identifiers }
    pfidWindows =
         pidWin32
      or pidWin64
         ;
    pfidOSX =
         pidOSX32
      {$IFDEF LEVEL_RX103_RTL}
        or pidOSX64
      {$ENDIF}
         ;
    pfidiOS =
           0
      {$IFDEF LEVEL_XE8_RTL}
        or pidiOSDevice32
        or pidiOSDevice64
      {$ELSE}
        or pidiOSDevice
      {$ENDIF}
      or pidiOSSimulator
         ;
    pfidAndroid =
         pidAndroid
         ;
 {$ENDIF}
    pfidiOS1 =
           0
      {$IFDEF LEVEL_XE8_RTL}
        or pidiOSDevice32
        or pidiOSDevice64
      {$ELSE}
        or pidiOSDevice
      {$ENDIF}
      or pidiOSSimulator
         ;

 pfidAllDesktops =
      pfidWindows
   or pfidOSX
   {$IFDEF LEVEL_RX103_RTL}
     or pfidLinux
   {$ENDIF}
   ;
 pfidAllMobiles =
      pfidiOS
   or pfidAndroid
   ;


implementation


uses
  System.DateUtils,
  System.Character,

  {$IFDEF ANDROID}
    Androidapi.JNI.JavaTypes,
    Androidapi.Helpers,
  {$ENDIF}
  {$IFDEF MACOSX}
    Macapi.Helpers,
    Macapi.Foundation,
  {$ENDIF}
  {$IFDEF IOS}
    Macapi.Helpers,
    iOSapi.Foundation,
  {$ENDIF}

  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}

  {$IFDEF USE_NEW_HTTP_CLIENT}
    System.Net.HttpClient,
    System.Net.URLClient,
    System.NetEncoding,
  {$ELSE}
    {$IFDEF MSWINDOWS}
      Winapi.WinInet,
    {$ELSE}
      IdHTTP,
      IdSSLOpenSSL,
    {$ENDIF}
  {$ENDIF}
  System.Math ;

const
    GIS_HTTP_FETCH_UNKNOWN          = 0   ;
    GIS_HTTP_FETCH_INTERNALERROR    = 1   ;
    GIS_HTTP_FETCH_TIMEOUT          = 2   ;
    GIS_HTTP_FETCH_NOTCOMPLETED     = 3   ;
    GIS_HTTP_OK                     = 200 ;
    GIS_HTTP_AUTHORIZATIONREQUIRED  = 401 ;
    GIS_HTTP_NOTFOUND               = 404 ;
    GIS_HTTP_SERVICEUNAVAILABLE     = 503 ;
    GIS_HTTP_GATEWAYTIMEOUT         = 504 ;

var
  dotFloatSettings : TFormatSettings ;

{$REGION 'Free handlers'}

  procedure FreeObject( var _obj ) ;
  begin
    if not Assigned( Pointer(_obj) ) then exit ;
    TObject(_obj).Free ;
    Pointer(_obj) := nil ;
  end ;

  procedure ReleaseInterface( var _obj ) ;
  begin
    if IInterface( _obj ) = nil then exit ;
    IInterface( _obj ) := nil ;
  end ;

  procedure FreeObjectNotNil( _obj : Pointer ) ;
  begin
    FreeObject( _obj ) ;
  end ;
{$ENDREGION}

{$REGION 'String helpers'}

  function IsStringEmptyOrWhiteSpace( const _str : String ) : Boolean ; inline ;
  begin
    Result := String.IsNullOrWhiteSpace( _str ) ;
  end;

  function HasStringWhiteSpace(
    const _str : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    for i := StringFirst to StringLast( _str ) do begin
      Result := IsWhiteSpace( _str, i ) ;
      if Result then
        exit ;
    end ;
    Result := False ;
  end ;

  function  IsStringEmpty(
    const _str : String
  ) : Boolean ;
  begin
    Result := length( _str ) = 0 ;
  end ;

  function StringLast(
    const _str : String
  ) : Integer ;
  begin
    Result := length( _str ) ;
  end ;

  procedure SetLengthStr( var _str : String; _len : Integer ) ;
  begin
    SetLength( _str, _len ) ;
  end ;
{$ENDREGION}

{$REGION 'String utilities'}
  function StringReplaceAll(
    const _str, _subs, _repl : String
  ) : String ;
  begin
    Result := StringReplace( _str, _subs, _repl, [rfReplaceAll] ) ;
  end ;

  function StringHash( const _str : String ) : Int64 ;
  begin
    Result := BobJenkinsHash( PChar( _str )^, SizeOf( Char ) * Length( _str ), 0 ) ;
  end;
{$ENDREGION}

{$REGION 'Convert(..)' }

  {$IFDEF NEXTGEN}
    // quickest possible ASCII to UniCode conversion
    function convert_pure_ascii(
      const _bytes : TBytes ;
      const _count : Integer
    ) : String ;
    var
      i  : Integer ;
      p  : PInteger ;
    begin
      SetLength( Result, _count ) ;

      p := @Result[1] ;
      for i:=0 to _count -1 do
        PInteger(IntPtr(p) + i*2)^ := _bytes[i] ;
    end ;
  {$ENDIF}


  function ConvertAnsiString(
    const _str : String
  ) : TBytes ;
  begin
    Result := TEncoding.ASCII.GetBytes( _str ) ;
  end ;

  function ConvertAnsiString(
    const _bytes : array of byte
  ) : String ;
  {$IFDEF NEXTGEN}
    var
      i        : Integer ;
      count    : Integer ;
      ar       : TBytes  ;
    begin
      SetLength( ar, Length( _bytes ) );
      count := 0 ;
      for i := 0 to high( _bytes ) do begin
        ar[i] := _bytes[i] ;
        if _bytes[i] = 0 then
          break ;
        inc( count ) ;
      end ;

      if count > 0 then
        Result := TEncoding.ASCII.GetString( ar, 0, count )
      else
        Result := '' ;
    end ;
  {$ELSE}
    var
      i        : Integer ;
      count    : Integer ;
    begin
      count := 0 ;
      for i := 0 to high( _bytes ) do begin
        if _bytes[i] = 0 then
          break ;
        inc( count ) ;
      end ;
      if count > 0 then
        Result := String( Copy( PAnsiChar( @_bytes[0] ), 0, count ) )
      else
        Result := '' ;
    end;
  {$ENDIF}


  function ConvertAnsiString(
    const _bytes : TBytes
  ) : String ;
  var
    i        : Integer ;
    count    : Integer ;
  begin
    count := 0 ;
    for i := 0 to high( _bytes ) do begin
      if _bytes[i] = 0 then
        break ;
      inc( count ) ;
    end ;

    if count > 0 then begin
      {$IFDEF NEXTGEN}
        Result := TEncoding.ASCII.GetString( _bytes, 0, count ) ;
      {$ELSE}
        Result := String( Copy( PAnsiChar( @_bytes[0] ), 0, count ) ) ;
      {$ENDIF}
    end
    else
      Result := '' ;
  end;


  function ConvertAnsiString(
    const _bytes : TBytes ;
    const _count : Integer
  ) : String ;
  var
    i :     Integer ;
    count : Integer ;
  begin
    count := 0 ;
    for i := 0 to Min( high( _bytes ), _count - 1 ) do begin
      if _bytes[i] = 0 then
        break ;
      inc( count ) ;
    end ;

    if count > 0 then begin
      {$IFDEF NEXTGEN}
        Result := convert_pure_ascii( _bytes, count ) ;
      {$ELSE}
        Result := String( Copy( PAnsiChar( @_bytes[0] ), 0, count ) ) ;
      {$ENDIF}
    end
    else
      Result := '' ;
  end;


  function ConvertAnsiString(
    const _bytes       : TBytes  ;
    const _count       : Integer ;
    const _codepage    : Integer ;
    const _outcodepage : Integer
  ) : String ;
  var
    i :     Integer ;
    count : Integer ;
    encode : TEncoding ;
  begin
    count := 0 ;
    for i := 0 to Min( high( _bytes ), _count - 1 ) do begin
      if _bytes[i] = 0 then
        break ;
      inc( count ) ;
    end ;

    if count > 0 then begin
      encode := TEncoding.GetEncoding( _codepage ) ;
      try
        Result := encode.GetString( _bytes, 0, count ) ;
      finally
        FreeObject( encode ) ;
      end ;
    end
    else
      Result := '' ;
  end;

  function ConvertAnsiString(
    const _bytes       : TBytes  ;
    const _codepage    : Integer ;
    const _outcodepage : Integer
  ) : String ;
  var
    i        : Integer ;
    count    : Integer ;
    encode : TEncoding ;
  begin
    count := 0 ;
    for i := 0 to high( _bytes ) do begin
      if _bytes[i] = 0 then
        break ;
      inc( count ) ;
    end ;

    if count > 0 then begin
      encode := TEncoding.GetEncoding( _codepage ) ;
      try
        Result := encode.GetString( _bytes, 0, count ) ;
      finally
        FreeObject( encode ) ;
      end ;
    end
    else
      Result := '' ;
  end ;

  function ConvertAnsiString(
    const _bytes       : TBytes  ;
    const _offset      : Integer ;
    const _count       : Integer ;
    const _codepage    : Integer ;
    const _outcodepage : Integer
  ) : String ;
  var
    i      : Integer ;
    count  : Integer ;
    encode : TEncoding ;
  begin
    count := 0 ;
    for i := _offset to Min( high( _bytes ), _offset + _count - 1 ) do begin
      if _bytes[i] = 0 then
        break ;
      inc( count ) ;
    end ;

    if count > 0 then begin
      encode := TEncoding.GetEncoding( _codepage ) ;
      try
        Result := encode.GetString( _bytes, _offset, count ) ;
      finally
        FreeObject( encode ) ;
      end ;
    end
    else
      Result := '' ;
  end ;

{$IFNDEF NEXTGEN}
  function ConvertStr2StrCP(
    const _source   : AnsiString ;
    const _sourceCP : Integer  ;
    const _destCP   : Integer
  ) : AnsiString ;
  var
    enc_src : TEncoding ;
    enc_dst : TEncoding ;
  begin
    if _sourceCP = _destCP then begin
      Result := _source ;
      exit ;
    end ;

    try
      {$MESSAGE WARN '### Verify no Windows code ## '}
      enc_src := TEncoding.GetEncoding( _sourceCP ) ;
      enc_dst := TEncoding.GetEncoding( _destCP   ) ;
      try
        Result := enc_dst.GetEncoding( _destCP ).GetString(
                    enc_src.GetBytes( _source )
                  ) ;
      finally
        FreeObject( enc_src );
        FreeObject( enc_dst );
      end ;
    except
      Result := _source ;
    end ;
  end ;

  function ConvertStr2WStrCP(
    const _source   : AnsiString ;
    const _sourceCP : Integer
  ) : WideString ;
  var
    enc_src : TEncoding ;
  begin
    try
      {$MESSAGE WARN '### Verify no Windows code ## '}
      enc_src := TEncoding.GetEncoding( _sourceCP ) ;
      try
        Result := enc_src.GetString(
                    TEncoding.Default.GetBytes( _source )
                  ) ;
      finally
        FreeObject( enc_src ) ;
      end ;
    except
      Result := _source ;
    end ;
  end ;

  function  ConvertWStr2StrCP(
    const _source : WideString ;
    const _destCP : Integer
  ) : AnsiString ;
  var
    enc_dst : TEncoding ;
  begin
    {$MESSAGE WARN '### Verify no Windows code ## '}
    try
      enc_dst := TEncoding.GetEncoding(_destCP) ;
      try
        Result := ConvertAnsiString( enc_dst.GetBytes( _source ) ) ;
      finally
        FreeObject( enc_dst ) ;
      end ;
    except
      Result := _source ;
    end ;
  end ;

  function ConvertVar2StrCP(
    const _source   : Variant ;
    const _sourceCP : Integer ;
    const _destCP   : Integer
  ) : AnsiString ;
  begin
    case VarTypeEx( _source ) of
      varExAnsiString :
        begin
          if _sourceCP <> _destCP then
            Result := ConvertStr2StrCP( AnsiString( _source ),
                                        _sourceCP, _destCP )
          else
            Result := AnsiString( _source ) ;
        end ;
        varExWideString :
          Result := ConvertWStr2StrCP( _source, _destCP ) ;
      else
          Result := AnsiString( _source ) ;
    end ;
  end ;

  function ConvertVar2WStrCP(
    const _source   : Variant ;
    const _sourceCP : Integer
  ) : WideString ;
  begin
    case VarTypeEx( _source ) of
      varExAnsiString :
        begin
          Result := ConvertStr2WStrCP( AnsiString( _source ), _sourceCP ) ;
        end ;
      else
        begin
          Result := _source ;
        end ;
    end ;
  end ;
{$ENDIF}

{$ENDREGION}

{$REGION 'Float converters'}

  function DotStrToFloat( const _val : String ) : Double ;
  var
    c        : Char ;
    state    : Integer ;
    i        : Integer ;
    sig_int  : Integer ;
    val_int  : Double  ;
    num_dec  : Double  ;
    val_dec  : Double  ;
    sig_exp  : Integer ;
    val_exp  : Integer ;
    err      : Integer ;
  begin
    sig_int := 1  ;
    val_int := 0  ;
    val_dec := 0  ;
    num_dec := 1  ;
    sig_exp := 1  ;
    val_exp := 0  ;

    if IsStringEmpty( _val ) then
      err := 1
    else
      err := 0 ;

    state := 0 ;
    for i:=1 to Length( _val ) do begin
      c := _val[i] ;
      case state of
        0 : begin
              if c = ' ' then begin
                continue ;
              end
              else if c = '+' then begin
                state := 1 ;
              end
              else if c = '-' then begin
                sig_int := -1 ;
                state := 1 ;
              end
              else if ( c >= '0' ) and ( c <= '9' ) then begin
                val_int := val_int * 10 + Ord( c ) - Ord( '0' ) ;
                state := 1 ;
              end
              else if ( c = '.' ) or ( c = ',' ) then begin
                state := 2 ;
              end
              else begin
                err := 1 ;
                break ;
              end;
            end;
        1 : begin
              if ( c >= '0' ) and ( c <= '9' ) then begin
                val_int := val_int * 10 + Ord( c ) - Ord( '0') ;
              end
              else if (c = '.') or (c = ',') then begin
                state := 2 ;
              end
              else if ( c = 'E' ) or ( c = 'e' ) then begin
                state := 3 ;
              end
              else if c = ' ' then begin
                continue ;
              end
              else begin
                err := 1 ;
                break ;
              end ;
            end;
        2 : begin
              if ( c >= '0' ) and ( c <= '9' ) then begin
                val_dec := val_dec * 10 + Ord( c ) - Ord( '0' ) ;
                num_dec := num_dec * 10 ;
              end
              else if ( c = 'E' ) or ( c = 'e' ) then begin
                state := 3 ;
              end
              else begin
                err := 1 ;
                break ;
              end ;
            end ;
        3 : begin
              if c = '+' then begin
                sig_exp := 1 ;
                state := 4 ;
              end
              else if c = '-' then begin
                sig_exp := -1 ;
                state := 4 ;
              end
              else if ( c >= '0' ) and ( c <= '9' ) then begin
                val_exp := val_exp * 10 + Ord( c ) - Ord( '0' ) ;
                state := 4 ;
              end
              else if c = ' ' then begin
                continue ;
              end
              else begin
                err := 2 ;
                break ;
              end ;
            end ;
        4 : begin
              if ( c >= '0' ) and ( c <= '9' ) then begin
                val_exp := val_exp * 10 + Ord( c ) - Ord( '0' ) ;
                state := 4 ;
              end
              else begin
                err := 3 ;
                break ;
              end ;
            end;
      end;
    end;

    if err <> 0 then
      raise EConvertError.CreateFmt(
              '''%s'' is not a valid floating point value',
             [_val]
            );

    Result := sig_int * ( val_int + val_dec / num_dec ) ;
    if val_exp <> 0 then begin
      Result := Power10( Result, sig_exp * val_exp ) ;
    end;
  end;

  function DotFloatToStr( const _val : Double ) : String ;
  begin
    Result := FloatToStr( _val, dotFloatSettings ) ;
  end ;

  function DotFloatToStrPrec(
    const _val  : Double ;
    const _prec : Integer
  ) : String ;
  begin
    Result := FloatToStr( RoundTo( _val, -_prec ), dotFloatSettings ) ;
  end ;


{$ENDREGION}

{$REGION 'Date handlers'}

  function DateTimeToUTC(
    const _val : TDateTime
  ) : TDateTime ;
  begin
    Result := TTimeZone.Local.ToUniversalTime( _val ) ;
  end;
  function DateTimeToLocal(
    const _val : TDateTime
  ) : TDateTime ;
  begin
    Result := TTimeZone.Local.ToLocalTime( _val ) ;
  end;

{$ENDREGION}

{$REGION 'Variant handlers'}


function VarTypeEx(
  const _var : Variant
): Integer;
begin
  case VarType( _var ) of
      varEmpty        : Result := varExNothing     ; // $0000
      varNull         : Result := varExNothing     ; // $0001
      varSmallInt     : Result := varExInt         ; // $0002
      varInteger      : Result := varExInt         ; // $0003
      varSingle       : Result := varExFloat       ; // $0004
      varDouble       : Result := varExFloat       ; // $0005
      varCurrency     : Result := varExFixed       ; // $0006
      varDate         : Result := varExDateTime    ; // $0007
      varOleStr       : Result := varExWideString  ; // $0008
      varDispatch     : Result := varExUnsupported ; // $0009
      varError        : Result := varExUnsupported ; // $000A
      varBoolean      : Result := varExBoolean     ; // $000B
      varVariant      : Result := varExUnsupported ; // $000C
      varUnknown      : Result := varExUnsupported ; // $000C
      //varUndef0F    : Result := varExUnsupported ; ; // $000F - unsupported by MS
      varShortInt     : Result := varExInt         ; // $0010
      varByte         : Result := varExUInt        ; // $0011
      varWord         : Result := varExUInt        ; // $0012
      varLongWord     : Result := varExUInt        ; // $0013
      varInt64        : Result := varExInt64       ; // $0014
      varUInt64       : Result := varExUInt64      ; // $0015
      varAny          : Result := varExUnsupported ; // $0101
      varString       : Result := varExAnsiString  ; // $0100
      varUString      : Result := varExWideString  ;
      varTypeMask     : Result := varExUnsupported ; // $0FFF
      varArray        : Result := varExUnsupported ; // $2000
      varByRef        : Result := varExUnsupported   // $4000
      else              Result := varExUnsupported ;
    end ;
end;

function IsVariantString(
  const _var : Variant
) : Boolean ;
begin
  case VarType( _var ) of
    varOleStr     : Result := True ;
    varStrArg     : Result := True ;
    varString     : Result := True ;
    varUString    : Result := True ;
    else            Result := False ;
  end ;
end ;

  function VarToBoolean(
    const _val : Variant
  ) : Boolean ;
  var
    val  : Boolean ;
    str  : String  ;

    function _t( const _str : String ) : Boolean ;
    begin
      Result := CompareText( str, _str, TLocaleOptions.loInvariantLocale ) = 0 ;
    end ;

  begin
    if VarTypeEx( _val ) = varExBoolean then begin
      Result := _val ;
      exit ;
    end ;

    str := String( _val ) ;
    if TryStrToBool( str, val ) then
                           Result := val
    else if _t('Y')   then Result := True
    else if _t('N')   then Result := False
    else if _t('T')   then Result := True
    else if _t('F')   then Result := False
    else if _t('YES') then Result := True
    else if _t('NO')  then Result := False
    else if _t('1')   then Result := True
    else if _t('0')   then Result := False
    else if _t('')    then Result := False
    else if VarIsNull( _val )
                      then Result := False
    else if VarIsEmpty( _val )
                      then Result := False
    else                   Result := _val ;
  end ;

  function VarToDecimal(
    const _val : Variant
  ) : Currency ;
  begin
    Result := _val ;
  end ;

  function VarToByte(
    const _val : Variant
  ) : Byte ;
  begin
    Result := _val ;
  end ;

  function VarToUInt16(
    const _val : Variant
  ) : Word ;
  begin
    Result := _val ;
  end ;

  function VarToUInt32(
    const _val : Variant
  ) : LongWord ;
  begin
    Result := _val ;
  end ;

  function VarToUInt64(
    const _val : Variant
  ) : Int64 ;
  begin
    Result := _val ;
  end ;

  function VarToSByte(
    const _val : Variant
  ) : ShortInt ;
  begin
    Result := _val ;
  end ;

  function VarToInt16(
    const _val : Variant
  ) : SmallInt ;
  begin
    if VarIsNull(_val) or VarIsEmpty(_val) then
      Result := 0
    else
      Result := _val ;
  end ;

  function VarToInt32(
    const _val : Variant
  ) : Integer  ;
  begin
    if VarIsNull(_val) or VarIsEmpty(_val) then
      Result := 0
    else
      Result := _val ;
  end ;

  function VarToInt64(
    const _val : Variant
  ) : Int64 ;
  begin
    if VarIsNull(_val) or VarIsEmpty(_val) then
      Result := 0
    else
      Result := _val ;
  end ;

  function VarToSingle(
    const _val : Variant
  ) : Single ;
  begin
    if VarIsNull(_val) or VarIsEmpty(_val) then
      Result := 0
    else
      Result := _val ;
  end ;

  function VarToDouble(
    const _val : Variant
  ) : Double ;
  begin
    if VarIsNull(_val) or VarIsEmpty(_val) then
      Result := 0
    else
      Result := _val ;
  end ;

  function VarToChar(
    const _val : Variant
  ) : String ;
  begin
    Result := _val ;
  end ;

  function VarToString(
    const _val : Variant
  ) : String ;
  begin
    if VarIsNull(_val) or VarIsEmpty(_val) then
      Result := ''
    else
      Result := _val ;
  end ;

  function VarToDateTime(
    const _val : Variant
  ) : TDateTime ;
  begin
    Result := System.Variants.VarToDateTime( _val ) ;
  end ;

  function NullVar : Variant ;
  begin
    Result := Null ;
  end;

  function VarEqual(
    const _val1 : Variant ;
    const _val2 : Variant
  ) : Boolean ;
  begin
    if IsVariantString( _val1 ) and  IsVariantString( _val2 ) then
      Result := ( VarToString( _val1 ) = VarToString( _val2 ) )
    else
      Result := ( _val1 = _val2 ) ;
  end ;

  procedure VarFromPtr(
    const _dest     : Variant ;
    const _source   : IntPtr ;
    const _size     : Integer
   ) ;
  var
    vr : Pointer ;
  begin
    vr := VarArrayLock( _dest ) ;
    try
      {$MESSAGE WARN '### Verify FMX code on OSX ## '}
      Move( Pointer(_source)^, vr^, _size ) ;
    finally
      VarArrayUnLock( _dest ) ;
    end ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_BaseStream'}

  destructor TGIS_BaseStream.Destroy ;
  begin
    doDestroy ;

    inherited ;
  end ;

  procedure TGIS_BaseStream.doDestroy ;
  begin
    // lowest level class, do nothing
  end ;

  function TGIS_BaseStream.ReadByte(
    var _buffer   : Byte
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Byte ) ) ;
  end ;

  function TGIS_BaseStream.ReadByte(
      var _buffer   : Byte ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadShortInt(
    var _buffer   : ShortInt
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( ShortInt ) ) ;
  end ;

  function TGIS_BaseStream.ReadShortInt(
      var _buffer   : ShortInt ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadSmallInt(
    var _buffer : SmallInt
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( SmallInt ) ) ;
  end ;

  function TGIS_BaseStream.ReadSmallInt(
      var _buffer : SmallInt ;
    const _count  : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadWord(
    var _buffer   : Word
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Word ) ) ;
  end ;

  function TGIS_BaseStream.ReadWord(
      var _buffer   : Word ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadInteger(
     var _buffer   : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Integer ) ) ;
  end ;

  function TGIS_BaseStream.ReadInteger(
      var _buffer   : Integer ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadCardinal(
    var _buffer   : Cardinal
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Cardinal ) ) ;
  end ;

  function TGIS_BaseStream.ReadCardinal(
      var _buffer   : Cardinal ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadInt64(
    var _buffer : Int64
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Int64 ) ) ;
  end ;

  function TGIS_BaseStream.ReadInt64(
      var _buffer : Int64 ;
    const _count  : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadSingle(
    var _buffer : Single
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Single ) ) ;
  end ;

  function TGIS_BaseStream.ReadSingle(
      var _buffer : Single ;
    const _count  : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.ReadDouble(
    var _buffer   : Double
  ) : Integer ;
  begin
    Result := Read( _buffer, sizeOf( Double ) ) ;
  end ;

  function TGIS_BaseStream.ReadDouble(
      var _buffer   : Double ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Read( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteByte(
    const _buffer   : Byte
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( Byte ) ) ;
  end ;

  function TGIS_BaseStream.WriteByte(
    const _buffer   : Byte    ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteShortInt(
    const _buffer   : ShortInt
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( ShortInt ) ) ;
  end ;

  function TGIS_BaseStream.WriteShortInt(
    const _buffer   : ShortInt ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteWord(
    const _buffer   : Word
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( Word ) ) ;
  end ;

  function TGIS_BaseStream.WriteWord(
    const _buffer   : Word ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteInteger(
    const _buffer   : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( Integer ) ) ;
  end ;

  function TGIS_BaseStream.WriteInteger(
    const _buffer   : Integer    ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteCardinal(
    const _buffer   : Cardinal
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( Cardinal ) ) ;
  end ;

  function TGIS_BaseStream.WriteCardinal(
    const _buffer   : Cardinal ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteDouble(
    const _buffer   : Double
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( Double ) ) ;
  end ;

  function TGIS_BaseStream.WriteDouble(
    const _buffer   : Double ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

  function TGIS_BaseStream.WriteSingle(
    const _buffer   : Single
  ) : Integer ;
  begin
    Result := Write( _buffer, sizeOf( Single ) ) ;
  end ;

  function TGIS_BaseStream.WriteSingle(
    const _buffer   : Single ;
    const _count    : Integer
  ) : Integer ;
  begin
    Result := Write( _buffer, _count ) ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_Http'}

  class function TGIS_HttpSettings.CreateDefault : TGIS_HttpSettings ;
  begin
    Result.UseCache    := False  ;
    Result.Timeout     := 0  ;
    Result.Agent       := ''   ;
    Result.Referer     := ''   ;
    Result.User        := ''   ;
    Result.Pass        := ''   ;
    Result.ProxyServer := '' ;
    Result.ProxyPort   := 0 ;
    Result.ProxyUser   := '' ;
    Result.ProxyPass   := '' ;
    Result.ProxyDomain := '' ;
  end;

  function httpDate( const _val : String ) : TDateTime;
  var
    sDay       : String ;
    sMonth     : String ;
    sYear      : String ;
    sHour      : String ;
    sMinute    : String ;
    sSecond    : String ;
    sZone      : String ;
    i          : Integer ;
    c          : Char    ;
    state      : Integer ;

    iDay       : Integer ;
    iMonth     : Integer ;
    iYear      : Integer ;
    iHour      : Integer ;
    iMinute    : Integer ;
    iSecond    : Integer ;

  begin
    if _val = '' then begin
      Result := DateTimeToUTC( Now ) + 1/24 ; // 60 minutes default expiration
      exit ;
    end;

    try
      if _val = '0' then Abort ;

      sDay       := '' ;
      sMonth     := '' ;
      sYear      := '' ;
      sZone      := '' ;

      state := 1 ;
      for i := StringFirst to StringLast( _val ) do begin
        c := _val[ i ] ;
        case state of
          1  : begin
                 // eat day of week
                 if (c = ',') or (c = ' ') then
                   state := 2 ;
               end;
          2  : begin
                 if ( c = ' ' ) and ( sMonth = '' ) then
                   continue ;

                 if c >='A' then begin
                   sMonth := sMonth + c ;
                   state := 50 // ANSIC
                 end
                 else begin
                   sDay := sDay + c ;
                   state := 3 ;
                 end ;
               end ;
          3  : begin
                // eat day
                 if ( c = ' ' ) and ( sDay = '' ) then
                   continue ;

                 if (c = '-') or (c = ' ') then
                   state := 4
                 else
                   sDay := sDay + c ;
               end ;
          4  : begin
                 // eat month
                 if ( c = ' ' ) and ( sMonth = '' ) then
                   continue ;

                 if (c = '-') or (c = ' ') then
                   state := 5
                 else
                   sMonth := sMonth + c ;
               end;
          5  : begin
                 // eat year
                 if ( c = ' ' ) and ( sYear = '' ) then
                   continue ;

                 if c = ' ' then
                   state := 6
                 else
                   sYear := sYear + c ;
               end ;
          6  : begin
                 // eat hour
                 if ( c = ' ' ) and ( sSecond = '' ) then
                   continue ;

                 if c = ':' then
                   state := 7
                 else
                   sHour := sHour + c ;
               end ;
          7 : begin
                  // eat minute
                 if ( c = ' ' ) and ( sMinute = '' ) then
                   continue ;

                  if c = ':' then
                   state := 8
                 else
                    sMinute := sMinute + c ;
               end ;
          8 : begin
                 // eat second
                 if ( c = ' ' ) and ( sSecond = '' ) then
                   continue ;

                 if c = ' ' then
                   state := 9
                 else
                   sSecond := sSecond + c ;
               end ;
          9  : begin
                 // eat Zone
                 if ( c = ' ' ) and ( sZone = '' ) then
                   continue ;

                 if c = ' ' then
                   state := 99
                 else
                   sZone := sZone + c ;
               end;
          50 : begin
                 // eat month
                 if ( c = ' ' ) and ( sMonth = '' ) then
                   continue ;

                 if (c = ' ') then
                   state := 51
                 else
                   sMonth := sMonth + c ;
               end;
          51 : begin
                 // eat day
                 if ( c = ' ' ) and ( sDay = '' ) then
                   continue ;

                 if (c = ' ') then
                   state := 52
                 else
                   sDay := sDay + c ;
                end ;
          52 :  begin
                 // eat hour
                 if ( c = ' ' ) and ( sHour = '' ) then
                   continue ;

                 if c = ':' then
                   state := 53
                 else
                   sHour := sHour + c ;
               end ;
          53 : begin
                 // eat minute
                 if ( c = ' ' ) and ( sMinute = '' ) then
                   continue ;

                 if c = ':' then
                   state := 54
                 else
                    sMinute := sMinute + c ;
               end ;
          54 : begin
                 // eat second
                 if ( c = ' ' ) and ( sSecond = '' ) then
                   continue ;

                 if c = ' ' then
                   state := 55
                 else
                   sSecond := sSecond + c ;
               end ;
          55 : begin
                 // eat year
                 if ( c = ' ' ) and ( sYear = '' ) then
                   continue ;

                 if c = ' ' then
                   state := 99
                 else
                   sYear := sYear + c ;
               end ;
          99 : begin
                 // eat trailing test
               end ;
        end;
      end ;

      iDay       := StrToInt( Trim( sDay ) ) ;

      sMonth := Uppercase( sMonth ) ;
      if      sMonth = 'JAN' then iMonth := 1
      else if sMonth = 'FEB' then iMonth := 2
      else if sMonth = 'MAR' then iMonth := 3
      else if sMonth = 'APR' then iMonth := 4
      else if sMonth = 'MAY' then iMonth := 5
      else if sMonth = 'JUN' then iMonth := 6
      else if sMonth = 'JUL' then iMonth := 7
      else if sMonth = 'AUG' then iMonth := 8
      else if sMonth = 'SEP' then iMonth := 9
      else if sMonth = 'OCT' then iMonth := 10
      else if sMonth = 'NOV' then iMonth := 11
      else if sMonth = 'DEC' then iMonth := 12
      else                        iMonth := 99 ;

      iYear      := StrToInt( Trim( sYear ) ) ;
      if iYear <= 99 then
        iYear := 1900 + iYear ;
      iHour      := StrToInt( Trim( sHour ) ) ;
      iMinute    := StrToInt( Trim( sMinute ) ) ;
      iSecond    := StrToInt( Trim( sSecond ) ) ;

      if ( sZone <> 'UTC' ) and
         ( sZone <> 'GMT' ) and
         ( sZone <> '' )
      then
       abort ;

      Result := EncodeDateTime( iYear, iMonth, iDay, iHour, iMinute, iSecond, 0 ) ;

    except
      Result := EncodeDateTime( 1, 1, 1, 0, 0, 0, 0 ) ;
    end;
  end;

  {$IFDEF USE_NEW_HTTP_CLIENT}

    type
      T_HTTPClientEventHepler = class
        class procedure doValidateServerCertificate(
                const Sender      : TObject;
                const ARequest    : TURLRequest;
                const Certificate : TCertificate;
                  var Accepted    : Boolean
              ) ;
        class procedure doNeedClientCertificate(
                const Sender        : TObject;
                const ARequest      : TURLRequest;
                const Certificates  : TCertificateList;
                  var AnIndex       : Integer
              ) ;
      end ;

    class procedure T_HTTPClientEventHepler.doValidateServerCertificate(
      const Sender      : TObject;
      const ARequest    : TURLRequest;
      const Certificate : TCertificate;
        var Accepted    : Boolean
    ) ;
    begin
      Accepted := True ;
    end ;

    class procedure T_HTTPClientEventHepler.doNeedClientCertificate(
            const Sender        : TObject;
            const ARequest      : TURLRequest;
            const Certificates  : TCertificateList;
              var AnIndex       : Integer
          ) ;
    begin
      //
    end ;

    procedure HttpFetchStream(
        const _url                    : String   ;
        const _options                : TGIS_HttpSettings ;
          var _responseStatus         : Integer  ;
          var _responseContentLength  : Int64    ;
          var _responseContentType    : String   ;
          var _responseContentExpires : TDateTime ;
          var _responseHeaders        : String    ;
          var _responseStream         : TStream
    ) ;
    var
      ohttp : THTTPClient ;
      ores  : IHTTPResponse ;
      bauth : TNetHeader ;
      refer : TNetHeader ;
      hdrs  : TNetHeaders ;
      itm   : TNameValuePair ;
    begin
      if IsStringEmpty( _url ) then
        exit ; // we only zeroed result

      if not assigned( _responseStream ) then
        _responseStream := TMemoryStream.Create ;

      ohttp := THTTPClient.Create ;
      try
        ohttp.HandleRedirects := True ;
        ohttp.UserAgent := _options.Agent ;
        ohttp.OnValidateServerCertificate := T_HTTPClientEventHepler.doValidateServerCertificate ;
        ohttp.OnNeedClientCertificate     := T_HTTPClientEventHepler.doNeedClientCertificate ;

        if not IsStringEmpty( _options.User ) then begin
          bauth := TNetHeader.Create(
                      'Authorization',
                      'Basic ' +
                      TNetEncoding.Base64.Encode(_options.User + ':' + _options.Pass)
                    ) ;
        end ;

        if not IsStringEmpty( _options.Referer ) then
          refer := TNetHeader.Create( 'Referer', _options.Referer ) ;

        if not IsStringEmpty( _options.ProxyServer ) then begin
          ohttp.ProxySettings := TProxySettings.Create( _options.ProxyServer,
                                                        _options.ProxyPort,
                                                        _options.ProxyUser,
                                                        _options.ProxyPass,
                                                        _options.ProxyDomain
                                                       ) ;
        end ;

        try
          hdrs := [] ;
          if not IsStringEmpty( _options.User ) then
            hdrs := hdrs + [bauth] ;
          if not IsStringEmpty( _options.Referer ) then
            hdrs := hdrs + [refer] ;

          if not _options.UseCache then begin
            hdrs := hdrs + [ TNetHeader.Create( 'Pragma', 'no-cache' )        ] ;
            hdrs := hdrs + [ TNetHeader.Create( 'Cache-Control', 'no-cache' ) ] ;
          end ;

          hdrs := hdrs + [ TNetHeader.Create( 'Accept-Encoding', 'gzip' ) ] ;

          if _options.Timeout > 0 then begin
            ohttp.ConnectionTimeout := _options.Timeout ;
            ohttp.ResponseTimeout   := _options.Timeout ;
          end ;

          ores := ohttp.Get( _url, _responseStream, hdrs ) ;

          _responseStatus        := ores.StatusCode ;
          _responseContentLength := ores.ContentLength ;
          _responseContentType   := ores.MimeType ;

          _responseHeaders := '' ;
          _responseContentExpires := httpDate( '' ) ; // default expiration
          for itm in ores.Headers do begin
            _responseHeaders := _responseHeaders + itm.Name + ': ' + itm.Value + #13#10 ;

            if itm.Name = 'Expires' then
              _responseContentExpires := httpDate( itm.Value )
            else if itm.Name = 'Cache-Control' then begin
              if itm.Value = 'no-cache' then
                _responseContentExpires := httpDate( '0' ) ;
            end
          end ;

        except
          on ex : ENetHTTPClientException do begin
            _responseStatus := GIS_HTTP_SERVICEUNAVAILABLE ;
            _responseHeaders := ex.Message ;
          end
          else begin
            if assigned( ores ) then begin
              _responseStatus := ores.StatusCode ;
              _responseHeaders := ores.StatusText ;
            end
            else
              _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
          end ;
        end;
      finally
        FreeObject( ohttp ) ;
      end ;
    end ;
  {$ELSE}
    {$IFDEF MSWINDOWS}
      {$HPPEMIT '#pragma comment( lib, "wininet" )'}

      procedure HttpFetchStream(
          const _url                    : String   ;
          const _options                : TGIS_HttpSettings ;
            var _responseStatus         : Integer  ;
            var _responseContentLength  : Int64    ;
            var _responseContentType    : String   ;
            var _responseContentExpires : TDateTime ;
            var _responseHeaders        : String    ;
            var _responseStream         : TStream
      ) ;
      var
        total     : Cardinal  ;
        res       : WordBool  ;
        bssl      : Boolean ;
        batn      : Boolean   ;
        ext       : String    ;
        agent     : String    ;
        hcon      : HINTERNET ;
        hdoc      : HINTERNET ;
        hurl      : HINTERNET ;
        hrequest  : HINTERNET ;
        option    : DWORD     ;
        a         : TURLComponents;
        val       : String ;
        usr       : String ;
        psw       : String ;
        dwflags   : DWORD ;
        abrt      : Boolean ;
        buf       : array[0..65535] of Byte ;
        flags     : DWORD ;
        buf_len   : DWORD ;
        surl      : String ;
        res_buf   : TBytes ;
        res_len   : DWORD  ;
        res_idx   : DWORD  ;
        curl      : String ;
      begin
        if IsStringEmpty( _url ) then
          exit ; // we only zeroed result

        if not assigned( _responseStream ) then
          _responseStream := TMemoryStream.Create ;

        FillChar( a, sizeOf( TURLComponents ), 0 ) ;

          a.dwStructSize      := sizeOf( a ) ;
          a.lpszScheme        := nil ;
          a.dwSchemeLength    := INTERNET_MAX_SCHEME_LENGTH ;
          a.lpszHostName      := nil ;
          a.dwHostNameLength  := INTERNET_MAX_HOST_NAME_LENGTH ;
          a.lpszUrlPath       := nil ;
          a.dwUrlPathLength   := INTERNET_MAX_PATH_LENGTH ;
          a.lpszExtraInfo     := nil ;
          a.dwExtraInfoLength := INTERNET_MAX_PATH_LENGTH ;
          a.lpszUserName      := nil ;
          a.dwUserNameLength  := INTERNET_MAX_USER_NAME_LENGTH ;
          a.lpszPassword      := nil ;
          a.dwPasswordLength  := INTERNET_MAX_PASSWORD_LENGTH ;

        SetLength( curl, INTERNET_MAX_URL_LENGTH ) ;
        buf_len := Length( curl ) ;
        if InternetCanonicalizeUrl( PChar(_url),
                                    PChar(curl),
                                    buf_len,
                                    ICU_BROWSER_MODE
                                   ) then begin
          SetLength( curl, StrLen( PChar(curl) ) ) ;
          InternetCrackUrl( PChar( curl ),length( curl ), 0, a ) ;
        end
        else
          InternetCrackUrl( PChar( _url ),length( _url ), 0, a ) ;

        agent := _options.Agent ;
        hcon := InternetOpen( PChar(agent),
                               INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0
                             ) ;
        try
          if _options.Timeout > 0 then begin
            option := _options.Timeout ;

            InternetSetOption( hcon, INTERNET_OPTION_CONNECT_TIMEOUT,
                               Addr( option ), sizeOf( option )
                             ) ;
            InternetSetOption( hcon, INTERNET_OPTION_DATA_SEND_TIMEOUT,
                               Addr( option ), sizeOf( option )
                             ) ;
            InternetSetOption( hcon, INTERNET_OPTION_DATA_RECEIVE_TIMEOUT,
                               Addr( option ), sizeOf( option )
                             ) ;
            InternetSetOption( hcon, INTERNET_OPTION_SEND_TIMEOUT,
                               Addr( option ), sizeOf( option )
                             ) ;
            InternetSetOption( hcon, INTERNET_OPTION_RECEIVE_TIMEOUT,
                               Addr( option ), sizeOf( option )
                             ) ;
          end ;

          batn := not IsStringEmpty( _options.User ) ;
          bssl := a.nPort = INTERNET_DEFAULT_HTTPS_PORT ;

          if batn then begin
            usr  := _options.User + #0 ;
            psw  := _options.Pass + #0 ;
            val  := copy( a.lpszHostName, StringFirst, Pos('/',a.lpszHostName)-1 ) ;

            hurl := InternetConnect(
                      hcon, PChar(val),
                      a.nPort, PChar(usr), PChar(psw),
                      INTERNET_SERVICE_HTTP, 0, 0
                    ) ;
            dwflags := INTERNET_FLAG_KEEP_CONNECTION ;
            if bssl then
              dwflags := dwflags or INTERNET_FLAG_SECURE ;

            hrequest := HttpOpenRequest(
                          hurl, PChar('GET'),
                          a.lpszUrlPath, HTTP_VERSION, '', nil,
                          dwflags, 0
                        ) ;
            if not HttpSendRequest( hrequest, nil, 0, nil, 0 ) then begin
              if GetLastError = ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED then begin ;
                flags   := 0 ;
                buf_len := sizeOf( flags ) ;
                InternetQueryOption( hcon, INTERNET_OPTION_SECURITY_FLAGS,
                                     @flags, buf_len
                                    ) ;
                flags := flags + SECURITY_FLAG_IGNORE_UNKNOWN_CA      +
                                 SECURITY_FLAG_IGNORE_CERT_CN_INVALID +
                                 SECURITY_FLAG_IGNORE_CERT_DATE_INVALID ;

                InternetSetOption( hcon, INTERNET_OPTION_SECURITY_FLAGS,
                                   Pointer(flags), sizeOf(flags)
                                  ) ;
                if not HttpSendRequest( hrequest, nil, 0, nil, 0 ) then begin
                  _responseStatus := GIS_HTTP_AUTHORIZATIONREQUIRED ;
                  exit ;
                end;
              end;
            end;

          end ;
          surl := a.lpszScheme ;

          hdoc := nil ;
          hrequest := nil ;
          flags := INTERNET_FLAG_RAW_DATA or INTERNET_FLAG_DONT_CACHE;
          if not _options.UseCache then
            flags := flags or INTERNET_FLAG_RELOAD ;

          if bssl then
            flags := flags or INTERNET_FLAG_SECURE ;

          try
            if not IsStringEmpty( _options.Referer ) then
              val := 'Referer: ' + _options.Referer
            else
              val := '' ;

            if not IsStringEmpty( val ) then
              val := val + #13#10 ;

            if not _options.UseCache then begin
              val := val + 'Pragma: no-cache' + #13#10;
              val := val + 'Cache-Control: no-cache' + #13#10 ;
            end;

            val := val + 'Accept-Encoding: gzip' ;

            hdoc := InternetOpenURL( hcon, PChar( surl ), PChar( val ), length( val ), flags, 0 ) ;
            try
                if hdoc = nil then begin
                  if ( GetLastError and ERROR_INTERNET_INVALID_CA ) = ERROR_INTERNET_INVALID_CA then begin

                    flags   := 0 ;
                    buf_len := sizeOf( flags ) ;
                    InternetQueryOption( hcon, INTERNET_OPTION_SECURITY_FLAGS,
                                         @flags, buf_len
                                        ) ;
                    flags := flags + SECURITY_FLAG_IGNORE_UNKNOWN_CA      +
                                     SECURITY_FLAG_IGNORE_CERT_CN_INVALID +
                                     SECURITY_FLAG_IGNORE_CERT_DATE_INVALID ;

                    InternetSetOption( hcon, INTERNET_OPTION_SECURITY_FLAGS,
                                       Pointer(flags), sizeOf(flags)
                                      ) ;
                    hdoc := InternetOpenUrl( hcon, PChar( surl ), nil, 0,
                                             INTERNET_FLAG_IGNORE_CERT_CN_INVALID, 0
                                            ) ;
                  end ;

                  if hdoc = nil then
                    if ( GetLastError and ERROR_INTERNET_TIMEOUT )= ERROR_INTERNET_TIMEOUT then begin
                      _responseStatus := GIS_HTTP_FETCH_TIMEOUT ;
                      exit ;
                    end;
                  if hdoc = nil then
                    if ( GetLastError and ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED )=
                       ERROR_INTERNET_CLIENT_AUTH_CERT_NEEDED then begin
                      _responseStatus := GIS_HTTP_AUTHORIZATIONREQUIRED ;
                      exit ;
                    end;
                 end ;

                 // verify response code
                 res_len := 512 ;
                 res_idx := 0  ;
                 SetLength( res_buf, res_len ) ;

                 if not HttpQueryInfoA(
                      hdoc,
                      HTTP_QUERY_STATUS_CODE,
                      @res_buf[0], res_len, res_idx
                    ) then
                 begin
                   _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
                   exit ;
                 end;

                 try
                   _responseStatus := StrToInt( ConvertAnsiString( res_buf, res_len ) ) ;
                 except
                   _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
                   exit ;
                 end;

                 // get content type
                 res_len := 512 ;
                 res_idx := 0  ;
                 SetLength( res_buf, res_len ) ;

                 if not HttpQueryInfoA(
                      hdoc,
                      HTTP_QUERY_CONTENT_TYPE,
                      @res_buf[0], res_len, res_idx
                    ) then
                 begin
                   _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
                   exit ;
                 end;
                 _responseContentType := ConvertAnsiString( res_buf, res_len ) ;

                 // get expires
                 _responseContentExpires := httpDate( '' ) ; // default expiration
                 res_len := 512 ;
                 res_idx := 0  ;
                 SetLength( res_buf, res_len ) ;

                 if HttpQueryInfoA(
                      hdoc,
                      HTTP_QUERY_EXPIRES,
                      @res_buf[0], res_len, res_idx
                    ) then
                 begin
                   _responseContentExpires
                      := httpDate( ConvertAnsiString( res_buf, res_len ) ) ;
                 end ;

                 // get cache-control
                 res_len := 512 ;
                 res_idx := 0  ;
                 SetLength( res_buf, res_len ) ;

                 if HttpQueryInfoA(
                      hdoc,
                      HTTP_QUERY_CACHE_CONTROL,
                      @res_buf[0], res_len, res_idx
                    ) then
                 begin
                   if ConvertAnsiString( res_buf, res_len ) = 'no-cache' then
                     _responseContentExpires := httpDate( '0' ) ;
                 end;

                 // get content type
                 res_len := 2048 ;
                 res_idx := 0  ;
                 SetLength( res_buf, res_len ) ;

                 if not HttpQueryInfoA(
                      hdoc,
                      HTTP_QUERY_RAW_HEADERS_CRLF,
                      @res_buf[0], res_len, res_idx
                    ) then
                 begin
                   _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
                   exit ;
                 end;
                 _responseHeaders := ConvertAnsiString( res_buf, res_len ) ;


                 // get content length
                 res_len := 512 ;
                 res_idx := 0  ;
                 SetLength( res_buf, res_len ) ;

                 if not HttpQueryInfoA(
                      hdoc,
                      HTTP_QUERY_CONTENT_LENGTH,
                      @res_buf[0], res_len, res_idx
                    ) then
                 begin
                   _responseContentLength := 0 ;
                 end
                 else begin
                   try
                     _responseContentLength := StrToInt( ConvertAnsiString( res_buf, res_len ) ) ;
                   except
                     _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
                     exit ;
                   end;
                 end ;

              if hdoc = nil then exit ;
              repeat
                res := InternetReadFile( hdoc, @buf[0], High( buf ), total);
                _responseStream.Write( buf[0], total ) ;
              until ( not res ) or ( total <= 0 ) ;
            finally
              if hrequest <> nil then
                InternetCloseHandle( hrequest ) ;
              if hrequest <> hdoc then
                InternetCloseHandle( hdoc ) ;
            end ;
          except
            // do nothing
            _responseStatus := GIS_HTTP_FETCH_INTERNALERROR ;
          end ;
        finally
          if hcon <> nil then
            InternetCloseHandle( hcon ) ;
        end ;
      end ;
    {$ELSE}
      procedure HttpFetchStream(
          const _url                    : String   ;
          const _options                : TGIS_HttpSettings ;
            var _responseStatus         : Integer  ;
            var _responseContentLength  : Int64    ;
            var _responseContentType    : String   ;
            var _responseContentExpires : TDateTime ;
            var _responseHeaders        : String    ;
            var _responseStream         : TStream
      ) ;
      var
        ohttp    : TIdHTTP ;
        lhandler : TIdSSLIOHandlerSocketOpenSSL ;
      begin
        if IsStringEmpty( _url ) then
          exit ; // we only zeroed result

        if not assigned( _responseStream ) then
          _responseStream := TMemoryStream.Create ;

        ohttp := TIdHTTP.Create(nil) ;
        try
          lhandler := TIdSSLIOHandlerSocketOpenSSL.Create(nil) ;
          ohttp.IOHandler := lhandler ;

          if not IsStringEmpty( _options.User ) then begin
            ohttp.Request.BasicAuthentication := True ;
            ohttp.Request.Username  := _options.User ;
            ohttp.Request.Password  := _options.Pass ;
          end ;
          ohttp.Request.UserAgent := _options.Agent ;
          ohttp.Request.Referer   := _options.Referer ;
          ohttp.ReadTimeout       := _options.Timeout ;
          ohttp.HandleRedirects   := True ;

          if not _options.UseCache then
            ohttp.Request.Pragma := 'no-cache' ;

          ohttp.Request.AcceptEncoding := 'gzip' ;

          try
            ohttp.Get( _url, _responseStream ) ;

            _responseStatus        := ohttp.ResponseCode ;
            _responseContentLength := ohttp.Response.ContentStream.Size ;
            _responseContentType   := ohttp.Response.ContentType ;

            _responseHeaders := ohttp.Response.RawHeaders.Text ;
            _responseContentExpires := DateTimeToUTC( ohttp.Response.Expires ) ;


            if ohttp.Response.CacheControl = 'no-cache' then
              _responseContentExpires := httpDate( '0' ) ;

          except
            on e : EIdHTTPProtocolException do
              _responseStatus := ohttp.ResponseCode
            else begin
              _responseStatus := ohttp.ResponseCode ;
              raise ;
            end ;
          end;
        finally
          FreeObject( lhandler ) ;
          FreeObject( ohttp ) ;
        end ;

      end;
    {$ENDIF}
  {$ENDIF}
{$ENDREGION}

{$REGION 'TGIS_Timer'}
function TGIS_Timer.fget_NativeTimer
  : TObject ;
begin
  Result := FTimer.NativeTimer ;
end;

function TGIS_Timer.fget_Enabled
  : Boolean ;
begin
  Result := FTimer.Enabled ;
end;

procedure TGIS_Timer.fset_Enabled(
  const _value : Boolean
) ;
begin
  FTimer.Enabled := _value ;
end;

function TGIS_Timer.fget_Interval
  : Cardinal ;
begin
  Result := FTimer.Interval ;
end;

procedure TGIS_Timer.fset_Interval(
  const _value : Cardinal
) ;
begin
  FTimer.Interval := _value ;
end;

function TGIS_Timer.fget_OnTimer
  : TNotifyEvent ;
begin
  Result := FTimer.OnTimer ;
end;

procedure TGIS_Timer.fset_OnTimer(
  const _value : TNotifyEvent
) ;
begin
  FTimer.OnTimer := _value ;
end;

procedure TGIS_Timer.doDestroy ;
begin
  FreeObject( FTimer ) ;
  inherited ;
end;

constructor TGIS_Timer.Create ;
begin
  FTimer := TimerHelper.DoCreate
end;
{$ENDREGION}

{$REGION TGIS_Hijack}
{$IFDEF MSWINDOWS}
  function TGIS_Hijack.getActualAddr(
    const _proc : Pointer
  ) : Pointer ;
  begin
    Result := nil ;

    if _proc = nil then
      exit ;

    if ( Win32Platform = VER_PLATFORM_WIN32_NT ) and
       ( PAbsoluteIndirectJmp(_proc)^.OpCode = $25FF )
    then
     {$IFDEF CPUX64}
       Result := PPointer(
                   NativeInt(_proc) +
                   PAbsoluteIndirectJmp(_proc)^.Addr +
                   SizeOf( TAbsoluteIndirectJmp)
                 )^
     {$ELSE}
        Result := PPointer( PAbsoluteIndirectJmp(_proc)^.Addr )^
     {$ENDIF}
    else
      Result := _proc;
  end;

  constructor TGIS_Hijack.Create(
    const _oldproc : Pointer ;
    const _newproc : Pointer
  ) ;
  var
    nsize   : NativeUInt ;
    newcode : TCode      ;
  begin
    oldProc := getActualAddr(_oldproc);

    if oldProc = nil then begin
      newcode.Jump := 0 ;
      newcode.Offset := 0 ;
      exit ;
    end ;

    if ReadProcessMemory(
         GetCurrentProcess,
         oldProc,
         @oldCode, SizeOf(oldCode),
         nsize
       )
    then begin
      newcode.Jump := $E9;
      newcode.Offset := IntPtr(_newproc) - IntPtr(oldProc) - SizeOf(newcode);
      WriteProcessMemory(
        GetCurrentProcess,
        oldProc,
        @newcode,
        SizeOf(newcode),
        nsize
      );
    end;
  end;

  destructor TGIS_Hijack.Destroy ;
  var
    nsize   : NativeUInt ;
  begin
    if ( oldCode.Jump = 0 ) or ( oldProc = nil ) then
      exit ;

    WriteProcessMemory(
      GetCurrentProcess,
      oldProc,
      @oldCode,
      SizeOf(oldCode),
      nsize
    );

    inherited ;
  end;
{$ENDIF}
{$ENDREGION}

{$REGION 'Math methods speedy version'}

function RoundS(
  const _val : Extended
) : Int64 ;
begin
  {$IF not defined(CPUARM) and defined(IOS)}
    // only for iOS Simulator
    Result := llrint_posix( _val ) ;
  {$ELSE}
    Result := Round( _val ) ;
  {$IFEND}
end;

function TruncS(
  const _val : Double
) : Int64 ;
begin
  {$IF defined(IOS) or defined(ANDROID)}
    Result := RoundS( trunc_posix( _val ) ) ;
  {$ELSE}
    Result := Trunc( _val ) ;
  {$IFEND}
end;

function FloorS(
  const _val : Double
) : Integer  ;
begin
  {$IF defined(IOS) or defined(OSX) or defined(ANDROID)}
    Result := RoundS( floor_posix( _val ) ) ;
  {$ELSE}
    Result := Floor( _val ) ;
  {$IFEND}
end;

function CeilS(
  const _val : Double
) : Integer  ;
begin
  {$IF defined(IOS) or defined(OSX) or defined(ANDROID)}
    Result := RoundS( ceil_posix( _val ) ) ;
  {$ELSE}
    Result := Ceil( _val ) ;
  {$IFEND}
end;

function IsWin11 : Boolean ;
begin
  {$IFDEF MSWINDOWS}
    Result := ( Win32MajorVersion >= 10 ) and ( Win32BuildNumber >= 22000 ) ;
  {$ELSE}
    Result := False ;
  {$ENDIF}
end;

{$ENDREGION}

{$IFDEF MSWINDOWS}
  function IsMetricSystem : Boolean ;
  var
    buf   : PChar;
    hlcid : LCID;
  begin
    buf := StrAlloc(1024);
    hlcid := GetUserDefaultLCID;
    GetLocaleInfo(hlcid, LOCALE_IMEASURE, buf, 1024);
    Result := buf = '0' ;
    StrDispose( buf );
  end ;
{$ENDIF}
{$IFDEF ANDROID}
  function IsMetricSystem : Boolean ;
  var
    str    : String ;
    locale : JLocale;
  begin
    locale := TJLocale.JavaClass.getDefault ;

    str := JStringToString(locale.getISO3Country) ;
    if      str = 'usa' then Result := False
    else if str = 'mmr' then Result := False
    else if str = 'lbr' then Result := False
    else                     Result := True ;
  end ;
{$ENDIF}
{$IFDEF MACOSX}
  function IsMetricSystem : Boolean ;
  var
    str : String ;
  begin
    str := NSStrToStr( TNSLocale.Wrap( TNSLocale.OCClass.currentLocale ).localeIdentifier ) ;
    if      str = 'en_US' then Result := False
    else if str = 'en_MM' then Result := False
    else if str = 'en_LR' then Result := False
    else                       Result := True ;
  end ;
{$ENDIF}
{$IFDEF IOS}
  function IsMetricSystem : Boolean ;
  var
    str : String ;
  begin
    str := NSStrToStr( TNSLocale.Wrap( TNSLocale.OCClass.currentLocale ).localeIdentifier ) ;
    if      str = 'en_US' then Result := False
    else if str = 'en_MM' then Result := False
    else if str = 'en_LR' then Result := False
    else                       Result := True ;
  end ;
{$ENDIF}
{$IFDEF LINUX}
  function IsMetricSystem : Boolean ;
  begin
   {$MESSAGE WARN '### VerifyLinux code ## '}
    Result := True ;
  end ;
{$ENDIF}

function GetTickCount : Cardinal ;
begin
  {$IFNDEF LEVEL_XE3_RTL}
    {$IFDEF MSWINDOWS}
      Result := Winapi.Windows.GetTickCount ;
    {$ELSE}
      Result := 0 ; // not compiled FMX
    {$ENDIF}
  {$ELSE}
    Result := TThread.GetTickCount ;
  {$ENDIF}
end;

function GetRandom(
  const _range : Integer
) : Integer ;
begin
  Result := Random( _range ) ;
end;

function GetRandom : Double ;
begin
  Result := Random() ;
end;

function CompareDateTime(
  const _d1 : TDateTime ;
  const _d2 : TDateTime
) : Integer ;
begin
  if _d1 > _d2 then
    Result := 1
  else if _d1 < _d2 then
    Result := -1
  else
    Result := 0 ;
end;

{$IFDEF MSWINDOWS}
  function IsWine
    : Boolean;
  var
    hdll : HMODULE ;
  begin
    Result := False;

    hdll := LoadLibrary('ntdll.dll');
    if hdll <> 0 then begin
      Result := Assigned( GetProcAddress( hdll, 'wine_get_version'));
      FreeLibrary( hdll );
    end;
  end;
{$ENDIF}

initialization
  dotFloatSettings := TFormatSettings.Create ; ;
  dotFloatSettings.DecimalSeparator := '.' ;


end.

