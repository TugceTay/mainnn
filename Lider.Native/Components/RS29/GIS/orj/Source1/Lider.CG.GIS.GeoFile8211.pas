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
  Internal part of ISO 8211 implementation.

  This unit was partially based on ISO 8211 Access Implementation:

  Copyright (c) 1999, Frank Warmerdam

  Permission is hereby granted, free of charge, to any person obtaining a
  Copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, Copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  Sublicensing of this unit is a subject of TatukGIS Developer
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFile8211 ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFile8211"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoStreams ;
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
  ///   ISO 8211 data types.
  /// </summary>
  TGIS_DDFDataType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Integer type.
    /// </summary>
    Int,
    /// <summary>
    ///   Floating point type.
    /// </summary>
    Float,
    /// <summary>
    ///   String type.
    /// </summary>
    &String,
    /// <summary>
    ///   Binary string type.
    /// </summary>
    BinaryString
  ) ;

  TGIS_DDFFieldDefn    = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_DDFSubfieldDefn = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_DDFRecord       = {$IFDEF OXYGENE} public {$ENDIF} class ;
  TGIS_DDFField        = {$IFDEF OXYGENE} public {$ENDIF} class ;

  /// <summary>
  ///   ISO 8211 module.
  /// </summary>
  TGIS_DDFModule = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      fpDDF              : TGIS_BufferedFileStream ;

      nFirstRecordOffset : LongInt ;

      interchangeLevel   : Byte ;
      inlineCodeExtIn    : Byte ;
      versionNumber      : Byte ;
      appIndicator       : Byte ;
      leaderIden         : Byte ;
      fieldControlLength : Byte ;
      extendedCharSet    : array[0..4] of Byte ;

      recLength          : LongInt ;
      fieldAreaStart     : LongInt ;
      sizeFieldLength    : LongInt ;
      sizeFieldPos       : LongInt ;
      sizeFieldTag       : LongInt ;

      fieldDefnCount     : Integer ;
      aFieldDefns        : array of TGIS_DDFFieldDefn ;
      oRecord            : TGIS_DDFRecord ;
      cloneCount         : Integer ;
      maxCloneCount      : Integer ;
      aClones            : array of TGIS_DDFRecord ;
    protected
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create         ;

      /// <summary>
      ///   Opens a file.
      /// </summary>
      /// <param name="_sFilename">
      ///   path to the file
      /// </param>
      /// <returns>
      ///   True if the operation succeeded
      /// </returns>
      function  Open             ( const _sFilename  : String
                                 ) : Boolean ;
      /// <summary>
      ///   Closes the file.
      /// </summary>
      procedure Close            ;

      /// <summary>
      ///   Reads a record from the file.
      /// </summary>
      /// <returns>
      ///   obtained record
      /// </returns>
      function  ReadRecord       : TGIS_DDFRecord ;

      /// <summary>
      ///   Gets the control length of the field.
      /// </summary>
      /// <returns>
      ///   control length of the field
      /// </returns>
      function  GetFieldControlLength : Integer ;

      /// <summary>
      ///   Jumps to the beginning of the file.
      /// </summary>
      procedure Rewind           ; overload;

      /// <summary>
      ///   Jumps to the beginning of the file.
      /// </summary>
      /// <param name="_nOffset">
      ///   offset from the beginning of the file
      /// </param>
      procedure Rewind           ( const _nOffset    : Int32
                                 ) ; overload;

      /// <summary>
      ///   Searches for a field definition.
      /// </summary>
      /// <param name="_sFieldName">
      ///   name of the field
      /// </param>
      /// <returns>
      ///   retrieved field
      /// </returns>
      function  FindFieldDefn    ( const _sFieldName : String
                                 ) : TGIS_DDFFieldDefn ;

      /// <summary>
      ///   Gets the number of fields.
      /// </summary>
      /// <returns>
      ///   number of fields
      /// </returns>
      function  GetFieldCount    : Integer ;

      /// <summary>
      ///   Retrieves a field by its index.
      /// </summary>
      /// <param name="_i">
      ///   index of the field
      /// </param>
      /// <returns>
      ///   retrieved field
      /// </returns>
      function  GetField         ( const _i : Integer
                                 ) : TGIS_DDFFieldDefn ;

      /// <summary>
      ///   Adds a new field.
      /// </summary>
      /// <param name="_poNewFDefn">
      ///   field to be added
      /// </param>
      procedure AddField         ( const _poNewFDefn : TGIS_DDFFieldDefn
                                 ) ;

      /// <summary>
      ///   Adds a clone of a record.
      /// </summary>
      /// <param name="_record">
      ///   record to be added
      /// </param>
      procedure AddCloneRecord   ( const _record    : TGIS_DDFRecord
                                 ) ;

      /// <summary>
      ///   Removes a clone of a record.
      /// </summary>
      /// <param name="_record">
      ///   record to be removed
      /// </param>
      procedure RemoveCloneRecord( const _record    : TGIS_DDFRecord
                                 ) ;

      /// <summary>
      ///   Retrieves an integer value from a buffer.
      /// </summary>
      /// <param name="_bufData">
      ///   source buffer
      /// </param>
      /// <param name="_offset">
      ///   buffer offset
      /// </param>
      /// <param name="_maxChars">
      ///   maximum number of characters to read
      /// </param>
      /// <returns>
      ///   retrieved integer
      /// </returns>
      function  DDFScanInt       ( const _bufData    : TBytes ;
                                   const _offset     : Integer ;
                                   const _maxChars   : Integer
                                 ) : LongInt ;

      /// <summary>
      ///   Retrieves a string value from a buffer.
      /// </summary>
      /// <param name="_bufData">
      ///   source buffer
      /// </param>
      /// <param name="_offset">
      ///   buffer offset
      /// </param>
      /// <param name="_maxChars">
      ///   maximum number of characters to read
      /// </param>
      /// <returns>
      ///   retrieved string
      /// </returns>
      function  DDFScanString    ( const _bufData    : TBytes ;
                                   const _offset     : Integer ;
                                   const _maxChars   : Integer
                                 ) : String ;

      /// <summary>
      ///   Retrieves a string value from a buffer.
      /// </summary>
      /// <param name="_record">
      ///   source buffer
      /// </param>
      /// <param name="_offset">
      ///   buffer offset
      /// </param>
      /// <param name="_maxChars">
      ///   maximum number of characters to read
      /// </param>
      /// <param name="_nDelimChar1">
      ///   first delimiting character
      /// </param>
      /// <param name="_nDelimChar2">
      ///   second delimiting character
      /// </param>
      /// <param name="_pnConsumedChars">
      ///   number of consumed characters
      /// </param>
      /// <returns>
      ///   retrieved integer
      /// </returns>
      function  DDFFetchVariable ( const _record          : TBytes ;
                                   const _offset          : Integer ;
                                   const _maxChars        : Integer ;
                                   const _nDelimChar1     : Integer ;
                                   const _nDelimChar2     : Integer ;
                                     var _pnConsumedChars : Integer
                                 ) : String ;
    public
      /// <summary>
      ///   Buffered file stream.
      /// </summary>
      property FP  : TGIS_BufferedFileStream read fpDDF ;
      /// <summary>
      ///   Current record.
      /// </summary>
      property CR  : TGIS_DDFRecord          read oRecord ;

  end ;


  /// <summary>
  ///   ISO 8211 data structures.
  /// </summary>
  TGIS_DDFDataStructCode = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Elementary structure.
    /// </summary>
    elementary,
    /// <summary>
    ///   Vector structure.
    /// </summary>
    vector,
    /// <summary>
    ///   Array structure.
    /// </summary>
    &array,
    /// <summary>
    ///   Concatenated structure.
    /// </summary>
    concatenated
  ) ;


  /// <summary>
  ///   ISO 8211 data type codes.
  /// </summary>
  TGIS_DDFDataTypeCode   = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Character string code.
    /// </summary>
    char_string,
    /// <summary>
    ///   Implicit point code.
    /// </summary>
    implicit_point,
    /// <summary>
    ///   Explicit point code.
    /// </summary>
    explicit_point,
    /// <summary>
    ///   Scaled explicit point code.
    /// </summary>
    explicit_point_scaled,
    /// <summary>
    ///   Character string bit code.
    /// </summary>
    char_bit_string,
    /// <summary>
    ///   String bit code.
    /// </summary>
    bit_string,
    /// <summary>
    ///   Mixed data type code.
    /// </summary>
    mixed_data_type
  ) ;


  /// <summary>
  ///   ISO 8211 field.
  /// </summary>
  TGIS_DDFField = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      oDefn    : TGIS_DDFFieldDefn ;
      nDataSize : Integer ;
      chData  : Integer ;
    public
      /// <summary>
      ///   Initializes the field.
      /// </summary>
      /// <param name="_poDefnIn">
      ///   field definition
      /// </param>
      /// <param name="_pachDataIn">
      ///   field data position
      /// </param>
      /// <param name="_nDataSizeIn">
      ///   field data size
      /// </param>
      procedure Initialize     ( const _poDefnIn     : TGIS_DDFFieldDefn ;
                                 const _pachDataIn   : Integer ;
                                 const _nDataSizeIn  : Integer
                               ) ;

      /// <summary>
      ///   Retrieves subfield data.
      /// </summary>
      /// <param name="_record">
      ///   record to be checked
      /// </param>
      /// <param name="_poSFDefn">
      ///   subfield definition
      /// </param>
      /// <param name="_pnMaxBytes">
      ///   size of the subfield
      /// </param>
      /// <returns>
      ///   final position
      /// </returns>
      function  GetSubfieldData ( const _record         : TGIS_DDFRecord ;
                                  const _poSFDefn       : TGIS_DDFSubfieldDefn ;
                                    var _pnMaxBytes     : Integer
                                ) : Integer ; overload;

      /// <summary>
      ///   Retrieves subfield data.
      /// </summary>
      /// <param name="_record">
      ///   record to be checked
      /// </param>
      /// <param name="_poSFDefn">
      ///   subfield definition
      /// </param>
      /// <param name="_pnMaxBytes">
      ///   size of the subfield
      /// </param>
      /// <param name="_subfieldIndex">
      ///   subfield index
      /// </param>
      /// <returns>
      ///   final position
      /// </returns>
      function  GetSubfieldData ( const _record         : TGIS_DDFRecord ;
                                  const _poSFDefn       : TGIS_DDFSubfieldDefn ;
                                    var _pnMaxBytes     : Integer ;
                                  const _subfieldIndex  : Integer
                                ) : Integer ; overload;

      /// <summary>
      ///   Retrieves the number of occurrences of a specific record.
      /// </summary>
      /// <param name="_record">
      ///   record to be checked
      /// </param>
      /// <returns>
      ///   number of occurrences
      /// </returns>
      function GetRepeatCount   ( const _record  : TGIS_DDFRecord
                                ) : Integer ;
    public

      /// <summary>
      ///   Data position.
      /// </summary>
      property Data      : Integer             read chData ;

      /// <summary>
      ///   Data size.
      /// </summary>
      property DataSize  : Integer             read nDataSize ;

      /// <summary>
      ///   Field definition.
      /// </summary>
      property FieldDefn : TGIS_DDFFieldDefn   read oDefn ;
  end ;


  /// <summary>
  ///   ISO 8211 record.
  /// </summary>
  TGIS_DDFRecord = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oModule           : TGIS_DDFModule  ;
      nReuseHeader      : Boolean ;
      fieldOffset       : Integer ;
      sizeFieldTag      : Integer ;
      sizeFieldPos      : Integer ;
      sizeFieldLength   : Integer ;
      nDataSize          : Integer ;
      chData            : TBytes ;
      nFieldCount        : Integer ;
      aFields           : array of TGIS_DDFField ;
      bIsClone          : Boolean ;
    private
      function readHeader : Boolean ;
    protected
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_poModuleIn">
      ///   source module
      /// </param>
      constructor Create          ( const _poModuleIn : TGIS_DDFModule
                                  ) ;
    public

      /// <summary>
      ///   Searches for a field.
      /// </summary>
      /// <param name="_sName">
      ///   field name
      /// </param>
      /// <param name="_fieldIndex">
      ///   field index
      /// </param>
      /// <returns>
      ///   retrieved field
      /// </returns>
      function  FindField         ( const _sName        : String ;
                                    const _fieldIndex     : Integer
                                  ) : TGIS_DDFField ;

      /// <summary>
      ///   Gets a field by its index.
      /// </summary>
      /// <param name="_i">
      ///   field index
      /// </param>
      /// <returns>
      ///   retrieved field
      /// </returns>
      function  GetField          ( const _i              : Integer
                                  ) : TGIS_DDFField ;

      /// <summary>
      ///   Gets an integer value of a subfield.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_fieldIndex">
      ///   field index
      /// </param>
      /// <param name="_subfield">
      ///   subfield name
      /// </param>
      /// <param name="_subfieldIndex">
      ///   subfield index
      /// </param>
      /// <returns>
      ///   retrieved integer value
      /// </returns>
      function  GetIntSubfield    ( const _field          : String ;
                                    const _fieldIndex     : Integer ;
                                    const _subfield       : String ;
                                    const _subfieldIndex  : Integer
                                  ) : Integer ;
      /// <summary>
      ///   Gets a floating point value of a subfield.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_fieldIndex">
      ///   field index
      /// </param>
      /// <param name="_subfield">
      ///   subfield name
      /// </param>
      /// <param name="_subfieldIndex">
      ///   subfield index
      /// </param>
      /// <returns>
      ///   retrieved floating point value
      /// </returns>
      function  GetFloatSubfield  ( const _field          : String ;
                                    const _fieldIndex     : Integer ;
                                    const _subfield       : String ;
                                    const _subfieldIndex  : Integer
                                  ) : Double ;

      /// <summary>
      ///   Gets a string value of a subfield.
      /// </summary>
      /// <param name="_field">
      ///   field name
      /// </param>
      /// <param name="_fieldIndex">
      ///   field index
      /// </param>
      /// <param name="_subfield">
      ///   subfield name
      /// </param>
      /// <param name="_subfieldIndex">
      ///   subfield index
      /// </param>
      /// <returns>
      ///   retrieved string value
      /// </returns>
      function  GetStringSubfield ( const _field         : String ;
                                    const _fieldIndex    : Integer ;
                                    const _subfield      : String ;
                                    const _subfieldIndex : Integer
                                  ) : String ;

      /// <summary>
      ///   Reads the record from the source module.
      /// </summary>
      /// <returns>
      ///   True if read was successful
      /// </returns>
      function  Read              : Boolean ;

      /// <summary>
      ///   Removes all the fields from the record.
      /// </summary>
      procedure Clear             ;

      /// <summary>
      ///   Clones the record.
      /// </summary>
      /// <returns>
      ///   clone of the record
      /// </returns>
      function  Clone             : TGIS_DDFRecord  ;
    public

      /// <summary>
      ///   Data size.
      /// </summary>
      property DataSize   : Integer        read nDataSize ;

      /// <summary>
      ///   Data.
      /// </summary>
      property Data       : TBytes         read chData ;

      /// <summary>
      ///   Source module.
      /// </summary>
      property DDFModule  : TGIS_DDFModule read oModule ;

      /// <summary>
      ///   Number of fields.
      /// </summary>
      property FieldCount : Integer        read nFieldCount ;
  end ;


  /// <summary>
  ///   ISO 8211 field definition.
  /// </summary>
  TGIS_DDFFieldDefn = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oModule             : TGIS_DDFModule ;
      pszTag              : String ;
      fieldName           : String ;
      arrayDescr          : String ;
      formatControls      : String ;
      bRepSubfields       : Boolean ;
      nFixedWidth         : Integer ;
      data_struct_code    : TGIS_DDFDataStructCode ;
      data_type_code      : TGIS_DDFDataTypeCode   ;
      nSubfieldCount      : Integer ;
      aSubfields          : array of TGIS_DDFSubfieldDefn ;
    private
      function buildSubfields : Boolean ;
      function applyFormats   : Boolean ;
      function expandFormat    ( const _sSrc : String ) : String ;
      function GetSubstring    ( const _sSrc : String ) : String ;
    protected
      procedure doDestroy ; override;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Initializes the field definition.
      /// </summary>
      /// <param name="_module">
      ///   source module
      /// </param>
      /// <param name="_sTag">
      ///   field tag
      /// </param>
      /// <param name="_nSize">
      ///   field size
      /// </param>
      /// <param name="_record">
      ///   record
      /// </param>
      /// <param name="_offset">
      ///   offset
      /// </param>
      /// <returns>
      ///   True if succeeded
      /// </returns>
      function Initialize       ( const _module    : TGIS_DDFModule ;
                                  const _sTag    : String ;
                                  const _nSize     : Integer ;
                                  const _record    : TBytes  ;
                                  const _offset    : Integer
                                ) : Boolean ;

      /// <summary>
      ///   Adds subfield definition to the field definition.
      /// </summary>
      /// <param name="_poNewSFDefn">
      ///   subfield definition
      /// </param>
      /// <param name="_bDontAddToFormat">
      ///  unused; must be True
      /// </param>
      procedure AddSubfield     ( const _poNewSFDefn      : TGIS_DDFSubfieldDefn ;
                                  const _bDontAddToFormat : Boolean
                                ) ;

      /// <summary>
      ///   Retrieves a subfield definition by its index.
      /// </summary>
      /// <param name="_i">
      ///   index of the subfield
      /// </param>
      /// <returns>
      ///   retrieved subfield definition
      /// </returns>
      function GetSubfield      ( const _i : Integer
                                ) : TGIS_DDFSubfieldDefn ;

      /// <summary>
      ///   Searches for a subfield definition.
      /// </summary>
      /// <param name="_sMnemonic">
      ///   name of the subfield definition
      /// </param>
      /// <returns>
      ///   retrieved subfield definition
      /// </returns>
      function FindSubfieldDefn ( const _sMnemonic : String
                                ) : TGIS_DDFSubfieldDefn ;
    public

      /// <summary>
      ///   Number of subfields.
      /// </summary>
      property SubfieldCount : Integer read nSubfieldCount ;

      /// <summary>
      ///   Fixed width.
      /// </summary>
      property FixedWidth    : Integer read nFixedWidth;

      /// <summary>
      ///   Name of the field.
      /// </summary>
      property Name          : String  read pszTag ;

      /// <summary>
      ///   True if there are repeating subfields
      ///   (many occurrences of the same subfield).
      /// </summary>
      property IsRepeating   : Boolean read bRepSubfields write bRepSubfields ;
  end ;


  /// <summary>
  ///   ISO 8211 binary format types.
  /// </summary>
  TGIS_DDFBinaryFormat = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Data is not in binary format.
    /// </summary>
    NotBinary    = 0,
    /// <summary>
    ///   Unsigned integer.
    /// </summary>
    UInt         = 1,
    /// <summary>
    ///   Signed integer.
    /// </summary>
    SInt         = 2,
    /// <summary>
    ///   Real Floating point number.
    /// </summary>
    FPReal       = 3,
    /// <summary>
    ///   Real floating point number.
    /// </summary>
    FloatReal    = 4,
    /// <summary>
    ///   Complex floating point number.
    /// </summary>
    FloatComplex = 5
  ) ;


  /// <summary>
  ///   ISO 8211 subfield definition.
  /// </summary>
  TGIS_DDFSubfieldDefn = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      oModule          : TGIS_DDFModule ;
      pszName           : String ;
      pszFormatString   : String ;
      bIsVariable       : Boolean ;
      eType             : TGIS_DDFDataType ;
      chFormatDelimeter : Byte ;
      nFormatWidth      : Integer ;
      eBinaryFormat     : TGIS_DDFBinaryFormat ;
      pachBuffer        : TBytes ;
      nMaxBufChars      : Integer ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function getDataLength    ( const _sourceData      : TBytes ;
                                  const _maxBytes        : Integer ;
                                    var _consumedBytes   : Integer
                                ) : Integer ;
    protected
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_module">
      ///   source module
      /// </param>
      constructor Create         ( const _module         : TGIS_DDFModule
                                 ) ;

      /// <summary>
      ///   Sets the name.
      /// </summary>
      /// <param name="_newName">
      ///   new name
      /// </param>
      procedure SetName          ( const _newName        : String
                                 ) ;

      /// <summary>
      ///   Sets the format.
      /// </summary>
      /// <param name="_sFormat">
      ///   format identifier
      /// </param>
      /// <returns>
      ///   True if successful
      /// </returns>
      function  SetFormat        ( const _sFormat      : String
                                 ) : Boolean ;

      /// <summary>
      ///   Retrieves string data from a source buffer.
      /// </summary>
      /// <param name="_sourceData">
      ///   source buffer
      /// </param>
      /// <param name="_maxBytes">
      ///   maximum number of bytes to be read
      /// </param>
      /// <param name="_consumedBytes">
      ///   number of consumed bytes
      /// </param>
      /// <returns>
      ///   retrieved string data
      /// </returns>
      function GetStringData     ( const _sourceData     : TBytes ;
                                   const _maxBytes       : Integer ;
                                     var _consumedBytes  : Integer
                                 ) : TBytes ;

      /// <summary>
      ///   Retrieves floating point data from a source buffer.
      /// </summary>
      /// <param name="_sourceData">
      ///   source buffer
      /// </param>
      /// <param name="_maxBytes">
      ///   maximum number of bytes to be read
      /// </param>
      /// <param name="_consumedBytes">
      ///   number of consumed bytes
      /// </param>
      /// <returns>
      ///   retrieved floating point data
      /// </returns>
      function GetFloatData      ( const _sourceData     : TBytes ;
                                   const _maxBytes       : Integer ;
                                     var _consumedBytes  : Integer
                                 ) : Double ;

      /// <summary>
      ///   Retrieves integer data from a source buffer.
      /// </summary>
      /// <param name="_sourceData">
      ///   source buffer
      /// </param>
      /// <param name="_maxBytes">
      ///   maximum number of bytes to be read
      /// </param>
      /// <param name="_consumedBytes">
      ///   number of consumed bytes
      /// </param>
      /// <returns>
      ///   retrieved integer data
      /// </returns>
      function GetIntData        ( const _sourceData     : TBytes ;
                                   const _maxBytes       : Integer ;
                                     var _consumedBytes  : Integer
                                 ) : Integer ;
    public

      /// <summary>
      ///   Format width.
      /// </summary>
      property Width     : Integer          read nFormatWidth ;

      /// <summary>
      ///   Subfield name.
      /// </summary>
      property Name      : String           read pszName ;

      /// <summary>
      ///   Data type.
      /// </summary>
      property DataType  : TGIS_DDFDataType read eType ;

      /// <summary>
      ///   Format.
      /// </summary>
      property Format    : String           read pszFormatString ;
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals ;
{$ENDIF}


const
  DDF_FIELD_TERMINATOR  = 30 ;
  DDF_UNIT_TERMINATOR   = 31 ;

//==============================================================================
// TGIS_DDFModule
//==============================================================================

  constructor TGIS_DDFModule.Create ;
  begin
    inherited;

    fieldDefnCount := 0;
    aFieldDefns  := nil;
    oRecord        := nil;

    aClones      := nil;
    cloneCount     := 0 ;
    maxCloneCount  := 0 ;

    fpDDF           := nil;

    interchangeLevel := 0;
    inlineCodeExtIn  := 0;
    versionNumber    := 0;
    appIndicator     := 0;
    fieldControlLength := 0;

    extendedCharSet[0] := ord(' ') ;
    extendedCharSet[1] := ord('!') ;
    extendedCharSet[2] := ord(' ') ;

    recLength        := 0;
    leaderIden       := ord('L');
    fieldAreaStart   := 0;
    sizeFieldLength  := 0;
    sizeFieldPos     := 0;
    sizeFieldTag     := 0;
  end ;

  procedure TGIS_DDFModule.doDestroy ;
  begin
    Close ;

    inherited ;
  end ;

  function TGIS_DDFModule.Open(
    const _sFilename : String
  ) : Boolean;
  const
    nLeaderSize = 24 ;
  var
    achLeader         : TBytes ;
    pachRecord        : TBytes ;
    i                 : Integer ;
    bValid            : Boolean ;
    nFieldEntryWidth  : Integer ;
    nFDCount          : Integer ;
    szTag             : String ;
    nEntryOffset      : Integer ;
    nFieldLength      : Integer ;
    nFieldPos         : Integer ;
    poFDefn           : TGIS_DDFFieldDefn ;
  begin
    if fpDDF <> nil then
      Close ;

    fpDDF := TGIS_BufferedFileStream.Create( _sFilename, TGIS_StreamMode.Read ) ;

    if fpDDF = nil then begin
      Result := False ;
      exit ;
    end ;

    SetLength( achLeader, nLeaderSize ) ;

    if fpDDF.ReadBytesCnt( achLeader, nLeaderSize ) <> nLeaderSize then begin
      FreeObject( fpDDF ) ;
      Result := False ;
      exit ;
    end ;

    bValid := True ;

    for i := 0 to nLeaderSize-1 do
      if ( achLeader[i] < 32 ) or ( achLeader[i] > 126 ) then
        bValid := False ;

    if ( achLeader[5] <> ord('1') ) and
       ( achLeader[5] <> ord('2') ) and
       ( achLeader[5] <> ord('3') ) then
      bValid := False ;

    if achLeader[6] <> ord('L') then
      bValid := False ;
    if ( achLeader[8] <> ord('1') ) and ( achLeader[8] <> ord(' ') ) then
      bValid := False ;

    if ( bValid ) then begin
      recLength                   := DDFScanInt( achLeader, 0, 5 );
      interchangeLevel            := achLeader[5];
      leaderIden                  := achLeader[6];
      inlineCodeExtIn             := achLeader[7];
      versionNumber               := achLeader[8];
      appIndicator                := achLeader[9];
      fieldControlLength          := DDFScanInt(achLeader,10,2);
      fieldAreaStart              := DDFScanInt(achLeader,12,5);
      extendedCharSet[0]          := achLeader[17];
      extendedCharSet[1]          := achLeader[18];
      extendedCharSet[2]          := achLeader[19];
      extendedCharSet[3]          := 0;
      sizeFieldLength             := DDFScanInt(achLeader,20,1);
      sizeFieldPos                := DDFScanInt(achLeader,21,1);
      sizeFieldTag                := DDFScanInt(achLeader,23,1);

      if ( recLength < 12      ) or ( fieldControlLength = 0 ) or
         ( fieldAreaStart < 24 ) or ( sizeFieldLength = 0    ) or
         ( sizeFieldPos = 0    ) or ( sizeFieldTag = 0       ) then
          bValid := False ;
    end ;

    if not bValid then begin
      FreeObject( fpDDF ) ;
      Result := False ;
      exit ;
    end ;

    SetLength( pachRecord, recLength ) ;

    GisCopyMemory( achLeader, 0, pachRecord, 0, nLeaderSize ) ;

    if ( fpDDF.ReadBytesCnt( pachRecord, recLength-nLeaderSize, nLeaderSize ) <>
       ( recLength - nLeaderSize ) ) then begin
      Result := False ;
      exit ;
    end ;

    nFDCount         := 0 ;
    nFieldEntryWidth := sizeFieldLength + sizeFieldPos + sizeFieldTag ;

    i := nLeaderSize ;
    while i < recLength do begin
      if ( pachRecord[i] = DDF_FIELD_TERMINATOR ) then break ;

      inc( nFDCount ) ;
      inc( i, nFieldEntryWidth ) ;
    end ;

    for i := 0 to nFDCount - 1 do begin
      nEntryOffset := nLeaderSize + i*nFieldEntryWidth ;

      szTag := DDFScanString( pachRecord, nEntryOffset, sizeFieldTag ) ;

      inc( nEntryOffset, sizeFieldTag ) ;
      nFieldLength := DDFScanInt( pachRecord, nEntryOffset, sizeFieldLength );

      inc( nEntryOffset, sizeFieldLength );
      nFieldPos := DDFScanInt( pachRecord, nEntryOffset, sizeFieldPos );

      poFDefn := TGIS_DDFFieldDefn.Create ;
      try
        if poFDefn.Initialize( self, szTag, nFieldLength,
                               pachRecord, fieldAreaStart+nFieldPos ) then
            AddField( poFDefn )
        else
          FreeObject( poFDefn ) ;
      except
        FreeObject( poFDefn ) ;
      end ;
    end ;

    pachRecord := nil ;
    nFirstRecordOffset := fpDDF.Position ;
    Result := True ;
  end ;

  function TGIS_DDFModule.DDFFetchVariable(
    const _record          : TBytes ;
    const _offset          : Integer ;
    const _maxChars        : Integer ;
    const _nDelimChar1     : Integer ;
    const _nDelimChar2     : Integer ;
      var _pnConsumedChars : Integer
  ) : String ;
  var
    i : Integer ;
  begin
    i := 0 ;
    while ( i < _maxChars-1 ) and
          ( _record[_offset+i] <> _nDelimChar1 ) and
          ( _record[_offset+i] <> _nDelimChar2  ) do
      inc( i ) ;

    _pnConsumedChars := i ;
    if ( i < _maxChars ) and
       (( _record[_offset+i] = _nDelimChar1 ) or
        ( _record[_offset+i] = _nDelimChar2 ) ) then
      inc( _pnConsumedChars ) ;

    Result := ConvertAnsiString( _record, _offset, i, 0, 0 ) ;
  end ;

  function TGIS_DDFModule.DDFScanInt(
    const _bufData   : TBytes ;
    const _offset    : Integer ;
    const _maxChars  : Integer
  ) : LongInt ;
  var
    nMaxChars : Integer ;
    szWorking : String ;
    c         : Integer ;
    v         : Variant ;
  begin
    Result := 0 ;
    if ( _maxChars > 32 ) or ( _maxChars = 0 ) then
      nMaxChars := 32
    else
      nMaxChars := _maxChars ;

    szWorking := ConvertAnsiString( _bufData, _offset, nMaxChars, 0, 0 ) ;
    v := Result ;
    {$IFDEF JAVA}
      Val( szWorking, v, c )  ;
      Result := VarToInt32(v) ;
    {$ELSE}
      Val( szWorking, Result, c )  ;
    {$ENDIF}
  end ;

  function TGIS_DDFModule.DDFScanString(
    const _bufData   : TBytes ;
    const _offset    : Integer ;
    const _maxChars  : Integer
  ) : String ;
  var
    nMaxChars : Integer ;
  begin
    if ( _maxChars = 0 ) then
      nMaxChars := 32
    else
      nMaxChars := _maxChars ;

    if length( _bufData ) = 0 then begin
      Result := '' ;
      exit ;
    end ;

    Result := ConvertAnsiString( _bufData, _offset, nMaxChars, 0, 0 ) ;
  end ;

  procedure TGIS_DDFModule.AddCloneRecord(
    const _record : TGIS_DDFRecord
  ) ;
  begin
    if ( cloneCount = maxCloneCount ) then begin
      maxCloneCount := cloneCount*2 + 20;
      SetLength( aClones, maxCloneCount ) ;
    end ;

    aClones[cloneCount] := oRecord ;
    inc( cloneCount ) ;
  end ;

  procedure TGIS_DDFModule.AddField(
    const _poNewFDefn : TGIS_DDFFieldDefn
  ) ;
  begin
    inc( fieldDefnCount);
    SetLength( aFieldDefns, fieldDefnCount ) ;
    aFieldDefns[fieldDefnCount-1] := _poNewFDefn ;
  end ;

  procedure TGIS_DDFModule.Close ;
  var
    i : Integer ;
  begin
    if fpDDF <> nil then
      FreeObject( fpDDF ) ;

    if oRecord <> nil then
      FreeObject( oRecord ) ;

    maxCloneCount := 0;
    aClones := nil ;

    for i := 0 to fieldDefnCount-1 do
      FreeObject( aFieldDefns[i] ) ;

    aFieldDefns := nil ;
    fieldDefnCount := 0 ;
  end ;

  function TGIS_DDFModule.FindFieldDefn(
    const _sFieldName : String
  ) : TGIS_DDFFieldDefn;
  var
    i : Integer         ;
    pszThisName : String ;
  begin
    Result := nil;

    for i := 0 to fieldDefnCount-1 do begin
      pszThisName := aFieldDefns[i].Name ;

      if ( pszThisName = _sFieldName ) and
         ( _sFieldName[StringFirst+1] = pszThisName[StringFirst+1] ) then begin
           Result := aFieldDefns[i] ;
           exit ;
         end ;
    end ;

    for i := 0 to fieldDefnCount-1 do
      if _sFieldName = aFieldDefns[i].Name then begin
         Result := aFieldDefns[i] ;
         exit ;
       end ;
  end ;

  function TGIS_DDFModule.GetField(
    const _i : Integer
  ) : TGIS_DDFFieldDefn ;
  begin
    if ( _i < 0 ) or ( _i >= fieldDefnCount ) then
      Result := nil
    else
      Result := aFieldDefns[_i] ;
  end ;

  function TGIS_DDFModule.GetFieldControlLength : Integer ;
  begin
    Result := fieldControlLength ;
  end ;

  function TGIS_DDFModule.GetFieldCount : Integer ;
  begin
    Result := fieldDefnCount ;
  end ;

  function TGIS_DDFModule.ReadRecord : TGIS_DDFRecord ;
  begin
    if ( oRecord = nil ) then
      oRecord := TGIS_DDFRecord.Create( self ) ;

    if oRecord.Read then
      Result := oRecord
    else
      Result := nil ;
  end ;

  procedure TGIS_DDFModule.RemoveCloneRecord(
    const _record : TGIS_DDFRecord
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to cloneCount-1 do begin
      if ( aClones[i] = _record ) then begin
        aClones[i] := aClones[cloneCount-1] ;
        dec( cloneCount ) ;
        break ;
      end ;
    end ;
  end ;

  procedure TGIS_DDFModule.Rewind ;
  begin
      Rewind( -1 ) ;
  end ;

  procedure TGIS_DDFModule.Rewind(
    const _nOffset : Int32
  ) ;
  var
    nOffset : Integer ;
  begin
    if ( _nOffset = -1 ) then
      nOffset := nFirstRecordOffset
    else
      nOffset := _nOffset ;

    if fpDDF <> nil then begin
      fpDDF.Position := nOffset ;

      if ( nOffset = nFirstRecordOffset ) and ( oRecord <> nil ) then
        oRecord.Clear ;
    end ;
  end ;

//==============================================================================
// TGIS_DDFFieldDefn
//==============================================================================

  constructor TGIS_DDFFieldDefn.Create;
  begin
    inherited Create ;

    oModule        := nil;
    pszTag          := '';
    fieldName       := '';
    arrayDescr      := '';
    formatControls  := '';
    nSubfieldCount  := 0;
    aSubfields   := nil;
    bRepSubfields   := False ;
    nFixedWidth     := 0;
  end ;

  procedure TGIS_DDFFieldDefn.doDestroy ;
  var
    i  : Integer ;
  begin
    for i := 0 to nSubfieldCount-1 do
      FreeObject( aSubfields[i] ) ;

    aSubfields := nil ;

    inherited ;
  end ;

  function TGIS_DDFFieldDefn.expandFormat(
    const _sSrc : String
  ) : String ;
  var
    iSrc, iDst, nRepeat, i : Integer ;
    pszDest : String ;
    pszContents, pszExpandedContents, pszNext : String ;
    {$IFDEF JAVA}
      v : Variant ;
    {$ENDIF}

    function isdigit( const _char : Char ) : Boolean ;
    {$IFDEF JAVA}
      const
        cNumericSet : TSysCharSet = ('0','1','2','3','4','5','6','7','8','9') ;
    {$ENDIF}
    begin
      {$IFDEF JAVA}
        Result := CharInSet( _char, cNumericSet ) ;
      {$ELSE}
        Result := CharInSet( _char, ['0'..'9'] ) ;
      {$ENDIF}
    end ;

  begin
    iSrc := StringFirst ;

    while ( iSrc <=StringLast(_sSrc) ) do begin
      if ( (iSrc = StringFirst) or (_sSrc[iSrc-1] = ',')) and ( _sSrc[iSrc] = '(' ) then begin
        pszContents         := GetSubstring( Copy( _sSrc, iSrc, length(_sSrc) ) );
        pszExpandedContents := expandFormat( pszContents );

        pszDest := pszDest + pszExpandedContents ;

        iSrc := iSrc + length(pszContents) + 2;
      end
      else if ((iSrc = StringFirst) or ( _sSrc[iSrc-1] = ',')) and isdigit(_sSrc[iSrc]) then begin
        {$IFDEF JAVA}
          v := nRepeat ;
          Val( Copy( _sSrc, iSrc, MaxInt ), v, iDst ) ;
          nRepeat := VarToInt32(v) ;
        {$ELSE}
          Val( Copy( _sSrc, iSrc, MaxInt ), nRepeat, iDst ) ;
        {$ENDIF}
        while isdigit( _sSrc[ iSrc ] ) do
          inc( iSrc ) ;

        pszNext := Copy( _sSrc, iSrc, length(_sSrc) ) ;

        pszContents         := GetSubstring( pszNext );
        pszExpandedContents := expandFormat( pszContents );

        for i := 0 to nRepeat-1 do begin
          pszDest := pszDest + pszExpandedContents ;
          if ( i < nRepeat-1 ) then
            pszDest := pszDest + ',' ;
        end ;

        if ( pszNext[StringFirst] = '(' ) then
            iSrc := iSrc + length(pszContents) + 2
        else
            iSrc := iSrc + length(pszContents) ;
      end
      else begin
        pszDest := pszDest + _sSrc[iSrc] ;
        inc( iSrc ) ;
      end ;
    end ;
    Result := pszDest ;
  end ;

  function TGIS_DDFFieldDefn.applyFormats : Boolean ;
  var
    pszFormatList,
    pszPastPrefix : String ;
    tkn           : TGIS_Tokenizer ;
    i ,j  : Integer ;
  begin
    if ( length( formatControls ) < 2 ) or
       ( formatControls[StringFirst] <> '(' ) or
       ( formatControls[StringLast(formatControls)] <> ')' ) then begin
        Result := False ;
        exit ;
    end ;

    pszFormatList := expandFormat( formatControls );

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( pszFormatList, ',' ) ;

      for i := 0 to tkn.Result.Count-1 do begin
        pszPastPrefix := tkn.Result[i] ;
        j := StringFirst ;
        while ( pszPastPrefix[j] >= '0' ) and ( pszPastPrefix[j] <= '9' ) do
          inc( j );

        if ( i >= nSubfieldCount ) then break ;

        if not aSubfields[i].SetFormat( Copy( pszPastPrefix, j, MaxInt ) ) then begin
          Result := False ;
          exit ;
        end ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;

    if ( i < nSubfieldCount ) then begin
      Result := False ;
      exit ;
    end ;

    nFixedWidth := 0;
    for i := 0 to nSubfieldCount-1 do begin
      if ( aSubfields[i].Width = 0 ) then begin
        nFixedWidth := 0;
        break;
      end
      else
        nFixedWidth := nFixedWidth + aSubfields[i].Width ;
    end ;

    Result := True;
  end ;

  function TGIS_DDFFieldDefn.buildSubfields : Boolean ;
  var
    pszSublist         : String ;
    ipos, i            : Integer ;
    tkn                : TGIS_Tokenizer ;
    poSFDefn           : TGIS_DDFSubfieldDefn ;
  begin
    pszSublist := arrayDescr ;

    ipos := StringFirst - 1 ;
    for i := StringFirst to StringLast( pszSublist ) do
      if ( pszSublist[ i ] = '*' ) then
        ipos := i ;

    if ( ipos > StringFirst - 1 ) and ( ipos <> StringLast( pszSublist ) ) then begin
      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( pszSublist, '*' ) ;
        if tkn.Result.Count > 0 then begin
          pszSublist := tkn.Result[ tkn.Result.Count -1 ] ;
          bRepSubfields := True ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    end ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( pszSublist, '!' ) ;

      for i := 0 to tkn.Result.Count - 1 do begin
        poSFDefn := TGIS_DDFSubfieldDefn.Create( oModule ) ;
        poSFDefn.SetName( tkn.Result[i] ) ;
        AddSubfield( poSFDefn, True );
      end ;

    finally
      FreeObject( tkn ) ;
    end ;

    Result := True ;
  end ;

  function TGIS_DDFFieldDefn.GetSubstring(
    const _sSrc : String
  ) : String ;
  var
    nBracket, i : Integer ;
    pszReturn : String ;
  begin
    nBracket := 0 ;
    i := StringFirst ;
    while ( i <= StringLast(_sSrc) ) and ( ( nBracket > 0 ) or ( _sSrc[i] <> ',')) do begin
      if ( _sSrc[i] = '(' ) then
        inc( nBracket )
      else if ( _sSrc[i] = ')' ) then
        dec( nBracket ) ;

      inc( i ) ;
    end ;

    if ( _sSrc[StringFirst] = '(' ) then begin
      pszReturn := Copy( _sSrc, StringFirst + 1, i-2-StringFirst ) ;
    end
    else begin
      pszReturn := Copy( _sSrc, StringFirst, i-StringFirst) ;
    end ;

    Result := pszReturn ;
  end ;

  function TGIS_DDFFieldDefn.FindSubfieldDefn(
    const _sMnemonic : String
  ) : TGIS_DDFSubfieldDefn ;
  var
    i  : Integer ;
  begin
    Result := nil ;
    for i := 0 to nSubfieldCount-1 do begin
      if aSubfields[i].Name = _sMnemonic then begin
        Result := aSubfields[i] ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_DDFFieldDefn.GetSubfield(
    const _i : Integer
  ) : TGIS_DDFSubfieldDefn ;
  begin
     if ( _i < 0 ) or ( _i >= nSubfieldCount ) then
       Result := nil
     else
       Result := aSubfields[_i] ;
  end ;

  procedure TGIS_DDFFieldDefn.AddSubfield(
    const _poNewSFDefn      : TGIS_DDFSubfieldDefn;
    const _bDontAddToFormat : Boolean
  ) ;
  begin
    inc( nSubfieldCount);

    SetLength( aSubfields, nSubfieldCount ) ;
    aSubfields[nSubfieldCount-1] := _poNewSFDefn ;

    if _bDontAddToFormat then exit ;

    assert( _bDontAddToFormat, 'AddSubfield' ) ;
  end ;

  function TGIS_DDFFieldDefn.Initialize(
    const _module     : TGIS_DDFModule;
    const _sTag     : String;
    const _nSize      : Integer;
    const _record     : TBytes;
    const _offset     : Integer
   ) : Boolean ;
  var
    iFDOffset       : Integer ;
    nCharsConsumed  : Integer ;
  begin
    iFDOffset := _module.GetFieldControlLength ;
    oModule  := _module ;
    pszTag    := _sTag ;

    case _record[_offset+0] of
      ord(' ') : data_struct_code := TGIS_DDFDataStructCode.elementary ;
      ord('0') : data_struct_code := TGIS_DDFDataStructCode.elementary ;
      ord('1') : data_struct_code := TGIS_DDFDataStructCode.vector ;
      ord('2') : data_struct_code := TGIS_DDFDataStructCode.array ;
      ord('3') : data_struct_code := TGIS_DDFDataStructCode.concatenated
    else         data_struct_code := TGIS_DDFDataStructCode.elementary;
    end ;

    case ( _record[_offset+1] ) of
      ord(' ') : data_type_code := TGIS_DDFDataTypeCode.char_string;
      ord('0') : data_type_code := TGIS_DDFDataTypeCode.char_string;
      ord('1') : data_type_code := TGIS_DDFDataTypeCode.implicit_point;
      ord('2') : data_type_code := TGIS_DDFDataTypeCode.explicit_point;
      ord('3') : data_type_code := TGIS_DDFDataTypeCode.explicit_point_scaled;
      ord('4') : data_type_code := TGIS_DDFDataTypeCode.char_bit_string;
      ord('5') : data_type_code := TGIS_DDFDataTypeCode.bit_string;
      ord('6') : data_type_code := TGIS_DDFDataTypeCode.mixed_data_type
    else         data_type_code := TGIS_DDFDataTypeCode.char_string;
    end ;

    fieldName := oModule.DDFFetchVariable( _record, _offset+iFDOffset,
                                            _nSize - iFDOffset,
                                            DDF_UNIT_TERMINATOR,
                                            DDF_FIELD_TERMINATOR,
                                            nCharsConsumed
                                          ) ;
    inc( iFDOffset, nCharsConsumed ) ;

    arrayDescr := oModule.DDFFetchVariable( _record, _offset+iFDOffset,
                                              _nSize - iFDOffset,
                                              DDF_UNIT_TERMINATOR,
                                              DDF_FIELD_TERMINATOR,
                                              nCharsConsumed
                                             ) ;
    inc( iFDOffset, nCharsConsumed ) ;

    formatControls := oModule.DDFFetchVariable( _record, _offset+iFDOffset,
                                                  _nSize - iFDOffset,
                                                  DDF_UNIT_TERMINATOR,
                                                  DDF_FIELD_TERMINATOR,
                                                  nCharsConsumed
                                                 ) ;

    if ( data_struct_code <> TGIS_DDFDataStructCode.elementary ) then begin
      if not buildSubfields then begin
        Result := False ;
        exit ;
      end ;

      if not applyFormats then begin
        Result := False ;
        exit ;
      end ;
    end ;

    Result := True ;
  end ;

//==============================================================================
// TGIS_DDFSubfieldDefn
//==============================================================================

  constructor TGIS_DDFSubfieldDefn.Create(
    const _module  : TGIS_DDFModule
  ) ;
  begin
    inherited Create ;

    pszName           := '' ;
    bIsVariable       := True ;
    nFormatWidth      := 0 ;
    chFormatDelimeter := DDF_UNIT_TERMINATOR ;
    eBinaryFormat     := TGIS_DDFBinaryFormat.NotBinary ;
    eType             := TGIS_DDFDataType.String ;
    pszFormatString   := '' ;
    nMaxBufChars      := 0 ;
    pachBuffer        := nil ;
    oModule           := _module ;
  end ;

  procedure TGIS_DDFSubfieldDefn.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_DDFSubfieldDefn.GetFloatData(
    const _sourceData    : TBytes ;
    const _maxBytes      : Integer ;
      var _consumedBytes : Integer
  ) : Double ;
  var
    abyData : TBytes ;
    i       : Integer ;
    v       : Variant ;
  begin
    Result := 0.0 ;
    v := Result ;
    SetLength( abyData, 8 ) ;
    case pszFormatString[StringFirst] of
      'A',
      'I',
      'R',
      'S',
      'C': begin
             {$IFDEF JAVA}
               Val( oModule.DDFScanString(
                      GetStringData( _sourceData, _maxBytes, _consumedBytes ),
                      0, MaxInt
                    ),
                    v,
                    i
                ) ;
               Result := VarToDouble(v);
             {$ELSE}
               Val( oModule.DDFScanString(
                      GetStringData( _sourceData, _maxBytes, _consumedBytes ),
                      0, MaxInt
                    ),
                    Result,
                    i
                ) ;
             {$ENDIF}
           end ;
      'B',
      'b': begin
          _consumedBytes := nFormatWidth ;

          if ( pszFormatString[StringFirst] = 'B' ) then begin
            for i := 0 to nFormatWidth-1 do
              abyData[nFormatWidth-i-1] := _sourceData[i];
          end
          else
            abyData := Copy( _sourceData, 0, nFormatWidth ) ;

          case eBinaryFormat of
            TGIS_DDFBinaryFormat.UInt :
              if      ( nFormatWidth = 1 ) then
                      Result := abyData[0]
              else if ( nFormatWidth = 2 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToUInt16( abyData, 0 )
                      {$ELSE}
                        Result := PWord( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 4 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToUInt32( abyData, 0 )
                      {$ELSE}
                        Result := PCardinal( @abyData[0] )^
                      {$ENDIF}
              else    Result := 0 ;

            TGIS_DDFBinaryFormat.SInt :
              if      ( nFormatWidth = 1 ) then
                      {$IFDEF OXYGENE}
                        Result := Byte( abyData[0] )
                      {$ELSE}
                        Result := PByte( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 2 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToInt16( abyData, 0 )
                      {$ELSE}
                        Result := PSmallInt( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 4 ) then
                {$IFDEF OXYGENE}
                 Result := BitConverter.ToInt32( abyData, 0 )
                {$ELSE}
                 Result := PInteger( @abyData[0] )^
                {$ENDIF}
              else Result := 0 ;

            TGIS_DDFBinaryFormat.FloatReal :
              if      ( nFormatWidth = 4 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToDouble( abyData, 0 )
                      {$ELSE}
                        Result := PDouble( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 8 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToDouble( abyData, 0 )
                      {$ELSE}
                        Result := PDouble( @abyData[0] )^
                      {$ENDIF}
              else    Result := 0 ;

            TGIS_DDFBinaryFormat.NotBinary,
            TGIS_DDFBinaryFormat.FPReal,
            TGIS_DDFBinaryFormat.FloatComplex :
              Result := 0 ;
          end
      end
    end ;
  end ;

  function TGIS_DDFSubfieldDefn.GetIntData(
    const _sourceData    : TBytes;
    const _maxBytes      : Integer;
    var   _consumedBytes : Integer
  ) : Integer ;
 var
    abyData : TBytes ;
    i       : Integer ;
    v       : Variant ;
  begin
    Result := 0 ;
    v := Result ;
    SetLength( abyData, 8 ) ;
    case pszFormatString[StringFirst] of
      'A',
      'I',
      'R',
      'S',
      'C': begin
             {$IFDEF JAVA}
               Val( oModule.DDFScanString(
                    GetStringData( _sourceData,
                                   _maxBytes,
                                   _consumedBytes
                                 ),
                    0, MaxInt ),
                   v,
                   i
                 ) ;
                Result := VarToInt32(v) ;
             {$ELSE}
               Val( oModule.DDFScanString(
                    GetStringData( _sourceData,
                                   _maxBytes,
                                   _consumedBytes
                                 ),
                    0, MaxInt ),
                   Result,
                   i
                 ) ;
             {$ENDIF}
            end ;
      'B',
      'b': begin
          _consumedBytes := nFormatWidth ;

          if ( pszFormatString[StringFirst] = 'B' ) then begin
            for i := 0 to nFormatWidth-1 do
              abyData[nFormatWidth-i-1] := _sourceData[i];
          end
          else begin
            SetLength( abyData, nFormatWidth ) ;
            GisCopyMemory( _sourceData, 0, abyData, 0, nFormatWidth );
          end ;

          case eBinaryFormat of
            TGIS_DDFBinaryFormat.UInt :
              if      ( nFormatWidth = 1 ) then
                      Result := abyData[0]
              else if ( nFormatWidth = 2 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToUInt16( abyData, 0 )
                      {$ELSE}
                        Result := PWord( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 4 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToUInt32( abyData, 0 )
                      {$ELSE}
                        Result := PCardinal( @abyData[0] )^
                      {$ENDIF}
              else    Result := 0 ;

            TGIS_DDFBinaryFormat.SInt :
              if      ( nFormatWidth = 1 ) then
                      {$IFDEF OXYGENE}
                        Result := Byte( abyData[ 0 ] )
                      {$ELSE}
                        Result := PByte( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 2 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToInt16( abyData, 0 )
                      {$ELSE}
                        Result := PSmallInt( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 4 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToInt32( abyData, 0 )
                      {$ELSE}
                        Result := PInteger( @abyData[0] )^
                      {$ENDIF}
              else    Result := 0 ;

            TGIS_DDFBinaryFormat.FloatReal :
              if      ( nFormatWidth = 4 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToInt32( abyData, 0 )
                      {$ELSE}
                        Result := PInteger( @abyData[0] )^
                      {$ENDIF}
              else if ( nFormatWidth = 8 ) then
                      {$IFDEF OXYGENE}
                        Result := BitConverter.ToInt32( abyData, 0 )
                      {$ELSE}
                        Result := PInteger( @abyData[0] )^
                      {$ENDIF}
              else    Result := 0 ;

            TGIS_DDFBinaryFormat.NotBinary,
            TGIS_DDFBinaryFormat.FPReal,
            TGIS_DDFBinaryFormat.FloatComplex :
              Result := 0 ;
          end
      end
    end ;
  end ;

  function TGIS_DDFSubfieldDefn.GetStringData(
    const _sourceData     : TBytes;
    const _maxBytes       : Integer;
      var _consumedBytes  : Integer
  ) : TBytes;
  var
    nLength : Integer ;
  begin
    nLength := getDataLength( _sourceData, _maxBytes, _consumedBytes ) ;

    if ( nMaxBufChars < nLength+1 ) then begin
      pachBuffer := nil;
      nMaxBufChars := nLength+1;
      SetLength( pachBuffer, nMaxBufChars ) ;
    end ;

    pachBuffer := Copy( _sourceData, 0, nLength ) ;

    Result := pachBuffer;
  end ;

  function TGIS_DDFSubfieldDefn.getDataLength(
    const _sourceData     : TBytes;
    const _maxBytes       : Integer;
      var _consumedBytes  : Integer
  ) : Integer ;
  var
    nLength, extraConsumedBytes : Integer ;
    bAsciiField : Boolean ;
  begin
    if not bIsVariable then begin
      if ( nFormatWidth > _maxBytes ) then begin
        _consumedBytes := _maxBytes ;

        Result := _maxBytes;
        exit ;
      end
      else begin
        _consumedBytes := nFormatWidth ;

        Result := nFormatWidth;
        exit ;
      end
    end
    else begin
      nLength := 0;
      bAsciiField := True;
      extraConsumedBytes := 0;
      Result := 0 ;
      if length( _sourceData ) = 0 then exit ;

      if ( _maxBytes > 1 ) and
         ( (_sourceData[_maxBytes-2] = chFormatDelimeter ) or
           ( _sourceData[_maxBytes-2] = DDF_FIELD_TERMINATOR) ) and
          ( _sourceData[_maxBytes-1] = 0 ) then
          bAsciiField := False ;

      while( nLength < _maxBytes) do begin
        if bAsciiField then begin
          if (_sourceData[nLength] = chFormatDelimeter) or
             (_sourceData[nLength] = DDF_FIELD_TERMINATOR) then
              break;
        end
        else begin
            if (nLength > 0)
                and ((_sourceData[nLength-1] = chFormatDelimeter)
                    or( _sourceData[nLength-1] = DDF_FIELD_TERMINATOR))
                and ( _sourceData[nLength] = 0) then begin
                  if (nLength+1 < _maxBytes) and
                     ( _sourceData[nLength+1] = DDF_FIELD_TERMINATOR) then
                      inc( extraConsumedBytes) ;
                  break;
                end ;
        end ;
        inc( nLength);
      end ;

        if( _maxBytes = 0 ) then
          _consumedBytes := nLength + extraConsumedBytes
        else
          _consumedBytes := nLength + extraConsumedBytes + 1;

      Result := nLength;
    end
  end ;

  function TGIS_DDFSubfieldDefn.SetFormat(
    const _sFormat : String
  ) : Boolean ;
  var
    str : String ;

    function getWidth( const _off : Integer ) : Integer ;
    var
      i   : Integer ;
      off : Integer ;
    begin
      i := 0 ;
      off := _off + StringFirst - 1 ;

      while i < StringLast( pszFormatString ) - off do
        if pszFormatString[ off+i ] <> ')' then
          inc( i ) ;

      str := Copy( pszFormatString, off, i ) ;
      Result := StrToInt( str ) ;
    end ;

    function getBWidth : Integer ;
    var
      i   : Integer ;
    begin
      i := 0 ;
      while i < length( pszFormatString ) do
        if pszFormatString[ StringFirst+i ] <> ')' then
          inc( i ) ;

      str := Copy( pszFormatString, StringFirst+1, i ) ;
      Result := StrToInt( str ) ;
    end ;

  begin
    pszFormatString := _sFormat ;

    if ( length( pszFormatString ) > 1 ) and ( pszFormatString[StringFirst+1] = '(' ) then
    begin
      nFormatWidth := getWidth( 3 ) ;
      bIsVariable  := nFormatWidth = 0;
    end
    else
      bIsVariable := True ;

    case pszFormatString[StringFirst] of
      'A': eType := TGIS_DDFDataType.String ;
      'C': eType := TGIS_DDFDataType.String ;
      'R': eType := TGIS_DDFDataType.Float ;
      'I': eType := TGIS_DDFDataType.Int;
      'S': eType := TGIS_DDFDataType.Int;
      'B',
      'b': begin
            bIsVariable := False;
            if ( pszFormatString[StringFirst+1] = '(' ) then begin
              nFormatWidth := getWidth( 3 ) div 8 ;
              eBinaryFormat := TGIS_DDFBinaryFormat.SInt ; // good default, works for SDTS.

              if( nFormatWidth < 5 ) then
                eType := TGIS_DDFDataType.Int
              else
                eType := TGIS_DDFDataType.BinaryString;
            end
            else begin
              eBinaryFormat := TGIS_DDFBinaryFormat( ord(pszFormatString[StringFirst+1]) - ord('0'));
              nFormatWidth  := StrToInt( pszFormatString[StringFirst+2] ) ;

              if ( eBinaryFormat = TGIS_DDFBinaryFormat.SInt ) or ( eBinaryFormat = TGIS_DDFBinaryFormat.UInt ) then
                eType := TGIS_DDFDataType.Int
              else
                eType := TGIS_DDFDataType.Float;
            end ;
        end ;
      else begin
        Result := False ;
        exit ;
      end ;
    end ;

    Result := True;
  end ;

  procedure TGIS_DDFSubfieldDefn.SetName(
    const _newName : String
  ) ;
  begin
    pszName := _newName ;
  end ;

//==============================================================================
// TGIS_DDFField
//==============================================================================

  function TGIS_DDFField.GetRepeatCount(
    const _record  : TGIS_DDFRecord
  ) : Integer;
  var
    iOffset, iRepeatCount, iSF, nBytesConsumed : Integer ;
    poThisSFDefn : TGIS_DDFSubfieldDefn ;
  begin
    if not oDefn.bRepSubfields then begin
      Result := 1 ;
      exit ;
    end ;

    if oDefn.FixedWidth > 0 then begin
      Result := TruncS( 1.0*nDataSize / oDefn.FixedWidth  );
      exit ;
    end ;

    iOffset      := 0;
    iRepeatCount := 1;

    while True do begin

      for iSF := 0 to oDefn.SubfieldCount-1 do begin

        poThisSFDefn := oDefn.GetSubfield( iSF ) ;

        if ( poThisSFDefn.Width > nDataSize - iOffset ) then
          nBytesConsumed := poThisSFDefn.Width
        else
          poThisSFDefn.getDataLength( Copy( _record.Data, chData+iOffset, length(_record.Data) ),
                                      nDataSize - iOffset,
                                      nBytesConsumed
                                     );
        inc( iOffset, nBytesConsumed ) ;
        if ( iOffset > nDataSize ) then begin
          Result := iRepeatCount-1;
          exit ;
        end ;
      end ;

      if ( iOffset > nDataSize - 2 ) then begin
        Result := iRepeatCount;
        exit ;
      end ;

      inc( iRepeatCount ) ;
    end ;
  end ;

  function TGIS_DDFField.GetSubfieldData(
    const _record         : TGIS_DDFRecord ;
    const _poSFDefn       : TGIS_DDFSubfieldDefn ;
      var _pnMaxBytes     : Integer
  ) : Integer;
  begin
    Result := GetSubfieldData( _record, _poSFDefn, _pnMaxBytes, 0 ) ;
  end ;

  function TGIS_DDFField.GetSubfieldData(
    const _record         : TGIS_DDFRecord;
    const _poSFDefn       : TGIS_DDFSubfieldDefn ;
      var _pnMaxBytes     : Integer;
    const _subfieldIndex  : Integer
  ) : Integer;
  var
    iOffset, iSF, nBytesConsumed , iSubfieldIndex: Integer ;
    poThisSFDefn : TGIS_DDFSubfieldDefn ;
    pdata : TBytes ;
  begin
    Result := 0 ;
    iOffset := 0 ;
    iSubfieldIndex := _subfieldIndex ;
    nBytesConsumed := 0 ;

    if ( _poSFDefn = nil ) then exit ;

    if ( iSubfieldIndex > 0 ) and ( oDefn.FixedWidth > 0 ) then begin
      iOffset := oDefn.FixedWidth * iSubfieldIndex ;
      iSubfieldIndex := 0;
    end ;

    while ( iSubfieldIndex >= 0 ) do begin

      for iSF := 0 to oDefn.SubfieldCount-1 do begin
        poThisSFDefn := oDefn.GetSubfield( iSF );

        if ( poThisSFDefn = _poSFDefn ) and ( iSubfieldIndex = 0 ) then begin
          _pnMaxBytes := nDataSize - iOffset;

          Result := chData+iOffset ;
          exit ;
        end ;

        pdata := Copy( _record.chData, chData+iOffset, nDataSize - iOffset ) ;
        poThisSFDefn.getDataLength( pdata, nDataSize - iOffset,
                                    nBytesConsumed);
        inc( iOffset, nBytesConsumed );
      end ;

      dec( iSubfieldIndex);
    end
  end ;

  procedure TGIS_DDFField.Initialize(
    const _poDefnIn    : TGIS_DDFFieldDefn ;
    const _pachDataIn  : Integer ;
    const _nDataSizeIn : Integer
  ) ;
  begin
    chData  := _pachDataIn ;
    nDataSize := _nDataSizeIn ;
    oDefn    := _poDefnIn ;
  end ;

//==============================================================================
// TGIS_DDFRecord
//==============================================================================

  constructor TGIS_DDFRecord.Create(
    const _poModuleIn : TGIS_DDFModule
  ) ;
  begin
    inherited Create ;

    oModule := _poModuleIn;
    nReuseHeader := False;
    fieldOffset := 0;
    nDataSize := 0;
    chData := nil;
    nFieldCount := 0;
    aFields := nil;
    bIsClone := False;
    sizeFieldTag := 4;
    sizeFieldPos := 0;
    sizeFieldLength := 0;
  end ;

  procedure TGIS_DDFRecord.doDestroy ;
  begin
    Clear ;

    if bIsClone then
      oModule.RemoveCloneRecord( self ) ;

    inherited ;
  end ;

  function TGIS_DDFRecord.Clone : TGIS_DDFRecord ;
  var
    poNR : TGIS_DDFRecord ;
    i,
    nOffset    : Integer ;
  begin
    poNR := TGIS_DDFRecord.Create( oModule );

    poNR.nReuseHeader := False;
    poNR.fieldOffset := fieldOffset;

    poNR.nDataSize := nDataSize;
    poNR.chData  := Copy( chData, 0, nDataSize );

    poNR.nFieldCount := nFieldCount;
    SetLength( poNR.aFields, nFieldCount ) ;

    for i := 0 to nFieldCount-1 do begin
      poNR.aFields[i] := TGIS_DDFField.Create ;
      nOffset := aFields[i].Data ;
      poNR.aFields[i].Initialize( aFields[i].FieldDefn,
                                    nOffset,
                                    aFields[i].DataSize
                                   );
    end ;

    poNR.bIsClone := True;
    oModule.AddCloneRecord( poNR );

    Result := poNR ;
  end ;

  function TGIS_DDFRecord.FindField(
    const _sName    : String ;
    const _fieldIndex : Integer
   ) : TGIS_DDFField ;
  var
    i, fieldIndex : Integer ;
  begin
    Result := nil ;
    fieldIndex := _fieldIndex ;

    for i := 0 to nFieldCount-1 do begin
      if aFields[i].FieldDefn.Name = _sName then begin
        if fieldIndex = 0 then begin
          Result := aFields[ i ] ;
          exit ;
        end
        else
          dec( fieldIndex ) ;
      end
    end ;
  end ;

  function TGIS_DDFRecord.GetField(
    const _i : Integer
  ) : TGIS_DDFField ;
  begin
    if ( _i < 0 ) or ( _i >= nFieldCount ) then
      Result := nil
    else
      Result := aFields[ _i ] ;
  end ;

  function TGIS_DDFRecord.GetFloatSubfield(
    const _field          : String;
    const _fieldIndex     : Integer;
    const _subfield       : String;
    const _subfieldIndex  : Integer
  ) : Double ;
  var
    poField   : TGIS_DDFField ;
    poSFDefn  : TGIS_DDFSubfieldDefn ;
    bytes_left,
    ncb       : Integer ;
    pachD     : TBytes ;
  begin
    poField := FindField( _field, _fieldIndex ) ;
    if ( poField = nil ) then begin
      Result := 0 ;
      exit ;
    end ;

    poSFDefn := poField.FieldDefn.FindSubfieldDefn( _subfield );
    if ( poSFDefn = nil ) then begin
      Result := 0 ;
      exit ;
    end ;

    pachD := Copy( chData,
                   poField.GetSubfieldData( self, poSFDefn, bytes_left, _subfieldIndex ),
                   length(chData) ) ;

    Result := poSFDefn.GetFloatData( pachD, bytes_left, ncb ) ;
  end ;

  function TGIS_DDFRecord.GetIntSubfield(
    const _field          : String ;
    const _fieldIndex     : Integer ;
    const _subfield       : String ;
    const _subfieldIndex  : Integer
  ) : Integer ;
  var
    poField   : TGIS_DDFField ;
    poSFDefn  : TGIS_DDFSubfieldDefn ;
    bytes_left,
    ncb,p,cnt : Integer ;
    pachD     : TBytes ;
  begin
    poField := FindField( _field, _fieldIndex ) ;
    if ( poField = nil ) then begin
      Result := 0 ;
      exit ;
    end ;

    poSFDefn := poField.FieldDefn.FindSubfieldDefn( _subfield );
    if ( poSFDefn = nil ) then begin
      Result := 0 ;
      exit ;
    end ;

    p   := poField.GetSubfieldData( self,poSFDefn, bytes_left, _subfieldIndex ) ;
    cnt := length( chData )-p ;
    SetLength( pachD, cnt ) ;
    GisCopyMemory( chData, p, pachD, 0, cnt );

    Result := poSFDefn.GetIntData( pachD, bytes_left, ncb ) ;
  end ;


  function TGIS_DDFRecord.GetStringSubfield(
    const _field          : String ;
    const _fieldIndex     : Integer ;
    const _subfield       : String ;
    const _subfieldIndex  : Integer
  ) : String ;
  var
    poField   : TGIS_DDFField ;
    poSFDefn  : TGIS_DDFSubfieldDefn ;
    bytes_left, ncb : Integer ;
    pachD : TBytes ;
  begin
    poField := FindField( _field, _fieldIndex ) ;
    if ( poField = nil ) then begin
      Result := '' ;
      exit ;
    end ;

    poSFDefn := poField.FieldDefn.FindSubfieldDefn( _subfield );
    if ( poSFDefn = nil ) then begin
      Result := '' ;
      exit ;
    end ;
    bytes_left := 0;
    pachD := Copy( chData,
                   poField.GetSubfieldData( self, poSFDefn, bytes_left, _subfieldIndex ),
                   length(chData) ) ;

    Result := oModule.DDFScanString( poSFDefn.GetStringData( pachD, bytes_left, ncb ),
                                      0, MaxInt ) ;
  end ;

  function TGIS_DDFRecord.Read : Boolean ;
  var
    nReadBytes : Integer ;
  begin
    if not nReuseHeader then begin
      Result := readHeader ;
      exit ;
    end ;

    nReadBytes := oModule.FP.ReadBytesCnt( chData, nDataSize - fieldOffset,
                                               fieldOffset ) ;
    if ( nReadBytes <> (nDataSize - fieldOffset) ) and
       ( nReadBytes = 0 ) and oModule.FP.Eof then begin
        Result := False ;
        exit ;
    end
    else if ( nReadBytes <> (nDataSize - fieldOffset) ) then begin
      Result := False ;
      exit ;
    end ;

    Result := True ;
  end ;

  procedure TGIS_DDFRecord.Clear ;
  var
    i : Integer ;
  begin
    for i := 0 to length( aFields ) - 1 do
      FreeObject( aFields[i] ) ;

    aFields    := nil;
    nFieldCount  := 0;
    chData     := nil;
    nDataSize    := 0;
    nReuseHeader := False;
  end ;

  function TGIS_DDFRecord.readHeader : Boolean ;
  const
    nLeaderSize = 24 ;
  var
    achLeader,
    tmpBuf,
    newBuf      : TBytes ;
    nReadBytes  : Integer ;
    recLength,
    fieldAreaStart : Integer ;
    leaderIden    :  Byte ;
    i,
    nFieldEntryWidth,
    rewindSize    : Integer ;
    szTag         : String ;
    nEntryOffset,
    nFieldLength,
    nFieldPos     : Integer ;
    poFieldDefn   : TGIS_DDFFieldDefn ;
    pos           : Int64 ;
  begin
    Clear;

    SetLength( achLeader, nLeaderSize );

    nReadBytes := oModule.FP.ReadBytesCnt( achLeader, nLeaderSize ) ;
    if ( nReadBytes = 0 ) and ( oModule.FP.Eof ) then begin
      Result := False ;
      exit ;
    end
    else if ( nReadBytes <> nLeaderSize ) then begin
      Result := False ;
      exit ;
    end ;

    recLength      := oModule.DDFScanInt( achLeader, 0, 5 ) ;
    leaderIden     := achLeader[6] ;
    fieldAreaStart := oModule.DDFScanInt(achLeader, 12, 5 ) ;

    sizeFieldLength := achLeader[20] - ord('0') ;
    sizeFieldPos    := achLeader[21] - ord('0') ;
    sizeFieldTag    := achLeader[23] - ord('0') ;

    if ( sizeFieldLength < 0 ) or ( sizeFieldLength > 9 ) or
       ( sizeFieldPos    < 0 ) or ( sizeFieldPos    > 9 ) or
       ( sizeFieldTag    < 0 ) or ( sizeFieldTag    > 9 ) then begin
      Result := False ;
      exit ;
    end ;

    if ( leaderIden = ord('R') ) then
      nReuseHeader := True ;

    fieldOffset := fieldAreaStart - nLeaderSize ;

    if ( ( (recLength < 24) or ( recLength > 100000000 ) or
         ( fieldAreaStart < 24 ) or ( fieldAreaStart > 100000 ) )
       and (recLength <> 0) ) then begin
      Result := False ;
      exit ;
    end ;

    if (recLength <> 0) then begin
      nDataSize := recLength - nLeaderSize;
      SetLength( chData, nDataSize ) ;

      if ( oModule.FP.ReadBytesCnt( chData, nDataSize ) <> nDataSize ) then begin
        Result := False ;
        exit ;
      end ;

      while ( chData[nDataSize-1] <> DDF_FIELD_TERMINATOR ) and
            ( (nDataSize = 0 ) or ( chData[nDataSize-2] <> DDF_FIELD_TERMINATOR) ) do
      begin

          inc( nDataSize ) ;
          SetLength( chData, nDataSize ) ;

          if ( oModule.FP.ReadBytesCnt( chData, 1, nDataSize - 1 ) <> 1 ) then
          begin
            Result := False ;
            exit ;
          end ;
      end ;

      nFieldEntryWidth := sizeFieldLength + sizeFieldPos + sizeFieldTag;
      nFieldCount := 0;
      i := 0 ;
      while i < nDataSize do begin
        if ( chData[i] = DDF_FIELD_TERMINATOR ) then
          break ;
        inc( nFieldCount ) ;
        inc( i, nFieldEntryWidth ) ;
      end ;

      SetLength( aFields, nFieldCount ) ;

      for i := 0 to nFieldCount-1 do begin
        nEntryOffset := i*nFieldEntryWidth;

        szTag := oModule.DDFScanString( chData, nEntryOffset, sizeFieldTag );
        inc( nEntryOffset, sizeFieldTag ) ;
        nFieldLength := oModule.DDFScanInt( chData,nEntryOffset, sizeFieldLength ) ;

        inc( nEntryOffset, sizeFieldLength ) ;
        nFieldPos := oModule.DDFScanInt( chData, nEntryOffset, sizeFieldPos ) ;

        poFieldDefn := oModule.FindFieldDefn( szTag ) ;

        if ( poFieldDefn = nil ) then begin
          Result := False ;
          exit ;
        end ;
        aFields[i] := TGIS_DDFField.Create ;
        aFields[i].Initialize( poFieldDefn,
                                 fieldAreaStart + nFieldPos - nLeaderSize,
                                 nFieldLength );
      end ;

      Result := True;
      exit ;
    end
    else begin
      nDataSize := 0;
      chData  := nil;

      nFieldEntryWidth := sizeFieldLength + sizeFieldPos + sizeFieldTag;
      nFieldCount := 0;
      SetLength( tmpBuf, nFieldEntryWidth ) ;

      if ( tmpBuf = nil ) then begin
        Result := False;
        exit ;
      end ;

      repeat
        if nFieldEntryWidth <> oModule.FP.ReadBytesCnt( tmpBuf, nFieldEntryWidth ) then
        begin
          Result := False;
          exit ;
        end ;

        SetLength( newBuf, nDataSize+nFieldEntryWidth ) ;
        if ( chData <> nil ) then begin
          newBuf := Copy( chData, 0, nDataSize ) ;
          chData := nil ;
        end ;

        GisCopyMemory( tmpBuf, 0, newBuf, nDataSize, nFieldEntryWidth ) ;
        chData := newBuf;
        inc( nDataSize, nFieldEntryWidth ) ;

        if ( DDF_FIELD_TERMINATOR <> tmpBuf[0] ) then
          inc( nFieldCount ) ;
      until (DDF_FIELD_TERMINATOR = tmpBuf[0]) ;

      rewindSize := nFieldEntryWidth - 1;
      pos := oModule.FP.Position - rewindSize;
      oModule.FP.Position := pos ;
      dec( nDataSize, rewindSize );

      for i := 0 to nFieldCount-1 do begin
        nEntryOffset := (i*nFieldEntryWidth) + sizeFieldTag;
        nFieldLength := oModule.DDFScanInt( chData, nEntryOffset,
                                             sizeFieldLength
                                            ) ;
        SetLength( tmpBuf, nFieldLength ) ;

        if nFieldLength <> oModule.FP.ReadBytesCnt( tmpBuf, nFieldLength ) then
        begin
          Result := False;
          exit ;
        end ;

        SetLength( newBuf, nDataSize+nFieldLength ) ;
        newBuf := Copy( chData, 0, nDataSize ) ;
        chData := nil ;

        GisCopyMemory( tmpBuf, 0, newBuf, nDataSize, nFieldLength ) ;
        tmpBuf := nil ;
        chData := newBuf;
        inc( nDataSize, nFieldLength ) ;
      end ;

      SetLength( aFields, nFieldCount) ;

      for i := 0 to nFieldCount-1 do begin
        nEntryOffset := i*nFieldEntryWidth;
        szTag := oModule.DDFScanString( chData, nEntryOffset, sizeFieldTag ) ;

        inc( nEntryOffset, sizeFieldTag );
        nFieldLength := oModule.DDFScanInt( chData,nEntryOffset, sizeFieldLength );

        inc( nEntryOffset, sizeFieldLength );
        nFieldPos := oModule.DDFScanInt( chData, nEntryOffset, sizeFieldPos );

        poFieldDefn := oModule.FindFieldDefn( szTag );

        if ( poFieldDefn = nil ) then begin
          Result := False;
          exit ;
        end ;
        aFields[i] := TGIS_DDFField.Create ;
        aFields[i].Initialize( poFieldDefn,
                                 fieldAreaStart + nFieldPos - nLeaderSize,
                                 nFieldLength );
      end ;

      Result := True;
    end ;
  end ;

//==================================== END =====================================
end.
