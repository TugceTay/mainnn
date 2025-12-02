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
  DK internal functions.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoInternals ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoInternals"'}
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

interface
{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF CLR}
  uses
    System.Drawing,
    System.Data,
    System.Runtime.InteropServices,
    System.Runtime.InteropServices.ComTypes,
    System.Text,
    System.Text.RegularExpressions,
    System.IO,
    System.Globalization,
    System.Net,
    {$IFDEF MSWINDOWS}
      System.Web,
    {$ENDIF}
    System.Xml,
    System.Collections,
    System.Collections.Generic,
    {$IFDEF MSWINDOWS}
      ADODB,
      ADOX ,
    {$ENDIF}
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Types,
    System.Classes,
    System.Math,
    System.DateUtils,
    System.Variants,
    {$IFDEF MSWINDOWS}
      System.Win.Registry,
      System.Win.ComObj,
      Winapi.Windows,
    {$ENDIF}
    {$IFNDEF GIS_NOADO}
      {$IFDEF ADOINTUNIT}
        Winapi.ADOint,
      {$ELSE}
        Lider.CG.GIS.GeoAdoInt,
      {$ENDIF}
    {$ENDIF}
    {$IFNDEF GIS_NODB}
      Data.DB,
    {$ENDIF}
    Data.FmtBcd,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

//----------------------------------------------------------------------------
// general GIS types and declarations
//----------------------------------------------------------------------------

{$IFNDEF NEXTGEN}
  {$MESSAGE WARN 'Consider avoid of using Ansi chars in all defintions'}
{$ELSE}
  type
    AnsiChar = Char ;
    AnsiString = String ;
    WideString = String ;
{$ENDIF}

  //----------------------------------------------------------------------------
  // general utility classes
  //----------------------------------------------------------------------------

   /// <summary>
   ///  Construct a part descriptor.
   /// </summary>
   /// <param name="_partType">
   /// Type index.
   /// </param>
   /// <param name="_levelOfDetail">
   ///  Level of detail.
   /// </param>
   /// <param name="_priority">
   ///  Priority of material drawing.
   /// </param>
   /// <param name="_material">
   ///  Material index.
   /// </param>
   /// <returns>
   ///  Part descriptor object.
   /// </returns>
   function GisPartDescriptor( const _partType      : Integer ;
                               const _levelOfDetail : Integer ;
                               const _priority      : Integer ;
                               const _material      : Integer
                              ) : TGIS_PartDescriptor ;

   /// <summary>
   ///  Construct a vector of single type values.
   /// </summary>
   /// <param name="_x">
   ///  X value.
   /// </param>
   /// <param name="_y">
   ///  Y value.
   /// </param>
   /// <param name="_z">
   ///  Z value.
   /// </param>
   /// <returns>
   ///  Single type vector.
   /// </returns>
   function GisSingleVector( const _x, _y, _z : Single ) : TGIS_SingleVector ;


   /// <summary>
   ///  Construct a color of single type values.
   /// </summary>
   /// <param name="_r">
   ///  Red value.
   /// </param>
   /// <param name="_g">
   ///  Green value.
   /// </param>
   /// <param name="_b">
   ///  Blue value.
   /// </param>
   /// <param name="_a">
   ///  Alpha value.
   /// </param>
   /// <returns>
   ///  Single type vector.
   /// </returns>
   function GisSingleColor( const _r, _g, _b, _a : Single ) : TGIS_SingleColor ;

   /// <summary>
   ///  Get empty label position.
   /// </summary>
   /// <returns>
   ///  Set for label positions.
   /// </returns>
   function GisGetEmptyLabelPosition: TGIS_LabelPositions ; inline ;
   /// <summary>
   ///  Get label position.
   /// </summary>
   /// <param name="_value">
   ///  Position of label.
   /// </param>
   /// <returns>
   ///  Set for label positions.
   /// </returns>
   function GisGetLabelPosition     ( _value    : TGIS_LabelPosition
                                    ) : TGIS_LabelPositions ; inline ;
   /// <summary>
   ///  Add label position.
   /// </summary>
   /// <param name = "_set">
   ///  Set for label positions.
   /// </param>
   /// <param name="_value">
   ///  Position of label.
   /// </param>
   /// <returns>
   ///  Set for label positions.
   /// </returns>
   function GisAddLabelPosition     ( _set      : TGIS_LabelPositions  ;
                                      _value    : TGIS_LabelPosition
                                    ) : TGIS_LabelPositions ; inline ;
   /// <summary>
   ///  Test label position.
   /// </summary>
   /// <param name="_value">
   ///  Position of label.
   /// </param>
   /// <param name = "_set">
   ///  Set for label positions.
   /// </param>
   /// <returns>
   ///  True if set includes label position.
   /// </returns>
   function GisTestLabelPosition    ( _value    : TGIS_LabelPosition ;
                                      _set      : TGIS_LabelPositions
                                    ) : Boolean ; inline ;

   /// <summary>
   /// Get empty file type.
   /// </summary>
   /// <returns>
   ///  Set for checking supported files.
   /// </returns>
   function GisGetEmptyFileType     : TGIS_FileTypes ; inline ;

   /// <summary>
   /// Get file type.
   /// </summary>
   /// <param name="_value">
   /// Shape type.
   /// </param>
   /// <returns>
   ///  Set for checking supported files.
   /// </returns>
   function GisGetFileType          ( _value    : TGIS_FileType
                                    ) : TGIS_FileTypes ; inline ;

   /// <summary>
   /// Add  type.
   /// </summary>
   /// <param name="_set">
   ///  Set for checking supported files.
   /// </param>
   /// <param name="_value">
   /// Shape type.
   /// </param>
   /// <returns>
   ///  Set for checking supported files.
   /// </returns>
   function GisAddFileType          ( _set      : TGIS_FileTypes ;
                                      _value    : TGIS_FileType
                                    ) : TGIS_FileTypes ; inline ;

   /// <summary>
   /// Test file type.
   /// </summary>
   /// <param name="_value">
   /// Shape type.
   /// </param>
   /// <param name="_set">
   ///  Set for checking supported files.
   /// </param>
   /// <returns>
   ///  True if set includes file type.
   /// </returns>
   function GisTestFileType         ( _value    : TGIS_FileType ;
                                      _set      : TGIS_FileTypes
                                    ) : Boolean ; inline ;

   /// <summary>
   /// Get empty shape type.
   /// </summary>
   /// <returns>
   ///  Set for checking shape supported by layer.
   /// </returns>
   function GisGetEmptyShapeType    : TGIS_ShapeTypes ; inline ;

   /// <summary>
   /// Get shape type.
   /// </summary>
   /// <param name="_value">
   /// Shape type.
   /// </param>
   /// <returns>
   ///  Set for checking shape supported by layer.
   /// </returns>
   function GisGetShapeType         ( _value    : TGIS_ShapeType
                                    ) : TGIS_ShapeTypes ; inline ;

   /// <summary>
   /// Add shape type.
   /// </summary>
   /// <param name="_set">
   ///  Set for checking shape supported by layer.
   /// </param>
   /// <param name="_value">
   /// Shape type.
   /// </param>
   /// <returns>
   ///  Set for checking shape supported by layer.
   /// </returns>
   function GisAddShapeType         ( _set      : TGIS_ShapeTypes ;
                                      _value    : TGIS_ShapeType
                                    ) : TGIS_ShapeTypes ; inline ;

   /// <summary>
   /// Test shape type.
   /// </summary>
   /// <param name="_value">
   /// Shape type.
   /// </param>
   /// <param name="_set">
   ///  Set for checking shape supported by layer.
   /// </param>
   /// <returns>
   ///  True if set includes shape type.
   /// </returns>
   function GisTestShapeType        ( _value    : TGIS_ShapeType ;
                                      _set      : TGIS_ShapeTypes
                                    ) : Boolean ; inline ;

   /// <summary>
   /// Get empty font style.
   /// </summary>
   /// <returns>
   ///  Set for checking font style.
   /// </returns>
   function GisGetEmptyFontStyle    : TGIS_FontStyles ; inline ;

   /// <summary>
   /// Get font style.
   /// </summary>
   /// <param name="_value">
   /// font style
   /// </param>
   /// <returns>
   ///  Set for checking font style.
   /// </returns>
   function GisGetFontStyle         ( _value    : TGIS_FontStyle
                                    ) : TGIS_FontStyles ; inline ;

   /// <summary>
   /// Add font style.
   /// </summary>
   /// <param name="_set">
   ///  Set for checking font style.
   /// </param>
   /// <param name="_value">
   /// font style.
   /// </param>
   /// <returns>
   ///  Set for checking font style.
   /// </returns>
   function GisAddFontStyle         ( _set      : TGIS_FontStyles ;
                                      _value    : TGIS_FontStyle
                                    ) : TGIS_FontStyles ; inline ;

   /// <summary>
   /// Remove font style.
   /// </summary>
   /// <param name="_set">
   ///  Set for checking font style.
   /// </param>
   /// <param name="_value">
   /// font style.
   /// </param>
   /// <returns>
   ///  Set for checking font style.
   /// </returns>
   function GisRemoveFontStyle      ( _set      : TGIS_FontStyles ;
                                      _value    : TGIS_FontStyle
                                    ) : TGIS_FontStyles ; inline ;

   /// <summary>
   /// Test font style.
   /// </summary>
   /// <param name="_value">
   /// Font style.
   /// </param>
   /// <param name="_set">
   ///  Set for checking font style.
   /// </param>
   /// <returns>
   ///  True if set includes font style.
   /// </returns>
   function GisTestFontStyle        ( _value    : TGIS_FontStyle ;
                                      _set      : TGIS_FontStyles
                                    ) : Boolean ; inline ;

   /// <summary>
   /// Get empty dimension type.
   /// </summary>
   /// <returns>
   ///  Set for checking dimension supported by layer.
   /// </returns>
   function GisGetEmptyDimensionType: TGIS_DimensionTypes ; inline ;

   /// <summary>
   /// Get dimension type.
   /// </summary>
   /// <param name="_value">
   ///  Dimension type.
   /// </param>
   /// <returns>
   ///  Set for checking dimension supported by layer.
   /// </returns>
   function GisGetDimensionType     ( _value    : TGIS_DimensionType
                                    ) : TGIS_DimensionTypes ; inline ;

   /// <summary>
   /// Get operation type.
   /// </summary>
   /// <param name="_value">
   ///  operation type.
   /// </param>
   /// <returns>
   ///  Set for checking operation supported by layer.
   /// </returns>
   function GisGetOperationType     ( _value    : TGIS_OperationType
                                    ) : TGIS_OperationTypes ; inline ;

   /// <summary>
   /// Add dimension type.
   /// </summary>
   /// <param name="_set">
   ///  Set for checking dimension supported by layer.
   /// </param>
   /// <param name="_value">
   ///  Dimension type.
   /// </param>
   /// <returns>
   ///  Set for checking dimension supported by layer.
   /// </returns>
   function GisAddDimensionType     ( _set      : TGIS_DimensionTypes ;
                                      _value    : TGIS_DimensionType
                                    ) : TGIS_DimensionTypes ; inline ;

   /// <summary>
   /// Add operation type.
   /// </summary>
   /// <param name="_set">
   ///  Set for checking operation supported by layer.
   /// </param>
   /// <param name="_value">
   ///  operation type.
   /// </param>
   /// <returns>
   ///  Set for checking operation supported by layer.
   /// </returns>
   function GisAddOperationType     ( _set      : TGIS_OperationTypes ;
                                      _value    : TGIS_OperationType
                                    ) : TGIS_OperationTypes ; inline ;

   /// <summary>
   /// Test dimension type.
   /// </summary>
   /// <param name="_value">
   ///  Dimension type.
   /// </param>
   /// <param name="_set">
   ///  Set for checking dimension supported by layer.
   /// </param>
   /// <returns>
   ///  True if set include dimension type.
   /// </returns>
   function GisTestDimensionType    ( _value    : TGIS_DimensionType ;
                                      _set      : TGIS_DimensionTypes
                                    ) : Boolean ; inline ;

   /// <summary>
   /// Check if value of lock is less than specified value.
   /// </summary>
   /// <param name="_lock">
   ///  Types of shape locking.
   /// </param>
   /// <param name="_value">
   ///  Threshold value.
   /// </param>
   /// <returns>
   ///  True if _lock is less than _value.
   /// </returns>
   function GisLockLessThan         ( const _lock  : TGIS_Lock ;
                                      const _value : TGIS_Lock
                                    ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
   /// <summary>
   /// Check if value of lock is greater than specified value.
   /// </summary>
   /// <param name="_lock">
   ///  Types of shape locking.
   /// </param>
   /// <param name="_value">
   ///  Threshold value.
   /// </param>
   /// <returns>
   ///  True if _lock is greater than _value.
   /// </returns>
   function GisLockGreaterThan      ( const _lock  : TGIS_Lock ;
                                      const _value : TGIS_Lock
                                    ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
   /// <summary>
   /// Check if value of lock is greater than or equal to specified value.
   /// </summary>
   /// <param name="_lock">
   ///  Types of shape locking.
   /// </param>
   /// <param name="_value">
   ///  Threshold value.
   /// </param>
   /// <returns>
   ///  True if _lock is greater than or equal to _value.
   /// </returns>
   function GisLockGreaterThanEqual ( const _lock  : TGIS_Lock ;
                                      const _value : TGIS_Lock
                                    ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

   /// <summary>
   /// Check if value of lock is less than or equal to specified value.
   /// </summary>
   /// <param name="_lock">
   ///  Types of shape locking.
   /// </param>
   /// <param name="_value">
   ///  Threshold value.
   /// </param>
   /// <returns>
   ///  True if _lock is less than or equal to _value.
   /// </returns>
   function GisLockLessThanEqual    ( const _lock  : TGIS_Lock ;
                                      const _value : TGIS_Lock
                                    ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

  /// <summary>
  ///   Returns file age.
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Date and time.
  /// </returns>
  function  GisFileAge         ( const _path   : String
                               ) : TDateTime ;


  /// <summary>
  ///   Copy memory block from a buffer to another buffer.
  /// </summary>
  /// <param name="_srcBuf">
  ///   source buffer
  /// </param>
  /// <param name="_srcOffset">
  ///   start offset of source buffer
  /// </param>
  /// <param name="_dstBuf">
  ///   destination buffer
  /// </param>
  /// <param name="_dstOffset">
  ///   start offset of destination buffer
  /// </param>
  /// <param name="_count">
  ///   count of bytes to copy
  /// </param>
  procedure GisCopyMemory     ( const _srcBuf    : TBytes ;
                                const _srcOffset : Integer ;
                                const _dstBuf    : TBytes ;
                                const _dstOffset : Integer ;
                                const _count     : Integer
                              ) ; inline;

  /// <summary>
  ///   Copy memory block from a buffer to another buffer.
  /// </summary>
  /// <param name="_srcBuf">
  ///   source buffer
  /// </param>
  /// <param name="_srcOffset">
  ///   start offset of source buffer
  /// </param>
  /// <param name="_dstBuf">
  ///   destination buffer
  /// </param>
  /// <param name="_dstOffset">
  ///   start offset of destination buffer
  /// </param>
  /// <param name="_count">
  ///   count of bytes to copy
  /// </param>
  procedure GisCopyMemoryEx ( const _srcBuf    : TGIS_SingleArray ;
                              const _srcOffset : Integer ;
                              const _dstBuf    : TGIS_SingleArray ;
                              const _dstOffset : Integer ;
                              const _count     : Integer
                            ) ; overload; inline;


  /// <summary>
  ///   Copy memory block from a buffer to another buffer.
  /// </summary>
  /// <param name="_srcBuf">
  ///   source buffer
  /// </param>
  /// <param name="_srcOffset">
  ///   start offset of source buffer
  /// </param>
  /// <param name="_dstBuf">
  ///   destination buffer
  /// </param>
  /// <param name="_dstOffset">
  ///   start offset of destination buffer
  /// </param>
  /// <param name="_count">
  ///   count of bytes to copy
  /// </param>
  procedure GisCopyPixels   ( const _srcBuf    : TGIS_Pixels ;
                              const _srcOffset : Integer ;
                              const _dstBuf    : TGIS_Pixels ;
                              const _dstOffset : Integer ;
                              const _count     : Integer
                            ) ; inline;

  //----------------------------------------------------------------------------
  // general utility functions & procedures
  //----------------------------------------------------------------------------

  {$IFDEF OXYGENE}
    function  IsSameType      ( const _obj1 : TObject ;
                                const _obj2 : TObject
                              ) : Boolean ; overload;
  {$ELSE}
    /// <summary>
    ///   Check if objects are of the same type.
    /// </summary>
    /// <param name="_obj1">
    ///   First object.
    /// </param>
    /// <param name="_obj2">
    ///   Second object.
    /// </param>
    /// <returns>
    ///   True if objects are of the same type.
    /// </returns>
    function  IsSameType      ( const _obj1 : TObject ;
                                const _obj2 : TObject
                              ) : Boolean ; overload;
  {$ENDIF}
  {$IFDEF OXYGENE}
    {$IFDEF CLR}
      function  IsSameType    ( const _obj  : System.Object ;
                                const _type : System.Type
                              ) : Boolean ; overload;
    {$ENDIF}
    {$IFDEF JAVA}
      function  IsSameType    ( const _obj  : TObject ;
                                const _type : &Class
                              ) : Boolean ; overload;
    {$ENDIF}
  {$ELSE}

    /// <summary>
    ///   Check if object is of the specified type.
    /// </summary>
    /// <param name="_obj">
    ///    Object.
    /// </param>
    /// <param name="_type">
    ///    Specified type.
    /// </param>
    /// <returns>
    ///   True if object type is the same as _type.
    /// </returns>
    function  IsSameType      ( const _obj  : TObject ;
                                const _type : TClass
                              ) : Boolean ; overload;

    /// <summary>
    ///   Check if object and second object passed as interface are of the same type.
    /// </summary>
    /// <param name="_obj1">
    ///    Object.
    /// </param>
    /// <param name="_obj2">
    ///    Object passed as interface.
    /// </param>
    /// <returns>
    ///   True if objects are of the same type.
    /// </returns>
    function  IsSameType      ( const _obj1 : TObject ;
                                const _obj2 : IInterface
                              ) : Boolean ; overload;
  {$ENDIF}

  /// <summary>
  ///   Returns the name of the class.
  /// </summary>
  /// <param name="_obj">
  ///   Object.
  /// </param>
  /// <returns>
  ///   Class Name of an object.
  /// </returns>
  function  GetClassName      ( const _obj  : TObject
                              ) : String ;

  /// <summary>
  ///   Returns the short path form of a specified input path. Function
  ///   provides 8.3 path equivalent without national characters, spaces etc.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Short path of a specified input path.
  /// </returns>
  function  GetShortPath    ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path given by _path but guaranteed directory separator at
  ///   then end
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <returns>
  ///   Path a given by _path but with guaranteed directory separator at the end
  /// </returns>
  function  GetPathDirSep     ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path given by _path but without an ending extension.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path given by _path but without an ending extension.
  /// </returns>
  function  GetPathNoExt      ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path given by _path but relative to directory given by _dir.
  ///   Path recognized as URL and path with CE delimited strings will not be
  ///   altered.
  /// </summary>
  /// <param name="_dir">
  ///   reference directory
  /// </param>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path given by _path but relative to directory given by _dir.
  /// </returns>
  function  GetPathRelative   ( const _dir  : String ;
                                const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path given by _path but absolute (based on relative directory
  ///   given by _dir.). Path recognized as URL and path with CE delimited
  ///   strings will not be altered.
  /// </summary>
  /// <param name="_dir">
  ///   reference directory; if empty then path will be calculated relative to
  ///   the current directory
  /// </param>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path given by _path but absolute (based on relative directory
  ///   given by _dir.).
  /// </returns>
  function  GetPathAbsolute   ( const _dir  : String ;
                                const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path w/o any sensitive login information if _path can be
  ///   recognized as a SQL embedded path.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path w/o any sensitive login information if _path can be
  ///   recognized as a SQL embedded path.
  /// </returns>
  function  GetSafeSQLPath    ( const _path : String ) : String  ;

  /// <summary>
  ///   Utility function to test if path represents an embedded SQL connection
  ///   (like embedded TTKLS file)
  /// </summary>
  /// <param name="_path">
  ///   path to be tested
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   If true path represents an embedded SQL connection.
  /// </returns>
  function  IsEmbeddedSQLPath ( const _path : String ) : Boolean ;

  /// <summary>
  ///   Returns True if provided path points to a server or embedded SQL layer.
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    TTKLS or TTKPS files will not be treated as a server path but as a
  ///    files.
  ///    </note>
  ///   Function will not test if provided server path points to existing
  ///   sever, but only will test if path looks like server URL or embedded SQL
  ///   layer.
  /// </remarks>
  /// <returns>
  ///  True if provided path points to a server or embedded SQL layer.
  /// </returns>
  function  IsServerPath      ( const _path : String ) : Boolean ;

  /// <summary>
  ///   Returns True if provided path points to a server, embedded SQL layer,
  ///   or existing file.
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   True if provided path points to a server, embedded SQL layer,
  ///   or existing file.
  /// </returns>
  function  IsFileOrServerPath( const _path : String ) : Boolean ;

  /// <summary>
  ///   Returns directory of a file. If file do not exists or path is an URL
  ///   then result will be empty String
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    function is thread save;
  ///    </note>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Directory of a file.
  /// </returns>
  function  GetFileDir        ( const _path : String
                              ) : String  ;

  /// <summary>
  ///   Change directory to directory provided by _path. If file do not exists
  ///   or path is an URL then result will be empty String and directory will
  ///   not be modified
  /// </summary>
  /// <param name="_path">
  ///   file path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function must be paired GisRestoreDir; Function is thread save;
  ///    GetPathAbsolute should not be used before GisRestoreDir will be called;
  ///    </note>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Change directory to directory provided by _path.
  /// </returns>
  function  ChangeDir         ( const _path : String
                              ) : String  ;

  /// <summary>
  ///   Restore directory to the directory obtained in GisChangeDir.
  /// </summary>
  /// <param name="_path">
  ///   previous directory
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Procedure must be paired GisChangeDirDir; Function is thread save;
  ///    GetPathAbsolute should not be used after GisChangeDir will be called;
  ///    </note>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  procedure RestoreDir        ( const _path : String
                              ) ;

  /// <summary>
  ///   Check if directory exists.
  /// </summary>
  /// <param name="_path">
  ///   path to directory
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   True if directory exists.
  /// </returns>
  function  CheckDir          ( const _path : String
                              ) : Boolean ;

  /// <summary>
  ///   Check if file can be written: directory is proper,file is not read-only
  ///   and is not used by any other application.
  /// </summary>
  /// <param name="_path">
  ///   path to be checked
  /// </param>
  /// <returns>
  ///   True if file can be written.
  /// </returns>
  function  CheckFileWriteAccess
                              ( const _path : String
                              ) : Boolean ;

  /// <summary>
  ///   Check if file can be written: directory is proper,file is not read-only
  ///   and is not used by any other application.
  /// </summary>
  /// <param name="_path">
  ///   path to be checked
  /// </param>
  /// <param name="_plain">
  ///   if true then only plain file path will be checked
  /// </param>
  /// <param name="_backup">
  ///   if true then backup file (*.~*) will be checked
  /// </param>
  /// <param name="_temp">
  ///   if true then temporary (*.*~tmp) will be checked
  /// </param>
  /// <returns>
  ///   True if file can be written.
  /// </returns>
  function  CheckFileWriteAccessEx
                              ( const _path   : String  ;
                                const _plain  : Boolean ;
                                const _backup : Boolean ;
                                const _temp   : Boolean
                              ) : Boolean ;

  /// <summary>
  ///   Supersede default GetCurrentDir to guarantee proper operation on WINCE.
  ///   On platforms other then WINCE just calls GetCurrentDir.
  /// </summary>
  /// <returns>
  ///  Current dir.
  /// </returns>
  function  GetCurrentDirEx   : String ;

  /// <summary>
  ///   Supersede default SetCurrentDir to guarantee proper operation on WINCE.
  ///   On platforms other then WINCE just calls SetCurrentDir.
  /// </summary>
  /// <param name="_path">
  ///   path to be set
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>

  procedure SetCurrentDirEx   ( const _path : String ) ;

  /// <summary>
  ///   Supersede default ExpandFileName to guarantee proper operation on
  ///   WINCE. On platforms other then WINCE just calls ExpandFileName.
  /// </summary>
  /// <param name="_path">
  ///   path to be expanded
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Expanded path.
  /// </returns>
  function  ExpandFileNameEx  ( const _path : String ) : String ;

  /// <summary>
  ///   Return temporary file name.
  /// </summary>
  /// <remarks>
  ///   File name returned by this function is without any extension. Returned
  ///   file name can be use as a base for creation a new layer etc.
  /// </remarks>
  /// <returns>
  ///   Temporary file name.
  /// </returns>
  function  GetTempFileName   : String ;

  /// <summary>
  ///   Return temporary folder name.
  /// </summary>
  /// <remarks>
  ///   File name returned by this function is without any extension. Returned
  ///   file name can be use as a base for creation a new layer etc.
  /// </remarks>
  /// <returns>
  ///   Temporary folder name.
  /// </returns>
  function  GetTempFolder     : String ;

  /// <summary>
  ///   Returns a path to Common Files\TatukGIS.
  /// </summary>
  /// <returns>
  ///  Path to Common Files\TatukGIS.
  /// </returns>
  function  GetPathCommonFiles: String ;

  /// <summary>
  ///   Returns path part of a given path.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path part of a given path.
  /// </returns>
  function  GetFilePath       ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns a file name part (including extension) of a given path.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  /// file name part (including extension) of a given path.
  /// </returns>
  function  GetFileName       ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns an extension of a given path.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Extension of a given path.
  /// </returns>
  function  GetFileExt        ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path part of a given path but without an ending extension.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path part of a given path but without an ending extension.
  /// </returns>
  function  GetFileNameNoExt  ( const _path : String ) : String  ;

  /// <summary>
  ///   Returns a path part of a given path but without an ending extension.
  /// </summary>
  /// <param name="_path">
  ///   given path
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Path part of a given path but without an ending extension.
  /// </returns>
  function  GetServerName     ( const _path : String ) : String  ;

  /// <summary>
  ///   Prepare backup name /*.~*/ from given path.
  /// </summary>
  /// <param name="_path">
  ///   path to be converted - must be full (with extension)
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Backup name /*.~*/ from given path.
  /// </returns>
  function  GetBackupName     ( const _path : String ) : String  ;

  /// <summary>
  ///   Prepare temporary name /*.*~/ from given path.
  /// </summary>
  /// <param name="_path">
  ///   path to be converted - must be full - with extension
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Function expands any embedded aliases (see GisAliasList)
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Temporary name /*.*~/ from given path.
  /// </returns>
  function  GetTemporaryName  ( const _path : String ) : String  ;

  /// <summary>
  ///   Safe version of FileExits.
  /// </summary>
  /// <param name="_path">
  ///   file path to be verified
  /// </param>
  /// <remarks>
  ///   Under .NET FileExits can throw exception if path is not properly  formed
  /// </remarks>
  /// <returns>
  ///   True if file exists.
  /// </returns>
  function  SafeFileExists    ( const _path : String ) : Boolean ;

  /// <summary>
  ///   Safe version of DirectoryExits.
  /// </summary>
  /// <param name="_dir">
  ///   dir path to be verified
  /// </param>
  /// <remarks>
  ///   Under .NET DirectoryExits can throw exception if path is not properly
  ///   formed
  /// </remarks>
  /// <returns>
  ///   True if directory exists.
  /// </returns>
  function  SafeDirectoryExists( const _dir : String ) : Boolean ;

  {$IFNDEF ISLAND}
    {$IFNDEF JAVA}

      /// <summary>
      ///   Loads a DLL library with searching for within a module instance
      ///   path first. Calling path for COM object relates by default to a
      ///   host application, not to the COM itself. As a result dynamic load
      ///   libraries could never be loaded. This function will search first in
      ///   HInstance path.
      /// </summary>
      /// <param name="_fileName">
      ///   same as Windows.LoadLibrary
      /// </param>
      /// <returns>
      ///   Base address of DLL library.
      /// </returns>
      function LoadLibraryWithinHinstance
                                ( const _fileName : String
                                ) : HMODULE ;
    {$ENDIF}
  {$ENDIF}


    {$IFDEF CLR}
      function ConvertNETFieldCP
                            ( const _source   : DataRow    ;
                              const _field    : DataColumn ;
                              const _sourceCP : Integer
                            ) : Variant ; overload;
      function ConvertNETFieldCP
                            ( const _source   : IDataReader ;
                              const _field    : DataRow    ;
                              const _sourceCP : Integer
                            ) : Variant ; overload;
    {$ENDIF}

  {$IFNDEF GIS_NOADO}
    /// <summary>
    ///   Perform code page conversion (only if _source is String).
    /// </summary>
    /// <param name="_source">
    ///   variant to be converted
    /// </param>
    /// <param name="_sourceCP">
    ///   code page of source String (converted from)
    /// </param>
    /// <remarks>
    ///   <note type="note">
    ///    Function will also perform a type casting for types unsupported by
    ///    variant type.
    ///    </note>
    /// </remarks>
    /// <returns>
    ///   Converted ADO Field.
    /// </returns>
    function  ConvertADOFieldCP
                            ( const _source   : Field      ;
                              const _sourceCP : Integer
                            ) : Variant ;
  {$ENDIF}
  {$IFNDEF GIS_NOJDBC}
    /// <summary>
    ///   Perform code page conversion (only if _source is String).
    /// </summary>
    /// <param name="_source">
    ///   variant to be converted
    /// </param>
    /// <param name="_sourceCP">
    ///   code page of source String (converted from)
    /// </param>
    /// <remarks>
    ///   <note type="note">
    ///    Function will also perform a type casting for types unsupported by
    ///    variant type.
    ///    </note>
    /// </remarks>
    /// <returns>
    ///   Converted JDBC Field.
    /// </returns>
    function  ConvertJDBCFieldCP ( const _source   : String      ;
                                   const _sourceCP : Integer
                                 ) : Variant ;
  {$ENDIF}

  {$IFNDEF GIS_NODB}

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
    ///   Converted DB Field.
    /// </returns>
    function  ConvertDBFieldCP( const _source   : TField    ;
                                const _sourceCP : Integer
                              ) : Variant ;
  {$ENDIF}

  {$IFNDEF GIS_NODB}

    /// <summary>
    ///   Return field specification for use by TGIS_DataSet. For example blob
    ///   fields will be ignored etc.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <param name="_subtype">
    ///   field type to which source field will be converted; ftUnknown if
    ///   field can not be used
    /// </param>
    /// <param name="_subsize">
    ///   field size for user defined field sizes (like ftString) for other
    ///   will always be 0
    /// </param>
    procedure JoinFieldInfo   ( const _source   : TField     ;
                                var   _subtype  : TFieldType ;
                                var   _subsize  : Integer
                              ) ; overload;

    /// <summary>
    ///   Verify if field can be used in queries, attributes etc. For example
    ///   blob fields will be ignored etc. Will be use to ignore unsupported
    ///   fields in queries, control attributes etc.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldInfo   ( const _source   : TField
                              ) : Boolean ; overload;

    /// <summary>
    ///   Verify if a field can be used in queries and returns a layer type that
    ///   can represent it.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <param name="_subtype">
    ///   field type to which source field will be converted
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldType   ( const _source   : TField ;
                                  var _subtype  : TGIS_FieldType
                              ) : Boolean ; overload;
  {$ENDIF}

  {$IFDEF CLR}
    /// <summary>
    ///   Verify if field can be used in queries, attributes etc. For example
    ///   blob fields will be ignored etc. Will be use to ignore unsupported
    ///   fields in queries, control attributes etc.
    /// </summary>
    /// <param name="_type">
    ///   field type to which source field will be converted
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function JoinFieldInfo( const _type     : System.Type
                          ) : Boolean ; overload;

    /// <summary>
    ///   Verify if a field can be used in queries and returns a layer type that
    ///   can represent it.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <param name="_subtype">
    ///   field type to which source field will be converted
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldType   ( const _source   : System.Type ;
                                  var _subtype  : TGIS_FieldType
                              ) : Boolean ; overload;
  {$ENDIF}

  {$IFNDEF GIS_NOADO}

    /// <summary>
    ///   Return field specification for use by TGIS_DataSet. For example
    ///   blob fields will be ignored etc.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <param name="_subtype">
    ///   field type to which source field will be converted; ftUnknown if
    ///   field can not be used
    /// </param>
    /// <param name="_subsize">
    ///   field size for user defined filed sizes (like ftString) for other
    ///   will always be 0
    /// </param>
    procedure JoinFieldInfo ( const _source   : Field      ;
                              var  _subtype   : TFieldType ;
                              var  _subsize   : Integer
                            ) ; overload;

    /// <summary>
    ///   Verify if field can be used in queries, attributes etc. For example
    ///   blob fields will be ignored etc. Will be use to ignore unsupported
    ///   fields in queries, control attributes etc.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldInfo ( const _source   : Field
                            ) : Boolean ; overload;

    /// <summary>
    ///   Verify if a field can be used in queries and returns a layer type that
    ///   can represent it.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <param name="_subtype">
    ///   field type to which source field will be converted
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldType   ( const _source   : Field;
                                  var _subtype  : TGIS_FieldType
                              ) : Boolean ; overload;
  {$ENDIF}

  {$IFNDEF GIS_NOJDBC}
    /// <summary>
    ///   Verify if field can be used in queries, attributes etc. For example
    ///   blob fields will be ignored etc. Will be use to ignore unsupported
    ///   fields in queries, control attributes etc.
    /// </summary>
    /// <param name="_columnType">
    ///   field type to be verified
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldInfo ( const _columnType   : Integer
                            ) : Boolean ;

    /// <summary>
    ///   Verify if a field can be used in queries and returns a layer type that
    ///   can represent it.
    /// </summary>
    /// <param name="_source">
    ///   field to be verified
    /// </param>
    /// <param name="_subtype">
    ///   field type to which source field will be converted
    /// </param>
    /// <returns>
    ///   True if field is supported.
    /// </returns>
    function  JoinFieldType   ( const _source   : Integer ;
                                  var _subtype  : TGIS_FieldType
                              ) : Boolean ;
  {$ENDIF}

  /// <summary>
  ///   De-quote String from " character.
  /// </summary>
  /// <param name="_str">
  ///   String to be dequoted
  /// </param>
  /// <returns>
  ///   De-quoted String.
  /// </returns>
  function  GetDequotedStr  ( const _str      : String
                            ) : String ; overload;

  /// <summary>
  ///   De-quote String from " character.
  /// </summary>
  /// <param name="_str">
  ///   String to be dequoted
  /// </param>
  /// <param name="_quote">
  ///   Quotation mark.
  /// </param>
  /// <returns>
  ///  De-quoted String.
  /// </returns>
  function  GetDequotedStr    ( const _str      : String ;
                                const _quote    : Char
                              ) : String ; overload;

  /// <summary>
  ///   Replace &lt;#token#&gt; occurrence in _text based on token list.
  /// </summary>
  /// <param name="_text">
  ///   text to be changed
  /// </param>
  /// <param name="_tokenlist">
  ///   list of tokens in a form "token=replacement"
  /// </param>
  /// <param name="_callback">
  ///   used only if _tokenlist is nil; for each token the callback will be
  ///   called to resolve the value
  /// </param>
  /// <param name="_leavetoken">
  ///   If True, token marker won't be removed
  /// </param>
  /// <returns>
  ///   Token value.
  /// </returns>
  function  TemplateProducer  ( const _text      : String      ;
                                const _tokenlist : TGIS_StringList ;
                                const _callback  : TGIS_TemplateProducerCallBack ;
                                const _leavetoken: Boolean
                              ) : String ;

  /// <summary>
  ///   Replace _token_src with _token_dst inside SQL command.
  /// </summary>
  /// <param name="_command">
  ///   SQL command
  /// </param>
  /// <param name="_token_src">
  ///   token to be replaced
  /// </param>
  /// <param name="_token_dst">
  ///   replacement token
  /// </param>
  /// <returns>
  ///   Token value.
  /// </returns>
  function  ReplaceSQLToken   ( const _command   : String ;
                                const _token_src : String ;
                                const _token_dst : String
                              ) : String ;

  /// <summary>
  ///  Expand macros inside SQL command. Currently supported macros are:
  /// </summary>
  /// <param name="_command">
  ///   SQL command
  /// </param>
  /// <param name="_lst">
  ///   list with dialect definitions like GIS_SQL_DIALECT_MSJET
  /// </param>
  /// <returns>
  ///   Macros value.
  /// </returns>
  function  ExpandSQLMacros   ( const _command   : String ;
                                const _lst       : TGIS_StringList
                              ) : String ;

  /// <summary>
  ///   Verify if SQL command contains any predefine tokens.
  /// </summary>
  /// <param name="_command">
  ///   SQL command
  /// </param>
  /// <param name="_tokens">
  ///   list of predefined tokens separated by #13#10
  /// </param>
  /// <returns>
  ///   True if SQL command contains any predefine tokens.
  /// </returns>
  function  TestSQLTokens     ( const _command   : String ;
                                const _tokens    : String
                              ) : Boolean ;

  /// <summary>
  ///   Returns, as a String, the content of file in a path related to
  ///   GetPathCommonFiles directory.
  /// </summary>
  /// <param name="_subdir">
  ///   path related to Program Files\Common Files\TatukGIS directory
  /// </param>
  /// <param name="_filename">
  ///   name of the file
  /// </param>
  /// <returns>
  ///  Content of file in a path related to
  ///   GetPathCommonFiles directory.
  /// </returns>
  function  GetCommonFilesItem
                            ( const _subdir    : String ;
                              const _filename  : String
                            ) : String ;

  /// <summary>
  ///   Set the String content of the file in a path related to
  ///   GetPathCommonFiles directory.
  /// </summary>
  /// <param name="_subdir">
  ///   path related to Program Files\TatukGIS directory
  /// </param>
  /// <param name="_filename">
  ///   name of the file
  /// </param>
  /// <param name="_content">
  ///   content to be set; if content is empty the any existing file will be
  ///   deleted
  /// </param>
  /// <exception cref="exception">
  ///   if file can not be saved
  /// </exception>
  procedure SetCommonFilesItem
                            ( const _subdir    : String ;
                              const _filename  : String ;
                              const _content   : String
                            ) ;

  /// <summary>
  ///   Read all parameters from TTKLS or TTKPS file. If _path is not
  ///   pointing to the file then will be treated as CRLF or '\n' delimited
  ///   list of parameters.
  /// </summary>
  /// <param name="_path">
  ///   path or list of parameters
  /// </param>
  /// <param name="_lst">
  ///   lst list to hold read parameters; could be nil
  /// </param>
  /// <returns>
  ///  Provider ID.
  /// </returns>
  function  ReadSQLParamsFromPath
                            ( const _path      : String   ;
                              const _lst       : TGIS_Strings
                            ) : Integer ; overload;

  /// <summary>
  ///   Read all parameters from TTKLS or TTKPS file. If _path is not pointing
  ///   to the file then will be treated as CRLF or '\n' delimited list of
  ///   parameters.
  /// </summary>
  /// <param name="_path">
  ///   path or list of parameters
  /// </param>
  /// <param name="_lst">
  ///   lst list to hold read parameters; could be nil
  /// </param>
  /// <param name="_nofile">
  ///   If True then file based params will be ignored (only path String will
  ///   be parsed)
  /// </param>
  /// <returns>
  ///   Provider ID.
  /// </returns>
  function  ReadSQLParamsFromPath
                            ( const _path      : String   ;
                              const _lst       : TGIS_Strings ;
                              const _nofile    : Boolean
                            ) : Integer ; overload;

  /// <summary>
  ///   Read single parameter from TTKLS or TTKPS file. If _path is not
  ///   pointing to the file then will be treated as CRLF or '\n' delimited
  ///   list of parameters.
  /// </summary>
  /// <param name="_path">
  ///   path or list of parameters
  /// </param>
  /// <param name="_name">
  ///   name of the parameter
  /// </param>
  /// <param name="_value">
  ///   read value of the parameter
  /// </param>
  /// <returns>
  ///   Provider ID.
  /// </returns>
  function  GetSQLParamFromPath
                              ( const _path      : String ;
                                const _name      : String ;
                                var   _value     : String
                              ) : Integer ;
  /// <summary>
  ///  Read parameters from specified path.
  /// </summary>
  /// <param name="_path">
  ///  Path for parameters.
  /// </param>
  /// <param name="_lst">
  /// list of parameters.
  /// </param>
  procedure ReadParamsFromPath
                              ( const _path      : String   ;
                                const _lst       : TGIS_Strings
                              ) ;

  /// <summary>
  ///   Read single parameter from TTKWV or TTKWP file. If _path is not
  ///   pointing to the file then will be treated as CRLF or '\n' delimited
  ///   list of parameters.
  /// </summary>
  /// <param name="_path">
  ///   path or list of parameters
  /// </param>
  /// <param name="_name">
  ///   name of the parameter
  /// </param>
  /// <returns>
  ///   Param value.
  /// </returns>
  function  GetParamFromPath
                              ( const _path      : String ;
                                const _name      : String
                              ) : String ;

  /// <summary>
  ///   Returns, as a String, the content of a SQL Dialect.
  /// </summary>
  /// <param name="_dialect">
  ///   name of the dialect
  /// </param>
  /// <remarks>
  ///   <para>
  ///     Dialect will be reviewed form the: internal list or files located
  ///     in Program Files\Common Files\TatukGIS\SQLDialect directory.
  ///   </para>
  ///   <para>
  ///      Files in a directory should be named without extension.
  ///   </para>
  ///   <para>
  ///      File in a directory will override the internal list.
  ///   </para>
  ///   <para>
  ///
  ///   </para>
  ///   <para>
  ///     Because of Oracle uniqueness is very important the first line of
  ///     the oracle related file will have the line:
  ///   </para>
  /// </remarks>
  /// <returns>
  ///   Content of a SQL Dialect.
  /// </returns>
  function  GetSQLDialect   ( const _dialect   : String
                            ) : String ;

    /// <summary>
    ///  Creates MSJET( Access, MDB) database.
    /// </summary>
    /// <param name="_path">
    ///   path to .mdb file
    /// </param>
    /// <returns>
    ///   provider String to a database
    /// </returns>
    function  CreateMSJET   ( const _path      : String
                            ) : String ;

  /// <summary>
  ///   Create a field name with 'db.' for joined db fields
  /// </summary>
  /// <param name="_name">
  ///   field name in a form "name
  /// </param>
  /// <returns>
  ///   Field name with 'db.' for joined db fields
  /// </returns>
  function  ToJoinFieldName   ( const _name      : String

                              ) : String ;

  /// <summary>
  ///   Extract field name without 'db.' prefix of joined fields
  /// </summary>
  /// <param name="_name">
  ///   field name in a form "db.name"
  /// </param>
  /// <returns>
  ///   Field name in a form "name".
  /// </returns>
  function  FromJoinFieldName ( const _name      : String
                              ) : String ;

  /// <summary>
  ///   Get path part the URL (part before and including '?').
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <returns>
  ///   Processed URL.
  /// </returns>
  function  URLGetPath        ( const _url       : String
                              ) : String ;

  /// <summary>
  ///   Get query part the URL (part after '?').
  /// </summary>
  /// <param name="_url">
  ///   url to be processed
  /// </param>
  /// <returns>
  ///   Processed URL.
  /// </returns>
  function  URLGetQuery       ( const _url       : String
                              ) : String ;

  /// <summary>
  ///   Get list of all parameters of the query part of the URL.
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <returns>
  ///   List of parameters in a form 'param=value'.
  /// </returns>
  /// <remarks>
  ///   <note type="note">
  ///    returned list should be freed by a calling application.
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   List of all parameters of the query part of the URL.
  /// </returns>
  function  URLGetParameters  ( const _url       : String
                              ) : TGIS_StringList ;

  /// <summary>
  ///   Delete parameter form the query part of the URL.
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <param name="_param">
  ///   parameter name
  /// </param>
  /// <returns>
  ///   Processed URL.
  /// </returns>
  function  URLDeleteParameter( const _url       : String ;
                                const _param     : String
                              ) : String ;

  /// <summary>
  ///   Test if parameter exists in the query part of the URL.
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <param name="_param">
  ///   parameter name
  /// </param>
  /// <returns>
  ///   True if parameter exists.
  /// </returns>
  function  URLTestParameter  ( const _url       : String ;
                                const _param     : String
                              ) : Boolean ;

  /// <summary>
  ///   Add parameter with to a provided URL. Markers '?' or '&' will be added
  ///   if required.
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <param name="_param">
  ///   parameter in a form 'param=value'
  /// </param>
  /// <returns>
  ///   Processed URL.
  /// </returns>
  function  URLAddParameter   ( const _url       : String ;
                                const _param     : String
                              ) : String ;


  /// <summary>
  ///   Get value of the parameter in decoded URL format.
  /// </summary>
  /// <param name="_encodedStr">
  ///   value to be processed
  /// </param>
  /// <returns>
  ///   Processed URL.
  /// </returns>
  function  URLDecode         ( const _encodedStr : String
                              ) : String ;

  /// <summary>
  ///   Get value of the parameter in encoded URL format.
  /// </summary>
  /// <param name="_decodedStr">
  ///   value to be processed
  /// </param>
  /// <returns>
  ///   Processed URL.
  /// </returns>
  function  URLEncode         ( const _decodedStr : String
                              ) : String ;

  /// <summary>
  ///   Get value of the parameter from the query part of the URL.
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <param name="_param">
  ///   parameter name
  /// </param>
  /// <remarks>
  ///   expected parameter format is:
  ///   ..&amp;parameter1=value1&amp;parameter2=value1&amp;.. etc.
  /// </remarks>
  /// <returns>
  ///   Value of the parameter from the query part of the URL.
  /// </returns>
  function  URLGetParameterValue(
                                const _url       : String ;
                                const _param     : String
                              ) : String ;

  /// <summary>
  ///   Get value of the parameter from the query part of the URL and delete it
  ///   from the url.
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <param name="_param">
  ///   parameter name
  /// </param>
  /// <remarks>
  ///   expected parameter format is:
  ///   ..&amp;parameter1=value1&amp;parameter2=value1&amp;.. etc.
  /// </remarks>
  /// <returns>
  ///   Value of the parameter from the query part of the URL.
  /// </returns>
  function  URLGetAndDeleteParameterValue(
                                var   _url       : String ;
                                const _param     : String
                              ) : String ;

  /// <summary>
  ///   Fix url for common format mistakes in '?' '&' markers like bad order,
  ///   or duplication
  /// </summary>
  /// <param name="_url">
  ///   URL to be processed
  /// </param>
  /// <returns>
  ///   Fixed URL.
  /// </returns>
  function URLFixed           ( const _url       : String
                              ) : String ;

  /// <summary>
  ///   Decode provided content type against value like 'image/png'.
  /// </summary>
  /// <param name="_content">
  ///   content type String
  /// </param>
  /// <param name="_basictypes">
  ///   if true then match all variation to a main type like
  ///   gisContentTypePng24 to gisContentTypePng
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Decoding is done by matching beginning of the String. So for example
  ///    'image/gif; something more' will match 'image/gif' sentinel.
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Decoded content type.
  /// </returns>
  function  DecodeContentType ( const _content    : String  ;
                                const _basictypes : Boolean
                              ) : TGIS_ContentType ;

  /// <summary>
  ///   Expand RTree path for file based of ForcedRTreePath.
  /// </summary>
  /// <param name="_path">
  ///   default .Rtree path
  /// </param>
  /// <returns>
  ///   Expanded RTree path.
  /// </returns>
  /// <remarks>
  ///   <list type="bullet">
  ///     <item>
  ///       If ForcedRtreePath is empty then default path of .rtree file is
  ///       used,
  ///     </item>
  ///     <item>
  ///       If ForcedRtreePath is no empty and .rtree file in a default
  ///       location does not exits then file will be managed in
  ///       ForcedRTreePath directory. Suppose that ForcedRtreePath is
  ///       'c:\tmp' and original file name is 'c:\data\file.shp.rtree'
  ///       then expanded path becomes: 'c:\tmp\c_data_file.shp.rtree'
  ///     </item>
  ///   </list>
  /// </remarks>
  function  ExpandForcedRTreePath(
                                const _path   : String
                              ) : String ;

  /// <summary>
  ///   Gets the time difference (in minutes) between the current time zone and
  ///   Coordinated Universal Time (UTC) including the Daylight Savings Time
  ///   (DTC).
  /// </summary>
  /// <returns>
  ///   Time difference (in minutes) between the current time zone and
  ///   Coordinated Universal Time (UTC) including the Daylight Savings Time
  /// </returns>
  function  GetTimeZoneBias     : Integer ;

  /// <summary>
  ///   Converts a TDateTime instance to a String in the native XML format (ISO
  ///   8601).
  /// </summary>
  /// <param name="_dtm">
  ///   TDateTime instance to be converted
  /// </param>
  /// <param name="_idt">
  ///   format identifier; if _idt &lt;= 0, format = 'yyyy-MM-dd' _idt = 1,
  ///   format = 'yyyy-MM-ddTHH:mm' _idt = 2, format = 'yyyy-MM-ddTHH:mm:ss'
  ///   _idt &gt;= 3, format = 'yyyy-MM-ddTHH:mm:ss.fffffff'
  /// </param>
  /// <param name="_btz">
  ///   if True then time zone String is added in the format 'zzz'; irrelevant
  ///   if _idt &lt;=0
  /// </param>
  /// <returns>
  ///   TDateTime as a String.
  /// </returns>
  function  DateTimeToXMLString(
                                      _dtm    : TDateTime ;
                                const _idt    : Integer   ;
                                const _btz    : Boolean
                              ) : String ;

  /// <summary>
  ///   Converts a String in the native XML format (ISO 8601) to a TDateTime
  ///   instance.
  /// </summary>
  /// <param name="_str">
  ///   ISO 8601 String to be converted
  /// </param>
  /// <returns>
  ///   TDateTime instance.
  /// </returns>
  function  XMLStringToDateTime(
                                const _str    : String
                              ) : TDateTime ;


  /// <summary>
  ///   Convert, YES, NO, TRUE, FALSE, 1, 0 to a boolean value.
  /// </summary>
  /// <param name="_value">
  ///   value to be converted
  /// </param>
  /// <param name="_default">
  ///   default value if provided value is empty or unrecognized
  /// </param>
  /// <returns>
  ///   Boolean value.
  /// </returns>
  function StrToBoolean       ( const _value     : String ;
                                const _default   : Boolean
                              ) : Boolean ;

  /// <summary>
  ///   Read parameter from list formatted as name=value.
  /// </summary>
  /// <param name="_list">
  ///   list to be read from
  /// </param>
  /// <param name="_name">
  ///   parameter name
  /// </param>
  /// <param name="_default">
  ///   default value to be used if name does not exists or is empty
  /// </param>
  /// <returns>
  ///   Parameter value or default if value is empty.
  /// </returns>
  function  LstReadString     ( const _list      : TGIS_StringList ;
                                const _name      : String ;
                                const _default   : String
                              ) : String ;

  /// <summary>
  ///   Read parameter from list formatted as name=value.
  /// </summary>
  /// <param name="_list">
  ///   list to be read from
  /// </param>
  /// <param name="_name">
  ///   parameter name
  /// </param>
  /// <param name="_default">
  ///   default value to be used if name does not exists or is empty
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_BADPARAM
  /// </exception>
  /// <returns>
  ///   Parameter value as Integer.
  /// </returns>
  function  LstReadInteger    ( const _list      : TGIS_StringList ;
                                const _name      : String ;
                                const _default   : Integer
                              ) : Integer ;

  /// <summary>
  ///   Read parameter from list formatted as name=value.
  /// </summary>
  /// <param name="_list">
  ///   list to be read from
  /// </param>
  /// <param name="_name">
  ///   parameter name
  /// </param>
  /// <param name="_default">
  ///   default value to be used if name does not exists or is empty
  /// </param>
  /// <exception cref="EGIS_Exception">
  ///   GIS_RS_ERR_BADPARAM
  /// </exception>
  /// <returns>
  ///   Parameter value or default if value is empty.
  /// </returns>
  function  LstReadFloat      ( const _list      : TGIS_StringList ;
                                const _name      : String ;
                                const _default   : Double
                              ) : Double ;

  /// <summary>
  ///   Read parameter from list formatted as name=value.
  /// </summary>
  /// <param name="_list">
  ///   list to be read from
  /// </param>
  /// <param name="_name">
  ///   parameter name
  /// </param>
  /// <param name="_default">
  ///   default value to be used if name does not exists or is empty
  /// </param>
  /// <returns>
  ///   Parameter value or default if value is or unrecognized.
  /// </returns>
  function  LstReadBoolean   ( const _list      : TGIS_StringList ;
                               const _name      : String ;
                               const _default   : Boolean
                              ) : Boolean ;

  /// <summary>
  ///   Formats current GetLastError value as a String.
  /// </summary>
  /// <returns>
  ///   System error message.
  /// </returns>
  function  SystemErrorMessage : String ;

  /// <summary>
  ///   Returns simplified type of provided variant variable.
  /// </summary>
  /// <param name="_var">
  ///   variant value to be tested
  /// </param>
  /// <remarks>
  ///   Use this function to simplify testing and make it easier  for
  ///   cross-platforms porting. Returned value will group similar  variant
  ///   types. For Example all normal integers like varInteger,  varSmallInt,
  ///   varShortInt will be grouped as TGIS_VariantType.Int.
  /// </remarks>
  /// <returns>
  ///   Simplified type of provided variant variable.
  /// </returns>
  function  GetVariantType     ( const _var : Variant
                                 ) : TGIS_VariantType ;

  /// <summary>
  ///   Thread storage access function.
  /// </summary>
  /// <remarks>
  ///   Use ThreadStorage.GisAliasList to obtain thread related values.
  /// </remarks>
  /// <returns>
  ///   Thread storage access function.
  /// </returns>
  function ThreadStorage        : TGIS_ThreadStorage ;


  /// <summary>
  ///   Get default user agent String (from web downloads)
  /// </summary>
  /// <param name="_agnt">
  ///   extension to default user agent String if empty then 'ttk' String will
  ///   be used; _agnt String should not contain spaces
  /// </param>
  /// <remarks>
  ///   <note type="note">
  ///    Default String is similar to (filled with actual information)
  ///    'Mozilla/4.0+(Winos_NT_6.0;+WOW64;+ttk)'
  ///    </note>
  /// </remarks>
  /// <returns>
  ///   Default user agent String.
  /// </returns>
  function GetDefaultUserAgent ( const _agnt   : String
                               ) : String ;

type
  /// <summary>
  ///   Simple ASCII charset  for platform independent InCharSet functionality.
  /// </summary>
  TCharSet = array[ 0..127 ] of Boolean ;

  /// <summary>
  ///   Prepare charset array based on provided characters
  /// </summary>
  /// <param name="_def">
  ///   array of characters to be defined
  /// </param>
  /// <returns>
  ///   Prepared charset.
  /// </returns>
  function PrepareCharSet( const _def : array of String ) : TCharSet ;

  /// <summary>
  ///   Test if char is in charset.
  /// </summary>
  /// <param name="_c">
  ///   char to be tested
  /// </param>
  /// <param name="_set">
  ///   set to be tested for
  /// </param>
  /// <returns>
  ///   True if char is in charset.
  /// </returns>
  function InCharSet     ( const _c : Char; const _set : TCharSet ) : Boolean ; overload;

  /// <summary>
  ///   Test if char is in charset.
  /// </summary>
  /// <param name="_c">
  ///   char to be tested
  /// </param>
  /// <param name="_def">
  ///   array of charters to be tested for
  /// </param>
  /// <returns>
  ///   True if char is in charset.
  /// </returns>
  function InCharSet     ( const _c : Char; const _def : array of String ) : Boolean ; overload;

  /// <summary>
  ///   Test if pointer is assigned
  /// </summary>
  /// <param name="_val">
  ///   pointer to be tested
  /// </param>
  /// <returns>
  ///   True if pointer is assigned.
  /// </returns>
  {$IFDEF MANAGED}
    function AssignedPtr( const _val : IntPtr
                        ) : Boolean ;
  {$ELSE}
    function AssignedPtr( const _val : Pointer
                        ) : Boolean ;
                        {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  {$ENDIF}

   /// <summary>
   ///   Convert a literal value into an internal representation.
   /// </summary>
   /// <param name="_value">
   ///   value to translate; _value string is expected to be in C/C# format: so
   ///   '\\' will be converted to '\' and '\n' to CRLF (new line - codes
   ///   13+10) ; if value starts with '
   /// </param>
   /// <param name="_default">
   ///   default value if provided value is empty
   /// </param>
   /// <returns>
   ///   Internal representation of value.
   /// </returns>
   function ConvertParamString( const _value   : String ;
                                const _default : String
                              ) : String ;


  {$IFDEF JAVA}
    /// <summary>
    ///   Initialize array.
    /// </summary>
    /// <param name="_arr">
    ///   array to be initialized
    /// </param>
    procedure InitializeMatrixArray( var _arr : TGIS_Matrix3x3 ) ;

    /// <summary>
    ///   Initialize array.
    /// </summary>
    /// <param name="_arr">
    ///   array to be initialized
    /// </param>
    /// <param name="_width">
    ///   width of array
    /// </param>
    procedure InitializeDoubleArray(
        var _arr    : array[0..] of array[0..] of Double ;
      const _width  : Integer
     ) ;


  {$ENDIF}


  /// <summary>
  ///   Initialize array.
  /// </summary>
  /// <param name="_height">
  ///   width of grid
  /// </param>
  /// <param name="_width">
  ///   width of grid
  /// </param>
  /// <returns>
  ///   Initialized grid.
  /// </returns>
  function InitializeGrid( const _height : Integer ;
                           const _width  : Integer
                         ) : TGIS_GridArray ;

  {$IFDEF OXYGENE}
    /// <summary>
    ///   Swap 2 bytes for Little Endian / Big Endian values
    /// </summary>
    /// <param name="_arr">
    ///   array of bytes
    /// </param>
    /// <param name="_off">
    ///   offset to the first Byte
    /// </param>
    /// <returns>
    ///   Swapped value.
    /// </returns>
    function SwapWord       ( const _arr : TBytes ;
                              const _off : Integer
                            ) : Word ;

    /// <summary>
    ///   Swap 2 bytes for Little Endian / Big Endian values
    /// </summary>
    /// <param name="_arr">
    ///   array of bytes
    /// </param>
    /// <param name="_off">
    ///   offset to the first Byte
    /// </param>
    /// <returns>
    ///   Swapped value.
    /// </returns>
    function SwapSmallInt   ( const _arr : TBytes ;
                              const _off : Integer
                            ) : SmallInt ; overload;

    /// <summary>
    ///   Swap 4 bytes for Little Endian / Big Endian values
    /// </summary>
    /// <param name="_arr">
    ///   array of bytes
    /// </param>
    /// <param name="_off">
    ///   offset to the first Byte
    /// </param>
    /// <returns>
    ///   Swapped value.
    /// </returns>
    function SwapCardinal   ( const _arr : TBytes ;
                              const _off : Integer
                            ) : Cardinal ;

    /// <summary>
    ///   Swap 4 bytes for Little Endian / Big Endian values
    /// </summary>
    /// <param name="_arr">
    ///   array of bytes
    /// </param>
    /// <param name="_off">
    ///   offset to the first Byte
    /// </param>
    /// <returns>
    ///   Swapped value.
    /// </returns>
    function SwapSingle     ( const _arr : TBytes ;
                              const _off : Integer
                            ) : Single ; overload;
  {$ENDIF}

  /// <summary>
  ///   Swap long value for Little Endian / Big Endian values
  /// </summary>
  /// <param name="_val">
  ///   value to be swapped
  /// </param>
  /// <returns>
  ///   Swapped value.
  /// </returns>
  function SwapLongInt      ( const _val : DWORD ) : DWORD ;

  /// <summary>
  ///   Swap short value for Little Endian / Big Endian values
  /// </summary>
  /// <param name="_val">
  ///   value to be swapped
  /// </param>
  /// <returns>
  ///   Swapped value.
  /// </returns>
  function SwapShort2LongInt( const _val : DWORD ) : DWORD ;

  {$IFDEF OXYGENE}
    /// <summary>
    ///   Swap Single value for Little Endian / Big Endian values
    /// </summary>
    /// <param name="_val">
    ///   value to be swapped
    /// </param>
    /// <returns>
    ///   Swapped value.
    /// </returns>
    function SwapSingle     ( const _val : Single
                            ) : Single   ; overload;

    /// <summary>
    ///   Swap SmallInt value for Little Endian / Big Endian values
    /// </summary>
    /// <param name="_val">
    ///   value to be swapped
    /// </param>
    /// <returns>
    ///   Swapped value.
    /// </returns>
    function SwapSmallint   ( const _val : SmallInt
                            ) : SmallInt ; overload;
  {$ENDIF}

  //----------------------------------------------------------------------------
  // conversion functions
  //----------------------------------------------------------------------------

  /// <summary>
  ///   Interpolates a value between two points.
  /// </summary>
  /// <param name="_x1">
  ///   an abscissa of the first point
  /// </param>
  /// <param name="_x2">
  ///   an abscissa of the second point
  /// </param>
  /// <param name="_y1">
  ///   an ordinate of the first point
  /// </param>
  /// <param name="_y2">
  ///   an ordinate of the second point
  /// </param>
  /// <param name="_x">
  ///   an abscissa of the interpolated point
  /// </param>
  /// <param name="_base">
  ///   controls the rate at which interpolated values change
  /// </param>
  /// <returns>
  ///   an ordinate of the interpolated point
  /// </returns>
  /// <remarks>
  ///   Base parameter of 1 produces a linear interpolation. This is the default.
  ///   To produce exponential interpolation use values greater than 1.
  ///   Starting from 1, the higher the base is, the higher the rate of change.
  ///   To produce logarithmic interpolation use values lower than 1.
  ///   Starting from 1, the lower the base is, the lower rate of change.
  ///   The minimum possible value is 0.
  /// </remarks>
  function InterpolateValue ( const _x1   : Double ;
                              const _x2   : Double ;
                              const _y1   : Double ;
                              const _y2   : Double ;
                              const _x    : Double ;
                              const _base : Double = 1
                            ) : Double ; inline ;

  /// <summary>
  ///   Interpolates a color between two colors.
  /// </summary>
  /// <param name="_startColor">
  ///   start color (color for _min value)
  /// </param>
  /// <param name="_endColor">
  ///   end color (color for _max value)
  /// </param>
  /// <param name="_minValue">
  ///   minimal value
  /// </param>
  /// <param name="_maxValue">
  ///   maximal value
  /// </param>
  /// <param name="_value">
  ///   value for which gradient color will be computed
  /// </param>
  /// <param name="_colorSpace">
  ///   use 0..1 (exclude 0) for a logaritmic interpolation, 1 for linear,
  ///   greater than 1 for exponential; default is 1
  /// </param>
  /// <param name="_base">
  ///   controls the rate at which interpolated values change
  /// </param>
  /// <returns>
  ///   Computed color.
  /// </returns>
  /// <remarks>
  ///   Base parameter of 1 produces a linear interpolation. This is the default.
  ///   To produce exponential interpolation use values greater than 1.
  ///   Starting from 1, the higher the base is, the higher the rate of change.
  ///   To produce logarithmic interpolation use values lower than 1.
  ///   Starting from 1, the lower the base is, the lower rate of change.
  ///   The minimum possible value is 0.
  /// </remarks>
  function GradientColor( const _startColor : TGIS_Color ;
                          const _endColor   : TGIS_Color ;
                          const _minValue   : Double ;
                          const _maxValue   : Double ;
                          const _value      : Double ;
                          const _colorSpace : TGIS_ColorInterpolationMode
                                            = TGIS_ColorInterpolationMode.HSL ;
                          const _base       : Double = 1
                        ) : TGIS_Color ;

  /// <summary>
  ///   Creates a color from a color ramp.
  /// </summary>
  /// <param name="_colorRamp">
  ///   color ramp
  /// </param>
  /// <param name="_min">
  ///   minimal range value
  /// </param>
  /// <param name="_max">
  ///   maximal range value
  /// </param>
  /// <param name="_value">
  ///   value for which color will be computed
  /// </param>
  /// <param name="_colorRampZoneIndex">
  ///   color ramp zone index; can be stored for next function call to improve performance
  /// </param>
  /// <returns>
  ///   Computed color.
  /// </returns>
  function ColorFromColorRamp( const _colorRamp          : TGIS_ColorMapArray ;
                               const _min                : Double ;
                               const _max                : Double ;
                               const _value              : Double ;
                               var   _colorRampZoneIndex : Integer
                             ) : TGIS_Color ;

  {$IFDEF DCC}
    type
      /// <summary>
      ///   Platform independent string replace mode.
      /// </summary>
      TReplaceFlag = (

        /// <summary>
        ///   Replace all occurrences.
        /// </summary>
        rfReplaceAll,

        /// <summary>
        ///   Ignore case upon replacing.
        /// </summary>
        rfIgnoreCase
      ) ;
  {$ENDIF}

  /// <summary>
  ///   Platform independent replacement flag builder.
  /// </summary>
  /// <param name="_def">
  ///   array of flags to be used
  /// </param>
  /// <returns>
  ///   Replacement flags.
  /// </returns>
  function DefReplaceFlags(
    _def : array of TReplaceFlag
  ) : TReplaceFlags ;

  /// <summary>
  ///   Decode a datetime value to a macro for TGIS_SqlQuery parser.
  /// </summary>
  /// <param name="_dt">
  ///   datetime value
  /// </param>
  /// <returns>
  ///   text macro.
  /// </returns>
  function DecodeDateTimeToSqlMacro( const _dt : Variant
                                    ) : String ;

  /// <summary>
  ///   Search for a class in the library and invoke its method by name.
  /// </summary>
  /// <param name="_class_prefix">
  ///   prefix for searching a class name
  /// </param>
  /// <param name="_class_method">
  ///   a class method name to invoke
  /// </param>
  /// <returns>
  ///   True if any method was invoked
  /// </returns>
  function SelfInvokeClassMethod   ( const _class_prefix : String ;
                                     const _class_method : String
                                   ) : Boolean ;

  /// <summary>
  ///   Detect a value type based on text representation.
  /// </summary>
  /// <param name="_value">
  ///   text value
  /// </param>
  /// <returns>
  ///   a field type
  /// </returns>
  function DetectValueType     ( const _value : String
                                ) : TGIS_FieldType ;

  /// <summary>
  ///   Delete a directory and its contents.
  /// </summary>
  /// <param name="_path">
  ///   directory path
  /// </param>
  procedure DeleteDirectory( const _path : String ) ;

var
  /// <summary>
  ///  Forced location of RTree files. Used only if not empty..
  /// </summary>
  ForcedRTreePath : String ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.IOUtils,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.Rtti,
    System.TypInfo,
    System.SyncObjs,
    System.Character,
    System.RegularExpressions,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoConfig,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoStringFormat;
{$ENDIF}

const

  /// <summary>
  ///  JET provider format.
  /// </summary>
  FILE_JET_PROVIDER =
    'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s' ;

  /// <summary>
  ///  JET provider format for creation.
  /// </summary>
  FILE_JET_PROVIDER_CREATE =
    'Provider=Microsoft.Jet.OLEDB.4.0;Jet OLEDB:Engine Type=5;Data Source=%s' ;

type

  // list of all allocated thread storage
  T_ThreadStorageList = class( TGIS_ThreadClass )
    private
      oList : TObjectList<TGIS_ThreadStorage> ;
    protected
      procedure doDestroy ; override;
    public
      constructor  Create ;
      function &New   : TGIS_ThreadStorage ;
  end ;

var
  { Critical section for "current directory" management.ss }
    othreadDir : TGIS_ThreadClass ;
  { List of all thread related object. }
    FThreadStorageList  : T_ThreadStorageList ;

var
  FThreadStorage : TGIS_ThreadStorage ;

  function threadDir : TGIS_ThreadClass ;
  begin
    if not assigned( othreadDir ) then
      othreadDir := TGIS_ThreadClass.Create ;
    Result := othreadDir ;
  end ;

//=============================================================================
// T_ThreadStorageList
//=============================================================================

  constructor T_ThreadStorageList.Create  ;
  begin
    inherited ;

    oList := TObjectList<TGIS_ThreadStorage>.Create ;
  end ;

  procedure T_ThreadStorageList.doDestroy ;
  begin
    FreeObject( oList ) ;
    inherited ;
  end ;

  // Create a new thread storage object and delete previously
  // allocated object which belongs to previous thread with same thread
  // signature.
  // return newly created storage
  function T_ThreadStorageList.&New : TGIS_ThreadStorage ;
  var
    i   : Integer            ;
    {$IFNDEF OXYGENE}
    obj : TGIS_ThreadStorage ;
    {$ENDIF}
  begin
    LockThread ;
    try
      // delete exiting object form already terminated thread
      // with same signature
      for i := 0 to oList.Count - 1 do begin
        {$IFDEF OXYGENE}
          assert( False, 'Must be tested' ) ;
        {$ELSE}
          obj := TGIS_ThreadStorage( oList[ i ] ) ;

          if obj.ThreadId = TThread.CurrentThread.ThreadID then begin
            // such object already exists
            oList.Remove( obj ) ;
            break ;
          end;
        {$ENDIF}
      end ;

      // and add newly created object
      Result := TGIS_ThreadStorage.Create ;
      oList.Add( Result ) ;
    finally
      UnlockThread ;
    end;
  end ;


  function DefReplaceFlags(
    _def : array of TReplaceFlag
  ) : TReplaceFlags ;
  var
    i : Integer ;
  begin
    Result := [] ;

    for i:= low(_def) to high(_def) do begin
      case _def[i] of
        {$IFDEF DCC}
          TReplaceFlag.rfReplaceAll : Result := [ System.SysUtils.rfReplaceAll ] ;
          TReplaceFlag.rfIgnoreCase : Result := [ System.SysUtils.rfIgnoreCase ] ;
        {$ELSE}
          TReplaceFlag.rfReplaceAll : Result := [ TReplaceFlag.rfReplaceAll ] ;
          TReplaceFlag.rfIgnoreCase : Result := [ TReplaceFlag.rfIgnoreCase ] ;
        {$ENDIF}
      end;
    end ;
  end;

  function GisSingleVector( const _x, _y, _z : Single ) : TGIS_SingleVector ;
  begin
    Result.X := _x ;
    Result.Y := _y ;
    Result.Z := _z ;
  end ;

  function GisSingleColor( const _r, _g, _b, _a : Single ) : TGIS_SingleColor ;
  begin
    Result.r := _r ;
    Result.g := _g ;
    Result.b := _b ;
    Result.a := _a ;
  end ;

  function GisPartDescriptor ( const _partType      : Integer ;
                               const _levelOfDetail : Integer ;
                               const _priority      : Integer ;
                               const _material      : Integer
                              ) : TGIS_PartDescriptor ;
   begin
    Result.PartType      := _partType ;
    Result.LevelOfDetail := _levelOfDetail ;
    Result.Priority      := _priority ;
    Result.Material      := _material ;
   end ;

  function GisGetEmptyLabelPosition
    : TGIS_LabelPositions ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [] ;
      {$ELSE}
        Result := 0 ;
      {$ENDIF}
    {$ELSE}
      Result := [] ;
    {$ENDIF}
  end;

  function GisGetLabelPosition(
    _value : TGIS_LabelPosition
  ) : TGIS_LabelPositions ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [ _value ] ;
      {$ELSE}
        Result := _value ;
      {$ENDIF}
    {$ELSE}
      // 64-bit workaround
      Result := [] + [ _value ] ;
    {$ENDIF}
  end;

  function GisAddLabelPosition(
    _set   : TGIS_LabelPositions ;
    _value : TGIS_LabelPosition
  ) : TGIS_LabelPositions ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set + [_value] ;
      {$ELSE}
        Result := _set or _value ;
      {$ENDIF}
    {$ELSE}
      Result := _set + [_value] ;
    {$ENDIF}
  end;

  function GisTestLabelPosition(
    _value : TGIS_LabelPosition ;
    _set   : TGIS_LabelPositions
  ) : Boolean ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _value in _set ;
      {$ELSE}
        Result := ( _value and _set ) <> 0 ;
      {$ENDIF}
    {$ELSE}
      Result := _value in _set ;
    {$ENDIF}
  end;

  function GisGetEmptyFileType
    : TGIS_FileTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [] ;
      {$ELSE}
        Result := 0 ;
      {$ENDIF}
    {$ELSE}
      Result := [] ;
    {$ENDIF}
  end ;

  function GisGetFileType(
    _value : TGIS_FileType
  ) : TGIS_FileTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [ _value ] ;
      {$ELSE}
        Result := _value ;
      {$ENDIF}
    {$ELSE}
      // 64-bit workaround
      Result := [] + [ _value ] ;
    {$ENDIF}
  end ;

  function GisAddFileType(
    _set   : TGIS_FileTypes ;
    _value : TGIS_FileType
  ) : TGIS_FileTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set + [_value] ;
      {$ELSE}
        Result := _set or _value ;
      {$ENDIF}
    {$ELSE}
      Result := _set + [_value] ;
    {$ENDIF}
  end ;

  function GisTestFileType(
    _value : TGIS_FileType ;
    _set   : TGIS_FileTypes
  ) : Boolean ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _value in _set ;
      {$ELSE}
        Result := ( _value and _set ) <> 0 ;
      {$ENDIF}
    {$ELSE}
      Result := _value in _set ;
    {$ENDIF}
  end;

  function GisGetEmptyShapeType
    : TGIS_ShapeTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [] ;
      {$ELSE}
        Result := 0 ;
      {$ENDIF}
    {$ELSE}
      Result := [] ;
    {$ENDIF}
  end ;

  function GisGetShapeType(
    _value : TGIS_ShapeType
  ) : TGIS_ShapeTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [ _value ] ;
      {$ELSE}
        Result := _value ;
      {$ENDIF}
    {$ELSE}
      // 64-bit workaround
      Result := [] + [ _value ] ;
    {$ENDIF}
  end ;

  function GisAddShapeType(
    _set   : TGIS_ShapeTypes ;
    _value : TGIS_ShapeType
  ) : TGIS_ShapeTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set + [_value] ;
      {$ELSE}
        Result := _set or _value ;
      {$ENDIF}
    {$ELSE}
      Result := _set + [_value] ;
    {$ENDIF}
  end ;

  function GisTestShapeType(
    _value : TGIS_ShapeType ;
    _set   : TGIS_ShapeTypes
  ) : Boolean ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _value in _set ;
      {$ELSE}
        Result := ( _value and _set ) <> 0 ;
      {$ENDIF}
    {$ELSE}
      Result := _value in _set ;
    {$ENDIF}
  end;

  function GisGetEmptyFontStyle
    : TGIS_FontStyles ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [] ;
      {$ELSE}
        Result := 0 ;
      {$ENDIF}
    {$ELSE}
      Result := [] ;
    {$ENDIF}
  end ;

  function GisGetFontStyle(
    _value : TGIS_FontStyle
  ) : TGIS_FontStyles ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [ _value ] ;
      {$ELSE}
        Result := _value ;
      {$ENDIF}
    {$ELSE}
      // 64-bit workaround
      Result := [] + [ _value ] ;
    {$ENDIF}
  end ;

  function GisAddFontStyle(
    _set   : TGIS_FontStyles ;
    _value : TGIS_FontStyle
  ) : TGIS_FontStyles ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set + [_value] ;
      {$ELSE}
        Result := _set or _value ;
      {$ENDIF}
    {$ELSE}
      Result := _set + [_value] ;
    {$ENDIF}
  end ;

  function GisRemoveFontStyle(
    _set   : TGIS_FontStyles ;
    _value : TGIS_FontStyle
  ) : TGIS_FontStyles ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set - [_value] ;
      {$ELSE}
        Result := _set and ( not _value ) ;
      {$ENDIF}
    {$ELSE}
      Result := _set - [_value] ;
    {$ENDIF}
  end ;

  function GisTestFontStyle(
    _value : TGIS_FontStyle ;
    _set   : TGIS_FontStyles
  ) : Boolean ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _value in _set ;
      {$ELSE}
        Result := ( _value and _set ) <> 0 ;
      {$ENDIF}
    {$ELSE}
      Result := _value in _set ;
    {$ENDIF}
  end;

  function GisGetEmptyDimensionType
    : TGIS_DimensionTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [] ;
      {$ELSE}
        Result := 0 ;
      {$ENDIF}
    {$ELSE}
      Result := [] ;
    {$ENDIF}
  end ;

  function GisGetDimensionType(
    _value : TGIS_DimensionType
  ) : TGIS_DimensionTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [ _value ] ;
      {$ELSE}
        Result := _value ;
      {$ENDIF}
    {$ELSE}
      // 64-bit workaround
      Result := [] + [ _value ] ;
    {$ENDIF}
  end ;

  function GisGetOperationType(
    _value    : TGIS_OperationType
  ) : TGIS_OperationTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := [ _value ] ;
      {$ELSE}
        Result := _value ;
      {$ENDIF}
    {$ELSE}
      // 64-bit workaround
      Result := [] + [ _value ] ;
    {$ENDIF}
  end ;

  function GisAddDimensionType(
    _set   : TGIS_DimensionTypes ;
    _value : TGIS_DimensionType
  ) : TGIS_DimensionTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set + [_value] ;
      {$ELSE}
        Result := _set or _value ;
      {$ENDIF}
    {$ELSE}
      Result := _set + [_value] ;
    {$ENDIF}
  end ;

  function GisAddOperationType(
    _set      : TGIS_OperationTypes ;
    _value    : TGIS_OperationType
  ) : TGIS_OperationTypes ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _set + [_value] ;
      {$ELSE}
        Result := _set or _value ;
      {$ENDIF}
    {$ELSE}
      Result := _set + [_value] ;
    {$ENDIF}
  end ;

  function GisTestDimensionType(
    _value : TGIS_DimensionType ;
    _set   : TGIS_DimensionTypes
  ) : Boolean ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := _value in _set ;
      {$ELSE}
        Result := ( _value and _set ) <> 0 ;
      {$ENDIF}
    {$ELSE}
      Result := _value in _set ;
    {$ENDIF}
  end;

  function GisLockLessThan(
    const _lock  : TGIS_Lock ;
    const _value : TGIS_Lock
  ) : Boolean ;
  begin
    Result := _lock < _value ;
  end ;

  function GisLockGreaterThan(
    const _lock  : TGIS_Lock ;
    const _value : TGIS_Lock
  ) : Boolean ;
  begin
    Result := _lock > _value ;
  end ;

  function GisLockGreaterThanEqual(
    const _lock  : TGIS_Lock ;
    const _value : TGIS_Lock
  ) : Boolean ;
  begin
    Result := _lock >= _value ;
  end ;

  function GisLockLessThanEqual(
    const _lock  : TGIS_Lock ;
    const _value : TGIS_Lock
  ) : Boolean ;
  begin
    Result := _lock <= _value ;
  end ;

  function GisFileAge( const _path : String ) : TDateTime ;
  var
    spath : String  ;
  begin
    spath := GisAliasList.Resolve( _path ) ;
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        var dt := new java.util.Date( New java.io.File(spath).lastModified() ) ;
        Result := New DateTime( dt ) + TimeZone.Local.getOffsetToUTC() ;
      {$ENDIF}
      {$IFDEF CLR}
        Result := ( New FileInfo( spath ) ).LastWriteTime ;
      {$ENDIF}
    {$ELSE}
      Result := FileDateToDateTime( FileAge( spath ) ) ;
    {$ENDIF}
  end;


  procedure GisCopyMemory( const _srcBuf    : TBytes ;
                           const _srcOffset : Integer ;
                           const _dstBuf    : TBytes ;
                           const _dstOffset : Integer ;
                           const _count     : Integer
                          ) ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        System.Buffer.BlockCopy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        System.arraycopy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
      {$IFDEF ISLAND}
        &Array.Copy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
    {$ELSE}
      {$IFDEF MSWINDOWS}
        CopyMemory( @_dstBuf[_dstOffset], @_srcBuf[_srcOffset], _count ) ;
      {$ELSE}
        {$MESSAGE WARN '### Verify no Windows code ##'}
        Move( _srcBuf[_srcOffset], _dstBuf[_dstOffset], _count ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure GisCopyMemoryEx( const _srcBuf    : TGIS_SingleArray ;
                       const _srcOffset : Integer ;
                       const _dstBuf    : TGIS_SingleArray ;
                       const _dstOffset : Integer ;
                       const _count     : Integer
                     ) ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        System.Array.Copy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        System.arraycopy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
      {$IFDEF ISLAND}
        &Array.Copy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
    {$ELSE}
      {$IFDEF MSWINDOWS}
        CopyMemory( @_dstBuf[_dstOffset], @_srcBuf[_srcOffset], sizeOf(Single)*_count ) ;
      {$ELSE}
        {$MESSAGE WARN '### Verify no Windows code ## '}
        Move( _srcBuf[_srcOffset], _dstBuf[_dstOffset], sizeOf(Single)*_count ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure GisCopyPixels(
    const _srcBuf    : TGIS_Pixels ;
    const _srcOffset : Integer ;
    const _dstBuf    : TGIS_Pixels ;
    const _dstOffset : Integer ;
    const _count     : Integer
  ) ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF CLR}
        System.Array.Copy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        System.arraycopy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
      {$IFDEF ISLAND}
        &Array.Copy( _srcBuf, _srcOffset, _dstBuf, _dstOffset, _count ) ;
      {$ENDIF}
    {$ELSE}
      Move( _srcBuf[_srcOffset], _dstBuf[_dstOffset], sizeOf(Integer)*_count ) ;
    {$ENDIF}
  end ;

//==============================================================================
// general util functions & procedures
//==============================================================================

  {$IFDEF OXYGENE}
    function IsSameType(
      const _obj1 : TObject ;
      const _obj2 : TObject
    ) : Boolean ;
    begin
      {$IFDEF JAVA}
        if (_obj1 = nil) or (_obj2 = nil) then begin
          Result := False ;
        end
        else begin
          Result := _obj1.Class = _obj2.Class ;
        end ;
      {$ELSE}
        Result := typeOf( _obj1 ) = typeOf( _obj2 ) ;
      {$ENDIF}
    end;
  {$ELSE}
    function IsSameType(
      const _obj1 : TObject ;
      const _obj2 : TObject
    ) : Boolean ;
    begin
      Result := _obj1.ClassName = _obj2.ClassName ;
    end ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    {$IFDEF CLR}
      function IsSameType(
        const _obj  : System.Object ;
        const _type : System.Type
      ) : Boolean ;
      begin
        Result := typeOf( _obj ) = _type ;
      end;
    {$ENDIF}
    {$IFDEF JAVA}
      function IsSameType(
        const _obj  : TObject ;
        const _type : &Class
      ) : Boolean ;
      begin
        {$IFDEF JAVA}
          if (_obj = nil) then
            Result := False
          else
            Result := _obj.Class = _type ;
        {$ELSE}
          Result := typeOf( _obj ) = _type ;
        {$ENDIF}
      end;
    {$ENDIF}
  {$ELSE}
    function IsSameType(
      const _obj  : TObject ;
      const _type : TClass
    ) : Boolean ;
    begin
      Result := _obj.ClassType = _type ;
    end ;

    function IsSameType(
      const _obj1 : TObject ;
      const _obj2 : IInterface
    ) : Boolean ;
    begin
      Result := _obj2 is _obj1.ClassType ;
    end ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    function GetClassName(
      const _obj : TObject
    ) : String ;
    begin
      {$IFDEF JAVA}
        Result := _obj.Class.toString ;
      {$ELSE}
        Result := _obj.GetType.Name ;
      {$ENDIF}
    end ;
  {$ELSE}
    function GetClassName(
      const _obj : TObject
    ) : String ;
    begin
      Result := _obj.ClassName ;
    end ;
  {$ENDIF}

  function GetShortPath( const _path : String  ) : String ;
  var
    {$IFDEF MSWINDOWS}
      {$IFDEF CLR}
        buf : StringBuilder ;
      {$ELSE}
        buf : array [0..8192] of Char;
      {$ENDIF}
      i     : Integer ;
    {$ENDIF}
    spath : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    {$IFDEF MSWINDOWS}

      if SafeFileExists( spath ) then begin
        {$IFDEF CLR}
          buf := System.Text.StringBuilder.Create( 8192+1 ) ;
          i := GetShortPathName( spath, buf, buf.Capacity-1 ) ;
        {$ELSE}
          i := GetShortPathName( PChar( spath ), @buf, high( buf ) ) ;
        {$ENDIF}

        {$IFDEF CLR}
          if ( i >0 ) and ( i < buf.Capacity-1 ) then
            Result := buf.ToString
        {$ELSE}
          if ( i >0 ) and ( i < high(buf) ) then
            Result := buf
        {$ENDIF}
        else
          Result := spath ;
      end
      else begin
        Result := spath ;
      end ;

    {$ELSE}
       Result := spath ;
    {$ENDIF}
  end ;

  function GetPathDirSep( const _path : String  ) : String ;
  begin
    Result := _path ;

    if IsStringEmpty( _path ) or
      ( _path[ StringLast( _path ) ] <> GisEnvironmentInfo.DirSep )
    then
      Result := Result + GisEnvironmentInfo.DirSep ;
   end ;

  function GetPathNoExt( const _path : String  ) : String ;
  var
    ext   : String ;
    spath : String ;
  begin
    spath := Trim( GisAliasList.Resolve( _path ) ) ;
    ext   := GetFileExt( spath ) ;

    if IsStringEmpty( ext ) then begin
      Result := spath ;
      exit ;
    end ;

    Result := Copy( spath, StringFirst, length( spath ) - length( ext ) ) ;

  end ;

  function GetPathRelative( const _dir : String; const _path : String
                          ) : String ;
  var
    dir   : String  ;
    path  : String  ;
    i     : Integer ;
    last  : Integer ;
    c     : Char    ;
    srv   : Integer ;
    spath : String  ;

    // Extract server name form path like //server/xxxx
    // _path path to be extracted
    // return extracted server name or empty String
    function server_name( const _path : String ) : String ;
    var
      i1 : Integer ;
      c1 : Char    ;
      b  : Integer ;
      e  : Integer ;
      l  : Integer ;
    begin
      Result := '' ;

      b := 0 ;
      e := 0 ;
      l := 0 ;

      for i1 := StringFirst to StringLast( _path ) do begin
        c1 := _path[i1] ;
        if ( c1 = '\' ) or ( c1 = '/' ) then begin
          inc( l ) ;
          if      i1 = StringFirst     then b := StringFirst + 1
          else if i1 = StringFirst + 1 then begin
                                              if b = StringFirst + 1 then
                                                b := StringFirst + 2 ;
                                            end
          else if i1 > StringFirst + 1 then begin
                                              if l = 4 then begin // include "\\server\dir"
                                                e := i1 - b ;
                                                break ;
                                              end ;
                                            end
          else                              begin
                                              b := 0 ;
                                              e := 0 ;
                                            end ;
        end;
      end;

      if b = StringFirst + 2 then begin
        if e = 0 then e := 8192 ;

        Result := Copy( _path, b, e ) ;
      end;
    end;

  begin
    spath  := GisAliasList.Resolve( _path ) ;

    if ( Pos ( String( '://' ), spath ) >= StringFirst ) or
       ( Pos ( String( #13   ), spath ) >= StringFirst ) or
       IsEmbeddedSQLPath( spath )                        then
    begin
      Result := spath ;
      exit ;
    end ;

    dir  := GetPathAbsolute( '', _dir  ) ;

    if not IsStringEmpty( dir ) then begin
      c := dir[ StringLast( dir ) ] ;
      if GisEnvironmentInfo.IsWindows then begin
        if ( c <> '\' ) and ( c <> '/' ) then
          dir := dir + '\' ;
      end
      else begin
        if c <> '/' then
          dir := dir + '/' ;
      end ;
    end;

    path := GetPathAbsolute( '', spath ) ;
    last := StringFirst - 1 ;

    // find first not matching character
    srv := 0 ;
    for i := StringFirst to Min( StringLast( dir ), StringLast( path ) ) do begin
      c := dir[i] ;
      if GisEnvironmentInfo.IsWindows then begin
        if UpCase( c ) <> UpCase( path[i] ) then break ;
        if ( c = '\' ) or ( c = '/' ) then begin
          last := i ;
          if      i = StringFirst      then srv := 1
          else if i = StringFirst + 1  then begin
                                              if srv = 1 then
                                                srv := 2 ;
                                            end;
        end ;
      end
      else begin
        if c <> path[i] then break ;
        if c = '/' then
          last := i ;
      end ;
    end ;

    if srv = 2 then begin
      if CompareText( server_name( dir ), server_name( path ) )<> 0 then
      begin
        // server are not same
        last := StringFirst - 1 ;
      end;
    end;

    // prepare up path ..\
    Result := Copy( path, last+1, 8192 ) ;
    if last > StringFirst - 1 then begin
      for i := last+1 to StringLast( dir ) do begin
        c := dir[i] ;
        if GisEnvironmentInfo.IsWindows then begin
          if ( c = '\' ) or ( c = '/' ) then
            Result := '..\' + Result ;
        end
        else begin
          if c = '/' then
            Result := '../' + Result ;
        end ;
      end ;
    end ;
  end ;

  function GetPathAbsolute( const _dir : String; const _path : String
                          ) : String ;
  var
    sdir   : String ;
    spath  : String ;
    qm_pos : Integer ;
    sextra : String ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;
    Result := spath ;
    if IsStringEmpty( spath ) then
      exit ;
    if ( Pos ( '://'      , spath ) >= StringFirst     ) or
       ( Pos ( String(#13), spath ) >= StringFirst     ) or
       ( Pos ( ':'        , spath ) >= StringFirst     ) or
       IsEmbeddedSQLPath( spath )                        then
    begin
      exit ;
    end ;

    qm_pos := Pos( String('?'), spath ) ;

    if qm_pos >= StringFirst then begin
      spath  := Copy( spath, StringFirst, qm_pos-StringFirst ) ;
      sextra := Copy( _path, qm_pos, 4096 ) ;
    end
    else begin
      sextra := ''
    end;

    if ( Pos ( '/'        , spath ) =  StringFirst     ) or
       ( Pos ( '\'        , spath ) =  StringFirst     ) then
    begin
      Result := ExpandFileNameEx( spath ) + sextra ;
      exit ;
    end ;

    sdir := _dir ;

    if sdir <> '' then
    begin
      case sdir[ StringLast( sdir ) ] of
        '/',
        '\': ;
        else sdir := sdir + '\' ;
      end ;
    end ;

    Result := ExpandFileNameEx( sdir + spath ) + sextra ;
  end;

  function GetSafeSQLPath( const _path : String ) : String ;
  var
    txt : String ;
    spath  : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    if IsEmbeddedSQLPath( spath ) then begin
      Result := 'SQL' ;
      GetSQLParamFromPath( spath, GIS_INI_LAYERSQL_STORAGE, txt  ) ;
      if not IsStringEmpty( txt ) then
        Result := Result + ':' + txt ;
      GetSQLParamFromPath( spath, GIS_INI_LAYERSQL_DIALECT, txt  ) ;
      if not IsStringEmpty( txt ) then
        Result := Result + ':' + txt ;
      GetSQLParamFromPath( spath, GIS_INI_LAYERSQL_LAYER  , txt  ) ;
      if not IsStringEmpty( txt ) then
        Result := Result + ':' + txt ;
    end
    else
      Result := spath ;
  end ;

  function IsEmbeddedSQLPath( const _path : String ) : Boolean ;
  var
    lst   : TGIS_StringList ;
    spath : String  ;
  begin
    Result := False ;
    if IsStringEmpty( _path ) then exit ;

    spath  := GisAliasList.Resolve( _path ) ;

    // do not use GetSQLParamFromPath to avoid circular references !!!!
    lst := TGIS_StringList.Create ;
    try
      lst.Text := ConvertParamString( spath, '' ) ;
      Result := not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_STORAGE ] ) ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  function IsServerPath( const _path : String ) : Boolean  ;
  var
    spath  : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    if ( Pos ( '://'      , spath ) >= StringFirst )     or
       ( Pos ( String(#13), spath ) >= StringFirst )     or
       ( Pos ( ':'        , spath ) >= StringFirst + 2 ) or
       IsEmbeddedSQLPath( spath )                        then
    begin
      Result := True ;
    end
    else begin
      Result := False ;
    end ;
  end ;

  function IsFileOrServerPath( const _path : String ) : Boolean  ;
  var
    spath  : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    if ( Pos ( '://'      , spath ) >= StringFirst )     or
       ( Pos ( String(#13), spath ) >= StringFirst )     or
       ( Pos ( ':'        , spath ) >= StringFirst + 2 ) or
       IsEmbeddedSQLPath( spath )                        then
    begin
      Result := True ;
    end
    else begin
      Result := SafeFileExists( spath ) ;
    end ;
  end ;

  function GetFileDir( const _path : String ) : String  ;
  var
    dir   : String ;
    spath : String ;
  begin
    Result := '' ;

    spath  := GisAliasList.Resolve( _path ) ;

    if ( not IsStringEmpty( spath ) ) and not IsEmbeddedSQLPath( spath ) and SafeFileExists( spath ) then
      dir := ExtractFilePath( GetPathAbsolute( '', spath ) )
    else
      dir := '' ;

    if CheckDir( dir ) then begin
      Result := dir ;
    end ;

  end ;

  function ChangeDir( const _path : String ) : String  ;
  var
    dir   : String ;
    spath : String ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;
    Result := '' ;

    dir := GetFileDir( spath ) ;
    if not IsStringEmpty( dir ) then begin
      threadDir.LockThread ;
      Result := GetCurrentDirEx ;
      SetCurrentDirEx( dir ) ;
    end ;
  end ;

  procedure RestoreDir( const _path : String ) ;
  begin
    if not IsStringEmpty( _path ) then begin
      SetCurrentDirEx( _path ) ;
      threadDir.UnlockThread ;
    end ;
  end ;

  function CheckDir( const _path : String ) : Boolean ;
  var
    spath : String  ;
  begin
    Result := False ;
    If IsStringEmpty( _path ) then exit ;

    spath := GisAliasList.Resolve( _path ) ;

    Result := ( not IsStringEmpty( spath ) ) and
              DirectoryExists( spath ) ;
  end ;

  function CheckFileWriteAccess(
    const _path : String
  ) : Boolean ;
  var
    f     : TGIS_FileStream ;
    spath : String          ;
  begin
    Result := True ;

    spath  := GisAliasList.Resolve( _path ) ;

    if IsStringEmpty( spath ) then exit ;
    try
      if SafeFileExists( spath ) then begin
        f := TGIS_FileStream.Create( _path,
                                     fmOpenReadWrite or
                                     fmShareDenyWrite
                                   ) ;
        FreeObject( f ) ;
      end
      else begin
        f := TGIS_FileStream.Create( _path,
                                     fmCreate
                                   ) ;
        FreeObject( f ) ;
        {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( spath ) ;
      end;
    except
      Result := False ;
    end ;
  end ;

  function CheckFileWriteAccessEx(
    const _path   : String  ;
    const _plain  : Boolean ;
    const _backup : Boolean ;
    const _temp   : Boolean
  ) : Boolean ;
  begin
    Result := True ;

    if _plain then
      Result := Result and CheckFileWriteAccess( _path ) ;
    if _backup then
      Result := Result and CheckFileWriteAccess( GetBackupName( _path ) ) ;
    if _temp then
      Result := Result and CheckFileWriteAccess( GetTemporaryName( _path ) ) ;
  end;

  function GetCurrentDirEx : String ;
  begin
    Result := GetCurrentDir ;
  end ;

  procedure SetCurrentDirEx( const _path : String ) ;
  var
    spath  : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    SetCurrentDir( spath ) ;
  end ;

  function ExpandFileNameEx( const _path : String ) : String ;
  var
    spath  : String  ;

  begin
    spath  := GisAliasList.Resolve( _path ) ;


    {$IFDEF DCC}
      spath := StringReplace( spath, '\', '/', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
    {$ELSE}
      {$IFDEF ANDROID}
        var dir := IncludeTrailingPathDelimiter( GetCurrentDirEx )  ;
        var fld := new java.io.File(dir, spath).getAbsoluteFile();
        spath := fld.AbsolutePath ;
      {$ELSE}
        {$IFDEF JAVA}
          spath := spath.Replace('\','/');
        {$ELSE}
          spath := StringReplace( spath, '\', '/', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}

    Result := ExpandFileName( spath ) ;
  end ;

  function GetTempFileName
    : String ;
  var
    sguid : String ;
    {$IFDEF DCC}
      oguid : TGuid  ;
    {$ENDIF}
  begin
    {$IFDEF DCC}
      CreateGUID(oguid);
      sguid := GuidToString( oguid ) ;
      sguid := StringReplace( sguid, '{', '', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
      sguid := StringReplace( sguid, '}', '', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
      {$IFDEF MSWINDOWS}
        Result := GetTempFolder + sguid ;
      {$ELSE}
        {$IFDEF NEXTGEN}
          Result := GetTempFolder + sguid ;
        {$ELSE}
          Result := sguid ;
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
    {$IFDEF CLR}
      sguid  := System.Guid.NewGuid().ToString('D') ;
      Result := GetTempFolder + sguid ;
    {$ENDIF}
    {$IFDEF JAVA}
      sguid  := java.util.UUID.randomUUID().toString() ;
      var tempdir : String := GetTempFolder ;
      if not ( tempdir.EndsWith('/') or tempdir.EndsWith('\\') ) then
         tempdir := tempdir + System.getProperty('file.separator') ;
      Result := tempdir + sguid;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end;

  function GetTempFolder
    : String ;
  begin
    {$IFDEF DCC}
      Result := TPath.GetTempPath ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := System.IO.Path.GetTempPath() ;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := System.getProperty('java.io.tmpdir') ;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end;

  function GetPathCommonFiles: String ;
  {$IFDEF MSWINDOWS}
    var
      reg : TRegistry ;
    begin
      Result := '' ;
      reg := TRegistry.Create ;
      try
        reg.RootKey := HKEY_LOCAL_MACHINE;
        if reg.OpenKey( 'SOFTWARE\Microsoft\Windows\CurrentVersion', False ) then begin
          Result := reg.ReadString( 'CommonFilesDir' ) + '\' + GIS_COMMONFILES_TATUKGIS ;
          reg.CloseKey ;
        end ;
      finally
        FreeObject( reg ) ;
      end ;
    end ;
  {$ELSE}
    begin
      {$MESSAGE WARN '### Verify no Windows code ## '}
      Result := '' ;
    end ;
  {$ENDIF}

  function GetFilePath( const _path : String ) : String ;
  var
    i     : Integer ;
    k     : Integer ;
    spath : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    k := 0 ;
    for i := StringFirst to StringLast( spath ) do begin
      if ( _path[i] = ':' ) or
         ( _path[i] = '\' ) or
         ( _path[i] = '/' ) then
      begin
        k := i+1-StringFirst ;
      end ;
    end;

    Result := Copy( spath, StringFirst, k ) ;
  end ;

  function GetFileName( const _path : String ) : String ;
  var
    i     : Integer ;
    k     : Integer ;
    l     : Integer ;
    spath : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    k := StringFirst-1 ;
    l := 0 ;
    for i := StringFirst to StringLast( spath ) do begin
      inc( l ) ;
      if ( _path[i] = ':' ) or
         ( _path[i] = '\' ) or
         ( _path[i] = '/' ) then
      begin
        k := i ;
        l := 1 ;
      end ;
    end;

    Result := Copy( spath, k+1, l )
  end ;

  function GetFileExt( const _path : String ) : String ;
  var
    i     : Integer ;
    k     : Integer ;
    l     : Integer ;
    spath : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    k := StringFirst-1 ;
    l := 0 ;
    for i := StringLast( spath ) downto StringFirst do begin
      inc( l ) ;
      case _path[i] of
        '.' : begin
                k := i ;
                break ;
              end ;
        '/',
        '\',
        ':' : begin
                k := StringFirst-1 ;
                break ;
              end ;
      end ;

    end;

    if k >= StringFirst then
      Result := Copy( spath, k, l )
    else
      Result := '' ;
  end ;

  function GetFileNameNoExt( const _path : String  ) : String ;
  var
    i     : Integer ;
    cnt   : Integer ;
    path  : TStringBuilder ;
    c     : Char    ;
    ext   : String  ;
    stmp  : String  ;
    spath : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;
    cnt := length( spath ) ;
    // replace '/' with '\'
    path := TStringBuilder.Create ;
    try

      for i := 0 to cnt - 1 do begin
        c := spath[StringFirst+i] ;
        if c = '/' then path.Append( '\' )
                   else path.Append( c   ) ;
      end ;

      stmp  := GetFileName( Trim( path.ToString ) ) ;
      ext   := GetFileExt ( stmp ) ;
    finally
      FreeObject( path ) ;
    end;

    if IsStringEmpty( ext ) then begin
      Result := stmp ;
      exit ;
    end ;

    Result := Copy( stmp, StringFirst, length( stmp ) - length( ext ) ) ;

  end ;

  function GetServerName( const _path : String  ) : String ;
  var
    i     : Integer ;
    k     : Integer ;
    l     : Integer ;
    spath : String  ;
    c     : Char    ;
    state : Integer ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    k     := StringFirst ;
    l     := 0 ;
    state := 1 ;
    for i := StringFirst to StringLast( spath ) do begin
      c := spath[ i ] ;

      case( state ) of
        1  : if c= '/' then state := 2 ;
        2  : if c= '/' then state := 3 ;
        3  : begin
               k := i ;
               state := 4 ;
             end ;
        4  : begin
               inc( l ) ;
               if      c = '?' then state := 5
               else if c = '/' then state := 5
               else if c = '&' then state := 5 ;
             end;
        5  : break ;
        else begin
               assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
             end;
      end;
    end ;

    Result := Copy( spath, k, l ) ;
  end ;

  function GetBackupName( const _path : String ) : String ;
  var
    ext : String ;
    spath  : String  ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    ext := GetFileExt( spath ) ;
    if Pos( String('.'), ext ) = StringFirst then
      Result := GetPathNoExt( spath ) + '.~' + Copy( ext, StringFirst+1, MaxInt )
    else
      Result := spath + '.~'  ;
  end ;

  function GetTemporaryName( const _path : String ) : String ;
  var
    spath : String  ;
  begin
    spath := GisAliasList.Resolve( _path ) ;

    Result := spath + '~tmp' ;
  end ;

  function SafeFileExists ( const _path : String ) : Boolean ;
  begin
    try
      Result := FileExists( _path ) ;
    except
      Result := False ;
    end ;
  end ;

  function SafeDirectoryExists ( const _dir : String ) : Boolean ;
  begin
    try
      Result := DirectoryExists( _dir ) ;
    except
      Result := False ;
    end ;
  end ;

{$IFNDEF ISLAND}
  {$IFDEF JAVA}
    {$WARNING '### No implementation for JAVA check if needed'}
  {$ELSE}
    function LoadLibraryWithinHinstance( const _fileName : String ) : HMODULE ;
    var
      {$IFNDEF CLR}
        buf   : array[ 0..MAX_PATH ] of Char ;
      {$ENDIF}
      spath     : String ;
      spath_ext : String ;
    begin
      // get histance path first
      {$IFDEF CLR}
        var asmb := System.Reflection.Assembly.GetExecutingAssembly ;
        var ur    := Uri.Create( asmb.EscapedCodeBase ) ;
        spath := GetPathDirSep(
                   GetFileDir(
                     ur.LocalPath +
                     Uri.UnescapeDataString(ur.Fragment).Replace('/', '\\')
                   )
                 )  ;

      {$ELSE}
        FillChar( buf[0], MAX_PATH+1, #0 ) ;
        GetModuleFileName( HInstance, buf, MAX_PATH ) ;
        spath  := GetFilePath( buf ) ;
      {$ENDIF}

      spath_ext := spath ;

      if GisEnvironmentInfo.IsWindows then begin
        if GisEnvironmentInfo.Is64 then
          spath_ext := spath + GIS_DEFAULT_DLL_PATH_X64 + GisEnvironmentInfo.DirSep
        else
          spath_ext := spath + GIS_DEFAULT_DLL_PATH_X86 + GisEnvironmentInfo.DirSep ;
      end ;

      Result := 0 ;

      {$IFDEF MSWINDOWS_OS}
        {$IFDEF CLR}
          if spath_ext <> spath then
            Result := LoadLibraryEx( spath_ext + _fileName, 0, $00001100 ) ;
          if Result = 0 then // try main folder
            Result := LoadLibraryEx( spath + _fileName, 0, $00001100 ) ;
        {$ELSE}
          if spath_ext <> spath then
            Result := LoadLibraryEx( PChar( spath_ext + _fileName ), 0, $00001100 ) ;
          if Result = 0 then // try main folder
            Result := LoadLibraryEx( PChar( spath + _fileName ), 0, $00001100 ) ;
        {$ENDIF}
      {$ELSE}
        {$IFDEF CLR}
          if spath_ext <> spath then
            Result := LoadLibrary(  spath_ext + _fileName ) ;
          if Result = 0 then // try main folder
            Result := LoadLibrary(  spath + _fileName ) ;
        {$ELSE}
          if spath_ext <> spath then
            Result := LoadLibrary( PChar( spath_ext + _fileName) ) ;
          if Result = 0 then // try main folder
            Result := LoadLibrary( PChar( spath + _fileName) ) ;
        {$ENDIF}
      {$ENDIF}

      if Result = 0 then begin
        // default loading
        {$IFDEF CLR}
          Result := LoadLibrary( _fileName ) ;
        {$ELSE}
          Result := LoadLibrary( PChar( _fileName ) ) ;
        {$ENDIF}
      end ;
    end ;
  {$ENDIF}
{$ENDIF}


    {$IFDEF CLR}
      function  ConvertNETFieldCP(
        const _source   : DataRow    ;
        const _field    : DataColumn ;
        const _sourceCP : Integer
      ) : Variant ;
      var
        str : String ;
      begin
        Result := '' ;
        str := _source[_field].ToString ;
        if _field.DataType = System.Type.GetType('System.Boolean') then
            Result := System.Boolean.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Byte') then
            Result := System.Byte.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Char') then
            Result := System.Char.Parse( str )
        else if _field.DataType = System.Type.GetType('System.DateTime') then
            Result := System.DateTime.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Decimal') then
            Result := System.Decimal.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Double') then
            Result := System.Double.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Int16') then
            Result := System.Int16.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Int32') then
            Result := System.Int32.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Int64') then
            Result := System.Int64.Parse( str )
        else if _field.DataType = System.Type.GetType('System.SByte') then
            Result := System.SByte.Parse( str )
        else if _field.DataType = System.Type.GetType('System.Single') then
            Result := System.Single.Parse( str )
        else if _field.DataType = System.Type.GetType('System.String') then
            Result := str
        else if _field.DataType = System.Type.GetType('System.TimeSpan') then
            { TODO -cReview : check this code }
            (*Result := System.TimeSpan.Parse( str )*)
        else if _field.DataType = System.Type.GetType('System.UInt16') then
            Result := System.UInt16.Parse( str )
        else if _field.DataType = System.Type.GetType('System.UInt32') then
            Result := System.UInt32.Parse( str )
        else if _field.DataType = System.Type.GetType('System.UInt64') then
            Result := System.UInt64.Parse( str )
        else
            assert( False ) ;
      end ;

      function ConvertNETFieldCP(
        const _source   : IDataReader ;
        const _field    : DataRow  ;
        const _sourceCP : Integer
      ) : Variant ;
      begin
        Result := nil;
      end ;
    {$ENDIF}


  {$IFNDEF GIS_NOADO}

    function  ConvertADOFieldCP ( const _source   : Field      ;
                                  const _sourceCP : Integer
                                ) : Variant ;
    var
      vr : Variant ;
    begin
      vr := ConvertVar( _source.Value ) ;
      if VarIsNull( vr ) then
        Result := NullVar
      else begin
        {$IFDEF CLR}
          Result := vr ;
        {$ELSE}
          case _source.Type_ of
            adTinyInt          ,
            adUnsignedTinyInt  ,
            adSmallInt         ,
            adUnsignedSmallInt ,
            adUnsignedInt      ,
            adInteger          :
              begin
                Result := _source.Value ;
              end ;

            adBigInt           ,
            adUnsignedBigInt   :
              begin
                try
                  Result := VarAsType( _source, varInt64  ) ;
                except
                  Result := Double( _source.Value )
                end
              end;

            adSingle           ,
            adDouble           :
              Result := _source.Value ;

            adCurrency         :
              Result := _source.Value ;

            adDecimal          ,
            adNumeric          ,
            adVarNumeric       :
              begin
                if _source.NumericScale = 0 then
                  try
                    Result := VarAsType( _source, varInt64  ) ;
                  except
                    Result := Double( _source.Value )
                  end
                else
                  Result := Double( _source.Value )
              end ;

            adBoolean          :
              Result := _source.Value ;

            adDate             ,
            adDBDate           ,
            adDBTime           ,
            adDBTimeStamp      :
              Result := _source.Value ;

            adChar             ,
            adVarChar          ,
            adLongVarChar      :
              begin
                if not VarIsNull( _source.Value ) then begin
                  Result := TrimRight(
                              ConvertStr2WStrCP( AnsiString( _source.Value ), _sourceCP )
                            )
                end
                else
                  Result := '' ;
              end ;
            adBSTR             ,
            adWChar            ,
            adVarWChar         ,
            adLongVarWChar     :
              begin
                if not VarIsNull( _source.Value ) then begin
                  Result := _source.Value ;
                end
                else
                  Result := '' ;
              end ;
            else
              Result := _source.Value ;
          end ;
        {$ENDIF}
      end ;
    end ;
  {$ENDIF}
  {$IFNDEF GIS_NOJDBC}
      function  ConvertJDBCFieldCP ( const _source   : String      ;
                                     const _sourceCP : Integer
                                   ) : Variant ;
      var
        vr : Variant ;
      begin
        vr := ConvertVar( _source ) ;
        if VarIsNull( vr ) then
          Result := NullVar
        else begin
          Result := vr ;
        end;
      end;
  {$ENDIF}
  {$IFNDEF GIS_NODB}

    function  ConvertDBFieldCP  ( const _source   : TField     ;
                                  const _sourceCP : Integer
                                ) : Variant ;
    var
      tmp : Variant ;
    begin
      if VarIsNull( _source.Value ) then
        Result := NullVar
      else begin
        case _source.DataType of
          TFieldType.ftBCD    ,
          TFieldType.ftFMTBcd :
            begin
              if BcdScale( _source.AsBcd ) = 0 then begin
                try
                  Result := BcdToInteger( _source.AsBcd )  ;
                except
                  Result := _source.AsFloat  ;
                end ;
              end
              else
                Result := _source.AsFloat  ;
            end ;
          else
            begin
              tmp := _source.AsVariant ;
              case VarTypeEx( tmp ) of
                varExAnsiString :
                  begin
                    if not VarIsNull( tmp ) then begin
                      {$IFDEF NEXTGEN}
                        { TODO -cVerify : TEST if varExAnsiString is reached for .IOS }
                      {$ELSE}
                        Result := TrimRight(
                                    ConvertStr2WStrCP( AnsiString( tmp ),
                                                       _sourceCP )
                                  )
                      {$ENDIF}
                    end
                    else
                      Result := '' ;
                  end ;
                varExWideString :
                  begin
                    if not VarIsNull( tmp ) then begin
                      Result := tmp ;
                    end
                    else
                      Result := '' ;
                  end ;
                else
                  try
                    Result := tmp ;
                  except
                    Result := NullVar ;
                  end ;
              end ;
            end ;
        end ;
      end ;
    end ;
  {$ENDIF}

  {$IFNDEF GIS_NODB}
    procedure JoinFieldInfo(
      const _source   : TField     ;
      var   _subtype  : TFieldType ;
      var   _subsize  : Integer
    ) ;
    begin
      _subtype := TFieldType.ftUnknown ;
      _subsize := 0 ;

      case _source.DataType of
        TFieldType.ftBoolean :
          begin
            _subtype := TFieldType.ftBoolean ;
            _subsize := 0 ;
          end ;
        TFieldType.ftDate             ,
        TFieldType.ftTime             ,
        TFieldType.ftDateTime         ,
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftOraTimeStamp   ,
        {$ENDIF}
        TFieldType.ftTimeStamp :
          begin
            _subtype := TFieldType.ftDateTime ;
            _subsize := 0 ;
          end ;
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftByte           ,
        {$ENDIF}
        TFieldType.ftWord             :
          begin
            _subtype := TFieldType.ftWord ;
            _subsize := 0 ;
          end ;
        TFieldType.ftSmallint         ,
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftShortint       ,
        {$ENDIF}
        TFieldType.ftInteger          ,
        TFieldType.ftLargeInt         :
          begin
            _subtype := TFieldType.ftInteger ;
            _subsize := 0 ;
          end ;
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftLongWord       ,
        {$ENDIF}
        TFieldType.ftAutoInc          :
          begin
            {$IFDEF LEVELCVCL2006}
              _subtype := TFieldType.ftLongWord ;
            {$ELSE}
              _subtype := TFieldType.ftInteger  ;
            {$ENDIF}
            _subsize := 0 ;
          end ;
        TFieldType.ftFloat            ,
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftExtended       ,
        {$ENDIF}
        TFieldType.ftCurrency         ,
        TFieldType.ftBCD              ,
        TFieldType.ftFMTBcd           :
          begin
            _subtype := TFieldType.ftFloat ;
            _subsize := 0 ;
          end ;
        TFieldType.ftFixedChar        ,
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftFixedWideChar  ,
        {$ENDIF}
        TFieldType.ftString           ,
        TFieldType.ftWideString       :
          begin
            _subtype := TFieldType.ftString ;
            _subsize := _source.Size ;
          end;
        TFieldType.ftMemo             ,
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftWideMemo       ,
        {$ENDIF}
        TFieldType.ftFmtMemo          ,
        {$IFDEF LEVELCVCL2006}
          TFieldType.ftOraInterval    ,
        {$ENDIF}
        TFieldType.ftOraClob          :
          begin
            _subtype := TFieldType.ftUnknown ;
            _subsize := 0 ;
          end;
        TFieldType.ftVariant          :
          begin
            _subtype := TFieldType.ftString ;
            _subsize := 50 ;
          end;
        else
          begin
            _subtype := TFieldType.ftUnknown ;
            _subsize := 0 ;
          end;
      end ;
    end ;

    function JoinFieldInfo(
      const _source : TField
    ) : Boolean ;
    var
      flt : TFieldType ;
      siz : Integer    ;
    begin
      JoinFieldInfo( _source, flt, siz ) ;
      Result := flt <> TFieldType.ftUnknown ;
    end ;

    function JoinFieldType(
      const _source   : TField ;
        var _subtype  : TGIS_FieldType
    ) : Boolean ;
    var
      flt : TFieldType ;
      siz : Integer    ;
    begin
      JoinFieldInfo( _source, flt, siz ) ;
      case flt of
        TFieldType.ftBoolean  : _subtype := TGIS_FieldType.Boolean;
        TFieldType.ftDateTime : _subtype := TGIS_FieldType.Date;
        TFieldType.ftInteger,
        TFieldType.ftWord     : _subtype := TGIS_FieldType.Number;
        TFieldType.ftFloat    : _subtype := TGIS_FieldType.Float;
        TFieldType.ftString   : _subtype := TGIS_FieldType.String
      end ;
      Result := flt <> TFieldType.ftUnknown ;
    end ;
  {$ENDIF}

  {$IFDEF CLR}
    function JoinFieldInfo(
      const _type   : System.Type
    ) : Boolean ;
    begin
      if (_type = typeOf(TBytes)) or (_type = typeOf(System.Object)) then
        Result := False
      else
        Result := True ;
    end ;

    function JoinFieldType(
      const _source   : System.Type ;
        var _subtype  : TGIS_FieldType
    ) : Boolean ;
    begin
      Result := True ;
      case System.Type.GetTypeCode( _source ) of
        System.TypeCode.String,
        System.TypeCode.Char    : _subtype := TGIS_FieldType.String ;
        System.TypeCode.Byte,
        System.TypeCode.SByte,
        System.TypeCode.Int16,
        System.TypeCode.UInt16,
        System.TypeCode.Decimal,
        System.TypeCode.Int32,
        System.TypeCode.UInt32,
        System.TypeCode.Int64,
        System.TypeCode.UInt64  : _subtype := TGIS_FieldType.Number ;
        System.TypeCode.Double,
        System.TypeCode.Single  : _subtype := TGIS_FieldType.Float ;
        System.TypeCode.Boolean : _subtype := TGIS_FieldType.Boolean ;
        System.TypeCode.DateTime: _subtype := TGIS_FieldType.Date
      else
        Result := False ;
      end ;
    end ;
  {$ENDIF}

  {$IFNDEF GIS_NOADO}

    procedure JoinFieldInfo(
      const _source  : Field      ;
      var   _subtype : TFieldType ;
      var   _subsize : Integer
    ) ;
    begin
      {$IFNDEF OXYGENE}
        case _source.Type_ of
      {$ELSE}
        case _source.Type of
      {$ENDIF}
        adBoolean          :
          begin
            _subtype := TFieldType.ftBoolean  ;
            _subsize := 0 ;
          end ;
        adDate             ,
        adDBDate           ,
        adDBTime           ,
        adDBTimeStamp      :
          begin
            _subtype := TFieldType.ftDateTime ;
            _subsize := 0 ;
          end ;
        adTinyInt          ,
        adUnsignedTinyInt  ,
        adSmallInt         ,
        adUnsignedSmallInt :
          begin
            _subtype := TFieldType.ftSmallint ;
            _subsize := 0 ;
          end ;
        adInteger          ,
        adUnsignedInt      ,
        adBigInt           ,
        adUnsignedBigInt   :
          begin
            _subtype := TFieldType.ftInteger  ;
            _subsize := 0 ;
          end ;
        adSingle           ,
        adDouble           ,
        adNumeric          ,
        adDecimal          ,
        adCurrency         :
          begin
            _subtype := TFieldType.ftFloat    ;
            _subsize := 0 ;
          end ;
        adChar             ,
        adWChar            ,
        adVarChar          ,
        adVarWChar         :
          begin
            _subtype := TFieldType.ftString ;
            _subsize := _source.DefinedSize ;
          end;
        else
          begin
            _subtype := TFieldType.ftUnknown  ;
            _subsize := 0 ;
          end ;
      end ;
    end ;

    function JoinFieldInfo(
      const _source : Field
    ) : Boolean ;
    var
      flt : TFieldType ;
      siz : Integer    ;
    begin
      JoinFieldInfo( _source, flt, siz ) ;
      Result := flt <> TFieldType.ftUnknown ;
    end ;

    function JoinFieldType(
      const _source   : Field ;
        var _subtype  : TGIS_FieldType
    ) : Boolean ;
    var
      flt : TFieldType ;
      siz : Integer    ;
    begin
      JoinFieldInfo( _source, flt, siz ) ;
      case flt of
        TFieldType.ftBoolean  : _subtype := TGIS_FieldType.Boolean;
        TFieldType.ftDateTime : _subtype := TGIS_FieldType.Date;
        TFieldType.ftSmallint,
        TFieldType.ftInteger,
        TFieldType.ftWord     : _subtype := TGIS_FieldType.Number;
        TFieldType.ftFloat    : _subtype := TGIS_FieldType.Float;
        TFieldType.ftString   : _subtype := TGIS_FieldType.String
      end ;
      Result := flt <> TFieldType.ftUnknown ;
    end ;
  {$ENDIF}

  {$IFNDEF GIS_NOJDBC}
    function  JoinFieldInfo (
      const _columnType   : Integer
    ) : Boolean ;
    begin
      case _columnType of
        java.sql.Types.VARCHAR  : Result := True ;
        java.sql.Types.CHAR     : Result := True ;
        java.sql.Types.SMALLINT : Result := True ;
        java.sql.Types.INTEGER  : Result := True ;
        java.sql.Types.DOUBLE   : Result := True ;
        java.sql.Types.DECIMAL  : Result := True ;
        java.sql.Types.NUMERIC  : Result := True ;
        java.sql.Types.BIGINT   : Result := True ;
        java.sql.Types.FLOAT    : Result := True ;
        java.sql.Types.DATE     : Result := True
      else                        Result := False
      end;
    end ;

    function JoinFieldType(
      const _source   : Integer ;
        var _subtype  : TGIS_FieldType
    ) : Boolean ;
    begin
      Result := True ;
      case _source of
        java.sql.Types.CHAR     : _subtype := TGIS_FieldType.Boolean;
        java.sql.Types.VARCHAR  : _subtype := TGIS_FieldType.String ;
        java.sql.Types.SMALLINT,
        java.sql.Types.INTEGER,
        java.sql.Types.DOUBLE,
        java.sql.Types.DECIMAL,
        java.sql.Types.NUMERIC,
        java.sql.Types.BIGINT   : _subtype := TGIS_FieldType.Number;
        java.sql.Types.FLOAT    : _subtype := TGIS_FieldType.Float ;
        java.sql.Types.DATE     : _subtype := TGIS_FieldType.Date
      else                        Result := False
      end;
    end ;
  {$ENDIF}

  function GetDequotedStr( const _str : String
                         ) : String ;
  begin
    Result := GetDequotedStr( _str, '"' ) ;
  end;

  function GetDequotedStr( const _str   : String ;
                           const _quote : Char
                         ) : String ;
  begin
    Result := Trim( _str ) ;

    if ( IsStringEmpty( Result )                ) or
       ( Result[ StringFirst ]        <> _quote ) or
       ( Result[ StringLast(Result) ] <> _quote )
    then exit ;

    Result := Copy( Result, StringFirst + 1, length( Result ) - 2 )
  end ;

  function TemplateProducer( const _text        : String      ;
                             const _tokenlist   : TGIS_StringList ;
                             const _callback    : TGIS_TemplateProducerCallBack ;
                             const _leavetoken  : Boolean
                           ) : String ;
  var
    i      : Integer ;
    k      : Integer ;
    c      : Char    ;
    state  : Integer ;
    sword  : TStringBuilder ;
    sbres  : TStringBuilder ;
  begin
    Result := '' ;
    if IsStringEmpty( _text ) then exit ;

    state  := 0  ;
    sbres  := TStringBuilder.Create( length( _text ) ) ;
    sword  := TStringBuilder.Create ;
    try
      for i := StringFirst to StringLast( _text ) do begin
        c := _text[i] ;
        case state of
          0 : if c = '<' then begin
                                sword.Length := 0 ;
                                sword.Append( c ) ;
                                state := 1 ;
                              end
                         else sbres.Append( c ) ;
          1 : if c = '#' then begin
                                sword.Length := 0 ;
                                state := 2 ;
                              end
                         else begin
                                sword.Append( c ) ;
                                state := 0 ;
                                {$IFDEF ISLAND}
                                  for p : Int32 := 0 to sword.Length-1 do
                                    sbres.Append( sword[p] ) ;
                                {$ELSE}
                                  {$IFDEF JAVA}
                                    java.lang.StringBuilder(sbres).append( sword ) ;
                                  {$ELSE}
                                    sbres.Append( sword ) ;
                                  {$ENDIF}
                                {$ENDIF}
                                sword.Length := 0 ;
                              end ;
          2 : if c = '#' then begin
                                sword.Append( c ) ;
                                state := 3 ;
                              end
                         else begin
                                sword.Append( c ) ;
                              end ;
          3 : case c of
                '>' : begin
                        sword.Length := sword.Length - 1 ;
                        if      assigned( _tokenlist ) then begin
                                  k := _tokenlist.IndexOfName( sword.ToString ) ;
                                  if k >= 0 then begin
                                    sbres.Append(
                                      {$IFNDEF CLR}
                                        _tokenlist.ValueFromIndex[ k ]
                                      {$ELSE}
                                        _tokenlist.Values[ sword.ToString ]
                                      {$ENDIF}
                                    )
                                  end
                                  else begin
                                    if _leavetoken then
                                      sbres.Append( Format( '<#%s#>', [sword.ToString] ) )
                                    else
                                      {$IFDEF ISLAND}
                                        for p : Int32 := 0 to sword.Length-1 do
                                          sbres.Append( sword[p] ) ;
                                      {$ELSE}
                                        {$IFDEF JAVA}
                                          java.lang.StringBuilder(sbres).append( sword.ToString ) ;
                                        {$ELSE}
                                          sbres.Append( sword ) ;
                                        {$ENDIF}
                                      {$ENDIF}
                                  end;
                                end
                        else if assigned( _callback ) then begin
                                  sbres.Append( _callback( sword.ToString ) ) ;
                                end ;
                        state := 0 ;
                        sword.Length := 0 ;
                      end ;
                '#' : begin
                        sword.Append( c ) ;
                      end ;
                 else begin
                        sword.Append( c ) ;
                        state := 2 ;
                      end ;
              end ;
          else assert( False ) ;
        end ;
      end ;
    finally
      Result := sbres.ToString ;
      FreeObject( sbres ) ;
      FreeObject( sword ) ;
    end ;
  end ;

  function ReplaceSQLToken(
    const _command   : String ;
    const _token_src : String ;
    const _token_dst : String
  ) : String ;
  var
    c          : Char    ;
    query_pos  : Integer ;
    res        : TStringBuilder ;

    procedure make_result( const _txt : String ) ;
    begin
      res.Append( _txt ) ;
    end ;

    function eof_char : Boolean ;
    begin
      Result := query_pos > StringLast( _command ) ;
    end ;

    function peek_char : Char ;
    begin
      Result := _command[ query_pos ] ;
    end ;

    function get_char : Char ;
    begin
      Result := peek_char ;
      inc( query_pos ) ;
    end ;

    procedure parse_char ;
    begin
      make_result( get_char ) ;
    end ;

    procedure parse_name ;
    var
      c1    : Char           ;
      tmp   : TStringBuilder ;
      txt   : String         ;
      first : Boolean        ;
    begin
      tmp := TStringBuilder.Create ;
      try

        if peek_char = '"' then begin  // "xxx" syntax
          make_result( get_char ) ;
          while not eof_char do begin
             c1 := get_char ;
             make_result( c1 ) ;
             if c1 = '"' then break ;
          end ;
        end
        else if peek_char = '[' then begin  // [xxx] syntax
          make_result( get_char ) ;
          while not eof_char do begin
             c1 := get_char ;
             make_result( c1 ) ;
             if c1 = ']' then break ;
          end ;
        end
        else begin                   // xxx syntax /without ""/
          first := True ;
          while not eof_char do begin
            if ( not first )
               and
               ( ord( peek_char ) < 128 )
               and
               ( not InCharSet( peek_char,
                                [ '.','AZ','az','09','_','@','$','#']
                              )
               )
            then
              break ;

            tmp.Append( get_char ) ;
            first := False ;
          end ;

          // after all it can be an operator
          txt := tmp.ToString ;
          if CompareText( txt, _token_src  ) = 0
            then make_result( _token_dst )
            else make_result( txt ) ;
        end ;

      finally
        FreeObject( tmp )
      end;
    end ;

    procedure parse_number ;
    var
      c     : Char    ;
      state : Integer ;
    begin
      state := 0 ;
      while not eof_char do begin

        c := peek_char ;

        case state of
          0 :  begin
                 if ( c >= '0' ) and  ( c <= '9' ) then begin
                   // OK
                 end
                 else
                 if ( c = '.' ) then begin
                   state := 1 ;
                 end
                 else
                 if ( c = 'E' ) or ( c = 'e' ) then begin
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

        make_result( get_char ) ;
      end ;
    end ;

    procedure parse_literal ;
    var
      c1   : Char ;
    begin
      make_result( get_char ) ;
      while not eof_char do begin
        c1 := get_char ;
        make_result( c1 ) ;
        if c1 = '''' then begin
          if ( not eof_char ) and ( peek_char = '''' )
            then continue
            else break ;
        end ;
      end ;
    end ;

    procedure parse_operator ;
    var
      tmp : TStringBuilder ;
      c1  : Char           ;
    begin
      tmp := TStringBuilder.Create ;
      try
        while not eof_char do begin
          c1 := peek_char ;
          if ( ord( c1 ) > 127 )
             or
             ( not InCharSet( c1, ['<','>','=','+','-','*','/'] ) )
          then
            break ;

          tmp.Append( get_char ) ;

          if ( tmp.Length = 1 ) and ( c = '=' ) then
            break ;
        end ;
        make_result( tmp.ToString ) ;
      finally
        FreeObject( tmp )
      end;
    end ;

    procedure parse_brace ;
    begin
      make_result( get_char ) ;
    end ;

  begin
    Result     := '' ;

    res := TStringBuilder.Create ;
    try
      query_pos  := StringFirst ;

      while not eof_char do
      begin
        c := peek_char ;
        if      c = ' '                            then parse_char
        else if ord( c ) > 127                     then parse_name
        else if InCharSet( c, ['AZ', 'az', '"', '[' ] )
                                                   then parse_name
        else if InCharSet( c, ['09', '.'] )        then parse_number
        else if InCharSet( c, [''''] )             then parse_literal
        else if InCharSet( c, ['<','>','=','+','-','*','/'] )
                                                   then parse_operator
        else if InCharSet( c, ['(',')'] )          then parse_brace
        else                                            parse_char     ;
      end ;

      Result := res.ToString ;

    finally
      FreeObject( res ) ;
    end;
  end ;

  function ExpandSQLMacros(
    const _command   : String ;
    const _lst       : TGIS_StringList
  ) : String ;
  var
    c          : Char    ;
    query_pos  : Integer ;
    res        : TStringBuilder ;

    year        : Integer ;
    month       : Integer ;
    day         : Integer ;
    hour        : Integer ;
    minutes     : Integer ;
    seconds     : Integer ;
    miliseconds : Integer ;

    procedure make_result( const _txt : String ) ;
    begin
      res.Append( _txt ) ;
    end ;

    function eof_char : Boolean ;
    begin
      Result := query_pos > StringLast( _command ) ;
    end ;

    function peek_char : Char ;
    begin
      Result := _command[ query_pos ] ;
    end ;

    function get_char : Char ;
    begin
      Result := peek_char ;
      inc( query_pos ) ;
    end ;

    procedure parse_char ;
    begin
      make_result( get_char ) ;
    end ;

    procedure parse_name ;
    var
      c1    : Char    ;
      first : Boolean ;
    begin
      if peek_char = '"' then begin  // "xxx" syntax
        make_result( get_char ) ;
        while not eof_char do begin
           c1 := get_char ;
           make_result( c1 ) ;
           if c1 = '"' then break ;
        end ;
      end
      else if peek_char = '[' then begin  // [xxx] syntax
        make_result( get_char ) ;
        while not eof_char do begin
           c1 := get_char ;
           make_result( c1 ) ;
           if c1 = ']' then break ;
        end ;
      end
      else begin                   // xxx syntax /without ""/
        first := True ;
        while not eof_char do begin
          if ( not first )
             and
             ( ord( peek_char ) < 128 )
             and
             ( not InCharSet( peek_char,
                              [ '.','AZ','az','09','_','@','$','#']
                            )
             )
          then
            break ;

          make_result( get_char ) ;
          first := False ;
        end ;
      end ;
    end ;


    procedure parse_macro_params(
      var _ar : TGIS_VariantArray
    )  ;
    var
      tmp    : String  ;
      c1     : Char    ;
      tkn    : TGIS_Tokenizer ;
      i      : Integer ;
    begin
      c1 := get_char ;

      if c1 <> '(' then
        Abort ;

      tmp := '' ;
      while not eof_char do
      begin
        c1 := get_char ;
        tmp := tmp + c1 ;
        if ( not eof_char ) and ( peek_char = ')' ) then
        begin
          get_char ;
          break ;
        end ;
      end ;

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.ExecuteEx( tmp, ',', '"' );
        SetLength( _ar, tkn.Result.Count ) ;

        for i:=0 to tkn.Result.Count-1 do begin
          _ar[i] := Trim( tkn.Result[i] ) ;
        end;
      finally
        FreeObject( tkn ) ;
      end;
    end;

    procedure parse_macro ;
    var
      tmp    : String  ;
      c1     : Char    ;
      param  : TGIS_VariantArray ;
      fmt    : String ;

      function get_today: TDateTime;
      begin
        {$IFDEF CLR}
          Result := System.DateTime.Now.Date ;
        {$ELSE}
          Result := Date ;
        {$ENDIF}
      end;

      function get_now: TDateTime;
      begin
        {$IFDEF CLR}
          Result :=  System.DateTime.Now ;
        {$ELSE}
          Result := Now ;
        {$ENDIF}
      end;

    begin
      get_char ;
      tmp := '' ;
      while not eof_char do
      begin
        c1 := get_char ;
        tmp := tmp + c1 ;
        if ( not eof_char ) and ( ( peek_char = '(' ) or ( peek_char = ' ' ) )
        then begin
          break ;
        end ;
      end ;

      if      tmp = 'date'      then begin
                                  fmt := _lst.Values[ '$date' ] ;
                                  if IsStringEmpty( fmt ) then
                                    fmt := 'yyyy-MM-dd' ;

                                  SetLength( param, 0 ) ;

                                  if not eof_char then
                                    parse_macro_params( param ) ;

                                  if length( param ) < 1 then
                                    exit ;

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
                                    exit ;

                                  try
                                    make_result(
                                      TGIS_StringFormat.Format(
                                        fmt,
                                        EncodeDate(
                                          year        ,
                                          month       ,
                                          day
                                        )
                                      )
                                    ) ;
                                  except
                                  end ;
                                end
      else if tmp = 'time'      then begin
                                  fmt := _lst.Values[ '$time' ] ;
                                  if IsStringEmpty( fmt ) then
                                    fmt := 'HH:mm:ss.fff' ;

                                  SetLength( param, 0 ) ;

                                  if not eof_char then
                                    parse_macro_params( param ) ;

                                  if length( param ) < 1 then
                                    exit ;

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
                                    exit ;

                                  try
                                      make_result(
                                        TGIS_StringFormat.Format(
                                          fmt,
                                          EncodeTime(
                                            hour        ,
                                            minutes     ,
                                            seconds     ,
                                            miliseconds
                                          )
                                        )
                                      ) ;
                                   except
                                  end ;
                                end
      else if tmp = 'datetime'  then begin
                                  fmt := _lst.Values[ '$datetime' ] ;
                                  if IsStringEmpty( fmt ) then
                                    fmt := 'yyyy-MM-dd HH:mm:ss.fff' ;

                                  SetLength( param, 0 ) ;

                                  if not eof_char then
                                    parse_macro_params( param ) ;

                                  if length( param ) < 1 then
                                    exit ;

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
                                    exit ;

                                  try
                                      make_result(
                                        TGIS_StringFormat.Format(
                                          fmt,
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
                                   except
                                  end ;
                                end
      else if tmp = 'now'       then begin
                                  fmt := _lst.Values[ '$datetime' ] ;
                                  if IsStringEmpty( fmt ) then
                                    fmt := 'yyyy-MM-dd HH:mm:ss.fff' ;

                                  make_result(
                                    TGIS_StringFormat.Format(
                                      fmt,
                                      get_now
                                    )
                                  ) ;
                                end
      else if tmp = 'today'     then begin
                                  fmt := _lst.Values[ '$date' ] ;
                                  if IsStringEmpty( fmt ) then
                                    fmt := 'yyyy-MM-dd' ;

                                  make_result(
                                    TGIS_StringFormat.Format(
                                      fmt,
                                      get_today
                                    )
                                  ) ;
                                end
      else Abort ;

    end ;

    procedure parse_number ;
    var
      c     : Char    ;
      state : Integer ;
    begin
      state := 0 ;
      while not eof_char do begin

        c := peek_char ;

        case state of
          0 :  begin
                 if ( c >= '0' ) and  ( c <= '9' ) then begin
                   // OK
                 end
                 else
                 if ( c = '.' ) then begin
                   state := 1 ;
                 end
                 else
                 if ( c = 'E' ) or ( c = 'e' ) then begin
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

        make_result( get_char ) ;
      end ;
    end ;

    procedure parse_literal ;
    var
      c1   : Char ;
    begin
      make_result( get_char ) ;
      while not eof_char do begin
        c1 := get_char ;
        make_result( c1 ) ;
        if c1 = '''' then begin
          if ( not eof_char ) and ( peek_char = '''' )
            then continue
            else break ;
        end ;
      end ;
    end ;

    procedure parse_operator ;
    var
      tmp : TStringBuilder ;
      c1  : Char           ;
    begin
      tmp := TStringBuilder.Create ;
      try
        while not eof_char do begin
          c1 := peek_char ;
          if ( ord( c1 ) > 127 )
             or
             ( not InCharSet( c1, ['<','>','=','+','-','*','/'] ) )
          then
            break ;

          tmp.Append( get_char ) ;

          if ( tmp.Length = 1 ) and ( c = '=' ) then
            break ;
        end ;
        make_result( tmp.ToString ) ;
      finally
        FreeObject( tmp )
      end;
    end ;

    procedure parse_brace ;
    begin
      make_result( get_char ) ;
    end ;

  begin
    Result     := '' ;

    res := TStringBuilder.Create ;
    try
      query_pos  := StringFirst ;

      while not eof_char do
      begin
        c := peek_char ;
        if      c = ' '                            then parse_char
        else if ord( c ) > 127                     then parse_name
        else if InCharSet( c, ['AZ', 'az', '"', '[' ] )
                                                   then parse_name
        else if InCharSet( c, ['$'] )              then parse_macro
        else if InCharSet( c, ['09', '.'] )        then parse_number
        else if InCharSet( c, [''''] )             then parse_literal
        else if InCharSet( c, ['<','>','=','+','-','*','/'] )
                                                   then parse_operator
        else if InCharSet( c, ['(',')'] )          then parse_brace
        else                                            parse_char     ;
      end ;

      Result := res.ToString ;

    finally
      FreeObject( res ) ;
    end;
  end;

  function TestSQLTokens( const _command   : String ;
                          const _tokens    : String
                        ) : Boolean ;
  var
    lst        : TGIS_StringList ;
    c          : Char        ;
    query_pos  : Integer     ;
    res        : Boolean     ;

    procedure make_result( const _txt : String ) ;
    begin
      // do nothing ;
    end ;

    function eof_char : Boolean ;
    begin
      Result := query_pos > StringLast( _command ) ;
    end ;

    function peek_char : Char ;
    begin
      Result := _command[ query_pos ] ;
    end ;

    function get_char : Char ;
    begin
      Result := peek_char ;
      inc( query_pos ) ;
    end ;

    procedure parse_char ;
    begin
      make_result( get_char ) ;
    end ;

    procedure parse_name ;
    var
      i     : Integer        ;
      c1    : Char           ;
      tmp   : TStringBuilder ;
      txt   : String         ;
      first : Boolean        ;
    begin
      tmp := TStringBuilder.Create ;
      try

        if peek_char = '"' then begin  // "xxx" syntax
          make_result( get_char ) ;
          while not eof_char do begin
             c1 := get_char ;
             make_result( c1 ) ;
             if c1 = '"' then break ;
          end ;
        end
        else if peek_char = '[' then begin  // [xxx] syntax
          make_result( get_char ) ;
          while not eof_char do begin
             c1 := get_char ;
             make_result( c1 ) ;
             if c1 = ']' then break ;
          end ;
        end
        else begin                   // xxx syntax /without ""/
          first := True ;
          while not eof_char do begin
            if ( not first )
               and
               ( ord( peek_char ) < 128 )
               and
               ( not InCharSet( peek_char,
                                [ '.','AZ','az','09','_','@','$','#']
                              )
               )
            then
              break ;

            tmp.Append( get_char ) ;
            first := False ;
          end ;

          // after all it can be an operator
          txt := tmp.ToString ;
          for i:=0 to lst.Count - 1 do begin
            res := res or ( CompareText( txt, lst[i] ) = 0 );
            if res then exit ;
          end ;
        end ;

      finally
        FreeObject( tmp ) ;
      end;
    end ;

    procedure parse_number ;
    var
      c     : Char    ;
      state : Integer ;
    begin
      state := 0 ;
      while not eof_char do begin

        c := peek_char ;

        case state of
          0 :  begin
                 if ( c >= '0' ) and  ( c <= '9' ) then begin
                   // OK
                 end
                 else
                 if ( c = '.' ) then begin
                   state := 1 ;
                 end
                 else
                 if ( c = 'E' ) or ( c = 'e' ) then begin
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

        make_result( get_char ) ;
      end ;
    end ;

    procedure parse_literal ;
    var
      c1   : Char ;
    begin
      make_result( get_char ) ;
      while not eof_char do begin
        c1 := get_char ;
        make_result( c1 ) ;
        if c1 = '''' then begin
          if ( not eof_char ) and ( peek_char = '''' )
            then begin
              get_char ;
              continue
            end
            else break ;
        end ;
      end ;
    end ;

    procedure parse_operator ;
    var
      tmp : TStringBuilder ;
      c1  : Char           ;
    begin
      tmp := TStringBuilder.Create ;
      try
        while not eof_char do begin
          c1 := peek_char ;
          if ( ord( c1 ) > 127 )
             or
             ( not InCharSet( c1, ['<','>','=','+','-','*','/'] ) )
          then
            break ;

          tmp.Append( get_char ) ;

          if ( tmp.Length = 1 ) and ( c = '=' ) then
            break ;
        end ;
        make_result( tmp.ToString ) ;
      finally
        FreeObject( tmp )
      end;
    end ;

    procedure parse_brace ;
    begin
      make_result( get_char ) ;
    end ;

  begin
    Result    := False ;
    res       := False ;
    query_pos := StringFirst ;

    lst := TGIS_StringList.Create ;
    try
      lst.Text := _tokens ;
      while ( not eof_char ) and ( not Result ) do begin
        c := peek_char ;
        if      c = ' '                            then parse_char
        else if ord( c ) > 127                     then parse_name
        else if InCharSet( c, ['AZ', 'az', '"', '[' ] )
                                                   then parse_name
        else if InCharSet( c, ['09', '.'] )        then parse_number
        else if InCharSet( c, [''''] )             then parse_literal
        else if InCharSet( c, ['<','>','=','+','-','*','/'] )
                                                   then parse_operator
        else if InCharSet( c, ['(',')'] )          then parse_brace
        else                                            parse_char     ;

        if res then break ;
      end ;
    finally
      FreeObject( lst ) ;
      Result := res ;
    end ;
  end ;

  function GetCommonFilesItem( const _subdir   : String ;
                               const _filename : String
                             ) : String ;
  var
    lst   : TGIS_StringList ;
    path1 : String      ;
    path2 : String      ;
  begin
    Result := '' ;

    lst := TGIS_StringList.Create ;
    try
      try
        path1 := GetPathAbsolute( ExtractFileDir(ParamStr(0)), _filename ) ;
        path2 := GetPathCommonFiles + '\' + _subdir + '\' +
                 GetFileName( _filename );

        if      SafeFileExists( path1 ) then lst.LoadFromFile( path1 )
        else if SafeFileExists( path2 ) then lst.LoadFromFile( path2 ) ;
      except
      end ;

      Result := lst.Text ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure SetCommonFilesItem( const _subdir   : String ;
                                const _filename : String ;
                                const _content  : String
                              ) ;
  var
    lst  : TGIS_StringList ;
    path : String      ;
  begin
    path := GetPathCommonFiles + '\' + _subdir + '\' + _filename ;

    if not IsStringEmpty( _content ) then begin
      lst := TGIS_StringList.Create ;
      try
        lst.Text := _content ;
        lst.SaveToFile( path ) ;
      finally
        FreeObject( lst ) ;
      end ;
    end
    else begin
      {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( path ) ;
    end ;
  end ;

  function GetSQLDialect( const _dialect : String ) : String ;

    function _t( const _str : String ) : Boolean ;
    begin
      Result := CompareText( _dialect, _str ) = 0 ;
    end ;

  begin
    Result := GetCommonFilesItem( GIS_COMMONFILES_SQLDIALECT, _dialect ) ;
    if not IsStringEmpty( Result ) then exit ;

    if      _t( GIS_SQL_DIALECT_NAME_MSJET        ) then
         Result := GIS_SQL_DIALECT_MSJET
    else if _t( GIS_SQL_DIALECT_NAME_MSSQL        ) then
         Result := GIS_SQL_DIALECT_MSSQL
    else if _t( GIS_SQL_DIALECT_NAME_MSSQLCE      ) then
         Result := GIS_SQL_DIALECT_MSSQLCE
    else if _t( GIS_SQL_DIALECT_NAME_INTERBASE    ) then
         Result := GIS_SQL_DIALECT_INTERBASE
    else if _t( GIS_SQL_DIALECT_NAME_MYSQL        ) then
         Result := GIS_SQL_DIALECT_MYSQL
    else if _t( GIS_SQL_DIALECT_NAME_DB2          ) then
         Result := GIS_SQL_DIALECT_DB2
    else if _t( GIS_SQL_DIALECT_NAME_ORACLE       ) then
         Result := GIS_SQL_DIALECT_ORACLE
    else if _t( GIS_SQL_DIALECT_NAME_INFORMIX     ) then
         Result := GIS_SQL_DIALECT_INFORMIX
    else if _t( GIS_SQL_DIALECT_NAME_ADVANTAGE    ) then
         Result := GIS_SQL_DIALECT_ADVANTAGE
    else if _t( GIS_SQL_DIALECT_NAME_SAPDB        ) then
         Result := GIS_SQL_DIALECT_SAPDB
    else if _t( GIS_SQL_DIALECT_NAME_POSTGRESQL   ) then
         Result := GIS_SQL_DIALECT_POSTGRESQL
    else if _t( GIS_SQL_DIALECT_NAME_BLACKFISHSQL ) then
         Result := GIS_SQL_DIALECT_BLACKFISHSQL
    else if _t( GIS_SQL_DIALECT_NAME_SQLITE       ) then
         Result := GIS_SQL_DIALECT_SQLITE
    else if _t( GIS_SQL_DIALECT_NAME_SYBASE       ) then
         Result := GIS_SQL_DIALECT_SYBASE
    else if _t( GIS_SQL_DIALECT_NAME_INTERSYSTEMS ) then
         Result := GIS_SQL_DIALECT_INTERSYSTEMS
    else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNSUPPORTEDDIALECT ),
                                      _dialect, 0
                                    ) ;
  end ;

  function ReadSQLParamsFromPath(
    const _path   : String ;
    const _lst    : TGIS_Strings
  ) : Integer ;
  begin
    Result := ReadSQLParamsFromPath( _path, _lst, False ) ;
  end;

  {$IFDEF ISLAND}
    function ReadSQLParamsFromPath(
      const _path   : String   ;
      const _lst    : TGIS_Strings ;
      const _nofile : Boolean
    ) : Integer ;
    begin
      Result := 0 ;
      {$WARNING '### Verify ISLAND code'}
    end;
  {$ELSE}
  function ReadSQLParamsFromPath(
    const _path   : String   ;
    const _lst    : TGIS_Strings ;
    const _nofile : Boolean
  ) : Integer ;
  var
    cfg   : TGIS_Config    ;
    lst   : TGIS_StringList ;
    ext   : String      ;
    spath : String      ;
    i     : Integer     ;

    function _t( const _ext : String ) : Boolean ;
    begin
      Result := CompareText( ext, _ext ) = 0 ;
    end ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;
    Result := 0 ;

    lst := TGIS_StringList.Create ;
    try
      lst.Text := ConvertParamString( spath, '' ) ;
      if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_STORAGE ] ) ) then begin
        // not file based
        if      not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO ] ) )
                then Result := GIS_SQL_PROVIDER_ADO
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO64 ] ) )
                then Result := GIS_SQL_PROVIDER_ADO
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_DBX ] ) )
                then Result := GIS_SQL_PROVIDER_DBX
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ] ) )
                then Result := GIS_SQL_PROVIDER_SQLITE
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADONET ] ) )
                then Result := GIS_SQL_PROVIDER_ADONET
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_OCI ] ) )
                then Result := GIS_SQL_PROVIDER_OCI
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_LIBPQ ] ) )
                then Result := GIS_SQL_PROVIDER_LIBPQ
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_JDBC ] ) )
                then Result := GIS_SQL_PROVIDER_JDBC
        else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_FIREDAC ] ) )
                then Result := GIS_SQL_PROVIDER_FIREDAC ;
      end
      else if _nofile then begin
        // do nothing = result should still be 0
      end
      else begin
        // file based
        ext := GetFileExt( spath ) ;
        if _t( GIS_TTKLS_EXT    ) or
           _t( GIS_TTKPS_EXT    ) or
           _t( GIS_TTKLAYER_EXT )
        then begin
          spath := GetPathAbsolute( '', spath ) ;
          if SafeFileExists( spath ) then begin
            cfg := TGIS_ConfigFactory.CreateConfig( nil, spath ) ;
            if assigned( cfg ) then begin
              try
                if ( cfg.ConfigFormat = TGIS_ConfigFormat.Ini ) then
                  cfg.ReadSectionValues( GIS_INI_LAYER_HEADER, lst )
                else
                  cfg.ReadSectionValues( GIS_INI_LAYERSQL_CONNECTOR, lst ) ;
              finally
                FreeObject( cfg ) ;
              end ;
            end ;

            if      not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO ] ) )
                    then Result := GIS_SQL_PROVIDER_ADO
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADO64 ] ) )
                    then Result := GIS_SQL_PROVIDER_ADO
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_DBX ] ) )
                    then Result := GIS_SQL_PROVIDER_DBX
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_SQLITE ] ) )
                    then Result := GIS_SQL_PROVIDER_SQLITE
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_ADONET ] ) )
                    then Result := GIS_SQL_PROVIDER_ADONET
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_OCI ] ) )
                    then Result := GIS_SQL_PROVIDER_OCI
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_LIBPQ ] ) )
                    then Result := GIS_SQL_PROVIDER_LIBPQ
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_JDBC ] ) )
                    then Result := GIS_SQL_PROVIDER_JDBC
            else if not IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_CONNECTOR_FIREDAC ] ) )
                    then Result := GIS_SQL_PROVIDER_FIREDAC ;
          end
          else
            Result := 1 ;
        end ;
      end ;

      if assigned( _lst ) then
        _lst.Assign( lst ) ;

      for i := 0 to _lst.Count - 1 do begin
        _lst[i] := GisAliasList.Resolve( _lst[i] ) ;
      end;

    finally
      FreeObject( lst ) ;
    end ;
  end ;
  {$ENDIF}

  function GetSQLParamFromPath(
    const _path  : String ;
    const _name  : String ;
    var   _value : String
  ) : Integer ;
  var
    lst   : TGIS_StringList ;
    spath : String      ;
  begin
    spath := GisAliasList.Resolve( _path ) ;

    lst := TGIS_StringList.Create ;
    try
      Result := ReadSQLParamsFromPath( spath, lst ) ;
      _value := Trim( lst.Values[ _name ] ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  {$IFDEF ISLAND}
    procedure ReadParamsFromPath(
      const _path : String   ;
      const _lst  : TGIS_Strings
    ) ;
    begin
      {$WARNING '### Verify ISLAND code'}
    end;
  {$ELSE}
  procedure ReadParamsFromPath(
    const _path : String   ;
    const _lst  : TGIS_Strings
  ) ;
  var
    cfg   : TGIS_Config    ;
    lst   : TGIS_StringList ;
    spath : String      ;
    i     : Integer     ;
  begin
    spath  := GisAliasList.Resolve( _path ) ;

    lst := TGIS_StringList.Create ;
    try
      lst.Text := ConvertParamString( spath, '' ) ;
      if IsStringEmpty( Trim( lst.Values[ GIS_INI_LAYERSQL_STORAGE ] ) ) then
      begin
        // file based
        if SafeFileExists( spath ) then begin
          cfg := TGIS_ConfigFactory.CreateConfig( nil, GetPathAbsolute( '', spath ) ) ;
          if assigned( cfg ) then begin
            try
              if ( cfg.ConfigFormat = TGIS_ConfigFormat.Ini ) then
                cfg.ReadSectionValues( GIS_INI_LAYER_HEADER, lst )
              else
                cfg.ReadSectionValues( GIS_INI_LAYERSQL_CONNECTOR, lst ) ;
            finally
              FreeObject( cfg ) ;
            end ;
          end ;
        end
        else
          exit ;
      end ;

      if assigned( _lst ) then
        _lst.Assign( lst ) ;

      for i := 0 to _lst.Count - 1 do
        _lst[i] := GisAliasList.Resolve( _lst[i] ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;
  {$ENDIF}

  function GetParamFromPath(
    const _path  : String ;
    const _name  : String
  ) : String ;
  var
    lst   : TGIS_StringList ;
    spath : String      ;
  begin
    spath := GisAliasList.Resolve( _path ) ;

    lst := TGIS_StringList.Create ;
    try
      ReadParamsFromPath( spath, lst ) ;
      Result := Trim( lst.Values[ _name ] ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  {$IFDEF MSWINDOWS_OS}
    function CreateMSJET ( const _path : String ) : String ;
    var
      {$IFDEF CLR}
        cat : Catalog  ;
      {$ELSE}
        cat : OleVariant ;
      {$ENDIF}
      spath : String     ;
    begin
      spath  := GisAliasList.Resolve( _path ) ;

      if not SafeFileExists( spath ) then begin
        {$IFDEF CLR}
          {$IFNDEF OXYGENE}
            cat := CoCatalog._Create ;
          {$ELSE}
            cat := new CatalogClass ;
          {$ENDIF}
        {$ELSE}
          cat := CreateOleObject( 'ADOX.Catalog' ) ;
        {$ENDIF}
        try
          {$IFNDEF OXYGENE}
            cat.Create( Format( FILE_JET_PROVIDER_CREATE, [ spath ] ) ) ;
          {$ELSE}
            cat.&Create( Format( FILE_JET_PROVIDER_CREATE, [ spath ] ) ) ;
          {$ENDIF}
        finally
          {$IFDEF CLR}
            cat := nil ;
          {$ELSE}
            cat := Unassigned ;
          {$ENDIF}
        end ;
      end ;

      Result := Format( FILE_JET_PROVIDER, [ GetFileName( spath ) ] )
    end ;
  {$ELSE}
    function CreateMSJET ( const _path : String ) : String ;
    begin
      Result := Format( FILE_JET_PROVIDER, [ '' ] ) //?
    end ;
  {$ENDIF}

  function ToJoinFieldName( const _name : String ) : String ;
  begin
    Result := GIS_FIELD_JOINPREFIX + _name ;
  end ;

  function FromJoinFieldName( const _name : String ) : String ;
  begin
    if Pos( UpperCase( GIS_FIELD_JOINPREFIX ), UpperCase( _name ) ) = StringFirst then
      Result := Copy( _name, length( GIS_FIELD_JOINPREFIX )+StringFirst, 1024 )
    else
      Result := _name ;
  end ;

  function URLGetPath(
    const _url : String
  ) : String ;
  var
    k : Integer ;
  begin
    k := Pos( '?', _url ) ;
    if k > StringFirst - 1 then
      Result := Copy( _url, StringFirst, k + 1 - StringFirst )
    else
      Result := _url ;
  end ;

  function URLGetQuery(
    const _url : String
  ) : String ;
  var
    k : Integer ;
  begin
    k := Pos( '?', _url ) ;
    if k > StringFirst - 1 then
      Result := Copy( _url, k+1, 16384 )
    else
      Result := '' ;
  end ;

  function URLGetParameters(
    const _url : String
  ) : TGIS_StringList ;
  var
    tmp : String ;
    tkn : TGIS_Tokenizer ;
  begin
    tmp := URLGetQuery( _url ) ;

    Result := TGIS_StringList.Create ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( tmp, ['&'], False );

      Result.Assign( tkn.Result );
    finally
      FreeObject( tkn ) ;
    end;
  end ;

  function URLDeleteParameter(
    const _url  : String ;
    const _param : String
  ) : String ;
  var
    lst : TGIS_StringList ;
    url : String      ;
    i   : Integer     ;
    k   : Integer     ;
  begin
    try
      url := URLGetPath(  _url ) ;

      lst := URLGetParameters( _url ) ;

      k := lst.IndexOfName( _param ) ;
      if k >= 0 then begin
        lst.Delete( k );
      end ;

      Result := url ;
      for i:= 0 to lst.Count - 1 do begin
        if i = 0 then
          Result := Result + lst[i]
        else
          Result := Result + '&' + lst[i] ;
      end ;

    finally
      FreeObject( lst  ) ;
    end ;
  end ;

  function URLTestParameter(
    const _url   : String ;
    const _param : String
  ) : Boolean ;
  var
    lst : TGIS_StringList ;
  begin
    try
      lst := URLGetParameters( _url ) ;

      Result := lst.IndexOf( _param ) > 0 ;
    finally
      FreeObject( lst  ) ;
    end ;
  end ;

  function URLAddParameter(
    const _url   : String ;
    const _param : String
  ) : String ;
  begin
    Result := _url ;

    if not IsStringEmpty( _param ) then begin
      if Pos( '?', _url ) > StringFirst then begin
        if _url[ StringLast( _url ) ] <> '?' then
          Result := Result + '&' ;
      end
      else
        Result := Result + '?' ;
      Result := Result + _param ;
    end ;
  end ;

  function URLDecode(
    const _encodedStr : String
  ) : String ;
  var
    i  : Integer ;
    sb : TStringBuilder ;
  begin
    Result := '';

    if length( _encodedStr ) > 0 then begin
      sb := TStringBuilder.Create ;
      try
        i := StringFirst ;
        while i <= StringLast( _encodedStr ) do begin
          if _encodedStr[i] = '%' then begin
            sb.Append( chr(StrToInt('$'+_encodedStr[i+1]+_encodedStr[i+2])) ) ;
            inc( i, 2 ) ;
          end
          else if _encodedStr[i] = '+' then
            sb.Append(' ')
          else
            sb.Append( _encodedStr[i] ) ;

          inc( i ) ;
        end;
        Result := sb.ToString() ;
      finally
        FreeObject( sb ) ;
      end;
    end;
  end;

  function URLEncode(
    const _decodedStr : String
  ) : String ;
  var
    j   : Integer ;
    c   : {$IFDEF JAVA} SByte
          {$ELSE}       Byte
          {$ENDIF}      ;
    arb : TBytes  ;
  begin
    Result := '' ;
    arb := TEncoding.UTF8.GetBytes( _decodedStr ) ;

    for j := low(arb) to high(arb) do begin
      c := arb[ j ] ;

      case Char(c) of
        // RFC 3986
        '_', '-', '.', '~',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        '1', '2', '3', '4', '5', '6', '7', '8', '9' :
          Result := Result + Char(c)
        else
          Result := Result + Format( '%%%x', [ c ] ) ;
      end;
    end ;
  end ;

  function URLGetParameterValue(
    const _url   : String ;
    const _param : String
  ) : String ;
  var
    lst : TGIS_StringList ;
  begin
    try
      lst := URLGetParameters( _url ) ;
      Result := URLDecode( lst.Values[ _param  ] ) ;
    finally
      FreeObject( lst  ) ;
    end ;
  end ;

  function URLGetAndDeleteParameterValue(
    var   _url   : String ;
    const _param : String
  ) : String ;
  begin
    Result := URLGetParameterValue( _url, _param ) ;
    _url := URLDeleteParameter( _url, _param ) ;
  end ;

  function URLFixed( const _url : String )
    : String ;
  var
    i   : Integer ;
    c   : Char    ;
    bld : TStringBuilder ;
    k   : Integer ;
  begin
    bld := TStringBuilder.Create ;
    try
      k := 0 ;
      for i:= StringFirst to StringLast( _url ) do begin
        c := _url[ i ] ;

        case c of
          '?' : begin
                  if k = 0 then
                    bld.Append( c )
                  else
                    bld.Append( '&' ) ;

                  inc( k ) ;
                end;
          '&' : begin
                  if k = 0 then
                    bld.Append( '?' )
                  else
                    bld.Append( c ) ;

                  inc( k ) ;
                end;
          else  begin
                  bld.Append( c ) ;
                end;
        end;

      end;

      Result := bld.ToString ;
    finally
      FreeObject( bld ) ;
    end;
  end;

  function DecodeContentType(
    const _content    : String ;
    const _basictypes : Boolean
  ) : TGIS_ContentType ;

    function _t( _format : String ) : Boolean ;
    begin
      Result := Pos( UpperCase( _format ), UpperCase( _content ) )
                = StringFirst ;
    end;

    function _p( _format : String ) : Boolean ;
    begin
      Result := Pos( UpperCase( _format ), UpperCase( _content ) ) >= StringFirst ;
    end;

  begin

    if _basictypes then begin
      if      _t( GIS_CONTENTTYPE_GIF      ) then Result := TGIS_ContentType.Gif
      else if _t( GIS_CONTENTTYPE_JPEG     ) then Result := TGIS_ContentType.Jpg
      else if _t( GIS_CONTENTTYPE_JPG      ) then Result := TGIS_ContentType.Jpg
      else if _t( GIS_CONTENTTYPE_PNG24    ) then Result := TGIS_ContentType.Png
      else if _t( GIS_CONTENTTYPE_PNG      ) then Result := TGIS_ContentType.Png
      else if _p( GIS_CONTENTTYPE_BINARY   ) then Result := TGIS_ContentType.Binary
      else if _p( GIS_CONTENTTYPE_BINARY2  ) then Result := TGIS_ContentType.Binary
      else if _p( GIS_CONTENTTYPE_BINARY3  ) then Result := TGIS_ContentType.Binary
      else                                        Result := TGIS_ContentType.Unknown ;
    end
    else begin
      if      _t( GIS_CONTENTTYPE_GIF    ) then Result := TGIS_ContentType.Gif
      else if _t( GIS_CONTENTTYPE_JPEG   ) then Result := TGIS_ContentType.Jpg
      else if _t( GIS_CONTENTTYPE_JPG    ) then Result := TGIS_ContentType.Jpg
      else if _t( GIS_CONTENTTYPE_PNG24  ) then Result := TGIS_ContentType.Png24
      else if _t( GIS_CONTENTTYPE_PNG    ) then Result := TGIS_ContentType.Png
      else if _p( GIS_CONTENTTYPE_BINARY ) then Result := TGIS_ContentType.Binary
      else if _p( GIS_CONTENTTYPE_BINARY2) then Result := TGIS_ContentType.Binary
      else if _p( GIS_CONTENTTYPE_BINARY3) then Result := TGIS_ContentType.Binary
      else                                      Result := TGIS_ContentType.Unknown ;
    end ;
  end;

  function ExpandForcedRTreePath( const _path : String ) : String ;
  begin
    Result := _path ;
    if not IsStringEmpty( ForcedRTreePath ) then
      if not SafeFileExists( Result ) then begin
        Result := _path ;
        {$IFDEF JAVA}
        Result := StringReplace( Result, ':\', '_', [TReplaceFlag.rfReplaceAll] ) ;
        Result := StringReplace( Result, ':' , '_', [TReplaceFlag.rfReplaceAll] ) ;
        Result := StringReplace( Result, '\' , '_', [TReplaceFlag.rfReplaceAll] ) ;
        Result := StringReplace( Result, '/' , '_', [TReplaceFlag.rfReplaceAll] ) ;
        {$ELSE}
        Result := StringReplace( Result, ':\', '_', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
        Result := StringReplace( Result, ':' , '_', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
        Result := StringReplace( Result, '\' , '_', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
        Result := StringReplace( Result, '/' , '_', DefReplaceFlags( [TReplaceFlag.rfReplaceAll] ) ) ;
        {$ENDIF}
        Result := GetPathDirSep( ForcedRTreePath ) + Result ;
      end ;
  end ;

  function GetTimeZoneBias : Integer ;
  begin
    {$IFNDEF OXYGENE}
      Result := RoundS( -TTimeZone.Local.UtcOffset.TotalMinutes ) ;
    {$ELSE}
      {$IFDEF JAVA}
       Result := Convert.ToInt32(
                    Math.Round(
                      ( DateTime.UtcNow - DateTime.Today ).Minutes
                    )
                 ) ;
      {$ENDIF}
      {$IFDEF CLR}
        Result := Convert.ToInt32(
                    Math.Round(
                      ( DateTime.UtcNow - DateTime.Now ).TotalMinutes
                    )
                 ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  function DateTimeToXMLString(
          _dtm : TDateTime ;
    const _idt : Integer   ;
    const _btz : Boolean
  ) : String ;
  var
    sb  : TStringBuilder ;
    yh  : Word    ;
    mm  : Word    ;
    ds  : Word    ;
    m   : Word    ;
    iz  : Integer ;
    ih  : Integer ;
    sdt : String  ;
    boo : Boolean ;
    i   : Integer ;
    len : Integer ;
    idt : Integer ;

    function zero_int_to_str( const _i : Integer ) : String ;
    begin
      Result := IntToStr( _i ) ;

      if length( Result ) = 1 then
        sb.Append( '0' ) ;
    end ;

  begin
    Result := '' ;

    if _idt < 0 then
      idt := 0
    else
      idt := _idt ;

    sb := TStringBuilder.Create ;
    try

      DecodeDate( _dtm, yh, mm, ds ) ;

      sdt := IntToStr( yh ) ;
      len := length( sdt ) ;
      if len < 4 then
        for i := len to 3 do
          sb.Append( '0' ) ;

      sb.Append( sdt ) ;
      sb.Append( '-' ) ;
      sb.Append( zero_int_to_str( mm ) ) ;
      sb.Append( '-' ) ;
      sb.Append( zero_int_to_str( ds ) ) ;

      if idt = 0 then begin
        Result := sb.ToString ;
        exit ;
      end ;

      sb.Append( 'T' ) ;

      DecodeTime( _dtm, yh, mm, ds, m ) ;

      sb.Append( zero_int_to_str( yh ) ) ;
      sb.Append( ':' ) ;
      sb.Append( zero_int_to_str( mm ) ) ;

      if idt > 1 then begin
        sb.Append( ':' ) ;
        sb.Append( zero_int_to_str( ds ) ) ;
      end ;

      if idt > 2 then begin
        sb.Append( '.' ) ;
        sb.Append( IntToStr( m ) ) ;
      end ;

      if _btz then begin
        iz := GetTimeZoneBias ;
        if iz = 0 then
          sb.Append( 'Z' )
        else begin
          ih := iz div -60 ;

          boo := True ;
          if ih < 0 then begin
            ih := Abs( ih ) ;
            boo := False ;
          end ;

          if boo then
            sb.Append( '+' )
          else
            sb.Append( '-' ) ;

          sb.Append( zero_int_to_str( ih ) ) ;
          sb.Append( ':' ) ;
          sb.Append( zero_int_to_str( Abs( iz ) mod 60 ) ) ;
        end ;
      end ;

      Result := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

  function XMLStringToDateTime(
    const _str : String
  ) : TDateTime ;
  var
    dt   : TDateTime ;
    sb   : TStringBuilder ;
    stt  : Integer ;
    v    : Integer ;
    last : Integer ;
    i, k : Integer ;
    stz  : String  ;
    btz  : Boolean ;
    dst  : Double  ;
    yr, mh, dy, hr, me, sd, md : Word ;

    procedure collect( const _num : Integer ) ;
    begin
      sb.Append( _str[i] ) ;
      if i = k + StringFirst + _num then
        inc( stt ) ;
    end ;

    function convert( const _chr : Char ; var _wrd : Word ) : Boolean ;
    begin
      Result := False ;

      if ( i = last ) and
         ( _str[i] <> 'Z' ) then
        exit
      else
      if _str[i] = _chr then begin
        if TryStrToInt( sb.ToString, v ) then begin
          _wrd := v ;
          sb.Length := 0 ;
          inc( stt ) ;
        end
        else
          exit ;
      end
      else
        exit ;

      Result := True ;
    end ;

    function getMsecs( const _str : String ) : Word ;
    var
      fs : String  ;
    begin
      fs := '0.' + _str ;
      try
        Result := FloorS( DotStrToFloat( fs ) * 1000 ) ;
      except
        Result := 0 ;
      end
    end ;

  begin
    dt := EncodeDateTime( 1899, 12, 30, 0, 0, 0, 0 ) ;

    if (Pos('-', _str) < StringFirst) then begin
      try
        dst := DotStrToFloat( _str ) ;
        {$IFDEF DCC}
          Result := dt + dst ;
        {$ENDIF}
        {$IFDEF CLR}
          Result := dt.AddDays( dst ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          Result := dt.AddDays( Integer(dst) );
        {$ENDIF}
        exit ;
      except
        Result := dt ;
      end ;
    end
    else
      Result := dt ;

    sb := TStringBuilder.Create ;
    try

      k := 0 ;
      if _str[StringFirst] = '-' then
        inc( k ) ;

      hr := 0 ; me := 0 ; sd := 0 ; md := 0 ;
      stt := 0 ;
      btz := False ;
      last := StringLast( _str ) ;
      for i := StringFirst + k to last do begin
        case stt of
          0  : begin
                collect( 3 ) ;
                if i = last then
                  exit ;
               end ;
          1  : if not convert( '-', yr ) then exit ;
          2  : begin
                 collect( 6 ) ;
                if i = last then
                  exit ;
               end ;
          3  : if not convert( '-', mh ) then exit ;
          4  : begin
                 collect( 9 ) ;
                 if i = last then
                   if (i - k) = (8 + StringFirst) then
                     exit
                   else
                   if TryStrToInt( sb.ToString, v ) then begin
                     dy := v ;
                     break ;
                   end
                   else
                     exit ;
               end ;
          5  : if not (convert( 'T', dy ) or convert( ' ', dy )) then exit ;
          6  : begin
                 collect( 12 ) ;
                if i = last then
                  exit ;
               end ;
          7  : if not convert( ':', hr ) then exit ;
          8  : begin
                 collect( 15 ) ;
                 if i = last then
                   if (i - k) = (14 + StringFirst) then
                     exit
                   else
                   if TryStrToInt( sb.ToString, v ) then begin
                     me := v ;
                     break ;
                   end
                   else
                     exit ;
               end ;
          9  : if not convert( ':', me ) then begin
                 if not ( convert( '+', me ) or
                          convert( '-', me ) )then begin
                   if not convert( 'Z', me ) then begin
                     exit ;
                   end
                   else
                     break ;
                 end
                 else begin
                   stt := 13 ;
                   btz := True ;
                 end ;
               end ;
          10 : begin
                 collect( 18 ) ;
                 if i = last then
                   if (i - k) = (17 + StringFirst) then
                     exit
                   else
                   if TryStrToInt( sb.ToString, v ) then begin
                     sd := v ;
                     break ;
                   end
                   else
                     exit ;
               end ;
          11 : if not convert( '.', sd ) then begin
                 if not ( convert( '+', sd ) or
                          convert( '-', sd ) ) then begin
                   if not convert( 'Z', sd ) then begin
                     exit ;
                   end
                   else
                     break ;
                 end
                 else begin
                   stt := 13 ;
                   btz := True ;
                 end ;
               end ;
          12 : if i = last then begin
                 if ( _str[i] <> 'Z' ) then
                   sb.Append( _str[i] ) ;
                 if TryStrToInt( sb.ToString, v ) then begin
                   md := v ;
                  // fractional seconds
                   if md > 999 then
                     md := getMsecs( sb.ToString ) ;
                   break ;
                 end
                 else
                   exit ;
               end
               else
               if InCharSet( _str[i], [ '+', '-' ] ) then begin
                 // fractional seconds
                 try
                   md := getMsecs( sb.ToString ) ;
                   sb.Length := 0 ;
                   btz := True ;
                   inc( stt ) ;
                 except
                   exit ;
                 end
               end
               else
                 sb.Append( _str[i] ) ;
          13 : sb.Append( _str[i] ) ;
        end ;
      end ;

      if btz then begin
        stz := sb.ToString ;
        if length( stz ) <> 5 then
          exit ;
        if stz[StringFirst+2] <> ':' then
          exit ;

        sb.Length := 0 ;
        sb.Append( _str[StringLast( _str ) - 5] ) ;
        sb.Append( stz[StringFirst]   ) ;
        sb.Append( stz[StringFirst+1] ) ;
        sb.Append( '.' ) ;
        sb.Append( stz[StringFirst+3] ) ;
        sb.Append( stz[StringFirst+4] ) ;

        stz := sb.ToString ;

        try
          dst := DotStrToFloat( stz ) ;
        except
          exit ;
        end ;

        if ( dst < -12 ) or
           ( dst >  14 ) then
          exit ;
      end ;

      try
        Result := EncodeDateTime( yr, mh, dy, hr, me, sd, md ) ;
      except
        Result := dt ;
      end ;

    finally
      FreeObject( sb ) ;
    end ;
  end ;

  function StrToBoolean(
    const _value     : String ;
    const _default   : Boolean
  ) : Boolean ;

    function t( const _txt : String ) : Boolean ;
    begin
      Result := CompareText( _txt, _value ) = 0 ;
    end ;
  begin
    if _value = GIS_PARAM_NIL then begin
      Result := _default ;
      exit ;
    end ;

    if      t( GIS_INI_PARAM_BOOLEAN_YES     ) then Result := True
    else if t( GIS_INI_PARAM_BOOLEAN_NO      ) then Result := False
    else if t( GIS_INI_PARAM_BOOLEAN_TRUE    ) then Result := True
    else if t( GIS_INI_PARAM_BOOLEAN_FALSE   ) then Result := False
    else if t( GIS_INI_PARAM_BOOLEAN_1       ) then Result := True
    else if t( GIS_INI_PARAM_BOOLEAN_0       ) then Result := False
    else                                            Result := _default ;
  end ;

  function LstReadString(
    const _list      : TGIS_StringList ;
    const _name      : String ;
    const _default   : String
  ) : String ;
  var
    s : String ;
  begin
    Result := _default ;

    s := Trim( _list.Values[ _name ] ) ;
    if IsStringEmpty( s ) then
      exit ;

    Result := s ;
  end ;

  function LstReadInteger(
    const _list      : TGIS_StringList ;
    const _name      : String ;
    const _default   : Integer
  ) : Integer ;
  var
    s : String ;
  begin
    Result := _default ;

    s := LstReadString( _list, _name, '' ) ;
    if IsStringEmpty( s ) then
      exit ;

    try
      Result := StrToInt( s ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                   _name + '=' + s ,
                                   0
                                 ) ;
    end ;
  end ;

  function LstReadFloat(
    const _list      : TGIS_StringList ;
    const _name      : String ;
    const _default   : Double
  ) : Double ;
  var
    s : String ;
  begin
    Result := _default ;

    s := LstReadString( _list, _name, '' ) ;
    if IsStringEmpty( s ) then
      exit ;

    try
      Result := DotStrToFloat( s ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPARAM ),
                                   _name + '=' + s ,
                                   0
                                 ) ;
    end ;
  end ;

  function LstReadBoolean(
    const _list      : TGIS_StringList ;
    const _name      : String ;
    const _default   : Boolean
  ) : Boolean ;
  var
    s : String ;
  begin
    s := LstReadString( _list, _name, '' ) ;

    Result := StrToBoolean( s, _default ) ;
  end ;


//==============================================================================
// conversion functions
//==============================================================================


  function SystemErrorMessage : String ;
  var
    err : Integer ;
    msg : String ;
  begin
    {$IFDEF CLR}
      err := Marshal.GetLastWin32Error() ;
      msg := System.ComponentModel.Win32Exception.Create(err).Message;
    {$ENDIF}
    {$IFDEF DCC}
      err := GetLastError ;
      msg := SysErrorMessage( err ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      err := 0 ;
      msg := '' ;
    {$ENDIF}
    Result := Format( '%s (errno %d)', [ msg, err ] ) ;
  end ;

  function GetVariantType(
    const _var : Variant
  ) : TGIS_VariantType ;
  begin
    case VarType( _var ) of
      varEmpty        : Result := TGIS_VariantType.Nothing     ; // $0000
      varNull         : Result := TGIS_VariantType.Nothing     ; // $0001
      varSmallInt     : Result := TGIS_VariantType.Int         ; // $0002
      varInteger      : Result := TGIS_VariantType.Int         ; // $0003
      varSingle       : Result := TGIS_VariantType.Float       ; // $0004
      varDouble       : Result := TGIS_VariantType.Float       ; // $0005
      {$IFNDEF OXYGENE}
        varCurrency   : Result := TGIS_VariantType.Fixed       ; // $0006
        varDate       : Result := TGIS_VariantType.DateTime    ; // $0007
      {$ENDIF}
      {$IFDEF CLR}
        varString     : Result := TGIS_VariantType.WideString  ; // $0008
      {$ELSE}
        varOleStr     : Result := TGIS_VariantType.WideString  ; // $0008
      {$ENDIF}
      {$IFDEF OXYGENE}
        varDispatch   : Result := TGIS_VariantType.Unsupported ; // $0009
      {$ELSE}
        {$IFNDEF CLR}
          varDispatch : Result := TGIS_VariantType.Unsupported ; // $0009
        {$ENDIF}
      {$ENDIF}
      varError        : Result := TGIS_VariantType.Unsupported ; // $000A
      varBoolean      : Result := TGIS_VariantType.Boolean     ; // $000B
      {$IFDEF CLR}
        varObject     : Result := TGIS_VariantType.Unsupported ; // $000C
      {$ELSE}
        varVariant    : Result := TGIS_VariantType.Unsupported ; // $000C
      {$ENDIF}
      {$IFDEF OXYGENE}
        varUnknown    : Result := TGIS_VariantType.Unsupported ; // $000D
      {$ELSE}
        {$IFNDEF CLR}
          varUnknown  : Result := TGIS_VariantType.Unsupported ; // $000D
        {$ENDIF}
      {$ENDIF}
      {$IFDEF CLR}
        varDecimal    : Result := TGIS_VariantType.Fixed       ; // $000E
      {$ENDIF}
      //varUndef0F    : Result := TGIS_VariantType.Unsupported ; // $000F - unsupported by MS
      varShortInt     : Result := TGIS_VariantType.Int         ; // $0010
      varByte         : Result := TGIS_VariantType.UInt        ; // $0011
      varWord         : Result := TGIS_VariantType.UInt        ; // $0012
      varLongWord     : Result := TGIS_VariantType.UInt        ; // $0013
      varInt64        : Result := TGIS_VariantType.Int64       ; // $0014
      {$IFDEF CLR}
        varUInt64     : Result := TGIS_VariantType.UInt64      ; // $0015
        varChar       : Result := TGIS_VariantType.WideString  ;  //$0016
      {$ENDIF}
      {$IFNDEF OXYGENE}
        varStrArg     : Result := TGIS_VariantType.Int         ; // $0048
        varString     : Result := TGIS_VariantType.AnsiString  ; // $0100
      {$ENDIF}
      {$IFDEF OXYGENE}
        varAny        : Result := TGIS_VariantType.Unsupported ; // $0101
        varDateTime   : Result := TGIS_VariantType.DateTime    ; // $0017
      {$ELSE}
        {$IFNDEF CLR}
          varAny      : Result := TGIS_VariantType.Unsupported ; // $0101
        {$ENDIF}
      {$ENDIF}
      {$IFNDEF OXYGENE}
        varUString  : Result := TGIS_VariantType.WideString  ; // $0102
      {$ENDIF}
      varTypeMask     : Result := TGIS_VariantType.Unsupported ; // $0FFF
      varArray        : Result := TGIS_VariantType.Unsupported ; // $2000
      {$IFDEF OXYGENE}
        varByRef      : Result := TGIS_VariantType.Unsupported   // $4000
      {$ELSE}
        {$IFNDEF CLR}
          varByRef    : Result := TGIS_VariantType.Unsupported   // $4000
        {$ENDIF}
      {$ENDIF}
      else              Result := TGIS_VariantType.Unsupported ;
    end ;
  end ;


  function ThreadStorage : TGIS_ThreadStorage ;
  begin
    if not assigned( FThreadStorageList ) then
      FThreadStorageList := T_ThreadStorageList.Create ;
    if not assigned( FThreadStorage ) then
      FThreadStorage := FThreadStorageList.New ;

    Result := FThreadStorage ;
  end;


  function GetDefaultUserAgent( const _agnt : String ) : String ;
  var
    ssystem   : String ;
    splatform : String ;
    agnt      : String ;
  begin
    {$IFDEF OXYGENE}
      {$MESSAGE ERROR 'CODE FOR MONO ON LINUX etx'}
    {$ENDIF}

    {$IFDEF MSWINDOWS}
      ssystem :=  StringReplace(
                    {$IFDEF CLR}Environment.{$ENDIF}
                    GetEnvironmentVariable( 'OS' ), '_', '+',
                    DefReplaceFlags( [TReplaceFlag.rfReplaceAll] )
                   ) ;
      splatform := 'Win32' ;
      if {$IFDEF CLR}Environment.{$ENDIF}
         GetEnvironmentVariable( 'PROCESSOR_ARCHITEW6432' ) = 'AMD64'
      then
        splatform := 'WOW64' ;
      if {$IFDEF CLR}Environment.{$ENDIF}
         GetEnvironmentVariable( 'PROCESSOR_ARCHITECTURE' ) = 'AMD64' then
        splatform := 'Win64;+x64' ;
    {$ENDIF}
    {$IFDEF JAVA}
      ssystem   := Environment.getOSName ;
      ssystem := StringReplace( ssystem, '_', '+',
                                [TReplaceFlag.rfReplaceAll]
                              ) ;
      ssystem := StringReplace( ssystem, ' ', '+',
                                [TReplaceFlag.rfReplaceAll]
                              ) ;
      if Environment.OSBitness = 32 then
        splatform := 'Win32'
      else
        splatform := 'Win64' ;
    {$ENDIF}
    {$IFDEF MONO}
      ssystem   := 'Unknown' ; //?
      splatform := 'Mono' ;
    {$ENDIF}
    {$IFDEF DCC}
      {$MESSAGE WARN '### Verify no Windows code ## '}
      ssystem := StringReplace( TOSVersion.Name, '_', '+',
                                DefReplaceFlags( [TReplaceFlag.rfReplaceAll] )
                              ) ;
      ssystem := StringReplace( ssystem, ' ', '+',
                                DefReplaceFlags( [TReplaceFlag.rfReplaceAll] )
                              ) ;

      if TOSVersion.Architecture = arIntelX86 then
        splatform := 'X32' ;
      if TOSVersion.Architecture = arIntelX64 then
        splatform := 'X64' ;
    {$ENDIF}

    if _agnt <> '' then
      agnt := _agnt
    else
      agnt := 'ttk' ;

    {$IFDEF ISLAND}
      Result := Format( 'Mozilla/4.0+(%s+%s;+%s;+%s)',
                        [ ssystem,
                          Environment.OSVersion,
                          splatform,
                          agnt
                        ]
                        ) ;
    {$ELSE}
      {$IFDEF JAVA}
        Result := Format( 'Mozilla/4.0+(%s+%s;+%s;+%s)',
                          [ ssystem,
                            System.getProperty("os.version"),
                            splatform,
                            agnt
                          ]
                         ) ;
      {$ELSE}
        Result := Format( 'Mozilla/4.0+(%s+%d.%d;+%s;+%s)',
                          [ ssystem,
                            {$IFDEF CLR}
                              System.Environment.OSVersion.Version.Major,
                            {$ELSE}
                              {$IFDEF MSWINDOWS}
                                Win32MajorVersion,
                              {$ELSE}
                                TOSVersion.Major,
                              {$ENDIF}
                            {$ENDIF}
                            {$IFDEF CLR}
                              System.Environment.OSVersion.Version.Minor,
                            {$ELSE}
                              {$IFDEF MSWINDOWS}
                                Win32MinorVersion,
                              {$ELSE}
                                TOSVersion.Minor,
                              {$ENDIF}
                            {$ENDIF}
                            splatform,
                            agnt
                          ]
                         ) ;
      {$ENDIF}
    {$ENDIF}
  end ;


  function PrepareCharSet( const _def : array of String ) : TCharSet ;
  var
    i : Integer ;
    j : Integer ;
    st : String ;
    k  : Integer ;
    l  : Integer ;
  begin
    for i := low( Result ) to high( Result ) do
      Result[i] := False ;

    for i := 0 to high( _def ) do begin
      st := _def[i] ;
      if length(st) = 0 then Abort
      else if length(st) = 1 then begin
        k := ord( st[StringFirst] ) ;
        if k < 32  then Abort ;
        if k > 127 then Abort ;

        Result[k] := True ;
      end
      else if length(st) = 2 then begin
        k := ord( st[StringFirst] ) ;
        if k < 32  then Abort ;
        if k > 127 then Abort ;

        l := ord( st[StringFirst+1] ) ;
        if l < 32  then Abort ;
        if l > 127 then Abort ;

        if l <= k then Abort ;

        {$IFDEF ISLAND}
          {$WARNING '### ISLAND compiler bug - review'}
          // compiler bug in Release doesn't set Result[j] values
          j := k ;
          while j = l do begin
            Result[j] := True ;
            Result[k] := True ;
            inc( j ) ;
            inc( k ) ;
          end ;
        {$ELSE}
          for j := k to l do
            Result[j] := True ;
        {$ENDIF}
      end
      else if length(st) > 2 then Abort ;
    end;
  end;

  function InCharSet( const _c : Char; const _set : TCharSet ) : Boolean ;
  var
    k : Integer ;
  begin
    k := ord( _c ) ;

    Result := False ;

    if k < 32  then exit ;
    if k > 127 then exit ;

    Result := _set[ k ] ;
  end;

  function InCharSet( const _c : Char; const _def : array of String ) : Boolean ;
  var
    st : TCharSet ;
  begin
    st := PrepareCharSet( _def ) ;

    Result := InCharSet( _c, st ) ;
  end;

  {$IFDEF MANAGED}
    function AssignedPtr(
      const _val : IntPtr
    ) : Boolean ;
    begin
      {$IFDEF JAVA}
        Result := _val <> 0 ;
      {$ELSE}
        Result := _val <> nil ;
      {$ENDIF}
    end ;
  {$ELSE}
    function AssignedPtr(
      const _val : Pointer
    ) : Boolean ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
    begin
      Result := assigned( _val ) ;
    end ;
  {$ENDIF}


  function ConvertParamString(
    const _value   : String ;
    const _default : String
  ) : String ;
  var
    i      : Integer ;
    ilen   : Integer ;
    state  : Integer ;
    c      : Char    ;
    ares   : array of Char ;
    ires   : Integer ;
    touch  : Boolean ;
  begin
    if _value = GIS_PARAM_NIL then begin
      Result := _default ;
      exit ;
    end ;

    if IsStringEmpty( _value ) then
      Result := _default
    else begin
      ilen := StringLast( _value ) ;
      SetLength( ares, ilen + 1 ) ;

      ires := 0 ;
      state := 0 ;
      touch := False ;
      for i:= StringFirst to ilen do begin
        c := _value[ i ] ;
        case state of
          0 : if      ( i = StringFirst ) and ( c = '@' )
                                  then begin
                                    state := 2 ;
                                  end
              else if c = '\'     then begin
                                    state := 1 ;
                                  end
              else                begin
                                    ares[ ires ] := c   ;
                                    inc( ires ) ;
                                  end ;
          1 : if      c = 'n'     then begin
                                    ares[ ires ] := #13 ;
                                    inc( ires ) ;
                                    ares[ ires ] := #10 ;
                                    inc( ires ) ;
                                    state := 0 ;
                                    touch := True ;
                                  end
              else if c = '\'     then begin
                                    ares[ ires ] := '\' ;
                                    inc( ires ) ;
                                    state := 0 ;
                                    touch := True ;
                                  end
              else                begin
                                    ares[ ires ] := '\' ;
                                    inc( ires ) ;
                                    ares[ ires ] := c   ;
                                    inc( ires ) ;
                                    state := 0 ;
                                  end ;
          2 :                     begin
                                    ares[ ires ] := c   ;
                                    inc( ires ) ;
                                    touch := True ;
                                  end ;
          else                  begin
                                  assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                                end ;
        end ;
      end ;
      if state = 1 then begin
        ares[ ires ] := '\' ;
        inc( ires ) ;
      end;

      if touch then begin
        {$IFDEF OXYGENE}
          {$IFDEF JAVA}
            ares := java.util.Arrays.copyOf(ares, ires) ;
          {$ELSE}
            SetLength( ares, ires ) ;
          {$ENDIF}
          {$IFNDEF OXYGENE}
            Result := String( ares ) ;
          {$ELSE}
            Result := new String( ares ) ;
          {$ENDIF}
        {$ELSE}
          Result := Copy( PChar(@(ares[0])), 0, ires ) ;
        {$ENDIF}
      end
      else
        Result := _value ;

    end ;

  end ;

  {$IFDEF JAVA}
  procedure InitializeMatrixArray(
    var _arr : TGIS_Matrix3x3
   ) ;
  var
    i : Integer ;
  begin
    {$WARNING '### remove when static array initialization is fixed'}
    for i := 0 to 4 do
      _arr[i] := new Double[4] ;
  end ;

  procedure InitializeDoubleArray(
      var _arr    : array[0..] of array[0..] of Double ;
    const _width  : Integer
   ) ;
  var
    i : Integer ;
  begin
    {$WARNING '### remove when static array initialization is fixed'}
    for i := 0 to length(_arr)-1 do
      _arr[i] := new Double[_width] ;
  end ;
  {$ENDIF}

  function InitializeGrid( const _height : Integer ;
                            const _width  : Integer
                          ) : TGIS_GridArray ;
  begin
    {$IFDEF OXYGENE}
      Result := new array of Single[_height] ;
      var i : Integer ;
      for i := 0 to _height-1 do
        Result[i] := new Single[_width] ;
    {$ELSE}
      SetLength( Result, _height, _width ) ;
    {$ENDIF}
  end ;


  {$IFDEF OXYGENE}
    function SwapWord    ( const _arr : TBytes ;
                           const _off : Integer
                         ) : Word ;
    var
      b : Byte ;
    begin
      b            := _arr[_off  ] ;
      _arr[_off  ] := _arr[_off+1] ;
      _arr[_off+1] := b            ;
      Result := _arr[_off  ] +
                _arr[_off+1] shl 8;
    end ;

    function SwapSmallInt( const _arr : TBytes ;
                           const _off : Integer
                         ) : SmallInt ;
    var
      a : array of Byte ;
    begin
      SetLength( a, 2 ) ;
      a[0] := _arr[_off+1] ;
      a[1] := _arr[_off] ;
      Result := BitConverter.ToInt16( a, 0 ) ;
    end ;

    function SwapCardinal( const _arr : TBytes ;
                           const _off : Integer
                         ) : Cardinal ;
    var
      b : Byte ;
    begin
      b            := _arr[_off  ] ;
      _arr[_off  ] := _arr[_off+3] ;
      _arr[_off+3] := b            ;
      b            := _arr[_off+1] ;
      _arr[_off+1] := _arr[_off+2] ;
      _arr[_off+2] := b            ;
      Result := _arr[_off  ] +
                _arr[_off+1] shl  8 +
                _arr[_off+2] shl 16 +
                _arr[_off+3] shl 24 ;
    end ;

    function SwapSingle( const _arr : TBytes ;
                         const _off : Integer
                       ) : Single ;
    var
      a : array of Byte ;
    begin
      SetLength( a, 4 ) ;
      a[0] := _arr[_off+3] ;
      a[1] := _arr[_off+2] ;
      a[2] := _arr[_off+1] ;
      a[3] := _arr[_off+0] ;
      Result := BitConverter.ToSingle( a, 0 ) ;
    end ;
  {$ENDIF}

  function SwapLongInt( const _val : DWORD ) : DWORD ;
  var
    word_A : DWORD ;
    word_B : DWORD ;
    lval   : DWORD;
  begin
    lval := _val;
    word_A := Integer(lval) shr 16 ;
    word_A := Swap(word_A) ;
    word_B := lval and $FFFF ;
    word_B := Swap(word_B) ;
    Result := DWORD( word_A  or (Integer(word_B) shl 16) );
  end ;

  function SwapShort2LongInt( const _val : DWORD ) : DWORD ;
  var
    word_A : DWORD ;
    word_B : DWORD ;
    lval   : DWORD;
  begin
    lval := _val ;
    word_A := lval shr 16;
    word_A := Swap( word_A ) ;
    word_B := lval and $FFFF;
    word_B := Swap( word_B ) ;
    Result :=  (word_A shl 16) or word_B  ;
  end ;

  {$IFDEF OXYGENE}
    function SwapSingle( const _val : Single ) : Single ;
    var
      a : Array of Byte ;
      b : Byte ;
    begin
      a := BitConverter.GetBytes( _val ) ;
      b    := a[0] ;
      a[0] := a[3] ;
      a[3] := b    ;
      b    := a[1] ;
      a[1] := a[2] ;
      a[2] := b    ;
      Result := BitConverter.ToSingle( a, 0 ) ;
    end ;

    function SwapSmallint( const _val : SmallInt ) : SmallInt ;
    var
      a : array of Byte ;
      b : Byte ;
    begin
      a := BitConverter.GetBytes( _val ) ;
      b    := a[0] ;
      a[0] := a[1] ;
      a[1] := b    ;
      Result := BitConverter.ToInt16( a, 0 ) ;
    end ;
  {$ENDIF}
  /// <summary>
  ///   Compares a value with min-max range.
  /// </summary>
  /// <param name="_min">
  ///   minimum range value
  /// </param>
  /// <param name="_max">
  ///   maximum range value
  /// </param>
  /// <param name="_value">
  ///   given value
  /// </param>
  /// <returns>
  ///   _value if it is in the range,
  ///   _min if given value is below range,
  ///   _max if given value is over the range
  /// </returns>
  function compareNumberWithRange(
    const _min   : Double ;
    const _max   : Double ;
    const _value : Double
  ) : Double ; inline ;
  var
    min, max : Double ;
  begin
    // high precision of comparing doubles is not required

    min := _min ;
    max := _max ;
    if min > max then begin
      min := _max ;
      max := _min
    end ;

    if _value < min then
      Result := min
    else if _value > max then
      Result := max
    else
      Result := _value ;
  end;

  function getInterpolationFactor(
    const _x1   : Double  ;
    const _x2   : Double ;
    const _x    : Double ;
    const _base : Double
  ) : Double ; inline;
  var
    x2_x1_diff : Double ;
    x_trans    : Double ;
    pow_x      : Double ;
    pow_x1     : Double ;
    pow_x2     : Double ;
  begin
    x2_x1_diff := _x2 - _x1 ;
    if GisIsSameValue( x2_x1_diff, 0 ) then begin
      Result := 0 ;
      exit;
    end ;

    // transform x to range 0..1
    x_trans := ( _x - _x1 ) / x2_x1_diff ;

    // linear interpolation
    if GisIsSameValue( _base, 1 ) then begin
      Result := x_trans ;
      exit;
    end ;

    // then calc power
    pow_x := Power( _base, x_trans ) ;
    pow_x1 := 1 ;
    pow_x2 := _base ;

    Result := ( pow_x - pow_x1 ) / ( pow_x2 - pow_x1 ) ;
  end;

  function calcInterpolationFormula(
    const _y1      : Double ;
    const _y2      : Double ;
    const _factor  : Double
  ) : Double ; inline ;
  begin
    // use simple inerpolation formula
    Result := _y1 + _factor * ( _y2 - _y1 ) ;
  end;

  function InterpolateValue(
    const _x1   : Double ;
    const _x2   : Double ;
    const _y1   : Double ;
    const _y2   : Double ;
    const _x    : Double ;
    const _base : Double
  ) : Double ;
  var
    x      : Double ;
    factor : Double ;
  begin
    x := compareNumberWithRange( _x1, _x2, _x ) ;

    factor := getInterpolationFactor( _x1, _x2, x, _base ) ;
    Result := calcInterpolationFormula( _y1, _y2, factor ) ;
  end;

  /// <summary>
  ///   Interpolates a color between two colors in HSL color space.
  /// </summary>
  /// <param name="_startColor">
  ///   start color (color for _min value)
  /// </param>
  /// <param name="_endColor">
  ///   end color (color for _max value)
  /// </param>
  /// <param name="_minValue">
  ///   minimal value
  /// </param>
  /// <param name="_maxValue">
  ///   maximal value
  /// </param>
  /// <param name="_value">
  ///   value for which gradient color will be computed
  /// </param>
  /// <param name="_base">
  ///   interpolation base; 1 for linear (default),
  ///   &gt1 for exponential, &lt for logarithmic
  /// </param>
  /// <param name="_force360Crossing">
  ///   interpolate with hue wrapping; False by default
  /// </param>
  /// <returns>
  ///   Computed color.
  /// </returns>
  function gradientColorAHSL(
    const _startColor       : TGIS_Color ;
    const _endColor         : TGIS_Color ;
    const _minValue         : Double ;
    const _maxValue         : Double ;
    const _value            : Double ;
    const _base             : Double = 1 ;
    const _force360Crossing : Boolean = False
  ) : TGIS_Color ;
  var
    factor : Double ;
    h, h1, h2 : Double ;
    s, s1, s2 : Double ;
    l, l1, l2 : Double ;
    a, a1, a2 : Double ;
  begin
    factor := getInterpolationFactor( _minValue, _maxValue, _value, _base ) ;
    _startColor.ToAHSL( a1, h1, s1, l1 ) ;
    _endColor.ToAHSL( a2, h2, s2, l2 ) ;

    if not GisIsSameValue( h1, h2 ) then begin
      if _force360Crossing then begin
        if h1 < h2 then
          h1 := h1 + 1
        else if h1 > h2 then
          h2 := h2 + 1 ;
      end ;
    end;

    h := calcInterpolationFormula( h1, h2, factor ) ;
    if _force360Crossing then begin
      h := h - TruncS( h ) ;
    end ;

    a := calcInterpolationFormula( a1, a2, factor ) ;
    s := calcInterpolationFormula( s1, s2, factor ) ;
    l := calcInterpolationFormula( l1, l2, factor ) ;

    Result := TGIS_Color.FromAHSL( a, h, s, l ) ;
  end;

  /// <summary>
  ///   Interpolates a color between two colors in RGB color space.
  /// </summary>
  /// <param name="_startColor">
  ///   start color (color for _min value)
  /// </param>
  /// <param name="_endColor">
  ///   end color (color for _max value)
  /// </param>
  /// <param name="_minValue">
  ///   minimal value
  /// </param>
  /// <param name="_maxValue">
  ///   maximal value
  /// </param>
  /// <param name="_value">
  ///   value for which gradient color will be computed
  /// </param>
  /// <param name="_base">
  ///   interpolation base; 1 for linear (default),
  ///   &gt1 for exponential, &lt1 for logarithmic
  /// </param>
  /// <returns>
  ///   Computed color.
  /// </returns>
  function gradientColorARGB(
    const _startColor : TGIS_Color ;
    const _endColor   : TGIS_Color ;
    const _minValue   : Double ;
    const _maxValue   : Double ;
    const _value      : Double ;
    const _base       : Double = 1
  ) : TGIS_Color ;
  var
    factor : Double ;
    a, r, g, b : Byte ;
  begin
    factor := getInterpolationFactor( _minValue, _maxValue, _value, _base ) ;
    a := RoundS( calcInterpolationFormula( _startColor.A, _endColor.A, factor ) ) ;
    r := RoundS( calcInterpolationFormula( _startColor.R, _endColor.R, factor ) ) ;
    g := RoundS( calcInterpolationFormula( _startColor.G, _endColor.G, factor ) ) ;
    b := RoundS( calcInterpolationFormula( _startColor.B, _endColor.B, factor ) ) ;

    Result := TGIS_Color.FromARGB( a, r, g, b ) ;
  end;

  function GradientColor(
    const _startColor : TGIS_Color ;
    const _endColor   : TGIS_Color ;
    const _minValue   : Double ;
    const _maxValue   : Double ;
    const _value      : Double ;
    const _colorSpace : TGIS_ColorInterpolationMode ;
    const _base       : Double
  ) : TGIS_Color ;
  var
    value : Double ;
  begin
    value := compareNumberWithRange( _minValue, _maxValue, _value );
    case _colorSpace of
      TGIS_ColorInterpolationMode.RGB : begin
        Result := gradientColorARGB(
          _startColor,
          _endColor,
          _minValue,
          _maxValue,
          value,
          _base
        ) ;
      end;
      TGIS_ColorInterpolationMode.HSL : begin
        Result := gradientColorAHSL(
          _startColor,
          _endColor,
          _minValue,
          _maxValue,
          value,
          _base
        ) ;
      end;
      TGIS_ColorInterpolationMode.HSL360 : begin
        Result := gradientColorAHSL(
          _startColor,
          _endColor,
          _minValue,
          _maxValue,
          value,
          _base,
          True
        ) ;
      end;
      TGIS_ColorInterpolationMode.None : Result := _startColor ;
    end ;
  end;

function ColorFromColorRamp(
  const _colorRamp          : TGIS_ColorMapArray ;
  const _min                : Double ;
  const _max                : Double ;
  const _value              : Double ;
  var   _colorRampZoneIndex : Integer
) : TGIS_Color ;
  var
    val            : Double ;
    start_colormap : TGIS_ColorMap ;
    end_colormap   : TGIS_ColorMap ;
    // ColorMapArray indexes
    i              : Integer ;
    i_low          : Integer ;
    i_high         : Integer ;
    // ramps use integer indexes from 0 to 100
    val_index      : Double ;
    min_index      : Double ;
    max_index      : Double ;
  begin
    val := compareNumberWithRange( _min, _max, _value ) ;

    val_index := 100 * ( _value - _min ) / ( _max - _min ) ;

    i_low := 0 ;
    // -2 because 'max_idx' can not exceed array size
    i_high := length( _colorRamp ) - 2 ;

    // bisection search
    while i_low <= i_high do begin
      // find zone index
      if ( _colorRampZoneIndex >= 0 ) and ( _colorRampZoneIndex <= i_high ) then begin
        i := _colorRampZoneIndex ;
        _colorRampZoneIndex := -1 ;
      end
      else begin
        i := ( i_high + i_low ) div 2 ;
      end ;

      min_index := _colorRamp[i].Index ;
      max_index := _colorRamp[i+1].Index;

      if val_index < min_index then
        i_high := i - 1
      else if val_index >= max_index then
        i_low := i + 1
      else
        break ;
    end;

    _colorRampZoneIndex := i ;

    start_colormap := _colorRamp[i];
    end_colormap := _colorRamp[i+1];

    Result := GradientColor(
      start_colormap.RGB,
      end_colormap.RGB,
      start_colormap.Index,
      end_colormap.Index,
      val_index,
      TGIS_ColorInterpolationMode.RGB
    ) ;
  end;

  function DecodeDateTimeToSqlMacro( const _dt : Variant ) : String ;
  var
    y, m, d, h, mi ,s, ms : Word ;
    len : Integer ;
  begin
    {$IFDEF OXYGENE}
      var dt := VarToDateTime(_dt) ;
      {$IFDEF JAVA}
      if (dt.getHour=0) and (dt.getMinute=0) and (dt.getSecond=0) then
        len := length( new java.text.SimpleDateFormat("dd.MM.yyyy").format( dt ) )
      else
        len := length( new java.text.SimpleDateFormat("dd.MM.yyyy hh:mm:ss").format( dt ) );
      {$ENDIF}
      {$IFDEF CLR}
        if (dt.Hour=0) and (dt.Minute=0) and (dt.Second=0) then
          len := length( dt.ToShortDateString )
        else
          len := length( dt.ToString );
      {$ENDIF}
      {$IFDEF ISLAND}
        len := length( dt.ToString );
      {$ENDIF}
    {$ELSE}
      len := length( _dt ) ;
    {$ENDIF}
    if len = 8 then begin
      DecodeTime(  VarToDateTime(_dt), h, mi, s, ms ) ;
      Result := Format( '$time(%d,%d,%d,%d)', [Integer(h), Integer(mi), Integer(s), Integer(ms)] ) ;
    end
    else if len > 10 then begin
      DecodeDate(  VarToDateTime(_dt), y, m , d ) ;
      DecodeTime(  VarToDateTime(_dt), h, mi, s, ms ) ;
      Result := Format( '$datetime(%d,%d,%d,%d,%d,%d)',
                        [ Integer(y), Integer(m), Integer(d),
                          Integer(h), Integer(mi), Integer(s), Integer(ms) ]
                      ) ;
    end
    else begin
      DecodeDate(  VarToDateTime(_dt), y, m, d ) ;
      Result := Format( '$date(%d,%d,%d)', [Integer(y), Integer(m), Integer(d)] ) ;
    end ;
  end ;

  function SelfInvokeClassMethod(
    const _class_prefix : String ;
    const _class_method : String
  ) : Boolean ;
  var
   {$IFDEF DCC}
     rtti_ctx    : TRttiContext ;
     otype       : TRttiType ;
     omth        : TRttiMethod ;
   {$ENDIF}
   {$IFDEF CLR}
     asmb : System.Reflection.Assembly ;
     atyp : Array of System.Type ;
     aobj : Array of Object ;
     mi   : System.Reflection.MethodInfo ;
   {$ENDIF}
   {$IFDEF JAVA}
     o : Object ;
   {$ENDIF}
   {$IFDEF ISLAND}
     o : Object ;
   {$ENDIF}
  begin
    Result := False ;

    {$IFDEF DCC}
      // we don't use Delphi DCC because it requires full RTTI compilation
      // to avoid linker code elimination
      rtti_ctx := TRttiContext.Create ;
      for otype in rtti_ctx.GetTypes do begin
        if otype.TypeKind <> tkClass then continue;
        if Pos( _class_prefix, otype.Name ) <> 1 then continue ;

        for omth in oType.GetMethods do begin
          if omth.Name <> _class_method then continue ;

          omth.Invoke( otype.AsInstance.MetaclassType, [] ) ;
          Result := True ;
        end ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      asmb := System.Reflection.Assembly.GetExecutingAssembly ;
      aobj := nil ;
      try
        atyp := asmb.GetTypes ;
      except
        on E : System.Reflection.ReflectionTypeLoadException do
          atyp := E.Types ;
      end ;
      for t in atyp do begin
        if not assigned( t ) then continue ;
        if Pos( _class_prefix, t.Name ) <> StringFirst then continue ;
        try
          mi := t.GetMethod( _class_method ) ;
        except
          continue ;
        end ;
        if ( mi <> nil ) then begin
          try
          mi.Invoke( nil, aobj ) ;
          except
            continue;
          end;
          Result := True ;
        end ;
      end ;
    {$ENDIF}
    {$IFDEF JAVA}
      {$IFDEF ANDROID}
        var df : dalvik.system.DexFile := new dalvik.system.DexFile(
                                            GisPackageCodePath
                                          );
        var classFileNames := df.entries();
        while (classFileNames.hasMoreElements()) do begin
          var cname : String := classFileNames.nextElement();
             if cname.startsWith(_class_prefix) then begin
                var c := java.lang.Thread
                                     .currentThread
                                     .ContextClassLoader
                                     .loadClass(cname) ;

                for omth in c.DeclaredMethods do begin
                  if omth.Name = _class_method then begin
                    omth.invoke( o, [] ) ;
                    Result := True ;
                  end ;
                end;
              end ;
        end ;
      {$ELSE}
        var cp : String := java.net.URLDecoder
                             .decode( typeOf(TGIS_RegistredLayers)
                                      .getProtectionDomain()
                                      .getCodeSource()
                                      .getLocation()
                                      .getPath(),
                                      'UTF-8'
                                    );
        if assigned( cp ) then begin
          var jar: java.io.File := new java.io.File(cp);
          var &is: java.util.jar.JarInputStream := new java.util.jar.JarInputStream(
                                                     new java.io.FileInputStream(jar)
                                                   );
          var entry: java.util.jar.JarEntry;
          entry := &is.NextJarEntry ;
          while (entry <> nil) do begin
            if entry.Name.endsWith('.class') then begin
              var cname := entry
                            .getName()
                            .replaceAll('/','.')
                            .replace('.class','') ;
              if cname.startsWith(_class_prefix) then begin
                var c := java.lang.Thread
                                    .currentThread()
                                    .ContextClassLoader
                                    .loadClass(cname) ;

                for omth in c.DeclaredMethods do begin
                  if omth.Name = _class_method then begin
                    omth.invoke( o, [] ) ;
                    Result := True ;
                  end ;
                end;
              end ;
            end;
            entry := &is.getNextJarEntry() ;
          end;
        end ;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end ;

  function DetectValueType(
    const _value : String
  ) : TGIS_FieldType ;
  var
    c           : Char ;
    j           : Integer ;
    i           : Integer ;
    w           : Integer ;
    clen        : Integer ;
    maxlen      : Integer ;
    fnd_mantisa : Boolean ;
    fnd_dot     : Boolean ;
    fnd_exp     : Boolean ;
    fnd_zero    : Boolean ;
    last_isexp  : Boolean ;
    is_real     : Boolean ;
    res         : Int64 ;

    function checkIsDate : Boolean ;
    const
      REG_DT_FRM = '^[0-9]{4}[-/.][0-9]{2}[-/.][0-9]{2}|[0-9]{2}[-/.][0-9]{2}[-/.][0-9]{4}$' ;
    var
      {$IFDEF OXYGENE}
        regex : Regex ;
      {$ELSE}
        regex : TRegEx ;
      {$ENDIF}
    begin
      {$IFDEF OXYGENE}
        regex := Regex.Create ( REG_DT_FRM ) ;
      {$ELSE}
        regex := TRegEx.Create( REG_DT_FRM ) ;
      {$ENDIF}
      Result := regex.IsMatch( _value ) ;
    end ;

  begin
    Result := TGIS_FieldType.String ;
    if IsStringEmpty( _value ) then exit ;

    if (CompareText( _value, 'true'  ) = 0) or
       (CompareText( _value, 'false' ) = 0) then
    begin
      Result := TGIS_FieldType.Boolean ;
      exit ;
    end ;

    fnd_mantisa := False ;
    last_isexp  := False ;
    fnd_exp     := False ;
    is_real     := False ;
    fnd_dot     := False ;
    fnd_zero    := False ;

    j := 0 ;
    clen := StringLast( _value ) ;
    w := StringFirst ;
    while (w <= clen) and {$IFDEF OXYGENE}Char.IsWhiteSpace(_value[w]){$ELSE}_value[w].IsWhiteSpace{$ENDIF} do
      inc( w ) ;

    for i := w to clen do begin
      c := _value[i] ;
      if {$IFDEF OXYGENE}Char.IsWhiteSpace(c){$ELSE}c.IsWhiteSpace{$ENDIF} then begin
        w := i ;
        while (w <= clen) and {$IFDEF OXYGENE}Char.IsWhiteSpace(_value[w]){$ELSE}_value[w].IsWhiteSpace{$ENDIF} do
          inc( w ) ;
        if w > clen then
          break
        else begin
          Result := TGIS_FieldType.String ;
          exit ;
        end;
      end
      else if {$IFDEF ISLAND}c.IsDigit{$ELSE}
              {$IFDEF OXYGENE}Char.IsDigit(c){$ELSE}c.IsDigit{$ENDIF}
              {$ENDIF} then
      begin
        fnd_mantisa := True ;
        last_isexp  := False ;
        if (i = StringFirst) and (c = '0') then
          fnd_zero := True ;
        if fnd_zero and (c <> '0') then begin
          Result := TGIS_FieldType.String ;
          exit ;
        end;
      end
      else if (c = '+') or (c = '-') then begin
        if i = StringFirst then continue
        else if last_isexp then
        else begin
          if checkIsDate then
            Result := TGIS_FieldType.Date
          else
            Result := TGIS_FieldType.String ;
          exit ;
        end;
        last_isexp := False ;
      end
      else if (c = '/') then begin
        if checkIsDate then
          Result := TGIS_FieldType.Date
        else
          Result := TGIS_FieldType.String ;
        exit ;
      end
      else if (c = '.') or (c = ',') then begin
        is_real := True ;
        if not fnd_dot and not last_isexp then
          fnd_dot := True
        else begin
          if checkIsDate then
            Result := TGIS_FieldType.Date
          else
            Result := TGIS_FieldType.String ;
          exit ;
        end ;
        last_isexp := False ;
      end
      else if (c = 'E') or (c = 'e') then begin
        if not fnd_mantisa then begin
          Result := TGIS_FieldType.String ;
          exit ;
        end ;
        if (i < clen) and
          not ((_value[i+1]='+') or (_value[i+1]='-') or
               {$IFDEF ISLAND}_value[i+1].IsDigit{$ELSE}
               {$IFDEF OXYGENE}Char.IsDigit(_value[i+1]){$ELSE}_value[i+1].IsDigit{$ENDIF}
               {$ENDIF}) then
        begin
          Result := TGIS_FieldType.String ;
          exit ;
        end ;
        is_real := True ;
        if not fnd_exp then
          fnd_exp := True
        else begin
          Result := TGIS_FieldType.String ;
          exit ;
        end ;
        j := i + 1 ;
        last_isexp := True ;
      end
      else begin
        Result := TGIS_FieldType.String ;
        exit ;
      end ;
    end ;

    if is_real and (j > 0) and ((clen - j) >= 3) then begin
      Result := TGIS_FieldType.String ;
      exit ;
    end ;

    if is_real then
      Result := TGIS_FieldType.Float
    else begin
      maxlen := StringLast( IntToStr( high(Int64) ) ) ;
      if clen > ( maxlen + 1 ) then
        Result := TGIS_FieldType.String
      else begin
        if clen in [maxlen, maxlen + 1] then begin
          if TryStrToInt64( _value, res ) then
            Result := TGIS_FieldType.Number
          else
            Result := TGIS_FieldType.String
        end
        else
          Result := TGIS_FieldType.Number ;
      end ;
    end ;
  end ;

  procedure DeleteDirectory( const _path : String ) ;
  begin
    try
      {$IFDEF DCC}
        TDirectory.Delete( _path, True ) ;
      {$ENDIF}
      {$IFDEF CLR}
        Directory.Delete( _path, True ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        Folder.Delete( new Folder(_path) );
      {$ENDIF}
    except

    end ;
  end ;

{$IFDEF DCC}
  initialization
    threadDir ;

  finalization
    FreeObject( FThreadStorageList ) ;
    FreeObject( othreadDir         ) ;
{$ENDIF}
//==================================== END =====================================
end.
