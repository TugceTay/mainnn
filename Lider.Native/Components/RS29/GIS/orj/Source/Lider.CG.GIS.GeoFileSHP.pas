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
  Internal part of SHP-file implementation /only SHP not DBF/.

  Visit www.esri.com to see SHP documentation.
}

{$IFDEF DCC}
  unit GisFileSHP ;
  {$HPPEMIT '#pragma link "GisFileSHP"'}
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

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    GisTypes,
    GisStreams,
    GisLayerVector ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
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
  ///   SHP-file record.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_RecordSHP = {$IFDEF OXYGENE} public {$ENDIF}
                   {$IFDEF GIS_PACKED} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    RecordNumber  : TGIS_Uid ;
    ContentLength : Int32 ;
    ShapeType     : TGIS_ShapeType ;
    ShapeTypeEx   : Int32 ;
    Dimension     : TGIS_DimensionType ;
    {$IFDEF MANAGED}
      Contents    : TGIS_Bytes ;
    {$ENDIF}
  end ;
  {$IFNDEF MANAGED}
    {#GENDOC:HIDE}
    PGIS_RecordSHP = ^TGIS_RecordSHP ;
  {$ENDIF}

  {#GENDOC:HIDE}
  TGIS_RecordSHX = {$IFDEF OXYGENE} public {$ENDIF}
                   {$IFDEF GIS_PACKED} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    Offset        : Int32 ;
    ContentLength : Int32 ;
  end ;
  {$IFNDEF MANAGED}
    {#GENDOC:HIDE}
    PGIS_RecordSHX = ^TGIS_RecordSHX ;
  {$ENDIF}

  /// <summary>
  ///   Internal SHP/SHX file header.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerSHP.
  ///    </note>
  /// </remarks>
  TGIS_FileSHPInternal = {$IFDEF OXYGENE} public abstract {$ENDIF}
                         class( TGIS_BufferedFileStream )

    private
    {$IFDEF OXYGENE} unit {$ENDIF}

      /// <summary>
      ///   Name of the file.
      /// </summary>
      filePath : String ;
      {$IFDEF MANAGED}
      bufInt : array [0..3] of Byte ;
      {$ENDIF}
    public

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        FileCode : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Unused1 : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Unused2 : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Unused3 : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Unused4 : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Unused5 : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        FileLength : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Version : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        ShapeType : Int32 ;

        /// <summary>
        ///   Header. See SHP-file documentation.
        /// </summary>
        Extent : TGIS_Extent3D ;

    protected

      /// <summary>
      ///   Read integer (4 bytes) from big endian stored bytes.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readIntegerBE  : Integer ;                  // Big Endian

      /// <summary>
      ///   Write integer (4 bytes) as a big endian.
      /// </summary>
      /// <param name="_value">
      ///   value to be written
      /// </param>
      procedure writeIntegerBE ( const _value : Integer ) ; // Big Endian

      /// <summary>
      ///   Read integer (4 bytes) from little endian stored bytes.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readIntegerLE  : Int32 ;                  // Little Endian

      /// <summary>
      ///   Write integer (4 bytes) as a little endian.
      /// </summary>
      /// <param name="_value">
      ///   value to be written
      /// </param>
      procedure writeIntegerLE ( const _value : Int32 ) ; // Little Endian

      /// <summary>
      ///   Read double (8 bytes) from big endian stored bytes.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readDoubleBE   : Double  ;                  // Big Endian

      /// <summary>
      ///   Write double (8 bytes) as a big endian.
      /// </summary>
      /// <param name="_value">
      ///   value to be written
      /// </param>
      procedure writeDoubleBE  ( const _value : Double ) ;  // Big Endian

      /// <summary>
      ///   Read double (8 bytes) from little endian stored bytes.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readDoubleLE   : Double  ;                  // Little Endian

      /// <summary>
      ///   Write double (8 bytes) as a little endian.
      /// </summary>
      /// <param name="_value">
      ///   value to be written
      /// </param>
      procedure writeDoubleLE  ( const _value : Double ) ;  // Little Endian

    public
      /// <summary>
      ///   Open new cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure  CursorOpen    ( const _cursor : Integer
                               ) ; virtual; abstract;
      /// <summary>
      ///   Close a cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      procedure  CursorClose   ( const _cursor : Integer
                               ) ; virtual; abstract;

      /// <summary>
      ///   Read file header.
      /// </summary>
      procedure  ReadHeader    ; virtual;

      /// <summary>
      ///   Write file header.
      /// </summary>
      procedure  WriteHeader   ;

      /// <summary>
      ///   Read extent value.
      /// </summary>
      /// <returns>
      ///    file extent
      /// </returns>
      function   ReadExtent    : TGIS_Extent3D ;

      /// <summary>
      ///   Write extent value.
      /// </summary>
      /// <param name="_value">
      ///   extent to be written
      /// </param>
      procedure  WriteExtent   ( const _value : TGIS_Extent3D ) ;

      /// <summary>
      ///   Convert shape type to integer value.
      /// </summary>
      /// <param name="_value">
      ///   converted value
      /// </param>
      /// <param name="_dim">
      ///   dimension
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function   ShapeTypeToInt( const _value : TGIS_ShapeType ;
                                 const _dim   : TGIS_DimensionType
                                ) : Integer ;

    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure doDestroy      ; override;

    public

      /// <summary>
      ///   Creates an instance on the file given by _path.
      /// </summary>
      /// <param name="_path">
      ///   file to open on
      /// </param>
      /// <param name="_mode">
      ///   file mode
      /// </param>
      /// <param name="_onread">
      ///   decoding event or nil
      /// </param>
      /// <param name="_onwrite">
      ///   encoding event or nil
      /// </param>
      constructor Create       ( const _path    : String              ;
                                 const _mode    : TGIS_StreamMode     ;
                                 const _onread  : TGIS_ReadWriteEvent ;
                                 const _onwrite : TGIS_ReadWriteEvent
                               ) ; reintroduce ; virtual;
  end ;

  {$IFDEF OXYGENE}
    T_cursorStateSHP nested in TGIS_FileSHP = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public

        curInUse : Boolean ;

        curRec : TGIS_RecordSHP ;

        curBuf : Array of Byte ;

        curBufSize : Integer ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of SHP-file low-level access.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerSHP.
  ///    </note>
  /// </remarks>
  TGIS_FileSHP = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_FileSHPInternal )

    private
      {$IFDEF OXYGENE}
        cursorState : array of T_cursorStateSHP ;
      {$ELSE}
        cursorState : array of record

           /// <summary>
           ///   Is cursor in use.
           /// </summary>
            curInUse : Boolean ;

           /// <summary>
           ///   Current shape file record.
           /// </summary>
            curRec : TGIS_RecordSHP ;

           /// <summary>
           ///   Buffer for shapes.
           /// </summary>
            curBuf : Array of Byte ;

           /// <summary>
           ///   Size of buffer for shapes.
           /// </summary>
            curBufSize : Integer ;
        end ;
      {$ENDIF}

    public

      /// <summary>
      ///   Allocate new cursor. Cursor should be closes by calling
      ///   CursorClose().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be opened
      /// </param>
      procedure CursorOpen   ( const _cursor : Integer
                             ) ; override;

      /// <summary>
      ///   Free cursor allocated by CursorOpen().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure CursorClose  ( const _cursor : Integer
                             ) ; override;

      /// <summary>
      ///   Read file header.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure ReadHeader   ( const _cursor : Integer
                             ) ; reintroduce;
      {$IFDEF CLR}

        /// <summary>
        ///   Read record (in the meaning of a shape file) value.
        /// </summary>
        /// <param name="_result">
        ///   retrieved record
        /// </param>
        /// <param name="_cursor">
        ///   cursor allocated by CursorOpen()
        /// </param>
        procedure ReadRecord (   var _result : TGIS_RecordSHP ;
                               const _cursor : Integer
                             ) ;
      {$ELSE}

        /// <summary>
        ///   Read record (in the meaning of a shape file) value.
        /// </summary>
        /// <param name="_cursor">
        ///   cursor allocated by CursorOpen()
        /// </param>
        /// <returns>
        ///   retrieved record
        /// </returns>
        function  ReadRecord ( const _cursor : Integer
                             ) : {$IFDEF JAVA OR ISLAND} TGIS_RecordSHP {$ELSE} PGIS_RecordSHP {$ENDIF} ;
      {$ENDIF}

      /// <summary>
      ///   Write record (in the meaning of a shape file) value.
      /// </summary>
      /// <param name="_value">
      ///   record to be written
      /// </param>
      procedure WriteRecord  ( const _value  : TGIS_RecordSHP
                             ) ;

      {$IFDEF MANAGED}

        /// <summary>
        ///   Read a current position in SHP file.
        /// </summary>
        /// <param name="_cursor">
        ///   cursor allocated by CursorOpen()
        /// </param>
        /// <returns>
        ///   current position as the pointer to memory
        /// </returns>
        /// <exception cref="EGIS_Exception">
        ///   GIS_RS_ERR_FILEBADFORMAT
        /// </exception>
        function  ReadOffset ( const _cursor : Integer
                             ) : Integer ;
      {$ELSE}

        /// <summary>
        ///   Read a current position in SHP file.
        /// </summary>
        /// <param name="_cursor">
        ///   cursor allocated by CursorOpen()
        /// </param>
        /// <returns>
        ///   current position as the pointer to memory
        /// </returns>
        /// <exception cref="EGIS_Exception">
        ///   GIS_RS_ERR_FILEBADFORMAT
        /// </exception>
        function  ReadOffset ( const _cursor : Integer
                             ) : Pointer ;
      {$ENDIF}

      /// <summary>
      ///   Write geometry.
      /// </summary>
      /// <param name="_shp">
      ///   Shape for which geometry will be written
      /// </param>
      procedure WriteGeometry( const _shp    : TGIS_Shape
                             ) ;

    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure doDestroy    ; override;
  end ;

  {$IFDEF OXYGENE}
    T_cursorStateSHX nested in TGIS_FileSHX = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public

        curInUse : Boolean ;

        curRec : TGIS_RecordSHX ;

        curPos : TGIS_Uid ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of SHX-file (SHP index) low-level access.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerSHP.
  ///    </note>
  /// </remarks>
  TGIS_FileSHX = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_FileSHPInternal )

    private
      {$IFDEF OXYGENE}
        cursorState : array of T_cursorStateSHX ;
      {$ELSE}
        cursorState : array of record

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse : Boolean ;

        /// <summary>
        ///   Current index file record.
        /// </summary>
        curRec : TGIS_RecordSHX ;

        /// <summary>
        ///   Start position (just after header ).
        /// </summary>
            curPos : Integer ;
        end ;
      {$ENDIF}

    private // property internal values

        /// <summary>
        ///   Number of records.
        /// </summary>
        FRecordsCount : Integer ;

    public

      /// <summary>
      ///   Allocate new cursor. Cursor should be closes by calling
      ///   CursorClose().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be opened
      /// </param>
      procedure CursorOpen  ( const _cursor : Integer
                            ) ; override;

      /// <summary>
      ///   Free cursor allocated by CursorOpen().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure CursorClose ( const _cursor : Integer
                            ) ; override;

      /// <summary>
      ///   Read file header.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure ReadHeader  ( const _cursor : Integer
                            ) ; reintroduce ;
      {$IFDEF MANAGED}

        /// <summary>
        ///   Read record given by Uid number.
        /// </summary>
        /// <param name="_uid">
        ///   uid to be retrieved
        /// </param>
        /// <param name="_cursor">
        ///   cursor allocated by CursorOpen()
        /// </param>
        /// <returns>
        ///   retrieved record
        /// </returns>
        function  ReadRecord( const _uid    : TGIS_Uid ;
                              const _cursor : Integer
                            ) : TGIS_RecordSHX ;
      {$ELSE}

        /// <summary>
        ///   Read record given by Uid number.
        /// </summary>
        /// <param name="_uid">
        ///   uid to be retrieved
        /// </param>
        /// <param name="_cursor">
        ///   cursor allocated by CursorOpen()
        /// </param>
        /// <returns>
        ///   retrieved record
        /// </returns>
        function  ReadRecord( const _uid    : TGIS_Uid ;
                              const _cursor : Integer
                            ) : PGIS_RecordSHX ;
      {$ENDIF}

      /// <summary>
      ///   Write record (in the meaning of a shape file) value.
      /// </summary>
      /// <param name="_offset">
      ///   file offset
      /// </param>
      /// <param name="_value">
      ///   record to be written
      /// </param>
      procedure WriteRecord ( const _offset : Integer ;
                              const _value  : TGIS_RecordSHP
                            ) ;
    public

        /// <summary>
        ///   Number of records.
        /// </summary>
        property RecordsCount : Integer read FRecordsCount ;
  end ;

//##############################################################################

implementation

{$IFDEF DCC}
  uses
    System.Math,
    GisClasses,
    GisResource ;
{$ENDIF}

const
  SHP_BUFFERSIZE_STEP = 65536 ;
  SHP_BUFFERSIZE_MAX  = 65536 * 4 * 512 ;

const
  SIZEOF_TGIS_RecordSHX = 2 * sizeOf( Int32 ) ;

//==============================================================================
// TGIS_FileSHPInternal
//==============================================================================

  constructor TGIS_FileSHPInternal.Create(
    const _path    : String              ;
    const _mode    : TGIS_StreamMode     ;
    const _onread  : TGIS_ReadWriteEvent ;
    const _onwrite : TGIS_ReadWriteEvent
  ) ;
  begin
    filePath := _path ;
    inherited Create( _path, _mode, _onread, _onwrite );
  end ;

  procedure TGIS_FileSHPInternal.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_FileSHPInternal.readIntegerBE : Integer ;
  {$IFDEF MANAGED}
    begin
      ReadBuffer( bufInt,  4 ) ;

      {$IFDEF JAVA}
        Result := Integer((Integer(bufInt[0]) shl 24) or
                          (Integer($ff and bufInt[1]) shl 16) or
                          (Integer($ff and bufInt[2]) shl 8) or
                          (Integer($ff and bufInt[3]) ))
      {$ELSE}
        Result := Integer( ( bufInt[3]        ) or
                           ( bufInt[2] shl  8 ) or
                           ( bufInt[1] shl 16 ) or
                           ( bufInt[0] shl 24 )
                         ) ;
      {$ENDIF}
    end ;
  {$ELSE}
    var
      p : PInteger ;
      b : Byte ;
    begin
      p := @Result ;
      Read( p^, 4 ) ;

      b := PByte(NativeInt(p) + 0)^ ;
      PByte(NativeInt(p) + 0)^ := PByte(NativeInt(p) + 3)^ ;
      PByte(NativeInt(p) + 3)^ := b ;

      b := PByte(NativeInt(p) + 1)^ ;
      PByte(NativeInt(p) + 1)^ := PByte(NativeInt(p) + 2)^ ;
      PByte(NativeInt(p) + 2)^ := b ;

    end ;
  {$ENDIF}

  procedure TGIS_FileSHPInternal.writeIntegerBE( const _value : Integer ) ;
  {$IFDEF MANAGED}
    var
      buf: array[0..3] of Byte;
    begin
      buf[3] := _value and $FF;
      buf[2] := (_value shr 8) and $FF;
      buf[1] := (_value shr 16) and $FF;
      buf[0] := (_value shr 24) and $FF;

      WriteBuffer( buf, 4 ) ;
    end ;
  {$ELSE}
    var
      p : PInteger ;
      b : Byte ;
    begin
      p := @_value ;

      b := PByte(NativeInt(p) + 0)^ ;
      PByte(NativeInt(p) + 0)^ := PByte(NativeInt(p) + 3)^ ;
      PByte(NativeInt(p) + 3)^ := b ;

      b := PByte(NativeInt(p) + 1)^ ;
      PByte(NativeInt(p) + 1)^ := PByte(NativeInt(p) + 2)^ ;
      PByte(NativeInt(p) + 2)^ := b ;

      WriteBuffer( p^, 4 ) ;
    end ;
  {$ENDIF}

  function TGIS_FileSHPInternal.readIntegerLE : Int32 ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := ReadInteger ;
      {$ELSE}
        ReadInteger( Result ) ;
      {$ENDIF}
    {$ELSE}
      Read( Result, sizeOf(Result) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_FileSHPInternal.writeIntegerLE( const _value : Int32 ) ;
  begin
    {$IFDEF OXYGENE}
      WriteInteger( _value ) ;
    {$ELSE}
      WriteBuffer( _value, sizeOf(_value) ) ;
    {$ENDIF}
  end ;

  function TGIS_FileSHPInternal.readDoubleBE : Double ;
  var
    {$IFDEF MANAGED}
      b : Byte    ;
    {$ENDIF}
      i : Integer ;
    a : Array[0..7] of Byte ;
  begin
    {$IFDEF MANAGED}
      assert( RemObjects.Elements.System.length( a ) = 8 ) ;
      assert( sizeOf(Double) = 8 ) ;

      ReadBuffer( a, 8 ) ;
      for i := 0 to 3 do begin
        b      := a[i  ] ;
        a[i  ] := a[7-i] ;
        a[7-i] := b      ;
      end ;

      Result := BitConverter.ToDouble(a, 0);
    {$ELSE}
      assert( sizeOf(a     ) = 8 ) ;
      assert( sizeOf(Double) = 8 ) ;

      Read( a, 8 ) ;
      for i:=0 to 7 do // swap bytes
        PByte( NativeInt( @Result ) + i)^ := a[7-i] ;
    {$ENDIF}
  end ;

  procedure TGIS_FileSHPInternal.writeDoubleBE( const _value : Double ) ;
  var
    a   : Array[0..7] of Byte ;
    {$IFDEF MANAGED}
      b : Byte    ;
    {$ENDIF}
      i : Integer ;
  begin
    {$IFDEF MANAGED}
      assert( RemObjects.Elements.System.length( a ) = 8 ) ;
      assert( sizeOf(Double) = 8 ) ;

      a := BitConverter.GetBytes( _value ) ;
      for i:=0 to 3 do begin // swap bytes
        b      := a[i]   ;
        a[i]   := a[7-i] ;
        a[7-i] := b      ;
      end ;
      WriteBuffer( a, 8 );
    {$ELSE}
      assert( sizeOf(a     ) = 8 ) ;
      assert( sizeOf(Double) = 8 ) ;

      for i:=7 downto 0 do // swap bytes
        a[i] := PByte( NativeInt( @_value ) + i)^  ;
      Write( a, 8 );
    {$ENDIF}
  end ;

  function TGIS_FileSHPInternal.readDoubleLE : Double ;
  begin
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        Result := ReadDouble ;
      {$ELSE}
        ReadDouble( Result ) ;
      {$ENDIF}
    {$ELSE}
      Read( Result, sizeOf(Result) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_FileSHPInternal.writeDoubleLE( const _value : Double ) ;
  begin
    {$IFDEF OXYGENE}
      WriteDouble( _value ) ;
    {$ELSE}
      WriteBuffer( _value, sizeOf(_value) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_FileSHPInternal.ReadHeader ;
  begin
    Position := 0 ;
    FileCode   := readIntegerBE ;     // Byte 00
    Unused1    := readIntegerBE ;     // Byte 04-20
    Unused2    := readIntegerBE ;     // Byte 08
    Unused3    := readIntegerBE ;     // Byte 12
    Unused4    := readIntegerBE ;     // Byte 16
    Unused5    := readIntegerBE ;     // Byte 20
    FileLength := readIntegerBE * 2 ; // Byte 24
    Version    := readIntegerLE ;     // Byte 28
    ShapeType  := readIntegerLE ;     // Byte 32
    Extent     := ReadExtent    ;     // Byte 36
  end ;

  procedure TGIS_FileSHPInternal.WriteHeader ;
  begin
    Position := 0 ;
    writeIntegerBE( $270A            ) ;     // Byte 00
    writeIntegerBE( Unused1          ) ;     // Byte 04-20
    writeIntegerBE( Unused2          ) ;     // Byte 08
    writeIntegerBE( Unused3          ) ;     // Byte 12
    writeIntegerBE( Unused4          ) ;     // Byte 16
    writeIntegerBE( Unused5          ) ;     // Byte 20
    writeIntegerBE( Size div 2       ) ;     // Byte 24
    writeIntegerLE( $03E8            ) ;     // Byte 28
    writeIntegerLE( ShapeType        ) ;     // Byte 32
    WriteExtent   ( Extent           ) ;     // Byte 36
  end ;

  function TGIS_FileSHPInternal.ReadExtent : TGIS_Extent3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent3D ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.XMin := readDoubleLE ;
    {$ELSE}
      Read( Result.XMin, sizeOf(Result.XMin) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.YMin := readDoubleLE ;
    {$ELSE}
      Read( Result.YMin, sizeOf(Result.YMin) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.XMax := readDoubleLE ;
    {$ELSE}
      Read( Result.XMax, sizeOf(Result.XMax) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.YMax := readDoubleLE ;
    {$ELSE}
      Read( Result.YMax, sizeOf(Result.YMax) ) ;
    {$ENDIF}

    {$IFDEF OXYGENE}
      Result.ZMin := readDoubleLE ;
    {$ELSE}
      Read( Result.ZMin, sizeOf(Result.ZMin) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.ZMax := readDoubleLE ;
    {$ELSE}
      Read( Result.ZMax, sizeOf(Result.ZMax) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.MMin :=- readDoubleLE ;
    {$ELSE}
      Read( Result.MMin, sizeOf(Result.MMin) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result.MMax := readDoubleLE ;
    {$ELSE}
      Read( Result.MMax, sizeOf(Result.MMax) ) ;
    {$ENDIF}

    if IsNan( Result.ZMin ) then
      Result.ZMin := 0 ;
    if IsNan( Result.ZMax ) then
      Result.ZMax := 0 ;
    if IsNan( Result.MMin ) then
      Result.MMin := 0 ;
    if IsNan( Result.MMax ) then
      Result.MMax := 0 ;
  end ;

  procedure TGIS_FileSHPInternal.WriteExtent( const _value : TGIS_Extent3D ) ;
  var
    val : TGIS_Extent3D ;
  begin
    {$IFDEF GIS_NORECORDS}
      if not assigned( _value ) then
        val := new TGIS_Extent3D
      else
        val := _value ;
    {$ELSE}
      val := _value ;
    {$ENDIF}
    writeDoubleLE( val.XMin ) ;
    writeDoubleLE( val.YMin ) ;
    writeDoubleLE( val.XMax ) ;
    writeDoubleLE( val.YMax ) ;
    writeDoubleLE( val.ZMin ) ;
    writeDoubleLE( val.ZMax ) ;
    writeDoubleLE( val.MMin ) ;
    writeDoubleLE( val.MMax ) ;
  end ;

  function TGIS_FileSHPInternal.ShapeTypeToInt( const _value : TGIS_ShapeType ;
                                                const _dim   : TGIS_DimensionType
                                              ) : Integer ;
  begin
    case _value of
       TGIS_ShapeType.Point : case _dim of
                                TGIS_DimensionType.XY   : Result := 1  ;
                                TGIS_DimensionType.XYZ  : Result := 11 ;
                                TGIS_DimensionType.XYZM : Result := 11 ;
                                TGIS_DimensionType.XYM  : Result := 21
                                else                      Result := 1  ;
                              end;
       TGIS_ShapeType.Arc   : case _dim of
                                TGIS_DimensionType.XY   : Result := 3  ;
                                TGIS_DimensionType.XYZM : Result := 13 ;
                                TGIS_DimensionType.XYZ  : Result := 13 ;
                                TGIS_DimensionType.XYM  : Result := 23
                                else                      Result := 3  ;
                              end;
       TGIS_ShapeType.Polygon
                            : case _dim of
                                TGIS_DimensionType.XY   : Result := 5  ;
                                TGIS_DimensionType.XYZM : Result := 15 ;
                                TGIS_DimensionType.XYZ  : Result := 15 ;
                                TGIS_DimensionType.XYM  : Result := 25
                                else                      Result := 5  ;
                              end;
       TGIS_ShapeType.MultiPoint
                            : case _dim of
                                TGIS_DimensionType.XY   : Result := 8  ;
                                TGIS_DimensionType.XYZ  : Result := 18 ;
                                TGIS_DimensionType.XYZM : Result := 18 ;
                                TGIS_DimensionType.XYM  : Result := 28
                                else                      Result := 8  ;
                              end;
       TGIS_ShapeType.MultiPatch
                            : case _dim of
                                TGIS_DimensionType.XY   : Result := 31  ;
                                TGIS_DimensionType.XYZ  : Result := 31 ;
                                TGIS_DimensionType.XYZM : Result := 31 ;
                                TGIS_DimensionType.XYM  : Result := 31
                                else                      Result := 31  ;
                              end;
    else
      Result := 1 ;
    end ;
  end ;

//==============================================================================
// TGIS_FileSHP
//==============================================================================

  procedure TGIS_FileSHP.doDestroy ;
  var
    i : Integer ;
  begin
    for i:= 0 to high( cursorState ) do begin
      {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState[i].curBuf, 0 ) ;
    end ;

    inherited
  end ;

  procedure TGIS_FileSHP.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( cursorState )  then
      {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, _cursor + 1 ) ;

    {$IFDEF GIS_NORECORDS}
      if not assigned( cursorState[_cursor] ) then
        cursorState[_cursor] := new T_cursorStateSHP ;
      cursorState[ _cursor ].curRec := new TGIS_RecordSHP ;
    {$ENDIF}
    cursorState[ _cursor ].curInUse := True ;
    ReadHeader( _cursor );
  end;

  procedure TGIS_FileSHP.CursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorState[_cursor].curInUse := False ;

    // truncate cursorState at the tail;
    {$IFNDEF OXYGENE}
      for i := length( cursorState ) - 1 downto 0 do begin
    {$ELSE}
      for i := cursorState.Length - 1 downto 0 do begin
    {$ENDIF}
      if not cursorState[i].curInUse then begin
        {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, i ) ;
      end
      else
        break ;
    end ;
  end;

  procedure TGIS_FileSHP.ReadHeader(
    const _cursor : Integer
  ) ;
  begin
    inherited ReadHeader ;
  end ;

  {$IFDEF CLR}
    procedure TGIS_FileSHP.ReadRecord (
      var _result : TGIS_RecordSHP ;
      const _cursor : Integer
    ) ;
  {$ELSE}

    function TGIS_FileSHP.ReadRecord(
      const _cursor : Integer
    ) : {$IFDEF JAVA OR ISLAND} TGIS_RecordSHP {$ELSE} PGIS_RecordSHP {$ENDIF} ;
  {$ENDIF}
  {$IFDEF OXYGENE}
  var
    len : Integer ;
  {$ENDIF}
  begin

    cursorState[_cursor].curRec.RecordNumber  := readIntegerBE ;
    cursorState[_cursor].curRec.ContentLength := readIntegerBE ;
    cursorState[_cursor].curRec.ShapeTypeEx := readIntegerLE ;
    case cursorState[_cursor].curRec.ShapeTypeEx of
      1  :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Point       ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XY      ;
            end;
      11 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Point       ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYZM    ;
            end;
      21 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Point       ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYM     ;
            end;
      3  :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Arc         ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XY      ;
            end;
      13 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Arc         ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYZM    ;
            end;
      23 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Arc         ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYM     ;
            end;
      5  :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Polygon     ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XY      ;
            end;
      15 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Polygon     ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYZM    ;
            end;
      25 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Polygon     ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYM     ;
            end;
      8  :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.MultiPoint  ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XY      ;
            end;
      18 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.MultiPoint  ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYZM    ;
            end;
      28 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.MultiPoint  ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYM     ;
            end;
      31 :  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.MultiPatch  ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.XYZM    ;
            end;
      else  begin
              cursorState[_cursor].curRec.ShapeType := TGIS_ShapeType.Unknown     ;
              cursorState[_cursor].curRec.Dimension := TGIS_DimensionType.Unknown ;
            end;
    end ;

    {$IFDEF OXYGENE}
      len := ReadOffset( _cursor );
      if not assigned(cursorState[_cursor].curRec.Contents) then
        cursorState[_cursor].curRec.Contents
          := TGIS_Bytes.Create( cursorState[_cursor].curBuf, 0 )
      else
        cursorState[_cursor].curRec.Contents.Recreate( cursorState[_cursor].curBuf, 0, len ) ;
      {$IFDEF JAVA OR ISLAND}
        Result := cursorState[_cursor].curRec ;
      {$ELSE}
        _result := cursorState[_cursor].curRec ;
      {$ENDIF}
    {$ELSE}
      Result := @cursorState[_cursor].curRec ;
    {$ENDIF}
  end ;

  procedure TGIS_FileSHP.WriteRecord( const _value : TGIS_RecordSHP ) ;
  begin
    writeIntegerBE( _value.RecordNumber  ) ;
    writeIntegerBE( _value.ContentLength ) ;
    writeIntegerLE( ShapeTypeToInt( _value.ShapeType, _value.Dimension ) ) ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_FileSHP.ReadOffset(
      const _cursor : Integer
    ) : Integer ;
  {$ELSE}

    function TGIS_FileSHP.ReadOffset(
      const _cursor : Integer
    ) :  Pointer ;
  {$ENDIF}
  var
    len : Integer ;
  begin
    if cursorState[_cursor].curRec.ContentLength < 0 then
       len := 0
    else
      len := cursorState[_cursor].curRec.ContentLength * 2 - 4 ;

    if ( len >= 0 ) and ( len < SHP_BUFFERSIZE_MAX ) then begin
      if ( len > cursorState[_cursor].curBufSize ) then begin
        cursorState[_cursor].curBufSize
           := ( len div SHP_BUFFERSIZE_STEP + 1 ) * SHP_BUFFERSIZE_STEP ;
        {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState[_cursor].curBuf,
                                                       cursorState[_cursor].curBufSize
                                                     ) ;
      end ;
    end
    else begin
      cursorState[_cursor].curBufSize := 0 ;
      cursorState[_cursor].curBuf := nil ;
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), filePath, 65536 ) ;
    end ;

    if len > 0 then begin
      {$IFDEF OXYGENE}
        Read( cursorState[_cursor].curBuf, len ) ;
        Result := len;
      {$ELSE}
        Read( cursorState[_cursor].curBuf[0], len ) ;
        Result := @cursorState[_cursor].curBuf[0];
      {$ENDIF}
    end
    else
      {$IFDEF OXYGENE}
        Result := 0 ;
      {$ELSE}
        Result := nil ;
      {$ENDIF}
  end ;

  procedure TGIS_FileSHP.WriteGeometry( const _shp : TGIS_Shape ) ;
  var
    val   : Double ;

    procedure write_fakebuf( const _size : Integer ) ;
    var
      buf : TBytes ;
      {$IFDEF OXYGENE}
        i : Integer ;
      {$ENDIF}
    begin
      if _size > 0 then begin
        {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( buf, _size ) ;
        {$IFDEF OXYGENE}
          for i := 0 to _size - 1 do
            buf[ i ] := 0 ;
        {$ELSE}
          FillChar( buf[0], _size, 0 ) ;
        {$ENDIF}
        {$IFNDEF OXYGENE}
          WriteBuffer( buf[0], _size ) ;
        {$ELSE}
          WriteBuffer( buf, _size ) ;
        {$ENDIF}
        buf := nil ;
      end;
    end ;

  begin
    {$IFDEF MANAGED}
      WriteBuffer( _shp.Parts,  _shp.PartsSize  ) ;
      if _shp.ShapeType = TGIS_ShapeType.MultiPatch then
        WriteBuffer( _shp.PartTypes, _shp.PartTypesSize ) ;
      WriteBuffer( _shp.Points, _shp.PointsSize ) ;
    {$ELSE}
      WriteBuffer( _shp.Parts^, _shp.PartsSize ) ;
      if _shp.ShapeType = TGIS_ShapeType.MultiPatch then
        WriteBuffer( _shp.PartTypes^, _shp.PartTypesSize ) ;
      WriteBuffer( _shp.Points^, _shp.PointsSize ) ;
    {$ENDIF}

    if (_shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ]) or
       (_shp.ShapeType = TGIS_ShapeType.MultiPatch) then begin
      if _shp.ShapeType = TGIS_ShapeType.Point then begin
        val := _shp.GetPoint3D( 0, 0 ).Z ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
      end
      else begin
        val := _shp.PointsZMin ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
        val := _shp.PointsZMax ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          WriteBuffer( _shp.PointsZ, _shp.PointsZSize ) ;
        {$ELSE}
          WriteBuffer( _shp.PointsZ^, _shp.PointsZSize ) ;
        {$ENDIF}
      end;
    end ;

    if (_shp.Dimension in [ TGIS_DimensionType.XYM, TGIS_DimensionType.XYZM ]) or
       (_shp.ShapeType = TGIS_ShapeType.MultiPatch) then begin
      if _shp.ShapeType = TGIS_ShapeType.Point then begin
        val := _shp.GetPoint3D( 0, 0 ).M ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
      end
      else begin
        val := _shp.PointsMMin ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
        val := _shp.PointsMMax ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
        {$IFDEF OXYGENE}
          WriteBuffer( _shp.PointsM, _shp.PointsMSize ) ;
        {$ELSE}
          WriteBuffer( _shp.PointsM^, _shp.PointsMSize ) ;
        {$ENDIF}
      end ;
    end
    else if ( _shp.Dimension in [ TGIS_DimensionType.XYZ, TGIS_DimensionType.XYZM ] ) then
    begin
      // SHP 3D is always ZM or Z so we must save M if only Z has been stored
      if _shp.ShapeType = TGIS_ShapeType.Point then begin
        val := 0 ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
      end
      else begin
        val := 0 ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
        val := 0 ;
        {$IFNDEF OXYGENE}
          WriteBuffer( val, sizeOf ( Double ) ) ;
        {$ELSE}
          WriteDouble( val ) ;
        {$ENDIF}
        write_fakebuf( _shp.PointsZSize  ) ;
      end ;
    end;

  end ;

//==============================================================================
// TGIS_FileSHX
//==============================================================================

  procedure TGIS_FileSHX.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( cursorState )  then
      {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, _cursor + 1 ) ;

    {$IFDEF GIS_NORECORDS}
      if not assigned( cursorState[_cursor] ) then
        cursorState[_cursor] := new T_cursorStateSHX ;
      cursorState[ _cursor ].curRec := new TGIS_RecordSHX ;
    {$ENDIF}
    cursorState[ _cursor ].curInUse := True ;
    ReadHeader( _cursor );
  end;

  procedure TGIS_FileSHX.CursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorState[_cursor].curInUse := False ;

    // truncate cursorState at the tail;
    {$IFNDEF OXYGENE}
      for i := length( cursorState ) - 1 downto 0 do begin
    {$ELSE}
      for i := cursorState.Length - 1 downto 0 do begin
    {$ENDIF}
      if not cursorState[i].curInUse then begin
        {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, i ) ;
      end
      else
        break ;
    end ;
  end;

  procedure TGIS_FileSHX.ReadHeader(
    const _cursor : Integer
  ) ;
  begin
    inherited ReadHeader ;
    cursorState[_cursor].curPos := Position ;
    FRecordsCount
      := ( Size - cursorState[_cursor].curPos) div SIZEOF_TGIS_RecordSHX ;
  end ;

  {$IFDEF MANAGED}

    function TGIS_FileSHX.ReadRecord(
      const _uid    : TGIS_Uid ;
      const _cursor : Integer
    ) : TGIS_RecordSHX ;
  {$ELSE}

    function TGIS_FileSHX.ReadRecord(
      const _uid    : TGIS_Uid ;
      const _cursor : Integer
    ) : PGIS_RecordSHX ;
  {$ENDIF}
  begin
    assert( _uid > 0 ) ;
    Position := cursorState[_cursor].curPos + (_uid-1) * SIZEOF_TGIS_RecordSHX ;
    cursorState[_cursor].curRec.Offset        := readIntegerBE * 2 ;
    cursorState[_cursor].curRec.ContentLength := readIntegerBE     ;

    {$IFDEF MANAGED}
      Result := cursorState[_cursor].curRec ;
    {$ELSE}
      Result := @cursorState[_cursor].curRec ;
    {$ENDIF}
  end ;

  procedure TGIS_FileSHX.WriteRecord( const _offset : Integer ;
                                      const _value : TGIS_RecordSHP ) ;
  begin
    writeIntegerBE( _offset div 2        ) ;
    writeIntegerBE( _value.ContentLength ) ;
  end ;

//==================================== END =====================================
end.
