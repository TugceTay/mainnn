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
  Encapsulation of Google Protocol Buffers serializer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileGPB ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileGPB"'}
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

{$IFDEF DCC}
  uses
    System.SysUtils ;
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
  {#GENDOC:HIDE}
  int8_t   = ShortInt ;
  {#GENDOC:HIDE}
  int32_t  = Int32 ;
  {#GENDOC:HIDE}
  int64_t  = Int64 ;
  {#GENDOC:HIDE}
  uint8_t  = Byte ;
  {#GENDOC:HIDE}
  uint32_t = UInt32 ;
  {#GENDOC:HIDE}
  uint64_t = UInt64 ;

  {$EXTERNALSYM size_t}
  {#GENDOC:HIDE}
  size_t   = UInt32 ;


  {#GENDOC:HIDE}
  TGIS_VarInt = record
    public
      function zigzag8              ( const _val      : int8_t
                                     ) : uint8_t ;
      function zigzag32             ( const _val      : int32_t
                                     ) : uint32_t ;
      function zigzag64             ( const _val      : int64_t
                                     ) : uint64_t ;
      function unzigzag8            ( const _val      : uint8_t
                                     ) : int8_t ;
      function unzigzag32           ( const _val      : uint32_t
                                     ) : int32_t ;
      function unzigzag64           ( const _val      : uint64_t
                                     ) : int64_t;
      function varint_s64_decode    ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t ;
                                        var _size     : size_t
                                     ) : int64_t ;
      function varint_u64_decode    ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t ;
                                        var _size     : size_t
                                     ) : uint64_t ;
      function varint_64_decode     ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t ;
                                        var _size     : size_t
                                     ) : int64_t ;
      function varint_s32_decode    ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t ;
                                        var _size     : size_t
                                     ) : int32_t ;
      function varint_32_decode     ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t ;
                                        var _size     : size_t
                                     ) : int32_t ;
      function varint_u32_decode    ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t ;
                                        var _size     : size_t
                                     ) : uint32_t ;
      function varint_u32_encode_buf( const _val      : uint32_t ;
                                      const _buf      : TBytes
                                     ) : size_t ;
      function varint_s32_encode_buf( const _val      : int32_t ;
                                      const _buf      : TBytes
                                     ) : size_t ;
      function varint_u64_encode_buf( const _val      : uint64_t ;
                                      const _buf      : TBytes
                                     ) : size_t ;
      function varint_s64_encode_buf( const _val      : int64_t ;
                                      const _buf      : TBytes
                                     ) : size_t ;
      function varint_size          ( const _buf      : TBytes ;
                                      const _start    : uint32_t ;
                                      const _end      : uint32_t
                                     ) : size_t ;
  end ;

  /// <summary>
  ///   Variant integer data reader.
  /// </summary>
  TGIS_VarIntReader = class
    {$IFDEF OXYGENE} private {$ELSE} private {$ENDIF}
      vi            : TGIS_VarInt ;
      pos           : int64_t ;
      buf           : TBytes ;
      len           : int64_t ;
    public
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readByte      : uint8_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readVarInt32  : int32_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readVarSInt32 : int32_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readVarUInt32 : uint32_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readVarInt64  : int64_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readVarSInt64 : int64_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  readVarUInt64 : uint64_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read data of type.
      /// </summary>
      /// <param name="_factor">
      ///   factor to multiply value
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function  readDouble    ( const _factor : Double
                               ) : Double ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Skip value.
      /// </summary>
      procedure skipVarInt64  ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Skip bytes.
      /// </summary>
      /// <param name="_len">
      ///   number of bytes to skip
      /// </param>
      procedure skipBytes     ( const _len    : uint32_t
                               ) ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read buffer.
      /// </summary>
      /// <param name="_buf">
      ///   buffer
      /// </param>
      /// <param name="_offset">
      ///   offset in buffer
      /// </param>
      /// <param name="_len">
      ///   number of bytes to read
      /// </param>
      procedure readBuffer    ( const _buf    : TBytes ;
                                const _offset : uint32_t ;
                                const _len    : uint32_t
                               ) ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Init buffer.
      /// </summary>
      /// <param name="_buf">
      ///   buffer
      /// </param>
      /// <param name="_len">
      ///   size of buffer
      /// </param>
      procedure initBuffer    ( const _buf    : TBytes ;
                                const _len    : uint32_t
                               ) ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   unzigzag byte
      /// </summary>
      /// <param name="_val">
      ///   value
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function unzigzag8      ( const _val    : uint8_t
                               ) : int8_t ;
                                {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
    public
      /// <summary>
      ///   Current position.
      /// </summary>
      property Position : int64_t read pos write pos ;
      /// <summary>
      ///   Current size.
      /// </summary>
      property Size     : int64_t read len ;
  end ;

  /// <summary>
  ///   OSM GPB format reader.
  /// </summary>
  TGIS_GPBReader = class( TGIS_VarIntReader )
    public
      const
        {#GENDOC:HIDE}
        WT_VARINT      = 0 ;
        {#GENDOC:HIDE}
        WT_64BIT       = 1 ;
        {#GENDOC:HIDE}
        WT_DATA        = 2 ;
        {#GENDOC:HIDE}
        WT_STARTGROUP  = 3 ;
        {#GENDOC:HIDE}
        WT_ENDGROUP    = 4 ;
        {#GENDOC:HIDE}
        WT_32BIT       = 5 ;
    public
      /// <summary>
      ///   Create new reader.
      /// </summary>
      /// <param name="_buf">
      ///   buffer
      /// </param>
      /// <param name="_len">
      ///   size of buffer
      /// </param>
      constructor Create          ( const _buf         : TBytes ;
                                    const _len         : uint32_t
                                   ) ;
      /// <summary>
      ///   Create new reader.
      /// </summary>
      /// <param name="_fieldNumber">
      ///   field index
      /// </param>
      /// <param name="_wireType">
      ///   wire type
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function  MakeKey           ( const _fieldNumber : int32_t ;
                                    const _wireType    : int32_t
                                   ) : Integer ;
                                   {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Get wire type.
      /// </summary>
      /// <param name="_key">
      ///   key value
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function  GetWireType       ( const _key         : int32_t
                                   ) : Integer ;
                                   {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Get field index.
      /// </summary>
      /// <param name="_key">
      ///   key value
      /// </param>
      /// <returns>
      ///    value
      /// </returns>
      function  GetFieldNumber    ( const _key         : int32_t
                                   ) : int32_t ;
                                   {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      /// <summary>
      ///   Read size.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  ReadSize          : uint32_t ;

      /// <summary>
      ///   Read field key.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  ReadFieldKey      : int32_t ;

      /// <summary>
      ///   Read text.
      /// </summary>
      /// <returns>
      ///    value
      /// </returns>
      function  ReadText          : String ;

      /// <summary>
      ///   Skip unknown field.
      /// </summary>
      /// <param name="_key">
      ///   key value
      /// </param>
      /// <param name="_verbose">
      ///   force skip
      /// </param>
      /// <returns>
      ///    Number of skipped bytes
      /// </returns>
      function  SkipUnknownField  ( const _key         : int32_t ;
                                    const _verbose     : Boolean
                                   ) : int32_t ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoInternals ;
{$ENDIF}

//=============================================================================
// TGIS_VarInt
//=============================================================================

  function TGIS_VarInt.unzigzag32(
    const _val : uint32_t
  ) : int32_t ;
  begin
    if (_val and $01) <> 0 then
      Result := -1 * int32_t((_val + 1) shr 1)
    else
      Result := int32_t(_val shr 1) ;
  end ;

  function TGIS_VarInt.unzigzag64(
    const _val : uint64_t
  ) : int64_t ;
  begin
    if (_val and $01) <> 0 then
      Result := -1 * int64_t((_val + 1) shr 1)
    else
      Result := int64_t(_val shr 1) ;
  end ;

  function TGIS_VarInt.unzigzag8(
    const _val : uint8_t
  ) : int8_t ;
  begin
    if (_val and $01) <> 0 then
      Result := -1 * int8_t((_val + 1) shr 1)
    else
      Result := int8_t(_val shr 1) ;
  end ;

  function TGIS_VarInt.zigzag32(
    const _val : int32_t
  ) : uint32_t ;
  begin
    if _val < 0 then
      Result := uint32_t((_val shl 1) xor (not (not _val shr 31)))
    else
      Result := uint32_t((_val shl 1) xor (_val shr 31)) ;
  end ;

  function TGIS_VarInt.zigzag64(
    const _val : int64_t
  ) : uint64_t ;
  begin
    if _val < 0 then
      Result := uint64_t((_val shl 1) xor (not (not _val shr 63)))
    else
      Result := uint64_t((_val shl 1) xor (_val shr 63)) ;
  end ;

  function TGIS_VarInt.zigzag8(
    const _val : int8_t
  ) : uint8_t ;
  begin
    if _val < 0 then
      Result := uint8_t((_val shl 1) xor (not (not _val shr 7)))
    else
      Result := uint8_t((_val shl 1) xor (_val shr 7)) ;
  end ;

  function TGIS_VarInt.varint_s64_decode(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t ;
      var _size  : size_t
   ) : int64_t ;
  begin
    Result := unzigzag64( varint_u64_decode(_buf, _start, _end, _size) ) ;
  end ;

  function TGIS_VarInt.varint_u64_decode(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t ;
      var _size  : size_t
   ) : uint64_t ;
  var
    nval    : uint64_t ;
    nshift  : Integer ;
    nbyte   : uint8_t ;
    ptr     : uint32_t ;
  begin
    Result := 0 ;
    nval   := 0 ;
    nshift := 0 ;
    ptr    := _start ;

    while (ptr < _end) do begin
      nbyte := _buf[ptr] ;

      if (nbyte and $80) <> 0 then begin
        nval := nval or (uint64_t(nbyte and $7f)) shl nshift ;
        inc(ptr) ;
        inc(nshift, 7) ;
      end
      else begin
        inc(ptr) ;
        _size   := ptr - _start ;
        Result := nval or (uint64_t(nbyte) shl nshift) ;
        exit ;
      end
    end ;
  end ;

  function TGIS_VarInt.varint_64_decode(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t ;
      var _size  : size_t
   ) : int64_t ;
  var
    nval    : int64_t ;
    nshift  : Integer ;
    nbyte   : uint8_t ;
    ptr     : uint32_t ;
  begin
    Result := 0 ;
    nval   := 0 ;
    nshift := 0 ;
    ptr    := _start ;

    while (ptr < _end) do begin
      nbyte := _buf[ptr] ;

      if (nbyte and $80) <> 0 then begin
        nval := nval or (int64_t(nbyte and $7f)) shl nshift ;
        inc(ptr) ;
        inc(nshift, 7) ;
      end
      else begin
        inc(ptr) ;
        _size   := ptr - _start ;
        Result := nval or (int64_t(nbyte) shl nshift) ;
        exit ;
      end
    end ;
  end ;

  function TGIS_VarInt.varint_s32_decode(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t ;
      var _size  : size_t
   ) : int32_t ;
  begin
    Result := unzigzag32( varint_u32_decode(_buf, _start, _end, _size) ) ;
  end ;

  function TGIS_VarInt.varint_32_decode(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t ;
      var _size  : size_t
   ) : int32_t ;
  var
    nval    : int32_t ;
    nshift  : Integer ;
    nbyte   : uint8_t ;
    ptr     : uint32_t ;
  begin
    Result := 0 ;
    nval   := 0 ;
    nshift := 0 ;
    ptr    := _start ;

    while (ptr < _end) do begin
      nbyte := _buf[ptr] ;

      if (nbyte and $80) <> 0 then begin
        nval := nval or (int32_t(nbyte and $7f)) shl nshift ;
        inc(ptr) ;
        inc(nshift, 7) ;
      end
      else begin
        inc(ptr) ;
        _size   := ptr - _start ;
        Result := nval or (int32_t(nbyte) shl nshift) ;
        exit ;
      end
    end ;
  end ;

  function TGIS_VarInt.varint_u32_decode(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t ;
      var _size  : size_t
   ) : uint32_t ;
  var
    nval    : uint32_t ;
    nshift  : Integer ;
    nbyte   : uint8_t ;
    ptr     : uint32_t ;
  begin
    Result := 0 ;
    nval   := 0 ;
    nshift := 0 ;
    ptr    := _start ;

    while (ptr < _end) do begin
      nbyte := _buf[ptr] ;

      if (nbyte and $80) <> 0 then begin
        nval := nval or (uint32_t(nbyte and $7f)) shl nshift ;
        inc(ptr) ;
        inc(nshift, 7) ;
      end
      else begin
        inc(ptr) ;
        _size   := ptr - _start ;
        Result := nval or (uint32_t(nbyte) shl nshift) ;
        exit ;
      end
    end ;
  end ;

  function TGIS_VarInt.varint_s32_encode_buf(
    const _val : int32_t ;
    const _buf : TBytes
  ) : size_t ;
  begin
    Result := varint_u64_encode_buf(uint64_t(zigzag32(_val)), _buf) ;
  end ;

  function TGIS_VarInt.varint_s64_encode_buf(
    const _val : int64_t ;
    const _buf : TBytes
  ) : size_t ;
  begin
    Result := varint_u64_encode_buf(zigzag64(_val), _buf) ;
  end ;

  function TGIS_VarInt.varint_u32_encode_buf(
    const _val : uint32_t ;
    const _buf : TBytes
  ) : size_t ;
  begin
    Result := varint_u64_encode_buf(uint64_t(_val), _buf) ;
  end ;

  function TGIS_VarInt.varint_u64_encode_buf(
    const _val : uint64_t ;
    const _buf : TBytes
  ) : size_t ;
  var
    grp : uint8_t ;
    q   : uint64_t ;
    ptr : uint8_t ;
  begin
    q   := _val ;
    ptr := 0 ;

    Result := 0 ;
    while True do begin
      grp := $7f and q ;
      q := q shr 7 ;
      if (q > 0) then begin
        _buf[ptr] := $80 or grp ;
        inc(ptr) ;
      end
      else begin
        _buf[ptr] := grp ;
        inc(ptr) ;
        Result := ptr ;
        exit ;
      end
    end ;
  end ;

  function TGIS_VarInt.varint_size(
    const _buf   : TBytes ;
    const _start : uint32_t ;
    const _end   : uint32_t
  ) : size_t ;
  var
    ptr : uint32_t ;
  begin
    ptr := _start ;
    while (ptr < _end) do begin
      if (_buf[ptr] and $80) <> 0 then
        inc(ptr)
      else begin
        inc(ptr) ;
        Result := ptr - _start ;
        exit ;
      end
    end ;
    Result := 0 ;
  end ;

//=============================================================================
// TGIS_VarIntReader
//=============================================================================

  procedure TGIS_VarIntReader.initBuffer(
    const _buf : TBytes ;
    const _len : uint32_t
  ) ;
  begin
    buf := _buf ;
    len := _len ;
    pos := 0 ;
  end ;

  procedure TGIS_VarIntReader.readBuffer(
    const _buf    : TBytes ;
    const _offset : uint32_t ;
    const _len    : uint32_t
  ) ;
  begin
    GisCopyMemory( buf, pos, _buf, _offset, _len ) ;
    inc( pos, _len ) ;
  end ;

  function TGIS_VarIntReader.readByte : uint8_t ;
  begin
    Result := buf[pos] ;
    inc( pos ) ;
  end ;

  function TGIS_VarIntReader.readDouble(
    const _factor : Double
  ): Double;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_s64_decode( buf, pos, len, vsize ) / _factor ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.readVarUInt32 : uint32_t ;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_u32_decode( buf, pos, len, vsize ) ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.readVarInt32 : int32_t ;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_32_decode( buf, pos, len, vsize ) ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.readVarSInt32 : int32_t ;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_s32_decode( buf, pos, len, vsize ) ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.readVarUInt64 : uint64_t ;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_u64_decode( buf, pos, len, vsize ) ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.readVarSInt64 : int64_t ;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_s64_decode( buf, pos, len, vsize ) ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.readVarInt64 : int64_t ;
  var
    vsize : size_t ;
  begin
    Result := vi.varint_64_decode( buf, pos, len, vsize ) ;
    inc( pos, vsize ) ;
  end ;

  procedure TGIS_VarIntReader.skipBytes(
    const _len : uint32_t
  ) ;
  begin
    inc( pos, _len ) ;
  end ;

  procedure TGIS_VarIntReader.skipVarInt64 ;
  var
    vsize : size_t ;
  begin
    vsize := vi.varint_size( buf, pos, len ) ;
    inc( pos, vsize ) ;
  end ;

  function TGIS_VarIntReader.unzigzag8(
    const _val    : uint8_t
  ) : int8_t ;
  begin
    Result := vi.unzigzag8( _val ) ;
  end ;

//=============================================================================
// TGIS_GPBReader
//=============================================================================

  constructor TGIS_GPBReader.Create(
    const _buf : TBytes ;
    const _len : uint32_t
  ) ;
  begin
    inherited Create ;

    initBuffer( _buf, _len ) ;
  end ;

  function TGIS_GPBReader.MakeKey(
    const _fieldNumber : int32_t ;
    const _wireType    : int32_t
  ) : Integer ;
  begin
    Result := (_fieldNumber shl 3) or _wireType ;
  end ;

  function TGIS_GPBReader.GetWireType(
    const _key : int32_t
  ) : Integer ;
  begin
    Result := _key and $7 ;
  end ;

  function TGIS_GPBReader.GetFieldNumber(
    const _key : int32_t
  ) : int32_t ;
  begin
    Result := _key shr 3 ;
  end ;

  function TGIS_GPBReader.ReadFieldKey : int32_t ;
  begin
    Result := readVarInt32 ;
  end ;

  function TGIS_GPBReader.ReadSize : uint32_t ;
  begin
    Result := readVarUInt32 ;
  end ;

  function TGIS_GPBReader.ReadText : String ;
  var
    datalen : uint32_t ;
    sbuf    : TBytes ;
  begin
    datalen := ReadSize ;
    if datalen > 0 then begin
      SetLength( sbuf, datalen+1 ) ;
      readBuffer( sbuf, 0, datalen) ;
      try
        Result := TEncoding.UTF8.GetString( sbuf, 0, datalen ) ;
      except
        Result := '' ;
      end;
    end
    else
      Result := '' ;
  end ;

  function TGIS_GPBReader.SkipUnknownField(
    const _key      : int32_t ;
    const _verbose  : Boolean
  ) : int32_t ;
  var
    wiretype : int32_t ;
  begin
    Result := Position ;

    wiretype := GetWireType( _key ) ;
    if _verbose then
      GetFieldNumber( _key ) ;

    case wiretype of
       WT_VARINT  : skipVarInt64 ;
       WT_32BIT   : skipBytes( 4 ) ;
       WT_64BIT   : skipBytes( 8 ) ;
       WT_DATA    : skipBytes( ReadSize ) ;
    end ;
    Result := Position - Result ;
  end ;

//==================================== END =====================================
end.
