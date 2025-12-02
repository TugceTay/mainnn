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
  Memory mapped file stream module.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoStreams ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoStreams"'}
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
    System.IO,
    System.Runtime.InteropServices,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Variants,
    System.Generics.Collections,
    System.Generics.Defaults,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    java.io ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type
  /// <summary>
  ///   Mode of stream access
  /// </summary>
  TGIS_StreamMode = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Read a stream.
    /// </summary>
    &Read,
    /// <summary>
    ///   Create a new stream.
    /// </summary>
    &Create,
    /// <summary>
    ///   Append to existing stream.
    /// </summary>
    Append
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisStreamRead   = TGIS_StreamMode.Read    ;
      gisStreamCreate = TGIS_StreamMode.&Create ;
      gisStreamAppend = TGIS_StreamMode.Append  ;
  {$ENDIF}

type

  /// <summary>
  ///   Enhanced stream. Added number of functions like ReadLine, ReadToken,
  ///   ReadString, WriteString.
  /// </summary>
  TGIS_Stream = {$IFDEF OXYGENE} public abstract {$ENDIF} class( TGIS_BaseStream )
    private
      FCodePage     : Integer ;
      FFixCodePage  : Boolean ;
      arBuffer      : TBytes ;
      layerEncoding : TEncoding ;
      fixedCodePage : Integer ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FOnRead      : TGIS_ReadWriteEvent ;
      FOnWrite     : TGIS_ReadWriteEvent ;

    private
      procedure fset_CodePage( const _value : Integer ) ;

    private
      procedure autofixCodePage ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy      ; override;
    public // constructors
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Create a stream manager on existing stream.
        /// </summary>
        /// <param name="_stream">
        ///   stream to manage
        /// </param>
        /// <param name="_onread">
        ///   event to be called upon each read operation
        /// </param>
        /// <param name="_onwrite">
        ///   event to be called upon each write operation
        /// </param>
        constructor Create   ( const _stream   : TStream             ;
                               const _onread   : TGIS_ReadWriteEvent ;
                               const _onwrite  : TGIS_ReadWriteEvent
                             ) ;

        /// <summary>
        ///   Create a stream manager on existing stream.
        /// </summary>
        /// <param name="_stream">
        ///   stream to manage
        /// </param>
        constructor Create   ( const _stream   : TStream
                             ) ;
      {$ENDIF}


      /// <summary>
      ///   Create a stream object.
      /// </summary>
      /// <param name="_onread">
      ///   event to be called upon each read operation
      /// </param>
      /// <param name="_onwrite">
      ///   event to be called upon each write operation
      /// </param>
      constructor Create         ( const _onread   : TGIS_ReadWriteEvent ;
                                   const _onwrite  : TGIS_ReadWriteEvent
                                 ) ; overload;

      /// <summary>
      ///   Create a stream object.
      /// </summary>
      constructor Create         ; overload;

    public // basic routines
      /// <summary>
      ///   Check if stream reached End Of File.
      /// </summary>
      /// <returns>
      ///   True is stream is art EOF.
      /// </returns>
      function  Eof              : Boolean ; virtual; abstract;

      /// <summary>
      ///   Read line from text file up to CR. LF are ignored.
      ///   Procedure is Code Page aware.
      /// </summary>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadLine         : String ;

      /// <summary>
      ///   Read token (delimited by a space char) from text file.
      ///   Procedure is Code Page aware.
      /// </summary>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadToken        : String ;

      /// <summary>
      ///   Write text line ended with CR/LF to the stream. Upon writing string
      ///   will be converted to representation defined by CodePage property.
      /// </summary>
      /// <param name="_buf">
      ///   string to be written
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function  WriteLine        ( const _buf      : String
                                 ) : Integer ;

      /// <summary>
      ///   Read string of the width from the stream. Upon reading string will
      ///   be converted from the representation defined by CodePage property.
      /// </summary>
      /// <param name="_buf">
      ///   string to be read
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function  ReadString       ( var   _buf      : String  ;
                                   const _count    : Integer
                                 ) : Integer ; {$IFNDEF OXYGENE} overload; {$ENDIF}

      /// <summary>
      ///   Read string of the width from the stream. Upon reading string will
      ///   be converted from the representation defined by CodePage property.
      /// </summary>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadString       ( const _count    : Integer
                                 ) : String ; {$IFNDEF OXYGENE} overload; {$ENDIF}

      /// <summary>
      ///   Read string of the width from the stream.
      ///   Procedure assumes that text is ASCII.
      /// </summary>
      /// <param name="_buf">
      ///   string to be read
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function  ReadAsciiString  ( var   _buf      : String  ;
                                   const _count    : Integer
                                 ) : Integer ; {$IFNDEF OXYGENE} overload; {$ENDIF}

      /// <summary>
      ///   Read string of the width from the stream.
      ///   Procedure assumes that text is ASCII.
      /// </summary>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadAsciiString  ( const _count    : Integer
                                 ) : String ; {$IFNDEF OXYGENE} overload; {$ENDIF}

      /// <summary>
      ///   Write a string to the stream. Upon writing string will be converted
      ///   to representation defined by CodePage property.
      /// </summary>
      /// <param name="_buf">
      ///   string to be written
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function  WriteString      ( const _buf      : String
                                 ) : Integer ;

      /// <summary>
      ///   Write a string to the stream. If written string is smaller then
      ///   _count bytes then rest of the string will be filled with spaces.
      ///   Upon writing string will be converted to representation defined by
      ///   CodePage property.
      /// </summary>
      /// <param name="_buf">
      ///   string to be written
      /// </param>
      /// <param name="_count">
      ///   number of bytes which string should occupy in the stream
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      function  WriteStringCnt   ( const _buf      : String  ;
                                   const _count    : Integer
                                 ) : Integer ;


      /// <summary>
      ///   <para>
      ///     Write a variant as a string to the stream. If written string is
      ///     smaller then
      ///   </para>
      ///   <para>
      ///     If written string is smaller then _count bytes then rest of the
      ///    string will be filled with spaces.
      ///   </para>
      /// </summary>
      /// <param name="_buf">
      ///   object to be written
      /// </param>
      /// <param name="_count">
      ///   number of bytes which string should occupy in the stream
      /// </param>
      /// <returns>
      ///   Number of written bytes.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    Upon writing string will be converted to representation defined
      ///    by CodePage property.
      ///    </note>
      /// </remarks>
      function WriteVarAsString( const _buf      : {$IFDEF OXYGENE}
                                                     TObject ;
                                                   {$ELSE}
                                                     Variant ;
                                                   {$ENDIF}
                                 const _count    : Integer
                               ) : Integer ;

      /// <summary>
      ///   Read array of bytes
      /// </summary>
      /// <param name="_buf">
      ///   array of bytes; buffer bust be allocated
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function  ReadBytesCnt     ( const _buf      : TBytes  ;
                                   const _count    : Integer
                                 ) : Integer ; overload;

      /// <summary>
      ///   Read array of bytes into a buffer at given offset.
      /// </summary>
      /// <param name="_buf">
      ///   array of bytes; buffer bust be allocated
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <param name="_offset">
      ///   offset in the buffer where the data will be written
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function  ReadBytesCnt     ( var   _buf      : TBytes  ;
                                   const _count    : Integer ;
                                   const _offset   : Integer
                                 ) : Integer ; overload;

      /// <summary>
      ///   Read array of bytes
      /// </summary>
      /// <param name="_buf">
      ///   array of bytes; buffer will be automatically allocated
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be read
      /// </param>
      /// <returns>
      ///   Number of read bytes.
      /// </returns>
      function  ReadBytesSafe    ( var   _buf      : TBytes  ;
                                   const _count    : Integer
                                 ) : Integer ;

      /// <summary>
      ///   Write array of bytes
      /// </summary>
      /// <param name="_buf">
      ///   array of bytes
      /// </param>
      procedure WriteBytes       ( const _buf      : TBytes
                                 ) ; overload;

      /// <summary>
      ///   Write array of bytes
      /// </summary>
      /// <param name="_buf">
      ///   array of bytes
      /// </param>
      /// <param name="_count">
      ///   number of bytes to be write (in a meaning no more then _count); if
      ///   _buf length is less then _count then only number of bytes equal to
      ///   the buffer length will be written
      /// </param>
      procedure WriteBytesCnt    ( const _buf    : TBytes  ;
                                   const _count    : Integer
                                 ) ; overload;
    public
      /// <summary>
      ///   Code page of the stream.
      /// </summary>
      property CodePage : Integer read FCodePage write fset_CodePage ;

      /// <summary>
      ///   If True then program will try to switch between UTF8 and System Code Page
      ///   to properly recognize files with improper setup.
      /// </summary>
      property FixCodePage : Boolean read FFixCodePage write FFixCodePage ;
  end ;

{$IFNDEF OXYGENE}

  /// <summary>
  ///   Equivalent to TStream with a handle to a stream.
  /// </summary>
  TGIS_HandleStream = class( TGIS_BaseStream )
    private
      function  fget_Position : Int64 ;
      procedure fset_Position ( _value : Int64 ) ;
      function  fget_Size : Int64 ;
      procedure fset_Size ( _value : Int64 ) ;
    protected

      /// <summary>
      ///   File handle.
      /// </summary>
      FStream : TStream ;
    protected
      procedure doDestroy ; virtual;
    public
      /// <summary>
      ///   Create an instance for the given stream.
      /// </summary>
      /// <param name="_stream">
      ///   given stream
      /// </param>
      constructor Create ( _stream : TStream ) ; overload;

      /// <summary>
      ///   Standard destructor.
      /// </summary>
      destructor Destroy ; override;

      /// <summary>
      ///   See documentation for TStream in Delphi help.
      /// </summary>
      /// <param name="_buffer">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <param name="_count">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <returns>
      ///   See documentation for TStream in Delphi help.
      /// </returns>
      function Read(var _buffer; _count: LongInt): LongInt; override;

      /// <summary>
      ///   See documentation for TStream in Delphi help.
      /// </summary>
      /// <param name="_buffer">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <param name="_count">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <returns>
      ///   See documentation for TStream in Delphi help.
      /// </returns>
      function Write(const _buffer; _count: LongInt): LongInt; override;

      /// <summary>
      ///   See documentation for TStream in Delphi help.
      /// </summary>
      /// <param name="_offset">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <param name="_origin">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <returns>
      ///   See documentation for TStream in Delphi help.
      /// </returns>
      function Seek(_offset: LongInt; _origin: Word): LongInt; override;

      /// <summary>
      ///   See documentation for TStream in Delphi help.
      /// </summary>
      /// <param name="_offset">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <param name="_origin">
      ///   See documentation for TStream in Delphi help.
      /// </param>
      /// <returns>
      ///   See documentation for TStream in Delphi help.
      /// </returns>
      function Seek(const _offset: Int64; _origin: TSeekOrigin): Int64; override;

    public
      /// <summary>
      ///   See documentation for TStream in Delphi help.
      /// </summary>
      property Position: Int64 read fget_Position
                               write fset_Position ;

      /// <summary>
      ///   See documentation for TStream in Delphi help.
      /// </summary>
      property Size: Int64 read fget_Size write fset_Size;
  end ;

{$ELSE}

  /// <summary>
  ///   Equivalent to TStream with a handle to a stream.
  /// </summary>
  TGIS_HandleStream = TGIS_BaseStream ;

{$ENDIF}

{$IFNDEF OXYGENE}

  /// <summary>
  ///   Equivalent to TMemoryStream.
  /// </summary>
  TGIS_MemoryStream = class( TGIS_HandleStream )
    private
      function fget_Memory : Pointer ;
    protected
      procedure doDestroy ; virtual;
    public
      /// <summary>
      ///   See documentation for TMemoryStream in Delphi help.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Standard destructor.
      /// </summary>
      destructor Destroy ; override;

      /// <summary>
      ///   See documentation for TMemoryStream in Delphi help.
      /// </summary>
      procedure Clear ;
    public
      /// <summary>
      ///   See documentation for TMemoryStream in Delphi help.
      /// </summary>
      property Memory : Pointer read fget_Memory ;
  end ;

{$ELSE}
  /// <summary>
  ///   File stream.
  /// </summary>
  TGIS_MemoryStream = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Stream )
    protected
      {$IFDEF CLR}
        function  fget_CanRead  : Boolean ; override ;
        function  fget_CanSeek  : Boolean ; override ;
        function  fget_CanWrite : Boolean ; override ;
      {$ENDIF}
      function    fget_Length   : Int64   ; override;
      function    fget_Position : Int64   ; override; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure   fset_Position ( const _i : Int64 ) ; override;
    private
      function    fget_Memory : array of Byte ;
    protected
      procedure   doDestroy ; override;
    public

      /// <summary>
      ///   Standard constructor.
      /// </summary>
      constructor Create          ;

      /// <summary>
      ///   Create stream based on existing platform stream
      /// </summary>
      /// <param name="_stream">
      ///   base stream
      /// </param>
      constructor Create        ( const _stream : {$IFDEF CLR}Stream{$ELSE}TStream{$ENDIF}
                                ) ;
    public
      /// <inheritdoc/>
      function  Eof              : Boolean ; override;
      /// <inheritdoc/>
      function  Seek            (     _offset   : Int64          ;
                                      _origin   : TSeekOrigin
                                ) : Int64 ; override;
      /// <inheritdoc/>
      procedure SetLength       (     _value    : Int64
                                ) ; override;
      /// <inheritdoc/>
      function  Read            (     _buffer   : array of Byte  ;
                                      _offset   : Int32          ;
                                      _count    : Int32
                                ) : Int32 ; override;
      /// <inheritdoc/>
      procedure Write           (     _buffer   : array of Byte ;
                                      _offset   : Int32          ;
                                      _count    : Int32
                                ) ; override;
      /// <inheritdoc/>
      procedure Flush             ; override;

      /// <summary>
      ///   CLear current stream content.
      /// </summary>
      procedure Clear             ;

      /// <summary>
      ///   Save content to other stream
      /// </summary>
      /// <param name="_stream">
      ///   stream to which content will be saved
      /// </param>
      procedure SaveToStream    (     _stream   : TGIS_MemoryStream
                                ) ;
    public
      /// <summary>
      ///   Stream content.
      /// </summary>
      property Memory : array of Byte read fget_Memory ;
  end ;

{$ENDIF}

{$IFNDEF OXYGENE}

  /// <summary>
  ///   Equivalent to TFileStream.
  /// </summary>
  TGIS_FileStream = class(TGIS_HandleStream)
    strict private
      FName       : String  ;
      FMode       : Integer ;
      FPatrolRead : Boolean ;
    private
      procedure fset_PatrolRead( const _val : Boolean ) ;
    protected
      procedure doDestroy ; virtual;
    public

      /// <summary>
      ///   Create an instance of stream.
      /// </summary>
      /// <param name="AFileName">
      ///   name of the file to be opened
      /// </param>
      /// <param name="Mode">
      ///   open mode: fmOpenRead, fmOpenWrite, fmOpenReadWrite,
      ///   fmShareExclusive, fmShareDenyWrite, fmShareDenyRead, fmShareDenyNone
      /// </param>
      /// <remarks>
      ///   if other flags are not specified then fmShareDenyNone is used
      /// </remarks>
      constructor Create(const AFileName: String; Mode: Word); overload;

      /// <summary>
      ///   Create an instance of stream.
      /// </summary>
      /// <param name="AFileName">
      ///   name of the file to be opened
      /// </param>
      /// <param name="Mode">
      ///   open mode: fmOpenRead, fmOpenWrite, fmOpenReadWrite,
      ///   fmShareExclusive, fmShareDenyWrite, fmShareDenyRead, fmShareDenyNone
      /// </param>
      /// <param name="Rights">
      ///   Not used upon Windows
      /// </param>
      /// <remarks>
      ///   if other flags are not specified then fmShareDenyNone is used
      /// </remarks>
      constructor Create(const AFileName: String; Mode: Word; Rights: Cardinal); overload;

      /// <summary>
      ///   Standard destructor.
      /// </summary>
      destructor Destroy ; override;

    public
      /// <summary>
      ///   See documentation for TFileStream in Delphi help.
      /// </summary>
      property FileName: String read FName;

      /// <summary>
      ///   If True, use patrol read to fetch whole file into OS cache.
      /// </summary>
      property PatrolRead : Boolean read FPatrolRead write fset_PatrolRead ;
  end ;

{$ELSE}

  /// <summary>
  ///   Equivalent to TFileStream.
  /// </summary>
  TGIS_FileStream = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Stream )
    private
      FName       : String ;
      FMode       : Integer ;
      FPatrolRead : Boolean ;
    private
      procedure fset_PatrolRead( const _val : Boolean
                               ) ;
      {$IFDEF JAVA}
        procedure translateMode( const _vcl  : UInt32     ;
                                   out _mode : String
                               ) ;
      {$ENDIF}
      {$IFDEF CLR}
        procedure translateMode( const _vcl : UInt32     ;
                                   out _net : FileMode   ;
                                   out _acc : FileAccess ;
                                   out _shr : FileShare
                               ) ;
      {$ENDIF}
    protected
      {$IFDEF CLR}
        function  fget_CanRead  : Boolean ; override ;
        function  fget_CanSeek  : Boolean ; override ;
        function  fget_CanWrite : Boolean ; override ;
      {$ENDIF}
        function  fget_Length   : Int64   ; override;
        function  fget_Position : Int64   ; override; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
        procedure fset_Position ( const _i : Int64 ) ; override;
    public

      /// <summary>
      ///   Create an instance of stream.
      /// </summary>
      /// <param name="_name">
      ///   name of the file to be opened
      /// </param>
      /// <param name="_mode">
      ///   open mode: fmOpenRead, fmOpenWrite, fmOpenReadWrite,
      ///   fmShareExclusive, fmShareDenyWrite, fmShareDenyRead, fmShareDenyNone
      /// </param>
      /// <remarks>
      ///   if other flags are not specified then fmShareDenyNone is used
      /// </remarks>
      constructor Create ( const _name : String ;
                           const _mode : UInt32
                         ) ;
    public
      /// <inheritdoc/>
      function  Seek            (     _offset   : Int64          ;
                                      _origin   : TSeekOrigin
                                ) : Int64 ; override;
      /// <inheritdoc/>
      procedure SetLength       (     _value    : Int64
                                ) ; override;
      /// <inheritdoc/>
      function  &Read           (     _buffer   : array of Byte  ;
                                      _offset   : Int32          ;
                                      _count    : Int32
                                ) : Int32 ; override;
      /// <inheritdoc/>
      procedure &Write          (     _buffer   : array of Byte ;
                                      _offset   : Int32          ;
                                      _count    : Int32
                                ) ; override;
      /// <inheritdoc/>
      procedure Flush             ; override;
    protected
      procedure doDestroy ; override;
    public
      /// <inheritdoc/>
      function  Eof : Boolean ; override;
    public
      /// <inheritdoc/>
      property Size : Int64 read fget_Length write SetLength ;

      /// <inheritdoc/>
      property PatrolRead : Boolean read FPatrolRead write fset_PatrolRead ;
  end ;

{$ENDIF}

  /// <summary>
  ///   Buffered stream.
  /// </summary>
  TGIS_BufferedStream = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Stream )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      {$IFNDEF OXYGENE}
          /// <summary>
          ///   File handle.
          /// </summary>
          FStream : TStream ;
      {$ENDIF}

      /// <summary>
      ///   Current position within file.
      /// </summary>
      FPosition   : Int64 ;

      /// <summary>
      ///   File size.
      /// </summary>
      FSize       : Int64 ;

      /// <summary>
      ///   File name of mapped file.
      /// </summary>
      FFileName   : String  ;

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      oldPosition      : Int64   ;

      iReadBufferPos   : LongInt ;
      iReadBufferSize  : LongInt ;
      iWriteBufferSize : LongInt ;
      arReadBuffer     : TBytes  ;
      arWriteBuffer    : TBytes  ;

    protected
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          function  fget_CanRead  : Boolean ; override ;
          function  fget_CanSeek  : Boolean ; override ;
          function  fget_CanWrite : Boolean ; override ;
        {$ENDIF}
        function    fget_Length   : Int64   ; override;
       public
         /// <inheritdoc/>
         procedure SetLength     (       _value    : Int64
                                 ) ; override;
         /// <inheritdoc/>
         procedure Flush         ; override;
      {$ENDIF}

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      procedure flushWrite ;
      {$IFDEF MANAGED}
        function writeBuf     ( const _buffer : array of Byte ;
                                      _count  : LongInt
                              ): LongInt;
      {$ELSE}
        function writeBuf     ( const _buffer ;
                                      _count  : LongInt
                              ): LongInt ;
      {$ENDIF}
    protected
      function  fget_Position  : Int64 ; {$IFDEF OXYGENE} override; {$ENDIF}
                                         {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure fset_Position  ( const _value : Int64
                              ) ; {$IFDEF OXYGENE} override; {$ENDIF}
      {$IFDEF OXYGENE}
          /// <summary>
          ///   Set stream size
          /// </summary>
          /// <param name="_value">
          ///   mew size of the stream
          /// </param>
         procedure SetSize    ( _value       : Int64
                              ) ; {$IFNDEF OXYGENE} override; {$ENDIF}
      {$ELSE}
      {$ENDIF}

      /// <summary>
      ///   Constructor helper.
      /// </summary>
      /// <param name="_stream">
      ///   exiting stream on which buffered stream must be build
      /// </param>
      /// <param name="_onread">
      ///   event to be called upon each read operation
      /// </param>
      /// <param name="_onwrite">
      ///   event to be called upon each write operation
      /// </param>
      procedure doCreate     ( const _stream   : TStream             ;
                               {$IFNDEF OXYGENE}
                                 const _onread   : TGIS_ReadWriteEvent  = nil ;
                                 const _onwrite  : TGIS_ReadWriteEvent  = nil
                               {$ELSE}
                                 const _onread   : TGIS_ReadWriteEvent := nil ;
                                 const _onwrite  : TGIS_ReadWriteEvent := nil
                               {$ENDIF}
                             ) ;
    protected

        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        procedure doDestroy  ; override;

    public

      /// <inheritdoc/>
      /// <summary>
      ///   Create a buffered file stream on a given stream.
      /// </summary>
      constructor Create   ( const _stream   : TStream             ;
                             const _onread   : TGIS_ReadWriteEvent ;
                             const _onwrite  : TGIS_ReadWriteEvent
                           ) ; {$IFNDEF OXYGENE} overload; {$ENDIF}

      /// <summary>
      ///   Create a buffered file stream on a given stream.
      /// </summary>
      /// <param name="_stream">
      ///   exiting stream on which buffered stream must be build
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    File must exist and not be empty.
      ///    </note>
      /// </remarks>
      constructor Create     ( const _stream  : TStream
                             ) ; overload;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Create a stream.
        /// </summary>
        /// <param name="_onread">
        ///   event to be called upon each read operation
        /// </param>
        /// <param name="_onwrite">
        ///   event to be called upon each write operation
        /// </param>
        constructor Create   ( const _onread   : TGIS_ReadWriteEvent ;
                               const _onwrite  : TGIS_ReadWriteEvent
                             ) ;

        /// <summary>
        ///   Standard constructor.
        /// </summary>
        constructor   Create   ;
      {$ENDIF}

      // basic routines
         {$IFDEF MANAGED}
           /// <inheritdoc/>
           function Read       {$IFNDEF OXYGENE}
                                 (
                                    var   _buffer   : array of Byte ;
                                          _offset   : LongInt       ;
                                          _count    : LongInt
                                 ) : LongInt ; override;
                               {$ELSE}
                                 (        _buffer   : array of Byte ;
                                          _offset   : Int32       ;
                                          _count    : Int32
                                 ) : Int32 ; override;
                              {$ENDIF}
         {$ELSE}

           /// <inheritdoc/>
           function Read       ( var   _buffer   ;
                                       _count    : LongInt
                               ) : LongInt ; override;
         {$ENDIF}
         {$IFDEF MANAGED}
           {$IFNDEF OXYGENE}
             /// <inheritdoc/>
             function  Write   ( const _buffer   : array of Byte ;
                                       _offset   : LongInt       ;
                                       _count    : LongInt
                               ) : LongInt ; override;
           {$ELSE}
             /// <inheritdoc/>
             procedure Write     ( const _buffer   : array of Byte ;
                                         _offset   : LongInt       ;
                                         _count    : LongInt
                                 ) ; override;
           {$ENDIF}
         {$ELSE}
           /// <inheritdoc/>
           function Write      ( const _buffer   ;
                                       _count    : LongInt
                               ) : LongInt ; override;
         {$ENDIF}

         /// <inheritdoc/>
         function Seek         ( const _offset   : Int64         ;
                                       _origin   : TSeekOrigin
                               ) : Int64 ; override;

         /// <summary>
         ///   Flush all buffers.
         /// </summary>
         /// <remarks>
         ///   All subsequent reads will be directly from file again.
         /// </remarks>
         procedure FlushBuffer ; virtual;

         /// <inheritdoc/>
         function  Eof         : Boolean ; override;

     public
       {$IFNDEF JAVA}
   {$IFNDEF ISLAND}
     /// <inheritdoc/>
     property Position : Int64 read  fget_Position
                 write fset_Position ;
             {$IFDEF OXYGENE} override ; {$ENDIF}
   {$ENDIF}
       {$ENDIF}

         /// <summary>
         ///   Get actual size of the stream.
         /// </summary>
         property Size     : Int64 read  FSize ;
  end ;

  /// <summary>
  ///   Buffered file stream.
  /// </summary>
  TGIS_BufferedFileStream = {$IFDEF OXYGENE} public {$ENDIF}
                            class( TGIS_BufferedStream )
    private

      /// <summary>
      ///   File name of mapped file.
      /// </summary>
      FFileName : String ;

    protected

      /// <summary>
      ///   Constructor helper.
      /// </summary>
      /// <param name="_filename">
      ///   name of file to be opened
      /// </param>
      /// <param name="_mode">
      ///   file mode to use
      /// </param>
      /// <param name="_onread">
      ///   event to be called upon each read operation
      /// </param>
      /// <param name="_onwrite">
      ///   event to be called upon each write operation
      /// </param>
      procedure doCreate   ( const _filename : String                    ;
                             const _mode     : TGIS_StreamMode           ;
                             const _onread   : TGIS_ReadWriteEvent       ;
                             const _onwrite  : TGIS_ReadWriteEvent
                           ) ;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;

    public // constructors

      /// <summary>
      ///   Create a buffered file stream on a given file.
      /// </summary>
      /// <param name="_filename">
      ///   name of file to be opened
      /// </param>
      /// <param name="_mode">
      ///   file mode to use
      /// </param>
      /// <param name="_onread">
      ///   event to be called upon each read operation
      /// </param>
      /// <param name="_onwrite">
      ///   event to be called upon each write operation
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEMAPPING
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    File must exist and not be empty.
      ///    </note>
      /// </remarks>
      constructor Create ( const _filename : String                    ;
                           const _mode     : TGIS_StreamMode           ;
                           const _onread   : TGIS_ReadWriteEvent       ;
                           const _onwrite  : TGIS_ReadWriteEvent
                         ) ; {$IFNDEF OXYGENE} overload; {$ENDIF}

      /// <summary>
      ///   Create a buffered file stream on a given file.
      /// </summary>
      /// <param name="_filename">
      ///   name of file to be opened
      /// </param>
      /// <param name="_mode">
      ///   opening mode of the stream
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    File must exist and not be empty.
      ///    </note>
      /// </remarks>
      constructor Create   ( const _filename : String                    ;
                             const _mode     : TGIS_StreamMode
                           ) ; overload;

    public

        /// <summary>
        ///   Stream file path (if applicable).
        /// </summary>
        property Path : String read FFileName ;
  end ;

  /// <summary>
  ///   Memory mapped temporary file stream.
  /// </summary>
  TGIS_TemporaryFileStream = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_ObjectDisposable )
    private

      /// <summary>
      ///   Data file handle.
      /// </summary>
      dataFile  : TGIS_BufferedFileStream ;

      /// <summary>
      ///   Header item list.
      /// </summary>
      items     : TDictionary<Int64,Int64> ;

      /// <summary>
      ///   Predefined buffer for point data.
      /// </summary>
      bufPtg    : TBytes ;

      /// <summary>
      ///   Predefined buffer for point 3D data.
      /// </summary>
      bufPtg3D  : TBytes ;

    protected

      /// <summary>
      ///   Destructor.
      /// </summary>
      procedure doDestroy       ; override;

    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_file">
      ///   file name
      /// </param>
      constructor Create          ( const _file : String
                                  ) ;
      /// <summary>
      ///   Add feature to the file.
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      /// <param name="_data">
      ///   data of a feature as a binary
      /// </param>
      procedure AddFeature        ( const _id   : Int64 ;
                                    const _data : TBytes
                                  ) ; overload;

      /// <summary>
      ///   Add feature to the file.
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      /// <param name="_data">
      ///   data of a feature as a point
      /// </param>
      procedure AddFeature        ( const _id   : Int64 ;
                                    const _data : TGIS_Point
                                  ) ; overload;

        /// <summary>
        ///   Add feature to the file.
        /// </summary>
        /// <param name="_id">
        ///   feature id
        /// </param>
        /// <param name="_data">
        ///   data of a feature as a point
        /// </param>
        procedure AddFeature      ( const _id   : Int64 ;
                                    const _data : TGIS_Point3D
                                  ) ; overload;

      /// <summary>
      ///   Get feature from the file.
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      /// <param name="_data">
      ///   returned data of a feature as binary
      /// </param>
      /// <returns>
      ///   True if feature is properly read.
      /// </returns>
      function  GetFeature        ( const _id   : Int64 ;
                                    var   _data : TBytes
                                  ) : Boolean ; overload;

      /// <summary>
      ///   Get feature from the file.
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      /// <param name="_data">
      ///   returned data of a feature as a point
      /// </param>
      /// <returns>
      ///   True if feature is properly read.
      /// </returns>
      function  GetFeature2D      ( const _id   : Int64 ;
                                    var   _data : TGIS_Point
                                  ) : Boolean ; overload;

      /// <summary>
      ///   Get feature from the file.
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      /// <param name="_data">
      ///   returned data of a feature as a point
      /// </param>
      /// <returns>
      ///   True if feature is properly read.
      /// </returns>
      function  GetFeature3D      ( const _id   : Int64 ;
                                    var   _data : TGIS_Point3D
                                  ) : Boolean ; overload;
      /// <summary>
      ///   Clear the file.
      /// </summary>
      procedure Clear             ;

      /// <summary>
      ///   Set file position.
      /// </summary>
      /// <param name="_position">
      ///   position in file
      /// </param>
      procedure SetFilePosition   ( const _position : Int64
                                  ) ;
  end ;

  /// <summary>
  ///   Reader optimized to read and decode Code Page of existing files as
  ///   fast as possible. Support BOM of Unicode files UTF8, UTG-16 Big Endian
  ///   and UTF-16 Low Endian. ANSI and other encoding must be set manually.
  /// </summary>
  TGIS_TextStreamReader = {$IFDEF OXYGENE} public {$ENDIF}
                          class( TGIS_ObjectDisposable )
    private
      FCodePage     : Integer ;
      FFixCodePage  : Boolean ;
      oStream       : TStream ;
      oEncoding     : TEncoding ;
      bEof          : Boolean ;
      bBom          : Boolean ;
      arBuffer      : TBytes  ;
      arOutBuffer   : TBytes  ;
      iBufSize      : Integer ;
      iBufPos       : Integer ;
      fixedCodePage : Integer ;
    private
      procedure fset_CodePage ( const _value : Integer ) ;

    private
      procedure autofixCodePage ;
      procedure readChunk ;

    protected
      procedure doDestroy; override;

    public
      /// <summary>
      ///   Create an instance based on existing stream.
      /// </summary>
      /// <param name="_stream">
      ///   stream to be attached to the reader
      /// </param>
      constructor Create( const _stream : TStream ) ;

    public
      /// <summary>
      ///   Read line from text file up to CR. LF are ignored.
      /// </summary>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadLine   : String ;

      /// <summary>
      ///   Read xml header from text file to detect encoding.
      /// </summary>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadXMLHeader   : String ;

      /// <summary>
      ///   Read all text up to the predefined buffer size (32K).
      ///   To be used on text file parsers like XML.
      /// </summary>
      /// <returns>
      ///   Read string.
      /// </returns>
      function  ReadBuffer : String ;

      /// <summary>
      ///   Check if stream reached End Of File.
      /// </summary>
      /// <returns>
      ///   True is stream is art EOF.
      /// </returns>
      function  Eof        : Boolean ;
    public
      /// <summary>
      ///    Code Page. If file had a BOM then it is set accordingly.
      ///    Default (0) is ASCII.
      /// </summary>
      property CodePage : Integer read FCodePage write fset_CodePage ;

      /// <summary>
      ///   If True then program will try to switch between UTF8 and System Code Page
      ///   to properly recognize files with improper setup.
      /// </summary>
      property FixCodePage : Boolean read FFixCodePage write FFixCodePage ;
  end;

const
   {#gendoc:hide}
   METADATA_STREAMPATROLREAD
     = 'TGIS_Stream.PatrolRead';

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SyncObjs,
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}

    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoClasses;
{$ENDIF}

type
  {$IFNDEF MANAGED}
    P_PCharArray = ^T_PCharArray;
    T_PCharArray = array [0..0] of PChar;
  {$ENDIF}

{$REGION 'TPatrolRead'}

{$IFNDEF ISLAND}
  TPatrolReadThread = class;

  { Patrol read.
  }
  TPatrolRead = class( TGIS_ThreadClass )
    private
    {$IFDEF OXYGENE} unit {$ENDIF}
      iThreads  : Integer ;
      oList     : TGIS_StringList ;
      bNotify   : Boolean ;
      bBreak    : Boolean ;
      iEnabled  : Integer ;
    protected
      function  fget_Count : Integer ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      procedure Push   ( const _path : String
                       ) ;
      function  Pop    : String ;
      procedure Delete ( const _path : String
                       ) ;
      function  Present( const _path : String
                       ) : Boolean ;
      procedure IncThreads ;
      procedure DecThreads ;
    public
      property Count : Integer read fget_Count ;
  end ;

  { Patrol read.
  }
  TPatrolReadThread = class( TThread )
    private
      oParent : TPatrolRead ;
      currentFile : String ;
    protected
      procedure Execute ; override;
    public
      constructor Create( const _parent : TPatrolRead ) ;
  end ;

  constructor TPatrolRead.Create;
  begin
    inherited ;

    iThreads := 0 ;
    bNotify  := False ;
    bBreak   := False ;
    iEnabled := 0 ;

    oList := TGIS_StringList.Create ;
  end ;

  procedure TPatrolRead.doDestroy;
  begin
    LockThread ;
    try
      oList.Clear ;
      bBreak := True ;
    finally
      UnlockThread ;
    end ;

    while iThreads > 0 do begin
      Sleep( 5 ) ;
    end;
    FreeObject( oList ) ;

    inherited ;
  end ;

  function TPatrolRead.fget_Count : Integer ;
  begin
    LockThread ;
    try
      Result := oList.Count
    finally
      UnlockThread ;
    end ;
  end ;

  function TPatrolRead.Pop : String ;
  begin
    LockThread ;
    try
      if oList.Count > 0 then begin
        Result := oList[0] ;
        oList.Delete(0) ;
      end
      else
        Result := '' ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TPatrolRead.Push( const _path : String ) ;
  var
    idx : Integer ;
    {$IFDEF DCC}
      {$IFDEF MSWINDOWS}
        tmp : String  ;
      {$ENDIF}
    {$ENDIF}
  begin
    if iEnabled = 0 then begin
      if GisMetadataAsBoolean( METADATA_STREAMPATROLREAD, false ) then
        iEnabled := 1
      else
        iEnabled := -1 ;
    end;

    if iEnabled < 1 then
      exit ;

    {$IFDEF DCC}
      {$IFDEF MSWINDOWS}
        if not FileGetSymLinkTarget( _path, tmp ) then
          tmp := _path ;

        tmp := ExtractFileDrive( tmp ) ;

        case GetDriveType( PChar( tmp ) ) of
          DRIVE_REMOVABLE,
          DRIVE_FIXED    : ;
          else exit ;
        end;
      {$ENDIF}
    {$ENDIF}

    LockThread ;
    try
      idx := oList.IndexOf( _path ) ;
      if idx < 0 then // not found
        oList.Add( _path ) ;

      if oList.Count = 1 then begin
        TPatrolReadThread.Create( Self ).Start ;
      end ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TPatrolRead.Delete( const _path : String ) ;
  var
    idx : Integer ;
  begin
    LockThread ;
    try
      idx := oList.IndexOf( _path ) ;
      if idx >= 0  then begin
        oList.Delete( idx ) ;
        bNotify := True ;
      end;
    finally
      UnlockThread ;
    end ;
  end ;

  function TPatrolRead.Present(
    const _path : String
  ) : Boolean ;
  begin
    LockThread ;
    try
      Result := oList.IndexOf( _path ) >= 0 ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TPatrolRead.IncThreads ;
  begin
    LockThread ;
    try
      inc( iThreads ) ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TPatrolRead.DecThreads ;
  begin
    LockThread ;
    try
      dec( iThreads ) ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TPatrolReadThread.Execute ;
  var
    ar  : array[0..65536] of Byte ;
    stm : TGIS_FileStream ;
    len : Int64 ;
  begin
    try
      while oParent.Count > 0 do begin
        currentFile := oParent.Pop ;

        if SafeFileExists( currentFile ) then begin
          try
            stm := TGIS_FileStream.Create( currentFile,
                                           fmOpenRead or
                                           fmShareDenyNone
                                         ) ;
            try
              len := stm.Size ;
              while stm.Position < len do begin
                if oParent.bBreak then
                  exit ;
                if oParent.bNotify then begin
                  if oParent.Present( currentFile ) then
                    break ;
                  oParent.bNotify := False ;
                end;

                {$IFDEF OXYGENE}
                  stm.Read( ar, 65536 ) ;
                {$ELSE}
                  stm.Read( ar[0], 65536 ) ;
                {$ENDIF}
              end ;
            finally
              FreeObject( stm ) ;
            end ;
          except

          end ;
        end ;
      end ;
    finally
      oParent.DecThreads ;
    end;
  end;

  constructor TPatrolReadThread.Create(
    const _parent : TPatrolRead
  ) ;
  begin
    inherited Create( True );
    oParent := _parent ;
    {$IFDEF JAVA}
      Priority := 2;
    {$ELSE}
      {$IFDEF POSIX}
        Priority := 2 ;
      {$ELSE}
        Priority := tpLower ;
      {$ENDIF}
    {$ENDIF}
    FreeOnTerminate := True ;

    oParent.IncThreads ;
  end;


  var
    oPatrolRead : TPatrolRead ;
{$ELSE}
  {$WARNING '### NO PatrolRead on ISLAND'}
{$ENDIF}

procedure patrol_read_kill( _path : String ) ;
begin
  {$IFNDEF ISLAND}
    if not assigned( oPatrolRead ) then
      exit ;
    oPatrolRead.Delete( _path );
  {$ENDIF}
end ;

procedure patrol_read( _path : String ) ;
begin
  {$IFNDEF ISLAND}
    if not assigned( oPatrolRead ) then begin
      oPatrolRead := TPatrolRead.Create ;
    end ;
    oPatrolRead.Push( _path );
  {$ENDIF}
end ;

{$ENDREGION 'TPatrolRead'}

{$REGION 'Utilities'}

const
  PBUFFER_SIZE = 32768  ;
  SBUFFER_SIZE = 16384 ;

  ANSI_LF     = AnsiChar( #10 ) ;
  ANSI_CR     = AnsiChar( #13 ) ;

{$IFDEF MANAGED}
  procedure copy_buf( const _buf1  : TBytes ; const _offset1 : Integer ;
                      const _buf2  : TBytes ; const _offset2 : Integer ;
                      const _count : Integer
                    ) ;
  begin
    GisCopyMemory( _buf1, _offset1, _buf2, _offset2, _count ) ;
  end ;
{$ELSE}
  procedure copy_buf( const _buf1  ; const _offset1 : Integer ;
                      const _buf2  ; const _offset2 : Integer ;
                      const _count : Integer
                    ) ;
  begin
    Move( Pointer( NativeInt( _buf1 ) + _offset1 )^,
          Pointer( NativeInt( _buf2 ) + _offset2 )^,
          _count
        ) ;
  end ;
{$ENDIF}

{$ENDREGION 'Utilities'}

{$REGION 'TGIS_HandleStream'}

{$IFNDEF OXYGENE}
  constructor TGIS_HandleStream.Create(
    _stream : TStream
  ) ;
  begin
    inherited Create ;
    FStream := _stream ;
  end ;

  destructor TGIS_HandleStream.Destroy ;
  begin
    doDestroy ;

    inherited ;
  end ;

  procedure TGIS_HandleStream.doDestroy ;
  begin
    // lowest level class, do nothing
  end ;

  function  TGIS_HandleStream.fget_Position : Int64 ;
  begin
    Result := FStream.Position ;
  end ;

  procedure TGIS_HandleStream.fset_Position ( _value : Int64 ) ;
  begin
    FStream.Position := _value ;
  end ;

  function  TGIS_HandleStream.fget_Size : Int64 ;
  begin
    Result := FStream.Size ;
  end ;

  procedure TGIS_HandleStream.fset_Size ( _value : Int64 ) ;
  begin
    FStream.Size := _value ;
  end ;

  function TGIS_HandleStream.Read(
    var _buffer ;
    _count : LongInt
  ) : LongInt ;
  begin
    Result := FStream.Read( _buffer, _count ) ;
  end ;

  function TGIS_HandleStream.Write(
    const _buffer ;
    _count : LongInt
  ) : LongInt ;
  begin
    Result := FStream.Write( _buffer, _count ) ;
  end ;

  function TGIS_HandleStream.Seek(
    _offset: LongInt;
    _origin: Word
  ): LongInt;
  begin
    Result := FStream.Seek( _offset, _origin ) ;
  end ;

  function TGIS_HandleStream.Seek(
    const _offset: Int64;
    _origin: TSeekOrigin
  ): Int64;
  begin
    Result := FStream.Seek( _offset, _origin ) ;
  end ;
{$ENDIF}

{$ENDREGION 'TGIS_HandleStream'}

{$REGION 'TGIS_MemoryStream'}

{$IFNDEF OXYGENE}

  constructor TGIS_MemoryStream.Create ;
  begin
    inherited Create( TMemoryStream.Create ) ;
  end ;

  destructor TGIS_MemoryStream.Destroy ;
  begin
    doDestroy ;

    inherited ;
  end ;

  procedure TGIS_MemoryStream.doDestroy ;
  begin
    FreeObject( FStream ) ;
  end ;

  function TGIS_MemoryStream.fget_Memory : Pointer ;
  begin
    Result := TMemoryStream(FStream).Memory ;
  end ;

  procedure TGIS_MemoryStream.Clear ;
  begin
    TMemoryStream(FStream).Clear ;
  end ;

{$ELSE}

  constructor TGIS_MemoryStream.Create ;
  begin
    inherited Create ;
    {$IFDEF JAVA}
      FStream := TMemoryStream.Create ;
      RAF := FStream.RAF ;
    {$ELSE}
      FStream := TMemoryStream.Create ;
    {$ENDIF}
  end ;

  constructor TGIS_MemoryStream.Create(
    const _stream : {$IFDEF CLR}Stream{$ELSE}TStream{$ENDIF}
  ) ;
  begin
    inherited Create( _stream ) ;
  end ;

  procedure TGIS_MemoryStream.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_MemoryStream.fget_Memory : array of Byte ;
  begin
    {$IFDEF JAVA}
      Result := TMemoryStream( FStream ).Memory ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result := TMemoryStream( FStream ).Memory ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := MemoryStream( FStream ).GetBuffer ;
    {$ENDIF}
  end ;

  {$IFDEF CLR}
    function TGIS_MemoryStream.fget_CanRead  : Boolean ;
    begin
      Result := FStream.CanRead ;
    end ;

    function TGIS_MemoryStream.fget_CanSeek  : Boolean ;
    begin
      Result := FStream.CanSeek ;
    end ;

    function TGIS_MemoryStream.fget_CanWrite : Boolean ;
    begin
      Result := FStream.CanWrite ;
    end ;
  {$ENDIF}

  function TGIS_MemoryStream.fget_Length : Int64   ;
  begin
    {$IFDEF JAVA}
      Result := FStream.Size;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result := FStream.Size;
    {$ENDIF}
    {$IFDEF CLR}
      Result := FStream.Length ;
    {$ENDIF}
  end ;

  function TGIS_MemoryStream.fget_Position : Int64   ;
  begin
    Result := FStream.Position ;
  end ;

  procedure TGIS_MemoryStream.fset_Position(
    const _i : Int64
  ) ;
  begin
    FStream.Position := _i ;
  end ;

  function TGIS_MemoryStream.Seek(
    _offset : Int64      ;
    _origin : TSeekOrigin
  ) : Int64 ;
  begin
    Result := FStream.Seek( _offset, _origin ) ;
  end ;

  procedure TGIS_MemoryStream.SetLength(
    _value : Int64
  ) ;
  begin
    FStream.SetLength( _value ) ;
  end ;

  function TGIS_MemoryStream.Read(
    _buffer : array of Byte ;
    _offset : Int32         ;
    _count  : Int32
  ) : Int32 ;
  begin
    Result := FStream.Read( _buffer, _offset, _count ) ;
  end ;

  procedure TGIS_MemoryStream.Write(
    _buffer : array of Byte ;
    _offset : Int32         ;
    _count  : Int32
  ) ;
  begin
    FStream.Write( _buffer, _offset, _count ) ;
  end ;

  procedure TGIS_MemoryStream.Flush ;
  begin
    FStream.Flush ;
  end ;

  procedure TGIS_MemoryStream.Clear ;
  begin
    FStream.Dispose ;
    FStream := TMemoryStream.Create ;
  end ;

  procedure TGIS_MemoryStream.SaveToStream(
    _stream : TGIS_MemoryStream
  ) ;
  var
    len : Int64 ;
  begin
    len := fget_Length ;
    if len <> 0 then
      _stream.WriteBuffer( Memory, len ) ;
  end ;

  function TGIS_MemoryStream.Eof : Boolean ;
  begin
    {$IFDEF JAVA}
      Result :=  FStream.Position >= FStream.Size ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result :=  FStream.Position >= FStream.Size ;
    {$ENDIF}
    {$IFDEF CLR}
      Result :=  FStream.Position >= FStream.Length ;
    {$ENDIF}
  end ;

{$ENDIF}

{$ENDREGION 'TGIS_MemoryStream'}

{$REGION 'TGIS_Stream'}

{$IFDEF OXYGENE}
  constructor TGIS_Stream.Create(
    const _stream   : TStream             ;
    const _onread   : TGIS_ReadWriteEvent ;
    const _onwrite  : TGIS_ReadWriteEvent
  ) ;
  begin
    inherited Create( _stream ) ;

    {$IFDEF OXYGENE}
      arBuffer := new Byte[SBUFFER_SIZE] ;
    {$ELSE}
      SetLength( arBuffer, SBUFFER_SIZE ) ;
    {$ENDIF}

    FOnRead  := _onread  ;
    FOnWrite := _onwrite ;

    CodePage := 0 ;
  end ;

  constructor TGIS_Stream.Create(
    const _stream   : TStream
  ) ;
  begin
     inherited Create( _stream ) ;

    {$IFDEF OXYGENE}
      arBuffer := new Byte[SBUFFER_SIZE] ;
    {$ELSE}
      SetLength( arBuffer, SBUFFER_SIZE ) ;
    {$ENDIF}

    CodePage := 0 ;
  end ;
{$ENDIF}

constructor TGIS_Stream.Create(
  const _onread   : TGIS_ReadWriteEvent ;
  const _onwrite  : TGIS_ReadWriteEvent
) ;
begin
  inherited Create ;
  {$IFDEF OXYGENE}
    arBuffer := new Byte[SBUFFER_SIZE] ;
  {$ELSE}
    System.SetLength( arBuffer, SBUFFER_SIZE ) ;
  {$ENDIF}

  FOnRead  := _onread  ;
  FOnWrite := _onwrite ;

  CodePage := 0 ;

  FFixCodePage := False ;
  fixedCodePage := 0 ;
end ;

constructor TGIS_Stream.Create ;
begin
  inherited Create ;

  {$IFDEF OXYGENE}
    arBuffer := new Byte[SBUFFER_SIZE] ;
  {$ELSE}
    System.SetLength( arBuffer, SBUFFER_SIZE ) ;
  {$ENDIF}

  CodePage := 0 ;
end ;

procedure TGIS_Stream.doDestroy ;
begin
  arBuffer := nil ;

  if assigned( layerEncoding ) then
    FreeObject( layerEncoding ) ;

  inherited ;
end ;

procedure TGIS_Stream.fset_CodePage(
  const _value : Integer
) ;
begin
  // setup encoding scheme
  if ( _value <> CodePage ) or
        ( not assigned( layerEncoding )
      ) then
  begin
    if assigned( layerEncoding ) then
      FreeObject( layerEncoding ) ;
    {$IFDEF JAVA}
      FCodePage := _value ;
      try
        var cp := TCodePageConverter.Convert(CodePage) ;
        if not IsStringEmpty(cp) then
          layerEncoding := TEncoding.GetEncoding( cp )
        else begin
          FCodePage := GisSystemCodePage ;
          layerEncoding := TEncoding.GetEncoding( TCodePageConverter.Convert(FCodePage) ) ;
        end ;
      except
        FCodePage := GisSystemCodePage ;
        layerEncoding := TEncoding.GetEncoding( TCodePageConverter.Convert(FCodePage) ) ;
      end ;
    {$ENDIF}
    {$IFDEF ISLAND}
      FCodePage := _value ;
      try
        var cp := TCodePageConverter.Convert(CodePage) ;
        if not IsStringEmpty(cp) then
          layerEncoding := TEncoding.GetEncoding( cp )
        else begin
          FCodePage := GisSystemCodePage ;
          layerEncoding := TEncoding.GetEncoding( TCodePageConverter.Convert(FCodePage) ) ;
        end ;
      except
        FCodePage := GisSystemCodePage ;
        layerEncoding := TEncoding.GetEncoding( TCodePageConverter.Convert(FCodePage) ) ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      FCodePage := _value ;
      try
        layerEncoding := TEncoding.GetEncoding( CodePage ) ;
      except
        FCodePage := GisSystemCodePage ;
        layerEncoding := TEncoding.GetEncoding( FCodePage ) ;
      end ;
      FCodePage := layerEncoding.CodePage ;
    {$ENDIF}
    {$IFDEF DCC}
      FCodePage := _value ;
      try
        layerEncoding := TEncoding.GetEncoding( CodePage ) ;
      except
        FCodePage := GisSystemCodePage ;
        layerEncoding := TEncoding.GetEncoding( FCodePage ) ;
      end ;
      FCodePage := layerEncoding.CodePage ;
    {$ENDIF}
  end ;
end ;

procedure TGIS_Stream.autofixCodePage ;
begin
  if ( not FFixCodePage ) or (  fixedCodePage <> 0 ) then
    exit ;

  // if UTF8 then try lock code page
  // if local codepage then try UTF8
  if CodePage = 65001 then
    CodePage := GisSystemCodePage
  else
  if CodePage = GisSystemCodePage then
    CodePage := 65001 ;
  fixedCodePage := CodePage ;

  autofixCodePage;
end;

function TGIS_Stream.ReadLine
  : String ;
var
  c0  : Byte    ;
  c1  : Byte    ;
  c   : Byte    ;
  cnt : Integer ;

  ascii : Boolean ; // optimization fot NEXTGEN compilator especially
begin
  cnt := 0 ;

  case FCodePage of
    1200 :
      begin
        ascii := False ;
        while ReadByte( c0, 1 ) = 1 do begin
          if ReadByte( c1, 1 ) <> 1 then
            break ;

          if c1 > 0 then begin
            arBuffer[ cnt ] := c0 ;
            inc( cnt ) ;
            arBuffer[ cnt ] := c1 ;
            inc( cnt ) ;
            if cnt >= SBUFFER_SIZE then
              break ;
          end
          else begin
            case c0 of
              ord( #10 ) : begin
                             // end of line
                             break    ;
                           end ;
              ord( #13 ) : begin
                             if ReadByte( c, 1 ) = 1 then begin
                               if c <> ord( #10 ) then begin
                                 // not a CRLF - undo
                                 Position := Position - 1 ;
                               end ;
                             end;
                             break ;
                           end ;
              ord( #0  ) ,
              ord( #26 ) : begin
                             // end of line
                             break    ;
                           end ;
              else         begin
                             arBuffer[ cnt ] := c0 ;
                             inc( cnt ) ;
                             arBuffer[ cnt ] := c1 ;
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                           end ;
            end ;
          end ;
        end;
      end;
    1201 :
      begin
        ascii := False ;
        while ReadByte( c0, 1 ) = 1 do begin
          if ReadByte( c1, 1 ) <> 1 then
            break ;

          if c0 > 0 then begin
            arBuffer[ cnt ] := c0 ;
            inc( cnt ) ;
            arBuffer[ cnt ] := c1 ;
            inc( cnt ) ;
            if cnt >= SBUFFER_SIZE then
              break ;
          end
          else begin
            case c1 of
              ord( #10 ) : begin
                             // end of line
                             break    ;
                           end ;
              ord( #13 ) : begin
                             if ReadByte( c, 1 ) = 1 then begin
                               if c <> ord( #10 ) then begin
                                 // not a CRLF - undo
                                 Position := Position - 1 ;
                               end ;
                             end;
                             break ;
                           end ;
              ord( #0  ) ,
              ord( #26 ) : begin
                             // end of line
                             break    ;
                           end ;
              else         begin
                             arBuffer[ cnt ] := c0 ;
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                             arBuffer[ cnt ] := c1 ;
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                           end ;
            end ;
          end ;
        end;
      end;
    else
      begin
        ascii := True ;
        while ReadByte( c, 1 ) = 1 do begin
          case c of
            ord( #10 ) : begin
                           // end of line
                           break    ;
                         end ;
            ord( #13 ) : begin
                           if ReadByte( c, 1 ) = 1 then begin
                             if c <> ord( #10 ) then begin
                               // not a CRLF - undo
                               Position := Position - 1 ;
                             end ;
                           end;
                           break ;
                         end ;
            ord( #0  ) ,
            ord( #26 ) : begin
                           // end of line
                           break    ;
                         end ;
            else         begin
                           if c > 127 then
                             ascii := False ;

                           arBuffer[ cnt ] := c ;
                           inc( cnt ) ;
                           if cnt >= SBUFFER_SIZE then
                             break ;
                         end ;
          end ;
        end;
      end;
  end ;

  if cnt > 0 then begin
    if ascii then
      Result := ConvertAnsiString( arBuffer, cnt )
    else
      try
        Result := layerEncoding.GetString( arBuffer, 0, cnt ) ;
      except
        autofixCodePage ;
        Result := layerEncoding.GetString( arBuffer, 0, cnt ) ;
      end;
  end
  else
    Result := '' ;
end ;


function TGIS_Stream.ReadToken
  : String ;
var
  c     : Byte    ;
  c0    : Byte    ;
  c1    : Byte    ;
  cnt   : Integer ;

  ascii : Boolean ; // optimization fot NEXTGEN compilator especially
begin
  cnt := 0 ;

  case FCodePage of
    1200 :
      begin
        ascii := False ;
        while ReadByte( c0, 1 ) = 1 do begin
          if ReadByte( c1, 1 ) <> 1 then
            break ;

          if c1 > 0 then begin
            arBuffer[ cnt ] := Byte( c0 );
            inc( cnt ) ;
            arBuffer[ cnt ] := Byte( c1 );
            inc( cnt ) ;
            if cnt >= SBUFFER_SIZE then
              break ;
          end
          else begin
            case c0 of
              ord( #9  ),
              ord( #32 ),
              ord( #10 ),
              ord( #13 ),
              ord( #0  ) : begin
                             if cnt > 0 then
                               break ;
                             cnt := 0 ;
                           end ;
              ord( #26 ) : begin
                             // end of line
                             break ;
                           end ;
              else         begin
                             arBuffer[ cnt ] := c0 ;
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                             arBuffer[ cnt ] := c1 ;
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                           end ;
            end ;
          end ;
        end;
      end;
    1201 :
      begin
        ascii := False ;
        while ReadByte( c0, 1 ) = 1 do begin
          if ReadByte( c1, 1 ) <> 1 then
            break ;

          if c0 > 0 then begin
            arBuffer[ cnt ] := Byte( c0 );
            inc( cnt ) ;
            arBuffer[ cnt ] := Byte( c1 );
            inc( cnt ) ;
            if cnt >= SBUFFER_SIZE then
              break ;
          end
          else begin
            case c1 of
              ord( #9  ),
              ord( #32 ),
              ord( #10 ),
              ord( #13 ),
              ord( #0  ) : begin
                             if cnt > 0 then
                               break ;
                             cnt := 0 ;
                           end ;
              ord( #26 ) : begin
                             // end of line
                             break ;
                           end ;
              else         begin
                             arBuffer[ cnt ] := Byte( c0 );
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                             arBuffer[ cnt ] := Byte( c1 );
                             inc( cnt ) ;
                             if cnt >= SBUFFER_SIZE then
                               break ;
                           end ;
            end ;
          end ;
        end;
      end;
    else
      begin
        ascii := True ;
        while ReadByte( c, 1 ) = 1 do begin
          case c of
            ord( #9  ),
            ord( #32 ),
            ord( #10 ),
            ord( #13 ),
            ord( #0  ) : begin
                           if cnt > 0 then
                             break ;
                           cnt := 0 ;
                         end ;
            ord( #26 ) : begin
                           // end of line
                           break ;
                         end ;
            else         begin
                           if c > 127 then
                             ascii := False ;

                           arBuffer[ cnt ] := Byte( c );
                           inc( cnt ) ;
                           if cnt >= SBUFFER_SIZE then break ;
                         end ;
          end ;
      end;
  end ;


  end ;

  if cnt > 0 then begin
    if ascii then
      Result := ConvertAnsiString( arBuffer, cnt )
    else
      try
        Result := layerEncoding.GetString( arBuffer, 0, cnt ) ;
      except
        autofixCodePage ;
        Result := layerEncoding.GetString( arBuffer, 0, cnt ) ;
      end;
  end
  else
    Result := '' ;
end ;

function TGIS_Stream.WriteLine(
  const _buf : String
) : Integer ;
var
  buf : TBytes ;
  str : String ;
begin
  str := _buf + #13#10 ;

  buf := layerEncoding.GetBytes( str ) ;
  WriteBytes( buf ) ;
  Result := {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( buf ) ;
end ;

function TGIS_Stream.ReadString(
  var   _buf    : String  ;
  const _count  : Integer
) : Integer ;
var
  buf : TBytes ;
begin
  {$IFDEF OXYGENE}
    buf := new Byte[_count] ;
  {$ELSE}
    System.SetLength( buf, _count ) ;
  {$ENDIF}

  if _count > 0 then
    Result := ReadBytesCnt( buf, _count )
  else
    Result := 0 ;


  if Result > 0 then begin
    {$IFNDEF OXYGENE}
       try
         _buf := layerEncoding.GetString( Copy( buf, 0, Result ) ) ;
       except
         autofixCodePage ;
         _buf := layerEncoding.GetString( Copy( buf, 0, Result ) ) ;
       end;
    {$ELSE}
      try
        _buf := layerEncoding.GetString( buf, 0, Result ) ;
      except
        autofixCodePage ;
        _buf := layerEncoding.GetString( buf, 0, Result ) ;
      end;
    {$ENDIF}
  end
  else
    _buf := '' ;
end ;

function TGIS_Stream.ReadString(
  const _count  : Integer
) : String ;
var
  buf : TBytes ;
  res : Integer ;
begin
  {$IFDEF OXYGENE}
    buf := new Byte[_count] ;
  {$ELSE}
    SetLength( buf, _count ) ;
  {$ENDIF}

  res := ReadBytesCnt( buf, _count ) ;

  if res > 0 then begin
    {$IFNDEF OXYGENE}
       try
         Result := layerEncoding.GetString( Copy( buf, 0, res ) ) ;
       except
         autofixCodePage ;
         Result := layerEncoding.GetString( Copy( buf, 0, res ) ) ;
       end;
    {$ELSE}
      try
        Result := layerEncoding.GetString( buf, 0, res ) ;
      except
        autofixCodePage ;
        Result := layerEncoding.GetString( buf, 0, res ) ;
      end;
    {$ENDIF}
  end
  else
    Result := '' ;
end ;

function TGIS_Stream.ReadAsciiString (
  var   _buf    : String  ;
  const _count  : Integer
) : Integer ;
var
  buf : TBytes ;
begin
  {$IFDEF OXYGENE}
    buf := new Byte[_count] ;
  {$ELSE}
    System.SetLength( buf, _count ) ;
  {$ENDIF}

  if _count > 0 then
    Result := ReadBytesCnt( buf, _count )
  else
    Result := 0 ;

  if Result > 0 then begin
    _buf := ConvertAnsiString( buf, Result )
  end
  else
    _buf := '' ;
end ;

function TGIS_Stream.ReadAsciiString (
  const _count  : Integer
) : String ;
var
  buf : TBytes ;
  res : Integer ;
begin
  {$IFDEF OXYGENE}
    buf := new Byte[_count] ;
  {$ELSE}
    SetLength( buf, _count ) ;
  {$ENDIF}

  res := ReadBytesCnt( buf, _count ) ;

  if res > 0 then begin
    Result := ConvertAnsiString( buf, res )
  end
  else
    Result := '' ;
end ;


function TGIS_Stream.WriteString(
  const _buf : String
) : Integer ;
var
  buf : TBytes ;
begin
  if {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( _buf ) <= 0 then begin
    Result := 0 ;
    exit ;
  end ;

  buf := layerEncoding.GetBytes( _buf ) ;
  WriteBytes( buf ) ;
  Result := {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( buf ) ;
end ;

function TGIS_Stream.WriteStringCnt(
  const _buf   : String ;
  const _count : Integer
) : Integer ;
var
  buf  : TBytes  ;
  obuf : TBytes  ;
  i    : Integer ;
begin
  if _count <= 0 then begin
    Result := 0 ;
    exit ;
  end ;

  {$IFDEF OXYGENE}
    obuf := new Byte[_count] ;
  {$ELSE}
    SetLength( obuf, _count ) ;
  {$ENDIF}

  buf := layerEncoding.GetBytes( _buf ) ;

  for i:= 0 to _count - 1 do begin
    if ( i >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( buf ) ) or ( buf[i] = 0 ) then
      obuf[i] := Byte(' ')
    else
      obuf[i] := buf[i] ;
  end ;
  WriteBytes( obuf ) ;

  Result := {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( obuf ) ;
end ;

function TGIS_Stream.WriteVarAsString(
  const _buf   :  {$IFDEF OXYGENE}
                    TObject ;
                  {$ELSE}
                    Variant ;
                  {$ENDIF}
  const _count : Integer
) : Integer ;
var
  buf  : TBytes  ;
  obuf : TBytes  ;
  i    : Integer ;
begin
  if _count <= 0 then begin
    Result := 0 ;
    exit ;
  end ;

  {$IFDEF OXYGENE}
    obuf := new Byte[_count] ;
  {$ELSE}
    SetLength( obuf, _count ) ;
  {$ENDIF}

  {$IFDEF JAVA}
    buf := layerEncoding.GetBytes( VarToString( _buf ) ) ;
  {$ELSE}
    {$IFDEF CLR}
      if assigned( _buf ) then
        buf := layerEncoding.GetBytes( _buf.ToString() )
    {$ELSE}
      if not VarIsEmpty( _buf ) then
        buf := layerEncoding.GetBytes( String( _buf ) )
    {$ENDIF}
      else
        buf := nil ;
  {$ENDIF}

    for i:= 0 to _count - 1 do begin
      if ( i >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( buf ) ) or ( buf[i] = 0 ) then
         obuf[i] := Byte(' ')
      else
        obuf[i] := buf[i] ;
    end ;
    WriteBytes( obuf ) ;

  Result := _count
end ;

function TGIS_Stream.ReadBytesCnt(
  const _buf   : TBytes  ;
  const _count : Integer
) : Integer ;
begin
  {$IFDEF OXYGENE}
    Result := Read( _buf   , _count ) ;
  {$ELSE}
    Result := Read( _buf[0], _count ) ;
  {$ENDIF}
end ;

function TGIS_Stream.ReadBytesCnt( var   _buf      : TBytes  ;
                                   const _count    : Integer ;
                                   const _offset   : Integer
                                  ) : Integer ;
{$IFDEF OXYGENE}
  var
    tmpBuf : TBytes ;
{$ENDIF}
begin
  {$IFDEF OXYGENE}
    Result := ReadBytesSafe( tmpBuf, _count ) ;
    GisCopyMemory( tmpBuf, 0, _buf, _offset, _count ) ;
  {$ELSE}
    Result := Read( _buf[_offset], _count ) ;
  {$ENDIF}
end ;

function TGIS_Stream.ReadBytesSafe(
  var   _buf   : TBytes  ;
  const _count : Integer
) : Integer ;
begin
  {$IFDEF OXYGENE}
    _buf := new Byte[_count] ;
  {$ELSE}
    SetLength( _buf, _count ) ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    Result := Read( _buf   , _count ) ;
  {$ELSE}
    Result := Read( _buf[0], _count ) ;
  {$ENDIF}
end ;

procedure TGIS_Stream.WriteBytes(
  const _buf : TBytes
) ;
begin
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      if assigned( _buf ) then
        WriteBuffer( _buf, _buf.length ) ;
    {$ELSE}
      WriteBuffer( _buf   , RemObjects.Elements.System.length( _buf ) ) ;
    {$ENDIF}
  {$ELSE}
    WriteBuffer( _buf[0], length( _buf ) ) ;
  {$ENDIF}
end ;

procedure TGIS_Stream.WriteBytesCnt(
  const _buf   : TBytes  ;
  const _count : Integer
) ;
begin
  {$IFDEF OXYGENE}
    {$IFDEF JAVA}
      WriteBuffer( _buf, Min(_buf.length, _count) ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      WriteBuffer( _buf, Min(_buf.Length, _count) ) ;
    {$ENDIF}
    {$IFDEF CLR}
      WriteBuffer( _buf, Min( RemObjects.Elements.System.length( _buf ), _count ) ) ;
    {$ENDIF}
  {$ELSE}
    WriteBuffer( _buf[0], Min( length( _buf ), _count ) ) ;
  {$ENDIF}
end ;

{$ENDREGION 'TGIS_Stream'}

{$REGION 'TGIS_FileStream'}

{$IFNDEF OXYGENE}

  procedure TGIS_FileStream.fset_PatrolRead(
    const _val : Boolean
  ) ;
  begin
    {$IFDEF MACOS}
      exit ; {$MESSAGE WARN 'Verify what to do on OSX/IOS' }
    {$ENDIF} ;
    {$IFDEF ANDROID}
      exit ; {$MESSAGE WARN 'Verify what to do on Android' }
    {$ENDIF} ;
    {$IFDEF LINUX}
      exit ; {$MESSAGE WARN 'Verify what to do on Linux' }
    {$ENDIF} ;

    // do not do start patroll read on small files
    if Size < 65536 then
      exit ;

    if FPatrolRead = _val then exit ;

    FPatrolRead := _val ;
    if _val then
      patrol_read( FName )
    else
      patrol_read_kill( FName ) ;
  end ;

  procedure TGIS_FileStream.doDestroy ;
  begin
    if FPatrolRead then begin
      FPatrolRead := False ;
      patrol_read_kill( FName ) ;
    end;
    FreeObject( FStream ) ;
  end ;

  constructor TGIS_FileStream.Create(const AFileName: String; Mode: Word);
  begin
    {$IFDEF WINDOWS}
      if ( ( Mode and fmShareExclusive ) = 0 ) and
         ( ( Mode and fmShareDenyWrite ) = 0 ) and
         ( ( Mode and fmShareDenyRead  ) = 0 ) and
         ( ( Mode and fmShareDenyNone  ) = 0 )
      then begin
        Mode := Mode or fmShareDenyNone ;
      end ;
    {$ELSE}
      if ( ( Mode and fmShareExclusive ) = 0 ) and
         ( ( Mode and fmShareDenyWrite ) = 0 ) and
         ( ( Mode and fmShareDenyNone  ) = 0 )
      then begin
        Mode := Mode or fmShareDenyNone ;
      end ;
    {$ENDIF}

    inherited Create( TFileStream.Create( AFileName, Mode ) ) ;

    FName := AFileName ;
    FMode := Mode      ;
    FPatrolRead := False ;
  end ;

  constructor TGIS_FileStream.Create(const AFileName: String; Mode: Word; Rights: Cardinal);
  begin
    {$IFDEF WINDOWS}
      if ( ( Mode and fmShareExclusive ) = 0 ) and
         ( ( Mode and fmShareDenyWrite ) = 0 ) and
         ( ( Mode and fmShareDenyRead  ) = 0 ) and
         ( ( Mode and fmShareDenyNone  ) = 0 )
      then begin
        Mode := Mode or fmShareDenyNone ;
      end ;
    {$ELSE}
      if ( ( Mode and fmShareExclusive ) = 0 ) and
         ( ( Mode and fmShareDenyWrite ) = 0 ) and
         ( ( Mode and fmShareDenyNone  ) = 0 )
      then begin
        Mode := Mode or fmShareDenyNone ;
      end ;
    {$ENDIF}

    inherited Create( TFileStream.Create( AFileName, Mode, Rights ) ) ;

    FName := AFileName ;
    FMode := Mode      ;
    FPatrolRead := False ;
  end ;

  destructor TGIS_FileStream.Destroy ;
  begin
    doDestroy ;

    inherited ;
  end ;

{$ELSE}

  procedure TGIS_FileStream.fset_PatrolRead(
    const _val : Boolean
  ) ;
  begin
    {$IFNDEF JAVA}
      {$IFNDEF MSWINDOWS_OS}
        exit ; {$MESSAGE WARN 'Verify what to do on OSX/IOS' }
      {$ENDIF} ;
    {$ENDIF}

    if FPatrolRead = _val then exit ;

    FPatrolRead := _val ;
    {$IFNDEF ISLAND}
      if _val then
        patrol_read( FName )
      else
        patrol_read_kill( FName ) ;
    {$ENDIF}
  end ;

  {$IFDEF ISLAND}
  {$ELSE}
  {$IFDEF JAVA}
    procedure TGIS_FileStream.translateMode(
      const _vcl  : UInt32     ;
        out _mode : String
    ) ;
  {$ENDIF}
  {$IFDEF CLR}
    procedure TGIS_FileStream.translateMode(
      const _vcl : UInt32     ;
        out _net : FileMode   ;
        out _acc : FileAccess ;
        out _shr : FileShare
    ) ;
  {$ENDIF}
  var
    vcl_mode   : UInt32 ;
    vcl_share  : UInt32 ;
  begin
    vcl_mode  := _vcl and $0000FF0F ;
    vcl_share := _vcl and $000000F0 ;

    case vcl_mode of
      fmCreate :
        begin
          {$IFDEF CLR}
            _net := FileMode.OpenOrCreate ;
            _acc := FileAccess.ReadWrite  ;
          {$ELSE}
            _mode := 'rw'                 ;
          {$ENDIF}
        end ;
      fmOpenWrite :
        begin
         {$IFDEF CLR}
           _net := FileMode.Truncate     ;
           _acc := FileAccess.Write      ;
         {$ELSE}
           _mode := 'rw'                 ;
         {$ENDIF}
        end ;
      fmOpenRead :
        begin
          {$IFDEF CLR}
            _net := FileMode.Open         ;
            _acc := FileAccess.Read       ;
          {$ELSE}
            _mode := 'r'                  ;
          {$ENDIF}
        end ;
      fmOpenReadWrite :
        begin
          {$IFDEF CLR}
            _net := FileMode.Open         ;
            _acc := FileAccess.ReadWrite  ;
          {$ELSE}
            _mode := 'rw'                 ;
          {$ENDIF}
         end ;
      else
        begin  // same as fmOpenRead
          {$IFDEF CLR}
            _net := FileMode.Open         ;
            _acc := FileAccess.Read       ;
          {$ELSE}
            _mode := 'r'                  ;
          {$ENDIF}
        end ;
    end ;

    case vcl_share of
      fmShareExclusive :
        begin
          {$IFDEF CLR}
             _shr := FileShare.None       ;
          {$ELSE}
          {$ENDIF}
        end ;
      fmShareDenyWrite :
        begin
          {$IFDEF CLR}
            _shr := FileShare.Read        ;
          {$ELSE}
          {$ENDIF}
        end ;
      fmShareDenyRead :
        begin
          {$IFDEF CLR}
            _shr := FileShare.Write       ;
          {$ELSE}
          {$ENDIF}
        end ;
      fmShareDenyNone :
        begin
          {$IFDEF CLR}
            _shr := FileShare.ReadWrite   ;
          {$ELSE}
          {$ENDIF}
        end ;
      else
        begin // same as fmShareExclusive
          {$IFDEF CLR}
            _shr := FileShare.None        ;
          {$ELSE}
          {$ENDIF}
        end ;
    end ;
  end ;
  {$ENDIF}

  constructor TGIS_FileStream.Create(
    const _name : String ;
    const _mode : UInt32
  ) ;
  var
  {$IFDEF JAVA}
    mode   : String   ;
  {$ENDIF}
  {$IFDEF ISLAND}
    mode   : String   ;
  {$ENDIF}
  {$IFDEF CLR}
    mode   : FileMode   ;
    access : FileAccess ;
    share  : FileShare  ;
  {$ENDIF}
  begin
    inherited Create ;

    FName := _name ;

    if ( ( _mode and fmShareExclusive ) = 0 ) and
       ( ( _mode and fmShareDenyWrite ) = 0 ) and
       ( ( _mode and fmShareDenyRead  ) = 0 ) and
       ( ( _mode and fmShareDenyNone  ) = 0 )
    then
      FMode := _mode or fmShareDenyNone
    else
      FMode := _mode ;

    FPatrolRead := False ;
    {$IFDEF JAVA}
      translateMode( FMode, mode ) ;
      FStream := TFileStream.Create( new RandomAccessFile( FName , mode ) ) ;
      RAF := FStream.RAF ;
    {$ENDIF}
    {$IFDEF CLR}
      translateMode( FMode, mode, access, share ) ;
      FStream := TFileStream.Create( _name, mode, access, share ) ;
    {$ENDIF}
    {$IFDEF ISLAND}
      {$WARNING '### Verify ISLAND code'}
    {$ENDIF}
  end ;

  procedure TGIS_FileStream.doDestroy ;
  begin
    if FPatrolRead then begin
      FPatrolRead := False ;
      {$IFNDEF ISLAND}
        patrol_read_kill( FName ) ;
      {$ENDIF}
    end ;

    inherited ;
  end ;

  {$IFDEF CLR}
    function TGIS_FileStream.fget_CanRead  : Boolean ;
    begin
      {$IFDEF JAVA}
        {$WARNING '### Verify JAVA code'}
      {$ELSE}
      Result := FStream.CanRead ;
      {$ENDIF}
    end ;

    function TGIS_FileStream.fget_CanSeek  : Boolean ;
    begin
      {$IFDEF JAVA}
        {$WARNING '### Verify JAVA code'}
      {$ELSE}
      Result := FStream.CanSeek ;
      {$ENDIF}
    end ;

    function TGIS_FileStream.fget_CanWrite : Boolean ;
    begin
      {$IFDEF JAVA}
        {$WARNING '### Verify JAVA code'}
      {$ELSE}
      Result := FStream.CanWrite ;
      {$ENDIF}
    end ;
  {$ENDIF}

  function TGIS_FileStream.fget_Length : Int64   ;
  begin
    {$IFDEF JAVA}
      Result := FStream.Size ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result := FStream.Size ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := FStream.Length ;
    {$ENDIF}
  end ;

  function TGIS_FileStream.fget_Position : Int64   ;
  begin
    Result := FStream.Position ;
  end ;

  procedure TGIS_FileStream.fset_Position(
    const _i : Int64
  ) ;
  begin
    FStream.Position := _i ;
  end ;

  function TGIS_FileStream.Seek(
    _offset : Int64      ;
    _origin : TSeekOrigin
  ) : Int64 ;
  begin
    Result := FStream.Seek( _offset, _origin ) ;
  end ;

  procedure TGIS_FileStream.SetLength(
    _value : Int64
  ) ;
  begin
    FStream.SetLength( _value ) ;
  end ;

  function TGIS_FileStream.Read(
    _buffer : array of Byte ;
    _offset : Int32         ;
    _count  : Int32
  ) : Int32 ;
  begin
    Result := FStream.Read( _buffer, _offset, _count ) ;
  end ;

  procedure TGIS_FileStream.Write(
    _buffer : array of Byte ;
    _offset : Int32         ;
    _count  : Int32
  ) ;
  begin
    FStream.Write( _buffer, _offset, _count ) ;
  end ;

  procedure TGIS_FileStream.Flush ;
  begin
    FStream.Flush ;
  end ;

  function TGIS_FileStream.Eof : Boolean ;
  begin
    {$IFDEF JAVA}
      Result :=  FStream.Position >= FStream.Size ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result :=  FStream.Position >= FStream.Size ;
    {$ENDIF}
    {$IFDEF CLR}
      Result :=  FStream.Position >= FStream.Length ;
    {$ENDIF}
  end ;

{$ENDIF}

{$ENDREGION 'TGIS_FileStream'}

{$REGION 'TGIS_BufferedStream'}

  constructor TGIS_BufferedStream.Create(
    const _stream   : TStream             ;
    const _onread   : TGIS_ReadWriteEvent ;
    const _onwrite  : TGIS_ReadWriteEvent
  ) ;
  begin
    {$IFNDEF OXYGENE}
      inherited Create( _onread, _onwrite )  ;

      doCreate( _stream, _onread, _onwrite ) ;
    {$ELSE}
      inherited Create( _stream, _onread, _onwrite )  ;

      doCreate( _stream, _onread, _onwrite ) ;
    {$ENDIF}
  end ;

 constructor TGIS_BufferedStream.Create(
   const _stream : TStream
 ) ;
 begin
   {$IFNDEF OXYGENE}
     inherited Create ;

     doCreate( _stream, nil, nil ) ;
   {$ELSE}
     inherited Create( _stream ) ;

     doCreate( _stream, nil, nil ) ;
   {$ENDIF}
 end ;

 {$IFDEF OXYGENE}
   constructor TGIS_BufferedStream.Create(
     const _onread   : TGIS_ReadWriteEvent ;
     const _onwrite  : TGIS_ReadWriteEvent
   ) ;
   begin
     inherited Create( _onread, _onwrite ) ;
   end ;

   constructor TGIS_BufferedStream.Create ;
   begin
     inherited Create ;
   end ;
 {$ENDIF}

 procedure TGIS_BufferedStream.doDestroy ;
 begin
   arReadBuffer  := nil ;
   arWriteBuffer := nil ;

   inherited ;
 end ;

procedure TGIS_BufferedStream.doCreate(
  const _stream   : TStream             ;
  const _onread   : TGIS_ReadWriteEvent ;
  const _onwrite  : TGIS_ReadWriteEvent
) ;
begin
  {$IFNDEF OXYGENE}
    FStream   := _stream ;
  {$ENDIF}
  FPosition   := -1   ;
  oldPosition := -1   ;
  FSize       := -1   ;
  FFileName   := ''   ;
  iReadBufferPos  := 0 ;
  iReadBufferSize := 0 ;

  arReadBuffer  := nil ;
  arWriteBuffer := nil ;

  if assigned( _stream ) then begin
    FPosition := _stream.Position ;
    FSize     := _stream.Size     ;
  end;

end ;

{$IFDEF OXYGENE}
  {$IFDEF CLR}
    function TGIS_BufferedStream.fget_CanRead  : Boolean ;
    begin
      Result := FStream.CanRead ;
    end ;

    function TGIS_BufferedStream.fget_CanSeek  : Boolean ;
    begin
      Result := FStream.CanSeek ;
    end ;

    function TGIS_BufferedStream.fget_CanWrite : Boolean ;
    begin
      Result := FStream.CanWrite ;
    end ;
  {$ENDIF}

  function TGIS_BufferedStream.fget_Length : Int64   ;
  begin
    {$IFDEF JAVA}
      Result := FStream.Size ;
    {$ENDIF}
    {$IFDEF ISLAND}
      Result := FStream.Size ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := FStream.Length ;
    {$ENDIF}
  end ;

  procedure TGIS_BufferedStream.SetLength(
    _value : Int64
  ) ;
  begin
    FStream.SetLength( _value ) ;
  end ;

  procedure TGIS_BufferedStream.Flush ;
  begin
    FStream.Flush ;
  end ;

{$ENDIF}

{$IFDEF MANAGED}
  function TGIS_BufferedStream.Read
    {$IFNDEF OXYGENE}
       ( var   _buffer : array of Byte ;
               _offset : LongInt       ;
               _count  : LongInt
       ) : LongInt ;
    {$ELSE}
       (
               _buffer : array of Byte ;
               _offset : Int32       ;
               _count  : Int32
       ) : Int32 ;
    {$ENDIF}
{$ELSE}
  function TGIS_BufferedStream.Read(
    var _buffer           ;
        _count  : LongInt
  ) : LongInt ;
{$ENDIF}
var
  {$IFDEF MANAGED}
    poutbuf  : TBytes  ;
  {$ELSE}
    poutbuf  : Pointer ;
  {$ENDIF}
  ioutbufpos : LongInt ;
  iunread    : LongInt ;
  iavailable : LongInt ;
  ireadnow   : LongInt ;
  oldpos     : Int64   ;

  function read_chunk : Boolean ;
  begin
    {$IFDEF OXYGENE}
      iReadBufferSize := FStream.Read( arReadBuffer, PBUFFER_SIZE ) ;
    {$ELSE}
      iReadBufferSize := FStream.Read( arReadBuffer[0], PBUFFER_SIZE ) ;
    {$ENDIF}
    FPosition   := FPosition + iReadBufferSize ;
    iReadBufferPos  := 0 ;
    Result := iReadBufferSize <> iReadBufferPos ;
  end ;
begin
  Result := 0 ;

  if not assigned( arReadBuffer ) then
    {$IFDEF OXYGENE}
      arReadBuffer := new Byte[PBUFFER_SIZE] ;
    {$ELSE}
      SetLength( arReadBuffer, PBUFFER_SIZE ) ;
    {$ENDIF}

  if iWriteBufferSize > 0 then
    flushWrite ;

  oldPosition := 0 ;
  if assigned( FOnRead ) then
    oldpos      := fget_Position ;

  if _count = 1 then begin

    if iReadBufferPos = iReadBufferSize then begin
      if not read_chunk then exit;
    end ;

    {$IFDEF OXYGENE}
      _buffer[0] := arReadBuffer[iReadBufferPos] ;
    {$ELSE}
      Byte(_buffer) := arReadBuffer[iReadBufferPos] ;
    {$ENDIF}
    inc( iReadBufferPos ) ;
    Result := _count ;

  end
  else if _count >= PBUFFER_SIZE then begin

    oldpos           := Position  ;
    FStream.Position := oldpos    ;
    {$IFNDEF OXYGENE}
      Result := FStream.Read( _buffer, _count ) ;
    {$ELSE}
      Result := FStream.Read( _buffer, 0, _count ) ;
    {$ENDIF}
    Position  :=  FStream.Position ;
    iReadBufferPos  := 0 ;
    iReadBufferSize := 0 ;

  end
  else begin

    {$IFDEF MANAGED}
      poutbuf  := _buffer ;
      ioutbufpos := _offset ;
    {$ELSE}
      poutbuf  := @_buffer ;
      ioutbufpos := 0 ;
    {$ENDIF}

    iunread := _count ;

    if assigned( FOnRead ) then
      oldpos := fget_Position ;

    while iunread > 0 do begin
      // fill buffer if required
      if iReadBufferPos = iReadBufferSize then begin
        if not read_chunk then exit;
      end ;

      iavailable := iReadBufferSize - iReadBufferPos ;
      if iavailable > iunread then
        ireadnow := iunread
      else
        ireadnow := iavailable ;

      {$IFDEF JAVA}
        System.arraycopy( arReadBuffer, iReadBufferPos, poutbuf, ioutbufpos, ireadnow ) ;
      {$ELSE}
        copy_buf( arReadBuffer, iReadBufferPos,
                  poutbuf     , ioutbufpos,
                  ireadnow
                ) ;
      {$ENDIF}

      iReadBufferPos := iReadBufferPos + ireadnow ;
      ioutbufpos := ioutbufpos + ireadnow ;
      iunread    := iunread    - ireadnow ;
      Result     := Result     + ireadnow ;
    end ;

  end ;

  if assigned( FOnRead ) then
    {$IFDEF OXYGENE}
      FOnRead( self,
                TGIS_ReadWriteEventArgs.Create( oldpos, _buffer, Result )
              ) ;
    {$ELSE}
      FOnRead( self, oldpos, @_buffer, _count ) ;
    {$ENDIF}
end ;

{$IFDEF MANAGED}
  {$IFNDEF OXYGENE}
    function TGIS_BufferedStream.Write(
      const _buffer : array of Byte ;
            _offset : LongInt       ;
            _count  : LongInt
    ) : LongInt ;
  {$ELSE}
    procedure TGIS_BufferedStream.Write(
      const _buffer : array of Byte ;
            _offset : LongInt       ;
            _count  : LongInt
    ) ;
  {$ENDIF}
{$ELSE}
  function TGIS_BufferedStream.Write(
    const _buffer ;
          _count  : LongInt
  ): LongInt ;
{$ENDIF}
var
  tmpbuf : TBytes ;
  {$IFDEF MANAGED}
    poutbuf : TBytes  ;
  {$ELSE}
    poutbuf : Pointer ;
  {$ENDIF}
  res : LongInt ;
begin
  if not assigned( arWriteBuffer ) then
    {$IFDEF OXYGENE}
      arWriteBuffer := new Byte[PBUFFER_SIZE] ;
    {$ELSE}
      SetLength( arWriteBuffer, PBUFFER_SIZE ) ;
    {$ENDIF}

  iReadBufferPos  := 0 ;
  iReadBufferSize := 0 ;

  if FPosition <> oldPosition then
    FStream.Position := FPosition ;

  if assigned( FOnWrite ) then begin
    {$IFDEF OXYGENE}
      tmpbuf :=  new Byte[_count] ;
    {$ELSE}
      SetLength( tmpbuf, _count ) ;
    {$ENDIF}
    try
      {$IFDEF MANAGED}
        poutbuf :=  _buffer ;
      {$ELSE}
        poutbuf := @_buffer ;
      {$ENDIF}

      copy_buf( poutbuf, 0, tmpbuf , 0, _count ) ;

      {$IFDEF OXYGENE}
        FOnWrite( self,
                  TGIS_ReadWriteEventArgs.Create( FPosition, tmpbuf, _count )
                ) ;
      {$ELSE}
        FOnWrite( self, FPosition, tmpbuf, _count ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        res := writeBuf( tmpbuf, _count ) ;
      {$ELSE}
        res := writeBuf( tmpbuf[0], _count ) ;
      {$ENDIF}
    finally
      tmpbuf := nil ;
    end ;
  end
  else begin
    res := writeBuf( _buffer, _count ) ;
  end ;

  if FPosition + res > FSize then
    FSize := FPosition + res ;

  FPosition := FPosition + res ;
  oldPosition := FPosition ;

  {$IFNDEF OXYGENE}
    Result := res ;
  {$ENDIF}
end ;

function TGIS_BufferedStream.Seek(
  const _offset : Int64       ;
        _origin : TSeekOrigin
) : Int64 ;
var
  ipos : Int64 ;
begin
  case _origin of
    soBeginning     : ipos := _offset;
    soCurrent       : ipos := Position  + _offset ;
    soEnd           : ipos := Size      - _offset ;
    else              begin
                        assert( False ) ;
                        ipos := 0 ;
                      end ;
  end ;

  flushWrite ;

  // seek within buffer
  if ( ipos < ( FPosition - iReadBufferSize ) ) or ( ipos > FPosition ) then begin
    // seek out of the buffer
    FStream.Position := ipos ;
    iReadBufferPos  := 0 ;
    iReadBufferSize := 0 ;
    FPosition   := ipos ;
  end
  else begin
    // seek within the buffer
    iReadBufferPos := ipos - ( FPosition - iReadBufferSize ) ;
  end ;

  Result := ipos ;
end ;

procedure TGIS_BufferedStream.flushWrite ;
begin
  if iWriteBufferSize > 0 then begin
    {$IFDEF OXYGENE}
      FStream.Write( arWriteBuffer, 0, iWriteBufferSize ) ;
    {$ELSE}
      FStream.Write( arWriteBuffer[0], iWriteBufferSize ) ;
    {$ENDIF}
    iReadBufferPos := 0 ;
    iReadBufferSize := 0 ;
    iWriteBufferSize := 0 ;
  end ;
end ;

{$IFDEF MANAGED}
  function TGIS_BufferedStream.writeBuf(
    const _buffer : array of Byte ;
          _count  : LongInt
  ) : LongInt;
{$ELSE}
  function TGIS_BufferedStream.writeBuf(
    const _buffer ;
          _count  : LongInt
  ) : LongInt ;
{$ENDIF}
var
  ilen1 : Integer ;
  ilen2 : Integer ;
  {$IFDEF MANAGED}
    poutbuf : TBytes ;
  {$ELSE}
    poutbuf : Pointer ;
  {$ENDIF}
begin
  if _count <= 0 then begin
    Result := 0 ;
    exit ;
  end ;

  Result := _count ;

  {$IFDEF MANAGED}
    poutbuf :=  _buffer ;
  {$ELSE}
    poutbuf := @_buffer ;
  {$ENDIF}

  if _count > PBUFFER_SIZE * 2 div 3 then begin
    flushWrite ;
    {$IFNDEF OXYGENE}
      FStream.Write( _buffer, _count ) ;
    {$ELSE}
      FStream.Write( _buffer, 0, _count ) ;
    {$ENDIF}
    exit ;
  end ;

  ilen1 := Min( _count, PBUFFER_SIZE - iWriteBufferSize ) ;

  assert( ilen1 > 0 ) ;

  copy_buf( poutbuf      , 0               ,
            arWriteBuffer, iWriteBufferSize,
            ilen1
          ) ;

  iWriteBufferSize := iWriteBufferSize + ilen1 ;

  assert( iWriteBufferSize <= PBUFFER_SIZE ) ;
  if iWriteBufferSize = PBUFFER_SIZE then
    flushWrite ;

  ilen2 := _count - ilen1 ;
  if ilen2 <= 0 then exit ;

  copy_buf( poutbuf      , ilen1           ,
            arWriteBuffer, iWriteBufferSize,
            ilen2
          ) ;

  iWriteBufferSize := iWriteBufferSize + ilen2 ;

  if iWriteBufferSize = PBUFFER_SIZE then
    flushWrite ;

end ;

function TGIS_BufferedStream.fget_Position
  : Int64 ;
begin
  Result := FPosition - ( iReadBufferSize - iReadBufferPos ) ;
end ;

procedure TGIS_BufferedStream.fset_Position(
  const _value : Int64
) ;
begin
  Seek( _value, soBeginning ) ;
end ;

{$IFDEF OXYGENE}
  procedure TGIS_BufferedStream.SetSize(
    _value : Int64
  ) ;
  begin
  end ;
{$ELSE}
{$ENDIF}

procedure TGIS_BufferedStream.FlushBuffer ;
begin
  flushWrite ;

  FStream.Position := 0 ;
  iReadBufferPos  := 0 ;
  iReadBufferSize := 0 ;
  FPosition   := 0 ;
end ;

function TGIS_BufferedStream.Eof
  : Boolean ;
begin
  Result := ( (FPosition - ( iReadBufferSize - iReadBufferPos )) >= FSize ) ;
end ;

{$ENDREGION 'TGIS_BufferedStream'}

{$REGION 'TGIS_BufferedFileStream'}

constructor TGIS_BufferedFileStream.Create(
  const _filename : String               ;
  const _mode     : TGIS_StreamMode      ;
  const _onread   : TGIS_ReadWriteEvent  ;
  const _onwrite  : TGIS_ReadWriteEvent
) ;
begin
  {$IFNDEF OXYGENE}
    inherited Create( nil, _onread, _onwrite )  ;

    doCreate( _filename, _mode, _onread, _onwrite ) ;
  {$ELSE}
    inherited Create( _onread, _onwrite ) ;

    doCreate( _filename, _mode, nil, nil ) ;
  {$ENDIF}
end ;

constructor TGIS_BufferedFileStream.Create(
  const _filename : String          ;
  const _mode     : TGIS_StreamMode
) ;
begin
  {$IFNDEF OXYGENE}
    inherited Create( nil )  ;

    doCreate( _filename, _mode, nil, nil ) ;
  {$ELSE}
    inherited Create ;

    doCreate( _filename, _mode, nil, nil ) ;
  {$ENDIF}
end ;

procedure TGIS_BufferedFileStream.doDestroy ;
begin
  flushWrite ;

  {$IFNDEF OXYGENE}
    FreeObject( FStream ) ;
  {$ENDIF}

  inherited ;
end ;

procedure TGIS_BufferedFileStream.doCreate(
  const _filename : String              ;
  const _mode     : TGIS_StreamMode     ;
  const _onread   : TGIS_ReadWriteEvent ;
  const _onwrite  : TGIS_ReadWriteEvent
) ;
begin
  FFileName := _filename ;

  try
    if _mode = TGIS_StreamMode.&Create then begin
      FStream := TGIS_FileStream.Create(
                    FFileName,
                    fmCreate or
                    fmShareExclusive
                  )
    end
    else if _mode = TGIS_StreamMode.Append then begin
      FStream := TGIS_FileStream.Create(
                    FFileName,
                    fmOpenReadWrite or
                    fmShareDenyNone
                  ) ;
    end
    else begin
      FStream := TGIS_FileStream.Create(
                    FFileName,
                    fmOpenRead or
                    fmShareDenyNone
                  ) ;
      TGIS_FileStream(FStream).PatrolRead := True ;
    end ;
  except
    {$IFNDEF OXYGENE}
      {$IFDEF GIS_PDK}
        on e : Exception  do begin
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_FILEMAPPING ) + ', ' + e.Message,
                  FFileName, 0
                ) ;
        end ;
      {$ELSE}
        raise EFOpenError.CreateFmt(
                _rsrc( GIS_RS_ERR_FILEMAPPING ) + ', ' + SystemErrorMessage,
                [ FFileName, 0 ]
              ) ;
      {$ENDIF}
    {$ELSE}
      on e : Exception  do begin
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_FILEMAPPING ) + ', ' + e.Message,
                FFileName, 0
              ) ;
      end ;
    {$ENDIF}
  end ;

  FSize        := FStream.Size ;
  {$IFDEF JAVA}
    RAF        := FStream.RAF ;
  {$ENDIF}
  FPosition    := 0 ;
  oldPosition  := 0 ;
end ;

{$ENDREGION 'TGIS_BufferedFileStream'}

{$REGION 'TGIS_TemporaryFileStream'}

constructor TGIS_TemporaryFileStream.Create(
  const _file : String
) ;
begin
  inherited Create ;

  dataFile := TGIS_BufferedFileStream.Create(
                GetTempFileName,
                TGIS_StreamMode.&Create
              ) ;
  dataFile.FlushBuffer ;

  items   := TDictionary<Int64,Int64>.Create ;

  {$IFDEF OXYGENE}
    bufPtg    := new Byte[SIZEOF_TGIS_POINT] ;
    bufPtg3D  := new Byte[SIZEOF_TGIS_POINT3D] ;
  {$ELSE}
    SetLength( bufPtg, SIZEOF_TGIS_POINT ) ;
    SetLength( bufPtg3D, SIZEOF_TGIS_POINT3D ) ;
  {$ENDIF}
end ;

procedure TGIS_TemporaryFileStream.doDestroy ;
var
  tempname : String ;
begin
  Clear ;
  FreeObject( items ) ;

  tempname := dataFile.Path ;
  FreeObject( dataFile ) ;
  {$IFDEF OXYGENE}
    DeleteFile( tempname ) ;
  {$ELSE}
    System.SysUtils.DeleteFile( tempname ) ;
  {$ENDIF}

  bufPtg   := nil ;
  bufPtg3D := nil ;

  inherited ;
end ;

procedure TGIS_TemporaryFileStream.AddFeature(
  const _id    : Int64 ;
  const _data  : TBytes
) ;
var
  size : Integer ;
  off  : Int64 ;
begin
  off   := dataFile.Position ;
  size  := length( _data ) ;
  assert( size > 0 ) ;

  {$IFNDEF OXYGENE}
    dataFile.Write( size, sizeOf( size ) ) ;
  {$ELSE}
    dataFile.WriteInteger( size ) ;
  {$ENDIF}
  {$IFDEF OXYGENE}
     dataFile.WriteBuffer( _data, length( _data ) ) ;
  {$ELSE}
     dataFile.WriteBuffer( _data[ 0 ], length( _data ) ) ;
  {$ENDIF}

  items.Add( _id, off );
end ;

procedure TGIS_TemporaryFileStream.AddFeature(
  const _id    : Int64 ;
  const _data  : TGIS_Point
) ;
begin
  {$IFDEF MANAGED}
     GisCopyMemory( BitConverter.GetBytes( _data.X ), 0,
                              bufPtg, 0, 8 ) ;
     GisCopyMemory( BitConverter.GetBytes( _data.Y ), 0,
                              bufPtg, 8, 8 ) ;
  {$ELSE}
     Move( _data.X, bufPtg[ 0 ], 8 ) ;
     Move( _data.Y, bufPtg[ 8 ], 8 ) ;
  {$ENDIF}
  AddFeature( _id, bufPtg ) ;
end ;

procedure TGIS_TemporaryFileStream.AddFeature(
  const _id    : Int64 ;
  const _data  : TGIS_Point3D
) ;
begin
  {$IFDEF MANAGED}
     GisCopyMemory( BitConverter.GetBytes( _data.X ), 0,
                              bufPtg3D, 0, 8 ) ;
     GisCopyMemory( BitConverter.GetBytes( _data.Y ), 0,
                              bufPtg3D, 8, 8 ) ;
     GisCopyMemory( BitConverter.GetBytes( _data.Z ), 0,
                              bufPtg3D, 16, 8 ) ;
     GisCopyMemory( BitConverter.GetBytes( _data.M ), 0,
                              bufPtg3D, 24, 8 ) ;
  {$ELSE}
     Move( _data.X, bufPtg3D[ 0 ] , 8 ) ;
     Move( _data.Y, bufPtg3D[ 8 ] , 8 ) ;
     Move( _data.Z, bufPtg3D[ 16 ], 8 ) ;
     Move( _data.M, bufPtg3D[ 24 ], 8 ) ;
  {$ENDIF}
  AddFeature( _id, bufPtg3D ) ;
end ;

function TGIS_TemporaryFileStream.GetFeature(
  const _id   : Int64 ;
  var   _data : TBytes
) : Boolean ;
var
  size  : Integer ;
  off   : {$IFDEF JAVA} nullable {$ENDIF} Int64 ;
begin
  Result := False ;
  if items.TryGetValue( _id, off ) then begin
    Result := True ;
    dataFile.Position := off ;
    {$IFNDEF OXYGENE}
      dataFile.Read( size, sizeOf( size ) ) ;
    {$ELSE}
      dataFile.ReadInteger( size ) ;
    {$ENDIF}
    assert( size > 0 ) ;
    {$IFDEF OXYGENE}
      _data := new Byte[size] ;
    {$ELSE}
      SetLength( _data, size ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      dataFile.ReadBuffer( _data, size ) ;
    {$ELSE}
      dataFile.ReadBuffer( _data[ 0 ], size ) ;
    {$ENDIF}
    exit ;
  end ;
end ;

function TGIS_TemporaryFileStream.GetFeature2D(
  const _id   : Int64 ;
  var   _data : TGIS_Point
) : Boolean ;
begin
  Result := GetFeature( _id, bufPtg ) ;

  {$IFDEF MANAGED}
    _data.X := BitConverter.ToDouble( bufPtg, 0 ) ;
    _data.Y := BitConverter.ToDouble( bufPtg, 8 ) ;
  {$ELSE}
    Move( bufPtg[ 0 ], _data.X, 8 ) ;
    Move( bufPtg[ 8 ], _data.Y, 8 ) ;
  {$ENDIF}
end ;

function TGIS_TemporaryFileStream.GetFeature3D(
  const _id   : Int64;
  var   _data : TGIS_Point3D
) : Boolean ;
begin
  Result := GetFeature( _id, bufPtg3D ) ;

  {$IFDEF MANAGED}
    _data.X := BitConverter.ToDouble( bufPtg3D, 0  ) ;
    _data.Y := BitConverter.ToDouble( bufPtg3D, 8  ) ;
    _data.Z := BitConverter.ToDouble( bufPtg3D, 16 ) ;
    _data.M := BitConverter.ToDouble( bufPtg3D, 24 ) ;
  {$ELSE}
    Move( bufPtg3D[ 0 ] , _data.X, 8 ) ;
    Move( bufPtg3D[ 8 ] , _data.Y, 8 ) ;
    Move( bufPtg3D[ 16 ], _data.Z, 8 ) ;
    Move( bufPtg3D[ 24 ], _data.M, 8 ) ;
  {$ENDIF}
end ;

procedure TGIS_TemporaryFileStream.Clear ;
begin
  items.Clear ;
  dataFile.FlushBuffer ;
end ;

procedure TGIS_TemporaryFileStream.SetFilePosition(
  const _position : Int64
) ;
begin
  dataFile.Position := _position ;
end ;

{$ENDREGION 'TGIS_TemporaryFileStream'}

{$REGION 'TGIS_TextStreamReader'}

const
  STREAMREADER_BUFFER = 32768 ;
  STREAMREADER_OUTBUFFER = 32768 ;

constructor TGIS_TextStreamReader.Create(
  const _stream : TStream
) ;
begin
  oStream := _stream ;
  bEof    := False ;
  bBom    := False ;

  {$IFDEF OXYGENE}
    oEncoding := TEncoding.ASCII ;
  {$ELSE}
    oEncoding := TEncoding.ASCII.Clone ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    arBuffer    := new Byte[STREAMREADER_BUFFER   ] ;
    arOutBuffer := new Byte[STREAMREADER_OUTBUFFER] ;
  {$ELSE}
    SetLength( arBuffer   , STREAMREADER_BUFFER    );
    SetLength( arOutBuffer, STREAMREADER_OUTBUFFER );
  {$ENDIF}

  readChunk ;

  FCodePage := 0 ;

  if iBufSize > 2 then begin
    if ( arBuffer[ 0 ] = $EF )
       and
       ( arBuffer[ 1 ] = $BB )
       and
       ( arBuffer[ 1 ] = $BB )
    then begin
      // UTF-8 8 bit
      CodePage := 65001 ;
      iBufPos := 3 ;
      bBom := True ;
    end
    else
    if ( arBuffer[ 0 ] = $FE )
       and
       ( arBuffer[ 1 ] = $FF )
    then begin
      // UTF-16 big endian
      CodePage := 1201 ;
      iBufPos := 2 ;
      bBom := True ;
    end
    else
    if ( arBuffer[ 0 ] = $FF )
       and
       ( arBuffer[ 1 ] = $FE )
    then begin
      // UTF-16 little endian
      CodePage := 1200 ;
      iBufPos := 2 ;
      bBom := True ;
    end
    else
      FCodePage := 0 ;
  end;

  FFixCodePage := False ;
  fixedCodePage := 0 ;
end;

procedure TGIS_TextStreamReader.doDestroy;
begin
  arBuffer    := nil ;
  arOutBuffer := nil ;

  {$IFNDEF OXYGENE}
    FreeObject( oEncoding ) ;
  {$ENDIF}

  inherited ;
end;

procedure TGIS_TextStreamReader.fset_CodePage(
  const _value : Integer
);
begin
  assert( not bBom ) ;
  FCodePage := _value ;

  {$IFDEF JAVA}
    try
      var cp := TCodePageConverter.Convert(CodePage) ;
      if not IsStringEmpty(cp) then
        oEncoding := TEncoding.GetEncoding( cp )
      else
        oEncoding := TEncoding.ASCII ;
    except
      oEncoding := TEncoding.ASCII ;
    end ;
  {$ENDIF}
  {$IFDEF ISLAND}
    try
      var cp := TCodePageConverter.Convert(CodePage) ;
      if not IsStringEmpty(cp) then
        oEncoding := TEncoding.GetEncoding( cp )
      else
        oEncoding := TEncoding.ASCII ;
    except
      oEncoding := TEncoding.ASCII ;
    end ;
  {$ENDIF}
  {$IFDEF CLR}
    FreeObject( oEncoding ) ;
    oEncoding := TEncoding.GetEncoding( FCodePage ) ;
  {$ENDIF}
  {$IFDEF DCC}
    FreeObject( oEncoding ) ;
    oEncoding := TEncoding.GetEncoding( FCodePage ) ;
  {$ENDIF}
end;

procedure TGIS_TextStreamReader.autofixCodePage ;
begin
  if ( not FFixCodePage ) or (  fixedCodePage <> 0 ) then
    exit ;

  // if UTF8 then try lock code page
  // if local codepage then try UTF8
  if CodePage = 65001 then
    CodePage := GisSystemCodePage
  else
  if CodePage = GisSystemCodePage then
    CodePage := 65001 ;
  fixedCodePage := CodePage ;
end;

procedure TGIS_TextStreamReader.readChunk;
begin
  {$IFDEF OXYGENE}
    iBufSize := oStream.Read( arBuffer, STREAMREADER_BUFFER ) ;
  {$ELSE}
    iBufSize := oStream.Read( arBuffer[0], STREAMREADER_BUFFER ) ;
  {$ENDIF}
  iBufPos := 0 ;
end;

function TGIS_TextStreamReader.Eof: Boolean;
begin
   Result := bEof ;
end;

function TGIS_TextStreamReader.ReadLine
  : String ;
var
  c     : Byte ;
  c0    : Byte ;
  c1    : Byte ;
  i_out : Integer ;
begin
  i_out := 0 ;

  if FCodePage = 1200 then begin
    while True do begin
      if iBufPos < iBufSize then begin
        c0 := arBuffer[ iBufPos ] ;
        c1 := arBuffer[ iBufPos+1 ] ;
        inc( iBufPos, 2 ) ;

        if c1 > 0 then begin
          arOutBuffer[ i_out ] := c0 ;
          inc( i_out ) ;
          arOutBuffer[ i_out ] := c1 ;
          inc( i_out ) ;
        end
        else begin
          case c0 of
            ord( #10 ) : begin
                           // end of line
                           break    ;
                         end ;
            ord( #13 ) : begin
                           // just ignore
                         end ;
            ord( #0  ) ,
            ord( #26 ) : begin
                           // end of line
                           break    ;
                         end ;
            else         begin
                           arOutBuffer[ i_out ] := c0 ;
                           inc( i_out ) ;
                           arOutBuffer[ i_out ] := c1 ;
                           inc( i_out ) ;
                         end;
          end;
        end;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end
  else
  if FCodePage = 1201 then begin
    while True do begin
      if iBufPos < iBufSize then begin
        c0 := arBuffer[ iBufPos ] ;
        c1 := arBuffer[ iBufPos+1 ] ;
        inc( iBufPos, 2 ) ;

        if c0 > 0 then begin
          arOutBuffer[ i_out ] := c0 ;
          inc( i_out ) ;
          arOutBuffer[ i_out ] := c1 ;
          inc( i_out ) ;
        end
        else begin
          case c1 of
            ord( #10 ) : begin
                           // end of line
                           break    ;
                         end ;
            ord( #13 ) : begin
                           // just ignore
                         end ;
            ord( #0  ) ,
            ord( #26 ) : begin
                           // end of line
                           break    ;
                         end ;
            else         begin
                           arOutBuffer[ i_out ] := c0 ;
                           inc( i_out ) ;
                           arOutBuffer[ i_out ] := c1 ;
                           inc( i_out ) ;
                         end;
          end;
        end;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end
  else begin
    while True do begin
      if iBufPos < iBufSize then begin
        c := arBuffer[ iBufPos ] ;
        inc( iBufPos ) ;

        case c of
          ord( #10 ) : begin
                         // end of line
                         break    ;
                       end ;
          ord( #13 ) : begin
                         // just ignore
                       end ;
          ord( #0  ) ,
          ord( #26 ) : begin
                         // end of line
                         break    ;
                       end ;
          else         begin
                         if i_out < STREAMREADER_OUTBUFFER then begin
                            arOutBuffer[ i_out ] := c ;
                            inc( i_out ) ;
                            if i_out > STREAMREADER_OUTBUFFER - 1000 then
                              if c < 127 then
                                break ; // first non special special char
                          end
                          else
                            break ;
                       end;
        end;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end ;

  try
    Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  except
    autofixCodePage ;
    Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  end;
end;

function TGIS_TextStreamReader.ReadXMLHeader
  : String ;
var
  c     : Byte ;
  c0    : Byte ;
  c1    : Byte ;
  i_out : Integer ;
begin
  i_out := 0 ;

  if FCodePage = 1200 then begin
    while True do begin
      if iBufPos < iBufSize then begin
        c0 := arBuffer[ iBufPos ] ;
        c1 := arBuffer[ iBufPos+1 ] ;
        inc( iBufPos, 2 ) ;

        if c1 > 0 then begin
          arOutBuffer[ i_out ] := c0 ;
          inc( i_out ) ;
          arOutBuffer[ i_out ] := c1 ;
          inc( i_out ) ;
        end
        else begin
          case c0 of
            ord( #10 ) : begin
                           // end of line
                           break    ;
                         end ;
            ord( #13 ) : begin
                           // just ignore
                         end ;
            ord( #0  ) ,
            ord( #26 ) : begin
                           // end of line
                           break    ;
                         end ;
            else         begin
                           arOutBuffer[ i_out ] := c0 ;
                           inc( i_out ) ;
                           arOutBuffer[ i_out ] := c1 ;
                           inc( i_out ) ;
                           if c0 = 62 then
                             break ; // closing > tag of encoding
                         end;
          end;
        end;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end
  else
  if FCodePage = 1201 then begin
    while True do begin
      if iBufPos < iBufSize then begin
        c0 := arBuffer[ iBufPos ] ;
        c1 := arBuffer[ iBufPos+1 ] ;
        inc( iBufPos, 2 ) ;

        if c0 > 0 then begin
          arOutBuffer[ i_out ] := c0 ;
          inc( i_out ) ;
          arOutBuffer[ i_out ] := c1 ;
          inc( i_out ) ;
        end
        else begin
          case c1 of
            ord( #10 ) : begin
                           // end of line
                           break    ;
                         end ;
            ord( #13 ) : begin
                           // just ignore
                         end ;
            ord( #0  ) ,
            ord( #26 ) : begin
                           // end of line
                           break    ;
                         end ;
            else         begin
                           arOutBuffer[ i_out ] := c0 ;
                           inc( i_out ) ;
                           arOutBuffer[ i_out ] := c1 ;
                           inc( i_out ) ;
                           if c1 = 62 then
                             break ; // closing > tag of encoding
                         end;
          end;
        end;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end
  else begin
    while True do begin
      if iBufPos < iBufSize then begin
        c := arBuffer[ iBufPos ] ;
        inc( iBufPos ) ;

        case c of
          ord( #10 ) : begin
                         // end of line
                         break    ;
                       end ;
          ord( #13 ) : begin
                         // just ignore
                       end ;
          ord( #0  ) ,
          ord( #26 ) : begin
                         // end of line
                         break    ;
                       end ;
          else         begin
                         if i_out < STREAMREADER_OUTBUFFER then begin
                            arOutBuffer[ i_out ] := c ;
                            inc( i_out ) ;
                            if c = 62 then
                              break ; // closing > tag of encoding
                            if i_out > STREAMREADER_OUTBUFFER - 1000 then
                              if c < 127 then
                                break ; // first non special special char
                          end
                          else
                            break ;
                       end;
        end;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end ;

  try
    Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  except
    autofixCodePage ;
    Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  end;
end;

function TGIS_TextStreamReader.ReadBuffer
  : String ;
var
  b     : Byte ;
  b0    : Byte ;
  b1    : Byte ;
  i_out : Integer ;
begin
  i_out := 0 ;

  if FCodePage = 1200 then begin
    while True do begin
      if iBufPos < iBufSize then begin
        b0 := arBuffer[ iBufPos     ] ;
        b1 := arBuffer[ iBufPos + 1 ] ;

        if i_out < STREAMREADER_OUTBUFFER - 1 then begin
          inc( iBufPos, 2 ) ;
          arOutBuffer[ i_out ] := b0 ;
          inc( i_out ) ;
          arOutBuffer[ i_out ] := b1 ;
          inc( i_out ) ;
        end
        else
          break ;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end
  else
  if FCodePage = 1201 then begin
    while True do begin
      if iBufPos < iBufSize  then begin
        b0 := arBuffer[ iBufPos     ] ;
        b1 := arBuffer[ iBufPos + 1 ] ;

        if i_out < STREAMREADER_OUTBUFFER - 1 then begin
          inc( iBufPos, 2 ) ;
          arOutBuffer[ i_out ] := b0 ;
          inc( i_out ) ;
          arOutBuffer[ i_out ] := b1 ;
          inc( i_out ) ;
        end
        else
          break ;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end
  else begin
    while True do begin
      if iBufPos < iBufSize then begin
        b := arBuffer[ iBufPos ] ;

        if i_out < STREAMREADER_OUTBUFFER then begin
          inc( iBufPos, 1 ) ;
          arOutBuffer[ i_out ] := b ;
          inc( i_out ) ;
          if i_out > STREAMREADER_OUTBUFFER - 1000 then
            if b < 127 then
              break ; // first non special special char
        end
        else
          break ;
      end
      else begin
        readChunk ;
        if iBufSize = 0 then begin
          bEof := True ;
          break ;
        end;
      end;
    end;
  end ;


  Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  try
    Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  except
    autofixCodePage ;
    Result := oEncoding.GetString( arOutBuffer, 0, i_out ) ;
  end;
end;
{$ENDREGION 'TGIS_TextStreamReader'}


{$IFNDEF OXYGENE}

initialization

finalization
  {$IFNDEF ACTIVEX}
    if assigned( oPatrolRead ) then begin
      {$IFNDEF GIS_EDITOR}
        {$IFNDEF GIS_VIEWER}
          oPatrolRead.Free ;
        {$ENDIF}
      {$ENDIF}
      oPatrolRead := nil ;
    end ;
  {$ENDIF}
{$ENDIF}

//==================================== END =====================================
end.
