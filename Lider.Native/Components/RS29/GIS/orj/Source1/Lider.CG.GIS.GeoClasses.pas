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
  This is a primary classes unit.

  Some very common classes are defined here.
}

{$IFDEF DCC}
   unit Lider.CG.GIS.GeoClasses ;
   {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoClasses"'}
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
    System.Types,
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,
    System.SysUtils,
    System.SyncObjs,
    {$IFDEF MSWINDOWS}
      System.Win.ComObj,
      Winapi.Windows,
      Winapi.ActiveX,
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoLogger ;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.ComponentModel,
    System.Collections,
    System.Collections.Generic,
    System.Text,
    System.IO,
    System.Runtime.InteropServices,
    System.Runtime.InteropServices.ComTypes,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF COCOA}
  uses
    remobjects.elements.rtl.*,
    Foundation,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

//----------------------------------------------------------------------------
// general utility classes
//----------------------------------------------------------------------------

type

  {#gendoc:hide}
  {$IFDEF MANAGED}
    TBytesBuffer = TBytes   ;
  {$ELSE}
    TBytesBuffer = Pointer  ;
  {$ENDIF}

  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENPDK}
  /// <summary>
  ///   Helper class like TBytes, but with setting start position.
  /// </summary>
  TGIS_Bytes = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FMemory    : TBytesBuffer  ;
      FPosition  : Integer ;
      FLength    : Integer ;
      FOwnMemory : Boolean ;
    private

      /// <summary>
      ///   PostInitialization.
      /// </summary>
      procedure doCreate ;
    public
      {$IFDEF MANAGED}
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_mem">
        ///   data buffer
        /// </param>
        /// <param name="_pos">
        ///   current position in buffer
        /// </param>
        constructor Create    ( const _mem : TBytes ;
                                const _pos : Integer
                              ) ; overload;

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_mem">
        ///   data buffer
        /// </param>
        /// <param name="_pos">
        ///   current position in buffer
        /// </param>
        /// <param name="_size">
        ///   buffer length
        /// </param>
        constructor Create    ( const _mem  : IntPtr ;
                                const _pos  : Integer ;
                                const _size : Integer
                              ) ; overload;

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_mem">
        ///   data buffer
        /// </param>
        /// <param name="_pos">
        ///   current position in buffer
        /// </param>
        /// <param name="_size">
        ///   buffer length
        /// </param>
        constructor Create    ( const _mem  : TBytes ;
                                const _pos  : Integer ;
                                const _size : Integer
                              ) ; overload;
      {$ELSE}
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_mem">
        ///   data buffer
        /// </param>
        /// <param name="_pos">
        ///   current position in buffer
        /// </param>
        /// <param name="_size">
        ///   buffer length
        /// </param>
        constructor Create    ( const _mem  : Pointer ;
                                const _pos  : Integer ;
                                const _size : Integer
                              ) ; overload;
      {$ENDIF}

      /// <summary>
      ///   Create an object.
      /// </summary>
      /// <param name="_size">
      ///   buffer length
      /// </param>
      constructor Create      ( const _size : Integer
                              ) ; overload;

      {$IFNDEF MANAGED}

        /// <summary>
        ///   Destroy allocated memory.
        /// </summary>
        destructor Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Recreate an object.
      /// </summary>
      /// <param name="_pos">
      ///   new parameter of the object
      /// </param>
      /// <param name="_count">
      ///   new parameter of the object
      /// </param>
      /// <param name="_size">
      ///   new parameter of the object
      /// </param>
      procedure   Recreate    ( const _pos   : Integer ;
                                const _count : Integer ;
                                const _size  : Integer
                              ) ; overload;

      /// <summary>
      ///   Recreate an object.
      /// </summary>
      /// <param name="_mem">
      ///   new parameter of the object
      /// </param>
      /// <param name="_pos">
      ///   new parameter of the object
      /// </param>
      /// <param name="_size">
      ///   new parameter of the object
      /// </param>
      procedure   Recreate    ( const _mem   : TBytes  ;
                                const _pos   : Integer ;
                                const _size  : Integer
                              ) ; overload;

      /// <summary>
      ///   Reset an object.
      /// </summary>
      procedure   Reset       ;

      /// <summary>
      ///   Realloc memory of an object.
      /// </summary>
      /// <param name="_size">
      ///   new size of the object
      /// </param>
      procedure   ReallocMem  ( const _size : Integer ) ;

      /// <summary>
      ///   Copy internal data to the buffer.
      /// </summary>
      /// <param name="_dst">
      ///   destination buffer
      /// </param>
      /// <param name="_dstOffset">
      ///   destination offset
      /// </param>
      /// <param name="_srcOffset">
      ///   source offset
      /// </param>
      /// <param name="_count">
      ///   number of bytes to copy
      /// </param>
      procedure   CopyTo      ( const _dst       : TBytesBuffer ;
                                const _dstOffset : Integer ;
                                const _srcOffset : Integer ;
                                const _count     : Integer
                              ) ;

      /// <summary>
      ///   Copy buffer to internal data.
      /// </summary>
      /// <param name="_src">
      ///   destination buffer
      /// </param>
      /// <param name="_srcOffset">
      ///   source offset
      /// </param>
      /// <param name="_dstOffset">
      ///   destination offset
      /// </param>
      /// <param name="_count">
      ///   number of bytes to copy
      /// </param>
      procedure   CopyFrom    ( const _src       : TBytesBuffer ;
                                const _srcOffset : Integer ;
                                const _dstOffset : Integer ;
                                const _count     : Integer
                              ) ;

      /// <summary>
      ///   Read a byte value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values is placed at
      /// </param>
      /// <returns>
      ///   read value
      /// </returns>
      function    ReadByte    ( const _off : Integer
                              ) : Byte ;

      /// <summary>
      ///   Read a word value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values is placed at
      /// </param>
      /// <returns>
      ///   read value
      /// </returns>
      function    ReadWord    ( const _off : Integer
                              ) : Word  ;

      /// <summary>
      ///   Read an integer value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values is placed at
      /// </param>
      /// <returns>
      ///   read value
      /// </returns>
      function    ReadInt32   ( const _off : Integer
                              ) : Integer  ;

      /// <summary>
      ///   Read an integer value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values is placed at
      /// </param>
      /// <returns>
      ///   read value
      /// </returns>
      function    ReadUInt32  ( const _off : Integer
                              ) : Cardinal  ;

      /// <summary>
      ///   Read a double value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values is placed at
      /// </param>
      /// <returns>
      ///   read value
      /// </returns>
      function    ReadDouble  ( const _off : Integer
                              ) : Double   ;

      /// <summary>
      ///   Read a single value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values is placed at
      /// </param>
      /// <returns>
      ///   read value
      /// </returns>
      function    ReadSingle  ( const _off : Integer
                              ) : Single   ;

      /// <summary>
      ///   Write byte value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values will be placed at
      /// </param>
      /// <param name="_val">
      ///   value to be written
      /// </param>
      procedure   WriteByte   ( const _off : Integer ;
                                const _val : Byte
                              ) ;

      /// <summary>
      ///   Write an integer value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values will be placed at
      /// </param>
      /// <param name="_val">
      ///   value to be written
      /// </param>
      procedure   WriteInt32  ( const _off : Integer ;
                                const _val : Integer
                              ) ;

      /// <summary>
      ///   Write a double value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values will be placed at
      /// </param>
      /// <param name="_val">
      ///   value to be written
      /// </param>
      procedure   WriteDouble ( const _off : Integer ;
                                const _val : Double
                              ) ;

      /// <summary>
      ///   Increment an integer value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values will be incremented at
      /// </param>
      procedure   Inc         ( const _off : Integer
                              ) ;

      /// <summary>
      ///   Decrement an integer value.
      /// </summary>
      /// <param name="_off">
      ///   offset the values will be decremented at
      /// </param>
      procedure   Dec         ( const _off : Integer
                              ) ;
    public
      /// <summary>
      ///   Underlying memory structure.
      /// </summary>
      property Memory : TBytesBuffer  read FMemory   ;

      /// <summary>
      ///   Start position for manipulating data.
      /// </summary>
      property Position : Integer read FPosition write FPosition ;

      /// <summary>
      ///   Length of buffer.
      /// </summary>
      property Size     : Integer read FLength ;
  end ;

  {$IFDEF JAVA OR COCOA}
    {$WARNING '### NO TGIS_OleStream in JAVA'}
  {$ELSE}
    {$IFDEF MSWINDOWS}
      TGIS_OleStream = {$IFDEF OXYGENE} public {$ENDIF} class( TStream )

        private
          FStream      : IStream ;

        protected
        {$IFDEF OXYGENE}
            function  fget_Length   : Int64 ;
            function  fget_Position : Int64 ;
            procedure fset_Position ( const _pos : Int64
                                    ) ;
          private
            FCanRead   : Boolean ;
            FCanSeek   : Boolean ;
            FCanWrite  : Boolean ;
        {$ENDIF}

        {$IFNDEF MANAGED}
          protected
            function GetIStream : IStream ;
        {$ENDIF}

        public

          /// <summary>
          ///   Constructor.
          /// </summary>
          constructor Create    ( const _stream : IStream
                                ) ;
          {$IFDEF MANAGED}
            function  Read      ( const _buffer : array of Byte ;
                                        _offset : Integer ;
                                        _count  : Integer
                                ) : Integer ; override;
          {$ELSE}
            function  Read      ( var   _buffer ;
                                        _count  : LongInt
                                ) : LongInt ; override;
          {$ENDIF}
          {$IFDEF MANAGED}
            procedure Write     ( const _buffer : array of Byte ;
                                        _offset : Integer ;
                                        _count  : Integer
                                ) ; override;
          {$ELSE}
            function  Write     ( const _buffer ;
                                        _count  : LongInt
                                ) : LongInt ; override;
          {$ENDIF}
          {$IFDEF MANAGED}
            function  Seek      ( const _offset : Int64 ;
                                  _origin       : TSeekOrigin
                                ) : Int64 ; override;
          {$ELSE}
            function  Seek      (       _offset : LongInt ;
                                        _origin : Word
                                ) : LongInt ; override;
          {$ENDIF}

        {$IFDEF OXYGENE}
          public
            procedure Flush     ;  override;
            procedure SetLength ( const _val : Int64
                                ) ; override;
          public
            property CanRead    : Boolean read  FCanRead     ; override;
            property CanSeek    : Boolean read  FCanSeek     ; override;
            property CanWrite   : Boolean read  FCanWrite    ; override;
            property Length     : Int64   read  fget_Length   ; override;
            property Position   : Int64   read  fget_Position
                                          write fset_Position ; override;
          public
            property Size       : Int64   read  fget_Length   ;
        {$ENDIF}
      end;
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF MANAGED}
    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    {#gendoc:hide:GENSCR}
    /// <summary>
    ///   Interfaced object that implements IStream interface. See TStreamAdapter.
    /// </summary>
    TGIS_StreamAdapter = class ( TStreamAdapter )
      public
        /// <inheritdoc/>
        function Stat   ( out statstg : TStatStg ;
                          grfStatFlag : {$IFDEF LEVEL_XE8_RTL}
                                          DWORD
                                        {$ELSE}
                                          LongInt
                                        {$ENDIF}
                        ) : HResult ; override; stdcall ;
      end;
  {$ENDIF}

    /// <summary>
    ///   A simple class with encapsulate basic critical section locking
    ///   mechanism.
    /// </summary>
    TGIS_ThreadClass = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
      private
        criticalSection : TCriticalSection ;
      public

        /// <summary>
        ///   Locks the thread.
        /// </summary>
        /// <remarks>
        ///   Locks the list against other threads.
        /// </remarks>
        procedure LockThread   ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}

        /// <summary>
        ///   Unlocks the  thread.
        /// </summary>
        /// <remarks>
        ///   Releases lock obtained by LockThread.
        /// </remarks>
        procedure UnlockThread ; {$IFNDEF GIS_NOINLINE} inline ; {$ENDIF}
      protected

        /// <summary>
        ///   Destroy thread class object.
        /// </summary>
        procedure doDestroy    ; override;
      public

        /// <summary>
        ///   Create thread class object.
        /// </summary>
        constructor Create ;
    end ;

    /// <summary>
    ///   List for storing passwords.
    /// </summary>
    TGIS_PasswordList = {$IFDEF OXYGENE} public {$ENDIF}
                        class( TGIS_ThreadClass )
      private
        oList : TGIS_StringList ;
      protected

        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        procedure doDestroy  ; override;
      public

        /// <summary>
        ///   Create an instance.
        /// </summary>
        constructor  Create  ;

        /// <summary>
        ///   Clear list - delete all passwords.
        /// </summary>
        procedure   Clear ;

        /// <summary>
        ///   Merge with values form provided list.
        /// </summary>
        /// <param name="_lst">
        ///   list with values
        /// </param>
        procedure   Merge ( const _lst       : TGIS_PasswordList
                          ) ;

        /// <summary>
        ///   Add new password.
        /// </summary>
        /// <param name="_name">
        ///   name of the item; for layers password use layer name
        /// </param>
        /// <param name="_key">
        ///   key; for layers use password keys; for example for SQL layer each
        ///   password can contain to keys: 'username' and 'password'; meaning
        ///   of key can be layer specific;
        /// </param>
        /// <param name="_value">
        ///   value of the name.key
        /// </param>
        procedure   Add   ( const _name      : String;
                            const _key       : String;
                            const _value     : String
                          ) ;

        /// <summary>
        ///   Get password.
        /// </summary>
        /// <param name="_name">
        ///   name of the item; for layers password use layer name
        /// </param>
        /// <param name="_key">
        ///   key; for layers use password keys; for example for SQL layer each
        ///   password can contain to keys: 'username' and 'password'; meaning
        ///   of key can be layer specific;
        /// </param>
        /// <returns>
        ///   value of the name.key pair
        /// </returns>
        function    Get   ( const _name      : String;
                            const _key       : String
                          ) : String ;

        /// <summary>
        ///   Check password.
        /// </summary>
        /// <param name="_name">
        ///   name of the item; for layers password use layer name
        /// </param>
        /// <param name="_key">
        ///   key; for layers use password keys; for example for SQL layer each
        ///   password can contain to keys: 'username' and 'password'; meaning
        ///   of key can be layer specific;
        /// </param>
        /// <param name="_value">
        ///   value of the name.key pair
        /// </param>
        /// <returns>
        ///   True if _value match the password name.key pair
        /// </returns>
        function    Check ( const _name      : String;
                            const _key       : String;
                            const _value     : String
                          ) : Boolean ;
    end ;

    /// <summary>
    ///   List for aliases.
    /// </summary>
    TGIS_AliasList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ThreadClass )
      private
        oList : TGIS_StringList ;
      protected

        /// <summary>
        ///   Destroy an instance.
        /// </summary>
        procedure doDestroy ; override;
      public

        /// <summary>
        ///   Create an instance.
        /// </summary>
        constructor  Create  ;

        /// <summary>
        ///   Clear list - delete all aliases.
        /// </summary>
        procedure Clear  ;

        /// <summary>
        ///   Add new alias.
        /// </summary>
        /// <param name="_alias">
        ///   name of the alias (case insensitive)
        /// </param>
        /// <param name="_value">
        ///   value of the alias
        /// </param>
        procedure Add    ( const _alias     : String;
                           const _value     : String
                         ) ;

        /// <summary>
        ///   Get alias.
        /// </summary>
        /// <param name="_alias">
        ///   name of the alias (case insensitive)
        /// </param>
        /// <returns>
        ///   value of the alias
        /// </returns>
        function  Get    ( const _alias     : String
                         ) : String ;

        /// <summary>
        ///   Resolve alias.
        /// </summary>
        /// <param name="_string">
        ///   String in a form of 'text &lt;#ALIAS1#&gt; text
        ///   &lt;&lt;#ALIAS2#&gt; text'
        /// </param>
        /// <returns>
        ///   string with all aliases resolved/expanded
        /// </returns>
        function  Resolve(
                           const _string    : String
                         ) : String ;
    end ;

    /// <summary>
    ///   Thread storage. Use this class to store any thread related classes.
    ///   Class will be free upon application terminate or upon exception at
    ///   undetermined time.
    /// </summary>
    TGIS_ThreadStorage = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
      private
        FThreadId     : Cardinal ;
        FGisAliasList : TGIS_AliasList ;
      protected

        /// <summary>
        ///   Free thread storage object.
        /// </summary>
        procedure doDestroy   ; override;
      public

        /// <summary>
        ///   Create a thread storage object.
        /// </summary>
        constructor Create    ;
      public
        /// <summary>
        ///   Thread id.
        /// </summary>
        property ThreadId     : Cardinal
                                read FThreadId     ;

        /// <summary>
        ///   List of aliases.
        /// </summary>
        property GisAliasList : TGIS_AliasList
                                read FGisAliasList ;
    end;

    /// <summary>
    ///   A simple tokenizer for parameters line splitting.
    /// </summary>
    TGIS_Tokenizer = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
      private
        FResult   : TStrings ;
        sDblQuote : String ;
        sQuote    : String ;
        FIndex    : Integer ;
      private

        /// <summary>
        ///   Trim leading and trailing quotes from the String.
        /// </summary>
        /// <param name="_str">
        ///   String to be trimmed
        /// </param>
        function trimQuotes ( const _str : String ) : String ;
      public

        /// <summary>
        ///   Move to the first token of Result list.
        /// </summary>
        procedure MoveFirst ;

        /// <summary>
        ///   Is a token on the last Result list position.
        /// </summary>
        /// <returns>
        ///   True if a token is the last one
        /// </returns>
        function  Eof : Boolean ;

        /// <summary>
        ///   Move to the next token of Result list.
        /// </summary>
        procedure MoveNext ;

        /// <summary>
        ///   Get a token at current Result list position.
        /// </summary>
        /// <returns>
        ///   token value
        /// </returns>
        function  CurrentToken : String ;

        /// <summary>
        ///   Current index value.
        /// </summary>
        property  CurrentIndex : Integer  read FIndex write FIndex ;

        /// <summary>
        ///   String list on which each element represent each token.
        /// </summary>
        property &Result  : TGIS_Strings read FResult ;
      protected

        /// <summary>
        ///   Destroy tokenizer object.
        /// </summary>
        procedure doDestroy   ; override;
      public

        /// <summary>
        ///   Create tokenizer object.
        /// </summary>
        constructor  Create   ;

        /// <summary>
        ///   Do tokenizing.
        /// </summary>
        /// <param name="_str">
        ///   String to be tokenized
        /// </param>
        /// <param name="_delims">
        ///   array of delimiters
        /// </param>
        /// <remarks>
        ///   <note type="note">
        ///    Subsequent empty tokens will be ignored. If you required
        ///    different functionality - use TGIS_Tokenizer.ExecuteEx
        ///    </note>
        /// </remarks>
        procedure  Execute    ( const _str    : String        ;
                                const _delims : array of Char
                              ) ; overload;

        /// <summary>
        ///   Do tokenizing.
        /// </summary>
        /// <param name="_str">
        ///   String to be tokenized
        /// </param>
        /// <param name="_delims">
        ///   array of delimiters
        /// </param>
        /// <param name="_quotes">
        ///   if true, then delimiter will be active only after a quote
        /// </param>
        /// <remarks>
        ///   <note type="note">
        ///    Subsequent empty tokens will be ignored. If you required
        ///    different functionality - use TGIS_Tokenizer.ExecuteEx
        ///    </note>
        /// </remarks>
        procedure    Execute  ( const _str    : String        ;
                                const _delims : array of Char ;
                                const _quotes : Boolean
                              ) ; overload;

        /// <summary>
        ///   Do tokenizing of simple stream like "aaaa","aaaa",123,"aaaa".
        /// </summary>
        /// <param name="_str">
        ///   String to be tokenized
        /// </param>
        /// <remarks>
        ///   This method is much faster then Execute.
        /// </remarks>
        procedure  ExecuteEx  ( const _str    : String
                              ) ; overload;

        /// <summary>
        ///   Do tokenizing of simple stream like "aaaa","aaaa",123,"aaaa".
        /// </summary>
        /// <param name="_str">
        ///   String to be tokenized
        /// </param>
        /// <param name="_delim">
        ///   delimiter char (like period)
        /// </param>
        /// <remarks>
        ///   This method is much faster then Execute.
        /// </remarks>
        procedure  ExecuteEx  ( const _str    : String ;
                                const _delim  : Char
                              ) ; overload;

        /// <summary>
        ///   Do tokenizing of simple stream like "aaaa","aaaa",123,"aaaa".
        /// </summary>
        /// <param name="_str">
        ///   String to be tokenized
        /// </param>
        /// <param name="_delim">
        ///   delimiter char (like period)
        /// </param>
        /// <param name="_quote">
        ///   quote char (like quotes)
        /// </param>
        /// <remarks>
        ///   This method is much faster then Execute.
        /// </remarks>
        procedure    ExecuteEx( const _str    : String ;
                                const _delim  : Char   ;
                                const _quote  : Char
                              ) ; overload;
    end ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   A point list.
    /// </summary>
    TGIS_PointList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectList )
      protected
        function  fget_Item  ( const _index : Integer
                             ) : TGIS_Point ; reintroduce ;
        procedure fset_Item  ( const _index : Integer ;
                               const _val   : TGIS_Point
                             ) ;
      public
        {$IFDEF GENSCR}
          constructor Create ;
        {$ENDIF}

        {$IFDEF MANAGED}
          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          procedure  Dispose ; override;
        {$ELSE}

          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          destructor Destroy ; override;
        {$ENDIF}

        /// <summary>
        ///   Add new item to the list.
        /// </summary>
        /// <param name="_val">
        ///   item to be added
        /// </param>
        /// <returns>
        ///   position of the item
        /// </returns>
        function     Add    ( const _val   : TGIS_Point
                            ) : Integer ;

        /// <summary>
        ///   Insert new item to the list.
        /// </summary>
        /// <param name="_index">
        ///   position where item will be inserted at
        /// </param>
        /// <param name="_val">
        ///   item to be inserted
        /// </param>
        procedure    Insert ( const _index : Integer ;
                              const _val   : TGIS_Point
                            ) ;
      public

          /// <summary>
          ///   Get/set an list item.
          /// </summary>
          /// <param name="_index">
          ///   position in the list
          /// </param>
          property Items[ const _index: Integer ] : TGIS_Point
                                                    read  fget_Item
                                                    write fset_Item ;
                                                    default ;
                                                    {$IFDEF CLR}
                                                      reintroduce ;
                                                    {$ENDIF}
    end ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   A point list.
    /// </summary>
    TGIS_Point3DList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectList )
      protected
        function  fget_Item  ( const _index : Integer
                             ) : TGIS_Point3D ; reintroduce ;
        procedure fset_Item  ( const _index : Integer ;
                               const _val   : TGIS_Point3D
                             ) ;
      public
        {$IFDEF GENSCR}
          constructor Create ;
        {$ENDIF}

        {$IFDEF MANAGED}
          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          procedure  Dispose ; override;
        {$ELSE}

          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          destructor Destroy ; override;
        {$ENDIF}

        /// <summary>
        ///   Add new item to the list.
        /// </summary>
        /// <param name="_val">
        ///   item to be added
        /// </param>
        /// <returns>
        ///   position of the item
        /// </returns>
        function     Add    ( const _val   : TGIS_Point3D
                            ) : Integer ;

        /// <summary>
        ///   Insert new item to the list.
        /// </summary>
        /// <param name="_index">
        ///   position where item will be inserted at
        /// </param>
        /// <param name="_val">
        ///   item to be inserted
        /// </param>
        procedure    Insert ( const _index : Integer ;
                              const _val   : TGIS_Point3D
                            ) ;
      public

          /// <summary>
          ///   Get/set an list item.
          /// </summary>
          /// <param name="_index">
          ///   position in the list
          /// </param>
          property Items[ const _index: Integer ] : TGIS_Point3D
                                                    read  fget_Item
                                                    write fset_Item ;
                                                    default ;
                                                    {$IFDEF CLR}
                                                      reintroduce ;
                                                    {$ENDIF}
    end ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   A variant list.
    /// </summary>
    TGIS_VariantList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectList )
      protected
        function  fget_Item  ( const _index : Integer
                             ) : Variant ; reintroduce ;
        procedure fset_Item  ( const _index : Integer ;
                               const _val   : Variant
                             ) ; reintroduce ;
      public
        {$IFDEF GENSCR}
          constructor Create ;
        {$ENDIF}

        {$IFDEF MANAGED}
          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          procedure  Dispose ; override;
        {$ELSE}
          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          destructor Destroy ; override;
        {$ENDIF}

        {$IFDEF GENSCR}
           function  Count : Integer ;
           procedure Clear ;
        {$ENDIF}

        /// <summary>
        ///   Add new item to the list.
        /// </summary>
        /// <param name="_val">
        ///   item to be added
        /// </param>
        /// <returns>
        ///   position of the item
        /// </returns>
        function     Add    ( const _val   : Variant
                            ) : Integer ;
                            {$IFDEF CLR} reintroduce ; {$ENDIF}

        /// <summary>
        ///   Insert new item to the list.
        /// </summary>
        /// <param name="_index">
        ///   position where item will be inserted at
        /// </param>
        /// <param name="_val">
        ///   item to be inserted
        /// </param>
        procedure    Insert ( const _index : Integer ;
                              const _val   : Variant
                            ) ;
                            {$IFDEF CLR} reintroduce ; {$ENDIF}

        /// <summary>
        ///   Sort the list.
        /// </summary>
        procedure    Sort   ;
                            {$IFDEF CLR} reintroduce ; {$ENDIF}

      public

          /// <summary>
          ///   Get/set an list item.
          /// </summary>
          /// <param name="_index">
          ///   position in the list
          /// </param>
          property Items[ const _index: Integer ] : Variant
                                                    read  fget_Item
                                                    write fset_Item ;
                                                    default ;
                                                    {$IFDEF CLR}
                                                      reintroduce ;
                                                    {$ENDIF}
    end ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    /// <summary>
    ///   A field-value list.
    /// </summary>
    TGIS_FieldList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectList )
      protected
        function  fget_Item  ( const _field : Integer
                             ) : Variant ; reintroduce ;
        procedure fset_Item  ( const _field : Integer ;
                               const _val   : Variant
                             ) ; reintroduce ;

        /// <summary>
        ///   Find element.
        /// </summary>
        /// <param name="_field">
        ///   field id
        /// </param>
        /// <param name="_index">
        ///   returned index on the list if found or position to insert new
        /// </param>
        /// <returns>
        ///   True if field was found
        /// </returns>
        function  find       ( const _field : Integer ;
                                var _index : Integer
                             ) : Boolean ;
      public
        {$IFDEF GISSCR}
          constructor Create ;
        {$ENDIF}

        {$IFDEF MANAGED}
          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          procedure  Dispose ; override;
        {$ELSE}
          /// <summary>
          ///   Destroy an instance and free all items.
          /// </summary>
          destructor Destroy ; override;
        {$ENDIF}

        /// <summary>
        ///   Add new item to the list.
        /// </summary>
        /// <param name="_field">
        ///   field id
        /// </param>
        /// <param name="_val">
        ///   item value
        /// </param>
        /// <returns>
        ///   position of the item
        /// </returns>
        function     Add    ( const _field : Integer ;
                              const _val   : Variant
                            ) : Integer ;
                              reintroduce ;

        /// <summary>
        ///   Insert new item to the list.
        /// </summary>
        /// <param name="_index">
        ///   position where item will be inserted at
        /// </param>
        /// <param name="_field">
        ///   field id
        /// </param>
        /// <param name="_val">
        ///   item value
        /// </param>
        procedure    Insert ( const _index : Integer ;
                              const _field : Integer ;
                              const _val   : Variant
                            ) ;
                            reintroduce ;

        /// <summary>
        ///   Sort the list.
        /// </summary>
        procedure    Sort   ;
                            reintroduce ;

        /// <summary>
        ///   Check if field exists.
        /// </summary>
        /// <param name="_field">
        ///   field id
        /// </param>
        /// <returns>
        ///   True if field was found
        /// </returns>
        function     Exists ( const _field : Integer
                             ) : Boolean ;

        /// <summary>
        ///   Copy a list to another.
        /// </summary>
        /// <param name="_list">
        ///   destination list
        /// </param>
        procedure    CopyTo ( const _list  : TGIS_FieldList
                             ) ;
      public

          /// <summary>
          ///   Get/set a list item.
          /// </summary>
          /// <param name="_field">
          ///   field id
          /// </param>
          property Items[ const _field : Integer ] : Variant
                                                    read  fget_Item
                                                    write fset_Item ;
                                                    default ;
                                                    {$IFDEF CLR}
                                                      reintroduce ;
                                                    {$ENDIF}
    end ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    {#gendoc:hide:GENSCR}
    /// <summary>
    ///   A stack
    /// </summary>
    TGIS_Stack<T> = {$IFDEF OXYGENE} public {$ENDIF} class( TStack<T> )
      public
        /// <summary>
        ///   Assign contents of existing stack to the current one.
        /// </summary>
        /// <param name="_source">
        ///   source stack
        /// </param>
        procedure   Assign  ( const _source : TGIS_Stack<T> ) ;

        /// <summary>
        ///   Return the top item object on the stack.
        /// </summary>
        /// <returns>
        ///   The top item object.
        /// </returns>
        function  Top     : T ;

        /// <summary>
        ///   Pop an item from the stack.
        /// </summary>
        /// <returns>
        ///   Item that was just removed from stack.
        /// </returns>
        function  Pop     : T ; {$IFDEF CLR} reintroduce ; {$ENDIF}

        /// <summary>
        ///   Push an item on the stack.
        /// </summary>
        /// <param name="_item">
        ///   Item to put on stack.
        /// </param>
        procedure  Push( const _item : T ) ; {$IFDEF CLR} reintroduce ; {$ENDIF}

        /// <summary>
        ///   Is stack empty?
        /// </summary>
        /// <returns>
        ///   True, If stack is empty.
        /// </returns>
        function    IsEmpty : Boolean ;
    end ;

    {#gendoc:hide:GENXDK}
    {#gendoc:hide:GENPDK}
    {#gendoc:hide:GENSCR}
    /// <summary>
    ///   Stack of integers.
    /// </summary>
    TGIS_StackInteger = {$IFDEF OXYGENE} public {$ENDIF} TGIS_Stack<Integer> ;

  {$IFNDEF GENDOC}
    /// <summary>
    ///   Key Value list.
    /// </summary>
    TGIS_KeyValueList<T, U> = {$IFDEF OXYGENE} public {$ENDIF}
                              {$IFDEF CLR}  class( List< KeyValuePair<T,U> > ) {$ENDIF}
                              {$IFDEF ISLAND}  class( RemObjects.Elements.System.List< KeyValuePair<T,U> > ) {$ENDIF}
                              {$IFDEF JAVA} class( ArrayList< TPair<T,U> > ) {$ENDIF}
                              {$IFDEF DCC}  class( TList< TPair<T,U> >     ) {$ENDIF}
    private
      function  fget_Item( const _index : Integer
                          ) : {$IFDEF DCC} TPair<T,U> {$ENDIF}
                              {$IFDEF OXYGENE} TPair<T,U> {$ENDIF};
                              {$IFNDEF GIS_NOINLINE}{$IFNDEF GIS_INLINE_BUG} inline; {$ENDIF}{$ENDIF}
      procedure fset_Item( const _index : Integer;
                           const _value : {$IFDEF DCC} TPair<T,U> {$ENDIF}
                                          {$IFDEF OXYGENE} TPair<T,U> {$ENDIF}
                          ); {$IFNDEF GIS_NOINLINE}{$IFNDEF GIS_INLINE_BUG} inline; {$ENDIF}{$ENDIF}
      {$IFDEF JAVA}
        function fget_Count : Integer ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      {$ENDIF}
    public
      {$IFDEF ISLAND}
        constructor Create ;
      {$ENDIF}
      procedure Add   ( const _key   : T ;
                        const _value : U
                       ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      procedure Delete( const _index : Integer
                       ) ; {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
    public
      property Items[ const _index : Integer] : {$IFDEF DCC} TPair<T,U> {$ENDIF}
                                                {$IFDEF OXYGENE} TPair<T,U> {$ENDIF}
                                                  read  fget_Item
                                                  write fset_Item ; default ;
      {$IFDEF JAVA}
        property Count : Integer read fget_Count ;
      {$ENDIF}
    end ;
  {$ENDIF}

  /// <summary>
  ///   Exception class for all GIS_Viewer exceptions.
  /// </summary>
  EGIS_Exception = {$IFDEF OXYGENE} public {$ENDIF} class( Exception )

    public
      /// <summary>
      ///   Error number.
      /// </summary>
      ErrNo     : Integer ;

      /// <summary>
      ///   Additional String referring to exception
      ///   (like file name in import filters, etc.)
      /// </summary>
      RefString : String  ;

      /// <summary>
      ///   Additional code referring to exception
      ///   (like line number in import filters etc.)
      /// </summary>
      RefCode   : Integer ;

    public

      /// <summary>
      ///   Construct exception.
      /// </summary>
      /// <param name="_message">
      ///   message of the exception; message must be in format '000 Message %s
      ///   %d';
      /// </param>
      /// <param name="_refstring">
      ///   additional String referring to exception (like file name in import
      ///   filters, etc.)
      /// </param>
      /// <param name="_refcode">
      ///   additional code referring to exception (like line number in import
      ///   filters etc.)
      /// </param>
      constructor Create( const _message   : String  ;
                          const _refstring : String  ;
                          const _refcode   : Integer
                        ) ; overload;

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Construct exception.
      /// </summary>
      /// <param name="_message">
      ///   message of the exception; message must be in format '000 Message %s
      ///   %d';
      /// </param>
      /// <param name="_refstring">
      ///   additional String referring to exception (like file name in import
      ///   filters, etc.)
      /// </param>
      /// <param name="_refcode">
      ///   additional code referring to exception (like line number in import
      ///   filters etc.)
      /// </param>
      /// <param name="_exception">
      ///   exception that is the cause of the current exception
      /// </param>
      constructor Create( const _message   : String  ;
                          const _refstring : String  ;
                          const _refcode   : Integer ;
                          const _exception : Exception
                        ) ; overload;
    end ;

    /// <summary>
    ///   Exception class for GIS_Viewer paint exceptions.
    /// </summary>
    EGIS_PaintException = {$IFDEF OXYGENE} public {$ENDIF} class( EGIS_Exception )

      public
        /// <summary>
        ///   Layer name.
        /// </summary>
        Layer            : String ;

        /// <summary>
        ///   Exception message.
        /// </summary>
        ExceptionMessage : String ;
      public

        {#gendoc:hide:GENXDK}
        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_layer">
        ///   layer name
        /// </param>
        /// <param name="_exception">
        ///   source exception
        /// </param>
        constructor Create( const _layer     : String ;
                            const _exception : Exception
                          ) ;
    end ;

    {$IFDEF OXYGENE}
      /// <summary>
      ///   Provides data for the exception event.
      /// </summary>
      TGIS_PaintExceptionEventArgs = public class ( EventArgs )
        private
          FException : EGIS_PaintException ;

        public
          /// <summary>
          ///   Create an object.
          /// </summary>
          /// <param name="_exception">
          ///   paint exception
          /// </param>
          constructor Create  ( const _exception : EGIS_PaintException
                              ) ;
        public
          /// <summary>
          ///   Paint exception.
          /// </summary>
          property Exception  : EGIS_PaintException read FException ;
      end ;

      /// <summary>
      ///   Standard event for exception.
      /// </summary>
      /// <param name="_sender">
      ///   who raises the event
      /// </param>
      /// <param name="_e">
      ///   paint exception
      /// </param>
      TGIS_PaintExceptionEvent = public procedure(
        _sender    : Object ;
        _e         : TGIS_PaintExceptionEventArgs
      ) of object ;

    {$ELSE}
      /// <summary>
      ///   Standard event for exception.
      /// </summary>
      /// <param name="_sender">
      ///   who raises the event
      /// </param>
      /// <param name="_exception">
      ///   paint exception
      /// </param>
      {$IFDEF GENXDK}
        TGIS_PaintExceptionEvent = procedure(
          var _translated : Boolean ;
              _sender     : TObject    ;
              _exception  : EGIS_PaintException
        ) of object ;
      {$ELSE}
        TGIS_PaintExceptionEvent = procedure(
          _sender    : TObject    ;
          _exception : EGIS_PaintException
        ) of object ;
      {$ENDIF}

    {$ENDIF}

  /// <summary>
  ///   Field structure information.
  /// </summary>
  TGIS_FieldInfo = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_Object )
    private
      FDeleted      : Boolean ;
      FSaved        : Boolean ;
      FFieldType    : TGIS_FieldType ;
      FName         : String  ;
      FWidth        : Integer ;
      FDecimal      : Integer ;
      FNewName      : String  ;
      FNewWidth     : Integer ;
      FNewDecimal   : Integer ;
      FBinary       : Integer ;
      FExportName   : String  ;
      FHidden       : Boolean ;
      FReadOnly     : Boolean ;
      FFileFormat   : Boolean ;
      FTemporary    : Boolean ;
      FIsUID        : Boolean ;
      FFlags        : TGIS_FieldFlagsSet ;
      FRules        : TObject ;
    protected

      /// <summary>
      ///   Check a field settings and set cumulative flags.
      /// </summary>
      procedure checkFlags ;

      procedure fset_Deleted   ( const _value : Boolean ) ;
      procedure fset_Saved     ( const _value : Boolean ) ;
      procedure fset_Hidden    ( const _value : Boolean ) ;
      procedure fset_ReadOnly  ( const _value : Boolean ) ;
      procedure fset_FileFormat( const _value : Boolean ) ;
      procedure fset_Temporary ( const _value : Boolean ) ;
      procedure fset_IsUID     ( const _value : Boolean ) ;
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create ;
    public

      /// <summary>
      ///   If True file must be deleted on save.
      /// </summary>
      property Deleted    : Boolean         read FDeleted     write fset_Deleted    ;

      /// <summary>
      ///   if True the field exist in disk based image; valid only for real disk
      ///   based layer like TGIS_LayerSHP, TGIS_LayerSHP etc. Layers like
      ///   TGIS_Layer_DXF and TGIS_LayerMIF are first imported into memory, value
      ///   of Saved will always be false
      /// </summary>
      property Saved      : Boolean         read FSaved       write fset_Saved      ;

      /// <summary>
      ///   Type of field.
      /// </summary>
      property FieldType  : TGIS_FieldType  read FFieldType   write FFieldType     ;

      /// <summary>
      ///   Name as represented in saved version.
      /// </summary>
      property Name       : String          read FName        write FName          ;

      /// <summary>
      ///   Width as represented in saved version.
      /// </summary>
      property Width      : Integer         read FWidth       write FWidth         ;

      /// <summary>
      ///   Decimal as represented in saved version.
      /// </summary>
      property Decimal    : Integer         read FDecimal     write FDecimal       ;

      /// <summary>
      ///   New name of field.
      /// </summary>
      property NewName    : String          read FNewName     write FNewName       ;

      /// <summary>
      ///   New width of field.
      /// </summary>
      property NewWidth   : Integer         read FNewWidth    write FNewWidth      ;

      /// <summary>
      ///   New decimal size of field.
      /// </summary>
      property NewDecimal : Integer         read FNewDecimal  write FNewDecimal    ;

      /// <summary>
      ///   Binary width for TAB/DAT files.
      /// </summary>
      property Binary     : Integer         read FBinary      write FBinary        ;

      /// <summary>
      ///   Field used in exporting.
      /// </summary>
      property ExportName : String          read FExportName  write FExportName    ;

      /// <summary>
      ///   If True, then field will not be visible in controls.
      /// </summary>
      property Hidden     : Boolean         read FHidden      write fset_Hidden     ;

      /// <summary>
      ///   If True, then value of the field can not be modified.
      /// </summary>
      property &ReadOnly  : Boolean         read FReadOnly    write fset_ReadOnly   ;

      /// <summary>
      ///   If True, then field is specific to file format and should not be visible.
      /// </summary>
      property FileFormat : Boolean         read FFileFormat  write fset_FileFormat ;

      /// <summary>
      ///   If True, then field is temporary (not stored in a file).
      /// </summary>
      property Temporary  : Boolean         read FTemporary   write fset_Temporary  ;

      /// <summary>
      ///   If True, then field is used as GIS_UID and will be exported.
      /// </summary>
      property IsUID      : Boolean         read FIsUID       write fset_IsUID ;

      /// <summary>
      ///   Set of cumulative field flags.
      /// </summary>
      property &Flags      :  TGIS_FieldFlagsSet    read FFlags ;

      /// <summary>
      ///   Field rules.
      /// </summary>
      {#ownership:set:release}
      property Rules      : TObject         read FRules     write FRules       ;
   end ;

   {#gendoc:hide:GENXDK}
   {#gendoc:hide:GENPDK}
   /// <summary>
   ///   Field structure information list.
   /// </summary>
   {$IFNDEF GIS_NOGENERICS}
     TGIS_FieldInfoList = {$IFDEF OXYGENE} public {$ENDIF} TObjectList<TGIS_FieldInfo>;
   {$ELSE}
     TGIS_FieldInfoList = class( TObjectList<TGIS_FieldInfo> );
   {$ENDIF}

   /// <summary>
   ///   Class managing a list of texture coordinates.
   /// </summary>
   TGIS_Textures = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FNumTextures       : Integer    ;
      FNumParts          : Integer    ;
      FTextureDimension  : Integer    ;
      FTextureParts      : TGIS_IntegerArray ;
      FTextureCoords     : TGIS_SingleArray ;
      FTextureCoordsLen  : Integer ;
    public
      /// <summary>
      ///   Assign textures array.
      /// </summary>
      /// <param name="_textures">
      ///   list to assign
      /// </param>
      procedure Assign         ( const _textures     : TGIS_Textures
                                ) ;

      /// <summary>
      ///   Resize array of textures.
      /// </summary>
      /// <param name="_numParts">
      ///   number of parts
      /// </param>
      /// <param name="_numTextures">
      ///   number of textures
      /// </param>
      /// <param name="_numDimension">
      ///   number of dimensions
      /// </param>
      procedure Resize         ( const _numParts     : Integer ;
                                 const _numTextures  : Integer ;
                                 const _numDimension : Integer
                                ) ;

      /// <summary>
      ///   Get part size.
      /// </summary>
      /// <param name="_part">
      ///   number of part
      /// </param>
      /// <returns>
      ///   part size.
      /// </returns>
      function  GetPartSize    ( const _part         : Integer
                                ) : Integer ;

      /// <summary>
      ///   Set a part offset.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_offset">
      ///   new offset
      /// </param>
      procedure SetPartOffset  ( const _part         : Integer ;
                                 const _offset       : Integer
                                ) ;

      /// <summary>
      ///   Get texture coordinate.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_index">
      ///   index
      /// </param>
      /// <returns>
      ///   coordinate.
      /// </returns>
      function  GetTextureCoord( const _part         : Integer ;
                                 const _index        : Integer
                                ) : Single ;

      /// <summary>
      ///   Set texture coordinate.
      /// </summary>
      /// <param name="_index">
      ///   index
      /// </param>
      /// <param name="_texture">
      ///   texture coordinate
      /// </param>
      procedure SetTextureCoord( const _index        : Integer ;
                                 const _texture      : Single
                                ) ;
    public

      /// <summary>
      ///   Texture coordinates dimension.
      /// </summary>
      property TextureDimension : Integer read FTextureDimension ;

      /// <summary>
      ///   Number of coordinate parts. Should be equal to number of shape parts.
      /// </summary>
      property NumParts         : Integer read FNumParts ;
   end ;

   /// <summary>
   ///   Class managing a list of texture materials.
   /// </summary>
   TGIS_Materials = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FNumMaterials      : Integer    ;
      FCompressionType   : TGIS_CompressionType    ;
      FMaterials         : array of TGIS_Material ;
    protected

      procedure fset_Material( const _index : Integer ;
                               const _material : TGIS_Material
                             ) ;
      function  fget_Material( const _index : Integer
                             ) : TGIS_Material ;
    public

      /// <summary>
      ///   Assign materials array.
      /// </summary>
      /// <param name="_materials">
      ///   list to be assigned
      /// </param>
      procedure Assign( const _materials : TGIS_Materials
                       ) ;

      /// <summary>
      ///   Resize materials array.
      /// </summary>
      /// <param name="_numMaterials">
      ///   new size
      /// </param>
      procedure Resize( const _numMaterials : Integer
                       ) ;
    public

      /// <summary>
      ///   Number of materials.
      /// </summary>
      property NumMaterials : Integer read FNumMaterials ;

      /// <summary>
      ///   Material array. Each position represents the material id.
      /// </summary>
      /// <param name="_index">
      ///   material index
      /// </param>
      property Material[const _index : Integer] : TGIS_Material read  fget_Material
                                                                write fset_Material ;

      /// <summary>
      ///   Compression type of materials.
      /// </summary>
      property CompressionType : TGIS_CompressionType  read  FCompressionType
                                                       write FCompressionType;
   end ;

   /// <summary>
   ///   Class managing a list of normal vectors.
   /// </summary>
   TGIS_Normals = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FNormals    : TGIS_SingleVectorArray ;
      FNumNormals : Integer ;
    protected

      function  fget_Normal( const _index   : Integer
                           ) : TGIS_SingleVector ;
      procedure fset_Normal( const _index   : Integer ;
                             const _normal  : TGIS_SingleVector
                           ) ;
    public

      /// <summary>
      ///   Assign normals array.
      /// </summary>
      /// <param name="_normals">
      ///   list to be assigned
      /// </param>
      procedure Assign     ( const _normals : TGIS_Normals
                           ) ;

      /// <summary>
      ///   Resize normals array.
      /// </summary>
      /// <param name="_size">
      ///   new size
      /// </param>
      procedure Resize     ( const _size    : Integer
                           ) ;
    public

      /// <summary>
      ///   Number of normals. Should be equal to number of shape points.
      /// </summary>
      property NumNormals : Integer read FNumNormals ;

      /// <summary>
      ///   Normal array. Each position represent a normal found at each point.
      /// </summary>
      /// <param name="_index">
      ///   normal array index
      /// </param>
      property Normal[ const _index : Integer ] : TGIS_SingleVector read  fget_Normal
                                                                    write fset_Normal ;
   end ;

   /// <summary>
   ///   Class managing a list of Vertex Color .
   /// </summary>
   TGIS_VertexColors = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FNumVertexColors       : Integer    ;
      FNumParts              : Integer    ;
      FVertexColorParts      : TGIS_IntegerArray ;
      FVertexColors          : TGIS_CardinalArray ;
    public

      /// <summary>
      ///   Assign vertex Colors array.
      /// </summary>
      /// <param name="_vertexColors">
      ///   list to assign
      /// </param>
      procedure Assign         ( const _vertexColors     : TGIS_VertexColors
                                ) ;

      /// <summary>
      ///   Resize array of Vertex Colors.
      /// </summary>
      /// <param name="_numParts">
      ///   number of parts
      /// </param>
      /// <param name="_numColors">
      ///   number of Vertex Colors
      /// </param>
      procedure Resize         ( const _numParts     : Integer ;
                                 const _numColors    : Integer
                                ) ;

      /// <summary>
      ///   Get part size.
      /// </summary>
      /// <param name="_part">
      ///   number of part
      /// </param>
      /// <returns>
      ///   part size.
      /// </returns>
      function  GetPartSize    ( const _part         : Integer
                                ) : Integer ;

      /// <summary>
      ///   Set a part offset.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_offset">
      ///   new offset
      /// </param>
      procedure SetPartOffset  ( const _part         : Integer ;
                                 const _offset       : Integer
                                ) ;

      /// <summary>
      ///   Get vertex color.
      /// </summary>
      /// <param name="_part">
      ///   part number
      /// </param>
      /// <param name="_index">
      ///   index
      /// </param>
      /// <returns>
      ///   color.
      /// </returns>
      function  GetVertexColor ( const _part         : Integer ;
                                 const _index        : Integer
                                ) : Cardinal ;

      /// <summary>
      ///   Set vertex color.
      /// </summary>
      /// <param name="_index">
      ///   index
      /// </param>
      /// <param name="_color">
      ///   vertex color
      /// </param>
      procedure SetVertexColor ( const _index        : Integer ;
                                 const _color        : Cardinal
                                ) ;
    public
      /// <summary>
      ///   Number of color parts. Should be equal to number of shape parts.
      /// </summary>
      property NumParts         : Integer read FNumParts ;
   end ;



   /// <summary>
   ///   Class managing a list of Part Descriptors.
   /// </summary>
   TGIS_PartDescriptors = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FPartDescriptors    : array of TGIS_PartDescriptor ;
      FNumPartDescriptors : Integer ;
    protected
      function  fget_PartDescriptor( const _index       : Integer
                                   ) : TGIS_PartDescriptor ;
      procedure fset_PartDescriptor( const _index       : Integer ;
                                     const _descriptor  : TGIS_PartDescriptor
                                   ) ;

    public
      /// <summary>
      ///   Assign part descriptors array.
      /// </summary>
      /// <param name="_descriptor">
      ///   list to be assigned
      /// </param>
      procedure Assign   ( const _descriptor : TGIS_PartDescriptors
                          ) ;

      /// <summary>
      ///   Resize part descriptors array.
      /// </summary>
      /// <param name="_size">
      ///   new size
      /// </param>
      procedure Resize   ( const _size       : Integer
                          ) ;
    public

      /// <summary>
      ///   Number of part descriptors.
      /// </summary>
      property NumPartDescriptors : Integer read FNumPartDescriptors ;

      /// <summary>
      ///   Part descriptor array. Should be equal to number of shape parts.
      /// </summary>
      /// <param name="_index">
      ///   part descriptor array index
      /// </param>
      property PartDescriptor[ const _index : Integer ]
                 : TGIS_PartDescriptor
                 read  fget_PartDescriptor
                 write fset_PartDescriptor ;
   end ;

type
  /// <summary>
  ///   Editor snap type.
  /// </summary>
  TGIS_EditorSnapType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Snap to the position of the nearest vertex within the set distance tolerance.
      /// </summary>
      Point,

      /// <summary>
      ///   Snap to a line segment or polygon edge (closest position).
      /// </summary>
      Line,

      /// <summary>
      ///   Custom snap to user defined point from event.
      /// </summary>
      Custom,

      /// <summary>
      ///   Prefer snap to a vertex over the nearest shape intersection, a line
      ///   segment or polygon edge.
      /// </summary>
      PointOverLine,

      /// <summary>
      ///   Snap to the first or last vertex of a line or polygon shape.
      /// </summary>
      EndPoint,

      /// <summary>
      ///   Snap to a line or polygon edge at the midpoint position between any two
      ///   sequential vertices forming the line or edge.
      /// </summary>
      Midpoint,

      /// <summary>
      ///   Snap to the point on a nearby line segment or polygon edge that results
      ///   in a perpendicular (90 degree) line drawn from the previously drawn point.
      /// </summary>
      Perpendicular,

      /// <summary>
      ///   Snap to grid points defined by grid spacing.
      /// </summary>
      GridPoint
  ) ;

  /// <summary>
  ///   Editor snap result type.
  /// </summary>
  TGIS_EditorSnapResultType = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Snap to vertex.
      /// </summary>
      Vertex,

      /// <summary>
      ///   Snap to edge or segment.
      /// </summary>
      Edge,

      /// <summary>
      ///   Snap to intersection.
      /// </summary>
      Intersection,

      /// <summary>
      ///   Snap to midpoint.
      /// </summary>
      Midpoint
  ) ;

type
  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the snap point event.
    /// </summary>
    TGIS_EditorSnapPointEventArgs = public class ( EventArgs )
      private
        FPtg  : TGIS_Point3D ;
        FPrec : Double ;
        FProj : TGIS_Point3D ;
        FDist : Double ;

      public
        /// <summary>
        ///   Constructor for TGIS_EditorSnapPointEventArgs.
        /// </summary>
        /// <param name="_ptg">
        ///   3D point
        /// </param>
        /// <param name="_prec">
        ///   precision
        /// </param>
        /// <param name="_proj">
        ///   projected point
        /// </param>
        /// <param name="_dist">
        ///   distance
        /// </param>
        constructor Create ( const _ptg       : TGIS_Point3D ;
                             const _prec      : Double ;
                             const _proj      : TGIS_Point3D ;
                             const _dist      : Double
                           ) ;
      public
        /// <summary>
        ///   3D point.
        /// </summary>
        property Ptg  : TGIS_Point3D read  FPtg  ;
        /// <summary>
        ///   Precision.
        /// </summary>
        property Prec : Double       read  FPrec ;
        /// <summary>
        ///   Projected point.
        /// </summary>
        property Proj : TGIS_Point3D read  FProj
                                     write FProj ;
        /// <summary>
        ///   Distance.
        /// </summary>
        property Dist : Double       read  FDist
                                     write FDist ;
    end ;

    /// <summary>
    ///   Event to call a custom snap routine.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_e">
    ///   TGIS_EditorSnapPointEvent arguments
    /// </param>
    TGIS_EditorSnapPointEvent = public procedure(
      _sender : Object ;
      _e      : TGIS_EditorSnapPointEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event to call a custom snap routine.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_ptg">
    ///   point
    /// </param>
    /// <param name="_prec">
    ///   precision
    /// </param>
    /// <param name="_proj">
    ///   projected point
    /// </param>
    /// <param name="_dist">
    ///   distance
    /// </param>
    TGIS_EditorSnapPointEvent = procedure(
          _sender    : TObject ;
          _ptg       : TGIS_Point3D ;
          _prec      : Double ;
      var _proj      : TGIS_Point3D ;
      var _dist      : Double
    ) of object ;
  {$ENDIF}

  /// <summary>
  ///   Editor operations.
  /// </summary>
  TGIS_EditorOperation = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Active point changed.
    /// </summary>
    Locate,
    /// <summary>
    ///   Add new point.
    /// </summary>
    &Add,
    /// <summary>
    ///   Move a point.
    /// </summary>
    Move,
    /// <summary>
    ///   Delete a point.
    /// </summary>
    Delete
  ) ;

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the editor point change event.
    /// </summary>
    TGIS_EditorPointChangeEventArgs = public class ( EventArgs )
      private
        FPtg       : TGIS_Point3D ;
        FOperation : TGIS_EditorOperation ;
        FProj      : TGIS_Point3D ;
      public
        /// <summary>
        ///   Constructor for TGIS_EditorPointChangeEventArgs.
        /// </summary>
        /// <param name="_ptg">
        ///   3D point
        /// </param>
        /// <param name="_operation">
        ///   edit operation
        /// </param>
        /// <param name="_proj">
        ///   projected point
        /// </param>
        constructor Create ( const _ptg       : TGIS_Point3D ;
                             const _operation : TGIS_EditorOperation ;
                             const _proj      : TGIS_Point3D
                           ) ;
      public
        /// <summary>
        ///   3D point.
        /// </summary>
        property Ptg  : TGIS_Point3D read  FPtg  ;
        /// <summary>
        ///   Edit operation.
        /// </summary>
        property Operation : TGIS_EditorOperation       read  FOperation ;
        /// <summary>
        ///   Projected point.
        /// </summary>
        property Proj : TGIS_Point3D read  FProj
                                     write FProj ;
    end ;

    /// <summary>
    ///   Event called on an editing point change.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_e">
    ///   TGIS_EditorPointChangeEvent arguments
    /// </param>
    TGIS_EditorPointChangeEvent = public procedure(
      _sender : Object ;
      _e      : TGIS_EditorPointChangeEventArgs
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Event to notify a point change operation.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_ptg">
    ///   point
    /// </param>
    /// <param name="_operation">
    ///   operation on the point
    /// </param>
    /// <param name="_proj">
    ///   projected point, can be used to update current point coordinates
    /// </param>
    {$IFDEF GENXDK}
      TGIS_EditorPointChangeEvent = procedure(
        var _translated : Boolean ;
            _sender     : TObject ;
            _ptg        : TGIS_Point3D ;
            _operation  : TGIS_EditorOperation ;
        var _proj       : TGIS_Point3D
      ) of object ;
    {$ELSE}
      TGIS_EditorPointChangeEvent = procedure(
            _sender    : TObject ;
            _ptg       : TGIS_Point3D ;
            _operation : TGIS_EditorOperation ;
        var _proj      : TGIS_Point3D
      ) of object ;
    {$ENDIF}
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the edited point event.
    /// </summary>
    TGIS_EditorPointMoveEventArgs = public class ( EventArgs )
      private
        FPtg       : TGIS_Point3D ;
        FPt        : TPoint ;
      public
        /// <summary>
        ///   Constructor for TGIS_EditorPointMoveEventArgs.
        /// </summary>
        /// <param name="_ptg">
        ///   3D point
        /// </param>
        /// <param name="_pt">
        ///   mouse position
        /// </param>
        constructor Create ( const _ptg       : TGIS_Point3D ;
                             const _pt        : TPoint
                           ) ;
      public
        /// <summary>
        ///   3D point.
        /// </summary>
        property Ptg  : TGIS_Point3D read  FPtg  ;
        /// <summary>
        ///   Mouse position.
        /// </summary>
        property Point : TPoint read  FPt
                                     write FPt ;
    end ;

    /// <summary>
    ///   Event called on an editing point Editing.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_e">
    ///   TGIS_EditorPointMoveEvent arguments
    /// </param>
    TGIS_EditorPointMoveEvent = public procedure(
      _sender : Object ;
      _e      : TGIS_EditorPointMoveEventArgs
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Event to notify mouse editing of a point.
    /// </summary>
    /// <param name="_sender">
    ///   who rises the event
    /// </param>
    /// <param name="_ptg">
    ///   current point
    /// </param>
    /// <param name="_pt">
    ///   mouse position
    /// </param>
    {$IFDEF GENXDK}
      TGIS_EditorPointMoveEvent = procedure(
        var _translated : Boolean ;
            _sender     : TObject ;
            _ptg        : TGIS_Point3D ;
            _pt         : TPoint
      ) of object ;
    {$ELSE}
      TGIS_EditorPointMoveEvent = procedure(
            _sender    : TObject ;
            _ptg       : TGIS_Point3D ;
            _pt        : TPoint
      ) of object ;
    {$ENDIF}
  {$ENDIF}

type

  /// <summary>
  ///   Editor vertex adding mode.
  /// </summary>
  TGIS_EditorMode = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   Editing behavior will be controlled by MouseBegin() _nearest
    ///   parameter. Usually TGIS_EditorMode.NearestPoint, but after pressing
    ///   CTRL same as TGIS_EditorMode.AfterActivePoint.
    /// </summary>
    Default,

    /// <summary>
    ///   Editing will be opposite to default. Usually TGIS_EditorMode.AfterActivePoint,
    ///   but after pressing CTRL same as TGIS_EditorMode.NearestPoint.
    /// </summary>
    Reversed,

    /// <summary>
    ///   Added vertex modifies the nearest line segment or polygon edge.
    /// </summary>
    NearestPoint,

    /// <summary>
    ///   A new vertex is added sequentially (by vertex number) after the last active vertex.
    /// </summary>
    AfterActivePoint
  ) ;

type

  /// <summary>
  ///   For internal use of TGIS_Editor. Basic editor modes.
  /// </summary>
  TGIS_EditorModeEx = {$IFDEF OXYGENE} public {$ENDIF} (

    /// <summary>
    ///   A new point will be added on mouse up.
    /// </summary>
    Normal,

    /// <summary>
    ///   For advanced edit modes, where a new point will not be added on
    ///   mouse up.
    /// </summary>
    Extended
  ) ;

type

  /// <summary>
  ///   Editor style.
  /// </summary>
  TGIS_EditorStyle = {$IFDEF OXYGENE} public {$ENDIF} class(TPersistent)
    private
      FBrushStyle : TGIS_BrushStyle ;
      FBrushColor : TGIS_Color ;
      FPenStyle   : TGIS_PenStyle ;
      FPenColor   : TGIS_Color ;
      FPenWidth   : Integer ;

    {$IFDEF OXYGENE} public {$ELSE} published {$ENDIF}

      /// <summary>
      ///   Brush style.
      /// </summary>
      {$IFDEF CLR}
        [DefaultValue(typeOf(TGIS_BrushStyle), "Solid")]
      {$ENDIF}
      property BrushStyle : TGIS_BrushStyle
                                read  FBrushStyle
                                write FBrushStyle
                                      {$IFDEF OXYGENE}
                                        ;
                                      {$ELSE}
                                        default TGIS_BrushStyle.Solid ;
                                      {$ENDIF}

      /// <summary>
      ///   Brush color.
      /// </summary>
      property BrushColor : TGIS_Color
                                read  FBrushColor
                                write FBrushColor ;

      /// <summary>
      ///   Pen style.
      /// </summary>
      {$IFDEF CLR}
        [DefaultValue(typeOf(TGIS_PenStyle), "Dot")]
      {$ENDIF}
      property PenStyle   : TGIS_PenStyle
                                read  FPenStyle
                                write FPenStyle
                                      {$IFDEF OXYGENE}
                                        ;
                                      {$ELSE}
                                        default TGIS_PenStyle.Dot ;
                                      {$ENDIF}

      /// <summary>
      ///   Pen color.
      /// </summary>
      property PenColor   : TGIS_Color
                                read  FPenColor
                                write FPenColor ;

      /// <summary>
      ///   Pen width.
      /// </summary>
      {$IFDEF CLR}
        [DefaultValue(1)]
      {$ENDIF}
      property PenWidth   : Integer
                                read FPenWidth
                                write FPenWidth
                                      {$IFDEF OXYGENE}
                                        ;
                                      {$ELSE}
                                        default 1 ;
                                      {$ENDIF}
  end;

  /// <summary>
  ///   Editor editing points style.
  /// </summary>
  TGIS_EditorEditingPointsStyle = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TPersistent )

    private
      FPointsFont       : TGIS_Font        ;
      FPointsBackground : TGIS_Color       ;
      FActivePoints     : TGIS_EditorStyle ;
      FInactivePoints   : TGIS_EditorStyle ;
      FSelectedPoints   : TGIS_EditorStyle ;
      F3DPoints         : TGIS_EditorStyle ;
      FSnappingPoints   : TGIS_EditorStyle ;
    protected
      procedure fset_PointsFont       ( const _value : TGIS_Font
                                      ) ;
    public

      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor  Create  ;
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destructor.
        /// </summary>
        destructor Destroy ; override;
      {$ENDIF}

    {$IFDEF OXYGENE} public {$ELSE} published {$ENDIF}

      /// <summary>
      ///   Font for points.
      /// </summary>
      property PointsFont     : TGIS_Font         read  FPointsFont
                                                  write fset_PointsFont ;

      /// <summary>
      ///   Background color for points.
      /// </summary>
      property PointsBackground : TGIS_Color      read  FPointsBackground
                                                  write FPointsBackground ;

      /// <summary>
      ///   Style of active points.
      /// </summary>
      property ActivePoints   : TGIS_EditorStyle  read  FActivePoints
                                                  write FActivePoints ;

      /// <summary>
      ///   Style of inactive points.
      /// </summary>
      property InactivePoints : TGIS_EditorStyle  read  FInactivePoints
                                                  write FInactivePoints ;

      /// <summary>
      ///   Style of selected points.
      /// </summary>
      property SelectedPoints : TGIS_EditorStyle  read  FSelectedPoints
                                                  write FSelectedPoints ;

      /// <summary>
      ///   Style of points 3D.
      /// </summary>
      property Points3D       : TGIS_EditorStyle  read  F3DPoints
                                                  write F3DPoints ;

      /// <summary>
      ///   Style of tracking points.
      /// </summary>
      property SnappingPoints : TGIS_EditorStyle  read  FSnappingPoints
                                                  write FSnappingPoints ;
  end ;

  /// <summary>
  ///   Editor editing edge lengths style.
  /// </summary>
  TGIS_EditorEdgeLengthsStyle = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TPersistent )

    private
      FFont             : TGIS_Font        ;
      FBackground       : TGIS_EditorStyle ;
      FUnitsEPSG        : Integer          ;
      FFollowEdgeAngle  : Boolean          ;

    protected
      procedure fset_Font       ( const _value : TGIS_Font
                                ) ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor  Create  ;
      {$IFNDEF OXYGENE}

        /// <summary>
        ///   Destructor.
        /// </summary>
        destructor Destroy ; override;
      {$ENDIF}

    {$IFDEF OXYGENE} public {$ELSE} published {$ENDIF}

      /// <summary>
      ///   Font for lengths.
      /// </summary>
      property Font     : TGIS_Font               read  FFont
                                                  write fset_Font ;

      /// <summary>
      ///   Style of edge lengths background.
      /// </summary>
      property Background : TGIS_EditorStyle      read  FBackground
                                                  write FBackground ;
      /// <summary>
      ///   Units EPSG code for edges lengths output.
      ///   If set to 0 then automatic Matric or Imperial system will be used
      ///   based on location.
      /// </summary>
      property UnitsEPSG : Integer                read  FUnitsEPSG
                                                  write FUnitsEPSG ;
      /// <summary>
      ///   If true, edge length text will follow an edge angle.
      /// </summary>
      property FollowEdgeAngle : Boolean          read  FFollowEdgeAngle
                                                  write FFollowEdgeAngle ;
  end ;

  /// <summary>
  ///   Structure for determining basic environment information.
  /// </summary>
  TGIS_EnvironmentInfo = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      FIs32      : Boolean ;
      FIs64      : Boolean ;
      FIsWindows : Boolean ;
      FIsMac     : Boolean ;
      FIsUnix    : Boolean ;
      FIsNative  : Boolean ;
      FIsNET     : Boolean ;
      FIsMsNET   : Boolean ;
      FIsMono    : Boolean ;
      FIsJava    : Boolean ;
      FDirSep    : Char    ;
    private
      procedure prepare ;

    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      constructor Create ;

    public

      /// <summary>
      ///   Is 32-bit
      /// </summary>
      property Is32      : Boolean read FIs32      ;

      /// <summary>
      ///   Is 64-bit
      /// </summary>
      property Is64      : Boolean read FIs64      ;

      /// <summary>
      ///   Is Windows
      /// </summary>
      property IsWindows : Boolean read FIsWindows ;

      /// <summary>
      ///   Is Mac
      /// </summary>
      property IsMac     : Boolean read FIsMac     ;

      /// <summary>
      ///   Is Unix
      /// </summary>
      property IsUnix    : Boolean read FIsUnix    ;

      /// <summary>
      ///   Is Native
      /// </summary>
      property IsNative  : Boolean read FIsNative  ;

      /// <summary>
      ///   Is NET
      /// </summary>
      property IsNET     : Boolean read FIsNET     ;

      /// <summary>
      ///   Is MsNET
      /// </summary>
      property IsMsNET   : Boolean read FIsMsNET   ;

      /// <summary>
      ///   Is Mono
      /// </summary>
      property IsMono    : Boolean read FIsMono    ;

      /// <summary>
      ///   Is Java
      /// </summary>
      property IsJava    : Boolean read FIsJava    ;

      /// <summary>
      ///   Directory separator char
      /// </summary>
      property DirSep    : Char    read FDirSep    ;
  end ;

  /// <summary>
  ///   Global proxy settings class.
  /// </summary>
  TGIS_ProxySettings  = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      FServer        : String ;
      FPort          : Integer ;
      FUser          : String ;
      FPass          : String ;
      FDomain        : String ;
      FNoProxyFilter : TGIS_StringList ;
    protected
      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy  ; override;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor  Create  ;

      /// <summary>
      ///   Clear all settings.
      /// </summary>
      procedure   Clear ;

      /// <summary>
      ///   Check using NoProxyFilter if proxy can be set for an url.
      /// </summary>
      /// <param name="_url">
      ///   url to check
      /// </param>
      /// <returns>
      ///   True if proxy can be set
      /// </returns>
      function CanUseProxy( const _url : String
                          ) : Boolean ;
    public
      /// <summary>
      ///   Host to be used as a proxy
      /// </summary>
      property Server        : String read FServer write FServer ;
      /// <summary>
      ///   Port used to communicate to the proxy
      /// </summary>
      property Port          : Integer read FPort write FPort;
      /// <summary>
      ///   UserName needed to be authenticated to the proxy
      /// </summary>
      property User          : String read FUser write FUser;
      /// <summary>
      ///   Password needed to be authenticated to the proxy
      /// </summary>
      property Pass          : String read FPass write FPass;
      /// <summary>
      ///   Domain to be used with the proxy
      /// </summary>
      property Domain        : String read FDomain write FDomain ;
      /// <summary>
      /// List of hosts for which proxy parameters will not be set
      /// </summary>
      property NoProxyFilter : TGIS_StringList read FNoProxyFilter ;
  end ;

  /// <summary>
  ///   List object for color ramps.
  /// </summary>
  {#typehint:list-ro:TGIS_GradientMap}
  TGIS_ColorRampList = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ThreadClass )
    private
      oList        : TObjectList<TGIS_GradientMap> ;
      oDic         : TDictionary<String, Integer> ;

    private
      function fget_Item  ( const _index : Integer ) : TGIS_GradientMap ;
      function fget_Count : Integer ;

    protected
      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure doDestroy ; override;

    private
      /// <summary>
      ///   Adds a new ramp to the list.
      /// </summary>
      /// <param name="_name">
      ///   name of the map
      /// </param>
      /// <param name="_mapType">
      ///   type of color map
      /// </param>
      /// <param name="_map">
      ///   array of ramp map (index-color)
      /// </param>
      /// <param name="_submap">
      ///   array of ramp map (class-colors)
      /// </param>
      procedure addEx          ( const _name      : String ;
                                 const _mapType   : TGIS_ColorSchema ;
                                 const _map       : TGIS_ColorMapArray ;
                                 const _submap    : TGIS_ColorMapExArray
                               ) ; overload ;

      /// <summary>
      ///   Adds a new ramp to the list.
      /// </summary>
      /// <param name="_def">
      ///   ramp definition
      /// </param>
      procedure addEx          ( const _def      : String
                               ) ; overload ;

      procedure addExDynamicHSL( const _name    : String ;
                                 const _mapType : TGIS_ColorSchema ;
                                 const _hMin    : Integer ;
                                 const _hMax    : Integer ;
                                 const _sMin    : Integer ;
                                 const _sMax    : Integer ;
                                 const _lMin    : Integer ;
                                 const _lMax    : Integer
                               ) ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;

      /// <summary>
      ///   Clears list - deletes all ramps.
      /// </summary>
      procedure Clear ;

      /// <summary>
      ///   Initializes list from internal definition.
      ///   Current ramps will be cleared.
      /// </summary>
      procedure Init ;

      /// <summary>
      ///   Adds a new ramp to the list.
      /// </summary>
      /// <param name="_ramp">
      ///   ramp definition
      /// </param>
      procedure Add     ( const _ramp      : TGIS_GradientMap
                        ) ;

      /// <summary>
      ///   Gets a color ramp by name.
      /// </summary>
      /// <param name="_name">
      ///   name of a ramp
      /// </param>
      /// <returns>
      ///   array of ramp map
      /// </returns>
      function ByName   ( const _name      : String
                        ) : TGIS_GradientMap ; overload ;

    public
      /// <summary>
      ///   Gets a ramp by index.
      /// </summary>
      /// <param name="_index">
      ///   index of a ramp
      /// </param>
      property Items [ const _index : Integer] : TGIS_GradientMap
                       read fget_Item ; default ;
      /// <summary>
      ///   Gets the ramp list count.
      /// </summary>
      property Count : Integer read fget_Count ;
  end ;

  /// <summary>
  ///   Class useful for progress implementation in single or multistage algorithms.
  /// </summary>
  TGIS_BusyEventManager = class ( TGIS_Object )
    private
      FAborted              : Boolean ;
      FSender               : TObject ;
      FUseProgressThreshold : Boolean ;
      FOnBusy               : TGIS_BusyEvent ;

    private
      senderIsLayer         : Boolean ;
      useBusy               : Boolean ;
      assignedBusy          : Boolean ;
      currentStage          : TObject ; // T_BusyEventStage ;

      // list of T_BusyEventStage
      stagesList : TObjectList< TObject > ;

    private
      // property getters
      function fget_Count             : Integer ;
      function fget_EndValue          ( const _stage : Integer ) : Int64 ;
      function fget_EstimatedTimeLeft ( const _stage : Integer ) : Int64 ;
      function fget_Max               ( const _stage : Integer ) : Integer ;
      function fget_Name              ( const _stage : Integer ) : String ;
      function fget_Position          ( const _stage : Integer ) : Integer ;
      function fget_Sender            ( const _stage : Integer ) : TObject ;

      /// <summary>
      ///   Calculates progress position for each stage.
      /// </summary>
      /// <param name="_start_event">
      ///   set True if new stage starts (StartEvent)
      /// </param>
      procedure calcStagePositions    ( const _start_event : Boolean = false ) ;

      /// <summary>
      ///   Resets object to initial state.
      /// </summary>
      procedure initialize ;

      /// <summary>
      ///   If True, use layer RaiseBusyPrepare/Shake/Release, instead of
      ///   default raiseBusyEvent function.
      /// </summary>
      function  shouldRaiseLayerBusyEvent : Boolean ;

      function  raiseBusyEvent        ( const _sender : TObject ;
                                        const _pos    : Integer ;
                                        const _end    : Integer
                                      ) : Boolean ;

    protected
      procedure doDestroy             ; override ;

    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      /// <param name="_sender">
      ///   the reference to the parent object
      /// </param>
      constructor Create ( const _sender : TObject ) ;

    public
      /// <summary>
      ///   Ends the currently executed stage.
      /// </summary>
      /// <returns>
      ///   True if an abort request was set inside message handler;
      ///   False otherwise
      /// </returns>
      /// <remarks>
      ///   Should be called at the end of an algorithm.
      /// </remarks>
      function EndEvent  : Boolean ;

      /// <summary>
      ///   Increments process position for current stage
      ///   and raises busy event smartly.
      /// </summary>
      /// <returns>
      ///   True if an abort request was set inside message handler;
      ///   False otherwise
      /// </returns>
      /// <remarks>
      ///   Should be called at each iteration in any long-time operation.
      /// </remarks>
      function PushEvent : Boolean ; overload ;

      /// <summary>
      ///   Increments process position for current stage,
      ///   updates name, and raises busy event smartly.
      /// </summary>
      /// <param name="_name">
      ///   the name of the event stage can be changed during the operation
      /// </param>
      /// <returns>
      ///   True if an abort request was set inside message handler;
      ///   False otherwise
      /// </returns>
      /// <remarks>
      ///   Should be called at each iteration in long-time operation.
      /// </remarks>
      function PushEvent ( const _name : String
                         ) : Boolean ; overload ;

      /// <summary>
      ///   Creates a new event stage and sets the maximum position for it.
      /// </summary>
      /// <param name="_name">
      ///   the name of a new event stage
      /// </param>
      /// <param name="_end_val">
      ///   the end value of a new event stage
      /// </param>
      /// <param name="_sender">
      ///   the reference to the parent object
      /// </param>
      /// <returns>
      ///   True if an abort request was set inside message handler;
      ///   False otherwise
      /// </returns>
      /// <remarks>
      ///   Should be called at the beginning of algorithm.
      /// </remarks>
      function StartEvent( const _name    : String ;
                           const _end_val : Int64 ;
                           const _sender  : TObject = nil
                         ) : Boolean ;

    public
      /// <summary>
      ///   Indicates whether an abort was requested.
      /// </summary>
      property Aborted              : Boolean
                                      read FAborted ;

      /// <summary>
      ///   Returns the number of stages.
      /// </summary>
      property Count                : Integer
                                      read fget_Count ;

      /// <summary>
      ///   The real upper limit of the process position at specified stage.
      /// </summary>
      /// <param name="_stage">
      ///   the index of a stage
      /// </param>
      property EndValue             [ const _stage : Integer] : Int64
                                      read fget_EndValue ;

      /// <summary>
      ///   Estimated remaining time of the process at specified stage.
      /// </summary>
      /// <param name="_stage">
      ///   the index of a stage
      /// </param>
      property EstimatedTimeLeft    [ const _stage : Integer] : Int64
                                      read fget_EstimatedTimeLeft;

      /// <summary>
      ///   The normalized upper limit of the process position at the
      ///   specified stage.
      /// </summary>
      /// <param name="_stage">
      ///   the index of a stage
      /// </param>
      property Max                  [ const _stage : Integer] : Integer
                                      read fget_Max ;

      /// <summary>
      ///   Name of the process at the specified stage.
      /// </summary>
      /// <param name="_stage">
      ///   the index of a stage
      /// </param>
      property Name                 [ const _stage : Integer] : String
                                      read fget_Name ;

      /// <summary>
      ///   Current position of the process at the specified stage.
      /// </summary>
      /// <param name="_stage">
      ///   the index of a stage
      /// </param>
      property Position             [ const _stage : Integer] : Integer
                                      read fget_Position ;

      /// <summary>
      ///   The reference to the parent object.
      /// </summary>
      /// <param name="_stage">
      ///   the index of a stage
      /// </param>
      property Sender               [ const _stage : Integer] : TObject
                                      read fget_Sender ;

      /// <summary>
      ///   If True, busy events are raised according to a time trigger (default).
      ///   If False, busy events are raised at every PushEvent.
      /// </summary>
      property UseProgressThreshold : Boolean
                                      read FUseProgressThreshold
                                      write FUseProgressThreshold ;

    published // events
      /// <summary>
      ///   Event fired upon progress of the currently executed process.
      /// </summary>
      {$IFDEF CLR}
        event BusyEvent             : TGIS_BusyEvent
                                      delegate FOnBusy ;
      {$ELSE}
        /// <event/>
        property BusyEvent          : TGIS_BusyEvent
                                      read  FOnBusy
                                      write FOnBusy ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    System.Variants,

    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource;
{$ENDIF}

{$REGION 'Various lists - internal types'}
type
  T_Point = class
    public
      FValue : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
    public
      // Creates point object.
      constructor Create( const _value : TGIS_Point ) ;
  end ;

  T_Point3D = class
    public
      FValue : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    public
      // Creates point object.
      constructor Create( const _value : TGIS_Point3D ) ;
  end ;

  T_Variant = class
    public
      FValue : Variant ;
    public
      // Creates variant object.
      constructor Create( const _value : Variant ) ;
  end ;
  {$IFNDEF MANAGED}
  P_Variant = ^T_Variant ;
  {$ENDIF}

  T_Field = class
    public
      FField : Integer ;
      FValue : Variant ;
    public
      // Creates variant object.
      constructor Create( const _field  : Integer ;
                          const _value  : Variant
                         ) ;
  end ;

  {$REGION 'T_BusyEventStage'}
  /// <summary>
  ///   Helper class used in TGIS_EventManager
  /// </summary>
  T_BusyEventStage = class( TObject )
    private
      FName            : String ;
      FSender          : TObject ;
      {$IFDEF JAVA}
        startTick      : Int64 ;
      {$ELSE}
        startTick      : Cardinal ;
      {$ENDIF}
      // progress position in range 0..100
      FPosition        : Integer ;
      FMax             : Integer ;
      // real process position
      FEndValue        : Int64 ;
      FPosValue        : Int64 ;

      // current process progress in range [0..1]
      doubleProgress : Double ;

      // raise busy event when condition (Position mod eventTrigger = 0) is met
      eventTrigger     : Integer ;

    private
      function  fget_EstimatedTimeLeft : Int64 ;
      procedure fset_PosValue          ( _value : Int64 ) ;

      /// <summary>
      ///   Increments PosValue.
      /// </summary>
      procedure incrementPosValue ;    inline ;

      /// <summary>
      ///   Checks if busy event should be raised.
      /// </summary>
      /// <returns>
      ///   True, if busy event should be raised
      /// </returns>
      function  shouldRaise            : Boolean ;

    public
      /// <summary>
      ///   Creates an instance of T_BusyEventStage.
      /// </summary>
      /// <param name="_name">
      ///   name of the current stage
      /// </param>
      /// <param name="_end_val">
      ///   end value of the current stage
      /// </param>
      /// <param name="_sender">
      ///   the reference to the parent object
      /// </param>
      constructor Create         ( const _name    : String ;
                                   const _end_val : Int64 ;
                                   const _sender  : TObject
                                 ) ;

    public
      /// <summary>
      ///   End value of iterative process.
      /// </summary>
      property EndValue          : Int64
                                   read FEndValue
                                   write FEndValue ;

      /// <summary>
      ///   Estimated time left in miliseconds.
      /// </summary>
      property EstimatedTimeLeft : Int64
                                   read fget_EstimatedTimeLeft ;

      /// <summary>
      ///   Maximum position in percents (100%).
      /// </summary>
      property Max               : Integer
                                   read FMax
                                   write FMax ;

      /// <summary>
      ///   Name of the stage.
      /// </summary>
      property Name              : String
                                   read FName
                                   write FName ;

      /// <summary>
      ///   Current position in percents (0%..100%).
      /// </summary>
      property Position          : Integer
                                   read FPosition
                                   write FPosition ;

      /// <summary>
      ///   Current position of iterative process.
      /// </summary>
      property PosValue          : Int64
                                   read FPosValue
                                   write fset_PosValue ;

      /// <summary>
      ///   The reference to parent object
      /// </summary>
      property Sender            : TObject
                                   read FSender
                                   write FSender ;
    end;

  constructor T_BusyEventStage.Create(
    const _name    : String ;
    const _end_val : Int64 ;
    const _sender  : TObject
  ) ;
  begin
    FSender := _sender ;
    FName := _name ;
    FEndValue := {$IFDEF DCC}System.{$ENDIF}Math.Max( 1, _end_val ) ;
    PosValue := 0 ;

    FPosition := 0 ;
    FMax := 100 ;

    startTick := 0 ;
    eventTrigger := 0 ;
  end;

  procedure T_BusyEventStage.fset_PosValue( _value : Int64 ) ;
  begin
    FPosValue := _value ;
    doubleProgress := FPosValue / FEndValue ;
  end;

  function T_BusyEventStage.fget_EstimatedTimeLeft : Int64 ;
  begin
    if ( FPosValue = 0 ) then
      Result := 0
    else
      Result := RoundS( (GetTickCount-startTick) * (1/doubleProgress - 1) ) ;
  end;

  procedure T_BusyEventStage.incrementPosValue ;
  begin
    PosValue := PosValue + 1 ;
    if PosValue > EndValue then
      EndValue := PosValue ;
  end ;

  function T_BusyEventStage.shouldRaise : Boolean ;
  begin
    Result := False ;
    // first push sets timer
    if startTick = 0 then begin
      startTick := GetTickCount ;
      Result := True ;
    end
    else if eventTrigger = 0 then begin
      if ( GetTickCount - startTick > GIS_PROGRESS_TICK_TRESHOLD ) or
         ( doubleProgress > GIS_PROGRESS_PERCENTAGE_THRESHOLD ) then begin
        eventTrigger := PosValue ;
        Result := True ;
      end ;
    end
    else if ( PosValue mod eventTrigger = 0 ) then
      Result := True ;
  end ;

  {$ENDREGION}

//=============================================================================
// T_Point
//=============================================================================

  constructor T_Point.Create( const _value : TGIS_Point ) ;
  begin
    inherited Create ;
    FValue.X := _value.X ;
    FValue.Y := _value.Y ;
  end ;

//=============================================================================
// T_Point3D
//=============================================================================

  constructor T_Point3D.Create( const _value : TGIS_Point3D ) ;
  begin
    inherited Create ;
    FValue.X := _value.X ;
    FValue.Y := _value.Y ;
    FValue.Z := _value.Z ;
    FValue.M := _value.M ;
  end ;

//=============================================================================
// T_Variant
//=============================================================================

  constructor T_Variant.Create( const _value : Variant ) ;
  begin
    inherited Create ;
    FValue := _value ;
  end ;

//=============================================================================
// T_Field
//=============================================================================

  constructor T_Field.Create( const _field  : Integer ;
                              const _value  : Variant
                             ) ;
  begin
    inherited Create ;

    FValue := _value ;
    FField := _field ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_Bytes'}
//=============================================================================
// TGIS_Bytes
//=============================================================================

  {$IFDEF MANAGED}
    constructor TGIS_Bytes.Create (
      const _mem : TBytes ;
      const _pos : Integer
    ) ;
    begin
      inherited Create ;

      FMemory   := _mem ;
      FPosition := _pos ;
      {$IFDEF JAVA}
        if assigned( FMemory ) then
          FLength := FMemory.length
        else
          FLength := 0 ;
      {$ELSE}
        FLength := length( FMemory ) ;
      {$ENDIF}
      doCreate ;

    end ;

    constructor TGIS_Bytes.Create (
      const _mem  : TBytes ;
      const _pos  : Integer;
      const _size : Integer
    ) ;
    begin
      inherited Create ;

      FMemory   := _mem ;
      FPosition := _pos ;
      FLength   := _size ;
      doCreate ;

    end ;
  {$ENDIF}

  {$IFDEF MANAGED}
    constructor TGIS_Bytes.Create (
      const _mem  : IntPtr ;
      const _pos  : Integer;
      const _size : Integer
    ) ;
  {$ELSE}
    constructor TGIS_Bytes.Create (
      const _mem  : Pointer ;
      const _pos  : Integer;
      const _size : Integer
    ) ;
  {$ENDIF}
  begin
    inherited Create ;

    {$IFDEF MANAGED}
      {$IFDEF CLR}
        SetLength( FMemory, _size ) ;
        Marshal.Copy( _mem, FMemory, 0, _size ) ;
      {$ENDIF}
      {$IFDEF JAVA}
        {$WARNING '### Verify JAVA code'}
      {$ENDIF}
    {$ELSE}
      FMemory    := _mem ;
      FOwnMemory := False ;
    {$ENDIF}
    FPosition := _pos ;
    FLength   := _size ;
    doCreate ;

  end ;

  constructor TGIS_Bytes.Create(
    const _size : Integer
  ) ;
  begin
    inherited Create ;

    ReallocMem( _size ) ;
    FPosition := 0 ;
    FLength   := _size ;
    doCreate ;
  end ;

  {$IFNDEF MANAGED}
    destructor TGIS_Bytes.Destroy ;
    begin
      if FOwnMemory then
        FreeMem( FMemory ) ;

      inherited ;
    end ;
  {$ENDIF}

  procedure TGIS_Bytes.Recreate (
    const _mem   : TBytes ;
    const _pos   : Integer ;
    const _size  : Integer
  ) ;
  begin
    if FLength < _size then
      ReallocMem( _size ) ;
    if _mem <> nil then
      {$IFDEF OXYGENE}
        GisCopyMemory( _mem, 0, FMemory, 0, _size ) ;
      {$ELSE}
        Move( _mem[0], FMemory^, _size ) ;
      {$ENDIF}
    FPosition := _pos ;
    FLength   := _size ;
  end ;

  procedure TGIS_Bytes.Recreate (
    const _pos   : Integer ;
    const _count : Integer ;
    const _size  : Integer
  ) ;
  var
    {$IFDEF MANAGED}
      mem_tmp  : TBytes  ;
    {$ELSE}
      ptr_tmp  : Pointer ;
    {$ENDIF}
  var
    len : Integer ;
  begin
    len := Min( _count, FLength - FPosition ) ;
    if len > 0 then begin
      {$IFDEF MANAGED}
        {$IFDEF CLR}
          SetLength( mem_tmp, _size ) ;
          System.Buffer.BlockCopy( FMemory, FPosition, mem_tmp, _pos, len ) ;
          FMemory := mem_tmp ;
        {$ENDIF}
        {$IFDEF JAVA}
          mem_tmp := new Byte[_size] ;
          GisCopyMemory( FMemory, FPosition, mem_tmp, _pos, len ) ;
          FMemory := mem_tmp ;
        {$ENDIF}
        {$IFDEF ISLAND}
          mem_tmp := new Byte[_size] ;
          GisCopyMemory( FMemory, FPosition, mem_tmp, _pos, len ) ;
          FMemory := mem_tmp ;
        {$ENDIF}
      {$ELSE}
        GetMem( ptr_tmp, _size) ;
        System.Move( FMemory^, ptr_tmp^, len ) ;
        if FOwnMemory then
          FreeMem( FMemory ) ;
        FMemory := ptr_tmp ;
      {$ENDIF}
    end ;
    FPosition := _pos ;
    {$IFDEF MANAGED}
      {$IFDEF JAVA}
        FLength := FMemory.length ;
      {$ELSE}
        FLength := length( FMemory ) ;
      {$ENDIF}
    {$ELSE}
      FLength := _size ;
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.doCreate;
  begin

  end;

  procedure TGIS_Bytes.Reset ;
  begin
    {$IFNDEF MANAGED}
      if assigned( FMemory ) then
        FreeMem( FMemory ) ;
    {$ENDIF}
    FMemory   := nil ;
    FPosition :=  -1 ;
    FLength   :=   0 ;
  end ;

  procedure TGIS_Bytes.ReallocMem (
    const _size : Integer
  ) ;
  begin
    {$IFDEF MANAGED}
      SetLength( FMemory, _size ) ;
      FLength := length( FMemory ) ;
    {$ELSE}
      System.ReallocMem( FMemory, _size );
      FOwnMemory := True ;
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.CopyTo (
    const _dst        : TBytesBuffer ;
    const _dstOffset  : Integer ;
    const _srcOffset  : Integer ;
    const _count      : Integer
  ) ;
  var
    len : Integer ;
  begin
    len := Min( _count, FLength - FPosition ) ;
    if len > 0 then
      {$IFDEF MANAGED}
        {$IFDEF CLR}
          System.Buffer.BlockCopy( FMemory, FPosition+_srcOffset, _dst, _dstOffset, len ) ;
        {$ENDIF}
        {$IFDEF JAVA}
          GisCopyMemory( FMemory, FPosition+_srcOffset, _dst, _dstOffset, len ) ;
        {$ENDIF}
      {$ELSE}
        Move( Pointer( NativeInt( FMemory ) + FPosition + _srcOffset)^,
              Pointer( NativeInt( _dst ) + _dstOffset )^,
              len
             ) ;
      {$ENDIF}
  end ;

  procedure TGIS_Bytes.CopyFrom(
    const _src        : TBytesBuffer ;
    const _srcOffset  : Integer ;
    const _dstOffset  : Integer ;
    const _count      : Integer
  ) ;
  var
    len : Integer ;
  begin
    len := Min( _count, FLength - FPosition ) ;
    if len > 0 then
      {$IFDEF MANAGED}
        GisCopyMemory( _src, _srcOffset, FMemory, FPosition + _dstOffset, len ) ;
      {$ELSE}
        Move( Pointer( NativeInt( _src ) + _srcOffset )^,
              Pointer( NativeInt( FMemory ) + FPosition + _dstOffset )^,
              len
             ) ;
      {$ENDIF}
  end ;

  function TGIS_Bytes.ReadByte(
    const _off : Integer
  ) : Byte ;
  begin
    {$IFDEF MANAGED}
      if (FPosition + _off < FLength) then
        Result := FMemory[FPosition+_off]
      else
        Result := 0 ;
    {$ELSE}
      Result := PByte( NativeInt( FMemory ) + FPosition + _off )^ ;
    {$ENDIF}
  end ;

  function TGIS_Bytes.ReadWord(
    const _off : Integer
  ) : Word ;
  begin
    {$IFDEF MANAGED}
      if (FPosition + _off < FLength) then
        Result := BitConverter.ToUInt16( FMemory, FPosition + _off )
      else
        Result := 0 ;
    {$ELSE}
      Result := PWord( NativeInt( FMemory ) + FPosition + _off )^ ;
    {$ENDIF}
  end ;

  function TGIS_Bytes.ReadInt32 (
    const _off : Integer
  ) : Integer ;
  begin
    {$IFDEF MANAGED}
      {$IFDEF JAVA}
        Result := Integer(  Integer($FF and FMemory[FPosition+_off  ]) or
                           (Integer($FF and FMemory[FPosition+_off+1]) shl 8 ) or
                           (Integer($FF and FMemory[FPosition+_off+2]) shl 16) or
                           (Integer($FF and FMemory[FPosition+_off+3]) shl 24)
                          ) ;
      {$ELSE}
        if (FPosition + _off + 3 < FLength) then
          Result := Integer(  FMemory[FPosition+_off  ] or
                              (FMemory[FPosition+_off+1] shl 8 ) or
                              (FMemory[FPosition+_off+2] shl 16) or
                              (FMemory[FPosition+_off+3] shl 24)
                            )
        else
          Result := 0 ;
      {$ENDIF}
    {$ELSE}
      Result := PInteger( NativeInt( FMemory ) + FPosition + _off )^ ;
    {$ENDIF}
  end ;

  function TGIS_Bytes.ReadUInt32 (
    const _off : Integer
  ) : Cardinal ;
  begin
    {$IFDEF MANAGED}
      if (FPosition + _off < FLength) then
        Result := BitConverter.ToUInt32( FMemory, FPosition + _off )
      else
        Result := 0 ;
    {$ELSE}
      Result := PCardinal( NativeInt( FMemory ) + FPosition + _off )^ ;
    {$ENDIF}
  end ;

  function TGIS_Bytes.ReadDouble (
    const _off : Integer
  ) : Double ;
  begin
    {$IFDEF MANAGED}
      if (FPosition + _off < FLength) then
        Result := BitConverter.ToDouble( FMemory, FPosition + _off )
      else
        Result := 0 ;
    {$ELSE}
      Result := PDouble( NativeInt( FMemory ) + FPosition + _off )^ ;
    {$ENDIF}
  end ;

  function TGIS_Bytes.ReadSingle (
    const _off : Integer
  ) : Single ;
  begin
    {$IFDEF MANAGED}
      if (FPosition + _off < FLength) then
        Result := BitConverter.ToSingle( FMemory, FPosition + _off )
      else
        Result := 0 ;
    {$ELSE}
      Result := PSingle( NativeInt( FMemory ) + FPosition + _off )^ ;
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.WriteByte (
    const _off : Integer ;
    const _val : Byte
  ) ;
  begin
    assert( FPosition + _off + 1 <= FLength ) ;
    {$IFDEF MANAGED}
      FMemory[FPosition+_off  ] := _val and $FF;
    {$ELSE}
      PByte( NativeInt( FMemory ) + FPosition + _off )^ := _val ;
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.WriteInt32 (
    const _off : Integer ;
    const _val : Integer
  ) ;
  begin
    assert( FPosition + _off + 4 <= FLength ) ;
    {$IFDEF MANAGED}
      {$IFDEF JAVA}
        FMemory[FPosition+_off  ] := Byte(_val and $FF);
        FMemory[FPosition+_off+1] := Byte(Integer(_val shr 8) and $FF);
        FMemory[FPosition+_off+2] := Byte(Integer(_val shr 16) and $FF);
        FMemory[FPosition+_off+3] := Byte(Integer(_val shr 24) and $FF);
      {$ELSE}
        FMemory[FPosition+_off  ] := _val and $FF;
        FMemory[FPosition+_off+1] := (_val shr 8) and $FF;
        FMemory[FPosition+_off+2] := (_val shr 16) and $FF;
        FMemory[FPosition+_off+3] := (_val shr 24) and $FF;
      {$ENDIF}
    {$ELSE}
      PInteger( NativeInt( FMemory ) + FPosition + _off )^ := _val ;
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.WriteDouble (
    const _off : Integer ;
    const _val : Double
  ) ;
  begin
    assert( FPosition + _off + 8 <= FLength ) ;
    {$IFDEF MANAGED}
      {$IFDEF CLR}
        System.Buffer.BlockCopy( BitConverter.GetBytes( _val ), 0,
                                 FMemory, FPosition + _off,
                                 sizeOf(Double) ) ;
      {$ENDIF}
      {$IFDEF JAVA OR ISLAND}
        GisCopyMemory( BitConverter.GetBytes( _val ), 0,
                       FMemory, FPosition + _off,
                       sizeOf(Double)
                      ) ;
      {$ENDIF}
    {$ELSE}
      {$IFDEF NEXTGEN}
        Move( _val, Pointer( NativeInt( FMemory ) + FPosition + _off )^, 8 ) ;
      {$ELSE}
        PDouble( NativeInt( FMemory ) + FPosition + _off )^ := _val ;
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.Inc (
    const _off : Integer
  ) ;
  {$IFDEF MANAGED}
  var
    val : Integer ;
  {$ENDIF}
  begin
    {$IFDEF MANAGED}
      val := ReadInt32( _off ) + 1 ;
      WriteInt32( _off, val ) ;
    {$ELSE}
      Inc( PInteger( NativeInt(FMemory) + _off)^ ) ;
    {$ENDIF}
  end ;

  procedure TGIS_Bytes.Dec (
    const _off : Integer
  ) ;
  {$IFDEF MANAGED}
  var
    val : Integer ;
  {$ENDIF}
  begin
    {$IFDEF MANAGED}
      val := ReadInt32( _off ) - 1 ;
      WriteInt32( _off, val ) ;
    {$ELSE}
      Dec( PInteger( NativeInt(FMemory) + _off)^ ) ;
    {$ENDIF}
  end ;
{$ENDREGION}

{$REGION 'TGIS_OleStream'}
//==============================================================================
// TGIS_OleStream
//==============================================================================

  {$IFDEF JAVA OR COCOA}
    {$WARNING '### No TGIS_OleStream in JAVA'}
  {$ELSE}
    {$IFDEF MSWINDOWS}

      constructor TGIS_OleStream.Create( const _stream : IStream ) ;
      begin
        inherited Create ;
        FStream := _stream;
        {$IFDEF OXYGENE}
          // for Oxygene compatibility only
          FCanRead  := True ;
          FCanSeek  := True ;
          FCanWrite := True ;
        {$ENDIF}
      end;

      {$IFDEF OXYGENE}

        function TGIS_OleStream.fget_Length : Int64 ;
        var
          pos : Int64 ;
        begin
          pos := Seek( 0, soCurrent ) ;
          Result := Seek( 0, soEnd ) ;
          Seek( pos, soBeginning ) ;
        end ;

        function TGIS_OleStream.fget_Position : Int64 ;
        begin
          Result := Seek( 0, soCurrent ) ;
        end ;

        procedure TGIS_OleStream.fset_Position(
          const _pos : Int64
        ) ;
        begin
          Seek( _pos, soBeginning ) ;
        end ;

      {$ENDIF}

      {$IFDEF MANAGED}
        {$IFNDEF OXYGENE}
          procedure TGIS_OleStream.SetSize( _newSize : Int64 ) ;
          begin
          end ;
        {$ENDIF}
      {$ELSE}
        function TGIS_OleStream.GetIStream : IStream ;
        begin
          Result := FStream ;
        end;
      {$ENDIF}

      {$IFDEF MANAGED}
        {$IFDEF OXYGENE}
          function TGIS_OleStream.Read( const _buffer : array of Byte ;
                                              _offset : Integer ;
                                              _count  : Integer
                                      ) : Integer ;
        {$ELSE}
          function TGIS_OleStream.Read(  var  _buffer : array of Byte ;
                                              _offset : LongInt ;
                                              _count  : LongInt
                                      ) : LongInt ;
        {$ENDIF}
        var
          iptr : IntPtr ;
        begin
          assert( _offset = 0 ) ;
          iptr := Marshal.AllocHGlobal(sizeOf(Integer)) ;
          try
            FStream.Read( _buffer, _count, iptr ) ;
            Result := Marshal.ReadInt32( iptr ) ;
          finally
            Marshal.FreeHGlobal( iptr ) ;
          end ;
        end ;
      {$ELSE}
        function TGIS_OleStream.Read( var _buffer ;
                                          _count  : LongInt
                                    ) : LongInt ;
        begin
          OleCheck( FStream.Read( @_buffer, _count, @Result ) ) ;
        end;
      {$ENDIF}

      {$IFDEF MANAGED}
        {$IFDEF OXYGENE}
          procedure TGIS_OleStream.Write( const _buffer : array of Byte ;
                                                _offset : Integer ;
                                                _count  : Integer
                                        ) ;
        {$ELSE}
          function  TGIS_OleStream.Write( const _buffer : array of Byte ;
                                                _offset : LongInt ;
                                                _count  : LongInt
                                        ) : LongInt ;
        {$ENDIF}
        var
          iptr : IntPtr  ;
        begin
          assert( _offset = 0 ) ;
          iptr := Marshal.AllocHGlobal(sizeOf(Integer)) ;
          try
            FStream.Write( _buffer, _count, iptr ) ;
            {$IFNDEF OXYGENE}
              Result := Marshal.ReadInt32( iptr ) ;
            {$ENDIF}
          finally
            Marshal.FreeHGlobal( iptr ) ;
          end ;
        end;
      {$ELSE}
        function TGIS_OleStream.Write( const _buffer ;
                                             _count  : LongInt
                                     ) : LongInt ;
        begin
          OleCheck( FStream.Write( @_buffer, _count, @Result ) ) ;
        end;
      {$ENDIF}

      {$IFDEF MANAGED}
        function TGIS_OleStream.Seek( const _offset : Int64 ;
                                            _origin : TSeekOrigin
                                    ) : Int64 ;
        var
          iptr : IntPtr ;
        begin
          iptr := Marshal.AllocHGlobal(sizeOf(Integer)) ;
          try
            FStream.Seek( _offset, Integer(_origin), iptr ) ;
            Result := Marshal.ReadInt32( iptr ) ;
          finally
            Marshal.FreeHGlobal( iptr ) ;
          end ;
        end;
      {$ELSE}
        function TGIS_OleStream.Seek( _offset : LongInt ;
                                      _origin : Word
                                    ) : LongInt ;
        var
          pos: {$IFDEF LEVEL_XE8_RTL}
                 UInt64;
               {$ELSE}
                 Largeint;
               {$ENDIF}
        begin
          OleCheck( FStream.Seek( _offset, _origin, pos ) ) ;
          Result := LongInt( pos ) ;
        end;
      {$ENDIF}

      {$IFDEF OXYGENE}
        procedure TGIS_OleStream.Flush ;
        begin
          // stub - for Oxygene compatibility only
        end ;

        procedure TGIS_OleStream.SetLength(
          const _val : Int64
        ) ;
        begin
          // stub - for Oxygene compatibility only
        end ;
      {$ENDIF}

    {$ENDIF}
  {$ENDIF}

//==============================================================================
// TGIS_StreamAdapter
//==============================================================================

  {$IFNDEF MANAGED}
    function TGIS_StreamAdapter.Stat(
      out statstg : TStatStg ;
      grfStatFlag : {$IFDEF LEVEL_XE8_RTL}
                      DWORD
                    {$ELSE}
                      LongInt
                    {$ENDIF}
    ) : HResult ;

      {$IFDEF MSWINDOWS}
        function DateTimeToFileTime( _dateTime : TDateTime ) : TFileTime ;
        var
          file_time   : TFileTime ;
          system_time : TSystemTime ;
        begin
          DateTimeToSystemTime( _dateTime, system_time ) ;
          SystemTimeToFileTime( system_time, file_time ) ;
          Result := file_time;
        end ;
      {$ENDIF}

    begin
      Result := S_OK;
      try
        if (@statstg <> nil) then
        begin
          FillChar(statstg, sizeOf(statstg), 0);
          with statstg do
          begin
            dwType := STGTY_STREAM;
            cbSize := Stream.Size;

            {$IFDEF MSWINDOWS}
              mtime  := DateTimeToFileTime(Now);
              ctime  := DateTimeToFileTime(Now);
              atime  := DateTimeToFileTime(Now);
            {$ELSE}
              {$MESSAGE WARN '### Verify FMX code on OSX ## '}
              mtime.dwLowDateTime  := 0;
              mtime.dwHighDateTime := 0;
              ctime.dwLowDateTime  := 0;
              ctime.dwHighDateTime := 0;
              atime.dwLowDateTime  := 0;
              atime.dwHighDateTime := 0;
            {$ENDIF}
            grfLocksSupported := LOCK_WRITE;
          end;
        end;
      except
        Result := E_UNEXPECTED;
      end;
    end;
  {$ENDIF}
{$ENDREGION}

{$REGION 'TGIS_ThreadClass'}
//=============================================================================
// TGIS_ThreadClass
//=============================================================================

  constructor TGIS_ThreadClass.Create ;
  begin
    inherited Create ;
    criticalSection := TCriticalSection.Create ;
  end ;

  procedure TGIS_ThreadClass.doDestroy ;
  begin
    FreeObject( criticalSection ) ;
    inherited ;
  end ;

  procedure TGIS_ThreadClass.LockThread ;
  begin
    criticalSection.Enter ;
  end;

  procedure TGIS_ThreadClass.UnlockThread ;
  begin
    criticalSection.Leave ;
  end;
{$ENDREGION}

{$REGION 'TGIS_PasswordList'}
//==============================================================================
// TGIS_PasswordList
//==============================================================================

  constructor TGIS_PasswordList.Create ;
  begin
    inherited ;

    oList := TGIS_StringList.Create ;
  end ;

  procedure TGIS_PasswordList.doDestroy ;
  begin
    FreeObject( oList ) ;
    inherited ;
  end ;

  procedure TGIS_PasswordList.Clear ;
  begin
    LockThread ;
    try
      oList.Clear ;
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_PasswordList.Merge ( const _lst : TGIS_PasswordList ) ;
  var
    i   : Integer ;
    tmp : String  ;
  begin
    if not assigned( _lst ) then exit ;

    LockThread ;
    try
      for i:= 0 to _lst.oList.Count - 1 do begin
        tmp := _lst.oList.Names[ i ] ;
        oList.Values[ tmp ] := _lst.oList.Values[ tmp ] ;
      end ;
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_PasswordList.Add( const _name  : String;
                                   const _key   : String;
                                   const _value : String
                                 ) ;
  begin
    LockThread ;
    try
      if not IsStringEmpty( _value ) then
        oList.Values[ UpperCase( _name + '.' + _key ) ] := _value ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_PasswordList.Get( const _name      : String;
                                  const _key       : String
                                ) : String ;
  begin
    LockThread ;
    try
     Result := oList.Values[ UpperCase( _name  + '.' +  _key ) ] ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_PasswordList.Check( const _name  : String;
                                    const _key   : String;
                                    const _value : String
                                  ) : Boolean ;
  begin
    LockThread ;
    try
      Result := oList.Values[ UpperCase( _name  + '.' +  _key ) ] = _value ;
    finally
      UnlockThread ;
    end;
  end ;
{$ENDREGION}

{$REGION 'TGIS_AliasList'}
//==============================================================================
// TGIS_AliasList
//==============================================================================

  constructor TGIS_AliasList.Create ;
  begin
    inherited Create ;

    oList := TGIS_StringList.Create ;
  end ;

  procedure TGIS_AliasList.doDestroy ;
  begin
    FreeObject( oList ) ;
    inherited ;
  end ;

  procedure TGIS_AliasList.Clear ;
  begin
    LockThread ;
    try
      oList.Clear ;
    finally
      UnlockThread ;
    end;
  end ;

  procedure TGIS_AliasList.Add( const _alias : String;
                                const _value : String
                              ) ;
  begin
    LockThread ;
    try
      if not IsStringEmpty( _value ) then
        oList.Values[ UpperCase( _alias ) ] := _value ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_AliasList.Get( const _alias : String
                             ) : String ;
  begin
    LockThread ;
    try
      Result := oList.Values[ UpperCase( _alias ) ] ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_AliasList.Resolve( const _string    : String
                                 ) : String ;
  begin
    LockThread ;
    try
      Result := TemplateProducer( _string, oList, nil, True ) ;
    finally
      UnlockThread ;
    end;
  end;
{$ENDREGION}

{$REGION 'TGIS_ThreadStorage'}
//=============================================================================
// TGIS_ThreadStorage
//=============================================================================

  constructor TGIS_ThreadStorage.Create  ;
  begin
    inherited ;

    FGisAliasList := TGIS_AliasList.Create ;

    {$IFNDEF MSWINDOWS}
      {$MESSAGE WARN '### Verify FMX code on OSX ## '}
      FThreadId := 0 ;
    {$ELSE}
      FThreadId := GetCurrentThreadId ;
    {$ENDIF}
  end ;

  procedure TGIS_ThreadStorage.doDestroy ;
  begin
    FreeObject( FGisAliasList ) ;
    inherited ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_Tokenizer'}
//=============================================================================
// TGIS_Tokenizer
//=============================================================================

  constructor TGIS_Tokenizer.Create ;
  begin
    inherited Create ;

    FResult := TGIS_StringList.Create;
    FIndex  := 0 ;
  end ;

  procedure TGIS_Tokenizer.doDestroy ;
  begin
    FreeObject( FResult ) ;
    inherited ;
  end ;

  function TGIS_Tokenizer.trimQuotes( const _str : String ) : String ;
  var
    flg : TReplaceFlags ;
  begin
    Result := Trim( _str ) ;
    if IsStringEmpty( Result ) then exit ;

    {$IFDEF OXYGENE}
      flg := [ TReplaceFlag.rfReplaceAll ] ;
    {$ELSE}
      flg := [ rfReplaceAll ] ;
    {$ENDIF}

    if not IsStringEmpty( sQuote ) then begin
      Result := StringReplace( Result, sDblQuote, sQuote, flg )
    end
    else if Result[StringFirst] = Result[ StringLast(Result) ] then begin
      if Result[StringFirst] = '''' then begin
        Result := Copy( Result, StringFirst + 1, length( Result ) - 2 ) ;
        Result := StringReplace( Result, '''''', '''',  flg )
      end
      else if Result[StringFirst] = '"' then begin
        Result := Copy( Result, StringFirst + 1, length( Result ) - 2 ) ;
        Result := StringReplace( Result, '""', '"', flg)
      end
    end ;
  end ;

  procedure TGIS_Tokenizer.Execute( const _str    : String        ;
                                    const _delims : array of Char
                                  ) ;
  begin
    Execute( _str, _delims, False ) ;
  end;

  procedure TGIS_Tokenizer.Execute( const _str    : String        ;
                                    const _delims : array of Char ;
                                    const _quotes : Boolean
                                  ) ;
  var
    i         : Integer ;
    j         : Integer ;
    found     : Boolean ;
    stmp      : String  ;
    inquotes  : Boolean ;
    skip      : Boolean ;
    c         : Char    ;
    cnt       : Integer ;
    strt      : Integer ;
  begin
    FResult.Clear ;
    FIndex    := 0  ;
    sQuote    := '' ;
    sDblQuote := '' ;

    stmp := Trim( _str ) ;

    found    := False ;
    inquotes := False ;
    cnt      := 0     ;
    strt     := 0     ;

    for i:= StringFirst to StringLast( stmp ) do begin
      c := stmp[i] ;
      if _quotes and (c = '"') then inquotes := not inquotes ;

      if (found = True) then begin
        skip := False ;
        for j := 0 to high(_delims) do begin
          if ( ( c = ' ' )  // ignore space
               or
               ( c = #09 )  // ignore tab
              ) or
             ( c = _delims[j] ) then
          begin
            skip := True ;
            break ;
          end ;
        end ;
        if skip then Continue ;
      end ;

      found := False ;
      for j := 0 to high(_delims) do begin
        if c = _delims[j] then begin
          if _quotes and inquotes then break ;
          found := True ;
          break ;
        end ;
      end ;

      if found then begin
        if cnt > 0 then
          FResult.Add( trimQuotes( Copy( stmp, strt, cnt ) ) ) ;
        cnt := 0  ;
        continue ;
      end;

      if cnt = 0 then
        strt := i ;
      inc( cnt ) ;

    end ;

    if cnt > 0 then
      FResult.Add( trimQuotes( Copy( stmp, strt, cnt ) ) ) ;
  end ;

  procedure TGIS_Tokenizer.ExecuteEx( const _str   : String
                                    ) ;
  begin
    ExecuteEx( _str, ',', '"' ) ;
  end;

  procedure TGIS_Tokenizer.ExecuteEx( const _str   : String ;
                                      const _delim : Char
                                    ) ;
  begin
    ExecuteEx( _str, _delim, '"' ) ;
  end;

  procedure TGIS_Tokenizer.ExecuteEx( const _str   : String ;
                                      const _delim : Char   ;
                                      const _quote : Char
                                    ) ;
  var
    i         : Integer ;
    istart    : Integer ;
    last      : Integer ;
    s         : String  ;
    bdblquote : Boolean ;

    // skip spaces
    procedure skip_left_spaces ;
    var
      c : Char ;
    begin
      if _delim <> #9 then begin
          repeat
            if i > last then break ;
            c := _str[i] ;
            if ord( c ) > 127 then break ;
            if not ( ( c >= #1 ) and ( c <= #32 ) ) then break ;
            inc( i ) ;
          until False ;
      end
      else begin
          repeat
            if i > last then break ;
            c := _str[i] ;
            if ord( c ) > 127 then break ;
            if not ( ( ( c >= #1  ) and ( c <= #8  ) ) or
                     ( ( c >= #10 ) and ( c <= #32 ) )
                   )
            then
              break ;
            inc( i ) ;
          until False ;
      end ;
    end ;

    // skip trailing spaces
    procedure skip_right_spaces ;
    var
      c : Char ;
    begin
      if _delim <> #9 then begin
          repeat
            inc( i ) ;
            if i > last then break ;
            c := _str[i] ;
            if ord( c ) > 126 then break ;
            if not ( ( c >= #1 ) and ( c <= #32 ) ) then break ;
          until False ;
      end
      else begin
          repeat
            inc( i ) ;
            if i > last then break ;
            c := _str[i] ;
            if ord( c ) > 127 then break ;
            if not ( ( ( c >= #1  ) and ( c <= #8  ) ) or
                     ( ( c >= #10 ) and ( c <= #32 ) )
                   )
            then
              break ;
          until False ;
      end ;
    end ;

    // calculates last not spaces position
    function back_right_spaces( _start : Integer ) : Integer ;
    var
      c : Char ;
    begin
      Result := i ;
        repeat
          if Result <= _start then break ;
          c := _str[Result-1] ;
          if ord( c ) > 127 then break ;
          if not ( ( ( c >= #1  ) and ( c <= #8  ) ) or
                   ( ( c >= #10 ) and ( c <= #32 ) )
                 )
          then
            break ;
          dec( Result ) ;
        until False ;
    end ;

  begin
    FResult.Clear;
    FIndex    := 0 ;
    sQuote    := _quote ;
    sDblQuote := _quote + _quote ;

    i    := StringFirst  ;
    last := StringLast( _str ) ;

    skip_left_spaces ;

    while i <= last do begin

      if _str[i] = _quote then begin // quoted String
        inc( i ) ;

        istart := i ;

        // until ending quote
        bdblquote := False ;
        while ( i <= last ) do begin
          if      ( _str[i] <> _quote ) then
                  inc( i )
          else if ( i < last ) and ( _str[i+1] = _quote ) then begin // "" found
                  inc( i, 2 ) ;
                  bdblquote := True ;
          end
          else
                  break ;
        end ;

        if bdblquote then
          s := trimQuotes( Copy( _str, istart, i - istart ) )
        else
          s := Copy( _str, istart, i - istart ) ;

        inc( i ) ;
      end
      else begin // normal parameter
        istart := i ;

        // until delimiter
        while ( i <= last ) and ( _str[i] <> _delim ) do
          inc( i ) ;
        s := Copy(_str, istart, back_right_spaces( istart ) - istart ) ;
      end ;

      FResult.Add( s ) ;

      skip_left_spaces ;

      if i <= last then begin
        if _str[i] = _delim then begin
          istart := i ;
          inc( istart ) ;
          if istart > last then
            FResult.Add('') ;
          skip_right_spaces ;
        end
        else
        if _delim <> ' ' then begin
          FResult.Clear ;
          exit ;
        end ;
      end ;

    end ;
  end ;

  procedure TGIS_Tokenizer.MoveFirst ;
  begin
    FIndex := 0 ;
  end ;

  function  TGIS_Tokenizer.Eof : Boolean ;
  begin
    Result := FIndex >= FResult.Count ;
  end ;

  procedure TGIS_Tokenizer.MoveNext ;
  begin
    inc( FIndex ) ;
  end ;

  function  TGIS_Tokenizer.CurrentToken : String ;
  begin
    if ( FIndex >=0 ) and ( FIndex < FResult.Count ) then
      Result := FResult[ FIndex ]
    else
      Result := '' ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_PointList'}
//==============================================================================
// TGIS_PointsList
//==============================================================================

  {$IFDEF MANAGED}
    procedure TGIS_PointList.Dispose ;
  {$ELSE}
    destructor TGIS_PointList.Destroy ;
  {$ENDIF}
  begin
    Clear ;
    inherited ;
  end ;

  function TGIS_PointList.fget_Item(
    const _index : Integer
  ) : TGIS_Point ;
  begin
    Result := T_Point( inherited Items[ _index ] ).FValue ;
  end ;

  procedure TGIS_PointList.fset_Item(
    const _index : Integer ;
    const _val   : TGIS_Point
  ) ;
  begin
    T_Point( inherited Items[ _index ] ).FValue := _TGIS_Point( _val ) ;
  end ;

  function TGIS_PointList.Add ( const _val : TGIS_Point ) : Integer ;
  begin
    Result := inherited Add( T_Point.Create( _val ) ) ;
  end ;

  procedure TGIS_PointList.Insert( const _index : Integer ;
                                   const _val   : TGIS_Point
                                 ) ;
  begin
    inherited Insert( _index, T_Point.Create( _val ) ) ;
  end;

//==============================================================================
// TGIS_Point3DList
//==============================================================================

  {$IFDEF MANAGED}
    procedure TGIS_Point3DList.Dispose ;
  {$ELSE}
    destructor TGIS_Point3DList.Destroy ;
  {$ENDIF}
  begin
    Clear ;
    inherited ;
  end ;

  function TGIS_Point3DList.fget_Item(
    const _index : Integer
  ) : TGIS_Point3D ;
  begin
    Result := T_Point3D( inherited Items[ _index ] ).FValue ;
  end ;

  procedure TGIS_Point3DList.fset_Item(
    const _index : Integer ;
    const _val   : TGIS_Point3D
  ) ;
  begin
    T_Point3D( inherited Items[ _index ] ).FValue := _TGIS_Point3D(_val) ;
  end ;

  function TGIS_Point3DList.Add ( const _val : TGIS_Point3D ) : Integer ;
  begin
    Result := inherited Add( T_Point3D.Create( _val ) ) ;
  end ;

  procedure TGIS_Point3DList.Insert( const _index : Integer ;
                                     const _val   : TGIS_Point3D
                                   ) ;
  begin
    inherited Insert( _index, T_Point3D.Create( _val ) ) ;
  end;
{$ENDREGION}

{$REGION 'TGIS_VariantList'}
//==============================================================================
// TGIS_VariantList
//==============================================================================

  {$IFDEF MANAGED}
    procedure TGIS_VariantList.Dispose ;
  {$ELSE}
    destructor TGIS_VariantList.Destroy ;
  {$ENDIF}
  begin
    Clear ;
    inherited ;
  end ;

  function TGIS_VariantList.fget_Item(
    const _index : Integer
  ) : Variant ;
  begin
    Result := T_Variant( inherited Items[ _index ] ).FValue ;
  end ;

  procedure TGIS_VariantList.fset_Item(
    const _index : Integer ;
    const _val   : Variant
  ) ;
  begin
    T_Variant( inherited Items[ _index ] ).FValue := _val ;
  end ;

  function TGIS_VariantList.Add ( const _val : Variant ) : Integer ;
  begin
    Result := inherited Add( T_Variant.Create( _val ) ) ;
  end ;

  procedure TGIS_VariantList.Insert( const _index : Integer ;
                                     const _val   : Variant
                                   ) ;
  begin
    inherited Insert( _index, T_Variant.Create( _val ) ) ;
  end;

  { Used to sort of variant list.
  }
  function compare_fun( const Item1, Item2 : TObject ): Integer ;
  var
    itm1 : T_Variant ;
    itm2 : T_Variant ;
  begin
    itm1 := T_Variant( Item1 ) ;
    itm2 := T_Variant( Item2 ) ;

    {$IFDEF OXYGENE}
      Result := VarCompareInt( itm1.FValue, itm2.FValue ) ;
    {$ELSE}
      if      itm1.FValue = itm2.FValue then Result :=  0
      else if itm1.FValue < itm2.FValue then Result := -1
      else                                   Result :=  1
    {$ENDIF}
  end ;

  procedure TGIS_VariantList.Sort ;
  begin
    {$IFDEF OXYGENE}
      inherited Sort( @compare_fun )
    {$ELSE}
      inherited Sort( compare_fun )
    {$ENDIF}
  end ;
{$ENDREGION}

{$REGION 'TGIS_FieldList'}
//==============================================================================
// TGIS_FieldList
//==============================================================================

  {$IFDEF MANAGED}
    procedure TGIS_FieldList.Dispose ;
  {$ELSE}
    destructor TGIS_FieldList.Destroy ;
  {$ENDIF}
  begin
    Clear ;
    inherited ;
  end ;

  function TGIS_FieldList.find( const _field : Integer ;
                                  var _index : Integer
                               ) : Boolean ;
  var
    L, H, I : Integer ;
  begin
    Result := False ;
    L := 0 ;
    H := Count - 1 ;

    while L <= H do begin
      I := (L + H) shr 1;
      If T_Field( inherited Items[I] ).FField < _field then
        L := I + 1
      else begin
        H := I - 1 ;
        If T_Field( inherited Items[I] ).FField = _field then begin
          Result := True ;
          L := I ;
        end ;
      end ;
    end ;
    _index := L ;
  end ;

  function TGIS_FieldList.fget_Item(
    const _field : Integer
  ) : Variant ;
  var
    idx : Integer ;
  begin
    if find( _field, idx ) then
      Result := T_Field( inherited Items[ idx ] ).FValue
    else
      Result := Unassigned ;
  end ;

  procedure TGIS_FieldList.fset_Item(
    const _field : Integer ;
    const _val   : Variant
  ) ;
  var
    idx : Integer ;
  begin
    if find( _field, idx ) then
      T_Field( inherited Items[ idx ] ).FValue := _val
    else begin
      if VarIsEmpty( _val ) then exit ;

      inherited Insert( idx, T_Field.Create( _field, _val ) ) ;
    end ;
  end ;

  function TGIS_FieldList.Add ( const _field : Integer ;
                                const _val : Variant
                               ) : Integer ;
  begin
    Result := inherited Add( T_Field.Create( _field, _val ) ) ;
  end ;

  procedure TGIS_FieldList.Insert( const _index : Integer ;
                                   const _field : Integer ;
                                   const _val   : Variant
                                   ) ;
  begin
    inherited Insert( _index, T_Field.Create( _field, _val ) ) ;
  end;

  { Used to sort a field list.
  }
  function compare_field_fun( const Item1, Item2 : TObject ): Integer ;
  var
    itm1 : T_Field ;
    itm2 : T_Field ;
  begin
    itm1 := T_Field( Item1 ) ;
    itm2 := T_Field( Item2 ) ;

    {$IFDEF OXYGENE}
      Result := VarCompareInt( itm1, itm2 ) ;
    {$ELSE}
      if      itm1.FField = itm2.FField then Result :=  0
      else if itm1.FField < itm2.FField then Result := -1
      else                                   Result :=  1
    {$ENDIF}
  end ;

  procedure TGIS_FieldList.Sort ;
  begin
    {$IFDEF OXYGENE}
      inherited Sort( @compare_field_fun )
    {$ELSE}
      inherited Sort( compare_field_fun )
    {$ENDIF}
  end ;

  function TGIS_FieldList.Exists( const _field : Integer
                                 ) : Boolean ;
  var
    idx : Integer ;
  begin
    Result := find( _field, idx ) ;
  end ;

  procedure TGIS_FieldList.CopyTo( const _list : TGIS_FieldList
                                 ) ;
  var
    i : Integer ;
  begin
    _list.Clear ;
    for i := 0 to Count - 1 do
      _list.Add( T_Field( inherited Items[ i ] ).FField,
                 T_Field( inherited Items[ i ] ).FValue
                ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_Stack'}
//==============================================================================
// TGIS_Stack
//==============================================================================

  procedure TGIS_Stack<T>.Assign( const _source : TGIS_Stack<T> ) ;
  {$IFDEF DCC}
    var
      itm : T ;
  {$ENDIF}
  begin
    Clear ;

    {$IFDEF CLR}
      var arr = _source.ToArray();
      Array.Reverse(arr);

      for elm in arr do
        Push( elm );
    {$ELSE}
      for itm in _source do
        Push( itm ) ;
    {$ENDIF}
  end ;

  procedure TGIS_Stack<T>.Push( const _item : T ) ;
  begin
    inherited Push( _item ) ;
  end ;

  function TGIS_Stack<T>.Top : T ;
  begin
    if IsEmpty then
      Result := default(T)
    else
      Result := inherited Peek ;
  end ;

  function TGIS_Stack<T>.Pop : T ;
  begin
    if IsEmpty then
      Result := default(T)
    else
      Result := inherited Pop ;
  end ;

  function TGIS_Stack<T>.IsEmpty : Boolean ;
  begin
    Result := Count = 0 ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_KeyValueList'}
//==============================================================================
// TGIS_KeyValueList
//==============================================================================

  {$IFDEF ISLAND}
    constructor TGIS_KeyValueList<T, U>.Create ;
     begin
      inherited ;
    end;
  {$ENDIF}

  function  TGIS_KeyValueList<T, U>.fget_Item(
    const _index : Integer
  ) : {$IFDEF DCC} TPair<T,U> {$ENDIF}
      {$IFDEF OXYGENE} TPair<T,U> {$ENDIF};
  begin
    {$IFDEF DCC}
      Result := inherited Items[_index] ;
    {$ENDIF}
    {$IFDEF JAVA}
      Result := get(_index) ;
    {$ENDIF}
    {$IFDEF CLR}
      Result := inherited Item[_index] ;
    {$ENDIF}
  end ;

  procedure TGIS_KeyValueList<T, U>.fset_Item(
    const _index : Integer;
    const _value : {$IFDEF DCC} TPair<T,U> {$ENDIF}
                   {$IFDEF OXYGENE} TPair<T,U> {$ENDIF}
  );
  begin
    {$IFDEF DCC}
      inherited Items[_index] := _value ;
    {$ENDIF}
    {$IFDEF JAVA}
      &set(_index, _value) ;
    {$ENDIF}
    {$IFDEF CLR}
      inherited Item[_index] := _value ;
    {$ENDIF}
  end ;

  {$IFDEF JAVA}
    function TGIS_KeyValueList<T,U>.fget_Count : Integer ;
    begin
      Result := size ;
    end ;
  {$ENDIF}

  procedure TGIS_KeyValueList<T, U>.Add(
    const _key    : T ;
    const _value  : U
  ) ;
  begin
    {$IFDEF CLR}
      inherited &Add( new KeyValuePair<T, U>(_key, _value) ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      &add( new TPair<T, U>(_key, _value) ) ;
    {$ENDIF}
    {$IFDEF DCC}
      inherited Add( TPair<T, U>.Create(_key, _value) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_KeyValueList<T, U>.Delete(
    const _index : Integer
  ) ;
  begin
    {$IFDEF CLR}
      inherited RemoveAt(_index) ;
    {$ENDIF}
    {$IFDEF JAVA}
      &remove(_index) ;
    {$ENDIF}
    {$IFDEF DCC}
      inherited Delete( _index ) ;
    {$ENDIF}
  end ;

{$ENDREGION}

{$REGION 'EGIS_Exception'}
  // Utility function for EGIS_Exception.
  // _message    message of the exception; message must be in format
  //            '000 Message %s %d';
  // _refstring additional String referring to exception
  //            (like file name in import filters, etc.)
  // _refcode   additional code referring to exception
  //            (like line number in import filters etc.)
  function format_msg(
    const _message   : String ;
    const _refstring : String ;
    const _refcode   : Integer
  ) : String ;
  begin
    if IsStringEmpty( _message ) then begin
      result := '' ;
      exit ;
    end;
    try
      Result := Format( _message, [_refstring, _refcode] )
    except
      try
        Result := Format( _message, [_refcode, _refstring] )
      except
        Result := _message ;
      end;
    end ;
  end;

  constructor EGIS_Exception.Create(
    const _message   : String ;
    const _refstring : String ;
    const _refcode   : Integer
  ) ;
  begin
    inherited Create( format_msg( _message, _refstring, _refcode ) ) ;

    try
      if not IsStringEmpty( _message ) then
        ErrNo   := StrToInt( Copy( _message, StringFirst, 3 ) )
      else
        ErrNo   := 0 ;
    except
      ErrNo   := 0 ;
    end ;

    RefString := _refstring ;
    RefCode   := _refcode   ;

    TGIS_Logger.AsTrace( 'EGIS_Exception: ' +
                         format_msg( _message, _refstring, _refcode ),
                         ''
                       ) ;
  end ;

  constructor EGIS_Exception.Create(
    const _message   : String ;
    const _refstring : String ;
    const _refcode   : Integer ;
    const _exception : Exception
  ) ;
  begin
    {$IFDEF MANAGED}
      inherited Create( format_msg( _message, _refstring, _refcode ) {$IFNDEF ISLAND}, _exception {$ENDIF} ) ;

      try
        ErrNo   := StrToInt( Copy( _message, StringFirst, 3 ) ) ;
      except
        ErrNo   := 0 ;
      end ;

      RefString := _refstring ;
      RefCode   := _refcode   ;
    {$ELSE}
      Create( _message + #13#10 + _exception.Message , _refstring, _refcode ) ;
    {$ENDIF}

    TGIS_Logger.AsTrace( 'EGIS_Exception: ' +
                         format_msg( _message, _refstring, _refcode ),
                         _exception.Message
                       ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_PaintException'}
//=============================================================================
// EGIS_PaintException
//=============================================================================

  constructor EGIS_PaintException.Create(
    const _layer     : String ;
    const _exception : Exception
  ) ;
  begin
    assert( _exception <> nil ) ;

    {$IFDEF CLR}
      inherited Create( Format( _rsrc( GIS_RS_ERR_PAINT_EXCEPTION ),
                                [ _layer, _exception.Message ]
                              ), '', 0, _exception
                      ) ;
    {$ELSE}
      inherited Create( Format( _rsrc( GIS_RS_ERR_PAINT_EXCEPTION ),
                                [ _layer, _exception.Message ]
                              ), '', 0
                      ) ;
    {$ENDIF}
    Layer            := _layer ;
    ExceptionMessage := _exception.Message ;
  end;

{$IFDEF OXYGENE}
//=============================================================================
// TGIS_PaintExceptionArgs
//=============================================================================

  constructor TGIS_PaintExceptionEventArgs.Create(
    const _exception : EGIS_PaintException
  ) ;
  begin
    inherited Create ;
    FException := _exception ;
  end ;
{$ENDIF}
{$ENDREGION}

{$REGION 'TGIS_FieldInfo'}
//==============================================================================
// TGIS_FieldInfo
//==============================================================================

  constructor TGIS_FieldInfo.Create ;
  begin
    inherited ;
    FFlags := [] ;
    FRules := nil ;
  end;

  procedure TGIS_FieldInfo.doDestroy ;
  begin
    if assigned( FRules ) then
      FreeObject( FRules );
    inherited ;
  end;

  procedure TGIS_FieldInfo.checkFlags ;
  begin
    if not FTemporary and not FDeleted and not FFileFormat then
      FFlags := FFlags + [ TGIS_FieldFlags.Exportable ]
    else
      FFlags := FFlags - [ TGIS_FieldFlags.Exportable ] ;

    if not FTemporary and not FDeleted and not FFileFormat and FSaved then
      FFlags := FFlags + [ TGIS_FieldFlags.Saveable ]
    else
      FFlags := FFlags - [ TGIS_FieldFlags.Saveable ] ;

    if FReadOnly or FFileFormat then
      FFlags := FFlags + [ TGIS_FieldFlags.ReadOnly ]
    else
      FFlags := FFlags - [ TGIS_FieldFlags.ReadOnly ] ;

    if not FDeleted and not FHidden and not FFileFormat then
      FFlags := FFlags + [ TGIS_FieldFlags.Visible ]
    else
      FFlags := FFlags - [ TGIS_FieldFlags.Visible ] ;
  end ;

  procedure TGIS_FieldInfo.fset_Deleted(
    const _value : Boolean
  ) ;
  begin
    FDeleted := _value ;
    checkFlags ;
  end ;

  procedure TGIS_FieldInfo.fset_Saved(
    const _value : Boolean
  ) ;
  begin
    FSaved := _value ;
    checkFlags ;
  end ;

  procedure TGIS_FieldInfo.fset_Hidden(
    const _value : Boolean
  ) ;
  begin
    FHidden := _value ;
    checkFlags ;
  end ;

  procedure TGIS_FieldInfo.fset_ReadOnly(
    const _value : Boolean
  ) ;
  begin
    FReadOnly := _value ;
    checkFlags ;
  end ;

  procedure TGIS_FieldInfo.fset_FileFormat(
    const _value : Boolean
  ) ;
  begin
    FFileFormat := _value ;
    checkFlags ;
  end ;

  procedure TGIS_FieldInfo.fset_Temporary(
    const _value : Boolean
  ) ;
  begin
    FTemporary := _value ;
    checkFlags ;
  end ;

  procedure TGIS_FieldInfo.fset_IsUID(
    const _value : Boolean
  ) ;
  begin
    FIsUID := _value ;
    checkFlags ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_Textures'}
//==============================================================================
// TGIS_Textures
//==============================================================================

  procedure TGIS_Textures.Assign(
    const _textures : TGIS_Textures
  ) ;
  begin
    FNumTextures      := _textures.FNumTextures ;
    FNumParts         := _textures.FNumParts ;
    FTextureDimension := _textures.FTextureDimension ;
    FTextureParts     := _textures.FTextureParts ;
    FTextureCoords    := _textures.FTextureCoords ;
    FTextureCoordsLen := _textures.FTextureCoordsLen ;
  end ;

  procedure TGIS_Textures.Resize(
    const _numParts     : Integer ;
    const _numTextures  : Integer ;
    const _numDimension : Integer
  ) ;
  begin
    FNumTextures      := _numTextures ;
    FTextureDimension := _numDimension ;
    FNumParts         := _numParts ;
    FTextureCoordsLen := _numTextures*_numDimension ;

    SetLength( FTextureParts, _numParts ) ;
    SetLength( FTextureCoords, FTextureCoordsLen ) ;
  end ;

  function  TGIS_Textures.GetPartSize(
    const _part : Integer
  ) : Integer ;
  begin
    Result := 0 ;
    if ( _part >=0 ) then begin
      if ( _part < FNumParts-1 ) then
        Result := FTextureParts[_part+1]-FTextureParts[_part]
      else if _part = FNumParts-1 then
        Result := FNumTextures - FTextureParts[_part]
    end
  end ;

  procedure TGIS_Textures.SetPartOffset(
    const _part   : Integer ;
    const _offset : Integer
  ) ;
  begin
    if ( _part >=0 ) and ( _part < FNumParts ) then
      FTextureParts[_part] := _offset ;
  end ;

  function  TGIS_Textures.GetTextureCoord(
    const _part  : Integer ;
    const _index : Integer
  ) : Single ;
  var
    idx : Integer ;
  begin
    if ( _index >=0 ) and ( _index < FTextureCoordsLen ) and
       ( _part  >=0 ) and ( _part  < FNumParts         ) then begin
      idx := FTextureParts[_part]*FTextureDimension+_index ;
      if idx < FTextureCoordsLen then
        Result := FTextureCoords[idx]
      else
        Result := 0 ;
    end
    else
      Result := 0 ;
  end ;

  procedure TGIS_Textures.SetTextureCoord(
    const _index   : Integer ;
    const _texture : Single
   ) ;
  begin
    if ( _index >=0 ) and ( _index < FTextureCoordsLen ) then
      FTextureCoords[_index] := _texture ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_Materials'}
//==============================================================================
// TGIS_Materials
//==============================================================================

  procedure TGIS_Materials.Assign(
    const _materials : TGIS_Materials
  ) ;
  begin
    FNumMaterials    := _materials.FNumMaterials ;
    FCompressionType := _materials.FCompressionType ;
    FMaterials       := _materials.FMaterials ;
  end ;

  procedure TGIS_Materials.Resize(
    const _numMaterials : Integer
  ) ;
  begin
    SetLength( FMaterials, _numMaterials ) ;
    FNumMaterials := _numMaterials ;
  end ;

  procedure TGIS_Materials.fset_Material(
    const _index : Integer ;
    const _material : TGIS_Material
   ) ;
  begin
    if ( _index >=0 ) and ( _index < FNumMaterials ) then
      FMaterials[_index] := _material ;
  end ;

  function TGIS_Materials.fget_Material(
    const _index : Integer
  ) : TGIS_Material ;
  begin
    if ( _index >=0 ) and ( _index < FNumMaterials ) then
      Result := FMaterials[_index] ;
  end;
{$ENDREGION}

{$REGION 'TGIS_Normals'}
//==============================================================================
// TGIS_Normals
//==============================================================================

  procedure TGIS_Normals.Assign(
    const _normals : TGIS_Normals
  ) ;
  begin
    FNumNormals := _normals.FNumNormals ;
    FNormals    := _normals.FNormals ;
  end ;

  procedure TGIS_Normals.Resize(
    const _size : Integer
  ) ;
  begin
    SetLength( FNormals, _size ) ;
    FNumNormals := _size ;
  end ;

  function TGIS_Normals.fget_Normal(
    const _index   : Integer
  ) : TGIS_SingleVector ;
  begin
    if ( _index >=0 ) and ( _index < FNumNormals ) then
      Result := FNormals[_index]
    else
      Result := GisSingleVector( 0, 0, 1 ) ;
  end ;

  procedure TGIS_Normals.fset_Normal(
    const _index   : Integer ;
    const _normal  : TGIS_SingleVector
   ) ;
  begin
    if ( _index >=0 ) and ( _index < FNumNormals ) then
      FNormals[_index] := _normal ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_VertexColors'}
//==============================================================================
// TGIS_VertexColors
//==============================================================================

  procedure TGIS_VertexColors.Assign(
    const _vertexColors : TGIS_VertexColors
  ) ;
  begin
    FNumVertexColors  := _vertexColors.FNumVertexColors ;
    FNumParts         := _vertexColors.FNumParts ;
    FVertexColorParts := _vertexColors.FVertexColorParts ;
    FVertexColors     := _vertexColors.FVertexColors ;
  end ;

  procedure TGIS_VertexColors.Resize(
    const _numParts   : Integer ;
    const _numColors  : Integer
  ) ;
  begin
    FNumVertexColors  := _numColors ;
    FNumParts         := _numParts ;

    SetLength( FVertexColorParts, _numParts ) ;
    SetLength( FVertexColors, _numColors    ) ;
  end ;

  function  TGIS_VertexColors.GetPartSize(
    const _part : Integer
  ) : Integer ;
  begin
    Result := 0 ;
    if ( _part >=0 ) then begin
      if ( _part < FNumParts-1 ) then
        Result := FVertexColorParts[_part+1]-FVertexColorParts[_part]
      else if _part = FNumParts-1 then
        Result := FNumVertexColors - FVertexColorParts[_part]
    end
  end ;

  procedure TGIS_VertexColors.SetPartOffset(
    const _part   : Integer ;
    const _offset : Integer
  ) ;
  begin
    if ( _part >=0 ) and ( _part < FNumParts ) then
      FVertexColorParts[_part] := _offset ;
  end ;

  function  TGIS_VertexColors.GetVertexColor(
    const _part  : Integer ;
    const _index : Integer
  ) : Cardinal ;
  var
    idx : Integer ;
  begin
    if ( _index >=0 ) and ( _index < FNumVertexColors  ) and
       ( _part  >=0 ) and ( _part  < FNumParts         ) then begin
      idx := FVertexColorParts[_part]+_index ;
      if idx < FNumVertexColors then
        Result := FVertexColors[idx]
      else
        Result := 0 ;
    end
    else
      Result := 0 ;
  end ;

  procedure TGIS_VertexColors.SetVertexColor(
    const _index   : Integer ;
    const _color   : Cardinal
   ) ;
  begin
    if ( _index >=0 ) and ( _index < FNumVertexColors ) then
      FVertexColors[_index] := _color ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_PartDescriptors'}

//==============================================================================
// TGIS_PartDescriptors
//==============================================================================

  procedure TGIS_PartDescriptors.Assign(
    const _descriptor : TGIS_PartDescriptors
  ) ;
  begin
    FNumPartDescriptors := _descriptor.FNumPartDescriptors ;
    FPartDescriptors    := _descriptor.FPartDescriptors ;
  end ;

  procedure TGIS_PartDescriptors.Resize(
    const _size : Integer
  ) ;
  begin
    SetLength( FPartDescriptors, _size ) ;
    FNumPartDescriptors := _size ;
  end ;

  function TGIS_PartDescriptors.fget_PartDescriptor(
    const _index   : Integer
  ) : TGIS_PartDescriptor ;
  begin
    if ( _index >=0 ) and ( _index < FNumPartDescriptors ) then
      Result := FPartDescriptors[_index]
    else
      Result := GisPartDescriptor( 0, 0, 0, 0 ) ;
  end ;

  procedure TGIS_PartDescriptors.fset_PartDescriptor(
    const _index       : Integer ;
    const _descriptor  : TGIS_PartDescriptor
   ) ;
  begin
    if ( _index >=0 ) and ( _index < FNumPartDescriptors ) then
      FPartDescriptors[_index] := _descriptor ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_Editor type helpers'}

//============================================================================
// TGIS_EditorSnapPointEventArgs
//============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_EditorSnapPointEventArgs.Create(
      const _ptg  : TGIS_Point3D ;
      const _prec : Double ;
      const _proj : TGIS_Point3D ;
      const _dist : Double
    ) ;
    begin
      inherited Create ;
      FPtg  := _ptg  ;
      FPrec := _prec ;
      FProj := _proj ;
      FDist := _dist ;
    end ;
  {$ENDIF}

//============================================================================
// TGIS_EditorPointChangeEventArgs
//============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_EditorPointChangeEventArgs.Create(
      const _ptg       : TGIS_Point3D ;
      const _operation : TGIS_EditorOperation ;
      const _proj      : TGIS_Point3D
    ) ;
    begin
      inherited Create ;
      FPtg        := _ptg  ;
      FOperation  := _operation ;
      FProj       := _proj ;
    end ;
  {$ENDIF}

//============================================================================
// TGIS_EditorPointMoveEventArgs
//============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_EditorPointMoveEventArgs.Create(
      const _ptg       : TGIS_Point3D ;
      const _pt        : TPoint
    ) ;
    begin
      inherited Create ;
      FPtg  := _ptg  ;
      FPt   := _pt ;
    end ;
  {$ENDIF}

//============================================================================
// TGIS_EditorEditingPointsStyle
//============================================================================

  constructor TGIS_EditorEditingPointsStyle.Create;
  begin
    inherited ;

    FActivePoints   := TGIS_EditorStyle.Create ;
    FInactivePoints := TGIS_EditorStyle.Create ;
    FSelectedPoints := TGIS_EditorStyle.Create ;
    F3DPoints       := TGIS_EditorStyle.Create ;
    FSnappingPoints := TGIS_EditorStyle.Create ;

    FPointsFont       := TGIS_Font.Create ;
    FPointsFont.Name  := 'Arial' ;
    FPointsFont.Size  := 7 ;
    FPointsFont.Color := TGIS_Color.White ;
    FPointsFont.Style := GisGetEmptyFontStyle ;
  end;

  {$IFNDEF MANAGED}

    destructor TGIS_EditorEditingPointsStyle.Destroy;
    begin
      FActivePoints.Free ;
      FInactivePoints.Free ;
      FSelectedPoints.Free ;
      F3DPoints.Free ;
      FSnappingPoints.Free ;
      FPointsFont.Free ;

      inherited;
    end;
  {$ENDIF}

  procedure TGIS_EditorEditingPointsStyle.fset_PointsFont(
    const _value : TGIS_Font
  ) ;
  begin
    FPointsFont.Assign( _value ) ;
  end ;


//============================================================================
// TGIS_EditorEdgeLengthsStyle
//============================================================================

  constructor TGIS_EditorEdgeLengthsStyle.Create;
  begin
    inherited ;

    FFont       := TGIS_Font.Create ;
    FFont.Name  := 'Arial' ;
    FFont.Size  := 6 ;
    FFont.Color := TGIS_Color.Black ;
    FFont.Style := GisGetEmptyFontStyle ;

    FBackground       := TGIS_EditorStyle.Create ;
    FFollowEdgeAngle  := False ;
    FUnitsEPSG        := 0 ;
  end;

  {$IFNDEF MANAGED}

    destructor TGIS_EditorEdgeLengthsStyle.Destroy;
    begin
      FFont.Free ;
      FBackground.Free ;

      inherited;
    end;
  {$ENDIF}

  procedure TGIS_EditorEdgeLengthsStyle.fset_Font(
    const _value : TGIS_Font
  ) ;
  begin
    FFont.Assign( _value ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ProxySettings'}
//==============================================================================
// TGIS_EnvironmentInfo_Class & TGIS_EnvironmentInfo
//==============================================================================

  constructor TGIS_EnvironmentInfo.Create ;
  begin
    {$IFNDEF OXYGENE}
      inherited Create ;
    {$ENDIF}

    prepare ;
  end ;

  procedure TGIS_EnvironmentInfo.prepare ;

  {$IFNDEF MANAGED}
    function IsWindows64 : Boolean ;
    {$IFDEF MSWINDOWS}
      type
        TIsWow64Process = function( AHandle      : THandle ;
                                    var AIsWow64 : BOOL
                                   ) : BOOL ; stdcall ;
      var
        kernel32Handle : DWORD ;
        isWow64Process : TIsWow64Process ;
        isWow64        : BOOL ;
      begin
        Result := False ;

        kernel32Handle := LoadLibrary('kernel32.dll') ;
        if (kernel32Handle = 0) then exit ;
        try
          @isWow64Process := GetProcAddress( kernel32Handle, 'IsWow64Process' ) ;
          if not assigned(isWow64Process) then exit ;
          isWow64 := False ;
          if isWow64Process( GetCurrentProcess, isWow64 ) then
            Result := isWow64 ;
        finally
          FreeLibrary( kernel32Handle ) ;
        end ;
      end ;
    {$ELSE}
      begin
        Result := False ;
      end ;
    {$ENDIF}
  {$ENDIF}

  procedure check_bits ;
  begin
    {$IFDEF MANAGED}
      {$IFDEF ISLAND}
        FIs32 := Environment.ProcessBitness = 32 ;
      {$ELSE}
        if sizeOf( IntPtr ) = 4 then
          FIs32 := True
        else
          FIs32 := False ;
      {$ENDIF}
    {$ELSE}
      if sizeOf( Pointer ) = 4 then
        FIs32 := True
      else
        FIs32 := False ;
    {$ENDIF}
    FIs64 := not FIs32 ;
  end ;

  procedure check_host ;
  begin
    {$IFDEF MANAGED}
      {$IFDEF JAVA}
        if java.io.File.separatorChar = '\' then
      {$ELSE}
        if Path.DirectorySeparatorChar = '\' then
      {$ENDIF}
      begin
        FIsWindows := True ;
        FDirSep    := '\'  ;
      end
      else begin
        FIsWindows := False ;
        FDirSep    := '/'   ;
      end ;

      FIsUnix := not FIsWindows ;

      FIsMac := False ;
      {$IFDEF JAVA}
        if FIsUnix and DirectoryExists( '/Users' ) then
      {$ENDIF}
      {$IFDEF ISLAND}
        if FIsUnix and DirectoryExists( '/Users' ) then
      {$ENDIF}
      {$IFDEF CLR}
        if FIsUnix and Directory.Exists( '/Users' ) then
      {$ENDIF}
        FIsMac := True ;
    {$ELSE}
      {$IFDEF MSWINDOWS}
        FDirSep    := '\'   ;
        FIsWindows := True  ;
        FIsUnix    := False ;
        FIsMac     := False ;
      {$ELSE}
        {$IFDEF MACOS}
          FDirSep    := '/'   ;
          FIsWindows := False ;
          FIsUnix    := True  ;
          FIsMac     := True  ;
        {$ELSE}
          {$IFDEF LINUX}
            FDirSep    := '/'   ;
            FIsWindows := False ;
            FIsUnix    := False ;
            FIsMac     := True  ;
          {$ELSE}
            {$IFDEF ANDROID}
              FDirSep    := '/'   ;
              FIsWindows := False ;
              FIsUnix    := True ;
              FIsMac     := True  ;
            {$ELSE}
              {$MESSAGE ERROR 'Unknown OS'}
            {$ENDIF}
          {$ENDIF}
        {$ENDIF}
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure check_runtime ;
  {$IFDEF CLR}
    var
      t : System.Type ;
  {$ENDIF}
  begin
    {$IFDEF CLR}
      FIsNative := False ;
      FIsJava  := True ;
      FIsNET    := True ;

      t := System.Type.GetType( 'Mono.Runtime' ) ;
      if t <> nil then
        FIsMono := True
      else
        FIsMono := False ;

      FIsMsNET := not FIsMono ;
    {$ELSE}
      {$IFDEF JAVA}
        FIsNative := False ;
        FIsJava   := True  ;
        FIsNET    := False ;
        FIsMsNET  := False ;
        FIsMono   := False ;
      {$ELSE}
        FIsNative := True  ;
        FIsJava   := False ;
        FIsNET    := False ;
        FIsMsNET  := False ;
        FIsMono   := False ;
      {$ENDIF}
    {$ENDIF}
  end ;

  begin
    check_bits ;
    check_host ;
    check_runtime ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ProxySettings'}
//==============================================================================
// TGIS_ProxySettings
//==============================================================================

  constructor TGIS_ProxySettings.Create ;
  begin
    inherited ;

    FNoProxyFilter := TGIS_StringList.Create ;
    Clear ;
  end ;

  procedure TGIS_ProxySettings.doDestroy  ;
  begin
    FreeObject( FNoProxyFilter ) ;
    inherited ;
  end ;

  procedure TGIS_ProxySettings.Clear ;
  begin
    FServer        := '' ;
    FPort          := 0 ;
    FUser          := '' ;
    FPass          := '' ;
    FDomain        := '' ;
    FNoProxyFilter.Clear ;
  end ;

  function TGIS_ProxySettings.CanUseProxy(
    const _url : String
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := True ;
    for i := 0 to FNoProxyFilter.Count - 1 do
      if Pos( UpperCase( FNoProxyFilter[i] ), UpperCase( _url ) ) >= StringFirst then
      begin
        Result := False ;
        exit ;
      end ;
  end;
{$ENDREGION}

{$REGION 'TGIS_ColorRampList'}

  constructor TGIS_ColorRampList.Create ;
  begin
    inherited ;
    oList := TObjectList<TGIS_GradientMap>.Create( True ) ;
    oDic  := TDictionary<String, Integer>.Create(
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
    Init ;
  end ;

  procedure TGIS_ColorRampList.doDestroy ;
  begin
    FreeObject( oList ) ;
    FreeObject( oDic  ) ;

    inherited ;
  end ;

  function TGIS_ColorRampList.fget_Item(
    const _index : Integer
  ) : TGIS_GradientMap ;
  begin
    if ( _index >= 0 ) and ( _index < oList.Count ) then
      Result := oList[_index]
    else
      Result := nil ;
  end ;

  function TGIS_ColorRampList.fget_Count : Integer ;
  begin
    Result := oList.Count ;
  end ;

  procedure TGIS_ColorRampList.addEx(
    const _name    : String ;
    const _mapType : TGIS_ColorSchema ;
    const _map     : TGIS_ColorMapArray ;
    const _submap  : TGIS_ColorMapExArray
  ) ;
  var
    gm : TGIS_GradientMap ;
  begin
    LockThread ;
    try
      gm := TGIS_GradientMap.Create(
        _name,
        _map,
        _submap,
        _mapType
      ) ;
      oList.Add( gm ) ;
      oDic.Add( _name, oList.Count-1 ) ;
    finally
      UnlockThread ;
    end ;
  end;

  procedure TGIS_ColorRampList.addExDynamicHSL(
    const _name    : String ;
    const _mapType : TGIS_ColorSchema ;
    const _hMin    : Integer ;
    const _hMax    : Integer ;
    const _sMin    : Integer ;
    const _sMax    : Integer ;
    const _lMin    : Integer ;
    const _lMax    : Integer
  ) ;
  var
    gm : TGIS_GradientMap ;
  begin
    LockThread ;
    try
      gm := TGIS_GradientMapUnique.Create(
        _name,
        _mapType,
        _hMin/360,
        _hMax/360,
        _sMin/100,
        _sMax/100,
        _lMin/100,
        _lMax/100
      ) ;
      oList.Add( gm ) ;
      oDic.Add( _name, oList.Count-1 ) ;
    finally
      UnlockThread ;
    end ;
  end;

  procedure TGIS_ColorRampList.addEx(
    const _def  : String
  ) ;
  var
    prms      : TArray<String> ;
    map       : TArray<String> ;
    submaps   : TArray<String> ;
    len       : Integer ;
    {$IFDEF DCC}
    str       : String ;
    {$ENDIF}
    r_name    : String ;
    r_map     : TGIS_ColorMapArray ;
    r_submap  : TGIS_ColorMapExArray ;
    r_mapType : TGIS_ColorSchema ;
    r_colors  : TGIS_CardinalArray ;
    idx       : Integer ;
    i         : Integer ;
    arr       : TArray<String> ;

    function cm(
      const _i     : Double ;
      const _color : Cardinal
    ) : TGIS_ColorMap ;
    begin
      Result.Index := _i ;
      Result.RGB   := TGIS_Color.FromARGB( _color ) ;
    end ;

    function cme(
      const _i      : Integer ;
      const _colors : TGIS_CardinalArray
    ) : TGIS_ColorMapEx ;
    var
      c, clen : Integer ;
    begin
      Result.Index := _i ;
      clen := length( _colors ) ;
      SetLength( Result.Colors, clen ) ;
      for c := 0 to clen-1 do
        Result.Colors[c] := TGIS_Color.FromRGB( _colors[c] ) ;
    end ;

  begin
    {$IFDEF JAVA OR ISLAND}
      prms := _def.Split( ';' ).ToArray ;
    {$ELSE}
      prms := _def.Split( [';'] ) ;
    {$ENDIF}
    assert( length(prms) = 4 ) ;
    r_name := prms[0] ;
    assert( length(r_name) > 0 ) ;

    {$IFDEF JAVA OR ISLAND}
      map := prms[1].Split( ',' ).ToArray ;
    {$ELSE}
      map := prms[1].Split( [','] ) ;
    {$ENDIF}
    len := length( map ) ;
    SetLength( r_map, len ) ;
    assert( len > 0 ) ;
    idx := 0 ;
    for str in map do begin
      {$IFDEF JAVA OR ISLAND}
        arr := str.Split( ' ' ).ToArray ;
      {$ELSE}
        arr := str.Split( [' '] ) ;
      {$ENDIF}
      r_map[idx] := cm( DotStrToFloat( arr[0] ), StrToInt64( arr[1] ) ) ;
      inc( idx ) ;
    end ;

    if not IsStringEmpty( prms[2] ) then begin
      {$IFDEF JAVA OR ISLAND}
        submaps := prms[2].Split( ',' ).ToArray ;
      {$ELSE}
        submaps := prms[2].Split( [','] ) ;
      {$ENDIF}
      len := length( submaps ) ;
      SetLength( r_submap, len ) ;

      idx := 0 ;
      for str in submaps do begin
        {$IFDEF JAVA OR ISLAND}
          arr := str.Split( ' ' ).ToArray ;
        {$ELSE}
          arr := str.Split( [' '] ) ;
        {$ENDIF}
        len := length( arr ) ;
        assert( len > 0 ) ;
        SetLength( r_colors, len-1 ) ;
        for i := 1 to len-1 do
          r_colors[i-1] := StrToInt64( arr[i] ) ;

        r_submap[idx] := cme( StrToInt( arr[0] ), r_colors ) ;
        inc( idx ) ;
      end ;
    end
    else
      r_submap := nil ;

    r_mapType := TGIS_ColorSchema.Sequential ;
    if prms[3] = 'Diverging' then
      r_mapType := TGIS_ColorSchema.Diverging
    else if prms[3] = 'Miscellaneous' then
      r_mapType := TGIS_ColorSchema.Miscellaneous
    else if prms[3] = 'Qualitative' then
      r_mapType := TGIS_ColorSchema.Qualitative
    else if prms[3] = 'Sequential' then
      r_mapType := TGIS_ColorSchema.Sequential
    else
      assert( False, 'Unknown TGIS_ColorSchema' ) ;

    addEx( r_name, r_mapType, r_map, r_submap ) ;
  end ;

  procedure TGIS_ColorRampList.Clear ;
  begin
    LockThread ;
    try
      oList.Clear ;
      oDic.Clear ;
    finally
      UnlockThread ;
    end;
  end ;

  function TGIS_ColorRampList.ByName(
    const _name : String
  ) : TGIS_GradientMap ;
  var
    idx : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  begin
    LockThread ;
    try
      if oDic.TryGetValue( _name, idx ) then begin
        Result := oList[idx] ;
      end
      else
        Result := nil ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TGIS_ColorRampList.Add(
    const _ramp : TGIS_GradientMap
  ) ;
  begin
    LockThread ;
    try
      oList.Add( _ramp ) ;
      oDic.Add( _ramp.Name, oList.Count-1 ) ;
    finally
      UnlockThread ;
    end ;
  end ;

  procedure TGIS_ColorRampList.Init ;

  {$REGION 'Colormaps'}
  procedure fill_1 ;
  begin
    addEx( 'Accent;'+
      '0 $FF7fc97f,'+
      '25 $FFbeaed4,'+
      '50 $FFfdc086,'+
      '75 $FFffff99,'+
      '100 $FF386cb0;'+
        '3 $7fc97f $beaed4 $fdc086,'+
        '4 $7fc97f $beaed4 $fdc086 $ffff99,'+
        '5 $7fc97f $beaed4 $fdc086 $ffff99 $386cb0,'+
        '6 $7fc97f $beaed4 $fdc086 $ffff99 $386cb0 $f0027f,'+
        '7 $7fc97f $beaed4 $fdc086 $ffff99 $386cb0 $f0027f $bf5b17,'+
        '8 $7fc97f $beaed4 $fdc086 $ffff99 $386cb0 $f0027f $bf5b17 $666666;'+
      'Qualitative'
    );
    addEx( 'Arctic;'+
      '0.00 $FF2C3563,'+
      '8.89 $FF3C4474,'+
      '17.78 $FF4D5585,'+
      '26.67 $FF626B9A,'+
      '35.56 $FF868BAF,'+
      '44.44 $FFAAACC1,'+
      '53.33 $FFC2C3CD,'+
      '56.67 $FFA8DD86,'+
      '58.44 $FFBFE58A,'+
      '60.22 $FFDAED8E,'+
      '64.44 $FFE3E584,'+
      '73.33 $FFEAD270,'+
      '82.22 $FFF0C15F,'+
      '91.11 $FFD2AD59,'+
      '100.00 $FFAE9E59;;'+
      'Miscellaneous'
    );
    //? aspect/slope has also default ramp
    addEx( 'Aspect;'+
      '0 $FF000000,'+
      '50 $FFFFFFFF,'+
      '100 $FF000000;;'+
      'Diverging'
    );
    addEx( 'Autumn;'+
      '0 $ffff0000,'+
      '5.26 $ffff0d00,'+
      '10.53 $ffff1a00,'+
      '15.79 $ffff2800,'+
      '21.05 $ffff3500,'+
      '26.32 $ffff4300,'+
      '31.58 $ffff5000,'+
      '36.84 $ffff5e00,'+
      '42.11 $ffff6b00,'+
      '47.37 $ffff7900,'+
      '52.63 $ffff8600,'+
      '57.89 $ffff9400,'+
      '63.16 $ffffa100,'+
      '68.42 $ffffaf00,'+
      '73.68 $ffffbc00,'+
      '78.95 $ffffca00,'+
      '84.21 $ffffd700,'+
      '89.47 $ffffe500,'+
      '94.74 $fffff200,'+
      '100 $ffffff00;;'+
      'Sequential'
    );
    addEx( 'BlackWhite;'+
      '0 $FF000000,'+
      '100 $FFFFFFFF;;'+
      'Sequential'
    );
    // surfer 'Bathymetry'
    addEx( 'Bathymetry1;'+
      '0 $FF3300CC,'+
      '58.13 $FF00FFFF,'+
      '79.80 $FF99FFFF,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    // new surfer 'Bathymetry'
    addEx( 'Bathymetry2;'+
      '0 $FF003399,'+
      '25 $FF0000FF,'+
      '50 $FF7AD3DD,'+
      '75 $FFA1F6F6,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'BathymetryWarm;'+
      '0 $FF720072,'+
      '16.67 $FF0000FF,'+
      '33.33 $FF99CCCC,'+
      '50 $FF009900,'+
      '66.67 $FFFFFF66,'+
      '83.33 $FFFF5757,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    // brewer 'BuGn'
    addEx( 'BlueGreen;'+
      '0 $FFEDF8FB,'+
      '25 $FFB2E2E2,'+
      '50 $FF66C2C2,'+
      '75 $FF2CA2A2,'+
      '100 $FF006D2C;'+
        '3 $e5f5f9 $99d8c9 $2ca25f,'+
        '4 $edf8fb $b2e2e2 $66c2a4 $238b45,'+
        '5 $edf8fb $b2e2e2 $66c2a4 $2ca25f $006d2c,'+
        '6 $edf8fb $ccece6 $99d8c9 $66c2a4 $2ca25f $006d2c,'+
        '7 $edf8fb $ccece6 $99d8c9 $66c2a4 $41ae76 $238b45 $005824,'+
        '8 $f7fcfd $e5f5f9 $ccece6 $99d8c9 $66c2a4 $41ae76 $238b45 $005824,'+
        '9 $f7fcfd $e5f5f9 $ccece6 $99d8c9 $66c2a4 $41ae76 $238b45 $006d2c $00441b;'+
      'Sequential'
    );
    // brewer 'BuPu'
    addEx( 'BluePurple;'+
      '0 $FFEDF8FB,'+
      '25 $FFB3CDCD,'+
      '50 $FF8C9696,'+
      '75 $FF885656,'+
      '100 $FF810F7C;'+
        '3 $e0ecf4 $9ebcda $8856a7,'+
        '4 $edf8fb $b3cde3 $8c96c6 $88419d,'+
        '5 $edf8fb $b3cde3 $8c96c6 $8856a7 $810f7c,'+
        '6 $edf8fb $bfd3e6 $9ebcda $8c96c6 $8856a7 $810f7c,'+
        '7 $edf8fb $bfd3e6 $9ebcda $8c96c6 $8c6bb1 $88419d $6e016b,'+
        '8 $f7fcfd $e0ecf4 $bfd3e6 $9ebcda $8c96c6 $8c6bb1 $88419d $6e016b,'+
        '9 $f7fcfd $e0ecf4 $bfd3e6 $9ebcda $8c96c6 $8c6bb1 $88419d $810f7c $4d004b;'+
      'Sequential'
    );
    // brewer 'Bwr'
    addEx( 'BlueRed;'+
      '0 $ff0000ff,'+
      '11.11 $ff3838ff,'+
      '22.22 $ff7070ff,'+
      '33.33 $ffaaaaff,'+
      '44.44 $ffe2e2ff,'+
      '55.56 $ffffe2e2,'+
      '66.67 $ffffaaaa,'+
      '77.78 $ffff7070,'+
      '88.89 $ffff3838,'+
      '100 $ffff0000;;'+
      'Diverging'
    );
    // brewer 'Blues'
    addEx( 'Blues1;'+
      '0 $FFeff3ff,'+
      '25 $FFbdd7e7,'+
      '50 $FF6baed6,'+
      '75 $FF3182bd,'+
      '100 $FF08519c;'+
        '3 $deebf7 $9ecae1 $3182bd,'+
        '4 $eff3ff $bdd7e7 $6baed6 $2171b5,'+
        '5 $eff3ff $bdd7e7 $6baed6 $3182bd $08519c,'+
        '6 $eff3ff $c6dbef $9ecae1 $6baed6 $3182bd $08519c,'+
        '7 $eff3ff $c6dbef $9ecae1 $6baed6 $4292c6 $2171b5 $084594,'+
        '8 $f7fbff $deebf7 $c6dbef $9ecae1 $6baed6 $4292c6 $2171b5 $084594,'+
        '9 $f7fbff $deebf7 $c6dbef $9ecae1 $6baed6 $4292c6 $2171b5 $08519c $08306b;'+
      'Sequential'
    );
    addEx( 'Blues2;'+
      '0 $FFFFFFFF,'+
      '20 $FFD0D0F6,'+
      '40 $FF7871F6,'+
      '60 $FF4F48F6,'+
      '80 $FF3352CC,'+
      '100 $FF330099;;'+
      'Sequential'
    );
    addEx( 'BlueSteel;'+
      '0 $FF000000,'+
      '70 $FFCCCCFF,'+
      '85 $FF999999,'+
      '93 $FFCCCCCC,'+
      '100 $FFE6E6E6;;'+
      'Miscellaneous'
    );
    addEx( 'BrownBlue;'+
      '0 $FF914F32,'+
      '16.67 $FFC69A6B,'+
      '33.33 $FFF9DCC3,'+
      '50.20 $FFECF9FC,'+
      '66.66 $FF8ED6EF,'+
      '83.33 $FF5EB3E3,'+
      '100 $FF2F68B3;;'+
      'Diverging'
    );
    // brewer 'BrBG'
    addEx( 'BrownGreen;'+
      '0 $FFA6611A,'+
      '25 $FFDFC2C2,'+
      '50 $FFF5F5F5,'+
      '75 $FF80CDCD,'+
      '100 $FF018571;'+
        '3 $d8b365 $f5f5f5 $5ab4ac,'+
        '4 $a6611a $dfc27d $80cdc1 $018571,'+
        '5 $a6611a $dfc27d $f5f5f5 $80cdc1 $018571,'+
        '6 $8c510a $d8b365 $f6e8c3 $c7eae5 $5ab4ac $01665e,'+
        '7 $8c510a $d8b365 $f6e8c3 $f5f5f5 $c7eae5 $5ab4ac $01665e,'+
        '8 $8c510a $bf812d $dfc27d $f6e8c3 $c7eae5 $80cdc1 $35978f $01665e,'+
        '9 $8c510a $bf812d $dfc27d $f6e8c3 $f5f5f5 $c7eae5 $80cdc1 $35978f $01665e,'+
        '10 $543005 $8c510a $bf812d $dfc27d $f6e8c3 $c7eae5 $80cdc1 $35978f $01665e $003c30,'+
        '11 $543005 $8c510a $bf812d $dfc27d $f6e8c3 $f5f5f5 $c7eae5 $80cdc1 $35978f $01665e $003c30;'+
      'Diverging'
    );
    addEx( 'BrownYellow;'+
      '0 $FF583210,'+
      '16.67 $FFAD684F,'+
      '33.33 $FFEFB77B,'+
      '50 $FFFFCC99,'+
      '66.67 $FFFFF2AA,'+
      '83.33 $FFFFFFC0,'+
      '100 $FFFFFFE3;;'+
      'Sequential'
    );
    addEx( 'Carnival;'+
      '0 $FF4EDA92,'+
      '2 $FF2F78C2,'+
      '3 $FFDA30B9,'+
      '5 $FFFD032F,'+
      '7 $FFCDA52F,'+
      '9 $FFCDDAB0,'+
      '10 $FF2CDA2F,'+
      '20 $FFCDDA12,'+
      '25 $FF46DA2F,'+
      '30 $FFCDAC2F,'+
      '40 $FFCDDA2F,'+
      '50 $FF2FD58D,'+
      '60 $FFCD5E2F,'+
      '70 $FFC63BA2,'+
      '80 $FFCDDA3B,'+
      '90 $FF34CDF7,'+
      '100 $FF47052F;;'+
      'Qualitative'
    );
    addEx( 'CBF-Bright;'+
      '0 $FF4477AA,'+
      '16.67 $FFEE6677,'+
      '33.33 $FF228833,'+
      '50 $FFCCBB44,'+
      '66.67 $FF66CCEE,'+
      '83.33 $FFAA3377,'+
      '100 $FFBBBBBB;;'+
      'Qualitative'
    );
    addEx( 'CBF-Dark;'+
      '0 $FF222255,'+
      '20 $FF225555,'+
      '40 $FF225522,'+
      '60 $FF666633,'+
      '80 $FF663333,'+
      '100 $FF555555;;'+
      'Qualitative'
    );
    addEx( 'CBF-Deuteranopia8;'+
      '0 $FF000000,'+
      '14.29 $FF2271B2,'+
      '28.57 $FF3DB7E9,'+
      '42.86 $FFF748A5,'+
      '57.14 $FF359B73,'+
      '71.43 $FFd55e00,'+
      '85.71 $FFe69f00,'+
      '100 $FFf0e442;;'+
      'Qualitative'
    );
    addEx( 'CBF-Deuteranopia12;'+
      '0 $FF9F0162,'+
      '9.09 $FF009F81,'+
      '18.18 $FFFF5AAF,'+
      '27.27 $FF00FCCF,'+
      '36.36 $FF8400CD,'+
      '45.45 $FF008DF9,'+
      '54.54 $FF00C2F9,'+
      '63.63 $FFFFB2FD,'+
      '72.72 $FFA40122,'+
      '81.82 $FFE20134,'+
      '90.92 $FFFF6E3A,'+
      '100 $FFFFC33B;;'+
      'Qualitative'
    );
    addEx( 'CBF-Deuteranopia15;'+
      '0 $FF68023F,'+
      '7.14 $FF008169,'+
      '14.29 $FFEF0096,'+
      '21.43 $FF00DCB5,'+
      '28.57 $FFFFCFE2,'+
      '35.71 $FF003C86,'+
      '42.86 $FF9400E6,'+
      '50 $FF009FFA,'+
      '57.14 $FFFF71FD,'+
      '64.29 $FF7CFFFA,'+
      '71.43 $FF6A0213,'+
      '78.57 $FF008607,'+
      '85.71 $FFF60239,'+
      '92.86 $FF00E307,'+
      '100 $FFFFDC3D;;'+
      'Qualitative'
    );
    addEx( 'CBF-HighContrast;'+
      '0 $FF004488,'+
      '50 $FFDDAA33,'+
      '100 $FFBB5566;;'+
      'Qualitative'
    );
    addEx( 'CBF-IBM;'+
      '0 $FF648FFF,'+
      '25 $FF785EF0,'+
      '50 $FFDC267F,'+
      '75 $FFFE6100,'+
      '100 $FFFFB000;;'+
      'Qualitative'
    );
    addEx( 'CBF-Incandescent;'+
      '0 $FFCEFFFF,'+
      '10 $FFC6F7D6,'+
      '20 $FFA2F49B,'+
      '30 $FFBBE453,'+
      '40 $FFD5CE04,'+
      '50 $FFE7B503,'+
      '60 $FFF19903,'+
      '70 $FFF6790B,'+
      '80 $FFF94902,'+
      '90 $FFE40515,'+
      '100 $FFA80003;;'+
      'Sequential'
    );
    addEx( 'CBF-Iridescent;'+
      '0 $FFFEFBE9,'+
      '4.55 $FFFCF7D5,'+
      '9.09 $FFF5F3C1,'+
      '13.64 $FFEAF0B5,'+
      '18.18 $FFDDECBF,'+
      '22.73 $FFD0E7CA,'+
      '27.27 $FFC2E3D2,'+
      '31.82 $FFB5DDD8,'+
      '36.36 $FFA8D8DC,'+
      '40.91 $FF9BD2E1,'+
      '45.45 $FF8DCBE4,'+
      '50 $FF81C4E7,'+
      '54.55 $FF7BBCE7,'+
      '59.09 $FF7EB2E4,'+
      '63.64 $FF88A5DD,'+
      '68.18 $FF9398D2,'+
      '72.73 $FF9B8AC4,'+
      '77.27 $FF9D7DB2,'+
      '81.82 $FF9A709E,'+
      '86.36 $FF906388,'+
      '90.91 $FF805770,'+
      '95.45 $FF684957,'+
      '100 $FF46353A;;'+
      'Sequential'
    );
    addEx( 'CBF-LandCover;'+
      '0 $F5566AA,'+
      '7.69 $FF117733,'+
      '15.38 $FF44AA66,'+
      '23.08 $FF55AA22,'+
      '30.77 $FF668822,'+
      '38.46 $FF99BB55,'+
      '46.15 $FF558877,'+
      '53.85 $FF88BBAA,'+
      '61.54 $FFAADDCC,'+
      '69.23 $FF44AA88,'+
      '76.92 $FFDDCC66,'+
      '84.62 $FFFFDD44,'+
      '92.31 $FFFFEE88,'+
      '100 $FFBB0011;;'+
      'Miscellaneous'
    );
    addEx( 'CBF-Light;'+
      '0.0 $FF77AADD,'+
      '12.5 $FFEE8866,'+
      '25.0 $FFEEDD88,'+
      '37.5 $FFFFAABB,'+
      '50.0 $FF99DDFF,'+
      '62.5 $FF44BB99,'+
      '75.0 $FFBBCC33,'+
      '87.5 $FFAAAA00,'+
      '100.0 $FFDDDDDD;;'+
      'Qualitative'
    );
    addEx( 'CBF-MediumContrast;'+
      '0 $FF6699CC,'+
      '20 $FF004488,'+
      '40 $FFEECC66,'+
      '60 $FF994455,'+
      '80 $FF997700,'+
      '100 $FFEE99AA;;'+
      'Qualitative'
    );
    addEx( 'CBF-Muted;'+
      '0 $FFCC6677,'+
      '14.29 $FF332288,'+
      '28.57 $FFDDCC77,'+
      '42.86 $FF117733,'+
      '57.14 $FF88CCEE,'+
      '71.43 $FF882255,'+
      '85.71 $FF44AA99,'+
      '100 $FF999933;;'+
      'Qualitative'
    );
    addEx( 'CBF-Pale;'+
      '0 $FFBBCCEE,'+
      '20 $FFCCEEFF,'+
      '40 $FFCCDDAA,'+
      '60 $FFEEEEBB,'+
      '80 $FFFFCCCC,'+
      '100 $FFDDDDDD;;'+
      'Qualitative'
    );
    addEx( 'CBF-RainbowDiscrete14;'+
      '0.00 $FFD1BBD7,'+
      '7.69 $FFAE76A3,'+
      '15.38 $FF882E72,'+
      '23.08 $FF1965B0,'+
      '30.77 $FF5289C7,'+
      '38.46 $FF7BAFDE,'+
      '46.15 $FF4EB265,'+
      '53.85 $FF90C987,'+
      '61.54 $FFCAE0AB,'+
      '69.23 $FFF7F056,'+
      '76.92 $FFF6C141,'+
      '84.62 $FFF1932D,'+
      '92.31 $FFE8601C,'+
      '100.00 $FFDC050C;;'+
      'Miscellaneous'
    );
    addEx( 'CBF-RainbowDiscrete29;'+
      '0.00 $FFE8ECFB,'+
      '3.57 $FFD9CCE3,'+
      '7.14 $FFD1BBD7,'+
      '10.71 $FFCAACCB,'+
      '14.29 $FFBA8DB4,'+
      '17.86 $FFAE76A3,'+
      '21.43 $FFAA6F9E,'+
      '25.00 $FF994F88,'+
      '28.57 $FF882E72,'+
      '32.14 $FF1965B0,'+
      '35.71 $FF437DBF,'+
      '39.29 $FF5289C7,'+
      '42.86 $FF6195CF,'+
      '46.43 $FF7BAFDE,'+
      '50.00 $FF4EB265,'+
      '53.57 $FF90C987,'+
      '57.14 $FFCAE0AB,'+
      '60.71 $FFF7F056,'+
      '64.29 $FFF7CB45,'+
      '67.86 $FFF6C141,'+
      '71.43 $FFF4A736,'+
      '75.00 $FFF1932D,'+
      '78.57 $FFEE8026,'+
      '82.14 $FFE8601C,'+
      '85.71 $FFE65518,'+
      '89.29 $FFDC050C,'+
      '92.86 $FFA5170E,'+
      '96.43 $FF72190E,'+
      '100.00 $FF42150A;;'+
      'Miscellaneous'
    );
    addEx( 'CBF-RainbowSmooth;'+
      '0.00 $FFE8ECFB,'+
      '3.03 $FFDDD8EF,'+
      '6.06 $FFD1C1E1,'+
      '9.09 $FFC3A8D1,'+
      '12.12 $FFB58FC2,'+
      '15.15 $FFA778B4,'+
      '18.18 $FF9B62A7,'+
      '21.21 $FF8C4E99,'+
      '24.24 $FF6F4C9B,'+
      '27.27 $FF6059A9,'+
      '30.30 $FF5568B8,'+
      '33.33 $FF4E79C5,'+
      '36.36 $FF4D8AC6,'+
      '39.39 $FF4E96BC,'+
      '42.42 $FF549EB3,'+
      '45.45 $FF59A5A9,'+
      '48.48 $FF60AB9E,'+
      '51.52 $FF69B190,'+
      '54.55 $FF77B77D,'+
      '57.58 $FF8CBC68,'+
      '60.61 $FFA6BE54,'+
      '63.64 $FFBEBC48,'+
      '66.67 $FFD1B541,'+
      '69.70 $FFDDAA3C,'+
      '72.73 $FFE49C39,'+
      '75.76 $FFE78C35,'+
      '78.79 $FFE67932,'+
      '81.82 $FFE4632D,'+
      '84.85 $FFDF4828,'+
      '87.88 $FFDA2222,'+
      '90.91 $FFB8221E,'+
      '93.94 $FF95211B,'+
      '96.97 $FF721E17,'+
      '100.00 $FF521A13;;'+
      'Miscellaneous'
    );
    addEx( 'CBF-Sunset;'+
      '0 $FF364B9A,'+
      '10 $FF4A7BB7,'+
      '20 $FF6EA6CD,'+
      '30 $FF98CAE1,'+
      '40 $FFC2E4EF,'+
      '50 $FFEAECCC,'+
      '60 $FFFEDA8B,'+
      '70 $FFFDB366,'+
      '80 $FFF67E4B,'+
      '90 $FFDD3D2D,'+
      '100 $FFA50026;;'+
      'Qualitative'
    );
    addEx( 'CBF-Vibrant;'+
      '0.0 $FFEE7733,'+
      '16.66 $FF0077BB,'+
      '33.33 $FF33BBEE,'+
      '50 $FFEE3377,'+
      '66.66 $FFCC3311,'+
      '83.33 $FF009988,'+
      '100.0 $FFBBBBBB;;'+
      'Qualitative'
    );
    addEx( 'CBF-OkabeIto;'+
      '0.0 $FFE69F00,'+
      '14.3 $FF56B4E9,'+
      '28.6 $FF009E73,'+
      '42.9 $FFF0E442,'+
      '57.1 $FF0072B2,'+
      '71.4 $FFD55E00,'+
      '85.7 $FFCC79A7,'+
      '100 $FF000000;;'+
      'Qualitative'
    );
    addEx( 'ChromaDepth;'+
      '0 $FF000000,'+
      '0.5 $FF000000,'+
      '0.5 $FF000088,'+
      '16.6 $FF0000DC,'+
      '16.6 $FF008888,'+
      '33.3 $FF00DCDC,'+
      '33.3 $FF008800,'+
      '50 $FF00DC00,'+
      '50 $FF888800,'+
      '66.7 $FFDCDC00,'+
      '66.7 $FF880000,'+
      '83.3 $FFDC0000,'+
      '83.3 $FF880088,'+
      '100 $FFD800D8;;'+
      'Qualitative'
    );
    addEx( 'Cividis;'+
      '0 $ff002554,'+
      '2 $ff00295d,'+
      '4 $ff002c66,'+
      '6 $ff00306f,'+
      '8 $ff053371,'+
      '10 $ff143670,'+
      '12 $ff1e3a6f,'+
      '14 $ff263d6e,'+
      '16 $ff2d416d,'+
      '18 $ff33446d,'+
      '20 $ff39486c,'+
      '22 $ff3e4b6c,'+
      '24 $ff434e6c,'+
      '26 $ff47516c,'+
      '28 $ff4c556c,'+
      '30 $ff51586d,'+
      '32 $ff555c6d,'+
      '34 $ff5a5f6e,'+
      '36 $ff5e636f,'+
      '38 $ff636670,'+
      '40 $ff676a71,'+
      '42 $ff6c6d72,'+
      '44 $ff707173,'+
      '46 $ff747475,'+
      '48 $ff787877,'+
      '50 $ff7d7c78,'+
      '52 $ff807f78,'+
      '54 $ff858279,'+
      '56 $ff8a8678,'+
      '58 $ff8f8a78,'+
      '60 $ff938e78,'+
      '62 $ff989277,'+
      '64 $ff9d9576,'+
      '66 $ffa29975,'+
      '68 $ffa79d73,'+
      '70 $ffaca172,'+
      '72 $ffb1a570,'+
      '74 $ffb7a96e,'+
      '76 $ffbcae6c,'+
      '78 $ffc0b16a,'+
      '80 $ffc5b568,'+
      '82 $ffcbb965,'+
      '84 $ffd0be62,'+
      '86 $ffd5c25e,'+
      '88 $ffdbc75a,'+
      '90 $ffe0cb56,'+
      '92 $ffe6d051,'+
      '94 $ffebd44b,'+
      '96 $fff1d945,'+
      '98 $fff7de3e,'+
      '100 $fffde334;;'+
      'Sequential'
    );
    addEx( 'Cold;'+
      '0 $FF0050FA,'+
      '5 $FF0089F5,'+
      '10 $FF1DBCEF,'+
      '15 $FF73E0F1,'+
      '20 $FFB7F4F7,'+
      '25 $FFD6FBFC,'+
      '30 $FFDBFFFD,'+
      '35 $FFE5FEFA,'+
      '40 $FFF8FAEA,'+
      '50 $FFE6E6E6,'+
      '60 $FFF8FAEA,'+
      '65 $FFE5FEFA,'+
      '70 $FFDBFFFD,'+
      '75 $FFD6FBFC,'+
      '80 $FFB7F4F7,'+
      '85 $FF73E0F1,'+
      '90 $FF1DBCEF,'+
      '95 $FF0089F5,'+
      '100 $FF0050FA;;'+
      'Diverging'
    );
    addEx( 'Copper;'+
      '0 $ff000000,'+
      '5.26 $ff100a06,'+
      '10.53 $ff20140d,'+
      '15.79 $ff311f14,'+
      '21.05 $ff41291a,'+
      '26.32 $ff533421,'+
      '31.58 $ff633e28,'+
      '36.84 $ff74492f,'+
      '42.11 $ff845435,'+
      '47.37 $ff955f3c,'+
      '52.63 $ffa66943,'+
      '57.89 $ffb7744a,'+
      '63.16 $ffc77e50,'+
      '68.42 $ffd88957,'+
      '73.68 $ffe8935e,'+
      '78.95 $fffa9e64,'+
      '84.21 $ffffa86b,'+
      '89.47 $ffffb372,'+
      '94.74 $ffffbd78,'+
      '100 $ffffc77f;;'+
      'Sequential'
    );
    addEx( 'CoolWarm;'+
      '0 $FF3B4CC0,'+
      '10.94 $FF476FE0,'+
      '17.19 $FF5384EC,'+
      '20.31 $FF5A8FF0,'+
      '23.44 $FF6399F3,'+
      '26.56 $FF6DA4F4,'+
      '29.69 $FF78AEF5,'+
      '32.81 $FF84B8F4,'+
      '35.94 $FF92C1F2,'+
      '39.06 $FFA1C9EF,'+
      '42.19 $FFB1D1EC,'+
      '45.31 $FFC3D7E7,'+
      '48.44 $FFD6DDE2,'+
      '50 $FFDFDFDF,'+
      '53.12 $FFF5D5CF,'+
      '56.25 $FFFFC0B8,'+
      '59.38 $FFFFA89F,'+
      '62.50 $FFFF938B,'+
      '65.62 $FFFF817A,'+
      '68.75 $FFFF716D,'+
      '71.88 $FFFF6262,'+
      '75 $FFFF5559,'+
      '78.12 $FFFF4851,'+
      '81.25 $FFFF3D4B,'+
      '84.38 $FFFF3246,'+
      '87.50 $FFFF2941,'+
      '90.62 $FFF21E3A,'+
      '93.75 $FFDF1433,'+
      '96.88 $FFCA0B2C,'+
      '100 $FFB40426;;'+
      'Diverging'
    );
    addEx( 'CottonCandy;'+
      '0 $FF34D1A5,'+
      '2 $FF71C8A4,'+
      '3 $FFD1BD7F,'+
      '5 $FF65BA71,'+
      '7 $FFD88071,'+
      '9 $FFD8D1BC,'+
      '10 $FF4BD171,'+
      '20 $FFD8D19A,'+
      '25 $FFE1D171,'+
      '30 $FFD87871,'+
      '40 $FFD8D171,'+
      '50 $FF71CCF0,'+
      '60 $FFD86B71,'+
      '70 $FFDE91B3,'+
      '80 $FFD8D1F4,'+
      '90 $FFC4D881,'+
      '100 $FF850A71;;'+
      'Qualitative'
    );
    addEx( 'Cubehelix;'+
      '0 $ff000000,'+
      '5.26 $ff120817,'+
      '10.53 $ff1a1631,'+
      '15.79 $ff192a46,'+
      '21.05 $ff16404e,'+
      '26.32 $ff17584a,'+
      '31.58 $ff236a3e,'+
      '36.84 $ff3f7632,'+
      '42.11 $ff637a2f,'+
      '47.37 $ff8d7a3d,'+
      '52.63 $ffb17959,'+
      '57.89 $ffca7c83,'+
      '63.16 $ffd485ac,'+
      '68.42 $ffd297d2,'+
      '73.68 $ffc9ade9,'+
      '78.95 $ffc2c6f3,'+
      '84.21 $ffc4dcf2,'+
      '89.47 $ffd1edef,'+
      '94.74 $ffe7f8f1,'+
      '100 $ffffffff;;'+
      'Sequential'
    );
    addEx( 'CurrentDir;'+
      '0 $FFBFE9FF,'+
      '3.17 $FFC8ECF7,'+
      '9.52 $FFDAF2E7,'+
      '15.87 $FFEBF7D7,'+
      '22.22 $FFFAFCC5,'+
      '28.57 $FFFFF8B5,'+
      '34.92 $FFFFEDA3,'+
      '41.27 $FFFFE194,'+
      '47.62 $FFFFD582,'+
      '53.97 $FFFCCE95,'+
      '60.32 $FFFACBB9,'+
      '66.67 $FFF2C4D8,'+
      '73.02 $FFE9BEFA,'+
      '79.37 $FFD7BFFF,'+
      '85.71 $FFC0BFFF,'+
      '92.06 $FFBFD3FF,'+
      '100 $FFBFE9FF;;'+
      'Miscellaneous'
    );
    //  slightly modified
    addEx( 'CurrentVelocity;'+
      '0 $FFBFFFFF,'+
      '6.25 $FFADF7FF,'+
      '12.50 $FF9CF0FF,'+
      '18.75 $FF8AE8FF,'+
      '25 $FF75DFFF,'+
      '31.25 $FF61D7FF,'+
      '37.50 $FF4AD2FF,'+
      '43.75 $FF2ECBFF,'+
      '50 $FF05BFFC,'+
      '56.25 $FF0CB1F2,'+
      '62.50 $FF0E9EE6,'+
      '68.75 $FF0F8BD9,'+
      '75 $FF0E7FCF,'+
      '81.25 $FF0E6FC4,'+
      '87.50 $FF0B63BA,'+
      '93.75 $FF0752AD,'+
      '100 $FF024DA8;;'+
      'Sequential'
    );
    addEx( 'Curvature;'+
      '0 $FF000000,'+
      '45 $FF0000FF,'+
      '50 $FFFFFFFF,'+
      '55 $FFFF0000,'+
      '100 $FFFF00FF;;'+
      'Diverging'
    );
  end ;

  procedure fill_2 ;
  begin
    // brewer 'Dark2'
    addEx( 'Dark;'+
      '0 $FF1b9e77,'+
      '25 $FFd95f02,'+
      '50 $FF7570b3,'+
      '75 $FFe7298a,'+
      '100 $FF66a61e;'+
        '3 $1b9e77 $d95f02 $7570b3,'+
        '4 $1b9e77 $d95f02 $7570b3 $e7298a,'+
        '5 $1b9e77 $d95f02 $7570b3 $e7298a $66a61e,'+
        '6 $1b9e77 $d95f02 $7570b3 $e7298a $66a61e $e6ab02,'+
        '7 $1b9e77 $d95f02 $7570b3 $e7298a $66a61e $e6ab02 $a6761d,'+
        '8 $1b9e77 $d95f02 $7570b3 $e7298a $66a61e $e6ab02 $a6761d $666666;'+
      'Qualitative'
    );
    // same as 'GMTDem'
    addEx( 'DEMPrint;'+
      '0 $FF336600,'+
      '12.50 $FF81C31F,'+
      '25 $FFFFFFCC,'+
      '50 $FFF4BD45,'+
      '62.50 $FF66330C,'+
      '75 $FF663300,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'DEMScreen;'+
      '0 $FF008435,'+
      '12.50 $FF33CC00,'+
      '25 $FFF4F071,'+
      '50 $FFF4BD45,'+
      '75 $FF99642B,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );

    // slightly modified
    addEx( 'Desert;'+
      '0 $FFFFFFE5,'+
      '12.50 $FFFFF7BC,'+
      '25 $FFFEE391,'+
      '37.50 $FFFEC44F,'+
      '50 $FFFE9929,'+
      '62.50 $FFEC7014,'+
      '75 $FFCC5102,'+
      '87.50 $FF993404,'+
      '100 $FF662E08;;'+
      'Sequential'
    );
    addEx( 'Desertification;'+
      '0 $FFCEED9D,'+
      '9.52 $FFDFED95,'+
      '11.11 $FFE1ED95,'+
      '15.87 $FFEAED91,'+
      '20.63 $FFF0E990,'+
      '25.40 $FFF0DD8B,'+
      '30.16 $FFF0D286,'+
      '34.92 $FFEDC785,'+
      '39.68 $FFE3B981,'+
      '44.44 $FFD9AA7E,'+
      '49.21 $FFD19F7B,'+
      '53.97 $FFC79479,'+
      '58.73 $FFBD8975,'+
      '63.49 $FFB37F70,'+
      '68.25 $FFAD7266,'+
      '73.02 $FFAD6857,'+
      '77.78 $FFAB5C46,'+
      '82.54 $FFAB5437,'+
      '87.30 $FFAB4926,'+
      '92.06 $FFA83F16,'+
      '96.83 $FFA83A07,'+
      '100 $FFA83800;;'+
      'Sequential'
    );
    addEx( 'Dirt;'+
      '0 $FF996633,'+
      '100 $FFFFFFFF;;'+
      'Sequential'
    );
    addEx( 'DireActive;'+
      '0 $FF4F4F4F,'+
      '4.76 $FF5C5954,'+
      '6.35 $FF615D55,'+
      '11.11 $FF706858,'+
      '15.87 $FF807257,'+
      '20.63 $FF8C7953,'+
      '25.40 $FF9C814C,'+
      '30.16 $FFA88743,'+
      '34.92 $FFB88D37,'+
      '39.68 $FFC79228,'+
      '44.44 $FFD69718,'+
      '49.21 $FFE69900,'+
      '53.97 $FFE68A00,'+
      '58.73 $FFE67A00,'+
      '63.49 $FFE66B00,'+
      '68.25 $FFE65C00,'+
      '73.02 $FFE65000,'+
      '77.78 $FFE63D00,'+
      '82.54 $FFE63200,'+
      '87.30 $FFE62200,'+
      '92.06 $FFE61300,'+
      '100 $FFE60000;;'+
      'Sequential'
    );
    addEx( 'DryWet;'+
      '0 $FFFFFFBF,'+
      '4.76 $FFFAF8B6,'+
      '9.52 $FFF5F0AE,'+
      '14.29 $FFF0E8A3,'+
      '19.05 $FFEBE09B,'+
      '23.81 $FFE6D791,'+
      '28.57 $FFE0CF89,'+
      '33.33 $FFDBC681,'+
      '38.10 $FFD6BD78,'+
      '42.86 $FFD1B471,'+
      '47.62 $FFCCAB68,'+
      '52.38 $FFC7A467,'+
      '57.14 $FFBF996D,'+
      '61.90 $FFB58D70,'+
      '66.67 $FFAD8576,'+
      '71.43 $FFA37979,'+
      '76.19 $FF9C707D,'+
      '80.95 $FF91667E,'+
      '85.71 $FF8A5E83,'+
      '90.48 $FF805485,'+
      '95.24 $FF764A87,'+
      '100 $FF71468A;;'+
      'Sequential'
    );
    // original name 'GistEarth'
    addEx( 'Earth;'+
      '0 $ff000000,'+
      '5.26 $ff090e75,'+
      '10.53 $ff112c77,'+
      '15.79 $ff1b497a,'+
      '21.05 $ff23627c,'+
      '26.32 $ff2d787f,'+
      '31.58 $ff348576,'+
      '36.84 $ff3a8d65,'+
      '42.11 $ff409455,'+
      '47.37 $ff4e9c47,'+
      '52.63 $ff6ba44f,'+
      '57.89 $ff86aa55,'+
      '63.16 $ff9baf59,'+
      '68.42 $ffb2b65d,'+
      '73.68 $ffbbae61,'+
      '78.95 $ffc2a46a,'+
      '84.21 $ffd0ad8a,'+
      '89.47 $ffe0bdad,'+
      '94.74 $ffeed8d4,'+
      '100 $fffdfbfb;;'+
      'Miscellaneous'
    );
    addEx( 'Elevation1;'+
      '0 $FF00BFBF,'+
      '20 $FF00FF00,'+
      '40 $FFFFFF00,'+
      '60 $FFFF7F00,'+
      '80 $FFBF7F3F,'+
      '100 $FF141414;;'+
      'Miscellaneous'
    );
    // original name 'GMTElevation'
    addEx( 'Elevation2;'+
      '0 $FF699885,'+
      '0.71 $FF76A992,'+
      '2.86 $FF83B59B,'+
      '8.57 $FFA5C0A7,'+
      '14.29 $FFD3C9B3,'+
      '28.57 $FFD4B8A4,'+
      '42.86 $FFD4C0B5,'+
      '57.14 $FFD6D1CE,'+
      '71.43 $FFDEDDDE,'+
      '85.71 $FFEEEEEE,'+
      '100 $FFF6F7F6;;'+
      'Miscellaneous'
    );
    // original name 'ElevationWiki'
    addEx( 'Elevation3;'+
      '0 $FF7FA8CB,'+
      '5 $FF92B5D5,'+
      '10 $FFA0C2DE,'+
      '15 $FFADCBE6,'+
      '20 $FFB9D5ED,'+
      '25 $FFC4DFF4,'+
      '30 $FFCEE7F9,'+
      '35 $FFDAF0FD,'+
      '40 $FFACD0A5,'+
      '45 $FF94BF8B,'+
      '50 $FFA8C68F,'+
      '55 $FFBDCC96,'+
      '60 $FFD1D7AB,'+
      '65 $FFE1E4B5,'+
      '70 $FFEFEBC0,'+
      '75 $FFE8E1B6,'+
      '80 $FFDED6A3,'+
      '85 $FFD3CA9D,'+
      '90 $FFCAB982,'+
      '100 $FFC3A76B;;'+
      'Miscellaneous'
    );
    addEx( 'ElevationDark;'+
      '0 $FF001E50,'+
      '11.11 $FF003366,'+
      '22.22 $FF006699,'+
      '33.33 $FF0099CD,'+
      '38.89 $FF64C8FF,'+
      '42.22 $FFC6ECFF,'+
      '44.44 $FF94AB84,'+
      '45.56 $FFACBF8B,'+
      '46.67 $FFBDCC96,'+
      '50 $FFE4DFAF,'+
      '55.56 $FFE6CA94,'+
      '66.67 $FFCDAB83,'+
      '77.78 $FFB59880,'+
      '100 $FF9B7B62;;'+
      'Miscellaneous'
    );
    addEx( 'EqualMixed;'+
      '0 $FFB6CEE5,'+
      '9.09 $FF5884B3,'+
      '18.18 $FFE5B5C5,'+
      '27.27 $FFCC6686,'+
      '36.36 $FFF2CEC1,'+
      '45.45 $FFE87B70,'+
      '54.55 $FFF9EBAA,'+
      '63.64 $FFE5CF6C,'+
      '72.73 $FFCCE5B5,'+
      '81.82 $FF91BE64,'+
      '90.92 $FFB6E3D1,'+
      '100 $FF5BBE94;;'+
      'Qualitative'
    );
    addEx( 'Exploration;'+
      '0 $FF000066,'+
      '5 $FF000066,'+
      '10 $FF3300CC,'+
      '15 $FF0050FA,'+
      '20 $FF0089F5,'+
      '25 $FF1DBCEF,'+
      '30 $FF73E0F1,'+
      '35 $FFB7F4F7,'+
      '40 $FFE5FEFA,'+
      '44.51 $FF99CC66,'+
      '50 $FF99CC66,'+
      '55 $FF99CC33,'+
      '60 $FFFFFF99,'+
      '65 $FFFFFF66,'+
      '70.49 $FFFFFF00,'+
      '75 $FFFF6699,'+
      '80 $FFFF0066,'+
      '85 $FFFF0000,'+
      '90 $FF993366,'+
      '95 $FF660066,'+
      '100 $FF330066;;'+
      'Miscellaneous'
    );
    addEx( 'Flag;'+
      '0 $ffff0000,'+
      '5.26 $ff2c0000,'+
      '10.53 $ff000096,'+
      '15.79 $ff132fff,'+
      '21.05 $fff1fcff,'+
      '26.32 $ffffc38f,'+
      '31.58 $ffcd0000,'+
      '36.84 $ff2f0000,'+
      '42.11 $ff000093,'+
      '47.37 $ff112aff,'+
      '52.63 $ffeefbff,'+
      '57.89 $ffffc693,'+
      '63.16 $ffd00000,'+
      '68.42 $ff320000,'+
      '73.68 $ff00008f,'+
      '78.95 $ff0e26ff,'+
      '84.21 $ffecfbff,'+
      '89.47 $ffffc996,'+
      '94.74 $ffd30000,'+
      '100 $ff000000;;'+
      'Miscellaneous'
    );
    addEx( 'Forecast;'+
      '0 $FF9999FF,'+
      '25 $FF00CCFF,'+
      '50 $FFCCFFCC,'+
      '75 $FFFFCC00,'+
      '100 $FFFFFF99;;'+
      'Miscellaneous'
    );
    addEx( 'Forest;'+
      '0 $FF00441B,'+
      '12.5 $FF006D2C,'+
      '25 $FF238B45,'+
      '37.50 $FF41AE76,'+
      '50 $FF66C2A4,'+
      '62.50 $FF99D8CF,'+
      '75 $FFCCECEC,'+
      '87.50 $FFE5F5F9,'+
      '100 $FFF7FCFD;;'+
      'Sequential'
    );
    addEx( 'Geology;'+
      '0 $FF2B83BA,'+
      '12.50 $FF66C2A5,'+
      '25 $FFABDDA4,'+
      '37.50 $FFE6F598,'+
      '50 $FFFFFFBF,'+
      '62.50 $FFFEE08B,'+
      '75 $FFFDAE61,'+
      '87.50 $FFF46D43,'+
      '100 $FFD73D29;;'+
      'Diverging'
    );
    addEx( 'GlobalWarming;'+
      '0 $FF000000,'+
      '12.70 $FF19151A,'+
      '17.46 $FF572963,'+
      '20.63 $FF781294,'+
      '23.81 $FF731EA8,'+
      '26.98 $FF5934A8,'+
      '28.57 $FF493DA8,'+
      '30.16 $FF3843A8,'+
      '33.33 $FF0550AB,'+
      '36.51 $FF0E6FC4,'+
      '39.68 $FF1092DE,'+
      '42.86 $FF07B7F7,'+
      '46.03 $FF32BDC9,'+
      '49.21 $FF43B582,'+
      '52.38 $FF42AD42,'+
      '55.56 $FF38A800,'+
      '68.25 $FF51BA00,'+
      '71.43 $FF71D400,'+
      '74.60 $FF92ED00,'+
      '77.78 $FFB5F700,'+
      '80.95 $FFCFDE00,'+
      '84.13 $FFE8C500,'+
      '87.30 $FFFCAD00,'+
      '90.48 $FFF77800,'+
      '93.65 $FFF04400,'+
      '96.83 $FFE81700,'+
      '100 $FFE60000;;'+
      'Miscellaneous'
    );
    // original name 'GMTGlobe'
    addEx( 'Globe;'+
      '0 $FF9900FF,'+
      '10 $FF8811FF,'+
      '15 $FF6633FF,'+
      '20 $FF4455FF,'+
      '25 $FF2277FF,'+
      '30 $FF0099FF,'+
      '35 $FF36AFFF,'+
      '40 $FF6CC5FF,'+
      '45 $FFA1DBFF,'+
      '50.50 $FFBBE492,'+
      '55 $FFF3CA89,'+
      '60 $FFD9A627,'+
      '65 $FFA49019,'+
      '70 $FF9F7B0D,'+
      '75 $FF996600,'+
      '80 $FFB27676,'+
      '85 $FFC2B0B0,'+
      '90 $FFE5E5E5,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'Gravity1;'+
      '0 $FFFF00FF,'+
      '15.27 $FF9900CC,'+
      '35.96 $FF00FFFF,'+
      '55.17 $FF00FF00,'+
      '64.04 $FFFFFF00,'+
      '77.83 $FFFF0000,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'Gravity2;'+
      '0 $FF000000,'+
      '5 $FF333333,'+
      '9.51 $FF660066,'+
      '14.01 $FF990099,'+
      '19.01 $FF9900CC,'+
      '24.51 $FF003399,'+
      '29.01 $FF0000FF,'+
      '33.52 $FF339966,'+
      '38.60 $FF00FF00,'+
      '45.32 $FFFFFF00,'+
      '52.19 $FFFF6600,'+
      '58.62 $FFFF0000,'+
      '71.92 $FFFF99CC,'+
      '100 $FFFF00FF;;'+
      'Miscellaneous'
    );
    // brewer 'GnBu'
    addEx( 'GreenBlue;'+
      '0 $FFF0F9E8,'+
      '25 $FFBAE4E4,'+
      '50 $FF7BCCCC,'+
      '75 $FF43A2A2,'+
      '100 $FF0868AC;'+
        '3 $e0f3db $a8ddb5 $43a2ca,'+
        '4 $f0f9e8 $bae4bc $7bccc4 $2b8cbe,'+
        '5 $f0f9e8 $bae4bc $7bccc4 $43a2ca $0868ac,'+
        '6 $f0f9e8 $ccebc5 $a8ddb5 $7bccc4 $43a2ca $0868ac,'+
        '7 $f0f9e8 $ccebc5 $a8ddb5 $7bccc4 $4eb3d3 $2b8cbe $08589e,'+
        '8 $f7fcf0 $e0f3db $ccebc5 $a8ddb5 $7bccc4 $4eb3d3 $2b8cbe $08589e,'+
        '9 $f7fcf0 $e0f3db $ccebc5 $a8ddb5 $7bccc4 $4eb3d3 $2b8cbe $0868ac $084081;'+
      'Sequential'
    );
    // brewer 'Greens'
    addEx( 'Greens1;'+
      '0 $FFedf8e9,'+
      '25 $FFbae4b3,'+
      '50 $FF74c476,'+
      '75 $FF31a354,'+
      '100 $FF006d2c;'+
        '3 $e5f5e0 $a1d99b $31a354,'+
        '4 $edf8e9 $bae4b3 $74c476 $238b45,'+
        '5 $edf8e9 $bae4b3 $74c476 $31a354 $006d2c,'+
        '6 $edf8e9 $c7e9c0 $a1d99b $74c476 $31a354 $006d2c,'+
        '7 $edf8e9 $c7e9c0 $a1d99b $74c476 $41ab5d $238b45 $005a32,'+
        '8 $f7fcf5 $e5f5e0 $c7e9c0 $a1d99b $74c476 $41ab5d $238b45 $005a32,'+
        '9 $f7fcf5 $e5f5e0 $c7e9c0 $a1d99b $74c476 $41ab5d $238b45 $006d2c $00441b;'+
      'Sequential'
    );
    // surfer 'Greens'
    addEx( 'Greens2;'+
      '0 $FFE5FEFA,'+
      '9.36 $FFC6E6D0,'+
      '44.33 $FFADD058,'+
      '100 $FF009933;;'+
      'Sequential'
    );
    addEx( 'Greys;'+
      '0 $FFf7f7f7,'+
      '25 $FFcccccc,'+
      '50 $FF969696,'+
      '75 $FF636363,'+
      '100 $FF252525;'+
        '3 $f0f0f0 $bdbdbd $636363,'+
        '4 $f7f7f7 $cccccc $969696 $525252,'+
        '5 $f7f7f7 $cccccc $969696 $636363 $252525,'+
        '6 $f7f7f7 $d9d9d9 $bdbdbd $969696 $636363 $252525,'+
        '7 $f7f7f7 $d9d9d9 $bdbdbd $969696 $737373 $525252 $252525,'+
        '8 $ffffff $f0f0f0 $d9d9d9 $bdbdbd $969696 $737373 $525252 $252525,'+
        '9 $ffffff $f0f0f0 $d9d9d9 $bdbdbd $969696 $737373 $525252 $252525 $000000;'+
      'Sequential'
    );
    addEx( 'Gnuplot;'+
      '0 $ff000000,'+
      '5.26 $ff3a0050,'+
      '10.53 $ff510098,'+
      '15.79 $ff6501d5,'+
      '21.05 $ff7402f6,'+
      '26.32 $ff8305fe,'+
      '31.58 $ff8f08eb,'+
      '36.84 $ff9b0dbb,'+
      '42.11 $ffa5137b,'+
      '47.37 $ffb01b29,'+
      '52.63 $ffb92500,'+
      '57.89 $ffc23200,'+
      '63.16 $ffcb4000,'+
      '68.42 $ffd35200,'+
      '73.68 $ffdb6600,'+
      '78.95 $ffe37f00,'+
      '84.21 $ffea9900,'+
      '89.47 $fff2b900,'+
      '94.74 $fff8da00,'+
      '100 $ffffff00;;'+
      'Sequential'
    );
  addEx( 'Heat;'+
      '0 $FFFFFFCC,'+
      '12.50 $FFFFEDA0,'+
      '25 $FFFED976,'+
      '37.50 $FFFEB24C,'+
      '50 $FFFD8D3C,'+
      '62.50 $FFFC4E2A,'+
      '75 $FFE31A1C,'+
      '87.50 $FFBD0026,'+
      '100 $FF800026;;'+
      'Sequential'
    );
    addEx( 'HighPoints1;'+
      '0 $FF00FF00,'+
      '72.30 $FF00FF00,'+
      '72.30 $FF26FF00,'+
      '73.20 $FF4CFF00,'+
      '74.10 $FF72FF00,'+
      '75 $FF98FF00,'+
      '75.90 $FFBEFF00,'+
      '76.80 $FFE5FF00,'+
      '77.70 $FFFFF300,'+
      '78.60 $FFFFCD00,'+
      '79.50 $FFFFA700,'+
      '80.40 $FFFF8000,'+
      '81.30 $FFFF5A00,'+
      '82.10 $FFFF3400,'+
      '83 $FFFF0E00,'+
      '83.90 $FFE60000,'+
      '84.80 $FFC00000,'+
      '85.70 $FF9A0000,'+
      '86.60 $FF730000,'+
      '87.50 $FF4D0000,'+
      '88.40 $FF270000,'+
      '89.30 $FF000000,'+
      '100 $FF000000;;'+
      'Miscellaneous'
    );
    addEx( 'HighPoints2;'+
      '0 $FF000000,'+
      '70 $FF6666FF,'+
      '78.80 $FF003399,'+
      '85 $FF336666,'+
      '93 $FFFFFF00,'+
      '100 $FFFF0000;;'+
      'Miscellaneous'
    );
    // original name 'GMTHot'
    addEx( 'Hot;'+
      '0 $FF000000,'+
      '37.50 $FFFF0000,'+
      '75 $FFFFFF00,'+
      '100 $FFFFFFFF;;'+
      'Sequential'
    );
    // slightly modified
    addEx( 'Humidity;'+
      '0 $FF31007E,'+
      '4.17 $FF000081,'+
      '8.33 $FF0032B3,'+
      '12.50 $FF0000FF,'+
      '16.67 $FF007DFF,'+
      '20.83 $FF00BDFF,'+
      '25 $FF17D78B,'+
      '29.17 $FF39AD73,'+
      '33.33 $FF2AA92A,'+
      '37.50 $FF2AC82A,'+
      '41.67 $FF00FF31,'+
      '45.83 $FF53FF00,'+
      '50 $FF9FFF00,'+
      '54.16 $FFFFFF00,'+
      '58.33 $FFFFCC00,'+
      '62.50 $FFFF9800,'+
      '66.67 $FFFF6500,'+
      '70.83 $FFFF5400,'+
      '75 $FFFF0000,'+
      '79.17 $FFFF007F,'+
      '83.33 $FFFF2290,'+
      '87.50 $FFFF5EB1,'+
      '91.67 $FFFF86C2,'+
      '95.83 $FFFFAED7,'+
      '100 $FFFFD7EB;;'+
      'Miscellaneous'
    );
  end ;

  procedure fill_3 ;
  begin
    addEx( 'Ice1;'+
      '0 $FF660066,'+
      '7.39 $FF996699,'+
      '13.79 $FF996699,'+
      '21.18 $FFFF99FF,'+
      '31.03 $FF9966CC,'+
      '37.44 $FF003399,'+
      '42.36 $FF6666CC,'+
      '46.80 $FF6666FF,'+
      '50.74 $FF00CCFF,'+
      '53.20 $FF99FFFF,'+
      '55.73 $FF99CCCC,'+
      '56.19 $FF99CC33,'+
      '57.26 $FF999933,'+
      '60.12 $FFFFCC99,'+
      '68.97 $FF993366,'+
      '82.76 $FF990000,'+
      '92.61 $FF663300,'+
      '100 $FF191919;;'+
      'Miscellaneous'
    );
    addEx( 'Ice2;'+
      '0 $FF660066,'+
      '36.07 $FF99A2E0,'+
      '44.28 $FFAEBBE8,'+
      '50.44 $FFBFCEEF,'+
      '54.25 $FFC8D9F3,'+
      '58.06 $FFCAD7E9,'+
      '61.58 $FFFFFFCC,'+
      '64.79 $FFCCFFCC,'+
      '69.72 $FF99CC66,'+
      '76.22 $FFFFCC99,'+
      '81.17 $FFFF9966,'+
      '90.32 $FF8A4B3F,'+
      '100 $FF990000;;'+
      'Miscellaneous'
    );
    addEx( 'Icefire;'+
      '0 $ffb2dfd8,'+
      '2 $ffa4d5d5,'+
      '4 $ff95cbd2,'+
      '6 $ff87c2cf,'+
      '8 $ff78b9ce,'+
      '10 $ff68b0cd,'+
      '12 $ff5aa7cd,'+
      '14 $ff4e9ecd,'+
      '16 $ff4394ce,'+
      '18 $ff3a89cf,'+
      '20 $ff377fd0,'+
      '22 $ff3a73ce,'+
      '24 $ff4167c7,'+
      '26 $ff465ebe,'+
      '28 $ff4954b0,'+
      '30 $ff494c9e,'+
      '32 $ff46458a,'+
      '34 $ff413e77,'+
      '36 $ff3b3866,'+
      '38 $ff343356,'+
      '40 $ff2e2d48,'+
      '42 $ff29283b,'+
      '44 $ff242430,'+
      '46 $ff212028,'+
      '48 $ff1f1f21,'+
      '50 $ff1f1e1e,'+
      '52 $ff231e1e,'+
      '54 $ff2a1e20,'+
      '56 $ff332023,'+
      '58 $ff3d2228,'+
      '60 $ff48242c,'+
      '62 $ff542732,'+
      '64 $ff622937,'+
      '66 $ff702b3c,'+
      '68 $ff7e2d40,'+
      '70 $ff8d2e43,'+
      '72 $ff9c2f45,'+
      '74 $ffab3043,'+
      '76 $ffb93540,'+
      '78 $ffc33a3c,'+
      '80 $ffce4338,'+
      '82 $ffd74e35,'+
      '84 $ffdf5a33,'+
      '86 $ffe66734,'+
      '88 $ffeb753a,'+
      '90 $ffef8445,'+
      '92 $fff29255,'+
      '94 $fff5a066,'+
      '96 $fff7ae79,'+
      '98 $fffabb8b,'+
      '100 $fffdc99e;;'+
      'Diverging'
    );
    addEx( 'Inferno;'+
      '0 $ff000004,'+
      '2 $ff02020c,'+
      '4 $ff050417,'+
      '6 $ff0a0722,'+
      '8 $ff10092d,'+
      '10 $ff160b39,'+
      '12 $ff1e0c45,'+
      '14 $ff260c51,'+
      '16 $ff2f0a5b,'+
      '18 $ff390963,'+
      '20 $ff420a68,'+
      '22 $ff4a0c6b,'+
      '24 $ff520e6d,'+
      '26 $ff5a116e,'+
      '28 $ff62146e,'+
      '30 $ff6a176e,'+
      '32 $ff721a6e,'+
      '34 $ff7c1d6d,'+
      '36 $ff84206b,'+
      '38 $ff8c2369,'+
      '40 $ff932667,'+
      '42 $ff9b2964,'+
      '44 $ffa32c61,'+
      '46 $ffab2f5e,'+
      '48 $ffb3325a,'+
      '50 $ffbc3754,'+
      '52 $ffc33b4f,'+
      '54 $ffca404a,'+
      '56 $ffd04545,'+
      '58 $ffd74b3f,'+
      '60 $ffdd513a,'+
      '62 $ffe25734,'+
      '64 $ffe75e2e,'+
      '66 $ffeb6628,'+
      '68 $fff06f20,'+
      '70 $fff37819,'+
      '72 $fff68013,'+
      '74 $fff8890c,'+
      '76 $fffa9207,'+
      '78 $fffb9b06,'+
      '80 $fffca50a,'+
      '82 $fffcae12,'+
      '84 $fffbba1f,'+
      '86 $fffac42a,'+
      '88 $fff8cd37,'+
      '90 $fff6d746,'+
      '92 $fff4e156,'+
      '94 $fff2ea69,'+
      '96 $fff2f27d,'+
      '98 $fff5f992,'+
      '100 $fffcffa4;;'+
      'Sequential'
    );
    addEx( 'Jet;'+
      '0 $ff000080,'+
      '2 $ff000096,'+
      '4 $ff0000ad,'+
      '6 $ff0000c4,'+
      '8 $ff0000da,'+
      '10 $ff0000f1,'+
      '12 $ff0000ff,'+
      '14 $ff000cff,'+
      '16 $ff0020ff,'+
      '18 $ff0038ff,'+
      '20 $ff004cff,'+
      '22 $ff0060ff,'+
      '24 $ff0074ff,'+
      '26 $ff0088ff,'+
      '28 $ff009cff,'+
      '30 $ff00b0ff,'+
      '32 $ff00c4ff,'+
      '34 $ff00dcfe,'+
      '36 $ff09f0ee,'+
      '38 $ff19ffde,'+
      '40 $ff29ffce,'+
      '42 $ff39ffbe,'+
      '44 $ff49ffad,'+
      '46 $ff5aff9d,'+
      '48 $ff6aff8d,'+
      '50 $ff7dff7a,'+
      '52 $ff8dff6a,'+
      '54 $ff9dff5a,'+
      '56 $ffadff49,'+
      '58 $ffbeff39,'+
      '60 $ffceff29,'+
      '62 $ffdeff19,'+
      '64 $ffeeff09,'+
      '66 $fffeed00,'+
      '68 $ffffd700,'+
      '70 $ffffc400,'+
      '72 $ffffb200,'+
      '74 $ffff9f00,'+
      '76 $ffff8d00,'+
      '78 $ffff7a00,'+
      '80 $ffff6800,'+
      '82 $ffff5500,'+
      '84 $ffff3f00,'+
      '86 $ffff2d00,'+
      '88 $ffff1a00,'+
      '90 $fff10800,'+
      '92 $ffda0000,'+
      '94 $ffc40000,'+
      '96 $ffad0000,'+
      '98 $ff960000,'+
      '100 $ff800000;;'+
      'Sequential'
    );
    // surfer 'Land'
    addEx( 'Land1;'+
      '0 $FF00FF00,'+
      '21.20 $FF33CC33,'+
      '45.80 $FF996633,'+
      '75.40 $FFFFFFCC,'+
      '90.10 $FFFFFFFF,'+
      '100 $FFCCCCFF;;'+
      'Miscellaneous'
    );
    // new surfer 'Land'
    addEx( 'Land2;'+
      '0 $FF6FA685,'+
      '25 $FF9BB777,'+
      '50 $FFA6B08C,'+
      '75 $FFB6A989,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'LandEurope;'+
      '0 $FF709959,'+
      '4.76 $FF84AB67,'+
      '9.52 $FF9BBF77,'+
      '14.29 $FFB1D487,'+
      '19.05 $FFCAE899,'+
      '23.81 $FFD9ED98,'+
      '28.57 $FFE9ED93,'+
      '33.33 $FFF0E48D,'+
      '38.10 $FFF0D086,'+
      '42.86 $FFE8BE84,'+
      '47.62 $FFDEAF83,'+
      '52.38 $FFD19D80,'+
      '57.14 $FFC4917E,'+
      '61.90 $FFC99885,'+
      '66.67 $FFD6AD94,'+
      '71.43 $FFE3C3A8,'+
      '76.19 $FFF0D9BB,'+
      '80.95 $FFF7E7CD,'+
      '85.71 $FFFAEEDC,'+
      '90.48 $FFFAF3E8,'+
      '95.24 $FFFCFAF5,'+
      '100 $FFFCFCFC;;'+
      'Miscellaneous'
    );
    addEx( 'LandAmerica;'+
      '0 $FF4C7300,'+
      '4.76 $FF678F18,'+
      '9.52 $FF85AD39,'+
      '14.29 $FFA4C963,'+
      '19.05 $FFCAE897,'+
      '23.81 $FFD9ED98,'+
      '28.57 $FFE9ED93,'+
      '33.33 $FFF0E48D,'+
      '38.10 $FFF0D086,'+
      '42.86 $FFE3B868,'+
      '47.62 $FFCC9843,'+
      '52.38 $FFB87C23,'+
      '57.14 $FFA3630A,'+
      '61.90 $FFA35200,'+
      '66.67 $FFB84000,'+
      '71.43 $FFC92C00,'+
      '76.98 $FFDB0F00,'+
      '81.75 $FFE81A1A,'+
      '86.51 $FFED5353,'+
      '91.27 $FFF28F8F,'+
      '96.03 $FFFACFCF,'+
      '100 $FFFCFAFA;;'+
      'Miscellaneous'
    );
    addEx( 'LandArid;'+
      '0 $FF99CC66,'+
      '21.20 $FFFFFF99,'+
      '45.80 $FF996633,'+
      '75.40 $FFFFFFCC,'+
      '90.10 $FFFFFFFF,'+
      '100 $FFCCCCFF;;'+
      'Miscellaneous'
    );
    addEx( 'LandSea;'+
      '0 $FF000000,'+
      '3 $FF003399,'+
      '32 $FF00FFFF,'+
      '44 $FF663300,'+
      '100 $FFFFFF66;;'+
      'Miscellaneous'
    );
    addEx( 'Magma;'+
      '0 $ff000004,'+
      '2 $ff02020b,'+
      '4 $ff050416,'+
      '6 $ff090720,'+
      '8 $ff0e0b2b,'+
      '10 $ff140e36,'+
      '12 $ff1a1042,'+
      '14 $ff21114e,'+
      '16 $ff29115a,'+
      '18 $ff331067,'+
      '20 $ff3b0f70,'+
      '22 $ff440f76,'+
      '24 $ff4c117a,'+
      '26 $ff54137d,'+
      '28 $ff5c167f,'+
      '30 $ff641a80,'+
      '32 $ff6b1d81,'+
      '34 $ff752181,'+
      '36 $ff7c2382,'+
      '38 $ff842681,'+
      '40 $ff8c2981,'+
      '42 $ff942c80,'+
      '44 $ff9c2e7f,'+
      '46 $ffa5317e,'+
      '48 $ffad347c,'+
      '50 $ffb73779,'+
      '52 $ffbf3a77,'+
      '54 $ffc73d73,'+
      '56 $ffcf4070,'+
      '58 $ffd6456c,'+
      '60 $ffde4968,'+
      '62 $ffe44f64,'+
      '64 $ffea5661,'+
      '66 $ffef5d5e,'+
      '68 $fff4675c,'+
      '70 $fff7705c,'+
      '72 $fff9795d,'+
      '74 $fffb835f,'+
      '76 $fffc8c63,'+
      '78 $fffd9668,'+
      '80 $fffe9f6d,'+
      '82 $fffea973,'+
      '84 $fffeb47b,'+
      '86 $fffebd82,'+
      '88 $fffec68a,'+
      '90 $fffecf92,'+
      '92 $fffed89a,'+
      '94 $fffde2a3,'+
      '96 $fffdebac,'+
      '98 $fffcf4b6,'+
      '100 $fffcfdbf;;'+
      'Sequential'
    );
    addEx( 'Mako;'+
      '0 $ff10060a,'+
      '2 $ff150a12,'+
      '4 $ff1a0e19,'+
      '6 $ff1f1220,'+
      '8 $ff241628,'+
      '10 $ff291930,'+
      '12 $ff2d1d38,'+
      '14 $ff312140,'+
      '16 $ff342548,'+
      '18 $ff372851,'+
      '20 $ff3a2c59,'+
      '22 $ff3c3162,'+
      '24 $ff3e356b,'+
      '26 $ff403872,'+
      '28 $ff413d7b,'+
      '30 $ff414184,'+
      '32 $ff40478b,'+
      '34 $ff3f4c92,'+
      '36 $ff3d5296,'+
      '38 $ff3b589a,'+
      '40 $ff395e9c,'+
      '42 $ff37649e,'+
      '44 $ff366a9f,'+
      '46 $ff3670a0,'+
      '48 $ff3575a1,'+
      '50 $ff357ba3,'+
      '52 $ff347fa4,'+
      '54 $ff3485a5,'+
      '56 $ff348ba6,'+
      '58 $ff3490a8,'+
      '60 $ff3496a9,'+
      '62 $ff359baa,'+
      '64 $ff35a1ab,'+
      '66 $ff37a6ac,'+
      '68 $ff39acac,'+
      '70 $ff3cb2ad,'+
      '72 $ff40b7ad,'+
      '74 $ff45bdad,'+
      '76 $ff4bc2ad,'+
      '78 $ff50c6ad,'+
      '80 $ff59ccad,'+
      '82 $ff65d0ad,'+
      '84 $ff73d4ad,'+
      '86 $ff82d8b0,'+
      '88 $ff91dbb4,'+
      '90 $ff9edfb8,'+
      '92 $ffabe2be,'+
      '94 $ffb7e6c5,'+
      '96 $ffc2e9cd,'+
      '98 $ffccedd6,'+
      '100 $ffd6f1de;;'+
      'Sequential'
    );
    addEx( 'Ocean1;'+
      '0 $ff008000,'+
      '5.26 $ff006c0d,'+
      '10.53 $ff00581a,'+
      '15.79 $ff004428,'+
      '21.05 $ff003035,'+
      '26.32 $ff001b43,'+
      '31.58 $ff000850,'+
      '36.84 $ff000e5e,'+
      '42.11 $ff00216b,'+
      '47.37 $ff003679,'+
      '52.63 $ff004a86,'+
      '57.89 $ff005e94,'+
      '63.16 $ff0072a1,'+
      '68.42 $ff0f87af,'+
      '73.68 $ff369bbc,'+
      '78.95 $ff60b0ca,'+
      '84.21 $ff87c3d7,'+
      '89.47 $ffb1d8e5,'+
      '94.74 $ffd8ebf2,'+
      '100 $ffffffff;;'+
      'Miscellaneous'
    );
    // original name 'GMTOcean'
    addEx( 'Ocean2;'+
      '0.0 $FF000000,'+
      '12.5 $FF000519,'+
      '25.0 $FF000A32,'+
      '37.5 $FF00507D,'+
      '50.0 $FF0096C8,'+
      '62.5 $FF56C5B8,'+
      '75.0 $FFACF5A8,'+
      '87.5 $FFD3FAD3,'+
      '100.0 $FFFAFFFF;;'+
      'Miscellaneous'
    );
    // brewer 'PuOr'
    addEx( 'OrangePurple;'+
      '0 $FFE66101,'+
      '25 $FFFDB863,'+
      '50 $FFF7F7F7,'+
      '75 $FFB2ABD2,'+
      '100 $FF5E3C99;'+
        '3 $f1a340 $f7f7f7 $998ec3,'+
        '4 $e66101 $fdb863 $b2abd2 $5e3c99,'+
        '5 $e66101 $fdb863 $f7f7f7 $b2abd2 $5e3c99,'+
        '6 $b35806 $f1a340 $fee0b6 $d8daeb $998ec3 $542788,'+
        '7 $b35806 $f1a340 $fee0b6 $f7f7f7 $d8daeb $998ec3 $542788,'+
        '8 $b35806 $e08214 $fdb863 $fee0b6 $d8daeb $b2abd2 $8073ac $542788,'+
        '9 $b35806 $e08214 $fdb863 $fee0b6 $f7f7f7 $d8daeb $b2abd2 $8073ac $542788,'+
        '10 $7f3b08 $b35806 $e08214 $fdb863 $fee0b6 $d8daeb $b2abd2 $8073ac $542788 $2d004b,'+
        '11 $7f3b08 $b35806 $e08214 $fdb863 $fee0b6 $f7f7f7 $d8daeb $b2abd2 $8073ac $542788 $2d004b;'+
      'Diverging'
    );
    // brewer 'OrRd'
    addEx( 'OrangeRed;'+
      '0 $FFfef0d9,'+
      '25 $FFfdcc8a,'+
      '50 $FFfc8d59,'+
      '75 $FFe34a33,'+
      '100 $FFb30000;'+
        '3 $fee8c8 $fdbb84 $e34a33,'+
        '4 $fef0d9 $fdcc8a $fc8d59 $d7301f,'+
        '5 $fef0d9 $fdcc8a $fc8d59 $e34a33 $b30000,'+
        '6 $fef0d9 $fdd49e $fdbb84 $fc8d59 $e34a33 $b30000,'+
        '7 $fef0d9 $fdd49e $fdbb84 $fc8d59 $ef6548 $d7301f $990000,'+
        '8 $fff7ec $fee8c8 $fdd49e $fdbb84 $fc8d59 $ef6548 $d7301f $990000,'+
        '9 $fff7ec $fee8c8 $fdd49e $fdbb84 $fc8d59 $ef6548 $d7301f $b30000 $7f0000;'+
      'Sequential'
    );
    addEx( 'Oranges;'+
      '0 $FFfeedde,'+
      '25 $FFfdbe85,'+
      '50 $FFfd8d3c,'+
      '75 $FFe6550d,'+
      '100 $FFa63603;'+
        '3 $fee6ce $fdae6b $e6550d,'+
        '4 $feedde $fdbe85 $fd8d3c $d94701,'+
        '5 $feedde $fdbe85 $fd8d3c $e6550d $a63603,'+
        '6 $feedde $fdd0a2 $fdae6b $fd8d3c $e6550d $a63603,'+
        '7 $feedde $fdd0a2 $fdae6b $fd8d3c $f16913 $d94801 $8c2d04,'+
        '8 $fff5eb $fee6ce $fdd0a2 $fdae6b $fd8d3c $f16913 $d94801 $8c2d04,'+
        '9 $fff5eb $fee6ce $fdd0a2 $fdae6b $fd8d3c $f16913 $d94801 $a63603 $7f2704;'+
      'Sequential'
    );
    addEx( 'Pastel1;'+
      '0 $FFfbb4ae,'+
      '25 $FFb3cde3,'+
      '50 $FFccebc5,'+
      '75 $FFdecbe4,'+
      '100 $FFfed9a6;'+
        '3 $fbb4ae $b3cde3 $ccebc5,'+
        '4 $fbb4ae $b3cde3 $ccebc5 $decbe4,'+
        '5 $fbb4ae $b3cde3 $ccebc5 $decbe4 $fed9a6,'+
        '6 $fbb4ae $b3cde3 $ccebc5 $decbe4 $fed9a6 $ffffcc,'+
        '7 $fbb4ae $b3cde3 $ccebc5 $decbe4 $fed9a6 $ffffcc $e5d8bd,'+
        '8 $fbb4ae $b3cde3 $ccebc5 $decbe4 $fed9a6 $ffffcc $e5d8bd $fddaec,'+
        '9 $fbb4ae $b3cde3 $ccebc5 $decbe4 $fed9a6 $ffffcc $e5d8bd $fddaec $f2f2f2;'+
      'Qualitative'
    );
    addEx( 'Pastel2;'+
      '0 $FFb3e2cd,'+
      '25 $FFfdcdac,'+
      '50 $FFcbd5e8,'+
      '75 $FFf4cae4,'+
      '100 $FFe6f5c9;'+
        '3 $b3e2cd $fdcdac $cbd5e8,'+
        '4 $b3e2cd $fdcdac $cbd5e8 $f4cae4,'+
        '5 $b3e2cd $fdcdac $cbd5e8 $f4cae4 $e6f5c9,'+
        '6 $b3e2cd $fdcdac $cbd5e8 $f4cae4 $e6f5c9 $fff2ae,'+
        '7 $b3e2cd $fdcdac $cbd5e8 $f4cae4 $e6f5c9 $fff2ae $f1e2cc,'+
        '8 $b3e2cd $fdcdac $cbd5e8 $f4cae4 $e6f5c9 $fff2ae $f1e2cc $cccccc;'+
      'Qualitative'
    );
    // modified surfer 'Pastel1'
    addEx( 'Pastel3;'+
      '0 $FFE6E6E6,'+
      '16.67 $FFCCCCFF,'+
      '33.33 $FF99CCCC,'+
      '50 $FFCCFFCC,'+
      '66.67 $FFFFFFCC,'+
      '83.33 $FFFFCCCC,'+
      '100 $FFFFFFFF;;'+
      'Qualitative'
    );
    // modified surfer 'Pastel2'
    addEx( 'Pastel4;'+
      '0 $FFFFFFC0,'+
      '25 $FF97C39D,'+
      '50 $FF7BDCFF,'+
      '75 $FF8E7BA7,'+
      '100 $FFEF58A4;;'+
      'Qualitative'
    );
    addEx( 'Paired;'+
      '0 $FFa6cee3,'+
      '11.11 $FF1f78b4,'+
      '22.22 $FFb2df8a,'+
      '33.33 $FF33a02c,'+
      '44.44 $FFfb9a99,'+
      '55.56 $FFe31a1c,'+
      '66.67 $FFfdbf6f,'+
      '77.78 $FFff7f00,'+
      '88.89 $FFcab2d6,'+
      '100 $FF6a3d9a;'+
        '3 $a6cee3 $1f78b4 $b2df8a,'+
        '4 $a6cee3 $1f78b4 $b2df8a $33a02c,'+
        '5 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99,'+
        '6 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c,'+
        '7 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c $fdbf6f,'+
        '8 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c $fdbf6f $ff7f00,'+
        '9 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c $fdbf6f $ff7f00 $cab2d6,'+
        '10 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c $fdbf6f $ff7f00 $cab2d6 $6a3d9a,'+
        '11 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c $fdbf6f $ff7f00 $cab2d6 $6a3d9a $ffff99,'+
        '12 $a6cee3 $1f78b4 $b2df8a $33a02c $fb9a99 $e31a1c $fdbf6f $ff7f00 $cab2d6 $6a3d9a $ffff99 $b15928;'+
      'Qualitative'
    );
    addEx( 'Pink;'+
      '0 $ff1e0000,'+
      '5.26 $ff4d2f2f,'+
      '10.53 $ff694242,'+
      '15.79 $ff815252,'+
      '21.05 $ff935f5f,'+
      '26.32 $ffa56b6b,'+
      '31.58 $ffb47575,'+
      '36.84 $ffc27f7e,'+
      '42.11 $ffc89287,'+
      '47.37 $ffcea48f,'+
      '52.63 $ffd3b397,'+
      '57.89 $ffd8c19f,'+
      '63.16 $ffddcea5,'+
      '68.42 $ffe3dbac,'+
      '73.68 $ffe8e7b3,'+
      '78.95 $ffededc4,'+
      '84.21 $fff1f1d4,'+
      '89.47 $fff6f6e4,'+
      '94.74 $fffbfbf2,'+
      '100 $ffffffff;;'+
      'Sequential'
    );
    addEx( 'Plasma;'+
      '0 $ff0d0887,'+
      '2 $ff1b068d,'+
      '4 $ff260591,'+
      '6 $ff2f0596,'+
      '8 $ff38049a,'+
      '10 $ff41049d,'+
      '12 $ff4903a0,'+
      '14 $ff5102a3,'+
      '16 $ff5901a5,'+
      '18 $ff6300a7,'+
      '20 $ff6a00a8,'+
      '22 $ff7201a8,'+
      '24 $ff7a02a8,'+
      '26 $ff8104a7,'+
      '28 $ff8808a6,'+
      '30 $ff8f0da4,'+
      '32 $ff9613a1,'+
      '34 $ff9e199d,'+
      '36 $ffa51f99,'+
      '38 $ffab2494,'+
      '40 $ffb12a90,'+
      '42 $ffb6308b,'+
      '44 $ffbc3587,'+
      '46 $ffc13b82,'+
      '48 $ffc6417d,'+
      '50 $ffcc4778,'+
      '52 $ffd04d73,'+
      '54 $ffd5536f,'+
      '56 $ffd9586a,'+
      '58 $ffdd5e66,'+
      '60 $ffe16462,'+
      '62 $ffe56a5d,'+
      '64 $ffe87059,'+
      '66 $ffeb7655,'+
      '68 $ffef7e50,'+
      '70 $fff2844b,'+
      '72 $fff58b47,'+
      '74 $fff79143,'+
      '76 $fff9983e,'+
      '78 $fffb9f3a,'+
      '80 $fffca636,'+
      '82 $fffdae32,'+
      '84 $fffeb72d,'+
      '86 $fffebe2a,'+
      '88 $fffdc627,'+
      '90 $fffcce25,'+
      '92 $fffbd724,'+
      '94 $fff8df25,'+
      '96 $fff6e826,'+
      '98 $fff3f027,'+
      '100 $fff0f921;;'+
      'Sequential'
    );
    // brewer 'PiYG'
    addEx( 'PinkGreen;'+
      '0 $FFd01c8b,'+
      '25 $FFf1b6da,'+
      '50 $FFf7f7f7,'+
      '75 $FFb8e186,'+
      '100 $FF4dac26;'+
        '3 $e9a3c9 $f7f7f7 $a1d76a,'+
        '4 $d01c8b $f1b6da $b8e186 $4dac26,'+
        '5 $d01c8b $f1b6da $f7f7f7 $b8e186 $4dac26,'+
        '6 $c51b7d $e9a3c9 $fde0ef $e6f5d0 $a1d76a $4d9221,'+
        '7 $c51b7d $e9a3c9 $fde0ef $f7f7f7 $e6f5d0 $a1d76a $4d9221,'+
        '8 $c51b7d $de77ae $f1b6da $fde0ef $e6f5d0 $b8e186 $7fbc41 $4d9221,'+
        '9 $c51b7d $de77ae $f1b6da $fde0ef $f7f7f7 $e6f5d0 $b8e186 $7fbc41 $4d9221,'+
        '10 $8e0152 $c51b7d $de77ae $f1b6da $fde0ef $e6f5d0 $b8e186 $7fbc41 $4d9221 $276419,'+
        '11 $8e0152 $c51b7d $de77ae $f1b6da $fde0ef $f7f7f7 $e6f5d0 $b8e186 $7fbc41 $4d9221 $276419;'+
      'Diverging'
    );
    addEx( 'Prism;'+
      '0 $ffff0000,'+
      '5.26 $ff0bbd39,'+
      '10.53 $ffff0e00,'+
      '15.79 $ff0043d7,'+
      '21.05 $ffff8c00,'+
      '26.32 $ff2000ff,'+
      '31.58 $fffff100,'+
      '36.84 $ff8f00e1,'+
      '42.11 $ff9fff00,'+
      '47.37 $ffff0048,'+
      '52.63 $ff2be300,'+
      '57.89 $ffff0e00,'+
      '63.16 $ff0075a2,'+
      '68.42 $ffff8b00,'+
      '73.68 $ff0300ff,'+
      '78.95 $fffff100,'+
      '84.21 $ff6000ff,'+
      '89.47 $ff9fff00,'+
      '94.74 $ffdd008b,'+
      '100 $ff54ff00;;'+
      'Miscellaneous'
    );
    // brewer 'PuBu'
     addEx( 'PurpleBlue;'+
      '0 $FFf1eef6,'+
      '25 $FFbdc9e1,'+
      '50 $FF74a9cf,'+
      '75 $FF2b8cbe,'+
      '100 $FF045a8d;'+
        '3 $ece7f2 $a6bddb $2b8cbe,'+
        '4 $f1eef6 $bdc9e1 $74a9cf $0570b0,'+
        '5 $f1eef6 $bdc9e1 $74a9cf $2b8cbe $045a8d,'+
        '6 $f1eef6 $d0d1e6 $a6bddb $74a9cf $2b8cbe $045a8d,'+
        '7 $f1eef6 $d0d1e6 $a6bddb $74a9cf $3690c0 $0570b0 $034e7b,'+
        '8 $fff7fb $ece7f2 $d0d1e6 $a6bddb $74a9cf $3690c0 $0570b0 $034e7b,'+
        '9 $fff7fb $ece7f2 $d0d1e6 $a6bddb $74a9cf $3690c0 $0570b0 $045a8d $023858;'+
      'Sequential'
    );
    // brewer 'PuBuGn'
    addEx( 'PurpleBlueGreen;'+
      '0 $FFf6eff7,'+
      '25 $FFbdc9e1,'+
      '50 $FF67a9cf,'+
      '75 $FF1c9099,'+
      '100 $FF016c59;'+
        '3 $ece2f0 $a6bddb $1c9099,'+
        '4 $f6eff7 $bdc9e1 $67a9cf $02818a,'+
        '5 $f6eff7 $bdc9e1 $67a9cf $1c9099 $016c59,'+
        '6 $f6eff7 $d0d1e6 $a6bddb $67a9cf $1c9099 $016c59,'+
        '7 $f6eff7 $d0d1e6 $a6bddb $67a9cf $3690c0 $02818a $016450,'+
        '8 $fff7fb $ece2f0 $d0d1e6 $a6bddb $67a9cf $3690c0 $02818a $016450,'+
        '9 $fff7fb $ece2f0 $d0d1e6 $a6bddb $67a9cf $3690c0 $02818a $016c59 $014636;'+
      'Sequential'
    );
    // brewer 'PRGn'
    addEx( 'PurpleGreen1;'+
      '0 $FF7b3294,'+
      '25 $FFc2a5cf,'+
      '50 $FFf7f7f7,'+
      '75 $FFa6dba0,'+
      '100 $FF008837;'+
        '3 $af8dc3 $f7f7f7 $7fbf7b,'+
        '4 $7b3294 $c2a5cf $a6dba0 $008837,'+
        '5 $7b3294 $c2a5cf $f7f7f7 $a6dba0 $008837,'+
        '6 $762a83 $af8dc3 $e7d4e8 $d9f0d3 $7fbf7b $1b7837,'+
        '7 $762a83 $af8dc3 $e7d4e8 $f7f7f7 $d9f0d3 $7fbf7b $1b7837,'+
        '8 $762a83 $9970ab $c2a5cf $e7d4e8 $d9f0d3 $a6dba0 $5aae61 $1b7837,'+
        '9 $762a83 $9970ab $c2a5cf $e7d4e8 $f7f7f7 $d9f0d3 $a6dba0 $5aae61 $1b7837,'+
        '10 $40004b $762a83 $9970ab $c2a5cf $e7d4e8 $d9f0d3 $a6dba0 $5aae61 $1b7837 $00441b,'+
        '11 $40004b $762a83 $9970ab $c2a5cf $e7d4e8 $f7f7f7 $d9f0d3 $a6dba0 $5aae61 $1b7837 $00441b;'+
      'Diverging'
    );
    //  surfer 'PurpleGreen'
    addEx( 'PurpleGreen2;'+
      '0 $FF585274,'+
      '17.75 $FF8E7BA7,'+
      '33.50 $FFC3C3E0,'+
      '50.25 $FFF9F9FF,'+
      '70 $FFC6E6D0,'+
      '83.25 $FF97C39D,'+
      '100 $FF396236;;'+
      'Diverging'
    );
    // brewer 'PuRd'
    addEx( 'PurpleRed;'+
      '0 $FFf1eef6,'+
      '25 $FFd7b5d8,'+
      '50 $FFdf65b0,'+
      '75 $FFdd1c77,'+
      '100 $FF980043;'+
        '3 $e7e1ef $c994c7 $dd1c77,'+
        '4 $f1eef6 $d7b5d8 $df65b0 $ce1256,'+
        '5 $f1eef6 $d7b5d8 $df65b0 $dd1c77 $980043,'+
        '6 $f1eef6 $d4b9da $c994c7 $df65b0 $dd1c77 $980043,'+
        '7 $f1eef6 $d4b9da $c994c7 $df65b0 $e7298a $ce1256 $91003f,'+
        '8 $f7f4f9 $e7e1ef $d4b9da $c994c7 $df65b0 $e7298a $ce1256 $91003f,'+
        '9 $f7f4f9 $e7e1ef $d4b9da $c994c7 $df65b0 $e7298a $ce1256 $980043 $67001f;'+
      'Sequential'
    );
    addEx( 'Purples;'+
      '0 $FFf2f0f7,'+
      '25 $FFcbc9e2,'+
      '50 $FF9e9ac8,'+
      '75 $FF756bb1,'+
      '100 $FF54278f;'+
        '3 $efedf5 $bcbddc $756bb1,'+
        '4 $f2f0f7 $cbc9e2 $9e9ac8 $6a51a3,'+
        '5 $f2f0f7 $cbc9e2 $9e9ac8 $756bb1 $54278f,'+
        '6 $f2f0f7 $dadaeb $bcbddc $9e9ac8 $756bb1 $54278f,'+
        '7 $f2f0f7 $dadaeb $bcbddc $9e9ac8 $807dba $6a51a3 $4a1486,'+
        '8 $fcfbfd $efedf5 $dadaeb $bcbddc $9e9ac8 $807dba $6a51a3 $4a1486,'+
        '9 $fcfbfd $efedf5 $dadaeb $bcbddc $9e9ac8 $807dba $6a51a3 $54278f $3f007d;'+
      'Sequential'
    );
  end ;

  procedure fill_4 ;
  begin
    addEx( 'Rainbow1;'+
      '0 $FF9966FF,'+
      '20 $FF0000FF,'+
      '40 $FF00FF00,'+
      '60 $FFFFFF00,'+
      '80 $FFFF6600,'+
      '100 $FFFF0000;;'+
      'Miscellaneous'
    );
    addEx( 'Rainbow2;'+
      '0 $FF000000,'+
      '11 $FF0000FF,'+
      '26 $FF00FFFF,'+
      '49 $FF009900,'+
      '59 $FFFFFF00,'+
      '80 $FFFF0000,'+
      '96 $FFFFFFFF,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    // surfer 'Rainbow5'
    addEx( 'Rainbow3;'+
      '0 $FF040404,'+
      '1 $FF0F040D,'+
      '2 $FF150612,'+
      '3 $FF190815,'+
      '4 $FF1D0A19,'+
      '5 $FF200C1C,'+
      '6 $FF230E1F,'+
      '7 $FF260F21,'+
      '8 $FF291124,'+
      '9 $FF2B1326,'+
      '10 $FF2E142B,'+
      '11 $FF2D1537,'+
      '12 $FF2D1642,'+
      '13 $FF2D184B,'+
      '14 $FF2D1953,'+
      '15 $FF2C1B5B,'+
      '16 $FF2C1C62,'+
      '17 $FF2C1E68,'+
      '18 $FF2C206E,'+
      '19 $FF2C2275,'+
      '20 $FF282678,'+
      '21 $FF232B78,'+
      '22 $FF202F78,'+
      '23 $FF1E337A,'+
      '24 $FF1D367D,'+
      '25 $FF1B397F,'+
      '26 $FF1C3B81,'+
      '27 $FF1B3E84,'+
      '28 $FF1B4186,'+
      '29 $FF1B4389,'+
      '30 $FF184887,'+
      '31 $FF144C83,'+
      '32 $FF0F507F,'+
      '33 $FF0A547B,'+
      '34 $FF085877,'+
      '35 $FF055B74,'+
      '36 $FF045F72,'+
      '37 $FF01626E,'+
      '38 $FF00656C,'+
      '39 $FF00696A,'+
      '40 $FF006C65,'+
      '41 $FF007063,'+
      '42 $FF00735F,'+
      '43 $FF00775B,'+
      '44 $FF007A58,'+
      '45 $FF007D54,'+
      '46 $FF008050,'+
      '47 $FF00844B,'+
      '48 $FF008747,'+
      '49 $FF008A42,'+
      '50 $FF008D3F,'+
      '51 $FF00913C,'+
      '52 $FF00943A,'+
      '53 $FF009737,'+
      '54 $FF009B33,'+
      '55 $FF009E30,'+
      '56 $FF00A22C,'+
      '57 $FF00A528,'+
      '58 $FF00A822,'+
      '59 $FF00AA10,'+
      '60 $FF14AB00,'+
      '61 $FF26AD00,'+
      '62 $FF31B000,'+
      '63 $FF39B200,'+
      '64 $FF40B500,'+
      '65 $FF46B700,'+
      '66 $FF4CBA00,'+
      '67 $FF51BD00,'+
      '68 $FF56BF00,'+
      '69 $FF63C100,'+
      '70 $FF75C100,'+
      '71 $FF82C300,'+
      '72 $FF8FC300,'+
      '73 $FF9AC400,'+
      '74 $FFA4C500,'+
      '75 $FFADC700,'+
      '76 $FFB7C700,'+
      '77 $FFBFC900,'+
      '78 $FFC7CA00,'+
      '79 $FFCCCA39,'+
      '80 $FFD1CB57,'+
      '81 $FFD6CC6A,'+
      '82 $FFDBCE79,'+
      '83 $FFE1CF88,'+
      '84 $FFE6D194,'+
      '85 $FFEAD29E,'+
      '86 $FFEFD4A9,'+
      '87 $FFF3D6B3,'+
      '88 $FFF8D7BB,'+
      '89 $FFF7DBC3,'+
      '90 $FFF8DECA,'+
      '91 $FFF7E2D1,'+
      '92 $FFF7E5D7,'+
      '93 $FFF7E9DD,'+
      '94 $FFF7ECE4,'+
      '95 $FFF7EFE9,'+
      '96 $FFF7F3EF,'+
      '97 $FFF8F6F5,'+
      '98 $FFF9F9F9,'+
      '99 $FFFCFCFC,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    // surfer 'Rainbow6'
    addEx( 'Rainbow4;'+
      '0 $FF000066,'+
      '20 $FF00FFFF,'+
      '40 $FF99CC33,'+
      '60 $FFFFFF00,'+
      '80 $FFFF6600,'+
      '100 $FF990000;;'+
      'Miscellaneous'
    );
    // surfer 'Rainbow8'
    addEx( 'Rainbow5;'+
      '0 $FF451888,'+
      '5 $FF230781,'+
      '10 $FF0D0E8B,'+
      '15 $FF0B2294,'+
      '20 $FF0F50AC,'+
      '25 $FF1590D4,'+
      '30 $FF09B9B6,'+
      '35 $FF0F862D,'+
      '40 $FF1D9422,'+
      '45 $FF3E9C18,'+
      '50 $FF5CAA12,'+
      '55 $FF94BF21,'+
      '60 $FFB0BD1C,'+
      '65 $FFD8C02C,'+
      '70 $FFE5B145,'+
      '75 $FFE7905D,'+
      '80 $FFE27973,'+
      '85 $FFEB867F,'+
      '90 $FFC8827D,'+
      '95 $FFBDA49E,'+
      '100 $FFC8B3B1;;'+
      'Miscellaneous'
    );
    addEx( 'RainbowLight;'+
      '0 $FFCC99CC,'+
      '16.75 $FFCC99FF,'+
      '28.08 $FF6699FF,'+
      '43.35 $FF99FFFF,'+
      '55.67 $FF99FF00,'+
      '66.50 $FFCCFF66,'+
      '78.82 $FFFFFF99,'+
      '92.12 $FFFFCC00,'+
      '100 $FFFF9966;;'+
      'Miscellaneous'
    );
    addEx( 'RainbowPastel;'+
      '0 $FFE6E6E6,'+
      '11 $FFCCCCFF,'+
      '26 $FF99CCCC,'+
      '42.90 $FFCCFFCC,'+
      '59 $FFFFFFCC,'+
      '80 $FFFFCCCC,'+
      '96 $FFFFFFFF,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    // brewer 'RdBu'
    addEx( 'RedBlue;'+
      '0 $FFca0020,'+
      '25 $FFf4a582,'+
      '50 $FFf7f7f7,'+
      '75 $FF92c5de,'+
      '100 $FF0571b0;'+
        '3 $ef8a62 $f7f7f7 $67a9cf,'+
        '4 $ca0020 $f4a582 $92c5de $0571b0,'+
        '5 $ca0020 $f4a582 $f7f7f7 $92c5de $0571b0,'+
        '6 $b2182b $ef8a62 $fddbc7 $d1e5f0 $67a9cf $2166ac,'+
        '7 $b2182b $ef8a62 $fddbc7 $f7f7f7 $d1e5f0 $67a9cf $2166ac,'+
        '8 $b2182b $d6604d $f4a582 $fddbc7 $d1e5f0 $92c5de $4393c3 $2166ac,'+
        '9 $b2182b $d6604d $f4a582 $fddbc7 $f7f7f7 $d1e5f0 $92c5de $4393c3 $2166ac,'+
        '10 $67001f $b2182b $d6604d $f4a582 $fddbc7 $d1e5f0 $92c5de $4393c3 $2166ac $053061,'+
        '11 $67001f $b2182b $d6604d $f4a582 $fddbc7 $f7f7f7 $d1e5f0 $92c5de $4393c3 $2166ac $053061;'+
      'Diverging'
    );
    // brewer 'RdGy'
    addEx( 'RedGray;'+
      '0 $FFca0020,'+
      '25 $FFf4a582,'+
      '50 $FFffffff,'+
      '75 $FFbababa,'+
      '100 $FF404040;'+
        '3 $ef8a62 $ffffff $999999,'+
        '4 $ca0020 $f4a582 $bababa $404040,'+
        '5 $ca0020 $f4a582 $ffffff $bababa $404040,'+
        '6 $b2182b $ef8a62 $fddbc7 $e0e0e0 $999999 $4d4d4d,'+
        '7 $b2182b $ef8a62 $fddbc7 $ffffff $e0e0e0 $999999 $4d4d4d,'+
        '8 $b2182b $d6604d $f4a582 $fddbc7 $e0e0e0 $bababa $878787 $4d4d4d,'+
        '9 $b2182b $d6604d $f4a582 $fddbc7 $ffffff $e0e0e0 $bababa $878787 $4d4d4d,'+
        '10 $67001f $b2182b $d6604d $f4a582 $fddbc7 $e0e0e0 $bababa $878787 $4d4d4d $1a1a1a,'+
        '11 $67001f $b2182b $d6604d $f4a582 $fddbc7 $ffffff $e0e0e0 $bababa $878787 $4d4d4d $1a1a1a;'+
      'Diverging'
    );
    // brewer 'RdPu'
    addEx( 'RedPurple;'+
      '0 $FFfeebe2,'+
      '25 $FFfbb4b9,'+
      '50 $FFf768a1,'+
      '75 $FFc51b8a,'+
      '100 $FF7a0177;'+
        '3 $fde0dd $fa9fb5 $c51b8a,'+
        '4 $feebe2 $fbb4b9 $f768a1 $ae017e,'+
        '5 $feebe2 $fbb4b9 $f768a1 $c51b8a $7a0177,'+
        '6 $feebe2 $fcc5c0 $fa9fb5 $f768a1 $c51b8a $7a0177,'+
        '7 $feebe2 $fcc5c0 $fa9fb5 $f768a1 $dd3497 $ae017e $7a0177,'+
        '8 $fff7f3 $fde0dd $fcc5c0 $fa9fb5 $f768a1 $dd3497 $ae017e $7a0177,'+
        '9 $fff7f3 $fde0dd $fcc5c0 $fa9fb5 $f768a1 $dd3497 $ae017e $7a0177 $49006a;'+
      'Sequential'
    );
    // brewer 'RdYlBu'
    addEx( 'RedYellowBlue;'+
      '0 $FFd7191c,'+
      '25 $FFfdae61,'+
      '50 $FFffffbf,'+
      '75 $FFabd9e9,'+
      '100 $FF2c7bb6;'+
        '3 $fc8d59 $ffffbf $91bfdb,'+
        '4 $d7191c $fdae61 $abd9e9 $2c7bb6,'+
        '5 $d7191c $fdae61 $ffffbf $abd9e9 $2c7bb6,'+
        '6 $d73027 $fc8d59 $fee090 $e0f3f8 $91bfdb $4575b4,'+
        '7 $d73027 $fc8d59 $fee090 $ffffbf $e0f3f8 $91bfdb $4575b4,'+
        '8 $d73027 $f46d43 $fdae61 $fee090 $e0f3f8 $abd9e9 $74add1 $4575b4,'+
        '9 $d73027 $f46d43 $fdae61 $fee090 $ffffbf $e0f3f8 $abd9e9 $74add1 $4575b4,'+
        '10 $a50026 $d73027 $f46d43 $fdae61 $fee090 $e0f3f8 $abd9e9 $74add1 $4575b4 $313695,'+
        '11 $a50026 $d73027 $f46d43 $fdae61 $fee090 $ffffbf $e0f3f8 $abd9e9 $74add1 $4575b4 $313695;'+
      'Diverging'
    );
    // brewer 'RdYlGn'
    addEx( 'RedYellowGreen;'+
      '0 $FFd7191c,'+
      '25 $FFfdae61,'+
      '50 $FFffffbf,'+
      '75 $FFa6d96a,'+
      '100 $FF1a9641;'+
        '3 $fc8d59 $ffffbf $91cf60,'+
        '4 $d7191c $fdae61 $a6d96a $1a9641,'+
        '5 $d7191c $fdae61 $ffffbf $a6d96a $1a9641,'+
        '6 $d73027 $fc8d59 $fee08b $d9ef8b $91cf60 $1a9850,'+
        '7 $d73027 $fc8d59 $fee08b $ffffbf $d9ef8b $91cf60 $1a9850,'+
        '8 $d73027 $f46d43 $fdae61 $fee08b $d9ef8b $a6d96a $66bd63 $1a9850,'+
        '9 $d73027 $f46d43 $fdae61 $fee08b $ffffbf $d9ef8b $a6d96a $66bd63 $1a9850,'+
        '10 $a50026 $d73027 $f46d43 $fdae61 $fee08b $d9ef8b $a6d96a $66bd63 $1a9850 $006837,'+
        '11 $a50026 $d73027 $f46d43 $fdae61 $fee08b $ffffbf $d9ef8b $a6d96a $66bd63 $1a9850 $006837;'+
      'Diverging'
    );
    // surfer 'RedHot2'
    addEx( 'RedHot;'+
      '0 $FFFFFF00,'+
      '100 $FFFF0000;;'+
      'Sequential'
    );
    addEx( 'Reds;'+
      '0 $FFfee5d9,'+
      '25 $FFfcae91,'+
      '50 $FFfb6a4a,'+
      '75 $FFde2d26,'+
      '100 $FFa50f15;'+
        '3 $fee0d2 $fc9272 $de2d26,'+
        '4 $fee5d9 $fcae91 $fb6a4a $cb181d,'+
        '5 $fee5d9 $fcae91 $fb6a4a $de2d26 $a50f15,'+
        '6 $fee5d9 $fcbba1 $fc9272 $fb6a4a $de2d26 $a50f15,'+
        '7 $fee5d9 $fcbba1 $fc9272 $fb6a4a $ef3b2c $cb181d $99000d,'+
        '8 $fff5f0 $fee0d2 $fcbba1 $fc9272 $fb6a4a $ef3b2c $cb181d $99000d,'+
        '9 $fff5f0 $fee0d2 $fcbba1 $fc9272 $fb6a4a $ef3b2c $cb181d $a50f15 $67000d;'+
      'Sequential'
    );
    addEx( 'Rocket;'+
      '0 $ff07071d,'+
      '2 $ff0e0b22,'+
      '4 $ff160e27,'+
      '6 $ff1d112c,'+
      '8 $ff241432,'+
      '10 $ff2b1637,'+
      '12 $ff33183c,'+
      '14 $ff3a1a41,'+
      '16 $ff421b45,'+
      '18 $ff491d49,'+
      '20 $ff511e4d,'+
      '22 $ff591e50,'+
      '24 $ff611f53,'+
      '26 $ff681f55,'+
      '28 $ff701f57,'+
      '30 $ff781f59,'+
      '32 $ff811e5a,'+
      '34 $ff891e5b,'+
      '36 $ff921c5b,'+
      '38 $ff9a1b5b,'+
      '40 $ffa3195b,'+
      '42 $ffab185a,'+
      '44 $ffb41658,'+
      '46 $ffbc1656,'+
      '48 $ffc41753,'+
      '50 $ffcb1b4f,'+
      '52 $ffd11f4c,'+
      '54 $ffd72549,'+
      '56 $ffdd2c45,'+
      '58 $ffe23442,'+
      '60 $ffe73d3f,'+
      '62 $ffeb463e,'+
      '64 $ffed503e,'+
      '66 $ffef5a41,'+
      '68 $fff16445,'+
      '70 $fff26d4b,'+
      '72 $fff37651,'+
      '74 $fff47f58,'+
      '76 $fff58860,'+
      '78 $fff58f66,'+
      '80 $fff5976e,'+
      '82 $fff6a077,'+
      '84 $fff6a880,'+
      '86 $fff6b089,'+
      '88 $fff6b893,'+
      '90 $fff6bf9d,'+
      '92 $fff7c7a8,'+
      '94 $fff7cfb3,'+
      '96 $fff8d6be,'+
      '98 $fff9ddc9,'+
      '100 $fff9e5d4;;'+
      'Sequential'
    );
    // slightly modified surfer 'Sea2'
    addEx( 'Sea;'+
      '0 $FF084081,'+
      '12.50 $FF0868AC,'+
      '25 $FF3690C0,'+
      '37.50 $FF4FB3D3,'+
      '50 $FF7BCCC4,'+
      '62.50 $FFA8DDB5,'+
      '75 $FFCCEBC5,'+
      '87.50 $FFE0F3DB,'+
      '100 $FFF7FCF0;;'+
      'Sequential'
    );
    // original name 'GMTSealand'
    addEx( 'Sealand;'+
      '0 $FF8C66FF,'+
      '5.56 $FF6666FF,'+
      '11.11 $FF668CFF,'+
      '16.67 $FF66B2FF,'+
      '22.22 $FF66D9FF,'+
      '27.78 $FF66FFFF,'+
      '33.33 $FF66FFD9,'+
      '38.89 $FF66FFB2,'+
      '44.44 $FF66FF8C,'+
      '50 $FF66FF66,'+
      '55.56 $FF8CFF66,'+
      '61.11 $FFB2FF66,'+
      '66.67 $FFD9FF66,'+
      '66.67 $FFFFFFA6,'+
      '72.22 $FFFFE1A6,'+
      '77.78 $FFFFC3A6,'+
      '83.33 $FFFFA6A6,'+
      '88.89 $FFFFB2C6,'+
      '94.44 $FFFFBFDF,'+
      '100 $FFFFCCF2;;'+
      'Miscellaneous'
    );
    addEx( 'Seismic;'+
      '0 $FFFF2929,'+
      '16.03 $FFFF6767,'+
      '29.62 $FFFFA1A1,'+
      '36.34 $FFFFBFBF,'+
      '40.76 $FFFFD4D4,'+
      '44.21 $FFFFE5E5,'+
      '47.19 $FFFFF4F4,'+
      '49.94 $FFFFFFFF,'+
      '51.32 $FFF5F5FF,'+
      '54.19 $FFE6E6FF,'+
      '57.38 $FFD5D5FF,'+
      '61.24 $FFC0C0FF,'+
      '66.58 $FFA1A1FF,'+
      '75.85 $FF6666FF,'+
      '100 $FF2828FF;;'+
      'Diverging'
    );
    addEx( 'Set1;'+
      '0 $FFe41a1c,'+
      '25 $FF377eb8,'+
      '50 $FF4daf4a,'+
      '75 $FF984ea3,'+
      '100 $FFff7f00;'+
        '3 $e41a1c $377eb8 $4daf4a,'+
        '4 $e41a1c $377eb8 $4daf4a $984ea3,'+
        '5 $e41a1c $377eb8 $4daf4a $984ea3 $ff7f00,'+
        '6 $e41a1c $377eb8 $4daf4a $984ea3 $ff7f00 $ffff33,'+
        '7 $e41a1c $377eb8 $4daf4a $984ea3 $ff7f00 $ffff33 $a65628,'+
        '8 $e41a1c $377eb8 $4daf4a $984ea3 $ff7f00 $ffff33 $a65628 $f781bf,'+
        '9 $e41a1c $377eb8 $4daf4a $984ea3 $ff7f00 $ffff33 $a65628 $f781bf $999999;'+
      'Qualitative'
    );
    addEx( 'Set2;'+
      '0 $FF66c2a5,'+
      '25 $FFfc8d62,'+
      '50 $FF8da0cb,'+
      '75 $FFe78ac3,'+
      '100 $FFa6d854;'+
        '3 $66c2a5 $fc8d62 $8da0cb,'+
        '4 $66c2a5 $fc8d62 $8da0cb $e78ac3,'+
        '5 $66c2a5 $fc8d62 $8da0cb $e78ac3 $a6d854,'+
        '6 $66c2a5 $fc8d62 $8da0cb $e78ac3 $a6d854 $ffd92f,'+
        '7 $66c2a5 $fc8d62 $8da0cb $e78ac3 $a6d854 $ffd92f $e5c494,'+
        '8 $66c2a5 $fc8d62 $8da0cb $e78ac3 $a6d854 $ffd92f $e5c494 $b3b3b3;'+
      'Qualitative'
    );
    addEx( 'Set3;'+
      '0 $FF8dd3c7,'+
      '25 $FFffffb3,'+
      '50 $FFbebada,'+
      '75 $FFfb8072,'+
      '100 $FF80b1d3;'+
        '3 $8dd3c7 $ffffb3 $bebada,'+
        '4 $8dd3c7 $ffffb3 $bebada $fb8072,'+
        '5 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3,'+
        '6 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462,'+
        '7 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462 $b3de69,'+
        '8 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462 $b3de69 $fccde5,'+
        '9 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462 $b3de69 $fccde5 $d9d9d9,'+
        '10 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462 $b3de69 $fccde5 $d9d9d9 $bc80bd,'+
        '11 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462 $b3de69 $fccde5 $d9d9d9 $bc80bd $ccebc5,'+
        '12 $8dd3c7 $ffffb3 $bebada $fb8072 $80b1d3 $fdb462 $b3de69 $fccde5 $d9d9d9 $bc80bd $ccebc5 $ffed6f;'+
      'Qualitative'
    );
    addEx( 'Soil;'+
      '0 $FF000000,'+
      '20 $FF0000FF,'+
      '40 $FFFF0000,'+
      '60 $FF00FF00,'+
      '80 $FFFFFF00,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'Spectral;'+
      '0 $FFd7191c,'+
      '25 $FFfdae61,'+
      '50 $FFffffbf,'+
      '75 $FFabdda4,'+
      '100 $FF2b83ba;'+
        '3 $fc8d59 $ffffbf $99d594,'+
        '4 $d7191c $fdae61 $abdda4 $2b83ba,'+
        '5 $d7191c $fdae61 $ffffbf $abdda4 $2b83ba,'+
        '6 $d53e4f $fc8d59 $fee08b $e6f598 $99d594 $3288bd,'+
        '7 $d53e4f $fc8d59 $fee08b $ffffbf $e6f598 $99d594 $3288bd,'+
        '8 $d53e4f $f46d43 $fdae61 $fee08b $e6f598 $abdda4 $66c2a5 $3288bd,'+
        '9 $d53e4f $f46d43 $fdae61 $fee08b $ffffbf $e6f598 $abdda4 $66c2a5 $3288bd,'+
        '10 $9e0142 $d53e4f $f46d43 $fdae61 $fee08b $e6f598 $abdda4 $66c2a5 $3288bd $5e4fa2,'+
        '11 $9e0142 $d53e4f $f46d43 $fdae61 $fee08b $ffffbf $e6f598 $abdda4 $66c2a5 $3288bd $5e4fa2;'+
      'Diverging'
    );

    // slightly modified
    addEx( 'Spice;'+
      '0 $FF7F0019,'+
      '12.50 $FFB2000D,'+
      '25 $FFD7301F,'+
      '37.50 $FFEF6548,'+
      '50 $FFFC8D59,'+
      '62.50 $FFFDBB84,'+
      '75 $FFFDD49E,'+
      '87.50 $FFFEE8C8,'+
      '100 $FFFFF7EC;;'+
      'Sequential'
    );
    addEx( 'Spring;'+
      '0 $ffff00ff,'+
      '5.26 $ffff0df2,'+
      '10.53 $ffff1ae5,'+
      '15.79 $ffff28d7,'+
      '21.05 $ffff35ca,'+
      '26.32 $ffff43bc,'+
      '31.58 $ffff50af,'+
      '36.84 $ffff5ea1,'+
      '42.11 $ffff6b94,'+
      '47.37 $ffff7986,'+
      '52.63 $ffff8679,'+
      '57.89 $ffff946b,'+
      '63.16 $ffffa15e,'+
      '68.42 $ffffaf50,'+
      '73.68 $ffffbc43,'+
      '78.95 $ffffca35,'+
      '84.21 $ffffd728,'+
      '89.47 $ffffe51a,'+
      '94.74 $fffff20d,'+
      '100 $ffffff00;;'+
      'Sequential'
    );
    // original name 'GistStern'
    addEx( 'Stern;'+
      '0 $ff000000,'+
      '5.26 $ffee0d1a,'+
      '10.53 $ffc31a34,'+
      '15.79 $ff7d2850,'+
      '21.05 $ff3c356a,'+
      '26.32 $ff434386,'+
      '31.58 $ff5050a0,'+
      '36.84 $ff5e5ebc,'+
      '42.11 $ff6b6bd6,'+
      '47.37 $ff7979f2,'+
      '52.63 $ff8686e3,'+
      '57.89 $ff9494a8,'+
      '63.16 $ffa1a170,'+
      '68.42 $ffafaf35,'+
      '73.68 $ffbcbc02,'+
      '78.95 $ffcaca37,'+
      '84.21 $ffd7d768,'+
      '89.47 $ffe5e59d,'+
      '94.74 $fff2f2ce,'+
      '100 $ffffffff;;'+
      'Miscellaneous'
    );
    addEx( 'Summer;'+
      '0 $ff008066,'+
      '5.26 $ff0d8666,'+
      '10.53 $ff1a8c66,'+
      '15.79 $ff289366,'+
      '21.05 $ff359a66,'+
      '26.32 $ff43a166,'+
      '31.58 $ff50a866,'+
      '36.84 $ff5eae66,'+
      '42.11 $ff6bb566,'+
      '47.37 $ff79bc66,'+
      '52.63 $ff86c266,'+
      '57.89 $ff94ca66,'+
      '63.16 $ffa1d066,'+
      '68.42 $ffafd766,'+
      '73.68 $ffbcde66,'+
      '78.95 $ffcae466,'+
      '84.21 $ffd7eb66,'+
      '89.47 $ffe5f266,'+
      '94.74 $fff2f866,'+
      '100 $ffffff66;;'+
      'Sequential'
    );
    addEx( 'Sunset;'+
      '0 $FF364B9A,'+
      '8.85 $FF4A7BB7,'+
      '20.37 $FFFFAA00,'+
      '33.39 $FFD95559,'+
      '53.26 $FFB200B2,'+
      '77.77 $FF5900C3,'+
      '100 $FF0000D4;;'+
      'Miscellaneous'
    );
  end;

  procedure fill_5;
  begin
    addEx( 'Tab10;'+
      '0 $ff1f77b4,'+
      '14.29 $ffff7f0e,'+
      '28.57 $ff2ca02c,'+
      '42.86 $ff9467bd,'+
      '57.14 $ff8c564b,'+
      '71.43 $ff7f7f7f,'+
      '85.71 $ffbcbd22,'+
      '100 $ff17becf;;'+
      'Qualitative'
    );
    addEx( 'Tab20;'+
      '0 $ff1f77b4,'+
      '14.29 $ffff7f0e,'+
      '28.57 $ff98df8a,'+
      '42.86 $ff9467bd,'+
      '57.14 $ffc49c94,'+
      '71.43 $ff7f7f7f,'+
      '85.71 $ffdbdb8d,'+
      '100 $ff9edae5;;'+
      'Qualitative'
    );
    addEx( 'Tab20b;'+
      '0 $ff393b79,'+
      '14.29 $ff6b6ecf,'+
      '28.57 $ff8ca252,'+
      '42.86 $ff8c6d31,'+
      '57.14 $ffe7cb94,'+
      '71.43 $ffd6616b,'+
      '85.71 $ffa55194,'+
      '100 $ffde9ed6;;'+
      'Qualitative'
    );
    addEx( 'Tab20c;'+
      '0 $ff3182bd,'+
      '14.29 $ff9ecae1,'+
      '28.57 $fffd8d3c,'+
      '42.86 $ff31a354,'+
      '57.14 $ffc7e9c0,'+
      '71.43 $ffbcbddc,'+
      '85.71 $ff969696,'+
      '100 $ffd9d9d9;;'+
      'Qualitative'
    );
    // slightly modified
    addEx( 'Temperature;'+
      '0 $FF1316B4,'+
      '4.54 $FF2331C7,'+
      '9.09 $FF2D42C9,'+
      '13.64 $FF3755CB,'+
      '18.18 $FF365FC6,'+
      '22.73 $FF466FCF,'+
      '27.27 $FF507DD2,'+
      '31.82 $FF598DD6,'+
      '36.36 $FF629BD9,'+
      '40.91 $FF7EB9E9,'+
      '45.45 $FFA5D7FF,'+
      '50 $FFC4E5B7,'+
      '54.54 $FFB4DFA8,'+
      '59.09 $FFB0D793,'+
      '63.64 $FFC7CF74,'+
      '68.18 $FFDBC85B,'+
      '72.73 $FFDEBD50,'+
      '77.27 $FFD9A449,'+
      '81.82 $FFD39242,'+
      '86.36 $FFD1853E,'+
      '90.91 $FFCC7139,'+
      '95.45 $FFCA6232,'+
      '100 $FFC74528;;'+
      'Sequential'
    );
    addEx( 'Terrain;'+
      '0 $FFC8D785,'+
      '5 $FFABD9B1,'+
      '10 $FF7CC478,'+
      '15 $FF75C178,'+
      '20 $FFAFCCA6,'+
      '25 $FFDBD04E,'+
      '30 $FFF1CF0E,'+
      '35 $FFF2A700,'+
      '40 $FFC09F0D,'+
      '45 $FFD3C070,'+
      '50 $FFF0DBA1,'+
      '55 $FFFCECC0,'+
      '60 $FFF8FAEA,'+
      '65 $FFE5FEFA,'+
      '70 $FFDBFFFD,'+
      '75 $FFD6FBFC,'+
      '80 $FFB7F4F7,'+
      '85 $FF73E0F1,'+
      '90 $FF1DBCEF,'+
      '95 $FF0089F5,'+
      '100 $FF0050FA;;'+
      'Miscellaneous'
    );
    // original name 'ETOPO'
    addEx( 'Topo1;'+
      '0 $FF0A0079,'+
      '2.56 $FF1A0089,'+
      '5.13 $FF260098,'+
      '7.69 $FF1B03A6,'+
      '10.26 $FF1006B4,'+
      '12.82 $FF0509C1,'+
      '15.38 $FF000ECB,'+
      '17.95 $FF0016D2,'+
      '20.51 $FF001ED8,'+
      '23.08 $FF0027DF,'+
      '25.64 $FF0C44E7,'+
      '28.21 $FF1A66F0,'+
      '30.77 $FF1375F4,'+
      '33.33 $FF0E85F9,'+
      '35.90 $FF159EFC,'+
      '38.46 $FF1EB2FF,'+
      '41.03 $FF2BBAFF,'+
      '43.59 $FF37C1FF,'+
      '46.15 $FF41C8FF,'+
      '48.72 $FF4FD2FF,'+
      '51.28 $FF5EDFFF,'+
      '53.85 $FF8AE3FF,'+
      '56.41 $FFBCE6FF,'+
      '56.41 $FF336600,'+
      '56.92 $FF33CC66,'+
      '57.44 $FFBBE492,'+
      '58.97 $FFFFDCB9,'+
      '61.54 $FFF3CA89,'+
      '64.10 $FFE6B858,'+
      '66.67 $FFD9A627,'+
      '69.23 $FFA89A1F,'+
      '71.79 $FFA49019,'+
      '74.36 $FFA28613,'+
      '76.92 $FF9F7B0D,'+
      '79.49 $FF9C7107,'+
      '82.05 $FF996600,'+
      '84.62 $FFA25959,'+
      '87.18 $FFB27676,'+
      '89.74 $FFB79393,'+
      '92.31 $FFC2B0B0,'+
      '94.87 $FFCCCCCC,'+
      '97.44 $FFE5E5E5,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    // original name 'GMTTopo'
    addEx( 'Topo2;'+
      '0 $FFC877D9,'+
      '3.57 $FFA077D9,'+
      '3.57 $FFB08AE6,'+
      '7.14 $FF8A8AE6,'+
      '10.71 $FF8AA8E6,'+
      '14.29 $FF8AC8E6,'+
      '17.86 $FF91F2EA,'+
      '21.43 $FF85F2BC,'+
      '25 $FF85F28E,'+
      '28.57 $FFABF285,'+
      '32.14 $FFD7F285,'+
      '35.71 $FFF2E085,'+
      '39.29 $FFF2B385,'+
      '46.43 $FFD99A8D,'+
      '46.43 $FFD9A3A3,'+
      '50 $FFCC9999,'+
      '50 $FF74A3B2,'+
      '51.43 $FF6BB29B,'+
      '52.86 $FF62B269,'+
      '54.29 $FF90CC70,'+
      '57.14 $FFB5CC70,'+
      '60.71 $FFE6D895,'+
      '75 $FFFFF0E6,'+
      '75 $FFFFF8F2,'+
      '100 $FFFFFFFF;;'+
      'Miscellaneous'
    );
    addEx( 'Turbo;'+
      '0 $ff30123b,'+
      '2 $ff36215f,'+
      '4 $ff3b2f80,'+
      '6 $ff3f3e9c,'+
      '8 $ff424bb5,'+
      '10 $ff4559cb,'+
      '12 $ff4666dd,'+
      '14 $ff4773eb,'+
      '16 $ff4680f6,'+
      '18 $ff448ffe,'+
      '20 $ff3e9bfe,'+
      '22 $ff37a8fa,'+
      '24 $ff2eb4f2,'+
      '26 $ff25c0e7,'+
      '28 $ff1ecbda,'+
      '30 $ff19d5cd,'+
      '32 $ff18dec0,'+
      '34 $ff1de7b2,'+
      '36 $ff27eea4,'+
      '38 $ff35f394,'+
      '40 $ff46f884,'+
      '42 $ff59fb73,'+
      '44 $ff6dfe62,'+
      '46 $ff80ff53,'+
      '48 $ff92ff47,'+
      '50 $ffa4fc3c,'+
      '52 $ffb1f936,'+
      '54 $ffbef434,'+
      '56 $ffcbed34,'+
      '58 $ffd7e535,'+
      '60 $ffe1dd37,'+
      '62 $ffebd339,'+
      '64 $fff2c93a,'+
      '66 $fff8be39,'+
      '68 $fffcb136,'+
      '70 $fffea431,'+
      '72 $fffe962b,'+
      '74 $fffc8725,'+
      '76 $fff9781e,'+
      '78 $fff56918,'+
      '80 $fff05b12,'+
      '82 $ffea4e0d,'+
      '84 $ffe14109,'+
      '86 $ffd83706,'+
      '88 $ffce2d04,'+
      '90 $ffc32503,'+
      '92 $ffb71d02,'+
      '94 $ffa91601,'+
      '96 $ff9b0f01,'+
      '98 $ff8b0902,'+
      '100 $ff7a0403;;'+
      'Sequential'
    );
    addEx( 'Twilight;'+
      '0 $ffdcd9e0,'+
      '2 $ffd3d6db,'+
      '4 $ffc7d0d5,'+
      '6 $ffb8c9d0,'+
      '8 $ffa9c1cb,'+
      '10 $ff9cb9c8,'+
      '12 $ff8eb1c5,'+
      '14 $ff82a7c3,'+
      '16 $ff789ec2,'+
      '18 $ff7094c0,'+
      '20 $ff6a8bbf,'+
      '22 $ff6580bd,'+
      '24 $ff6276ba,'+
      '26 $ff606ab7,'+
      '28 $ff5f5fb3,'+
      '30 $ff5e54ae,'+
      '32 $ff5e48a8,'+
      '34 $ff5d3ca0,'+
      '36 $ff5b3095,'+
      '38 $ff572588,'+
      '40 $ff531d7a,'+
      '42 $ff4c1669,'+
      '44 $ff431359,'+
      '46 $ff3b114a,'+
      '48 $ff34113f,'+
      '50 $ff2f1436,'+
      '52 $ff351138,'+
      '54 $ff3d113c,'+
      '56 $ff481341,'+
      '58 $ff541546,'+
      '60 $ff61184b,'+
      '62 $ff6d1b4e,'+
      '64 $ff7a2050,'+
      '66 $ff862750,'+
      '68 $ff912f50,'+
      '70 $ff9b3850,'+
      '72 $ffa34150,'+
      '74 $ffab4b50,'+
      '76 $ffb25652,'+
      '78 $ffb86255,'+
      '80 $ffbd6e5a,'+
      '82 $ffc17960,'+
      '84 $ffc58569,'+
      '86 $ffc89174,'+
      '88 $ffcb9d82,'+
      '90 $ffcea991,'+
      '92 $ffd1b4a0,'+
      '94 $ffd5bfb1,'+
      '96 $ffd9c9c2,'+
      '98 $ffddd1d1,'+
      '100 $ffe0d7db;;'+
      'Diverging'
    );
    addEx( 'TwilightShifted;'+
      '0 $ff34113f,'+
      '2 $ff3b114a,'+
      '4 $ff431359,'+
      '6 $ff4c1669,'+
      '8 $ff531d7a,'+
      '10 $ff572588,'+
      '12 $ff5b3095,'+
      '14 $ff5d3ca0,'+
      '16 $ff5e48a8,'+
      '18 $ff5e54ae,'+
      '20 $ff5f5fb3,'+
      '22 $ff606ab7,'+
      '24 $ff6276ba,'+
      '26 $ff6580bd,'+
      '28 $ff6a8bbf,'+
      '30 $ff7094c0,'+
      '32 $ff789ec2,'+
      '34 $ff82a7c3,'+
      '36 $ff8eb1c5,'+
      '38 $ff9cb9c8,'+
      '40 $ffa9c1cb,'+
      '42 $ffb8c9d0,'+
      '44 $ffc7d0d5,'+
      '46 $ffd3d6db,'+
      '48 $ffdcd9e0,'+
      '50 $ffe2d9e2,'+
      '52 $ffe0d7db,'+
      '54 $ffddd1d1,'+
      '56 $ffd9c9c2,'+
      '58 $ffd5bfb1,'+
      '60 $ffd1b4a0,'+
      '62 $ffcea991,'+
      '64 $ffcb9d82,'+
      '66 $ffc89174,'+
      '68 $ffc58569,'+
      '70 $ffc17960,'+
      '72 $ffbd6e5a,'+
      '74 $ffb86255,'+
      '76 $ffb25652,'+
      '78 $ffab4b50,'+
      '80 $ffa34150,'+
      '82 $ff9b3850,'+
      '84 $ff912f50,'+
      '86 $ff862750,'+
      '88 $ff7a2050,'+
      '90 $ff6d1b4e,'+
      '92 $ff61184b,'+
      '94 $ff541546,'+
      '96 $ff481341,'+
      '98 $ff3d113c,'+
      '100 $ff351138;;'+
      'Diverging'
    );
    addEx( 'Winter;'+
      '0 $ff0000ff,'+
      '5.26 $ff000df8,'+
      '10.53 $ff001af2,'+
      '15.79 $ff0028eb,'+
      '21.05 $ff0035e4,'+
      '26.32 $ff0043de,'+
      '31.58 $ff0050d7,'+
      '36.84 $ff005ed0,'+
      '42.11 $ff006bca,'+
      '47.37 $ff0079c2,'+
      '52.63 $ff0086bc,'+
      '57.89 $ff0094b5,'+
      '63.16 $ff00a1ae,'+
      '68.42 $ff00afa8,'+
      '73.68 $ff00bca1,'+
      '78.95 $ff00ca9a,'+
      '84.21 $ff00d793,'+
      '89.47 $ff00e58c,'+
      '94.74 $ff00f286,'+
      '100 $ff00ff80;;'+
      'Sequential'
    );
    addEx( 'Wistia;'+
      '0 $ffe4ff7a,'+
      '5.26 $ffeafa66,'+
      '10.53 $ffeff653,'+
      '15.79 $fff5f13e,'+
      '21.05 $fffaec2a,'+
      '26.32 $ffffe619,'+
      '31.58 $ffffdd13,'+
      '36.84 $ffffd40e,'+
      '42.11 $ffffcb08,'+
      '47.37 $ffffc103,'+
      '52.63 $ffffba00,'+
      '57.89 $ffffb400,'+
      '63.16 $ffffae00,'+
      '68.42 $ffffa700,'+
      '73.68 $ffffa100,'+
      '78.95 $fffe9a00,'+
      '84.21 $fffe9400,'+
      '89.47 $fffd8c00,'+
      '94.74 $fffd8600,'+
      '100 $fffc7f00;;'+
      'Sequential'
    );
    addEx( 'Viridis;'+
      '0 $ff440154,'+
      '2 $ff46085c,'+
      '4 $ff471063,'+
      '6 $ff481769,'+
      '8 $ff481d6f,'+
      '10 $ff482475,'+
      '12 $ff472a7a,'+
      '14 $ff46307e,'+
      '16 $ff453781,'+
      '18 $ff433e85,'+
      '20 $ff414487,'+
      '22 $ff3e4989,'+
      '24 $ff3c4f8a,'+
      '26 $ff3a548c,'+
      '28 $ff375a8c,'+
      '30 $ff355f8d,'+
      '32 $ff32648e,'+
      '34 $ff306a8e,'+
      '36 $ff2e6f8e,'+
      '38 $ff2c738e,'+
      '40 $ff2a788e,'+
      '42 $ff287d8e,'+
      '44 $ff26828e,'+
      '46 $ff24868e,'+
      '48 $ff228b8d,'+
      '50 $ff21918c,'+
      '52 $ff1f958b,'+
      '54 $ff1f9a8a,'+
      '56 $ff1f9f88,'+
      '58 $ff20a386,'+
      '60 $ff22a884,'+
      '62 $ff26ad81,'+
      '64 $ff2cb17e,'+
      '66 $ff32b67a,'+
      '68 $ff3bbb75,'+
      '70 $ff44bf70,'+
      '72 $ff4ec36b,'+
      '74 $ff58c765,'+
      '76 $ff63cb5f,'+
      '78 $ff6ece58,'+
      '80 $ff7ad151,'+
      '82 $ff86d549,'+
      '84 $ff95d840,'+
      '86 $ffa2da37,'+
      '88 $ffb0dd2f,'+
      '90 $ffbddf26,'+
      '92 $ffcae11f,'+
      '94 $ffd8e219,'+
      '96 $ffe5e419,'+
      '98 $fff1e51d,'+
      '100 $fffde725;;'+
      'Sequential'
    );
    // brewer 'YlGn'
    addEx( 'YellowGreen;'+
      '0 $FFffffcc,'+
      '25 $FFc2e699,'+
      '50 $FF78c679,'+
      '75 $FF31a354,'+
      '100 $FF006837;'+
        '3 $f7fcb9 $addd8e $31a354,'+
        '4 $ffffcc $c2e699 $78c679 $238443,'+
        '5 $ffffcc $c2e699 $78c679 $31a354 $006837,'+
        '6 $ffffcc $d9f0a3 $addd8e $78c679 $31a354 $006837,'+
        '7 $ffffcc $d9f0a3 $addd8e $78c679 $41ab5d $238443 $005a32,'+
        '8 $ffffe5 $f7fcb9 $d9f0a3 $addd8e $78c679 $41ab5d $238443 $005a32,'+
        '9 $ffffe5 $f7fcb9 $d9f0a3 $addd8e $78c679 $41ab5d $238443 $006837 $004529;'+
      'Sequential'
    );
    // brewer 'YlGnBu'
    addEx( 'YellowGreenBlue;'+
      '0 $FFffffcc,'+
      '25 $FFa1dab4,'+
      '50 $FF41b6c4,'+
      '75 $FF2c7fb8,'+
      '100 $FF253494;'+
        '3 $edf8b1 $7fcdbb $2c7fb8,'+
        '4 $ffffcc $a1dab4 $41b6c4 $225ea8,'+
        '5 $ffffcc $a1dab4 $41b6c4 $2c7fb8 $253494,'+
        '6 $ffffcc $c7e9b4 $7fcdbb $41b6c4 $2c7fb8 $253494,'+
        '7 $ffffcc $c7e9b4 $7fcdbb $41b6c4 $1d91c0 $225ea8 $0c2c84,'+
        '8 $ffffd9 $edf8b1 $c7e9b4 $7fcdbb $41b6c4 $1d91c0 $225ea8 $0c2c84,'+
        '9 $ffffd9 $edf8b1 $c7e9b4 $7fcdbb $41b6c4 $1d91c0 $225ea8 $253494 $081d58;'+
      'Sequential'
    );
    addEx( 'YellowHigh;'+
      '0 $FF85AED6,'+
      '18.556701 $FF202A81,'+
      '32.109419 $FF000066,'+
      '40.893471 $FFCC9933,'+
      '50.171822 $FF990000,'+
      '65.292096 $FFB61D00,'+
      '81.786942 $FFFF6600,'+
      '100 $FFFFFF00;;'+
      'Miscellaneous'
    );
    addEx( 'YellowJacket;'+
      '0 $FF666666,'+
      '50 $FFCCCCCC,'+
      '100 $FFFFFF00;;'+
      'Sequential'
    );
    // brewer 'YlOrBr'
    addEx( 'YellowOrangeBrown;'+
      '0 $FFffffd4,'+
      '25 $FFfed98e,'+
      '50 $FFfe9929,'+
      '75 $FFd95f0e,'+
      '100 $FF993404;'+
        '3 $fff7bc $fec44f $d95f0e,'+
        '4 $ffffd4 $fed98e $fe9929 $cc4c02,'+
        '5 $ffffd4 $fed98e $fe9929 $d95f0e $993404,'+
        '6 $ffffd4 $fee391 $fec44f $fe9929 $d95f0e $993404,'+
        '7 $ffffd4 $fee391 $fec44f $fe9929 $ec7014 $cc4c02 $8c2d04,'+
        '8 $ffffe5 $fff7bc $fee391 $fec44f $fe9929 $ec7014 $cc4c02 $8c2d04,'+
        '9 $ffffe5 $fff7bc $fee391 $fec44f $fe9929 $ec7014 $cc4c02 $993404 $662506;'+
      'Sequential'
    );
    // brewer 'YlOrRd'
    addEx( 'YellowOrangeRed;'+
      '0 $FFffffb2,'+
      '25 $FFffffb2,'+
      '50 $FFfd8d3c,'+
      '75 $FFf03b20,'+
      '100 $FFbd0026;'+
        '3 $ffeda0 $feb24c $f03b20,'+
        '4 $ffffb2 $fecc5c $fd8d3c $e31a1c,'+
        '5 $ffffb2 $fecc5c $fd8d3c $f03b20 $bd0026,'+
        '6 $ffffb2 $fed976 $feb24c $fd8d3c $f03b20 $bd0026,'+
        '7 $ffffb2 $fed976 $feb24c $fd8d3c $fc4e2a $e31a1c $b10026,'+
        '8 $ffffcc $ffeda0 $fed976 $feb24c $fd8d3c $fc4e2a $e31a1c $b10026,'+
        '9 $ffffcc $ffeda0 $fed976 $feb24c $fd8d3c $fc4e2a $e31a1c $bd0026 $800026;'+
      'Sequential'
    );
  end;

  procedure fill_unique;
  begin
    // add dynamic HSL color ramps
    // [HueMin, HueMax, SaturationMin, SaturationMax, LightnessMin, LightnessMax]
    //? alternatively (0, 360, 70, 90, 70, 90), but to close to pastel
    addExDynamicHSL('Unique', TGIS_ColorSchema.Qualitative, 0, 360, 70, 90, 70, 80);
    addExDynamicHSL('UniquePastel', TGIS_ColorSchema.Qualitative, 0, 360, 50, 100, 85, 90);
    addExDynamicHSL('UniqueDark', TGIS_ColorSchema.Qualitative, 0, 360, 20, 100, 15, 40);
    addExDynamicHSL('UniquePure', TGIS_ColorSchema.Qualitative, 0, 360, 100, 100, 50, 50);
  end;
  {$ENDREGION}

  begin
    LockThread ;
    try
      Clear ;

      fill_1 ;  // A-C
      fill_2 ;  // D-H
      fill_3 ;  // I-P
      fill_4 ;  // R-S
      fill_5 ;  // T-Z
      fill_unique ;
    finally
      UnlockThread ;
    end ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_EventManager'}
  constructor TGIS_BusyEventManager.Create(
    const _sender : TObject
  ) ;
  begin
    inherited Create ;

    FAborted := False ;
    FUseProgressThreshold := True ;
    FSender := _sender ;
    if assigned( FSender ) and ( FSender is TGIS_Layer ) then
      senderIsLayer := True
    else
      senderIsLayer := False ;

    // list of T_BusyEventStage
    stagesList := TObjectList< TObject >.Create ;

    initialize ;
  end;

  procedure TGIS_BusyEventManager.doDestroy ;
  begin
    FreeObject( stagesList ) ;
    inherited ;
  end;

  procedure TGIS_BusyEventManager.initialize ;
  begin
    stagesList.Clear ;
    currentStage := nil ;
  end;

  function TGIS_BusyEventManager.raiseBusyEvent(
    const _sender : TObject ;
    const _pos    : Integer ;
    const _end    : Integer
  ) : Boolean ;
  var
    abrt   : Boolean ;
    {$IFDEF OXYGENE}
      args : TGIS_BusyEventArgs ;
    {$ENDIF}
  begin
    Result := False ;

    if assigned( FOnBusy ) then begin
      abrt := False ;
      {$IFDEF OXYGENE}
        args := TGIS_BusyEventArgs.Create( _pos, _end, abrt ) ;
        FOnBusy( _sender, args ) ;
        Result := args.Abort ;
      {$ELSE}
        FOnBusy( _sender, _pos, _end, abrt ) ;
        Result := abrt ;
      {$ENDIF}
    end ;
  end;

  procedure TGIS_BusyEventManager.calcStagePositions(
    const _start_event : Boolean
  );
  var
    cum_val   : Double ;
    i         : Integer ;
    stage     : T_BusyEventStage ;
    iposition : Integer ;
  begin
    cum_val := 0.0 ;
    // cumulative sum
    for i := stagesList.Count - 1 downto 0 do begin
      stage := T_BusyEventStage( stagesList[i] ) ;
      cum_val := ( stage.PosValue + cum_val ) / stage.EndValue ;
      iposition := RoundS( 100 * cum_val ) ;

      if i = stagesList.Count - 1 then begin
        // 0 can be set only at starting event
        if _start_event then
          iposition := 0
        else if iposition = 0 then
          iposition := 1 ;
      end ;

      stage.Position := iposition ;
    end ;
  end ;

  function TGIS_BusyEventManager.fget_Max( const _stage : Integer ) : Integer ;
  begin
    Result := T_BusyEventStage( stagesList[_stage] ).Max ;
  end;

  function TGIS_BusyEventManager.fget_Sender( const _stage : Integer ) : TObject ;
  begin
    Result := T_BusyEventStage( stagesList[_stage] ).Sender ;
  end;

  function TGIS_BusyEventManager.fget_EndValue( const _stage : Integer ) : Int64 ;
  begin
    Result := T_BusyEventStage( stagesList[_stage] ).EndValue ;
  end;

  function TGIS_BusyEventManager.fget_Position( const _stage : Integer ) : Integer ;
  begin
    Result := T_BusyEventStage( stagesList[_stage] ).Position ;
  end;

  function TGIS_BusyEventManager.fget_Name( const _stage : Integer ) : String ;
  begin
    Result := T_BusyEventStage( stagesList[_stage] ).Name ;
  end;

  function TGIS_BusyEventManager.fget_EstimatedTimeLeft( const _stage : Integer ) : Int64 ;
  var
    current_etl : Int64 ;
    stage_etl   : Int64 ;
  begin
    current_etl := T_BusyEventStage( currentStage ).EstimatedTimeLeft ;
    stage_etl := T_BusyEventStage( stagesList[_stage] ).EstimatedTimeLeft ;

    if current_etl > stage_etl then
      Result := current_etl
    else
      Result := stage_etl ;
  end;

  function TGIS_BusyEventManager.fget_Count : Integer ;
  begin
    Result := stagesList.Count ;
  end;

  function TGIS_BusyEventManager.shouldRaiseLayerBusyEvent : Boolean ;
  begin
    Result := not assignedBusy and senderIsLayer ;
  end;

  function TGIS_BusyEventManager.StartEvent(
    const _name    : String ;
    const _end_val : Int64 ;
    const _sender  : TObject = nil
  ) : Boolean ;
  var
    event_stage  : T_BusyEventStage ;
  begin
    Result := False ;

    assignedBusy := assigned( FOnBusy ) ;
    if assignedBusy or shouldRaiseLayerBusyEvent then
      useBusy := True
    else begin
      useBusy := False ;
      exit ;
    end ;

    if Count = 0 then
      initialize ;

    if _sender = nil then
      currentStage := T_BusyEventStage.Create( _name, _end_val, FSender )
    else
      // external sender
      currentStage := T_BusyEventStage.Create( _name, _end_val, _sender ) ;

    stagesList.Add( currentStage ) ;
    calcStagePositions( True ) ;

    event_stage := T_BusyEventStage( stagesList[0] ) ;

    if ( Count = 1 ) and shouldRaiseLayerBusyEvent then
      TGIS_Layer( FSender ).RaiseBusyPrepare( Self, Name[0] )
    else
      Result := raiseBusyEvent( Self , event_stage.Position, event_stage.Max ) ;

    FAborted := FAborted or Result ;
  end;

  function TGIS_BusyEventManager.PushEvent( const _name : String ) : Boolean ;
  begin
    Result := False ;
    if not useBusy then
      exit ;
    T_BusyEventStage( currentStage ).Name := _name ;
    Result := PushEvent ;
  end;

  function TGIS_BusyEventManager.PushEvent : Boolean ;
  var
    event_stage  : T_BusyEventStage ;
    event_pos    : Integer ;
    event_max    : Integer ;
  begin
    Result := False ;
    if not useBusy then
      exit ;

    event_stage := T_BusyEventStage( currentStage ) ;
    event_stage.incrementPosValue ;

    if event_stage.shouldRaise or ( not UseProgressThreshold ) then begin
      calcStagePositions ;

      event_stage := T_BusyEventStage( stagesList[0] ) ;
      event_pos := event_stage.Position ;
      event_max := event_stage.Max ;

      if shouldRaiseLayerBusyEvent then
        Result := TGIS_Layer( FSender ).RaiseBusyShake( Self, event_pos, event_max )
      else
        Result := raiseBusyEvent( Self, event_pos, event_max ) ;
    end ;

    FAborted := FAborted or Result ;
  end;

  function TGIS_BusyEventManager.EndEvent : Boolean ;
  var
    event_stage  : T_BusyEventStage ;
    pos_val      : Integer ;
    end_val      : Integer ;
  begin
    Result := False ;
    if not useBusy then
      exit ;

    calcStagePositions ;

    event_stage := T_BusyEventStage( stagesList[Count-1] ) ;
    event_stage.Position := -1 ;
    event_stage.Max := -1 ;

    event_stage := T_BusyEventStage( stagesList[0] ) ;
    if event_stage.Max = -1 then begin
      pos_val := -1 ;
      end_val := -1 ;
    end
    else begin
      pos_val := event_stage.Position ;
      end_val := 100 ;
    end ;

    if ( Count = 1 ) and shouldRaiseLayerBusyEvent then
      TGIS_Layer( FSender ).RaiseBusyRelease( Self )
    else
      Result := raiseBusyEvent( Self, pos_val, end_val ) ;

    if ( Count >= 1 ) then
      stagesList.Delete( Count - 1 ) ;

    if ( Count >= 1 ) then begin
      currentStage := stagesList[Count - 1] ;

      T_BusyEventStage( currentStage ).incrementPosValue ;
    end
    else
      currentStage := nil ;

    FAborted := FAborted or Result ;
  end;
{$ENDREGION}

//==================================== END =====================================
end.
