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
  DBF access module.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileDBF ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileDBF"'}
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

{$IFNDEF OXYGENE}
  {$ALIGN OFF}
{$ENDIF}

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Variants,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoStreams ;
{$ENDIF}
{$IFDEF CLR}
  uses
    System.Text,
    System.Globalization,
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
  ///   Encapsulation of DBF file access.
  /// </summary>
  TGIS_FileDBF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_BufferedFileStream )

    private
      {$IFDEF DCC} [weak] {$ENDIF} FLayer : TGIS_LayerVector ;

    private
      writemode  : Boolean ;
      appendmode : Boolean ;
      fileHeader : TObject ;
    private
      bufIn  : TBytes; // place for 0 zeros on the end

    private
      lastUid       : TGIS_Uid            ;
      fetchedFields : TGIS_VariantList ;
      offsetFields  : array of Integer ;

    protected
      /// <summary>
      ///   True if skip end marker.
      /// </summary>    
      skipEndMarkerFlag : Boolean ;

      /// <summary>
      ///   Read number of records.
      /// </summary>
      /// <returns>
      ///    records count
      /// </returns>      
      function getRecordsCount : Integer ;
      
    protected

      /// <summary>
      ///   Destroy an instance; write termination marker if file is in
      ///   write-mode.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Create an instance based on given file name.
      /// </summary>
      /// <param name="_path">
      ///   path to a DBF file
      /// </param>
      /// <param name="_layer">
      ///   layer associated with the DBF file; used to handling filed names
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
      constructor Create  ( const _path    : String              ;
                            const _layer   : TGIS_LayerVector    ;
                            const _mode    : TGIS_StreamMode     ;
                            const _onread  : TGIS_ReadWriteEvent ;
                            const _onwrite : TGIS_ReadWriteEvent
                          ) ; reintroduce ;

      /// <summary>
      ///   Read file header.
      /// </summary>
      procedure ReadHeader  ; overload;

      /// <summary>
      ///   Read file header.
      /// </summary>
      /// <param name="_clearLayerFields">
      ///   If true, then assigned layer fields will be cleared.
      /// </param>
      procedure ReadHeader  ( const _clearLayerFields : Boolean
                            ) ; overload;

      /// <summary>
      ///   Write file header.
      /// </summary>
      procedure WriteHeader ;

      /// <summary>
      ///   Read field structure.
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_WRONGFIELD
      /// </exception>
      procedure ReadStructure ;

      /// <summary>
      ///   Copy DBF file records.
      /// </summary>
      /// <param name="_file">
      ///   DBF file handle
      /// </param>
      /// <param name="_layer">
      ///   layer handle to retrieve fields information
      /// </param>
      procedure CopyRecords ( const _file  : TGIS_Stream ;
                              const _layer : TGIS_LayerVector
                            );

      /// <summary>
      ///   Write field structure. If no date field exists then a field called
      ///   _DUMMY Char(1) will be created to guarantee a correct DBF structure
      ///   (each shape must have a DBF record counterpart.
      /// </summary>
      procedure WriteStructure ;

      /// <summary>
      ///   Copy header to another file.
      /// </summary>
      /// <param name="_file">
      ///   DBF file handle to copy header
      /// </param>
      procedure CopyHeader  ( const _file  : TGIS_FileDBF
                            );

      /// <summary>
      ///   Get field given by _field_name from _record_no. If not possible,
      ///   then empty String will be returned.
      /// </summary>
      /// <param name="_uid">
      ///   uid of record to be read
      /// </param>
      /// <param name="_field">
      ///   name of field to be read
      /// </param>
      /// <param name="_id">
      ///   field id (if _name = '' )
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_WRONGFIELD
      /// </exception>
      /// <returns>
      ///    value
      /// </returns>      
      function  GetField    ( const _uid   : TGIS_Uid;
                              const _field : String ;
                              const _id    : Integer
                            ) : Variant ;

      /// <summary>
      ///   Add a new record to the database.
      /// </summary>
      /// <param name="_shape">
      ///   shape handle to get records from.
      /// </param>
      procedure AddRecord   ( const _shape : TGIS_Shape
                            ) ;

      /// <summary>
      ///   Write DBF record.
      /// </summary>
      /// <param name="_file">
      ///   DBF file stream handle
      /// </param>
      /// <param name="_value">
      ///   value to write
      /// </param>
      /// <param name="_type">
      ///   value type
      /// </param>
      /// <param name="_size">
      ///   value size
      /// </param>
      /// <param name="_decimal">
      ///   decimal precision for value
      /// </param>
      /// <param name="_binary">
      ///   binary storage size (not 0 only for MapInfo DAT format)
      /// </param>
      procedure WriteRecord ( const _file    : TGIS_Stream    ;
                              const _value   : Variant        ;
                              const _type    : TGIS_FieldType ;
                              const _size    : Integer        ;
                              const _decimal : Integer        ;
                              const _binary  : Integer
                             );
    public

      /// <summary>
      ///   Number of records in DBF file.
      /// </summary>
      property RecordsCount   : Integer read  getRecordsCount ;

      /// <summary>
      ///   Skip writing end marker byte upon a file close.
      /// </summary>
      property SkipEndMarker  : Boolean read  skipEndMarkerFlag
                                        write skipEndMarkerFlag ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFunctions ;
{$ENDIF}

type
  { For internal use only.
    Used this way only because documenting tool hung-up
    up if record is inside class.
  }
  T_FileHeader = class
    public
      FileCode     : Byte     ;
      Year         : Byte     ;
      Month        : Byte     ;
      Day          : Byte     ;
      RecordsCount : Integer  ;
      HeaderSize   : SmallInt ;
      RecordSize   : SmallInt ;
      Reserved1    : SmallInt ;
      StatustTrans : Byte     ;
      Encrypted    : Byte     ;
      MultiUser    : TBytes   ; // 12 characters
      IndexByte    : Byte     ;
      CodePage     : Byte     ;
      Reserved2    : SmallInt ;
    public
      constructor Create ;
  end ;

  { Full description of field.
  }
  T_FieldDescriptor = class
    public
      Name          : TBytes    ; // 11 characters ;
      FieldType     : Byte      ;
      Displacement  : Integer   ;
      Width         : Byte      ;
      Decimal       : Byte      ;
      Reserved1     : SmallInt  ;
      WorkAreaID    : Byte      ;
      Reserved2     : TBytes    ; // 10 characters ;
      IndexByte     : Byte      ;
    public
      constructor Create ;
  end ;

  constructor T_FileHeader.Create ;
  var
    i : Integer ;
  begin
    inherited ;
    FileCode       := 0 ;
    Year           := 0 ;
    Month          := 0 ;
    Day            := 0 ;
    RecordsCount   := 0 ;
    HeaderSize     := 0 ;
    RecordSize     := 0 ;
    Reserved1      := 0 ;
    StatustTrans   := 0 ;
    Encrypted      := 0 ;
    SetLength( MultiUser, 12 ) ;
    for i := low( MultiUser ) to high( MultiUser ) do
      MultiUser[i] := 0 ;
    IndexByte      := 0 ;
    CodePage       := 0 ;
    Reserved2      := 0 ;
  end ;

  constructor T_FieldDescriptor.Create ;
  var
    i : Integer ;
  begin
    inherited ;
    SetLength( Name, 11 ) ;
    for i := low( Name ) to high( Name ) do
      Name[i]      := 0 ;
    FieldType      := 0 ;
    Displacement   := 0 ;
    Width          := 0 ;
    Decimal        := 0 ;
    Reserved1      := 0 ;
    WorkAreaID     := 0 ;
    SetLength( Reserved2, 10 ) ;
    for i := low( Reserved2 ) to high( Reserved2 ) do
      Reserved2[i] := 0 ;
    IndexByte      := 0 ;
  end ;

//==============================================================================
// TGIS_FileDBF
//==============================================================================

  constructor TGIS_FileDBF.Create( const _path    : String              ;
                                   const _layer   : TGIS_LayerVector    ;
                                   const _mode    : TGIS_StreamMode     ;
                                   const _onread  : TGIS_ReadWriteEvent ;
                                   const _onwrite : TGIS_ReadWriteEvent
                                 ) ;
  var
    i : Integer ;
  begin
    inherited Create( _path, _mode, _onread, _onwrite ) ;

    fileHeader := T_FileHeader.Create ;
    skipEndMarkerFlag := False ;

    if ( _mode = TGIS_StreamMode.&Create ) or
       ( _mode = TGIS_StreamMode.Append ) then begin
      writemode  := _mode = TGIS_StreamMode.&Create  ;
      appendmode := _mode = TGIS_StreamMode.Append ;
      FLayer := _layer ;
    end
    else begin
      writemode  := False ;
      appendmode := False ;
      FLayer := _layer ;

      ReadHeader;
      ReadStructure ;
    end ;

    fetchedFields := TGIS_VariantList.Create ;
    if assigned( FLayer ) then begin
      for i := 0 to FLayer.Fields.Count - 1 do
        fetchedFields.Add( Unassigned ) ;
    end ;

    {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( bufIn , 257 ) ;
    lastUid := -1 ;
  end ;

  procedure TGIS_FileDBF.doDestroy ;
  var
    trm : Byte ;
  begin
    FreeObject( fileHeader ) ;

    if (( writemode and not appendmode ) or ( not writemode and appendmode )) and
       not skipEndMarkerFlag then begin

      Seek( 0,  soEnd );
      trm := $1a ;

      {$IFNDEF OXYGENE}
        WriteBuffer( trm, sizeOf(trm) ) ;
      {$ELSE}
        WriteByte( trm ) ;
      {$ENDIF}
    end ;

    FreeObject( fetchedFields ) ;
    {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( offsetFields, 0 ) ;

    inherited
  end ;

  function TGIS_FileDBF.getRecordsCount : Integer ;
  begin
    Result := T_FileHeader(fileHeader).RecordsCount ;
  end ;

  procedure TGIS_FileDBF.ReadHeader ;
  begin
    ReadHeader( True ) ;
  end ;

  procedure TGIS_FileDBF.ReadHeader(
    const _clearLayerFields : Boolean
  ) ;
  begin
    if _clearLayerFields then
     if assigned( FLayer ) then begin
       FLayer.Fields.Clear ;
     end ;

    // Read not as a BufferToStruct but read all elements independently
    with T_FileHeader(fileHeader) do begin
      {$IFDEF OXYGENE}
        ReadByteBuffer    ( FileCode     , sizeOf( Byte      ) ) ;
        ReadByteBuffer    ( Year         , sizeOf( Byte      ) ) ;
        ReadByteBuffer    ( Month        , sizeOf( Byte      ) ) ;
        ReadByteBuffer    ( Day          , sizeOf( Byte      ) ) ;
        ReadIntegerBuffer ( RecordsCount , sizeOf( Integer   ) ) ;
        ReadSmallIntBuffer( HeaderSize   , sizeOf( SmallInt  ) ) ;
        ReadSmallIntBuffer( RecordSize   , sizeOf( SmallInt  ) ) ;
        ReadSmallIntBuffer( Reserved1    , sizeOf( SmallInt  ) ) ;
        ReadByteBuffer    ( StatustTrans , sizeOf( Byte      ) ) ;
        ReadByteBuffer    ( Encrypted    , sizeOf( Byte      ) ) ;
        {$IFDEF CLR}
          Read            ( MultiUser    , 12                  ) ;
        {$ELSE}
          ReadBytesCnt    ( MultiUser    , MultiUser.Length    ) ;
        {$ENDIF}
        ReadByteBuffer    ( IndexByte    , sizeOf( Byte      ) ) ;
        ReadByteBuffer    ( CodePage     , sizeOf( Byte      ) ) ;
        ReadSmallIntBuffer( Reserved2    , sizeOf( SmallInt  ) ) ;
      {$ELSE}
        ReadBuffer        ( FileCode     , sizeOf( Byte      ) ) ;
        ReadBuffer        ( Year         , sizeOf( Byte      ) ) ;
        ReadBuffer        ( Month        , sizeOf( Byte      ) ) ;
        ReadBuffer        ( Day          , sizeOf( Byte      ) ) ;
        ReadBuffer        ( RecordsCount , sizeOf( Integer   ) ) ;
        ReadBuffer        ( HeaderSize   , sizeOf( SmallInt  ) ) ;
        ReadBuffer        ( RecordSize   , sizeOf( SmallInt  ) ) ;
        ReadBuffer        ( Reserved1    , sizeOf( SmallInt  ) ) ;
        ReadBuffer        ( StatustTrans , sizeOf( Byte      ) ) ;
        ReadBuffer        ( Encrypted    , sizeOf( Byte      ) ) ;
        {$IFDEF CLR}
          Read            ( MultiUser    , 12                  ) ;
        {$ELSE}
          ReadBytesCnt    ( MultiUser    , length( MultiUser ) ) ;
        {$ENDIF}
        ReadBuffer        ( IndexByte    , sizeOf( Byte      ) ) ;
        ReadBuffer        ( CodePage     , sizeOf( Byte      ) ) ;
        ReadBuffer        ( Reserved2    , sizeOf( SmallInt  ) ) ;
      {$ENDIF}
    end ;

    if assigned( FLayer ) then begin
      case T_FileHeader(fileHeader).CodePage of
        $01 : FLayer.CodePage := 437   ;
        $02 : FLayer.CodePage := 850   ;
        $03 : FLayer.CodePage := 1252  ;
        $04 : FLayer.CodePage := 10000 ;
        $08 : FLayer.CodePage := 865   ;
        $0A : FLayer.CodePage := 850   ;
        $0B : FLayer.CodePage := 437   ;
        $0D : FLayer.CodePage := 437   ;
        $0E : FLayer.CodePage := 850   ;
        $0F : FLayer.CodePage := 437   ;
        $10 : FLayer.CodePage := 850   ;
        $11 : FLayer.CodePage := 437   ;
        $12 : FLayer.CodePage := 850   ;
        $13 : FLayer.CodePage := 932   ;
        $14 : FLayer.CodePage := 850   ;
        $15 : FLayer.CodePage := 437   ;
        $16 : FLayer.CodePage := 850   ;
        $17 : FLayer.CodePage := 865   ;
        $18 : FLayer.CodePage := 437   ;
        $19 : FLayer.CodePage := 437   ;
        $1A : FLayer.CodePage := 850   ;
        $1B : FLayer.CodePage := 437   ;
        $1C : FLayer.CodePage := 863   ;
        $1D : FLayer.CodePage := 850   ;
        $1F : FLayer.CodePage := 852   ;
        $22 : FLayer.CodePage := 852   ;
        $23 : FLayer.CodePage := 852   ;
        $24 : FLayer.CodePage := 860   ;
        $25 : FLayer.CodePage := 850   ;
        $26 : FLayer.CodePage := 866   ;
        $37 : FLayer.CodePage := 850   ;
        $40 : FLayer.CodePage := 852   ;
        $4D : FLayer.CodePage := 936   ;
        $4E : FLayer.CodePage := 949   ;
        $4F : FLayer.CodePage := 950   ;
        $50 : FLayer.CodePage := 874   ;
        $57 : FLayer.CodePage := 28591 ;
        $58 : FLayer.CodePage := 1252  ;
        $59 : FLayer.CodePage := 1252  ;
        $64 : FLayer.CodePage := 852   ;
        $65 : FLayer.CodePage := 866   ;
        $66 : FLayer.CodePage := 865   ;
        $67 : FLayer.CodePage := 861   ;
        $6A : FLayer.CodePage := 737   ;
        $6B : FLayer.CodePage := 857   ;
        $6C : FLayer.CodePage := 863   ;
        $78 : FLayer.CodePage := 950   ;
        $79 : FLayer.CodePage := 949   ;
        $7A : FLayer.CodePage := 936   ;
        $7B : FLayer.CodePage := 932   ;
        $7C : FLayer.CodePage := 874   ;
        $86 : FLayer.CodePage := 737   ;
        $87 : FLayer.CodePage := 852   ;
        $88 : FLayer.CodePage := 857   ;
        $96 : FLayer.CodePage := 10007 ;
        $97 : FLayer.CodePage := 10029 ;
        $C8 : FLayer.CodePage := 1250  ;
        $C9 : FLayer.CodePage := 1251  ;
        $CA : FLayer.CodePage := 1254  ;
        $CB : FLayer.CodePage := 1253  ;
        $CC : FLayer.CodePage := 1257  ;
        else  FLayer.CodePage := 0     ;
      end ;
    end ;

  end ;

  procedure TGIS_FileDBF.WriteHeader ;
  var
    year, month, day : Word ;
  begin
    if assigned( FLayer ) then
      case FLayer.CodePage of
        437   : T_FileHeader(fileHeader ).CodePage := $01 ;
        850   : T_FileHeader(fileHeader ).CodePage := $02 ;
        860   : T_FileHeader(fileHeader ).CodePage := $24 ;
        874   : T_FileHeader(fileHeader ).CodePage := $7C ;
        852   : T_FileHeader(fileHeader ).CodePage := $64 ;
        866   : T_FileHeader(fileHeader ).CodePage := $65 ;
        865   : T_FileHeader(fileHeader ).CodePage := $66 ;
        861   : T_FileHeader(fileHeader ).CodePage := $67 ;
        857   : T_FileHeader(fileHeader ).CodePage := $6B ;
        863   : T_FileHeader(fileHeader ).CodePage := $6C ;
        950   : T_FileHeader(fileHeader ).CodePage := $4F ;
        949   : T_FileHeader(fileHeader ).CodePage := $4E ;
        936   : T_FileHeader(fileHeader ).CodePage := $4D ;
        932   : T_FileHeader(fileHeader ).CodePage := $13 ;
        737   : T_FileHeader(fileHeader ).CodePage := $6A ;
        1250  : T_FileHeader(fileHeader ).CodePage := $C8 ;
        1251  : T_FileHeader(fileHeader ).CodePage := $C9 ;
        1252  : T_FileHeader(fileHeader ).CodePage := $03 ;
        1254  : T_FileHeader(fileHeader ).CodePage := $CA ;
        1253  : T_FileHeader(fileHeader ).CodePage := $CB ;
        1257  : T_FileHeader(fileHeader ).CodePage := $CC ;
        10000 : T_FileHeader(fileHeader ).CodePage := $04 ;
        10007 : T_FileHeader(fileHeader ).CodePage := $96 ;
        10029 : T_FileHeader(fileHeader ).CodePage := $97 ;
        28591 : T_FileHeader(fileHeader ).CodePage := $57 ;
        else    T_FileHeader(fileHeader ).CodePage := $00 ;
      end
    else        T_FileHeader(fileHeader ).CodePage := $00 ;

    T_FileHeader(fileHeader ).FileCode := $03 ;

    DecodeDate( Now, year, month, day );
    T_FileHeader(fileHeader ).Year  := year - 1900 ;
    T_FileHeader(fileHeader ).Month := month ;
    T_FileHeader(fileHeader ).Day   := day   ;

    Seek( 0, soBeginning ) ;
    {$IFNDEF OXYGENE}
      with T_FileHeader(fileHeader) do begin
        WriteBuffer  ( FileCode     , sizeOf( FileCode     ) ) ;
        WriteBuffer  ( Year         , sizeOf( Year         ) ) ;
        WriteBuffer  ( Month        , sizeOf( Month        ) ) ;
        WriteBuffer  ( Day          , sizeOf( Day          ) ) ;
        WriteBuffer  ( RecordsCount , sizeOf( RecordsCount ) ) ;
        WriteBuffer  ( HeaderSize   , sizeOf( HeaderSize   ) ) ;
        WriteBuffer  ( RecordSize   , sizeOf( RecordSize   ) ) ;
        WriteBuffer  ( Reserved1    , sizeOf( Reserved1    ) ) ;
        WriteBuffer  ( StatustTrans , sizeOf( StatustTrans ) ) ;
        WriteBuffer  ( Encrypted    , sizeOf( Encrypted    ) ) ;
        WriteBytesCnt( MultiUser    , length( MultiUser    ) ) ;
        WriteBuffer  ( IndexByte    , sizeOf( IndexByte    ) ) ;
        WriteBuffer  ( CodePage     , sizeOf( CodePage     ) ) ;
        WriteBuffer  ( Reserved2    , sizeOf( Reserved2    ) ) ;
      end ;
    {$ELSE}
      with T_FileHeader(fileHeader) do begin
        WriteByte( FileCode      ) ;
        WriteByte( Year          ) ;
        WriteByte( Month         ) ;
        WriteByte( Day           ) ;
        WriteInteger( RecordsCount  ) ;
        WriteSmallInt( HeaderSize    ) ;
        WriteSmallInt( RecordSize    ) ;
        WriteSmallInt( Reserved1     ) ;
        WriteByte( StatustTrans  ) ;
        WriteByte( Encrypted     ) ;
        Write( MultiUser, 12 ) ;
        WriteByte( IndexByte     ) ;
        WriteByte( CodePage      ) ;
        WriteSmallInt( Reserved2     ) ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_FileDBF.CopyHeader(
    const _file : TGIS_FileDBF
  ) ;
  var
    src : T_FileHeader ;
    dst : T_FileHeader ;
  begin
    src := T_FileHeader( _file.fileHeader ) ;
    dst := T_FileHeader( self.fileHeader  ) ;

    src.FileCode     := dst.FileCode     ;
    src.Year         := dst.Year         ;
    src.Month        := dst.Month        ;
    src.Day          := dst.Day          ;
    src.RecordsCount := dst.RecordsCount ;
    src.HeaderSize   := dst.HeaderSize   ;
    src.RecordSize   := dst.RecordSize   ;
    src.Reserved1    := dst.Reserved1    ;
    src.StatustTrans := dst.StatustTrans ;
    src.Encrypted    := dst.Encrypted    ;
    src.MultiUser    := dst.MultiUser    ;
    src.IndexByte    := dst.IndexByte    ;
    src.CodePage     := dst.CodePage     ;
    src.Reserved2    := dst.Reserved2    ;
  end ;

  procedure TGIS_FileDBF.ReadStructure ;
  var
    i           : Integer ;
    fld_no      : Integer ;
    readed      : Integer ;
    field       : T_FieldDescriptor ;
    tmp_name    : String ;
    tmp_name2   : String ;
    tmp_type    : TGIS_FieldType ;
    tmp_width   : Integer ;
    tmp_decimal : Integer ;
    tmp_encoding: TEncoding ;

    // clear name fill zero
    procedure clear_name( const _cnt : Integer ) ;
    var
      fnd : Boolean ;
      ii  : Integer ;
    begin
      case FLayer.CodePage of
        1200,
        1201 : // UTF-16
          begin
            fnd := False ;
            for ii := 0 to (_cnt - 1) div 2 do begin
              if fnd then begin
                field.Name [ ii*2   ] := 0 ;
                field.Name [ ii*2+1 ] := 0 ;
              end
              else
              if ( field.Name[ ii*2   ] = 0 ) and
                 ( field.Name[ ii*2+1 ] = 0 )
              then
                fnd := True  ;
            end ;
          end ;
        else
          begin
            fnd := False ;
            for ii := 0 to _cnt-1 do begin
            if fnd then
              field.Name [ ii ] := 0
            else
            if field.Name[ ii ] = 0 then
              fnd := True  ;
          end;
        end ;
      end;
    end;


    {$IFDEF OXYGENE}
      function  read_field : Boolean ;
    {$ELSE}
      procedure read_field ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      var
        cnt : Integer ;
    {$ENDIF}
    begin
      {$IFDEF OXYGENE}
        Result := False ;
      {$ENDIF}
      // Read not as a BufferToStruct but read all elements independently
      with field do begin
        {$IFDEF OXYGENE}
          cnt := Read       ( Name          , 11                  ) ;
          if cnt <> 11 then exit ;
          clear_name        (                 11                  ) ;

          ReadByteBuffer    ( FieldType     , sizeOf( Byte      ) ) ;
          ReadIntegerBuffer ( Displacement  , sizeOf( Integer   ) ) ;
          ReadByteBuffer    ( Width         , sizeOf( Byte      ) ) ;
          ReadByteBuffer    ( Decimal       , sizeOf( Byte      ) ) ;
          ReadSmallIntBuffer( Reserved1     , sizeOf( SmallInt  ) ) ;
          ReadByteBuffer    ( WorkAreaID    , sizeOf( Byte      ) ) ;
          cnt := Read       ( Reserved2     , 10                  ) ;
          if cnt <> 10 then exit ;
          ReadByteBuffer    ( IndexByte     , sizeOf( Byte      ) ) ;
        {$ELSE}
          ReadBytesCnt      ( Name          , length( Name      ) ) ;
          clear_name        (                 length( Name      ) ) ;
          ReadBuffer        ( FieldType     , sizeOf( Byte      ) ) ;
          ReadBuffer        ( Displacement  , sizeOf( Integer   ) ) ;
          ReadBuffer        ( Width         , sizeOf( Byte      ) ) ;
          ReadBuffer        ( Decimal       , sizeOf( Byte      ) ) ;
          ReadBuffer        ( Reserved1     , sizeOf( SmallInt  ) ) ;
          ReadBuffer        ( WorkAreaID    , sizeOf( Byte      ) ) ;
          ReadBytesCnt      ( Reserved2     , length( Reserved2 ) ) ;
          ReadBuffer        ( IndexByte     , sizeOf( Byte      ) ) ;
        {$ENDIF}
      end ;
      {$IFDEF OXYGENE}
        Result := True ;
      {$ENDIF}
    end ;

  begin
    readed := Position ;

    {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( offsetFields, 1 ) ;
    offsetFields[0] := 0 ;

    {$IFDEF JAVA OR ISLAND}
      var cp := '' ;
      if assigned( FLayer ) then
        cp := TCodePageConverter.Convert(FLayer.CodePage) ;
      if not IsStringEmpty(cp) then
        tmp_encoding := TEncoding.GetEncoding( cp )
      else
        tmp_encoding := TEncoding.GetEncoding( TCodePageConverter.Convert(GisSystemCodePage) ) ;
    {$ELSE}
      if assigned( FLayer ) then
        tmp_encoding := TEncoding.GetEncoding( FLayer.CodePage )
      else
        tmp_encoding := TEncoding.GetEncoding( GisSystemCodePage ) ;
    {$ENDIF}
    try
      repeat
        field := T_FieldDescriptor.Create ;
        try
          {$IFDEF OXYGENE}
            if not read_field then
              break ;
          {$ELSE}
            try
              read_field ;
            except
              // dbf file is corrupted, but we still want to open it
              break ;
            end ;
          {$ENDIF}

          tmp_name    := Trim( tmp_encoding.GetString( field.Name ) ) ;
          tmp_width   := field.Width ;
          tmp_decimal := field.Decimal ;

          {$IFDEF JAVA}
            if tmp_name.length = 0 then // end
          {$ELSE}
            if {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( tmp_name ) = 0 then // end
          {$ENDIF}
            break ;
          case field.FieldType of
            Byte('C') : tmp_type := TGIS_FieldType.String  ;
            Byte('D') : tmp_type := TGIS_FieldType.Date    ;
            Byte('L') : tmp_type := TGIS_FieldType.Boolean ;
            Byte('N') : tmp_type := TGIS_FieldType.Number  ;
            Byte('F') : tmp_type := TGIS_FieldType.Float   ;
            else  begin
                    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
                    {$IFNDEF OXYGENE}
                    tmp_type := TGIS_FieldType.String ; // to avoid warning that it
                                                     // was uninitialized
                    {$ENDIF}
                  end ;
          end ;

          if assigned( FLayer ) then
            if FLayer.FindField( tmp_name ) = -1 then
              FLayer.AddFieldInternal( tmp_name, tmp_type, tmp_width, tmp_decimal )
            else begin
              for i := 1 to 255 do begin
                tmp_name2 := Format( '%s%.2x',
                                     [ Copy( tmp_name, 1, 11 - 2 ), i ]
                                   ) ;
                if FLayer.FindField( tmp_name2 ) = -1 then begin
                  FLayer.AddFieldInternal(
                    tmp_name2,
                    tmp_type,
                    tmp_width,
                    tmp_decimal
                  ) ;
                  break ;
                end ;
              end ;
            end ;
          readed := Position ;
        finally
          FreeObject( field ) ;
        end ;
      until readed > T_FileHeader(fileHeader).HeaderSize - 32 ;

      if assigned( FLayer ) then begin
        fld_no := FLayer.FindFieldInternal( GIS_FIELD_DUMMY ) ;
        if fld_no >= 0 then
          TGIS_FieldInfo( FLayer.FieldInfo(fld_no) ).Deleted := True ;
      end ;
    finally
      FreeObject( tmp_encoding ) ;
    end ;

  end ;

  procedure TGIS_FileDBF.WriteStructure ;
  var
    i,j         : Integer ;
    field       : T_FieldDescriptor ;
    trm         : Byte    ;
    fname       : TBytes  ;
    tmp_encoding: TEncoding ;

    procedure write_field ;
    begin
      // Write not as a StructToBuffer but write all elements independently
      {$IFDEF OXYGENE}
          Write        ( field.Name,        11 ) ;
          WriteByte    ( field.FieldType       ) ;
          WriteInteger ( field.Displacement    ) ;
          WriteByte    ( field.Width           ) ;
          WriteByte    ( field.Decimal         ) ;
          WriteSmallInt( field.Reserved1       ) ;
          WriteByte    ( field.WorkAreaID      ) ;
          Write        ( field.Reserved2,   10 ) ;
          WriteByte    ( field.IndexByte       ) ;
      {$ELSE}
        with field do begin
          WriteBytesCnt( Name        , length( Name          ) ) ;
          WriteBuffer  ( FieldType   , sizeOf( FieldType     ) ) ;
          WriteBuffer  ( Displacement, sizeOf( Displacement  ) ) ;
          WriteBuffer  ( Width       , sizeOf( Width         ) ) ;
          WriteBuffer  ( Decimal     , sizeOf( Decimal       ) ) ;
          WriteBuffer  ( Reserved1   , sizeOf( Reserved1     ) ) ;
          WriteBuffer  ( WorkAreaID  , sizeOf( WorkAreaID    ) ) ;
          WriteBytesCnt( Reserved2   , length( Reserved2     ) ) ;
          WriteBuffer  ( IndexByte   , sizeOf( IndexByte     ) ) ;
        end ;
      {$ENDIF}
    end ;

  begin
    T_FileHeader(fileHeader ).RecordSize := 1 ; // at least deleted !

    if assigned( FLayer ) then begin

      // delete unwanted temporary field(s)
         for i:=0 to FLayer.Fields.Count - 1 do
           if FLayer.FieldInfo( i ).ExportName = GIS_FIELD_DUMMY then
             FLayer.FieldInfo( i ).Deleted := True ;

      {$IFDEF JAVA OR ISLAND}
        var cp := '' ;
        cp := TCodePageConverter.Convert(CodePage) ;
        if not IsStringEmpty(cp) then
          tmp_encoding := TEncoding.GetEncoding( cp )
        else
          tmp_encoding := TEncoding.GetEncoding( TCodePageConverter.Convert(GisSystemCodePage) ) ;
      {$ELSE}
        tmp_encoding := TEncoding.GetEncoding( CodePage ) ;
      {$ENDIF}
      try
        for i:=0 to FLayer.Fields.Count - 1 do begin
          if FLayer.FieldInfo( i ).Deleted    then continue ;
          if FLayer.FieldInfo( i ).Temporary  then continue ;

          field := T_FieldDescriptor.Create ;
          try
            if FLayer.FieldInfo( i ).Saved then begin
              fname := tmp_encoding.GetBytes( FLayer.FieldInfo( i ).ExportName ) ;
            end
            else
              fname := ConvertAnsiString(
                         UpperCase( FLayer.FieldInfo( i ).ExportName )
                       ) ;
            {$IFDEF JAVA}
              for j := 0 to RemObjects.Oxygene.System.length( field.Name )-1 do begin
                if j >= RemObjects.Oxygene.System.length( fname ) then
                  break ;
            {$ELSE}
              for j := 0 to {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( field.Name )-1 do begin
                if j >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( fname ) then
                  break ;
            {$ENDIF}
              field.Name[j] := fname[j] ;
            end ;

            with FLayer.FieldInfo( i ) do begin
              case FieldType of
                TGIS_FieldType.String  :
                     begin
                       if NewWidth > 254 then NewWidth := 254 ;
                       field.FieldType := Byte('C') ;
                       field.Width     := NewWidth ;
                       field.Decimal   := 0 ;
                     end ;
                TGIS_FieldType.Date :
                     begin
                       if Binary = 0 then
                         NewWidth        := 8   ;
                       field.FieldType := Byte('D') ;
                       field.Width     := 8   ;
                       field.Decimal   := 0   ;
                     end ;
                TGIS_FieldType.Boolean :
                     begin
                       NewWidth        := 1   ;
                       field.FieldType := Byte('L') ;
                       field.Width     := 1   ;
                       field.Decimal   := 0   ;
                     end ;
                TGIS_FieldType.Number :
                     begin
                       if NewWidth              > 20 then NewWidth   := 20 ;
                       if NewWidth - NewDecimal < 2  then NewDecimal := NewWidth-2 ;
                       if NewDecimal            < 0  then NewDecimal := 0 ;

                       field.FieldType := Byte('N') ;
                       field.Width     := FLayer.FieldInfo( i ).NewWidth ;
                       field.Decimal   := FLayer.FieldInfo( i ).NewDecimal ;
                     end ;
                TGIS_FieldType.Float :
                     begin
                       NewWidth        := 20  ;
                       field.FieldType := Byte('F') ;
                       field.Width     := 20  ;
                       field.Decimal   :=  5  ;
                     end ;
                else raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_WRONGFIELD ), '', 0 ) ;
              end ;

              if Binary > 0 then begin
                field.FieldType := Byte('C') ;
                field.Width     := Binary ;
                field.Decimal   := 0      ;
              end ;

            end ;

            write_field ;
            // limitation of RecordSize
            if  (T_FileHeader(fileHeader ).RecordSize + field.Width) > 32767 then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 32767 ) ;

            T_FileHeader(fileHeader ).RecordSize :=
              T_FileHeader(fileHeader ).RecordSize + field.Width ;
          finally
            FreeObject( field ) ;
          end ;
        end ;
      finally
        FreeObject( tmp_encoding );
      end ;

    end ;

    if T_FileHeader(fileHeader ).RecordSize <=1 then begin
      // create dummy field

      field := T_FieldDescriptor.Create ;
      try
        fname := ConvertAnsiString( GIS_FIELD_DUMMY ) ;
        {$IFDEF JAVA}
          for j := 0 to RemObjects.Oxygene.System.length( field.Name )-1 do begin
            if j >= RemObjects.Oxygene.System.length( fname ) then
        {$ELSE}
          for j := 0 to {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( field.Name ) - 1 do begin
            if j >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( fname ) then
        {$ENDIF}
            field.Name[j] := 0
          else
            field.Name[j] := fname[j] ;
        end ;

        field.FieldType := Byte('C') ;
        field.Width     := 1 ;
        field.Decimal   := 0 ;

        write_field ;
        T_FileHeader(fileHeader ).RecordSize :=
          T_FileHeader(fileHeader ).RecordSize + field.Width ;
      finally
        FreeObject( field ) ;
      end ;
    end ;

    trm := $0d ;

    {$IFNDEF OXYGENE}
      WriteBuffer( trm, sizeOf( trm ) ) ;
    {$ELSE}
      WriteByte( trm ) ;
    {$ENDIF}
    T_FileHeader(fileHeader ).HeaderSize := Position  ;
  end ;

  function TGIS_FileDBF.GetField(
    const _uid   : TGIS_Uid ;
    const _field : String  ;
    const _id    : Integer
  ) : Variant ;
  var
    i       : Integer ;
    nr, nr2 : Integer ;
    off     : Int64   ;
    fld     : TGIS_FieldInfo ;
    fld2    : TGIS_FieldInfo ;
    ival    : Integer ;
    jval    : Integer ;
    {$IFNDEF OXYGENE}
      dval  : Double  ;
    {$ENDIF}
    size    : Integer ;
    txt     : String  ;
    h,m,s,ms: Word ;
  begin
    Result := Unassigned ;

    if (_uid < 1) or (_uid > T_FileHeader(fileHeader ).RecordsCount) then exit ;

    CodePage    := FLayer.CodePage    ;

    if _uid <> lastUid then begin
      for i:=0 to fetchedFields.Count - 1 do
        fetchedFields[ i ] := Unassigned ;
      lastUid := _uid ;
    end ;

    if not IsStringEmpty( _field ) then
      nr := FLayer.FindFieldInternal( _field )
    else
      nr := _id ;

    try
      if ( nr >= 0 ) and ( nr < fetchedFields.Count ) then begin
        Result := fetchedFields[ nr ] ;
        if not VarIsEmpty( Result ) then exit ;

        fld  := FLayer.FieldInfo( nr ) ;
        assert( assigned( fld ) ) ;
        assert( not IsStringEmpty( fld.Name ) ) ;

        if fld.Saved then begin
          off := Int64(_uid -1) * T_FileHeader(fileHeader ).RecordSize +
                 T_FileHeader(fileHeader ).HeaderSize;
          nr2 := high( offsetFields ) ;

          if nr2 < nr then begin
            {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( offsetFields, nr + 1 ) ;
            for i := nr2 to nr-1 do begin
              fld2 := FLayer.FieldInfo( i ) ;
              size := fld2.Binary ;
              if size = 0 then size := fld2.Width ;
              offsetFields[i+1] := offsetFields[i] + size ;
            end ;
          end ;

          off := off + offsetFields[ nr ] ;

          Seek( off + 1, soBeginning ) ;
          for i := low( bufIn ) to high( bufIn ) do
            bufIn[i] := 0 ;

          with fld do begin
            case FieldType of
              TGIS_FieldType.String :
                   begin
                     {$IFDEF JAVA}
                       txt := ReadString( fld.Width );
                     {$ELSE}
                       ReadString( txt, fld.Width );
                     {$ENDIF}

                     Result := TrimRight( txt ) ;
                   end ;
              TGIS_FieldType.Float :
                   begin
                     ReadBytesCnt( bufIn, fld.Width );

                     if Binary = 0 then begin
                       txt := Trim( ConvertAnsiString( bufIn ) ) ;
                       if IsStringEmpty( txt ) then
                         Result := NullVar
                       else if txt[StringFirst] = '*' then
                         Result := NullVar
                       else
                         Result := DotStrToFloat( txt ) ;
                     end
                     else begin
                       {$IFDEF MANAGED}
                         Result := BitConverter.ToDouble( bufIn, 0 ) ;
                       {$ELSE}
                         dval := 0 ;
                         System.Move( bufIn[0], dval, Binary ) ;
                         Result := dval ;
                       {$ENDIF}
                     end ;
                   end ;
              TGIS_FieldType.Number :
                   begin
                     ReadBytesCnt( bufIn, fld.Width );

                     try
                       if Decimal = 0 then begin
                         if Binary = 0 then begin
                           txt := Trim( ConvertAnsiString( bufIn ) ) ;
                           if IsStringEmpty( txt ) then
                             Result := NullVar
                           else begin
                             if txt[StringFirst] = '*' then begin
                               Result := NullVar ;
                             end
                             else if Width > 9 then begin
                               Result := StrToInt64( txt ) ;
                             end
                             else begin
                               Result := StrToInt( txt ) ;
                             end ;
                           end ;
                         end
                         else begin
                           {$IFDEF MANAGED}
                             case Binary of
                               2 : Result := BitConverter.ToInt16( bufIn, 0 ) ;
                               4 : Result := BitConverter.ToInt32( bufIn, 0 ) ;
                               8 : Result := BitConverter.ToInt64( bufIn, 0 ) ;
                             else  Result := BitConverter.ToInt32( bufIn, 0 ) ;
                             end ;
                           {$ELSE}
                             ival := 0 ;
                             System.Move( bufIn[0], ival, Binary ) ;
                             Result := ival ;
                           {$ENDIF}
                         end ;
                       end
                       else begin
                         if Binary = 0 then begin
                           txt := Trim( ConvertAnsiString( bufIn ) ) ;
                           if IsStringEmpty( txt ) then
                             Result := NullVar
                           else if txt[StringFirst] = '*' then
                             Result := NullVar
                           else
                             Result := DotStrToFloat( txt ) ;
                         end
                         else begin
                           {$IFDEF MANAGED}
                             case Binary of
                               4 : Result := BitConverter.ToSingle( bufIn, 0 ) ;
                               8 : Result := BitConverter.ToDouble( bufIn, 0 ) ;
                             else  Result := BitConverter.ToDouble( bufIn, 0 ) ;
                             end ;
                           {$ELSE}
                             dval := 0 ;
                             System.Move( bufIn[0], dval, Binary ) ;
                             Result := dval ;
                           {$ENDIF}
                         end ;
                       end ;
                     except
                       Result := 0 ;
                     end ;
                   end ;
              TGIS_FieldType.Boolean :
                   begin
                     ReadBytesCnt( bufIn, fld.Width );

                     case bufIn[0] of
                       Byte('t'),
                       Byte('T'),
                       Byte('y'),
                       Byte('Y'),
                       Byte('1'),
                       Byte(#1)  : Result := True  ;
                       Byte('f'),
                       Byte('F'),
                       Byte('n'),
                       Byte('N'),
                       Byte('0'),
                       Byte(#0)  : Result := False ;
                       else        Result := NullVar  ;
                     end ;
                   end ;
              TGIS_FieldType.Date :
                   begin
                     ReadBytesCnt( bufIn, fld.Width );

                     try
                       if Binary = 0 then begin
                         txt := Trim( ConvertAnsiString( bufIn ) ) ;
                         if IsStringEmpty( txt ) then
                           Result := NullVar
                         else if txt[StringFirst] = '*' then
                           Result := NullVar
                         else if txt = '00000000' then
                           Result := NullVar
                         else
                           Result := EncodeDate(
                             StrToInt( Copy( txt, StringFirst    , 4 ) ),
                             StrToInt( Copy( txt, StringFirst + 4, 2 ) ),
                             StrToInt( Copy( txt, StringFirst + 6, 2 ) )
                           )
                       end
                       else begin
                         Result := 0 ;
                         ival := 0 ;
                         jval := 0 ;
                           if Binary = 4 then begin
                             {$IFDEF MANAGED}
                               ival := BitConverter.ToInt32( bufIn, 0 ) ;
                             {$ELSE}
                               System.Move( bufIn[0], ival, Binary ) ;
                             {$ENDIF}
                             ival := ival shr 32 ;
                           end
                           else if Binary = 8 then begin
                             {$IFDEF MANAGED}
                               ival := BitConverter.ToInt32( bufIn, 0 ) ;
                               jval := BitConverter.ToInt32( bufIn, 4 ) ;
                             {$ELSE}
                               System.Move( bufIn[0], ival, 4 ) ;
                               System.Move( bufIn[4], jval, 4 ) ;
                             {$ENDIF}
                           end ;

                         if ival <> 0 then begin
                           case Binary of
                             4 : if (fld.Width = 8) and (ival <= 86400000) then begin
                                   h  := TruncS(ival/3600000.0);
                                   m  := TruncS((ival/1000.0 - h*3600.0)/60.0);
                                   s  := TruncS(ival/1000.0 - h*3600.0 - m*60.0);
                                   ms := TruncS(ival-h*3600000.0-m*60000.0-s*1000.0);
                                   Result :=  EncodeTime( h, m, s, ms ) ;
                                 end
                                 else
                                   Result :=  EncodeDate(
                                                ( ival and $0000FFFF ),
                                                ( ival and $00FF0000 ) shr 16,
                                                ( ival and $FF000000 ) shr 24
                                              ) ;
                             8 : begin
                                   h  := TruncS(jval/3600000.0);
                                   m  := TruncS((jval/1000.0 - h*3600.0)/60.0);
                                   s  := TruncS(jval/1000.0 - h*3600.0 - m*60.0);
                                   ms := TruncS(jval-h*3600000.0-m*60000.0-s*1000.0);
                                   {$IFDEF OXYGENE}
                                      Result := TDateTime.Create(( ival and $0000FFFF ),
                                                    ( ival and $00FF0000 ) shr 16,
                                                    ( ival and $FF000000 ) shr 24,
                                                    h, m, s{$IFNDEF JAVA}, ms{$ENDIF}
                                                ) ;
                                   {$ELSE}
                                      Result :=   EncodeDate(
                                                    ( ival and $0000FFFF ),
                                                    ( ival and $00FF0000 ) shr 16,
                                                    ( ival and $FF000000 ) shr 24
                                                  ) +
                                                  EncodeTime( h, m, s, ms ) ;
                                   {$ENDIF}
                                 end ;
                           end ;
                         end
                         else
                           Result := NullVar ;
                       end ;

                     except
                       Result := NullVar ;
                     end ;
                   end ;
              else begin
                     ReadBytesCnt( bufIn, fld.Width );
                     raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_UNTESTED ), '', 0 ) ;
                   end ;
            end ;
          end ;
        end ;
        fetchedFields[ nr ] := Result ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FIELDNOEXIST ), _field, 0 );
    end ;
  end ;

  procedure TGIS_FileDBF.WriteRecord (
    const _file    : TGIS_Stream    ;
    const _value   : Variant        ;
    const _type    : TGIS_FieldType ;
    const _size    : Integer        ;
    const _decimal : Integer        ;
    const _binary  : Integer
  ) ;
  var
    j,k     : Integer        ;
    l       : Integer        ;
    sbuf    : String         ;
    sbuf_sb : TStringBuilder ;
    bbuf    : TBytes         ;
    sgap    : String         ;
    dtmp    : Double         ;
    itmp    : Integer        ;
    sitmp   : SmallInt       ;
    uitmp   : Cardinal       ;
    year    : Word           ;
    month   : Word           ;
    day     : Word           ;
    h       : Word           ;
    m       : Word           ;
    s       : Word           ;
    ms      : Word           ;
    exp_txt : String         ;
    v       : Variant        ;
    ewidth  : Integer        ;
    i64     : Int64          ;
  begin
    ewidth := _size ;
    v := _value ;

    CodePage    := FLayer.CodePage    ;

    case _type of
      TGIS_FieldType.String :
        // left justify
           begin
             if VarIsNull( v ) or VarIsEmpty( v ) then
               _file.WriteVarAsString( '', ewidth )
             else
               _file.WriteVarAsString( v, ewidth ) ;
           end ;
      TGIS_FieldType.Number :
        // right justify
           begin
            if _binary = 0 then begin

               try
                 case VarType( v ) of
                   varSmallInt  : sbuf := IntToStr( VarToInt16( v ) ) ;
                   varInteger   : sbuf := IntToStr( VarToInt32( v ) ) ;
                   varInt64     : begin
                                   i64 := VarToInt64( v ) ;
                                   sbuf := IntToStr( i64 ) ;
                                  end ;
                   varShortInt  : sbuf := IntToStr( VarToSByte( v ) ) ;
                   varWord      : sbuf := IntToStr( VarToUInt16( v ) ) ;
                   varLongWord  : sbuf := IntToStr( VarToUInt32( v ) ) ;
                   varByte      : sbuf := IntToStr( VarToByte( v ) ) ;
                   varNull      : sbuf := '' ;
                   else           sbuf := Format( '%.*f',
                                                  [ _decimal, VarToDouble(v) ]
                                                ) ;
                 end ;
                 {$IFDEF JAVA}
                   if RemObjects.Oxygene.System.length( sbuf ) > ewidth then begin
                 {$ELSE}
                   if {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( sbuf ) > ewidth then begin
                 {$ENDIF}
                   SetLengthStr( sbuf, ewidth ) ;
                   sbuf_sb := TStringBuilder.Create( sbuf ) ;
                   try
                     {$IFDEF JAVA}
                     for l := 0 to sbuf.length - 1 do
                     {$ELSE}
                     for l := 0 to {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( sbuf ) - 1 do
                     {$ENDIF}
                       sbuf_sb[l] := '9' ;
                     if Sign( VarToDouble( v ) ) < 0 then
                       sbuf_sb[0] := '-' ;
                     if _decimal > 0 then begin
                       j := ewidth - _decimal - 1 ;
                       if j > -1 then
                         sbuf_sb[j] := '.' ;
                     end ;
                     sbuf := sbuf_sb.ToString ;
                   finally
                     FreeObject( sbuf_sb ) ;
                   end ;
                 end ;
               except
                 sbuf := '0' ;
               end ;

               {$IFDEF JAVA}
                 k := Max( 0, ewidth - sbuf.length ) ;
               {$ELSE}
                 k := Max( 0, ewidth - {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( sbuf ) ) ;
               {$ENDIF}
               if k > 0 then begin
                 sgap := '' ;
                 _file.WriteStringCnt( sgap, k ) ;
               end ;

               _file.WriteStringCnt( sbuf, ewidth -k ) ;
            end
            else begin
              {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( bbuf, _binary ) ;
               case VarType( v ) of
                 varSmallInt : begin
                   try
                     sitmp := VarToInt16( v ) ;
                   except
                     sitmp := 0 ;
                   end ;
                   {$IFDEF MANAGED}
                     GisCopyMemory( BitConverter.GetBytes( sitmp ),
                                              0, bbuf, 0, _binary ) ;
                   {$ELSE}
                     System.Move( sitmp, bbuf[0], _binary ) ;
                   {$ENDIF}
                 end ;
                 varInteger  ,
                 varInt64    :
                    begin
                     i64 := VarToInt64( v ) ;
                     {$IFDEF MANAGED}
                       GisCopyMemory( BitConverter.GetBytes( i64 ),
                                                0, bbuf, 0, _binary ) ;
                     {$ELSE}
                       System.Move( i64, bbuf[0], _binary ) ;
                     {$ENDIF}
                   end ;
                 varShortInt ,
                 varWord     ,
                 varByte       : begin
                   try
                     itmp := VarToInt32( v ) ;
                   except
                     itmp := 0 ;
                   end ;
                   {$IFDEF MANAGED}
                     GisCopyMemory( BitConverter.GetBytes( itmp ),
                                              0, bbuf, 0, _binary ) ;
                   {$ELSE}
                     System.Move( itmp, bbuf[0], _binary ) ;
                   {$ENDIF}
                 end ;
                 varLongWord : begin
                   try
                     uitmp := VarToUInt32( v ) ;
                   except
                     uitmp := 0 ;
                   end ;
                   {$IFDEF MANAGED}
                     GisCopyMemory( BitConverter.GetBytes( uitmp ),
                                              0, bbuf, 0, _binary ) ;
                   {$ELSE}
                     System.Move( uitmp, bbuf[0], _binary ) ;
                   {$ENDIF}
                 end ;
               end ;

               _file.WriteBytes( bbuf );
            end ;
           end ;
      TGIS_FieldType.Float :
        // fill
           begin
            if _binary = 0 then begin
               if VarIsNull( v ) then begin
                 sbuf := ' ' ;
               end
               else begin
                 try
                   dtmp := VarToDouble( v ) ;
                 except
                   dtmp := 0 ;
                 end ;

                 {$IFDEF CLR}
                   {$IFNDEF OXYGENE}
                     sbuf := Format( '%.14e', [dtmp] ) ;
                   {$ELSE}
                     sbuf := Format( '%.14E', [dtmp] ) ;
                   {$ENDIF}
                 {$ELSE}
                   sbuf := Format( '%.15e', [dtmp] ) ;
                 {$ENDIF}
                 if Copy( sbuf, StringLast( sbuf ) - 2 , 3 ) > '099' then
                   exp_txt := '99'
                 else
                   exp_txt := Copy( sbuf, StringLast( sbuf ) - 1 , 2 ) ;
                 sbuf := Copy( sbuf, StringFirst,16 ) +
                         Copy( sbuf, StringLast( sbuf ) - 4 , 2 ) +
                         exp_txt ;
               end ;

               _file.WriteStringCnt( sbuf, ewidth ) ;
            end
            else begin
              {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( bbuf, _binary ) ;

               if VarIsNull( v ) then begin
                 dtmp := 0 ;
               end
               else begin
                 try
                   dtmp := VarToDouble( v ) ;
                 except
                   dtmp := 0 ;
                 end ;
               end ;
               {$IFDEF MANAGED}
                 GisCopyMemory( BitConverter.GetBytes( dtmp ), 0, bbuf, 0, _binary ) ;
               {$ELSE}
                 System.Move( dtmp, bbuf[0], _binary ) ;
               {$ENDIF}
               _file.WriteBytes( bbuf );
           end ;
           end ;
      TGIS_FieldType.Date :
        // right justify
           begin
             if _binary = 0 then begin
               try
                 if VarIsNull( v ) then
                   sbuf := ''
                 else
                   sbuf := FormatDateTime( 'yyyyMMdd', VarToDateTime( v ) ) ;
               except
                 sbuf := '19000101' ;
               end ;
               _file.WriteStringCnt( sbuf, ewidth ) ;
             end
             else begin
               {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( bbuf, _binary ) ;

               for j := 0 to _binary-1 do
                 bbuf[j] := 0 ;

               if not VarIsNull( v ) then begin
                 if _binary = 4 then begin
                   if (_size = 10) or (_size = 0) then begin
                     DecodeDate( VarToDateTime( v ), year, month, day ) ;
 
                     bbuf[0] := Byte( ( year  and $00FF )       ) ;
                     bbuf[1] := Byte( ( year  and $FF00 ) shr 8 ) ;
                     bbuf[2] := Byte( ( month and $00FF )       ) ;
                     bbuf[3] := Byte( ( day   and $00FF )       ) ;
                   end
                   else begin
                     DecodeTime( VarToDateTime( v ), h, m, s, ms ) ;
                     itmp := (h*3600+m*60+s)*1000+ms ;
                     {$IFDEF MANAGED}
                       GisCopyMemory( BitConverter.GetBytes( itmp ), 0, bbuf, 0, 4 ) ;
                     {$ELSE}
                       System.Move( itmp, bbuf[0], 4 ) ;
                     {$ENDIF}
                   end;
                 end
                 else begin
                   DecodeDate( VarToDateTime( v ), year, month, day ) ;
                   DecodeTime( VarToDateTime( v ), h, m, s, ms ) ;

                   bbuf[0] := Byte( ( year  and $00FF )       ) ;
                   bbuf[1] := Byte( ( year  and $FF00 ) shr 8 ) ;
                   bbuf[2] := Byte( ( month and $00FF )       ) ;
                   bbuf[3] := Byte( ( day   and $00FF )       ) ;
                   itmp := (h*3600+m*60+s)*1000+ms ;
                   {$IFDEF MANAGED}
                     GisCopyMemory( BitConverter.GetBytes( itmp ), 0, bbuf, 4, 4 ) ;
                   {$ELSE}
                     System.Move( itmp, bbuf[4], 4 ) ;
                   {$ENDIF}
                 end ;
               end ;
               _file.WriteBytes( bbuf );
             end ;
           end ;
      TGIS_FieldType.Boolean :
        // no justification
           begin
             try
               if VarIsNull( v ) then
                 sbuf := ''
               else begin
                 if VarToBoolean( v ) then sbuf := 'Y'
                      else sbuf := 'N' ;
               end ;
             except
               sbuf := ' ' ;
             end ;

             _file.WriteStringCnt( sbuf, ewidth ) ;
           end ;
       else begin
             sbuf := '' ;
             _file.WriteStringCnt( sbuf, ewidth ) ;

    end ;
    end ;
  end ;

  procedure TGIS_FileDBF.AddRecord(
    const _shape : TGIS_Shape
  ) ;
  var
    i       : Integer ;
    v       : Variant ;
    empty   : Boolean ;
    ofld    : TGIS_FieldInfo ;
    ewidth  : Integer ;
    {$IFNDEF OXYGENE}
      oldsep  : Char   ;
    {$ELSE}
      oldsep  : String ;
    {$ENDIF}
  begin
    oldsep := {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator ;
    try
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := '.' ;

      Seek( 0, soEnd ) ;

      // deleted flag
      WriteStringCnt( ' ', 1 ) ;

      empty := True ;

      for i := 0 to FLayer.Fields.Count - 1 do begin
        ofld := FLayer.FieldInfo( i ) ;
        if ofld.Deleted   then continue ;
        if ofld.Temporary then continue ;

        ewidth := ofld.NewWidth ;
        v := _shape.GetFieldEx( ofld.NewName ) ;

        WriteRecord( self, v, ofld.FieldType,
                     ewidth, ofld.NewDecimal,
                     ofld.Binary
                   );

        empty := False ;
      end ;

      if empty then begin
        // write dummy field
        WriteStringCnt( ' ', 1 ) ;
      end ;

      inc( T_FileHeader(fileHeader ).RecordsCount ) ;
    finally
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := oldsep ;
    end ;
  end ;

  procedure TGIS_FileDBF.CopyRecords(
    const _file  : TGIS_Stream ;
    const _layer : TGIS_LayerVector
  ) ;
  var
    i, f    : Integer ;
    v       : Variant ;
    ofld    : TGIS_FieldInfo ;
    {$IFDEF OXYGENE}
      oldsep  : String ;
    {$ELSE}
      oldsep  : Char   ;
    {$ENDIF}
  begin
    if not assigned( _file ) or not assigned( _layer ) then exit ;

    {$IFDEF CLR}
      if not assigned( DecimalSeparator ) then
        DecimalSeparator := System.Threading.Thread.CurrentThread.CurrentCulture.NumberFormat.NumberDecimalSeparator ;
      oldsep := DecimalSeparator ;
    {$ELSE}
      oldsep := {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator ;
    {$ENDIF}
    try
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := '.' ;

      _file.Seek( 0, soEnd ) ;

      for f := 0 to T_FileHeader(fileHeader).RecordsCount - 1 do begin
        // deleted flag
        _file.WriteStringCnt( ' ', 1 ) ;

        for i := 0 to _layer.Fields.Count - 1 do begin
          ofld := _layer.FieldInfo( i ) ;
          if ofld.Deleted   then continue ;
          if ofld.Temporary then continue ;

          v := GetField( f+1, ofld.NewName, - 1 ) ;

          WriteRecord( _file, v, ofld.FieldType,
                       ofld.NewWidth, ofld.NewDecimal,
                       ofld.Binary
                     ) ;
        end ;
      end ;
    finally
      {$IFDEF DCC}FormatSettings.{$ENDIF}DecimalSeparator := oldsep ;
    end ;
  end ;

//==================================== END =====================================
end.

