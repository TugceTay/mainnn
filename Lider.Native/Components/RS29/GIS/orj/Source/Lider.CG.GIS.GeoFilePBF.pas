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
  Encapsulation of Openstreetmap PBF format support.
}

{$IFDEF DCC}
  unit GisFilePBF ;
  {$HPPEMIT '#pragma link "GisFilePBF"'}
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
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  GisFileGPB,
  GisTypes,
  GisClasses,
  GisStreams ;
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
  ///   Class storing a node information.
  /// </summary>
  TGIS_OSMNode = class
    public
      /// <summary>
      ///   node id
      /// </summary>
      id    : Int64 ;

      /// <summary>
      ///   node coordinates
      /// </summary>
      ptg   : TGIS_Point ;

      /// <summary>
      ///   node attributes
      /// </summary>
      attrs : TGIS_KeyValueList<String,String> ;
    public
      /// <summary>
      ///   Constructor
      /// </summary>
      constructor Create ;
      {$IFNDEF OXYGENE}
      /// <summary>
      ///   Destructor
      /// </summary>
      destructor  Destroy ; override;
      {$ENDIF}
  end ;

  /// <summary>
  ///   Class storing a way information.
  /// </summary>
  TGIS_OSMWay = class
    public
      /// <summary>
      ///   way id
      /// </summary>
      id    : Int64 ;

      /// <summary>
      ///   way attributes
      /// </summary>
      attrs : TGIS_KeyValueList<String,String> ;

      /// <summary>
      ///   list of way references to nodes
      /// </summary>
      refs  : TList<Int64> ;
    public
      /// <summary>
      ///   Constructor
      /// </summary>
      constructor Create ;
      {$IFNDEF OXYGENE}
      /// <summary>
      ///   Destructor
      /// </summary>
      destructor  Destroy ; override;
      {$ENDIF}
  end ;

  /// <summary>
  ///   Type of osm member
  /// </summary>
  TGIS_OSMMemberType = (
    /// <summary>
    ///   osm node
    /// </summary>
    node = 0,
    /// <summary>
    ///   osm way
    /// </summary>
    way = 1,
    /// <summary>
    ///   osm relation
    /// </summary>
    relation = 2
  ) ;

  /// <summary>
  ///   Class storing a member information.
  /// </summary>
  TGIS_OSMMember = class
    public
      /// <summary>
      ///   member id
      /// </summary>
      id    : Int64 ;

      /// <summary>
      ///   member type
      /// </summary>
      mtype : TGIS_OSMMemberType ;

      /// <summary>
      ///   member role
      /// </summary>
      role  : String ;
    public
      /// <summary>
      ///   Constructor
      /// </summary>
      constructor Create ; overload;
      /// <summary>
      ///   Constructor
      /// </summary>
      /// <param name="_id">
      ///   member id
      /// </param>
      /// <param name="_mtype">
      ///   member type
      /// </param>
      /// <param name="_role">
      ///   member role
      /// </param>
      constructor Create ( const _id    : Int64 ;
                           const _mtype : TGIS_OSMMemberType ;
                           const _role  : String
                          ) ; overload;
  end ;

  /// <summary>
  ///   Class storing a relation information.
  /// </summary>
  TGIS_OSMRelation = class
    public
      /// <summary>
      ///   relation id
      /// </summary>
      id      : Int64 ;

      /// <summary>
      ///   relation attributes
      /// </summary>
      attrs   : TGIS_KeyValueList<String,String> ;

      /// <summary>
      ///   list of relation members
      /// </summary>
      members : TList<TGIS_OSMMember> ;
    public
      /// <summary>
      ///   Constructor
      /// </summary>
      constructor Create ;
      {$IFNDEF OXYGENE}
      /// <summary>
      ///   Destructor
      /// </summary>
      destructor  Destroy ; override;
      {$ENDIF}
  end ;

  /// <summary>
  ///  Type of event fired for each node.
  /// </summary>
  /// <param name="_node">
  ///   node object
  /// </param>
  TGIS_OnNodeEvent = procedure(
    const _node : TGIS_OSMNode
  ) of object ;

  /// <summary>
  ///  Type of event fired for each way.
  /// </summary>
  /// <param name="_way">
  ///   way object
  /// </param>
  TGIS_OnWayEvent = procedure(
    const _way : TGIS_OSMWay
  ) of object ;

  /// <summary>
  ///  Type of event fired for each relation.
  /// </summary>
  /// <param name="_relation">
  ///   relation object
  /// </param>
  TGIS_OnRelationEvent= procedure(
    const _relation : TGIS_OSMRelation
  ) of object ;

  /// <summary>
  ///  Type of event fired for extent.
  /// </summary>
  /// <param name="_extent">
  ///   extent
  /// </param>
  TGIS_OnExtentEvent = procedure(
    const _extent : TGIS_Extent
  ) of object ;

  /// <summary>
  ///   Encapsulation of PBF format parser.
  /// </summary>
  TGIS_PBFParser = class
    private
    const
      BLOBHEADER_IDX_TYPE      =   1 ;
      BLOBHEADER_IDX_INDEXDATA =   2 ;
      BLOBHEADER_IDX_DATASIZE  =   3 ;

      BLOB_UNKNOWN    = 0 ;
      BLOB_OSMHEADER  = 1 ;
      BLOB_OSMDATA    = 2 ;

      BLOB_IDX_RAW       =  1 ;
      BLOB_IDX_RAW_SIZE  =  2 ;
      BLOB_IDX_ZLIB_DATA =  3 ;

      OSMHEADER_IDX_BBOX                           = 1  ;
      OSMHEADER_IDX_REQUIRED_FEATURES              = 4  ;
      OSMHEADER_IDX_OPTIONAL_FEATURES              = 5  ;
      OSMHEADER_IDX_WRITING_PROGRAM                = 16 ;
      OSMHEADER_IDX_SOURCE                         = 17 ;
      OSMHEADER_IDX_OSMOSIS_REPLICATION_TIMESTAMP  = 32 ;
      OSMHEADER_IDX_OSMOSIS_REPLICATION_SEQ_NUMBER = 33 ;
      OSMHEADER_IDX_OSMOSIS_REPLICATION_BASE_URL   = 34 ;

      HEADERBBOX_IDX_LEFT   =  1 ;
      HEADERBBOX_IDX_RIGHT  =  2 ;
      HEADERBBOX_IDX_TOP    =  3 ;
      HEADERBBOX_IDX_BOTTOM =  4 ;

      PRIMITIVEBLOCK_IDX_STRINGTABLE      = 1  ;
      PRIMITIVEBLOCK_IDX_PRIMITIVEGROUP   = 2  ;
      PRIMITIVEBLOCK_IDX_GRANULARITY      = 17 ;
      PRIMITIVEBLOCK_IDX_DATE_GRANULARITY = 18 ;
      PRIMITIVEBLOCK_IDX_LAT_OFFSET       = 19 ;
      PRIMITIVEBLOCK_IDX_LON_OFFSET       = 20 ;

      PRIMITIVEGROUP_IDX_NODES      = 1 ;
      PRIMITIVEGROUP_IDX_DENSE      = 2 ;
      PRIMITIVEGROUP_IDX_WAYS       = 3 ;
      PRIMITIVEGROUP_IDX_RELATIONS  = 4 ;
      PRIMITIVEGROUP_IDX_CHANGESETS = 5 ;

      READSTRINGTABLE_IDX_STRING  = 1 ;

      NODE_IDX_ID    =  1  ;
      NODE_IDX_LAT   =  8  ;
      NODE_IDX_LON   =  9  ;
      NODE_IDX_KEYS  =  2  ;
      NODE_IDX_VALS  =  3 ;
      NODE_IDX_INFO  =  4 ;

      DENSEINFO_IDX_VERSION     = 1 ;
      DENSEINFO_IDX_TIMESTAMP   = 2 ;
      DENSEINFO_IDX_CHANGESET   = 3 ;
      DENSEINFO_IDX_UID         = 4 ;
      DENSEINFO_IDX_USER_SID    = 5 ;
      DENSEINFO_IDX_VISIBLE     = 6 ;

      DENSENODES_IDX_ID         =  1 ;
      DENSENODES_IDX_DENSEINFO  =  5 ;
      DENSENODES_IDX_LAT        =  8 ;
      DENSENODES_IDX_LON        =  9 ;
      DENSENODES_IDX_KEYVALS    =  10 ;

      WAY_IDX_ID    =  1 ;
      WAY_IDX_KEYS  =  2 ;
      WAY_IDX_VALS  =  3 ;
      WAY_IDX_INFO  =  4 ;
      WAY_IDX_REFS  =  8 ;

      RELATION_IDX_ID         =  1  ;
      RELATION_IDX_KEYS       =  2  ;
      RELATION_IDX_VALS       =  3  ;
      RELATION_IDX_INFO       =  4  ;
      RELATION_IDX_ROLES_SID  =  8  ;
      RELATION_IDX_MEMIDS     =  9  ;
      RELATION_IDX_TYPES      =  10 ;

    private
      fs              : TGIS_BufferedFileStream ;
      granularity     : Integer ;
      dateGranularity : Integer ;
      latOffset       : Int64 ;
      lonOffset       : Int64 ;

      osmnode         : TGIS_OSMNode ;
      osmway          : TGIS_OSMWay ;
      osmrelation     : TGIS_OSMRelation ;
      stringtable     : TStringList ;
    private
      FOnNodeEvent      : TGIS_OnNodeEvent ;
      FOnWayEvent       : TGIS_OnWayEvent ;
      FOnRelationEvent  : TGIS_OnRelationEvent ;
      FOnExtentEvent    : TGIS_OnExtentEvent ;
    private
      function  readFSBuffer         ( const _buf : TBytes ;
                                       const _len : Integer
                                      ) : Integer ;
      function  processBlock         : Boolean ;
      function  readBlobHeader       ( const _data      : TBytes ;
                                       const _dataSize  : Integer ;
                                         var _blobSize  : Cardinal ;
                                         var _blobType  : Integer
                                      ) : Boolean ;
      function  readBlob             ( const _data      : TBytes ;
                                       const _dataSize  : Integer ;
                                       const _blobSize  : Cardinal ;
                                       const _blobType  : Integer
                                      ) : Boolean ;
      function  readOSMHeader        ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readPrimitiveBlock   ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readPrimitiveGroup   ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readHeaderBBox       ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readStringTable      ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readNode             ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readDenseNodes       ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readWay              ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
      function  readRelation         ( const _data      : TBytes ;
                                       const _dataSize  : Integer
                                      ) : Boolean ;
    public
      /// <summary>
      ///   Constructor
      /// </summary>
      constructor Create ;
      {$IFNDEF OXYGENE}
      /// <summary>
      ///   Destructor
      /// </summary>
      destructor  Destroy ; override;
      {$ENDIF}

      /// <summary>
      ///   Parse a file
      /// </summary>
      /// <param name="_path">
      ///   file path
      /// </param>
      procedure Parse                ( const _path : String
                                      ) ;
   published //events
      /// <event/>
      /// <summary>
      ///   Event fired for each node.
      /// </summary>
      property NodeEvent      : TGIS_OnNodeEvent      read  FOnNodeEvent
                                                      write FOnNodeEvent ;
      /// <event/>
      /// <summary>
      ///   Event fired for each way.
      /// </summary>
      property WayEvent       : TGIS_OnWayEvent       read  FOnWayEvent
                                                      write FOnWayEvent ;
      /// <event/>
      /// <summary>
      ///   Event fired for each relation.
      /// </summary>
      property RelationEvent  : TGIS_OnRelationEvent  read  FOnRelationEvent
                                                      write FOnRelationEvent ;
      /// <event/>
      /// <summary>
      ///   Event fired for extent.
      /// </summary>
      property ExtentEvent    : TGIS_OnExtentEvent    read  FOnExtentEvent
                                                      write FOnExtentEvent ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisFunctions,
    GisInternals,
    GisCompression ;
{$ENDIF}

//=============================================================================
// TGIS_OSMNode
//=============================================================================

  constructor TGIS_OSMNode.Create ;
  begin
    inherited ;

    attrs := TGIS_KeyValueList<String,String>.Create ;
    {$IFDEF GIS_NORECORDS}
      ptg := new TGIS_Point ;
    {$ENDIF}
  end ;

  {$IFNDEF OXYGENE}
  destructor TGIS_OSMNode.Destroy ;
  begin
    FreeObject( attrs ) ;

    inherited ;
  end ;
  {$ENDIF}

//=============================================================================
// TGIS_OSMWay
//=============================================================================

  constructor TGIS_OSMWay.Create ;
  begin
    inherited ;

    attrs := TGIS_KeyValueList<String,String>.Create ;
    refs  := {$IFDEF JAVA} TList {$ELSE} TList {$ENDIF}<Int64>.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor TGIS_OSMWay.Destroy;
  begin
    FreeObject( attrs ) ;
    FreeObject( refs  ) ;

    inherited ;
  end ;
  {$ENDIF}

//=============================================================================
// TGIS_OSMRelation
//=============================================================================

  constructor TGIS_OSMRelation.Create ;
  begin
    inherited ;

    attrs   := TGIS_KeyValueList<String,String>.Create ;
    members := TList<TGIS_OSMMember>.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor TGIS_OSMRelation.Destroy ;
  var
    i : Integer ;
  begin
    for i := 0 to members.Count-1 do
      FreeObjectNotNil( TGIS_OSMMember(members[i]) ) ;
    FreeObject( attrs   ) ;
    FreeObject( members ) ;

    inherited ;
  end ;
  {$ENDIF}

//=============================================================================
// TGIS_OSMMember
//=============================================================================

  constructor TGIS_OSMMember.Create(
    const _id     : Int64 ;
    const _mtype  : TGIS_OSMMemberType ;
    const _role   : String
  ) ;
  begin
    inherited Create ;

    id    := _id ;
    mtype := _mtype ;
    role  := _role ;
  end ;

  constructor TGIS_OSMMember.Create ;
  begin
    inherited ;
  end ;

//=============================================================================
// TGIS_PBFParser
//=============================================================================


  constructor TGIS_PBFParser.Create ;
  begin
    inherited ;

    osmnode     := TGIS_OSMNode.Create ;
    osmway      := TGIS_OSMWay.Create ;
    osmrelation := TGIS_OSMRelation.Create ;
    stringtable := TStringList.Create ;
  end ;

  {$IFNDEF OXYGENE}
  destructor TGIS_PBFParser.Destroy ;
  begin
    FreeObject( osmnode     ) ;
    FreeObject( osmway      ) ;
    FreeObject( osmrelation ) ;
    FreeObject( stringtable ) ;

    inherited ;
  end ;
  {$ENDIF}


  procedure TGIS_PBFParser.Parse(
    const _path : String
  ) ;
  var
    ret : Boolean ;
  begin
    fs := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
    try
      while True do begin
        ret := processBlock ;
        if not ret then break ;
      end ;
    finally
      FreeObject( fs ) ;
    end ;
  end ;

  function TGIS_PBFParser.readFSBuffer(
    const _buf : TBytes ;
    const _len : Integer
  ) : Integer ;
  begin
    {$IFDEF OXYGENE}
      Result := fs.Read( _buf, _len ) ;
    {$ELSE}
      Result := fs.Read( _buf[0], _len ) ;
    {$ENDIF}
  end ;

  function TGIS_PBFParser.processBlock : Boolean ;
  var
    blob    : TBytes ;
    hdr     : TBytes ;
    hdrsize : Cardinal ;
    bsize   : Cardinal ;
    btype   : Integer ;
  begin
    Result := False ;

    SetLength( hdr, 4 ) ;
    if readFSBuffer( hdr, 4 ) <> 4 then exit ;

    hdrsize := (hdr[0] shl 24) or (hdr[1] shl 16) or (hdr[2] shl 8) or hdr[3] ;

    SetLength( blob, 64*1024+1 ) ;
    readFSBuffer( blob, hdrsize ) ;

    if not readBlobHeader( blob, hdrsize, bsize, btype ) then exit ;

    if Cardinal( length( blob )  ) < bsize then
      SetLength( blob, bsize+1 ) ;

    readFSBuffer( blob, bsize ) ;

    Result := readBlob( blob, bsize, bsize, btype ) ;
  end ;

  function TGIS_PBFParser.readBlobHeader(
    const _data     : TBytes ;
    const _dataSize : Integer ;
      var _blobSize : Cardinal ;
      var _blobType : Integer
  ) : Boolean ;
  var
    key     : Integer ;
    datalen : Cardinal ;
    sbuf    : TBytes ;
    str     : String ;
    gpb     : TGIS_GPBReader ;
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      _blobSize := 0 ;
      _blobType := BLOB_UNKNOWN ;

      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        if ( key = gpb.MakeKey( BLOBHEADER_IDX_TYPE, gpb.WT_DATA ) ) then begin
          datalen := gpb.ReadSize ;

          SetLength( sbuf, datalen ) ;
          gpb.readBuffer( sbuf, 0, datalen ) ;
          str := TEncoding.UTF8.GetString( sbuf, 0, datalen ) ;

          if str = 'OSMData' then
            _blobType := BLOB_OSMDATA
          else if str = 'OSMHeader' then
            _blobType := BLOB_OSMHEADER ;
        end
        else if (key = gpb.MakeKey( BLOBHEADER_IDX_INDEXDATA, gpb.WT_DATA ) ) then begin
          datalen := gpb.ReadSize ;
          gpb.skipBytes( datalen ) ;
        end
        else if (key = gpb.MakeKey( BLOBHEADER_IDX_DATASIZE, gpb.WT_VARINT ) ) then
          _blobSize := gpb.readVarUInt32
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;
      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;

  end ;


  function TGIS_PBFParser.readBlob(
    const _data      : TBytes ;
    const _dataSize  : Integer ;
    const _blobSize  : Cardinal ;
    const _blobType  : Integer
  ) : Boolean ;
  var
    usize     : Cardinal ;
    dlen      : Cardinal ;
    zlibcsize : Cardinal ;
    key       : Integer ;
    olen      : Integer ;
    ubuf      : TBytes ;
    cbuf      : TBytes ;
    gpb       : TGIS_GPBReader ;
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      Result := True ;
      usize  := 0 ;
      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        if ( key = gpb.MakeKey( BLOB_IDX_RAW, gpb.WT_DATA ) ) then begin

          dlen := gpb.ReadSize ;
          SetLength( cbuf, dlen ) ;
          gpb.readBuffer( cbuf, 0, dlen ) ;

          if ( _blobType = BLOB_OSMHEADER ) then
            Result := readOSMHeader( cbuf, dlen )
          else if ( _blobType = BLOB_OSMDATA ) then
            Result := readPrimitiveBlock( cbuf, dlen ) ;
        end
        else if ( key = gpb.MakeKey( BLOB_IDX_RAW_SIZE, gpb.WT_VARINT ) ) then
          usize := gpb.readVarUInt32
        else if ( key = gpb.MakeKey(BLOB_IDX_ZLIB_DATA, gpb.WT_DATA ) ) then begin
          zlibcsize := gpb.readVarUInt32 ;

          if (usize <> 0) and (zlibcsize <> 0) then begin
            SetLength( cbuf, zlibcsize ) ;
            gpb.readBuffer( cbuf, 0, zlibcsize ) ;
            DecompressDeflateBuffer(cbuf, 0, zlibcsize, ubuf, olen ) ;

            if (_blobType = BLOB_OSMHEADER) then
              Result := readOSMHeader( ubuf, olen )
            else if (_blobType = BLOB_OSMDATA) then
              Result := readPrimitiveBlock( ubuf, olen ) ;
          end ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;


  function TGIS_PBFParser.readOSMHeader(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    str       : String ;
    key       : Integer ;
    bboxsize  : Cardinal ;
    cbuf      : TBytes ;
    gpb       : TGIS_GPBReader ;
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        if ( key = gpb.MakeKey( OSMHEADER_IDX_BBOX, gpb.WT_DATA ) ) then begin
          bboxsize := gpb.ReadSize ;
          SetLength( cbuf, bboxsize ) ;
          gpb.readBuffer( cbuf, 0, bboxsize ) ;

          Result := readHeaderBBox( cbuf, bboxsize ) ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_REQUIRED_FEATURES, gpb.WT_DATA)) then begin
          str := gpb.ReadText ;
          assert( (str='OsmSchema-V0.6') or (str='DenseNodes'),'unsupported required feature' ) ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_OPTIONAL_FEATURES, gpb.WT_DATA)) then begin
          str := gpb.ReadText ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_WRITING_PROGRAM, gpb.WT_DATA)) then begin
          str := gpb.ReadText ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_SOURCE, gpb.WT_DATA)) then begin
          str := gpb.ReadText ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_OSMOSIS_REPLICATION_TIMESTAMP, gpb.WT_VARINT)) then begin
          gpb.skipVarInt64 ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_OSMOSIS_REPLICATION_SEQ_NUMBER, gpb.WT_VARINT)) then begin
          gpb.skipVarInt64 ;
        end
        else if (key = gpb.MakeKey(OSMHEADER_IDX_OSMOSIS_REPLICATION_BASE_URL, gpb.WT_DATA)) then begin
          str := gpb.ReadText ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;
      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;

  end ;


  function TGIS_PBFParser.readHeaderBBox(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    left,
    right,
    top,
    bottom  : Double ;
    key     : Integer ;
    ival    : Int64 ;
    gpb     : TGIS_GPBReader ;
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      left    := 0.0 ;
      right   := 0.0 ;
      top     := 0.0 ;
      bottom  := 0.0 ;

      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        if ( key = gpb.MakeKey( HEADERBBOX_IDX_LEFT, gpb.WT_VARINT ) ) then begin
          ival := gpb.readVarSInt64 ;
          left := ival * 1e-9 ;
        end
        else if (key = gpb.MakeKey( HEADERBBOX_IDX_RIGHT, gpb.WT_VARINT )) then begin
          ival := gpb.readVarSInt64 ;
          right := ival * 1e-9 ;
        end
        else if (key = gpb.MakeKey( HEADERBBOX_IDX_TOP, gpb.WT_VARINT )) then begin
          ival := gpb.readVarSInt64 ;
          top := ival * 1e-9 ;
        end
        else if (key = gpb.MakeKey( HEADERBBOX_IDX_BOTTOM, gpb.WT_VARINT )) then begin
          ival := gpb.readVarSInt64 ;
          bottom := ival * 1e-9 ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;
      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;

    if assigned( FOnExtentEvent ) then
      FOnExtentEvent( GisExtent( left, bottom, right, top ) ) ;
  end ;

  function TGIS_PBFParser.readPrimitiveBlock(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key       : Integer ;
    oldPos    : int64_t ;
    size      : Cardinal ;
    gpb       : TGIS_GPBReader ;
    cbuf      : TBytes ;
  begin
    granularity     := 100 ;
    dateGranularity := 1000 ;
    latOffset       := 0 ;
    lonOffset       := 0 ;

    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      oldPos := gpb.Position ;
      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey(PRIMITIVEBLOCK_IDX_GRANULARITY, gpb.WT_VARINT)) then
          granularity := gpb.readVarInt32
        else if (key = gpb.MakeKey(PRIMITIVEBLOCK_IDX_DATE_GRANULARITY, gpb.WT_VARINT)) then
          dateGranularity := gpb.readVarInt32
        else if (key = gpb.MakeKey(PRIMITIVEBLOCK_IDX_LAT_OFFSET, gpb.WT_VARINT)) then
          latOffset := gpb.readVarInt64
        else if (key = gpb.MakeKey(PRIMITIVEBLOCK_IDX_LON_OFFSET, gpb.WT_VARINT)) then
          lonOffset := gpb.readVarInt64
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      gpb.Position := oldPos ;

      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey(PRIMITIVEBLOCK_IDX_STRINGTABLE, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          SetLength( cbuf, size ) ;
          gpb.readBuffer( cbuf, 0, size ) ;
          Result := readStringTable( cbuf, size ) ;
        end
        else if (key = gpb.MakeKey(PRIMITIVEBLOCK_IDX_PRIMITIVEGROUP, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;

          SetLength( cbuf, size ) ;
          gpb.readBuffer( cbuf, 0, size ) ;
          Result := readPrimitiveGroup( cbuf, size ) ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;
      // notifyNodes ....
      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;

  function TGIS_PBFParser.readPrimitiveGroup(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key     : Integer ;
    fieldno : Integer ;
    size    : Cardinal ;
    gpb     : TGIS_GPBReader ;
    cbuf    : TBytes ;
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      while ( gpb.Position < _dataSize ) do begin

        key := gpb.ReadFieldKey ;

        fieldno := gpb.GetFieldNumber( key ) - 1 ;

        if ( gpb.GetWireType( key ) = gpb.WT_DATA ) and
           ( fieldno >= PRIMITIVEGROUP_IDX_NODES - 1     ) and
           ( fieldno <= PRIMITIVEGROUP_IDX_RELATIONS - 1 ) then begin

           size := gpb.ReadSize ;
           SetLength( cbuf, size ) ;
           gpb.readBuffer( cbuf, 0, size ) ;

           case fieldno of
              0 : Result := readNode( cbuf, size  ) ;
              1 : Result := readDenseNodes( cbuf, size ) ;
              2 : Result := readWay( cbuf, size ) ;
              3 : Result := readRelation( cbuf, size ) ;
           end ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;

  function TGIS_PBFParser.readStringTable(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key     : Integer ;
    gpb     : TGIS_GPBReader ;
    str     : String ;
  begin
    stringtable.Clear ;

    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      while ( gpb.Position < _dataSize ) do begin
        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey( READSTRINGTABLE_IDX_STRING, gpb.WT_DATA )) then begin
           str := gpb.ReadText ;
           stringtable.Add( str ) ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;

  function TGIS_PBFParser.readNode(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key     : Integer ;
    ukey    : Cardinal ;
    size    : Cardinal ;
    val     : Cardinal ;
    gpb     : TGIS_GPBReader ;
    id      : Int64 ;
    ilat    : Int64 ;
    ilon    : Int64 ;
    lat     : Double ;
    lon     : Double ;
    tags    : Integer ;
    i       : Integer ;
    cbuf    : TBytes ;
    off     : Cardinal ;
    {$IFDEF DCC}
      kvpair  : TPair<String,String> ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      kvpair  : TPair<String,String> ;
    {$ENDIF}
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      tags := 0 ;
      while ( gpb.Position < _dataSize ) do begin
        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey( NODE_IDX_ID, gpb.WT_VARINT )) then begin
          id := gpb.readVarSInt64 ;
          osmnode.id := id ;
        end
        else if (key = gpb.MakeKey(NODE_IDX_LAT, gpb.WT_VARINT)) then begin
          ilat := gpb.readVarSInt64 ;
          lat := 0.000000001 * (latOffset + (granularity * ilat)) ;
          osmnode.ptg.Y := lat ;
        end
        else if (key = gpb.MakeKey(NODE_IDX_LON, gpb.WT_VARINT)) then begin
          ilon := gpb.readVarSInt64 ;
          lon := 0.000000001 * (lonOffset + (granularity * ilon)) ;
          osmnode.ptg.X := lon ;
        end
        else if (key = gpb.MakeKey(NODE_IDX_KEYS, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          off := gpb.Position+size ;
          tags := 0 ;
          osmnode.attrs.Clear ;

          while (gpb.Position < off ) do begin
            ukey := gpb.readVarUInt32 ;

            osmnode.attrs.Add( stringtable[ukey], '' ) ;
            inc( tags ) ;
          end ;
        end
        else if (key = gpb.MakeKey(NODE_IDX_VALS, gpb.WT_DATA)) then begin
          gpb.skipVarInt64 ;

          for i := 0 to tags-1 do begin
            val := gpb.readVarUInt32 ;

            kvpair := osmnode.attrs.Items[i]  ;
            {$IFDEF OXYGENE}
            kvpair := new TPair<String,String>(kvpair.Key, stringtable[val]) ;
            {$ELSE}
            kvpair.Value := stringtable[val] ;
            {$ENDIF}
            osmnode.attrs.Items[i] := kvpair ;
          end ;
        end
        else if (key = gpb.MakeKey(NODE_IDX_INFO, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          SetLength( cbuf, size ) ;
          gpb.readBuffer( cbuf, 0, size ) ;
          //Result := readOSMInfo( cbuf, size ) ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      if assigned( FOnNodeEvent ) then
        FOnNodeEvent( osmnode ) ;

      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;


  function TGIS_PBFParser.readDenseNodes(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key             : Integer ;
    ukey            : Cardinal ;
    size            : Cardinal ;
    val             : Cardinal ;
    gpb             : TGIS_GPBReader ;
    id              : Int64 ;
    ilat            : Int64 ;
    ilon            : Int64 ;
    lat             : Double ;
    lon             : Double ;
    i               : Integer ;
    cbuf            : TBytes ;
    off             : Cardinal ;
    fieldNumber     : Integer ;
    pdata_ids       : Cardinal ;
    pdata_idslimit  : Cardinal ;
    pdata_lat       : Cardinal ;
    pdata_lon       : Cardinal ;
    pdata_keyval    : Cardinal ;
    pdata           : array[0..DENSEINFO_IDX_VISIBLE-1] of TGIS_GPBReader ;

    itimestamp      : Int64 ;
    ichangeset      : Int64 ;
    idelta1         : Int64 ;
    idelta2         : Int64 ;
    uid             : Integer ;
    usersid         : Cardinal ;
    nodes           : Integer ;
    version         : Integer ;
    deltauid        : Integer ;
    deltausersid    : Integer ;

    pdata_version   : TGIS_GPBReader ;
    pdata_timestamp : TGIS_GPBReader ;
    pdata_changeset : TGIS_GPBReader ;
    pdata_uid       : TGIS_GPBReader ;
    pdata_usersid   : TGIS_GPBReader ;

  begin
    pdata_ids       := 0 ;
    pdata_idslimit  := 0 ;
    pdata_lat       := 0 ;
    pdata_lon       := 0 ;
    pdata_keyval    := 0 ;
    for i := 0 to DENSEINFO_IDX_VISIBLE-1 do
      pdata[i] := nil ;

    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      while ( gpb.Position < _dataSize ) do begin
        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey(DENSENODES_IDX_ID, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          pdata_ids := gpb.Position;
          pdata_idslimit := gpb.Position + size ;
          gpb.Position := gpb.Position + size ;
        end
        else if (key = gpb.MakeKey( DENSENODES_IDX_DENSEINFO, gpb.WT_DATA )) then begin
          size := gpb.ReadSize ;

          off := gpb.Position+size ;

          while (gpb.Position < off ) do begin
            ukey := gpb.ReadFieldKey ;
            fieldNumber := gpb.GetFieldNumber( ukey ) - 1 ;

            if ( gpb.GetWireType( ukey ) = gpb.WT_DATA ) and
               ( fieldNumber >= DENSEINFO_IDX_VERSION     ) and
               ( fieldNumber <= DENSEINFO_IDX_VISIBLE ) then begin

               size := gpb.ReadSize ;
               cbuf := nil ;
               SetLength( cbuf, size ) ;
               gpb.readBuffer( cbuf, 0, size ) ;
               pdata[fieldNumber - 1] := TGIS_GPBReader.Create( cbuf, size ) ;
            end
            else
              gpb.SkipUnknownField( ukey, True ) ;
          end ;
        end
        else if (key = gpb.MakeKey(DENSENODES_IDX_LAT, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          pdata_lat := gpb.Position ;
          gpb.Position := gpb.Position + size ;
        end
        else if (key = gpb.MakeKey(DENSENODES_IDX_LON, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          pdata_lon := gpb.Position ;
          gpb.Position := gpb.Position + size ;
        end
        else if (key = gpb.MakeKey(DENSENODES_IDX_KEYVALS, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          pdata_keyval := gpb.Position ;
          gpb.Position := gpb.Position + size ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      if (pdata_ids > 0) and (pdata_lon > 0) and (pdata_lat > 0) then begin
        itimestamp    := 0 ;
        ichangeset    := 0 ;
        uid           := 0 ;
        id            := 0 ;
        ilat          := 0 ;
        ilon          := 0 ;
        usersid       := 0 ;
        nodes         := 0 ;
        version       := 0 ;
        deltauid      := 0 ;
        deltausersid  := 0 ;

        pdata_version   := pdata[DENSEINFO_IDX_VERSION - 1];
        pdata_timestamp := pdata[DENSEINFO_IDX_TIMESTAMP - 1];
        pdata_changeset := pdata[DENSEINFO_IDX_CHANGESET - 1];
        pdata_uid       := pdata[DENSEINFO_IDX_UID - 1];
        pdata_usersid   := pdata[DENSEINFO_IDX_USER_SID - 1];

        gpb.Position := int64_t(pdata_ids) ;

        while ( pdata_ids < pdata_idslimit ) do begin
          gpb.Position := int64_t(pdata_ids) ;
          idelta1 := gpb.readVarSInt64 ;
          pdata_ids := gpb.Position ;

          id := id + idelta1;
          osmnode.id := id ;

          gpb.Position := int64_t(pdata_lat) ;
          idelta2 := gpb.readVarSInt64 ;
          pdata_lat := gpb.Position ;
          ilat := ilat + idelta2 ;

          gpb.Position := int64_t(pdata_lon) ;
          idelta1 := gpb.readVarSInt64 ;
          pdata_lon := gpb.Position ;
          ilon := ilon + idelta1 ;

          if ( pdata_timestamp <> nil ) then begin
            idelta2 := pdata_timestamp.readVarSInt64 ;
            itimestamp := itimestamp + idelta2 ;
          end ;

          if ( pdata_changeset <> nil ) then begin
            idelta1 := pdata_changeset.readVarSInt64 ;
            ichangeset := ichangeset + idelta1 ;
          end ;

          if ( pdata_version <> nil ) then
            version := pdata_version.readVarInt32 ;

          if ( pdata_uid <> nil ) then begin
            deltauid := pdata_uid.readVarSInt32 ;
            uid := uid + deltauid ;
          end ;

          if ( pdata_usersid <> nil ) then begin
            deltausersid := pdata_usersid.readVarSInt32 ;
            usersid := usersid + Cardinal(deltausersid) ;
          end ;

          osmnode.attrs.Clear ;

          if ( pdata_keyval > 0 ) then begin
            gpb.Position := int64_t(pdata_keyval) ;
            while True do begin

              key  := gpb.readVarUInt32 ;
              pdata_keyval := gpb.Position ;
              if key = 0 then break ;

              val  := gpb.readVarUInt32 ;
              pdata_keyval := gpb.Position ;

              osmnode.attrs.Add( stringtable[key], stringtable[val] ) ;
            end ;
          end ;

          lat := 0.000000001 * (latOffset + (granularity * ilat)) ;
          lon := 0.000000001 * (latOffset + (granularity * ilon)) ;
          inc( nodes ) ;

          osmnode.ptg.X := lon ;
          osmnode.ptg.Y := lat ;

          if assigned( FOnNodeEvent ) then
            FOnNodeEvent( osmnode ) ;
        end ;
      end ;

      Result := pdata_ids = pdata_idslimit ;
    finally
      for i := 0 to DENSEINFO_IDX_VISIBLE-1 do
        if pdata[i] <> nil then
           FreeObject( pdata[i] );
      FreeObject( gpb ) ;
    end ;
  end ;


  function TGIS_PBFParser.readWay(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key     : Integer ;
    ukey    : Cardinal ;
    size    : Cardinal ;
    val     : Cardinal ;
    gpb     : TGIS_GPBReader ;
    id      : Int64 ;
    tags    : Integer ;
    refs    : Integer ;
    i       : Integer ;
    cbuf    : TBytes ;
    off     : Cardinal ;
    deltaref: Int64 ;
    refval  : Int64 ;
    {$IFDEF DCC}
      kvpair  : TPair<String,String> ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      kvpair  : TPair<String,String> ;
    {$ENDIF}
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      tags := 0 ;
      id := 0 ;
      while ( gpb.Position < _dataSize ) do begin
        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey( WAY_IDX_ID, gpb.WT_VARINT )) then begin
          id := gpb.readVarInt64 ;
        end
        else if (key = gpb.MakeKey(WAY_IDX_KEYS, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          off := gpb.Position+size ;
          tags := 0 ;
          osmway.attrs.Clear ;
          while (gpb.Position < off ) do begin
            ukey := gpb.readVarUInt32 ;
            osmway.attrs.Add( stringtable[ukey], '' ) ;
            inc( tags ) ;
          end ;
        end
        else if (key = gpb.MakeKey(WAY_IDX_VALS, gpb.WT_DATA)) then begin
          gpb.skipVarInt64 ;

          for i := 0 to tags-1 do begin
            val := gpb.readVarUInt32 ;
            kvpair := osmway.attrs.Items[ i ] ;
            {$IFDEF OXYGENE}
            kvpair := new TPair<String,String>(kvpair.Key, stringtable[val]) ;
            {$ELSE}
            kvpair.Value := stringtable[val] ;
            {$ENDIF}
            osmway.attrs.Items[ i ] := kvpair ;
          end ;
        end
        else if (key = gpb.MakeKey(WAY_IDX_INFO, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          SetLength( cbuf, size ) ;
          gpb.readBuffer( cbuf, 0, size ) ;
          //Result := readOSMInfo( cbuf, size ) ;
        end
        else if (key = gpb.MakeKey(WAY_IDX_REFS, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          off := gpb.Position+size ;
          refval := 0 ;
          refs := 0 ;
          osmway.refs.Clear ;
          while (gpb.Position < off ) do begin
            deltaref := gpb.readVarSInt64 ;
            refval := refval + deltaref ;
            osmway.refs.Add( refval ) ;
            inc( refs ) ;
          end ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      osmway.id := id ;
      if assigned( FOnWayEvent ) then
        FOnWayEvent( osmway ) ;

      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;

  function TGIS_PBFParser.readRelation(
    const _data     : TBytes ;
    const _dataSize : Integer
  ) : Boolean ;
  var
    key       : Integer ;
    ukey      : Cardinal ;
    size      : Cardinal ;
    val       : Cardinal ;
    gpb       : TGIS_GPBReader ;
    id        : Int64 ;
    tags      : Integer ;
    members   : Integer ;
    i         : Integer ;
    cbuf      : TBytes ;
    off       : Cardinal ;
    rolesid   : Cardinal ;
    deltaref  : Int64 ;
    refval    : Int64 ;
    {$IFDEF DCC}
      kvpair  : TPair<String,String> ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      kvpair  : TPair<String,String> ;
    {$ENDIF}
  begin
    gpb := TGIS_GPBReader.Create( _data, _dataSize ) ;
    try
      tags := 0 ;
      id := 0 ;
      members := 0 ;
      while ( gpb.Position < _dataSize ) do begin
        key := gpb.ReadFieldKey ;

        if (key = gpb.MakeKey( RELATION_IDX_ID, gpb.WT_VARINT )) then begin
          id := gpb.readVarInt64 ;
        end
        else if (key = gpb.MakeKey(RELATION_IDX_KEYS, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          off := gpb.Position+size ;
          tags := 0 ;
          osmrelation.attrs.Clear ;
          while (gpb.Position < off ) do begin
            ukey := gpb.readVarUInt32 ;
            osmrelation.attrs.Add( stringtable[ukey], '' ) ;
            inc( tags ) ;
          end ;
        end
        else if (key = gpb.MakeKey(RELATION_IDX_VALS, gpb.WT_DATA)) then begin
          gpb.skipVarInt64 ;

          for i := 0 to tags-1 do begin
            val := gpb.readVarUInt32 ;
            kvpair := osmrelation.attrs.Items[ i ] ;
            {$IFDEF OXYGENE}
            kvpair := new TPair<String,String>(kvpair.Key, stringtable[val]) ;
            {$ELSE}
            kvpair.Value := stringtable[val] ;
            {$ENDIF}
            osmrelation.attrs.Items[ i ] := kvpair ;
          end ;
        end
        else if (key = gpb.MakeKey(RELATION_IDX_INFO, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          SetLength( cbuf, size ) ;
          gpb.readBuffer( cbuf, 0, size ) ;
          //Result := readOSMInfo( cbuf, size ) ;
        end
        else if (key = gpb.MakeKey(RELATION_IDX_ROLES_SID, gpb.WT_DATA)) then begin
          size := gpb.ReadSize ;
          off := gpb.Position+size ;

          members := 0 ;
          for i := 0 to osmrelation.members.Count-1 do
            FreeObjectNotNil( osmrelation.members[i] ) ;
          osmrelation.members.Clear ;
          while (gpb.Position < off ) do begin
            rolesid := gpb.readVarUInt32 ;
            osmrelation.members.Add(
              TGIS_OSMMember.Create(
                0,
                TGIS_OSMMemberType.node,
                stringtable[rolesid]
              )
            ) ;
            inc( members ) ;
          end ;
        end
        else if (key = gpb.MakeKey(RELATION_IDX_MEMIDS, gpb.WT_DATA)) then begin
          gpb.skipVarInt64 ;
          refval := 0 ;
          for i := 0 to members-1 do begin
            deltaref := gpb.readVarSInt64 ;
            refval := refval + deltaref ;
            osmrelation.members[ i ].id := refval ;
          end ;
        end
        else if (key = gpb.MakeKey(RELATION_IDX_TYPES, gpb.WT_DATA)) then begin
          gpb.ReadSize ;

          for i := 0 to members-1 do
            osmrelation.members[ i ].mtype := TGIS_OSMMemberType(gpb.readByte) ;
        end
        else
          gpb.SkipUnknownField( key, True ) ;
      end ;

      osmrelation.id := id ;
      if assigned( FOnRelationEvent ) then
        FOnRelationEvent( osmrelation ) ;

      Result := gpb.Position = _dataSize ;
    finally
      FreeObject( gpb ) ;
    end ;
  end ;


end.
