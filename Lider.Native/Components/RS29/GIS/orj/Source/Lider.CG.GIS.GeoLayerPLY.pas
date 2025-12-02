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
  Encapsulation of PLY file format known as the Polygon File Format.
}

{$IFDEF DCC}
  unit GisLayerPLY ;
  {$HPPEMIT '#pragma link "GisLayerPLY"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    System.Classes,

    System.Generics.Collections,

    GisClasses,
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

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerPLY = class
    public
      class procedure SelfRegisterLayer() ;
  end ;


  /// <summary>
  ///   Layer which can read a PLY file.
  /// </summary>
  TGIS_LayerPLY = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      FVertexOnly : Boolean ;
      bbox        : TGIS_Extent3D ;
      offset      : TGIS_Point3D ;
    private
      procedure buildModel( const _parser : TObject ) ;
      procedure checkBox ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

      procedure fset_UseRTree         ( const _value : Boolean
                                      ) ; override;

     /// <inheritdoc/>
      procedure setUp ; override;

     // cursor access function(s)
    protected
      // destructors

      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create ; override;

      /// <inheritdoc/>
      function  DrawEx            ( const _extent : TGIS_Extent
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  PreRecognize      ( const _path   : String ;
                                    var _new_path : String
                                  ) : Boolean ; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.IOUtils,
    GisRtl,
    GisTypesUI,
    GisFunctions,
    GisInternals,
    GisInterfaces,
    GisLayer,
    GisShapes3D,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

type

  T_PlyDataType = (
    // UNKNOWN.
    UNKNOWN,
    // One byte signed integer.
    CHAR,
    // One byte unsigned integer.
    UCHAR,
    // Two byte signed integer.
    SHORT,
    // Two byte unsigned integer.
    USHORT,
    // Four byte signed integer.
    INT,
    // Four byte unsigned integer.
    UINT,
    // four byte floating point number.
    FLOAT,
    // Eight byte byte floating point number.
    DOUBLE
  ) ;

  T_PlyFormat = (
    // UNKNOWN.
    UNKNOWN,
    // Data is stored in ASCII format.
    ASCII,
    // Data is stored in little endian binary format.
    BINARY_LITTLE_ENDIAN,
    // Data is stored in big endian binary format.
    BINARY_BIG_ENDIAN
  ) ;

  T_PlyProperty = class
    public
      name          : String ;
      dataType      : T_PlyDataType ;
      countDataType : T_PlyDataType ;
      dataIndex     : Integer ;
      dataCount     : Integer ;
    public
      constructor Create ; overload;
      constructor Create( const _name          : String ;
                          const _dataType      : T_PlyDataType
                         ) ; overload;
      constructor Create( const _name          : String ;
                          const _dataType      : T_PlyDataType ;
                          const _countDataType : T_PlyDataType 
                         ) ; overload;
      procedure SetDataIndex( const _idx : Integer
                             ) ;
      procedure SetDataCount( const _count : Integer
                             ) ;
  end ;

  T_PlyData = array of TGIS_DoubleArray ;

  T_PlyElement = class( TGIS_ObjectDisposable )
    public
      name       : String ;
      count      : Integer ;
      properties : TList<T_PlyProperty> ;
      data       : T_PlyData ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ; overload;
      constructor Create( const _name  : String ;
                          const _count : Integer
                         ) ; overload;
      procedure AddProperty( const _property : T_PlyProperty
                            ) ;
  end ;

  T_PlyParser = class( TGIS_ObjectDisposable )
    private
      plyFile     : TGIS_BufferedFileStream ;
      strLine     : String ;
      curElement  : T_PlyElement ;
      curProperty : T_PlyProperty ;
      oLayer      : TGIS_Layer ;
      tkn         : TGIS_Tokenizer ;
      buf         : array[0..7] of Byte ;

    private
      procedure readLine ;
      procedure readHeader ;
      procedure readElements ;
      function  readBinary        ( const _dataType  : T_PlyDataType
                                   ) : Double ;
      procedure readElementAscii  ( const _elm       : T_PlyElement
                                   ) ;
      procedure readElementBinary ( const _elm       : T_PlyElement
                                   ) ;
      function  parseProperty     ( const _property  : String
                                   ) : T_PlyProperty ;
      function  parseDataType     ( const _typeName  : String
                                   ) : T_PlyDataType ;
      function  parseFormat       ( const _format    : String
                                   ) : T_PlyFormat ;
      function  parseElement      ( const _element   : String
                                   ) : T_PlyElement ;
      function  parseComment      ( const _comment   : String
                                   ) : String ;
    protected
      procedure doDestroy ; override;
    public
      elements : TList<T_PlyElement> ;
      format   : T_PlyFormat ;
      comments : TStringList ;

      constructor Create( const _path   : String ;
                          const _layer  : TGIS_Layer
                         ) ;
      procedure Parse ;
      function  ParamFromHeader( const _name : String
                               ) : Double ;
  end ;

  function starts_with( const _text     : String ;
                        const _subtext  : String
                       ) : Boolean ;
  begin
    {$IFDEF DCC}
      {$IFDEF LEVEL_XE3_RTL}
        Result := _text.StartsWith( _subtext ) ;
      {$ELSE}
        Result := StartsText( _subtext, _text ) ;
      {$ENDIF}
    {$ENDIF}
    {$IFDEF OXYGENE}
      Result := _text.StartsWith( _subtext ) ;
    {$ENDIF}
  end ;

//=============================================================================
// T_PlyProperty
//=============================================================================

  constructor T_PlyProperty.Create ;
  begin
    inherited  ;

    name          := '' ;
    dataType      := T_PlyDataType.UNKNOWN ;
    countDataType := T_PlyDataType.UNKNOWN ;
  end ;

  constructor T_PlyProperty.Create(
    const _name     : String;
    const _dataType : T_PlyDataType
  );
  begin
    inherited Create ;

    name          := _name ;
    dataType      := _dataType ;
    countDataType := T_PlyDataType.UNKNOWN ;
  end ;

  constructor T_PlyProperty.Create(
    const _name           :  String;
    const _dataType       : T_PlyDataType ;
    const _countDataType  : T_PlyDataType
  );
  begin
    inherited Create ;

    name          := _name ;
    dataType      := _dataType ;
    countDataType := _countDataType ;
  end ;

  procedure T_PlyProperty.SetDataCount(
    const _count : Integer
  ) ;
  begin
    dataCount := _count ;
  end ;

  procedure T_PlyProperty.SetDataIndex(
    const _idx : Integer
  ) ;
  begin
    dataIndex := _idx ;
  end ;

//=============================================================================
// T_PlyElement
//=============================================================================

  constructor T_PlyElement.Create ;
  begin
    inherited ;

    name       := '' ;
    count      := 0 ;
    properties := TList<T_PlyProperty>.Create ;
  end ;

  constructor T_PlyElement.Create(
    const _name  : String ;
    const _count : Integer
  ) ;
  begin
    inherited Create ;

    name       := _name ;
    count      := _count ;
    properties := TList<T_PlyProperty>.Create ;
  end ;

  procedure T_PlyElement.doDestroy;
  {$IFDEF DCC}
    var
      prop : T_PlyProperty ;
  {$ENDIF}
  begin
    for prop in properties do
      FreeObjectNotNil( prop ) ;

    FreeObject( properties ) ;
    data := nil ;

    inherited;
  end ;

  procedure T_PlyElement.AddProperty(
    const _property : T_PlyProperty
  ) ;
  begin
    properties.Add( _property ) ;
  end ;

//=============================================================================
// T_PlyParser
//=============================================================================

  constructor T_PlyParser.Create(
    const _path   : String ;
    const _layer  : TGIS_Layer
  ) ;
  begin
    inherited Create ;

    plyFile  := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
    elements := TList<T_PlyElement>.Create ;
    format   := T_PlyFormat.UNKNOWN ;
    comments := TStringList.Create ;
    oLayer   := _layer ;
    tkn      := TGIS_Tokenizer.Create ;
  end ;

  procedure T_PlyParser.doDestroy;
  {$IFDEF DCC}
    var
      elm : T_PlyElement ;
  {$ENDIF}
  begin
    for elm in elements do
      FreeObjectNotNil( elm ) ;

    FreeObject( tkn      ) ;
    FreeObject( elements ) ;
    FreeObject( comments ) ;
    FreeObject( plyFile  ) ;

    oLayer      := nil ;
    curElement  := nil ;
    curProperty := nil ;

    inherited;
  end ;

  procedure T_PlyParser.readLine;
  begin
    strLine := plyFile.ReadLine ;
  end ;

  procedure T_PlyParser.readHeader;
  begin
    curElement := nil ;

    readLine ;
    strLine := Trim( strLine ) ;
    if strLine <> 'ply' then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERBADFORMAT ) +
                                   '; ' + strLine,
                                   plyFile.Path, 101
                                 ) ;
    while (strLine <> 'end_header') and not plyFile.Eof do begin
      readLine ;
      strLine := TrimLeft( strLine ) ;

      if starts_with( strLine, 'format' ) then
        format := parseFormat( strLine )
      else if starts_with( strLine, 'comment' ) then
        comments.Add( parseComment( strLine ) )
      else if starts_with( strLine, 'obj_info' ) then
        comments.Add( parseComment( strLine ) )
      else if starts_with( strLine, 'element' ) then begin
        curElement := parseElement( strLine ) ;
        elements.Add( curElement ) ;
      end
      else if starts_with( strLine, 'property' ) then begin
        curProperty := parseProperty( strLine ) ;
        if assigned( curElement ) then
          curElement.AddProperty( curProperty ) ;
      end ;
    end ;

  end ;

  procedure T_PlyParser.readElements;
    {$IFDEF DCC}
    var
      elm : T_PlyElement ;
    {$ENDIF}
  begin
    for elm in elements do begin
      oLayer.HourglassShake ;

      if format = T_PlyFormat.ASCII then
        readElementAscii( elm )
      else
        readElementBinary( elm ) ;
    end ;
  end ;

  procedure T_PlyParser.readElementAscii(
    const _elm : T_PlyElement
  );
  var
    {$IFDEF DCC}
    prop    : T_PlyProperty ;
    {$ENDIF}
    i, p,t,k: Integer;
    cnt     : Integer ;
  begin
    SetLength( _elm.data, _elm.count ) ;
    i := 0 ;
    while i < _elm.count do begin
      readLine ;
      if IsStringEmpty( strLine ) then continue ;
      
      tkn.ExecuteEx( strLine, ' ' ) ;
      p := 0 ;
      t := 0 ;
      SetLength( _elm.data[i], _elm.properties.Count ) ;
      for prop in _elm.properties do begin
        if prop.countDataType = T_PlyDataType.UNKNOWN then begin
          prop.SetDataIndex( t ) ;
          prop.SetDataCount(1);
          _elm.data[i][t] := DotStrToFloat( tkn.Result[p] ) ;
          inc(t) ;
          inc(p) ;
        end
        else begin
          cnt := StrToInt( tkn.Result[p] ) ;
          inc( p ) ;
          prop.SetDataIndex(t);
          prop.SetDataCount(cnt);
          SetLength( _elm.data[i], length( _elm.data[i] ) + cnt ) ;
          for k := 0 to cnt-1 do begin
            _elm.data[i][t] := DotStrToFloat( tkn.Result[p] ) ;
            inc(p) ;
            inc(t) ;
          end ;
        end ;
      end ;
      inc( i ) ;
      if (i mod 10000) = 0 then
        oLayer.RaiseBusyShake( oLayer, plyFile.Position, plyFile.Size ) ;
    end ;
  end ;

  procedure T_PlyParser.readElementBinary(
    const _elm : T_PlyElement
  );
  var
    {$IFDEF DCC}
    prop    : T_PlyProperty ;
    {$ENDIF}
    i, p, t : Integer;
    cnt     : Integer ;
  begin
    SetLength( _elm.data, _elm.count ) ;
    for i := 0 to _elm.count-1 do begin
      p := 0 ;
      SetLength( _elm.data[i], _elm.properties.Count ) ;
      for prop in _elm.properties do begin
        if prop.countDataType = T_PlyDataType.UNKNOWN then begin
          prop.SetDataIndex( p ) ;
          prop.SetDataCount(1);
          _elm.data[i][p] := readBinary( prop.dataType ) ;
          inc( p ) ;
        end
        else begin
          cnt := TruncS(readBinary( prop.countDataType )) ;
          SetLength( _elm.data[i], length( _elm.data[i] ) + cnt ) ;
          prop.SetDataIndex( p ) ;
          prop.SetDataCount(cnt);
          for t := 0 to cnt-1 do begin
            _elm.data[i][p] := readBinary( prop.dataType ) ;
            inc(p) ;
          end ;
        end ;

        if (i mod 10000) = 0 then 
          oLayer.RaiseBusyShake( oLayer, plyFile.Position, plyFile.Size ) ;
      end ;
    end ;
  end ;

  function T_PlyParser.readBinary(
    const _dataType : T_PlyDataType
  ) : Double ;
  var
    bval  : Byte ;
    sval  : ShortInt ;
    fval  : Single ;
    dval  : Double ;
    i     : Integer ;
  begin
    Result := 0 ;
    case _dataType of
      T_PlyDataType.UNKNOWN :
        Result := 0 ;
      T_PlyDataType.CHAR    :
        begin
          plyFile.ReadByte( bval ) ;
          Result := bval ;
        end ;
      T_PlyDataType.UCHAR   :
        begin
          plyFile.ReadByte( bval ) ;
          Result := bval and $FF ;
        end ;
      T_PlyDataType.SHORT   :
        begin
          {$IFDEF OXYGENE}
            plyFile.Read( buf, 2 ) ;
          {$ELSE}
            plyFile.Read( buf[0], 2 ) ;
          {$ENDIF}
          if format = T_PlyFormat.BINARY_BIG_ENDIAN then
            Result := SmallInt(((($ff and buf[0]) shl 8) or (($ff and buf[1]) shl 0)))
          else
            Result := SmallInt(((($ff and buf[1]) shl 8) or (($ff and buf[0]) shl 0))) ;
        end ;
      T_PlyDataType.USHORT  :
        begin
          {$IFDEF OXYGENE}
            plyFile.Read( buf, 2 ) ;
          {$ELSE}
            plyFile.Read( buf[0], 2 ) ;
          {$ENDIF}
          if format = T_PlyFormat.BINARY_BIG_ENDIAN then
            sval := SmallInt(((($ff and buf[0]) shl 8) or (($ff and buf[1]) shl 0)))
          else
            sval := SmallInt(((($ff and buf[1]) shl 8) or (($ff and buf[0]) shl 0))) ;
          Result := sval and $0000FFFF ;
        end ;
      T_PlyDataType.INT     :
        begin
          {$IFDEF OXYGENE}
            plyFile.Read( buf, 4 ) ;
          {$ELSE}
            plyFile.Read( buf[0], 4 ) ;
          {$ENDIF}
          if format = T_PlyFormat.BINARY_BIG_ENDIAN then
            Result := Integer((Integer(buf[0]) shl 24) or
                              (Integer($ff and buf[1]) shl 16) or
                              (Integer($ff and buf[2]) shl 8) or
                              (Integer($ff and buf[3]) ))
          else
            Result := Integer((Integer(buf[3]) shl 24) or
                              (Integer($ff and buf[2]) shl 16) or
                              (Integer($ff and buf[1]) shl 8) or
                              (Integer($ff and buf[0]) ))
        end ;
      T_PlyDataType.UINT    :
        begin
          {$IFDEF OXYGENE}
            plyFile.Read( buf, 4 ) ;
          {$ELSE}
            plyFile.Read( buf[0], 4 ) ;
          {$ENDIF}
          if format = T_PlyFormat.BINARY_BIG_ENDIAN then
            Result := UInt32((UInt32(buf[0]) shl 24) or
                             (UInt32($ff and buf[1]) shl 16) or
                             (UInt32($ff and buf[2]) shl 8) or
                             (UInt32($ff and buf[3]) ))
          else
            Result := UInt32((UInt32(buf[3]) shl 24) or
                             (UInt32($ff and buf[2]) shl 16) or
                             (UInt32($ff and buf[1]) shl 8) or
                             (UInt32($ff and buf[0]) ))
        end ;
      T_PlyDataType.FLOAT   :
        begin
          {$IFDEF OXYGENE}
            plyFile.Read( buf, 4 ) ;
          {$ELSE}
            plyFile.Read( buf[0], 4 ) ;
          {$ENDIF}
          if format = T_PlyFormat.BINARY_BIG_ENDIAN then
            for i:=3 downto 0 do // swap bytes
              {$IFDEF OXYGENE}
                fval := SwapSingle(fval)
              {$ELSE}
                Byte(Pointer( NativeInt( @fval ) + i)^) := buf[3-i]
              {$ENDIF}
          else
            {$IFDEF OXYGENE}
              fval := BitConverter.ToSingle( buf, 0) ;
            {$ELSE}
            for i:=3 downto 0 do // swap bytes
              Byte(Pointer( NativeInt( @fval ) + i)^) := buf[i] ;
            {$ENDIF}

          Result := fval ;
        end ;
      T_PlyDataType.DOUBLE  :
        begin
          plyFile.ReadDouble( dval ) ;
          Result := dval ;
        end ;
    end ;
  end ;

  function T_PlyParser.parseProperty(
    const _property : String
  ) : T_PlyProperty ;
  var
    name    : String ;
    dtype   : T_PlyDataType ;
    cdtype  : T_PlyDataType ;
  begin
    tkn.Execute( _property, [' '] ) ;
    assert( tkn.Result.Count > 0 ) ;

    if tkn.Result[1] = 'list' then begin
      assert( tkn.Result.Count = 5 ) ;
      cdtype := parseDataType( tkn.Result[2] ) ;
      dtype  := parseDataType( tkn.Result[3] ) ;
      name   := tkn.Result[4] ;

      Result := T_PlyProperty.Create( name, dtype, cdtype ) ;
    end
    else begin
      assert( tkn.Result.Count = 3 ) ;
      name  := tkn.Result[2] ;
      dtype := parseDataType( tkn.Result[1] ) ;

      Result := T_PlyProperty.Create( name, dtype ) ;
    end ;
  end ;

  function T_PlyParser.parseDataType(
    const _typeName : String
  ) : T_PlyDataType ;
  begin
    if       'char'   = _typeName then
      Result := T_PlyDataType.CHAR
    else if 'uchar'   = _typeName then
      Result := T_PlyDataType.UCHAR
    else if 'short'   = _typeName then
      Result := T_PlyDataType.SHORT
    else if 'ushort'  = _typeName then
      Result := T_PlyDataType.USHORT
    else if 'int'     = _typeName then
      Result := T_PlyDataType.INT
    else if 'uint'    = _typeName then
      Result := T_PlyDataType.UINT
    else if 'float'   = _typeName then
      Result := T_PlyDataType.FLOAT
    else if 'double'  = _typeName then
      Result := T_PlyDataType.DOUBLE
    else if 'int8'    = _typeName then
      Result := T_PlyDataType.CHAR
    else if 'uint8'   = _typeName then
      Result := T_PlyDataType.UCHAR
    else if 'int16'   = _typeName then
      Result := T_PlyDataType.SHORT
    else if 'uint16'  = _typeName then
      Result := T_PlyDataType.USHORT
    else if 'int32'   = _typeName then
      Result := T_PlyDataType.INT
    else if 'uint32'  = _typeName then
      Result := T_PlyDataType.UINT
    else if 'float32' = _typeName then
      Result := T_PlyDataType.FLOAT
    else if 'float64' = _typeName then
      Result := T_PlyDataType.DOUBLE
    else
      Result := T_PlyDataType.UNKNOWN
  end ;

  function T_PlyParser.parseFormat(
    const _format : String
  ) : T_PlyFormat ;
  var
    fmt     : String ;
    version : String ;
  begin
    tkn.Execute( _format, [' '] ) ;
    assert( tkn.Result.Count = 3 ) ;

    fmt     := tkn.Result[1] ;
    version := tkn.Result[2] ;
    assert( version = '1.0' ) ;

    if      fmt = 'ascii' then
      Result := T_PlyFormat.ASCII
    else if fmt = 'binary_little_endian' then
      Result := T_PlyFormat.BINARY_LITTLE_ENDIAN
    else if fmt = 'binary_big_endian' then
      Result := T_PlyFormat.BINARY_BIG_ENDIAN
    else
      Result := T_PlyFormat.UNKNOWN
  end ;

  function T_PlyParser.parseElement(
    const _element : String
  ) : T_PlyElement ;
  var
    name    : String ;
    count   : Integer ;
  begin
    tkn.Execute( _element, [' '] ) ;
    assert( tkn.Result.Count = 3 ) ;

    name  := tkn.Result[1] ;
    count := StrToInt( tkn.Result[2] ) ;

    Result := T_PlyElement.Create( name, count ) ;
  end ;

  function T_PlyParser.parseComment(
    const _comment : String
  ) : String ;
  begin
    Result := Copy( _comment, StringFirst+length('comment') + 1, MaxInt ) ;
  end ;

  function T_PlyParser.ParamFromHeader(
    const _name : String
  ) : Double ;
  var
    i   : Integer ;
    str : String ;
  begin
    Result := 0 ;
    for i := 0 to comments.Count-1 do begin
      str := Trim( comments[i] ) ;
      if starts_with( str, _name ) then begin
        tkn.Execute( str, [' '] ) ;
        if tkn.Result.Count > 1 then
          Result := DotStrToFloat( tkn.Result[1] ) ;
        break;
      end ;
    end ;
  end ;

  procedure T_PlyParser.Parse;
  begin
    readHeader ;
    readElements ;
  end ;

//=============================================================================
// TGIS_LayerPLY
//=============================================================================

  constructor TGIS_LayerPLY.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory ] ;

    FVertexOnly := False ;

    SupportsAutoStyle := False ;
  end ;

  procedure TGIS_LayerPLY.doDestroy ;
  begin

    inherited ;
  end ;

  procedure TGIS_LayerPLY.buildModel(
    const _parser : TObject
  ) ;
  var
    ply         : T_PlyParser ;
    {$IFDEF DCC}
      elm       : T_PlyElement ;
      prop      : T_PlyProperty ;
    {$ENDIF}
    mshp        : TGIS_ShapeMultiPatch ;
    xindex      : Integer ;
    yindex      : Integer ;
    zindex      : Integer ;
    tindex      : Integer ;
    nxindex     : Integer ;
    nyindex     : Integer ;
    nzindex     : Integer ;
    rindex      : Integer ;
    gindex      : Integer ;
    bindex      : Integer ;
    aindex      : Integer ;
    has_tcoord  : Boolean ;
    has_normals : Boolean ;
    has_colors  : Boolean ;
    velem       : T_PlyElement ;
    felem       : T_PlyElement ;
    ptype       : TGIS_PartType ;
    scalex      : Double ;
    scaley      : Double ;
    scalez      : Double ;
    must_scale  : Boolean ;
    vbbox       : TGIS_Extent3D ;

    procedure analyzeVertex( const _elm : T_PlyElement ) ;
    var
      p : Integer ;
    {$IFDEF DCC}
      prop : T_PlyProperty ;
    {$ENDIF}
    begin
      velem := _elm ;
      p := 0 ;
      assert( velem <> nil ) ;

      for prop in velem.properties do begin
        if prop.name = 'x' then
          xindex := p
        else if prop.name = 'y' then
          yindex := p
        else if prop.name = 'z' then
          zindex := p
        else if prop.name = 'nx' then
          nxindex := p
        else if prop.name = 'ny' then
          nyindex := p
        else if prop.name = 'nz' then
          nzindex := p
        else if (prop.name = 'red')   or (prop.name = 'diffuse_red') then
          rindex := p
        else if (prop.name = 'green') or (prop.name = 'diffuse_green') then
          gindex := p
        else if (prop.name = 'blue')  or (prop.name = 'diffuse_blue') then
          bindex := p
        else if prop.name = 'alpha' then
          aindex := p ;

        inc( p ) ;
      end ;

      if (nxindex<>-1) and (nyindex<>-1) and (nzindex<>-1) then
        has_normals := True ;

      if (rindex<>-1) and (gindex<>-1) and (bindex<>-1) then
        has_colors := True ;

      {$IFDEF GIS_NORECORDS}
        vbbox := new TGIS_Extent3D ;
      {$ENDIF}
      vbbox.XMin := GIS_MAX_SINGLE ;
      vbbox.YMin := GIS_MAX_SINGLE ;
      vbbox.ZMin := GIS_MAX_SINGLE ;
      vbbox.XMax := - GIS_MAX_SINGLE ;
      vbbox.YMax := - GIS_MAX_SINGLE ;
      vbbox.ZMax := - GIS_MAX_SINGLE ;

      for p := 0 to velem.count-1 do begin
        if velem.data[p][xindex] < vbbox.XMin then
          vbbox.XMin := velem.data[p][xindex] ;
        if velem.data[p][xindex] > vbbox.XMax then
          vbbox.XMax := velem.data[p][xindex] ;
        if velem.data[p][yindex] < vbbox.YMin then
          vbbox.YMin := velem.data[p][yindex] ;
        if velem.data[p][yindex] > vbbox.YMax then
          vbbox.YMax := velem.data[p][yindex] ;
        if velem.data[p][zindex] < vbbox.ZMin then
          vbbox.ZMin := velem.data[p][zindex] ;
        if velem.data[p][zindex] > vbbox.ZMax then
          vbbox.ZMax := velem.data[p][zindex] ;
      end ;

      if not GisIsNoWorld3D( bbox ) and not GisIsNoWorld3D( vbbox ) then begin
        if ((vbbox.YMax - vbbox.YMin) > (vbbox.XMax - vbbox.XMin)) then begin
          if ((vbbox.YMax - vbbox.YMin) > (vbbox.ZMax - vbbox.ZMin)) then
            scalex := (bbox.YMax - bbox.YMin)/(vbbox.YMax - vbbox.YMin)
          else
            scalex := (bbox.ZMax - bbox.ZMin)/(vbbox.ZMax - vbbox.ZMin);
        end
        else begin
          if ((vbbox.XMax - vbbox.XMin) > (vbbox.ZMax - vbbox.ZMin)) then
            scalex := (bbox.XMax - bbox.XMin)/(vbbox.XMax - vbbox.XMin)
          else
            scalex := (bbox.ZMax - bbox.ZMin)/(vbbox.ZMax - vbbox.ZMin)
        end ;
        scaley := scalex ;
        scalez := scalex ;

       // scalex := (bbox.XMax - bbox.XMin) / (vbbox.XMax - vbbox.XMin) ;
       // scaley := (bbox.YMax - bbox.YMin) / (vbbox.YMax - vbbox.YMin) ;
       // scalez := (bbox.ZMax - bbox.ZMin) / (vbbox.ZMax - vbbox.ZMin) ;
        must_scale := True ;
      end ;


    end ;

    procedure analyzeFace( const _elm : T_PlyElement ) ;
    var
      p : Integer ;
      {$IFDEF DCC}
        prop : T_PlyProperty ;
      {$ENDIF}
    begin
      felem := _elm ;
      assert( velem <> nil ) ;
      p := 0 ;

      for prop in felem.properties do begin
        if prop.name = 'texcoord' then begin
          has_tcoord := True ;
          tindex := prop.dataIndex ;
        end
        else if (prop.name = 'vertex_indices') or
                (prop.name = 'vertex_index'  ) then begin

          if felem.name = 'tristrips' then
            ptype := TGIS_PartType.Triangle
          else if prop.dataCount > 3 then
            ptype := TGIS_PartType.Ring
          else
            ptype := TGIS_PartType.Triangle ;
        end ;

        inc( p ) ;
      end ;

    end ;

    procedure buildPoints ;
    var
      pshp    : TGIS_ShapePoint ;
      e, p    : Integer ;
      {$IFDEF DCC}
        prop  : T_PlyProperty ;
      {$ENDIF}
      a,r,g,b : Byte ;
      ptg     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    begin
      assert( velem <> nil ) ;
      Params.Marker.Size := 20 ;

      for e := 0 to velem.count-1 do begin
        pshp := CreateShape( TGIS_ShapeType.Point ) as TGIS_ShapePoint;
        pshp.AddPart ;
        p := 0 ;
        a := 255 ;
        r := 0 ;
        g := 0 ;
        b := 0 ;

        ptg.X := velem.data[e][xindex] ;
        ptg.Y := velem.data[e][yindex] ;
        ptg.Z := velem.data[e][zindex] ;
        ptg.M := 0 ;

        if must_scale then
          ptg := GisPoint3D( ptg.X  + offset.X,
                             ptg.Y  + offset.Y,
                             ptg.Z  + offset.Z
                            ) ;
        pshp.AddPoint3D( ptg ) ;

        if has_colors then begin
          r := TruncS( velem.data[e][rindex] ) ;
          g := TruncS( velem.data[e][gindex] ) ;
          b := TruncS( velem.data[e][bindex] ) ;
          if aindex <> -1 then
            a := TruncS( velem.data[e][aindex] ) 
          else
            a := 255 ;
        end ;
        pshp.Params.Marker.Color := TGIS_Color.FromARGB( a, r, g, b ) ;
      end ;
    end ;

    procedure buildMultiPoints ;
    var
      pshp    : TGIS_ShapePointCloud ;
      e, p    : Integer ;
      {$IFDEF DCC}
        prop  : T_PlyProperty ;
      {$ENDIF}
      a,r,g,b : Byte ;
      ptg     : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF};
    begin
      assert( velem <> nil ) ;
      Params.Marker.Size := 20 ;

      pshp := CreateShape( TGIS_ShapePointCloud ) as TGIS_ShapePointCloud;
      pshp.AddPart ;
      for e := 0 to velem.count-1 do begin
        p := 0 ;
        a := 255 ;
        r := 0 ;
        g := 0 ;
        b := 0 ;

        ptg.X := velem.data[e][xindex] ;
        ptg.Y := velem.data[e][yindex] ;
        ptg.Z := velem.data[e][zindex] ;
        ptg.M := 0 ;

        if must_scale then
          ptg := GisPoint3D( ptg.X  + offset.X,
                             ptg.Y  + offset.Y,
                             ptg.Z  + offset.Z
                            ) ;

        if has_colors then begin
          r := TruncS( velem.data[e][rindex] ) ;
          g := TruncS( velem.data[e][gindex] ) ;
          b := TruncS( velem.data[e][bindex] ) ;
          if aindex <> -1 then
            a := TruncS( velem.data[e][aindex] )
          else
            a := 255 ;

          pshp.AddVertex( ptg, TGIS_Color.FromARGB( a, r, g, b ) ) ;
        end
        else
          pshp.AddPoint3D( ptg ) ;
      end ;
    end ;

    procedure parseTextures ;
    var
      i         : Integer ;
      material  : TGIS_Material ;
      tpath     : String ;
      fname     : String ;
      bmp       : TGIS_Bitmap ;
      pix       : TGIS_Pixels ;
    begin
      for i := 0 to ply.comments.Count-1 do begin
        if starts_with( ply.comments[i], 'TextureFile' ) then begin
          fname := Copy( ply.comments[i], Pos( ply.comments[i], 'TextureFile' ) + 12, MaxInt ) ;
          tpath := GetFilePath( Path ) + Trim( fname ) ;
          if SafeFileExists( tpath ) then begin
            mshp.HasMaterials := True ;
            mshp.Materials.Resize( 1 ) ;
            mshp.Materials.CompressionType := TGIS_CompressionType.ARGB ;

            material.HasColor           := False ;
            material.Color              := TGIS_Color.White ;
            material.HasEdgeColor       := False ;
            material.EdgeColor          := TGIS_Color.Black ;
            material.HasEdgeWidth       := False ;
            material.EdgeWidth          := 0 ;
            material.HasTransparency    := False ;
            material.Transparency       := 0 ;
            material.HasShininess       := False ;
            material.Shininess          := 0 ;
            material.HasCullBackFaces   := False ;
            material.SharedTextureIndex := 0 ;
            material.HasSharedTexture   := False ;
            material.Bpp                := 4 ;
            material.CompressionType    := TGIS_CompressionType.ARGB ;

            bmp := TGIS_Bitmap.Create ;
            try
              bmp.LoadFromFile( tpath ) ;

              material.Width              := bmp.Width ;
              material.Height             := bmp.Height ;
              material.Size               := bmp.Width * bmp.Height * 4 ;
              SetLength( material.Buffer, bmp.Width * bmp.Height * 4 ) ;

              bmp.LockPixels( pix, false, TGIS_BitmapFormat.ARGB, TGIS_BitmapLinesOrder.Native ) ;
              try
                {$IFDEF CLR}
                  Buffer.BlockCopy( pix, 0, material.Buffer, 0, bmp.Width * bmp.Height * 4 ) ;
                {$ENDIF}
                {$IFDEF JAVA}
                  System.arraycopy( pix, 0, material.Buffer, 0, bmp.Width * bmp.Height ) ;
                {$ENDIF}
                {$IFDEF DCC}
                  System.Move( pix[0], material.Buffer[0], bmp.Width * bmp.Height * 4 ) ;
                {$ENDIF}
              finally
               bmp.UnlockPixels ;
              end ;
            finally
              FreeObject( bmp ) ;
            end ;

            material.HasTextureMap     := True ;
            mshp.Materials.Material[0] := material ;
          end ;
        end ;

      end ;

    end ;

    procedure buildTriangles( const _elm : T_PlyElement ) ;
    var
      e, p, i,
      c, t      : Integer ;
      {$IFDEF DCC}
        prop    : T_PlyProperty ;
      {$ENDIF}
      a,r,g,b   : Byte ;
      ptg       : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    begin
      mshp := CreateShape( TGIS_ShapeType.MultiPatch ) as TGIS_ShapeMultiPatch ;
      mshp.Lock( TGIS_Lock.Internal ) ;
      mshp.AddPart ;
      mshp.SetPartType( 0, ptype ) ;

      if has_tcoord then begin
        mshp.HasTextures := True ;
        mshp.Textures.Resize( 1, 2*6*_elm.count, 2 ) ;
        mshp.Textures.SetPartOffset( 0, 0 ) ;
      end ;

      if has_normals then begin
        mshp.HasNormals := True ;
        mshp.Normals.Resize( velem.count ) ;
        for e := 0 to velem.count-1 do
          mshp.Normals.Normal[e] :=
            GisSingleVector( velem.data[e][nxindex],
                             velem.data[e][nyindex],
                             velem.data[e][nzindex]
                            ) ;
      end ;

      if has_colors then begin
        mshp.HasVertexColors := True ;
        mshp.VertexColors.Resize( 1, _elm.count*3 ) ;
        mshp.VertexColors.SetPartOffset( 0, 0 ) ;
      end ;

      if has_tcoord or has_colors then begin
        mshp.HasPartDescriptors := True ;
        mshp.PartDescriptors.Resize( 1 ) ;
        mshp.PartDescriptors.PartDescriptor[0] := GisPartDescriptor( 6,0,0,0 ) ;
      end ;

      t := 0 ;
      c := 0 ;
      for e := 0 to _elm.count-1 do begin
        i := 0 ;
        for prop in _elm.properties do begin
          if (prop.name = 'vertex_indices') or
             (prop.name = 'vertex_index'  ) then begin


            for p := 0 to prop.dataCount-1 do begin
              i := TruncS(_elm.data[e][p]) ;
              if i >= 0 then begin
                ptg.X := velem.data[i][xindex] ;
                ptg.Y := velem.data[i][yindex] ;
                ptg.Z := velem.data[i][zindex] ;
                ptg.M := 0 ;

                if must_scale then
//                  ptg := GisPoint3D(
//                          ( (ptg.X + bbox.XMin) * scalex ) + offset.X,
//                          ( (ptg.Y + bbox.YMin) * scaley ) + offset.Y,
//                          ( (ptg.Z + bbox.ZMin) * scalez ) + offset.Z
//                         ) ;
                  ptg := GisPoint3D( ptg.X  + offset.X,
                                     ptg.Y  + offset.Y,
                                     ptg.Z  + offset.Z
                                   ) ;
                mshp.AddPoint3D( ptg ) ;

                if has_colors then begin
                  a := 255 ;
                  r := 0 ;
                  g := 0 ;
                  b := 0 ;

                  if rindex >= 0 then
                    r := TruncS( velem.data[i][rindex] ) ;
                  if gindex >= 0 then
                    g := TruncS( velem.data[i][gindex] ) ;
                  if bindex >= 0 then
                    b := TruncS( velem.data[i][bindex] ) ;
                  if aindex >= 0 then
                    a := TruncS( velem.data[i][aindex] ) ;

                  mshp.VertexColors.SetVertexColor( c, TGIS_Color.FromARGB( a, r, g, b).ARGB ) ;
                  inc(c);
                end ;
              end ;
            end ;

          end
          else if prop.name = 'texcoord' then begin
            for p := 0 to prop.dataCount-1 do begin
              if (p mod 2)=0 then
                mshp.Textures.SetTextureCoord( t, _elm.data[e][tindex+p] )
              else
                mshp.Textures.SetTextureCoord( t, 1-_elm.data[e][tindex+p] ) ;
              inc(t) ;
            end ;

          end ;

        end ;
      end ;
      mshp.Unlock ;
    end ;

    procedure unpackTriStrips( const _elm : T_PlyElement ) ;
    var
      {$IFDEF DCC}
        prop    : T_PlyProperty ;
      {$ENDIF}
      i, p, f   : Integer ;
      numfaces  : Integer ;
      numpt     : Integer ;
      faces     : T_PlyData ;
    begin
      for prop in _elm.properties do begin
        if (prop.name = 'vertex_indices') or
           (prop.name = 'vertex_index'  ) then begin

          numfaces := 0 ;
          numpt    := 0 ;
          assert( length(_elm.data) > 0 ) ;
          for p := 0 to prop.dataCount-1 do begin
            i := TruncS(_elm.data[0][p]) ;
            if i = -1 then begin
              numpt := 0 ;
              continue ;
            end ;
            inc( numpt ) ;
            if numpt >=3 then
              inc( numfaces ) ;
          end ;

          SetLength( faces, numfaces ) ;

          f := 0;
          numpt := 0;
          for p := 0 to prop.dataCount-1 do begin
            if _elm.data[0][p] = -1 then begin
              numpt := 0 ;
              continue ;
            end ;
            inc( numpt ) ;

            if (numpt < 3) then
              continue ;
            
            SetLength( faces[f], 3 ) ;
            if (numpt mod 2) > 0 then begin
              faces[f][0] := _elm.data[0][p-2] ;
              faces[f][1] := _elm.data[0][p-1] ;
            end
            else begin
              faces[f][0] := _elm.data[0][p-1] ;
              faces[f][1] := _elm.data[0][p-2] ;
            end ;
            faces[f][2]   := _elm.data[0][p] ;
            inc( f ) ;
          end ;

          // replace data
          _elm.data := faces ;
          _elm.count := numfaces ;
          prop.SetDataCount( 3 ) ;

        end ;
      end ;

    end ;

    procedure unpackRangeGrid( const _elm : T_PlyElement ) ;
    var
      {$IFDEF DCC}
        prop    : T_PlyProperty ;
      {$ENDIF}
      i, p, j   : Integer ;
      numfaces  : Integer ;
      faces     : T_PlyData ;
      w, h, f   : Integer ;
      v0i, v1i  : Integer ;
      v2i, v3i  : Integer ;
      grid      : TGIS_IntegerArray ;
      ndone     : Integer ;
      quad      : Boolean ;
    begin
      for prop in _elm.properties do begin
        if (prop.name = 'vertex_indices') or
           (prop.name = 'vertex_index'  ) then begin

          w := TruncS( ply.ParamFromHeader( 'num_cols' ) ) ;
          h := TruncS( ply.ParamFromHeader( 'num_rows' ) ) ;

          assert( w > 0 ) ;
          assert( h > 0 ) ;
          SetLength( grid, w*h ) ;

          for p := 0 to _elm.count-1 do begin
            assert( length( _elm.data[p] ) > 0 ) ;
            if _elm.data[p][0] = 0 then
              grid[p] := -1
            else
              grid[p] := TruncS(_elm.data[p][0]) ;
          end ;

          numfaces := w * h ;

          SetLength( faces, numfaces * 2 ) ;
          f := 0 ;
          for i := 0 to h-2 do
            for j := 0 to w-2 do begin
              v0i := grid[(i+0)*w + j+0] ;
              v1i := grid[(i+0)*w + j+1] ;
              v2i := grid[(i+1)*w + j+0] ;
              v3i := grid[(i+1)*w + j+1] ;

              ndone := 0 ;
              quad  := (v0i >= 0) and (v1i >= 0) and (v2i >= 0) and (v3i >= 0) ;

              if (v0i >= 0) and (v2i >= 0) and (v3i >= 0) then begin
                SetLength( faces[f], 3 ) ;
                faces[f][0] := v3i ;
                faces[f][1] := v2i ;
                faces[f][2] := v0i ;
                inc( ndone ) ;
                inc( f ) ;
              end ;

              if (v0i >= 0) and (v1i >= 0) and (v3i >= 0) then begin
                SetLength( faces[f], 3 ) ;
                faces[f][0] := v0i ;
                faces[f][1] := v1i ;
                faces[f][2] := v3i ;
                inc( ndone ) ;
                inc( f ) ;
              end ;

              if (ndone = 0) then begin
                if (v2i >= 0) and (v0i >= 0) and (v1i >=0) then begin
                  SetLength( faces[f], 3 ) ;
                  faces[f][0] := v2i ;
                  faces[f][1] := v0i ;
                  faces[f][2] := v1i ;
                  inc( ndone ) ;
                  inc( f ) ;
                end ;

                if (v1i >= 0) and (v3i >= 0) and (v2i >= 0) then begin
                  SetLength( faces[f], 3 ) ;
                  faces[f][0] := v1i ;
                  faces[f][1] := v3i ;
                  faces[f][2] := v2i ;
                  inc( ndone ) ;
                  inc( f ) ;
                end ;
              end ;
            end ;

          SetLength( faces, f ) ;

          // replace data
          _elm.data := faces ;
          _elm.count := f ;
          prop.SetDataCount( 3 ) ;
        end ;
      end ;

    end ;

  begin
    ply := T_PlyParser(_parser) ;

    xindex  := 0 ;
    yindex  := 0 ;
    zindex  := 0 ;
    tindex  := 0 ;
    nxindex := -1 ;
    nyindex := -1 ;
    nzindex := -1 ;
    rindex  := -1 ;
    gindex  := -1 ;
    bindex  := -1 ;
    aindex  := -1 ;

    has_tcoord  := False ;
    has_normals := False ;
    has_colors  := False ;
    must_scale  := False ;

    for elm in ply.elements do begin
      if elm.name = 'vertex' then begin
        analyzeVertex( elm ) ;
        if ply.elements.Count = 1 then
          buildPoints ;
      end
      else if (elm.name = 'face') then begin
        analyzeFace( elm ) ;

        if (felem.count = 0) or FVertexOnly then begin
          buildMultiPoints
        end
        else begin
          buildTriangles( elm ) ;
          parseTextures ;
        end
      end
      else if (elm.name = 'tristrips') then begin
        analyzeFace( elm ) ;
        unpackTriStrips( elm ) ;
        buildTriangles( elm ) ;
        parseTextures ;
      end
      else if (elm.name = 'range_grid') then begin
        analyzeFace( elm ) ;
        unpackRangeGrid( elm ) ;
        buildTriangles( elm ) ;
        parseTextures ;
      end ;

    end ;

  end ;

  procedure TGIS_LayerPLY.fset_UseRTree(
    const _value : Boolean
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_LayerPLY.checkBox ;
  var
    lst   : TStringList ;
    fname : String ;
    tkn   : TGIS_Tokenizer ;
    i     : Integer ;
  begin
    offset := GisPoint3D( 0, 0, 0 ) ;
    bbox   := GisNoWorld3D ;

    fname  := GetPathDirSep( GetFileDir(Path) ) + GetFileNameNoExt( Path ) + '.box' ;
    if not SafeFileExists( fname ) then exit ;

    lst := TStringList.Create ;
    try
      lst.LoadFromFile( fname ) ;

      tkn := TGIS_Tokenizer.Create ;
      try
        for i := 0 to lst.Count-1 do begin
          tkn.Execute( lst[i], [' '] ) ;
          if tkn.Result.Count > 3 then begin
            if CompareText( tkn.Result[0], 'bb_min_f' ) = 0 then begin
              bbox.XMin := DotStrToFloat( tkn.Result[1] ) ;
              bbox.YMin := DotStrToFloat( tkn.Result[2] ) ;
              bbox.ZMin := DotStrToFloat( tkn.Result[3] ) ;
            end
            else if CompareText( tkn.Result[0], 'bb_max_f' ) = 0 then begin
              bbox.XMax := DotStrToFloat( tkn.Result[1] ) ;
              bbox.YMax := DotStrToFloat( tkn.Result[2] ) ;
              bbox.ZMax := DotStrToFloat( tkn.Result[3] ) ;
            end
            else if CompareText( tkn.Result[0], 'offset' ) = 0 then begin
              offset.X := DotStrToFloat( tkn.Result[1] ) ;
              offset.Y := DotStrToFloat( tkn.Result[2] ) ;
              offset.Z := DotStrToFloat( tkn.Result[3] ) ;
            end ;
          end ;
        end ;
      finally
        FreeObject( tkn ) ;
      end ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_LayerPLY.setUp ;
  var
    ply : T_PlyParser ;
  begin
    inherited ;

    FSupportedShapes       := GisGetShapeType( TGIS_ShapeType.MultiPatch ) +
                              GisGetShapeType( TGIS_ShapeType.Point      ) +
                              GisGetShapeType( TGIS_ShapeType.MultiPoint ) ;

    FSupportedDimensions   := GisGetDimensionType( TGIS_DimensionType.XYZM ) +
                              GisGetDimensionType( TGIS_DimensionType.XYZ ) ;

    // block selection to avoid gdi hanging
    FUnSupportedOperations := GisAddOperationType( FUnSupportedOperations,
                                                   TGIS_OperationType.Select
                                                  ) ;
    // block editing to avoid gdi hanging
    FUnSupportedOperations := GisAddOperationType( FUnSupportedOperations,
                                                   TGIS_OperationType.Edit
                                                  ) ;

    if not IsStringEmpty( Path ) and SafeFileExists( Path ) then begin
      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        ply := T_PlyParser.Create( Path, self) ;
        try
          ply.Parse ;
          checkBox ;
          buildModel( ply ) ;

          FFileInfo := 'PLY Polygon File Format' + #13#10 +
                        ply.comments.Text ;
        finally
          FreeObject( ply ) ;
        end ;
      finally
        Unlock ;
        RaiseBusyRelease( Self ) ;
      end ;
    end ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;
  end ;

  function TGIS_LayerPLY.DrawEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    shp : TGIS_Shape ;
  begin
    Result := IsVisible( _extent ) ;
    if not Result then exit ;

    // just a fake area covering the model to avoid drawing mess in gdi
    shp := TGIS_ShapePolygon.Create( nil, nil, False, -1, self, TGIS_DimensionType.XY ) ;
    try
      shp.Lock( TGIS_Lock.Internal ) ;
      shp.AddPart ;
      shp.AddPoint( GisPoint( Extent.XMin, Extent.YMin ) ) ;
      shp.AddPoint( GisPoint( Extent.XMax, Extent.YMin ) ) ;
      shp.AddPoint( GisPoint( Extent.XMax, Extent.YMax ) ) ;
      shp.AddPoint( GisPoint( Extent.XMin, Extent.YMax ) ) ;
      shp.Unlock ;
      shp.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
      shp.Draw ;
    finally
      FreeObject( shp ) ;
    end ;
  end ;

  function TGIS_LayerPLY.PreRecognize(
    const _path     : String ;
      var _new_path : String
  ) : Boolean ;
  var
    mode : String   ;
    k    : Integer  ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    if not IsServerPath( _path ) then begin
      k := Pos( '?', _path ) ;
      if k = 0 then
        mode := ''
      else
        mode := Copy( _path, k+1, MaxInt ) ;
    end
    else
      mode := '' ;

    if CompareText( mode, 'v' ) = 0 then
      FVertexOnly := True ;
  end;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerPLY.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-PLY', 'PLY Polygon File Format',
                   TGIS_LayerPLY, '.ply',
                   TGIS_RegisteredLayerType.Vector3D,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    True
                  ) ;
   end ;

{$IFDEF DCC}
  initialization
    Unit_GisLayerPLY.SelfRegisterLayer() ;
{$ENDIF}


end.

