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
  Encapsulation of SAGA GIS Binary Grid layer.
}

{$IFDEF DCC}
  unit GisLayerSGRD ;
  {$HPPEMIT '#pragma link "GisLayerSGRD"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      Winapi.Windows,
    {$ENDIF}
    System.Types,
    System.SysUtils,
    System.Classes,

    GisTypes,
    GisStreams,
    GisCsSystems,
    GisLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerSGRD = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of SAGA GIS Binary Grid layer.
  /// </summary>
  TGIS_LayerSGRD = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private
      bufStream : TGIS_BufferedFileStream ;
    private const
      SGRD_DTYPE_BYTE   = 1 ;
      SGRD_DTYPE_UINT16 = 2 ;
      SGRD_DTYPE_INT16  = 3 ;
      SGRD_DTYPE_UINT32 = 4 ;
      SGRD_DTYPE_INT32  = 5 ;
      SGRD_DTYPE_FLOAT  = 6 ;
      SGRD_DTYPE_DOUBLE = 7 ;
    private // various private variables
      /// <summary>
      ///   Byte order from most significant to least significant.
      /// </summary>
      bigEndian       : Boolean ;

      /// <summary>
      ///   line buffer (line of singles).
      /// </summary>
      lineBuffer      : array of Single ;

      /// <summary>
      ///   line buffer (line of singles).
      /// </summary>
      lineByteBuffer  : TBytes ;

      /// <summary>
      ///   Number of lines present in lineBuffer.
      /// </summary>
      lineInBuffer    : Integer ;

      /// <summary>
      ///   Number of bytes in a cell.
      /// </summary>
      cellByteSize    : Integer ;

      /// <summary>
      ///   Data type of cell.
      /// </summary>
      cellType : Integer ;

      /// <summary>
      ///   Is grid data from top to bottom.
      /// </summary>
      topToBottom : Boolean ;

    private
      procedure parseHeader( const _fname : String
                           ) ;
      procedure writeHeader( const _fname : String
                           ) ;
      function  readValue : Single ;

      procedure writeGrid  ( const _outStream : TGIS_FileStream ;
                             const _x         : Integer ;
                             const _y         : Integer ;
                             const _grd       : TGIS_GridArray
                           ) ;
      function  findOffset ( const _line : Integer
                           ) : Int64 ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines
      /// <inheritdoc/>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure setUp          ; override;

      /// <summary>
      ///   Convert from big endian to low endian format.
      /// </summary>
      procedure convertLineToLE  ;

      /// <inheritdoc/>
      function  getLine      ( const _buffer : TBytes  ;
                               const _offset : Integer ;
                               const _linenr : Integer ;
                               const _start  : Integer ;
                               const _bytes  : Integer
                             ) : Integer; override  ;

      /// <inheritdoc/>
      function  getNativeValue ( const _pt     : TPoint  ;
                                 const _ar     : TGIS_DoubleArray
                               ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine  ( const  _buffer   : TGIS_SingleArray ;
                                 const _linenr   : Integer          ;
                                 const _startIdx : Integer          ;
                                 const _count    : Integer
                               ) : Integer ; override;

      /// <inheritdoc/>
      procedure prepareMinMaxZ ( const _zoom : Double = -1
                               ) ; override ;

     function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

    protected
      /// <inheritdoc/>
      procedure doDestroy ; override;
    public
      // constructors

      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      procedure Build          (  const _path      : String         ;
                                  const _grid      : Boolean        ;
                                  const _cs        : TGIS_CSCoordinateSystem  ;
                                  const _ext       : TGIS_Extent    ;
                                  const _width     : Integer        ;
                                  const _height    : Integer ;
                                  const _subformat : TGIS_LayerPixelSubFormat
                                ) ; override;

      /// <inheritdoc/>
      procedure SaveData   ; override;
   end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    GisRtl,
    GisParams,
    GisTypesUI,
    GisFunctions,
    GisInternals,
    GisClasses,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

//==============================================================================
// TGIS_LayerSGRD
//==============================================================================

  constructor TGIS_LayerSGRD.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable ] ;
    FIsGridImage       := True ;
    FIsNativeGridImage := True ;
    FBandsCount        := 1 ;
  end ;

  procedure TGIS_LayerSGRD.doDestroy ;
  begin
    Dormant ;

    FreeObject( bufStream ) ;

    inherited ;
  end ;

  procedure TGIS_LayerSGRD.parseHeader(
    const _fname : String
  ) ;
  var
    txt       : TArray<String> ;
    ffile     : TGIS_BufferedFileStream ;
    sdata_frm : String ;
    ext       : TGIS_Extent ;
    p         : Integer ;

    procedure read_param ;
    var
      line_str : String ;
    begin
      if not ffile.Eof then begin
        line_str := Trim( ffile.ReadLine ) ;
        if not IsStringEmpty( line_str ) then begin
          {$IFDEF JAVA OR ISLAND}
            txt := line_str.Split( '=' ).ToArray ;
          {$ELSE}
            txt := line_str.Split( ['='] ) ;
          {$ENDIF}
        end ;
        if length( txt ) >= 2 then begin
          txt[0] := Trim( txt[0] ) ;
          txt[1] := Trim( txt[1] ) ;
        end ;
      end ;
    end ;

  begin
    ffile := TGIS_BufferedFileStream.Create( _fname, TGIS_StreamMode.Read ) ;
    try
      scaleY := 0;
      cellByteSize := 0 ;
      ext := GisExtent( 0,0,0,0 ) ;
      topToBottom := False ;
      repeat
        read_param ;
        if IsStringEmpty( txt[0] ) then break ;

        if      txt[0] = 'CELLCOUNT_X' then
          FBitWidth  := StrToInt( txt[1] )
        else if txt[0] = 'CELLCOUNT_Y' then
          FBitHeight := StrToInt( txt[1] )
        else if txt[0] = 'POSITION_XMIN' then
          ext.XMin := DotStrToFloat( txt[1] )
        else if txt[0] = 'POSITION_YMIN' then
          ext.YMin := DotStrToFloat( txt[1] )
        else if txt[0] = 'DATAFORMAT' then begin
          sdata_frm := txt[1] ;
          if sdata_frm = 'BIT' then begin
            cellByteSize := 1 ;
            cellType     := SGRD_DTYPE_BYTE ;
          end
          else if sdata_frm = 'BYTE_UNSIGNED' then begin
            cellByteSize := 1 ;
            cellType     := SGRD_DTYPE_BYTE ;
          end
          else if sdata_frm = 'BYTE' then begin
            cellByteSize := 1 ;
            cellType     := SGRD_DTYPE_BYTE ;
          end
          else if sdata_frm = 'SHORTINT_UNSIGNED' then begin
            cellByteSize := 2 ;
            cellType     := SGRD_DTYPE_UINT16 ;
          end
          else if sdata_frm = 'SHORTINT' then begin
            cellByteSize := 2 ;
            cellType     := SGRD_DTYPE_INT16 ;
          end
          else if sdata_frm = 'INTEGER_UNSIGNED' then begin
            cellByteSize := 4 ;
            cellType     := SGRD_DTYPE_UINT32 ;
          end
          else if sdata_frm = 'INTEGER' then begin
            cellByteSize := 4 ;
            cellType     := SGRD_DTYPE_INT32 ;
          end
          else if sdata_frm = 'FLOAT' then begin
            cellByteSize := 4 ;
            cellType     := SGRD_DTYPE_FLOAT ;
          end
          else if sdata_frm = 'DOUBLE' then begin
            cellByteSize := 8 ;
            cellType     := SGRD_DTYPE_DOUBLE ;
          end;
        end
        else if txt[0] = 'CELLSIZE' then begin
          scaleX := DotStrToFloat(txt[1]) ;
          scaleY := -scaleX ;
        end
        else if txt[0] = 'NODATA_VALUE' then begin
          p := Pos( ';', txt[1] ) ;
          if p > StringFirst then
            FNoDataValue   := DotStrToFloat( Copy( txt[1], StringFirst, p-1 ) )
          else
            FNoDataValue   := DotStrToFloat(txt[1]) ;

          redTransp[0]   := BASE_TRANSPARENT_FLAG ;
          greenTransp[0] := BASE_TRANSPARENT_FLAG ;
          blueTransp[0]  := BASE_TRANSPARENT_FLAG ;

          Params.Pixel.GridNoValue := NoDataValue ;
        end
        else if txt[0] = 'BYTEORDER_BIG' then begin
          if txt[1] = 'TRUE' then
            bigEndian := True
          else
            bigEndian := False ;
        end
        else if txt[0] = 'TOPTOBOTTOM' then
          topToBottom := txt[1] = 'TRUE'

      until ffile.Eof ;

      if scaleY = 0 then
        scaleY := -scaleX ;

      ext.XMax := ext.XMin + (FBitWidth  * scaleX) ;
      ext.YMax := ext.YMin - (FBitHeight * scaleY) ;

      Extent := ext ;
    finally
      FreeObject( ffile ) ;
    end ;
  end ;

  procedure TGIS_LayerSGRD.writeHeader(
    const _fname : String
  ) ;
  var
    lst : TGIS_StringList ;
  begin
    lst := TGIS_StringList.Create ;
    try
      lst.Values['NAME'           ] := Name ;
      lst.Values['DESCRIPTION'    ] := Caption ;
      lst.Values['UNIT'           ] := '' ;
      lst.Values['DATAFORMAT'     ]  := 'FLOAT' ;
      lst.Values['DATAFILE_OFFSET'] := '0' ;
      lst.Values['BYTEORDER_BIG'  ]  := 'FALSE' ;
      lst.Values['TOPTOBOTTOM'    ]  := 'TRUE' ;
      lst.Values['POSITION_XMIN'  ] := DotFloatToStr( Extent.XMin ) ;
      lst.Values['POSITION_YMIN'  ] := DotFloatToStr( Extent.YMin ) ;
      lst.Values['CELLCOUNT_X'    ] := IntToStr( FBitWidth ) ;
      lst.Values['CELLCOUNT_Y'    ] := IntToStr( FBitHeight ) ;
      lst.Values['CELLSIZE'        ] := DotFloatToStr( (Extent.XMax-Extent.XMin)/FBitWidth ) ;
      lst.Values['Z_FACTOR'        ] := '1.000000' ;
      lst.Values['Z_OFFSET'        ] := '0.000000' ;
      lst.Values['NODATA_VALUE'    ] := DotFloatToStr( FNoDataValue ) ;

      lst.SaveToFile( _fname ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

  procedure TGIS_LayerSGRD.setUp ;
  var
    fext   : String ;
    sfname : String ;
  begin
    if isBuilt then exit ;

    try
      fext := UpperCase( GetFileExt(Path) ) ;
      if fext = '.SDAT' then
        sfname := GetPathNoExt( Path ) + '.sgrd'
      else
        sfname := Path ;

      if SafeFileExists( sfname ) then begin
        try
          parseHeader( sfname ) ;
        except
          scaleX := 0 ;
          scaleY := 0 ;
          Extent := GisExtent( 0,0,0,0 ) ;
        end ;
      end
      else
        Extent := GisExtent( 0,0,0,0 ) ;

      realBitCount  := 24 ;
      colorsNo      := 0 ;
      FAntialias    := True ;
      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;

      SetLength( lineBuffer, FBitWidth ) ;
      lineInBuffer := -1 ;

      // open binary file
      if fext = '.SDAT' then
        bufStream := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read )
      else begin
        sfname := GetPathNoExt( Path ) + '.sdat' ;
        bufStream := TGIS_BufferedFileStream.Create( sfname, TGIS_StreamMode.Read ) ;
      end ;

      if FMinZ = FMaxZ then begin
        FMaxZ := -GIS_MAX_SINGLE  ;
        FMinZ :=  GIS_MAX_SINGLE  ;
        prepareMinMaxZ ;
      end ;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;
      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'SAGA GIS Binary Grid (SGRD)' + #13#10 +
                           '(%d x %d %d byte)',
                           [ FBitWidth, FBitHeight, cellByteSize ]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end ;

  procedure TGIS_LayerSGRD.prepareMinMaxZ(
    const _zoom : Double = -1
  ) ;
  var
    i,j,k : Integer ;
    zoom  : Double ;
    bytes : Int64 ;
    lines : Double ;
    fact  : Double ;
  const
    MAX_LINES = 750 ;
  begin
    if not assigned( bufStream ) then exit ; // call Alive ?

    bytes := FBitWidth * cellByteSize ;
    if (_zoom > 0) AND (_zoom <= 1) then begin
      lines := RoundS(FBitHeight * _zoom) ;
      if lines = 0 then
        lines := 1 ;
    end
    else begin
      if MAX_LINES > FBitHeight then
        lines := FBitHeight
      else
        lines := MAX_LINES ;
    end ;

    zoom := lines/FBitHeight ;
    fact := zoom/2 ;

    for i := 0 to FBitHeight -1 do begin
      fact := fact + zoom ;
      if fact >= 1 then begin
        fact := fact - 1 ;
        bufStream.Seek( bytes * i, soBeginning ) ;
        for j := 0 to FBitWidth-1 do
          lineBuffer[j] := readValue ;

        if bigEndian then
          convertLineToLE ;

        for k := 0 to FBitWidth -1 do begin
          if lineBuffer[k] <> NoDataValue then begin
            if lineBuffer[k] < FMinZ then
              FMinZ := lineBuffer[k] ;
            if lineBuffer[k] > FMaxZ then
              FMaxZ := lineBuffer[k] ;
          end ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_LayerSGRD.readValue : Single ;
  var
    bval  : Byte ;
    sval  : Int16 ;
    suval : Word ;
    fval  : Single ;
    dval  : Double ;
    ival  : Integer ;
    cval  : Cardinal ;
  begin
    Result := 0 ;
    case cellType of
      SGRD_DTYPE_BYTE : begin
        bufStream.ReadByte( bval, sizeOf(Byte) ) ;
        Result := bval ;
      end ;
      SGRD_DTYPE_UINT16 : begin
        bufStream.ReadWord( suval, sizeOf(UInt16) ) ;
        Result := suval ;
      end ;
      SGRD_DTYPE_INT16 : begin
        bufStream.ReadSmallInt( sval, sizeOf(Int16) ) ;
        Result := sval ;
      end ;
      SGRD_DTYPE_UINT32 : begin
        bufStream.ReadCardinal( cval, sizeOf(Cardinal) ) ;
        Result := cval ;
      end ;
      SGRD_DTYPE_INT32 : begin
        bufStream.ReadInteger( ival, sizeOf(Integer) ) ;
        Result := ival ;
      end ;
      SGRD_DTYPE_FLOAT : begin
        bufStream.ReadSingle( fval, sizeOf(Single) ) ;
        Result := fval ;
      end ;
      SGRD_DTYPE_DOUBLE : begin
        bufStream.ReadDouble( dval, sizeOf(Double) ) ;
        Result := dval ;
      end ;
    end ;
  end ;

  function TGIS_LayerSGRD.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  begin
    Result := inherited ;

    Result.Clear ;
    Result.Add( TGIS_LayerPixelSubFormat.Create(
                  TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID,
                  TGIS_CompressionType.None,
                  0
                )
              ) ;
  end ;

  function TGIS_LayerSGRD.findOffset(
    const _line : Integer
  ) : Int64 ;
  begin
    if topToBottom then
      Result := Int64(_line) * baseCellWidth * cellByteSize
    else
      Result := Int64(FBitHeight-_line-1) * baseCellWidth * cellByteSize ;
  end ;

  procedure TGIS_LayerSGRD.convertLineToLE ;
  var
    i  : Integer ;
    bd : TGIS_Bytes ;
  begin
    bd := TGIS_Bytes.Create(lineByteBuffer, 0, sizeOf(Single)*FBitWidth);
    try
      for i := 0 to FBitWidth -1 do
        lineBuffer[i] := bd.ReadSingle(sizeOf(Single)*i) ;
    finally
      FreeObject(bd) ;
    end ;
  end ;

  function TGIS_LayerSGRD.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i, j   : Integer;
    start  : Integer ;
    line   : Integer ;
    pixels : Integer ;
    offset : Int64 ;
    ccolor : TGIS_Color ;
  begin
    try
      pixels := _bytes div 3 ;
      line   := _linenr ;
      Result := _bytes ;

      if lineInBuffer <> line then begin
        offset := findOffset(_linenr) ;
        bufStream.Seek( offset, soBeginning ) ;
        for j := 0 to baseCellWidth-1 do
          lineBuffer[j] := readValue ;

        if bigEndian then
          convertLineToLE ;

        lineInBuffer := line ;
      end ;

      start := (_start div 3) ;
      offset := 0 ;
      pixels := pixels +start ;
      for i := start to pixels -2 do begin
        if lineBuffer[i] = NoDataValue then begin
          {$IFDEF OXYGENE}
            ccolor := colorNoData ;
            _buffer[_offset + offset    ] := ccolor.R ;
            _buffer[_offset + offset + 1] := ccolor.G ;
            _buffer[_offset + offset + 2] := ccolor.B ;
          {$ELSE}
            PInteger( NativeInt(_buffer) +offset )^ := colorNoData.ARGB ;
          {$ENDIF}
          makeTransparent := True ;
        end
        else
          {$IFDEF OXYGENE}
            begin
              ccolor := GetColorRamp( lineBuffer[i] ) ;
              _buffer[_offset + offset    ] := ccolor.R ;
              _buffer[_offset + offset + 1] := ccolor.G ;
              _buffer[_offset + offset + 2] := ccolor.B ;
            end ;
          {$ELSE}
            PInteger( NativeInt(_buffer) +offset )^ := NativeInt(GetColorRamp(
               lineBuffer[i] ).ARGB) ;
          {$ENDIF}
        offset := offset +3 ;
      end ;

      // last triple
      if lineBuffer[pixels -1] = NoDataValue then begin
        ccolor := colorNoData ;
        makeTransparent := True ;
      end
      else
        ccolor := GetColorRamp( lineBuffer[pixels -1] ) ;

      {$IFDEF OXYGENE}
        _buffer[_offset+offset  ] := Byte(ccolor.ARGB and $FF) ;
      {$ELSE}
        PByte( NativeInt(_buffer) +offset )^   := Byte(ccolor.ARGB and $FF) ;
      {$ENDIF}
      ccolor.ARGB := ccolor.ARGB shr 8 ;
      {$IFDEF OXYGENE}
        _buffer[_offset+offset+1] := Byte(ccolor.ARGB and $FF) ;
      {$ELSE}
        PByte( NativeInt(_buffer) +offset +1)^ := Byte(ccolor.ARGB and $FF) ;
      {$ENDIF}
      ccolor.ARGB := ccolor.ARGB shr 8 ;
      {$IFDEF OXYGENE}
        _buffer[_offset+offset+2] := Byte(ccolor.ARGB and $FF) ;
      {$ELSE}
        PByte( NativeInt(_buffer) +offset +2)^ := Byte(ccolor.ARGB and $FF) ;
      {$ENDIF}
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function  TGIS_LayerSGRD.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
  ) : Boolean ;
  var
    start  : Integer ;
    offset : Int64 ;
  begin
    Result := True ;
    try
      start  := _pt.X*4 ;
      offset := findOffset(_pt.Y) + start ;
      bufStream.Seek( offset, soBeginning ) ;

      _ar[0] := readValue ;

      if _ar[0] = NoDataValue then
        Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerSGRD.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    offset : Int64 ;
    i      : Integer ;
  begin
    Result := 0 ;
    try
      if (_linenr < 0) or (_linenr > FBitHeight) then exit ;

      if isBuilt then begin
        Result := inherited getNativeLine(_buffer, _linenr, _startIdx, _count) ;
        exit ;
      end ;

      if lineInBuffer <> _linenr then begin
        offset := findOffset(_linenr) ;
        bufStream.Seek( offset, soBeginning ) ;
        for i := 0 to baseCellWidth-1 do
          lineBuffer[i] := readValue ;

        if bigEndian then
          convertLineToLE ;

        lineInBuffer := _linenr ;
      end ;

      for i := 0 to _count-1 do
        _buffer[i] := lineBuffer[_startIdx +i] ;

      Result := _count ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  procedure TGIS_LayerSGRD.writeGrid(
    const _outStream  : TGIS_FileStream ;
    const _x          : Integer ;
    const _y          : Integer ;
    const _grd        : TGIS_GridArray
  ) ;
  var
    pos     : Int64 ;
    w, h    : Integer ;
    s, i    : Integer ;
    {$IFDEF OXYGENE}
      j     : Integer ;
      buf   : TBytes  ;
    {$ELSE}
      cnt   : Integer ;
    {$ENDIF}
  begin
     s := sizeOf( _grd[0][0] ) ;
     h := length( _grd ) ;
     w := length( _grd[0] ) ;

     if (h + _y) > FBitHeight then
       h := FBitHeight - _y ;
     if (w + _x) > FBitWidth then
       w := FBitWidth - _x ;

     {$IFNDEF OXYGENE}
       cnt := s * w ;
     {$ENDIF}
     pos := s * FBitWidth * _y + s * _x ;

     for i := 0 to h-1 do begin
       _outStream.Position := pos ;
       {$IFDEF OXYGENE}
         for j := 0 to w-1 do begin
           buf := BitConverter.GetBytes( _grd[i][j] ) ;
           _outStream.Write( buf, length( buf ) ) ;
         end ;
       {$ELSE}
         _outStream.Write( _grd[i][0], cnt ) ;
       {$ENDIF}
       pos := pos + FBitWidth * s ;
     end ;
  end ;

  function TGIS_LayerSGRD.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  var
    grda        : TGIS_GridArray ;
    offset      : Int64 ;
    i, k        : Integer ;
    cw, ch      : Integer ;
    lw, lh      : Integer ;
    maxc, maxr  : Integer ;
    ext         : TGIS_Extent {$IFDEF GIS_NORECORDS}= new TGIS_Extent{$ENDIF} ;
    sx, sy      : Double ;
    outStream   : TGIS_FileStream ;
    scale_x   : Double ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;
    scale_x := ( FExtent.XMax - FExtent.XMin ) / FBitWidth  ;
    if RoundS(( FExtent.YMax - FExtent.YMin ) / scale_x) <> FBitHeight then
      FBitHeight := RoundS(( FExtent.YMax - FExtent.YMin ) / scale_x) ;


    writeHeader( GetPathNoExt( Path ) + '.sgrd' ) ;

    // save projection to prj file
    if assigned( CS ) and not IsEmbeddedSQLPath( Path ) then
      CS.SaveAsWKTFile( GetPathNoExt( Path ) + GIS_PRJ_EXT ) ;

    outStream := TGIS_FileStream.Create( Path, fmCreate ) ;
    try
      offset := Int64(sizeOf(Single)) * FBitWidth * FBitHeight ;
      {$IFDEF CLR}
        outStream.SetSize( offset ) ;
      {$ELSE}
        outStream.Size := offset ;
      {$ENDIF}
      outStream.Position := 0 ;

      if FBitHeight <= MAX_CELL_WH then
        ch := FBitHeight
      else
        ch := MAX_CELL_WH ;

      if FBitWidth <= MAX_CELL_WH then
        cw := FBitWidth
      else
        cw := MAX_CELL_WH ;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if (maxr > 0) then begin
        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end;
         grda :=  InitializeGrid(ch,cw) ;
      end ;

      for i := 0 to maxr -1 do begin
        ext.YMax := FExtent.YMax -i*(ch*sy) ;
        ext.YMin := ext.YMax -ch*sy ;
        for k := 0 to maxc -1 do begin
          ext.XMin := FExtent.XMin +k*(cw*sx) ;
          ext.XMax := ext.XMin +cw*sx ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          writeGrid(outStream,k*cw, i*ch, grda);
        end;
        if lw <> 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(ch, lw) ;
          ext.XMin := FExtent.XMax -lw*sx;
          ext.XMax := FExtent.XMax ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          writeGrid(outStream,maxc*cw, i*ch, grda );
          grda := nil ;
          grda := InitializeGrid(ch, cw) ;
        end;
      end;

      if lh <> 0 then begin
        ext.YMin := FExtent.YMin ;
        ext.YMax := FExtent.YMin +(lh*sy) ;
        if maxc > 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(lh, cw) ;

          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            setNoDataTable( grda ) ;
            _layer.GetGrid(ext, grda) ;
            writeGrid(outStream,k*cw, maxr*ch, grda);
          end;
        end ;

        if lw <> 0 then begin
          ext.XMin := FExtent.XMax -(lw*sx) ;
          ext.XMax := FExtent.XMax ;
          grda := nil ;
          grda :=  InitializeGrid(lh, lw) ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          writeGrid(outStream,maxc*cw, maxr*ch, grda);
        end;
      end;
      grda := nil ;
    finally
      FreeObject( outStream ) ;
    end ;
    Result := True ;
  end ;

  procedure TGIS_LayerSGRD.Build(
    const _path      : String         ;
    const _grid      : Boolean        ;
    const _cs        : TGIS_CSCoordinateSystem  ;
    const _ext       : TGIS_Extent    ;
    const _width     : Integer        ;
    const _height    : Integer ;
    const _subformat : TGIS_LayerPixelSubFormat
  ) ;
  var
    offset  : Int64 ;
    oStream : TGIS_FileStream ;
  begin
    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    inherited Build(_path, _grid, _cs, _ext, _width, _height, _subformat) ;

    if not IsStringEmpty( _path ) then begin
      oStream := TGIS_FileStream.Create( _path, fmCreate ) ;
      try
        offset := Int64(sizeOf(Single)) * FBitWidth * FBitHeight ;
        {$IFDEF CLR}
          oStream.SetSize( offset ) ;
        {$ELSE}
          oStream.Size := offset ;
        {$ENDIF}
        oStream.Position := 0 ;
      finally
        FreeObject( oStream ) ;
      end ;

      writeHeader( GetPathNoExt( _path ) + '.sgrd' ) ;
    end ;

    cellByteSize := 4 ;
    cellType     := SGRD_DTYPE_FLOAT ;
    lineInBuffer := -1 ;
    SetLength( lineBuffer, FBitWidth ) ;
  end ;

  procedure TGIS_LayerSGRD.SaveData ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc      : Integer ;
    maxr      : Integer ;
    grda      : TGIS_GridArray ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS}= new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
    offset    : Int64 ;
    outStream : TGIS_FileStream ;
  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    if not isBuilt and IsStringEmpty( Path ) then exit ;

    outStream := TGIS_FileStream.Create( Path, fmCreate ) ;
    try
      offset := Int64(sizeOf(Single)) * FBitWidth * FBitHeight ;
      {$IFDEF CLR}
        outStream.SetSize( offset ) ;
      {$ELSE}
        outStream.Size := offset ;
      {$ENDIF}
      outStream.Position := 0 ;

      cw   := FBitWidth ;
      ch   := FBitHeight ;
      maxc := FBitWidth div cw ;
      lw   := FBitWidth mod cw ;
      maxr := FBitHeight div ch ;
      lh   := FBitHeight mod ch ;
      sx   := (FExtent.XMax - FExtent.XMin) / FBitWidth  ;
      sy   := (FExtent.YMax - FExtent.YMin) / FBitHeight ;

      if (maxr > 0) then begin
        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end ;
        grda := InitializeGrid( ch, cw ) ;
      end ;

      for i := 0 to maxr -1 do begin
        ext.YMax := FExtent.YMax -i*(ch*sy) ;
        ext.YMin := ext.YMax -ch*sy ;

        for k := 0 to maxc -1 do begin
          ext.XMin := FExtent.XMin +k*(cw*sx) ;
          ext.XMax := ext.XMin +cw*sx ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          writeGrid(outStream,k*cw, i*ch, grda);
        end ;

        if lw <> 0 then begin
          grda := nil ;
          grda := InitializeGrid(ch, lw) ;
          ext.XMin := FExtent.XMax -lw*sx;
          ext.XMax := FExtent.XMax ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          writeGrid(outStream,maxc*cw, i*ch, grda );
          grda := nil ;
          grda := InitializeGrid(ch, cw) ;
        end ;
      end ;

      if lh <> 0 then begin
        ext.YMin := FExtent.YMin ;
        ext.YMax := FExtent.YMin +(lh*sy) ;
        if maxc > 0 then begin
          grda := nil ;
          grda := InitializeGrid(lh, cw) ;

          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            getGridData(ext, grda) ;
            writeGrid(outStream,k*cw, maxr*ch, grda);
          end ;
        end ;

        if lw <> 0 then begin
          ext.XMin := FExtent.XMax -(lw*sx) ;
          ext.XMax := FExtent.XMax ;
          grda := nil ;
          grda := InitializeGrid(lh, lw) ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          writeGrid(outStream,maxc*cw, maxr*ch, grda);
        end ;
      end ;

      grda := nil ;
    finally
      FreeObject( outStream ) ;
    end ;

    writeHeader( GetPathNoExt( Path ) + '.sgrd' ) ;

    // save projection to prj file
    if assigned( CS ) and not IsEmbeddedSQLPath( Path ) then
      CS.SaveAsWKTFile( GetPathNoExt( Path ) + GIS_PRJ_EXT ) ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerSGRD.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-SGRD', 'SAGA GIS Binary Grid', TGIS_LayerSGRD, '.sdat',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   True
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerSGRD.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

