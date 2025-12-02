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
  Encapsulation of an Arcinfo Float(Binary) Grid layer.
}

{$IFDEF DCC}
  unit GisLayerFLT ;
  {$HPPEMIT '#pragma link "GisLayerFLT"'}
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

    GisFileFLT,
    GisTypes,
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
  Unit_GisLayerFLT = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of an Arcinfo Float(Binary) Grid layer.
  /// </summary>
  TGIS_LayerFLT = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // various private variables
    //From Grid

      /// <summary>
      ///   Byte order from most significant to least significant. If True then
      ///   msbfirst. If false then lsbfirst.
      /// </summary>
      bigEndian : Boolean ;

      /// <summary>
      ///   line buffer (line of singles).
      /// </summary>
      lineBuffer : array of Single ;

      /// <summary>
      ///   line buffer (line of singles).
      /// </summary>
      lineByteBuffer : TBytes ;

      /// <summary>
      ///   Number of lines present in lineBuffer.
      /// </summary>
      lineInBuffer  : Integer ;
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
    GisRegistredLayers,
    GisStreams ;
{$ENDIF}

//==============================================================================
// TGIS_LayerFLT
//==============================================================================

  constructor TGIS_LayerFLT.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable ] ;
    FIsGridImage := True;
    FIsNativeGridImage := True ;
    FBandsCount := 1 ;
  end ;

  procedure TGIS_LayerFLT.Build(
    const _path      : String         ;
    const _grid      : Boolean        ;
    const _cs        : TGIS_CSCoordinateSystem  ;
    const _ext       : TGIS_Extent    ;
    const _width     : Integer        ;
    const _height    : Integer ;
    const _subformat : TGIS_LayerPixelSubFormat
  ) ;
  begin
    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    inherited Build(_path, _grid, _cs, _ext, _width, _height, _subformat) ;
  end ;

  procedure TGIS_LayerFLT.SaveData ;
  var
    i, k : Integer ;
    cw, ch : Integer ;
    lw, lh : Integer ;
    maxc, maxr : Integer ;
    f : TGIS_FileFLT ;
    grda : TGIS_GridArray ;
    ext : TGIS_Extent {$IFDEF GIS_NORECORDS}= new TGIS_Extent{$ENDIF} ;
    sx, sy : Double ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    if not isBuilt then exit ;

    f := TGIS_FileFLT.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;
    try
      if not assigned(f) then exit ;
      if FBitHeight <> f.Height then
        FBitHeight := f.Height ;

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
          getGridData(ext, grda) ;
          f.WriteGrid(k*cw, i*ch, grda);
        end;
        if lw <> 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(ch, lw) ;
          ext.XMin := FExtent.XMax -lw*sx;
          ext.XMax := FExtent.XMax ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          f.WriteGrid(maxc*cw, i*ch, grda );
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
            getGridData(ext, grda) ;
            f.WriteGrid(k*cw, maxr*ch, grda);
          end;
        end ;

        if lw <> 0 then begin
          ext.XMin := FExtent.XMax -(lw*sx) ;
          ext.XMax := FExtent.XMax ;
          grda := nil ;
          grda :=  InitializeGrid(lh, lw) ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          f.WriteGrid(maxc*cw, maxr*ch, grda);
        end;
      end;

      grda := nil ;
    finally
      FreeObject(f) ;
    end;
  end;

  function TGIS_LayerFLT.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  var
    i, k : Integer ;
    cw, ch : Integer ;
    lw, lh : Integer ;
    maxc, maxr : Integer ;
    f : TGIS_FileFLT ;
    grda : TGIS_GridArray ;
    ext : TGIS_Extent {$IFDEF GIS_NORECORDS}= new TGIS_Extent{$ENDIF} ;
    sx, sy : Double ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;

    f := TGIS_FileFLT.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;
    try

      if not assigned(f) then exit ;

      if FBitHeight <> f.Height then begin
        FBitHeight := f.Height ;
        scaleY := -scaleX ;
      end;

      if FBitWidth <= MAX_CELL_WH then
        cw := FBitWidth
      else
        cw := MAX_CELL_WH ;

     if FBitHeight <= MAX_CELL_WH then
        ch := FBitHeight
      else
        ch := MAX_CELL_WH ;

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
          f.WriteGrid(k*cw, i*ch, grda);
        end;
        if lw <> 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(ch, lw) ;
          ext.XMin := FExtent.XMax -lw*sx;
          ext.XMax := FExtent.XMax ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          f.WriteGrid(maxc*cw, i*ch, grda );
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
            f.WriteGrid(k*cw, maxr*ch, grda);
          end;
        end ;

        if lw <> 0 then begin
          ext.XMin := FExtent.XMax -(lw*sx) ;
          ext.XMax := FExtent.XMax ;
          grda := nil ;
          grda :=  InitializeGrid(lh, lw) ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          f.WriteGrid(maxc*cw, maxr*ch, grda);
        end;
      end;

      grda := nil ;
    finally
      FreeObject(f) ;
    end;
    Result := True ;
  end;

  procedure TGIS_LayerFLT.doDestroy ;
  begin
    Dormant ;

    FreeObject( fileStream ) ;

    inherited ;
  end ;

  procedure TGIS_LayerFLT.setUp ;
  var
    ext             : TGIS_Extent ;
    fext            : String ;
    fname           : String ;
    ffile           : TGIS_BufferedFileStream ;

    procedure setHdrInfo ;
    var
      txt   : array [0..1] of String ;
      xllCenter : Double ;
      yllCenter : Double ;
      isCenter : Boolean ;

      procedure read_param ;
      var
        idx, tidx : Integer ;
        idxmax : Integer ;
        lineStrBuffer : String ;
      begin
        txt[0] := '' ;
        txt[1] := '' ;
        if not ffile.Eof then begin
          lineStrBuffer := ffile.ReadLine ;
          idxmax := StringLast(lineStrBuffer) ;
          tidx := 0 ;
          for idx := StringFirst to idxmax do begin
            case lineStrBuffer[idx] of
              #13,
              #32 :
                begin
                  if (tidx = 0) and ( not IsStringEmpty( txt[0] ) ) then
                    tidx := 1
                  else if (tidx = 1) and ( not IsStringEmpty( txt[1] ) ) then
                    exit ;
                  continue ;
                end ;
              else  txt[tidx] := txt[tidx] + UpCase(lineStrBuffer[idx]) ;
            end ;
          end ;
        end ;
      end ;

    begin

      isCenter := False ;
      xllCenter := 0 ;
      yllCenter := 0 ;
      scaleY := 0;

      repeat
        read_param ;
        if IsStringEmpty( txt[0] ) then break ;

        if txt[0] = 'NCOLS'        then FBitWidth  := StrToInt(txt[1])
        else
        if txt[0] = 'NROWS'        then FBitHeight := StrToInt(txt[1])
        else
        if txt[0] = 'XLLCORNER'    then ext.XMin   := DotStrToFloat(txt[1])
        else
        if txt[0] = 'YLLCORNER'    then ext.YMin   := DotStrToFloat(txt[1])
        else
        if txt[0] = 'XLLCENTER'    then
                                      begin
                                        xllCenter := DotStrToFloat(txt[1]) ;
                                        isCenter  := True ;
                                      end
        else
        if txt[0] = 'YLLCENTER'    then
                                      begin
                                        yllCenter := DotStrToFloat(txt[1]) ;
                                        isCenter := True ;
                                      end
        else
        if txt[0] = 'CELLSIZE'     then
                                      begin
                                        scaleX := DotStrToFloat(txt[1]) ;
                                        scaleY := -scaleX ;
                                      end
        else
        if txt[0] = 'XDIM'         then scaleX := DotStrToFloat(txt[1])
        else
        if txt[0] = 'YDIM'         then scaleY := - DotStrToFloat(txt[1])
        else
        if txt[0] = 'NODATA_VALUE' then
                                      begin
                                        Params.Pixel.GridNoValue := DotStrToFloat(txt[1]) ;
                                        FNoDataValue := Params.Pixel.GridNoValue;
                                        redTransp[0]   := BASE_TRANSPARENT_FLAG ;
                                        greenTransp[0] := BASE_TRANSPARENT_FLAG ;
                                        blueTransp[0]  := BASE_TRANSPARENT_FLAG ;
                                      end
        else
        if txt[0] = 'BYTEORDER'    then
                                      begin
                                        if txt[1] = 'MSBFIRST' then
                                          bigEndian := True
                                        else
                                          bigEndian := False ;
                                      end
        else
        if not (AnsiChar(txt[0][StringFirst]) in [ 'A'..'Z', 'a'..'z' ])
        then
          break ;

      until IsStringEmpty( txt[0] ) ;

      if scaleY = 0 then
        scaleY := -scaleX ;

      if isCenter then begin
        ext.XMin := xllCenter - (FBitWidth  * scaleX)/2 ;
        ext.YMin := yllCenter + (FBitHeight * scaleY)/2 ;
      end ;

      ext.XMax := ext.XMin + (FBitWidth  * scaleX) ;
      ext.YMax := ext.YMin - (FBitHeight * scaleY) ;

      Extent := ext ;
    end ; //end of setHdrInfo
  begin

    if isBuilt then
      exit ;
    ext := GisExtent( 0,0,0,0 ) ;

    try

      fext := UpperCase( GetFileExt(Path) ) ;
      if fext = '.FLT' then begin
        fname := GetPathNoExt( Path ) + '.hdr' ;
      end ;

      if SafeFileExists( fname ) then begin
        try
          try
            ffile := TGIS_BufferedFileStream.Create( fname, TGIS_StreamMode.Read ) ;
            setHdrInfo ;
          finally
            FreeObject( ffile ) ;
          end ;
        except
          scaleX := 0 ;
          scaleY := 0 ;
          Extent := ext;
        end ;
      end
      else
        Extent := ext;

      realBitCount := 24 ;
      colorsNo := 0 ;
      FAntialias := True ;
      realLineWidth := ( FBitWidth*realBitCount +7 ) div 8 ;
      SetLength( lineBuffer, FBitWidth ) ;

      lineInBuffer := -1 ;

      // open binary file
      fileStream := openBufferedFileStream( Path ) ;

      if FMinZ = FMaxZ then begin
        FMaxZ   := -GIS_MAX_SINGLE  ;
        FMinZ   :=  GIS_MAX_SINGLE  ;
        prepareMinMaxZ ;
      end;

      Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                               Extent.XMax, Extent.YMax, FMaxZ
                              ) ;
      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'Arc/Info Float(Binary) Grid Format (FLT)' + #13#10 +
                           '%d x %d',
                           [ FBitWidth, FBitHeight ]
                         ) ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;


  function TGIS_LayerFLT.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FileFLT ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FileFLT.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  procedure TGIS_LayerFLT.convertLineToLE  ;
  var
    i : Integer ;
    bd : TGIS_Bytes ;
  begin
    bd := TGIS_Bytes.Create(lineByteBuffer, 0, sizeOf(Single)*FBitWidth);
    for i := 0 to FBitWidth -1 do begin
      lineBuffer[i] := bd.ReadSingle(sizeOf(Single)*i) ;
    end ;
    FreeObject(bd) ;
  end ;

  function TGIS_LayerFLT.getLine(  const _buffer : TBytes  ;
                                   const _offset : Integer ;
                                   const _linenr : Integer ;
                                   const _start  : Integer ;
                                   const _bytes  : Integer
                                 ) : Integer ;
  var
    i    : Integer;
    {$IFDEF OXYGENE}
      j  : Integer ;
    {$ENDIF}
    start    : Integer ;
    line     : Integer ;
    pixels   : Integer ;
    off64    : Int64 ;
    ccolor   : TGIS_Color ;
    offset   : Integer ;
  begin
    try

      pixels := _bytes div 3 ;
      line   := _linenr ;
      Result := _bytes ;

      if lineInBuffer <> line then begin
        off64 := Int64(line)*baseCellWidth*sizeOf(Single) ;
        fileStream.Seek( off64, soBeginning ) ;
        {$IFDEF OXYGENE}
          for j := 0 to baseCellWidth-1 do
            fileStream.ReadSingle( lineBuffer[j], sizeOf(Single) ) ;
        {$ELSE}
          fileStream.Read( ( Addr( lineBuffer[0]))^, baseCellWidth * sizeOf(Single) ) ;
        {$ENDIF}
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

  function  TGIS_LayerFLT.getNativeValue( const _pt : TPoint  ;
                                          const _ar : TGIS_DoubleArray
                                         ) : Boolean ;

  var
    start  : Integer ;
    off64  : Int64 ;
    bd     : TGIS_Bytes ;
  begin
    try
      Result := True ;

      start := _pt.X*4 ;

      off64 := Int64(_pt.Y*baseCellWidth)*sizeOf(Single) +start ;
      bd := TGIS_Bytes.Create(sizeOf(Single));
      fileStream.Seek( off64, soBeginning ) ;

      {$IFDEF OXYGENE}
        fileStream.Read( bd.Memory, sizeOf(Single) ) ;
      {$ELSE}
        fileStream.Read( bd.Memory^, sizeOf(Single));
      {$ENDIF}
      _ar[0] := bd.ReadSingle(0) ;
      FreeObject(bd) ;
      if _ar[0] = NoDataValue then
        Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerFLT.getNativeLine( const _buffer   : TGIS_SingleArray ;
                                        const _linenr   : Integer          ;
                                        const _startIdx : Integer          ;
                                        const _count    : Integer
                                      ) : Integer ;
  var
    offset64 : Int64 ;
    i    : Integer ;
  begin
    try
      Result := 0 ;

      if (_linenr < 0) or (_linenr > FBitHeight) then
        exit ;
      if isBuilt then begin
        Result := inherited getNativeLine(_buffer, _linenr, _startIdx, _count) ;
        exit ;
      end;

      if lineInBuffer <> _linenr then begin
        offset64 := Int64(_linenr)*baseCellWidth*sizeOf(Single) ;
        fileStream.Seek( offset64, soBeginning ) ;
        {$IFDEF OXYGENE}
          for i := 0 to baseCellWidth-1 do
            fileStream.ReadSingle( lineBuffer[i], sizeOf(Single) ) ;
        {$ELSE}
          fileStream.Read( ( Addr(lineBuffer[0]))^, baseCellWidth * sizeOf(Single) ) ;
        {$ENDIF}
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

  { Perform initialization section.
  }
  class procedure Unit_GisLayerFLT.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-FLT', 'Arcinfo Float Grid', TGIS_LayerFLT, '.flt',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   True
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerFLT.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

