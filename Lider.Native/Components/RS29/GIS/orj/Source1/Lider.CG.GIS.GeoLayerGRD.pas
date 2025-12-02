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
  Encapsulation of an Arcinfo ASCII Grid, Surfer ASCII Grid, Surfer 6 binary Grid
  and Surfer 7 binary Grid layers
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerGRD ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerGRD"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Types,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoRtl,

    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoFileGRD,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoStreams;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerGRD = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of an Arcinfo ASCII Grid, Surfer ASCII Grid, Surfer 6
  ///   binary Grid and Surfer 7 binary Grid
  /// </summary>
  TGIS_LayerGRD = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    protected // various protected variables

        /// <summary>
        ///   File Pos for Large ASCII
        /// </summary>
        rowsPos           : Array of Int64 ;

        /// <summary>
        ///   File Pos for Large ASCII
        /// </summary>
        firstCol          : Array of Single ;

        /// <summary>
        ///   File name
        /// </summary>
        fileName          : String ;

        /// <summary>
        ///   Input file when ASCII
        /// </summary>
        fileStream        : TGIS_Stream ;

        /// <summary>
        ///   Helper stream
        /// </summary>
        memoryStream      : TStream ;

        /// <summary>
        ///   Last read token from file stream
        /// </summary>
        fileToken         : String ;

        /// <summary>
        ///   line buffer (line of singles).
        /// </summary>
        lineBuffer        : array of Single ;

        /// <summary>
        ///   X-coordinate of the center.
        /// </summary>
        xllCenter         : Double ;

        /// <summary>
        ///   Y-coordinate of the center.
        /// </summary>
        yllCenter         : Double ;

        /// <summary>
        ///   Min x value of the grid
        /// </summary>
        xllCorner         : Double ;

        /// <summary>
        ///   Min y value of the grid
        /// </summary>
        yllCorner         : Double ;

        /// <summary>
        ///   Byte order from most significant to least significant. If True
        ///   then msbfirst. If false then lsbfirst.
        /// </summary>
        bigEndian         : Boolean ;

        /// <summary>
        ///   True, if row order is bottom up.
        /// </summary>
        reversedRowOrder  : Boolean ;

        /// <summary>
        ///   Is Binary surfer format.
        /// </summary>
        isBinary          : Boolean ;

        /// <summary>
        ///   Surfer format version.
        /// </summary>
        surferVersion     : Integer ;

        /// <summary>
        ///   Data position.
        /// </summary>
        dataPosition      : Int64 ;
    private

      /// <summary>
      ///   Get next token form the file
      /// </summary>
      /// <remarks>
      ///   Token will be stored in a fileToken variable
      /// </remarks>
      /// <returns>
      ///   extracted token
      /// </returns>
      function  getToken          : String ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines
      /// <inheritdoc/>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure setUp             ; override;

      /// <summary>
      ///   Reads and sets layer information.
      /// </summary>
      procedure setHdrInfo        ;

      /// <summary>
      ///   Test file format.
      /// </summary>
      procedure testFormat ;

      /// <summary>
      ///   Convert from big endian to little endian format.
      /// </summary>
      procedure convertLineToLE   ;

      /// <inheritdoc/>
      procedure prepareMinMaxZ    ( const _zoom : Double = -1
                                  ) ; override ;

      /// <summary>
      ///   Read the data form the file into the memory.
      /// </summary>
      procedure prepareImage      ;

      /// <inheritdoc/>
      function  getLine         ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                  ) : Integer; override  ;


      /// <inheritdoc/>
      function  getNativeValue    ( const _pt  : TPoint  ;
                                    const _ar  : TGIS_DoubleArray
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine     ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ; override;

     function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

    protected
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      procedure doDestroy ; override;
    public // API
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

      /// <inheritdoc/>
      function  PreRecognize     ( const _path     : String ;
                                     var _new_path : String
                                  ) : Boolean ; override;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers;
{$ENDIF}

const
  GRD_FMT_DSBB = 'DSBB' ; // surfer 6
  GRD_FMT_DSRB = 'DSRB' ; // surfer 7
  GRD_FMT_DSAA = 'DSAA' ; // surfer ascii
  GRD_FMT_NCOL = 'NCOL' ; // arcinfo
  GRD_FMT_NROW = 'NROW' ; // arcinfo
  GRD_FMT_HGPC = 'HGPC' ; // Northwood - via gdal
  MAX_GRID_ASCII_SIZE = 200000000 ;

{$IFNDEF OXYGENE}
  type
    PDWord = ^DWord ;
{$ENDIF}

//==============================================================================
// TGIS_LayerGRD
//==============================================================================

  constructor TGIS_LayerGRD.Create ;
  begin
    inherited ;

    FSubType := FSubType + [TGIS_LayerSubType.Persistent,
                            TGIS_LayerSubType.Exportable] ;

    FIsGridImage       := True;
    FIsNativeGridImage := True ;
    FBandsCount        := 1 ;
    memoryStream       := nil ;
  end ;

  procedure TGIS_LayerGRD.doDestroy ;
  begin
    Dormant ;

    SetLength( oGrid, 0, 0 ) ;
    if assigned(fileStream) then begin
      SetLength( firstCol, 0 ) ;
      SetLength( rowsPos, 0 ) ;
      FreeObject(fileStream) ;
      FreeObject(memoryStream) ;
    end ;
    inherited ;
  end ;

  procedure TGIS_LayerGRD.Build(
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

  procedure TGIS_LayerGRD.SaveData ;
  const
    GIS_GRD_NOVALUE = -9999 ;
    MAX_CELL_WH = 256 ;
  var
    i, k    : Integer ;
    cw, ch  : Integer ;
    lw, lh  : Integer ;
    maxc,
    maxr    : Integer ;
    f       : TGIS_FileGRD ;
    grda    : TGIS_GridArray ;
    ext     : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent {$ENDIF} ;
    sx, sy  : Double ;

    procedure set_nodata ;
    var
      m, n : Integer ;
    begin
      for m := low(grda) to high(grda) do
        for n := low(grda[0]) to high(grda[0]) do
          if grda[m][n] = GIS_GRID_NOVALUE then
            grda[m][n] := GIS_GRD_NOVALUE  ;
    end;

  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    
    if not isBuilt then exit ;

    f := TGIS_FileGRD.Create(
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



      if FBitHeight <= MAX_CELL_WH then
        ch := FBitHeight
      else
        ch := MAX_CELL_WH ;

      cw := FBitWidth ;

      maxc := FBitWidth div cw ;
      lw   := FBitWidth mod cw ;
      maxr := FBitHeight div ch ;
      lh   := FBitHeight mod ch ;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if (maxr > 0) then begin
        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end;
        grda :=  InitializeGrid(ch, cw);
      end ;

      for i := 0 to maxr -1 do begin
        ext.YMax := FExtent.YMax -i*(ch*sy) ;
        ext.YMin := ext.YMax -ch*sy ;
        for k := 0 to maxc -1 do begin
          ext.XMin := FExtent.XMin +k*(cw*sx) ;
          ext.XMax := ext.XMin +cw*sx ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          set_nodata ;
          f.WriteGrid(k*cw, i*ch, grda);
        end;
        if lw <> 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(ch, lw);
          ext.XMin := FExtent.XMax -lw*sx;
          ext.XMax := FExtent.XMax ;
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          set_nodata ;
          f.WriteGrid(maxc*cw, i*ch, grda );
          grda := nil ;
          grda :=  InitializeGrid(ch, cw);
        end;
      end;

      if lh <> 0 then begin
        ext.YMin := FExtent.YMin ;
        ext.YMax := FExtent.YMin +(lh*sy) ;
        if maxc > 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(lh, cw);

          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            getGridData(ext, grda) ;
            set_nodata ;
            f.WriteGrid(k*cw, maxr*ch, grda);
          end;
        end ;

        if lw <> 0 then begin
          ext.XMin := FExtent.XMax -(lw*sx) ;
          ext.XMax := FExtent.XMax ;
          grda := nil ;
          grda :=  InitializeGrid(lh, lw);
          setNoDataTable( grda ) ;
          getGridData(ext, grda) ;
          set_nodata ;
          f.WriteGrid(maxc*cw, maxr*ch, grda);
        end;
      end;

      grda := nil ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  function TGIS_LayerGRD.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  const
    GIS_GRD_NOVALUE = -9999 ;
    MAX_CELL_WH = 256 ;
  var
    i, k    : Integer ;
    cw, ch  : Integer ;
    lw, lh  : Integer ;
    maxc,
    maxr    : Integer ;
    f       : TGIS_FileGRD ;
    grda    : TGIS_GridArray ;
    ext     : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent {$ENDIF} ;
    sx, sy  : Double ;
    scale_x : Double ;

    procedure set_nodata ;
    var
      m, n : Integer ;
    begin
      for m := low(grda) to high(grda) do
        for n := low(grda[0]) to high(grda[0]) do
          if grda[m][n] = GIS_GRID_NOVALUE then
            grda[m][n] := GIS_GRD_NOVALUE  ;
    end;

  begin
    if IsStringEmpty( Path ) then exit ;

    scale_x := ( FExtent.XMax - FExtent.XMin ) / FBitWidth  ;
    if RoundS(( FExtent.YMax - FExtent.YMin ) / scale_x) <> FBitHeight then begin
      FBitHeight := RoundS(( FExtent.YMax - FExtent.YMin ) / scale_x) ;
      scaleY := -scale_x ;
    end;

    f := TGIS_FileGRD.Create(
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

      if FBitHeight <= MAX_CELL_WH then
        ch := FBitHeight
      else
        ch := MAX_CELL_WH ;

      cw := FBitWidth  ;

      maxc := FBitWidth div cw ;
      lw   := FBitWidth mod cw ;
      maxr := FBitHeight div ch ;
      lh   := FBitHeight mod ch ;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if (maxr > 0) then begin
        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end;
        grda :=  InitializeGrid(ch, cw);
      end ;

      for i := 0 to maxr -1 do begin
        ext.YMax := FExtent.YMax -i*(ch*sy) ;
        ext.YMin := ext.YMax -ch*sy ;
        for k := 0 to maxc -1 do begin
          ext.XMin := FExtent.XMin +k*(cw*sx) ;
          ext.XMax := ext.XMin +cw*sx ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          set_nodata ;
          f.WriteGrid(k*cw, i*ch, grda);
        end;
        if lw <> 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(ch, lw);
          ext.XMin := FExtent.XMax -lw*sx;
          ext.XMax := FExtent.XMax ;
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          set_nodata ;
          f.WriteGrid(maxc*cw, i*ch, grda );
          grda := nil ;
          grda :=  InitializeGrid(ch, cw);
        end;
      end;

      if lh <> 0 then begin
        ext.YMin := FExtent.YMin ;
        ext.YMax := FExtent.YMin +(lh*sy) ;
        if maxc > 0 then begin
          grda := nil ;
          grda :=  InitializeGrid(lh, cw);
          setNoDataTable( grda ) ;

          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            _layer.GetGrid(ext, grda) ;
            set_nodata ;
            f.WriteGrid(k*cw, maxr*ch, grda);
          end;
        end ;

        if lw <> 0 then begin
          ext.XMin := FExtent.XMax -(lw*sx) ;
          ext.XMax := FExtent.XMax ;
          grda := nil ;
          grda :=  InitializeGrid(lh, lw);
          setNoDataTable( grda ) ;
          _layer.GetGrid(ext, grda) ;
          set_nodata ;
          f.WriteGrid(maxc*cw, maxr*ch, grda);
        end;
      end;

      grda := nil ;
    finally
      FreeObject(f) ;
    end;
    Result := True ;
  end ;

  procedure TGIS_LayerGRD.setUp ;
  var
    ext  : TGIS_Extent ;
    fext : String ;
  begin
    if isBuilt then exit ;

    ext := GisExtent( 0,0,0,0 ) ;
    try
      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      try
        fext := UpperCase( GetFileExt(Path) ) ;
        if ( fext = '.GRD' ) or ( fext = '.AGR' ) or ( fext = '.ASC' ) then
          fileName := Path ;

        if assigned( Stream ) then begin
          Stream.Position := 0 ;
          memoryStream := TGIS_MemoryStream.Create ;
          memoryStream.CopyFrom( Stream, Stream.Size ) ;
          memoryStream.Position := 0 ;
          fileStream := TGIS_BufferedStream.Create( memoryStream ) ;
        end
        else
          fileStream := TGIS_BufferedFileStream.Create( fileName, TGIS_StreamMode.Read ) ;

        try
          testFormat ;
          setHdrInfo ;
        except
          scaleX  := 0 ;
          scaleY  := 0 ;
          Extent := ext ;
        end ;

        realBitCount := 24 ;
        colorsNo := 0 ;

        realLineWidth := ( FBitWidth * realBitCount +7 ) div 8 ;
        intLineWidth  := FBitWidth * 3 ;

        SetLength( lineBuffer, FBitWidth ) ;

        if ( FMaxZ = 0) and ( FMinZ = 0 ) then begin
          FMaxZ   := - GIS_MAX_SINGLE ;
          FMinZ   :=   GIS_MAX_SINGLE ;
        end ;

        prepareImage ;
        FIsGridImage := True ;
        FIsNativeGridImage := True ;


        if  FMaxZ < FMinZ  then begin
          FMaxZ   := - GIS_MAX_SINGLE ;
          FMinZ   :=   GIS_MAX_SINGLE ;
          prepareMinMaxZ ;
        end ;

        Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                                 Extent.XMax, Extent.YMax, FMaxZ
                                ) ;
        inherited ;

        if SafeFileExists( Path ) then
          FAge := GisFileAge( Path ) ;

        if isBinary then
          FFileInfo := Format( 'Golden Software %d Binary Grid Format (GRD)' +#13#10 +
                               '%d x %d',
                               [ surferVersion, FBitWidth, FBitHeight ]
                             )
        else if reversedRowOrder then
          FFileInfo := Format( 'Golden Software Surfer ASCII Grid Format (GRD)' +#13#10 +
                               '%d x %d',
                               [ FBitWidth, FBitHeight ]
                             )
        else
          FFileInfo := Format( 'Arc/Info ASCII Grid Format (GRD)' +#13#10 +
                               '%d x %d',
                               [ FBitWidth, FBitHeight ]
                             ) ;
      finally

        if not assigned(rowsPos) then
          FreeObject( fileStream ) ;
        RaiseBusyRelease( Self ) ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end ;

  procedure TGIS_LayerGRD.convertLineToLE  ;
  var
    i : Integer ;
    {$IFDEF OXYGENE}
    {$ELSE}
      dwval : DWord ;
    {$ENDIF}
  begin
    for i := 0 to FBitWidth -1 do begin
      {$IFDEF OXYGENE}
        lineBuffer[i] := SwapSingle( lineBuffer[i] ) ;
      {$ELSE}
        dwval := SwapLongInt( PDword( Addr( lineBuffer[i] ) )^ ) ;
        lineBuffer[i] := PSingle( Addr(dwval) )^ ;
      {$ENDIF}
    end ;
  end ;

  function TGIS_LayerGRD.getToken : String ;
  begin
    fileToken := String( fileStream.ReadToken ) ;
    Result := fileToken ;
  end ;

  procedure TGIS_LayerGRD.testFormat ;
  var
    fmt : String ;
  begin
    fileStream.Position := 0 ;
    fileStream.ReadAsciiString( fmt, 4 ) ;
    fmt := UpperCase( fmt ) ;

    if ( fmt = GRD_FMT_DSBB ) then begin
      isBinary      := True ;
      surferVersion := 6 ;
    end
    else if ( fmt = GRD_FMT_DSRB ) then begin
      isBinary      := True ;
      surferVersion := 7 ;
    end
    else
      isBinary := False ;

    fileStream.Position := 0 ;
  end ;

  procedure TGIS_LayerGRD.setHdrInfo ;
  var
    ext      : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    iscenter : Boolean     ;
    nTemp    : Word ;
    dTemp    : Double ;
    iTemp    : Integer ;
    nTag     : Integer ;
    nSize    : Integer ;
    nVersion : Integer ;
    minz     : Single ;
    maxz     : Single ;

    function _c( const _value : String ) : Boolean ;
    begin
      Result := CompareText( fileToken, _value ) = 0 ;
    end ;

  begin
    reversedRowOrder := False ;
    iscenter         := False ;

    maxz := - GIS_MAX_SINGLE ;
    minz :=   GIS_MAX_SINGLE ;

    if isBinary then begin
      {$IFDEF OXYGENE}
        Params.Pixel.GridNoValue := Single( 1.701410009187828e+38 ) ;
      {$ELSE}
        Params.Pixel.GridNoValue := 1.701410009187828e+38 ;
      {$ENDIF}
      if surferVersion = 6 then begin
        fileStream.Position := 4 ;
        fileStream.ReadWord( nTemp, 2 ) ;
        FBitWidth := nTemp ;
        fileStream.ReadWord( nTemp, 2 ) ;
        FBitHeight := nTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.XMin := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.XMax := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.YMin := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.YMax := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        minz := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        maxz := dTemp ;
      end
      else if surferVersion = 7 then begin
        fileStream.Position := 0 ;

        fileStream.ReadInteger( nTag, 4 ) ;
        assert( nTag = $42525344 ) ;
        fileStream.ReadInteger( nSize, 4 ) ;
        fileStream.ReadInteger( nVersion, 4 ) ;
        assert( (nVersion = 1) or (nVersion = 2) )  ;

        while ( nTag <> $44495247 ) do begin

          fileStream.ReadInteger( nTag, 4 ) ;
          fileStream.ReadInteger( nSize, 4 ) ;

          if ( nTag <> $44495247 ) then
            fileStream.Position := fileStream.Position + nSize ;
        end ;

        fileStream.ReadInteger( iTemp, 4 ) ;
        FBitHeight := iTemp ;
        fileStream.ReadInteger( iTemp, 4 ) ;
        FBitWidth := iTemp ;

        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.XMin := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.YMin := dTemp ;

        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.XMax := ext.XMin + dTemp * (FBitWidth - 1) ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        ext.YMax := ext.YMin + dTemp * (FBitHeight - 1) ; ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        minz := dTemp ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        maxz := dTemp ;

        fileStream.ReadDouble( dTemp, 8 ) ;
        fileStream.ReadDouble( dTemp, 8 ) ;
        Params.Pixel.GridNoValue := dTemp ;

        fileStream.ReadInteger( nTag, 4 ) ;
        assert( nTag = $41544144 ) ;
        fileStream.ReadInteger( nSize, 4 ) ;
        dataPosition := fileStream.Position ;
      end ;

      FNoDataValue := Params.Pixel.GridNoValue ;
      redTransp[0]   := BASE_TRANSPARENT_FLAG ;
      greenTransp[0] := BASE_TRANSPARENT_FLAG ;
      blueTransp[0]  := BASE_TRANSPARENT_FLAG ;

      scaleX := ( ext.XMax - ext.XMin ) / FBitWidth ;
      scaleY := ( ext.YMin - ext.YMax ) / FBitHeight ;
      reversedRowOrder := True ;
    end
    else begin
      repeat
        getToken ;
        if IsStringEmpty(fileToken) then break ;

        if      _c('NCOLS'       ) then
                                     FBitWidth      := StrToInt( getToken )
        else if _c('NROWS'       ) then
                                     FBitHeight     := StrToInt( getToken )
        else if _c('XLLCORNER'   ) then
                                     ext.XMin       := DotStrToFloat( getToken )
        else if _c('YLLCORNER'   ) then
                                     ext.YMin       := DotStrToFloat( getToken )
        else if _c('XLLCENTER'   ) then begin
                                     xllCenter      := DotStrToFloat( getToken ) ;
                                     iscenter       := True ;
                                   end
        else if _c('YLLCENTER'   ) then begin
                                     yllCenter      := DotStrToFloat( getToken ) ;
                                     iscenter       := True ;
                                   end
        else if _c('CELLSIZE'    ) then
                                     scaleX         := DotStrToFloat( getToken )
        else if _c('XDIM'        ) then
                                     scaleX         := DotStrToFloat( getToken )
        else if _c('YDIM'        ) then
                                     scaleY         := DotStrToFloat( getToken )
        else if _c('DX'          ) then
                                     scaleX         := DotStrToFloat( getToken )
        else if _c('DY'          ) then
                                     scaleY         := DotStrToFloat( getToken )
        else if _c('NODATA_VALUE') then begin
                                     Params.Pixel.GridNoValue := DotStrToFloat( getToken ) ;
                                     FNoDataValue   := Params.Pixel.GridNoValue ;
                                     redTransp[0]   := BASE_TRANSPARENT_FLAG ;
                                     greenTransp[0] := BASE_TRANSPARENT_FLAG ;
                                     blueTransp[0]  := BASE_TRANSPARENT_FLAG ;
                                   end
        else if _c('BYTEORDER'   ) then begin
                                     getToken ;
                                     if _c( 'MSBFIRST' ) then bigEndian := True
                                                         else bigEndian := False ;
                                   end
        else if _c('DSAA'        ) then begin
                                     reversedRowOrder := True ;

                                     FBitWidth      := StrToInt( getToken ) ;
                                     FBitHeight     := StrToInt( getToken ) ;
                                     ext.XMin       := DotStrToFloat( getToken ) ;
                                     ext.XMax       := DotStrToFloat( getToken ) ;
                                     ext.YMin       := DotStrToFloat( getToken ) ;
                                     ext.YMax       := DotStrToFloat( getToken ) ;
                                     minz           := DotStrToFloat( getToken ) ;
                                     maxz           := DotStrToFloat( getToken ) ;
                                     scaleX         := ( ext.XMax - ext.XMin )
                                                       / FBitWidth ;
                                     scaleY         := ( ext.YMin - ext.YMax )
                                                       / FBitHeight ;
                                     {$IFDEF OXYGENE}
                                       Params.Pixel.GridNoValue   := Single( 1.70141E+38 ) ;
                                     {$ELSE}
                                       Params.Pixel.GridNoValue   := 1.70141E+38 ;
                                     {$ENDIF}
                                     FNoDataValue := Params.Pixel.GridNoValue  ;
                                     redTransp[0]   := BASE_TRANSPARENT_FLAG ;
                                     greenTransp[0] := BASE_TRANSPARENT_FLAG ;
                                     blueTransp[0]  := BASE_TRANSPARENT_FLAG ;
                                   end
          else if not (AnsiChar(fileToken[StringFirst]) in [ 'A'..'Z', 'a'..'z' ]) then
                                     break ;
      until IsStringEmpty(fileToken) ;
    end ;

    if FMaxZ <= FMinZ then begin
      if maxz >= minz then begin
        FMaxZ := maxz ;
        FMinZ := minz ;
      end;
    end;
    if scaleY = 0 then
       scaleY := -scaleX ;

    if iscenter then begin
      ext.XMin := xllCenter - scaleX / 2 ;
      ext.YMin := yllCenter + scaleY / 2 ; //scaleY < 0
      ext.XMax := ext.XMin + ( FBitWidth  * Abs(scaleX) ) ;
      ext.YMax := ext.YMin + ( FBitHeight * Abs(scaleY) ) ;
    end
    else if isBinary or reversedRowOrder then begin
      ext.XMin := ext.XMin - scaleX / 2 ;
      ext.XMax := ext.XMax + scaleX / 2 ;
      ext.YMax := ext.YMax - scaleY / 2 ;
      ext.YMin := ext.YMin + scaleY / 2 ;
    end
    else begin
      ext.XMax := ext.XMin + ( FBitWidth  * Abs(scaleX) ) ;
      ext.YMax := ext.YMin + ( FBitHeight * Abs(scaleY) ) ;
    end ;

    Extent := ext ;

  end ;

  procedure TGIS_LayerGRD.prepareMinMaxZ(
    const _zoom : Double = -1
  ) ;
  var
    i, k : Integer ;
  begin
    if assigned(oGrid) then begin
      for k := 0 to FBitHeight -1 do begin
        for i := 0 to FBitWidth -1 do begin
          if oGrid[k][i] <> FNoDataValue then
          begin
            if oGrid[k][i] < FMinZ then
              FMinZ := oGrid[k][i] ;
            if oGrid[k][i] > FMaxZ then
              FMaxZ := oGrid[k][i] ;
          end ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerGRD.prepareImage  ;
  var
    i, j : Integer ;
    cnt  : Integer ;
    abrt : Boolean ;
    row  : Integer ;
    val7 : Double  ;
    val6 : Single ;
    minz : Single ;
    maxz : Single ;
  begin
    maxz := - GIS_MAX_SINGLE ;
    minz :=   GIS_MAX_SINGLE ;

    if isBinary then begin
      oGrid := InitializeGrid( FBitHeight, FBitWidth ) ;
    end
    else begin
      if FBitHeight*FBitWidth > MAX_GRID_ASCII_SIZE then begin
        SetLength(rowsPos, FBitHeight) ;
        SetLength(firstCol, FBitHeight) ;
      end
      else
        oGrid := InitializeGrid( FBitHeight, FBitWidth ) ;
    end ;

    cnt := 0 ;
    if isBinary then begin
      case surferVersion of
        6 : fileStream.Position := 56 ;
        7 : fileStream.Position := dataPosition ;
      end;
    end ;

    for i := 0 to FBitHeight -1 do begin
     if reversedRowOrder then
        row := (FBitHeight - 1 ) - i
      else
        row := i ;

      inc( cnt ) ;
      if cnt mod 200 = 1 then begin
        abrt := RaiseBusyShake( Self, i, FBitHeight ) ;
        if abrt then break ;
      end ;

      for j := 0 to FBitWidth -1 do begin
        if isBinary then begin
          case surferVersion of
            6 : begin
                  fileStream.ReadSingle( val6, 4 ) ;
                  oGrid[row][j] := val6 ;
                end;
            7 : begin
                  fileStream.ReadDouble( val7, 8 ) ;
                  oGrid[row][j] := val7 ;
                end;
          end;
        end
        else begin
          if IsStringEmpty(fileToken) then exit ;

          if not (AnsiChar(fileToken[StringFirst]) in [ '0'..'9', '-', '+']) then
            val6 := FNoDataValue
          else
            val6 := DotStrToFloat( fileToken ) ;

          if val6 <> FNoDataValue then
          begin
            if val6 < minz then
              minz := val6
            else
            if val6 > maxz then
              maxz := val6 ;
          end ;

          if assigned(rowsPos) then begin
            if j = 0 then begin
              firstCol[row] := val6 ;
              rowsPos[row] := fileStream.Position ;
            end ;
          end
          else
            oGrid[row][j] := val6 ;
          getToken ;
        end;
      end ;
    end ;

    if FMaxZ <= FMinZ then begin
      if maxz > minz then begin
        FMaxZ := maxz ;
        FMinZ := minz ;
      end;
    end;

    FSubFormat := Capabilities[0] ;

  end ;

  function TGIS_LayerGRD.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
    ) : Integer ;
  var
    i      : Integer;
    start  : Integer ;
    line   : Integer ;
    pixels : Integer ;
    offset : Integer ;
    ccolor : TGIS_Color ;
  begin
    try
      pixels := _bytes div 3 ;
      line   := _linenr ;
      Result := _bytes ;

      start := (_start div 3) ;

      offset := 0 ;
      if assigned(rowsPos) then begin
        lineBuffer[0] := firstCol[line] ;
        fileStream.Position := rowsPos[line] ;
        for i := 1 to start +pixels -1 do begin
          getToken ;
          lineBuffer[i] := DotStrToFloat( fileToken ) ;
        end;

        for i := start to start +pixels -2 do begin
          if lineBuffer[i] = FNoDataValue then begin
              ccolor := colorNoData ;
              _buffer[_offset + offset    ] := ccolor.B ;
              _buffer[_offset + offset + 1] := ccolor.G ;
              _buffer[_offset + offset + 2] := ccolor.R ;
            makeTransparent := True ;
          end else
            begin
              ccolor := GetColorRamp( lineBuffer[i] ) ;
              _buffer[_offset + offset    ] := ccolor.B ;
              _buffer[_offset + offset + 1] := ccolor.G ;
              _buffer[_offset + offset + 2] := ccolor.R ;
            end ;
          offset := offset +3 ;
        end ;

        // last triple
        if lineBuffer[start +pixels -1] = FNoDataValue then begin
          makeTransparent := True ;
          ccolor := colorNoData ;
        end else
          ccolor := GetColorRamp(lineBuffer[start +pixels -1]) ;

        _buffer[_offset+offset  ] := ccolor.B ;
        _buffer[_offset+offset+1] := ccolor.G ;
        _buffer[_offset+offset+2] := ccolor.R ;

        exit ;
      end;

      for i := start to start +pixels -2 do begin
        if oGrid[line][i] = FNoDataValue then begin
          ccolor := colorNoData ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
          makeTransparent := True ;
        end else
        begin
          ccolor := GetColorRamp( oGrid[line][i] ) ;
          _buffer[_offset + offset    ] := ccolor.B ;
          _buffer[_offset + offset + 1] := ccolor.G ;
          _buffer[_offset + offset + 2] := ccolor.R ;
          offset := offset +3 ;
      end ;

      // last triple
      if oGrid[line][start +pixels -1] = FNoDataValue then begin
        makeTransparent := True ;
        ccolor := colorNoData ;
      end else
        ccolor := GetColorRamp(oGrid[line][start +pixels -1]) ;

        _buffer[_offset+offset  ] := ccolor.B ;
        _buffer[_offset+offset+1] := ccolor.G ;
        _buffer[_offset+offset+2] := ccolor.R ;
      end;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function  TGIS_LayerGRD.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
  ) : Boolean ;
  var
    buf : TGIS_SingleArray ;
  begin
    try
       Result := True ;
      if (  _pt.Y < 0 ) or (  _pt.Y >= FBitHeight ) then
        exit ;

      if assigned(rowsPos) then begin
        SetLength(buf, _pt.X +1) ;
        getNativeLine(buf, _pt.Y, 0, _pt.X +1) ;
        _ar[0] := buf[_pt.X ] ;
        if _ar[0] = FNoDataValue then Result := False ;
        exit ;
      end;

      _ar[0] := oGrid[ _pt.Y][_pt.X ] ;

      if _ar[0] = FNoDataValue then Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerGRD.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := 0 ;
    try
      if ( _linenr < 0 ) or ( _linenr >= FBitHeight ) then exit ;

      if isBuilt then begin
        Result := inherited getNativeLine(_buffer, _linenr, _startIdx, _count) ;
        exit ;
      end;

      Result := _count ;

      if assigned(rowsPos) then begin
        lineBuffer[0] := firstCol[_linenr] ;
        fileStream.Position := rowsPos[_linenr] ;
        for i := 1 to _startIdx +_count -1 do begin
          getToken ;
          lineBuffer[i] := DotStrToFloat( fileToken ) ;
        end;
        for i := 0 to _count -1 do
          _buffer[i] := lineBuffer[i +_startIdx] ;
        exit ;
      end ;

      for i := 0 to _count -1 do
        _buffer[i] := oGrid[_linenr][i +_startIdx] ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerGRD.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FileGRD ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FileGRD.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  function TGIS_LayerGRD.PreRecognize(
    const _path     : String ;
     var _new_path  : String
  ) : Boolean ;
  var
    fmt : String ;
  begin
    if SafeFileExists( _path ) then begin
    fileStream := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
    try
      fileStream.Position := 0 ;
      if UpperCase( GetFileExt(_path) ) = '.ASC' then
        // read more to avoid whitespaces
        fileStream.ReadAsciiString( fmt, 10 )
      else
        fileStream.ReadAsciiString( fmt, 4 ) ;

      fmt := UpperCase( Trim(fmt) ) ;

      if ( fmt = GRD_FMT_DSBB ) then
        Result := True // surfer 6
      else if ( fmt = GRD_FMT_DSRB ) then
        Result := True // surfer 7
      else if ( fmt = GRD_FMT_DSAA ) then
        Result := True // surfer ascii
      else if ( Pos( GRD_FMT_NCOL, fmt ) >= StringFirst ) or
              ( Pos( GRD_FMT_NROW, fmt ) >= StringFirst )
      then
        Result := True // arcinfo
      else if ( fmt = GRD_FMT_HGPC ) then
        Result := False // Northwood - via gdal
      else
        Result := False ;
    finally
      if not assigned( rowsPos ) then
        FreeObject( fileStream ) ;
    end ;
    end
    else
      Result := True ;

    Result := inherited PreRecognize( _path, _new_path ) and Result ;
  end ;

  { Perform initialization section.
  }
  class procedure GisLayerGRD.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-GRD_AI', 'Arcinfo Ascii Grid',
                   TGIS_LayerGRD, '.asc',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
    RegisterLayer( 'DK-GRD_AI', 'Arcinfo Ascii Grid',
                   TGIS_LayerGRD, '.grd;.agr',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   False
                 ) ;
    RegisterLayer( 'DK-GRD_GSS', 'Golden Software Surfer Grid',
                   TGIS_LayerGRD, '.grd',
                   TGIS_RegisteredLayerType.Grid, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
  end;

{$IFNDEF OXYGENE}
  initialization
    GisLayerGRD.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

