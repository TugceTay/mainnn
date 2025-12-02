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
  Encapsulation of a PixelStore file access.
}

{$IFDEF DCC}
  unit GisLayerPixelStore ;
  {$HPPEMIT '#pragma link "GisLayerPixelStore"'}
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
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,
    System.SysUtils,

    GisTypes,
    GisTypesUI,
    GisLayerPixel,
    GisFilePixelStore,
    GisLayerPixelSql,
    GisCsSystems,
    GisLayer ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type
  /// <summary>
  ///   Encapsulation of PixelStore layer.
  /// </summary>
  TGIS_LayerPixelStoreAbstract = {$IFDEF OXYGENE} public {$ENDIF}
                                 class( TGIS_LayerPixelSqlAbstract )
    private  // various private variables
      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledPixObj   : TGIS_Pixels ;

      /// <summary>
      ///   Underlying bitmap which handles all images.
      /// </summary>
      pixObj         : TGIS_Pixels ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmpArr   : Array of TGIS_Pixels ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scIdx           : Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledWidthArr  : Array of Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledHeightArr : Array of Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledScopeSize : Integer ;

      /// <summary>
      ///  Offset in file to bitmap bytes.
      /// </summary>
       bitsOffset     : Integer ;
    protected // property internal values
      /// <summary>
      ///  Pixel store data encapsulation.
      /// </summary>
      FStore : TGIS_FilePixelStoreAbstract ;

      /// <summary>
      ///  Work (current) grid array from Store.
      /// </summary>
      arGrid : TGIS_GridArray ;
    private // various private routines
      /// <summary>
      ///   Prepare current cell parameters.
      /// </summary>
      procedure   prepareCell ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <summary>
      ///   setting up pixel store layer parameters
      /// </summary>
      procedure   setUp    ; override;

      /// <inheritdoc/>
      function  setFileScale      ( const _dwidth : Double ;
                                    const _swidth : Double
                                  ) : Double ; override;

      /// <inheritdoc/>
      function  getLinePixels    ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;
      /// <inheritdoc/>
      function  getLine         ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ; override;

      /// <summary>
      ///   Reading an image line.
      /// </summary>
      /// <param name="_buffer">
      ///   pointer
      /// </param>
      /// <param name="_offset">
      ///   start offset in _buffer
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   left margin (bytes to skip)
      /// </param>
      /// <param name="_bytes">
      ///   byte count
      /// </param>
      /// <returns>
      ///   read byte count
      /// </returns>
      function  getScaledLine   ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer;
      /// <summary>
      ///   Frees bitmaps used when Antialias is true
      /// </summary>
      procedure   freeScaledBmpList ;

      /// <inheritdoc/>
      function    getNativeValue(
                             const _pt       : TPoint           ;
                             const _ar       : TGIS_DoubleArray
                           ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine     ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ; override;

      /// <inheritdoc/>
      procedure   prepareMinMaxZ  ( const _zoom : Double = -1
                                  ) ; override ;

      function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // other functions

      /// <inheritdoc/>
      function  getBitmapData     ( const _extent   : TGIS_Extent ;
                                    const _bitmap   : TGIS_Pixels ;
                                    const _width    : Integer ;
                                    const _height    : Integer
                                  ) : Boolean ; override;


      /// <inheritdoc/>
      function  getGridData       ( const _extent   : TGIS_Extent      ;
                                    const _grid     : TGIS_GridArray
                                  ) : Boolean ; override;

    public
      {$IFDEF WINFORMS}
        function FireEventPassword( const _sender : System.Object ;
                                    const _e      : TGIS_TemplateProducerEventArgs
                                  ) : String ;
      {$ENDIF}
    protected

      /// <summary>
      ///   Destroy an instance and free PixelStore.
      /// </summary>
      procedure doDestroy ; override;
    public // API

      /// <inheritdoc/>
      constructor Create          ; override;


      /// <inheritdoc/>
      procedure Build             ( const _path   : String         ;
                                    const _grid   : Boolean        ;
                                    const _cs     : TGIS_CSCoordinateSystem  ;
                                    const _ext    : TGIS_Extent    ;
                                    const _width  : Integer        ;
                                    const _height : Integer        ;
                                    const _subformat : TGIS_LayerPixelSubFormat
                                  ) ; override;

      /// <inheritdoc/>
      function LocateEx           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_Color       ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean          ;
                                    const _pixelsize    : Double
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure Dormant           ; override;

      /// <inheritdoc/>
      function   GetAvailableLayers : TGIS_LayerInfoList ; override;

      /// <inheritdoc/>
      function    LockPixels       ( const _rct        : TRect ;
                                     const _ext        : TGIS_Extent ;
                                     const _tilelevel  : Integer ;
                                     const _tilerow    : Integer ;
                                     const _tilecolumn : Integer ;
                                     const _writable   : Boolean
                                   ) : TGIS_LayerPixelLock ; override;

      /// <inheritdoc/>
      procedure   UnlockPixels     ( {$IFDEF GIS_PDK}
                                       const
                                     {$ELSE}
                                       var
                                     {$ENDIF}
                                           _lock   : TGIS_LayerPixelLock
                                   ) ; override;
      /// <inheritdoc/>
      procedure SaveData   ; override;

      /// <inheritdoc/>
      procedure   InitializeWrite      ; override;

      /// <inheritdoc/>
      procedure   FinalizeWrite      ; override;

      /// <inheritdoc/>
      procedure ImportLayer     ( const _layer     : TGIS_LayerPixel  ;
                                  const _extent    : TGIS_Extent ;
                                  const _cs        : TGIS_CSCoordinateSystem ;
                                  const _width     : Cardinal ;
                                  const _height    : Cardinal ;
                                  const _subformat : TGIS_LayerPixelSubFormat
                                ) ; override;
    public // properties
      {#gendoc:hide:GENXDK}
      {#gendoc:hide:GENPDK}
      {#gendoc:hide:GENSCR}
      /// <summary>
      ///   Layer storage used for read write operations.
      /// </summary>
      property Store : TGIS_FilePixelStoreAbstract read FStore ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    GisRtl,
    GisClasses,
    GisDb,
    GisInternals,
    GisFunctions,
    GisParams,
    GisResource ;
{$ENDIF}

const
  BI_RGB = 0 ;

//==============================================================================
// TGIS_LayerPixelStoreAbstract
//==============================================================================

  constructor TGIS_LayerPixelStoreAbstract.Create ;
  begin
    inherited ;

    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    FIsTiled := True ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.freeScaledBmpList ;
  var
    i : Integer ;
    pix : TGIS_Pixels ;
  begin
    for i := low(scaledBmpArr) to high(scaledBmpArr) do begin
      pix := scaledBmpArr[i] ;

      if assigned(pix) then begin
        pix := nil ;
        scaledBmpArr[i] := nil ;
      end;
    end;
  end;

  procedure TGIS_LayerPixelStoreAbstract.doDestroy ;
  begin
    if assigned( gridCol ) then
      SetLength(gridCol, 0) ;

    if assigned( scaledBmpArr ) then begin
      freeScaledBmpList ;
      scaledBmpArr := nil ;
      scaledScopeSize := -1 ;
    end;
    if assigned( FStore ) then begin
      {$IFDEF WINFORMS}
        {$IFDEF OXYGENE}
          FStore.Password -= FireEventPassword ;
        {$ELSE}
          Exclude ( FStore.Password, FireEventPassword ) ;
        {$ENDIF}
      {$ELSE}
        FStore.PasswordEvent := nil ;
      {$ENDIF}
      FreeObject( FStore ) ;
    end ;
    inherited ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.prepareCell ;
  begin
    if IsGridImage or IsNativeGridImage then begin
      arGrid := FStore.CurrentCell.Grid.Grid ;
      if not assigned( gridCol ) then
        SetLength(gridCol, FCellHeight, 2) ;
    end ;
  end;

  procedure TGIS_LayerPixelStoreAbstract.setUp ;
  begin
    try
      assert( assigned( FStore ) ) ;

      FStore.Path := Path ;
      FStore.Name := Name ;
      if assigned( Viewer ) and assigned( Viewer.Ref.ProjectFile ) then
        FStore.RelativePath := Viewer.Ref.ProjectName ;

      scaledScopeSize := -1 ;

      {$IFDEF WINFORMS}
        {$IFDEF OXYGENE}
          FStore.Password += FireEventPassword ;
        {$ELSE}
          Include ( FStore.Password, FireEventPassword ) ;
        {$ENDIF}
      {$ELSE}
        FStore.PasswordEvent := PasswordEvent ;
      {$ENDIF}

      FStore.Open ;

      scaleX :=  FStore.PixelSize ;
      scaleY := -FStore.PixelSize ;

      FBitWidth   := 0 ;
      FCellWidth  := 0 ;
      FBitHeight  := 0 ;
      FCellHeight := 0 ;
      if FStore.PixelSize <> 0 then begin
        FBitWidth  := RoundS( ( FStore.Extent.XMax - FStore.Extent.XMin ) /
                             FStore.PixelSize
                           ) ;

        FBitHeight := RoundS( ( FStore.Extent.YMax - FStore.Extent.YMin ) /
                             FStore.PixelSize
                           ) ;
        FCellHeight := FStore.CellSize ;
        FCellWidth  := FStore.CellSize ;
        baseCellHeight := FStore.CellSize ;
        baseCellWidth  := FStore.CellSize ;
      end ;

      realBitCount := 24 ;

      FIsNativeGridImage := FStore.IsGrid ;

      if FStore.IsGrid then begin
        realLineWidth := ( FBitWidth * realBitCount +7 ) div 8 ;
        intLineWidth  := FBitWidth * 3 ;

        if IsStringEmpty( FFileInfo ) then
          FFileInfo := Format( 'PixelStore Grid Layer' +#13#10 +
                               '%d x %d',
                               [FBitWidth, FBitHeight]
                             ) ;
        FBandsCount := 1 ;
      end
      else begin
        if IsStringEmpty( FFileInfo ) then
          FFileInfo := Format( 'PixelStore Image Layer' +#13#10 +
                               '%d x %d',
                               [FBitWidth, FBitHeight]
                             ) ;
      end ;

      inherited ;

      SetCSByWKT( FStore.WKT ) ;

      Extent := FStore.Extent ;
      IsGridImage := FStore.IsGrid ;

      if FIsGridImage or IsNativeGridImage then begin
        if FMinZ > FMaxZ then
          prepareMinMaxZ ;
        Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                                 Extent.XMax, Extent.YMax, FMaxZ
                                ) ;
      end ;
      FAntialias := True ;

    except
      on e : Exception do begin
        if IsStringEmpty( Path ) then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                  Format( '%s; %s', [ FStore.Table, e.Message ] ),
                  0
                )
        else
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_LAYERSQLERROR ),
                  Format( '%s; %s', [ GetSafeSQLPath( Path ), e.Message ] ),
                  0
                ) ;
      end ;
    end ;

  end;

  function  TGIS_LayerPixelStoreAbstract.setFileScale(
    const _dwidth : Double ;
    const _swidth : Double
  ) : Double ;
  var
    w, h : Integer ;
    scale : Double ;
  begin
    if FAntialias and (not FIsGridImage) then begin
      if Abs(_dwidth -_swidth) < 0.5 then
        scale := 1
      else
        scale := (_dwidth +0.5)/_swidth ;
      if scale = 1 then begin
        Result := 1 ;
       {$IFDEF JAVA}
         pixObj := new Integer[FStore.CurrentCell.Bitmap.Height*
         FStore.CurrentCell.Bitmap.Width] ;
       {$ELSE}
         SetLength(pixObj,FStore.CurrentCell.Bitmap.Width*
                  FStore.CurrentCell.Bitmap.Height) ;
       {$ENDIF}
        FStore.CurrentCell.Bitmap.LockPixels(pixObj);
        exit ;
      end;

      if scale > 2 then
        scale := 2 ;

      w := RoundS( FStore.CurrentCell.Bitmap.Width * scale  ) ;
      if w < 1 then
        w := 1;
      h := RoundS( FStore.CurrentCell.Bitmap.Height * scale ) ;
      if h < 1 then
        h := 1 ;

      Result   := scale ;

      if (w = 1) and (h = 1)then begin
        scaledWidth  :=  1 ;
        scaledHeight :=  1 ;
        exit ;
      end;

      if assigned(scaledBmpArr) then begin
        if assigned(scaledBmpArr[scIdx]) then begin
          scaledPixObj := scaledBmpArr[scIdx] ;
          scaledWidth  :=  scaledWidthArr[scIdx] ;
          scaledHeight :=  scaledHeightArr[scIdx] ;
        end
      end
      else
        exit ;


      if (w <> scaledWidth) or (h <> scaledHeight) then begin
        scaledWidth  := w ;
        scaledHeight := h ;

        if  assigned(scaledPixObj) then begin
          {$IFDEF JAVA}
            FreeObject(scaledBmpArr[scIdx]) ;
          {$ELSE}
            SetLength(scaledBmpArr[scIdx], 0) ;
          {$ENDIF}
        end;
        {$IFDEF JAVA}
          scaledBmpArr[scIdx]:= new Integer[w * h] ;
        {$ELSE}
          SetLength(scaledBmpArr[scIdx], w * h) ;
        {$ENDIF}
        scaledPixObj := TGIS_Pixels(scaledBmpArr[scIdx] );
        scaledWidthArr[scIdx] := w ;
        scaledHeightArr[scIdx] := h ;
        scaledWidth := w ;
        scaledHeight := h ;

       {$IFDEF JAVA}
         pixObj := new Integer[FStore.CurrentCell.Bitmap.Height*
         FStore.CurrentCell.Bitmap.Width] ;
       {$ELSE}
         SetLength(pixObj,FStore.CurrentCell.Bitmap.Width*
                  FStore.CurrentCell.Bitmap.Height) ;
       {$ENDIF}
        FStore.CurrentCell.Bitmap.LockPixels(pixObj);

        TGIS_Bitmap.ScaleBitmap(pixObj, FStore.CurrentCell.Bitmap.Width,
                    FStore.CurrentCell.Bitmap.Height,
                    scaledPixObj, w, h, TGIS_ScalingFilter.Linear) ;
      end
      else
        scaledPixObj := scaledBmpArr[scIdx] ;
    end
    else begin
      scale := 1.0 ;
      {$IFDEF JAVA}
        pixObj := new Integer[FStore.CurrentCell.Bitmap.Height*
        FStore.CurrentCell.Bitmap.Width] ;
      {$ELSE}
         SetLength(pixObj,FStore.CurrentCell.Bitmap.Width*
                  FStore.CurrentCell.Bitmap.Height) ;
       {$ENDIF}
        FStore.CurrentCell.Bitmap.LockPixels(pixObj);
    end;
    Result   := scale ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.prepareMinMaxZ(
    const _zoom : Double = -1
  ) ;
  var
    i, k : Integer ;
    zm   : Double  ;
    col, row : Integer ;
    dx, dy : Double ;
  begin
    FMaxZ   := -GIS_MAX_SINGLE  ;
    FMinZ   :=  GIS_MAX_SINGLE  ;

    dx := Extent.XMax - Extent.XMin  ;
    dy := Extent.YMax - Extent.YMin  ;
    if dx < dy then
      zm := 1024 / dx
    else
      zm := 1024 / dy ;

    FStore.SetScope( Extent, zm ) ;
    FStore.MoveFirst ;
    while not FStore.Eof do begin
      if rgbAsGrid and (FGridBand = 0)then begin
        extZoom := 1 ;
        prepareMinMaxZGray ;
        FStore.MoveNext ;
        continue ;
      end;

      prepareCell ;
      arGrid := FStore.CurrentCell.Grid.Grid ;

      row := length(arGrid) ;
      col := length(arGrid[0]) ;
      for i := 0 to row -1 do begin
        for k := 0 to col -1 do begin
          if arGrid[i][k] <> NoDataValue then
          begin
            if arGrid[i][k] < FMinZ then
              FMinZ := arGrid[i][k] ;
            if arGrid[i][k] > FMaxZ then
              FMaxZ := arGrid[i][k] ;
          end ;
        end ;
      end ;
      FStore.MoveNext ;
    end ;
  end ;

  function  TGIS_LayerPixelStoreAbstract.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer;
  var
    i         : Integer ;
    pixl      : Integer ;
    pixidx    : Integer ;
    k         : Integer ;
    w         : Integer ;
    linenr,
    start     : Integer ;
    pixels,
    idx       : Integer ;
  begin
    pixels := _pixCount ;
    linenr := _linenr ;
    start  := _pixStart ;

    if extZoom = 1 then begin
      pixl := length(pixObj) ;
      if linenr >= FStore.CurrentCell.Bitmap.Height then
        linenr := FStore.CurrentCell.Bitmap.Height -1 ;

      pixidx := linenr * FStore.CurrentCell.Bitmap.Width +start;
      if pixidx < pixl  then begin
        for k := 0 to pixels -1 do begin
          _buffer[_offset+k] := pixObj[pixidx +k] ;
        end ;
      end ;
      Result := pixels ;
      exit ;
    end;

    if linenr >= scaledHeight then
      linenr := scaledHeight -1 ;
    w := scaledWidth ;
    if (start +pixels) > w then begin
      if start > 0 then begin
        start := w -pixels ;
        if start < 0 then begin
          start := 0 ;
          pixels := w ;
        end;
      end
      else
        pixels := w -start ;
    end;
    if pixels <= 0 then begin
      Result := 0 ;
      exit ;
    end;

    i := _offset ;
    if start +pixels > scaledWidth then begin
      start := scaledWidth -pixels ;
      if start < 0 then begin
        start := 0 ;
        pixels := scaledWidth ;
      end;
    end;

    idx := linenr*scaledWidth +start;
    for k := 0 to pixels -1 do begin
      _buffer[_offset +k] := scaledPixObj[idx +k]
    end ;

    Result := pixels ;
  end ;

  function TGIS_LayerPixelStoreAbstract.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i, k      : Integer ;
    start  : Integer ;
    line : Integer ;
    pixels : Integer ;
    pixidx : Integer ;
    offset : Integer ;
    ccolor : TGIS_Color ;
    pixl : Integer ;
  begin
   pixels := _bytes div 3 ;
   line   := _linenr ;
   Result := _bytes ;

   start := (_start div 3) ;
    offset := 0 ;

    if IsGridImage or IsNativeGridImage then begin
      try
       if (start + pixels) > length(arGrid[0]) then
         pixels := length(arGrid[0]) -start ;

      if _linenr >= length(arGrid) then begin
          Result := 0 ;
          exit ;
        end ;

        for i := start to start +pixels -2 do begin
          if arGrid[line][i] = FNoDataValue then begin
            ccolor := colorNoData ;
            _buffer[_offset + offset    ] := ccolor.B ;
            _buffer[_offset + offset + 1] := ccolor.G ;
            _buffer[_offset + offset + 2] := ccolor.R ;
            makeTransparent := True ;
          end
          else begin
            ccolor := GetColorRamp( arGrid[line][i] ) ;
            _buffer[_offset + offset    ] := ccolor.B ;
            _buffer[_offset + offset + 1] := ccolor.G ;
            _buffer[_offset + offset + 2] := ccolor.R ;
          end ;

          offset := offset +3 ;
        end ;

        // last triple
        if arGrid[line][start +pixels -1] = FNoDataValue then begin
          makeTransparent := True ;
          ccolor := colorNoData ;
        end else
          ccolor := GetColorRamp(arGrid[line][start +pixels -1]) ;

          gridCol[line][0] := prevShadowValue ;
          gridCol[line][1] := prevShadowDelta ;

          _buffer[_offset+offset    ] := ccolor.B ;
          _buffer[_offset+offset +1 ] := ccolor.G ;
          _buffer[_offset+offset +2 ] := ccolor.R ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), GetSafeSQLPath( Path ), 0 ) ;
      end ;
    end
    else begin
      i := 0 ;
      pixl := length(pixObj) ;
      if line >= FStore.CurrentCell.Bitmap.Height then
        line := FStore.CurrentCell.Bitmap.Height -1 ;

      pixidx := line * FStore.CurrentCell.Bitmap.Width +start;
      if pixidx < pixl  then begin
        for k := 0 to pixels -1 do begin
          _buffer[_offset+i]   :=  pixObj[pixidx] and $FF ;
          _buffer[_offset+i+1] :=  (pixObj[pixidx] shr 8 ) and $FF ;
          _buffer[_offset+i+2] :=  (pixObj[pixidx] shr 16) and $FF ;
          inc( i, 3 ) ;
          inc(pixidx) ;
          if pixidx >= pixl  then begin
            pixidx := 0 ;
            break ;
          end;
        end ;
      end ;
      Result := _bytes ;
    end;
  end ;

  function TGIS_LayerPixelStoreAbstract.getScaledLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i         : Integer ;
      k       : Integer ;
    w3        : Integer ;
    linenr, bytes, start : Integer ;
    pixels, idx : Integer ;
    c : TGIS_Color ;
  begin
    if extZoom = 1 then begin
      Result := getLine(_buffer,
                        _offset,
                        _linenr, _start , _bytes );
      exit ;
    end;
    linenr := _linenr ;
    bytes  := _bytes ;
    start  := _start div 3;

    if linenr >= scaledHeight then
      linenr := scaledHeight -1 ;
    w3 := scaledWidth*3 ;
    if (start +bytes) > w3 then begin
      if start > 0 then begin
        start := w3 -bytes ;
        if start < 0 then begin
          start := 0 ;
          bytes := w3 ;
        end;
      end
      else
        bytes := w3 -start ;
    end;
    pixels := bytes div 3 ;
    if bytes <= 0 then begin
      Result := 0 ;
      exit ;
    end;

    i := _offset ;
    if start +pixels > scaledWidth then begin
      start := scaledWidth -pixels ;
      if start < 0 then begin
        start := 0 ;
        pixels := scaledWidth ;
      end;
    end;

    idx := linenr*scaledWidth +start;
    for k := 1 to pixels do begin
      c :=  TGIS_Color.FromARGB( Cardinal(scaledPixObj[idx]) ) ;
      _buffer[i] := c.B ;
      inc( i ) ;
      _buffer[i] := c.G ;
      inc( i ) ;
      _buffer[i] := c.R ;
      inc( i ) ;
      inc(idx) ;
    end ;

    Result := bytes ;
  end ;

  function  TGIS_LayerPixelStoreAbstract.getNativeValue(
    const _pt : TPoint           ;
    const _ar : TGIS_DoubleArray
  ) : Boolean  ;
  begin
    Result := True ;
    _ar[0] := arGrid[_pt.Y][_pt.X] ;
  end ;

  function TGIS_LayerPixelStoreAbstract.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    i : Integer ;
  begin
    if IsNativeGridImage then begin
      Result := 0 ;
      try

        if ( _linenr < 0 ) or ( _linenr > FBitHeight ) then
          exit ;

        Result := _count ;
        for i := 0 to _count -1 do
          _buffer[i] := arGrid[_linenr][i +_startIdx] ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), GetSafeSQLPath( Path ), 0 ) ;
      end ;
    end
    else begin
      Result := inherited getNativeLine( _buffer, _linenr, _startIdx, _count) ;
    end ;
  end ;

  function TGIS_LayerPixelStoreAbstract.LocateEx(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_Color       ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean          ;
    const _pixelsize    : Double
  ) : Boolean ;
  var
    zoom          : Double      ;
    uptg, ptg     : TGIS_Point  ;
    bptg          : TGIS_Point  ;
    cex           : TGIS_Extent ;
    dx            : Double      ;
    dy            : Double      ;
    found         : Boolean     ;
    sx, sy        : Double      ;
    aa, rss       : Boolean     ;
    pt, bpt       : TPoint      ;
    bytes_wanted  : Integer     ;
    line          : TBytes      ;
    corpix        : TGIS_Pixels ;
    mr, mg, mb    : Byte        ;
    pixparams     : TGIS_ParamsSectionPixel ;
    ccolor : TGIS_Color {$IFDEF GIS_NORECORDS} = new TGIS_Color {$ENDIF};
  begin
    Result := False ;

    uptg := Unproject(_ptg) ;

    if not GisIsPointInsideExtent( uptg, Extent ) then
      exit ;

    zoom := FStore.PixelSize
            /
            calculatePixelsize( _pixelsize ) ; ;

    found := False ;
    if ( FStore.CurrentCell.Level <> FStore.CalculateLevel( zoom ) )
       or
       ( not GisIsPointInsideExtent( uptg, FStore.CurrentCell.Extent ) )
    then begin
      dx := ( Extent.XMax - Extent.XMin ) * 1e-7 ;
      dy := ( Extent.YMax - Extent.YMin ) * 1e-7 ;
      cex := GisExtent( uptg.X - dx, uptg.Y - dy, uptg.X + dx, uptg.Y + dy);
      FStore.SetScope( GisCommonExtent( cex, Extent ), zoom ) ;
      FStore.MoveFirst ;
      while not FStore.Eof do begin
        if GisIsPointInsideExtent( uptg, FStore.CurrentCell.Extent ) then begin
          prepareCell ;
          found := True ;
          break
        end ;
        FStore.MoveNext ;
      end ;
    end
    else
      found := True ;

    if found then begin
      cex := FStore.CurrentCell.Extent ;
      ptg.X :=  uptg.X - cex.XMin ;
      ptg.Y :=  uptg.Y - cex.YMax ;
      bptg.X := uptg.X - Extent.XMin ;
      bptg.Y := uptg.Y - Extent.YMax ;

      sx := ( cex.XMax - cex.XMin ) / baseCellWidth ;
      sy := ( cex.YMin - cex.YMax ) / baseCellHeight ;

      bpt.X := FloorS((bptg.X + GIS_DOUBLE_RESOLUTION)/ sx) ;
      bpt.Y := FloorS((bptg.Y + GIS_DOUBLE_RESOLUTION)/ sy) ;
      if bpt.Y <  0              then exit ;
      if bpt.Y >= FBitHeight then exit ;
      if bpt.X <  0              then exit ;
      if bpt.X >= FBitWidth  then exit ;

      pt.X := FloorS((ptg.X + GIS_DOUBLE_RESOLUTION)/ sx) ;
      pt.Y := FloorS((ptg.Y + GIS_DOUBLE_RESOLUTION)/ sy) ;
      if pt.Y <  0              then exit ;
      if pt.Y >= baseCellHeight then exit ;
      if pt.X <  0              then exit ;
      if pt.X >= baseCellWidth  then exit ;

      if FAntialias then begin
        aa := True ;
        FAntialias := False ;
      end
      else
        aa := False ;
      setFileScale( baseCellWidth, baseCellWidth) ;
      bytes_wanted := 3 ;
      SetLength( line, bytes_wanted +3 ) ;

      try
        if IsGridImage or IsNativeGridImage then begin
          SetLength(_nativesVals, 1) ;
          if not getNativeValue(pt, _nativesVals) then begin
            SetLength(_nativesVals, 0) ;
            exit ;
          end ;
        end
        else begin
          SetLength(_nativesVals, FBandsCount) ;
          getLine( line, 0, pt.Y, 3*pt.X, bytes_wanted ) ;

          _nativesVals[0] := line[2];
          _nativesVals[1] := line[1];
          _nativesVals[2] := line[0];
        end ;

        SetLength(corpix, 1 ) ;

        corpix[0] := Integer($FF000000) or Integer(line[2]) shl 16
                                        or Integer(line[1]) shl 8
                                        or Integer(line[0]) ;

        pixparams := TGIS_ParamsSectionPixel(Params) ;
        if not IsGridImage then begin
          if pixparams.Pixel.GrayScale  then
            ARGB2Gray(corpix, 1)
          else
          if makeSomeCorrection then
            fullCorrection( corpix, 1, 1 ) ;
        end
        else begin
          rss := isShadow ;
          isShadow := False ;
          ccolor := GetColorRamp(_nativesVals[0]) ;
          isShadow := rss ;
          corpix[0] := Integer(ccolor.ARGB) ;
        end ;



        mr := Byte(Integer(corpix[0] and $00FF0000) shr 16) ;
        mg := Byte(Integer(corpix[0] and $0000FF00) shr 08) ;
        mb := Byte((corpix[0] and $000000FF)) ;

        _rgbMapped := TGIS_Color.FromRGB( mr ,        //red
                                          mg ,        //green
                                          mb ) ;      //blue
        corpix := nil ;

        if makeTransparent then begin
          if ( redTransp  [ mr ]  and
               greenTransp[ mg ]  and
               blueTransp [ mb ] ) <> 0 then
            _transparency := True
          else
            _transparency := False ;
        end else
          _transparency := False ;

      finally
        SetLength(line, 0) ;
      end ;

      if aa then
        FAntialias := True ;
      Result := True ;
    end ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.Dormant           ;
  var
    i : Integer ;
  begin
    if assigned(scaledBmpArr) then begin
      for i := low(scaledBmpArr) to high(scaledBmpArr) do begin
        if assigned(scaledBmpArr[i]) then
          SetLength(scaledBmpArr[i], 0) ;
      end;
      SetLength(scaledBmpArr, 0) ;
      SetLength(scaledWidthArr, 0) ;
      SetLength(scaledHeightArr, 0) ;
    end;
  end;


  function  TGIS_LayerPixelStoreAbstract.getGridData(
    const _extent : TGIS_Extent ;
    const _grid   : TGIS_GridArray
  ) : Boolean ;
  var
    fullext : TGIS_Extent ;
    r       : Boolean     ;
    zm      : Double      ;
    ex      : TGIS_Extent ;
    rp_zoom : Double ;
  begin
    if FMinZ > FMaxZ then begin
      if not IsNativeGridImage then begin
        extZoom := 1 ;
        FMinZ := 0 ;
        FMaxZ := 255 ;
      end;
    end;

    Result := False ;
    fullext := Extent ;

    ex := _extent ;
    zm := length( _grid[0] ) / ( ex.XMax - ex.XMin ) ;

    if assigned( Viewer ) then
      rp_zoom := Viewer.Ref.PPI / 96
    else
      rp_zoom := 1 ;

    zm := zm / rp_zoom ;

    FStore.SetScope( ex, zm ) ;
    FStore.MoveFirst ;
    while not FStore.Eof do begin
      prepareCell ;
      FExtent := FStore.CurrentCell.Extent ;
      baseCellWidth  := 512 ;
      baseCellHeight := 512 ;
      r := inherited getGridData(ex, _grid ) ;
      Result := Result or r ;
      FStore.MoveNext ;
    end ;
    FExtent := fullext ;
  end ;


  function TGIS_LayerPixelStoreAbstract.getBitmapData(
    const _extent : TGIS_Extent ;
    const _bitmap : TGIS_Pixels ;
    const _width  : Integer ;
    const _height : Integer
  ) : Boolean ;
  var
    ymin,
    ymina  : Double ;
    ext    : TGIS_Extent ;
    cext   : TGIS_Extent ;
    wlzoom : Double ;
    rp_zoom : Double ;
    scsize, scrows, sccols : Integer ;
    posx, posy, i : Integer ;
    dw, dh : Double ;
    s_base_cell_width  : Integer ;
    s_base_cell_height : Integer ;
    s_cell_width  : Integer ;
    s_cell_height : Integer ;
    s_extent : TGIS_Extent ;
    s_proj_extent : TGIS_Extent ;
    src_grid   : TGIS_GridArray ;
  procedure reset_grid_col ;
    var
      k : Integer ;
    begin
      prevShadowValue := FNoDataValue ;
      prevShadowDelta := FNoDataValue ;
      for k := 0 to baseCellHeight -1 do begin
        gridCol[k][0] := NoDataValue ;
        gridCol[k][1] := NoDataValue ;
      end ;
    end ;

  begin

    if FIsNativeGridImage then begin //grid as ARGB
      src_grid := InitializeGrid( _height, _width ) ;
      setNoDataTable( src_grid ) ;
      Result := getGridData( _extent , src_grid ) ;
      if Result then
        gridToARGBTable( _extent, _bitmap, src_grid ,_width, _height ) ;
      src_grid := nil ;
      exit ;
    end;

    Result := True ;

    wlzoom := _width/( _extent.XMax - _extent.XMin ) ;

    if assigned( Viewer ) then
      rp_zoom := Viewer.Ref.PPI / 96
    else
      rp_zoom := 1 ;
    wlzoom := wlzoom / rp_zoom ;

    ext := _extent ;

    s_base_cell_width  := baseCellWidth ;
    s_base_cell_height := baseCellHeight ;
    s_cell_width       := FCellWidth ;
    s_cell_height      := FCellHeight ;
    s_extent           := FExtent ;
    s_proj_extent      := FProjectedExtent ;

    baseCellWidth  := 512 ;
    baseCellHeight := 512 ;
    FCellWidth     := 512 ;
    FCellHeight    := 512 ;

    ext := GisCommonExtent( ext, FExtent ) ;
    if ( ext.XMax <= ext.XMin ) or
       ( ext.YMax <= ext.YMin ) then
    begin
      exit ;
    end ;

    FStore.SetScope( ext, wlzoom ) ;
    FStore.MoveFirst ;
    if Antialias then begin
      dw := Store.CurrentCell.Extent.XMax -FStore.CurrentCell.Extent.XMin ;
      dh := Store.CurrentCell.Extent.YMax -FStore.CurrentCell.Extent.YMin ;
      scrows := RoundS((s_extent.YMax - s_extent.YMin +dh/2)/dh ) ;
      sccols := RoundS((s_extent.XMax - s_extent.XMin +dw/2)/dw ) ;
      scsize := scrows * sccols ;
      if (scsize <> scaledScopeSize) or (not assigned(scaledBmpArr)) then begin
        Dormant ;
        {$IFDEF JAVA}
          scaledBmpArr := new TGIS_Pixels[scsize] ;
        {$ELSE}
          SetLength(scaledBmpArr, scsize) ;
        {$ENDIF}

        for i := 0 to scsize -1 do begin
          scaledBmpArr[i] := nil ;
        end;
        scaledScopeSize := scsize ;
        {$IFDEF JAVA}
          scaledWidthArr := new Integer[scsize] ;
          scaledHeightArr := new Integer[scsize] ;
        {$ELSE}
          SetLength(scaledWidthArr, scsize) ;
          SetLength(scaledHeightArr, scsize) ;
        {$ENDIF}
      end ;
    end
    else begin
      dw := 1 ;
      dh := 1 ;
      sccols := 0 ;
    end ;

    ymin := 0 ;
    if IsGridImage or IsNativeGridImage then begin
      reset_grid_col ;
      ymin := FStore.CurrentCell.Extent.YMin ;
    end ;
    while not FStore.Eof do begin
      if HourglassShake then break ;
      prepareCell ;
      cext :=   FStore.CurrentCell.Extent ;
      FExtent := cext ;
      FProjectedExtent := cext ;

      if Antialias then begin
        posy := RoundS((s_extent.YMax - cext.YMax)/dh)  ;
        posx := RoundS((cext.XMin -s_extent.XMin)/dw) ;
        if posx < 0 then
          posx := 0 ;
        if posy < 0 then
          posy := 0 ;
        scIdx := sccols*posy + posx ;
        if scIdx < 0 then
          scIdx := 0 ;
        if scIdx >= scaledScopeSize then
          scIdx := scaledScopeSize -1;

        scaledPixObj := scaledBmpArr[scIdx] ;

        if assigned(scaledPixObj) then begin
          scaledWidth := scaledWidthArr[scIdx] ;
          scaledHeight := scaledHeightArr[scIdx] ;
        end
        else begin
          scaledWidth := 0 ;
          scaledHeight := 0 ;
        end ;

        Result := inherited getBitmapData(_extent, _bitmap, _width, _height ) ;
        scaledBmpArr[scIdx] := scaledPixObj ;
        scaledWidthArr[scIdx] := scaledWidth ;
        scaledHeightArr[scIdx] := scaledHeight ;

      end
      else
        Result := inherited getBitmapData(_extent, _bitmap, _width, _height ) ;
      FStore.MoveNext ;
      if IsGridImage or IsNativeGridImage then begin
        ymina := FStore.CurrentCell.Extent.YMin ;
        if ymin <> ymina then
          reset_grid_col ;
        ymin := ymina ;
      end ;
    end ;

    baseCellWidth := s_base_cell_width ;
    baseCellHeight := s_base_cell_height ;
    FCellWidth  := s_cell_width ;
    FCellHeight := s_cell_height ;
    FExtent := s_extent ;
    FProjectedExtent := s_proj_extent ;

    if (bandsMap[0] <> 0 ) or ( bandsMap[1] <> 1) or (bandsMap[2] <> 2) or
       (bandsMap[3] <> 3)
    then
      finalARGBMap(_bitmap, _width, _height) ;

  end ;


  function TGIS_LayerPixelStoreAbstract.GetAvailableLayers : TGIS_LayerInfoList ;
  begin
    assert( assigned( FStore ) ) ;

    FStore.Path := Path ;
    FStore.Name := Name ;

    Result := FStore.GetAvailableLayers ;
  end ;

  {$IFDEF WINFORMS}
    function TGIS_LayerPixelStoreAbstract.FireEventPassword
                                    ( const _sender : System.Object ;
                                      const _e      : TGIS_TemplateProducerEventArgs
                                    ) : String ;
    begin
      if Assigned( FOnPassword ) then
        Result := FOnPassword( _sender, _e )
      else
        Result := '' ;
    end;
  {$ENDIF}

  function TGIS_LayerPixelStoreAbstract.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FilePixelStoreAbstract ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    if not assigned( FStore ) then exit ;
    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        f := TGIS_FilePixelStoreAbstract(&Class.forName(FStore.Class.Name).getConstructor().newInstance());
      {$ELSE}
        f := TGIS_FilePixelStoreAbstract( Activator.CreateInstance( FStore.GetType() ) ) ;
      {$ENDIF}
    {$ELSE}
      f := TGIS_FilePixelStoreAbstractClass(FStore.ClassType).Create ;
    {$ENDIF}
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.Build(
    const _path      : String         ;
    const _grid      : Boolean        ;
    const _cs        : TGIS_CSCoordinateSystem  ;
    const _ext       : TGIS_Extent    ;
    const _width     : Integer        ;
    const _height    : Integer        ;
    const _subformat : TGIS_LayerPixelSubFormat
  ) ;
  var
    f : TGIS_FilePixelStoreAbstract ;
    width  : Integer ;
    height : Integer ;
  begin
    if not IsStringEmpty( _path ) then
        Path := _path ;

    FIsGridImage := _grid ;
    FIsNativeGridImage := _grid ;
    if (_subformat.PixelFormat = TGIS_PixelFormat.ARGB) and (not _grid)  then
     NoDataColor := TGIS_Color.Maroon ;


    if FIsGridImage then
      oGrid := InitializeGrid( 512, 512 )
    else begin
      if (not assigned(oBitmap)) then
        SetLength(oBitmap, 512*512 ) ;
    end;

    width := _width ;
    height := _height ;
    if width = 0 then begin
      if Abs(_ext.YMax - _ext.YMin) <> 0 then
        width := RoundS(Abs(_ext.XMax - _ext.XMin) * height / Abs(_ext.YMax - _ext.YMin)) ;
    end;
    if height = 0 then begin
      if Abs(_ext.XMax - _ext.XMin) <> 0 then
        height := RoundS(Abs(_ext.YMax - _ext.YMin) * width / Abs(_ext.XMax - _ext.XMin)) ;
    end;

    FCS     := _cs     ;
    FExtent := _TGIS_Extent(_ext)    ;
    FProjectedExtent := _TGIS_Extent(_ext)    ;
    FBitWidth  := width  ;
    FBitHeight := height ;

    FCellWidth  := width  ;
    FCellHeight := height ;

    baseCellWidth  := width  ;
    baseCellHeight := height ;

    FPixelSize := GisPoint(
                    ( FExtent.XMax - FExtent.XMin ) / FBitWidth,
                    ( FExtent.YMax - FExtent.YMin ) / FBitHeight
                  ) ;
    if width > height then begin
      FPixelSize.Y := FPixelSize.X ;
      FExtent.YMin := FExtent.YMax - height*FPixelSize.Y ;
    end
    else begin
      FPixelSize.X := FPixelSize.Y ;
      FExtent.XMax := FExtent.XMin + width*FPixelSize.X ;
    end;
    FProjectedExtent := FExtent ;

    FSubFormat := _TGIS_LayerPixelSubFormat(_subformat) ;
    assert( assigned( FStore ) ) ;

    {$IFDEF OXYGENE}
      {$IFDEF JAVA}
        var prm := new &Class[7] ;
        prm[0] := typeOf( String ) ;
        prm[1] := typeOf( TGIS_Extent ) ;
        prm[2] := typeOf( Integer ) ;
        prm[3] := typeOf( Integer ) ;
        prm[4] := typeOf( TGIS_LayerPixelSubFormat ) ;
        prm[5] := typeOf( Integer ) ;
        prm[6] := typeOf( TGIS_CSCoordinateSystem ) ;
        f := TGIS_FilePixelStoreAbstract( FStore.Class.getConstructor(prm).newInstance(
              [Path,
              FProjectedExtent ,
              CellWidth,
              CellHeight,
              FSubFormat,
              96,
              CS ]
              ) );
      {$ELSE}
        f := TGIS_FilePixelStoreAbstract( Activator.CreateInstance(FStore.GetType(),
              [Path,
              FProjectedExtent ,
              CellWidth,
              CellHeight,
              FSubFormat,
              96,
              CS ]
              ) ) ;
      {$ENDIF}
    {$ELSE}
      f := TGIS_FilePixelStoreAbstractClass(FStore.ClassType).Create(
            Path,
            FProjectedExtent ,
            CellWidth,
            CellHeight,
            FSubFormat,
            96,
            CS
          ) ;
    {$ENDIF}
    try
      FStore.Path := Path ;
      FStore.Name := Name ;

      if assigned( Viewer ) and assigned( Viewer.Ref.ProjectFile ) then
        FStore.RelativePath := Viewer.Ref.ProjectName ;

      FStore.Open ;
      FStore.UpdateSubFormat( FSubFormat ) ;
    finally
      FreeObject(f) ;
    end ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.SaveData ;
  begin
    if IsStringEmpty( Path ) then exit ;

//    FStore.FinalizeWrite ;
  end;

  function TGIS_LayerPixelStoreAbstract.LockPixels(
    const _rct        : TRect ;
    const _ext        : TGIS_Extent ;
    const _tilelevel  : Integer ;
    const _tilerow    : Integer ;
    const _tilecolumn : Integer ;
    const _writable   : Boolean
  ) : TGIS_LayerPixelLock ;
  var
    rct : TRect ;
  begin
    rct := _rct ;

    if rct.Width > FStore.CellSize then
      rct.Width := FStore.CellSize ;
    if rct.Height > FStore.CellSize then
      rct.Height := FStore.CellSize ;

    Result := inherited LockPixels( rct, _ext, _tilelevel,
                              _tilerow, _tilecolumn, _writable ) ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.UnlockPixels(
    {$IFDEF GIS_PDK}
      const
    {$ELSE}
       var
    {$ENDIF}
          _lock   : TGIS_LayerPixelLock
  ) ;
  begin
    try
      if _lock.Writable then begin
        if assigned( _lock.Bitmap ) then
          FStore.Write( _lock.Extent,
                        _lock.TileInfo.Level,
                        _lock.Bitmap,
                        _lock.Bounds.Width +1,
                        _lock.Bounds.Height +1
                       )
        else
          FStore.WriteGrid( _lock.Extent,
                            _lock.TileInfo.Column,
                            _lock.TileInfo.Row,
                            _lock.TileInfo.Level,
                            _lock.Grid
                           ) ;
      end ;
    finally
      {$IFDEF GIS_PDK}
        FreeObjectNotNil( _lock ) ;
      {$ELSE}
        FreeObject( _lock ) ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.InitializeWrite ;
  begin
    FStore.InitializeWrite ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.FinalizeWrite ;
  begin
    FStore.FinalizeWrite ;
  end ;

  procedure TGIS_LayerPixelStoreAbstract.ImportLayer(
    const _layer     : TGIS_LayerPixel  ;
    const _extent    : TGIS_Extent ;
    const _cs        : TGIS_CSCoordinateSystem ;
    const _width     : Cardinal ;
    const _height    : Cardinal ;
    const _subformat : TGIS_LayerPixelSubFormat
  ) ;
  var
    xtiles, ytiles : Integer ;
    cell, row      : Integer ;
    level          : Integer ;
    max_level      : Integer ;
    ext            : TGIS_Extent ;
    tile_rect      : TRect ;
    ext_rect       : TRect ;
    ddx, ddy       : Double ;
    dx, dy         : Double ;
    wextent        : TGIS_Extent ;
    ext_width      : Double ;
    ext_height     : Double ;
    abrt           : Boolean ;
    cnt            : Integer ;
    pos            : Integer ;
    rw, rh         : Integer ;
    psx, psy       : Double ;


    {$IFDEF GIS_DUMPIMAGE}
    procedure writeWorldFile(
      const _ext   : TGIS_Extent ;
      const _w, _h : Integer ;
      const _path  : String
    ) ;
    var
      scale_x : Double   ;
      scale_y : Double   ;
      olist   : TGIS_StringList ;
    begin
      scale_x := ( _ext.XMax - _ext.XMin ) / _w  ;
      scale_y := ( _ext.YMax - _ext.YMin ) / _h ;

      olist := TGIS_StringList.Create ;
      try
        olist.Text := Format( '%s' + #13#10 + '%d' + #13#10 + '%d' + #13#10 +
                              '%s' + #13#10 + '%s' + #13#10 + '%s' + #13#10 ,
                              [ DotFloatToStr(   scale_x ), 0, 0,
                                DotFloatToStr( - scale_y ),
                                DotFloatToStr( _ext.XMin  ),
                                DotFloatToStr( _ext.YMax  )
                              ]
                            ) ;
        olist.SaveToFile( _path ) ;
      finally
        FreeObject( olist ) ;
      end ;
    end ;
    {$ENDIF}

    procedure draw_map(
      const _rect : TRect
    ) ;
    var
      w, h      : Integer ;
      dw, adw   : Integer ;
      l, t      : Integer ;
      r, bw     : Integer ;
      plock     : TGIS_LayerPixelLock ;
      pdata     : TGIS_Pixels ;
      pos       : Integer ;
      {$IFDEF GIS_DUMPIMAGE}
      dump_name : String ;
      {$ENDIF}
    begin
      if ( _rect.Width = 0 ) or ( _rect.Height = 0 ) then exit ;
      w := 512 ;
      h := 512 ;

      plock := LockPixels( _rect, ext, level, row, cell, True ) ;
      try
        if not IsGridImage then begin

          SetLength( pdata, w*h ) ;
          _layer.GetBitmap( ext, pdata, w, h ) ;

          {$IFDEF GIS_DUMPIMAGE}
            dump_name := Format( 'C:\TMP\tiles\%d-%d-%d', [ level, row, cell ] ) ;
            bmp.SaveToFile( dump_name + '.bmp' );
            writeWorldFile( ext, w, h, dump_name + '.bpw' ) ;
          {$ENDIF}

          bw := plock.BitmapPos(0, 1) ;
          t :=  plock.Bounds.Top ;
          l :=  plock.Bounds.Left ;

          if (w = bw) and (h = plock.Bounds.Height +1) then begin
            pos := plock.BitmapPos(l, t) ;
            GisCopyPixels( pdata, 0, plock.Bitmap, pos, h*w ) ;
          end
          else begin
            dw := plock.Bounds.Width +1;
            adw := dw ;
            for r := 0 to  plock.Bounds.Height do begin
              pos := plock.BitmapPos(l, t +r) ;
              GisCopyPixels( pdata, r*w, plock.Bitmap, pos, adw) ;
            end;
          end
        end
        else begin
          setNoDataTable(plock.Grid) ;
          if _layer.GetGrid( ext, plock.Grid ) then ;
        end ;
      finally
        UnlockPixels( plock ) ;
      end ;

    end ;

  begin
    if GisIsNoWorld( _extent ) then exit ;
    if IsReadOnly then exit ;
    if not assigned( _layer ) then exit ;

    ext := _TGIS_Extent(_extent) ;
    ext_width  := _layer.Extent.XMax - _layer.Extent.XMin ;
    ext_height := _layer.Extent.YMax - _layer.Extent.YMin ;
    if True then

    psx := ext_width/_layer.BitWidth ;
    psy := ext_height/_layer.BitHeight ;

    rh := RoundS((ext.YMax - ext.YMin) / ext_height * _layer.BitHeight);
    rw := RoundS( (ext.XMax - ext.XMin) / ext_width * _layer.BitWidth );

    MinHeight := _layer.MinHeight ;
    MaxHeight := _layer.MaxHeight ;

    wextent := GisExtent( ext.XMin, ext.YMin, ext.XMax,
                          ext.YMin + ((ext.XMax - ext.XMin) / rw) * rh
                         ) ;

    if _subformat.Subformat = TGIS_PixelSubFormat.GRID then
      Build(Path, True, _layer.CS, wextent, rw, rh, _subformat)
    else
      Build(Path, False, _layer.CS, wextent, rw, rh, _subformat) ;

    abrt := False ;
    tile_rect := Rect( 0, 0, _width, _height ) ;

    if ( tile_rect.Width = 0 ) and ( tile_rect.Height = 0 ) then
      tile_rect.Height := 1024 ;

    if tile_rect.Width = 0  then
      tile_rect.Width  := RoundS( ( wextent.XMax - wextent.XMin ) /
                                  ( wextent.YMax - wextent.YMin )
                                  * tile_rect.Height
                                ) ;
    if tile_rect.Height = 0 then
      tile_rect.Height := RoundS( ( wextent.YMax - wextent.YMin ) /
                                  ( wextent.XMax - wextent.XMin )
                                  * tile_rect.Width
                                 ) ;
    if assigned( FOnBusy ) then
      {$IFDEF OXYGENE}
        FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
      {$ELSE}
        FOnBusy( self, -1, -1, abrt ) ;
      {$ENDIF}
    try
      xtiles := tile_rect.Width div 512 ;
      if ( tile_rect.Width mod 512 ) > 0 then
        xtiles := xtiles + 1 ;

      ytiles := tile_rect.Height div 512 ;
      if ( tile_rect.Height mod 512 ) > 0 then
        ytiles := ytiles + 1 ;

      max_level := TruncS( Ln(Max(xtiles, ytiles)) / Ln(2) ) + 1 ;

      cnt := 0 ;
      pos := 0 ;
      for level := 0 to max_level-1 do begin
        cnt := cnt + (TruncS((tile_rect.Width/512)/Power(2,level))+1) *
                     (TruncS((tile_rect.Height/512)/Power(2,level)+1)) ;
      end ;

      try
        // find scale for whole extent
        dx := (wextent.XMax-wextent.XMin)/(tile_rect.Width / 512)*Power(2,max_level-1) ;
        dy := (wextent.YMax-wextent.YMin)/(tile_rect.Height / 512)*Power(2,max_level-1) ;
        ddx := dx ;
        ddy := dy ;

        InitializeWrite ;
        for level := max_level downto 0 do begin
          dx := (wextent.XMax-wextent.XMin)/(tile_rect.Width/512)*Power(2,level) ;
          dy := (wextent.YMax-wextent.YMin)/(tile_rect.Height/512)*Power(2,level) ;
          ddx := dx ;
          ddy := dy ;

          dy := wextent.YMax ;
          row := 0 ;
          while dy > wextent.YMin do begin
            dx := wextent.XMin ;
            cell := 0 ;
            while dx < wextent.XMax do begin
              // set range
              ext := GisExtent( dx, dy - ddy, dx + ddx, dy  ) ;

              ext_rect := Rect( cell, row, cell + 512, row + 512 ) ;

              draw_map( ext_rect ) ;

              inc(cell, 512) ;
              dx := dx + ddx ;

              if assigned( FOnBusy ) then
                {$IFDEF OXYGENE}
                  FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                {$ELSE}
                  FOnBusy( self, pos, cnt, abrt ) ;
                {$ENDIF}

              if abrt then break ;
              inc( pos ) ;
            end ;
            dy := dy - ddy ;
            inc(row, 512 ) ;
            if abrt then break ;
          end ;
        end ;
      finally
        FinalizeWrite ;
      end ;
    finally
      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, abrt ) ;
        {$ENDIF}
    end ;
  end ;

{==================================== END =====================================}
end.

