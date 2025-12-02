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
  Encapsulation of a JPEG Layer.
}

{$IFDEF DCC}
  unit GisLayerJPG ;
  {$HPPEMIT '#pragma link "GisLayerJPG"'}
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
    System.Drawing,
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.Classes,
    System.SysUtils,
    System.Math,
    {$IFDEF MSWINDOWS}
//      Winapi.Windows,
    {$ENDIF}
    GisTypes,
    GisTypesUI,
    GisLayerPixel,
    GisCSSystems,
    GisStreams,
    GisFileJPEG ;
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

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerJPG = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of a JPEG layer.
  /// </summary>
  TGIS_LayerJPG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private  // various private variables

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmp       : TGIS_Pixels ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmpWidth  : Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmpHeight : Integer ;

      /// <summary>
      ///   JPEG decoder object.
      /// </summary>
      jpegDecoder     : TGIS_JPEGDecoder ;

    // various protected procedures
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <inheritdoc/>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure setUp             ; override;

      /// <inheritdoc/>
      function bandsMappingChanging  : Boolean  ; override;


      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
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

      /// <inheritdoc/>
      function  getNativeValue    ( const _pt : TPoint  ;
                                    const _ar : TGIS_DoubleArray
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine     ( const _buffer   : TGIS_SingleArray ;
                                    const _linenr   : Integer          ;
                                    const _startIdx : Integer          ;
                                    const _count    : Integer
                                  ) : Integer ; override;

      /// <summary>
      ///   Reading an image line.
      /// </summary>
      /// <param name="_buffer">
      ///   pointer
      /// </param>
      /// <param name="_offset">
      ///   buffer offset
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
      ///    read bytes count
      /// </returns>
      function  getScaledLine   ( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer;

      /// <inheritdoc/>
      function  setFileScale      ( const _dwidth : Double ;
                                    const _swidth : Double
                                  ) : Double ; override;

      /// <inheritdoc/>
      function  setFileScaleXY    ( const _dwidth  : Double ;
                                    const _swidth  : Double ;
                                    const _dheight : Double ;
                                    const _sheight : Double
                                  ) : Double ; override;

      procedure fset_Antialias    ( const _value  : Boolean
                                  ) ; override;

      function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

    public // various public routines

      /// <inheritdoc/>
      procedure Alive             ; override;

      /// <inheritdoc/>
      function  DormantGain       : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant           ; override;

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
    protected

      /// <inheritdoc/>
      procedure doDestroy         ; override;

    public // API
      // constructors

      /// <inheritdoc/>
      constructor Create          ; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisResource,
    GisRegistredLayers;
{$ENDIF}

const

    // Extension of world file (extension file for pixel layer).
    WORLD_FILE_EXT_JPG = '.jgw' ;

    // Extension of world file (extension file for pixel layer).
    // Alternative naming for .JPG files.
    WORLD_FILE_EXT2_JPG = '.jfw' ;

    // Extension of world file (extension file for pixel layer).
    // Alternative naming for .JPEG files
    WORLD_FILE_EXT3_JPG = '.jpgw' ;

    // Extension of world file (extension file for pixel layer).
    // Alternative naming for .PSI files.
    WORLD_FILE_EXT4_JPG = '.piw' ;

//==============================================================================
// TGIS_LayerJPG
//==============================================================================

  constructor TGIS_LayerJPG.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable ] ;

    FBitmapFormat      := TGIS_BitmapFormat.ARGB ;
    FBitmapLinesOrder  := TGIS_BitmapLinesOrder.Down ;

    jpegDecoder := TGIS_JPEGDecoder.Create ;
  end ;

  procedure TGIS_LayerJPG.doDestroy ;
  begin
    FreeObject( jpegDecoder ) ;
    SetLength( scaledBmp, 0   ) ;

   inherited ;
  end ;

  procedure TGIS_LayerJPG.setUp ;
  var
    tmp : Word ;
  begin
    if isBuilt then
      exit ;
    tmp := 0;
    try
      if assigned( Stream ) then begin
        Stream.Position := 0 ;
        jpegDecoder.LoadFromStream( Stream ) ;
      end
      else begin
        jpegDecoder.LoadFromFile( Path ) ;
      end ;

      tmp := jpegDecoder.Initialize(FDormantMode) ;

      if tmp <> 0 then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, tmp) ;

      realBitCount  := 24;
      FBitHeight    := jpegDecoder.Height ;
      FBitWidth     := jpegDecoder.Width  ;
      realLineWidth := ((3* (FBitWidth +1)) div 4)*4 ;
      FBandsCount := 3 ;

      // read a word file
      setWorldFile( WORLD_FILE_EXT_JPG ) ;

      // read a word file (alternative)
      if GisIsNoWorld( Extent ) then
        setWorldFile( WORLD_FILE_EXT2_JPG ) ;

      // read a word file (alternative)
      if GisIsNoWorld( Extent ) then
        setWorldFile( WORLD_FILE_EXT3_JPG ) ;

      // read a word file (alternative)
      if GisIsNoWorld( Extent ) then
        setWorldFile( WORLD_FILE_EXT4_JPG ) ;

      inherited setUp ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;
      FFileInfo := Format( 'JPEG File Interchange Format (JPG)' +#13#10 +
                           '%d x %d; %d bit (%s)',
                           [ FBitWidth, FBitHeight, jpegDecoder.Precision*3, jpegDecoder.SubFormatInfo ]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, tmp ) ;
    end ;
  end;

  function TGIS_LayerJPG.bandsMappingChanging : Boolean ;
  begin
    Result := False ;
    if bandsMap[0] <> 0 then
      Result := True ;
    if bandsMap[1] <> 1 then
      Result := True ;
    if bandsMap[2] <> 2 then
      Result := True ;
    if (bandsMap[3] <> 3) and (bandsMap[3] <> -1) then
      Result := True ;
  end;


  function  TGIS_LayerJPG.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer;
  var
    linenr  : Integer ;
    bitsidx : Integer  ;
    st      : Integer ;
    pixels  : Integer ;
    k       : Integer ;
    lbuf    : TBytes ;

    procedure make_scaled_bmp ;
    var
      bitmapObj : TGIS_Pixels ;
      sw, sh    : Integer ;
      w, h      : Integer ;
      decscale  : Double ;
    begin
      w := scaledWidth ;
      h := scaledHeight ;

      decscale := jpegDecoder.Scale ;
      sw := RoundS(baseCellWidth /decscale) ;
      sh := RoundS(baseCellHeight/decscale) ;
      SetLength(bitmapObj, sw*sh) ;
      fset_Antialias(False) ;
      self.getBitmapData(FExtent, bitmapObj, sw, sh) ;
      fset_Antialias(True) ;
      SetLength(scaledBmp, w * h) ;
      scaledWidth := w ;
      scaledHeight := h ;
      scaledBmpWidth := w ;
      scaledBmpHeight := h ;
      TGIS_Bitmap.ScaleBitmap( bitmapObj, sw, sh, scaledBmp, w, h,
                               FAntialiasFilter
                             ) ;
    end ;
  begin
    linenr := _linenr ;
    pixels  := _pixCount ;
    Result := _pixCount ;
    st  :=  _pixStart ;

    if not Antialias then begin
      if isBuilt then begin
        if _linenr < FBitHeight then begin
          Result := inherited  getLinePixels(_buffer, _offset, _linenr,
                                             _pixStart, _pixCount) ;
        end ;
      end
      else begin
        SetLength(lbuf, 3*pixels) ;
        Result := jpegDecoder.DecodeLine(
                    lbuf,
                    0,
                    linenr*jpegDecoder.Scale,
                    st,
                    pixels
                  ) ;
        for k := 0 to pixels -1 do
          _buffer[k +_offset] := Integer($FF000000)
                                +(lbuf[3*k +2] shl 16)
                                +(lbuf[3*k +1] shl 08)
                                +(lbuf[3*k   ] ) ;

      end;
    end
    else begin
      if (scaledWidth = 0) or (scaledHeight = 0) then begin
        if extZoom = 1 then begin
          setFileScale(1, 1) ;
        end
        else begin
          Result := 0 ;
          exit ;
        end ;
      end ;

      if not assigned(scaledBmp) then
        make_scaled_bmp
      else
      if (scaledBmpWidth <> scaledWidth) or (scaledBmpHeight <> scaledHeight) then
      begin
        SetLength( scaledBmp, 0   ) ;
        make_scaled_bmp ;
      end ;

      if linenr >= scaledHeight then
        linenr := scaledHeight -1 ;
      st := _pixStart ;
      pixels := _pixCount ;
      if (st +pixels) >= scaledWidth then begin
        if st > 0 then begin
          st := scaledWidth -pixels ;
          if st < 0 then begin
            st := 0 ;
            pixels := scaledWidth ;
          end ;
        end
        else
          pixels := scaledWidth ;
      end ;

      bitsidx := scaledBmpWidth*linenr +st;
      for k := 0 to pixels -1 do begin
        _buffer[_offset +k] := scaledBmp[bitsidx + k]  ;
      end ;

    end;
  end ;

  function TGIS_LayerJPG.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  begin
    Result := 0 ;

    if isBuilt then begin
      if _linenr < FBitHeight then begin
          Result := inherited  getLine(_buffer, _offset, _linenr,
                                       _start, _bytes) ;
      end ;
    end
    else
      Result := jpegDecoder.DecodeLine(
                  _buffer,
                  _offset,
                  _linenr*jpegDecoder.Scale,
                  _start div 3,
                  _bytes div 3
                ) ;
  end ;

  function  TGIS_LayerJPG.getNativeValue(
    const _pt : TPoint  ;
    const _ar : TGIS_DoubleArray
  ) : Boolean ;
  var
    cnt :Integer ;
    sbuf : TGIS_SingleArray ;
  begin
    cnt := length(_ar) ;
    SetLength(sbuf, cnt) ;
    cnt := getNativeLine(sbuf, _pt.Y, _pt.X, 1) ;
    if cnt > 0 then
      Result := True
    else
      Result := False ;
  end;

  function  TGIS_LayerJPG.getNativeLine(
    const _buffer   : TGIS_SingleArray ;
    const _linenr   : Integer          ;
    const _startIdx : Integer          ;
    const _count    : Integer
  ) : Integer ;
  var
    linenr  : Integer ;
    st      : Integer ;
    pixels  : Integer ;
    k       : Integer ;
    lbuf    : TBytes ;
    bidx    : Integer ;
    bytes   : Integer ;
  begin
    linenr := _linenr ;
    pixels  := _count ;
    Result := _count  ;
    st  :=  _startIdx ;
    if isBuilt then begin
      if _linenr < FBitHeight then begin
        Result := inherited  getNativeLine(_buffer, _linenr, _startIdx, _count) ;
      end ;
    end
    else begin
      if jpegDecoder.Precision <= 8 then
        bytes := 3*pixels
      else
        bytes := 6*pixels ;
      SetLength(lbuf, bytes) ;

      Result := jpegDecoder.DecodeLine(
                  lbuf,
                  0,
                  linenr*jpegDecoder.Scale,
                  st,
                  bytes
                ) ;
      if jpegDecoder.Precision <= 8 then begin
        if FGridBand <= 0 then begin
          for k := 0 to pixels -1 do begin
            _buffer[3*k +0] := lbuf[3*k +2] ;
            _buffer[3*k +1] := lbuf[3*k +1] ;
            _buffer[3*k +2] := lbuf[3*k +0] ;
          end;
        end
        else begin
          bidx := FBandsCount -FGridBand ;
          for k := 0 to pixels -1 do
            _buffer[ k ] :=Integer(lbuf[3*k +bidx +0]);
        end;
      end
      else begin
        if FGridBand <= 0 then begin
          for k := 0 to pixels -1 do begin
            _buffer[3*k +0] := (Integer(lbuf[6*k +5]) shl 8) +Integer(lbuf[6*k +4]);
            _buffer[3*k +1] := (Integer(lbuf[6*k +3]) shl 8) +Integer(lbuf[6*k +2]);
            _buffer[3*k +2] := (Integer(lbuf[6*k +1]) shl 8) +Integer(lbuf[6*k +0]);
          end;
        end
        else begin
          bidx := 2*(FBandsCount -FGridBand) ;
          for k := 0 to pixels -1 do
            _buffer[ k ] := (Integer(lbuf[6*k +bidx +1]) shl 8) +Integer(lbuf[6*k +bidx +0]);
        end;

      end;
    end;
  end;


  function TGIS_LayerJPG.getScaledLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i, ix, k  : Integer ;
    w3        : Integer ;
    linenr    : Integer ;
    bytes     : Integer ;
    start     : Integer ;
    pix       : Integer ;
    pbits_idx : Integer ;

    procedure make_scaled_bmp ;
    var
      bitmapObj : TGIS_Pixels ;
      sw, sh    : Integer ;
      w, h      : Integer ;
      decscale  : Double ;
    begin
      w := scaledWidth ;
      h := scaledHeight ;

      decscale := jpegDecoder.Scale ;
      sw := RoundS(baseCellWidth /decscale) ;
      sh := RoundS(baseCellHeight/decscale) ;
      SetLength(bitmapObj, sw*sh) ;
      fset_Antialias(False) ;
      self.getBitmapData(FExtent, bitmapObj, sw, sh) ;
      fset_Antialias(True) ;
      SetLength(scaledBmp, w * h) ;
      scaledWidth := w ;
      scaledHeight := h ;
      scaledBmpWidth := w ;
      scaledBmpHeight := h ;
      TGIS_Bitmap.ScaleBitmap( bitmapObj, sw, sh, scaledBmp, w, h,
                               FAntialiasFilter
                             ) ;
    end;

  begin
    linenr := _linenr ;
    bytes  := _bytes ;
    start  := _start ;

    if (scaledWidth = 0) or (scaledHeight = 0) then begin
      if extZoom = 1 then begin
        setFileScale(1, 1) ;
      end
      else begin
        Result := 0 ;
        exit ;
      end ;
    end ;

    if not assigned(scaledBmp) then
      make_scaled_bmp
    else
      if (scaledBmpWidth <> scaledWidth) or (scaledBmpHeight <> scaledHeight) then
      begin
        SetLength( scaledBmp, 0   ) ;
        make_scaled_bmp ;
      end;

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
    if bytes <= 0 then begin
      Result := 0 ;
      exit ;
    end;

    pbits_idx := scaledBmpWidth*linenr ;
    pbits_idx := pbits_idx +(start div 3) ;
    pix := bytes div 3 ;
    i := 0 ;
    ix := 0 ;

    for k := 1 to pix do begin
      _buffer[i] := Byte(scaledBmp[pbits_idx + ix] and $FF) ;
      inc( i ) ;
      _buffer[i] := Byte((scaledBmp[pbits_idx + ix] shr 8) and $FF) ;
      inc( i ) ;
      _buffer[i] := Byte((scaledBmp[pbits_idx + ix] shr 16) and $FF);
      inc( i ) ;
      inc( ix ) ;
    end ;
    Result := bytes ;
  end ;

  function  TGIS_LayerJPG.setFileScale(
    const _dwidth : Double ;
    const _swidth : Double
  ) : Double ;
  var
    nzoom   : Double ;
    dzoom   : Double ;
    inscale : Integer ;
    scale   : Double ;
    w, h    : Integer ;
  begin
    inscale := 1 ;
    if _swidth <> 0 then begin
      nzoom := _dwidth / _swidth ;

      if (nzoom > 0) and (nzoom < 1) then begin
        dzoom := 0.5 ;
        while nzoom < dzoom do begin
          inscale := inscale * 2 ;
          dzoom := dzoom / 2 ;
        end ;
      end ;
    end ;

    jpegDecoder.Scale := inscale ;
    Result := 1.0/jpegDecoder.Scale ;

    if Antialias then begin
      scale := _dwidth/_swidth ;
      if scale < 0.0125 then
        scale := 0.0125
      else
      if scale > 2 then
        scale := 2 ;
      w := RoundS( baseCellWidth  * scale  ) ;
      if w < 1 then
        w := 1;
      h := RoundS(( baseCellHeight ) * scale ) ;
      if h < 1 then
        h := 1 ;
      scaledWidth  := w ;
      scaledHeight := h ;
      Result := scale ;
    end ;
    extZoom := Result ;
  end ;

  function  TGIS_LayerJPG.setFileScaleXY(
    const _dwidth  : Double ;
    const _swidth  : Double ;
    const _dheight : Double ;
    const _sheight : Double
  ) : Double ;
  var
    nzoom   : Double ;
    dzoom   : Double ;
    inscale : Integer ;
    scale_x : Double ;
    scale_y : Double ;
    w, h    : Integer ;
  begin
    inscale := 1 ;
    if _swidth <> 0 then begin
      nzoom := _dwidth / _swidth ;

      if (nzoom > 0) and (nzoom < 1) then begin
        dzoom := 0.5 ;
        while nzoom < dzoom do begin
          inscale := inscale * 2 ;
          dzoom := dzoom / 2 ;
        end ;
      end ;
    end ;

    jpegDecoder.Scale := inscale ;
    scale_x := 1.0/jpegDecoder.Scale ;
    scale_y := scale_x ;

    if Antialias then begin
      scale_x := _dwidth/_swidth ;
      scale_y := _dheight/_sheight ;
      if scale_x <= scale_y then begin
        if scale_y > 2 then begin
          scale_x := 2*(scale_x/scale_y) ;
          scale_y := 2 ;
        end;
      end
      else begin
        if scale_x > 2 then begin
          scale_y := 2*(scale_y/scale_x) ;
          scale_x := 2 ;
        end;
        extZoomX := scale_x ;
        extZoomY := scale_y ;
      end ;
      w := RoundS( baseCellWidth  * scale_x  ) ;
      if w < 1 then
        w := 1;
      h := RoundS(( baseCellHeight ) * scale_y ) ;
      if h < 1 then
        h := 1 ;
      scaledWidth  := w ;
      scaledHeight := h ;
    end ;
    extZoomX := scale_x ;
    extZoomY := scale_y ;
    extZoom  := scale_x ;
    Result   := scale_x ;
  end ;

  procedure TGIS_LayerJPG.fset_Antialias(
    const _value : Boolean
  ) ;
  begin
    if FAntialias <> _value then begin
      FAntialias := _value ;
    end;
  end ;

  function TGIS_LayerJPG.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FileJPEG ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FileJPEG.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  function TGIS_LayerJPG.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FileJPEG ;
    pix       : TGIS_Pixels ;
    pixformat : TGIS_PixelFormat ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
    abrt      : Boolean ;
    cnt       : Integer ;
    pos       : Integer ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;

    f := TGIS_FileJPEG.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;
    if assigned( FOnBusy ) then
      {$IFDEF OXYGENE}
        FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
      {$ELSE}
        FOnBusy( self, -1, -1, abrt ) ;
      {$ENDIF}

    try
      if not assigned(f) then exit ;
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
      if lw > 0 then begin
        lw := 0 ;
        inc(maxc) ;
      end;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then begin
        inc(maxr) ;
      end;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
      end;

      try
        if not isBuilt then begin
          _layer.Alive ;

          SetLength(pix, cw*ch) ;
          pixformat := TGIS_PixelFormat.RGB ;
          cnt := maxr * maxc ;
          pos := 0 ;

          for i := 0 to maxr -1 do begin
            ext.YMax := FExtent.YMax -i*(ch*sy) ;
            ext.YMin := ext.YMax -ch*sy ;
            for k := 0 to maxc -1 do begin
              ext.XMin := FExtent.XMin +k*(cw*sx) ;
              ext.XMax := ext.XMin +cw*sx ;
              setBmpTransparent( pix ) ;
              _layer.GetBitmap( ext, pix, cw, ch ) ;
              f.Write( k*cw, i*ch, pix, pixformat, cw, ch );

              if assigned( FOnBusy ) then
                {$IFDEF OXYGENE}
                  FOnBusy( self, TGIS_BusyEventArgs.Create( pos, cnt, abrt ) ) ;
                {$ELSE}
                  FOnBusy( self, pos, cnt, abrt ) ;
                {$ENDIF}
              inc( pos ) ;
            end;
          end ;
        end
        else begin
          cw  := FBitWidth ;
          ch  := FBitHeight ;

          if not assigned(oBitmap) then begin
            SetLength(pix, cw*ch) ;
            _layer.GetBitmap(FExtent, pix, cw, ch) ;
          end
          else
            pix := oBitmap ;

          pixformat := TGIS_PixelFormat.RGB ;
          f.Write(0, 0, pix, pixformat, cw, ch );
          if pix <> oBitmap then
            pix := nil ;
        end ;
      finally
        if pix <> oBitmap then
          pix := nil ;
      end ;
    finally
      FreeObject(f) ;

      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, abrt ) ;
        {$ENDIF}
    end ;
  end;

  procedure TGIS_LayerJPG.Alive ;
  begin
    jpegDecoder.Alive ;
  end ;

  function TGIS_LayerJPG.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0;
      else
        Result := 10 ;
    end ;
  end ;

  procedure TGIS_LayerJPG.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    SetLength( scaledBmp, 0 ) ;

    jpegDecoder.Dormant ;
    inherited ;
  end ;

  procedure TGIS_LayerJPG.Build(
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

    try
      FBitWidth  := _width ;
      FBitHeight := _height ;

      inherited Build(_path, _grid, _cs, _ext, _width, _height, _subformat) ;
    except
      FBitWidth  := 0 ;
      FBitHeight := 0 ;
    end;
  end ;

  procedure TGIS_LayerJPG.SaveData ;
  const
    MAX_CELL_WH = 1024 ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FileJPEG ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    lsf       : TGIS_LayerPixelSubFormat ;
    tmpname   : String ;
  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;

    if not isBuilt then begin

      lsf := TGIS_LayerPixelSubFormat.Create( TGIS_PixelFormat.RGB, False,
                    TGIS_PixelSubFormat.JPEG,  TGIS_CompressionType.JPEG, 60 ) ;

        tmpname := GetFilePath(Path) +'2' +GetFileName(Path) ;
       f := TGIS_FileJPEG.Create(
            tmpname,
            FExtent ,
            FBitWidth,
            FBitHeight,
            lsf,
            96,
            CS
          ) ;
    end
    else begin
      tmpname := '' ;
      f := TGIS_FileJPEG.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;
    end;
    try
      if not assigned(f) then exit ;

      if assigned(oBitmap) then begin
        cw := FBitWidth ;
        ch := FBitHeight ;
      end
      else begin
        if FBitHeight <= MAX_CELL_WH then
          ch := FBitHeight
        else
          ch := MAX_CELL_WH ;

        if FBitWidth <= MAX_CELL_WH then
          cw := FBitWidth
        else
          cw := MAX_CELL_WH ;
      end;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      if lw > 0 then begin
        lw := 0 ;
        inc(maxc) ;
      end;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then begin
        inc(maxr) ;
      end;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
      end;
      try
        if not assigned( oBitmap ) then
          SetLength(pix, cw*ch)
        else
          pix := oBitmap ;

        pixformat := TGIS_PixelFormat.RGB ;

        for i := 0 to maxr -1 do begin
          ext.YMax := FExtent.YMax -i*(ch*sy) ;
          ext.YMin := ext.YMax -ch*sy ;
          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            if not assigned(oBitmap) then
              getBitmapData(ext, pix, cw, ch) ;
            f.Write(k*cw, i*ch, pix, pixformat, cw, ch );
          end;
        end ;
      finally
        if pix <> oBitmap then
          pix := nil ;
      end ;
    finally
      FreeObject(f) ;
      if tmpname <> '' then
        replaceWorkingFiles(tmpname) ;
    end;
  end;


  { Perform initialization section.
  }
  class procedure Unit_GisLayerJPG.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-JPEG', 'JPEG File Interchange Format', TGIS_LayerJPG, '.jpg;.jpeg',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   True
                 ) ;
    RegisterLayer( 'DK-PSI', 'Pictometry Oblique Images', TGIS_LayerJPG, '.psi',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end;

{$IFDEF DCC}
  initialization
    Unit_GisLayerJPG.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
