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
  Encapsulation of a PNG Layer.

  Use Gustavo Huffenbacher Daud TPNGImage (http://pngdelphi.sourceforge.net).
  We included TPNGImage based on author permission.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerPNG ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerPNG"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

{$IFDEF GIS_NOPNG}
  {$Message Error 'Unit not supported - no PNG support' }
{$ENDIF}

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
    {$IFDEF DEBUG_WEBTILES}
      VCL.Graphics,
    {$ENDIF}
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoLayerPixel ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerPNG = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of a PNG layer.
  /// </summary>
  TGIS_LayerPNG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private  // various private variables

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmp      : TGIS_Pixels ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmpWidth  : Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      scaledBmpHeight : Integer ;

      /// <summary>
      ///   Underlying bitmap which handles all images.
      /// </summary>
      pixObj : TGIS_Pixels ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      pngBitmapWidth    : Integer ;

      /// <summary>
      ///   Used in scaling 2D
      /// </summary>
      pngBitmapHeight   : Integer ;


      png : TGIS_Bitmap ;
    {$IFNDEF OXYGENE}
      private

        /// <summary>
        ///   Initializing background for transparency.
        ///   Filling bitmap with given color.
        /// </summary>
        procedure setBackground   ( const _color  : TGIS_Color
                                  ) ;
    {$ENDIF}
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected procedures
      /// <inheritdoc/>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure setUp             ; override;

      /// <inheritdoc/>
      function bandsMappingChanging  : Boolean  ; override;

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

      /// <summary>
      ///   Property access routine for Antialias property.
      /// </summary>
      procedure fset_Antialias    ( const _value  : Boolean
                                  ) ; override;

      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getLine           ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer ; override;

      function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;

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
      function  getScaledLine     ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer;


      /// <inheritdoc/>
      function  getAlphaLine      ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer; override;

    public // various public routines

      /// <inheritdoc/>
      procedure Alive              ; override;

      /// <inheritdoc/>
      function  DormantGain        : Integer ; override;

      /// <inheritdoc/>
      procedure Dormant            ; override;

      /// <inheritdoc/>
      procedure Build             ( const _path   : String            ;
                                    const _cs     : TGIS_CSCoordinateSystem  ;
                                    const _ext    : TGIS_Extent    ;
                                    const _width  : Integer        ;
                                    const _height : Integer
                                   ) ; override;

      /// <inheritdoc/>
      procedure SaveData           ; override;

    protected

      /// <inheritdoc/>
      procedure doDestroy          ; override;

    public // API
      // constructors

      /// <inheritdoc/>
      constructor Create           ; override;

  end ;

//##############################################################################
implementation

{$IFNDEF OXYGENE}
  uses
    {$IFDEF FMX}
      FMX.Graphics,
    {$ENDIF}
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoFilePNG ;
{$ENDIF}

const

  // Extension of world file (extension file for pixel layer).
  WORLD_FILE_EXT_PNG = '.pgw' ;

  // extension of world file (extension file for pixel layer).
  // Alternative naming.
  WORLD_FILE_EXT2_PNG = '.pngw' ;

//==============================================================================
// TGIS_LayerJPG
//==============================================================================

  constructor TGIS_LayerPNG.Create ;
  begin
    inherited ;

    FSubType := FSubType + [TGIS_LayerSubType.Persistent,
                            TGIS_LayerSubType.Exportable] ;
    png := nil ;
    
    FBitmapFormat      := TGIS_BitmapFormat.ARGB ;
    FBitmapLinesOrder  := TGIS_BitmapLinesOrder.Down ;
  end ;

  procedure TGIS_LayerPNG.doDestroy ;
  begin
    SetLength( scaledBmp, 0   ) ;
    SetLength( pixObj, 0   ) ;

    if assigned(png) then
      FreeObject( png ) ;

    inherited ;
  end ;

  procedure TGIS_LayerPNG.setUp ;
  begin
    if isBuilt then exit ;

    try
      png := TGIS_Bitmap.Create ;
      try
        if assigned( Stream ) then begin
          Stream.Position := 0 ;
          png.LoadFromStream( Stream ) ;
        end
        else begin
          png.LoadFromFile( Path ) ;
        end ;

        // get bitmap size into easy to access properties
        FBitHeight      := png.Height ;
        FBitWidth       := png.Width ;
        pngBitmapHeight := FBitHeight ;
        pngBitmapWidth  := FBitWidth ;

        FBandsCount               := 4 ;
        realBitCount              := 32 ;
        defaultPartialTransparent := True ;
        isPartialTransparent      := True ;

        {$IFDEF DEBUG_WEBTILES}
          with VCL.Graphics.TBitmap( png.NativeBitmap ).Canvas do begin
            Font.Color := clRed ;
            TextOut( 1, 1, Caption ) ;
            Brush.Color := clRed ;
            FrameRect( Rect( 0, 0, png.Width, png.Height ) ) ;
          end;
        {$ENDIF}

        png.LockPixels( pixObj, False, FBitmapFormat, FBitmapLinesOrder ) ;

        realLineWidth := (BitWidth*realBitCount) div 8 ;
      finally
        FreeObject( png ) ;
      end ;

      intLineWidth := realLineWidth ;

      // read a word file
      setWorldFile( WORLD_FILE_EXT_PNG ) ;

      // read a word file (alternative)
      if GisIsSameExtent( Extent, GisExtent( 0, 0, 0, 0 ) ) then
        setWorldFile( WORLD_FILE_EXT2_PNG ) ;

      inherited setUp ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'Portable Network Graphic (PNG)' + #13#10 +
                           '%d x %d, %d bit',
                           [ FBitWidth, FBitHeight, realBitCount ]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;

  end ;

  function TGIS_LayerPNG.bandsMappingChanging : Boolean ;
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


  {$IFNDEF OXYGENE}

    procedure TGIS_LayerPNG.setBackground(
      const _color : TGIS_Color
    ) ;
    var
      no, i : Integer ;
    begin
      no := Length(pixObj) ;
      for i := 0 to no -1 do
        pixObj[i] := _color.ARGB ;
    end ;

  {$ENDIF}

  function  TGIS_LayerPNG.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer;
  var
    l       : Integer ;
    linenr  : Integer ;
    bitsidx : Integer ;
    count   : Integer ;
    st      : Integer ;
    k       : Integer ;
    lpix    : TGIS_Pixels ;

    procedure make_scaled_bmp ;
    var
      bitmapObj : TGIS_Pixels ;
      sw, sh    : Integer ;
      w, h      : Integer ;
      decscale  : Double ;
    begin
      w := scaledWidth ;
      h := scaledHeight ;

      decscale := 1 ;
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
    Result := 0 ;
    linenr := _linenr ;

    if not Antialias then begin
      if assigned(pixObj) then
        lpix := pixObj
      else if isBuilt then begin
        Result := inherited getLinePixels(_buffer, _offset, _linenr, _pixStart,
                              _pixCount) ;
        exit ;
      end
      else begin
        exit ;
      end ;

      linenr := _linenr ;
      if linenr >= pngBitmapHeight then
        linenr := pngBitmapHeight - 1  ;

      bitsidx := pngBitmapWidth*linenr ;
      l := _pixStart ;
      count := _pixCount ;
      if count > pngBitmapWidth then
        count := pngBitmapWidth ;

      if (l + count) > pngBitmapWidth then
        l := pngBitmapWidth -count ;

      st := l +bitsidx ;
      for k := 0 to count -1 do begin
         _buffer[_offset+k] := Integer(lpix[ st+k ]);
      end ;
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

      bitsidx := scaledWidth*linenr ;

      l := _pixStart ;
      count := _pixCount ;
      if count > scaledWidth then
        count := scaledWidth ;

      if (l + count) > scaledWidth then
        l := scaledWidth -count ;

      st := l +bitsidx ;
      for k := 0 to count -1 do begin
         _buffer[_offset+k] := Integer(scaledBmp[ st+k ]);
      end ;

    end;
    Result := _pixCount ;
  end ;


  function TGIS_LayerPNG.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i       : Integer ;
    linenr  : Integer ;
    bitsidx : Integer  ;
    st      : Integer ;
    pix     : TGIS_Color {$IFDEF GIS_NORECORDS} := new TGIS_Color {$ENDIF} ;
    k       : Integer ;
    lpix    : TGIS_Pixels ;
  begin
    Result := 0 ;
    linenr := _linenr ;

    if assigned(pixObj) then
      lpix := pixObj
    else if isBuilt then begin
      Result := inherited getLine(_buffer, _offset, _linenr, _start, _bytes) ;
      exit ;
    end
    else begin
      exit ;
    end;

    linenr := _linenr ;
    if linenr >= pngBitmapHeight then
      linenr := pngBitmapHeight - 1  ;

    bitsidx := pngBitmapWidth*linenr ;
    st := ( _start div 3 ) +bitsidx ;
    i := 0 ;
    for k := 0 to (_bytes div 3)-1 do begin
      pix.ARGB  := Cardinal(lpix[ st+k ] ) ;
      _buffer[_offset+i]   := pix.B ;
      _buffer[_offset+i+1] := pix.G ;
      _buffer[_offset+i+2] := pix.R ;
      inc( i, 3 ) ;
    end ;
    Result := _bytes ;
  end ;

  function TGIS_LayerPNG.getScaledLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i, ix, k  : Integer ;
    w3        : Integer ;
    linenr,
    bytes,
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

      decscale := 1 ;
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
    end ;

    if linenr >= scaledHeight then
      linenr := scaledHeight -1 ;
    w3 := scaledWidth*3 ;
    if (start +bytes) > w3 then begin
      if start > 0 then begin
        start := w3 -bytes ;
        if start < 0 then begin
          start := 0 ;
          bytes := w3 ;
        end ;
      end
      else
        bytes := w3 -start ;
    end ;
    if bytes <= 0 then begin
      Result := 0 ;
      exit ;
    end ;

    pbits_idx := scaledBmpWidth*linenr ;
    pbits_idx := pbits_idx +(start div 3) ;
    pix := bytes div 3 ;
    i := 0 ;
    ix := 0 ;

    for k := 1 to pix do begin
      _buffer[i] := Byte((scaledBmp[pbits_idx + ix]       ) and $FF) ;
      inc( i ) ;
      _buffer[i] := Byte((scaledBmp[pbits_idx + ix] shr  8) and $FF) ;
      inc( i ) ;
      _buffer[i] := Byte((scaledBmp[pbits_idx + ix] shr 16) and $FF);
      inc( i ) ;
      inc( ix ) ;
    end ;
    Result := bytes ;
  end ;

  function  TGIS_LayerPNG.setFileScale(
    const _dwidth : Double ;
    const _swidth : Double
  ) : Double ;
  var
    w, h  : Integer ;
    scale : Double ;
  begin
    if Antialias then begin
      scale := _dwidth/_swidth ;
      if scale < 0.05 then
        scale := 0.05
      else
      if scale > 2 then
        scale := 2 ;

      w := RoundS( pngBitmapWidth * scale  ) ;
      if w < 1 then
        w := 1;
      h := RoundS( pngBitmapHeight * scale ) ;
      if h < 1 then
        h := 1 ;
      if (w <> scaledWidth) or (h <> scaledHeight) then begin
        scaledWidth  := w ;
        scaledHeight := h ;
      end ;
    end
    else begin
      scale := 1.0 ;
      pngBitmapHeight := FBitHeight ;
      pngBitmapWidth  := FBitWidth ;
    end ;

    Result   := scale ;
  end ;

  function  TGIS_LayerPNG.setFileScaleXY(
    const _dwidth  : Double ;
    const _swidth  : Double ;
    const _dheight : Double ;
    const _sheight : Double
  ) : Double ;
  var
    w, h    : Integer ;
    scale_x : Double ;
    scale_y : Double ;
  begin
    if Antialias then begin
        scale_x := _dwidth/_swidth ;
        scale_y := _dheight/_sheight ;
        if scale_x <= scale_y then begin
          if scale_y > 2 then begin
            scale_x := 2*(scale_x/scale_y) ;
            scale_y := 2 ;
          end ;
        end
        else begin
          if scale_x > 2 then begin
            scale_y := 2*(scale_y/scale_x) ;
            scale_x := 2 ;
          end ;
        end ;

      w := RoundS( pngBitmapWidth * scale_x  ) ;
      if w < 1 then
        w := 1;
      h := RoundS( pngBitmapHeight * scale_y ) ;
      if h < 1 then
        h := 1 ;
      if (w <> scaledWidth) or (h <> scaledHeight) then begin

        scaledWidth  := w ;
        scaledHeight := h ;
      end ;

    end
    else begin
      scale_x := 1.0 ;
      scale_y := 1.0 ;
      pngBitmapHeight := FBitHeight ;
      pngBitmapWidth  := FBitWidth ;
    end ;

    extZoomX := scale_x ;
    extZoomY := scale_y ;
    Result   := scale_x ;
  end ;

  procedure TGIS_LayerPNG.fset_Antialias(
    const _value : Boolean
  ) ;
  begin
    if FAntialias <> _value then begin
      FAntialias := _value ;
    end ;
    if not FAntialias then
      extZoom := 1 ;
  end ;

  function TGIS_LayerPNG.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FilePNG ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FilePNG.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;


  function TGIS_LayerPNG.getAlphaLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i      : Integer ;
    bytes  : Integer ;
    start  : Integer ;
    linenr : Integer ;
    pix    : TGIS_Color ;
    lpix   : TGIS_Pixels ;
  begin
    Result := 0 ;
    if (not defaultPartialTransparent) or isBuilt then begin
      Result := inherited getAlphaLine(_buffer, _offset, _linenr, _start, _bytes) ;
      exit ;
    end ;

    if assigned(pixObj) then
      lpix := pixObj
    else if assigned(oBitmap) then
      lpix := oBitmap
    else
      exit ;

    start  := _start ;
    bytes  := _bytes ;
    linenr := _linenr ;

    if FAntialias and assigned(scaledBmp) then begin
      if (bytes + start) > scaledBmpWidth then begin
        if start > 0 then
          start := scaledBmpWidth -bytes ;
        if start < 0 then begin
          start := 0 ;
          bytes := scaledBmpWidth ;
        end ;
      end ;
      if linenr >= scaledBmpHeight then
        linenr := scaledBmpHeight -1 ;
    end
    else begin
      if (bytes + start) > FBitWidth then begin
        if start > 0 then
          start := FBitWidth -bytes ;
        if start < 0 then begin
          start := 0 ;
          bytes := FBitWidth ;
        end ;
      end ;

      if linenr >= pngBitmapHeight then
        linenr := pngBitmapHeight -1 ;
    end ;

    if FAntialias and assigned(scaledBmp) then begin
      for i := 0 to bytes -1 do begin
        pix := TGIS_Color.FromARGB(Cardinal(scaledBmp[scaledBmpWidth*linenr +start +i])) ;
        _buffer[i] := pix.A ;
      end ;
    end
    else begin
      for i := 0 to bytes -1 do begin
        pix := TGIS_Color.FromARGB(Cardinal(lpix[FBitWidth*linenr +start +i])) ;
        _buffer[i] := pix.A ;
      end ;
    end ;
    Result := bytes ;
  end ;

  function TGIS_LayerPNG.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FilePNG ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    sf        : TGIS_LayerPixelSubFormat ;
    abrt      : Boolean ;
    cnt       : Integer ;
    pos       : Integer ;
    istr      : Boolean ;
    trcolor   : Integer ;
  const
    MAX_CELL_WH = 1024 ;
    procedure set_transparent ;
      var
        ii : Integer ;
    begin
      for ii := low(pix) to high(pix) do
        if (pix[ii] and $00FFFFFF) = trcolor then
          pix[ii] := 0 ;
    end;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;

    sf := TGIS_LayerPixelSubFormat.Create(
             FSubFormat.PixelFormat,
             False,
             TGIS_PixelSubFormat.PNG,
             TGIS_CompressionType.PNG,
             0
           ) ;

    f := TGIS_FilePNG.Create(
            Path,
            FExtent ,
            FBitWidth,
            FBitHeight,
            sf,
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

      pngBitmapHeight := FBitHeight ;
      pngBitmapWidth  := FBitWidth ;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      if lw > 0 then begin
        lw := 0 ;
        inc(maxc) ;
      end;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then begin
        lh := 0 ;
        inc(maxr) ;
      end;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
        lw := 0 ;
      end;

      pixformat := FSubFormat.PixelFormat ;
      istr := False ;
      if pixformat = TGIS_PixelFormat.ARGB then begin
        trcolor := Integer(NoDataColor.ARGB) and $00FFFFFF ;
        if trcolor <> 0 then
          istr := True
        else
        if (Integer(NoDataColor.ARGB) and Integer($FF000000)) <> 0 then
          istr := True ;
      end ;

      try
        if not assigned(oBitmap) then begin
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
              if istr then
                set_transparent ;
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
          pix := oBitmap ;
          cw  := FBitWidth ;
          ch  := FBitHeight ;

          pixformat := TGIS_PixelFormat.RGB ;
          if istr then
            set_transparent ;
          f.Write(0, 0, pix, pixformat, cw, ch );
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
  end ;

  procedure TGIS_LayerPNG.Alive ;
  begin
    if assigned( pixObj ) and
       ( length( pixObj ) > 0 ) then exit ;
    if isBuilt then exit ;

    try
      png := TGIS_Bitmap.Create ;
      try
        if assigned( Stream ) then begin
          Stream.Position := 0 ;
          png.LoadFromStream( Stream ) ;
        end
        else
          png.LoadFromFile( Path ) ;

        pngBitmapHeight := png.Height ;
        pngBitmapWidth  := png.Width ;

        png.LockPixels( pixObj, False, FBitmapFormat, FBitmapLinesOrder ) ;
      finally
        FreeObject( png ) ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerPNG.DormantGain
    : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0 ;
      else
        Result := 5 ;
    end ;
  end ;

  procedure TGIS_LayerPNG.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then exit ;

    inherited ;

    SetLength( pixObj, 0 ) ;
    SetLength( scaledBmp, 0 ) ;
    if assigned(png) then
      FreeObject( png ) ;
  end ;

  procedure TGIS_LayerPNG.Build(
    const _path   : String            ;
    const _cs     : TGIS_CSCoordinateSystem  ;
    const _ext    : TGIS_Extent    ;
    const _width  : Integer        ;
    const _height : Integer
  ) ;
  begin
    if SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXIST ), _path, 0 ) ;
    end ;

    inherited Build( _path, _cs, _ext, _width, _height) ;
  end ;

  procedure TGIS_LayerPNG.SaveData ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FilePNG ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    istr      : Boolean ;
    trcolor   : Integer ;
    tmpname   : String ;
  const
    MAX_CELL_WH = 1024 ;

    procedure set_transparent ;
      var
        ii : Integer ;
    begin
      for ii := low(pix) to high(pix) do
        if (pix[ii] and $00FFFFFF) = trcolor then
          pix[ii] := 0 ;
    end;
  begin
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    if not isBuilt then begin


     tmpname := GetFilePath(Path) +'2' +GetFileName(Path) ;
     f := TGIS_FilePNG.Create(
            tmpname,
            FExtent ,
            FBitWidth,
            FBitHeight,
            FSubFormat,
            96,
            CS
          ) ;

    end
    else begin
      tmpname := '' ;
      f := TGIS_FilePNG.Create(
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
      cw := FBitWidth ;
      ch := FBitHeight ;
      pngBitmapHeight := FBitHeight ;
      pngBitmapWidth  := FBitWidth ;

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
      end ;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      if lw > 0 then begin
        lw := 0 ;
        inc(maxc) ;
      end ;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;
      if lh > 0 then begin
        lh := 0 ;
        inc(maxr) ;
      end ;

      sx := (FExtent.XMax -FExtent.XMin)/FBitWidth  ;
      sy := (FExtent.YMax -FExtent.YMin)/FBitHeight ;

      if maxc = 0 then begin
        maxc := 1 ;
        cw := lw ;
        lw := 0 ;
      end ;
      try
        if not assigned(oBitmap) then
          SetLength(pix, cw*ch)
        else
          pix := oBitmap ;

        pixformat := FSubFormat.PixelFormat ;
        istr := False ;
        trcolor := Integer(NoDataColor.ARGB) and $00FFFFFF ;
        if trcolor <> 0 then
          istr := True
        else
        if (Integer(NoDataColor.ARGB) and Integer($FF000000)) <> 0 then
          istr := True ;



        for i := 0 to maxr -1 do begin
          ext.YMax := FExtent.YMax -i*(ch*sy) ;
          ext.YMin := ext.YMax -ch*sy ;
          for k := 0 to maxc -1 do begin
            ext.XMin := FExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            if not assigned(oBitmap) then begin
              setBmpTransparent( pix ) ;
              getBitmapData(ext, pix, cw, ch) ;
            end;
            if istr then
              set_transparent ;
            f.Write(k*cw, i*ch, pix, pixformat, cw, ch );
          end ;
        end ;
      finally
        if pix <> oBitmap then
          pix := nil ;
      end ;
    finally
      FreeObject(f) ;
      if tmpname <> '' then
        replaceWorkingFiles(tmpname) ;
    end ;
  end ;


  { Perform initialization section.
  }
  class procedure GisLayerPNG.SelfRegisterLayer() ;
  begin
    {$IFNDEF GIS_NOPNG}
    RegisterLayer( 'DK-PNG', 'Portable Network Graphic', TGIS_LayerPNG, '.png',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create ],
                   True
                 ) ;

    {$ENDIF}
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerPNG.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

