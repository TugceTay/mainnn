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
  Encapsulation of a BMP Layer.
}

{$IFDEF DCC}
   unit Lider.CG.GIS.GeoLayerBMP ;
   {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerBMP"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoFileBMP,
    Lider.CG.GIS.GeoCsSystems ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  GisLayerBMP = class
    public
      class procedure SelfRegisterLayer() ;
  end ;

  /// <summary>
  ///   Encapsulation of bitmap layer.
  /// </summary>
  TGIS_LayerBMP = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private
      bmpBitHeight : Integer ;
      bitsOffset : Cardinal ;
      imageSize  : Cardinal ;
      isBitFields  : Boolean ;
      is15Bits     : Boolean ;
      rBitMask     : Word ;
      gBitMask     : Word ;
      bBitMask     : Word ;
      rShrVal      : Integer ;
      gShrVal      : Integer ;
      bShrVal      : Integer ;
      rShlVal      : Integer ;
      gShlVal      : Integer ;
      bShlVal      : Integer ;
      alphaBMP     : TBytes ;
      bitmapHeader : TGIS_BitmapInfoHeader ;
      fileHeader   : TGIS_BitmapFileHeader ;
    // various protected routines
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      function  importPixelData   ( const _layer       : TGIS_LayerPixel
                                  ) : Boolean ; override;

      /// <inheritdoc/>
      procedure setUp    ; override;

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
                                  ) : Integer; override;

      /// <inheritdoc/>
      function  getAlphaLine      ( const _buffer : TBytes  ;
                                    const _offset : Integer ;
                                    const _linenr : Integer ;
                                    const _start  : Integer ;
                                    const _bytes  : Integer
                                  ) : Integer ; override;
      /// <inheritdoc/>
      function  getLineBits       ( const _buffer   : TBytes  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer ; override;

      function  fget_Capabilities   : TGIS_LayerPixelSubFormatList ; override;
    public // various public routines

      /// <inheritdoc/>
      constructor Create  ; override;


      /// <inheritdoc/>
      procedure Build ( const _path   : String            ;
                        const _cs     : TGIS_CSCoordinateSystem  ;
                        const _ext    : TGIS_Extent    ;
                        const _width  : Integer        ;
                        const _height : Integer
                       ) ; override;

      /// <inheritdoc/>
      procedure SaveData   ; override;

  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRegistredLayers ;
{$ENDIF}

const
  // Extension of world file (extension file for pixel layer).
  WORLD_FILE_EXT_BMP = '.bpw' ;

  // Extension of world file (extension file for pixel layer).
  // Alternative naming.
  WORLD_FILE_EXT2_BMP = '.bmpw' ;

  GIS_BI_RGB = 0;
  GIS_BI_RLE8 = 1;
  GIS_BI_RLE4 = 2;
  GIS_BI_BITFIELDS = 3;

//==============================================================================
// TGIS_LayerBMP
//==============================================================================

  function TGIS_LayerBMP.importPixelData(
    const _layer : TGIS_LayerPixel
  ) : Boolean ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FileBMP ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent {$IFDEF GIS_NORECORDS} = new TGIS_Extent{$ENDIF} ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
    abrt      : Boolean ;
    cnt       : Integer ;
    pos       : Integer ;
  const
    MAX_CELL_WH = 1024 ;
  begin
    Result := False ;
    if IsStringEmpty( Path ) then exit ;
    {$IFDEF GIS_NORECORDS}
     ext:= TGIS_Extent.Create ;
    {$ENDIF}

    f := TGIS_FileBMP.Create(
            Path,
            FProjectedExtent ,
            CellWidth,
            CellHeight,
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
      lw   := FBitWidth mod cw ;
      maxr := FBitHeight div ch ;
      lh   := FBitHeight mod ch ;

      sx := (FProjectedExtent.XMax -FProjectedExtent.XMin)/FBitWidth  ;
      sy := (FProjectedExtent.YMax -FProjectedExtent.YMin)/FBitHeight ;

      if (maxr > 0) then begin
        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end ;
        SetLength(pix, cw*ch) ;
      end ;

      pixformat := FSubFormat.PixelFormat ;
      cnt := maxr * maxc ;
      pos := 0 ;

      for i := 0 to maxr -1 do begin
        ext.YMax := FProjectedExtent.YMax -i*(ch*sy) ;
        ext.YMin := ext.YMax -ch*sy ;
        for k := 0 to maxc -1 do begin
          ext.XMin := FProjectedExtent.XMin +k*(cw*sx) ;
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
        end ;
        if lw <> 0 then begin
          SetLength(pix, lw*ch) ;

          ext.XMin := FProjectedExtent.XMax -lw*sx;
          ext.XMax := FProjectedExtent.XMax ;
          setBmpTransparent( pix ) ;
           _layer.GetBitmap(ext, pix, lw, ch) ;
          f.Write(maxc*cw, i*ch, pix, pixformat, lw, ch );
          SetLength(pix, cw*ch) ;
        end ;
      end ;

      if lh <> 0 then begin
        ext.YMin := FProjectedExtent.YMin ;
        ext.YMax := FProjectedExtent.YMin +(lh*sy) ;

        if maxc > 0 then begin
          SetLength(pix, cw*lh) ;
          for k := 0 to maxc -1 do begin
            ext.XMin := FProjectedExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            setBmpTransparent( pix ) ;
             _layer.GetBitmap(ext, pix, cw, lh) ;
            f.Write( k*cw, maxr*ch, pix, pixformat, cw, lh );
          end ;
        end ;
        if lw <> 0 then begin
          ext.XMin := FProjectedExtent.XMax -(lw*sx) ;
          ext.XMax := FProjectedExtent.XMax ;
          SetLength(pix, lw*lh) ;
          setBmpTransparent( pix ) ;
           _layer.GetBitmap(ext, pix, lw, lh) ;
          f.Write( maxc*cw, maxr*ch, pix, pixformat, lw, lh );
        end ;
      end ;
      pix := nil ;
    finally
      FreeObject(f) ;
      if assigned( FOnBusy ) then
        {$IFDEF OXYGENE}
          FOnBusy( self, TGIS_BusyEventArgs.Create( -1, -1, abrt ) ) ;
        {$ELSE}
          FOnBusy( self, -1, -1, abrt ) ;
        {$ENDIF}
    end ;
    Result := True ;
  end ;

  constructor TGIS_LayerBMP.Create ;
  begin
    inherited ;

    {$IFDEF OXYGENE}
      bitmapHeader := new TGIS_BitmapInfoHeader ;
      fileHeader   := new TGIS_BitmapFileHeader ;
    {$ENDIF}

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.Exportable ] ;
  end ;

  function TGIS_LayerBMP.fget_Capabilities : TGIS_LayerPixelSubFormatList ;
  var
    f : TGIS_FileBMP ;
    {$IFDEF DCC}
      c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
  begin
    Result := inherited ;

    f := TGIS_FileBMP.Create ;
    try
      Result.Clear ;

      for c in f.Capabilities do
        Result.Add( c.CreateCopy ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;

  procedure TGIS_LayerBMP.Build(
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

  procedure TGIS_LayerBMP.SaveData ;
  var
    i, k      : Integer ;
    cw, ch    : Integer ;
    lw, lh    : Integer ;
    maxc,maxr : Integer ;
    f         : TGIS_FileBMP ;
    pix       : TGIS_Pixels ;
    ext       : TGIS_Extent ;
    sx, sy    : Double ;
    pixformat : TGIS_PixelFormat ;
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
    if IsStringEmpty( Path ) then exit ;

    if not MustSave then exit ;
    if not isBuilt then exit ;

    {$IFDEF GIS_NORECORDS}
    ext := TGIS_Extent.Create ;
    {$ENDIF}

    f := TGIS_FileBMP.Create(
            Path,
            FProjectedExtent ,
            CellWidth,
            CellHeight,
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

      if FBitWidth <= MAX_CELL_WH then
        cw := FBitWidth
      else
        cw := MAX_CELL_WH ;

      maxc := FBitWidth div cw ;
      lw := FBitWidth mod cw ;
      maxr := FBitHeight div ch ;
      lh := FBitHeight mod ch ;

      sx := (FProjectedExtent.XMax -FProjectedExtent.XMin)/FBitWidth  ;
      sy := (FProjectedExtent.YMax -FProjectedExtent.YMin)/FBitHeight ;

      if (maxr > 0) then begin
        if maxc = 0 then begin
          maxc := 1 ;
          cw := lw ;
          lw := 0 ;
        end ;
        SetLength(pix, cw*ch) ;
      end ;

      pixformat := FSubFormat.PixelFormat ;
      istr := False ;
      trcolor := Integer(NoDataColor.ARGB) and $00FFFFFF ;
      if trcolor <> 0 then
        istr := True
      else
      if (Integer(NoDataColor.ARGB) and Integer($FF000000)) <> 0 then
        istr := True ;

      for i := 0 to maxr -1 do begin
        ext.YMax := FProjectedExtent.YMax -i*(ch*sy) ;
        ext.YMin := ext.YMax -ch*sy ;
        for k := 0 to maxc -1 do begin
          ext.XMin := FProjectedExtent.XMin +k*(cw*sx) ;
          ext.XMax := ext.XMin +cw*sx ;
          setBmpTransparent( pix ) ;
          getBitmapPixels(ext, pix, cw, ch) ;
          if istr then
            set_transparent ;
          f.Write(k*cw, i*ch, pix, pixformat, cw, ch );
        end ;
        if lw <> 0 then begin
          SetLength(pix, lw*ch) ;

          ext.XMin := FProjectedExtent.XMax -lw*sx;
          ext.XMax := FProjectedExtent.XMax ;
          setBmpTransparent( pix ) ;
          getBitmapData(ext, pix, lw, ch) ;
          if istr then
            set_transparent ;
          f.Write(maxc*cw, i*ch, pix, pixformat, lw, ch );
          SetLength(pix, cw*ch) ;
        end ;
      end ;

      if lh <> 0 then begin
        ext.YMin := FProjectedExtent.YMin ;
        ext.YMax := FProjectedExtent.YMin +(lh*sy) ;

        if maxc > 0 then begin
          SetLength(pix, cw*lh) ;
          for k := 0 to maxc -1 do begin
            ext.XMin := FProjectedExtent.XMin +k*(cw*sx) ;
            ext.XMax := ext.XMin +cw*sx ;
            setBmpTransparent( pix ) ;
            getBitmapPixels(ext, pix, cw, lh) ;
            if istr then
              set_transparent ;
            f.Write(k*cw, maxr*ch, pix, pixformat, cw, lh );
          end ;
        end ;
        if lw <> 0 then begin
          ext.XMin := FProjectedExtent.XMax -(lw*sx) ;
          ext.XMax := FProjectedExtent.XMax ;
          SetLength(pix, lw*lh) ;
          setBmpTransparent( pix ) ;
          getBitmapPixels(ext, pix, lw, lh) ;
          if istr then
            set_transparent ;
          f.Write(maxc*cw, maxr*ch, pix, pixformat, lw, lh );
        end ;
      end ;
      pix := nil ;
    finally
      FreeObject(f) ;
    end ;
  end ;

  procedure TGIS_LayerBMP.setUp ;
  var
    {$IFDEF OXYGENE}
      i  : Integer ;
      cc : Cardinal ;
    {$ENDIF}
    intBitCount  : Integer;
    intLineWidth : Integer;
    wfile        : String ;
  begin
    if isBuilt then exit ;

    try
      // read bitmap parameters
      if assigned( Stream ) then begin
        Stream.Position := 0 ;
        fileStream := TGIS_HandleStream.Create(Stream) ;
      end
      else begin
        fileStream := TGIS_FileStream.Create( Path,
                                              fmOpenRead or
                                              fmShareDenyWrite
                                            ) ;
      end ;

      {$IFDEF OXYGENE}
        fileStream.ReadWord( fileHeader.bfType        ) ;
        fileStream.ReadCardinal( fileHeader.bfSize    ) ;
        fileStream.ReadWord( fileHeader.bfReserved1   ) ;
        fileStream.ReadWord( fileHeader.bfReserved2   ) ;
        fileStream.ReadCardinal( fileHeader.bfOffBits ) ;
      {$ELSE}
       fileStream.Read( fileHeader, sizeof (TGIS_BitmapFileHeader) ) ;
      {$ENDIF}

      bitsOffset := fileHeader.bfOffBits;
      {$IFDEF OXYGENE}
        fileStream.ReadCardinal( bitmapHeader.biSize          ) ;
        fileStream.ReadInteger( bitmapHeader.biWidth          ) ;
        fileStream.ReadInteger( bitmapHeader.biHeight         ) ;
        fileStream.ReadWord( bitmapHeader.biPlanes            ) ;
        fileStream.ReadWord( bitmapHeader.biBitCount          ) ;
        fileStream.ReadCardinal( bitmapHeader.biCompression   ) ;
        fileStream.ReadCardinal( bitmapHeader.biSizeImage     ) ;
        fileStream.ReadInteger( bitmapHeader.biXPelsPerMeter  ) ;
        fileStream.ReadInteger( bitmapHeader.biYPelsPerMeter  ) ;
        fileStream.ReadCardinal( bitmapHeader.biClrUsed       ) ;
        fileStream.ReadCardinal( bitmapHeader.biClrImportant  ) ;
      {$ELSE}
        fileStream.Read( bitmapHeader, sizeof (TGIS_BitmapInfoHeader) ) ;
      {$ENDIF}

      realBitCount := bitmapHeader.biBitCount ;

      case realBitCount of
        1,
        4,
        8  :
          begin
          {$IFDEF OXYGENE}
            for i := 0 to (1 shl realBitCount) -1 do begin
            {$IFDEF GIS_NORECORDS}
               bitPalette[i] := new TGIS_Color;
            {$ENDIF}
              fileStream.ReadCardinal( cc ) ;
              bitPalette[i].ARGB := cc ;
            end ;
          {$ELSE}
            fileStream.Read( bitPalette[0], (1 shl realBitCount)*4) ;
          {$ENDIF}
            bitsPerBand[0] := realBitCount ;
            bitsPerPixel := bitsPerBand[0] ;
            bytesPerPixel := 1 ;
            bytesPerBand[0] := bytesPerPixel ;
            FBandsCount := 1 ;
            if realBitCount <> 8 then
              isBitsString := True ;
          end;
        15 :
          begin
            bitsSkipLeft[0] := 1 ;
            bitsPerBand[0]  := 6 ;
            bitsPerBand[1 ] := 5 ;
            bitsPerBand[2]  := 5 ;
            bytesPerBand[0] := 1 ;
            bytesPerBand[1] := 1 ;
            bytesPerBand[2] := 1 ;
            bitsPerPixel    := realBitCount ;
            bytesPerPixel   := 2 ;
            FBandsCount     := 3 ;
            isBitsString    := True ;
          end;
        16 :
          begin
            if  bitmapHeader.biCompression = GIS_BI_BITFIELDS then begin
              bitsPerBand[0]  := 5 ;
              bitsPerBand[1]  := 6 ;
            end
            else begin
              bitsPerBand[0]  := 6 ;
              bitsPerBand[1]  := 5 ;
            end;
            bitsPerBand[2]  := 5 ;
            bytesPerBand[0] := 1 ;
            bytesPerBand[1] := 1 ;
            bytesPerBand[2] := 1 ;
            bitsPerPixel    := realBitCount ;
            bytesPerPixel   := realBitCount div 8 ;
            FBandsCount     := 3 ;
            isBitsString    := True ;
          end;
        24,
        32   :
          begin
            bitsPerBand[0] := 8 ;
            bitsPerBand[1] := 8 ;
            bitsPerBand[2] := 8 ;
            bytesPerBand[0] := 1 ;
            bytesPerBand[1] := 1 ;
            bytesPerBand[2] := 1 ;
            bitsPerPixel := realBitCount ;
            bytesPerPixel := realBitCount div 8 ;
            FBandsCount := bytesPerPixel ;
            isBGR := True ;
            if realBitCount = 32 then begin
              alphaAssociated := True ;
              bitsPerBand[3] := 8 ;
              bytesPerBand[3] := 1 ;
            end;
          end;
      end;

      if (realBitCount = 15 ) or
         (realBitCount = 16 ) then
      begin
        is15Bits := True ;
        if ( bitmapHeader.biCompression = GIS_BI_BITFIELDS )
             AND (realBitCount <> 15 )
        then begin
          rBitMask:= $F800;
          gBitMask:= $07e0;
          bBitMask:= $001f;
          rShrVal:= 11;
          gShrVal:= 5;
          bShrVal:= 0;
          rShlVal:= 3;
          gShlVal:= 2;
          bShlVal:= 3;
        end
        else begin
          rBitMask:= $7c00;
          gBitMask:= $03e0;
          bBitMask:= $001f;
          rShrVal:= 10;
          gShrVal:= 5;
          bShrVal:= 0;
          rShlVal:= 3;
          gShlVal:= 3;
          bShlVal:= 3;
        end ;
      end
      else
        is15Bits := False ;

//      imageSize := bitmapHeader.biSizeImage ;

      if bitmapHeader.biCompression <> GIS_BI_RGB then // compression not supported
        if (bitmapHeader.biCompression <> GIS_BI_BITFIELDS) then
          Abort ;
      //get bitmap size into easy-to-access properties
      FBitHeight    := bitmapHeader.biHeight   ;
      bmpBitHeight  := FBitHeight ;
      FBitHeight    := Abs(FBitHeight);
      FBitWidth     := bitmapHeader.biWidth    ;
      realLineWidth := (FBitWidth*realBitCount +7) div 8;

      while (realLineWidth mod 4) <> 0 do
        inc(realLineWidth);

      imageSize := realLineWidth*FBitHeight;

      if (realBitCount = 1 ) or
         (realBitCount = 4 ) or
         (realBitCount = 8 ) or
         (realBitCount = 15 ) or
         (realBitCount = 16 ) or
         (realBitCount = 32) then
      begin
        intBitCount  := 24 ;
        intLineWidth := FBitWidth * 3 ;

        while (intLineWidth mod 4) <> 0 do
          inc(intLineWidth);
          bitmapHeader.biBitCount := intBitCount ;
      end ;

      // read a word file
      setWorldFile( WORLD_FILE_EXT_BMP ) ;

      // read a word file (alternative)
      wfile := GetPathNoExt( Path ) + UpperCase( WORLD_FILE_EXT2_BMP ) ;
      if SafeFileExists( wfile ) then
       if GisIsSameExtent( Extent, GisExtent( 0, 0, 0, 0 ) ) or
          ( ( scaleX = 0 ) and ( scaleY = 0 ) ) then
         setWorldFile( WORLD_FILE_EXT2_BMP ) ;

      if bitmapHeader.biCompression = GIS_BI_BITFIELDS then begin
        bitmapHeader.biCompression := 0 ;
        bitmapHeader.biBitCount := 24;
      end
      else
         isBitFields := False ;

      if realBitCount = 32 then begin
        SetLength(alphaBMP, FBitWidth) ;
        isBitFields := True ;
      end ;

      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'Windows Bitmap (BMP)' + #13#10 +
                           '%d x %d; %d bit',
                           [ FBitWidth, FBitHeight, realBitCount ]
                         ) ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end ;

  function TGIS_LayerBMP.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    buf : TBytes ;
    procedure changeOrder ;
    var
      hb : Byte ;
      i : Integer ;
    begin
      for i := 0 to _pixCount -1 do begin
        hb := buf[2*i] ;
        buf[2*i] := buf[2*i +1] ;
        buf[2*i +1] := hb ;
      end;
    end;
    procedure icreaseWeight555 ;
    var
      k : Integer ;
    begin
      for k := 0 to _pixCount -1 do
        _buffer[k] := (_buffer[k] shl 3) or Integer($FF000000) ;
    end;

    procedure icreaseWeight565 ;
    var
      n  : Integer ;
      ib : Integer ;
    const
      r_m = $0000FF00 ;
      s_m = $FFFF00FF ;
    begin
      for n := 0 to _pixCount -1 do begin
        ib := ((_buffer[n] and r_m) shl 2) or Integer($FF000000 );
        _buffer[n] := Integer(((Cardinal(_buffer[n]) and s_m) shl 3) or Cardinal(ib)) ;
      end;
    end;


  begin
    if isBuilt then begin
      Result := inherited getLinePixels(_buffer, _offset, _linenr, _pixStart, _pixCount) ;
      exit ;
    end;

    if bytesPerPixel = 0 then begin
      if isBuilt then begin
          bitsPerPixel := 32 ;
          bytesPerPixel := 4 ;
          FBandsCount := 4 ;
          bitsPerBand[0] := 8 ;
          bitsPerBand[1] := 8 ;
          bitsPerBand[2] := 8 ;
          bitsPerBand[3] := 8 ;
          bytesPerBand[0] := 1 ;
          bytesPerBand[1] := 1 ;
          bytesPerBand[2] := 1 ;
          bytesPerBand[3] := 1 ;
          alphaAssociated := True ;
          isBGR := True ;
      end
      else begin
        bytesPerPixel := 1 ;
        FBandsCount := bytesPerPixel ;
        bitsPerBand[0] := bytesPerPixel ;
        bytesPerBand[0] := bytesPerPixel ;
      end ;
    end;
    SetLength(buf, _pixCount*bytesPerPixel) ;

    getLineBits(buf, 0, _linenr, _pixStart, _pixCount) ;

    if realBitCount = 16 then
      changeOrder ;


    Result := convertBitsToPixels(buf, 0, _buffer, _offset,
                                  _pixStart, _pixCount) ;
    if realBitCount = 16 then begin
      if bitsPerBand[1] = 6 then
        icreaseWeight565
      else
        icreaseWeight555 ;
    end ;
  end;

  function TGIS_LayerBMP.getLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer;
  var
    offset : Int64 ;
    start : Integer ;
    sbuffer  : TBytes ;
    sbuffoff : Integer ;
    soffset    : Integer ;
    lp     : Integer ;
    bytes  : Integer ;

    procedure del_fields ;
    var
      k, l : Integer ;
      sas : Cardinal ;
      ss : Integer ;
    begin
      lp := bytes div 4 ;
      offset := _offset ;
      soffset := sbuffoff ;

      sas := 0 ;
      ss := _start div 3 ;

      for k := 0 to lp -1 do begin
        for l := 0 to 2 do begin
            _buffer[offset+l] := sbuffer[soffset+l] ;
        end ;
        sas := sas + sbuffer[soffset+3] ;
        alphaBMP[ss +k] := sbuffer[soffset+3] ;
        offset := offset + 3 ;
        soffset := soffset + 4 ;
      end ;

      sbuffer := nil ;
      if sas <> 0 then
        isPartialTransparent := True ;
    end ;

    procedure convert15bits ;
    var
     i       : Integer ;
     w16     : Word ;
     r, g, b : Byte ;
    begin
      lp := bytes div 2 ;
      offset := _offset ;
      soffset := sbuffoff ;

      for i := 0 to lp - 1 do begin
          w16 := sbuffer[soffset] +(Word(sbuffer[soffset +1]) shl 8) ;

        b := Byte(((w16 and bBitMask) shr bShrVal)) shl bShlVal ;
        _buffer[offset] := b ;
        inc(offset) ;

        g := Byte(((w16 and gBitMask) shr gShrVal)) shl gShlVal ;
        _buffer[offset] := g ;
        inc(offset) ;

        r := Byte(((w16 and rBitMask) shr rShrVal)) shl rShlVal ;
        _buffer[offset] := r ;
        inc(offset) ;
        inc(soffset, 2) ;
      end ;
      sbuffer := nil ;
    end ;

  begin
    try
      if bmpBitHeight = 0 then begin
        Result := inherited getLine( _buffer, _offset, _linenr,
                                     _start, _bytes) ;
        exit ;
      end ;

      if isBitFields then begin
        bytes := (_bytes div 3)*4 ;
        SetLength( sbuffer, bytes ) ;

        sbuffoff := 0 ;
        start := (_start div 3)*4 ;
      end
      else
      if is15Bits then begin
        bytes := (_bytes div 3)*2 ;
        SetLength( sbuffer, bytes ) ;
        sbuffoff := 0 ;

        start := (_start div 3)*2 ;
      end
      else begin
        sbuffer := _buffer ;
        sbuffoff := _offset ;
        bytes := _bytes ;
        start := _start ;
      end ;

      if bmpBitHeight > 0 then
        offset := Int64(bitsOffset) + imageSize + start
                  - (_linenr +1 )*realLineWidth
      else
        offset := Int64(bitsOffset) +  start + (_linenr  )*realLineWidth ;

      {$IFDEF MANAGED}
        fileStream.Seek( offset, soBeginning ) ;
        Result := fileStream.Read( sbuffer, sbuffoff, bytes ) ;
      {$ELSE}
        fileStream.Seek( offset, soBeginning ) ;
        Result := fileStream.Read( sbuffer[0], bytes ) ;
      {$ENDIF}

      if isBitFields then
        del_fields
      else
      if is15Bits then
        convert15bits ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end ;

  function TGIS_LayerBMP.getAlphaLine(
    const _buffer : TBytes  ;
    const _offset : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer ;
  var
    i : Integer ;
  begin
    Result := _bytes ;
    if not assigned(alphaBMP) then  begin
      inherited getAlphaLine( _buffer, _offset, _linenr, _start, _bytes ) ;
    end
    else begin
      for i := 0 to _bytes - 1 do
        _buffer[_offset +i] := alphaBMP[_start +i] ;
    end ;
  end ;

  function TGIS_LayerBMP.getLineBits(
    const _buffer   : TBytes  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    line : Integer ;
    bytestart      : Integer ;
    bytestop       : Integer ;
    bytes          : Integer ;
    offset         : Integer ;
    i, outoff       : Integer ;
  begin
    line := _linenr ;
    if line >= baseCellHeight then
      line := baseCellHeight -1 ;

    bytestart  := ( _pixStart * bitsPerPixel) div 8 ;
    bytestop   := ((_pixStart + _pixCount -1) * bitsPerPixel) div 8 ;
    bytes := bytestop -bytestart +((bitsPerPixel +7) div 8);

    if assigned(oBitmap) then begin
      outoff := _offset ;
      offset := line*FBitWidth +_pixStart ;
      for i := 0 to _pixCount -1 do begin
         _buffer[outoff] := Byte(oBitmap[offset] and $FF) ;
         inc(outoff) ;
         _buffer[outoff] := Byte((oBitmap[offset] shr 8 )and $FF) ;
         inc(outoff) ;
         _buffer[outoff] := Byte((oBitmap[offset] shr 16) and $FF) ;
         inc(outoff) ;
         _buffer[outoff] := Byte((oBitmap[offset] shr 24)) ;
         inc(outoff) ;
         inc(offset) ;
      end;
      Result := bytes ;
      exit ;
    end ;

    if bmpBitHeight > 0 then
      offset := Int64(bitsOffset) + imageSize + bytestart
                - (line +1 )*realLineWidth
    else
      offset := Int64(bitsOffset) +  bytestart + (line  )*realLineWidth ;

    {$IFDEF MANAGED}
      fileStream.Seek( offset, soBeginning ) ;
      Result := fileStream.Read( _buffer, _offset, bytes ) ;
    {$ELSE}
      fileStream.Seek( offset, soBeginning ) ;
      Result := fileStream.Read( _buffer[_offset], bytes ) ;
    {$ENDIF}

  end;

  { Perform initialization section.
  }
  class procedure GisLayerBMP.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-BMP', 'Window Bitmap', TGIS_LayerBMP, '.bmp',
                   TGIS_RegisteredLayerType.Pixel,
                   TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read,
                     TGIS_RegisteredOperationType.Write,
                     TGIS_RegisteredOperationType.&Create
                   ],
                   True
                 ) ;
  end ;

{$IFDEF DCC}
  initialization
    GisLayerBMP.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

