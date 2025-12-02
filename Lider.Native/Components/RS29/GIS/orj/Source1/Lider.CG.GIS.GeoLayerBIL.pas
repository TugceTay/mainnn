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
  Encapsulation of a BIL (SPOT) Layer (.BIL extension).
  GTOPO30 global digital elevation model (.DEM extension)
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerBIL ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerBIL"'}
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
    Lider.CG.GIS.GeoResource,
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
  GisLayerBIL = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Encapsulation of BIL (SPOT) layer.
  /// </summary>
  TGIS_LayerBIL = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )

    private // various private values

        /// <summary>
        ///   Width of band in pixels.
        /// </summary>
        totalBitWidth  : Integer ;

        /// <summary>
        ///   Width of layer in bytes.
        /// </summary>
        totalLineWidth : Integer ;

        /// <summary>
        ///   Number of rows in the image.
        /// </summary>
        nrRows : Integer ;

        /// <summary>
        ///   Number of columns in the image.
        /// </summary>
        nrColumns : Integer ;

        /// <summary>
        ///   Number of bytes per row (twice the number of columns for a
        ///   16-bit).
        /// </summary>
        bandRowBytesNr : Integer ;

        /// <summary>
        ///   The number of bytes between bands.
        /// </summary>
        bandGaPBytesNr : Integer ;

        /// <summary>
        ///   Total number of bytes of data per row.
        /// </summary>
        totalRowBytesNr : Integer ;

        /// <summary>
        ///   Byte order from most significant to least significant.
        /// </summary>
        bigEndian : Boolean ;

        /// <summary>
        ///   One pixel is represented by 16 bits.
        /// </summary>
        sixteenBits : Boolean ;

        /// <summary>
        ///   One pixel is represented by 32 bits.
        /// </summary>
        thirtyTwoBits : Boolean ;

        /// <summary>
        ///   Bits count to internal use.
        /// </summary>
        bitsCountInfo : Integer ;

        /// <summary>
        ///
        /// </summary>
        buffByte     : Array of Byte ;

        /// <summary>
        ///
        /// </summary>
        buffSmallint : Array of SmallInt ;

        /// <summary>
        ///   line buffer (line of singles).
        /// </summary>
        lineBuffer : array of Single ;

        /// <summary>
        ///   Number of lines present in lineBuffer.
        /// </summary>
        lineInBuffer  : Integer ;

        /// <summary>
        ///   Contrast balance.
        /// </summary>
        redCB   : Array of Byte ;

        /// <summary>
        ///   Contrast balance.
        /// </summary>
        greenCB : Array of Byte ;

        /// <summary>
        ///   Contrast balance.
        /// </summary>
        blueCB  : Array of Byte ;
    private // various private values

      /// <summary>
      ///   Reads header file.
      /// </summary>
      function  readHdr           : Boolean ;

      /// <summary>
      ///   Makes one 24-bits per pixel image line from separated red, green
      ///   and blue bands
      /// </summary>
      /// <param name="_dest">
      ///   pointer to full color line
      /// </param>
      /// <param name="_destx">
      ///   offset in buffer
      /// </param>
      /// <param name="_src">
      ///   pointer to red band (after are the green and blue bands)
      /// </param>
      /// <param name="_width">
      ///   band width
      /// </param>      
      procedure makeFullColor   ( const _dest  : TBytes  ;
                                  const _destx : Integer ;
                                  const _src   : TBytes  ;
                                  const _width : Integer
                                ) ;

    // various protected routine
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <inheritdoc/>
      procedure setUp          ; override;

      /// <inheritdoc/>
      function  getLine      ( const _buffer : TBytes  ;
                               const _offset : Integer ;
                               const _linenr : Integer ;
                               const _start  : Integer ;
                               const _bytes  : Integer
                             ) : Integer; override;

      /// <inheritdoc/>
      function  getNativeValue ( const _pt     : TPoint  ;
                                 const _ar     : TGIS_DoubleArray
                               ) : Boolean ; override;

      /// <inheritdoc/>
      function  getNativeLine  ( const _buffer   : TGIS_SingleArray ;
                                 const _linenr   : Integer          ;
                                 const _startIdx : Integer          ;
                                 const _count    : Integer
                               ) : Integer ; override;

      /// <summary>
      ///   Convert from big endian to lw endian format.
      /// </summary>
      procedure convertLineToLE  ;

      /// <inheritdoc/>
      procedure prepareMinMaxZ ( const _zoom : Double = -1
                               ) ; override ;

      /// <summary>
      ///   Build CB tables
      /// </summary>
      procedure prepareCBTables  ;

      /// <inheritdoc/>
      procedure setupParams ; override;

    public // various public routines

      /// <inheritdoc/>
      constructor Create  ; override;

      /// <inheritdoc/>
      function  PreRecognize     ( const _path      : String ;
                                     var _new_path  : String
                                  ) : Boolean ; override;

  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoRegistredLayers, 
    Lider.CG.GIS.GeoStreams ;
{$ENDIF}

const

    // Extension of world file (extension file for pixel layer).
    WORLD_FILE_EXT_BIL = '.blw' ;

    // Extension of world file (extension file for pixel layer).
    // Alternative naming.
    WORLD_FILE_EXT2_BIL = '.bilw' ;

    // Extension of world file (extension file for pixel layer).
    WORLD_FILE_EXT_DEM = '.dmw' ;

    // Extension of world file (extension file for pixel layer).
    // Alternative naming.
    WORLD_FILE_EXT2_DEM = '.demw' ;

  // BIL depended names
     BIL_BYTEORDER     =  'BYTEORDER'     ;
     BIL_LAYOUT        =  'LAYOUT'        ;
     BIL_NROWS         =  'NROWS'         ;
     BIL_NCOLS         =  'NCOLS'         ;
     BIL_ULXMAP        =  'ULXMAP'        ;
     BIL_ULYMAP        =  'ULYMAP'        ;
     BIL_XDIM          =  'XDIM'          ;
     BIL_YDIM          =  'YDIM'          ;
     BIL_MAPUNITS      =  'MAPUNITS'      ;
     BIL_NBANDS        =  'NBANDS'        ;
     BIL_NBITS         =  'NBITS'         ;
     BIL_BANDROWBYTES  =  'BANDROWBYTES'  ;
     BIL_TOTALROWBYTES =  'TOTALROWBYTES' ;
     BIL_BANDGAPByteS  =  'BANDGAPByteS'  ;
     BIL_NODATAVALUE   =  'NODATA'        ;

{$IFNDEF OXYGENE}
  type
    PDWord = ^DWord ;
{$ENDIF}

//==============================================================================
// TGIS_LayerBIL
//==============================================================================

  constructor TGIS_LayerBIL.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
    Params.Pixel.GridNoValue   := -9999 ;
    FNoDataValue := Params.Pixel.GridNoValue  ;
  end ;

  function TGIS_LayerBIL.readHdr : Boolean ;
  var
    ext           : TGIS_Extent ;
    txt           : array [0..1] of String ;
    ffile         : TGIS_BufferedFileStream ;
    lineStrBuffer : String ;
    xluCorner     : Double ;
    yluCorner     : Double ;

    procedure read_param ;
    var
      idx, tidx : Integer ;
      idxmax : Integer ;
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
                if (tidx = 0) AND ( not IsStringEmpty( txt[0] ) ) then
                  tidx := 1
                else if (tidx = 1) AND ( not IsStringEmpty( txt[1] ) ) then
                  exit ;
                continue ;
              end ;
            else  txt[tidx] := txt[tidx] + UpCase(lineStrBuffer[idx]) ;
          end ;
        end ;
      end ;
    end ;

  begin
    Result    := False ;
    scaleX    := 0 ;
    scaleY    := 0 ;
    xluCorner := 0 ;
    yluCorner := 0 ;

    {$IFDEF GIS_NORECORDS} 
    ext := new TGIS_Extent() ; 
    {$ENDIF} 
    if not SafeFileExists(GetPathNoExt(Path) + '.hdr') then exit ;

    // read a header file
    ffile := TGIS_BufferedFileStream.Create( GetPathNoExt(Path) + '.hdr', TGIS_StreamMode.Read ) ;
    try
      repeat
        read_param ;
        if IsStringEmpty( txt[0] ) then break ;

        if      txt[0] = BIL_BYTEORDER   then begin
                                           if txt[1] = 'M' then bigEndian := True
                                                           else bigEndian := False ;
                                         end
        else if txt[0] = BIL_LAYOUT      then begin
                                           if txt[1] <> 'BIL' then exit ;
                                         end
        else if txt[0] = BIL_NROWS       then
                                           nrRows     :=  StrToInt(txt[1])
        else if txt[0] = BIL_NCOLS       then
                                           nrColumns  :=  StrToInt(txt[1])

        else if txt[0] = BIL_ULXMAP      then
                                           xluCorner  :=  DotStrToFloat(txt[1])
        else if txt[0] = BIL_ULYMAP      then
                                           yluCorner  :=  DotStrToFloat(txt[1])
        else if txt[0] = BIL_XDIM        then
                                           scaleX     :=  DotStrToFloat(txt[1])
        else if txt[0] = BIL_YDIM        then
                                           scaleY     := -DotStrToFloat(txt[1])
        else if txt[0] = BIL_NODATAVALUE then begin
                                           Params.Pixel.GridNoValue := DotStrToFloat(txt[1]) ;
                                           FNoDataValue := Params.Pixel.GridNoValue  ;
                                         end
        else if txt[0] = BIL_NBANDS      then
                                           FBandsCount     :=  StrToInt(txt[1])
        else if txt[0] = BIL_NBITS       then
                                           realBitCount    :=  StrToInt(txt[1])
        //set but not read
        else if txt[0] = BIL_BANDROWBYTES then
                                         bandRowBytesNr  :=  StrToInt(txt[1])
        else if txt[0] = BIL_BANDGAPByteS then
                                         bandGaPBytesNr  :=  StrToInt(txt[1])
        else if txt[0] = BIL_TOTALROWBYTES then
                                           totalRowBytesNr :=  StrToInt(txt[1]) ;
      until IsStringEmpty( txt[0] ) ;
    finally
      FreeObject( ffile ) ;
    end;

    if realBitCount = 16 then begin
      realBitCount := 8 ;
      sixteenBits  := True ;
    end else
    if realBitCount = 32 then begin
      realBitCount := 8 ;
      thirtyTwoBits := True ;
    end;

    ext.XMin := xluCorner ;
    ext.XMax := xluCorner ;

    ext.YMin := yluCorner ;
    ext.YMax := yluCorner ;

    if totalRowBytesNr = 0 then
      totalRowBytesNr := ((nrColumns*realBitCount      +7) div 8)*FBandsCount ;

    FExtent := ext;
    Result := True ;
  end ;

  procedure TGIS_LayerBIL.makeFullColor ( const _dest  : TBytes  ;
                                          const _destx : Integer ;
                                          const _src   : TBytes  ;
                                          const _width : Integer
                                        ) ;
  var
    i, width2, t : Integer ;
  begin

    width2 := 2*_width ;
    for i := 0 to _width - 1 do begin
      // red byte
      t := _src[i] ;
      _dest[_destx+3*i] :=  redCB[t] ;
      // green byte
      t := _src[i+_width] ;
      _dest[_destx+3*i + 1] := greenCB[t] ;
      // blue byte
      t := _src[i +width2] ;
      _dest[_destx+3*i+2] := blueCB[t] ;
    end;

  end ;

  procedure TGIS_LayerBIL.setUp ;
  var
    t       : Integer;
    fext    : String ;
  begin
    try
      if readHdr then begin
        // read bitmap parameters
           fileStream := openBufferedFileStream( Path ) ;

        // get bitmap size into easy to access properties
           FBitWidth      := nrColumns ;
           totalBitWidth  := totalRowBytesNr ;
           FBitHeight     := nrRows ;

           realLineWidth      := (FBitWidth*realBitCount      +7) div 8 ;
           totalLineWidth := (totalBitWidth*realBitCount +7) div 8 ;

           if FBandsCount >= 3 then
             realBitCount := 24
           else
           begin
             realBitCount := 8 ;
             colorsNo := 255 ;
           end;

        // set the palette
           for t := 0 to 255 do begin
             bitPalette[t] := TGIS_Color.FromARGB(255, t, t, t) ;
           end ;

      end ;

      if (realBitCount = 1) OR (realBitCount = 4)
          OR (realBitCount = 8) then begin
        intLineWidth := (((FBitWidth +1) * 3) div 4)*4 ;
        colorsNo := 0 ;
      end else
      begin
         intLineWidth := realLineWidth ;
      end ;

      // read a word file

      fext := UpperCase( GetFileExt(Path) ) ;
      if fext = '.BIL' then begin
        if SafeFileExists( GetPathNoExt( Path ) + WORLD_FILE_EXT_BIL ) then
          setWorldFile( WORLD_FILE_EXT_BIL )
        else if SafeFileExists( GetPathNoExt( Path ) + WORLD_FILE_EXT2_BIL ) then
          setWorldFile( WORLD_FILE_EXT2_BIL )
      end
      else
      if fext = '.DEM' then begin
        if SafeFileExists( GetPathNoExt( Path ) + WORLD_FILE_EXT_DEM ) then
          setWorldFile( WORLD_FILE_EXT_DEM )
        else if SafeFileExists( GetPathNoExt( Path ) + WORLD_FILE_EXT2_DEM ) then
          setWorldFile( WORLD_FILE_EXT2_DEM )
      end ;

      if sixteenBits then
        bitsCountInfo := 16
      else
      if thirtyTwoBits then
        bitsCountInfo := 32
      else
        bitsCountInfo := realBitCount ;

      if FBandsCount = 1 then begin
        FIsGridImage        := True ;
        FIsNativeGridImage  := True ;
        FAntialias := True ;

        SetLength( lineBuffer, FBitWidth ) ;

        lineInBuffer := -1 ;
        // Transparency
        redTransp[0]    := BASE_TRANSPARENT_FLAG ;
        greenTransp[0]  := BASE_TRANSPARENT_FLAG ;
        blueTransp[0]   := BASE_TRANSPARENT_FLAG ;

        case bitsCountInfo of
          8  :
            begin
              SetLength( buffByte, FBitWidth ) ;
            end ;
          16 :
            begin
              SetLength( buffSmallint, FBitWidth ) ;
            end ;
        end ;

        if FMaxZ <= FMinZ then begin
          FMaxZ   := -GIS_MAX_SINGLE  ;
          FMinZ   :=  GIS_MAX_SINGLE  ;
          prepareMinMaxZ ;
        end;

        Extent3D := GisExtent3D( Extent.XMin, Extent.YMin, FMinZ,
                                 Extent.XMax, Extent.YMax, FMaxZ
                                ) ;

      end ;

      inherited ;

      if FBandsCount >= 3 then begin
        SetLength(redCB, 256) ;
        SetLength(greenCB, 256) ;
        SetLength(blueCB, 256) ;
        for t := 0 to 255 do begin
          redCB[t] := Byte(t) ;
          greenCB[t] := Byte(t) ;
          blueCB[t] := Byte(t) ;
        end ;
        if (FBandsCount > 3) and (not FIsGridImage) then
          prepareCBTables ;
      end ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo :=
        Format( 'Band Interleaved by Line Format - (BIL/SPOT)' +#13#10 +
                '%d x %d; %d bit; %d band(s)',
                [ FBitWidth, FBitHeight, bitsCountInfo, FBandsCount ]
              ) ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;

  end;

  function TGIS_LayerBIL.getLine( const _buffer : TBytes  ;
                                  const _offset : Integer ;
                                  const _linenr : Integer ;
                                  const _start  : Integer ;
                                  const _bytes  : Integer
                                ) : Integer ;
  var
    offset  : Integer ;
    bytes3  : Integer ;
    locbuf  : TBytes ;
    idx     : Integer ;
    ival    : Integer ;
    wval    : Word ;
  begin
    Result := 0 ;
    try
      if FBandsCount >= 3 then begin
       SetLength( locbuf, _bytes ) ;
       offset := bandsMap[0]*realLineWidth +
                _linenr*totalLineWidth  +  _start div 3 ;
       bytes3 := _bytes div 3 ;

       fileStream.Seek( offset, soBeginning );
       Result := fileStream.Read( locbuf, bytes3 ) ;

       offset := bandsMap[1]*realLineWidth +
                _linenr*totalLineWidth  +  _start div 3 ;
       fileStream.Seek( offset, soBeginning ) ;
       {$IFDEF OXYGENE}
         Result := Result + fileStream.Read( locbuf,bytes3,
                                             bytes3
                                           ) ;
       {$ELSE}
         Result := Result + fileStream.Read( locbuf[bytes3],
                                             bytes3
                                           ) ;
       {$ENDIF}
       offset := bandsMap[2]*realLineWidth +
                _linenr*totalLineWidth  +  _start div 3 ;
       fileStream.Seek( offset, soBeginning ) ;
       {$IFDEF OXYGENE}
         Result := Result + fileStream.Read( locbuf,2*bytes3,
                                             bytes3
                                           ) ;
       {$ELSE}
         Result := Result + fileStream.Read( locbuf[2*bytes3],
                                             bytes3
                                           ) ;
       {$ENDIF}

       makeFullColor( _buffer, _offset, locbuf, bytes3 ) ;

       SetLength( locbuf, 0 ) ;
     end
     else begin
       if not (sixteenBits or thirtyTwoBits) then begin
         offset := _linenr*totalLineWidth + _start ;
         fileStream.Seek( offset, soBeginning ) ;
         {$IFDEF OXYGENE}
           Result := fileStream.Read( _buffer,_offset, _bytes ) ;
         {$ELSE}
           Result := fileStream.Read( _buffer[_offset], _bytes ) ;
         {$ENDIF}
       end else
       if sixteenBits then begin
          SetLength( locbuf, 2*_bytes ) ;

         offset := _linenr*totalLineWidth + 2*_start ;
         fileStream.Seek( offset, soBeginning ) ;
         Result := fileStream.Read( locbuf, 2*_bytes ) div 2;

         for idx := 0 to _bytes -1 do begin
           {$IFDEF OXYGENE}
             wval := SwapWord( locbuf, 2*idx ) ;
           {$ELSE}
             wval := Swap(PWORD(NativeInt(locbuf) +2*idx)^) ;
           {$ENDIF}

           ival := Integer(2*wval -32728) ;
           ival := (ival div 256) +128 ;
           wval := Word(ival) ;
           _buffer[_offset+idx] := Byte(wval) ;
         end ;
          SetLength( locbuf, 0 ) ;
       end else
       if thirtyTwoBits then begin
         offset := _linenr*totalLineWidth + 4*_start ;
         fileStream.Seek( offset, soBeginning ) ;
         {$IFDEF OXYGENE}
           Result := fileStream.Read( _buffer,0, 4*_bytes ) div 4;
         {$ELSE}
           Result := fileStream.Read( _buffer[0], 4*_bytes ) div 4;
         {$ENDIF}
       end ;
     end;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;
  end;

  procedure TGIS_LayerBIL.convertLineToLE  ;
  var
    i : Integer ;
    {$IFNDEF OXYGENE}
      dwval : DWord ;
    {$ENDIF}
  begin
    for i := 0 to FBitWidth -1 do begin
      {$IFDEF OXYGENE}
        lineBuffer[i] := SwapSingle( lineBuffer[i] ) ;
      {$ELSE}
        dwval := SwapLongInt( PDWord( Addr( lineBuffer[i] ) )^ ) ;
        lineBuffer[i] := PSingle( Addr(dwval) )^ ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_LayerBIL.prepareMinMaxZ( const _zoom : Double = -1 ) ;
  var
    i, k : Integer ;
    {$IFDEF OXYGENE}
      l : Integer ;
    {$ENDIF}
    zoom : Double ;
    bytes : Int64 ;
    offset : Int64 ;
    lines : Double ;
    fact : Double ;
  const
    MAX_LINES = 900 ;
  begin
    Alive ;
    case bitsCountInfo of
      8  :
        begin
          bytes := FBitWidth * sizeOf(Byte) ;
        end ;
      16 :
        begin
          bytes := FBitWidth * sizeOf(SmallInt) ;
        end ;
      else // 24
        begin
          bytes := FBitWidth * sizeOf(Single) ;
        end ;
    end ;


    if (_zoom > 0) AND (_zoom <= 1) then begin
      lines := RoundS(FBitHeight * _zoom) ;
      if lines = 0 then
        lines := 1 ;
    end
    else
    begin
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
        offset := bytes * i ;
        fileStream.Seek( offset, soBeginning ) ;
        case bitsCountInfo of
          8  :
            begin
              {$IFDEF OXYGENE}
                fileStream.Read( buffByte, 0, bytes ) ;
              {$ELSE}
                fileStream.Read( buffByte[0], bytes ) ;
              {$ENDIF}
              for k := 0 to FBitWidth -1 do
                lineBuffer[k] := buffByte[k] ;
            end ;
          16 :
            begin
              {$IFDEF OXYGENE}
                for l := 0 to FBitWidth-1 do
                  fileStream.ReadSmallInt( buffSmallint[l], 2 ) ;
              {$ELSE}
                fileStream.Read( buffSmallint[0], bytes ) ;
              {$ENDIF}
              if bigEndian then begin
                {$IFDEF OXYGENE}
                  for k := 0 to FBitWidth -1 do
                    lineBuffer[k] := SwapSmallint( buffSmallint[k] ) ;
                {$ELSE}
                  for k := 0 to FBitWidth -1 do
                    lineBuffer[k] := Swap( buffSmallint[k] ) ;
                {$ENDIF}
              end
              else
                for k := 0 to FBitWidth -1 do
                  lineBuffer[k] := buffSmallint[k] ;
            end ;
          else begin
            {$IFDEF OXYGENE}
              for l := 0 to FBitWidth-1 do
                fileStream.ReadSingle( lineBuffer[l], 4 ) ;
            {$ELSE}
              fileStream.Read( lineBuffer[0], bytes ) ;
            {$ENDIF}
            if bigEndian then
              convertLineToLE ;
          end ;
        end ;
        for k := 0 to FBitWidth -1 do begin
          if lineBuffer[k] <> FNoDataValue then
          begin
            if lineBuffer[k] < FMinZ then
              FMinZ := lineBuffer[k] ;
            if lineBuffer[k] > FMaxZ then
              FMaxZ := lineBuffer[k] ;
          end ;
        end ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerBIL.prepareCBTables ;
  var
    i, t : Integer ;
  begin

    for i := 0 to 255 do begin
      // red byte
        t := ( i * 110 ) div 21 ;
        if t < 255 then
          redCB[i] := t
        else
          redCB[i]:= 255 ;

      // green byte
        t := ( i * 110 ) div 18 ;
        if t < 255 then
          greenCB[i] := t
        else
          greenCB[i]:= 255 ;

      // blue byte
        t := ( i * 110 ) div 46 ;
        if t < 255 then
          blueCB[i] := t
        else
          blueCB[i]:= 255 ;
    end;
  end ;

  procedure TGIS_LayerBIL.setupParams ;
  begin
  //Bands managing
    if not FIsGridImage then begin
      if Params.Pixel.GridBand > 0 then
        FIsGridImage         := True ;
    end ;

    if not FIsGridImage  then begin
      if Params.Pixel.RedBand = 0 then begin
        Params.Pixel.RedBand := 3 ;
      end ;

      if Params.Pixel.GreenBand = 0 then begin
        Params.Pixel.GreenBand := 2 ;
      end ;

      if Params.Pixel.BlueBand = 0 then begin
        Params.Pixel.BlueBand := 1 ;
      end ;
    end ;

    inherited ;

    if FIsGridImage then begin
      bandsMap[0] := FGridBand -1 ;
      bandsMap[1] := bandsMap[0] ;
      bandsMap[2] := bandsMap[0] ;
    end ;

  end;


  function  TGIS_LayerBIL.getNativeValue( const _pt : TPoint  ;
                                          const _ar : TGIS_DoubleArray
                                         ) : Boolean ;

  var
    start  : Integer ;
    offset : Integer ;
    datasize : Word ;
    sval   : Single ;
    bval   : Byte ;
    ival   : SmallInt ;
    {$IFNDEF OXYGENE}
      dwval  : DWord ;
    {$ENDIF}
  begin
    try
      Result := True ;

      case bitsCountInfo of
        8  :  datasize := 1 ;
        16 :  datasize := 2 ;
        else // 24
              datasize := 4 ;
      end ;

      start := _pt.X*datasize ;

      offset := (_pt.Y*FBitWidth*datasize) +start ;

      fileStream.Seek( offset, soBeginning ) ;

      {$IFDEF OXYGENE}
        case bitsCountInfo of
          8  :  fileStream.ReadByte( bval, 1 ) ;
          16 :  fileStream.ReadSmallInt( ival, 2 ) ;
          else // 24
                fileStream.ReadSingle( sval, 4 ) ;
        end ;
      {$ELSE}
        case bitsCountInfo of
          8  :  fileStream.Read( (Addr(bval))^, 1);
          16 :  fileStream.Read( (Addr(ival))^, 2);
          else // 24
                fileStream.Read( (Addr(sval))^, sizeOf(Single));
        end ;
      {$ENDIF}

      if bigEndian then begin
        {$IFDEF OXYGENE}
          case bitsCountInfo of
            8  :  _ar[0] := bval ;
            16 :  _ar[0] := SwapSmallint(ival) ;
            else // 24
              _ar[0] := SwapSingle( sval ) ;
          end ;
        {$ELSE}
          case bitsCountInfo of
            8  :  _ar[0] := bval ;
            16 :  _ar[0] := Swap(ival) ;
            else // 24
                begin
                  dwval := SwapLongInt(PDWord(Addr(sval))^) ;
                  _ar[0] := PSingle(Addr(dwval))^ ;
                end ;
          end ;
        {$ENDIF}
      end else begin
        case bitsCountInfo of
          8  :  _ar[0] := bval ;
          16 :  _ar[0] := ival ;
          else // 24
                _ar[0] := sval ;
        end ;

      end ;

      if _ar[0] = FNoDataValue then
        Result := False ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerBIL.getNativeLine( const  _buffer   : TGIS_SingleArray ;
                                         const _linenr   : Integer          ;
                                         const _startIdx : Integer          ;
                                         const _count    : Integer
                                       ) : Integer ;
  var
    {$IFDEF OXYGENE}
      j    : Integer ;
    {$ENDIF}
    k      : Integer ;
    line   : Int64 ;
    offset : Int64 ;
    bytes  : Integer ;
  begin
    if not FIsNativeGridImage then begin
      Result := inherited  getNativeLine( _buffer, _linenr, _startIdx, _count ) ;
      exit ;
    end ;

    Result := 0 ;
    try

      line   := _linenr ;

      if lineInBuffer <> line then begin

        case bitsCountInfo of
          8  :  bytes := FBitWidth * sizeOf(Byte) ;
          16 :  bytes := FBitWidth * sizeOf(SmallInt) ;
          else // 24
                bytes := FBitWidth * sizeOf(Single) ;
        end ;

        offset := (line*bytes) ;
        fileStream.Seek( offset, soBeginning ) ;
        case bitsCountInfo of
          8  :
            begin
              {$IFDEF OXYGENE}
                fileStream.Read( buffByte, 0, bytes ) ;
              {$ELSE}
                fileStream.Read( buffByte[0], bytes ) ;
              {$ENDIF}
              for k := 0 to FBitWidth -1 do
                lineBuffer[k] := buffByte[k] ;
            end ;
          16 :
            begin
              {$IFDEF OXYGENE}
                for j := 0 to FBitWidth-1 do
                  fileStream.ReadSmallInt( buffSmallint[j], 2 ) ;
              {$ELSE}
                fileStream.Read( buffSmallint[0], bytes ) ;
              {$ENDIF}
              if bigEndian then begin
                {$IFDEF OXYGENE}
                  for k := 0 to FBitWidth -1 do
                    lineBuffer[k] := SwapSmallint( buffSmallint[k] ) ;
                {$ELSE}
                  for k := 0 to FBitWidth -1 do
                    lineBuffer[k] := Swap( buffSmallint[k] ) ;
                {$ENDIF}
              end
              else
                for k := 0 to FBitWidth -1 do
                  lineBuffer[k] := buffSmallint[k] ;
            end ;
          else begin
            {$IFDEF OXYGENE}
              for j := 0 to FBitWidth-1 do
                fileStream.ReadSingle( lineBuffer[j], 4 ) ;
            {$ELSE}
              fileStream.Read( lineBuffer[0], bytes ) ;
            {$ENDIF}
            if bigEndian then
              convertLineToLE ;
          end ;
        end ;
        lineInBuffer := line ;
      end ;
      for k := 0 to _count -1 do
        _buffer[k] := lineBuffer[k +_startIdx] ;
      if _count > 0 then
        Result := _count ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0 ) ;
    end ;
  end ;

  function TGIS_LayerBIL.PreRecognize( const _path     : String ;
                                         var _new_path : String
                                     ) : Boolean ;
  begin
    Result := inherited PreRecognize( _path, _new_path );

    if not SafeFileExists( GetPathNoExt( _path ) + '.hdr' ) then
      Result := False ;
  end;

  { Perform initialization section.
  }
  class procedure GisLayerBIL.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-BIL', 'SPOT Binary In-line Format', TGIS_LayerBIL, '.bil;.dem',
                   TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                    True
                  );
  end ;

{$IFNDEF OXYGENE}
  initialization
    GisLayerBIL.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

