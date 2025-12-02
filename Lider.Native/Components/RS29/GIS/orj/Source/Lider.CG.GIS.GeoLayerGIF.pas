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
  Encapsulation of a GIF (Graphics Interchange Format) Layer.
}

{$IFDEF DCC}
  unit GisLayerGIF ;
  {$HPPEMIT '#pragma link "GisLayerGIF"'}
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
    System.Drawing,
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Types,

    GisRtl,
    GisTypes,
    GisTypesUI,
    GisClasses,
    GisLayer,
    GisLayerPixel,
    GisStreams;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerGIF = class
    public
      class procedure SelfRegisterLayer() ;
  end ;


  /// <summary>
  ///   Encapsulation of GIF layer.
  /// </summary>
  TGIS_LayerGIF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerPixel )
    private

      /// <summary>
      ///   Underlying bitmap which handles all images.
      /// </summary>
      bitmapObj : TBytes ;

      /// <summary>
      ///   Image is saved as interlaced.
      /// </summary>
      interleaced : Boolean ;

      /// <summary>
      ///   Data starting position.
      /// </summary>
      dataOffset : Int64 ;
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF} // various protected routines

      /// <inheritdoc/>
      procedure setUp    ; override;

      /// <summary>
      ///   Internal use only. Image LZW decompressing.
      /// </summary>
      /// <param name="_interlaced">
      ///   set when image is interlaced.
      /// </param>
      procedure decompress(const _interlaced : Boolean ) ;
      
      /// <inheritdoc/>
      function  getLinePixels     ( const _buffer   : TGIS_Pixels  ;
                                    const _offset   : Integer ;
                                    const _linenr   : Integer ;
                                    const _pixStart : Integer ;
                                    const _pixCount : Integer
                                  ) : Integer; override;

    public // various public routines

      /// <inheritdoc/>
      procedure Alive             ; override;

      /// <inheritdoc/>
      function  DormantGain       : Integer ; override;
      /// <inheritdoc/>
      procedure Dormant           ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public // various public routines

      /// <inheritdoc/>
      constructor Create  ; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisFunctions,
    GisInternals,
    GisResource,
    GisRegistredLayers ;
{$ENDIF}

const

    // Extension of world file (extension file for pixel layer).
    WORLD_FILE_EXT_GIF = '.gfw' ;

    // Extension of world file (extension file for pixel layer).
    // Alternative naming.
    WORLD_FILE_EXT2_GIF = '.gifw' ;

  { Block identification }
    BEGIN_OF_DATA = $2C ;
    END_OF_DATA   = $3B ;
  { Flags testing masks}
    COLOR_PALETTE_MASK  = $80 ;
    COLOR_TABLE_SIZE_MASK = $07 ;
    INTERLACED_MASK = $40 ;
    TRANSPARENCY_MASK = $01 ;

type

  T_DecodeState = {$IFDEF GIS_PACKED} packed {$ENDIF} 
                  {$IFDEF JAVA} class {$ELSE} record {$ENDIF}
      dataLen       : Integer ;
      readPos       : Integer ;
      posY          : Integer ;
      bitsLeft      : Integer ;
      currByte      : Integer ;
      ilPass        : Integer ;
      {$IFDEF OXYGENE}
        &step       : Integer ;
      {$ELSE}
        step        : Integer ;
      {$ENDIF}
      codeSize      : Byte ;
      currCodeSize  : Integer ;
      clearCode     : Integer;
      endingCode    : Integer;
      highCode      : Word;
  end ;

  { GIF identification.
  }
  T_GIFHeader = packed record
    signature : TBytes ;
    version   : TBytes ;
  end ;

  T_LogicalScreenDescriptor = packed record
    width     : Word ;
    height    : Word ;
    {$IFDEF OXYGENE}
      &flags  : Byte ;
    {$ELSE}
      flags   : Byte ;
    {$ENDIF}
    gbcolor   : Byte ;
    asp_ratio : Byte ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  T_ImageInfo = packed record
    left   : Word ;
    top    : Word ;
    width  : Word ;
    height : Word ;
    {$IFDEF OXYGENE}
      &flags  : Byte ;
    {$ELSE}
      flags   : Byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

  T_PictureModifier = packed record
    {$IFDEF OXYGENE}
      &flags  : Byte ;
    {$ELSE}
      flags   : Byte ;
    {$ENDIF}
    pause      : Word ;
    transpIdx  : Byte ;
    terminator : Byte ;
    {$IFDEF OXYGENE}
      function Read ( _stream : TGIS_BaseStream ) : Integer ;
    {$ENDIF}
  end ;

{$IFDEF OXYGENE}
//==============================================================================
// T_LogicalScreenDescriptor
//==============================================================================

  function T_LogicalScreenDescriptor.Read ( _stream : TGIS_BaseStream ) : Integer ;
  begin
    Result := _stream.ReadWord ( width,     2 ) +
              _stream.ReadWord ( height,    2 ) +
              _stream.ReadByte ( &flags,    1 ) +
              _stream.ReadByte ( gbcolor,   1 ) +
              _stream.ReadByte ( asp_ratio, 1 ) ;
  end ;

//==============================================================================
// T_PictureModifier
//==============================================================================

  function T_PictureModifier.Read ( _stream : TGIS_BaseStream ) : Integer ;
  begin
    Result := _stream.ReadByte ( &flags,     1 ) +
              _stream.ReadWord ( pause,      2 ) +
              _stream.ReadByte ( transpIdx,  1 ) +
              _stream.ReadByte ( terminator, 1 ) ;
  end ;

//==============================================================================
// T_ImageInfo
//==============================================================================

  function T_ImageInfo.Read ( _stream : TGIS_BaseStream ) : Integer ;
  begin
    Result := _stream.ReadWord ( left,   2 ) +
              _stream.ReadWord ( top,    2 ) +
              _stream.ReadWord ( width,  2 ) +
              _stream.ReadWord ( height, 2 ) +
              _stream.ReadByte ( &flags, 1 ) ;
  end ;

{$ENDIF}

//==============================================================================
// TGIS_LayerGIF
//==============================================================================

  constructor TGIS_LayerGIF.Create ;
  begin
    inherited ;
    FSubType := FSubType + [TGIS_LayerSubType.Persistent] ;
  end ;

  procedure TGIS_LayerGIF.doDestroy ;
  begin
    bitmapObj := nil ;

    inherited ;
  end ;

  procedure TGIS_LayerGIF.setUp ;
  var
    header    : T_GIFHeader ;
    lsd       : T_LogicalScreenDescriptor ;
    img_info  : T_ImageInfo ;
    colortablesize : Integer ;
    red       : Byte  ;
    green     : Byte  ;
    blue      : Byte  ;
    infobyte  : Byte ;
    infoword  : Word ;
    signature : AnsiString ;
    version   : AnsiString ;
    i         : Integer ;
    modif     : T_PictureModifier ;
  begin
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

      try
        SetLength( header.signature, 3 ) ;
        SetLength( header.version, 3 ) ;
        {$IFDEF OXYGENE}
          fileStream.Read( header.signature, 3 ) ;
          fileStream.Read( header.version, 3 ) ;
        {$ELSE}
          fileStream.Read( header.signature[0], 3 ) ;
          fileStream.Read( header.version[0], 3 ) ;
        {$ENDIF}

        signature := ConvertAnsiString( header.signature, 3 ) ;
        version   := ConvertAnsiString( header.version, 3 ) ;

        if (signature <> 'GIF') or
           ((version <> '87a') and (version <> '89a')) then
          raise EGIS_Exception.Create(
                 'This is not valid Graphics Interchange Format', Path, 0) ;

        {$IFDEF OXYGENE}
          lsd.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read( lsd, sizeOf(T_LogicalScreenDescriptor) ) ;
        {$ENDIF}

        if (lsd.flags and COLOR_PALETTE_MASK) <> 0 then begin
          colortablesize := 2 shl ( Integer(lsd.flags) and COLOR_TABLE_SIZE_MASK ) ;
          for i := 0 to colortablesize -1 do begin
            fileStream.ReadByte( red,   1) ;
            fileStream.ReadByte( green, 1) ;
            fileStream.ReadByte( blue,  1) ;
            bitPalette[i] := TGIS_Color.FromRGB(red, green, blue) ;
          end ;
        end ;

        repeat
          fileStream.ReadByte( infobyte ) ;

          if infobyte = $21 then begin
            fileStream.ReadWord( infoword ) ;
            if infoword = $04F9 then begin
              // Picture Modifier
              {$IFDEF OXYGENE}
                modif.Read( fileStream ) ;
              {$ELSE}
                fileStream.Read( modif, sizeOf (T_PictureModifier) ) ;
              {$ENDIF}
              if (modif.flags and TRANSPARENCY_MASK) <> 0 then begin
                internalTransparentColor := TGIS_Color.FromARGB(
                                             0,
                                             bitPalette[modif.transpIdx].R,
                                             bitPalette[modif.transpIdx].G,
                                             bitPalette[modif.transpIdx].B
                                            ) ;
              end ;
            end ;
          end ;
        until (infobyte = $2c) ;

        {$IFDEF OXYGENE}
          img_info.Read( fileStream ) ;
        {$ELSE}
          fileStream.Read( img_info, sizeOf (T_ImageInfo) ) ;
        {$ENDIF}

        if (img_info.flags and COLOR_PALETTE_MASK) <> 0 then begin
          colortablesize := 2 shl ( Integer(lsd.flags) and COLOR_TABLE_SIZE_MASK ) ;
          for i := 0 to colortablesize -1 do begin
            fileStream.ReadByte( red,   1) ;
            fileStream.ReadByte( green, 1) ;
            fileStream.ReadByte( blue,  1) ;
            bitPalette[i] := TGIS_Color.FromRGB(red, green, blue) ;
          end ;
        end ;

        FBitWidth := img_info.width ;
        FBitHeight := img_info.height ;
        realBitCount := 8 ;
        realLineWidth := FBitWidth ;
        if (img_info.flags and INTERLACED_MASK) <> 0 then
          interleaced := True
        else
          interleaced := False ;


        if realBitCount = 8 then begin
          intLineWidth := FBitWidth * 3 ;
          while (intLineWidth mod 4) <> 0 do
            inc(intLineWidth);
          colorsNo := 0 ;
        end else
        begin
          intLineWidth := realLineWidth ;
        end ;

        // read a word file
        setWorldFile( WORLD_FILE_EXT_GIF ) ;

        // read a word file (alternative)
        if GisIsSameExtent( Extent, GisExtent( 0, 0, 0, 0 ) ) then
          setWorldFile( WORLD_FILE_EXT2_GIF ) ;

        SetLength(bitmapObj, FBitHeight*FBitWidth) ;

        decompress(interleaced) ;
      finally
        FreeObject( fileStream ) ;
      end ;

      inherited ;

      if SafeFileExists( Path ) then
        FAge := GisFileAge( Path ) ;

      FFileInfo := Format( 'Graphics Interchange Format (GIF)' + #13#10 +
                           '%d x %d; %d bit',
                           [ FBitWidth, FBitHeight, realBitCount ]
                         ) ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), Path, 0) ;
    end ;

  end ;

  function TGIS_LayerGIF.getLinePixels(
    const _buffer   : TGIS_Pixels  ;
    const _offset   : Integer ;
    const _linenr   : Integer ;
    const _pixStart : Integer ;
    const _pixCount : Integer
  ) : Integer ;
  var
    i, k, idx : Integer ;
    linenr    : Integer ;
    pix_start : Integer ;
    pixels    : Integer ;
  begin
    if _linenr < FBitHeight then
      linenr := _linenr
    else
      linenr := FBitHeight -1 ;
    pix_start := _pixStart +linenr*FBitWidth ;
    pixels := _pixCount ;

    i := _offset ;
    for k := 0 to pixels -1 do begin
      idx := bitmapObj[pix_start + k] ;
      _buffer[i] := Integer($FF000000) or (Integer(bitPalette[idx].R) shl 16)
                                       or (Integer(bitPalette[idx].G) shl 08)
                                       or (Integer(bitPalette[idx].B)       ) ;

      inc(i) ;

    end ;
    Result := pixels ;
  end ;


  procedure TGIS_LayerGIF.decompress(
    const _interlaced : Boolean
  ) ;
  var
    data : TBytes ;
    sp : Integer ;
    decode_dat : array[0..4095] of Byte ;
    dec_state : T_DecodeState {$IFDEF JAVA} := new T_DecodeState {$ENDIF} ;
    prefix : array [0..4095] of Integer ;
    suffix : array [0..4095] of Integer ;
    in_buf : Word ;
    px  : Integer ;
    code_size : Byte;
    temp_old_code, old_code : Word;
    code, c : Word;
    max_val : Boolean;
    prt   : Word;
    tprt  : Word;
    b,v : Byte;
    spos : Integer;

    function init_decompression( const _initCodeSize : Byte ) : Boolean;
    begin
      Result := true ;
      with dec_state do begin
        codeSize := _initCodeSize;
        if not (codeSize in [2..9]) then begin
          Result:=false;
          exit;
        end ;
        {$IFDEF OXYGENE}
          &step := 8 ;
        {$ELSE}
          step := 8 ;
        {$ENDIF}
        dataLen := 0 ;
        {$IFDEF OXYGENE}
          currCodeSize := codeSize + 1 ;
          clearCode := 1 shl codeSize;
          endingCode := clearCode + 1 ;
          highCode := clearCode - 1 ;
        {$ELSE}
          currCodeSize := succ(codeSize);
          clearCode := 1 shl codeSize;
          endingCode := succ(clearCode);
          highCode := pred(clearCode);
        {$ENDIF}
        bitsLeft := 0;
        posY := 0;
        ilPass := 0;
      end ;
    end ;

    function get_final_line_number( const _lineNo : Integer;
                                    var   _step   : Integer ;
                                    var   _iPass  : Integer
                                   ): Integer;
    begin
      Result := _lineNo;
      inc(Result, _step);
      if (Result >= BitHeight) then
        repeat
          if (_iPass > 0) then
            _step := _step div 2;
          inc(_iPass);
          Result := _step div 2;
        until (Result < BitHeight);
    end ;

    function next_code : Word;
    const
    {$IFDEF OXYGENE}
      code_mask: array[0..12] of Word = [
        $0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
        $01FF, $03FF, $07FF, $0FFF
      ] ;
    {$ELSE}
      code_mask: array[0..12] of Word = (
        $0000, $0001, $0003, $0007, $000F, $001F, $003F, $007F, $00FF,
        $01FF, $03FF, $07FF, $0FFF
      ) ;
    {$ENDIF}
    var
      result32 : Integer ;
    begin
      with dec_state do begin
        if bitsLeft = 0 then begin
          currByte := data[readPos];
          inc(readPos);
          bitsLeft := 8;
         end ;
         result32 := currByte shr (8 - bitsLeft);
         while currCodeSize > bitsLeft do begin
           currByte := data[readPos];
           inc(readPos);
           result32 := result32 or (currByte shl bitsLeft);
           bitsLeft := bitsLeft + 8;
         end ;
         bitsLeft := bitsLeft - currCodeSize;
         Result := result32 and code_mask[currCodeSize];
      end ;
    end ;

    procedure decode_code(var _code: Word);
    begin
      while _code > dec_state.highCode do begin
        decode_dat[sp] := suffix[_code];
        _code := prefix[_code];
        inc(sp);
      end ;
      decode_dat[sp] := _code;
      inc(sp);
    end ;

    procedure get_data ;
    begin
      with dec_state do begin
        while sp > 0 do begin
          dec(sp);
          if posY < FBitHeight then
            bitmapObj[px] := decode_dat[sp] ;
          inc(px);
          inc(in_buf);
          if in_buf > FBitWidth then begin
            if not _interlaced then
              inc(posY)
            else
              posY := get_final_line_number(posY, step, ilPass) ;
            in_buf := 1;
            if posY >= FBitHeight then
              exit ;
              px := posY*FBitWidth +in_buf-1 ;
          end ;
        end ;
      end ;
    end ;

    procedure checkPrtValue(var _prt    : Word ;
                            var _tprt   : Word ;
                            var _maxVal : Boolean
                         );
    begin
      if _prt >= _tprt then begin
        if dec_state.currCodeSize < 12 then begin
          _tprt := _tprt shl 1;
          inc(dec_state.currCodeSize)
        end else
          _maxVal := True;
      end ;
    end ;

  begin
    spos:=fileStream.Position;
    dataOffset := spos ;

    fileStream.ReadByte(code_size, 1);
    if not init_decompression(code_size) then begin
      fileStream.Position:=spos;
      exit;
    end ;
    px := dec_state.posY*FBitWidth ;
    SetLength(data, fileStream.Size -spos) ;
    repeat
      if (fileStream.ReadByte(b,1)=0) then
        break;
      if b=0 then break;
      {$IFDEF OXYGENE}
        v := fileStream.Read( data, dec_state.dataLen, b ) ;
      {$ELSE}
        v := fileStream.Read( data[dec_state.dataLen], b ) ;
      {$ENDIF}
      dec_state.dataLen := dec_state.dataLen + (v and $ff) ;
    until false;
    dec_state.readPos := 0;
    old_code := 0;
    sp := 0;
    in_buf := 1;
    max_val := False;
    if dec_state.readPos >= dec_state.dataLen then begin
      data := nil ;
      exit;
    end ;
    c := next_code ;
    while c <> dec_state.endingCode do begin
      if c = dec_state.clearCode then begin
        dec_state.currCodeSize := dec_state.codeSize + 1;
        prt := dec_state.endingCode + 1;
        tprt := 1 shl dec_state.currCodeSize;
        while c = dec_state.clearCode do begin
          if dec_state.readPos >= dec_state.dataLen then begin
            data := nil ;
            exit;
          end ;
          c := next_code ;
        end ;
        if c = dec_state.endingCode then begin
          data := nil ;
          fileStream.Position:=spos;
         exit;
        end ;
        if c >= prt then
          c := 0;
        old_code := c;
        decode_dat[sp] := c;
        inc(sp);
      end
      else begin
        code := c ;
        if code < prt then begin
          decode_code(code);
          if prt <= tprt then begin
            suffix[prt] := code;
            prefix[prt] := old_code;
            inc(prt);
            checkPrtValue(prt, tprt, max_val);
            old_code := c;
          end ;
        end
        else begin
          if code <> prt then begin
             data := nil ;
             fileStream.Position:=spos;
             exit;
          end ;
          temp_old_code := old_code;
           while old_code > dec_state.highCode do begin
             decode_dat[sp] := suffix[old_code];
             old_code := prefix[old_code];
           end ;
           decode_dat[sp] := old_code;
           if prt <= tprt then begin
             suffix[prt] := old_code;
             prefix[prt] := temp_old_code ;
             inc(prt);
             checkPrtValue(prt, tprt, max_val);
           end ;
           decode_code(code);
           old_code := c;
         end ;
       end ;
       get_data;
       if dec_state.readPos >= dec_state.dataLen then begin
         data := nil ;
         exit;
       end ;
       c := next_code ;
       if (max_val = True) and (c <> dec_state.clearCode) then begin
         data := nil ;
         fileStream.Position:=spos;
         exit;
       end ;
       max_val := False;
     end ;
    data := nil ;
  end ;

  procedure TGIS_LayerGIF.Alive ;
  begin
    if assigned( bitmapObj ) then exit ;

    SetLength(bitmapObj, FBitHeight*BitWidth) ;

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

    try
      fileStream.Position := dataOffset ;
      decompress(interleaced) ;
    finally
      FreeObject( fileStream ) ;
    end ;
  end ;

  function TGIS_LayerGIF.DormantGain : Integer ;
  begin
    case DormantMode of
      TGIS_LayerDormantMode.Off :
        Result := 0 ;
      else
        Result := 1 ;
    end ;
  end ;

  procedure TGIS_LayerGIF.Dormant ;
  begin
    if DormantMode = TGIS_LayerDormantMode.Off then
      exit ;

    inherited ;
    bitmapObj := nil ;
  end ;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerGIF.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-GIF', 'Graphic Interchange Format', TGIS_LayerGIF, '.gif',
                   TGIS_RegisteredLayerType.Pixel, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   False
                 ) ;
  end ;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerGIF.SelfRegisterLayer() ;
{$ENDIF}

{==================================== END =====================================}
end.

