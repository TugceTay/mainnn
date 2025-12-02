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
  Procedures to write a bitmap file of any size.
}

{$IFDEF DCC}
  unit GisFileBMP ;
  {$HPPEMIT '#pragma link "GisFileBMP"'}
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
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    GisRtl,
    GisTypes,
    GisTypesUI,
    GisStreams,
    GisFilePixel,
    GisResource,
    GisCsSystems ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}
{$IFDEF ISLAND}
uses
  TatukGIS.RTL ;
{$ENDIF}


type
  {#GENDOC:HIDE}
  TGIS_BitmapFileHeader = {$IFDEF OXYGENE} public {$ELSE} packed {$ENDIF} record
    bfType: Word;
    bfSize: Cardinal;
    bfReserved1: Word;
    bfReserved2: Word;
    bfOffBits: Cardinal;
  end;

  {#GENDOC:HIDE}
  TGIS_BitmapInfoHeader = {$IFDEF OXYGENE} public {$ELSE} packed {$ENDIF} record
    biSize: Cardinal;
    biWidth: Int32;
    biHeight: Int32;
    biPlanes: Word;
    biBitCount: Word;
    biCompression: Cardinal;
    biSizeImage: Cardinal;
    biXPelsPerMeter: Int32;
    biYPelsPerMeter: Int32;
    biClrUsed: Cardinal;
    biClrImportant: Cardinal;
  end;


  /// <summary>
  ///   The Class which encapsulates the writing of a bitmap file. A file of
  ///   any size can be written.
  /// </summary>
  TGIS_FileBMP = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_FilePixel )

    protected

      /// <summary>
      ///   Bitmap file stream.
      /// </summary>
      fileStream : TGIS_FileStream;

      /// <summary>
      ///   TBitmapFileHeader record .
      /// </summary>
      fileHeader : TGIS_BitmapFileHeader;

      /// <summary>
      ///   TBitmapFileHeader record .
      /// </summary>
      infoHeader : TGIS_BitmapInfoHeader;

      /// <summary>
      ///   Length of line in bits.
      /// </summary>
      bitLineWidth  : Integer ;

      /// <summary>
      ///   If True then write mode
      /// </summary>
      writeMode : Boolean ;

    protected
      /// <inheritdoc/>
      function  writeLine          ( const _buffer : TBytes  ;
                                     const _idx    : Integer ;
                                     const _linenr : Integer ;
                                     const _start  : Integer ;
                                     const _bytes  : Integer
                                     ) : Integer; override;
    protected

      /// <inheritdoc/>
      procedure prepareCapabilities  ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy            ; override;
    public

      /// <inheritdoc/>
      constructor Create             ; overload; override;

      /// <inheritdoc/>
      constructor Create             ( const _path        : String      ;
                                       const _ext         : TGIS_Extent ;
                                       const _width       : Integer     ;
                                       const _height      : Integer     ;
                                       const _subformat   : TGIS_LayerPixelSubFormat     ;
                                       const _ppi         : Integer     ;
                                       const _cs          : TGIS_CSCoordinateSystem
                                     ) ; overload; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisClasses ;
{$ENDIF}

const
    // Extension of world file (extension file for pixel layer).
    WORLD_FILE_EXT = '.bpw' ;

//==============================================================================
// TGIS_FileBMP
//==============================================================================

  procedure TGIS_FileBMP.prepareCapabilities  ;
  begin
    inherited ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.RGB,
                         False,
                         TGIS_PixelSubFormat.None,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.RGB,
                         False,
                         TGIS_PixelSubFormat.BMP,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.ARGB,
                         False,
                         TGIS_PixelSubFormat.None,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.ARGB,
                         False,
                         TGIS_PixelSubFormat.BMP,
                         TGIS_CompressionType.None,
                        0
                      )
                    ) ;
  end ;

  function TGIS_FileBMP.writeLine(
    const _buffer : TBytes  ;
    const _idx    : Integer ;
    const _linenr : Integer ;
    const _start  : Integer ;
    const _bytes  : Integer
  ) : Integer;
  var
    offset : Int64 ;
  begin
    try
      offset := Int64(fileHeader.bfOffBits) + Int64(infoHeader.biSizeImage)
                + _start - (_linenr +1 )*bitLineWidth ;

      {$IFDEF OXYGENE}
        fileStream.Seek( offset, soBeginning ) ;
      {$ELSE}
        fileStream.Seek( offset, 0 ) ;
      {$ENDIF}
      fileStream.Write( _buffer{$IFDEF DCC}[0]{$ENDIF}, _bytes ) ;
      Result := _bytes ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0) ;
    end ;
  end;

  constructor TGIS_FileBMP.Create ;
  begin
    inherited ;
  end;

  constructor TGIS_FileBMP.Create(
     const _path        : String      ;
     const _ext         : TGIS_Extent ;
     const _width       : Integer     ;
     const _height      : Integer     ;
     const _subformat   : TGIS_LayerPixelSubFormat     ;
     const _ppi         : Integer     ;
     const _cs          : TGIS_CSCoordinateSystem
   ) ;
  var
    bfhs, bihs : Cardinal ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    writeMode := True ;

    if SubFormat.PixelFormat = TGIS_PixelFormat.RGB then begin
      infoHeader.biBitCount := 24 ;
      bitLineWidth := ((Width * 3 +3) div 4) * 4;
    end
    else
    if SubFormat.PixelFormat = TGIS_PixelFormat.ARGB then begin
      infoHeader.biBitCount := 32 ;
      bitLineWidth := Width * 4;
    end
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0) ;

    FPixelFormat := SubFormat.PixelFormat  ;

    writeWorldFile( _ext, WORLD_FILE_EXT, _cs ) ;

    {$IFDEF OXYGENE}
      bihs := sizeOf(Cardinal)*5 + sizeOf(Int32)*4 + sizeOf(Word)*2 ;
    {$ELSE}
      bihs := sizeOf(TGIS_BitmapInfoHeader) ;
    {$ENDIF}

    {$IFDEF OXYGENE}
      bfhs := sizeOf(Cardinal)*2 + sizeOf(Word)*3 ;
    {$ELSE}
      bfhs := sizeOf(TGIS_BitmapFileHeader) ;
    {$ENDIF}

    infoHeader.biSize := bihs ;
    infoHeader.biPlanes        := 1 ;
    infoHeader.biHeight        := Height ;
    infoHeader.biWidth         := Width ;
    infoHeader.biSizeImage     := Cardinal(bitLineWidth)*Cardinal(_height) ;
    infoHeader.biClrUsed       := 0 ;
    infoHeader.biXPelsPerMeter := RoundS(_ppi*(100/2.53995));
    infoHeader.biYPelsPerMeter := infoHeader.biXPelsPerMeter ;
    infoHeader.biCompression   := 0 ;
    fileHeader.bfOffBits       :=  bihs + bfhs;
    fileHeader.bfType          := $4D42 ;
    fileHeader.bfSize          := fileHeader.bfOffBits ;

    fileStream := TGIS_FileStream.Create( _path, fmCreate ) ;
    {$IFDEF OXYGENE}
      fileStream.WriteWord( fileHeader.bfType) ;
      fileStream.WriteCardinal( fileHeader.bfSize) ;
      fileStream.WriteWord( fileHeader.bfReserved1 );
      fileStream.WriteWord( fileHeader.bfReserved2 ) ;
      fileStream.WriteCardinal( fileHeader.bfOffBits ) ;
    {$ELSE}
      fileStream.Write(fileHeader, sizeOf(TGIS_BitmapFileHeader));
    {$ENDIF}
    {$IFDEF OXYGENE}
      fileStream.WriteCardinal( infoHeader.biSize) ;
      fileStream.WriteInteger( infoHeader.biWidth) ;
      fileStream.WriteInteger( infoHeader.biHeight) ;
      fileStream.WriteWord( infoHeader.biPlanes) ;
      fileStream.WriteWord( infoHeader.biBitCount) ;
      fileStream.WriteCardinal( infoHeader.biCompression ) ;
      fileStream.WriteCardinal( infoHeader.biSizeImage) ;
      fileStream.WriteInteger( infoHeader.biXPelsPerMeter) ;
      fileStream.WriteInteger( infoHeader.biYPelsPerMeter );
      fileStream.WriteCardinal( infoHeader.biClrUsed) ;
      fileStream.WriteCardinal( infoHeader.biClrImportant ) ;
    {$ELSE}
      fileStream.Write(infoHeader, sizeOf(TGIS_BitmapInfoHeader));
    {$ENDIF}
  end ;

  procedure TGIS_FileBMP.doDestroy ;
  var
    i       : Integer ;
    bt_zero : array [0..0] of Byte  ;
    bsize  : Cardinal ;
    bms    : Integer ;
  begin
    if writeMode and assigned( fileStream ) then begin
      // guarantee that size is *4
      bt_zero[0] := 0 ;
      bsize :=  fileStream.Size ;
      bms := bsize mod 4 ;
      if bms = 0 then
        bms := 4 ;
      fileStream.Position := fileStream.Size ;
      for i:=1 to bms do
        fileStream.Write( bt_zero, 1 ) ;
    end ;
    FreeObject( fileStream ) ;
    inherited ;
  end ;

//==================================== END =====================================
end.
