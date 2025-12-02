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
  Procedures to write a PNG file of any size (supported by computer memory).

  Use Gustavo Huffenbacher Daud TPNGImage (http://pngdelphi.sourceforge.net).
  We included TPNGImage based on author permission.
}

{$IFDEF DCC}
  unit GisFilePNG ;
  {$HPPEMIT '#pragma link "GisFilePNG"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}


interface

{$IFDEF CLR}
  uses
    System.Runtime.InteropServices,
    System.Drawing,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Math,
    GisRtl,
    GisTypes,
    GisTypesUI,
    GisInternals,
    GisFunctions,
    GisFilePixel,
    GisResource,
    GisCsSystems,
    GisStreams ;
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

  /// <summary>
  ///   The Class which encapsulates the writing of a bitmap file. A file of
  ///   any size can be written.
  /// </summary>
  TGIS_FilePNG = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_FilePixel )
    private
      bmpObj    : TGIS_Bitmap    ;
      pixObj    : TGIS_Pixels    ;
      writeMode : Boolean   ;
      isReady   : Boolean   ;
    protected
      /// <summary>
      ///   PNG file stram.
      /// </summary>
      fileStream : TGIS_FileStream;
    protected

      /// <inheritdoc/>
      procedure prepareCapabilities  ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy ; override;
    public

      /// <inheritdoc/>
      constructor Create             ; overload; override;

      /// <inheritdoc/>
      constructor Create            ( const _path        : String      ;
                                      const _ext         : TGIS_Extent ;
                                      const _width       : Integer     ;
                                      const _height      : Integer     ;
                                      const _subformat   : TGIS_LayerPixelSubFormat     ;
                                      const _ppi         : Integer     ;
                                      const _cs          : TGIS_CSCoordinateSystem
                                     ) ; overload; override;

      /// <inheritdoc/>
      procedure   Write              ( const _x       : Integer ;
                                       const _y       : Integer ;
                                       const _pixels  : TGIS_Pixels ;
                                       const _pformat : TGIS_PixelFormat ;
                                       const _width   : Integer ;
                                       const _height  : Integer
                                     ) ; override;
  end ;

//##############################################################################
implementation
{$IFDEF DCC}
  uses
    GisClasses ;
{$ENDIF}

const
  { Extension of world file (extension file for pixel layer). }
    WORLD_FILE_EXT_PNG_FILE = '.pgw' ;

//==============================================================================
// TGIS_FilePNG
//==============================================================================

  procedure TGIS_FilePNG.prepareCapabilities  ;
  begin
    inherited ;

    {$IFDEF DCC}
    if not ( Assigned( NativeBitmapFactory ) and NativeBitmapFactory.ClassName.Contains('FMX') )
    then
    {$ENDIF}
    begin
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.RGB, False, TGIS_PixelSubFormat.PNG,
                           TGIS_CompressionType.PNG,
                           0
                        )
                      ) ;
      Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                           TGIS_PixelFormat.Bit8, False, TGIS_PixelSubFormat.PNG,
                           TGIS_CompressionType.PNG,
                           0
                        )
                      ) ;
    end;
    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                         TGIS_PixelFormat.ARGB, False, TGIS_PixelSubFormat.PNG,
                         TGIS_CompressionType.PNG,
                         0
                      )
                    ) ;
  end ;


  constructor TGIS_FilePNG.Create ;
  begin
    inherited ;
  end;

  constructor TGIS_FilePNG.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    writeMode := True ;
    isReady := True ;

    FPixelFormat := _subformat.PixelFormat ;

    writeWorldFile( _ext, WORLD_FILE_EXT_PNG_FILE, _cs ) ;

    bmpObj := TGIS_Bitmap.Create(_width, _height) ;
    pixObj := nil ;

  end ;

  procedure TGIS_FilePNG.doDestroy ;
  var
    blob : TGIS_MemoryStream ;
    ls   : Int64 ;
    buf  : TBytes ;
  begin
    try
      if writeMode  and isReady then begin
        try
            fileStream := TGIS_FileStream.Create( Path, fmCreate ) ;
            blob := TGIS_MemoryStream.Create ;
            bmpObj.UnlockPixels ;
//          try
            bmpObj.SaveToStream( blob, FSubFormat.PixelFormat, FSubFormat.Subformat, CompressionLevel ) ;
            blob.Position := 0 ;
            ls := blob.Size ;
           {$IFDEF DCC}
             SetLength( buf, ls ) ;
             Move( blob.Memory^, buf[0], ls ) ;
             fileStream.Write( buf[0], ls) ;
           {$ELSE}
             buf := blob.Memory ;
             fileStream.Write( buf, ls) ;
          {$ENDIF}
         except
           raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_IMAGETOBIG ), '', 0) ;
         end;
       end ;
    finally
      if writeMode  and isReady then begin
        FreeObject( blob ) ;
        FreeObject( fileStream ) ;
      end ;
      FreeObject( bmpObj ) ;
      inherited ;
    end ;
  end ;

  procedure TGIS_FilePNG.Write(
    const _x       : Integer ;
    const _y       : Integer ;
    const _pixels  : TGIS_Pixels ;
    const _pformat : TGIS_PixelFormat ;
    const _width   : Integer ;
    const _height  : Integer
  ) ;
  var
    w, h : Integer ;
    i    : Integer ;
  begin
    if not assigned(pixObj) then begin
      try
        bmpObj.LockPixels( pixObj, True ) ;
      except
        isReady := False ;
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_IMAGETOBIG ), '', 0) ;
      end;
    end;

    if (_x + _width) > Width then
      w := Width -_x
    else
      w := _width ;

    if (_y + _height) > Height then
      h := Height -_y
    else
      h := _height ;

    if (w = Width) and (w = _width) then // solid input and draw area
      GisCopyPixels( _pixels, 0, pixObj, _y*w, w*h )
    else begin
      for i := 0 to h -1 do
        GisCopyPixels( _pixels, i*_width, pixObj, (_y +i)*Width +_x, w ) ;
    end;
  end ;

//==================================== END =====================================
end.
