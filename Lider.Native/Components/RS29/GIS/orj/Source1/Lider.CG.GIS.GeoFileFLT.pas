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
  Procedures to write an Arcinfo Float(Binary) Grid file of any size.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileFLT ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileFLT"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoFilePixel ;
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
  ///   The Class which encapsulates the writing of a Arcinfo Float(Binary)
  ///   Grid file. A file of any size can be written.
  /// </summary>
  TGIS_FileFLT = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_FilePixel )
    protected // properties internal values

        /// <summary>
        ///   Bitmap file stram.
        /// </summary>
        fileStream : TGIS_FileStream ;
    protected

      /// <inheritdoc/>
      procedure prepareCapabilities   ; override;
    protected

      procedure doDestroy             ; override;
    public

      /// <inheritdoc/>
      constructor Create              ; overload; override;

      /// <inheritdoc/>
      /// <param name="_subformat">
      ///   unused
      /// </param>
      /// <param name="_ppi">
      ///   unused
      /// </param>
      constructor Create              ( const _path        : String      ;
                                        const _ext         : TGIS_Extent ;
                                        const _width       : Integer     ;
                                        const _height      : Integer     ;
                                        const _subformat   : TGIS_LayerPixelSubFormat     ;
                                        const _ppi         : Integer     ;
                                        const _cs          : TGIS_CSCoordinateSystem
                                      ) ; overload; override;

      /// <inheritdoc/>
      procedure   WriteGrid           ( const _x           : Integer ;
                                        const _y           : Integer ;
                                        const _grd         : TGIS_GridArray
                                      ) ; overload; override;
  end ;

//##############################################################################
implementation

const

    // Extension of header file.
    HEADER_FILE_EXT_FLT       = '.hdr' ;

    // Write buffer size.
    MAX_WRITE_BUFFER_SIZE_FLT = 1024000 ;

//==============================================================================
// TGIS_FileFLT
//==============================================================================

  constructor TGIS_FileFLT.Create ;
  begin
    inherited ;
  end;

  constructor TGIS_FileFLT.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  var
    scale_x    : Double   ;
    olist      : TStringList ;
    nd         : Double ;
    height     :Integer ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    scale_x := ( _ext.XMax - _ext.XMin ) / _width  ;
    height := _height ;
    if RoundS(( _ext.YMax - _ext.YMin ) / scale_x) <> height then
      height := RoundS(( _ext.YMax - _ext.YMin ) / scale_x) ;

    nd      :=  GIS_GRID_NOVALUE ;
    FWidth  := _width ;
    FExtent := _ext   ;
    FHeight := height ;

    olist := TStringList.Create ;
    try
      olist.Text := Format( 'NCOLS        %d'    + #13#10 +
                            'NROWS        %d'    + #13#10 +
                            'XLLCORNER    %s'    + #13#10 +
                            'YLLCORNER    %s'    + #13#10 +
                            'CELLSIZE     %s'    + #13#10 +
                            'NODATA_VALUE %s'    + #13#10 +
                            'BYTEORDER LSBFIRST' ,
                            [ _width,
                              height,
                              DotFloatToStr( _ext.XMin ),
                              DotFloatToStr( _ext.YMin ),
                              DotFloatToStr( scale_x ),
                              DotFloatToStr( nd )
                            ]
                          ) ;
      olist.SaveToFile( GetPathNoExt( _path ) + HEADER_FILE_EXT_FLT ) ;
    finally
      FreeObject( olist ) ;
    end ;

    fileStream := TGIS_FileStream.Create( _path, fmCreate ) ;
    fileStream.Size := Int64(sizeOf(Single))*FWidth*FHeight ;
  end ;

  procedure TGIS_FileFLT.doDestroy ;
  begin
    if assigned( fileStream ) then
      FreeObject( fileStream ) ;

    inherited ;
  end ;

  procedure TGIS_FileFLT.prepareCapabilities;
  begin
    inherited ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID,
                        TGIS_CompressionType.None,
                        0
                      )
                    ) ;
  end;

  procedure TGIS_FileFLT.WriteGrid(
    const _x   : Integer ;
    const _y   : Integer ;
    const _grd : TGIS_GridArray
  ) ;
  var
    pos     : Int64 ;
    w, h    : Integer ;
    s       : Integer ;
    i       : Integer ;
    {$IFDEF OXYGENE}
     j      : Integer ;
     buf    : TBytes  ;
    {$ELSE}
      portion : Integer ;
    {$ENDIF}
  begin
    if not assigned( fileStream ) then exit ;

     s := sizeOf( _grd[0][0] ) ;
     h := length( _grd ) ;
     w := length( _grd[0] ) ;

     if (h + _y) > FHeight then
       h := FHeight - _y ;
     if (w + _x) > FWidth then
       w := FWidth - _x ;

     {$IFDEF OXYGENE}
     {$ELSE}
      portion := s * w ;
     {$ENDIF}
     pos      := Int64(s) * Int64(FWidth) * _y + s * _x ;

     for i := 0 to h-1 do begin
       fileStream.Position := pos ;
       {$IFDEF OXYGENE}
        for j := 0 to w-1 do begin
          buf := BitConverter.GetBytes( _grd[i][j] ) ;
          fileStream.Write( buf, length( buf ) ) ;
        end;
       {$ELSE}
        fileStream.Write( _grd[i][0], portion ) ;
       {$ENDIF}
       pos := pos + FWidth * s ;
     end ;
  end ;

//==================================== END =====================================
end.
