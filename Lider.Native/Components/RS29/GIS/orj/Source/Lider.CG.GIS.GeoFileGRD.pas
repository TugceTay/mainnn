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
  Procedures to write an Arcinfo ASCII Grid and Surfer ASCII Grid file of any size.
}

{$IFDEF DCC}
  unit GisFileGRD ;
  {$HPPEMIT '#pragma link "GisFileGRD"'}
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
    System.Runtime.InteropServices,
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    GisRtl,
    GisTypes,
    GisTypesUI,
    GisStreams,
    GisCsSystems,
    GisFilePixel ;
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
  ///   The Class which encapsulates the writing of an Arcinfo ASCII Grid file.
  ///   A file of any size can be written.
  /// </summary>
  TGIS_FileGRD = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_FilePixel )
    protected // properties internal values

      /// <summary>
      ///   Bitmap file stram.
      /// </summary>
      fileStream : TGIS_FileStream ;

    protected

      /// <inheritdoc/>
      procedure prepareCapabilities   ; override;
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
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
      procedure   WriteGrid           ( const _x      : Integer ;
                                        const _y      : Integer ;
                                        const _grd    : TGIS_GridArray
                                      ) ; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisFunctions ;
{$ENDIF}

const
    // Extension of header file .
    HEADER_FILE_EXT_GRD       = '.HDR' ;
    // Write buffer size.
    MAX_WRITE_BUFFER_SIZE_GRD = 1024000 ;
    // GRD file specific no data value
    GIS_GRD_NOVALUE = -9999 ;

    GRD_METADATA_WRITE_PRECISION = 'TGIS_LayerGRD.WritePrecision' ;

//==============================================================================
// TGIS_FileGRD
//==============================================================================

  constructor TGIS_FileGRD.Create ;
  begin
    inherited ;
  end ;

  constructor TGIS_FileGRD.Create(
    const _path        : String      ;
    const _ext         : TGIS_Extent ;
    const _width       : Integer     ;
    const _height      : Integer     ;
    const _subformat   : TGIS_LayerPixelSubFormat     ;
    const _ppi         : Integer     ;
    const _cs          : TGIS_CSCoordinateSystem
  ) ;
  var
    scale_x   : Double ;
    nd        : Double ;
    hdrstr    : String ;
    buf       : TBytes ;
    height    :Integer ;
  begin
    inherited Create( _path, _ext, _width, _height,
                      _subformat, _ppi, _cs
                    ) ;

    scale_x := ( _ext.XMax - _ext.XMin ) / _width  ;
    height := _height ;
    if RoundS(( _ext.YMax - _ext.YMin ) / scale_x) <> height then
      height := RoundS(( _ext.YMax - _ext.YMin ) / scale_x) ;

    nd      :=  GIS_GRD_NOVALUE ;
    FWidth  := _width ;
    FExtent := _ext   ;
    FHeight := height ;

    try
      hdrstr := Format(  'NCOLS        %d'    + #13#10 +
                         'NROWS        %d'    + #13#10 +
                         'XLLCORNER    %s'    + #13#10 +
                         'YLLCORNER    %s'    + #13#10 +
                         'CELLSIZE     %.15s' + #13#10 +
                         'NODATA_VALUE %s'    + #13#10 ,
                         [ _width,
                           height,
                           DotFloatToStr( _ext.XMin ),
                           DotFloatToStr( _ext.YMin ),
                           DotFloatToStr( scale_x ),
                           DotFloatToStr( nd )
                         ]
                       ) ;

      fileStream := TGIS_FileStream.Create( _path, fmCreate ) ;
      fileStream.Position := 0 ;

      buf := ConvertAnsiString( hdrstr ) ;
      {$IFDEF OXYGENE}
        fileStream.Write( buf, length( buf ) ) ;
      {$ELSE}
        fileStream.Write(buf[0], length( buf ) ) ;
      {$ENDIF}
    finally
      buf := nil ;
    end ;
  end ;

  procedure TGIS_FileGRD.doDestroy ;
  begin
    if assigned( fileStream ) then
      FreeObject( fileStream ) ;

    inherited ;
  end ;

  procedure TGIS_FileGRD.prepareCapabilities ;
  begin
    inherited ;

    Capabilities.Add( TGIS_LayerPixelSubFormat.Create(
                        TGIS_PixelFormat.Custom, False, TGIS_PixelSubFormat.GRID,
                        TGIS_CompressionType.None,
                        0
                      )
                    ) ;
  end ;

  procedure TGIS_FileGRD.WriteGrid(
    const _x   : Integer ;
    const _y   : Integer ;
    const _grd : TGIS_GridArray
  ) ;
  var
    w, h  : Integer ;
    i, k  : Integer ;
    buf   : TBytes ;
    sb    : TStringBuilder ;
    prec  : Integer ;
  begin
    if not assigned( fileStream ) then exit ;

     h := length( _grd ) ;
     w := length( _grd[0] ) ;

     if (h + _y) > FHeight then
       h := FHeight - _y ;

     if (w + _x) > FWidth then
       w := FWidth - _x ;

     prec := GisMetadataAsInteger( GRD_METADATA_WRITE_PRECISION, 3 ) ;

     sb := TStringBuilder.Create ;
     try
       for i := 0 to h -1 do begin
         for k := 0 to w -1 do begin
           sb.Clear ;
           sb.Append( DotFloatToStrPrec( _grd[i][k], prec ) ) ;

           if ( _x + k ) = ( FWidth - 1 ) then
             sb.Append( #13#10 )
           else
             sb.Append( ' ' ) ;

           buf := ConvertAnsiString( sb.ToString ) ;

           {$IFDEF OXYGENE}
             fileStream.Write( buf, length(buf) ) ;
           {$ELSE}
             fileStream.Write( buf[0], length(buf) ) ;
           {$ENDIF}
         end ;
       end ;
     finally
       FreeObject( sb ) ;
     end ;
  end ;


//==================================== END =====================================
end.
