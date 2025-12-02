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
  unit GisFilePixel ;
  {$HPPEMIT '#pragma link "GisFilePixel"'}
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
    System.Math,

    GisRtl,
    GisTypes,
    GisClasses,
    GisTypesUI,
    GisCsSystems ;
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
  ///   The Class which encapsulates the writing of a pixel file. A file of any
  ///   size can be written.
  /// </summary>
  TGIS_FilePixel = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    protected // property internal values
      /// <summary>
      ///   Path to a bitmap file.
      /// </summary>
      FPath              : String       ;
      /// <summary>
      ///   The width of bitmap in pixels.
      /// </summary>
      FWidth             : Integer      ;
      /// <summary>
      ///   The height of bitmap in pixels.
      /// </summary>
      FHeight            : Integer      ;
      /// <summary>
      ///   The width of bitmap tile upon writing.
      /// </summary>
      FWriteTileWidth    : Integer      ;
      /// <summary>
      ///   The height of bitmap tile upon writing.
      /// </summary>
      FWriteTileHeight   : Integer      ;
      /// <summary>
      ///   The extent of the of bitmap in map units.
      /// </summary>
      FExtent            : TGIS_Extent  ;
      /// <summary>
      ///   The compression of image. 0 means the minimal size (and lower quality);
      ///   100 means the maximum size.
      /// </summary>
      FCompressionLevel  : Integer      ;
      /// <summary>
      ///   The PPI of image.
      /// </summary>
      FPpi               : Integer      ;
      /// <summary>
      ///   The pixel format of bitmap.
      /// </summary>
      FPixelFormat       : TGIS_PixelFormat ;
      /// <summary>
      ///   List of TGIS_PixelCapabilities.
      /// </summary>
      FCapabilities      : TGIS_LayerPixelSubFormatList  ;
      /// <summary>
      ///   Busy event. Will be fired regularly during long-drawn
      ///   operations like populating quad tree over PixelStore file etc.
      /// </summary>
      FOnBusy            : TGIS_BusyEvent ;
      /// <summary>
      ///   Coordinate system.
      /// </summary>
      FCS                : TGIS_CSCoordinateSystem ;
      /// <summary>
      ///   Current subformat information.
      /// </summary>
      FSubFormat : TGIS_LayerPixelSubFormat ;
    protected

      /// <summary>
      ///   Write extent to the world file (like .tfw, .bpw and .tab file).
      /// </summary>
      /// <param name="_extent">
      ///   extent of the file
      /// </param>
      /// <param name="_ext">
      ///   extension of world file (file existence will be checked)
      /// </param>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      procedure writeWorldFile       ( const _extent : TGIS_Extent ;
                                       const _ext    : String      ;
                                       const _cs     : TGIS_CSCoordinateSystem
                                     ) ;

      /// <summary>
      ///   Write an image line. Do nothing - just for save inheritance.
      /// </summary>
      /// <param name="_buffer">
      ///   buffer to write
      /// </param>
      /// <param name="_idx">
      ///   buffer start index
      /// </param>
      /// <param name="_linenr">
      ///   line number
      /// </param>
      /// <param name="_start">
      ///   offset from line start
      /// </param>
      /// <param name="_bytes">
      ///   bytes count
      /// </param>
      /// <returns>
      ///   written number of bytes
      /// </returns>
      function  writeLine          ( const _buffer : TBytes  ;
                                     Const _idx    : Integer ;
                                     const _linenr : Integer ;
                                     const _start  : Integer ;
                                     const _bytes  : Integer
                                   ) : Integer; virtual;

      /// <summary>
      ///   Prepare capabilities list.
      /// </summary>
      procedure prepareCapabilities  ; virtual;
    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure  doDestroy           ; override;
    public

      /// <summary>
      ///   Create and instance.
      /// </summary>
      constructor Create             ; overload; virtual;

      /// <summary>
      ///   Create and open a new pixel file.
      /// </summary>
      /// <param name="_path">
      ///   path to a created file
      /// </param>
      /// <param name="_ext">
      ///   extent of the file to be written
      /// </param>
      /// <param name="_width">
      ///   width in pixels of a created file
      /// </param>
      /// <param name="_height">
      ///   height in pixels of a created file
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELFORMAT
      /// </exception>
      constructor Create             ( const _path        : String      ;
                                       const _ext         : TGIS_Extent ;
                                       const _width       : Integer     ;
                                       const _height      : Integer
                                     ) ; overload; virtual;

      /// <summary>
      ///   Create and open a new pixel file.
      /// </summary>
      /// <param name="_path">
      ///   path to a created file
      /// </param>
      /// <param name="_ext">
      ///   extent of the file to be written
      /// </param>
      /// <param name="_width">
      ///   width in pixels of a created file
      /// </param>
      /// <param name="_height">
      ///   height in pixels of a created file
      /// </param>
      /// <param name="_subformat">
      ///   subformat number; use property Capabilities to obtain a list of
      ///   supported formats; use 0 to get default
      /// </param>
      /// <param name="_ppi">
      ///   density of output image; will be used to calculate feature size and
      ///   will be embedded into final image whenever possible
      /// </param>
      /// <param name="_cs">
      ///   coordinate system
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BADPIXELFORMAT
      /// </exception>
      constructor Create             ( const _path        : String      ;
                                       const _ext         : TGIS_Extent ;
                                       const _width       : Integer     ;
                                       const _height      : Integer     ;
                                       const _subformat   : TGIS_LayerPixelSubFormat ;
                                       const _ppi         : Integer     ;
                                       const _cs          : TGIS_CSCoordinateSystem
                                     ) ; overload; virtual;

      /// <summary>
      ///   Write a single chunk of a pixel file. The chunk can be any valid
      ///   bitmap that has the same pixel format as the pixel file.
      /// </summary>
      /// <param name="_x">
      ///   horizontal offset of the chunk within bitmap file
      /// </param>
      /// <param name="_y">
      ///   vertical offset of the chunk within bitmap file
      /// </param>
      /// <param name="_pixels">
      ///   buffer to write
      /// </param>
      /// <param name="_pformat">
      ///   pixel format of buffer
      /// </param>
      /// <param name="_width">
      ///   buffer width
      /// </param>
      /// <param name="_height">
      ///   buffer height
      /// </param>
      procedure   Write              ( const _x       : Integer ;
                                       const _y       : Integer ;
                                       const _pixels  : TGIS_Pixels ;
                                       const _pformat : TGIS_PixelFormat ;
                                       const _width   : Integer ;
                                       const _height  : Integer
                                     ) ; virtual;

      /// <summary>
      ///   Write a single chunk of a grid file. The chunk can be any valid
      ///   grid that has the same data format as the grid file.
      /// </summary>
      /// <param name="_x">
      ///   horizontal offset of the chunk within grid file
      /// </param>
      /// <param name="_y">
      ///   vertical offset of the chunk within grid file
      /// </param>
      /// <param name="_grd">
      ///   grid to be written at the _x,_y position
      /// </param>
      procedure   WriteGrid          ( const _x     : Integer ;
                                       const _y     : Integer ;
                                       const _grd   : TGIS_GridArray
                                     ) ; virtual;

      /// <summary>
      ///   Do any operation required to safely initialize write operation like
      ///   batch operations for TGIS_FilePixelStore.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Should be called before writing the whole image.
      ///    </note>
      /// </remarks>
      procedure   InitializeWrite      ; virtual;

      /// <summary>
      ///   Do any operation required to safely finalize write operation like
      ///   building quad trees for TGIS_FilePixelStore.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Should be called after writing the whole image.
      ///    </note>
      /// </remarks>
      procedure   FinalizeWrite      ; virtual;


    public // properties
      /// <summary>
      ///   Path to a bitmap file.
      /// </summary>
      property Path : String read FPath;

      /// <summary>
      ///   The width of bitmap in pixels.
      /// </summary>
      property Width : Integer read FWidth ;

      /// <summary>
      ///   The height of bitmap in pixels.
      /// </summary>
      property Height : Integer read FHeight ;

      /// <summary>
      ///   The width of bitmap tile upon writing.
      /// </summary>
      property WriteTileWidth : Integer read FWriteTileWidth ;

      /// <summary>
      ///   The height of bitmap tile upon writing.
      /// </summary>
      property WriteTileHeight : Integer read FWriteTileHeight ;

      /// <summary>
      ///   The extent of the of bitmap in map units.
      /// </summary>
      property Extent : TGIS_Extent read FExtent ;

      /// <summary>
      ///   The compression level of image. 0 means the minimal size (and lower quality);
      ///   100 means the maximum size.
      /// </summary>
      property CompressionLevel : Integer read FCompressionLevel ;

      /// <summary>
      ///   The PPI of image.
      /// </summary>
      property PPI : Integer read FPpi ;

      /// <summary>
      ///   The pixel format of bitmap.
      /// </summary>
      property PixelFormat : TGIS_PixelFormat read FPixelFormat ;

      /// <summary>
      ///   List of TGIS_PixelCapabilities.
      /// </summary>
      property Capabilities : TGIS_LayerPixelSubFormatList read FCapabilities ;

      /// <summary>
      ///   Current subformat information.
      /// </summary>
      property SubFormat : TGIS_LayerPixelSubFormat read FSubFormat ;

    published //events
      {$IFDEF OXYGENE}
        {$IFDEF CLR}
          /// <event/>
          /// <summary>
          ///   Busy event. Will be fired regularly during long-drawn
          ///   operations like populating quad tree over PixelStore file etc.
          /// </summary>
          event BusyEvent    : TGIS_BusyEvent delegate FOnBusy ;
        {$ENDIF}
        {$IFDEF JAVA}
          /// <event/>
          /// <summary>
          ///  Busy event. Will be fired regularly during long-drawn
          ///  operations like populating quad tree over PixelStore file etc.
          /// </summary>
          property Busy : TGIS_BusyEvent read FOnBusy write FOnBusy ;
        {$ENDIF}
      {$ELSE}
          /// <event/>
          /// <summary>
          ///   Busy event. Will be fired regularly during long-drawn
          ///   operations like populating quad tree over PixelStore file etc.
          /// </summary>
          property BusyEvent   : TGIS_BusyEvent read FOnBusy write FOnBusy ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisInternals,
    GisResource,
    GisCsMapInfo;
{$ENDIF}

const
  { Size of caching device for big image output. }
    MAX_WRITETILE_SIZE = 1024 ;

//==============================================================================
// TGIS_FilePixel
//==============================================================================

  procedure TGIS_FilePixel.writeWorldFile(
    const _extent : TGIS_Extent ;
    const _ext    : String      ;
    const _cs     : TGIS_CSCoordinateSystem
  ) ;
  var
    scale_x : Double   ;
    scale_y : Double   ;
    olist   : TGIS_StringList ;
  begin
    scale_x := ( _extent.XMax - _extent.XMin ) / Width  ;
    scale_y := ( _extent.YMax - _extent.YMin ) / Height ;

    olist := TGIS_StringList.Create ;
    try
      olist.Text := Format( '%s' + #13#10 +
                            '%d' + #13#10 +
                            '%d' + #13#10 +
                            '%s' + #13#10 +
                            '%s' + #13#10 +
                            '%s' + #13#10 ,
                            [ DotFloatToStr(   scale_x ),
                              0,
                              0,
                              DotFloatToStr( - scale_y ),
                              DotFloatToStr( _extent.XMin + 0.5 * scale_x ),
                              DotFloatToStr( _extent.YMax - 0.5 * scale_y )
                            ]
                          ) ;
      olist.SaveToFile( GetPathNoExt( Path ) + _ext, TEncoding.ASCII ) ;

      olist.Text := Format( '!table'                           + #13#10+
                            '!version 300'                     + #13#10+
                            '!charset WindowsLatin1'           + #13#10+
                            ''                                 + #13#10+
                            'Definition Table'                 + #13#10+
                            '  File "%s"'                      + #13#10+
                            '  Type "RASTER"'                  + #13#10+
                            '  (%s,%s) (%d,%d) Label "Pt1",'   + #13#10+
                            '  (%s,%s) (%d,%d) Label "Pt2",'   + #13#10+
                            '  (%s,%s) (%d,%d) Label "Pt3"'    + #13#10+
                            '  %s ',
                            [ GetFileName( Path ),

                              DotFloatToStr( Extent.XMin + 0.5 * scale_x ),
                              DotFloatToStr( Extent.YMax - 0.5 * scale_y ),
                              0, 0 ,

                              DotFloatToStr( Extent.XMax - 0.5 * scale_x ),
                              DotFloatToStr( Extent.YMax - 0.5 * scale_y ),
                              Width, 0,

                              DotFloatToStr( Extent.XMax - 0.5 * scale_x ),
                              DotFloatToStr( Extent.YMin - 0.5 * scale_y ),
                              Width, Height, TGIS_CSFactoryMapInfo.CsToText( _cs )
                            ]
                         ) ;
      olist.SaveToFile( GetPathNoExt( Path ) + '.tab', TEncoding.ASCII ) ;

      olist.Text := Format( '<?xml version="1.0" encoding="UTF-8"?>'      + #13#10+
                            '<kml xmlns="http://www.opengis.net/kml/2.2">'+ #13#10+
                            '<GroundOverlay>'                             + #13#10+
                            '  <name>%s</name>'                           + #13#10+
                            '  <Icon>'                                    + #13#10+
                            '    <href>%s</href>'                         + #13#10+
                            '  </Icon>'                                   + #13#10+
                            '  <LatLonBox>'                               + #13#10+
                            '    <north>%s</north>'                       + #13#10+
                            '    <south>%s</south>'                       + #13#10+
                            '    <east>%s</east>'                         + #13#10+
                            '    <west>%s</west>'                         + #13#10+
                            '  </LatLonBox>'                              + #13#10+
                            '</GroundOverlay>'                            + #13#10+
                            '</kml>',
                            [ GetFileName( Path ), GetFileName( Path ),
                              DotFloatToStr( Extent.YMax ),
                              DotFloatToStr( Extent.YMin ),
                              DotFloatToStr( Extent.XMax ),
                              DotFloatToStr( Extent.XMin )
                            ]
                         ) ;
      olist.SaveToFile( GetPathNoExt( Path ) + '.kml', TEncoding.UTF8 ) ;
    finally
      FreeObject( olist ) ;
    end ;
  end ;

  function  TGIS_FilePixel.writeLine(
      const _buffer : TBytes  ;
      const _idx    : Integer ;
      const _linenr : Integer ;
      const _start  : Integer ;
      const _bytes  : Integer
    ) : Integer ;
  begin
    // just for safe inheritance
    Result := 0 ;
  end ;

  procedure TGIS_FilePixel.prepareCapabilities  ;
  begin
    FCapabilities := TGIS_LayerPixelSubFormatList.Create ;
  end ;

  constructor TGIS_FilePixel.Create ;
  begin
    inherited ;

    FWriteTileWidth  := 0 ;
    FWriteTileHeight := 0 ;

    prepareCapabilities ;
  end ;

  constructor TGIS_FilePixel.Create(
     const _path   : String      ;
     const _ext    : TGIS_Extent ;
     const _width  : Integer     ;
     const _height : Integer
   ) ;
  begin
    Create( _path, _ext, _width, _height,
            TGIS_LayerPixelSubFormat.DefaultSubFormat, 300, nil
           ) ;
  end ;

  constructor TGIS_FilePixel.Create(
     const _path        : String      ;
     const _ext         : TGIS_Extent ;
     const _width       : Integer     ;
     const _height      : Integer     ;
     const _subformat   : TGIS_LayerPixelSubFormat ;
     const _ppi         : Integer     ;
     const _cs          : TGIS_CSCoordinateSystem
   ) ;
  var
    {$IFDEF DCC}
    c : TGIS_LayerPixelSubFormat ;
    {$ENDIF}
    sf_found : Boolean ;
  begin
    inherited Create ;
    FWriteTileWidth  := 0 ;
    FWriteTileHeight := 0 ;

    prepareCapabilities ;

    FPath         := _path  ;
    FWidth        := _width ;
    FExtent       := _ext   ;
    FHeight       := _height ;

    FWriteTileWidth  := Min( MAX_WRITETILE_SIZE, FWidth ) ;
    FWriteTileHeight := Min( MAX_WRITETILE_SIZE, FWidth ) ;

    FCompressionLevel := Max( 0, Min( 100, _subformat.CompressionLevel ) ) ;
    FPpi              := Max( 10, _ppi ) ;
    FPixelFormat      := _subformat.PixelFormat ;

    sf_found := False ;
    for c in Capabilities do begin
      if _subformat = c then begin
        FSubFormat := c ;
        sf_found := True ;
        break ;
      end ;
    end ;

    if not sf_found then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BADPIXELFORMAT ), '', 0  ) ;

    FCS :=_cs ;
    // save projection to prj file
    if assigned( _cs ) and not IsEmbeddedSQLPath( _path ) then
      _cs.SaveAsWKTFile( _path + GIS_PRJ_EXT ) ;
  end ;

  procedure TGIS_FilePixel.doDestroy ;
  begin
    FreeObject( FCapabilities ) ;

    inherited ;
  end ;

  procedure TGIS_FilePixel.Write(
    const _x       : Integer ;
    const _y       : Integer ;
    const _pixels  : TGIS_Pixels ;
    const _pformat : TGIS_PixelFormat ;
    const _width   : Integer ;
    const _height  : Integer
  ) ;
  var
    row        : Integer ;
    inptr      : TBytes  ;
    i          : Integer ;
    px         : TGIS_Color ;
    bmpwidth   : Integer ;
    bmpheight  : Integer ;
  begin
    {$IFDEF GIS_NORECORDS}
    px := TGIS_Color.Create ;
    {$ENDIF} ;
    bmpwidth  := _width  ;
    bmpheight := _height ;
    if ( bmpwidth  + _x ) > FWidth  then bmpwidth  := FWidth  - _x ;
    if ( bmpheight + _y ) > FHeight then bmpheight := FHeight - _y ;

    SetLength( inptr, bmpwidth * 4 ) ;

    if _pformat = TGIS_PixelFormat.ARGB then begin
      for row := 0 to bmpheight -1 do begin
        for i := 0 to bmpwidth -1 do begin
          px.ARGB := Cardinal(_pixels[row*bmpwidth + i]) ;
          inptr[i*4  ] := px.B ;
          inptr[i*4+1] := px.G ;
          inptr[i*4+2] := px.R ;
          inptr[i*4+3] := px.A ;
        end ;
        writeLine( inptr, 0, _y +row, _x * 4, bmpwidth * 4 ) ;
      end ;
    end
    else begin
      for row := 0 to bmpheight -1 do begin
        for i := 0 to bmpwidth -1 do begin
          px.ARGB := Cardinal(_pixels[row*bmpwidth + i]) ;
          inptr[i*3  ] := px.B ;
          inptr[i*3+1] := px.G ;
          inptr[i*3+2] := px.R ;
        end ;
        writeLine( inptr, 0, _y +row, _x * 3, bmpwidth * 3 ) ;
      end ;
    end;
  end ;

  procedure TGIS_FilePixel.WriteGrid(
    const _x     : Integer ;
    const _y     : Integer ;
    const _grd   : TGIS_GridArray
  ) ;
  begin
    // just for safe inheritance
  end ;

  procedure TGIS_FilePixel.InitializeWrite ;
  begin
    // only for safe inheritance
  end ;

  procedure TGIS_FilePixel.FinalizeWrite ;
  begin
    // only for safe inheritance
  end ;

//==================================== END =====================================
end.
